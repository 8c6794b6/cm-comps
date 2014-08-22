{-# LANGUAGE MagicHash #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

A small REPL server implemented with GHC.

Supports defining (and redefining) top-level functions without @let@ and
function callback by name.

Server executable will use cabal package database in sandbox when the server was
started from cabal package directory containing @cabal.sandbox.config@ file, and
add source paths listed in cabal configuration file if found.

-}
module Language.Haskell.Replenish.Server where

import           Language.Haskell.Replenish.Client

-- ghcLoop
import           DynFlags
import           Exception
import           GHC
import           HscTypes
import           Outputable                            (Outputable (..),
                                                        showPpr,
                                                        showSDocForUser,
                                                        showSDocUnqual)
import           PprTyThing                            (pprTyThingHdr)
import           Var                                   (isId, varType)

-- evalStatement
import           GhcMonad
import           HscMain                               (hscStmt)
import           Linker                                (deleteFromLinkEnv,
                                                        extendLinkEnv)

-- load
import           DriverPipeline                        (preprocess)
import           Finder                                (addHomeModuleToFinder,
                                                        mkHomeModLocation)
import           HeaderInfo                            (getImports)
import           ObjLink                               (loadObj, unloadObj)
import           RdrName                               (pprGlobalRdrEnv)
import           StringBuffer                          (hGetStringBuffer)
import           UniqFM
import           Util                                  (getModificationUTCTime,
                                                        modificationTimeIfExists)


import           GHC.Exts                              (unsafeCoerce#)
import           GHC.Paths                             (libdir)

import           Control.Concurrent                    (Chan, ThreadId, forkIO,
                                                        killThread, modifyMVar_,
                                                        myThreadId, newChan,
                                                        newMVar, readChan,
                                                        readMVar, writeChan)
import           Control.Monad                         (forever, unless, void,
                                                        when)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Char8                 as BS
import           Data.Char                             (isSpace)
import           Data.IORef                            (readIORef)
import           Data.List                             (intersperse, isPrefixOf,
                                                        nub)
import           Distribution.PackageDescription       (BuildInfo (..),
                                                        CondTree (..),
                                                        Executable (..),
                                                        Library (..),
                                                        condExecutables,
                                                        condLibrary)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)
import qualified Network                               as Network
import           Network.Socket                        hiding (send)
import           Sound.OSC                             (pauseThreadUntil)
import           System.Directory                      (doesFileExist,
                                                        getCurrentDirectory,
                                                        getTemporaryDirectory,
                                                        removeDirectoryRecursive,
                                                        createDirectoryIfMissing)
import           System.Exit
import           System.FilePath                       (takeBaseName, (<.>),
                                                        (</>))
import           System.IO                             (BufferMode (..), Handle,
                                                        IOMode (..), hClose,
                                                        hFlush, hIsEOF,
                                                        hSetBinaryMode,
                                                        hSetBuffering, openFile)
import           System.Posix.Files                    (createNamedPipe,
                                                        stdFileMode)



-- --------------------------------------------------------------------------
--
-- Server
--
-- --------------------------------------------------------------------------

-- | Start a server.
runServer
  :: Int -- ^ Port number to receive fragment of valid Haskell codes.
  -> IO ()
runServer port =
  Network.withSocketsDo
    (bracket
     (do s <- Network.listenOn (Network.PortNumber (fromIntegral port))
         me <- myThreadId
         fileVar <- newMVar []
         tmpd <- fmap (\d -> d </> "replenish" <.> show port)
                      getTemporaryDirectory
         createDirectoryIfMissing True tmpd
         return (s, me, tmpd, fileVar))
     (\(s, _, tmpd, fileVar) ->
        do putStrLn "Server killed, cleaning up resources  ... "
           close s
           mapM_ (\(tids, hdl) -> do mapM_ killThread tids
                                     hClose hdl) =<< readMVar fileVar
           removeDirectoryRecursive tmpd
           putStrLn "done.")
     (\(s, serverTid, tmpd, fileVar) ->
       forever
        (do (hdl, host, clientPort) <- Network.accept s
            putStrLn (unwords ["Client connected from"
                              , host ++ ":" ++ show clientPort])
            hSetBuffering hdl (BlockBuffering Nothing)
            hSetBinaryMode hdl True
            input <- newChan
            output <- newChan
            let clientPort' = show (fromIntegral clientPort :: Int)
                opath = tmpd </> "out" <.> clientPort'
            forkIO
              (bracket
                (do createNamedPipe opath stdFileMode
                    ohdl <- openFile opath ReadMode
                    hSetBuffering ohdl (BlockBuffering Nothing)
                    hSetBinaryMode ohdl True
                    putStrLn ("Opened " ++ show opath ++ " for output")
                    ptid <- forkIO (printLoop ohdl output)
                    gtid <- forkIO (ghcLoop opath tmpd serverTid input output)
                    htid <- myThreadId
                    let tids = [ptid, gtid, htid]
                    modifyMVar_ fileVar (return . ((tids,ohdl):))
                    return (ptid,  gtid))
                (\(ptid, gtid) ->
                   mapM_ (\tid -> throwTo tid ExitSuccess) [ptid, gtid])
                (\_ -> readLoop hdl host clientPort input output)))))


-- --------------------------------------------------------------------------
--
-- Read/Print loops
--
-- --------------------------------------------------------------------------

-- | Loop to return output to client.
printLoop :: Handle -> Chan ByteString -> IO ()
printLoop hdl output =
  forever (BS.hGetSome hdl 8192 >>= writeChan output)

-- | Loop to get input and reply output with connected 'Handle'.
readLoop
  :: Handle -> HostName -> PortNumber
  -> Chan ByteString -> Chan ByteString -> IO ()
readLoop hdl host clientPort input output = forkIO outLoop >>= inLoop
  where
    inLoop tid =
      (do eof <- hIsEOF hdl
          if eof
            then do showLine (BS.pack "disconnected")
                    killThread tid
            else do chunk <- BS.hGetSome hdl 65536
                    unless (BS.all isSpace chunk)
                           (do mapM_ showLine (BS.lines chunk)
                               writeChan input chunk)
                    inLoop tid)
      `catch`
      (\(SomeException e) ->
        do putStrLn ("handleLoop: Caught " ++ show e)
           killThread tid)
    outLoop = forever (do BS.hPutStr hdl =<< readChan output
                          hFlush hdl)
    showLine bs =
      BS.putStrLn
        (BS.concat
           (map BS.pack ["[", host, ":", show clientPort, "] "])
         `BS.append` bs)

-- --------------------------------------------------------------------------
--
-- Eval loop
--
-- --------------------------------------------------------------------------

-- | Loop to interpret Haskell codes with GHC.
ghcLoop ::
  FilePath -> FilePath -> ThreadId -> Chan ByteString -> Chan ByteString -> IO ()
ghcLoop path dir parentThread input output =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc
      (Just libdir)
      (do srcPaths <- liftIO getCabalSourcePaths
          pkgDbs <- liftIO getCabalPackageConf
          dflags <- getSessionDynFlags
          (dflags',_, _) <- parseDynamicFlags
                              dflags
                              [mkGeneralLocated "flag" initialOptions]
          void (setSessionDynFlags
                  dflags' {verbosity = 0
                          ,packageFlags = [ExposePackage "ghc"]
                          ,extraPkgConfs = const pkgDbs
                          ,hscTarget = HscInterpreted
                          ,ghcLink = LinkInMemory
                          ,importPaths = dir : srcPaths
                          ,objectDir = Just dir
                          ,hiDir = Just dir})
          setContext =<< mapM (fmap IIDecl . parseImportDecl) initialImports
          liftIO initClientPrinter
          forever (eval input output)))
  `catch`
  \exception ->
    do putStrLn ("ghcLoop: Got " ++ show exception)
       case fromException exception of
         Just UserInterrupt -> throwTo parentThread UserInterrupt
         _                  -> ghcLoop path dir parentThread input output
  where
    initialImports :: [String]
    initialImports =
      ["import Prelude"
      ,"import Language.Haskell.Replenish.Client"]

    initialOptions :: String
    initialOptions = "-XTemplateHaskell"

    initClientPrinter :: IO ()
    initClientPrinter =
      mapM_ (writeChan input . BS.pack)
            ["__hdl__ <- __getHdl " ++ show path
            ,"_interactivePrint :: Show a => a -> IO ()\n\
             \_interactivePrint = __interactivePrint __hdl__"
            ,":set_print _interactivePrint"]

-- | Get input from 'Chan', evaluate, write result to ouput 'Chan'.
eval :: Chan ByteString -> Chan ByteString -> Ghc ()
eval input output =
  do expr <- liftIO (readChan input)
     let x = BS.unpack expr
     result <- evalSpecial x `gcatch`
               \(SomeException e) -> return (Just (show e))
     maybe (return ()) (liftIO . writeChan output . BS.pack) result
{-# INLINE eval #-}

-- | Evaluate special cases, e.g; /:info/, /:set/, ... etc.
evalSpecial :: String -> Ghc (Maybe String)
evalSpecial expr
  | ":dump_hsc_env" `isPrefixOf` expr =
    do hsc_env <- getSession
       liftIO
         (do let pp :: Outputable a => a -> String
                 pp = showPpr df
                 df = hsc_dflags hsc_env
                 ic = hsc_IC hsc_env
             putStrLn "hsc_IC . ic_rn_gbl_env:"
             putStrLn (showSDocForUser
                         df
                         alwaysQualify
                         (pprGlobalRdrEnv True (ic_rn_gbl_env ic)))
             putStrLn "hsc_IC . ic_imports:"
             mapM_ (putStrLn . pp) (ic_imports ic)
             putStrLn "hsc_HPT:"
             putStrLn (showSDocForUser df alwaysQualify
                                          (pprHPT (hsc_HPT hsc_env))))
       return (Just "dumped hsc_env.")
  | ":dump_names" `isPrefixOf` expr =
    do names <- getNamesInScope
       dflags <- getSessionDynFlags
       liftIO (mapM_ (putStrLn . showPpr dflags) names)
       return (Just "dumped names.")
  | ":dump_info " `isPrefixOf` expr =
    do df <- getSessionDynFlags
       let expr' = dropWhile isSpace (drop 10 expr)
           pp :: Outputable o => o -> String
           pp   = showPpr df
       names <- parseName expr'
       mbInfo:_ <- mapM (getInfo True) names
       return
         (return
            (maybe ("not in scope: " ++ expr')
                   (\(t,_f,cs,_fs) ->
                      let t' = showSDocUnqual df (pprTyThingHdr t)
                          is = t' : map pp cs
                      in  concat (intersperse "\n" is))
                    mbInfo))
  | ":set_print " `isPrefixOf` expr =
    do let _:name:_ = words expr
       modifyInteractivePrint name
       return Nothing
  | ":set " `isPrefixOf` expr =
    do let _:body = words expr
           loc = map (mkGeneralLocated "set") body
       dflags <- getSessionDynFlags
       (dflags',_, warns) <- parseDynamicFlags dflags loc
       void (setSessionDynFlags dflags')
       return (Just (concatMap (showPpr dflags') warns))
  | ":load " `isPrefixOf` expr = loadSource (drop 6 expr)
  | "import " `isPrefixOf` expr =
    do mdl <- parseImportDecl expr
       getContext >>= setContext . (IIDecl mdl :)
       dflags <- getSessionDynFlags
       return (Just (showPpr dflags mdl))
  | otherwise = evalStatement expr
{-# INLINE evalSpecial #-}

-- | Modify interactive printer, for writing to named pipe file.
modifyInteractivePrint :: GhcMonad m => String -> m ()
modifyInteractivePrint func =
  do (name:_) <- parseName func
     modifySession
       (\hsc_env ->
          hsc_env {hsc_IC = setInteractivePrintName (hsc_IC hsc_env) name})

-- | Load module source file, compile to object code, and add to home package
-- table.  Module defined in given file could be imported after this.
--
-- Unlike GHCi, this will keep the interactively evaluated declarations in GHC
-- session.
loadSource :: FilePath -> Ghc (Maybe String)
loadSource path =
  do modifySession
       (\e -> e {hsc_dflags = (hsc_dflags e) {hscTarget = HscAsm}})
     hsc_env <- getSession
     ms <- liftIO (summariseSource hsc_env path)
     let mdl = ms_mod ms
         last_hmis = [hmi |hmi <- eltsUFM (hsc_HPT hsc_env)
                          ,mdl == mi_module (hm_iface hmi)]

     -- [Note about reloading modules]
     -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     --
     -- Manually doing `ObjLink.loadObj' and `ObjLink.unloadObj' when load
     -- request with same module happens. This sequence of operations will make
     -- `Linker.PersistentLinkerState.obj_loaded', out of sync with loaded
     -- object. Fields in `Linker.PersistentLinkerState' are not exported by
     -- 'Linker' module.
     --
     mapM_ unloadModule last_hmis
     void
       (parseModule ms >>= typecheckModule >>= desugarModule >>= loadModule)
     mapM_ doLoadObj last_hmis

     modifySession
       (\e -> e {hsc_dflags = (hsc_dflags e) {hscTarget = HscInterpreted}})
     return (Just (if not (null last_hmis)
                      then "reloaded: " ++ path
                      else "loaded: " ++ path))
  where
    summariseSource :: HscEnv -> FilePath -> IO ModSummary
    summariseSource hsc_env src =
      do (dflags', hspp_file) <- preprocess hsc_env (src, Nothing)
         buf <- hGetStringBuffer hspp_file
         (simps, timps, L _ mod_name) <- getImports dflags' buf hspp_file src
         location <- mkHomeModLocation dflags' mod_name path
         mdl <- addHomeModuleToFinder hsc_env mod_name location
         hs_date <- getModificationUTCTime path
         obj_date <- modificationTimeIfExists (ml_obj_file location)
         return ModSummary {ms_mod = mdl
                           ,ms_hsc_src = HsSrcFile
                           ,ms_location = location
                           ,ms_hspp_file = hspp_file
                           ,ms_hspp_opts = dflags'
                           ,ms_hspp_buf = Just buf
                           ,ms_srcimps = simps
                           ,ms_textual_imps = timps
                           ,ms_hs_date = hs_date
                           ,ms_obj_date = obj_date}

    unloadModule :: GhcMonad m => HomeModInfo -> m ()
    unloadModule hmi =
      do mb_mi <- getModuleInfo (mi_module (hm_iface hmi))
         case mb_mi of
           Nothing -> return ()
           Just mi -> liftIO (deleteFromLinkEnv (modInfoExports mi))
         case hm_linkable hmi of
           Nothing  -> return ()
           Just lnk -> doUnloadObj lnk

    doUnloadObj :: GhcMonad m => Linkable -> m ()
    doUnloadObj (LM _ _ us) = mapM_ doReloadOne us
      where
        doReloadOne (DotO o) = liftIO (unloadObj o)
        doReloadOne _        = return ()

    doLoadObj :: GhcMonad m => HomeModInfo -> m ()
    doLoadObj hmi =
      case hm_linkable hmi of
        Nothing  -> return ()
        Just lnk ->
          mapM_ (\o -> case o of
                         DotO p -> liftIO (loadObj p)
                         _      -> return ()) (linkableUnlinked lnk)
{-# INLINE loadSource #-}

-- | Evaluate statements:
--
-- > foldr (+) 0 [1..10]
-- > print True
--
evalStatement :: String -> Ghc (Maybe String)
evalStatement stmt =
  do hsc_env <- getSession
     r <- gtry (liftIO (hscStmt hsc_env stmt))
     let dflags = hsc_dflags hsc_env
     case r of
       Right (Just ([],           _,        _)) -> return Nothing
       Right (Just (vars@(var:_), hvals_io, _)) ->
        do let nameStr = showPpr dflags (getName var)
           hvals@(hval:_) <- liftIO hvals_io
           when (nameStr /= "it")
                (bindNames hsc_env vars hvals)
           callbackOrVoid dflags var hval
       Right Nothing                            -> return Nothing
       Left (SomeException e)
         | looksLikeParseError (show e) -> evalDec stmt
         | otherwise                    -> return (Just (show e))
  where
    callbackOrVoid :: DynFlags -> Id -> HValue -> Ghc (Maybe String)
    callbackOrVoid df var hval =
      do let tyStr = showPpr df (varType var)
             isCallback = isId var && tyStr == cbTyStr
             isCallbacks = isId var && tyStr == cbsTyStr
         case () of
           _ | isCallback ->
               do session <- getSessionRef
                  liftIO (void (forkIO (loopCallback
                                          session (unsafeCoerce# hval))))
             | isCallbacks ->
               do session <- getSessionRef
                  liftIO (mapM_ (forkIO . loopCallback session)
                                (unsafeCoerce# hval))
             | otherwise   -> return ()
         return Nothing

    bindNames :: GhcMonad m => HscEnv -> [Id] -> [HValue] -> m ()
    bindNames hsc_env vars hvals =
      do liftIO
           (extendLinkEnv (zipWith (\v h -> (getName v, h)) vars hvals))
         setSession
           (hsc_env {hsc_IC = extendInteractiveContext
                                (hsc_IC hsc_env) (map AnId vars)})

    getSessionRef :: Ghc Session
    getSessionRef = Ghc return

    cbTyStr :: String
    cbTyStr = "Language.Haskell.Replenish.Client.Callback"

    cbsTyStr :: String
    cbsTyStr = "[" ++ cbTyStr ++ "]"

    looksLikeParseError :: String -> Bool
    looksLikeParseError errString
      | "parse error " `isPrefixOf` errString = True
      | otherwise = False

    loopCallback :: Session -> Callback -> IO ()
    loopCallback s@(Session session) cb =
      case cb of
        Callback t f args ->
          do hsc_env <- readIORef session
             r <- hscStmt hsc_env (unwords [f, args])
             case r of
               Just (var:_, hvals_io, _) ->
                 do pauseThreadUntil t
                    let varTyStr = showPpr (hsc_dflags hsc_env) (varType var)
                    hvals <- hvals_io
                    case hvals of
                      hval:_ -> when (varTyStr == cbTyStr)
                                     (loopCallback s (unsafeCoerce# hval))
                      _      -> return ()
               _ -> return ()
        End               -> return ()
    {-# INLINE loopCallback #-}
{-# INLINE evalStatement #-}

-- | Evaluate declarations:
--
-- > f x = x + 1
-- > data Foo a = Foo a Int | Bar a a
--
evalDec :: GhcMonad m => String -> m (Maybe String)
evalDec dec =
  do names <- runDecls dec
     dflags <- getSessionDynFlags
     case names of
       [] -> return Nothing
       _  -> fmap (Just . ("dec: " ++) . unwords)
                  (mapM (showDec dflags) names)
  where
    showDec :: GhcMonad m => DynFlags-> Name -> m String
    showDec df name =
      do Just (t, _, _, _) <- getInfo True name
         return (unwords [showSDocUnqual df (pprTyThingHdr t)])
{-# INLINE evalDec #-}


-- --------------------------------------------------------------------------
--
-- For Cabal configuration file
--
-- --------------------------------------------------------------------------

-- | Reads cabal sandbox file if it exists, returns 'PkgConfRef' from the file.
getCabalPackageConf :: IO [PkgConfRef]
getCabalPackageConf =
  do pkgDbs <- getSandboxPackageDbs
     if not (null pkgDbs)
        then return (GlobalPkgConf : map PkgConfFile pkgDbs)
        else return [GlobalPkgConf, UserPkgConf]

-- | Get package db from cabal sandbox config file.
getSandboxPackageDbs :: IO [FilePath]
getSandboxPackageDbs =
  do cwd <- getCurrentDirectory
     let sandboxConfigFile = cwd </> "cabal.sandbox.config"
         filterPackageDbLines =
           map (tail . dropWhile (not . isSpace)) .
           filter ("package-db:" `isPrefixOf`) .
           lines
     hasSandboxConfig <- doesFileExist sandboxConfigFile
     if hasSandboxConfig
        then fmap filterPackageDbLines (readFile sandboxConfigFile)
        else return []

-- | Reads cabal config file and get source directories.
getCabalSourcePaths :: IO [String]
getCabalSourcePaths =
  do cwd <- getCurrentDirectory
     let configFile = cwd </> takeBaseName cwd <.> "cabal"
     hasConfigFile <- doesFileExist configFile
     if hasConfigFile
        then
          do gPkgDesc <- readPackageDescription normal configFile
             let lib = fmap (hsSourceDirs . libBuildInfo . condTreeData)
                            (condLibrary gPkgDesc)
                 exes = fmap (hsSourceDirs . buildInfo . condTreeData . snd)
                             (condExecutables gPkgDesc)
                 sources = maybe exes (: exes) lib
             return (nub (concat sources))
        else return []
