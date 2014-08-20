{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

A small REPL server implemented with GHC. Supports defining (and redefining)
top-level functions without @let@ and function callback by name.

-}
module Language.Haskell.Replenish.Server where

import           Language.Haskell.Replenish.Client

import           DynFlags
import           Exception
import           GHC
import           HscTypes
import           Outputable                            (Outputable (..),
                                                        showPpr, showSDocUnqual)
import           PprTyThing                            (pprTyThingHdr)
import           Var                                   (isId, varType)

import           GHC.Exts                              (unsafeCoerce#)
import           GHC.Paths                             (libdir)

import           Control.Concurrent                    (Chan, ThreadId, forkIO,
                                                        killThread, modifyMVar_,
                                                        myThreadId, newChan,
                                                        newMVar, readChan,
                                                        readMVar, writeChan)
import           Control.Monad                         (filterM, forever,
                                                        unless, void, when)
import           Control.Monad.IO.Class                (MonadIO (..))
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
                                                        removeFile)
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

import           GhcMonad
import           HscMain
import           Linker


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
         return (s, me, fileVar))
     (\(s, _, fileVar) ->
        do putStr "Server killed, cleaning up resources  ... "
           close s
           mapM_ (\(tids, hdl, path) ->
                    do mapM_ killThread tids
                       hClose hdl
                       putStrLn ("Removing " ++ path)
                       removeFile path) =<< readMVar fileVar
           putStrLn "done.")
     (\(s, me, fileVar) ->
       forever
        (do (hdl, host, clientPort) <- Network.accept s
            putStrLn (unwords ["Client connected from"
                              , host ++ ":" ++ show clientPort])
            hSetBuffering hdl (BlockBuffering Nothing)
            hSetBinaryMode hdl True
            input <- newChan
            output <- newChan
            tmpd <- getTemporaryDirectory
            let clientPort' = show (fromIntegral clientPort :: Int)
                opath = tmpd </> "replenish.out." ++ clientPort'
            forkIO
              (bracket
                (do createNamedPipe opath stdFileMode
                    ohdl <- openFile opath ReadMode
                    hSetBuffering ohdl NoBuffering
                    hSetBinaryMode ohdl True
                    putStrLn ("Opened " ++ show opath ++ " for output")
                    ptid <- forkIO (printLoop ohdl output)
                    gtid <- forkIO (ghcLoop opath me input output)
                    htid <- myThreadId
                    let tids = [ptid, gtid, htid]
                    modifyMVar_ fileVar (return . ((tids,ohdl,opath):))
                    return (ptid,  gtid))
                (\(ptid, gtid) ->
                   mapM_ (\tid -> throwTo tid ExitSuccess) [ptid, gtid])
                (\_ -> handleLoop me hdl host clientPort input output)))
        `catch`
     (\(SomeException e) ->
        putStrLn ("runServer: Got " ++ show e))))

-- | Loop to return output to client.
printLoop :: Handle -> Chan ByteString -> IO ()
printLoop hdl output =
  forever (BS.hGetSome hdl 8192 >>= writeChan output)

-- | Loop to get input and reply output with connected 'Handle'.
handleLoop
  :: ThreadId -> Handle -> HostName -> PortNumber
  -> Chan ByteString -> Chan ByteString -> IO ()
handleLoop parentThread hdl host clientPort input output =
  (forkIO outLoop >>= inLoop)
  `catch`
  (\(e :: AsyncException) ->
    case e of
      UserInterrupt ->
        do putStrLn "Got user interrupt, killing the server"
           throwTo parentThread UserInterrupt
      ThreadKilled  -> putStrLn "handleLoop: ThreadKilled"
      _             -> putStrLn "handleLoop: AsyncException")
  where
    inLoop tid =
      do eof <- hIsEOF hdl
         if eof
           then do showLine (BS.pack "disconnected")
                   killThread tid
           else do chunk <- BS.hGetSome hdl 65536
                   unless (BS.all isSpace chunk)
                          (do mapM_ showLine (BS.lines chunk)
                              writeChan input chunk)
                   inLoop tid

    outLoop = forever (do BS.hPutStr hdl =<< readChan output
                          hFlush hdl)
    showLine bs =
      BS.putStrLn
        (BS.concat
           (map BS.pack ["[", host, ":", show clientPort, "] "])
         `BS.append` bs)

-- | Loop to interpret Haskell codes with GHC.
ghcLoop ::
  FilePath -> ThreadId -> Chan ByteString -> Chan ByteString -> IO ()
ghcLoop path parentThread input output =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc
      (Just libdir)
      (do srcPaths <- liftIO getCabalSourcePaths
          pkgDbs <- liftIO getCabalPackageConf
          dflags <- getSessionDynFlags
          -- liftIO
          --   (do putStr ("src:\n" ++
          --               unlines (map ("  " ++) srcPaths))
          --       putStr ("package-dbs:\n" ++
          --               unlines (map (("  " ++ ) . showPkgConfRef) pkgDbs)))
          (dflags',_, _) <- parseDynamicFlags
                              dflags
                              [mkGeneralLocated "flag" initialOptions]
          void (setSessionDynFlags
                  dflags' {verbosity = 0
                          ,packageFlags = [ExposePackage "ghc"]
                          ,extraPkgConfs = const pkgDbs
                          ,hscTarget = HscInterpreted
                          ,ghcLink = LinkInMemory
                          ,importPaths = srcPaths})
          setContext =<< mapM (fmap IIDecl . parseImportDecl) initialImports
          let getHdl = "__hdl__ <- __getHdl " ++ show path
              bindIP = "_interactivePrint :: Show a => a -> IO ()\n\
                       \_interactivePrint = __interactivePrint __hdl__"
              setIP  = ":set_print _interactivePrint"
          liftIO (mapM_ (writeChan input . BS.pack )
                        [getHdl, bindIP, setIP, "readyMessage"])
          forever (eval input output)))
  `catch`
  \exception ->
    do putStrLn ("ghcLoop: Got " ++ show exception)
       case fromException exception of
         Just UserInterrupt -> throwTo parentThread UserInterrupt
         _                  -> ghcLoop path parentThread input output

modifyInteractivePrint :: GhcMonad m => String -> m ()
modifyInteractivePrint func =
  do (name:_) <- parseName func
     modifySession
       (\hsc_env ->
          let ic = hsc_IC hsc_env
          in  hsc_env {hsc_IC = setInteractivePrintName ic name})

showPkgConfRef :: PkgConfRef -> String
showPkgConfRef ref =
  case ref of
    GlobalPkgConf    -> "GlobalPkgConf"
    UserPkgConf      -> "UserPkgConf"
    PkgConfFile path -> "PkgConfFile \"" ++ path  ++ "\""

initialImports :: [String]
initialImports =
  ["import Prelude"
  ,"import Language.Haskell.Replenish.Client"]

initialOptions :: String
initialOptions = "-XTemplateHaskell"

eval :: Chan ByteString -> Chan ByteString -> Ghc ()
eval input output =
  do expr <- liftIO (readChan input)
     let x = BS.unpack expr
     result <- evalDump x `gcatch`
               \(SomeException e) -> return (Just (show e))
     maybe (return ()) (liftIO . writeChan output . BS.pack) result

evalDump :: String -> Ghc (Maybe String)
evalDump expr
  | ":dump_hsc_env" `isPrefixOf` expr =
    do hsc_env <- getSession
       liftIO
         (do let pp :: Outputable a => a -> String
                 pp = showPpr (hsc_dflags hsc_env)
             putStrLn "hsc_mod_graph:"
             mapM_ (putStrLn . pp) (hsc_mod_graph hsc_env)
             putStrLn "hsc_targets:"
             mapM_ (putStrLn . pp) (hsc_targets hsc_env)
             case hsc_type_env_var hsc_env of
               Nothing          -> return ()
               Just (mdl,teRef) ->
                 do putStrLn "hsc_type_env_var:"
                    putStrLn (pp mdl)
                    te <- readIORef teRef
                    putStrLn (pp te))
       return (Just "dumped hsc_env.")
  | ":dump_names" `isPrefixOf` expr =
    do names <- getNamesInScope
       dflags <- getSessionDynFlags
       liftIO (mapM_ (putStrLn . showPpr dflags) names)
       return (Just "dumped names.")
  | ":dump_info" `isPrefixOf` expr =
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
  | ":set_print" `isPrefixOf` expr =
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
  | otherwise = evalLoadOrImport expr
{-# INLINE evalDump #-}

evalLoadOrImport :: String -> Ghc (Maybe String)
evalLoadOrImport expr
  | ":load " `isPrefixOf` expr =
    do target <- guessTarget (drop 6 expr) Nothing
       setTargets [target]
       _ <- load LoadAllTargets
       loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
       setContext . map IIDecl =<<
         mapM parseImportDecl
              ("import Prelude" :
               map (\m -> "import " ++ moduleNameString m) loaded)
       dflags <- getSessionDynFlags
       return (Just ("loaded: " ++ showPpr dflags target))
  | "import " `isPrefixOf` expr =
    do mdl <- parseImportDecl expr
       getContext >>= setContext . (IIDecl mdl :)
       dflags <- getSessionDynFlags
       return (Just (showPpr dflags mdl))
  | otherwise = evalStatement expr
{-# INLINE evalLoadOrImport #-}

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
{-# INLINE evalStatement #-}

cbTyStr :: String
cbTyStr = "Language.Haskell.Replenish.Client.Callback"

cbsTyStr :: String
cbsTyStr = "[" ++ cbTyStr ++ "]"

looksLikeParseError :: String -> Bool
looksLikeParseError errString
  | "parse error " `isPrefixOf` errString = True
  | otherwise = False

liftFork :: (MonadIO m, Functor m) => IO a -> m ()
liftFork a = void (liftIO (forkIO (void a)))

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

-- | Evaluate declarations.
evalDec :: String -> Ghc (Maybe String)
evalDec dec =
  do names <- runDecls dec
     dflags <- getSessionDynFlags
     if null names
        then return Nothing
        else do decs <- mapM (showDec dflags) names
                return (Just ("dec: " ++ unwords decs))
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
