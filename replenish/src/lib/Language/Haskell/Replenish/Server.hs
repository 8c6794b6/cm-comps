{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
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
                                                        myThreadId, newChan,
                                                        readChan, writeChan)
import           Control.Monad                         (filterM, forever,
                                                        unless, void, when)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Char8                 as BS
import           Data.Char                             (isSpace)
import           Data.IORef                            (atomicModifyIORef',
                                                        readIORef)
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
                                                        removeFile,
                                                        getCurrentDirectory,
                                                        getTemporaryDirectory)
import           System.FilePath                       (takeBaseName, (<.>),
                                                        (</>))
import           System.IO                             (BufferMode (..), Handle,
                                                        openFile, hIsEOF, hClose, IOMode (..),
                                                        hFlush, hSetBinaryMode,
                                                        hSetBuffering, openTempFile,
                                                        withFile)

import           GhcMonad
import           HscMain
import           Linker

import System.Posix.IO
import System.Posix.Files


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
         return (s, me))
     (\(s, _) ->
        do putStr "Server killed, closing socket ... "
           close s
           putStrLn "done.")
     (\(s, me) ->
       forever
        (do (hdl, host, clientPort) <- Network.accept s
            putStrLn (unwords ["Client connected from"
                              , host ++ ":" ++ show clientPort])
            hSetBuffering hdl (BlockBuffering Nothing)
            hSetBinaryMode hdl True
            input <- newChan
            output <- newChan
            tmpd <- getTemporaryDirectory
            let opath = tmpd ++ "/replenish.out." ++
                        show (fromIntegral clientPort :: Int)
            createNamedPipe opath stdFileMode
            putStrLn ("Opened " ++ show opath ++ " for output")
            ohdl <- openFile opath ReadMode
            hSetBuffering ohdl NoBuffering
            hSetBinaryMode ohdl True
            _ptid <- forkIO (printLoop ohdl output)
            _gtid <- forkIO (ghcLoop opath me input output)
            void (forkIO (handleLoop hdl host clientPort input output)))))

printLoop :: Handle -> Chan ByteString -> IO ()
printLoop readHdl output =
  forever (do chunk <- BS.hGetSome readHdl 8192
              writeChan output chunk)
  -- do eof <- hIsEOF readHdl
  --    if eof
  --       then do hClose readHdl
  --               readHdl' <- openFile readPath ReadMode
  --               hSetBuffering readHdl' NoBuffering
  --               hSetBinaryMode readHdl' True
  --               printLoop readPath readHdl' output
  --       else do ln <- BS.hGetLine readHdl
  --               writeChan output ln
  --               printLoop readPath readHdl output

-- | Loop to get input and reply output with connected 'Handle'.
handleLoop
  :: Handle -> HostName -> PortNumber
  -> Chan ByteString -> Chan ByteString -> IO ()
handleLoop hdl host clientPort input output = forkIO outLoop >> inLoop
  where
    inLoop =
      do eof <- hIsEOF hdl
         if eof
           then showLine (BS.pack "disconnected")
           else do chunk <- BS.hGetSome hdl 65536
                   unless (BS.all isSpace chunk)
                          (do mapM_ showLine (BS.lines chunk)
                              writeChan input chunk)
                   inLoop
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
          liftIO
            (do putStr ("src:\n" ++
                        unlines (map ("  " ++) srcPaths))
                putStr ("package-dbs:\n" ++
                        unlines (map (("  " ++ ) . showPkgConfRef) pkgDbs)))
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
          liftIO (do writeChan input (BS.pack getHdl)
                     writeChan input (BS.pack bindIP)
                     writeChan input (BS.pack setIP))
          eval input output))
  `catch`
  \UserInterrupt ->
    do putStrLn "Got UserInterrupt, killing the server."
       throwTo parentThread UserInterrupt
  `catch`
  \(SomeException e) ->
    putStrLn ("ghcLoop: " ++ show e)

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
  forever
    (do expr <- liftIO (readChan input)
        let x = BS.unpack expr
        result <- evalDump input x `gcatch`
                  \(SomeException e) -> return (Just (show e))
        maybe (return ()) (liftIO . writeChan output . BS.pack) result)

evalDump :: Chan ByteString -> String -> Ghc (Maybe String)
evalDump input expr
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
  | otherwise = evalLoadOrImport input expr
{-# INLINE evalDump #-}

evalLoadOrImport :: Chan ByteString -> String -> Ghc (Maybe String)
evalLoadOrImport input expr
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
  | otherwise = evalStatement input expr
{-# INLINE evalLoadOrImport #-}

evalStatement :: Chan ByteString -> String -> Ghc (Maybe String)
evalStatement input stmt =
  do hsc_env <- getSession
     r <- gtry (runStmt stmt RunToCompletion)
     case r of
       Right (RunOk [])       -> return Nothing
       Right (RunOk (name:_)) ->
         do Just (thing,_,_,_) <- getInfo False name
            case thing of
              AnId var -> do hval <- liftIO (getHValue hsc_env name)
                             forkOrShow (hsc_dflags hsc_env) var hval
              _        -> return Nothing
       Right (RunException e) -> return (Just ("RunException: " ++ show e))
       Right (RunBreak{})     -> return (Just "RunBreak")
       Left (SomeException e)
         | looksLikeParseError (show e) -> evalDec stmt
         | otherwise                    -> return (Just (show e))

     -- r <- gtry (liftIO (hscStmt hsc_env stmt))
     -- case r of
     --   Right (Just ([],    _,        _)) -> return Nothing
     --   Right (Just (var:_, hvals_io, _)) ->
     --    do hval:_ <- liftIO hvals_io
     --       forkOrShow (hsc_dflags hsc_env) var hval
     --   Right Nothing -> return Nothing
     --   Left (SomeException (show -> e))
     --     | looksLikeParseError e -> evalDec stmt
     --     | otherwise             -> return (Just e)
  where
    forkOrShow :: DynFlags -> Id -> HValue -> Ghc (Maybe String)
    -- forkOrShow df var hval
    --   | isId var && cbTyStr == tyStr =
    --     do liftIO (forkIt (unsafeCoerce# hval))
    --        return Nothing
    --   | isId var && cbsTyStr == tyStr =
    --     do liftIO (mapM_ forkIt (unsafeCoerce# hval))
    --        return Nothing
    --   | otherwise =
    --     do let expr' = "Prelude.show (" ++ showPpr df (getName var) ++ ")"
    --        either_hval' <- gtry (compileExpr expr')
    --        case either_hval' of
    --          Right hval' -> return (Just (unsafeCoerce# hval'))
    --          Left (SomeException err) -> return (Just (show err))
    --   where
    --     tyStr = showPpr df (varType var)
    forkOrShow df var hval =
      do let tyStr = showPpr df (varType var)
             isCallback = isId var && tyStr == cbTyStr
             isCallbacks = isId var && tyStr == cbsTyStr
         case () of
           _ | isCallback  -> liftIO (forkIt (unsafeCoerce# hval))
             | isCallbacks -> liftIO (mapM_ forkIt (unsafeCoerce# hval))
             | otherwise   -> return ()
         return Nothing

    forkIt :: Callback -> IO ()
    forkIt cb =
      case cb of
        Callback t f args ->
          void (forkIO
                  (do pauseThreadUntil t
                      writeChan input (BS.unwords (map BS.pack [f, args]))))
        _ -> return ()

    -- Better to compare with other thing than String.
    cbTyStr :: String
    cbTyStr = "Language.Haskell.Replenish.Client.Callback"

    cbsTyStr :: String
    cbsTyStr = "[" ++ cbTyStr ++ "]"

    looksLikeParseError :: String -> Bool
    looksLikeParseError errString
      | "parse error " `isPrefixOf` errString = True
      | otherwise = False

{-# INLINE evalStatement #-}

-- Unused. Getting segfault after updating function with same name.
loopCallback :: Callback -> Session -> IO ()
loopCallback cb (Session session) =
  case cb of
    Callback t f args ->
      do pauseThreadUntil t
         hsc_env <- readIORef session
         r <- hscStmt hsc_env (unwords [f, args])
         case r of
            Just (is, hvals_io, fixity) ->
             do let up e = e {hsc_IC = (extendInteractiveContext
                                         (hsc_IC e)
                                         (map AnId is))
                                         {ic_fix_env = fixity}}
                hvals <- hvals_io
                extendLinkEnv (zip (map getName is) hvals)
                atomicModifyIORef'
                  session
                  (\hsc_env' -> (up hsc_env', ()))
                case hvals of
                  hval:_ -> loopCallback (unsafeCoerce# hval) (Session session)
                  _      -> return ()
            _ -> return ()
    End -> return ()
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
