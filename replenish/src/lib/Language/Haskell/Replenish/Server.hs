{-# LANGUAGE MagicHash         #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Simple REPL server, implemented with GHC. Supports defining (and redefining)
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

import           GHC.Exts                              (unsafeCoerce#)
import           GHC.Paths                             (libdir)
import GHC.IO.Handle

import           Control.Concurrent
import           Control.Monad                         (filterM, forever,
                                                        unless, void, when)
-- import           Control.Monad.IO.Class                (liftIO)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Char8                 as BS
import           Data.Char                             (isSpace)
import           Data.IORef                            (atomicModifyIORef', readIORef)
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
import           System.Directory                      (doesFileExist,
                                                        getCurrentDirectory, getTemporaryDirectory)
import           System.FilePath                       (takeBaseName, (<.>),
                                                        (</>))
import           System.IO                             (BufferMode (..), Handle,
                                                        openFile,
                                                        stdout,
                                                        IOMode (..),
                                                        openTempFile,
                                                        hFlush,

                                                        hSetBinaryMode,
                                                        hSetBuffering)

import Sound.OSC

import GhcMonad
import Var
import Linker
import HscMain

-- import Type

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
            hSetBuffering hdl (BlockBuffering Nothing)
            hSetBinaryMode hdl True
            input <- newChan
            output <- newChan
            putStrLn (unwords ["Client connected from"
                              , host ++ ":" ++ show clientPort])
            _htid <- forkIO (handleLoop hdl host clientPort input output)
            void (forkIO (ghcLoop me input output)))))

-- | Loop to get input and reply output with connected 'Handle'.
handleLoop
  :: Handle -> HostName -> PortNumber
  -> Chan ByteString -> Chan ByteString -> IO ()
handleLoop hdl host clientPort input output = forkIO outLoop >> inLoop
  where
    inLoop = forever (do chunk <- BS.hGetSome hdl 65536
                         unless (BS.all isSpace chunk)
                                (do -- mapM_ showLine (BS.lines chunk)
                                    writeChan input chunk))
    outLoop = forever (do BS.hPutStr hdl =<< readChan output
                          hFlush hdl)
    showLine bs =
      BS.putStrLn
        (BS.concat
           (map BS.pack ["[", host, ":", show clientPort, "] "])
         `BS.append` bs)

-- | Loop to interpret Haskell codes with GHC.
ghcLoop :: ThreadId -> Chan ByteString -> Chan ByteString -> IO ()
ghcLoop parentThread input output =
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
          let client = "Language.Haskell.Replenish.Client"
          (setContext =<< mapM (fmap IIDecl . parseImportDecl)
                               (("import " ++ client) : initialImports))
            `gcatch`
            (\(SomeException e) ->
              do liftIO (do putStrLn ("Caught exception: " ++ show e)
                            putStrLn ("Loading " ++ client ++ " as target."))
                 target <- guessTarget client Nothing
                 setTargets [target]
                 _ <- load LoadAllTargets
                 inis <- mapM (fmap IIDecl . parseImportDecl) initialImports
                 setContext (IIModule (mkModuleName client) : inis))
          liftIO (putStrLn "GHC loop ready.")
          eval input output))
  `catch`
  \UserInterrupt ->
    do putStrLn "Got UserInterrupt, killing the server."
       throwTo parentThread UserInterrupt
  `catch`
  \(SomeException e) ->
    putStrLn ("ghcLoop: " ++ show e)

showPkgConfRef :: PkgConfRef -> String
showPkgConfRef ref =
  case ref of
    GlobalPkgConf    -> "GlobalPkgConf"
    UserPkgConf      -> "UserPkgConf"
    PkgConfFile path -> "PkgConfFile \"" ++ path  ++ "\""

initialImports :: [String]
initialImports = ["import Prelude"]

initialOptions :: String
initialOptions = "-XTemplateHaskell"

eval ::  Chan ByteString -> Chan ByteString -> Ghc ()
eval input output =
  -- liftIO (getChanContents input) >>=
  -- mapM_ (\x ->
  --          let x' = BS.unpack x
  --          in (evalStatement input output path x'
  --              `gcatch` \(SomeException _e1) -> evalLoadOrImport x'
  --              `gcatch` \(SomeException _e3) -> evalDec x'
  --              `gcatch` \(SomeException _e4) -> evalDump x'
  --              `gcatch` \(SomeException e5) -> return (show e5))
  --              >>= liftIO . writeChan output . BS.pack)
  forever
    (do expr <- liftIO (readChan input)
        let x = BS.unpack expr
        result <- evalStatement input x `gcatch`
                  \(SomeException _) -> evalLoadOrImport x `gcatch`
                  \(SomeException _) -> evalDec x `gcatch`
                  \(SomeException _) -> evalDump x `gcatch`
                  \(SomeException e) -> return (Just (show e))
        maybe (return ()) (liftIO . writeChan output . BS.pack) result)

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
  | otherwise                 =
    do mdl <- parseImportDecl expr
       getContext >>= setContext . (IIDecl mdl :)
       dflags <- getSessionDynFlags
       return (Just (showPpr dflags mdl))
{-# INLINE evalLoadOrImport #-}

evalDec :: String -> Ghc (Maybe String)
evalDec dec =
  do names <- runDecls dec
     dflags <- getSessionDynFlags
     return (Just (if null names
                       then "decs: no names bound."
                       else "decs: " ++ concatMap (showPpr dflags) names))
{-# INLINE evalDec #-}

evalStatement :: Chan ByteString -> String -> Ghc (Maybe String)
-- evalStatement input output path stmt =
--   do res' <- runStmt (interceptStmt path stmt) RunToCompletion
--      case res' of
--        RunOk names ->
--          do dflags <- getSessionDynFlags
--             tryCallback input output names
--             return
--               (if null names
--                   then "stmt: no names bound."
--                   else "stmt: " ++ unwords (map (showPpr dflags) names))
--        RunException e ->
--          liftIO (do putStrLn ("RunException: " ++ show e)
--                     return (show e))
--        RunBreak {} ->
--          liftIO (do putStrLn "Got RunBreak"
--                     return "RunBreak")
evalStatement input stmt =
  do hsc_env <- getSession
     r <- liftIO (hscStmt hsc_env stmt)
     case r of
       Just (vars, hvals_io, fixty) ->
         do updateFixityEnv hsc_env fixty
            hvals <- liftIO hvals_io
            case (vars, hvals) of
              (var:_, hval:_) -> forkOrShow (hsc_dflags hsc_env) var hval
              _               -> return (Just "ambiguous or not hvals.")
       Nothing -> return (Just "not a statement.")
  where
    updateFixityEnv hsc_env fix_env =
      let ic = hsc_IC hsc_env
      in  setSession $ hsc_env {hsc_IC = ic {ic_fix_env = fix_env}}
    forkOrShow df var hval
      | isId var && cbTyStr == tyStr =
        case unsafeCoerce# hval of
          cb@Callback{} -> do liftIO (forkIt cb)
                              return Nothing
          _             -> return (Just tyStr)
      | otherwise =
       do let expr = "Prelude.show (" ++ showPpr df (getName var) ++ ")"
          r <- fmap unsafeCoerce# (compileExpr expr)
          return (Just (r ++ " :: " ++ tyStr))
      where
        tyStr = showPpr df (varType var)
    forkIt cb =
      case cb of
        Callback t f args ->
          void (forkIO
                  (do pauseThreadUntil t
                      writeChan input (BS.unwords (map BS.pack [f, args]))))
        _ -> return ()
{-# INLINE evalStatement #-}

tryCallback :: Chan ByteString -> Chan ByteString -> [Name] -> Ghc ()
tryCallback input output names =
  case names of
    name:_ ->
      do mbTyThing <- lookupName name
         case mbTyThing of
           Just (AnId x) | isId x ->
             do df <- getSessionDynFlags
                when (cbTyStr == showPpr df (varType x))
                     (reifyGhc (forkCallback input output name))
           _  -> return ()
    _ -> return ()
{-# INLINE tryCallback #-}

-- XXX: Better to compare with other thing than String.
cbTyStr :: String
cbTyStr = "Language.Haskell.Replenish.Client.Callback"

forkCallback
  :: Chan ByteString -> Chan ByteString -> Name -> Session -> IO ()
forkCallback input output name (Session session) =
  do hsc_env <- readIORef session
     hval <- getHValue hsc_env name
     case unsafeCoerce# hval of
       -- cb@(Callback {}) -> void (forkIO (loopCallback cb))
       Callback t f args ->
         void
           (forkIO
              (do pauseThreadUntil t
                  writeChan input (BS.unwords (map BS.pack [f, args]))
                  void (forkIO (void (readChan output)))))
       _  -> return ()
  where
    -- Unused. Get segfault after updating function with same name.
    loopCallback :: Callback -> IO ()
    loopCallback cb =
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
                      hval:_ -> loopCallback (unsafeCoerce# hval)
                      _      -> return ()
                _ -> return ()
        End -> return ()
{-# INLINE forkCallback #-}

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
  | otherwise = error "Not a dump command."
{-# INLINE evalDump #-}


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
