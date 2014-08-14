{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Concurrent
import           Control.Monad                         (filterM, forever,
                                                        unless, void)
import           Control.Monad.IO.Class                (liftIO)
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
import           System.Directory                      (doesFileExist,
                                                        getCurrentDirectory)
import           System.FilePath                       (takeBaseName, (<.>),
                                                        (</>))
import           System.IO                             (BufferMode (..), Handle,
                                                        hClose, hFlush,
                                                        hSetBinaryMode,
                                                        hSetBuffering)

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
            let clientPort' = fromIntegral clientPort + 1
            _ctid <- forkIO (callbackLoop clientPort' input output)
            _htid <- forkIO (handleLoop hdl host clientPort input output)
            void (forkIO (ghcLoop clientPort' me input output)))))

-- | Loop to get input and reply output with connected 'Handle'.
handleLoop
  :: Handle -> HostName -> PortNumber
  -> Chan ByteString -> Chan ByteString -> IO ()
handleLoop hdl host clientPort input output = go
  where
    go = forever
           (do chunk <- BS.hGetSome hdl 65536
               unless (BS.all isSpace chunk)
                      (do mapM_ showLine (BS.lines chunk)
                          writeChan input chunk
                          BS.hPutStr hdl =<< readChan output
                          hFlush hdl))
    showLine bs =
      BS.putStrLn
        (BS.concat
           (map BS.pack ["[", host, ":", show clientPort, "] "])
         `BS.append` bs)

-- | Loop to take care of callbacks.
callbackLoop :: Int -> Chan ByteString -> Chan ByteString -> IO ()
callbackLoop port input output =
  bracket
    (do s <- Network.listenOn (Network.PortNumber (fromIntegral port))
        (hdl, _, _) <- Network.accept s
        hSetBuffering hdl (BlockBuffering Nothing)
        hSetBinaryMode hdl True
        return (s, hdl))
    (\(s, hdl) ->
      do hClose hdl
         close s)
    (\(_, hdl) ->
      do putStrLn "Callback loop ready."
         forever (do chunk <- BS.hGetSome hdl 8192
                     writeChan input chunk
                     void (readChan output)))

-- | Loop to interpret Haskell codes with GHC.
ghcLoop :: Int -> ThreadId -> Chan ByteString -> Chan ByteString -> IO ()
ghcLoop port parentThread input output =
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
                 setContext [IIModule (mkModuleName client)])
          void (evalStatement (getCallbackHandle_stmt port))
          void (evalDec (callback_dec'))
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
  liftIO (getChanContents input) >>=
  mapM_ (\x ->
           let x' = BS.unpack x
           in (evalShow x'
               `gcatch` \(SomeException _) -> evalVoid x'
               `gcatch` \(SomeException _) -> evalStatement x'
               `gcatch` \(SomeException _) -> evalDump x'
               `gcatch` \(SomeException _) -> evalLoadOrImport x'
               `gcatch` \(SomeException _) -> evalDec x'
               `gcatch` \(SomeException e) -> return (show e))
               >>= liftIO . writeChan output . BS.pack)

evalDump :: String -> Ghc String
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
       return "dumped hsc_env."
  | ":dump_names" `isPrefixOf` expr =
    do names <- getNamesInScope
       dflags <- getSessionDynFlags
       liftIO (mapM_ (putStrLn . showPpr dflags) names)
       return "dumped names."
  | ":dump_info" `isPrefixOf` expr =
    do df <- getSessionDynFlags
       let expr' = dropWhile isSpace (drop 10 expr)
           pp :: Outputable o => o -> String
           pp   = showPpr df
       names <- parseName expr'
       mbInfo:_ <- mapM (getInfo True) names
       return
         (maybe ("not in scope: " ++ expr')
                (\(t,_f,cs,_fs) ->
                   let t' = showSDocUnqual df (pprTyThingHdr t)
                       is = t' : map pp cs
                   in  concat (intersperse "\n" is))
                 mbInfo)
  | otherwise = error "Not a dump command."
{-# INLINE evalDump #-}

evalLoadOrImport :: String -> Ghc String
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
       return ("loaded: " ++ showPpr dflags target)
  | otherwise                 =
    do mdl <- parseImportDecl expr
       getContext >>= setContext . (IIDecl mdl :)
       dflags <- getSessionDynFlags
       return (showPpr dflags mdl)
{-# INLINE evalLoadOrImport #-}

evalVoid :: String -> Ghc String
evalVoid expr =
  do hvalue <- compileExpr expr
     liftIO (do unsafeCoerce# hvalue :: IO ()
                return "<<IO ()>>")
{-# INLINE evalVoid #-}

evalShow :: String -> Ghc String
evalShow expr =
  fmap unsafeCoerce# (compileExpr ("Prelude.show (" ++ expr ++ ")"))
{-# INLINE evalShow #-}

evalDec :: String -> Ghc String
evalDec dec =
  do names <- runDecls dec
     dflags <- getSessionDynFlags
     return (if null names
                then "dec: no names bound."
                else "dec: " ++ concatMap (showPpr dflags) names)
{-# INLINE evalDec #-}

evalStatement :: String -> Ghc String
evalStatement stmt =
  do res' <- runStmt stmt RunToCompletion
     case res' of
       RunOk names ->
         do dflags <- getSessionDynFlags
            return
              (if null names
                  then "stmt: no names bound."
                  else "stmt: " ++ unwords (map (showPpr dflags) names))
       RunException e ->
         liftIO (do putStrLn ("RunException: " ++ show e)
                    return (show e))
       RunBreak {} ->
         liftIO (do putStrLn "Got RunBreak"
                    return "RunBreak")
{-# INLINE evalStatement #-}


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
