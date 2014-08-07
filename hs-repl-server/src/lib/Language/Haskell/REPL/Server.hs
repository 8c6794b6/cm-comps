{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Simple REPL server with GHC.

-}
module Language.Haskell.REPL.Server where

import           DynFlags
import           Exception
import           GHC
import           GHC.Paths                 (libdir)
import           HscTypes
import           Outputable                (Outputable (..), showPpr)

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reload      (getCabalPackageConf,
                                            getCabalSourcePaths)
import qualified Data.ByteString.Char8     as BS
import           Data.Char                 (isSpace)
import           Data.IORef
import           Data.List                 (isPrefixOf)
import           Network
import qualified Network.Socket            as Socket
import qualified Network.Socket.ByteString as ByteString
import           System.IO                 (BufferMode (..), Handle, hFlush,
                                            hSetBuffering)
import           Unsafe.Coerce             (unsafeCoerce)

import           Sound.OSC


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
  withSocketsDo
   (bracket
    (do s <- listenOn (PortNumber (fromIntegral port))
        me <- myThreadId
        return (s, me))
    (\(s, _) ->
       do putStr "Server killed, closing socket"
          sClose s
          putStrLn "... done.")
    (\(s, tid) ->
      forever
       (do (hdl, host, clientPort) <- accept s
           input <- newChan
           output <- newChan
           putStrLn (unwords ["Client connected from"
                             , host ++ ":" ++ show clientPort])
           hSetBuffering hdl LineBuffering
           _ <- forkIO (handleLoop hdl host clientPort input output)
           _ <- forkIO (callbackLoop (port + 1) input output)
           _ <- forkIO (ghcLoop tid input output)
           return ())))

handleLoop
  :: Handle -> HostName -> PortNumber -> Chan String -> Chan String -> IO ()
handleLoop hdl host clientPort input output = go [] False
  where
    go acc isMultiLine =
      do ln <- BS.hGetLine hdl
         if isEmptyLine ln
            then go acc isMultiLine
            else do showLine ln
                    case (isMultiLine, ln) of
                      (True,  ":}") -> do doEval (BS.unlines (reverse acc))
                                          go [] False
                      (True,  _   ) -> go (ln:acc) isMultiLine
                      (False, ":{") -> go acc True
                      (False, _   ) -> do doEval ln
                                          go acc False
    isEmptyLine = BS.foldr (\c t -> isSpace c && t) True
    showLine bs =
      BS.putStrLn
        (BS.concat ["[", BS.pack host
                   ,":", BS.pack (show clientPort)
                   ,"] ", bs])
    doEval bs =
      do writeChan input (BS.unpack bs)
         BS.hPutStr hdl . BS.pack =<< readChan output
         hFlush hdl

ghcLoop :: ThreadId -> Chan String -> Chan String -> IO ()
ghcLoop parentThread input output =
  (defaultErrorHandler
     defaultFatalMessager
     defaultFlushOut
     (runGhc
       (Just libdir)
       (do srcPaths <- liftIO getCabalSourcePaths
           pkgDbs <- liftIO getCabalPackageConf
           dflags <- getSessionDynFlags
           _pkgs <- setSessionDynFlags
                     dflags {verbosity = 0
                            ,packageFlags = [ExposePackage "ghc"]
                            ,extraPkgConfs = const pkgDbs
                            ,hscTarget = HscInterpreted
                            ,ghcLink = LinkInMemory
                            ,importPaths = srcPaths}
           imps <- return . map IIDecl =<< mapM parseImportDecl initialImports
           target <- guessTarget "Language.Haskell.REPL.Server" Nothing
           setTargets [target]
           _ <- load LoadAllTargets
           setContext (IIModule (mkModuleName "Language.Haskell.REPL.Server")
                      : imps )
           void (runStatement ("__sock__ <- getCallbackSocket 9238"))
           void (runDec
                  (unlines ["callback :: Show a => Time -> String -> a -> IO ()"
                           ,"callback = _callback __sock__"]))
           liftIO (putStrLn "GHC loop ready.")
           forever (replStep input output))))
  `catch`
  (\UserInterrupt ->
     do putStrLn "Got user interrupt, killing server."
        throwTo parentThread UserInterrupt)
  `catch`
  (\(SomeException e) ->
     do putStrLn (show e)
        ghcLoop parentThread input output)

initialImports :: [String]
initialImports = ["import Prelude"]

callbackLoop :: Int -> Chan String -> Chan String -> IO ()
callbackLoop port input output =
  bracket
    (do (serverAddr:_) <- Socket.getAddrInfo
                            (Just
                               (Socket.defaultHints
                                 {Socket.addrFlags = [Socket.AI_PASSIVE]}))
                            Nothing
                            (Just (show port))
        sock <- Socket.socket
                  (Socket.addrFamily serverAddr)
                  Socket.Datagram
                  Socket.defaultProtocol
        Socket.bindSocket sock (Socket.addrAddress serverAddr)
        return sock)
    Socket.sClose
    (\sock ->
      forever (do (msg, _) <- ByteString.recvFrom sock 2048
                  writeChan input (BS.unpack msg)
                  _ <- readChan output
                  return ()))

replStep :: Chan String -> Chan String -> Ghc ()
replStep input output =
  do expr <- liftIO (readChan input)
     res <- evalShow expr
            `gcatch` (\(SomeException _) -> evalVoid expr)
            `gcatch` (\(SomeException _) -> runStatement expr)
            `gcatch` (\(SomeException _) -> dumpHscEnv expr)
            `gcatch` (\(SomeException _) -> loadOrImport expr)
            `gcatch` (\(SomeException _) -> runDec expr)
            `gcatch` (\(SomeException e) -> return (show e))
     liftIO (writeChan output res)

dumpHscEnv :: String -> Ghc String
dumpHscEnv expr
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
  | otherwise = error "Not a dump command."

loadOrImport :: String -> Ghc String
loadOrImport expr
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

evalVoid :: String -> Ghc String
evalVoid expr =
  do hvalue <- compileExpr expr
     liftIO (do unsafeCoerce hvalue :: IO ()
                return "<<IO ()>>")

evalShow :: String -> Ghc String
evalShow expr =
  fmap unsafeCoerce (compileExpr ("Prelude.show (" ++ expr ++ ")"))

runDec :: String -> Ghc String
runDec dec =
  do names <- runDecls dec
     dflags <- getSessionDynFlags
     return ("dec: " ++ concatMap (showPpr dflags) names)

runStatement :: String -> Ghc String
runStatement stmt =
  do res' <- runStmt stmt RunToCompletion
     case res' of
       RunOk names ->
         do dflags <- getSessionDynFlags
            return
              (if null names
                  then ""
                  else ("stmt: " ++ unwords (map (showPpr dflags) names)))
       RunException e ->
         liftIO (do putStrLn ("RunException: " ++ show e)
                    return (show e))
       RunBreak {} ->
         liftIO (do putStrLn "Got RunBreak"
                    return "RunBreak")


-- --------------------------------------------------------------------------
--
-- Client side functions for callback
--
-- --------------------------------------------------------------------------

getCallbackSocket :: Int -> IO Socket
getCallbackSocket port =
  do (serverAddr:_) <- Socket.getAddrInfo
                         Nothing (Just "127.0.0.1") (Just (show port))
     sock <- Socket.socket
               (Socket.addrFamily serverAddr)
               Socket.Datagram
               Socket.defaultProtocol
     Socket.connect sock (Socket.addrAddress serverAddr)
     return sock

_callback :: Show a => Socket -> Time -> String -> a -> IO ()
_callback sock scheduled name args =
  void (forkIO
          (do pauseThreadUntil scheduled
              void (ByteString.send
                      sock (BS.pack (name ++ " " ++ show args)))))
