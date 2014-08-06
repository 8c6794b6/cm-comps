{-# LANGUAGE MagicHash #-}
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
import           GHC.Paths              (libdir)
import           HscTypes
import           Outputable             (Outputable (..), showPpr)

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reload   (getCabalPackageConf,
                                         getCabalSourcePaths)
import qualified Data.ByteString.Char8 as BS
import           Data.Char              (isSpace)
import           Data.Dynamic           (fromDyn)
import           Data.IORef
import           Data.List              (isPrefixOf)
import           Network
import qualified Network.Socket.ByteString as ByteString
import qualified Network.Socket         as Socket
import           System.IO

runServer :: Int -> IO ()
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
           putStrLn (unwords ["Client connected from "
                             , host ++ ":" ++ show clientPort])
           hSetBuffering hdl LineBuffering
           _ <- forkIO (handleLoop hdl host clientPort input output)
           _ <- forkIO (callbackLoop (port + 1) input output)
           forkIO (ghcLoop tid input output))))

handleLoop
  :: Handle -> HostName -> PortNumber -> Chan String -> Chan String -> IO ()
handleLoop hdl host clientPort input result = go [] False
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
         BS.hPutStr hdl . BS.pack =<< readChan result
         hFlush hdl

ghcLoop :: ThreadId -> Chan String -> Chan String -> IO ()
ghcLoop parentThread input result =
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
           imports <- return . map IIDecl
                      =<< mapM parseImportDecl ["import Prelude"]
           target <- guessTarget "Language.Haskell.REPL.Server" Nothing
           setTargets [target]
           _ <- load LoadAllTargets
           setContext (IIModule (mkModuleName "Language.Haskell.REPL.Server")
                      : imports )
           liftIO (putStrLn "Preparing callback")
           liftIO . putStrLn =<<
             runStatement ("__sock__ <- getCallbackSocket 9238")
           liftIO . putStrLn =<<
             runDec (unlines ["callback :: Show a => String -> a -> IO ()"
                             ,"callback = _callback __sock__"])
           forever (replStep input result))))
  `catch`
  (\UserInterrupt ->
     do putStrLn "Got user interrupt, killing server."
        throwTo parentThread UserInterrupt)
  `catch`
  (\(SomeException e) ->
     do putStrLn (show e)
        ghcLoop parentThread input result)

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

_callback :: Show a => Socket -> String -> a -> IO ()
_callback sock name args =
  void (forkIO
          (void (ByteString.send sock (BS.pack (name ++ " " ++ show args)))))

replStep :: Chan String -> Chan String -> Ghc ()
replStep input result =
  do expr <- liftIO (readChan input)
     res <- evalIO expr
            `gcatch` (\(SomeException _) -> evalShow expr)
            `gcatch` (\(SomeException _) -> runStatement expr)
            `gcatch` (\(SomeException _) -> dumpHscEnv expr)
            `gcatch` (\(SomeException _) -> loadOrImport expr)
            `gcatch` (\(SomeException _) -> runDec expr)
            `gcatch` (\(SomeException e) -> return (show e))
     liftIO (writeChan result res)

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

evalIO :: String -> Ghc String
evalIO expr =
  do hvalue <- dynCompileExpr expr
     liftIO (do fromDyn hvalue (undefined :: IO ())
                return "<<IO ()>>")

evalShow :: String -> Ghc String
evalShow expr =
  do hvalue <- dynCompileExpr ("Prelude.show (" ++ expr ++ ")")
     let hv = fromDyn hvalue (undefined :: String)
     return hv

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
