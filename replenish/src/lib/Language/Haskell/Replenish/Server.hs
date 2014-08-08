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
           _ <- forkIO (callbackLoop (fromIntegral clientPort + 1) input output)
           _ <- forkIO (ghcLoop (fromIntegral clientPort + 1) tid input output)
           return ())))

-- | Loop to get input and reply output with connected 'Handle'.
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
                      (True,  ":}") ->
                        do case acc of
                             []  -> return ()
                             [l] -> doEval l
                             _   -> doEval (BS.unlines (reverse acc))
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

-- | Loop to take care of callbacks.
callbackLoop :: Int -> Chan String -> Chan String -> IO ()
callbackLoop port input output =
  bracket
    (do (saddr:_) <- Socket.getAddrInfo
                       (Just
                          (Socket.defaultHints
                            {Socket.addrFlags = [Socket.AI_PASSIVE]}))
                       Nothing
                       (Just (show port))
        sock <- Socket.socket
                  (Socket.addrFamily saddr)
                  Socket.Datagram
                  Socket.defaultProtocol
        Socket.bindSocket sock (Socket.addrAddress saddr)
        return sock)
    Socket.sClose
    (\sock ->
      forever (do (msg, _) <- ByteString.recvFrom sock 2048
                  writeChan input (BS.unpack msg)
                  _ <- readChan output
                  return ()))

-- | Loop to interpret Haskell codes with GHC.
ghcLoop :: Int -> ThreadId -> Chan String -> Chan String -> IO ()
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
            (do putStrLn ("src:\n" ++
                          unlines (map ("  " ++) srcPaths))
                putStrLn ("package-dbs:\n" ++
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
          let me = "Language.Haskell.Replenish.Server"
          (setContext =<< mapM (fmap IIDecl . parseImportDecl)
                               (("import " ++ me) : initialImports))
            `gcatch`
            (\(SomeException e) ->
              do liftIO (do putStrLn ("Caught exception: " ++ show e)
                            putStrLn ("Loading " ++ me ++ " as target."))
                 target <- guessTarget me Nothing
                 setTargets [target]
                 _ <- load LoadAllTargets
                 setContext [IIModule (mkModuleName me)])
          liftIO (putStr "Binding callback function ...")
          void (evalStatement (getCallbackSocket_stmt port))
          void (evalDec callback_dec)
          liftIO (putStrLn " replenish server ready.")
          forever (replStep input output)))
  `catch`
  (\UserInterrupt ->
     do putStrLn "Got user interrupt, killing server."
        throwTo parentThread UserInterrupt)
  `catch`
  (\(SomeException e) ->
     do putStrLn (show e)
        ghcLoop port parentThread input output)

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

-- | Single step to evaluate Haskell code.
replStep :: Chan String -> Chan String -> Ghc ()
replStep input output =
  do expr <- liftIO (readChan input)
     res <- evalShow expr
            `gcatch` (\(SomeException _) -> evalVoid expr)
            `gcatch` (\(SomeException _) -> evalStatement expr)
            `gcatch` (\(SomeException _) -> evalDump expr)
            `gcatch` (\(SomeException _) -> evalLoadOrImport expr)
            `gcatch` (\(SomeException _) -> evalDec expr)
            `gcatch` (\(SomeException e) -> return (show e))
     liftIO (writeChan output res)

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
  | otherwise = error "Not a dump command."

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

evalVoid :: String -> Ghc String
evalVoid expr =
  do hvalue <- compileExpr expr
     liftIO (do unsafeCoerce hvalue :: IO ()
                return "<<IO ()>>")

evalShow :: String -> Ghc String
evalShow expr =
  fmap unsafeCoerce (compileExpr ("Prelude.show (" ++ expr ++ ")"))

evalDec :: String -> Ghc String
evalDec dec =
  do names <- runDecls dec
     dflags <- getSessionDynFlags
     return (if null names
                then "dec: no names bound."
                else "dec: " ++ concatMap (showPpr dflags) names)

evalStatement :: String -> Ghc String
evalStatement stmt =
  do res' <- runStmt stmt RunToCompletion
     case res' of
       RunOk names ->
         do dflags <- getSessionDynFlags
            return
              (if null names
                  then "stmt: no names bound."
                  else ("stmt: " ++ unwords (map (showPpr dflags) names)))
       RunException e ->
         liftIO (do putStrLn ("RunException: " ++ show e)
                    return (show e))
       RunBreak {} ->
         liftIO (do putStrLn "Got RunBreak"
                    return "RunBreak")


-- --------------------------------------------------------------------------
--
-- Client side code
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

getCallbackSocket_stmt :: Int -> String
getCallbackSocket_stmt port = "__sock__ <- getCallbackSocket " ++ show port

_callback :: Show a => Socket -> Time -> String -> a -> IO ()
_callback sock scheduled name args =
  void (forkIO
          (do pauseThreadUntil scheduled
              void (ByteString.send
                      sock (BS.pack (name ++ " " ++ show args)))))

callback_dec :: String
callback_dec =
  unlines ["callback :: Show a => Double -> String -> a -> IO ()"
          ,"callback = _callback __sock__"]
