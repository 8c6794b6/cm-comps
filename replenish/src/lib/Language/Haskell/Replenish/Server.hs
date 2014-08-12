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

import           Sound.OSC                 (Time, pauseThreadUntil)


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
       do putStr "Server killed, closing socket ... "
          sClose s
          putStrLn "done.")
    (\(s, tid) ->
      forever
       (do (hdl, host, clientPort) <- accept s
           input <- newChan
           output <- newChan
           putStrLn (unwords ["Client connected from"
                             , host ++ ":" ++ show clientPort])
           hSetBuffering hdl LineBuffering
           let clientPort' = fromIntegral clientPort + 1
           gtid <- forkIO (ghcLoop clientPort' tid input output)
           ctid <- forkIO (callbackLoop clientPort' input output)
           void
             (forkIO
                (handleLoop hdl host clientPort [gtid,ctid] input output)))))

-- | Loop to get input and reply output with connected 'Handle'.
handleLoop
  :: Handle -> HostName -> PortNumber -> [ThreadId]
  -> Chan String -> Chan String -> IO ()
handleLoop hdl host clientPort tids input output =
  go [] False
  `catch`
  (\(SomeException e) ->
     do putStr (unlines ["handleLoop: Caught " ++ show e
                        ,"Killing ghc and callback loop for " ++
                          host ++ ":" ++ show clientPort])
        mapM_ killThread tids)
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
      forever (do msg <- ByteString.recv sock 1024
                  writeChan input (BS.unpack msg)
                  void (readChan output)))

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
            (do putStr ("src:\n" ++
                        unlines (map ("  " ++) srcPaths))
                putStr ("package-dbs:\n" ++
                        unlines (map (("  " ++ ) . showPkgConfRef) pkgDbs))
                putStrLn "Setting up context ...")
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
          -- void (evalStatement (getCallbackSocket_stmt port))
          -- void (evalDec callback_dec)
          void (evalDec (callback_dec' port))
          liftIO (putStrLn "Server ready.")
          replStep input output))
  `catch`
  (\UserInterrupt ->
     do putStrLn "Got UserInterrupt, killing the server."
        throwTo parentThread UserInterrupt)
  `catch`
  (\(SomeException e) ->
     do putStrLn ("ghcLoop: " ++ show e))

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

-- | Evaluate Haskell code.
replStep :: Chan String -> Chan String -> Ghc ()
replStep input output =
  liftIO (getChanContents input) >>=
  mapM_ (\x -> (evalShow x
                `gcatch` (\(SomeException _) -> evalVoid x)
                `gcatch` (\(SomeException _) -> evalStatement x)
                `gcatch` (\(SomeException _) -> evalDump x)
                `gcatch` (\(SomeException _) -> evalLoadOrImport x)
                `gcatch` (\(SomeException _) -> evalDec x)
                `gcatch` (\(SomeException e) -> return (show e)))
                >>= liftIO . writeChan output)

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
     liftIO (do unsafeCoerce hvalue :: IO ()
                return "<<IO ()>>")
{-# INLINE evalVoid #-}

evalShow :: String -> Ghc String
evalShow expr =
  fmap unsafeCoerce (compileExpr ("Prelude.show (" ++ expr ++ ")"))
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
                  else ("stmt: " ++ unwords (map (showPpr dflags) names)))
       RunException e ->
         liftIO (do putStrLn ("RunException: " ++ show e)
                    return (show e))
       RunBreak {} ->
         liftIO (do putStrLn "Got RunBreak"
                    return "RunBreak")
{-# INLINE evalStatement #-}


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
              ByteString.sendAll sock (BS.pack (name ++ " " ++ show args))))

__callback :: Show a => Int -> Time -> String -> a -> IO ()
__callback port scheduled name args =
  void
    (forkIO
       (do pauseThreadUntil scheduled
           bracket
             (do addr:_ <- Socket.getAddrInfo
                             Nothing (Just "127.0.0.1") (Just (show port))
                 sock <- Socket.socket
                           (Socket.addrFamily addr)
                           Socket.Datagram
                           Socket.defaultProtocol
                 Socket.connect sock (Socket.addrAddress addr)
                 return sock)
             Socket.sClose
             (\sock ->
                ByteString.sendAll sock (BS.pack (name ++ " " ++ show args)))))

callback_dec' :: Int -> String
callback_dec' port =
  unlines ["callback :: Show a => Double -> String -> a -> IO ()"
          ,"callback = __callback " ++ show port]

callback_dec :: String
callback_dec =
  unlines ["callback :: Show a => Double -> String -> a -> IO ()"
          ,"callback = _callback __sock__"]
