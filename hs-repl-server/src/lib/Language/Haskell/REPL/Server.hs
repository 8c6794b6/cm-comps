{-# LANGUAGE MagicHash #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Simple REPL server with GHC.

-}
module Language.Haskell.REPL.Server where

import Language.Haskell.REPL.Global

import GHC
import GHC.Paths
import DynFlags
import Exception
import Outputable (showPpr)
import HscMain (hscParseIdentifier, hscTcRnLookupRdrName)
import Linker (getHValue)

import GHC.Prim (unsafeCoerce#)

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Dynamic
import Data.List (isPrefixOf)
import Network
import System.IO

-- --------------------------------------------------------------------------
--
-- Server
--
-- --------------------------------------------------------------------------

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
           input <- newEmptyMVar
           result <- newEmptyMVar
           putStrLn (unwords ["Client connected from "
                             , host ++ ":" ++ show clientPort])
           hSetBuffering hdl LineBuffering
           _ <- forkIO (handleLoop hdl host clientPort input result)
           forkIO (ghcLoop tid input result))))

handleLoop
  :: Handle -> HostName -> PortNumber -> MVar String -> MVar String -> IO ()
handleLoop hdl host port input result =
  forever
    (do ln <- hGetLine hdl
        unless
          (all isSpace ln)
          (do putStrLn ("[" ++ host ++ ":" ++ show port ++ "] " ++ ln)
              putMVar input ln
              hPutStr hdl =<< takeMVar result
              hFlush hdl))

ghcLoop :: ThreadId -> MVar String -> MVar String -> IO ()
ghcLoop parentThread input result =
  (defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc
      (Just libdir)
      (do dflags <- getSessionDynFlags
          _pkgs <- setSessionDynFlags
                    dflags {verbosity = 0
                           ,packageFlags = [ExposePackage "ghc"]
                           ,hscTarget = HscInterpreted
                           ,ghcLink = LinkInMemory
                           ,importPaths = [".", "src/lib"]}
          setContext . map IIDecl =<< mapM parseImportDecl ["import Prelude"]
          hsc_env <- getSession
          liftIO (initServerHscEnv hsc_env)
          forever (replStep input result))))
  `catch`
  (\UserInterrupt ->
     do putStrLn "Got user interrupt, killing server."
        throwTo parentThread UserInterrupt)
  `catch`
  (\(SomeException e) ->
     do putStrLn (show e)
        ghcLoop parentThread input result)

replStep :: MVar String -> MVar String -> Ghc ()
replStep input result =
  do expr <- liftIO (takeMVar input)
     res <- loadOrImport expr  `gcatch`
            (\(SomeException _) -> evalIO expr) `gcatch`
            (\(SomeException _) -> evalShow expr) `gcatch`
            (\(SomeException _) -> runStatement expr) `gcatch`
            (\(SomeException _) -> runDec expr) `gcatch`
            (\(SomeException e) -> return (show e))
     liftIO (putMVar result res)

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
       liftIO . initServerHscEnv =<< getSession
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
     return ("bound: " ++ concatMap (showPpr dflags) names)

runStatement :: String -> Ghc String
runStatement stmt =
  do res' <- runStmt stmt RunToCompletion
     case res' of
       RunOk names ->
         do dflags <- getSessionDynFlags
            (return ("bound: " ++ unwords (map (showPpr dflags) names)))
       RunException e ->
           liftIO (do putStrLn ("RunException: " ++ show e)
                      return (show e))
       RunBreak {} ->
           liftIO (do putStrLn "Got RunBreak"
                      return "RunBreak")


-- --------------------------------------------------------------------------
--
-- Client
--
-- --------------------------------------------------------------------------

callback
  :: String -- ^ Name of function with type @a -> IO a@.
  -> a      -- ^ Argument to the function.
  -> IO a
callback name arg =
  do hsc_env <- readServerHscEnv
     L _ rdr_name <- hscParseIdentifier hsc_env name
     names <- hscTcRnLookupRdrName hsc_env rdr_name
     hvalues <- mapM (getHValue hsc_env) names
     case hvalues of
       hvalue:_ -> unsafeCoerce# hvalue arg
       []       -> error ("callback: Cannot find " ++ name)
