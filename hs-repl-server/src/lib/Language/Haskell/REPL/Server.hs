{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Simple REPL server with GHC, with name lookup.

-}
module Language.Haskell.REPL.Server where

import Language.Haskell.REPL.Callback

import GHC
import GHC.Paths
import DynFlags
import Exception
import Outputable

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Dynamic
import Data.List (isPrefixOf)
import Network
import System.IO

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
          getSession >>= liftIO . putMVar server_hsc_env
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
     res <- loadOrImport expr `gcatch`
            (\(SomeException _) -> evalIO expr) `gcatch`
            (\(SomeException _) -> evalShow expr) `gcatch`
            (\(SomeException _) -> runToCompletion expr) `gcatch`
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
               map (\m -> "import " ++ moduleNameString m)
                   loaded)
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
     liftIO (do _ <- fromDyn hvalue (undefined :: IO ())
                return "<<IO ()>>")

evalShow :: String -> Ghc String
evalShow expr =
  do hvalue <- dynCompileExpr ("Prelude.show (" ++ expr ++ ")")
     let hv = fromDyn hvalue (undefined :: String)
     liftIO (putStrLn hv >> return hv)

runToCompletion :: String -> Ghc String
runToCompletion stmt =
  do res' <- (runStmt stmt RunToCompletion)
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
