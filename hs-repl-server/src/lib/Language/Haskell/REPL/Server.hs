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
import Network
import System.IO

runServer :: Int -> IO ()
runServer port =
  do me <- myThreadId
     withSocketsDo
      (bracket
       (listenOn (PortNumber (fromIntegral port)))
       (\s ->
          do putStr "Server killed, closing socket..."
             sClose s
             putStrLn " done.")
       (\s ->
         forever
          (do (hdl, host, clientPort) <- accept s
              input <- newEmptyMVar
              result <- newEmptyMVar
              putStrLn (unwords ["Client connected from "
                                , host ++ ":" ++ show clientPort])
              hSetBuffering hdl LineBuffering
              forkIO (handleLoop hdl host clientPort input result)
              forkIO (ghcLoop me input result))))

handleLoop
  :: Handle -> HostName -> PortNumber -> MVar String -> MVar String -> IO ()
handleLoop hdl host port input result =
  forever
    (do ln <- hGetLine hdl
        unless (all isSpace ln)
               (do putStrLn (host ++ ":" ++ show port ++ " " ++ ln)
                   putMVar input ln
                   hPutStr hdl =<< takeMVar result
                   hFlush hdl))

ghcLoop :: ThreadId -> MVar String -> MVar String -> IO ()
ghcLoop parentThread input result =
  defaultErrorHandler
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
         forever (replStep input result)))
  `catch`
   (\UserInterrupt ->
      do putStrLn "Got user interrupt, killing server."
         throwTo parentThread UserInterrupt)

replStep :: MVar String -> MVar String -> Ghc ()
replStep input result =
  do expr <- liftIO (takeMVar input)
     tryEvalIO expr result

tryEvalIO :: String -> MVar String -> Ghc ()
tryEvalIO expr res =
  do hvalue <- gtry (dynCompileExpr expr) :: Ghc (Either SomeException Dynamic)
     case fmap fromDynamic hvalue of
       Right (Just act) -> liftIO (do act :: IO ()
                                      putMVar res "<<IO ()>>")
       _                -> tryEvalShow expr res

tryEvalShow :: String -> MVar String -> Ghc ()
tryEvalShow expr res =
  do hvalue <- gtry (dynCompileExpr ("Prelude.show (" ++ expr ++ ")"))
                    :: Ghc (Either SomeException Dynamic)
     case fmap fromDynamic hvalue of
       Right (Just hv) -> liftIO (putStrLn hv >> putMVar res hv)
       _               -> tryRunStatement expr res

tryRunStatement :: String -> MVar String -> Ghc ()
tryRunStatement stmt res =
  do res' <- gtry (runStmt stmt RunToCompletion)
     case res' of
       Left (SomeException e) ->
         liftIO (do print e
                    putMVar res (show e))
       Right r  ->
         case r of
           RunOk names ->
             do dflags <- getSessionDynFlags
                liftIO (putMVar res ("bound: " ++
                                     unwords (map (showPpr dflags) names)))
           _           ->
             liftIO (do putStrLn "Statement result not RunOk"
                        putMVar res "<<Statement>>")
