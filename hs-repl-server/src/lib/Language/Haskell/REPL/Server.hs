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
     tids <- newMVar [me]
     withSocketsDo
       (bracket
          (listenOn (PortNumber (fromIntegral port)))
          (\s -> do mapM_ killThread =<< readMVar tids
                    sClose s)
          (\sock ->
            do inp <- newEmptyMVar :: IO (MVar String)
               res <- newEmptyMVar :: IO (MVar String)
               tid <- forkIO (ghcLoop inp res)
               modifyMVar_ tids (return . (tid :))
               serverLoop sock inp res))

serverLoop :: Socket -> MVar String -> MVar String -> IO ()
serverLoop sock inp res =
  do (hdl, _, _) <- accept sock
     bracket
       (return hdl)
       (\h -> hClose h >> sClose sock)
       (\h -> do hSetBuffering h NoBuffering
                 _ <- forkIO (go h)
                 serverLoop sock inp res)
  where
    go h =
      do ln <- hGetLine h
         unless
           (all isSpace ln)
           (do putStrLn ("serverLoop: " ++ ln)
               putMVar inp ln
               hPutStr h =<< takeMVar res
               hFlush h
               go h)


ghcLoop :: MVar String -> MVar String -> IO ()
ghcLoop inp res =
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
                            ,ghcMode = CompManager
                            ,importPaths = [".", "src/lib"]}
          setContext . map IIDecl =<< mapM parseImportDecl ["import Prelude"]
          getSession >>= liftIO . putMVar server_hsc_env
          replLoop inp res))

replLoop :: MVar String -> MVar String -> Ghc ()
replLoop inp res =
  do expr <- liftIO (takeMVar inp)
     tryEvalIO expr res
     replLoop inp res

tryEvalIO :: String -> MVar String -> Ghc ()
tryEvalIO expr res =
  do hvalue <- gtry (dynCompileExpr expr) :: Ghc (Either SomeException Dynamic)
     case fmap fromDynamic hvalue of
       Right (Just act) -> liftIO (do act :: IO ()
                                      putMVar res "<<IO ()>>")
       _               -> tryEvalShow expr res

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
       Left err -> liftIO (do print (err :: SomeException)
                              putMVar res (show err))
       Right r  ->
         case r of
           RunOk names ->
             do dflags <- getSessionDynFlags
                liftIO (putMVar res ("bound: " ++
                                     unwords (map (showPpr dflags) names)))
           _           ->
             liftIO (do putStrLn "Statement result not RunOk"
                        putMVar res "<<Statement>>")
