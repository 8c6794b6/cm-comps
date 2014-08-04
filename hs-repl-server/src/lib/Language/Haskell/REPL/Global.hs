{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-cse #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Simple REPL server with GHC, client side name lookup.
-}
module Language.Haskell.REPL.Global where

import GHC (HscEnv)

import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

server_hsc_env :: IORef (MVar HscEnv)
server_hsc_env =
  unsafePerformIO
    (newMVar (error "server_hsc_env not initialized") >>= newIORef)
{-# NOINLINE server_hsc_env #-}


initServerHscEnv :: HscEnv -> IO ()
initServerHscEnv he =
  do putStrLn "Initialized server_hsc_env"
     mvar <- readIORef server_hsc_env
     takeMVar mvar >> putMVar mvar he

readServerHscEnv :: IO HscEnv
readServerHscEnv = readMVar =<< readIORef server_hsc_env
