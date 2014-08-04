{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Simple REPL server with GHC, client side name lookup.
-}
module Language.Haskell.REPL.Callback where

import Data.IORef
import GHC
import Linker (getHValue)
import System.IO.Unsafe

-- | Unsafe global state.
server_hsc_env :: IORef HscEnv
server_hsc_env =
  unsafePerformIO (newIORef (error "server_hsc_env not initialized"))
{-# NOINLINE server_hsc_env #-}

putServerHscEnv :: HscEnv -> IO ()
putServerHscEnv hsc_env = writeIORef server_hsc_env hsc_env
{-# NOINLINE putServerHscEnv #-}

callback :: String -> a -> IO a
callback _name _arg =
  do hsc_env <- readIORef server_hsc_env
     let name = undefined
     _hvalue <- getHValue hsc_env name
     return undefined

pure_function_in_callback :: Int -> Int
pure_function_in_callback x = x + 200
