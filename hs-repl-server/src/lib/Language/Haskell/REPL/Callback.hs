module Language.Haskell.REPL.Callback where

import Control.Concurrent
import GHC
import Linker (getHValue)
import System.IO.Unsafe

-- | Unsafe global state.
server_hsc_env :: MVar HscEnv
server_hsc_env = unsafePerformIO newEmptyMVar
{-# NOINLINE server_hsc_env #-}

callback :: String -> a -> IO a
callback _name _arg =
  do hsc_env <- readMVar server_hsc_env
     let name = undefined
     _hvalue <- getHValue hsc_env name
     return undefined
