{-# LANGUAGE DeriveDataTypeable #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Client side code for function callback in REPL.

-}
module Language.Haskell.Replenish.Client where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as BS
import           Network                   (PortID (..), connectTo)
import           Sound.OSC                 (pauseThreadUntil)
import           System.IO                 (BufferMode (..), Handle, hFlush,
                                            hSetBinaryMode, hSetBuffering)

import Data.Data (Data, Typeable)

_callback :: Show a => Handle -> Double -> String -> a -> IO ()
_callback hdl scheduled name args =
  void
    (forkIO
      (do pauseThreadUntil scheduled
          BS.hPutStrLn hdl (BS.unwords (map BS.pack [name, show args]))
          hFlush hdl))

_getCallbackHandle :: Int -> IO Handle
_getCallbackHandle port =
  do hdl <- connectTo "127.0.0.1" (PortNumber (fromIntegral port))
     hSetBuffering hdl (BlockBuffering Nothing)
     hSetBinaryMode hdl True
     return hdl

getCallbackHandle_stmt :: Int -> String
getCallbackHandle_stmt port = "__cb_hdl__ <- _getCallbackHandle " ++ show port

callback_dec' :: String
callback_dec' =
  unlines ["callback :: Show a => Double -> String -> a -> IO ()"
          ,"callback = _callback __cb_hdl__"]

data Callback = Callback {cbTime :: Double
                         ,cbFunc :: String
                         ,cbArgs :: String}
              | End
              deriving (Eq, Show, Data, Typeable)

callback :: Show a => Double -> String -> a -> IO Callback
callback scheduled f args = return (Callback scheduled f (show args))
