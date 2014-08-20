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

import Data.Data (Data, Typeable)
import qualified Data.ByteString.Char8 as BS
import System.IO (Handle, IOMode(..), openFile, hFlush)

newtype RawString = RawString String

instance Show RawString where
  show (RawString str) = str

readyMessage :: RawString
readyMessage = RawString "Server ready."

data Callback = Callback {cbTime :: {-# UNPACK #-} !Double
                         ,cbFunc :: String
                         ,cbArgs :: String}
              | End
              deriving (Eq, Data, Typeable)

callback :: Show a => Double -> String -> a -> IO Callback
callback scheduled f args = return (Callback scheduled f (show args))

__getHdl :: FilePath -> IO Handle
__getHdl path = openFile path WriteMode

__interactivePrint :: Show a => Handle -> a -> IO ()
__interactivePrint hdl str =
  do BS.hPutStr hdl (BS.pack (show str))
     hFlush hdl
