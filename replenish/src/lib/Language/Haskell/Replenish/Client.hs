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

-- | Data type for callback.
--
-- When an action had type: @IO Callback@, the server will re-run the function
-- specified by the returned callback value.
--
data Callback
  = -- | Holds information to run next function.
    Callback
      {cbTime :: {-# UNPACK #-} !Double
       -- ^ Scheduled time for next run.
      ,cbFunc :: String
       -- ^ Name of function.
      ,cbArgs :: String
       -- ^ Haskell source code string for argument.
      }
  | -- | Finish the callback.
    End
  deriving (Eq, Data, Typeable)

newtype RawString = RawString String

instance Show RawString where
  show (RawString str) = str

callback :: Show a => Double -> String -> a -> IO Callback
callback scheduled f args = return (Callback scheduled f (show args))

__getHdl :: FilePath -> IO Handle
__getHdl path = openFile path WriteMode

__interactivePrint :: Show a => Handle -> a -> IO ()
__interactivePrint hdl str =
  do BS.hPutStr hdl (BS.pack (show str))
     hFlush hdl

readyMessage :: RawString
readyMessage = RawString "Server ready."
