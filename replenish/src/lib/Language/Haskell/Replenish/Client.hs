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

data Callback = Callback {cbTime :: Double
                         ,cbFunc :: String
                         ,cbArgs :: String}
              | End
              deriving (Eq, Data, Typeable)

callback :: Show a => Double -> String -> a -> IO Callback
callback scheduled f args = return (Callback scheduled f (show args))

data RawString = RawString String

instance Show RawString where
  show (RawString str) = str

readyMessage :: RawString
readyMessage = RawString "GHC loop ready."
