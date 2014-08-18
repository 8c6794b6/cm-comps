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

import GHC.IO.Handle
import Data.Data (Data, Typeable)
import System.IO

data Callback = Callback {cbTime :: Double
                         ,cbFunc :: String
                         ,cbArgs :: String}
              | End
              deriving (Eq, Data, Typeable)

callback :: Show a => Double -> String -> a -> IO Callback
callback scheduled f args = return (Callback scheduled f (show args))

intercept :: FilePath -> IO a -> IO a
intercept path act =
  withFile
    path AppendMode
    (\myout -> do stdout' <- hDuplicate stdout
                  hDuplicateTo myout stdout
                  a <- act
                  hFlush myout
                  hDuplicateTo stdout' stdout
                  return a)
