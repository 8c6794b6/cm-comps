{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Common codes used in Bogus.
-}
module Sound.Bogus.Common where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad.Reader
import Control.Monad.State
import Data.Word

import Sound.OpenSoundControl
import Sound.SC3

-- | Newtype to run send messages to default scsynth.
newtype SC3 a =
  SC3 {unSC3 :: StateT (Int,Word64) (ReaderT UDP IO) a} deriving
  ( Functor, Applicative, Monad
  , MonadReader UDP, MonadState (Int,Word64), MonadIO)

-- | Communicate with default scsynth running on localhost:57110, UDP.
runSC3 :: SC3 a -> IO a
runSC3 m = withSC3 $ \fd -> do
  bracket
    (async fd (notify True) >> return fd)
    (\fd' -> send fd' $ notify False)
    (\fd' -> do
        t0 <- ntpi
        runReaderT (evalStateT (unSC3 m) (0,t0+10000)) fd')
