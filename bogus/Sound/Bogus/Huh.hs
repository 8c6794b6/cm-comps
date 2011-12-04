{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Main entry point for /huh/.
-}
module Sound.Bogus.Huh where

import Control.Exception (bracket_)

import Sound.SC3
import Sound.SC3.Lepton

import Sound.Bogus.Huh.Patterns
import Sound.Bogus.Huh.Nodes
import Sound.Bogus.Huh.Synthdefs

huh :: IO ()
huh = withSC3 $ \fd -> bracket_
  (reset fd >> setupHuh fd >> patchNode (nodify n0) fd)
  (reset fd)
  (play fd $ toL allP)
