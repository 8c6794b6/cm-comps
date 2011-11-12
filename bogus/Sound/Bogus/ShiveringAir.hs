{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Main entry point for /shivering air/.
-}
module Sound.Bogus.ShiveringAir where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Bogus.ShiveringAir.Synthdefs

main :: IO ()
main = withSC3 $ \fd -> do
  reset fd
  setup'sa fd
  patchNode (nodify sand) fd