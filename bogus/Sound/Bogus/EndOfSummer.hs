{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Main entry point for /end of summer/.
-}
module Sound.Bogus.EndOfSummer where

import Control.Concurrent

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Bogus.EndOfSummer.Nodes
import Sound.Bogus.EndOfSummer.Synthdefs

main :: IO ()
main = withSC3 $ \fd -> do
  setup'eos fd
  patchNode t3 fd
  threadDelay (20*1000000)
  patchNode t1 fd

setup'eos :: Transport t => t -> IO OSC
setup'eos fd = do
  async fd $ bundle immediately $ map (\(n,u) -> d_recv $ synthdef n u)
    [("cefoo", cefoo)
    ,("cebar", cebar)
    ,("cebuzz", cebuzz)
    ,("cequux", cequux)
    ,("cehoge", cehoge)
    ,("cepippo", cepippo)
    ,("lfsin", lfsin)
    ,("lftri", lftri)
    ,("lfnz", lfnz)
    ,("lftrig", lftrig)
    ,("lfdust", lfdust)
    ]