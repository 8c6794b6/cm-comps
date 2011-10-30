{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Rewrite of /Understanding Streams, Patterns, and Events/ example from
SuperCollider help file.

-}
module Sound.Study.Porting.SPE where

import System.Random

import Sound.OpenSoundControl
import Sound.SC3 hiding (limiter)
import Sound.SC3.ID hiding (limiter)
import Sound.SC3.Lepton

main :: IO ()
main = withSC3 $ \fd -> do
  setup'spe fd
  patchNode (nodify nspe) fd
  play fd $ toL pspe

setup'spe :: Transport t => t -> IO ()
setup'spe fd = do
  let go (n,u) = send fd . d_recv . synthdef n $ u
  rs <- randomRs (0,0.05) `fmap` newStdGen
  ls <- randomRs (0,0.05) `fmap` newStdGen
  mapM_ go [("speSynth",speSynth),("apf",apf ls rs)]

------------------------------------------------------------------------------
-- Node mapping

nspe :: Nd
nspe =
  grp 0
  [ grp 1
    [ syn "apf" ["out"*=0,"in"*=0] ]]

------------------------------------------------------------------------------
-- Synthdef

speSynth :: UGen
speSynth = out 0 (v * ("amp"@@0.3)) where
  v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
  evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
  shp = envPerc 10e-3 1
  nz = midiCPS (lfNoise1 'ζ' KR 1 * 36 + 110)
  freq = control KR "freq" 440

apf :: [Double] -> [Double] -> UGen
apf ls rs = replaceOut ("out"@@0) sig where
  sig = foldr f v (take 4 $ zip ls rs)
  f (l,r) b = allpassN b (0.05) (mce [constant l,constant r]) 4
  v = in' 1 AR ("in"@@0)

------------------------------------------------------------------------------
-- Patterns

pspe = psnew "speSynth" Nothing AddToHead 1
  [("dur",pforever (d 0.13))
  ,("amp",pforever (d 0.6))
  ,("freq",pmidiCPS pspeF)]

pspeF =
  pcycle
  [ prep (pir 0 1)
    (pconcat (ds [24,31,36,43,48,55]))
  , prep (pir 2 5)
    (pconcat [d 60,prnd (ds [63,65]),d 67,prnd (ds [70,72,74])])
  , prep (pir 3 9)
    (prnd (ds [74,75,77,79,81]))]

------------------------------------------------------------------------------
-- Helpers

i = pint
d = pdouble
ds = map d
pir l h = pirange (i l) (i h)
prnd = prand (i 1)
prep = preplicate
