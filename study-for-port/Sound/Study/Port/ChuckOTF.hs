{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Port of /on-the-fly synchronization (concurrent)/ chuck example
by Perry and Ge, from: <http://chuck.cs.princeton.edu/doc/examples/> .

-}
module Sound.Study.Port.ChuckOTF where

import System.FilePath ((</>))

import Sound.OpenSoundControl (bundle, immediately)
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

------------------------------------------------------------------------------
-- Synth defs

otfperc :: UGen
otfperc = out ("out"@@0) sig where
  sig = pan2 (pb * ("amp"@@0.3)) ("pan"@@0) 1
  pb = playBuf 1 AR ("bnum"@@0) 1 1 0 NoLoop RemoveSynth

otfsine :: UGen
otfsine = out ("out"@@0) sig where
  sig = pan2 (sinOsc AR ("freq"@@0) 0 * e) ("pan"@@0) 1
  e = envGen KR 1 ("amp"@@0) 0 ("dur"@@1) RemoveSynth $
      env [0,1,1,0] [1e-3,998e-3,1e-3] [EnvLin] 0 0

otfrev1 :: UGen
otfrev1 = replaceOut ("out"@@0) sig where
  sig = freeVerb2 inl inr 0.5 0.5 0.5
  inl = (in' 1 AR ("inl"@@0))
  inr = (in' 1 AR ("inr"@@1))

otfrev2 :: UGen
otfrev2 = out ("out"@@0) sig where
  sig = ins * m + foldr f ins [1..8::Int] * (1-m)
  m = "mix"@@0.6
  ins = in' 2 AR ("in"@@0)
  f a b = allpassC b 0.8 (rand a 1e-3 8e-2) (rand a 4e-2 4)

------------------------------------------------------------------------------
-- Patterns

kikP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (d t))
  ,("bnum", pforever (d 0))
  ,("pan", pforever (d (-0.1)))
  ,("amp", pforever (pdrange (d 0.7) (d 0.9)))]

snrP = psnew "otfperc" Nothing AddToHead 1
  [("dur",
    (pforever (d t) *@ pforever (prnd [d 2, pconcat (ds [0.75,1.25])])))
  ,("bnum", pforever (d 1))
  ,("pan", pforever (d 0.3))]

hatP = psnew "otfperc" Nothing AddToHead 1
  [("dur",
    pforever (d t) *@ pforever (prnd [d 0.5, pconcat (ds [0.25,0.25])]))
  ,("bnum", pforever (d 2))
  ,("pan", pforever (d (-0.3)))
  ,("amp", pforever (pdrange (d 0.3) (d 0.4)))]

hatoP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (d t))
  ,("bnum", pforever (d 3))
  ,("pan", pforever (d (-0.3)))
  ,("amp", pforever (pdrange (d 0.2) (d 0.4)))]

sin1P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (d (0.25*t)))
  ,("out", pforever (d 2))
  ,("freq",
    let fs = [midiCPS (21+x+y)|x<-[0,12,24,36],y<-[0,2,4,7,9]] in
    pforever (prnd (ds fs)))
  ,("amp", pforever (d 0.65))
  ,("pan", pforever (d 0.1))]

sin2P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (prnd (ds [0.5*t,0.25*t])))
  ,("out", pforever (d 2))
  ,("freq",
    let fs = [midiCPS (69+x+y)|x<-[0,12,24,36],y<-[0,2,4,7,9]] in
    pforever (prnd (ds fs)))
  ,("amp", pforever (d 0.2))
  ,("pan", pforever (d (-0.2)))]
  
t = 0.5
d = pdouble
ds = map d
i = pint
prnd = prand (i 1)

sin1N = 0xbaca
sin2N = 0xcaba

setup'otf :: IO ()
setup'otf = withSC3 $ \fd -> do
  reset fd
  async fd $ bundle immediately $
    (map (d_recv . uncurry synthdef))
     [("otfperc",otfperc),("otfsine", otfsine)
     ,("otfrev1", otfrev1),("otfrev2",otfrev2)] ++
    (zipWith (\bid file -> b_allocRead bid (soundsDir </> file) 0 0)
     [0..]
     ["kick.wav", "snare-hop.wav", "hihat.wav", "hihat-open.wav"])
  patchNode (nodify otfNodes) fd

soundsDir = "/home/atsuro/Downloads/chuck-1.2.1.3/examples/data"

------------------------------------------------------------------------------
-- Nodes

otfNodes :: Nd
otfNodes =
  grp 0
  [ grp 1
    [ syn "otfrev1" []
    , syn "otfrev2" ["in"*=2,"mix"*=0.5] ]]
  
------------------------------------------------------------------------------
-- Running patterns

addKik = addPat 0 "kik" kikP
addHat = addPat 0 "hat" hatP
addHato = addPat 0 "hat-open" hatoP
addSnr = addPat t "snr" snrP
addSin1 = addPat 0 "sin-lo" sin1P
addSin2 = addPat 0 "sin-hi" sin2P
addPat s n e = withLept . flip send =<< bundle' (t*2) s [l_new n e]

resetAll = withLept (flip send l_freeAll)

addAll = sequence_ [addKik, addSnr, addHat, addHato, addSin1, addSin2]

addAllButSnare' = bundle' (t*2) 0
  [ l_new "kik" kikP, l_new "hat" hatP, l_new "hat-open" hatoP
  , l_new "sin-lo" sin1P, l_new "sin-hi" sin2P ]
  
-- XXX: Why this pattern sound different from above?
-- 
-- sin2P is not giving frequencies intended to be played, it differs 
-- when its run individually. This difference was also appearing in pure
-- implementation of L interpreter, using System.Random.Mersenne.Pure64.
-- 
addAllButSnare = leptseq =<< bundle' (t*2) 0
  [ l_new "all-without-snare"
    (ppar [kikP, hatP, hatoP, sin1P, sin2P]) ]

dumpPat = leptseq  l_dump
delPat n = leptseq =<< bundle' (t*2) 0 [l_free n]
pausePat n = leptseq =<< bundle' (t*2) 0 [l_pause n]
runPat n = leptseq =<< bundle' (t*2) 0 [l_run n]
