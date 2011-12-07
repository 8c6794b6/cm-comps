{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Lost in the woods.

-}
module Sound.Bogus.LostInTheWoods where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

-- --------------------------------------------------------------------------
--
-- Synthdefs
--

setup_litw :: Transport t => t -> IO ()
setup_litw fd = mapM_ (async fd . d_recv . uncurry synthdef)
  [ ("rspdef1", rspdef1)
  , ("rspdef2", rspdef2)
  , ("rspdef3", rspdef3)
  , ("rspdef5", rspdef5)
  , ("rspctr1", rspctr1)
  , ("rspctr2", rspctr2)
  , ("rspmst", rspmst)]

rspdef1 :: UGen
rspdef1 =
  let phase = rand 'k' 0 pi in
  out 0 $ pan2
  (lfCub AR ("freq"@@440 * ("fmul"@@1 `lag2` 3.5)) phase * 0.3 *
   envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef2 :: UGen
rspdef2 =
  out 0 $ pan2
  (resonz (whiteNoise 'd' AR)
   ("freq"@@1320)
   (clip ("q"@@0.8) 1e-5 9999e-4) * 0.3 *
   envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvSin] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef3 :: UGen
rspdef3 = out ("out"@@100) (tExpRand 'f' 0.25 4 ("t_trig"@@1))

rspdef4 :: UGen
rspdef4 = out 0 $
  lfPar AR ("freq"@@440 `lag` 0.25) 0 * ("amp"@@0.3 `lag3` ("lagt"@@0.3))

rspdef5 :: UGen
rspdef5 =
  let sig = mix (sinOsc AR (mce [ofq,ffq]) pw * amp)
      pw = lfdNoise3 'p' KR 2 ** 4
      ofq = "freq"@@332.32 * fq0
      ffq = "freq"@@332.32 * fq3
      fq0 = mkfq lfdNoise0
      fq3 = mkfq lfdNoise3
      mkfq n = linExp (n 'ε' KR (1/3.13) + 1.01) 0.01 2.01 0.25 32
      amp = envGen KR tck ("amp"@@0.2) 0 (dur+5e-2) DoNothing shp
      dur = (1/tfq) * 0.985
      shp = env [0,1,0] [atk,1-atk] [EnvCub] (-1) 0
      tck = impulse KR tfq 0
      tfq = linLin (lfdNoise3 'ω' KR 0.25) (-1) 1 0.25 5.65 ** 2
      atk = linLin (sinOsc KR (1/37) 0) (-1) 1 1e-3 999e-3
      pan = sinOsc KR pfm 0 * 0.75
      pfm = sinOsc KR (1/7.32) 0 * pi + (1/82)
      rev = foldr apc sig ['a'..'h']
      apc a b = allpassC b 0.8 (rand a 1e-1 0.8) (rand a 4e-1 4)
  in  out ("out"@@0) (pan2 rev pan 1)

rspctr1 :: UGen
rspctr1 =
  let sig = lfdNoise3 'μ' KR frq * mul + add
      frq = "freq"@@1
      mul = "mul"@@1
      add = "add"@@0
  in  out ("out"@@0) sig

rspctr2 :: UGen
rspctr2 =
  let sig = sinOsc KR ("freq"@@1) ("phase"@@1) * ("mul"@@1) + ("add"@@0)
  in  out ("out"@@0) sig

rspmst :: UGen
rspmst =
  let sig0 = in' 2 AR 0
      sig1 = rhpf sig0 15 0.01
  in  replaceOut 0 sig1

-- --------------------------------------------------------------------------
--
-- Nodes
--

litwNd :: Nd
litwNd =
  grp 0
    [ grp 1
      [ grp 10
         [ loop01_freq
         , loop02_freq
         , loop04_amp
         , syn' 100004 "rspdef5" ["amp"*=0.1]]
      , grp 11 [] ]
    , grp 2
      [syn "rspmst" []] ]

loop01_freq :: Nd
loop01_freq =
  syn' 100001 "rspdef3" ["mul"*=2.1,"add"*=2.1,"freq"*=0.113]

loop02_freq :: Nd
loop02_freq =
  syn' 100002 "rspctr1"
    ["out"*=101,"add"*=0.5,"mul"*=0.5,"freq"*=0.0683]

loop04_amp :: Nd
loop04_amp =
  syn' 100003 "rspctr2"
    ["out"*=102,"add"*=0.2,"mul"*=0.2,"freq"*=0.025
    ,"phase"*=Dval(-pi/2)]

-- --------------------------------------------------------------------------
--
-- Patterns
--

psw = ppar [loop01, loop02, loop03, loop04]

{-
psw' =
  withLept . flip send =<< bundle' 0 0
  [ l_new "set03" set03
  , l_new "loop01" loop01
  , l_new "loop02" loop02
  , l_new "loop03" loop03 ]

set03 = psnew "rspdef3" (Just 100001) AddToHead 1 [("dur", pdouble 0.1)]
-}

loop01 =
  let d = pdouble
      i = pint
      fr l h = pforever (pdrange (d l) (d h))
      pr n v = preplicate (i n) (d v)
  in  psnew "rspdef1" Nothing AddToHead 11
        [("dur",
          pseq (i 2)
            [ pr 1024 (1/41)
            , pr 512 (2/41)
            , pr 256 (4/41)
            , pr 128 (8/41) ])
        ,("freq",
          pmidiCPS $ pforever $ prand (i 1) $
          map d [40,41,48,52,55,58,62,67,70,74,79,86,90])
        ,("pan", fr (-1) 1)
        ,("atk", fr 1e-4 1)
        ,("dcy", fr 1e-2 1)
        ,("amp", fr 1e-3 1)
        ,("n_map/fmul", pforever (d 100))]

loop02 =
  let d = pdouble
      fr l h = pforever (pdrange (d l) (d h))
  in  psnew "rspdef2" Nothing AddToHead 11
        [("dur",  fr 1e-1 5e-1)
        ,("freq", pforever $ pexp $ pdrange (plog (d 110)) (plog (d 11000)))
        ,("atk",  fr 1e-4 2)
        ,("dcy",  fr 1e-4 2)
        ,("amp",  fr 1e-2 1)
        ,("pan",  fr (-1) 1)
        ,("lagt", pforever $ pexp $ pdrange (plog (d 1e-3)) (plog (d 8)))
        ,("n_map/q", pforever (d 101)) ]

loop03 = let d = pdouble in
  pnset 100001
  [("dur",    pforever $ pdrange (d 4) (d 32))
  ,("t_trig", pforever (d 1))]

loop04 = let d = pdouble; i = pint in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur", {- pforever (d 0.2) -} preplicate (i 1000) (d 0.2))
  ,("n_map/amp", pforever (d 102))
  ,("pan", pforever $ pdrange (d (-1)) (d 1))
  ,("freq", pforever $ pdrange (d 100) (d 8000))
  ,("atk", pforever (d 1e-4))
  ,("dcy", pforever (d 1)) ]

-- --------------------------------------------------------------------------
--
-- Main
--

litw :: IO ()
litw = withSC3 $ \fd -> do
  setup_litw fd
  patchNode (nodify litwNd) fd
  play fd $ toL psw
