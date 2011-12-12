{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Vomitus Marinus.

... Or, vomitus marinus.

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
  , ("rspdef4", rspdef4)
  , ("rspdef5", rspdef5)
  , ("rspctr1", rspctr1)
  , ("rspctr2", rspctr2)
  , ("rspmst", rspmst) ]

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
  let sig = resonz (whiteNoise 'd' AR) ("freq"@@1320) q * 0.3 * e
      e = envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth $
          env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvSin] (-1) 0
      q = clip ("q"@@0.8) 1e-5 9999e-4
  in  out 0 $ pan2 sig ("pan"@@0) ("amp"@@1)

rspdef3 :: UGen
rspdef3 = out ("out"@@100) (tExpRand 'f' 0.5 2 ("t_trig"@@1))

rspdef4 :: UGen
rspdef4 =
  let sig = mix (sinOsc AR (mce [ofq,ffq]) pw * amp)
      pw = lfdNoise3 'p' KR 2 ** 10
      ofq = fbs * fq0
      ffq = fbs * fq3
      -- fbs = "freq"@@664.647
      fbs = "freq"@@1320.918
      fq0 = mkfq lfdNoise0
      fq3 = mkfq lfdNoise3
      mkfq n = linExp (n 'ε' KR (1/4.13) + 1.01) 0.01 2.01 0.25 32
      amp = envGen KR tck ("amp"@@0.2) 0 (dur+5e-2) DoNothing shp
      dur = (1/tfq) * 0.985
      shp = env [0,1,0] [atk,1-atk] [EnvCub] (-1) (-1)
      tck = impulse KR tfq 0
      tfq = linLin (lfdNoise3 'ω' KR 0.25) (-1) 1 0.25 5.65 ** 2
      atk = linLin (sinOsc KR (1/37) 0) (-1) 1 1e-3 999e-3
      pan = sinOsc KR pfm 0 * 0.75
      pfm = sinOsc KR (1/7.32) 0 * pi + (1/82)
      rev = foldr fab sig ['a'..'h']
      fab a b = allpassC b 8 (rand a 0.1 0.8) (rand a 5e-1 2)
      rev' = freeVerb rev 0.5 0.9 0.9
  in  out ("out"@@0) (pan2 rev' pan 1)

rspdef5 :: UGen
rspdef5 =
  let phase = rand 'k' 0 pi
      atk = linExp atkf 1e-3 (1+1e-3) 999e-3 1e-3
      atkf = (sinOsc KR (1/23) 0 * 0.5) + 0.5 + 1e-3
      dcy = 1 - atk
  in  out 0 $ pan2
      (lfCub AR ("freq"@@440 * ("fmul"@@1 `lag2` 3.5)) phase * 0.3 *
       envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
       (env [0,1,0] [atk,dcy] [EnvSqr] (-1) 0))
      ("pan"@@0) ("amp"@@1)

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
      sig1 = rhpf sig0 15 0.81
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
         , loop02_q
         , loop04_amp
         , syn' 100004 "rspdef4" ["amp"*=0.2]]
      , grp 11 [] ]
    , grp 2
      [ syn "rspmst" []] ]

loop01_freq :: Nd
loop01_freq =
  syn' 100001 "rspdef3" ["mul"*=2.1,"add"*=2.1,"freq"*=0.113]

loop02_q :: Nd
loop02_q =
  syn' 100002 "rspctr1"
    ["out"*=101,"add"*=0.5,"mul"*=0.5,"freq"*=0.0683]

loop04_amp :: Nd
loop04_amp =
  syn' 100003 "rspctr2"
    ["out"*=102,"add"*=0.2,"mul"*=0.2,"freq"*=0.025,"phase"*=Dval(-pi/2)]

-- --------------------------------------------------------------------------
--
-- Patterns
--

psw = ppar [loop01, loop02, loop03, loop04]

{-
Alternative: Sending patterns to leptseq.

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
      pitches = map d [40,41,48,52,55,58,62,67,70,74,79,86,90]
      i = pint
      fr l h = pforever (pdrange (d l) (d h))
      pr n v = preplicate (i n) (d v)
  in  psnew "rspdef1" Nothing AddToHead 11
        [("dur",
          -- pseq (i 2)
          -- pcycle
          --   [ pr 2048 (1/41)
          --   , pr 1024 (2/41)
          --   , pr 512 (4/41)
          --   , pr 256 (8/41)
          --   , pr 128 (16/41)
          --   , pr 64 (32/41) ])
          -- pcycle
          pconcat
            [ pr 2048 (1/117)
            , pr 512 (4/117)
            , pr 128 (16/117)
            , pr 32 (64/117)
            , pr 2048 (1/117)
            , pr 512 (4/117)
            , pr 128 (16/117)
            , pr 2048 (1/117)
            , pr 1024 (2/117) ])
        ,("freq", pmidiCPS $ pforever $ prand (i 1) $ pitches)
        ,("pan", fr (-1) 1)
        ,("atk", fr 1e-4 1)
        ,("dcy", fr 1e-2 1)
        ,("amp", fr 1e-2 6.8e-1)
        ,("n_map/fmul", pforever (d 100))]

loop02 =
  let d = pdouble
      fr l h = pforever (pdrange (d l) (d h))
      er l h = pforever $ pexp $ pdrange (plog (d l)) (plog (d h))
  in  psnew "rspdef2" Nothing AddToHead 11
        [("dur", er 1e-1 5e-1)
        ,("freq", er 110 17000)
        ,("atk",  fr 1e-4 2)
        ,("dcy",  fr 5e-1 2)
        ,("amp",  fr 1e-3 0.48)
        ,("pan",  fr (-1) 1)
        ,("lagt", er 1e-3 8)
        ,("n_map/q", pforever (d 101)) ]

loop03 =
  let d = pdouble
      er l h = pforever $ pexp $ pdrange (plog (d l)) (plog (d h))
  in pnset 100001
       [("dur", er 4 32)
       ,("t_trig", pforever (d 1))]

loop04 = let d = pdouble in
  psnew "rspdef5" Nothing AddToHead 1
  [("dur", pforever (d 0.2) {- preplicate (i 1000) (d 0.2) -})
  ,("n_map/amp", pforever (d 102))
  ,("pan", pforever $ pdrange (d (-1)) (d 1))
  ,("freq", pforever $ pdrange (d 1000) (d 11000)) ]
  -- ,("atk", pforever (d 1e-4))
  -- ,("dcy", pforever (d 1)) ]

-- --------------------------------------------------------------------------
--
-- Main
--

litw :: IO ()
litw = withSC3 $ \fd -> do
  setup_litw fd
  patchNode (nodify litwNd) fd
  play fd $ toL psw
