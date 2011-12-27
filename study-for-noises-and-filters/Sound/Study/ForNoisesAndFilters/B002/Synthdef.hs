{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- |
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

B002 - white noise factory.

-}
module Sound.Study.ForNoisesAndFilters.B002.Synthdef where

import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

r :: IO ()
r = withSC3 reset

setup_b002 fd = mapM_ (send fd . d_recv . uncurry synthdef) defs where
  defs =
    [ ("b002met", b002met)
    , ("quickNoiseC", quickNoiseC)
    , ("quickNoise", quickNoise)
    , ("slowNoiseC", slowNoiseC)
    , ("slowNoise", slowNoise)
    , ("hatLike1C", hatLike1C)
    , ("hatLike2C", hatLike2C)
    , ("hatLike", hatLike)
    , ("bhitC", bhitC tick)
    , ("bhit", bhit tick)
    , ("boscC", boscC)
    , ("b002amps", b002amps)
    , ("b002mix1", b002mix1)
    , ("b002mix2", b002mix2)
    , ("b002mst", b002mst)
    , ("noises", noises)
    ]
  tick = "t_trig"@@0

-- --------------------------------------------------------------------------
--
-- Demand ugens
--

-- | Trigger values for quickNoise.
qnT1 :: Supply
qnT1 =
  sseq 2
  [sseq 128 [0]
  ,sseq 4
   [sseq {- 3 -} 1 [base1]
   ,sseq 1
    [1.0,   0,  0.5, 1.0, 0.3, 1.0,   0, 0.8
    ,srand 1
     [srand 8 [0, 0.2, 0.4, 1.0]
     ,sseq 8 [0]]]
   ,sseq 2 [base1]
   ,srand 32 [0.4, 0.6, 0.8, 1.0]]]
  where
    base1 = sser 16
      [1.0,   0, 0.5, 1.0, 0.3, 1.0,   0, 0.8
      ,0.4, 1.0,   0, 1.0, 1.0,   0, 0.8,   0]

-- | Frequencies for quickNoise.
qnF1 :: Supply
qnF1 =
  sseq sinf
  [sseq 6
   [srand 3 fs, 420,  srand 2 fs, 420, srand 1 fs]
  ,swhite 16 20 12000]
  where
    fs = [440,1760,880,440,880,3520,7220,14080]

-- | Gate values for bosc.
boG1 :: Supply
boG1 =
  sseq 2
  [sseq 1
   [srand 48 [1,0.8,0.2,0,-1], sseq 1 [1,sseq 14 [-1],0.8]
   ,srand 48 [1,0.8,-1], swhite 16 0.3 1]
  ,sseq {- 2 -} 1 verse1
  ,sseq 1
   [sseq 3 [base], swhite 16 0.3 1
   ,srand 48 [1,0.8,0.75,-1], swhite 16 0.3 1]
  ,sseq 1 verse1]
  where
    base =
      sseq 1 [1,0.5,srand 1 [0,-1],   0.8,0.4,srand 1 [0,-1]
             ,0.6,0.4,srand 1 [0,-1], 0.7,0.4,srand 1 [0,-1]
             ,0.8,srand 1 [0,-1,0.7], 0.8,srand 1 [0,-1,0.4]]
    verse1 =
      [sseq 3 [base]
      ,srand 1 [sseq 4 [1.0,srand 1 [0,-1],0.8,srand 1 [0,-1]]
               ,sseq 1 [sseq 2 [1,0.8,srand 1 [0,-1]], 1, srand 1 [0,-1]
                       ,sseries 8 0.4 7.5e-2]
               ,sseq 1 [1,sseq 14 [-1],0.8]]
      ,sseq 3 [base]
      ,swhite 16 0.3 1]

-- | Frequency value for bosc, in midi node.
boF1 :: Supply
boF1 =
  sseq 1
  [sseq 2
    [sseq 1 [sseq 16 [52], srand 16 [52,64]
            ,srand 16 [52,64,76], srand 16 base
            ,sseq 16 [52], srand 16 [52,64]
            ,srand 16 [52,64,76], sseq 2 base]
    ,sseq 1
     [sseq {- 32 -} 16 base
     ,srand 112 base, sseq 2 base
     ,sseq 16 base]]
  , sseq 7 base
  , sseq 1 base2
  , sseq 32 [sval 52]
  ]
  -- ,sseq 1 [sseq 48 base, srand 128 plus7]]
  -- ,sseq 1 [sseq 24 base, sseq 8 plus5
  --         ,sseq 8 base, sseq 8 minus2
  --         ,srand 32 (minus5 ++ plus5)
  --         ,srand 32 plus7
  --         ,sseq 4 plus5, sseq 4 base]]
          -- ,srand 128 (minus5 ++ plus5)]]
  where
    base = [52,57,59, 64,69,71, 76,78]
    base2 = map (+12) base
    -- plus2 = map (+ 2) base
    -- plus5 = map (+ 5) base
    -- plus7 = map (+ 7) base
    -- minus2 = map (+ (-2)) base
    -- minus5 = map (+ (-5)) base

snt1 :: Supply
snt1 =
  sseq sinf
    [ sseq {- 112 -} 96 [0], sseq {- 4 -} 4 b
    , sseq {- 128 -} {- 96 -} 48 b ]
  where
    b = [0,0,0,w, 0,0,0,0]
    w = swhite 1 0.8 1

hatt1 :: Supply
hatt1 =
  sseq sinf
  [sseq 64 [snil]
  ,sseq 144 [1,w1,w2,0]]
  where
    w1 = swhite 1 0.3 0.6
    w2 = swhite 1 0.4 0.8

hatt2 :: Supply
hatt2 =
  sseq sinf
  [sseq 64 [snil]
  ,sseq 144 [1,0,w2,w1]]
  where
    w1 = swhite 1 0.3 0.6
    w2 = swhite 1 0.4 0.8

bhitp :: Supply
bhitp = sseq 1 [sseq 2 [p], sseq sinf [w1,sseq 3 [r2]]]  where
  p = sseq 1
      [sseq 12 [w1,0,0,0], sseq 1 (replicate 16 0)
      ,sseq 12 [w1,0,0,0], sseq 2 [w1,0,0,w2,0,0,w2,r2]
      ,sseq {- 8 -} 6
       [sseq 3 [1,0,0,r2, w1,0,r2,0, w1,r2,0,0, w1,0,r2,0]
       ,sseq 2 [w1,0,0,w2,0,0,w2,r2]]]
  w1 = swhite 1 0.9 1
  w2 = swhite 1 0.6 0.8
  -- r1 = srand 1 [0,w1]
  r2 = srand 1 [0,w2]

-- --------------------------------------------------------------------------
--
-- Sound sources
--

-- | Synthdef to count beat.
--
-- 'outt' give 2 triggers in each single beat, 'outb' gives the number
-- of current beat count.
--
b002met :: UGen
b002met = mrg [out ("outt"@@0) sig, out ("outb"@@0) cnt, fslf] where
  sig = impulse KR f 0
  cnt = pulseCount sig 0
  fslf = freeSelf (cnt >=* (1024 + 64 + 1))
  f = 2 * ("bpm"@@60) / 60

noises :: UGen
noises = noises' ("colour"@@0)

noises' :: UGen -> UGen
noises' colour = out ("out"@@0) sig where
  sig = select colour $ mce nz
  nz = [whiteNoise 'w' AR
       ,pinkNoise 'p' AR
       ,brownNoise 'b' AR
       ,grayNoise 'g' AR
       ,henonL AR (sampleRate/2) 0.1 0.3 x y
       ,lorenzL AR sampleRate (x*10) (y*10) (z*12) h 0 0 0]
  x = linLin (lfdNoise3 'x' KR 1) (-1) 1 0 1.4
  y = linLin (lfdNoise3 'y' KR 1) (-1) 1 0 0.3
  z = linLin (lfdNoise3 'z' KR 1) (-1) 1 0 0.8
  h = linLin (lfdNoise3 'h' KR 1) (-1) 1 0 1

quickNoiseC :: UGen
quickNoiseC = quickNoiseC' ("t_trig"@@0)
quickNoiseC' t_trig = mrg [out ("outt"@@0) t,out ("outf"@@0) f] where
  t = demand t_trig 0 (supply0 qnT1) * t_trig
  f = demand t_trig 0 $ supply0 qnF1

quickNoise :: UGen
quickNoise = quickNoise' ("t_trig"@@0) dur atck ("freq"@@6600) envn where
  dur = linLin (lfdNoise1 'd' KR (1/10.32)) (-1) 1
        ("dmin"@@6.25e-2) ("dmax"@@0.25) `lag` 2e-4
  atck = linLin (lfdNoise1 'a' KR (1/12.123)) (-1) 1
         ("amin"@@1e-4) ("amax"@@999e-4) `lag` 2e-4
  envn = linLin (sinOsc KR (1/13.321) 0) (-1) 1 (-10) 10 `lag` 2e-4

quickNoise' tick dur atk freq en = out ("out"@@0) (sig * aenv * ("amp"@@1.2)) where
  sig = resonz nz freq' rq
  nz = "a_in"@@0
  freq' = lfreq + 5
  lfreq = latch freq tick
  aenv = envGen KR tick (latch tick tick) 0 dur DoNothing $
         env [0,1,0] [atk,1-atk] [EnvNum en] (-1) 0 -- 2
  rq = linLin (lfdNoise1 'r' KR (1/5.12)) (-1) 1 0.125 0.999

slowNoiseC :: UGen
slowNoiseC = out ("out"@@0) sig where
  sig = demand t 0 $ supply0 snt1
  t = "t_trig"@@0

slowNoise :: UGen
slowNoise = slowNoise' ("t_trig"@@0) ("amp"@@0.3)

slowNoise' tick amp = out ("out"@@0) sig where
  sig = foldr f sig' "slownz"
  f a b = allpassN b 1e-1 (rand a 3e-4 1e-2) (rand a 5e-2 5e-1)
  sig' = rlpf nz freq rq * aenv * amp
  freq = envGen KR tick 18000 4000 1 DoNothing $
         env [0.5,0.5,0] [100e-3,100e-3] [EnvNum (-14)] 0 (-1)
  rq  = linLin (sinOsc KR (1/pi * rqf) (pi/4)) (-1) 1 250e-3 850e-3
  rqf = linLin (sinOsc KR (1/pi) 0) (-1) 1 1 pi
  nz = "a_in"@@0
  aenv = envGen KR tick (latch tick tick) 0 1 DoNothing $
         env [0,1,0] [atk,dcy] [EnvNum 11] (-1) 0
  atk = index atkb idx
  dcy = index dcyb idx
  idx = pulseCount tick 0 `mod` 2
  atkb = asLocalBuf 'a' [20e-3,150e-3]
  dcyb = asLocalBuf 'd' [190e-3,60e-3]

hatLike1C :: UGen
hatLike1C = out ("out"@@0) (t * demand t 0 (supply0 hatt1)) where
  t = "t_trig"@@1

hatLike2C :: UGen
hatLike2C = out ("out"@@0) (t * demand t 0 (supply0 hatt2)) where
  t = "t_trig"@@1

hatLike :: UGen
hatLike = hatLike' ("t_trig"@@0) ("amp"@@0.3) sus where
  sus = linExp (lfdNoise3 'd' KR (1/8)) (-1) 1 2e-2 8e-2

hatLike' t_trig amp sst = out ("out"@@0) sig where
  sig = mix (mkSig fs) * aenv * amp
  fs = mce [3197*r1, 5889*r2, 7321.32*r3, 12210*r1, 15032*r2]
  mkSig f = resonz nz f (0.1 + (10/f))
  nz = "a_in"@@0
  aenv = envGen KR t_trig (latch t_trig t_trig) 0 1 DoNothing $
         env [0,1,0] [sst,15e-3] [EnvNum (-14)] 0 (-1)
  r1 = rand 'a' 0.995 1.005
  r2 = rand 'b' 0.995 1.005
  r3 = rand 'c' 0.995 1.005

bhitC :: UGen -> UGen
bhitC t_trig = out ("out"@@0) sig where
  sig = t_trig * demand t_trig 0 (supply0 bhitp)

bhit :: UGen -> UGen
bhit t_trig = out ("out"@@0) sig where
  sig = hpf (ringz nz (mce fs * fenv) (mce as) * aenv) 30
  fs = [80.57, 132.98, 201.33, 293.98, 488.88, 532.17]
  as = [0.4,0.18,0.16,0.14,0.12,0.1]
  nz = "a_in"@@0
  trig' = latch t_trig t_trig
  aenv =
    envGen KR t_trig trig' 0 1 DoNothing $
    env [0,1,0.7,0] [1e-4,40e-3,80e-3] [EnvNum (-13)] (-1) 0
  fenv =
    envGen KR t_trig 1 0 1 DoNothing $
    env [0.25,1,0.5] [30e-3,180e-3] [EnvNum 8] (-1) 0

boscC :: UGen
boscC = mrg [outg, outf] where
  outg = out ("outg"@@0) (toSustain boG1 tick)
  outf = out ("outf"@@0) (freq / 5)
  freq = midiCPS $ gate freq' (freq' >* 0)
  freq' = demand tick 0 $ supply0 boF1
  tick = "t_trig"@@0

bosc :: UGen
bosc = bosc' ("gt"@@0) ("freq"@@0) ("amp"@@1)

bosc' tick freq amp = out ("out"@@0) sig where
  sig = clip2 (rlpf sig' filtf filtq) 1 * amp
  sig' = sum [(sig1 + sig2 + sig3) * aenv
             ,pulse AR (freq * mce [1.0001,1]) 0.7 * atk]
  sig1 = combL nz 0.2 (1/ (freq * mce [1,0.9998]))
         (0.7 + (lfdNoise3 'q' KR 1 * 0.25))
  sig2 = combL nz 0.2 (1/ (freq * mce [1.5,1.5002]))
         (0.7 + (lfdNoise3 'q' KR 1 * 0.28))
  sig3 = combL nz 0.2 (1/ (freq * mce [2,1.9998]))
         (0.7 + (lfdNoise3 'q' KR 1 * 0.29))
  nz = "a_in"@@0
  aenv = envGen KR tick tick' 0 1 DoNothing
         (env [0,1,1,0.2,0] [1e-3,80e-3,20e-3,10e-3] [EnvNum (-28)] 3 (-1)) *
         tExpRand 'γ' 0.75 1.33 tick
  atk = envGen KR tick tick' 0 1 DoNothing $
        envPerc 1e-3 280e-3
  tick' = latch tick tick
  filtf = ("ff"@@12800)
  filtq = ("fq"@@0.8)

b002amps :: UGen
b002amps = mrg [qn,bo,sn,h1,h2,bh] where
  qn = out (800) (("quickNoise"@@1) `lag` 0.1)
  bo = out (801) (("bosc"@@1) `lag` 0.1)
  sn = out (802) (("slowNoise"@@1) `lag` 0.1)
  h1 = out (803) (("hat1"@@1) `lag` 0.1)
  h2 = out (804) (("hat2"@@1) `lag` 0.1)
  bh = out (805) (("bhit"@@1) `lag` 0.1)

-- | Mixer with pan, for single channel input.
b002mix1 :: UGen
b002mix1 = out ("out"@@0) sig where
  input = "a_in"@@0
  seeds = ['a'..'h']
  rev f i = foldr f i seeds
  rev_c = "rev"@@0.125
  f1 x y =
    y + rev_c * combL y 1e-1 (expRand x 6e-3 6e-2) (expRand x 6e-3 6e-1 * dtr)
  f2 x y =
    y + rev_c * combL y 1e-1 (expRand x 5e-3 5e-2) (expRand x 5e-3 5e-1 * dtl)
  sig = pan2 flt ("pan"@@0) 1 + mce [dlyl,dlyr]
  flt = input * ("amp"@@1)
  dtl = "dtl"@@0
  dtr = "dtr"@@0
  dlyl = rev f1 $ delayL flt 0.5 dtl
  dlyr = rev f2 $ delayL flt 0.5 dtr

-- | Mixer without pan, for dual channel input.
b002mix2 :: UGen
b002mix2 = out ("out"@@0) sig where
  inl = "a_inl"@@0
  inr = "a_inr"@@1
  rev1 i = foldr f i [1::Int,2..8]
  rev2 i = foldr f i [9::Int,10..16]
  f x y = y + ("rev"@@0.125) * combL y 1e-1 (expRand x 5e-3 5e-2) (expRand x 5e-3 5e-1)
  sig = mce [rev1 inl, rev2 inr] * ("amp"@@1)

-- | Master control
b002mst :: UGen
b002mst = replaceOut ("out"@@0) sig where
  sig  = sig' * ("amp"@@1)
  sig' = freeVerb sig'' 0.075 0.999 0.999
  sig'' = hpf (mce ["a_inl"@@0, "a_inr"@@1]) 20

-- --------------------------------------------------------------------------
--
-- Helpers
--

-- | Sustains with given trigger.
--
-- When value 'x' is:
--
-- * x > 0  : Triggers output x and hold as x.
--
-- * x == 0 : Gives output 0
--
-- * x < 0  : Holds last value.
--
toSustain :: Supply -- ^ Demand ugen pattern containint above values
          -> UGen -- ^ Trigger
          -> UGen -- ^
toSustain sup tick = sig where
  sig = gate (sig' * (1 - (tick >* 0))) (sig' >=* 0)
  sig' = demand tick 0 $ supply0 sup

supply0 :: Supply -> UGen
supply0 p = evalSupply p (mkStdGen 0)