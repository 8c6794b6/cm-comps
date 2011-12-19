{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch for /huh/ composition.
Rewrite using pattern instead of demand ugens.

-}
module Sound.Bogus.Huh.Synthdefs where

import Control.Concurrent
import Control.Exception

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

{-

Sequencing patterns in lepton server.

UDP protocol cannot send message larger than 65535 bytes in single
connection. Sending multiple bundle with 'mapM_' with same timestamps.

-}
-- huhps fd = do
--   t0 <- (+0.1) `fmap` utcr
--   mapM_ (\(n,p) -> send fd $ bundle (UTCr t0) [l_new n p])
--     [ ("huh1",huh1P), ("huh2",huh2P), ("huh3",huh3P)
--     , ("kik", kikP), ("snr", snrP), ("hat", hatP)
--     , ("pu", puP), ("bell", bellP), ("drn1P",drn1P), ("drn2P",drn2P)
--     ]

setupHuh :: Transport t => t -> IO OSC
setupHuh fd = do
  mapM_ (uncurry writeSynthdef) huhdefs
  async fd $ bundle immediately $
    map (d_recv . uncurry synthdef) huhdefs

huhdefs =
  [("cf2huh", cf2huh)
  ,("cf2nzf", cf2nzf)
  ,("cf2kik", cf2kik)
  ,("cf2snr", cf2snr)
  ,("cf2hat", cf2hat)
  ,("cf2drn", cf2drn)
  ,("cf2pu", cf2pu)
  ,("cf2bell", cf2bell)
  ,("cf2shw", cf2shw)
  ,("cf2rev", cf2rev)
  ,("cf2dly", cf2dly)
  ,("cf2mix", cf2mix)
  ,("cf2mixm", cf2mixm)
  ,("cf2mst", cf2mst)
  ,("cflfnz", cflfnz)
  ,("cflfsin", cflfsin)
  ]

------------------------------------------------------------------------------
-- Control synths
--

cflfsin :: UGen
cflfsin =
  out ("out"@@100) $ sinOsc KR ("freq"@@1) 0 * ("mul"@@1) + ("add"@@0)

cflfnz :: UGen
cflfnz =
  let freq = ("freq"@@0.3) * (sinOsc KR ("mfreq"@@1) 0 * 0.5 + 0.5)
  in  out ("out"@@101) $ lfdNoise1 'z' KR freq * ("mul"@@1) + ("add"@@0)

------------------------------------------------------------------------------
-- Source synths
--
-- Many synths are using detectSilence ugen, to free itself.
--
-- Using more memory than demand ugen version, need to increase
-- scsynth server memory with '-m' option.
--

-- | Synthdef for 'huh' human vowel like noise.
cf2huh :: UGen
cf2huh = cf2huh' (whiteNoise 'a' AR) ("t_trig"@@0)
cf2huh' srcn tick = mrg [out ("out"@@0) sig, d] where
  sig = mix $ resonz srcn freq bndw * ampe
  freq = mce [tRand 'a' 600 800 tick
             ,tRand 'b' 1000 1400 tick
             ,tRand 'c' 2400 2800 tick] `lag` 0.1
  bndw = mce [130/700, 70/1220, 160/2600]
  ampe = envGen KR tick 1 0 1 RemoveSynth $ envSine ed ev
  ampv = latch tick tick
  ed = tRand 'd' 0.1 0.4 tick
  ev = tRand 'e' 0.1 0.6 tick
  d = detectSilence' ampe 0.01 0.1 RemoveSynth

cf2nzf :: UGen
cf2nzf = cf2nzf' ("t_amp"@@0) ("freq"@@0)
cf2nzf' amp freq = out ("out"@@0) sig where
  sig = sum [rlpf nz freq 2 * ae1
            ,rlpf nz (freq*2.002321) 1.5 * ae2
            ,rlpf nz (freq*2.9989989) 1 * ae3]
  nz = pulse AR freq (lfdNoise3 'f' KR 9.32 * 0.5 + 0.5)
  ae1 = mkAE [0,1,0.2,0.8,0] [28e-3,200e-3,100e-3,285e-3]
  ae2 = mkAE [0,0.5,0.8,0] [120e-3,30e-3, 130e-3]
  ae3 = mkAE [0,1,0.2,0] [25e-3, 180e-3, 310e-3]
  mkAE vs ts = envGen KR amp amp 0 0.25 RemoveSynth $
               env vs ts [EnvNum 3] (-1) (-1)

cf2kik :: UGen
cf2kik = cf2kik' ("t_trig"@@0)
cf2kik' tick = out ("out"@@0) ((lfCub AR freq 0.05 + impl) * ampe) where
  freq = (mix $ mce [200.32, 230.32, 360.79, 110.13]) * fenv
  fenv = envGen KR tick 1 0 1 DoNothing $
         env [0.2, 0.2, 0.1, 0.1] [10e-3, 10e-3, 10e-3] [EnvSqr] 0 (-1)
  impl = impulse AR 28 0.3 * decay2 tick 1e-4 2e-3
  ampe = envGen KR tick lvls 0 1 RemoveSynth $
         env [0,1,1,0] [1e-3,25e-3,228e-3] [EnvNum (-13)] (-1) 0
  lvls = latch tick tick

cf2snr :: UGen
cf2snr = cf2snr' ("t_trig"@@0)
cf2snr' tick = out ("out"@@0) (sig * ampe * 0.3) where
  sig = mix $ ringz (whiteNoise 'a' AR) fs qs * 0.1
  fs = mce [943.232, 350.32, 680.192]
  qs = mce [0.1,0.05,0.025]
  ampe = envGen KR tick amp' 0 1 RemoveSynth $
         env [0,1,1,0] [1e-3,15e-3,189e-3] [EnvNum (-8)] (-1) 0
  amp' = latch tick tick

cf2hat :: UGen
cf2hat = cf2hat' ("t_trig"@@0)
cf2hat' tick = out ("out"@@0) (sig * amp) where
  sig = mix $ rhpf (whiteNoise 'z' AR)
        (mce [5908.32,8803,6723]) (mce [0.1,0.1,0.1])
  amp = envGen KR tick tamp 0 1 RemoveSynth $
        env [0,1,0] [3e-4, 80e-3] [EnvNum (-3)] (-1) (-1)
  tamp = latch tick tick

cf2drn :: UGen
cf2drn = cf2drn' ("amp"@@0) ("gate"@@1) ((("freq"@@440) `lag` 0.6 `lag` 0.6))
cf2drn' amp gt freq = out ("out"@@0) sig where
  sig = foldr (\a b -> allpassN b 0.05 a 4) sig' $
        map (\i -> mce2 (rand i 0 0.05) (rand (succ i) 0 0.05)) "qrst"
  sig' = resonz sig'' q 0.5
  q = lfdNoise3 '\813' KR 1.23 * 2300 + 3000
  sig'' = lfPulse AR freq 0 bw * aenv
  bw = lfdNoise3 '\992' KR 0.7778 * 0.4 + 0.5
  aenv = envGen KR gt amp 0 1 DoNothing $
         env [0, 1, 0.8, 0.8, 0]
         [5e-3, 20e-3, 20e-3, 30e-3] [EnvCub] 2 1

cf2pu :: UGen
cf2pu = cf2pu' ("t_trig"@@0)
cf2pu' tick = mrg [out ("out"@@0) sig, d] where
  sig = foldr f v (zipWith mce2 rs1 rs2)
  v = rlpf (pulse AR (mce2 freq (freq*1.01)) bw * 0.2 * ampe * amp)
      (lfdNoise3 'n' KR 2.323 * 2000 + 2200)
      (lfdNoise3 'q' KR 1.110 * 0.498 + 0.5)
  f a b = allpassN b 0.05 a 4
  rs1 = map mkR "abcd"
  rs2 = map mkR "efgh"
  mkR x = rand x 0.001 0.05
  freq = "freq"@@0 `lag` 0.6
  ampe = decay2 tick 5e-4 950e-3
  amp = "amp"@@0
  bw = lfdNoise3 'b' KR 0.1123 * 0.48 + 0.5
  d = detectSilence' ampe 0.01 1 RemoveSynth

cf2bell :: UGen
cf2bell = cf2bell' ("t_trig"@@0) ("freq"@@0) ("amp"@@0.3)
cf2bell' tick freq amp = mrg [out ("out"@@0) (sig * amp), d] where
  sig = foldr f sig' (zipWith mce2 rs1 rs2)
  sig' = sinOsc AR (mce [freq',freq'*1.01]) 0 * aenv
  freq' = freq''
  freq'' = latch freq tick
  f a b = allpassL b 250e-3 a 2
  rs1 = map mkR [(1::Int)..8]
  rs2 = map mkR [(1001::Int)..1008]
  mkR x = expRand x 1e-4 150e-3
  aenv = decay2 tick 50e-3 1 * ampm
  ampm = sinOsc KR (tExpRand 'z' tick 1e-4 3041.21327) (pi/2) * 0.5 + 0.5
  d = detectSilence' aenv 0.01 1 RemoveSynth

cf2shw :: UGen
cf2shw = cf2shw' ("t_trig"@@0)
cf2shw' tick = out ("out"@@0) (resonz sig freq bw) where
  sig = brownNoise 'b' AR * tamp
  tamp = decay2 tick 1e-3 1
  freq = el * ("freq"@@8000) + 50
  bw = el * 0.8 + 0.1
  el = envGen KR ("t_envr"@@0) 1 0 1 DoNothing $
       env [2.5e-2,1,2.5e-2] [25e-3,8.4] [EnvCub] (-1) (-1)

------------------------------------------------------------------------------
-- Effect synths
--

cf2rev :: UGen
cf2rev = cf2rev' ("a_in"@@0)
cf2rev' input = replaceOut ("out"@@0) sig where
  sig = (input*m) + ((1-m)*foldr f input is)
  r1 = map mkR [(1::Int) .. 3]
  r2 = map mkR [(101::Int) .. 103]
  mkR i = rand i 1e-3 8e-2
  is = [1..16::Int]
  f a b = allpassC b dlyt (rand a 1e-3 dlyt) (rand a 4e-2 dcyt)
  m = "mix"@@0.5
  dlyt = "dlyt"@@0.8
  dcyt = "dcyt"@@2

cf2dly :: UGen
cf2dly = cf2dly' ("a_in"@@0)
cf2dly' input = replaceOut ("out"@@0) sig where
  sig = foldr f input rs
  f a b = delayL b ("maxdt"@@0.5) a
  rs = map mkR "asdqwerpoiu;lkj/.m"
  mkR x = expRand x 5e-3 ("maxdt"@@0.5)

cf2mix :: UGen
cf2mix = cf2mix' ("a_in"@@0)
cf2mix' input =
  out ("out"@@0) ((pan2 input ("pan"@@0) 1) * ("amp"@@0) * ("mamp"@@1))

cf2mixm :: UGen
cf2mixm = cf2mixm' ("a_in"@@0)
cf2mixm' input = out ("out"@@0) (input * ("amp"@@1))

cf2mst :: UGen
cf2mst = cf2mst' ("amp"@@0)
cf2mst' amp = mrg [l', r'] where
  l' = replaceOut ("out_l"@@0) (hpf l 15 * amp)
  r' = replaceOut ("out_r"@@1) (hpf r 15 * amp)
  -- (hpf (limiter (mce [l,r] * amp) 1 0.25) 15) where
  l = in' 1 AR 0
  r = in' 1 AR 1
