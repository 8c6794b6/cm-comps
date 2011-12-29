{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

B004 - Pluck.

-}
module Sound.Study.ForNoisesAndFilters.B004 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

go :: Transport t => t -> IO ()
go fd = do
  setup_b004 fd
  patchNode (nodify n0) fd
  play_b004 fd

setup_b004 :: Transport t => t -> IO ()
setup_b004 fd = do
  mapM_ (async fd . d_recv . uncurry synthdef)
    [ ("bp01", bp01)
    , ("bp02", bp02)
    , ("bp03", bp03)
    ]

w :: (UDP -> IO a) -> IO a
w = withSC3

-- --------------------------------------------------------------------------
--
-- Nodes
--

n0 :: Nd
n0 =
  grp 0
    [ grp 1
      [ grp 2
        [syn "bp03" ["out"*=100]]
      , grp 3
        [syn "bp02" ["in"*=0,"out"*=0]]
      ]]

-- --------------------------------------------------------------------------
--
-- Patterns
--

play_b004 :: Transport t => t -> IO ()
play_b004 = flip play (toL $ ppar [rp01, rp02, rp03])

base_ptchs :: Num t => [t]
base_ptchs = concat (replicate 4 [0,5,3,10,7])

gen_randpluck pan pshift durs =
  let i = pint; d = pdouble; ds = map d
      ptchs = map (+pshift) base_ptchs
  in  psnew "bp01" Nothing AddToHead 3
        [("dur", pforever $ prand (i 16) (ds durs))
        ,("ptch", pforever $ prand (i 1) (ds ptchs))
        ,("t_trig", pforever (d 1))
        ,("pan", pforever (d pan))
        ,("n_map/dcyT", pforever (d 100))
        ]

gen_seqpluck pan pshift durs =
  let i = pint; d = pdouble; ds = map d
      ptchs = map (+pshift) base_ptchs
  in  psnew "bp01" Nothing AddToHead 3
        [("dur", pforever $ pseq (i 1) (ds durs))
        ,("ptch", pforever $ pseq (i 1) (ds ptchs))
        ,("t_trig", pforever (d 1))
        ,("pan", pforever (d pan))
        ,("n_map/dcyT", pforever (d 100))
        ]

rp01 = gen_randpluck 0.25    48 [1.0, 1.5, 2.0, 2.5]
rp02 = gen_randpluck (-0.25) 60 [0.25, 0.50, 0.75]
rp03 = gen_randpluck 0       72 [1/8, 2/8, 3/8, 4/8]

-- --------------------------------------------------------------------------
--
-- Synthdefs
--

-- | Plucked noise.
--
bp01 :: UGen
bp01 = mrg [dts, out 0 $ pan2 sig ("pan"@@0) 1] where
  dts = detectSilence' sig 0.01 1 RemoveSynth
  sig = (combL nz dlyT dlyT dcyT + nz) * 0.125 * tamp
  nz = hpf ((grayNoise 'p' AR + pinkNoise 'p' AR) * burstEnv) 20
  burstEnv = envGen KR trg 1 0 1 DoNothing $ envPerc 0.01 0.05
  trg = "t_trig"@@1
  dlyT = recip $ midiCPS ptch
  ptch = "ptch"@@69
  dcyT = "dcyT"@@0.125
  tamp = tExpRand 'a' 0.5 1 trg

-- | Allpass reverb.
--
bp02 :: UGen
bp02 = replaceOut ("out"@@0) sig where
  sig = foldr f src "a98312-kI{+A"
  f i acc = allpassL acc 1 (rand i 1e-4 2e-2) (rand i 1e-4 3)
  src = in' 2 AR ("in"@@0)

-- | Low frequency sin oscillator.
--
bp03 :: UGen
bp03 = out ("out"@@0) sig where
  sig = linExp fso 1e-9 (1+1e-9) (1/64) 4
  fso = fSinOsc KR (1/60) (pi*1.5) + 1 + 1e-9
