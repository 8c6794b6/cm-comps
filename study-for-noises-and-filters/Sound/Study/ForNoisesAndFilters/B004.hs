{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

B004 - Pluck fiesta.

-}
module Sound.Study.ForNoisesAndFilters.B004 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (tu)

main :: IO ()
main = w go

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
    , ("bp04", bp04)
    , ("bp05", bp05)
    , ("bp06", bp06)
    , ("bp07", bp07)
    , ("bp08", bp08)
    ]

w :: (UDP -> IO a) -> IO a
w = withSC3

-- --------------------------------------------------------------------------
--
-- Synthdefs
--

-- | Plucked noise.
--
bp01 :: UGen
bp01 = mrg [out 0 $ pan2 sig ("pan"@@0) 1, dts] where
  dts = detectSilence' sig 0.01 1 RemoveSynth
  sig = (combL nz dlyT dlyT dcyT + nz) * (1/8) * tamp
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
  sig = foldr f src (zip "a98312-kI{+A" dcyts)
  dcyts = [ 2e-3, 3.3e-3, 4.32e-3, 5.79e-3
          , 6.017e-3, 8.32e-3, 8.991e-3, 9.327e-3
          , 10.32e-3, 15.339e-3, 19.19e-3, 23.1093e-3 ]
  f (i,j) acc = allpassC acc 1 j (rand i 1e-3 3)
  src = in' 2 AR ("in"@@0)

-- | Decay time of pitched pluck.
--
bp03 :: UGen
bp03 = out ("out"@@0) sig where
  sig = (sig1*m) + (sig2*(1-m))
  sig1 = (fSinOsc KR (1/(tu*32)) (pi*1.5) + 1) * 4
  sig2 = envGen KR ("t_trig"@@1) 1 0 1 DoNothing $
         env [1/64,64] [150 * tu] [EnvExp] (-1) 0
  m = squared (line KR 1 0 (120*tu) DoNothing)

-- | Tambourine-ish percussive pluck.
--
bp04 :: UGen
bp04 = mrg [out ("out"@@0) (pan2 sig ("pan"@@0) 1), dts] where
  dts = detectSilence' sig 0.01 1 RemoveSynth
  sig = mix (hpf (combL nz dlyt dlyt dcyt + nz) 60) * 0.08 * amp
  amp = "amp"@@1
  nz = whiteNoise 'w' AR * e
  dlyt = recip (mce [4932.372, 5919.26, 8211.391])
  dcyt = 0.3
  t_trig = "t_trig"@@1
  e = envGen KR t_trig 1 0 1 DoNothing $
      env [0,1,0] [2e-3,50e-3] [EnvCub] (-1) 0

-- | Low frequency gray pluck.
--
bp05 :: UGen
bp05 = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig = (sig0 + sig1) * 0.7 * amp2
  sig0 = lpf (hpf (mix (combC nz dlyT1 dlyT1 dcyT1)) 5) 2000 * e1 * amp
  sig1 = lpf (hpf (mix (combL nz dlyT2 dlyT2 dcyT2)) 10) 8000 * e2 * amp
  amp = "amp"@@0.3
  amp2 = "amp2"@@1
  nz = grayNoise 'g' AR
  dlyT1 = mce [1/27.32, 1/39.17, 1/53.32]
  dcyT1 = 0.3
  dlyT2 = mce [1/3203.32, 1/3970.17, 1/5318.32]
  dcyT2 = 0.1
  e1 = envGen KR t_trig 1 0 1 DoNothing $
       env [0,1,1,0] [1e-4,20e-3,405e-3] [EnvCos] (-1) 0
  e2 = envGen KR t_trig 1 0 1 DoNothing $
       env [0,1,0.5,0] [1e-4,25e-3,5e-3] [EnvCub] (-1) 0
  t_trig = "t_trig"@@0

-- | Clap-ish pluck.
--
bp06 :: UGen
bp06 = mrg [out ("out"@@0) (pan2 sig ("pan"@@0) 1), dts, fse] where
  dts = detectSilence' sig 0.01 1 RemoveSynth
  fse = freeSelfWhenDone (line KR 0 1 4 RemoveSynth)
  sig = resonz (mix (combC nz dlyT dlyT dcyT) * e1 * 0.2 * amp) 1200 e2
  amp = "amp"@@0.3 * "amp2"@@1
  e1 = envGen KR t_trig 1 0 1 DoNothing $
       env [0,1,1,0] [2e-3,25e-3,310e-3] [EnvCub] (-1) 0
  e2 = envGen KR t_trig 1 0 1 DoNothing $
       env [0.999,0.5,0.95] [30e-3,100e-3] [EnvCub] (-1) 0
  dlyT = mce fs * e2
  dcyT = 0.2
  fs = [recip x| x<-[139.32, 341.119, 513.13, 208.88, 429.13]]
  t_trig = "t_trig"@@0
  nz = whiteNoise 'b' AR

-- | Graduate amplitude env for bp05 and bp06.
--
bp07 :: UGen
bp07 = out ("out"@@0) sig where
  sig = envGen KR ("t_trig"@@1) 1 0 1 DoNothing shp
  shp = env [1,1,0] [90,30] [EnvSqr] (-1) 0

-- | Using localIn and localOut.
-- More close to original karplus-strong.
--
bp08 :: UGen
bp08 = mrg [offsetOut 0 sig2, localOut sig0, fslf] where
  fslf = freeSelfWhenDone (line KR 0 1 8 RemoveSynth)
  sig2 = pan2 sig1 ("pan"@@0) 1
  sig1 = fdbk
  sig0 = delayL (nsrc + (fdbk * artc)) 1 (recip freq - recip controlRate)
  fdbk = localIn 1 AR
  nsrc = hpf ((pinkNoise 'p' AR + brownNoise 'b' AR) * 0.25 * ebst) 20
  ebst = envGen KR t_trig 1 0 1 DoNothing $
         env [0,1,0] [5e-3,200e-3] [EnvCub] (-1) 0
  t_trig = "t_trig"@@0
  artc = mouseX KR 0 (1-1e-4) Linear 0.1
  freq = midiCPS ("ptch"@@69)

-- --------------------------------------------------------------------------
--
-- Nodes
--

n0 =
  grp 0
  [ grp 1
    [ grp 2
      [ bp03_n, bp07_n ]
    , grp 3
      [ bp05_n, bp02_n ]
    ]]

bp03_n = syn "bp03" ["out"*=100]
bp07_n = syn "bp07" ["out"*=102]

bp05_n = syn' 3999 "bp05" ["amp2"*<-prmv bp07_n "out"]
bp02_n = syn "bp02" ["in"*=0,"out"*=0]


-- --------------------------------------------------------------------------
--
-- Patterns
--

play_b004 :: Transport t => t -> IO ()
play_b004 = flip play (toL patterns)

tu = 60/63

patterns = ppar [rp01, rp02, rp03, sps, hp01, hp02, hp03]

base_ptchs = [0,5,3,10,7]

rp01 = gen_pluck1 (-0.35) p d where
  p = map (+48) base_ptchs
  d = [1.5, 0.5, 2.0, 1.0, 1.0]

rp02 = gen_pluck1 0.48 p d where
  p = map (+60) base_ptchs
  d = [0.75, 0.25, 1.00, 1.00]

rp03 = gen_pluck1 (-0.02) p d where
  p = map (+72) base_ptchs
  d = [0.375, 0.125, 0.500, 0.250, 0.125, 0.125]

gen_pluck1 pan ptchs durs =
  let i = pint; d = pdouble; ds = map d
      rep = ceiling (180 * tu/sum durs) + 1
  in  psnew "bp01" Nothing AddToHead 3
        [("dur", pseq (i rep) (ds $ map (*tu) durs))
        ,("ptch", pforever $ prand (i 1) (ds ptchs))
        ,("t_trig", pforever (d 1))
        ,("pan", pforever (d pan))
        ,("n_map/dcyT", pforever (d 100)) ]


seq_ptchs = [0,3,0,10,0,5,0,7]

sps = pconcat [rst, sps'] where
  rst = psnew "rest" Nothing AddToHead 3 [("dur", 132 * tu)]

sps' = ppar [sp01, sp02, sp03]

sp01 = gen_pluck2 (0.95) p d where
  p = map (+36) seq_ptchs
  d = [3, 1, 2, 2]

sp02 = gen_pluck2 (-0.92) p d where
  p = map (+60) seq_ptchs
  d = [1.5, 0.5, 1, 1]

sp03 = gen_pluck2 (0.03) p d where
  p = map (+84) seq_ptchs
  d = [0.75, 0.25, 0.50, 0.50]

gen_pluck2 pan ptchs durs =
  let i = pint; d = pdouble; ds = map d
      rep = floor (44 * tu/sum durs)
  in  psnew "bp01" Nothing AddToHead 3
        [("dur", pseq (i rep) (ds $ map (*tu) durs))
        ,("ptch", pforever $ pseq (i 1) (ds ptchs))
        ,("t_trig", pforever (d 1))
        ,("pan", pforever (d pan))
        ,("dcyT", pforever (d 16)) ]


hp01 =
  let d = pdouble; i = pint; ds = map d
  in  psnew "bp04" Nothing AddToHead 3
      [("dur",
        prand (i 252)
        [ pseq (i 1) (ds $ map (*tu) [0.250, 0.125, 0.125])
        , pseq (i 1) (ds $ map (*tu) [0.125, 0.125, 0.125, 0.125])
        ])
      ,("amp", pforever (pdrange 0.5 1.0))
      ,("t_trig", pforever (d 1))
      ,("pan", pforever (d 0.0))]

hp02 =
  let d = pdouble; i = pint; ds = map d
  in  pnset (nodeId $ nodify bp05_n)
      [("dur",
        pforever $ pseq (i 1)
        [ prand (i 1)
          [ pseq (i 1)
            [ prand (i 1)
              [ d (tu * 1.5), pseq (i 1) (ds $ map (*tu) [1, 0.5])]
            , prand (i 1)
              [ d (tu * 0.5), pseq (i 1) (ds $ map (*tu) [0.25, 0.25])]]
          , d (tu * 2) ]])
      ,("amp", pforever $ pdrange 0.075 0.10)
      ,("t_trig", pforever (d 1))
      ,("pan", pforever (d 0.1))]

hp03 =
  let d = pdouble; i = pint; r = pdrange 0.6 0.8; z = d 0
  in  psnew "bp06" Nothing AddToHead 3
      [("dur", pforever (tu * 0.25))
      ,("amp", pforever $ pseq (i 1) [z,z,1.0,r, z,z,1.0,z])
      ,("t_trig", pforever $ pseq (i 1) [0,0,1,1, 0,0,1,1])
      ,("n_map/amp2", pforever (d 102))
      ,("pan", pforever (d (-0.05)))]

{-

-- Sample patterns using bp08 synthdef

go2 fd = do
  setup_b004 fd
  patchNode (nodify n0) fd
  play fd $ toL $ ppar [rp04, rp05]

rp04 = gen_pluck2 0 p d where
  p = map (+48) base_ptchs
  d = [1.5, 0.5, 1, 2, 3]

rp05 = gen_pluck2 0 p d where
  p = map (+60) base_ptchs
  d = [0.75, 0.25, 0.5, 1, 1.5]

rp06 = gen_pluck2 0 p d where
  p = map (+72) base_ptchs
  d = [0.375, 0.125, 0.25, 0.50, 0.75]

gen_pluck2 pan ptchs durs =
  let i = pint; d = pdouble; ds = map d
  in  psnew "bp08" Nothing AddToHead 3
        [("dur", pforever $ pseq (i 1) (ds $ map (*tu) durs))
        ,("ptch", pforever $ prand (i 1) (ds ptchs))
        ,("t_trig", pforever (d 1))
        ,("pan", pforever (d pan)) ]
-}
