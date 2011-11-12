{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Synthdefs used in /shivering air/.
-}
module Sound.Bogus.ShiveringAir.Synthdefs where

import Language.Haskell.Extract
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (limiter)

setup'sa :: Transport t => t -> IO ()
setup'sa fd = do
  let path = "/home/atsuro/repos/git/cm-comps/bogus/data/lipsum-4lines.wav"
      start = 48000 * 35
  async fd $ b_allocRead 0 path start 0
  mapM_ (send fd . d_recv . uncurry synthdef) ugs

sand :: Nd
sand =
  grp 0
    [ grp 1
        []
    , grp 2
        [ nd1, nd2, nd3 ]
    , grp 3
        [ syn "sa00"
          [ "in_1"*=prmv nd1 "out"
          , "in_2"*=prmv nd2 "out"
          , "in_3"*=prmv nd3 "out" ]]
    ]
  where
    nd1 = syn "sa01" ["out"*=2]
    nd2 = syn "sa02" ["out"*=4]
    nd3 = syn "sa03" ["out"*=6]

ugs :: [(String,UGen)]
ugs = $(functionExtractor "^sa[0-9]+")

-- | Mixer
sa00 :: UGen
sa00 =
  let mkI key = in' 2 AR (("in_"++key)@@0)
      sig = mix $ mce $ map (mkI . show) [1..3]
  in  replaceOut ("out"@@0) (limiter sig 1 0.1)

-- | Comb filtered noises.
sa01 :: UGen
sa01 =
  let f x acc = combC acc 0.5 (dlt x) (dct x)
      dlt i = lag3 (tExpRand i (recip rmax) (recip rmin) tick) 28e-3
      dct i = lag3 (tExpRand i 120e-3 800e-3 tick) 28e-3
      -- nz = lorenzL AR (sampleRate/2) lx 27 2.667 0.05 0.1 0 0
      nz = whiteNoise 'w' AR
      lx = (lfdNoise3 'x' KR 0.5 * 2) + 10
      -- rmin = "rmin"@@5
      rmin = 1 + (menv * 24)
      -- rmax = "rmax"@@50
      rmax = 50 - (menv * 25)
      tick = dust 'τ' KR tfreq + impulse KR tfreq 0
      tfreq = 48 * squared (cubed menv)
      tdur = "tdur"@@65
      aenv = decay2 tick 1e-3 0.3
      menv = envGen KR 1 1 0 tdur DoNothing (env [0,1,0] [1,0.2] [EnvLin] (-1) 0)
      sig = foldr f (nz*aenv) [1..16::Int]
      rsig = freeVerb sig (1-menv) 0.9 0.9
      psig = pan2 rsig ((1-penv) * lfdNoise3 'u' KR 2 * 0.5 + pos) 1
      penv = envGen KR tick 1 0 pdur DoNothing (env [0,1] [1] [EnvLin] (-1) 0)
      pdur = tRand 'p' 1e-4 2e-1 tick
      pos = tRand 'o' (-0.5) 0.5 tick
      dl = tExpRand 'l' 1e-6 1e-3 tick
      dr = tExpRand 'r' 1e-6 1e-3 tick
      dsig = hpf (mce [delayL psig 1e-3 dl, delayL psig 1e-3 dr]) 15
  in  out ("out"@@0) dsig

fb_test :: IO ()
fb_test =
  let i = impulse AR 1 0
      c = lfCub AR 1200 0
      s = decay i 0.25 * c * 0.1
      x = mouseX' KR 0 1 Linear 0.1
      y = mouseY' KR 0 1 Linear 0.1
      r = freeVerb s y x 0.5
  in  audition $ out 0 r

-- | Plays buffer containing sound file made with UPIC.
sa02 :: UGen
sa02 =
  let sig = playBuf 2 AR 0 rate 1 0 NoLoop RemoveSynth
      rate = bufRateScale KR 0
  in  out ("out"@@0) sig

-- | Pink noise, percussive
sa03 :: UGen
sa03 =
  let sig = pp' tick amp freq pan en dur atk
      tick = impulse KR (menv*36) 0
      menv = envGen KR 1 1 0 65 DoNothing mshp
      mshp = env [0,1,0.5] [0.8,0.2] [EnvSin] (-1) 0
      freq = tExpRand 'd' ((12800 * (1-menv)) + 2000) 15000 tick
      pan = lfdNoise3 'p' KR 0.8 * menv
      en = 13
      dur = 0.001
      atk = 1e-4
      amp = "amp"@@0.2
  in  sig

sa04 :: UGen
sa04 =
  let nz = pinkNoise 'p' AR
      rg = pan2 (ringz (nz * aenv) freq q) pan 1
      aenv = envGen KR tick 1 0 dur DoNothing ashp
      -- aenv = envGen KR tick 1 0 dur RemoveSynth ashp
      ashp = env [0,1,0] [atk,1-atk] [EnvNum 13] (-1) 0
      mkF x is =
        let f x acc = allpassN acc 5e-1 (rand x 1e-4 5e-2) 3
        in  foldr f x is
      freq = "freq"@@2400
      q = "q"@@0.3
      dur = "dur"@@1e-4
      tick = "t_trig"@@1
      atk = "atk"@@1e-3
      pan = "pan"@@0
      sig = mkF rg [0..15::Int]
      dts = detectSilence' sig 0.01 1 RemoveSynth
  in  mrg [dts, out ("out"@@0) sig]

sa05 = out 0 sig where
  sig = sinOsc AR freq 0 * ae
  ae = envGen KR 1 1 0 1 RemoveSynth (envPerc 1e-4 0.3)
  freq = "freq"@@440

sa06 = out 0 sig where
  sig = pan2 hit pan 1
  pan = sin (2 * pi * menv)
  menv = envGen KR 1 1 0 48 RemoveSynth (env [0,1] [1] [EnvLin] (-1) 0)
  hit = sinOsc AR freq 0 * aenv
  freq = exp (5 + (menv * 4.5))
  aenv = decay2 tick 1e-4 0.1
  tick = impulse KR tfreq 0 + dust 't' KR tfreq
  tfreq = 1 + (16 * menv)

p1 =
  psnew "sa05" Nothing AddToTail 1
    [ ("dur", pconcat (ds [1,0.97 .. 0.01]))
    , ("freq", pconcat (ds [8800,8700..1200]))
    ]
  where
    d = pdouble; ds = map d; i = pint

play_n0 :: IO ()
play_n0 = withSC3 $ patchNode (nodify n0)

pp :: UGen
pp =
  pp' ("t_trig"@@1) ("amp"@@0.3) ("freq"@@1200) ("pan"@@0)
  ("envn"@@0) ("dur"@@0.1) ("atk"@@0.1)
pp' tick amp freq pan en dur atk = out ("out"@@0) sig where
  sig = foldr f sig' $ map (\x -> rand x 1e-4 5e-2) "blahouqp32813"
  f a b = allpassN b 5e-3 a 3
  sig' = pan2 (ringz (nz * aenv) freq q) pan 1
  aenv = envGen KR tick amp 0 dur DoNothing ashp
  ashp = env [0,1,0] [atk,1-atk] [EnvNum en] (-1) 0
  nz = pinkNoise 'p' AR
  q = lfdNoise3 'q' KR 1 * 2.4 + 2.401

n0 :: Nd
n0 =
  grp 0
  [grp 1
   [grp 10
    [syn "ppC"
     ["bpm"*=60,"outt"*=100,"outf"*=101,"outa"*=102,"outp"*=103
     ,"minf"*=600,"maxf"*=12800,"outatk"*=105]
    ,syn "ppt"
     ["out"*=100,"freq"*=8]
    ,syn "pp"
     ["t_trig"*<-100,"freq"*<-101,"amp"*<-102,"pan"*<-103
     ,"dur"*=1,"atk"*<-105,"en"*=1] ]]]

ppt :: UGen
ppt = out ("out"@@0) (impulse KR (lfdNoise3 'f' KR 1 * f + (f/2)) 0) where
  f = "freq"@@1

ppC :: UGen -> UGen
ppC bpm = mrg [outt, outf, outa, outp, outatk] where
  t = dust 'd' KR (bpm/60)
  outt = out ("outt"@@0) t
  outf = out ("outf"@@0) (tExpRand 'f' ("minf"@@1020) ("maxf"@@12080) t)
  outa = out ("outa"@@0) (tExpRand 'a' 1e-3 5e-2 t)
  outp = out ("outp"@@0) (tRand 'p' (-1) 1 t)
  outatk = out ("outatk"@@0) (tRand 'k' 1e-4 9999e-4 t)

pp01 :: UGen
pp01 = pp' t a f p (-14) 1 1e-4
  where
    f = tExpRand 'f' 1200 12800 t
    a = tExpRand 'a' 0.05 0.15 t
    p = tRand 'p' (-1) 1 t
    t = dust 'd' KR (mouseY KR 0.25 2 Exponential 0.1)
