{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Synthdefs used in /birth/.
-}
module Sound.Bogus.Birth.Synthdefs where

import Data.List (foldl')

import Language.Haskell.Extract
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (limiter)

-- | List of pair of UGens and its name.
ugs :: [(String, UGen)]
ugs = $(functionExtractor "^bth")

------------------------------------------------------------------------------
-- Sound sources

-- | Percussive simple sine tone.
bth01 :: UGen
bth01 =
  let sig = sinOsc AR frq 0 * aev * amp
      frq = "freq"@@440
      aev = envGen KR 1 1 0 1 RemoveSynth shp
      shp = env [0,1,0] [1e-3,999e-3] [EnvCub] (-1) 0
      amp = "amp"@@0.2
      pan = "pan"@@0
  in  out ("out"@@0) (pan2 sig pan 1)

-- | Frog like fm sin osc.
bth02 :: UGen
bth02 =
  let sig = sinOsc AR frq 0 * aev * amp
      frq = frq' + sinOsc KR (frq' * 0.24999812) 0 * idx
      idx = frq' * 3.5 * fev
      fev = envGen KR frq' 1 0 dur DoNothing fshp
      fshp = env [0,1,0] [750e-4,250e-4] [EnvCos] (-1) 0
      frq' = "freq"@@440
      amp = "amp"@@0.2
      aev = envGen KR 1 1 0 dur RemoveSynth ashp
      ashp = envPerc 2e-2 1
      pan = lfdNoise3 'φ' KR (1/13) * 0.5
      dur = "dur"@@1
      dtl = rand 'α' 0 1e-2
      dtr = rand 'β' 0 1e-2
      sig' = mce2 (delayL sig 1e-2 dtl) (delayL sig 1e-2 dtr)
  in  out ("out"@@0) (pan2 sig' pan 1)

-- | Screaming gremlin, with granularation.
bth03 :: UGen
bth03 =
  let ampf = cubed (clip2 (lfdNoise3 'a' KR dmf' * 0.5 + 0.5) 1 * dmi') + dense'
      dense' = 12.5
      dmf' = 3.7
      dmi' = 7
      edur = "edur"@@1e-3
      mkO i =
        let f = lag (tExpRand i 4000 24000 tick * clip (lfdNoise3 i KR 0.75) 0 1)
                (tExpRand i 0.125 3 tick)
            a = envGen KR tick vc 0 edur DoNothing
                (env [1e-9,1e-9,1,1e-9] [0,1-edgy,edgy] [EnvNum ecrv] (-1) 0)
            p = rand i (-pi) pi
            o = sinOsc AR f p * a
            l = delayC o 1e-2 (rand (-i) 0 1e-2 * dtn)
            r = delayC o 1e-2 (rand i 0 1e-2 * dtn)
        in  mce2 l r
      tick = impulse KR ampf 0
      dtn = 1
      -- ecrv = lfdNoise3 'c' KR 0.125 * 12
      ecrv = 0.995
      edgy = 0.9999
      -- edgy = clip (lfdNoise1 'e' KR 0.125) 0 1
      vc = 1
      oss = foldl' (\acc i -> acc + mkO i) 0 $ [1000..1031::Int]
      sig = pan2 oss (linLin (lfNoise2 'p' KR (1/59)) (-1) 1 (-0.3) 0.3) 1
  in  out ("out"@@0) $ clip2 (sig*0.14) 1 * 0.3

-- | Percussive mixed sound of FM and high pass noise.
bth04 :: UGen
bth04 =
  let sig = (no1+so1) * aenv * amp
      so1 = sinOsc AR (frq + (frq * sinOsc KR (mfrq*fltq) 0 * midx)) 0
      no1 = rhpf (whiteNoise 'ζ' AR) fltf fltq * 0.5
      fltf =
        envGen KR 1 1 0 dur DoNothing (envPerc 1e-1 1) * frq
      fltq =
        envGen KR 1 1 0 dur DoNothing
        (env [0,1,0.2,0.8,0] [t1,t2,t3,t4] [EnvNum (-9)] (-1) 0)
      t1 = rand 't' 1e-3 1e-2
      t2 = rand 'u' 1e-3 1e-1
      t3 = rand 'v' 1e-3 1e-1
      t4 = rand 'w' 1e-3 1e-1
      aenv =
        envGen KR 1 1 0 dur RemoveSynth
        (env [0,1,0] [edgey,1-edgey] [EnvCub] (-1) 0)
      edgey = "edgey"@@1e-3
      amp = "amp"@@0.3
      frq = "freq"@@1300
      mfrq = "mfreq"@@12
      midx = "midx"@@1.25
      dur = "dur"@@1
      pan = "pan"@@0
  in  out ("out"@@0) (pan2 sig pan 1)

-- | Percussive noise sound.
bth05 :: UGen
bth05 =
  let sig = foldl' (+) 0 [nz1, nz2, nz3, o1] * amp

      nz1 = flt1 (whiteNoise 'ε' AR) * ae1
      flt1 x = resonz x freq1 0.05
      ae1 = envGen KR 1 1 0 dur RemoveSynth (envPerc 1e-3 1)
      freq1 = "freq1"@@1080

      nz2 = flt2 (whiteNoise 'δ' AR) * ae2
      flt2 x = rhpf x freq2 0.2
      ae2 =
        decay2 (dust 'd' KR 32) 1e-3 0.4 *
        decay (impulse KR 1 0) 1
      freq2 = "freq2"@@2003

      nz3 = flt3 (whiteNoise 'ι' AR) * ae3
      flt3 x = rlpf x freq3 0.04
      ae3 = envGen KR 1 1 0 (dur*0.8) DoNothing (envPerc 1e-3 1)
      freq3 = "freq3"@@8829

      o1 = pulse AR (ofreq*120) 0.5 * ae4
      ofreq =
        envGen KR 1 1 0 (dur*0.8) DoNothing (env [1,0.25] [1] [EnvCos] (-1) 0)
      ae4 =
        envGen KR 1 1 0 dur DoNothing
        (env [0,1,0] [1e-4,999e-4] [EnvCub] (-1) 0)

      amp = "amp"@@0.1
      dur = "dur"@@1
      pan = "pan"@@0

  in  out ("out"@@0) (pan2 sig pan 1)

bth06 :: UGen
bth06 =
  let sig = pulse AR frqs pw * amp
      pw = lfdNoise3 'p' KR (8.3213) * 0.3 + 0.3
      frqs = mce (map (*frq) partials) * fenv
      partials = [0.251, 1, 1.321, 2.8829, 5.71832, 7.2813, 11.2109]
      fenv = envGen KR 1 1 0 edur DoNothing (env [0.25,1] [1] [EnvExp] (-1) 0)
      frq = "freq"@@162
      hit =
        envGen KR tick 1 0 0.5 DoNothing
        (env [0,1,1,0] [1e-2,0.2,0.15] [EnvCub] (-1) 0)
      tick = dust 'h' KR 8 + coinGate 'g' 0.25 (impulse KR 8 0)
      amp = "amp"@@0.3 * hit * squared eenv
      eenv =
        envGen KR 1 1 0 edur RemoveSynth (env [0,1] [1] [EnvSin] (-1) 0)
      edur = "edur"@@32
      pan = lfdNoise3 'p' KR (1/pi) * 0.4
  in  out ("out"@@0) (pan2 (mix sig) pan 1)

------------------------------------------------------------------------------
-- Effects

-- | Master mixer.
bthmst :: UGen
bthmst =
  let mkIn key = in' 2 AR (key@@0) * ((key++"_amp")@@0.3)
      sig = rhpf (foldl' (+) 0 [mkIn "in1", mkIn "in2", mkIn "in3"]) 10 0.4
  in  replaceOut ("out"@@0) (limiter sig 1 1)

-- | Reverb with allpass filter.
bthrev :: UGen
bthrev =
  let rev = foldr f insig dts
      dts =
        [ 0.00232, 0.00343, 0.0123, 0.0132, 0.0819
        , 0.12132, 0.24892, 0.2898, 0.3832, 0.4532 ]
      f x acc = allpassC acc 0.1 x (rand 'd' 1e-4 1e-1)
      insig = in' 2 AR ("in"@@0)
      wet = "wet"@@0.5
      sig = (rev * wet) + (insig * (1-wet))
  in  replaceOut ("out"@@0) sig

------------------------------------------------------------------------------
-- Controls

-- | Lagged signal, using lag.
bthlgc :: UGen
bthlgc =
  let sig = lag ("val"@@0) ("dur"@@1)
  in  out ("out"@@0) sig

-- | Lagged signal, using lag2.
bthlgc2 :: UGen
bthlgc2 =
  let sig = lag2 ("val"@@0) ("dur"@@1)
  in  out ("out"@@0) sig