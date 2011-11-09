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
import Sound.SC3.Lepton

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
      -- pan = "pan"@@0
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
      -- edur = mouseX KR 1e-4 1 Exponential 0.1
      edur = "edur"@@1e-3
      -- edur = 0.006
      mkO i =
        let f = lag (tExpRand i 4000 20000 tick * clip (lfdNoise3 i KR 0.75) 0 1)
                (tExpRand i 0.25 4 tick)
            a = envGen KR tick vc 0 edur DoNothing
                (env [1e-9,1e-9,1,1e-9] [0,1-edgy,edgy] [EnvNum ecrv] (-1) 0)
            p = rand i (-pi) pi
            o = sinOsc AR f p * a
            l = delayC o 1e-2 (rand (-i) 0 1e-2 * dtn)
            r = delayC o 1e-2 (rand i 0 1e-2 * dtn)
        in  mce2 l r
        -- in  sinOsc AR f p * a
      tick = impulse KR ampf 0 -- + dust 'd' KR (ampf/2)
      dtn = 1 -- sinOsc KR (lfdNoise3 'j' KR (1/33)) 0 * 0.5 + 0.5
      -- ecrv = linLin (lfdNoise3 'c' KR 0.125) (-1) 1 (-12) 12
      -- ecrv = lfdNoise3 'c' KR 0.125 * 12
      ecrv = 0.995
      edgy = 0.9999
      -- edgey = clip (lfdNoise1 'e' KR 0.125) 0 1
      -- edgey = linLin (lfdNoise3 'g' KR 0.06125) (-1) 1 0 1
      -- tick = dust 'd' KR ampf
      vc = 1
      oss = foldl' (\acc i -> acc + mkO i) 0 $ [1000..1015::Int]
      -- sig = oss
      sig = pan2 oss (linLin (lfNoise2 'p' KR (1/59)) (-1) 1 (-0.3) 0.3) 1
  in  out ("out"@@0) $ clip2 (sig*0.14) 1 * 0.3

------------------------------------------------------------------------------
-- Effects

-- | Master mixer.
bthmst :: UGen
bthmst =
  let mkIn key = in' 2 AR (key@@0) * ((key++"_amp")@@0.3)
      sig = foldl' (+) 0 [mkIn "in1", mkIn "in2", mkIn "in3"]
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