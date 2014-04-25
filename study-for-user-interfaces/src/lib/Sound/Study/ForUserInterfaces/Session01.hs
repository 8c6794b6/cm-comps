{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Sample session using functions from "Sound.Study.ForUserInterfaces.TUI01", take
1.

-}
module Sound.Study.ForUserInterfaces.Session01 where

import Control.Applicative ((<$>), (<*>))
import System.Random

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Supply
import Sound.SC3.Tree
import Sound.SC3.TH.Synthdef (synthdefGenerator)

import Sound.Study.ForUserInterfaces.TUI01


-- --------------------------------------------------------------------------
--
-- * Synthdefs
--
-- --------------------------------------------------------------------------

-- | Sample synth controlled by mapping control rate bus with signals from
-- demand ugen.
synth_tuis01 :: UGen
synth_tuis01 = out (control KR "out" 0) $ osig
  where
    osig  = rlpf (saw AR (mce [freq,freq*1.001])) cf rq  * 0.1 * amp
    rq    = control KR "rq" 0.1
    cf    = linExp (control KR "cf" 0.5 + 0.001) 0.001 1.001 20 20000 `lag` lagt
    freq  = control KR "freq" 440 `lag` lagt
    lagt  = linExp (control KR "lagt" 0 + 0.001) 0.001 1.001 0.001 1
    amp   = control KR "amp" 0

-- | Percussive synth with 'resonz'ated 'whiteNoise'.
synth_tuis02 :: UGen
synth_tuis02 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = mix (resonz (whiteNoise 'w' AR) (mce [2232,3123,4502]) 0.3 * aenv)
    aenv  = decay t_tr0 0.2
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 0.3

-- | Percussive synth with 'sineOsc' and 'saw'.
synth_tuis03 :: UGen
synth_tuis03 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = (sig1 + sig2) * amp
    sig1  = mix (sinOsc AR (mce frq1s) 0 * aenv1 * 0.3)
    frq1s = map (*frq1) [1,1.423,2.87,4.12,5.83,7.32]
    aenv1 = envGen KR t_tr0 1 0 dur1 DoNothing $
            Envelope [0,1,0] [0.001, 0.999] [EnvCub] (Just 0) Nothing
    sig2  = saw AR frq2 * aenv2 * 0.2
    aenv2 = envGen KR t_tr0 1 0 dur2 DoNothing $
            Envelope [0,1,0.8,0] [0.01,0.98,0.01] [EnvCub] (Just 0) Nothing
    frq1  = linExp (control KR "frq1" 0.1 + 0.001) 0.001 1.001 20 80
    frq2  = linExp (control KR "frq2" 0.1 + 0.001) 0.001 1.001 200 2000
    dur1  = linExp (control KR "dur1" 0.2 + 0.001) 0.001 1.001 0.1 1
    dur2  = linExp (control KR "dur2" 0.2 + 0.001) 0.001 1.001 0.001 0.01
    amp   = control KR "amp" 0
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 1

synth_tuis04 :: UGen
synth_tuis04 = out obus osig
  where
    osig = sinOsc AR freq 0 * aenv * 0.3
    aenv = envGen KR tr0 1 0 dur DoNothing $
           Envelope [0,1,0.8,0] [0.001,0.5,0.4999] [EnvSqr] (Just 0) Nothing
    freq = control KR "freq" 440
    dur  = linExp (control KR "dur" 0.5 + 0.001) 0.001 1.001 0.1 8
    tr0  = tr_control "t_tr0" 1
    obus = control KR "out" 0

synth_tuis05 :: UGen
synth_tuis05 = out obus (pan2 osig pan 1)
  where
    osig = sinOsc AR freq phs * aenv * 0.3
    aenv = envGen KR tr0 1 0 dur DoNothing $
           Envelope [0,1,0.3,0.2,0] [0.02,0.2,0.78] [EnvCub] (Just 0) Nothing
    tr0  = tr_control "t_tr0" 1
    freq = control KR "freq" 440
    phs  = sinOsc AR (freq * 2.4995) 0 * idx
    idx  = linLin (control KR "idx" 0) 0 1 0 15
    pan  = linLin (control KR "pan" 0) 0 1 (-1) 1
    dur  = linExp (control KR "dur" 0 + 0.001) 0.001 1.001 0.1 8
    obus = control KR "out" 0

-- | Reverb effect with 'allpassN' and foldr.
--
-- Requires /out/ control for specifying audio rate output bus of 'replaceOut',
-- and /in/ control for specifying audio rate input bus.
--
synth_tuie01 :: UGen
synth_tuie01 = replaceOut (control KR "out" 0) osig
  where
    osig    = wsig * wet + isig * (1-wet)
    wsig    = foldr f isig (zipWith mce2 (rs "abcd") (rs "efgh"))
    f x acc = allpassN acc 0.1 x dcy
    rs      = map (\i -> rand i 0.001 0.05)
    isig    = in' 2 AR (control KR "in" 0)
    wet     = control KR "wet" 0 `lag2` 0.1
    dcy     = linExp (control KR "dcy" 0.2 + 0.001) 0.001 1.001 0.25 8

-- | Eeffect synth with 'rlpf' and 'rhpf', /rq/ values are shared.
synth_tuie02 :: UGen
synth_tuie02 = replaceOut (control KR "out" 0) osig
  where
    osig   = wsig * wet + isig * (1-wet)
    wsig   = rlpf isig0 lfreq rq
    isig0  = rhpf isig hfreq rq
    rq     = clip (control KR "rq" 0.5) 0.001 1
    isig   = in' 2 AR (control KR "in" 0)
    lfreq  = linExp (control KR "lfreq" 0 + 0.001) 0.001 1.001 20 17000
    hfreq  = linExp (control KR "hfreq" 0 + 0.001) 0.001 1.001 20 17000
    wet    = control KR "wet" 0

-- | Sends synthdefs defined in this Haskell module.
sendSynthdefs :: IO ()
sendSynthdefs = withSC3 $ mapM_ (async . d_recv) synthdefs

-- | Synthdefs defined in this haskell module.
synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

-- --------------------------------------------------------------------------
--
-- * Setup
--
-- --------------------------------------------------------------------------

-- | Sample setup.
sampleSetup :: IO ()
sampleSetup = do

    -- Initializations
    sendSynthdefs
    initializeTUI01

    -- Adding synths
    mapM_ sendSynth $ words "tuis01 tuis02 tuis03"

    -- Adding supply synths for controlling above synths.
    tuis01_freq_01
    tuis01_cf
    tuis02_t_tr
    tuis03_t_tr

    -- Adding couple effects.
    sendFx "tuis01" "tuie01"
    sendFx "tuis02" "tuie01"
    sendFx "tuis03" "tuie01"

    -- Fade in
    sendParam (synthName ==? "tuis01" ||?
               synthName ==? "tuis03" ||?
               synthName ==? "tuie00") "amp" 1 12
    sendParam (synthName ==? "tuie01") "wet" 0.5 8

-- --------------------------------------------------------------------------
--
-- ** Patterns with demand UGens
--
-- --------------------------------------------------------------------------

-- | Send 'pat01' and map the output to freq input of tuis01.
tuis01_freq_01 :: IO ()
tuis01_freq_01 =
    sendSupply01 "tuis01" "freq" False $ fmap (midiCPS . (+50)) $
    let a = sseq 3
            [ sseq 1
              [0, 0, 7, 0, 0, 7, 0, 7]
            , srand 8
              [-12, 0, 2, 5, 7, 12]
            ]
        b = srand 16 [-24,-12,0,12,24]
    in  sseq sinf [ a, b, fmap (+12) a, b, fmap (+(-12)) a, b ]

tuis01_freq_02 :: IO ()
tuis01_freq_02 =
    sendSupply01 "tuis01" "freq" False $ fmap (midiCPS . (+62)) $
    sshuf sinf $ sseq 1
      [ sseq 1 [0, 7, 0, 12]
      , sseq 1 [0, 2, 7, 12]
      , sseq 1
        [0, srand 1 [-12,0,12], 7, srand 1 [-5, 7, 19]]]

tuis01_freq_03 :: IO ()
tuis01_freq_03 =
    sendSupply01 "tuis01" "freq" False $ fmap (midiCPS . (+38)) $
    sseq sinf
    [ 0, srand 1 [-10, 2, 14, 26], 7, srand 1 [-5, 12, 19] ]

tuis01_freq_04 :: IO ()
tuis01_freq_04 =
    sendSupply01 "tuis01" "freq" False $ fmap midiCPS $
    sseq sinf
    [srand 1
     [snil, sseq 1 [24,31,36,43,48,55]]
    ,sseq (siwhite sinf 2 5)
     [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
    ,srand (siwhite sinf 3 9)
     [74,75,77,79,81]]

tuis01_freq_05 :: IO ()
tuis01_freq_05 =
    sendSupply01 "tuis01" "freq" False $
    (\x y -> lfdNoise3 x KR 0 * 880 + y) <$>
    sseq sinf [30,400,5,30,400,5,30,400] <*>
    srand sinf [220,440,660,880]

tuis01_cf :: IO ()
tuis01_cf =
    sendSupply01 "tuis01" "cf" False $
    -- sseq 1 [0.9]
    -- sseq sinf (map (/8) [5,1,1,5, 1,1,5,1])
    -- sseq sinf [ sseries 64 0.62 0.005
    --           , sseries 64 0.94 (-0.005) ]
    -- srand sinf [ 0.5, 0.6, 0.7, 0.8, 0.9 ]

    -- srand sinf [ 0.45, 0.5, 0.6, 0.9, 0.95, 0.98 ]

    let a = [ 0.45, 0.95, 0.45, 0.95, 0.93, 0.45, 0.98, 0.94 ]
        b = [ 0.45, 0.55, 0.65, 0.75, 0.85, 0.95, 0.98, 0.99 ]
    in  sseq sinf
        [ sseq 3 [sseq 3 a, srand 8 a]
        , sseq 16 b
        ]

    -- srand sinf (map (sval . constant) [0.5,0.52..0.9])

tuis01_cf_02 :: IO ()
tuis01_cf_02 =
    sendSupply01 "tuis01" "cf" False $
    sseq sinf (map (/8) [5,1,1,5, 1,1,5,1])

tuis01_tuie01 :: IO ()
tuis01_tuie01 = sendFx "tuis01" "tuie01"

tuis01_tuie02 :: IO ()
tuis01_tuie02 = sendFx "tuis01" "tuie02"

tuis02_t_tr :: IO ()
tuis02_t_tr =
    sendSupply01 "tuis02" "t_tr0" True $ fmap (/10) $
    srand sinf
    [ 4, sseq 3 [srand 1 [0, 0, 0, 2, 4]]
    , 8, sseq 7 [srand 1 [0, 0, 2, 4] ]
    ]

    -- sseq sinf [8, 0, 4, 3]

    -- sseq sinf [0]
    -- sseq sinf [ 6, srand 3 [0,1,2,3,4] ]

    -- sseq sinf
    -- [ sseq 7 [ srand 1 [4,6]
    --          , 0
    --          , srand 1 [0,4]
    --          , srand 1 [0,1] ]
    -- , srand 4 (map (sval . constant) [2,4,6,10])
    --

tuis02_t_tr_2 :: IO ()
tuis02_t_tr_2 =
    sendControl "tuis02" "t_tr0" $
    (const (dust 'a' KR 8))

tuis03_t_tr_2 :: IO ()
tuis03_t_tr_2 =
    sendSupply01 "tuis03" "t_tr0" True $
    let f p = (<=* p) <$> swhite 1 0 1
    in  sseq sinf [1, f (1/16), f (1/8), f (1/5)]

tuis03_t_tr :: IO ()
tuis03_t_tr =
    sendSupply01 "tuis03" "t_tr0" True $ fmap (/10) $
    let r = srand 1 (1:replicate 15 0)
    in  sseq sinf [ sseq 7 [1, 0, r, r]
                  , sseq 1 [1, 0, 0, 1] ]
    -- sseq sinf [ sseq 4 [1], srand 12 [0,1]]

tuis05_freq_02 :: IO ()
tuis05_freq_02 =
    sendSupply01 "tuis05" "freq" False $
    fmap (const $ lfdNoise3 'a' KR 4.8 + 1 * 800) snil

tuis05_freq :: IO ()
tuis05_freq =
    sendSupply01 "tuis05" "freq" False $
    fmap (\x -> midiCPS (x+50)) $
    sseq sinf [ srand 16 [-12, -5, 0, 7, 12, 24]
              , sseq 2 [0,7,-12,7, 12,7,0,7]
              ]
    -- sseq sinf [0, srand 1 [-12,-5], 7, srand 1 [12,19] ]
    -- ]

tuis05_t_tr0 :: IO ()
tuis05_t_tr0 =
    sendSupply01 "tuis05" "t_tr0" True $
    -- sseq sinf [ sseq 3 [1, 0, 0, 1, 0, 0, 1, 0]
    --           , srand 8 [1,0]
    --           ]
    sseq sinf
    [ srand 16 [0,0,0,1]
    , sseq 1 [1,1,0,1, 1,0,1,1, 0,1,1,0, 1,0,1,0]
    ]
    -- sseq sinf [ sseq 4 [1], srand 12 [0,1]]

tuis05_idx :: IO ()
tuis05_idx =
    sendSupply01 "tuis05" "idx" False $
    -- fmap (const (lfdNoise3 'i' KR 0.25 * 0.5 + 0.5)) $
    -- swhite sinf 0 1
    sseq sinf $ fmap (/ 10) [1, 2, 4, 8, 1, 8, 4, 8]
    -- sseq sinf [sseries 96 0 0.01
    --           ,sseries 96 0.96 (-0.01)
    --           ]

tuis05_pan :: IO ()
tuis05_pan =
    sendSupply01 "tuis05" "pan" False $
    sseq sinf
    [ sstutter (srand 1 [4,8,16]) $
      swhite 1 0 1
    ]


-- --------------------------------------------------------------------------
--
-- * Misc
--
-- --------------------------------------------------------------------------

play_speUGen :: IO ()
play_speUGen = audition speUGen

speUGen :: UGen
speUGen = out 0 sig
  where
    sig   = foldr f v (zipWith mce2 (mkRs "abcd") (mkRs "efgh"))
    f a b = allpassN b 0.1 a 4
    v     = rlpf (pulse AR (mce2 freq (freq*1.01)) bw) cf rq * amp
    rq    = lfdNoise3 'q' KR 1.110 * 0.498 + 0.5
    cf    = lfdNoise3 'n' KR 2.323 * 2000 + 2200
    bw    = lfdNoise3 'b' KR 0.1123 * 0.48 + 0.5
    mkRs  = map (\x -> rand x 0.001 0.1)
    freq  = midiCPS (demand tick 0 (evalSupply supSpe (mkStdGen 0x81aafad)))
    amp   = decay2 tick 5e-4 950e-3 * 0.2
    tick  = impulse KR (tfreq*12) 0
    tfreq = control KR "trate" 0.641025 -- 7.6923

supSpe :: Supply
supSpe =
  sseq sinf
  [srand 1
   [snil, sseq 1 [24,31,36,43,48,55]]
  ,sseq (siwhite sinf 2 5)
   [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
  ,srand (siwhite sinf 3 9)
   [74,75,77,79,81]]
