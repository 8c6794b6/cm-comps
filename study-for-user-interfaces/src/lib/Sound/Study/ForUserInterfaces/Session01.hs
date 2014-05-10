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

import Sound.OSC
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
synth_saw01 :: UGen
synth_saw01 = out (control KR "out" 0) $ osig
  where
    osig  = rlpf (saw AR (mce [freq,freq*1.001])) cf rq  * 0.1 * amp
    rq    = control KR "rq" 0.1
    cf    = linExp (control KR "cf" 0.5 + 0.001) 0.001 1.001 20 20000 `lag` lagt
    freq  = control KR "freq" 440 `lag` lagt
    lagt  = linExp (control KR "lagt" 0 + 0.001) 0.001 1.001 0.001 1
    amp   = control KR "amp" 0

-- | Percussive synth with 'resonz'ated 'whiteNoise'.
synth_nz02 :: UGen
synth_nz02 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = mix (resonz (whiteNoise 'w' AR) (mce [2232,3123,4502]) 0.3 * aenv)
    aenv  = decay t_tr0 0.2
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 0.3

-- | Percussive synth with 'sineOsc' and 'saw'.
synth_bd03 :: UGen
synth_bd03 = out (control KR "out" 0) $ pan2 osig pan 1
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

synth_poly01 :: UGen
synth_poly01 = out obus (pan2 osig pan 1)
  where
    npoly  = 8
    osig   = sum $ map fsig [1..npoly::Int]
    c      = constant
    fsig i = rlpf (pulse AR freq wd) cf rq * aenv * 0.1
      where
        tr   = pulseDivider tr0 (c npoly) (c i)
        aenv = envGen KR tr 1 0 dur DoNothing $
               Envelope [0,1,0.8,0] [0.001,0.5,0.4999] [EnvSqr] (Just 0) Nothing
        freq = gate freq0 tr
        wd   = linLin (lfdNoise3 i KR (15.2 * aenv)) (-1) 1 0.25 0.5
        cf   = squared aenv * freq * 8
        rq   = 0.8
    freq0 = control KR "freq" 440
    dur   = linExp (control KR "dur" 0.5 + 0.001) 0.001 1.001 0.1 16
    tr0   = tr_control "t_tr0" 1
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    obus  = control KR "out" 0

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
synth_ap01 :: UGen
synth_ap01 = replaceOut (control KR "out" 0) osig
  where
    osig    = wsig * wet + isig * (1-wet)
    wsig    = foldr f isig (zipWith mce2 (rs "abcd") (rs "efgh"))
    f x acc = allpassN acc 0.1 x dcy
    rs      = map (\i -> rand i 0.001 0.05)
    isig    = in' 2 AR (control KR "in" 0)
    wet     = control KR "wet" 0 `lag2` 0.1
    dcy     = linExp (control KR "dcy" 0.2 + 0.001) 0.001 1.001 0.25 8

-- | Eeffect synth with 'rlpf' and 'rhpf', /rq/ values are shared.
synth_eq02 :: UGen
synth_eq02 = replaceOut (control KR "out" 0) osig
  where
    osig   = wsig * wet + isig * (1-wet)
    wsig   = rlpf isig0 lfreq rq
    isig0  = rhpf isig hfreq rq
    rq     = clip (control KR "rq" 0.5) 0.001 1
    isig   = in' 2 AR (control KR "in" 0)
    lfreq  = linExp (control KR "lfreq" 1 + 0.001) 0.001 1.001 20 17000
    hfreq  = linExp (control KR "hfreq" 0 + 0.001) 0.001 1.001 20 17000
    wet    = control KR "wet" 0

-- | Sends synthdefs defined in this Haskell module.
sendSynthdefs :: Transport m => m ()
sendSynthdefs = mapM_ (async . d_recv) synthdefs

-- | Synthdefs defined in this haskell module.
synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

-- --------------------------------------------------------------------------
--
-- * Setup
--
-- --------------------------------------------------------------------------

-- | Do this before invoking 'sampleSetup'.
initSession01 :: IO ()
initSession01 = withSC3 $ do
    sendSynthdefs
    initializeTUI01

-- | Sample setup.
sampleSetup :: IO ()
sampleSetup = do

    withSC3 $ do
        -- Adding synths
        mapM_ sendSynth $ words "saw01 nz02 bd03"

    -- Adding supply synths for controlling above synths.
    saw01_freq_01
    saw01_cf
    nz02_t_tr
    bd03_t_tr

    -- Adding couple effects.
    withSC3 $ do
        sendFx "saw01" "ap01"
        sendFx "nz02" "ap01"
        sendFx "bd03" "ap01"

        -- Fade in
        sendParam (synthName ==? "saw01" ||?
                   synthName ==? "bd03" ||?
                   synthName ==? "router") "amp" 1 12

        -- Enable effect
        sendParam (synthName ==? "ap01") "wet" 0.5 8


-- --------------------------------------------------------------------------
--
-- ** Patterns with demand UGens
--
-- --------------------------------------------------------------------------

-- | Send 'pat01' and map the output to freq input of saw01.
saw01_freq_01 :: IO ()
saw01_freq_01 = withSC3 $
    sendSupply01 "saw01" "freq" False $ fmap (midiCPS . (+50)) $
    let a = sseq 3
            [ sseq 1
              [0, 0, 7, 0, 0, 7, 0, 7]
            , srand 8
              [-12, 0, 2, 5, 7, 12]
            ]
        b = srand 16 [-24,-12,0,12,24]
    in  sseq sinf [ a, b, fmap (+12) a, b, fmap (+(-12)) a, b ]

saw01_freq_02 :: IO ()
saw01_freq_02 = withSC3 $
    sendSupply01 "saw01" "freq" False $ fmap (midiCPS . (+62)) $
    sshuf sinf
      [ sseq 1 [0, 7, 0, 12]
      , sseq 1 [0, 2, 7, 12]
      , sseq 1
        [0, srand 1 [-12,0,12], 7, srand 1 [-5, 7, 19]]]

saw01_freq_03 :: IO ()
saw01_freq_03 = withSC3 $
    sendSupply01 "saw01" "freq" False $ fmap (midiCPS . (+38)) $
    sseq sinf
    [ 0, srand 1 [-10, 2, 14, 26], 7, srand 1 [-5, 12, 19] ]

saw01_freq_04 :: IO ()
saw01_freq_04 = withSC3 $
    sendSupply01 "saw01" "freq" False $ fmap midiCPS $
    sseq sinf
    [srand 1
     [snil, sseq 1 [24,31,36,43,48,55]]
    ,sseq (siwhite sinf 2 5)
     [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
    ,srand (siwhite sinf 3 9)
     [74,75,77,79,81]]

saw01_freq_05 :: IO ()
saw01_freq_05 = withSC3 $
    sendSupply01 "saw01" "freq" False $
    (\x y -> lfdNoise3 x KR 0.12 * 880 + y) <$>
    sseq sinf [30,400,5,30,400,5,30,400] <*>
    srand sinf [220,440,660,880]

saw01_cf :: IO ()
saw01_cf = withSC3 $
    sendSupply01 "saw01" "cf" False $

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
        , sseq 16 b ]

    -- srand sinf (map (sval . constant) [0.5,0.52..0.9])

saw01_cf_02 :: IO ()
saw01_cf_02 = withSC3 $
    sendSupply01 "saw01" "cf" False $
    sseq sinf (map (/8) [5,1,1,5, 1,1,5,1])

saw01_rq :: IO ()
saw01_rq = withSC3 $
    sendControl01 "saw01" "rq" $ \_ ->
    let rt = linExp (control KR "rt" 0 + 0.01) 0.01 1.01 (1/30) 30
    in  squared (lfdNoise3 'r' KR rt) + 0.01

saw01_ap01 :: IO ()
saw01_ap01 = withSC3 $ sendFx "saw01" "ap01"

saw01_eq02 :: IO ()
saw01_eq02 = withSC3 $ sendFx "saw01" "eq02"

saw01_amp :: IO ()
saw01_amp = withSC3 $ sendParam (nodeId ==? 102) "amp" 1 18

nz02_t_tr :: IO ()
nz02_t_tr = withSC3 $ do
    sendSupply01 "nz02" "t_tr0" True $ fmap (/10) $
        -- srand sinf
        -- [ 4, sseq 3 [srand 1 [0, 0, 0, 2, 4]]
        -- , 8, sseq 7 [srand 1 [0, 0, 2, 4] ]
        -- ]

        -- sseq sinf [8, 0, srand 1 [0,4,7], 3]
        -- sseq sinf [ 0, 0, 7, srand 1 [0,0,0,4,7] ]

        sseq sinf
        [ let x = srand 1 [6,7,8,9]
              y = srand 1 [0,1,2,3]
          in  sseq 7 [x,y,y, x,y,y, x,y]
        , sseq 3 [0]
        , srand 5 [1,2,3,4,5,6,7,8] ]

        -- sseq sinf
        -- [ sseq 24 [0], sseq 8 [srand 1 [0,2,4,6]] ]

        -- sseq sinf [0,0,6, srand 1 [0,0,0,4] ]
        -- sseq sinf [ sseq 3
        --             [6, sseq 11 [0], srand 4 [1,2,3,4,5,6]]
        --           , let x = srand 1 [4,6,8]
        --                 y = srand 1 [0,0,1,2,3]
        --             in sseq 1 [x,y,0,x, y,0,x,y] ]

        -- sseq sinf
        -- [ sseq 6 [ srand 1 [4,6]
        --          , 0
        --          , srand 1 [0,4]
        --          , srand 1 [0,1] ]
        -- , srand 8 [2,4,6,10] ]


nz02_t_tr_2 :: IO ()
nz02_t_tr_2 = withSC3 $ do
    sendControl01 "nz02" "t_tr0" $
        let rt = linExp (control KR "rt" 0 + 0.01) 0.01 1.01 1 16
        in  const (dust 'a' KR rt)

nz02_t_tr_3 :: IO ()
nz02_t_tr_3 = withSC3 $ do
    sendSupply02 (synthName ==? "nz02") "t_tr0" 4 True $
        sseq sinf [0,0,1,0, 0,1,0,1]

nz02_amp :: IO ()
nz02_amp = withSC3 $ sendParam (nodeId ==? 104) "amp" 1 19

bd03_t_tr :: IO ()
bd03_t_tr = withSC3 $ do
    sendSupply02 (synthName ==? "bd03") "t_tr0" 4 True $
        fmap (/10) $
        let r = srand 1 (1:replicate 15 0)
        in  sseq sinf [ sseq 7 [1, 0, r, r]
                      , sseq 1 [1, 0, 0, 1] ]
        -- sseq sinf [ sseq 4 [1], srand 12 [0,1]]

bd03_t_tr_2 :: IO ()
bd03_t_tr_2 = withSC3 $ do
    -- sendSupply01 "bd03" "t_tr0" True $
    --     let f p = (<=* p) <$> swhite 1 0 1
    --     in  sseq sinf [1, f (1/16), f (1/8), f (1/5)]
    sendSupply02 (synthName ==? "bd03") "t_tr0" 4 True $
        let f p = swhite 1 0 1 <=* p
        in  sseq sinf
            [1,       f (1/32), f (1/16), f (1/10)
            ,f (5/6), f (1/32), f (1/16), f (1/10)]

poly01_init :: IO ()
poly01_init = withSC3 $ do
    _ <- sendSynth "poly01"
    sendFx "poly01" "ap01"

poly01_params :: IO ()
poly01_params = withSC3 $ do
    sendParam (nodeId ==? 104) "amp" 0.3 8

poly01_freq :: IO ()
poly01_freq = withSC3 $ do
    sendSupply01 "poly01" "freq" False $
        fmap (midiCPS . (+62)) $
        -- sseq sinf (foldr (\a b -> map (+a) [0,4,7] ++ b) [] [-24,-12,0,12,24])
        -- srand sinf [0,2,5,7] +
        -- srand sinf [-24,-12,0,12,24]
        srand sinf (foldr (\x acc -> map (+x) [0,2,5,7] ++ acc) [] [-24,-12,0,12,24])
        -- ((+) <$>
        --  sseq sinf [0,2,5,7] <*>
        --  (srand sinf [-24,-12,0,12,24]))

poly01_freq_02 :: IO ()
poly01_freq_02 = withSC3 $ do
    sendControl01 "poly01" "freq" $ \_ ->
        linExp (lfdNoise3 'f' KR 2 + 2) 1 3 120 12000

poly01_t_tr :: IO ()
poly01_t_tr = withSC3 $ do
    sendSupply01 "poly01" "t_tr0" True $
        let p x = swhite 1 0 1 <=* x
        -- -- in  sseq sinf [ p 1, sseq 3 [p 0.1]
        -- --               , p 0.8, sseq 3 [p 0.2] ]
        -- in  sseq sinf [p 1, p 0, p 0, p 0, p 0, p 0, p 1, p 0.5]
        in sseq sinf
           [ p 0.75
           , sseq 15 [p 0.085]
           , p 0.125
           , sseq 15 [p 0.085] ]
           -- , swhite 15 0 1 <=* 0.085 ]
           -- , sseq 15 [p 0.095] ]

poly01_pan :: IO ()
poly01_pan = withSC3 $ do
    sendControl02 (synthName ==? "poly01") "pan" 4 $ \_ ->
        linLin (lfdNoise3 'p' KR 35) (-1) 1 0 1

poly01_short :: IO ()
poly01_short = withSC3 $ do
    sendSupply01 "poly01" "t_tr0" True $ sseq sinf [1]
    sendParam (synthName ==? "poly01") "dur" 0.15 3

poly01_long :: IO ()
poly01_long = do
    poly01_t_tr
    withSC3 $ sendParam (synthName ==? "poly01") "dur" 3.9 3

tuis05_init :: IO ()
tuis05_init = withSC3 $ do
    _ <- sendSynth "tuis05"
    sendFx "tuis05" "ap01"

tuis05_pan :: IO ()
tuis05_pan = withSC3 $ do
    -- sendControl01 "tuis05" "pan" (const (sinOsc KR 0.25 0 * 0.5 + 0.5))
    sendSupply01 "tuis05" "pan" False $ do
        -- sseq sinf
        --     [ sstutter (srand 1 [4,8]) $
        --       swhite 1 0 1 ]
        sseq 1 [0.5]

tuis05_freq_02 :: IO ()
tuis05_freq_02 = withSC3 $
    sendSupply01 "tuis05" "freq" False $
    fmap (const $ lfdNoise3 'a' KR 4.8 + 1 * 800) snil

tuis05_freq :: IO ()
tuis05_freq = withSC3 $ do
    sendSupply01 "tuis05" "freq" False $
        fmap (midiCPS . (+62)) $
        -- sseq sinf
        -- [ sseq  6 [0,7,14,12]
        -- , srand 8 [0,7,12,19] ]

        -- sseq sinf [ srand 16 [-12, -5, 0, 7, 12, 24]
        --           , sseq 2 [0,7,-12,7, 12,7,0,7] ]

        srand sinf [-24, -12, -5, 0, 5, 7, 12, 17, 19, 24]

        -- sseq sinf
        -- [ sseq 1 [0, srand 1 [-12,-5], 7, srand 1 [12,19] ]
        -- , sseq 1 [7, srand 1 [-12,-5], 0, srand 1 [7,24] ] ]

tuis05_freq_03 :: IO ()
tuis05_freq_03 = withSC3 $ do
    sendControl01 "tuis05" "freq" $ \_ ->
        let fmin = linExp (control KR "fmin" 0.1 + 0.001) 0.001 1.001 20 20000
            fmax = linExp (control KR "fmax" 0.8 + 0.001) 0.001 1.001 20 20000
            ffrq = linExp (control KR "ffrq" 0.2 + 0.001) 0.001 1.001 (1/32) 320
        in  linLin (lfdNoise0 'a' KR ffrq) (-1) 1 fmin fmax `lag3` 1

tuis05_t_tr0 :: IO ()
tuis05_t_tr0 = withSC3 $ do
    sendSupply01 "tuis05" "t_tr0" True $
    -- sseq sinf [ sseq 3 [1, 0, 0, 1, 0, 0, 1, 0]
    --           , srand 8 [1,0]
    --           ]
        sseq sinf
        [ srand 16 [0,0,0,1]
        , sseq 1 [1,1,0,1, 1,0,1,1, 0,1,1,0, 1,0,1,0] ]
    -- sseq sinf
    -- [ 1, sseq 15 [0] ]
    -- [ 1, 0, 0, 0, ]
    -- sseq sinf [ sseq 4 [1], srand 12 [0,1]]

tuis05_idx :: IO ()
tuis05_idx = withSC3 $ do
    sendSupply01 "tuis05" "idx" False $
        -- fmap (const (lfdNoise3 'i' KR 0.25 * 0.5 + 0.5)) $ snil
        -- swhite sinf 0 1
        -- fmap (/10) $
        -- sseq sinf
        -- [ sseq 1 [0, 2, 4, 8, 0, 8, 4, 8]
        -- , sseq 1 [0, 2, 4], srand 5 [2,4,8] ]
        -- sstutter (srand sinf [1,4,8,16,32]) $
        -- sseq sinf [sseries 16 0 0.01
        --           ,sseries 16 0.16 (-0.01)]
        sseq sinf
        [ sgeom 32 0.1 1.075
        , sgeom 32 (0.1 * 1.075 ** 32) (recip 1.01) ]

tuis05_idx_02 :: IO ()
tuis05_idx_02 = withSC3 $ do
    sendSupply02 (synthName ==? "tuis05") "freq" 4 False $
        sseq sinf
        -- [ siwhite 12 1 8 * 33
        -- , sseq 1 [33,33,66,33] ]
        [ sseq 3 [33,33,66,33]
        , siwhite 4 1 8 * 33 ]
        -- let sw = linLin (sinOsc KR 0.25 0) (-1) 1 22 2200
        --     nz = linLin (lfdNoise3 'a' KR 0.5) (-1) 1 17 1700
        -- in  srand sinf
        --     [ swhite 16 0 1 * 330
        --     , sseq 16 [sval sw]
        --     , sseq 4 [33, 33, 66, 33]
        --     , sseq 16 [sval nz] ]

-- tuis05_idx :: IO ()
-- tuis05_idx = undefined

-- XXX:
-- Add function to specify amp of newly added synth without looking group
-- ID and use it.
tuis05_amp_and_effect :: IO ()
tuis05_amp_and_effect = withSC3 $ do
    sendParam (nodeId ==? 110) "amp" 0.6 8
    sendParam (nodeId ==? 110) "wet" 1 0.2
    sendParam (nodeId ==? 110) "dcy" 0.8 3

{-

do { sendParam (nodeId ==? 112) "amp" 1 19
   ; sendParam (nodeId ==? 104) "amp" 0 9
   ; sendParam (nodeId ==? 108) "amp" 0.8 19
   ; sendParam (nodeId ==? 110) "amp" 1 9
   }

-}

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
    f a b = allpassN b 0.1 a dcy
    v     = rlpf (pulse AR (mce2 freq (freq*1.01)) bw) cf rq * amp
    rq    = lfdNoise3 'q' KR 1.110 * 0.498 + 0.5
    cf    = lfdNoise3 'n' KR 2.323 * 2000 + 2200
    bw    = lfdNoise3 'b' KR 0.1123 * 0.48 + 0.5
    mkRs  = map (\x -> rand x 0.001 0.1)
    freq  = midiCPS (demand tick 0 (evalSupply supSpe (mkStdGen 0x81aafad)))
    amp   = decay2 tick 5e-4 950e-3 * 0.2
    tick  = impulse KR (tfreq*12) 0
    dcy   = linExp (control KR "dcy" 0 + 0.001) 0.001 1.001 0.1 8
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
