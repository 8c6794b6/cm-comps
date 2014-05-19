{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Session.Session05 where

import Sound.OSC
import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
import Sound.SC3.Supply
import Sound.SC3.Tree

import Sound.Study.ForUserInterfaces.TUI.TUI02
import Session.Synthdefs (synthdefs)

-- --------------------------------------------------------------------------
--
-- * Synthdef
--
-- --------------------------------------------------------------------------

sendSynthdefs :: IO Message
sendSynthdefs =
    withSC3 . async . foldr1 withCM $ map d_recv synthdefs

-- --------------------------------------------------------------------------
--
-- * Controls
--
-- --------------------------------------------------------------------------

initTestSession :: IO ()
initTestSession = withSC3 $ do
    initializeTUI02

t99 :: IO ()
t99 = withSC3 $ runTrack 99 $ do
    offset 4
    router $ do
        "amp" ==> curveTo EnvLin 32 0

t103 :: IO ()
t103 = withSC3 $ runTrack 103 $ do
    offset 8
    let freq :: (Transport m, Assignable val) => val -> Track m ()
        freq = param "freq"
        amp  :: (Transport m, Assignable val) => val -> Track m ()
        amp  = param "amp"
    source "sin03" $ do
        "tr" ==>
            trigger
             (sseq sinf
              [ sseq 1 [1,0,0,srand 1 [1,0]]
              , sseq 2 [srand 1 [1,0],0] ])
        "dur" ==> curveTo EnvLin 8 0.32
        freq $ sustain
            (midiCPS
             (sstutter 4
              ((sseq sinf [4,9,5,8,6,7] * 12) +
               (sseq sinf [0,3,5,7,10]))))
        "atk" ==> linLin (lfdNoise0 'A' KR (1/5)) (-1) 1 0.001 0.999
        -- "sus" ==> sustain (sval (linLin (sinOsc KR (1/3) 0) (-1) 1 0.1 0.9))
        "sus" ==> linLin (sinOsc KR (1/3) 0 + 2) 1 3 0.05 0.2
        "vlevel" ==> sustain (sval 8)
        "vrate" ==> sustain (sval 2.5)
        "pan" ==> sustain (sval 0.5)
    -- effect "ap02" $ do
    --     "wet" ==> curveTo EnvLin 3 1
    --     -- "dcy" ==> curveTo EnvLin 32 12
    --     "dcy" ==> linLin (lfdNoise3 'Y' KR (1/13) + 2) 1 3 4 12
    effect "lp01" $ do
        "wet" ==> curveTo EnvLin 16 1
        "cf"  ==>
            let df  = linExp (lfdNoise3 'F' KR (1/8) + 2) 1 3 (1/16) 16
                mul = linLin (lfdNoise1 'M' KR (1/9)) (-1) 1 0 1
            in  linExp ((sinOsc KR df 0 * mul) + 2) 1 3 50 12000
        "rq"  ==> curveTo EnvLin 18 0.8
    effect "dc01" $
        "wet" ==> curveTo EnvLin 8 1
    router $ do
        amp $ curveTo EnvCub 16 1
        -- amp $ curveTo EnvCub 1e-9 0

ng01 :: IO ()
ng01 = withSC3 $ runTrack 103 $ do
    router $ do
        "amp" ==> sustain (sval 0)

-- | Testing curve.
t106 :: IO ()
t106 = withSC3 $ runTrack 106 $ do
    offset 8
    source "sin03" $ do
        "freq" ==> sustain
            (midiCPS
             (srand sinf [36,48..108] +
              sseq sinf [-2,0,2]))
        "tr" ==> trigger
            (srand sinf
             [ sseq 1 [1,0,0,0]
             , sseq 2 [1,0]
             , sseq 1 [1,0,0,1]
             , sseq 4 [1]])
        "pan" ==> curveTo EnvLin 8 0.5
    effect "ap02" $ do
        "wet" ==> curveTo EnvLin 8 0.5
        "dcy" ==>
            let fq = linExp (lfdNoise1 'A' KR (1/32) + 2) 1 3 (1/3) 3
            in  lfTri KR fq 0 * 0.5 + 0.5
    router $ do
        "amp" ==> curveTo EnvCub 1e-9 0
        -- "amp" ==> curveTo EnvCub 24 1


-- | Using same synthdef multiple times in one track.
t107 :: IO ()
t107 = withSC3 $ runTrack 107 $ do
    let sus name val = name ==> sustain val
        trg name val = name ==> trigger val
    offset 8
    source' 0 "sin04" $ do
        sus "freq"
            (midiCPS
             ((sstutter (siwhite sinf 1 5)
              (sseq sinf
               [ srand 1 [-12,0], 2, 5, srand 1 [7, 14]
               , 2,7,5,7 ]))
              +
              (12 * sseq sinf [2,3,6,5, 6,5,6,5 ,6,7,6,9, 5])
              +
              sstutter (sibrown sinf 1 128 8)
              (srand sinf [0,2,5,7]))
             -- (sswitch1
             --  (sstutter
             --   (srand sinf [1,2,4,8,16])
             --   (sseq sinf [0,1,2]))
             --  [ srand sinf [48,60,72]
             --  , sibrown sinf 20 100 1
             --  , siwhite sinf 20 100 ])
            )
        trg "tr"
            (srand sinf
             [ sseq 1 [1,1,0,1, 1,1,0]
             , srand 1 [1,0] ])
        sus "pan" (sval 0)
    let ncond = synthName ==? "sin04"
        fin = input 107 ncond (paramName ==? "freq")
        tin = input 107 ncond (paramName ==? "tr")
        vib rate amount = lfTri KR rate 0 * amount
    source' 1 "sin04" $ do
        "freq" ==> fin (\sig -> (sig*1.5) + vib 3 1.5)
        "tr"   ==> tin id
        "pan"  ==> sustain (sval (-0.3))
    source' 2 "sin04" $ do
        "freq" ==> fin (\sig -> (sig*1.003) + vib 0.3 8)
        "tr"   ==> tin id
        "pan"  ==> sustain (sval 0.3)
    source' 3 "sin04" $ do
        "freq" ==> fin (\sig -> (sig*3.002) + vib 0.2 11)
        "tr"   ==> tin id
        "pan"  ==> sustain (sval 0.15)
    source' 4 "sin04" $ do
        "freq" ==> fin (\sig -> (sig*4.008) + vib 8 0.2)
        "tr"   ==> tin id
        "pan"  ==> sustain (sval (-0.15))
    source' 5 "sin04" $ do
        "freq" ==> fin (\sig -> (sig*0.508) + vib 7 9.32)
        "tr"   ==> tin (\tr -> pulseDivider tr 0 3)
        "pan"  ==> sustain (sval (-0.15))
    effect "ap02" $ do
        "wet"  ==> (1 - squared (lfdNoise3 'D' KR (1/5)))
        "dcy"  ==> linLin (lfTri KR (1/32) 0) (-1) 1 0 1 `lag` 2

    effect "dc01" $ do
        sus "wet" (sval 1)
    router $ do
        "amp"  ==> curveTo EnvCub 16 0

-- | Using same source synth multiple times.
t108 :: IO ()
t108 = withSC3 $ runTrack 108 $ do
    -- offset 32
    offset 8
    let p = sstutter 2
            (sseq sinf
             [ sseq (siwhite sinf 8 16)
               [ sseq 3 [ 0,5,7,0, 5,7,0,5
                        , 7,0,5,7, srand 1 [0,12],5,7,0]
               , srand 16 [0,5,7,12]]
             , sseq (siwhite sinf 8 16)
               [ sseq 3 [ 5,0,7,5, 0,7,5,0
                        , 7,5,0,7, 5,0,7,srand 1 [0,12] ]
               , sseq 1 [ 5,0,7,5, 0,7,5,0
                        , 7, srand 7 [0,5,7,12]] ]
             ])
        t = sseq sinf [1,0,1,1]

    source' 0 "sin03" $ do
        "freq" ==> sustain (midiCPS (p+72))
        "tr"  ==> trigger t
        "pan" ==> sustain (sval 0.75)
        "atk" ==> linLin (lfdNoise3 'A' KR (1/3) + 2) 1 3 0.001 0.05

    source' 1 "sin03" $ do
        "freq" ==> sustain (midiCPS (p+79))
        "tr"  ==> trigger t
        "pan" ==> sustain (sval 0.25)
        "atk" ==> linLin (lfdNoise3 'B' KR (1/4) + 2) 1 3 0.001 0.05

    source' 2 "sin03" $ do
        "freq" ==> sustain (midiCPS (p+84))
        "tr"  ==> trigger t
        "pan" ==> sustain (sval 0.5)
        "atk" ==> linLin (lfdNoise3 'C' KR (1/5) + 2) 1 3 0.001 0.05

    effect "ap01" $ do
        "wet" ==> sustain (sval 1)
        "dcy" ==> linLin (lfdNoise3 'D' KR 2 + 2) 1 3 0 1

    router $ do
        -- "amp" ==> curveTo EnvCub 24 0.28
        "amp" ==> curveTo EnvCub 1e-9 0

testCurve01 :: IO ()
testCurve01 = withSC3 $ runTrack 101 $ do
    offset 4
    source "sin03" $ do
        "freq" ==> sustain
            (sswitch1
             (sstutter
              (srand sinf [1,2])
              (srand sinf [0..3]))
             [ sval (linLin (lfClipNoise '0' KR 6 + 2) 1 3 60 240)
             , sval (linLin (lfClipNoise 'g' KR 7 + 2) 1 3 80 8000)
             , 1320
             , sval (linLin (sinOsc KR 3 0 + 2) 1 3 80 8000)
             ])
        "dur" ==> curveTo EnvCub 8 2
        "tr" ==>
            let df = linExp (lfdNoise3 'U' KR (1/23) + 2) 1 3 5 500
            in  dust 'd' KR df
        "pan"  ==> sustain (sval 0.5)
    effect "ap02" $ do
        "wet" ==> linLin (lfSaw KR (1/16) 0 + 2) 1 3 0 1 ** 4 `lag` 0.1
        "dcy" ==> linLin (lfdNoise3 'D' KR (1/31) + 2) 1 3 0 1
    router $ do
        "amp" ==> curveTo EnvCub 8 1

-- | @scsynth@ running with TCP, 127.0.0.1:57111.
withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

-- | Default scsynth, UDP, 127.0.0.1:57110,
-- withSC3 :: Connection UDP a -> IO a
-- withSC3 = withTransport (openUDP "127.0.0.1" 57110)
