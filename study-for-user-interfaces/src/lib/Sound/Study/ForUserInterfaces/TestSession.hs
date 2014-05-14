{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Sound.Study.ForUserInterfaces.TestSession where

import Sound.OSC
import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
import Sound.SC3.Supply
import Sound.SC3.TH.Synthdef (synthdefGenerator)
import Sound.SC3.Tree

import Sound.Study.ForUserInterfaces.TUI02

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

-- --------------------------------------------------------------------------
--
-- * Synthdef
--
-- --------------------------------------------------------------------------

synth_sin03 :: UGen
synth_sin03 = out obus osig
  where
    osig   = pan2 (sinOsc AR (freq+vib) 0 * aenv * 0.3) pan 1
    aenv   = envGen KR tr 1 0 dur DoNothing $
             Envelope [0,1,1,0] [atk,sus,dec] [EnvCub] Nothing Nothing
    vib    = lfTri KR vrate 0 * vlevel
    pan    = linLin (k "pan" 0) 0 1 (-1) 1
    dur    = k "dur" 0.4
    atk    = k "atk" 0.01
    sus    = k "sus" 0.29
    dec    = abs (1 - atk - sus) + 0.001
    freq   = k "freq" 440
    vrate  = k "vrate" 3
    vlevel = k "vlevel" 10
    tr     = tr_control "tr" 1
    obus   = k "out" 0
    k      = control KR

synth_sin04 :: UGen
synth_sin04 = out obus osig
  where
    osig = pan2 (sosc * aenv * 0.3) pan 1
    sosc = sinOsc AR freq 0
    aenv = envGen KR tr 1 0 0.3 DoNothing (envPerc 1e-3 1)
    obus = k "out" 0
    freq = k "freq" 440
    pan  = k "pan" 0
    tr   = tr_control "tr" 0
    k    = control KR

synth_saw02 :: UGen
synth_saw02 = out obus osig
  where
    osig = pan2 (saw AR freq * aenv * 0.3) pan 1
    aenv = envGen KR tr 1 0 0.3 DoNothing
           (Envelope [0,1,1,0] [1e-3,0.2,0.799] [EnvSin] Nothing Nothing)
    obus = k "out" 0
    freq = k "freq" 220
    pan  = k "pan" 0
    tr   = tr_control "tr" 0
    k    = control KR

synth_lp01 :: UGen
synth_lp01 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = rlpf isig cf rq
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    wet  = k "wet" 0
    cf   = k "cf" 2000
    rq   = k "rq" 0.3
    k    = control KR

synth_ap02 :: UGen
synth_ap02 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = foldr f isig $ zip "abcde" "fghij"
    f (r,l) acc =
         let fr i = rand i 0.001 0.1
         in  allpassN acc 0.1 (mce [fr r, fr l]) dcy
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    wet  = k "wet" 0
    dcy  = k "dcy" 0.5
    k    = control KR

sendSynthdefs :: IO ()
sendSynthdefs =
    withSC3 $ mapM_ (async . d_recv) $(synthdefGenerator)


-- --------------------------------------------------------------------------
--
-- * Controls
--
-- --------------------------------------------------------------------------

initTestSession :: IO ()
initTestSession = withSC3 $ do
    mapM_ (async . d_recv) $(synthdefGenerator)
    initializeTUI02

changes :: IO ()
changes = t99 >> t107

t99 :: IO ()
t99 = withSC3 $ runTrack 99 $ do
    router $ do
        "amp" ==> line KR 0 1 10 DoNothing

t107 :: IO ()
t107 = withSC3 $ runTrack 107 $ do
    offset 8
    source "sin03" $ do
        "tr" ==>
            trigger
             (sseq sinf
              [ sseq 1 [1,0,0,srand 1 [1,0]]
              , sseq 2 [srand 1 [1,0],0] ])
        "dur" ==> curveTo EnvLin 8 0.32
        "freq" ==>
            sustain
            (midiCPS
             (sstutter 4
              ((sseq sinf [4,9,5,8,6,7] * 12) +
               (sseq sinf [0,3,5,7,10]))))
        "atk" ==> linLin (lfdNoise0 'A' KR (1/5)) (-1) 1 0.001 0.999
        "sus" ==> sustain (sval (linLin (sinOsc KR (1/3) 0) (-1) 1 0.1 0.9))
        "vlevel" ==> sustain (sval 8)
        "vrate" ==> sustain (sval 2.5)
        "pan" ==> sustain (sval 0.5)
    effect "ap02" $ do
        "wet" ==> curveTo EnvLin 3 1
        -- "dcy" ==> curveTo EnvLin 32 16
        "dcy" ==> linLin (sinOsc KR (1/7) 0 + 2) 1 3 4 16
    effect "lp01" $ do
        "wet" ==> curveTo EnvLin 16 1
        "cf"  ==>
            let df  = linExp (lfdNoise3 'F' KR (1/8) + 2) 1 3 (1/16) 16
                mul = linLin (lfdNoise1 'M' KR (1/9)) (-1) 1 0 1
            in  linExp ((sinOsc KR df 0 * mul) + 2) 1 3 50 12000
        "rq"  ==> curveTo EnvLin 18 0.8
    router $ do
        "amp" ==> curveTo EnvCub 16 0.8

ng01 :: IO ()
ng01 = withSC3 $ runTrack 107 $ do
    -- XXX: Writing below is possible in current types.
    source "bar0" $
        source "bar1" $
        source "bar2" $
        effect "bar3" (param "wet" (1::Double))

-- | Testing curve.
t106 :: IO ()
t106 = withSC3 $ runTrack 106 $ do
    offset 4
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
        "pan"  ==> curveTo EnvLin 8 0.5
    effect "ap02" $ do
        "wet" ==> curveTo EnvLin 8 0.5
        "dcy" ==>
            (let fq = linExp (lfdNoise1 'A' KR (1/32) + 2) 1 3 (1/3) 3
             in  lfTri KR fq 0 * 0.5 + 0.5)
    router $ do
        "amp" ==> curveTo EnvCub 24 0.6

-- | Using same synthdef multiple times in one track.
t109 :: IO ()
t109 = withSC3 $ runTrack 109 $ do
    offset 8
    source' 0 "sin04" $ do
        "freq" ==> sustain
            (midiCPS
              (sseq sinf [0,4,5,7, 2,4,5,7] +
               (sstutter
                (srand sinf [1,1,1,2,4,8])
                (12 * srand sinf [3,4..8]))
             ))
        "tr" ==> trigger
            (srand sinf [sseq 1 [1,1,0,1, 1,1,0], srand 1 [1,0] ])
        "pan"  ==> sustain (sval 0)
    let fin = in' 1 KR 428
        tin = in' 1 KR 429
    source' 1 "sin04" $ do
        "freq" ==> fin * 2
        "tr"   ==> tin
        "pan"  ==> sustain (sval (-0.3))
    source' 2 "sin04" $ do
        "freq" ==> fin * 0.5
        "tr"   ==> tin
        "pan"  ==> sustain (sval 0.3)
    source' 3 "sin04" $ do
        "freq" ==> fin * 0.25
        "tr"   ==> tin
        "pan"  ==> sustain (sval 0.15)
    source' 4 "sin04" $ do
        "freq" ==> fin * 3
        "tr"   ==> tin
        "pan"  ==> sustain (sval (-0.15))
    effect "ap02" $ do
        "wet"  ==> linLin (lfSaw KR (1/32) 0) (-1) 1 0 1 `lag` 2
        "dcy"  ==> linLin (lfdNoise3 'D' KR (1/5)) (-1) 1 0 1
    router $ do
        "amp"  ==> curveTo EnvCub 8 0.6


-- | Using same source synth multiple times.
t108 :: IO ()
t108 = withSC3 $ runTrack 108 $ do
    offset 32

    let p = sstutter 2
            (sseq sinf
             [ sseq 3 [ 0,5,7,0, 5,7,0,5
                      , 7,0,5,7, srand 1 [0,12],5,7,0]
             , srand 16 [0,5,7,12]
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
        "amp" ==> curveTo EnvCub 32 0.28

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

nd02_before :: SCNode
nd02_before =
    Group 101
    [ Synth 1616049966 "sin02"
      ["out":=18]
    , Synth 1777095663 "ap01"
      ["in":=18,"out":=18]
    , Synth 1425562103 "cmb02"
      ["in":=18,"out":=18]
    , Synth 10199 "router"
      ["in":=18,"out":=16] ]

nd02_after :: SCNode
nd02_after =
    Group 101
    [ Synth 53822416 "sin03"
      ["out":=18]
    , Synth 112652199 "ap02"
      ["in":=18,"out":=18]
    , Synth 690662386 "lp01"
      ["out":=18,"in":=18]
    , Synth 10199 "router"
      ["in":=18,"out":=16] ]
