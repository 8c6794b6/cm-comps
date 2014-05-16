{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Sound.Study.ForUserInterfaces.Session04 where

import Sound.OSC
import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
import Sound.SC3.Supply
import Sound.SC3.TH.Synthdef (synthdefGenerator)
import Sound.SC3.Tree

import Sound.Study.ForUserInterfaces.TUI02

synth_fm01 :: UGen
synth_fm01 = out obus osig
  where
    osig = pan2 sosc pan 1
    sosc = sinOsc AR freq phs * aenv * 0.3
    aenv = envGen KR tr 1 0 dur DoNothing
           (Envelope [0,1,0] [atk,1-atk] [EnvCub] Nothing Nothing)
    phs  = sinOsc AR (freq*mfac) 0 * idx
    idx  = k "idx" 1
    mfac = k "mfac" 1
    obus = k "out" 0
    freq = k "freq" 440
    atk  = k "atk" 1e-3
    dur  = k "dur" 1
    pan  = k "pan" 1
    tr   = tr_control "tr" 0
    k    = control KR

synth_cmb03 :: UGen
synth_cmb03 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = combC isig 1 dlt dcy
    dlt  = k "dlt" 0.22
    dcy  = k "dcy" 4
    isig = in' 2 AR (k "in" 0)
    wet  = k "wet" 0
    obus = k "out" 0
    k    = control KR

synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

sendSynthdefs :: IO ()
sendSynthdefs = withSC3 $ mapM_ (async . d_recv) synthdefs

initSession04 :: IO ()
initSession04 = withSC3 $ do
    mapM_ (async . d_recv) $(synthdefGenerator)
    initializeTUI02

t99 :: IO ()
t99 = withSC3 $ runTrack 99 $ do
    offset 8
    effect "cmb03" $ do
        "wet" ==> curveTo EnvLin 32 1
        "dlt" ==> sustain (sval 0.38)
        "dcy" ==> curveTo EnvLin 64 8
    router $ do
        "amp" ==> curveTo EnvCub 16 1.2

t106 :: IO ()
t106 = withSC3 $ runTrack 106 $ do
    offset 8
    source "fm01" $ do
        (param "freq"
         (sustain

          -- (midiCPS
          --  (sstutter
          --   (srand sinf [1,2,4,8])
          --   (srand sinf [0,3,7] +
          --    (12 * sseq sinf
          --     [3,5,5,srand 1 [3,8], 3,5,7,5 ]))))
          -- (sseq sinf [0,7,12,0, 7,12,0,24] + 60)

          (midiCPS
           (sstutter
            -- (1 * sseq sinf [2,1,4,1])
            (srand sinf [2,4] * sseq sinf [2,2,1,3])
            (let x = srand 1 [0,3,7,10]
             in  sseq sinf
                 [ sseq 3
                   [ sseq 2 [0,3,x,0, 3,x,0,x]
                   , sseq 2 [0,3,x,10, 10,x,3,0] ]
                 , sseq 1 [0,x,3,x,7,x,10,x]
                 , srand 17 [0,3,x,7]
                 ] +
                 sstutter
                 (srand sinf [1,2,4])
                 (srand sinf [0,-3,0,0,0,0,2]) +
                 sseq sinf
                  [ 3,5,7,5
                  , srand 1 [3,5],5,7,5] * 12)))
          -- (midiCPS
          --  (sseq sinf [-24,7,12,0, 7,12,0,24] + 36))
          -- (sseq sinf [0,7,12,0, 7,12,0,))
          -- (sswitch
          --  (sstutter (srand sinf [1,2,4,8,16])
          --    (srand sinf [0,1]))
          --      [ sval
          --        (linExp (lfdNoise3 'F' KR (1/2) + 2) 1 3 110 1380)
          --      , midiCPS (sseq sinf [0,7,12,0, 7,12,0,24] + 60)])
         ))
        param "mfac"
            -- linExp (lfdNoise3 'M' KR 5 + 2) 1 3 0.25 4
            -- (sustain
            --  (sstutter (srand sinf [1..16])
            --   (srand sinf [0.25,0.75,1,1.5,2,2.5,8]))))
            -- (sustain (sval 0.5)))
            -- (let df = linLin (lfSaw KR (1/13) 0) 1 3 1 100 `lag` 0.5
            --  in  linLin (lfdNoise3 'M' KR df + 2) 1 3 (1/16) 16)
            (sustain
             (sstutter
              (sseq sinf [1,1,2])
              (sseq sinf [ 1,2,srand 1 [3,5,7] ,1
                         , 1,2,srand 1 [1,2,4,8,16], 2] * 0.25
              )))
            -- (\tr ->
            --   let vs = mce [0.25,0.75,1,1.5,2,2.5,8]
            --   in  tChoose 'v' (coinGate 'g' 0.5 tr) vs `lag` 0.08))
        param "idx" $
            -- (sustain
            --  (sstutter (srand sinf [1,1,1,3,5,8])
            --   (sseq sinf
            --    [ sseq 3 [12,2,0.5, 10,2,0.5, 8,0.5]
            --    , srand 8 [0,0.5,2,8,10,12] ])))
            -- linLin (squared (lfdNoise3 'd' KR 1.8)) 0 1 0 8
            sustain
             (sstutter
              (srand sinf [1,2,4])
              (sseq sinf
               [ sseq 3 [8,7,0,6, 8,7,6,0]
               , swhite 8 0 10 ]))

        (param "pan"
         (sustain
          (sstutter (srand sinf [1,2,4])
           (swhite sinf (-0.3) 0.3))))

        param "tr"
            -- (trigger
            --  (sxrand sinf
            --   [ sseq 2 [1,0,srand 1 [1,0],0]
            --   , srand 8 [0,1]
            --   , sseq 1 [sseq 3 [0], sseq 5 [1]]
            --   , sseq 1 [1,0,0,1,0,0,1,0] ]))
            -- (trigger $ sseq sinf [1,1,0,1])
            (trigger $
             sseq sinf [1, sseq (2 * srand sinf [1,2] - 1) [0]])
        param "atk" $
            -- (linExp (lfdNoise1 'a' KR (1/7) + 2) 1 3 1e-3 0.99)
            -- (sustain
            --  (srand sinf [0.1,0.2..0.5]))
            -- linLin (lfdNoise1 'A' KR (1/9) + 2) 1 3 1e-4 0.5
            -- curveTo EnvCos 4 1e-4
            \tr -> tChoose 'a' (mce [1e-4,0.69]) (coinGate 'g' 0.25 tr)
        param "dur" $
            linLin (lfdNoise1 'D' KR (1/8) + 2) 1 3 2 6
            -- (linExp (lfdNoise3 'a' KR 7 + 2) 1 3 2e-1 8)

    effect' 1 "cmb03" $ do
        param "wet"
            (sustain
             -- (sseq sinf
             --  [ sseq 4 [0,0,srand 1 [1,0],0, 0,1,0,1]
             --  , sseq 2 [sseq 4 [0], srand 4 [1,0]]
             --  , srand 16 [0,1] ])
             (sval 0)
             -- (sstutter 4
             --  (sseq sinf
             --   [ sseq 3 [0,0,0,1]
             --   , srand 2 [1,0], 1,1
             --   , sseq 3 [0,srand 1 [0,1], 0,srand 1 [0,1]]
             --   , srand 1 [1,0], 1,1,1]))
             )
        param "dcy"
            (linLin (lfdNoise3 'Y' KR 2 + 2) 1 3 0.1 2)
        param "dlt"
            (linLin (lfdNoise3 'L' KR 3 + 2) 1 3 (recip 200) (recip 5))

    effect "cmb03" $ do
        param "wet" (curveTo EnvCub 8 0.3)
        param "dlt" $
            -- curveTo EnvCos 32 0.100
            linLin (lfdNoise1 'V' KR 5.192 + 2) 1 3 0.100 0.104
            -- linLin (lfdNoise3 'Y' KR (1/64)) (-1) 1 0.003 0.8
        param "dcy" (curveTo EnvCub 32 8)


    effect "dc01" $
        "wet" ==> curveTo EnvSin 4 1

    router $ do
        param "amp" (curveTo EnvCub 32 0)
        -- param "amp" (squared (lfdNoise3 'a' KR (1/9)) * 0.8)


class BuildList a r | r -> a where
    build' :: [a] -> a -> r

instance BuildList a [a] where
    build' l x = reverse $ x : l

instance BuildList a r => BuildList a (a->r) where
    build' l x = \y -> build' (x:l) y

bl :: BuildList a r => a -> r
bl x = build' [] x

-- --------------------------------------------------------------------------
--
-- * Experiments
--
-- --------------------------------------------------------------------------

synth_def1 :: UGen
synth_def1 = osig
  where
    osig = out obus (sinOsc AR freq 0 * decay tr 0.123 * 0.3)
    tr   = dust 'k' KR df
    df   = linLin (sinOsc KR (1/64) 0) (-1) 1 1 30
    freq = control AR "freq" 440
    obus = control KR "out" 0

synth_def2 :: UGen
synth_def2 = out obus (sinOsc AR freq 0 * mul + add)
  where
    freq = k "freq" 440
    mul  = k "mul" 1
    add  = k "add" 0
    obus = k "out" 0
    k    = control KR

synth_def3 :: UGen
synth_def3 = out obus (sinOsc KR freq 0 * mul + add)
  where
    freq = k "freq" 440
    mul  = k "mul" 1
    add  = k "add" 0
    obus = k "out" 0
    k    = control KR

s_new_ex01 :: IO ()
s_new_ex01 = withSC3 $ do
    send $ withCM
        (d_recv
         (synthdef "foo"
          (out 0 (pan2 (sinOsc AR 440 0 * 0.1) 0 1))))
        (s_new "foo" 1002 AddToTail 1 [])

s_new_ex02 :: IO ()
s_new_ex02 = withSC3 $ do
    send $ withCM
        (d_recv
         (synthdef "foo2"
          (out 0
           (sinOsc AR (control KR "freq" 440) 0 * 0.1))))
         (bundle immediately
          [ s_new "foo2" (-1) AddToTail 1 [("freq",330)]
          , s_new "foo2" (-1) AddToTail 1 [("freq",440)]
          , s_new "foo2" (-1) AddToTail 1 [("freq",550)] ])

controlBus_ex01 :: IO ()
controlBus_ex01 = withSC3 $ do
    let f1 = syn "def2"
               ["freq"*=3,"mul"*=220,"add"*=330,"out"*=2]
        f2 = syn "def3"
               ["freq"*=3,"mul"*=220,"add"*=330,"out"*=1023]
        nd =
            grp 0
            [ grp 1
              [ grp 10 [f1,f2]
              , grp 11
                [ syn "def1" ["freq"*<-f2-*"out","out"*=1]
                , syn "def1" ["freq"*<=f1-*"out","out"*=0] ]]]

    mapM_ (async . d_recv . uncurry synthdef)
        [("def1",synth_def1),("def2",synth_def2),("def3",synth_def3)]
    play nd

audioBus_ex01 :: IO ()
audioBus_ex01 = withSC3 $ do
    return ()

triggerOnce_ex01 :: IO ()
triggerOnce_ex01 = withSC3 $ do
    let d1    = out (k "out" 0) (gate e exceed)
        e     = envGen KR matched 1 0 2 DoNothing
                (Envelope [from,to] [1] [EnvLin] Nothing Nothing)
        to      = 880
        from    = in' 1 KR 100
        matched = count ==* 4
        exceed  = count >* 4
        count = pulseCount tr 1
        tr    = impulse KR 1 0
        d2 = out (k "out" 0) (sinOsc AR freq 0 * 0.3)
        freq = k "freq" 440
        k    = control KR
    mapM_ (async . d_recv . uncurry synthdef) [("d1",d1),("d2",d2)]
    sendOSC $ bundle immediately
        [ s_new "d1" (-1) AddToHead 1 [("out",100)]
        , s_new "d2" (-1) AddAfter (-1) [("out",0)]
        , n_map (-1) [("freq",100)] ]

-- in dumped message
-- audio bus 0 = "c-552556612"
-- audio bus 1 = "c-552556548"
-- audio bus 2 = "c-552556484"

queryTree_ex01 :: IO ()
queryTree_ex01 = withSC3 $ do
    send $ g_queryTree [(101,True)]
    msg <- waitMessage
    liftIO $ putStrLn $ messagePP msg

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)
