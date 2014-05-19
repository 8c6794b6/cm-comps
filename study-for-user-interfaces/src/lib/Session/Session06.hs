{-# LANGUAGE TemplateHaskell #-}
module Session.Session06 where

import Sound.Study.ForUserInterfaces.TUI.TUI01 (masterNid)
import Sound.Study.ForUserInterfaces.TUI.TUI02
import Session.Synthdefs (synthdefs)

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

-- --------------------------------------------------------------------------
--
-- * Synthdefs
--
-- --------------------------------------------------------------------------

sendSynthdefs :: IO Message
sendSynthdefs = withSC3 . async . foldr1 withCM $ map d_recv synthdefs


-- --------------------------------------------------------------------------
--
-- * Functions to modify
--
-- --------------------------------------------------------------------------

t99 :: IO ()
t99 = withSC3 $ runTrack masterNid $ do
    offset 8
    effect "dc01" $ do
        "wet" ==> sustain (sval 1)
    effect "lmt01" $ do
        "wet" ==> curveTo EnvLin 16 1
    router $ do
        "amp" ==> curveTo EnvCub 32 1

t103 :: IO ()
t103 = withSC3 $ runTrack 103 $ do
    offset 8

    source "sin05" $ do
        "freq" ==>
            sustain
            (sswitch1
             (sxrand sinf
              [ sseq (siwhite sinf 1 4 * 256) [0]
              , sseq (siwhite sinf 1 4 * 16) [1]
              , sseq (siwhite sinf 1 4 * 8) [2]])
             [ let deg  = [0,0,0,2, 2,5,7,7]
                   stt1 = [1,1,1,1, 1,1,1,1, 2,2,2,2, 4,4, 8]
               in sstutter
                  (srand sinf
                   [ sseq (64*64) [1]
                   , srand 32 [1,2]
                   , sshuf 2 stt1 ])
                  (midiCPS
                   (sseq sinf
                    [ sseq 8
                      [sser 64
                       [sshuf ((siwhite sinf 1 8) * 8) deg]]]
                    +
                    (sseq sinf
                     [ srand 1 [36,84]
                     , sseq ((siwhite sinf 1 5 * 2) - 1) [60]])
                    +
                    (sstutter (siwhite sinf 2 6 * 65 * 2)
                     (sibrown sinf (-6) 6
                      (siwhite sinf 1 7)))))
             , midiCPS
               (sstutter
                (2 ** siwhite sinf 1 2)
                (sseq sinf
                 [sshuf
                  (siwhite sinf 1 4 * 8)
                  [0,2,4,5,7,9,11]])
                +
                sstutter
                (2 * sseq sinf [1,1,1,1, 2,2, 4])
                (sseq sinf [3,5,7] * 12))
             , midiCPS
               ((sibrown sinf 0 11 1) + sibrown sinf 3 9 1 * 12)
             ])
        "tr" ==> trigger
            (sseq sinf
             [ 1, 1, 0, 1,  1, 0, 1, 0
             , 1, 0, 1, 0, srand 4 [1,1,0]])

        "dur" ==> sustain
            ((sstutter (siwhite sinf 1 4)
              (srand sinf [2,4,8])) *
             sseq sinf [0.5, 0.5, 0.25, 0.75] *
             (sstutter
              (siwhite sinf 2 4 * 4)
              (swhite sinf 1 2)))
        "pan" ==>
            sustain (sval 0.5)
        "amp" ==> curveTo EnvCub 8 0.3

    let inpt prm = input 103 (synthName ==? "sin05") (paramName ==? prm)
        fsin2 i ffrq fd fpan  =
            source' i "sin05" $ do
                "freq" ==> inpt "freq" ffrq
                "tr"   ==> inpt "tr" id
                "dur"  ==> inpt "dur"
                    (\x -> squared (lfdNoise3 i KR (1/8)) * fd x)
                "pan"  ==> fpan
                "amp"  ==> linLin (lfdNoise3 i KR (1/13)) (-1) 1 0 0.3
                "atk"  ==> linLin (lfdNoise3 i KR (1/15)) (-1) 1 0 1
        ld3 i f = linLin (lfdNoise3 i KR f + 2) 1 3 0 1

    fsin2 1 (*0.25) (*2.8) $ ld3 'd' 7
    fsin2 2 (*0.5)  (*2) $ ld3 'e' 9
    fsin2 3 (*1.008) (*1.8) $ ld3 'f' 8
    fsin2 4 (*2) (*1.2)$ ld3 'g' 7
    fsin2 5 (*3) (*1.3) $ ld3 'h' 8
    fsin2 6 (*4) (*0.9) $ ld3 'i' 9
    fsin2 7 (*5) (*0.8) $ ld3 'j' 7
    fsin2 8 (*6) (*0.8) $ ld3 'k' 8
    fsin2 9 (*7) (*0.8) $ ld3 'l' 9

    effect' 1 "ap01" $ do
        "wet" ==> curveTo EnvLin 16 0.5
        "dcy" ==> curveTo EnvLin 16 0.5

    -- effect "rz01" $ do
    --     "wet" ==> curveTo EnvLin 32 1
    --     "cf"  ==>
    --         let df = linExp (lfdNoise1 'F' KR (1/3) + 2) 1 3 (1/32) 8
    --         in  linExp (sinOsc KR df 0 + 2) 1 3 800 8000
    --     "rq"  ==>
    --         let df = linExp (lfdNoise1 'R' KR (1/3) + 2) 1 3 (1/32) 8
    --         in  linExp (sinOsc KR df 0 + 2) 1 3 0.7 0.8

    -- effect "ap02" $ do
    --     "wet" ==> linLin (lfdNoise1 'A' KR (1/9)) (-1) 1 0 1
    --     "dcy" ==> sustain (sval 0.7)

    -- effect "cmb02" $ do
    --     "wet" ==> curveTo EnvLin 24 1
    --     "dcy" ==> curveTo EnvLin 8 2
    --     "dlt" ==> \tr ->
    --         let tr' = coinGate 'L' (1/9) tr
    --         in  (60/120) / (2 * tIRand 'R' 2 8 tr' `lag` 0.001)

    effect "lpf01" $ do
        "wet" ==> curveTo EnvLin 32 0.8
        "cf"  ==>
            let df = linExp (lfdNoise1 'F' KR (1/3) + 2) 1 3 (1/16) 16
                m  = linLin (lfdNoise3 'M' KR (1/8)) (-1) 1 0 1
            in  linExp ((sinOsc KR df 0) * m + 2) 1 3 200 20000
        "rq"  ==>
            -- curveTo EnvLin 16 0.3
            let df = linExp (lfdNoise1 'G' KR (1/3) + 2) 1 3 (1/8) 8
                m  = linLin (lfdNoise3 'L' KR (1/8)) (-1) 1 0 1
            in  linExp ((sinOsc KR df 0) * m + 2) 1 3 0.2 0.9

    -- effect' 2 "ap01" $ do
    --     "wet" ==> curveTo EnvLin 16 0.25
    --     "dcy" ==> curveTo EnvLin 16 0.3

    effect "dc01" $ do
        "wet" ==> curveTo EnvLin 8 1

    effect "lmt01" $ do
        "wet" ==> curveTo EnvLin 8 1

    router $ do
        "amp" ==> curveTo EnvCub 64 1
        -- "amp" ==> curveTo EnvCub 1e-9 0

t108 :: IO ()
t108 = withSC3 $ runTrack 108 $ do
    offset 8
    source "sin05" $ do
        "amp" ==> curveTo EnvLin 8 0.1
        "freq" ==> linExp (lfdNoise3 '\NUL' KR (1/9)+2) 1 3 20 300
        "tr"   ==> dust '\NUL' KR 2
        "dur"  ==> curveTo EnvLin 8 2
        "pan"  ==> linLin (lfdNoise3 '\NUL' KR (1/3)) (-1) 1 0 1
    let inp p = input 108 (synthName ==? "sin05") (paramName ==? p)
        fsin i = source' i "sin05" $ do
            "amp" ==>
                let df = linExp (lfdNoise3 i KR 1 + 2) 1 3 1 8
                in  linExp (squared (lfdNoise1 i KR df) + 2) 1 3 1e-9 1
            "freq" ==>
                let df = linExp (lfdNoise3 i KR 0.3 + 2) 1 3 (1/8) 8
                in  linExp (squared (lfdNoise1 i KR df) + 2) 2 3 120 12000
            "tr"  ==> inp "tr" (\t -> t + dust i KR 1)
            "dur" ==> linExp (lfdNoise3 i KR 0.3 + 2) 1 3 0.5 2
            "pan" ==> linLin (lfdNoise3 i KR (1/3)) (-1) 1 0 1
            "atk" ==> linLin (lfdNoise3 i KR 1 + 2) 1 3 0 1
    mapM_ fsin [0..31]
    effect "dc01" $ do
        "wet" ==> curveTo EnvLin 8 1
    effect "lmt01" $ do
        "wet" ==> curveTo EnvLin 8 1
    router $ do
        "amp" ==> curveTo EnvCub 8 1

main :: IO ()
main = t99 >> t103
