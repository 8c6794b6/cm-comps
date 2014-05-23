module Session.Session06 where

import Sound.Study.ForUserInterfaces.TUI.TUI01 (masterNid)
import Sound.Study.ForUserInterfaces.TUI.TUI02
import Session.Synthdefs (synthdefs, changed)

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
        "amp" ==> curveTo EnvCub 8 0

t103 :: IO ()
t103 = withSC3 $ runTrack 103 $ do
    offset 8

    source "sin05" $ do
        "freq" ==>
            sustain
            (sswitch1
             -- (sxrand sinf
             --  [ sseq (siwhite sinf 1 4 * 256) [0]
             --  , sseq (siwhite sinf 1 4 * 16) [1]
             --  , sseq (siwhite sinf 1 4 * 8) [2]])
             (sseq sinf [0])
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
             , 1, 0, 1, 0,  srand 4 [1,1,0] ])

        "dur" ==> sustain
            ((sstutter (siwhite sinf 1 4)
              (srand sinf [1,2,4,8])) *
             sseq sinf [0.5, 0.5, 0.25, 0.75])
        "pan" ==>
            sustain (sval 0.5)
        "amp" ==> curveTo EnvCub 8 0.3

    let inpt :: String -> (UGen->UGen->UGen) -> Input
        inpt prm = input 103 (synthName ==? "sin05") (paramName ==? prm)
        fsin2 i ffrq fd fpan  =
            source' i "sin05" $ do
                "freq" ==> inpt "freq" (\_ x -> ffrq x)
                "tr"   ==> inpt "tr" (\_ x -> x)
                "dur"  ==> inpt "dur"
                    (\_ x -> squared (lfdNoise3 i KR (1/8)) * fd x)
                "pan"  ==> fpan
                "amp"  ==> curveTo EnvCub 64 0.3
                "atk"  ==> linLin (lfdNoise3 i KR (1/15)) (-1) 1 0 1
        ld3 i f = linLin (lfdNoise3 i KR f + 2) 1 3 0 1

    fsin2 1 (*0.25) (*2.8) $ ld3 'd' 7
    fsin2 2 (*0.5)  (*2) $ ld3 'e' 9
    fsin2 3 (*1.008) (*1.8) $ ld3 'f' 8
    fsin2 4 (*2) (*2)$ ld3 'g' 7
    fsin2 5 (*3) (*3) $ ld3 'h' 8
    fsin2 6 (*4) (*2.5) $ ld3 'i' 9
    fsin2 7 (*5) (*3.1) $ ld3 'j' 7
    fsin2 8 (*6) (*2.1) $ ld3 'k' 8
    fsin2 9 (*7) (*2.2) $ ld3 'l' 9
    fsin2 10 (*0.998) (*1.8) $ ld3 'f' 8

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
            let df = linExp (lfdNoise1 'G' KR (1/3) + 2) 1 3 (1/8) 8
                m  = linLin (lfdNoise3 'L' KR (1/8)) (-1) 1 0 1
            in  linExp ((sinOsc KR df 0) * m + 2) 1 3 0.2 0.9

    effect "dc01" $ do
        "wet" ==> curveTo EnvLin 8 1

    effect "lmt01" $ do
        "wet" ==> curveTo EnvLin 8 1

    router $ do
        -- "amp" ==> curveTo EnvCub 16 0.38
        "amp" ==> curveTo EnvCub 8 0

t104 :: IO ()
t104 = withSC3 $ runTrack 104 $ do
    offset 8
    let rep1   = siwhite sinf 1 4
        in104  = input 104 (synthName ==? "saw03")
        infreq = in104 (paramName ==? "freq")
        degs   = [0,3,5,7,10]
    source "saw03" $ do
        param "freq"
            (sustain
             (sstutter
              (srand sinf
               [ sshuf rep1 [1,1,2]
               , sseq rep1 [2,1,1]
               , srand rep1 [sseq 1 [3,5], sseq 1 [5,3]]
               , sseq (2 ** siwhite sinf 1 7) [1]
               , sseq 1 [ 2,1,1, 1,2,1
                        , srand 1
                          [ srand 1 [4, sseq 1 [2,2]]
                          , sseq 1 [1,1,1,1] ]]
               , 4, 8
               ])
              (midiCPS
               (sseq sinf
                [ sxrand sinf
                  [ sseq rep1 degs
                  , sseq rep1 [0,3,5,8,10]
                  , sseq rep1 [0,2,5,7,10] -- map (\x -> (x+7) `mod` 12) degs
                  , sseq rep1 [1,3,6,8,10]
                  , sseq rep1 [1,3,5,8,10]
                  ] ] +
                (12 *
                 let lo = srand 1 [3,4]
                     hi = srand 1 [6,7]
                 in  sseq sinf
                     [lo,5,5,5, hi,5,5,5]) +
                sstutter
                (2 ** siwhite sinf 5 9)
                (srand sinf [-10,-7,-5,-3])
               ))))
        param "tr"
            (infreq
             (\tr isig ->
               changed isig 0 + coinGate 'd' 0.05 tr))
        param "en"
            (linLin (lfdNoise1 'E' KR (1/7) + 2) 1 3 (-5) 5)
        param "pan"
            (infreq
             (\_tr isig ->
               tRand 'P' 0 1 (changed isig 0)))
        param "dur"
            (curveTo EnvCub 8 3.3)
        param "atk"
            (infreq
             (\_tr isig ->
               tExpRand 'A' 1e-3 0.999 (changed isig 0)))

    effect "muladd" $ do
        "wet" ==> curveTo EnvLin 8 1
        "mul" ==> curveTo EnvCub 4 200

    effect "clip2" $ do
        param "wet" $ curveTo EnvLin 4 1
        param "clip" $ curveTo EnvLin 4 0.6

    effect' 1 "muladd" $ do
        param "wet" $ curveTo EnvLin 4 1
        param "mul" $ curveTo EnvCub 4 0.3

    effect "lpf01" $ do
        param "wet" (curveTo EnvLin 8 1)
        param "cf"
            (infreq
             (\tr isig ->
               let tr' = coinGate 'T' 0.01 tr + changed isig 0
                   dur = tExpRand 'D' 0.4 0.8 tr'
                   ccf = linExp (squared (lfdNoise1 'L' KR (5/3)) + 1) 1 2
                         2000 12000
               in  decay tr' dur * ccf))
        param "rq"
            -- (linLin (sinOsc KR (1/3) 0) (-1) 1 0.1 0.8)
            (curveTo EnvLin 8 0.6)

    effect "cmb02" $ do
        param "wet" (curveTo EnvCub 32 0.003)
        param "dcy" (curveTo EnvCub 1e-9 4)
        param "dlt" (curveTo EnvCub 1e-9 ((120/60) * (0.98/4)))

    effect "ap02" $ do
        param "wet"
            -- (curveTo EnvLin 8 1)
            (squared (squared (lfSaw KR (1/128) 0)) `lag` 0.1)
        param "dcy" (curveTo EnvCub 64 8)

    effect' 1 "ap02" $ do
        param "wet" (mulAdd (sinOsc KR (1/126) 0) 0.5 0.5)
        param "dcy" (squared (squared (lfSaw KR 2 0)) `lag` 0.1)

    effect "dc01" $ do
        "wet" ==> curveTo EnvCub 8 1

    router $ do
        -- "amp" ==> curveTo EnvCub 8 0.8
        "amp" ==> curveTo EnvCub 8 0

t108_freqbuf :: Num a => a
t108_freqbuf = 13

b013_pchs :: [Double]
b013_pchs =
    let pchs = takeWhile (\x -> midiCPS x < 20000) $
               foldr (\o acc -> map (+o) degs ++ acc) [] octs
        degs = [0,3,4,5,7,10]
        octs = iterate (+12) 24
    in  pchs

b013 :: IO ()
b013 = withSC3 $ send $ b_alloc_setn1 t108_freqbuf 0 b013_pchs

t108 :: IO ()
t108 = withSC3 $ runTrack 108 $ do
    offset 8
    -- `pchs' could be shared with buffer to reduce amount of data sent on
    -- change, see above b13.
    -- let pchs = foldr (\o acc -> map (+o) degs ++ acc) [] octs
    --     degs = [0,3,7]
    --     octs = take 7 $ iterate (+12) 24
    source "sin05" $ do
        "amp"  ==> curveTo EnvLin 8 0.1
        "freq" ==> linExp (lfdNoise3 '\NUL' KR (1/9)+2) 1 3 20 300
        "tr"   ==> dust '\NUL' KR 2
        "dur"  ==> curveTo EnvLin 8 2
        "pan"  ==> linLin (lfdNoise3 '\NUL' KR (1/3)) (-1) 1 0 1
    let inp p = input 108 (synthName ==? "sin05") (paramName ==? p)
        fsin i = source' i "sin05" $ do
            "amp" ==>
                let df = linExp (lfdNoise3 i KR 1 + 2) 1 3 0.25 8
                in  linExp (squared (lfdNoise1 i KR df) + 2) 1 3 1e-9 0.5
            "freq" ==>
                -- (linExp (lfdNoise3 i KR (1/31) + 2) 1 3 120 12000)
                sustain
                (midiCPS
                 (sstutter
                  (2 ** siwhite sinf 0 4)
                  (sbufrd
                   (sval 13)
                   (sibrown sinf 0 (sval (constant (length b013_pchs))) 1) Loop))
                )
                -- (\tr -> tChoose i (coinGate i (1/32) tr) pchs)
            "tr"  ==> inp "tr"
                (\_ t -> t + dust i KR (lfdNoise3 i KR (1/3) * 3 + 3))
            "dur" ==> linExp (lfdNoise3 i KR 0.3 + 2) 1 3 0.5 2
            "pan" ==> linLin (lfdNoise3 i KR (1/3)) (-1) 1 0 1
            "atk" ==> linLin (lfdNoise3 i KR 1 + 2) 1 3 0 1
    mapM_ fsin [0..63]
    effect "ap02" $ do
        "wet" ==> curveTo EnvLin 32 1
        "dcy" ==> linExp (squared (lfdNoise3 'd' KR (1/4)) + 1) 1 2 0.15 12
    effect "dc01" $ do
        "wet" ==> curveTo EnvLin 8 1
    effect "lmt01" $ do
        "wet" ==> curveTo EnvLin 8 1
    router $ do
        "amp" ==> curveTo EnvCub 8 0

main :: IO ()
main = t99 >> t103
