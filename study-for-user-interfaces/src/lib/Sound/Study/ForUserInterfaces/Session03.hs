{-# LANGUAGE TemplateHaskell #-}
{-|

Session with textual UI, take 3. Using TUI02.

-}
module Sound.Study.ForUserInterfaces.Session03 where

import           Sound.SC3
import           Sound.SC3.ID
import           Sound.SC3.Supply
import           Sound.SC3.TH.Synthdef (synthdefGenerator)

import           Sound.Study.ForUserInterfaces.TUI02
import qualified Sound.Study.ForUserInterfaces.Session01 as Session01
import qualified Sound.Study.ForUserInterfaces.Session02 as Session02


-- --------------------------------------------------------------------------
--
-- * Synthdefs
--
-- --------------------------------------------------------------------------

synth_sin02 :: UGen
synth_sin02 = out obus (pan2 osig pan 1)
    where
      osig = sinOsc AR freq 0 * aenv * 0.3
      aenv = envGen KR tr 1 0 dur DoNothing $
             Envelope [0,1,1,0] [0.001,0.099,0.9] [EnvCub] Nothing Nothing
      obus = k "out" 0
      freq = k "freq" 440
      dur  = k "dur" 0.8
      pan  = linLin (k "pan" 0) 0 1 (-1) 1
      tr   = tr_control "t_tr" 1
      k    = control KR

synth_cmb02 :: UGen
synth_cmb02 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = combC isig 1 dlt dcy
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    dcy  = k "dcy" 8
    dlt  = k "dlt" 0.2
    wet  = k "wet" 0
    k    = control KR

synth_dc01 :: UGen
synth_dc01 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = leakDC isig coef
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    wet  = k "wet" 0
    coef = k "coef" 0.995
    k    = control KR

synthdefs :: [Synthdef]
synthdefs = $synthdefGenerator


-- --------------------------------------------------------------------------
--
-- * Controls
--
-- --------------------------------------------------------------------------

sendSynthdefs :: IO ()
sendSynthdefs = withSC3 $ do
    mapM_ (async . d_recv)
        (synthdefs ++ Session01.synthdefs ++ Session02.synthdefs)

initSession03 :: IO ()
initSession03 = withSC3 $ do
    initializeTUI02

t101 :: IO ()
t101 = withSC3 $ runTrack 101 $ do

    offset 8

    source "sin02" $ do
        "pan"  ==> linLin (lpf (whiteNoise 'w' KR) 5) (-1) 1 0 1
        "dur"  ==> vsup 0.3
        "t_tr" ==> do
            let p1 = sseq 1 [ sseq 3 [1,0,0,1,0,0,1,0]
                            , srand 8 [0,1] ]
            tsup $
                -- sseq sinf
                -- [ p1
                -- , sseq 16 [0]
                -- , srand 32 [0,0,0,1] ]
                -- sseq sinf [1]
                sseq sinf [p1]
                -- srand sinf [0,1]
                -- srand sinf
                -- [ sseq 1 [1, sseq 15 [0]]
                -- , sseq 8 [0]
                -- , sseq 2 [1, sseq 7 [0]] ]
        "freq" ==> vsup
            (
                midiCPS $
                sseq sinf
                -- [ sstutter
                --   (srand sinf [4,8,16])
                --   (srand 1 [48,60,72,84]) ]
                [ 96, sseq 4 [72], 48, sseq 2 [72]]
                +
                sstutter 4
                (sseq sinf
                 [ sseq 3 [0,2,7,0,2,7,0,7]
                 , srand 8 [0,2,5,7] ])

                -- sseq sinf [0,7,5,7]
                -- sseq sinf
                -- [ sstutter 8 (srand sinf [0,2,5,7]) ]
            )

            -- vsup (sval $ midiCPS (lfSaw KR (1/5) 0 * 60 + 12))

    effect "ap01" $ do
        "wet" ==> vsup 1
        "dcy" ==> linLin (sinOsc KR (1/2) 0) (-1) 1 0.01 1
    effect "cmb02" $ do
        "wet" ==> line KR 0 1 119 DoNothing
        "dcy" ==> linLin (lfSaw KR (1/32) 0) (-1) 1 0.05 6 `lag` 0.1
        "dlt" ==> vsup 0.25 -- vsup 0.8

    router $ do
        "amp" ==>
            (\tr ->
              envGen KR tr 1 0 rdur DoNothing
              (Envelope [1,0.6] [1] [EnvCub] Nothing Nothing))

t102 :: IO ()
t102 = withSC3 $ runTrack 102 $ do
    offset 8
    source "nz01" $ do
        "pan"  ==> do
            vsup $
                sstutter (srand sinf [2,4,8]) $
                srand sinf [0.5, swhite 1 0.2 0.8]
        "t_tr" ==> do
            tsup $
                srand sinf
                [ sseq 1 [1,0,0,0]
                , sseq 1 [1,0,1,0]
                , sseq 1 [1,0,0,1]
                , sseq 1 [1,1,0,1]
                , sseq 1 [1,1,1,1] ]
                -- srand sinf
                -- [ sseq 1 [1, 0]
                -- , sseq 1 [1, 1]
                -- , sseq 1 [0, 1]]
                -- sseq sinf
                -- [ sseq 3 [1,1,0,1, 1,0,1,0]
                -- , sser 16 [0]
                -- , srand 1
                --   [ sseq 1 [srand 4 [0,1], sseq 1 [1,1,1,1]]
                --   , srand 8 [0,1]
                --   , sseq 8 [1] ]]
                -- sseq sinf
                -- [ sseq 3 [srand 4 [1,0], sseq 12 [0]]
                -- , srand 8 [0,1,1] ]
                -- sseq sinf [ sseq 8 [0]
                --           , srand (srand sinf [2,4,8])
                --             [sseq 1 [1,0], sseq 1 [1,1] ]]
                -- srand sinf [sseq 1 [1,0], sseq 1 [1,1]]
        "cf" ==> do
            let b = 0.05
                h = 0.96
                s = 0.18
                p1 = [b,h,h,h, s,h,h,h]
                p2 = [b,h,s,b, h,s,s,b]
                p3 = sseq 1
                     [ sgeom 4 0.9 0.6
                     , sgeom 4 (0.9*(0.6**4)) (recip 0.6) ]
                p4 = swhite 8 0 1
            vsup $
                -- sseq sinf
                -- [ sseq 3 [sseq 1 p1, sseq 1 p2]
                -- , p3, p4 ]
                -- sseq sinf p1
                sseq sinf
                [ sseq 1 p1
                , srand 1 [sseq 1 p2, p4]
                , sseq 1 p1
                , srand 1 [srand 8 p1, p3, p4]]

    effect "ap01" $ do
        "wet" ==> lfClipNoise 'w' KR 4 * 0.5 + 0.5
        "dcy" ==> vsup 0.83
    effect "cmb02" $ do
        "wet" ==>
            -- vsup
            -- (sstutter (srand sinf [2,4,8])
            --  (srand sinf [1,0]))
            -- vsup
            -- (sseq sinf
            --  [sstutter 8 (sseq 1 [0,0,1,0, 0,0,1,1])])
            lfClipNoise 'W' KR 2 * 0.5 + 0.5
            -- vsup 0
        "dcy" ==>
            linExp (lfdNoise0 '0' KR 4 + 1.1) 0.1 1.1 0.1 0.8 `lag` 0.01
        "dlt" ==>
            linExp (lfdNoise3 'd' KR 1 + 1.1) 0.1 1.1 0.01 0.03
            -- (\tr ->
            --   let sig = linExp nz 0.1 1.1 (recip 100) (recip 33)
            --       nz  = lfdNoise3 'd' KR 4 + 1.1
            --   in  gate sig (coinGate 'g' 0.35 tr) `lag` 0.01)
    effect "dc01" $ do
        "wet" ==> vsup 1

    router $ do
        "amp" ==>
            (\tr ->
              envGen KR tr 1 0 rdur DoNothing
              (Envelope [2,1.5] [1] [EnvCub] Nothing Nothing))

t103 :: IO ()
t103 = withSC3 $ runTrack 103 $ do
    offset 8

    source "bd03" $ do
        "t_tr0" ==> do
            let p x = swhite 1 0 1 <=* x
            tsup $ sseq sinf [1,p 0.05,p 0.15, p 0.25]
        "amp"   ==>
            vsup (sseq sinf
                  [ swhite 1 0.9 1.5
                  , swhite 1 0.3 0.5
                  , swhite 1 0.5 0.8
                  , swhite 1 0.5 0.9 ])

    effect "ap01" $ do
        "wet" ==> vsup 0.34
        "dcy" ==> lfClipNoise 'Y' KR 2 * 0.5 + 0.5
    effect "cmb02" $ do
        -- Manually typed with printing the contents of current node.
        -- control bus numbers are from t102's wet, dcy, and dlt for
        -- "cmb02" synth.
        "wet" ==>
            -- vsup 0
            -- lfClipNoise 'd' KR (1/2) * 0.5 + 0.5
            (\tr -> envGen KR tr 1 0 16 DoNothing
                    (Envelope [0.5,1,0] [0.5,0.5] [EnvLin] Nothing Nothing))
            -- in' 1 KR 281 * 0.5
        "dcy" ==> in' 1 KR 282
        "dlt" ==> in' 1 KR 283
    effect "dc01" $ do
        "wet" ==> vsup 1
    router $ do
        "amp" ==> vsup 1.2

t104 :: IO ()
t104 = withSC3 $ runTrack 104 $ do
    offset 8
    source "poly01" $ do
        "freq" ==> do
            vsup $
                fmap midiCPS
                (srand sinf [24,36,48,60,72,84] +
                 srand sinf [0,2,5,7])
        "dur" ==>
            vsup 3
            -- line KR 3 0.3 18 DoNothing
            -- linLin (lfdNoise1 'D' KR (1/4)) (-1) 1 0.25 2
        "pan" ==> lpf (whiteNoise 'w' KR) 15 * 0.5 + 0.5
        "t_tr0" ==> do
            tsup $
                srand sinf
                [ sseq 1 [ 1, sseq 31 [0]]
                , sseq 1 [ 1, sseq 15 [0]
                         , sseq 2 [1, sseq 7 [0]] ]]
            -- tsup $
            --     srand sinf [1,0]

    effect "ap01" $ do
        "wet" ==> linLin (sinOsc KR (1/8) 0) (-1) 1 0 1
        "dcy" ==> linLin (lfSaw KR 4 0) (-1) 1 0 1 `lag` 0.001

    effect "cmb02" $ do
        param "wet" $ vsup $
            sseq sinf [ sseries 32 0 (1/32)
                      , sseries 32 1 (-1/32) ]
        -- "wet" ==> vsup 1
        "dcy" ==> vsup 3
        "dlt" ==> vsup 0.32

    router $ do
        "amp" ==>
            (\tr ->
              envGen KR tr 1 0 rdur DoNothing
              (Envelope [0,0.3] [1] [EnvCub] Nothing Nothing))
            -- line KR 0 0.5 30 DoNothing

t105 :: IO ()
t105 = withSC3 $ runTrack 105 $ do
    offset 8
    source "saw01" $ do
        "lagt" ==>
            (\tr ->
              envGen KR tr 1 0 rdur DoNothing
              (Envelope [0.1,1] [1] [EnvCub] Nothing Nothing))
            -- linLin (lfdNoise3 'l' KR pi) (-1) 1 0 1
            -- vsup 1
        "freq" ==> do
            let a = sseq 3
                    [ sseq 1 [0,0,7,0, 0,0,7,0,7]
                    , srand 8 [-12,0,2,5,7,12] ]
                b = srand 16 [0,2,5,7]
            vsup $
                midiCPS $
                (sstutter 4
                 (sseq sinf [3,4,5] * 12)) +
                (sseq sinf [a, b, a + 12, b, a -12, b])
        "cf" ==> do
            vsup $
                (* 0.1) $
                sseq sinf
                [ sseq 3
                  [1, 5, 1, 5, 8, 1, 5, 8]
                , sshuf 4
                  [srand 8 [2, 4, 5, 8]]]
        "amp" ==> line KR 0 1 30 DoNothing
    effect "ap01" $ do
        "wet" ==> vsup 0.8
        "dcy" ==> linLin (lfSaw KR (1/4) 0) (-1) 1 0 1 `lag` 0.001
    effect "cmb02" $ do
        "wet" ==> vsup 0 -- in' 1 KR 272
        "dcy" ==> in' 1 KR 273
        "dlt" ==> in' 1 KR 274
    effect "dc01" $ do
        "wet" ==> vsup 1
    router $ do
        "amp" ==>
            (\tr ->
              envGen KR tr 1 0 rdur DoNothing
              (Envelope [1.0,0] [1] [EnvCub] Nothing Nothing))

t106 :: IO ()
t106 = withSC3 $ runTrack 106 $ do
    offset 8
    source "pv03" $ do
        "t_tr" ==>
            tsup (srand sinf
                  [ sseq 2 [1,srand 3 [1,0]]
                  , sseq 24 [0]])
        "pan" ==> lpf (whiteNoise 'P' KR) 20 * 0.5 + 0.5
    effect "cmb02" $ do
        "wet" ==> vsup 0 -- in' 1 KR 272
        "dcy" ==> in' 1 KR 273
        "dlt" ==> in' 1 KR 274
    effect "dc01" $ do
        "wet" ==> vsup 1
    router $ do
        "amp" ==>
            (\tr ->
             envGen KR tr 1 0 rdur DoNothing
             (Envelope [1.2,0] [1] [EnvCub] Nothing Nothing))

t99 :: IO ()
t99 = withSC3 $ runTrack 99 $ do
    router $ do
        "amp" ==>
            (\tr ->
              envGen KR tr 1 0 16 DoNothing
              (Envelope [0,1] [1] [EnvCub] Nothing Nothing))

rdur :: Num a => a
rdur = 24


routers :: IO ()
routers = withSC3 $ do
    let fromTo trck s e d = runTrack trck $ do
            offset 8
            router $ do
                "amp" ==>
                    (\tr ->
                      envGen KR tr 1 0 d DoNothing
                      (Envelope [s,e] [1] [EnvCub] Nothing Nothing))
    fromTo 101 0 1 rdur
    fromTo 103 0 1 rdur
    fromTo 105 1 0 rdur
    fromTo 106 1 0 rdur

changes :: IO ()
changes = sequence_ [t101, t102, t103]
