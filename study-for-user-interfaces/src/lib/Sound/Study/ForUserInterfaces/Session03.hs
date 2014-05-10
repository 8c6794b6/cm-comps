{-# LANGUAGE TemplateHaskell #-}
{-|

Session with textual UI, take 3. Using TUI02.

-}
module Sound.Study.ForUserInterfaces.Session03 where

import           Sound.OSC
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
    mapM_ (async . d_recv)
        (synthdefs ++ Session01.synthdefs ++ Session02.synthdefs)
    initializeTUI02

init_t101 :: IO ()
init_t101 = withSC3 $ track 101 $ do
    addSource "sin02"
    addFx "cmb02"
    addFx "ap01"

dump_t101 :: IO ()
dump_t101 = withSC3 $ track 101 dumpTrack

t101 :: IO ()
t101 = withSC3 $ track 101 $ do

    offset 8

    source ".sin02" $ do
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
                fmap midiCPS $
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

    source ".ap01" $ do
        "wet" ==> vsup 1
        "dcy" ==> linLin (sinOsc KR (1/2) 0) (-1) 1 0.01 1
        -- "dcy" ==> vsup 0.8
    source ".cmb02" $ do
        "wet" ==> line KR 0 1 119 DoNothing
        "dcy" ==> linLin (lfSaw KR (1/32) 0) (-1) 1 0.05 6 `lag` 0.1
        "dlt" ==> vsup 0.8

    source ".router" $ do
        "amp" ==> line KR 0 1 20 DoNothing

init_t102 :: IO ()
init_t102 = withSC3 $ track 102 $ do
    addSource "nz01"
    addFx "cmb02"
    addFx "dc01"
    addFx "ap01"

dump_t102 :: IO ()
dump_t102 = withSC3 (track 102 dumpTrack)

t102 :: IO ()
t102 = withSC3 $ track 102 $ do
    offset 8
    source ".nz01" $ do
        "pan"  ==> do
            vsup $
                sstutter (srand sinf [2,4,8,16]) $
                srand sinf [0.5, swhite 1 0.25 0.75]
        "t_tr" ==> do
            tsup $
                -- srand sinf
                -- [ sseq 1 [1,0,0,0]
                -- , sseq 1 [1,0,1,0]
                -- , sseq 1 [1,0,0,1]
                -- , sseq 1 [1,1,0,1]
                -- , sseq 1 [1,1,1,1] ]
                srand sinf
                [ sseq 1 [1, 0]
                , sseq 1 [1, 1]
                , sseq 1 [0, 1]]
                -- sseq sinf
                -- [ sser 8 [0]
                -- , srand 4 [0,1], sseq 1 [1,1,1,1] ]
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
                -- sseq sinf [sseq 1 p1, sseq 1 p2]
                -- sseq sinf p1
                sseq sinf
                [ sseq 1 p1
                , srand 1 [sseq 1 p2, p4]
                , sseq 1 p1
                , srand 1 [srand 8 p1, p3, p4]]

    source ".ap01" $ do
        "wet" ==>
            lfClipNoise 'w' KR 4 * 0.5 + 0.5
            -- line KR 0 1 33 DoNothing
        "dcy" ==> vsup 0.83

    source ".cmb02" $ do
        "wet" ==>
            -- vsup
            -- (sstutter (srand sinf [2,4,8])
            --  (srand sinf [1,0]))
            lfClipNoise 'W' KR 2 * 0.5 + 0.5
        "dcy" ==>
            linExp (lfdNoise0 '0' KR 4 + 1.1) 0.1 1.1 0.1 0.8 `lag` 0.01
        "dlt" ==>
            -- linExp (lfdNoise3 'd' KR 1 + 1.1) 0.1 1.1 0.01 0.03
            (\tr ->
              let sig = linExp nz 0.1 1.1 (recip 100) (recip 33)
                  nz  = lfdNoise3 'd' KR 4 + 1.1
              in  gate sig (coinGate 'g' 0.35 tr) `lag` 0.01)

    source ".dc01" $ do
        "wet" ==> vsup 1

    source ".router" $ do
        "amp" ==> vsup 1.25

initT103 :: IO ()
initT103 = withSC3 $ track 103 $ do
    addSource "poly01"
    addFx "ap01"
    addFx "cmb02"

dumpT103 :: IO ()
dumpT103 = withSC3 $ track 103 dumpTrack

t103 :: IO ()
t103 = withSC3 $ track 103 $ do
    offset 8
    source ".poly01" $ do
        "freq" ==> do
            vsup $
                fmap midiCPS
                (srand sinf [24,36,48,60,72,84] +
                 srand sinf [0,2,5,7,9])
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

    source ".ap01" $ do
        "wet" ==> linLin (sinOsc KR (1/8) 0) (-1) 1 0 1
        "dcy" ==> linLin (lfSaw KR 4 0) (-1) 1 0 1 `lag` 0.001

    source ".cmb02" $ do
        param "wet" $ vsup $
            sseq sinf [ sseries 32 0 (1/32)
                      , sseries 32 1 (-1/32) ]
        -- "wet" ==> vsup 1
        "dcy" ==> vsup 3
        "dlt" ==> vsup 0.32

    source ".router" $ do
        "amp" ==> line KR 0.8 0.2 19 DoNothing

param :: (Assignable a, Transport m) => String -> a -> Track m ()
param a b = a ==> b

initT104 :: IO ()
initT104 = withSC3 $ track 104 $ do
    addSource "bd03"
    addFx "ap01"
    addFx "cmb02"
    addFx "dc01"

dumpT104 :: IO ()
dumpT104 = withSC3 $ track 104 dumpTrack

t104 :: IO ()
t104 = withSC3 $ track 104 $ do
    offset 8

    source ".bd03" $ do
        "t_tr0" ==> do
            let p x = swhite 1 0 1 <=* x
            tsup $ sseq sinf [1,p 0.05,p 0.15, p 0.25]
        "amp"   ==> vsup 1

    source ".ap01" $ do
        "wet" ==> vsup 1
        "dcy" ==> vsup 0.98
    source ".cmb02" $ do
        -- Manually typed with printing the contents of current node.
        -- control bus numbers are from t102's wet, dcy, and dlt for
        -- "cmb02" synth.
        "wet" ==> (1 - in' 1 KR 285) * 0.5
        "dcy" ==> in' 1 KR 286
        "dlt" ==> in' 1 KR 287
    source ".dc01" $ do
        "wet" ==> vsup 1
    source ".router" $ do
        "amp" ==> vsup 0.8

initT105 :: IO ()
initT105 = withSC3 $ track 105 $ do
    addSource "saw01"
    addFx  "ap01"
    addFx "dc01"

dump105 :: IO ()
dump105 = withSC3 $ track 105 dumpTrack

t105 :: IO ()
t105 = withSC3 $ track 105 $ do
    offset 8
    source ".saw01" $ do
        "lagt" ==> line KR 0 1 30 DoNothing
        "freq" ==> do
            let a = sseq 3
                    [ sseq 1 [0,0,7,0,0,7,0,7]
                    , srand 8 [-12,0,2,5,7,12] ]
                b = srand 16 [-12,-12,0,12,24]
            vsup $
                fmap midiCPS $ (+48) $
                sseq sinf [a, b, a + 12, b, a -12, b]
        "cf" ==> do
            vsup $
                (* 0.1) $
                sseq sinf
                [ sseq 3
                  [1, 5, 1, 5, 8, 1, 5, 8]
                , srand 8
                  [2, 4, 5, 8] ]
        "amp" ==> line KR 0 1 30 DoNothing
    source ".ap01" $ do
        "wet" ==> vsup 0.8
        "dcy" ==> linLin (lfSaw KR (1/4) 0) (-1) 1 0 1 `lag` 0.001
    source ".dc01" $ do
        "wet" ==> vsup 1
    source ".router" $ do
        "amp" ==> line KR 1 0 30 DoNothing

t99 :: IO ()
t99 = withSC3 $ track 99 $ do
    source ".router" ("amp" ==> line KR 0 1 29 DoNothing)

changes :: IO ()
changes = sequence_ [t101, t102, t103, t104, t105]
