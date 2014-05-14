{-# LANGUAGE TemplateHaskell #-}
{-|

Session with textual UI, take 3. Using TUI02.

-}
module Sound.Study.ForUserInterfaces.Session03 where

import           Sound.OSC (Connection, TCP, openTCP, withTransport)
import           Sound.SC3 hiding (withSC3)
import           Sound.SC3.ID hiding (withSC3)
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

t99 :: IO ()
t99 = withSC3 $ runTrack 99 $ do
    offset 4
    router $ "amp" ==> curveTo EnvCub 4 1.2

t_tr1, t_tr2, t_tr3, t_tr4, t_tr5, t_tr6, t_tr7 :: Trigger Supply
t_tr1  =
    trigger $
    srand sinf
    [sseq 1 [1,0], sseq 1 [1,1]]

t_tr2 =
    trigger $
    srand sinf
    [ sseq 1 [1,0,0,0]
    , sseq 1 [1,0,1,0]
    , sseq 1 [1,0,0,1]
    , sseq 1 [1,1,0,1]
    , sseq 1 [1,1,1,1] ]

t_tr3 =
    trigger $
    srand sinf
    [ sseq 1 [1, 0]
    , sseq 1 [1, 1]
    , sseq 1 [0, 1]]

t_tr4 =
    trigger $
    srand sinf [sseq 1 [1,0], sseq 1 [1,1]]

t_tr5 =
    trigger $
    sseq sinf
    [ sseq 1 [1,1,0,1, 0,1,0,1]
    , sser 16 [0]
    , srand 1
      [ sseq 1 [srand 4 [0,1], sseq 1 [1,1,1,1]]
      , srand 8 [0,1]
      , sseq 8 [1] ]]

t_tr6 =
    trigger $
    sseq sinf
    [ sseq 3 [srand 4 [1,0], sseq 12 [0]]
    , srand 8 [0,1,1] ]

t_tr7 =
    trigger $
    sseq sinf [ sseq 16 [0]
              , srand (srand sinf [2,4,8])
                [sseq 1 [1,0,1,1] ]]

cf_b, cf_h, cf_s :: Supply
cf_b = 0.05
cf_h = 0.96
cf_s = 0.18

cf_p1, cf_p2 :: [Supply]
cf_p1 = [cf_b,cf_h,cf_h,cf_h, cf_s,cf_h,cf_h,cf_h]
cf_p2 = [cf_b,cf_h,cf_s,cf_b, cf_h,cf_s,cf_s,cf_b]

cf_p3, cf_p4 :: Supply
cf_p3 = sseq 1 [ sgeom 4 0.9 0.6
               , sgeom 4 (0.9*(0.6**4)) (recip 0.6) ]
cf_p4 = swhite 8 0 1

cf1, cf2, cf3 :: Sustain Supply
cf1 =
    sustain $
    sseq sinf
    [ sseq 3 [sseq 1 cf_p1, sseq 1 cf_p2]
    , srand 1 [sseq 1 cf_p1, cf_p3], cf_p4 ]

cf2 =
    sustain $
    sseq sinf
    [ sseq 3 [sseq 1 cf_p1, sseq 1 cf_p2]
    , sseq 1 cf_p1, srand 1 [cf_p3, cf_p4] ]

cf3 =
    sustain $
    sseq sinf
    [ sseq 1 cf_p1
    , srand 1 [sseq 1 cf_p2, cf_p4]
    , sseq 1 cf_p1
    , srand 1 [srand 8 cf_p1, cf_p3, cf_p4]]

wet1, wet2, wet3 :: Sustain Supply
wet1 = sustain
       (sseq sinf
        [ sseq 3 [sseq 8 [0]]
        , sseq 1 [srand 4 [0,1], sseq 4 [1]] ])

wet2 = sustain
       (sstutter (srand sinf [2,4,8])
        (srand sinf [1,0]))

wet3 = sustain
       (sseq sinf
        [sstutter 8
         (srand sinf
          [ sseq 1 [0,0,1,0]
          , sseq 1 [0,0,1,1]])])

wet4 :: UGen
wet4 = lfClipNoise 'W' KR 2 * 0.5 + 0.5

wet5 :: CurveTo UGen
wet5 = curveTo EnvLin 32 0

dlt1 :: UGen
dlt1 = linExp (lfdNoise3 'd' KR 1 + 1.1) 0.1 1.1 0.01 0.03

dlt2 :: UGen -> UGen
dlt2 = \tr ->
    let sig = linExp nz 0.1 1.1 (recip 100) (recip 33)
        nz  = lfdNoise3 'd' KR 1 + 1.1
        gt  = toggleFF (coinGate 'g' 0.15 tr)
    in  gate sig gt `lag` 0.001

f101 ::
    (Assignable a1, Assignable a2, Assignable a3, Assignable a4)
    => a1 -> a2 -> a3 -> a4 -> IO ()
f101 t_tr cf wet dlt = withSC3 $ runTrack 101 $ do
    offset 8
    source "nz01" $ do
        "pan"  ==> sustain (sseq sinf [sseq 56 [0.5], swhite 8 0 1])
        "t_tr" ==> t_tr
        "cf"   ==> cf
    effect "ap01" $ do
        "wet" ==> lfClipNoise 'w' KR 4 * 0.5 + 0.5
        "dcy" ==> sustain (sval 0.95)
    effect "cmb02" $ do
        "wet" ==> wet
        "dcy" ==> linExp (lfdNoise0 '0' KR 4 + 1.1) 0.1 1.1 0.1 0.8 `lag` 0.01
        "dlt" ==> dlt
    effect "dc01" $ do
        "wet" ==> sustain (sval 1)
    router $ do
        "amp" ==> curveTo EnvCub 1e-9 1.5

t101_2 :: IO ()
t101_2 = f101 t_tr7 cf2 wet1 dlt1

t101 :: IO ()
t101 = withSC3 $ runTrack 101 $ do
    offset 8
    source "nz01" $ do
        "pan"  ==> do
            sustain $
                -- sstutter (srand sinf [2,4,8]) $
                -- srand sinf [0.5, swhite 1 0.35 0.65]
                sseq sinf
                [ sseq 56 [0.5]
                , swhite 8 0 1 ]
        "t_tr" ==> do
            trigger $
                -- srand sinf
                -- [ sseq 1 [1,0,0,0]
                -- , sseq 1 [1,0,1,0]
                -- , sseq 1 [1,0,0,1]
                -- , sseq 1 [1,1,0,1]
                -- , sseq 1 [1,1,1,1] ]
                -- srand sinf
                -- [ sseq 1 [1, 0]
                -- , sseq 1 [1, 1]
                -- , sseq 1 [0, 1]]
                srand sinf [sseq 1 [1,0], sseq 1 [1,1]]
                -- sseq sinf
                -- [ sseq 1 [1,1,0,1, 0,1,0,1]
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
                --             [sseq 1 [1,0,1,1] ]]
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
            sustain $
                -- sseq sinf
                -- [ sseq 3 [sseq 1 p1, sseq 1 p2]
                -- , srand 1 [sseq 1 p1, p3], p4 ]
                sseq sinf
                [ sseq 3 [sseq 1 p1, sseq 1 p2]
                , sseq 1 p1, srand 1 [p3, p4] ]
                -- sseq sinf
                -- [ sseq 1 p1
                -- , srand 1 [sseq 1 p2, p4]
                -- , sseq 1 p1
                -- , srand 1 [srand 8 p1, p3, p4]]

    effect "ap01" $ do
        "wet" ==> lfClipNoise 'w' KR 4 * 0.5 + 0.5
        "dcy" ==> sustain (sval 0.95)
    effect "cmb02" $ do
        "wet" ==>
            -- sustain
            -- (sseq sinf
            --  [ sseq 3 [sseq 8 [0]]
            --  , sseq 1 [srand 4 [0,1], sseq 4 [1]] ])

            sustain
            (sstutter (srand sinf [2,4,8])
             (srand sinf [1,0]))

            -- sustain
            -- (sseq sinf
            --  [sstutter 8
            --   (srand sinf
            --    [ sseq 1 [0,0,1,0]
            --    , sseq 1 [0,0,1,1]])])

            -- lfClipNoise 'W' KR 2 * 0.5 + 0.5
            -- curveTo EnvLin 32 0
        "dcy" ==>
            linExp (lfdNoise0 '0' KR 4 + 1.1) 0.1 1.1 0.1 0.8 `lag` 0.01
        "dlt" ==>
            -- linExp (lfdNoise3 'd' KR 1 + 1.1) 0.1 1.1 0.01 0.03
            (\tr ->
              let sig = linExp nz 0.1 1.1 (recip 100) (recip 33)
                  nz  = lfdNoise3 'd' KR 1 + 1.1
                  gt  = toggleFF (coinGate 'g' 0.15 tr)
              in  gate sig gt `lag` 0.001)
    effect "dc01" $ do
        "wet" ==> sustain (sval 1)

    router $ do
        "amp" ==> curveTo EnvCub 1e-9 1.5

t102 :: IO ()
t102 = withSC3 $ runTrack 102 $ do
    offset 8

    source "bd03" $ do
        "t_tr0" ==> do
            let r x = swhite 1 0 1 <=* x
            trigger $ sseq sinf [1,r 0.05,r 0.15, r 0.25]
        "amp"   ==>
            sustain
            (sseq sinf
             [ swhite 1 0.9 1.5
             , swhite 1 0.3 0.5
             , swhite 1 0.5 0.8
             , swhite 1 0.5 0.9 ])

    effect "ap01" $ do
        "wet" ==> sustain (sval 0.34)
        "dcy" ==> lfCub KR (1/4) 0 * 0.5 + 0.5
    effect "cmb02" $ do
        -- Manually typed with printing the contents of current node.
        -- control bus numbers are from t102's wet, dcy, and dlt for
        -- "cmb02" synth.
        "wet" ==>
            curveTo EnvLin 16 0
            -- lfClipNoise 'd' KR 1.5 * 0.5 + 0.5
            -- in' 1 KR 270
        "dcy" ==> in' 1 KR 271 * 2.5
        "dlt" ==> in' 1 KR 272

    effect "dc01" $ do
        "wet" ==> sustain (sval 1)

    router $ do
        "amp" ==> sustain (sval 1.2)

t103 :: IO ()
t103 = withSC3 $ runTrack 103 $ do

    offset 8

    source "sin02" $ do
        "pan"  ==> linLin (lpf (whiteNoise 'w' KR) 5) (-1) 1 0 1
        "dur"  ==> sustain (sval 0.3)
        "t_tr" ==> do
            let p1 = sseq 1 [ sseq 3 [1,0,0,1,0,0,1,0]
                            , srand 8 [0,1] ]
            trigger $
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
        "freq" ==> sustain
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

            -- sustain (sval $ midiCPS (lfSaw KR (1/5) 0 * 60 + 12))

    effect "ap01" $ do
        "wet" ==> sustain (sval 1)
        "dcy" ==> linLin (sinOsc KR (1/2) 0) (-1) 1 0.01 1

    effect "cmb02" $ do
        "wet" ==> line KR 0 1 119 DoNothing
        "dcy" ==> linLin (lfSaw KR (1/32) 0) (-1) 1 0.05 6 `lag` 0.1
        "dlt" ==> sustain (sval 0.25) -- sustain 0.8

    router $ do
        "amp" ==> curveTo EnvCub 8 1

t104 :: IO ()
t104 = withSC3 $ runTrack 104 $ do
    offset 8
    source "poly01" $ do
        "freq" ==> do
            sustain $
                fmap midiCPS
                (srand sinf [24,36,48,60,72,84] +
                 srand sinf [0,2,5,7])
        "dur" ==> sustain (sval 3)
        "pan" ==> lpf (whiteNoise 'w' KR) 15 * 0.5 + 0.5
        "t_tr0" ==> do
            trigger $
                srand sinf
                [ sseq 1 [ 1, sseq 31 [0]]
                , sseq 1 [ 1, sseq 15 [0]
                         , sseq 2 [1, sseq 7 [0]] ]]
    effect "ap01" $ do
        "wet" ==> linLin (sinOsc KR (1/8) 0) (-1) 1 0 1
        "dcy" ==> linLin (lfSaw KR 4 0) (-1) 1 0 1 `lag` 0.001
    effect "cmb02" $ do
        "wet" ==>
            sustain
            (sseq sinf [ sseries 32 0 (1/32)
                       , sseries 32 1 (-1/32) ])
        "dcy" ==> sustain (sval 3)
        "dlt" ==> sustain (sval 0.32)

    router $ do
        "amp" ==> curveTo EnvLin 16 0.3

t105 :: IO ()
t105 = withSC3 $ runTrack 105 $ do
    offset 8
    source "saw01" $ do
        "lagt" ==> linLin (lfdNoise3 'L' KR (1/3) + 2) 1 3 0.96 1
        "freq" ==> do
            let a = sseq 3
                    [ sseq 1 [0,0,7,0, 0,0,7,0,7]
                    , srand 8 [-12,0,2,5,7,12] ]
                b = srand 16 [0,2,5,7]
            sustain $
                midiCPS $
                (sstutter 4
                 (sseq sinf [3,4,5] * 12)) +
                (sseq sinf [a, b, a + 12, b, a -12, b])
        "cf" ==> do
            sustain $
                (* 0.1) $
                sseq sinf
                [ sseq 3
                  [1, 5, 1, 5, 8, 1, 5, 8]
                , sshuf 4
                  [srand 8 [2, 4, 5, 8]]]
        "amp" ==> line KR 0 1 30 DoNothing
    effect "ap01" $ do
        "wet" ==> sustain (sval 0.8)
        "dcy" ==> linLin (lfSaw KR (1/16) 0) (-1) 1 0 1 `lag` 0.001
    effect "cmb02" $ do
        "wet" ==> (lfClipNoise 'D' KR 2 * 0.5 + 0.5) `lag` 0.01
        "dcy" ==> linLin (lfdNoise0 'C' KR 4 + 2) 1 3 0.1 1
        "dlt" ==> linExp (lfdNoise3 'C' KR (1/3)+2) 1 3 (recip 100) (recip 30)
    effect "dc01" $ do
        "wet" ==> sustain (sval 1)
    router $ do
        "amp" ==> curveTo EnvCub 8 0.8

t106 :: IO ()
t106 = withSC3 $ runTrack 106 $ do
    offset 8
    source "pv03" $ do
        "t_tr" ==>
            trigger
            (srand sinf
             [ sseq 2 [1,srand 3 [1,0]]
             , sseq 24 [0]])
        "pan" ==> linLin (lfdNoise3 'P' KR 13) (-1) 1 0 1
    effect "cmb02" $ do
        "wet" ==> lfdClipNoise 'D' KR 4 * 0.5 + 0.5
        "dcy" ==> in' 1 KR 277
        "dlt" ==> in' 1 KR 276
    effect "dc01" $ do
        "wet" ==> sustain (sval 1)
    router $ do
        "amp" ==> curveTo EnvCub 16 0.8

rdur :: Num a => a
rdur = 32

routers :: IO ()
routers = withSC3 $ do
    let fromTo trck e = runTrack trck $ do
            offset 8
            router ("amp" ==> curveTo EnvCub e dur)
        dur = 32
    -- XXX: runTrack will take diff, 'fromTo' will remove all nodes except for
    -- 'router'.
    fromTo 101 0
    fromTo 103 0
    fromTo 105 1
    fromTo 106 1

changes :: IO ()
changes = sequence_ [t101, t102, t103]

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)
