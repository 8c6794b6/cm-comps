{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|

Session with textual UI, take 3. Using TUI02.

-}
module Session.Session03 where

import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
import Sound.SC3.Supply
import Sound.SC3.Tree

import Session.Synthdefs (synthdefs)
import Sound.Study.ForUserInterfaces.TUI.TUI02

-- --------------------------------------------------------------------------
--
-- * Controls
--
-- --------------------------------------------------------------------------

sendSynthdefs :: IO Message
sendSynthdefs =
    withSC3 . async . foldr1 withCM $ map d_recv $ synthdefs

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
t101_2 = f101 t_tr1 cf2 wet1 dlt1

t101 :: IO ()
t101 = withSC3 $ runTrack 101 $ do
    offset 8
    source "nz01" $ do
        "pan"  ==> do
            sustain $
                -- sstutter (srand sinf [2,4,8]) $
                -- srand sinf [0.5, swhite 1 0.35 0.65]
                sseq sinf
                [ sseq 24 [0.5]
                , swhite 8 0 1 ]
        "t_tr" ==> trigger
            (
                -- srand sinf
                -- [ sseq 1 [1,0,0,0]
                -- , sseq 1 [1,0,1,0]
                -- , sseq 1 [1,0,0,1]
                -- , sseq 1 [1,1,0,1]
                -- , sseq 1 [1,1,1,1] ]

                -- sstutter (sseq sinf [1])
                -- (sxrand sinf
                --  [ sseq 1 [1,1,0,1, 1,0,1,1]
                --  , sseq 1 [1,0,0,1, 0,0,1,0]
                --  , sseq 2 [0,1,0,1]
                --  , sseq 1 [0,1,0,1, 0,0,1,0]
                --  , sseq 1 [0,1,1,0, 1,1,0,1]
                --  , sseq 1 [1,srand 7 [1,1,1,1,1,0]] ])

                -- srand sinf
                -- [ sseq 1 [1, 0]
                -- , sseq 1 [1, 1]
                -- , sseq 1 [0, 1]]

                -- srand sinf [sseq 1 [1,0], sseq 1 [1,1]]

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

                srand sinf [ sseq 8 [0]
                           , srand (srand sinf [1,2,4])
                             [sseq 1 [1,0,1,1] ]]

                -- sseq sinf
                -- [ sseq (siwhite sinf 1 4)
                --   [ 0, 0, 0, 0, srand 4 [0,1]]
                -- , srand 16 [0,1]
                -- , sseq 16 [0] ]
             )
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
                -- sseq sinf [ sseq 1 p1, sseq 1 p2 ]
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

            -- sustain
            -- (sstutter 2
            --  (sseq sinf
            --   [ sseq 28 [0]
            --   , srand 4 [0,1]])
            -- )

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
        -- "amp" ==> curveTo EnvCub 1e-9 0
        -- "amp" ==> curveTo EnvCub 14 2.3
        -- "amp" ==> curveTo EnvCub 14 0
        -- "amp" ==> curveTo EnvCub 8 1.8
        "amp" ==> curveTo EnvCub 1e-9 1.2

t102 :: IO ()
t102 = withSC3 $ runTrack 102 $ do
    offset 8
    source "bd03" $ do
        let r x = swhite 1 0 1 <=* x
        "t_tr0" ==>
            -- trigger (sseq sinf [1,r 0.05,r 0.15, r 0.25])
            -- trigger (sseq sinf [r 0.975,r 0.05,r 0.15, r 0.25])
            trigger
            (sseq sinf
             [1,      r 0.05, r 0.15, r 0.25
             ,r 0.08, r 0.12, r 0.22, r 0.23 ])
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
        "wet" ==>
            curveTo EnvLin 16 0
            -- (lfdNoise3 'W' KR (3/5) * 0.5 + 0.5) ** 4
            -- (lfClipNoise 'd' KR (3/5) * 0.5 + 0.5) `lag` 0.05
            -- in' 1 KR 261
            -- input 101 (synthName ==? "cmb02") (paramName ==? "wet") seq
        "dcy" ==>
            input 101 (synthName ==? "cmb02") (paramName ==? "dcy") seq
        "dlt" ==>
            input 101 (synthName ==? "cmb02") (paramName ==? "dlt") seq
            -- in' 1 KR 263 * 1.25

    -- effect "cmb03" $ do
    --     "wet" ==>
    --         -- curveTo EnvLin 32 0
    --         sustain
    --         (sstutter 4
    --          (sseq sinf
    --           (li
    --            -- XXX: context reduction size has limitation.
    --            -- Use quosi-quote?
    --            0 0 1 0  0 0 1 0  0 0 1 0  0 0 1 0
    --            0 0 1 0  0 0 1 0  0 1 0 1  0 1 0 1
    --            0 1 0 1  0 1 0 1  0 0 1 0  1 0 1 0
    --            1 0 0 0  1 0 0 0  1 0 0 1  0 1 0 1
    --           )))
    --         -- (sstutter
    --         --  (siwhite sinf 1 3 * 4)
    --         --  (srand sinf (li 0 0 0 0 1)))
    --     "dcy" ==>
    --         let df = tExpRand 'F' 1.7 5 (dust 'T' KR (1/4))
    --         in  linExp (lfdNoise0 'd' KR 0.7 + 2) 1 3 1.6 2

    --     -- "dlt" ==> \tr ->
    --     --     let tr' = coinGate 'T' (5/8) tr
    --     --     in  (60/120) / (tIRand 'a' 3 16 tr')

        param "dlt" $
            let r = sstutter 2
                    (sseq sinf
                     (li
                      8 16 15 (srand 2 (li 1 2 3 4) * 2) 14 2 13)
                    )
            in  sustain ((60/120) * recip r)

    effect' 1 "ap02" $ do
        "wet" ==> curveTo EnvLin 64 0
        "dcy" ==> sustain (sval 4) -- lfCub KR (1/4) 0 * 0.5 + 0.5

    effect "dc01" $ do
        "wet" ==> sustain (sval 1)
    router $ do
        -- "amp" ==> sustain (sval 1.2)
        -- "amp" ==> curveTo EnvCub 8 1.2
        -- "amp" ==> curveTo EnvCub 32 1.8
        "amp" ==> curveTo EnvCub 8 1.8
        -- "amp" ==> curveTo EnvCub 32 0

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
        "dlt" ==> sustain (sval 0.25)

    router $ do
        "amp" ==> curveTo EnvCub 4 1
        -- "amp" ==> curveTo EnvCub 32 0

t104 :: IO ()
t104 = withSC3 $ runTrack 104 $ do
    offset 8
    -- offset 32
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
        "amp" ==> curveTo EnvLin 32 0.50
        -- "amp" ==> curveTo EnvLin 32 0
        -- "amp" ==> curveTo EnvLin 1e-9 0.3

t105 :: IO ()
t105 = withSC3 $ runTrack 105 $ do
    offset 8
    source "saw01" $ do
        -- "lagt" ==> linLin (lfdNoise3 'L' KR (1/3) + 2) 1 3 0.89 1
        "lagt" ==> curveTo EnvSin 18 1
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
            -- sustain
            --     (srand sinf [0,2,5,7] +
            --      srand sinf [3,4,5] * 12)
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
        "amp" ==> curveTo EnvCub 24 0
        -- "amp" ==> curveTo EnvCub 2 0.8
        -- "amp" ==> curveTo EnvCub 24 0.6

t107 :: IO ()
t107 = withSC3 $ runTrack 107 $ do
    offset 8
    source "pv03" $ do
        "t_tr" ==>
            trigger
            (srand sinf
             [ sseq 2 [1,srand 3 [1,0]]
             , sseq 24 [0]])
        "pan" ==> linLin (lfdNoise3 'P' KR 5) (-1) 1 0 1
    effect "cmb02" $ do
        "wet" ==> (lfdClipNoise 'D' KR (1/4) * 0.5 + 0.5) `lag` 0.25
        "dcy" ==> in' 1 KR 262
        "dlt" ==> in' 1 KR 263
    effect "ap01" $ do
        "wet" ==> curveTo EnvLin 16 0.5
        "dcy" ==> curveTo EnvLin 16 6
    effect "lpf01" $ do
        "wet" ==> curveTo EnvLin 16 1
        "cf"  ==> linExp (lfdNoise3 'D' KR (1/3.28) + 2) 1 3 200 12000
        "rq"  ==> curveTo EnvLin 16 0.6
    effect "cmb02" $ do
        "wet" ==> curveTo EnvLin 16 1
        "dcy" ==> curveTo EnvLin 16 8
        "dlt" ==> curveTo EnvLin 2 0.23289
    effect "dc01" $ do
        "wet" ==> sustain (sval 1)
    router $ do
        "amp" ==> curveTo EnvCub 32 1

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
changes = t101 >> t103 >> t105

-- | @scsynth@ running with TCP, 127.0.0.1:57111.
withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

-- | Default scsynth, UDP, 127.0.0.1:57110,
-- withSC3 :: Connection UDP a -> IO a
-- withSC3 = withTransport (openUDP "127.0.0.1" 57110)


class BuildList a r | r -> a where
    build' :: [a] -> a -> r

instance BuildList a [a] where
    build' l x = reverse $ x : l

instance BuildList a r => BuildList a (a->r) where
    build' l x = \y -> build' (x:l) y

li :: BuildList a r => a -> r
li x = build' [] x
