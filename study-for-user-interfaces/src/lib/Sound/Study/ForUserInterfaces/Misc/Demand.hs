{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Scratch written while reading demand related sc3 help files.

-}
module Sound.Study.ForUserInterfaces.Misc.Demand where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.Random

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Supply

-- --------------------------------------------------------------------------
--
-- * Dbrown
--
-- --------------------------------------------------------------------------

-- | Example of 'dbrown', take 1.
dbrown_ex01 :: IO ()
dbrown_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = demand tr 0 a * 30 + 340
    tr   = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a    = dbrown 'b' dinf lo hi step
    lo   = 0
    hi   = 15
    step = 1

-- | Example of 'dbrown', take 2.
dbrown_ex02 :: IO ()
dbrown_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = combC (sinOsc AR freq 0) 0.3 0.3 8 * aenv * 0.1
    aenv = decay tr (recip rate * 0.8)
    freq = midiCPS (demand tr 0 a)
    tr   = impulse KR rate 0
    rate = mouseX KR 1 40 Exponential 0.1
    a    = dbrown 'a' dinf lo hi step
    lo   = 24
    hi   = 120
    step = 12

-- --------------------------------------------------------------------------
--
-- * Dbufrd
--
-- --------------------------------------------------------------------------

-- | Example of 'dbufrd', take 1.
dbufrd_ex01 :: IO ()
dbufrd_ex01 = withSC3 $ do
    let bufn :: Num a => a
        bufn = 1
        bufsz :: Num a => a
        bufsz = 24

        osig = sinOsc AR freq 0 * 0.1
        freq = demand tr 0 (dbufrd 'r' bufn idx Loop)
        idx  = dseq 'a' dinf
               (mce [dseq 'b' 3 (mce [0,3,5,0,3,7,0,5,9])
                    ,dbrown 'c' 5 0 23 1])
        tr   = dust 'd' KR 10
    _ <- async $ b_alloc bufn bufsz 1
    vals <- liftIO ((map exp . randomRs (log 20,log 5000)) `fmap` newStdGen)
    send $ b_set bufn $ zip [0..23] vals
    play $ out 0 $ mce2 osig osig

-- | Example of 'dbufrd', take 2.
-- Assuming 'dbufrd_ex01' has invoked once already.
dbufrd_ex02 :: IO ()
dbufrd_ex02 = withSC3 $ do
    let bufn :: Num a => a
        bufn = 2
        bufsz :: Num a => a
        bufsz = 24
        osig = sinOsc AR freq 0 * 0.1
        freq = duty KR ddur 0 DoNothing dval
        ddur = dbufrd 'a' bufn idxd Loop * 0.5
        idxd = dseries 'b' dinf 0 1
        dval = dbufrd 'b' 1 idxv Loop
        idxv = dseq 'c' dinf
               (mce [dseq 'd' 3 (mce [0,3,5,0,3,7,0,5,9])
                    ,dbrown 'e' 5 0 23 1])
    _ <- async $ b_alloc bufn bufsz 1
    vals <- liftIO ((map (\x -> [0.25,0.5,1] !! x) . randomRs (0,2))
                    `fmap` newStdGen)
    send $ b_set bufn $ zip [0..23] vals
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * Dbufwr
--
-- --------------------------------------------------------------------------

-- | Example of 'dbufwr', take 1.
dbufwr_ex01 :: IO ()
dbufwr_ex01 = withSC3 $ do
    let bufn      :: Num a => a
        bufn      = 1
        bufsz     :: Num a => a
        bufsz     = 24

        osig      = sinOsc AR freq 0 * 0.1
        freq      = demand readTrig 0 dread `lag` 0.01 * mce [1,1.01]
        readTrig  = impulse KR 16 0
        writeTrig = dust 'a' KR 1
        readPos   = dseries 'b' dinf 0 1
        writePos  = dseq 'c' dinf
                    (mce [ dseries 'd' 30 0 3
                         , dseries 'e' 30 0 1 ])
        dread     = dbufrd 'f' bufn readPos Loop
        dwrite    = dbufwr 'g' bufn writePos
                    (dseq 'h' dinf
                     (mce [ dseries 'i' 16 1 1
                          , dwhite 'j' 8 1 16]) * 60) Loop
        osig'     = mrg [out 0 osig, demand writeTrig 0 dwrite]
    _ <- async $ b_alloc bufn bufsz 1
    send $ b_zero bufn
    play osig'

-- | Example of 'dbufwr', take 2.
dbufwr_ex02 :: IO ()
dbufwr_ex02 = withSC3 $ do
    let bufn  :: Num a => a
        bufn  = 1
        osig  = sinOsc AR freq 0 * 0.1
        freq  = duty KR (dseq 'a' dinf (mce [0.5,0.75,0.5,1]) * 0.2)
                0 DoNothing (dbufrd 'b' bufn (dseries 'c' dinf 0 1) Loop)
        val   = mouseY KR 1000 200 Exponential 0.1
        pos   = mouseX KR 0 23 Linear 0.1
        write = mouseButton KR 0 1 0.1
        dwr   = demand write 0 (dbufwr 'd' bufn pos val Loop)
        osig' = mrg [out 0 osig, dwr]
    play osig'

-- | Example of 'dbufwr', take 3.
dbufwr_ex03 :: IO ()
dbufwr_ex03 = withSC3 $ do
    let bufn :: Num a => a
        bufn = 2
        bufsz :: Num a => a
        bufsz = 2
        osig = demand wtrig 0 write
        wtrig = impulse KR 2 0
        write = dbufwr 'a' bufn wpos dread Loop
        dread = dbufrd 'b' bufn rpos Loop
        rpos  = dbufrd 'c' bufn 1 Loop
        wpos  = dbufrd 'd' bufn 0 Loop
    _ <- async $ b_alloc bufn bufsz 1
    send $ b_setn bufn [(0,[1,0])]
    play osig


-- --------------------------------------------------------------------------
--
-- * Demand
--
-- --------------------------------------------------------------------------

-- | Function to make demand' example, take 1.
f_demand_01 ::
    UGen -- ^ Frequency of trigger
    -> IO ()
f_demand_01 trf = audition $ out 0 osig
  where
    osig = cubed (cubed (sinOsc AR (freq + mce [0,0.7]) 0)) * 0.1
    freq = demand tr 0 (s*100)
    tr   = impulse KR trf 0
    s    = drand 'a' 2000
           (mce [ dseq 'b' 1 (mce [1,2,3,4,5,5,4,3,2,1])
                , drand 'c' 8 (mce [4,5,6,7,8,9,10]) ])

-- | Example of 'demand', take 1.
demand_ex01 :: IO ()
demand_ex01 = f_demand_01 24

-- | Example of 'demand', take 2.
demand_ex02 :: IO ()
demand_ex02 = f_demand_01 12

-- | Function to make 'demand' example, take 2.
f_demand_02 :: UGen -> IO ()
f_demand_02 dmnd = audition $ out 0 osig
  where
    osig = cubed (cubed (sinOsc AR (freq + mce [0,0.7]) 0)) * 0.1
    freq = demand tr 0 dmnd
    tr   = impulse KR 10 0

-- | Example of 'demand', take 3.
demand_ex03 :: IO ()
demand_ex03 = f_demand_02 (midiCPS $ diwhite 'a' dinf 60 72)

-- | Example of 'demand', take 4.
demand_ex04 :: IO ()
demand_ex04 = f_demand_02 d
  where
    d = midiCPS $ dseq 'a' dinf
        (mce [72, 75, 79, drand 'b' 1 (mce [82,84,86])])

-- | Example of 'demand', take 5.
demand_ex05 :: IO ()
demand_ex05 = f_demand_02 d
  where
    d = midiCPS $ dswitch1 'a' (lfPulse KR 0.2 0 0.5)
        (mce [ diwhite 'b' dinf 60 72
             , dseq 'c' dinf
               (mce [72,75,79, drand 'd' 1 (mce [82,84,86])]) ])

-- | Example of 'demand', take 6.
demand_ex06 :: IO ()
demand_ex06 = f_demand_02 d
  where
    d = mce [d0,d1]
    d0 = midiCPS $ drand 'a' dinf (mce [72,75,79,82] - 12)
    d1 = midiCPS $ dseq 'b' dinf
         (mce [72,75,79,drand 'c' 1 (mce [82,84,86])])

-- | Example of 'demand', take 7.
demand_ex07 :: IO ()
demand_ex07 = audition $ out 0 $ mce2 osig osig
  where
    osig = lpf (pinkNoise 'P' AR) 5000 * decay tr 0.5
    tr   = demand tr0 0 (d*0.4) * tr0
    tr0  = impulse KR 8 0
    d    = evalSupply0 s
    s    = srand sinf
           [ sseq 1 [4,0,0,1,2,1,0,1]
           , sseq 1 [4,0,2,0,1,0,1,1]
           , sseq 1 [4,0,0,2,0,0,1,1]
           , sseq 1 [4,0,1,2,0,1,2,0]
           , sseq 1 [4,1,1,1,2,2,3,3]
           , sseq 1 [4,1,0,1,0,1,0,1]
           ]


-- --------------------------------------------------------------------------
--
-- * DemandEnvGen
--
-- --------------------------------------------------------------------------

-- | Example of 'demandEnvGen', take 1.
demandEnvGen_ex01 :: IO ()
demandEnvGen_ex01 = audition $ out 0 osig
  where
    osig  = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1
    freq  = demandEnvGen AR lvl dur crv shp gt rst lscl lbias tscl dact
    lvl   = dseq 'a' dinf (mce [204,400,201,502,300,200])
    dur   = drand 'b' dinf (mce [1.01,0.2,0.1,2]) *
            mouseY KR 0.01 3 Exponential 0.1
    crv   = env_curve_shape EnvCub
    shp   = 0
    gt    = 1
    rst   = 1
    lscl  = 1
    lbias = 0
    tscl  = 1
    dact  = DoNothing

-- | Example of 'demandEnvGen', take 2, frequency modulation.
demandEnvGen_ex02 :: IO ()
demandEnvGen_ex02 = do
    let vals =
            fmap (map exp . take 32 . randomRs (log 200, log 1000)) newStdGen
    val1 <- vals
    val2 <- vals

    let osig = sinOsc AR freq 0 * 0.1
        freq = demandEnvGen AR lvl dur crv shp gt rst lscl lbias tscl dact
        lvl  = dseq 'a' dinf (mce [mce val1, mce val2])
        dur  = sampleDur * mouseY KR 1 3000 Exponential 0.1
        crv  = 5
        shp  = mouseX KR (-0.01) (-4) Linear 0.1
        gt   = 1
        rst  = 0
        lscl = 1
        lbias = 0
        tscl = 1
        dact = DoNothing
    audition $ out 0 osig

-- | Example of 'demandEnvGen', take 3.
demandEnvGen_ex03 :: IO ()
demandEnvGen_ex03 = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.21]) 0 * 0.1
    freq = demandEnvGen AR lvls durs crv shp gt rst lscl lbias tscl dact
    lvls = diwhite 'a' dinf 3 10 * 100
    durs = 0.1
    crv  = env_curve_shape (EnvNum undefined)
    shp  = 0.3
    -- gt   = mouseX KR 0 1 Linear 0.1 >* 0.5
    gt   = lfdNoise3 'a' KR 1.5 >* 0
    rst  = 0
    lscl = 1
    lbias = 0
    tscl  = 1
    dact  = DoNothing

-- | Example of 'demandEnvGen', take 4.
demandEnvGen_ex04 :: IO ()
demandEnvGen_ex04 = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.001]) 0 * 0.1
    freq = demandEnvGen KR lvls durs crv shp gt rst lscl lbs tscl dact
    lvls = evalSupply0
           (sseq 2 [sseries 5 400 200, 500, 800, 530, 4000, 900])
    durs = 0.1
    crv  = 3
    shp  = 0
    gt   = mouseX KR 0 1 Linear 0.1 >* 0.5
    rst  = mouseButton KR 0 1 0.1 >* 0.5
    lscl = 1
    lbs  = 0
    tscl = 1
    dact = DoNothing

-- | For short sequence 'demandEnvGen' example with different curves.
f_demand_03 :: UGen -> IO ()
f_demand_03 crv = audition $ out 0 osig
  where
    osig = sinOsc AR (freq*mce[1,1.01]) 0 * 0.1
    freq = demandEnvGen KR lvls 0.2 crv 0 1 0 1 0 1 RemoveSynth
    lvls = evalSupply (sseq 1 [1300,500,800,300,400]) (mkStdGen 0)

-- | Example of 'demandEnvGen', take 5.
demandEnvGen_ex05 :: IO ()
demandEnvGen_ex05 = f_demand_03 (env_curve_shape EnvLin)

-- | Example of 'demandEnvGen', take 6
demandEnvGen_ex06 :: IO ()
demandEnvGen_ex06 = f_demand_03 (env_curve_shape EnvStep)

-- | To play sinOsc with given frequency.
f_demand_04 :: UGen -> IO ()
f_demand_04 freq = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1

-- | Example of 'demandEnvGen', take 7.
demandEnvGen_ex07 :: IO ()
demandEnvGen_ex07 = f_demand_04 d
  where
    d = demandEnvGen KR lvls 1 crv 0 1 0 1 0 1 DoNothing
    lvls = evalSupply0 (sseq 1 [300,800])
    crv  = env_curve_shape EnvLin

-- | Example of 'demandEnvGen', take 8.
demandEnvGen_ex08 :: IO ()
demandEnvGen_ex08 = f_demand_04 d
  where
    d    = demandEnvGen KR lvls 0.03 crv 0 gt rst 1 0 1 DoNothing
    lvls = evalSupply0 (sseq sinf [500,800,600])
    crv  = env_curve_shape EnvLin
    gt   = toggleFF (dust 'a' KR 5)
    rst  = 0

-- | Example of 'demandEnvGen', take 9, lfnoise1.
demandEnvGen_ex09 :: IO ()
demandEnvGen_ex09 = audition $ out 0 $ osig
  where
    osig = demandEnvGen AR lvls dur crv shp 1 0 1 0 1 DoNothing
    lvls = dwhite 'a' dinf (-0.1) 0.1
    dur  = sampleDur * mouseY KR 0.5 20 Linear 0.1
    crv  = env_curve_shape (EnvNum undefined)
    shp  = (-4)

-- | Example of 'demandEnvGen', take 10, lfbrownnoise.
demandEnvGen_ex10 :: IO ()
demandEnvGen_ex10 = audition $ out 0 $ osig
  where
    osig = demandEnvGen AR lvls dur 1 0 1 0 1 0 1 DoNothing
    lvls = dbrown 'a' dinf (-0.1) 0.1 0.1
    dur  = sampleDur * mouseY KR 0.5 20 Linear 0.1

-- | Syncing saw with using given ugen as reset for 'demandEnvGen'.
f_demand_05 :: UGen -> IO ()
f_demand_05 rst = audition $ out 0 $ osig
  where
    osig = demandEnvGen AR lvls dur 1 0 gt rst 1 0 1 DoNothing
    lvls = evalSupply0 (sseq sinf [sseries 20 (-0.1) 0.01])
    dur  = sampleDur * mouseY KR 1 100 Exponential 0.1
    gt   = k2A 1

-- | Example of 'demandEnvGen', take 11, hardsyncing a saw.
demandEnvGen_ex11 :: IO ()
demandEnvGen_ex11 = f_demand_05 rst
  where
    rst  = impulse AR rate 0
    rate = mouseX KR 1 rmax Exponential 0.1 * 1.5
    rmax = sampleRate * mouseX KR 0.002 1 Exponential 0.1

-- | Example of 'demandEnvGen', take 12, softsyncing a saw
demandEnvGen_ex12 :: IO ()
demandEnvGen_ex12 = f_demand_05 rst
  where
    rst  = impulse AR rate 0 + mce [0, 0.3]
    rate = mouseX KR 1 rmax Exponential 0.1
    rmax = sampleRate * mouseX KR 0.002 1 Exponential 0.1

-- | Example of 'demandEnvGen', take 13, hardsyncing a saw with some random
-- elements.
demandEnvGen_ex13 :: IO ()
demandEnvGen_ex13 = audition $ out 0 $ mce2 osig osig
  where
    osig = demandEnvGen AR lvls dur crv 0 1 rst 1 0 1 DoNothing
    lvls = evalSupply0
           (sseq sinf [ sseries 20 (-0.1) 0.01
                      , sseries 20 (-0.1) 0.01
                      , swhite 5 (-0.1) 0.1 ])
    dur  = sampleDur * mouseY KR 1 100 Exponential 0.1
    crv  = env_curve_shape EnvSin
    rst  = impulse AR rate 0 * 1.5
    rate = mouseX KR 1 rmax Exponential 0.1
    rmax = sampleRate * mouseX KR 0.002 1 Exponential 0.1

-- | Example of 'demandEnvGen', take 14, softsyncing a saw with some random
-- elements.
demandEnvGen_ex14 :: IO ()
demandEnvGen_ex14 = audition $ out 0 $ mce2 osig osig
  where
    osig = demandEnvGen AR lvls dur crv 0 1 rst 1 0 1 DoNothing
    lvls = evalSupply0
           (sseq sinf
            [ sseries 20 (-0.1) 0.01
            , sseries 20 (-0.1) 0.01
            , swhite 5 (-0.1) 0.1 ])
    dur  = sampleDur * mouseY KR 1 100 Exponential 0.1
    crv  = env_curve_shape EnvLin
    rst  = impulse AR rate 0
    rate = mouseX KR 1 rmax Exponential 0.1
    rmax = sampleRate * mouseX KR 0.002 1 Exponential 0.1

-- | Example of 'demandEnvGen', take 15, mce.
demandEnvGen_ex15 :: IO ()
demandEnvGen_ex15 = audition $ out 0 osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = demandEnvGen KR lvls dur crv shp gt 0 1 0 1 DoNothing
    ss   = sseq sinf [300, 800, srand 1 [1000,460,300], 400]
    lvls = mce [ evalSupply ss (mkStdGen 0) + rand 'a' 0 3
               , evalSupply ss (mkStdGen 1) + rand 'b' 0 3 ]
    dur  = mouseY KR 0.001 2 Exponential 0.1
    crv  = env_curve_shape (EnvNum undefined)
    shp  = -4
    gt   = mouseX KR 0 1 Linear 0.1 >* 0.5


-- --------------------------------------------------------------------------
--
-- * Dgeom
--
-- --------------------------------------------------------------------------

-- | Plays 'sinOsc' with 'demand', with given trigger and frequency.
f_demand_06 ::
    UGen    -- ^ UGen for trigger.
    -> UGen -- ^ Demand UGen for freq.
    -> IO ()
f_demand_06 tr dmnd = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = demand tr 0 dmnd * 30 + 340

dgeom_ex01 :: IO ()
dgeom_ex01 = f_demand_06 t a
  where
    t    = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a    = dgeom 'a' 15 1 1.2

dgeom_ex02 :: IO ()
dgeom_ex02 = f_demand_06 t a
  where
    t    = dust 't' KR (mouseX KR 1 40 Exponential 0.1)
    a    = dgeom 'a' dinf 1 1.2


-- --------------------------------------------------------------------------
--
-- * Dibrown
--
-- --------------------------------------------------------------------------

dibrown_ex01 :: IO ()
dibrown_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = dibrown 'a' dinf 0 15 1

dibrown_ex02 :: IO ()
dibrown_ex02 = audition $ mrg [out 0 (mce2 osig osig), maxLocalBufs 1]
  where
    osig = combC (sinOsc AR freq 0 * 0.1 * decay tr 0.5) 0.12 0.12 3
    freq = midiCPS (demand tr 0 (dbufrd 'a' buf pos Loop))
    tr   = coinGate 'g' 0.5 (impulse KR 8 0)
    buf  = asLocalBuf 'b' pchs
    pos  = dibrown 'a' dinf 0 (constant $ length pchs) stp
    stp  = linLin (lfdNoise3 'a' KR 0.125) (-1) 1 1 4
    pchs = foldr (\o acc -> map (+o) [0,3,5,7,10] ++ acc) [] [48,60,72]


-- --------------------------------------------------------------------------
--
-- * Diwhite
--
-- --------------------------------------------------------------------------

diwhite_ex01 :: IO ()
diwhite_ex01 = f_demand_06 t a
  where
    t   = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a   = diwhite 'a' len lo hi
    len = dinf
    lo  = 0
    hi  = 15

-- --------------------------------------------------------------------------
--
-- * Drand
--
-- --------------------------------------------------------------------------

drand_ex01 :: IO ()
drand_ex01 = f_demand_06 t a
  where
    t  = impulse KR (mouseX KR 1 400 Exponential 0.1) 0
    a  = evalSupply0 s
    s  = srand sinf [1,3,2,7,8]


-- --------------------------------------------------------------------------
--
-- * Dreset
--
-- --------------------------------------------------------------------------

-- | Demand rate reset.
dreset :: ID i => i -> UGen -> UGen -> UGen
dreset z i r = mkOscId z DR "Dreset" [i,r] 1

-- | Example of 'dreset', take 1.
dreset_ex01 :: IO ()
dreset_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = duty KR (1/mouseX KR 1 40 Exponential 0.1) 0 DoNothing a * 30 + 340
    a    = dreset 'a' (dseries 'b' dinf 0 2) (drand 'c' dinf (mce [0,0,0,1]))

-- | Example of 'dreset', take 2, reset when mouse button is pressed.
dreset_ex02 :: IO ()
dreset_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = duty KR (1/mouseX KR 1 40 Exponential 0.1) 0 DoNothing a * 30 + 340
    a    = dreset 'a' (dseries 'b' dinf 0 2) (mouseButton KR 0 0.1 0.1)

-- | Example of 'dreset', take 3, audio rate.
dreset_ex03 :: IO ()
dreset_ex03 = audition $ out 0 osig
  where
    osig = duty AR sampleDur 0 DoNothing a * (0.1 / constant nmax)
    a    = dreset 'a' (dseries 'b' dinf 0 1)
           (mce [drand 'c' dinf (mce (1 : replicate nmax 0))
                ,drand 'd' dinf (mce (1 : replicate nmax 0))])
    nmax = 90


-- --------------------------------------------------------------------------
--
-- * Dseq
--
-- --------------------------------------------------------------------------

dseq_ex01 :: IO ()
dseq_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = dseq 'a' 3 (mce [1,2,3,7,8])

dseq_ex02 :: IO ()
dseq_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = demand t 0 a * 30 + 340
    t    = impulse AR (mouseX KR 1 10000 Exponential 0.1) 0
    a    = dseq 'a' dinf (mce [rand i 0 10| i <- [0..31::Int]])


-- --------------------------------------------------------------------------
--
-- * Dser
--
-- --------------------------------------------------------------------------

-- | Example of 'dser', take 1.
dser_ex01 :: IO ()
dser_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = dser 'a' 8 (mce [1,3,2,7,8])

-- | Example of 'dser', take 2, from SC3 help of /Pser/.
dser_ex02 :: IO ()
dser_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * aenv
    aenv = envGen KR tr 0.2 0 1 DoNothing $ envPerc 0.01 0.05
    tr   = impulse KR (recip 0.2) 0
    freq = midiCPS (demand tr 0 s)
    s    = evalSupply0 (sser sinf [sser 3 [60,61,63,65,72]])

-- --------------------------------------------------------------------------
--
-- * Dseries
--
-- --------------------------------------------------------------------------

dseries_ex01 :: IO ()
dseries_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = evalSupply0 (sseries 15 0 1)

dseries_ex02 :: IO ()
dseries_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = (demand t 0 a `modE` 15) * 30 + 340
    t    = dust 't' KR (mouseX KR 1 40 Exponential 0.1)
    a    = evalSupply0 (sseries sinf 0 1)


-- --------------------------------------------------------------------------
--
-- * Dshuf
--
-- --------------------------------------------------------------------------

dshuf_ex01 :: IO ()
dshuf_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = evalSupply0 (sseq sinf [sshuf 3 [1,3,2,7,8.5]])

dshuf_ex02 :: IO ()
dshuf_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = demand t 0 a * 30 + 340
    t    = impulse KR (mouseX KR 1 10000 Exponential 0.1) 0
    a    = evalSupply0 s
    s    = sseq sinf [sshuf 5 [sval (rand i 0 10)| i <- [0..80::Int]]]

dshuf_ex03 :: IO ()
dshuf_ex03 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = evalSupply0 s
    s = sseq sinf
        [sshuf 8
         [srand 1 [1,2,3], 3, srand 1 [20,23,56], 7, 8.5]
        ]

-- --------------------------------------------------------------------------
--
-- * Dstutter
--
-- --------------------------------------------------------------------------

dstutter_ex03 :: IO ()
dstutter_ex03 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = evalSupply0 s
    s = sstutter (siwhite sinf 2 8) i
    i = sseq sinf [1,2,3]


-- --------------------------------------------------------------------------
--
-- * Dswitch
--
-- --------------------------------------------------------------------------

f_demand_07 :: (Supply -> [Supply] -> Supply) -> IO ()
f_demand_07 swtch = audition $ out 0 $ mce2 osig osig
  where
    osig = sinOsc AR freq 0 * 0.1
    freq = demand t 0 d * 300 + 400
    t    = impulse KR 4 0
    d    = evalSupply0 $
           swtch (sseq sinf [0,1,2,1,0])
           [ swhite 2 3 4
           , swhite 2 0 1
           , sseq 2 [1,1,1,0]
           ]

dswitch_ex01 :: IO ()
dswitch_ex01 = f_demand_07 sswitch


-- --------------------------------------------------------------------------
--
-- * Dswitch1
--
-- --------------------------------------------------------------------------

dswitch1_ex01 :: IO ()
dswitch1_ex01 = f_demand_07 sswitch1

dswitch1_ex02 :: IO ()
dswitch1_ex02 = f_demand_06 t a
  where
    t = impulse KR 6 0
    a = evalSupply0 s
    s = sswitch1 (sval (mouseX KR 0 4 Linear 0.1))
        [ 1, 3, sval (mouseY KR 1 15 Linear 0.1), 2, swhite sinf 0 3 ]

dswitch1_ex03 :: IO ()
dswitch1_ex03 = f_demand_06 t a
  where
    t = impulse KR 6 0
    a = evalSupply0
        (sswitch1 (sval (mouseX KR 0 4 Linear 0.1))
         [sseq sinf [0..i*3] | i <- [0,1,2,3,4,5]])


-- --------------------------------------------------------------------------
--
-- * Duty
--
-- --------------------------------------------------------------------------

duty_ex01 :: IO ()
duty_ex01 = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1
    freq = duty KR dur 0 DoNothing val
    dur  = evalSupply0 $ srand sinf [0.01,0.2,0.4]
    val  = evalSupply0 $ sseq sinf [204,400,201,502,300,200]

duty_ex02 :: IO ()
duty_ex02 = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1
    freq = duty KR dur 0 DoNothing val
    dur  = mouseX KR 0.001 2 Exponential 0.1
    val  = evalSupply0 $ sseq sinf [204,400,201,502,300,200]

duty_ex03 :: IO ()
duty_ex03 = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1
    freq = duty KR dur rst DoNothing val * 30 + 250
    dur  = evalSupply0 (sseq 1 [0.2, 0.3, 0.4, sseq sinf [1,1,1,2,1,2]] / 2)
    rst  = dust 'r' KR 1
    val  = evalSupply0 (sseq sinf [0, 1, 2, sseq sinf [1,2,3,4,5]])

duty_ex04 :: IO ()
duty_ex04 = audition $ out 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1
    freq = duty KR dur rst DoNothing val * 30 + 250
    dur  = evalSupply0 (sseq 1 [0.2,0.3,0.4, sseq sinf [1,1,1,2,1,2]] / 2)
    rst  = evalSupply0 (sseq sinf [1,2,4,5])
    val  = evalSupply0 (sseq 1 [0,1,2,sseq sinf [1,2,3,4,5]])

duty_ex05 :: IO ()
duty_ex05 = audition $ out 0 osig
  where
    osig = duty AR dur rst DoNothing val
    dur  = mouseX KR 1 125 Exponential 0.1 * sampleDur * mce [1,1.02]
    rst  = 0
    val  = evalSupply0 (sswitch1 idx a)
    idx  = sval (mouseY KR 0 (constant n-1) Linear 0.1)
    a    = [ sseq sinf vs
           | i <- [0..n]
           , let vs = map sval $ take 64 $ randomRs (-0.2,0.2) (mkStdGen i)
           ]
    n    = 5


-- --------------------------------------------------------------------------
--
-- * Dwhite
--
-- --------------------------------------------------------------------------

dwhite_ex01 :: IO ()
dwhite_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 40 Exponential 0.1) 0
    a = dwhite 'a' dinf 0 15


-- --------------------------------------------------------------------------
--
-- * Dwrand
--
-- --------------------------------------------------------------------------

dwrand_ex01 :: IO ()
dwrand_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 400 Exponential 0.1) 0
    a = evalSupply0 $ swrand sinf [0,1,2,7] [0.4,0.4,0.1,0.1]


-- --------------------------------------------------------------------------
--
-- * Dxrand
--
-- --------------------------------------------------------------------------

dxrand_ex01 :: IO ()
dxrand_ex01 = f_demand_06 t a
  where
    t = impulse KR (mouseX KR 1 400 Exponential 0.1) 0
    a = evalSupply0 $ sxrand sinf [1,3,2,7,8]


-- --------------------------------------------------------------------------
--
-- * TDuty
--
-- --------------------------------------------------------------------------

tDuty_ex01 :: IO ()
tDuty_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = tDuty AR dur rst DoNothing lvl gap
    dur  = evalSupply0 $ sseq sinf [0.1,0.2,0.4,0.3]
    rst  = 0
    lvl  = 1
    gap  = 0

tDuty_ex02 :: IO ()
tDuty_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = ringz tr 1000 0.1
    tr   = tDuty AR dur 0 DoNothing lvl 0
    dur  = evalSupply0 $ sseq sinf [0.1,0.2,0.4,0.4]
    lvl  = evalSupply0 $ sseq sinf [0.1,0.4,0.01,0.5,1.0]

tDuty_ex03 :: IO ()
tDuty_ex03 = audition $ out 0 $ mce2 osig osig
  where
    osig = ringz tr 1000 0.1
    tr   = tDuty AR dur 0 DoNothing lvl 0
    dur  = mouseX KR 0.001 2 Exponential 0.1
    lvl  = evalSupply0 $ sseq sinf [0.1,0.4,0.01,0.5,1.0]

tDuty_ex04 :: IO ()
tDuty_ex04 = audition $ out 0 osig
  where
    osig = ringz tr 1000 0.01
    tr   = tDuty AR dur 0 DoNothing lvl 0
    dur  = mouseX KR 1 2048 Exponential 0.1 * sampleDur * mce [1,1.02]
    lvl  = evalSupply0 (sswitch1 idx a)
    idx  = sval (mouseY KR 0 (constant n-1) Linear 0.1)
    a    = [ sseq sinf $ map sval $ take 64 $ randomRs (-0.2,0.2) (mkStdGen i)
           | i <- [0..n-1]
           ]
    n    = 5

tDuty_ex05 :: IO ()
tDuty_ex05 = withSC3 $ do
    let ddmnd = offsetOut 0 osig
        osig  = tDuty AR (dseq 'a' 1 (mce [0])) 0 RemoveSynth 0.5 0
    _ <- async $ d_recv $ synthdef "ddmnd" ddmnd
    forM_ [0..9::Int] $ \_ -> do
        now <- liftIO time
        sendOSC $ bundle (now + 0.2) [s_new "ddmnd" (-1) AddToTail 1 []]
        liftIO $ do
            del <- getStdRandom (randomR (0,1000000::Int))
            threadDelay del

tDuty_ex06 :: IO ()
tDuty_ex06 = withSC3 $ do
    let ddmnd2 = offsetOut 0 osig
        osig   = tDuty AR (dgeom 'a' 20 0.05 0.9) 0 RemoveSynth 0.5 0
    _ <- async $ d_recv $ synthdef "ddmnd2" ddmnd2
    forM_ [0..9::Int] $ \_ -> do
        now <- liftIO time
        sendOSC $ bundle (now+0.2) [s_new "ddmnd2" (-1) AddToTail 1 []]
        liftIO $ do
            del <- getStdRandom (randomR (0,1000000::Int))
            threadDelay del

tDuty_ex07 :: IO ()
tDuty_ex07 = audition $ out 0 osig
  where
    osig = ringz tr (mce [400,700]) 0.1 * 0.1
    tr   = tDuty AR dur 0 DoNothing val 0
    dur  = evalSupply0 $ srand sinf [sgeom 20 0.1 0.8, 1, 2]
    val  = mce [ evalSupply0 $ srand sinf $ map sval $
                 take 8 $ randomRs (0,1) (mkStdGen 1)
               , evalSupply0 $ sseq sinf $ map sval $
                 take 8 $ randomRs (0,1) (mkStdGen 2)
               ] * 2

-- --------------------------------------------------------------------------
--
-- * Compound structure
--
-- --------------------------------------------------------------------------

f_demand_08 :: Supply -> IO ()
f_demand_08 sup = audition $ offsetOut 0 osig
  where
    osig = sinOsc AR (freq * mce [1,1.01]) 0 * 0.1 *
           envGen KR tr 1 0 0.1 DoNothing
           (Envelope [0,1,1,0.1] [0.01,0.1,0.89] [EnvCub] Nothing Nothing)
    freq = demand tr 1 (evalSupply0 sup)
    tr   = impulse KR 6 0

cmp_ex01 :: IO ()
cmp_ex01 = f_demand_08 (fmap midiCPS sup)
  where
    sup =
        sseq sinf
        [ sxrand 3
          [ sseq 2
            (map (+60) [0,2,7,0, 2,7,0,srand 1 [12,24,36]])
          , sseq 2
            (map (+48) [0,2,7,0, 2,7,0,srand 1 [12,24,36]])
          , sseq 1
            [sshuf 2 (map (+72) [0,2,3,5, 7,10,12,14])]
          ]
        -- , sbrown 16 48 84 2
        , sshuf 2
          [ sseq 1 [60,60,72,60, 60,72,60,72]
          , sxrand 8 [36,48,60,72,84,96]
          ]
        , let x = srand 1 [48,55,67,72,79,84]
          in  sstutter 2 $
              sseq (srand sinf [2,4]) [60, 60, x, 60, 60, x, 60, x]
        ]


-- --------------------------------------------------------------------------
--
-- * Polypohny inside single synth
--
-- --------------------------------------------------------------------------

-- | Simple example showing polyphony sound with 'gate' and 'pulseDivider'.
--
-- Each sound source is built with /fsig/, which takes index value to identify
-- the voicing.  Number of voices is specified at the time of defining the
-- synthdef.
--
-- Frequency and pan position are holded with 'gate' inside the function for making
-- sound of single voice. Trigger for the gate is routed with 'pulseDivider' with
-- given index value for this voice as offset.  Each voice will not take new
-- parameter until all the other voices get new value.
--
poly_01 :: IO ()
poly_01 = audition $ out 0 osig
  where
    osig    = sum $ map fsig [0..npoly-1::Int]
    npoly   :: Num a => a
    npoly   = 12

    freq0   = demand tr0 0 (midiCPS (dxrand 'a' dinf (mce pchs)))
    pan0    = demand tr0 0 (dwhite 'w' dinf (-1) 1)
    tr0     = coinGate 'a' 0.5 (impulse KR 4 0)

    pchs    = foldr (\p ps -> map (+p) degs ++ ps) [] octs
    degs    = [0,2,3,5,7,10]
    octs    = take 5 $ iterate (+12) 36

    fsig i  = pan2 sig0 pan 1
      where
        sig0    = foldr (\a b -> allpassN b 0.1 a 4) sig1 rs
        rs      = map (\j -> rand j 0.001 0.1) "abcde"
        sig1    = combC (rlpf sig2 (freq*cfm) 0.3) 0.12 0.12 3
        sig2    = sinOsc AR freq phs * aenv
        phs     = sinOsc AR freq 0 *
                  envGen KR tr midx 0 1 DoNothing
                  (Envelope [0,1,0,1,0.5] [0.001,0.005,0.25,0.75] [EnvLin]
                   (Just 0) Nothing) *
                  (linLin (lfTri KR (vibr*0.3) 0) (-1) 1 0 1)
        freq    = gate freq0 tr + vib
        pan     = gate pan0 tr
        tr      = pulseDivider tr0 npoly (constant i)
        vib     = lfTri KR (vibr*5) 0 * squared (lfdNoise3 i KR (1/9)) * 16
        vibr    = envGen KR tr 1 0 dur DoNothing $
                  Envelope [0,1,0.5] [0.8,0.2] [EnvLin] (Just 0) Nothing
        aenv    = envGen KR tr 0.1 0 dur DoNothing
                  (Envelope [0,1,0] [atk,1-atk] [EnvCub] Nothing Nothing)
        atk     = tExpRand i 0.001 0.999 tr
        dur     = tExpRand 'd' 0.25 8 tr
        midx    = linLin (control KR "midx" 0.25) 0 1 0 16
        cfm     = linLin (control KR "cfm" 0.2) 0 1 1 8


-- --------------------------------------------------------------------------
--
-- * Auxiliary
--
-- --------------------------------------------------------------------------

-- | Passes given 'Supply' to 'evalSupply' with ('mkStdGen' 0).
evalSupply0 :: Supply -> UGen
evalSupply0 = flip evalSupply (mkStdGen 0)
