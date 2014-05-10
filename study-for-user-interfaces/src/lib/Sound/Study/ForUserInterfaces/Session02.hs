{-# LANGUAGE TemplateHaskell #-}
{-|

Session with TUI01, take 2.

-}
module Sound.Study.ForUserInterfaces.Session02 where

import           Sound.OSC
import           Sound.SC3
import           Sound.SC3.ID
import           Sound.SC3.Supply
import           Sound.SC3.TH.Synthdef (synthdefGenerator)
import           Sound.SC3.Tree

import           Sound.Study.ForUserInterfaces.TUI01
import qualified Sound.Study.ForUserInterfaces.Session01 as Session01

-- --------------------------------------------------------------------------
--
-- * Synthdefs
--
-- --------------------------------------------------------------------------

sendSynthdefs :: Transport m => m ()
sendSynthdefs = mapM_ (async . d_recv) synthdefs

synthdefs :: [Synthdef]
synthdefs =  Session01.synthdefs ++ $(synthdefGenerator)

synth_sin01 :: UGen
synth_sin01 = out obus (pan2 osig pan 1)
  where
    osig = sinOsc AR freq 0 * aenv * 0.3
    aenv = envGen KR tr 1 0 dur DoNothing (envPerc 0.01 1)
    obus = k "out" 0
    freq = k "freq" 440
    dur  = k "dur" 0.2
    pan  = linControl "pan" (-1) 1 0
    tr   = tr_control "t_tr" 0
    k    = control KR

linControl :: String -> Double -> Double -> Double -> UGen
linControl name minv maxv iniv =
    let iniv' = (iniv - minv) / (maxv - minv)
        c     = constant
    in  linLin (control KR name iniv') 0 1 (c minv) (c maxv)

synth_nz01 :: UGen
synth_nz01 = out obus (pan2 osig pan 1)
  where
    osig = resonz (whiteNoise 'W' AR) cf rq * aenv * 0.5
    aenv = envGen KR tr 1 0 dur DoNothing (envPerc 0.01 1)
    tr   = tr_control "t_tr" 0
    dur  = 0.3
    cf   = linControl "cf" 200 8000 1200
    rq   = linControl "rq" 0.1 0.95 0.5
    obus = k "out" 0
    pan  = linControl "pan" (-1) 1 0
    k    = control KR

synth_cmb01 :: UGen
synth_cmb01 = replaceOut obus osig
  where
    osig = wsig * wet + isig * (1-wet)
    wsig = combC isig 1 dlt dct
    dlt  = control KR "dlt" 0.2
    dct  = control KR "dct" 2
    isig = in' 2 AR (control KR "in" 0)
    wet  = control KR "wet" 0
    obus = control KR "out" 0

synth_pv03:: UGen
synth_pv03 = out obus (pan2 osig pan 1)
    where
      osig = muld
      muld = 0.1 * pv_with2Inputs inA inB pv_MagMul * 0.3
      inA  = mix $ lfSaw AR frqs 0 * 0.1
      frqs = mce [midiCPS (tChoose i tr (mce pchs))|i<-"abcdefg"]
      pchs = foldr (\o acc -> map ((+ofst) . (+o)) degs ++ acc) [] octs
      degs = [0,thrd,7,10]
      thrd = tIRand '3' 3 4 (coinGate '#' (1/15) tr)
      octs = take 5 $ iterate (+12) 33
      ofst = tIRand 'O' (-6) 6 (coinGate 'g' (1/31) tr)
      tr   = control KR "t_tr" 0
      inB  = playBuf 1 AR bufn (bufRateScale KR bufn * rt) 1 0 Loop DoNothing
      rt   = mouseY KR 0.25 4 Exponential 0.1
      bufn = control KR "bufn" 12
      obus = control KR "out" 0
      pan  = linLin (control KR "pan" 0) 0 1 (-1) 1

pv_with2Inputs :: UGen -> UGen -> (UGen -> UGen -> UGen) -> UGen
pv_with2Inputs sigA sigB fpv = osig
  where
    osig = ifft' $ fpv chainA chainB
    chainA = f 'x' sigA
    chainB = f 'y' sigB
    f i sig = fft' (mrg2 (localBuf i 2048 1) (maxLocalBufs 2)) sig


-- --------------------------------------------------------------------------
--
-- * Control functions
--
-- --------------------------------------------------------------------------

initSession02 :: IO ()
initSession02 = withSC3 (initializeTUI01 >> sendSynthdefs)

setupSession02 :: IO ()
setupSession02 = withSC3 $ do
    mapM_ sendSynth $ words "nz01 sin01"
    sendParam (nodeId ==? defaultTargetNid ||?
               nodeId ==? masterNid) "amp" 1 8

param_sin01 :: IO ()
param_sin01 = withSC3 $ do

    sendSupply02 (synthName ==? "sin01") "dur" 4 False $
        0.2

    sendSupply02 (synthName ==? "sin01") "t_tr" 4 True $
        -- sseq sinf [1,0,srand 1 [1,0], 0]
        -- sseq sinf [1,0,0,1,0,0,1,0]

        -- sseq sinf [1, sseq 3 [0], srand 1 [1,0], sseq 3 [0]]
        1
        -- srand sinf [1,0]

        -- sseq sinf
        -- [ sseq 3
        --   [ sseq 4 [1], 1, 0, srand 2 [1,0] ]
        -- , srand 8 [1,0] ]

        -- srand sinf [ sseq 1 [1,sseq 3 [0]]
        --            , sseq 1 [1,0]
        --            , sseq 1 [1,1] ]
        -- sseq 1 [0], 1, sseq 1 [0]]

    -- sendSupply01 "sin01" "freq" False $
    --     sstutter 8 $
    --     sseq sinf [srand 1 [440, 660, 880]]

    -- sendControl "sin01" "freq" $ \_ ->
    --     linLin (lfdNoise3 'a' KR 13) (-1) 1 100 2000

    -- sendControl02 (synthName ==? "sin01") "freq" 8 $ \tr ->
    --     let l1   = linLin (lfdNoise1 'a' KR lf) (-1) 1 1 100
    --         t1   = coinGate 'a' 0.1 tr
    --         lf   = tExpRand 'g' 0.1 10 t1
    --         fmax = tExpRand 'g' 100 20000 t1
    --         sig  = lpf (whiteNoise 'a' KR) l1
    --     in  linLin (combC sig 0.2 0.2 8) (-1) 1 10 fmax

    -- sendControl02 (synthName ==? "sin01") "freq" 4 $ \tr ->
    --     demand tr 0
    --     (evalSupply
    --      (sstutter 4 $ sseq sinf [330,110,330,2200]) (mkStdGen 0))

    sendSupply02 (synthName ==? "sin01") "freq" 4 False $
        fmap (midiCPS . (+91)) $
        sstutter 1 $
        -- let x = [7,5,0, 7,5,12, 7,0] -- 12,10,9,7,5,4,2,0]
        --     y = reverse x
        -- let x = [0,7,12,5, 0,12,7,12]
        let x = [0,12,7,12, 7,0,12,7]
            y = reverse x
        -- in sseq sinf
        --    [ sseq 1 x
        --    , sseq 1 y
        --    , sseq 1 x
        --    , sxrand 8 x ]
        in  sseq sinf
            [ sseq 3 x, sseq 1 y ]

    sendSupply02 (synthName ==? "sin01") "pan" 4 False $
        sstutter (srand sinf [2,4,8]) $
        swhite sinf 0 1

    -- sendControl02 (synthName ==? "sin01") "pan" 4 $ \_ ->
    --     -- linLin (lfTri KR (1/6) 0) (-1) 1 0 1
    --     linLin (lfdNoise1 'a' KR 0.23) (-1) 1 0 1

    -- sendControl02 (nodeId ==? -280) "dcy" 4 $ \_ ->
    --     linLin (lfdNoise3 'D' KR (1/31)) (-1) 1 0 1

    -- sendControl02 (nodeId ==? -16728) "wet" 4 $ \_ -> 1
    -- sendControl02 (nodeId ==? -16728) "dcy" 4 $ \_ ->
    --     linLin (lfSaw KR 0.125 0) (-1) 1 0 1 `lag` 0.1

    -- sendParam (nodeId ==? -1800) "dlt" 4 0.38

param_sin01_02 :: IO ()
param_sin01_02 = withSC3 $ do
    sendSupply02 (synthName ==? "sin01") "freq" 4 False $
        (sstutter (4*4) $ sseq sinf [220,660,220,440])

param_sin01_03 :: IO ()
param_sin01_03 = withSC3 $ do
    sendSupplys (synthName ==? "sin01") 8
        [ supply "t_tr" True $
          (sseq sinf [1,0,0,0])
        , supply "freq" False $
          sstutter 8 $ sseq sinf [220,440,880,1760]
        ]

param_nz01 :: IO ()
param_nz01 = withSC3 $ do
    sendSupply02 (synthName ==? "nz01") "t_tr" 8 True  $
        -- sseq sinf [1]
        -- sseq sinf [1,0]
        -- srand sinf [sseq 1 [1,0], sseq 1 [1,1] ]
        -- sseq sinf [0,0,sseq 1 [1,0], sseq 1 [1,1]]

        srand sinf [ sseq 1 [1,0,0,0]
                   , sseq 1 [1,0,1,0]
                   , sseq 1 [1,0,0,1]
                   , sseq 1 [1,1,0,1]
                   , sseq 1 [1,1,1,1] ]

        -- sseq sinf
        -- [ sser 16 [0]
        -- , sseq 1 [1,1,0,1, 1,0,1,1]
        -- , sser 8 [sseq 1 [1,0], sseq 1 [1,1]] ]

        -- sseq sinf [0]

        -- srand sinf [0,1,1,1,1,1]
        -- sseq sinf
        -- [ sseq 20 [0], sseq 4 [1]
        -- , sseq 8 [1] ]

    let b = 0.05
        h = 0.98
        s = 0.25
        p1 = [b,h,h,h, s,h,h,h]
        p2 = [b,h,s,b, s,b,h,h]
    sendSupply02 (synthName ==? "nz01") "cf" 8 False $
        -- sstutter 1 $
        -- sseq sinf [0.05,sseq 1 [0.92], 0.25, sseq 1 [0.93]]
        -- sstutter 2 $

        -- sseq sinf
        -- [ sseq 1 p1
        -- , sseq 1 p2
        -- , sseq 1 p1
        -- , sseq (srand sinf [1,5]) [b, srand 7 [b,s,h]] ]

        -- sseq sinf
        -- [ sseq 6 p1
        -- , srand 1
        --   [ sseq 2 p2
        --   , swhite 16 0 1 ] ]

        -- swhite sinf 0 1
        -- sseq sinf [h]
        -- sseq sinf [sseq 3 p1, sseq 1 p2]

        sseq sinf [ sseq 2 [sseq 1 p1]
                  , srand 1 [ sseq 1
                              [ sgeom 8 1 0.75
                              , sgeom 8 (0.75**8) (recip 0.75) ]
                            , sseq 1 p2
                            , swhite 16 0 1 ]]
        -- [0.05, sseq 3 [0.98], 0.25, sseq 3 [0.98]]

    sendSupply02 (synthName ==? "nz01") "pan" 8 False $
        0.5
        -- swhite sinf 0 1
        -- sstutter (srand sinf [4,8]) $
        -- swrand sinf [swhite 1 0 1,0.5] [0.01,0.99]
        -- sstutter (srand sinf [2,4,8]) $
        -- swhite sinf 0 1
        -- srand sinf [0,0.45,0.5,0.55,1]

init_bd03 :: IO ()
init_bd03 = withSC3 $ do
    _ <- sendSynth "bd03"
    sendFx "bd03" "ap01"

params_bd03 :: IO ()
params_bd03 = withSC3 $ do
    sendSupply02 (synthName ==? "bd03") "t_tr0" 4 True $
        let r p = swhite 1 0 1 <=* p
        in  sseq sinf
            [ sseq 7 [1, 0, r (1/19), r (1/13)]
            , sseq 1 [1, 0, 0, 1] ]

init_pv03 :: IO ()
init_pv03 = withSC3 $ do
    -- _ <- sendSynth "pv03"
    sendFx "pv03" "ap01"
    sendFx "pv03" "cmb01"

params_pv03 :: IO ()
params_pv03 = withSC3 $ do
    sendSupply02 (synthName ==? "pv03") "t_tr" 4 True $
        sstutter 8 $
        sseq sinf [1,0,0,0]
        -- srand sinf [1,0]

    sendControl02 (synthName ==? "pv03") "pan" 4 $ \_ ->
        linLin (lpf (whiteNoise 'P' KR) 20) (-1) 1 0 1

    -- XXX: Modify here.
    -- Specify effect nodes with other way than node ID.
    let apid = -26152
        cmid = -26144
    sendParam (nodeId ==? cmid) "wet" 1 31

    -- sendControl02 (nodeId ==? cmid) "dlt" 4 $ \tr ->
    --     -- let tr' = coinGate 'g' (1/3) tr
    --     -- in  tExpRand 'f' 0.0032 0.008 tr' `lag` 0.2
    --     0.03

    -- sendSupply02 (nodeId ==? -20016) "dlt" 4 False $
    --     sseq sinf
    --     [ sbrown (srand sinf [4,8,16,32])
    --       (recip 330) (recip 110) (swhite sinf 0.0001 0.001)
    --     , swhite (srand sinf [4,8,16,32])
    --       (recip 330) (recip 110) ]

    sendControl02 (nodeId ==? cmid) "dct" 4 $ \_ ->
        linLin (lfdNoise0 'c' KR (1/8)) (-1) 1 0.05 8

    sendParam (nodeId ==? apid) "wet" 1 19
    sendSupply02 (nodeId ==? apid) "dcy" 4 False $
        sseq sinf
        [ sseq 6 [0, 0, 1, 0, 0, 0, 1, 0 ]
        , srand 4
          [ sseq 1 [1,0]
          , sseq 1 [0,0] ]
        , srand 8 [0,1] ]

-- routers :: IO ()
-- routers = withSC3 $ do
--     sendParam (nodeId ==? 99)  "amp" 1 13
--     sendParam (nodeId ==? 102) "amp" 1.5 8  -- nz02
--     sendParam (nodeId ==? 104) "amp" 1 0    -- bd03
--     sendParam (nodeId ==? 106) "amp" 1 9    -- sin01
--     sendParam (nodeId ==? 108) "amp" 1 10   -- pv03

routers :: IO ()
routers = withSC3 $ do
    sendParam (nodeId ==? 101) "amp" 2.3 19
    sendParam (nodeId ==? 102) "amp" 1   19
    sendParam (nodeId ==? 103) "amp" 1   19
    sendParam (nodeId ==? 104) "amp" 0.3 19
    -- sendParam (nodeId ==? 105) "amp" 0   19
    -- sendParam (nodeId ==? 106) "amp" 0.8 19


-- --------------------------------------------------------------------------
--
-- * Miscellaneous
--
-- --------------------------------------------------------------------------

free_ex01 :: IO ()
free_ex01 = withSC3 $ do
    let def1 = out 0 (sinOsc AR 440 0 * 0.1)
        def2 = out 1 (sinOsc AR 330 0 * 0.1)
        def3 = free (impulse KR 0.25 0.5) (mce [1000,1001])
    mapM_ (async . d_recv . uncurry synthdef)
        [("def1",def1),("def2",def2),("def3",def3)]
    send $ s_new "def1" 1000 AddToTail 1 []
    send $ s_new "def2" 1001 AddToTail 1 []
    send $ s_new "def3" (-1) AddToTail 1 []

parentOf :: Condition SCNode -> Condition SCNode
parentOf cond = \n -> case n of
    Group _ ns -> not $ null $ filter cond ns
    _          -> False

-- XXX: Query with condition to parent group.
-- inGroup ::
--     Condition SCNode
--     -> Condition SCNode
--     -> Condition SCNode
-- inGroup pCond cCond = \n ->
--     case n of
--         Group _ ns -> pCond n && all cCond ns
--         Synth {}   -> False

printCond :: Condition SCNode -> IO ()
printCond cond = withSC3 $ do
    nodes <- getRootNode
    liftIO $ mapM_ (putStrLn . drawSCNode) $ queryN cond nodes

sendNSynths :: Transport m => Int -> m ()
sendNSynths n = do
    let def   = out 0 (pan2 (sinOsc AR freq phs) pan amp * aenv)
        freq  = control KR "freq" 440
        amp   = control KR "amp" 0.01
        msg f = s_new "s1000" (-1) AddToTail 1 [("freq",f),("amp",0.008)]
        aenv  = squared (lfdNoise3 'k' KR aef)
        aef   = mouseY KR 0.01 10 Exponential 0.1
        pan   = rand 'p' (-1) 1
        phs   = rand 'h' 0 (2*pi)
        bdl   = bundle immediately $ map msg $ take n $ iterate (*1.01) 20
    send $ withCM (d_recv $ synthdef "s1000" def) bdl

withSC3' :: Connection TCP a -> IO a
withSC3' = withTransport (openTCP "127.0.0.1" 57111)

sc3 :: Connection UDP a -> IO a
sc3 = withSC3

tmp01 :: IO ()
tmp01 = withSC3 $ do
    sendParam (nodeId ==? 112) "amp" 0 13


-- | Playing with order-of-execution. Adding two source synths with /freq/
-- parameter mapped from control synth.  One source synth is after the control
-- synth, another is before, to hear one control block delay.
--
-- Sustained value could be heard, triggered value could not.
--
ooe_ex01 :: IO ()
ooe_ex01 = withSC3 $ do
    let src  = out obus (sinOsc AR freq 0 * 0.1 * decay t_tr 0.3)
        freq = control KR "freq" 440
        t_tr = tr_control "t_tr" 1
        ctr  = out obus $ dust 'a' KR freq
        obus = control KR "out" 0
    mapM_ (async . d_recv . uncurry synthdef)
        [("src",src),("ctr",ctr)]

    sendOSC $ bundle immediately
        [ s_new "src" 1000 AddToTail 1
          [("out",0),("freq",440)]
        , s_new "ctr" 1001 AddBefore 1000
          [("out",100),("freq",10)]
        , n_map 1000 [("t_tr",100)] ]

    sendOSC $ bundle immediately
        [ s_new "src" 1002 AddToHead 1
          [("out",1),("freq",880)]
        , n_map 1002 [("t_tr",100)] ]

    -- Placing node after ctrl synth will make sound.
    sendOSC $ bundle immediately
        [ s_new "src" 1003 AddToTail 1
          [("out",1),("freq",110)]
        , n_map 1003 [("t_tr",100)] ]


-- | Example showing a OSC completion message containing s_new and d_recv.
--
-- The s_new part of the message will be executed after d_recv has been done.
--
withCM_ex01 :: IO ()
withCM_ex01 = withSC3 $ do
    let def  = out 0 (pan2 osig (whiteNoise 'w' KR) 1)
        osig = sinOsc AR freq 0 * 0.3 * decay tr dct
        tr   = dust 'a' KR df
        freq = tExpRand 'f' 100 10000 (coinGate 'g' 0.01 tr)
        df   = squared (lfdNoise3 'f' KR 0.5) * 88 + 1
        dct  = squared (lfdNoise1 'd' KR 0.25)
        name = "temp_" ++ show (hashUGen def)
    sendOSC $
        withCM (d_recv $ synthdef name def) $
        s_new name 1000 AddToTail 1 []
