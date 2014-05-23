{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Scratches, take 1

-}
module Sound.Study.ForUserInterfaces.Misc.Scratch01 where

import Control.Arrow (first)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Function (fix)
import System.Random (newStdGen, randomR, randomRs)

import Sound.OSC
import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
import Sound.SC3.Tree

import qualified Sound.Study.ForUserInterfaces.GUI.GUI01 as GUI01

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

-- --------------------------------------------------------------------------
--
-- Scratches written while working on GUI01
--
-- --------------------------------------------------------------------------


-- Change "len" in group 1.
play_percs :: IO ()
play_percs = withSC3 $ do
    mapM_ (async . d_recv) (GUI01.synthdefs)
    sendOSC $ bundle immediately
        [ s_new "trig00" 998 AddToHead 1 [("out",101),("beat",4),("bpm",128)]
        , s_new "rbufrd01" 2000  AddToTail 1
          [("out",102),("bufn",100),("len",16)]
        , n_map 2000 [("tr0",101)]
        , s_new "bd01" 2001 AddToTail 1 [("out",0),("freq",69),("dur",0.18)]
        , n_map 2001 [("t_tr0",102)]
        , s_new "rbufrd01" 2002 AddToTail 1 [("out",103),("bufn",101)]
        , n_map 2002 [("tr0",101)]
        , s_new "hat01" 2003 AddToTail 1 [("out",0)]
        , n_map 2003 [("t_tr0",103)]
        , s_new "rbufrd01" 2004 AddToTail 1 [("out",104),("bufn",102)]
        , n_map 2004 [("tr0",101)]
        , s_new "snr01" 2005 AddToTail 1 [("out",0)]
        , n_map 2005 [("t_tr0",104)]
        ]

alloc_buf_100 :: IO Message
alloc_buf_100 = withSC3 $
    async $ b_alloc_setn1 100 0 $
    [1,1,0,1, 1,0,1,1, 0,1,1,0, 1,0,0,0.5]
    -- [1,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,1,0]

alloc_buf_101 :: IO Message
alloc_buf_101 = withSC3 $
    async $ b_alloc_setn1 101 0 $
    [1,0,0,0.8, 1,0,0,0, 0.8,0.3,0,0, 1.2,0,0,0]

alloc_buf_102 :: IO Message
alloc_buf_102 = withSC3 $
    async $ b_alloc_setn1 102 0 $
    [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0]

sb_r :: Int -> Double -> [Double] -> IO ()
sb_r bufn offset ds = withSC3 $ do
    g <- liftIO newStdGen
    let is = take 16 $ randomRs (0,length ds-1) g
    send $ b_setn1 bufn 0 $ [(ds!!i)+offset|i<-is]

sb :: Int -> [Double] -> IO Message
sb bufn vals = withSC3 $ async $ b_alloc_setn1 bufn 0 vals

crep :: Int -> [a] -> [a]
crep n vs = concat $ replicate n vs

{-

sb   0 (crep 7 [1,0,0,0] ++ [1,0,0,1])
sb   0 (crep 8 [1,1/32,1/32,1/32])
sb_r 0 0 [0,0.5,1]

sb   1 (replicate 17 1 ++ replicate 15 0)
sb   1 (concat $ replicate 8 [0,0,1,0.25])

sb   3  (concat $ replicate 2 [0,0,0,0, 0,0,0,0, 1,0,0,0])
sb   5  (concat $ replicate 2 [0,0,0,0, 0,0,0,0, 1,0,0,0])

sb   4 (replicate 32 0)
sb   4 (1 : replicate 31 0)
sb   4 (replicate 32 1)
sb   4 (concat $ replicate 16 [0.75,0.25])
sb   4 (concat $ replicate 8 [1,0,0.5,0.25])
sb_r 4 0 [0.25,0.5,0.75,1]
sb   4 (concat $ replicate 16 [0,0.5])

sb_r 5 0 [0,0.5]

sb   7  (1 : replicate 31 0)

sb   11 (1 : replicate 31 0)
sb   11 (replicate 17 1 ++ replicate 15 0)
sb   11 (concat $ replicate 8 [1,0,0.25,0])
sb   11 (concat $ replicate 16 [1,0])
sb   11 (replicate 32 1)
sb   11 (replicate 32 0.9)
sb_r 11 0 [0,0,0.5,0.75,1]
sb_r 11 0 [0,0.5]

slen 5
slen 7
slen 4
slen 8
slen 32

-}

slen :: Double -> IO ()
slen n = withSC3 $ send $ n_set 10 [("len",n)]

sb_100r1, sb_100r2, sb_100r3, sb_100r4 :: IO ()
sb_100r1 = sb_r 100 0 [0.1,0.2..1]
sb_100r2 = sb_r 100 0 [0,0.5,1]
sb_100r3 = sb_r 100 0 [0,1]
sb_100r4 = sb_r 100 0 [1]

sb_101r1, sb_101r2, sb_101r3, sb_101r4, sb_101r5 :: IO ()
sb_101r1 = sb_r 101 0 [0,0.5,0.8,1]
sb_101r2 = sb_r 101 0 [0.1,0.2..1]
sb_101r3 = sb_r 101 0 [0,1]
sb_101r4 = sb_r 101 0 [0,0.5,1]
sb_101r5 = sb_r 101 0 [1]

sb_102r1, sb_102r2, sb_102r3, sb_102r4 :: IO ()
sb_102r1 = sb_r 102 0 [0,0.5,0.8,1]
sb_102r2 = sb_r 102 0 [0.1,0.2..1]
sb_102r3 = sb_r 102 0 [0,1]
sb_102r4 = sb_r 102 0 [0,0,0.5,1]


-- --------------------------------------------------------------------------
--
-- * Playing with UGens
--
-- --------------------------------------------------------------------------

-- | From hsc3 help of 'stepper'.
stepper_ex :: IO ()
stepper_ex =
    let {compose = foldl (flip (.)) id
        ;rvb z s =
            let f i = let dly = mce [rand (z `joinID` i `joinID` 'a') 0 0.5
                                    ,rand (z `joinID` i `joinID` 'b') 0 0.5]
                        in allpassN i 0.05 dly (rand i 1.5 2)
            in compose (replicate 5 f) s
        ;stpr = let {rate = mouseX KR 1.75 2.25 Exponential 0.1
                    ;clock = impulse KR rate 0
                    ;envl = decay2 clock 0.002 2.5
                    ;indx = stepper clock 0 0 15 1 0
                    ;freq = bufRdN 1 KR 10 indx Loop
                    ;ffreq = lag2 freq 0.1 + mce [0,0.3]
                    ;lfo = sinOsc KR 0.2 (mce [0,pi/2]) * 0.0024 + 0.0025
                    ;top = mix (lfPulse AR (freq * mce [1,1.5,2]) 0 0.3)
                    ;chn = [\s -> rlpf s ffreq 0.3 * envl
                           ,\s -> rlpf s ffreq 0.3 * envl
                           ,\s -> s * 0.5
                           ,\s -> combL s 1 (0.66 / rate) 2 * 0.8 + s
                           ,\s -> s + (rvb 'a' s * 0.3)
                           ,\s -> leakDC s 0.1
                           ,\s -> delayL s 0.1 lfo + s
                           ,\s -> onePole s 0.9]}
                in compose chn top
        ;stprInit =
         let n = [97.999,195.998,523.251,466.164,195.998
                 ,233.082,87.307,391.995,87.307,261.626
                 ,195.998,77.782,233.082,195.998,97.999
                 ,155.563]
         in do {_ <- async (b_alloc 10 128 1)
               ;send (b_setn 10 [(0,n)])}}
    in withSC3 $ (stprInit >> play (out 0 stpr))

-- | Simple 'sendTrig' example to schedule 's_new' message responding to @/tr@
-- messages sent back from scsynth server.
sendTrig01_ex01 :: IO ()
sendTrig01_ex01 = do
    g0 <- newStdGen
    tid <- forkIO $ withSC3 $ withNotifications $ do
        let -- trigger synth
            st01  = sendTrig tr0 1000 tcnt
            tr0   = tr1 + dust 'A' KR df
            tr1   = impulse KR 1 0
            df    = linLin (lfdNoise3 'F' KR (1/32)) (-1) 1 0.5 32
            tcnt  = pulseCount tr1 0
            -- fm sine tone synth
            sin01 = out 0 (pan2 (sinOsc AR freq phs * e * 0.08) pos 1)
            pos   = rand 'A' (-1) 1
            freq  = control KR "freq" 440
            phs   = rlpf (saw AR (freq*2.9998)) (freq*4*e) 0.8 *
                    e * rand 'B' 1 6
            e     = envGen KR 1 1 0 dur RemoveSynth esh
            esh   = envCoord [(0,0),(atk,1),(1,0)] 1 1 EnvCub
            dur   = control KR "dur" 1
            atk   = control KR "atk" 0.001
        mapM_ (async . d_recv . uncurry synthdef)
            [("sin01",sin01),("st01",st01)]
        send $ s_new "st01" (-1) AddToTail 1 []
        let pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
            octs = take 6 $ iterate (+12) 36
            degs = [0,4,7,11]
        -- looping action responding to /tr message from scsynth server.
        flip fix g0 $ \f gen -> do
            [Int32 _nid, Int32 1000, Float val] <- waitDatum "/tr"
            let pidx       = ceiling val `mod` (length pchs - 1)
                frq        = pchs !! pidx
                (patk,gen1) = first exp $ randomR (log 0.001, log 0.999) gen
                (pdur,gen2) = first exp $ randomR (log 0.09, log 9) gen1
            send $ s_new "sin01" (-1) AddToTail 1
                [("freq",frq),("atk",patk),("dur",pdur)]
            f gen2
    getChar >> killThread tid

-- | Example of 'diskOut'.
diskOut_ex01 :: IO ()
diskOut_ex01 = withSC3 $ do

    let bbl  = out 0 $ combN (sinOsc AR f0 0 * 0.04) 0.2 0.2 4
        f0   = midiCPS (f1+f2)
        f1   = lfSaw KR 0.4 0 * 24
        f2   = lfSaw KR (mce [8,7.23]) 0 * 3 + 80
        dout = diskOut (control IR "bufn" 0) (in' 2 AR 0)
        bufn :: Num a => a
        bufn = 0

    mapM_ async
        [ b_alloc bufn 65536 2
        , b_write bufn "out.wav" Wave PcmFloat (-1) 0 True
        , d_recv $ synthdef "bbl" bbl
        , d_recv $ synthdef "dout" dout
        ]

    now <- liftIO time
    sendOSC $ bundle now
        [ s_new "bbl" 2003 AddToHead 1 []
        , s_new "dout" 2004 AddAfter 2003 [("bufn",bufn)]
        ]
    sendOSC $ bundle (now+4)
        [ n_free [2003,2004], b_close bufn, b_free bufn ]

-- | Example of 'recordBuf'.
recordBuf_ex01 :: IO ()
recordBuf_ex01 = withSC3 $ do
    let -- buffer number used to record.
        bufn :: Num a => a
        bufn = 0

        -- synthdef recording audio bus 0 and 1.
        rbuf = recordBuf AR chs b lvl 0 run NoLoop tr RemoveSynth i
        chs  = mce [0,1]
        lvl  = 1
        run  = 1
        tr   = 1
        i    = in' 2 AR 0
        b    = control KR "bufn" 0

        -- sound to record
        fmt  = out 0 $
               formant AR
               (xLine KR (mce [400,200]) (mce [1000,2000]) 4 DoNothing)
               2000 880 * 0.125

    mapM_ async
        [ b_alloc bufn (48000 * 4) 2
        , d_recv $ synthdef "rbuf" rbuf
        , d_recv $ synthdef "fmt" fmt
        ]

    now <- liftIO time
    sendOSC $ bundle immediately
        [ s_new "fmt" 2003 AddToHead 1 []
        , s_new "rbuf" 2004 AddAfter 2003 [("bufn",bufn)]
        ]
    sendOSC $ bundle (now+4)
        [ n_free [2003]
        , b_write bufn "rbuf.wav"  Wave PcmFloat (-1) 0 False
        , b_free bufn
        ]

-- | Recording and playing back with 'localBuf'.
recordBuf_ex02 :: Transport m => UGen -> m ()
recordBuf_ex02 rsig = do
    let buf   = localBuf 'z' 96000 1
        rbuf  = recordBuf AR buf 0 1 0 1 Loop trec DoNothing rsig
        trec  = impulse KR 0.5 0
        pbuf  = playBuf 1 AR buf rate rst pos Loop DoNothing
        rate  = bufRateScale KR buf
        rst   = coinGate 'd' rprob trst
        rprob = mouseY KR 0 1 Linear 0.1
        pos   = 12000 * tIRand 'p' 0 7 (coinGate 'p' 0.5 rst)
        trst  = impulse KR rstf 0
        rstf  = mouseX KR 1 32 Exponential 0.1
        osig  = mrg [out 0 (mce2 pbuf pbuf), rbuf, maxLocalBufs 1]
    play osig

recordBuf_ex02_play01 :: IO ()
recordBuf_ex02_play01 =
    let sig = resonz (whiteNoise 'W' AR) f rq * decay t 0.8
        f   = tExpRand 'f' 20 12000 t
        rq  = squared (lfdNoise3 'a' KR 1) + 0.1
        t   = dust 'd' KR 8
    in  withSC3 $ recordBuf_ex02 sig

recordBuf_ex02_play02 :: FilePath -> IO ()
recordBuf_ex02_play02 file = withSC3 $ do
    let bufn :: Num a => a
        bufn = 13
        sig  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
    _ <- async $ b_allocRead bufn file 0 0
    recordBuf_ex02 sig

recordBuf_ex02_play03 :: IO ()
recordBuf_ex02_play03 =
    let sig    = sum [fsig i|i<-[0..7::Int]]
        fsig i = pan2 (sinOsc AR frq 0 * aenv0 * 0.03) pos0 1
          where
            frq   = midiCPS (tChoose i tr0 (mce frqs) + ofst)
            aenv0 = envGen KR tr0 1 0 (tExpRand i 0.1 4 tr0) DoNothing $
                    Envelope [0,1,0] [atk,1-atk] [EnvCub] (Just 0) Nothing
            atk   = tExpRand i 0.001 0.999 tr0
            pos0  = envGen KR tr0 1 0 (tExpRand i 0.01 4 tr0) DoNothing $
                    Envelope [pstrt, pstrt, pstrt*(-1)] [0.001,1] [EnvLin]
                    (Just 0) Nothing
            pstrt = tRand i (-1) 1 tr0
        frqs   = foldr (\o ps -> map (+o) degs ++ ps) [] octs
        octs   = take 10 $ iterate (+12) 24
        degs   = [0,2,5,7,10]
        ofst   = tChoose 'o' (coinGate 'O' (1/128) tr0) (mce degs)
        tr0    = dust 'A' KR 2
        buf    = localBuf 'y' 96000 2
        rbuf   = recordBuf AR buf 0 1 plvl run Loop trec DoNothing sig
        plvl   = toggleFF (coinGate 't' (1/32) tr0)
        run    = lfClipNoise 'c' KR (1/16)
        trec   = impulse KR 0.5 0
        osig   = playBuf 2 AR buf rate rst pos Loop DoNothing
        rate   = bufRateScale KR buf
        rst    = coinGate 'd' rprb trst
        rprb   = mouseY KR 0 1 Linear 0.1
        trst   = impulse KR rstf 0
        rstf   = mouseX KR 1 32 Exponential 0.1
        pos    = 12000 * tIRand 'p' 0 7 (coinGate 'p' 0.5 rst)
    in  audition $ mrg [out 0 osig, rbuf, maxLocalBufs 1]

-- | Example of 'grainIn'.
grainIn_ex01 :: IO ()
grainIn_ex01 =
    let i   = pinkNoise 'a' AR
        pan = mouseX KR (-0.5) 0.5 Linear 0.1
        tf  = mouseY KR 5 25 Linear 0.1
        tr  = impulse KR tf 0
        g   = grainIn 2 tr 0.1 i pan (-1) 512 * 0.1
    in  audition $ out 0 g

-- | Example for 'AddReplace' add action. This Haskell action adds node 1000
-- to tail of node 1.
replace_ex01_seq01 :: IO ()
replace_ex01_seq01 = withSC3 $ do
    let sdef = out 0 (sinOsc AR 440 0 * 0.2 * decay (dust 'a' KR 8) 0.5)
    _ <- async $ d_recv $ synthdef "replace_ex01_seq01" sdef
    send $ s_new "replace_ex01_seq01" 1000 AddToTail 1 []

-- | Replaces node 1000 with node 1001. 'AddReplace' add action will not work if
-- target node id is absent in scsynth.
replace_ex01_seq02 :: IO ()
replace_ex01_seq02 = withSC3 $ do
    let sdef = out 0 (sinOsc AR 880 0 * 0.2 * decay (dust 'a' KR 32) 0.2)
    _ <- async $ d_recv $ synthdef "replace_ex01_seq02" sdef
    send $ s_new "replace_ex01_seq02" 1001 AddReplace 1000 []

-- | Replacing node 1001 with node 1000.
replace_ex01_seq03 :: IO ()
replace_ex01_seq03 = withSC3 $ do
    send $ s_new "replace_ex01_seq01" 1000 AddReplace 1001 []

-- | Playing with 'sendReply'.
sendReply_ex01 :: IO ()
sendReply_ex01 = do
    withSC3 $ do
        let sr     = sendReply tr rid "/sendReply_ex01" [freq,amp,dur]
            freq   = midiCPS (select (tIRand 'f' 0 79 tr) pchs)
            pchs   = mce $ take 80 $
                     foldr (\o acc -> map (+o) degs ++ acc) [] octs
            degs   = [0,2,thrd,7,svnth]
            thrd   = tChoose 't' (coinGate '3' (1/128) tr) (mce [3,4])
            svnth  = tChoose 's' (coinGate '7' (1/127) tr) (mce [10,11])
            octs   = iterate (+12) 36
            amp    = tExpRand 'a' 0.1 0.3 tr
            dur    = tExpRand 'd' 0.1 8 tr
            atk    = tRand 'k' 0.1 0.9 tr
            pan    = tRand 'p' (-1) 1 tr
            dcy    = tExpRand 'y' 0.008 4 tr
            tr     = dust 't' KR dfrq + impulse KR ifrq 0
            dfrq   = linExp (lfdNoise3 'D' KR (1/13) + 2) 1 3 (1/8) 32
            ifrq   = linExp (lfdNoise1 'I' KR (1/9) + 2) 1 3 (1/4) 16
            rid    = pulseCount tr 0
            npoly  = 32
            osig   = out 0 (sum $ map fsig [0..npoly-1])
            fsig i = pan2 (allpassC sigi 0.2 0.2 dcyi) pani 1
              where
                sigi  = sinOsc AR freqi phs * ampi * aenvi
                tri   = pulseDivider tr npoly i
                freqi = gate freq tri
                ampi  = gate amp tri
                duri  = gate dur tri
                pani  = gate pan tri
                aenvi = envGen KR tri 1 0 duri DoNothing $ Envelope
                        [0,1,0] [atki,1-atki] [EnvCub] Nothing Nothing
                atki  = gate atk tri
                dcyi  = gate dcy tri `lag` 0.2
                phs   = lfSaw AR (freq * 1.00003) 0 * idx
                idx   = linExp (lfdNoise1 i KR (1/3) + 2) 1 3 0.5 6
            gout = mrg [sr, osig]
        send $ withCM (d_recv $ synthdef "sr01" gout)
            (s_new "sr01" (-1) AddToTail 1 [])

    -- tid <- forkIO $
    --        let go = do
    --                [Int32 nid,Int32 rid,Float freq,Float amp,Float dur] <-
    --                    waitDatum "/sendReply_ex01"
    --                liftIO $ putStrLn $ unwords
    --                    ["rid:  " ++ show rid
    --                    ,"nid:  " ++ show nid
    --                    ,"freq: " ++ show freq
    --                    ,"amp:  " ++ show amp
    --                    ,"dur:  " ++ show dur ]
    --                go
    --        in  withSC3 $ withNotification go
    -- getChar >> killThread tid >> withSC3 (send $ n_free [-1])

-- | Last example from /Order-of-execution/ SC3 help.
ooe_ex01_0 :: IO ()
ooe_ex01_0 = withSC3 $ do
    let ifreq = out 0 (sinOsc AR (in' 1 KR bus) 0 * 0.5)
        bus   = control KR "bus" 0
        ofreq = out bus (sinOsc KR 1 0 * (freq/40) + freq)
        freq  = control KR "freq" 400
    mapM_ (async . d_recv . uncurry synthdef) [("ifreq",ifreq),("ofreq",ofreq)]

-- | Send new synths.
ooe_ex01_1 :: IO ()
ooe_ex01_1 = withSC3 $
    sendOSC $ bundle immediately
    [ s_new "ofreq" 1000 AddToTail 1 [("bus",300)]
    , s_new "ifreq" 1001 AddBefore 1000 [("bus",300)]
    , s_new "ofreq" 1002 AddToHead 1 [("bus",300),("freq",800)]
    ]

-- | Maps new control rate bus for frequency of synth making sound.
ooe_ex01_2 :: IO ()
ooe_ex01_2 = withSC3 $ do
    sendOSC $ bundle immediately
        [ n_set 1000 [("bus",301)]
        , n_set 1001 [("bus",301)] ]

-- | Brings back the control rate bus used by node 1002 for output.
ooe_ex01_3 :: IO ()
ooe_ex01_3 = withSC3 $ do
    sendOSC $ bundle immediately
        [ n_set 1000 [("bus",300)]
        , n_set 1001 [("bus",300)] ]

n_map_negative_ex01 :: IO ()
n_map_negative_ex01 = withSC3 $ do
    let def = out 0 (sinOsc AR freq 0 * 0.3)
        freq = control KR "freq" 440
        pause2sec = liftIO $ threadDelay (1000000 * 2)
    play def
    pause2sec
    sendOSC $ bundle immediately
        [ n_map (-1) [("freq",100)]
        , c_set [(100,330)] ]
    pause2sec
    send $ n_map (-1) [("freq",-1)]
    pause2sec
    send $ n_free [-1]

-- | Sends @/s_new@ message with string value used for mapping parameter to
-- control bus 100.
s_new_with_c100 :: IO ()
s_new_with_c100 = withSC3 $ do
    let def1 = out (k "out" 0) (sinOsc AR (k "freq" 440) 0 * 0.2)
        def2 = out (k "out" 0)
               (lfClipNoise 'd' KR (k "freq" 3) * (k "mul" 1) + (k "add" 0))
        def3 = replaceOut (k "out" 0) (combC (a "in" 0) 0.2 dlt dcy)
        dcy  = lfCub KR (k "dcyf" 1) 0 * 3 + 3
        dlt  = lfCub KR (k "dltf" 1) 0 * 0.5 + 0.6
        k    = control KR
        a    = control AR
    mapM_ (async . d_recv)
        [synthdef "def1" def1, synthdef "def2" def2, synthdef "def3" def3]
    sendOSC $ bundle immediately
        [ s_new' "def1" (-1) AddToTail 1
          ["freq":<-100]
        , s_new' "def2" (-1) AddToHead 1
          ["out":=100,"mul":=800,"add":=850,"freq":=6]
        , s_new' "def1" (-1) AddToTail 1
          ["out":=1]
        , n_set' (-1) ["freq":<-101]
        , s_new' "def2" (-1) AddToHead 1
          ["out":=101,"mul":=800,"add":=850,"freq":=6.025]
        , s_new' "def3" (-1) AddToTail 1
          ["in":<=0,"out":=0,"dcyf":=1/16,"dltf":=1/13]
        , s_new' "def3" (-1) AddToTail 1
          ["in":<=1,"out":=1,"dcyf":=1/17,"dltf":=1/14]
        ]

-- Currently hsc3 does not have function to send @/s_new@ message with string
-- data type.

-- | Variant of 'n_set' taing 'SynthParam' for parameter values.
n_set' :: Int -> [SynthParam] -> Message
n_set' nid pvs =
    message "/n_set" (int32 nid : foldr accSynthParams [] pvs)

-- | Variant of 's_new' taing 'SynthParam' for parameter values.
s_new' :: String -> Int -> AddAction -> Int -> [SynthParam] -> Message
s_new' name nid aa tid pvs =
    message "/s_new"
    (string name : int32 nid : int32 (fromEnum aa) : int32 tid :
     foldr accSynthParams [] pvs)

-- | Accumulate 'SynthParam' for @/s_new@ and @/n_set@ messages.
accSynthParams :: SynthParam -> [Datum] -> [Datum]
accSynthParams pv acc = case pv of
    p :=  v -> string p : float v : acc
    p :<- c -> string p : string ('c':show c) : acc
    p :<= a -> string p : string ('a':show a) : acc


-- Replacing with target node id `-1' will show FAILURE IN SERVER.
--
-- Following two actions show an attempt to replace node ID
-- 1000. 'replace_ex01b' is sending s_noid [1000] message and s_new with
-- AddReplace add action taking (-1) as target node id, node id of new node is
-- 1000.

replace_ex01a :: IO ()
replace_ex01a = withSC3 $ do
    let def1 = out 0 (sinOsc AR 440 0 * 0.3 * aenv)
        aenv = envGen KR tr 1 0 0.3 DoNothing (envPerc 0.1 1)
        tr   = impulse KR 1 0
    send $
        withCM (d_recv (synthdef "def1" def1)) $
        s_new "def1" 1000 AddToHead 1 []

replace_ex01b :: IO ()
replace_ex01b = withSC3 $ do
    let def2 = out 0 (saw AR 220 * 0.3 * aenv)
        aenv = decay (impulse KR 1 0) 0.3
    send $ withCM (d_recv (synthdef "def2" def2)) $
        bundle immediately
        [ s_noid [1000]
        , s_new "def2" 1001 AddReplace (-1) [] ]

-- Sending bundled message with overwriting anonymous synthdef.
--
-- Synthdef named /Anonymous/ is sent with @/d_recv@ and @/s_new@ completion
-- message. Then soon follows another @/d_recv@ message and @/s_new@, with using
-- /Anonymous/ as synthdef name.
--
overwriting_anonymous_ex01 :: IO ()
overwriting_anonymous_ex01 = withSC3 $ do
    let def1 = out 0 (sinOsc AR 440 0 * e)
        def2 = out 1 (saw AR 110 * e)
        def3 = out 0 (pulse AR 220 0.5 * e)
        def4 = out 1 (resonz (whiteNoise 'W' AR) 2000 0.6 * e)
        e    = envGen KR 1 0.1 0 3 RemoveSynth (envPerc 0.1 1)
        dr u = d_recv (synthdef "Anonymous" u)
        sn   = s_new "Anonymous" (-1) AddToTail 1 []
    sendOSC $ bundle immediately
        [ withCM (dr def1) sn
        , withCM (dr def2) sn
        , withCM (dr def3) sn
        , withCM (dr def4) sn ]

-- Sending @/d_free@ message while synthdef is running.
-- Will not free the running node with freed synthdef.
d_free_running_ex01 :: IO ()
d_free_running_ex01 = withSC3 $ do
    let def1 = out 0 (sinOsc AR 440 0 *
                      decay (dust 'a' KR 8 * 0.3) (1/9))
    send $ withCM
        (d_recv (synthdef "def1" def1))
        (s_new "def1" (-1) AddToHead 1 [])
    liftIO $ threadDelay 2000000
    send $ d_free ["def1"]
    liftIO $ putStrLn "sent /d_free"
    liftIO $ threadDelay 2000000
    send $ n_free [-1]
    liftIO $ putStrLn "sent /n_free"
