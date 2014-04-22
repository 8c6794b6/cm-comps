{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Miscellaneous functions.

-}
module Sound.Study.ForUserInterfaces.Misc where

import Control.Arrow (first)
import Control.Concurrent (forkIO, killThread)
import Data.Function (fix)
import System.Random (newStdGen, randomR, randomRs)

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID

import qualified Sound.Study.ForUserInterfaces.GUI01 as GUI01


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
diskOut_ex01 = withSC3 $ withNotifications $ do

    let bbl  = out 0 $ combN (sinOsc AR f0 0 * 0.04) 0.2 0.2 4
        f0   = midiCPS (f1+f2)
        f1   = lfSaw KR 0.4 0 * 24
        f2   = lfSaw KR (mce [8,7.23]) 0 * 3 + 80
        dout = diskOut (control IR "bufn" 0) (in' 2 AR 0)
        bufn :: Num a => a
        bufn = 0

    mapM_ async
        [ b_alloc bufn 1024 2
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

grainIn_ex01 :: IO ()
grainIn_ex01 =
    let i   = pinkNoise 'a' AR
        pan = mouseX KR (-0.5) 0.5 Linear 0.1
        tf  = mouseY KR 5 25 Linear 0.1
        tr  = impulse KR tf 0
        g   = grainIn 2 tr 0.1 i pan (-1) 512 * 0.1
    in  audition $ out 0 g
