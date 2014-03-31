{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Miscellaneous functions.

-}
module Sound.Study.ForUserInterfaces.Misc where

import Data.Function (fix)
import System.Random (newStdGen, randomRs)

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID

import Sound.Study.ForUserInterfaces.GUI01


-- --------------------------------------------------------------------------
--
-- Scratches written while working on GUI01
--
-- --------------------------------------------------------------------------


-- Change "len" in group 1.
play_percs :: IO ()
play_percs = withSC3 $ do
    mapM_ (async . d_recv . uncurry synthdef)
        [("rbufrd01",rbufrd01)
        ,("bd01",bd01), ("hat01",hat01)
        ]
    sendOSC $ bundle immediately
        [ s_new "trig00" 998 AddToHead 1 [("out",101),("beat",4),("bpm",128)]
        , s_new "rbufrd01" 2000  AddToTail 1
          [("out",102),("bufn",100),("len",16)]
        , n_map 2000 [("tr0",101)]
        , s_new "bd01" 2001 AddToTail 1
          [("out",0),("freq",69),("dur",0.18)]
        , n_map 2001 [("t_tr0",102)]
        , s_new "rbufrd01" 2002 AddToTail 1
          [("out",103),("bufn",101)]
        , n_map 2002 [("tr0",101)]
        , s_new "hat01" 2003 AddToTail 1
          [("out",0)]
        , n_map 2003 [("t_tr0",103)]
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
go_sendTrig01 :: IO ()
go_sendTrig01 = withSC3 $ withNotifications $ do
    let st01  = sendTrig tr0 1000 tcnt
        tr0   = tr1 + dust 'A' KR df
        tr1   = impulse KR 1 0
        df    = linLin (lfdNoise3 'F' KR (1/32)) (-1) 1 0.5 32
        tcnt  = pulseCount tr1 0
        sin01 = out 0 (pan2 (sinOsc AR freq 0 * e) pos 1)
        pos   = rand 'A' (-1) 1
        freq  = midiCPS $ select idx (mce pchs)
        idx   = iRand 'I' 0 (constant $ length pchs)
        pchs  = foldr (\o acc -> map (+o) degs ++ acc) [] octs
        octs  = take 4 $ iterate (+12) 48
        degs  = [0,4,7,11]
        e     = envGen KR 1 0.1 0 dur RemoveSynth esh
        esh   = envCoord [(0,0),(atk,1),(1,0)] 1 1 EnvCub
        dur   = expRand 'd' 0.3 3
        atk   = expRand 'a' 0.001 0.999
    _ <- mapM_ (async . d_recv . uncurry synthdef)
         [("sin01",sin01),("st01",st01)]
    send $ s_new "st01" (-1) AddToTail 1 []
    fix $ \f -> do
        [Int32 _nid, Int32 _tid, Float _val] <- waitDatum "/tr"
        -- print _val
        send $ s_new "sin01" (-1) AddToTail 1 []
        f
    -- getChar >> killThread tid
