{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Main entry point for /birth/.
-}
module Sound.Bogus.Birth where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Word

import Sound.OpenSoundControl
import Sound.SC3 hiding (pause)
import Sound.SC3.Lepton

import Sound.Bogus.Birth.Synthdefs
import Sound.Bogus.Common

main :: IO ()
main = do
  withSC3 $ \fd -> do
    reset fd
    load_ugens fd
    patchNode (nodify birthNd) fd
  n <- runSC3 (fib 13)
  n `seq` withSC3 $ \fd -> do
    send fd $ bundle immediately
      [ s_new "bth07" (-1) AddToTail 2 [("out",6),("amp",4.8)]
      , n_set 1001 [("val",1e-1),("dur",1.5)]
      , n_free [2100] ]
    threadDelay (3 * 10 ^ (6 :: Int))
    send fd $ n_set 1001 [("val",1e-4),("dur",10)]
    threadDelay (10 * 10 ^ (6 :: Int))
    send fd $ n_set 1000 [("val",0),("dur",4.75)]
    threadDelay (7 * 10 ^ (6 :: Int))
    reset fd

load_ugens :: Transport t => t -> IO ()
load_ugens fd = mapM_ (send fd . d_recv . uncurry synthdef) ugs

------------------------------------------------------------------------------
-- Node

birthNd :: Nd
birthNd =
  let in1_amp =
        syn' 1000 "bthlgc"
          ["out"*=100, "val"*=0, "dur"*=0]
      edur =
        syn' 1001 "bthlgc"
          ["out"*=101, "val"*=1e-2, "dur"*=1]
      in1_wet =
        syn' 1002 "bthlgc"
          ["out"*=102, "val"*=0, "dur"*=2]
      bth03Nd =
        syn "bth03"
          ["out"*=2, "edur"*<- prmv edur "out"]
      rev =
        syn "bthrev"
          [ "out"*=2, "in"*=2, "wet"*<-prmv in1_wet "out"]
      mst =
        syn "bthmst"
          [ "out"*=0
          , "in1"*=2, "in1_amp"*<-prmv in1_amp "out"
          , "in2"*=4, "in2_amp"*= 0.25
          , "in3"*=6, "in3_amp"*= 0.25 ]
  in  grp 0
        [ grp 1 [ in1_amp, in1_wet, edur ]
        , grp 2 [ bth03Nd ]
        , grp 3 [ rev, mst ]]

------------------------------------------------------------------------------
-- Actions

-- | Fibonacci sequence sending message as side effect.
fib :: Int -> SC3 Int
fib n
  | n == 0 = tone 0 >> return 1
  | n == 1 = tone 1 >> return 1
  | otherwise = do
    a <- fib (n-2)
    b <- fib (n-1)
    tone n
    return $ a + b

-- | Unit duration.
dt :: Word64
dt = 380 * 10 ^ (6::Word64)

-- | Make sound with given Int, and pause for one beat.
tone :: Int -> SC3 ()
tone n = do
  fd  <- ask
  (ticks,now) <- get
  (nid, msg) <- liftIO $ mkMsg ticks n
  modify $ \(ts,t0) -> (ts+1,t0+dt)
  liftIO $ do
    send fd $ bundle (NTPi (now + dt)) msg
    waitUntil fd "/n_go" nid

-- | Make OSC messages. Pause when second arg is 0.
mkMsg ::
  Int    -- ^ Current number of ticks.
  -> Int -- ^ N passed from seeding logic (e.g. fibonacci sequence).
  -> IO (Int,[OSC])
mkMsg ticks n
  | n == 0    = do
    nid <- newNid
    return (nid, [s_new "rest" nid AddToTail 1 []])
  | otherwise = do
    -- liftIO $ print ticks
    nid <- newNid
    let os = case ticks of
          _ | ticks == 50  -> [scream]
            | ticks == 151 -> [edur]
            | ticks == 289 -> [sweeper]
            | ticks >= 300 -> mkperc ticks
            | otherwise    -> []
    return (nid,msg nid:os)
  where
    msg i =
      s_new "bth02" i AddToHead 2
        [ ("freq", n' * 88)
        , ("amp", 0.08 + (n'*0.01))
        , ("dur", n' * 1.2)
        , ("out", 4)
        , ("pan", 0.5 - (fromIntegral (n `mod` 4) * (1/3))) ]
    scream = n_set 1000 [("val",1),("dur",48)]
    edur = n_set 1001 [("val",1e-3),("dur",24)]
    -- `fib 13` has 753 ticks.
    mkperc t
      | t == 749                                = [sp1,sp2,edur_change,wet]
      | t >= 400 && (t `mod` 8 == 0) = [sp1,sp2]
      | t >= 512 && t < 640 && (t `mod` 8 == 4) = [sp1]
      | t >= 578 && (t `mod` 8 == 4)            = [sp1,sp2]
      | t >= 612 && t < 712 && (t `mod` 8 == 2) = [sp1,sp2]
      | t >= 712 && even t                      = [sp1,sp2]
      | otherwise                               = [sp1]
    sp1 =
      s_new "bth04" (-1) AddToTail 2
        [ ("mfreq", ((n' * 110) + (n'*18.32)))
        , ("freq", n' * 1180)
        , ("amp",0.7)
        , ("dur",0.08 + (n'*0.01))
        , ("out",6)
        , ("pan",1 - (fromIntegral (n `mod` 6) * (2/5))) ]
    sp2 =
      s_new "bth05" (-1) AddToTail 1
        [ ("freq1", 1080.317 + (n'*3.432))
        , ("freq2", 2003.982 + (n'*1.991))
        , ("freq3", 8830.003 + (n'*13.32))
        , ("amp", 0.15 + (n'*0.02) + (fromIntegral ticks * 0.001))
        , ("dur", 0.31)
        , ("out", 6)
        , ("pan", 0.04) ]
    sweeper =
      s_new "bth06" 2100 AddToTail 2
        [("edur",60),("amp",0.8),("out",6)]
    edur_change =
      n_set 1001 [("val",0.1),("dur",3)]
    wet = n_set 1002 [("val",1), ("dur",12)]
    n' = fromIntegral n

------------------------------------------------------------------------------
-- Misc

fib_plain :: (Monad m, MonadState Int m, MonadIO m) => Int -> m Int
fib_plain n
  | n == 0    = modify (+1) >> return 1
  | n == 1    = modify (+1) >> return 1
  | otherwise = do
    a <- fib_plain (n-2)
    b <- fib_plain (n-1)
    i <- get
    liftIO $ print i
    put (i+1)
    return $ a + b

bth05_msg :: Double -> Double -> Double -> IO ()
bth05_msg f1 f2 f3 = withSC3 $ \fd ->
  send fd $ s_new "bth05" (-1) AddToTail 1
    [ ("freq1", f1)
    , ("freq2", f2)
    , ("freq3", f3)
    , ("amp", 0.8 + (3*0.02))
    , ("dur", 0.31)
    , ("out", 0)
    , ("pan", 0.24) ]

bthrev_msg :: IO ()
bthrev_msg = withSC3 $ \fd ->
  let n = grp 0
          [ grp 1
            [ syn "bth03" ["out"*=0]
            , syn "bthrev" ["in"*=0, "out"*=0 ]]]
  in  patchNode (nodify n) fd
