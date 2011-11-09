{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    send fd $ n_set 1001 [("val",0.1),("dur",16)]
    threadDelay (14 * 10 ^ (6 :: Int))
    send fd $ n_set 1000 [("val",0),("dur",2)]
    threadDelay (2 * 10 ^ (6 :: Int))
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
      bth03Nd =
        syn "bth03"
          ["out"*=2, "edur"*<- prmv edur "out"]
      mst =
        syn "bthmst"
          [ "out"*=0
          , "in1"*=2, "in1_amp"*<-prmv in1_amp "out"
          , "in2"*=4, "in2_amp"*= 0.2
          , "in3"*=6, "in3_amp"*= 0.2 ]
  in  grp 0
        [ grp 1 [ in1_amp, edur ]
        , grp 2 [ bth03Nd ]
        , grp 3 [ mst ]]

------------------------------------------------------------------------------
-- Actions

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
mkMsg :: Int -> Int -> IO (Int,[OSC])
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
            | ticks >= 300 -> [sperc]
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
    scream = n_set 1000 [("val",0.8),("dur",48)]
    edur = n_set 1001 [("val",1e-3),("dur",24)]
    sperc =
      s_new "bth01" (-1) AddToTail 2
        [ ("freq", 40.013**n')
        , ("amp",0.4)
        , ("out",6)
        , ("pan",0.5) ]
    n' = fromIntegral n
