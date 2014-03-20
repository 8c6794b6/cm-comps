{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with a pile of oscillators, take 7.
--
module Sound.Study.ForAPileOfOscillators.A007 where

import Control.Concurrent
  (threadDelay, forkIO, killThread, newEmptyMVar, MVar, readMVar)
import Control.Monad (forM_)
import Data.Traversable (sequenceA)
import Data.List ((\\))
import Data.Map (Map, (!))
import System.Random
import qualified Data.Map as M

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Study.ForAPileOfOscillators.Common

main :: IO ()
main = putStrLn "Not yet."

setup_a007 :: Transport m => m Message
setup_a007 = do
  liftIO $ mapM_ (uncurry writeSynthdef)
    [("oc71",oc71),("oc72",oc72),("pc71",pc71),("pc72",pc72)
    ,("ac71",ac71),("ac72",ac72),("ac73",ac73)
    ,("lmtr",lmtr)]
  reloadSynthdef
  addNode 0 a007Nodes
  async $ b_free pitchBuf
  async $ b_alloc pitchBuf 256 1

a007Nodes :: SCNode
a007Nodes =
  g 1
    [g 11
      [s pc72Id "pc72" ["lagt":=100e-3]
      ,s ac71Id "ac71" []
      ,s ac72Id "ac72" []
      ,s ac73Id "ac73" []
      ,s oc71Id "oc71" ["pan":=0.9,"dmax":=800e-3]
      ,s oc72Id "oc72" ["pan":=0.8,"dmax":=1200e-3]]
    ,g 12 oscs
    ,g 13 [s lmtrId "lmtr" []]]
  where
    g = Group
    s = Synth

oc71Id = 1100
oc72Id = 1101
pc71Id = 1102
pc72Id = 1103
ac71Id = 1104
ac72Id = 1105
ac73Id = 1106
lmtrId = 1301

bpm = 120

setFreqBus =
  send $ c_setn [(head fBusses, take numOsc [100,200..])]

nsetP nid ps = do
  ms <- act $ runLIO $ sequenceA $ M.fromList ps
  forM_ ms $ \m -> do
    act $ time >>= \t -> withSC3 $
      sendOSC $ Bundle (t+0.1) [n_set nid (M.assocs m)]
    rest (m!"del")
    pauseHere

goAtk p i = do
  ms <- liftIO . runLIO . sequenceA $ p
  forM_ ms $ \m -> do
    now <- liftIO time
    sendOSC $ Bundle (now+0.1) [n_set i (M.assocs m)]
    liftIO $ threadDelay (floor $ (60/bpm) * 1e6 * m ! "del")

pAtk1 = M.fromList
  [("t_trig",pforever (prand 1 [1,1,1,1,0]))
  ,("edgey",pcycle [2e-3, prand (pirange 5 13) [998e-3,993e-3,997e-3]])
  ,("del",pforever (prand 1 [1/2, 1/4, 1/4, 1/4]))
  ,("mamp",pforever (pdrange 15e-3 3e-2))]

pAtk2 = M.fromList
  [("t_trig",pforever 1)
  ,("edgey",pforever (prand 1 [999e-3,800e-3,750e-3]))
  ,("del",pforever (pseq 1 [1,1/2,1/2, 1,1/2,1/2]))
  ,("mamp",pcycle [0.03, prand 2 [pdrange 1e-2 1.5e-2]
                  ,0.04, prand 2 [pdrange 1e-2 1.5e-2]])]

bank1 = filter even oscIds
bank2 = filter odd oscIds

oc71 = oc7x bank1
oc72 = oc7x bank2
oc7x oids =
  oc7x' oids ("t_trig"=:0) ("mamp"=:0.03) ("edgey"=:1) ("pan"=:0.5) ("dmax"=:800e-3)
oc7x' oids t_trig mamp edgey pan dmax = mrg $ concatMap mkO oids
  where
    mkO i = [out (ampBus i) amp, out (panBus i) pan']
      where
        amp = envGen KR t_trig mamp 0 dur DoNothing $
              Envelope [0,0,1,0] [0,1-edgey,edgey] [EnvNum (-10)]
              (Just (-1)) (Just 0)
        dur = tExpRand i 5e-3 dmax t_trig
        pan' = tRand i (-pan) pan t_trig

ac71 = ac71' ("amp"=:0)
ac71' amp = mrg $ map mkO oscIds
  where
    mkO i = out (ampBus i) amp

ac72 = ac72' ("amp"=:0.003) ("freq"=:1) ("edgey"=:0.5) ("dmax"=:280e-3)
ac72' amp freq edgey dmax = mrg $ map mkO oscIds
  where
    mkO i = out (ampBus i) sig
      where
        sig = envGen KR tr amp 0 dur DoNothing $
              Envelope [0,0,1,0] [0,1-edgey,edgey] [EnvNum (-13)]
              (Just (-1)) (Just 0)
        tr = dust i KR freq
        dur = tExpRand i 1e-4 dmax tr

ac73 = ac73' ("amp"=:0.003) ("freq"=:1) ("edgey"=:0.5) ("dmax"=:1000e-1)
ac73' amp freq edgey dmax = mrg $ map mkO oscIds
  where
    mkO i = out (ampBus i) sig
      where
        sig = envGen KR tr amp 0 dur DoNothing $
              Envelope [0,0,1,0] [0,1-edgey,edgey] [EnvNum (-13)]
              (Just (-1)) (Just 0)
        dur = tExpRand i 1e-4 dmax tr
        tr = impulse KR freq phase
        phase = rand i 0 1

pc71 = pc71' oscIds
pc71' oids = mrg $ map mkO oids
  where
    mkO i = out (freqBus i) freq
      where
        freq = index pitchBuf (("idx_"++show i)=:i')
        i' = fromIntegral (i `mod` numOsc)

pc72 = pc72' oscIds ("t_trig"=:0) ("lagt"=:20e-3)
pc72' oids t_trig lagt = mrg $ map mkO oids
  where
    mkO i = out (freqBus i) freq
      where
        freq = envGen KR t_trig 1 0 1 DoNothing $
               Envelope [0,0,index pitchBuf (("idx_"++show i)=:i')]
               [0,lagt] [EnvNum (-3)] (Just (-1)) (Just 0)
        i' = fromIntegral (i `mod` numOsc)

lmtr = replaceOut 0 $ mce [sig0, sig1]
  where
    sig0 = lmt $ in' 1 AR 0
    sig1 = lmt $ in' 1 AR 1
    lmt x = limiter x 1 0.2

pitchBuf = 10

setPitchBuf vs = withSC3 $ do
  async $ b_free pitchBuf
  async $ b_alloc pitchBuf 256 1
  send $ b_setn pitchBuf [(0,vs)]

pf1 n i = setPitchBuf $ map midiCPS $
          take numOsc $ cycle $ takeWhile (< 127) $ iterate (+n) i

rec3 :: MVar Int -> MVar Int -> Map Int Double -> Map Int Double -> Double -> Act ()
rec3 mvChg mvThr p1 p2 f = do
  now <- time
  act $ withSC3 $
    sendOSC $ Bundle (now+0.1)
    [b_setn pitchBuf [(0,map midiCPS $ M.elems p1)]
    ,n_set pc72Id [("t_trig",1)]]
  rt <- act $ runLIO $ prand 1 [0.25,0.5,1,2,4]
  chg <- act $ readMVar mvChg
  thr <- act $ readMVar mvThr
  rest $ head rt
  pauseHere
  let diff = length $ M.elems p1 \\ M.elems p2
  if diff <= thr
    then do
      shift <- act $ runLIO $ prand 1 [2,5,7,10]
      mjr <- act $ runLIO $ prand 1 [pint 1, pint 0]
      let shift' = head shift
          f' = fromIntegral $ (floor $ f+shift') `mod` 12
      fs <- recSeeds f' (head mjr)
      -- act $ putStrLn $ "f': " ++ show f' ++ " shift': " ++ show shift'
      rec3 mvChg mvThr p2 (M.fromList $ zip [0..255] fs) f'
    else do
      idxs <- act $ runLIO $ prand (pint thr) [pirange 0 (pint (numOsc-1))]
      let p1' = foldr (\k m -> M.update (const $ M.lookup k p2) k m) p1 idxs
      rec3 mvChg mvThr p1' p2 f

recSeeds :: Double -> Int -> Act [Double]
recSeeds shift maj =
  act $ runLIO $ pcycle $ map pdouble $
    takeWhile (< 138) $ dropWhile (< 20) $
    zipWith (+) (cycle ps) $
    concatMap (replicate $ length ps) (map (+shift) [0,12..])
  where
    -- ps = [0,3,5,7,10]
    ps = if maj == 1 then psMjr else psMnr
    psMjr = [0,4,7]
    psMnr = [0,3,7]

goRec3 :: MVar Int -> MVar Int -> Act ()
goRec3 mvChg mvThr = do
  f1 <- recSeeds 0 1 -- True
  f2 <- recSeeds 7 1 -- True
  let p1 = M.fromList $ zip [0..255] f1
      p2 = M.fromList $ zip [0..255] f2
  rec3 mvChg mvThr p1 p2 0

diskOut :: UGen -> UGen -> UGen
diskOut b s = mkOsc AR "DiskOut" [b, s] 1
