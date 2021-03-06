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
-- Playing with a pile of oscillators, take 4.
--
module Sound.Study.ForAPileOfOscillators.A004 where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.Traversable (sequenceA)
import qualified Data.Map as M

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI
import Sound.SC3.Tree

import Sound.Study.ForAPileOfOscillators.Common

main = withSC3 $
  treeToGui (Group 0 ([ac3Node]++smasterNode)) hints'
  where
    hints' = foldr M.union hints [h1,h2]
    h1 = M.fromList $ map (\x -> (x,h1')) ["ac2_1","ac2_2","ac2_3","ac2_4"]
    h1' = [ParamRange "freq" 5 8000
          ,ParamRange "amp" 0 0.2
          ,ParamRange "pan" (-1) 1]
    h2 = M.fromList
      [("ac3_1",
        [ParamRange "fc" 0 8000
        ,ParamRange "vc" 0 0.2])]

writeA004Score :: FilePath -> IO ()
writeA004Score path = do
  ps <- runLIO $ pat000
  m1s <- runLIO $ sequenceA patA00
  m2s <- runLIO $ sequenceA patF01
  writeNRT path $ NRT $ initial ++ zipWith3 f (take 1664 ps) m1s m2s ++ last
  where
    f p m1 m2 = Bundle (0.5+(p*(1/4)*beat)) (oscAddition p msgs)
      where
        msgs = [n_set 1001 (M.assocs m1),n_set 1105 m2',n_set 1106 m2']
        m2' = M.assocs m2
    initial = map (\m -> Bundle 0 [m])
              (b_alloc pitchBuf (length pitches) 1:
               b_setn pitchBuf [(0,pitches)]:
               treeToNew 0 a004Nodes)
    last = [Bundle ((1664 * beat * (1/4))+1) []]

setup_a004 :: Transport m => m Message
setup_a004 = do
  liftIO $ mapM_ (\(n,u) -> loadSynthdef n u)
    [("ac2_1", ac2_1),("ac2_2",ac2_2),("ac2_3",ac2_3),("ac2_4",ac2_4)
    ,("fc2_1",fc2_1),("fc2_2",fc2_2),("ac3_1",ac3_1)
    ,("t0041",t0041),("t0043_1",t0043_1),("smaster",smaster)]
  async $ b_free pitchBuf
  async $ b_alloc pitchBuf (length pitches) 1
  send $ b_setn pitchBuf [(0,pitches)]
  reloadSynthdef

go = do
  addNode 0 a004Nodes
  goTrig

--
-- Try:
--
-- * 0 (+3) (-2) 0
-- * 0 (+4) (-3) 0 (+4) (-3) (+1) (+3) (-1) (+6) (+4) 0
-- * 0 +6 -1 +5 -2 +4 -3 +3 -4 +2 -5 +1 -6
--

-- | Change values of pitch buffer.
goPitchBuf :: (Transport t)
           => (Double -> Double) -- ^ Function mapped to pitches
           -> t ()
goPitchBuf f = do
  async $ b_alloc pitchBuf (length pitches) 1
  send $ b_setn pitchBuf [(0,map f pitches)]

goTrig = do
  m1s <- runLIO $ sequenceA patA00
  m2s <- runLIO $ sequenceA patF01
  ps <- runLIO $ pat000
  let z = ZipList
  sequence_ $ getZipList $ playPat <$> z ps <*> z m1s <*> z m2s

--
-- Node mappings
--

a004Nodes :: SCNode
a004Nodes =
  Group 0
    [Group 1
       [Group 10 k004s
       ,Group 11 afp4
       ,Group 12 oscs
       ,Group 13 smasterNode]]

k004s =
  [Synth 1001 "t0041"
     ["gbus":=t1001g,"lbus":=t1001l,"hbus":=t1001h,"hamp":=1]
  ,Synth 1003 "t0043_1"
     ["freq":=4,"fidx":=16,"fmod":=1,"amp":<-t1001l]]

afp4 =
  [Synth 1101 "ac2_1"
     ["amp":=0.03,"freq":=239.3,"pan":=0.9,"atk":=2e-3,"rel":=308e-3
     ,"master":<-t1001h,"t_trig":<-t1101b]
  ,Synth 1102 "ac2_2"
     ["amp":=0.034,"freq":=1307.98,"pan":=0.3,"atk":=8e-3,"rel":=271e-3
     ,"master":<-t1001h,"t_trig":<-t1102b]
  ,Synth 1103 "ac2_3"
     ["amp":=0.034,"freq":=761.8,"pan":=(-0.3),"atk":=3e-3,"rel":=250e-3
     ,"master":<-t1001h,"t_trig":<-t1103b]
  ,Synth 1104 "ac2_4"
     ["amp":=0.03,"freq":=81.3,"pan":=(-0.9),"atk":=12e-3,"rel":=333e-3
     ,"master":<-t1001h,"t_trig":<-t1104b]
  ,Synth 1105 "fc2_1"
     ["bufnum":=pitchBuf,"lagt":=10e-3,"t_trig":<-t1000b]
  ,Synth 1106 "fc2_2"
     ["bufnum":=pitchBuf,"lagt":=8*beat,"t_trig":<-t1000b]
  ,ac3Node]

ac3Node =
  Synth 1107 "ac3_1"
     ["fc":=8000,"fd":=1,"dur":=0.5,"vc":<-t1001g,"edgey":=0.999
     ,"curve":=0.095,"ffreq":=0.5,"fpan":=0.125
     ,"dense":=0.25,"dmi":=0.7,"dmf":=0.57,"t_trig":=1]

smasterNode =
  [Synth 1301 "smaster"
     ["amp":=1,"rmix":=0.01,"rroom":=0.85,"rdamp":=0.75]]

--
-- Oscillator ids
--

bank1 = filter (\x -> (x `mod` 8) `elem` [0..3]) oscIds
(bank1a:bank1b:bank1c:bank1d:_) = packBy 4 bank1
bank2 = filter (\x -> (x `mod` 8) `elem` [4,5]) oscIds
bank3 = filter (\x -> (x `mod` 8) `elem` [6,7]) oscIds

--
-- Bus and buffers
--

t1000b = 100
t1101b = 101
t1102b = 102
t1103b = 103
t1104b = 104
t1001l = 105
t1001g = 106
t1001h = 107

pitchBuf = 1

--
-- Common values
--

bpm :: (Num a) => a
bpm = 72

beat :: (Fractional a, Num a) => a
beat = 60/bpm

-- | C minor penta tonic.
pitches :: (Num a) => [a]
pitches = map (+36) $ zipWith (+) (cycle [0,3,5,7,10])
  (concatMap (replicate 5) [0,12,24,36,48,60,72,84])

--
-- Patterns
--

playPat p0 m1 m2 = do
  now <- liftIO time
  let m2' = M.assocs m2
  let msgs = [n_set 1001 (M.assocs m1),n_set 1105 m2',n_set 1106 m2']
  sendOSC $ Bundle (now+0.1) (oscAddition p0 msgs)
  liftIO $ threadDelay (floor $ 1e6 * (1/4) * beat)

oscAddition :: Double -> [Message] -> [Message]
oscAddition p ms = case p of
  129  -> n_set 1001 [("camp",0.006),("clag",beat*128)]:ms
  240  -> n_set 1001 [("gamp",0.03)]:ms
  256  -> n_set 1001 [("lamp",1)]:ms
  512  -> pch (+6):ms
  640  -> pch (+(-1)):ms
  645  -> n_set 1001 [("gamp",0),("lamp",0)]:ms
  705  -> n_set 1001 [("gamp",0.03)]:ms
  768  -> pch (+5):n_set 1001 [("lamp",1)]:ms
  896  -> pch (+(-2)):ms
  1018 -> n_set 1001 [("camp",0),("clag",beat*8)]:ms
  1023 -> n_set 1001 [("gamp",0)]:ms
  1024 -> pch (+4):ms
  1152 -> pch (+(-3)):n_set 1001 [("camp",0.006),("clag",beat*64)]:ms
  1272 -> n_set 1001 [("gamp",0.03)]:ms
  1280 -> pch (+(-4)):ms
  1284 -> n_set 1001 [("camp",0),("clag",64)]:ms
  1408 -> pch (+2):ms
  1410 -> n_set 1001 [("lamp",0)]:ms
  1528 -> n_set 1001 [("gamp",0)]:ms
  1536 -> pch (+(-5)):n_set 1001 [("camp",0.006),("clag",beat*120)]:ms
  1664 -> n_set 1001 [("camp",0),("clag",beat)]:ms
  _    -> ms
  where
    pch f = b_setn pitchBuf [(0,map f pitches)]

pat000 = plist $ map pdouble [0,1..1700]

patA00 =
  fmap pforever $ M.unionsWith pappend
  [fmap (\xs -> pseq 7 [plist xs]) patA02
  ,fmap plist patA01
  ,fmap (\xs -> pseq 7 [plist xs]) patA03
  ,fmap plist patA01
  ,fmap (\xs -> pseq 7 [plist xs]) patA04
  ,fmap plist patA01
  ,fmap (\xs -> pseq 7 [plist xs]) patA05
  ,fmap plist patA01]

patA01 = M.fromList
   [("t_t1", [0,1,0,0, 1,0,0,0, 1,1,0,1, 1,1,1,0])
   ,("t_t2", [1,0,0,0, 0,0,1,0, 1,1,1,0, 1,0,1,1])
   ,("t_t3", [0,0,0,1, 0,1,0,0, 0,1,1,1, 1,1,0,1])
   ,("t_t4", [0,0,1,0, 0,0,0,1, 1,0,1,1, 0,1,1,1])]

patA02 = M.fromList
   [("t_t1", [0,1,0,0, 1,0,0,0, 0,0,1,0, 0,0,0,1])
   ,("t_t2", [1,0,0,0, 0,0,1,0, 0,0,0,1, 0,1,0,0])
   ,("t_t3", [0,0,0,1, 0,1,0,0, 1,0,0,0, 0,0,1,0])
   ,("t_t4", [0,0,1,0, 0,0,0,1, 0,1,0,0, 1,0,0,0])]

patA03 = M.fromList
   [("t_t1", [0,0,1,0, 0,1,0,0, 0,0,0,0, 0,0,0,0])
   ,("t_t2", [0,1,0,0, 1,0,0,1, 1,0,0,1, 0,0,1,0])
   ,("t_t3", [1,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,0,1])
   ,("t_t4", [0,0,0,0, 0,0,0,0, 0,0,1,0, 0,1,0,0])]

patA04 = M.fromList
   [("t_t1", [0,1,0,0, 0,0,0,1, 0,0,1,0, 1,0,0,0])
   ,("t_t2", [1,0,0,0, 0,0,1,0, 0,0,0,1, 0,1,0,0])
   ,("t_t3", [0,0,0,1, 0,1,0,0, 1,0,0,0, 0,0,1,0])
   ,("t_t4", [0,0,1,0, 1,0,0,0, 0,1,0,0, 0,0,0,1])]

patA05 = M.fromList
   [("t_t1", [0,1,0,1, 0,0,0,1, 0,0,0,0, 1,0,0,0])
   ,("t_t2", [1,0,1,0, 0,0,1,0, 0,0,0,0, 0,1,0,0])
   ,("t_t3", [0,0,0,0, 0,1,0,0, 1,0,1,0, 0,0,1,0])
   ,("t_t4", [0,0,0,0, 1,0,0,0, 0,1,0,1, 0,0,0,1])]

patF01 = M.fromList
  [("t_trig", pcycle [pseq 60 [0], plist [0,1,0,0]])]

plist = pseq 1

--
-- UGens
--
t0041 = t0041' ("t_t1"=:0) ("t_t2"=:0) ("t_t3"=:0) ("t_t4"=:0)
  ("hamp"=:1) ("hlag"=:1) ("hbus"=:0) ("camp"=:0) ("clag"=:1e-3)
  ("lamp"=:0) ("lbus"=:0) ("llag"=:1) ("gamp"=:0) ("gbus"=:0)
t0041' t_t1 t_t2 t_t3 t_t4 hamp hlag hbus camp clag lamp lbus llag gamp gbus =
  mrg (hit:longAttack:gremlin:trigs ++ outs)
  where
    outs = map (\i -> out (fromIntegral $ ampBus i) csig) bank2
    csig = lag camp clag
    trigs = [out t1101b t_t1, out t1102b t_t2, out t1103b t_t3, out t1104b t_t4]
    longAttack = out lbus (lag lamp llag)
    hit = out hbus (lag hamp hlag)
    gremlin = out gbus (lag gamp 10e-3)

t0043_1 = t0043 bank1
t0043 ids = t0043' ids ("amp"=:0) ("freq"=:1) ("fmod"=:1) ("fidx"=:1)
t0043' ids amp freq fmod fidx = out outBus sig
  where
    outBus = select idx (mce $ map ampBus ids)
    idx = tIRand 'i' 0 (fromIntegral $ length ids) tr
    sig = linen tr atk lv 10e-3 DoNothing * amp
    lv = tExpRand 'v' 0.05 0.2 tr
    atk = tExpRand 'x' 100e-3 2 tr
    tr = dust 'a' KR dfreq
    dfreq = clip (freq + (lfdNoise3 'f' KR fmod) * fidx) 0 (freq+fidx)

(ac2_1:ac2_2:ac2_3:ac2_4:_) = map ac2 [bank1a,bank1b,bank1c,bank1d]
ac3_1 = ac3 bank3
fc2_1,fc2_2 :: UGen
fc2_1 = fc2 bank1
fc2_2 = fc2 bank2

--
-- UGen makers
--

-- | Controls amp and pan of oscillator specified with node id.
ac2 :: [NodeId] -> UGen
ac2 ids = ac2' ids ("master"=:1) ("amp"=:0.05) ("atk"=:5e-3) ("rel"=:300e-3)
  ("pan"=:0) ("t_trig"=:0)
ac2' ids master amp atk rel pan t_trig = mrg (concatMap mkO ids)
  where
    mkO i = [out (ampBus i) (amp' i), out (panBus i) pan]
    amp' i = ampEnv * amp * master * tRand i 0.5 1.5 t_trig
    ampEnv = envGen KR t_trig 1 0 1 DoNothing $
             Envelope [0,0,1,0] [0,atk,rel] [EnvCub] (Just (-1)) (Just 0)

-- | Rapid envelope with envgen triggered by impulse.
ac3 :: [NodeId] -> UGen
ac3 ids = ac3' ids ("edgey"=:999e-3) ("dur"=:1) ("fc"=:8000) ("fd"=:1)
  ("dense"=:1) ("dmf"=:1) ("dmi"=:0) ("ffreq"=:0.5) ("fpan"=:0.125)
  ("vc"=:0.001) ("curve"=:(-12)) ("t_trig"=:1)
ac3' ids edgey dur fc fd dense dmf dmi ffreq fpan vc curve t_trig =
  mrg $ concatMap mkO ids
  where
    mkO i = [out (ampBus i) amp, out (freqBus i) freq,out (panBus i) pan]
      where
        amp = envGen KR tr vc 0 edur DoNothing $
              Envelope [1e-9,1e-9,1,1e-9] [0,1-edgey,edgey] [EnvNum ecurve]
              (Just (-1)) (Just 0)
        edur = linExp dur 1e-9 1 1e-4 2
        ecurve = linLin curve 0 1 (-12) 12
        tr = impulse KR trf 1
        trf = cubed (clip2 (lfdNoise3 'a' KR dmf' * 0.5 + 0.5) 1 * dmi') + dense'
        dmf' = linLin dmf 0 1 0 10
        dmi' = linLin dmi 0 1 0 10
        dense' = linLin dense 0 1 0 50
        freq = clip (tExpRand i flo fhi t_trig) 0 12000 *
               lfdNoise3 i KR ffreq
        flo = 1e-3 + (fc/2) - (fd*(fc/2))
        fhi = (fc/2) + (fd*(fc/2))
        pan = lfdNoise3 'p' KR fpan

-- | Look up a buffer for pitch value and use for each oscillators.
fc2 :: [NodeId] -> UGen
fc2 ids = fc2' ids ("bufn"=:1) ("lagt"=:(2*60/bpm)) ("t_trig"=:0)
fc2' ids bufn lagt t_trig = mrg $ map mkO ids
  where
    mkO i = out (freqBus i) (freq i)
    freq i = lag (targetFreq i) (lagt * tExpRand i 0.25 4 t_trig)
    targetFreq i = midiCPS (index bufn (idx i)) * detune i
    detune i = tRand i 0.9998 1.0002 t_trig
    idx i = tIRand i 0 (fromIntegral $ length pitches - 1)
            (tDelay t_trig (tExpRand (succ i) 0.25 4 t_trig))
