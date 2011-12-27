{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

B003 - Pink blizzard.

-}
module Sound.Study.ForNoisesAndFilters.B003 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

-- --------------------------------------------------------------------------
--
-- Main
--

go :: Transport t => t -> IO ()
go fd = do
  mapM_ (async fd . d_recv . uncurry synthdef)
    [("ppnz",ppnz)
    ,("ppC",ppC ("bpm"@@60))
    ,("ppt01",ppt01)
    ,("ppt02",ppt02)
    ,("pp",pp)
    ,("pp02",pp02)
    ,("pp03",pp03)
    ,("pp04",pp04)
    ,("pp05",pp05)]
  patchNode (nodify n0) fd

w :: (UDP -> IO a) -> IO a
w = withSC3

-- --------------------------------------------------------------------------
--
-- Nodes
--

n0 :: Nd
n0 =
  let pn = syn "ppnz" ["out"*=10]
      pn_out = prmv pn "out"
      ppt02_nd = syn "ppt02" ["out"*=104]
      tout = prmv ppt02_nd "out"
  in  grp 0
      [grp 1
       [grp 10
        [pn
        ,syn "ppC"
         ["bpm"*=160,"outt"*=100,"outf"*=101,"outa"*=102,"outp"*=103
         ,"minf"*=600,"maxf"*=12800,"outatk"*=105]
        ,syn "ppt01"
         ["out"*=100,"freq"*=8]
        ,syn "pp"
         ["t_trig"*<-100,"freq"*<-101,"amp"*<-102,"pan"*<-103
         ,"dur"*=1,"atk"*<-105,"en"*=1,"a_nz"*<=pn_out]
        ,ppt02_nd
        ,syn "pp02" ["t_trig"*<-tout,"a_nz"*<=pn_out]
        ,syn "pp03" ["t_trig"*<-tout,"a_nz"*<=pn_out]
        ,syn "pp04" ["t_trig"*<-tout,"a_nz"*<=pn_out]
        ,syn "pp05" ["t_trig"*<-tout,"a_nz"*<=pn_out]
        ]]]

-- --------------------------------------------------------------------------
--
-- Triggers and controls
--

ppt01 :: UGen
ppt01 = out ("out"@@0) (impulse KR (lfdNoise3 'f' KR 1 * f + (f/2)) 0) where
  f = "freq"@@1

ppt02 :: UGen
ppt02 = out ("out"@@0) (impulse KR ("bpm"@@480/60) 0)

-- --------------------------------------------------------------------------
--
-- Source pink noise
--

ppnz :: UGen
ppnz = out ("out"@@0) (pinkNoise 'p' AR)

-- ppnz = out ("out"@@0) sig where
--   sig = xFade2 hnz pnz (fSinOsc KR (1/60) 0) 1
--   hnz = henonC AR (sampleRate/2) y x 0 0 * 0.65
--   pnz = pinkNoise 'p' AR
--   x = linLin (lfdNoise0 'x' KR m) (-1) 1 0.1 0.5 `lag3` 0.25
--   y = linLin (lfdNoise0 'y' KR m) (-1) 1 0.3 1.8 `lag3` 0.25
--   m = linLin (fSinOsc KR (1/32) 0) (-1) 1 (1/32) 4

-- --------------------------------------------------------------------------
--
-- Windy chorus.
--

ppC :: UGen -> UGen
ppC bpm = mrg [outt, outf, outa, outp, outk] where
  t = dust 'd' KR (bpm/45)
  outt = out ("outt"@@0) t
  outf = out ("outf"@@0) $ index b (tRand 'i' 0 (bufFrames KR b) t) where
    ptchs = [midiCPS (x+y)|x<-[0,4,7,11],y<-[36,48..108]]
    b = asLocalBuf 'a' ptchs
  outa = out ("outa"@@0) (tExpRand 'a' 1e-3 5e-2 t)
  outp = out ("outp"@@0) en where
    en = envGen KR t 1 0 1 DoNothing sh
    sh = env [st, ed] [du] [EnvSin] (-1) 0
    st = tRand 's' (-1) 1 t
    ed = tRand 'e' (-1) 1 t
    du = tRand 'd' 0.25 2 t
  outk = out ("outatk"@@0) (tRand 'k' 1e-4 9999e-4 t)

pp01 :: UGen
pp01 = pp' t a f p 8 d k where
  ptchs = [midiCPS (x+y)|x<-[0,4,7,11],y<-[36,48..108]]
  b = asLocalBuf 'a' ptchs
  f = index b (tRand 'i' 0 (bufFrames KR b) t)
  a = tExpRand 'a' 0.0125 0.025 t
  p = tRand 'p' (-1) 1 t
  k = tExpRand 'k' 1e-4 9999e-4 t
  t = dust 'd' KR (mouseY KR 0.25 12 Exponential 0.1)
  d = linLin (lfdNoise1 'δ' KR (1/16)) (-1) 1 1e-4 5e-2

pp :: UGen
pp = pp' ("t_trig"@@1) ("amp"@@0.3) ("freq"@@1200) ("pan"@@0)
     ("envn"@@0) ("dur"@@0.1) ("atk"@@0.1)
pp' tick amp freq pan en dur atk = out ("out"@@0) sig0 where
  sig0 = rlpf sig1 12000 0.85
  sig1 = rhpf (foldr f sig2 "blahouqp32814") 30 1
  sig2 = pan2 (ringz (nz * aenv) freq (q*(min 1.25 (4000/(freq**1.1))))) pan 1
  f a b = allpassC b 1 (rand a 1e-1 1) (rand a 1 4)
  aenv = envGen KR tick amp 0 dur DoNothing $
         env [0,1,0] [atk,1-atk] [EnvNum en] (-1) 0
  nz = "a_nz"@@0
  q = linLin (lfdNoise3 'q' KR 20) (-1) 1 1e-3 125e-3


-- --------------------------------------------------------------------------
--
-- Arpeggio
--

pp02 :: UGen
pp02 = pp02' ("t_trig"@@0)

pp02' :: UGen -> UGen
pp02' t = out ("out"@@0) (pan2 sig pan 1) where
  pan = fSinOsc KR (1/61.32) pi * 0.1
  p1 chr = tail $ zipWith (+) (cycle chr) (concatMap (replicate 4) [24,36..84])
  mj7 = [0,4,7,11]
  mn7 = [0,3,7,10]
  svn = [0,4,7,10]
  arp offset n chr = xs where
    xs = concat $ replicate n
      [ i | j <- [24] ++ p1 chr ++ [84] ++ reverse (p1 chr)
          , let i = j + offset, 36 <= i, i <= 84 ]
  i_M7   n = arp  0 n mj7
  ii_m7  n = arp  2 n mn7
  ii_7   n = arp  2 n svn
  iii_m7 n = arp  4 n mn7
  iv_M7  n = arp  5 n mj7
  v_7    n = arp  7 n svn
  v_M7   n = arp  7 n mj7
  vi_m7  n = arp  9 n mn7
  vii_m7 n = arp 11 n mn7
  ptchs = concat
    [ i_M7 2, iv_M7 2, i_M7 2, iv_M7 2, i_M7 1, vi_m7  1, ii_m7  1, v_7  1
    , i_M7 2, iv_M7 2, i_M7 2, iv_M7 2, i_M7 1, vi_m7  1, ii_7   1, v_M7 1
    , v_M7 2, i_M7  2, v_M7 2, i_M7  2, v_M7 1, iii_m7 1, vii_m7 1, ii_7 1
    , v_M7 2, i_M7  2, v_M7 2, i_M7  2, v_M7 1, vi_m7  1, ii_7   1, v_7  1 ]
  c = asLocalBuf 'c' (map midiCPS ptchs)
  frq = index c (pulseCount t 0 `mod` bufFrames IR c)
  q = linLin (lfdNoise3 'q' KR 1) (-1) 1 0.01 3
  nz = "a_nz"@@0
  sig' = mix $ ringz nz fs qs * e * (1/84) where
    fs = mce [frq*i|i<-[1..16]]
    qs = mce [q*recip i|i<-[1..16]]
    e = envGen KR t 1 0 dur DoNothing
        (env [0,1,0] [8e-3,920e-3] [EnvCub] (-1) 0)
    dur = linLin (fSinOsc KR (1/176) (1.5*pi) + 1) 0 2 (1/128) 2.56
  sig = freeVerb sig' 0.5 0.99 0.99 * 0.6


-- --------------------------------------------------------------------------
--
-- Wood/bamboo like hit.
--

pp03 :: UGen
pp03 = pp03' ("t_trig"@@0)

pp03' :: UGen -> UGen
pp03' tick = out 0 (pan2 (hpf sig 20) ("pan"@@0.1) 1) where
  sig = hpf (sig1 + foldr h sig1 "oiqwe82-#@~3zp[") 50 where
    h x y = allpassL y 1 (expRand x 1e-4 1e-1) (rand x 2e-1 2.1)
  sig1 = (mix $ (sig0 * (1/650) + (sig0' * (1/160))) * e) * 0.9
  sig0 = mce
    [ f x r
    | x <- [293.13, 360.03, 391.32, 459.99, 533.13, 882.3]
    , r <- [0.9, 0.2, 0.8, 0.4, 0.25, 0.1]
    , let f y z = ringz nz (y + q) (1 - z * decay2 10e-3 300e-3 t)]
  sig0' = mce
    [ f x r
    | x <- [321.139, 479.32, 632.03]
    , r <- [28e-3, 12e-3, 16e-3]
    , let v i = env [0,1,0] [i,800e-3] [EnvNum (-3)] (-1) 0
    , let h i = i * envGen KR t 1 0 1 DoNothing (v i)
    , let f a b = rhpf nz a (h b) ]
  nz = "a_nz"@@0
  q = linLin (lfdNoise1 'q' KR 0.25) (-1) 1 1e-1 20
  e = envGen KR t 1 0 1 DoNothing $
      env [0,1,0.8,0.8,0] [1e-3,20e-3,50e-3,100e-3] [EnvNum (-8)] (-1) 0
  t = pulseDivider tick 16 0 + coinGate 'p' prob tick' where
    prob  = linLin (lfdNoise3 'r' KR 0.125) (-1) 1 0 (1/6)
    tick' = pulseDivider tick 2 0

-- --------------------------------------------------------------------------
--
-- Reverbed low frequency percussive hit
--

pp04 :: UGen
pp04 = pp04' ("t_trig"@@0)

pp04' :: UGen -> UGen
pp04' tick = out ("out"@@0) (pan2 sig ("pan"@@(-0.1)) 1) where
  t = pulseDivider tick 16 8 +
      coinGate 'g' prob1 (pulseDivider tick 16 12) +
      coinGate 'z' prob2 (pulseDivider tick 8 0) where
    prob1 = linExp (lfdNoise1 'l' KR (1/32) + 1) 0 2 (1/8) (1/2)
    prob2 = linExp (lfdNoise1 'm' KR (1/32) + 1) 0 2 (1/64) (1/4)
  nz = "a_nz"@@0
  sig = hpf (sig' + foldr f sig' "zixab8=_") 20 * 0.5 where
    f x y = allpassL y 1 (rand x 1e-4 1e-1) (rand x 5e-1 0.8)
  fs = [59.32,120,255,372,493,893,1203.32,3821.32]
  sig' = mix $ rlpf nz (mce fs) q * e * tExpRand 'a' 0.6 1 t
  q = envGen KR t 1 0 1 DoNothing $
      env [0,1,0] [20e-3,210e-3] [EnvNum (-8)] (-1) 0
  e = envGen KR t 1 0 1 DoNothing $
      env [0,1,0] [3e-3,250e-3] [EnvNum (-3)] (-1) 0


-- --------------------------------------------------------------------------
--
-- Dull attack short duration filtered noise
--

pp05 :: UGen
pp05 = pp05' ("t_trig"@@0)

pp05' :: UGen -> UGen
pp05' t = out ("out"@@0) (pan2 sig0 ("pan"@@0.05) 1) where
  sig0 = sig1 * 0.95
  sig1 = sig2 + foldr h sig2 "btQ8231[a,>@<%*" * tamp where
    h x y = allpassC y 1e-1 (expRand x 1e-3 3e-1) (expRand x 1e-4 5e-1)
  sig2 = mix $ resonz nz (mce fs) (mce qs) * e * 4
  nz = "a_nz"@@0
  tamp = index ampb (pulseCount (pulseDivider t 2 0) 0 `mod` nbuf)
  nbuf = bufFrames IR ampb
  ampb = asLocalBuf '5' [1,0.50,0.70,0.53, 0.95,0.55,0.62,0.55]
  fs = [f * f1 + bf, f * f2 + bf, f * f3 + bf]
  f1 = tExpRand 'u' 2400 4800 tf
  f2 = tExpRand 'e' 1600 6400 tf
  f3 = tExpRand 'o' 4100 8200 tf
  bf = tExpRand 'g' 600 1200 tf
  tf = pulseDivider t 128 0
  qs = [q * 0.3 + 0.04, q * 0.4 + 0.03, q * 0.9 + 0.01]
  f = tExpRand '≒' 0.5 1 tick
  q = envGen KR tick 1 0 et DoNothing $
      env [1,1e-8,0.5] [120e-3,220e-3] [EnvLin] (-1) 0
  e = envGen KR tick 1 0 et DoNothing $
      env [0,1,l1,l2,0] [d0,d1,d2,d3] [EnvNum (-11)] (-1) 0 where
    l1 = tExpRand '1' 0.1 1 tick
    l2 = tExpRand '2' 0.1 1 tick
    d0 = tExpRand 'κ' 1e-4 10e-4 tick
    d1 = tExpRand 'δ' 80e-3 160e-3 tick
    d2 = tExpRand 'γ' 80e-3 160e-3 tick
    d3 = tExpRand 'ε' 80e-3 160e-3 tick
  et = linLin (lfdNoise3 'w' KR (1/12.32)) (-1) 1 0.25 1.5
  tick = coinGate 't' r (pulseDivider t 2 0) where
    r = linLin (lfdNoise3 't' KR (1/33)) (-1) 1 (1/4) (3/4)

{-

cd2tkl :: UGen
cd2tkl = cd2tkl' ("t_trig"@@1)
cd2tkl' tick = out ("out"@@0) $ decay2 tick 1e-3 1.2 * sig * 0.2 where
  sig = foldr f sig' (map (\i -> rand i 0.001 0.05) "abwewpoew")
  f a b = allpassN b 0.05 a 4
  sig' = ringz nz freq rt
  freq = tExpRand 'f' 1020 12800 tick
  nz = pinkNoise 'a' AR
  rt = mouseX KR 0.04 4 Linear 0.1

b003dust :: UGen -> UGen
b003dust bpm = out ("out"@@0) sig where
  sig = dust 'd' KR (bpm/60)

fshift :: UGen
fshift = fshift' ("a_in"@@0)
fshift' input = out ("out"@@0) sig where
  sig = freqShift input (sinOsc KR f 0 * 250.5 + 250.5) 0
  f = sinOsc KR (1/5.1) 0 * 2.5 + 2.5

fs2 :: UGen
fs2 = out ("out"@@0) sig where
  sig = freqShift source (sinOsc KR f 0 * 250.5 + 250.5) 0
  f = sinOsc KR (1/5.1) 0 * 2.5 + 2.5
  source = ringz (pinkNoise 'p' AR * aenv * 0.3) freq rq
  freq = tExpRand 'f' 1200 8800 tick
  rq = lfdNoise3 'q' KR 1 * 0.49 + 0.5
  aenv = decay2 tick 1e-3 1
  tick = dust 'd' KR 1

-}
