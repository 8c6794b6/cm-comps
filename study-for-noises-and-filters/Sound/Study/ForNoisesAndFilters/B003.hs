module Sound.Study.ForNoisesAndFilters.B003.Synthdef where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

w = withSC3
g = Group
s = Synth

go :: Transport t => t -> IO ()
go = patchNode n0

-- Wish list:
--
-- * Get rid of manual node id book keeping,
--  e.g. enable querying of 'give me what ever free id'.
--
-- * Get rid of manual bus number book keeping,
--  e.g. enable querying like 'value used in this node's that parameter'.
--
-- * Enable doing the same thing as hsynthdef shell command with using
--   Template haskell.

n0 =
  g 0
  [g 1
   [g 10
    [s 1000 "ppC"
     ["bpm":=60,"outt":=100,"outf":=101,"outa":=102,"outp":=103
     ,"minf":=600,"maxf":=12800,"outatk":=105]
    ,s 1001 "ppt"
     ["out":=100,"freq":=8]
    ,s 1002 "pp"
     ["t_trig":<-100,"freq":<-101,"amp":<-102,"pan":<-103
     ,"dur":=1,"atk":<-105,"en":=1]
    ]]]

data PP01 = PP01 Freq Amp Dur
data Freq = Freq UGen
data Amp = Amp UGen deriving (Eq,Show)
data Dur = Dur UGen

cd2tkl :: UGen
cd2tkl = cd2tkl' ("t_trig"@@1)
cd2tkl' tick = out ("out"@@0) $ decay2 tick 1e-3 1.2 * sig * 0.2 where
  sig = foldr f sig' (map (\i -> rand i 0.001 0.05) "abwewpoew")
  f a b = allpassN b 0.05 a 4
  sig' = ringz nz freq rt
  freq = tExpRand 'f' 1020 12800 tick
  nz = pinkNoise 'a' ar
  rt = mouseX kr 0.04 4 Linear 0.1

ppt :: UGen
ppt = out ("out"@@0) (impulse kr (lfdNoise3 'f' kr 1 * f + (f/2)) 0) where
  f = "freq"@@1

b003dust :: UGen -> UGen
b003dust bpm = out ("out"@@0) sig where
  sig = dust 'd' kr (bpm/60)

ppC :: UGen -> UGen
ppC bpm = mrg [outt, outf, outa, outp, outatk] where
  t = dust 'd' kr (bpm/60)
  outt = out ("outt"@@0) t
  outf = out ("outf"@@0) (tExpRand 'f' ("minf"@@1020) ("maxf"@@12080) t)
  outa = out ("outa"@@0) (tExpRand 'a' 1e-3 5e-2 t)
  outp = out ("outp"@@0) (tRand 'p' (-1) 1 t)
  outatk = out ("outatk"@@0) (tRand 'k' 1e-4 9999e-4 t)

pp01 :: UGen
pp01 = pp' t a f p (-14) 1 1e-4
  where
    f = tExpRand 'f' 1200 12800 t
    a = tExpRand 'a' 0.05 0.15 t
    p = tRand 'p' (-1) 1 t
    t = dust 'd' kr (mouseY kr 0.25 2 Exponential 0.1)

pp :: UGen
pp = pp' ("t_trig"@@1) ("amp"@@0.3) ("freq"@@1200) ("pan"@@0)
     ("envn"@@0) ("dur"@@0.1) ("atk"@@0.1)
pp' tick amp freq pan en dur atk = out ("out"@@0) sig where
  sig = foldr f sig' $ map (\x -> rand x 1e-4 5e-3) "blahouqp32813"
  f a b = allpassN b 5e-3 a 3
  sig' = pan2 (ringz (nz * aenv) freq q) pan 1
  -- aenv = decay2 tick 1e-3 1 * amp
  aenv = envGen kr tick amp 0 dur DoNothing $
         env [0,1,0] [atk,1-atk] [EnvNum en] (-1) 0
  nz = pinkNoise 'p' ar
  -- nz = henonC ar (sampleRate/2) {- 8800 -} 1.4 0.3 0 0
  q = lfdNoise3 'q' kr 1 * 2.4 + 2.401

fshift :: UGen
fshift = fshift' ("a_in"@@0)
fshift' input = out ("out"@@0) sig where
  sig = freqShift input (sinOsc kr f 0 * 250.5 + 250.5) 0
  f = sinOsc kr (1/5.1) 0 * 2.5 + 2.5

fs2 :: UGen
fs2 = out ("out"@@0) sig where
  sig = freqShift source (sinOsc kr f 0 * 250.5 + 250.5) 0
  f = sinOsc kr (1/5.1) 0 * 2.5 + 2.5
  source = ringz (pinkNoise 'p' ar * aenv * 0.3) freq rq
  freq = tExpRand 'f' 1200 8800 tick
  rq = lfdNoise3 'q' kr 1 * 0.49 + 0.5
  aenv = decay2 tick 1e-3 1
  tick = dust 'd' kr 1
