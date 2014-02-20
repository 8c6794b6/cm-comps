-- |
-- Copyright    : 8c6794b6, 2014
-- License      : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scratch written while reading year 2001 synth secrets articles.
--
module Sound.Study.ForSynthSecrets.Y2001 where

import           Control.Monad (void, zipWithM_)
import           Control.Monad.Trans.Class (lift)
import           Data.List (zipWith5)
import           System.Random (mkStdGen, newStdGen, randomR, randomRs)

import           Sound.OSC (SendOSC(..), bundle, time)
import           Sound.OSC.Time (pauseThreadUntil)
import           Sound.SC3
    ( AddAction(..), DoneAction(..), Envelope(..), Envelope_Curve(..)
    , UGen, Rate(..), Warp(..)
    , async, audition, mrg, mce, withSC3
    )
import qualified Sound.SC3.Server as Server
import qualified Sound.SC3.UGen as UGen
import qualified Sound.SC3.UGen.ID as ID

import           Sound.Study.ForSynthSecrets.Y1999 (centeredOut)


-- --------------------------------------------------------------------------
--
-- * January 2001
--
-- --------------------------------------------------------------------------

{-
Notes:
SEM - Synthesizer Expander Module

Quotes:
In mainstream synthesis, there have only ever been two totally analogue
polysynths: the Korg PS3100 and PS3300.
-}

-- | Synthdef used in 'play_sem01':
--
-- >>> audition sem01
sem01 :: UGen
sem01 = centeredOut sig
  where
    sig   = UGen.rlpf (UGen.sinOsc AR freq phase) cf 0.9 * ampe
    freq  = cnt "freq" 440
    phase = UGen.sinOsc AR (UGen.mix freqs) (pi/2) * idx
    freqs = mce [freq*2.001,freq*3.99989]
    idx   = UGen.envGen KR 1 10 1 dur DoNothing $
            UGen.envCoord [(0,1),(0.5,0.2),(1,1)] 1 1 EnvLin
    ampe  = UGen.envGen KR 1 amp 0 dur RemoveSynth $
            UGen.envCoord [(0,0),(1e-2,1),(2e-1,0.8),(0.9,0.7),(1,0)]
            1 1 EnvCub
    cf    = UGen.envGen KR 1 (freq*10) 0 dur DoNothing $
            UGen.envCoord [(0,1),(0.5,0.75),(0.75,0.75),(1,0.3)]
            1 1 EnvExp
    amp   = cnt "amp" 0.3
    dur   = cnt "dur" 1
    cnt   = UGen.control KR

play_sem01_once :: IO ()
play_sem01_once = audition sem01

-- | Play polyphonically.
play_sem01 :: IO ()
play_sem01 = withSC3 $ do
    let name  = "sem01"
        bdur  = 3
        n p d = Server.s_new name (-1) AddToTail 1
                [("freq",UGen.midiCPS (p-7)),("dur",d*bdur),("amp",0.1)]
        pat   = ([n 60 32, n 67 1, n 76 1]:pat0) ++ pat1 ++ pat1 ++ pat1
        pat0  = [ [n 65 1, n 69 1]
                , [n 67 1, n 76 1]
                , [n 65 1, n 69 1]

                , [n 67 1, n 76 1]
                , [n 69 1, n 76 1]
                , [n 65 2, n 69 1]
                , [n 67 1, n 72 1]
                ]
        pat1  = [n 67 1, n 76 1]:pat0
    _ <- async . Server.d_recv $ Server.synthdef name sem01
    now <- time
    zipWithM_ (\t o -> sendOSC $ bundle (now+(t*bdur)) o) [0..] pat

-- | Synthdef used in 'play_sem02'.
sem02 :: UGen
sem02 = centeredOut sig
  where
    sig  = UGen.rlpf (UGen.pulse AR freq pw) cf rq * ampe
    freq = k "freq" 440
    pw   = (ID.lfNoise1 'a' KR 0.8 + 1) * 0.5
    ampe = UGen.envGen KR 1 amp 0 dur RemoveSynth $
           UGen.envCoord [(0,0),(0.1,1),(0.3,0.5),(0.7,0.5),(1,0)]
           1 1 EnvCub
    cf   = UGen.envGen KR 1 9600 400 dur DoNothing $
           UGen.envCoord [(0,0),(0.3,1),(1,0)] 1 1 EnvCos
    rq   = (ID.lfNoise2 'b' KR 3.73 + 1) * 0.5 + 0.5
    amp  = k "amp" 0.3
    dur  = k "dur" 1
    k    = UGen.control KR

-- | Another action using OSC messaging with timestamps.
play_sem02 :: IO ()
play_sem02 = withSC3 $ do
    let name     = "sem02"
        s2 f d a = Server.s_new name (-1) AddToTail 1
                 [("freq",f),("dur",d),("amp",a)]
        go g0 t0 tend =
            let (t1,g1)  = randomR (log 0.1, log 2) g0
                t2       = t0 + exp t1
                (nv, g2) = randomR (1,10) g1
                keyl     = [0,2,4,5,7,9,11]
                ptcl     = zipWith (+)
                           (concatMap (replicate (length keyl)) [24,36..96])
                           (cycle keyl)
                ptcf j   = UGen.midiCPS (ptcl !! j)
                ptcs     = map ptcf $ randomRs (0,length ptcl - 1) g0
                durs     = map exp $ randomRs (log 0.4, log 3.95) g1
                amps     = randomRs (0.08,0.25) g2
                params   = zipWith3 s2 ptcs durs amps
                os       = take nv $ params
            in  if tend > t2
                   then (t2,os):go g2 t2 tend
                   else []
    void $ async $ Server.d_recv $ Server.synthdef name sem02
    now <- time
    g0 <- lift newStdGen
    mapM_ (sendOSC . uncurry bundle) $ go g0 now (now + 3000)

-- --------------------------------------------------------------------------
--
-- * Feburary 2001
--
-- --------------------------------------------------------------------------

{-
Quotes:

You don't need powerful DSPs to dabble with physical modelling of acoustic
spaces... a few analogue reverbs are more than enough.
-}

-- | Simple delay using 'UGen.delayC'. Making delays manually, without feedback.
del01 :: UGen
del01 = centeredOut sig
  where
    sig      = sum (nz : zipWith fdel ts as)
    fdel i a = UGen.delayL nz 0.2 (UGen.constant i) * a
    nz       = ID.whiteNoise 'a' AR * amp * 0.2
    amp      = UGen.decay (ID.dust 'd' KR 1) 0.2
    ts       = scanl1 (+) $ take 64 $ randomRs (1e-4::Double,4e-4) g0
    g0       = mkStdGen 0x347282
    as       = iterate (*0.97) 0.8

-- | Example of 'UGen.delayC' from hsc3 help.
delayC_ex01 :: IO ()
delayC_ex01 = audition $ centeredOut (z + UGen.delayC z 0.2 x)
  where
    z = UGen.decay d 0.3 * n
    d = ID.dust 'a' AR 1
    n = ID.whiteNoise 'b' AR
    x = UGen.mouseX KR 0 0.2 Linear 0.1

-- | Another example of 'UGen.delayC' from hsc3 help.
delayC_ex02 :: IO ()
delayC_ex02 = audition $ centeredOut (o + UGen.delayC o l x)
  where
    o = UGen.sinOsc AR 320 0 * 0.1
    l = 0.005
    x = UGen.mouseX KR 0 l Linear 0.15

-- | From hsc3 help file of 'UGen.replaceOut'.
replaceOut_ex01 :: IO ()
replaceOut_ex01 = audition $ mrg [b,a]
  where
    a = UGen.out 24 (ID.pinkNoise 'a' AR * 0.1)
    b = UGen.replaceOut 0 (UGen.bpf (UGen.in' 1 AR 24) f 1)
    f = (UGen.sinOsc KR (recip 8) 0 + 1) * 4000

-- | From hsc3 help of 'UGen.allpassN'.
allpassN_ex01 :: IO ()
allpassN_ex01 = audition $ centeredOut s
  where
    s = UGen.decay (d * 0.5) 0.2 * n
    d = ID.dust 'K' AR 1
    n = ID.whiteNoise 'n' AR

combC_ex01 :: IO ()
combC_ex01 = audition $ centeredOut sig
  where
    sig = UGen.combC i 0.2 0.2 3
    i   = UGen.decay (d * 0.5) 0.2 * n
    n   = ID.whiteNoise 'a' AR
    d   = ID.dust 'b' KR 1

-- | Simple reverb using  with reverb /inside the patch/.
del02 :: UGen
del02 = mrg [centeredOut sig, UGen.localOut sig1]
  where
    sig    = UGen.hpf (UGen.rlpf sig1 cf rq * amp) 20
    sig1   = sum $ map fdel [1.323e-4,2.889e-4,3.32e-3,8.3746e-3]
    fdel i = UGen.delayC sig2 0.2 i * 0.25
    sig2   = (UGen.localIn 1 AR * 0.99) + UGen.saw AR freq
    freq   = ID.tIRand 'a' 1 8 tr0 * 220
    amp    = UGen.envGen KR tr0 0.3 0 0.8 DoNothing $
             UGen.envCoord [(0,0),(1e-3,1),(1,0)]
             1 1 EnvCub
    tr0    = tr1 + tr2
    tr1    = ID.dust 'd' KR 1.5
    tr2    = UGen.impulse KR tf2 0
    tf2    = ((ID.lfNoise2 'f' KR 0.3) + 1) * 4
    cf     = 8000
    rq     = 0.98

-- | Simple delay with 'UGen.delayC', 'UGen.localIn', and 'UGen.localOut'.
del03 :: UGen
del03     = mrg [UGen.localOut sig0, centeredOut $ UGen.hpf sig1 20]
  where
    sig0  = sum [UGen.delayC sig1 1 delt0
                ,UGen.delayC sig1 1 delt1
                ,UGen.delayC sig1 1 delt2
                ]
    sig1  = UGen.localIn 1 AR * fbk + sig2
    fbk   = UGen.mouseY KR 1e-2 0.6 Exponential 0.1
    sig2  = UGen.sinOsc AR freq 0 * amp
    freq  = (ID.lfdNoise0 'f' KR (1/3) + 1) * 1000 + 260
    -- freq  = ID.tExpRand 'F' 100 1000 tr0
    amp   = UGen.decay tr0 (recip tfreq * drat) * 0.1 * ampr
    ampr  = ID.tExpRand 'a' 0.25 1.75 tr0
    drat  = (ID.lfNoise2 'd' KR (1/11) + 1) * 1.3
    tr0   = ID.coinGate 'G' 0.70 (UGen.impulse KR tfreq 0) +
            ID.coinGate 'g' 0.40 (UGen.impulse KR (tfreq*2) 0) +
            UGen.impulse KR (tfreq/4) 0 +
            ID.dust 'd' KR ((ID.lfNoise0 'f' KR (1/3) + 1) * tfreq * 1.5)
    tfreq = 2
    delt  = UGen.mouseX KR 1e-4 1 Exponential 0.1
    delt0 = (ID.lfNoise2 'd' KR (1/13) + 1) * delt + 5e-5
    delt1 = (ID.lfNoise2 'e' KR (1/15) + 1) * delt + 5e-5
    delt2 = (ID.lfNoise2 'f' KR (1/17) + 1) * delt + 5e-5

-- | Plays 'del03'.
play_del03 :: IO ()
play_del03 = audition del03

-- | Another delay, using sawtooth as input.
del04 :: UGen
del04 = mrg [UGen.localOut (UGen.hpf sig0 20), centeredOut sig1]
  where
    sig0     = foldr f sig1 dts
    f dt acc = acc + UGen.delayC sig1 1 (ID.rand dt 1e-4 1 * mx) * 0.25
    dts      = [1..16::Int]
    mx       = UGen.mouseX KR 5e-5 2 Exponential 0.1
    sig1     = (UGen.localIn 1 AR * my) + sig2
    my       = UGen.mouseY KR 0.01 0.50 Linear 0.1
    sig2     = UGen.sinOsc AR freq 0 * amp
    freq     = ID.tExpRand 'Σ' 100 2000 tr0
    amp      = UGen.decay tr0 0.3 * 0.3
    tr0      = ID.dust '兎' KR 1.5

-- | Reverb, with 'UGen.allpassC'.
aps05 :: UGen
aps05 = centeredOut sig
  where
    sig    = foldr f sig0 [1..16::Int]
    f t x  = x + UGen.allpassC sig0 0.4 (delt t) (dect t) * 0.25
    delt s = ID.rand s 1e-4 8e-1 * mx
    dect s = ID.rand s 4e-2 4
    mx     = UGen.mouseX KR 1e-5 2 Exponential 0.1
    sig0   = UGen.sinOsc AR freq 0 * amp
    freq   = ID.tExpRand 'F' 100 2800 tr0
    amp    = UGen.decay2 tr0 1e-4 0.3 * 0.3
    tr0    = ID.dust 'D' KR 1


-- --------------------------------------------------------------------------
--
-- * March 2001
--
-- --------------------------------------------------------------------------

{-
Notes:

VOWEL SOUND AS IN... F1 F2 F3

"ee" leap 270 2300 3000
"oo" loop 300  870 2250
"i"  lip  400 2000 2550
"e"  let  530 1850 2500
"u"  lug  640 1200 2400
"a"  lap  660 1700 2400

VOWEL SOUND "EE":

     FREQ GAIN (dB)   Q
F1    270        0    5
F2   2300      -15   20
F3   3000       -9   50

-}

-- | Simple filter, using multiple band-pass filters with 400Hz, 800Hz, and
-- 1200Hz.
bpfilt01 :: UGen
bpfilt01 = centeredOut sig
  where
    sig     = sum $ map sigf [400,800,1200]
    sigf fc = UGen.resonz sig0 fc rq
    rq      = UGen.mouseX KR 1e-3 (1-1e-3) Linear 0.1
    sig0    = UGen.saw AR freq * amp
    freq    = UGen.mouseY KR 100 1000 Exponential 0.1
    amp     = UGen.decay2 tr0 1e-4 1 * 0.5
    tr0     = ID.dust 'Β' KR 4

-- | Playing with /Q/ of band-pass filter.
qfilt01 :: UGen
qfilt01 = centeredOut sig
  where
    sig  = UGen.resonz sig0 fc rq
    sig0 = ID.whiteNoise 'A' AR * 0.3
    fc   = UGen.mouseY KR 50 8000 Exponential 0.1
    rq   = UGen.mouseX KR 1e-9 10 Exponential 0.1

-- | Simple vowel sound /EEE/ filter.
vfilt01 ::
    UGen -- ^ Input source.
    -> UGen
vfilt01 source = centeredOut sig
  where
    sig  = UGen.rlpf (sum $ map mkf pars) 4000 mrq
    pars = [ (270,  UGen.dbAmp 0,     recip 0.5 * envq)
           , (2300, UGen.dbAmp (-15), recip 20 * envq)
           , (3000, UGen.dbAmp (-9),  recip 50 * envq) ]
    envq = UGen.envGen KR tr0 1 1e-9 dur DoNothing $
           UGen.envCoord [(0,0),(0.3,1),(0.75,1),(1,0)] 1 1 EnvCos
    tr0  = UGen.impulse KR (recip dur) 0
    dur  = (ID.lfNoise2 'Δ' KR (1/5) + 1) * 0.5 + 0.1
    mkf (f,a,rq) = UGen.resonz source f' rq' * a'
      where
        f'  = UGen.lag2 ((ID.lfdNoise0 a KR 5 + 1) * (2*f/5) + (f/5)) lt
        a'  = UGen.lag2 ((ID.lfdNoise0 f KR 7 + 1) * (2*a/5) + (a/5)) lt
        rq' = UGen.lag2 ((ID.lfdNoise0 rq KR 3 + 1) * (2*rq/5) + (rq/5)) lt
    lt = 1e-2
    mrq = (ID.lfNoise2 'M' KR (1/11) + 1) * 0.4 + 0.1

-- | Saw tooth signal passed to 'vfilt01'.
vfilt01_saw :: UGen
vfilt01_saw = vfilt01 (UGen.saw AR freq * amp)
  where
    freq = (ID.lfNoise2 'Ω' KR 0.84 + 1) * 80 + 280
    amp  = UGen.envGen KR tr0 1 0 1 DoNothing $
           UGen.envCoord [(0,0),(1e-1,1),(0.4,0.8),(0.8,0.1),(1,0)]
           1 1 EnvLin
    tr0  = ID.dust 'T' KR 1

-- | White noise passed to 'vfilt01'.
vfilt01_wn :: UGen
vfilt01_wn = vfilt01 (ID.whiteNoise 'A' AR * 0.3)

-- | Another vowel sound.
vfilt02 :: UGen
vfilt02 = centeredOut sig
  where
    sig  = UGen.rlpf (sum $ map mkf pars) 9000 0.5
    pars = [( 270, UGen.dbAmp 0,     recip 0.5)
           ,(1080, UGen.dbAmp (-5),  recip 10)
           ,(2300, UGen.dbAmp (-15), recip 20)
           ,(3000, UGen.dbAmp (-9),  recip 50)
           ,(6800, UGen.dbAmp (-10), recip 60)]
    tr0  = ID.dust 'D' KR mY
    sig0 = UGen.saw AR freq * amp
    freq = UGen.lag3 (ID.tExpRand 'f' f0 (f0*2) tr0) lt
    f0   = 80
    lt   = mX
    mX   = UGen.mouseX KR 0.1 2 Exponential 0.1
    mY   = UGen.mouseY KR 1 100 Exponential 0.1
    amp = UGen.lag2 amp0 (lt*0.5)
    amp0 = amp1 * (amp1 UGen.>=* 0.15)
    amp1 = ID.tRand 'a' 1e-3 0.5 (ID.coinGate 'ℵ' 0.8 tr0)
    mkf (f,a,q) = UGen.resonz sig0 f' q' * a'
      where
        f' = g (ID.tExpRand a (f*0.5) (f*2) (ID.coinGate q 0.8 tr0))
        a' = g (ID.tExpRand q (a*0.7) (a*3) (ID.coinGate f 0.8 tr0))
        q' = g (ID.tExpRand f (q*0.66) (q*3) (ID.coinGate a 0.8 tr0))
        g x = UGen.lag2 x lt

-- | Format with skirt. Mouse Y controls frquency, mouse X controls Q.
skirt01 ::
    UGen  -- ^ Source signal to filter.
    -> UGen
skirt01 source = centeredOut sig
  where
    sig  = sig0 + sig1
    sig0 = UGen.rlpf source cf q0
    sig1 = UGen.resonz source cf q1
    q0   = UGen.mouseX KR 0.1 1 Exponential 0.1
    q1   = 0.01
    cf   = UGen.mouseY KR 100 8000 Exponential 0.1

-- | Passing 'UGen.saw' to 'skirt01'.
skirt01_saw :: UGen
skirt01_saw = skirt01 (UGen.saw AR 440 * 0.3)

-- | Passing 'UGen.whiteNoise' to 'skirt01'
skirt01_wn :: UGen
skirt01_wn = skirt01 (ID.whiteNoise 'A' AR * 0.3)


-- --------------------------------------------------------------------------
--
-- * April 2001
--
-- --------------------------------------------------------------------------

-- | Simple wind instrument, amplitude controls cutoff frequency.
wind01 :: UGen
wind01 = centeredOut sig
  where
    sig  = UGen.rlpf (UGen.saw AR freq * amp) cf rq
    freq = UGen.midiCPS $ ID.tChoose 'P' trf ps
    ps   = mce $ map (+60) [-1,0,2,4,5,7,9,11,12,14,16]
    amp  = UGen.envGen KR tra ampv 0 dur DoNothing $
           UGen.envCoord [(0,0),(3e-1,1),(4e-1,0.7),(5e-1,0.6),(1,0)]
           1 1 EnvSqr
    ampv = ID.tExpRand 'A' 0.1 0.3 tr0
    tr0  = UGen.impulse KR hps 0
    tra  = ID.coinGate 'a' 0.6 tr0
    trf  = ID.coinGate 'f' 0.8 tr0
    cf   = mX * ((ampv / 0.1) ** 2)
    mX   = UGen.mouseX KR 100 400 Linear 0.1
    rq   = 0.2 * recip (ampv / 0.1)
    hps  = 1
    dur  = recip hps * dfac
    dfac = ID.tExpRand 'D' 0.35 2 tra


-- --------------------------------------------------------------------------
--
-- * May 2001
--
-- --------------------------------------------------------------------------

-- | Brass sound, with trumpet in mind.
brass01 :: IO ()
brass01 = audition $ centeredOut sig
  where
    sig   = UGen.rlpf sig0 cf rq * 0.5
    sig0  = UGen.saw AR (freq + vib) * amp * 0.5 + nz

    ampv  = UGen.dbAmp $ (ID.tExpRand 'a' 50 60 tr0 - 60)
    amp   = UGen.envGen KR tr0 ampv 0 dur DoNothing
            (ash {env_release_node = Just 3}) + trem
    ash   = UGen.envCoord alvls 1 1 EnvCub
    alvls = [(0,0),(atk,1),(0.2,0.8),(0.6,0.4),(1,0)]
    atk   = 0.01 / ampv

    trem  = (UGen.sinOsc AR aofr 0 + 1) * aolv
    aolv  = UGen.envGen KR tr0 0.0625 0 dur DoNothing
            (aosh {env_release_node = Just 2})
    aosh  = UGen.envCoord [(0,0),(0.2,0),(0.99,1),(1,0)] 1 1 EnvSqr
    aofr  = UGen.envGen KR tr0 1.8781 0 0.5 DoNothing
            (aofsh {env_release_node = Just 3})
    aofsh = UGen.envCoord [(0,0),(0.2,0),(0.6,1),(0.98,0.2),(1,0)] 1 1 EnvLin

    freq  = ID.tChoose 'f' tr0 (mce $ map UGen.midiCPS pchs)
    pchs  = foldr (\f acc -> map (+ f) degs ++ acc) [] [31,43..79]
    degs  = [0,2,4,5,7,9,11]

    vib   = UGen.lfCub KR vfreq 0 * venv
    vfreq = (ID.lfNoise2 'V' KR 1 + 1) * 2.4
    venv  = UGen.envGen KR tr0 1.5 0 1 DoNothing $
            UGen.envCoord [(0,0),(0.8,0),(1,1)] 1 1 EnvLin

    cf    = (freq * 9 * ampv * cfenv) + (freq * cfmod)
    cfenv = UGen.envGen KR tr0 2 0.5 0.6 DoNothing $
            UGen.envCoord [(0,0),(2e-1,1),(1,0.5)] 1 1 EnvSqr
    cfmod = (UGen.lfTri KR 80 0 + 1) * cmenv
    cmenv = UGen.envGen KR tr0 1 0 0.6 DoNothing $
            UGen.envCoord [(0,0),(0.05,1),(1,0)] 1 1 EnvCub
    rq    = (0.12 * recip ampv) + 0.6

    nz    = ID.mix $ UGen.resonz (ID.whiteNoise 'N' AR * namp)
            (mce [40.09,91.2,137.1]) 0.3
    namp  = UGen.envGen KR tr0 (0.1 + ampv * 0.5) 0 0.1 DoNothing $
            UGen.envCoord [(0,0),(2e-1,1),(1,0)] 1 1 EnvSin

    tr0   = UGen.toggleFF tr1
    tr1   = ID.dust 'T' KR bps
    bps   = 0.5
    dur   = recip bps * 0.9

-- | Played by 'play_brass012'.
brass012 :: UGen
brass012 = UGen.out 0 (UGen.linPan2 sig pos 1)
  where
    sig   = UGen.rlpf sig0 cf rq * 0.5
    sig0  = UGen.saw AR (freq + vib) * amp * 0.5 + nz
    ampv  = k "amp" 0.5
    amp   = UGen.envGen KR tr0 ampv 0 dur RemoveSynth
            (ash {env_release_node = Just 3}) + trem
    ash   = UGen.envCoord alvls 1 1 EnvCub
    alvls = [(0,0),(atk,1),(0.2,0.8),(0.6,0.4),(1,0)]
    atk   = 0.01 / ampv
    trem  = (UGen.sinOsc AR aofr 0 + 1) * aolv
    aolv  = UGen.envGen KR tr0 0.0625 0 dur DoNothing
            (aosh {env_release_node = Just 2})
    aosh  = UGen.envCoord [(0,0),(0.2,0),(0.99,1),(1,0)] 1 1 EnvSqr
    aofr  = UGen.envGen KR tr0 1.8781 0 0.5 DoNothing
            (aofsh {env_release_node = Just 3})
    aofsh = UGen.envCoord [(0,0),(0.2,0),(0.6,1),(0.98,0.2),(1,0)] 1 1 EnvLin
    freq  = k "freq" 440
    vib   = UGen.lfCub KR vfreq 0 * venv
    vfreq = (ID.lfNoise2 'V' KR 1 + 1) * 2.4
    venv  = UGen.envGen KR tr0 1.5 0 1 DoNothing $
            UGen.envCoord [(0,0),(0.8,0),(1,1)] 1 1 EnvLin
    cf    = (freq * 9 * ampv * cfenv) + (freq * cfmod)
    cfenv = UGen.envGen KR tr0 2 0.5 0.6 DoNothing $
            UGen.envCoord [(0,0),(2e-1,1),(1,0.5)] 1 1 EnvSqr
    cfmod = (UGen.lfTri KR 80 0 + 1) * cmenv
    cmenv = UGen.envGen KR tr0 1 0 0.6 DoNothing $
            UGen.envCoord [(0,0),(0.05,1),(1,0)] 1 1 EnvCub
    rq    = (0.12 * recip ampv) + 0.6
    nz    = ID.mix $ UGen.resonz (ID.whiteNoise 'N' AR * namp)
            (mce [40.09,91.2,137.1]) 0.3
    namp  = UGen.envGen KR tr0 (0.1 + ampv * 0.5) 0 0.1 DoNothing $
            UGen.envCoord [(0,0),(2e-1,1),(1,0)] 1 1 EnvSin
    tr0   = k "gate" 1
    dur   = k "dur" 0.9
    pos   = ID.rand 'P' (-0.5) 0.5
    k     = UGen.control KR

-- | Like 'play_sem02', but with 'brass012' synthdef.
play_brass012 :: IO ()
--
-- XXX:
-- To stop the recursive action, need to kill ghci. Or run with
-- 'Control.Concurrent.forkIO', bind thread id, then stop with
-- 'Control.Concurrent.killThread'.
--
play_brass012 = withSC3 $ do
    let name = "brass012"
        sn t nid f a d =
            [bundle t
             [Server.s_new name nid AddToTail 1
              [("freq",f),("amp",a),("dur",d)]]
            ,bundle (t+d)
             [Server.n_set nid [("gate",0)]]]
        degs   = [0,4,5,7,9]
        ofsets = [0,7,2,9,4,11,6,1,8,3,10,5]
        tmul   = 0.22
        go g0 t0 tend nid0 oidx0 =
            let (t1,g1) = randomR (1,4::Int) g0
                t2      = t0 + (tmul * fromIntegral t1)
                (nv,g2) = randomR (1,12) g1
                oidx1   | rval > 0.95 = (oidx0 + 1) `mod` 12
                        | otherwise   = oidx0
                ofset   = ofsets !! oidx1
                rval    = fst $ randomR (0,1::Double) g0
                keyl    = map (+ofset) degs
                ptcl    = foldr (\o acc -> map (+o) keyl ++ acc) [] [24,36..72]
                ptcf j  = UGen.midiCPS (ptcl !! j)
                ptcs    = map ptcf $ randomRs (0, length ptcl - 1) g0
                durs    = map exp $ randomRs (log 0.08, log 2.2) g1
                amps    = randomRs (0.2,0.8) g2
                params  = zipWith5 sn (repeat t0) [nid0..] ptcs amps durs
                os      = take nv $ params
            in  if tend > t2
               then do
                    mapM_ sendOSC $ concat os
                    pauseThreadUntil (t2 - 0.1)
                    go g2 t2 tend (nid0+nv) oidx1
               else return ()
    _ <- async $ Server.d_recv $ Server.synthdef name brass012
    now <- time
    gen0 <- lift newStdGen
    let now' = now + 1
    go gen0 now' (now'+3000) 1000 0

-- | Simple synth to play with OSC bundle, to see how bundle message behaves in
-- server side scsynth.
sem03 :: UGen
sem03 = centeredOut sig
  where
    sig  = UGen.sinOsc AR freq 0 * ampe
    freq = k "freq" 440
    ampe = UGen.envGen KR gt amp 0 1 RemoveSynth ash
    amp  = k "amp" 0.1
    ash  = ash0 { env_release_node = Just 2 }
    ash0 = UGen.envCoord [(0,0),(1e-4,1),(0.5,0.5),(0,0)] 1 1 EnvCub
    gt   = k "gate" 1
    k    = UGen.control KR

-- | Plays 'sem03'.
play_sem03 :: IO ()
play_sem03 = withSC3 $ do
    let name = "sem03"
        sn t nid d f a =
            [bundle t
             [Server.s_new name nid AddToTail 1 [("freq",f),("amp",a)]]
            ,bundle (t+d)
             [Server.n_set nid [("gate",0)]]]
        computeTime = 0.1
        go g0 t0 tend nid0 = do
            let (fr,g1) = randomR (log 50,log 17000) g0
                (a, g2) = randomR (0.01,0.2) g1
                t2      = t0 + 0.05
            if t0 < tend
               then mapM_ sendOSC (sn t0 nid0 0.001 (exp fr) a) >>
                    pauseThreadUntil (t2-computeTime) >>
                    go g2 t2 tend (nid0+1)
               else return ()
    _ <- async $ Server.d_recv $ Server.synthdef name sem03
    now <- time
    gen0 <- lift newStdGen
    let now' = now + 1
    go gen0 now' (now'+30) 1000

-- --------------------------------------------------------------------------
--
-- * June 2001
--
-- --------------------------------------------------------------------------

{-
Quote:

Once you've learned how to create a brass patch on one synth, you can recreate
it on any synth capable of doing so.

-}

-- | Brass like sound, with Moog synth in ming.
brass02 :: IO ()
brass02 = audition $ centeredOut sig
  where
    sig   = UGen.rlpf sig0 cf rq
    sig0  = UGen.saw AR freq * amp
    freq  = ID.tChoose 'f' tr0 (mce $ map UGen.midiCPS pchs) + vib
    pchs  = foldr (\oct acc -> map (+oct) degs ++ acc) [] [36,48,60,72]
    degs  = [0,2,4,5,7,9,11]
    vib   = ID.lfTri KR vfreq 0 * 3
    vfreq = (ID.lfNoise2 'V' KR 1 + 1) * 1.8
    amp   = UGen.envGen KR tr0 ampv 0 1 DoNothing ash
    ash   = (UGen.envCoord [(0,0),(2e-3,1),(0.8,1),(1,0)] 1 1 EnvCub)
            { env_release_node = Just 2 }
    ampv  = UGen.dbAmp (ID.tExpRand 'D' 50 60 tr0 - 60)
    cf    = freq * 9 * ampv * cfenv
    cfenv = UGen.envGen KR tr0 1 0 0.6 DoNothing $
            UGen.envCoord [(0,1e-1),(1e-1,1),(1,1e-1)] 1 1 EnvSqr
    rq    = 0.9
    tr0   = UGen.toggleFF tr1
    tr1   = ID.dust 't' KR bps
    bps   = 1


-- --------------------------------------------------------------------------
--
-- * July 2001
--
-- --------------------------------------------------------------------------

-- | Playing with weve form change between saw tooth and pulse.
sh101_01 :: IO ()
sh101_01 = audition $ centeredOut sig
  where
    sig  = sig0 * amp
    sig0 = UGen.mceSum $ UGen.xFade2 sig1 sig2 idx 1
    sig1 = UGen.saw AR freq
    sig2 = UGen.pulse AR freq 0.5
    freq = 440
    idx  = UGen.lfCub KR 0.1 0
    amp  = UGen.envGen KR tr0 0.2 0 1 DoNothing ash
    ash  = ash0 { env_release_node = Just 3 }
    ash0 = UGen.envCoord [(0,0),(1e-3,1),(0.3,0.5),(0.8,0.5),(1,0)]
           1 1 EnvCub
    tr0  = ID.dust 'D' KR 3 + UGen.impulse KR 1.5 0


-- --------------------------------------------------------------------------
--
-- * August 2001
--
-- --------------------------------------------------------------------------

-- | Signal with sinOscillators: freq=F amp=A phase=0, freq=3*F amp=A/9
-- phase=pi/2, and freq=5*F amp=A/25 phase=0, ...
str01 :: UGen
str01 = centeredOut sig
  where
    sig  = foldr sigf 0 [1..30] * 0.3
    sigf :: Int -> UGen -> UGen
    sigf n acc = acc + (UGen.sinOsc AR freq' phase * amp')
      where
        freq' = freq * n'
        phase = if even n then pi else pi/2
        n'    = UGen.constant (n*2-1)
        amp'  = amp / (n'**2)
    freq = UGen.midiCPS $ ID.tRand 'F' 48 72 tr0
    amp  = UGen.decay2 tr0 1e-4 1
    tr0  = ID.dust 'd' KR 1


-- --------------------------------------------------------------------------
--
-- * September 2001
--
-- --------------------------------------------------------------------------

{-
Quotes:

Each string produces a different waveform depending upon the plucking position.

The shape and hardness of the fingers or the plectrum influences the
high-frequency content of the initial waveform.

The amplitude envelope of the oscillators depends upon the direction in which
you pluck the string(s).

Strings' harmonics are 'stretched' as the pitch increases and/or the excitation
increases in amplitude.

The guitar body has many densely packed resonances and anti-resonances that
cannot be imitated using conventional equalisers or filters.

The nature of the resultant sound is determined by the listening position and
the angle between the listener and the instrument.
-}

-- | Simple sound with simple guitar chord strike.
gtr01 :: UGen
gtr01 = centeredOut sig
  where
    nS      = 6
    sig     = sum (map mks [1..nS]) * 0.3
    pchs    = foldr fp [] octs
    fp o ps = map (UGen.midiCPS . (+offset) . (+o)) degs ++ ps
    octs    = take 3 $ iterate (+24) 40
    degs    = [0,7,12,16,19]
    offset  = UGen.select oidx (mce offs)
    offs    = [0,2,4,5,7,9,11]
    oidx    = ID.tRand 'O' 0 (UGen.constant $ length offs) tr1
    -- offs    = [0,5,7,0]
    -- oidx    = UGen.pulseCount tr1 0 `UGen.modE` 4
    tr1     = ID.coinGate 'G' 0.4 tr0
    tr0     = UGen.impulse KR sps 0 +
              ID.coinGate '2' 0.3 (UGen.impulse KR (sps*2) 0)
    sps     = 0.68
    mks :: Int -> UGen
    mks n = sigN * amp
      where
        sigN  = UGen.sinOsc AR freq phs
        phs   = UGen.sinOsc AR (freq*0.9998) 0 * amp * 9
        freq  = UGen.select idx (UGen.mce pchs)
        idx   = UGen.constant (n+1)
        tr'   = UGen.tDelay tr0 (tdt * UGen.constant n)
        tdt   = (ID.lfNoise2 'C' KR (recip 5) * arpt) + (arpt + 1e-4)
        arpt  = recip sps * (1/ UGen.constant nS) * 0.125
        amp   = UGen.decay2 tr' 1e-5 dur * 0.3
        dur   = 2.1


-- --------------------------------------------------------------------------
--
-- * October 2001
--
-- --------------------------------------------------------------------------

-- | Simple synth for plucked string.
gtr02 :: UGen
gtr02 = centeredOut sig
  where
    sig  = UGen.sinOsc AR freq phs * ampe * 0.6
    ampe = UGen.envGen KR 1 amp 0 dur RemoveSynth $
           UGen.envCoord [(0,1e-9),(1e-4,1),(1,1e-9)] 1 1 EnvCub
    amp  = k "amp" 0.3
    freq = k "freq" 440
    phs  = UGen.sinOsc AR (freq*0.99998) 0 * idx
    idx  = freq ** (1/4)
    dur  = k "dur" 2
    k    = UGen.control KR

-- | Playing 'gtr02', simple chord arpegio.
play_gtr02 :: IO ()
play_gtr02 = withSC3 $ do
    let name = "gtr02"
        sn f a = Server.s_new name (-1) AddToTail 1
                 [("freq",UGen.midiCPS f), ("amp",a),("dur",1.6)]
    void $ async $ Server.d_recv (Server.synthdef name gtr02)
    now <- time
    let art  = 0.09915
        tmul = 1.785
        cI   = [ 0, 7, 12, 16, 19, 24]
        cii7 = [ 2, 5, 12, 14, 21, 24]
        cIV  = [ 0, 5, 12, 17, 20, 24]
        cdim = [ -1, 2, 8, 14, 17, 24]
        cs   = [cI,cii7,cIV,cdim]
        cads :: [(Int, [Int])]
        cads = [(0, [0,1,2,3]) -- I
               ,(1, [0,1,3])   -- ii7
               ,(2, [0,1,2,3]) -- IV
               ,(3, [0,3])]    -- dim
        amps = [0.12, 0.15, 0.18, 0.22]
        bnd t a ps = zipWith (\dt p -> bundle (now+(t*tmul)+dt) [sn p a])
                     [art,art*2..] ps
        go g0 t0 tend idx =
            let (idx',g1) = randomR (0, length nexts - 1) g0
                nexts     = snd (cads !! idx)
                (dur,g2)  = randomR (1, 2::Int) g1
                (amp,g3)  = randomR (0, length amps - 1) g2
                t1        = t0 + (1 / fromIntegral dur)
                bs        = bnd t1 (amps !! amp) $ map (+52) (cs !! idx')
            in if t1 > tend
                   then return ()
                   else mapM_ sendOSC bs >> go g3 t1 tend idx'
    gen0 <- lift newStdGen
    go gen0 0 300 0

-- | Simple cross fade between saw tooth wave and pulse wave.
fade_ex01 :: IO ()
fade_ex01 = audition $ centeredOut (sig * amp)
  where
    sig  = (bal * sig0) + ((1-bal) * sig1)
    sig0 = UGen.pulse AR freq 0.5
    sig1 = UGen.saw AR freq
    freq = 440
    bal  = UGen.sinOsc KR mY 0 * 0.5 + 0.5
    amp  = 0.3
    mY   = UGen.mouseY KR 1e-1 freq Exponential 0.1

-- | Playing with changing pulse width.
pulseWidth :: IO ()
pulseWidth = audition $ centeredOut sig
  where
    sig  = UGen.pulse AR freq wdth * amp
    wdth = ID.lfNoise2 'W' KR (1/5) * 0.5 + 0.5
    freq = ID.lfNoise2 'f' KR (1/12) * 800 + 850
    amp  = UGen.decay2 tr0 1e-4 1 * 0.1
    tr0  = tr1 + tr2
    tr1  = ID.dust 'K' KR 2
    tr2  = UGen.impulse KR tr2f 0
    tr2f = ID.lfNoise2 'F' KR (1/11) * 3.5 + 3.51


-- --------------------------------------------------------------------------
--
-- * November 2001
--
-- --------------------------------------------------------------------------

-- | Simple inharmonic percussive instrument with additive synthesis. Mouse
-- button triggers a sound, mouse X controls pitch, and mouse Y controls
-- duration.
kettle01 :: IO ()
kettle01 = audition $ centeredOut sig
  where
    sig  = (sig0 + sig1) * 0.2
    sig0 = sum $ map f [1, 1.50, 1.98, 2.44, 3.16]
    sig1 = UGen.rlpf (ID.whiteNoise 'A' AR) 3000 0.2 * ampz
    ampz = UGen.envGen KR tr0 1 0 (dur * 0.1) DoNothing $
           UGen.envCoord [(0,0),(1e-8,1),(0.1,0.9),(1,0)] 1 1 EnvCub
    dur  = UGen.mouseY KR 1e-1 2 Exponential 0.1
    freq = UGen.mouseX KR 100 800 Exponential 0.1
    tr0  = UGen.mouseButton KR 0 1 0.1
    f rat  = UGen.sinOsc AR (freq*rat) 0 * ampl
      where
        ampl = UGen.envGen KR tr0 1 0 (durl * dur) DoNothing $
               UGen.envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvSqr
        durl = 1.8 - (abs (2.2 - rat))


-- --------------------------------------------------------------------------
--
-- * December 2001
--
-- --------------------------------------------------------------------------

-- | AM with saw tooth and saw tooth.
sawSaw :: UGen
sawSaw = centeredOut sig
  where
    sig = sig1 * sig2 * amp
    sig1 = UGen.saw AR freq1
    sig2 = UGen.saw AR freq2 * 0.5 + 0.5
    amp  = 0.3
    freq1 = UGen.mouseX KR 100 400 Exponential 0.1
    freq2 = UGen.mouseY KR 40  100 Exponential 0.1

-- | Simple kettle drum, take 2.
kettle02 :: UGen
kettle02 = centeredOut sig
  where
    sig   = UGen.hpf (sig0 + sig1) 50 * mamp
    sig0  = sum $ zipWith f0 [1,1.50,1.98,2.44] [5,4,3,1]
    sig1  = UGen.rhpf (sig10 * sig11 * amp1) 87 0.1
    sig10 = UGen.saw AR 100
    sig11 = UGen.saw AR 87 * 0.5 + 0.5
    amp1  = UGen.envGen KR tr0 1 0 (dur*0.1) DoNothing $
            UGen.envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub
    dur   = UGen.mouseX KR 0.1 2 Exponential 0.1
    mamp  = ID.tExpRand '0' 0.04 0.12 tr0
    tr0   = tr1 + tr2 + tr3 + tr4
    hps   = 1.57
    tr1   = ID.coinGate '1' 0.95 (UGen.impulse KR hps 0)
    tr2   = ID.coinGate '2' 0.48 (UGen.impulse KR (hps*2) 0)
    tr3   = ID.coinGate '3' 0.23 (UGen.impulse KR (hps*4) 0)
    tr4   = ID.dust '4' KR dfreq
    dfreq = ID.lfNoise2 '5' KR (1/13) * 4 + 4.01
    freq0 = 150

    f0 freq amp = UGen.sinOsc AR (freq*freq0) 0 * a
      where
        a = UGen.envGen KR tr0 (amp*0.5) 0 dur DoNothing $
            UGen.envCoord [(0,0),(1e-5,1),(1,0)] 1 1 EnvCub


-- --------------------------------------------------------------------------
--
-- * For testing elisp
--
-- --------------------------------------------------------------------------

-- | Place cursor inside the body of 'act01' source, hit "C-M-<return>" in emacs
-- to send the declaration to inferior haskell.
act01 :: IO ()
act01 = do
    putStrLn "============================================="
    putStrLn "invoking top-level IO declaration with emacs"
    putStrLn "============================================="

-- | Simple action to make sound, to test elisp function
-- /inferior-haskell-send-and-eval-decl/.
act02 :: IO ()
act02 = audition (centeredOut sig)
  where
    sig  = UGen.sinOsc AR freq 0 * amp
    freq = ID.tExpRand 'F' 50 8000 1
    dur  = ID.tExpRand 'd' 1e-1 3 1
    amp  = UGen.envGen KR 1 0.3 0 dur RemoveSynth $
           UGen.envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub

-- | From @haskell-sc-scratch/SCHelp/SPE/Part1.hs@.
help_SPE1 :: IO ()
help_SPE1 = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap`newStdGen
  let nz = UGen.midiCPS (ID.lfNoise1 'a' KR 1 * 36 + 110)
      evl = UGen.envGen KR 1 1 0 1 RemoveSynth shp * 0.3
      shp = UGen.envPerc 0.01 1
      v = UGen.rlpf (UGen.lfSaw AR (k "freq" 440) 0 * evl) nz 0.1
      f a b = UGen.allpassN b 0.05 a 4
      sig = foldr f v (take 4 $ zipWith UGen.mce2 dl dr)
      k = UGen.control KR
  audition $ UGen.out 0 sig

-- | Playing with 'UGen.envGen'.
--
-- There's more to understand in 'UGen.envGen'.
env_ex02 :: IO ()
env_ex02 = audition $ centeredOut sig
  where
    sig  = UGen.sinOsc AR freq 0 * amp
    freq = 440
    amp = UGen.envGen KR tr0 0.3 0 1 DoNothing ash'
    ash' = ash { env_release_node = Just 4
               , env_loop_node    = Just 1 }
    ash  = UGen.envCoord lvls 1 1 EnvLin
    lvls = [(0,0),(1e-4,1),(0.75,0.05),(0.85,0.5),(1,0)]
    tr0  = ID.lfdNoise0 'T' KR hps ID.>=* 0
    hps  = 2.5
