-- |
-- Copyright    : 8c6794b6, 2014
-- License      : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scratch written while reading year 2002 synth secrets articles.
--
module Sound.Study.ForSynthSecrets.Y2002 where

import           Control.Monad.Trans.Class (lift)
import           Data.List (nub, zipWith4)
import           System.Random
    (getStdRandom, mkStdGen, newStdGen , randomR, randomRs)
import           System.Random.Shuffle (shuffle')

import           Sound.OSC (Message, bundle, sendOSC, time)

import           Sound.SC3
import           Sound.SC3.ID
import qualified Sound.SC3.UGen.Monad as M

import           Sound.Study.ForSynthSecrets.Y1999 (centeredOut)
import           Sound.Study.ForSynthSecrets.Y2000 (soundFile05)


-- --------------------------------------------------------------------------
--
-- * January 2002
--
-- --------------------------------------------------------------------------

{-
Quotes:

The frequency components of a kick drum approximate a harmonic spectrum at low
frequencies, with a large number of densely packed enharmonic components at mid
and high frequencies.
-}

-- | Simple bass drum, done with additive synthesis of sine tones.
bd01 :: UGen
bd01 = centeredOut sig
  where
    sig  = sig0 * amp
    sig0 = sum $ map f0 $ take 6 $ map (+7) [43,86..]
    amp  = 0.2
    dur  = 0.3
    tr0  = impulse KR 1 0
    f0 f = sinOsc AR f 0 * decay2 tr0 1e-3 (dur*43/f)

-- | Pitched tone without fundamental frequency.
noFundamental01 :: UGen
noFundamental01 = centeredOut sig
  where
    sig  = sig0 * amp * anz
    sig0 = mceSum $ sum $ map f0 [2,3,4,6,7,8,9,11,12,13]
    f0 p = sinOsc AR (fund*p) 0 *
           decay2 tr0 1e-4 (dur*recip p) * 0.2
    dur  = 1.5
    fund = midiCPS $ select idx (mce ps)
    idx  = latch (lfdNoise3 'i' KR ifrq * lp + lp) tr0
    ifrq = lfNoise2 'F' KR 1 * 2 + 2
    lp   = constant (length ps) / 2
    ps   = foldr (\o acc -> map (+o) degs ++ acc) [] [60,72]
    -- degs = [0,4,5,10,11]
    degs = [0,3,5,7,10]
    amp  = envGen KR tr0 ampv 0 2 DoNothing $
           envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub
    ampv = tExpRand 'A' 0.3 0.6 tr0
    anz  = lfdNoise1 'a' KR (1/3) * 0.3 + 0.7
    tr0  = tr1 + tr2 + tr3
    tr1  = impulse KR hps 0
    tr2  = impulse KR tf2 0
    tf2  = envGen KR (coinGate 'G' 0.15 tr1) 12 1 3 DoNothing $
           envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub
    tr3  = coinGate '$' 0.5 (impulse KR (hps*8) 0)
    hps  = 0.5

-- | Play 'noFundamental01'
play_noFundamental01 :: IO ()
play_noFundamental01 = audition noFundamental01

-- | Another pitched tone without fundamental frequency.
noFundamental02 :: UGen
noFundamental02 = mrg [sig0, sig1]
  where
    sig0  = out 0 (sum (map f0 parts) * amp)
    sig1  = out 1 (sinOsc AR fund 0 * amp)
    parts = take 10 $ nub $ randomRs (2,30::Int) $ mkStdGen 0x832731
    f0 p  = sinOsc AR (constant p*fund) 0 *
            (lfNoise2 (constant p) KR 5 * 0.5 + 0.5)
    fund  = mouseY KR 30 1000 Exponential 0.1
    amp   = 0.2

-- | Example of frequency shifter, take 1.
fshift_ex01 :: IO ()
fshift_ex01 = audition $ centeredOut sig
  where
    sig = freqShift i s 0 * 0.1
    i   = sinOsc AR 100 0
    s   = xLine KR 1 500 5 RemoveSynth

-- | 'freqShift' example, take 2.
fshift_ex02 :: IO ()
fshift_ex02 = audition $ centeredOut sig
  where
    sig = freqShift i s 0 * 0.1
    i   = klang AR 1 0 d
    d   = klangSpec [101, 303, 606, 808] [1,1,1,1] [1,1,1,1]
    s   = xLine KR 1 500 5 RemoveSynth

-- | Frequency shifter example, take 3.
fshift_ex03 :: IO ()
fshift_ex03 = audition $ centeredOut sig
  where
    sig = freqShift i (s * 1500) p * 0.1
    i   = sinOsc AR 10 0
    s   = lfNoise2 'a' AR 0.3
    p   = linLin (sinOsc AR 500 0) (-1) 1 0 (2 * pi)

-- | Frequency shifter example, 4th.
fshift_ex04 :: IO ()
fshift_ex04 = audition $ centeredOut sig
  where
    sig = freqShift i s 0 * 32
    i   = bpf n1 1000 0.001
    n1  = whiteNoise 'a' AR
    s   = n2 * 1000
    n2  = lfNoise0 'a' AR 5.5

-- | Frequency shifter example, 5th.
fshift_ex05 :: IO ()
fshift_ex05 = audition $ centeredOut sig
  where
    sig = mrg [out 0 s, localOut z]
    s   = freqShift a (lfNoise0 'a' KR (1/4) * 90) 0
    a   = o / 4 + localIn 2 AR
    o   = blip AR 60 4 * e
    e   = lfGauss AR 4 (1/8) 0 Loop DoNothing
    z   = delayC s 1 0.1 * 0.9

-- | Frequency shifter example, 6th.
fshit_ex06 :: IO ()
fshit_ex06 = withSC3 $ do
    let bnum :: Num a => a
        bnum = 99
        n    = 1
        sig  = freqShift i s 0
        i    = diskIn n bnum Loop
        s    = lfNoise2 's' KR sfrq * 1000
        sfrq = mouseY KR (1/8) 8 Exponential 0.1
    _ <- async $ b_alloc bnum 65536 n
    _ <- async $ b_read bnum soundFile05 0 (-1) 0 True
    play $ centeredOut sig

-- | Frequency shifter example, 7th.
fshift_ex07 :: IO ()
fshift_ex07 = audition $ centeredOut sig
  where
    sig = freqShift i s 0 * 0.1
    i   = mceSum $ sinOsc AR (mce [110,220,330,440]) 0
    s   = lag2 (lfPulse KR pf 0 0.5 * 37) 0.3
    pf  = lfNoise2 'f' KR 1 + 1.01

-- | Simple bass drum, take 2.
bd02 :: UGen
bd02 = centeredOut sig
  where
    sig   = (sig0 + sig1) * mamp * 0.3
    sig0  = freqShift sigI 7 0 * amp0
    sigI  = rlpf (saw AR ifreq) 500 0.99 * 10
    amp0  = decay2 tr0 1e-3 dur
    ifreq = 50 * fenv
    fenv  = envGen KR tr0 1 0 dur DoNothing $
            envCoord [(0,0),(1e-5,1),(1e-4,1),(1,0.5)] 1 1 EnvExp
    dur   = 0.125
    sig1  = lpf (hpf sigT 500) 12000 * 0.8
    sigT  = sinOsc AR cfreq phase * amp1
    cfreq = 1100
    phase = sinOsc AR 526 0 * 6
    amp1  = decay2 tr0 0.001 0.008
    mamp  = tExpRand 'a' 0.08 0.3 tr0
    tr0   = bassTrigger 120

-- | Play 'noFundamental01' and 'bd02'.
play_bd02AndNonfund :: IO ()
play_bd02AndNonfund = audition $ mrg [bd02, noFundamental01]


-- --------------------------------------------------------------------------
--
-- * Feburary 2002
--
-- --------------------------------------------------------------------------

-- | Simple bass drum, take 3.
bd03 :: UGen
bd03 = centeredOut sig
  where
    sig  = (sig0 + sig1) * 0.3
    sig0 = sinOsc AR freq pi * amp0
    amp0 = envGen KR tr0 1 0 0.75 DoNothing ash0
    ash0 = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub
    freq = amp0 * 50
    sig1 = rhpf (whiteNoise 'S' AR) 800 0.4 * amp1
    amp1 = envGen KR tr0 0.3 0 0.002 DoNothing ash1
    ash1 = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvSqr
    tr0  = bassTrigger 120

-- | Play 'noFundamental01' and '
play_bd03AndNonfund :: IO ()
play_bd03AndNonfund = audition $ mrg [bd03, noFundamental01]

-- | Trigger for simple bass drums.
bassTrigger ::
    UGen    -- ^ Beats per minute.
    -> UGen
bassTrigger bpm = tr1 + tr2
  where
    tr1 = impulse KR (bpm/60) 0
    tr2 = coinGate 'K' 0.078 (impulse KR (4*bpm/60) 0)

-- | Simple bass drum, take 4.
bd04 :: UGen
bd04 = centeredOut sig
  where
    sig   = (sig0 + sig1) * 0.125
    sig0  = sum (map f0 freqs) * amp0
    amp0  = envGen KR tr0 1 0 dur0 DoNothing $
            envCoord [(0,0),(1e-9,1),(1,0)] 1 1 EnvCub
    freqs = take 5 $ map (+ (-7)) [50,100..]
    f0 x  = sinOsc AR ((lfdNoise1 x KR 1*0.25+0.51)*x*amp0) 0
    dur0  = lfNoise2 '0' KR (1/5) * 0.4 + 0.45
    sig1  = resonz (whiteNoise 'S' AR) cf rq * amp1
    amp1  = envGen KR tr0 (1/5) 0 dur1 DoNothing $
            envCoord [(0,0),(1e-8,1),(1-1e-8,1),(1,0)] 1 1 EnvCub
    dur1  = lfNoise2 '1' KR (1/5) * 0.008 + 0.008
    cf    = lfdNoise3 '3' KR (1/5) * 4000 + 4000
    rq    = lfdNoise1 'A' KR (1/5) * 0.5 + 0.46
    tr0   = bassTrigger 120

-- | Play 'noFundamental01' and 'bd04'.
play_bd04AndNonfund :: IO ()
play_bd04AndNonfund = audition $ mrg [bd04, noFundamental01]

-- | Another simple bass drum.
--
-- Contains low-pass filter from high frequency to low, with short duration.
--
bd05 :: UGen -> UGen
bd05 bpm = centeredOut sig
  where
    sig   = rlpf (sig1+sig2+sig3) cf rq * 0.6
    sig1  = sinOsc AR cfreq phase * amp1 * 0.2
    cfreq = 1250
    phase = sinOsc AR 870 0 * 5
    amp1  = envGen KR tr0 1 0 0.005 DoNothing ash
    amp2  = envGen KR tr0 1 0 0.5 DoNothing ash
    ash   = envCoord [(0,0),(1e-5,1),(1,0)] 1 1 EnvCub
    cf    = 22000 * amp1 + 65
    rq    = 0.999
    sig2  = mceSum $ saw AR (mce [50,93]) * amp2
    sig3  = lpf (whiteNoise 'a' AR) 8000 * trig1 tr0 0.02
    tr0   = bassTrigger bpm

-- | Play 'noFundamental01' and 'bd05'.
play_bd05AndNonfund :: IO ()
play_bd05AndNonfund = audition $ mrg [bd05 120, noFundamental01]

-- | Sweeping low-pass filter applied to saw tooth wave.
sawToSine :: UGen
sawToSine = centeredOut sig
  where
    sig  = rlpf sig1 cf rq * env0
    sig1 = saw AR freq
    freq = tExpRand 'f' 80 800 tr0
    cf   = 12000 * env1 + freq
    rq   = 0.999
    tr0  = dust 't' KR 2
    env0 = envGen KR tr0 0.3 0 2 DoNothing esh
    env1 = envGen KR tr0 1 0 0.1 DoNothing esh
    esh  = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub


-- --------------------------------------------------------------------------
--
-- * March 2002
--
-- --------------------------------------------------------------------------

-- | Simple snare, take 1.
--
-- Using pitched tone with frequency shifter applied to triangle waves, plus two
-- sine waves, and low pass filter applied to white noise, cutoff frequency
-- controlled by velocity of the entire sound.
--
snr01 :: UGen -> UGen
snr01 bpm = centeredOut sig
  where
    sig    = ((sig0*ampp) + (sig1*(1-ampp))) * mamp
    sig0   = rlpf (rhpf (sig00 + sig01) 180 0.8) 800 0.9
    sig00  = (f00 175 + f00 224) * amp00
    f00 x  = freqShift (lfTri AR 111 0) x 0
    amp00  = envGen KR tr0 1 0 (dur*0.5) DoNothing $ envPerc 1e-4 1
    sig01  = (f01 330 + f01 180) * amp01
    f01 x  = sinOsc AR x 0
    amp01  = envGen KR tr0 1 0 (dur*0.5) DoNothing $ envPerc 1e-4 1
    dur    = 0.25
    sig1   = foldr f1 (rlpf nz cf rq) [653,1329,2823] * amp1
    f1 x y = bBandStop y x 0.1
    nz     = whiteNoise 'a' AR
    amp1   = decay2 tr0 0.001 dur
    cf     = 7850 * mamp
    rq     = 0.8
    mamp   = tExpRand 'A' 0.3 0.6 tr0
    ampp   = 0.2
    tr0    = snrTrigger bpm

-- | Trigger used for simple snare sounds.
snrTrigger ::
    UGen    -- ^ Beats per minute.
    -> UGen
snrTrigger bpm = tr1 + tr2
  where
    tr1 = impulse KR ((bpm*0.5)/60) 0.5
    tr2 = coinGate 'n' 0.12 (impulse KR (bpm*2/60) 0.5)

-- | Play 'bsd05' and 'snr01'.
play_bs01 :: IO ()
play_bs01 = let bpm = 120 in audition $ mrg [bd05 bpm, snr01 bpm]


-- --------------------------------------------------------------------------
--
-- * April 2002
--
-- --------------------------------------------------------------------------

-- | Prepare the buffer used in 'snr02'.
prepare_snr02 :: IO Message
prepare_snr02 = withSC3 $ do
    let bufn :: Num a => a
        bufn = 10
        bgen a = do
            _ <- async $ b_alloc bufn 512 1
            async $ b_gen_cheby 10 [Wavetable] a
    bgen $ take 8 $ randomRs (0,1) (mkStdGen 0x983721)

-- | Simple snare sound inspired from TR909 patch.
snr02 :: UGen -> UGen
snr02 bpm = centeredOut sig
  where
    sig   = sig0 + sig1
    sig0  = rhpf sig00 200 0.3
    sig00 = mceSum (shaper bufn sig01 * amps) * 0.5
    sig01 = sinOsc AR frqs 0 * (lfNoise2 's' KR 15 * 0.48 + 0.48)
    frqs  = mce [51,89]
    amp0  = decay2 tr0 1e-4 0.15 * 0.6
    amp1  = decay2 tr0 1e-4 0.13 * 0.5
    tr0   = snrTrigger bpm
    amps  = mce [amp0,amp1]
    sig1  = sig10 + sig11
    sig10 = rlpf nz 3800 0.73 * amp10
    sig11 = rhpf sig10 400 0.18 * amp11
    amp10 = decay2 tr0 1e-3 0.25
    amp11 = decay2 tr0 1e-3 0.15
    nz    = whiteNoise 'a' AR * 0.3
    bufn  :: Num a => a
    bufn  = 10

-- | Play 'bd05' and 'snr02'.
play_bd05snr02 :: IO ()
play_bd05snr02 = let bpm = 120 in audition $ mrg [bd05 bpm, snr02 bpm]

-- | Example of 'bBandStop'.
bndstop_ex01 :: IO ()
bndstop_ex01 = audition $ centeredOut sig
  where
    sig = foldr f0 sig0 [('B',259),('P',1111),('F',12000)] * 0.8
    f0 (a,b) c = bBandStop c b' bw
      where
        b' = lfdNoise3 (succ a) KR 3 * b + b
        bw = lfdNoise3 a KR 2 * 3 + 3.05
    sig0 = whiteNoise 'a' AR

-- | Simple snare, inspired from TR808 patch.
snr03 :: UGen -> UGen
snr03 bpm = centeredOut sig
  where
    sig   = (sig0 + sig1) * ampm
    sig0  = mceSum (sinOsc AR freqs 0 * amp0)
    freqs = mce [283,331] * fenv
    fenv  = envGen KR tr0 1 0 (dur*1.3) DoNothing $
            envCoord [(0,1),(1e-4,1),(1,0)] 1 1 EnvSqr
    amp0  = envGen KR tr0 0.3 0 dur DoNothing $ envPerc 1e-4 1
    dur   = 0.1
    tr0   = snrTrigger bpm
    sig1  = rhpf nz cf 0.8 * amp1
    nz    = whiteNoise 'A' AR
    amp1  = envGen KR tr0 0.5 0 dur DoNothing $ envPerc 1e-3 1
    cf    = (5500 * fenv) + 100
    ampm  = 0.4

-- | Play 'bsd03' and 'snr03'.
play_bd05snr03 :: IO ()
play_bd05snr03 = audition $ mrg [bd05 120, snr03 120]


-- --------------------------------------------------------------------------
--
-- * May 2002
--
-- --------------------------------------------------------------------------

-- | Playing with sweeping noise.
cym01 :: UGen
cym01 = centeredOut sig
  where
    sig  = rhpf (rlpf sig1 cfl rql) cfh rqh * amp
    sig1 = whiteNoise 'C' AR
    cfl  = envGen KR tr0 12000 8000 dur DoNothing $
           envCoord [(0,0),(1e-4,0.5),(0.02,1),(1,0.3)] 1 1 EnvCub
    rql  = e0 $ envCoord [(0,0.9),(1e-4,0.3),(0.03,0.5),(1,0.9)] 1 1 EnvLin
    cfh  = envGen KR tr0 3000 1000 dur DoNothing $
           envCoord [(0,0.2),(0.5,1),(1,0.1)] 1 1 EnvCub
    rqh  = e0 $ envCoord [(0,0.1),(1e-4,0.5),(0.05,0.9),(1,0.8)] 1 1 EnvLin
    dur  = 0.5
    e0   = envGen KR tr0 1 0 dur DoNothing
    amp  = decay2 tr0 5e-3 dur * 0.3 * ampr
    ampr = tExpRand 'A' 0.1 1 tr0
    tr0  = tr1 + tr2
    tr1  = impulse KR 4 0
    tr2  = coinGate 'G' 0.5 (impulse KR 8 0)


-- --------------------------------------------------------------------------
--
-- * June 2002
--
-- --------------------------------------------------------------------------

-- | FM with two 'lfPulse's. Generates quite flat spectrum from
-- low to high frequency.
cym02 :: UGen
cym02 = centeredOut sig
  where
    sig   = (sig0 + sig1) * ampm
    sig0  = f0 car * amp0 * 1.5
    car   = lfPulse AR (1049 + mdl) 0 0.5 - 0.5
    mdl   = lfPulse AR mfreq 0 0.5 * mfreq * idx
    idx   = 28.73
    mfreq = 1831
    f0 x  = resonz x bf rq0
    rq0   = amp0
    bf    = 17831
    amp0  = envGen KR tr0 1 0 0.2 DoNothing $
            envPerc 1e-3 1
    sig1  = rhpf car 20 rq1 * amp1 * 0.8
    amp1  = envGen KR tr0 1 0 3.7 DoNothing $
            envCoord [(0,0),(0.12,1),(0.3,1),(1,0)] 1 1 EnvCub
    rq1   = amp1
    ampm  = decay2 tr0 1e-9 0.75 * 0.03
    tr0   = cymTrigger 120

-- | Trigger used for cymbal sound.
cymTrigger ::
    UGen    -- ^ Beats per minute.
    -> UGen
cymTrigger bpm = tr1 + tr2
  where
    tr1 = coinGate 'y' 0.96 (impulse KR (2*bpm/60) 0)
    tr2 = coinGate 'c' 0.88 (impulse KR (4*bpm/60) 0)

-- | Play rhythm, take 1.
play_rhy01 :: IO ()
play_rhy01 = audition $ mrg [bd05 60, snr01 60, cym02, noFundamental01]


-- --------------------------------------------------------------------------
--
-- * July 2002
--
-- --------------------------------------------------------------------------

-- | Buffer number used in 'cym03'.
cym03_bufn :: Num a => a
cym03_bufn = 99

-- | Prepare buffer for 'cym03'.
prepare_cym03 :: IO Message
prepare_cym03 = withSC3 $ do
    let path = "/home/atsuro/sound/wav/cymbal_48000.wav"
    async $ b_allocRead cym03_bufn path 0 0

-- | Simple cymbal using sound file playback.
--
-- Sound file was extraction of cymbal recorcing, done with /snd/ editor.
cym03 :: UGen
cym03 = centeredOut sig
  where
    sig  = sig1 * amp
    sig1 = playBuf 1 AR cym03_bufn scl tr0 0 NoLoop DoNothing
    scl  = bufRateScale KR cym03_bufn
    amp  = envGen KR tr0 0.3 0 1 DoNothing $
           envCoord [(0,1),(1e-4,1),(0.98,1),(1,0)] 1 1 EnvSin
    tr0  = cymTrigger 120

-- | Play rhythm, take 2.
play_rhy02 :: IO ()
play_rhy02 = audition $ mrg [bd05 60, snr01 60, cym03, noFundamental01]

-- | Simple cymbal, inspired from TR808.
cym04 :: UGen
cym04 = centeredOut sig
  where
    sig   = (sig0 + sig1) * 0.2
    sig0  = (sig00 + sig01)
    sig00 = resonz plss (8971+107) (cf01*rq00+0.1) * cf00
    rq00  = 0.1
    cf00  = decay2 tr0 1e-4 0.2
    sig01 = rhpf plss (1333+33) (cf01*rq01+0.1) * cf01
    rq01  = 0.1
    cf01  = decay2 tr0 1e-4 0.4
    f0 x  = pulse AR x 0.5 * 0.3
    sig1  = resonz plss (687+10) (cf1*rq1+0.1) * cf1
    cf1   = decay2 tr0 1e-4 1.5
    rq1   = 0.1
    plss  = sum (map f0 [83, 117, 143, 219, 307, 513]) * 0.3
    tr0   = cymTrigger 120

-- | Plays rhythm, take 3.
play_rhy03 :: IO ()
play_rhy03 = audition $ mrg [bd05 60, snr01 60, cym04, noFundamental01]

-- | Simple cymbal, take 5.
--
-- Summing up 3 FM using 'lfPulse' for both carrier and
-- modulator. Resulting signal is passed to low pass filter instead of high pass
-- filter for sustained part.
--
cym05 :: UGen
cym05 = centeredOut sig
  where
    sig   = (sig0 + sig1) * ampm
    sig0  = resonz car bf rq0 * amp0 * 2.5
    car   = sum $ map fc [813, 1049, 1517]
    fc x  = lfPulse AR (x + mp) 0 0.5 - 0.5
      where
        mp = lfPulse AR mf 0 0.5 * mf * idx
        mf = x * det
    det   = 1.8813
    idx   = 3.73
    rq0   = amp0
    bf    = 17831
    amp0  = envGen KR tr0 1 0 0.2 DoNothing $
            envPerc 1e-3 1
    sig1  = rlpf car 7800 rq1 * amp1 * 0.6
    amp1  = envGen KR tr0 1 0 3.7 DoNothing $
            envCoord [(0,0),(0.12,1),(0.3,1),(1,0)] 1 1 EnvCub
    rq1   = amp1
    ampm  = decay2 tr0 1e-9 0.75 * ampr * 0.01
    ampr  = tExpRand 'R' 0.5 1 tr0
    tr0   = cymTrigger 120

-- | Plays rhythm, take 4.
play_rhy04 :: IO ()
play_rhy04 = audition $ mrg [bd05 60, snr01 60, cym05, noFundamental01]


-- --------------------------------------------------------------------------
--
-- * August 2002
--
-- --------------------------------------------------------------------------

-- | Simple bell sound.
bell01 :: UGen
bell01 = centeredOut (sig * amp)
  where
    sig   = ((sig0*0.2) + (sig1*0.3) * alf) + (sig2*0.5)
    sig0  = sum (map f0 [2, 3.0102, 4.1653, 5.4317, 6.7974, 8.2159]) * amp0
    f0 x  = sinOsc AR (x*freq) 0
    amp0  = envGen KR tr0 1 0 dur DoNothing $
            envCoord [(0,0),(0.001,1),(1,0)] 1 1 EnvSin
    freq  = mceSum $ midiCPS $ tChoose 'p' tr0 (mce ps)
    ps    = foldr (\o acc -> map (+o) degs ++ acc) [] [48,60,72]
    degs  = [0,2,4,5,7,9,11]
    sig1  = sinOsc AR freq phs * amp1
    phs   = sinOsc AR (freq*0.5006) 0 * 8
    amp1  = envGen KR tr0 1 0 (dur*2) RemoveSynth $
            envCoord [(0,0),(0.001,1),(1,0)] 1 1 EnvCub
    alf   = sinOsc KR alff 0 * 0.5 + 0.5
    alff  = decay2 tr0 1e-4 8 * 3
    sig2  = (sig20 + sig21) * amp2
    sig20 = sinOsc AR (freq*0.9938) 0
    sig21 = sinOsc AR (freq*2.0237) 0
    amp2  = envGen KR tr0 1 0 (dur*1.8) DoNothing $
            envCoord [(0,0),(0.2,0),(0.4,1),(1,0)] 1 1 EnvSin
    tr0   = control KR "gate" 1
    amp   = control KR "amp" 0.1
    dur   = control KR "dur" 1.2

-- | Plays 'bell01'.
play_bell01 :: IO ()
play_bell01 = withSC3 $ do
    let name = "bell01"
        sn t = bundle t [s_new name (-1) AddToTail 1
                         [("amp",0.1),("dur",1.2)]]
        dmul = 0.25
        go g0 t0 tend
            | tend < t0 = []
            | otherwise =
                let (len,g1) = randomR (1,4) g0
                in  sn t0 : go g1 (t0+(dmul*len)) tend
    _ <- async $ d_recv $ synthdef name bell01
    now <- time
    g0 <- lift newStdGen
    mapM_ sendOSC $ go g0 now (now+300)


-- --------------------------------------------------------------------------
--
-- * September 2002
--
-- --------------------------------------------------------------------------

-- | Simple cowbell inspired from TR808.
cowbell01 :: UGen
cowbell01 = centeredOut sig
  where
    sig  = sig0 * amp
    sig0 = bBandPass (f0 587 + f0 845) cf rq
    cf   = lfdNoise3 'C' KR (1/3) * 500 + 3300
    rq   = tExpRand 'Q' 0.7 0.9 tr0
    f0 x = lfTri AR x 0
    amp  = envGen KR tr0 ampv 0 0.3 DoNothing $
           envCoord [(0,0),(1e-3,1),(1e-2,0.8),(1,0)] 1 1 EnvCub
    ampv = tExpRand 'Α' 0.79 1 tr0
    tr0  = impulse KR (bpm/60) 0 +
           coinGate 'R' 0.32 (impulse KR (4*bpm/60) 0)
    bpm  = 120

-- | Simple cowbell, take 2.
--
-- Using low pass filter with low rQ, to make pitched oscillation.
--
cowbell02 :: UGen
cowbell02 = centeredOut sig
  where
    sig  = f0 (sig0*amp0 + sig1*amp1) * 0.3
    f0 x = rlpf x cf 0.01
    cf   = tExpRand 'C' 845 849 tr0
    sig0 = lfTri AR freq 0
    freq = tExpRand 'F' 585 589 tr0
    amp0 = envGen KR tr0 ampv 0 dur DoNothing $
           envCoord [(0,0),(1e-4,1),(1e-1,0.3),(1,0)] 1 1 EnvCub
    ampv = tExpRand 'V' 0.5 1 tr0
    sig1 = pinkNoise 'P' AR
    amp1 = decay2 tr0 1e-4 0.05
    dur  = 0.5
    tr0  = impulse KR (bpm/60) 0 +
           coinGate 'R' 0.32 (impulse KR (4*bpm/60) 0)
    bpm  = 120

-- | Simple claves, take 1.
claves01 :: UGen -> UGen
claves01 bpm = centeredOut sig
  where
    sig  = f0 sig0 * amp
    f0 x = rlpf x 2000 0.01
    sig0 = lfPulse AR 637 0 0.5 * amp0
    amp0 = decay2 tr0 1e-4 1e-2 * ar0
    ar0  = tExpRand 'r' 0.5 1 tr0
    amp  = 0.5
    tr0  = impulse KR (bpm/60) 0 +
           coinGate 'L' 0.15 (impulse KR (bpm/60) 0.25) +
           coinGate 'L' 0.35 (impulse KR (bpm/60) 0.5) +
           coinGate 'L' 0.12 (impulse KR (bpm/60) 0.75)

-- | Play rhythm, take 6.
play_rhy06 :: IO ()
play_rhy06 = audition $ mrg [bd05 120, snr01 120, cym05, claves01 120]


-- --------------------------------------------------------------------------
--
-- * October 2002
--
-- --------------------------------------------------------------------------

-- | Simple piano-like sound, take 1.
pn01 :: UGen
pn01 = centeredOut sig
  where
    sig  = f0 (sig0 + sig1) * 0.1
    f0 x = rlpf x cf rq
    cf   = envGen KR tr0 (freq*4) freq (dur1*0.8) DoNothing $
           envCoord [(0,1),(1e-5,0.5),(1,1)] 1 1 EnvCub
    rq   = 0.5
    sig0 = sum (map f1 [freq, freq*0.999978, freq*1.000023]) * amp0
    f1 x = lfTri AR x 0
    amp0 = envGen KR tr0 v0 0 dur0 RemoveSynth $
           envCoord [(0,0),(1e-4,1),(0.6,0.3),(1,0)] 1 1 EnvCub
    v0   = tExpRand 'P' 0.3 0.5 tr0
    dur0 = 0.2 + (150/freq)
    freq = control KR "freq" 440 * 1.00005
    tr0  = control KR "gate" 1
    sig1 = (f2 (freq*2) + f2 (freq*3)) * 3
    f2 x = saw AR x * amp1
    amp1 = envGen KR tr0 v1 0 dur1 DoNothing $
           envCoord [(0,0),(1e-4,1),(1e-3,1),(0.2,0)] 1 1 EnvCub
    v1   = tExpRand 'I' 0.4 0.6 tr0
    dur1 = 0.5

-- | Performance with repetation.
perform_rep01 :: UGen -> IO ()
perform_rep01 sdef = withSC3 $ do
    let name   = "rep01"
        sn a f = s_new name (-1) AddToTail 1 [("amp",a),("freq",f)]
        pcs    = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] os
        os     = [36,48..96]
        degs   = [0,2,4,5,7,9,11]
        dur    = (1/4) * (60/120)
        plen   = length pcs
        rlen   = 8
        fpcs g = take rlen $ shuffle' pcs plen g
        go g0 t0 tend i0 p0
            | tend < t0 = []
            | otherwise =
                let (dt,g1) = randomR (1,1::Int) g0
                    t1      = t0 + (fromIntegral dt * dur)
                    i1      = (i0 + 1) `mod` rlen
                    (p1,g2) = foldr fr ([],g1) p0
                    (a1,g3) = randomR (0.3,0.6) g2
                    fr x (acc,h0) =
                        let (dp,h1) = randomR (0,1::Double) h0
                            (i',h2) = randomR (0,plen-1) h1
                            x'      = if dp < 0.996 then x else pcs !! i'
                        in  (x':acc,h2)
                in  bundle t0 [sn a1 (p1!!i1)] : go g3 t1 tend i1 p1
    _ <- async $ d_recv $ synthdef name sdef
    now <- time
    g0 <- lift newStdGen
    mapM_ sendOSC $ go g0 now (now+3000) 0 $ fpcs g0

-- | Play 'perform_rep01' with 'pn01'.
play_pn01 :: IO ()
play_pn01 = perform_rep01 pn01

-- | Play 'perform_rep01' with 'phasor_ex01'.
play_phasor_ex01 :: IO ()
play_phasor_ex01 = perform_rep01 phasor_ex01

-- --------------------------------------------------------------------------
--
-- * November 2012
--
-- --------------------------------------------------------------------------

{-
Quote:

When two oscillators are hard-synchronised, the pitch of the output is equal to
the pitch of the master.

When two oscillators are hard-synchronised, then if the master frequency is
lower than the slave frequency, changing the pitch of the slave changes the
timbre of the output.

-}

-- | Example for 'phasor', take 1.
--
-- Using audio rate 'impulse' for triggering reset of slave phasor.
--
phasor_ex01 :: UGen
phasor_ex01 = centeredOut (f0 sig0 * amp0)
  where
    f0 x = rlpf x (clip (frq*2) 0 4000) 0.5
    sig0 = (phasor AR tr0 sfrq 0 1 0 - 0.5) * 0.3
    sfrq = frq*rat/sampleRate
    rat  = lfNoise2 'r' AR (pi/2) + 2
    amp0 = envGen KR tr1 1 0 dur RemoveSynth $ envPerc 1e-5 1
    dur  = 6 * recip (log frq)
    tr0  = impulse AR frq 0
    frq  = k "freq" 440
    tr1  = k "gate" 1
    k    = control KR

-- | Simple hard sync example, take 1.
--
-- Out 0 is playing synced phasor, out 1 is playing sine tone with same
-- frequency as master trigger.
--
hardsync01 :: UGen
hardsync01 = mrg [out 0 sig0, out 1 sig1]
  where
    sig0  = (phasor AR tr0 sfreq 0 1 0 - 0.5) * 0.1
    sfreq = mouseX KR mfreq (mfreq*2) Exponential 0.1 / srate
    srate = sampleRate
    tr0   = impulse AR mfreq 0
    mfreq = mouseY KR 100 200 Exponential 0.1
    sig1  = sinOsc AR mfreq 0 * 0.1

-- | Buffer number for 'hardsync02'.
hsbuf :: Num a => a
hsbuf = 99

-- | Prepare the buffer used in 'hardsync02'.
prepare_hardsync02 :: IO Message
prepare_hardsync02 = withSC3 $ async $ b_allocRead hsbuf soundFile05 0 0

-- | Simple example of hard sync, take 2.
--
-- Using sound file as wave form, with 'bufRdC', 'phasor', and other
-- buffer info ugens. Mouse X controls slave phasor frequency (or length of
-- frames in buffer), mouse Y controls frequency of master trigger used for hard
-- sync.
--
hardsync02 :: UGen
hardsync02 = mrg [out 0 sig0, out 1 sig1]
  where
    sig0  = bufRdC 1 AR hsbuf phs Loop * 0.8
    phs   = phasor AR tr0 rate pstr pend rpos
    pstr  = nfrm * (lfNoise2 'S' KR (1/5) * 0.25 + 0.25)
    pend  = nfrm * (lfNoise2 'E' KR (1/5) * 0.25 + 0.75)
    rpos  = pstr
    nfrm  = bufFrames KR hsbuf
    rate  = sfreq
    tr0   = impulse KR mfreq 0
    sig1  = sinOsc AR mfreq 0 * 0.1
    sfreq = mouseX KR 0.25 800 Exponential 0.1
    mfreq = mouseY KR 1 400 Exponential 0.1

-- | Simple example of hard sync, take 3.
--
-- Controlling ratio between slave phasor and master impulse trigger.
--
hardsync03 :: UGen
hardsync03 = centeredOut sig
  where
    sig  = (phasor AR tr0 sfrq 0 1 0 - 0.5) * 0.1
    sfrq = lag2 ((2*mfrq+mx)/sampleRate) 1.85
    mx   = mouseX KR 0 100 Linear 0.1
    tr0  = impulse AR mfrq 0
    mfrq = tExpRand 'F' 100 1000 tr1
    tr1  = impulse KR 0.5 0

-- | Simple example of hard sync, take 4.
--
-- Slave frequency jumps to /2 * Fm/, then shift to /Fm/, where /Fm/ is
-- frequency of master trigger.
--
hardsync04 :: UGen
hardsync04 = centeredOut sig
  where
    sig  = (phasor AR tr0 sfrq 0 1 0 - 0.5) * 0.1 * amp0
    sfrq = envGen KR tr1 sv 0 dur DoNothing $
           envCoord [(0,1),(1e-3,2),(1,1)] 1 1 EnvLin
    sv   = mfrq / sampleRate
    dur  = recip hps
    mfrq = midiCPS $ tChoose 'M' tr1 (mce pcs)
    pcs  = foldr (\o acc -> map (+o) degs ++ acc) [] octs
    degs = [0,2,4,5,7,9,11]
    octs = take 3 $ iterate (+12) 43
    tr0  = impulse AR mfrq 0
    amp0 = envGen KR tr1 1 0 dur DoNothing $
           envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub
    tr1  = impulse KR hps 0 +
           coinGate 'T' 0.25 (impulse KR (4*hps) 0)
    hps  = 2


-- --------------------------------------------------------------------------
--
-- * December 2012
--
-- --------------------------------------------------------------------------

{-
Quote:

... Clearly, the complexity lies in the JX10's ability to control these
components using its assignable contour generators and touch-sensitivity. And
that, dear readers, is one of the great secrets of synthesis: as is so often the
case, it's not what you've got, it's what you can do with it that matters.

-}

-- | Simple piano like sound, take 2.
pn02 :: UGen
pn02 = centeredOut sig
  where
    sig  = sig0 * amp0 * 0.3
    sig0 = rlpf sig1 cf 0.3
    -- cf   = mfrq * 1.25003
    cf   = mfrq * 2
    sig1 = (phasor AR tr0 sfrq 0 1 0 - 0.5) * 2
    sfrq = envGen AR tr1 sval 0 0.01 DoNothing $
           envCoord [(0,2),(1e-3,2),(1,1)] 1 1 (EnvNum en)
    en   = 3
    sval = mfrq / sampleRate
    tr0  = impulse AR mfrq 0
    mfrq = k "freq" 440
    amp0 = envGen KR tr1 vel 0 dur RemoveSynth $
           envCoord [(0,0),(1e-5,1),(0.2,0.7),(1,0)] 1 1 EnvCub
    vel  = k "amp" 0.5
    dur  = 3.5 * recip (log mfrq) * exp vel
    tr1  = k "gate" 1
    k    = control KR

-- | Play 'pn02' once.
play_pn02_once :: IO ()
play_pn02_once = withSC3 $ do
    _ <- async $ d_recv $ synthdef "pn02" pn02
    amp <- lift $ getStdRandom (randomR (-15,0))
    pch <- lift $ getStdRandom (randomR (5,14::Int))
    sendOSC $ s_new "pn02" (-1) AddToTail 1
        [ ("amp",dbAmp amp)
        , ("freq",midiCPS $ fromIntegral (pch * 7))
        ]

-- | Play 'pn02' with 'perform_rep01'.
play_pn02 :: IO ()
play_pn02 = perform_rep01 pn02

-- --------------------------------------------------------------------------
--
-- * Miscellaneous
--
-- --------------------------------------------------------------------------

-- | Set values of last node.
set_nlast :: [(String,Double)] -> IO ()
set_nlast = withSC3 . sendOSC . n_set (-1)

-- | Play monaural sound file with buffer 99.
playFile :: FilePath -> IO ()
playFile path = withSC3 $ do
    let bnum :: Num a => a
        bnum = 99
        nchn = 1
    _ <- async $ b_alloc bnum 65536 nchn
    _ <- async $ b_read bnum path 0 (-1) 0 True
    play $ centeredOut $ diskIn nchn bnum Loop

-- | Example from hsc3 help of 'M.dbufwr'.
dbufwr_ex01 :: IO ()
dbufwr_ex01 = do
    {s1 <- M.dseries 30 0 3
    ;s2 <- M.dseries 30 0 1
    ;s3 <- M.dseries 16 1 1
    ;s4 <- M.dwhite 8 1 16
    ;s5 <- M.dseq dinf (mce2 s3 s4)
    ;wt <- M.dust KR 1                        {- write trigger -}
    ;rp <- M.dseries dinf 0 1            {- read pointer -}
    ;wp <- M.dseq dinf (mce2 s1 s2) {- write pointer -}
    ;r <- M.dbufrd 0 rp Loop                  {- reader -}
    ;w <- M.dbufwr 0 wp (s5 * 60) Loop        {- writer -}
    ;let {d = demand wt 0 w
         ;f = lag (demand (impulse KR 16 0) 0 r) 0.01
         ;o = sinOsc AR (f * mce2 1 1.01) 0 * 0.1
         ;g = mrg [d, out 0 o]
         ;run = do {_ <- async (b_alloc_setn1 0 0 (replicate 24 210))
                   ;play g}}
     in withSC3 run}

-- | Example from hsc3 help of 'combC'.
combC_ex02 :: UGen
combC_ex02 = centeredOut sig
  where
    sig  = combC (n*0.1) 0.01 dt dcy
    dt   = lfdNoise3 'd' AR (1/2) * ((maxv-minv) * 0.5) + ((maxv+minv)*0.5)
    dcy  = lfdNoise3 'c' AR (1/3) * 0.5 + 0.5
    minv = 0.0001
    maxv = 0.01
    n    = whiteNoise 'A' AR

-- | Simple sine tone with freq, amp, dur, and attack controls.
simpleSine :: UGen
simpleSine = centeredOut sig
  where
    sig  = sinOsc AR freq phs * amp
    phs  = rand 'k' 0 pi
    freq = k "freq" 440
    amp  = envGen KR gat vel 0 dur RemoveSynth $
           envCoord [(0,0),(atk,1),(1,0)] 1 1 EnvCub
    gat  = k "gate" 1
    dur  = k "dur" 1
    vel  = k "amp" 0.5
    atk  = k "atk" 1e-1
    k    = control KR

-- | Play sine tone additive synth without fundamental frequencies and low freq
-- partials.
play_simpleSine :: IO ()
play_simpleSine = withSC3 $ do
    let name    = "simpleSine"
        sn a d k f = s_new name (-1) AddToTail 1
                     [("amp",a),("freq",f),("dur",d),("atk",k)]
        bdl t as ds ks fs = bundle t $ zipWith4 sn as ds ks fs
        go g0 t0 tend =
            let (f0,g1) = randomR  (50,100) g0
                as      = randomRs (0.005,0.05) g0
                ds      = randomRs (0.25,1.0) g1
                ks      = map exp $ randomRs (log 0.001, log 1.0) g1
                (dt,g2) = randomR (0.5, 2) g1
                t1      = t0 + dt
                fs = takeWhile (< 20000) $ dropWhile (< 1000) [f0,f0*2..]
                bs = bdl t0 as (map (*dt) ds) ks fs
            in  if tend < t0 then [] else bs : go g2 t1 tend
    _ <- async $ d_recv $ synthdef name simpleSine
    now <- time
    g0 <- lift newStdGen
    mapM_ sendOSC $ go g0 now (now+30)
