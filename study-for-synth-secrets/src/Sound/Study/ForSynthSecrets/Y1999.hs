-- |
-- Copyright    : 8c6794b6, 2014
-- License      : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scratch written while reading year 1999 synth secrets articles.
--
module Sound.Study.ForSynthSecrets.Y1999 where

import Control.Monad (foldM_)

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.UGen.Dot (draw_svg)


-- --------------------------------------------------------------------------
--
-- * May 1999
--
-- --------------------------------------------------------------------------

-- | A sine tone with amplitude envelope.
--
-- >>> audition aSineTone
--
aSineTone :: UGen
aSineTone = centeredOut sig where
  sig = sinOsc AR 440 0 * amp
  amp = envGen KR atr 0.3 0 (0.5) DoNothing ash
  atr = dust 'd' KR 2
  ash = envCoord [(0,0),(0.05,1),(1,0)] 1 1 (EnvNum (-6))

-- | Plays 'aSineTone'.
play_aSineTone :: IO ()
play_aSineTone = withSC3 reset >> audition aSineTone

-- | Sine tone with partials:
--
-- >>> audition $ partialTone 'a' 330
--
partialTone :: Char -> UGen -> UGen
partialTone iSeed frq = centeredOut sig where
  sig = amp * foldr hf 0 (zip [1..50] [iSeed..])
  amp = (lfNoise2 'a' KR 0.25 ** 2) * 0.1
  hf (part,seed) acc = acc + lsig
    where
      lsig = sinOsc AR (frq * part) lph * lamp
      lamp = lfNoise2 seed KR 5 ** 2
      lph  = rand seed 0 (2*pi)

-- | Playing partial tones.
play_partialTones :: IO ()
play_partialTones = withSC3 $ do
    let go i p = do
            play $ partialTone i p
            return $ toEnum (fromEnum i + 50)
    foldM_ go 'Σ' . take 7 $ iterate (*1.5) 55

-- | Generates saw tooth wave tone.
genSawTone ::
    UGen              -- ^ Frequency.
    -> (UGen -> UGen) -- ^ Function applied to saw oscillator.
    -> UGen
genSawTone freq fn = centeredOut sig where
  sig = fn (saw AR freq) * amp
  amp = 0.3

-- | Plays 'saw' with applying 'id'.
play_aSawTone :: IO ()
play_aSawTone = audition $ genSawTone 440 id

-- | Plays 'saw' with applying 'rlpf'.
--
-- Mouse X controls resonance Q, mouse Y controls resonance frequency.
--
play_filteredSawTone :: IO ()
play_filteredSawTone = audition sig where
  sig = genSawTone 220 $ \n ->
      let freq = mouseY KR 55 (sampleRate/2) Exponential 0.1
          rq   = mouseX KR 1e-2 1 Linear 0.1
      in  rlpf n freq rq

-- --------------------------------------------------------------------------
--
-- * June 1999
--
-- --------------------------------------------------------------------------

-- | Amplitude modulation. Using sine as carrier, and sawtooth as
-- modulator. Mouse X controls modulator frequency ratio, mouse Y controls
-- carrier frequency.
sawAM :: UGen
sawAM = centeredOut sig where
  sig       = sinOsc AR freq 0 * modulator * amp
  freq      = mouseY KR 50 2000 Exponential 0.1
  mfreq     = freq * mouseX KR 0.25 4 Linear 0.1
  modulator = (lfSaw AR mfreq 0 + 1) * 0.5
  amp       = envGen KR atrig 0.3 0 0.3 DoNothing shape
  shape     = envCoord [(0,0),(0.005,1),(1,0)] 1 1 EnvCub
  atrig     = atrig0 + atrig1 + atrig2
  atrig0    = coinGate 'g' 0.4 (impulse KR 6 0)
  atrig1    = impulse KR 1.5 0
  atrig2    = impulse KR ((lfNoise2 't' KR (1/10) ** 2) * 16) 0

-- | Plays 'sawAM'.
play_sawAM :: IO ()
play_sawAM = withSC3 reset >> audition sawAM

-- --------------------------------------------------------------------------
--
-- * July 1999
--
-- --------------------------------------------------------------------------

-- | Playing with 'envCoord'. Envelope shape contains attack, decay, sustain and
-- release.
env_ex01 :: UGen
env_ex01 = centeredOut sig
  where
    sig    = sinOsc AR freq 0 * amp
    amp    = envGen KR tr0 0.3 0 2 DoNothing shape
    tr0    = impulse KR (1/2) 0
    freq   = demand tr0 0 freqd
    freqd  = dseq 'δ' dinf (mce [440,660,330,880])
    atkt   = tRand 'α' 1e-9 1 tr0
    dcyt   = atkt + tRand 'β' 1e-9 (1-atkt) tr0
    sust   = dcyt + tRand 'γ' 1e-9 (1-dcyt) tr0
    relt   = 1
    susl   = tRand 'ε' 1e-9 1 tr0
    envN   = tRand 'ζ' (-10) 10 tr0
    shape  = envCoord levels 1 1 (EnvNum envN)
    levels = [(0,0),(atkt,1),(dcyt,susl),(sust,susl),(relt,0)]

-- | Plays 'env_ex01'.
play_env_ex01 :: IO ()
play_env_ex01 = withSC3 reset >> audition env_ex01

-- | Using low frequency oscillator for tremolo effect.
lfo_ex01 :: UGen
lfo_ex01 = centeredOut sig
  where
    sig   = sinOsc AR freq 0 * amp
    freq  = 220
    amp   = ampE * ampO
    ampE  = envGen KR tr0 0.3 0 dur DoNothing shape
    dur   = 1
    shape = envPerc 1e-4 1
    tr0   = impulse KR (recip (dur*2)) 0
    ampO  = (sinOsc AR pi 0 + 1) * 0.5

-- --------------------------------------------------------------------------
--
-- * August 1999
--
-- --------------------------------------------------------------------------

-- | Two sine waves are added. Mouse X controlls phase of one sine wave from @0@
-- to @2 * pi@. When the mouse reach closer to middle, the sound become silence.
cancelSineTones :: UGen
cancelSineTones = centeredOut sig
  where
    sig   = (sig0 + sig1) * 0.3
    sig0  = sinOsc AR freq 0
    sig1  = sinOsc AR freq phase
    freq  = 440
    phase = mouseX KR 0 (2*pi) Linear 0.1

-- | Adding 100Hz sine wave and 200Hz sine wave, mouse X controls the phase of
-- 200Hz sine wave oscillator.
sine100hzAnd200hz :: UGen
sine100hzAnd200hz = centeredOut sig
  where
    sig   = (sig0 + sig1) * 0.3
    sig0  = sinOsc AR 100 0
    sig1  = sinOsc AR 200 phase
    phase = mouseX KR 0 (2*pi) Linear 0.1

-- | Sine wave with phase 0 on out 0, phase (pi/2) on out 1.
phaseDifference :: UGen
phaseDifference = out 0 $ mce [sig0, sig1]
  where
    sig0 = sinOsc AR freq 0 * amp
    sig1 = sinOsc AR freq (pi/2) * amp
    amp  = 0.3
    freq = mouseX KR 20 2000 Exponential 0.1

-- | Cancelling two sawtooth oscillators.
--
-- >>> audition $ cancelSawTeeth 0
-- >>> audition $ cancelSawTeeth pi
--
cancelSawTeeth ::
    UGen    -- ^ Phase of second sawtooth oscillator.
    -> UGen
cancelSawTeeth phase = centeredOut sig
  where
    sig = (sig0 + sig1) * 0.3
    sig0 = lfSaw AR freq 0
    sig1 = lfSaw AR freq phase
    freq = 440

-- | Filters input with 'delayC', 'localIn', and
-- 'localOut'.
simpleDelayedIn ::
    UGen    -- ^ Input.
    -> UGen
simpleDelayedIn sig0 = mrg [o0,o1]
  where
    o0    = centeredOut (sig*0.3)
    o1    = localOut dly
    sig   = localIn 1 AR
    dly   = delayC (sig0 + (sig * scale)) 1 dtime
    scale = mouseY KR 0 (1-1e-5) Linear 0.1
    dtime = recip (mouseX KR 50 (sampleRate/2) Exponential 0.1)

-- | 'pulse' fed to 'simpleDelayedIn'.
simpleDelayedIn_pulse :: UGen
simpleDelayedIn_pulse = simpleDelayedIn (pulse AR 440 0.5 * 0.1)

-- | 'whiteNoise' fed to 'simpleDelayedIn'.
simpleDelayedIn_wn :: UGen
simpleDelayedIn_wn = simpleDelayedIn (whiteNoise 'Γ' AR * 0.1)

-- | Filtered input with 'combC'.
simpleCombed ::
    UGen    -- ^ Input.
    -> UGen
simpleCombed sig0 = centeredOut sig
  where
    sig = combC sig0 0.01 dlt dct * 0.3
    dlt = recip (mouseX KR 50 (sampleRate/2) Exponential 0.1)
    dct = mouseY KR 1e-3 3 Exponential 0.1

-- | 'pulse' fed to 'simpleCombed'.
simpleCombed_pulse :: UGen
simpleCombed_pulse = simpleCombed (pulse AR 440 0.5 * 0.1)

-- | 'whiteNoise' fed to 'simpleCombed'.
simpleCombed_wn :: UGen
simpleCombed_wn = simpleCombed (whiteNoise 'G' AR * 0.1)


-- --------------------------------------------------------------------------
--
-- * September 1999
--
-- --------------------------------------------------------------------------

-- | Applys 'lpf' to input signal. Mouse X controls cutoff frequency.
simpleLowpass ::
    UGen   -- ^ Signal fed to lowpass filter.
    -> UGen
simpleLowpass sig0 = centeredOut sig
  where
    sig = lpf sig0 cf
    cf  = mouseX KR 100 3000 Exponential 0.1

-- | Feed 100Hz 'saw' to 'simpleLowpass'.
simpleLowpass_saw :: UGen
simpleLowpass_saw = simpleLowpass (saw AR 100)

-- | Cascading 'lpf', applys to input signal.
cascadeLowpass ::
    Int     -- ^ Number of cascades.
    -> UGen -- ^ Input signal.
    -> UGen
cascadeLowpass n sig0 = centeredOut sig
  where
    sig = foldr (\_ acc -> lpf acc cf) sig0 $ replicate n ()
    cf  = mouseX KR 100 3000 Exponential 0.1

-- | Feed 100Hz 'saw' to 'cascadedLowpass', number of cascades is 16.
cascadeLowpass_saw :: UGen
cascadeLowpass_saw = cascadeLowpass 16 (saw AR 100)


-- --------------------------------------------------------------------------
--
-- * October 1999
--
-- --------------------------------------------------------------------------

-- | Applys 'resonz' to input signal.
simpleResonz ::
    UGen    -- ^ Input signal.
    -> UGen
simpleResonz sig = centeredOut sig'
  where
    sig' = resonz sig freq bwr
    freq = mouseY KR 55 (sampleRate / 2) Exponential 0.1
    bwr  = mouseX KR 1e-4 1 Linear 0.1


-- --------------------------------------------------------------------------
--
-- * November 1999
--
-- --------------------------------------------------------------------------

-- | Adds two envelopes.
addTwoEnvelopes :: UGen
addTwoEnvelopes = centeredOut sig
  where
    sig      = sinOsc AR freq 0 * amp
    amp      = ampf esh0 + ampf esh1
    ampf esh = envGen KR tr0 0.2 0 1.2 DoNothing esh
    esh0     = envCoord [(0,0),(tm0,1),(1,0)] 1 1 EnvSqr
    esh1     = envCoord [(0,0),(tm1,1),(1,0)] 1 1 EnvCub
    tm0      = tRand 's' 1e-4 1 tr0
    tm1      = tRand 'l' 1e-4 1 tr0
    tr0      = impulse KR 0.5 0
    freq     = (lfNoise2 'f' KR 0.8 ** 2) * 1000 + 200

-- | From hsc3 help file of 'envGen', originally from:
--
-- * <https://www.listarc.bham.ac.uk/lists/sc-users-2012/msg14815.html>
--
envGen_ex_from_mlist :: IO ()
envGen_ex_from_mlist =
    let n = range 0.01 0.1 (lfNoise1 'α' KR 2)
        e = Envelope [0,1] [n] [EnvLin] Nothing (Just 0)
        a = envGen AR 1 1 0 1 DoNothing (env_circle e 0 EnvLin)
        o = sinOsc AR (a * 400 + 500) 0 * 0.1
    in  audition (out 0 (pan2 o 0 1))

-- | Triggering envs with pulses and dusts.
triggeringEnvs :: UGen
triggeringEnvs = centeredOut sig
  where
    sig   = sinOsc AR freq phase * amp
    freq  = 398
    phase = sinOsc AR freq 0 * idx
    idx   = lfNoise2 'r' KR 0.3 ** 2 * 8
    amp   = envGen KR t0 0.5 0 dur DoNothing shape
    shape = envCoord [(0,0),(tm0,1),(1,0)] 1 1 (EnvNum enum)
    tm0   = lfNoise2 't' KR (1/8) ** 2
    enum  = lfNoise2 'c' KR (1/13) * 10
    dur   = (lfNoise2 'd' KR (1/7) ** 2) * 2 + 0.1
    t0    = t1 + t2 + t3
    t1    = impulse KR 1 0
    t2    = coinGate 'g' 0.15 (impulse KR 8 0)
    t3    = impulse KR ((lfNoise2 'α' KR (0.33) + 1) * 4) 0

-- | Controlling vibrato with envelopes.
changingVibs :: UGen
changingVibs = centeredOut sig
  where
    sig   = rlpf (pulse AR (freq+vib) 0.5) freq rq * amp
    dur   = 3
    tr0   = impulse KR (recip (dur*1.5)) 0

    freq  = demand tr0 0 $ drand 'δ' dinf $ mce freqs
    freqs = map midiCPS [60,63,65,67,70,72,75,77,79]
    vib   = sinOsc KR vfreq 0 * vamp
    vfreq = envGen KR tr0 5 1 dur DoNothing vfsh
    vfsh  = envCoord [(0,0),(vfl,vft),(1,0)] 1 1 EnvLin
    vfl   = tRand '1' 0.1 1 tr0
    vft   = tRand '2' 0.1 0.9 tr0
    vamp  = envGen KR tr0 10 0 dur DoNothing vsh
    vsh   = envCoord [(0,0),(tv0,1),(0.9,0.1),(1,0)] 1 1 EnvSin
    tv0   = tRand '3' 0.1 0.9 tr0

    rq    = envGen KR tr0 1 0 1 DoNothing rqsh
    rqsh  = envCoord [(0,0.01),(0.1,0.9),(1,0.8)] 1 1 EnvLin

    amp   = envGen KR tr0 0.3 0 dur DoNothing ash
    ash   = envCoord [(0,0),(tm0,1),(tm1,0.5),(1,0)] 1 1 EnvCub
    tm0   = tRand '4' 1e-4 1 tr0
    tm1   = tm0 + tRand '5' 1e-4 (1-tm0) tr0


-- --------------------------------------------------------------------------
--
-- * December 1999
--
-- --------------------------------------------------------------------------

{-
Quotes:

Increasing the complexity of the contour generators adds many possibilities for
more detailed sound creation, without precluding the creation of simpler sounds.

Don't become carried away by the current craze for vintage synths or their
DSP-generated descendants. Think about the type of sounds you want to generate,
and choose your instrument carefully so that you can produce them.

-}

-- | Using non-ADSR envelope with 'envCoord'.
nonADSREnv :: UGen
nonADSREnv = centeredOut sig0
  where
    sig0   = rlpf sig1 (freq*5) 0.8 * amp
    sig1   = saw AR freq
    freq   = 330
    tr0    = impulse KR (recip (dur*1.5)) 0
    dur    = 1
    amp    = envGen KR tr0 0.3 0 dur DoNothing ash
    ash    = envCoord coords 1 1 (EnvNum en)
    coords = [(0,0),(t1,v1),(t2,v2),(t3,v3),(t4,v4),(1,0)]
    t1     = tRand '1' 1e-4 1 tr0
    t2     = t1 + tRand '2' 1e-3 (1-t1) tr0
    t3     = t2 + tRand '3' 1e-3 (1-t2) tr0
    t4     = t3 + tRand '4' 1e-3 (1-t3) tr0
    v1     = vf '!'
    v2     = vf '@'
    v3     = vf '#'
    v4     = vf '$'
    vf x   = tRand x 1e-3 1 tr0 ** 2
    en     = lfNoise2 'e' KR 1 * 3

-- | Draw SVG file to @/tmp@.
draw_nonADSREnv :: IO ()
draw_nonADSREnv = draw_svg nonADSREnv


-- --------------------------------------------------------------------------
--
-- * Auxiliary functions
--
-- --------------------------------------------------------------------------

centeredOut :: UGen -> UGen
centeredOut sig = out 0 (pan2 sig 0 1)
