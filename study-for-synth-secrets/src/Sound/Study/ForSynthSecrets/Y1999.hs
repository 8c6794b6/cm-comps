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
    ( DoneAction(..), Envelope(..), Envelope_Curve(..), UGen, Rate(..)
    , Warp(..), audition, play, reset, withSC3
    )
import Sound.SC3.UGen.Dot (draw_svg)

import qualified Sound.SC3.UGen as UGen
import qualified Sound.SC3.UGen.ID as ID


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
  sig = UGen.sinOsc AR 440 0 * amp
  amp = UGen.envGen KR atr 0.3 0 (0.5) DoNothing ash
  atr = ID.dust 'd' KR 2
  ash = UGen.envCoord [(0,0),(0.05,1),(1,0)] 1 1 (EnvNum (-6))

-- | Plays 'aSineTone'.
play_aSineTone :: IO ()
play_aSineTone = withSC3 reset >> audition aSineTone

-- | Sine tone with partials:
--
-- >>> audition $ partialTone 330 'a'
--
partialTone :: Char -> UGen -> UGen
partialTone iSeed frq = centeredOut sig where
  sig = amp * foldr hf 0 (zip [1..50] [iSeed..])
  amp = (ID.lfNoise2 'a' KR 0.25 ** 2) * 0.1
  hf (part,seed) acc = acc + lsig
    where
      lsig = UGen.sinOsc AR (frq * part) lph * lamp
      lamp = ID.lfNoise2 seed KR 5 ** 2
      lph  = ID.rand seed 0 (2*pi)

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
  sig = fn (UGen.saw AR freq) * amp
  amp = 0.3

-- | Plays 'UGen.saw' with applying 'id'.
play_aSawTone :: IO ()
play_aSawTone = audition $ genSawTone 440 id

-- | Plays 'UGen.saw' with applying 'UGen.rlpf'.
--
-- Mouse X controls resonance Q, mouse Y controls resonance frequency.
--
play_filteredSawTone :: IO ()
play_filteredSawTone = audition sig where
  sig = genSawTone 220 $ \n ->
      let freq = UGen.mouseY KR 55 (UGen.sampleRate/2) Exponential 0.1
          rq   = UGen.mouseX KR 1e-2 1 Linear 0.1
      in  UGen.rlpf n freq rq

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
  sig       = UGen.sinOsc AR freq 0 * modulator * amp
  freq      = UGen.mouseY KR 50 2000 Exponential 0.1
  mfreq     = freq * UGen.mouseX KR 0.25 4 Linear 0.1
  modulator = (UGen.lfSaw AR mfreq 0 + 1) * 0.5
  amp       = UGen.envGen KR atrig 0.3 0 0.3 DoNothing shape
  shape     = UGen.envCoord [(0,0),(0.005,1),(1,0)] 1 1 EnvCub
  atrig     = atrig0 + atrig1 + atrig2
  atrig0    = ID.coinGate 'g' 0.4 (UGen.impulse KR 6 0)
  atrig1    = UGen.impulse KR 1.5 0
  atrig2    = UGen.impulse KR ((ID.lfNoise2 't' KR (1/10) ** 2) * 16) 0

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
    sig    = UGen.sinOsc AR freq 0 * amp
    amp    = UGen.envGen KR tr0 0.3 0 2 DoNothing shape
    tr0    = UGen.impulse KR (1/2) 0
    freq   = ID.demand tr0 0 freqd
    freqd  = ID.dseq 'δ' ID.dinf (ID.mce [440,660,330,880])
    atkt   = ID.tRand 'α' 1e-9 1 tr0
    dcyt   = atkt + ID.tRand 'β' 1e-9 (1-atkt) tr0
    sust   = dcyt + ID.tRand 'γ' 1e-9 (1-dcyt) tr0
    relt   = 1
    susl   = ID.tRand 'ε' 1e-9 1 tr0
    envN   = ID.tRand 'ζ' (-10) 10 tr0
    shape  = UGen.envCoord levels 1 1 (EnvNum envN)
    levels = [(0,0),(atkt,1),(dcyt,susl),(sust,susl),(relt,0)]

-- | Plays 'env_ex01'.
play_env_ex01 :: IO ()
play_env_ex01 = withSC3 reset >> audition env_ex01

-- | Using low frequency oscillator for tremolo effect.
lfo_ex01 :: UGen
lfo_ex01 = centeredOut sig
  where
    sig   = UGen.sinOsc AR freq 0 * amp
    freq  = 220
    amp   = ampE * ampO
    ampE  = UGen.envGen KR tr0 0.3 0 dur DoNothing shape
    dur   = 1
    shape = UGen.envPerc 1e-4 1
    tr0   = UGen.impulse KR (recip (dur*2)) 0
    ampO  = (UGen.sinOsc AR pi 0 + 1) * 0.5

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
    sig0  = UGen.sinOsc AR freq 0
    sig1  = UGen.sinOsc AR freq phase
    freq  = 440
    phase = UGen.mouseX KR 0 (2*pi) Linear 0.1

-- | Adding 100Hz sine wave and 200Hz sine wave, mouse X controls the phase of
-- 200Hz sine wave oscillator.
sine100hzAnd200hz :: UGen
sine100hzAnd200hz = centeredOut sig
  where
    sig   = (sig0 + sig1) * 0.3
    sig0  = UGen.sinOsc AR 100 0
    sig1  = UGen.sinOsc AR 200 phase
    phase = UGen.mouseX KR 0 (2*pi) Linear 0.1

-- | Sine wave with phase 0 on out 0, phase (pi/2) on out 1.
phaseDifference :: UGen
phaseDifference = UGen.out 0 $ UGen.mce [sig0, sig1]
  where
    sig0 = UGen.sinOsc AR freq 0 * amp
    sig1 = UGen.sinOsc AR freq (pi/2) * amp
    amp  = 0.3
    freq = UGen.mouseX KR 20 2000 Exponential 0.1

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
    sig0 = UGen.lfSaw AR freq 0
    sig1 = UGen.lfSaw AR freq phase
    freq = 440

-- | Filters input with 'UGen.delayC', 'UGen.localIn', and
-- 'UGen.localOut'.
simpleDelayedIn ::
    UGen    -- ^ Input.
    -> UGen
simpleDelayedIn sig0 = UGen.mrg [o0,o1]
  where
    o0    = centeredOut (sig*0.3)
    o1    = UGen.localOut dly
    sig   = UGen.localIn 1 AR
    dly   = UGen.delayC (sig0 + (sig * scale)) 1 dtime
    scale = UGen.mouseY KR 0 (1-1e-5) Linear 0.1
    dtime = recip (UGen.mouseX KR 50 (UGen.sampleRate/2) Exponential 0.1)

-- | 'UGen.pulse' fed to 'simpleDelayedIn'.
simpleDelayedIn_pulse :: UGen
simpleDelayedIn_pulse = simpleDelayedIn (UGen.pulse AR 440 0.5 * 0.1)

-- | 'ID.whiteNoise' fed to 'simpleDelayedIn'.
simpleDelayedIn_wn :: UGen
simpleDelayedIn_wn = simpleDelayedIn (ID.whiteNoise 'Γ' AR * 0.1)

-- | Filtered input with 'UGen.combC'.
simpleCombed ::
    UGen    -- ^ Input.
    -> UGen
simpleCombed sig0 = centeredOut sig
  where
    sig = UGen.combC sig0 0.01 dlt dct * 0.3
    dlt = recip (UGen.mouseX KR 50 (UGen.sampleRate/2) Exponential 0.1)
    dct = UGen.mouseY KR 1e-3 3 Exponential 0.1

-- | 'UGen.pulse' fed to 'simpleCombed'.
simpleCombed_pulse :: UGen
simpleCombed_pulse = simpleCombed (UGen.pulse AR 440 0.5 * 0.1)

-- | 'ID.whiteNoise' fed to 'simpleCombed'.
simpleCombed_wn :: UGen
simpleCombed_wn = simpleCombed (ID.whiteNoise 'G' AR * 0.1)


-- --------------------------------------------------------------------------
--
-- * September 1999
--
-- --------------------------------------------------------------------------

-- | Applys 'UGen.lpf' to input signal. Mouse X controls cutoff frequency.
simpleLowpass ::
    UGen   -- ^ Signal fed to lowpass filter.
    -> UGen
simpleLowpass sig0 = centeredOut sig
  where
    sig = UGen.lpf sig0 cf
    cf  = UGen.mouseX KR 100 3000 Exponential 0.1

-- | Feed 100Hz 'UGen.saw' to 'simpleLowpass'.
simpleLowpass_saw :: UGen
simpleLowpass_saw = simpleLowpass (UGen.saw AR 100)

-- | Cascading 'UGen.lpf', applys to input signal.
cascadeLowpass ::
    Int     -- ^ Number of cascades.
    -> UGen -- ^ Input signal.
    -> UGen
cascadeLowpass n sig0 = centeredOut sig
  where
    sig = foldr (\_ acc -> UGen.lpf acc cf) sig0 $ replicate n ()
    cf  = UGen.mouseX KR 100 3000 Exponential 0.1

-- | Feed 100Hz 'UGen.saw' to 'cascadedLowpass', number of cascades is 16.
cascadeLowpass_saw :: UGen
cascadeLowpass_saw = cascadeLowpass 16 (UGen.saw AR 100)


-- --------------------------------------------------------------------------
--
-- * October 1999
--
-- --------------------------------------------------------------------------

-- | Applys 'UGen.resonz' to input signal.
simpleResonz ::
    UGen    -- ^ Input signal.
    -> UGen
simpleResonz sig = centeredOut sig'
  where
    sig' = UGen.resonz sig freq bwr
    freq = UGen.mouseY KR 55 (UGen.sampleRate / 2) Exponential 0.1
    bwr  = UGen.mouseX KR 1e-4 1 Linear 0.1


-- --------------------------------------------------------------------------
--
-- * November 1999
--
-- --------------------------------------------------------------------------

-- | Adds two envelopes.
addTwoEnvelopes :: UGen
addTwoEnvelopes = centeredOut sig
  where
    sig      = UGen.sinOsc AR freq 0 * amp
    amp      = ampf esh0 + ampf esh1
    ampf esh = UGen.envGen KR tr0 0.2 0 1.2 DoNothing esh
    esh0     = UGen.envCoord [(0,0),(tm0,1),(1,0)] 1 1 EnvSqr
    esh1     = UGen.envCoord [(0,0),(tm1,1),(1,0)] 1 1 EnvCub
    tm0      = ID.tRand 's' 1e-4 1 tr0
    tm1      = ID.tRand 'l' 1e-4 1 tr0
    tr0      = UGen.impulse KR 0.5 0
    freq     = (ID.lfNoise2 'f' KR 0.8 ** 2) * 1000 + 200

-- | From hsc3 help file of 'UGen.envGen', originally from:
--
-- * <https://www.listarc.bham.ac.uk/lists/sc-users-2012/msg14815.html>
--
envGen_ex_from_mlist :: IO ()
envGen_ex_from_mlist =
    let n = UGen.range 0.01 0.1 (ID.lfNoise1 'α' KR 2)
        e = Envelope [0,1] [n] [EnvLin] Nothing (Just 0)
        a = UGen.envGen AR 1 1 0 1 DoNothing (UGen.env_circle e 0 EnvLin)
        o = UGen.sinOsc AR (a * 400 + 500) 0 * 0.1
    in  audition (UGen.out 0 (UGen.pan2 o 0 1))

-- | Triggering envs with pulses and dusts.
triggeringEnvs :: UGen
triggeringEnvs = centeredOut sig
  where
    sig   = UGen.sinOsc AR freq phase * amp
    freq  = 398
    phase = UGen.sinOsc AR freq 0 * idx
    idx   = ID.lfNoise2 'r' KR 0.3 ** 2 * 8
    amp   = UGen.envGen KR t0 0.5 0 dur DoNothing shape
    shape = UGen.envCoord [(0,0),(tm0,1),(1,0)] 1 1 (EnvNum enum)
    tm0   = ID.lfNoise2 't' KR (1/8) ** 2
    enum  = ID.lfNoise2 'c' KR (1/13) * 10
    dur   = (ID.lfNoise2 'd' KR (1/7) ** 2) * 2 + 0.1
    t0    = t1 + t2 + t3
    t1    = UGen.impulse KR 1 0
    t2    = ID.coinGate 'g' 0.15 (UGen.impulse KR 8 0)
    t3    = ID.impulse KR ((ID.lfNoise2 'α' KR (0.33) + 1) * 4) 0

-- | Controlling vibrato with envelopes.
changingVibs :: UGen
changingVibs = centeredOut sig
  where
    sig   = UGen.rlpf (UGen.pulse AR (freq+vib) 0.5) freq rq * amp
    dur   = 3
    tr0   = UGen.impulse KR (recip (dur*1.5)) 0

    freq  = ID.demand tr0 0 $ ID.drand 'δ' ID.dinf $ UGen.mce freqs
    freqs = map UGen.midiCPS [60,63,65,67,70,72,75,77,79]
    vib   = UGen.sinOsc KR vfreq 0 * vamp
    vfreq = UGen.envGen KR tr0 5 1 dur DoNothing vfsh
    vfsh  = UGen.envCoord [(0,0),(vfl,vft),(1,0)] 1 1 EnvLin
    vfl   = ID.tRand '1' 0.1 1 tr0
    vft   = ID.tRand '2' 0.1 0.9 tr0
    vamp  = UGen.envGen KR tr0 10 0 dur DoNothing vsh
    vsh   = UGen.envCoord [(0,0),(tv0,1),(0.9,0.1),(1,0)] 1 1 EnvSin
    tv0   = ID.tRand '3' 0.1 0.9 tr0

    rq    = UGen.envGen KR tr0 1 0 1 DoNothing rqsh
    rqsh  = UGen.envCoord [(0,0.01),(0.1,0.9),(1,0.8)] 1 1 EnvLin

    amp   = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash   = UGen.envCoord [(0,0),(tm0,1),(tm1,0.5),(1,0)] 1 1 EnvCub
    tm0   = ID.tRand '4' 1e-4 1 tr0
    tm1   = tm0 + ID.tRand '5' 1e-4 (1-tm0) tr0


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

-- | Using non-ADSR envelope with 'UGen.envCoord'.
nonADSREnv :: UGen
nonADSREnv = centeredOut sig0
  where
    sig0   = UGen.rlpf sig1 (freq*5) 0.8 * amp
    sig1   = UGen.saw AR freq
    freq   = 330
    tr0    = UGen.impulse KR (recip (dur*1.5)) 0
    dur    = 1
    amp    = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash    = UGen.envCoord coords 1 1 (EnvNum en)
    coords = [(0,0),(t1,v1),(t2,v2),(t3,v3),(t4,v4),(1,0)]
    t1     = ID.tRand '1' 1e-4 1 tr0
    t2     = t1 + ID.tRand '2' 1e-3 (1-t1) tr0
    t3     = t2 + ID.tRand '3' 1e-3 (1-t2) tr0
    t4     = t3 + ID.tRand '4' 1e-3 (1-t3) tr0
    v1     = vf '!'
    v2     = vf '@'
    v3     = vf '#'
    v4     = vf '$'
    vf x   = ID.tRand x 1e-3 1 tr0 ** 2
    en     = ID.lfNoise2 'e' KR 1 * 3

-- | Draw SVG file to @/tmp@.
draw_nonADSREnv :: IO ()
draw_nonADSREnv = draw_svg nonADSREnv


-- --------------------------------------------------------------------------
--
-- * Auxiliary functions
--
-- --------------------------------------------------------------------------

centeredOut :: UGen -> UGen
centeredOut sig = UGen.out 0 (UGen.pan2 sig 0 1)
