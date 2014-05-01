-- |
-- Copyright    : 8c6794b6, 2014
-- License      : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scratch written while reading year 2000 synth secrets articles.
--
module Sound.Study.ForSynthSecrets.Y2000 where

import Sound.OSC (DuplexOSC, Message(..), SendOSC(..), bundle, time)
import Sound.SC3.UGen.Dot (draw_svg)
import System.FilePath ((</>))

import Sound.SC3
import Sound.SC3.ID

import Sound.Study.ForSynthSecrets.Y1999 (centeredOut)

-- --------------------------------------------------------------------------
--
-- * January 2000
--
-- --------------------------------------------------------------------------

{-
Notes:

* VCA stands for Voltatge Controlled Amplifier.

Quotes:

Whenever you have more than one amplifier and/or attenuator in series, you can
calculate the Gain of the whole system simply by multiplying the individual
Gains together.

One of the most common uses for VCAs is to modify the actions of CVs using other
CVs;

We should not differentiate between amplifiers in the audio signal chain and
those used to modify control voltages.

Whenever you have more than one amplifier and/or attenuator in series, you can
at any moment calculate the Gain of the whole system simply by multiplying the
individual Gains -- at each instant -- together.

-}

-- | Using 'envGen' to control cutoff frequency of 'rlpf'.
envsAsVCAs :: UGen
envsAsVCAs = centeredOut sig0
  where
    -- output signal
    sig0   = rlpf sig1 cutoff rq * amp
    sig1   = saw AR freq
    freq   = tRand 'f' 439 441 tr0
    cfreq  = 400
    dur    = 1
    -- cutoff of rlpf for sig0
    cmul   = (lfNoise2 'm' KR 0.25 + 1) * 8
    cutoff = envGen KR tr0 (cfreq * cmul) cfreq dur DoNothing csh
    csh    = envCoord [(0,1),(ct0,0),(ct1,1),(1,0)] 1 1 EnvCub
    ct0    = tRand '0' 1e-3 1 tr0
    ct1    = ct0 + tRand '1' 1e-3 (1-ct0) tr0
    rq     = 0.8
    -- amplitude
    amp    = envGen KR tr0 0.3 0 dur DoNothing ash
    ash    = envCoord [(0,0),(t1,1),(1,0)] 1 1 (EnvNum envn)
    t1     = lfNoise1 't' KR 1 ** 2
    envn   = tRand 'n' (-10) 10 tr0
    -- trigger
    tr0    = tr1 + tr2
    tr1    = dust 'D' KR (recip (dur*2))
    tr2    = impulse KR (recip dur) 0

play_envsAsVCAs :: IO ()
play_envsAsVCAs = withSC3 reset >> audition envsAsVCAs


-- --------------------------------------------------------------------------
--
-- * Feburary 2000
--
-- --------------------------------------------------------------------------

-- | UGen from figure 1.
simpleFeb00 ::
    UGen    -- ^ Controller for cutoff frequency.
    -> UGen -- ^ Source signal.
    -> UGen
simpleFeb00 cutoff sig0 = centeredOut sig1
  where
    sig1   = rlpf sig0 cutoff rq * amp
    rq     = envGen KR tr 1 0 dur DoNothing rsh
    rsh    = envCoord [(0,0.1),(0.5,0.8),(1,0.1)] 1 1 EnvLin
    amp    = envGen KR tr 0.5 0 dur DoNothing ash
    ash    = envCoord [(0,0),(1e-3,1),(0.5,0.7),(1,0)] 1 1 EnvCub
    dur    = 1.5
    tr     = tr0 + tr1
    tr1    = impulse KR (recip (dur*1.5)) 0
    tr0    = dust 't' KR (recip (dur*1.5))

-- | Feeding 'saw' to 'simpleFeb00'.
simpleFeb00_saw :: UGen
simpleFeb00_saw = simpleFeb00 2000 (saw AR 440)

-- | Feeding 'whiteNoise' to 'simpleFeb00'.
simpleFeb00_wn :: UGen
simpleFeb00_wn = simpleFeb00 2000 (whiteNoise 'w' AR)

-- | Feeding 'saw' with vibrato to 'simpleFeb00'.
simpleFeb00_vib :: UGen
simpleFeb00_vib = simpleFeb00 2000 sig
  where
    sig    = saw AR freq
    freq   = 440 + vib
    vib    = nzvib + sinvib
    sinvib = sinOsc KR 8 0 * 2.8
    nzvib  = lfNoise2 'v' KR 1 * 2.2

-- | Feeding 'saw' with tremolo to 'simpleFeb00'.
simpleFeb00_trm :: UGen
simpleFeb00_trm = simpleFeb00 2000 sig
  where
    sig = saw AR 440 * amp
    amp = (sinOsc AR (2*pi) 0 + 1) * 0.5

-- | Feeding 'saw', and controlling cutoff frequency with sine wave.
-- Mouse Y controls the frquency of cutoff frequency modulator.
simpleFeb00_cutoff :: UGen
simpleFeb00_cutoff = simpleFeb00 csig ssig
  where
    csig = (sinOsc KR cfreq 0 + 1) * 1900 + 200
    cfreq = mouseY KR 1e-3 200 Exponential 0.1
    ssig = saw AR 440

-- | Feedinb 'pulse' with modulating the width to 'simpleFeb00'.
-- Mouse Y controls the frequency of pulse width modulator.
simpleFeb00_pulse :: UGen
simpleFeb00_pulse = simpleFeb00 2000 sig
  where
    sig   = pulse AR 440 width
    width = (sinOsc AR wfreq 0 + 1) * 0.5
    wfreq = mouseY KR 1e-3 100 Exponential 0.1


-- --------------------------------------------------------------------------
--
-- * March 2000
--
-- --------------------------------------------------------------------------

{-
Quote:

Amplitude Modulation is a powerful tool that allows you to create and play new
sounds that you cannot obtain using conventional oscillators alone.
-}

-- | Simple amplitude modulation.
simpleAM :: UGen
simpleAM = centeredOut sig0
  where
    sig0      = carrier * modulator * 0.3
    carrier   = sinOsc AR 300 0
    modulator = ((sinOsc AR 200 0 * mamp) + 1) * 0.5
    mamp      = lfNoise2 'm' KR 0.3 + 1

-- | Simple amplitude modulation, with fixed modulator frequency.
simpleInharmonicAM :: UGen
simpleInharmonicAM = centeredOut sig0
  where
    sig0      = carrier * modulator * amp
    carrier   = sinOsc AR cfreq 0
    cfreq     = tRand 'f' 100 1000 tr
    modulator = (sinOsc AR 100 0 + 1) * 0.5
    amp       = envGen KR tr 0.3 0 dur DoNothing ash
    ash       = envCoord [(0,0),(1e-2,1),(0.5,0.7),(1,0)] 1 1 EnvCub
    tr        = dust 't' KR (recip (dur*1.2))
    dur       = 1.75

-- | Simple ring modulation, with fixed modulator frequency.
simpleInharmonicRM :: UGen
simpleInharmonicRM = centeredOut sig0
  where
    sig0      = carrier * modulator * amp
    carrier   = sinOsc AR cfreq 0
    cfreq     = tRand 'f' 100 1000 tr
    modulator = (sinOsc AR 100 0) * 0.5
    amp       = envGen KR tr 0.3 0 dur DoNothing ash
    ash       = envCoord [(0,0),(1e-2,1),(0.5,0.7),(1,0)] 1 1 EnvCub
    tr        = dust 't' KR (recip (dur*1.2))
    dur       = 1.75

-- | Sine wave signal used to control cutoff frequency.
--
-- Mouse X controls index of cutoff modulator, Y controls frequency of
-- the modulator.
simpleCutoffMod :: UGen
simpleCutoffMod = centeredOut sig0
  where
    sig0      = rlpf sig1 cutoff rq * amp
    sig1      = saw AR cfreq
    cfreq     = tRand 'Ζ' 100 2000 tr0
    cutoff    = (modulator * idx) + 2000
    modulator = (sinOsc AR mfreq 0 + 1) * 0.5
    mfreq     = mouseY KR 1 3000 Exponential 0.1
    idx       = mouseX KR 800 3000 Exponential 0.1
    rq        = 0.8
    amp       = envGen KR tr0 0.3 0 dur DoNothing ash
    tr0       = dust 't' KR (recip dur)
    dur       = 1.25
    ash       = envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub


-- --------------------------------------------------------------------------
--
-- * April 2000
--
-- --------------------------------------------------------------------------

{-
Quotes:

The number of significant spectral components and their amplitudes are
determined by the Modulation Index, which is proportional to the Modulator's
amplitude; but inversely proportional to the Modulator's frequency...

For any given Carrier frequency, the position of the spectral components is
determined by the Modulator's frequency alone.

Frequency Modulation is a powerful method of synthesis that is as relevant to
analogue synthesizers as it is to digital ones, and which is capable of
generating sounds unobtainable by any other method.
-}

-- | Sine tone with slow vibrato with wide range of frequency change.
hugeVibrato :: UGen
hugeVibrato = centeredOut sig
  where
    sig       = sinOsc AR freq 0 * amp
    freq      = 1000 + (idx * modulator)
    idx       = 800
    modulator = sinOsc AR (recip 6) 0
    amp       = 0.3

-- | Simple frequency modulation.
simpleFM :: UGen
simpleFM = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = sinOsc AR freq 0
    freq      = 3300 + (mfreq * modulator * idx)
    modulator = sinOsc AR mfreq 0
    mfreq     = mouseY KR 1 4400 Exponential 0.1
    idx       = mouseX KR 0 20 Linear 0.1
    amp       = 0.3

-- | Another FM, with controlling modulator index with 'envGen'.
simpleFM2 :: UGen
simpleFM2 = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = sinOsc AR freq 0
    freq      = cfreq + (mfreq * modulator * idx)
    cfreq     = tExpRand 'f' 100 1000 tr0
    mfreq     = cfreq * ratio
    ratio     = 2.001
    modulator = sinOsc AR mfreq 0
    idx       = envGen KR tr0 10 0 dur DoNothing ish
    ish       = envCoord ilvls 1 1 EnvLin
    ilvls     = [(0,0),(it0,iv0),(it1,1),(it2,iv2),(1,0)]
    it0       = tRand 'i' 1e-4 (1-1e-4) tr0
    it1       = it0 + tRand 'j' 1e-4 (1-it0) tr0
    it2       = it1 + tRand 'k' 1e-4 (1-it1) tr0
    [iv0,iv2] = map (\c -> tExpRand c 1e-4 1 tr0) "lm"
    tr0       = impulse KR ftr0 0
    ftr0      = 0.25
    dur       = recip ftr0 * 0.8
    amp       = envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = envCoord [(0,0),(at,1),(1,0)] 1 1 (EnvNum 10)
    at        = tExpRand 'a' 1e-4 (1-1e-4) tr0


-- --------------------------------------------------------------------------
--
-- * May 2000
--
-- --------------------------------------------------------------------------

-- | FM with changing frequency ratio between carrier and modulator.
simpleFMRatio :: UGen
simpleFMRatio = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = sinOsc AR freq 0
    freq      = cfreq + (modulator * mfreq * idx)
    cfreq     = 440
    modulator = sinOsc AR mfreq 0
    mfreq     = cfreq * ratio
    ratio     = mr / cr
    cr        = 1 + pulseCount tr1 tr2
    mr        = pulseCount tr0 tr1
    tr0       = impulse KR (recip dur * 0.8) 0
    tr1       = impulse KR ((recip dur * 0.8) / 16) 0
    tr2       = impulse KR ((recip dur * 0.8) / 256) 0
    dur       = 0.20
    idx       = envGen KR tr0 10 0 dur DoNothing ish
    ish       = envCoord [(0,0),(t1,1),(1,0)] 1 1 EnvLin
    t1        = tExpRand 't' 1e-4 0.999 tr0
    amp       = envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub

-- | Another FM with changing C:M ratio.
simpleFMRatio2 :: UGen
simpleFMRatio2 = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = sinOsc AR freq 0
    freq      = cfreq + (modulator * mfreq * idx)
    cfreq     = 440
    modulator = sinOsc AR mfreq 0
    mfreq     = cfreq * ratio
    ratio     = mr / cr
    mr        = floorE (tRand 'm' 1 17 tr0)
    cr        = floorE (tRand 'c' 1 17 tr00)
    idx       = envGen KR tr0 10 0 dur DoNothing ish
    ish       = envCoord [(0,0),(1e-1,1),(1,0)] 1 1 EnvCub
    amp       = envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = envCoord [(0,1e-4),(1e-3,1),(1,1e-4)] 1 1 EnvExp
    dur       = 0.7
    tr0       = tr00 + tr1 + tr2 + tr3
    tr00      = impulse KR (recip dur) 0
    tr1       = coinGate 'g' 0.6 (impulse KR (recip dur * 2) 0)
    tr2       = coinGate 'G' 0.3 (impulse KR (recip dur * 4) 0)
    tr3       = coinGate 'γ' 0.21 (impulse KR (recip dur * 8) 0)

-- | FM with fixed ratio.
simpleFMTimber :: UGen
simpleFMTimber = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = sinOsc AR freq 0
    freq      = cfreq + (modulator * mfreq * idx)
    cfreq     = 220 * cmul
    cmul      = 1.5 ** floorE (tRand 'c' 1 3 tr0)
    modulator = sinOsc AR mfreq 0
    mfreq     = cfreq * ratio
    ratio     = 2.000002 * floorE (tRand 'r' 1 2.2 tr0)
    idx       = envGen KR tr0 10 0 dur DoNothing ish
    ish       = envCoord [(0,0),(it0,1),(1,0)] 1 1 EnvSin
    it0       = tRand 't' 0 1 tr0
    amp       = envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = envCoord [(0,0),(1e-3,1),(0.8,0.5),(1,0)] 1 1 EnvCub
    dur       = recip hps * 0.8
    tr0       = impulse KR hps 0
    hps       = 0.55

-- | Ping-pong delay example from 'localIn'
pingPongWithLocalIn :: IO ()
pingPongWithLocalIn =
    let n  = whiteNoise 'a' AR
        a0 = decay (impulse AR 0.3 0) 0.1 * n * 0.2
        a1 = localIn 2 AR + mce [a0,0]
        a2 = delayN a1 0.2 0.2
        a3 = mceEdit reverse a2 * 0.8
    in  audition (mrg [localOut a3, out 0 a2])

-- | Simple feedback FM.
simpleFeedbackFM :: UGen
simpleFeedbackFM = mrg [o0, o1]
  where
    o0        = centeredOut (sig0 * amp)
    sig0      = sinOsc AR freq modulator
    freq      = mouseY KR 110 880 Exponential 0.1
    modulator = localIn 1 AR * idx
    idx       = mouseX KR 0 3 Linear 0.1
    amp       = envGen KR tr 0.3 0 1 DoNothing ash
    ash       = envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub
    tr        = impulse KR 1 0
    o1        = localOut sig0


-- --------------------------------------------------------------------------
--
-- * June 2000
--
-- --------------------------------------------------------------------------

{-
Quote:

Organs sound like organs not because of the simplicity (or not) of their waveform gene
rators, but because their sounds do not change over time.

No matter how clever the method of synthesis, and no matter how complex an
initial waveform may be, any timbre will sound static and 'organ-like' if it
does not change in time.
-}

-- | Manually summed up sine waves to 9th partial.
manualSum9th :: UGen
manualSum9th = centeredOut sig0
  where
    sig0    = sum oscs * amp
    oscs    = map mkosc [1..9]
    mkosc x = sinOsc AR (x*freq) 0 * (1/x)
    freq    = sum9thFreqs tr0
    amp     = envGen KR tr0 0.3 0 1 DoNothing ash
    ash     = envCoord [(0,0),(1e-2,1),(1-1e-2,1),(1,0)] 1 1 EnvCub
    tr0     = impulse KR 8 0

-- | Saw tooth oscillator, to compare with 'manualSum9th'.
sawForSum9th :: UGen
sawForSum9th = centeredOut sig0
  where
    sig0  = saw AR freq * amp
    freq  = sum9thFreqs tr0
    amp   = envGen KR tr0 0.3 0 1 DoNothing ash
    ash   = envCoord [(0,0),(1e-3,1),(0.7,0.2),(1,0)] 1 1 EnvCub
    tr0   = impulse KR 8 0

-- | Manually summed up sine waves to 9th partial, take 2.
-- Each partial has individual amplitude, higher frequency decaying
-- sooner.
manualSum9th2 :: UGen
manualSum9th2 = centeredOut sig0
  where
    sig0    = sum oscs * 0.1
    oscs    = map mkosc [1..9]
    mkosc x = sinOsc AR (x*freq) 0 * mkamp x
    mkamp x = envGen KR tr0 (3/x) 0 (1/x) DoNothing ash
    ash     = envCoord lvls 1 1 EnvCub
    lvls    = [(0,0),(1e-3,1),(1,0)]
    freq    = sum9thFreqs tr0
    tr0     = impulse KR hps 0
    hps     = 8

-- | Manually summed up sine waves to 9th partial, take 3.
-- Each partial has individual amplitude, controlled with 'lfNoise2'.
manualSum9th3 :: UGen
manualSum9th3 = centeredOut sig0
  where
    sig0    = sum oscs * 0.1
    nosc :: Num a => a
    nosc    = 9
    oscs    = map mkosc [1..nosc]
    mkosc x = sinOsc AR (x*freq) 0 * mkamp x
    mkamp x = envGen KR tr0 1 0 (recip hps * 0.8) DoNothing (ash x)
    ash x   = envCoord [(0,0),(at x,1),(1,0)] 1 1 EnvCub
    at x    = select x nzs
    nzs     = mce $ map (\c -> (lfNoise2 c KR 0.2 + 1)*0.5) [(1::Int)..nosc]
    freq    = sum9thFreqs tr0
    tr0     = impulse KR hps 0
    hps     = 8

-- | Draws svg graph of 'manualSum9th3'.
draw_manualSum9th3 :: IO ()
draw_manualSum9th3 = draw_svg manualSum9th3

-- | Random pentatonic frequencies.
sum9thFreqs ::
    UGen    -- ^ Trigger to get new random frequency value.
    -> UGen
sum9thFreqs tr0 = sig
  where
    sig     = tChoose 'p' tr0 $ mce $ map midiCPS ptchs
    ptchs   = zipWith (+) (cycle degs)
              (concatMap (replicate $ length degs) [36,48,60,72])
    degs    = [0,3,5,7,10]


-- --------------------------------------------------------------------------
--
-- * July 2000
--
-- --------------------------------------------------------------------------

{-
Quote:

If your synthesizer has an external signal input plus an envelope follower and a
pitch/CV converter, it is a much more powerful and flexible instrument than it
would otherwise be. You should experiment with them!
-}

wavFileDir :: FilePath
wavFileDir = "/home/atsuro/sound/wav"

-- | Extracted audio from introduction to functional programming lecture.
soundFile01 :: FilePath
soundFile01 = wavFileDir </> "fp_10_to_40.wav"

-- | Another sound file containing human speech.
soundFile02 :: FilePath
soundFile02 = wavFileDir </> "ted_intro_mono.wav"

-- | Sound file containing pop song.
soundFile03 :: FilePath
soundFile03 = wavFileDir </> "rakista_mono.wav"

-- | Sound file containing short male human voice.
soundFile04 :: FilePath
soundFile04 = wavFileDir </> "voice.wav"

-- | Sound file, shorter variant of 'soundFile01'.
soundFile05 :: FilePath
soundFile05 = wavFileDir </> "lecture_is_about.wav"

-- | From hsc3 help file of 'diskIn'.
diskIn_example ::
    FilePath -- ^ Input sound file.
    -> IO ()
diskIn_example filepath = withSC3 $ do
    let n   = 1
        d   = diskIn n 0 Loop
        o0  = out 0 (d * 0.3)
        g   = o0
    _ <- async $ b_alloc 0 65536 n
    _ <- async $ b_read 0 filepath 0 (-1) 0 True
    play g

-- | Playing input file and sine oscillator with analyzed frequency and
-- amplitude.
followingSine ::
    FilePath -- ^ Input sound file to extract pitch and amplitude envelope.
    -> IO ()
followingSine file = withSC3 $ do
    let n      = 1
        input  = diskIn n 0 Loop
        ifilt  = hpf (lpf input 3000) 100
        ptc    = pitch ifilt 440 60 4000 100 16 1 0.05 0.5 1
        freq   = lag2 ptc 5e-4
        acurve = amplitude KR ifilt 0.1 0.1
        amp    = lag2 acurve 5e-4
        sig    = sinOsc AR freq 0 * amp
        o0     = out 0 (input * 0.3)
        o1     = out 1 sig
        outs   = mce [o0, o1]
    _ <- async $ b_alloc 0 65536 n
    _ <- async $ b_read 0 file 0 (-1) 0 True
    play outs

-- | Frequency used in 'simpleVocoder01'.
vocFreqs :: UGen -> UGen
vocFreqs tr0 = tChoose 'v' tr0 $ mce $ map midiCPS ptcs
  where
    ptcs = zipWith (+) (cycle degs) (concatMap (replicate ld) octs)
    degs = [0,3,7,10]
    octs = [36,48]
    ld   = length degs

-- | Simple vocoder.
--
-- Using 36 banks to detect amplitude envelope of input sound file. Mouse X
-- controls lag time of oscillator pitches.
--
-- >>> simpleVocoder01 soundFile01
--
simpleVocoder01 ::
    FilePath -- ^ Input sound file.
    -> IO ()
simpleVocoder01 file = withSC3 $ do
    let n      = 1
        bnum   = 0
        input  = diskIn n 0 Loop
        mkbank f = rlpf (pulse AR f 0.5) (aFreq*36) 0.3 * amp1
          where
            bank  = resonz input f 0.05 * 3
            amp0  = amplitude KR bank 0.1 0.1
            amp1  = lag2 amp0 lagt * 0.8
        freqs  = map (\x -> aFreq * x) [1..36]
        aFreq  = lag2 (vocFreqs (impulse KR hps 0)) (recip hps * 0.1)
        hps    = 4
        lagt   = mouseX KR 1e-4 4 Exponential 0.1
        comp x = compander x cnt thrsh slBelow slAbove clTime relTime
          where
            cnt     = x
            thrsh  = 0.8
            slBelow = 1
            slAbove = 1
            clTime  = 0.01
            relTime = 0.1
        outs   = centeredOut . comp . sum $ map mkbank freqs
    _ <- async $ b_alloc bnum 65536 n
    _ <- async $ b_read bnum file 0 (-1) 0 True
    play outs

-- | Another simple vocoder.
--
-- Using 36 banks to detect pitch and amplitude envelope of input sound
-- file. Mouse X controls lag time of oscillator pitches, mouse Y controls
-- frequency factor for oscillators.
--
-- >>> simpleVocoder02 soundFile02
simpleVocoder02 ::
    FilePath -- ^ Input sound file.
    -> IO ()
simpleVocoder02 file = withSC3 $ do
    let n     = 1
        bnum  = 0
        input = diskIn n 0 Loop
        mkbank f = sinOsc AR ptc2 0 * amp1
          where
            bank = resonz input f 0.05 * 3
            ptc0 = pitch bank f (f*0.5) (f*2) 100 16 1 0.01 0.5 1
            ptc1 = lag3 ptc0 (lagt / logBase 1.22 (f/20))
            ptc2 = mceChannel 0 ptc1 * prat
            prat = mouseY KR 0.5 2 Exponential 0.1
            amp0 = amplitude KR bank 0.1 0.1
            amp1 = lag2 amp0 lagt * 0.8
        freqs = map (\x -> 20 * (1.22 ** x)) [0..35]
        lagt  = mouseX KR 1e-4 4 Exponential 0.1
        comp x = compander x cnt thrsh slBelow slAbove clTime relTime
          where
            cnt     = x
            thrsh  = 0.8
            slBelow = 1
            slAbove = 1
            clTime  = 0.01
            relTime = 0.1
        outs  = centeredOut . comp . sum $ map mkbank freqs
    _ <- async $ b_alloc bnum 65536 n
    _ <- async $ b_read bnum file 0 (-1) 0 True
    play outs


-- --------------------------------------------------------------------------
--
-- * August 2000
--
-- --------------------------------------------------------------------------

-- | Simple sample and hold using 'latch'.
--
-- From SC help file:
--
-- /... LFNoise0 is a faster way to generate random steps ... /
--
sampleAndHold01 :: UGen
sampleAndHold01 = centeredOut sig
  where
    sig  = resonz (saw AR 220) freq rq * amp
    freq = latch nz0 tr0
    nz0  = (whiteNoise 'n' KR + 1) * 1200 + 200
    tr0  = impulse KR hps 0
    hps  = 0.5
    rq   = envGen KR tr0 1 0 (recip hps * 0.75) DoNothing rsh
    rsh  = envCoord [(0,0.1),(0.5,1),(1,0.1)] 1 1 EnvCub
    amp  = envGen KR tr0 1 0 (recip hps * 0.75) DoNothing ash
    ash  = envCoord [(0,0),(1e-2,1),(0.7,0.3),(1,0)] 1 1 EnvCub

-- | Simple track and hold using 'gate' and 'lfNoise2'.
trackAndHold01 :: UGen
trackAndHold01 = centeredOut sig
  where
    sig   = sinOsc AR freq 0 * amp
    freq  = (gate hold tr0) * 2200 + 150
    hold  = lfNoise2 'e' KR (hfreq * 4)
    hfreq = 1.25
    tr0   = pulse KR hfreq 0.75
    amp   = envGen KR tr0 0.3 0 (recip hfreq * 0.8) DoNothing ash
    ash   = envCoord [(0,0),(1e-2,1),(0.8,0.5),(1,0)] 1 1 EnvCub

-- | Simple sequence with demand ugens: 'demand', 'dseq', and 'dinf'.
simpleSequence01 :: UGen
simpleSequence01 = centeredOut sig
  where
    sig   = sinOsc AR freq phase * amp
    phase = sinOsc AR (freq*1.0051) 0 * idx
    idx   = (fSinOsc AR 0.125 0 * lfNoise2 'i' KR 0.25 + 1) * 10
    freq  = lag2 (demand tr0 0 freqs) 0.1
    freqs = dseq 'a' dinf ptchs
    ptchs = mce $ map midiCPS ps
    ps    = [60,60,55,58]
    tr0   = impulse KR hps 0
    hps   = 4
    amp   = envGen KR tr0 0.3 0 (recip hps * 0.8) DoNothing ash
    ash   = envCoord [(0,0),(2e-2,1),(0.8,0.3),(1,0)] 1 1 EnvSin

-- | Simple sequence with 'select' and 'pulseCount'.
simpleSequence02 :: UGen
simpleSequence02 = centeredOut sig
  where
    sig   = sinOsc AR freq phase * amp
    freq  = select (pulseCount tr0 tr1) (mce ptcs)
    ptcs  = map midiCPS $ zipWith (+) octs (cycle degs)
    octs  = concatMap (replicate (length degs)) [60,63,58,55]
    degs  = [-12,-7,-5,0,5,7,12]
    phase = sinOsc AR (freq * 1.5001) 0 * idx
    idx   = (fSinOsc KR (recip (hps * 4)) 0 + 1) * 10
    amp   = envGen KR tr0 0.3 0 (recip hps *  0.8) DoNothing ash
    ash   = envCoord [(0,0),(1e-3,1),(0.1,0.5),(0.8,0.5),(1,0)] 1 1 EnvCub
    tr0   = impulse KR hps 0
    tr1   = impulse KR (hps / fromIntegral (length ptcs)) 0
    hps   = 8

-- | Another sequence with 'select', 'pulseCount', 'tChoose' and
-- 'coinGate'.
simpleSequence03 :: UGen
simpleSequence03 = centeredOut sig
  where
    sig    = sinOsc AR freq phase * amp
    freq   = seq03Freqs hps keys
    keys   = [0,2,3,5,7,9,10,12,14,15,17,19,21,22]
    hps    = 8
    tr0    = impulse KR hps 0
    phase  = sinOsc AR (freq * 1.50092) 0 * idx
    idx    = (sinOsc KR (recip hps * 0.25) 0 + 1) * 5
    amp    = envGen KR tr0 0.3 0 (recip hps * 0.95) DoNothing ash
    ash    = envCoord [(0,0),(1e-3,1),(2e-2,0.8),(1,0)] 1 1 EnvCub

-- | Frequency sequence pattern used in 'simpleSequence03'.
seq03Freqs ::
    UGen      -- ^ Hits per seconds.
    -> [UGen] -- ^ Degree values.
    -> UGen
seq03Freqs hps keys = freq
  where
    freq   = select (pulseCount tr0 tr1) (mce ptcs)
    tr0    = impulse KR hps 0
    tr1    = impulse KR (hps / fromIntegral (length ptcs)) 0
    ptcs   = map midiCPS $ zipWith (+) octs (cycle degs)
    octs   = concatMap (replicate (length degs)) [60]
    degs   = take 8 $ map degf ['a'..]
    degf c = tChoose c (coinGate c (1/29) tr1) (mce keys)


-- --------------------------------------------------------------------------
--
-- * September 2000
--
-- --------------------------------------------------------------------------

{-
Quotes:

If a signal has a bandwidth of less than F (the Nyquist frequency), then 2F
samples per second are sufficient to represent that signal fully and
unambiguously.

An analogue signal can be reconstructed, without error, from samples taken at
equal time intervals, provided that the sampling rate is greater than twice the
highest-frequency component in the original signal.

-}

-- | From hsc3 help file of 'shaper'. This action is used by other
-- examples.
shaper_ex01 :: DuplexOSC m => [Double] -> m Message
shaper_ex01 a = do
    _ <- async (b_alloc 10 512 1)
    let f = [Normalise, Wavetable, Clear]
    async $ b_gen_cheby 10 f a

-- | From hsc3 help file of 'shaper'.
shaper_ex02 :: IO ()
shaper_ex02 = withSC3 $ do
    let s = sinOsc AR 300 0 * line KR 0 1 6 RemoveSynth
    _ <- shaper_ex01 [1,0,1,1,0,1]
    play (out 0 (shaper 10 s * 0.1))

-- | Another example from hsc3 help file of 'shaper'.
shaper_ex03 :: IO ()
shaper_ex03 = withSC3 $ do
    let s = sinOsc AR 400 (pi/2) * line KR 0 1 6 RemoveSynth
    _ <- shaper_ex01 [0.25,0.5,0.25]
    play $ out 0 (shaper 10 s * 0.1)

-- | Simple wave table oscillator with 'osc'.
-- Buffer values are cauculated with formula shown in /Advanced notes: wavetable
-- format/ from SC3 help file of Shaper ugen.
simpleWaveTable01 :: IO ()
simpleWaveTable01 = withSC3 $ do
    let bnum  :: Num a => a
        bnum  = 11
        bsize :: Num a => a
        bsize = 512
        vals  = take bsize $ map (\x -> cos (x*pi)) $ cycle [-1,-1+(2/bsize)..1]
        vals' = f vals
        f (a0:a1:as) = 2*a0-a1 : a1-a0 : f as
        f _          = []
        sig   = osc AR bnum 220 0 * 0.2
    _ <- async $ b_alloc bnum bsize 1
    send $ b_set bnum $ zip [0..] vals'
    play $ centeredOut sig


-- --------------------------------------------------------------------------
--
-- * October 2000
--
-- --------------------------------------------------------------------------

{-
Quotes:

If we're going to get the best from our synthesizers, we need to understand how
their sounds react when played -- whatever means we use to play them.
-}

-- | Triggering amplitude and frequency change with different rate.
simpleTriggers01 :: UGen
simpleTriggers01 = centeredOut sig
  where
    sig   = sinOsc AR freq phase * amp
    freq  = lag3 (tChoose 'f' tr0 ptchs) 0.0125
    ptchs = mce $ map midiCPS [36,58,60,62,63,67]
    phase = mix (sinOsc AR (mce [freq*1.4998,freq*4.003]) 0) * idx
    idx   = envGen KR tr1 5 0 (recip cps1 * 0.85) DoNothing ash
    amp   = envGen KR tr1 0.3 0 (recip cps1 * 0.98) DoNothing ash
    ash   = envCoord lvls 1 1 EnvCub
    lvls  = [(0,0),(0.1,1),(0.2,0.8),(0.8,0.8),(1,0.3)]
    tr0   = coinGate 'a' 0.7 (impulse KR cps0 0)
    tr1   = coinGate 'd' 0.8 (impulse KR cps1 0)
    cps0  = 1.5
    cps1  = 0.8


-- --------------------------------------------------------------------------
--
-- * November 2000
--
-- --------------------------------------------------------------------------

{-
Quotes:

Duophonic synthesizers are far less than polyphonic synthesizers restricted to
playing two notes at a time.

Duophonic synthesizers are far less than polyphonic synthesizers restricted to
playing two notes at a time, but duo-timbral synthesizers are far more than
monophonic synthesizers that can play two notes at a time.

-}

-- | Playing multiple notes in server side with 'pulseDivider'.
simplePoly01 ::
    UGen    -- ^ Signal to trigger.
    -> UGen
simplePoly01 tr0 = out 0 sig
  where
    numv :: Num a => a
    numv = 5
    hps  = 1.15
    sig  = sum $ map go [(1::Int)..numv]
    ps   = zipWith (+)
           (concatMap (replicate 5) [32,44,56,68,80])
           (cycle [0,3,5,7,10])
    rsh  = envCoord [(0,0.9),(0.5,0.1),(1,0.7)] 1 1 EnvLin
    ash  = envCoord lvls 1 1 EnvCub
    lvls = [(0,0),(1e-3,1),(0.1,0.3),(0.7,0.7),(1,0)]
    prob = (lfNoise2 'p' KR 0.25 + 1) * 0.275 + 0.2
    ampm = ((sinOsc KR
             ((lfNoise2 'q' KR (1/23) + 1) * 9) 0 + adep) *
            (1/(adep+1)))
    adep = ((sinOsc KR (1/33) 0 + 1) * 0.5) * 6
    go n = psig
      where
        psig = pan2 lsig (-0.4 + 0.16 * fromIntegral n) 1
        lsig = rlpf (saw AR freq) cf rq * amp
        freq = midiCPS $ tChoose n ltr (mce ps)
        cf   = envGen KR ltr (freq*8) 0 (recip hps*3.8) DoNothing rsh
        rq   = envGen KR ltr 1 0 (recip hps*3.8) DoNothing rsh
        amp  = envGen KR ltr 0.2 0 (recip hps*4.2) DoNothing ash * ampm
        ltr  = coinGate n prob $
               pulseDivider tr0 numv (fromIntegral $ n-1)

-- | Simple pattern with noise and oscillator
simplePoly02 ::
    UGen    -- ^ Signal to trigger.
    -> UGen
simplePoly02 tr0 =
    mrg $ map ($ tr0)
    [simplePoly02_sig0, simplePoly02_sig1, simplePoly02_sig2]

-- | Sig0 in 'simplePoly02'.
simplePoly02_sig0 :: UGen -> UGen
simplePoly02_sig0 tr0 = out 0 (pan2 sig0 (-0.08) 1)
  where
    sig0   = sinOsc AR freq0 phase0 * amp0
    amp0   = envGen KR tr00 amp00 0 dur0 DoNothing ash0
    amp00  = tExpRand '鯆' 0.24 0.35 tr00
    ash0   = envCoord alvls  1 1 EnvCub
    alvls  = [(0,0),(1e-4,1),(1e-2,0.9),(0.5,0.5),(1,0)]
    dur0   = (0.125 * (lfNoise2 '鱈' KR (1/14.53) + 1)) + 0.1
    freq0  = envGen KR tr00 famp0 0 dur0 DoNothing fsh0 * ofreq
    fsh0   = envCoord [(0,1),(0.2,0.5),(0.45,0.9),(1,0)] 1 1 EnvCub
    ofreq  = envGen KR tr00 0.5 0 1 DoNothing ofsh0 *
             sinOsc AR 20 0
    ofsh0  = envCoord [(0,1),(1e-4,1),(1,0)] 1 1 EnvSin
    famp0  = tExpRand 'γ' 40 80 (coinGate 'c' (1/17) tr0)
    phase0 = sinOsc AR (freq0 * 0.72001) 0 * idx0
    idx0   = envGen KR tr00 18 0 dur0 DoNothing ish0 *
             (lfNoise2 'β' AR (1/37) + 1) * 2
    ish0   = envCoord [(0,0),(1e-5,1),(1e-2,0.3),(1,0)] 1 1 EnvCos
    tr00   = coinGate '鰯' 0.63 tr0

-- | Sig1 in 'simplePoly02'.
simplePoly02_sig1 :: UGen -> UGen
simplePoly02_sig1 tr0 = out 0 (pan2 sig 0.08 1)
  where
    sig   = sig0 + sig1
    sig0  = rlpf (mix $ sinOsc AR freq0 phs0) 8000 0.8 * amp0
    freq0 = mce [585.78, 113.23, 332.10]
    amp0  = envGen KR tr1 ampr0 0 dur0 DoNothing ash0 * aosc0
    ash0  = envCoord [(0,1e-3),(1e-4,1),(1e-3,0.9),(1,1e-3)] 1 1 EnvExp
    ampr0 = tExpRand 'c' 0.2 0.35 tr1
    dur0  = 0.3
    aosc0 = (sinOsc KR aoscf 0 + 1) * 0.3 + 0.1
    aoscf = envGen KR tr1 100 0 dur0 DoNothing aosh
    aosh  = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvLin
    phs0  = (sinOsc KR 158 0 + 1) * 10
    sig1  = resonz (rhpf wnz freq1 rq) famp1 0.99 * amp1
    wnz   = whiteNoise 'a' AR
    freq1 = envGen KR tr1 famp1 0 dur1 DoNothing fsh1
    famp1 = tExpRand 'd' 6000 12000 (coinGate 'e' (1/23) tr0)
    fsh1  = envCoord [(0,1),(0.25,0.5),(1,0.01)] 1 1 EnvExp
    rq    = 0.9
    amp1  = envGen KR tr1 0.2 0 dur1 DoNothing ash1
    ash1  = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub
    dur1  = 0.3
    tr1   = pulseDivider tr0 4 1

-- | Sig2 in 'simplePoly02'.
simplePoly02_sig2 :: UGen -> UGen
simplePoly02_sig2 tr0 = out 0 (pan2 sig2 pos 1)
  where
    sig2  = rlpf (wnz + pls) freq rq2 * amp2
    wnz   = whiteNoise '鮪' AR
    pls   = pulse AR (freq/16) ((sinOsc AR 1.111 0 + 1) * 0.5)
    freq  = envGen KR tr0 1 0 dur2 DoNothing fsh
    fsh   = envCoord [(0,fv0),(ft1,fv1),(1,fv2)] 1 1 EnvSqr
    fv0   = tExpRand '鯵' 6000 13000 (coinGate '鰍' (1/7) tr0)
    ft1   = tExpRand '鯨' 0 1 (coinGate '鱚' (1/11) tr0)
    fv1   = tExpRand '鰹' 100 8000 (coinGate '鮎' (1/13) tr0)
    fv2   = tExpRand '鰤' 300 9000 (coinGate '鯏' (1/11) tr0)
    rq2   = (((lfNoise2 '鮗' KR 3) + 1) * 0.4) + 0.1
    amp2  = envGen KR tr01 0.1 0 dur2 DoNothing ash2
    ash2  = envCoord [(0,0),(atk2,1),(1,0)] 1 1 (EnvNum en2)
    atk2  = (lfNoise0 '鯖' KR (1/3) + 1) * 0.5
    en2   = lfNoise0 '鮭' KR (1/13) * 10
    dur2  = (lfNoise2 '鯱' KR (1/17) + 1) * 0.3
    tr01  = coinGate '鰻' 0.38 tr0
    pos   = lfNoise2 '鰌' KR (1/3) * 0.4

-- | Plays 'simplePoly01' and 'simplePoly02' with same impulse to trigger.
simplePolys :: UGen
simplePolys = mrg $ [simplePoly01 tr0, simplePoly02 tr1]
  where
    tr0  = impulse KR 3.75 0 + coinGate 'g' 0.25 tr2
    tr1  = tr0 + tr2
    tr2  = dust 'a' KR dhps
    dhps = ((lfNoise2 'b' KR (1/19) + 1) ** 4) * 0.3

-- | Play 'simplePolys'.
play_simplePolys :: IO ()
play_simplePolys = audition simplePolys

-- --------------------------------------------------------------------------
--
-- * December 2000
--
-- --------------------------------------------------------------------------

{-
Quotes:

Just like human beings, sounds are born, reach their prime, and
die. Furthermore, if you have more than one person in a room, each must be born,
reach his/her prime, and die independently of all the others.
-}

-- | To play with attack contour.
simpleContour01 :: UGen
simpleContour01 = centeredOut sig
  where
    sig  = sinOsc AR freq 0 * amp
    freq = tChoose '樹' tr0 (mce ptcs)
    tr0  = impulse KR cps 0
    cps  = 2
    ptcs = map midiCPS [60,65,67,70]
    amp  = envGen KR tr0 0.3 0 (recip cps * 0.8) DoNothing ash
    ash  = envCoord [(0,0),(atk,1),(1,0)] 1 1 (EnvNum en)
    atk  = lfNoise2 '植' KR (1/9) ** 2
    en   = lfNoise2 '標' KR (1/11) * 10

-- | UGen with freq, amp, and dur controls.
fad01 :: UGen
fad01 = centeredOut sig
  where
    sig   = sinOsc AR freq phase * ampe
    freq  = control KR "freq" 440
    amp   = control KR "amp" 0.3
    dur   = control KR "dur" 1
    phase = sinOsc AR (freq*1.498) 0 * idx
    idx   = envGen KR 1 10 0 dur DoNothing ish
    ish   = envCoord [(0,1),(1,0)] 1 1 EnvLin
    ampe  = envGen KR 1 amp 0 dur RemoveSynth ash
    ash   = envCoord
            [(0,0),(1e-3,1),(1e-1,0.6),(0.9,0.6),(1,0)]
            1 1 EnvCub

-- | Using 'sendOSC' to send 'Bundle' message with timestamps, specifying
-- duration of new synth sent with 's_new' messages.
digiEx01 :: IO ()
digiEx01 = withSC3 $ do
    let name   = "fad01"
        sn p d = s_new name (-1) AddToTail 1
                 [("freq",midiCPS p),("dur",d),("amp",0.2)]
    _ <- async $ d_recv $ synthdef name fad01
    now <- time
    mapM_ sendOSC
        [ bundle now [sn 48 2, sn 76 1]
        , bundle (now+0.5) [sn 67 1.5]
        , bundle (now+1) [sn 60 1]
        ]
