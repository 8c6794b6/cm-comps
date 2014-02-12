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

import Sound.OSC (DuplexOSC, Message(..))
import Sound.SC3
    ( B_Gen(..), DoneAction(..), Envelope_Curve(..), Loop(..), UGen,
      Rate(..), Warp(..), async, audition, mce, mceChannel, mceEdit, mrg
    , play, reset, send, withSC3
    )
import Sound.SC3.UGen.Dot (draw_svg)
import System.FilePath ((</>))

import qualified Sound.SC3.UGen as UGen
import qualified Sound.SC3.UGen.ID as ID
import qualified Sound.SC3.Server as Server

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

-- | Using 'UGen.envGen' to control cutoff frequency of 'UGen.rlpf'.
envsAsVCAs :: UGen
envsAsVCAs = centeredOut sig0
  where
    -- output signal
    sig0   = UGen.rlpf sig1 cutoff rq * amp
    sig1   = UGen.saw AR freq
    freq   = ID.tRand 'f' 439 441 tr0
    cfreq  = 400
    dur    = 1
    -- cutoff of rlpf for sig0
    cmul   = (ID.lfNoise2 'm' KR 0.25 + 1) * 8
    cutoff = UGen.envGen KR tr0 (cfreq * cmul) cfreq dur DoNothing csh
    csh    = UGen.envCoord [(0,1),(ct0,0),(ct1,1),(1,0)] 1 1 EnvCub
    ct0    = ID.tRand '0' 1e-3 1 tr0
    ct1    = ct0 + ID.tRand '1' 1e-3 (1-ct0) tr0
    rq     = 0.8
    -- amplitude
    amp    = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash    = UGen.envCoord [(0,0),(t1,1),(1,0)] 1 1 (EnvNum envn)
    t1     = ID.lfNoise1 't' KR 1 ** 2
    envn   = ID.tRand 'n' (-10) 10 tr0
    -- trigger
    tr0    = tr1 + tr2
    tr1    = ID.dust 'D' KR (recip (dur*2))
    tr2    = UGen.impulse KR (recip dur) 0

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
    sig1   = UGen.rlpf sig0 cutoff rq * amp
    rq     = UGen.envGen KR tr 1 0 dur DoNothing rsh
    rsh    = UGen.envCoord [(0,0.1),(0.5,0.8),(1,0.1)] 1 1 EnvLin
    amp    = UGen.envGen KR tr 0.5 0 dur DoNothing ash
    ash    = UGen.envCoord [(0,0),(1e-3,1),(0.5,0.7),(1,0)] 1 1 EnvCub
    dur    = 1.5
    tr     = tr0 + tr1
    tr1    = UGen.impulse KR (recip (dur*1.5)) 0
    tr0    = ID.dust 't' KR (recip (dur*1.5))

-- | Feeding 'UGen.saw' to 'simpleFeb00'.
simpleFeb00_saw :: UGen
simpleFeb00_saw = simpleFeb00 2000 (UGen.saw AR 440)

-- | Feeding 'ID.whiteNoise' to 'simpleFeb00'.
simpleFeb00_wn :: UGen
simpleFeb00_wn = simpleFeb00 2000 (ID.whiteNoise 'w' AR)

-- | Feeding 'UGen.saw' with vibrato to 'simpleFeb00'.
simpleFeb00_vib :: UGen
simpleFeb00_vib = simpleFeb00 2000 sig
  where
    sig    = UGen.saw AR freq
    freq   = 440 + vib
    vib    = nzvib + sinvib
    sinvib = UGen.sinOsc KR 8 0 * 2.8
    nzvib  = ID.lfNoise2 'v' KR 1 * 2.2

-- | Feeding 'UGen.saw' with tremolo to 'simpleFeb00'.
simpleFeb00_trm :: UGen
simpleFeb00_trm = simpleFeb00 2000 sig
  where
    sig = UGen.saw AR 440 * amp
    amp = (UGen.sinOsc AR (2*pi) 0 + 1) * 0.5

-- | Feeding 'UGen.saw', and controlling cutoff frequency with sine wave.
-- Mouse Y controls the frquency of cutoff frequency modulator.
simpleFeb00_cutoff:: UGen
simpleFeb00_cutoff = simpleFeb00 csig ssig
  where
    csig = (UGen.sinOsc KR cfreq 0 + 1) * 1900 + 200
    cfreq = UGen.mouseY KR 1e-3 200 Exponential 0.1
    ssig = UGen.saw AR 440

-- | Feedinb 'UGen.pulse' with modulating the width to 'simpleFeb00'.
-- Mouse Y controls the frequency of pulse width modulator.
simpleFeb00_pulse :: UGen
simpleFeb00_pulse = simpleFeb00 2000 sig
  where
    sig   = UGen.pulse AR 440 width
    width = (UGen.sinOsc AR wfreq 0 + 1) * 0.5
    wfreq = UGen.mouseY KR 1e-3 100 Exponential 0.1


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
    carrier   = UGen.sinOsc AR 300 0
    modulator = ((UGen.sinOsc AR 200 0 * mamp) + 1) * 0.5
    mamp      = ID.lfNoise2 'm' KR 0.3 + 1

-- | Simple amplitude modulation, with fixed modulator frequency.
simpleInharmonicAM :: UGen
simpleInharmonicAM = centeredOut sig0
  where
    sig0      = carrier * modulator * amp
    carrier   = UGen.sinOsc AR cfreq 0
    cfreq     = ID.tRand 'f' 100 1000 tr
    modulator = (UGen.sinOsc AR 100 0 + 1) * 0.5
    amp       = UGen.envGen KR tr 0.3 0 dur DoNothing ash
    ash       = UGen.envCoord [(0,0),(1e-2,1),(0.5,0.7),(1,0)] 1 1 EnvCub
    tr        = ID.dust 't' KR (recip (dur*1.2))
    dur       = 1.75

-- | Simple ring modulation, with fixed modulator frequency.
simpleInharmonicRM :: UGen
simpleInharmonicRM = centeredOut sig0
  where
    sig0      = carrier * modulator * amp
    carrier   = UGen.sinOsc AR cfreq 0
    cfreq     = ID.tRand 'f' 100 1000 tr
    modulator = (UGen.sinOsc AR 100 0) * 0.5
    amp       = UGen.envGen KR tr 0.3 0 dur DoNothing ash
    ash       = UGen.envCoord [(0,0),(1e-2,1),(0.5,0.7),(1,0)] 1 1 EnvCub
    tr        = ID.dust 't' KR (recip (dur*1.2))
    dur       = 1.75

-- | Sine wave signal used to control cutoff frequency.
--
-- Mouse X controls index of cutoff modulator, Y controls frequency of
-- the modulator.
simpleCutoffMod :: UGen
simpleCutoffMod = centeredOut sig0
  where
    sig0      = UGen.rlpf sig1 cutoff rq * amp
    sig1      = UGen.saw AR cfreq
    cfreq     = ID.tRand 'Ζ' 100 2000 tr0
    cutoff    = (modulator * idx) + 2000
    modulator = (UGen.sinOsc AR mfreq 0 + 1) * 0.5
    mfreq     = UGen.mouseY KR 1 3000 Exponential 0.1
    idx       = UGen.mouseX KR 800 3000 Exponential 0.1
    rq        = 0.8
    amp       = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    tr0       = ID.dust 't' KR (recip dur)
    dur       = 1.25
    ash       = UGen.envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub


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
    sig       = UGen.sinOsc AR freq 0 * amp
    freq      = 1000 + (idx * modulator)
    idx       = 800
    modulator = UGen.sinOsc AR (recip 6) 0
    amp       = 0.3

-- | Simple frequency modulation.
simpleFM :: UGen
simpleFM = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = UGen.sinOsc AR freq 0
    freq      = 3300 + (mfreq * modulator * idx)
    modulator = UGen.sinOsc AR mfreq 0
    mfreq     = UGen.mouseY KR 1 4400 Exponential 0.1
    idx       = UGen.mouseX KR 0 20 Linear 0.1
    amp       = 0.3

-- | Another FM, with controlling modulator index with 'UGen.envGen'.
simpleFM2 :: UGen
simpleFM2 = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = UGen.sinOsc AR freq 0
    freq      = cfreq + (mfreq * modulator * idx)
    cfreq     = ID.tExpRand 'f' 100 1000 tr0
    mfreq     = cfreq * ratio
    ratio     = 2.001
    modulator = UGen.sinOsc AR mfreq 0
    idx       = UGen.envGen KR tr0 10 0 dur DoNothing ish
    ish       = UGen.envCoord ilvls 1 1 EnvLin
    ilvls     = [(0,0),(it0,iv0),(it1,1),(it2,iv2),(1,0)]
    it0       = ID.tRand 'i' 1e-4 (1-1e-4) tr0
    it1       = it0 + ID.tRand 'j' 1e-4 (1-it0) tr0
    it2       = it1 + ID.tRand 'k' 1e-4 (1-it1) tr0
    [iv0,iv2] = map (\c -> ID.tExpRand c 1e-4 1 tr0) "lm"
    tr0       = UGen.impulse KR ftr0 0
    ftr0      = 0.25
    dur       = recip ftr0 * 0.8
    amp       = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = UGen.envCoord [(0,0),(at,1),(1,0)] 1 1 (EnvNum 10)
    at        = ID.tExpRand 'a' 1e-4 (1-1e-4) tr0


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
    carrier   = UGen.sinOsc AR freq 0
    freq      = cfreq + (modulator * mfreq * idx)
    cfreq     = 440
    modulator = UGen.sinOsc AR mfreq 0
    mfreq     = cfreq * ratio
    ratio     = mr / cr
    cr        = 1 + UGen.pulseCount tr1 tr2
    mr        = UGen.pulseCount tr0 tr1
    tr0       = UGen.impulse KR (recip dur * 0.8) 0
    tr1       = UGen.impulse KR ((recip dur * 0.8) / 16) 0
    tr2       = UGen.impulse KR ((recip dur * 0.8) / 256) 0
    dur       = 0.20
    idx       = UGen.envGen KR tr0 10 0 dur DoNothing ish
    ish       = UGen.envCoord [(0,0),(t1,1),(1,0)] 1 1 EnvLin
    t1        = ID.tExpRand 't' 1e-4 0.999 tr0
    amp       = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = UGen.envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub

-- | Another FM with changing C:M ratio.
simpleFMRatio2 :: UGen
simpleFMRatio2 = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = UGen.sinOsc AR freq 0
    freq      = cfreq + (modulator * mfreq * idx)
    cfreq     = 440
    modulator = UGen.sinOsc AR mfreq 0
    mfreq     = cfreq * ratio
    ratio     = mr / cr
    mr        = UGen.floorE (ID.tRand 'm' 1 17 tr0)
    cr        = UGen.floorE (ID.tRand 'c' 1 17 tr00)
    idx       = UGen.envGen KR tr0 10 0 dur DoNothing ish
    ish       = UGen.envCoord [(0,0),(1e-1,1),(1,0)] 1 1 EnvCub
    amp       = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = UGen.envCoord [(0,1e-4),(1e-3,1),(1,1e-4)] 1 1 EnvExp
    dur       = 0.7
    tr0       = tr00 + tr1 + tr2 + tr3
    tr00      = UGen.impulse KR (recip dur) 0
    tr1       = ID.coinGate 'g' 0.6 (UGen.impulse KR (recip dur * 2) 0)
    tr2       = ID.coinGate 'G' 0.3 (UGen.impulse KR (recip dur * 4) 0)
    tr3       = ID.coinGate 'γ' 0.21 (UGen.impulse KR (recip dur * 8) 0)

-- | FM with fixed ratio.
simpleFMTimber :: UGen
simpleFMTimber = centeredOut sig
  where
    sig       = carrier * amp
    carrier   = UGen.sinOsc AR freq 0
    freq      = cfreq + (modulator * mfreq * idx)
    cfreq     = 220 * cmul
    cmul      = 1.5 ** UGen.floorE (ID.tRand 'c' 1 3 tr0)
    modulator = UGen.sinOsc AR mfreq 0
    mfreq     = cfreq * ratio
    ratio     = 2.000002 * UGen.floorE (ID.tRand 'r' 1 2.2 tr0)
    idx       = UGen.envGen KR tr0 10 0 dur DoNothing ish
    ish       = UGen.envCoord [(0,0),(it0,1),(1,0)] 1 1 EnvSin
    it0       = ID.tRand 't' 0 1 tr0
    amp       = UGen.envGen KR tr0 0.3 0 dur DoNothing ash
    ash       = UGen.envCoord [(0,0),(1e-3,1),(0.8,0.5),(1,0)] 1 1 EnvCub
    dur       = recip hps * 0.8
    tr0       = UGen.impulse KR hps 0
    hps       = 0.55

-- | Ping-pong delay example from 'UGen.localIn'
pingPongWithLocalIn :: IO ()
pingPongWithLocalIn =
    let n  = ID.whiteNoise 'a' AR
        a0 = UGen.decay (UGen.impulse AR 0.3 0) 0.1 * n * 0.2
        a1 = UGen.localIn 2 AR + mce [a0,0]
        a2 = UGen.delayN a1 0.2 0.2
        a3 = mceEdit reverse a2 * 0.8
    in  audition (mrg [UGen.localOut a3, UGen.out 0 a2])

-- | Simple feedback FM.
simpleFeedbackFM :: UGen
simpleFeedbackFM = mrg [o0, o1]
  where
    o0        = centeredOut (sig0 * amp)
    sig0      = UGen.sinOsc AR freq modulator
    freq      = UGen.mouseY KR 110 880 Exponential 0.1
    modulator = UGen.localIn 1 AR * idx
    idx       = UGen.mouseX KR 0 3 Linear 0.1
    amp       = UGen.envGen KR tr 0.3 0 1 DoNothing ash
    ash       = UGen.envCoord [(0,0),(1e-3,1),(1,0)] 1 1 EnvCub
    tr        = UGen.impulse KR 1 0
    o1        = UGen.localOut sig0


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
    mkosc x = UGen.sinOsc AR (x*freq) 0 * (1/x)
    freq    = sum9thFreqs tr0
    amp     = UGen.envGen KR tr0 0.3 0 1 DoNothing ash
    ash     = UGen.envCoord [(0,0),(1e-2,1),(1-1e-2,1),(1,0)] 1 1 EnvCub
    tr0     = UGen.impulse KR 8 0

-- | Saw tooth oscillator, to compare with 'manualSum9th'.
sawForSum9th :: UGen
sawForSum9th = centeredOut sig0
  where
    sig0  = UGen.saw AR freq * amp
    freq  = sum9thFreqs tr0
    amp   = UGen.envGen KR tr0 0.3 0 1 DoNothing ash
    ash   = UGen.envCoord [(0,0),(1e-3,1),(0.7,0.2),(1,0)] 1 1 EnvCub
    tr0   = UGen.impulse KR 8 0

-- | Manually summed up sine waves to 9th partial, take 2.
-- Each partial has individual amplitude, higher frequency decaying
-- sooner.
manualSum9th2 :: UGen
manualSum9th2 = centeredOut sig0
  where
    sig0    = sum oscs * 0.1
    oscs    = map mkosc [1..9]
    mkosc x = UGen.sinOsc AR (x*freq) 0 * mkamp x
    mkamp x = UGen.envGen KR tr0 (3/x) 0 (1/x) DoNothing ash
    ash     = UGen.envCoord lvls 1 1 EnvCub
    lvls    = [(0,0),(1e-3,1),(1,0)]
    freq    = sum9thFreqs tr0
    tr0     = UGen.impulse KR hps 0
    hps     = 8

-- | Manually summed up sine waves to 9th partial, take 3.
-- Each partial has individual amplitude, controlled with 'ID.lfNoise2'.
manualSum9th3 :: UGen
manualSum9th3 = centeredOut sig0
  where
    sig0    = sum oscs * 0.1
    nosc :: Num a => a
    nosc    = 9
    oscs    = map mkosc [1..nosc]
    mkosc x = UGen.sinOsc AR (x*freq) 0 * mkamp x
    mkamp x = UGen.envGen KR tr0 1 0 (recip hps * 0.8) DoNothing (ash x)
    ash x   = UGen.envCoord [(0,0),(at x,1),(1,0)] 1 1 EnvCub
    at x    = UGen.select x nzs
    nzs     = mce $ map (\c -> (ID.lfNoise2 c KR 0.2 + 1)*0.5) [(1::Int)..nosc]
    freq    = sum9thFreqs tr0
    tr0     = UGen.impulse KR hps 0
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
    sig     = ID.tChoose 'p' tr0 $ mce $ map UGen.midiCPS ptchs
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

-- | From hsc3 help file of 'UGen.diskIn'.
diskIn_example ::
    FilePath -- ^ Input sound file.
    -> IO ()
diskIn_example filepath = withSC3 $ do
    let n   = 1
        d   = UGen.diskIn n 0 Loop
        o0  = UGen.out 0 (d * 0.3)
        g   = o0
    _ <- async $ Server.b_alloc 0 65536 n
    _ <- async $ Server.b_read 0 filepath 0 (-1) 0 True
    play g

-- | Playing input file and sine oscillator with analyzed frequency and
-- amplitude.
followingSine ::
    FilePath -- ^ Input sound file to extract pitch and amplitude envelope.
    -> IO ()
followingSine file = withSC3 $ do
    let n      = 1
        input  = UGen.diskIn n 0 Loop
        ifilt  = UGen.hpf (UGen.lpf input 3000) 100
        ptc    = UGen.pitch ifilt 440 60 4000 100 16 1 0.05 0.5 1
        freq   = UGen.lag2 ptc 5e-4
        acurve = UGen.amplitude KR ifilt 0.1 0.1
        amp    = UGen.lag2 acurve 5e-4
        sig    = UGen.sinOsc AR freq 0 * amp
        o0     = UGen.out 0 (input * 0.3)
        o1     = UGen.out 1 sig
        outs   = mce [o0, o1]
    _ <- async $ Server.b_alloc 0 65536 n
    _ <- async $ Server.b_read 0 file 0 (-1) 0 True
    play outs

-- | Frequency used in 'simpleVocoder01'.
vocFreqs :: UGen -> UGen
vocFreqs tr0 = ID.tChoose 'v' tr0 $ mce $ map UGen.midiCPS ptcs
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
simpleVocoder01 ::
    FilePath -- ^ Input sound file.
    -> IO ()
simpleVocoder01 file = withSC3 $ do
    let n      = 1
        bnum   = 0
        input  = UGen.diskIn n 0 Loop
        mkbank f = UGen.rlpf (UGen.pulse AR f 0.5) (aFreq*36) 0.3 * amp1
          where
            bank  = UGen.resonz input f 0.05 * 3
            amp0  = UGen.amplitude KR bank 0.1 0.1
            amp1  = UGen.lag2 amp0 lagt * 0.8
        freqs  = map (\x -> aFreq * x) [1..36]
        aFreq  = UGen.lag2 (vocFreqs (UGen.impulse KR hps 0)) (recip hps * 0.1)
        hps    = 4
        lagt   = UGen.mouseX KR 1e-4 4 Exponential 0.1
        comp x = UGen.compander x cnt thresh slBelow slAbove clTime relTime
          where
            cnt     = x
            thresh  = 0.8
            slBelow = 1
            slAbove = 1
            clTime  = 0.01
            relTime = 0.1
        outs   = centeredOut . comp . sum $ map mkbank freqs
    _ <- async $ Server.b_alloc bnum 65536 n
    _ <- async $ Server.b_read bnum file 0 (-1) 0 True
    play outs

-- | Another simple vocoder.
--
-- Using 36 banks to detect pitch and amplitude envelope of input sound
-- file. Mouse X controls lag time of oscillator pitches, mouse Y controls
-- frequency factor for oscillators.
--
simpleVocoder02 ::
    FilePath -- ^ Input sound file.
    -> IO ()
simpleVocoder02 file = withSC3 $ do
    let n     = 1
        bnum  = 0
        input = UGen.diskIn n 0 Loop
        mkbank f = UGen.sinOsc AR ptc2 0 * amp1
          where
            bank = UGen.resonz input f 0.05 * 3
            ptc0 = UGen.pitch bank f (f*0.5) (f*2) 100 16 1 0.01 0.5 1
            ptc1 = UGen.lag3 ptc0 (lagt / logBase 1.22 (f/20))
            ptc2 = mceChannel 0 ptc1 * prat
            prat = UGen.mouseY KR 0.5 2 Exponential 0.1
            amp0 = UGen.amplitude KR bank 0.1 0.1
            amp1 = UGen.lag2 amp0 lagt * 0.8
        freqs = map (\x -> 20 * (1.22 ** x)) [0..35]
        lagt  = UGen.mouseX KR 1e-4 4 Exponential 0.1
        comp x = UGen.compander x cnt thresh slBelow slAbove clTime relTime
          where
            cnt     = x
            thresh  = 0.8
            slBelow = 1
            slAbove = 1
            clTime  = 0.01
            relTime = 0.1
        outs  = centeredOut . comp . sum $ map mkbank freqs
    _ <- async $ Server.b_alloc bnum 65536 n
    _ <- async $ Server.b_read bnum file 0 (-1) 0 True
    play outs


-- --------------------------------------------------------------------------
--
-- * August 2000
--
-- --------------------------------------------------------------------------

-- | Simple sample and hold using 'UGen.latch'.
--
-- From SC help file:
--
-- /... LFNoise0 is a faster way to generate random steps ... /
--
sampleAndHold01 :: UGen
sampleAndHold01 = centeredOut sig
  where
    sig  = UGen.resonz (UGen.saw AR 220) freq rq * amp
    freq = UGen.latch nz0 tr0
    nz0  = (ID.whiteNoise 'n' KR + 1) * 1200 + 200
    tr0  = UGen.impulse KR hps 0
    hps  = 0.5
    rq   = UGen.envGen KR tr0 1 0 (recip hps * 0.75) DoNothing rsh
    rsh  = UGen.envCoord [(0,0.1),(0.5,1),(1,0.1)] 1 1 EnvCub
    amp  = UGen.envGen KR tr0 1 0 (recip hps * 0.75) DoNothing ash
    ash  = UGen.envCoord [(0,0),(1e-2,1),(0.7,0.3),(1,0)] 1 1 EnvCub

-- | Simple track and hold using 'UGen.gate' and 'ID.lfNoise2'.
trackAndHold01 :: UGen
trackAndHold01 = centeredOut sig
  where
    sig   = UGen.sinOsc AR freq 0 * amp
    freq  = (UGen.gate hold tr0) * 2200 + 150
    hold  = ID.lfNoise2 'e' KR (hfreq * 4)
    hfreq = 1.25
    tr0   = UGen.pulse KR hfreq 0.75
    amp   = UGen.envGen KR tr0 0.3 0 (recip hfreq * 0.8) DoNothing ash
    ash   = UGen.envCoord [(0,0),(1e-2,1),(0.8,0.5),(1,0)] 1 1 EnvCub

-- | Simple sequence with demand ugens: 'UGen.demand', 'ID.dseq', and 'ID.dinf'.
simpleSequence01 :: UGen
simpleSequence01 = centeredOut sig
  where
    sig   = UGen.sinOsc AR freq phase * amp
    phase = UGen.sinOsc AR (freq*1.0051) 0 * idx
    idx   = (UGen.fSinOsc AR 0.125 0 * ID.lfNoise2 'i' KR 0.25 + 1) * 10
    freq  = UGen.lag2 (UGen.demand tr0 0 freqs) 0.1
    freqs = ID.dseq 'a' ID.dinf ptchs
    ptchs = mce $ map UGen.midiCPS ps
    ps    = [60,60,55,58]
    tr0   = UGen.impulse KR hps 0
    hps   = 4
    amp   = UGen.envGen KR tr0 0.3 0 (recip hps * 0.8) DoNothing ash
    ash   = UGen.envCoord [(0,0),(2e-2,1),(0.8,0.3),(1,0)] 1 1 EnvSin

-- | Simple sequence with 'UGen.select' and 'UGen.pulseCount'.
simpleSequence02 :: UGen
simpleSequence02 = centeredOut sig
  where
    sig   = UGen.sinOsc AR freq phase * amp
    freq  = UGen.select (UGen.pulseCount tr0 tr1) (mce ptcs)
    ptcs  = map UGen.midiCPS $ zipWith (+) octs (cycle degs)
    octs  = concatMap (replicate (length degs)) [60,63,58,55]
    degs  = [-12,-7,-5,0,5,7,12]
    phase = UGen.sinOsc AR (freq * 1.5001) 0 * idx
    idx   = (ID.fSinOsc KR (recip (hps * 4)) 0 + 1) * 10
    amp   = UGen.envGen KR tr0 0.3 0 (recip hps *  0.8) DoNothing ash
    ash   = UGen.envCoord [(0,0),(1e-3,1),(0.1,0.5),(0.8,0.5),(1,0)] 1 1 EnvCub
    tr0   = UGen.impulse KR hps 0
    tr1   = UGen.impulse KR (hps / fromIntegral (length ptcs)) 0
    hps   = 8

-- | Another sequence with 'UGen.select', 'UGen.pulseCount', 'UGen.tChoose' and
-- 'ID.coinGate'.
simpleSequence03 :: UGen
simpleSequence03 = centeredOut sig
  where
    sig    = UGen.sinOsc AR freq phase * amp
    freq   = seq03Freqs hps keys
    keys   = [0,2,3,5,7,9,10,12,14,15,17,19,21,22]
    hps    = 8
    tr0    = UGen.impulse KR hps 0
    phase  = UGen.sinOsc AR (freq * 1.50092) 0 * idx
    idx    = (UGen.sinOsc KR (recip hps * 0.25) 0 + 1) * 5
    amp    = UGen.envGen KR tr0 0.3 0 (recip hps * 0.95) DoNothing ash
    ash    = UGen.envCoord [(0,0),(1e-3,1),(2e-2,0.8),(1,0)] 1 1 EnvCub

-- | Frequency sequence pattern used in 'simpleSequence03'.
seq03Freqs ::
    UGen      -- ^ Hits per seconds.
    -> [UGen] -- ^ Degree values.
    -> UGen
seq03Freqs hps keys = freq
  where
    freq   = UGen.select (UGen.pulseCount tr0 tr1) (mce ptcs)
    tr0    = UGen.impulse KR hps 0
    tr1    = UGen.impulse KR (hps / fromIntegral (length ptcs)) 0
    ptcs   = map UGen.midiCPS $ zipWith (+) octs (cycle degs)
    octs   = concatMap (replicate (length degs)) [60]
    degs   = take 8 $ map degf ['a'..]
    degf c = ID.tChoose c (ID.coinGate c (1/29) tr1) (mce keys)


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

-- | From hsc3 help file of 'UGen.shaper'. This action is used by other
-- examples.
shaper_ex01 :: DuplexOSC m => [Double] -> m Message
shaper_ex01 a = do
    _ <- async (Server.b_alloc 10 512 1)
    let f = [Normalise, Wavetable, Clear]
    async $ Server.b_gen_cheby 10 f a

-- | From hsc3 help file of 'UGen.shaper'.
shaper_ex02 :: IO ()
shaper_ex02 = withSC3 $ do
    let s = UGen.sinOsc AR 300 0 * UGen.line KR 0 1 6 RemoveSynth
    _ <- shaper_ex01 [1,0,1,1,0,1]
    play (UGen.out 0 (UGen.shaper 10 s * 0.1))

-- | Another example from hsc3 help file of 'UGen.shaper'.
shaper_ex03 :: IO ()
shaper_ex03 = withSC3 $ do
    let s = UGen.sinOsc AR 400 (pi/2) * UGen.line KR 0 1 6 RemoveSynth
    _ <- shaper_ex01 [0.25,0.5,0.25]
    play $ UGen.out 0 (UGen.shaper 10 s * 0.1)

-- | Simple wave table oscillator with 'UGen.osc'.
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
        sig   = UGen.osc AR bnum 220 0 * 0.2
    _ <- async $ Server.b_alloc bnum bsize 1
    send $ Server.b_set bnum $ zip [0..] vals'
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

simpleTriggers01 :: UGen
simpleTriggers01 = centeredOut sig
  where
    sig   = UGen.sinOsc AR freq phase * amp
    freq  = UGen.lag3 (ID.tChoose 'f' tr0 ptchs) 0.0125
    ptchs = mce $ map UGen.midiCPS [36,58,60,62,63,67]
    phase = UGen.mix (UGen.sinOsc AR (mce [freq*1.4998,freq*4.003]) 0) * idx
    idx   = UGen.envGen KR tr1 5 0 (recip cps1 * 0.85) DoNothing ash
    amp   = UGen.envGen KR tr1 0.3 0 (recip cps1 * 0.98) DoNothing ash
    ash   = UGen.envCoord lvls 1 1 EnvCub
    lvls  = [(0,0),(0.1,1),(0.2,0.8),(0.8,0.8),(1,0.3)]
    tr0   = ID.coinGate 'a' 0.7 (UGen.impulse KR cps0 0)
    tr1   = ID.coinGate 'd' 0.8 (UGen.impulse KR cps1 0)
    cps0  = 1.5
    cps1  = 0.8


-- --------------------------------------------------------------------------
--
-- * November 2000
--
-- --------------------------------------------------------------------------
