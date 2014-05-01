{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Copyright    : 8c6794b6, 2014
-- License      : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scratch written while reading year 2004 synth secrets articles.
--
module Sound.Study.ForSynthSecrets.Y2004 where

import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..), StateT(..), modify)
import Data.Monoid ((<>))
import System.Random (getStdRandom, mkStdGen, randomR)

import Sound.OSC (Bundle(..), Message(..), Time, bundle, sendOSC, time)
import Sound.SC3
import Sound.SC3.ID

import Sound.Study.ForSynthSecrets.Y1999 (centeredOut)
import Sound.Study.ForSynthSecrets.Y2000 (soundFile01)
import Sound.Study.ForSynthSecrets.Y2003 (perform_gtd01)


-- --------------------------------------------------------------------------
--
-- * January 2004
--
-- --------------------------------------------------------------------------

-- | Simple organ sound, take 4. Percussive sound.
orgn04 :: (UGen -> UGen) -> UGen
orgn04 f0 = centeredOut $ f0 sig
  where
    sig  = foldr fsig 0 lvss * amp * 0.3
    fsig (p,a,s) acc =
        let esh  = Envelope [0,a,s,0] [1e-4,0.2,1] [EnvCub] (Just 2) Nothing
            sig' = sinOsc AR (freq*p) 0 * envGen KR tr0 1 0 dur RemoveSynth esh
        in  sig' + acc
    lvss = [ (1, 0.9,  0.4)
           , (2, 0.95, 0.45)
           , (3, 1,    0.3)
           , (4, 0.2,  0.15)
           , (5, 0.18, 0.12)
           , (6, 0.15, 0.10)
           ]
    dur  = k "dur" 1
    tr0  = k "gate" 1
    freq = k "freq" 440
    amp  = k "amp" 0.3
    k    = control KR

-- | Play 'perform_gtd01' with 'orgn04'.
play_gtd01_orgn04 :: IO ()
play_gtd01_orgn04 = perform_gtd01 (orgn04 id)

-- | Plays routine until given function returns tru.
routine_gtd01 ::
    String            -- ^ Name of synthdef.
    -> (Time -> Bool) -- ^ Function to terminate the routine.'Time' value
                      -- retrived with 'getTime' would be applied.
    -> Routine ()
routine_gtd01 name fterm = go 3000 where
  go nid = do
    let pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
        octs = take 6 $ iterate (+12) 24
        degs = [0,2,5,7]
    (t,i,d) <- liftIO $ getStdRandom $ \g0 ->
        let (i,g1) = randomR (0, length pchs - 1) g0
            (d,g2) = randomR (0, 3::Int) g1
            (t,g3) = randomR (1, 2::Int) g2
        in  ((t,i,d),g3)
    let df  = 1/4
        dt  = df * fromIntegral t
        dur = df * (2^d)
        p   = pchs !! i
    t0 <- getTime
    if fterm t0
        then return ()
        else modifyRoutine (+ dt)
             [ bundle t0 [s_new name nid AddToTail 1 [("freq",p)]]
             , bundle (t0+dur) [n_set nid [("gate",0)]]
             ] >> go (nid+1)

-- | Run 'routine_gtd01' with given synth.
run_r_gtd01 :: UGen -> IO ()
run_r_gtd01 ug = do
    let name = "r_gtd01"
    _ <- withSC3 $ async $ d_recv $ synthdef name ug
    now <- time
    runRoutine $ routine_gtd01 name (> (now + 300))

-- | Simple organ, take 5. Overdrive and equalization applied to 'orgn04'.
orgn05 :: UGen
orgn05 = orgn04 f0
  where
    f0 x = f1 (clip2 (x * pre) 0.6) * post
    f1 x = resonz x 80 rql + bBandStop x 800 rqm + resonz x 4000 rqh
    pre  = 80
    post = 0.1
    rql  = lfNoise2 'L' KR (1/8) * 0.2 + 0.79
    rqm  = lfNoise2 'M' KR (1/8) * 0.5 + 0.49
    rqh  = lfNoise2 'H' KR (1/8) * 0.2 + 0.79

-- | Play 'perform_gtd01' with 'orgn05'.
play_gtd01_orgn05 :: IO ()
play_gtd01_orgn05 = perform_gtd01 orgn05

-- | Play 'run_r_gtd01' with 'orgn05'
run_r_gtd01_orgn05 :: IO ()
run_r_gtd01_orgn05 = run_r_gtd01 orgn05


-- --------------------------------------------------------------------------
--
-- * Feburary 2004
--
-- --------------------------------------------------------------------------

-- | Simple pitch shift with 'delayC'.
pshift01 :: UGen
pshift01 = centeredOut sig
  where
    sig  = sig0 + sig1
    sig0 = mix (sinOsc AR (mce [440,660,1100]) 0 * aenv * 0.3)
    aenv = decay2 tr0 0.01 0.3
    tr0  = dust 'k' KR 2 + impulse KR 1 0
    sig1 = delayC sig0 1 (sinOsc AR dt 0 * v + v)
    dt   = 0.185
    v    = 0.301

-- | Simple effect with multile 'delayC' with varying delay time using sine
-- wave.
simpleChorus01 :: UGen -> UGen
simpleChorus01 = f0
  where
    f0 x   = hpf (foldr f1 x [0..7] * 0.5) 20
    f1 a b = b + rlpf (delayC b dt dt) 12000 0.9 * 0.25
      where
        (dt0,g1) = randomR (0.001,0.02::Double) (mkStdGen a)
        (fr0,_)  = randomR (0.01,0.2::Double) g1
        dt0'     = constant dt0
        fr0'     = constant fr0
        dt       = lfCub KR fr0' 0 * dt0' + dt0'

-- | Apply 'simpleChorus01' to sound file played with 'diskIn'.
play_simpleChorus01 :: FilePath -> IO ()
play_simpleChorus01 = playWithDiskIn simpleChorus01

-- | Apply given function to 'diskIn'.
playWithDiskIn ::
    (UGen -> UGen) -- ^ Function applied before 'diskIn' passed to 'out'.
    -> FilePath    -- ^ Sound file to play.
    -> IO ()
playWithDiskIn f path = withSC3 $ do
    let bufn :: Num a => a
        bufn = 103
        nchan :: Num a => a
        nchan = 1
        di    = diskIn nchan bufn Loop
        sig   = f di
    _ <- async $ b_alloc bufn 65536 nchan
    _ <- async $ b_read bufn path 0 (-1) 0 True
    play sig

-- | 'simpleChorus01' applied to 'orgn04'.
orgn06 :: UGen
orgn06 = orgn04 simpleChorus01

-- | Play 'perform_gtd01' with 'orgn06'.
play_gtd01_orgn06 :: IO ()
play_gtd01_orgn06 = perform_gtd01 orgn06


-- --------------------------------------------------------------------------
--
-- * March 2004
--
-- --------------------------------------------------------------------------

-- | Sample and hold example. Sample and hold sine wave signal with 'latch',
-- mouse X changes the number of triggers applied to sample-and-hold.
sah01 :: UGen
sah01 = centeredOut sig
  where
    sig  = latch sig0 tr0 * 0.1
    sig0 = sinOsc AR freq 0
    freq = mouseY KR 100 1000 Exponential 0.2
    tr0  = impulse AR (freq*shr) 0
    shr  = mouseX KR 1 32 Linear 0.2

-- | Sample and hold example, take 2. Modifying the frequency of delay time to
-- apply frequency shift to input signal.
sah02 :: UGen -> UGen
sah02 sig1 = sig
  where
    sig  = rlpf sig0 22000 0.9
    sig0 = delayC sig1 2 (1/mdl)
    mdl  = linLin lfo (-1) 1 0.8 1.2
    lfo  = sinOsc KR 0.25 0

-- | Passing percussive sine tone to 'sah02'.
sah02_sine :: UGen
sah02_sine = centeredOut $ sah02 sig
  where
    sig = sinOsc AR 440 0 * decay (dust '2' KR 2) 1 * 0.8

-- | Passing audio file signal to 'sah02'.
sah02_diskIn :: IO ()
sah02_diskIn = playWithDiskIn (centeredOut . sah02) soundFile01

-- | Simple sample and hold example, take 3. Mouse X controls the delay
-- modulation frequency.
sah03 :: UGen -> UGen
sah03 sig0 = sig1
  where
    sig1 = delayC sig0 0.5 mdl
    mdl  = linLin lfo (-1) 1 0.001 0.01
    lfo  = sinOsc KR (mouseX KR 0.25 10 Exponential 0.3) 0

-- | Percussive sine tone with random frequency, triggered with dust.
percussiveSine :: UGen
percussiveSine = sig
  where
    sig  = sinOsc AR freq 0 * aenv
    freq = tExpRand 'F' 100 8000 tr0
    aenv = envGen KR tr0 0.3 0 1 DoNothing ash
    ash  = Envelope [0,1,0] [atk,1-atk] [EnvCub] Nothing Nothing
    atk  = tExpRand 'A' 0.0001 0.9999 tr0
    tr0  = dust 'D' KR 2

-- | Pitched sine tone with amplitude envelope.
pitchedSine :: UGen
pitchedSine = sig
  where
    sig  = sinOsc AR freq 0 * aenv
    freq = tChoose 'w' tr0 (mce pchs)
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    degs = [0,4,5,7,9]
    octs = take 9 $ iterate (+12) 21
    aenv = envGen KR tr0 0.3 0 1 DoNothing ash
    ash  = Envelope [0,1,0] [atk,1-atk] [EnvCub] Nothing Nothing
    atk  = lfdNoise3 'N' KR 1 * 0.5 + 0.5
    tr0  = dust 'P' KR 2

-- | Pass percussive sine tone to 'sah03'.
sah03_sine :: UGen
sah03_sine = mrg [out 0 $ sah03 percussiveSine, out 1 percussiveSine]

-- | Play audio file with 'sah03'.
sah03_diskIn :: IO ()
sah03_diskIn = playWithDiskIn (centeredOut . sah03) soundFile01


-- --------------------------------------------------------------------------
--
-- * April 2004
--
-- --------------------------------------------------------------------------

-- | Simple delay with 'combC'.
dly01 :: UGen -> UGen
dly01 sig0 = combC sig0 0.3 0.3 4

-- | Pass 'percussiveSine' to 'dly01'.
dly01_sine :: UGen
dly01_sine = centeredOut $ dly01 percussiveSine

-- | Pass 'pitchedSine' to 'dly01'.
dly01_sine2 :: UGen
dly01_sine2 = centeredOut $ dly01 pitchedSine


-- --------------------------------------------------------------------------
--
-- * May 2004
--
-- --------------------------------------------------------------------------

-- | Simple delay, take 2. With 'localIn', 'localOut', and 'delayC'.
dly02 :: UGen -> UGen
dly02 sig0 = mrg [offsetOut 0 (pan2 sig1 0 1), localOut sig2]
  where
    sig1 = localIn 1 AR
    sig2 = delayC (sig0 + (sig1*dmul)) delt delt
    dmul = 0.85
    delt = 1.33

-- | Pass 'percussiveSine' to 'dly02'.
dly02_sine1 :: UGen
dly02_sine1 = dly02 percussiveSine

-- | Pass 'pitchedSine' to 'dly02'.
dly02_sine2 :: UGen
dly02_sine2 = dly02 pitchedSine

-- | Pass 'pitchedSine' to 'dly02'.
play_dly02_sine2 :: IO ()
play_dly02_sine2 = audition $ dly02 pitchedSine

-- | Simple delay, take 3. With 'combC' again, to get similar result as in
-- 'dly02'.
dly03 :: UGen -> UGen
dly03 sig0 = combC sig0 dlt dlt dct
  where
    dlt = 1.33
    dct = logBase 0.85 (dbAmp (-60)) * dlt

-- | Pass 'pitchedSine' to 'dly03'.
play_dly03_sine2 :: IO ()
play_dly03_sine2 = audition $ centeredOut $ dly03 pitchedSine

-- | Simple delay, take 4. With 'combC', changing delay time with 'lfdNoise0'.
dly04 :: UGen -> UGen
dly04 sig0 = combC sig0 dlt dlt dct
  where
    dlt = lfdNoise0 'D' AR lff * 2 + 2
    lff = linLin (sinOsc KR (1/16) 0) (-1) 1 (1/4) 4
    dct = logBase 0.85 (dbAmp (-60)) * dlt

-- | Play 'dly04' with 'pitchedSine'.
play_dly04_sine2 :: IO ()
play_dly04_sine2 = audition $ centeredOut $ dly04 pitchedSine

-- | Read audio file used bu 'bufLoop01'.
prepare_bufLoop01 :: FilePath -> IO Message
prepare_bufLoop01 path = withSC3 $ async $ b_allocRead 102 path 0 0

-- | Allocate and do nothing with buffer.
prepare_bufLoop01_02 :: IO Message
prepare_bufLoop01_02 = withSC3 $ async $ b_alloc 102 48000 1

-- | Simple buffer loop, take 1.
bufLoop01 :: UGen
bufLoop01 = centeredOut (sig * 0.3)
  where
    sig   = bufRd nc AR buf rphs NoLoop CubicInterpolation * aenv
    rphs  = phasor AR posT rate 0 nfrm rpos
    rate  = bufRateScale KR bufn
    nc    = 1
    buf   = 102
    nfrm  = bufFrames KR bufn
    bufn  = 102
    rpos  = tRand 'P' 0 nfrm rposT
    tr0   = impulse KR (8 * bpm/60) 0
    posT  = coinGate 'T' 0.33 tr0
    rposT = coinGate 'G' 0.12 tr0
    aenv  = envGen KR tr0 1 0 0.05 DoNothing $
            Envelope [0,1,0] [0.999,0.001] [EnvCub] Nothing Nothing
    bpm   = 120

-- | Example from /BufWr/ SC3 Help.
bufWr01 :: UGen
bufWr01 = sig0
  where
    sig0 = bufWr bufn phs NoLoop sig1
    bufn = 102
    phs  = phasor AR 0 rate 0 nfrm 0
    rate = bufRateScale KR bufn
    nfrm = bufFrames KR bufn
    sig1 = sinOsc AR freq 0 * aenv
    aenv = decay2 tr0 1e-4 0.5 * 0.3
    tr0  = dust 'A' KR 0.5
    freq = tExpRand 'Q' 100 2000 tr0

-- | Reads buffer contents written by 'bufWr01'.
bufWr01_reader :: UGen
bufWr01_reader = centeredOut sig0
  where
    sig0 = bufRd 1 AR bufn rphs NoLoop CubicInterpolation
    rphs = phasor AR 0 rate 0 nfrm 0
    nfrm = bufFrames KR bufn
    rate = bufRateScale KR bufn
    bufn = 102

-- | Ping-pong delay, from hsc3 help of 'localIn', delay time modulated with
-- 'lfdNoise0'.
del05 :: UGen -> UGen
del05 sig = mrg [localOut sig0, out 0 sig1]
  where
    sig0 = mceEdit reverse sig1 * 0.8
    sig1 = delayN sig2 delt delt
    delt = lfdNoise0 'D' KR (1/4) * 2 + 2
    sig2 = localIn 2 AR + mce [sig,0]

-- | Play 'del05' with 'pitchedSine'.
play_del05_sine2 :: IO ()
play_del05_sine2 = audition $ del05 pitchedSine

-- | Simple reverb, take 1.
rev01 :: UGen -> UGen
rev01 sig0 = mrg [out 0 (pan2 sig1 0 1), localOut sig2]
  where
    sig1 = localIn 1 AR
    sig2 = foldr f sig0 $ fst (mts 0 (mkStdGen 0xaf8931ddb4))
    f (m,t) b = b + delayC (sig1*m) t t
    mts n g0 =
        let (m,g1)  = randomR (0.02,0.03) g0
            (t,g2)  = randomR (0.010,0.850) g1
            (ps,g3) = mts (n+1) g2
        in  if n < (30::Int)
                then ((m,t):ps, g3)
                else ([(m,t)], g2)

-- | Play 'rev01' with 'pitchedSine'.
play_rev01_sine2 :: IO ()
play_rev01_sine2 = audition $ rev01 pitchedSine

-- | Simple reverb, take 2. Using ping-pong delay with different delay times for
-- out 1 and out 2.
rev02 :: UGen -> UGen
rev02 sig = mrg [localOut sig0, out 0 sig1]
  where
    sig0  = mceEdit reverse sig1 * 0.85
    sig1  = delayN sig20 0.1291 0.1291 * 0.29 +
            delayN sig20 0.0187 0.0187 * 0.24 +
            delayN sig21 0.1934 0.1934 * 0.30 +
            delayN sig21 0.0389 0.0389 * 0.21
    sig20 = localIn 2 AR + mce [sig,0]
    sig21 = localIn 2 AR + mce [0,sig]

-- | Play 'rev02' with 'pitchedSine'.
play_rev02_sine2 :: IO ()
play_rev02_sine2 = audition $ rev02 pitchedSine

-- | Play 'rev02' and 'dly03' with 'pitchedSine'.
play_rev02_dly03_sine2 :: IO ()
play_rev02_dly03_sine2 = audition $ rev02 $ dly03 pitchedSine

-- | Simple phrased sine, 'lfPulse' used as amplitude envelope.
phrasedSine :: UGen
phrasedSine = sig
  where
    sig   = sinOsc AR freq 0 * aenv2
    freq  = tChoose 'P' tr0 (mce pchs)
    pchs  = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    degs  = [0,2,5,7]
    octs  = take 8 $ iterate (+12) 36
    aenv2 = decay2 tr0 1e-4 0.3 * 0.5 * (40/cpsMIDI freq)
    tr0   = lfPulse KR 0.125 0 wdth * impulse KR hps 0
    wdth  = lfdNoise3 'W' KR 1 * 0.125 + 0.125
    hps   = 8

-- | Play 'phrasedSine' with 'rev02' and 'dly03'.
play_phrasedSine01 :: IO ()
play_phrasedSine01 = audition $ rev02 $ dly03 phrasedSine


-- --------------------------------------------------------------------------
--
-- * June 2004
--
-- --------------------------------------------------------------------------

-- | Chorus sound with manual detunes.
detuned01 :: UGen
detuned01 = mix sig
  where
    sig  = rlpf (sig0 + sig1) (2000+freq*0.25) 0.5 * aenv
    sig0 = saw AR (freq+vib)
    sig1 = pulse AR (freq+1.23) wdth
    wdth = linLin (sinOsc AR 5.3 0) (-1) 1 0.5 0.25
    aenv = envGen KR tr0 0.3 0 dur DoNothing aesh
    aesh = Envelope [0,1,1,0] [atk,sus,dec] [EnvCub] Nothing Nothing
    atk  = lfdNoise3 'A' AR 1 * 0.5 + 0.5
    sus  = tExpRand 'S' 1e-4 (1-atk) tr0
    dec  = 1 - sus
    tr0  = dust 'K' KR hps
    hps  = 0.66
    dur  = recip hps
    freq = mce $ take 3 $ map ff ['P'..]
    ff x = tChoose x tr0 (mce pchs)
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    degs = [0,3,5,7,10]
    octs = take 4 $ iterate (+12) 36
    vib  = lfTri AR 3.32 0 * 4.32

-- | Play 'detuned01'.
play_detuned01 :: IO ()
play_detuned01 = audition $ centeredOut detuned01

-- | Simple chrous effect, take 2. Using saw having vibrato and pulse with width
-- modulation.
detuned02 ::
    [UGen]  -- ^ List of frequencies to choose with 'tChoose'.
    -> UGen -- ^ Hits per seconds for 'dust'.
    -> Int  -- ^ Number of voices.
    -> UGen
detuned02 pchs hps nv = mix sig
  where
    sig  = rlpf (sig0+sig1) (freq*3.94) 0.6 * aenv
    sig0 = saw AR (freq+vib)
    sig1 = pulse AR (freq+1.23) wdth
    wdth = linLin (sinOsc AR 5.3 0) (-1) 1 0.5 0.25
    aenv = envGen KR tr0 0.3 0 dur DoNothing aesh
    aesh = Envelope [0,1,1,0] [atk,sus,dec] [EnvCub] Nothing Nothing
    atk  = lfdNoise3 'A' AR 1 * 0.5 + 0.5
    sus  = tExpRand 'S' 1e-4 (1-atk) tr0
    dec  = 1 - sus
    tr0  = dust 'K' KR hps
    dur  = recip hps
    freq = mce $ take nv $ map ff ['P'..]
    ff x = lag2 (tChoose x tr0 (mce pchs)) $
           linLin (lfdNoise3 x KR 0.25) (-1) 1 (1/64) (1/2)
    vib  = lfTri AR 3.32 0 * 4.32

-- | Play two 'detuned02'.
play_detuned02 :: IO ()
play_detuned02 =
    let pchs    = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
        octs    = iterate (+12) 0
        degs    = [0,1..11]
        pft f t = takeWhile (< (midiCPS t)) $ dropWhile (< (midiCPS f)) pchs
        sig1    = detuned02 (pft 36 55) (1/8) 1
        sig2    = detuned02 (pft 55 79) (1/2) 4
        sig     = (sig1*0.8) + (sig2*0.4)
    in  audition $ centeredOut sig

-- | Simple chorus, take 1.
chrs01 :: UGen -> UGen
chrs01 sig = sig0
  where
    sig0   = (sig + mix (delayC sig 0.01 (mce dlts))) * 0.12
    ft x p = linLin (sinOsc AR x p) (-1) 1 0.001 (rand x 0.0005 0.003)
    dlts   = let f x (n,acc) =
                     let a1 = ft x (n*pi/fromIntegral len)
                         a2 = ft (x/10) (n*pi/fromIntegral len)
                     in  (n+1,a1:a2:acc)
                 z = (0,[])
             in  snd $ foldr f z cfs
    cfs    = [5, 5.32, 5.69, 6.123, 6.379, 6.819, 7.117]
    len    = length cfs

-- | Sine with partial frequencies.
partialsSine :: UGen
partialsSine = sig
  where
    sig  = mix (sinOsc AR (freq*ps) 0) * aenv * 0.1
    freq = tChoose 'e' tr0 (mce [330,440,550])
    ps   = mce [1, 2, 2.5, 3]
    aenv = decay2 tr1 1e-3 0.5
    tr0  = impulse KR 0.5 0
    tr1  = impulse KR tf1 0
    tf1  = linLin (lfdNoise3 'F' KR 1) (-1) 1 0.5 8

-- | Play 'chrs01' and 'dly03' with 'pitchedSine'.
play_chrs01 :: IO ()
play_chrs01 = audition $ centeredOut $ chrs01 $ dly03 $ pitchedSine

-- | Simple stereo chrous. Delay time used here is too short for chrous, sounds
-- like panning rather than chorus.
chrs02 :: UGen -> UGen
chrs02 sig = mrg [out 0 sig0, out 1 sig1]
  where
    sig0 = (sig + mix (delayC sig 0.01 (fd '0' [3.52,3.91,4.13]))) * 0.3
    sig1 = (sig + mix (delayC sig 0.01 (fd '1' [3.31,3.87,4.81]))) * 0.3
    fd x dts = linLin (lfdNoise3 x AR (mce dts)) (-1) 1
               0.0005 (rand x 0.0010 0.0020)

-- | Play 'chrs02' and 'dly04' with 'pitchedSine'.
play_chrs02 :: IO ()
play_chrs02 = audition $ chrs02 $ dly04 pitchedSine


-- --------------------------------------------------------------------------
--
-- * July 2004
--
-- --------------------------------------------------------------------------

{-
Quote:

Adding A Bit Of Interest

Even if you have a clear idea how to use the building blocks of your
synthesizers, there are still a few other things it's a good idea to consider
when designing and playing sounds. For example, it's sensible to think about
timing signals, and be clear about the differences between triggers and
gates. You should also make sure that you know when it might be preferable to
use single-triggering, multi-triggering, and the various types of key priority,
as well as when monophony is superior to polyphony. You'll find all of these
discussed if you look back over the course of this series.

But not everything is cerebral. Just as important as your understanding of
sounds is your ability to play them. If you want to sound good, it makes sense
to develop good techniques for using controllers such as pitch-bend and
modulation wheels, joysticks and aftertouch. Unfortunately, the number of
synthesists who ignore articulation and expression has led to an oft-quoted
statement, usually made by guitarists of the teeth-clenching, groin-thrusting
variety, who believe that wringing the neck of a bit of dead tree constitutes a
purer form of music. They claim that "synthesizers are just a big collection of
soulless switches."

They're wrong, of course, but the inability of many keyboard players to coax
even a modicum of expression from a synth lends credibility to their views. You
can't solely rely on contour and modulation generators to do the hard work for
you. While their great strength is that they make everything consistent and
repeatable, so that a sound is recognisably the same from one note to the next,
their great weakness is that they make everything consistent and repeatable, so
that every note sounds the same from one to the next.

So, next time you're making music, why not try using the physical controllers on
your synth to adjust the way in which a note 'speaks', or add expressive vibrato
(or tremolo, or growl) using the pitch-bend wheel or ribbon, rather than (say)
relying on an LFO which always produces the same effect. It doesn't matter
whether you're playing in an orchestral style, or prog-rock, or dance, or
industrial techno... you might be surprised at the possibilities this opens up.

-}

-- --------------------------------------------------------------------------
--
-- * Miscellaneous
--
-- --------------------------------------------------------------------------

-- | Newtype wrapper for routine.
newtype Routine a = Routine {unRoutine :: StateT RoutineState IO a}
     deriving (Monad, Functor, MonadState RoutineState, MonadIO)

-- | State of routine.
data RoutineState = RoutineState
     { rsTime    :: Time
     , rsBundles :: [Bundle]
     } deriving (Eq, Show)

-- | Send all bundle messages in state.
runRoutine :: Routine a -> IO a
runRoutine routine = do
    now <- time
    let st0 = RoutineState {rsTime=now, rsBundles=[]}
    (result,st1) <- runStateT (unRoutine routine) st0
    putStrLn $ unwords ["Duration:", show (rsTime st1 - now), "secs"]
    withSC3 $ mapM_ sendOSC $ rsBundles st1
    return result

-- | Get 'rsTime' from current state.
getTime :: Routine Double
getTime = rsTime <$> get

-- | Set new tyme, and append bundle messages to current state.
modifyRoutine :: (Time -> Time) -> [Bundle] -> Routine ()
modifyRoutine ft bs = modify $ \st -> st { rsBundles = rsBundles st <> bs
                                         , rsTime    = ft $ rsTime st }

-- | Synthdef used in /Understanding Streams, Patterns and Events - Part 1/.
help_SPE1 :: UGen
help_SPE1 = out 0 sig
  where
    sig  = foldr f (rlpf sig0 cf 0.1) $ take 4 [0x832432,0x8320ffe..]
    f g0 acc = allpassN acc 0.05 (mce [dl0, dl1]) 4
      where
        (dl0,g1) = randomR (0,0.05) (mkStdGen g0)
        (dl1,_ ) = randomR (0,0.05) g1
    sig0 = lfSaw AR freq 0 * aenv
    aenv = envGen KR 1 0.3 0 1 RemoveSynth $ envPerc 0.01 1
    freq = control KR "freq" 440
    cf   = midiCPS (lfNoise1 'Z' KR 1 * 36 + 100)

-- | Send \"help_SPE1\" synthdef with 'help_SPE1'.
prepare_help_SPE1 :: IO Message
prepare_help_SPE1 = withSC3 $ async $ d_recv $ synthdef "help_SPE1" help_SPE1

-- | Make /s_new/ message with 'help_SPE1' synth.
new_help_SPE1 ::
    Double     -- ^ Pitch.
    -> Message
new_help_SPE1 ptch =
    s_new "help_SPE1" (-1) AddToTail 1 [("freq",midiCPS ptch),("amp",0.2)]

play_r0 :: IO ()
play_r0 = do
    _ <- prepare_help_SPE1
    runRoutine $ replicateM_ 32 r0

r0 :: Routine ()
r0 = do
    t0 <- getTime
    let f t p = bundle t [new_help_SPE1 p]
        bs t  = zipWith f [t,t+(1/8)..] [24,31,36,43,48,55]
        ps1   = [63, 65]
        ps2   = [70, 72, 74]
        ps3   = [74, 75, 77, 79, 81]
    coin <- liftIO $ getStdRandom $ randomR (0,1::Double)
    if coin < 0.5
        then modifyRoutine (+(6/8)) (bs t0)
        else return ()

    nrep0 <- liftIO $ getStdRandom $ randomR (2,5)
    replicateM_ nrep0 $ do
        t1 <- getTime
        (i1,i2) <- liftIO $ getStdRandom $ \g0 ->
            let (i1,g1) = randomR (0,1) g0
                (i2,g2) = randomR (0,2) g1
            in  ((i1,i2),g2)
        modifyRoutine (+(4/8)) $
            zipWith f [t1,t1+(1/8)..] [60, ps1 !! i1, 67, ps2 !! i2]

    nrep1 <- liftIO $ getStdRandom $ randomR (3,9)
    replicateM_ nrep1 $ do
        t2 <- getTime
        i <- liftIO $ getStdRandom $ randomR (0,4)
        modifyRoutine (+(1/8)) [bundle t2 [new_help_SPE1 (ps3!!i)]]

-- | 'Routine' could be sequenced, since its an instance of 'Monad'.
r1 :: Routine ()
r1 = r00 >> r01 >> r02

-- | First part of 'r1'.
r00 :: Routine ()
r00 = do
    t0 <- getTime
    let f t p = bundle t [new_help_SPE1 p]
        bs    = zipWith f [t0,t0+(1/8)..] [24,31,36,43,48,55]
    coin <- liftIO $ getStdRandom $ randomR (0,1::Double)
    if coin < 0.5
        then modifyRoutine (+(6/8)) bs
        else return ()

-- | Second part of 'r1'.
r01 :: Routine ()
r01 = do
    nrep <- liftIO $ getStdRandom $ randomR (2,5)
    replicateM_ nrep $ do
        t0 <- getTime
        (i1,i2) <- liftIO $ getStdRandom $ \g0 ->
                   let (i1,g1) = randomR (0,1) g0
                       (i2,g2) = randomR (0,2) g1
                   in  ((i1,i2),g2)
        let bs = zipWith (\t p -> bundle t [new_help_SPE1 p])
                 [t0,t0+(1/8)..] [60, [63,65] !! i1, 67, [70,72,74] !! i2]
        modifyRoutine (+(4/8)) bs

-- | Third part of 'r1'.
r02 :: Routine ()
r02 = do
    let ps = [74,75,77,79,81]
    nrep <- liftIO $ getStdRandom $ randomR (3,9)
    replicateM_ nrep $ do
        t0 <- getTime
        i <- liftIO $ getStdRandom $ randomR (0,4)
        let bs = [bundle t0 [new_help_SPE1 (ps!!i)]]
        modifyRoutine (+(1/8)) bs

-- | Example for playing buffer contents with 'localBuf'.
localBuf_ex01 :: UGen
localBuf_ex01 = mrg [centeredOut sig0, sig1]
  where
    sig0 = sinOsc AR freq 0 * amp0 + i
    amp0 = decay2 tr0 0.01 1
    tr0  = impulse KR 1 0
    sig1 = bufWr b (linLin phs (-1) 1 0 nf) Loop sig0
    b    = mrg2 (localBuf 'a' 8192 1) m
    m    = maxLocalBufs 1
    phs  = lfNoise1 'N' KR 100
    nf   = bufFrames KR b
    i    = playBuf 1 AR b x 1 0 Loop DoNothing
    freq = 100
    x    = mouseX KR 1 2 Linear 0.1
