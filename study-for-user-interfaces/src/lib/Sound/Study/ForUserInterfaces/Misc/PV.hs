{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Scratch written while reading FFT related sc3 help files.

-}
module Sound.Study.ForUserInterface.Misc.PV where

import Control.Applicative ((<$>))
import System.Random

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.UGen.Protect (uclone, uclone')


-- --------------------------------------------------------------------------
--
-- * FFT Overview
--
-- --------------------------------------------------------------------------

-- | Example of 'pv_RandComb'.
ov_ex01 :: IO ()
ov_ex01 = audition $ out 0 (pan2 osig pan 1)
  where
    isig   = whiteNoise 'w' AR * 0.8 * e
    e      = envGen KR tr 1 0 dur DoNothing $ envPerc 0.01 1
    dur    = recip tf * 0.95
    buf    = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 1)
    tr     = impulse KR tf 0
    tf     = mouseX KR 0.5 30 Exponential 0.1
    chain1 = fft' buf isig
    chain2 = pv_RandComb 'b' chain1 0.95 tr
    osig   = ifft' chain2
    pan    = tRand 'p' tr (-1) 1

-- | Another example of 'pv_RandComb' with two local bufs using 'uclone'.
ov_ex02 :: IO ()
ov_ex02 = audition $ out 0 osig
  where
    osig   = ifft' chain0
    chain0 = pv_RandComb 'b' chain1 0.95 tr
    chain1 = fft' bufs insig
    -- bufs   = mrg2 (uclone 'a' 2 (localBuf 'a' 2048 1)) (maxLocalBufs 2)
    bufs   = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 2)
    insig  = ringz (impulse AR (mce [2,3]) 0) (mce [700,800]) 0.1 * 5
    tr     = impulse KR 0.4 0

-- | Example of /in place/ writing done in PV ugens.
ov_ex03 :: IO ()
ov_ex03 = audition $ out 0 (mce [osig, osig])
  where
    osig   = ifft' chain0 * 0.05
    chain0 = pv_MagMul chainA chainB
    chainA = fn 'a' inA
    chainB = fn 'b' inB
    fn k sig = fft'  (mrg2 (localBuf k 2048 1) (maxLocalBufs 2)) sig
    inA    = lfSaw AR my 0 * 0.2
    inB    = ringz (impulse AR mx 0) 700 0.5
    my     = mouseY KR 100 1000 Exponential 0.1
    mx     = mouseX KR 1 100 Linear 0.1

-- | Example similar to 'ov_ex03', using sound file as input signal with
-- 'playBuf'.
--
-- >>> ov_ex04 sharp_mono
-- >>> ov_ex04 bsd_mono
-- >>> ov_ex04 ted_intro_mono
-- >>> ov_ex04 rakista_mono
--
ov_ex04 :: FilePath -> IO ()
ov_ex04 file = withSC3 $ do
    let bufn   :: Num a => a
        bufn   = 12
        inA    = lfSaw AR (mce [my,my*2,my*3,my*5]) 0 * 0.2
        my     = mouseY KR 25 200 Exponential 0.1
        inB    = playBuf 1 AR bufn brate 1 0 Loop DoNothing * 0.1
        brate  = bufRateScale KR bufn * (lfdNoise3 'R' KR mx ** 2) * 4 + 0.25
        mx     = mouseX KR 0.25 25 Exponential 0.1
        fn k s = fft' (mrg2 (localBuf k 2048 1) (maxLocalBufs 2)) s
        chainA = fn 'a' inA
        chainB = fn 'b' inB
        chain  = pv_MagMul chainA chainB
        osig   = mix $ ifft' chain
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 (pan2 osig 0 1)

-- | Example showing use of 'pv_Copy'. Cancelling with same signal to get
-- silence.
--
-- XXX: 'pv_Copy' not working?
--
ov_ex05 :: IO ()
ov_ex05 = withSC3 $ do
    mapM_ (\n -> async $ b_alloc n 2048 1) [0,1]
    play $ out 0 osig
  where
    osig   = ifft' chainA - ifft' chainB
    chainA = fft' 0 inA
    chainB = pv_Copy chainA 1
    inA = lfClipNoise 'C' AR 100 * 0.1

-- | Example showing multichannel expansion. FFT ugen is using 'localBuf' with
-- 'maxLocalBufs' = 2. Input signal is two channel 'mce' signal: 'sinOsc' and
-- 'whiteNoise'.
ov_ex06 :: IO ()
ov_ex06 = audition $ out 0 osig
  where
    osig   = ifft' chain0
    chain0 = pv_BrickWall chain1 (sinOsc KR (-0.1) 0)
    chain1 = fft' bufs insig
    bufs   = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 2)
    insig  = mce [sinOsc AR 200 0 * 0.2, whiteNoise 'W' AR * 0.2]


-- --------------------------------------------------------------------------
--
-- * BeatTrack
--
-- --------------------------------------------------------------------------

beatTrack_ex01 :: UGen -> UGen
beatTrack_ex01 ug = out 0 (pan2 (osig + ug) 0 1)
  where
    osig = mix $ sinOsc AR frqs 0 * amps * decay trgs 0.05
    frqs = mce [440, 660, 880]
    amps = mce [0.4, 0.2, 0.1]
    trgs = mce [tb,  th,  tq ]
    buf = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 1)
    mx = mouseX KR (-1) 1 Linear 0.2
    [tb, th, tq, _bpm] = mceChannels $ beatTrack insig mx
    insig = fft' buf ug

beatTrack_ex01_sine :: IO ()
beatTrack_ex01_sine = audition $ beatTrack_ex01 sig
  where
    sig = sinOsc AR 100 0 * decay (impulse KR 2.32 0) 0.1 * 0.1

beatTrack_ex01_soundfile :: FilePath -> IO ()
beatTrack_ex01_soundfile file = withSC3 $ do
    _ <- async $ b_allocRead 12 file 0 0
    let sig  = playBuf 1 AR 12 rate 1 0 Loop DoNothing
        rate = bufRateScale KR 12
    play $ beatTrack_ex01 sig


-- --------------------------------------------------------------------------
--
-- * Convolution
--
-- --------------------------------------------------------------------------

convolution_ex01 :: UGen -> UGen
convolution_ex01 i = out 0 (pan2 sig0 0 0.3)
  where
    sig0 = convolution i k 1024 * 0.3
    k    = mix $ lfSaw AR (mce [300,500,800,1000] * mx) 0
    mx   = mouseX KR 1 2 Linear 0.1

convolution_ex01_01 :: IO ()
convolution_ex01_01 = withSC3 $
    play $ convolution_ex01 $ in' 2 AR numInputBuses

convolution_ex01_02 :: IO ()
convolution_ex01_02 = withSC3 $ do
    let file = sharp_mono
        bufn = 12 :: Num a => a
        i = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing * 0.1
    _ <- async $ b_allocRead 12 file 0 0
    play $ convolution_ex01 i

convolution_ex02 :: Transport m => UGen -> m ()
convolution_ex02 insig = do
    gen0 <- liftIO newStdGen
    let vals  = take len $ randomRs (0,1) gen0
        len   = 2048
        bufn  = 0
        nchan = 1
        krnl  = playBuf 1 AR (constant bufn)
                (bufRateScale KR (constant bufn)) 1 0 Loop DoNothing
        sig0  = convolution insig krnl (constant (len*2)) * 0.5
    _ <- async $ b_alloc bufn len 1
    send $ b_setn1 bufn nchan vals
    play $ out 0 (pan2 sig0 0 0.3)

convolution_ex02_01 :: IO ()
convolution_ex02_01 = withSC3 $ do
    let file  = sharp_mono
        insig = playBuf 1 AR 12 (bufRateScale KR 12) 1 0 Loop DoNothing * 0.1
    _ <- async $ b_allocRead 12 file 0 0
    convolution_ex02 insig

convolution_ex02_02 :: IO ()
convolution_ex02_02 = withSC3 $ convolution_ex02 $ in' 2 AR numInputBuses

-- | Synonym for type of convolution UGens.
type ConvolutionUGen = UGen -> UGen -> UGen -> UGen -> UGen -> UGen

gen_convtest :: ConvolutionUGen -> IO ()
gen_convtest ug = withSC3 $ do
    let bs@[b0,b1,b2] = [0,1,2]
        fillbuf n start len interval vals =
            send . b_set n $ take len $ zip (iterate (+interval) start) vals

    mapM_ (\n -> async $ b_alloc n 2048 1) bs
    fillbuf b0 10 50 10 . randomRs (0,1) =<< liftIO newStdGen
    fillbuf b1 100 3 400 (repeat 1)
    fillbuf b2 20 20 40 (repeat  1)

    let def   = out 0 (ug insig krnl tr 2048 100)
        insig = impulse AR 1 0
        tr    = tr_control "trig" 0
        krnl  = control KR "kernel"0

    _ <- async $ d_recv $ synthdef "conv-test" def
    send $ s_new "conv-test" (-1) AddToHead 1 []

-- | Run 'gen_convtest' with 'convolution2'.
convolution2_ex01 :: IO ()
convolution2_ex01 = gen_convtest $ \insig krnl tr bufsz _ ->
    convolution2 insig krnl tr bufsz

-- | Try:
--
-- >>> convolution2_ex01
-- >>> convolution2_ex01_set [("trig",1),("kernel",1)]
-- >>> convolution2_ex01_set [("trig",1),("kernel",2)]
-- >>> convolution2_ex01_set [("trig",1),("kernel",0)]
--
convolution2_ex01_set :: [(String,Double)] -> IO ()
convolution2_ex01_set kv = withSC3 $ send $ n_set (-1) kv

-- | Try:
-- >>> convolution2_ex01_setbuf 0 5 300 80
convolution2_ex01_setbuf :: Int -> Int -> Int -> Int -> IO ()
convolution2_ex01_setbuf bufn len start interval = withSC3 $ do
    send $ b_set bufn $ take len $ zip (iterate (+interval) start) (repeat 1)

-- | Try:
--
-- >>> convolution2_ex02 fp_10_to_40
-- >>> convolution2_ex02 a11wlk01
--
convolution2_ex02 :: FilePath -> IO ()
convolution2_ex02 file = withSC3 $ do
    let bufn = 12
        osig = convolution2 isig (constant bufn) tr 8192 * 0.5
        isig = blip AR frq 32 * decay tr (recip mx * 0.8)
        tr   = impulse KR mx 0
        frq  = 440 * tIRand 'F' 1 16 tr
        mx   = mouseX KR 0.5 32 Exponential 0.1

        -- Alternatively, using buffer sound as input to convolution
        -- isig = playBuf 1 AR (constant bufn) rate tr1 pos Loop DoNothing
        -- rate = 1.5 ** tIRand 'r' (-2) 2 tr1 * bufRateScale KR (constant bufn)
        -- pos  = tIRand 'p' 0 15 tr2 * (bufFrames KR (constant bufn) / 15)
        -- tr1  = coinGate '1' 0.5 tr
        -- tr2  = coinGate '2' 0.5 tr

    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig

-- | Convolution with random impulse response.
--
-- >>> convolution2_ex03 bsd_mono
--
convolution2_ex03 :: FilePath -> IO ()
convolution2_ex03 file = withSC3 $ do
    let bufk = 0
        bufi = 12
        sz   = 2048

        osig = convolution2 (isig*gt) (constant bufk) 0 (constant sz) * 0.5
        isig = playBuf 1 AR (constant bufi) rate 0 0 Loop DoNothing
        rate = bufRateScale KR (constant bufi)
        gt   = iamp >* 0.02 `lag` 0.01
        iamp = amplitude KR isig 0.01 0.01

    _ <- async $ b_alloc bufk sz 1
    gen <- liftIO newStdGen
    send $ b_set bufk $ take sz $
        zip (randomRs (0,2047) gen) (map recip [1..100])
    _ <- async $ b_allocRead bufi file 0 0
    play $ out 0 $ mce2 osig osig

-- | Convolution with buffer having values filled with 'b_gen_sine1' command.
convolution2_ex04 :: FilePath -> IO ()
convolution2_ex04 file = withSC3 $ do
    let bufi = 12
        bufk = 0
        sz   = 2048
        c    = constant
        osig = convolution2 isig (c bufk) 0 (c sz) * 0.3
        isig = playBuf 1 AR (c bufi) rate 0 0 Loop DoNothing
        rate = bufRateScale KR (c bufi)

    _ <- async $ b_allocRead bufi file 0 0
    _ <- async $ b_alloc bufk sz 1
    send $ b_gen_sine1 bufk [Normalise,Wavetable,Clear] [recip x|x<-[1..6]]
    play $ out 0 $ mce2 osig osig

convolution2_ex05 :: IO ()
convolution2_ex05 = audition $ mrg2 (out 0 $ mce2 osig osig) recbuf
  where
    osig          = fade * conv0 + (1-fade) * conv1
    fade          = lfTri KR (trigFreq*0.5) 1 * 0.5 + 0.5
    [conv0,conv1] = mceChannels conv
    conv          = convolution2 isig bufk convTrigs frames
    frames        = bufFrames KR bufk
    convTrigs     = pulseDivider convTrig 2 (mce [0,1])
    convTrig      = tDelay recTrig (bufDur KR bufk)
    recbuf        = recordBuf AR bufk 0 1 0 1 NoLoop recTrig DoNothing irSig
    irSig         = saw AR (tExpRand 's' minFreq maxFreq recTrig) * 0.4
    recTrig       = impulse KR trigFreq 0
    trigFreq      = recip trigPeriod
    isig          = lpf (dust2 'd' AR density) cutoff
    trigPeriod    = k "trigPeriod" 10
    minFreq       = k "minFreq" 200
    maxFreq       = k "maxFreq" 2000
    cutoff        = k "cutoff" 1000
    density       = k "density" 200
    k             = control KR
    bufk          = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 2)

-- | Almost same with 'convolution2_ex01', but using 'convolution2L'.
convolution2L_ex01 :: IO ()
convolution2L_ex01 = gen_convtest convolution2L


-- --------------------------------------------------------------------------
--
-- * FFT
--
-- --------------------------------------------------------------------------

fft_ex01 :: IO ()
fft_ex01 = audition $ out 0 $ mce2 sig0 sig0
  where
    sig0   = ifft' chain0
    chain0 = fft' buf  sig1
    buf    = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 1)
    sig1   = whiteNoise 'W' AR * 0.1

-- fft_ex02 is using '.inspect' method.

fft_ex03 :: IO ()
fft_ex03 = audition $ out 0 (pan2 osig 0 1)
  where
    osig   = ifft' chain0 * 0.5
    chain0 = fft' buf isig
    buf    = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 1)
    isig   = sinOsc AR frq 0
    frq    = sinOsc KR (sinOsc KR 0.08 0 * 6 + 6.2) 0 * 100 + 800

fft_ex04 :: IO ()
fft_ex04 = audition $ out 0 (pan2 sig0 0 1)
  where
    sig0   = ifft' chain0 * 0.5
    chain0 = pv_MagAbove chain1 310
    chain1 = fft' buf sig1
    buf    = localBuf' 'a' 2048 1 1
    sig1   = sinOsc AR frq 0
    frq    = sinOsc KR (squared (sinOsc KR 0.08 0 * 6 + 6.2)) 0 * 100 + 800

fft_ex05 :: IO ()
fft_ex05 = withSC3 $ do
    let ug = pv_withWhiteNoise $ \chain ->
            pv_BrickWall chain (sinOsc KR 0.1 0)
    play $ out 0 $ mce2 ug ug

fft_ex06 :: IO ()
fft_ex06 = withSC3 $ do
    let ug = pv_withWhiteNoise $ \chain ->
            pv_RandComb 'a' chain 0.95 (impulse KR 0.4 0)
    play $ out 0 $ mce2 ug ug

fft_ex07 :: IO ()
fft_ex07 = withSC3 $ do
    let ug = pv_withWhiteNoise $ \chain ->
            let x = mouseX KR 0 0.5 Linear 0.1
                y = mouseY KR 0 0.5 Linear 0.1
            in  pv_RectComb chain 8 x y
    play $ out 0 $ mce2 ug ug

fft_ex08 :: IO ()
fft_ex08 =
    let isig = sinOsc AR (lfNoise1 'F' KR 5.2 * 250 + 400) 0
        osig = pv_withInput isig $ \chain ->
            pv_MagFreeze chain (sinOsc KR 0.2 0)
    in  audition $ out 0 $ mce2 osig osig

-- | Example of 'pv_MagFreeze' with 'playBuf' playing given file.
--
-- Trigger of freeze is controlled by 'dust', which the frequency of 'dust' is
-- controlled by 'mouaseX'.
--
fft_ex08b :: FilePath -> IO ()
fft_ex08b = pv_withFile_unary $ \chain ->
    pv_MagFreeze chain $
    toggleFF (dust 'a' KR (mouseX KR 1 8 Exponential 0.1))

-- | Stereo example.
fft_ex09 :: IO ()
fft_ex09 = audition $ out 0 osig
  where
    osig   = ifft' chain0 * 0.5
    chain0 = pv_MagFreeze chain1 (sinOsc KR (mce [0.2,0.3]) 0)
    chain1 = fft' bufs isig
    bufs   = mrg2 (localBuf 'a' 2048 2) (maxLocalBufs 2)
    isig   = sinOsc AR (lfNoise1 'd' KR (mce [5.2, 3.3]) * 250 + 400) 0

-- --------------------------------------------------------------------------
--
-- * PV_Add
--
-- --------------------------------------------------------------------------

pv_Add_ex01 :: FilePath -> IO ()
pv_Add_ex01 file = withSC3 $ do
    let osig   = pv_with2Inputs (p 1) (p 0.5) pv_Add
        p rate = playBuf 1 AR (constant bufn)
                 (bufRateScale KR (constant bufn) * rate) 1 0
                 Loop DoNothing
        bufn   = 10
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig

-- --------------------------------------------------------------------------
--
-- * PV_BinScramble
--
-- --------------------------------------------------------------------------

pv_BinScramble_ex01 :: FilePath -> IO ()
pv_BinScramble_ex01 = pv_withFile_unary $ \chain ->
    let wipe  = mouseX KR 0 1 Linear 0.1
        tr    = mouseButton KR 0 1 0.1
        width = mouseY KR 0 1 Linear 0.1
    in  pv_BinScramble 'S' chain wipe width tr


-- --------------------------------------------------------------------------
--
-- * PV_BinShift
--
-- --------------------------------------------------------------------------

pv_BinShift_ex01 :: IO ()
pv_BinShift_ex01 = withSC3 $ do
    let osig = pv_withInput isig $ \chain ->
            pv_BinShift chain (mouseX KR 0.25 4 Exponential 0.1) 0
        isig = lfSaw AR 200 0 * 0.2
    play $ out 0 $ mce2 osig osig

pv_BinShift_ex02 :: FilePath -> IO ()
pv_BinShift_ex02 = pv_withFile_unary $ \chain ->
    pv_BinShift chain (mouseX KR 0.25 4 Exponential 0.1) 0

pv_BinShift_ex03 :: IO ()
pv_BinShift_ex03 = withSC3 $ do
    let osig = pv_withInput isig $ \chain ->
            pv_BinShift chain 1 (mouseX KR (-128) 128 Linear 0.1)
        isig = lfSaw AR 200 0 * 0.2
    play $ out 0 $ mce2 osig osig

pv_BinShift_ex04 :: FilePath -> IO ()
pv_BinShift_ex04 = pv_withFile_unary $ \chain ->
    pv_BinShift chain 1 (mouseX KR (-128) 128 Linear 0.1)

-- --------------------------------------------------------------------------
--
-- * PV_BinWipe
--
-- --------------------------------------------------------------------------

pv_BinWipe_ex01 :: IO ()
pv_BinWipe_ex01 = withSC3 $ do
    let osig = pv_with2Inputs sigA sigB $ \a b ->
            pv_BinWipe a b $ mouseX KR (-1) 1 Linear 0.1
        sigA = whiteNoise 'W' AR * 0.2
        sigB = lfSaw AR 100 0 * 0.2
    play $ out 0 $ mce2 osig osig

pv_BinWipe_ex02 :: FilePath -> IO ()
pv_BinWipe_ex02 file = withSC3 $ do
    let osig = pv_with2Inputs sigA sigB $ \a b ->
            pv_BinWipe a b $ mouseX KR (-1) 1 Linear 0.1
        sigA = whiteNoise 'W' AR * 0.2
        sigB = p (constant bufn)
        p n  = playBuf 1 AR n (bufRateScale KR n) 1 0 Loop DoNothing
        bufn = 12
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_BrickWall
--
-- --------------------------------------------------------------------------

pv_BrickWall_ex01 :: IO ()
pv_BrickWall_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withWhiteNoise $ \chain ->
        pv_BrickWall chain $ sinOsc KR 0.1 0

pv_BrickWall_ex02 :: IO ()
pv_BrickWall_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = mix $ resonz atkd cf rq
    cf   = mce [ lfdNoise3 'C' KR 0.97 * 4000 + 6000
               , lfdNoise3 'D' KR 2.13 * 500 + 1000
               , lfdNoise3 'E' KR 1.82 * 200 + 400
               ]
    rq   = tChoose 'W' tr $ mce [1-e,e]
    e    = envGen KR tr 1 0 0.3 DoNothing $ envPerc 0.01 1
    atkd = decay tr 0.3 * brck * amp
    amp  = dbAmp $ tRand 'A' (-10) 0 tr
    tr   = dust 'D' KR 1 +
           impulse KR 3 0 +
           coinGate 'G' 0.25 (impulse KR 6 0)
    brck = pv_withWhiteNoise $ \chain ->
        pv_BrickWall chain $ lfdNoise1 'B' KR 3.25

--
-- XXX: Rewrite pvcalc, pvcalc2 with hsc3.
--

-- --------------------------------------------------------------------------
--
-- * pvcollect
--
-- --------------------------------------------------------------------------

pvcollect_ex01 :: FilePath -> IO ()
pvcollect_ex01 file = withSC3 $ do
    let fftSize :: Num a => a
        fftSize = 1024
    osig <- pv_withFileAndSize fftSize file 1 $ \ch ->
        let f mag phs _idx = (squared mag, squared phs)
        in  pvcollect ch fftSize f 0 255 1
    let osig' = osig * 0.01
    play $ out 0 $ mce2 osig' osig'


-- --------------------------------------------------------------------------
--
-- * PV_ConformalMap
--
-- --------------------------------------------------------------------------

pv_ConformalMap_ex01 :: IO ()
pv_ConformalMap_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withWhiteNoise $ \chain -> pv_ConformalMap chain mx my
    mx   = mouseX KR (-1) 1 Linear 0.1
    my   = mouseY KR (-1) 1 Linear 0.1

pv_ConformalMap_ex02 :: IO ()
pv_ConformalMap_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \chain -> pv_ConformalMap chain real imag
    real = mouseX KR 0.01 2 Exponential 0.1
    imag = mouseY KR 0.01 10 Exponential 0.1
    isig = mix $ lfSaw AR freq 0 * 0.3
    freq = sinOsc KR (mce [rand i 0.1 0.5|i<-"abc"]) 0 * 10 +
           mce [220*v|v<-[1,1.1,1.5,1.78,2.45,6.7,8]]

pv_ConformalMap_ex03 :: FilePath -> IO ()
pv_ConformalMap_ex03 = pv_withFile_unary $ \chain ->
    let m f = f KR (-1) 1 Linear 0.1
    in  pv_ConformalMap chain (m mouseX) (m mouseY)

pv_ConformalMap_ex04 :: IO ()
pv_ConformalMap_ex04 = audition $ out 0 $ mce2 osig osig
  where
    osig = combN cmap 0.1 0.1 10 * 0.5 + cmap
    cmap = pv_withInput isig $ \c -> pv_ConformalMap c real imag
    isig = mix (lfSaw AR freq 0) * 0.3
    freq = sinOsc KR (mce [0.16, 0.33, 0.41]) 0 * 10 + ofst
    ofst = mce [1, 1.1, 1.5, 1.78, 2.45, 6.7, 8] * 220
    real = mouseX KR 0.01 2 Linear 0.1
    imag = mouseY KR 0.01 10 Linear 0.1


-- --------------------------------------------------------------------------
--
-- * PV_Conj
--
-- --------------------------------------------------------------------------

-- | Complex conjugate.
pv_Conj :: UGen -> UGen
pv_Conj b = mkOsc KR "PV_Conj" [b] 1

pv_Conj_ex01 :: FilePath -> IO ()
pv_Conj_ex01 file = withSC3 $ do
    let bufn  = 12
        bufn' = constant bufn
        isig  = playBuf 1 AR bufn' (bufRateScale KR bufn') 1 0 Loop DoNothing
        sigr  = pv_withInput isig pv_Conj
        sigl  = isig
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 sigl sigr

pv_Conj_ex02 :: FilePath -> IO ()
pv_Conj_ex02 file = withSC3 $ do
    let osig = mix $ pv_with2Inputs isig isig $ \a b -> pv_Add a (pv_Conj b)
        isig = playBuf 1 AR bufn rate rst pos Loop DoNothing
        rate = bufRateScale KR bufn * rmal
        rmal = sqrt 2 ** tIRand 'r' (-2) 2 (coinGate 'r' 0.85 rst)
        rst  = dust 'd' KR 2 + impulse KR 2 0
        pos  = tRand 'p' 0 (bufFrames KR bufn) (coinGate '2' 0.5 rst)
        bufn :: Num a => a
        bufn = 12
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_Copy
--
-- --------------------------------------------------------------------------

gen_pv_Copy_ex01 :: FilePath -> UGen -> IO ()
gen_pv_Copy_ex01 file in1 = withSC3 $ do
    let bufn :: Num a => a
        bufn = 12
        in0  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
        ch0  = fft' (localBuf 'a' 2048 1) in0
        ch1  = fft' (localBuf 'b' 2048 1) in1
        ch2  = pv_Copy ch0 (localBuf 'c' 2048 1)
        ch3  = pv_MagMul ch1 ch2
        osig = fade (ifft' ch0) (ifft' ch3 * 0.1) (sinOsc KR 0.1 (pi * 1.5))
        fade a b c = let c' = c * 0.5 + 0.5 in (c'*a) + (1-c')*b
    _ <- async $ b_allocRead bufn file 0 0
    play $ mrg [out 0 $ mce2 osig osig, maxLocalBufs 3]

pv_Copy_ex01 :: FilePath -> IO ()
pv_Copy_ex01 file = gen_pv_Copy_ex01 file $ whiteNoise 'W' AR

pv_Copy_ex02 :: FilePath -> IO ()
pv_Copy_ex02 file = gen_pv_Copy_ex01 file $ blip AR 100 50

pv_Copy_ex03 :: FilePath -> IO ()
pv_Copy_ex03 file = withSC3 $ do
    let bufn :: Num a => a
        bufn = 13
        in0  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
        ch0  = fft' (localBuf 'a' 2048 1) in0
        ch1  = pv_Copy ch0 (localBuf 'b' 2048 1)
        pan  = mouseX KR 0.001 1.001 Exponential 0.1 - 0.001
        ch2  = pv_BrickWall ch0 pan
        ch3  = pv_BrickWall ch1 (pan-1)
        osig = mrg [mce [ifft' ch2, ifft' ch3 * 1000], maxLocalBufs 2]
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ osig

pv_Copy_ex04 :: IO ()
pv_Copy_ex04 = audition $ mrg [out 0 osig, maxLocalBufs 2]
  where
    osig = ifft' c1 - ifft' c0
    c0   = fft' buf0 in0
    c1   = pv_Copy c0 buf1
    buf0 = localBuf 'a' 2048 1
    buf1 = localBuf 'b' 2048 1
    in0  = lfClipNoise 'c' AR 100


-- --------------------------------------------------------------------------
--
-- * PV_CopyPhase
--
-- --------------------------------------------------------------------------

pv_CopyPhase_ex01 :: IO ()
pv_CopyPhase_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_with2Inputs inA inB pv_CopyPhase
    inA  = sinOsc AR frq0 0
    frq0 = sinOsc KR (frq1*frq1) 0 * 100 + 800
    frq1 = sinOsc KR 0.08 0 * 6 + 6.2
    inB  = whiteNoise 'W' AR * 0.2

pv_CopyPhase_ex02 :: FilePath -> IO ()
pv_CopyPhase_ex02 file = withSC3 $ do
    let osig = pv_with2Inputs inA inB pv_CopyPhase
        bufn :: Num a => a
        bufn = 13
        inA  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
        inB  = sinOsc AR frq0 0
        frq0 = sinOsc KR frq1 0 * 100 + 800
        frq1 = squared (sinOsc KR 0.08 0 * 6 + 6.2)
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_Diffuser
--
-- --------------------------------------------------------------------------

pv_Diffuser_ex01 :: IO ()
pv_Diffuser_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \chain -> pv_Diffuser chain tr
    isig = mix $ sinOsc AR frqs 0 * amps
    frqs = mce [200*x|x<-[1..10]]
    amps = mce [rand i 0.2 0.2|i<-[1..10::Int]]
    tr   = mouseY KR 0 1 Linear 0.1 >* 0.5

pv_Diffuser_ex02 :: FilePath -> IO ()
pv_Diffuser_ex02  =pv_withFile_unary $ \ch ->
    pv_Diffuser ch (mouseY KR 0 1 Linear 0.1 >* 0.5)

pv_Diffuser_ex03 :: FilePath -> IO ()
pv_Diffuser_ex03 = pv_withFile_unary $ \ch ->
    let frq = mouseX KR 0.251 25.001 Exponential 0.1 - 0.001
    in  pv_Diffuser ch $ lfPulse KR frq 0 0.5


-- --------------------------------------------------------------------------
--
-- * PV_Div
--
-- --------------------------------------------------------------------------

pv_Div :: UGen -> UGen -> UGen
pv_Div a b = mkOsc KR "PV_Div" [a,b] 1

pv_Div_ex01 :: IO ()
pv_Div_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_with2Inputs inA inB pv_Div * 0.1
    inA  = lpf inB (mouseX KR 100 1000 Linear 1)
    inB  = whiteNoise 'W' AR


-- --------------------------------------------------------------------------
--
-- * PV_HainsworthFoote
--
-- --------------------------------------------------------------------------

pv_HainsworthFoote_ex01 :: IO ()
pv_HainsworthFoote_ex01 = withSC3 $ do
    let osig = sinOsc AR (mrg2 440 445) 0 * decay (0.1*det) 0.2
        det  = pv_HainsworthFoote chain 1 0 1 0.04
        chain = fft' buf (soundIn 0)
        buf  = localBuf' 'a' 2048 1 1
    play $ out 0 osig

pv_HainsworthFoote_ex01' :: IO ()
pv_HainsworthFoote_ex01' =
    let i = soundIn 0
        b = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 1)
        f = fft' b i
        x = mouseX KR 0.5 1.25 Linear 0.2
        h = pv_HainsworthFoote f 1 0 x 0.01
        o = sinOsc AR (mrg2 440 445) 0 * decay (h * 0.1) 0.1
    in  audition $ out 0 (o+i)


-- --------------------------------------------------------------------------
--
-- * PV_JensenAndersen
--
-- --------------------------------------------------------------------------

pv_JensenAndersen_ex01 :: IO ()
pv_JensenAndersen_ex01 = withSC3 $ do
    let osig  = sinOsc AR (mce [440,445]) 0 * decay (0.1*det) 0.1
        det   = pv_JensenAndersen chain 0.25 0.25 0.25 0.25 thres 0.04
        chain = fft' buf (soundIn 0)
        buf   = localBuf' 'a' 2048 1 1
        thres = mouseX KR 0.1 1 Linear 0.1
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_LocalMax
--
-- --------------------------------------------------------------------------

pv_LocalMax_ex01 :: IO ()
pv_LocalMax_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig  = pv_withInput isig $ \chain -> pv_LocalMax chain thres
    thres = mouseX KR 0 50 Linear 0.1
    isig  = mix (lfSaw AR frqs 0 * 0.1)
    frqs  = mce [expRand i 100 500 | i <- "abc"]

pv_LocalMax_ex02 :: FilePath -> IO ()
pv_LocalMax_ex02 = pv_withFile_unary $ \chain ->
    let thres = mouseX KR 0 50 Linear 0.1
    in  pv_LocalMax chain thres


-- --------------------------------------------------------------------------
--
-- * PV_MagAbove
--
-- --------------------------------------------------------------------------

pv_MagAbove_ex01 :: IO ()
pv_MagAbove_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \chain -> pv_MagAbove chain 310
    isig = sinOsc AR frq0 0
    frq0 = sinOsc KR frq1 0 * 100 + 800
    frq1 = squared $ sinOsc KR 0.08 0 * 6 + 6.2

pv_MagAbove_ex02 :: IO ()
pv_MagAbove_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig  = pv_withInput isig $ \chain -> pv_MagAbove chain thres
    thres = mouseX KR 0 10 Linear 0.1
    isig  = whiteNoise 'W' AR * 0.2

pv_MagAbove_ex03 :: FilePath -> IO ()
pv_MagAbove_ex03 = pv_withFile_unary $ \chain ->
    let thres = mouseX KR 0 310 Linear 0.1
    in  pv_MagAbove chain thres

pv_MagAbove_ex04 :: FilePath -> IO ()
pv_MagAbove_ex04 = pv_withFile_unary $ \chain ->
    let thrs = lfdNoise3 't' KR frq * mag + mag
        frq  = mouseX KR 0.001 25.001 Exponential 0.1 - 0.001
        mag  = mouseY KR 10 120 Linear 0.1
    in  pv_MagAbove chain thrs

-- --------------------------------------------------------------------------
--
-- * PV_MagBelow
--
-- --------------------------------------------------------------------------

pv_MagBelow_ex01 :: IO ()
pv_MagBelow_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \chain -> pv_MagBelow chain 310
    isig = sinOsc AR frq0 0
    frq0 = sinOsc KR frq1 0 * 100 + 800
    frq1 = squared $ sinOsc KR 0.08 0 * 6 + 6.2

pv_MagBelow_ex02 :: IO ()
pv_MagBelow_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig  = limiter (blw*mul) 1 0.01
    mul   = mouseY KR 1 1000 Linear 0.1
    blw   = pv_withInput isig $ \chain -> pv_MagBelow chain thres
    thres = mouseX KR 0 10 Linear 0.1
    isig  = whiteNoise 'W' AR * 0.2

pv_MagBelow_ex03 :: FilePath -> IO ()
pv_MagBelow_ex03 file = withSC3 $ do
    osig <- pv_withFile file 1 $ \chain ->
        let thres = mouseX KR 0 310 Linear 0.1
        in  pv_MagBelow chain thres
    let osig' = limiter (osig*1000) 1 0.01 * 0.3
    play $ out 0 $ mce2 osig' osig'

pv_MagBelow_ex04 :: FilePath -> IO ()
pv_MagBelow_ex04 file = withSC3 $ do
    osig <- pv_withFile file 1 $ \chain ->
        let thres = lfdNoise3 't' KR frq * mag + mag
            mag   = mouseY KR 10 120 Linear 0.1
            frq   = mouseX KR 0.001 25.001 Exponential 0.1 - 0.001
        in  pv_MagBelow chain thres
    let osig' = limiter (osig*200) 1 0.1
    play $ out 0 $ mce2 osig' osig'


-- --------------------------------------------------------------------------
--
-- * PV_MagClip
--
-- --------------------------------------------------------------------------

pv_MagClip_ex01 :: IO ()
pv_MagClip_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \chain -> pv_MagClip chain thrs
    isig = mix $ lfSaw AR frqs 0 * 0.1
    frqs = mce [expRand i 100 500 | i <- [0,1,2::Int]]
    thrs = mouseX KR 0 15 Linear 0.1

pv_MagClip_ex02 :: FilePath -> IO ()
pv_MagClip_ex02 = pv_withFile_unary $ \chain ->
    let thrs = mouseX KR 0 50 Linear 0.1
    in  pv_MagClip chain thrs


-- --------------------------------------------------------------------------
--
-- * PV_MagDiv
--
-- --------------------------------------------------------------------------

pv_MagDiv :: UGen -> UGen -> UGen -> UGen
pv_MagDiv a b z = mkOsc KR "PV_MagDiv" [a,b,z] 1

pv_MagDiv_ex01 :: IO ()
pv_MagDiv_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = 0.1 * (pv_with2Inputs inA inB $ \a b -> pv_MagDiv a b z)
    inA  = whiteNoise 'W' AR * 0.2
    inB  = lfSaw AR 100 0 * 0.2
    z    = 0.001

pv_MagDiv_ex02 :: FilePath -> IO ()
pv_MagDiv_ex02 file = withSC3 $ do
    let bufn :: Num a => a
        bufn = 12
        osig = divd * 0.1
        divd = pv_with2Inputs inA inB $ \a b -> pv_MagDiv a b z
        inA  = mix $ lfSaw AR (mce [100,150]) 0 * 0.2
        inB  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
        z    = 0.001
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig

pv_MagDiv_ex03 :: FilePath -> FilePath -> IO ()
pv_MagDiv_ex03 fileA fileB = withSC3 $ do
    divd <- pv_with2Files fileA fileB $ \a b -> pv_MagDiv a b 0.001
    let osig = limiter (hpf divd 20) 1 0.1
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_MagFreeze
--
-- --------------------------------------------------------------------------

pv_MagFreeze_ex01 :: IO ()
pv_MagFreeze_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = 0.1 * (pv_withInput isig $ \chain -> pv_MagFreeze chain tr)
    isig = sinOsc AR (lfNoise1 'F' KR 5 * 250 + 400) 0
    tr   = sinOsc KR 0.2 0

pv_MagFreeze_ex02 :: FilePath -> IO ()
pv_MagFreeze_ex02 file = withSC3 $ do
    osig <- pv_withFileAndSize 4096 file 1 $ \chain ->
        let tr = mouseButton KR 0 1 0.01
        in  pv_MagFreeze chain tr
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_MagMul
--
-- --------------------------------------------------------------------------

pv_MagMul_ex01 :: IO ()
pv_MagMul_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_with2Inputs inA inB pv_MagMul * 0.3
    inA  = whiteNoise 'W' AR * 0.2
    inB  = mix $ lfSaw AR (mce [1, 2, 3] * f0) 0 * 0.2
    f0   = mouseX KR 25 1000 Exponential 0.1

pv_MagMul_ex02 :: FilePath -> IO ()
pv_MagMul_ex02 file = withSC3 $ do
    let bufn :: Num a => a
        bufn = 12
        osig = pv_with2Inputs inA inB pv_MagMul * 0.03
        inA  = mix $ lfSaw AR (mce [100, 150]) 0 * 0.2
        inB  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig

-- | Try:
--
-- >>> pv_MagMul_ex03 bsd_mono
--
pv_MagMul_ex03 :: FilePath -> IO ()
pv_MagMul_ex03 file = withSC3 $ do
    let -- osig = combC muld 0.3 0.3 6
        osig = muld
        muld = 0.1 * pv_with2Inputs inA inB pv_MagMul * 0.3
        inA  = mix $ lfSaw AR frqs 0 * 0.1
        frqs = mce [midiCPS (tChoose i tr (mce pchs))|i<-"abcdefg"]
        pchs = foldr (\o acc -> map ((+ofst) . (+o)) degs ++ acc) [] octs
        degs = [0,thrd,7,10]
        thrd = tIRand '3' 3 4 (coinGate '#' (1/15) tr)
        octs = take 5 $ iterate (+12) 33
        ofst = tIRand 'O' (-6) 6 (coinGate 'g' (1/31) tr)
        tr   = dust 'D' KR trf
        trf  = mouseX KR (1/8) 8 Exponential 0.1
        inB  = playBuf 1 AR bufn (bufRateScale KR bufn * rt) 1 0 Loop DoNothing
        rt   = mouseY KR 0.25 4 Exponential 0.1
        bufn :: Num a => a
        bufn = 12
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_MagNoise
--
-- --------------------------------------------------------------------------

pv_MagNoise_ex01 :: IO ()
pv_MagNoise_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig pv_MagNoise
    isig = sinOsc AR frq0 0
    frq0 = sinOsc KR frq1 0 * 100 + 800
    frq1 = squared $ sinOsc KR 0.08 0 * 6 + 6.2

pv_MagNoise_ex02 :: FilePath -> IO ()
pv_MagNoise_ex02 = pv_withFile_unary pv_MagNoise


-- --------------------------------------------------------------------------
--
-- * PV_MagShift
--
-- --------------------------------------------------------------------------

pv_MagShift_ex01 :: IO ()
pv_MagShift_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \ch -> pv_MagShift ch strt shft
    isig = lfSaw AR 200 0 * 0.2
    strt = mouseX KR 0.25 4 Exponential 0.1
    shft = 0

-- | Chainging pitch with keeping speed.
pv_MagShift_ex02 :: FilePath -> IO ()
pv_MagShift_ex02 = pv_withFile_unary $ \ch ->
    let strt = mouseX KR 0.25 4 Exponential 0.1
    in  pv_MagShift ch strt 0

-- | Changings speed with keeping pitch.
pv_MagShift_ex02b :: FilePath -> IO ()
pv_MagShift_ex02b file = withSC3 $ do
    let strt = mouseX KR 0.25 4 Exponential 0.1
    osig <- pv_withFile file (recip strt) $ \chain -> pv_MagShift chain strt 0
    play $ out 0 $ mce2 osig osig

pv_MagShift_ex03 :: IO ()
pv_MagShift_ex03 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \ch -> pv_MagShift ch 1 shft
    isig = lfSaw AR 200 0 * 0.2
    shft = mouseX KR (-128) 128 Linear 0.1

pv_MagShift_ex04 :: FilePath -> IO ()
pv_MagShift_ex04 file = withSC3 $ do
    let shft = mouseX KR (-256) 256 Linear 0.1
    osig <- pv_withFile file 1 $ \ch -> pv_MagShift ch 1 shft
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_MagSmear
--
-- --------------------------------------------------------------------------

pv_MagSmear_ex01 :: IO ()
pv_MagSmear_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \chain -> pv_MagSmear chain bins
    isig = lfSaw AR 500 0 * decay2 tr 0.01 2 * 0.2
    tr   = impulse KR 2 0
    bins = mouseX KR 0 512 Linear 0.1

pv_MagSmear_ex02 :: FilePath -> IO ()
pv_MagSmear_ex02 = pv_withFile_unary $ \chain ->
    pv_MagSmear chain $ mouseX KR 0 512 Linear 0.1


-- --------------------------------------------------------------------------
--
-- * PV_MagSquared
--
-- --------------------------------------------------------------------------

pv_MagSquared_ex01 :: FilePath -> IO ()
pv_MagSquared_ex01 file = withSC3 $ do
    sqrd <- pv_withFile file 1 pv_MagSquared
    let osig = sqrd * 0.003
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_Max
--
-- --------------------------------------------------------------------------

pv_Max_ex01 :: FilePath -> FilePath -> IO ()
pv_Max_ex01 fileA fileB = withSC3 $ do
    osig <- pv_with2Files fileA fileB pv_Max
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_Min
--
-- --------------------------------------------------------------------------

pv_Min_ex01 :: FilePath -> FilePath -> IO ()
pv_Min_ex01 fileA fileB = withSC3 $ do
    osig <- pv_with2Files fileA fileB pv_Min
    play . out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_Mul
--
-- --------------------------------------------------------------------------

pv_Mul_ex01 :: IO ()
pv_Mul_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_with2Inputs inA inB pv_Mul * 0.1
    inA  = sinOsc AR 500 0 * 0.5
    inB  = sinOsc AR (line KR 100 400 5 DoNothing) 0 * 0.5

pv_Mul_ex02 :: IO ()
pv_Mul_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_with2Inputs inA inB pv_Mul * 0.1
    inA  = sinOsc AR 500 0 * 0.5 * line KR 0 1 10 DoNothing
    inB  = lfNoise1 'L' AR 20

pv_Mul_ex03 :: FilePath -> FilePath -> IO ()
pv_Mul_ex03 fileA fileB = withSC3 $ do
    osig <- (*0.1) <$> pv_with2Files fileA fileB pv_Mul
    play $ out 0 $ mce2 osig osig

pv_Mul_ex04 :: FilePath -> IO ()
pv_Mul_ex04 file = withSC3 $ do
    let bufn :: Num a => a
        bufn = 13
        inA  = playBuf 1 AR bufn (bufRateScale KR bufn * rt) 1 0 Loop DoNothing
        rt   = mouseY KR 0.25 4 Exponential 0.1
        inB  = mix $ lfSaw AR frqs 0 * 0.2
        frqs = mce [midiCPS (tChoose i tr (mce pchs))|i<-"abcdefg"]
        pchs = foldr (\o acc -> map ((+ofst) . (+o)) degs ++ acc) [] octs
        degs = [0,thrd,7,10]
        thrd = tIRand '3' 3 4 (coinGate '#' (1/15) tr)
        octs = take 5 $ iterate (+12) 33
        ofst = tIRand 'O' (-6) 6 (coinGate 'g' (1/31) tr)
        tr   = dust 'D' KR trf
        trf  = mouseX KR (1/8) 8 Exponential 0.1
        muld = pv_with2Inputs inA inB pv_Mul * 0.025
        osig = combC muld 0.3 0.3 6
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_PhaseShift
--
-- --------------------------------------------------------------------------

pv_PhaseShift_ex01 :: IO ()
pv_PhaseShift_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput sigA $ \ch -> pv_PhaseShift ch shft
    shft = lfNoise2 's' KR  1 * 180 + 180
    sigA = sinOsc AR 500 0

pv_PhaseShift270_ex01 :: FilePath -> IO ()
pv_PhaseShift270_ex01 file = withSC3 $ do
    osig0 <- pv_withFile file 1 pv_PhaseShift270
    let bufn :: Num a => a
        bufn = 21
        osig1 = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig0 osig1

pv_PhaseShift90_ex01 :: IO ()
pv_PhaseShift90_ex01 = audition $ out 0 $ mce2 osig0 osig1
  where
    osig0  = ifft' chain0
    osig1  = ifft' $ pv_PhaseShift90 chain0
    chain0 = fft' buf inA
    buf    = localBuf' 'a' 2048 1 1
    inA    = whiteNoise 'A' AR * 0.2


-- --------------------------------------------------------------------------
--
-- * PV_RandComb
--
-- --------------------------------------------------------------------------

pv_RandComb_ex01 :: IO ()
pv_RandComb_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withInput isig $ \ch -> pv_RandComb 'a' ch 0.95 tr
    isig = whiteNoise 'W' AR
    tr   = impulse KR 0.4 0

pv_RandComb_ex02 :: FilePath -> IO ()
pv_RandComb_ex02 file = withSC3 $ do
    let wipe = mouseY KR 0 1 Linear 0.1
        tr   = impulse KR trt 0
        trt  = mouseX KR 0.1 25 Exponential 0.1
    osig <- pv_withFile file 1 $ \ch -> pv_RandComb 'r' ch wipe tr
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PV_RandWipe
--
-- --------------------------------------------------------------------------

pv_RandWipe_ex01 :: IO ()
pv_RandWipe_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_with2Inputs inA inB (\a b -> pv_RandWipe 'p' a b wipe tr) * 0.5
    inA  = mix $ lfSaw AR (mce [expRand i 400 1000|i<-"abcdef"]) 0 * 0.1
    inB  = mix $ lfPulse AR (mce [expRand i 80 400|i<-"ghijkl"]) 0 wdt
    wdt  = abs (sinOsc KR (rand 'R' 0 0.8) 0 * 0.2)
    wipe = mouseX KR 0 1 Linear 0.1
    tr   = mouseY KR 0 1 Linear 0.1 >* 0.5

pv_RandWipe_ex02 :: FilePath -> IO ()
pv_RandWipe_ex02 file = withSC3 $ do
    let osig = rwpd * 0.2
        rwpd = pv_with2Inputs inA inB $ \a b -> pv_RandWipe 'p' a b wipe tr
        wipe = mouseX KR 0 1 Linear 0.1
        tr   = mouseY KR 0 1 Linear 0.1 >* 0.5
        inA  = mix $ lfSaw AR (mce [expRand i 400 1000|i<-"abcdef"]) 0 * 0.1
        inB  = playBuf 1 AR bufn (bufRateScale KR bufn) 1 0 Loop DoNothing
        bufn :: Num a => a
        bufn = 13
    _ <- async $ b_allocRead bufn file 0 0
    play $ out 0 $ mce2 osig osig

pv_RandWipe_ex03 :: FilePath -> FilePath -> IO ()
pv_RandWipe_ex03 fileA fileB = withSC3 $ do
    let wipe = sinOsc KR (mouseX KR 0 20 Linear 0.1) 0 * 0.5 + 0.5
        tr   = impulse KR (mouseY KR 0 20 Linear 0.1) 0
    osig <- (*0.5) <$>
            pv_with2Files fileA fileB (\a b -> pv_RandWipe 'R' a b wipe tr)
    play $ out 0 $ mce2 osig osig

-- --------------------------------------------------------------------------
--
-- * PV_RectComb
--
-- --------------------------------------------------------------------------

pv_RectComb_ex01 :: IO ()
pv_RectComb_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withWhiteNoise $ \ch -> pv_RectComb ch ntth phs wdth
    ntth = 8
    phs  = lfTri KR 0.097 0 * 0.4 + 0.5
    wdth = lfTri KR 0.24 0 * (-0.5) + 0.5

pv_RectComb_ex02 :: IO ()
pv_RectComb_ex02 = audition $ out 0 $ mce2 osig osig
  where
    osig = pv_withWhiteNoise $ \ch -> pv_RectComb ch ntth phs wdth
    ntth = mouseX KR 0 32 Linear 0.1
    phs  = mouseY KR 0 1 Linear 0.1
    wdth = 0.2

pv_RectComb2_ex01 :: FilePath -> FilePath -> IO ()
pv_RectComb2_ex01 fileA fileB = withSC3 $ do
    osig <- pv_with2Files fileA fileB $ \a b ->
        let ntth = mouseX KR 0 32 Linear 0.1
            phs  = mouseY KR 0 1 Linear 0.1
            wdth = 0.3
        in  pv_RectComb2 a b ntth phs wdth
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * PackFFT
--
-- --------------------------------------------------------------------------

-- | Example showing how to write fft data manually with 'packFFT'.
packFFT_ex01 :: IO ()
packFFT_ex01 = withSC3 $ do
    let bufn   = 10
        bufsz  = 512
        n      = 100
        sq x   = x * x
        r1     = linLin (fSinOsc KR (expRand 'a' 0.1 1) 0) (-1) 1 0 1
        mag1   = uclone' 'a' n r1
        mag2   = zipWith (*) mag1 $ map sq [1.0,0.99..]
        r2     = lfPulse KR (2 ** iRand 'a' (-3) 5) 0 0.3
        mag3   = zipWith (*) mag2 $ uclone' 'a' n r2
        phs    = replicate n 0
        chain1 = fft' (constant bufn) (fSinOsc AR 440 0)
        chain2 = packFFT chain1 (constant bufsz) 0 (constant n - 1) 1
                 (packFFTSpec mag3 phs)
        s      = ifft' chain2 * 10
    _ <- async $ b_alloc bufn bufsz 1
    play $ out 0 $ mce [s,s]


-- --------------------------------------------------------------------------
--
-- * PartConv
--
-- --------------------------------------------------------------------------

partConv_ex01 :: IO ()
partConv_ex01 = withSC3 $ do
    gen <- liftIO $ newStdGen
    let vals = go gen 0 [(0,1)]
        go g0 i acc
            | i == irSize = acc
            | otherwise   =
                let (v1,g1) = randomR (0,1::Double) g0
                    (v2,g2) = randomR (0,1::Double) g1
                    i'      = fromIntegral i
                    v2'     = v2**8*((irSize-i')/irSize)
                    (acc',g0')
                        | v1 >= 0.95 = ((i+1,v2'):acc,g2)
                        | otherwise  = (acc,g1)
                in  go g0' (i+1) acc'

        sendvals vs = do
            let (mine,others) = splitAt 2048 vs
            send $ b_set buftd mine
            case others of
                [] -> return ()
                _  -> sendvals others

        fftSize = 2048
        irSize  :: Num a => a
        irSize  = 96000
        accSize = pc_calcAccumSize fftSize irSize
        buftd   = 11
        buffd   = 12

        isig    = sum $ map fsig [0..npoly-1::Int]
        npoly   = 8
        tr0     = dust 'D' KR (mouseX KR 1.001 20.001 Exponential 0.1 - 0.001)
        pchs    = foldr (\x xs -> map (+x) degs ++ xs) [] octs
        octs    = take 9 $ iterate (+12) 24
        degs    = [0,2,3,7,10]
        c       = constant
        fsig i  =
            let tr = pulseDivider tr0 (c npoly) (c i)
                -- fq = tExpRand i 100 8000 tr
                fq = midiCPS (tChoose i tr (mce pchs))
                ev = envGen KR tr 1 0 0.5 DoNothing (envPerc 0.001 1)
            in  sinOsc AR fq 0 * ev
        osig    = partConv isig (c fftSize) (c buffd) * 0.01

    sequence_ [ async $ b_alloc buftd irSize 1
              , async $ b_alloc buffd accSize 1 ]
    sendvals vals
    send $ pc_preparePartConv buffd buftd fftSize
    play $ out 0 $ mce2 osig osig


-- --------------------------------------------------------------------------
--
-- * SpecCentroid
--
-- --------------------------------------------------------------------------

specCentroid_ex01 :: IO ()
specCentroid_ex01 = audition osig
  where
    osig   = mrg [ out 0 $ mce2 isig isig
                 , poll (impulse KR 1 0) cntr (label "centroid") 0
                 , maxLocalBufs 1
                 ]
    isig   = blip AR frq (mouseX KR 1 100 Exponential 0.1)
    frq    = mouseY KR 1000 100 Exponential 0.1
    cntr   = specCentroid $ fft' (localBuf 'a' 2048 1) isig


-- --------------------------------------------------------------------------
--
-- * SpecFlatness
--
-- --------------------------------------------------------------------------

specFlatness_ex01 :: IO ()
specFlatness_ex01 =
    let i = (f * whiteNoise 'W' AR + (1-f) * sinOsc AR 440 0) * 0.1
        f = mouseX KR 0 1 Linear 0.1
        s = specFlatness $ fft' (localBuf 'a' 2048 1) i
        o = mrg [ out 0 $ mce2 i i
                , poll (impulse KR 1 0) s (label "flatness") 0
                , maxLocalBufs 1
                ]
    in  audition o


-- --------------------------------------------------------------------------
--
-- * SpecPcile
--
-- --------------------------------------------------------------------------

specPcile_ex01 :: IO ()
specPcile_ex01 =
    let i  = lpf (whiteNoise 'w' AR) rc
        rc = mouseX KR 0.1 22050 Exponential 0.1
        ec = lag (specPcile ch 0.9 1) 1
        ch = fft' (localBuf 'a' 2048 1) i
        t  = impulse KR 1 0
        o  = mrg [ out 0 $ mce2 i i
                 , poll t rc (label "real") 0
                 , poll t ec (label "estimated") 0
                 , maxLocalBufs 1
                 ]
    in  audition o


-- --------------------------------------------------------------------------
--
-- * UnpackFFT
--
-- --------------------------------------------------------------------------

unpack1FFT_ex01 :: FilePath -> IO ()
unpack1FFT_ex01 file = withSC3 $ do
    let b :: Num a => a
        b = 10
        i = playBuf 1 AR b (bufRateScale KR b) 1 0 Loop DoNothing
        c = fft' (localBuf 'a' 2048 1) i
        u = unpack1FFT c (bufFrames KR b) 0 0
        d = demand (c >=* 0) 0 u
        o = mrg [ out 0 $ mce $ map (*0.1) [i,i]
                , poll (c >=* 0) d (label "unpack") 0
                , maxLocalBufs 1
                ]
    _ <- async $ b_allocRead b file 0 0
    play o

-- unpackFFT_ex01 ::


-- --------------------------------------------------------------------------
--
-- * LocalBuf
--
-- --------------------------------------------------------------------------

localBuf_ex02 :: IO ()
localBuf_ex02 = audition $ out 0 osig
  where
    osig = ifft' c * 0.1
    c    = pv_BrickWall f (sinOsc KR (mce2 0.1 0.11) 0 * 0.75)
    f    = fft' b n
    b    = mrg2 (uclone 'a' 2 (localBuf 'a' 2048 1)) m
    m    = maxLocalBufs 2
    n    = uclone 'a' 2 (whiteNoise 'a' AR)


-- --------------------------------------------------------------------------
--
-- * Analysis ugens
--
-- --------------------------------------------------------------------------

-- | Use input amplitude to control 'pulse' amplitude - use headphones to prevent
-- feedback.
amplitude_ex01 :: IO ()
amplitude_ex01 =
    let s = in' 1 AR numOutputBuses
        a = amplitude KR s 0.1 0.1
    in  audition $ out 0 (pulse AR 90 0.3 * a)

-- | Use input amplitude to control 'sinOsc' frequency - use headphones to
-- prevent feedback.
amplitude_ex02 :: IO ()
amplitude_ex02 =
    let s = in' 1 AR numOutputBuses
        f = amplitude KR s 0.1 0.1 * 1200 + 400
    in  audition $ out 0 (sinOsc AR f 0 * 0.3)

-- | Audible example of 'slope'.
slope_ex01 :: IO ()
slope_ex01 =
    let a = lfNoise2 'a' KR 2
        s = recip 2
        b = slope a * s
        c = slope b * squared s
        f = mce [a, b, c] * 2200 + 2200
        o = sinOsc AR f 0 * 0.3
    in  audition $ out 0 (mix o)

-- | Example of takng RMS power of 'lfSaw' with 'runningSum'.
runningSum_ex01 :: IO ()
runningSum_ex01 =
    let i = lfSaw AR 440 0
        n = 30
        r = sqrt (runningSum (squared i) n / n)
    in  audition $ out 0 r

-- | Audible example of 'zeroCrossing'.
zeroCrossing_ex01 :: IO ()
zeroCrossing_ex01 =
    let a = sinOsc AR f 0 * 0.1
        f = sinOsc KR 1 0 * 600 + 700
        z = zeroCrossing a
        i = impulse AR z 0 * 0.25
    in  audition $ out 0 $ mce [a, i]

-- | Source input for 'compander' examples.
compander_src :: UGen
compander_src =
    let e = decay2 (impulse AR 8 0 * lfSaw KR 0.3 0 * 0.3) 0.001 0.3
        p = mix (pulse AR (mce [80,81]) 0.3)
    in  e * p

-- | Play 'comapander_src'.
play_compander_src :: IO ()
play_compander_src = audition $ out 0 compander_src

-- | Play 'compander_src' with 'compander'
play_compander_ex :: UGen -> UGen -> UGen -> UGen -> IO ()
play_compander_ex slopeBelow slopeAbove clampTime relaxTime =
    let x = mouseX KR 0.01 1 Linear 0.1
        z = compander_src
        c = compander z z x slopeBelow slopeAbove clampTime relaxTime
    in  audition $ out 0 $ mce [z, c]

-- | Noise gate example of 'compander'.
compander_noise_gate :: IO ()
compander_noise_gate = play_compander_ex 10 1 0.01 0.01

-- | Compressor example of 'compander'.
compander_compressor :: IO ()
compander_compressor = play_compander_ex 1 0.5 0.01 0.01

-- | Limiter example of 'compander'.
compander_limiter :: IO ()
compander_limiter = play_compander_ex 1 0.1 0.01 0.01

-- | Sustainer example of 'companer'
compander_sustainer :: IO ()
compander_sustainer = play_compander_ex 0.1 1 0.01 0.01


-- --------------------------------------------------------------------------
--
-- * Demand
--
-- --------------------------------------------------------------------------

-- | Example from 'dstutter' hsc3 help.
dstutter_ex01 :: IO ()
dstutter_ex01 =
    let inp = dseq 'a' dinf $ mce [1,2,3]
        nse = diwhite 'a' dinf 2 8
        rep = dstutter 'a' nse inp
        trg = impulse KR (mouseX KR 1 40 Exponential 0.2) 0
        frq = demand trg 0 rep * 30 + 340
    in  audition $ out 0 $ sinOsc AR frq 0 * 0.1

-- | Another example from 'dstutter' hsc3 help.
dstutter_ex02 :: IO ()
dstutter_ex02 =
    let {a z = let {xr = dxrand z dinf (mce [0.1,0.2,0.3,0.4,0.5])
                   ;lf = dstutter z 2 xr
                   ;du = duty AR lf 0 DoNothing lf
                   ;tr = abs (hpz1 du) >* 0
                   ;ph = sweep tr (1/du)}
               in linExp ph 0 1 (rand z 50 100) (rand z 500 2000)
        ;f = mce (map a ['a'..'h'])
        ;[s0,s1] = mceChannels (splay (sinOsc AR f 0) 1 1 0 True)
        ;o = limiter (rotate2 s0 s1 (lfSaw KR 0.1 0)) 1 1e-2}
    in audition (out 0 (o * 0.25))

-- | Another example from SC3 help file of PStutter.
dstutter_ex03 :: IO ()
dstutter_ex03 =
    let f = dstutter 'a' r p
        r = diwhite 'a' dinf 2 8
        p = dxrand 'a' dinf $ mce [midiCPS (x*7+60)|x<-[1,2,3]]
        e = envGen KR t 0.3 0 1 DoNothing $ envPerc 0.01 0.2
        t = impulse KR (recip 0.12) 0
        o = sinOsc AR (demand t 0 f) 0 * e
    in  audition $ out 0 o


-- --------------------------------------------------------------------------
--
-- * Other
--
-- --------------------------------------------------------------------------

-- | Example of manual fading to get similar effect done in 'xFade2'.
xFade_ex01 :: IO ()
xFade_ex01 = audition $ out 0 $ mce2 osig osig
  where
    osig = fade * sig0 + (1 - fade) * sig1
    fade = lfTri KR (1/20) 1 * 0.5 + 0.5
    sig0 = resonz (whiteNoise 'W' AR) 8000 0.2 * 0.5
    sig1 = saw AR 440


-- --------------------------------------------------------------------------
--
-- * Auxiliary functions
--
-- --------------------------------------------------------------------------

-- | File path to @sharp_mono.wav@.
sharp_mono :: FilePath
sharp_mono = wavFile "sharp_mono.wav"

-- | File path to @bsd_mono.wav@.
bsd_mono :: FilePath
bsd_mono = wavFile "bsd_mono.wav"

-- | File path to @fp_10_to_40.wav@.
fp_10_to_40 :: FilePath
fp_10_to_40 = wavFile "fp_10_to_40.wav"

-- | File path to @ted_intro_mono.wav@.
ted_intro_mono :: FilePath
ted_intro_mono = wavFile "ted_intro_mono.wav"

-- | File path to @rakista_mono.wav"
rakista_mono :: FilePath
rakista_mono = wavFile "rakista_mono.wav"

a11wlk01 :: FilePath
a11wlk01 = wavFile "a11wlk01.wav"

just_adel :: FilePath
just_adel = wavFile "just_adel.wav"

break_339 :: FilePath
break_339 = wavFile "break_339.wav"

-- | Directory containing wave files.
wavFile :: FilePath -> FilePath
wavFile file = "/home/atsuro/sound/wav/" ++ file

-- | Returns 'MRG' UGen with maxLocalBufs.
localBuf' ::
    ID i
    => i    -- ^ ID passed to 'localBuf'.
    -> UGen -- ^ FFT size.
    -> UGen -- ^ Number of channels in 'localBuf'.
    -> UGen -- ^ Number of max local buffer, passed to 'maxLocalBufs'.
    -> UGen
localBuf' i size nchan maxNum =
    mrg2 (localBuf i size nchan) (maxLocalBufs maxNum)

-- | Apply given pv function to input signal. Contains 'MRG' of 1
-- 'maxLocalBufs'.
pv_withInput ::
    UGen              -- ^ Input signal.
    -> (UGen -> UGen) -- ^ Function taking fft chain.
    -> UGen
pv_withInput isig fpv = sig0
  where
    sig0   = ifft' chain0 * 0.5
    chain0 = fpv chain1
    chain1 = fft' buf isig
    buf    = localBuf' 'a' 2048 1 1

pv_with2Inputs :: UGen -> UGen -> (UGen -> UGen -> UGen) -> UGen
pv_with2Inputs sigA sigB fpv = osig
  where
    osig = ifft' $ fpv chainA chainB
    chainA = f 'x' sigA
    chainB = f 'y' sigB
    f i sig = fft' (mrg2 (localBuf i 2048 1) (maxLocalBufs 2)) sig

-- | Play 'pv_withInput' with 'whiteNoise'.
pv_withWhiteNoise ::
    (UGen -> UGen) -- ^ FUnction taking fft chain of 'whiteNoise'.
    -> UGen
pv_withWhiteNoise = pv_withInput (whiteNoise 'W' AR)

-- | Returns UGen with applying given function to 'playBuf'.
pv_withFile ::
    Transport m
    => FilePath       -- ^ File to play with 'playBuf'.
    -> UGen           -- ^ Rate scale of 'playBuf': 1 = original rate.
    -> (UGen -> UGen) -- ^  Function applied in fft chain.
    -> m UGen
pv_withFile = pv_withFileAndSize 2048

pv_withFileAndSize ::
    Transport m
    => Int            -- ^ FFT size, must be power of 2.
    -> FilePath       -- ^ File to play with 'playBuf'.
    -> UGen           -- ^ Ratescale of 'playBuf': 1 = original rate.
    -> (UGen -> UGen) -- ^ Function applied in fft chain.
    -> m UGen
pv_withFileAndSize size file rate fpv = do
    let bufn :: Num a => a
        bufn = 10
        osig = ifft' $ fpv $ fft' lbuf isig
        lbuf = mrg2 (localBuf 'z' (constant size) 1) (maxLocalBufs 1)
        isig = playBuf 1 AR bufn (bufRateScale KR bufn * rate) 1 0
               Loop DoNothing
    _ <- async $ b_allocRead bufn file 0 0
    return osig

-- | Apply given unary function to fft chain of 'playBuf'.
pv_withFile_unary :: (UGen -> UGen) -> FilePath -> IO ()
pv_withFile_unary funary file = withSC3 $ do
    osig <- pv_withFile file 1 funary
    play $ out 0 $ mce2 osig osig

-- | Performs given fft chain function with two files played with 'playBuf'.
pv_with2Files ::
    Transport m
    => FilePath -- ^ File A.
    -> FilePath -- ^ File B.
    -> (UGen -> UGen -> UGen) -- ^ Function in fft chain.
    -> m UGen
pv_with2Files fileA fileB fpv = do
    let bufA :: Num a => a
        bufA = 12
        bufB :: Num a => a
        bufB = 13
        p buf = playBuf 1 AR buf (bufRateScale KR buf) 1 0 Loop DoNothing
        osig  = pv_with2Inputs (p bufA) (p bufB) fpv
    mapM_ (\(bufn,file) -> async $ b_allocRead bufn file 0 0)
        [(bufA,fileA),(bufB,fileB)]
    return osig
