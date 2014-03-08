-- |
-- Copyright    : 8c6794b6, 2014
-- License      : BSD3
--
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scratch written while reading year 2003 synth secrets articles.
--
module Sound.Study.ForSynthSecrets.Y2003 where

import Control.Monad.Trans.Class (lift)
import System.Random (getStdRandom, newStdGen, randomR, randomRs)

import Sound.OSC (Bundle(..), bundle, immediately, sendOSC, time)
import Sound.SC3
import Sound.SC3.ID

import Sound.Study.ForSynthSecrets.Y1999 (centeredOut)
import Sound.Study.ForSynthSecrets.Y2001 (perform_sem02)
import Sound.Study.ForSynthSecrets.Y2002 (bd05, cym05, snr03, pn02, perform_rep01)


-- --------------------------------------------------------------------------
--
-- * January 2003
--
-- --------------------------------------------------------------------------

-- | Simple FM with two 'lfPulse' oscillators.
pfm03 :: UGen
pfm03 = centeredOut sig
  where
    sig  = car * aenv * 0.1
    car  = (lfPulse AR (cfrq + mdl) 0 0.5 - 0.5) * 2
    mdl  = lfPulse AR mfrq 0 0.5 * mfrq * idx
    idx  = aenv * 3
    cfrq = 440
    mfrq = cfrq * 1.99998
    aenv = fe ash
    fe   = envGen KR tr0 1 0 dur DoNothing
    ash  = envCoord alvs 1 1 (EnvNum en)
    alvs = [(0,0),(1e-3,1),(0.25,0.2),(0.75,1),(1,0)]
    en   = lfNoise2 'E' KR (1/3) * 25
    tr0  = impulse KR hps 0
    hps  = 0.5
    dur  = recip hps * 0.9

-- | Play 'pfm03' with rhythms from last year.
play_pfm03 :: IO ()
play_pfm03 = audition $ mrg [bd05 120, snr03 120, pfm03]

-- | Buffer number used in 'phasor_ex02'.
pex02_bufn :: Num a => a
pex02_bufn = 99

-- | Fill in the buffer for 'phasor_ex02' with given function.
prepare_pex02 :: (Int -> [Double]) -> IO ()
prepare_pex02 fval = withSC3 $ do
    let len :: Num a => a
        len = 4096
        val = fval len
    _ <- async $ b_alloc pex02_bufn len 1
    sendOSC $ b_set pex02_bufn $ zip [0..len-1] val

-- | Write sine shape to buffer.
prepare_pex02_sine :: IO ()
prepare_pex02_sine = prepare_pex02 $ \len ->
    take len $ map (\x -> cos (x*pi)) [0,2/fromIntegral len..]

-- | Play buffer with 'phasor'.
phasor_ex02 :: UGen
phasor_ex02 = centeredOut sig
  where
    sig  = bufRdC 1 AR pex02_bufn phs NoLoop * 0.3
    phs  = phasor AR 0 sfrq 0 nfrm 0
    nfrm = bufFrames KR pex02_bufn
    frq  = mouseX KR 100 800 Linear 0.1
    sfrq = (frq * nfrm) / sampleRate

-- | Write pulse shape to buffer. Buffer contents could be changed while synth
-- from 'phasor_ex02' is running.
prepare_pex02_pulse :: IO ()
prepare_pex02_pulse = prepare_pex02 $ \len ->
    let len' = len `div` 2
    in  take len' (repeat 1) ++ take len' (repeat (-1))

-- | Write triangle shape to buffer.
prepare_pex02_tri :: IO ()
prepare_pex02_tri = prepare_pex02 $ \len ->
    let dv = 2 / (fromIntegral (len `div` 2))
    in  [-1, -1 + dv .. 1] ++ [1, 1-dv .. -1]

-- | Phasor ugen example, take 3. Hard sync with buffer. Rate between master and
-- slave is controlled by 'lfNoise2'.
phasor_ex03 :: UGen
phasor_ex03 = centeredOut sig
  where
    sig  = (bufRdC 1 AR pex02_bufn phs NoLoop - 0.5) * 0.3
    phs  = phasor AR tr0 sfrq 0 nfrm 0
    tr0  = impulse AR mfrq 0
    nfrm = bufFrames KR pex02_bufn
    sfrq = (snz * mfrq * nfrm) / sampleRate
    snz  = lfNoise2 'S' AR (1/2) * 0.5 + 1.5
    mfrq = mouseY KR 100 400 Linear 0.1

-- | Plays hard sync with triggering amplitude envelope.
pn03 :: UGen
pn03 = centeredOut (sig+sig1)
  where
    sig  = rlpf sig0 (mfrq*2) 0.2 * 0.3
    sig1 = rhpf sig0 12000 0.8 *
           envGen KR tr1 1 0 0.005 DoNothing (envPerc 1e-5 1)
    sig0 = (bufRdC 1 AR pex02_bufn phs NoLoop) * 0.3 * aenv
    phs  = phasor AR tr0 sfrq 0 nfrm 0
    tr0  = impulse AR mfrq 0
    mfrq = k "freq" 440 * 0.9999998
    sfrq = smul * mfrq * nfrm / sampleRate
    smul = envGen KR tr1 1 0 0.1 DoNothing $
           envCoord [(0,1),(1e-1,2),(1,1)] 1 1 EnvLin
    tr1  = k "gate" 1
    nfrm = bufFrames KR pex02_bufn
    aenv = envGen KR tr1 amp 0 dur RemoveSynth $ envPerc 1e-4 1
    dur  = 0.8
    amp  = k "amp" 0.5
    k    = control KR

-- | Play 'pn03' once.
play_pn03_once :: IO ()
play_pn03_once = withSC3 $ do
    let name = "pn03"
    _ <- async $ d_recv $ synthdef name (mrg [pn02,pn03])
    f <- lift $ getStdRandom (randomR (log 100,log 2000))
    sendOSC $ s_new name (-1) AddToTail 1 [("freq",exp f)]

-- | Play 'perform_rep01' with 'pn03'.
play_pn03 :: IO ()
play_pn03 = do
    audition $ mrg [bd05 120, cym05]
    perform_rep01 pn03

-- | 'pn02' and 'pn03' combined with 'mrg'.
pn04 :: UGen
pn04 = mrg [pn02, pn03]

-- | Play 'perform_rep01' with 'pn04'.
perform_pn04 :: IO ()
perform_pn04 = perform_rep01 pn04

-- | Play 'perform_sem02' with 'pn04'.
perform_pn04_with_sem02 :: IO ()
perform_pn04_with_sem02 = perform_sem02 pn04


-- --------------------------------------------------------------------------
--
-- * Feburary 2003
--
-- --------------------------------------------------------------------------

-- | Apply F and F+1 to given function.
twoSigs :: (UGen -> UGen) -> UGen
twoSigs f = centeredOut sig
  where
    sig  = (sum $ map f [freq,freq+1]) * amp * 0.5
    freq = tExpRand 'f' 100 8000 tr0
    amp  = envGen KR tr0 0.3 0 (recip hps * 0.8) DoNothing $
           envCoord [(0,0),(1e-4,1),(0.1,0.8),(0.5,0.8),(1,0)] 1 1 EnvLin
    tr0  = impulse KR hps 0
    hps  = 0.5

-- | Adding two sine waves with frequency F and F+1.
twoSines :: UGen
twoSines = twoSigs $ \f -> sinOsc AR f 0

-- | Adding two saw waves with frequency F and F+1.
twoSaws :: UGen
twoSaws = twoSigs $ \f -> saw AR f

-- | Adding two pulse waves with frequency F and F+1.
twoPulses :: UGen
twoPulses = twoSigs $ \f -> pulse AR f 0.5

-- | Two signals, with vibrato given with 'lfTri'.
twoSigV :: (UGen -> UGen) -> UGen
twoSigV f = centeredOut (mceSum sig)
  where
    sig = (sum $ map f [frq+v0,frq+det]) * amp * 0.5
    frq = tExpRand 'f' 100 2000 tr0
    v0  = lfTri KR (det*5.189) 0 * det * 3.19
    det = 1
    tr0 = impulse KR hps 0
    hps = 0.25
    amp = envGen KR tr0 0.3 0 (recip hps * 0.7) DoNothing $
          envCoord [(0,0),(0.2,1),(0.4,0.8),(0.8,0.8),(1,0)] 1 1 EnvCub

-- | Sine waves with vibrato.
twoSinesV :: UGen
twoSinesV = twoSigV $ \f -> sinOsc AR (mce [f,f*1.5,f*2.5]) 0

-- | Saw waves with vibrato.
twoSawsV :: UGen
twoSawsV = twoSigV $ \f -> saw AR (mce [f,f*1.5,f*2.5])

-- | Pulse waves with vibrato.
twoPulsesV :: UGen
twoPulsesV = twoSigV $ \f -> pulse AR (mce [f,f*1.5,f*2.5]) 0.5

-- | Two signal with vibrato given by 'lfdNoise2'.
twoSigN :: (UGen -> UGen) -> UGen
twoSigN f = centeredOut (mceSum sig)
  where
    sig = (sum $ map f [frq+v0,frq+det]) * amp * 0.5
    frq = tExpRand 'f' 100 1600 tr0
    v0  = lfdNoise3 'V' KR (det*5.189) * det * 3.19
    det = 1
    tr0 = impulse KR hps 0
    hps = 0.125
    amp = envGen KR tr0 0.3 0 (recip hps * 0.7) DoNothing $
          envCoord [(0,0),(0.2,1),(0.4,0.8),(0.8,0.8),(1,0)] 1 1 EnvCub

-- | 'twoSigN' with 'sinOsc'.
twoSinesN :: UGen
twoSinesN = twoSigN $ \f -> sinOsc AR (mce [f,f*1.5,f*2.5]) 0

-- | 'twoSigN' with 'saw'.
twoSawsN :: UGen
twoSawsN = twoSigN $ \f -> saw AR (mce [f,f*1.5,f*2.5,f*2])

-- | 'twoSigN' with 'pulse'.
twoPulsesN :: UGen
twoPulsesN = twoSigN $ \f -> pulse AR (mce [f,f*1.5,f*2.5]) 0.5

-- | Multiple saw tooth with detuned frequencies.
multipleSaws :: UGen
multipleSaws = centeredOut sig
  where
    sig  = sig0 * aenv * 0.3
    sig0 = rlpf (mceSum (saw AR fs)) (freq * 5) 0.5
    fs   = mce $ map (+freq) dets
    dets = [-0.8694528980902436,-0.4787910043931145,-0.5560687349572753
           , 0.8518397572101095,-0.7231956566130175,-0.6259530115658036
           , 0.6062475508105767, 0.3871863006930405,-0.4595432108493276
           ,-0.6613892194271618, v]
    freq = k "freq" 440
    v    = lfNoise2 'V' KR 3.22 * 1.17
    aenv = envGen KR tr0 amp 0 dur RemoveSynth $ envCoord lvls 1 1 EnvCub
    lvls = [(0,0),(0.2,1),(0.5,1),(0.8,0.95),(1,0)]
    dur  = k "dur" 1
    amp  = k "amp" 0.3
    tr0  = k "gate" 1
    k    = control KR

-- | Modulationg pulse width between 50% and 5% with triangle wave.
pwm01 :: UGen
pwm01 = centeredOut sig
  where
    sig    = rhpf (rlpf sigs (frq0*5) 0.5) 40 0.85 * aenv * 0.2
    sigs   = sum $ map (uncurry f1) ps
    ps     = (0,1) : zip dps dws
    dps    = [-0.3651983252650044,0.46339821255519764
             ,-0.7497778590265456,0.33883386935335813
             ,0.46722952654838257,0.9304308805488517
             ,0.16887344312030006,0.4109131382358091
             ,-0.1967293764068594,-0.901161274196016]
    dws    = [0.9555108548236257,0.9600920677511051
             ,1.0353251024207,0.9705633604923299
             ,0.9495772991655825,1.0209414303560214
             ,1.0818177545480583,0.9686373140782325
             ,0.9651075392628111,0.9986139793666811]
    f1 x y = f0 (frq0+x) (wmf*y)
    f0 x y = pulse AR x (lfTri KR y 0 * 0.225 + 0.275)
    frq0   = k "freq" 440
    tr0    = k "gate" 1
    aenv   = envGen KR tr0 amp 0 dur RemoveSynth ash
    ash    = envCoord lvls 1 1 EnvLin
    lvls   = [(0,0),(0.2,1),(0.5,1),(0.8,0.95),(1,0)]
    dur    = k "dur" 2
    amp    = k "amp" 0.3
    wmf    = 1.32
    k      = control KR

-- | Simple performance for string machine.
perform_smch01 :: UGen -> IO ()
perform_smch01 ug = withSC3 $ do
    let name = "smch01"
        sn a d f = s_new name (-1) AddToTail 1 [("amp",a),("dur",d),("freq",f)]
        pcs  = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
        octs = take 5 $ iterate (+12) 36
        dur  = 1
        degs = [2,4,5,7,9,11]
        go g0 t0 tend =
            let (nv,g1) = randomR (1,5) g0
                is      = take nv $ randomRs (0,length pcs - 1) g0
                (a,g2)  = randomR (0.04,0.12) g1
                (d,g3)  = randomR (1,4::Int) g2
                (dr,g4) = randomR (0.75,4) g3
                d'      = fromIntegral d * dur
                t1      = t0 + d'
                sn' i   = sn a (d'*dr) (pcs !! i)
                bdl     = bundle t0 (map sn' is)
            in  if t0 > tend then [] else bdl : go g4 t1 tend
    _ <- async $ d_recv $ synthdef name ug
    now <- time
    g0 <- lift newStdGen
    mapM_ sendOSC $ go g0 now (now+3000)

-- | Play 'perform_smch01' with 'multipleSaws'.
play_multipleSaws :: IO ()
play_multipleSaws = perform_smch01 multipleSaws

-- | Play 'perform_smch01' with 'pwm01'.
play_pwm01 :: IO ()
play_pwm01 = perform_smch01 pwm01


-- --------------------------------------------------------------------------
--
-- * March 2003
--
-- --------------------------------------------------------------------------

-- | Making pulse wave with duty cycle of 50% with 'toggleFF' and 'impulse's.
manualPulse01 :: UGen
manualPulse01 = centeredOut sig
  where
    sig = toggleFF t * 0.3
    t   = impulse AR frq 0 + impulse AR frq 0.5
    frq = 440

-- | Making pulse wave with 'toggleFF' and 'impulse's.
--
-- Changing phase of one of the impulse to change the duty cycle between 50% and
-- 5%. Mouse X contrls pulse frequency, Y controls width modulation frequency.
--
manualPulse02 :: UGen
manualPulse02 = centeredOut sig
  where
    sig = toggleFF t * 0.3
    t   = impulse AR frq 0 + impulse AR frq phs
    phs = lfTri KR mf 0 * 0.225 + 0.275
    frq = mouseX KR 100 800 Exponential 0.1
    mf  = mouseY KR 1 30 Linear 0.1

-- | Making pulse wave with 'lfSaw's.
manualPulse03 :: UGen
manualPulse03 = centeredOut sig
  where
    sig  = (sig0 + sig1 - 1) * 0.3
    sig0 = lfSaw AR frq 0
    sig1 = 1 - lfSaw AR (frq+det) 0
    frq  = mouseY KR 100 800 Linear 0.1
    det  = sinOsc AR 0.25 0 * 0.225 + 0.275

-- --------------------------------------------------------------------------
--
-- * April 2003
--
-- --------------------------------------------------------------------------

-- | Simple filtered saw.
vln01 :: UGen
vln01 = centeredOut sig
  where
    sig  = f0 (saw AR frq) * amp
    amp  = envGen KR tr0 0.3 0 dur DoNothing ash
    ash  = envCoord lvls v0 1 (EnvNum (lfNoise2 'e' KR 1 * 15))
    lvls = [(0,0),(t0,1),(t1,1),(1,0)]
    t0   = tRand '0' 1e-3 (1-1e-3) tr0
    t1   = t0 + tRand '1' 1e-3 (1-t0) tr0
    v0   = tRand 'v' 0.1 1 tr0
    f0 x = f1 $ sum [resonz x 300 0.3, resonz x 1800 0.6, resonz x 3200 0.1]
    f1 x = rlpf x 6000 0.2
    dur  = recip hps
    frq  = select idx (mce frqs)
    frqs = [440,660,330,550,880]
    idx  = pulseCount tr0 0 `modE` constant (length frqs)
    tr0  = impulse KR hps 0
    hps  = 1


-- --------------------------------------------------------------------------
--
-- * May 2003
--
-- --------------------------------------------------------------------------

-- | Simple violin with filtered saw wave.
vln02 :: UGen
vln02 = centeredOut sig
  where
    sig  = freeVerb (f0 (saw AR frq) * aenv) 0.33 0.9 0.3
    f0 x = mix $ sum $ map f0' [(500,0.4),(700,0.4),(4000,0.8)]
      where f0' (a,b) = resonz (f1 x) a b
    f1 x = rhpf (rlpf x (6592+rf*0.5) 0.3) (100+rf*0.125) 0.2
    aenv = envGen KR tr0 amp 0 dur RemoveSynth ash * trml
    ash  = envCoord lvls 1 1 EnvCub
    lvls = [(0,0),(atk,1),(atk+0.15,0.8),(0.8,0.8),(1,0)]
    atk  = tRand 'a' 5e-4 0.5 tr0
    trml = lfCub KR vrt 0 * 0.25 + 0.75
    frq  = fenv * rf + vib
    fenv = envGen KR tr0 1 0 dur DoNothing fsh
    fsh  = envCoord [(0,fv0),(0.1,1),(0.12,fv1),(0.13,1)] 1 1 EnvLin
    fv0  = tRand ')' 0.99 1.01 tr0
    fv1  = tRand '!' 0.99 1.01 tr0
    rf   = k "freq" 440
    vib  = lfTri KR vrt 0 * (rf/64)
    vrt  = envGen KR tr0 vrtv 0 dur DoNothing fvsh
    fvsh = envCoord [(0,0),(vst,0.0),(0.4,0.8),(1,1)] 1 1 EnvSin
    vrtv = tRand 'V' 3 6 tr0
    vst  = tRand 'v' 0.1 1 tr0
    amp  = k "amp" 0.3
    dur  = k "dur" 0.8
    tr0  = k "gate" 1
    k    = control KR


-- --------------------------------------------------------------------------
--
-- * June 2003
--
-- --------------------------------------------------------------------------

-- | Play 'perform_smch01' with 'vln02'.
play_smch01_vln02 :: IO ()
play_smch01_vln02 = perform_smch01 vln02

-- | Play 'perform_rep01' with 'vln02'.
play_rep01_vln02 :: IO ()
play_rep01_vln02 = perform_rep01 vln02

-- --------------------------------------------------------------------------
--
-- * July 2003
--
-- --------------------------------------------------------------------------
{-
Quote:

So there we have this month's Synth Secret; two modules and a more appropriate
method of controlling them can be far more expressive and create more realistic
bowed string and brass sounds than any number of modules and facilities
controlled by a less suitable device. It's an important lesson, but because of
the ubiquity of the keyboard synthesizer, it's not one that many people have had
the opportunity to learn.

-}

-- | Simple saw tooth signal with \"amp\" and \"freq\" controls.
vln03 :: UGen
vln03 = centeredOut (freeVerb sig 0.4 0.3 0.9)
  where
    sig  = mceSum (selectX form (mce [sig0,sig1])) * amp
    sig0 = saw AR freq
    sig1 = lfTri AR freq 0
    amp  = control KR "amp" 0.1
    freq = control KR "freq" 440
    form = control KR "form" 0

-- | Modify synth's control parameters with mapping control signal output.
perform_nmap01 :: UGen -> IO ()
perform_nmap01 ug = withSC3 $ do
    let name    = "nmap01"
        sd a b = async . d_recv $ synthdef a b
        cntrl  = mrg [oamp, ofrq, ofrm]
          where
            oamp = out 100 ((lfdNoise3 'E' KR (1/7) * 0.5 + 0.5) * trm * 0.2)
            trm  = lfCub KR vf 0 * 0.35 + 0.65
            ofrq = out 101 (lag2 frq lt + vib)
            vib  = lfTri KR vf 0 * frq / 64
            vf   = tRand 'V' 3 5 tr0
            frq  = tChoose 'f' tr0 (mce pch)
            pch  =
                let f p ps = map (midiCPS . (+p)) degs ++ ps
                in  foldr f [] octs
            octs = take 3 $ iterate (+12) 48
            degs = [0,2,4,5,7,9,11]
            lt   = tExpRand 'L' 0.01 2 tr0
            tr0  = coinGate 'G' prob (impulse KR 4 0)
            prob = lfNoise2 'F' KR 0.5 * 0.5 + 0.5
            ofrm = out 102 (lfdNoise3 'M' KR 1.3 * 0.5 + 1.5)
    mapM_ (uncurry sd) [("nmap01",ug),("nmap01_cntrl",cntrl)]
    sendOSC $ bundle immediately
        [ s_new name 1000 AddToTail 1 []
        , s_new "nmap01_cntrl" (-1) AddBefore 1000 []
        , n_map 1000 [("amp",100),("freq",101),("form",102)]
        ]

-- | Play 'perform_nmap01' with 'vln03'.
play_nmap01_vln03 :: IO ()
play_nmap01_vln03 = perform_nmap01 vln03


-- --------------------------------------------------------------------------
--
-- * August 2003
--
-- --------------------------------------------------------------------------

-- | Simple blow sound, take 1.
blow01 :: UGen
blow01 = centeredOut sig
  where
    sig  = (sig0 * amp0) + (sig1 * amp1)
    sig0 = pulse AR frq 0.5
    frq  = 440
    amp0 = envGen KR tr0 0.3 0 dur DoNothing ash0
    ash0 = Envelope [0,1,0.5,0] [0.1,0.2,0.7] [EnvCub] Nothing Nothing
    tr0  = impulse KR 1 0
    dur  = 0.8
    sig1 = whiteNoise 'B' AR
    amp1 = envGen KR tr0 0.02 0 dur DoNothing ash1
    ash1 = Envelope [0,1,0.5,0] [0.03,0.8,0.07] [EnvSqr] Nothing Nothing

-- | Simple blow sound, take 2.
blow02 :: UGen
blow02 = centeredOut sig
  where
    sig  = (sig0 * amp0) + (sig1 * amp1)
    sig0 = foldr (\n x -> ringz nz0 (frq*n) dcyt * 0.05 + x) 0 [1..6]
    dcyt = linLin (lfdNoise3 'D' KR 0.5) (-1) 1 0.1 0.4
    nz0  = rhpf (whiteNoise 'W' AR) 800 0.8
    amp0 = envGen KR tr1 0.2 0 dur DoNothing ash0
    ash0 = Envelope [0,1,0.5,0] [0.1,0.7,0.2] [EnvCub] Nothing (Just 0)
    frq  = tExpRand 'F' 200 800 (coinGate 'c' 0.125 tr0)
    sig1 = rlpf nz0 8000 0.5
    amp1 = envGen KR tr1 0.05 0 dur DoNothing ash1
    ash1 = Envelope [0,1,0.1,0.1,0] [0.01,0.2,0.6,0.09] [EnvCub] Nothing Nothing
    tr0  = impulse KR hps 0
    tr1  = coinGate 'R' 0.5 tr0
    hps  = 3
    dur  = (recip hps * 0.8)

-- | Simple blow sound, take 3.
blow03 :: UGen
blow03 = centeredOut sig
  where
    sig   = (sig0 + sig1) * amp * 0.9
    sig0  = ((sig00*amp00) + (sig01*amp01)) * 0.7
    sig00 = foldr (\n x -> ringz nz0 (freq*n) dcyt * 0.03 + x) 0 [1..6]
    dcyt  = 0.08
    nz0   = rhpf (whiteNoise 'A' AR) 2800 0.8
    amp00 = envGen KR tr0 0.2 0 dur DoNothing ash00
    ash00 = Envelope [0,1,0.15,0.1,0] [0.1,0.05,0.8,0.05] [EnvCub]
            Nothing Nothing
    sig01 = rlpf nz0 800 0.9
    amp01 = envGen KR tr0 0.3 0 dur01 DoNothing ash01
    dur01 = clip dur (dur*0.5) (tRand 'U' 0.3 0.6 tr0)
    ash01 = envCoord [(0,0),(0.01,1),(0.1,0.1),(0.8,0.1),(1,0)] 1 1 EnvCub
    sig1  = rlpf (pulse AR freq 0.5) (freq*1.28) 0.9988 * amp1
    amp1  = envGen KR tr0 0.3 0 dur RemoveSynth ash1
    ash1  = envCoord [(0,0),(0.06,1),(0.85,1),(1,0)] 1 1 EnvCub
    dur   = k "dur" 1
    tr0   = k "gate" 1
    amp   = k "amp" 1
    freq  = freq0 + lfNoise2 'F' KR (10/log freq0) * 0.2
    freq0 = k "freq" 440
    k     = control KR

-- | Play 'perform_smch01' with 'blow03'.
play_smch01_blow03 :: IO ()
play_smch01_blow03 = perform_smch01 blow03


-- --------------------------------------------------------------------------
--
-- * September 2003
--
-- --------------------------------------------------------------------------

-- | Simple blow sound, take 4.
blow04 :: UGen
blow04 = centeredOut sig
  where
    sig  = f0 sig0 * aenv
    sig0 = lfTri AR freq 0 + (whiteNoise 'A' AR * 0.15)
    f0 x = resonz x freq qenv
    qenv = envGen KR tr0 1 0 dur DoNothing qsh
    qsh  = envCoord [(0,0.9),(0.001,0.98),(0.8,0.001),(1,0.001)] 1 1 EnvLin
    aenv = envGen KR tr0 amp 0 dur RemoveSynth ash
    ash  = envCoord [(0,0),(0.1,1),(0.4,0.6),(0.8,0.6),(1,0)] 1 1 EnvCub
    dur  = k "dur" 1
    amp  = k "amp" 0.3
    freq = k "freq" 440
    tr0  = k "gate" 1
    k    = control KR

-- | Play 'perform_smch01' with 'blow04'.
play_smch01_blow04 :: IO ()
play_smch01_blow04 = perform_smch01 blow04

-- | Simple blow sound, take 5.
blow05 :: UGen
blow05 = centeredOut sig
  where
    sig   = f0 sig0 * aenv0 * 0.6
    sig0  = pulse AR freq 0.4 + (whiteNoise 'W' AR * 0.35)
    f0 x  = rlpf x (freq*0.85) qenv + resonz x (freq*0.65) qenv
    qenv  = envGen KR tr0 1 0 dur DoNothing qsh
    qsh   = envCoord [(0,0.9),(0.001,0.9),(0.8,0.001),(1,0.9)] 1 1 EnvLin
    aenv0 = envGen KR tr0 amp0 0 dur RemoveSynth ash0
    ash0  = envCoord [(0,0),(0.1,1),(0.3,0.5),(0.8,0.5),(1,0)] 1 1 EnvCub
    amp0  = k "amp" 0.3
    freq  = freq0 + lfNoise2 'F' KR (8/log freq0) * 0.2
    freq0 = k "freq" 440
    dur   = k "dur" 0.8
    tr0   = k "gate" 1
    k     = control KR

-- | Play 'perform_smch01' with 'blow05'.
play_smch01_blow05 :: IO ()
play_smch01_blow05 = perform_smch01 blow05


-- --------------------------------------------------------------------------
--
-- * October 2003
--
-- --------------------------------------------------------------------------

{-
Quote:

A flute does not get significantly louder or softer as the player alters the
blowing pressure; it becomes brighter or duller.
-}

-- | Simple blow sound, take 6.
blow06 :: UGen
blow06 = centeredOut sig
  where
    sig   = sig0 * aenv0 * amp * 0.05
    sig0  = rhpf sig1 freq 0.05
    sig1  = rlpf sig2 ((2000+freq*0.25)+(3*freq*cflo)) rql
    sig2  = saw AR freq
    cflo  = lfCub KR cflf 0 * 0.5 + 0.5
    cflf  = aenv0 * (lfNoise2 'c' KR 1 * 0.5 + 0.5) * 7
    rql   = aenv0 * 0.89 + 0.01
    aenv0 = envGen KR tr0 1 0 dur RemoveSynth ash0
    ash0  = Envelope [0,1,1,0] [0.3,0.4,0.3] [EnvCub] Nothing Nothing
    tr0   = k "gate" 1
    amp   = k "amp" 0.3
    dur   = k "dur" 1
    freq  = k "freq" 440
    k     = control KR

-- | Play 'perform_smch01' with 'blow06'.
play_smch01_blow06 :: IO ()
play_smch01_blow06 = perform_smch01 blow06


-- --------------------------------------------------------------------------
--
-- * November 2003
--
-- --------------------------------------------------------------------------

-- | Simple organ patch, take 1.
orgn01 :: UGen
orgn01 = centeredOut sig
  where
    sig  = f0 sig0 * aenv
    f0 x = rlpf x cf rq
    rq   = envGen KR tr1 1 0 0.2 DoNothing rqsh
    rqsh = envCoord [(0,0.91),(0.01,0.08),(0.9,0.05),(1,0.06)] 1 1 EnvLin
    cf   = freq * 3
    sig0 = pulse AR freq (1/3) + pulse AR freq (1/2)
    aenv = envGen KR tr1 amp 0 1 RemoveSynth ash
    ash  = Envelope [0,1,1,0] [0.001,0.998,0.001] [EnvCub] (Just 2) Nothing
    amp  = 0.03
    freq = k "freq" 440
    tr1  = k "gate" 1
    k    = control KR

-- | Perform gated synth having sustained notes.
perform_gtd01 :: UGen -> IO ()
perform_gtd01 ug = withSC3 $ do
    let name = "gtd01"
        pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
        octs = take 6 $ iterate (+12) 24
        degs = [0,2,5,7]
        bdl t nid f d =
            [ bundle t
              [ s_new name nid AddToTail 1 [("freq",f)]]
            , bundle (t+d)
              [ n_set nid [("gate",0)]]
            ]
        go t0 tend nid0 g0 =
            let bdls      = bdl t0 nid0 (pchs !! i) dur
                (i,g1)    = randomR (0,length pchs - 1) g0
                (dpow,g2) = randomR (0,3::Int) g1
                (imul,g3) = randomR (1,2::Int) g2
                dur       = dt * (2 ^ dpow)
                nid1      = nid0 + 1
                t1        = t0 + (dt * fromIntegral imul)
                dt        = 0.125
            in  if tend < t1 then [] else bdls ++ go t1 tend nid1 g3
    _ <- async $ d_recv $ synthdef name ug
    now <- time
    g0 <- lift newStdGen
    -- mapM_ sendOSC $ go now (now+300) 10000 g0
    let os = go now (now+300) 10000 g0
    lift $ do
        let o1 = head os
            o2 = last os
        putStrLn $ unwords ["sending", show $ length os, "bundles,"
                           , show (bundleTime o2 - bundleTime o1), "secs."]
    mapM_ sendOSC os

-- | Play 'perform_gtd01' with 'orgn01'.
play_gtd01_orgn01 :: IO ()
play_gtd01_orgn01 = perform_gtd01 orgn01


-- --------------------------------------------------------------------------
--
-- * December 2003
--
-- --------------------------------------------------------------------------

-- | Simple organ, take 2.
orgn02 :: UGen
orgn02 = centeredOut sig
  where
    sig   = f0 (sig0 + sig1) * aenv0
    f0 x  = rlpf x (freq*1.5*cfenv) 0.3
    cfenv = envGen KR tr0 1 0 dur DoNothing cfsh
    cfsh  = envCoord [(0,0.01),(0.001,10),(0.01,1)] 1 1 EnvCub
    sig0  = pulse AR freq (1/2)
    sig1  = pulse AR (freq*2) (1/3)
    aenv0 = envGen KR tr0 amp 0 dur RemoveSynth ash0
    ash0  = Envelope [0,1,1,0] [0.05,0.9,0.05] [EnvLin] (Just 2) Nothing
    tr0   = k "gate" 1
    amp   = k "amp" 0.2
    freq  = k "freq" 440
    k     = control KR
    dur   = 1

-- | Play 'perform_gtd01' with 'orgn02'.
play_gtd01_orgn02 :: IO ()
play_gtd01_orgn02 = perform_gtd01 orgn02

-- | Simple organ, take 3. Using additive synthesis method.
orgn03 :: UGen
orgn03 = centeredOut sig
  where
    sig   = sig0 * aenv0
    sig0  = mix $ sinOsc AR (mce [freq,freq*2,freq*3,freq*6]) 0
    aenv0 = envGen KR tr0 amp 0 dur RemoveSynth ash0
    ash0  = Envelope [0,1,1,0] [0.0125,0.975,0.0125] [EnvCub] (Just 2) Nothing
    tr0   = k "gate" 1
    amp   = k "amp" 0.05
    freq  = k "freq" 440
    k     = control KR
    dur   = 1

-- | Play 'perform_gtd01' with 'orgn03'.
play_gtd01_orgn03 :: IO ()
play_gtd01_orgn03 = perform_gtd01 orgn03


-- ------------------------------------------------------------------------
--
-- * Miscellaneous
--
-- --------------------------------------------------------------------------

-- | Brought from B001.
hit001 :: UGen
hit001 = out (k "out" 0) o
  where
    o     = i * hit * 0.1 * amp
    hit   = envGen KR tr 1 0 dur DoNothing shape
    shape = Envelope [1e-9,1e-9,1,1e-9] [0,atk,1-atk] [EnvExp] (Just (-1)) (Just 0)
    atk   = linExp (lfdNoise3 'k' KR (1/16) * 0.5 + 0.51) 0.1 1.01 1e-4 9999e-4
    dur   = linExp (lfdNoise3 'd' KR (1/16) * 0.5 + 0.51) 0.1 1.01 5e-3 2
    amp   = tExpRand 'a' 0.1 1 tr
    tr    = coinGate 'd' prob itr + dust 't' KR dtf
    itr   = impulse KR 8 0
    prob  = linExp (lfdNoise3 'p' KR (1/32) * 0.5 + 0.51) 0.1 1.01 (1/3) 1
    dtf   = linLin (lfdNoise3 'r' KR (1/32) * 0.5 + 0.51) 0.1 1.01 1e-2 3
    i     = henonC AR 24000 n0 n1 0 0
    n0    = n * 0.2 + 1.2
    n1    = n * 0.1 + 0.25
    n     = lfdNoise3 'h' KR 1
    -- i     = whiteNoise 'W' AR
    k     = control KR

-- | Brought from B001.
rng001 :: UGen
rng001 = mrg [replaceOut (k "out_1" 0) o, out (k "out_2" 1) o]
  where
    h f t a = ringz sig (lag2 f 8) t * a
    o   = sum $ zipWith3 h fs ts as
    fs  = [lfdNoise3 'a' KR (1/128) * 100 + 150
          ,lfdNoise3 'b' KR (1/128) * 300 + 500
          ,lfdNoise3 'c' KR (1/128) * 1200 + 2000
          ,lfdNoise3 'd' KR (1/128) * 4800 + 8000]
    ts  = [lfdNoise3 'b' KR 1 * 0.5 + 0.6
          ,lfdNoise3 'd' KR 1 * 0.8 + 0.9
          ,lfdNoise3 'a' KR 1 * 0.5 + 0.6
          ,lfdNoise3 'c' KR 1 * 0.8 + 0.9]
    as  = [lfdNoise3 'c' KR 1 * 0.25 + 0.24
          ,lfdNoise3 'a' KR 1 * 0.25 + 0.24
          ,lfdNoise3 'b' KR 1 * 0.25 + 0.24
          ,lfdNoise3 'd' KR 1 * 0.25 + 0.24]
    sig = control AR "a_in" 0
    k   = control KR

-- | Brought from B001.
cmb001 :: UGen
cmb001 = replaceOut (k "out" 0) o
  where
    o = hpf ((c*m) + (sig * (1-m))) 20
    c = foldr f sig [1..16::Int]
    f a b = combC b 0.5 (dlt a) (dct a)
    dlt i = lag3 (tExpRand i (recip (k "rmin" 50)) (recip (k "rmax" 1)) tr) 28e-3
    dct i = lag3 (tExpRand i 120e-3 800e-3 tr) 28e-3
    tr = coinGate 't' prob tin
    prob = linLin (lfdNoise3 'q' KR (1/64) * 0.5 + 0.51) 0.1 1.01 (1/32) (1/2)
    m =   lag (k "mix" 1) 0.2
    tin = impulse KR 8 0
    sig = control AR "a_in" 0
    k   = control KR

-- | Setup and route the ins and outs of 'hit001', 'rng001', and 'cmb001'.
setup_hit_rng_cmb :: IO ()
setup_hit_rng_cmb = withSC3 $ do
    mapM_ (\(n,ug) -> async $ d_recv $ synthdef n ug)
        [("hit001",hit001),("rng001",rng001),("cmb001",cmb001)]
    sendOSC $ bundle immediately
        [ g_new [(10,AddToTail,1)]
        , s_new "hit001" 1000 AddToTail 10 []
        , s_new "rng001" 1001 AddAfter 1000 []
        , s_new "cmb001" 1002 AddAfter 1001 []
        , n_mapa 1001 [("a_in",0)]
        , n_mapa 1002 [("a_in",0)]
        ]

-- | Compare filter ugen.
lpfcomp_ex01 :: (UGen -> UGen -> UGen -> UGen) -> UGen
lpfcomp_ex01 filt = centeredOut sig
  where
    sig  = filt sig0 cf rq
    sig0 = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.3)
    cf   = mouseX KR 100 20000 Exponential 0.2
    rq   = mouseY KR 0.1 1 Linear 0.1

-- | Apply 'rlpf' to 'lpfcomp_ex01'.
lpfcomp_rlpf :: IO ()
lpfcomp_rlpf = audition $ lpfcomp_ex01 rlpf

-- | Apply 'bLowPass' to 'lpfcomp_ex01'.
lpfcomp_bLowPass :: IO ()
lpfcomp_bLowPass = audition $ lpfcomp_ex01 bLowPass
