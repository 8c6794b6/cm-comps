{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Synthdefs used in session modules.

-}
module Session.Synthdefs where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.TH.Synthdef (synthdefGenerator)

-- --------------------------------------------------------------------------
--
-- * All synthdefs from this module
--
-- --------------------------------------------------------------------------

-- | Synthdefs defined in this haskell module.
synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

-- --------------------------------------------------------------------------
--
-- * Source synthdefs
--
-- --------------------------------------------------------------------------

-- | Sample synth controlled by mapping control rate bus with signals from
-- demand ugen.
synth_saw01 :: UGen
synth_saw01 = out (control KR "out" 0) $ osig
  where
    osig  = rlpf (saw AR (mce [freq,freq*1.001])) cf rq  * 0.1 * amp
    rq    = control KR "rq" 0.1
    cf    = linExp (control KR "cf" 0.5 + 0.001) 0.001 1.001 20 20000 `lag` lagt
    freq  = control KR "freq" 440 `lag` lagt
    lagt  = linExp (control KR "lagt" 0 + 0.001) 0.001 1.001 0.001 1
    amp   = control KR "amp" 0

synth_saw02 :: UGen
synth_saw02 = out obus osig
  where
    osig = pan2 (saw AR freq * aenv * 0.3) pan 1
    aenv = envGen KR tr 1 0 0.3 DoNothing
           (Envelope [0,1,1,0] [1e-3,0.2,0.799] [EnvSin] Nothing Nothing)
    obus = k "out" 0
    freq = k "freq" 220
    pan  = k "pan" 0
    tr   = tr_control "tr" 0
    k    = control KR

synth_saw03 :: UGen
synth_saw03 = out (k "out" 0) osig
  where
    osig  = pan2 saws (linLin pan 0 1 (-1) 1) 1
    saws  = mix $ saw AR freqs * aenv * 0.3
    aenv  = envGen KR tr 1 0 dur DoNothing
            (Envelope [0,1,0] [atk,1-atk] [EnvNum en] Nothing Nothing)
    freqs = mce [freq, freq*0.998, freq*1.003]
    freq  = k "freq" 220
    pan   = k "pan" 0
    en    = k "en" 0
    dur   = k "dur" 1
    atk   = k "atk" 1e-4
    tr    = tr_control "tr" 0
    k     = control KR

-- | Polyphony sum of 'saw' oscillators with 'rlpf'.
synth_saw04 :: UGen
synth_saw04 = out (k "out" 0) osig
  where
    osig = sum (map fsig [0..npoly-1])
    npoly :: Int
    npoly = 8
    fsig i = pan2 saws (linLin pani 0 1 (-1) 1) 1
      where
        saws  = mix (rlpf sig0 cfi rqi * 0.3 * aenv)
        sig0  = clip2 (saw AR freqs * preg) clp2
        aenv  = envGen KR tri 1 0 duri DoNothing
                (Envelope [0,1,0] [atki,reli] [EnvNum eni] Nothing Nothing)
        freqs = mce [freqi, freqi*0.998, freqi*1.003]
        tri   = pulseDivider tr (c npoly) (c i)
        duri  = gate dur tri
        atki  = gate atk tri
        reli  = 1 - atki
        -- eni   = gate en tri
        eni   = en
        freqi = gate freq tri
        pani  = gate pan tri
        cfi   = gate cf tri * decay tri cdu
        cdu   = tExpRand i 0.4 0.8 tri
        rqi   = gate rq tri
    cf   = k "cf" 8000
    freq = k "freq" 220
    pan  = k "pan" 0
    en   = k "en" 0
    dur  = k "dur" 1
    atk  = k "atk" 1e-4
    rq   = k "rq" 0.6
    clp2 = k "clp2" 0.6
    preg = k "gain" 1
    tr   = tr_control "tr" 0
    k    = control KR
    c    = constant

-- | Percussive synth with 'resonz'ated 'whiteNoise'.
synth_nz02 :: UGen
synth_nz02 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = mix (resonz (whiteNoise 'w' AR) (mce [2232,3123,4502]) 0.3 * aenv)
    aenv  = decay t_tr0 0.2
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 0.3

-- | Percussive synth with 'sineOsc' and 'saw'.
synth_bd03 :: UGen
synth_bd03 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = (sig1 + sig2) * amp
    sig1  = mix (sinOsc AR (mce frq1s) 0 * aenv1 * 0.3)
    frq1s = map (*frq1) [1,1.423,2.87,4.12,5.83,7.32]
    aenv1 = envGen KR t_tr0 1 0 dur1 DoNothing $
            Envelope [0,1,0] [0.001, 0.999] [EnvCub] (Just 0) Nothing
    sig2  = saw AR frq2 * aenv2 * 0.2
    aenv2 = envGen KR t_tr0 1 0 dur2 DoNothing $
            Envelope [0,1,0.8,0] [0.01,0.98,0.01] [EnvCub] (Just 0) Nothing
    frq1  = linExp (control KR "frq1" 0.1 + 0.001) 0.001 1.001 20 80
    frq2  = linExp (control KR "frq2" 0.1 + 0.001) 0.001 1.001 200 2000
    dur1  = linExp (control KR "dur1" 0.2 + 0.001) 0.001 1.001 0.1 1
    dur2  = linExp (control KR "dur2" 0.2 + 0.001) 0.001 1.001 0.001 0.01
    amp   = control KR "amp" 0
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 1

synth_snr01 :: UGen
synth_snr01 = out (control KR "out" 0) (pan2 osig pan 1)
  where
    osig   = (nz + oscs) * amp
    nz     = rhpf (whiteNoise 'Γ' AR) (cf * fenv + 100) rq * aenv0
    aenv0  = envGen KR tr 1 0 durn DoNothing
             (Envelope [0,1,1,0] [atkn,susn,reln] [EnvNum enn] Nothing Nothing)
    oscs   = resonz (mix (fosc (mce frqs * fenv)) * aenv1) cf rq
    fosc x = pulse AR x wdth
    aenv1  = envGen KR tr 1 0 duro DoNothing
             (Envelope [0,1,1,0] [atko,suso,relo] [EnvNum eno] Nothing Nothing)
    fenv   = envGen KR tr 1 0 duro DoNothing
             (Envelope [1,1,0.5] [atko,1-atko] [EnvCub] Nothing Nothing)
    frqs   = [108.32, 150.32, 202.321, 257.83, 301.23]
    tr     = tr_control "tr" 0
    reln   = 1 - atkn - susn
    relo   = 1 - atko - suso
    wdth   = kc "wdth" 0.25
    amp    = kc "amp" 0.3
    durn   = kc "durn" 0.3
    duro   = kc "duro" 0.1
    eno    = kc "eno" 3
    enn    = kc "enn" (-3)
    atkn   = kc "atkn" 0.01
    atko   = kc "atko" 0.02
    susn   = kc "susn" 0.3
    suso   = kc "suso" 0.3
    cf     = kc "cf" 800
    rq     = kc "rq" 0.8
    pan    = kc "pan" 0

synth_snr02 :: UGen
synth_snr02 = out (kc "out" 0) (pan2 osig pan 1)
  where
    osig  = (sig0 + sig1) * (kc "mamp" 0.4)
    sig0  = mceSum (sinOsc AR freqs 0 * amp0)
    freqs = mce [283,331] * fenv
    fenv  = envGen KR tr 1 0 (dur*1.3) DoNothing
            (Envelope [1,1,0] [1e-4,1] [EnvSqr] Nothing Nothing)
    amp0  = envGen KR tr 0.3 0 dur DoNothing (envPerc 1e-4 1)
    dur   = 0.1
    tr    = tr_control "tr" 0
    sig1  = rhpf nz cf 0.8 * amp1
    nz    = whiteNoise 'A' AR
    amp1  = envGen KR tr 0.5 0 dur DoNothing (envPerc 1e-3 1)
    cf    = (5000 * fenv) + 100
    pan   = linLin (kc "pan" 0.5) 0 1 (-1) 1

synth_snr03 :: UGen
synth_snr03 = out (kc "out" 0) (pan2 sig (kc "pan" 0) 1)
  where
    sig    = ((sig0*ampp) + (sig1*(1-ampp))) * (kc "ampm" 1)
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
    mamp   = tExpRand 'A' 0.3 0.6 tr0
    rq     = 0.8
    ampp   = 0.2
    tr0    = tr_control "tr" 0

synth_poly01 :: UGen
synth_poly01 = out obus (pan2 osig pan 1)
  where
    npoly  = 8
    osig   = sum $ map fsig [1..npoly::Int]
    c      = constant
    fsig i = rlpf (pulse AR freq wd) cf rq * aenv * 0.1
      where
        tr   = pulseDivider tr0 (c npoly) (c i)
        aenv = envGen KR tr 1 0 dur DoNothing $
               Envelope [0,1,0.8,0] [0.001,0.5,0.4999] [EnvSqr] (Just 0) Nothing
        freq = gate freq0 tr
        wd   = linLin (lfdNoise3 i KR (15.2 * aenv)) (-1) 1 0.25 0.5
        cf   = squared aenv * freq * 8
        rq   = 0.8
    freq0 = control KR "freq" 440
    dur   = linExp (control KR "dur" 0.5 + 0.001) 0.001 1.001 0.1 16
    tr0   = tr_control "t_tr0" 1
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    obus  = control KR "out" 0

synth_tuis05 :: UGen
synth_tuis05 = out obus (pan2 osig pan 1)
  where
    osig = sinOsc AR freq phs * aenv * 0.3
    aenv = envGen KR tr0 1 0 dur DoNothing $
           Envelope [0,1,0.3,0.2,0] [0.02,0.2,0.78] [EnvCub] (Just 0) Nothing
    tr0  = tr_control "t_tr0" 1
    freq = control KR "freq" 440
    phs  = sinOsc AR (freq * 2.4995) 0 * idx
    idx  = linLin (control KR "idx" 0) 0 1 0 15
    pan  = linLin (control KR "pan" 0) 0 1 (-1) 1
    dur  = linExp (control KR "dur" 0 + 0.001) 0.001 1.001 0.1 8
    obus = control KR "out" 0

synth_nz01 :: UGen
synth_nz01 = out obus (pan2 osig pan 1)
  where
    osig = resonz (whiteNoise 'W' AR) cf rq * aenv * 0.5
    aenv = envGen KR tr 1 0 dur DoNothing (envPerc 0.01 1)
    tr   = tr_control "t_tr" 0
    dur  = 0.3
    cf   = linControl "cf" 200 8000 1200
    rq   = linControl "rq" 0.1 0.95 0.5
    obus = k "out" 0
    pan  = linControl "pan" (-1) 1 0
    k    = control KR

synth_pv03 :: UGen
synth_pv03 = out obus (pan2 osig pan 1)
    where
      osig = muld
      muld = 0.1 * pv_with2Inputs inA inB pv_MagMul * 0.3
      inA  = mix $ lfSaw AR frqs 0 * 0.1
      frqs = mce [midiCPS (tChoose i tr (mce pchs))|i<-"abcdefg"]
      pchs = foldr (\o acc -> map ((+ofst) . (+o)) degs ++ acc) [] octs
      degs = [0,thrd,7,10]
      thrd = tIRand '3' 3 4 (coinGate '#' (1/15) tr)
      octs = take 5 $ iterate (+12) 33
      ofst = tIRand 'O' (-6) 6 (coinGate 'g' (1/31) tr)
      tr   = control KR "t_tr" 0
      inB  = playBuf 1 AR bufn (bufRateScale KR bufn * rt) 1 0 Loop DoNothing
      rt   = mouseY KR 0.25 4 Exponential 0.1
      bufn = control KR "bufn" 12
      obus = control KR "out" 0
      pan  = linLin (control KR "pan" 0) 0 1 (-1) 1

synth_sin01 :: UGen
synth_sin01 = out obus (pan2 osig pan 1)
  where
    osig = sinOsc AR freq 0 * aenv * 0.3
    aenv = envGen KR tr 1 0 dur DoNothing (envPerc 0.01 1)
    obus = k "out" 0
    freq = k "freq" 440
    dur  = k "dur" 0.2
    pan  = linControl "pan" (-1) 1 0
    tr   = tr_control "t_tr" 0
    k    = control KR

synth_sin02 :: UGen
synth_sin02 = out obus (pan2 osig pan 1)
    where
      osig = sinOsc AR freq 0 * aenv * 0.3
      aenv = envGen KR tr 1 0 dur DoNothing $
             Envelope [0,1,1,0] [0.001,0.099,0.9] [EnvCub] Nothing Nothing
      obus = k "out" 0
      freq = k "freq" 440
      dur  = k "dur" 0.8
      pan  = linLin (k "pan" 0) 0 1 (-1) 1
      tr   = tr_control "t_tr" 1
      k    = control KR

synth_sin03 :: UGen
synth_sin03 = out obus osig
  where
    osig   = pan2 (sinOsc AR (freq+vib) 0 * aenv * 0.3) pan 1
    aenv   = envGen KR tr 1 0 dur DoNothing $
             Envelope [0,1,1,0] [atk,sus,dec] [EnvCub] Nothing Nothing
    vib    = lfTri KR vrate 0 * vlevel
    pan    = linLin (k "pan" 0) 0 1 (-1) 1
    dur    = k "dur" 0.4
    atk    = k "atk" 0.01
    sus    = k "sus" 0.29
    dec    = abs (1 - atk - sus) + 0.001
    freq   = k "freq" 440
    vrate  = k "vrate" 3
    vlevel = k "vlevel" 10
    tr     = tr_control "tr" 1
    obus   = k "out" 0
    k      = control KR

synth_sin04 :: UGen
synth_sin04 = out obus osig
  where
    osig = pan2 (sosc * aenv * 0.3) pan 1
    sosc = sinOsc AR freq 0
    aenv = envGen KR tr 1 0 0.3 DoNothing (envPerc 1e-3 1)
    obus = k "out" 0
    freq = k "freq" 440
    pan  = k "pan" 0
    tr   = tr_control "tr" 0
    k    = control KR

synth_sin05 :: UGen
synth_sin05 = out (kc "out" 0) (pan2 osig pan 1)
  where
    osig = sinOsc AR freq phs * amp * aenv
    pan  = linLin (kc "pan" 0) 0 1 (-1) 1
    freq = kc "freq" 440
    amp  = kc "amp" 1
    aenv = envGen KR tr 1 0 dur DoNothing
           (Envelope [0,1,0] [atk,1-atk] [EnvNum en] Nothing Nothing)
    en   = kc "en" 1
    phs  = kc "phs" 0
    tr   = tr_control "tr" 0
    dur  = kc "dur" 1
    atk  = kc "atk" 1e-4

synth_fm01 :: UGen
synth_fm01 = out obus osig
  where
    osig = pan2 sosc pan 1
    sosc = sinOsc AR freq phs * aenv * 0.3
    aenv = envGen KR tr 1 0 dur DoNothing
           (Envelope [0,1,0] [atk,1-atk] [EnvCub] Nothing Nothing)
    phs  = sinOsc AR (freq*mfac) 0 * idx
    idx  = k "idx" 1
    mfac = k "mfac" 1
    obus = k "out" 0
    freq = k "freq" 440
    atk  = k "atk" 1e-3
    dur  = k "dur" 1
    pan  = k "pan" 1
    tr   = tr_control "tr" 0
    k    = control KR

synth_pls01 :: UGen
synth_pls01 = out (kc "out" 0) (pan2 osig (linLin pan 0 1 (-1) 1) 1)
  where
    osig = pulse AR freq wdth * aenv * 0.3
    freq = kc "freq" 440
    wdth = linLin (sinOsc KR wf 0) (-1) 1 0 0.5
    aenv = envGen KR tr 1 0 dur DoNothing
           (Envelope [0,1,1,0] [atk,sus,1-atk-sus] [EnvCub] Nothing Nothing)
    dur = kc "dur" 0.8
    atk = kc "atk" 1e-3
    sus = kc "sus" 1e-1
    wf  = kc "wf" 0.5
    pan = kc "pan" 0
    tr  = tr_control "tr" 0


-- --------------------------------------------------------------------------
--
-- * Effect synthdefs
--
-- --------------------------------------------------------------------------

-- | Reverb effect with 'allpassN' and foldr.
--
-- Requires /out/ control for specifying audio rate output bus of 'replaceOut',
-- and /in/ control for specifying audio rate input bus.
--
synth_ap01 :: UGen
synth_ap01 = replaceOut (control KR "out" 0) osig
  where
    osig    = wsig * wet + isig * (1-wet)
    wsig    = foldr f isig (zipWith mce2 (rs "abcd") (rs "efgh"))
    f x acc = allpassN acc 0.1 x dcy
    rs      = map (\i -> rand i 0.001 0.05)
    isig    = in' 2 AR (control KR "in" 0)
    wet     = control KR "wet" 0 `lag2` 0.1
    dcy     = linExp (control KR "dcy" 0.2 + 0.001) 0.001 1.001 0.25 8

synth_ap02 :: UGen
synth_ap02 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = foldr f isig $ zip "abcde" "fghij"
    f (r,l) acc =
         let fr i = rand i 0.001 0.1
         in  allpassC acc 0.1 (mce [fr r, fr l]) dcy
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    wet  = k "wet" 0
    dcy  = k "dcy" 0.5
    k    = control KR

-- | Eeffect synth with 'rlpf' and 'rhpf', /rq/ values are shared.
synth_eq02 :: UGen
synth_eq02 = replaceOut (control KR "out" 0) osig
  where
    osig   = wsig * wet + isig * (1-wet)
    wsig   = rlpf isig0 lfreq rq
    isig0  = rhpf isig hfreq rq
    rq     = clip (control KR "rq" 0.5) 0.001 1
    isig   = in' 2 AR (control KR "in" 0)
    lfreq  = linExp (control KR "lfreq" 1 + 0.001) 0.001 1.001 20 17000
    hfreq  = linExp (control KR "hfreq" 0 + 0.001) 0.001 1.001 20 17000
    wet    = control KR "wet" 0

synth_lp01 :: UGen
synth_lp01 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = rlpf isig cf rq
    isig = in' 2 AR ibus
    obus = kc "out" 0
    ibus = kc "in" 0
    wet  = kc "wet" 0
    cf   = kc "cf" 2000
    rq   = kc "rq" 0.3

synth_lpf01 :: UGen
synth_lpf01 = replaceOut (kc "out" 0) osig
  where
    osig = wet * wsig + (1-wet) * isig
    isig = in' 2 AR (kc "in" 0)
    wsig = rlpf isig (kc "cf" 2000) (kc "rq" 0.3)
    wet  = kc "wet" 0

synth_rz01 :: UGen
synth_rz01 = replaceOut (kc "out" 0) osig
  where
    osig = wet * wsig + (1-wet) * isig
    isig = in' 2 AR (kc "in" 0)
    wsig = resonz isig (kc "cf" 2000) (kc "rq" 0.3)
    wet  = kc "wet" 0

synth_cmb01 :: UGen
synth_cmb01 = replaceOut obus osig
  where
    osig = wsig * wet + isig * (1-wet)
    wsig = combC isig 1 dlt dct
    dlt  = control KR "dlt" 0.2
    dct  = control KR "dct" 2
    isig = in' 2 AR (control KR "in" 0)
    wet  = control KR "wet" 0
    obus = control KR "out" 0

synth_cmb02 :: UGen
synth_cmb02 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = combC isig 1 dlt dcy
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    dcy  = k "dcy" 8
    dlt  = k "dlt" 0.2
    wet  = k "wet" 0
    k    = control KR

synth_cmb03 :: UGen
synth_cmb03 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = combC isig 1 dlt dcy
    dlt  = k "dlt" 0.22
    dcy  = k "dcy" 4
    isig = in' 2 AR (k "in" 0)
    wet  = k "wet" 0
    obus = k "out" 0
    k    = control KR

synth_dc01 :: UGen
synth_dc01 = replaceOut obus osig
  where
    osig = wet * wsig + (1-wet) * isig
    wsig = leakDC isig coef
    isig = in' 2 AR ibus
    obus = k "out" 0
    ibus = k "in" 0
    wet  = k "wet" 0
    coef = k "coef" 0.995
    k    = control KR

synth_lmt01 :: UGen
synth_lmt01 = replaceOut (kc "out" 0) osig
  where
    osig = wet * wsig + (1-wet) * isig
    isig = in' 2 AR (kc "in" 0)
    wsig = limiter isig 1 1.0e-2
    wet  = kc "wet" 0

gen_effect :: (UGen -> UGen) -> UGen
gen_effect f = replaceOut (kc "out" 0) osig
  where
    osig = wet * wsig + (1-wet) * isig
    isig = in' 2 AR (kc "in" 0)
    wsig = f isig
    wet  = kc "wet" 0

synth_muladd :: UGen
synth_muladd = gen_effect $ \isig ->
    let mul = kc "mul" 1
        add = kc "add" 0
    in  mulAdd isig mul add

synth_clip2 :: UGen
synth_clip2 = gen_effect $ \isig -> clip2 isig (kc "clip" 1)

-- --------------------------------------------------------------------------
--
-- * Auxiliary
--
-- --------------------------------------------------------------------------

linControl :: String -> Double -> Double -> Double -> UGen
linControl name minv maxv iniv =
    let iniv' = (iniv - minv) / (maxv - minv)
        c     = constant
    in  linLin (control KR name iniv') 0 1 (c minv) (c maxv)

pv_with2Inputs :: UGen -> UGen -> (UGen -> UGen -> UGen) -> UGen
pv_with2Inputs sigA sigB fpv = osig
  where
    osig = ifft' $ fpv chainA chainB
    chainA = f 'x' sigA
    chainB = f 'y' sigB
    f i sig = fft' (mrg2 (localBuf i 2048 1) (maxLocalBufs 2)) sig

kc :: String -> Double -> UGen
kc = control KR

synth_bypass :: UGen
synth_bypass = mrg outs
  where
    outs = [control KR [n] 0| n <- ns]
    ns   = ['0'..'z']

-- | Triggers when input signal has changed.
--
-- > changed i threshold = abs (hpz1 i) >* threshold
--
changed :: UGen -> UGen -> UGen
changed i thres = abs (hpz1 i) >* thres
