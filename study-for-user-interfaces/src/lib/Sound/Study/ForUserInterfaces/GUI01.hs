{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Graphical user interface, take 1.

-}
module Sound.Study.ForUserInterfaces.GUI01 where

import           Control.Concurrent (forkIO, killThread)
import           Control.Monad (forM, unless, void)
import           Control.Monad.Reader (runReaderT)
import           Data.Maybe (catMaybes)
import           Text.Printf (printf)

import           Sound.OSC.FD
import           Sound.SC3.FD
import           Sound.SC3.TH.Synthdef (synthdefGenerator)
import           Sound.SC3.Tree
import           Sound.SC3.UGen.ID

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (stepper)

import qualified Graphics.UI.Threepenny.Extra as Extra


-- --------------------------------------------------------------------------
--
-- * UI
--
-- --------------------------------------------------------------------------

-- | Main entry point.
main :: IO ()
main = withSC3 $ \fd -> do
    mapM_ (\x -> async fd $ b_alloc x 32 1) [0..11]

    -- buffers for grouping mute/unmute
    mapM_ (\x -> async fd $ b_alloc x 12 1) [100,101,102]

    mapM_ (async fd . d_recv) synthdefs
    runReaderT (patchNode $ nodify nodes) fd
    static <- Extra.getStaticDir
    tid <- forkIO $ startGUI defaultConfig {tpStatic=Just static} (setup fd)
    getChar >> killThread tid

-- | Node arrangement for GUI.
nodes :: Nd
nodes =
  let t00 = syn "trig00" ["out"*=100,"bpm"*=128,"beat"*=4]
      rb o b = syn "rbufrd01" ["out"*=o,"bufn"*=b,"len"*=32,"tr0"*<-t00-*"out"]
      t01 = rb 101 0
      t02 = rb 102 1
      t03 = rb 103 2
      t04 = rb 104 3
      t05 = rb 105 4
      t06 = rb 106 5
  in  grp 0
      [ grp 1
        [ grp 10
          [ t00, t01, t02, t03, t04, t05, t06 ]
        , grp 11
          [ syn "bd01"
            ["out"*=0,"freq"*=70,"dur"*=0.12,"t_tr0"*<-t01-*"out"]
          , syn "hat01"
            ["out"*=1,"t_tr0"*<-t02-*"out"]
          , syn "snr01"
            ["out"*=2,"t_tr0"*<-t03-*"out"]
          , syn "prc01"
            ["out"*=3,"t_tr0"*<-t04-*"out"]
          , syn "nz01"
            ["out"*=4,"t_tr0"*<-t05-*"out"]
          , syn "add01"
            ["out"*=5,"tr1"*<-t00-*"out","faf"*=9.45,"hps"*=2.80]
          , syn "saw01"
            ["out"*=6,"tr1"*<-t00-*"out","cfhi"*=7562,"ftrr"*=0.1]
          , syn "sine01"
            ["out"*=7,"t_tr0"*<-t06-*"out"]
          ]
        , grp 12
          [ syn "mixer01" [] ]
        ]
      , grp 2 []
      ]

-- | Setup GUI with given 'Transport'.
setup :: Transport t => t -> Window -> UI ()
setup fd window = do
    void $ return window # set title "gui 01"
    UI.addStyleSheet window "ui.css"

    currentNodes <- liftIO $ runReaderT getRootNode fd
    let queryByName name =
            maybe (error (name ++ " not found")) id $
            queryN' (synthName ==? name) currentNodes
        queryParam name node =
            case queryP' (paramName ==? name) node of
                Just (name' := val) | name == name' -> val
                _                                   ->
                    error $ unwords ["no", name, "found in", show node]
        tr00node = queryByName "trig00"
        tr00nid  = nodeId tr00node
        add01node = queryByName "add01"
        add01nid  = nodeId add01node
        saw01node = queryByName "saw01"
        saw01nid  = nodeId saw01node
        mixer01node = queryByName "mixer01"
        mixer01nid  = nodeId mixer01node

    -- status div
    (tmr,stDiv) <- Extra.statusDiv fd

    let vr ::
            String -> Int -> Double -> Double -> Double -> UI Element
        vr l nid minv maxv iniv =
            Extra.vslider l 30 128 minv maxv iniv $ \v -> do
                liftIO $ send fd $ n_set nid [(l,v)]
                return $ printf "%3.2f" v

    -- common trigger
    let iniBpmVal  = queryParam "bpm" tr00node
        iniBeatVal = queryParam "beat" tr00node
    bpm <- Extra.textbox "bpm" 50 (show (ceiling iniBpmVal :: Int)) $ \v -> do
        let vs = reads v
        unless (null vs) $ liftIO $
            send fd $ n_set tr00nid [("bpm",fst $ head vs)]
    beat <- Extra.hslider "beat (2**(v/2))" 128 20 0 16 iniBeatVal $ \v -> do
        liftIO $ send fd $ n_set tr00nid [("beat",2**(v/2))]
        return $ show v

    -- add01
    let iniFafVal = queryParam "faf" add01node
        iniHpsVal = queryParam "hps" add01node
    add01faf <- vr "faf" add01nid 0.2 30
                (read (printf "%3.2f" iniFafVal) :: Double)
    add01hps <- vr "hps" add01nid 0.1 10
                (read (printf "%3.2f" iniHpsVal) :: Double)

    -- saw01
    let xysiz :: Num a => a
        xysiz = 128
    saw01xy <- Extra.xyarea "cfhi x ftrr" xysiz $ \(x,y) -> do
        let x' = (fromIntegral x / xysiz) * 8000
            y' = fromIntegral y / xysiz
        liftIO $ send fd $ n_set saw01nid [("cfhi",x'),("ftrr",y')]
        return $ printf "(%3.2f,%3.2f)" x' y'

    -- buffer
    (gridboxes, _boxes) <- Extra.toggleGrids 32 12 $ \(i,j) val ->
        liftIO $ send fd $ b_set i [(j, fromIntegral val)]

    -- effects
    revrmix <- vr "rmix" mixer01nid 0 1 0
    revdamp <- vr "damp" mixer01nid 0 1 0
    revroom <- vr "room" mixer01nid 0 1 0
    cf_x_rq <- Extra.xyarea "cf x rq" xysiz $ \(x,y) -> do
        let x' = (exp ((fromIntegral x + 0.001) / xysiz) - 1) * 10000
            y' = fromIntegral y / xysiz
        liftIO $ send fd $ n_set mixer01nid [("cf",x'),("rq",y')]
        return $ printf "(%3.2f,%3.2f)" x' y'

    -- mixer
    let divClear = UI.div # set style [("clear","both")]
        vrm :: String -> Int -> Double -> Double -> Double -> UI Element
        vrm l n minv maxv iniv = vr (l++show n) mixer01nid minv maxv iniv
        amp_x_pan n = do
            let fpan v = do
                    liftIO $ send fd $ n_set mixer01nid [("pos"++show n,v)]
                    return ""
                feff v =
                    liftIO $ send fd $
                    n_set mixer01nid [("efx"++show n,if v then 1 else 0)]
                fmute k v = liftIO $ do
                    let v' = if v then 1 else 0
                    send fd $ b_setn1 (100+k) n [v']
            UI.new #+
                [ Extra.toggleBox "" 15 15 (fmute 0)
                  # set style [("float","center")
                              ,("margin","5px auto 0 auto")]
                , divClear
                , Extra.toggleBox "" 15 15 (fmute 1)
                  # set style [("float","center")
                              ,("margin","5px auto 0 auto")]
                , divClear
                , Extra.toggleBox "" 15 15 (fmute 2)
                  # set style [("float","center")
                              ,("margin","5px auto 0 auto")]
                , divClear
                , Extra.hslider "pan" 40 12 (-1) 1 0 fpan
                  # set style [("float","left")
                              ,("margin-bottom","5px")
                              ]
                , divClear
                , Extra.toggleBox "fx" 15 15 feff
                  # set style [("float","center")
                              ,("margin","5px auto 0 auto")]
                , divClear
                , vrm "amp" n (-60) 25 0
                  # set style [("float","center")
                              ,("margin","5px auto 15px auto")
                              ]
                ] # set style [("float","left")]

    -- mutes
    mutes <- do
        radios <- forM [100,101,102::Int] $ \n -> do
            radio <- UI.input
                # set UI.type_ "radio"
                # set UI.name "set"
                # set UI.value (show n)
            on UI.click radio $ \_ -> do
                bufn <- radio # get UI.value
                liftIO $ do
                    vals <- queryBuffer (read bufn) fd
                    sendOSC fd $ n_set mixer01nid $
                        zipWith (\i v -> ("mute"++show i,realToFrac v))
                        [(0::Int)..] vals
            return radio
        lagt <- Extra.textbox "lag" 50 "4" $ \str -> do
            let val = reads str
            unless (null val) $ liftIO $
                send fd $ n_set mixer01nid [("lagt", fst (head val))]
        UI.new #+ [ element lagt
                    # set style [("float","center")]
                  , divClear
                  , UI.new
                    #+ map element radios
                    # set style [("float","center")
                                ,("margin","5px")
                                ]
                  ]
            -- # set style [("float","left")]

    mstr <- UI.new #+
            ( UI.div #+
              [ element mutes
              , vr "mamp" mixer01nid (-60) 25 0
                # set style [("float","center")
                            ,("margin","5px auto")
                            ]
              ] # set style [("float", "left")]
            : map amp_x_pan [0..11])

    -- layout --
    mapM_ (\e -> element e # set style [("float","left")])
        [bpm, beat, add01faf, add01hps, revrmix, revroom, revdamp, saw01xy]

    void $ getBody window #+
        [ element stDiv
        , UI.new #
          set style
          [("margin","0 auto"),("width", "980px"),("padding","5px")] #+
          [ UI.new #
            set style
            [("float","left")
            ,("padding-bottom","5px"),("margin-bottom","5px")] #+
            [ element bpm, element beat ]
          , divClear
          , UI.new #
            set style
            [("float","left")
            ,("padding-bottom","5px"),("margin-bottom","5px")] #+
            [ element gridboxes ]
          , divClear
          , UI.new #
            set style
            [("float","left")
            ,("padding-bottom","5px"),("margin-bottom", "5px")] #+
            [ element add01faf, element add01hps
            , element saw01xy
            , element revrmix, element revdamp, element revroom
            , element cf_x_rq
            ]
          , divClear
          , UI.new #
            set style [("padding-bottom","5px"),("float","left")] #+
            [ element mstr ]
          , divClear
          ]
        ]
    UI.start tmr


-- --------------------------------------------------------------------------
--
-- * Synth
--
-- --------------------------------------------------------------------------

-- | Trigger to sync other synths.
synth_trig00 :: UGen
synth_trig00 = out (control KR "out" 0) sig0
  where
    sig0 = impulse KR freq 0
    freq = (beat*bpm)/60
    beat = control KR "beat" 4
    bpm  = control KR "bpm" 128

-- | 'bufRdN' with 'coinGate'.
synth_rbufrd01 :: UGen
synth_rbufrd01 = out (control KR "out" 0) sig
  where
    sig  = coinGate 'g' (bufRdN 1 KR bufn idx Loop) tr0
    bufn = control KR "bufn" 100
    idx  = gate (stepper tr0 0 0 (len-1) 1 0) tr0
    len  = control KR "len" 16
    tr0  = control KR "tr0" 1

-- | Simple additive synth.
synth_add01 :: UGen
synth_add01 = out (control KR "out" 0) sig0
  where
    sig0 = mix (mce fs) * aenv
    fs   = foldr (\x acc -> fa x * sinOsc AR (x*freq) 0 : acc) [] [1..9]
    freq = lag (select idx (mce pchs)) 0.005
    idx  = pulseCount tr1 tr2 `modE` (constant (length pchs))
    pchs = map midiCPS degs
    degs = map (+off) [36,48,60,72, 48,60,72,84]
    off  = tChoose 'o' tr3 (mce [-12,-7,-5,0,5,7,12])
    tr2  = coinGate 'g' 0.25 tr1
    tr3  = coinGate 'o' (1/32) tr1
    fa c = lfdNoise3 c KR (lfdNoise3 (c+100) KR 1 * faf + faf) ** 2
    aenv = envGen AR tr0 amp 0 dur DoNothing ash
    amp  = tExpRand 'a' 0.5 1 tr0 * 0.3
    ash  = envCoord [(0,0),(atk,1),(sus,1),(1,0)] 1 1 EnvCub
    atk  = tExpRand 'A' 0.01 0.999 tr0
    sus  = tExpRand 's' 0.01 (1-atk) tr0
    tr0  = coinGate 'G' 0.8 (impulse KR hps 0)
    dur  = recip hps
    faf  = control KR "faf" 9.23 `lag` 0.1
    hps  = control KR "hps" 0.3 `lag` 0.1
    tr1  = control KR "tr1" 0

-- | Simple saw tooth oscillator.
synth_saw01 :: UGen
synth_saw01 = out (control KR "out" 1) sig0
  where
    sig0     = rlpf sig1 cf rq * aenv * 0.3
    sig1     = mix (saw AR (mce [freq+vib, freq*0.998, freq*1.002]))
    freq     = select idx (mce pchs)
    vib      = lfTri KR 5.21 0 * 2.23
    idx      = pulseCount tr1 tr2 `modE` (constant (length pchs))
    pchs     = map midiCPS degs
    degs     = [36,67,84,38, 67,65,36,84]
    cf       = lag2 (f0 'c' 50 cfhi) 0.5
    rq       = lag2 (f0 'r' 0.05 0.95) 0.25
    f0 c l h = gate (linLin (lfdNoise3 c KR 3) (-1) 1 l h) zo0
    aenv     = envGen KR tr3 1 0 0.2 DoNothing ash
    ash      = envCoord [(0,0),(1e-4,1),(0.5,0.2)] 1 1 EnvCub
    zo0      = toggleFF (dust 'D' KR 1)
    tr1      = control KR "tr1" 0
    tr2      = coinGate 'g' ftrr tr1
    tr3      = coinGate 'G' 0.38 tr1
    cfhi     = control KR "cfhi" 8000 `lag` 0.1
    ftrr     = control KR "ftrr" (1/16) `lag` 0.1

-- | Simple bass drum sound.
synth_bd01 :: UGen
synth_bd01 = out (control KR "out" 0) sig0
  where
    sig0  = (resonz sig12 (freq*2) 0.98 + sig3) * amp
    sig12 = rlpf (mix (sig1+sig2)) cf rq
    sig1  = sinOsc AR (freq*12.32) phase * aenv0 * 0.1
    sig2  = saw AR freqs * aenv1 * 0.8
    sig3  = lpf (whiteNoise 'W' AR) 8000 * trig1 tr0 0.002 * 0.1
    phase = sinOsc AR (freq*5.29) 0 * aenv0 * 9
    aenv0 = envGen KR tr0 1 0 0.01 DoNothing $ ash0
    aenv1 = envGen KR tr0 1 0 dur DoNothing $ ash0
    ash0  = envCoord [(0,0),(1e-4,1),(4e-1,0.8),(1,0)] 1 1 EnvCub
    freqs = mce $ map (*freq) [1, 1.32, 1.732, 2.79]
    cf    = 20000 * aenv0 + freq
    rq    = 0.99
    freq  = control KR "freq" 30
    amp   = tExpRand 'a' 0.7 1 tr0
    dur   = control KR "dur" 0.25
    tr0   = tr_control "t_tr0" 1

-- | Simple hat sound.
synth_hat01 :: UGen
synth_hat01 = out (control KR "out" 0) (mix sig0)
  where
    sig0  = (sig1 + sig3)
    sig1  = rhpf sig2 8000 aenv0 * aenv0
    sig2  = pulse AR (freq * pulse AR (freq*1.83) 0.5) 0.5
    sig3  = rhpf sig2 9000 aenv2 * aenv2 * 0.5
    freq  = mce [803, 1729, 2532, 3783]
    aenv0 = envGen KR tr0 amp 0 dur DoNothing ash0 * aenv1
    ash0  = envPerc 1e-3 1
    aenv1 = lfdNoise3 'A' KR aef1 * 0.5 + 0.5
    aenv2 = decay2 tr0 1e-3 0.2
    aef1  = decay2 tr0 1e-2 3 * 10
    amp   = tExpRand 'a' 0.8 1 tr0
    dur   = 0.1
    tr0   = tr_control "t_tr0" 1

-- | Simple snare sound.
synth_snr01 :: UGen
synth_snr01 = out (control KR "out" 0) sig0
  where
    sig0  = (sig1*aenv1 + sig2*aenv2) * 0.2
    sig1  = rhpf (whiteNoise 'W' AR) cf rq
    cf    = aenv1 * 2000 + 50
    rq    = 0.9
    aenv1 = decay2 tr0 1e-4 0.3
    sig2  = mix (sinOsc AR freqs 0) * 0.8
    freqs = mce [128.31,179.9,258.12,371.3] * lfCub KR 193.32 pi
    aenv2 = envGen KR tr0 1 0 0.1 DoNothing ash2
    ash2  = envCoord [(0,0),(1e-2,1),(0.2,0.8),(1,0)] 1 1 EnvCub
    tr0   = tr_control "t_tr0" 1

-- | Simple percussive sound.
synth_prc01 :: UGen
synth_prc01 = out (control KR "out" 0) sig0
  where
    sig0  = mix (rhpf sig2 (freqs*0.759) (aenv1+1e-3)) * aenv0 * 0.1
    freqs = mce [  889 + (lfdNoise3 'a' KR (1/30) * 200)
                , 1829 + (lfdNoise3 'b' KR (1/60) * 800)
                , 2803 + (lfdNoise3 'c' KR (1/120) * 1820)
                , 5231 + (lfdNoise3 'd' KR (1/240) * 3800)
                ]
    sig2  = whiteNoise 'W' AR
    aenv0 = envGen KR tr0 1 0 dur DoNothing ash0
    ash0  = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub
    aenv1 = decay2 tr0 1e-4 0.1
    dur   = 0.3
    tr0   = tr_control "t_tr0" 1

-- | Simple filtered white noise sound.
synth_nz01 :: UGen
synth_nz01 = out (control KR "out" 0) sig0
  where
    sig0  = mix (resonz sig1 cf rq) * aenv0 * 0.1
    sig1  = whiteNoise 'ω' AR
    fenv  = envGen KR tr0 1 0 dur DoNothing
    aenv0 = fenv ash0
    ash0  = Envelope [0,1,1,0] [0.7,0.2,0.1] [EnvNum 2] Nothing Nothing
    cf    = mce [2000,4100,6300,8800] * fenv0
    fenv0 = fenv fsh0
    fsh0  = Envelope [0.001,1] [1] [EnvExp] Nothing Nothing
    rq    = aenv0 * 3 + 0.001
    dur   = 0.4
    tr0   = tr_control "t_tr0" 1

-- | Simple sines, percussive chords with impulse triggers.
synth_sine01 :: UGen
synth_sine01 = out (control KR "out" 0) sig0
  where
    sig0 = mix (sinOsc AR freq 0) * aenv * 0.06
    freq = select (mce idx) (mce pchs)
    idx  = [tIRand n 0 (constant $ length pchs - 1) tr0 | n <-"alskdf*&%"]
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    octs = take 5 $ iterate (+12) 48
    degs = [0,2,4,5,7,11]
    aenv = decay2 tr1 1e-4 dur
    dur  = 0.125
    tr1  = impulse KR df 0 + tr0
    df   = linLin (lfdNoise0 'f' KR dff) (-1) 1 1 32
    dff  = tExpRand 'd' 1 8 tr0
    tr0  = tr_control "t_tr0" 1

-- | Simple mixer.
synth_mixer01 :: UGen
synth_mixer01 = replaceOut 0 (sigs0 * dbAmp mamp)
  where
    sigs0 = efx (sum sigsA) + sum sigsB
    (sigsA, sigsB) = unzip $ map f [0..12::Int]
    f n   = (sig0*efxc, sig0*(1-efxc))
      where
        sig0 = pan2 sig1 posn (dbAmp ampn) * mute
        sig1 = in' 1 AR inn
        inn  = k ("in" ++ show n) (fromIntegral n)
        posn = k ("pos" ++ show n) 0
        ampn = k ("amp" ++ show n) 0.5
        efxc = k ("efx" ++ show n) 0
        mute = control KR ("mute" ++ show n) 1 `lag3` lagt
        lagt = control KR "lagt" 2
    efx x = freeVerb sig0 rmix room damp
      where
        sig0 = rlpf x cf rq
    mamp  = k "mamp" 0
    rmix  = k "rmix" 0.33
    room  = k "room" 0.5
    damp  = k "damp" 0.5
    cf    = k "cf" 2800
    rq    = k "rq" 0.999
    k n v = control KR n v `lag` 0.2

-- | All 'Synthdef's starting with /synth_/ in this module.
synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

-- --------------------------------------------------------------------------
--
-- * Miscellaneous
--
-- --------------------------------------------------------------------------

-- | Query buffer values.
--
-- >>> let vs = [0,0,1,0, 1,0,1,0.5, 0,0,1,0, 0.5,1,0.5,1] :: [Float]
-- >>> let b  = 100 :: Int
-- >>> withSC3 $ \fd -> async fd (b_free b)
-- >>> withSC3 $ \fd -> send fd (b_alloc_setn1 b 0 (map realToFrac vs)
-- >>> vs' <- withSC3 $ queryBuffer b
-- >>> vs' == vs
-- True
--
queryBuffer ::
    Transport t
    => Int -- ^ Buffer number.
    -> t
    -> IO [Float]
queryBuffer bufn fd = do
    send fd $ b_query [bufn]
    (_:Int32 buflen:_) <- waitDatum fd "/b_info"
    send fd $ b_getn bufn [(0,fromIntegral buflen)]
    (_:_:_:vals) <- waitDatum fd "/b_setn"
    return $ catMaybes $ map d_get vals
