{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Graphical user interface for synths, take 1.

-}
module Sound.Study.ForUserInterfaces.GUI01 where

import           Control.Monad (forM, unless, void, zipWithM_)
import           Control.Monad.Reader (runReaderT)
import           Data.Maybe (catMaybes)
import           System.Random
import           Text.Printf (printf)

import           Sound.OSC.FD
import           Sound.SC3.FD
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
    mapM_ (async fd) $
        [ b_alloc 100 16 1
        , b_alloc 101 16 1
        ]
    mapM_ (async fd . d_recv . uncurry synthdef)
         [ ("trig00",trig00)
         , ("add01",add01)
         , ("osc02",osc02)
         , ("rbufrd01",rbufrd01)
         , ("bd01",bd01)
         , ("hat01",hat01)
         , ("mixer01",mixer01)
         ]
    runReaderT (patchNode $ nodify nodes) fd
    static <- Extra.getStaticDir
    startGUI defaultConfig {tpStatic=Just static} (setup fd)

nodes :: Nd
nodes =
  let t00 = syn "trig00" ["out"*=100]
      t01 = syn "rbufrd01" ["out"*=101,"bufn"*=100,"len"*=16,"tr0"*<-t00-*"out"]
      t02 = syn "rbufrd01" ["out"*=102,"bufn"*=101,"len"*=16,"tr0"*<-t00-*"out"]
      sadd01 = syn "add01" ["out"*=0,"tr1"*<-t00-*"out" ]
      sosc02 = syn "osc02" ["out"*=1,"tr1"*<-t00-*"out"]
      sbd01 = syn "bd01" ["out"*=2,"freq"*=70,"dur"*=0.12,"t_tr0"*<-t01-*"out"]
      shat01 = syn "hat01" ["out"*=3,"t_tr0"*<-t02-*"out"]
      smix = syn "mixer01" []
  in  grp 0
      [ grp 1
        [ t00, t01, t02 ]
      , grp 2
        [ sadd01
        , sosc02
        , sbd01
        , shat01 ]
      , grp 3
        [ smix ]
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
        tr00node = queryByName "trig00"
        tr00nid  = nodeId tr00node
        add01node = queryByName "add01"
        add01nid  = nodeId add01node
        osc02node = queryByName "osc02"
        osc02nid  = nodeId osc02node
        mixer01node = queryByName "mixer01"
        mixer01nid  = nodeId mixer01node

    -- status div --
    (tmr,stDiv) <- Extra.statusDiv fd

    let vr :: String -> Int
              -> Double -> Double -> Double -> Double -> UI Element
        vr l nid minv maxv stepv iniv =
            Extra.vrange l minv maxv stepv iniv $ \v -> do
                liftIO $ send fd $ n_set nid [(l,v)]
                return $ printf "%3.2f" v

    -- common trigger --
    bpm <- Extra.textbox "bpm" 50 "128" $ \v -> do
        let vs = reads v
        -- unless (null vs) $ liftIO $ send fd $ n_set 999 [("bpm",fst $ head vs)]
        unless (null vs) $ liftIO $
            send fd $ n_set tr00nid [("bpm",fst $ head vs)]
    beat <- Extra.hrange "beat (2**(v/4))" 0 32 1 8 $ \v -> do
        liftIO $ send fd $ n_set tr00nid [("beat",2**(v/4))]
        -- liftIO $ send fd $ n_set 999 [("beat",2**(v/4))]
        return $ show v
    mapM_ (\e -> element e # set style [("float","left")]) [bpm, beat]

    -- add01 --
    -- add01faf <- vr "faf" 2000 0.2 30 0.05 5
    add01faf <- vr "faf" add01nid 0.2 30 0.05 5
    -- add01hps <- vr "hps" 2000 0.1 10 0.05 0.3
    add01hps <- vr "hps" add01nid 0.1 10 0.05 0.3

    -- osc01 --
    let xysiz :: Num a => a
        xysiz = 128
    osc02xy <- Extra.xyarea "cfhi x ftrr" xysiz $ \(x,y) -> do
        let x' = (fromIntegral x / xysiz) * 8000
            y' = fromIntegral y / xysiz
        -- liftIO $ send fd $ n_set 1001 [("cfhi",x'),("ftrr",y')]
        liftIO $ send fd $ n_set osc02nid [("cfhi",x'),("ftrr",y')]
        return $ printf "(%3.2f,%3.2f)" x' y'

    -- buffer --
    let hcheckbox ::
            String
            -> Int
            -> (Int -> Bool -> UI ())
            -> UI (Element, [Element])
        hcheckbox lbl n act = do
            checks <- forM [0..n-1] $ \m -> do
                cbox <- UI.input # set UI.type_ "checkbox"
                on UI.checkedChange cbox $ act m
                return cbox
            lbldiv <- UI.div
                # set text lbl
                # set style [("float","left")
                            ,("font-size","10px")
                            ,("padding","4px 5px 0 5px")
                            ]
            wrapper <- UI.div
                #+ (element lbldiv : map element checks)
                # set style [("padding","10px 10px 0 10px")
                            ]
            return (wrapper, checks)
    let bufbox bnum = do
            let lbl = "buf " ++ show bnum
            (wrapper,chks) <- hcheckbox lbl 16 $ \n isChecked ->
                liftIO $ send fd $ b_set bnum [(n,if isChecked then 1 else 0)]
            vals <- liftIO $ queryBuffer bnum fd
            zipWithM_ (\c v -> element c # set UI.checked (v==1)) chks vals
            return wrapper

    chk100 <- bufbox 100
    chk101 <- bufbox 101

    -- mixer --
    mamp <- vr  "mamp" mixer01nid (-60) 25 0.1 0
    let vrm :: String -> Int -> Double -> Double -> Double -> UI Element
        vrm l n minv maxv iniv = vr (l++show n) mixer01nid minv maxv 0.01 iniv
        pos_x_amp n = do
            pos <- vrm "pos" n (-1) 1 0
            amp <- vrm "amp" n (-60) 25 0
            return [pos,amp]
    pos_x_amps <- concat <$> mapM pos_x_amp [0..3]
    mstr <- UI.new #+ map element (mamp : pos_x_amps)

    -- layout --
    mapM_ (\e -> element e # set style [("float","left")])
        ([bpm, add01faf, add01hps, osc02xy, chk100, chk101] ++ pos_x_amps)
    let divClear = UI.div # set UI.clear_ "both"
    void $ getBody window #+
        [ element stDiv
        , UI.new #
          set style
          [("margin","0 auto"),("width", "960px"),("padding","20px")] #+
          [ UI.new #
            set style
            [("padding","5px"),("float", "left"),("margin-bottom","10px")] #+
            [element bpm, element beat]
          , divClear
          , UI.new #
            set style
            [("padding","5px"),("float","left"),("margin-bottom", "10px")] #+
            [element add01faf, element add01hps , element osc02xy
            ,element chk100, element chk101
            ]
          , divClear
          , UI.new #
            set style [("padding","5px"),("float","left")] #+
            [element mstr]
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
trig00 :: UGen
trig00 = out (control KR "out" 0) sig0
  where
    sig0 = impulse KR freq 0
    freq = (beat*bpm)/60
    beat = control KR "beat" 4
    bpm  = control KR "bpm" 128

-- | Simple additive synth.
add01 :: UGen
add01 = out (control KR "out" 0) sig0
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
osc02 :: UGen
osc02 = out (control KR "out" 1) sig0
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

-- | 'bufRdN' with 'coinGate'.
rbufrd01 :: UGen
rbufrd01 = out (control KR "out" 0) sig
  where
    sig   = coinGate 'g' (bufRdN 1 KR bufn idx Loop) tr0
    bufn  = control KR "bufn" 100
    idx   = gate (stepper tr0 0 0 (len-1) 1 0) tr0
    len   = control KR "len" 16
    tr0   = control KR "tr0" 1

-- | Simple bass drum sound.
bd01 :: UGen
bd01 = out (control KR "out" 0) sig0
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
hat01 :: UGen
hat01 = out (control KR "out" 0) (mix sig0)
  where
    sig0  = (sig1 + sig3)
    sig1  = rhpf sig2 8000 aenv0 * aenv0
    sig2  = pulse AR (freq * pulse AR (freq*1.83) 0.5) 0.5
    sig3  = rhpf sig2 9000 aenv2 * aenv2 * 0.5
    freq  = mce [803, 1729, 2532, 3783]
    aenv0 = envGen KR tr0 1 0 dur DoNothing ash0 * aenv1
    ash0  = envPerc 1e-3 1
    aenv1 = lfdNoise3 'A' KR aef1 * 0.5 + 0.5
    aenv2 = decay2 tr0 1e-3 0.2
    aef1  = decay2 tr0 1e-2 3 * 10
    dur   = 0.1
    tr0   = tr_control "t_tr0" 1

-- Change "len" in group 1.
play_percs :: IO ()
play_percs = withSC3 $ \fd -> do
    mapM_ (async fd . d_recv . uncurry synthdef)
        [("rbufrd01",rbufrd01)
        ,("bd01",bd01), ("hat01",hat01)
        ]
    sendOSC fd $ bundle immediately
        [ s_new "trig00" 998 AddToHead 1 [("out",101),("beat",4),("bpm",128)]
        , s_new "rbufrd01" 2000  AddToTail 1
          [("out",102),("bufn",100),("len",16)]
        , n_map 2000 [("tr0",101)]
        , s_new "bd01" 2001 AddToTail 1
          [("out",0),("freq",69),("dur",0.18)]
        , n_map 2001 [("t_tr0",102)]
        , s_new "rbufrd01" 2002 AddToTail 1
          [("out",103),("bufn",101)]
        , n_map 2002 [("tr0",101)]
        , s_new "hat01" 2003 AddToTail 1
          [("out",0)]
        , n_map 2003 [("t_tr0",103)]
        ]

alloc_buf_100 :: IO Message
alloc_buf_100 = withSC3 $ \fd ->
    async fd $ b_alloc_setn1 100 0 $
    [1,1,0,1, 1,0,1,1, 0,1,1,0, 1,0,0,0.5]
    -- [1,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,1,0]

alloc_buf_101 :: IO Message
alloc_buf_101 = withSC3 $ \fd ->
    async fd $ b_alloc_setn1 101 0 $
    [1,0,0,0.8, 1,0,0,0, 0.8,0.3,0,0, 1.2,0,0,0]

alloc_buf_102 :: IO Message
alloc_buf_102 = withSC3 $ \fd ->
    async fd $ b_alloc_setn1 102 0 $
    [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0]

sb_r :: Int -> Double -> [Double] -> IO ()
sb_r bufn offset ds = withSC3 $ \fd -> do
    g <- newStdGen
    let is = take 16 $ randomRs (0,length ds-1) g
    send fd $ b_setn1 bufn 0 $ [(ds!!i)+offset|i<-is]

sb_100r1, sb_100r2, sb_100r3, sb_100r4 :: IO ()
sb_100r1 = sb_r 100 0 [0.1,0.2..1]
sb_100r2 = sb_r 100 0 [0,0.5,1]
sb_100r3 = sb_r 100 0 [0,1]
sb_100r4 = sb_r 100 0 [1]

sb_101r1, sb_101r2, sb_101r3, sb_101r4, sb_101r5 :: IO ()
sb_101r1 = sb_r 101 0 [0,0.5,0.8,1]
sb_101r2 = sb_r 101 0 [0.1,0.2..1]
sb_101r3 = sb_r 101 0 [0,1]
sb_101r4 = sb_r 101 0 [0,0,0.5,1]
sb_101r5 = sb_r 101 0 [1]

sb_102r1, sb_102r2, sb_102r3, sb_102r4 :: IO ()
sb_102r1 = sb_r 102 0 [0,0.5,0.8,1]
sb_102r2 = sb_r 102 0 [0.1,0.2..1]
sb_102r3 = sb_r 102 0 [0,1]
sb_102r4 = sb_r 102 0 [0,0,0.5,1]

-- | Simple mixer.
mixer01 :: UGen
mixer01 = replaceOut 0 (sigs * dbAmp mamp)
  where
    sigs = sum $ map f [0..3::Int]
    f n  = pan2 (in' 1 AR inn) posn (dbAmp ampn)
      where
        inn  = control KR ("in" ++ show n) (fromIntegral n)
        posn = control KR ("pos" ++ show n) 0
        ampn = control KR ("amp" ++ show n) 0.5
    mamp = control KR "mamp" 0


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

-- | From hsc3 help of 'stepper'.
stepper_ex :: IO ()
stepper_ex =
    let {compose = foldl (flip (.)) id
        ;rvb z s =
            let f i = let dly = mce [rand (z `joinID` i `joinID` 'a') 0 0.5
                                    ,rand (z `joinID` i `joinID` 'b') 0 0.5]
                        in allpassN i 0.05 dly (rand i 1.5 2)
            in compose (replicate 5 f) s
        ;stpr = let {rate = mouseX KR 1.75 2.25 Exponential 0.1
                    ;clock = impulse KR rate 0
                    ;envl = decay2 clock 0.002 2.5
                    ;indx = stepper clock 0 0 15 1 0
                    ;freq = bufRdN 1 KR 10 indx Loop
                    ;ffreq = lag2 freq 0.1 + mce [0,0.3]
                    ;lfo = sinOsc KR 0.2 (mce [0,pi/2]) * 0.0024 + 0.0025
                    ;top = mix (lfPulse AR (freq * mce [1,1.5,2]) 0 0.3)
                    ;chn = [\s -> rlpf s ffreq 0.3 * envl
                           ,\s -> rlpf s ffreq 0.3 * envl
                           ,\s -> s * 0.5
                           ,\s -> combL s 1 (0.66 / rate) 2 * 0.8 + s
                           ,\s -> s + (rvb 'a' s * 0.3)
                           ,\s -> leakDC s 0.1
                           ,\s -> delayL s 0.1 lfo + s
                           ,\s -> onePole s 0.9]}
                in compose chn top
        ;stprInit fd =
         let n = [97.999,195.998,523.251,466.164,195.998
                 ,233.082,87.307,391.995,87.307,261.626
                 ,195.998,77.782,233.082,195.998,97.999
                 ,155.563]
         in do {_ <- async fd (b_alloc 10 128 1)
               ;send fd (b_setn 10 [(0,n)])}}
    in withSC3 $ \fd -> (stprInit fd >> play fd (out 0 stpr))

-- | Simple 'sendTrig' example to schedule 's_new' message responding to @/tr@
-- messages sent back from scsynth server.
go_sendTrig01 :: IO ()
go_sendTrig01 = withSC3 $ \fd' -> withNotifications fd' $ \fd -> do
    let st01  = sendTrig tr0 1000 tcnt
        tr0   = tr1 + dust 'A' KR df
        tr1   = impulse KR 1 0
        df    = linLin (lfdNoise3 'F' KR (1/32)) (-1) 1 0.5 32
        tcnt  = pulseCount tr1 0
        sin01 = out 0 (pan2 (sinOsc AR freq 0 * e) pos 1)
        pos   = rand 'A' (-1) 1
        freq  = midiCPS $ select idx (mce pchs)
        idx   = iRand 'I' 0 (constant $ length pchs)
        pchs  = foldr (\o acc -> map (+o) degs ++ acc) [] octs
        octs  = take 4 $ iterate (+12) 48
        degs  = [0,4,7,11]
        e     = envGen KR 1 0.1 0 dur RemoveSynth esh
        esh   = envCoord [(0,0),(atk,1),(1,0)] 1 1 EnvCub
        dur   = expRand 'd' 0.3 3
        atk   = expRand 'a' 0.001 0.999
    _ <- mapM_ (async fd . d_recv . uncurry synthdef)
         [("sin01",sin01),("st01",st01)]
    send fd $ s_new "st01" (-1) AddToTail 1 []
    fix $ \f -> do
        [Int32 _nid, Int32 _tid, Float _val] <- waitDatum fd "/tr"
        -- print _val
        send fd $ s_new "sin01" (-1) AddToTail 1 []
        f
    -- getChar >> killThread tid
