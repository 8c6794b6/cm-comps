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

import           Control.Monad (forM, unless, void, when)
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
    mapM_ (\x -> async fd $ b_alloc x 32 1) [0..8]
    mapM_ (async fd . d_recv) $(synthdefGenerator)
    runReaderT (patchNode $ nodify nodes) fd
    static <- Extra.getStaticDir
    startGUI defaultConfig {tpStatic=Just static} (setup fd)

nodes :: Nd
nodes =
  let t00 = syn "trig00" ["out"*=100,"bpm"*=128]
      rbufrd outn bufn =
          syn "rbufrd01" ["out"*=outn,"bufn"*=bufn,"len"*=32,"tr0"*<-t00-*"out"]
      t01 = rbufrd 101 0
      t02 = rbufrd 102 1
      t03 = rbufrd 103 2
  in  grp 0
      [ grp 1
        [ t00, t01, t02, t03 ]
      , grp 2
        [ syn "add01"
          ["out"*=0,"tr1"*<-t00-*"out","faf"*=9.45,"hps"*=2.80]
        , syn "osc02"
          ["out"*=1,"tr1"*<-t00-*"out","cfhi"*=7562,"ftrr"*=0.1]
        , syn "bd01"
          ["out"*=2,"freq"*=70,"dur"*=0.12,"t_tr0"*<-t01-*"out"]
        , syn "hat01"
          ["out"*=3,"t_tr0"*<-t02-*"out"]
        , syn "snr01"
          ["out"*=4,"t_tr0"*<-t03-*"out"]
        ]
      , grp 3
        [ syn "mixer01" [] ]
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
                _ -> error $ unwords ["no", name, "in", show node]
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

    let vr ::
            String -> Int -> Double -> Double -> Double -> Double -> UI Element
        vr l nid minv maxv stepv iniv =
            Extra.vrange l minv maxv stepv iniv $ \v -> do
                liftIO $ send fd $ n_set nid [(l,v)]
                return $ printf "%3.2f" v

    -- common trigger --
    let iniBpmVal  = queryParam "bpm" tr00node
        iniBeatVal = queryParam "beat" tr00node
    bpm <- Extra.textbox "bpm" 50 (show (ceiling iniBpmVal :: Int)) $ \v -> do
        let vs = reads v
        unless (null vs) $ liftIO $
            send fd $ n_set tr00nid [("bpm",fst $ head vs)]
    beat <- Extra.hrange "beat (2**(v/4))" 0 32 1 iniBeatVal $ \v -> do
        liftIO $ send fd $ n_set tr00nid [("beat",2**(v/4))]
        return $ show v
    mapM_ (\e -> element e # set style [("float","left")]) [bpm, beat]

    -- add01 --
    let iniFafVal = queryParam "faf" add01node
        iniHpsVal = queryParam "hps" add01node
    add01faf <- vr "faf" add01nid 0.2 30 0.05 iniFafVal
    add01hps <- vr "hps" add01nid 0.1 10 0.05 iniHpsVal

    -- osc01 --
    let xysiz :: Num a => a
        xysiz = 128
    osc02xy <- Extra.xyarea "cfhi x ftrr" xysiz $ \(x,y) -> do
        let x' = (fromIntegral x / xysiz) * 8000
            y' = fromIntegral y / xysiz
        liftIO $ send fd $ n_set osc02nid [("cfhi",x'),("ftrr",y')]
        return $ printf "(%3.2f,%3.2f)" x' y'

    -- buffer --
    -- let bufbox bnum = do
    --         let lbl = "buf " ++ show bnum
    --         (wrapper,chks) <- hcheckbox lbl 16 $ \n isChecked ->
    --             liftIO $ send fd $ b_set bnum [(n,if isChecked then 1 else 0)]
    --         vals <- liftIO $ queryBuffer bnum fd
    --         zipWithM_ (\c v -> element c # set UI.checked (v==1)) chks vals
    --         return wrapper

    -- To query a node containing `"bufn":=100` parameter:
    --
    -- > queryN (params (id ==? "bufn":=100)) currentNodes
    --
    -- chk100 <- bufbox 0
    -- chk101 <- bufbox 1

    -- grid boxes --
    (gridboxes, _boxes) <- toggleBoxes 32 12 $ \(i,j) val ->
        liftIO $ send fd $ b_set i [(j, fromIntegral val)]

    -- mixer --
    mamp <- vr  "mamp" mixer01nid (-60) 25 0.1 0
    let vrm :: String -> Int -> Double -> Double -> Double -> UI Element
        vrm l n minv maxv iniv = vr (l++show n) mixer01nid minv maxv 0.01 iniv
        pos_x_amp n = do
            pos <- vrm "pos" n (-1) 1 0
            amp <- vrm "amp" n (-60) 25 0
            return [pos,amp]
    pos_x_amps <- concat <$> mapM pos_x_amp [0..4]
    mstr <- UI.new #+ map element (mamp : pos_x_amps)

    -- layout --
    mapM_ (\e -> element e # set style [("float","left")])
        ([bpm, add01faf, add01hps, osc02xy {- , chk100, chk101 -}] ++ pos_x_amps)
    let divClear = UI.div # set style [("clear","both")]
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
            set style [("padding","5px"),("float","left"),("margin-bottom","10px")] #+
            [element gridboxes]
          , divClear
          , UI.new #
            set style
            [("padding","5px"),("float","left"),("margin-bottom", "10px")] #+
            [element add01faf, element add01hps, element osc02xy
            -- ,element chk100, element chk101
            ]
          , divClear
          , UI.new #
            set style [("padding","5px"),("float","left")] #+
            [element mstr]
          , divClear
          ]
        ]
    UI.start tmr

-- | Returns checkbox in horizontal sequence.
hcheckbox ::
    String -- ^ Label name
    -> Int -- ^ Number of checkboxes.
    -> (Int -> Bool -> UI ())
    -- ^ Action taken on check, with index value and 'True' on check, 'False' on
    -- uncheck.
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
        # set style [("padding","10px 10px 0 10px")]
    return (wrapper, checks)

-- | Returns grid of toggle boxes.
toggleBoxes ::
    Int
    -> Int
    -> ((Int,Int) -> Int -> UI ())
    -> UI (Element, [((Int,Int), Element)])
toggleBoxes w h act = do
    let width = 25 :: Int
        height = 20 :: Int
    columns_x_boxes <- forM [0..h-1] $ \rowi -> do
        columWrap <- UI.div # set style
                     [("clear","both")
                     ,("height",show height ++ "px")
                     ]
        boxes <- forM [0..w-1] $ \coli -> do
            box <- UI.div
                   # set style [("width",show width ++ "px")
                               ,("height",show height ++ "px")
                               ,("float","left")
                               ,("border-right","solid 1px")
                               ,("border-bottom","solid 1px")
                               ,("font-size","8px")
                               ]
                   # set value "0"
            when (coli `mod` 4 == 0 && rowi `mod` 4 == 0) $
                void $ element box # set UI.text (show coli)
            on UI.click box $ \_ -> do
                val <- toggleBox box
                void $ act (rowi,coli) val
            return ((rowi,coli),box)
        clm <- element columWrap #+ map (element . snd) boxes
        return (clm, boxes)
    wrapper <- UI.div # set style
               [("border-top","solid 1px")
               ,("border-left","solid 1px")
               ,("width",show ((width+1) * w) ++ "px")
               ,("float","left")
               ]
    let (columns, boxes) = unzip columns_x_boxes
    wrapper' <- element wrapper #+ map element columns
    return (wrapper', concat boxes)

-- | Toggle value and background colour of single box.
toggleBox ::
    Element   -- ^ Div element of box, returned from 'toggleBoxes'.
    -> UI Int -- ^ Value after the toggle.
toggleBox box = do
    val <- box # get UI.value
    if val == "0"
       then turnOnBox box >> return 1
       else turnOffBox box >> return 0

-- | Turn on the box element returned from 'toggleBoxes'.
turnOnBox :: Element -> UI Element
turnOnBox box =
    element box
        # set style [("background-color","#888")]
        # set UI.value "1"

-- | Turn off the box element returned from 'toggleBoxes'.
turnOffBox :: Element -> UI Element
turnOffBox box =
    element box
        # set style [("background-color","#fff")]
        # set UI.value "0"


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
synth_osc02 :: UGen
synth_osc02 = out (control KR "out" 1) sig0
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
synth_rbufrd01 :: UGen
synth_rbufrd01 = out (control KR "out" 0) sig
  where
    sig   = coinGate 'g' (bufRdN 1 KR bufn idx Loop) tr0
    bufn  = control KR "bufn" 100
    idx   = gate (stepper tr0 0 0 (len-1) 1 0) tr0
    len   = control KR "len" 16
    tr0   = control KR "tr0" 1

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
    aenv0 = envGen KR tr0 1 0 dur DoNothing ash0 * aenv1
    ash0  = envPerc 1e-3 1
    aenv1 = lfdNoise3 'A' KR aef1 * 0.5 + 0.5
    aenv2 = decay2 tr0 1e-3 0.2
    aef1  = decay2 tr0 1e-2 3 * 10
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

-- | Simple mixer.
synth_mixer01 :: UGen
synth_mixer01 = replaceOut 0 (sigs * dbAmp mamp)
  where
    sigs = sum $ map f [0..4::Int]
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
