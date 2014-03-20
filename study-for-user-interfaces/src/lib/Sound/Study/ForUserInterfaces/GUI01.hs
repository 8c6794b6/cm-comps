{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Graphical user interface for synths, take 1.

-}
module Sound.Study.ForUserInterfaces.GUI01 where

import           Control.Concurrent (forkIO, killThread)
import           Control.Monad (unless, void)
import           Text.Printf (printf)

import           Sound.OSC.FD
import           Sound.SC3.FD
import           Sound.SC3.UGen.ID

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import qualified Graphics.UI.Threepenny.Extra as Extra


-- --------------------------------------------------------------------------
--
-- * UI
--
-- --------------------------------------------------------------------------

-- | Main entry point.
main :: IO ()
main = withSC3 $ \fd -> do
    _ <- mapM_ (async fd . d_recv . uncurry synthdef)
         [ ("trig00",trig00)
         , ("add01",add01)
         , ("osc02",osc02)
         , ("mixer01",mixer01)
         ]
    static <- Extra.getStaticDir
    tid <- forkIO $
           startGUI defaultConfig { tpStatic = Just static } $
           setup fd
    getChar >> killThread tid

-- | Setup GUI with given 'Transport'.
setup :: Transport t => t -> Window -> UI ()
setup fd window = do
    void $ return window # set title "gui 01"
    UI.addStyleSheet window "ui.css"

    let xysiz :: Num a => a
        xysiz = 128

    let vr :: String -> Int
              -> Double -> Double -> Double -> Double -> UI Element
        vr l nid minv maxv stepv iniv =
            Extra.vrange l minv maxv stepv iniv $ \v -> do
                liftIO $ send fd $ n_set nid [(l,v)]
                return $ printf "%3.2f" v

        vrm :: String -> Int -> Double -> Double -> Double -> UI Element
        vrm l n minv maxv iniv = vr (l++show n) 1002 minv maxv 0.01 iniv

    bpm <- Extra.textbox "bpm" 50 "128" $ \v -> do
        let vs = reads v
        unless (null vs) $ liftIO $ send fd $ n_set 999 [("bpm",fst $ head vs)]
    beat <- Extra.hrange "beat (2**(v/4))" 0 32 1 8 $ \v -> do
        liftIO $ send fd $ n_set 999 [("beat",2**(v/4))]
        return $ show v
    mapM_ (\e -> element e # set style [("float","left")]) [bpm, beat]

    add01faf <- vr "faf" 1000 0.2 30 0.05 5
    add01hps <- vr "hps" 1000 0.1 10 0.05 0.3

    osc02xy <- Extra.xyarea "cfhi x ftrr" xysiz $ \(x,y) -> do
        let x' = (fromIntegral x / xysiz) * 8000
            y' = fromIntegral y / xysiz
        liftIO $ send fd $ n_set 1001 [("cfhi",x'),("ftrr",y')]
        return $ printf "(%3.2f,%3.2f)" x' y'

    mamp <- vr  "mamp" 1002 (-60) 25 0.1 0
    pos0 <- vrm "pos" 0 (-1) 1 0
    amp0 <- vrm "amp" 0  (-60) 25 0
    pos1 <- vrm "pos" 1 (-1) 1 0
    amp1 <- vrm "amp" 1  (-60) 25 0
    mstr <- UI.new #+ map element [mamp,pos0, amp0, pos1, amp1]

    mapM_ (\e -> element e # set style [("float","left")])
        [bpm, add01faf, add01hps, osc02xy, pos0, amp0, pos1, amp1]

    let divClear = UI.div # set style [("clear","both")]

    (tmr,stDiv) <- Extra.statusDiv fd
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
            [element add01faf, element add01hps , element osc02xy]
          , divClear
          , UI.new #
            set style [("padding","5px"),("float","left")] #+
            [element mstr]
          , divClear
          ]
        ]

    liftIO $ sendOSC fd $
        bundle immediately
        [ s_new "trig00"   999 AddToTail 1 [("out",100)]
        , s_new "add01"   1000 AddToTail 1 [("out",0)]
        , s_new "osc02"   1001 AddToTail 1 [("out",1)]
        , s_new "mixer01" 1002 AddToTail 1 []
        , n_map 1000 [("tr1",100)]
        , n_map 1001 [("tr1",100)]
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

-- | Simple mixer.
mixer01 :: UGen
mixer01 = replaceOut 0 (sigs * dbAmp mamp)
  where
    sigs = sum $ map f [0,1::Int]
    f n  = pan2 (in' 1 AR inn) posn (dbAmp ampn)
      where
        inn  = control KR ("in" ++ show n) (fromIntegral n)
        posn = control KR ("pos" ++ show n) 0
        ampn = control KR ("amp" ++ show n) 0.5
    mamp = control KR "mamp" 0
