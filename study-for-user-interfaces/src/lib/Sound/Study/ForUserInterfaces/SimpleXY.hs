{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer   : 8c6794b6@gmail.com
Stability    : experimental
Portability  : unknown

Simple example with xy coordinate area.

-}
module Sound.Study.ForUserInterfaces.SimpleXY where

import           Control.Monad (void)
import           Text.Printf (printf)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Extra as Extra

import           Sound.OSC.FD
import           Sound.SC3.FD
import           Sound.SC3.UGen.ID

-- | Main entry point for showing simple GUI with xy areas.
main :: IO ()
main = withSC3 $ \fd -> do
    mapM_ (async fd . d_recv . uncurry synthdef)
        [("noiz1", noiz1),("noiz2",noiz2),("noiz3",noiz3)]
    static <- Extra.getStaticDir
    startGUI defaultConfig {tpStatic=Just static} (setup fd)

-- | Synth used in this example.
noiz1 :: UGen
noiz1 = out 0 (pan2 sig0 0 1)
  where
    sig0 = resonz sig1 cf rq * 0.1
    sig1 = whiteNoise 'W' AR
    cf   = control KR "cf" 440 `lag2` 0.1
    rq   = control KR "rq" 0.3 `lag2` 0.1

-- | Another synth.
noiz2 :: UGen
noiz2 = out 0 (pan2 sig0 0 1)
  where
    sig0  = mix (sinOsc AR freqs phs) * aenv * 0.1
    phs   = sinOsc AR mf 0 * 3
    aenv  = envGen KR tr0 ampv 0 dur DoNothing ash
    ash   = envCoord [(0,0),(0.01,1),(1,0)] 1 1 EnvCub
    ampv  = tRand 'A' 0.3 1 tr0
    freqs = mce [ freq
                , freq * lfdNoise3 '1' KR 0.37 + 1
                , freq * lfdNoise3 '2' KR 1.23 * 0.5 + 0.5
                ]
    freq  = 440
    mf    = (freq * 4.32)
    tr0   = impulse KR 2 0 + tr1
    tr1   = dust 'T' KR df
    dur   = control KR "dur" 0.17 `lag2` 0.1
    df    = control KR "df" 34 `lag2` 0.1

noiz3 :: UGen
noiz3 = out 0 sig0
  where
    sig0 = bBandStop nz cf rq * 0.1
    nz   = whiteNoise 'W' AR
    cf   = control KR "cf" 800
    rq   = control KR "rq" 0.3

-- | Setup GUI window with given 'Transport'.
setup :: Transport t => t -> Window -> UI ()
setup fd window = do
    void $ return window # set title "simple xy"
    UI.addStyleSheet window "ui.css"

    let xysiz :: Num a => a
        xysiz = 128

    area1 <- Extra.xyarea "noiz1 (cf,rq)" xysiz $ \(x,y) -> do
        let x' = (fromIntegral x / xysiz) * 8000
            y' = fromIntegral y / xysiz
        liftIO $ send fd $ n_set 1000 [("cf",x'),("rq",y')]
        return $ printf "(%2.3f,%2.3f)" x' y'

    area2 <- Extra.xyarea "noiz2 (dur,df)" xysiz $ \(x,y) -> do
        let x' = (fromIntegral x / xysiz) * 2
            y' = (fromIntegral y / xysiz) * 20
        liftIO $ send fd $ n_set 1001 [("dur",x'),("df",y')]
        return $ printf "(%2.3f,%2.3f)" x' y'

    mapM_ (\e -> element e # set style [("float","left")]) [area1, area2]

    void $ getBody window #+
        [ element area1
        , element area2
        ]

    liftIO $ do
        send fd $ s_new "noiz1" 1000 AddToTail 1 []
        send fd $ s_new "noiz2" 1001 AddToTail 1 []
        send fd $ s_new "noiz3" 1002 AddToTail 1 []
