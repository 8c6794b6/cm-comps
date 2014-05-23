module Main where

import Sound.OSC
import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
import Sound.Study.ForUserInterfaces.TUI.TUI02

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

main :: IO ()
main = withSC3 $ runTrack 108 $ do
    offset 8
    let pchs = midiCPS (mce (foldr (\o acc -> map (+o) degs ++ acc) [] octs))
        degs = [0,2,4,5,7,9,11]
        octs = take 7 $ iterate (+12) 24
    source "sin05" $ do
        "amp"  ==> curveTo EnvLin 8 0.1
        "freq" ==> linExp (lfdNoise3 '\NUL' KR (1/9)+2) 1 3 20 300
        "tr"   ==> dust '\NUL' KR 2
        "dur"  ==> curveTo EnvLin 8 2
        "pan"  ==> linLin (lfdNoise3 '\NUL' KR (1/3)) (-1) 1 0 1
    let inp p = input 108 (synthName ==? "sin05") (paramName ==? p)
        fsin i = source' i "sin05" $ do
            "amp" ==>
                let df = linExp (lfdNoise3 i KR 1 + 2) 1 3 (1/16) 16
                in  linExp (squared (lfdNoise1 i KR df) + 2) 1 3 1e-9 0.5
            "freq" ==>
                (\tr -> tChoose i (coinGate i (1/128) tr) pchs)
            "tr"  ==> inp "tr"
                (\_ t -> t + dust i KR (lfdNoise3 i KR (1/3) * 3 + 3))
            "dur" ==> linExp (lfdNoise3 i KR 0.3 + 2) 1 3 0.5 2
            "pan" ==> linLin (lfdNoise3 i KR (1/3)) (-1) 1 0 1
            "atk" ==> linLin (lfdNoise3 i KR 1 + 2) 1 3 0 1
    mapM_ fsin [0..63]
    effect "dc01" $ do
        "wet" ==> curveTo EnvLin 8 1
    effect "lmt01" $ do
        "wet" ==> curveTo EnvLin 8 1
    router $ do
        "amp" ==> curveTo EnvCub 8 1
