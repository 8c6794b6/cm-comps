{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Scratch written while reading sc3-plugins related sc3 help files.

-}
module Sound.Study.ForUserInterfaces.Scratch.Plugins where

import Sound.SC3
import Sound.SC3.ID

-- | Example of 'coyote'.
coyote_ex01 :: IO ()
coyote_ex01 = withSC3 $ do
    let osig = out 0 (mce2 dsig ssig)
        tr   = dust 'D' KR 1
        ssig = mixFill 3 fs
        fs :: Int -> UGen
        fs i = saw AR (tRand i 50 880 tr) *
               decay tr (tRand i 0.2 2 tr) *
               recip 3
        dsig = pinkNoise 'P' AR * decay trc 0.2 * recip 3
        trc  = coyote KR ssig 0.2 0.2 0.01 0.5 0.001 00.1
    play osig

-- Could be imported from Sound.SC3.UGen.External.SC3_Plugins in hsc3-0.15.
coyote :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
coyote rate in_ trackFall slowLag fastLag fastMul thrsh minDur =
    mkOscR [KR] rate "Coyote"
    [in_,trackFall,slowLag,fastLag,fastMul,thrsh,minDur] 1
