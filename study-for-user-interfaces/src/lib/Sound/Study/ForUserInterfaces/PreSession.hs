{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-|

Session with textual interface, without implementation.

Considering the ideal set of functions to write before writing the
implementation of functions.

-}
module Sound.Study.ForUserInterfaces.PreSession where

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Supply
import Sound.SC3.Tree

-- | A track number 101.
--
-- Contains synthdef /src01/, with its parameters assigned to constant value and
-- ugen, and its signal is routed to effect synth named /efx01/.
--
-- XXX: Specify loop offset count.
--
t101 :: Monad m => m a
t101 = track 101 $ do
    source "src01" $ do
        "freq" @= 110
        "amp"  @= 0.5
        "cf"   @= anon (\_ _ -> linLin (sinOsc KR 0.25 0 * 0.5 + 0.5) (-1) 1 0 1)
    effect "efx01" $ do
        "wet" @= 0.5
        "dcy" @= linLin (sinOsc KR 0.25 0) (-1) 0 0 1

-- | Updated version of 't101', contains two /src01/ synth, first /src01/
-- changed freq parameter to demand ugen, also dcy parameter of /efx01/ has
-- changed.
t101' :: Monad m => m a
t101' = track 101 $ do
    source "src01" $ do
        "freq" @= sug (sseq sinf [440,220,330,220])
        "amp"  @= 0.5
        "cf"   @= anon (\_ _ -> linLin (sinOsc KR 0.25 0 * 0.5 + 0.5) (-1) 1 0 1)
    source "src01" $ do
        "freq" @= 220
        "amp"  @= 0.5
    effect "efx01" $ do
        "wet" @= 0.5
        "dcy" @= linLin (lfdNoise3 'a' KR 0.25) (-1) 1 0 1

-- | Master control.
mstr :: Monad m => m b
mstr = master $ do
    track 101 $ "amp" @= 1
    track 102 $ "amp" @= 0
    track 103 $ "amp" @= 0
    track 104 $ "amp" @= 1

-- | A Node written with current implementation of mini EDSL for building
-- 'SCNode'.
nd01 :: Nd
nd01 = do
    let p1 = syn "p1" ["out"*=100]
        p2 = syn "p2" ["out"*=101]
    grp 101
        [ p1, p2
        , syn "src1"
          [ "freq" *= 110
          , "amp"  *= 0.5
          , "cf"   *<- p1 -* "out"]
        , syn "src1"
          [ "freq"*= 220
          , "amp"*=0.5 ]
        , syn "efx01"
          [ "wet" *= 0.5
          , "dcy" *<- p2 -* "out" ]
        , syn "router"
          [] ]

master :: Monad m => m a -> m b
master = undefined

track :: Monad m => Int -> m a -> m b
track = undefined

source :: Monad m => String -> m a -> m b
source = undefined

effect :: Monad m => String -> m a -> m b
effect = undefined

router = undefined

(@=) :: Monad m => String -> UGen -> m a
a @= b  = undefined
a @=* b = undefined

sug :: Supply -> UGen
sug = undefined

-- | Similar to 'sug', but with triggered values.
tug :: Supply -> UGen
tug = undefined

anon :: (Double -> Double -> UGen) -> UGen
anon = undefined

g101 :: IO ()
g101 = withSC3 $ track 101 $ do
    offset 8
    let pan = linLin (sinOsc KR 0.25 0) (-1) 1 0 1
    source "sin01" $ do
        "freq" ==> sseq sinf [1,2,3,4]
        "pan"  ==> const pan
    source "sin01" $ do
        "freq" ==> sseq sinf [4,3,2,1]
        "pan"  ==> const (1 - pan)
    effect "ap01" $ do
        "wet" ==> dble 0.8
        "dcy" ==> const (linLin (lfdNoise3 'a' KR 0.21) (-1) 1 0 1)

class Assignable a where
    assign :: (Monad m) => a -> m Message

instance Assignable (a -> UGen) where
    assign f = undefined

instance Assignable Double where
    assign d = return (n_set (-1) [("foo",d)]) -- undefined

instance Assignable Supply where
    assign d = undefined

(==>) :: Assignable a => String -> a -> c
a ==> b = undefined

vsup = undefined

dble :: Double -> Double
dble = id

-- dval :: Monad m => Double -> m b
-- dval = undefined

offset :: Monad m => Int -> m a
offset n = undefined


{-
group = undefined

setP = undefined


-}
