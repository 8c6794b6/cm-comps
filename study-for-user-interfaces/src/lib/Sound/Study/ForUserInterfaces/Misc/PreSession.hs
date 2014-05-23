{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-|

Session with textual interface, without implementation.

Considering the ideal set of functions to write before writing the
implementation of functions.

-}
module Sound.Study.ForUserInterfaces.Misc.PreSession where

import Data.Monoid
import Data.Tree
import System.Random

import Sound.OSC
import Sound.SC3 hiding (withSC3)
import Sound.SC3.ID hiding (withSC3)
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

offset :: Monad m => Int -> m a
offset n = undefined

-- --------------------------------------------------------------------------
--
-- Editing node graph as in filling templates
--
-- --------------------------------------------------------------------------

{-
As in HTML, filling in a node graph data structure for patching to running
scsynth, inspired from the work of HSXML.

Suppose that, defaultSetting is a function which takes SCNode to embed in
initial setting done in TUI02. The initial setting contains synth for triggering
common impulse, and master router.

> defaultSetting :: SCNode -> SCNode
> defaultSetting node = ...

> runQuery :: Transport m => SCNode -> m ()
> runQuery node = ...

-}


--
-- List builder from:
--
--    http://okmij.org/ftp/Haskell/vararg-fn.lhs
--

class BuildList a r | r -> a where
    build' :: [a] -> a -> r

instance BuildList a [a] where
    build' l x = reverse $ x : l

instance BuildList a r => BuildList a (a->r) where
    build' l x = \y -> build' (x:l) y

li :: BuildList a r => a -> r
li x = build' [] x

-- Avoiding commas in demand ugen builder

b01 = dseq 'a' dinf
      (mce (mcb 1 2 3 4 5))

mcb x = build' [] x

-- dseq :: ID i => i -> UGen -> UGen -> UGen
bseq = undefined

-- dinf :: UGen
-- dinf = constant (maxBound :: Int)
binf = undefined

-- drand :: ID i => i -> UGen -> UGen -> UGen
brand = undefined

-- dwhite :: ID i => i -> UGen -> UGen -> UGen -> UGen
bwhite = undefined

c01 = buildC $ cseq cinf 1 2 3 4 5
c02 = buildC $
      cseq cinf
      (crand 1
       cnil (cseq 1 24 31 36 43 48 55))
      (cseq (ciwhite cinf 2 5)
       60 (crand 1 63 65) 67 (crand 1 70 72 74))
      (crand (ciwhite cinf 3 9) 74 75 77 79 81)

buildC = undefined

cseq x y = undefined

cinf = undefined

crand x y = undefined

ciwhite x y z = undefined

cnil = undefined

-- cs :: BuildList a r => a -> r
-- cs x = build' []
cs x = undefined

blah :: a -> c -> (b->b)
blah _ _ = id

-- instance Num [UGen] where
--     fromInteger x = [constant x]

-- Take 2

class Monoid acc => Build acc out r | r -> out where
    build :: (acc -> out) -> acc -> r

-- instance Monoid dc => Build dc [a] [a] where
--     build tr acc = tr acc
instance Monoid dc => Build dc (UGs a) (UGs a) where
    build tr acc = tr acc

instance (Build acc out r, a ~ acc) => Build acc out (a->r) where
    build tr acc = \s -> build tr (acc `mappend` s)

eseq = undefined
einf = undefined
enil = undefined
erand = undefined
eiwhite = undefined
es = undefined
runE = undefined

e01 = runE $
      eseq einf
      (es
       (erand 1
        (es enil (eseq 1 (es 24 31 36 43 48 55))))
       (eseq (eiwhite einf 2 5)
        (es 60 (erand 1 (es 63 65)) 67 (erand 1 (es 70 72 74))))
       (erand (eiwhite einf 3 9)
        (es 74 75 77 79 81)))

d00 =
    sseq sinf
    [ srand 1 [snil, sseq 1 [24,31,36,43,48,55]]
    , sseq (siwhite sinf 2 5)
      [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
    , srand (siwhite sinf 3 9)
      [74, 75, 77, 79, 81]]


-- | This works:
--
-- >>> d00 == d01
-- True
--
-- Though, those when passing 'UGen' insteadof '[UGen]' still need to apply
-- 'mce' function. Some of the functions like 'sseq', 'srand' takes listof
-- 'Supply', but not all of them. Also, types of these functions are defined
-- with Streams-Patterns-Event example in mind. Start feeling that these
-- types may not the best choice.
--
d01 :: Supply
d01 =
    sseq sinf
    (li
     (srand 1
      (li snil (sseq 1 (li 24 31 36 43 48 55))))
     (sseq (siwhite sinf 2 5)
      (li 60 (srand 1 (li 63 65)) 67 (srand 1 (li 70 72 74))))
     (srand (siwhite sinf 3 9)
      (li 74 75 77 79 81)))

play_d01 :: IO ()
play_d01 = withSC3 $ do
    let osig = rlpf (saw AR freq) cf rq
        cf   = linLin (lfdNoise3 'a' KR (1/3) + 2) 1 3 200 4000
        rq   = 0.3
        freq = midiCPS $ demand tr 1 (evalSupply d01 (mkStdGen 0x83))
        tr   = impulse KR 8 0
    play $ out 0 (mce (li osig osig) * 0.1)

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

fseq x y = dseq 'a' (mce x) (mce y)

d02 :: Supply
d02 =
    midiCPS
    ((sseq sinf (li 0 4 5 7 2 4 5 7)) +
     (sstutter
      (srand sinf (li 1 1 1 2 4 8))
      (12 * (siwhite sinf 3 8))))

{-
Thoughts:

* Combination of demand ugen functions are not S-expression.

-}

class Unify as a | as -> a where
    unify :: as -> a

instance (Real a, Num a) => Unify a UGen where
    unify = constant

newtype UGs a = UGs {unUGs :: [UGen]}
    deriving (Eq, Show, Monoid)

instance Unify (UGs a) UGen where
    unify = mce . unUGs


class UGc1 v where
    ugc1 :: v -> UGen

instance Real a => UGc1 a where
    ugc1 = constant

instance UGc1 UGen where
    ugc1 = id

instance UGc1 a => UGc1 [a] where
    ugc1 = mce . map ugc1

{-
This works:

    >>> unify $ UGs (li 1 2 3 4 5)
    MCE_U (MCE_Vector [Constant_U (Constant {constantValue = 1.0})
                      ,Constant_U (Constant {constantValue = 2.0})
                      ,Constant_U (Constant {constantValue = 3.0})
                      ,Constant_U (Constant {constantValue = 4.0})
                      ,Constant_U (Constant {constantValue = 5.0})])

-}

{-

Suppose that, desired way to write `vrand', variant of `drand' UGen is any of:

    > vrand 1 100
    > vrand 5 (li 1 2 3 4)
    > vrand vinf (li 1 2 3 4)
    > vrand (li 2 3) (li 3 4 5 6)
    > vrand (vrand vinf (li 2 3 4)) (li 100 200 300)

vrand has type ::

    vrand :: (SomeClass c1, SomeClass c2, SomeClass c3) => c1 -> c2 -> c3

From the syntax, numerical value and returned type of vinf, li, and vrand itself
is an instance of `SomeClass'.

What is this `SomeClass', and what are its instances?

-}


leaf x = Node x []

node x xs = mcb (Node x xs)


class BT bt where
    btree :: Int -> [Tree Int] -> bt

instance BT (Tree Int) where
    btree x xs = Node x (reverse xs)

instance (BT t) => BT (Tree Int -> t) where
    btree x xs = \t -> btree x (t:xs)

type T = Tree Int

t0 :: T
t0 = btree 3 []

class BT2 t where
    bt2 :: Int -> [Tree Int] -> t

-- instance BT2 (Tree Int) where
--     bt2 x ts = Node x ts

instance BT2 [Tree Int] where
    bt2 x ts = [Node x ts]

instance (BT2 t) => BT2 ([Tree Int] -> t) where
    bt2 x ts = \t -> bt2 x (ts ++ t)

bt2' :: BT2 t => Int -> t
bt2' x = bt2 x []
