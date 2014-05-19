{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Sound.Study.ForUserInterfaces.Misc.PreSession02 where

import Control.Monad
import Data.Monoid

import Sound.SC3
import Sound.SC3.UGen.MCE
import Sound.SC3.Supply

{-

Desired features for list builder

Would like to take numerical constants:

  > li 1 2 3 4

Would like take UGen:

  > li (sinOsc AR 440 0) (saw AR 220) (pulse AR 110 0.5)

Would like to take pattern:

  > li (vrand 1 100) (vseq 1 (li 1 2 3 4 5))

Would like to take mixture of above:

  > li 1 (sinOsc AR 220 0) (vrand 2 (li 100 200 300))

Would like to nest:

  > li (li 1 2 3) (li 4 5 6) (li 7 8 9)

v
Desired features for pattern functions:

Would like to take numerical constants:

  > vrand 1 100

Would like to take UGen (... really?):

  > vrand (sinOsc AR 440) 1
  > vrand 3

Would like to take result from polyvariadic function:

  > vrand 5 (li 1 2 3 4)
  > vrand (li 1 2 3) 5
  > vrand (li 2 3) (li 1 2 3 4)

Would like to take itself:

  > vrand (vrand 1 3) (vrand 3 20)


-}

-- MCE is Monoid.

instance Monoid (MCE u) where
    mempty = MCE_Vector []
    mappend (MCE_Unit a) (MCE_Unit b)       = MCE_Vector [a,b]
    mappend (MCE_Unit a) (MCE_Vector [])    = MCE_Unit a
    mappend (MCE_Unit a) (MCE_Vector bs)    = MCE_Vector (a:bs)
    mappend (MCE_Vector []) (MCE_Unit b)    = MCE_Unit b
    mappend (MCE_Vector as) (MCE_Unit b)    = MCE_Vector (as ++ [b])
    mappend (MCE_Vector []) (MCE_Vector []) = MCE_Vector []
    mappend (MCE_Vector as) (MCE_Vector bs) = MCE_Vector (as ++ bs)

class Num v => DU v where
    sup :: v -> Demand UGen

instance DU UGen where
    sup = sval

instance DU a => DU (Demand a) where
    sup = join . fmap sup

gseq :: (DU v1, DU v2) => v1 -> [v2] -> Demand UGen
gseq x ys = sseq (sup x) (map sup ys)

grand :: (DU v1, DU v2) => v1 -> [v2] -> Demand UGen
grand x ys = sseq (sup x) (map sup ys)

giwhite :: (DU v1, DU v2, DU v3) => v1 -> v2 -> v3 -> Demand UGen
giwhite x y z = siwhite (sup x) (sup y) (sup z)

ginf :: Demand UGen
ginf = sinf

gnil :: Demand UGen
gnil = snil

-- g01 =
--     gseq ginf
--      [ grand 1
--        [gnil, (gseq 1 [24,31,36,43,48,55])]
--      , gseq (giwhite ginf 2 5)
--        [60, grand 1 [63,65], 67, grand 1 [70,72,74]]
--      ,grand (giwhite ginf 3 9)
--       [74,75,77,79,81] ]

-- List builder, from:
--
--  http://stackoverflow.com/questions/2156207/polyvariadic-functions-in-haskell

class Variadic a b r | r -> a where
    variadic :: ([a] -> b) -> r

instance Variadic a b (a->b) where
    variadic f x = f [x]

instance Variadic a b (a->r) => Variadic a b (a->a->r) where
    variadic f x y = variadic (f . (x:)) y

vList :: Variadic a [a] r => r
vList = variadic id

sumInt :: [Int] -> Int
sumInt = sum

-- List builder, from HSXML:
--
--  http://okmij.org/ftp/Haskell/HSXML/HSXML.hs

class Monoid acc => Mariadic acc out r | r -> out where
    mariadic :: (acc->out) -> acc -> r

-- instance Monoid acc => Mariadic acc a a where
instance Monoid acc => Mariadic acc (MCE ug) (MCE ug) where
    mariadic f acc = f acc

instance (Mariadic acc out r, a ~ acc) => Mariadic acc out (a->r) where
    mariadic f acc = \u -> mariadic f (acc <> u)

-- >>> MCE_U 1
-- MCE_U (MCE_Unit (Constant_U (Constant {constantValue = 1.0})))
---
-- >>> MCE_U (mceu 1 2 3 4 5)
-- MCE_U (MCE_Vector [Constant_U (Constant {constantValue = 1.0})
--                   ,Constant_U (Constant {constantValue = 2.0})
--                   ,Constant_U (Constant {constantValue = 3.0})])
--
-- >>> MCE_U (mceu 1 (sinOsc AR 220 0))
-- <interactive>:37285:8:
--     No instance for (Mariadic UGen UGen (MCE UGen))
--       arising from a use of ‘mceu’
--     In the first argument of ‘MCE_U’, namely
--       ‘(mceu 1 (sinOsc AR 220 0))’
--     In the expression: MCE_U (mceu 1 (sinOsc AR 220 0))
--     In an equation for ‘it’: it = MCE_U (mceu 1 (sinOsc AR 220 0))
-- >>> MCE_U (mceu 1 (MCE_Unit (sinOsc AR 220 0)))
-- MCE_U (MCE_Vector
--    [Constant_U (Constant {constantValue = 1.0})
--    ,Primitive_U (Primitive {ugenRate = AR
--                           , ugenName = "SinOsc"
--                           , ugenInputs = [Constant_U (Constant {constantValue = 220.0})
--                                          ,Constant_U (Constant {constantValue = 0.0})]
--                           , ugenOutputs = [AR]
--                           , ugenSpecial = Special 0, ugenId = NoId})])
--

-- Or,
-- mceu :: Mariadic (MCE a) (MCE a) r => MCE a -> r
mceu :: Mariadic a a b => a -> b
mceu x = mariadic id mempty x

mu = MCE_U
