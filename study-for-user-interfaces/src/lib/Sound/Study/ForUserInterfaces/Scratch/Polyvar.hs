{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Sound.Study.ForUserInterfaces.Scratch.Polyvar where

import Sound.SC3.Tree

type DiffList a = [a] -> [a]

snoc :: DiffList a -> a -> DiffList a
as `snoc` a = as . (a:)

nil :: DiffList a
nil = id

class G g where
    _group' :: DiffList SCNode -> Int -> SCNode -> g

instance G SCNode where
    _group' ns i n = Group i ((ns `snoc` n) [])

instance G r => G (SCNode -> r) where
    _group' ns i n = _group' (ns `snoc` n) i

group' :: G g => Int -> SCNode -> g
group' = _group' nil

synth' :: Int -> String -> [SynthParam] -> SCNode
synth' = Synth

scn :: SCNode -> SCNode
scn = id

n1 :: SCNode
n1 =
    group' 1
    (synth' 1001 "foo" [])
    (synth' 1002 "bar" [])
    (synth' 1003 "buzz" [])

n2 :: SCNode
n2 =
    group' 0
    (group' 1
     (synth' 1001 "foo" []) (synth' 1002 "bar" []) :: SCNode)
    (scn $ group' 2
     (synth' 2001 "foo" []) (synth' 2002 "bar" []))

class Nodable n => G2 n g where
    __group' :: DiffList n -> Int -> n -> g

instance G2 SCNode SCNode where
    __group' ns i n = Group i $ (ns `snoc` n) []

-- instance (Nodable n, G2 n g) => G2 n (n -> g) where
--     __group' ns i n = __group' (ns `snoc` fromNodable n) i

group'' :: (Nodable n, G2 n g) => Int -> n -> g -- requires signature.
group'' = __group' nil

class Nodable n where
    toNodable   :: SCNode -> n
    fromNodable :: n -> SCNode

instance Nodable SCNode where
    fromNodable = id
    toNodable   = id

-- n1 :: SCNode
-- n1 =
--     group' 1
--     (group' 10
--      (group' 100
--       (synth' 1000 "foo" []) (synth' 1001 "bar" []) :: SCNode)
--      (scn $ group' 200
--       (synth' 2000 "foo" []) (synth' 2001 "bar" [])))

class SumRes r where
    sumOf :: Integer -> r

instance SumRes Integer where
    sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    sumOf x = sumOf . (+x) . toInteger

sum1 :: Integer
sum1 = sumOf 1 2 3 4 5

sum2 :: Integer
sum2 = sumOf 1 2 3 4 5 6 7 8 9 10 -- easier to write with `sum [1..10]'.

-- Product:

class ProductRes p where
    productOf :: Int -> p

instance ProductRes Int where
    productOf = id

instance ProductRes p => ProductRes (Int -> p) where
    productOf = \i -> productOf . (*i)

-- Requires type signatures in arguments.
p1 :: Int
p1 = productOf (1 :: Int) (2 :: Int) (3 :: Int)

p2 :: Int
p2 = productOf (10 :: Int) (2 :: Int) (3 :: Int) (4 :: Int)

-- Product: take 2

class ProductOf2 p where
    productOf2 :: Integer -> p

instance ProductOf2 Integer where
    productOf2 = id

instance (Integral a , ProductOf2 p) => ProductOf2 (a -> p) where
    productOf2 x = \y -> productOf2 (x * toInteger y)

-- With below instance definition, p'1 and p'2 requires type signature in
-- arguments.
--
-- > instance ProductOf2 p => ProductOf2 (Integer -> p) where
-- >     productOf2 x = \(y :: Integer) -> productOf2 (x * y)
--

-- Does not require type signatures in arguments.
p2'1 :: Integer
p2'1 = productOf2 1 2 3

p2'2 :: Integer
p2'2 = productOf2 1 2 3 4 5 6


-- Product: take 3

class P3 p where
    p3 :: Int -> p

instance P3 Int where
    p3 = id

instance P3 Integer where
    p3 = fromIntegral

instance (Enum a, P3 p) => P3 (a -> p) where
    p3 x = \y -> p3 (x * fromEnum y)

p3'1 :: Int
p3'1 = p3 1 2 3

p3'2 :: Integer
p3'2 = p3 1 2 (p3 3 4 :: Int) 5

-- Won't work:
--
-- p3'2_ng :: Int
-- p3'2_ng = p3 1 2 (p3 3 4) 5
--
--     No instance for (Enum t0) arising from a use of ‘p3’
--     The type variable ‘t0’ is ambiguous
--     Note: there are several potential instances:
--       instance Enum Double -- Defined in ‘GHC.Float’
--       instance Enum Float -- Defined in ‘GHC.Float’
--       instance Integral a => Enum (GHC.Real.Ratio a)
--         -- Defined in ‘GHC.Real’
--       ...plus 60 others
--     In the expression: p3 1 2 (p3 3 4) 5
--     In an equation for ‘p3'2_ng’: p3'2_ng = p3 1 2 (p3 3 4) 5
--
