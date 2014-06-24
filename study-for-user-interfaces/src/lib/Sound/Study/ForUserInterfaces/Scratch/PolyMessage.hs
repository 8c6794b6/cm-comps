{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExtendedDefaultRules #-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-|

Attampt to write polyvariadic variant of 's_new' function.

  s_newp "foo" (-1) AddToHead 1
  == s_new "foo" (-1) AddToHead 1 []

  s_newp "foo" (-1) AddToTail 1 "amp" 0.1 "freq" 440
  == s_new "foo" (-1) AddToTail 1 [("amp",0.1),("freq",440)]

  s_newp "foo" (-1) AddToTail 1 "amp" 0.1 "freq" "c100" "cf" 2000
  == message "/s_new"
     [ ASCII_String "foo", Int32 (-1), Int32 1, Int32 1
     , ASCII_String "amp",  Float 0.1
     , ASCII_String "freq", ASCII_String "c100"
     , ASCII_String "cf",   Float 2000
     ]

The point is, how to build [Datum] used for s_new message from variable number
of arguments.

In ghc-7.8.2, requires /ExtendedDefaultRules/ language pragma, for conveniet
typing in numeric constants:

<http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/interactive-evaluation.html#extended-default-rules>

-}
module Sound.Study.ForUserInterfaces.Scratch.PolyMessage where

import Data.Monoid
import Data.Tree

import Sound.OSC
import Sound.SC3
import Sound.SC3.UGen.MCE
import Sound.SC3.Tree

import Sound.Study.ForUserInterfaces.Scratch.PreSession02 ()

newtype DList a = DList ([a]->[a])
    deriving (Monoid)

snoc :: DList a -> a -> DList a
snoc (DList xs) x = let xs' = xs . (x:) in xs' `seq` DList xs'

class PolyMessage msg where
    message' :: String -> DList Datum -> msg

instance PolyMessage Message where
    message' addr (DList dtm) = message addr (dtm [])

instance (ToDatum d, PolyMessage msg) => PolyMessage (d -> msg) where
    message' addr dtm = \d -> message' addr (dtm `snoc` toDatum d)

class ToDatum d where
    toDatum :: d -> Datum

instance ToDatum Datum where
    toDatum = id

instance ToDatum String where
    toDatum = string

instance ToDatum Double where
    toDatum = float

instance ToDatum Int where
    toDatum = int32

-- | An alias of 'id' to fix type.
toMessage :: Message -> Message
toMessage = id

-- | Polyvariadic variant of 's_new'.
s_new' :: PolyMessage msg => String -> Int -> AddAction -> Int -> msg
s_new' name nid aa tid =
    message' "/s_new" (DList id)
    (string name) (int32 nid) (int32 (fromEnum aa)) (int32 tid)

test01 :: Bool
test01 =
    s_new' "foo" (-1) AddToHead 1 ==
    s_new  "foo" (-1) AddToHead 1 []

test02 :: Bool
test02 =
    s_new' "foo" (-1) AddToTail 1 "amp" 0.1 "freq" 440 ==
    s_new  "foo" (-1) AddToTail 1 [("amp",0.1),("freq",440)]

test03 :: Bool
test03 =
    s_new' "foo" (-1) AddToTail 1 "amp" 0.1 "freq" "c100" ==
    message "/s_new"
    [ string "foo",  int32 (-1), int32 1, int32 1
    , string "amp",  float 0.1
    , string "freq", string "c100"
    ]

{-

  ] node 'a' == Node 'a' []
  ==> node :: Char -> Tree Char

  ] node 'a' 'b' 'c' == Node 'a' [Node 'b' [], Node 'c' []]
  ==> node :: Char -> Char -> Char -> Tree Char

  ] node 'a' (node 'b' 'c') 'd' == Node 'a' [Node 'b' [Node 'c' []], Node 'd' []]
  ==> node :: Node nb => Char -> nb -> Char  -and-
      node :: Node nb => Char -> Char -> nb

-}

class NodeBuilder a nb | nb -> a where
    node :: a -> [Tree a] -> nb

instance NodeBuilder a (Tree a) where
    node a ts = Node a (reverse ts)

instance NodeBuilder a nb => NodeBuilder a (a -> nb) where
    node x ts = \y -> node x (Node y []:ts)


class BranchBuilder a bb | bb -> a where
    branch :: [Tree a] -> bb

instance BranchBuilder a [Tree a] where
    branch = id

instance BranchBuilder a bb => BranchBuilder a (a -> bb) where
    branch ts = \a -> branch (Node a [] : ts)

class TreeElem a where
    toElem :: a -> a
    toElem = id

instance TreeElem Char
instance TreeElem Double
instance TreeElem Bool

node' :: NodeBuilder a nb => a -> nb
node' x = node x []

leaf :: a -> Tree a
leaf = flip Node []

class Builder1 acc ret | ret -> acc where
    build1' :: [acc] -> acc -> ret

instance Builder1 acc [acc] where
    build1' xs x = reverse (x:xs)

instance Builder1 acc ret => Builder1 acc (acc->ret) where
    build1' xs x = \y -> build1' (x:xs) y

build1 :: Builder1 acc ret => acc -> ret
build1 = build1' []

build1_run01 = sum (build1 1 2 3 4 5)
build1_run02 = product (build1 20 30 40)
build1_run03 = length (build1 undefined undefined undefined)


-- another attempt for tree builder

charTree :: Tree Char -> Tree Char
charTree = id

intTree :: Tree Int -> Tree Int
intTree = id

class TreeBuilder a tb | tb -> a where
    _tree :: a -> [Tree a] -> tb

instance TreeBuilder a (Tree a) where
    _tree x ts = Node x (reverse ts)

instance (TrE a, TreeBuilder a tb) => TreeBuilder a (Tree a -> tb) where
    _tree x ts = \t -> _tree x (t:ts)

class TrE e

instance TrE Int

tree :: (TrE a, TreeBuilder a tb) => a -> tb
tree x = _tree x []

#define nil :: Tree Int

-- t4 :: Tree Int
-- t4 =
--     tree 0
--     (tree 1
--      (leaf 2))
--     (tree 4
--      (tree 5
--       (leaf 6) (leaf 7) (leaf 8)))
--     (leaf 9)

-- --------------------------------------------------------------------------
-- Working: Polyvariadic rose tree builder

class Monoid acc => Builder2 acc out ret | ret -> out where
    build2' :: (acc->out) -> acc -> ret

instance (Builder2 acc out ret, e ~ acc) => Builder2 acc out (e->ret) where
    build2' f acc = \s -> build2' f (acc <> s)

-- Building list.

build2 :: Builder2 out out ret => ret
build2 = build2' id mempty

instance Monoid acc => Builder2 acc [e] [e] where
    build2' f acc = f acc

-- >>> intList02
-- [1,2,3,4,5,6]
intList02 :: [Int]
intList02 = build2 [1] [2] [3] [4] [5] [6]

newtype Branch a = Branch {unBranch :: [Tree a]}
   deriving (Eq, Show, Monoid)

instance Monoid acc => Builder2 acc (Branch e) (Branch e) where
    build2' f acc = f acc

bnode :: Builder2 (Branch a) (Branch a) ret => a -> Branch a -> ret
bnode x = build2' (\(Branch b') -> Branch [Node x b'])

bleaf :: a -> Branch a
bleaf x = Branch [Node x []]

headNode :: Branch a -> Tree a
headNode (Branch ts) = case ts of
    []  -> error "headNode: empty branch"
    t:_ -> t

b01 :: Tree Int
b01 = headNode $
    bnode 0
    (bnode 1
     (bleaf 2))
    (bleaf 3)
    (bnode 4
     (bnode 5
      (bleaf 6) (bleaf 7) (bleaf 8)))
    (bleaf 9)

print_b01 :: IO ()
print_b01 = putStrLn $ drawTree $ fmap show b01

-- b01b :: Tree Int
-- b01b = headNode $
--     bnode 0
--     (bnode 1
--      (bleaf 2))
--     (bleaf 3)
--     (bnode 4
--      (bnode 5
--       (bleaves 6 7 8)))
--     (bleaf 9)


-- --------------------------------------------------------------------------
-- Building MCE

instance Monoid acc => Builder2 acc (MCE a) (MCE a) where
    build2' f acc = f acc

unMCE :: MCE a -> [a]
unMCE m = case m of
    MCE_Unit a    -> [a]
    MCE_Vector as -> as

mces :: Builder2 acc acc ret => ret
mces = build2' id mempty

-- Flattening nested MCEs.
--
-- >>> mce_ex01
-- MCE_Vector [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
--
mce_ex01 :: MCE Int
mce_ex01 =
    mces
     1 2 3 4 5
     (mces 6 7 8) 9
     (mces 10
      (mces 11 12 13) 14)
     15

--
-- Building SCNode
--

newtype SCNs a = SCNs {unSCNs :: [a]}
    deriving (Eq, Show, Monoid)

instance Monoid acc => Builder2 acc (SCNs a) (SCNs a) where
    build2' f acc = f acc

grps i = build2' (\(SCNs ns) -> SCNs [Group i ns])

syns i n = SCNs [Synth i n []]

scn_ex0 =
    grps 0
    (grps 1
     (syns 1000 "foo")
     (syns 1001 "bar"))
    (grps 2
     (syns 2001 "buzz"))

print_scn_ex0 :: IO ()
print_scn_ex0 = putStrLn $ drawSCNode $ head $ unSCNs scn_ex0

grps' i = build2' (\ns -> [Group i ns])
syns' i n = [Synth i n []]

scn_ex1 =
    grps' 0
    (grps' 1
     (syns' 1000 "foo")
     (syns' 1001 "bar"))
    (grps' 2
     (syns' 2001 "buzz"))

scn_ex2 =
  grps' 0
  (grps' 1
   (syns' 1000 "foo")
   (syns' 1001 "bar"))
  (grps' 2
   (syns' 2001 "buzz"))

print_scn_ex1 :: IO ()
print_scn_ex1 = putStrLn $ drawSCNode $ head scn_ex1

test_ex1_ex0 = unSCNs scn_ex0 == scn_ex1


-- --------------------------------------------------------------------------
-- Fun with monoid:

newtype Max' a = Max' {unMax::a}
    deriving (Eq, Show, Num)

instance (Bounded a, Ord a) => Monoid (Max' a) where
    mempty = Max' minBound
    mappend (Max' a) (Max' b) = Max' (max a b)

newtype Min' a = Min' {unMin::a}
    deriving (Eq, Show, Num)

instance (Bounded a, Ord a) => Monoid (Min' a) where
    mempty = Min' maxBound
    mappend (Min' a) (Min' b) = Min' (min a b)

data MinMax a = MinMax (Min' a) (Max' a)
   deriving (Eq, Show)

instance (Bounded a, Ord a) => Monoid (MinMax a) where
    mempty = MinMax mempty mempty
    mappend (MinMax a1 b1) (MinMax a2 b2) = MinMax (a1 <> a2) (b1 <> b2)

mm :: a -> MinMax a
mm a = MinMax (Min' a) (Max' a)

mm01 :: (Int, Int)
mm01 =
  case mconcat (map mm [1,100,2,3,4,5]) of
    MinMax a b -> (unMin a, unMax b)
