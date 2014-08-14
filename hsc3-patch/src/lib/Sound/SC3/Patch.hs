{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Scratch to work with patching nodes in scsynth server.
-}
module Sound.SC3.Patch where

import Sound.SC3.Orphan ()

import Control.Applicative
import Control.Monad (unless, when)
import Data.Data (Data, Typeable)
import GHC.Generics
import Data.Hashable
import Data.Int (Int32)
import Data.Monoid
import Sound.OSC
import Sound.SC3
import Sound.SC3.ID hiding (hash)
import Sound.SC3.Tree

-- | Anononymous synth nodes.
data ANode
  = AGroup {-# UNPACK #-} !Int [ANode]
  | ASynth {-# UNPACK #-} !Int !String !UGen
    deriving (Eq, Show, Data, Typeable, Generic)

hashUG :: UGen -> Int32
hashUG = abs . fromIntegral . hash

ppANode :: ANode -> String
ppANode = drawSCNode . anToSCNode

-- | Get synthdef names used in 'ANode'.
anSyns :: ANode -> [(Int, String, UGen)]
anSyns an = foldr f [] [an]
  where
    f x acc = case x of
                AGroup _ ans -> foldr f acc ans
                ASynth i n u -> (i,n,u) : acc

anToSCNode :: ANode -> SCNode
anToSCNode a =
  case a of
    AGroup i ns  -> Group i (map anToSCNode ns)
    ASynth i n _ -> Synth i n []

patch :: Transport m => Int -> ANode -> m [Message]
patch gid an =
  do sn <- getNode gid
     let sn' = filterSCNode (fmap (>= 0) nodeId) sn
         drecvs =
           foldr (\(_,n,ug) acc ->
                    case queryN (synthName ==? n) sn' of
                      [] -> d_recv (synthdef n ug) : acc
                      _  -> acc)
                 []
                 (anSyns an)
         dms = diffMessage sn' (anToSCNode an)
         msgs
           | null dms && null dms = []
           | null dms             = drecvs
           | null drecvs          = dms
           | otherwise            = [go drecvs]
             where
               go (dr:[])  = withCM dr (bundle immediately dms)
               go (dr:drs) = withCM dr (go drs)
     return msgs

ag :: Int -> [ANode] -> ANode
ag = AGroup

as :: UGen -> ANode
as ug = ASynth (fromIntegral i) n ug
  where
    i = hashUG ug
    n = "anon_" ++ show i

-- --------------------------------------------------------------------------
--
-- Builder
--
-- --------------------------------------------------------------------------

-- | Type class for building 'ANode' with polyvariadic function.
class Monoid acc => BuildA acc out ret | ret -> out where
  buildA :: (acc->out) -> acc -> ret

instance (BuildA acc out ret, e ~ acc) => BuildA acc out (e->ret) where
  buildA f acc = \x -> buildA f (acc <> x)

-- | Wrapper newtype for diff list.
newtype DiffList a = DiffList {unDiffList :: [a] -> [a]}
  deriving (Monoid)

-- | Wrapper type to contain intermediate build result.
newtype AList a = AList {unAList :: DiffList a}
  deriving (Monoid)

instance Monoid acc => BuildA acc (AList e) (AList e) where
  buildA f acc = f acc

-- | Convert to 'ANode'. Fail when given 'AList' is empty.
toANode :: AList ANode -> ANode
toANode (AList ns) =
  case unDiffList ns [] of
    []  -> error "toANode: empty node"
    x:_ -> x

-- | Group node with node ID.
gnode :: BuildA (AList ANode) (AList ANode) ret =>
         Int -> AList ANode -> ret
gnode n =
  buildA (\(AList ns) ->
            let ns' = unDiffList ns []
            in  AList (DiffList (AGroup n ns' :)))

-- | Anonymous synth node.
anon :: UGen -> AList ANode
anon u = AList (DiffList (as u :))

-- | Empty node.
nil :: AList a
nil = AList (DiffList id)

--
-- From: <http://www.haskell.org/haskellwiki/Idiom_brackets>
--

class Applicative i => Idiomatic i f g | g -> f i where
  idiomatic :: i f -> g

iI :: Idiomatic i f g => f -> g
iI = idiomatic . pure

data Ii = Ii

instance Applicative i => Idiomatic i x (Ii -> i x) where
  idiomatic xi Ii = xi

instance Idiomatic i f g => Idiomatic i (s -> f) (i s -> g) where
  idiomatic sfi si = idiomatic (sfi <*> si)

-- Needs type signature,
f01 :: (Idiomatic f f1 (Ii -> t), Num f1) => t
f01 = iI (+) (pure 3) (pure 8) Ii

-- ... or unused argument to make GHC quiet.
f02 () = iI (+) (pure 3) (pure 8) Ii
