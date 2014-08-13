{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Scratch to work with patching running nodes in scsynth server.
-}
module Sound.SC3.Patch where

import Sound.SC3.Orphan ()

import Control.Monad (unless)
import Data.Data (Data, Typeable)
import GHC.Generics
import Data.Hashable
import Data.Int (Int32)
import Sound.OSC
import Sound.SC3
import Sound.SC3.ID hiding (hash)
import Sound.SC3.Tree

-- | Anonymous synth nodes.
data An = Ag {-# UNPACK #-} !Int [An]
        | As !Int {-# UNPACK #-} !String {-# UNPACK #-} !UGen
          deriving (Eq, Show, Data, Typeable, Generic)

hashUG :: UGen -> Int32
hashUG = abs . fromIntegral . hash

ppAn :: An -> String
ppAn = drawSCNode . anToSCNode

-- | Get synthdef names used in 'An'.
anSyns :: An -> [(Int,String,UGen)]
anSyns an =
  case an of
    Ag _ ans -> concatMap anSyns ans
    As i n u -> [(i,n,u)]

anToSCNode :: An -> SCNode
anToSCNode a =
  case a of
    Ag i ns  -> Group i (map anToSCNode ns)
    As i n _ -> Synth i n []

ag :: Int -> [An] -> An
ag = Ag

as :: UGen -> An
as ug = As (fromIntegral i) n ug
  where
    i = hashUG ug
    n = "anon_" ++ show i

patch :: Transport m => Int -> An -> m [Message]
patch gid an =
  do sn <- getNode gid
     let sn' = filterSCNode (fmap (>= 0) nodeId) sn
         as_d_recv (_,n,ug) =
           case queryN (synthName ==? n) sn' of
             [] -> [d_recv (synthdef n ug)]
             _  -> []
         drecvs = concatMap as_d_recv (anSyns an)
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

an01 :: An
an01 =
  ag 0
     [ag 999 [as (out 100 (sinOsc AR 110.02 0 * 8))]
     ,ag 1
          [as (out 0 (sinOsc AR 220 (in' 1 KR 100) *
                      (sinOsc KR 0.333 0 * 0.1 + 0.2)))
          ,as (out 1 (sinOsc AR 221 (in' 1 KR 100) *
                      (sinOsc KR 0.328 0 * 0.1 + 0.2)))]
     ,ag 2
         [as (let i = in' 2 AR 0
                  t = linLin (lfdNoise3 'd' KR 0.03 + 2) 1 3 0.05 0.01
                  f x acc = allpassN acc 0.01 t x + acc * 0.5
                  c = foldr f i [0.0032,0.01,0.103,0.238,0.327]
                  a = c * linLin (lfCub KR 3.8 0) (-1) 1 0.4 0.6
              in  replaceOut 0 (limiter a 0.9 0.01))]]

test_ex01 :: IO ()
test_ex01 =
  do withSC3
       (do msgs <- patch 0 an01
           now <- time
           unless (null msgs)
                  (sendOSC (bundle (now + 0.1) msgs))
           send (sync (hash now))
           _ <- waitAddress "/synced"
           r <- getRootNode
           liftIO
             (putStrLn
                (drawSCNode (filterSCNode (fmap (>= 0) nodeId) r))))
