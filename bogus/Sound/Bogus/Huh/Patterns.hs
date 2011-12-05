{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Huh patterns.

-}
module Sound.Bogus.Huh.Patterns where

import Sound.SC3
import Sound.SC3.Lepton

bpm :: Double
bpm = 295

allP = ppar
  [ huh1P, huh2P, huh3P
  , kikP, snrP, hatP
  , puP, drn1P, drn2P, bellP
  , shwP ]

-- --------------------------------------------------------------------------
-- Helpers

d = pdouble
ds = map pdouble
i = pint

mkSN def out key pat =
  mkSN' def out [(key,pat)]
mkSN2 def out key pat =
  mkSN' def out [("t_trig", pforever (d 1)),(key,pat)]
mkSN3 def out key pat =
  mkSN' def out [("t_trig", pforever (d 1)),("amp",pforever (d 0.3)), (key,pat)]
mkSN' def out kvs =
  psnew def Nothing AddToTail 3
  ([("dur", pforever (d (60/bpm)))
   ,("out", pforever (d out))] ++ kvs)

-- --------------------------------------------------------------------------
-- huhs

huh1P = pconcat (map (mkSN "cf2huh" 10 "t_trig") [as, bs]) where
  as = pseq (i 4) huh1P_a
  bs =
    pcycle
      [ pseq (i 12) huh1P_a
      , pseq (i 4) huh1P_b ]

huh1P_a = ds [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0]
huh1P_b = ds [0,1,0,1, 1,1,0,0, 0,0,0,1, 1,0,0,1]

huh2P = pconcat (map (mkSN "cf2huh" 11 "t_trig") [as, bs]) where
  as = pseq (i 16) huh2P_a
  bs =
    pseq (i 3)
      [ pseq (i 12) huh2P_c
      , pseq (i 14) huh2P_a
      , pseq (i 4) huh2P_b ]

huh2P_a = ds [0,0,0,0]
huh2P_b = ds [0,1]
huh2P_c = ds [1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0]

huh3P = pconcat (map (mkSN "cf2huh" 12 "t_trig") [as, bs]) where
  as = pseq (i 16) huh3P_a
  bs =
    pseq (i 3)
      [ pseq (i 12) huh3P_c
      , pseq (i 14) huh3P_a
      , pseq (i 4) huh3P_b ]

huh3P_a = ds [0,0,0,0]
huh3P_b = ds [1,0]
huh3P_c = ds [0,1,0,1, 1,0,0,0, 0,1,0,1, 0,0,0,1]

-- --------------------------------------------------------------------------
-- Percussive tones

kikP = pconcat (map (mkSN "cf2kik" 13 "t_trig") [kikPa,kikPb])
kikPa =
  pseq (i 4) (ds [1,0,0,0, 1,0,0,0, 0.8,0,0,0, 1,0,0,0])
kikPb =
  pcycle
  [ pseq (i 3)
    [ pseq (i 4)
      [ d 1,   d 0, d 0, d 0
      , d 0.8, d 0, d 0, prand (i 1) (ds [0,0.7,0.8,1])
      , d 0.9, d 0, d 0, d 0
      , d 1,   d 0, d 0, prand (i 1) (ds [0,0.7,0.8,1])]]
  , pseq (i 4)
    (ds [1,0,0,0.7, 1,0,0,1, 0,0.9,0,0.8, 0.9,0,0,1 ])]

snrP = pconcat (map (mkSN "cf2snr" 14 "t_trig") [snrPa,snrPb])
snrPa =
  pconcat
  [ pseq (i 56) (ds [0])
  , pconcat (ds [0.8,0.6,0,0.2, 0.2,0.8,0.4,1.0])]
snrPb =
  let z = d 0; dr l h = pdrange (d l) (d h) in
  pseq (i 3)
    [ pseq (i 3)
      [ pseq (i 3)
          [ pseq (i 3) [z, z, dr 0.6 1, z]
          , pconcat    [z, z, dr 0.6 0.8, dr 0.6 0.8]]
      , pconcat
          [ pseq (i 2)  [z, z, dr 0.6 0.8, d 0]
          , prand (i 8) [z, d 0.5, d 0.75, d 1]]]
    , pseq (i 3)
        [ z, z, z, z
        , dr 0.9 1.0, z, z, z
        , z, z, z, z
        , prand (i 4) (ds [1,0.8,0])]
    , pconcat
        [ z, z, z, z
        , dr 0.9 1.0, z, z, z
        , prand (i 8) (ds [0,0,0,0.5,0.6,0.7,0.8,0.9,1])]]

hatP = pconcat (map (mkSN "cf2hat" 15 "t_trig") [hatPa,hatPb])
hatPa =
  pappend (pseq (i 62) (ds [0])) (pconcat (ds [0.6,0.8]))
hatPb =
  pseq (i 3)
  [ pseq (i 3)
    [ pseq (i 32)
      [ prand (i 1) (ds [0,0,0,0,0,0,0.2])
      , prand (i 1) (ds [0.5,0.8,1.0]) ]]
  , pseq (i 32) (ds [0])
  , pconcat
    [ pseq (i 30) (ds [0])
    , pconcat (ds [0.6,0.8]) ]]

-----------------------------------------------------------------------------
-- Pitched tones

puP = mkSN3 "cf2pu" 16 "freq" (pmidiCPS puPa)
puPa =
  pcycle
   [ prand (i 7)
     [ pconcat (ds [36,55,62,36, 55,62,36,55])
     , pconcat (ds [36,60,72,36, 60,72,36,60])
     , pconcat (ds [36,53,58,36, 53,58,36,53]) ]
   , d 36, prand (i 2) (ds [60,67])
   , d 36, prand (i 2) (ds [67,72])
   , prand (i 2) (ds [48,53,55,60,65,67])]

drn1P =
  let f = pmidiCPS . pdouble; z = d 0 in
  pnset 3001
  [ ("dur", pforever (d (60/bpm)))
  , ("amp", pforever (d 0.3))
  , ("freq",
     pconcat
     [ pseq (i 32) (ds [0])
     , pseq (i 3)
       [ pseq (i 3)
         [ f 72, z, z, z,  z, z, z, z,  z, f 67, z, z,  f 65, z, z, z
         , f 67, z, z, z,  z, z, z, z,  z, z,    z, z,  f 65, z, z, z
         , f 60, z, z, z,  z, z, z, z,  z, f 55, z, z,  f 65, z, z, z
         , f 67, z, z, z,  z, z, z, z,  z, z,    z, z,  z,    z, z, z ]
       , pconcat
         [ f 72, pseq (i 31) [z]
         , f 60, pseq (i 31) [z]]]])]

drn2P =
  let f = pmidiCPS . pdouble; z = d 0 in
  pnset 3002
  [ ("dur", pforever (d (60/bpm)))
  , ("amp", pforever (d 0.3))
  , ("freq",
     pconcat
     [ pseq (i 32) (ds [0])
     , pseq (i 3)
       [ pseq (i 3)
         [ z, z, f 55,  z, z,    z, f 60, z,  z, z, z, z,  z,    z, z, z
         , z, z, z,     z, f 67, z, z,    z,  z, z, z, z,  f 60, z, z, z
         , z, z, z,     z, z,    z, f 67, z,  z, z, z, z,  z,    z, z, z
         , z, z, z,     z, f 60, z, z,    z,  z, z, z, z,  z,    z, z, z ]
       , pconcat
         [ z, z, f 55, z,  pseq (i 28) [z]
         , z, z,    z, z,  z, z, f 67, z, pseq (i 24) [z]]]]) ]

bellP = mkSN2 "cf2bell" 18 "freq" bellPa
bellPa =
  pconcat
  [ pseq (i 16) (ds [0,0,0,0])
  , pseq (i 6) (ds [0,0,0,0])
  , pcycle
    [ pmidiCPS $ prand (i 16) $ map pdouble $
      replicate 12 0 ++ [65,67,72,77,79,84,89,91,96,103,110]
    , pseq (i 12) (ds [0,0,0,0])]]

-- --------------------------------------------------------------------------
-- SE

shwP =
  let z = d 0; o = d 1 in
  pnset 3005
    [ ("dur",
       pforever (d (60/bpm)))
    , ("t_trig",
       pcycle
         [z,z,z,z, z,z,z,z, z,z,z,z, z,z,z,z
         ,z,z,z,z, z,z,z,z, z,z,z,z, z,z,z,z
         ,z,z,z,z, z,z,z,z, z,z,z,z, z,z,z,z
         ,z,o,o,z, o,o,z,o, o,z,o,o, z,o,o,z

         ,o,o,z,o, o,z,o,o, z,o,o,z, o,o,z,o
         ,o,z,o,o, z,o,o,z, o,o,z,o, o,z,o,o
         ,z,z,z,z, z,z,z,z, z,z,z,z, z,z,z,z
         ,z,z,z,z, z,z,z,z, z,z,z,z, z,z,z,z
         ])
    , ("t_envr",
       pcycle
         [z,z,z,z, z,z,z,z, z,z,z,z, z,z,z,z
         ,z,o,z,z, z,z,z,z, z,z,z,z, z,z,z,z])
    ]
