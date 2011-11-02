{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Rewrite of /Understanding Streams, Patterns, and Events/ example from
SuperCollider help file.

-}
module Sound.Study.Port.SPE where

import System.Random

import Sound.OpenSoundControl
import Sound.SC3 hiding (limiter)
import Sound.SC3.ID hiding (limiter)
import Sound.SC3.Lepton

main :: IO ()
main = withSC3 $ \fd -> do
  setup'spe fd
  patchNode (nodify nspe) fd
  play fd $ toL pspe

setup'spe :: Transport t => t -> IO ()
setup'spe fd = do
  let go (n,u) = send fd . d_recv . synthdef n $ u
  rs <- randomRs (0,0.05) `fmap` newStdGen
  ls <- randomRs (0,0.05) `fmap` newStdGen
  -- speSynth' <- speSynth
  mapM_ go [("speSynth",speSynth),("apf",apf ls rs)]

------------------------------------------------------------------------------
-- Node mapping

nspe :: Nd
nspe =
  grp 0
  [ grp 1
    [ syn "apf" ["out"*=0,"in"*=0] ]]

------------------------------------------------------------------------------
-- Synthdef

-- | Main synthdef.
speSynth :: UGen
speSynth = out 0 (v * ("amp"@@0.3)) where
  v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
  evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
  shp = envPerc 10e-3 1
  nz = midiCPS (lfNoise1 'ζ' KR 1 * 36 + 110)
  freq = control KR "freq" 440

speSynth' :: IO UGen
speSynth' = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ (mkSig dl dr * ("amp"@@0.3))
  where
    mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' KR 1 * 36 + 110)
    freq = control KR "freq" 440


-- | All pass filters
apf :: [Double] -> [Double] -> UGen
apf ls rs = replaceOut ("out"@@0) sig where
  sig = foldr f v (take 4 $ zip ls rs)
  f (l,r) b = allpassN b (0.05) (mce [constant l,constant r]) 4
  v = in' 2 AR ("in"@@0)

------------------------------------------------------------------------------
-- Patterns

pspe = psnew "speSynth" Nothing AddToHead 1
  [("dur",pforever (d 0.13))
  ,("amp",pforever (d 0.6))
  ,("freq",pforever (pmidiCPS pspeF))]

pspeF =
  pconcat
  [ prep (pir 0 1)
    (pconcat (ds [24,31,36,43,48,55]))
  , prep (pir 2 5)
    (pconcat [d 60,prnd (ds [63,65]),d 67,prnd (ds [70,72,74])])
  , prep (pir 3 9)
    (prnd (ds [74,75,77,79,81]))]

------------------------------------------------------------------------------
-- Helpers

i = pint
d = pdouble
ds = map d
pir l h = pirange (i l) (i h)
prnd = prand (i 1)
prep = preplicate


-- SPE builder
mkspe x = let d=pdouble in
  psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever (d 0.13)),("amp", pforever (d 0.1)),("freq", x)]

mkspe' t a x = let d=pdouble in
  psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever t),("amp", pforever a),("freq", x)]

-- Non-unison.
pspe2 = let x = pmidiCPS pspeF in
  ppar
  [ mkspe x
  , mkspe (x *@ pforever (pdouble 0.25))
  , mkspe (x *@ pforever (pdouble 2))]

-- Unison.
pspe3 =
  let d=pdouble; m=pmidiCPS pz
  in  plam tdouble (ppar [mkspe m,mkspe (m *@ d 0.25),mkspe (m *@ d 2)])
      `papp` pspeF

-- Inter octave.
pspe3a =
  let d=pdouble
  in  plam tdouble (mkspe (pmidiCPS $ pconcat [pz-@d 12,pz,pz+@d 12]))
      `papp` pspeF

-- Inter octave unison with repetation.
pspe3b =
  let d=pdouble; i=pint
  in  plam tdouble
      (ppar [mkspe' (d 0.13) (d 0.1) (pmidiCPS $ preplicate (i 4) (pz+@d 12))
            ,mkspe' (d 0.26) (d 0.1) (pmidiCPS $ preplicate (i 2) pz)
            ,mkspe' (d 0.52) (d 0.1) (pmidiCPS (pz-@d 24))])
      `papp` pspeF

-- addspe name p = leptseq =<< bundle' (0.13*8) 0 [l_new name p]

delpat name = leptseq =<< bundle' (0.13*8) 0 [l_free name]
dumpp = leptseq l_dump

-- Unison.
pspe4a =
  let d=pdouble; x=pmidiCPS pz
  in  plam tdouble
       (ppar
        [mkspe x
        ,mkspe (x *@ d 0.9925)
        ,mkspe (x *@ d 1.002)])
       `papp` pspeF

-- Non-unison.
pspe4b =
  let d = pdouble; x=pmidiCPS pspeF
  in  ppar
      [mkspe x
      ,mkspe (x *@ pforever (d 0.9925))
      ,mkspe (x *@ pforever (d 1.002))]

ppp01 =
  let i = pint; pr = preplicate in
  pconcat
    [ pr (i 3)
      (mkspe $ pmidiCPS pspeF)
    , pappend (pr (i 3) pspe3a) (pr (i 3) pspe3b)
    , pr (i 2)
      (pappend (pr (i 3) pspe3) (pr (i 3) pspe2))
    , pr (i 2)
      (pappend (pr (i 2) pspe3) pspe2)
    ]

pbase =
  pconcat
    [prep (pir 0 1)
      (pconcat (ds [24,31,36,43,48,55]))
    ,prep (pir 2 5)
      (pconcat [d 60,prnd (ds [63,65]),d 67,prnd (ds [70,72,74])])
    ,prep (pir 3 9)
      (prnd (ds [74,75,77,79,81]))]

p01 =
  let ts = [0,7,5,2,7,0,4,7,5] in
  ppar
  [mkspe2 (-12) 64
  ,pforever $
   pconcat (map (preplicate (pirange (i 1) (i 4)) . mkbase) ts)]

----

t :: Double
t = 0.13

mkspe2 trans rep =
  let freq = pmidiCPS (body +@ pforever (d trans))
      body = plam tdouble (preplicate (i rep) pz) `papp` (pforever pspeF)
  in  psnew "speSynth" Nothing AddToTail 1
      [ ("dur", pforever (pdouble t))
      , ("amp", pforever (pdouble 0.08))
      , ("freq", freq) ]

mkspe3 ts rs =
  let d=pdouble; m=pz
      fun x y =
        psnew "speSynth" Nothing AddToTail 1
        [ ("dur", pforever (pdouble t))
        , ("amp", pforever (pdouble 0.06))
        , ("freq", preplicate (pint y) (pmidiCPS pz *@ d x))
        ]
  in  plam tdouble (ppar (zipWith fun ts rs)) `papp` (pforever pspeF)


mkbase trans =
  psnew "speSynth" Nothing AddToTail 1
  [ ("dur", pforever (d t))
  , ("freq", pmidiCPS (pbase +@ pforever (d trans))) ]

addspe name tran rep =
  leptseq =<< bundle' (t*8) 0 [l_new name (mkspe2 tran rep)]

addspe2 name trans reps =
  leptseq =<< bundle' (t*8) 0 [l_new name (mkspe3 trans reps)]

delspe name =
  leptseq =<< bundle' (t*8) 0 [l_free name]

{-

addspe2 "spe-u1" [0,-12,12] [1,32,16]
addspe2 "spe-u2" [0.25,1,1.5] [1,1,1]

leptseq l_dump
leptseq l_freeAll

delspe "spe-u2"
delspe "spe-u1"
delspe "spe2"
delspe "spe-lo"
delspe "spe-mid"
delspe "spe"
delspe "spe2"
delspe "spe3"
delspe "spe-mid-2"

addspe "spe" 12 1
addspe "spe" 0 1
addspe "spe" 0 2
addspe "spe" 0 32
addspe "spe" 2 1
addspe "spe" 3 1
addspe "spe" 4 1
addspe "spe" 5 1
addspe "spe" 7 1
addspe "spe" 8 1
addspe "spe" 9 1

addspe "spe-lo" (-12) 128
addspe "spe-lo" 0 6

addspe "spe2" 17 1
addspe "spe2" 19 1
addspe "spe2" 24 1
addspe "spe2" 29 1

addspe "spe3" 0 4
addspe "spe3" 0 4
addspe "spe3" 3 4
addspe "spe3" 4 4
addspe "spe3" 5 4
addspe "spe3" 7 4
addspe "spe3" 9 4

addspe "spe-mid" 0 5
addspe "spe-mid" 0 32
addspe "spe-mid" 0 8
addspe "spe-mid" 2 32
addspe "spe-mid" 3 32
addspe "spe-mid" 4 32
addspe "spe-mid" 5 32
addspe "spe-mid" 7 32
addspe "spe-mid" 8 32
addspe "spe-mid" 9 32
addspe "spe-mid" 0 4
addspe "spe-mid" 0 2

leptseq l_freeAll

withSC3 $ flip send $ dumpOSC NoPrinter
withSC3 $ flip send $ dumpOSC HexPrinter
withSC3 printRootNode

-}
