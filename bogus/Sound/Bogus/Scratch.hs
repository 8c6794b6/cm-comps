{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Scratch for bogus.

-}
module Sound.Bogus.Scratch where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

pfsm002 = pfsm [0]
  [(mkfsm02 [0,4,7],    [1,2,3,4]) -- 0
  ,(mkfsm02 [2,5,9],    [3])       -- 1
  ,(mkfsm02 [0,4,9],    [0,1,3,4]) -- 2
  ,(mkfsm02 [2,5,7,-1], [0,4])     -- 3
  ,(mkfsm02 [0,4,9],    [1,2,3])   -- 4
  ]

pfinite001 =
  let i = pint; d = pdouble; ds = map d
      fs = ds [midiCPS (y+x)| x <- [0,2,4,5,7,9,11], y <- [48,60]]
  in  psnew "rspdef1" Nothing AddToTail 1
      [("dur",
        (prand (i 32)
         [ d 0.25
         , pconcat [d 0.125, d 0.125]
         , pconcat (ds (replicate 4 6.25e-2))]))
      ,("freq", pmidiCPS $ pforever (prand (i 1) fs))
      ,("atk", pforever $ prand (i 1) (ds [0.998,0.5,0.002]))
      ]

mkfsm02 fs =
  let fs' = concatMap (\x -> map d [x+48,x+60,x+72{-,x+84-}]) fs
      d = pdouble; i = pint
  in psnew "rspdef1" Nothing AddToTail 1
     [("dur",  pforever (prand (i 1) (map d [0.125,0.125,0.125])))
     ,("freq", pmidiCPS $ pforever (prand (i 1) fs'))
     ,("atk",  pforever $ pdrange (d 1e-4) (d 3e-3))
     ,("dcy",  pforever $ pdrange (d 1e-1) (d 1))
     ,("pan",  pforever $ prand (i 1) (map d [-1,-0.5,0,0.5,1]))
     ,("amp",  preplicate (i 16) (pdrange (d 0.4) (d 0.6)))]
