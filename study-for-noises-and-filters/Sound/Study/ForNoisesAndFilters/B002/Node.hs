{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable
-}
module Sound.Study.ForNoisesAndFilters.B002.Node where

import Sound.SC3
import Sound.SC3.Lepton hiding (s)
import Sound.Study.ForNoisesAndFilters.B002.Synthdef hiding (go)

go :: IO ()
go = withSC3 $ patchNode n0

w = withSC3

n0 :: SCNode
n0 =
  g 0
  [g 1
   [g 10 -- master control
    [s 1001 "b002met"
     ["bpm":=290,"outt":=100,"outb":=101]
    ]
   ,g 20 -- controls
    [s 2001 "quickNoiseC"
     ["outt":=201,"outf":=202,"t_trig":<-100]
    ,s 2002 "boscC"
     ["outg":=203,"outf":=204,"t_trig":<-100]
    ]
   ,g 30 -- sources
    [s 3001 "quickNoise"
     ["out":=20,"t_trig":<-201,"freq":<-202]
    ,s 3002 "bosc"
     ["out":=22,"gt":<-203,"freq":<-204]
    ]
   ,g 40 -- effects
    []
   ,g 50 -- mix
    [s 5001 "b002mix2" -- quickNoise
     ["out":=0,"a_inl":<=20, "a_inr":<=21,"amp":=1.4]
    ,s 5002 "b002mix2" -- bosc
     ["out":=0,"a_inl":<=22, "a_inr":<=23,"amp":=0.25]
    ]
   ,g 99 -- master
    [s 9901 "b002mst"
    ["amp":=1,"a_inl":<=0,"a_inr":<=1]]
   ]]

g :: NodeId -> [SCNode] -> SCNode
g = Group

s :: NodeId -> SynthName -> [SynthParam] -> SCNode
s = Synth