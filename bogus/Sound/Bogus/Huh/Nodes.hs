{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Nodes for /huh/ composition.

-}
module Sound.Bogus.Huh.Nodes where

import Sound.SC3.Lepton
    
n0 :: Nd
n0 =
  let bellpan = syn "cflfnz" ["mfreq"*=0.25,"freq"*=3.333,"out"*=101] in
  grp 0
    [ grp 1
     [ grp 3
       [ syn "lfsin" []
       , syn' 3001 "cf2drn"
         ["out"*=20,"gate"*=1, "amp"*=0]
       , syn' 3002 "cf2drn"
         ["out"*=22,"gate"*=1, "amp"*=0]
       , bellpan
       ]
     ]
    , grp 2
      [ syn "cf2rev" -- huh1
        ["out"*=10,"a_in"*<=abus 10,"dlyt"*=0.2,"dcyt"*=4.8,"mix"*=0.85]
      , syn "cf2rev" -- snr
        ["out"*=14,"a_in"*<= abus 14,"dlyt"*=0.02,"dcyt"*=0.8,"mix"*=0.85]
      , syn "cf2dly" -- bell
        ["out"*=18,"a_in"*<= abus 18,"maxdt"*=0.8]
      ]
    , grp 8
      [ syn "cf2mix" -- huh1
        ["out"*=0,"a_in"*<= abus 10,"amp"*=1.8,"pan"*=0]
      , syn "cf2mix" -- huh2
        ["out"*=0,"a_in"*<= abus 11,"amp"*=1.4,"pan"*=(-0.8)]
      , syn "cf2mix" -- huh3
        ["out"*=0,"a_in"*<= abus 12,"amp"*=1.4,"pan"*=0.8]
      , syn "cf2mix" -- kik
        ["out"*=0,"a_in"*<= abus 13,"amp"*=0.8,"pan"*=0.03]
      , syn "cf2mix" -- snr
        ["out"*=0,"a_in"*<= abus 14,"amp"*=0.55,"pan"*=(-0.1)]
      , syn "cf2mix" -- hat
        ["out"*=0,"a_in"*<= abus 15,"amp"*=0.1,"pan"*=(-0.2)]
      , syn "cf2mixm" -- pu right
        ["out"*=0,"a_in"*<= abus 16,"amp"*=1]
      , syn "cf2mixm" -- pu left
        ["out"*=1,"a_in"*<= abus 17,"amp"*=1]
      , syn "cf2mix"  -- bell
        ["out"*=0,"a_in"*<= abus 18,"amp"*=0.8,"pan"*<-prmv bellpan "out"]
      , syn "cf2mix" -- drn 1
        ["out"*=0,"a_in"*<= abus 20,"amp"*=0.9,"pan"*=(-0.25)]
      , syn "cf2mix" -- drn 2
        ["out"*=0,"a_in"*<= abus 22,"amp"*=0.9,"pan"*=0.25]
      ]
    , grp 9
        [ syn "cf2mst"
          ["out_l"*=0, "out_r"*=1, "amp"*=1]]]
