{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Node graph for /end of summer/.

-}
module Sound.Bogus.EndOfSummer.Nodes where

import Sound.SC3
import Sound.SC3.Lepton

t1 :: SCNode
t1 =
  Group 0
    [Group 1
      [Group 10
        [Group 100
         [Synth 1001 "lfnz"
           ["out":=100,"mul":=1500,"add":=1500,"freq":=0.125]
         ,Synth 1002 "lftri"
           ["out":=101,"mul":=1760,"add":=1.8,"freq":=0.5]]
        ,Synth 101 "cefoo"
          ["amp":=0.1,"freq":<-100]
        ,Synth 102 "cefoo"
          ["amp":=0.1,"freq":<-101]]
      ,Group 11
        [Group 110
         [Synth 1101 "lfsin"
           ["out":=110,"mul":=0.1,"add":=0.1,"freq":=0.25]
         ,Synth 1102 "lftri"
           ["out":=111,"mul":=0.1,"add":=0.1,"freq":=0.3]
         ,Synth 1103 "lfnz"
           ["out":=112,"mul":=0.4,"add":=0.4,"freq":=0.4]]
        ,Synth 111 "cebar"
           ["amp":=0.2,"freq":=4399.8]
        ,Synth 112 "cebar"
           ["amp":<-112,"freq":=6601]]
      ,Group 12
        [Group 120
         [Synth 1201 "lfsin"
           ["out":=121,"freq":=0.0232]
         ,Synth 1202 "lftri"
           ["out":=122,"freq":=0.0899]
         ,Synth 1203 "lfnz"
           ["out":=123,"freq":=0.0713]
         ,Synth 1204 "lfsin"
           ["out":=124,"freq":=0.1203]
         ,Synth 1205 "lfnz"
           ["out":=125,"freq":=0.0983]]
        ,Synth 121 "cebuzz"
           ["amp":=0.07,"freq":=440,"pan":<-121]
        ,Synth 122 "cebuzz"
          ["amp":=0.07,"freq":=554.365,"pan":<-122]
        ,Synth 123 "cebuzz"
          ["amp":=0.07,"freq":=660,"pan":<-123]
        ,Synth 124 "cebuzz"
          ["amp":=0.07,"freq":=880,"pan":<-124]
        ,Synth 125 "cebuzz"
          ["amp":=0.07,"freq":=1110,"pan":<-125]]
      ,Group 13
        [Synth 131 "cequux"
          ["amp":=0.4,"freq":=9327,"pan":=0.7]
        ,Synth 132 "cequux"
          ["amp":=0.3,"freq":=3422,"pan":=0.08]
        ,Synth 133 "cequux"
          ["amp":=0.4,"freq":=121,"pan":=(-0.12)]
        ,Synth 134 "cequux"
          ["amp":=0.2,"freq":=1893,"pan":=(-0.6)]]
      ,Group 14
        [Group 141
          [Synth 1410 "lftrig"
            ["freq":=1,"out":=141]
          ,Synth 1411 "lftrig"
            ["freq":=2,"out":=142]]
        ,Synth 1401 "cehoge"
           ["t_trig":<-141]
        ,Synth 1402 "cehoge"
           ["t_trig":<-142,"flo":=880,"fhi":=1320]
        ,Synth 1403 "cehoge"
           ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

t2 =
  Group 0
    [Group 1
      [Group 10
        [Group 100
         [Synth 1001 "lfnz"
           ["out":=100,"mul":=1500,"add":=1500,"freq":=0.125]
         ,Synth 1002 "lftri"
           ["out":=101,"mul":=1760,"add":=1.8,"freq":=0.5]]
        ,Synth 101 "cefoo"
          ["amp":=0.3,"freq":<-100]
        ,Synth 102 "cefoo"
          ["amp":=0.4,"freq":<-101]]
      ,Group 11
        [Group 110
         [Synth 1101 "lfsin"
           ["out":=110,"mul":=0.1,"add":=0.1,"freq":=0.25]
         ,Synth 1102 "lftri"
           ["out":=111,"mul":=0.1,"add":=0.1,"freq":=0.3]
         ,Synth 1103 "lfnz"
           ["out":=112,"mul":=0.4,"add":=0.4,"freq":=0.4]]
        ,Synth 111 "cebar"
          ["amp":=0.2,"freq":=4399.8]
        ,Synth 112 "cebar"
          ["amp":<-112,"freq":=7700]]
      ,Group 12
        [Group 120
         [Synth 1201 "lfsin"
           ["out":=121,"freq":=0.0232]
         ,Synth 1202 "lftri"
           ["out":=122,"freq":=0.0899]
         ,Synth 1203 "lfnz"
           ["out":=123,"freq":=0.0713]
         ,Synth 1204 "lfsin"
           ["out":=124,"freq":=0.1203]
         ,Synth 1205 "lfnz"
           ["out":=125,"freq":=0.0889]]
        ,Synth 121 "cebuzz"
          ["amp":=0.07,"freq":=440,"pan":<-121]
        ,Synth 122 "cebuzz"
          ["amp":=0.07,"freq":=554.365,"pan":<-122]
        ,Synth 123 "cebuzz"
          ["amp":=0.07,"freq":=660,"pan":<-123]
        ,Synth 124 "cebuzz"
          ["amp":=0.07,"freq":=880,"pan":<-124]
        ,Synth 125 "cebuzz"
          ["amp":=0.07,"freq":=1110,"pan":<-125]]
      ,Group 13
        [Synth 131 "cequux"
          ["amp":=0.4,"freq":=9327,"pan":=0.7]
        ,Synth 132 "cequux"
          ["amp":=0.3,"freq":=3422,"pan":=0.08]
        ,Synth 133 "cequux"
          ["amp":=0.4,"freq":=121,"pan":=(-0.12)]
        ,Synth 134 "cequux"
          ["amp":=0.2,"freq":=1893,"pan":=(-0.6)]]
      ,Group 14
        [Group 141
          [Synth 1410 "lftrig"
            ["freq":=1,"out":=141]
          ,Synth 1411 "lftrig"
            ["freq":=2,"out":=142]]
        ,Synth 1401 "cehoge"
          ["t_trig":<-141]
        ,Synth 1402 "cehoge"
          ["t_trig":<-142,"flo":=880,"fhi":=1320]
        ,Synth 1402 "cehoge"
          ["t_trig":<-141,"flo":=180,"fhi":=320]]]]

-- Tree treated as base.
t3 =
  Group 0
  [Group 1
   [Group 14
    [Group 141
     [Synth 1410 "lftrig" ["freq":=1,"out":=141]
     ,Synth 1411 "lftrig" ["freq":=2,"out":=142]]
    ,Synth 1401 "cehoge" ["t_trig":<-141]
    ,Synth 1402 "cehoge" ["t_trig":<-142,"flo":=880,"fhi":=1320]
    ,Synth 1403 "cehoge" ["t_trig":<-141,"flo":=180,"fhi":=320]]]]
