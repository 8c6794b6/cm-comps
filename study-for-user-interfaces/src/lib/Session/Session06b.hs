{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Rewrite of Session06 with using 'runSettings'.
-}
module Session.Session06b where

import Session.Synthdefs
import Sound.Study.ForUserInterfaces.TUI.TUI02

main :: IO ()
main = withSC3 (runSettings g06b)

g06b :: Transport m => Track m ()
g06b =
  do offset 8
     track 0 (do track 1 (track 10 body)
                 track 2 nil)
  where
     body =
       do track 100 t100
          track 101 (do t101
                        amp 8 1.3)
          track 102 (do t102
                        amp 8 1.8)
          track 103 (do t103
                        amp 2 0.6)
          track 104 (do t104
                        amp 2 0.6)
          track 105 (do t105
                        amp 8 0.6)
          track 99 (do t99
                       amp 8 1)
     amp dur val = router (param "amp" (curveTo EnvCub dur val))

t99 :: Transport m => Track m ()
t99 =
  do effect "dc01" (param "wet" (sustain (sval 1)))
     effect "lmt01" (param "wet" (curveTo EnvLin 16 1))

t100 :: Transport m => Track m ()
t100 =
  do metroNode 120 4
     bypass "bypass"
       (do let rep1 = siwhite sinf 1 4
               p x = swhite 1 0 1 <=* (x/100)
               siw2 lo hi = 2 ** siwhite sinf lo hi
           param "i"
                 (sustain
                   (sseq sinf
                         [sseq (siw2 3 8) [0]
                         ,sseq (siw2 3 8) [1]
                         ,sseq (siw2 3 8) [2]]))
           idx <- getInput 100 (synthName ==? "bypass") "i"
           param "0"
                 (sustain
                   (sswitch1
                     (sval idx)
                     [srand sinf
                            [sshuf rep1 [1,1,2]
                            ,sshuf rep1 [2,1,1]
                            ,sshuf rep1 [sseq 1 [3,5], sseq 1 [5,3]]]
                     ,srand sinf
                            [2, 1, 1, 1, 2, 1
                            ,srand (siwhite sinf 1 3)
                                   [srand 1 [4, sseq 1 [2, 2]]
                                   ,sseq 1 [1,1,1,1]]
                            ,4, 8]
                     ,srand sinf
                            [sseq (2 ** siwhite sinf 1 7) [1]]]))
           param "1"
                 (trigger
                   (srand sinf
                          [sseq (4 * siw2 0 3) (map p [5,7,4,6, 95,7,4,8])
                          ,sseq (4 * siw2 0 2) (replicate 8 0)
                          ,sseq (4 * siw2 0 1) (replicate 8 (p 35))])))

t101 :: Transport m => Track m ()
t101 =
  do idx <- getInput 100 (synthName ==? "bypass") "i"
     source "nz01"
            (do param "pan"
                      (sustain
                        (sseq sinf
                         [sseq 24 [0.5]
                         ,siwhite 8 0 1]))
                param "t_tr"
                      (trigger
                       (sswitch1
                         (sval idx)
                         [srand sinf [sseq (srand sinf [4,8]) [0]
                                     ,srand
                                        (srand sinf [1,2,4])
                                        [sseq 1 [1,0,1,srand 1 [0,1]]]]
                         ,sseq sinf [0,0,srand 1 [0,1,1],0]
                         ,srand sinf [sseq 1 [1,0], sseq 1 [1,1]]]))
                param "cf"
                      (sustain
                       (sseq sinf
                             [sseq 3 [sseq 1 p1, sseq 1 p2]
                             ,sseq 1 p1, srand 1 [p3,p4]])))
     effect "ap01"
            (do param "wet" (lfClipNoise 'w' KR 4 * 0.5 + 0.5)
                param "dcy" (sustain (sval 0.95)))
     effect "cmb02"
            (do param "wet"
                      (sustain
                       (sstutter (srand sinf [2,4,8])
                        (srand sinf [1,0])))
                param "dcy"
                      (linExp
                        (lfdNoise0 '0' KR 4 + 1.1) 0.1 1.1 0.1 0.8 `lag` 0.01)
                param "dlt"
                      (\tr ->
                         let sig = linExp nz 0.1 1.1 (recip 100) (recip 33)
                             nz  = lfdNoise3 'd' KR 1 + 1.1
                             gt  = toggleFF (coinGate 'g' 0.15 tr)
                         in  gate sig gt `lag` 0.001))
 where
     b = 0.05
     h = 0.96
     s = 0.18
     p1 = [b,h,h,h, s,h,h,h]
     p2 = [b,h,s,b, h,s,s,b]
     p3 = sseq 1
          [sgeom 4 0.9 0.6
          ,sgeom 4 (0.9* (0.6**4)) (recip 0.6)]
     p4 = swhite 8 0 1

t102 :: Transport m => Track m ()
t102 =
 do idx <- getInput 100 (synthName ==? "bypass") "i"
    let r x = swhite 1 0 1 <=* x
        fa x y = swhite 1 x y
    source "bd03"
     (do param "t_tr0"
               (trigger
                (sswitch1 (sval idx)
                          [sseq sinf [1,     r 0.05, r 0.15, r 0.25
                                     ,r 0.08,r 0.12, r 0.22, r 0.3]
                          ,sseq sinf [1,     r 0.05, r 0.15, r 0.25
                                     ,r 0.2, r 0.12, r 0.12, r 0.12]
                          ,sseq sinf [1, r 0.08, r 0.08, r 0.15
                                     ,1, r 0.08, r 0.12, r 0.08]]))
         param "amp"
               (sustain
                 (sseq sinf [fa 0.9 1.5
                            ,fa 0.3 0.5
                            ,fa 0.5 0.8
                            ,fa 0.5 0.9])))
    effect "ap01"
           (do param "wet" (sustain (sval 0.34))
               param "dcy" (lfCub KR (1/4) 0 * 0.5 + 0.5))
    effect' 1 "ap02"
              (do param "wet" (curveTo EnvLin 64 0)
                  param "dcy" (sustain (sval 4)))
    effect "dc01" (param "wet" (sustain (sval 1)))

t103 :: Transport m => Track m ()
t103 =
 do stut01 <- getInput 100 (synthName ==? "bypass") "0"
    let rot n = map (\x -> modE (x+n) 12) degs
    s03freq <- source "saw03"
      (do param "freq"
           (sustain
            (sstutter
              (sval stut01)
              (midiCPS
               (sseq sinf
                (map (sseq rep1 . rot) [0, 3, 5, 7, 10]) +
               (let lo = srand 1 [3,4]
                    hi = srand 1 [6,7]
                in  12 * sseq sinf [lo,5,5,5, hi,5,5,5]) +
                sstutter
                (2 ** siwhite sinf 5 9)
                (srand sinf [-10, -7, -5, -3])))))
          s03freq <- getInput 103 (synthName ==? "saw03") "freq"
          param "tr"
                (\tr -> changed s03freq 0 + coinGate 'd' 0.05 tr)
          param "en"
                (linLin (lfdNoise1 'E' KR (1/7) + 2) 1 3 (-5) 5)
          param "pan"
                (tRand 'P' 0.45 0.55 (changed s03freq 0))
          param "dur"
                (\tr -> (60/120) * stut01 * (1/4) * tIRand 'd' 1 4 tr)
          param "atk"
                (tExpRand 'A' 1e-3 0.999 (changed s03freq 0))
          return s03freq)
    effect "muladd"
           (do param "wet" (curveTo EnvLin 8 1)
               param "mul" (curveTo EnvCub 4 50))
    effect "clip2"
           (do param "wet" (curveTo EnvLin 4 1)
               param "clip" (curveTo EnvLin 4 0.6))
    effect' 1 "muladd"
              (do param "wet" (curveTo EnvLin 4 1)
                  param "mul" (curveTo EnvCub 4 0.4))
    effect "lpf01"
           (do param "wet" (curveTo EnvLin 8 1)
               param "cf"
                     (\tr ->
                        let tr' = coinGate 'T' 0.01 tr + changed s03freq 0
                            dur = tExpRand 'D' 0.4 0.8 tr'
                            ccf = linExp
                                  (squared (lfdNoise1 'L' KR (5/3)) + 1) 1 2
                                  2000 12000
                        in  decay tr' dur * ccf)
               param "rq"
                 (curveTo EnvLin 8 0.6))
    effect "cmb02"
           (do param "wet" (curveTo EnvCub 32 0.08)
               param "dcy" (curveTo EnvCub 1e-9 4)
               param "dlt" (curveTo EnvCub 1e-9 ((120/60) * (0.98/4))))
    effect "ap02"
           (do param "wet"
                (let df = linLin (sinOsc KR (1/128) 0 + 2) 1 3 (1/128) 128
                  in squared (squared (lfSaw KR df 0)) `lag` 0.2)
               param "dcy" (curveTo EnvCub 64 8))
    effect' 1 "ap02"
              (do param "wet" (mulAdd (sinOsc KR (1/126) 0) 0.5 0.5)
                  param "dcy" (squared (squared (lfSaw KR 2 0)) `lag` 0.1))
    effect "dc01" (param "wet" (curveTo EnvCub 8 1))
  where
    rep1 = siwhite sinf 1 4
    degs = [0,3,5,7,10]

t104 :: Transport m => Track m ()
t104 =
 do stut01 <- getInput 100 (synthName ==? "bypass") "0"
    sfreq <- getInput 103 (synthName ==? "saw03") "freq"
    tr <- getInput 103 (synthName ==? "saw03") "tr"
    source "saw04"
           (do param "freq" (sfreq * 1.000001)
               param "tr" tr
               param "dur" (recip (sfreq/midiCPS 76) * stut01)
               param "atk" (tExpRand 'A' 1e-3 0.999 tr)
               param "pan" (tRand 'ρ' 0.2 0.8 tr)
               param "en" (linLin (lfdNoise1 'E' KR (1 / 7) + 2) 1 3 (-5) 5)
               param "gain" (curveTo EnvLin 8 50)
               param "clp2" (curveTo EnvLin 8 0.6)
               param "rq" (curveTo EnvCub 8 0.6)
               param "cf"
                     (linExp
                       (squared
                          (lfdNoise1 'D' KR (5 / 3)) + 1) 1 2 2000 12000))
    effect "cmb02"
           (do param "wet" (curveTo EnvCub 32 0.08)
               param "dcy" (curveTo EnvCub 1e-9 4)
               param "dlt" (curveTo EnvCub 1e-9 ((120/60) * (0.98/4))))
    effect "ap02"
           (do param "wet"
                 (let df = linLin (sinOsc KR (1/128) 0 + 2) 1 3 (1/128) 128
                  in  squared (squared (lfSaw KR df 0)) `lag` 0.2)
               param "dcy" (curveTo EnvCub 64 8))
    effect' 1 "ap02"
              (do param "wet" (mulAdd (sinOsc KR (1/126) 0) 0.5 0.5)
                  param "dcy" (squared (squared (lfSaw KR 2 0)) `lag` 0.1))
    effect "dc01"
      (param "wet" (curveTo EnvCub 8 1))

t105 :: Transport m => Track m ()
t105 =
  do tr <- getInput 100 (synthName ==? "bypass") "1"
     source "snr01"
            (do param "tr" (coinGate 'κ' prb1 tr)
                param "duro" (curveTo EnvLin 9 0.23)
                param "durn" (curveTo EnvLin 8 0.28)
                param "cf" (linExp cf 1 2 1400 2800)
                param "pan" (curveTo EnvLin 32 0.15)
                param "enn" (ld3 'n' (1/19) (-10) 12)
                param "eno" (ld3 'o' (1/19) (-10) 12)
                param "wdth" (ld3 'w' 3 0.2 0.8)
                param "rq" (rq tr))
     source "snr03"
            (do param "tr" (coinGate 'ι' prb2 tr)
                param "ampm" (tExpRand 'α' 0.8 1 tr)
                param "pan" (curveTo EnvLin 32 (-0.15)))
     effect "ap02"
            (do param "wet" (curveTo EnvLin 4 0.1)
                param "dcy" (curveTo EnvLin 3 0.5))
     effect "dc01" (param "wet" (curveTo EnvLin 2 1))
  where
    cf    = squared (lfdNoise1 'C' KR (2/31)) + 1
    prb1  = (lfdNoise3 'P' KR 1 + 2) * 0.5
    prb2  = (lfdNoise1 'ρ' KR 1 + 2) / 2
    rq tr = envGen KR tr 1 0 0.3 DoNothing
            (Envelope [0.01, 1, 0.03] [1e-4, 1] [EnvCub] Nothing Nothing)
    ld3 i lf = linLin (lfdNoise3 i KR lf + 2) 1 3


-- --------------------------------------------------------------------------
--
-- * Auxiliary functions
--
-- --------------------------------------------------------------------------

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

sendSynthdefs :: IO ()
sendSynthdefs =
 do withSC3 (mapM_ (async . d_recv) synthdefs)
    putStrLn "done"

resetSession06b :: IO ()
resetSession06b = withSC3 resetTUI02Settings

nil :: Monad m => m ()
nil = return ()

-- Localwords: bpm lmt nz ap dcy cmb dcy dlt bd dur atk clp rq duro durn enn eno
-- LocalWords: wdth ampm snr haskell lpf

-- Local Variables:
-- mode: haskell
-- indent-tabs-mode: nil
-- End:
