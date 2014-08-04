{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module to play with haskell-mode, structured-haskell-mode, and various other
functionality with emacs.

-}
module Sound.Study.ForUserInterfaces.Scratch.Emacs where

-- import Sound.Study.ForUserInterfaces.TUI.TUI02
import Sound.OSC
import Sound.SC3
import Sound.SC3.ID

main :: IO ()
main =
  do foo
     bar

synth_foo01 :: UGen
synth_foo01 =
  out
    (control KR "out" 0)
    (pan2
     (sinOsc
        AR freq (sinOsc AR (freq * mfreq) 0 *
                 linLin (lfdNoise3 'I' KR ifreq) (-1) 1 0 8) * 0.1)
     0 1)
  where
    ifreq = linExp (lfdNoise1 'i' KR 0.5 + 1.1) 0.1 2.1 1 18
    mfreq = control KR "mfreq" 1.0001
    freq  = control KR "freq" 165

foo :: IO ()
foo =
  do putStrLn ("foo:" ++ show (3::Int))
     putStrLn ("bar:" ++ show (100::Int))
     putStrLn ("bar:" ++ show (f1 20))
     putStrLn
       (unlines
          [unwords
             ["showing (f1 21):"
             ,show (f1 21)]
          ,unwords
             ["showing (concatMap show [1..5]):"
             ,concatMap show [1..5::Int]]
          ,unwords
             ["showing (f2 3):"
             ,show (f2 3)]])
  where
    f1 :: Int -> Int
    f1 x = x + (100 * 2 ^ (3::Int))
    f2 :: Int -> Int
    f2 x = x * 3

bar :: IO ()
bar =
  do putStr "b"
     putStr "aaaa"
     putStrLn "r"

instance Audible (IO a) where
  play act = liftIO act >> return ()

bar02 :: UGen
bar02 =
  out (control KR "out" 0)
      (pan2
       (mix
        (allpassC
         (product
          [rlpf (lfCub AR
                 ((tChoose 'a' t1 (mce [110, 220, 330, 440, 1320])) +
                  (lfCub KR 0.5 0 * 3.8 + 3.9))
                 0)
                (lfdNoise3 'γ' KR 0.8 * 2800 + 2880)
                (lfdNoise3 'δ' KR 0.3 * 0.4 + 0.41)
          ,decay (sum [dust 'α' KR df])
                 (lfCub KR (1/23) 0 * 0.8 + 0.8)
          ,0.08])
          1.0
          (mce (map f [0,1..10]))
          8))
       0
       1)
  where
    t1 = impulse KR df 0
    df = lfBrownNoise0 'β' KR (3/5) 1 0 * 8 + 8
    f :: Int -> UGen
    f x = rand x 0.1 1.0

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

quux :: String
quux = "QUUX"

-- Local Variables:
-- imenu-auto-rescan: t
-- End:
