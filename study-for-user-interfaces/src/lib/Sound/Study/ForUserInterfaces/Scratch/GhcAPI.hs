{-# LANGUAGE TemplateHaskell #-}
{-|

Module to play with ghc package.

-}
module Sound.Study.ForUserInterfaces.Scratch.GhcAPI where

import Control.Concurrent (forkIO, threadDelay)
import DynFlags
import GHC hiding (Name)
import GHC.Paths (libdir)
import Data.Dynamic (fromDyn)
import Language.Haskell.TH.Syntax (Name)
import System.Random (randomR, randomRs, newStdGen)
import Unsafe.Coerce (unsafeCoerce)

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID


-- --------------------------------------------------------------------------
--
-- From "GHC/As a library - Haskell Wiki"
--
-- --------------------------------------------------------------------------

thisModule :: String
thisModule = "Sound.Study.ForUserInterfaces.Scratch.GhcAPI"

sandbox :: FilePath
sandbox  = "/home/atsuro/sandbox/cm/x86_64-linux-ghc-7.8.2-packages.conf.d"

-- | Evaluate given expression having type @a :: IO ()@.
eval :: Show a => a -> IO ()
eval expr =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do dflags <- getSessionDynFlags
        _ <- setSessionDynFlags
               dflags {verbosity = 0
                      ,extraPkgConfs = (PkgConfFile sandbox :)
                      ,packageFlags = [ExposePackage "ghc"
                                      ,ExposePackage "hsc3"
                                      ,ExposePackage "hosc"]
                      ,hscTarget = HscInterpreted
                      ,ghcLink = LinkInMemory
                      ,importPaths = ["dist/build"
                                     ,"src/lib"
                                     ,"dist/build/autogen"]}
        liftIO
          (do putStrLn
                (unlines ["outputFile:    " ++ show (outputFile dflags)
                         ,"dynOutputFile: " ++ show (dynOutputFile dflags)
                         ,"stubDir:       " ++ show (stubDir dflags)
                         ,"dumpDir:       " ++ show (dumpDir dflags)]))
        target <- guessTarget thisModule Nothing
        setTargets [target]
        _ <- load LoadAllTargets
        setContext [IIModule (mkModuleName thisModule)]
        result <- dynCompileExpr (show expr)
        liftIO (fromDyn result (putStrLn "Failed"))))

-- This works quicker than 'eval', but recursion is hardcoded inside this
-- action, which makes difficult to terminate the forked thread.
callback :: Show a => a -> IO ()
callback expr =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do setupSession
        let err = putStrLn ("callback: Error compiling `" ++ show expr ++ "'")
            go :: Ghc ()
            go = do _ <- load LoadAllTargets
                    setContext [IIModule (mkModuleName thisModule)]
                    result <- dynCompileExpr (show expr)
                    liftIO (fromDyn result err)
                    go
        go))

setupSession :: Ghc ()
setupSession =
  do dflags <- getSessionDynFlags
     _ <- setSessionDynFlags
          dflags {verbosity = 0
                 ,extraPkgConfs = (PkgConfFile sandbox :)
                 ,packageFlags = [ExposePackage "ghc"
                                 ,ExposePackage "hsc3"
                                 ,ExposePackage "hosc"]
                 ,hscTarget = HscInterpreted
                 ,ghcLink = LinkInMemory
                 ,importPaths = ["src/lib"]}
     target <- guessTarget thisModule Nothing
     setTargets [target]

withSession :: Ghc () -> IO ()
withSession body =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc (Just libdir)
            (do setupSession
                body))

reloadAndEval :: String -> Ghc ()
reloadAndEval expr =
  do _ <- load LoadAllTargets
     setContext [IIModule (mkModuleName thisModule)]
     value <- compileExpr expr
     unsafeCoerce value

apply :: Name -> a -> Ghc ()
apply func arg =
  do _ <- load LoadAllTargets
     setContext [IIModule (mkModuleName thisModule)]
     hvalue <- compileExpr (show func)
     unsafeCoerce hvalue arg


-- --------------------------------------------------------------------------
--
-- * Using the recursion
--
-- --------------------------------------------------------------------------

send_d04 :: IO Message
send_d04 = withSC3 (async (d_recv (synthdef "d04" def)))
  where
    def = out 0 (pan2 (sinOsc AR (control KR "freq" 440) 0 *
                       envGen KR 1 0.1 0 1 RemoveSynth esh)
                      (lfdNoise3 'p' KR 3.3)
                      1)
    dur = expRand 'a' 0.5 8
    atk = expRand 'b' 0.01 0.5
    esh = envPerc atk dur

foo :: IO ()
foo =
  do putStrLn "Hello, foo"
     threadDelay 500000

bar :: IO ()
bar = putStrLn "This is bar"

-- | This works, but CPU expensive.
a01 :: IO ()
a01 =
  do putStrLn "Yet not working nicely ..."
     threadDelay 100000
     eval 'a01

-- | This works with:
--
-- > forkIO (callback 'a02)
--
-- But need to manage ThreadId, or make compilation error to terminate the
-- thread.
a02 :: IO ()
a02 =
  do g0 <- newStdGen
     let (dt,g1) = randomR (1,3 :: Int) g0
         (n,g2)  = randomR (1,6) g1
         is      = take n (randomRs (0,length pchs - 1) g2)
         pchs    = foldr (\o acc -> map (+ (o + offset)) degs ++ acc) [] octs
         degs    = [0,2,5,7]
         offset  = 0
         octs    = take 7 (iterate (+12) 24)
         notes   = map (pchs !!) is
     withSC3 (sendOSC (bundle immediately (map d04 notes)))
     threadDelay ((2 ^ dt) * 125000)

quux :: IO ()
quux = putStrLn "quux"

buzz2 :: Ghc ()
buzz2  =
  do liftIO a01
     reloadAndEval (show 'a01)

-- | Try:
-- > forkIO (withSession a03)
a03 :: Ghc ()
a03 =
  do liftIO (do g0 <- newStdGen
                let (t,g1) = randomR (0,3::Int) g0
                    (n,g2) = randomR (0,4::Int) g1
                    ixs    = take (2 ^ n) (randomRs (0,length p03s - 1) g2)
                    msgs   = map (\i -> d04 (p03s !! i)) ixs
                withSC3 (sendOSC (bundle immediately msgs))
                threadDelay (250000 * (2 ^ t)))
     reloadAndEval (show 'a03)

p03s :: [Double]
p03s = foldr f z vs
  where
    f o acc = map (+ (o + offset)) degs ++ acc
    z       = []
    vs      = take 6 (iterate (+12) 24)
    degs    = [0,4,7,11]
    offset  = 0

p04s :: [Double]
p04s = map (+ 48) [0, 3, 7, 5, 3, 7, 3, 2]

a04 :: Int -> Ghc ()
a04 i =
  do i' <- liftIO (do let msgs = [d04 n
                                 ,d04 (n + 19)
                                 ,d04 (n - 12)]
                          i'   = i `mod` length ps
                          ps   = p04s
                          n    = ps !! i'
                      withSC3 (sendOSC (bundle immediately msgs))
                      threadDelay 250000
                      return i')
     apply 'a04 (i' + 1)

-- | Try:
-- > forkIO (withSession (a05 (0,5))
a05 :: (Int,Int) -> Ghc ()
a05 (i,n) =
  do (i',n') <- liftIO (do g <- newStdGen
                           let (n',_) = randomR (1,8::Int) g
                               i' = i `mod` length ps
                               freq = ps !! i'
                               msg  = d04 freq
                               ps = p03s
                           withSC3 (sendOSC (bundle immediately [msg]))
                           threadDelay 250000
                           return (i',n'))
     if n == 0
        then apply 'a05 (i' + 1, n')
        else apply 'a05 (i', n - 1)

-- | Update OSC message while thread is running.
--
-- Updating the in-memory link outside of this thread breaks loaded function.
--
playBlah :: IO ()
playBlah =
  do g <- newStdGen
     let (n,_)  = randomR (1,16) g
         is     = take n (randomRs (0,length pchs - 1) g)
         offset = -2
         degs   = [0,2,5,7]
         pchs   = foldr (\o acc -> map (+ (o + offset)) degs ++ acc)
                        []
                        (take 6 (iterate (+12) 24))
     withSC3
      (sendOSC
       (bundle immediately
               (map (\i -> d04 (fromIntegral (pchs !! i) )) is)))
     threadDelay 1000000
     eval 'playBlah

-- | Plays synthdef \"d04\" with given MIDI CPS.
d04 :: Double -> Message
d04 n = s_new "d04" (-1) AddToTail 1 [("freq",midiCPS n)]

-- | Currently playing multiple threads simultaneously will stop the other.
--
-- Recompiling the file and invoking other function than the one playing with
-- running thread will stop the thread.
--
playBuzz :: IO ()
playBuzz =
  do withSC3 (sendOSC (bundle immediately
                              [d04 60
                              ,d04 60.01
                              ,d04 59.99]))
     threadDelay 500000
     eval 'playBuzz
