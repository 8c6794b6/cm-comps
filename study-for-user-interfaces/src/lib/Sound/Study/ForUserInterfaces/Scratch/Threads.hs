{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module playing with client side thread.
-}
module Sound.Study.ForUserInterfaces.Scratch.Threads where

import qualified Config.Dyre as Dyre
import           Config.Dyre.Relaunch
import           Control.Concurrent
import           Control.Monad (void, when)
import           DynFlags
import           System.IO
import           System.Random

import           Sound.OSC hiding (message)
import           Sound.SC3 hiding (withSC3)

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

-- | Forks a thread waiting for message from scsynth server.
a01 :: IO ()
a01 =
  do _ <- forkIO (withSC3 (withNotifications go))
     putStrLn "Forked a thread waiting for /tr message."
  where
    go =
      do myTid <- liftIO myThreadId
         msg <- waitMessage
         case msg of
           dtm@(Message "/tr" [_,Int32 9999,_]) ->
             liftIO (putStr (unlines [show myTid
                                     ,"Datum: " ++ show dtm]))
           _                                    -> go

-- | Sends a synthdef containing 'sendTrig' with trigger ID 9999.
a02 :: IO ()
a02 = sendTrMessage 9999

-- | Fork a thread sending s_new message repeatedly.
a03 :: IO ()
a03 =
  do withSC3 (void (async (d_recv (synthdef "d02" d02))))
     _ <- forkIO go
     putStrLn "Thread forked from a03."
  where
    go =
      do now <- time
         workerThreadId <- forkIO (withSC3 (worker now))
         -- withSC3 (loop workerThreadId)
         withSC3 (withNotifications (loop workerThreadId))
    loop threadIdToKill =
      do msg <- waitMessage
         case msg of
           Message "/tr" [_, Int32 9998, _] ->
             liftIO (killThread threadIdToKill)
           _                                -> loop threadIdToKill
    d02 = out 0 (pan2 (o * 0.08 * e) (k "pan" 0) 1)
    e   = envGen KR 0.1 1 0 (k "dur" 1) RemoveSynth (envPerc 0.01 1)
    o   = sinOsc AR (k "freq" 440) 0
    k   = control KR

-- | Worker thread in 'a03'.
worker :: Transport m => Double -> m ()
worker t0 =
   do didx <- liftIO (getStdRandom (randomR (0, length ds - 1)))
      fidx <- liftIO (getStdRandom (randomR (0, length pchs - 1)))
      let params = [("freq", midiCPS (pchs !! fidx))
                   ,("dur", durs * 8)]
          durps  :: Int
          durps  = truncate ((ds !! didx) * 2000000)
          durs   = fromIntegral durps * 1e-6
          t1     = t0 + durs
      sendBundle (bundle t1 [s_new "d02" (-1) AddToHead 1 params])
      now <- time
      let dlt = t1 - now
      when (dlt > 1)
           (liftIO (threadDelay (ceiling (dlt * 1000000))))
      worker t1
  where
    pchs = foldr fpch [] [36,48,60,72,84]
    fpch o acc = map (+ o) [0,2,4,5,7,9,11] ++ acc
    ds :: [Double]
    ds  = [recip (2**n) | n <- [1,2,3]]

-- | Playing with time stamp in OSC bundle.
sb01 :: IO ()
sb01 =
  withSC3
    (do now <- time
        mapM_ sendOSC [bundle now [f 440, f 880, f 1320]
                      ,bundle (now + 1) [f 330, f 660, f 1320]
                      ,bundle (now + 2) [f 220, f 880, f 1320]])
  where
    f freq = s_new "d02" (-1) AddToTail 1 [("freq",freq)]

-- | Sends a synthdef containing 'sendTrig' with trigger ID 9998.
a04 :: IO ()
a04 = sendTrMessage 9998

-- | Forks given action.
forker01 :: (Double -> Connection TCP ()) -> IO ()
forker01 act =
  do _ <- forkIO
            (do now <- time
                workerThreadId <- forkIO (withSC3 (act now))
                withSC3 (withNotifications (loop workerThreadId)))
     putStrLn "Thread forked by forker01."
  where
    loop threadIdToKill =
      do msg <- waitMessage
         case msg of
           Message "/tr" [_, Int32 9998, _] ->
             liftIO (killThread threadIdToKill)
           _                                -> loop threadIdToKill

-- | Worker loop to send message. Try:
--
-- >>> forker01 worker01
--
-- To stop the loop, invoke 'a04'.
--
worker01 :: (Transport m, Functor m) => Time -> m a
worker01 t0 =
  do freq <- fmap (midiCPS . (pchs !!))
                  (liftIO (getStdRandom (randomR (0, length pchs - 1))))
     pan <- liftIO (getStdRandom (randomR (-1, 1)))
     let act t =
           do durSec <- fmap ((* 0.25) . fromIntegral)
                             (liftIO (getStdRandom (randomR (1,4::Int))))
              let t' = t + durSec
                  params = [("freq",freq),("dur",durSec),("pan",pan)]
              sendBundle (bundle t' [s_new "d02" (-1) AddToHead 1 params])
              now <- time
              when (t' - now > 1)
                   (liftIO
                    (threadDelay (ceiling (1e6 * (t' - now)) - 100000)))
              act t'
     act t0
  where
    pchs = foldr fpch [] [36,48,60,72,84]
    fpch o acc = map (+ o) [0,2,4,5,7,9,11] ++ acc

boo :: [Char]
boo = "boo"

-- | Sends a 'sendTrig' synthdef.
sendTrMessage :: UGen -> IO ()
sendTrMessage trid =
  withSC3
    (send (withCM
             (d_recv (synthdef "d01" d01))
             (s_new "d01" (-1) AddToHead 1 [("tr", 1)])))
  where
    d01 = mrg [sendTrig tr0 trid 0
              ,freeSelf tr0]
    tr0 = tr_control "tr" 0

-- | Referencing itself as TemplateHaskell Name.
printMyName :: IO ()
printMyName =
  putStrLn ("Name of this function is \"" ++ show 'printMyName ++ "\"")


-- --------------------------------------------------------------------------
--
-- Example from Dyre haddock
--
-- --------------------------------------------------------------------------

data Config = Config {message :: String
                     ,errorMsg :: Maybe String}

newtype State = State {bufferLines :: [String]}
  deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config "Dyre Example v0.1" Nothing

showError :: Config -> String -> Config
showError cfg msg = cfg {errorMsg = Just msg}

realMain :: Config -> IO ()
realMain Config { message = msg, errorMsg = eMsg } =
  do State buffer <- restoreTextState (State [])
     maybe (return ()) (\em -> putStrLn ("Error: " ++ em)) eMsg
     putStrLn msg
     mapM_ putStrLn (reverse buffer)
     putStr "> "
     hFlush stdout
     input <- getLine
     case input of
       "exit" -> return ()
       "quit" -> return ()
       _      -> relaunchWithTextState (State (input:buffer)) Nothing

dyreExample :: Config -> IO ()
dyreExample =
  Dyre.wrapMain (Dyre.defaultParams { Dyre.projectName = "dyreExample"
                                    , Dyre.realMain    = realMain
                                    , Dyre.showError   = showError})

-- LocalWords: scsynth sendTrig OSC
--
-- Local Variables:
-- mode: haskell
-- End:
