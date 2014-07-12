{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.Study.ForUserInterfaces.Scratch.OSCRec
  ( module Sound.Study.ForUserInterfaces.Scratch.OSCRec
  , module Sound.Study.ForUserInterfaces.Scratch.Reload
  ) where

import           Sound.Study.ForUserInterfaces.Scratch.Reload

import           Control.Applicative (Applicative)
import           Control.Monad.Catch
import           Control.Monad.Random                         (MonadRandom (..))
import           Control.Monad.Reader
import           Sound.OSC                                    (DuplexOSC,
                                                               RecvOSC (..),
                                                               SendOSC (..),
                                                               Time, Transport,
                                                               pauseThreadUntil,
                                                               time,
                                                               withTransport)

import qualified Sound.OSC.Transport.FD.UDP                   as UDP


-- --------------------------------------------------------------------------
--
-- OSC wrapper
--
-- --------------------------------------------------------------------------

-- | Newtype wrapper for recursive OSC action.
newtype OSCRec a = OSCRec {unOSCRec :: ReloadT (ReaderT UDP.UDP IO) a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask,
     HasDynFlags, ExceptionMonad, GhcMonad, MonadReload)

instance SendOSC OSCRec where
  sendOSC = OSCRec . lift . sendOSC

instance RecvOSC OSCRec where
  recvPacket = OSCRec (lift recvPacket)

instance DuplexOSC OSCRec
instance Transport OSCRec

instance MonadRandom OSCRec where
  getRandom = liftIO getRandom
  getRandoms = liftIO getRandoms
  getRandomR = liftIO . getRandomR
  getRandomRs = liftIO . getRandomRs

-- | Run 'OSCRec' with given settings.
runOSCRec ::
  ReloadConfig  -- ^ Setting for 'ReloadT'.
  -> IO UDP.UDP -- ^ Destination of OSC messages.
  -> OSCRec a   -- ^ Action to run.
  -> IO a
runOSCRec config udpIO m = withTransport udpIO (runReloadT config (unOSCRec m))

-- | Apply function with given argument after reloading the module.
--
applyAt ::
  (MonadReload m, GhcMonad m)
  => Time -- ^ Scheduled time.
  -> Name -- ^ Name of function to evaluate.
  -> t    -- ^ Argument passed to the function.
  -> m b
applyAt scheduledTime func arg =
  reload func (do pauseThreadUntil scheduledTime
                  return (Just arg))


-- --------------------------------------------------------------------------
--
-- Client side synchronization
--
-- --------------------------------------------------------------------------

-- | Data type to manage client side time synchronization.
data Metro =
  Metro {beatsPerMinute :: {-# UNPACK #-} !Rational
        ,oneBeat        :: {-# UNPACK #-} !Rational
        ,currentBeat    :: !(Time -> Rational)
        ,nextOffset     :: !(Int -> Time -> Time)}

instance Show Metro where
  show m = "Metro {beatsPerMinute = " ++ show (beatsPerMinute m) ++ "}"

-- | Returns a 'Metro' with given beats per minute.
mkMetro :: Rational -> Metro
mkMetro bpm =
  let m = Metro {beatsPerMinute = bpm
                ,oneBeat = 60 / bpm
                ,currentBeat = \t -> (toRational t / oneBeat m)
                ,nextOffset = \n t ->
                  let gridDuration = oneBeat m * fromIntegral n
                      numGrids :: Int
                      (numGrids, _) =
                        properFraction
                          (toRational t / (fromIntegral n * oneBeat m))
                  in  realToFrac (gridDuration * fromIntegral (numGrids + 1)) }
  in  m `seq` m

beatDuration :: Metro -> Double
beatDuration = fromRational . oneBeat

-- | Get next offset from current time.
getOffset :: Metro -> Int -> IO Time
getOffset m n = nextOffset m n `fmap` time

{-

--
-- Seems like, when running bytecode, data type with function records
-- performs better than defining each function as top level in this module.
-- Check it again later.
--

newtype Metro = Metro {beatsPerMinute :: Double}

mkMetro :: Double -> Metro
mkMetro = Metro

beatDuration   :: Metro -> Double
beatDuration (Metro bpm) = 60 / bpm
{-# INLINE beatDuration #-}

currentBeat :: Metro -> Time -> Int
currentBeat m t = fst (properFraction (t / beatDuration m))
{-# INLINE currentBeat #-}

nextOffset :: Metro -> Int -> Double -> Double
nextOffset m n t =
  let gridDuration = beatDuration m * fromIntegral n
      numGrids :: Int
      numGrids = fst (properFraction (t / (fromIntegral n * beatDuration m)))
  in  gridDuration * fromIntegral (numGrids + 1)
{-# INLINE nextOffset #-}

-}
