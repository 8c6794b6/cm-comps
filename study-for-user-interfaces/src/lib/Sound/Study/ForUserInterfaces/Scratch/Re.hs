{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
module Sound.Study.ForUserInterfaces.Scratch.Re where

import           DynFlags                                     (HasDynFlags (..))
import           Exception                                    (ExceptionMonad (..))
import           GHC
import           GHC.Paths                                    (libdir)
import           GhcMonad                                     (GhcT (..),
                                                               liftGhcT)

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.IORef

import           Sound.OSC
import           Sound.OSC.FD                                 (close)

import           Sound.Study.ForUserInterfaces.Scratch.Reload (MonadFallback (..), ReloadConfig (..),
                                                               applyAt,
                                                               setupSession)

-- --------------------------------------------------------------------------
--
-- * Fallback wrapper
--
-- --------------------------------------------------------------------------

newtype FallbackT m a = FallbackT {unFallbackT :: IORef HValue -> m a}

liftFallbackT :: Monad m => m a -> FallbackT m a
liftFallbackT m = FallbackT (\_ -> m)

mapGhcT :: (m a -> n b) -> GhcT m a -> GhcT n b
mapGhcT f m = GhcT (\s -> f (unGhcT m s))

mapFallbackT :: (m a -> n b) -> FallbackT m a -> FallbackT n b
mapFallbackT f m = FallbackT (\r -> f (unFallbackT m r))

instance MonadIO m => MonadFallback (FallbackT m) where
  getFallback = FallbackT (\r -> liftIO (readIORef r))
  setFallback hv = FallbackT (\r -> liftIO (writeIORef r hv))

instance Functor m => Functor (FallbackT m) where
  fmap f m = FallbackT (\r -> fmap f (unFallbackT m r))

instance Applicative m => Applicative (FallbackT m) where
  pure x = FallbackT (\_ -> pure x)
  m <*> f = FallbackT (\r -> unFallbackT m r <*> unFallbackT f r)

instance Monad m => Monad (FallbackT m) where
  return x = FallbackT (\_ -> return x)
  m >>= k  = FallbackT (\r -> unFallbackT m r >>= \a -> unFallbackT (k a) r)

instance MonadTrans FallbackT where
  lift = liftFallbackT

instance MonadIO m => MonadIO (FallbackT m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (FallbackT m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (FallbackT m) where
  m `catch` h =
    FallbackT (\r -> unFallbackT m r `catch` (\e -> unFallbackT (h e) r))

instance MonadMask m => MonadMask (FallbackT m) where
  mask f = FallbackT (\r -> mask (\u -> unFallbackT (f (q u)) r))
    where
      q u m = FallbackT (\r -> u (unFallbackT m r))
  uninterruptibleMask f =
    FallbackT (\r -> uninterruptibleMask (\u -> unFallbackT (f (q u)) r))
    where
      q u m = FallbackT (\r -> u (unFallbackT m r))

instance (MonadCatch m, MonadIO m, MonadMask m)
  => ExceptionMonad (FallbackT m) where
  gcatch = catch
  gmask f = mask (\x -> f x)


-- --------------------------------------------------------------------------
--
-- * Reload newtype
--
-- --------------------------------------------------------------------------

newtype ReloadT m a = ReloadT {unReloadT :: GhcT (FallbackT m) a}
  deriving (Functor, Applicative, Monad, MonadIO)

mapReloadT :: (m a -> n b) -> ReloadT m a -> ReloadT n b
mapReloadT f = ReloadT . (mapGhcT (mapFallbackT f)) . unReloadT

instance MonadIO m => MonadFallback (ReloadT m) where
  getFallback = ReloadT (liftGhcT getFallback)
  setFallback v = ReloadT (liftGhcT (setFallback v))

instance MonadTrans ReloadT where
  lift = ReloadT . liftGhcT . liftFallbackT

instance MonadReader r m => MonadReader r (ReloadT m) where
  ask = lift ask
  local = mapReloadT . local

instance MonadState s m => MonadState s (ReloadT m) where
  get = lift get
  put = lift . put

instance (MonadThrow m) => MonadThrow (ReloadT m) where
  throwM = lift . throwM

instance (MonadIO m, MonadMask m, MonadCatch m) => MonadCatch (ReloadT m) where
  m `catch` h = ReloadT (unReloadT m `gcatch` (\e -> unReloadT (h e)))

instance (MonadIO m, MonadMask m) => MonadMask (ReloadT m) where
  mask f =
    wrap (\s ->
            mask
              (\u -> unwrap (f (\m -> (wrap (\s' -> u (unwrap m s'))))) s))
    where
      wrap = ReloadT . GhcT
      unwrap = unGhcT . unReloadT
  uninterruptibleMask f =
    wrap (\s ->
            uninterruptibleMask
              (\u -> unwrap (f (\m -> (wrap (\s' -> u (unwrap m s'))))) s))
    where
      wrap = ReloadT . GhcT
      unwrap = unGhcT . unReloadT

instance (MonadIO m, MonadCatch m, MonadMask m)
   => ExceptionMonad (ReloadT m) where
  gcatch = catch
  gmask f = mask (\x -> f x)

instance (Functor m, MonadCatch m, MonadIO m, MonadMask m)
  => HasDynFlags (ReloadT m) where
  getDynFlags = ReloadT getDynFlags

instance (Functor m, MonadCatch m, MonadIO m, MonadMask m)
  => GhcMonad (ReloadT m) where
  getSession = ReloadT getSession
  setSession hsc_env = ReloadT (setSession hsc_env)

instance SendOSC m => SendOSC (ReloadT m) where
  sendOSC = lift . sendOSC

instance RecvOSC m => RecvOSC (ReloadT m) where
  recvPacket = lift recvPacket

instance DuplexOSC m => DuplexOSC (ReloadT m)

instance Transport m => Transport (ReloadT m)

instance MonadRandom m => MonadRandom (ReloadT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

type Reloadd a = ReloadT IO a

runReloadT
  :: (MonadIO m, MonadMask m, Functor m)
  => ReloadConfig
  -> ReloadT m a
  -> m a
runReloadT config m =
  do packageDbs <- liftIO (rcPackageDbs config)
     sourcePaths <- liftIO (rcSourcePaths config)
     fallback <- liftIO (newIORef (error "runReloadT: empty fallback."))
     unFallbackT
       (runGhcT
          (Just libdir)
          (do target <- setupSession packageDbs sourcePaths (rcTarget config)
              rflag <- load LoadAllTargets
              case rflag of
                Failed    -> error ("Error loading " ++ rcTarget config)
                Succeeded ->
                  case targetId target of
                    TargetModule mdlName -> setContext [IIModule mdlName]
                    _                    -> return ()
              unReloadT m))
       fallback

type OSCRld a = ReloadT (ReaderT UDP IO) a

runOSCRld :: ReloadConfig -> IO UDP -> OSCRld a -> IO a
runOSCRld config udpIO m =
  bracket udpIO close (runReaderT (runReloadT config m))

-- Local Variables:
-- flycheck-haskell-ghc-executable: "ghc-with-ghc"
-- End:
