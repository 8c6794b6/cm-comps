{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Reload haskell module with GHC within monadic action.
-}
module Control.Monad.Reload
  ( -- * Type class
    MonadReload(..)

    -- * Transformer type
  , ReloadT(..)
  , mapReloadT
  , runReloadT
  , Reload
  , runReload
  , FallbackT(..)
  , liftFallbackT
  , mapFallbackT

    -- * Reload configuration
  , ReloadConfig(..)
  , defaultReloadConfig
  , cabalizedReloadConfig
  , myModuleNameE
  , getCabalSourcePaths
  , getCabalPackageConf
  , extractModuleName

    -- * Reloading
  , reload
  , reloadWith
  , setupSession

    -- * Re-exports
  , HasDynFlags(..)
  , ExceptionMonad(..)
  , GhcMonad(..)
  , PkgConfRef(..)
  , Name
  , ProcessID
  , forkProcess

  ) where

import           Control.Applicative                   (Applicative (..))
import           Control.Monad.Catch                   (MonadCatch (..),
                                                        MonadMask (..),
                                                        MonadThrow (..))
import           Control.Monad.IO.Class                (MonadIO (..))
import           Control.Monad.Reader                  (ReaderT (..))
import           Control.Monad.Reader.Class            (MonadReader (..))
import qualified Control.Monad.RWS.Lazy                as Lazy
import qualified Control.Monad.RWS.Strict              as Strict
import           Control.Monad.State.Class             (MonadState (..))
import qualified Control.Monad.State.Lazy              as Lazy
import qualified Control.Monad.State.Strict            as Strict
import           Control.Monad.Trans                   (MonadTrans (..))
import           Control.Monad.Trans.Identity          (IdentityT (..))
import           Control.Monad.Writer.Class            (MonadWriter (..))
import qualified Control.Monad.Writer.Lazy             as Lazy
import qualified Control.Monad.Writer.Strict           as Strict
import           Data.Char                             (isSpace)
import           Data.IORef                            (IORef, newIORef,
                                                        readIORef, writeIORef)
import           Data.List                             (isPrefixOf, nub)
import           Data.Monoid                           (Monoid)
import           Language.Haskell.TH                   (ExpQ, Name, stringE)
import           Language.Haskell.TH.Syntax            (Loc (..), Quasi (..))
import           System.Directory                      (doesFileExist,
                                                        getCurrentDirectory)
import           System.FilePath                       (takeBaseName, (<.>),
                                                        (</>))
import           System.Posix                          (ProcessID, forkProcess)

import           Distribution.PackageDescription       (BuildInfo (..),
                                                        CondTree (..),
                                                        Executable (..),
                                                        Library (..),
                                                        condExecutables,
                                                        condLibrary)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)

import           DynFlags                              (HasDynFlags (..),
                                                        PackageFlag (..),
                                                        PkgConfRef (..),
                                                        defaultFatalMessager,
                                                        defaultFlushOut,
                                                        defaultLogAction)
import           Exception                             (ExceptionMonad (..))
import           GHC                                   hiding (Name)
import           GHC.Paths                             (libdir)
import           GHC.Prim                              (unsafeCoerce#)
import           GhcMonad                              (GhcT (..), liftGhcT)


-- --------------------------------------------------------------------------
--
-- Type class
--
-- --------------------------------------------------------------------------

-- | Type class with getter and setter for fallback value used during module
-- reload.
class Monad m => MonadReload m where
  -- | Set fallback value.
  setFallback :: HValue -> m ()
  -- | Get fallback value.
  getFallback :: m HValue

instance MonadReload m => MonadReload (IdentityT m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback hv = lift (setFallback hv)
  {-# INLINE setFallback #-}

instance MonadReload m => MonadReload (ReaderT r m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback = lift . setFallback
  {-# INLINE setFallback #-}

instance MonadReload m => MonadReload (Lazy.StateT s m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback = lift . setFallback
  {-# INLINE setFallback #-}

instance MonadReload m => MonadReload (Strict.StateT s m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback = lift . setFallback
  {-# INLINE setFallback #-}

instance (Monoid w, MonadReload m) => MonadReload (Lazy.WriterT w m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback = lift . setFallback
  {-# INLINE setFallback #-}

instance (Monoid w, MonadReload m) => MonadReload (Strict.WriterT w m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback = lift . setFallback
  {-# INLINE setFallback #-}

instance (Monoid w, MonadReload m) => MonadReload (Lazy.RWST r w s m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback  = lift . setFallback
  {-# INLINE setFallback #-}

instance (Monoid w, MonadReload m) => MonadReload (Strict.RWST r w s m) where
  getFallback = lift getFallback
  {-# INLINE getFallback #-}
  setFallback = lift . setFallback
  {-# INLINE setFallback #-}


-- --------------------------------------------------------------------------
--
-- Transformer types
--
-- --------------------------------------------------------------------------

-- | Newtype wrapper to hold fallback value.
newtype FallbackT m a = FallbackT {unFallbackT :: IORef HValue -> m a}

liftFallbackT :: Monad m => m a -> FallbackT m a
liftFallbackT m = FallbackT (\_ -> m)
{-# INLINE liftFallbackT #-}

mapFallbackT :: (m a -> n b) -> FallbackT m a -> FallbackT n b
mapFallbackT f m = FallbackT (\r -> f (unFallbackT m r))
{-# INLINE mapFallbackT #-}

instance MonadIO m => MonadReload (FallbackT m) where
  getFallback = FallbackT (\r -> liftIO (readIORef r))
  {-# INLINE getFallback #-}
  setFallback hv = FallbackT (\r -> liftIO (writeIORef r hv))
  {-# INLINE setFallback #-}

instance Functor m => Functor (FallbackT m) where
  fmap f m = FallbackT (\r -> fmap f (unFallbackT m r))
  {-# INLINE fmap #-}

instance Applicative m => Applicative (FallbackT m) where
  pure x = FallbackT (\_ -> pure x)
  {-# INLINE pure #-}
  m <*> f = FallbackT (\r -> unFallbackT m r <*> unFallbackT f r)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (FallbackT m) where
  return x = FallbackT (\_ -> return x)
  {-# INLINE return #-}
  m >>= k  = FallbackT (\r -> unFallbackT m r >>= \a -> unFallbackT (k a) r)
  {-# INLINE (>>=) #-}

instance MonadTrans FallbackT where
  lift = liftFallbackT
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (FallbackT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadThrow m => MonadThrow (FallbackT m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

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


-- | Newtype wrapper to wrap 'GhcT' with fallback value.
newtype ReloadT m a = ReloadT {unReloadT :: GhcT (FallbackT m) a}
  deriving (Functor, Applicative, Monad, MonadIO, HasDynFlags, GhcMonad)

mapReloadT :: (m a -> n b) -> ReloadT m a -> ReloadT n b
mapReloadT f = ReloadT . (mapGhcT (mapFallbackT f)) . unReloadT
{-# INLINE mapReloadT #-}

mapGhcT :: (m a -> n b) -> GhcT m a -> GhcT n b
mapGhcT f m = GhcT (\s -> f (unGhcT m s))
{-# INLINE mapGhcT #-}

instance MonadIO m => MonadReload (ReloadT m) where
  getFallback = ReloadT (liftGhcT getFallback)
  {-# INLINE getFallback #-}
  setFallback v = ReloadT (liftGhcT (setFallback v))
  {-# INLINE setFallback #-}

instance MonadTrans ReloadT where
  lift = ReloadT . liftGhcT . liftFallbackT
  {-# INLINE lift #-}

instance MonadReader r m => MonadReader r (ReloadT m) where
  ask = lift ask
  {-# INLINE ask #-}
  local = mapReloadT . local
  {-# INLINE local #-}

instance MonadState s m => MonadState s (ReloadT m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

instance (Monoid w, MonadWriter w m) => MonadWriter w (ReloadT m) where
  tell = lift . tell
  {-# INLINE tell #-}
  listen = mapReloadT listen
  {-# INLINE listen #-}
  pass = mapReloadT pass
  {-# INLINE pass #-}

instance (MonadThrow m) => MonadThrow (ReloadT m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

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

type Reload a = ReloadT IO a

-- | Run 'ReloadT' with given settings.
--
-- This works with GHC running as separate OS process.  Takes TemplateHaskell
-- name of 'GhcMonad' action and argument passed to the action. Only single
-- thread could be forked at the same time in each GHC process.  When forking
-- actions with 'runReloadT', use 'forkProcess' instead of 'forkIO' or 'forkOS'.
--
runReloadT ::
  (MonadIO m, MonadMask m, Functor m)
  => ReloadConfig -- ^ Configuration passed to inner 'runGhcT'.
  -> ReloadT m a  -- ^ Action to run.
  -> m a
runReloadT config m =
  do packageDbs <- liftIO (rcPackageDbs config)
     sourcePaths <- liftIO (rcSourcePaths config)
     fallback <- liftIO (newIORef (error "runReloadT: empty fallback."))
     unFallbackT
       (defaultErrorHandler
          defaultFatalMessager
          defaultFlushOut
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
                 unReloadT m)))
       fallback

-- | Type fixed variant of 'runReloadT'.
runReload :: ReloadConfig -> Reload a -> IO a
runReload = runReloadT


-- --------------------------------------------------------------------------
--
-- Configuration
--
-- --------------------------------------------------------------------------

-- | Configuraton for running 'ReloadT'.
data ReloadConfig =
  ReloadConfig
    { -- | Package dbs. Passed to 'extraPkgConfs' field in
      -- 'DynFlags'.
      rcPackageDbs  :: IO [PkgConfRef]
      -- | Source paths. Passed to 'importPaths' field in 'DynFlags'.
     ,rcSourcePaths :: IO [FilePath]
      -- | Bulild target. Passed to 'guessTarget' function.
     ,rcTarget      :: String}

-- | Default configuration to run reloads.
defaultReloadConfig :: ReloadConfig
defaultReloadConfig =
  ReloadConfig {rcPackageDbs = return [GlobalPkgConf,UserPkgConf]
               ,rcSourcePaths = return ["."]
               ,rcTarget = "Main"}


-- | Configuratoin for running ghci process under cabal package directory.
--
-- Have NOT tested thoughroughly, might not work as expected for cabal package
-- with lots of fields and flags.
--
cabalizedReloadConfig :: String -> ReloadConfig
cabalizedReloadConfig target =
  ReloadConfig {rcPackageDbs = getCabalPackageConf
               ,rcSourcePaths = getCabalSourcePaths
               ,rcTarget = target}

-- | Reads cabal sandbox file if it exists, returns 'PkgConfRef' from the file.
getCabalPackageConf :: IO [PkgConfRef]
getCabalPackageConf =
  do pkgDbs <- getSandboxPackageDbs
     if not (null pkgDbs)
        then return (GlobalPkgConf : map PkgConfFile pkgDbs)
        else return [GlobalPkgConf, UserPkgConf]

-- | Get package db from cabal sandbox config file.
getSandboxPackageDbs :: IO [FilePath]
getSandboxPackageDbs =
  do cwd <- getCurrentDirectory
     let sandboxConfigFile = cwd </> "cabal.sandbox.config"
         filterPackageDbLines =
           map (tail . dropWhile (not . isSpace)) .
           filter ("package-db:" `isPrefixOf`) .
           lines
     hasSandboxConfig <- doesFileExist sandboxConfigFile
     if hasSandboxConfig
        then fmap filterPackageDbLines (readFile sandboxConfigFile)
        else return []

-- | Reads cabal config file and gets source directories.
getCabalSourcePaths :: IO [String]
getCabalSourcePaths =
  do cwd <- getCurrentDirectory
     let configFile = cwd </> takeBaseName cwd <.> "cabal"
     hasConfigFile <- doesFileExist configFile
     if hasConfigFile
        then
          do gPkgDesc <- readPackageDescription normal configFile
             let lib = fmap (hsSourceDirs . libBuildInfo . condTreeData)
                            (condLibrary gPkgDesc)
                 exes = fmap (hsSourceDirs . buildInfo . condTreeData . snd)
                             (condExecutables gPkgDesc)
                 sources = maybe exes (: exes) lib
             return (nub (concat sources))
        else return []


-- | Template haskell expression to get current module name.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > module Foo.Bar.Buzz where
-- > ...
-- > myModuleName :: String
-- > myModuleName = $(myModuleNameE)
--
-- In above, @myModuleName@ will be \"Foo.Bar.Buzz\".
--
myModuleNameE :: ExpQ
myModuleNameE = stringE . loc_module =<< qLocation


-- --------------------------------------------------------------------------
--
-- Reloading functions
--
-- --------------------------------------------------------------------------

-- | Reload the module containing given function, then run the function.
reload ::
  (MonadReload m, GhcMonad m)
  => Name        -- ^ Name of function to reload, compile, and run.
  -> m (Maybe t) -- ^ Action to return a value passed to reloaded function, or
                 -- 'Nothing' if reloaded function does not need argument.
  -> m a
reload func mbArg =
  do result <- load (LoadUpTo mdlName)
     case result of
       Failed    -> run =<< getFallback
       Succeeded ->
         do setContext [IIModule mdlName]
            hvalue <- compileExpr (show func)
            setFallback hvalue
            run hvalue
  where
    run hvalue =
      maybe (unsafeCoerce# hvalue) (unsafeCoerce# hvalue) =<< mbArg
    mdlName = mkModuleName (extractModuleName func)

-- | Like 'reload', but use given fallback function.
reloadWith ::
  GhcMonad m
  => Name             -- ^ Function to run after reloading the module.
  -> m (Maybe t)      -- ^ Maybe argument.
  -> (Maybe t -> m a) -- ^ Fallback function.
  -> m a
reloadWith func mbArg fallback =
  do result <- load (LoadUpTo mdlName)
     case result of
       Failed    -> fallback =<< mbArg
       Succeeded ->
         do setContext [IIModule mdlName]
            hvalue <- compileExpr (show func)
            arg <- mbArg
            case arg of
              Nothing   -> unsafeCoerce# hvalue
              Just arg' -> unsafeCoerce# hvalue arg'
  where
     mdlName = mkModuleName (extractModuleName func)

-- | Setup GHC session with given package-dbs and target.
setupSession ::
  GhcMonad m
  => [PkgConfRef] -- ^ Package db file paths (i.e.; arguments passed to ghc
                  -- @-package-db@ option).
  -> [FilePath]   -- ^ Source paths (i.e.; arguments passed to ghc @-i@ option).
  -> String       -- ^ Target, passed to 'guessTarget'.
  -> m Target
setupSession pkgDbs sourcePaths targetSource =
  do dflags <- getSessionDynFlags
     _pkgs <- setSessionDynFlags
               dflags {verbosity = 0
                      ,log_action = silentLogAction
                      ,extraPkgConfs = const pkgDbs
                      ,packageFlags = [ExposePackage "ghc"]
                      ,hscTarget = HscInterpreted
                      ,ghcLink = LinkInMemory
                      ,ghcMode = CompManager
                      ,importPaths = sourcePaths}
     target <- guessTarget targetSource Nothing
     setTargets [target]
     return target
  where
    silentLogAction dflag severity srcSpan style msg =
      case severity of
         SevError   -> return ()
         SevWarning -> return ()
         _          -> defaultLogAction dflag severity srcSpan style msg
{-# INLINEABLE setupSession #-}

-- | Extract module name from template-haskell name.
extractModuleName :: Name -> String
extractModuleName n = reverse (tail (dropWhile (/= '.') (reverse (show n))))
{-# WARNING extractModuleName "Will be removed soon." #-}
