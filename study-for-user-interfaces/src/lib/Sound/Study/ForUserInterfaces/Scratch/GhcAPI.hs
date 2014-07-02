{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module to periodically reload haskell Module with GHC APIs.
-}
module Sound.Study.ForUserInterfaces.Scratch.GhcAPI where

import           Control.Applicative
import           Control.Monad              (ap, liftM)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Random       (MonadRandom (..))
import           Data.Dynamic               (fromDyn)
import           Data.IORef
import           Data.List                  (intersperse)
import           Language.Haskell.TH.Syntax (Name)

import           Sound.OSC                  (DuplexOSC, RecvOSC (..),
                                             SendOSC (..), Time, Transport,
                                             pauseThreadUntil)
import qualified Sound.OSC.Transport.FD     as FD

import           DynFlags                   (HasDynFlags (..), PackageFlag (..),
                                             PkgConfRef (..),
                                             defaultFatalMessager,
                                             defaultFlushOut)
import           Exception                  (ExceptionMonad (..))
import           GHC                        hiding (Name)
import           GHC.Paths                  (libdir)
import           GHC.Prim                   (unsafeCoerce#)


-- Inspired from GhciMonad.Ghci, see how it's holding Ghc inside.
-- It is reader Monad using IORef.
newtype Rec t a = Rec {unRec :: RecEnv t -> Ghc a}

data RecEnv t =
  RecEnv {reHValueRef :: {-# UNPACK #-} !(IORef HValue)
         ,reTransport :: {-# UNPACK #-} !t}

instance MonadRandom (Rec t) where
  {-# INLINE getRandom #-}
  getRandom = liftIO getRandom
  {-# INLINE getRandoms #-}
  getRandoms = liftIO getRandoms
  {-# INLINE getRandomR #-}
  getRandomR = liftIO . getRandomR
  {-# INLINE getRandomRs #-}
  getRandomRs = liftIO . getRandomRs

instance FD.Transport t => SendOSC (Rec t) where
  {-# INLINE sendOSC #-}
  sendOSC o = Rec (\e -> liftIO (FD.sendOSC (reTransport e) o))

instance FD.Transport t => RecvOSC (Rec t) where
  {-# INLINE recvPacket #-}
  recvPacket = Rec (liftIO . FD.recvPacket . reTransport)

instance FD.Transport t => DuplexOSC (Rec t)

instance FD.Transport t => Transport (Rec t)

-- | Getter and setter fallback value for module reload.
class HasFallback h where
  setFallback :: HValue -> h ()
  getFallback :: h HValue

instance HasFallback (Rec t) where
  {-# INLINE setFallback #-}
  setFallback hvalue = Rec (\e -> liftIO (writeIORef (reHValueRef e) hvalue))
  {-# INLINE getFallback #-}
  getFallback = Rec (\e -> liftIO (readIORef (reHValueRef e)))

-- | Setup GHC session and run 'Rec' with given arguments.
runRecWith ::
  FilePath          -- ^ Path to package db.
  -> String         -- ^ Name of target source.
  -> IO t           -- ^ Transport to send OSC messages.
  -> IO a           -- ^ 'IO' action returning initial argument.
  -> (a -> Rec t b) -- ^ Function taking argument and returns 'Rec' to run.
  -> IO b
runRecWith pkgConf targetSource transport arg frec =
  do ref <- newIORef (error "runRecWith: fallback not initialized.")
     transport' <- transport
     let env = RecEnv {reHValueRef = ref
                      ,reTransport = transport'}
     defaultErrorHandler
       defaultFatalMessager
       defaultFlushOut
       (runGhc
          (Just libdir)
          (do setupSession pkgConf targetSource
              rflag <- load LoadAllTargets
              case rflag of
                Failed    -> error "setupSession: failed loading module."
                Succeeded -> return ()
              arg' <- liftIO arg
              unRec (frec arg') env))

-- | Like 'runRecWith', but without initial argument.
runRec :: FilePath -> String -> IO t -> Rec t a -> IO a
runRec pkgConf targetSource transport body =
  runRecWith pkgConf targetSource transport (return ()) (\_ -> body)

liftGhc :: Ghc a -> Rec t a
liftGhc m = Rec (\_ -> m)

instance Monad (Rec t) where
  {-# INLINE (>>=) #-}
  Rec m >>= k = Rec (\r -> m r >>= \a -> unRec (k a) r)
  {-# INLINE return #-}
  return = liftGhc . return

instance Functor (Rec t) where
  {-# INLINE fmap #-}
  fmap = liftM

instance Applicative (Rec t) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance MonadIO (Rec t) where
  {-# INLINE liftIO #-}
  liftIO = liftGhc . liftIO

instance ExceptionMonad (Rec t) where
  {-# INLINE gcatch #-}
  gcatch m h = Rec (\r -> unRec m r `gcatch` (\e -> unRec (h e) r))
  {-# INLINE gmask #-}
  gmask f = Rec (\r -> gmask (\g -> let f' (Rec m) = Rec (\r' -> g (m r'))
                                    in  unRec (f f' ) r))

instance HasDynFlags (Rec t) where
  {-# INLINE getDynFlags #-}
  getDynFlags = liftGhc getDynFlags

instance GhcMonad (Rec t) where
  {-# INLINE getSession #-}
  getSession = liftGhc getSession
  {-# INLINE setSession #-}
  setSession = liftGhc . setSession

-- | Evaluate given expression having type @a :: IO ()@.
eval :: Show a => FilePath -> String -> a -> IO ()
eval pkgConf callerModule expr =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do let thisModule = callerModule
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags
               dflags {verbosity = 0
                      ,extraPkgConfs = (PkgConfFile pkgConf :)
                      ,packageFlags = [ExposePackage "ghc"]
                      ,hscTarget = HscInterpreted
                      ,ghcLink = LinkInMemory
                      ,importPaths = ["dist/build"
                                     ,"src/lib"
                                     ,"dist/build/autogen"]}
        target <- guessTarget thisModule Nothing
        setTargets [target]
        _ <- load LoadAllTargets
        setContext [IIDecl (simpleImportDecl (mkModuleName thisModule))]
        result <- dynCompileExpr (show expr)
        liftIO (fromDyn result (putStrLn "Failed"))))


-- | Recursively load and evaluate given function name.
recurse ::
  FilePath  -- ^ Path to package conf file.
  -> Name   -- ^ Name of function to recurse.
            --
            -- Expecting an IO action having type @(a -> IO a)@, an IO action
            -- which returns next state for itself to update the state
            -- recursively.
            --
  -> a      -- ^ Initial argument.
  -> IO b
recurse pkgConf expr arg =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do let sourceModule = moduleOfFunctionName expr
            myModuleName = mkModuleName sourceModule
            go x fallback =
               do rflag <- load LoadAllTargets
                  case rflag of
                    Failed ->
                      do x' <- liftIO ((unsafeCoerce# fallback :: a -> IO a) x)
                         go x' fallback
                    Succeeded ->
                      do setContext [IIDecl (simpleImportDecl myModuleName)]
                         result <- compileExpr (show expr)
                         x' <- liftIO ((unsafeCoerce# result :: a -> IO a) x)
                         go x' result
        setupSession pkgConf sourceModule
        go arg (error "recurse: Failed the initial load.")))

-- | Pause thread until given time, then return given value.
returnAt :: MonadIO m => Time -> a -> m a
returnAt t a = pauseThreadUntil t >> return a

withGhc :: FilePath -> String -> Ghc () -> IO ()
withGhc pkgConf targetSource body =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc (Just libdir)
            (do setupSession pkgConf targetSource
                body))

-- | Setup GHC session with given package conf file and target.
setupSession :: GhcMonad m => FilePath -> String -> m ()
setupSession pkgConf targetSource =
  do dflags <- getSessionDynFlags
     _pkgs <- setSessionDynFlags
               dflags {verbosity = 0
                      ,extraPkgConfs = const [GlobalPkgConf
                                             ,PkgConfFile pkgConf]
                      -- ,extraPkgConfs = (PkgConfFile pkgConf :)
                      -- ,extraPkgConfs = const [PkgConfFile pkgConf]
                      ,packageFlags = [ExposePackage "ghc"]
                      ,hscTarget = HscInterpreted
                      ,ghcLink = LinkInMemory
                      ,ghcMode = CompManager
                      ,importPaths = ["src/lib"]}
     target <- guessTarget targetSource Nothing
     setTargets [target]

-- | This works with GHCs running with separate OS process.
--
-- Takes TemplateHaskell name of function, possibly updated argument, and an
-- action to take when failed to reload the module. Only single thread could be
-- forked at the same time.
--
apply :: GhcMonad m => Name -> t -> (t -> m a) -> m a
apply func arg fallback =
  do --
     -- Tryed 'DriverPipeline.linkBinary'. When linking binary, there will be no
     -- success flag since no compilation will happen here.
     -- 'DriverPipeline.linkBinary' was for static lib and dynamic lib. Need to
     -- load interface before linking object file. Following did not work:
     --
     --   hsc_env <- getSession
     --   dflags <- getSessionDynFlags
     --   mg <- depanal [] False
     --   mapM_ (\mdl -> do pm <- parseModule mdl
     --                     tm <- typecheckModule pm
     --                     loadModule tm) mg
     --   result <- liftIO (link (ghcLink dflags)
     --                          dflags
     --                          False
     --                          (hsc_HPT hsc_env))
     --
     let thisModuleName = mkModuleName (moduleOfFunctionName func)
     result <- load (LoadUpTo thisModuleName)
     case result of
       Failed    -> fallback arg
       Succeeded ->
         do setContext [IIModule thisModuleName]
            hvalue <- compileExpr (show func)
            unsafeCoerce# hvalue arg

apply' :: GhcMonad m => Name -> t -> m ()
apply' n arg = apply n arg (\_ -> liftIO (putStrLn "Compilation failed."))

applyAt :: (HasFallback m, GhcMonad m) => Time -> Name -> t -> m b
applyAt scheduledTime func arg =
  do let thisModuleName = mkModuleName (moduleOfFunctionName func)
     result <- load (LoadUpTo thisModuleName)
     case result of
       Failed    ->
         do fallback <- getFallback
            pauseThreadUntil scheduledTime
            unsafeCoerce# fallback arg
       Succeeded ->
         do setContext [IIModule thisModuleName]
            hvalue <- compileExpr (show func)
            setFallback hvalue
            pauseThreadUntil scheduledTime
            unsafeCoerce# hvalue arg


moduleOfFunctionName :: Name -> String
moduleOfFunctionName name = concat (intersperse "." (init (ns (show name))))
  where
    ns [] = [""]
    ns xs = let (pre, post) = break (== '.') xs
            in  pre : if null post
                         then []
                         else ns (tail post)


-- Local Variables:
-- flycheck-haskell-ghc-executable: "ghc-with-ghc"
-- End:
