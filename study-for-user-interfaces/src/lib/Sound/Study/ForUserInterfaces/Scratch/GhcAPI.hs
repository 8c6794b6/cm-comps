{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module to play with ghc package and temporal recursion.
-}
module Sound.Study.ForUserInterfaces.Scratch.GhcAPI where

import           Control.Applicative
import           Control.Monad (ap, liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Random (MonadRandom(..))
import           Data.Dynamic (fromDyn)
import           Data.List (intersperse)
import           Data.IORef
import           Language.Haskell.TH.Syntax (Name)
import           Unsafe.Coerce (unsafeCoerce)

import           Sound.OSC (Time, pauseThreadUntil)

-- From "ghc" package
import           GHC hiding (Name)
import           DynFlags
                  ( HasDynFlags(..), PkgConfRef(..), PackageFlag(..)
                  , defaultFatalMessager, defaultFlushOut)
import           Exception (ExceptionMonad(..))

-- From "ghc-paths" package
import           GHC.Paths (libdir)

data RecEnv t =
  RecEnv {reHValueRef :: {-# UNPACK  #-} !(IORef HValue)
         ,reTransport :: {-# UNPACK  #-} !t}

-- Inspired from GhciMonad.Ghci, see how it's holding Ghc inside.
-- It is reader Monad using IORef.
newtype Rec a = Rec {unRec :: IORef HValue -> Ghc a}

instance MonadRandom Rec where
  getRandom = liftIO getRandom
  getRandoms = liftIO getRandoms
  getRandomR = liftIO . getRandomR
  getRandomRs = liftIO . getRandomRs

-- | Getter and setter fallback value for module reload.
class HasFallback h where
  setFallback :: HValue -> h ()
  getFallback :: h HValue

instance HasFallback Rec where
  setFallback hvalue = Rec (\ref -> liftIO (writeIORef ref hvalue))
  getFallback = Rec (liftIO . readIORef)

-- | Setup GHC session and run 'Rec' with initial argument.
runRecWith ::
  FilePath        -- ^ Path to package db.
  -> String       -- ^ Name of target source.
  -> IO a         -- ^ 'IO' action returning initial argument.
  -> (a -> Rec b) -- ^ Function taking argument and returns 'Rec' to run.
  -> IO b
runRecWith pkgConf targetSource arg frec =
  do ref <- newIORef (error "runRecWith: fallback not initialized.")
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
              unRec (frec arg') ref))

-- | Like 'runRecWith', but without initial argument.
runRec :: FilePath -> String -> Rec a -> IO a
runRec pkgConf targetSource body =
  runRecWith pkgConf targetSource (return ()) (\_ -> body)

liftGhc :: Ghc a -> Rec a
liftGhc m = Rec (\_ -> m)

instance Monad Rec where
  Rec m >>= k = Rec (\r -> m r >>= \a -> unRec (k a) r)
  return = liftGhc . return

instance Functor Rec where
  fmap = liftM

instance Applicative Rec where
  pure = return
  (<*>) = ap

instance MonadIO Rec where
  liftIO = liftGhc . liftIO

instance ExceptionMonad Rec where
  gcatch m h = Rec (\r -> unRec m r `gcatch` (\e -> unRec (h e) r))
  gmask f = Rec (\r -> gmask (\g -> let f' (Rec m) = Rec (\r' -> g (m r'))
                                    in  unRec (f f' ) r))

instance HasDynFlags Rec where
  getDynFlags = liftGhc getDynFlags

instance GhcMonad Rec where
  getSession = liftGhc getSession
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
                      do x' <- liftIO ((unsafeCoerce fallback :: a -> IO a) x)
                         go x' fallback
                    Succeeded ->
                      do setContext [IIDecl (simpleImportDecl myModuleName)]
                         result <- compileExpr (show expr)
                         x' <- liftIO ((unsafeCoerce result :: a -> IO a) x)
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
                      ,extraPkgConfs = (PkgConfFile pkgConf :)
                      ,packageFlags = [ExposePackage "ghc"]
                      ,hscTarget = HscInterpreted
                      -- ,hscTarget = HscLlvm
                      ,ghcLink = LinkInMemory
                      -- ,ghcLink = LinkBinary
                      ,ghcMode = CompManager
                      -- ,ghcMode = OneShot
                      ,importPaths = ["dist/build"
                                     ,"src/lib"]}
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
            unsafeCoerce hvalue arg

apply' :: GhcMonad m => Name -> t -> m ()
apply' n arg = apply n arg (\_ -> liftIO (putStrLn "Compilation failed."))

applyAt :: (HasFallback m, GhcMonad m) => Time -> Name -> t -> m b
applyAt scheduledTime func arg =
  do --
     -- Cannot compile expression, try looking up the value of given Name from
     -- current HscEnv to get last result. Use 'ByteCodeLink.lookupName'? Where
     -- to get ClosureEnv and Name? There is a function named
     -- 'Linker.getHValue', which takes HscEnv and Name.
     --
     -- Use lookupOrigNameCache? Not working, showing error message for /home
     -- module not loaded/ (homeModError). Alternate idea are:
     --
     -- + Pass extra argument, use that function as fallback (easiest).
     --
     -- * Use StateMonad, hold last compiled result. (might work, make newtype
     -- and wrap Ghc in with State Monad, store HValue in the state).
     --
     -- * Don't interpret, load object code only. Use does compilation manually
     -- which leads to reloading by looped process (don't understand ghc this
     -- much yet).
     --
     let thisModuleName = mkModuleName (moduleOfFunctionName func)
     --     ocache = nsNames hsc_nc
     --     mdls = [mdl | ms <- hsc_mod_graph hsc_env
     --                 , let mdl = ms_mod ms
     --                 , moduleName mdl == thisModuleName]
     --     myModule = head mdls
     --     baseName = tail (takeExtension (show func))
     --     mbName = lookupOrigNameCache ocache myModule (mkVarOcc baseName)
     --     name = case mbName of
     --              Just n  -> n
     --              Nothing -> error ("applyAt: Lookup failed.")
     -- fallback <- liftIO (getHValue hsc_env name)
     result <- load (LoadUpTo thisModuleName)
     case result of
       Failed    ->
         do fallback <- getFallback
            pauseThreadUntil scheduledTime
            unsafeCoerce fallback arg
       Succeeded ->
         do setContext [IIModule thisModuleName]
            hvalue <- compileExpr (show func)
            setFallback hvalue
            pauseThreadUntil scheduledTime
            unsafeCoerce hvalue arg

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
