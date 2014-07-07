{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : GHC-only

Pperiodically reload haskell Module with GHC APIs.
-}
module Sound.Study.ForUserInterfaces.Scratch.Reload where

import           Control.Applicative          (Applicative (..))
import           Control.Concurrent           (forkIO)
import           Control.Monad                (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Random         (MonadRandom (..))
import           Control.Monad.Reader         (ReaderT (..))
import qualified Control.Monad.RWS.Lazy       as Lazy
import qualified Control.Monad.RWS.Strict     as Strict
import qualified Control.Monad.State.Lazy     as Lazy
import qualified Control.Monad.State.Strict   as Strict
import           Control.Monad.Trans          (MonadTrans (..))
import           Control.Monad.Trans.Identity (IdentityT (..))
import qualified Control.Monad.Writer.Lazy    as Lazy
import qualified Control.Monad.Writer.Strict  as Strict
import           Data.ByteString.Char8        (unpack)
import           Data.Dynamic                 (fromDyn)
import           Data.IORef                   (IORef, newIORef, readIORef,
                                               writeIORef)
import           Data.List                    (intersperse)
import           Data.Monoid                  (Monoid)
import           Data.Ratio                   (approxRational)
import           Language.Haskell.TH          (Name)
import           System.Exit                  (exitSuccess)
import           System.Process               (rawSystem)

import           Control.Monad.Ghc            (GhcT, runGhc, runGhcT)
import           DynFlags                     (HasDynFlags (..),
                                               PackageFlag (..),
                                               PkgConfRef (..),
                                               defaultFatalMessager,
                                               defaultFlushOut,
                                               defaultLogAction)
import           Exception                    (ExceptionMonad (..))
import           GHC                          hiding (Ghc, GhcT, Name, runGhc,
                                               runGhcT)
import           GHC.Paths                    (ghc, libdir)
import           GHC.Prim                     (unsafeCoerce#)
import           HscMain                      (hscParseIdentifier,
                                               hscTcRnLookupRdrName)
import           Linker                       (getHValue)

import           Sound.OSC                    (Datum (..), DuplexOSC,
                                               Message (..), RecvOSC (..),
                                               SendOSC (..), Time, Transport,
                                               message, openUDP,
                                               pauseThreadUntil, sendOSC,
                                               string, time, udpServer,
                                               waitMessage, withTransport)
import qualified Sound.OSC.Transport.FD.UDP   as UDP


-- --------------------------------------------------------------------------
--
-- * Type class
--
-- --------------------------------------------------------------------------

-- | Type class with getter and setter for fallback value used in case of
-- failure in module reload.
class (Monad m, MonadIO m, MonadMask m) => MonadReload m where
  -- | Set fallback value.
  setFallback :: HValue -> m ()
  -- | Get fallback value.
  getFallback :: m HValue

instance MonadReload m => MonadReload (IdentityT m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance MonadReload m => MonadReload (ReaderT r m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance MonadReload m => MonadReload (Lazy.StateT s m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance MonadReload m => MonadReload (Strict.StateT s m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance (Monoid w, MonadReload m) => MonadReload (Lazy.WriterT w m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance (Monoid w, MonadReload m) => MonadReload (Strict.WriterT w m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance (Monoid w, MonadReload m) => MonadReload (Lazy.RWST r w s m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)

instance (Monoid w, MonadReload m) => MonadReload (Strict.RWST r w s m) where
  getFallback = lift getFallback
  setFallback hv = lift (setFallback hv)


-- --------------------------------------------------------------------------
--
-- * Reload monad transformer
--
-- --------------------------------------------------------------------------

-- | MonadReload transformer, which wraps 'GhcT' to make couple instances for
-- convenience.
--
-- This newtype same as 'ReaderT' with 'IORef' 'HValue'. Defining another
-- newtype wrapper for 'GhcMonad' and 'MonadReload' should be trivial.
newtype ReloadT m a = ReloadT {unReloadT :: IORef HValue -> GhcT m a}

-- ReloadT is wrapping GhcT to avoid some orphan instances. If ReloadT did not
-- wrap GhcT, requires below instances for GhcT to work with RecvOSC, SendOSC,
-- ... etc, e.g.:
--
--   > instance RecvOSC m => RecvOSC (GhcT m) where
--   >   recvPacket = lift recvPacket
--   > instance SendOSC m => SendOSC (GhcT m) where
--   >   sendOSC = lift . sendOSC
--   > instance DuplexOSC m => DuplexOSC (GhcT m)
--   > instance Transport m => Transport (GhcT m)
--

liftReloadT :: Monad m => m a -> ReloadT m a
liftReloadT m = ReloadT (\_ -> lift m)
{-# INLINE liftReloadT #-}

type Reload a = ReloadT IO a

instance Functor m => Functor (ReloadT m) where
  fmap f (ReloadT m) = ReloadT (fmap f . m)

instance (Functor m, Monad m, Applicative m) => Applicative (ReloadT m) where
  pure a = ReloadT (\_ -> pure a)
  f <*> v = ReloadT (\r -> unReloadT f r <*> unReloadT v r)

instance Monad m => Monad (ReloadT m) where
  return = liftReloadT . return
  m >>= k = ReloadT (\r -> do a <- unReloadT m r
                              unReloadT (k a) r)

instance MonadIO m => MonadIO (ReloadT m) where
  liftIO = liftReloadT . liftIO

instance MonadTrans ReloadT where
  lift = liftReloadT

instance Lazy.MonadState s m => Lazy.MonadState s (ReloadT m) where
  put = liftReloadT . Lazy.put
  get = liftReloadT Lazy.get

instance MonadThrow m => MonadThrow (ReloadT m) where
  throwM = liftReloadT . throwM

instance (MonadIO m, MonadMask m) => MonadCatch (ReloadT m) where
  m `catch` f =
    ReloadT (\r -> unReloadT m r `catch` (\e -> unReloadT (f e) r))

instance (MonadIO m, MonadMask m) => MonadMask (ReloadT m) where
  mask a = ReloadT (\r -> mask (\r' -> unReloadT (a (q r')) r))
    where
      q u (ReloadT r) = ReloadT (u . r)
  uninterruptibleMask a =
    ReloadT (\r -> uninterruptibleMask (\r' -> unReloadT (a (q r')) r))
    where
      q u (ReloadT r) = ReloadT (u . r)

instance (Functor m, MonadIO m, MonadMask m) => HasDynFlags (ReloadT m) where
  getDynFlags = ReloadT (\_ -> getDynFlags)

instance (MonadIO m, MonadCatch m, MonadMask m)
          => ExceptionMonad (ReloadT m) where
  gcatch m h = ReloadT (\r -> unReloadT m r `catch` (\e -> unReloadT (h e) r))
  gmask f = mask (\x -> f x)

instance (Functor m, MonadIO m, MonadMask m) => GhcMonad (ReloadT m) where
  getSession = ReloadT (\_ -> getSession)
  setSession s = ReloadT (\_ -> setSession s)

instance RecvOSC m => RecvOSC (ReloadT m) where
  recvPacket = liftReloadT recvPacket

instance SendOSC m => SendOSC (ReloadT m) where
  sendOSC = liftReloadT . sendOSC

instance DuplexOSC m => DuplexOSC (ReloadT m)

instance Transport m => Transport (ReloadT m)

instance MonadRandom m => MonadRandom (ReloadT m) where
  getRandom = liftReloadT getRandom
  getRandoms = liftReloadT getRandoms
  getRandomR = liftReloadT . getRandomR
  getRandomRs = liftReloadT . getRandomRs

instance (MonadIO m, MonadMask m) => MonadReload (ReloadT m) where
  getFallback = ReloadT (\r -> liftIO (readIORef r))
  setFallback hv = ReloadT (\r -> liftIO (writeIORef r hv))

-- | Configuraton for running 'ReloadT'.
data ReloadConfig =
  ReloadConfig
    { -- | Package dbs. Passed to 'extraPkgConfs' field in
      -- 'DynFlags'.
      rcPackageDbs  :: IO [FilePath]
      -- | Source paths. Passed to 'importPaths' field in 'DynFlags'.
     ,rcSourcePaths :: IO [FilePath]
      -- | Bulild target. Passed to 'guessTarget' function.
     ,rcTarget      :: String}

-- | Default configuration to run reloads.
defaultReloadConfig :: ReloadConfig
defaultReloadConfig = ReloadConfig {rcPackageDbs = return []
                                   ,rcSourcePaths = return ["."]
                                   ,rcTarget = "Main"}

-- | Type fixed variant of 'runReloadT'.
runReload :: ReloadConfig -> Reload a -> IO a
runReload = runReloadT

-- | Run 'ReloadT' with given settings.
runReloadT ::
  (MonadIO m, MonadMask m, Functor m)
  => ReloadConfig -- ^ Configuration passed to inner 'runGhcT'.
  -> ReloadT m b  -- ^ Reload action to run.
  -> m b
runReloadT config m =
  do ref <- liftIO (newIORef (error "runReloadT: fallback not initialized."))
     pkgDbs <- liftIO (rcPackageDbs config)
     sourcePaths <- liftIO (rcSourcePaths config)
     -- XXX: Running without error handler, no FatalMessager, no FlushOut.
     runGhcT
       (Just libdir)
       (do target <- setupSession pkgDbs sourcePaths (rcTarget config)
           rflag <- load LoadAllTargets
           case rflag of
             Failed -> error "runReloadT: failed loading module."
             Succeeded ->
               case targetId target of
                 TargetModule mdlName -> setContext [IIModule mdlName]
                 _                    -> return ()
           unReloadT m ref)

-- | To work in /temporal-recursion/ with OSC actions.
type OSCRec a = ReloadT (ReaderT UDP.UDP IO) a

runOSCRec :: ReloadConfig -> IO UDP.UDP -> OSCRec a -> IO a
runOSCRec config transportIO m =
  withTransport transportIO (runReloadT config m)

-- | Setup GHC session with given package conf file and target.
setupSession ::
  GhcMonad m
  => [FilePath] -- ^ Package db file paths (i.e.; arguments passed to ghc
                -- @-package-db@ option).
  -> [FilePath] -- ^ Source paths (i.e.; arguments passed to ghc @-i@ option).
  -> String     -- ^ Target, passed to 'guessTarget'.
  -> m Target
setupSession pkgDbs sourcePaths targetSource =
  do dflags <- getSessionDynFlags
     _pkgs <- setSessionDynFlags
               dflags {verbosity = 0
                      ,log_action = silentLogAction
                      ,extraPkgConfs =
                         const (GlobalPkgConf : map PkgConfFile pkgDbs)
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

-- | Apply function with given argument after reloading the module.
--
-- This works with GHC running as separate OS process.  Takes TemplateHaskell
-- name of 'Rec' function and argument passed to the 'Rec'. Only single thread
-- could be forked at the same time in each GHC instance.
--
applyAt ::
  (MonadReload m, GhcMonad m)
  => Time -- ^ Scheduled time.
  -> Name -- ^ Name of function to evaluate.
  -> t    -- ^ Argument passed to the function.
  -> m b
applyAt scheduledTime func arg =
  do let thisModuleName = mkModuleName (extractModuleName func)
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

-- | Extract module name from template-haskell name.
extractModuleName :: Name -> String
extractModuleName name = concat (intersperse "." (init (ns (show name))))
  where
    ns [] = [""]
    ns xs = let (pre, post) = break (== '.') xs
            in  pre : if null post
                         then []
                         else ns (tail post)


-- --------------------------------------------------------------------------
--
-- * Forking and killing GHC process
--
-- --------------------------------------------------------------------------

-- | Fork expression with given configuration.
forkExprWith ::
  Show a
   => ReloadConfig -- ^ Configuration passed to @ghc@ command.
   -> a            -- ^ Expression ran by @ghc -e@.
   -> IO ()
forkExprWith config expr = forkGhcProcess config (show expr)

-- | Fork GHC process by running @ghc@ command.
forkGhcProcess :: ReloadConfig -> String -> IO ()
--
-- Forking "ghc" command.  As of ghc-7.8.2, runGhc with forkIOs are not working
-- nicely. See:
-- <http://www.haskell.org/pipermail/ghc-devs/2014-January/003774.html>
--
forkGhcProcess config str =
  do packageDbs <- rcPackageDbs config
     sourcePaths <- rcSourcePaths config
     let args = [unwords (map ("-package-db=" ++) packageDbs)
                ,unwords (map ("-i" ++) sourcePaths)
                ,"-package", "ghc"
                ,"-e", str
                ,rcTarget config]
     void (forkIO (void (rawSystem ghc args)))

-- | Kill forked process with @pkill@ system command.
pkill :: Show a => a -> IO ()
pkill str =
  let args = ["-f", show str]
  in  void (rawSystem "pkill" args)


-- --------------------------------------------------------------------------
--
-- * Client side synchronization
--
-- --------------------------------------------------------------------------

-- | Data type to manage of synchronization in client side.
data Metro =
  Metro {beatsPerMinute :: {-# UNPACK #-} !Rational
        ,beatDuration   :: {-# UNPACK #-} !Double
        ,currentBeat    :: !(Time -> Rational)
        ,nextOffset     :: !(Int -> Time -> Time)}

instance Show Metro where
  show m = "Metro {beatsPerMinute = " ++ show (beatsPerMinute m) ++ "}"

-- | Returns a 'Metro' with given beats per minute.
mkMetro :: Rational -> Metro
mkMetro bpm =
  let m = Metro {beatsPerMinute = bpm
                ,beatDuration = 60 / realToFrac bpm
                ,currentBeat = \t -> approxRational (t / beatDuration m) 1e-9
                ,nextOffset = \n t ->
                  let gridDuration = beatDuration m * fromIntegral n
                      numGrids :: Int
                      (numGrids, _) =
                        properFraction (t / (fromIntegral n * beatDuration m))
                  in  gridDuration * fromIntegral (numGrids + 1) }
  in  m `seq` m

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

-- --------------------------------------------------------------------------
--
-- * Running as server
--
-- --------------------------------------------------------------------------

-- Might not necessary to start Server, why not fork GHC with rawSystem with
-- arguments used in current ghci? ... how to get current arguments in ghci?
--
-- Use per-directory .ghci file? Is package-db path available in .ghci?
--
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-dot-files.html>
--
-- How to apply `liftIO . forkIO' inside evaluated expression in 'GhcMonad'?

-- | Configuration for recursion server.
data RecServerConfig = RecServerConfig {rscPort :: Int}

defaultRecServerConfig :: RecServerConfig
defaultRecServerConfig = RecServerConfig 40703

startServer :: ReloadConfig -> RecServerConfig -> IO a
startServer config serverConfig =
  withTransport
    (udpServer "127.0.0.1" (rscPort serverConfig))
    (serverLoop config)

serverLoop :: (RecvOSC m, MonadIO m) => ReloadConfig -> m b
serverLoop config =
  do let loop = serverLoop config
     msg@(Message addr dtm) <- waitMessage
     case addr of
       "/h_quit"    -> liftIO (putStrLn "quit.") >> liftIO exitSuccess
       "/h_forkGhc" -> case dtm of
                         [ASCII_String str] ->
                           do liftIO (forkGhcProcess config (unpack str))
                              loop
                         _                  -> loop
       _            -> liftIO (print msg) >> loop

h_stmt :: String -> Message
h_stmt stmt = message "/h_stmt" [string stmt]

h_eval :: String -> Message
h_eval expr = message "/h_eval" [string expr]

callAt :: Time -> Name -> HscEnv -> t -> IO b
callAt scheduledTime func hsc_env arg  =
  do L _ rdr_name <- hscParseIdentifier hsc_env (show func)
     names <- hscTcRnLookupRdrName hsc_env rdr_name
     hvalues <- mapM (getHValue hsc_env) names
     pauseThreadUntil scheduledTime
     case hvalues of
       [hvalue] -> unsafeCoerce# hvalue arg
       _        -> error ("callAt: error for " ++ show func)

sendRec :: Message -> IO ()
sendRec = sendRecWith defaultRecServerConfig

sendRecWith ::  RecServerConfig -> Message -> IO ()
sendRecWith config msg =
  withTransport (openUDP "127.0.0.1" (rscPort config))
                (sendOSC msg)

h_fork :: Show a => a -> Message
h_fork func = message "/h_forkGhc" [string (show func)]

h_quit :: Message
h_quit = message "/h_quit" []

newtype RawString = RawString String

instance Show RawString where
  show (RawString str) = str


-- --------------------------------------------------------------------------
--
-- Attempt to evaluate received message in server
--
-- --------------------------------------------------------------------------

{-
-- Work stopped in the middle. It was difficult to fork compiled result with
-- keeping HscEnv.

startGhcServer :: ReloadConfig -> RecServerConfig -> IO a
startGhcServer config serverConfig =
   do putStrLn "Starting server ...."
      runReloadT config (udpServer "127.0.0.1" (rscPort serverConfig)) ghcLoop

ghcLoop :: GhcMonad m => UDP.UDP -> m a
ghcLoop udp =
  do let loop = ghcLoop udp
     liftIO (putStrLn "Ready to receive message.")
     (pkt, _sockAddrm) <- liftIO (UDP.recvFrom udp)
     let msg = packet_to_message pkt
     liftIO (print msg)
     case msg of
      Just (Message addr dtm) ->
        case addr of
          "/h_stmt" ->
            do case dtm of
                 [ASCII_String expr] ->
                   void (runStmt (unpack expr) RunToCompletion)
                 _                   -> return ()
               loop
          "/h_eval" ->
            do case dtm of
                 [ASCII_String expr] ->
                   do hsc_env <- getSession
                      now <- time
                      _tid <-
                         liftIO
                          (forkIO
                           (do L _ rdr_name <- hscParseIdentifier hsc_env (unpack expr)
                               names <- hscTcRnLookupRdrName hsc_env rdr_name
                               hvalues <- mapM (getHValue hsc_env) names
                               case hvalues of
                                 [hvalue] -> unsafeCoerce# hvalue hsc_env now
                                 _        ->
                                   liftIO (putStrLn ("Error on eval:" ++ (unpack expr)))))
                      return ()
                 _ -> return ()
               loop
          _ -> loop
      Nothing -> loop
-}

-- --------------------------------------------------------------------------
--
-- * Legacy experiments
--
-- --------------------------------------------------------------------------

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
    (do let sourceModule = extractModuleName expr
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
        _ <- setupSession [pkgConf] ["."] sourceModule
        go arg (error "recurse: Failed the initial load.")))

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
     let thisModuleName = mkModuleName (extractModuleName func)
     result <- load (LoadUpTo thisModuleName)
     case result of
       Failed    -> fallback arg
       Succeeded ->
         do setContext [IIModule thisModuleName]
            hvalue <- compileExpr (show func)
            unsafeCoerce# hvalue arg

-- | Pause thread until given time, then return given value.
returnAt :: MonadIO m => Time -> a -> m a
returnAt t a = pauseThreadUntil t >> return a


-- Local Variables:
-- flycheck-haskell-ghc-executable: "ghc-with-ghc"
-- End:
