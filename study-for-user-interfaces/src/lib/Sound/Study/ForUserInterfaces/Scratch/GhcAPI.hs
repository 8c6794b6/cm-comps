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

import           Control.Applicative    (Applicative (..))
import           Control.Concurrent     (forkIO)
import           Control.Monad          (ap, liftM, void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Random   (MonadRandom (..))
import           Data.ByteString.Char8  (unpack)
import           Data.Dynamic           (fromDyn)
import           Data.IORef
import           Data.List              (intersperse)
import           Language.Haskell.TH    (Name)
import           System.Exit            (exitSuccess)
import           System.Process         (rawSystem)

import           DynFlags               (HasDynFlags (..), LogAction,
                                         PackageFlag (..), PkgConfRef (..),
                                         defaultFatalMessager, defaultFlushOut,
                                         defaultLogAction)
import           Exception              (ExceptionMonad (..))
import           GHC                    hiding (Name)
import           GHC.Paths              (ghc, libdir)
import           GHC.Prim               (unsafeCoerce#)

import           Sound.OSC              (Datum (..), DuplexOSC, Message (..),
                                         RecvOSC (..), SendOSC (..), Time,
                                         Transport, message, openUDP,
                                         pauseThreadUntil, sendOSC, string,
                                         udpServer, waitMessage, withTransport)
import qualified Sound.OSC.Transport.FD as FD


-- --------------------------------------------------------------------------
--
-- * Newtype wrapper for reload-aware recursion
--
-- --------------------------------------------------------------------------

-- Inspired from GhciMonad.Ghci, see how it's holding Ghc inside.
-- It is reader Monad using IORef.
newtype Rec t a = Rec {unRec :: RecEnv t -> Ghc a}

data RecEnv t =
  RecEnv {reHValueRef :: !(IORef HValue)
         ,reTransport :: !t}

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

liftGhc :: Ghc a -> Rec t a
liftGhc m = Rec (\_ -> m)
{-# INLINE liftGhc #-}

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


-- --------------------------------------------------------------------------
--
-- * Forking GHC
--
-- --------------------------------------------------------------------------

data RecConfig t =
  RecConfig {recPackageDbs    :: IO [FilePath]
            ,recSourcePaths   :: IO [FilePath]
            ,recTarget        :: String
            ,recTransportFunc :: String -> Int -> IO t
            ,recTransportHost :: String
            ,recTransportPort :: Int}

defaultRecConfig :: RecConfig t
defaultRecConfig =
  RecConfig {recPackageDbs = return []
            ,recSourcePaths = return []
            ,recTarget = "Main"
            ,recTransportFunc = errTransportFunc
            ,recTransportHost = "127.0.0.1"
            ,recTransportPort = 0}
  where
    errTransportFunc =
      error "defaultRecConfig: recTransportFunc not specified."

-- | Fork expression with given configuration.
forkExprWith ::
  Show a
   => RecConfig t -- ^ Configuration passed to @ghc@ command.
   -> a           -- ^ Expression ran by @ghc -e@.
   -> IO ()
forkExprWith config expr = forkGhcProcess config (show expr)

forkGhcProcess :: RecConfig t -> String -> IO ()
--
-- Forking "ghc" command because as of ghc-7.8.2, forkIO and runGhc is not
-- working nicely. See:
--   <http://www.haskell.org/pipermail/ghc-devs/2014-January/003774.html>
--
forkGhcProcess config str =
  do packageDbs <- recPackageDbs config
     sourcePaths <- recSourcePaths config
     let args = [unwords (map ("-package-db=" ++) packageDbs)
                ,unwords (map ("-i" ++) sourcePaths)
                ,"-package", "ghc"
                ,"-e", str
                ,recTarget config]
     void (forkIO (void (rawSystem ghc args)))

-- | Kill forked process with @pkill@ system command.
pkill :: Show a => a -> IO ()
pkill str =
  let args = ["-f", show str]
  in  void (rawSystem "pkill" args)

--
-- Might not necessary to start Server, why not fork GHC with rawSystem with
-- arguments used in current ghci? ... how to get current arguments in ghci?
--
-- Use per-directory .ghci file? Is package-db path available in .ghci?
--
--   <https://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-dot-files.html>
--

-- | Configuration for recursion server.
data RecServerConfig = RecServerConfig {rscPort :: Int}

defaultRecServerConfig :: RecServerConfig
defaultRecServerConfig = RecServerConfig 40703

startServer :: RecConfig t -> RecServerConfig -> IO a
startServer config serverConfig =
  withTransport
    (udpServer "127.0.0.1" (rscPort serverConfig))
    (serverLoop config)

serverLoop :: (RecvOSC m, MonadIO m) => RecConfig t -> m b
serverLoop config =
  do let loop = serverLoop config
     msg@(Message addr dtm) <- waitMessage
     case addr of
       "/h_quit"    -> liftIO (putStrLn "quit.") >> liftIO exitSuccess
       -- XXX: Append thread id to state and kill the forked thread when receiving
       -- quit message?
       "/h_forkGhc" -> case dtm of
                         [ASCII_String str] ->
                           do liftIO (forkGhcProcess config (unpack str))
                              loop
                         _                  -> loop
       _            -> liftIO (print msg) >> loop

sendRec :: Message -> IO ()
sendRec = sendRecWith defaultRecServerConfig

sendRecWith ::  RecServerConfig -> Message -> IO ()
sendRecWith config msg =
  withTransport (openUDP "127.0.0.1" (rscPort config))
                (sendOSC msg)

h_forkGhc :: Name -> Message
h_forkGhc func = message "/h_forkGhc" [string (show func)]

h_quit :: Message
h_quit = message "/h_quit" []


-- --------------------------------------------------------------------------
--
-- * Running recursion
--
-- --------------------------------------------------------------------------

-- | Setup GHC session and run 'Rec' with given arguments.
runRecWith ::
  RecConfig t       -- ^ Configuration for GHC to run recursion.
  -> IO a           -- ^ 'IO' action returning initial argument.
  -> (a -> Rec t b) -- ^ Function taking argument and returns 'Rec' to run.
  -> IO b
runRecWith config arg frec =
  do pkgDbs <- recPackageDbs config
     sourcePaths <- recSourcePaths config
     ref <- newIORef (error "runRecWith: fallback not initialized.")
     transport <- recTransportFunc config
                                   (recTransportHost config)
                                   (recTransportPort config)
     let env = RecEnv {reHValueRef = ref
                      ,reTransport = transport}
     defaultErrorHandler
       defaultFatalMessager
       defaultFlushOut
       (runGhc
        (Just libdir)
        (do _target <- setupSession pkgDbs sourcePaths (recTarget config)
            rflag <- load LoadAllTargets
            case rflag of
              Failed    -> error "runRecWith: failed loading module."
              Succeeded -> return ()
            arg' <- liftIO arg
            unRec (frec arg') env))

-- | Setup GHC session with given package conf file and target.
setupSession ::
  GhcMonad m
  => [FilePath] -- ^ Package db file paths (i.e; arguments passed to ghc
                -- @-package-db@ option).
  -> [FilePath] -- ^ Source paths (i.e; arguments passed to ghc @-i@ option).
  -> String     -- ^ Target, passed to 'guessTarget'.
  -> m Target
setupSession pkgDbs sourcePaths targetSource =
  do dflags <- getSessionDynFlags
     _pkgs <- setSessionDynFlags
               dflags {verbosity = 0
                      ,log_action = naughtyLogAction
                      ,extraPkgConfs =
                         const (GlobalPkgConf : map PkgConfFile pkgDbs)
                      -- ,extraPkgConfs = (PkgConfFile pkgConf :)
                      -- ,extraPkgConfs = const [PkgConfFile pkgConf]
                      ,packageFlags = [ExposePackage "ghc"]
                      ,hscTarget = HscInterpreted
                      ,ghcLink = LinkInMemory
                      ,ghcMode = CompManager
                      ,importPaths = sourcePaths}
     target <- guessTarget targetSource Nothing
     setTargets [target]
     return target

-- | Log action hiding errors and warnings.
naughtyLogAction :: LogAction
naughtyLogAction dflag severity srcSpan style msg =
  case severity of
     SevError   -> return ()
     SevWarning -> return ()
     _          -> defaultLogAction dflag severity srcSpan style msg

-- | This works with GHC running as separate OS process.
--
-- Takes TemplateHaskell name of 'Rec' function and argument passed to the
-- 'Rec'. Only single thread could be forked at the same time in each GHC
-- instance.
--
applyAt ::
  (HasFallback m, GhcMonad m)
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
-- * Client side synchronization
--
-- --------------------------------------------------------------------------

-- | Data type to manage of synchronization in client side.
data Metro =
  Metro {beatsPerMinute :: {-# UNPACK #-} !Double
        ,beatDuration   :: {-# UNPACK #-} !Double
        ,currentBeat    :: !(Time -> Double)
        ,nextOffset     :: !(Int -> Double -> Double)}

instance Show Metro where
  show m = "Metro {beatsPerMinute = " ++ show (beatsPerMinute m) ++ "}"

-- | Returns a 'Metro' with given beats per minute.
mkMetro :: Double -> Metro
mkMetro bpm =
  let m = Metro {beatsPerMinute = bpm
                ,beatDuration = 60 / bpm
                ,currentBeat = \t -> t / beatDuration m
                ,nextOffset = \n t ->
                  let gridDuration = beatDuration m * fromIntegral n
                      numGrids :: Int
                      (numGrids, _) =
                        properFraction (t / (fromIntegral n * beatDuration m))
                  in  gridDuration * fromIntegral (numGrids + 1) }
  in  m `seq` m

{-
--
-- Seems like, when running as bytecode, data type with function records
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
