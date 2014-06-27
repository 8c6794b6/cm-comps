{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module to play with ghc package and temporal recursion.
-}
module Sound.Study.ForUserInterfaces.Scratch.GhcAPI where

import Control.Monad.IO.Class (liftIO)
import Data.Dynamic (fromDyn)
import Data.List (intersperse)
import Language.Haskell.TH.Syntax (Name)
import Unsafe.Coerce (unsafeCoerce)

import Sound.OSC (Time, pauseThreadUntil)

-- From "ghc" package
import GHC hiding (Name)

-- import DriverPipeline (link, linkBinary)


import DynFlags ( PkgConfRef(..), PackageFlag(..), defaultFatalMessager
                , defaultFlushOut)

-- import Linker (getHValue, showLinkerState)
-- import Module (packageIdString, stringToPackageId)
-- import OccName (mkVarOcc)
-- import HscMain (hscStmt)
-- import HscTypes (HscEnv(..), NameCache(..), emptyHomePackageTable)
-- import IfaceEnv (lookupOrigNameCache)

-- From "ghc-paths" package
import GHC.Paths (libdir)


-- --------------------------------------------------------------------------
--
-- From "GHC/As a library - Haskell Wiki"
--
-- --------------------------------------------------------------------------

thisModule :: String
thisModule = moduleOfFunctionName 'thisModule

-- | Evaluate given expression having type @a :: IO ()@.
eval :: Show a => FilePath -> a -> IO ()
eval pkgConf expr =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do dflags <- getSessionDynFlags
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
        setContext [IIModule (mkModuleName thisModule)]
        result <- dynCompileExpr (show expr)
        liftIO (fromDyn result (putStrLn "Failed"))))

-- This works quicker than 'eval', but recursion is hardcoded inside this
-- action, which makes difficult to terminate the forked thread and managing
-- recursive state update.
callback :: Show a => FilePath -> String -> a -> IO ()
callback pkgConf sourceModule expr =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do setupSession pkgConf sourceModule
        let err = putStrLn ("callback: Error compiling `" ++ show expr ++ "'")
            myModuleName = mkModuleName sourceModule
            go = do _ <- load LoadAllTargets
                    setContext [IIDecl (simpleImportDecl myModuleName)]
                    result <- dynCompileExpr (show expr)
                    liftIO (fromDyn result err)
                    go
        go))

-- | Recursively load and evaluate given function name.
callback' ::
  FilePath  -- ^ Path to package conf file.
  -> String -- ^ Module name.
  -> Name   -- ^ Name of function to recurse.
            --
            -- Expecting an action which returns next state for itself
            -- to update the state recursively.
            --
  -> a      -- ^ Initial argument.
  -> IO ()
callback' pkgConf sourceModule expr arg =
  defaultErrorHandler
   defaultFatalMessager
   defaultFlushOut
   (runGhc
    (Just libdir)
    (do setupSession pkgConf sourceModule
        let myModuleName = mkModuleName sourceModule
            go x = do _ <- load LoadAllTargets
                      setContext [IIDecl (simpleImportDecl myModuleName)]
                      result <- compileExpr (show expr)
                      x' <- liftIO ((unsafeCoerce result :: a -> IO a) x)
                      go x'
        go arg))

withGhc :: FilePath -> String -> Ghc () -> IO ()
withGhc pkgConf targetSource body =
  defaultErrorHandler
    defaultFatalMessager
    defaultFlushOut
    (runGhc (Just libdir)
            (do setupSession pkgConf targetSource
                body))

setupSession :: FilePath -> String -> Ghc ()
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
     -- rflag <- load LoadAllTargets
     -- case rflag of
     --   Succeeded -> return ()
     --   _         -> error ("Loading " ++ targetSource ++ " failed.")

-- | This works with GHCs running with separate OS process.
--
-- Takes TemplateHaskell name of function, possibly updated argument, and an
-- action to take when failed to reload the module. Only single thread could be
-- forked at the same time.
--
apply :: Name -> a -> (a -> Ghc b) -> Ghc b
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

apply' :: Name -> a -> Ghc ()
apply' n arg = apply n arg (\_ -> liftIO (putStrLn "Compilation failed."))

applyAt' :: Time -> Name -> a -> Ghc b
applyAt' theTime func arg =
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
     -- * Save the source file which compiled successfully, load this if current
     -- reload iteration has failed. Need to copy the file in every loop
     -- (workaround).
     --
     -- * Don't interpret, load object code only. Use does compilation manually
     -- which leads to reloading by looped process (don't understand ghc this
     -- much yet).
     --

     -- hsc_env <- getSession
     -- hsc_nc <- liftIO (readIORef (hsc_NC hsc_env))
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
       Failed    -> error "applyAt': Failed loading module"
         -- do liftIO (pauseThreadUntil theTime)
         --    unsafeCoerce fallback arg
       Succeeded ->
         do setContext [IIModule thisModuleName]
            hvalue <- compileExpr (show func)
            liftIO (pauseThreadUntil theTime)
            unsafeCoerce hvalue arg

-- applyAt' :: Double -> Name -> a -> Ghc ()
applyAt :: GhcMonad m => Time -> Name -> t -> m b
applyAt theTime func arg =
  do let thisModuleName = mkModuleName (moduleOfFunctionName func)
     result <- load (LoadUpTo thisModuleName)
     case result of
       Failed    -> -- error "applyAt: Compilation failed."
         do setContext [IIDecl (simpleImportDecl thisModuleName)]
            hvalue <- compileExpr (show func)
            hvalue `seq` liftIO (pauseThreadUntil theTime)
            unsafeCoerce hvalue arg
       Succeeded ->
         do setContext [IIModule thisModuleName]
            hvalue <- compileExpr (show func)
            hvalue `seq` liftIO (pauseThreadUntil theTime)
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
