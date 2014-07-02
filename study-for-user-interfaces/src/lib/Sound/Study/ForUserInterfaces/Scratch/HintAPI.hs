{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module to periodically reload haskell Module with Hint APIs.

-}
module Sound.Study.ForUserInterfaces.Scratch.HintAPI where

import Data.List (intersperse)
import Data.Typeable
import Language.Haskell.Interpreter
import Language.Haskell.TH.Syntax (Name)
import Sound.OSC

-- Orphan....
deriving instance Typeable InterpreterT

runCall :: String -> Interpreter a -> IO (Either InterpreterError a)
runCall targetModule body =
  -- InterpreterT does not allow forking multiple instance of GHC from ghci
  -- session. See MultipleInstancesNotAllowed in "InterpreterT.hs".
  runInterpreter
    (do set [searchPath := ["src/lib"]]
        setImports ["Prelude"
                   ,"System.IO"
                   ,"Language.Haskell.Interpreter"]
        loadModules [targetModule]
        setTopLevelModules [targetModule]
        body)

callAt :: Double -> Name -> Double -> Interpreter Double
callAt scheduled func arg =
  do let expr = show func
         targetModule = moduleOfFunctionName func
     setImports ["Prelude"
                ,"System.IO"
                ,"Language.Haskell.Interpreter"]
     loadModules [targetModule]
     setTopLevelModules [targetModule]
     pauseThreadUntil scheduled
     m <- interpret expr (as :: Double -> Interpreter Double)
     m arg

moduleOfFunctionName :: Name -> String
moduleOfFunctionName sym = concat (intersperse "." (init (ns (show sym))))
  where
    ns [] = [""]
    ns xs = let (pre, post) = break (== '.') xs
            in  pre : if null post
                         then []
                         else ns (tail post)
