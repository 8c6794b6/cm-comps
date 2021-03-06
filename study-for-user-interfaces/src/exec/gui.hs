module Main where

import           Control.Monad (unless)
import           System.Console.GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)

import qualified Sound.Study.ForUserInterfaces.GUI.GUI01 as GUI01
import qualified Sound.Study.ForUserInterfaces.GUI.GUI02 as GUI02
import qualified Sound.Study.ForUserInterfaces.GUI.SimpleRange as SimpleRange
import qualified Sound.Study.ForUserInterfaces.GUI.SimpleStatus as SimpleStatus
import qualified Sound.Study.ForUserInterfaces.GUI.SimpleXY as SimpleXY

data MainModule
    = GUI01
    | GUI02
    | SimpleRange
    | SimpleStatus
    | SimpleXY
    deriving (Eq, Show)

options :: [OptDescr MainModule]
options =
    [ Option [] ["gui01"] (NoArg GUI01)
      "Run GUI01"
    , Option [] ["gui02"] (NoArg GUI02)
      "Run GUI02"
    , Option [] ["simplerange"] (NoArg SimpleRange)
      "Run SimpleXY"
    , Option [] ["simplestatus"] (NoArg SimpleStatus)
      "Run SimpleStatus"
    , Option [] ["simplexy"] (NoArg SimpleXY)
      "Run SimpleRange"
    ]

main :: IO ()
main = do
    args <- getArgs
    name <- getProgName

    let (opts, _rests, errs) = getOpt Permute options args
    unless (null errs && not (null opts)) $ do
        putStrLn $ usageInfo (unwords ["usage:",name,"[OPTION]"]) options
        exitFailure

    case head opts of
         GUI01        -> GUI01.main
         GUI02        -> GUI02.main
         SimpleRange  -> SimpleRange.main
         SimpleStatus -> SimpleStatus.main
         SimpleXY     -> SimpleXY.main
