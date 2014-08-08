module Main where

import Language.Haskell.Replenish.Server

import Control.Exception
import System.Environment

main :: IO ()
main =
  do port <- handle
               (\(SomeException _) -> return 9237)
               (do (port':_) <- getArgs
                   evaluate (read port'))
     putStrLn ("Starting REPL server at port " ++ show port)
     runServer port
