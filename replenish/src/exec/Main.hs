module Main where

import Language.Haskell.Replenish.Server

import Control.Exception
import System.Environment

main :: IO ()
main =
  do port <- do (port':_) <- getArgs
                evaluate (read port')
             `catch`
             \(SomeException _) ->
               do putStrLn "Using default port"
                  return 9237
     putStrLn ("Starting REPL server at port " ++ show port)
     runServer port
