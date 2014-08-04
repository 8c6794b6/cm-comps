module Main where

import Language.Haskell.REPL.Server
import System.Environment

main :: IO ()
main =
  do (port':_) <- getArgs
     let port = read port'
     putStrLn ("Starting REPL server at port " ++ show port)
     runServer port
