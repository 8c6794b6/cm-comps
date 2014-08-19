{-# LANGUAGE DeriveDataTypeable #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Client side code for function callback in REPL.

-}
module Language.Haskell.Replenish.Client where

import Control.Exception (bracket)
import Data.Data (Data, Typeable)
import Network (HostName, PortID(..), sendTo, connectTo, withSocketsDo)
import qualified Data.ByteString.Char8 as BS

import Network.Socket hiding (send, recv)
import qualified Network.Socket.ByteString as BS
import System.IO (Handle, IOMode(..), openFile, withFile, hFlush, hPutStr)

data Callback = Callback {cbTime :: Double
                         ,cbFunc :: String
                         ,cbArgs :: String}
              | End
              deriving (Eq, Data, Typeable)

callback :: Show a => Double -> String -> a -> IO Callback
callback scheduled f args = return (Callback scheduled f (show args))

-- nullPrinter :: Show a => a -> IO ()
-- nullPrinter _ = putStrLn "nullPrinter"

-- __connectTo :: HostName -> Int -> IO Handle
-- __connectTo host port = connectTo host (PortNumber (fromIntegral port))

__getHdl :: FilePath -> IO Handle
__getHdl path = openFile path WriteMode

__interactivePrint :: Show a => Handle -> a -> IO ()
__interactivePrint hdl str = hPutStr hdl (show str) >> hFlush hdl

-- __interactivePrint :: Show a => HostName -> Int -> a -> IO ()
-- __interactivePrint host port str =
--   -- withFile path WriteMode (\hdl -> hPutStrLn hdl (show str) >> hFlush hdl)
--   withSocketsDo
--     (bracket
--        (do saddr:_ <- getAddrInfo Nothing (Just host) (Just (show port))
--            s <- socket (addrFamily saddr) Stream defaultProtocol
--            connect s (addrAddress saddr)
--            return s)
--        close
--        (\s ->
--          do putStrLn ("interactivePrint: " ++ show str)
--             BS.sendAll s (BS.pack (show str))))
