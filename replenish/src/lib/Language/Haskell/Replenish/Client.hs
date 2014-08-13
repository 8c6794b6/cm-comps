{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Client side code for function callback in REPL.

-}
module Language.Haskell.Replenish.Client where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (bracket)
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as BS
import           Network.Socket            hiding (recv, send)
import qualified Network.Socket.ByteString as BS
import           Sound.OSC                 (Time, pauseThreadUntil)

__callback :: Show a => Int -> Time -> String -> a -> IO ()
__callback port scheduled name args =
  void
    (forkIO
       (do pauseThreadUntil scheduled
           bracket
             (do addr:_ <- getAddrInfo
                             Nothing (Just "127.0.0.1") (Just (show port))
                 sock <- socket (addrFamily addr) Datagram defaultProtocol
                 connect sock (addrAddress addr)
                 return sock)
             sClose
             (\sock ->
                BS.sendAll
                  sock (BS.unwords (map BS.pack [name, show args])))))

callback_dec :: Int -> String
callback_dec port =
  unlines ["callback :: Show a => Double -> String -> a -> IO ()"
          ,"callback = __callback " ++ show port]
