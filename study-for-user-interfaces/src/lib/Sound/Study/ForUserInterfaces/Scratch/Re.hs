{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Sound.Study.ForUserInterfaces.Scratch.Re where

import           Sound.Study.ForUserInterfaces.Scratch.Reload

import           Control.Monad.Catch                          (bracket)
import           Control.Monad.Reader                         (ReaderT (..))
import           Sound.OSC
import           Sound.OSC.FD                                 (close)


type OSCRld a = ReloadT (ReaderT UDP IO) a

runOSCRld :: ReloadConfig -> IO UDP -> OSCRld a -> IO a
runOSCRld config udpIO m =
  bracket udpIO close (runReaderT (runReloadT config m))


-- Local Variables:
-- flycheck-haskell-ghc-executable: "ghc-with-ghc"
-- End:
