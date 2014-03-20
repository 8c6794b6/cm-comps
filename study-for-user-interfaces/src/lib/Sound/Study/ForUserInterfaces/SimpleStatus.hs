{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer   : 8c6794b6@gmail.com
Stability    : experimental
Portability  : unknown

Simple GUI to show status of scsynth.

-}
module Sound.Study.ForUserInterfaces.SimpleStatus where

import           Control.Concurrent (forkIO, killThread)
import           Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Extra as Extra
import           Graphics.UI.Threepenny.Core

import           Sound.OSC.FD (Transport)
import           Sound.SC3.FD (withSC3)

-- | Main entry point for running threepenny-gui server.
main :: IO ()
main = withSC3 $ \fd -> do
    static <- Extra.getStaticDir
    tid <- forkIO $ startGUI defaultConfig {tpStatic=Just static} (setup fd)
    getChar >> killThread tid

-- | Setup window with given 'Transport'.
setup :: Transport t => t -> Window -> UI ()
setup fd window =  do
    void $ return window # set title "simple status"
    UI.addStyleSheet window "ui.css"
    (timer, stDiv) <- Extra.statusDiv fd
    void $ getBody window #+ [element stDiv]
    UI.start timer
