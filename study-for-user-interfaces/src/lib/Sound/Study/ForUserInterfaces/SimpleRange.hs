{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer   : 8c6794b6@gmail.com
Stability    : experimental
Portability  : unknown

Simple GUI using web browser, done with threepenny-gui.

-}
module Sound.Study.ForUserInterfaces.SimpleRange where

import           Control.Concurrent (forkIO, killThread)
import           Data.Unique (hashUnique, newUnique)
import           Text.Printf (printf)

import qualified Graphics.UI.Threepenny as UI hiding (input)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Extra as Extra

import           Sound.OSC.FD
import           Sound.SC3.FD
import           Sound.SC3.UGen.ID

-- | Open connection to scsynth and start the threepenny-gui server.
-- Type \'q\' to shutdown the threepenny-gui server.
main :: IO ()
main = withTransport (openUDP "127.0.0.1" 57110) $ \fd -> do
    send fd $ notify True
    _ <- async fd $ d_recv $ synthdef "simpleSaw" simpleSaw
    static <- Extra.getStaticDir
    tid <- forkIO $ startGUI defaultConfig {tpStatic = Just static} (setup fd)
    let go = getChar >>= \c -> case c of 'q' -> return (); _ -> go
    go >> killThread tid
    send fd $ notify False
    reset fd

-- | UGen used in this interface.
simpleSaw :: UGen
simpleSaw = out 0 (pan2 sig0 pos 1)
  where
    sig0 = resonz sig1 cf rq
    sig1 = saw AR freq * aenv
    aenv = decay2 tr0 0.01 dur * amp
    tr0  = impulse KR hps 0
    cf   = control KR "cf" 2000 `lag` 0.1
    rq   = control KR "rq" 0.3 `lag` 0.1
    freq = tChoose 't' tr0 (mce pchs)
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [0] octs
    degs = [0,2,4,7,5]
    octs = [48,60,72]
    amp  = control KR "amp" (dbAmp (-10)) `lag` 0.1
    hps  = control KR "hps" 1 `lag` 0.1
    dur  = control KR "dur" 1 `lag` 0.1
    pos  = control KR "pos" 0 `lag` 0.1

-- | Setup GUI.
setup ::
    UDP       -- ^ Connection to scsynth.
    -> Window -- ^ Window to show the GUI contents.
    -> UI ()
setup fd window = do
    _ <- return window # set title "simple-sine.hs"
    UI.addStyleSheet window "ui.css"

    nid <- liftIO $ do
        nid <- hashUnique `fmap` newUnique
        let nid' = nid + 30000
        send fd $ s_new "simpleSaw" nid' AddToTail 1 []
        return nid'

    let nset name minv maxv stepv iniv =
            Extra.vrange name minv maxv stepv iniv $ \v -> do
                liftIO (send fd $ n_set nid [(name,v)])
                return (printf "%3.2f" v)

    cf  <- nset "cf"  50   8000 1    2000
    rq  <- nset "rq"  0.1  0.9  0.01 0.3
    hps <- nset "hps" 0.5  32   0.25 1
    dur <- nset "dur" 0.01 3    0.01 1
    pos <- nset "pos" (-1) 1    0.01 0

    button <- UI.button # set text "free"
    on UI.click button $ \_ -> do
        liftIO $ send fd $ n_free [nid]
    _ <- getBody window #+ (map element [cf, rq, hps, dur, pos, button])
    return ()
