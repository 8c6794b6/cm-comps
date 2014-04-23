{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Graphical user interface, take 2.

Draws simple GUI with fetching current node from scsynth server.

-}
module Sound.Study.ForUserInterfaces.GUI02 where

import           Control.Concurrent (forkIO, killThread)
import           Control.Monad (void)
import           Control.Monad.Reader (runReaderT)
import           Sound.OSC.FD
import           Sound.SC3.FD
import           Sound.SC3.Tree
import           Sound.SC3.UGen.ID

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import qualified Graphics.UI.Threepenny.Extra as Extra
import Paths_study_for_user_interfaces (getDataDir)

main :: IO ()
main = withSC3 $ \fd -> do
    dataDir <- getDataDir
    tid <- forkIO $ startGUI
           defaultConfig { tpStatic     = Just (dataDir ++ "/static")
                         , tpCustomHTML = Just (dataDir ++ "/gui02.html")
                         } (setup fd)
    getChar >> killThread tid

setup :: Transport fd => fd -> Window -> UI ()
setup fd window = do
    void $ return window # set title "gui 02"
    (tmr,stDiv) <- Extra.statusDiv fd
    currentNodes <- liftIO $ runReaderT getRootNode fd
    void $ getBody window #+
        [ element stDiv
        , nodeToHTML fd currentNodes
        ]
    UI.start tmr

nodeToHTML :: Transport fd => fd -> SCNode -> UI Element
nodeToHTML fd node = do
    case node of
         Group nid nodes     ->
             UI.new
             # set text ("group " ++ show nid)
             #. "group_node"
             #+ map (nodeToHTML fd) nodes
         Synth nid name prms ->
             UI.new
             # set text (show nid ++ " " ++ name)
             #. "synth_node"
             #+ (divClear :
                 foldr (\p acc ->
                         paramToHTML fd name nid p : acc) [divClear] prms)

divClear :: UI Element
divClear = UI.new # set style [("clear","both")]

paramToHTML :: Transport fd => fd -> String -> Int -> SynthParam -> UI Element
paramToHTML fd sname nid param = case param of
    pname :=  val -> Extra.knobControl fd pname nid val (crange sname pname)
    pname :<- bus -> showParam pname ": c" bus
    pname :<= bus -> showParam pname ": b" bus
    where
      showParam n lbl v =
          UI.new
          # set text (n ++ lbl ++ show v)
          #. "param"
      crange = lookupControl defaultControls

type ControlList = [(String, [(String, Extra.ControlRange)])]

defaultControls :: ControlList
defaultControls =
    [("Anonymous",
      [("cf",e 20 20000)
      ,("dbAmp",l (-60) 15)
      ,("freq",e 20 20000)
      ,("pan",l (-1) 1)
      ,("rq",l 0.0001 0.9999)
      ])

    ,("foo",
      (map d $ words "freq dbAmp pan"))

    ,("bar",
      (map d $ words "freq dbAmp pan cf rq"))
    ]
    where
      l  = Extra.LinRange
      e  = Extra.ExpRange
      d n = (n, c) where
        c = maybe (error $ "Anonymous does not contain " ++ n) id
              (do { anon <- lookup "Anonymous" defaultControls
                  ; lookup n anon })

lookupControl :: ControlList -> String -> String -> Extra.ControlRange
lookupControl db sname pname =
    maybe (Extra.LinRange 0 1) id (lookup sname db >>= lookup pname)

synth_foo :: UGen
synth_foo = out (kr "out" 0) (pan2 osig pan 1)
  where
    osig  = sinOsc AR freq 0 * dbAmp amp
    freq  = k "freq" 2000
    amp   = k "dbAmp" (-15)
    pan   = k "pan" 0
    k n v = control KR n v `lag` 0.1

synth_bar :: UGen
synth_bar = out (kr "out" 0) (pan2 osig pan 1)
  where
    osig  = rlpf (saw AR freq) cf rq * dbAmp amp
    freq  = k "freq" 400
    cf    = k "cf" 2000
    rq    = k "rq" 0.5
    amp   = k "dbAmp" (-15)
    pan   = k "pan" 0
    k n v = control KR n v `lag` 0.1

-- | Alias to @control KR@.
kr :: String -> Double -> UGen
kr = control KR
