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

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import qualified Graphics.UI.Threepenny.Extra as Extra
import Paths_study_for_user_interfaces (getDataDir)

-- --------------------------------------------------------------------------
--
-- * GUI
--
-- --------------------------------------------------------------------------

main :: IO ()
main = withSC3 $ \fd0 -> withNotifications fd0 $ \fd -> do
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
                 foldr (\p acc -> paramToHTML fd name nid p : acc)
                 [divClear] prms)

divClear :: UI Element
divClear = UI.new # set style [("clear","both")]

paramToHTML :: Transport fd => fd -> String -> Int -> SynthParam -> UI Element
paramToHTML fd sname nid param = case param of
    pname :=  val
        | pname `elem` ignored -> showParam pname ":" val
        | otherwise            ->
            Extra.knobControl fd pname nid val (crange sname pname)
    pname :<- bus -> showParam pname ": c" bus
    pname :<= bus -> showParam pname ": a" bus
    where
      showParam n lbl v =
          UI.new
          # set text (n ++ lbl ++ show v)
          #. "param"
      crange = lookupControl defaultControls
      ignored = [ "out", "in" ]

-- | Synonym for list of controls.
--
-- Containing a pair of synthdef name and list of control parameter. Control
-- parameter is pair of parameter name and 'ControlRange' for the paramater of
-- synthdef.
--
type ControlList = [(String, [(String, Extra.ControlRange)])]

lookupControl :: ControlList -> String -> String -> Extra.ControlRange
lookupControl db sname pname =
    maybe (Extra.LinRange 0 1) id (lookup sname db >>= lookup pname)

defaultControls :: ControlList
defaultControls =
    [("Anonymous", defaultControlRanges)
    ,("foo",
      ("freq", Extra.ExpRange 100 800) : map d (words "dbAmp pan"))
    ,("bar", map d (words "freq dbAmp pan cf rq"))
    ]
    where
      d n   = (n, maybe (err n) id (lookup n defaultControlRanges))
      err n = error $ "Anonymous does not contain " ++ n

-- | Control ranges for default synthdef (Anonymous).
defaultControlRanges :: [(String, Extra.ControlRange)]
defaultControlRanges =
    [p "amp" l 0 1
    ,p "cf" e 20 20000
    ,p "cutoff" e 20 20000
    ,p "dbAmp" l (-60) 15
    ,p "freq" e 20 20000
    ,p "maxFreq" e 20 20000
    ,p "minFreq" e 20 20000
    ,p "pan" l (-1) 1
    ,p "prob" l 0 1
    ,p "rq" l 0.0001 0.9999
    ]
  where
    p n f start end = (n,f start end)
    l = Extra.LinRange
    e = Extra.ExpRange


-- --------------------------------------------------------------------------
--
-- * Synths for testing
--
-- --------------------------------------------------------------------------

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
