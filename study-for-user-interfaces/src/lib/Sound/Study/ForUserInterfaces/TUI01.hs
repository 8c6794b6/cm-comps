{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Textual user interface for realtime performance, take 1. Managing routed
control bus signal sequences with demand UGens.

One downside of routing busses with emand UGen outputs is, not easy to translate
as non-realtime score. Though this is same for quite a lot of environment
dedicated for realtime performance. Use other method than OSC score to record
the audio output of performance.

-}
module Sound.Study.ForUserInterfaces.TUI01 where

import Control.Monad (foldM, foldM_)
import System.Random

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Supply
import Sound.SC3.TH.Synthdef (synthdefGenerator)
import Sound.SC3.Tree

-- --------------------------------------------------------------------------
--
-- * Synthdefs
--
-- --------------------------------------------------------------------------

-- | Synth to synchronize other synths, mainly demand UGens.
synth_metro :: UGen
synth_metro = out (control KR "out" 0) osig
  where
    osig = impulse KR (beat*bpm/60) 0
    bpm  = control KR "bpm" 120
    beat = control KR "beat" 4

-- | Default mixer for new synth added to scsynth server.
synth_tuie00 :: UGen
synth_tuie00 = out obus osig
  where
    osig = in' 2 AR isig * amp
    obus = control KR "out" 0
    isig = control KR "in" 0
    amp  = control KR "amp" 0


-- --------------------------------------------------------------------------
--
-- * Constants
--
-- --------------------------------------------------------------------------

-- | Reserved output bus number for output of metro.
metroOut :: Num a => a
metroOut = 128

-- | Reserved output bus for source synths.
sourceOut :: Num a => a
sourceOut = 16

defaultTargetNid :: Num a => a
defaultTargetNid = 10

-- | Reserved node id for master output.
masterNid :: Num a => a
masterNid = 99

-- | Initial setup. Currently not working.
initialTUI01Nodes :: Nd
initialTUI01Nodes =
    grp 0
    [ grp 1
      [ grp defaultTargetNid
        [ grp (masterNid+1)
          [ syn "metro" [] ]
        ]
      , grp masterNid
        [ syn "tuie00" [] ]
      ]
    , grp 2 [] ]

{- XXX:

There is a bug that 'initialTUI01Nodes' wont be added to scsynth server
correctly. When sending below Nd:

    let nd =
      grp 0
      [ grp 1
        [ grp 10 []
        , grp 11 []
        ]
      , grp 2 []
      ]

>>> withSC3 (patchNode nd)

Results in:

    0 group
       1 group
          10 group
             11 group
       2 group

Group 11 is added as child of group 10, not as child of group 1.

-}

-- | Add nodes used by tui01.
initializeTUI01 :: IO ()
initializeTUI01 = withSC3 $ do
    mapM_  (async . d_recv) $(synthdefGenerator)
    sendOSC $ bundle immediately
        [ g_new [(defaultTargetNid,AddToHead,1)
                ,(masterNid,AddAfter,defaultTargetNid)
                ,(masterNid+1,AddToHead,defaultTargetNid)
                ]
        , s_new "metro" 10000 AddToHead (masterNid+1) [("out",metroOut)]
        , s_new "tuie00" 11000 AddToHead masterNid [("in",sourceOut)]
        ]
    --
    -- XXX: Fix the bug and use:
    --
    -- >>> patchNode (nodify initialTUI01Nodes)
    --

-- | Get next output bus.
--
-- Takes current node graph, looks for control bus mapped to existing synth and
-- value of parameter named /\"out\"/.
nextOutBus ::
    Num a
    => SCNode -- ^ Current node graph.
    -> a
nextOutBus currentNode =
    let cond = paramName ==? "out" ||?
               (\p -> case p of _ :<- _ -> True; _ -> False)
        outs = [paramValue p | p <- queryP cond currentNode]
    in  case outs of
        [] -> metroOut + 1
        ps -> fromIntegral
              (ceiling $ maximum [p | p <- ps] + 1 :: Int)

-- --------------------------------------------------------------------------
--
-- * Functions to update scsynth server
--
-- --------------------------------------------------------------------------

-- | Synthdef used to controlling parameter with 'sendParam'.
synth_sendParam :: UGen
synth_sendParam =
    out (control KR "out" 0)
    (line KR
     (control KR "from" 0) (control KR "to" 0) (control KR "dur" 0)
     RemoveSynth)

-- | Set parameter of specified nodes, from current value to specified value.
--
-- Sends a synthdef and route the output bus to parameters of nodes matching
-- given condition. When parameter is frequently changing, may hear glitches.
--
sendParam ::
    Condition SCNode -- ^ Condition for target nodes.
    -> String        -- ^ Parameter name.
    -> Double        -- ^ Target value.
    -> Double        -- ^ Duration in seconds.
    -> IO ()
sendParam condition pname value dur = withSC3 $ do
    currentNode <- getRootNode
    let srcs = queryN condition currentNode
        assignOut nid obus fromValue =
            sendOSC $ bundle immediately
            [ s_new "sendParam" (-1) AddToHead defaultTargetNid
              [("out",obus),("from",fromValue),("to",value),("dur",dur)]
            , n_map nid [(pname, ceiling obus)]
            ]
        -- When source parameter is mapped to control bus, free the synth
        -- controlling the specified output. Do nothing when parameter is mapped to
        -- audio rate bus.
        go obus src | isSynth src = do
            case [p | p <- synthParams src, paramName p == pname ] of
                (_ := currentValue) :_ -> do
                    assignOut (nodeId src) obus currentValue
                    return (obus+1)
                (_ :<- currentBus) :_ -> do
                    let cond n =
                            not $ null
                                [n| "out" := fromIntegral currentBus `elem`
                                    synthParams n
                                  ]
                        currentControlNids =
                            map nodeId (queryN cond currentNode)
                    send $ n_free currentControlNids
                    send (c_get [currentBus])
                    [Int32 _, Float currentValue] <- waitDatum "/c_set"
                    assignOut (nodeId src) obus (realToFrac currentValue)
                    return (obus+1)
                _                     -> return obus
                    | otherwise = do
            let Group _ srcs' = src in foldM go obus srcs'
    foldM_ go (nextOutBus currentNode) srcs

-- | Free the synth controlling parameter if exist, set the value of specified
-- parameter with given value and 'n_set'.
freeParam :: Condition SCNode -> String -> IO ()
freeParam cond pname = withSC3 $ do
    currentNodes <- getRootNode
    case queryN cond currentNodes of
        [] -> return ()
        ns -> return ()

-- | Add new synthdef and mixer.
sendSynth ::
    String    -- ^ Synthdef name.
    -> IO Message
sendSynth name = withSC3 $ do
    currentNodes <- getRootNode
    let gid = case queryN (nodeId ==? defaultTargetNid) currentNodes of
            []    -> 101
            [g10] -> case g10 of
                Group _ ns | not (null ns) -> nodeId (last ns) + 2
                           | otherwise     -> 101
                _          -> error "Node id 10 is not a group."
            _     -> error "(Re) Initialize the setup."
        gid' = fromIntegral gid
        g = grp gid
            [ syn name ["out"*=gid']
            , syn "tuie00" ["in"*=gid',"out"*=sourceOut]
            ]
    addNode defaultTargetNid (nodify g)
    send (sync $ hash name)
    waitMessage

-- | Function to route 'Supply' to synth found in current running node graph.
sendSupply01 ::
    String     -- ^ Target synthdef name.
    -> String  -- ^ Target parameter name.
    -> Bool    -- ^ True if making output signal as trigger.
    -> Supply  -- ^ Pattern sequenced with trigger from 'synth_metro'.
    -> IO ()
sendSupply01 sname pname isTrig sup = sendControl sname pname ug
  where
    ug = (\tr ->
           (if isTrig then (* tr) else id)
           (demand tr 0 (evalSupply sup (mkStdGen 0x123456))))

-- | Function to route 'UGen' to synth found in current running node graph.
sendControl ::
    String    -- ^ Target synthdef name.
    -> String -- ^ Target parameter name.
    -> (UGen -> UGen)
    -- ^ Value mapped to parameter. Takes output from 'synth_metro'.
    -> IO ()
sendControl sname pname ugen = withSC3 $ do
    currentNode <- getRootNode
    case queryN (synthName ==? sname) currentNode of
        snth:[]  -> do
            let nid  = nodeId snth
                tr  = control KR "tr" 0
                sdef = out (control KR "out" 0) (ugen tr)
                pdefname = paramDefName sname pname
                psynths = queryN (synthName ==? pdefname) currentNode
                (addAction,targetNid)
                    | psynthExist = (AddReplace, nodeId (head psynths))
                    | otherwise   = (AddBefore,  nid)
                psynthExist = not (null psynths)
            let obus = if psynthExist
                    then paramValue $
                         head [p | p <- synthParams $ head psynths
                                 , paramName p == "out"
                                 ]
                    else nextOutBus currentNode
            _ <- async $ d_recv $ synthdef pdefname sdef
            sendOSC $ bundle immediately
                [ s_new pdefname (-1) addAction targetNid [("out",obus)]
                , n_map (-1) [("tr",metroOut)]
                , n_map nid [(pname,ceiling obus)] ]
            liftIO $ print nid
        _:_       -> liftIO $ putStrLn ("More than one '" ++ sname ++ "' found.")
        []        -> liftIO $ putStrLn "No matching synth found."


-- | Send effect synth with 'AddAfter' add action of target synth.
sendFx ::
    String    -- ^ Name synth for getting target node id.
    -> String -- ^ Name of new synth to add.
    -> IO ()
sendFx sname ename = withSC3 $ do
    currentNode <- getRootNode
    case queryN (synthName ==? sname) currentNode of
        snth:[] -> do
            let targetNid = nodeId snth
                sout      = [p | p <- synthParams snth, paramName p == "out"]
                sout'     = if null sout then 0 else paramValue $ head sout
            sendOSC $ bundle immediately
                [ s_new ename (-1) AddAfter targetNid
                  [("in",sout'),("out",sout')]
                ]
        _:_      -> liftIO $ putStrLn ("More than one '" ++ sname ++ "' found.")
        _        -> liftIO $ putStrLn "No matching synth found."

-- | Send effect synth just before master output.
sendMasterFx :: String -> IO ()
sendMasterFx ename = withSC3 $
    sendOSC $ bundle immediately
        [ s_new ename (-1) AddToHead masterNid
          [("in",sourceOut),("out",sourceOut)]
        ]

-- | Make a synthdef name used for controlling parameter of given synthdef.
paramDefName :: String -> String -> String
paramDefName sName pName = sName ++ "." ++ pName

-- --------------------------------------------------------------------------
--
-- * Functions which should be defined elsewheres
--
-- --------------------------------------------------------------------------

-- | Orphan instance....
instance Audible Nd where
    play = patchNode . nodify

-- | XXX: Should be moved to "Sound.SC3.Tree.Query"
isSynth :: SCNode -> Bool
isSynth n = case n of Synth _ _ _ -> True; _ -> False

-- | XXX: Should be moved to "Sound.SC3.Tree.Query"
isGroup :: SCNode -> Bool
isGroup = not . isSynth
