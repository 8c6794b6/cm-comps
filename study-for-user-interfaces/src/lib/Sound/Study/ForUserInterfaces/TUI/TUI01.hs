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
module Sound.Study.ForUserInterfaces.TUI.TUI01 where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (foldM, foldM_, forever)
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
synth_metro = mrg [ out (control KR "out" 0) osig
                  , out (control KR "count" 0) pcnt ]
  where
    osig = impulse KR (beat*bpm/60) 0
    bpm  = control KR "bpm" 120
    beat = control KR "beat" 4
    pcnt = pulseCount (pulseDivider osig beat 0) 0

-- | Default mixer for new synth added to scsynth server.
synth_router :: UGen
synth_router = out obus osig
  where
    osig = in' 2 AR isig * amp
    obus = control KR "out" 0
    isig = control KR "in" 0
    amp  = control KR "amp" 0

-- | Synthdef used to controlling parameter with 'sendParam'.
synth_sendParam :: UGen
synth_sendParam =
    out (control KR "out" 0)
    (line KR
     (control KR "from" 0) (control KR "to" 0) (control KR "dur" 0)
     RemoveSynth)

-- --------------------------------------------------------------------------
--
-- * Constants
--
-- --------------------------------------------------------------------------

-- | Reserved control output bus number for output of metro trigger.
metroOut :: Num a => a
metroOut = 128

-- | Reserved control output bus number for output of beat count.
countOut :: Num a => a
countOut = 127

-- | Reserved audio output bus for source synths.
sourceOut :: Num a => a
sourceOut = 16

-- | Reserved node id for default target node to add new nodes.
defaultTargetNid :: Num a => a
defaultTargetNid = 10

-- | Reserved node id for master output.
masterNid :: Num a => a
masterNid = 99

-- | Initial setup.
initialTUI01Nodes :: Nd
initialTUI01Nodes =
    grp 0
    [ grp 1
      [ grp defaultTargetNid
        [ grp (masterNid+1)
          [ syn "metro" ["out"*=metroOut,"count"*=countOut] ]]
      , grp masterNid
        [ syn "router" ["in"*=sourceOut] ]]
    , grp 2 [] ]

-- | Add nodes used by tui01.
initializeTUI01 :: Transport m => m ()
initializeTUI01 = do
    mapM_  (async . d_recv) tui01Synthdefs
    play initialTUI01Nodes

tui01Synthdefs :: [Synthdef]
tui01Synthdefs = $(synthdefGenerator)


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
        ps -> fromIntegral (ceiling $ maximum [p | p <- ps] + 1 :: Int)

-- | Get audio bus number from group ID.
audioBus :: Int -> Int
audioBus gid = 16 + (gid-100)*2

-- --------------------------------------------------------------------------
--
-- * Functions to update scsynth server
--
-- --------------------------------------------------------------------------


-- | Set parameter of specified nodes, from current value to specified value.
--
-- Sends a synthdef and route the output bus to parameters of nodes matching
-- given condition. When parameter is frequently changing, may hear glitches.
--
sendParam ::
    Transport m
    => Condition SCNode -- ^ Condition for target nodes.
    -> String        -- ^ Parameter name.
    -> Double        -- ^ Target value.
    -> Double        -- ^ Duration in seconds.
    -> m ()
sendParam condition pname value dur = do
    currentNode <- getRootNode
    let srcs = queryN condition currentNode
        assignOut nid obus fromValue =
            sendOSC $ bundle immediately
            [ s_new "sendParam" (-1) AddToHead defaultTargetNid
              [("out",obus),("from",fromValue),("to",value),("dur",dur)]
            , n_map nid [(pname, ceiling obus)]
            ]
        -- When source parameter is mapped to control bus, free the synth
        -- controlling the specified output. Do nothing when parameter is mapped
        -- to audio rate bus.
        go obus src = case src of
            Synth {} ->
              case [p | p <- synthParams src, paramName p == pname ] of
                  (_ := currentValue) :_ -> do
                      assignOut (nodeId src) obus currentValue
                      return (obus+1)
                  (_ :<- currentBus) :_ -> do
                      let cond n =
                              "out" := fromIntegral currentBus `elem` synthParams n
                          currentControlNids =
                              map nodeId (queryN cond currentNode)
                      send $ n_free currentControlNids
                      send (c_get [currentBus])
                      [Int32 _, Float currentValue] <- waitDatum "/c_set"
                      assignOut (nodeId src) obus (realToFrac currentValue)
                      return (obus+1)
                  _                     -> return obus
            Group _ srcs' -> foldM go obus srcs'
    foldM_ go (nextOutBus currentNode) srcs

-- | Free the synth controlling parameter if exist, set the value of specified
-- parameter with given value and 'n_set'.
freeParam :: Condition SCNode -> String -> IO ()
freeParam cond _pname = withSC3 $ do
    -- XXX: TODO.
    currentNodes <- getRootNode
    case queryN cond currentNodes of
        []  -> return ()
        _ns -> return ()

-- | Add new synthdef and mixer.
sendSynth ::
    Transport m
    => String    -- ^ Synthdef name.
    -> m Message
sendSynth name = do
    currentNodes <- getRootNode
    let gid = case queryN (nodeId ==? defaultTargetNid) currentNodes of
            []    -> 101
            [g10] -> case g10 of
                Group _ ns | not (null ns) -> nodeId (last ns) + 1
                           | otherwise     -> 101
                _          -> error "Node id 10 is not a group."
            _     -> error "(Re) Initialize the setup."
        obus = Dval $ fromIntegral $ audioBus gid
        g = grp gid
            [ syn name ["out"*=obus]
            , syn "router" ["in"*= obus,"out"*=sourceOut] ]
    addNode defaultTargetNid (nodify g)
    send (sync $ hash name)
    waitMessage

-- | Function to route 'Supply' to synth found in current running node graph.
sendSupply01 ::
    Transport m
    => String  -- ^ Target synthdef name.
    -> String  -- ^ Target parameter name.
    -> Bool    -- ^ True if making output signal as trigger.
    -> Supply  -- ^ Pattern sequenced with trigger from 'synth_metro'.
    -> m ()
sendSupply01 sname pname isTrig sup = sendControl01 sname pname ug
  where
    ug tr =
        (if isTrig then (* tr) else id)
        (demand tr 0 (evalSupply sup (mkStdGen 0x123456)))

-- | Function to route 'UGen' to synth found in current running node graph.
sendControl01 ::
    Transport m
    => String    -- ^ Target synthdef name.
    -> String -- ^ Target parameter name.
    -> (UGen -> UGen)
    -- ^ Value mapped to parameter. Takes output from 'synth_metro'.
    -> m ()
sendControl01 sname pname ugen = do
    currentNode <- getRootNode
    case queryN (synthName ==? sname) currentNode of
        snth:[]  -> do
            let nid      = nodeId snth
                tr       = control KR "tr" 0
                sdef     = out (control KR "out" 0) (ugen tr)
                pdefname = paramDefName sname pname
                psynths  = queryN (synthName ==? pdefname) currentNode
                psynthExist = not (null psynths)
                (addAction,targetNid)
                    | psynthExist = (AddReplace, nodeId (head psynths))
                    | otherwise   = (AddBefore,  nid)
                obus
                    | psynthExist =
                        paramValue $
                        head [p | p <- synthParams $ head psynths
                                , paramName p == "out"
                                ]
                    | otherwise   = nextOutBus currentNode
            _ <- async $ d_recv $ synthdef pdefname sdef
            sendOSC $ bundle immediately
                [ s_new pdefname (-1) addAction targetNid [("out",obus)]
                , n_map (-1) [("tr",metroOut)]
                , n_map nid [(pname,ceiling obus)] ]
            liftIO $ print nid
        _:_       -> liftIO $ putStrLn ("More than one '" ++ sname ++ "' found.")
        []        -> liftIO $ putStrLn "No matching synth found."

-- | Amount of offset beat to wait before start sending trigger signal from
-- 'synth_metro'.
type BeatOffset = Int

-- | Send control with specified beat offset.
sendControl02 ::
    Transport m
    => Condition SCNode -- ^ Condition fo node.
    -> String           -- ^ Parameter name.
    -> BeatOffset       -- ^ Beat offset count.
    -> (UGen -> UGen)   -- ^ Function taking impulse from 'synth_metro'.
    -> m ()
sendControl02 cond pname beatOffset ug = do
    currentNodes <- getRootNode
    case queryN cond currentNodes of
        snth:[] -> do
            send $ c_get [countOut]
            [_,Float cnt] <- waitDatum "/c_set"
            let nid    = nodeId snth
                cnt'   | beatOffset <= 0 = 0
                       | otherwise       =
                           (ceiling (cnt / fromIntegral beatOffset) + 1) *
                           beatOffset
                tr     = gate (control KR "tr" 0) exceed
                exceed = in' 1 KR countOut >* constant cnt'
                fr     = free exceed $ mce $ map (constant . nodeId) psynths
                -- Free existing synths with new synthdef with 'free' ugen.
                -- Node id of old synthdef is known at this point.
                sdef
                    | psynthExist = mrg [out (control KR "out" 0) (ug tr), fr]
                    | otherwise   = out (control KR "out" 0) (ug tr)
                pdefname = "ctrl." ++ pname
                -- output bus mapped to pname of snth
                mbObus  = queryP' (paramName ==? pname) snth
                obus    = case mbObus of
                    Just (_ :<- v) -> fromIntegral v
                    _              -> nextOutBus currentNodes
                -- Condition for synth controlling specified parameter:
                -- - Parameter is mapped to control rate out bus.
                -- - The out bus is mapped to pname of snth.
                pcond p = case p of
                    n := v | n == "out" && v == obus -> True
                    _                                -> False
                psynths = queryN (params pcond) currentNodes
                psynthExist = not (null psynths)
                (addAction,targetNid) = (AddBefore, nid)
            _ <- async $ d_recv $ synthdef pdefname sdef
            sendOSC $ bundle immediately
                [ s_new pdefname (-1) addAction targetNid [("out",obus)]
                , n_map (-1) [("tr",metroOut)]
                , n_map nid [(pname,ceiling obus)] ]
            liftIO $ print nid
        _  -> liftIO $ putStrLn "Did nothing"

-- | Sends 'Supply', take 2.
sendSupply02 ::
    Transport m
    => Condition SCNode -- ^ Condition for querying nodes.
    -> String           -- ^ Parameter name.
    -> BeatOffset       -- ^ Offset count for beats.
    -> Bool             -- ^ Make the values triggered or not.
    -> Supply           -- ^ Pattern to send.
    -> m ()
sendSupply02 cond pname beatOffset isTrig sup =
    sendControl02 cond pname beatOffset
    (\tr -> (if isTrig then (* tr) else id)
            (demand tr 0 (evalSupply sup (mkStdGen 0x123456))))

data SupplyInfo = SupplyInfo
     { siName :: String
     , siIsTrig :: Bool
     , siSupply :: Supply
     } deriving (Eq, Show)

sendSupplys ::
    Transport m
    => Condition SCNode
    -> BeatOffset
    -> [SupplyInfo]
    -> m ()
sendSupplys _cond _beatOffset _sups = do
    liftIO $ putStrLn "Not yet implemented"

-- | Send effect synth with 'AddAfter' add action of target synth.
sendFx ::
    Transport m
    => String -- ^ Name synth for getting target node id.
    -> String -- ^ Name of new synth to add.
    -> m ()
sendFx sname ename = do
    currentNode <- getRootNode
    case queryN (synthName ==? sname) currentNode of
        snth:[] -> do
            let targetNid = nodeId snth
                sout      = [p | p <- synthParams snth, paramName p == "out"]
                sout'     = if null sout then 0 else paramValue $ head sout
            sendOSC $ bundle immediately
                [ s_new ename (-1) AddAfter targetNid
                  [("in",sout'),("out",sout')] ]
        _:_      -> liftIO $ putStrLn ("More than one '" ++ sname ++ "' found.")
        _        -> liftIO $ putStrLn "No matching synth found."

-- | Send effect synth just before master output.
sendMasterFx :: Transport m => String -> m ()
sendMasterFx ename =
    sendOSC $ bundle immediately
        [ s_new ename (-1) AddToHead masterNid
          [("in",sourceOut),("out",sourceOut)] ]

-- | Free nodes matching to given condition.
freeNodes :: Transport m => Condition SCNode -> m ()
freeNodes cond = send .  n_free . map nodeId . queryN cond =<< getRootNode

-- | Make a synthdef name used for controlling parameter of given synthdef.
paramDefName :: String -> String -> String
paramDefName sName pName = sName ++ "." ++ pName


-- --------------------------------------------------------------------------
--
-- * Playing with synchronization
--
-- --------------------------------------------------------------------------

timer_01 :: IO ()
timer_01 = do
    let def  = mrg [sendReply (tr_control "t_tr" 0) 0 "/timer" [tm1]
                   , tm2 ]
        buf  = asLocalBuf 'β' [0]
        tm1  = recip controlRate + bufRd 1 KR buf 0 NoLoop NoInterpolation
        tm2  = bufWr buf 0 NoLoop tm1
    withSC3 $ do
        _ <- async $ d_recv $ synthdef "timer01" def
        send $ s_new "timer01" 1000 AddToTail 1 []
    tid <- forkIO $
           let go = do
                   send $ n_set 1000 [("t_tr",1)]
                   [_,_,Float t] <- waitDatum "/timer"
                   liftIO $ putStrLn $ unwords ["time:",show t]
                   go
           in  withSC3 $ withNotifications go
    getChar >> killThread tid

askTime :: Transport m => m ()
askTime = do
    send $ n_set 1000 [("t_tr",1)]
    [_,_,Float t] <- waitDatum "/timer"
    liftIO $ putStrLn $ unwords ["time:",show t]

askTimeOnce :: IO ()
askTimeOnce = withSC3 $ withNotifications askTime

askTimeFork :: IO ()
askTimeFork = do
    tid <- forkIO $ withSC3 $ withNotifications (forever askTime)
    getChar >> killThread tid
