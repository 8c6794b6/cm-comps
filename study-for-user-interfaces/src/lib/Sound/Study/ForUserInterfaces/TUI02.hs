{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Textual user interface, take 2.

-}
module Sound.Study.ForUserInterfaces.TUI02 where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, when)
import Control.Monad.State (MonadTrans(..), MonadState(..), StateT(..), modify)
-- import Data.List (isPrefixOf)
import System.Random (mkStdGen)

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Supply
import Sound.SC3.TH.Synthdef (synthdefGenerator)
import Sound.SC3.Tree

import Sound.Study.ForUserInterfaces.TUI01 as TUI01
    ( audioBus, defaultTargetNid, sourceOut, masterNid, countOut, metroOut
    , tui01Synthdefs )

-- | Get node id of 'TUI01.synth_router' synth node. The group containing router
-- node must be added with 'addTrack'
routerNid ::
    Int -- ^ ID of Group node.
    -> Int
routerNid gid = gid * 1000

-- | Reserved control outbus to get number of used control out bus.
controlBusCounter :: Num a => a
controlBusCounter = 126

-- | Add nodes and empty groups used by TUI02.
initializeTUI02 :: Transport m => m ()
initializeTUI02 = do
    mapM_ (async . d_recv) ($(synthdefGenerator) ++ tui01Synthdefs)
    play $
        grp 0
        [ grp 1
          [ grp defaultTargetNid
            [ grp (masterNid+1)
              [ syn "metro" ["out"*=metroOut, "count"*=countOut] ]]
          , grp masterNid
            [ syn "router" ["in"*=sourceOut] ]]
        , grp 2 [] ]
    send $ c_set [(controlBusCounter,256)]
    send $ sync (-1)
    _ <- waitMessage
    sequence_ $ replicate 8 $ addTrack

-- | Add and setup new group.
addTrack :: Transport m => m Message
addTrack = do
    currentNodes <- getRootNode
    let gid = case queryN (nodeId ==? defaultTargetNid) currentNodes of
            []    -> 101
            [g10] -> case g10 of
                Group _ ns | not (null ns) -> nodeId (last ns) + 1
                           | otherwise     -> 101
                _                          -> error "Node id 10 is not a group"
            _     -> error "(Re) initialize the setup"
        obus = Dval $ fromIntegral $ audioBus gid
        g = grp gid
            [ syn "router" ["in"*=obus, "out"*=sourceOut] ]
    addNode defaultTargetNid (nodify g)
    send (sync $ audioBus gid)
    waitMessage

-- | Wrapper newtype to hold 'TrackState'.
newtype Track m a = Track {unTrack :: StateT TrackState m a}
    deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO
             , MonadState TrackState )

-- | State for single track.
data TrackState = TrackState
    { tsGroupId       :: Int
    , tsOffset        :: Int
    , tsCurrentNode   :: SCNode
    , tsTargetNodes   :: [SCNode]
    , tsControlBusNum :: Int
    , tsBeatCount     :: Float
    -- XXX: Add record field to store node id offset.
    -- XXX: Add record to hold list of Message to send.
    }

instance SendOSC m => SendOSC (Track m) where
    sendOSC = lift . sendOSC

instance RecvOSC m => RecvOSC (Track m) where
    recvPacket = lift recvPacket

instance DuplexOSC m => DuplexOSC (Track m)

instance Transport m => Transport (Track m)

-- | Do action with 'Track'.
track ::
    Transport m
    => Int       -- ^ Group ID of this track.
    -> Track m a -- ^ Action to do.
    -> m a
track groupId trck = do
    -- XXX: Update metro synthdef and use sendReply?
    send (c_get [controlBusCounter,countOut])
    [_,Float currentBusNum,_,Float cnt] <- waitDatum "/c_set"

    node <- getNode groupId
    (val,st) <- runStateT (unTrack trck)
                TrackState { tsGroupId = groupId
                           , tsOffset  = 0
                           , tsCurrentNode = node
                           , tsTargetNodes = []
                           , tsControlBusNum  = truncate currentBusNum
                           , tsBeatCount = cnt
                           }
    -- XXX: Send whole message
    when (currentBusNum /= fromIntegral (tsControlBusNum st)) $
         send $ c_set [(controlBusCounter,fromIntegral (tsControlBusNum st))]
    return val

-- | Add new source synth to head of specified group.
addSource ::
    (Transport m)
    => String    -- ^ Synthdef name.
    -> Track m ()
addSource name = do
    targetNid <- liftM tsGroupId get
    let obus = fromIntegral $ audioBus targetNid
    sendOSC $ bundle immediately $
        [ s_new name (-1) AddToHead targetNid [("out",obus)]]

-- | Add new effect synth to specified group before 'synth_router'.
addFx ::
    Transport m
    => String -- ^ Synthdef name.
    -> Track m ()
addFx name = do
    targetNid <- liftM tsGroupId get
    let obus = fromIntegral $ audioBus targetNid
    sendOSC $ s_new name (-1) AddBefore (routerNid targetNid)
        [("out",obus),("in",obus)]

-- | Free nodes matching to given query string, in specified group.
freeNodes :: Transport m => String -> Track m ()
freeNodes = freeNodesCond . qString

-- | Free nodes matching to given condition, in specified group.
freeNodesCond :: Transport m => Condition SCNode -> Track m ()
freeNodesCond cond =
    send . n_free . map nodeId . queryN cond . tsCurrentNode =<< get

-- | Apply parameter actions to nodes specified by query string.
source ::
    Monad m
    => String     -- ^ Query string, see 'qString'.
    -> Track m a  -- ^ Action for parameter.
    -> Track m a
source qstr act = do
    -- XXX: Add synth to SCNode built from current haskell source code.
    modify $ \st ->
        st {tsTargetNodes = queryN (qString qstr) (tsCurrentNode st)}
    act

-- | Run effect synth, apply paremater actions to nodes.
effect :: Monad m => String -> Track m a -> Track m a
effect = undefined

-- | Translate to condition:
--
-- * \'*\' - entire node
--
-- * .NAME - synth with synthdef name /NAME/.
--
-- * #ID   - synth with node id /ID/.
--
qString :: String -> Condition SCNode
qString str0 = foldr1 (||?) (map parse (words str0))
  where
    parse str =
        case str of
            '*':_    -> const True
            '.':name -> synthName ==? name
            '#':nid  -> nodeId    ==? read nid
            _        -> error ("qString: Unknown query " ++ str)

-- | Assign parameter with given value.
(==>) ::
    (Transport m, Assignable a)
    => String  -- ^ Parameter name.
    -> a       -- ^ Value.
    -> Track m ()
name ==> value =
    mapM_ (\nd -> assign nd name value) . tsTargetNodes =<< get

infixr 1 ==>

-- | Data to control synth parameter with 'Supply'.
data Vsupply = Vsupply Supply

-- | Triggered variant of 'Vsupply'.
data Tsupply = Tsupply Supply

-- | Wraps given 'Supply' to synth parameter.
vsup :: Supply -> Vsupply
vsup = Vsupply

-- | Triggered variang of 'vsup'.
tsup :: Supply -> Tsupply
tsup = Tsupply

class Assignable a where
    assign ::
        Transport m
        => SCNode       -- ^ Target node to assign value.
        -> String       -- ^ Parameter name.
        -> a            -- ^ Value to assign.
        -> Track m ()  -- ^ New output bus, could be same as given value.

instance Assignable Double where
    assign nd name val = do
        send $ n_set (nodeId nd) [(name,val)]

instance Assignable UGen where
    assign nd name val = do
        sendControl nd name (const val)

instance Assignable (UGen -> UGen) where
    assign nd name f = sendControl nd name f

instance Assignable Vsupply where
    assign nd name (Vsupply val) = do
        let ug tr = demand tr 1 (evalSupply val (mkStdGen 0x12345678))
        sendControl nd name ug

instance Assignable Tsupply where
    assign nd name (Tsupply val) = do
        let ug tr = tr * demand tr 1 (evalSupply val (mkStdGen 0x12345678))
        sendControl nd name ug


-- | Send UGen and map control output to synth specified by given 'SCNode'.
sendControl ::
    Transport m
    => SCNode
    -> String
    -> (UGen -> UGen)
    -> Track m ()
sendControl node pname fu = do
    ts <- get
    let cnt         = tsBeatCount ts
        beatOffset  = tsOffset ts
        currentObus = tsControlBusNum ts
        nid    = nodeId node
        cnt'   | beatOffset <= 0 = 0
               | otherwise       =
                   (ceiling (cnt / fromIntegral beatOffset) + 1) *
                   beatOffset
        tr     = gate (control KR "tr" 0) exceed
        exceed = in' 1 KR countOut >* control KR "count" 0
        fr     = free exceed $ mce $ map (constant . nodeId) psynths
        osig   = out (control KR "out" 0) (gate (fu tr) exceed)
        -- Free existing synths with new synthdef with 'free' ugen.
        -- Node id of old synthdef is known at this point.
        sdef
            | psynthExist = mrg [osig, fr]
            | otherwise   = osig
        pdefname = concat ["p_",show nid,"_",pname,"_",show (hashUGen osig)]
        -- output bus mapped to pname of snth
        mbObus  = queryP' (paramName ==? pname) node
        (reusing,obus) = case mbObus of
            Just (_ :<- v) -> (True,v)
            _              -> (False,currentObus)
        -- Condition for synth controlling specified parameter:
        -- - Parameter is mapped to control rate out bus.
        -- - The out bus is mapped to pname of snth.
        pcond p = case p of
            n := v | n == "out" && v == fromIntegral obus -> True
            _                                             -> False
        psynths = queryN (params pcond) (tsCurrentNode ts)
        psynthExist = not (null psynths)
        psynthChanged = not (pdefname `elem` map synthName psynths)
        (addAction,targetNid) = (AddBefore, nid)
    when psynthChanged $ do
        liftIO $ putStrLn $ unlines
            [ "Updating node       : " ++ show  nid
            , "Parameter UGen name : " ++ pdefname ]
        -- XXX: Dont send at this moment, add to state.
        send $ withCM
            (d_recv $ synthdef pdefname sdef)
            (bundle immediately
             [s_new pdefname (-1) addAction targetNid
              [("out",fromIntegral obus),("count",fromIntegral cnt')]
             , n_map (-1) [("tr",metroOut)]
             , n_map nid [(pname, obus)] ])
    modify $ \st ->
        st {tsControlBusNum = if reusing then currentObus else currentObus+1}

-- | Dump nodes in track.
dumpTrack :: Transport m => Track m ()
dumpTrack =
    get >>= getNode . tsGroupId >>=
    liftIO . putStrLn . drawSCNode
    -- filterSCNode (isGroup ||? (not . ("p_" `isPrefixOf`) . synthName))

-- | Set offset to update the parameters.
offset :: Monad m => Int -> Track m ()
offset count = modify $ \st -> st {tsOffset = count}


-- --------------------------------------------------------------------------
--
-- * Functions should be defined elsewhere
--
-- --------------------------------------------------------------------------

-- | Filter 'SCNode' with given condition.
filterSCNode :: (SCNode -> Bool) -> SCNode -> SCNode
filterSCNode p n0 =
    let f n acc =
            case n of
                Group i ns | p n       -> Group i (foldr f [] ns) : acc
                           | otherwise -> acc
                Synth {}   | p n       -> n : acc
                           | otherwise -> acc
    in head $ foldr f [] [n0]


-- --------------------------------------------------------------------------
--
-- * Experiments
--
-- --------------------------------------------------------------------------

synth_def1 :: UGen
synth_def1 = osig
  where
    osig = out obus (sinOsc AR freq 0 * decay tr 0.123 * 0.3)
    tr   = dust 'k' KR df
    df   = linLin (sinOsc KR (1/64) 0) (-1) 1 1 30
    freq = control AR "freq" 440
    obus = control KR "out" 0

synth_def2 :: UGen
synth_def2 = out obus (sinOsc AR freq 0 * mul + add)
  where
    freq = k "freq" 440
    mul  = k "mul" 1
    add  = k "add" 0
    obus = k "out" 0
    k    = control KR

synth_def3 :: UGen
synth_def3 = out obus (sinOsc KR freq 0 * mul + add)
  where
    freq = k "freq" 440
    mul  = k "mul" 1
    add  = k "add" 0
    obus = k "out" 0
    k    = control KR

s_new_ex01 :: IO ()
s_new_ex01 = withSC3 $ do
    send $ withCM
        (d_recv
         (synthdef "foo"
          (out 0 (pan2 (sinOsc AR 440 0 * 0.1) 0 1))))
        (s_new "foo" 1002 AddToTail 1 [])

s_new_ex02 :: IO ()
s_new_ex02 = withSC3 $ do
    send $ withCM
        (d_recv
         (synthdef "foo2"
          (out 0
           (sinOsc AR (control KR "freq" 440) 0 * 0.1))))
         (bundle immediately
          [ s_new "foo2" (-1) AddToTail 1 [("freq",330)]
          , s_new "foo2" (-1) AddToTail 1 [("freq",440)]
          , s_new "foo2" (-1) AddToTail 1 [("freq",550)] ])

controlBus_ex01 :: IO ()
controlBus_ex01 = withSC3 $ do
    let f1 = syn "def2"
               ["freq"*=3,"mul"*=220,"add"*=330,"out"*=2]
        f2 = syn "def3"
               ["freq"*=3,"mul"*=220,"add"*=330,"out"*=1023]
        nd =
            grp 0
            [ grp 1
              [ grp 10 [f1,f2]
              , grp 11
                [ syn "def1" ["freq"*<-f2-*"out","out"*=1]
                , syn "def1" ["freq"*<=f1-*"out","out"*=0] ]]]

    mapM_ (async . d_recv . uncurry synthdef)
        [("def1",synth_def1),("def2",synth_def2),("def3",synth_def3)]
    play nd

audioBus_ex01 :: IO ()
audioBus_ex01 = withSC3 $ do
    return ()

-- in dumped message
-- audio bus 0 = "c-552556612"
-- audio bus 1 = "c-552556548"
-- audio bus 2 = "c-552556484"

queryTree_ex01 :: IO ()
queryTree_ex01 = withSC3 $ do
    send $ g_queryTree [(11,True)]
    msg <- waitMessage
    liftIO $ putStrLn $ messagePP msg

withSC3' :: Connection TCP a -> IO a
withSC3' = withTransport (openTCP "127.0.0.1" 57111)

-- for taking 'diffMessage'.

nd01 =
    Group 101
    [ Synth 10101 "foo" []
    , Synth 10102 "bar" [] ]

nd02 =
    Group 101
    [ Synth 10101 "foo" []
    , Synth 10103 "buzz" []
    , Synth 10102 "bar" [] ]
