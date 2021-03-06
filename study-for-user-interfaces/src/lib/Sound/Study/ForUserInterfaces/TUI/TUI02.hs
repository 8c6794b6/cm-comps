{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Textual user interface, take 2.

TUI02 has a concept of 'Track', which aligns given source synths and effect
synths with parameter controlling adhoc UGens. Source and effect synth setup is
idemopotent, initialization and update of track could be done with invoking same
code block in Haskell code.

See "Session.Session06" and "Session.Session06b" for example.

/Known limitations/:

* Ordering nodes may not work properly, need to remove the node once, then send
again with new ordering. Might need couple updates in diff related functions in
hsc3-tree package.

* Returned @/g_queryTree.reply@ could get too large for single UDP communication
to receive. Current workarounds are: query single group only, or drop support
for UDP which means support TCP connection only.

-}
module Sound.Study.ForUserInterfaces.TUI.TUI02
    ( module Sound.Study.ForUserInterfaces.TUI.TUI02
    , module Sound.OSC
    , module Sound.SC3
    , module Sound.SC3.ID
    , module Sound.SC3.Supply
    , module Sound.SC3.TH.Synthdef
    , module Sound.SC3.Tree
    ) where

import           Control.Applicative                  (Applicative (..))
import           Control.Monad                        (unless, when)
import           Control.Monad.State.Strict           (MonadState (..),
                                                       MonadTrans (..),
                                                       StateT (..), modify)
import           Data.Hashable                        (hash)
import           Data.Int                             (Int32)
import           Data.List                            (isPrefixOf)
import           Data.Maybe                           (fromMaybe)

import           Sound.OSC
import           Sound.SC3                            hiding (withSC3)
import           Sound.SC3.ID                         hiding (hash, withSC3)
import           Sound.SC3.Supply
import           Sound.SC3.TH.Synthdef                (synthdefGenerator)
import           Sound.SC3.Tree

import           Sound.SC3.Orphan ()

-- --------------------------------------------------------------------------
--
-- * Synthdefs
--
-- --------------------------------------------------------------------------

-- | Synth to synchronize other synths, mainly demand UGens.
synth_metro :: UGen
synth_metro = mrg [out (control KR "count" 0) pcnt
                  ,out (control KR "out" 0) osig]
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

-- --------------------------------------------------------------------------
--
-- * Reserved values
--
-- --------------------------------------------------------------------------

-- | Reserved node id for master output.
masterNid :: Num a => a
masterNid = 99

-- | Reserved node id for default target node to add new nodes.
defaultTargetNid :: Num a => a
defaultTargetNid = 10

-- | Get node id of 'TUI01.synth_router' synth node. The group containing router
-- node must be added with 'addTrack'
routerNid ::
    Int -- ^ Node ID of group.
    -> Int
routerNid gid = ((gid + 1) * 100) - 1

-- | Reserved control output bus number for output of beat count.
countOut :: Num a => a
countOut = 127

-- | Reserved control output bus number for output of metro trigger.
metroOut :: Num a => a
metroOut = 128

-- | Reserved control outbus to get number of used control out bus.
controlBusCounter :: Num a => a
controlBusCounter = 126

-- | Get audio rate bus for given group.
audioBus :: Int -> Int
audioBus gid
    | gid == routerNid masterNid = sourceOut 1 -- any number except masterNid.
    | gid == masterNid           = sourceOut 1 -- again, for effect.
    | otherwise                  = 16 + (gid-100) * 2

-- | Get audio rate bus for given group.
sourceOut :: Num a => Int -> a
sourceOut gid
    | gid == masterNid = 0
    | otherwise        = 16

-- --------------------------------------------------------------------------
--
-- * Track
--
-- --------------------------------------------------------------------------

-- | Wrapper newtype to hold 'TrackState'.
newtype Track m a = Track {unTrack :: StateT TrackState m a}
    deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO
             , MonadState TrackState )

instance SendOSC m => SendOSC (Track m) where
    sendOSC = lift . sendOSC

instance RecvOSC m => RecvOSC (Track m) where
    recvPacket = lift recvPacket

instance DuplexOSC m => DuplexOSC (Track m)

instance Transport m => Transport (Track m)

-- | State for single track.
data TrackState = TrackState
    { tsGroupId       :: !Int
    , tsBeatOffset    :: Int
    , tsRootNode      :: SCNode
    , tsCurrentNode   :: SCNode
    , tsTargetNodes   :: [SCNode]
    , tsControlBusNum :: Int
    , tsBeatCount     :: Float
    , tsSourceNB      :: DList SCNode
    , tsMessages      :: DList Message
    }

-- | Show information in 'TrackState'.
showTrackState :: TrackState -> String
showTrackState ts =
  unlines
    ["----------- TrackState ------------"
    ,unwords ["tsGroupId:", show (tsGroupId ts)]
    ,unwords ["tsBeatOffset:", show (tsBeatOffset ts)]
    ,unlines ["tsCurrentNode:", drawSCNode (tsCurrentNode ts)]
    ,unlines ["tsTargetNodes:", unlines (map drawSCNode (tsTargetNodes ts))]
    ,unwords ["tsControlBusNum:", show (tsControlBusNum ts)]
    ,unwords ["tsBeatCount:", show (tsBeatCount ts)]
    ,unlines ["tsSourceNB:", unlines (map drawSCNode (tsSourceNB ts []))]
    ,unwords ["tsMessages:", unlines (map messagePP (tsMessages ts []))]
    ]

-- | Synonym for simple difference list.
type DList a = [a] -> [a]

-- | Add single element to end of 'DList'.
snoc :: a -> DList a -> DList a
-- snoc x f = f . (x:)
snoc x f = let x' = x `seq` (x:) in x' `seq` f . x'

-- | Add nodes and empty groups used by TUI02.
initializeTUI02 :: Transport m => m ()
initializeTUI02 = do
    sendOSC $ bundle immediately
        (c_set [(controlBusCounter,256)] : map d_recv ($(synthdefGenerator)))
    play $
        grp 0
        [ grp 1
          [ grp defaultTargetNid
            (grp (masterNid+1)
             [ syn "metro"
               ["out"*=metroOut, "count"*=countOut] ]
             : map
             (\gid ->
               grp gid
               [ syn' (routerNid gid) "router"
                 [ "in"  *= Dval (fromIntegral (audioBus gid))
                 , "out" *= sourceOut gid ]]) [101..108]
            )
          , grp masterNid
            [ syn' (routerNid masterNid) "router"
              ["in"*=Dval (fromIntegral (audioBus (routerNid masterNid))) ]]]
        , grp 2 [] ]

-- | Reset values in control buses 'controlBusCounter' and 'countOut', then
-- calls 'Sound.SC3.reset'.
resetTUI02Settings :: Transport m => m ()
resetTUI02Settings =
 do send (c_set [(controlBusCounter,256),(countOut,0)])
    reset

-- | Add and setup new group used by 'Track'.
addTrack :: Transport m => m Message
addTrack = do
    currentNodes <- getRootNode
    let gid = case queryN' (nodeId ==? defaultTargetNid) currentNodes of
            Nothing  -> 101
            Just g10 -> case g10 of
                Group _ ns | not (null ns) -> nodeId (last ns) + 1
                           | otherwise     -> 101
                _                          -> error "Node id 10 is not a group"
        obus = Dval $ fromIntegral $ audioBus gid
        g = grp gid
            [ syn' (routerNid gid) "router" ["in"*=obus, "out"*=sourceOut gid] ]
    addNode defaultTargetNid (nodify g)
    send (sync $ audioBus gid)
    waitMessage

-- | Do action with 'Track'.
runTrack ::
    Transport m
    => Int       -- ^ Group ID of this track.
    -> Track m a -- ^ Action to do.
    -> m a
runTrack groupId trck = do
    -- XXX: Update metro synthdef and use sendReply?
    -- If so, how to detect whether the count has exceeded the specified beat
    -- offset for UGens defined in 'sendCurve' etc?
    send (c_get [controlBusCounter,countOut])
    [_,Float currentBusNum,_,Float cnt] <- waitDatum "/c_set"
    rootNode <- getRootNode
    let node = fromMaybe (error ("No group: " ++ show groupId))
               (queryN' (nodeId ==? groupId) rootNode)
    (val,st) <- runStateT (unTrack trck)
                TrackState { tsGroupId     = groupId
                           , tsBeatOffset  = 0
                           , tsRootNode    = rootNode
                           , tsCurrentNode = node
                           , tsTargetNodes = []
                           , tsControlBusNum = truncate currentBusNum
                           , tsBeatCount = cnt
                           , tsSourceNB = id
                           , tsMessages = id
                           }
    let n0 = tidyUpParameters node
        n1 = Group groupId $ tsSourceNB st []
        ds = foldr
             (\m acc -> case m of
                   Message pat dtm
                       | pat == "/n_set"  -> acc
                       | pat == "/n_free" -> case dtm of
                           [] -> m : acc
                           _  -> foldr
                                 (\(Int32 nid) acc' ->
                                   let (n_frees,d_frees) = removeParams nid
                                   in  n_frees : d_frees : acc')
                                 (m:acc) dtm
                   _                      -> m : acc)
             [ c_set [(controlBusCounter, fromIntegral (tsControlBusNum st))]
             | currentBusNum /= fromIntegral (tsControlBusNum st)]
             (diffMessage n0 n1)
        removeParams nid =
            let paramNodes =
                    queryN ((("p:" ++ show nid) `isPrefixOf`) . synthName) node
            in  (n_free $ map nodeId paramNodes,
                 d_free $ map synthName paramNodes)

        ps = tsMessages st []
        ms = ds ++ ps
    unless (null ds)
        (liftIO $ putStrLn $ unlines $ map messagePP ds)
    unless (null ms) $
        sendOSC $ bundle immediately ms
    return val

-- | Filters out parameter nodes and mapped control parameter in source and
-- effect nodes.
tidyUpParameters :: SCNode -> SCNode
tidyUpParameters n0 =
    let f n acc = case n of
            Group i ns -> Group i (foldr f [] ns) : acc
            Synth i name ps
                | "p:" `isPrefixOf` name -> acc
                | name == "metro"        -> Synth i name ps : acc
                | otherwise              ->
                    let ps' = [p | p <- ps
                                 , paramName p == "out" || paramName p == "in"]
                    in  Synth i name ps' : acc
        {-# INLINE f #-}
    in  head $ foldr f [] [n0]

-- | Free nodes matching to given query string, in specified group.
freeNodes :: Transport m => String -> Track m ()
freeNodes = freeNodesCond . qString

-- | Free nodes matching to given condition, in specified group.
freeNodesCond :: Transport m => Condition SCNode -> Track m ()
freeNodesCond cond =
    send . n_free . map nodeId . queryN cond . tsCurrentNode =<< get

-- | Dump nodes in track.
dumpTrack :: Transport m => Track m ()
dumpTrack =
    get >>= getNode . tsGroupId >>= liftIO . putStrLn . drawSCNode

-- | Set offset to update the parameters.
offset :: Monad m => Int -> Track m ()
offset count = modify $ \st -> st {tsBeatOffset = count}


-- --------------------------------------------------------------------------
--
-- * Target synths
--
-- --------------------------------------------------------------------------

-- | 'TrackState' taking root node, currentBusNum, and beatCount.
initialTrackState :: SCNode -> Float -> Float -> TrackState
initialTrackState rootNode currentBusNum beatCount = TrackState
     { tsGroupId = 0
     , tsBeatOffset = 0
     , tsRootNode = rootNode
     , tsCurrentNode = Group 0 []
     , tsTargetNodes = []
     , tsControlBusNum = truncate currentBusNum
     , tsBeatCount = beatCount
     , tsSourceNB = id
     , tsMessages = id
     }

-- | Run settings of entire nodes from root node.
--
-- This function performs diff of given 'Track' with current root node.
--
runSettings :: Transport m => Track m a -> m a
runSettings trck = do
    send (c_get [controlBusCounter,countOut])
    [_,Float currentBusNum,_,Float beatCount] <- waitDatum "/c_set"
    rootNode <- getRootNode
    (val,st) <- runStateT (unTrack trck)
                (initialTrackState rootNode currentBusNum beatCount)
    let n0 = tidyUpParameters rootNode
        -- n1 = Group 0 (tsSourceNB st [])
        n1 = head (tsSourceNB st [])
        ds = foldr
             (\m acc -> case m of
                   Message pat dtm
                       | pat == "/g_new" &&
                         dtm == [Int32 0,Int32 0,Int32 0] -> acc
                       | pat == "/n_set"  -> acc
                       | pat == "/n_free" -> case dtm of
                           [] -> m : acc
                           _  -> foldr
                                 (\(Int32 nid) acc' ->
                                   let (n_frees,d_frees) = removeParams nid
                                   in  n_frees : d_frees : acc')
                                 (m:acc) dtm
                   _                      -> m : acc)
             [ c_set [(controlBusCounter, fromIntegral (tsControlBusNum st))]
             | currentBusNum /= fromIntegral (tsControlBusNum st)]
             (diffMessage n0 n1)
        removeParams nid =
            let paramNodes =
                    queryN ((("p:" ++ show nid) `isPrefixOf`) . synthName) rootNode
            in  (n_free $ map nodeId paramNodes,
                 d_free $ map synthName paramNodes)
        ps = tsMessages st []
        ms = ds ++ ps
    -- liftIO
    --   (putStr
    --     (unlines
    --        ["size of n0: " ++ show (sizeSCNode n0)
    --        ,prettyDump n0
    --        ,"size of n1: " ++ show (sizeSCNode n1)
    --        ,prettyDump n1]))
    unless (null ds)
        (liftIO $ putStrLn $ unlines $ map messagePP ds)
    unless (null ms) $
        sendOSC $ bundle immediately ms
    return val

runSourceBuilder :: Track m a -> m (a, TrackState)
runSourceBuilder t = runStateT (unTrack t) (initialTrackState (Group 0 []) 0 0)

-- XXX: TODO.
defaultSettings :: Transport m => Track m () -> Track m ()
defaultSettings body =
    track 0 $ do
        track 1 $
            track defaultTargetNid $ do
                track (masterNid+1) $
                    source "metro" $ do
                        param "out" (Dval metroOut)
                        param "count" (Dval countOut)
                body
                track masterNid $
                    -- source "router" $
                    router $
                        param "in" (constant (audioBus (routerNid masterNid)))
        track 2 $ return ()

-- | Add track with given group id and child nodes.
track :: Transport m => Int -> Track m a -> Track m a
track gid act = do
    st0 <- get
    let currentNode = fromMaybe (Group gid [])
                        (queryN' (nodeId ==? gid) (tsRootNode st0))
    (val,st1) <- lift (runStateT (unTrack act)
                        (st0 { tsSourceNB = id
                             , tsGroupId = gid
                             , tsTargetNodes = []
                             , tsCurrentNode = currentNode
                             }))
    put (st1 { tsSourceNB =
                 Group gid (tsSourceNB st1 []) `snoc` tsSourceNB st0
             , tsTargetNodes = []
             })
    return val

-- | Apply parameter actions to nodes specified by query string.
source ::
    Monad m
    => String     -- ^ Synthdef name.
    -> Track m a  -- ^ Action for parameter.
    -> Track m a
source = genControlledNode (\obus -> ["out":=obus]) Nothing

source' :: Monad m => Int -> String -> Track m a -> Track m a
source' i = genControlledNode (\obus -> ["out":=obus]) (Just i)

-- | Add given 'SCNode' to track.
rawNode :: Monad m => SCNode -> Track m ()
rawNode node =
  modify (\st -> st {tsSourceNB = node `snoc` tsSourceNB st})

-- | Add given 'Message' to track.
rawMessage :: Monad m => Message -> Track m ()
rawMessage msg = modify (\st -> st {tsMessages = msg `snoc` tsMessages st})

bypass :: Monad m => String -> Track m a -> Track m a
bypass = genControlledNode (\_ -> []) Nothing

-- | Source node for ticking synchronized trigger with 'impulse'.
metroNode :: Transport m => Double -> Double -> Track m ()
metroNode bpm beat =
  do st <- get
     let nid = fromIntegral
                 (abs (fromIntegral
                         (joinID (tsGroupId st) (hash name)) :: Int32))
         name = "metro"
     rawNode (Synth nid name ["out" := metroOut
                             ,"count" := countOut
                             ,"bpm" := bpm
                             ,"beat" := beat])

-- | Run effect synth, apply paremater actions to nodes.
effect :: Monad m => String -> Track m a -> Track m a
effect = genControlledNode (\obus -> ["out":=obus,"in":=obus]) Nothing

effect' :: Monad m => Int -> String -> Track m a -> Track m a
effect' i = genControlledNode (\obus -> ["out":=obus,"in":=obus]) (Just i)

-- | Make an 'Track' action for source and effect nodes.
genControlledNode ::
    Monad m
    => (Double -> [SynthParam])
    -- ^ Function taking output bus for this synthdef.
    -> Maybe Int -- ^ Value used for 'hash'.
    -> String    -- ^ Synthdef name.
    -> Track m a -- ^ Next action.
    -> Track m a
genControlledNode fparam mbi name act = do
    modify $ \st ->
        let gid = tsGroupId st
            nid = fromIntegral
                  (abs (fromIntegral
                        (case mbi of
                              Nothing -> joinID gid (hash name)
                              Just i  -> joinID i (joinID gid (hash name)))
                        :: Int32))
            obus = fromIntegral $ audioBus gid
            node = Synth nid name (fparam obus)
        in  st { tsTargetNodes =
                      case queryN' (nodeId ==? nid) (tsCurrentNode st) of
                          Nothing -> [node]
                          Just n  -> [n]
               , tsSourceNB = node `snoc` tsSourceNB st
               }
    act

-- | Action for 'synth_router'.
router :: Monad m => Track m a -> Track m a
router act = do
    modify $ \st ->
        let gid  = tsGroupId st
            ibus = fromIntegral (audioBus gid)
            obus = sourceOut gid
            nid  = routerNid gid
            node = Synth nid "router" ["out":=obus,"in":=ibus]
        in  st { -- tsTargetNodes = queryN (synthName ==? "router") (tsCurrentNode st)
                 tsTargetNodes =
                    case queryN' (nodeId ==? nid) (tsCurrentNode st) of
                      Nothing -> [node]
                      Just n  -> [n]
               , tsSourceNB = node `snoc` tsSourceNB st
               }
    act

-- | Translate to condition:
--
-- * \'*\' - entire node
--
-- * #ID   - synth with node id /ID/.
--
-- Otherwise, synth with given synthdef name.
--
qString :: String -> Condition SCNode
qString str0 = foldr1 (||?) (map parse (words str0))
  where
    parse str =
        case str of
            '*':_    -> const True
            '#':nid  -> nodeId    ==? read nid
            '.':name -> synthName ==? name
            name     -> synthName ==? name


-- --------------------------------------------------------------------------
--
-- * Assignable
--
-- --------------------------------------------------------------------------

class Assignable a where
    assign ::
        Transport m
        => SCNode     -- ^ Target node to assign value.
        -> String     -- ^ Parameter name.
        -> a          -- ^ Value to assign.
        -> Track m () -- ^ New output bus, could be same as given value.

-- | Assign parameter with given value.
param :: (Assignable a, Transport m) => String -> a -> Track m ()
param name value = do
    st <- get
    mapM_ (\nd -> assign nd name value) . tsTargetNodes $ st

-- | Operator variant of 'param'.
(==>) ::
    (Transport m, Assignable a)
    => String  -- ^ Parameter name.
    -> a       -- ^ Value.
    -> Track m ()
name ==> value = param name value

infixr 1 ==>

instance Assignable Double where
    assign nd name val = sendControl nd name (const (constant val))

instance Assignable PrmVal where
    assign nd name val = modify (\st -> st {tsMessages = msg (tsMessages st)})
      where
        msg = case val of
                Dval v        -> snoc ((n_set (nodeId nd) [(name,v)]))
                Cbus (Ival v) -> snoc ((n_map (nodeId nd) [(name,v)]))
                Abus (Ival v) -> snoc ((n_mapa (nodeId nd) [(name,v)]))
                _      -> id

instance Assignable UGen where
    assign nd name val = sendControl nd name (const val)

instance Assignable (UGen -> UGen) where
    assign = sendControl

-- | Data to control synth parameter with 'Supply'.
newtype Sustain a = Sustain {unSustain :: a}
     deriving (Eq, Show, Ord, Num, Real, Fractional)

-- | Wraps given 'Supply' to synth parameter.
sustain :: a -> Sustain a
sustain = Sustain

instance Assignable (Sustain UGen) where
    assign nd name (Sustain ug) = sendControl nd name (const ug)

instance Assignable (Sustain Supply) where
    assign nd name (Sustain val) = do
        let ug tr = demand tr 1 (supply 0x12345678 val)
        sendControl nd name ug

-- | Wrapper for triggered pattern.
newtype Trigger a = Trigger {unTrigger :: a}
     deriving (Eq, Show, Ord, Num, Real, Fractional)

-- | Wraps 'Trigger'.
trigger :: a -> Trigger a
trigger = Trigger

instance Assignable (Trigger Supply) where
    assign nd name (Trigger val) = do
        let ug tr = tr * demand tr 1 (supply 0x12345678 val)
        sendControl nd name ug

-- | Data type for assininng curved change.
data CurveTo a = CurveTo
   { ctCurve :: Envelope_Curve a -- ^ Curve shape.
   , ctDur   :: Double           -- ^ Curve duration.
   , ctValue :: Double           -- ^ Curve target value.
   } deriving (Eq, Show)

instance Assignable (CurveTo UGen) where
    assign = sendCurve

-- | Assign curved value to parameter
curveTo :: EnvCurve -> Double -> Double -> CurveTo UGen
curveTo = CurveTo

-- | Send curve to running scsynth.
--
-- XXX: Currently this function will not work when 'tsBeatOffset' is 0.
--
sendCurve ::
    Transport m
    => SCNode -- ^ Target node.
    -> String -- ^ Parameter name.
    -> CurveTo UGen -- ^ Curve shape.
    -> Track m ()
sendCurve node0 pname ct = sendParamUGen node0 pname $ \node _tr ->
    let crv     = ctCurve ct
        toVal   = ctValue ct
        dur     = ctDur ct
        matched = in' 1 KR countOut ==* (control KR "count" 0 + 1)
        osig    = envGen KR matched 1 0 (constant dur) DoNothing
                  (Envelope
                   [fromVal,constant toVal] [1] [crv] Nothing Nothing)
        fromVal = case node of
            Synth _ _ ps -> case [p | p <- ps, paramName p == pname] of
                (_ :=  val):_ -> constant val
                (_ :<- bus):_ ->
                    latch (in' 1 KR (fromIntegral bus))
                    (in' 1 KR countOut Sound.SC3.ID.<* control KR "count" 0)
                _             -> constant (0::Double)
            _            -> constant (0::Double)
    in  osig

-- | Data type for getting input from running node.
data Input = Input Int (Condition SCNode) (Condition SynthParam)
             (UGen -> UGen -> UGen)

-- | Route input signal from other node.
input ::
    Int -- ^ Group node ID to query.
    -> Condition SCNode -- ^ Condition to query synth.
    -> Condition SynthParam -- ^ Condition to query param.
    -> (UGen->UGen->UGen) -- ^ Function applied to input.
    -> Input
input = Input

-- | Get input signal from other node.
getInput :: Transport m => Int -> Condition SCNode -> String -> Track m UGen
getInput gid ncond pname = do
    st <- get
    let tmpNode = tsRootNode st
        gnode   = queryN' (nodeId ==? gid) tmpNode
        snode   = queryN' ncond =<< gnode
        mbprm   = queryP' (paramName ==? pname) =<< snode
    case mbprm of
        Just (_ :<- bus) -> return (in' 1 KR (fromIntegral bus))
        _                -> return 0

instance Assignable Input where
    assign node pname (Input gid scond pcond fu) = do
        st <- get
        -- Building node from `tsSourceNB st []' will not work since
        -- parameter names and mapped bus numbers are not tracked.
        -- Currently using SCNode built from `/g_queryTree.reply' message.
        let tmpNode = tsRootNode st
            gnode   = queryN' (nodeId ==? gid) tmpNode
            snode   = queryN' scond =<< gnode
            mbprm   = queryP' pcond =<< snode
        case mbprm of
            Just (_ :<- bus) ->
                let bus' = fromIntegral bus
                in  sendControl node pname (\tr -> fu tr (in' 1 KR bus'))
            _                -> return ()


-- | Send UGen and map control output to synth specified by given 'SCNode'.
sendControl ::
    Transport m
    => SCNode
    -> String
    -> (UGen -> UGen)
    -> Track m ()
sendControl node pname fu = sendParamUGen node pname $ \_ tr -> fu tr

-- | Send UGen used for controlling parameter.
sendParamUGen ::
    Transport m
    => SCNode
    -> String
    -> (SCNode -> UGen -> UGen)
    -> Track m ()
sendParamUGen node pname fp = do
    ts <- get
    -- Freeing existing synths with new synthdef via 'free' ugen.
    -- Node IDs of the old synthdefs is known at this point.
    let beatOffset  = tsBeatOffset ts
        currentObus = tsControlBusNum ts
        nid    = nodeId node
        cnt'   = nextOffset beatOffset (tsBeatCount ts)
        tr     = gate (control KR "tr" 0) exceed
        exceed = in' 1 KR countOut >* control KR "count" 0
        fr     = free exceed $ mce $ map (constant . nodeId) psynths
        osig   = out (control KR "out" 0) (gate (fp node tr) exceed)
        sdef | psynthExist = mrg [osig, fr]
             | otherwise   = osig
        -- pdefname = concat ["p:",show nid,":",pname,":",show (hashUGen osig)]
        pdefname = concat ["p:",show nid,":",pname,":",show (hash osig)]
        (reusing,obus) = case queryP' (paramName ==? pname) node of
            Just (_ :<- v) -> (True,v)
            _              -> (False,currentObus)
        psynths = queryN (paramCondition obus) (tsCurrentNode ts)
        psynthExist   = case psynths of
            [] -> False
            _  -> True
        psynthNames   = map synthName psynths
        psynthChanged = pdefname `notElem` psynthNames

        newMessage =
            withCM
            (d_recv $ synthdef pdefname sdef)
            (bundle immediately
             [s_new pdefname (-1) AddBefore nid
              [("out",fromIntegral obus),("count",fromIntegral cnt')]
             , n_map (-1) [("tr",metroOut)]
             , n_map nid [(pname, obus)]
             , d_free (filter (/= pdefname) psynthNames) ])

    when psynthChanged $
        liftIO $ dumpParamChange node pname pdefname

    modify $ \st ->
        st { tsControlBusNum = if reusing then currentObus else currentObus+1
           , tsMessages = if psynthChanged
                          then newMessage `snoc` tsMessages st
                          else tsMessages st
           }

-- | Get next count for let trigger signal to path.
nextOffset ::
    Int      -- ^ Beat offset.
    -> Float -- ^ Current count.
    -> Int
nextOffset beatOffset currentCount
    | beatOffset <= 0 = truncate currentCount
    | otherwise       =
        let count = ceiling (currentCount/fromIntegral beatOffset) + 1
        in  count * beatOffset

-- | Condition to filter out parameter node using given output bus number.
--
-- Conditions for synth controlling specified parameter are:
--
-- * Parameter is mapped to control rate out bus, and:
--
-- * The out bus is mapped to parameter name of target synth.
--
paramCondition :: Int -> Condition SCNode
paramCondition bus = params $ \p -> case p of
    n := v | n == "out" && v == fromIntegral bus -> True
    _                                            -> False

-- | Dump information of changed parameter synthdef.
dumpParamChange ::
    SCNode     -- ^ Node containing parameter.
    -> String  -- ^ Parameter name.
    -> String  -- ^ Parameter synthdef name.
    -> IO ()
dumpParamChange node pname pdefName =
    putStrLn $ unwords
        [ synthName node ++ " (" ++ show (nodeId node) ++ "):"
        , "\"" ++ pname ++ "\"", " -- ", pdefName ]
