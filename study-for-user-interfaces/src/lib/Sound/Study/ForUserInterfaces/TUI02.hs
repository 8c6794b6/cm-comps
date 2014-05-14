{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Textual user interface, take 2. TUI02 has a concept of 'Track', which aligns
given source synths and effect synths with parameter controlling adhoc
UGens. Source and effect synth setup is idemopotent, initialization and update
of track could be done with invoking same code block in Haskell code.

Known limitation:

* Ordering nodes may not work properly. Might need couple updated in diff
related functions in hsc3-tree package.

-}
module Sound.Study.ForUserInterfaces.TUI02 where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, when)
import Control.Monad.State (MonadTrans(..), MonadState(..), StateT(..), modify)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int32)
import Data.List (isPrefixOf)
import System.Random (mkStdGen)

import Sound.OSC
    ( Datum(..), DuplexOSC, RecvOSC(..), Message(..), SendOSC(..)
    , Transport, bundle, immediately, messagePP, waitDatum, waitMessage
    )
import Sound.SC3.ID
import Sound.SC3.Supply (Supply, evalSupply)
import Sound.SC3.TH.Synthdef (synthdefGenerator)
import Sound.SC3.Tree

import Sound.Study.ForUserInterfaces.TUI01 as TUI01
    ( audioBus, defaultTargetNid, sourceOut, masterNid, countOut, metroOut
    , tui01Synthdefs )

-- --------------------------------------------------------------------------
--
-- * Constant values
--
-- --------------------------------------------------------------------------

-- | Get node id of 'TUI01.synth_router' synth node. The group containing router
-- node must be added with 'addTrack'
routerNid ::
    Int -- ^ ID of Group node.
    -> Int
routerNid gid = ((gid + 1) * 100) - 1

-- | Reserved control outbus to get number of used control out bus.
controlBusCounter :: Num a => a
controlBusCounter = 126


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
    { tsGroupId       :: Int
    , tsBeatOffset    :: Int
    , tsCurrentNode   :: SCNode
    , tsTargetNodes   :: [SCNode]
    , tsControlBusNum :: Int
    , tsBeatCount     :: Float
    , tsSourceNB      :: DList SCNode
    , tsMessages      :: DList Message
    }

-- | Synonym for simple difference list.
type DList a = [a] -> [a]

-- | Add single element to end of 'DList'.
snoc :: a -> DList a -> DList a
snoc x f = f . (x:)
{-# INLINE snoc #-}

-- | Add and setup new group used by 'Track'.
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
            [ syn' (routerNid gid) "router" ["in"*=obus, "out"*=sourceOut] ]
    addNode defaultTargetNid (nodify g)
    send (sync $ audioBus gid)
    waitMessage

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
            [ syn' (routerNid 99) "router" ["in"*=sourceOut] ]]
        , grp 2 [] ]
    send $ c_set [(controlBusCounter,256)]
    send $ sync (-1)
    _ <- waitMessage
    sequence_ $ replicate 8 $ addTrack

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

    node <- getNode groupId
    (val,st) <- runStateT (unTrack trck)
                TrackState { tsGroupId = groupId
                           , tsBeatOffset = 0
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
                           [Int32 nid] -> m : removeParams nid : acc
                           _           -> m : acc
                   _                      -> m : acc)
             (if currentBusNum /= fromIntegral (tsControlBusNum st)
                then [c_set
                      [(controlBusCounter,fromIntegral (tsControlBusNum st))]]
                else [])
             (diffMessage n0 n1)
        removeParams nid =
            -- XXX: Send d_free for parameter synthdef.
            n_free $ map nodeId $
            queryN ((("p:" ++ show nid) `isPrefixOf`) . synthName) node
        ps = tsMessages st []
        ms = ds ++ ps

    liftIO $ do
        -- putStrLn "=== NODE RUNNING (n0) =============="
        -- putStrLn $ drawSCNode n0
        -- putStrLn ""
        -- putStrLn "=== NODE IN SOURCE TEXT (n1) ======="
        -- putStrLn $ drawSCNode n1
        -- putStrLn ""

        case ds of
            [] -> return ()
            _  -> putStrLn $ unlines $ map messagePP ds

    when (not (null ms)) $
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
                | otherwise              ->
                    let ps' = [p | p <-ps
                                 , paramName p == "out" || paramName p == "in"]
                    in  Synth i name ps' : acc
        {-# INLINE f #-}
    in  head $ foldr f [] [n0]
{-# INLINEABLE tidyUpParameters #-}

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
{-# INLINEABLE offset #-}


-- --------------------------------------------------------------------------
--
-- * Target synths
--
-- --------------------------------------------------------------------------

-- | Apply parameter actions to nodes specified by query string.
source ::
    Monad m
    => String     -- ^ Query string, see 'qString'.
    -> Track m a  -- ^ Action for parameter.
    -> Track m a
source = genControlledNode (\obus -> ["out":=obus]) Nothing
{-# INLINEABLE source #-}

source' :: Monad m => Int -> String -> Track m a -> Track m a
source' i name act =
    genControlledNode (\obus -> ["out":=obus]) (Just i) name act
{-# INLINEABLE source' #-}

-- | Run effect synth, apply paremater actions to nodes.
effect :: Monad m => String -> Track m a -> Track m a
effect = genControlledNode (\obus -> ["out":=obus,"in":=obus]) Nothing
{-# INLINEABLE effect #-}

effect' :: Monad m => Int -> String -> Track m a -> Track m a
effect' i name act =
    genControlledNode (\obus -> ["out":=obus,"in":=obus]) (Just i) name act
{-# INLINEABLE effect' #-}

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
                      case queryN (nodeId ==? nid) (tsCurrentNode st) of
                          [] -> [node] -- initial case.
                          ns -> ns
               , tsSourceNB = node `snoc` (tsSourceNB st)
               }
    act
{-# INLINEABLE genControlledNode #-}

-- | Action for 'synth_router'.
router :: Monad m => Track m a -> Track m a
router act = do
    modify $ \st ->
        let ibus | tsGroupId st == 99 = 16
                 | otherwise          = fromIntegral $ audioBus $ tsGroupId st
            obus | tsGroupId st == 99 = 0
                 | otherwise          = sourceOut
            nid  = routerNid $ tsGroupId st
            node = Synth nid "router" ["out":=obus,"in":=ibus]
        in  st { tsTargetNodes = queryN (qString "router") (tsCurrentNode st)
               , tsSourceNB = node `snoc` tsSourceNB st
               }
    act
{-# INLINEABLE router #-}

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
{-# INLINEABLE qString #-}


-- --------------------------------------------------------------------------
--
-- * Control parameter
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
{-# INLINEABLE param #-}

-- | Operator variant of 'param'.
(==>) ::
    (Transport m, Assignable a)
    => String  -- ^ Parameter name.
    -> a       -- ^ Value.
    -> Track m ()
name ==> value = param name value
{-# INLINEABLE (==>) #-}

infixr 1 ==>

instance Assignable Double where
    assign nd name val = sendControl nd name (const (constant val))
    {-# INLINEABLE assign #-}

instance Assignable UGen where
    assign nd name val = sendControl nd name (const val)
    {-# INLINEABLE assign #-}

instance Assignable (UGen -> UGen) where
    assign nd name f = sendControl nd name f
    {-# INLINEABLE assign #-}

-- | Data to control synth parameter with 'Supply'.
newtype Sustain a = Sustain {unSustain :: a}
     deriving (Eq, Show, Ord, Num, Real, Fractional)

-- | Wraps given 'Supply' to synth parameter.
sustain :: a -> Sustain a
sustain = Sustain

instance Assignable (Sustain Double) where
    assign nd name (Sustain val) = do
        sendControl nd name (\_ -> constant val)
    {-# INLINEABLE assign #-}

instance Assignable (Sustain Supply) where
    assign nd name (Sustain val) = do
        let ug tr = demand tr 1 (evalSupply val (mkStdGen 0x12345678))
        sendControl nd name ug
    {-# INLINEABLE assign #-}

-- | Wrapper for triggered pattern.
newtype Trigger a = Trigger {unTrigger :: a}
     deriving (Eq, Show, Ord, Num, Real, Fractional)

-- | Wraps 'Trigger'.
trigger :: a -> Trigger a
trigger = Trigger

instance Assignable (Trigger Supply) where
    assign nd name (Trigger val) = do
        let ug tr = tr * demand tr 1 (evalSupply val (mkStdGen 0x12345678))
        sendControl nd name ug
    {-# INLINEABLE assign #-}

-- | Data type for assininng curved change.
data CurveTo a = CurveTo
   { ctCurve :: Envelope_Curve a -- ^ Curve shape.
   , ctDur   :: Double           -- ^ Curve duration.
   , ctValue :: Double           -- ^ Curve target value.
   } deriving (Eq, Show)

instance Assignable (CurveTo UGen) where
    assign = sendCurve
    {-# INLINEABLE assign #-}

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
        osig      = envGen KR matched 1 0 (constant dur) DoNothing
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
{-# INLINEABLE sendCurve #-}

-- | Send UGen and map control output to synth specified by given 'SCNode'.
sendControl ::
    Transport m
    => SCNode
    -> String
    -> (UGen -> UGen)
    -> Track m ()
sendControl node pname fu = sendParamUGen node pname $ \_ tr -> fu tr
{-# INLINEABLE sendControl #-}

sendParamUGen ::
    Transport m
    => SCNode
    -> String
    -> (SCNode -> UGen -> UGen)
    -> Track m ()
sendParamUGen node pname fp = do
    -- Free existing synths with new synthdef with 'free' ugen.
    -- Node id of old synthdef is known at this point.
    --
    ts <- get
    let cnt         = tsBeatCount ts
        beatOffset  = tsBeatOffset ts
        currentObus = tsControlBusNum ts
        nid    = nodeId node
        cnt'   = nextOffset beatOffset cnt
        tr     = gate (control KR "tr" 0) exceed
        exceed = in' 1 KR countOut >* control KR "count" 0
        fr     = free exceed $ mce $ map (constant . nodeId) psynths
        osig   = out (control KR "out" 0) (gate (fp node tr) exceed)
        sdef | psynthExist = mrg [osig, fr]
             | otherwise   = osig
        pdefname = concat ["p:",show nid,":",pname,":",show (hashUGen osig)]
        (reusing,obus) = case queryP' (paramName ==? pname) node of
            Just (_ :<- v) -> (True,v)
            _              -> (False,currentObus)
        psynths = queryN (paramCondition obus) (tsCurrentNode ts)
        psynthExist   = not (null psynths)
        psynthChanged = not (pdefname `elem` map synthName psynths)
        mfunc
            | psynthChanged =
                (withCM
                 (d_recv $ synthdef pdefname sdef)
                 (bundle immediately
                  [s_new pdefname (-1) AddBefore nid
                   [("out",fromIntegral obus),("count",fromIntegral cnt')]
                  , n_map (-1) [("tr",metroOut)]
                  , n_map nid [(pname, obus)] ]) :)
            | otherwise     = id

    when psynthChanged $ do
        liftIO $ dumpParamChange node pname pdefname

    modify $ \st ->
        st { tsControlBusNum = if reusing then currentObus else currentObus+1
           , tsMessages = tsMessages st . mfunc
           }
{-# INLINEABLE sendParamUGen #-}

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
-- Condition for synth controlling specified parameter is :
--
-- * Parameter is mapped to control rate out bus, and:
--
-- * The out bus is mapped to pname of snth.
--
paramCondition :: Int -> Condition SCNode
paramCondition bus = params $ \p -> case p of
    n := v | n == "out" && v == fromIntegral bus -> True
    _                                             -> False

-- | Dump information of changed parameter synthdef.
dumpParamChange ::
    SCNode     -- ^ Node containing parameter.
    -> String  -- ^ Parameter name.
    -> String  -- ^ Parameter synthdef name.
    -> IO ()
dumpParamChange node pname pdefName = do
    putStrLn $ unlines
        [ "Setting : " ++ "\"" ++ pname ++ "\" in " ++
          synthName node ++ " (" ++ show (nodeId node) ++ ")"
        , "Param   : " ++ pdefName
        ]

-- --------------------------------------------------------------------------
--
-- * Deprecated
--
-- --------------------------------------------------------------------------

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
