{-|
Copyright    : 8c6794b6, 2014
License      : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Extra elements for threepenny-gui. See @data/static/ui.css@ for CSS values.

-}
module Graphics.UI.Threepenny.Extra
 ( -- * Element
   textbox
 , hrange
 , vrange
 , xyarea
 , statusDiv
   -- * Event
 , change
 , input
   -- * IO
 , getStaticDir
 ) where

import           Control.Monad (replicateM, void, when)
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.Maybe (catMaybes)
import           System.FilePath ((</>))
import           Text.Printf (printf)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Sound.OSC.FD
import           Sound.SC3.FD (serverStatusData)

import           Paths_study_for_user_interfaces (getDataDir)


-- --------------------------------------------------------------------------
--
-- * Elements
--
-- --------------------------------------------------------------------------

-- | Simple text box with @<input type="text">@.
textbox ::
    String               -- ^ Label name.
    -> Int               -- ^ Width, in pixel.
    -> String            -- ^ Initial value.
    -> (String -> UI ()) -- ^ Action to take with input value.
    -> UI Element
textbox name width iniv fv = do
    label <- UI.div
             #. "textbox-label"
             # set text name
    inval <- UI.input
             #. "textbox-inval"
             # set UI.type_ "text"
             # set value iniv
             # set style [("width",show width ++ "px")]
    on change inval $ \_ -> fv =<< get value inval
    UI.div #. "textbox-wrapper"
        #+ [element label, element inval]

-- | Horizontal slider with @<input type="range">@.
hrange ::
    String    -- ^ Label to show.
    -> Double -- ^ Minimum value.
    -> Double -- ^ Maximum value.
    -> Double -- ^ Step value.
    -> Double -- ^ Initial value.
    -> (Double -> UI String)
    -- ^ Action to be donw with paramter. Returned value will be shown as
    -- parameter's label.
    -> UI Element
hrange name minv maxv stepv iniv act = do
    cnam <- UI.div
            #. "hrange-label"
            # set text name
    clab <- UI.div
            #. "hrange-label"
            # set text (show iniv)
    cval <- UI.input
            #. "hrange-input"
            # set UI.type_ "range"
            # set (attr "min") (show minv)
            # set (attr "max") (show maxv)
            # set (attr "step") (show stepv)
            # set value (show iniv)
            # set style [("width","128px")]
    on change cval $ \_v -> do
        v <- get value cval
        v' <- act (read v)
        element clab # set text v'
    UI.div
        #. "hrange-wrapper"
        #+ [element cnam, element clab, UI.div, element cval]


-- | Vertical slider with @<input type="range">@.
vrange ::
    String    -- ^ Label to show.
    -> Double -- ^ Minimum value.
    -> Double -- ^ Maximum value.
    -> Double -- ^ Step value.
    -> Double -- ^ Initial value.
    -> (Double -> UI String)
    -- ^ Action to be done with parameter. Returned value will be shown as
    -- parameter's label.
    -> UI Element
vrange name minv maxv stepv iniv act = do
    cval <- UI.input
            #. "vrange-input"
            # set UI.type_ "range"
            # set (attr "min") (show minv)
            # set (attr "max") (show maxv)
            # set (attr "step") (show stepv)
            # set value (show iniv)
            # set style [("height","119px")]
    cnam <- UI.span
            #. "vrange-label"
            # set text name
            #+ [UI.br]
    clab <- UI.span
            # set text (show iniv)
            #. "vrange-label"
    on input cval $ \_v -> do
        v' <- get value cval
        v'' <- act (read v')
        element clab # set text v''
    cclear <- UI.div
    UI.div
        #. "vrange-wrapper"
        # set style [("padding","10px 2px")]
        #+ map element [cnam, cval, cclear, clab]

-- | Returns a div element with controllable XY coordinates.
xyarea ::
    String -- ^ Label string to show.
    -> Int -- ^ Size of area.
    -> ((Int,Int) -> UI String)
    -- ^ Action to be done with (x,y) coordinates. Returned values will be shown
    -- as label.
    -> UI Element
xyarea name size fxy = do
    let sizePx = show size ++ "px"
    label <- UI.div #. "xyarea-label" # set text name
    param <- UI.div #. "xyarea-param" # set text ""
    xline <- UI.div #. "xyarea-line" #
             set style [("height",sizePx),("width","1px")]
    yline <- UI.div #. "xyarea-line"
             # set style [("height","1px"),("width",sizePx)]
    area <- UI.div #. "xyarea-control"
            # set style [("width",sizePx),("height",sizePx)]
            # set (attr "tabindex") "1"
            #+ [element xline, element yline]

    let setxy (x,y) = do
            void $ element xline # set style [("left", show x ++ "px")]
            void $ element yline # set style [("bottom", show (size-y) ++ "px")]
            vlabel <- fxy (x,y)
            void $ element param # set text vlabel

    activeRef <- liftIO $ newIORef False
    on UI.mousemove area $ \xy -> do
        active <- liftIO $ readIORef activeRef
        when active $ setxy xy
    on UI.mousedown area $ \xy -> liftIO (writeIORef activeRef True) >> setxy xy
    on UI.mouseup area $ \_ -> liftIO $ writeIORef activeRef False

    wrapper <- UI.div
        #. "xyarea-wrapper"
        #+ [element label, element area, element param]

    return wrapper

-- | Returns timer and div to show scsynth server status.
statusDiv ::
    Transport t
    => t -- ^ Transport of scsynth.
    -> UI (UI.Timer, Element)
statusDiv fd = do
    timer <- UI.timer # set UI.interval 500
    ds@[stCPUp,stCPUa,stUGens,stSynths,stGroups,stInstruments] <-
        replicateM 6 $ UI.div #. "status-param"
    onEvent (UI.tick timer) $ \_ -> do
        st <- liftIO $ getStatus fd
        void $ element stCPUp # set text (printf "%2.2f%%" (ssCPUPeak st))
        void $ element stCPUa # set text (printf "%2.2f%%" (ssCPUAverage st))
        void $ element stUGens # set text (show (ssUGens st) ++ "u")
        void $ element stSynths # set text (show (ssSynths st) ++ "s")
        void $ element stGroups # set text (show (ssGroups st) ++ "g")
        void $ element stInstruments # set text (show (ssInstruments st) ++ "d")
    stext <- UI.div
             #. "status-text"
             # set text "scsynth"
    stWrapper <- UI.div
             #. "status"
             # set UI.children (stext:ds)
    return (timer, stWrapper)

-- | Data type to hold scsynth server status.
data ServerStatus = ServerStatus
    { ssUGens             :: Int    -- ^ Number of UGens.
    , ssSynths            :: Int    -- ^ Number of Synths.
    , ssGroups            :: Int    -- ^ Number of Groups.
    , ssInstruments       :: Int    -- ^ Number of Instruments.
    , ssCPUAverage        :: Float  -- ^ CPU average.
    , ssCPUPeak           :: Float  -- ^ CPU peak.
    , ssSampleRateNominal :: Double -- ^ Sample rate, nominal.
    , ssSampleRateActual  :: Double -- ^ Sample rate, actual.
    } deriving (Eq, Show)

-- | Get status information from given transport.
getStatus :: Transport t => t -> IO ServerStatus
getStatus fd = do
    Int32 _:Int32 i1:Int32 i2:Int32 i3:Int32 i4:
        Float f0:Float f1:Double d0:Double d1:[] <- serverStatusData fd
    return $ ServerStatus
        { ssUGens             = fromIntegral i1
        , ssSynths            = fromIntegral i2
        , ssGroups            = fromIntegral i3
        , ssInstruments       = fromIntegral i4
        , ssCPUAverage        = f0
        , ssCPUPeak           = f1
        , ssSampleRateNominal = d0
        , ssSampleRateActual  = d1
        }


-- --------------------------------------------------------------------------
--
-- * Events
--
-- --------------------------------------------------------------------------

-- | Represent @oninpu@ dom event.
input :: Element -> Event String
input = fmap eventDataTexts . domEvent "input"

-- | Represent @onchange@ dom event.
change :: Element -> Event String
change = fmap eventDataTexts . domEvent "change"

-- | Concatenate strings in 'EventData'.
eventDataTexts :: EventData -> String
eventDataTexts (EventData ms) = concat (catMaybes ms)


-- --------------------------------------------------------------------------
--
-- * IO
--
-- --------------------------------------------------------------------------

-- | Get static directory using @getDataDir@ from /Paths_*.hs/ module generated
-- by cabal.
--
-- The function @getDataDir@ understands environment variable
-- /study_for_user_interfaces_datadir/.
--
getStaticDir :: IO FilePath
getStaticDir = fmap (</> "static") getDataDir
