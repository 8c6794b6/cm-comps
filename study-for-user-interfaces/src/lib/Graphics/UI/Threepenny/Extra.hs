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
   addJavaScript
 , textbox
 , hrange
 , vrange
 , hslider
 , vslider
 , hcheckbox
 , toggleGrids
 , toggleBox
 , toggleSingleGrid
 , turnOnGrid
 , turnOffGrid
 , xyarea
 , knob
 , statusDiv
   -- * Event
 , change
 , input
   -- * IO
 , getStaticDir
 ) where

import           Control.Monad (forM, replicateM, void, when)
import           Data.Maybe (catMaybes)
import           Data.IORef (newIORef, readIORef, writeIORef)
import           System.FilePath ((</>))
import           Text.Printf (printf)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Sound.OSC.FD
import           Sound.SC3.FD (dumpOSC, serverStatusData, withSC3)

import           Paths_study_for_user_interfaces (getDataDir)


-- --------------------------------------------------------------------------
--
-- * Elements
--
-- --------------------------------------------------------------------------

addJavaScript :: Window -> FilePath -> UI ()
addJavaScript w filename = void $ do
    el <- mkElement "script"
          # set (attr "src") ("/static/js/" ++ filename)
    getHead w #+ [element el]

-- | Simple text box with @\<input type=\"text\"\>@.
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

-- | Horizontal slider with @\<input type=\"range\"\>@.
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
        v' <- case reads v of
            (v',""):_ -> act v'
            _         -> return ""
        element clab # set text v'
    UI.div
        #. "hrange-wrapper"
        #+ [element cnam, element clab, UI.div, element cval]

-- | Vertical slider with @\<input type=\"range\"\>@.
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
    on input cval $ \_ -> do
        v' <- get value cval
        v'' <- case reads v' of
            (v'',""):_ -> act v''
            _          -> return ""
        element clab # set text v''
    cclear <- UI.div
    UI.div
        #. "vrange-wrapper"
        # set style [("padding","10px 2px")]
        #+ map element [cnam, cval, cclear, clab]

-- | Vertical slider.
vslider ::
    String                   -- ^ Label to show.
    -> Int                   -- ^ Width.
    -> Int                   -- ^ Height.
    -> Double                -- ^ Min value.
    -> Double                -- ^ Max value.
    -> Double                -- ^ Initial value.
    -> (Double -> UI String) -- ^ Action to take with slider value.
    -> UI Element
vslider = mk_slider 'v'

-- | Horizontal slider.
hslider ::
    String                   -- ^ Label to show.
    -> Int                   -- ^ Width.
    -> Int                   -- ^ Height.
    -> Double                -- ^ Min value.
    -> Double                -- ^ Max value.
    -> Double                -- ^ Initial value.
    -> (Double -> UI String) -- ^ Action to take with slider value.
    -> UI Element
hslider = mk_slider 'h'

-- | Make slider.
mk_slider ::
    Char
    -> String
    -> Int
    -> Int
    -> Double
    -> Double
    -> Double
    -> (Double -> UI String)
    -> UI Element
mk_slider axis label width height minv maxv iniv act = do
    let (faxis,valname,pivotname,vallen,pivotlen) = case axis of
            'v' -> (snd,"height","width",height,width)
            'h' -> (fst,"width","height",width,height)
            _   -> error ("Invalid axis: " ++ show axis)
        vallen_d = fromIntegral vallen :: Double
        inivallen' :: Int
        inivallen' = ceiling (vallen_d * ((iniv - minv) / (maxv - minv)))
        inivallen = show inivallen'

    label' <- UI.div # set UI.text label
    sld <- UI.div
           #. (axis:"slider-sld")
           # set UI.style [(valname, show vallen ++ "px")
                          ,(pivotname, show (pivotlen-2) ++ "px")
                          ]
    val <- UI.div
           #. (axis:"slider-val")
           # set UI.style [(valname, inivallen ++ "px")
                          ,(pivotname, show (pivotlen-2) ++ "px")
                          ]
    param <- UI.div # set UI.text (show iniv)

    let setv v = do
            let vallen' = case axis of
                    'v' -> vallen - faxis v
                    _   -> faxis v
                vallen'_d = fromIntegral vallen'
                v'  = minv + (maxv-minv) * vallen'_d / fromIntegral vallen
            void $ element val #
                set UI.style [(valname, show vallen' ++ "px")]
            v'' <- act v'
            void $ element param # set UI.text v''

    activeRef <- liftIO $ newIORef False

    on UI.mousemove sld $ \xy -> do
        active <- liftIO $ readIORef activeRef
        when active $ setv xy
    on UI.mousedown sld $ \xy ->
        liftIO (writeIORef activeRef True) >> setv xy
    on UI.mouseup sld $ \_ ->
        liftIO $ writeIORef activeRef False
    on UI.leave sld $ \_ ->
        liftIO $ writeIORef activeRef False

    v'' <- act iniv
    void $ element param # set UI.text v''

    UI.div
        #. (axis:"slider-wrapper")
        #+ [ element label'
           , element sld #+ [ element val ]
           , element param
           ]
        # set UI.style [("width",show width ++ "px")
                       ,("height",show height ++ "px")
                       ]

-- | Returns checkbox in horizontal sequence.
hcheckbox ::
    String -- ^ Label name
    -> Int -- ^ Number of checkboxes.
    -> (Int -> Bool -> UI ())
    -- ^ Action taken on check, with index value and 'True' on check, 'False' on
    -- uncheck.
    -> UI (Element, [Element])
hcheckbox lbl n act = do
    checks <- forM [0..n-1] $ \m -> do
        cbox <- UI.input # set UI.type_ "checkbox"
        on UI.checkedChange cbox $ act m
        return cbox
    lbldiv <- UI.div
        # set text lbl
        # set style [("float","left")
                    ,("font-size","10px")
                    ,("padding","4px 5px 0 5px")
                    ]
    wrapper <- UI.div
        #+ (element lbldiv : map element checks)
        # set style [("padding","10px 10px 0 10px")]
    return (wrapper, checks)

toggleBox ::
    String -- ^ Label.
    -> Int -- ^ Width.
    -> Int -- ^ Height.
    -> (Bool -> UI ()) -- ^ Action to take on toggle.
    -> UI Element
toggleBox label width height act = do
    label' <- UI.div
              # set text label
              # set style [("font-size","10px")]
    box <- UI.div
        # set style [("border", "solid 1px #888")
                    ,("width", show (width-2) ++ "px")
                    ,("height", show (height-2) ++ "px")
                    ]
        # set value "0"
    on UI.click box $ \_ -> do
        val <- get value box
        let val' = if val == "1" then False else True
        void $ if not val'
           then element box
                # set value "0"
                # set style [("background-color","#fff")]
           else element box
                # set value "1"
                # set style [("background-color","#888")]
        act val'
    UI.div
        #+ map element [label', box]
        # set style [("width", show (width-2) ++ "px")]

-- | Returns grid of toggle boxes.
toggleGrids ::
    Int    -- ^ Number of columns.
    -> Int -- ^ Number of rows.
    -> ((Int,Int) -> Int -> UI ())
    -- ^ Action for each toggle box. Passed arguments are: (row index, column
    -- index) and 0 for the box being off and 1 for being on.
    -> UI (Element, [((Int,Int), Element)])
    -- ^ A pair of whole wrapper contents and each toggle box with (row,column)
    -- index.
toggleGrids ncol nrow act = do
    let width = 25 :: Int
        height = 20 :: Int
    columns_x_grds <- forM [0..nrow-1] $ \rowi -> do
        columWrap <- UI.div # set style
                     [("clear","both")
                     ,("height",show height ++ "px")
                     ]
        grds <- forM [0..ncol-1] $ \coli -> do
            grd <- UI.div
                   # set style [("width",show width ++ "px")
                               ,("height",show height ++ "px")
                               ,("float","left")
                               ,("border-right","solid 1px")
                               ,("border-bottom","solid 1px")
                               ,("font-size","8px")
                               ]
                   # set value "0"
            when (coli `mod` 4 == 0 && rowi `mod` 4 == 0) $
                void $ element grd # set UI.text (show coli)
            on UI.click grd $ \_ -> do
                val <- toggleSingleGrid grd
                void $ act (rowi,coli) val
            return ((rowi,coli),grd)
        clm <- element columWrap #+ map (element . snd) grds
        return (clm, grds)
    wrapper <- UI.div # set style
               [("border-top","solid 1px")
               ,("border-left","solid 1px")
               ,("width",show ((width+1) * ncol) ++ "px")
               ,("float","left")
               ]
    let (columns, grds) = unzip columns_x_grds
    wrapper' <- element wrapper #+ map element columns
    return (wrapper', concat grds)

-- | Toggle value and background colour of single box.
toggleSingleGrid ::
    Element   -- ^ Div element of box, returned from 'toggleGrids'.
    -> UI Int -- ^ Value after the toggle.
toggleSingleGrid grd = do
    val <- grd # get UI.value
    if val == "0"
       then turnOnGrid grd >> return 1
       else turnOffGrid grd >> return 0

-- | Turn on the box element returned from 'toggleGrids'.
turnOnGrid :: Element -> UI Element
turnOnGrid box =
    element box
        # set style [("background-color","#888")]
        # set UI.value "1"

-- | Turn off the box element returned from 'toggleGrids'.
turnOffGrid :: Element -> UI Element
turnOffGrid box =
    element box
        # set style [("background-color","#fff")]
        # set UI.value "0"

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
    param <- UI.div #. "xyarea-param" # set text "(--,--)"
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
    on UI.mousedown area $ \xy -> do
        liftIO $ writeIORef activeRef True
        setxy xy
    on UI.mouseup area $ \_ ->
        liftIO $ writeIORef activeRef False
    on UI.leave area $ \_ ->
        liftIO $ writeIORef activeRef False

    wrapper <- UI.div
        #. "xyarea-wrapper"
        #+ [element label, element area, element param]

    return wrapper

-- | Simple knob with /knobjs/.
knob :: String -> Int -> Double -> Double -> (Double -> UI ()) -> UI Element
knob lbl size minv maxv act = do
    let sizePx  = show size ++ "px"
    k <- mkElement "x-knobjs-knob"
         # set UI.style [("width",sizePx)
                        ,("height",sizePx)
                        ]
         # set (attr "throw") (show (size * 2))
         # set (attr "min") (show minv)
         # set (attr "max") (show maxv)
    on change k $ \_ -> do
        v <- get value k
        case reads v of
            (v',""):_ -> act v'
            _         -> return ()
    UI.new
        #+ [ element k
           , UI.div
             # set UI.text lbl
             # set UI.style [("font-size","10px")
                            ,("margin-top","-10px")
                            ,("text-align","center")
                            ]
           ]
        # set UI.style [("float","left")
                       ,("margin-right","5px")
                       ]


-- | Returns timer and div to show scsynth server status.
statusDiv ::
    Transport t
    => t -- ^ Transport of scsynth.
    -> UI (UI.Timer, Element)
statusDiv fd = do
    timer <- UI.timer # set UI.interval 1000
    ds@[stCPUp,stCPUa,stUGens,stSynths,stGroups,stInstruments] <-
        replicateM 6 $ UI.div #. "status-param"
    onEvent (UI.tick timer) $ \_ -> do
        -- st <- liftIO $ getStatus fd
        st <- liftIO (withSC3 $ getStatus)
        void $ element stCPUp # set text (printf "%2.2f%%" (ssCPUPeak st))
        void $ element stCPUa # set text (printf "%2.2f%%" (ssCPUAverage st))
        void $ element stUGens # set text (show (ssUGens st) ++ "u")
        void $ element stSynths # set text (show (ssSynths st) ++ "s")
        void $ element stGroups # set text (show (ssGroups st) ++ "g")
        void $ element stInstruments # set text (show (ssInstruments st) ++ "d")
    sel <- UI.select
           #. "status-printlevel"
           #+ map (\s -> UI.option # set text s)
           ["NoPrinter","TextPrinter","HexPrinter","AllPrinter"]
    on UI.selectionChange sel $ \v ->
        liftIO $ maybe (return ()) (\v' -> sendOSC fd $ dumpOSC $ toEnum v') v
    stext <- UI.div
             #. "status-text"
             # set text "scsynth"
    stWrapper <- UI.div
             #. "status"
             # set UI.children (stext:sel:ds)
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
