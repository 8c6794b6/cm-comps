{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Graphical user interface, take 1.

Simple 32 step grid sequencers with 0/1 values, with grouped mute and un-mutes.
Each grid row controls synth, which takes common trigger signal and steps
through assigned buffer. How the trigger signal treated are defined inside each
synthdef, some uses for percussive hit, some for picking up new pitches, and so
on.

-}
module Sound.Study.ForUserInterfaces.GUI01 where

import           Control.Arrow (first)
import           Control.Concurrent (forkIO, killThread)
import           Control.Monad (foldM, foldM_, forM, forM_, unless, void, when)
import           Control.Monad.Reader (runReaderT)
import           Data.List (isPrefixOf)
import           Data.Maybe (catMaybes)
import           System.Random (newStdGen, randomRs)
import           Text.Printf (printf)

import           Sound.OSC.FD
import           Sound.SC3.FD
import           Sound.SC3.TH.Synthdef (synthdefGenerator)
import           Sound.SC3.Tree
import           Sound.SC3.UGen.ID

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (stepper)

import qualified Graphics.UI.Threepenny.Extra as Extra
import qualified Sound.Study.ForUserInterfaces.JS as JS
import           Paths_study_for_user_interfaces (getDataDir)

-- --------------------------------------------------------------------------
--
-- * UI
--
-- --------------------------------------------------------------------------

-- | Main entry point.
main :: IO ()
main = withSC3 $ \fd -> do

    -- buffers for toggled sequences
    mapM_ (\x -> async fd $ b_alloc x 32 1) [0..ntrack-1]
    -- buffers for grouping mute/unmute
    mapM_ (\x -> async fd $ b_alloc x ntrack 1) mutebufs
    mapM_ (async fd . d_recv) synthdefs
    runReaderT (patchNode $ nodify nodes) fd

    dataDir <- getDataDir
    tid <- forkIO $ startGUI
           defaultConfig { tpStatic=Just (dataDir ++ "/static")
                         , tpCustomHTML=Just (dataDir ++ "/gui01.html")
                         } (setup fd)
    getChar >> killThread tid

-- | Number of tracks.
ntrack :: Num a => a
ntrack = 14

-- | Buffer numbers used for mute/un-mute.
mutebufs :: (Show a, Num a) => [a]
mutebufs = [100, 101, 102]

-- | Buffer number used for recording with 'diskOut'.
recbufn :: Num a => a
recbufn = 999

-- | Control rate outputs for triggers.
kouts :: (Num a, Enum a) => [a]
kouts = [101..]

-- | Node arrangement for GUI.
nodes :: Nd
nodes =
  let t00 = syn "trig00" ["out"*=100,"bpm"*=128,"beat"*=4]
      rb o b = syn "rbufrd01"
               ["out"*=Ival o,"bufn"*=Dval b, "len"*=32,"tr0"*<-t00-*"out"]
      ts = zipWith rb kouts [0..ntrack-1]
  in  grp 0
      [ grp 1
        [ grp 10 (t00:ts)
        , grp 11
          [ syn "bd01"
            ["out"*=0,"freq"*=48,"dur"*=0.09,"t_tr0"*<-ts!!0-*"out"]
          , syn "hat01"
            ["out"*=1,"t_tr0"*<-ts!!1-*"out"]
          , syn "snr01"
            ["out"*=2,"t_tr0"*<-ts!!2-*"out"]
          , syn "prc01"
            ["out"*=3,"t_tr0"*<-ts!!3-*"out"]
          , syn "prc02"
            ["out"*=4,"t_tr0"*<-ts!!4-*"out"]
          , syn "nz01"
            ["out"*=5,"t_tr0"*<-ts!!5-*"out"]
          , syn "add01"
            ["out"*=6,"t_tr0"*<-ts!!6-*"out","t_tr1"*<-t00-*"out"
            ,"faf"*=9.45,"hps"*=2.80]
          , syn "add02"
            ["out"*=7,"t_tr0"*<-ts!!7-*"out"]
          , syn "saw01"
            ["out"*=8,"t_tr0"*<-ts!!8-*"out","t_tr1"*<-t00-*"out"
            ,"cfhi"*=7562,"ftrr"*=0.1]
          , syn "sine01"
            ["out"*=9,"t_tr0"*<-ts!!9-*"out"]
          , syn "sine02"
            ["out"*=10,"t_tr0"*<-ts!!10-*"out"]
          , syn "pulse01"
            ["out"*=11,"t_tr0"*<-ts!!11-*"out"]
          , syn "fm01"
            ["out"*=12,"t_tr0"*<-ts!!12-*"out"]
          , syn "pv01"
            ["out"*=13,"t_tr0"*<-ts!!13-*"out"]
          ]
        , grp 12
          [ syn "mixer01" ["t_tr0"*<-t00-*"out"]
          , syn "rec01"   ["bufn"*=999]
          ]
        ]
      , grp 2 []
      ]

-- | Synonym for pattern format loaded from text file.
type PatternFormat = [(String,[Double])]

-- | Data type to specify curved range of control value.
data ControlRange
    = -- | Control value changes exponentially.
      ExpRange Double Double
      -- | Control value changes linearly.
    | LinRange Double Double

-- | Setup GUI with given 'Transport'.
setup :: Transport t => t -> Window -> UI ()
setup fd window = do
    void $ return window # set title "gui 01"
    UI.addStyleSheet window "ui.css"

    currentNodes <- liftIO $ runReaderT getRootNode fd
    let queryByName name =
            maybe (error (name ++ " not found")) id $
            queryN' (synthName ==? name) currentNodes
        queryParam name node =
            case queryP' (paramName ==? name) node of
                Just (name' := val) | name == name' -> val
                _                                   ->
                    error $ unwords ["no", name, "found in", show node]
        tr00node = queryByName "trig00"
        tr00nid  = nodeId tr00node
        mixer01node = queryByName "mixer01"
        mixer01nid  = nodeId mixer01node
        rec01node = queryByName "rec01"
        rec01nid = nodeId rec01node

    -- status div
    (tmr,stDiv) <- Extra.statusDiv fd

    -- common trigger
    let iniBpmVal  = queryParam "bpm" tr00node
        iniBeatVal = queryParam "beat" tr00node
    bpm <- Extra.textbox "bpm" 50 (show (ceiling iniBpmVal :: Int)) $ \v -> do
        let vs = reads v
        unless (null vs) $ liftIO $
            send fd $ n_set tr00nid [("bpm",fst $ head vs)]
    beat <- Extra.hslider "beat (2**(v/2))" 128 20 0 16 iniBeatVal $ \v -> do
        liftIO $ send fd $ n_set tr00nid [("beat",2**(v/2))]
        return $ show v

    -- mixer controls
    mamp <- Extra.hslider "mamp" 128 20 (-60) 25 0 (\v -> do
        liftIO $ send fd $ n_set mixer01nid [("mamp",v)]
        return $ printf "%3.2f" v) # set style [("float","left")]
    lmt <- Extra.toggleBox "lmt" 15 15 (\checked ->
            liftIO $ send fd $ n_set mixer01nid [("lmt",if checked then 1 else 0)])
            # set style [("margin","10px 5px 0px 5px")]
    revrmix <- knobControl fd "rmix" mixer01nid 0 $ LinRange 0 1
    revdamp <- knobControl fd "damp" mixer01nid 0 $ LinRange 0 1
    revroom <- knobControl fd "room" mixer01nid 0 $ LinRange 0 1
    mcf     <- knobControl fd "cf" mixer01nid 8000 $ ExpRange 20 20000
    mrq     <- knobControl fd "rq" mixer01nid 0.5 $ LinRange 0.01 0.99
    mfp     <- knobControl fd "mfp" mixer01nid 0 $ LinRange 0 1

    let goRec checked =
            let act | checked   = do
                    mapM_ (async fd) $
                        [ b_alloc recbufn 8192 2
                        , b_write recbufn "out.wav" Wave PcmFloat (-1) 0 True
                        ]
                    send fd $ n_run [(rec01nid, True)]
                    | otherwise =
                    sendOSC fd $ bundle immediately
                        [ n_run [(rec01nid,False)]
                        , b_close recbufn, b_free recbufn
                        ]
            in  liftIO act

    recbtn  <- Extra.toggleBox "rec" 15 15 goRec
               # set style [("margin","10px 5px 0px 5px")]

    -- div for layout
    let divClear = UI.div # set style [("clear","both")]

    -- mutes
    (mutes, radios) <- do
        radios <- forM mutebufs $ \n -> do
            radio <- UI.input
                # set UI.type_ "radio"
                # set UI.name "set"
                # set UI.value (show (n::Int))
            on UI.click radio $ \_ -> do
                bufn <- radio # get UI.value
                liftIO $ do
                    vals <- queryBuffer (read bufn) fd
                    sendOSC fd $ n_set mixer01nid $
                        zipWith (\i v -> ("mute"++show i,realToFrac v))
                        [(0::Int)..] vals
            return radio
        lagt <- Extra.textbox "lag" 50 "4" $ \str -> do
            let val = reads str
            unless (null val) $ liftIO $
                send fd $ n_set mixer01nid [("lagt", fst (head val))]
        mutes <- UI.new #+ [ UI.new
                             #+ map element radios
                             # set style [("float","left")
                                         ,("margin-top","18px")
                                         ]
                           , element lagt
                             # set style [("float","left")]
                           ] # set style [("float","left")]
        void $ element lagt # set style [("float","left")]
        return (mutes, radios)

    -- synth controls and sequence grids
    let track (lbl,nid) n = do
            (grids, _boxes) <- Extra.toggleGrids 32 1 $ \(_i,j) val ->
                liftIO $ send fd $ b_set n [(j, fromIntegral val)]
            let feff v = do
                    liftIO $ send fd $ n_set mixer01nid
                        [("efx"++show n, if v then 1 else 0)]
                fmute k v = do
                    let v' = if v then 1 else 0
                        bufn = mutebufs !! k :: Int
                        fr _ radio = do
                            isChecked <- get UI.checked radio
                            radioVal  <- get value radio
                            when (isChecked && radioVal == show bufn) $
                                liftIO $ send fd $ n_set mixer01nid
                                    [("mute"++show n, realToFrac v')]
                    liftIO $ send fd $ b_setn1 bufn n [v']
                    foldM_ fr () radios
                muteBox m = Extra.toggleBox "" 15 15 (fmute m)
                            # set style [("float","left")
                                        ,("margin","12px 4px 0")
                                        ]
                mkbtn val act = do
                    btn <- UI.button
                        # set text val
                        # set style [("font-size","10px")
                                    ,("margin","12px 3px 0 3px")
                                    ,("float", "left")
                                    ]
                    on UI.click btn $ act
                    return btn

                (_,boxes) = unzip _boxes

            knobs <- knobControls fd lbl nid
            ampKnob <- knobControl fd ("amp" ++ show n) mixer01nid 0
                       (LinRange (-60) 25)
            panKnob <- knobControl fd ("pos" ++ show n) mixer01nid 0
                       (LinRange (-1) 1)
            knobs' <- UI.new #+ map element (ampKnob : panKnob : knobs)
                     # set style [("display", "none")
                                 ,("margin", "10px")
                                 ,("padding", "10px")
                                 ,("border", "solid 1px #888")
                                 ,("height", "40px")
                                 ]

            clearButton <- mkbtn "c" $ \_ -> do
                mapM_ Extra.turnOffGrid boxes
                liftIO $ send fd $ b_setn1 n 0 (replicate 32 0)

            randButton <- mkbtn "r" $ \_ -> do
                g0 <- liftIO newStdGen
                let vals = take 32 $ randomRs (0,1::Int) g0
                mapM_ (\(v,b) -> if v == 1 then Extra.turnOnGrid b
                                 else Extra.turnOffGrid b)
                    (zip vals boxes)
                liftIO $ send fd $ b_setn1 n 0 (map realToFrac vals)

            showButton <- mkbtn "k" $ \_ -> JS.toggle knobs'

            wrapper <- UI.new #+
                ([ muteBox 0, muteBox 1, muteBox 2
                 , Extra.toggleBox "fx" 15 15 feff
                   # set style [("float", "left")
                               ,("margin","1px 3px 0")]
                 , element showButton
                 , element clearButton
                 , element randButton
                 , element grids
                   # set style [("margin-top","9px")]
                 , divClear
                 ] ++ [element knobs'] ++
                 [ divClear
                 , UI.new
                   # set text lbl
                   # set style [("font-size","10px")]
                 ])
            return (wrapper, (lbl,(n,boxes)))

    -- layout --
    mapM_ (\e -> element e # set style [("float","left")])
        [ bpm, beat, lmt, recbtn ]

    let synths = map (\s -> (synthName s, nodeId s)) $
                 queryN (not . null . synthName) g11
        g11 = case queryN' (nodeId ==? 11) (nodify nodes) of
            Just node -> node
            Nothing   -> error "node id 11 not found"

        tracksM :: UI ([Element],[(String,(Int,[Element]))])
        tracksM =
            let go (ts,ps) (lbl,n) =
                    track lbl n >>= \(t,p) -> return (t:ts,p:ps)
            in  first reverse <$> foldM go ([],[]) (zip synths [0..ntrack-1])

    (tracks,lbl_bufn_boxes) <- tracksM

    let openFile act = do
            btn <- UI.input
                   # set UI.type_ "file"
                   # set UI.name "files[]"
                   # set style [("font-size","10px")
                               ,("float","left")
                               ,("width", "150px")
                               ,("margin","18px 3px 0 3px")
                               ]
            on Extra.change btn $ \_ -> act =<< JS.getFileName btn
            return btn

        fillPatternFormat :: PatternFormat -> UI ()
        fillPatternFormat pf =
            forM_ pf $ \(n1,vs) ->
              forM_ lbl_bufn_boxes $ \(n2,(bufn,boxes)) -> when (n1 == n2) $ do
                liftIO $ send fd $ b_setn1 bufn 0 vs
                forM_ (zip vs boxes) $ \(v,b) ->
                    if v == 1 then Extra.turnOnGrid b else Extra.turnOffGrid b

    -- button to load grid patterns
    loadButton <- openFile $ \filename -> unless (null filename) $ do
        datadir <- liftIO getDataDir
        contents <- liftIO $ readFile (datadir ++ "/buffer/" ++ filename)
        let vs :: [(PatternFormat ,String)]
            vs = reads contents
        case vs of
            (pf,_):_  -> fillPatternFormat pf
            _         -> liftIO $ print "malformed pattern"

    -- Disable recording
    liftIO $ send fd $ n_run [(rec01nid,False)]

    void $ getBody window #+
        [ element stDiv
        , UI.new #
          set style
          [("margin","0 auto"),("width", "836px"),("padding","5px")] #+
          ([ UI.new #
            set style [("float","left")] #+
            [ element mutes, element lmt, element mamp
            , element bpm, element beat, element recbtn, element loadButton
            , divClear
            , element revrmix, element revdamp, element revroom
            , element mcf, element mrq, element mfp
            ]
           , divClear
           ] ++
           map element tracks)
        ]
    UI.start tmr

-- | Make knob control from 'ControlRange'.
knobControl ::
    Transport fd
    => fd           -- ^ Transport to send message.
    -> String       -- ^ Parameter name
    -> Int          -- ^ Node id.
    -> Double       -- ^ Initial value
    -> ControlRange -- ^ Control range for this 'Extra.knob'.
    -> UI Element
knobControl fd param nid iniv cc = Extra.knob param 40 minv maxv ini $ \v ->
    liftIO $ send fd $ n_set nid [(param, fv v)]
  where
    (fv, minv, maxv, ini) = case cc of
        ExpRange s e -> (exp, log s, log e, log iniv)
        LinRange s e -> (id, s, e, iniv)

-- | Make knob controls for synthdef using preset value for min and max.
knobControls :: Transport fd => fd -> String -> Int -> UI [Element]
knobControls fd lbl nid =
    let ks = [ knobControl fd pname nid (realToFrac pval) cc
             | (pname, pval) <- ps
             , pname /= "out", not ("t_" `isPrefixOf` pname)
             , let err = error $ "knobControls: " ++ pname ++ " not found"
             , let cc  = maybe err id (lookup lbl knobPresets >>= lookup pname)
             ]
        ps = [ (node_k_name c, node_k_default c)
             | def <- synthdefs
             , c <- controls $ synthdefGraph def
             , synthdefName def == lbl
             ]
    in  sequence ks

-- | Knob preset values.
knobPresets :: [(String, [(String,ControlRange)])]
knobPresets =
    [("bd01",    [("dur", ExpRange 0.01 1.0)
                 ,("freq",ExpRange 20 100)
                 ])
    ,("prc02",   [("prb1", LinRange 0 1)
                 ,("prb2", LinRange 0 1)
                 ,("prb3", LinRange 0 1)
                 ,("prb4", LinRange 0 1)
                 ])
    ,("add01",   [("hps", LinRange 0.1 10)
                 ,("faf", LinRange 0.2 30)
                 ])
    ,("add02",   [("dur1", ExpRange 0.1 10)
                 ,("prb1", LinRange 0 1)
                 ,("pamp", LinRange 1 50)
                 ,("en1",  LinRange (-10) 10)
                 ])
    ,("saw01",   [("cfhi", ExpRange 200 12000)
                 ,("ftrr", LinRange 0 1)
                 ])
    ,("sine01",  [("dmax", ExpRange 0.1 32)
                 ,("dff",  ExpRange 0.01 100)
                 ])
    ,("sine02",  [("dct", ExpRange 0.5 16)
                 ,("dlt", ExpRange 0.02 2)
                 ,("dur", ExpRange 0.1 8)
                 ])
    ,("pulse01", [("rfrq", ExpRange 0.01 50)
                 ,("cfrq", ExpRange 0.01 50)
                 ,("wfrq", ExpRange 0.01 50)
                 ,("lagt", ExpRange 0.01 4)
                 ])
    ,("fm01",    [("dur",  ExpRange 0.01 2)
                 ,("maxi", LinRange 1 64)
                 ])
    ,("pv01",    [("tlag", ExpRange 0.002 3)
                 ,("minFreq", ExpRange 20 20000)
                 ,("maxFreq", ExpRange 20 20000)
                 ,("cutoff",  ExpRange 20 20000)
                 ,("density", ExpRange 1 2000)
                 ])
    ]

-- --------------------------------------------------------------------------
--
-- * Synth
--
-- --------------------------------------------------------------------------

-- | Trigger to sync other synths.
synth_trig00 :: UGen
synth_trig00 = out (control KR "out" 0) sig0
  where
    sig0 = impulse KR freq 0
    freq = (beat*bpm)/60
    beat = control KR "beat" 4
    bpm  = control KR "bpm" 128

-- | 'bufRdN' with 'coinGate'.
synth_rbufrd01 :: UGen
synth_rbufrd01 = out (control KR "out" 0) sig
  where
    sig  = coinGate 'g' (bufRdN 1 KR bufn idx Loop) tr0
    bufn = control KR "bufn" 100
    idx  = gate (stepper tr0 0 0 (len-1) 1 0) tr0
    len  = control KR "len" 16
    tr0  = control KR "tr0" 1

-- | Simple additive synth.
synth_add01 :: UGen
synth_add01 = out (control KR "out" 0) sig0
  where
    sig0 = mix (mce fs) * aenv
    fs   = foldr (\x acc -> fa x * sinOsc AR (x*freq) 0 : acc) [] [1..9]
    freq = lag (select idx (mce pchs)) 0.005
    idx  = pulseCount tr1 tr2 `modE` (constant (length pchs))
    pchs = map midiCPS degs
    degs = map (+off) [36,48,60,72, 48,60,72,84]
    off  = tChoose 'o' tr3 (mce [-12,-7,-5,0,5,7,12])
    tr2  = coinGate 'g' 0.25 tr1
    tr3  = coinGate 'o' (1/32) tr1
    fa c = lfdNoise3 c KR (lfdNoise3 (c+100) KR 1 * faf + faf) ** 2
    aenv = envGen AR tr0 amp 0 dur DoNothing ash
    amp  = tExpRand 'a' 0.5 1 tr0 * 0.3
    ash  = envCoord [(0,0),(atk,1),(sus,1),(1,0)] 1 1 EnvCub
    atk  = tExpRand 'A' 0.01 0.999 tr0
    sus  = tExpRand 's' 0.01 (1-atk) tr0
    tr0  = control KR "t_tr0" 1
    dur  = recip hps
    faf  = control KR "faf" 9.23 `lag` 0.1
    hps  = control KR "hps" 0.3 `lag` 0.1
    tr1  = control KR "t_tr1" 0

-- | Simple additive synth with 'clip2'.
synth_add02 :: UGen
synth_add02 = out (control KR "out" 0) sig0
  where
    sig0  = rlpf (sig1+sig2+sig3) 8000 0.1 * 0.1 * aenv1
    sig1  = resonz sig4 12000 0.3
    sig2  = bBandStop sig4 1000 0.9
    sig3  = resonz sig4 125 0.6
    sig4  = sig5 + sig6
    sig5  = clip2 (mix (sinOsc AR freq 0 * aenv0 * pamp)) 1
    sig6  = rlpf sig7 14000 0.8 * aenv2
    sig7  = brownNoise 'B' AR
    freq  = tChoose 'T' tr1 (mce $ map frq [0,5,7]) `lag2` 0.01
    frq x = mce $ concat
            [[y,y*2.999,y*6.001,y*8.999]|y<-map midiCPS [x+36,x+43,x+48]]
    aenv0 = envGen KR tr0 amp 0 dur DoNothing ash0
    ash0  = Envelope [0,1,0.1,0.005,0] [0.002,0.08,0.85,0.08] [EnvNum en1]
            Nothing Nothing
    aenv1 = envGen KR tr0 1 0 dur1 DoNothing ash1
    ash1  = Envelope [1,1,1,0] [0.0005,0.9999,0.0005] [EnvCub] (Just 0) Nothing
    aenv2 = envGen KR tr0 1 0 0.1 DoNothing $
            Envelope [0,1,0] [0.01,0.99] [EnvSqr] (Just 0) Nothing
    dur   = 3
    dur1  = control KR "dur1" 0.4
    amp   = tExpRand 'A' 0.5 1 tr0
    tr0   = tr_control "t_tr0" 1
    tr1   = coinGate 'g' prb1 tr0
    prb1  = control KR "prb1" 0.125
    pamp  = control KR "pamp" 20
    en1   = control KR "en1" (-2)

-- | Plays 'synth_add02' with 'synth_trig00'.
go_add02 :: IO ()
go_add02 = withSC3 $ \fd -> do
    play fd synth_add02
    send fd $ n_map (-1) [("t_tr0",100)]
    send fd $ s_new "trig00" (-1) AddBefore (-1) [("out",100)]

-- | Simple saw tooth oscillator.
synth_saw01 :: UGen
synth_saw01 = out (control KR "out" 1) sig0
  where
    sig0     = rlpf sig1 cf rq * aenv * 0.3
    sig1     = mix (saw AR (mce [freq+vib, freq*0.998, freq*1.002]))
    freq     = select idx (mce pchs)
    vib      = lfTri KR 5.21 0 * 2.23
    idx      = pulseCount tr1 tr2 `modE` (constant (length pchs))
    pchs     = map midiCPS degs
    degs     = [36,67,84,38, 67,65,36,84]
    cf       = lag2 (f0 'c' 50 cfhi) 0.5
    rq       = lag2 (f0 'r' 0.05 0.95) 0.25
    f0 c l h = gate (linLin (lfdNoise3 c KR 3) (-1) 1 l h) zo0
    aenv     = envGen KR tr3 1 0 0.2 DoNothing ash
    ash      = envCoord [(0,0),(1e-4,1),(0.5,0.2)] 1 1 EnvCub
    zo0      = toggleFF (dust 'D' KR 1)
    tr1      = control KR "t_tr1" 0
    tr2      = coinGate 'g' ftrr tr1
    tr3      = tr_control "t_tr0" 1
    cfhi     = control KR "cfhi" 8000 `lag` 0.1
    ftrr     = control KR "ftrr" (1/16) `lag` 0.1

-- | Simple pulse chord.
synth_pulse01 :: UGen
synth_pulse01 = out (control KR "out" 0) sig0
  where
    sig0 = rlpf sig1 cf rq * aenv * 0.1
    sig1 = mix (pulse AR (mce freq `lag2` lagt) wdt)
    freq = map (\i -> select (tIRand i 0 (constant (length pchs) - 1) tr0)
                      (mce pchs))
           [0..3::Int]
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    octs = take 3 $ iterate (+12) 48
    degs = [0,2,5,7]
    wdt  = linLin (lfdNoise3 'W' KR wfrq) (-1) 1 0 0.5
    cf   = linExp (lfdNoise3 'C' KR cfrq + 2) 1 3 400 12000
    rq   = linLin (lfdNoise3 'Q' KR rfrq) (-1) 1 0.1 0.9
    aenv = envGen KR tr0 1 0 dur DoNothing ash
    ash  = Envelope [0,1,1,0.3,0] [0.001,0.1,0.9,0.1] [EnvCub] Nothing Nothing
    dur  = 8
    tr0  = tr_control "t_tr0" 1
    lagt = control KR "lagt" 0.05
    wfrq = control KR "wfrq" 0.25
    cfrq = control KR "cfrq" 3
    rfrq = control KR "rfrq" 5

-- | Simple bass drum sound.
synth_bd01 :: UGen
synth_bd01 = out (control KR "out" 0) sig0
  where
    sig0  = (resonz sig12 (freq*2) 0.98 + sig3) * amp
    sig12 = rlpf (mix (sig1+sig2)) cf rq
    sig1  = sinOsc AR (freq*12.32) phase * aenv0 * 0.1
    sig2  = saw AR freqs * aenv1 * 0.8
    sig3  = lpf (whiteNoise 'W' AR) 8000 * trig1 tr0 0.002 * 0.1
    phase = sinOsc AR (freq*5.29) 0 * aenv0 * 9
    aenv0 = envGen KR tr0 1 0 0.01 DoNothing $ ash0
    aenv1 = envGen KR tr0 1 0 dur DoNothing $ ash0
    ash0  = envCoord [(0,0),(1e-4,1),(5e-1,0.8),(1,0)] 1 1 EnvCub
    freqs = mce $ map (*freq) [1, 1.32, 1.732, 2.79]
    cf    = 20000 * aenv0 + freq
    rq    = 0.99
    freq  = control KR "freq" 30
    amp   = tExpRand 'a' 0.9 1 tr0
    dur   = control KR "dur" 0.25
    tr0   = tr_control "t_tr0" 1

-- | Simple hat sound.
synth_hat01 :: UGen
synth_hat01 = out (control KR "out" 0) (mix sig0)
  where
    sig0  = (sig1 + sig3)
    sig1  = rhpf sig2 8000 aenv0 * aenv0
    sig2  = pulse AR (freq * pulse AR (freq*1.83) 0.5) 0.5
    sig3  = rhpf sig2 9000 aenv2 * aenv2 * 0.5
    freq  = mce [803, 1729, 2532, 3783]
    aenv0 = envGen KR tr0 amp 0 dur DoNothing ash0 * aenv1
    ash0  = envPerc 1e-3 1
    aenv1 = lfdNoise3 'A' KR aef1 * 0.5 + 0.5
    aenv2 = decay2 tr0 1e-3 0.2
    aef1  = decay2 tr0 1e-2 3 * 10
    amp   = tExpRand 'a' 0.8 1 tr0
    dur   = 0.1
    tr0   = tr_control "t_tr0" 1

-- | Simple snare sound.
synth_snr01 :: UGen
synth_snr01 = out (control KR "out" 0) sig0
  where
    sig0  = (sig1*aenv1 + sig2*aenv2) * 0.2
    sig1  = rhpf (whiteNoise 'W' AR) cf rq
    cf    = aenv1 * 2000 + 50
    rq    = 0.9
    aenv1 = decay2 tr0 1e-4 0.3
    sig2  = mix (sinOsc AR freqs 0) * 0.8
    freqs = mce [128.31,179.9,258.12,371.3] * lfCub KR 193.32 pi
    aenv2 = envGen KR tr0 1 0 0.1 DoNothing ash2
    ash2  = envCoord [(0,0),(1e-2,1),(0.2,0.8),(1,0)] 1 1 EnvCub
    tr0   = tr_control "t_tr0" 1

-- | Simple percussive sound.
synth_prc01 :: UGen
synth_prc01 = out (control KR "out" 0) sig0
  where
    sig0  = mix (rhpf sig2 (freqs*0.759) (aenv1+1e-3)) * aenv0 * 0.1
    freqs = mce [  889 + (lfdNoise3 'a' KR (1/30) * 200)
                , 1829 + (lfdNoise3 'b' KR (1/60) * 800)
                , 2803 + (lfdNoise3 'c' KR (1/120) * 1820)
                , 5231 + (lfdNoise3 'd' KR (1/240) * 3800)
                ]
    sig2  = whiteNoise 'W' AR
    aenv0 = envGen KR tr0 1 0 dur DoNothing ash0
    ash0  = envCoord [(0,0),(1e-4,1),(1,0)] 1 1 EnvCub
    aenv1 = decay2 tr0 1e-4 0.1
    dur   = 0.3
    tr0   = tr_control "t_tr0" 1

-- | Simple percussive sound, using buffered trigger for 'tChoose', chooses one
-- signal from array of signals.
synth_prc02 :: UGen
synth_prc02 = out (control KR "out" 0) sig0
  where
    sig0  = tChoose 'I' tr0 sigs
    sigs  = mce [sig1, sig2, sig3, sig4]
    sig1  = rlpf (saw AR frq1) cf1 rq1 * aenv1 * 0.3
    frq1  = 63 * (2 ** tIRand 'F' 0 7 tr1)
    cf1   = aenv1 * frq1 * 3 * (sinOsc AR 23 0 * 0.5 + 0.5)
    rq1   = 0.3
    aenv1 = envGen KR tr0 1 0 1 DoNothing $
            Envelope [0,1,0.8,0] [0.001,0.1,0.9] [EnvCub] Nothing Nothing
    sig2  = rhpf (whiteNoise 'W' AR) cf2 rq2 * aenv2
    cf2   = 76.5 * (2 ** tIRand 'f' 0 6 tr2)
    rq2   = tExpRand 'R' 0.10 0.99 tr2
    aenv2 = envGen KR tr0 0.3 0 1 DoNothing $
            Envelope [0,1,1,0] [0.01,0.9,0.01] [EnvNum en2] Nothing Nothing
    en2   = tRand '2' (-3) 3 tr2
    sig3  = sinOsc AR mfrq3 frq3 * aenv3
    frq3  = sinOsc AR mfrq3 0 * mfrq3 * idx3
    mfrq3 = 213 * (2 ** tIRand 'H' 0 5 tr3)
    idx3  = linLin (lfdNoise3 'I' AR 8) (-1) 1 0 16
    aenv3 = (sinOsc AR amf3 0 * 0.5 + 0.5) * aenv2
    amf3  = linLin (lfdNoise3 'M' KR 0.06) (-1) 1 2 20
    sig4  = clipNoise 'G' AR * aenv4
    aenv4 = envGen KR (impulse KR aef4 0) 0.5 0 0.02 DoNothing $
            Envelope [0,1,1,0] [0.3,0.3,0.3] [EnvCub] Nothing Nothing
    aef4  = tExpRand 'E' 2 32 tr4
    tr0   = tr_control "t_tr0" 1
    tr1   = cg '1'
    tr2   = cg '2'
    tr3   = cg '3'
    tr4   = cg '4'
    cg n  = coinGate n (control KR ("prb"++[n]) 0.5) tr0

-- | Play 'synth_perc02' with 'synth_trig00'.
go_perc02 :: IO ()
go_perc02 = withSC3 $ \fd -> do
    play fd synth_prc02
    sendOSC fd $ bundle immediately
        [ n_map (-1) [("t_tr0",100),("bpm",128),("beat",2)]
        , s_new "trig00" (-1) AddBefore (-1) [("out",100)]
        ]

-- | Simple filtered white noise sound.
synth_nz01 :: UGen
synth_nz01 = out (control KR "out" 0) sig0
  where
    sig0  = mix (resonz sig1 cf rq) * aenv0 * 0.1
    sig1  = whiteNoise 'ω' AR
    fenv  = envGen KR tr0 1 0 dur DoNothing
    aenv0 = fenv ash0
    ash0  = Envelope [0,1,1,0] [0.7,0.2,0.1] [EnvNum 2] Nothing Nothing
    cf    = mce [2000,4100,6300,8800] * fenv0
    fenv0 = fenv fsh0
    fsh0  = Envelope [0.001,1] [1] [EnvExp] Nothing Nothing
    rq    = aenv0 * 3 + 0.001
    dur   = 0.4
    tr0   = tr_control "t_tr0" 1

-- | Simple sines, percussive chords with impulse triggers.
synth_sine01 :: UGen
synth_sine01 = out (control KR "out" 0) sig0
  where
    sig0 = mix (sinOsc AR freq 0) * aenv * 0.06
    freq = select (mce idx) (mce pchs)
    idx  = [ tIRand n 0 (constant $ length pchs - 1) (coinGate n 0.5 tr0)
           | n <-"alskdf*&%@1289-+" ]
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    octs = take 6 $ iterate (+12) 48
    degs = [0,2,4,5,7,9,11]
    aenv = decay2 tr1 1e-4 dur
    dur  = (recip df) `lag2` 0.05
    tr1  = impulse KR df 0 + tr0
    df   = linLin (lfdNoise0 'f' KR dff) (-1) 1 0.25 dmax
    dmax = control KR "dmax" 32
    dff  = control KR "dff" 8
    tr0  = tr_control "t_tr0" 1

-- | Simple sine, with echo.
synth_sine02 :: UGen
synth_sine02 = out (control KR "out" 0) sig0
  where
    sig0 = combC (sinOsc AR freq 0 * aenv) 1 dlt dct * 0.3
    freq = select idx (mce pchs)
    pchs = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    octs = take 3 $ iterate (+12) 60
    degs = [0,2,4,5,7,9,11]
    idx  = tIRand 'p' 0 (constant $ length pchs - 1) tr0
    aenv = envGen KR tr0 1 0 dur DoNothing ash
    ash  = Envelope [0,1,0.8,0] [0.001,0.099,0.9] [EnvCub] Nothing Nothing
    dur  = control KR "dur" 3
    tr0  = tr_control "t_tr0" 1
    dlt  = control KR "dlt" 0.15
    dct  = control KR "dct" 8

-- | Simple FM bass.
synth_fm01 :: UGen
synth_fm01 = out (control KR "out" 0) (mix sig0)
  where
    sig0  = mix (sinOsc AR freq phase) * amp0 * 0.005
    freq  = midiCPS (36 + df) `lag2` lagt
    df    = tWChoose 'F' tr0 (mce [0,2,5,7]) (mce [0.95,0.1,0.1,0.3]) 0
    phase = mce
            [ sinOsc AR (freq*x) 0 *
              envGen KR tr1 mi 0 (f*0.1) DoNothing
              (envCoord [(0,0),(tExpRand i 0.001 1 tr1,1),(1,0)] 1 1 EnvCub)
            | x <- [0.9999, 2.003, 3.999, 6.001, 8.001]
            , i <- "abcde"
            , f <- [0.15, 0.24, 0.12, 0.41, 0.08]]
    mi    = linExp (lfdNoise3 'M' KR 0.3 + 2) 1 3 1 maxi
    maxi  = control KR "maxi" 8
    lagt  = (1/64) * (2 ** tIRand 'L' 0 4 tr0)
    amp0  = envGen KR tr0 amp 0 dur DoNothing $
            envCoord [(0,0),(0.001,1),(0.2,0.2),(1,0.1)] 1 1 EnvCub
    dur   = control KR "dur" 2
    amp   = tExpRand 'A' 0.3 1 tr0
    tr0   = tr_control "t_tr0" 1
    tr1   = coinGate 'G' 0.6 tr0

-- | Simple convolution with 'saw' chord and 'dust'.
synth_pv01 :: UGen
synth_pv01 = mrg2 (out (k "out" 0) osig) recbuf
  where
    osig          = (fade * conv0 + (1-fade) * conv1) * 0.2
    fade          = toggleFF recTrig `lag` tlag
    [conv0,conv1] = mceChannels conv
    conv          = convolution2 isig bufk convTrigs frames
    frames        = bufFrames KR bufk
    convTrigs     = pulseDivider convTrig 2 (mce [0,1])
    convTrig      = tDelay recTrig (bufDur KR bufk)
    recbuf        = recordBuf AR bufk 0 1 0 1 NoLoop recTrig DoNothing irSig
    irSig         = mix (hpf (lpf (saw AR freqs) maxFreq) minFreq) * 0.4
    freqs         = mce $ map (\i -> tChoose i recTrig $ mce pchs) [1..8::Int]
    pchs          = foldr (\o acc -> map (midiCPS . (+o)) degs ++ acc) [] octs
    octs          = take 5 $ iterate (+12) 24
    degs          = [0,2,5,7]
    recTrig       = tr_control "t_tr0" 0
    isig          = lpf (dust2 'd' AR density) cutoff
    tlag          = k "tlag" 1
    minFreq       = k "minFreq" 200
    maxFreq       = k "maxFreq" 8000
    cutoff        = k "cutoff" 1000
    density       = k "density" 200
    k             = control KR
    bufk          = mrg2 (localBuf 'a' 2048 1) (maxLocalBufs 2)

-- | Simple mixer.
synth_mixer01 :: UGen
synth_mixer01 = mrg [replaceOut 0 (sigs0 * dbAmp mamp), maxLocalBufs 1]
  where
    sigs0 = clip2 lmtd 1
    lmtd = (lmt * limiter sigs1 1 0.02) + ((1-lmt) * sigs1)
    sigs1 = efx (sum sigsA) + sum sigsB
    (sigsA, sigsB) = unzip $ map f [0..ntrack-1::Int]
    f n   = (sig0*efxc, sig0*(1-efxc))
      where
        sig0 = pan2 sig1 posn (dbAmp ampn) * mute
        sig1 = in' 1 AR inn
        inn  = k ("in" ++ show n) (fromIntegral n)
        posn = k ("pos" ++ show n) 0
        ampn = k ("amp" ++ show n) 0.5
        efxc = k ("efx" ++ show n) 0
        mute = control KR ("mute" ++ show n) 1 `lag3` lagt
        lagt = control KR "lagt" 2
    efx x = freeVerb sig0 rmix room damp
      where
        sig0 = rlpf sig1 cf rq
        sig1 = ifft' $ pv_MagFreeze ch0 trmf
        ch0  = fft' (localBuf 'a' 2048 1) x
        trmf = toggleFF $ coinGate 'M' mfp (t_tr0)
    mamp  = k "mamp" 0
    rmix  = k "rmix" 0
    room  = k "room" 0.5
    damp  = k "damp" 0.5
    cf    = k "cf" 2800
    rq    = k "rq" 0.999
    lmt   = k "lmt" 1
    mfp   = k "mfp" 1
    t_tr0 = tr_control "t_tr0" 1
    k n v = control KR n v `lag` 0.2

synth_rec01 :: UGen
synth_rec01 = diskOut (control IR "bufn" 0) (in' 2 AR 0)

-- | All 'Synthdef's starting with /synth_/ in this module.
synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)

-- --------------------------------------------------------------------------
--
-- * Miscellaneous
--
-- --------------------------------------------------------------------------

-- | Query buffer values.
--
-- >>> let vs = [0,0,1,0, 1,0,1,0.5, 0,0,1,0, 0.5,1,0.5,1] :: [Float]
-- >>> let b  = 100 :: Int
-- >>> withSC3 $ \fd -> async fd (b_free b)
-- >>> withSC3 $ \fd -> send fd (b_alloc_setn1 b 0 (map realToFrac vs)
-- >>> vs' <- withSC3 $ queryBuffer b
-- >>> vs' == vs
-- True
--
queryBuffer ::
    Transport t
    => Int -- ^ Buffer number.
    -> t
    -> IO [Float]
queryBuffer bufn fd = do
    send fd $ b_query [bufn]
    (_:Int32 buflen:_) <- waitDatum fd "/b_info"
    send fd $ b_getn bufn [(0,fromIntegral buflen)]
    (_:_:_:vals) <- waitDatum fd "/b_setn"
    return $ catMaybes $ map d_get vals
