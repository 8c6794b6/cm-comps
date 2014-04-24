{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Textual user interface, take 1.

Managing routed control bus signal sequences with demand UGens.

-}
module Sound.Study.ForUserInterfaces.TUI01 where

import Control.Applicative ((<$>))
import Data.Unique (newUnique, hashUnique)
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


-- | Sample synth controlled by mapping control rate bus with signals from
-- demand ugen.
synth_tuis01 :: UGen
synth_tuis01 = out (control KR "out" 0) $ osig
  where
    osig  = rlpf (saw AR (mce [freq,freq*1.001])) cf rq  * 0.1 * amp
    rq    = control KR "rq" 0.1
    cf    = linExp (control KR "cf" 0.5 + 0.001) 0.001 1 20 20000 `lag` lagt
    freq  = control KR "freq" 440 `lag` lagt
    lagt  = linExp (control KR "lagt" 0 + 0.001) 0.001 1.001 0.001 1
    amp   = control KR "amp" 0

-- | Percussive synth with 'resonz'ated 'whiteNoise'.
synth_tuis02 :: UGen
synth_tuis02 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = mix (resonz (whiteNoise 'w' AR) (mce [2232,3123,4502]) 0.3 * aenv)
    aenv  = decay t_tr0 0.2
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 0.3

-- | Percussive synth with 'sineOsc' and 'saw'.
synth_tuis03 :: UGen
synth_tuis03 = out (control KR "out" 0) $ pan2 osig pan 1
  where
    osig  = (sig1 + sig2) * amp
    sig1  = mix (sinOsc AR (mce frq1s) 0 * aenv1 * 0.3)
    frq1s = map (*frq1) [1,1.423,2.87,4.12,5.83,7.32]
    aenv1 = envGen KR t_tr0 1 0 dur1 DoNothing $
            Envelope [0,1,0] [0.001, 0.999] [EnvCub] (Just 0) Nothing
    sig2  = saw AR frq2 * aenv2 * 0.2
    aenv2 = envGen KR t_tr0 1 0 dur2 DoNothing $
            Envelope [0,1,0.8,0] [0.01,0.98,0.01] [EnvCub] (Just 0) Nothing
    frq1  = linExp (control KR "frq1" 0.1 + 0.001) 0.001 1.001 20 80
    frq2  = linExp (control KR "frq2" 0.1 + 0.001) 0.001 1.001 200 2000
    dur1  = linExp (control KR "dur1" 0.2 + 0.001) 0.001 1.001 0.1 1
    dur2  = linExp (control KR "dur2" 0.2 + 0.001) 0.001 1.001 0.001 0.01
    amp   = control KR "amp" 0
    pan   = linLin (control KR "pan" 0.5) 0 1 (-1) 1
    t_tr0 = tr_control "t_tr0" 1

-- | Default mixer for new synth added to running scsynth server.
synth_tuie00 :: UGen
synth_tuie00 = out (control KR "out" 0) osig
  where
    osig = in' 2 AR (control KR "in" 0) * (control KR "amp" 1)

-- | Reverb effect with 'allpassN' and foldr.
--
-- Requires /out/ control for specifying audio rate output bus of 'replaceOut',
-- and /in/ control for specifying audio rate input bus.
--
synth_tuie01 :: UGen
synth_tuie01 = replaceOut (control KR "out" 0) osig
  where
    osig    = wsig * wet + isig * (1-wet)
    wsig    = foldr f isig (zipWith mce2 (rs "abcd") (rs "efgh"))
    f x acc = allpassN acc 0.1 x dcy
    rs      = map (\i -> rand i 0.001 0.05)
    isig    = in' 2 AR (control KR "in" 0)
    wet     = control KR "wet" 0
    dcy     = linExp (control KR "dcy" 0.2 + 0.001) 0.001 1.001 0.25 8

-- | Eq with 'rlpf' and 'rhpf'
synth_tuie02 :: UGen
synth_tuie02 = replaceOut (control KR "out" 0) osig
  where
    osig   = wsig * wet + isig * (1-wet)
    wsig   = rlpf isig0 lfreq rq
    isig0  = rhpf isig hfreq rq
    rq     = clip (control KR "rq" 0.5) 0.001 1
    isig   = in' 2 AR (control KR "in" 0)
    lfreq  = linExp (control KR "lfreq" 0 + 0.001) 0.001 1.001 20 17000
    hfreq  = linExp (control KR "hfreq" 0 + 0.001) 0.001 1.001 20 17000
    wet    = control KR "wet" 0

test_tuie02 :: IO ()
test_tuie02 = withSC3 $ do
    let foo = out 0 (sinOsc AR 440 0 * decay (impulse KR 1 0) 1 * 0.1)
    mapM_ async
         [ d_recv $ synthdef "tuie02" synth_tuie02
         , d_recv $ synthdef "foo" foo ]
    sendOSC $ bundle immediately
        [ s_new "tuie02" (-1) AddToTail 1 []
        , s_new "foo" (-1) AddBefore (-1) []
        ]

-- | Synth to synchronize demand ugens.
synth_metro :: UGen
synth_metro = out (control KR "out" 0) osig
  where
    osig = impulse KR (beat*bpm/60) 0
    bpm  = control KR "bpm" 120
    beat = control KR "beat" 4

-- | Sends synthdefs defined in this Haskell module.
sendSynthdefs :: IO ()
sendSynthdefs = withSC3 $ mapM_ (async . d_recv) synthdefs

-- | Synthdefs defined in this haskell module.
synthdefs :: [Synthdef]
synthdefs = $(synthdefGenerator)


-- --------------------------------------------------------------------------
--
-- * Nodes
--
-- --------------------------------------------------------------------------

metroOut :: Num a => a
metroOut = 128

-- | Sample node graph.
tree01 :: Nd
tree01 =
    grp 0
    [ grp 1
      [ -- Target group to add new group for synths, effects, and controls.
        grp 10
        [ grp 100
          [ syn "metro"
            ["out"*=metroOut,"bpm"*=120, "beat"*=4] ]
        , grp 101
          [ syn "tuis01" ["out"*=101]
          , syn "tuie00" ["in"*=101] ]
        , grp 103
          [ syn "tuis02" ["out"*=103]
          , syn "tuie00" ["in"*=103] ]
        , grp 105
          [ syn "tuis03" ["out"*=105]
          , syn "tuie00" ["in"*=105] ]]
        -- Target group to add new mixer for entire graph
      , grp 11
        [ syn "tuie00" [] ] ]
      -- Reserved group for e.g.; recording sounds.
    ,grp 2 [] ]


-- --------------------------------------------------------------------------
--
-- * Patterns with demand UGens
--
-- --------------------------------------------------------------------------

sampleSetup :: IO ()
sampleSetup = do
    audition tree01
    tuis01_freq_01
    tuis01_cf
    tuis02_t_tr
    tuis03_t_tr

-- | Send 'pat01' and map the output to freq input of tuis01.
tuis01_freq_01 :: IO ()
tuis01_freq_01 =
    sendSupply01 "tuis01" "freq" False $ fmap (midiCPS . (+50)) $
    sseq sinf
    [ srand 8
      [-12, 0, 2, 5, 7, 12]
    , sseq 1
      [0, 0, 7, 0, 0, 7, 0, 7]
    ]

tuis01_freq_02 :: IO ()
tuis01_freq_02 =
    sendSupply01 "tuis01" "freq" False $ fmap (midiCPS . (+62)) $
    sshuf sinf $ sseq 1
      [ sseq 1 [0, 7, 0, 12]
      , sseq 1 [0, 2, 7, 12]
      , sseq 1
        [0, srand 1 [-12,0,12], 7, srand 1 [-5, 7, 19]]]

tuis01_freq_03 :: IO ()
tuis01_freq_03 =
    sendSupply01 "tuis01" "freq" False $ fmap (midiCPS . (+38)) $
    sseq sinf
    [ 0, srand 1 [-10, 2, 14, 26], 7, srand 1 [-5, 12, 19] ]

tuis01_freq_04 :: IO ()
tuis01_freq_04 =
    sendSupply01 "tuis01" "freq" False $ fmap midiCPS $
    sseq sinf
    [srand 1
     [snil, sseq 1 [24,31,36,43,48,55]]
    ,sseq (siwhite sinf 2 5)
     [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
    ,srand (siwhite sinf 3 9)
     [74,75,77,79,81]]

tuis01_cf :: IO ()
tuis01_cf =
    sendSupply01 "tuis01" "cf" False $
    -- sseq 1 [0.9]
    -- sseq sinf (map (/8) [5,1,1,5, 1,1,5,1])
    -- sseq sinf [ sseries 64 0.62 0.005
    --           , sseries 64 0.94 (-0.005) ]
    -- srand sinf [ 0.5, 0.6, 0.7, 0.8, 0.9 ]
    srand sinf [ 0.45, 0.5, 0.6, 0.9, 0.95, 0.98 ]
    -- srand sinf (map (sval . constant) [0.5,0.52..0.9])

tuis01_comb :: IO ()
tuis01_comb = sendFx "tuis01" "tuie01"

tuis01_chrs :: IO ()
tuis01_chrs = sendFx "tuis01" "tuie02"

tuis02_t_tr :: IO ()
tuis02_t_tr =
    sendSupply01 "tuis02" "t_tr0" True $ fmap (/10) $

    -- srand sinf
    -- [ 4, sseq 3 [srand 1 [0, 0, 0, 2, 4]]
    -- , 8, sseq 7 [srand 1 [0, 0, 2, 4] ] ]

    -- sseq sinf [0]

    sseq sinf
    [ sseq 7 [ srand 1 [4,6]
             , 0
             , srand 1 [0,4]
             , srand 1 [0,1] ]
    , srand 4 (map (sval . constant) [2,4,6,10])
    ]

tuis03_t_tr :: IO ()
tuis03_t_tr =
    sendSupply01 "tuis03" "t_tr0" True $ fmap (/10) $
    sseq sinf [1, 0, 0, srand 1 [0,0,0,0,0,0,0,0,1]]

-- | Function to route 'Supply' to synth found in current running node graph,
-- take 1.
sendSupply01 ::
    String     -- ^ Name of synthdef.
    -> String  -- ^ Name of parameter.
    -> Bool    -- ^ True if making output signal as trigger.
    -> Supply  -- ^ Pattern to sequence
    -> IO ()
sendSupply01 sname pname isTrig sup = withSC3 $ do
    currentNode <- getRootNode
    case queryN (synthName ==? sname) currentNode of
        snth:[]  -> do
            let nid  = nodeId snth
                tr0  = control KR "tr" 0
                sdef = out (control KR "out" 0)
                       ((if isTrig then (* tr0) else id)
                        (demand tr0 0 (evalSupply sup (mkStdGen 0x123456))))
                pdefname = sname ++ "." ++ pname
                psynths = queryN (synthName ==? pdefname) currentNode
                (addAction,targetNid)
                    | psynthExist = (AddReplace, nodeId (head psynths))
                    | otherwise   = (AddBefore,  nid)
                psynthExist = not (null psynths)
            obus <- if psynthExist
                    then return $ paramValue $
                         head [p | p <- synthParams $ head psynths
                                 , paramName p == "out"
                                 ]
                    else liftIO (((+metroOut) . fromIntegral . hashUnique) <$>
                                 newUnique)
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

send_tree01 :: IO ()
send_tree01 = withSC3 $ patchNode (nodify tree01)


-- --------------------------------------------------------------------------
--
-- * Misc
--
-- --------------------------------------------------------------------------

instance Audible Nd where
    play = patchNode . nodify

play_speUGen :: IO ()
play_speUGen = audition speUGen

speUGen :: UGen
speUGen = out 0 sig
  where
    sig   = foldr f v (zipWith mce2 (mkRs "abcd") (mkRs "efgh"))
    f a b = allpassN b 0.1 a 4
    v     = rlpf (pulse AR (mce2 freq (freq*1.01)) bw) cf rq * amp
    rq    = lfdNoise3 'q' KR 1.110 * 0.498 + 0.5
    cf    = lfdNoise3 'n' KR 2.323 * 2000 + 2200
    bw    = lfdNoise3 'b' KR 0.1123 * 0.48 + 0.5
    mkRs  = map (\x -> rand x 0.001 0.1)
    freq  = midiCPS (demand tick 0 (evalSupply supSpe (mkStdGen 0x81aafad)))
    amp   = decay2 tick 5e-4 950e-3 * 0.2
    tick  = impulse KR (tfreq*12) 0
    tfreq = control KR "trate" 0.641025 -- 7.6923

supSpe :: Supply
supSpe =
  sseq sinf
  [srand 1
   [snil, sseq 1 [24,31,36,43,48,55]]
  ,sseq (siwhite sinf 2 5)
   [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
  ,srand (siwhite sinf 3 9)
   [74,75,77,79,81]]
