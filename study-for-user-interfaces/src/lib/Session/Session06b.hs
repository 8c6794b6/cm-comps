module Session.Session06b where

import Session.Synthdefs
import Sound.Study.ForUserInterfaces.TUI.TUI02

main :: IO ()
main = withSC3 (runSettings g2)

withSC3 :: Connection TCP a -> IO a
withSC3 = withTransport (openTCP "127.0.0.1" 57111)

sendSynthdefs :: IO ()
sendSynthdefs = withSC3 (mapM_ (async . d_recv) synthdefs)

g0 :: Transport m => Track m ()
g0 = do
  offset 8
  track 0 $ do
    track 1 $
      track 10 $ do
        track 100 $
          source "sin01" (return ())
        track 101 $
          source "sin02" (return ())
    track 2 (return ())

g1 :: Transport m => Track m ()
g1 = do
    offset 8
    track 1 $
        track 10 $
            -- XXX: Nested third level leads to duplication.
            track 100 $
                source "sin02" $
                    -- XXX: Assigning multiple parameters leads to duplication.
                    param "amp" (curveTo EnvLin 8 0.1)
                    -- param "freq" (curveTo EnvLin 1e-9 2200)
            -- track 101 (return ())
            -- track 102 (return ())
    -- track 2 $ return ()

g2 :: Transport m => Track m ()
g2 =
  do offset 8
     track 1
       (track 10
         (do track 100
              (source "metro"
                 (do param "out" (Dval metroOut)
                     param "count" (Dval countOut)))
             track 101
              (do source "sin01"
                   (do param "amp" (curveTo EnvLin 8 0.3)
                       param "freq" (curveTo EnvLin 8 440)
                       param "pan" (lfSaw KR 5 0)
                       param "t_tr" (dust 'd' KR 8))
                  router
                   (do param "amp" (curveTo EnvCub 3 0.3)
                       param "in" (Dval (fromIntegral (audioBus 101)))
                       param "out" (Dval (sourceOut 101))))
             track masterNid
              (router
                (do param "in" (constant (audioBus (routerNid masterNid)))
                    param "amp" (curveTo EnvLin 8 1)))))
     track 2 (return ())

run :: Transport m => Track m () -> m ()
run t = do
    (_,st) <- runSourceBuilder t
    liftIO (mapM_ (putStrLn . drawSCNode) (tsSourceNB st []))
    let msgs = tsMessages st []
    liftIO (case msgs  of
                [] -> putStrLn "empty list"
                _  -> mapM_ (putStrLn . messagePP) msgs)
