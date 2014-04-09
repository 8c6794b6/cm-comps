module Sound.Study.ForUserInterfaces.JS
    ( getFileName
    , bar
    ) where

import Control.Applicative ((<$>))
import Graphics.UI.Threepenny (Element, UI, ffi, callFunction)

-- | Get file name from \@input type=\"file\"@.
getFileName :: Element -> UI String
getFileName el = callFunction $ ffi "getFileName(%1)" el

-- | Example function to get sum from javascript.
-- Arguments in Haskell side are two 'Int's.
bar :: Int -> Int -> UI Int
bar x y = read <$> (callFunction $ ffi "bar(%1, %2)" x y)
