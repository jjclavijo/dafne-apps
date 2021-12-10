{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Leaflet where

import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as L

import Data.Text.Template

import Text.RawString.QQ

arrow :: String
arrow = [r|<svg width="${pxw}px" height="${pxh}px"
    viewBox="$vb1 $vb2 $vb3 $vb4"
    xmlns="http://www.w3.org/2000/svg" version="1.1">
  <path d= "M -2 0 L -2 $y1 L -7 $y2 L 0 $y3 L 7 $y4 L 2 $y5 L 2 0 L -2 0"
    stroke="black"
    stroke-width="1"
    fill="#69CC5B"
    fill-opacity="None"
    transform="rotate($angle)" />
</svg> |]


-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

svgArrow :: (Num a, Show a) => a -> a -> T.Text
svgArrow length ang = L.toStrict $ substitute (T.pack arrow) (arrowContext length ang)
  where
    arrowContext length ang = context [("pxw",tshow $ length * 40),
                                       ("pxh",tshow $ length * 40),
                                       ("vb1",tshow $ length * (-20) ),
                                       ("vb2",tshow $ length * (-20) ),
                                       ("vb3",tshow $ length * 40 ),
                                       ("vb4",tshow $ length * 40 ),
                                       ("y1",tshow $ length * (-20) + 10),
                                       ("y2",tshow $ length * (-20) + 15),
                                       ("y3",tshow $ length * (-20) ),
                                       ("y4",tshow $ length * (-20) + 15),
                                       ("y5",tshow $ length * (-20) + 10),
                                       ("angle",tshow ang)]
      where tshow = T.pack . show

-- For testing --
--main :: IO ()
--main = S.putStr . E.encodeUtf8 $ svgArrow 10 10
-- stack runghc --package template --package raw-strings-qq -- ./Leaflet.hs
