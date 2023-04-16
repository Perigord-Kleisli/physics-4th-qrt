{-# LANGUAGE OverloadedStrings #-}

module Environment where

import           Codec.Picture (PixelRGBA8(..), Pixel8)
import           Graphics.SvgTree (Tree, Cap(..))
import           Reanimate (mkBackgroundPixel, gridLayout, withFillColorPixel
                          , Animation, mapA, center, rotate)
import           Reanimate.Svg (gridLayout, mkBackgroundPixel, mkRect
                              , withFillOpacity, withStrokeColorPixel
                              , withStrokeOpacity, withStrokeWidth, mkGroup
                              , scale, mkPathString)
import           Data.String (IsString, fromString)
import           Util (withStrokeCap, mkMarker)
import           Graphics.SvgTree.Types (Marker)

red :: PixelRGBA8
red = PixelRGBA8 223 41 53 255

blue :: PixelRGBA8
blue = PixelRGBA8 0x11 0x9D 0xA4 255

green :: PixelRGBA8
green = PixelRGBA8 0x96 0xF5 0x50 255

yellow :: PixelRGBA8
yellow = PixelRGBA8 0xFF 0xF9 0x4f 255

instance IsString PixelRGBA8 where
  fromString ['#', r1, r2, g1, g2, b1, b2, a1, a2] = PixelRGBA8
    (read $ "0x" ++ [r1, r2])
    (read $ "0x" ++ [g1, g2])
    (read $ "0x" ++ [b1, b2])
    (read $ "0x" ++ [a1, a2])
  fromString ['#', r1, r2, g1, g2, b1, b2] = PixelRGBA8
    (read $ "0x" ++ [r1, r2])
    (read $ "0x" ++ [g1, g2])
    (read $ "0x" ++ [b1, b2])
    255
  fromString [r1, r2, g1, g2, b1, b2, a1, a2] = PixelRGBA8
    (read $ "0x" ++ [r1, r2])
    (read $ "0x" ++ [g1, g2])
    (read $ "0x" ++ [b1, b2])
    (read $ "0x" ++ [a1, a2])
  fromString [r1, r2, g1, g2, b1, b2] = PixelRGBA8
    (read $ "0x" ++ [r1, r2])
    (read $ "0x" ++ [g1, g2])
    (read $ "0x" ++ [b1, b2])
    255
  fromString _ = error "Invalid Pixel String"

effectYellow :: PixelRGBA8
effectYellow = "#FFDA3F"

strongerYellow :: PixelRGBA8
strongerYellow = PixelRGBA8 241 211 2 255

darkBlue :: PixelRGBA8
darkBlue = PixelRGBA8 0x17 0x1D 0x26 255

white :: PixelRGBA8
white = "#F1DEDE"

gray :: PixelRGBA8
gray = "#7F8594"

background :: Tree
background = mkBackgroundPixel darkBlue

-- | For visualization purposes
colorScheme :: Tree
colorScheme = gridLayout
  [ [ withFillColorPixel red (mkRect 1 1)
    , withFillColorPixel blue (mkRect 1 1)
    , withFillColorPixel yellow (mkRect 1 1)
    , withFillColorPixel green (mkRect 1 1)]
  , [withFillColorPixel effectYellow (mkRect 1 1)]
  , [ withFillColorPixel white (mkRect 1 1)
    , withFillColorPixel gray (mkRect 1 1)
    , withFillColorPixel darkBlue (mkRect 1 1)]]

bgGrid :: Tree
bgGrid = gridLayout
  $ replicate 9
  $ replicate 16
  $ withStrokeColorPixel white
  $ withStrokeWidth 0.005
  $ withStrokeOpacity 0.3
  $ withFillOpacity 0 (mkRect 1 1)

env :: Animation -> Animation
env = mapA
  $ \svg -> mkGroup
    [ mkBackgroundPixel darkBlue
    , withFillOpacity 1
      $ withStrokeCap CapRound
      $ withStrokeWidth 0.1
      $ withFillColorPixel white
      $ withStrokeColorPixel white
      $ mkGroup [svg]]

envNoBg :: Animation -> Animation
envNoBg = mapA
  $ \svg -> mkGroup
    [ withFillOpacity 1
      $ withStrokeCap CapRound
      $ withStrokeWidth 0.1
      $ withFillColorPixel white
      $ withStrokeColorPixel white
      $ mkGroup [svg]]

triangleMarker :: Marker
triangleMarker = mkMarker "triangle"
  $ scale 0.7
  $ center
  $ mkPathString "M 0 0 L 10 5 L 0 10 z"

triangleMarker' :: String -> Marker
triangleMarker' name =
  mkMarker name $ scale 0.7 $ center $ mkPathString "M 0 0 L 10 5 L 0 10 z"
