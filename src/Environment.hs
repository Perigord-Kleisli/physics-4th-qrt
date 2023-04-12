{-# LANGUAGE OverloadedStrings #-}

module Environment where

import           Codec.Picture (PixelRGBA8(..), Pixel8)
import           Graphics.SvgTree (Tree)
import           Reanimate (mkBackgroundPixel, gridLayout, withFillColorPixel)
import           Reanimate.Svg (gridLayout, mkBackgroundPixel, mkRect
                              , withFillOpacity, withStrokeColorPixel
                              , withStrokeOpacity, withStrokeWidth)
import           Data.String (IsString, fromString)

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
white = PixelRGBA8 0xF1 0xDe 0xDE 255

background :: Tree
background = mkBackgroundPixel darkBlue

colorScheme :: Tree
colorScheme = gridLayout
  [ [ withFillColorPixel red (mkRect 1 1)
    , withFillColorPixel blue (mkRect 1 1)
    , withFillColorPixel yellow (mkRect 1 1)
    , withFillColorPixel green (mkRect 1 1)]
  , [withFillColorPixel effectYellow (mkRect 1 1)]
  , [ withFillColorPixel white (mkRect 1 1)
    , withFillColorPixel darkBlue (mkRect 1 1)]]

bgGrid :: Tree
bgGrid = gridLayout
  $ replicate 9
  $ replicate 16
  $ withStrokeColorPixel white
  $ withStrokeWidth 0.005
  $ withStrokeOpacity 0.3
  $ withFillOpacity 0 (mkRect 1 1)
