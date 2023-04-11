{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ParallelListComp #-}

module Wave (wave) where

import           Codec.Picture
import           Control.Lens
import           Data.Complex
import           Graphics.SvgTree
import           Linear.V2
import           Reanimate
import           Reanimate.Scene (oDraw)
import           Data.Text (pack)
import           Reanimate.Animation

wave :: IO ()
wave = reanimate
  $ scene
    do
      _ <- newSpriteSVG (mkBackgroundPixel (PixelRGBA8 10 10 10 0xFF))
      width <- fork
        $ newSpriteA' SyncFreeze
        $ oDraw
        $ translate 0 2
        $ withFillColorPixel (PixelRGBA8 255 255 255 255)
        $ mkText (pack $ show screenWidth)
      height <- newSpriteA' SyncFreeze
        $ oDraw
        $ translate 0 (-2)
        $ withFillColorPixel (PixelRGBA8 255 255 255 255)
        $ mkText (pack $ show screenHeight)
      play
        $ setDuration 2
        $ oDraw
        $ withStrokeColorPixel (PixelRGBA8 0xFF 0xFF 0xFF 0xFF)
        $ withFillOpacity 0
        $ mkLinePath
          [(x, sin x) | x <- [-8,-7.9..8]]
