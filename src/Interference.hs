{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}

module Interference where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Environment
import           Util
import           Codec.Picture
import qualified Data.Text as T
import Numeric (showFFloat)

interference :: Animation
interference = env
  $ scene
    do
      wavePhase <- newVar 0
      wave1Freq <- newVar 2
      let wave1 = mkWave' (-8.1) 8 1 <$> unVar wavePhase <*> unVar wave1Freq
      let waveEnv = withStrokeWidth 0.1
            . withStrokeOpacity 1
            . withFillOpacity 0
            . mkLinePath
      let waveArrows = mkGroup
            . map
              (\(elemX, elemY) -> withFillColorPixel effectYellow
               $ (if elemY < 0.2
                    && elemY > -0.2 -- Change marker to circle when nearing (0,0) for a smoother transition
                  then withMarkerEnd (mkMarker "circle" (mkCircle 3))
                  else withMarkerEnd triangleMarker)
               $ withStrokeColorPixel effectYellow
               $ mkLine (elemX, 0) (elemX, shorten 0.2 elemY))
            . nth 20
      let mkWaveLine s = mkGroup [waveEnv s, waveArrows s]
      waveSvg <- newSprite $ withStrokeColorPixel blue . mkWaveLine <$> wave1
      waveGradient <- newSprite
        do
          wave1' <- map snd <$> wave1
          pure
            $ translate 0 (-3)
            $ scaleXY 1 0.2
            $ showColorMap
              (\v -> (\(floor . (*255)  -> d) -> PixelRGB8 d d d)
               $ (/2) $ (+1) $ wave1' !! floor (fromIntegral (length wave1' - 1) * v))
      tweenVar wavePhase 5 (\v t -> v + (t * 5))
      wait 10
