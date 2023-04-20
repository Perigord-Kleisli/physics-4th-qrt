{-# LANGUAGE BlockArguments #-}

module DoubleSplit where

import           Reanimate
import           Codec.Picture
import           Debug.Trace (traceShowId)
import           Data.Fixed (mod')
import           Graphics.SvgTree.Types (ElementRef(Ref))

radialGradient :: Animation
radialGradient = mkAnimation 2 showColorMap

showColorMap :: Time -> SVG
showColorMap t = center $ scaleToSize screenWidth screenHeight $ embedImage img
  where
    resolution = 1
    width = 409*resolution

    height = 256*resolution

    img = generateImage pixelRenderer width height

    pixelRenderer x y = f
      (fromIntegral x / fromIntegral (height - 1))
      (fromIntegral y / fromIntegral (height - 1))

    f x y =
      let waveGen x' y = (\x -> (/4) $ sin ((360 * 10 * x - (t * 360)) * (pi / 180)))
            $ (x - x') ** 2 + (y - 0.7) ** 2
          x' = floor
            $ (* 255)
            $ (+0.5)
            $ if y < 0.2 && x > 0.2 && x < 1.4
              then waveGen 0.4 0.2 + waveGen 1.2 0.2
              else waveGen 0.4 y + waveGen 1.2 y
      in PixelRGB8 x' x' x'
