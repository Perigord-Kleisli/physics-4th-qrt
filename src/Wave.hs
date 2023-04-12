{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Wave  where

import           Codec.Picture
import           Control.Lens
import           Data.Complex
import           Graphics.SvgTree
import           Linear.V2
import           Reanimate
import           Reanimate.Scene (oDraw, oNew, oShowWith, oModify, oEasing
                                , oFadeOut, oHideWith)
import           Data.Text (pack)
import           Reanimate.Animation
import           Environment
import           Control.Monad
import           Util
import           Control.Applicative

waveSvg :: Float -> Float -> Tree
waveSvg = undefined

mkWave :: Double -> Double -> Double -> [(Double, Double)]
mkWave amplitude phase frequency =
  [(x, amplitude * sin (x * amplitude + phase)) | x <- [0, 0.01 ..]]

nth :: Int -> [a] -> [a]
nth _ [] = []
nth n xs = let (x,xs') = splitAt n xs in head x : nth n xs'

-- mkArrow :: (Double,Double) -> Tree
-- mkArrow = mkLine ()
wave :: IO ()
wave = reanimate
  $ scene
    do
      _ <- newSpriteSVG background
      _ <- newSpriteSVG bgGrid
      phase <- newVar 0
      amplitude <- newVar 0
      frequency <- newVar 0
      _ <- do
        newSprite
          $ liftA3 (,,) (unVar phase) (unVar amplitude) (unVar frequency)
          <&> \(phase', amplitude', frequency')
          -> let waveVals = [(x, amplitude' * sin (x * frequency' + phase'))
                            | x <- realRange (-8.0) 8.0]
             in mkGroup
                  (withStrokeColorPixel
                     white
                     (withStrokeWidth 0.1
                      $ withStrokeOpacity 1
                      $ withFillOpacity 0
                      $ mkLinePath waveVals)
                   :map
                     (\elem -> withStrokeWidth 0.1
                      $ withStrokeOpacity 1
                      $ withStrokeColorPixel white
                      $ mkLine (fst elem, 0) elem)
                     (nth 20 waveVals))
      fork $ tweenVar phase 4 (\x t -> (t * 20) + x)
      fork $ tweenVar amplitude 2 (\x (curveS 2 -> t) -> (t * 3) + x)
      tweenVar frequency 2 (\x (curveS 2 -> t) -> ((t * 3) + x) / 2)
      wait 1
