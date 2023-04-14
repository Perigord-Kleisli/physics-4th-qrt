{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Intro where

import           Codec.Picture ()
import           Control.Lens ((&), (<&>), (.~), (?~), (^.))
import           Data.Complex ()
import           Graphics.SvgTree
import           Linear.V2 ()
import           Reanimate
import           Reanimate.Scene (oDraw, oNew, oShowWith, oModify, oEasing
                                , oFadeOut, oHideWith, spriteMap, spriteTween
                                , spriteModify, renderSprite, spriteZ
                                , destroySprite, oGrow, oTweenS)
import           Data.Text (pack)
import           Reanimate.Animation (Sync(..))
import           Environment (background, bgGrid, white, env, yellow, blue
                            , effectYellow, darkBlue, gray)
import           Control.Monad ()
import           Util (realRange, tweenVarToVal, withStrokeCap, nth
                     , lineWithMarker, withMarkerEnd, withMarkerStart, mkMarker
                     , tweenVarToValWithEase, staggerE, clamp, mkWave)
import           Control.Applicative (liftA3)
import           Data.Maybe (fromMaybe)
import           Reanimate.Svg (scale, translate, pathify)
import           Data.Functor
import           Reanimate.Effect
import           Reanimate.LaTeX (latexCfg, biolinum)
import           GHC.Float (double2Float)
import           Control.Arrow (Arrow(..))

textIntro :: Animation
textIntro =
  let title = setDuration 2
        $ oDraw
          (translate 0 3
           $ withStrokeWidth 0
           $ center
           $ latexCfg biolinum "\\textbf{Electromagnetic Waves}")
      subtitle = setDuration 2
        $ oDraw
          (translate 0 (-3)
           $ withStrokeWidth 0
           $ center
           $ latexCfg biolinum "Nature and duality of Light")
  in title `andThen` subtitle

outro :: Scene s ()
outro = do
  square <- fork
    $ newSpriteA
    $ mapA
      (\s -> mkGroup
         [ translate 0 5
           $ withStrokeWidth 0
           $ withStrokeOpacity 0
           $ scale 1.5
           $ rotate 45
           $ withStrokeWidth 0.1
           $ withStrokeColorPixel blue
           $ withFillColorPixel darkBlue
           $ mkRect 1 1
         , translate 0 5
           $ rotate 45
           $ withStrokeWidth 0.1
           $ withStrokeColorPixel blue
           $ withFillColorPixel darkBlue
           $ mkRect 1 1
         , s])
    $ setDuration 0.5
    $ oDraw
    $ translate 0 5
    $ rotate 45
    $ withStrokeWidth 0.1
    $ withStrokeColorPixel blue
    $ withFillColorPixel darkBlue
    $ mkRect 1 1
  fork $ spriteTween square 0.8 (\(curveS 2 -> t) -> translate 0 (-t * 5))
  fork
    $ wait 0.5 >> spriteTween square 0.8 (\(curveS 2 -> t) -> rotate (t * 180))
  wait 0.7
    >> spriteTween
      square
      0.5
      (\(powerS 2 -> t) -> scale (clamp 1 100 (t * 20)))

intro :: Animation
intro = env
  $ scene
    do
      phase <- newVar 0
      amplitude <- newVar 0
      frequency <- newVar 0
      -- The numerical values of the wave
      let wave = mkWave <$> unVar amplitude <*> unVar phase <*> unVar frequency
      waveLinePre <- newSprite
        $ withStrokeOpacity 0 . withFillOpacity 0 . mkLinePath <$> wave
      waveLine <- newSprite
        $ withStrokeColorPixel blue
        . withStrokeOpacity 0
        . withFillOpacity 0
        . mkLinePath
        <$> wave
      spriteMap
        waveLinePre
        (withStrokeColorPixel effectYellow . withStrokeOpacity 1)
      fork $ spriteTween waveLinePre 2 (overBeginning 1 drawInE 2 . powerS 2)
      wait 0.1 >> spriteMap waveLine (withStrokeOpacity 1)
      fork $ wait 0.3 >> newSpriteA' SyncFreeze textIntro
      fork
        do
          wait 0.1
          fork $ tweenVarToValWithEase phase (powerS 2) 8 20
          fork $ tweenVarToValWithEase amplitude (curveS 2) 4 1.6
          tweenVarToVal frequency 4 2
      hideArrows <- newVar 1
      waveArrows <- newSprite
        $ (\hideArrows' -> mkGroup
           . onPercentOfList
             hideArrows'
             (withStrokeOpacity 0 . withFillOpacity 0)
           . map
             (\(elemX, elemY) -> withStrokeColorPixel white
              $ withFillColorPixel white
              $ (if elemY < 0.3
                   && elemY > -0.3 -- Change marker to circle when nearing (0,0) for a smoother transition
                 then withMarkerEnd (mkMarker "circle" (mkCircle 3))
                 else withMarkerEnd triangleMarker)
              $ mkLine (elemX, 0) (elemX, shorten 0.2 elemY))
           . nth 20)
        <$> unVar hideArrows
        <*> wave
      fork $ wait 2 >> tweenVarToVal hideArrows 2 0
      spriteTween waveLine 2 (overBeginning 1 drawInE 2 . powerS 2)
      wait 5 >> outro
      wait 1
      play
        $ playThenReverseA
        $ pauseAtEnd 0.5
        $ oDraw
          (withStrokeOpacity 1
           $ withStrokeWidth 0.01
           $ center
           $ latex "Group 2")
      wait 0.5
  where
    onPercentOfList :: Double -> (a -> a) -> [a] -> [a]
    onPercentOfList percent f xs =
      let (l, r) = splitAt (floor $ fromIntegral (length xs) * percent) xs
      in map f l ++ r

    shorten subtrahend minuend = case signum minuend of
      -1 -> min 0 (minuend + subtrahend)
      1  -> max 0 (minuend - subtrahend)
      _  -> 0

    triangleMarker :: Marker
    triangleMarker = mkMarker "triangle"
      $ scale 0.7
      $ center
      $ mkPathString "M 0 0 L 10 5 L 0 10 z"
