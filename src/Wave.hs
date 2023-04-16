{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Wave where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Codec.Picture.Types
import           Control.Lens ((&), (^.), (-=), (%~), bimap)
import           Control.Monad
import qualified Data.Text as T
import           Graphics.SvgTree
import           NeatInterpolation
import           System.Random
import           System.Random.Shuffle
import           Environment
import           Util
import           Reanimate.Povray (povraySlow)
import           Reanimate.Scene (oDraw, oNew, oShow, oShowWith, oModifyS
                                , oZIndex, oHide, oHideWith, oFadeOut, oGrow)
import           Reanimate.Animation (Sync(..))
import           Reanimate.LaTeX (latexCfg, biolinum)
import           Data.List.Split (divvy)
import           Control.Lens.At
import           Data.Maybe (mapMaybe)
import           Control.Arrow ((&&&), (***))

waveLine :: PixelRGBA8 -> Animation
waveLine color = envNoBg
  $ scene
    do
      amp <- newVar 0
      phase <- newVar 0
      freq <- newVar 0
      _ <- fork
        $ newSprite
          do
            amp' <- unVar amp
            phase' <- unVar phase
            freq' <- unVar freq
            pure
              $ withFillOpacity 1
              $ withFillColorPixel effectYellow
              $ withStrokeColorPixel yellow
              $ mkGroup
              $ map
                (\(elemX, elemY)
                 -> (if elemY < 0.3
                       && elemY > -0.3 -- Change marker to circle when nearing (0,0) for a smoother transition
                     then withMarkerEnd (mkMarker "circle" (mkCircle 3))
                     else withMarkerEnd triangleMarker)
                 $ mkLine (elemX, 0) (elemX, shorten 0.2 elemY))
              $ nth 20
              $ mkWave amp' phase' freq'
      _ <- newSprite
        do
          amp' <- unVar amp
          phase' <- unVar phase
          freq' <- unVar freq
          pure
            $ withFillOpacity 0
            $ withStrokeColorPixel color
            $ mkLinePath
            $ mkWave amp' phase' freq'
      wait 0.1
      fork $ tweenVarToVal phase 40 80
      fork $ tweenVarToValWithEase amp (curveS 2) 4 1.6
      tweenVarToVal freq 4 2
      pointShow <- newVar 0
      pointLoc <- newVar 1.5996
      newSprite_
        do
          show' <- unVar pointShow
          loc' <- unVar pointLoc
          amp' <- unVar amp
          phase' <- unVar phase
          freq' <- unVar freq
          pure
            $ withFillOpacity 1
            $ withStrokeWidth 0
            $ mkGroup
            $ map
              (\(x, y)
               -> let pointOpacity = fromToRangeS 0 1 (min 1 (show' * 2)) 1
                        $ fromToRangeS (-8) 8 0 1 x
                  in mkGroup
                     $ withFillOpacity
                       (pointOpacity * show')
                       (translate x y (mkCircle 0.1))
                     :[translate x y
                        $ withStrokeWidth
                          (0.1 * (1 + negate $ fromToRangeS 0.6 0.9 0 1 show'))
                        $ withFillOpacity 0
                        $ mkCircle
                          ((* 0.8) $ powerS 2 $ fromToRangeS 0.6 0.9 0 1 show')
                      | show' >= 0.6 && show' <= 1])
            $ mergeByDistance 0.1
            $ filter
              (\(x, y) -> if 0 > loc'
                          then y <= loc'
                          else y >= loc')
            $ mkWave amp' phase' freq'
      let withTitle name' action = do
            name <- oNew
              $ translate (-7) 3
              $ withStrokeWidth 0.01
              $ latexCfg biolinum name'
            oShowWith name oDraw
            action
            oHideWith name oFadeOut
      let part name' loc' = do
            writeVar pointLoc loc'
            withTitle
              name'
              do
                tweenVar pointShow 1 (const (curveS 2))
                wait 2
                tweenVar pointShow 1 (const (reverseS . curveS 2))
      part "Crest" 1.5996
      part "Trough" (-1.5996)
      name <- oNew
        $ translate (-7) 3
        $ withStrokeWidth 0.01
        $ latexCfg biolinum "Wave Length ($\\lambda$)"
      oShowWith name oDraw
      showArrows <- newVar 0
      wavelens <- newSprite
        do
          amp' <- unVar amp
          phase' <- unVar phase
          freq' <- unVar freq
          showArrows' <- unVar showArrows
          pure
            $ withFillColorPixel white
            $ (if showArrows' < 0.3
               then withStrokeWidth
                 (0.1 * curveS 2 (fromToRangeS 0 0.3 0 1 showArrows'))
               else withFillOpacity 1 . withStrokeWidth 0.1)
            $ mkGroup
            $ uncurry (++)
            $ join
              bimap
              (mapMaybe
                 (\case
                    [x, y] -> Just
                      $ mkGroup
                        [ withMarkerStart
                            (triangleMarker' "tri1"
                             & markerElements %~ fmap (rotate 180))
                          $ mkLine
                            (interp showArrows' (midPoint x y) x)
                            (midPoint x y)
                        , withMarkerEnd (triangleMarker' "tri2")
                          $ mkLine
                            (midPoint x y)
                            (interp showArrows' (midPoint x y) y)]
                    _      -> Nothing)
               . divvy 2 2)
            $ (if 1 == (`mod` 2) (floor (phase' / pi - 0.348))
               then tail
               else id)
            *** (if 1 == (`mod` 2) (floor (phase' / pi - 0.848))
                 then tail
                 else id)
            $ join bimap (mergeByDistance 2)
            $ filter ((>= 1.5996) . snd) &&& filter ((<= -1.5996) . snd)
            $ mkWave' (-16) 16 amp' phase' freq'
      tweenVar showArrows 1 (const (curveS 2))
      wait 3
      fork $ tweenVar showArrows 1 (const (reverseS . curveS 2))
      oHideWith name oFadeOut
      withTitle
        "Amplitude"
        do
          showAmp <- newVar 0
          wavelens <- newSprite
            do
              showAmp' <- unVar showAmp
              amp' <- unVar amp
              phase' <- unVar phase
              freq' <- unVar freq
              pure
                $ withStrokeColorPixel white
                $ withFillOpacity 1
                $ withStrokeWidth 0.1
                $ withMarkerEnd (triangleMarker' "tri")
                $ mkGroup
                $ map
                  (\(elemX, elemY)
                   -> (if elemY < 0.3
                         && elemY > -0.3 -- Change marker to circle when nearing (0,0) for a smoother transition
                       then withMarkerEnd (mkMarker "circle" (mkCircle 3))
                       else withMarkerEnd (triangleMarker' "tri"))
                   $ mkLine (elemX, 0) (elemX, shorten 0.2 elemY * showAmp'))
                $ filter (\(x, y) -> y > 0)
                $ nth 20
                $ mkWave amp' phase' freq'
          tweenVar showAmp 1 (const (curveS 2))
          wait 2
          tweenVar showAmp 1 (const (reverseS . curveS 2))
          destroySprite wavelens

magneticWave :: Double -> T.Text
magneticWave t =
  T.pack $ svgAsPngFile $ frameAt (t * duration (waveLine red)) (waveLine red)

electricWave :: Double -> T.Text
electricWave t = T.pack
  $ svgAsPngFile
  $ frameAt (t * duration (waveLine blue)) (waveLine blue)

title :: Animation
title = oDraw
  $ translate (-7) 3
  $ withStrokeWidth 0.002
  $ withStrokeColorPixel effectYellow
  $ mkGroup
    [ withFillColorPixel red $ latexCfg biolinum "Magnetic Wave"
    , withFillColorPixel blue
      $ translate 0 (-1)
      $ latexCfg biolinum "Electric Wave"]

wave3D :: Animation
wave3D = env
  $ mkAnimation 40
  $ \t -> mkGroup
  $ ([frameAt (duration title * curveS 2 (fromToRangeS 0.8 0.9 0 1 t)) title] ++)
  $ pure
  $ translate (-0.4) (-1)
  $ scaleXY 1 0.7
  $ scale 1.3
  $ povraySlow []
  $ script
    (magneticWave t)
    (electricWave t)
    (curveS 2 $ fromToRangeS 0.8 0.9 0 1 t)
  where
    script magWave elecWave (T.pack . show . (* 2) -> cameraLoc) =
      [text| #include "colors.inc"
camera {
    location <${cameraLoc}, ${cameraLoc}, -10> // <x, y, z>
    right     x*image_width/image_height // keep propotions regardless of aspect ratio
    look_at  <0, 0,  0> // <x, y, z>
}

global_settings { ambient_light rgb<10, 10, 10> }

polygon {
    4 // Number of points
    <0, 0>, <0, 1>, <1, 1>, <1, 0> // point1, point2, point3, point4
    texture {
        pigment {
            image_map { png
"${magWave}"
            }
        }
    }
    translate <-1/2, -1/2,0> // <x, y, z>
    rotate <90, 0, 0> // <x°, y°, z°>
    scale 10
    scale <1.0, 1.0, 1.7> // <x, y, z>
}

polygon {
    4 // Number of points
    <0, 0>, <0, 1>, <1, 1>, <1, 0> // point1, point2, point3, point4
    texture {
        pigment {
            image_map { png
"${elecWave}"
            }
        }
    }
    translate <-1/2, -1/2,0> // <x, y, z>
    rotate <0, 0, 0> // <x°, y°, z°>
    scale 10
}
|]

textIntro :: Animation
textIntro = scene
  do
    title <- oNew
      (withStrokeWidth 0.002 $ center $ latex "Structure of a Wave")
    oShowWith title (signalA (curveS 2) . setDuration 1.3 . oDraw)
    oModifyS title (oZIndex -= 1)
    wait 1
    cover <- newVar 0
    unCover <- newVar 0
    covers <- newSprite
      do
        cover' <- (* 4.5) <$> unVar cover
        uncover' <- (* 5) <$> unVar unCover
        pure
          $ withStrokeWidth 0
          $ withFillColorPixel darkBlue
          $ mkGroup
            ([ mkLinePath
                 [(-8, uncover'), (-8, cover'), (8, cover'), (8, uncover')]
             , mkLinePath
                 [(-8, -uncover'), (-8, -cover'), (8, -cover'), (8, -uncover')]]
             ++ [withStrokeWidth 0.3
                  $ withStrokeColorPixel blue
                  $ mkLine (-8, uncover' * pole) (8, uncover' * pole)
                | uncover' > 0
                , pole <- [-1, 1]])
    cutLine <- fork
      do
        wait 0.1
        newSpriteA' SyncFreeze
          $ (signalA (powerS 4) . setDuration 0.5 . oDraw)
            (withStrokeWidth 0.3
             $ withStrokeColorPixel blue
             $ mkLine (-8.1, 0) (8, 0))
    _ <- fork
      do
        wait 1.25
        fork
          $ play
          $ (pauseAtEnd 0.1 . signalA (curveS 2) . setDuration 0.5 . oDraw)
            (withStrokeWidth 0.05
             $ withStrokeColorPixel white
             $ mkLine (0, 0) (8, 0))
        play
          $ (pauseAtEnd 0.1 . signalA (curveS 2) . setDuration 0.5 . oDraw)
            (withStrokeWidth 0.05
             $ withStrokeColorPixel white
             $ mkLine (0, 0) (-8, 0))
    tweenVar cover 0.5 (const $ powerS 4)
    wait 1
    oHide title
    destroySprite cutLine
    tweenVar unCover 2 (const $ curveS 4)
    wait 2

outro :: Animation
outro = signalA (curveS 2)
  $ mkAnimation
    2
    \t -> withStrokeWidth 1
    $ translate 0 (1 / t)
    $ withStrokeColorPixel blue
    $ withFillColorPixel darkBlue
    $ mkCircle (t * 11)

wave :: Animation
wave = env
  $ scene
    do
      adjustZ (+ 1) $ fork $ play textIntro
      wait 3
      play wave3D
      adjustZ (+ 1) $ play outro
