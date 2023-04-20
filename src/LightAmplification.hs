{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module LightAmplification where

import           Reanimate
import           Util
import           Environment
import           Control.Arrow (Arrow(..), (***))
import           Control.Monad (join)
import           Control.Lens (Bifunctor(bimap))
import           Control.Lens.Getter
import           Graphics.SvgTree
import           Control.Lens.Lens
import           Control.Lens.Setter ((%~))
import qualified Data.Text as T
import           Reanimate.Scene
import           Data.List (foldl1')
import           Reanimate.Animation (Sync(SyncFreeze))
import           Reanimate.Transition
import           Reanimate.Builtin.Slide (slideLeftT)

lightAmplification :: Animation
lightAmplification = env
  $ scene
    do
      let waveLen = 1.571
      wavePhase <- newVar 0
      let wave = do
            wavePhase <- unVar wavePhase
            pure
              $ (\(l, r)
                 -> (l, zipWith (\(x, _) (_, y) -> (x, y)) l $ reverse r))
              $ splitAt (400 * 2) -- There are 400 values in a -8..8 length wave
              $ mkWave' (-16) 16 1 wavePhase 2
      showWaveL <- newVar 0
      showWaveR <- newVar 0
      let waveArrows showWave name col = mkGroup
            . onPercent
              (\v -> if v > showWave
                     then scale 0
                     else scale 1)
            . map
              (\(elemX, elemY) -> withStrokeColorPixel col
               $ withFillColorPixel col
               $ (if elemY < 0.2
                    && elemY > -0.2 -- Change marker to circle when nearing (0,0) for a smoother transition
                  then withMarkerEnd (mkMarker name (mkCircle 3))
                  else withMarkerEnd (triangleMarker' (name ++ "tri")))
               $ mkLine (elemX, 0) (elemX, 0.7 * elemY))
            . nth 20
      let mkWaveLine showWave name col s = partialSvg showWave
            $ pathify
            $ mkGroup
              [ withStrokeColorPixel col
                $ withStrokeWidth 0.1
                $ withFillOpacity 0
                $ mkLinePath s
              , waveArrows showWave name col s]
      waveSvg <- newSprite
        do
          wave <- wave
          showWaveL <- unVar showWaveL
          showWaveR <- unVar showWaveR
          pure
            $ translate 8 0
            $ (\(x, y) -> mkGroup
                 [ mkWaveLine showWaveR "+" purple
                   $ zipWith3
                     (\(_, y1) (_, y2) (x, _) -> (x, y1 + y2))
                     x
                     (reverse y)
                     y
                 , mkWaveLine showWaveL "l" blue x
                 , mkWaveLine showWaveR "r" red y])
            $ second reverse wave
      wait 1
      fork $ tweenVar wavePhase 20 (\d t -> d + (-t) * 10)
      tweenVar showWaveL 3 (+)
      tweenVar showWaveR 3 (+)

lightAmplificationLoop :: Animation
lightAmplificationLoop = env
  $ scene
    do
      let waveLen = 1.571
      wavePhase <- newVar 0
      let wave = do
            wavePhase <- unVar wavePhase
            pure
              $ (\(l, r)
                 -> (l, zipWith (\(x, _) (_, y) -> (x, y)) l $ reverse r))
              $ splitAt (400 * 2) -- There are 400 values in a -8..8 length wave
              $ mkWave' (-16) 16 1 wavePhase 2
      let waveArrows showWave name col = mkGroup
            . onPercent
              (\v -> if v > showWave
                     then scale 0
                     else scale 1)
            . map
              (\(elemX, elemY) -> withStrokeColorPixel col
               $ withFillColorPixel col
               $ (if elemY < 0.2
                    && elemY > -0.2 -- Change marker to circle when nearing (0,0) for a smoother transition
                  then withMarkerEnd (mkMarker name (mkCircle 3))
                  else withMarkerEnd (triangleMarker' (name ++ "tri")))
               $ mkLine (elemX, 0) (elemX, 0.7 * elemY))
            . nth 20
      let mkWaveLine showWave name col s = partialSvg showWave
            $ pathify
            $ mkGroup
              [ withStrokeColorPixel col
                $ withStrokeWidth 0.1
                $ withFillOpacity 0
                $ mkLinePath s
              , waveArrows showWave name col s]
      waveSvg <- newSprite
        do
          translate 8 0
            . (\(x, y) -> mkGroup
                 [ mkWaveLine 1 "+" purple
                   $ zipWith3
                     (\(_, y1) (_, y2) (x, _) -> (x, y1 + y2))
                     x
                     (reverse y)
                     y
                 , mkWaveLine 1 "l" blue x
                 , mkWaveLine 1 "r" red y])
            . second reverse
            <$> wave
      tweenVar wavePhase 2 (\d t -> d + (-t * waveLen * 2))

stimulatedEmmision :: Int -> Animation
stimulatedEmmision count = scene
  do
    phase <- newVar (-10)
    elecPos <- newVar 3
    rayCounts <- newVar count
    lightRay <- newSprite
      do
        phase <- unVar phase
        rayCounts <- unVar rayCounts
        pure
          $ mkGroup
            [translate 0 (3 - (fromIntegral offset - 1) * 0.4)
              $ withFillColorPixel effectYellow
              $ withMarkerEnd triangleMarker
              $ withStrokeColorPixel effectYellow
              $ withFillOpacity 0
              $ mkLinePath (mkWave' phase (phase + 2) 0.2 (0 * pi / 180) 10)
            | offset <- [1 .. rayCounts]]
    atom <- newSprite
      do
        elecPos <- unVar elecPos
        pure
          $ mkGroup
            [ withStrokeWidth 0.02
              $ withStrokeColorPixel gray
              $ withFillOpacity 0
              $ mkCircle 3
            , withStrokeWidth 0.02
              $ withStrokeColorPixel gray
              $ withFillOpacity 0
              $ mkCircle 2
            , withFillColorPixel gray $ mkCircle 0.5
            , translate 0 elecPos
              $ withFillColorPixel blue
              $ withStrokeOpacity 0
              $ mkCircle 0.2]
    fork
      do
        wait 1.3
        pop <- adjustZ (-5)
          $ oNew
          $ translate 0 3
          $ withStrokeColorPixel gray
          $ withFillColorPixel gray
          $ withStrokeWidth 0.01
          $ mkCircle 1
        fork
          do
            oShowWith
              pop
              (\v -> signalA (powerS 2)
               $ mkAnimation 0.5 \t -> aroundCenter (scale t) v)
            oHideWith pop (setDuration 0.3 . oFadeOut)
        wait 0.2
        modifyVar rayCounts (+ 1)
        tweenVar elecPos 0.5 (\v t -> v - curveS 2 t)
    tweenVar phase 4 (\v t -> v + (t * 18))

stimulatedEmmisionPump :: Int -> Animation
stimulatedEmmisionPump count = scene
  do
    phase <- newVar (-10)
    elecPos <- newVar 2
    rayCounts <- newVar count
    lightRay <- newSprite
      do
        phase <- unVar phase
        rayCounts <- unVar rayCounts
        pure
          $ mkGroup
            [translate 0 (3 - (fromIntegral offset - 1) * 0.4)
              $ withFillColorPixel effectYellow
              $ withMarkerEnd triangleMarker
              $ withStrokeColorPixel effectYellow
              $ withFillOpacity 0
              $ mkLinePath (mkWave' phase (phase + 2) 0.2 (0 * pi / 180) 10)
            | offset <- [1 .. rayCounts]]
    atom <- newSprite
      do
        elecPos <- unVar elecPos
        pure
          $ mkGroup
            [ withStrokeWidth 0.02
              $ withStrokeColorPixel gray
              $ withFillOpacity 0
              $ mkCircle 3
            , withStrokeWidth 0.02
              $ withStrokeColorPixel gray
              $ withFillOpacity 0
              $ mkCircle 2
            , withFillColorPixel gray $ mkCircle 0.5
            , translate 0 elecPos
              $ withFillColorPixel blue
              $ withStrokeOpacity 0
              $ mkCircle 0.2]
    fork
      do
        wait 0.5
        plus <- oNew
          $ withFillColorPixel darkBlue
          $ translate 0 (-0.2)
          $ scale 0.4
          $ center
          $ mkText "+"
        fork $ oShowWith plus oScaleIn
        pop' <- adjustZ (-5)
          $ oNew
          $ translate 0 2
          $ withStrokeColorPixel gray
          $ withFillColorPixel gray
          $ withStrokeWidth 0.01
          $ mkCircle 1
        fork
          do
            oShowWith
              pop'
              (\v -> signalA (powerS 2)
               $ mkAnimation 0.5 \t -> aroundCenter (scale t) v)
            oHideWith pop' (setDuration 0.3 . oFadeOut)
        tweenVar elecPos 0.5 (\v t -> v + curveS 2 t)
        oHideWith plus oScaleOut
        wait 0.5
        pop <- adjustZ (-5)
          $ oNew
          $ translate 0 3
          $ withStrokeColorPixel gray
          $ withFillColorPixel gray
          $ withStrokeWidth 0.01
          $ mkCircle 1
        fork
          do
            oShowWith
              pop
              (\v -> signalA (powerS 2)
               $ mkAnimation 0.5 \t -> aroundCenter (scale t) v)
            oHideWith pop (setDuration 0.3 . oFadeOut)
        modifyVar rayCounts (+ 1)
        tweenVar elecPos 0.5 (\v t -> v - curveS 2 t)
    tweenVar phase 4 (\v t -> v + (t * 18))

stimulatedAbsorption :: Int -> Animation
stimulatedAbsorption count = scene
  do
    phase <- newVar (-10)
    elecPos <- newVar 2
    rayCounts <- newVar count
    lightRay <- newSprite
      do
        phase <- unVar phase
        rayCounts <- unVar rayCounts
        pure
          $ mkGroup
            [translate 0 (3 - (fromIntegral offset - 1) * 0.4)
              $ withFillColorPixel effectYellow
              $ withMarkerEnd triangleMarker
              $ withStrokeColorPixel effectYellow
              $ withFillOpacity 0
              $ mkLinePath (mkWave' phase (phase + 2) 0.2 (0 * pi / 180) 10)
            | offset <- [1 .. rayCounts]]
    atom <- newSprite
      do
        elecPos <- unVar elecPos
        pure
          $ mkGroup
            [ withStrokeWidth 0.02
              $ withStrokeColorPixel gray
              $ withFillOpacity 0
              $ mkCircle 3
            , withStrokeWidth 0.02
              $ withStrokeColorPixel gray
              $ withFillOpacity 0
              $ mkCircle 2
            , withFillColorPixel gray $ mkCircle 0.5
            , translate 0 elecPos
              $ withFillColorPixel blue
              $ withStrokeOpacity 0
              $ mkCircle 0.2]
    fork
      do
        wait 1.3
        pop <- adjustZ (-5)
          $ oNew
          $ translate 0 2
          $ withStrokeColorPixel gray
          $ withFillColorPixel gray
          $ withStrokeWidth 0.01
          $ mkCircle 1
        fork
          do
            oShowWith
              pop
              (\v -> signalA (powerS 2)
               $ mkAnimation 0.5 \t -> aroundCenter (scale t) v)
            oHideWith pop (setDuration 0.3 . oFadeOut)
        wait 0.2
        modifyVar rayCounts (subtract 1)
        tweenVar elecPos 0.5 (\v t -> v + curveS 2 t)
    tweenVar phase 4 (\v t -> v + (t * 18))

laser :: Animation
laser = env
  $ chainT
    (overlapT 0.2 (signalA (curveS 2) .*. slideLeftT))
    [ stimulatedEmmision 2
    , stimulatedAbsorption 3
    , stimulatedEmmisionPump 2
    , stimulatedAbsorption 3
    ]
