{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}

module Interference where

import           Reanimate
import           Reanimate.Scene
import           Reanimate.Builtin.Documentation
import           Environment
import           Util
import           Codec.Picture
import qualified Data.Text as T
import           Numeric (showFFloat)
import Reanimate.Animation
import Control.Applicative
import Reanimate.LaTeX (biolinum, latexCfg)
import Control.Lens
import Graphics.SvgTree

interferenceIntro :: Animation
interferenceIntro = scene
  do
    title <- oNew
      $ center
      $ withFillColorPixel white
      $ withStrokeWidth 0.01
      $ latex "Wave Interference"
    oShowWith title (signalA (curveS 2) . adjustDuration (/2) . oDraw)
    wait 1
    exitSquare <- oNew $ withStrokeColorPixel blue $ withStrokeWidth 0.5 $  withFillColorPixel darkBlue $ center $ mkRect 20 12
    oShowWith exitSquare (signalA (curveS 2) . setDuration 1 . oScaleIn)


interference :: Animation
interference = env
  $ scene
    do
      intro <- newSpriteA' SyncFreeze interferenceIntro
      spriteZ intro 3
      fork $ wait 0.5 >> spriteTween intro 1 (translate 0 . (*13) . curveS 2)
      wavePhase1 <- newVar 0
      wavePhase2 <- newVar 0
      let wave1 = mkWave' (-8.1) 8 0.5 <$> unVar wavePhase1 <*> pure 2
      let wave2 = mkWave' (-8.1) 8 0.5 <$> unVar wavePhase2 <*> pure 2
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
      let mkWaveLine s = pathify $ mkGroup [waveEnv s, waveArrows s]
      waveSvg1 <- newSprite $ withStrokeColorPixel blue . mkWaveLine <$> wave1
      waveSvg2 <- newSprite $ translate 0 2 . withStrokeColorPixel red . mkWaveLine <$> wave2
      waveGradient <- newSprite
        do
          waveSum' <- map snd <$> liftA2 (zipWith (+)) wave1 wave2
          pure
            $ translate 0 (-3)
            $ scaleXY 1 0.2
            $ showColorMap
              (\v -> (\(floor . (* 255) . (*2) . abs . subtract 0.5 -> d) -> PixelRGB8 d d d)
               $ (/ 2)
               $ (+ 1)
               $ waveSum' !! floor (fromIntegral (length waveSum' - 1) * v))
      fork $ spriteTween waveSvg1 2 (partialSvg . curveS 2)
      let mkTitle t = oNew $ translate 0 3.5 $ center $ withStrokeWidth 0.01 $ latexCfg biolinum t
      let mkSubtitle t = oNew $ translate 0 (-1.5) $ scale 0.6 $ center $ withStrokeWidth 0.01 $ latexCfg biolinum t
      spriteTween waveSvg2 2 (partialSvg . curveS 2)

      fork $ spriteTween waveGradient 2 ((`scaleXY` 1) . curveS 4)
      wait 1

      addPos <- newVar 0.2
      let waveLen = 1.571
      title <- mkTitle "Constructive Interference"
      oShowWith title (signalA (curveS 2) . setDuration 1.5 . oDraw)
      subtitle <- mkSubtitle "Waves reinforce each other"
      oShowWith subtitle (signalA (curveS 2) . setDuration 2 . oScaleIn)
      wait 1
      addition <- newSprite do
        getArr <- (\v x -> x !! floor (fromIntegral (length x) * v)) <$> unVar addPos
        wave1 <- wave1
        wave2 <- map (+(0,2)) <$> wave2
        pure $ mkGroup [pathify 
                        $ withMarkers ( Just (triangleMarker' "r" & markerElements %~ map (rotate 180))
                                    , Nothing
                                    , Just (triangleMarker' "l")) 
                        $ mkLine (getArr wave1) (getArr wave2)
                       , translate (-0.6) (-1) $ withStrokeWidth 0.01 $ translate (fst $ getArr wave1) 0 $ scale 0.5 
                         $ latex $ T.pack $ showFFloat (Just 3) (snd $ getArr wave1) "" 
                       , translate 0.4 (-0.1) $ withStrokeWidth 0.01 $ translate (fst $ getArr wave1) 1 $ scale 0.5 
                         $ latex $ T.pack $ ("(+) = " ++) $ showFFloat (Just 3) (snd (getArr wave1) + snd (getArr wave2) - 2) "" 
                       , translate (-0.6) 2.6 $ withStrokeWidth 0.01 $ translate (fst $ getArr wave2) 0 $ scale 0.5 
                         $ latex $ T.pack $ showFFloat (Just 3) (snd (getArr wave2) - 2) "" 
                       ]
      spriteTween addition 2 (partialSvg . curveS 2)
      wait 1
      wait 1
      tweenVar addPos 4 (\v t -> v + (0.5 * curveS 2 t))
      wait 1
      oHideWith title oScaleOut
      oHideWith subtitle oScaleOut
      title <- mkTitle "Destructive Interference"
      oShowWith title (signalA (curveS 2) . setDuration 1.5 . oDraw)
      subtitle <- mkSubtitle "Waves cancel each other"
      oShowWith subtitle (signalA (curveS 2) . setDuration 2 . oScaleIn)
      wait 0.5
      tweenVar wavePhase1 2 (\d t -> d + (curveS 2 t * waveLen)) 
      wait 1
      tweenVar addPos 4 (\v t -> v - (0.5 * curveS 2 t))
      wait 1
