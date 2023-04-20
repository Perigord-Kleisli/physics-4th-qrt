{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

module WaveAddition where

import           Reanimate (Animation, mkLinePath, withStrokeWidth
                          , withStrokeOpacity, partialSvg, curveS
                          , withFillOpacity, mkGroup, mkAnimation
                          , withStrokeColorPixel, reverseS, reverseA)
import           Reanimate.Scene
import           Util (mkWave, nth, shorten, withMarkerEnd, mkMarker, mkWave'
                     , frameAt')
import           Reanimate.Svg (mkLine, mkCircle, translate, center
                              , withFillColorPixel, withFillColor)
import           Environment
import           Reanimate.LaTeX
import           Reanimate.Animation (Sync(..), mapA)
import qualified Data.Text as T
import           Numeric (showFFloat)
import           Control.Applicative
import           Control.Monad (forM_)

singleWaveAddition :: Animation
singleWaveAddition = env $ scene
  do
    wavePhase1 <- newVar 0
    wavePhase2 <- newVar 0
    waveFreq1 <- newVar 2
    waveFreq2 <- newVar 4
    waveAmp1 <- newVar 1
    waveAmp2 <- newVar 2
    let wave1 = mkWave' (-8.1) 8 <$> unVar waveAmp1
          <*> unVar wavePhase1
          <*> unVar waveFreq1
    let wave2 = mkWave' (-8.1) 8 <$> unVar waveAmp2
          <*> unVar wavePhase2
          <*> unVar waveFreq2
    let waveEnv = withStrokeWidth 0.01
          . withStrokeOpacity 0.5
          . withFillOpacity 0
          . mkLinePath
    let waveArrow n (elemX, elemY) =
          (if elemY < 0.1
             && elemY > -0.1 -- Change marker to circle when nearing (0,0) for a smoother transition
           then withMarkerEnd (mkMarker ("c" ++ n) (mkCircle 3))
           else withMarkerEnd (triangleMarker' ("t" ++ n)))
          $ mkLine (elemX, 0) (elemX, shorten 0.1 elemY)
    waveSprite1 <- newSprite $ waveEnv <$> wave1
    waveSprite2 <- newSprite $ waveEnv <$> wave2
    waveText1 <- newSprite
      $ translate 1.1 1
      . withFillColorPixel red
      . withStrokeWidth 0.001
      . latex
      . T.pack
      . (\x -> showFFloat (Just 3) x "")
      . snd
      . (!! 405)
      <$> wave1
    waveText2 <- newSprite
      $ mkGroup
      . (:[translate 0.2 0 $ withStrokeOpacity 0 $ latex "+"])
      . translate 1.1 0
      . withFillColorPixel blue
      . withStrokeWidth 0.001
      . latex
      . T.pack
      . (\x -> showFFloat (Just 3) x "")
      . snd
      . (!! 405)
      <$> wave2
    waveText3 <- newSprite
      $ translate 1.1 (-1)
      . mkGroup
      . (:[withStrokeWidth 0.05 $ mkLine (0, 0.8) (2.8, 0.8)])
      . withFillColorPixel white
      . withStrokeWidth 0.001
      . latex
      . T.pack
      . (\x -> showFFloat (Just 3) x "")
      . snd
      . (!! 405)
      <$> liftA2 (zipWith (+)) wave1 wave2
    waveArrow1 <- newSprite
      $ withStrokeColorPixel red
      . withFillColorPixel red
      . waveArrow "wv1"
      . (!! 405)
      <$> wave1
    waveArrow2 <- newSprite
      $ withStrokeColorPixel blue
      . withFillColorPixel blue
      . waveArrow "wv2"
      . (!! 405)
      <$> wave2
    waveSum <- adjustZ pred . newSprite
      $ (\x y -> withStrokeWidth 0.1 $ waveArrow "wvs" $ x !! 405 + y !! 405) <$> wave1 <*> wave2
    fork
      $ forM_
        [waveSprite1, waveSprite2]
        \s -> do
          spriteTween s 2 (\t s -> frameAt' t (oDraw s))
    fork
      $ wait 1
      >> forM_
        [waveArrow1, waveSum, waveArrow2]
        \s -> do
          spriteTween s 1.2 (\t s -> frameAt' t (oScaleIn s))
    forM_
      [waveText1, waveText2, waveText3]
      \s -> do
        spriteTween s 1 (\t s -> frameAt' t (oDraw s))
    wait 1
    tweenVar wavePhase1 2 (\v t -> v + curveS 2 t)
    wait 1
    tweenVar wavePhase2 2 (\v t -> v + curveS 2 t)
    fork $ tweenVar wavePhase1 5 (\v t -> v + curveS 2 (t*2))
    fork $ tweenVar wavePhase2 5 (\v t -> v + curveS 2 (t*2))
    tweenVar waveAmp1 1 (\v t -> v + curveS 2 t)
    tweenVar waveAmp2 1 (\v t -> v - curveS 2 (t/2))
    tweenVar waveFreq2 1 (\v t -> v + curveS 2 (t/2))
    tweenVar waveFreq2 1 (\v t -> v + curveS 2 (t*2))
    wait 3

waveAddition :: Animation
waveAddition = env $ scene
  $ do
    wavePhase <- newVar 0
    wave1Freq <- newVar 2
    let wave1 = mkWave' (-8.1) 8 0.6 <$> unVar wavePhase <*> unVar wave1Freq
    wave2Freq <- newVar 1
    let wave2 = mkWave' (-8.1) 8 0.7 <$> unVar wavePhase <*> unVar wave2Freq
    let waveEnv = withStrokeWidth 0.1
          . withStrokeOpacity 1
          . withFillOpacity 0
          . mkLinePath
    let waveArrows = mkGroup
          . map
            (\(elemX, elemY)
             -> (if elemY < 0.1
                   && elemY > -0.1 -- Change marker to circle when nearing (0,0) for a smoother transition
                 then withMarkerEnd (mkMarker "circle" (mkCircle 3))
                 else withMarkerEnd triangleMarker)
             $ mkLine (elemX, 0) (elemX, shorten 0.1 elemY))
          . nth 20
    let mkWaveLine s = mkGroup [waveEnv s, waveArrows s]
    wave1Svg <- newSprite
      $ withStrokeColorPixel blue . translate 0 3 . mkWaveLine <$> wave1
    wave2Svg <- newSprite
      $ withStrokeColorPixel red . translate 0 0 . mkWaveLine <$> wave2
    fork $ tweenVar wavePhase 24 (const (* 40))
    spriteTween
      wave1Svg
      2
      (\(curveS 2 -> t) -> withStrokeWidth (t * 0.1) . partialSvg t)
    spriteTween
      wave2Svg
      2
      (\(curveS 2 -> t) -> withStrokeWidth (t * 0.1) . partialSvg t)
    wait 1
    plus <- oNew
      (withStrokeOpacity 1
       $ withStrokeWidth 0.01
       $ translate 0 1.6
       $ center
       $ latex "$+$")
    oShowWith plus oDraw
    wave3Svg <- newSprite
      $ (\s1 s2 -> translate 0 (-2.5) $ mkWaveLine $ zipWith (+) s1 s2)
      <$> wave1
      <*> wave2
    newSpriteA' SyncFreeze (mkAnimation 2
                                        \((* 6) . curveS 2 -> t) -> translate 0 (-1)
                                                    $ mkLinePath [(-t, 0), (0, 0), (t, 0)])
    spriteTween wave3Svg 2 (\(curveS 2 -> t) -> withStrokeWidth (t * 0.1) . partialSvg t)

    wait 1
    tweenVar wave1Freq 2 (\x (curveS 2 -> t) -> x + (t * 2))
    wait 1
    tweenVar wave2Freq 1 (\x (curveS 2 -> t) -> x - (t * 0.5))
    wait 1
    tweenVar wave1Freq 1 (\x (curveS 2 -> t) -> x - (t * 3))
    wait 1
    tweenVar wave1Freq 2 (\x (curveS 2 -> t) -> x + (t * 3))
    wait 1
