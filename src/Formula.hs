{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NumericUnderscores #-}

module Formula where

import           Reanimate.Animation (Animation, Sync(..))
import           Reanimate.Scene (scene, oNew, oDraw, oShowWith, oFadeOut
                                , oHideWith, oHide, oModifyS, oTranslateX
                                , oTweenS, oGrow, oScaleIn, oScaleOut)
import           Reanimate
import           Environment
import           Util (midPoint, frameAt', fromToRangeS, fromToRangeS'
                     , replicateT3, mkWave, staggerE)
import           Reanimate.Builtin.Documentation (drawBox)
import           Control.Monad (replicateM, zipWithM_)
import           Data.Foldable (forM_)
import qualified Data.Text as T
import           NeatInterpolation as T
import           Graphics.SvgTree (ElementRef(Ref))
import           Control.Lens ((+=))
import           Reanimate.LaTeX (latexCfg, biolinum)
import           Numeric (showFFloat, showEFloat)

showSwipe :: T.Text -> Animation
showSwipe text =
  let svg = withStrokeWidth 0.001 $ latexCfg biolinum text
      (xStart, yStart, width, height) = boundingBox svg
  in scene
       do
         progress <- newVar 0
         line <- oNew
           (let start = (xStart - 0.2, yStart - 0.2)
                end = (xStart - 0.2, yStart + height + 0.2)
            in mkGroup
                 [ mkLine (midPoint start end) start
                 , mkLine (midPoint start end) end])
         oShowWith
           line
           (\svg -> mkAnimation 1 \t -> partialSvg t $ pathify svg)
         fork
           $ oTweenS
             line
             2
             \t -> do
               oTranslateX += (t * (width + 1))
         wait 0.4
         mask <- newSprite
           do
             progress' <- unVar progress
             pure
               $ mkLinePathClosed
                 [ (xStart - 1, yStart - 1)
                 , (xStart - 1, yStart + height + 1)
                 , ((xStart + width + 1) * progress', yStart + height + 1)
                 , ((xStart + width + 1) * progress', yStart - 1)]
         spriteMap
           mask
           (\s -> mkGroup
              [mkClipPath "maskClip" [s], withClipPathRef (Ref "maskClip") svg])
         tweenVar progress 2 (const id)
         oHideWith
           line
           (\svg -> reverseA $ mkAnimation 0.3 \t -> partialSvg t $ pathify svg)

formulaTri :: Animation
formulaTri = scene
  do
    let triangleEnv = newSpriteA' SyncFreeze
          . setDuration 0.7
          . signalA (curveS 2)
          . oDraw
          . withStrokeWidth 0.01
          . withFillOpacity 0
    let [triVert1, triVert2, triVert3] = [(-2, -2), (0, 2), (2, -2)]
    speedOfLight <- triangleEnv
      $ mkLinePathClosed
        [midPoint triVert1 triVert2, triVert2, midPoint triVert2 triVert3]
    waveLength <- triangleEnv
      $ mkLinePathClosed
        [ triVert1
        , midPoint triVert1 triVert2
        , (0, 0)
        , midPoint triVert1 triVert3]
    waveFreq <- triangleEnv
      $ mkLinePathClosed
        [ triVert3
        , midPoint triVert2 triVert3
        , (0, 0)
        , midPoint triVert1 triVert3]
    let text f text t = aroundCenter
          \s -> mkGroup
            [ frameAt' (curveS 2 t)
              $ oDraw
              $ withStrokeOpacity (fromToRangeS 0 0.1 0 1 t)
              $ f
              $ withStrokeWidth 0.005
              $ center
              $ latex text
            , s]
    fork $ spriteTween speedOfLight 1 (translate 0 (-0.1) `text` "c")
    fork $ spriteTween waveLength 1 (translate 0.1 0 `text` "$\\lambda$")
    spriteTween waveFreq 1 (translate (-0.1) 0 `text` "$\\nu$")
    let tri = [waveFreq, waveLength, speedOfLight]
    waveFreqShine <- newVar 0
    waveLenShine <- newVar 0
    speedOfLightShine <- newVar 0
    let triShines = [waveFreqShine, waveLenShine, speedOfLightShine]
    zipWithM_ (\s v -> applyVar v s withFillOpacity) tri triShines
    forM_
      tri
      \formulas -> do
        fork $ spriteTween formulas 1 (\(curveS 2 -> t) -> scale (1 - (t / 2)))
        fork
          $ spriteTween
            formulas
            1
            (\(curveS 2 -> t)
             -> translate (-6 * cubicBezierS (0, 0.8, 0.8, 1) t) (2.5 * t))
    wait 1
    tweenVar speedOfLightShine 0.4 (const $ powerS 2)
    spriteScope
      do
        title <- newSpriteA' SyncFreeze
          $ mapA (translate (-3) 2.2)
          $ signalA (curveS 2)
          $ adjustDuration (/ 2)
          $ showSwipe "Speed of Light $(c)$"
        body <- oNew
          (withStrokeWidth 0
           $ scale 0.5
           $ center
           $ latex
           $ T.unlines
             [ "A constant value\n"
             , "Equal to: $299,792,548 \\frac{m}{s}$ in a vacuum"])
        oShowWith body oScaleIn
        wait 3
        spriteTween title 1 (\t -> translate 0 (curveS 2 t * 5))
        oHideWith body oScaleOut
    fork $ tweenVar speedOfLightShine 0.4 (const $ reverseS . powerS 2)
    fork $ tweenVar waveLenShine 0.4 (const $ powerS 2)
    tweenVar waveFreqShine 0.4 (const $ powerS 2)
    spriteScope
      do
        title <- newSpriteA' SyncFreeze
          $ mapA (scale 0.8 . translate (-2.7) 2.7)
          $ signalA (curveS 2)
          $ adjustDuration (/ 2)
          $ showSwipe "Wave Length $(\\lambda)$\n\nFrequency $(\\nu)$"
        frequency <- newVar 2
        body <- newSprite
          do
            freq <- (\d -> if d <= 2
                           then "\\nu"
                           else T.pack $ showFFloat (Just 3) d "")
              <$> unVar frequency
            waveLen
              <- (\d -> if d <= 2
                        then "\\lambda"
                        else T.pack $ showEFloat (Just 3) (299_792_548 / d) "")
              <$> unVar frequency
            pure
                $ withStrokeWidth 0
                  $ translate 2 1
                  $ center
                  $ latex [T.text|$ ${waveLen} = \frac{c}{${freq}}$|]
        spriteTween body 1 (partialSvg . curveS 2)
        wave <- newSprite
          do
            frequency' <- unVar frequency
            pure
              $ withFillOpacity 0
              $ withStrokeWidth 0.1
              $ withStrokeOpacity 1
              $ withStrokeColorPixel blue
              $ translate 0 (-1.5)
              $ mkLinePath
              $ mkWave 1.5 0 frequency'
        spriteTween wave 2 (partialSvg . curveS 2)
        wait 1
        tweenVar frequency 5 (\d (curveS 2 -> t) -> d + (t * 5))
        wait 1
        tweenVar frequency 5 (\d (curveS 2 -> t) -> d - (t * 2))
        wait 3
        spriteTween body 1 (partialSvg . reverseS . curveS 2)
        spriteTween wave 2 (partialSvg . reverseS . curveS 2)
        spriteTween title 1 (\t -> translate 0 (curveS 2 t * 5))
    fork $ tweenVar waveLenShine 0.4 (const $ reverseS . powerS 2)
    tweenVar waveFreqShine 0.4 (const $ reverseS . powerS 2)
    forM_ tri \s -> do
      fork $ spriteTween s 1 (partialSvg . reverseS . curveS 2)

formulaIntro :: Animation
formulaIntro = scene
  do
    title <- oNew (withStrokeWidth 0.002 $ center $ latex "Wave Formulas")
    oShowWith title oDraw
    box <- adjustZ (+ 1)
      $ newSpriteA' SyncFreeze
      $ signalA (curveS 2)
      $ mkAnimation
        2
        \t -> withStrokeWidth 0.5
        $ translate 0 (-0.2 / t)
        $ rotate (360 * cubicBezierS (0, 0.9, 0.93, 1) t)
        $ withStrokeColorPixel blue
        $ withFillColorPixel darkBlue
        $ mkRect (t * 20) (t * 20)
    oHide title
    spriteTween box 1 (\(curveS 2 -> t) -> translate 0 (t * 20))

formula :: Animation
formula = env $ scene do
  adjustZ (+1) $ fork $ play formulaIntro
  wait (duration formulaIntro - 2)
  play formulaTri
