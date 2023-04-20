{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ElectroMagnetic where

import           Reanimate
import           Reanimate.Transition
import           Util
import           Environment
import           Reanimate.Animation (Sync(SyncLoop))
import           Data.Fixed (mod')
import           Reanimate.LaTeX (biolinum, latexCfg)
import           Reanimate.Scene
import           NeatInterpolation

radioWave t = scale t
  $ withStrokeWidth
    ((* 0.05) $ reverseS $ fromToRangeS' 0.5 1 0 1 t)
    (pathify $ withStrokeWidth 0.05 $ withFillOpacity 0 $ mkCircle 1)

radioWaves :: Int -> Animation
radioWaves count = foldr1 parA
  $ zipWith
    (\(fromIntegral -> x) y -> pause (x / fromIntegral count) `seqA` y)
    [1 ..]
    (replicate count (mkAnimation 1 radioWave))

oscilattingElectricField :: Animation
oscilattingElectricField = wave `parA` sphere
  where
    wave = mkAnimation
      1
      \t -> let y = t * 2 - 1
            in withStrokeColorPixel
                 red
                 (if y > 0
                  then radioWave $ abs y
                  else radioWave $ reverseS $ abs y)

    sphere = mkAnimation
      1
      \t -> let y = sin ((t * 360) * (pi / 180))
            in translate 0 y
               $ withStrokeWidth 0
               $ withFillColorPixel blue
               $ mkCircle 0.2

oscilattingMagneticField :: Animation
oscilattingMagneticField = line `parA` wave
  where
    wave = mkAnimation
      1
      \t -> let y = t * 2 - 1
            in withStrokeColorPixel
                 blue
                 (if y > 0
                  then radioWave $ abs y
                  else radioWave $ reverseS $ abs y)

    line = mkAnimation
      1
      \t -> mkGroup
        [ withStrokeColorPixel red $ mkLine (-1, 0) (1, 0)
        , translate (-1.35) 0
          $ mkGroup
          $ zipWith (\x -> translate (fromIntegral x * 0.7) 0) [1 ..]
          $ replicate 3
          $ withStrokeColorPixel effectYellow
          $ withFillColorPixel effectYellow
          $ scaleXY
            (if t > 0.5
             then -0.5
             else 0.5)
            1
          $ withMarkerEnd triangleMarker
          $ partialSvg 0.9
          $ pathify
          $ withFillOpacity 0
          $ rotate 110
          $ mkCircle 0.4]

electroMagnetic :: Animation
electroMagnetic = env
  $ scene
    do
      magnet <- fork $ newSpriteA' SyncLoop oscilattingMagneticField
      spriteE magnet (overBeginning 1 fadeInE)
      let mkDesc desc = do
            desc' <- oNew
              $ withStrokeWidth 0.01
              $ scale 0.5
              $ translate 0 (-4)
              $ center
              $ latexCfg biolinum [text|\begin{center}${desc}\end{center}|]
            oShowWith desc' (signalA (curveS 1) . adjustDuration (/ 3) . oDraw)
            wait 2
            oHideWith
              desc'
              (reverseA . signalA (curveS 2) . adjustDuration (/ 5) . oDraw)
      mkDesc
        [text|According to Maxwell's 3rd Equation: \\
              $\Delta \times E = -\frac{dB}{dt}$ \\
              "A changing magnetic field will always produce an electric field"|]
      spriteTween magnet 1 (\(curveS 2 -> t) -> translate (t * 3) 0)
      elec <- fork $ newSpriteA' SyncLoop oscilattingElectricField
      spriteE elec (overBeginning 1 fadeInE)
      mkDesc
        [text|Likewise, a changing electric field will always produce a magnetic field|]
      spriteTween elec 1 (\(curveS 2 -> t) -> translate (-t * 3) 0)
      elecmagnet <- fork
        $ newSpriteA'
          SyncLoop
          (oscilattingElectricField
           `parA` signalA (\t -> mod' (t + 0.25) 1) oscilattingMagneticField)
      spriteE elecmagnet (overBeginning 1 fadeInE)
      mkDesc
        [text|This creates a feedback loop of electric fields producing magnetic fields\\
              and vice versa, creating wave propagations thus the name: \\
              "\emph{Electromagnetic Radiation}"|]
      wait 2

electroMagneticLoop :: Animation
electroMagneticLoop = env
  $ scene
    do
      elec <- fork $ newSpriteA' SyncLoop oscilattingElectricField
      elecDesc <- oNew $ latexCfg biolinum ""
      spriteMap elec (translate (-3) 0)
      magnet <- fork $ newSpriteA' SyncLoop oscilattingMagneticField
      spriteMap magnet (translate 3 0)
      fork
        $ newSpriteA'
          SyncLoop
          (oscilattingElectricField
           `parA` signalA (\t -> mod' (t + 0.25) 1) oscilattingMagneticField)
      return ()

