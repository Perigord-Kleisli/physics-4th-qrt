module Util where

import           Reanimate (pathify, svgGlyphs, mkGroup, withFillOpacity, Signal
                          , Effect, Var, Duration, Scene, tweenVar, fromToS
                          , withId, mkDefinitions, mkLine)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter))
import           Text.ParserCombinators.ReadP
import           Control.Lens
import           Reanimate.Scene (readVar)
import           Graphics.SvgTree
import           Data.Maybe
import           Reanimate.Animation
import           GHC.Read (list)
import           Reanimate.Svg

-- | Given @fromToRangeS fromMin fromMax toMin toMax@:
-- Remaps the range in @[fromMin..fromMax]@ to fit the range in @[toMin..toMax]@
--
--  Example:
-- >>> map (fromToRangeS 0 5 0 1) [0,1,2,3,4,5]
-- [0.0,0.2,0.4,0.6000000000000001,0.8,1.0]
-- >>> map (fromToRangeS 0 1 0 5) [0.0,0.2,0.4,0.6000000000000001,0.8,1.0]
-- [0.0,1.0,2.0,3.0000000000000004,4.0,5.0]
fromToRangeS :: Double -> Double -> Double -> Double -> Signal
fromToRangeS lmin lmax rmin rmax t
  | ans <= rmin = rmin
  | ans >= rmax = rmax
  | otherwise = ans
  where
    ans = (t - lmin) * ((rmax - rmin) / (lmax - lmin))

-- | apply an effect to each child in parallel
staggerE :: Effect -> Effect
staggerE effect d t svg = mkGroup
  $ zipWith
    (\(rangeL, rangeR)
     -> effect avg_duration (fromToRangeS rangeL rangeR 0 avg_duration t))
    (zip <*> tail $ [0, avg_duration .. d])
    (map (\(f, _, s) -> withFillOpacity 0 $ pathify $ f s) svgs)
  where
    avg_duration = d / fromIntegral (length svgs)
    svgs = svgGlyphs svg

-- | Cut off a number after going past a specific value
-- >>> clamp 0 2 5
-- 2
-- >>> clamp 0 2 (-3)
-- 0
clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp min max val
  | val < min = min
  | val > max = max
  | otherwise = val

-- | change a Double value with across a duration
tweenVarToVal :: Var s Double -> Duration -> Double -> Scene s ()
tweenVarToVal var duration target = tweenVar
  var
  duration
  (\v t -> v + (target - v) * t)

tweenVarToValWithEase
  :: Var s Double -> Signal -> Duration -> Double -> Scene s ()
tweenVarToValWithEase var s duration target = tweenVar
  var
  duration
  (\v t -> v + (target - v) * s t)

-- | Tween with the time value be in a 0..1 range
tweenNormalized :: Var s a -> Duration -> (a -> Time -> a) -> Scene s ()
tweenNormalized var duration f =
  tweenVar var duration (\x t -> f x (fromToRangeS 0 duration 0 1 t))

-- | Make a double range with a predefined step
realRange :: (Fractional p, Enum p) => p -> p -> [p]
realRange from to = [from, from + 0.02 .. to]

withStrokeCap :: Cap -> Tree -> Tree
withStrokeCap = (strokeLineCap ?~)

namedList :: QuasiQuoter
namedList = QuasiQuoter parseList undefined undefined undefined
  where
    parseList :: String -> Q Exp
    parseList str = case head
      $ readP_to_S
        (sepBy (many1 get <* skipSpaces) (char ',' <* skipSpaces) <* eof)
        str of
        (names, "") -> return
          $ ListE
          $ map
            (\name -> TupE
               [Just (LitE (StringL name)), Just (VarE (mkName name))])
            names
        _           -> error ""

mkWave :: Double -> Double -> Double -> [(Double, Double)]
mkWave amplitude phase frequency =
  [(x, amplitude * sin ((x + phase) * frequency)) | x <- realRange (-8) 8]

withMarkers :: (Maybe Marker, Maybe Marker, Maybe Marker) -> Tree -> Tree
withMarkers (mStart, mMid, mEnd) svg =
  let markerNames = (map . fmap)
        (\m
         -> fromMaybe (error "Define an attrId for the marker") (m ^. attrId))
        [mStart, mMid, mEnd]
      markers = mkDefinitions
        $ zipWith
          (\m n -> withId n $ defaultSvg & treeBranch .~ MarkerNode m)
          (catMaybes [mStart, mMid, mEnd])
          (catMaybes markerNames)
  in mkGroup
       [ markers
       , svg
         & markerStart .~ (mStart >>= fmap Ref . (^. attrId))
         & markerMid .~ (mMid >>= fmap Ref . (^. attrId))
         & markerEnd .~ (mEnd >>= fmap Ref . (^. attrId))]

withMarkerStart :: Marker -> Tree -> Tree
withMarkerStart m = withMarkers (Just m, Nothing, Nothing)

withMarkerMid :: Marker -> Tree -> Tree
withMarkerMid m = withMarkers (Nothing, Just m, Nothing)

withMarkerEnd :: Marker -> Tree -> Tree
withMarkerEnd m = withMarkers (Nothing, Nothing, Just m)

lineWithMarker :: Marker -> (Double, Double) -> (Double, Double) -> Tree
lineWithMarker marker x y = withMarkerEnd marker $ mkLine x y

-- | Take only every `nth` value from a list
-- >>> nth 5 [0..20]
-- [0,5,10,15,20]
nth :: Int -> [a] -> [a]
nth _ [] = []
nth n xs = let (x, xs') = splitAt n xs
           in head x:nth n xs'

mkMarker :: String -> SVG -> Marker
mkMarker name svg = defaultSvg
  & attrId ?~ name
  & markerOrient ?~ OrientationAuto
  & markerViewBox ?~ (-5, -5, 10, 10)
  & markerElements .~ [svg]