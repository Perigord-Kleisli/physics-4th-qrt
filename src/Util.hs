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
tweenVarToVal var duration target =
  tweenVar var duration (\v t -> v + (target - v) * t)

tweenVarToValWithEase
  :: Var s Double -> Signal -> Duration -> Double -> Scene s ()
tweenVarToValWithEase var s duration target =
  tweenVar var duration (\v t -> v + (target - v) * s t)

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

mkWave' :: Double -> Double -> Double -> Double -> Double -> [(Double, Double)]
mkWave' start end amplitude phase frequency =
  [(x, amplitude * sin ((x + phase) * frequency)) | x <- realRange start end]

withMarkers :: (Maybe Marker, Maybe Marker, Maybe Marker) -> Tree -> Tree
withMarkers (mStart, mMid, mEnd) svg =
  let markerNames = (map . fmap)
        (\m
         -> fromMaybe (error "Define an attrId for the marker") (m ^. attrId))
        [mStart, mMid, mEnd]
      markers = mkDefinitions
        $ zipWith
          (\m n -> withId n $ svg & treeBranch .~ MarkerNode m)
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

-- |  The Blackbird combinator
-- Applies a function after a binary function.
-- @f .*. g = (\\x y -> f (g x y))@
--
--
-- >>> (Just .*. (+)) 2 3
-- Just 5
(.*.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*.) = (.) . (.)

-- | Subtract the absolute value with a clamp on 0
-- >>> shorten 1 (-2)
-- -1
-- >>> shorten 1 2
-- 1
-- >>> shorten 3 2
-- 0
shorten :: (Num a, Ord a) => a -> a -> a
shorten minuend subtrahend = case signum subtrahend of
  -1 -> min 0 (subtrahend + minuend)
  1  -> max 0 (subtrahend - minuend)
  _  -> 0

groupByDistance :: Double -> [(Double, Double)] -> [[(Double, Double)]]
groupByDistance dist = go []
  where
    go accum ((x1, y1):(x2, y2):xs) =
      if dist >= abs (x1 - x2) && dist >= abs (y1 - y2)
      then go ((x1, y1):accum) ((x2, y2):xs)
      else ((x1, y1):accum):go [] ((x2, y2):xs)
    go accum x = accum:[x]

frameAt' :: Time -> Animation -> SVG
frameAt' t a = frameAt (t * duration a) a

interp :: Num b => b -> (b, b) -> (b, b) -> (b, b)
interp prog (x1, y1) (x2, y2) = (x1 + (x2 - x1) * prog, y1 + (y2 - y1) * prog)

inDistance :: (Ord a, Num a) => a -> (a, a) -> (a, a) -> Bool
inDistance dist (x1, y1) (x2, y2) = dist >= abs (x1 - x2)
  && dist >= abs (y1 - y2)

mergeByDistance :: Double -> [(Double, Double)] -> [(Double, Double)]
mergeByDistance dist xs = map (\x -> sum x / fromIntegral (length x))
  $ filter (not . null)
  $ groupByDistance dist xs

mkTriangle :: Tree
mkTriangle = mkLinePath [(-2, -1), (0, 2), (2, -1)]

replicateT :: Applicative m => m a -> m (a, a)
replicateT m = (,) <$> m <*> m

replicateT3 :: Applicative m => m a -> m (a, a, a)
replicateT3 m = (,,) <$> m <*> m <*> m

midPoint :: Fractional a => (a, a) -> (a, a) -> (a, a)
midPoint x y = x + ((y - x) / 2)

onPercent :: (Fractional a, Enum a) => (a -> b -> c) -> [b] -> [c]
onPercent f xs = zipWith f [0, 1 / fromIntegral (length xs - 1) .. 1] xs

data ColorRampStop = Stop { stopPosition :: Double, stopValue :: Double }

colorRamp :: [ColorRampStop] -> Signal
colorRamp [] t = t
colorRamp [x] _ = stopValue x
colorRamp stops t = let (Stop stopPos1 stopVal1, Stop stopPos2 stopVal2) =
                          findBracketingStops t stops
                        t' = fromToRangeS' 0 1 stopPos1 stopPos2 t
                    in lerp stopVal1 stopVal2 t'

-- Find the two stops that bracket the current time
findBracketingStops
  :: Double -> [ColorRampStop] -> (ColorRampStop, ColorRampStop)
findBracketingStops t stops = case span ((< t) . stopPosition) stops of
  ([], x:_)      -> (x, x)
  (x@(_:_), [])  -> (last x, last x)
  (x@(_:_), y:_) -> (last x, y)
  ([], [])       -> error "No stops provided"

-- Linearly interpolate between two values
lerp :: Double -> Double -> Double -> Double
lerp a b t = a * (1 - t) + b * t

fromToRangeS' :: Double -> Double -> Double -> Double -> Signal
fromToRangeS' from1 to1 from2 to2 t =
  (t - from1) / (to1 - from1) * (to2 - from2) + from2
