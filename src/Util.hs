module Util where

import           Reanimate (pathify, svgGlyphs, mkGroup, withFillOpacity, Signal
                          , Effect)

fromToRangeS :: (Double, Double) -> (Double, Double) -> Signal
fromToRangeS (lmin, lmax) (rmin, rmax) t
  | ans <= rmin = rmin
  | ans >= rmax = rmax
  | otherwise = ans
  where
    ans = (t - lmin) * ((rmax - rmin) / (lmax - lmin))

staggerE :: Effect -> Effect
staggerE effect d t svg = mkGroup
  $ zipWith
    (\range -> effect avg_duration (fromToRangeS range (0, avg_duration) t))
    (zip <*> tail $ [0, avg_duration .. d])
    (map (\(f, _, s) -> withFillOpacity 0 $ pathify $ f s) svgs)
  where
    avg_duration = d / fromIntegral (length svgs)

    svgs = svgGlyphs svg

realRange :: (Fractional p, Enum p) => p -> p -> [p]
realRange from to = [from, from + 0.02 .. to]
