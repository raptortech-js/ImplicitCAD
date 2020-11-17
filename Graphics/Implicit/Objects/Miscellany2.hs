module Graphics.Implicit.Objects.Miscellany2 where

import Prelude

import Debug.Trace

import Graphics.Implicit.Definitions
import Graphics.Implicit.MathUtil
import Graphics.Implicit.Export.MySymbolicObj2

import Data.VectorSpace ((^-^))
import Data.List (nub, genericIndex, genericLength)

errorBuilder = undefined

infiniteBox :: Box2
infiniteBox = ((-infinity, -infinity), (infinity, infinity))
    where
        infinity :: (Fractional t) => t
        --infinity = 1/0
        infinity = undefined

-- | Define a Box2 around all of the given points.
pointsBox :: [ℝ2] -> Box2
pointsBox points =
    let
        (xs, ys) = unzip points
    in
        ((minimum xs, minimum ys), (maximum xs, maximum ys))

polygonR2 rounding points = MySymbolicObj2 implicit box Nothing Nothing Nothing errorBuilder "polygon"
    where
        implicit :: Obj2
        implicit p =
            let
                pair :: ℕ -> (ℝ2,ℝ2)
                pair n = (points `genericIndex` n, points `genericIndex` mod (n + 1) (genericLength points) )
                pairs :: [(ℝ2,ℝ2)]
                pairs = [ pair n | n <- [0 .. genericLength points - 1] ]
                relativePairs =  fmap (\(a,b) -> (a ^-^ p, b ^-^ p) ) pairs
                crossing_points =
                    [x2 ^-^ y2*(x2-x1)/(y2-y1) | ((x1,y1), (x2,y2)) <-relativePairs,
                       ( (y2 <= 0) && (y1 >= 0) ) || ( (y2 >= 0) && (y1 <= 0) ) ]
                -- FIXME: use partition instead?
                seemsInRight = odd . length . filter (>0) $ nub crossing_points
                seemsInLeft = odd . length . filter (<0) $ nub crossing_points
                isIn = seemsInRight && seemsInLeft
                dists :: [ℝ]
                dists = fmap (distFromLineSeg p) pairs
            in
                minimum dists * if isIn then -1 else 1

        box :: Box2
        box = pointsBox points
