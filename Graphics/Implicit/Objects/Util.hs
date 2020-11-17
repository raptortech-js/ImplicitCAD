module Graphics.Implicit.Objects.Util where

import Prelude

import Graphics.Implicit.Definitions

import Data.VectorSpace ((^-^), (^+^))

-- | An empty box.
emptyBox :: Box3
emptyBox = ((0,0,0), (0,0,0))

-- | Is a Box3 empty?
-- | Really, this checks if it is one dimensional, which is good enough.
isEmpty :: (Eq a2, Eq a1, Eq a) =>
           ((a, a1, a2), (a, a1, a2)) -> Bool
isEmpty ((a,b,c),(d,e,f)) = a==d || b==e || c==f


-- | Increase a boxes size by a rounding value.
outsetBox :: ℝ -> Box3 -> Box3
outsetBox r (a,b) =
    (a ^-^ (r,r,r), b ^+^ (r,r,r))

errorBuilder = undefined

infiniteBox = ((-infinity, -infinity, -infinity), (infinity, infinity, infinity))
    where
        infinity :: (Fractional t) => t
        infinity = undefined
