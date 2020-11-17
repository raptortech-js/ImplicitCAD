module Graphics.Implicit.Objects.Miscellany3 where

import Prelude

import qualified Data.Either as Either (either)
import Data.Maybe

import Graphics.Implicit.Definitions

import Graphics.Implicit.MathUtil
import Graphics.Implicit.Objects.Util

import Graphics.Implicit.Export.MySymbolicObj3
import Graphics.Implicit.Export.MySymbolicObj2


--complement3 = MySymbolicObj3 undefined infiniteBox Nothing errorBuilder "complement"
complement3 = undefined

intersectR3 rounding objs = MySymbolicObj3 implicit box Nothing errorBuilder "intersect"
    where
        implicit :: ℝ3 -> ℝ
        implicit p = rmaximum rounding $ fmap ($p) implicit3 <$> objs
        box =
            let
                boxes = fmap box3 objs
                (leftbot, topright) = unzip boxes
                (lefts, bots, ins) = unzip3 leftbot
                (rights, tops, outs) = unzip3 topright
                left = maximum lefts
                bot = maximum bots
                inward = maximum ins
                right = minimum rights
                top = minimum tops
                out = minimum outs
            in
                if   top   > bot
                  && right > left
                  && out   > inward
                then ((left,bot,inward),(right,top,out))
                else emptyBox

conicalFrustrum r1 r2 h = MySymbolicObj3 implicit box Nothing errorBuilder "conical frustrum"
    where
        implicit :: Obj3
        implicit (x, y, z) =
            let
                d = sqrt (x*x + y*y) - ((r2-r1)/h*z+r1)
                θ = atan2 (r2-r1) h
            in
                max (d * cos θ) (abs (z-h/2) - (h/2))
        box =
            let
                r = max r1 r2
            in
                ((-r, -r, 0), (r, r, h))


