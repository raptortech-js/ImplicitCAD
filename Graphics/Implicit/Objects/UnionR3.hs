module Graphics.Implicit.Objects.UnionR3 (unionR3) where

import Prelude

import Graphics.Implicit.Definitions
import Graphics.Implicit.Objects.Util
import Graphics.Implicit.MathUtil

--import Graphics.Implicit.Export.MySymbolic2 (MySymbolic2)
import Graphics.Implicit.Export.MySymbolicObj3

unionR3 :: ℝ -> [MySymbolicObj3] -> MySymbolicObj3
unionR3 rounding objs = MySymbolicObj3 implicit box Nothing scad "a union"
    where
        implicit :: Obj3
        implicit p = rminimum rounding $ fmap ($p) $ implicit3 <$> objs 

        box :: Box3
        box = outsetBox rounding ((left, bottom, inward), (right, top, out))
            where
                boxes = fmap box3 objs
                (leftbottom, topright) = unzip $ filter (not . isEmpty) boxes
                (lefts, bottoms, ins) = unzip3 leftbottom
                (rights, tops, outs) = unzip3 topright
                left = minimum lefts
                bottom = minimum bottoms
                inward = minimum ins
                right = maximum rights
                top = maximum tops
                out = maximum outs

        scad = undefined

