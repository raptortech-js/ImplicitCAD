module Graphics.Implicit.Objects.RotateExtrude (rotateExtrude2) where

import Prelude

import Debug.Trace

import qualified Data.Either as Either (either)
import Data.Maybe

import Graphics.Implicit.Definitions

import Graphics.Implicit.MathUtil
import Graphics.Implicit.Objects.Util

import Graphics.Implicit.Export.MySymbolicObj3
import Graphics.Implicit.Export.MySymbolicObj2


rotateExtrude2 totalRotation round translate rotate symbObj = MySymbolicObj3 implicit box Nothing errorBuilder "rotate extrude"
    where
        implicit = rotateExtrudeImplicit totalRotation round translate rotate symbObj
        -- Note: Assumes x2 is always greater than x1.
        -- FIXME: Insert the above assumption as an assertion in the type system?
        box = case translate of
            (Left (xshift,yshift)) ->
                let
                    ((_,y1),(x2,y2)) = box2 symbObj
                    r = max x2 (x2 + xshift)
                in
                    ((-r, -r, min y1 (y1 + yshift)),(r, r, max y2 (y2 + yshift)))
            otherwise -> undefined


rotateExtrudeImplicit :: ℝ            -- ^ Angle to sweep to (in rad)
    -> (Maybe ℝ)              -- ^ Loop or path (rounded corner)
    -> (Either ℝ2 (ℝ -> ℝ2))  -- ^ translate
    -> (Either ℝ  (ℝ -> ℝ ))  -- ^ rotate
    -> MySymbolicObj2           -- ^ object to extrude
    -> Obj3

rotateExtrudeImplicit totalRotation round translate rotate symbObj =
    let
        tau :: ℝ
        tau = 2 * pi
        k :: ℝ
        k   = tau / 360
        totalRotation' = totalRotation*k
        obj = implicit2 symbObj
        capped = isJust round
        round' = fromMaybe 0 round
        translate' :: ℝ -> ℝ2
        translate' = Either.either
                (\(a,b) θ -> (a*θ/totalRotation', b*θ/totalRotation'))
                (. (/k))
                translate
        rotate' :: ℝ -> ℝ
        rotate' = Either.either
                (\t θ -> t*θ/totalRotation' )
                (. (/k))
                rotate
        twists = case rotate of
                   Left 0  -> True
                   _       -> False
    in
        \(x,y,z) -> myMinimum $ do
            let
                r = sqrt $ x*x + y*y
                θ = atan2 y x
                ns :: [ℕ]
                ns =
                    if capped
                    then -- we will cap a different way, but want leeway to keep the function cont
                        [-1 .. ceiling $ (totalRotation' / tau) + 1]
                    else
                        [0 .. floor $ (totalRotation' - θ) / tau]
            n <- ns
            let
                θvirt = fromℕtoℝ n * tau + θ
                (rshift, zshift) = translate' θvirt
                twist = rotate' θvirt
                rz_pos = if twists
                        then let
                            (c,s) = (cos (twist*k), sin (twist*k))
                            (r',z') = (r-rshift, z-zshift)
                        in
                            (c*r' - s*z', c*z' + s*r')
                        else (r - rshift, z - zshift)
            pure $
              if capped
              then rmax round'
                    (abs (θvirt - (totalRotation' / 2)) - (totalRotation' / 2))
                    (obj rz_pos)
              else obj rz_pos

myMinimum :: [Double] -> Double
myMinimum xs = minimum xs 
