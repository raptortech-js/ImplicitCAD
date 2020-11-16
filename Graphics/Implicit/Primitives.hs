-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: Required. why?
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- A module exporting all of the primitives, and some operations on them.
module Graphics.Implicit.Primitives (
                                     translate,
                                     scale,
                                     outset,
                                     complement, union, intersect, difference,
                                     unionR, intersectR, differenceR,
                                     shell,
                                     getBox,
                                     getImplicit,
                                     extrudeR,
                                     extrudeRM,
                                     extrudeRotateR,
                                     extrudeOnEdgeOf,
                                     sphere,
                                     rect3R,
                                     circle,
                                     cylinder,
                                     cylinder2,
                                     rectR,
                                     polygonR,
                                     rotateExtrude,
                                     rotate3,
                                     rotate3V,
                                     pack3,
                                     rotate,
                                     pack2,
                                     implicit,
                                     Object
                                    ) where

import Prelude(Maybe(Just, Nothing), Either, fmap, ($), undefined)

import Graphics.Implicit.Export.MySymbolicObj2 (MySymbolicObj2 (box2))
import Graphics.Implicit.Export.MySymbolicObj3 (MySymbolicObj3 (box3), ExtrudeRMScale)
import Graphics.Implicit.Objects.UnionR3 (unionR3)
import Graphics.Implicit.Objects.UnionR2 (unionR2)

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, Box2)
import Graphics.Implicit.MathUtil   (pack)

-- $ 3D Primitives

sphere ::
    ℝ                  -- ^ Radius of the sphere
    -> MySymbolicObj3    -- ^ Resulting sphere

sphere = undefined

-- | A rectangular prism, with rounded corners.
rect3R ::
    ℝ                 -- ^ Rounding of corners
    -> ℝ3             -- ^ Bottom.. corner
    -> ℝ3             -- ^ Top right... corner
    -> MySymbolicObj3   -- ^ Resuting cube - (0,0,0) is bottom left...

rect3R = undefined

-- | A conical frustum --- ie. a cylinder with different radii at either end.
cylinder2 ::
    ℝ                   -- ^ Radius of the cylinder
    -> ℝ                -- ^ Second radius of the cylinder
    -> ℝ                -- ^ Height of the cylinder
    -> MySymbolicObj3     -- ^ Resulting cylinder

cylinder2 r1 r2 h = undefined

cylinder ::
    ℝ                   -- ^ Radius of the cylinder
    -> ℝ                -- ^ Height of the cylinder
    -> MySymbolicObj3     -- ^ Resulting cylinder

cylinder r = undefined

-- $ 2D Primitives

circle ::
    ℝ               -- ^ radius of the circle
    -> MySymbolicObj2 -- ^ resulting circle

circle   = undefined

-- | A rectangle, with rounded corners.
rectR ::
    ℝ               -- ^ Rounding radius (in mm) of corners
    -> ℝ2           -- ^ Bottom left corner
    -> ℝ2           -- ^ Top right corner
    -> MySymbolicObj2 -- ^ Resulting square (bottom right = (0,0) )

rectR = undefined

-- | A 2D polygon, with rounded corners.
polygonR ::
    ℝ                -- ^ Rounding radius (in mm) of the polygon
    -> [ℝ2]          -- ^ Verticies of the polygon
    -> MySymbolicObj2  -- ^ Resulting polygon

polygonR = undefined

-- $ Shared Operations

-- | Operations available on both 2D and 3D objects. The obvious omission of
-- rotation operations from this class are a technical limitation, and are
-- instead provided by 'rotate' and 'rotate3'.
--
-- Library users shouldn't need to provide new instances of this class.
class Object obj vec | obj -> vec where

    -- | Complement an Object
    complement ::
        obj     -- ^ Object to complement
        -> obj  -- ^ Result

    -- | Rounded union
    unionR ::
        ℝ        -- ^ The radius (in mm) of rounding
        -> [obj] -- ^ objects to union
        -> obj   -- ^ Resulting object

    -- | Rounded difference
    differenceR ::
        ℝ        -- ^ The radius (in mm) of rounding
        -> [obj] -- ^ Objects to difference
        -> obj   -- ^ Resulting object

    -- | Rounded minimum
    intersectR ::
        ℝ        -- ^ The radius (in mm) of rounding
        -> [obj] -- ^ Objects to intersect
        -> obj   -- ^ Resulting object

    -- | Translate an object by a vector of appropriate dimension.
    translate ::
        vec      -- ^ Vector to translate by
        -> obj   -- ^ Object to translate
        -> obj   -- ^ Resulting object

    -- | Scale an object
    scale ::
        vec     -- ^ Amount to scale by
        -> obj  -- ^ Object to scale
        -> obj  -- ^ Resulting scaled object

    uniformScale ::
        ℝ
        -> obj
        -> obj

    -- | Outset of an object.
    outset ::
        ℝ        -- ^ distance to outset
        -> obj   -- ^ object to outset
        -> obj   -- ^ resulting object

    -- | Make a shell of an object.
    shell ::
        ℝ        -- ^ width of shell
        -> obj   -- ^ object to take shell of
        -> obj   -- ^ resulting shell

    -- | Get the bounding box an object
    getBox ::
        obj           -- ^ Object to get box of
        -> (vec, vec) -- ^ Bounding box

    -- | Get the implicit function for an object
    getImplicit ::
        obj           -- ^ Object to get implicit function of
        -> (vec -> ℝ) -- ^ Implicit function

    implicit ::
        (vec -> ℝ)     -- ^ Implicit function
        -> (vec, vec)  -- ^ Bounding box
        -> obj         -- ^ Resulting object


instance Object MySymbolicObj2 ℝ2 where
    scale       = undefined
    uniformScale= undefined
    complement  = undefined
    unionR      = unionR2
    intersectR  = undefined
    differenceR = undefined
    outset      = undefined
    shell       = undefined
    getBox      = undefined
    getImplicit = undefined
    implicit a b= undefined
    translate   = undefined

instance Object MySymbolicObj3 ℝ3 where
    scale       = undefined
    uniformScale= undefined
    complement  = undefined
    unionR      = unionR3
    intersectR  = undefined
    differenceR = undefined
    outset      = undefined
    shell       = undefined
    getBox      = undefined
    getImplicit = undefined
    implicit a b= undefined
    translate   = undefined

union :: Object obj vec => [obj] -> obj
union = unionR 0

difference :: Object obj vec => [obj] -> obj
difference = differenceR 0

intersect :: Object obj vec => [obj] -> obj
intersect = intersectR 0

-- 3D operations

-- | Extrude a 2d object upwards, with rounded corners.
extrudeR
    :: ℝ   -- ^ Rounding radius (in mm) of corners
    -> MySymbolicObj2
    -> ℝ   -- ^ Extrusion height
    -> MySymbolicObj3
extrudeR = undefined

-- | This function is not implemented
extrudeRotateR :: ℝ -> ℝ -> MySymbolicObj2 -> ℝ -> MySymbolicObj3
extrudeRotateR = undefined

extrudeRM :: ℝ              -- ^ rounding radius (in mm)
    -> Either ℝ (ℝ -> ℝ)    -- ^ twist
    -> ExtrudeRMScale       -- ^ scale
    -> Either ℝ2 (ℝ -> ℝ2)  -- ^ translate
    -> MySymbolicObj2         -- ^ object to extrude
    -> Either ℝ (ℝ2 -> ℝ)   -- ^ height to extrude to
    -> MySymbolicObj3
extrudeRM = undefined


rotateExtrude :: ℝ            -- ^ Angle to sweep to (in rad)
    -> (Maybe ℝ)              -- ^ Loop or path (rounded corner)
    -> (Either ℝ2 (ℝ -> ℝ2))  -- ^ translate
    -> (Either ℝ  (ℝ -> ℝ ))  -- ^ rotate
    -> MySymbolicObj2           -- ^ object to extrude
    -> MySymbolicObj3
rotateExtrude = undefined

extrudeOnEdgeOf :: MySymbolicObj2 -> MySymbolicObj2 -> MySymbolicObj3
extrudeOnEdgeOf = undefined

-- | Rotate a 3D object via an Euler angle, measured in radians, along the
-- world axis.
rotate3 :: ℝ3 -> MySymbolicObj3 -> MySymbolicObj3
rotate3 = undefined

-- | Rotate a 3D object along an arbitrary axis.
rotate3V
    :: ℝ   -- ^ Angle of rotation
    -> ℝ3  -- ^ Axis of rotation
    -> MySymbolicObj3
    -> MySymbolicObj3
rotate3V = undefined

-- FIXME: shouldn't this pack into a 3d area, or have a 3d equivalent?
-- | Attempt to pack multiple 3D objects into a fixed area. The @z@ coordinate
-- of each object is dropped, and the resulting packed objects will all be on
-- the same plane.
pack3
    :: ℝ2                  -- ^ Area to pack
    -> ℝ                   -- ^ Separation between objects
    -> [MySymbolicObj3]      -- ^ Objects to pack
    -> Maybe MySymbolicObj3  -- ^ 'Just' if the objects could be packed into the given area
pack3 (dx, dy) sep objs =
    let
        boxDropZ :: (ℝ3,ℝ3) -> (ℝ2,ℝ2)
        boxDropZ ((a,b,_),(d,e,_)) = ((a,b),(d,e))
        withBoxes :: [(Box2, MySymbolicObj3)]
        withBoxes = fmap (\obj -> ( boxDropZ $ box3 obj, obj)) objs
    in case pack ((0,0),(dx,dy)) sep withBoxes of
            (a, []) -> Just $ union $ fmap (\((x,y),obj) -> translate (x,y,0) obj) a
            _ -> Nothing

-- 2D operations

rotate :: ℝ -> MySymbolicObj2 -> MySymbolicObj2
rotate = undefined

-- | Attempt to pack multiple 2D objects into a fixed area.
pack2
    :: ℝ2                  -- ^ Area to pack
    -> ℝ                   -- ^ Separation between objects
    -> [MySymbolicObj2]      -- ^ Objects to pack
    -> Maybe MySymbolicObj2  -- ^ 'Just' if the objects could be packed into the given area
pack2 (dx, dy) sep objs =
    let
        withBoxes :: [(Box2, MySymbolicObj2)]
        withBoxes = fmap (\obj -> ( box2 obj, obj)) objs
    in case pack ((0,0),(dx,dy)) sep withBoxes of
            (a, []) -> Just $ union $ fmap (\((x,y),obj) -> translate (x,y) obj) a
            _ -> Nothing

