module Graphics.Implicit.Export.MySymbolicObj3
{-(
    MySymbolicObj3 (
        MySymbolicObj3,
        implicit3,
        box3,
        box3R,
        symbolicGetContour3,
        symbolicGetContourMesh3,
        scad3
    ),
    ExtrudeRMScale (C1, C2, Fn),
    toScaleFn,
    isScaleID
    )-}
    where

import Prelude(Either (Left, Right), Maybe, Show (show), Bool (True, False), String)

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, Obj3, Box3, BoxedObj3, Polyline, Polytri, TriangleMesh (TriangleMesh))

import Graphics.Implicit.Export.TextBuilderUtils (Builder)

import Control.Monad.Reader (Reader)

import Graphics.Implicit.Export.MySymbolicObj2 (MySymbolicObj2)

data MySymbolicObj3 = MySymbolicObj3
    { implicit3 :: Obj3
    , box3      :: Box3
--    , symbolicGetContour3 :: Maybe (ℝ -> [Polyline])
--    , symbolicGetContourMesh3 :: Maybe (ℝ -> [Polytri])
    , symbolicGetMesh :: Maybe (ℝ -> TriangleMesh)
    , myScad3 :: Reader ℝ Builder
    , myShow :: String
    }

instance Show MySymbolicObj3 where
    show x = myShow x

-- | A symbolic 3D format!
data SymbolicObj3 =
    -- Primitives
      Rect3R ℝ ℝ3 ℝ3 -- rounding, start, stop.
    | Sphere ℝ -- radius
    | Cylinder ℝ ℝ ℝ -- 
    -- (Rounded) CSG
    | Complement3 SymbolicObj3
    | UnionR3 ℝ [SymbolicObj3]
    | DifferenceR3 ℝ [SymbolicObj3]
    | IntersectR3 ℝ [SymbolicObj3]
    -- Simple transforms
    | Translate3 ℝ3 SymbolicObj3
    | Scale3 ℝ3 SymbolicObj3
    | Rotate3 ℝ3 SymbolicObj3
    | Rotate3V ℝ ℝ3 SymbolicObj3
    -- Boundary mods
    | Outset3 ℝ SymbolicObj3
    | Shell3 ℝ SymbolicObj3
    -- Misc
    | EmbedBoxedObj3 BoxedObj3
    -- 2D based
    | ExtrudeR ℝ MySymbolicObj2 ℝ
    | ExtrudeRotateR ℝ ℝ MySymbolicObj2 ℝ
    | ExtrudeRM
        ℝ                     -- rounding radius
        (Either ℝ (ℝ -> ℝ))   -- twist
        ExtrudeRMScale        -- scale
        (Either ℝ2 (ℝ -> ℝ2)) -- translate
        MySymbolicObj2          -- object to extrude
        (Either ℝ (ℝ2 -> ℝ))  -- height to extrude to
    | RotateExtrude
        ℝ                     -- Angle to sweep to
        (Maybe ℝ)             -- Loop or path (rounded corner)
        (Either ℝ2 (ℝ -> ℝ2)) -- translate
        (Either ℝ  (ℝ -> ℝ )) -- rotate
        MySymbolicObj2          -- object to extrude
    | ExtrudeOnEdgeOf MySymbolicObj2 MySymbolicObj2
    deriving Show

data ExtrudeRMScale =
      C1 ℝ                  -- constant ℝ
    | C2 ℝ2                 -- constant ℝ2
    | Fn (ℝ -> Either ℝ ℝ2) -- function mapping height to either ℝ or ℝ2
    deriving Show

toScaleFn :: ExtrudeRMScale -> ℝ -> ℝ2
toScaleFn (C1 s) _ = (s, s)
toScaleFn (C2 s) _ = s
toScaleFn (Fn f) z = case f z of
    Left s -> (s, s)
    Right s -> s

isScaleID :: ExtrudeRMScale -> Bool
isScaleID (C1 1) = True
isScaleID (C2 (1, 1)) = True
isScaleID _ = False
