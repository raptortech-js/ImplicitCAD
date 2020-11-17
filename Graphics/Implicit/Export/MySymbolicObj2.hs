module Graphics.Implicit.Export.MySymbolicObj2 where

import Prelude(fmap, ($), (-), (/), (+), (>), (*), reverse, cos, pi, sin, max, ceiling, (<$>), Show, show, String)

import Graphics.Implicit.Definitions (ℝ, ℝ2, Fastℕ, Obj2, Box2, BoxedObj2, Polyline(Polyline), Polytri(Polytri), Triangle, TriangleMesh (TriangleMesh), (⋯*), fromFastℕtoℝ)

import Graphics.Implicit.Export.TextBuilderUtils (Builder)

import Control.Monad.Reader (Reader)

import Data.Maybe (Maybe)

data MySymbolicObj2 = MySymbolicObj2
    { implicit2 :: Obj2
    , box2      :: Box2
    , box2R     :: Maybe (ℝ -> Box2)
    , symbolicGetContour :: Maybe (ℝ -> [Polyline])
    , symbolicGetContourMesh :: Maybe (ℝ -> [Polytri])
    , myScad2 :: Reader ℝ Builder
    , myShow :: String
    }

instance Show MySymbolicObj2 where
    show x = myShow x

-- | A symbolic 2D object format.
--   We want to have symbolic objects so that we can
--   accelerate rendering & give ideal meshes for simple
--   cases.
data SymbolicObj2 =
    -- Primitives
      RectR ℝ ℝ2 ℝ2   -- rounding, start, stop.
    | Circle ℝ        -- radius.
    | PolygonR ℝ [ℝ2] -- rounding, points.
    -- (Rounded) CSG
    | Complement2 SymbolicObj2
    | UnionR2 ℝ [SymbolicObj2]
    | DifferenceR2 ℝ [SymbolicObj2]
    | IntersectR2 ℝ [SymbolicObj2]
    -- Simple transforms
    | Translate2 ℝ2 SymbolicObj2
    | Scale2 ℝ2 SymbolicObj2
    | Rotate2 ℝ SymbolicObj2
    -- Boundary mods
    | Outset2 ℝ SymbolicObj2
    | Shell2 ℝ SymbolicObj2
    -- Misc
    | EmbedBoxedObj2 BoxedObj2
    deriving Show

