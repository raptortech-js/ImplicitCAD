-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: describe why we need this.
{-# LANGUAGE OverloadedStrings #-}

-- output SCAD code, AKA an implicitcad to openscad converter.
module Graphics.Implicit.Export.SymbolicFormats (scad2, scad3) where

import Prelude(Either(Left, Right), ($), (*), ($!), (-), (/), pi, error, (+), (==), take, floor, (&&), const, pure, (<>), sequenceA, (<$>))

import Graphics.Implicit.Definitions(ℝ)
import Graphics.Implicit.Export.TextBuilderUtils(Text, Builder, toLazyText, fromLazyText, bf)
import Graphics.Implicit.Export.MySymbolicObj2 (MySymbolicObj2 (myScad2))
import Graphics.Implicit.Export.MySymbolicObj3 (MySymbolicObj3 (myScad3))

import Control.Monad.Reader (Reader, runReader, ask)

import Data.List (intersperse)
import Data.Function (fix)
import Data.Foldable(fold, foldMap)

default (ℝ)

scad2 :: ℝ -> MySymbolicObj2 -> Text
scad2 res obj = toLazyText $ runReader (buildS2 obj) res

scad3 :: ℝ -> MySymbolicObj3 -> Text
scad3 res obj = toLazyText $ runReader (buildS3 obj) res

-- used by rotate2 and rotate3
rad2deg :: ℝ -> ℝ
rad2deg r = r * (180/pi)

-- | Format an openscad call given that all the modified objects are in the Reader monad...
callToken :: (Text, Text) -> Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
callToken cs name args []    = pure $ name <> buildArgs cs args <> ";"
callToken cs name args [obj] = ((name <> buildArgs cs args) <>) <$> obj
callToken cs name args objs  = do
  objs' <- foldMap (<> "\n") <$> sequenceA objs
  pure $! name <> buildArgs cs args <> "{\n" <> objs' <> "}\n"

buildArgs :: (Text, Text) -> [Builder] -> Builder
buildArgs _ [] = "()"
buildArgs (c1, c2) args = "(" <> fromLazyText c1 <> fold (intersperse "," args) <> fromLazyText c2 <> ")"

call :: Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
call = callToken ("[", "]")

callNaked :: Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
callNaked = callToken ("", "")

-- | First, the 3D objects.

buildS3 :: MySymbolicObj3 -> Reader ℝ Builder
buildS3 x = myScad3 x
{-
buildS3 (Rect3R r (x1,y1,z1) (x2,y2,z2)) | r == 0 = call "translate" [bf x1, bf y1, bf z1] [
                                            call "cube" [bf $ x2 - x1, bf $ y2 - y1, bf $ z2 - z1] []
                                           ]

buildS3 (Sphere r) = callNaked "sphere" ["r = " <> bf r] []

buildS3 (Cylinder h r1 r2) = callNaked "cylinder" [
                              "r1 = " <> bf r1
                             ,"r2 = " <> bf r2
                             , bf h
                             ] []

buildS3 (Complement3 obj) = call "complement" [] [buildS3 obj]

buildS3 (UnionR3 r objs) | r == 0 = call "union" [] $ buildS3 <$> objs

buildS3 (IntersectR3 r objs) | r == 0 = call "intersection" [] $ buildS3 <$> objs

buildS3 (DifferenceR3 r objs) | r == 0 = call "difference" [] $ buildS3 <$> objs

buildS3 (Translate3 (x,y,z) obj) = call "translate" [bf x, bf y, bf z] [buildS3 obj]

buildS3 (Scale3 (x,y,z) obj) = call "scale" [bf x, bf y, bf z] [buildS3 obj]

buildS3 (Rotate3 (x,y,z) obj) = call "rotate" [bf (rad2deg x), bf (rad2deg y), bf (rad2deg z)] [buildS3 obj]

buildS3 (Rotate3V a v obj) = callNaked "rotate" [ "a=" <> bf (rad2deg a), "v=" <> bvect v ] [buildS3 obj]
    where
    bvect (x, y, z) = "[" <> fold (intersperse "," [bf x, bf y, bf z]) <> "]"

buildS3 (Outset3 r obj) | r == 0 = call "outset" [] [buildS3 obj]

buildS3 (Shell3 r obj) | r == 0 = call "shell" [] [buildS3 obj]

-- FIXME: where is EmbedBoxedObj3?

buildS3 (ExtrudeR r obj h) | r == 0 = callNaked "linear_extrude" ["height = " <> bf h] [buildS2 obj]

buildS3 (ExtrudeRotateR r twist obj h) | r == 0 = callNaked "linear_extrude" ["height = " <> bf h, "twist = " <> bf twist] [buildS2 obj]

-- FIXME: handle scale, center.
buildS3 (ExtrudeRM r twist scale (Left translate) obj (Left height)) | r == 0 && isScaleID scale && translate == (0,0) = do
  res <- ask
  let
    twist' = case twist of
               Left twval  -> const twval
               Right twfun -> twfun
  call "union" [] [
             call "rotate" ["0","0", bf $ twist' h] [
                        callNaked "linear_extrude" ["height = " <> bf res, "twist = " <> bf (twist' (h+res) - twist' h)][
                                   buildS2 obj
                                  ]
                       ] |  h <- take (floor (res / height)) $ fix (\f x -> x : f (x+res)) 0
            ]

-- FIXME: where are RotateExtrude, ExtrudeOnEdgeOf?

buildS3 Rect3R{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(UnionR3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(IntersectR3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(DifferenceR3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(Outset3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(Shell3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 ExtrudeR{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 ExtrudeRotateR {} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 ExtrudeRM{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(EmbedBoxedObj3 _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 RotateExtrude{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(ExtrudeOnEdgeOf _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
-}
-- Now the 2D objects/transforms.

buildS2 :: MySymbolicObj2 -> Reader ℝ Builder
buildS2 x = myScad2 x
{-
buildS2 (RectR r (x1,y1) (x2,y2)) | r == 0 = call "translate" [bf x1, bf y1] [
                                    call "cube" [bf $ x2 - x1, bf $ y2 - y1] []
                                   ]

buildS2 (Circle r) = call "circle" [bf r] []

buildS2 (PolygonR r points) | r == 0 = call "polygon" [buildVector [x,y] | (x,y) <- points] []
    where buildVector comps = "[" <> fold (intersperse "," $  bf <$> comps) <> "]"

buildS2 (Complement2 obj) = call "complement" [] [buildS2 obj]

buildS2 (UnionR2 r objs) | r == 0 = call "union" [] $ buildS2 <$> objs

buildS2 (DifferenceR2 r objs) | r == 0 = call "difference" [] $ buildS2 <$> objs

buildS2 (IntersectR2 r objs) | r == 0 = call "intersection" [] $ buildS2 <$> objs

buildS2 (Translate2 (x,y) obj) = call "translate" [bf x, bf y] [buildS2 obj]

buildS2 (Scale2 (x,y) obj)     = call "scale" [bf x, bf y] [buildS2 obj]

buildS2 (Rotate2 r obj)     = call "rotate" [bf (rad2deg r)] [buildS2 obj]

buildS2 (Outset2 r obj) | r == 0 = call "outset" [] [buildS2 obj]

buildS2 (Shell2 r obj) | r == 0 =  call "shell" [] [buildS2 obj]

-- Generate errors for rounding requests. OpenSCAD does not support rounding.
buildS2 RectR{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (PolygonR _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (UnionR2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (DifferenceR2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (IntersectR2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (Outset2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (Shell2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."

-- FIXME: missing EmbedBoxedObj2?
buildS2 (EmbedBoxedObj2 _) = error "EmbedBoxedObj2 not implemented."
-}
