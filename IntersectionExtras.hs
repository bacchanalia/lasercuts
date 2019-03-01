{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module IntersectionExtras where
import Data.Colour
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Path
import Diagrams.Trail
import Diagrams.TwoD.Segment

defEps :: Fractional n => n
defEps = 1e-8

intersectParams :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n) => t -> s -> [(n, n)]
intersectParams = intersectParams' defEps

intersectParams' :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n) => n -> t -> s -> [(n, n)]
intersectParams' eps as bs = intersectParamsP' defEps (toPath as) (toPath bs)

intersectParamsP :: OrderedField n => Path V2 n -> Path V2 n -> [(n, n)]
intersectParamsP = intersectParamsP' defEps

intersectParamsP' :: OrderedField n => n -> Path V2 n -> Path V2 n -> [(n, n)]
intersectParamsP' eps as bs = do
  a <- pathTrails as
  b <- pathTrails bs
  intersectParamsT' eps a b

intersectParamsT :: OrderedField n => Located (Trail V2 n) -> Located (Trail V2 n) -> [(n, n)]
intersectParamsT = intersectParamsT' defEps

intersectParamsT' :: OrderedField n => n -> Located (Trail V2 n) -> Located (Trail V2 n) -> [(n, n)]
intersectParamsT' eps as bs = do
  a <- fixTrail as
  b <- fixTrail bs
  map (\(p, q, _) -> (p, q)) $ segmentSegment eps a b

splitSeg :: (Additive v, Num n) => FixedSegment v n -> n -> (FixedSegment v n, FixedSegment v n)
splitSeg (FLinear p0 p1) t = (FLinear p0 sp, FLinear sp p1)
  where
    interp = flip $ lerp t
    sp = interp p0 p1
-- Based on Geom2D.CubicBezier.Basic.splitBezierCubic
splitSeg (FCubic p0 p1 p2 p3) t = (FCubic p0 p01 p0112 sp, FCubic sp p1223 p23 p3)
  where
    interp = flip $ lerp t
    p12   = interp p1    p2
    p01   = interp p0    p1
    p0112 = interp p01   p12
    sp    = interp p0112 p1223
    p1223 = interp p12   p23
    p23   = interp p2    p3

-- Based on Geom2D.CubicBezier.Basic.bezierSubsegment
sliceSeg :: (Additive v, OrderedField n) => FixedSegment v n -> n -> n -> FixedSegment v n
sliceSeg s t1 t2
  | t1 > t2   = sliceSeg s t2 t1
  | t2 == 0   = fst $ splitSeg s t1
  | otherwise = snd $ flip splitSeg (t1/t2) $ fst $ splitSeg s t2

explodeIntersections :: (InSpace V2 n t, TrailLike t) => Path V2 n -> [[[t]]]
explodeIntersections = explodeIntersections' defEps

explodeIntersections' :: (InSpace V2 n t, TrailLike t) => n -> Path V2 n -> [[[t]]]
explodeIntersections' eps path = map (map $ map toTrailLike . cut) explodedPath
  where
    explodedPath = explodePath path
    toTrailLike  = fromLocSegments . mapLoc (:[]) . fromFixedSeg
    toFixedSeg   = mkFixedSeg . mapLoc (head . trailSegments)
    cut t = foldr (\(a, b) cs -> sliceSeg (toFixedSeg t) a b : cs) [] $ zip isects (tail isects)
      where
        isects         = exactEndpoints . sort . avoidEmptySegs . subSegs $ concat explodedPath
        exactEndpoints = (0:) . (++[1]) . filter (\p -> (p > eps) && (p < 1-eps))
        avoidEmptySegs = nubBy (\a b -> abs (a - b) < eps)
        subSegs        = concatMap $ notOnSelf $ map fst . intersectParamsT' eps t
        notOnSelf f t' = if t' /= t then f t' else [] -- intersecting an FCubic with itself explodes

onExplodedPath :: (TypeableFloat n, Renderable (Path V2 n) b)
  => Path V2 n -> [[QDiagram b V2 n Any -> QDiagram b V2 n Any]] -> QDiagram b V2 n Any
onExplodedPath p fs = mconcat . mconcat . zipWith (zipWith ($)) fs . map (map strokeP) $ explodePath p

onExplodedIntersections :: (TypeableFloat n, Renderable (Path V2 n) b)
  => Path V2 n -> [[[QDiagram b V2 n Any -> QDiagram b V2 n Any]]] -> QDiagram b V2 n Any
onExplodedIntersections p fs = mconcat . mconcat . mconcat . zipWith (zipWith (zipWith ($))) fs . map (map (map strokeP)) $ explodeIntersections p

{-
s1, s2, s3, s4, s5, st :: Path V2 Double
s1 = square 100 # translateY 5
s2 = square 100 # rotate (1/8 @@ turn)
s3 = s1 <> s2
s4 = circle (25*(1 + sqrt 2)) # translateX 5
s5 = s3 <> s4

st = mconcat $ map (\n -> hrule 100 # rotate (n/8 @@ turn)) [0..3]

rainbow = cycle [black, red, orange, yellow, green, blue, indigo, violet]

dia :: Diagram Cairo
dia = mconcat . mconcat . mconcat
    $ map (map $ zipWith lc rainbow . map stroke)
    $ (explodeIntersections s5 :: [[[Path V2 Double]]])

main = mainWith $ dia # bg white
-}
