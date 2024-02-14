module LaserCutting
  ( module Relude
  , module Diagrams.Prelude
  , module Diagrams.TwoD.Offset
  , module Diagrams.TwoD.Polygons
  , module Diagrams.TwoD.Path
  , module Diagrams.TwoD.Path.IntersectionExtras
  , Dia, Path, Direction
  , defaultMain, sortTrailsBy, sortTrailsOn, sortTrailsRadial
  , CutterParams(..), Material(..), cutOn, epilogZing
  , ptPerIn, pxPerInCairo, pxPerInSVG, pxPerIn, pxPerCm, pxPerMm, ε
  , tile, tilePairs, tileOrth, tileDiag, tileOrthPairs, tileDiagPairs
  , pathUnion, pathIntersection, pathDifference, pathExclusion
  , pathUnion', pathIntersection', pathDifference', pathExclusion'
  , pathUnionEvenOdd, pathIntersectionEvenOdd, pathDifferenceEvenOdd, pathExclusionEvenOdd
  , pathUnionEvenOdd', pathIntersectionEvenOdd', pathDifferenceEvenOdd', pathExclusionEvenOdd'
  , (∪), (∩), (∖), (⊕), (⋓), (⋒), (∖∖), (⊛)
  , counterclockwise, clockwise
  , ring
  ) where
import Relude hiding (First, Last, (??), getFirst, getLast, local, phantom, trace, uncons, universe)
import Diagrams.Prelude                       hiding (Path, Direction)
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Polygons
import Diagrams.TwoD.Path                     hiding (Path)
import Diagrams.TwoD.Path.IntersectionExtras
import Diagrams.Backend.Cairo
import System.Environment

import Relude.Unsafe              qualified as Unsafe
import Diagrams.Prelude           qualified as Diagrams
import Diagrams.TwoD.Path.Boolean qualified as Dia

type Dia b = QDiagram b V2 Double Any
type Path = Diagrams.Path V2 Double
type LocTrail = Located (Trail V2 Double)
type Direction = Diagrams.Direction V2 Double

defaultMain :: Dia Cairo -> IO ()
defaultMain out = do
  name <- getProgName
  let render ext = renderCairo (name ++ "." ++ ext) (dims $ size out) out
  mapM_ render ["pdf", "svg", "png"]

sortTrailsBy :: (LocTrail -> LocTrail -> Ordering) -> Path -> Path
sortTrailsBy f = foldMap pathFromLocTrail . sortBy f' . pathTrails where
  f' a b | isInsideWinding a (centerPoint b) = GT
         | isInsideWinding b (centerPoint a) = LT
         | otherwise = f a b

sortTrailsOn :: Ord a => (LocTrail -> a) -> Path -> Path
sortTrailsOn = sortTrailsBy . comparing

sortTrailsRadial :: Path -> Path
sortTrailsRadial = sortTrailsOn (view _theta . centerPoint)

data CutterParams = CutterParams
  { abstractCutWidth :: Double
  , bedSize          :: V2 Double
  , cutWidth         :: Material -> Double
  }

data Material = Wood | Acrylic
  deriving (Eq, Ord, Read, Show)


-- Set the line width of the diagram based on the CutterParams
cutOn :: _ => CutterParams -> Dia b -> Dia b
cutOn = lwO . abstractCutWidth

epilogZing :: CutterParams
epilogZing = CutterParams
  { abstractCutWidth = 0.001 {-in-} * ptPerIn
  , bedSize          = r2 $ (15.75 {-in-}, 12 {-in-}) & both %~ (* pxPerIn)
  , cutWidth         = \case
      Wood    -> 0.025 {-cm-} * pxPerCm / 2
      Acrylic -> 0.020 {-cm-} * pxPerCm / 2
  }

ptPerIn, pxPerInCairo, pxPerInSVG, pxPerIn, pxPerCm, pxPerMm :: Double
ptPerIn      = 72
pxPerInCairo = 72
pxPerInSVG   = 96
pxPerIn      = pxPerInCairo
pxPerCm      = pxPerIn / 2.54 {-cm/in-}
pxPerMm      = pxPerCm / 10 {-cm/mm-}
ε            = 1e-5 -- subpixel spacer to prevent intersections

tile :: _ => V2 Double -> V2 Double -> Dia b -> Dia b
tile size offset dia = sheet where
  sheet = foldMap shiftRow [0..nRows-1]
  row   = foldl' (|||) mempty $ replicate nCols (dia # alignL)
  shiftRow n = row # translate (offset & _x %~ (*(fromIntegral $ n`mod`2))
                                       & _y %~ (* fromIntegral n))
  nRows = floor $ (size^._y - height dia)/offset^._y + 1
  nCols = floor $ (size^._x - offset^._x)/(width dia)

tilePairs :: _ => V2 Double -> V2 Double -> Dia b -> Dia b
tilePairs size offset dia = tile size (unitY ^* offset^._y * 2) pair where
  row  = tile (size^._x ^& height dia) offset dia
  pair = row <> row # reflectX # alignL # translate (offset # reflectY)

tileOrth  :: _ => V2 Double -> Dia b -> Dia b
tileOrth sz dia = tile sz (0 ^& height dia) dia

tileDiag  :: _ => V2 Double -> Dia b -> Dia b
tileDiag sz dia = tile sz ((width dia/2) ^& (height dia/2)) dia

tileOrthPairs  :: _ => V2 Double -> Dia b -> Dia b
tileOrthPairs sz dia = tilePairs sz (0 ^& height dia) dia

tileDiagPairs  :: _ => V2 Double -> Dia b -> Dia b
tileDiagPairs sz dia = tilePairs sz ((width dia/2) ^& (height dia/2)) dia

-- Test if a trail winds counterclockwise. May return either true or false
-- if the trail contains portions that wind in each direction. Returns true
-- for lines and loops with zero area.
isCounterclockwiseT :: RealFloat n => Located (Trail V2 n) -> Bool
isCounterclockwiseT t = w > 0 || w == 0 && any (> 0) ws where
  w  = sample t (atStart t)
  ws = resample t

isClockwiseT :: RealFloat n => Located (Trail V2 n) -> Bool
isClockwiseT = not . isCounterclockwiseT

resample :: RealFloat n => Located (Trail V2 n) -> [Crossings]
resample t = if null segs then [0] else windings where
  segs = fixTrail t
  -- Three arbitrary params on the first segment of the trail.
  params  = map (/ genericLength segs) [1/2, 1/3, 2/3]
  normals = map (t `normalAtParam`) params
  -- Since a segment has at most two cusps, at least one param must have a normal.
  (param, normal) = Unsafe.head . filter (isValid . snd) $ zip params normals
    where isValid v = let s = sum v in not $ isNaN s || isInfinite s
  point   = t `atParam` param
  traced  = map ((point.+^).(*^normal)) $ getSortedList $ appTrace (getTrace t) point normal
  -- Some point along the trace must be in the loop.
  windings = map (sample t) $ zipWith (\p q -> (p+q)/2) traced (Unsafe.tail traced)

isCounterclockwise :: Path -> Bool
isCounterclockwise p = p == mempty || (isCounterclockwiseT . Unsafe.head . pathTrails) p

isClockwise :: Path -> Bool
isClockwise = not . isCounterclockwise

counterclockwise :: Path -> Path
counterclockwise p = if isCounterclockwise p then p else reversePath p

clockwise :: Path -> Path
clockwise p = if isClockwise p then p else reversePath p

pathUnion :: Path -> Path -> Path
pathUnion a b = counterclockwise $ Dia.union Winding (a <> b)

pathIntersection :: Path -> Path -> Path
pathIntersection = counterclockwise <<$>> Dia.intersection Winding

pathDifference :: Path -> Path -> Path
pathDifference = counterclockwise <<$>> Dia.difference Winding

pathExclusion :: Path -> Path -> Path
pathExclusion = counterclockwise <<$>> Dia.exclusion Winding

pathUnion' :: Double -> Path -> Path -> Path
pathUnion' eps a b = counterclockwise $ Dia.union' eps Winding (a <> b)

pathIntersection' :: Double -> Path -> Path -> Path
pathIntersection' eps = counterclockwise <<$>> Dia.intersection' eps Winding

pathDifference' :: Double -> Path -> Path -> Path
pathDifference' eps = counterclockwise <<$>> Dia.difference' eps Winding

pathExclusion' :: Double -> Path -> Path -> Path
pathExclusion' eps = counterclockwise <<$>> Dia.exclusion' eps Winding

pathUnionEvenOdd :: Path -> Path -> Path
pathUnionEvenOdd a b = counterclockwise $ Dia.union EvenOdd (a <> b)

pathIntersectionEvenOdd :: Path -> Path -> Path
pathIntersectionEvenOdd = counterclockwise <<$>> Dia.intersection EvenOdd

pathDifferenceEvenOdd :: Path -> Path -> Path
pathDifferenceEvenOdd = counterclockwise <<$>> Dia.difference EvenOdd

pathExclusionEvenOdd :: Path -> Path -> Path
pathExclusionEvenOdd = counterclockwise <<$>> Dia.exclusion EvenOdd

pathUnionEvenOdd' :: Double -> Path -> Path -> Path
pathUnionEvenOdd' eps a b = counterclockwise $ Dia.union' eps EvenOdd (a <> b)

pathIntersectionEvenOdd' :: Double -> Path -> Path -> Path
pathIntersectionEvenOdd' eps = counterclockwise <<$>> Dia.intersection' eps EvenOdd

pathDifferenceEvenOdd' :: Double -> Path -> Path -> Path
pathDifferenceEvenOdd' eps = counterclockwise <<$>> Dia.difference' eps EvenOdd

pathExclusionEvenOdd' :: Double -> Path -> Path -> Path
pathExclusionEvenOdd' eps = counterclockwise <<$>> Dia.exclusion' eps EvenOdd

infixr 2 ∖, ∖∖
infixr 3 ∪, ⋓
infixr 4 ∩, ⋒, ⊕, ⊛
(∪), (∩), (∖), (⊕), (⋓), (⋒), (∖∖), (⊛) :: Path -> Path -> Path
(∪)  = pathUnion
(∩)  = pathIntersection
(∖)  = pathDifference
(⊕)  = pathExclusion
(⋓)  = pathUnionEvenOdd
(⋒)  = pathIntersectionEvenOdd
(∖∖) = pathDifferenceEvenOdd
(⊛)  = pathExclusionEvenOdd

ring :: Double -> Double -> Path
ring rOuter rInner = circle rOuter ∖ circle rInner
