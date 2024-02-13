module LaserCutting
  ( module Relude
  , module Diagrams.Prelude
  , module Diagrams.TwoD.Offset
  , module Diagrams.TwoD.Polygons
  , module Diagrams.TwoD.Path
  , module Diagrams.TwoD.Path.IntersectionExtras
  , Dia, Path, Direction
  , defaultMain, CutterParams(..), Material(..), cutOn, epilogZing
  , ptPerIn, pxPerInCairo, pxPerInSVG, pxPerIn, pxPerCm, pxPerMm, ε
  , tile, tilePairs, tileOrth, tileDiag, tileOrthPairs, tileDiagPairs
  , pathUnion, pathIntersection, pathDifference, pathExclusion
  , pathUnion', pathIntersection', pathDifference', pathExclusion'
  , pathUnionEvenOdd, pathIntersectionEvenOdd, pathDifferenceEvenOdd, pathExclusionEvenOdd
  , pathUnionEvenOdd', pathIntersectionEvenOdd', pathDifferenceEvenOdd', pathExclusionEvenOdd'
  , (∪), (∩), (∖), (⊗)
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
type Direction = Diagrams.Direction V2 Double

defaultMain :: Dia Cairo -> IO ()
defaultMain out = do
  name <- getProgName
  let render ext = renderCairo (name ++ "." ++ ext) (dims $ size out) out
  mapM_ render ["pdf", "svg", "png"]

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

-- | Test if a trail winds counterclockwise. May return either true or false
--   if the trail contains portions that wind in each direction. Returns true
--   for lines and loops with zero area.
isCounterclockwise :: RealFloat n => Located (Trail V2 n) -> Bool
isCounterclockwise t = w > 0 || w == 0 && any (> 0) ws where
  w  = sample t (atStart t)
  ws = resample t

isClockwise :: RealFloat n => Located (Trail V2 n) -> Bool
isClockwise t = w < 0 || w == 0 && any (< 0) ws where
  w  = sample t (atStart t)
  ws = resample t

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


-- | Force a trail to wind counterclockwise.
counterclockwise :: RealFloat n => Located (Trail V2 n) -> Located (Trail V2 n)
counterclockwise t | isClockwise t = reverseLocTrail t
                   | otherwise     = t

-- | Force a trail to wind clockwise.
clockwise :: RealFloat n => Located (Trail V2 n) -> Located (Trail V2 n)
clockwise t | isCounterclockwise t = reverseLocTrail t
            | otherwise            = t

-- | Force a path to wind counterclockwise.
counterclockwiseP :: Path -> Path
counterclockwiseP p | p == mempty                                     = mempty
                    | isCounterclockwise . Unsafe.head $ pathTrails p = p
                    | otherwise                                       = reversePath p

clockwiseP :: Path -> Path
clockwiseP p | p == mempty                              = mempty
             | isClockwise . Unsafe.head $ pathTrails p = p
             | otherwise                                = reversePath p

pathUnion :: Path -> Path
pathUnion = counterclockwiseP . Dia.union Winding

pathIntersection :: Path -> Path -> Path
pathIntersection = counterclockwiseP <<$>> Dia.intersection Winding

pathDifference :: Path -> Path -> Path
pathDifference = counterclockwiseP <<$>> Dia.difference Winding

pathExclusion :: Path -> Path -> Path
pathExclusion = counterclockwiseP <<$>> Dia.exclusion Winding

pathUnion' :: Double -> Path -> Path
pathUnion' eps = counterclockwiseP . Dia.union' eps Winding

pathIntersection' :: Double -> Path -> Path -> Path
pathIntersection' eps = counterclockwiseP <<$>> Dia.intersection' eps Winding

pathDifference' :: Double -> Path -> Path -> Path
pathDifference' eps = counterclockwiseP <<$>> Dia.difference' eps Winding

pathExclusion' :: Double -> Path -> Path -> Path
pathExclusion' eps = counterclockwiseP <<$>> Dia.exclusion' eps Winding

pathUnionEvenOdd :: Path -> Path
pathUnionEvenOdd = counterclockwiseP . Dia.union EvenOdd

pathIntersectionEvenOdd :: Path -> Path -> Path
pathIntersectionEvenOdd = counterclockwiseP <<$>> Dia.intersection EvenOdd

pathDifferenceEvenOdd :: Path -> Path -> Path
pathDifferenceEvenOdd = counterclockwiseP <<$>> Dia.difference EvenOdd

pathExclusionEvenOdd :: Path -> Path -> Path
pathExclusionEvenOdd = counterclockwiseP <<$>> Dia.exclusion EvenOdd

pathUnionEvenOdd' :: Double -> Path -> Path
pathUnionEvenOdd' eps = counterclockwiseP . Dia.union' eps EvenOdd

pathIntersectionEvenOdd' :: Double -> Path -> Path -> Path
pathIntersectionEvenOdd' eps = counterclockwiseP <<$>> Dia.intersection' eps EvenOdd

pathDifferenceEvenOdd' :: Double -> Path -> Path -> Path
pathDifferenceEvenOdd' eps = counterclockwiseP <<$>> Dia.difference' eps EvenOdd

pathExclusionEvenOdd' :: Double -> Path -> Path -> Path
pathExclusionEvenOdd' eps = counterclockwiseP <<$>> Dia.exclusion' eps EvenOdd

infixl 2 ∪
infixl 3 ∩
infixl 5 ∖
infixl 6 ⊗
(∪), (∩), (∖), (⊗) :: Path -> Path -> Path
a ∪ b = pathUnion (a <> b)
(∩) = pathIntersection
(∖) = pathDifference
(⊗) = pathExclusion

ring :: Double -> Double -> Path
ring rOuter rInner = pathDifference (circle rOuter) (circle rInner)
