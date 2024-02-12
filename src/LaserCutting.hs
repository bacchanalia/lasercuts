module LaserCutting
  ( module Relude
  , module Diagrams.Prelude
  , module Diagrams.TwoD.Polygons
  , module Diagrams.TwoD.Path.IntersectionExtras
  , Dia, defaultMain, CutterParams(..), Material(..), cutOn, epilogZing
  , ptPerIn, pxPerInCairo, pxPerInSVG, pxPerIn, pxPerCm, pxPerMm, ε
  , tile, tilePairs, tileOrth, tileDiag, tileOrthPairs, tileDiagPairs
  ) where
import Relude hiding (First, Last, (??), getFirst, getLast, local, phantom, trace, uncons, universe)
import Diagrams.Prelude
import Diagrams.TwoD.Polygons
import Diagrams.TwoD.Path.IntersectionExtras
import Diagrams.Backend.Cairo

type Dia b = QDiagram b V2 Double Any

defaultMain :: String -> Dia Cairo -> IO ()
defaultMain name out = do
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
