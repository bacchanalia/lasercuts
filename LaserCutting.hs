{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module LaserCutting where
import Diagrams.Prelude

ptPerIn, pxPerInCairo, pxPerInSVG, pxPerIn :: Double
ptPerIn      = 72
pxPerInCairo = 72 -- cursed
pxPerInSVG   = 96
pxPerIn      = pxPerInSVG

type Dia b = QDiagram b V2 Double Any

cutOn :: _ => CutterParams -> Dia b -> Dia b
cutOn = lwO . cutWidth

data CutterParams = CutterParams
  { cutWidth :: Double
  , bedSize  :: V2 Double
  } deriving (Eq, Ord, Show)

epilogZing = CutterParams
  { cutWidth  = 0.001 {-in-} * ptPerIn
  , bedSize   = r2 $ (15.75 {-in-}, 12 {-in-}) & both %~ (* pxPerIn)
  }

tile :: _ => V2 Double -> V2 Double -> Dia b -> Dia b
tile size offset dia = sheet where
  sheet = foldMap shiftRow [0..nRows-1]
  row   = foldl1 (|||) $ replicate nCols (dia # alignL)
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
