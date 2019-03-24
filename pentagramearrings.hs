{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Control.Monad.Writer
import System.Environment
import Diagrams.Prelude
import Diagrams.TwoD.Path.IntersectionExtras
import Diagrams.TwoD.Polygons hiding (star)
import qualified Diagrams.TwoD.Polygons as P
import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG
import LaserCutting

import Data.Fixed
import Data.List

dia :: _ => Dia b
dia = cuts # frame 12 # cutOn epilogZing

out :: _ => Dia b
out = dia

main = do
  let dim = dims $ size (out :: Dia SVG)
  name <- getProgName
  renderCairo (name++".pdf") dim out
  renderSVG   (name++".svg") dim out


rRing   = 6.66 {-cm-} / 2.54 {-cm/in-} * pxPerIn / 2
δ       = 1/32 {-in-} * pxPerIn
rHanger = 3*δ

ε       = 1e-6 -- subpixel spacer to prevent intersections

star r = P.star (StarSkip 2) $ map (\n -> p2 (r * cos (a n), r * sin (a n))) [0..4] where
  a n = (3/4 + n/5)*tau

rawPath :: Path V2 Double
rawPath = mconcat [hanger, penta, ring] where
  hanger = execWriter $ do
    tell$ circle δ       # translateY (rRing + δ)
    tell$ circle rHanger # translateY (rRing - δ)
  ring = execWriter $ do
    tell$ circle (rRing - δ)
    tell$ circle rRing
  penta = execWriter $ do
    tell$ star (rRing - δ - ε)
    tell$ star (rRing     - ε)

cuts :: _ => QDiagram b V2 Double Any
cuts = onSections (explodeIntersections rawPath) $ concat [hanger, penta, ring] where
  onOff = tell . (:[]) . concat
        . concatMap (flip (zipWith replicate) [id, lw 0])
        . map (\(a,b) -> [a,b])
  hanger = execWriter $ do
    onOff $ repeat (1,1)
    onOff $ repeat (1,1)
  ring = execWriter $ do
    onOff $ repeat (1,1)
    onOff $ repeat (1,1)
  penta = execWriter $ do
    onOff $ repeat (1,1)
    onOff $ repeat (1,1)
