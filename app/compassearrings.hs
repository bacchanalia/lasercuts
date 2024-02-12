{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Control.Monad.Writer
import System.Environment
import Diagrams.Prelude
import Diagrams.TwoD.Path.IntersectionExtras
import Diagrams.TwoD.Polygons
import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG
import LaserCutting

import Data.Fixed
import Data.List

dia :: _ => Dia b
dia = cuts # frame 12 # cutOn epilogZing

sheet :: _ => Dia b
sheet = tileDiagPairs (bedSize epilogZing) dia

out :: _ => Dia b
out = dia

main = do
  let dim = dims $ size (out :: Dia SVG)
  name <- getProgName
  renderCairo (name++".pdf") dim out
  renderSVG   (name++".svg") dim out


rOuterRose = 1 {-in-} * pxPerIn
δ          = rOuterRose/32
ε          = 1e-6 -- subpixel spacer to prevent intersections

rInnerRoseOuter = rOuterRose * 4/5 -   δ
rOuterCircle    = rOuterRose * 2/3 - 3*δ
rInnerCircle    = rOuterRose * 1/4
rHanger         = rOuterRose - rOuterRoseΔ - δ

roseΔ r1 r2 = r1 - δ*αR3/αR2 where
  αR3 = tau/4
  r3  = sqrt $ r1^2 + r2^2 - 2*r1*r2*cos αR3
  αR2 = r2*αR3/r3
rOuterRoseΔ = roseΔ rOuterRose      (rInnerCircle + δ)
rInnerRoseΔ = roseΔ rInnerRoseOuter rInnerCircle

rose r1 r2 = polygon $ PolygonOpts (PolyPolar as rs) NoOrient origin where
  as = replicate 7 (1/8 @@ turn)
  rs = cycle [r1, r2]
rot4 p = foldMap (\α -> p # rotate (α/4 @@ turn)) [0..3]
line n = p2 (rInnerCircle - 2*δ, 0) ~~ p2 (n, 0)


rawPath :: Path V2 Double
rawPath = mconcat
  [hanger, innerCircle, outerCircle, lines, innerRose, outerRose] where
  hanger = execWriter $ do
    tell$ circle (δ       + ε) # translateY (rOuterRose - δ)
    tell$ circle (rHanger - ε) # translateY (rOuterRose - δ)
  innerCircle = execWriter $ do
    tell$ circle (rInnerCircle - δ)
    tell$ circle rInnerCircle
    tell$ circle (rInnerCircle + δ)
  outerCircle = execWriter $ do
    tell$ circle (rOuterCircle - δ)
    tell$ circle rOuterCircle
  innerRose = execWriter $ do
    tell$ rose (rInnerCircle - δ + ε) rInnerRoseΔ
    tell$ rose (rInnerCircle     + ε) rInnerRoseOuter
  outerRose = execWriter $ do
    tell$ rose rOuterRoseΔ (rInnerCircle     + ε)
    tell$ rose rOuterRose  (rInnerCircle + δ + ε)
  lines = execWriter $ do
    tell$ rot4 (line (rInnerRoseΔ - δ) # translateY (-δ) # rotate (1/8 @@ turn))
    tell$ rot4 (line (rOuterRoseΔ - δ) # translateY (-δ))
    tell$ rot4 (line rInnerCircle # rotate (1/8 @@ turn))
    tell$ rot4 (line rInnerCircle)

cuts :: _ => QDiagram b V2 Double Any
cuts = onSections (explodeIntersections rawPath) $ concat
  [hanger, innerCircle, outerCircle, lines, innerRose, outerRose] where
  onOff = tell . (:[]) . concat
        . concatMap (flip (zipWith replicate) [id, lw 0])
        . map (\(a,b) -> [a,b])
  hanger = execWriter $ do
    onOff [(2, 0)]
    onOff [(1, 1)]
  innerCircle = map cycle . execWriter $ do
    onOff [(1, 1)]
    onOff [(2, 1)]
    onOff [(0, 5), (3, 0)]
  outerCircle = map cycle . execWriter $ do
    onOff [(0, 2), (1, 4), (1, 2)]
    onOff [(0, 2), (1, 4), (1, 2)]
  innerRose = map cycle . execWriter $ do
    onOff [(0, 4), (2, 9)]
    onOff [(0, 3), (1, 1), (1, 1), (1, 3)]
  outerRose = execWriter $ do
    onOff $ cycle [(0, 7), (5, 0)]
    onOff $ s1 ++ s2' ++ s1' ++ cycle (s2 ++ s1)
    where
      s1  = [(1, 1), (1, 1), (1, 0)]
      s2  = [(0, 2), (1, 1)]
      s2' = s2 ++ [(1, 1)]
      s1' = [(0, 2)] ++ s1
  lines = concatMap (replicate 4) . execWriter $ do
    onOff [(0, 1), (1, 3), (2, 2)]
    onOff [(0, 1), (2, 2), (3, 1)]
    onOff [(0, 1), (1, 0)]
    onOff [(0, 1), (2, 0)]
