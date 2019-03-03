{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
import Control.Monad.Writer
import System.Environment
import Diagrams.Prelude
import Diagrams.TwoD.Polygons
import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG
import IntersectionExtras
import LaserCutting

dia :: _ => Dia b
dia = cuts # frame 12 # cutOn epilogZing

sheet :: _ => Dia b
sheet = tileDiagPairs (bedSize epilogZing) dia

out :: _ => Dia b
out = sheet

main = do
  let dim = dims $ size (out :: Dia SVG)
  name <- getProgName
  renderCairo (name++".pdf") dim out
  renderSVG   (name++".svg") dim out


rOuterRose = 1 {-in-} * pxPerIn
--δOrig      = rOuterRose*8/1000
δ          = rOuterRose/32
rHanger    = 3*δ

rInnerRoseOuter = rOuterRose * 4/5 -   δ--Orig
rOuterCircle    = rOuterRose * 2/3 - 3*δ--Orig
rInnerCircle    = rOuterRose * 1/4
rInnerRoseInner = rInnerCircle / cos (tau/8 - sin (rInnerCircle / rInnerRoseOuter))

roseΔ r1 r2 = r1 - δ*αR3/αR2 where
  αR3 = tau/4
  r3  = sqrt $ r1^2 + r2^2 - 2*r1*r2*cos αR3
  αR2 = r2*αR3/r3
rOuterRoseΔ = roseΔ rOuterRose      (rInnerCircle    + δ)
rInnerRoseΔ = roseΔ rInnerRoseOuter rInnerRoseInner

rose r1 r2 = polygon $ PolygonOpts (PolyPolar as rs) NoOrient origin where
  as = replicate 7 (1/8 @@ turn)
  rs = cycle [r1, r2]
rot4 p = foldMap (\α -> p # rotate (α/4 @@ turn)) [0..3]
line n = p2 (rInnerCircle - 2*δ, 0) ~~ p2 (n, 0)

logoRawPath :: Path V2 Double
logoRawPath = mconcat
  [hanger, innerCircle, outerCircle, lines, innerRose, outerRose] where
  hanger = execWriter $ do
    tell$ circle rHanger       # translateY (rOuterRose - δ)
    tell$ circle δ # translateY (rOuterRose - δ)
  innerCircle = execWriter $ do
    tell$ circle (rInnerCircle - δ)
    tell$ circle rInnerCircle
    tell$ circle (rInnerCircle + δ)
  outerCircle = execWriter $ do
    tell$ circle (rOuterCircle - δ)
    tell$ circle rOuterCircle
  innerRose = execWriter $ do
    tell$ rose (rInnerRoseInner - δ) rInnerRoseΔ
    tell$ rose rInnerRoseInner       rInnerRoseOuter
  outerRose = execWriter $ do
    tell$ rose rOuterRoseΔ rInnerCircle
    tell$ rose rOuterRose  (rInnerCircle + δ)
  lines = execWriter $ do
    tell$ rot4 (line (rInnerRoseΔ - δ) # translateY (-δ) # rotate (1/8 @@ turn))
    tell$ rot4 (line (rOuterRoseΔ - δ) # translateY (-δ))
    tell$ rot4 (line rInnerCircle # rotate (1/8 @@ turn))
    tell$ rot4 (line rInnerCircle)

cuts :: _ => QDiagram b V2 Double Any
cuts = onExplodedIntersections logoRawPath $ concat
  [hanger, innerCircle, outerCircle, lines, innerRose, outerRose] where
  on  = id
  off = lw 0
  --off = lc red
  w :: _ => a -> m ()
  w = tell . (:[])
  hanger = execWriter $ do
    w [ [on], [on], [on, off, off], [off, off, on] ]
    w [ [on], [on], [on,  on], [ on, on] ]
  innerCircle = map repeat . execWriter $ do
    w [ on, off,  on, off]
    w [off,  on, off,  on, off]
    w [off, off, off, off, off, off, on,  on, off]
  outerCircle = map repeat . execWriter $ do
    w [off, off,  on, off, off, off, off, on, off, off, off]
    w [off, off,  on, off, off, off, off, on, off, off, off]
  innerRose = map cycle . execWriter $ do
    w [[off, off, off, off,  on,  on,  on, off], repeat off]
    w [[off, off, off,  on, off,  on,  on], [ on, off, on, off, off, off]]
  outerRose = execWriter $ do
    w $ cycle [repeat off, [off,  on,  on,  on,  on,  on, off]]
    w . execWriter $ do
      w [ on, off,  on, off,  on, off]           >> w [off, off,  on, off,  on, off, off]
      w [ off, off, on, off,  on, off,  on, off] >> w [off, off,  on, off,  on,  on]
      w [ on, off,  on, off,  on, off]           >> w [off, off,  on, off,  on,  on]
      w [ on, off,  on, off,  on, off]           >> w [off, off,  on, off,  on,  on]
  lines = concatMap (replicate 4 . return) . execWriter $ do
    w [off,  on, off, off, off,  on,  on,  on, off]
    w [off,  on, off, off,  on,  on,  on,  on, off]
    w [off, on]
    w [off, on]
