module Main where

material = Wood
cutter   = epilogZing
cutPx    = cutWidth cutter material

main = defaultMain "pentagramearrings" do
  stroke pathWithHanger # cutOn cutter

rRing        = 6.66  {-cm-} * pxPerCm / 2 + cutPx
δ            = 0.666 {-mm-} * pxPerMm     + cutPx
rHangerInner = 0.666 {-mm-} * pxPerMm
rHangerOuter = 3 * rHangerInner           + cutPx

ringOuter = circle rRing
ringInner = offsetPath (-δ) ringOuter

starOuter = offsetPath δ starInner
starInner = star (StarSkip 2) $ map (\n -> p2 (r * cos (a n), r * sin (a n))) [0..4]
  where (r, a) = (rRing - δ - ε, \n -> (3/4 + n/5) * tau)

hangerOuter = circle rHangerOuter # translateY (rRing + rHangerInner)
hangerInner = circle rHangerInner # translateY (rRing + rHangerInner)

path = pathDifference ringOuter (pathDifference ringInner (pathDifference starOuter starInner))

pathWithHanger = pathUnion $ path <> ((hangerOuter `pathDifference` hangerInner) `pathDifference` ringInner)
