module Main where

material = Wood
cutter   = epilogZing
cutPx    = cutWidth cutter material

dia :: _ => Dia b
dia = path # sortTrailsRadial # stroke # frame 12 # cutOn cutter

main = defaultMain dia

-- Measurements
rRing        = 6.66  {-cm-} * pxPerCm / 2 + cutPx
δ            = 0.666 {-mm-} * pxPerMm     + cutPx
rHangerInner = 0.666 {-mm-} * pxPerMm
rHangerOuter = 3 * rHangerInner           + cutPx

-- Components
ringOuter = circle rRing
ringInner = circle (rRing - δ)

starInner = star (StarSkip 2) $ map (\n -> p2 (r * cos (a n), r * sin (a n))) [0..4]
  where (r, a) = (rRing - δ - ε, \n -> (3/4 + n/5) * tau)
thickStar = expandPath (δ/2) $ offsetPath (δ/2) starInner

hangerOuter = circle rHangerOuter # translateY (rRing + rHangerInner)
hangerInner = circle rHangerInner # translateY (rRing + rHangerInner)

-- It
path = hangerOuter ∪ ringOuter ∖ hangerInner ∪ (ringInner ∖ thickStar)
