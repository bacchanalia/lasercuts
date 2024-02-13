{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where
import SemigroupDo qualified as Semigroup

dia :: _ => Dia b
dia = cuts # frame 12 # cutOn epilogZing

sheet :: _ => Dia b
sheet = tileDiagPairs (bedSize epilogZing) dia

main :: IO ()
main = defaultMain dia


rOuterRose = 1 {-in-} * pxPerIn
δ          = rOuterRose/32

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


rawPath :: Path
rawPath = fold [hanger, innerCircle, outerCircle, lines, innerRose, outerRose] where
  hanger = Semigroup.do
    circle (δ       + ε) # translateY (rOuterRose - δ)
    circle (rHanger - ε) # translateY (rOuterRose - δ)
  innerCircle = Semigroup.do
    circle (rInnerCircle - δ)
    circle rInnerCircle
    circle (rInnerCircle + δ)
  outerCircle = Semigroup.do
    circle (rOuterCircle - δ)
    circle rOuterCircle
  innerRose =  Semigroup.do
    rose (rInnerCircle - δ + ε) rInnerRoseΔ
    rose (rInnerCircle     + ε) rInnerRoseOuter
  outerRose = Semigroup.do
    rose rOuterRoseΔ (rInnerCircle     + ε)
    rose rOuterRose  (rInnerCircle + δ + ε)
  lines = Semigroup.do
    rot4 (line (rInnerRoseΔ - δ) # translateY (-δ) # rotate (1/8 @@ turn))
    rot4 (line (rOuterRoseΔ - δ) # translateY (-δ))
    rot4 (line rInnerCircle # rotate (1/8 @@ turn))
    rot4 (line rInnerCircle)

cuts :: _ => Dia b
cuts = explodeIntersections rawPath `onSections` concat [hanger, innerCircle, outerCircle, lines, innerRose, outerRose] where
  onOff = pure . concat
        . concatMap (flip (zipWith replicate) [id, lw 0])
        . map (\(a,b) -> [a,b])
  hanger = Semigroup.do
    onOff [(2, 0)]
    onOff [(1, 1)]
  innerCircle = map cycle Semigroup.do
    onOff [(1, 1)]
    onOff [(2, 1)]
    onOff [(0, 5), (3, 0)]
  outerCircle = map cycle Semigroup.do
    onOff [(0, 2), (1, 4), (1, 2)]
    onOff [(0, 2), (1, 4), (1, 2)]
  innerRose = map cycle Semigroup.do
    onOff [(0, 4), (2, 9)]
    onOff [(0, 3), (1, 1), (1, 1), (1, 3)]
  outerRose = Semigroup.do
    onOff $ cycle [(0, 7), (5, 0)]
    onOff $ s1 ++ s2' ++ s1' ++ cycle (s2 ++ s1)
    where
      s1  = [(1, 1), (1, 1), (1, 0)]
      s2  = [(0, 2), (1, 1)]
      s2' = s2 ++ [(1, 1)]
      s1' = [(0, 2)] ++ s1
  lines = concatMap (replicate 4) Semigroup.do
    onOff [(0, 1), (1, 3), (2, 2)]
    onOff [(0, 1), (2, 2), (3, 1)]
    onOff [(0, 1), (1, 0)]
    onOff [(0, 1), (2, 0)]
