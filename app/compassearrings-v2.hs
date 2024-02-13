module Main where
import SemigroupDo qualified as Semigroup

cutter = epilogZing

dia :: _ => Dia b
dia = stroke path # frame 12 # cutOn cutter

sheet :: _ => Dia b
sheet = tileDiagPairs (bedSize cutter) dia

main :: IO ()
main = defaultMain dia

-- Measurements
rOuterRose = 1 {-in-} * pxPerIn
δ          = rOuterRose/32

rInnerRose  = rOuterRose * 4/5 -   δ
rOuterRing  = rOuterRose * 2/3 - 3*δ
rInnerRing  = rOuterRose * 1/4
rHanger     = rOuterRose - rOuterRoseΔ - δ
rOuterRoseΔ = roseΔ rOuterRose (rInnerRing + δ)
rInnerRoseΔ = roseΔ rInnerRose rInnerRing

-- Util
roseΔ :: Double -> Double -> Double
roseΔ r1 r2 = r1 - δ*αR3/αR2 where
  αR3 = tau/4
  r3  = sqrt $ r1^2 + r2^2 - 2*r1*r2*cos αR3
  αR2 = r2*αR3/r3

rot45 :: _ => t -> t
rot45 = rotate (1/8 @@ turn)

sweep :: Path -> Path
sweep = pathUnion mempty . fold . take 4 . iterate (rot45 . rot45)

rose :: Double -> Double -> Path
rose r1 r2 = polygon $ PolygonOpts (PolyPolar as rs) NoOrient origin where
  as = replicate 7 (1/8 @@ turn)
  rs = cycle [r1, r2]

chaff :: Path -> Path -> Path
chaff p q = sweep $ (p∖q) ∩ sector where
  sector = wedge (radius unitX p) (rot45 xDir) (1/8 @@ turn)

-- Components
hangerOuter    = circle rHanger # translateY (rOuterRose - δ)
hangerInner    = circle δ       # translateY (rOuterRose - δ)
outerRose      = rose rOuterRose  (rInnerRing + δ + ε)
innerRose      = rose rInnerRose  (rInnerRing     + ε) # rot45
outerRoseΔ     = rose rOuterRoseΔ (rInnerRing     + ε)
innerRoseΔ     = rose rInnerRoseΔ (rInnerRing - δ + ε) # rot45
outerRing      = ring rOuterRing  (rOuterRing - δ)
innerRing      = ring rInnerRing  (rInnerRing - δ)
outerRoseChaff = chaff outerRoseΔ (circle (rInnerRing + δ))
innerRoseChaff = chaff (innerRoseΔ # rot45) (outerRose # rot45) # rot45
orthoginalBars = sweep $ rect δ (rOuterRoseΔ + ε) # translateY (-(rOuterRoseΔ + ε)/2)
diagonalBars   = sweep $ rect δ (rInnerRoseΔ + ε) # translateY (-(rInnerRoseΔ + ε)/2) # rot45

-- It
path = hangerOuter ∪ outerRose      ∪ innerRose      ∪ outerRing
     ∖ hangerInner ∪ outerRoseChaff ∪ innerRoseChaff ∪ innerRing
     ∖ orthoginalBars ∪ diagonalBars
