module Vec2

%access public export

data Vec2 = MkVec2 Double Double

vecZero : Vec2
vecZero = MkVec2 0.0 0.0

vecAdd : Vec2 -> Vec2 -> Vec2
vecAdd (MkVec2 a b) (MkVec2 x y)
  = MkVec2 (a+x) (b+y)

vecSub : Vec2 -> Vec2 -> Vec2
vecSub (MkVec2 a b) (MkVec2 x y)
  = MkVec2 (a-x) (b-y)

vecScale : Vec2 -> Double -> Vec2
vecScale (MkVec2 a b) s
  = MkVec2 (a*s) (b*s)

vecDot : Vec2 -> Vec2 -> Double
vecDot (MkVec2 a b) (MkVec2 x y)
  = (a*x)+(b*y)

vecNorm : Vec2 -> Double
vecNorm v = sqrt (vecDot v v)

vecNormalize : Vec2 -> Vec2
vecNormalize v
  = vecScale v (1.0 / (vecNorm v))

vecDimSelect : Vec2 -> Int -> Double
vecDimSelect (MkVec2 a b) n
  = case mod n 2 of
        0 => a
        1 => b

vecLessThan : Vec2 -> Vec2 -> Bool
vecLessThan (MkVec2 a b) (MkVec2 x y)
  = a < x && b < y

vecGreaterThan : Vec2 -> Vec2 -> Bool
vecGreaterThan (MkVec2 a b) (MkVec2 x y)
  = a > x && b > y