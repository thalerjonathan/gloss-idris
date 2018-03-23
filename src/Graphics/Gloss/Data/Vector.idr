||| Geometric functions concerning vectors.
module Graphics.Gloss.Data.Vector

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Rendering

||| The magnitude of a vector.
export
magV : Vector -> Double
magV (x, y)     
        = sqrt (x * x + y * y)

||| The angle of this vector, relative to the +ve x-axis.
export
argV : Vector -> Double
argV (x, y)
        = normalizeAngle $ atan2 y x

||| The dot product of two vectors.
export
dotV : Vector -> Vector -> Double
dotV (x1, x2) (y1, y2)
        = x1 * y1 + x2 * y2

||| The determinant of two vectors.
export
detV : Vector -> Vector -> Double
detV (x1, y1) (x2, y2)
        = x1 * y2 - y1 * x2

||| Multiply a vector by a scalar.
export
mulSV : Double -> Vector -> Vector
mulSV s (x, y)          
        = (s * x, s * y)

||| Rotate a vector by an angle (in radians). +ve angle is counter-clockwise.
export
rotateV : Double -> Vector -> Vector
rotateV r (x, y)
 =      (  x * cos r - y * sin r
        ,  x * sin r + y * cos r)

||| Compute the inner angle (in radians) between two vectors.
export
angleVV : Vector -> Vector -> Double
angleVV p1 p2
 = let  m1      = magV p1
        m2      = magV p2
        d       = p1 `dotV` p2
        aDiff   = acos $ d / (m1 * m2)

   in   aDiff   


||| Normalise a vector, so it has a magnitude of 1.
export
normalizeV : Vector -> Vector
normalizeV v    = mulSV (1 / magV v) v

||| Produce a unit vector at a given angle relative to the +ve x-axis.
|||      The provided angle is in radians.
export
unitVectorAtAngle : Double -> Vector
unitVectorAtAngle r
        = (cos r, sin r)