||| Geometric functions concerning angles. If not otherwise specified, all angles are in radians.
module Graphics.Gloss.Geometry.Angle

||| Convert degrees to radians
export
degToRad : Double -> Double
degToRad d = d * pi / 180
{-# INLINE degToRad #-}


||| Convert radians to degrees
export
radToDeg : Double -> Double
radToDeg r = r * 180 / pi
{-# INLINE radToDeg #-}


||| Normalize an angle to be between 0 and 2*pi radians
export
normalizeAngle : Double -> Double
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
  where
    floor' : Double -> Double
    floor' x = floor x -- TODO: is this correct? fromIntegral (floor x :: Int)
{-# INLINE normalizeAngle #-}
