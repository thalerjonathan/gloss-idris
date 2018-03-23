module Graphics.Gloss.Data.ViewPort

import Graphics.Gloss.Data.Picture

||| The 'ViewPort' represents the global transformation applied to the displayed picture.
|||      When the user pans, zooms, or rotates the display then this changes the 'ViewPort'.
public export
record ViewPort where
  constructor MkViewPort
  ||| Global translation.
  viewPortTranslate : (Double, Double)
  ||| Global rotation (in degrees).
  viewPortRotate    : Double               
  ||| Global scaling (of both x and y coordinates).
  viewPortScale     : Double               

||| Convert degrees to radians
degToRad : Double -> Double
degToRad d   = d * pi / 180

||| Multiply a vector by a scalar.
mulSV : Double -> Vector -> Vector
mulSV s (x, y) = (s * x, s * y)

||| Rotate a vector by an angle (in radians). +ve angle is counter-clockwise.
rotateV : Double -> Vector -> Vector
rotateV r (x, y) = 
  (x * cos r - y * sin r,  x * sin r + y * cos r)

||| The initial state of the viewport.
export
viewPortInit : ViewPort
viewPortInit = MkViewPort (0, 0) 0 1

||| Translates, rotates, and scales an image according to the 'ViewPort'.
export
applyViewPortToPicture : ViewPort -> Picture -> Picture
applyViewPortToPicture (MkViewPort (transX, transY) vrotate vscale)
  = Scale vscale vscale . Rotate vrotate . Translate transX transY

||| Takes a point using screen coordinates, and uses the `ViewPort` to convert
|||   it to Picture coordinates. This is the inverse of `applyViewPortToPicture` 
|||   for points.
export
invertViewPort : ViewPort -> Point -> Point
invertViewPort (MkViewPort vtrans vrotate vscale) pos
  = rotateV (degToRad vrotate) (mulSV (1 / vscale) pos) - vtrans