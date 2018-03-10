module Graphics.Gloss.Data.Picture

import public Graphics.Gloss.Rendering
import        Graphics.Gloss.Geometry.Angle

-- Constructors ----------------------------------------------------------------
-- NOTE: The docs here should be identical to the ones on the constructors.

||| A blank picture, with nothing in it.
export
blank : Picture
blank = Blank

||| A convex polygon filled with a solid color.
export
polygon : Path -> Picture
polygon = Polygon

||| A line along an arbitrary path.
export
line : Path -> Picture
line = Line

||| A circle with the given radius.
export
circle : Double  -> Picture
circle = Circle

||| A circle with the given thickness and radius.
|||   If the thickness is 0 then this is equivalent to `Circle`.
export
thickCircle : Double -> Double -> Picture
thickCircle = ThickCircle

||| A circular arc drawn counter-clockwise between two angles (in degrees) 
|||   at the given radius.
export
arc : Double -> Double -> Double -> Picture
arc = Arc

||| A circular arc drawn counter-clockwise between two angles (in degrees),
|||  with the given radius  and thickness.
|||  If the thickness is 0 then this is equivalent to `Arc`.
export
thickArc : Double -> Double -> Double -> Double -> Picture
thickArc = ThickArc

||| Some text to draw with a vector font.
export
text : String -> Picture
text = Text

||| A bitmap image with a width, height and a Vector holding the 
|||   32-bit RGBA bitmap data.
||| 
|||  The boolean flag controls whether Gloss should cache the data
|||  between frames for speed.
|||  If you are programatically generating the image for
|||  each frame then use `False`.  
|||  If you have loaded it from a file then use `True`.
export
bitmap : Int -> Int -> BitmapData -> Bool -> Picture
bitmap = Bitmap

||| A picture drawn with this color.
export
color : Color -> Picture -> Picture
color = Color

||| A picture translated by the given x and y coordinates.
export
translate : Double -> Double -> Picture -> Picture
translate = Translate

||| A picture rotated clockwise by the given angle (in degrees).
export
rotate : Double -> Picture -> Picture
rotate = Rotate

||| A picture scaled by the given x and y factors.
export
scale : Double -> Double -> Picture -> Picture
scale = Scale

||| A picture consisting of several others.
export
pictures : List Picture -> Picture
pictures = Pictures

-- Other Shapes ---------------------------------------------------------------
||| A closed loop along a path.
export
lineLoop : Path -> Picture
lineLoop []        = Line []
lineLoop (x :: xs) = Line ((x :: xs) ++ [x])

-- Circles and Arcs -----------------------------------------------------------
||| A solid circle with the given radius.
export
circleSolid : Double -> Picture
circleSolid r 
  = thickCircle (r/2) r

||| A solid arc, drawn counter-clockwise between two angles at the given radius.
export
arcSolid : Double -> Double -> Double -> Picture
arcSolid a1 a2 r 
  = thickArc a1 a2 (r/2) r 

||| A wireframe sector of a circle. 
|||  An arc is draw counter-clockwise from the first to the second angle at
|||  the given radius. Lines are drawn from the origin to the ends of the arc.
|||
|||  NOTE: We take the absolute value of the radius incase it's negative.
|||  It would also make sense to draw the sector flipped around the 
|||  origin, but I think taking the absolute value will be less surprising
|||  for the user.
|||
export
sectorWire : Double -> Double -> Double -> Picture
sectorWire a1 a2 r_
 = let r        = abs r_
   in  Pictures 
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))] ]

-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

||| A path representing a rectangle centered about the origin
export
rectanglePath 
        : Double        -- ^ width of rectangle
        -> Double        -- ^ height of rectangle
        -> Path
rectanglePath sizeX sizeY                       
 = let  sx      = sizeX / 2
        sy      = sizeY / 2
   in   [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]

||| A wireframe rectangle centered about the origin.
export
rectangleWire : Double -> Double -> Picture
rectangleWire sizeX sizeY
  = lineLoop $ rectanglePath sizeX sizeY

||| A path representing a rectangle in the y > 0 half of the x-y plane.
export
rectangleUpperPath : Double -> Double -> Path
rectangleUpperPath sizeX sy
 = let  sx      = sizeX / 2
   in   [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]

||| A wireframe rectangle in the y > 0 half of the x-y plane.
export
rectangleUpperWire : Double -> Double -> Picture
rectangleUpperWire sizeX sizeY
  = lineLoop $ rectangleUpperPath sizeX sizeY

||| A solid rectangle centered about the origin.
export
rectangleSolid : Double -> Double -> Picture
rectangleSolid sizeX sizeY
  = Polygon $ rectanglePath sizeX sizeY

||| A solid rectangle in the y > 0 half of the x-y plane.
export
rectangleUpperSolid : Double -> Double -> Picture
rectangleUpperSolid sizeX sizeY
  = Polygon  $ rectangleUpperPath sizeX sizeY