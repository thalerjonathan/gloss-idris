||| Data type for representing colors.
module Graphics.Gloss.Internals.Data.Color

--import Data.Data


||| An abstract color value.
|||      We keep the type abstract so we can be sure that the components
|||      are in the required range. To make a custom color use 'makeColor'.
public export
data Color =
        ||| Holds the color components. All components lie in the range [0..1.
        RGBA Double Double Double Double
        -- TODO: port to idris
        -- deriving (Show, Eq, Data, Typeable)

Num Color where
 (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1

 (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1

 fromInteger i
  = let f = fromInteger i
    in  RGBA f f f 1

Abs Color where
 abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1

Neg Color where
  -- added because idris Neg interface requires the implementation of negate
  negate (RGBA r g b a) = (RGBA (-r) (-g) (-b) (-a))
  (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1

||| Take the RGBA components of a color.
export
rgbaOfColor : Color -> (Double, Double, Double, Double)
rgbaOfColor (RGBA r g b a) = (r, g, b, a)

||| Clamp components of a raw color into the required range.
export
clampColor : Color -> Color
clampColor cc
 = let (r, g, b, a) = rgbaOfColor cc
   in   RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)

||| Make a custom color. All components are clamped to the range  [0..1].
||| @ r Red component.
||| @ g Green component.
||| @ b Blue component.
||| @ a Alpha component.
export
makeColor :  (r : Double)
          -> (g : Double)
          -> (b : Double)
          -> (a : Double)
          -> Color
makeColor r g b a 
  = clampColor $ RGBA r g b a

||| Make a custom color. All components are clamped to the range [0..255].
export
makeColorI : Int -> Int -> Int -> Int -> Color
makeColorI r g b a 
  = clampColor $ RGBA (cast r / 255) 
                      (cast g / 255)
                      (cast b / 255)
                      (cast a / 255)

||| Make a custom color. 
|||
|||   Using this function over `makeColor` avoids clamping the components,
|||   which saves time. However, if the components are out of range then
|||   this will result in integer overflow at rendering time, and the actual
|||   picture you get will be implementation dependent. 
|||
|||   You'll only need to use this function when using the @gloss-raster@
|||   package that builds a new color for every pixel. If you're just working
|||   with the Picture data type then it there is no need for raw colors.
|||
export
makeRawColor : Double -> Double -> Double -> Double -> Color
makeRawColor r g b a = RGBA r g b a

||| Make a custom color, taking pre-clamped components.
export
makeRawColorI : Int -> Int -> Int -> Int -> Color
makeRawColorI r g b a = RGBA (cast r / 255) 
                             (cast g / 255)
                             (cast b / 255)
                             (cast a / 255)