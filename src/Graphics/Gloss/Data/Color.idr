||| Predefined and custom colors.
module Graphics.Gloss.Data.Color

import Data.Vect 

import Graphics.Gloss.Rendering

-------------------------------------------------------------------------------
||| Normalise a color to the value of its largest RGB component.
normalizeColor : Double -> Double -> Double -> Double -> Color
normalizeColor r g b a
 = let  m               = maximum [r, g, b]
   in   makeColor (r / m) (g / m) (b / m) a
  where
    maximum : Vect n Double -> Double
    maximum [x] = x
    maximum (x :: xs) = let max = maximum xs
                        in if x > max
                            then x
                            else max

-- Color functions ------------------------------------------------------------
-- Mix two colors with the given ratios.
export
mixColors  : Double        -- ^ Proportion of first color.
          -> Double        -- ^ Proportion of second color.
          -> Color        -- ^ First color.
          -> Color        -- ^ Second color.
          -> Color        -- ^ Resulting color.
mixColors m1 m2 c1 c2
 = let  (r1, g1, b1, a1) = rgbaOfColor c1
        (r2, g2, b2, a2) = rgbaOfColor c2

        -- Normalise mixing proportions to ratios.
        m12 = m1 + m2
        m1' = m1 / m12
        m2' = m2 / m12

        -- Colors components should be added via sum of squares,
        -- otherwise the result will be too dark.
        r1s = r1 * r1;    r2s = r2 * r2
        g1s = g1 * g1;    g2s = g2 * g2
        b1s = b1 * b1;    b2s = b2 * b2

   in   makeColor
                (sqrt (m1' * r1s + m2' * r2s))
                (sqrt (m1' * g1s + m2' * g2s))
                (sqrt (m1' * b1s + m2' * b2s))
                ((m1 * a1   + m2 * a2) / m12)

||| Add RGB components of a color component-wise,
|||   then normalise them to the highest resulting one. 
|||   The alpha components are averaged.
export
addColors : Color -> Color -> Color
addColors c1 c2
 = let  (r1, g1, b1, a1) = rgbaOfColor c1
        (r2, g2, b2, a2) = rgbaOfColor c2

   in   normalizeColor 
                (r1 + r2)
                (g1 + g2)
                (b1 + b2)
                ((a1 + a2) / 2)

||| Make a dimmer version of a color, scaling towards black.
export
dim : Color -> Color
dim c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r / 1.2) (g / 1.2) (b / 1.2) a
        
||| Make a brighter version of a color, scaling towards white.
export
bright : Color -> Color
bright c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r * 1.2) (g * 1.2) (b * 1.2) a

||| Lighten a color, adding white.
export
light : Color -> Color
light c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r + 0.2) (g + 0.2) (b + 0.2) a
        
||| Darken a color, adding black.
export
dark : Color -> Color
dark c
 = let  (r, g, b, a)    = rgbaOfColor c
   in   makeColor (r - 0.2) (g - 0.2) (b - 0.2) a

-------------------------------------------------------------------------------
||| Set the red value of a `Color`.
export
withRed : Double -> Color -> Color
withRed r c
 = let  (_, g, b, a) = rgbaOfColor c
   in   makeColor r g b a

||| Set the green value of a `Color`.
export
withGreen : Double -> Color -> Color
withGreen g c
 = let  (r, _, b, a) = rgbaOfColor c
   in   makeColor r g b a

||| Set the blue value of a `Color`.
export
withBlue : Double -> Color -> Color
withBlue b c
 = let  (r, g, _, a) = rgbaOfColor c
   in   makeColor r g b a

||| Set the alpha value of a `Color`.
export
withAlpha : Double -> Color -> Color
withAlpha a c
 = let  (r, g, b, _) = rgbaOfColor c
   in   makeColor r g b a

-- Pre-defined Colors ---------------------------------------------------------
||| A greyness of a given order.
||| 
|||   Range is 0 = black, to 1 = white.
export
greyN   : Double -> Color
greyN n = makeRawColor n   n   n   1.0

export
black : Color
black = makeRawColor 0.0 0.0 0.0 1.0

export
white : Color
white = makeRawColor 1.0 1.0 1.0 1.0

-- Colors from the additive color wheel.
export
red : Color
red = makeRawColor 1.0 0.0 0.0 1.0
export
green : Color
green = makeRawColor 0.0 1.0 0.0 1.0
export
blue : Color
blue = makeRawColor 0.0 0.0 1.0 1.0

-- secondary
export
yellow : Color
yellow = addColors red green
export
cyan : Color
cyan = addColors green blue
export
magenta : Color
magenta = addColors red blue

-- tertiary
export
rose : Color
rose = addColors red magenta
export
violet : Color
violet = addColors magenta blue
export
azure : Color
azure  = addColors blue cyan
export
aquamarine : Color
aquamarine = addColors cyan green
export
chartreuse : Color
chartreuse = addColors green yellow
export
orange : Color
orange = addColors yellow red
