module Main

-- Draw a color wheel.
import Graphics.Gloss

import Config

main : IO ()
main = do
    display 
        (InWindow  "Colors" (800, 800) (5, 5))
        (greyN 0.4)
        --(Color (withAlpha 0.8 orange) (circleSolid 100))
        pics
  where
    colors : List Color
    colors
      = [ red
        , orange
        , yellow
        , chartreuse
        , green
        , aquamarine
        , cyan
        , azure
        , blue
        , violet
        , magenta
        , rose
        ]

    ncs : List (Nat, Color)
    ncs = zip [0 .. length colors] colors

    pics : Picture
    pics = Pictures [ Translate (200 * cos (2 * pi * (cast n) / 12)) (200 * sin (2 * pi * (cast n) / 12)) 
              (Color (withAlpha 0.8 c) (circleSolid 100)) | (n, c) <- ncs ]

