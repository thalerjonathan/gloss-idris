module Main

-- Draw a color wheel.
import Graphics.Gloss

import Graphics.UI.GLFW.Utils.GlfwConfig

-- TODO: can we somehow get rid of this?
%flag C "-I/usr/include/libdrm -I/usr/include/libpng16 "
%flag C "-lGLEW -lGLU -lGL -lpng16 -lz "

main : IO ()
main = do
    display 
        (InWindow  "Colors" (800, 800) (5, 5))
        (greyN 0.4)
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

    pics : Picture
    pics = Pictures [ Translate (200 * cos (2 * pi * (cast n) / 12)) (200 * sin (2 * pi * (cast n) / 12)) 
              (Color (withAlpha 0.8 c) (circleSolid 100)) | n <- [0 .. length colors], c <- colors ]
