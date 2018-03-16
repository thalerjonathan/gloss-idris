import Graphics.Gloss

-- TODO: can we somehow get rid of this?
%flag C "-I/usr/include/libdrm -I/usr/include/libpng16 "
%flag C "-lGLEW -lGLU -lGL -lpng16 -lz "

main : IO ()
main = display (InWindow "My Window" (200, 200) (10, 10)) white (Circle 80)