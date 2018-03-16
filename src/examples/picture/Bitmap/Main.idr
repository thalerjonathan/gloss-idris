import Graphics.Gloss

-- import Effect.System

{-
import Codec.BMP
import System.Environment
-}

-- TODO: can we somehow get rid of this?
%flag C "-I/usr/include/libdrm -I/usr/include/libpng16 "
%flag C "-lGLEW -lGLU -lGL -lpng16 -lz "

frame : Int -> Int -> Picture -> Double -> Picture
frame width height picture t
  = Color (greyN (abs $ sin (t * 2)))
  $ Pictures 
          [rectangleSolid (cast width) (cast height)
          , picture]

run : String -> IO ()
run fileName = do
  pic <- loadPNG fileName

  let (Bitmap width height _ _) = pic

  animate (InWindow fileName (width, height) (10,  10))
          black (frame width height pic)

||| Displays 32 bit PNG images.
main : IO ()
main = do
  args <- getArgs
  case args of
    [_, fileName] => run fileName
    _ => putStr 
      $  unlines [ "usage: bitmap <file.png>"
                , "  file.png should be a 32-bit PNG file" ]