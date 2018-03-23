import Graphics.Gloss

import Config

frame : Int -> Int -> Picture -> Double -> Picture
frame width height picture t
  = Color (greyN (abs $ sin (t * 2)))
  $ Pictures 
      [rectangleSolid (cast width) (cast height)
      , picture]

run : String -> IO ()
run fileName = do
  pic <- loadPNG fileName

  let (Bitmap width height bd flag) = pic

  putStrLn $ "width = " ++ show width
  putStrLn $ "height = " ++ show height
  putStrLn $ "bitmapdata = " ++ show  bd

  animate 
    (InWindow fileName (width, height) (10,  10))
    black
    (frame width height pic)

||| Displays 32 bit PNG images.
main : IO ()
main = do
  args <- getArgs
  case args of
    [_, fileName] => run fileName
    _ => putStr 
      $  unlines [ "usage: bitmap <file.png>"
                , "  file.png should be a 32-bit PNG file" ]