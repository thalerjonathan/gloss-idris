||| Helper functions for rendering bitmaps
module Graphics.Gloss.Internals.Rendering.Bitmap

import CFFI.Memory

|||  Order of rows in an image are either:
||| 
|||    * `TopToBottom` - 
|||    * `BottomToTop` - the bottom row followed by the next-higher row and so on.
||| 
public export
data RowOrder = 
  ||| the top row, followed by the next-lower row and so on.
  TopToBottom |
  ||| the bottom row followed by the next-higher row and so on.
  BottomToTop

||| Pixel formats describe the order of the color channels in memory.
public export
data PixelFormat = PxRGBA | PxABGR

||| Description of how the bitmap is layed out in memory.
||| 
|||   * Prior version of Gloss assumed `BitmapFormat BottomToTop PxAGBR`
|||
public export
record BitmapFormat where
  constructor MkBitmapFormat
  rowOrder    : RowOrder
  pixelFormat : PixelFormat

||| Abstract 32-bit RGBA bitmap data.
public export
record BitmapData where
  constructor MkBitmapData
  bitmapDataLength : Int  -- length (in bytes)
  bitmapFormat     : BitmapFormat
  bitmapPointer    : Ptr -- Int --(ForeignPtr Word8)

{-
Show BitmapData where
 show _ = "BitmapData"
-}

||| Generates the point path to display the bitmap centred
export
bitmapPathd : Double -> Double -> List (Double, Double)
bitmapPathd width height = [(-width', -height'), (width', -height'), (width', height'), (-width', height')]
  where
    width'  = width  / 2
    height' = height / 2

||| Frees the allocated memory given to OpenGL to avoid a memory leak
export
freeBitmapData : Ptr -> IO ()
freeBitmapData p = mfree p