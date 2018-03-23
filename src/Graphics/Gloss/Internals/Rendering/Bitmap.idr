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
data PixelFormat = PxRGB | PxRGBA

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

public export
Show RowOrder where
  show TopToBottom = "TopToBottom"
  show BottomToTop = "BottomToTop"

public export
Show PixelFormat where
  show PxRGB = "PxRGB"
  show PxRGBA = "PxRGBA"

public export
Show BitmapFormat where
  show (MkBitmapFormat ro pf) = "BitmapFormat rowOrder: " ++ show ro ++ " pixelFormat: " ++ show pf

public export
Show BitmapData where
 show (MkBitmapData l f p) = "BitmapData bitmapDataLength: " ++ show l ++ " bitmapFormat: " ++ show f

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