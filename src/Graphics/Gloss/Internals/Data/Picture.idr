||| Data types for representing pictures.
module Graphics.Gloss.Internals.Data.Picture

import Graphics.Gloss.Internals.Codec.PNG
import Graphics.Gloss.Internals.Data.Color
import Graphics.Gloss.Internals.Rendering.Bitmap

||| A point on the x-y plane.
public export
Point : Type
Point = (Double, Double)

||| Pretend a point is a number.
|||      Vectors aren't real numbers according to Haskell, because they don't
|||      support the multiply and divide field operators. We can pretend they
|||      are though, and use the (+) and (-) operators as component-wise
|||      addition and subtraction.
public export
Num Point where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  fromInteger x         = (fromInteger x, fromInteger x)

public export
Abs Point where
  abs (x, y) = (abs x, abs y)

public export
Neg Point where
  (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  negate (x, y)         = (negate x, negate y)  

||| A vector can be treated as a point, and vis-versa.
public export
Vector : Type
Vector = Point

||| A path through the x-y plane.
public export
Path : Type
Path = List Point

||| A 2D picture
public export
data Picture =
  |||Primitives -------------------------------------

  ||| A blank picture, with nothing in it.
  Blank        |

  ||| A convex polygon filled with a solid color.
  Polygon Path |
  
  ||| A line along an arbitrary path.
  Line Path    |

  ||| A circle with the given radius.
  Circle Double |

  ||| A circle with the given thickness and radius.
  |||  If the thickness is 0 then this is equivalent to `Circle`.
  ThickCircle Double Double |

  ||| A circular arc drawn counter-clockwise between two angles 
  |||  (in degrees) at the given radius.
  Arc Double Double Double |

  ||| A circular arc drawn counter-clockwise between two angles 
  |||  (in degrees), with the given radius  and thickness.
  |||   If the thickness is 0 then this is equivalent to `Arc`.
  ThickArc Double Double Double Double |

  ||| Some text to draw with a vector font.
  Text String |

  ||| A bitmap image with a width, height and some 32-bit RGBA
  |||  bitmap data.
  |||
  ||| The boolean flag controls whether Gloss should cache the data
  ||| in GPU memory between frames. If you are programatically generating
  ||| the image for each frame then use @False@. If you have loaded it
  ||| from a file then use @True@. 
  ||| Setting @False@ for static images will make rendering slower
  ||| than it needs to be.
  ||| Setting @True@  for dynamically generated images will cause a
  ||| GPU memory leak.
  Bitmap Int Int BitmapData Bool |

  ||| Color ------------------------------------------
  ||| A picture drawn with this color.
  Color Color Picture |

  ||| Transforms -------------------------------------
  ||| A picture translated by the given x and y coordinates.
  Translate Double Double Picture |

  ||| A picture rotated clockwise by the given angle (in degrees).
  Rotate Double Picture |

  ||| A picture scaled by the given x and y factors.
  Scale Double Double Picture |

  ||| More Pictures ----------------------------------
  ||| A picture consisting of several others.
  Pictures (List Picture)
  -- deriving (Show, Eq, Data, Typeable)

||| Instances ------------------------------------------------------------------
export
Semigroup Picture where
  (<+>) a b = Pictures [a, b]

export
Monoid Picture where
  neutral = Blank
  --mempty      = Blank
  --mappend a b = Pictures [a, b]
  --mconcat     = Pictures

||| Bitmaps --------------------------------------------------------------------
||| O(1). Use a `ForeignPtr` of RGBA data as a bitmap with the given
|||  width and height.
|||
|||  The boolean flag controls whether Gloss should cache the data
|||  between frames for speed. If you are programatically generating
|||  the image for each frame then use `False`. If you have loaded it
|||  from a file then use `True`.
export
bitmapOfForeignPtr : Int -> Int -> BitmapFormat -> Ptr -> Bool -> Picture
bitmapOfForeignPtr width height fmt ptr cacheMe
 = let  len     = width * height * 4
        bdata   = MkBitmapData len fmt ptr
   in   Bitmap width height bdata cacheMe

{-
||| O(size). Copy a `ByteString` of RGBA data into a bitmap with the given
|||  width and height.
|||
|||  The boolean flag controls whether Gloss should cache the data
|||  between frames for speed. If you are programatically generating
|||  the image for each frame then use `False`. If you have loaded it
|||  from a file then use `True`.
export
bitmapOfByteString : Int -> Int -> BitmapFormat -> Buffer -> Bool -> Picture
bitmapOfByteString width height fmt buffer cacheMe
 = unsafePerformIO $ do
  let len = width * height * 4
  ptr  <- mallocBytes len
  fptr <- newForeignPtr finalizerFree ptr

  BSU.unsafeUseAsCString bs
    $ \cstr => copyBytes ptr (castPtr cstr) len


  let bdata = BitmapData len fmt fptr
  return $ Bitmap width height bdata cacheMe
{-# NOINLINE bitmapOfByteString #-}
-}

||| O(size). Copy a `PNG` file into a bitmap.
-- TODO: respect format? what if the PNG has no alpha channel?
export
bitmapOfPNG : PNG -> Picture
bitmapOfPNG png =
  let width  = pngWidth png
      height = pngHeight png
      format = pngFormat png
      ptr    = pngDataRaw png

      len    = width * height * 4

      bdata  = MkBitmapData len (MkBitmapFormat BottomToTop PxRGBA) ptr
  in  Bitmap width height bdata True

    
{-
 = unsafePerformIO
 $ do   let (width, height)     = bmpDimensions bmp
        let bs                  = unpackBMPToRGBA32 bmp
        let len                 = width * height * 4

        ptr     <- mallocBytes len
        fptr    <- newForeignPtr finalizerFree ptr

        BSU.unsafeUseAsCString bs
         $ \cstr => copyBytes ptr (castPtr cstr) len

        let bdata = BitmapData len (BitmapFormat BottomToTop PxRGBA) fptr

        pure $ Bitmap width height bdata True
        -}
{-# NOINLINE bitmapOfBMP #-}

||| Load an uncompressed 24 or 32bit RGBA PNG file as a bitmap.
export
-- TODO: proper error handling
loadPNG : String -> IO Picture
loadPNG filePath = do
  epng <- readPNG filePath
  case epng of
    Left err  => do
      print err 
      pure Blank
    Right png => pure $ bitmapOfPNG png