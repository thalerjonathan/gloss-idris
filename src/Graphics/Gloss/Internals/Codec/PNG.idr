module Graphics.Gloss.Internals.Codec.PNG

import CFFI.Memory
import CFFI.Types

%include C "gloss_idris.h"
%link C "gloss_idris.o"

-- followed https://groups.google.com/forum/#!topic/idris-lang/EiYRVBENwjc

public export
data PNGFormat = RGB | RGBA 

public export
record PNG where
  constructor MkPNG
  pngWidth   : Int
  pngHeight  : Int
  pngFormat  : PNGFormat
  pngDataRaw : Ptr
  pngRaw     : Ptr

export
-- TODO: need a proper error handling
readPNG : String -> IO (Either String PNG)
readPNG filename = do
    pngLoad <- readPNGForeign
    let pngLoadCptr = toCPtr pngLoad

    wInt32  <- peek I32 $ field pngLoadStruct 0 pngLoadCptr
    hInt32  <- peek I32 $ field pngLoadStruct 1 pngLoadCptr
    fInt32  <- peek I32 $ field pngLoadStruct 2 pngLoadCptr
    dataPtr <- peek PTR $ field pngLoadStruct 3 pngLoadCptr

    let w = prim__zextB32_Int wInt32
    let h = prim__zextB32_Int hInt32
    let f = prim__zextB32_Int fInt32

    let pf = 
      if f == 0
        then RGB
        else RGBA

    pure $ Right $ MkPNG w h pf dataPtr pngLoad

  where
    readPNGForeign : IO Ptr
    readPNGForeign = foreign FFI_C "png_load" (String -> IO Ptr) filename

    pngLoadStruct : Composite
    pngLoadStruct = STRUCT [I32, I32, I32, PTR]