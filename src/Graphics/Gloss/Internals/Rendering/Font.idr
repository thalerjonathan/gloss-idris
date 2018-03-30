module Graphics.Gloss.Internals.Rendering.Font

%include C "gloss_idris.h"
%link C "gloss_idris.o"

export
initFontRendering : IO ()
initFontRendering 
  = foreign FFI_C "initFontRendering" (IO ())

export
renderString : String -> IO ()
renderString str
  = foreign FFI_C "renderString" (String -> IO ()) str