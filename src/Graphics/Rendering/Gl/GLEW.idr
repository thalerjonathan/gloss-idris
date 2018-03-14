module Graphics.Rendering.Gl.GLEW

%include C "gl_idris.h"
%link C "gl_idris.o"

||| initializes GLEW
export
glewInit : IO Int 
glewInit = foreign FFI_C "idr_init_glew" (IO Int)