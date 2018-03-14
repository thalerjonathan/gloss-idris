module Graphics.Gloss.Internals.Rendering.GLUtils

import Graphics.Rendering.OpenGL.Internal.GLBindings
import Graphics.Rendering.OpenGL.Internal.Types

import Graphics.Gloss.Internals.Data.Color

export
clearWithGlossColor : Color -> IO ()
clearWithGlossColor c = do
  let (r,g,b,a) = rgbaOfColor c 
  glClearColor r g b a
    