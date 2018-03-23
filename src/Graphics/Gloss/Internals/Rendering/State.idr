||| Rendering options
module Graphics.Gloss.Internals.Rendering.State

import Data.IORef
import Graphics.Gloss.Internals.Data.Picture
import Graphics.Gloss.Internals.Rendering.Bitmap
import Graphics.Rendering.OpenGL.Internal.Types

||| A texture that we've sent to OpenGL.
public export
record Texture where
  constructor MkTexture
  ||| Stable name derived from the `BitmapData` that the user gives us.
  texName : Int
  ||| Width of the image, in pixels.
  texWidth : Int
  ||| Height of the image, in pixels.
  texHeight : Int
  ||| Pointer to the Raw texture data.
  texData : Ptr
  ||| The OpenGL texture object.
  texObject : GLuint
  ||| Whether we want to leave this in OpenGL texture memory between frames.
  texCacheMe  : Bool

||| Abstract Gloss render state which holds references to textures
|||  loaded into the GPU context.
public export
record State where
  constructor MkState
  ||| Whether to use color
  stateColor : Bool
  ||| Whether to force wireframe mode only
  stateWireframe : Bool
  ||| Whether to use alpha blending
  stateBlendAlpha : Bool
  ||| Whether to use line smoothing
  stateLineSmooth : Bool 
  ||| Cache of Textures that we've sent to OpenGL.
  stateTextures : IORef (List Texture)

||| A mutable render state holds references to the textures currently loaded
|||   into the OpenGL context. To ensure that textures are cached in GPU memory,
|||   pass the same `State` each time you call `displayPicture` or `renderPicture`.
export
initState : IO State
initState = do
  textures <- newIORef []
  pure $ MkState 
          True      -- stateColor
          False     -- stateWireframe
          True      -- stateBlendAlpha
          False     -- stateLineSmooth
          textures  -- stateTextures
