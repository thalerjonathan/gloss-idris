module Graphics.Gloss.Internals.Interface.ViewState.Reshape

import Data.IORef
import Graphics.Rendering.Gl.Gl41

import Graphics.Gloss.Internals.Interface.Backend

export
viewState_reshape : Backend GLFWState => IORef GLFWState -> (Int,Int) -> IO () -- ReshapeCallback
viewState_reshape stateRef (width, height) = do
  -- Setup the viewport
  --      This controls what part of the window openGL renders to.
  --      We'll use the whole window.
  --
  --GL.viewport     $= ( GL.Position 0 0
  --                    , GL.Size (fromIntegral width) (fromIntegral height))
  -- glViewport : GLint -> GLint -> GLsizei -> GLsizei -> IO ()
  glViewport 0 0 width height

  postRedisplay stateRef

||| Callback to handle keyboard and mouse button events
|||     for controlling the viewport.
export
callback_viewState_reshape : Callback
callback_viewState_reshape = Reshape (viewState_reshape)