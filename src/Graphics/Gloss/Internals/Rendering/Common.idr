module Graphics.Gloss.Internals.Rendering.Common 

import Graphics.Rendering.OpenGL.Internal.GLBindings as GL
import Graphics.Rendering.OpenGL.Internal.Types

import Graphics.Gloss.Internals.Data.Color

||| The OpenGL library doesn't seem to provide a nice way convert
|||      a Float to a GLfloat, even though they're the same thing
|||      under the covers.  
|||
|||  Using realToFrac is too slow, as it doesn't get fused in at
|||      least GHC 6.12.1
|||
export
gf : Double -> GLfloat
--gf x = unsafeCoerce x
gf = id
{-# INLINE gf #-}


||| Used for similar reasons to above
export
gsizei : Int -> GLsizei
-- gsizei x = unsafeCoerce x
gsizei = id
{-# INLINE gsizei #-}

||| used for clearing color with the Color type
|||  because need to extract the quadruple
export
glossClearColor : (c : Color) -> IO ()
glossClearColor c = do
    let (r,g,b,a) = rgbaOfColor c
    glClearColor r g b a

export
preservingMatrix : (action : IO ()) -> IO () 
preservingMatrix action = do
  GL.glPushMatrix
  action
  GL.glPopMatrix

||| Set up the OpenGL rendering context for orthographic projection and run an
|||   action to draw the model.
||| @ winSize Width and height of window.
||| @ action  Action to perform.
export
withModelview :  (winSize : (Int, Int))
              -> (action : IO ())
              -> IO ()
withModelview (sizeX, sizeY) action = do
  GL.glMatrixMode GL_PROJECTION
  preservingMatrix $ do
    -- setup the co-ordinate system
    GL.glLoadIdentity
    let (sx, sy)    = ((cast {to=Double} sizeX) / 2, (cast {to=Double} sizeY) / 2)
    GL.glOrtho (-sx) sx (-sy) sy 0 (-100)

    -- draw the world
    -- GL.matrixMode   $= GL.Modelview 0
    GL.glMatrixMode GL_MODELVIEW

    action

    GL.glMatrixMode GL_PROJECTION

  --GL.matrixMode   $= GL.Modelview 0
  GL.glMatrixMode GL_MODELVIEW

||| Clear the OpenGL buffer with the given background color and run 
|||   an action to draw the model.
||| @ clearColor Background color
||| @ action     Action to perform
export
withClearBuffer :  (clearColor : Color)
                -> (action : IO ())
                -> IO ()
withClearBuffer clearColor action = do   
  -- initialization (done every time in this case)
  -- we don't need the depth buffer for 2d.
  --GL.depthFunc    GL.$= Just GL.Always
  GL.glDepthFunc GL_ALWAYS

  -- always clear the buffer to white
  glossClearColor clearColor

  -- on every loop
  -- GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.glClear GL_COLOR_BUFFER_BIT
  GL.glClear GL_DEPTH_BUFFER_BIT
  GL.glColor4f 0 0 0 1

  action