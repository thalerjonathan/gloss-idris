|||   The main display function.
module Graphics.Gloss.Internals.Interface.Window

import Data.IORef

import Graphics.Rendering.Gl.Gl41

import Graphics.Gloss.Internals.Rendering.GLUtils
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Data.Color
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Debug

||| Open a window and use the supplied callbacks to handle window events.
export
createWindow
        : Backend GLFWState
        => GLFWState
        -> Display
        -> Color                -- ^ Color to use when clearing.
        -> List Callback           -- ^ Callbacks to use.
        -> (IORef GLFWState -> IO ())   -- ^ Give the backend back to the caller before entering the main loop.
        -> IO ()
createWindow
        backend
        display
        clearColor
        callbacks
        eatBackend = do
  -- Turn this on to spew debugging info to stdout
  let debug = True

  -- Initialize backend state
  backendStateRef <- newIORef backend

  when debug
    $ do   putStr  $ "* displayInWindow\n"

  -- Intialize backend
  initializeBackend backendStateRef debug

  -- Here we go!
  when debug
    $ do   putStr  $ "* c window\n\n"

  -- Open window
  openWindow backendStateRef display

  -- Setup callbacks
  installDisplayCallback     backendStateRef callbacks
  installWindowCloseCallback backendStateRef
  installReshapeCallback     backendStateRef callbacks
  installKeyMouseCallback    backendStateRef callbacks
  installMotionCallback      backendStateRef callbacks
  installIdleCallback        backendStateRef callbacks

  -- we don't need the depth buffer for 2d.
  --GL.depthFunc    $= Just GL.Always
  glDepthFunc GL_ALWAYS

  -- always clear the buffer to white
  -- GL.clearColor   $= glColor4OfColor clearColor
  -- glClearColor 1 1 1 1
  clearWithGlossColor clearColor

  -- Dump some debugging info
  when debug
    $ do  dumpBackendState backendStateRef
          dumpFramebufferState
          dumpFragmentState

  eatBackend backendStateRef

  when debug
    $ do   putStr  $ "* entering mainloop..\n"

  -- Start the main backend loop
  runMainLoop backendStateRef

  when debug
    $      putStr  $ "* all done\n"