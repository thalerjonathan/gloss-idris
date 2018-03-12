module Graphics.Gloss.Internals.Interface.Display

import Data.IORef

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewState.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewState.Motion
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Rendering.State
import Graphics.Gloss.Rendering

renderFun : Backend GLFWState 
          => IORef ViewState
          -> IORef State
          -> Color
          -> IO Picture
          -> IORef GLFWState 
          -> IO ()
renderFun viewSR renderSR background makePicture backendRef = do
  port       <- viewStateViewPort <$> readIORef viewSR
  options    <- readIORef renderSR
  windowSize <- getWindowDimensions backendRef
  picture    <- makePicture

  displayPicture 
          windowSize
          background
          options
          (viewPortScale port)
          (applyViewPortToPicture port picture)

||| 
||| @ backend       Initial state of the backend.
||| @ displayMode   Display config.
||| @ background    Background color.
||| @ makePicture   Make the picture to draw.
||| @ eatController Eat the controller
export
displayWithBackend  : Backend GLFWState 
                   => (backend : GLFWState)
                   -> (displayMode : Display)
                   -> (background : Color)
                   -> (makePicture : IO Picture)
                   -> (eatController : (Controller -> IO ()))
                   -> IO ()
displayWithBackend backend displayMode background makePicture eatController = pure () {- do
  viewSR   <- newIORef viewStateInit
  renderS  <- initState
  renderSR <- newIORef renderS

  let callbacks = [
      Display (renderFun viewSR renderSR background makePicture)
    -- Escape exits the program
    , callback_exit () 
    -- Viewport control with mouse
    , callback_viewState_keyMouse viewSR
    , callback_viewState_motion   viewSR
    , callback_viewState_reshape ]
  
  -- When we create the window we can pass a function to get a
  -- reference to the backend state. Using this we make a controller
  -- so the client can control the window asynchronously.
  createWindow backend displayMode background callbacks
    $ \backendRef
      => eatController
          $ MkController
              (postRedisplay backendRef)
              (\modViewPort => do
                  viewState <- readIORef viewSR
                  port'     <- modViewPort $ viewStateViewPort viewState
                  let viewState' = record { viewStateViewPort = port' } viewState
                  writeIORef viewSR viewState'
                  postRedisplay backendRef)
                  -}