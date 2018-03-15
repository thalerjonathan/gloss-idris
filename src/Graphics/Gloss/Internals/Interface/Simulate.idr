module Graphics.Gloss.Internals.Interface.Simulate

import Data.IORef

import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Rendering
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewState.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewState.Motion
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import Graphics.Gloss.Internals.Interface.Simulate.State      as SM
import Graphics.Gloss.Internals.Interface.Animate.State       as AN
import Graphics.Gloss.Internals.Rendering.State               as RS

displayFun :   IORef model
            -> IORef ViewState
            -> IORef RS.State
            -> Color
            -> (model -> IO Picture) 
            -> IORef GLFWState 
            -> IO ()
displayFun worldSR viewSR renderSR backgroundColor worldToPicture backendRef = do
  -- convert the world to a picture
  world           <- readIORef worldSR
  port            <- viewStateViewPort <$> readIORef viewSR
  picture         <- worldToPicture world

  -- display the picture in the current view
  renderS         <- readIORef renderSR

  windowSize      <- getWindowDimensions backendRef

  -- render the frame
  displayPicture
          windowSize
          backgroundColor
          renderS
          (viewPortScale port)
          (applyViewPortToPicture port picture)

  -- no GC in idris
  -- perform GC every frame to try and avoid long pauses
  -- performGC

export
simulateWithBackendIO
        :  Backend GLFWState
        => GLFWState            -- ^ Initial state of the backend
        -> Display      -- ^ Display mode.
        -> Color        -- ^ Background color.
        -> Int          -- ^ Number of simulation steps to take for each second of real time.
        -> model        -- ^ The initial model.
        -> (model -> IO Picture) 
                -- ^ A function to convert the model to a picture.
        -> (ViewPort -> Double -> model -> IO model) 
                -- ^ A function to step the model one iteration. It is passed the
                --     current viewport and the amount of time for this simulation
                --     step (in seconds).
        -> IO ()
simulateWithBackendIO
        backend
        display
        backgroundColor
        simResolution
        worldStart
        worldToPicture
        worldAdvance = do
  let singleStepTime      = 1

  -- make the simulation state
  stateSR         <- newIORef $ SM.stateInit simResolution

  -- make a reference to the initial world
  worldSR         <- newIORef worldStart

  -- make the initial GL view and render states
  viewSR          <- newIORef viewStateInit
  animateSR       <- newIORef AN.stateInit
  renderS_        <- initState
  renderSR        <- newIORef renderS_

  let callbacks = [ 
      Display      (animateBegin animateSR)
    , Display      (displayFun worldSR viewSR renderSR backgroundColor worldToPicture)
    , Display      (animateEnd   animateSR)
    , Idle         (callback_simulate_idle 
                                    stateSR animateSR
                                    (viewStateViewPort <$> readIORef viewSR)
                                    worldSR worldAdvance
                                    singleStepTime)
    , callback_exit backend 
    , callback_viewState_keyMouse viewSR
    , callback_viewState_motion   viewSR
    , callback_viewState_reshape ]

  createWindow backend display backgroundColor
          callbacks
          (const (pure ()))