module Graphics.Gloss.Internals.Interface.Game

import Data.IORef

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Event
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import Graphics.Gloss.Internals.Interface.Simulate.State      as SM
import Graphics.Gloss.Internals.Interface.Animate.State       as AN
import Graphics.Gloss.Internals.Rendering.State               as RS

displayFun :   IORef world
            -> IORef ViewPort
            -> IORef RS.State
            -> Color
            -> (world -> IO Picture) 
            -> IORef GLFWState 
            -> IO ()
displayFun worldSR viewSR renderSR backgroundColor worldToPicture backendRef = do
  -- convert the world to a picture
  world           <- readIORef worldSR
  picture         <- worldToPicture world

  -- display the picture in the current view
  renderS         <- readIORef renderSR
  viewPort        <- readIORef viewSR

  windowSize <- getWindowDimensions backendRef

  -- render the frame
  displayPicture
          windowSize
          backgroundColor
          renderS
          (viewPortScale viewPort)
          (applyViewPortToPicture viewPort picture)

  -- idris doesn't have a GC
  -- perform GC every frame to try and avoid long pauses
  -- performGC

handle_keyMouse : Backend GLFWState
                => IORef a
                -> t
                -> (Event -> a -> IO a)
                -> IORef GLFWState -> Key -> KeyState -> Modifiers -> (Int,Int) -> IO () -- KeyboardMouseCallback
handle_keyMouse worldRef _ eventFn backendRef key keyState keyMods pos = do  
  ev         <- keyMouseEvent backendRef key keyState keyMods pos
  world      <- readIORef worldRef
  world'     <- eventFn ev world
  writeIORef worldRef world'

||| Callback for KeyMouse events.
callback_keyMouse :  IORef world                  -- ^ ref to world state
                  -> IORef ViewPort
                  -> (Event -> world -> IO world) -- ^ fn to handle input events
                  -> Callback
callback_keyMouse worldRef viewRef eventFn
  = KeyMouse (handle_keyMouse worldRef viewRef eventFn)

handle_motion :  Backend GLFWState 
              => IORef a
              -> (Event -> a -> IO a)
              -> IORef GLFWState -> (Int,Int) -> IO () -- MotionCallback
handle_motion worldRef eventFn backendRef pos = do
  ev       <- motionEvent backendRef pos
  world    <- readIORef worldRef
  world'   <- eventFn ev world
  writeIORef worldRef world'

||| Callback for Motion events.
callback_motion :  IORef world                  -- ^ ref to world state
                -> (Event -> world -> IO world) -- ^ fn to handle input events
                -> Callback
callback_motion worldRef eventFn
  = Motion (handle_motion worldRef eventFn)

handle_reshape :  Backend GLFWState
               => IORef world
               -> (Event -> world -> IO world)
               -> IORef GLFWState -> (Int,Int) -> IO () -- ReshapeCallback
handle_reshape worldRef eventFn stateRef (width,height) = do
  world  <- readIORef worldRef
  world' <- eventFn (EventResize (width, height)) world
  writeIORef worldRef world'
  viewState_reshape stateRef (width, height)

||| Callback for Handle reshape event.
callback_reshape :  IORef world
                 -> (Event -> world -> IO world)
                 -> Callback
callback_reshape worldRef eventFN
  = Reshape (handle_reshape worldRef eventFN)

export
playWithBackendIO :  Backend GLFWState
                  => GLFWState            -- ^ Initial state of the backend
                  -> Display      -- ^ Display mode.
                  -> Color        -- ^ Background color.
                  -> Int          -- ^ Number of simulation steps to take for each second of real time.
                  -> world        -- ^ The initial world.
                  -> (world -> IO Picture)   
                                  -- ^ A function to convert the world to a picture.
                  -> (Event -> world -> IO world) 
                                  -- ^ A function to handle input events.
                  -> (Double -> world -> IO world) 
                                  -- ^ A function to step the world one iteration.
                                  --   It is passed the period of time (in seconds) needing to be advanced.
                  -> Bool         -- ^ Whether to use the callback_exit or not.
                  -> IO ()
playWithBackendIO
        backend
        display
        backgroundColor
        simResolution
        worldStart
        worldToPicture
        worldHandleEvent
        worldAdvance
        withCallbackExit = do
  let singleStepTime      = 1

  -- make the simulation state
  stateSR         <- newIORef $ SM.stateInit simResolution

  -- make a reference to the initial world
  worldSR         <- newIORef worldStart

  -- make the initial GL view and render states
  viewSR          <- newIORef viewPortInit
  animateSR       <- newIORef AN.stateInit
  renderS_        <- initState
  renderSR        <- newIORef renderS_

  let callbacks = [ 
      Display      (animateBegin animateSR)
    , Display      (displayFun worldSR viewSR renderSR backgroundColor worldToPicture)
    , Display      (animateEnd   animateSR)
    , Idle         (callback_simulate_idle 
                                    stateSR animateSR (readIORef viewSR)
                                    worldSR (\_ => worldAdvance)
                                    singleStepTime)
    , callback_keyMouse worldSR viewSR worldHandleEvent
    , callback_motion   worldSR worldHandleEvent
    , callback_reshape  worldSR worldHandleEvent]

  let exitCallback
            = if withCallbackExit then [callback_exit backend] else []

  createWindow 
    backend 
    display 
    backgroundColor 
    (callbacks ++ exitCallback)
    (\_ => pure ())