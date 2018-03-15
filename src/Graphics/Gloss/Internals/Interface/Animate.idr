module Graphics.Gloss.Internals.Interface.Animate

import Data.IORef

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Controller
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
import Graphics.Gloss.Internals.Interface.Animate.State       as AN
import Graphics.Gloss.Internals.Rendering.State as RS

getsIORef : IORef a -> (a -> r) -> IO r
getsIORef ref fun = liftA fun $ readIORef ref

displayFun :   IORef AN.State
            -> IORef ViewState
            -> IORef RS.State
            -> Color
            -> (Double -> IO Picture)
            -> IORef GLFWState 
            -> IO ()
displayFun animateSR viewSR renderSR backColor frameOp backendRef = do
  -- extract the current time from the state
  timeS           <- animateSR `getsIORef` stateAnimateTime

  -- call the user action to get the animation frame
  picture         <- frameOp timeS -- (double2Float timeS)

  renderS         <- readIORef renderSR
  portS           <- viewStateViewPort <$> readIORef viewSR

  windowSize      <- getWindowDimensions backendRef

  -- render the frame
  displayPicture
    windowSize
    backColor
    renderS
    (viewPortScale portS)
    (applyViewPortToPicture portS picture)

  -- no GC in Idris
  -- perform GC every frame to try and avoid long pauses
  -- performGC

export
animateWithBackendIO
        :  Backend GLFWState
        => GLFWState             -- ^ Initial State of the backend
        -> Bool                  -- ^ Whether to allow the image to be panned around.
        -> Display               -- ^ Display mode.
        -> Color                 -- ^ Background color.
        -> (Double -> IO Picture) -- ^ Function to produce the next frame of animation.
                                 --     It is passed the time in seconds since the program started.
        -> (Controller -> IO ()) -- ^ Eat the controller.
        -> IO ()
animateWithBackendIO
        backend pannable display backColor
        frameOp eatController = do   
  -- 
  viewSR          <- newIORef viewStateInit
  animateSR       <- newIORef AN.stateInit
  renderS_        <- initState
  renderSR        <- newIORef renderS_

  let callbacks =  [ 
      Display      (animateBegin animateSR)
    , Display      (displayFun animateSR viewSR renderSR backColor frameOp)
    , Display      (animateEnd   animateSR)
    , Idle         (\s => postRedisplay s)
    , callback_exit backend 
    , callback_viewState_motion viewSR
    , callback_viewState_reshape ]

        ++ (if pannable 
            then [callback_viewState_keyMouse viewSR]
            else [])

  createWindow backend display backColor callbacks 
      $ \ backendRef
      =>  eatController
          $ MkController
            (postRedisplay backendRef) -- controllerSetRedraw
            (\modViewPort => do
              viewState       <- readIORef viewSR
              port'           <- modViewPort $ viewStateViewPort viewState
              let viewState'  =  record { viewStateViewPort = port' } viewState
              writeIORef viewSR viewState'
              postRedisplay backendRef) -- controllerModifyViewPort