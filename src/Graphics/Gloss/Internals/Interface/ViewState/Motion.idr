module Graphics.Gloss.Internals.Interface.ViewState.Motion

import Data.IORef

import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Backend.GLFW
import Graphics.Gloss.Internals.Interface.Backend.Types
import Graphics.Gloss.Internals.Interface.Event

-- TODO: do we really parameterise Backend with GLFWState? in haskell implementation it is left with type parameter a, and in IORef the same
viewState_motion : Backend GLFWState => IORef ViewState -> IORef GLFWState -> (Int,Int) -> IO () -- MotionCallback
viewState_motion viewStateRef stateRef pos = do 
  viewState <- readIORef viewStateRef
  ev        <- motionEvent stateRef pos
  case updateViewStateWithEventMaybe ev viewState of
        Nothing => pure ()
        Just viewState' => do
          viewStateRef `writeIORef` viewState'
          postRedisplay stateRef

||| Callback to handle keyboard and mouse button events
|||      for controlling the viewport.
export
callback_viewState_motion 
        : IORef ViewState
        -> Callback
callback_viewState_motion portRef
  = Motion (viewState_motion portRef)