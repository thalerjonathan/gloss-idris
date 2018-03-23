module Graphics.Gloss.Internals.Interface.ViewState.KeyMouse

import Data.IORef

import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Event

viewState_keyMouse :  Backend GLFWState 
                   => IORef ViewState 
                   -> IORef GLFWState 
                   -> Key 
                   -> KeyState 
                   -> Modifiers 
                   -> (Int,Int) 
                   -> IO () -- KeyboardMouseCallback
viewState_keyMouse viewStateRef stateRef key keyState keyMods pos = do
  viewState <- readIORef viewStateRef
  ev        <- keyMouseEvent stateRef key keyState keyMods pos
  case updateViewStateWithEventMaybe ev viewState of
      Nothing => pure ()
      Just viewState' => do
        viewStateRef `writeIORef` viewState'
        postRedisplay stateRef

||| Callback to handle keyboard and mouse button events
|||      for controlling the 'ViewState'.
export
callback_viewState_keyMouse : IORef ViewState -> Callback
callback_viewState_keyMouse viewStateRef
  = KeyMouse (viewState_keyMouse viewStateRef)