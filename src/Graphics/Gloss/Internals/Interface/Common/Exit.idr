||| Callback for exiting the program.
module Graphics.Gloss.Internals.Interface.Common.Exit

import Data.IORef

import Graphics.Gloss.Internals.Interface.Backend

keyMouse_exit :  Backend GLFWState 
              => GLFWState 
              -> IORef GLFWState 
              -> Key 
              -> KeyState 
              -> Modifiers 
              -> (Int,Int) 
              -> IO () -- KeyboardMouseCallback
keyMouse_exit _ backend (SpecialKey KeyEsc) Down _ _ = exitBackend backend
keyMouse_exit _ _ _ _ _ _ = pure ()

export
callback_exit : GLFWState -> Callback
callback_exit stateRef = KeyMouse (keyMouse_exit stateRef)