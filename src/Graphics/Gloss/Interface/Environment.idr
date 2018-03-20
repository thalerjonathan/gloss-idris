module Graphics.Gloss.Interface.Environment

import Data.IORef

import Graphics.UI.GLFW as GLFW

||| Get the size of the screen, in pixels.
|||
|||   This will be the size of the rendered gloss image when 
|||   fullscreen mode is enabled.
|||
export
getScreenSize : IO (Int, Int)
getScreenSize = do
  True <- GLFW.initialize | False => do
    putStrLn "Initializing GLFW failed"
    pure (0, 0)

  mon <- GLFW.getPrimaryMonitor
  vmode <- GLFW.getVideoMode mon

  GLFW.shutdown

  pure (width vmode, height vmode)
