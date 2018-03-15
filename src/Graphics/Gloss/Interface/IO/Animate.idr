||| Animate a picture in a window.
module Graphics.Gloss.Interface.IO.Animate
{-
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , animateIO
        , animateFixedIO
        , Controller (..))
-}

import public Graphics.Gloss.Data.Display
import        Graphics.Gloss.Data.Controller
import public Graphics.Gloss.Data.Picture
import public Graphics.Gloss.Data.Color
import        Graphics.Gloss.Internals.Interface.Animate
import        Graphics.Gloss.Internals.Interface.Backend

||| Open a new window and display the given animation.
|||
|||   Once the window is open you can use the same commands as with @display@.
|||
export
animateIO : Display                -- ^ Display mode.
          -> Color                  -- ^ Background color.
          -> (Double -> IO Picture)  -- ^ Function to produce the next frame of animation. 
                                    --      It is passed the time in seconds since the program started.
          -> (Controller -> IO ())  -- ^ Callback to take the display controller.
          -> IO ()
animateIO display backColor
        frameFunIO eatControllerIO
        = animateWithBackendIO 
                defaultBackendState 
                True              -- pannable
                display backColor
                frameFunIO
                eatControllerIO

||| Like `animateIO` but don't allow the display to be panned around.
|||
export
animateFixedIO : Display                -- ^ Display mode.
               -> Color                  -- ^ Background color.
               -> (Double -> IO Picture)  -- ^ Function to produce the next frame of animation. 
                                        --      It is passed the time in seconds since the program started.
               -> (Controller -> IO ())  -- ^ Callback to take the display controller.
               -> IO ()
animateFixedIO display backColor
        frameFunIO eatControllerIO
        = animateWithBackendIO 
                defaultBackendState 
                False
                display backColor
                frameFunIO
                eatControllerIO
