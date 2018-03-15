||| Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.Pure.Animate

import public Graphics.Gloss.Data.Display
import public Graphics.Gloss.Data.Picture
import public Graphics.Gloss.Data.Color
import        Graphics.Gloss.Data.Controller
import        Graphics.Gloss.Internals.Interface.Animate
import        Graphics.Gloss.Internals.Interface.Backend


||| Open a new window and display the given animation.
|||
|||   Once the window is open you can use the same commands as with `display`.
|||
export
animate :  Display              -- ^ Display mode.
        -> Color                -- ^ Background color.
        -> (Double -> Picture)   -- ^ Function to produce the next frame of animation. 
                                --      It is passed the time in seconds since the program started.
        -> IO ()
animate display backColor frameFun
        = animateWithBackendIO 
                defaultBackendState
                True            -- pannable
                display backColor
                (return . frameFun) 
                (const (return ()))
