||| This game mode lets you manage your own input. Pressing ESC will not abort the program.
|||   You also don't get automatic pan and zoom controls like with `display`.
module Graphics.Gloss.Interface.IO.Game

import public Graphics.Gloss.Data.Display
import public Graphics.Gloss.Data.Picture
import public Graphics.Gloss.Data.Color
import        Graphics.Gloss.Internals.Interface.Backend
import        Graphics.Gloss.Internals.Interface.Event
import        Graphics.Gloss.Internals.Interface.Game

||| Play a game in a window, using IO actions to build the pictures. 
export
playIO  :  Display                      -- ^ Display mode.
        -> Color                        -- ^ Background color.
        -> Int                          -- ^ Number of simulation steps to take for each second of real time.
        -> world                        -- ^ The initial world.
        -> (world -> IO Picture)        -- ^ An action to convert the world a picture.
        -> (Event -> world -> IO world) -- ^ A function to handle input events.
        -> (Double -> world -> IO world) -- ^ A function to step the world one iteration.
                                        --   It is passed the period of time (in seconds) needing to be advanced.
        -> IO ()
playIO  display backColor simResolution
        worldStart worldToPicture worldHandleEvent worldAdvance
 = playWithBackendIO defaultBackendState
        display backColor simResolution
        worldStart worldToPicture worldHandleEvent worldAdvance
        False