-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

||| This game mode lets you manage your own input. Pressing ESC will still abort the program,
|||   but you don't get automatic pan and zoom controls like with `displayInWindow`.
module Graphics.Gloss.Interface.Pure.Game

import public Graphics.Gloss.Data.Display
import public Graphics.Gloss.Data.Picture
import public Graphics.Gloss.Data.Color
import public Graphics.Gloss.Internals.Interface.Backend
import        Graphics.Gloss.Internals.Interface.Game
import public Graphics.Gloss.Internals.Interface.Event

|||| Play a game in a window. Like `simulate`, but you manage your own input events.
export
play     : Display              -- ^ Display mode.
        -> Color                -- ^ Background color.
        -> Int                  -- ^ Number of simulation steps to take for each second of real time.
        -> world                -- ^ The initial world.
        -> (world -> Picture)   -- ^ A function to convert the world a picture.
        -> (Event -> world -> world)    
                -- ^ A function to handle input events.
        -> (Double -> world -> world)
                -- ^ A function to step the world one iteration.
                --   It is passed the period of time (in seconds) needing to be advanced.
        -> IO ()
play    display backColor simResolution
        worldStart worldToPicture worldHandleEvent worldAdvance =
  playWithBackendIO defaultBackendState 
                        display backColor simResolution
                        worldStart 
                        (pure . worldToPicture)
                        (\event, world => pure $ worldHandleEvent event world)
                        (\time,  world => pure $ worldAdvance     time  world)
                        True
