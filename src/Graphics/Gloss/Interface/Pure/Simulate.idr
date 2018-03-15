-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

||| Simulate mode is for producing an animation of some model who's picture
|||   changes over finite time steps. The behavior of the model can also depent
|||   on the current `ViewPort`.
module Graphics.Gloss.Interface.Pure.Simulate

import public Graphics.Gloss.Data.Display
import public Graphics.Gloss.Data.Picture
import public Graphics.Gloss.Data.Color
import public Graphics.Gloss.Data.ViewPort
import        Graphics.Gloss.Internals.Interface.Simulate
import        Graphics.Gloss.Internals.Interface.Backend

||| Run a finite-time-step simulation in a window. You decide how the model is represented,
|||      how to convert the model to a picture, and how to advance the model for each unit of time. 
|||      This function does the rest.
|||
|||   Once the window is open you can use the same commands as with `display`.
|||
export
simulate : Display      -- ^ Display mode.
        -> Color        -- ^ Background color.
        -> Int          -- ^ Number of simulation steps to take for each second of real time.
        -> model        -- ^ The initial model.
        -> (model -> Picture)           
                -- ^ A function to convert the model to a picture.
        -> (ViewPort -> Double -> model -> model) 
                -- ^ A function to step the model one iteration. It is passed the 
                --     current viewport and the amount of time for this simulation
                --     step (in seconds).
        -> IO ()
simulate display backColor simResolution modelStart modelToPicture modelStep = 
  simulateWithBackendIO 
        defaultBackendState
        display 
        backColor 
        simResolution
        modelStart
        (\model => pure $ modelToPicture model)
        (\view, time, model => pure $ modelStep view time model)