-- Adapted from ANUPlot version by Clem Baker-Finch
module Main
import World as W
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import Config

-- varying prng sequence
main : IO () 
main = do
  let seed = 1023
  simulate 
    (InWindow "Lifespan" (800, 600) (10, 10))
    (greyN 0.1)       -- background color
    2                 -- number of steps per second
    (W.genesis seed)  -- initial world
    W.render          -- function to convert world to a Picture
    W.evolve          -- function to step the world one iteration