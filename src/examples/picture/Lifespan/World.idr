module World

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import Community
import Cell
import Random

%access public export

stepsMax : Int
stepsMax = 20

-- The World consists of a Community and a random number generator.
-- (The RNG is a model of chaos or hand-of-god.)
data World = MkWorld Community RandomIntStream Int

-- Seeding the prng means every run is identical.
-- To get different runs, need to use gen <- getStdGen in main :: IO()
-- and pass gen in as an argument.  Edit Main.hs accordingly.
genesis : Int -> World.World
genesis seed = MkWorld [MkCell (0,0) 50 5] (randoms seed) 0

-- Consume some random numbers to advance the simulation
evolve : ViewPort -> Double -> World.World -> World.World
evolve _ _ world@(MkWorld comm ss step) =
    if step > stepsMax
      then world
      else let (rsThis, rsNext)   = splitRandom ss
               (rsA, rsS)         = splitRandom rsThis
               angles             = transformStream (0.0, 2*pi) rsA
               scales             = transformStream (0.7, 0.9) rsS
           in  MkWorld (generation comm angles scales) rsNext (step + 1)

-- Converting the world to a picture is just converting the community component
render : World.World -> Picture
render (MkWorld comm ss _) 
    = Color (makeColor 0.3 0.3 0.6 1.0)
    $ Community.render comm