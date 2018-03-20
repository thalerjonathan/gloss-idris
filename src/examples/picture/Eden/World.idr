module World

import Effects
import Effect.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import Cell
import Community
import Random

maxSteps : Int
maxSteps = 30

-- The World consists of a Community and a random number generator.
-- (The RNG is a model of chaos or hand-of-god.)
export
data EdenWorld = MkEdenWorld Community RandomIntStream Int

-- Seeding the prng means every run is identical.
-- To get different runs, need to use gen <- getStdGen in main :: IO()
-- and pass gen in as an argument.  Edit Main.hs accordingly.
export
genesis : Int -> EdenWorld
genesis seed = MkEdenWorld [MkCell (0,0) 30 0] (randoms seed) 0

-- Consume some random numbers to advance the simulation
export
evolve : ViewPort -> Double -> EdenWorld -> EdenWorld
evolve vp step world@(MkEdenWorld comm ss steps) =
  if steps < maxSteps    
    then  let   --(genThis, genNext) = split gen
                --(genA, genS)       = split genThis
                (rsThis, rsNext)   = splitRandom ss
                (rsA, rsS)         = splitRandom rsThis
                angles             = transformStream (0.0, 2*pi) rsA
                scales             = transformStream (0.7, 0.9) rsS
          in    MkEdenWorld (generation comm angles scales) rsNext (steps + 1)
    else world

-- Converting the world to a picture is just converting the community component
export
render : EdenWorld -> Picture
render (MkEdenWorld comm gen steps) 
  = Color (makeColor 0.3 0.3 0.6 1.0)
  $ Community.render comm