module Community

import Graphics.Gloss

import Cell
import Random

public export
Community : Type
Community = List Cell

-- does a (newly spawned) cell fit in the community?
-- that is, does it overlap with any others?
fits : Cell -> Community -> Bool
fits cell cells = not $ any (overlap cell) cells

-- For each member of a community, produce one offspring
-- The lists of Floats are the (random) parameters that determine size
-- and location of each offspring.
spawn : Community -> RandomDoubleStream -> RandomDoubleStream -> List Cell
spawn [] _ _ = [] 
spawn (c :: com) (alpha :: as) (factor :: fs) = offspring c alpha factor :: spawn com as fs

-- Given a collection of cells (one spawned by each member of the
-- community) check if it fits, and if so add it to the community.
-- That check must include new cells that have been added to the
-- community in this process.
survive : List Cell -> Community -> Community
survive [] comm = comm
survive (cell :: cells) comm =
  if fits cell comm
    then survive cells (cell :: comm)
    else survive cells comm

-- The next generation of a community
export
generation :  Community 
           -> RandomDoubleStream 
           -> RandomDoubleStream
           -> Community
generation comm angles scales 
  = survive (spawn comm angles scales) comm

export
render : Community -> Picture
render comm = Pictures $ map Cell.render comm

initial : Community
initial = [MkCell (0,0) 50 0]

-- thread the random lists for testing outside IO()
--
{-
life : Community -> List Double -> List Double -> (Community, List Double, List Double)
life comm randomAngles randomScales
  = let population = length comm
        (angles, randomAngles') = splitAt population randomAngles
        (scales, randomScales') = splitAt population randomScales
    in  (generation comm angles scales, randomAngles', randomScales')
  
evolution : Community -> List Double -> List Double -> List Community
evolution comm randomAngles randomScales 
  = let (comm1, ras, rss) = life comm randomAngles randomScales
        comms = evolution comm1 ras rss
    in  comm1 :: comms
    -}