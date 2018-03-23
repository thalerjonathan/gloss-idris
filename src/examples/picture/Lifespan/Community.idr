module Community

import Graphics.Gloss

import Cell
import Random

%access public export

Community : Type
Community = List Cell

-- does a (newly spawned) cell fit in the community?
-- that is, does it overlap with any others?
fits : Cell -> Community -> Bool
fits cell cells = not $ any (overlap cell) cells

-- For each member of a community, produce one offspring
-- The lists of Doubles are the (random) parameters that determine size

-- and location of each offspring.
spawn : Community -> RandomDoubleStream -> RandomDoubleStream -> Stream Int -> List Cell
spawn = zipWith4 offspring
  where
    zipWith4 : (a -> b -> c -> d -> e) -> List a -> Stream b -> Stream c -> Stream d -> List e
    zipWith4 f [] _ _ _ = []
    zipWith4 f (b::bs) (c::cs) (d::ds) (e::es)
        = f b c d e :: zipWith4 f bs cs ds es

-- Given a collection of cells (one spawned by each member of the
-- community) check if it fits, and if so add it to the community.
-- That check must include new cells that have been added to the
-- community in this process.
survive : List Cell -> Community -> Community
survive [] comm = comm
survive (cell::cells) comm =
  if fits cell comm
    then survive cells (cell::comm)
    else survive cells comm

age : Community -> Community
age [] = []
age (MkCell c r 0 :: cells) = age cells
age (MkCell c r life :: cells) = MkCell c r (life-1) :: age cells

-- The next generation of a community
generation : Community -> RandomDoubleStream -> RandomDoubleStream -> Community
generation comm angles scales
  = survive (spawn comm angles scales (repeat 5)) (age comm)

render : Community -> Picture
render comm 
  = Pictures 
  $ map Cell.render comm

initial : Community
initial = [MkCell (0,0) 50 5]