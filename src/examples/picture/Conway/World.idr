module World

import Effects
import Effect.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import Cell

%access public export

-- Index ----------------------------------------------------------------------
||| An index into the vector holding all the cells.
Index : Type
Index = Int

||| The x y coordinate of a cell.
Coord : Type
Coord = (Int, Int)

-- World ----------------------------------------------------------------------
record ConwayWorld where 
  constructor MkConwayWorld
  worldCells            : List Cell -- quite slow, lookup is O(n)
  worldWidth            : Int 
  worldHeight           : Int 
  ||| Width and height of each cell.
  worldCellSize         : Int
  ||| Number of pixels to leave between each cell.
  worldCellSpace        : Int
  ||| Cells less than this age are drawn with the color ramp
  worldCellOldAge       : Int
  ||| Seconds to wait between each simulation step.
  worldSimulationPeriod : Double 
  ||| Time that has elapsed since we drew the last step
  worldElapsedTime      : Double

indexOfCoord : ConwayWorld -> Coord -> Index
indexOfCoord world (x, y) 
  = x + y * (worldWidth world)

coordOfIndex : ConwayWorld -> Index -> Coord
coordOfIndex world i            
  = ( i `mod` worldWidth world
    , i `div` worldWidth world)

||| Convert a bool to a live or dead cell.
cellOfBool : Bool -> Cell
cellOfBool True = CellAlive 0
cellOfBool False = CellDead

||| Make a new world of a particular size.
randomWorld : Int -> Int -> Eff ConwayWorld [RND]
randomWorld width height = do
    bs <- randomWorldAux (width * height) []
    pure $ MkConwayWorld
            (map cellOfBool bs) -- worldCells
            width               -- worldWidth
            height              -- worldHeight
            5                   -- worldCellSize
            1                   -- worldCellSpace
            20                  -- worldCellOldAge
            0.1                 -- worldSimulationPeriod
            0                   -- worldElapsedTime
  where
    rndBool : Eff Bool [RND]
    rndBool = do
      r <- rndInt 0 2
      if 0 == r
        then pure False
        else pure True

    randomWorldAux : Int -> List Bool -> Eff (List Bool) [RND]
    randomWorldAux 0 acc = pure acc
    randomWorldAux n acc = do
      r <- rndBool
      randomWorldAux (n - 1) (r :: acc)

||| Get the cell at a particular coordinate in the world.
getCell : ConwayWorld -> Coord -> Cell
getCell world coord@(x, y) =
    if x < 0 || (x >= (worldWidth world)) || y < 0 || (y >= (worldHeight world))
      then CellDead
      else fromMaybe CellDead mayCell
  where
    iInt : Int
    iInt = indexOfCoord world coord

    iInteger : Integer
    iInteger = cast iInt

    iNat : Nat
    iNat = fromIntegerNat iInteger

    mayCell : Maybe Cell
    mayCell = Prelude.List.index' iNat (worldCells world)

||| Get the neighbourhood of cells aroudn this coordinate.
getNeighbourhood : ConwayWorld -> Coord -> List Cell
getNeighbourhood world (ix, iy) = map (getCell world) indexes
  where
    indexes : List Coord
    indexes = [ (x, y) | x <- [(ix - 1) .. (ix + 1)]
                       , y <- [(iy - 1) .. (iy + 1)]
                       , not (x == ix && y == iy) ]

||| Compute the next cell state depending on its neighbours.
stepCell : Cell -> List Cell -> Cell
stepCell cell neighbours
 = let  live    = length (filter isAlive neighbours)
   in   case cell of
         CellAlive age  => if elem live [2, 3] then CellAlive (age + 1) else CellDead
         CellDead       => if live == 3        then CellAlive 0         else CellDead

||| Compute the next state of the cell at this index in the world.
stepIndex : ConwayWorld -> (Int, Cell) -> Cell
stepIndex world (index, cell)
 = let  coord   = coordOfIndex world index
        neigh   = getNeighbourhood world coord
   in   stepCell cell neigh

||| Compute the next world state.
stepWorld : ConwayWorld -> ConwayWorld
stepWorld world
    = record { worldCells = map (stepIndex world) cs } world
  where
    n : Int
    n = toIntNat $ Prelude.List.length $ worldCells world

    cs : List (Int, Cell)
    cs = zip [0 .. n] (worldCells world)

||| Simulation function for worlds.
simulateWorld : ViewPort -> Double -> ConwayWorld -> ConwayWorld
simulateWorld _ time world =
  -- If enough time has passed then it's time to step the world.
  if worldElapsedTime world >= (worldSimulationPeriod world)
    then let world'    = stepWorld world
         in  record { worldElapsedTime = 0 } world'
    -- Wait some more.
    else record { worldElapsedTime = worldElapsedTime world + time } world