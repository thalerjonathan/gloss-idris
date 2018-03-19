module Main

import Effects
import Effect.Random

import Graphics.Gloss

import Cell
import Config
import World

||| Convert a cell at a particular coordinate to a picture.
drawCell : ConwayWorld -> (Index, Cell) -> Picture
drawCell world (index, cell)
 = let  cs      = worldCellSize world
        cp      = worldCellSpace world

        (x, y)  = coordOfIndex world index
        fx      = x * (cs + cp) + 1
        fy      = y * (cs + cp) + 1

   in   pictureOfCell
                (worldCellOldAge world)
                (worldCellSize   world)
                fx
                fy
                cell

||| Get the size of the window needed to display a world.
windowSizeOfWorld : ConwayWorld -> (Int, Int)
windowSizeOfWorld world
 = let  cellSize        = worldCellSize world
        cellSpace       = worldCellSpace world
        cellPad         = cellSize + cellSpace
        height          = cellPad * (worldHeight world) + cellSpace
        width           = cellPad * (worldWidth  world) + cellSpace
   in   (width, height)

||| Convert a world to a picture.
drawWorld : ConwayWorld -> Picture
drawWorld world 
 = let  (windowWidth, windowHeight)     
                = windowSizeOfWorld world
                
        offsetX = - cast windowWidth  / 2
        offsetY = - cast windowHeight / 2 
   in   Translate offsetX offsetY
                $ Pictures 
                $ map (drawCell world) cs
  where
    n : Int
    n = toIntNat $ Prelude.List.length $ worldCells world

    cs : List (Int, Cell)
    cs = zip [0 .. n] (worldCells world)

main : IO ()
main = do   
  let width       = 150
  let height      = 100
  world <- run $ randomWorld width height
  
  simulate (InWindow "John Conway's Game of Life" 
                      (windowSizeOfWorld world) (5, 5))
          white 10 world drawWorld simulateWorld