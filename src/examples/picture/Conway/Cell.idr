module Cell

import Graphics.Gloss
   
||| A cell in the world.
public export
data Cell =
  ||| A living cell with its age
  CellAlive Int |
  ||| A dead / blank cell.
  CellDead


-- Color Ramps  -----------------------------------------------------------------------------------
||| Standard Hot -> Cold hypsometric color ramp.
--      Sequence is red, yellow, green, cyan, blue.
rampColorHotToCold 
        :  Double
        -> Double
        -> Double 
        -> (Double, Double, Double)
rampColorHotToCold vmin vmax vNotNorm = 
    if v < vmin + 0.25 * dv
      then ( 0, 4 * (v - vmin) / dv, 1.0)
      else if v < vmin + 0.5 * dv
        then ( 0, 1.0, 1 + 4 * (vmin + 0.25 * dv - v) / dv)
        else if v < vmin + 0.75 * dv
          then ( 4 * (v - vmin - 0.5 * dv) / dv, 1.0, 0.0)
          else ( 1.0, 1 + 4 * (vmin + 0.75 * dv - v) / dv, 0)
  where
    v : Double
    v = if vNotNorm < vmin 
          then vmin
          else if vNotNorm > vmax
            then vmax
            else vNotNorm
    
    dv : Double
    dv = vmax - vmin   

    result : (Double, Double, Double)
    result = (0, 0, 0)

ageColor : Int -> Int -> Color
ageColor oldAge age
 = let (r, g, b) = rampColorHotToCold 0 (cast oldAge) (cast age)
   in  makeColor r g b 1.0

||| Sort the living from the dead.
export
isAlive : Cell -> Bool
isAlive (CellAlive _) = True
isAlive CellDead = False

||| The basic shape of a cell.
cellShape : Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi
  = let cs      = cast cellSize
        posX    = cast posXi
        posY    = cast posYi
        x1      = posX
        x2      = posX + cs
        y1      = posY 
        y2      = posY + cs
    in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
                

||| Convert a cell to a picture, based on a primitive shape.
--      We pass the shape in to avoid recomputing it for each cell.
export
pictureOfCell : Int -> Int -> Int -> Int -> Cell -> Picture
pictureOfCell oldAge cellSize posX posY (CellAlive age) = Color (ageColor oldAge age)  (cellShape cellSize posX posY)
pictureOfCell oldAge cellSize posX posY CellDead = Color (greyN 0.8)            (cellShape cellSize posX posY)