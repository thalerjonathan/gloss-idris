module Cell

import Graphics.Gloss

%access public export

data Cell
  = MkCell 
      Point    -- centre
      Double    -- radius
      Int      -- remaining lifetime

-- Produce a new cell of a certain relative radius at a certain angle.
-- The factor argument is in the range [0..1] so spawned cells are
-- smaller than their parent.
-- The check whether it fits in the community is elsewhere.
offspring : Cell -> Double -> Double -> Int -> Cell
offspring (MkCell (x,y) r _) alpha factor lifespan =
    MkCell (x + (childR+r) * cos alpha, y + (childR+r) * sin alpha)
         childR 
         lifespan
  where
    childR : Double
    childR = factor * r

-- Do two cells overlap?         
-- Used to decide if newly spawned cells can join the community.
overlap : Cell -> Cell -> Bool
overlap (MkCell (x1,y1) r1 _) (MkCell (x2,y2) r2 _) = 
  let xdiff = x1 - x2
      ydiff = y1 - y2
      centreDist = sqrt(xdiff*xdiff + ydiff*ydiff)
  in  centreDist < (r1 + r2) *0.999

-- thickness of circle is determined by lifespan
render : Cell -> Picture
render (MkCell (x,y) r life) 
    = Color (makeColor 0.6 (cast life * 0.12) 0.6 1.0)
    $ Translate x y
    $ ThickCircle (r - (cast life) / 2) (cast life)