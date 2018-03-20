module Cell

import Graphics.Gloss

%access public export

data Cell 
  = MkCell Point     -- centre
           Double    -- radius
           Int

-- Produce a new cell of a certain relative radius at a certain angle.
--      The factor argument is in the range [0..1] so spawned cells are
--      smaller than their parent.
-- The check whether it fits in the community is elsewhere.
offspring : Cell -> Double -> Double -> Cell
offspring (MkCell (x,y) r gen) alpha factor 
    = MkCell  (x + (childR+r) * cos alpha, y + (childR+r) * sin alpha) 
              childR 
              (gen + 1)
  where
    childR : Double
    childR = factor * r

-- Do two cells overlap?         
-- Used to decide if newly spawned cells can join the community.
overlap : Cell -> Cell -> Bool
overlap (MkCell (x1,y1) r1 _) (MkCell (x2,y2) r2 _) 
    = centreDist < (r1 + r2) * 0.999
  where
    xdiff : Double
    xdiff = x1 - x2

    ydiff : Double
    ydiff = y1 - y2

    centreDist : Double
    centreDist = sqrt(xdiff*xdiff + ydiff*ydiff)

render : Cell -> Picture
render (MkCell (x,y) r gen) 
  = let  z       = cast gen * 0.1
         color   = makeColor 0.0 z 0.5 1.0
    in   Color color
                $ Translate x y
                $ Circle r
