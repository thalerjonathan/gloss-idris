||| Geometric functions concerning lines and segments.
|||
|||   A @Line@ is taken to be infinite in length, while a @Seg@ is finite length
|||  line segment represented by its two endpoints. 
module Graphics.Gloss.Geometry.Line

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

||| Check if line segment (P1-P2) clears a box (P3-P4) by being well outside it.
export
segClearsBox 
         : Point        -- ^ P1 First point of segment. 
        -> Point        -- ^ P2 Second point of segment.
        -> Point        -- ^ P3 Lower left point of box.
        -> Point        -- ^ P4 Upper right point of box.
        -> Bool
segClearsBox (x1, y1) (x2, y2) (xa, ya) (xb, yb) =
  if x1 < xa && x2 < xa
    then True
    else if x1 > xb && x2 > xb
      then True
      else if y1 < ya && y2 < ya
        then True
        else if y1 > yb && y2 > yb
          then True
          else False

||| Given an infinite line which intersects P1 and P2,
--      let P4 be the point on the line that is closest to P3.
--
--      Return an indication of where on the line P4 is relative to P1 and P2.
--
-- @
--      if P4 == P1 then 0
--      if P4 == P2 then 1
--      if P4 is halfway between P1 and P2 then 0.5
-- @
--
-- @
--        |
--       P1
--        | 
--     P4 +---- P3      
--        |
--       P2
--        |
-- @
--
export
closestPointOnLineParam
        : Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Double
closestPointOnLineParam p1 p2 p3
        = (dotV (p3 - p1) (p2 - p1))
        / (dotV (p2 - p1) (p2 - p1))

||| Given an infinite line which intersects `P1` and `P1`,
--      return the point on that line that is closest to `P3`
export
closestPointOnLine
        : Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Point        -- ^ the point on the line P1-P2 that is closest to `P3`
closestPointOnLine p1 p2 p3
        = p1 + (u `mulSV` (p2 - p1))
        where   u       = closestPointOnLineParam p1 p2 p3

-- Line-Line intersection -----------------------------------------------------

||| Given four points specifying two lines, get the point where the two lines
|||   cross, if any. Note that the lines extend off to infinity, so the
|||   intersection point might not line between either of the two pairs of points.
|||
||| 
|||     \\      /
|||      P1  P4
|||       \\ /
|||        +
|||       / \\
|||     P3  P2
|||     /     \\
||| 
export
intersectLineLine 
        : Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Point        -- ^ `P4`
        -> Maybe Point
intersectLineLine (x1, y1) (x2, y2) (x3, y3) (x4, y4)
 = let  dx12    = x1 - x2
        dx34    = x3 - x4

        dy12    = y1 - y2
        dy34    = y3 - y4
        
        den     = dx12 * dy34  - dy12 * dx34

   in if den == 0
        then Nothing
        else let
                det12   = x1*y2 - y1*x2
                det34   = x3*y4 - y3*x4 

                numx    = det12 * dx34 - dx12 * det34
                numy    = det12 * dy34 - dy12 * det34
             in Just (numx / den, numy / den)


-- Segment-Line intersection --------------------------------------------------
||| Get the point where a segment @P1-P2@ crosses an infinite line @P3-P4@,
|||   if any.
export
intersectSegLine
         : Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Point        -- ^ `P4`
        -> Maybe Point
intersectSegLine p1 p2 p3 p4 = do
  -- TODO: merge closest point checks with intersection, reuse subterms.
  p0  <- intersectLineLine p1 p2 p3 p4
  let t12 = closestPointOnLineParam p1 p2 p0

  if t12 >= 0 && t12 <= 1
    then Just p0
    else Nothing


||| Get the point where a segment crosses a horizontal line, if any.
|||
||| 
|||                + P1
|||               /
|||       -------+---------
|||             /        y0
|||         P2 +
|||
export
intersectSegHorzLine 
        : Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Double        -- ^ y value of line.
        -> Maybe Point
intersectSegHorzLine (x1, y1) (x2, y2) y0 =
  -- seg is on line
  if y1 == y0 && y2 == y0
    then Nothing
    -- seg is above line
    else if y1 > y0 && y2 > y0
      then Nothing
      -- seg is below line
      else if y1 < y0 &&  y2 < y0 
        then Nothing
        -- seg is a single point on the line.
        -- this should be caught by the first case, 
        -- but we'll test for it anyway.
        else if y2 - y1 == 0  
          then Just (x1, y1)
          else Just ((y0 - y1) * (x2 - x1) / (y2 - y1) + x1, y0)

||| Get the point where a segment crosses a vertical line, if any.
|||
|||
|||              |
|||              |   + P1
|||              | /
|||              +
|||            / |
|||       P2 +   |
|||              | x0
|||
export
intersectSegVertLine 
        : Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Double        -- ^ x value of line.
        -> Maybe Point
intersectSegVertLine (x1, y1) (x2, y2) x0 = 
  -- seg is on line
  if x1 == x0 && x2 == x0
    then Nothing
    -- seg is to right of line
    else if x1 > x0 && x2 > x0 
      then Nothing
      -- seg is to left of line
      else if x1 < x0 &&  x2 < x0
        then Nothing
        -- seg is a single point on the line.
        -- this should be caught by the first case, 
        -- but we'll test for it anyway.
        else if x2 - x1 == 0          
          then Just (x1, y1)
          else Just (x0, (x0 - x1) * (y2 - y1) / (x2 - x1) + y1)


-- Segment-Segment intersection -----------------------------------------------

||| Get the point where a segment @P1-P2@ crosses another segement @P3-P4@,
|||   if any.
export
intersectSegSeg
        : Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Point        -- ^ `P4`
        -> Maybe Point
intersectSegSeg p1 p2 p3 p4 = do
  -- TODO: merge closest point checks with intersection, reuse subterms.
  p0  <- intersectLineLine p1 p2 p3 p4
  let t12 = closestPointOnLineParam p1 p2 p0
  let t23 = closestPointOnLineParam p3 p4 p0

  if t12 >= 0 && t12 <= 1 && t23 >= 0 && t23 <= 1
    then Just p0
    else Nothing

||| Check if an arbitrary segment intersects a horizontal segment.
|||
||| 
|||                 + P2
|||                /
||| (xa, y3)  +---+----+ (xb, y3)
|||              /
|||          P1 +
export
intersectSegHorzSeg
         : Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Double        -- ^ (y3) y value of horizontal segment.
        -> Double        -- ^ (xa) Leftmost x value of horizontal segment.
        -> Double        -- ^ (xb) Rightmost x value of horizontal segment.
        -> Maybe Point  -- ^ (x3, y3) Intersection point, if any.
intersectSegHorzSeg p1@(x1, y1) p2@(x2, y2) y0 xa xb =
    if segClearsBox p1 p2 (xa, y0) (xb, y0)
      then Nothing
      else if x0 < xa
        then Nothing
        else if x0 > xb
          then Nothing
          else Just (x0, y0)
                  
  where 
    x0 = if (y2 - y1) == 0 
          then x1
          else (y0 - y1) * (x2 - x1) / (y2 - y1) + x1

||| Check if an arbitrary segment intersects a vertical segment.
|||
||| 
|||      (x3, yb) +
|||               |   + P1
|||               | /
|||               +
|||             / |
|||        P2 +   |
|||               + (x3, ya)
export
intersectSegVertSeg
        : Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Double        -- ^ (x3) x value of vertical segment
        -> Double        -- ^ (ya) Lowest y value of vertical segment.
        -> Double        -- ^ (yb) Highest y value of vertical segment.
        -> Maybe Point  -- ^ (x3, y3) Intersection point, if any.
intersectSegVertSeg p1@(x1, y1) p2@(x2, y2) x0 ya yb =
    if segClearsBox p1 p2 (x0, ya) (x0, yb)
      then Nothing
      else if y0 < ya 
        then Nothing
        else if y0 > yb
          then Nothing
          else Just (x0, y0)
          
  where 
    y0 = if (x2 - x1) == 0 
          then y1
          else (x0 - x1) * (y2 - y1) / (x2 - x1) + y1