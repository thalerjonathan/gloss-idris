-- Implementation of the Boids flocking algorithm. 
--   by Matthew Sottile <matt@galois.com> <mjsottile@computer.org>
--   Described in http://syntacticsalt.com/2011/03/10/functional-flocks/
--
-- Read more about Boids here: http://www.red3d.com/cwr/boids/
-- 

import Debug.Error
import Effects
import Effect.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

import KDTree2d
import Vec2

-- TODO: can we somehow get rid of this?
%flag C "-I/usr/include/libdrm -I/usr/include/libpng16 "
%flag C "-lGLEW -lGLU -lGL -lpng16 -lz "

-- Parameters -----------------------------------------------------------------
cParam : Double
cParam  = 0.0075

sParam : Double
sParam  = 0.1
sScale : Double
sScale  = 1.25

maxx : Double
maxx    = 8.0
maxy : Double
maxy    = 8.0
minx : Double
minx    = -8.0
miny : Double
miny    = -8.0
aParam : Double
aParam  = 1.0 / 1.8
vLimit : Double
vLimit  = 0.0025 * max (maxx - minx) (maxy - miny)
epsilon : Double
epsilon = 0.40


-- Colors ---------------------------------------------------------------------
boidColor : Color
boidColor       = makeColor 1.0 1.0 0.0 1.0
radiusColor : Color
radiusColor     = makeColor 0.5 1.0 1.0 0.2
cohesionColor : Color
cohesionColor   = makeColor 1.0 0.0 0.0 1.0
separationColor : Color
separationColor = makeColor 0.0 1.0 0.0 1.0
alignmentColor : Color
alignmentColor  = makeColor 0.0 0.0 1.0 1.0


-- Types ----------------------------------------------------------------------
record World where
  constructor MkWorld
  width         : Double
  height        : Double
  pixWidth      : Int
  pixHeight     : Int

record Boid where
  constructor MkBoid
  identifier    : Int
  position      : Vec2
  velocity      : Vec2
  dbgC          : Vec2
  dbgS          : Vec2
  dbgA          : Vec2


-- Coordinate Conversion ------------------------------------------------------
modelToScreen : Main.World -> (Double, Double) -> (Double, Double)
modelToScreen world (x,y) 
 = let  xscale = cast (pixWidth world)  / width world
        yscale = cast (pixHeight world) / height world
   in   (x * xscale, y * yscale)

scaleFactor : Main.World -> Double
scaleFactor world
 = let  xscale = cast (pixWidth world)  / width world
        yscale = cast (pixHeight world) / height world
   in   max xscale yscale

velocityScale : Double
velocityScale = 10.0 * (max (maxx - minx) (maxy - miny))

-- Rendering -----------------------------------------------------------------
renderboid : Main.World -> Boid -> Picture
renderboid world b 
 = let  (MkVec2 x y)      = position b
        (MkVec2 vx vy)    = velocity b
        v                 = velocity b
        (MkVec2 dCX dCY)  = dbgC b
        (MkVec2 dSX dSY)  = dbgS b
        (MkVec2 dAX dAY)  = dbgA b
        sf              = 5.0 * (scaleFactor world)
        sf'             = 1.0 * (scaleFactor world)
        sf2             = sf * 10
        (xs,ys)         = modelToScreen world (x,y)
        vxs             = sf * vx
        vys             = sf * vy

   in Pictures  
        [ Color boidColor $ 
                Translate xs ys $
                Circle 2

        , Color radiusColor $
                Translate xs ys $
                Circle ((epsilon) * sf')

        , Color boidColor $          
                Line [(xs, ys), (xs + vxs, ys + vys)]

        , Color cohesionColor $
                Line [(xs, ys), (xs + sf2 * dCX, ys + sf2 * dCY) ]

        , Color alignmentColor $
                Line [(xs, ys), (xs + sf2 * dAX, ys + sf2 * dAY) ]

        , Color separationColor $
                Line [(xs, ys), (xs + sf' * dSX, ys +  sf' * dSY)] ]

renderboids : Main.World -> KDTreeNode Boid -> Picture
renderboids world bs = Pictures $ mapKDTree bs (renderboid world)

-- Initialisation -------------------------------------------------------------
rnlist : Int ->  Eff (List Double) [RND]
rnlist 0 = pure []
rnlist n = do
  r <- rndInt 1 1000
  let rd = (cast r / 1000)
  rds <- rnlist (n - 1)
  pure (rd :: rds)

makeboids :  Double 
          -> Double
          -> List Double
          -> List Int 
          -> List Boid
makeboids _ _ [] [] = []
makeboids sv sp (a::b::c::d::e::f::rest) (id::ids) 
  = (MkBoid 
      id                     -- identifier
      (MkVec2 (a*sv) (b*sv)) -- velocity
      (MkVec2 (d*sp) (e*sp)) -- position
      vecZero                 -- dbgC
      vecZero                 -- dbgS
      vecZero)                -- dbgA
    :: makeboids sv sp rest ids
  
initialize :  Int 
           -> Double 
           -> Double 
           -> IO (List Boid)
initialize n sp sv = do
  nums <- run $ rnlist (n*6) 
  let nums' = map (\i => (0.5 - i) / 2.0) nums
  pure $ makeboids sv sp nums' [1..n]

-- Vector Helpers -------------------------------------------------------------
||| Sometimes we want to control runaway of vector scales, so this can
|||   be used to enforce an upper bound
limiter : Vec2 -> Double -> Vec2
limiter x lim = let d = vecNorm x
                in if (d < lim) then x
                       else vecScale (vecNormalize x) lim

|||| Vector with all components length epsilon
epsvec : Vec2
epsvec = MkVec2 epsilon epsilon


-- Boids Logic ----------------------------------------------------------------

-- three rules: 
--      cohesion   (seek centroid)
--      separation (avoid neighbors),
-- and  alignment  (fly same way as neighbors)


||| Centroid is average position of boids, or the vector sum of all
|||  boid positions scaled by 1/(number of boids)
findCentroid : List Boid -> Vec2
findCentroid []    = MkVec2 0 0 -- error "Bad centroid"
findCentroid boids 
 = let  n = length boids
   in   vecScale (foldl1 vecAdd (map position boids))
                 (1.0 / cast n)

||| cohesion : go towards centroid. Parameter dictates fraction of
|||   distance from boid to centroid that contributes to velocity
cohesion : Boid -> List Boid -> Double -> Vec2
cohesion b boids a = vecScale diff a
  where
    c : Vec2
    c    = findCentroid boids
    p : Vec2
    p    = position b
    diff : Vec2
    diff = vecSub c p
        
||| separation: avoid neighbours
separation : Boid -> List Boid -> Double -> Vec2
separation b []    a = vecZero
separation b boids a
 = let  diff_positions  = map (\i => vecSub (position i) (position b)) boids
        closeby         = filter (\i => (vecNorm i) < a) diff_positions
        sep             = foldl vecSub vecZero closeby
   in   vecScale sep sScale


||| alignment: fly the same way as neighbours
alignment : Boid -> List Boid -> Double -> Vec2
alignment b [] a = vecZero
alignment b boids a 
 = let  v       = foldl1 vecAdd (map velocity boids)
        s       = 1.0 / (cast $ length boids)
        v'      = vecScale v s
   in   vecScale (vecSub v' (velocity b)) a

wraparound : Vec2 -> Vec2
wraparound (MkVec2 x y) 
 = let  w = maxx-minx
        h = maxy-miny
        x' = if x > maxx then x - w else (if x < minx then x+w else x)
        y' = if y > maxy then y - h else (if y < miny then y+h else y)

   in MkVec2 x' y'

splitBoxHoriz :  (Vec2, Vec2, Double, Double) 
              -> List (Vec2, Vec2, Double, Double)
splitBoxHoriz ((MkVec2 lx ly), (MkVec2 hx hy), ax, ay) =
    if hx-lx > w
      then [(MkVec2 minx ly,              MkVec2 maxx hy, ax, ay)]
      else if lx < minx
      then [ (MkVec2 minx ly,             MkVec2 hx hy, ax, ay)
           , (MkVec2 (maxx-(minx-lx)) ly, MkVec2 maxx hy, (ax-w), ay)]
        else if hx > maxx
          then [ (MkVec2 lx ly,               MkVec2 maxx hy, ax, ay)
               , (MkVec2 minx ly,             MkVec2 (minx + (hx-maxx)) hy, ax+w, ay)]
          else [((MkVec2 lx ly), (MkVec2 hx hy), ax, ay)]
  where 
    w = maxx - minx

splitBoxVert :  (Vec2, Vec2, Double, Double)
             -> List (Vec2, Vec2, Double, Double)
splitBoxVert ((MkVec2 lx ly), (MkVec2 hx hy), ax, ay) =
    if hy-ly > h
      then [(MkVec2 lx miny,              MkVec2 hx maxy, ax, ay)]
      else if ly < miny
        then [ (MkVec2 lx miny,             MkVec2 hx hy, ax, ay)
            ,  (MkVec2 lx (maxy-(miny-ly)), MkVec2 hx maxy, ax, ay-h) ]
        else if hy > maxy
          then [ (MkVec2 lx ly,               MkVec2 hx maxy, ax, ay)
               , (MkVec2 lx miny,             MkVec2 hx (miny + (hy-maxy)), ax, ay+h) ]
          else [((MkVec2 lx ly), (MkVec2 hx hy), ax, ay)]
  where
    h = maxy-miny

||| Move one boid, with respect to its neighbours.
oneboid : Boid -> List Boid -> Boid
oneboid b boids 
 = let  c       = cohesion b boids cParam
        s       = separation b boids sParam
        a       = alignment b boids aParam
        p       = position b
        v       = velocity b
        id      = identifier b
        v'      = vecAdd v (vecScale (vecAdd c (vecAdd s a)) 0.1)
        v''     = limiter (vecScale v' 1.0025) vLimit
        p'      = vecAdd p v''

  in    MkBoid    
          id              -- identifie
          (wraparound p') -- position
          v''             -- velocity
          c               -- dbgC
          s               -- dbgS
          a               -- dbgA

-- adjuster for wraparound
adj1 :  Double 
     -> Double 
     -> (Vec2, Boid) 
     -> (Vec2, Boid)
adj1 ax ay (pos, theboid) = (vecAdd pos av, record { position = vecAdd p av } theboid)
  where 
    av = MkVec2 ax ay
    p = position theboid

adjust :  KDTreeNode Boid 
         -> Vec2 
         -> Vec2 
         -> Double 
         -> Double 
         -> List (Vec2, Boid)
adjust w lo hi ax ay 
  = let neighbors = kdtRangeSearch w lo hi
    in  map (adj1 ax ay) neighbors

||| Neighbor finding code
--
--   This is slightly tricky if we want to represent a world that wraps
--   around in one or more dimensions (aka, a torus or cylinder).
--
--   The issue is that we need to split the bounding box that we query the
--   KDTree with when that box extends outside the bounds of the world.
--   Furthermore, when a set of boids are found in the split bounding boxes
--   representing a neighbor after wrapping around, we need to adjust the
--   relative position of those boids with respect to the reference frame
--   of the central boid.  For example, if the central boid is hugging the left
--   boundary, and another boid is right next to it hugging the right
--   boundary, their proper distance is likely very small.  If the one on the
--   right boundary isn't adjusted, then the distance will actually appear to
--   be very large (approx. the width of the world).
findNeighbors : KDTreeNode Boid -> Boid -> List Boid
findNeighbors w b = b :: bs
  where
    p : Vec2
    p = position b

    vlo : Vec2
    vlo = vecSub p epsvec
    
    vhi : Vec2
    vhi = vecAdd p epsvec
    
    splith : List (Vec2, Vec2, Double, Double)
    splith = splitBoxHoriz (vlo, vhi, 0.0, 0.0)

    splitv : List (Vec2, Vec2, Double, Double)
    splitv = concatMap splitBoxVert splith

    ns : List (Vec2, Boid)
    ns = concatMap (\(lo, hi, fx, fy) => adjust w lo hi fx fy) splitv

    dists : List (Double, Boid)
    dists = map (\(np,n) => (vecNorm (vecSub p np), n)) ns

    bs : List Boid
    bs = map snd (filter (\(d,_) => d <= epsilon) dists)

iteration : ViewPort -> Double -> KDTreeNode Boid -> KDTreeNode Boid
iteration vp step w 
 = let  all     = kdtreeToList w
        boids   = mapKDTree w (\i => oneboid i all)
   in   foldl (\t, b => kdtAddPoint t (position b) b) newKDTree boids

iterationkd : ViewPort -> Double -> KDTreeNode Boid -> KDTreeNode Boid
iterationkd vp step w 
 = let  boids = mapKDTree w (\i => oneboid i (findNeighbors w i))
   in   foldl (\t, b => kdtAddPoint t (position b) b) newKDTree boids

-- Main -----------------------------------------------------------------------
main : IO ()
main = do
  let w = MkWorld   
      (maxx - minx) -- width
      (maxy - miny) -- height
      700           -- pixWidth
      700           -- pixHeight

  bs <- initialize 500 10.0 0.5
  let t   = foldl (\t, b => kdtAddPoint t (position b) b) newKDTree bs

  simulate (InWindow "Boids" (pixWidth w, pixHeight w) (10,10))
          (greyN 0.1) 30 t (renderboids w) iterationkd