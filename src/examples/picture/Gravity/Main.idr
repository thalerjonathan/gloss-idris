import Graphics.Gloss
import Graphics.Gloss.Interface.Environment

import Config
import Random

-- x, y, dx, dy
Particle : Type
Particle = (Double, Double, Double, Double) 

||| Generates particles from StdGen
generateParticles :  RandomIntStream 
                  -> Int 
                  -> Int 
                  -> List Particle
generateParticles rs widthInt heightInt 
    = let (gen1,gen2)  = splitRandom rs
          rands1 = transformStream (0, 1) gen1
          rands2 = transformStream (0, 1) gen2
          tups = take 50 $ zip rands1 rands2 
      in  map (g . f)  tups
  where
    -- change range [0,1] ->  [-s/2,s/2]
    f : (Double, Double) -> (Double, Double)
    f (x,y) = (x * cast widthInt - cast widthInt / 2, y * cast heightInt - cast heightInt / 2) 

    -- add speed of 0
    g : (Double, Double) -> Particle
    g (x,y) = (x,y,0,0) 

||| Particle to its picture
particleImage : Particle -> Picture
particleImage (x,y,_,_) 
  = translate x y $ color white $ circleSolid 2

||| Moves particles based on their speed
moveParticles : Double -> List Particle -> List Particle
moveParticles dt 
  = map (\(x,y,dx,dy) => (x+dx*dt,y+dy*dt,dx,dy))

||| Normalized vector from one point to another.
direction : (Double, Double) -> (Double, Double) -> (Double, Double)
direction (x,y) (x',y') =
  let dx      = x' - x
      dy      = y' - y
      scale'  = 1 / sqrt (dx * dx + dy * dy)
  in  (dx * scale', dy * scale')

||| Checks if Doubles not too close to each other
separated : Double -> Double -> Bool
separated x y = 0.001 < abs (x - y)

||| Gravitational force of one particle to another
gravitation : (Double,Double) -> (Double,Double) -> Double
gravitation (x,y) (x',y') 
    = g / sqrt (dx * dx + dy * dy)
  where  
    dx = x' - x
    dy = y' - y
    g = 1

||| Given particles to be gravitating to and for how long,
|||  updates a single particle's speed 
gravitate : List Particle -> Double -> Particle -> Particle
gravitate [] _ p = p
gravitate ((x',y',_,_) :: ps) dt p@(x,y,dx,dy) 
  =  -- To dodge divByZero or near divByZero anomalies
    if separated x x' && separated y y' 
      then gravitate ps dt p'
      else gravitate ps dt p   
  where
    g : Double
    g = gravitation (x,y) (x',y')

    dir : (Double, Double)
    dir = direction (x,y) (x',y')

    ddx : Double
    ddx = fst dir * g

    ddy : Double
    ddy = snd dir * g

    p' : Particle
    p' = (x,y,dx+ddx,dy+ddy)

||| Accelerates particles based on gravity
accelerateParticles : Double -> List Particle -> List Particle
accelerateParticles dt ps = map (gravitate ps dt) ps

||| To update particles for next frame
updateParticles : Double -> List Particle -> List Particle
updateParticles dt 
  = (accelerateParticles dt) . (moveParticles dt)

main : IO ()
main = do
    --(width,height) <- getScreenSize
    let width = 800
    let height = 600

    let initialstate = generateParticles (randoms 42) width height
    simulate
      (InWindow "Gravity" (width, height) (0, 0)) --FullScreen 
      black 
      60
      initialstate
      (\xs => pictures $ map particleImage xs) 
      (const updateParticles)
