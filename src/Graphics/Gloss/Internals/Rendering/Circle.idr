||| Fast(ish) rendering of circles.
module Graphics.Gloss.Internals.Rendering.Circle

import Graphics.Rendering.Gl.Types
import Graphics.Rendering.Gl.Gl41 as GL

{-
        ( renderCircle
        , renderArc)
where
import  Graphics.Gloss.Internals.Rendering.Common
import  GHC.Exts
import  qualified Graphics.Rendering.OpenGL.GL          as GL
-}

||| Decide how many line segments to use to render the circle.
|||  The number of segments we should use to get a nice picture depends on 
|||  the size of the circle on the screen, not its intrinsic radius.
|||  If the viewport has been zoomed-in then we need to use more segments.
--
circleSteps : Double -> Int
circleSteps sDiam =
  if sDiam < 8
    then 8
    else if sDiam < 16
      then 16
      else if sDiam < 32
        then 32
        else 64
{-# INLINE circleSteps #-}

addPoint : Double -> Double -> IO ()
addPoint x y = GL.glVertex2f x y
{-# INLINE addPoint #-}

addPointOnCircle : Double -> Double -> Double -> Double -> IO ()
addPointOnCircle posX posY rad tt =
  addPoint
    (posX + (rad * (cos tt)))
    (posY + (rad * (sin tt)))
{-# INLINE addPointOnCircle #-}

||| Step functions -------------------------------------------------------------
renderCircleLine_step : 
           Double -> Double
        -> Double -> Double
        -> Double -> Double 
        -> IO ()
renderCircleLine_step posX posY tStep tStop rad tt = 
  if tt > tStop
    then pure ()
    else do
      addPointOnCircle posX posY rad tt
      renderCircleLine_step posX posY tStep tStop rad 
        (tt + tStep)
{-# INLINE renderCircleLine_step #-}

renderCircleStrip_step 
        : Double -> Double 
        -> Double -> Double 
        -> Double -> Double
        -> Double -> Double -> IO ()
renderCircleStrip_step posX posY tStep tStop r1 t1 r2 t2 =
  if t1 > tStop
    then pure ()
    else do    
      addPointOnCircle posX posY r1 t1
      addPointOnCircle posX posY r2 t2
      renderCircleStrip_step posX posY tStep tStop r1 
              (t1 + tStep) r2 (t2 + tStep)
{-# INLINE renderCircleStrip_step #-}

||| Convert degrees to radians
{-# INLINE degToRad #-}
degToRad : Double -> Double
degToRad d = d * pi / 180

||| Normalise an angle to be between 0 and 2*pi radians
{-# INLINE normalizeAngle #-}
normalizeAngle : Double -> Double
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
  where  
    floor' : Double -> Double
    floor' x = floor x

||| Render a circle as a line.
renderCircleLine : Double -> Double -> Int -> Double -> IO ()
renderCircleLine posX posY steps rad
 = let  tStep = (2 * pi) / cast steps
        tStop = (2 * pi)

   in  do
      GL.glBegin GL_LINE_LOOP
      renderCircleLine_step posX posY tStep tStop rad 0.0
      GL.glEnd
{-
    GL.renderPrimitive GL.LineLoop
         $ renderCircleLine_step posX posY tStep tStop rad 0.0
         -}
{-# INLINE renderCircleLine #-}

||| Render a circle with a given thickness as a triangle strip
renderCircleStrip : Double -> Double -> Int -> Double -> Double -> IO ()
renderCircleStrip posX posY steps r width
 = let  tStep = (2 * pi) / cast steps
        tStop = (2 * pi) + tStep / 2
        r1    = r - width / 2
        r2    = r + width / 2

   in   do
      GL.glBegin GL_TRIANGLE_STRIP
      renderCircleStrip_step posX posY tStep tStop r1 0.0 r2 (tStep / 2.0)
      GL.glEnd

{-# INLINE renderCircleStrip #-}

-- Circle ---------------------------------------------------------------------
||| Render a circle with the given thickness
export
renderCircle : Double -> Double -> Double -> Double -> Double -> IO ()
renderCircle posX posY scaleFactor radius_ thickness_ =
    if thickness_ == 0
      then goZero (abs radius_)
      else goNonZero (abs radius_) (abs thickness_)
  where 
    goZero : Double -> IO ()
    goZero radius = do
      let radScreenThick = scaleFactor * radius -- + thickness / 2) remember, thickness is 0, we can omit 
      if radScreenThick <= 1
        then do
          --GL.renderPrimitive GL.Points
          GL.glBegin GL_POINTS
          --$ GL.vertex $ GL.Vertex2 (gf posX) (gf posY)
          GL.glVertex2f posX posY
          GL.glEnd
        else do
          let radScreen = scaleFactor * radius
          let steps     = circleSteps radScreen
          renderCircleLine  posX posY steps radius

    goNonZero : Double -> Double -> IO () 
    goNonZero radius thickness = do
      let radScreen = scaleFactor * (radius + thickness / 2)
      let steps     = circleSteps radScreen
      renderCircleStrip posX posY steps radius thickness
    {-
      -- If the circle is smaller than a pixel, render it as a point.
      | thickness     == 0
      , radScreen     <- scaleFactor * (radius + thickness / 2)
      , radScreen     <= 1
      = 

      -- Render zero thickness circles with lines.
      | thickness == 0
      , radScreen     <- scaleFactor * radius
      , steps         <- circleSteps radScreen
      = renderCircleLine  posX posY steps radius

      -- Some thick circle.
      | radScreen     <- scaleFactor * (radius + thickness / 2)
      , steps         <- circleSteps radScreen
      = renderCircleStrip posX posY steps radius thickness
-}


-- Arc ------------------------------------------------------------------------

||| Render an arc as a line.
renderArcLine : Double -> Double -> Int -> Double -> Double -> Double -> IO ()
renderArcLine posX posY steps rad a1 a2
 = let  tStep     = (2 * pi) / cast steps
        tStart    = degToRad a1
        tStop     = degToRad a2 + if a1 >= a2 then 2 * pi else 0
        -- force the line to end at the desired angle
        endVertex = addPointOnCircle posX posY rad tStop

   in   do
      GL.glBegin GL_LINE_STRIP
      renderCircleLine_step posX posY tStep tStop rad tStart
      endVertex
      GL.glEnd
{-# INLINE renderArcLine #-}


||| Render an arc with a given thickness as a triangle strip
renderArcStrip : Double -> Double -> Int -> Double -> Double -> Double -> Double -> IO ()
renderArcStrip posX posY steps r a1 a2 width
 = let  tStep           = (2 * pi) / cast steps

        t1              = normalizeAngle $ degToRad a1
        t2              = normalizeAngle $ degToRad a2
        (tStart, tStop) = if t1 <= t2 then (t1, t2) else (t2, t1)
        tDiff           = tStop - tStart
        tMid            = tStart + tDiff / 2

        tStep'    = tStep
        tStep2'   = tStep / 2
        tStart'   = tStart
        tStop'    = tStop
        tCut'     = tStop - tStep
        tMid'     = tMid
        r1'       = r - width / 2
        r2'       = r + width / 2
                
   in   do
      GL.glBegin GL_TRIANGLE_STRIP  
      -- start vector
      addPointOnCircle posX posY r1' tStart'
      addPointOnCircle posX posY r2' tStart'

      -- If we don't have a complete step then just drop a point
      -- between the two ending lines.
      if tDiff < tStep
        then do
          addPointOnCircle posX posY r1' tMid'
          -- end vectors
          addPointOnCircle posX posY r2' tStop'
          addPointOnCircle posX posY r1' tStop'
          GL.glEnd
        else do
          renderCircleStrip_step posX posY tStep' tCut' r1' tStart' r2'
                  (tStart' + tStep2')
          -- end vectors
          addPointOnCircle posX posY r1' tStop'
          addPointOnCircle posX posY r2' tStop'
          GL.glEnd
{-# INLINE renderArcStrip #-}

||| Render an arc with the given thickness.
export
renderArc : Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
renderArc posX posY scaleFactor radius_ a1 a2 thickness_ =
    if thickness_ == 0
      then goZero (abs radius_)
      else goNonZero (abs radius_) (abs thickness_)
  where 
    goZero : Double -> IO ()
    goZero radius = do
      let radScreen = scaleFactor * radius
      let steps     = circleSteps radScreen
      renderArcLine posX posY steps radius a1 a2

    goNonZero : Double -> Double -> IO ()
    goNonZero radius thickness = do
      let radScreen = scaleFactor * (radius + thickness / 2)
      let steps     = circleSteps radScreen
      renderArcStrip posX posY steps radius a1 a2 thickness

{- Unused sector drawing code.
   Sectors are currently drawn as compound Pictures,
   but we might want this if we end up implementing the ThickSector 
   version as well.

||| Render a sector as a line.
renderSectorLine :: Double -> Double -> Int -> Double -> Double -> Double -> IO ()
renderSectorLine pX@(F# posX) pY@(F# posY) steps (F# rad) a1 a2
 = let  n               = fromIntegral steps
        !(F# tStep)     = (2 * pi) / n
        !(F# tStart)    = degToRad a1
        !(F# tStop)     = degToRad a2 + if a1 >= a2 then 2 * pi else 0

        |||need to set up the edges of the start/end triangles
        startVertex     = GL.vertex $ GL.Vertex2 (gf pX) (gf pY)
        endVertex       = addPointOnCircle posX posY rad tStop

   in   GL.renderPrimitive GL.LineLoop
         $ do   startVertex
                renderCircleLine_step posX posY tStep tStop rad tStart
                endVertex

||| Render a sector.
renderSector :: Double -> Double -> Double -> Double -> Double -> Double -> IO ()
renderSector posX posY scaleFactor radius a1 a2
        | radScreen     <- scaleFactor * radius
        , steps         <- circleSteps (2 * radScreen)
        = renderSectorLine posX posY steps radius a1 a2
-}

