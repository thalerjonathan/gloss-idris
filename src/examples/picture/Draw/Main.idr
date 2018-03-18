-- Simple picture drawing application. 
--  Like MSPaint, but you can only draw lines.
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Config

||| The game state.
-- The current line being drawn and All the lines drawn previously.
data DrawState = MkState (Maybe Path) (List Picture)
  
||| A Line Segment
Segment : Type
Segment = ((Double, Double), (Double, Double))

||| Convert our state to a picture.
makePicture : DrawState -> Picture
makePicture (MkState m xs) = Pictures (maybe xs (\x => Line x :: xs) m)

||| Handle mouse click and motion events.
handleEvent : Event -> DrawState -> DrawState
-- If the mouse has moved, then extend the current line.
handleEvent (EventMotion (x, y)) (MkState (Just ps) ss) 
  = MkState (Just ((x, y) :: ps)) ss
-- Start drawing a new line.
handleEvent (EventKey (MouseButton LeftButton) Down _ pt@(x,y)) (MkState Nothing ss)
  = MkState (Just [pt]) ((Translate x y $ Scale 0.1 0.1 $ Text "Down") :: ss)
-- Finish drawing a line, and add it to the picture.
handleEvent (EventKey (MouseButton LeftButton) Up _ pt@(x,y)) (MkState (Just ps) ss)
  = MkState Nothing ((Translate x y $ Scale 0.1 0.1 $ Text "up") :: Line (pt :: ps) :: ss)
handleEvent _ s = s

stepWorld : Double -> DrawState -> DrawState
stepWorld _ = id

main : IO ()
main = do
  let state = MkState Nothing []
  play (InWindow "Draw" (600, 600) (0,0))
          white 100 state
          makePicture handleEvent stepWorld