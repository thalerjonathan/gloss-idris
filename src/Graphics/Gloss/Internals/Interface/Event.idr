module Graphics.Gloss.Internals.Interface.Event

import Data.IORef
import Graphics.Gloss.Internals.Interface.Backend

||| Possible input events.
public export
data Event
        = EventKey    Key KeyState Modifiers (Double, Double)
        | EventMotion (Double, Double)
        | EventResize (Int, Int)
        --deriving (Eq, Show)

export
convertPoint :
        Backend a
        => IORef a
        -> (Int, Int)
        -> IO (Double,Double)
convertPoint backendRef pos = do
  (sizeX_, sizeY_)        <- getWindowDimensions backendRef
  let (sizeX, sizeY)      = (cast {to=Double} sizeX_, cast {to=Double} sizeY_)

  let (px_, py_)          = pos
  let px                  = cast px_
  let py                  = sizeY - cast py_
  
  let px'                 = px - sizeX / 2
  let py'                 = py - sizeY / 2
  let pos'                = (px', py')
  pure pos'

export
keyMouseEvent :
        Backend a
        => IORef a
        -> Key
        -> KeyState
        -> Modifiers
        -> (Int, Int)
        -> IO Event
keyMouseEvent backendRef key keyState modifiers pos
  = EventKey key keyState modifiers <$> convertPoint backendRef pos

export
motionEvent :
        Backend a
        => IORef a
        -> (Int, Int)
        -> IO Event
motionEvent backendRef pos
  = EventMotion <$> convertPoint backendRef pos
