module Graphics.Gloss.Internals.Interface.Event

import Data.IORef
import Graphics.Gloss.Internals.Interface.Backend

||| Possible input events.
public export
data Event
  = EventKey    Key KeyState Modifiers (Double, Double)
  | EventMotion (Double, Double)
  | EventResize (Int, Int)

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

export
Show SpecialKeyData where
  show KeyUnknown = "KeyUnknown"
  show KeySpace = "KeySpace"
  show KeyEsc = "KeyEsc"
  show KeyF1 = "KeyF1"
  show KeyF2 = "KeyF2"
  show KeyF3 = "KeyF3"
  show KeyF4 = "KeyF4"
  show KeyF5 = "KeyF5"
  show KeyF6 = "KeyF6"
  show KeyF7 = "KeyF7"
  show KeyF8 = "KeyF8"
  show KeyF9 = "KeyF9"
  show KeyF10 = "KeyF10"
  show KeyF11 = "KeyF11"
  show KeyF12 = "KeyF12"
  show KeyF13 = "KeyF13"
  show KeyF14 = "KeyF14"
  show KeyF15 = "KeyF15"
  show KeyF16 = "KeyF16"
  show KeyF17 = "KeyF17"
  show KeyF18 = "KeyF18"
  show KeyF19 = "KeyF19"
  show KeyF20 = "KeyF20"
  show KeyF21 = "KeyF21"
  show KeyF22 = "KeyF22"
  show KeyF23 = "KeyF23"
  show KeyF24 = "KeyF24"
  show KeyF25 = "KeyF25"
  show KeyUp = "KeyUp"
  show KeyDown = "KeyDown"
  show KeyLeft = "KeyLeft"
  show KeyRight = "KeyRight"
  show KeyTab = "KeyTab"
  show KeyEnter = "KeyEnter"
  show KeyBackspace = "KeyBackspace"
  show KeyInsert = "KeyInsert"
  show KeyNumLock = "KeyNumLock"
  show KeyBegin = "KeyBegin"
  show KeyDelete = "KeyDelete"
  show KeyPageUp = "KeyPageUp"
  show KeyPageDown = "KeyPageDown"
  show KeyHome = "KeyHome"
  show KeyEnd = "KeyEnd"
  show KeyShiftL = "KeyShiftL"
  show KeyShiftR = "KeyShiftR"
  show KeyCtrlL = "KeyCtrlL"
  show KeyCtrlR = "KeyCtrlR"
  show KeyAltL = "KeyAltL"
  show KeyAltR = "KeyAltR"
  show KeyPad0 = "KeyPad0"
  show KeyPad1 = "KeyPad1"
  show KeyPad2 = "KeyPad2"
  show KeyPad3 = "KeyPad3"
  show KeyPad4 = "KeyPad4"
  show KeyPad5 = "KeyPad5"
  show KeyPad6 = "KeyPad6"
  show KeyPad7 = "KeyPad7"
  show KeyPad8 = "KeyPad8"
  show KeyPad9 = "KeyPad9"
  show KeyPadDivide = "KeyPadDivide"
  show KeyPadMultiply = "KeyPadMultiply"
  show KeyPadSubtract = "KeyPadSubtract"
  show KeyPadAdd = "KeyPadAdd"
  show KeyPadDecimal = "KeyPadDecimal"
  show KeyPadEqual = "KeyPadEqual"
  show KeyPadEnter = "KeyPadEnter"

export
Show KeyState where
  show Down = "Down"
  show Up = "Up"

export
Show Modifiers where
  show (MkModifiers shift ctrl alt) = "Modifiers (shift:" ++ show shift ++ 
                                      ", ctrl:" ++ show ctrl ++ 
                                      ", alt:" ++ show alt ++ ")"

export
Show MouseButtonData where
  show LeftButton = "LeftButton"
  show MiddleButton = "MiddleButton"
  show RightButton = "RightButton"
  show WheelUp = "WheelUp"
  show WheelDown = "WheelDown"
  show (AdditionalButton x) = "AdditionalButton " ++ show x

export
Show Key where
  show (CharKey c) = "CharKey " ++ show c
  show (SpecialKey kd) = "SpecialKey " ++ show kd
  show (MouseButton mb) = "MouseButton " ++ show mb

export
Show Event where
  show (EventKey key keyState modifiers (x, y)) = "EventKey " ++ show key ++ 
                                                  " " ++ show keyState ++ 
                                                  " " ++ show modifiers ++ 
                                                  " pos = (" ++ show x ++ "/" ++ show y ++ ")"
  show (EventMotion (x, y)) = "EventMotion (" ++ show x ++ "/" ++ show y ++ ")"
  show (EventResize (x, y)) = "EventResize (" ++ show x ++ "/" ++ show y ++ ")"