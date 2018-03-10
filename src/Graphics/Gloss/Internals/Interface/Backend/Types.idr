module Graphics.Gloss.Internals.Interface.Backend.Types

import        Data.IORef

import public Graphics.Gloss.Data.Display

mutual
  -------------------------------------------------------------------------------
  public export
  data Callback
          = Display  (Backend a => IORef a -> IO ()) -- DisplayCallback
          | KeyMouse (Backend a => IORef a -> Key -> KeyState -> Modifiers -> (Int,Int) -> IO ()) -- KeyboardMouseCallback
          | Idle     (Backend a => IORef a -> IO ()) -- IdleCallback
          | Motion   (Backend a => IORef a -> (Int,Int) -> IO ())-- MotionCallback
          | Reshape  (Backend a => IORef a -> (Int,Int) -> IO ()) -- ReshapeCallback

  ||| Check if this is an `Idle` callback.
  export
  isIdleCallback : Callback -> Bool
  isIdleCallback (Idle _) = True
  isIdleCallback _        = False

  -------------------------------------------------------------------------------
  -- This is Glosses view of mouse and keyboard events.
  -- The actual events provided by the backends are converted to this form
  -- by the backend module.
  public export
  data Key
          = CharKey     Char
          | SpecialKey  SpecialKeyData
          | MouseButton MouseButtonData
          --deriving (Show, Eq, Ord)

  public export
  data MouseButtonData
          = LeftButton
          | MiddleButton
          | RightButton
          | WheelUp
          | WheelDown
          | AdditionalButton Int
          --deriving (Show, Eq, Ord)

  public export
  data KeyState
          = Down
          | Up
          --deriving (Show, Eq, Ord)

  public export
  data SpecialKeyData
          = KeyUnknown
          | KeySpace
          | KeyEsc
          | KeyF1
          | KeyF2
          | KeyF3
          | KeyF4
          | KeyF5
          | KeyF6
          | KeyF7
          | KeyF8
          | KeyF9
          | KeyF10
          | KeyF11
          | KeyF12
          | KeyF13
          | KeyF14
          | KeyF15
          | KeyF16
          | KeyF17
          | KeyF18
          | KeyF19
          | KeyF20
          | KeyF21
          | KeyF22
          | KeyF23
          | KeyF24
          | KeyF25
          | KeyUp
          | KeyDown
          | KeyLeft
          | KeyRight
          | KeyTab
          | KeyEnter
          | KeyBackspace
          | KeyInsert
          | KeyNumLock
          | KeyBegin
          | KeyDelete
          | KeyPageUp
          | KeyPageDown
          | KeyHome
          | KeyEnd
          | KeyShiftL
          | KeyShiftR
          | KeyCtrlL
          | KeyCtrlR
          | KeyAltL
          | KeyAltR
          | KeyPad0
          | KeyPad1
          | KeyPad2
          | KeyPad3
          | KeyPad4
          | KeyPad5
          | KeyPad6
          | KeyPad7
          | KeyPad8
          | KeyPad9
          | KeyPadDivide
          | KeyPadMultiply
          | KeyPadSubtract
          | KeyPadAdd
          | KeyPadDecimal
          | KeyPadEqual
          | KeyPadEnter
          --deriving (Show, Eq, Ord)

  public export
  record Modifiers where
    constructor MkModifiers
    shift : KeyState
    ctrl  : KeyState
    alt   : KeyState
    -- deriving (Show, Eq, Ord)

  ||| The functions every backend window managed backend needs to support.
  |||
  |||   The Backend module interfaces with the window manager, and handles opening
  |||   and closing the window, and managing key events etc.
  |||
  |||   It doesn't know anything about drawing lines or setting colors.
  |||   When we get a display callback, Gloss will perform OpenGL actions, and
  |||   the backend needs to have OpenGL in a state where it's able to accept them.
  |||
  public export
  interface Backend a where
    ||| Initialize the state used by the backend. If you don't use any state,
    |||  make a Unit-like type; see the GLUT backend for an example.
    initBackendState           : a

    ||| Perform any initialization that needs to happen before opening a window
    |||   The Boolean flag indicates if any debug information should be printed to
    |||   the terminal
    initializeBackend          : IORef a -> Bool -> IO ()

    ||| Perform any deinitialization and close the backend.
    exitBackend                : IORef a -> IO ()

    ||| Open a window with the given display mode.
    openWindow                 : IORef a -> Display -> IO ()

    ||| Dump information about the backend to the terminal.
    dumpBackendState           : IORef a -> IO ()

    ||| Install the display callbacks.
    installDisplayCallback     : IORef a -> List Callback -> IO ()

    ||| Install the window close callback.
    installWindowCloseCallback : IORef a -> IO ()

    ||| Install the reshape callbacks.
    installReshapeCallback     : IORef a -> List Callback -> IO ()

    ||| Install the keymouse press callbacks.
    installKeyMouseCallback    : IORef a -> List Callback -> IO ()

    ||| Install the mouse motion callbacks.
    installMotionCallback      : IORef a -> List Callback -> IO ()

    ||| Install the idle callbacks.
    installIdleCallback        : IORef a -> List Callback -> IO ()

    ||| The mainloop of the backend.
    runMainLoop                : IORef a -> IO ()

    ||| A function that signals that screen has to be updated.
    postRedisplay              : IORef a -> IO ()

    ||| Function that returns (width,height) of the window in pixels.
    getWindowDimensions        : IORef a -> IO (Int,Int)

    ||| Function that reports the time elapsed since the application started.
    |||   (in seconds)
    elapsedTime                : IORef a -> IO Double

    ||| Function that puts the current thread to sleep for 'n' seconds.
    sleep                      : IORef a -> Double -> IO ()


  -- The callbacks should work for all backends. We pass a reference to the
  -- backend state so that the callbacks have access to the class dictionary and
  -- can thus call the appropriate backend functions.

  {-
  ||| Display callback has no arguments.
  DisplayCallback : Type
  DisplayCallback = (Backend a => IORef a -> IO ())

  type DisplayCallback
          = forall a . Backend a => IORef a -> IO ()
          -}
    {-
  ||| Arguments: KeyType, Key Up \/ Down, Ctrl \/ Alt \/ Shift pressed, latest mouse location.

  KeyboardMouseCallback : Type
  KeyboardMouseCallback = (Backend a => IORef a -> Key -> KeyState -> Modifiers -> (Int,Int) -> IO ())

  type KeyboardMouseCallback 
          = forall a . Backend a => IORef a -> Key -> KeyState -> Modifiers -> (Int,Int) -> IO ()
  -}
    {-
  ||| Arguments: (PosX,PosY) in pixels.

  MotionCallback : Type
  MotionCallback = (Backend a => IORef a -> (Int,Int) -> IO ())

  type MotionCallback
          = forall a . Backend a => IORef a -> (Int,Int) -> IO ()
  -}
    {-
  ||| No arguments.

  IdleCallback : Type
  IdleCallback = (Backend a => IORef a -> IO ())

  type IdleCallback
          = forall a . Backend a => IORef a -> IO ()
  -}
    {-
  ||| Arguments: (Width,Height) in pixels.

  ReshapeCallback : Type
  ReshapeCallback = (Backend a => IORef a -> (Int,Int) -> IO ())

  type ReshapeCallback
          = forall a . Backend a => IORef a -> (Int,Int) -> IO ()
  -}

export
Eq SpecialKeyData where
  (==) KeyUnknown KeyUnknown = True
  (==) KeySpace KeySpace = True
  (==) KeyEsc KeyEsc = True
  (==) KeyF1 KeyF1 = True
  (==) KeyF2 KeyF2 = True
  (==) KeyF3 KeyF3 = True
  (==) KeyF4 KeyF4 = True
  (==) KeyF5 KeyF5 = True 
  (==) KeyF6 KeyF6 = True
  (==) KeyF7 KeyF7 = True
  (==) KeyF8 KeyF8 = True
  (==) KeyF9 KeyF9 = True
  (==) KeyF10 KeyF10 = True
  (==) KeyF11 KeyF11 = True
  (==) KeyF12 KeyF12 = True
  (==) KeyF13 KeyF13 = True
  (==) KeyF14 KeyF14 = True
  (==) KeyF15 KeyF15 = True
  (==) KeyF16 KeyF16 = True
  (==) KeyF17 KeyF17 = True
  (==) KeyF18 KeyF18 = True
  (==) KeyF19 KeyF19 = True
  (==) KeyF20 KeyF20 = True
  (==) KeyF21 KeyF21 = True
  (==) KeyF22 KeyF22 = True
  (==) KeyF23 KeyF23 = True
  (==) KeyF24 KeyF24 = True
  (==) KeyF25 KeyF25 = True
  (==) KeyUp KeyUp = True
  (==) KeyDown KeyDown = True
  (==) KeyLeft KeyLeft = True
  (==) KeyRight KeyRight = True
  (==) KeyTab KeyTab = True
  (==) KeyEnter KeyEnter = True
  (==) KeyBackspace KeyBackspace = True
  (==) KeyInsert KeyInsert = True
  (==) KeyNumLock KeyNumLock = True
  (==) KeyBegin KeyBegin = True
  (==) KeyDelete KeyDelete = True
  (==) KeyPageUp KeyPageUp = True
  (==) KeyPageDown KeyPageDown = True
  (==) KeyHome KeyHome = True
  (==) KeyEnd KeyEnd = True
  (==) KeyShiftL KeyShiftL = True
  (==) KeyShiftR KeyShiftR = True
  (==) KeyCtrlL KeyCtrlL = True
  (==) KeyCtrlR KeyCtrlR = True
  (==) KeyAltL KeyAltL = True
  (==) KeyAltR KeyAltR = True
  (==) KeyPad0 KeyPad0 = True
  (==) KeyPad1 KeyPad1 = True
  (==) KeyPad2 KeyPad2 = True
  (==) KeyPad3 KeyPad3 = True
  (==) KeyPad4 KeyPad4 = True
  (==) KeyPad5 KeyPad5 = True
  (==) KeyPad6 KeyPad6 = True
  (==) KeyPad7 KeyPad7 = True
  (==) KeyPad8 KeyPad8 = True
  (==) KeyPad9 KeyPad9 = True 
  (==) KeyPadDivide KeyPadDivide = True
  (==) KeyPadMultiply KeyPadMultiply = True
  (==) KeyPadSubtract KeyPadSubtract = True
  (==) KeyPadAdd KeyPadAdd = True
  (==) KeyPadDecimal KeyPadDecimal = True
  (==) KeyPadEqual KeyPadEqual = True
  (==) KeyPadEnter KeyPadEnter = True
  (==) _ _ = False

export
Eq MouseButtonData where
  (==) LeftButton LeftButton = True
  (==) MiddleButton MiddleButton = True
  (==) RightButton RightButton = True
  (==) WheelUp WheelUp = True
  (==) WheelDown WheelDown = True
  (==) (AdditionalButton b1) (AdditionalButton b2) = b1 == b2 
  (==) _ _ = False

export
Eq Key where
  (==) (CharKey c1) (CharKey c2)         = c1 == c2
  (==) (SpecialKey k1) (SpecialKey k2)   = k1 == k2
  (==) (MouseButton b1) (MouseButton b2) = b1 == b2
  (==) _ _ = False

export
Eq KeyState where
  (==) Down Down = True
  (==) Up Up = True
  (==) _ _ = False

export 
Eq Modifiers where
  (==) (MkModifiers s1 c1 a1) (MkModifiers s2 c2 a2) = s1 == s2 && c1 == c2 && a1 == a2 
