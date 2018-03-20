||| removed GLUT support, who uses GLUT these days anyway? its outdated
module Graphics.Gloss.Internals.Interface.Backend

import        Data.IORef

import        Graphics.Rendering.OpenGL.Internal.GLBindings as GL
import        Graphics.Rendering.OpenGL.Internal.GLEW as GLEW
import        Graphics.Rendering.OpenGL.Internal.Types
import        Graphics.UI.GLFW as GLFW

import public Graphics.Gloss.Data.Display

mutual
  ||| State of the GLFW backend library.
  public export
  record GLFWState where
    constructor MkGLFWState
    ||| Status of Ctrl, Alt or Shift (Up or Down?)
    modifiers     : Modifiers
    ||| Latest mouse position
    mousePosition : (Int,Int)
    ||| Latest mousewheel position
    mouseWheelPos : Int
    ||| Does the screen need to be redrawn?
    dirtyScreen   : Bool
    ||| Action that draws on the screen
    display       : IO ()
    ||| Action perforrmed when idling
    idle          : IO ()
    ||| The handle to the GLFW window
    winHdl        : GLFW.Window

  -------------------------------------------------------------------------------
  public export
  data Callback
          = Display  (Backend GLFWState => IORef GLFWState -> IO ()) -- DisplayCallback
          | KeyMouse (Backend GLFWState => IORef GLFWState -> Key -> KeyState -> Modifiers -> (Int,Int) -> IO ()) -- KeyboardMouseCallback
          | Idle     (Backend GLFWState => IORef GLFWState -> IO ()) -- IdleCallback
          | Motion   (Backend GLFWState => IORef GLFWState -> (Int,Int) -> IO ())-- MotionCallback
          | Reshape  (Backend GLFWState => IORef GLFWState -> (Int,Int) -> IO ()) -- ReshapeCallback

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

-- Key Code Conversion --------------------------------------------------------
||| Convert char keys to special keys to work around a bug in 
|||   GLFW 2.7. On OS X, GLFW sometimes registers special keys as char keys,
|||   so we convert them back here.
|||   GLFW 2.7 is current as of Nov 2011, and is shipped with the Hackage
|||   binding GLFW-b 0.2.*
charToSpecial : Char -> Key
charToSpecial c = case (cast {to=Int} c) of
  32    => SpecialKey KeySpace
  63232 => SpecialKey KeyUp
  63233 => SpecialKey KeyDown
  63234 => SpecialKey KeyLeft
  63235 => SpecialKey KeyRight
  63236 => SpecialKey KeyF1
  63237 => SpecialKey KeyF2
  63238 => SpecialKey KeyF3
  63239 => SpecialKey KeyF4
  63240 => SpecialKey KeyF5
  63241 => SpecialKey KeyF6
  63242 => SpecialKey KeyF7
  63243 => SpecialKey KeyF8
  63244 => SpecialKey KeyF9
  63245 => SpecialKey KeyF10
  63246 => SpecialKey KeyF11
  63247 => SpecialKey KeyF12
  63248 => SpecialKey KeyF13
  63272 => SpecialKey KeyDelete
  63273 => SpecialKey KeyHome
  63275 => SpecialKey KeyEnd
  63276 => SpecialKey KeyPageUp
  63277 => SpecialKey KeyPageDown
  _     => CharKey c

interface GLFWKey a where
  fromGLFW : a -> Key

GLFWKey GLFW.GLFWKey where
  fromGLFW key = case key of
    GLFW.CharKey c      => charToSpecial (toLower c)
    GLFW.KeySpace       => SpecialKey KeySpace
    GLFW.KeyEsc         => SpecialKey KeyEsc
    GLFW.KeyF1          => SpecialKey KeyF1
    GLFW.KeyF2          => SpecialKey KeyF2
    GLFW.KeyF3          => SpecialKey KeyF3
    GLFW.KeyF4          => SpecialKey KeyF4
    GLFW.KeyF5          => SpecialKey KeyF5
    GLFW.KeyF6          => SpecialKey KeyF6
    GLFW.KeyF7          => SpecialKey KeyF7
    GLFW.KeyF8          => SpecialKey KeyF8
    GLFW.KeyF9          => SpecialKey KeyF9
    GLFW.KeyF10         => SpecialKey KeyF10
    GLFW.KeyF11         => SpecialKey KeyF11
    GLFW.KeyF12         => SpecialKey KeyF12
    GLFW.KeyF13         => SpecialKey KeyF13
    GLFW.KeyF14         => SpecialKey KeyF14
    GLFW.KeyF15         => SpecialKey KeyF15
    GLFW.KeyF16         => SpecialKey KeyF16
    GLFW.KeyF17         => SpecialKey KeyF17
    GLFW.KeyF18         => SpecialKey KeyF18
    GLFW.KeyF19         => SpecialKey KeyF19
    GLFW.KeyF20         => SpecialKey KeyF20
    GLFW.KeyF21         => SpecialKey KeyF21
    GLFW.KeyF22         => SpecialKey KeyF22
    GLFW.KeyF23         => SpecialKey KeyF23
    GLFW.KeyF24         => SpecialKey KeyF24
    GLFW.KeyF25         => SpecialKey KeyF25
    GLFW.KeyUp          => SpecialKey KeyUp
    GLFW.KeyDown        => SpecialKey KeyDown
    GLFW.KeyLeft        => SpecialKey KeyLeft
    GLFW.KeyRight       => SpecialKey KeyRight
    GLFW.KeyTab         => SpecialKey KeyTab
    GLFW.KeyEnter       => SpecialKey KeyEnter
    GLFW.KeyBackspace   => SpecialKey KeyBackspace
    GLFW.KeyInsert      => SpecialKey KeyInsert
    GLFW.KeyDel         => SpecialKey KeyDelete
    GLFW.KeyPageup      => SpecialKey KeyPageUp
    GLFW.KeyPagedown    => SpecialKey KeyPageDown
    GLFW.KeyHome        => SpecialKey KeyHome
    GLFW.KeyEnd         => SpecialKey KeyEnd
    GLFW.KeyPad0        => SpecialKey KeyPad0
    GLFW.KeyPad1        => SpecialKey KeyPad1
    GLFW.KeyPad2        => SpecialKey KeyPad2
    GLFW.KeyPad3        => SpecialKey KeyPad3
    GLFW.KeyPad4        => SpecialKey KeyPad4
    GLFW.KeyPad5        => SpecialKey KeyPad5
    GLFW.KeyPad6        => SpecialKey KeyPad6
    GLFW.KeyPad7        => SpecialKey KeyPad7
    GLFW.KeyPad8        => SpecialKey KeyPad8
    GLFW.KeyPad9        => SpecialKey KeyPad9
    GLFW.KeyPadDivide   => SpecialKey KeyPadDivide
    GLFW.KeyPadMultiply => SpecialKey KeyPadMultiply
    GLFW.KeyPadSubtract => SpecialKey KeyPadSubtract
    GLFW.KeyPadAdd      => SpecialKey KeyPadAdd
    GLFW.KeyPadDecimal  => SpecialKey KeyPadDecimal
    GLFW.KeyPadEqual    => CharKey '='
    GLFW.KeyPadEnter    => SpecialKey KeyPadEnter
    _                   => SpecialKey KeyUnknown

GLFWKey GLFW.GLFWMouseButton where
  fromGLFW mouse = case mouse of
    GLFW.MouseButton1 => MouseButton LeftButton
    GLFW.MouseButton2 => MouseButton RightButton
    GLFW.MouseButton3 => MouseButton MiddleButton
    GLFW.MouseButton4 => MouseButton $ AdditionalButton 4
    GLFW.MouseButton5 => MouseButton $ AdditionalButton 5
    GLFW.MouseButton6 => MouseButton $ AdditionalButton 6
    GLFW.MouseButton7 => MouseButton $ AdditionalButton 7
    GLFW.MouseButton8 => MouseButton $ AdditionalButton 8


mutual
  public export
  Backend GLFWState where
    initBackendState           = glfwStateInit
    initializeBackend          = initializeGLFW
    exitBackend                = exitGLFW
    openWindow                 = openWindowGLFW
    dumpBackendState           = dumpStateGLFW
    installDisplayCallback     = installDisplayCallbackGLFW
    installWindowCloseCallback = installWindowCloseCallbackGLFW
    installReshapeCallback     = installReshapeCallbackGLFW
    installKeyMouseCallback    = installKeyMouseCallbackGLFW
    installMotionCallback      = installMotionCallbackGLFW
    installIdleCallback        = installIdleCallbackGLFW
    runMainLoop                = runMainLoopGLFW
    postRedisplay              = postRedisplayGLFW
    getWindowDimensions        = windowDimensionsFunc
    elapsedTime                = (const GLFW.getTime)
    sleep                      = sleepFunc

  windowDimensionsFunc : IORef GLFWState -> IO (Int, Int)
  windowDimensionsFunc stateRef = do
    s <- readIORef stateRef
    GLFW.getWindowDimensions (winHdl s)

  sleepFunc : IORef a -> Double -> IO ()
  sleepFunc _ sec = GLFW.sleep sec

  ||| Initial GLFW state.
  glfwStateInit : GLFWState
  glfwStateInit
    = MkGLFWState
        (MkModifiers Up Up Up)
        (0, 0)
        0
        True
        (pure ())
        (pure ())
        NullWindow

  export
  defaultBackendState : GLFWState
  defaultBackendState = glfwStateInit

  ||| Initialise -----------------------------------------------------------------
  |||  Initialise the GLFW backend.
  initializeGLFW : IORef GLFWState -> Bool-> IO ()
  initializeGLFW _ debug = do
    _           <- GLFW.initialize
    glfwVersion <- GLFW.getGlfwVersion

    when debug
      (putStr  $ "  glfwVersion        = " ++ show glfwVersion   ++ "\n")

  -- Exit -----------------------------------------------------------------------
  ||| Tell the GLFW backend to close the window and exit.
  exitGLFW : IORef GLFWState -> IO ()
  exitGLFW stateRef = do
    s <- readIORef stateRef
    GLFW.destroyWindow (winHdl s)

  -- Open Window ----------------------------------------------------------------
  ||| Open a new window.
  openWindowGLFW :  IORef GLFWState
                 -> Display
                 -> IO ()
  openWindowGLFW stateRef (InWindow title (sizeX, sizeY) pos) = do
    let disp = record 
      { displayOptions_width        = sizeX
      , displayOptions_height       = sizeY
      , displayOptions_displayMode  = GLFW.WindowMode } GLFW.defaultDisplayOptions

    win <- GLFW.createWindow title disp
    GLFW.makeContextCurrent win

    modifyIORef stateRef (\s => record {winHdl = win } s)
    
    GLEW.glewInit -- don't forget to initialize glew AFTER window creation because OpenGL context exists only then

    uncurry (GLFW.setWindowPosition win) pos
    
    -- Try to enable sync-to-vertical-refresh by setting the number 
    -- of buffer swaps per vertical refresh to 1.
    GLFW.setSwapInterval 1

  -- TODO: really no idea where (sizeX, sizeY) comes from at this point, they don't show up in the type
  openWindowGLFW stateRef (FullScreen) = do --(sizeX, sizeY)) = do
    let disp = record 
      { --displayOptions_width        = sizeX
      --, displayOptions_height       = sizeY
      displayOptions_displayMode  = GLFW.FullscreenMode } GLFW.defaultDisplayOptions

    win <- GLFW.createWindow "" disp
    GLFW.makeContextCurrent win
    
    modifyIORef stateRef (\s => record {winHdl = win } s)

    GLEW.glewInit -- don't forget to initialize glew AFTER window creation because OpenGL context exists only then

    -- Try to enable sync-to-vertical-refresh by setting the number 
    -- of buffer swaps per vertical refresh to 1.
    GLFW.setSwapInterval 1
    GLFW.showMouseCursor win True
    
  -- Dump State -----------------------------------------------------------------
  ||| Print out the internal GLFW state.
  dumpStateGLFW : IORef GLFWState -> IO ()
  dumpStateGLFW stateRef = do
    s           <- readIORef stateRef
    let win = (winHdl s) 

    (ww,wh)     <- GLFW.getWindowDimensions win

    r           <- GLFW.getWindowValue win NumRedBits
    g           <- GLFW.getWindowValue win NumGreenBits
    b           <- GLFW.getWindowValue win NumBlueBits
    a           <- GLFW.getWindowValue win NumAlphaBits
    let rgbaBD  = [r,g,b,a]

    depthBD     <- GLFW.getWindowValue win NumDepthBits

    ra          <- GLFW.getWindowValue win NumAccumRedBits
    ga          <- GLFW.getWindowValue win NumAccumGreenBits
    ba          <- GLFW.getWindowValue win NumAccumBlueBits
    aa          <- GLFW.getWindowValue win NumAccumAlphaBits
    let accumBD = [ra,ga,ba,aa]

    stencilBD   <- GLFW.getWindowValue win NumStencilBits

    auxBuffers  <- GLFW.getWindowValue win NumAuxBuffers

    fsaaSamples <- GLFW.getWindowValue win NumFsaaSamples

    {- TODO: seems to let idris compiler hang
    putStr  $ "* dumpGlfwState\n"
            ++ " windowWidth  = " ++ show ww          ++ "\n"
            ++ " windowHeight = " ++ show wh          ++ "\n"
            ++ " depth rgba   = " ++ show rgbaBD      ++ "\n"
            ++ " depth        = " ++ show depthBD     ++ "\n"
            ++ " accum        = " ++ show accumBD     ++ "\n"
            ++ " stencil      = " ++ show stencilBD   ++ "\n"
            ++ " aux Buffers  = " ++ show auxBuffers  ++ "\n"
            ++ " FSAA Samples = " ++ show fsaaSamples ++ "\n"
            ++ "\n"
    -}
    
    putStr  $ "* dumpGlfwState\n"


  -- Display Callback -----------------------------------------------------------
  callbackDisplay : Backend GLFWState
                  => IORef GLFWState 
                  -> List Callback
                  -> IO ()
  callbackDisplay stateRef callbacks = do
      -- clear the display
      -- GL.clear [GL.ColorBuffer, GL.DepthBuffer]
      -- GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

      GL.glClear GL_COLOR_BUFFER_BIT
      GL.glClear GL_DEPTH_BUFFER_BIT
      GL.glClearColor 0 0 0 1

      -- get the display callbacks from the chain
      -- let funs  = [f stateRef | (Display f) <- callbacks]
      -- sequence_ funs
      runDisplayClbks callbacks
    where
      runDisplayClbks : Backend GLFWState => List Callback -> IO ()
      runDisplayClbks [] = pure ()
      runDisplayClbks (Display f :: cs) = do
        f stateRef
        runDisplayClbks cs
      runDisplayClbks _ = pure ()

  ||| Callback for when GLFW needs us to redraw the contents of the window.
  installDisplayCallbackGLFW : Backend GLFWState
                            => IORef GLFWState 
                            -> List Callback
                            -> IO ()
  installDisplayCallbackGLFW stateRef callbacks
    = modifyIORef stateRef $ \s => record { display = callbackDisplay stateRef callbacks } s

  -- Close Callback -------------------------------------------------------------
  ||| Callback for when the user closes the window.
  --   We can do some cleanup here.
  installWindowCloseCallbackGLFW  : IORef GLFWState -> IO ()
  installWindowCloseCallbackGLFW stateRef = do
      s <- readIORef stateRef
      GLFW.setWindowCloseCallback (winHdl s) !windowCloseCallbackPtr
    where
      -- IO callbacks not (yet) suppored by Idris, need to use unsafePerformIO
      windowCloseCallback : WindowCloseCallback
      windowCloseCallback _ = ()
    
      -- unfortunately we need to pass the callback as a pointer...
      windowCloseCallbackPtr : IO Ptr
      windowCloseCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr WindowCloseCallback -> IO Ptr) (MkCFnPtr windowCloseCallback)

  -- Reshape --------------------------------------------------------------------
  ||| Callback for when the user reshapes the window.
  installReshapeCallbackGLFW :  Backend GLFWState 
                             => IORef GLFWState 
                             -> List Callback 
                             -> IO ()
  installReshapeCallbackGLFW stateRef callbacks = do
      s <- readIORef stateRef
      GLFW.setWindowSizeCallback (winHdl s) !windowSizeCallbackPtr -- (callbackReshape stateRef callbacks)
    where
      callbackReshape :  List Callback
                      -> Int 
                      -> Int
                      -> IO ()
      callbackReshape [] _ _ = pure ()
      callbackReshape (Reshape f :: cs) sizeX sizeY  = f stateRef (sizeX, sizeY)
      callbackReshape _ _ _ = pure ()
      {-
        = sequence_
        $ map (\f => f (sizeX, sizeY))
          [f glfwState | Reshape f <- callbacks] -- TODO: this looks like its not gonna work in idris
        -}

      windowSizeCallback : WindowSizeCallback
      windowSizeCallback win' w h = unsafePerformIO $ do 
        callbackReshape callbacks w h

      windowSizeCallbackPtr : IO Ptr
      windowSizeCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr WindowSizeCallback -> IO Ptr) (MkCFnPtr windowSizeCallback)


  -- KeyMouse -----------------------------------------------------------------------
  setModifiers :  IORef GLFWState
              -> GLFW.GLFWKey 
              -> Bool
              -> IO (Bool, GLFWState)
  setModifiers stateRef key pressed = do
    glfwState <- readIORef stateRef
    let mods  = modifiers glfwState
    let mods' = case key of
            GLFW.KeyLeftShift => record {shift = if pressed then Down else Up} mods
            GLFW.KeyLeftCtrl  => record {ctrl  = if pressed then Down else Up} mods
            GLFW.KeyLeftAlt   => record {alt   = if pressed then Down else Up} mods
            _                 => mods

    if (mods' /= mods)
      then do
        let glfwState' = record { modifiers = mods' } glfwState
        writeIORef stateRef glfwState'
        pure (True, glfwState')
      else pure (False, glfwState)

  runKeyMouseClbk :  Backend GLFWState
                  => IORef GLFWState 
                  -> Key
                  -> KeyState
                  -> Modifiers
                  -> (Int,Int)
                  -> List Callback 
                  -> IO ()
  runKeyMouseClbk _ _ _ _ _ [] = pure ()
  runKeyMouseClbk stateRef key keystate mods pos (KeyMouse f :: cs) = do
    f stateRef key keystate mods pos
    runKeyMouseClbk stateRef key keystate mods pos cs
  runKeyMouseClbk _ _ _ _ _ _ = pure ()

  ||| Callbacks for when the user presses a key or moves / clicks the mouse.
  |||   This is a bit verbose because we have to do impedence matching between
  |||   GLFW's event system, and the one use by Gloss which was originally
  |||   based on GLUT. The main problem is that GLUT only provides a single callback
  |||   slot for character keys, arrow keys, mouse buttons and mouse wheel movement, 
  |||   while GLFW provides a single slot for each.
  |||
  installKeyMouseCallbackGLFW :  IORef GLFWState 
                              -> List Callback 
                              -> IO ()
  installKeyMouseCallbackGLFW stateRef callbacks = do
      s <- readIORef stateRef
      GLFW.setKeyCallback         (winHdl s) !keyCallbackPtr
      GLFW.setCharCallback        (winHdl s) !charCallbackPtr
      GLFW.setMouseButtonCallback (winHdl s) !mouseButtonCallbackPtr
      GLFW.setMouseWheelCallback  (winHdl s) !mouseWheelCallbackPtr
    where
      -- GLFW calls this on a non-character keyboard action.
      callbackKeyboard : GLFW.GLFWKey 
                      -> Bool
                      -> IO ()
      callbackKeyboard key keystate = do
          (modsSet, MkGLFWState mods pos _ _ _ _ _) <- setModifiers stateRef key keystate     
          let key'      = fromGLFW key
          let keystate' = if keystate then Down else Up

          -- Call the Gloss KeyMouse actions with the new state.
          -- original code uses unless, idris doesn't have unless but it is just the reverse of when
          -- => when not
          when (not (modsSet || isCharKey key' && keystate))
            (runKeyMouseClbk stateRef key' keystate' mods pos callbacks)

            --$ sequence_ 
            --$ map  (\f => f key' keystate' mods pos)
            --      [f stateRef | KeyMouse f <- callbacks] -- TODO: this looks like its not gonna work in idris
                  
        where
          isCharKey : Key -> Bool
          isCharKey (CharKey _) = True
          isCharKey _           = False

      -- GLFW calls this on a when the user presses or releases a character key.
      callbackChar : Char 
                  -> Bool 
                  -> IO ()
      callbackChar char keystate = do
          (MkGLFWState mods pos _ _ _ _ _) <- readIORef stateRef
          let key'      = charToSpecial char
          -- Only key presses of characters are passed to this callback,
          -- character key releases are caught by the 'keyCallback'. This is an
          -- intentional feature of GLFW. What this means that a key press of
          -- the '>' char  (on a US Intl keyboard) is captured by this callback,
          -- but a release is captured as a '.' with the shift-modifier in the
          -- keyCallback.
          let keystate' = if keystate then Down else Up

          -- Call all the Gloss KeyMouse actions with the new state.
          {-
          sequence_ 
            $ map  (\f => f key' keystate' mods pos) 
                  [f stateRef | KeyMouse f <- callbacks] -- TODO: this looks like its not gonna work in idris
                  -}
          runKeyMouseClbk stateRef key' keystate' mods pos callbacks

      -- GLFW calls on this when the user clicks or releases a mouse button.
      callbackMouseButton :  GLFW.GLFWMouseButton
                          -> Bool
                          -> IO ()
      callbackMouseButton key keystate = do
        (MkGLFWState mods pos _ _ _ _ _) <- readIORef stateRef
        let key'      = fromGLFW key
        let keystate' = if keystate then Down else Up

        -- Call all the Gloss KeyMouse actions with the new state.
        runKeyMouseClbk stateRef key' keystate' mods pos callbacks
        {-
        sequence_ 
          $ map  (\f => f key' keystate' mods pos)
                [f stateRef | KeyMouse f <- callbacks] -- TODO: this looks like its not gonna work in idris
                -}

      setMouseWheel :  Int
                    -> IO (Key, KeyState)
      setMouseWheel w = do
        glfwState <- readIORef stateRef
        writeIORef stateRef $ record { mouseWheelPos = w } glfwState
        case compare w (mouseWheelPos glfwState) of
              LT => pure (MouseButton WheelDown , Down)
              GT => pure (MouseButton WheelUp   , Down)
              EQ => pure (SpecialKey  KeyUnknown, Up  )

      -- GLFW calls on this when the user moves the mouse wheel.
      callbackMouseWheel :  Int
                         -> IO ()
      callbackMouseWheel w = do
        (key, keystate)  <- setMouseWheel w
        (MkGLFWState mods pos _ _ _ _ _) <- readIORef stateRef

        -- Call all the Gloss KeyMouse actions with the new state.
        runKeyMouseClbk stateRef key keystate mods pos callbacks
        {-
        sequence_ 
          $ map  (\f => f key keystate mods pos)
                [f stateRef | KeyMouse f <- callbacks]
                -}

      keyCallback : KeyCallback
      keyCallback win' key scancode action mods = unsafePerformIO $ do
          let mglfwKey = GLFW.glfwKeyFromInt key
          let mglfwAct = GLFW.glfwKeyActionFromInt action
          keyCallbackAux mglfwKey mglfwAct
        where
          keyCallbackAux :  Maybe GLFWKey
                         -> Maybe GLFWKeyAction
                         -> IO ()
          keyCallbackAux (Just glfwKey) (Just Press)   = callbackKeyboard glfwKey True
          keyCallbackAux (Just glfwKey) (Just Release) = callbackKeyboard glfwKey False
          keyCallbackAux _ _ = pure () -- ignore invalid key (which won't happen, but need to catch it this way)

      keyCallbackPtr : IO Ptr
      keyCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr KeyCallback -> IO Ptr) (MkCFnPtr keyCallback)

      charCallback : CharCallback
      charCallback win' c = unsafePerformIO $ do 
        callbackChar c True -- TODO: is true correct here?
        pure ()

      charCallbackPtr : IO Ptr
      charCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr CharCallback -> IO Ptr) (MkCFnPtr charCallback)

      mouseButtonCallback : MouseButtonCallback
      mouseButtonCallback win' button action mods = unsafePerformIO $ do 
          let mglfwMb = GLFW.glfwMouseButtonFromInt button
          let mglfwAct = GLFW.glfwKeyActionFromInt action

          mouseButtonCallbackAux mglfwMb mglfwAct
        where
          mouseButtonCallbackAux :  Maybe GLFWMouseButton
                                 -> Maybe GLFWKeyAction
                                 -> IO ()
          mouseButtonCallbackAux (Just glfwMb) (Just Press)   = callbackMouseButton glfwMb True
          mouseButtonCallbackAux (Just glfwMb) (Just Release) = callbackMouseButton glfwMb False
          mouseButtonCallbackAux _ _ = pure () -- ignore invalid key (which won't happen, but need to catch it this way)


      mouseButtonCallbackPtr : IO Ptr
      mouseButtonCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr MouseButtonCallback -> IO Ptr) (MkCFnPtr mouseButtonCallback)

      mouseWheelCallback : MouseWheelCallback
      mouseWheelCallback win' xoff yoff = unsafePerformIO $ do 
        callbackMouseWheel (cast yoff)
        pure ()

      mouseWheelCallbackPtr : IO Ptr
      mouseWheelCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr MouseButtonCallback -> IO Ptr) (MkCFnPtr mouseButtonCallback)

  -- Motion Callback ------------------------------------------------------------
  ||| Callback for when the user moves the mouse.
  installMotionCallbackGLFW :  IORef GLFWState 
                            -> List Callback
                            -> IO ()
  installMotionCallbackGLFW stateRef callbacks = do
      s <- readIORef stateRef
      GLFW.setMousePositionCallback (winHdl s) !mousePositionCallbackPtr -- (callbackMotion stateRef callbacks)
    where
      setMousePos :  Int 
                  -> Int
                  -> IO (Int, Int)
      setMousePos x y = do
        putStrLn "setMousePos 1"
        let pos = (x,y)
        putStrLn "setMousePos 2"
        -- TODO: crash occurs when evaluating this statement
        modifyIORef stateRef $ \s => record { mousePosition = pos } s
        putStrLn "setMousePos 3"
        pure pos

      callbackMotion :  Int 
                    -> Int
                    -> IO ()
      callbackMotion x y = do
          putStrLn "callbackMotion"
          pos <- setMousePos x y

          -- Call all the Gloss Motion actions with the new state.
          putStrLn "runMotionClbks before"
          runMotionClbks pos callbacks
          {-
          sequence_ 
            $ map  (\f => f pos)
                  [f stateRef | Motion f <- callbacks] -- TODO: this looks like its not gonna work in idris
                  -}
        where
          runMotionClbks : (Int, Int) 
                        -> List Callback
                        -> IO ()
          runMotionClbks _ [] = pure ()
          runMotionClbks pos (Motion f :: cs) = do
            putStrLn "runMotionClbks Motion"
            f stateRef pos
            runMotionClbks pos cs
          runMotionClbks _ _ = pure ()


      mousePositionCallback : MousePositionCallback
      mousePositionCallback win' xpos ypos = unsafePerformIO $ do 
        putStrLn "mousePositionCallback"
        callbackMotion (cast xpos) (cast ypos)
        pure ()

      mousePositionCallbackPtr : IO Ptr
      mousePositionCallbackPtr = foreign FFI_C "%wrapper" (CFnPtr MousePositionCallback -> IO Ptr) (MkCFnPtr mousePositionCallback)

  -- Idle Callback --------------------------------------------------------------
  callbackIdle :  IORef GLFWState 
               -> List Callback 
               -> IO ()
  callbackIdle _ [] = pure ()
  callbackIdle stateRef (Idle f :: cs) = do
    f stateRef
    callbackIdle stateRef cs
  
    -- = sequence_ $ [f stateRef | Idle f <- callbacks] -- TODO: this looks like its not gonna work in idris

  ||| Callback for when GLFW has finished its jobs and it's time for us to do
  |||   something for our application.
  installIdleCallbackGLFW :  IORef GLFWState 
                          -> List Callback 
                          -> IO ()
  installIdleCallbackGLFW stateRef callbacks 
    = modifyIORef stateRef $ \s => record { idle = callbackIdle stateRef callbacks } s

  -- Main Loop ------------------------------------------------------------------
  -- TODO: need to implement some kind of exception handling here!
  runMainLoopGLFW : IORef GLFWState -> IO ()
  runMainLoopGLFW stateRef = go -- = X.catch go exit
    where
      go : IO ()
      go = do
        putStrLn "1"
        let win = winHdl !(readIORef stateRef)
        windowIsOpen <- GLFW.windowIsOpen win
        putStrLn "2"
        when windowIsOpen 
          $ do  
            putStrLn "3"
            GLFW.pollEvents
            putStrLn "4"
            s <- readIORef stateRef

            let ds = dirtyScreen s
            putStrLn $ "dirtyScreen = " ++ show ds
            dirty <- map dirtyScreen $ readIORef stateRef
            putStrLn "5"

            when dirty
              $ do
                putStrLn "5a"
                display s
                putStrLn "5b"
                GLFW.swapBuffers win
                putStrLn "5c"

            putStrLn "6"
            modifyIORef stateRef $ \s => record { dirtyScreen = False } s
            putStrLn "7"

            (readIORef stateRef) >>= (\s => idle s)
            putStrLn "8"
            GLFW.sleep 0.001
            putStrLn "9"
            runMainLoopGLFW stateRef


  -- Redisplay ------------------------------------------------------------------
  postRedisplayGLFW : IORef GLFWState -> IO ()
  postRedisplayGLFW stateRef = do
    putStrLn "postRedisplayGLFW"
    modifyIORef stateRef $ \s => record { dirtyScreen = True } s