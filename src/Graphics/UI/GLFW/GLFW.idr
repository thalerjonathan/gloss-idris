module Graphics.UI.GLFW.GLFW

public export
data DisplayMode
  = Window
  | Fullscreen

public export
data OpenGLProfile
  = DefaultProfile
  | CoreProfile
  | CompatibilityProfile

public export
record DisplayOptions where
  constructor MkDisplayOptions
  displayOptions_width                   : Int
  displayOptions_height                  : Int
  displayOptions_numRedBits              : Int
  displayOptions_numGreenBits            : Int
  displayOptions_numBlueBits             : Int
  displayOptions_numAlphaBits            : Int
  displayOptions_numDepthBits            : Int
  displayOptions_numStencilBits          : Int
  displayOptions_displayMode             : DisplayMode
  displayOptions_refreshRate             : Maybe Int
  displayOptions_accumNumRedBits         : Maybe Int
  displayOptions_accumNumGreenBits       : Maybe Int
  displayOptions_accumNumBlueBits        : Maybe Int
  displayOptions_accumNumAlphaBits       : Maybe Int
  displayOptions_numAuxiliaryBuffers     : Maybe Int
  displayOptions_numFsaaSamples          : Maybe Int
  displayOptions_windowIsResizable       : Bool
  displayOptions_stereoRendering         : Bool
  displayOptions_openGLVersion           : (Int, Int)
  displayOptions_openGLForwardCompatible : Bool
  displayOptions_openGLDebugContext      : Bool
  displayOptions_openGLProfile           : OpenGLProfile

public export
data WindowValue
  = NumRedBits
  | NumGreenBits
  | NumBlueBits
  | NumAlphaBits
  | NumDepthBits
  | NumStencilBits
  | NumAccumRedBits
  | NumAccumGreenBits
  | NumAccumBlueBits
  | NumAccumAlphaBits
  | NumAuxBuffers
  | NumFsaaSamples

public export
data Key
  = CharKey Char
  | KeyUnknown
  | KeySpace
  | KeySpecial
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
  | KeyLeftShift
  | KeyRightShift
  | KeyLeftCtrl
  | KeyRightCtrl
  | KeyLeftAlt
  | KeyRightAlt
  | KeyTab
  | KeyEnter
  | KeyBackspace
  | KeyInsert
  | KeyDel
  | KeyPageup
  | KeyPagedown
  | KeyHome
  | KeyEnd
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

data MouseButton
  = MouseButton0 | MouseButton1 | MouseButton2 | MouseButton3
  | MouseButton4 | MouseButton5 | MouseButton6 | MouseButton7

export
defaultDisplayOptions : DisplayOptions
defaultDisplayOptions =
  MkDisplayOptions
    0               -- isplayOptions_width                    = 0
    0               -- displayOptions_height                  = 0
    0               -- displayOptions_numRedBits              = 0
    0               -- displayOptions_numGreenBits            = 0
    0               -- displayOptions_numBlueBits             = 0
    0               -- displayOptions_numAlphaBits            = 0
    0               -- displayOptions_numDepthBits            = 0
    0               -- displayOptions_numStencilBits          = 0
    Window          -- displayOptions_displayMode             = Window
    Nothing         -- displayOptions_refreshRate             = Nothing
    Nothing         -- displayOptions_accumNumRedBits         = Nothing
    Nothing         -- displayOptions_accumNumGreenBits       = Nothing
    Nothing         -- displayOptions_accumNumBlueBits        = Nothing
    Nothing         -- displayOptions_accumNumAlphaBits       = Nothing
    Nothing         -- displayOptions_numAuxiliaryBuffers     = Nothing
    Nothing         -- displayOptions_numFsaaSamples          = Nothing
    True            -- displayOptions_windowIsResizable       = True
    False           -- displayOptions_stereoRendering         = False
    (1, 1)          -- displayOptions_openGLVersion           = (1,1)
    False           -- displayOptions_openGLForwardCompatible = False
    False           -- displayOptions_openGLDebugContext      = False
    DefaultProfile  -- displayOptions_openGLProfile           = DefaultProfile

public export
WindowCloseCallback : Type
WindowCloseCallback = IO Bool

public export
WindowSizeCallback : Type 
WindowSizeCallback = Int -> Int -> IO ()

public export
CharCallback : Type 
CharCallback = Char -> Bool -> IO ()

public export
KeyCallback : Type
KeyCallback = Key -> Bool -> IO ()

public export
MouseButtonCallback : Type
MouseButtonCallback = MouseButton -> Bool -> IO ()

public export
MousePositionCallback : Type
MousePositionCallback = Int -> Int -> IO ()

public export
MouseWheelCallback : Type
MouseWheelCallback  = Int -> IO ()

export
initialize : IO ()
initialize = ?initialize

export
getGlfwVersion : IO String
getGlfwVersion = ?getGlfwVersion

export
closeWindow : IO ()
closeWindow = ?closeWindow

export
openWindow : DisplayOptions -> IO Bool
openWindow disp = ?openWindow

export
setWindowPosition : Int -> Int -> IO ()
setWindowPosition w h = ?setWindowPosition

export
setWindowTitle : String -> IO ()
setWindowTitle title = ?setWindowTitle

export
setWindowBufferSwapInterval : Int -> IO ()
setWindowBufferSwapInterval interval = ?setWindowBufferSwapInterval

export
enableMouseCursor : IO ()
enableMouseCursor = ?enableMouseCursor

export
getWindowDimensions : IO (Int, Int)
getWindowDimensions = ?getWindowDimensions

export
getWindowValue : WindowValue -> IO Int
getWindowValue wv = ?getWindowValue

export
setWindowCloseCallback : WindowCloseCallback -> IO ()
setWindowCloseCallback clbk = ?setWindowCloseCallback

export
setWindowSizeCallback : WindowSizeCallback -> IO ()
setWindowSizeCallback clbk = ?setWindowSizeCallback

export
setKeyCallback : KeyCallback -> IO ()
setKeyCallback clbk = ?setKeyCallback

export
setCharCallback : CharCallback -> IO ()
setCharCallback clbk = ?setCharCallback

export
setMouseButtonCallback : MouseButtonCallback -> IO ()
setMouseButtonCallback clbk = ?setMouseButtonCallback

export
setMouseWheelCallback : MouseWheelCallback -> IO ()
setMouseWheelCallback clbk = ?setMouseWheelCallback

export
setMousePositionCallback : MousePositionCallback -> IO ()
setMousePositionCallback clbk = ?setMousePositionCallback

export
windowIsOpen : IO Bool
windowIsOpen = ?windowIsOpen

export
pollEvents : IO ()
pollEvents = ?pollEvents

export
swapBuffers : IO ()
swapBuffers = ?swapBuffers

export
sleep : Double -> IO ()
sleep = ?sleep

export
getTime : IO Double
getTime = ?getTime