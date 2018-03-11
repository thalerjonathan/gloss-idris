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