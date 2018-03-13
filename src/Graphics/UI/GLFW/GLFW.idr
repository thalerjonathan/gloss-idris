module Graphics.UI.GLFW.GLFW

import Graphics.Rendering.Gl.Buffers

import public Graphics.UI.GLFW.GlfwConfig

%include C "GLFW/glfw3.h"

export
Window : Type
Window = Ptr

public export
data DisplayMode
  = WindowMode
  | FullscreenMode

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
data GLFWKey
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

public export
data GLFWMouseButton
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
    WindowMode      -- displayOptions_displayMode             = Window
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
KeyCallback = GLFWKey -> Bool -> IO ()

public export
MouseButtonCallback : Type
MouseButtonCallback = GLFWMouseButton -> Bool -> IO ()

public export
MousePositionCallback : Type
MousePositionCallback = Int -> Int -> IO ()

public export
MouseWheelCallback : Type
MouseWheelCallback  = Int -> IO ()

export
initialize : IO Bool
initialize = do
  ret <- foreign FFI_C "glfwInit" (IO Int)
  pure $ if ret == 1 then True else False

export
getGlfwVersion : IO String
getGlfwVersion = do
  majPtr <- intBuffer 1
  minPtr <- intBuffer 1
  revPtr <- intBuffer 1

  foreign FFI_C "glfwGetVersion" (Ptr -> Ptr -> Ptr -> IO ()) majPtr minPtr revPtr

  maj <- readInt majPtr 0 
  min <- readInt majPtr 0
  rev <- readInt majPtr 0

  free majPtr
  free minPtr
  free revPtr

  pure $ show maj ++ "." ++ show min ++ "." ++ show rev

export
closeWindow : IO ()
closeWindow
  = foreign FFI_C "glfwDestroyWindow" (IO ())

export
openWindow : DisplayOptions -> String -> IO Window
openWindow disp title = do
{-
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
-}

  let width  = displayOptions_width disp
  let height = displayOptions_height disp

  let monitor = 
    case displayOptions_displayMode disp of
      WindowMode     => null
      FullscreenMode => null -- TODO: set to valid monitor config

  win <- foreign FFI_C "glfwCreateWindow" (Int -> Int -> String -> Ptr -> Ptr) width height title monitor null

  pure win

export
setWindowPosition : Window -> Int -> Int -> IO ()
setWindowPosition win x y
  = foreign FFI_C "glfwSetWindowPos" (Ptr -> Int -> Int -> IO ()) win w h

export
setWindowTitle : Window -> String -> IO ()
setWindowTitle win title
  = foreign FFI_C "glfwSetWindowTitle" (Ptr -> String -> IO ()) win title

export
setWindowBufferSwapInterval : Int -> IO ()
setWindowBufferSwapInterval interval
  = foreign FFI_C "glfwSwapInterval" (Int -> IO ()) interval
  
export
enableMouseCursor : IO ()
enableMouseCursor = ?enableMouseCursor

export
getWindowDimensions : Window -> IO (Int, Int)
getWindowDimensions win = do
  widthPtr <- intBuffer 1
  heightPtr <- intBuffer 1

  foreign FFI_C "glfwGetWindowSize" (Ptr -> Ptr -> Ptr IO ()) win widthPtr heightPtr

  width <- readInt widthPtr 0 
  height <- readInt heightPtr 0 

  free widthPtr
  free heightPtr
  
  pure (width, height)

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
pollEvents = foreign FFI_C "glfwPollEvents" (IO ())

export
swapBuffers : Window -> IO ()
swapBuffers win
  = foreign FFI_C "glfwSwapBuffers" (Ptr -> IO ()) win

export
sleep : Double -> IO ()
sleep = ?sleep

export
getTime : IO Double
getTime = ?getTime