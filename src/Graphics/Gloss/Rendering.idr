
module Graphics.Gloss.Rendering 

{-
( |||* Picture data type
          Picture (..)
        , Point, Vector, Path

          |||* Colors
        , Color
        , makeColor
        , makeColorI
        , makeRawColor
        , makeRawColorI
        , rgbaOfColor
        , clampColor

          |||* Bitmaps
        , BitmapData
        , BitmapFormat(..), PixelFormat(..), RowOrder(..)
        , bitmapOfForeignPtr
        , bitmapOfByteString
        , bitmapOfBMP
        , loadBMP

          |||* Rendering
        , displayPicture
        , renderPicture
        , withModelview
        , withClearBuffer
        , RS.initState
        , RS.State)
-}

import public Graphics.Gloss.Internals.Data.Color
import public Graphics.Gloss.Internals.Data.Picture

import        Graphics.Gloss.Internals.Rendering.Common
import public Graphics.Gloss.Internals.Rendering.Picture
import        Graphics.Gloss.Internals.Rendering.State as RS
import public Graphics.Gloss.Internals.Rendering.Bitmap

||| Set up the OpenGL context, clear the buffer, and render the given picture
|||  into it. 
|||
|||  This is the same as `renderPicture` composed with `withModelview`
|||  and `withClearBuffer`. If you want to manage your own OpenGL context then
|||  you can just call `renderPicture`. 
|||
|||  Using this function assumes that you've already opened a window
|||  and set that to the active context. If you don't want to do your own window
|||  management then use the @gloss@ package instead.
|||
||| @ windowSize Window width and height.
||| @ colorClear Color to clear the window with.
||| @ state      Current rendering state.
||| @ scale       View port scale, which controls the level of detail. Use 1.0 to start with.
||| @ picture     Picture to draw.
export
displayPicture :  (windowSize : (Int, Int)) 
               -> (colorClear : Color)    
               -> (state : RS.State)     
               -> (scale : Double) 
               -> (picture : Picture)
               -> IO ()
displayPicture windowSize colorClear state scale picture
  = withModelview      windowSize
  $ withClearBuffer    colorClear
  $ renderPicture  state scale picture
