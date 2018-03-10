||| Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.Pure.Display

import public Graphics.Gloss.Data.Display
import public Graphics.Gloss.Data.Picture
import public Graphics.Gloss.Data.Color
import        Graphics.Gloss.Internals.Interface.Display
import        Graphics.Gloss.Internals.Interface.Backend

||| Open a new window and display the given picture.
export
||| @ dis       Display mode.
||| @ backColor Background color.
||| @ picture   The picture to draw.
display :  (dis : Display)
        -> (backColor : Color)
        -> (picture : Picture)
        -> IO ()
display dis backColor picture
  = displayWithBackend
          defaultBackendState
          dis
          backColor
          (return picture)
          (const (pure ()))