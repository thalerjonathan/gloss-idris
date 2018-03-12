||| Display mode is for drawing a static picture.
module Graphics.Gloss.Interface.Pure.Display

import public Graphics.Gloss.Data.Color
import public Graphics.Gloss.Data.Controller
import public Graphics.Gloss.Data.Display
import public Graphics.Gloss.Data.Picture
import        Graphics.Gloss.Internals.Interface.Display
import        Graphics.Gloss.Internals.Interface.Backend

||| Open a new window and display the given picture.
||| @ dis       Display mode.
||| @ backColor Background color.
||| @ picture   The picture to draw.
export
display  : (dis : Display)
        -> (backColor : Color)
        -> (picture : Picture)
        -> IO ()
display dis backColor picture
  = displayWithBackend
      defaultBackendState
      dis
      backColor
      (pure picture)
      (const (pure ()))