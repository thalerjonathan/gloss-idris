module Graphics.Gloss.Data.Display

||| Describes how Gloss should display its output.
public export
data Display =
  ||| Display in a window with the given name, size and position.
  InWindow   String (Int, Int) (Int, Int) |
  ||| Display full screen.
  FullScreen
  -- TODO: port to idris 
  -- deriving (Eq, Read, Show)
