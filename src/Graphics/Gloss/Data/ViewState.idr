module Graphics.Gloss.Data.ViewState

import Data.SortedMap

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Rendering 

import Graphics.Gloss.Internals.Interface.Event
import Graphics.Gloss.Internals.Interface.Backend

||| The commands suported by the view controller.
public export
data Command = 
        CRestore        |
        CTranslate      |
        CRotate         |
        CScale          |

        ||| bump zoom
        CBumpZoomOut    |
        CBumpZoomIn     |

        ||| bump translate
        CBumpLeft       |
        CBumpRight      |
        CBumpUp         |
        CBumpDown       |

        ||| bump rotate
        CBumpClockwise  |
        CBumpCClockwise
        -- deriving (Show, Eq, Ord)

Eq Command where
  (==) CRestore CRestore                = True
  (==) CTranslate CTranslate            = True
  (==) CRotate CRotate                  = True
  (==) CScale CScale                    = True
  (==) CBumpZoomOut CBumpZoomOut        = True
  (==) CBumpZoomIn CBumpZoomIn          = True
  (==) CBumpLeft CBumpLeft              = True
  (==) CBumpRight CBumpRight            = True
  (==) CBumpUp CBumpUp                  = True
  (==) CBumpDown CBumpDown              = True
  (==) CBumpClockwise CBumpClockwise    = True
  (==) CBumpCClockwise CBumpCClockwise  = True
  (==) _ _ = False
  
toEnum : Command -> Nat
toEnum CRestore        = 0
toEnum CTranslate      = 1
toEnum CRotate         = 2
toEnum CScale          = 3
toEnum CBumpZoomOut    = 4
toEnum CBumpZoomIn     = 5
toEnum CBumpLeft       = 6
toEnum CBumpRight      = 7
toEnum CBumpUp         = 8
toEnum CBumpDown       = 9
toEnum CBumpClockwise  = 10
toEnum CBumpCClockwise = 11

Ord Command where
  compare c1 c2 = compare (toEnum c1) (toEnum c2)

public export
CommandConfig : Type
CommandConfig = List (Command, List (Key, Maybe Modifiers))

||| The default commands.  Left click pans, wheel zooms, right click
|||   rotates, "r" key resets.
export
defaultCommandConfig : CommandConfig
defaultCommandConfig 
 =      [ (CRestore,
                [ (CharKey 'r',                    Nothing) ])

        , (CTranslate,
                [ ( MouseButton LeftButton
                  , Just (MkModifiers Up Up Up))
                ])

        , (CScale,
                [ ( MouseButton LeftButton
                  , Just (MkModifiers Up Down Up))

                , ( MouseButton RightButton
                  , Just (MkModifiers Up Up Up)) 
                ])

        , (CRotate,
                [ ( MouseButton LeftButton
                  , Just (MkModifiers Up Up Down))

                , ( MouseButton RightButton
                  , Just (MkModifiers Up Down Up))
                ])

        -- bump zoom
        , (CBumpZoomOut,
                [ (MouseButton WheelDown,       Nothing)
                , (SpecialKey  KeyPageDown,     Nothing) ])

        , (CBumpZoomIn,
                [ (MouseButton WheelUp,         Nothing)
                , (SpecialKey  KeyPageUp,       Nothing)] )

        -- bump translate
        , (CBumpLeft,
                [ (SpecialKey  KeyLeft,         Nothing) ])

        , (CBumpRight,
                [ (SpecialKey  KeyRight,        Nothing) ])

        , (CBumpUp,
                [ (SpecialKey  KeyUp,           Nothing) ])

        , (CBumpDown,
                [ (SpecialKey  KeyDown,         Nothing) ])

        -- bump rotate
        , (CBumpClockwise,
                [ (SpecialKey  KeyHome,         Nothing) ])

        , (CBumpCClockwise,
                [ (SpecialKey  KeyEnd,          Nothing) ])

        ]

||| Check if the provided key combination is some gloss viewport command.
isCommand2 : Command -> Key -> Modifiers -> (Key, Maybe Modifiers) -> Bool
isCommand2 _ key keyMods (keyC, mModsC) =
  if keyC == key
    then maybe True (\modsC => modsC == keyMods) mModsC
    else False

||| Check if the provided key combination is some gloss viewport command.
isCommand : SortedMap Command (List (Key, Maybe Modifiers))
          -> Command -> Key -> Modifiers -> Bool
isCommand commands c key keyMods =
    maybe False (\csMatch => or $ map (isCommand2 c key keyMods) csMatch) mayCsMatch
  where
    mayCsMatch = SortedMap.lookup c commands

    or : List Bool -> Bool
    or [] = False
    or (x :: xs) = let v = or xs 
                   in  v || x

-- ViewControl State -----------------------------------------------------------
||| State for controlling the viewport.
|||     These are used by the viewport control component.
public export
record ViewState where
  constructor MkViewState
  ||| The command list for the viewport controller.
  |||      These can be safely overwridden at any time by deleting
  |||      or adding entries to the list.
  |||      Entries at the front of the list take precedence.
  viewStateCommands      : SortedMap Command (List (Key, Maybe Modifiers))

  ||| How much to scale the world by for each step of the mouse wheel.
  viewStateScaleStep     : Double

  ||| How many degrees to rotate the world by for each pixel of x motion.
  viewStateRotateFactor  : Double

  ||| Ratio to scale the world by for each pixel of y motion.
  viewStateScaleFactor   : Double

  ||| During viewport translation,
  |||     where the mouse was clicked on the window to start the translate.
  viewStateTranslateMark : Maybe (Double, Double)

  ||| During viewport rotation,  
  |||     where the mouse was clicked on the window to starte the rotate.
  viewStateRotateMark    : Maybe (Double, Double)

  ||| During viewport scale,
  |||      where the mouse was clicked on the window to start the scale.
  viewStateScaleMark     : Maybe (Double, Double)

  ||| The current viewport.
  viewStateViewPort      : ViewPort

||| Apply a translation to the `ViewState`.
motionTranslate 
         : Maybe (Double, Double)         -- Location of first mark.
        -> (Double, Double)               -- Current position.
        -> ViewState -> Maybe ViewState
motionTranslate Nothing _ _ = Nothing
motionTranslate (Just (markX, markY)) (posX, posY) viewState
    = Just $ record
            { viewStateViewPort      = port'
            , viewStateTranslateMark = Just (posX, posY) } viewState
  where
    port : ViewPort
    port = viewStateViewPort viewState

    trans : (Double, Double)
    trans   = viewPortTranslate port

    scale : Double
    scale   = viewPortScale port

    r : Double
    r = viewPortRotate port

    dX : Double
    dX = markX - posX

    dY : Double
    dY = markY - posY

    offset : (Double, Double)
    offset = (dX / scale, dY / scale)

    o : Vector
    o = rotateV (degToRad r) offset
    port'   = record { viewPortTranslate = trans - o } port

||| Apply a rotation to the `ViewState`.
motionRotate 
         : Maybe (Double, Double)         -- Location of first mark.
        -> (Double, Double)               -- Current position.
        -> ViewState -> Maybe ViewState
motionRotate Nothing _ _ = Nothing
motionRotate (Just (markX, _markY)) (posX, posY) viewState
  = Just $ record
        { viewStateViewPort = port'
        , viewStateRotateMark   = Just (posX, posY) } viewState
  where
    port : ViewPort
    port = viewStateViewPort viewState

    rotate : Double
    rotate = viewPortRotate port

    rotateFactor : Double
    rotateFactor = viewStateRotateFactor viewState

    port' = record { viewPortRotate = rotate - rotateFactor * (posX - markX) } port

||| Apply a scale to the `ViewState`.
motionScale
         : Maybe (Double, Double)         -- Location of first mark.
        -> (Double, Double)               -- Current position.
        -> ViewState -> Maybe ViewState
motionScale Nothing _ _ = Nothing
motionScale (Just (_markX, markY)) (posX, posY) viewState
    = Just $ record
        { viewStateViewPort  = port'
        , viewStateScaleMark = Just (posX, posY) } viewState
  where
    port : ViewPort
    port = viewStateViewPort viewState

    scale : Double
    scale = viewPortScale port

    scaleFactor : Double
    scaleFactor = viewStateScaleFactor viewState

    -- Limit the amount of downward scaling so it maxes
      -- out at 1 percent of the original. There's not much
      -- point scaling down to no pixels, or going negative
      -- so that the image is inverted.
    ss : Double
    ss = if posY > markY
          then scale - scale * (scaleFactor * (posY  - markY))
          else scale + scale * (scaleFactor * (markY - posY))

    ss' : Double
    ss' = max 0.01 ss
    
    port' = record { viewPortScale = ss' } port


||| Zoom in a `ViewState` by the scale step.
controlZoomIn : ViewState -> ViewState
controlZoomIn viewState 
    = record { viewStateViewPort = port' } viewState
  where
    port : ViewPort
    port = viewStateViewPort viewState
    
    scaleStep : Double
    scaleStep = viewStateScaleStep viewState

    port' = record { viewPortScale = viewPortScale port / scaleStep } port

||| Zoom out a `ViewState` by the scale step.
controlZoomOut : ViewState -> ViewState
controlZoomOut viewState
    = record { viewStateViewPort = port' } viewState
  where
    port : ViewPort
    port = viewStateViewPort viewState

    scaleStep : Double
    scaleStep = viewStateScaleStep viewState

    port' = record { viewPortScale = viewPortScale port * scaleStep } port

||| Offset a viewport.
motionBump : ViewPort -> (Double, Double) -> ViewPort
motionBump port (bumpX, bumpY)
    = record { viewPortTranslate = trans - o } port
  where
    trans = viewPortTranslate port

    scale : Double
    scale = viewPortScale port

    r : Double
    r = viewPortRotate port

    offset : (Double, Double)
    offset = (bumpX / scale, bumpY / scale)

    o       = rotateV (degToRad r) offset

||| Initial view state, with user defined config.
export
viewStateInitWithConfig : CommandConfig -> ViewState
viewStateInitWithConfig commandConfig 
  = MkViewState 
      (SortedMap.fromList commandConfig)  -- viewStateCommands
      0.85                                -- viewStateScaleStep
      0.6                                 -- viewStateRotateFactor
      0.01                                -- viewStateScaleFactor
      Nothing                             -- viewStateTranslateMark
      Nothing                             -- viewStateRotateMark
      Nothing                             -- viewStateScaleMark
      viewPortInit                        -- viewStateViewPort

||| The initial view state.
export
viewStateInit : ViewState
viewStateInit = viewStateInitWithConfig defaultCommandConfig

keyDownViewPortCommandView : SortedMap Command (List (Key, Maybe Modifiers)) 
                           -> Key 
                           -> Modifiers
                           -> Maybe Command 
keyDownViewPortCommandView commands key keyMods = do
    (cmd, cmdKeys) <- findViewPortCommand commands viewPortCommands
    let flag = or $ map (isCommand2 cmd key keyMods) cmdKeys 
    if flag 
      then Just cmd
      else Nothing
  where
    or : List Bool -> Bool
    or [] = False
    or (x :: xs) = let v = or xs 
                   in  v || x

    viewPortCommands : List Command
    viewPortCommands = [ CRestore, CBumpZoomOut, CBumpZoomIn
                       , CBumpLeft, CBumpRight, CBumpUp
                       , CBumpDown, CBumpClockwise
                       , CBumpCClockwise, CTranslate
                       , CRotate, CScale ]

    findViewPortCommand : SortedMap Command (List (Key, Maybe Modifiers))
                        -> List Command
                        -> Maybe (Command, (List (Key, Maybe Modifiers)))
    findViewPortCommand commands [] = Nothing
    findViewPortCommand commands (c :: cs) =
        maybe 
          (findViewPortCommand commands cs)
          (\csMatch => Just (c, csMatch))
          mayCsMatch
      where
        mayCsMatch = SortedMap.lookup c commands

sub : Double -> Double -> Double
sub x y = x - y

||| Like 'updateViewStateWithEvent', but returns 'Nothing' if no update
|||   was needed.
-- matching on Key Down
export
updateViewStateWithEventMaybe : Event -> ViewState -> Maybe ViewState
updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState with (keyDownViewPortCommandView (viewStateCommands viewState) key keyMods)
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CRestore 
      = Just $ record { viewStateViewPort = viewPortInit } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpZoomOut 
      = Just $ controlZoomIn viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpZoomIn 
      = Just $ controlZoomOut viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpLeft 
      = Just $ record { viewStateViewPort = motionBump (viewStateViewPort viewState) (20, 0) } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpRight 
      = Just $ record { viewStateViewPort = motionBump (viewStateViewPort viewState) (-20, 0) } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpUp 
      = Just $ record { viewStateViewPort = motionBump (viewStateViewPort viewState) (0, -20) } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpDown 
      = Just $ record { viewStateViewPort = motionBump (viewStateViewPort viewState) (0, 20) } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpClockwise 
      = Just $ record { viewStateViewPort = record { viewPortRotate $= (+ 5) } (viewStateViewPort viewState) } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CBumpCClockwise 
      = Just $ record { viewStateViewPort = record { viewPortRotate $= flip sub (-5) } (viewStateViewPort viewState) } viewState
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CTranslate 
      = if not $ (isJust $ viewStateRotateMark viewState) || (isJust $ viewStateScaleMark viewState)
          then Just $ record { viewStateViewPort = record { viewPortRotate $= flip sub (-5) } (viewStateViewPort viewState) } viewState
          else Nothing
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CRotate
      = if not $ (isJust $ viewStateTranslateMark viewState) || (isJust $ viewStateScaleMark viewState)
          then Just $ record { viewStateRotateMark = Just pos } viewState
          else Nothing
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Just CScale
      = if not $ (isJust $ viewStateTranslateMark viewState) || (isJust $ viewStateRotateMark viewState)
          then Just $ record { viewStateScaleMark  = Just pos } viewState
          else Nothing
    updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState | Nothing = Nothing

-- matching on key Up
updateViewStateWithEventMaybe (EventKey key Up keyMods pos) viewState = 
  -- Kill current translate/rotate/scale command when the mouse button
  -- is released.
    Just $ (if currentlyTranslating then killTranslate else id)
      $ (if currentlyRotating    then killRotate    else id)
      $ (if currentlyScaling     then killScale     else id)
      $ viewState
  where
    currentlyTranslating    = isJust $ viewStateTranslateMark viewState
    currentlyRotating       = isJust $ viewStateRotateMark    viewState
    currentlyScaling        = isJust $ viewStateScaleMark     viewState

    killTranslate vs = record { viewStateTranslateMark = Nothing } vs
    killRotate    vs = record { viewStateRotateMark    = Nothing } vs
    killScale     vs = record { viewStateScaleMark     = Nothing } vs

-- Note that only a translation or rotation applies, not both at the same time.
-- TODO: is the usage of <+> correct here? 
updateViewStateWithEventMaybe (EventMotion pos) viewState
  = motionScale     (viewStateScaleMark     viewState) pos viewState <+> -- `mplus` 
    motionTranslate (viewStateTranslateMark viewState) pos viewState <+> -- `mplus`
    motionRotate    (viewStateRotateMark    viewState) pos viewState
updateViewStateWithEventMaybe (EventResize _) _ = Nothing

||| Apply an event to a `ViewState`.
export
updateViewStateWithEvent : Event -> ViewState -> ViewState
updateViewStateWithEvent ev viewState
  = fromMaybe viewState $ updateViewStateWithEventMaybe ev viewState