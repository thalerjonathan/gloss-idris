module Graphics.Gloss.Data.ViewState
{-
        ( Command      (..)
        , CommandConfig
        , defaultCommandConfig
        , ViewState     (..)
        , viewStateInit
        , viewStateInitWithConfig
        , updateViewStateWithEvent
        , updateViewStateWithEventMaybe)
where
-}

{-
import qualified Data.Map                       as Map
import Data.Map                                 (Map)
import Data.Maybe
import Control.Monad (mplus)
-}

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

-- very primitive implementation of ordering
-- respect equality but don't care about ordering, its always LT
-- because creating all combinations is too much work
-- TODO: does this really work? 
Ord Command where
  compare CRestore CRestore                = EQ
  compare CTranslate CTranslate            = EQ
  compare CRotate CRotate                  = EQ
  compare CScale CScale                    = EQ
  compare CBumpZoomOut CBumpZoomOut        = EQ
  compare CBumpZoomIn CBumpZoomIn          = EQ
  compare CBumpLeft CBumpLeft              = EQ
  compare CBumpRight CBumpRight            = EQ
  compare CBumpUp CBumpUp                  = EQ
  compare CBumpDown CBumpDown              = EQ
  compare CBumpClockwise CBumpClockwise    = EQ
  compare CBumpCClockwise CBumpCClockwise  = EQ
  compare _ _ = LT

public export
CommandConfig : Type
CommandConfig = List (Command, List (Key, Maybe Modifiers))

||| The default commands.  Left click pans, wheel zooms, right click
|||   rotates, "r" key resets.
defaultCommandConfig : CommandConfig
defaultCommandConfig = ?defaultCommandConfig
{-
 =      [ (CRestore,
                [ (Char 'r',                    Nothing) ])

        , (CTranslate,
                [ ( MouseButton LeftButton
                  , Just (Modifiers { shift = Up, ctrl = Up,   alt = Up }))
                ])

        , (CScale,
                [ ( MouseButton LeftButton
                  , Just (Modifiers { shift = Up, ctrl = Down, alt = Up }))

                , ( MouseButton RightButton
                  , Just (Modifiers { shift = Up, ctrl = Up,   alt = Up })) 
                ])

        , (CRotate,
                [ ( MouseButton LeftButton
                  , Just (Modifiers { shift = Up, ctrl = Up,   alt = Down }))

                , ( MouseButton RightButton
                  , Just (Modifiers { shift = Up, ctrl = Down, alt = Up }))
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
-}

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

-- | Apply a rotation to the `ViewState`.
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

-- | Apply a scale to the `ViewState`.
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

||| Initial view state, with user defined config.
viewStateInitWithConfig : CommandConfig -> ViewState
viewStateInitWithConfig commandConfig 
  = MkViewState 
      (SortedMap.fromList commandConfig)
      0.85 
      0.6
      0.01
      Nothing
      Nothing
      Nothing
      viewPortInit
    {-
  { viewStateCommands             = Map.fromList commandConfig
  , viewStateScaleStep            = 0.85
  , viewStateRotateFactor         = 0.6
  , viewStateScaleFactor          = 0.01
  , viewStateTranslateMark        = Nothing
  , viewStateRotateMark           = Nothing
  , viewStateScaleMark            = Nothing
  , viewStateViewPort             = viewPortInit }
-}

||| The initial view state.
viewStateInit : ViewState
viewStateInit = viewStateInitWithConfig defaultCommandConfig

||| Like 'updateViewStateWithEvent', but returns 'Nothing' if no update
|||   was needed.
updateViewStateWithEventMaybe : Event -> ViewState -> Maybe ViewState
-- matching on Key Down 
updateViewStateWithEventMaybe (EventKey key Down keyMods pos) viewState = ?bla
{-
        | isCommand commands CRestore key keyMods
        = Just $ viewState { viewStateViewPort = viewPortInit }

        | isCommand commands CBumpZoomOut key keyMods
        = Just $ controlZoomIn viewState

        | isCommand commands CBumpZoomIn key keyMods
        = Just $ controlZoomOut viewState

        | isCommand commands CBumpLeft key keyMods
        = Just $ viewState { viewStateViewPort = motionBump port (20, 0) }

        | isCommand commands CBumpRight key keyMods
        = Just $ viewState { viewStateViewPort = motionBump port (-20, 0) }

        | isCommand commands CBumpUp key keyMods
        = Just $ viewState { viewStateViewPort = motionBump port (0, -20) }

        | isCommand commands CBumpDown key keyMods
        = Just $ viewState { viewStateViewPort = motionBump port (0, 20) }

        | isCommand commands CBumpClockwise key keyMods
        = Just $ viewState { viewStateViewPort 
                                = port { viewPortRotate = viewPortRotate port + 5 } }

        | isCommand commands CBumpCClockwise key keyMods
        = Just $ viewState { viewStateViewPort 
                                = port { viewPortRotate = viewPortRotate port - 5 } }


        -- Start Translation.
        | isCommand commands CTranslate key keyMods
        , not  $ currentlyRotating    || currentlyScaling
        = Just $ viewState { viewStateTranslateMark = Just pos }

        -- Start Rotation.
        | isCommand commands CRotate key keyMods
        , not  $ currentlyTranslating || currentlyScaling
        = Just $ viewState { viewStateRotateMark = Just pos }

        -- Start Scale.
        | isCommand commands CScale key keyMods
        , not  $ currentlyTranslating || currentlyRotating
        = Just $ viewState { viewStateScaleMark  = Just pos }

        | otherwise
        = Nothing
-}

-- matching on key Up
updateViewStateWithEventMaybe (EventKey key Up keyMods pos) viewState = 
  -- Kill current translate/rotate/scale command when the mouse button
  -- is released.
    Just $ (if currentlyTranslating then killTranslate else id)
      $ (if currentlyRotating    then killRotate    else id)
      $ (if currentlyScaling     then killScale     else id)
      $ viewState
  where
    commands : SortedMap Command (List (Key, Maybe Modifiers))
    commands                = viewStateCommands viewState

    port : ViewPort
    port = viewStateViewPort viewState
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
updateViewStateWithEvent : Event -> ViewState -> ViewState
updateViewStateWithEvent ev viewState
  = fromMaybe viewState $ updateViewStateWithEventMaybe ev viewState

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