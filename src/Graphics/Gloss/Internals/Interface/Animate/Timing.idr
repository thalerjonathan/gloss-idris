||| Handles timing of animation.
|||      The main point is that we want to restrict the framerate to something
|||      sensible, instead of just displaying at the machines maximum possible
|||      rate and soaking up 100% cpu. 
|||
|||      We also keep track of the elapsed time since the start of the program,
|||      so we can pass this to the user's animation function.
|||
module Graphics.Gloss.Internals.Interface.Animate.Timing

import Data.IORef

import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Animate.State

getsIORef : IORef a -> (a -> r) -> IO r
getsIORef ref fun = liftA fun $ readIORef ref

||| Handles animation timing details.
|||     Call this function at the start of each frame.
export
animateBegin :  Backend GLFWState 
             => IORef State 
             -> IORef GLFWState 
             -> IO () -- DisplayCallback
animateBegin stateRef backendRef = do
  -- write the current time into the display state
  displayTime             <- elapsedTime backendRef
  displayTimeLast         <- stateRef `getsIORef` stateDisplayTime
  let displayTimeElapsed  = displayTime - displayTimeLast

  modifyIORef stateRef $ \s => record 
          { stateDisplayTime      = displayTime 
          , stateDisplayTimeLast  = displayTimeLast } s

  -- increment the animation time
  animate        <- stateRef `getsIORef` stateAnimate
  animateCount   <- stateRef `getsIORef` stateAnimateCount
  animateTime    <- stateRef `getsIORef` stateAnimateTime
  animateStart   <- stateRef `getsIORef` stateAnimateStart

{-
  when (animateCount `mod` 5 == 0)
    $  putStr  $  "  displayTime        = " ++ show displayTime                ++ "\n"
              ++ "  displayTimeLast    = " ++ show displayTimeLast            ++ "\n"
              ++ "  displayTimeElapsed = " ++ show displayTimeElapsed         ++ "\n"
              ++ "  fps                = " ++ show (1 / displayTimeElapsed)   ++ "\n"
  -}
     
  when (animate && not animateStart)
    $ modifyIORef stateRef $ \s => record
          { stateAnimateTime       = animateTime + displayTimeElapsed } s
                  
  when animate
    $ modifyIORef' stateRef $ \s => record 
          { stateAnimateCount      = animateCount + 1
          , stateAnimateStart      = False  } s

||| Handles animation timing details.
|||     Call this function at the end of each frame.
export
animateEnd :  Backend GLFWState 
           => IORef State 
           -> IORef GLFWState 
           -> IO () --DisplayCallback
animateEnd stateRef backendRef = do
  -- timing gate, limits the maximum frame frequency (FPS)
  timeClamp       <- stateRef `getsIORef` stateDisplayTimeClamp

  -- the start of this gate
  gateTimeStart   <- elapsedTime backendRef                       

  -- end of the previous gate
  gateTimeEnd     <- stateRef `getsIORef` stateGateTimeEnd        
  let gateTimeElapsed = gateTimeStart - gateTimeEnd

  when (gateTimeElapsed < timeClamp)
    $ do   sleep backendRef (timeClamp - gateTimeElapsed)

  gateTimeFinal   <- elapsedTime backendRef

  modifyIORef' stateRef $ \s => record
          { stateGateTimeEnd      = gateTimeFinal 
          , stateGateTimeElapsed  = gateTimeElapsed } s