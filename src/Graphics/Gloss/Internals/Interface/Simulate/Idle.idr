module Graphics.Gloss.Internals.Interface.Simulate.Idle

import Data.IORef

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Internals.Interface.Backend             as Backend
import Graphics.Gloss.Internals.Interface.Animate.State       as AN
import Graphics.Gloss.Internals.Interface.Simulate.State      as SM

getsIORef : IORef a -> (a -> r) -> IO r
getsIORef ref fun = liftA fun $ readIORef ref

untilM : (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM test op i = go i
  where
    go x = if test x
              then pure x
              else op x >>= go
              {-
    go x | test x    = return x
         | otherwise = op x >>= go
          -}

-- take the number of steps specified by controlWarp
simulate_run 
        :  Backend GLFWState
        => IORef SM.State
        -> IORef AN.State
        -> IO ViewPort
        -> IORef world
        -> (ViewPort -> Double -> world -> IO world)
        -> IORef GLFWState -> IO ()-- IdleCallback
simulate_run simSR _ viewSA worldSR worldAdvance backendRef = do   
  viewS           <- viewSA
  simS            <- readIORef simSR
  worldS          <- readIORef worldSR

  -- get the elapsed time since the start simulation (wall clock)
  elapsedTime     <- Backend.elapsedTime backendRef

  -- get how far along the simulation is
  simTime                 <- simSR `getsIORef` stateSimTime

  -- we want to simulate this much extra time to bring the simulation
  --      up to the wall clock.
  let thisTime    = elapsedTime - simTime
    
  -- work out how many steps of simulation this equals
  resolution      <- simSR `getsIORef` stateResolution
  let timePerStep = 1 / cast resolution
  let thisSteps_  = cast {to=Int} (cast resolution * thisTime) --truncate $ fromIntegral resolution * thisTime
  let thisSteps   = if thisSteps_ < 0 then 0 else thisSteps_

  let newSimTime  = simTime + cast thisSteps * timePerStep
    
{-      putStr  $  "elapsed time    = " ++ show elapsedTime     ++ "\n"
          ++ "sim time        = " ++ show simTime         ++ "\n"
          ++ "this time       = " ++ show thisTime        ++ "\n"
          ++ "this steps      = " ++ show thisSteps       ++ "\n"
          ++ "new sim time    = " ++ show newSimTime      ++ "\n"
          ++ "taking          = " ++ show thisSteps       ++ "\n\n"
-}
  -- work out the final step number for this display cycle
  let nStart      = stateIteration simS
  let nFinal      = nStart + cast thisSteps

  -- keep advancing the world until we get to the final iteration number
  (_,world') 
    <- untilM (\(n, _)        => n >= nFinal)
              (\(n, w)        => liftA (\w' => (n+1,w')) ( worldAdvance viewS timePerStep w))
              (nStart, worldS)

  -- write the world back into its IORef
  -- We need to seq on the world to avoid space leaks when the window is not showing.
  -- world' `seq` writeIORef worldSR world'
  writeIORef worldSR world'  -- note: no need to seq in Idris

  -- update the control state
  modifyIORef simSR $ \c => record
          { stateIteration     = nFinal
          , stateSimTime       = newSimTime } c
  
  -- tell glut we want to draw the window after returning
  Backend.postRedisplay backendRef

||| The graphics library calls back on this function when it's finished drawing
|||      and it's time to do some computation.
export
callback_simulate_idle
         : Backend GLFWState
        => IORef SM.State                               -- ^ the simulation state
        -> IORef AN.State                               -- ^ the animation statea
        -> IO ViewPort
        -- ^ action to get the 'ViewPort'.  We don't use an 'IORef'
        -- directly because sometimes we hold a ref to a 'ViewPort' (in
        -- Game) and sometimes a ref to a 'ViewState'.
        -> IORef world                                  -- ^ the current world
        -> (ViewPort -> Double -> world -> IO world)     -- ^ fn to advance the world
        -> Double                                        -- ^ how much time to advance world by 
                                                        --      in single step mode
        -> IORef GLFWState -> IO ()-- IdleCallback
callback_simulate_idle simSR animateSR viewSA worldSR worldAdvance _singleStepTime backendRef
  = {-# SCC "callbackIdle" #-}
    do simulate_run simSR animateSR viewSA worldSR worldAdvance backendRef