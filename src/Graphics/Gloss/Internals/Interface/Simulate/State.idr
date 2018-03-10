module Graphics.Gloss.Internals.Interface.Simulate.State
       
||| Simulation state
public export
data State
  constructor MkState
  ||| The iteration number we're up to.
  stateIteration        : Integer
  ||| How many simulation setps to take for each second of real time
  stateResolution       : Int 
  ||| How many seconds worth of simulation we've done so far
  stateSimTime          : Float

||| Initial control state
export
stateInit : Int -> State
stateInit resolution
  = MkState
      0           -- stateIteration
      resolution  -- stateResolution
      0           -- stateSimTime