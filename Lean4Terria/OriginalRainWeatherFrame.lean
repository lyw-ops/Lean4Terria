import Lean4Terria.OriginalRainAtmosphere

/-! Resumable desktop DoUpdate weather protocol. This is the actual caller
order around complete method ports, not a claim to implement all of DoUpdate.
External ambience/golf/snow/sand/graphics calls remain explicit requests. -/
namespace Lean4Terria.OriginalRainWeatherFrame
open OriginalRainAtmosphere

structure State where
  weather : OriginalOuterWeather.State
  music : MusicState
  rain : Pool
  cloudAlpha : Float32
  gamePaused : Bool := false
  random : UnifiedRandom

structure Context where
  music : MusicContext
  rain : RainContext
  outer : OriginalOuterWeather.Context
  selectedApplication : Bool := true
  canUpdateGameplay : Bool := true
  canPauseGame : Bool := false

inductive Stage where
  | updateWindyDayState
  | makeRain
  | updateCloudLayer
  | updateWeather (background : Nat)
  deriving BEq, Repr

structure Call where
  stage : Stage
  random : UnifiedRandom

inductive Request where
  | ambienceServer
  | backgroundFlashes
  | golfState
  | outer (request : OriginalOuterWeather.Request)
  | unpausedUpdateSeed
  | ambience
  | snowing
  | sandstormEmitDust
  deriving Repr

inductive Continuation where
  | afterAmbienceServer
  | afterBackgroundFlashes
  | afterGolf
  | outer (background : Nat) (continuation : OriginalOuterWeather.Continuation)
  | afterUnpausedUpdateSeed
  | afterAmbience
  | afterSnowing
  | afterSandstormEmitDust
  deriving BEq, Repr

inductive Status where
  | outsideGameplay
  | paused
  | pending
  | finishedWeather
  deriving BEq, Repr

structure Pending where
  request : Request
  continuation : Continuation

structure Result where
  state : State
  status : Status
  pending : Option Pending := none
  calls : Array Call := #[]
  rainCalls : Array OriginalRainAtmosphere.Call := #[]
  outerCalls : Array OriginalOuterWeather.NativeCall := #[]
  outerEvents : Array OriginalOuterWeather.Event := #[]

private def rainContext (s : State) (c : Context) : RainContext :=
  {c.rain with gamePaused := s.gamePaused,cloudAlpha := s.cloudAlpha,windCurrent := s.weather.weather.windCurrent}

private def outerContext (s : State) (c : Context) (background : Nat) : OriginalOuterWeather.Context :=
  {c.outer with cloudAlpha := s.cloudAlpha,stormMusic := s.music.storm,background := background}

private def ask (state : State) (request : Request) (continuation : Continuation) : Result :=
  {state,status := .pending,pending := some {request,continuation}}

private def runOuter (state : State) (c : Context) (start : Nat) : Result := Id.run do
  let mut state := state
  let mut calls := #[]
  let mut outerCalls := #[]
  let mut outerEvents := #[]
  for background in [start:c.outer.dayRate.toNat] do
    calls := calls.push {stage := .updateWeather background,random := state.random}
    let next := OriginalOuterWeather.advance state.weather (outerContext state c background) state.random
    state := {state with weather := next.state,random := next.random}
    outerCalls := outerCalls ++ next.calls
    outerEvents := outerEvents ++ next.events
    if let some pending := next.pending then
      return {state,status := .pending,pending := some {request := .outer pending.request,continuation := .outer background pending.continuation},calls,outerCalls,outerEvents}
  return {ask state .unpausedUpdateSeed .afterUnpausedUpdateSeed with calls,outerCalls,outerEvents}

private def afterGolf (state : State) (c : Context) (grid : OriginalCollision.Grid) : Except String Result := do
  let mut state := state
  let mut calls := #[]
  let mut rainCalls := #[]
  let mut outerCalls := #[]
  let mut outerEvents := #[]
  if allowRain c.selectedApplication state.gamePaused && state.cloudAlpha > 0 then
    calls := calls.push {stage := .makeRain,random := state.random}
    let next ← makeRain state.rain (rainContext state c) grid state.random
    state := {state with rain := next.pool,random := next.random}
    rainCalls := next.calls
  if c.outer.netMode != 1 then
    calls := calls.push {stage := .updateCloudLayer,random := state.random}
    let next := OriginalOuterWeather.advanceCloudLayer state.weather (outerContext state c 0) state.random
    state := {state with weather := next.state,random := next.random}
    outerCalls := next.calls
    outerEvents := next.events
  let next := runOuter state c 0
  return {next with calls := calls ++ next.calls,rainCalls,outerCalls := outerCalls ++ next.outerCalls,outerEvents := outerEvents ++ next.outerEvents}

/-- The gameplay/menu gate precedes windy state. Windy state precedes the server
cloudAlpha assignment and CanPauseGame. A paused frame completes no rain/outer
stage; a nonpaused frame explicitly writes Main.gamePaused=false. -/
def beginFrame (state : State) (c : Context) : Except String Result := do
  if c.rain.netMode != c.outer.netMode || c.rain.gameMenu != c.outer.gameMenu then
    throw "Inconsistent rain/outer caller net or menu context"
  if c.outer.gameMenu && c.outer.netMode != 2 then return {state,status := .outsideGameplay}
  if !c.canUpdateGameplay && c.outer.netMode != 2 then return {state,status := .outsideGameplay}
  let call : Call := {stage := .updateWindyDayState,random := state.random}
  let music := updateWindyDayState state.music {c.music with cloudAlpha := state.cloudAlpha,windTarget := state.weather.weather.windTarget}
  let state := {state with music,cloudAlpha := if c.outer.netMode == 2 then c.outer.rainStrength else state.cloudAlpha}
  if c.canPauseGame then return {state := {state with gamePaused := true},status := .paused,calls := #[call]}
  return {ask {state with gamePaused := false} .ambienceServer .afterAmbienceServer with calls := #[call]}

/-- Caller executes exactly the pending source method and supplies its real
resulting state/RNG/live Context. No identity assumption is made for a callee.
finishedWeather ends this weather protocol, not the entity/time/draw frame. -/
def resumeFrame (state : State) (c : Context) (grid : OriginalCollision.Grid)
    (continuation : Continuation) : Except String Result := do
  match continuation with
  | .afterAmbienceServer => return ask state .backgroundFlashes .afterBackgroundFlashes
  | .afterBackgroundFlashes => return ask state .golfState .afterGolf
  | .afterGolf => afterGolf state c grid
  | .outer background continuation =>
    let next := OriginalOuterWeather.resume state.weather (outerContext state c background) state.random continuation
    let state := {state with weather := next.state,random := next.random}
    if let some pending := next.pending then
      return {state,status := .pending,pending := some {request := .outer pending.request,continuation := .outer background pending.continuation},outerCalls := next.calls,outerEvents := next.events}
    let rest := runOuter state c (background+1)
    return {rest with outerCalls := next.calls ++ rest.outerCalls,outerEvents := next.events ++ rest.outerEvents}
  | .afterUnpausedUpdateSeed => return ask state .ambience .afterAmbience
  | .afterAmbience =>
    if c.outer.netMode == 2 then return {state,status := .finishedWeather}
    return ask state .snowing .afterSnowing
  | .afterSnowing => return ask state .sandstormEmitDust .afterSandstormEmitDust
  | .afterSandstormEmitDust => return {state,status := .finishedWeather}

end Lean4Terria.OriginalRainWeatherFrame
