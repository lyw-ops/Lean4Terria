import Lean4Terria.OriginalForestClock

/-! Desktop1.4.5.8 Main.UpdateWeather and updateCloudLayer. UI/audio/lighting
calls are explicit resumable requests in their actual order. Neither this
module nor its weather RNG silently completes an unimplemented callee. -/
namespace Lean4Terria.OriginalOuterWeather
open OriginalForestClock

structure Point where
  x : Float32 := 0
  y : Float32 := 0
  deriving Repr

structure Rectangle where
  x : Int
  y : Int
  width : Int
  height : Int
  deriving BEq, Repr

structure Camera where
  scaledPosition : Point := {}
  scaledSize : Point := {}
  unscaledPosition : Point := {}
  unscaledSize : Point := {}
  usingNewLighting : Bool := true
  deriving Repr

private def i32 (value : Int) : Int := (Int32.ofInt value).toInt
private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def nextFloat (random : UnifiedRandom) : Float32 × UnifiedRandom :=
  let (sample,random) := random.sample
  (sample.toFloat32,random)

/-- Actual GetAreaToLight arithmetic, with the camera's live getter values. -/
def areaToLight (camera : Camera) : Rectangle :=
  let position := if camera.usingNewLighting then camera.scaledPosition else camera.unscaledPosition
  let size := if camera.usingNewLighting then camera.scaledSize else camera.unscaledSize
  let left := i32 ((position.x / 16).toFloat.floor.toInt64.toInt - 1)
  let right := i32 (((position.x + size.x) / 16).toFloat.floor.toInt64.toInt + 2)
  let top := i32 ((position.y / 16).toFloat.floor.toInt64.toInt - 1)
  let bottom := i32 (((position.y + size.y) / 16).toFloat.floor.toInt64.toInt + 2)
  {x := left,y := top,width := i32 (right - left),height := i32 (bottom - top)}

structure Thunder where
  delay : Int := 0
  distance : Int := 0
  skipSound : Bool := false
  lightning : Float32 := 0
  speed : Float32 := 0
  decay : Float32 := 0
  deriving Repr

structure State where
  weather : DryWeather
  thunder : Thunder := {}
  deriving Repr

structure Context where
  netMode : Int := 0
  gameMenu : Bool := false
  background : Int := 0
  selectedApplication : Bool := true
  playWhenUnfocused : Bool := false
  freezeWind : Bool := false
  lanternsUp : Bool := false
  /-- Real active-player life scan (statLifeMax>=120) at the wind reset. -/
  hasLifeCrystal : Bool := false
  /-- Fresh independent Guid.NewGuid().GetHashCode() at ResetWindCounter. -/
  windGuidHash : Option Int32 := none
  rainStrength : Float32 := 0
  cloudAlpha : Float32 := 0
  raining : Bool := false
  dayRate : Int := 1
  sceneCenter : Point := {}
  sceneTileY : Int := 0
  worldSurface : Float := 350
  rockLayer : Float := 600
  atmosphere : Float32 := 1
  remixWorld : Bool := false
  /-- Original IsItStorming simply returns Main._shouldUseStormMusic. -/
  stormMusic : Bool := false
  graveyardIntensity : Float32 := 0
  flashyEffectsWorld : Bool := true
  shimmerAlpha : Float32 := 0
  camera : Camera := {}
  deriving Repr

inductive Event where
  | syncWorld
  | newLightningCalled (forced skipSound : Bool)
  | windReset (extreme : Bool)
  | lightningStarted (forced skipSound : Bool)
  deriving BEq, Repr

inductive Request where
  | sound43 (position : Point)
  | lightTiles (area : Rectangle)
  | windEntropy
  deriving Repr

inductive Continuation where
  | afterThunderSound
  | afterFirstLightTiles
  | afterSecondLightTiles
  | afterWindEntropy (oldNegative resetExtreme : Bool)
  deriving BEq, Repr

structure Pending where
  request : Request
  continuation : Continuation
  deriving Repr

inductive CallKind where
  | newLightning (forced skipSound : Bool)
  | syncWorld
  | resetWindCounter (extreme : Bool)
  deriving BEq, Repr

/-- Observational call order and the exact Main.rand state at each native callee
entry. This is produced by the independent port, never an oracle input. -/
structure NativeCall where
  kind : CallKind
  random : UnifiedRandom

structure Result where
  state : State
  random : UnifiedRandom
  events : Array Event := #[]
  calls : Array NativeCall := #[]
  pending : Option Pending := none

/-- Complete NewLightning. The flashy-world/shimmer gate precedes every draw.
The forced branch overrides fields AFTER consuming the ordinary draws. -/
def newLightning (state : State) (context : Context) (force skipSound : Bool)
    (random : UnifiedRandom) : Result := Id.run do
  let call : NativeCall := {kind := .newLightning force skipSound,random}
  if !context.flashyEffectsWorld || context.shimmerAlpha > 0 then return {state,random,events := #[.newLightningCalled force skipSound],calls := #[call]}
  let (close,next) := random.next 0 3
  let (delay,random) := if close == 0 then next.next 1 31 else
    let (far,next) := next.next 0 2
    if far == 0 then next.next 31 121 else next.next 11 61
  let (decay,random) := nextFloat random
  let (speed,random) := nextFloat random
  let thunder := {state.thunder with
    delay := if force then 1 else delay
    distance := if force then 0 else delay
    decay := decay * 0.05 + 0.008
    speed := if force then 0.2 else speed * 0.05 + 0.05
    skipSound}
  return {state := {state with thunder},random,events := #[.newLightningCalled force skipSound,.lightningStarted force skipSound],calls := #[call]}

private def boundWind (target : Float32) (hasLifeCrystal : Bool) : Float32 :=
  if !hasLifeCrystal && target.abs > 0.35 then (if target < 0 then -0.35 else 0.35) else target

private def advanceClouds (state : State) (context : Context) (random : UnifiedRandom) : Result := Id.run do
  let weather := state.weather
  let mut pending := weather.pendingClouds
  let (drift,random) := random.next 0 60
  let (change,random) := if drift == 0 then random.next (-1) 2 else (0,random)
  pending := i32 (pending + change)
  let (increase,random) := random.next 0 1000
  if single increase < 50 * weather.cloudBackground.alpha then pending := i32 (pending + 1)
  let (decrease,random) := random.next 0 1300
  if single decrease < 25 * (1 - weather.cloudBackground.alpha) then pending := i32 (pending - 1)
  let (rainIncrease,random) := random.next 0 1000
  if single rainIncrease < 200 * context.cloudAlpha && pending < 100 then pending := i32 (pending + 1)
  let (rainExtra,random) := random.next 0 1000
  if single rainExtra < 50 * context.cloudAlpha then pending := i32 (pending + 1)
  let mut random := random
  if pending > 66 then
    let (reduce,next) := random.next 0 100
    random := next
    if reduce == 0 then
      let (count,next) := random.next 1 3
      pending := i32 (pending - count)
      random := next
  if pending < 50 then
    let (increase,next) := random.next 0 100
    random := next
    if increase == 0 then
      let (count,next) := random.next 1 3
      pending := i32 (pending + count)
      random := next
  if weather.cloudBackground.active ≤ 0 && pending > 100 && context.cloudAlpha == 0 then pending := 100
  pending := max (-20) pending
  let mut clouds := weather.clouds
  let mut events := #[]
  let mut calls : Array NativeCall := #[]
  if context.cloudAlpha > 0 && single clouds < 200 * context.cloudAlpha then
    while single clouds < 200 * context.cloudAlpha do
      let (count,next) := random.next 0 30
      random := next
      clouds := min 200 (i32 (clouds + count))
      if pending < clouds then pending := clouds
    if context.netMode == 2 then
      events := events.push .syncWorld
      calls := calls.push {kind := .syncWorld,random}
  let mut counter := i32 (weather.weatherCounter - 1)
  if counter ≤ 0 then
    let (burst,next) := random.next 0 2
    random := next
    if burst == 0 then
      let (large,next) := random.next 0 2
      let (extra,next) := next.next 0 (if large == 0 then 250 else 100)
      pending := i32 (pending + extra)
      random := next
    pending := min 200 (max 0 pending)
    clouds := pending
    let (interval,next) := random.next 3600 10800
    counter := interval
    random := next
    if context.netMode == 2 then
      events := events.push .syncWorld
      calls := calls.push {kind := .syncWorld,random}
  return {state := {state with weather := {weather with pendingClouds := pending,clouds,weatherCounter := counter}}
          random,events,calls}

private def finishWindReset (state : State) (context : Context) (oldNegative resetExtreme : Bool)
    (guidHash : Int32) (random : UnifiedRandom) : Result := Id.run do
  let entryRandom := random
  let (wind,extreme) := resetWindCounters guidHash resetExtreme state.weather.extremeWindCounter
  let mut wind := wind
  let mut extreme := extreme
  let mut target := state.weather.windTarget
  let mut random := random
  if resetExtreme then
    let (calm,next) := random.next 0 30
    random := next
    if calm < 13 then
      let (still,next) := random.next 0 2
      random := next
      if still == 0 then
        target := 0
        let (interval,next) := random.next 7200 28801
        wind := interval
        random := next
      else
        let (speed,next) := random.next (-200) 201
        target := single speed * 0.001
        random := next
    else
      let (medium,next) := random.next 0 20
      let (speed,next) := if medium < 13 then next.next (-400) 401 else next.next (-850) 851
      target := single speed * 0.001
      random := next
    target := boundWind target context.hasLifeCrystal
    for (threshold,lower,upper) in #[(0.3,5,11),(0.5,10,21),(0.7,15,31)] do
      if target.abs.toFloat > threshold then
        let (extra,next) := random.next lower upper
        extreme := i32 (extreme + extra)
        random := next
  let (flip,next) := random.next 0 3
  random := next
  if flip != 0 && ((oldNegative && target > 0) || (!oldNegative && target < 0)) then target := -target
  if target > 0.8 then target := 0.8
  if target < -0.8 then target := -0.8
  let result := advanceClouds {state with weather := {state.weather with windTarget := target,windCounter := wind,extremeWindCounter := extreme}} context random
  return {result with events := #[.windReset resetExtreme] ++ result.events,calls := #[{kind := .resetWindCounter resetExtreme,random := entryRandom}] ++ result.calls}

private def advanceWind (state : State) (context : Context) (random : UnifiedRandom) : Result := Id.run do
  if context.netMode == 1 || (context.netMode != 2 && context.gameMenu) then return {state,random}
  if context.freezeWind then return advanceClouds state context random
  if context.lanternsUp then return {state,random}
  let weather : DryWeather := {state.weather with windCounter := i32 (state.weather.windCounter - 1)}
  if weather.windCounter > 0 then
    let target : Float32 := if weather.windTarget > 0.8 then 0.8 else weather.windTarget
    let target := if target < -0.8 then -0.8 else target
    return advanceClouds {state with weather := {weather with windTarget := target}} context random
  let oldNegative : Bool := weather.windTarget < 0
  let (small,random) := random.next 0 4
  let (change,random) := if small == 0 then random.next (-25) 26 else
    let (medium,random) := random.next 0 2
    if medium == 0 then random.next (-50) 51 else random.next (-100) 101
  let weather := {weather with windTarget := boundWind (weather.windTarget + single change * 0.001) context.hasLifeCrystal,extremeWindCounter := i32 (weather.extremeWindCounter - 1)}
  let state := {state with weather}
  let resetExtreme := weather.extremeWindCounter ≤ 0
  match context.windGuidHash with
  | none => return {state,random,pending := some {request := .windEntropy,continuation := .afterWindEntropy oldNegative resetExtreme}}
  | some guidHash => return finishWindReset state context oldNegative resetExtreme guidHash random

private def beginWind (state : State) (context : Context) (random : UnifiedRandom) : Result :=
  let target := state.weather.windTarget * (1 + 0.5555556 * context.rainStrength)
  let current := state.weather.windCurrent
  let step := 0.0003 + (target - current).abs * 0.0015
  let current := if current < target then
      let current := current + step
      if current > target then target else current
    else if current > target then
      let current := current - step
      if current < target then target else current
    else current
  advanceWind {state with weather := {state.weather with windCurrent := current}} context random

private def afterSound (state : State) (context : Context) (random : UnifiedRandom) : Result := Id.run do
  let thunder := state.thunder
  if thunder.speed > 0 then
    let flash := thunder.lightning + thunder.speed
    let thunder := if flash ≥ 1 then {thunder with lightning := 1,speed := 0} else {thunder with lightning := flash}
    return {state := {state with thunder},random,pending := some {request := .lightTiles (areaToLight context.camera),continuation := .afterFirstLightTiles}}
  if thunder.lightning > 0 then
    let thunder := {thunder with lightning := thunder.lightning - thunder.decay}
    return {state := {state with thunder},random,pending := some {request := .lightTiles (areaToLight context.camera),continuation := .afterFirstLightTiles}}
  let mut state := state
  let mut random := random
  let mut events := #[]
  let mut calls := #[]
  if thunder.delay ≤ 0 && context.sceneTileY.toFloat < context.rockLayer && context.atmosphere == 1 && !context.remixWorld then
    if context.stormMusic then
      let mut lower : Float32 := 600
      let mut upper : Float32 := 1600
      for (threshold,lo,hi) in #[(0.8,0.6,0.8),(0.7,0.7,0.9),(0.6,0.8,0.95)] do
        if context.rainStrength.toFloat > threshold then
          lower := lower * lo
          upper := upper * hi
      for (threshold,lo,hi) in #[(0.7,0.6,0.8),(0.6,0.7,0.9),(0.5,0.8,0.95)] do
        if state.weather.windTarget.abs.toFloat > threshold then
          lower := lower * lo
          upper := upper * hi
      let (baseDelay,next) := random.next lower.toInt32.toInt upper.toInt32.toInt
      let delay := single baseDelay * ((1 - context.rainStrength + 1) / 2)
      let delay := delay * ((1 - state.weather.windTarget + 1) / 2)
      let (strike,next) := next.next 0 delay.toInt32.toInt
      random := next
      if strike == 0 then
        let next := newLightning state context false false random
        state := next.state; random := next.random; events := next.events; calls := next.calls
    else if context.graveyardIntensity.toFloat ≥ 0.9 then
      let (strike,next) := random.next 0 120
      random := next
      if strike == 0 then
        let next := newLightning state context false false random
        state := next.state; random := next.random; events := next.events; calls := next.calls
  let result := beginWind state context random
  return {result with events := events ++ result.events,calls := calls ++ result.calls}

/-- Full Main.UpdateWeather with exact external-call boundaries. GameTime is
unused by the native method. updateCloudLayer is a DIFFERENT method. -/
def advance (state : State) (context : Context) (random : UnifiedRandom) : Result := Id.run do
  if context.netMode == 2 || context.background != 0 then return beginWind state context random
  let thunder := state.thunder
  if thunder.delay > 0 then
    let thunder := {thunder with delay := i32 (thunder.delay - 1)}
    let state := {state with thunder}
    if thunder.delay == 0 then
      let (side,random) := random.next 0 2
      let distance := single (i32 (thunder.distance * 15)) * (if side == 0 then -1 else 1)
      let ceiling := single (i32 ((context.worldSurface * 16).toInt32.toInt - 500))
      let position := {context.sceneCenter with
        x := context.sceneCenter.x + distance
        y := if context.sceneCenter.y > ceiling then ceiling else context.sceneCenter.y}
      if (context.selectedApplication || context.playWhenUnfocused) && !thunder.skipSound then
        return {state,random,pending := some {request := .sound43 position,continuation := .afterThunderSound}}
      return afterSound state context random
    return afterSound state context random
  return afterSound state context random

/-- Resume after the caller has executed the pending sound/lighting method,
including its RNG and live camera changes, or supplied the real independent
GUID hash. Completed prefix events must not be re-applied on resume. -/
def resume (stateAfterCallee : State) (context : Context) (randomAfterCallee : UnifiedRandom)
    (continuation : Continuation) : Result :=
  match continuation with
  | .afterThunderSound => afterSound stateAfterCallee context randomAfterCallee
  | .afterFirstLightTiles => {state := stateAfterCallee,random := randomAfterCallee,pending := some {request := .lightTiles (areaToLight context.camera),continuation := .afterSecondLightTiles}}
  | .afterSecondLightTiles => beginWind stateAfterCallee context randomAfterCallee
  | .afterWindEntropy oldNegative resetExtreme =>
    match context.windGuidHash with
    | none => {state := stateAfterCallee,random := randomAfterCallee,pending := some {request := .windEntropy,continuation := .afterWindEntropy oldNegative resetExtreme}}
    | some guidHash => finishWindReset stateAfterCallee context oldNegative resetExtreme guidHash randomAfterCallee

/-- Separate Main.updateCloudLayer, using live cloudAlpha/dayRate/raining and
storm-music flags. This changes only cloudBGActive; cloudBGAlpha is elsewhere. -/
def advanceCloudLayer (state : State) (context : Context) (random : UnifiedRandom) : Result := Id.run do
  if context.netMode == 1 || (context.netMode == 0 && context.gameMenu) then return {state,random}
  let multiplier := max (1 : Float32) (1 + 4 * context.cloudAlpha)
  let mut timer := state.weather.cloudBackground.active
  let mut random := random
  let mut events := #[]
  if timer > 0 then
    if timer > 1 then timer := timer - single context.dayRate / multiplier
    if timer < 1 then timer := 1
    if timer == 1 then
      let bound := (single ((7200 : Int).tdiv (max context.dayRate 1)) * multiplier).toInt32.toInt
      let (stop,next) := random.next 0 bound
      random := next
      if stop == 0 then
        let (delay,next) := random.next 14400 345600
        timer := single (-delay)
        random := next
        if context.netMode == 2 then events := events.push .syncWorld
  else
    if timer < 0 then
      timer := timer + single context.dayRate * multiplier
      if context.raining then timer := timer + single (i32 (2 * context.dayRate)) * multiplier
    if timer > 0 then timer := 0
    let mut early := false
    if timer == 0 then
      let bound := (single ((43200 : Int).tdiv (if context.dayRate == 0 then 1 else context.dayRate)) / multiplier).toInt32.toInt
      let (start,next) := random.next 0 bound
      random := next
      if start == 0 then
        let (delay,next) := random.next 10800 172800
        timer := single delay
        random := next
        if context.netMode == 2 then events := events.push .syncWorld
        early := true
    if !early && context.stormMusic then
      let (delay,next) := random.next 3600 14400
      timer := single delay
      random := next
      if context.netMode == 2 then events := events.push .syncWorld
  return {state := {state with weather := {state.weather with cloudBackground := {state.weather.cloudBackground with active := timer}}}
          random,events,calls := events.map (fun _ => {kind := .syncWorld,random})}

end Lean4Terria.OriginalOuterWeather
