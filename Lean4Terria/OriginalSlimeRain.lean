import Lean4Terria.UnifiedRandom

/-! Desktop1.4.5.8 StartSlimeRain/StopSlimeRain and the independent warning clock.
The sky/network/chat effects are explicit requests; they do not stand in for
rendering or a caller's NPC spawning. Original-method receipts live under
`.lake/original-world-time-weather/`. -/
namespace Lean4Terria.OriginalSlimeRain

structure State where
  active : Bool := false
  time : Float := 0
  kills : Int := 0
  warningTime : Int := 0
  warningDelay : Int := 420
  deriving Repr

structure Context where
  netMode : Int := 0
  remixWorld : Bool := false
  hasWorldSurface : Bool := true
  myPlayer : Int := 0
  deriving Repr

inductive Event where
  | sky (active : Bool)
  | syncWorld
  | requestStart (player : Int)
  | warning (starting : Bool)
  deriving BEq, Repr

structure Result where
  state : State
  random : UnifiedRandom
  events : Array Event := #[]

/-- Client announce asks the server without changing local state. A received
client start sets54000; ordinary local/server starts choose the native duration.
Rain blocks local/server start, after the client branch, as in the original. -/
def start (state : State) (context : Context) (raining announce : Bool)
    (random : UnifiedRandom) : Result := Id.run do
  if context.remixWorld || !context.hasWorldSurface || state.active then
    return {state,random}
  if context.netMode == 1 then
    if announce then return {state,random,events := #[.requestStart context.myPlayer]}
    return {state := {state with active := true,time := 54000},random,events := #[.sky true]}
  if raining then return {state,random}
  let (time,random) := if state.time > 0 then (state.time,random) else
    let (duration,random) := random.next 32400 54000
    (duration.toFloat,random)
  let state := {state with active := true,time,kills := 0}
  let state := if announce then {state with warningTime := state.warningDelay} else state
  let events := if context.netMode == 0 then #[Event.sky true]
    else if announce then #[Event.syncWorld] else #[]
  return {state,random,events}

/-- The original computes a hardmode week value but never uses it in the
cooldown draw: both modes use -Next(3024,6048)*100. -/
def stop (state : State) (context : Context) (announce : Bool)
    (random : UnifiedRandom) : Result :=
  if !state.active then {state,random} else
  if context.netMode == 1 then
    {state := {state with active := false,time := 0},random,events := #[.sky false]}
  else
    let (wait,random) := random.next 3024 6048
    let state := {state with active := false,time := (-wait * 100).toFloat}
    let state := if announce then {state with warningTime := state.warningDelay} else state
    let events := if context.netMode == 0 then #[Event.sky false]
      else if announce then #[Event.syncWorld] else #[]
    {state,random,events}

/-- Main.UpdateTime's timer prefix uses the ENTRY dayRate, before UpdateTimeRate.
Expiry calls the actual stop contract. Negative cooldowns approach zero. -/
def advanceTimer (state : State) (context : Context) (dayRate : Int)
    (random : UnifiedRandom) : Result :=
  if state.time > 0 then
    let state := {state with time := state.time - dayRate.toFloat}
    if state.time ≤ 0 then stop state context true random else {state,random}
  else if state.time < 0 then
    {state := {state with time := min 0 (state.time + dayRate.toFloat)},random}
  else {state,random}

/-- UpdateSlimeRainWarning advances once per invocation, independent of dayRate.
Its caller omits the invocation on a multiplayer client. -/
def updateWarning (state : State) : State × Array Event :=
  if state.warningTime > 0 then
    let state := {state with warningTime := state.warningTime - 1}
    (state,if state.warningTime ≤ 0 then #[Event.warning (state.time > 0)] else #[])
  else (state,#[])

end Lean4Terria.OriginalSlimeRain
