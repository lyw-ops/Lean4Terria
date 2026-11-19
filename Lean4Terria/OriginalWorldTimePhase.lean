import Lean4Terria.OriginalWorldTimePhase.Town

/-! Main.UpdateTime after travel/homeless checks. Clock/rain/slime/sandstorm
have already advanced. Every actor/world callee can suspend with its preceding
state and stream; resume receives the callee's resulting state and live census. -/
namespace Lean4Terria.OriginalWorldTimePhase

private def finish (state : State) : Step :=
  {state:={state with blockLight718:=!state.world.clock.dayTime},complete:=true}

private def plusEffects (effects : Array Effect) (out : Step) : Step :=
  {out with effects:=effects++out.effects}

mutual
private partial def afterHelper (out : Step) (context : Context) (entryTime : Float)
    (returnTo : Continuation) : Except String Step := do
  if out.pending.isSome then return out
  return plusEffects out.effects (← dispatch out.state context entryTime returnTo)

private partial def meteorLate (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  if !(state.world.clock.time > 16200) || !state.spawnMeteor then return finish state
  let state := {state with spawnMeteor:=false}
  if context.lowTiles then
    let (state,effects) := meteorShower state
    return plusEffects effects (finish state)
  return suspend state .dropMeteor .afterMeteorDrop entryTime .finish

private partial def meteor (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  if context.netMode != 1 && state.spawnMeteor then
    if state.world.clock.time < 15000 then
      return ← meteorLate {state with canShowMeteorFall:=true} context entryTime
    if state.canShowMeteorFall && state.world.clock.time ≤ 16200 then
      return suspend state .meteorAmbience .afterMeteorAmbience entryTime .finish
    return ← meteorLate {state with canShowMeteorFall:=false} context entryTime
  meteorLate state context entryTime

private partial def nightTail (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  if state.world.clock.time > 32400 then
    return ← afterHelper (startDay state context entryTime .dayToMeteor) context entryTime .dayToMeteor
  meteor state context entryTime

private partial def deerclops (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  if context.netMode == 1 || !(entryTime < 16200 && state.world.clock.time ≥ 16200) || !state.world.rain.active then
    return ← nightTail state context entryTime
  let mut state := state
  if context.progress.deerclops then
    let (roll,rng) := state.world.random.next 0 4
    state := {state with world.random:=rng}
    if roll != 0 then return ← nightTail state context entryTime
  let player := context.players.find? fun p => p.active && !p.dead &&
    p.y.toFloat < context.worldSurface*16 && p.zoneSnow && p.townNPCs ≤ 0 &&
    (p.lifeMax2 ≥ 200 || p.defense ≥ 9) && p.canSpawnEnemiesNear && !context.anyDanger
  if let some player := player then
    return suspend state (.spawnOnPlayer player.index 668) .afterDeerclops entryTime .finish
  nightTail state context entryTime

private partial def mech (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  if state.spawnHardBoss ≤ 0 || context.netMode == 1 || !(state.world.clock.time > 4860) then
    return ← deerclops state context entryTime
  if !context.npcs.any (fun n => n.active && n.boss) then
    if let some player := context.players.find? (eligibleSurfacePlayer context) then
      if context.mechdusa then
        return suspend state (.spawnMechQueen player.index) .afterMech entryTime .finish
      if state.spawnHardBoss == 1 then
        return suspend state (.spawnOnPlayer player.index 134) .afterMech entryTime .finish
      if state.spawnHardBoss == 2 then
        return suspend state (.spawnOnPlayer player.index 125) (.afterTwinFirst player.index) entryTime .finish
      if state.spawnHardBoss == 3 then
        return suspend state (.spawnOnPlayer player.index 127) .afterMech entryTime .finish
  deerclops {state with spawnHardBoss:=0} context entryTime

private partial def eye (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  if state.spawnEye && context.netMode != 1 && state.world.clock.time > 4860 then
    if let some player := context.players.find? (eligibleSurfacePlayer context) then
      return suspend state (.spawnOnPlayer player.index 4) .afterEye entryTime .finish
  mech state context entryTime

private partial def dispatch (state : State) (context : Context) (entryTime : Float)
    (next : Continuation) : Except String Step := do
  match next with
  | .finish => return finish state
  | .nightToTown =>
    afterHelper (spawnTownNpcs state context entryTime .finish) context entryTime .finish
  | .dayToMeteor => meteor state context entryTime
  | _ => throw "WorldTimePhase: continuation requires a matching completed callee"
end

/-- Entry is original IL0758, after stage-one town checks. Do not add time here
or reuse the old advanceForestTime. entryTime is the pre-addition local1. -/
def advance (state : State) (context : Context) (entryTime : Float) : Except String Step := do
  let state := {state with normalEventsBlocked:=normalEventsBlocked state context}
  if !state.world.clock.dayTime then
    let state := {state with eclipse:=false}
    if state.fastForwarding || state.normalEventsBlocked then return ← nightTail state context entryTime
    eye state context entryTime
  else
    let state := stopMoonEvents {state with spawnHardBoss:=0,spawnEye:=false,bloodMoon:=false} context.netMode
    if state.world.clock.time > 54000 then
      afterHelper (startNight state context entryTime .nightToTown) context entryTime .nightToTown
    else dispatch state context entryTime .nightToTown

/-- Run after the actual callee, using its resulting state/RNG and refreshed
actor/player inputs. Previously emitted effects must not be emitted again. -/
def resume (pending : Pending) (stateAfterCallee : State) (context : Context)
    (receipt : Receipt) : Except String Step := do
  let state := {stateAfterCallee with normalEventsBlocked:=pending.normalEventsBlocked}
  let entryTime := pending.entryTime
  let helper := fun out => afterHelper out context entryTime pending.returnTo
  match pending.call,pending.continuation,receipt with
  | .updateTimeRate,.nightAfterRate,.unit => helper (nightAfterRate state context entryTime pending.returnTo)
  | .updateTimeRate,.dayAfterRate,.unit => helper (dayAfterRate state context entryTime pending.returnTo)
  | .scanMysticLogs,.nightAfterLogs,.unit => helper (nightAfterLogs state context)
  | .checkXmas,.dayAfterXmas,.unit => helper (dayAfterXmas state context entryTime pending.returnTo)
  | .checkHalloween,.dayAfterCalendar,.unit => helper (dayAfterCalendar state context entryTime pending.returnTo)
  | .startInvasion 1,.dayAfterGoblin,.unit => helper (dayPirates state context entryTime pending.returnTo)
  | .startInvasion 3,.dayAfterPirates,.unit => dispatch state context entryTime pending.returnTo
  | .spawnOnPlayer _ 4,.afterEye,.unit => mech {state with spawnEye:=false} context entryTime
  | .spawnOnPlayer _ 125,.afterTwinFirst player,.unit =>
    return suspend state (.spawnOnPlayer player 126) .afterMech entryTime .finish
  | .spawnOnPlayer _ _,.afterMech,.unit => deerclops {state with spawnHardBoss:=0} context entryTime
  | .spawnMechQueen _,.afterMech,.unit => deerclops {state with spawnHardBoss:=0} context entryTime
  | .spawnOnPlayer _ 668,.afterDeerclops,.unit => nightTail state context entryTime
  | .meteorAmbience,.afterMeteorAmbience,.unit => meteorLate {state with canShowMeteorFall:=false} context entryTime
  | .dropMeteor,.afterMeteorDrop,.meteorDropped success =>
    if success then return finish state
    let (roll,rng) := state.world.random.next 0 3
    let state := {state with world.random:=rng}
    if roll == 0 then
      let (state,effects) := meteorShower state
      return plusEffects effects (finish state)
    return finish state
  | .quickFindHome _,.censusAfterHome slot counts total,.unit =>
    helper (censusAfterHome state context slot counts total entryTime pending.returnTo)
  | .bestiaryReport,.censusAfterBestiary census,.bestiary completion =>
    helper (censusWithReport state context census completion entryTime pending.returnTo)
  | .spawnOldMan _ _,.censusAfterOldMan census,.npcSlot slot =>
    let out := censusFinish state context census
    helper {out with effects:=#[.npcHome slot context.dungeonX context.dungeonY]++out.effects}
  | _,_,_ => throw "WorldTimePhase.resume: callee/continuation/receipt mismatch"

end Lean4Terria.OriginalWorldTimePhase
