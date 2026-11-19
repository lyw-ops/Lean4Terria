import Lean4Terria.OriginalWorldTimePhase.Events

namespace Lean4Terria.OriginalWorldTimePhase

def suspend (state : State) (call : Call) (continuation : Continuation)
    (entryTime : Float) (returnTo : Continuation) (effects : Array Effect := #[]) : Step :=
  { state := state
    effects := effects
    pending := some {
      call := call
      continuation := continuation
      entryTime := entryTime
      returnTo := returnTo
      normalEventsBlocked := state.normalEventsBlocked } }


private def nightEvents (state : State) (context : Context) : Step := Id.run do
  let mut state := state
  let mut effects := #[]
  state := {state with prioritizedTown:=0,checkForSpawns:=0}
  let (meteor,rng) := state.world.random.next 0 50
  state := {state with world.random:=rng}
  if meteor == 0 && context.netMode != 1 && context.progress.evil then state := {state with spawnMeteor:=true}
  if !context.surface then state := {state with spawnMeteor:=false}
  if state.lanternsUp then state := {state with normalEventsBlocked:=true}
  if state.eclipse && context.netMode != 1 then effects := effects.push (.achievement 3)
  state := {state with eclipse:=false}
  if context.netMode != 1 then effects := effects.push (.achievement 0)
  if !state.fastForwarding && !state.normalEventsBlocked then
    if (!context.progress.eye || context.bossesKeepSpawning || context.noAltars) && context.netMode != 1 then
      let ready := context.players.any fun p => p.active && p.lifeMax ≥ 200 && p.defense > 10
      if ready then
        let (roll,rng) := state.world.random.next 0 (if context.lowTiles then 10 else 3)
        state := {state with world.random:=rng}
        if roll == 0 && (context.npcs.filter fun n => n.active && n.town).size ≥ 4 then
          state := {state with spawnEye:=true}
          effects := effects.push (.chat (.misc 9))
    if context.netMode != 1 && !state.pumpkinMoon && !context.dd2 && !state.snowMoon &&
        (context.altarCount > 0 || context.lowTiles) && context.hardMode && !state.spawnEye then
      let (roll,rng) := state.world.random.next 0 10
      state := {state with world.random:=rng}
      if roll == 0 && !context.npcs.any (fun n => n.active && n.boss) &&
          (!(context.progress.mech1 && context.progress.mech2 && context.progress.mech3) || context.bossesKeepSpawning) then
        if context.remix && context.good then
          let (attempt,rng) := state.world.random.next 0 2
          state := {state with world.random:=rng}
          if attempt == 0 then
            let (boss,rng) := state.world.random.next 0 3
            state := {state with world.random:=rng,spawnHardBoss:=boss+1}
            effects := effects.push (.chat (.misc 108))
        else
          for _ in [:1000] do
            let (boss,rng) := state.world.random.next 0 3
            state := {state with world.random:=rng}
            let boss := boss+1
            let defeated := if boss == 1 then context.progress.mech1 else if boss == 2 then context.progress.mech2 else context.progress.mech3
            if !defeated || context.bossesKeepSpawning then
              state := {state with spawnHardBoss:=boss}
              effects := effects.push (.chat (.misc (if boss == 1 then 28 else if boss == 2 then 29 else 30)))
              break
    if !state.spawnEye && state.world.clock.moonPhase != 4 then
      let (blood,rng) := state.world.random.next 0 (if context.anniversary then 6 else 9)
      state := {state with world.random:=rng}
      if blood == 0 && context.netMode != 1 then
        if context.players.any (fun p => p.active && p.lifeMax > 120) then state := {state with bloodMoon:=true}
        if state.bloodMoon then
          state := {state with sundialCooldown:=0,moondialCooldown:=0}
          effects := effects ++ #[.achievement 4,.chat (.misc 8)]
  state := {state with world.clock.time:=0,world.clock.dayTime:=false}
  if context.netMode == 2 then effects := effects.push .syncWorld
  return {state,effects}

def nightAfterLogs (state : State) (context : Context) : Step := nightEvents state context

def nightAfterRate (state : State) (context : Context) (entryTime : Float) (returnTo : Continuation) : Step := Id.run do
  let state := {state with
    moondialCooldown:=if state.moondialCooldown > 0 then state.moondialCooldown-1 else state.moondialCooldown
    eyeKilledToday:=false,wallKilledToday:=false,freeCake:=false}
  let state := starNightSetup state context.anniversary
  let (critters,rng) := OriginalForestClock.rollCritters state.world.random context.anniversary
  let (state,effects) := partyNight {state with critters,world.random:=rng}
  let mut state := lanternNight state context
  if context.netMode != 1 then
    state := {state with mysticCanSpawn:=true,mysticDelay:=0}
    match context.logGrid with
    | none => return suspend state .scanMysticLogs .nightAfterLogs entryTime returnTo effects
    | some grid =>
      let stumps := scanLogs grid context.worldSurface context.rockLayer context.remix
      state := {state with stumps,fairyLog:=!stumps.isEmpty}
  let out := nightAfterLogs state context
  return {out with effects:=effects++out.effects}

def startNight (state : State) (context : Context) (entryTime : Float)
    (returnTo : Continuation := .nightToTown) : Step :=
  if state.fastForwardDusk then
    suspend {state with fastForwardDusk:=false} .updateTimeRate .nightAfterRate entryTime returnTo
  else nightAfterRate state context entryTime returnTo

def dayPirates (state : State) (context : Context) (entryTime : Float) (returnTo : Continuation) : Step := Id.run do
  if state.invasionType != 0 || !context.hardMode || (context.altarCount ≤ 0 && !context.lowTiles) then return {state}
  let (roll,rng) := state.world.random.next 0 (if context.progress.pirates then 60 else 30)
  let state := {state with world.random:=rng}
  if roll == 0 then return suspend state (.startInvasion 3) .dayAfterPirates entryTime returnTo
  return {state}

def dayAfterCalendar (state : State) (context : Context) (entryTime : Float) (returnTo : Continuation) : Step := Id.run do
  let state := stopMoonEvents state context.netMode
  let mut state := {state with
    world.clock.dayTime:=true
    world.clock.moonPhase:=if state.world.clock.moonPhase+1 ≥ 8 then 0 else state.world.clock.moonPhase+1
    sundialCooldown:=if state.sundialCooldown > 0 then state.sundialCooldown-1 else state.sundialCooldown}
  if context.drunk && context.netMode != 1 then state := {state with crimson:=!state.crimson}
  let mut effects := if context.netMode == 2 then #[Effect.syncWorld] else #[]
  if context.netMode == 1 then return {state,effects}
  effects := effects.push (.achievement 1)
  if state.normalEventsBlocked then return {state,effects}
  if context.hardMode && context.progress.mechAny then
    let (eclipse,rng) := state.world.random.next 0 20
    state := {state with world.random:=rng}
    if eclipse == 0 then
      state := {state with sundialCooldown:=0,moondialCooldown:=0,eclipse:=true}
      effects := effects ++ #[.achievement 2,.chat (.misc (if context.remix then 106 else 20))]
      if context.netMode == 2 then effects := effects.push .syncWorld
      return {state,effects}
  if state.snowMoon || state.pumpkinMoon || context.dd2 then return {state,effects}
  if context.shadowOrbSmashed then
    let (roll,rng) := state.world.random.next 0 (if !context.progress.goblins then 3 else if context.hardMode then 60 else 30)
    state := {state with world.random:=rng}
    if roll == 0 then return suspend state (.startInvasion 1) .dayAfterGoblin entryTime returnTo effects
  let out := dayPirates state context entryTime returnTo
  return {out with effects:=effects++out.effects}

def dayAfterXmas (state : State) (context : Context) (entryTime : Float) (returnTo : Continuation) : Step :=
  match context.halloweenCalendar with
  | none => suspend state .checkHalloween .dayAfterCalendar entryTime returnTo
  | some calendar => dayAfterCalendar (applyHalloweenCalendar state context calendar) context entryTime returnTo

def dayAfterRate (state : State) (context : Context) (entryTime : Float) (returnTo : Continuation) : Step := Id.run do
  let (state,effects) := anglerSwap state context
  let (state,partyEffects) := partyMorning state context
  let mut effects := effects++partyEffects
  let mut state := {state with
    lanterns.genuine:=false,lanterns.manual:=false
    invasionDelay:=if state.invasionDelay > 0 then state.invasionDelay-1 else state.invasionDelay
    prioritizedTown:=0,checkForSpawns:=0,world.clock.time:=0}
  if state.bloodMoon && context.netMode != 1 then effects := effects.push (.achievement 5)
  state := {state with bloodMoon:=false}
  if context.netMode != 1 && (state.pumpkinMoon || state.snowMoon) then
    effects := effects.push (.chat (.moonScore state.snowMoon state.totalInvasionPoints) .bossOrEvent)
  let seasons := morningSeasons state context
  state := seasons.1
  effects := effects++seasons.2
  match context.xmasCalendar with
  | none => return suspend state .checkXmas .dayAfterXmas entryTime returnTo effects
  | some calendar =>
    let out := dayAfterXmas (applyXmasCalendar state context calendar) context entryTime returnTo
    return {out with effects:=effects++out.effects}

def startDay (state : State) (context : Context) (entryTime : Float)
    (returnTo : Continuation := .dayToMeteor) : Step :=
  let state := {state with treeShakes:=0}
  if state.fastForwardDawn then
    suspend {state with fastForwardDawn:=false} .updateTimeRate .dayAfterRate entryTime returnTo
  else dayAfterRate state context entryTime returnTo

end Lean4Terria.OriginalWorldTimePhase
