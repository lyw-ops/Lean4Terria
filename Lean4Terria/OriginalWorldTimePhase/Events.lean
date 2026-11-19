import Lean4Terria.OriginalWorldTimePhase.Types

namespace Lean4Terria.OriginalWorldTimePhase

open OriginalForestClock

def starNightSetup (state : State) (anniversary : Bool) : State := Id.run do
  let (high,rng) := state.world.random.next 0 (if anniversary then 5 else 10)
  let (boost,rng) := if high == 0 then
      let (n,rng) := rng.next 300 501
      ((Int32.ofInt n).toFloat32*0.01,rng)
    else
      let (low,rng) := rng.next 0 (if anniversary then 2 else 3)
      if low == 0 then
        let (n,rng) := rng.next 100 151
        ((Int32.ofInt n).toFloat32*0.01,rng)
      else (1,rng)
  return {state with world.random:=rng,starfallBoost:=boost,starFallCount:=0}

def scanLogs (grid : LogGrid) (surface rock : Float) (remix : Bool) : Array (Int × Int) := Id.run do
  let top := if remix then rock.toInt32.toInt else 100
  let bottom := if remix then (grid.height : Int)-350 else surface.toInt32.toInt-10
  let mut x : Int := 100
  let mut found := #[]
  while x < (grid.width : Int)-100 do
    let mut y := bottom
    while y ≥ top do
      let tile := grid.tileAt x y
      if tile.active && tile.typeId == 488 && tile.liquid == 0 then
        found := found.push (x-tile.frameX.tdiv 18,y-tile.frameY.tdiv 18)
      y := y-2
    x := x+3
  return found

def canParty (npc : Npc) : Bool :=
  npc.active && npc.town && npc.aiStyle != 0 &&
    npc.typeId != 37 && npc.typeId != 453 && npc.typeId != 441 && !npc.townPet

def partyNight (state : State) : State × Array Effect :=
  let ended := state.party.genuine || state.party.manual
  let party := {state.party with
    genuine:=false,manual:=false
    celebrating:=if state.party.genuine then #[] else state.party.celebrating}
  ({state with party},if ended then #[.chat (.misc 99) .party] else #[])

def partyMorning (state : State) (context : Context) : State × Array Effect := Id.run do
  if context.netMode == 1 || !context.npcs.any (fun n => n.active && n.typeId == 208) then
    return (state,#[])
  if state.party.cooldown > 0 then
    return ({state with party.cooldown:=state.party.cooldown-1},#[])
  let (attempt,rng) := state.world.random.next 0 (if context.anniversary then 7 else 10)
  let mut state := {state with world.random:=rng}
  if attempt != 0 then return (state,#[])
  let candidates := context.npcs.filter canParty
  if candidates.size < 5 then return (state,#[])
  let (cooldown,rng) := state.world.random.next 5 11
  let (three,rng) := rng.next 0 5
  let (count,rng) := if three == 0 && candidates.size > 12 then (3,rng)
    else
      let (two,rng) := rng.next 0 3
      (if two == 0 then 2 else 1,rng)
  let mut rng := rng
  let mut keys : Array (Int × Nat × Nat) := #[]
  for index in [:candidates.size] do
    let (key,next) := rng.nextInt
    rng := next
    keys := keys.push (key,index,candidates[index]!.index)
  let sorted := keys.qsort fun a b => a.1 < b.1 || (a.1 == b.1 && a.2.1 < b.2.1)
  let celebrating := (sorted.take count).map (·.2.2)
  state := {state with
    world.random:=rng
    freeCake:=true
    party:={state.party with genuine:=true,cooldown,celebrating}}
  return (state,#[.chat (.birthday celebrating) .party,.syncWorld,.birthdayAchievement])

def lanternsCanStart (state : State) (context : Context) : Bool :=
  !state.spawnMeteor && !state.bloodMoon && !state.pumpkinMoon && !state.snowMoon &&
    state.invasionType == 0 && context.moonLordCountdown == 0 &&
    !context.npcs.any (fun n => n.active && (n.boss || (13 ≤ n.typeId && n.typeId ≤ 15)))

def lanternNight (state : State) (context : Context) : State := Id.run do
  if context.netMode == 1 || !lanternsCanStart state context then return state
  let cooldown := if state.lanterns.cooldown > 0 then state.lanterns.cooldown-1 else state.lanterns.cooldown
  let mut state := {state with lanterns.cooldown:=cooldown}
  let mut attempt := false
  if cooldown == 0 && context.progress.moonLord then
    let (roll,rng) := state.world.random.next 0 14
    state := {state with world.random:=rng}
    attempt := roll == 0
  if !attempt && state.lanterns.nextNight then
    state := {state with lanterns.nextNight:=false}
    attempt := true
  if !attempt then return state
  let (cooldown,rng) := state.world.random.next 5 11
  return {state with world.random:=rng,lanterns.genuine:=true,lanterns.cooldown:=cooldown}

def anglerSwap (state : State) (context : Context) : State × Array Effect := Id.run do
  if context.netMode == 1 then return (state,#[])
  let progressed := context.progress.eye || context.progress.evil || context.progress.skeletron ||
    context.hardMode || context.progress.slimeKing || context.progress.queenBee
  let mut rng := state.world.random
  let mut index := 0
  let mut rejected := true
  while rejected do
    let (candidate,next) := rng.next 0 41
    rng := next
    index := candidate
    let item := if index < 39 then 2450+index else 4393+(index-39)
    rejected :=
      (#[2454,2462,2463,2465,2468,2471,2473,2480,2483,2484].contains item && !context.hardMode) ||
      (#[2454,2457,2485].contains item && state.crimson) ||
      (#[2463,2477].contains item && !state.crimson) ||
      (#[2476,2453,2473].contains item && !progressed) ||
      (!context.surface && #[2476,2479,2480,2452,2453,2481,2483,2456,2458,2459,2461,
        2467,2468,2487,2488,2470,2473,2474].contains item)
  return ({state with
    world := {state.world with random:=rng}
    anglerQuest := index.toNat
    anglerQuestFinished := false
    anglerWhoFinished := #[]},#[.syncAngler])

def stopMoonEvents (state : State) (netMode : Int) : State :=
  let active := state.pumpkinMoon || state.snowMoon
  let state := {state with pumpkinMoon:=false,snowMoon:=false}
  if active && netMode != 1 then {state with moonWave:=0,moonWaveKills:=0,totalInvasionPoints:=0}
  else state

def morningSeasons (state : State) (context : Context) : State × Array Effect := Id.run do
  if context.netMode == 1 then return (state,#[])
  let halloween := state.pumpkinMoon && state.moonWave ≥ 15 && !context.forceHalloweenForever
  let xmas := state.snowMoon && state.moonWave ≥ 15 && !context.forceXmasForever
  let mut effects := #[]
  if state.forceHalloweenToday != halloween then
    effects := effects.push (.chat (.key (if halloween then "Misc.StartedVictoryHalloween" else "Misc.EndedVictoryHalloween")))
  if state.forceXmasToday != xmas then
    effects := effects.push (.chat (.key (if xmas then "Misc.StartedVictoryXmas" else "Misc.EndedVictoryXmas")))
  return ({state with forceHalloweenToday:=halloween,forceXmasToday:=xmas},effects)

def applyXmasCalendar (state : State) (context : Context) (calendar : Calendar) : State :=
  {state with xmas := (calendar.month == 12 && calendar.day ≥ 15) || state.forceXmasToday || context.forceXmasForever}

def applyHalloweenCalendar (state : State) (context : Context) (calendar : Calendar) : State :=
  {state with halloween := (calendar.month == 10 && calendar.day ≥ 10) ||
      (calendar.month == 11 && calendar.day ≤ 1) || state.forceHalloweenToday || context.forceHalloweenForever}

def meteorShower (state : State) : State × Array Effect :=
  let (count,rng) := state.world.random.next 650 751
  ({state with world.random:=rng,meteorShowerCount:=count*4},#[.chat (.gen 92)])

end Lean4Terria.OriginalWorldTimePhase
