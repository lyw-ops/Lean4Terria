import Lean4Terria.OriginalWorldTimePhase
import Lean4Terria.OriginalWorld

/-! One complete `Main.UpdateTime` for the session, through the released staged modules:
`OriginalForestClock.beginWorldTime` (rain / slime-rain prefix, clock), the event hooks of
`Main.UpdateTime` IL_03b6..05d5 in their order, `advanceWorldTimeSandstorm`, `advanceWorldTimeWarning`,
`advanceTownTime`, and the phase coordinator `OriginalWorldTimePhase.advance` with its suspensions.

Every hook is transcribed from desktop 1.4.5.8 CIL (`.lake/forest-desktop-integration/time-runtime/il/`).
Where an original callee is not ported, the step returns a boundary naming it; nothing is skipped
silently. Callees that change the caller's world (an NPC birth, a house check) are performed by the
caller through `Callee`, at their original position, and the step resumes with the receipt.

Claude, 2026-11-19. -/
namespace Lean4Terria.OriginalTimeCaller

open OriginalForestClock (WorldTimeState WorldTimeStep TownTimeInputs TownTimeStep)
open OriginalWorldTimePhase (Call Receipt Effect)

/-- Event state of the hooks between the clock and the phase coordinator. -/
structure Hooks where
  /-- `CultistRitual.delay` (saved: `_tempCultistDelay`) and `recheck` (not saved; static default 0). -/
  cultistDelay : Int
  cultistRecheck : Int := 0
  /-- `CreditsRollEvent._creditsRollRemainingTime` (not saved). -/
  creditsRemaining : Int := 0
  /-- `TeleportPylonsSystem._cooldownForUpdatingPylonsList` (not saved). -/
  pylonCooldown : Int := 0
  /-- `BirthdayParty._wasCelebrating`, `LanternNight._wasLanternNight` (static defaults false). -/
  partyWasCelebrating : Bool := false
  lanternWasUp : Bool := false
  deriving Repr

structure State where
  phase : OriginalWorldTimePhase.State
  hooks : Hooks
  /-- The census's town table and priority were computed while the bestiary completion was unknown
  (`Main.GetBestiaryProgressReport` is not ported): whatever reads them must stop. -/
  townPriorityUnknown : Bool := false

/-- World and player facts of this update, from the caller's real state. -/
structure Live where
  netMode : Int := 0
  gameMenu : Bool := false
  windCurrent : Float32
  expertMode : Bool
  /-- `OriginalWorldWeather.Context` flags. -/
  hasWorldSurface : Bool
  /-- `WorldGen.Skyblock.lowTiles`. -/
  lowTiles : Bool
  downedSlimeKing : Bool
  worldWidth : Int
  /-- `Main.player` in slot order (rain eligibility, King Slime readiness, highest luck). -/
  weatherPlayers : Array OriginalWorldWeather.Player
  /-- The phase coordinator's context (players, NPCs, progression, seeds, …). -/
  phase : OriginalWorldTimePhase.Context
  /-- `NPC.downedGolemBoss`, `NPC.downedBoss3` (the cultist ritual gate). -/
  downedGolem : Bool
  downedSkeletron : Bool
  /-- `NPC.AnyDanger(false, false, false)`. -/
  anyDanger : Bool
  /-- `NPC.taxCollector`: a Tax Collector is present. -/
  taxCollector : Bool
  /-- `DD2Event.Ongoing`. -/
  dd2Ongoing : Bool := false
  /-- `NPC.MoonLordCountdown`. -/
  moonLordCountdown : Int := 0
  /-- `MysticLogFairiesEvent._canSpawnFairies` is kept in the phase state (`mysticCanSpawn`). -/
  remixWorld : Bool
  /-- `Skyblock.noAltars` is not tracked by the caller: the dusk Eye-of-Cthulhu roll after the Eye is
  down (`Transitions` IL path reading it) must stop. -/
  noAltarsKnown : Bool := false
  /-- Town checks: `NPC.travelNPC`, active town NPCs except 37 / 453, a homeless actor. -/
  travelNPC : Bool := false
  eligibleTownCount : Nat := 0
  homelessActorPresent : Bool := false

/-- An original callee performed by the caller on its own world `σ`. -/
abbrev Callee (σ : Type) := Call → σ → Except String (σ × Receipt)

structure Out (σ : Type) where
  state : State
  world : σ
  weatherEvents : Array OriginalWorldWeather.Event := #[]
  effects : Array Effect := #[]
  /-- `BirthdayParty.UpdateTime` IL_00b3..0107: the party ended (`Lang.misc[99]`, colour 255/0/160). -/
  partyEnded : Bool := false

/-- The state of a freshly loaded world in a fresh process. `WorldFile.SetOngoingToTemps`
(`il/WorldFile.SetOngoingToTemps.il`) applies the saved clock, blood moon, eclipse, rain (`raining`,
`rainTime`, `maxRaining`, and `cloudAlpha := maxRaining`), the cultist delay, the party, the sandstorm,
lantern night, the meteor shower count and coin rain; `LoadWorldFlags` the clouds, the cloud background and
the slime-rain time; `WorldGen.clearWorld` resets `checkForSpawns`, `freeCake`, `spawnEye`, `spawnHardBoss`
and the prioritised town NPC; `playWorldCallBack` clears `anglerQuestFinished`
(`il/load-world-static-writes.txt`). Every other field keeps its static default. `Main.halloween` / `xMas`
are set by the caller once it knows the local date (`checkHalloween` / `checkXMas` at load). -/
def initial (header : OriginalWorld.Header) (saved : OriginalForestClock.SavedWorldWeather)
    (clock : OriginalForestClock.Clock) (random : UnifiedRandom) (critters : OriginalForestClock.CritterChances)
    (starfallBoost : Float32) (fairyLog : Bool) (npcTypeCount : Nat) : State :=
  let tf := header.timeFlags
  { phase := {
      world := {
        clock, random
        rain := {
          active := saved.raining, time := saved.rainTime, strength := saved.maxRain, oldStrength := 0
          coinRain := tf.coinRain, cloudBackgroundActive := Float32.ofInt saved.cloudBackgroundActive
          cloudAlpha := saved.maxRain, clouds := saved.clouds, ladyBugBoost := 0 }
        slime := { active := false, time := saved.slimeRainTime }
        sandstorm := saved.sandstorm }
      crimson := header.crimson, spawnMeteor := tf.spawnMeteor, meteorShowerCount := tf.meteorShowerCount
      eclipse := header.eclipse, bloodMoon := header.bloodMoon
      forceHalloweenToday := header.forceHalloweenForToday, forceXmasToday := header.forceXMasForToday
      fastForwardDawn := tf.fastForwardTimeToDawn, fastForwardDusk := tf.fastForwardTimeToDusk
      sundialCooldown := Int.ofNat tf.sundialCooldown, moondialCooldown := Int.ofNat tf.moondialCooldown
      invasionDelay := tf.invasionDelay, invasionType := header.invasionType
      townCanSpawn := Array.replicate npcTypeCount false
      anglerQuest := header.anglerQuest, anglerWhoFinished := tf.anglerWhoFinishedToday
      starfallBoost, critters, fairyLog
      party := { genuine := tf.partyGenuine, manual := tf.partyManual, cooldown := tf.partyCooldown
                 celebrating := tf.partyCelebratingNPCs.map Int.toNat }
      lanterns := { genuine := header.lanternNight.genuine, manual := header.lanternNight.manual
                    nextNight := header.lanternNight.nextNightIsGenuine, cooldown := header.lanternNight.cooldown }
      blockLight718 := !clock.dayTime }
    hooks := { cultistDelay := tf.cultistDelay } }

/-- `Main.UpdateTimeRate` (IL in `il/Main.UpdateTimeRate.il`): 60 while fast-forwarding, otherwise the
creative time power's rate (not ported: 1, with no sleeping players), 1 on the menu. -/
def updateTimeRate (phase : OriginalWorldTimePhase.State) (gameMenu : Bool) : Int :=
  if phase.fastForwarding then 60 else if gameMenu then 1 else 1

/-- `NPC.BusyWithAnyInvasionOfSorts` without its slime term (the prefix tests live slime time itself). -/
private def busyInvasion (phase : OriginalWorldTimePhase.State) (live : Live) : Bool :=
  phase.bloodMoon || phase.eclipse || phase.snowMoon || phase.pumpkinMoon || phase.invasionType > 0 ||
    live.dd2Ongoing

private def weatherContext (phase : OriginalWorldTimePhase.State) (live : Live) (dayRate : Int) :
    OriginalWorldWeather.Context :=
  { netMode := live.netMode, gameMenu := live.gameMenu, dayRate
    dayTime := phase.world.clock.dayTime, time := phase.world.clock.time, windCurrent := live.windCurrent
    hardMode := live.phase.hardMode, expertMode := live.expertMode, remixWorld := live.remixWorld
    hasWorldSurface := live.hasWorldSurface, tenthAnniversary := live.phase.anniversary
    goodWorld := live.phase.good, lowTiles := live.lowTiles, downedSlimeKing := live.downedSlimeKing
    busyInvasion := busyInvasion phase live, freezeRain := false
    lanternsUp := phase.lanternsUp, nextNightIsLanternNight := phase.lanterns.nextNight
    worldWidth := live.worldWidth, players := live.weatherPlayers }

/-- `CultistRitual.UpdateTime` (IL_0000..0085). `TrySpawning` cannot act before Golem and Skeletron in
hardmode (`CheckRitual` IL_0000..0020) and draws nothing; `PlayerLOS` has no side effect. -/
private def cultist (hooks : Hooks) (live : Live) (rate : Int) : Except String Hooks := do
  if live.netMode == 1 then return hooks
  let delay := max 0 (hooks.cultistDelay - rate)
  let recheck := max 0 (hooks.cultistRecheck - rate)
  if delay != 0 || recheck != 0 then return { hooks with cultistDelay := delay, cultistRecheck := recheck }
  if live.anyDanger then return { hooks with cultistDelay := delay, cultistRecheck := 600 * 6 }
  if live.phase.hardMode && live.downedGolem && live.downedSkeletron then
    throw "CultistRitual.TrySpawning with Golem down (CheckRitual / NewNPC 437) not ported"
  return { hooks with cultistDelay := delay, cultistRecheck := 600 }

/-- `BirthdayParty.UpdateTime` (IL_0000..0116): drop celebrating NPCs that can no longer party
(`CanNPCParty`: active, town, aiStyle ≠ 0, not 37 / 453 / 441, not a town pet); an emptied genuine party
ends with its chat line unless a manual one is up. -/
private def party (phase : OriginalWorldTimePhase.State) (hooks : Hooks) (live : Live) :
    OriginalWorldTimePhase.State × Hooks × Bool := Id.run do
  let up := phase.party.genuine || phase.party.manual
  let mut phase := phase
  let mut ended := false
  if hooks.partyWasCelebrating != up && live.netMode != 1 && phase.party.celebrating.size > 0 then
    let canParty := fun (slot : Nat) => match live.phase.npcs[slot]? with
      | some n => n.active && n.town && n.aiStyle != 0 && n.typeId != 37 && n.typeId != 453 &&
          n.typeId != 441 && !n.townPet
      | none => false
    -- IL_0071..00a5: the index advances past a removal, as the original does
    let mut list := phase.party.celebrating
    let mut i := 0
    while i < list.size do
      if !canParty list[i]! then list := list.eraseIdx! i
      i := i + 1
    phase := { phase with party := { phase.party with celebrating := list } }
    if list.size == 0 then
      phase := { phase with party := { phase.party with genuine := false } }
      ended := !phase.party.manual
  let upNow := phase.party.genuine || phase.party.manual
  return (phase, { hooks with partyWasCelebrating := upNow }, ended)

/-- `LanternNight.UpdateTime` (IL_0000..008d): a genuine lantern night ends when it may not persist
(`LanternsCanPersist`: night and `LanternsCanStart`). -/
private def lanterns (phase : OriginalWorldTimePhase.State) (hooks : Hooks) (live : Live) :
    OriginalWorldTimePhase.State × Hooks :=
  let canPersist := !phase.world.clock.dayTime && OriginalWorldTimePhase.lanternsCanStart phase live.phase
  let phase := if phase.lanterns.genuine && !canPersist then
    { phase with lanterns := { phase.lanterns with genuine := false } } else phase
  (phase, { hooks with lanternWasUp := phase.lanternsUp })

/-- `MysticLogFairiesEvent.UpdateTime` (IL_0000..0048) with `IsAGoodTime` (night, 6480 ≤ time ≤ 25920
unless remix). `TrySpawningFairies` is not ported. -/
private def mysticLogs (phase : OriginalWorldTimePhase.State) (live : Live) (rate : Int) :
    Except String OriginalWorldTimePhase.State := do
  if live.netMode == 1 || !phase.mysticCanSpawn then return phase
  let clock := phase.world.clock
  let goodTime := !clock.dayTime && (live.remixWorld || (clock.time ≥ 6480.0000965595245 && clock.time ≤ 25920.000386238098))
  if !goodTime then return phase
  let delay := max 0 (phase.mysticDelay - rate)
  if delay != 0 then return { phase with mysticDelay := delay }
  throw "MysticLogFairiesEvent.TrySpawningFairies not ported"

/-- One `Main.UpdateTime`. The caller passes its world and a callee handler for the coordinator's calls. -/
def step {σ : Type} (state : State) (live : Live) (callee : Callee σ) (world : σ) :
    Except String (Out σ) := do
  let phase := state.phase
  let entryTime := phase.world.clock.time
  -- IL_0000..03a7: rain / slime-rain prefix with the ENTRY rate; IL_03b6 UpdateTimeRate; clock addition
  let rate := updateTimeRate phase live.gameMenu
  let begin := OriginalForestClock.beginWorldTime phase.world (weatherContext phase live phase.dayRate) rate
  let mut phase := { phase with world := begin.state, dayRate := rate }
  let mut weatherEvents := begin.events
  let mut hooks := state.hooks
  -- IL_03d2..03e1: CultistRitual, BirthdayParty, LanternNight
  hooks ← cultist hooks live rate
  let (phase1, hooks1, partyEnded) := party phase hooks live
  let (phase2, hooks2) := lanterns phase1 hooks1 live
  phase := phase2
  hooks := hooks2
  -- IL_03e1 Sandstorm.UpdateTime with the live (effective) rate
  let sand := OriginalForestClock.advanceWorldTimeSandstorm phase.world (weatherContext phase live rate)
  phase := { phase with world := sand.state }
  weatherEvents := weatherEvents ++ sand.events
  -- IL_03e6 DD2Event.UpdateTime: without an event only scene filters change
  if live.dd2Ongoing then throw "DD2Event.UpdateTime during an Old One's Army event not ported"
  -- IL_03eb CreditsRollEvent.UpdateTime: Clamp(remaining - 1, 0, 28800)
  hooks := { hooks with creditsRemaining := max 0 (min 28800 (hooks.creditsRemaining - 1)) }
  -- IL_03f5 MysticLogFairiesEvent.UpdateTime
  phase ← mysticLogs phase live rate
  -- IL_03ff TeleportPylonsSystem.Update: the list scan draws nothing
  hooks := if live.netMode == 1 then hooks
    else if hooks.pylonCooldown > 0 then { hooks with pylonCooldown := hooks.pylonCooldown - 1 }
    else { hooks with pylonCooldown := 2147483647 }
  -- IL_0404..0558: Moon Lord countdown (screen shake, then SpawnOnPlayer 398)
  if live.moonLordCountdown > 0 then throw "Moon Lord countdown (SpawnOnPlayer 398) not ported"
  -- IL_0558..05c8: taxes
  if live.taxCollector && live.netMode != 2 && !live.gameMenu then throw "Player.CollectTaxes not ported"
  -- IL_05d5 UpdateSlimeRainWarning
  let warning := OriginalForestClock.advanceWorldTimeWarning phase.world live.netMode
  phase := { phase with world := warning.state }
  weatherEvents := weatherEvents ++ warning.events
  -- IL_05e0..0753: travelling merchant and homeless NPC checks
  let town := OriginalForestClock.advanceTownTime phase.world
    { netMode := live.netMode, dayRate := rate, fastForwarding := phase.fastForwarding
      worldUpdateRate := live.phase.worldUpdateRate, travelNPC := live.travelNPC
      eligibleTownCount := live.eligibleTownCount, homelessActorPresent := live.homelessActorPresent }
  if let some pending := town.pending then
    throw s!"town time callee {reprStr pending.call} not ported"
  phase := { phase with world := town.state }
  -- the coordinator's dusk branch reads Skyblock.noAltars once the Eye is down
  if phase.world.clock.dayTime && phase.world.clock.time > 54000 && live.phase.progress.eye &&
      !live.phase.bossesKeepSpawning && !live.noAltarsKnown then
    throw "nightfall after the Eye of Cthulhu: Skyblock.noAltars (altar scan) not tracked"
  -- IL_0758..: phase coordinator, with every suspension performed by the caller
  let mut world := world
  let mut effects : Array Effect := #[]
  let mut townPriorityUnknown := state.townPriorityUnknown
  let mut out ← OriginalWorldTimePhase.advance phase live.phase entryTime
  let mut guard := 0
  while out.pending.isSome && guard < 1000 do
    let some pending := out.pending | break
    effects := effects ++ out.effects
    let mut after := out.state
    let receipt ← match pending.call with
      | .updateTimeRate =>
        -- Main.UpdateTimeRate after the coordinator cleared the reached fast-forward flag
        after := { after with dayRate := updateTimeRate after live.gameMenu }
        pure Receipt.unit
      | .bestiaryReport =>
        -- Main.GetBestiaryProgressReport is not ported: the census continues (its only draw, Next(40),
        -- does not read it) and its town table / priority are marked unknown
        townPriorityUnknown := true
        pure (Receipt.bestiary 0)
      | call => do
        let (world', receipt) ← callee call world
        world := world'
        pure receipt
    out ← OriginalWorldTimePhase.resume pending after live.phase receipt
    guard := guard + 1
  if !out.complete then throw "WorldTimePhase did not complete"
  effects := effects ++ out.effects
  return { state := { state with phase := out.state, hooks, townPriorityUnknown }, world, weatherEvents, effects
           partyEnded }

end Lean4Terria.OriginalTimeCaller
