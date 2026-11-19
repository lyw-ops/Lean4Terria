import Lean4Terria.OriginalCombat
import Lean4Terria.OriginalForestAmbience
import Lean4Terria.OriginalForestClock
import Lean4Terria.OriginalTimeCaller
import Lean4Terria.OriginalRainWeatherFrame
import Lean4Terria.OriginalForestSpawn
import Lean4Terria.OriginalSceneWaterStyle
import Lean4Terria.OriginalTileRules
import Lean4Terria.OriginalWallWork.Tables

/-! The open-world forest spawn phase, between players and NPC updates. -/

namespace Lean4Terria.OriginalForest

open OriginalCollision (Grid Vector)
open OriginalForestSpawn (Outcome Rng)
open OriginalPlayerCollision (Body)
open OriginalPlayerMovement (Controls)
open OriginalWorld (Terrain)

/-- `Main.ambientWaterfall*` / `ambientLavafall*` as the last `WaterfallManager.DrawWaterfall` left them
(the static initializer's values until a draw writes them; `ambientLavaStrength` is written only by the
legacy `oldDrawWater` and keeps its initializer value 0). -/
structure AmbientSources where
  waterfallX : Float32 := -1
  waterfallY : Float32 := -1
  waterfallStrength : Float32 := 0
  lavafallX : Float32 := -1
  lavafallY : Float32 := -1
  lavafallStrength : Float32 := 0
  deriving Repr

structure WeatherState where
  dry : OriginalForestClock.DryWeather
  random : UnifiedRandom
  ambience : OriginalForestAmbience.State
  skySpawn : Option OriginalForestAmbience.SkySpawn := none
  windyDay : Bool := false
  /-- The complete weather frame (`OriginalRainWeatherFrame`, used whenever the UpdateTime caller runs):
  thunder statics, both music flags, the `Main.rain` pool and `Collision.honey/shimmer` as rain left them. -/
  thunder : OriginalOuterWeather.Thunder := {}
  music : OriginalRainAtmosphere.MusicState := {}
  rain : OriginalRainAtmosphere.Pool := {}
  wetFlags : OriginalRainAtmosphere.WetFlags := {}
  /-- `Main.Ambience` (il/Main.Ambience.il): its counter and the two loop flags. -/
  ambientCounter : Int := 0
  waterfallPlaying : Bool := false
  lavafallPlaying : Bool := false
  ambientSources : AmbientSources := {}
  /-- `Main.atmo` as the last draw left it (`UpdateAtmosphereTransparencyToSkyColor`); the menu's
  draws leave 1 (`gameMenu`), and the first world update precedes the first world draw. -/
  atmo : Float32 := 1

structure State where
  clock : OriginalForestClock.Clock
  critters : OriginalForestClock.CritterChances
  spawnRandom : Rng
  timeRandom : UnifiedRandom
  slimeRainTime : Float
  fairyLog : Bool
  townNPCs : Nat := 0
  afkCounter : Nat := 0
  playerFlags : List (String × Bool) := []
  candleZones : OriginalCandleBuffs.Zones := {}
  sceneMetrics : Option OriginalSceneWaterStyle.Metrics := none
  zoneBeach : Bool := false
  tick : Nat := 0
  townSpawnCheckTicks : Nat := 0
  ranTownCensus : Bool := false
  /-- `Main.halloween`, `Main.xMas` as `Main.checkHalloween` / `checkXMas` last set them (at world load and
  at every dawn, `UpdateTime_StartDay`); `none` until the caller supplies the date, and again at dawn. -/
  season : Option (Bool × Bool) := none
  /-- The full `Main.UpdateTime` caller (`OriginalTimeCaller`) when the saved weather could be read; the
  legacy dry clock below runs only without it. -/
  timeCaller : Option OriginalTimeCaller.State := none
  starfallBoost : Float32 := 1
  anglerQuest : Option Nat := none
  loadedCloudBackground : Float32 := 0
  lastSpawn : Option Outcome := none
  /-- First unimplemented spawn, NPC, or clock phase. Later spawn attempts continue. -/
  stopped : Option String := none
  timeBoundary : Option OriginalForestClock.OpeningTimeBoundary := none
  weather : Option WeatherState := none
  weatherBoundary : Option OriginalForestClock.OpeningWeatherBoundary := none
  /-- `SceneMetrics.SurfaceAtmospherics`, `ZoneRain`, `ZoneSandstorm` (`CalculateZones` IL_02da..033f) of
  the last scan that did not return early; `Reset` leaves them false. -/
  surfaceAtmospherics : Bool := false
  zoneRain : Bool := false
  zoneSandstorm : Bool := false
  /-- `Main.waterStyle` as the caller last computed it (the scene water style chain); `none`: unknown. -/
  waterStyle : Option Int := none

def State.needsWindHash (state : State) : Bool :=
  state.weatherBoundary.isNone && state.weather.any (fun runtime => runtime.dry.windCounter ≤ 1)

/-- `Main.raining` now: the UpdateTime caller's rain when it runs, else the loaded flag. -/
def State.raining (state : State) (header : OriginalWorld.Header) : Bool :=
  (state.timeCaller.map (·.phase.world.rain.active)).getD header.raining

/-- What DoUpdate's weather block reads from the update's caller: the tile grid, the update's camera
(`Main.screenPosition`, `screenWidth`, `screenHeight`) and the client's graphics settings. -/
structure FrameInputs where
  grid : Grid
  screenPosition : Vector
  screenWidth : Int
  screenHeight : Int
  /-- `Main.AnyDanger(true, false, false)` (boss cache, Moon Lord, invasions, moons, DD2). -/
  anyDanger : Bool
  /-- `Main.gfxQuality`: the static initializer's 1 unless the client's automatic quality lowered it. -/
  gfxQuality : Float32 := 1
  /-- `Main.FlashyEffectsWorld`, an option (static initializer: true). -/
  flashyEffectsWorld : Bool := true

/-- `Main.Ambience` (il/Main.Ambience.il). Every 15th call plays the ambient waterfall (legacy 34) or
lavafall (35) loop at the last drawn source, or stops one that plays. Those sounds are not ported:
such a call is a boundary; quiet calls only count. -/
private def ambienceSound (w : WeatherState) : WeatherState × Option String :=
  let counter := w.ambientCounter + 1
  if counter < 15 then ({ w with ambientCounter := counter }, none) else
  let w := { w with ambientCounter := 0 }
  if w.ambientSources.waterfallStrength > 0 || w.waterfallPlaying then
    (w, some "Main.Ambience: ambient waterfall sound (legacy 34) not ported")
  else if w.ambientSources.lavafallStrength > 0 || w.lavafallPlaying then
    (w, some "Main.Ambience: ambient lavafall sound (legacy 35) not ported")
  else (w, none)

/-- `Main.dayRate` as the last `UpdateTimeRate` left it (a function of the persisted phase flags). -/
def State.dayRate (state : State) : Int :=
  (state.timeCaller.map fun tc => OriginalTimeCaller.updateTimeRate tc.phase false).getD 1

/-- The live `RainContext` of `Rain.MakeRain` / `Rain.Update` (INTERFACE of the rain release). -/
def State.rainContext (state : State) (header : OriginalWorld.Header) (body : Body) (runtime : WeatherState)
    (screenPosition : Vector) (screenWidth screenHeight : Int) (anyDanger : Bool) (gfxQuality : Float32)
    (cloudAlpha : Float32) : OriginalRainAtmosphere.RainContext :=
  { zoneRain := state.zoneRain, cloudAlpha, gfxQuality, atmosphere := runtime.atmo
    snowTiles := (state.sceneMetrics.map (·.snowTileCount)).getD 0
    snowThreshold := OriginalSceneWaterStyle.snowTileThreshold false
    waterStyle := state.waterStyle.getD 0, windCurrent := runtime.dry.windCurrent
    screenPosition, screenWidth, screenHeight, maxWorldViewX := 1920
    playerPosition := body.position, playerVelocity := body.velocity
    remixWorld := header.remixWorld, anyDanger }

/-- One complete DoUpdate weather block (il/Main.DoUpdate.il IL_0959..IL_0aab) through the released
`OriginalRainWeatherFrame`: UpdateWindyDayState, AmbienceServer, flashes, golf, MakeRain, updateCloudLayer,
UpdateWeather, UnpausedUpdateSeed, Ambience, snowing, Sandstorm.EmitDust, on the unscoped `Main.rand`.
Callees: `AmbienceServer.Update` is `OriginalForestAmbience.step`; `UpdateFlashValues` only decays
background-change flash powers (no draw, no consumer here), `GolfState.Update` with no golf ball only clears
its wait flag, `UnpausedUpdateSeed` has no consumer in the session; `Main.Ambience` above. Boundaries:
thunder (Sound43), lightning (`Lighting.LightTiles`), a wind reset without a fresh GUID hash, `snowing`
with snow at the surface, sandstorm dust, a graveyard (its visual intensity), `Main.waterStyle` unknown
when rain is made. -/
def State.advanceWeatherFrame (state : State) (header : OriginalWorld.Header) (body : Body)
    (lifeMax : Nat) (windGuidHash : Option Int32) (tc : OriginalTimeCaller.State) (runtime : WeatherState)
    (inputs : FrameInputs) : State := Id.run do
  let stop := fun (why : String) => { state with stopped := state.stopped.or (some s!"outer weather: {why}") }
  let zone := fun name => (state.playerFlags.lookup name).getD false
  if zone "ZoneGraveyard" then return stop "GraveyardVisualIntensity (graveyard lightning) not modelled"
  let world := tc.phase.world
  if state.waterStyle.isNone && state.zoneRain && world.rain.cloudAlpha > 0 then
    return stop "Main.waterStyle unknown when MakeRain creates drops"
  let rate := state.dayRate
  let snowTiles := (state.sceneMetrics.map (·.snowTileCount)).getD 0
  let desert := (state.sceneMetrics.map (·.desertSandTileCount)).getD 0
  let center := (state.sceneMetrics.map (·.center)).getD (0, 0)
  let outer : OriginalOuterWeather.Context := {
    hasLifeCrystal := lifeMax ≥ 120, windGuidHash, rainStrength := world.rain.strength
    raining := world.rain.active, dayRate := rate, lanternsUp := tc.phase.lanternsUp
    sceneCenter := ⟨center.1, center.2⟩, sceneTileY := (state.sceneMetrics.map (·.tileCenter.2)).getD 0
    worldSurface := header.surface, rockLayer := header.rockLayer, atmosphere := runtime.atmo
    remixWorld := header.remixWorld, flashyEffectsWorld := inputs.flashyEffectsWorld }
  let ctx : OriginalRainWeatherFrame.Context := {
    music := { time := state.clock.time, dayTime := state.clock.dayTime, remixWorld := header.remixWorld }
    rain := state.rainContext header body runtime inputs.screenPosition inputs.screenWidth inputs.screenHeight
      inputs.anyDanger inputs.gfxQuality world.rain.cloudAlpha
    outer }
  let frame : OriginalRainWeatherFrame.State := {
    weather := { weather := runtime.dry, thunder := runtime.thunder }, music := runtime.music
    rain := runtime.rain, cloudAlpha := world.rain.cloudAlpha, random := runtime.random }
  let viewer : OriginalForestAmbience.Player := {
    slot := 0, positionY := body.position.y, desert := zone "ZoneDesert", hallow := zone "ZoneHallow"
    beach := state.zoneBeach, jungle := zone "ZoneJungle", corrupt := zone "ZoneCorrupt", crimson := zone "ZoneCrimson" }
  let mut w := runtime
  let mut ambience := runtime.ambience
  let mut skySpawn : Option OriginalForestAmbience.SkySpawn := none
  let mut result ← match OriginalRainWeatherFrame.beginFrame frame ctx with
    | .ok r => pure r
    | .error why => return stop why
  for _ in [:64] do
    if result.status != .pending then break
    let some p := result.pending | return stop "weather frame pending without a request"
    let fs := result.state
    let mut resumeState := fs
    match p.request with
    | .ambienceServer =>
      let ambientWorld : OriginalForestAmbience.World := {
        time := state.clock.time, dayTime := state.clock.dayTime, worldSurface := header.surface
        underworldLayer := (header.height : Int) - 200, dayRate := rate.toNat, cloudAlpha := fs.cloudAlpha
        windTarget := fs.weather.weather.windTarget, windyDayMusic := fs.music.windy
        butterflyChance := state.critters.butterflyChance, tenthAnniversary := header.tenthAnniversary
        eclipse := header.eclipse, hardMode := header.hardMode, bloodMoon := header.bloodMoon }
      let step := OriginalForestAmbience.step ambience ambientWorld #[viewer] fs.random
      ambience := step.state
      skySpawn := step.spawn
      resumeState := { fs with random := step.random }
    | .backgroundFlashes | .golfState | .unpausedUpdateSeed => pure ()
    | .ambience =>
      let (next, why) := ambienceSound w
      w := next
      if let some why := why then return stop why
    | .snowing =>
      if !header.remixWorld && snowTiles > 0 && state.surfaceAtmospherics then
        return stop "Main.snowing with snow at the surface not ported"
    | .sandstormEmitDust =>
      if world.sandstorm.active && state.zoneSandstorm && desert ≥ 100 then
        return stop "Sandstorm.EmitDust not ported"
    | .outer .windEntropy => return stop "wind reset without a fresh GUID hash"
    | .outer (.sound43 _) => return stop "thunder (Sound43) not ported"
    | .outer (.lightTiles _) => return stop "lightning: Lighting.LightTiles not wired"
    match OriginalRainWeatherFrame.resumeFrame resumeState ctx inputs.grid p.continuation with
    | .ok r => result := r
    | .error why => return stop why
  if result.status != .finishedWeather then return stop s!"weather frame ended {reprStr result.status}"
  let fs := result.state
  return { state with weather := some { w with
    dry := fs.weather.weather, thunder := fs.weather.thunder, music := fs.music, rain := fs.rain
    random := fs.random, ambience, skySpawn, windyDay := fs.music.windy } }

/-- `UpdateWindyDayState` reads the existing target before `updateCloudLayer`
and `UpdateWeather` run in the same update. With the UpdateTime caller the complete weather frame
runs (`advanceWeatherFrame`); the legacy dry stage below is kept for sessions without it. -/
def State.advanceWeather (state : State) (header : OriginalWorld.Header) (body : Body)
    (lifeMax : Nat) (windGuidHash : Option Int32) (frame : Option FrameInputs := none) : State :=
  if state.weatherBoundary.isSome then state else
  match state.weather, state.timeCaller with
  | none, _ => state
  | some runtime, some tc =>
    match frame with
    | some inputs => state.advanceWeatherFrame header body lifeMax windGuidHash tc runtime inputs
    | none => { state with stopped := state.stopped.or (some "outer weather: the update's grid and camera were not supplied") }
  | some runtime, none =>
  -- The dry stage below is the original's outer weather only without rain.
  if header.raining then
    { state with stopped := state.stopped.or (some "outer weather under rain without the UpdateTime caller") }
  else
    let windyDay := OriginalForestAmbience.windyDayMusic state.clock 0 runtime.dry.windTarget runtime.windyDay
    let ambientWorld : OriginalForestAmbience.World := {
      time := state.clock.time, dayTime := state.clock.dayTime, worldSurface := header.surface
      underworldLayer := (header.height : Int) - 200, windTarget := runtime.dry.windTarget
      windyDayMusic := windyDay, butterflyChance := state.critters.butterflyChance
      tenthAnniversary := header.tenthAnniversary, eclipse := header.eclipse
      hardMode := header.hardMode, bloodMoon := header.bloodMoon }
    let zone := fun name => (state.playerFlags.lookup name).getD false
    let viewer : OriginalForestAmbience.Player := {
      slot := 0, positionY := body.position.y, desert := zone "ZoneDesert", hallow := zone "ZoneHallow"
      beach := state.zoneBeach, jungle := zone "ZoneJungle", corrupt := zone "ZoneCorrupt", crimson := zone "ZoneCrimson" }
    let ambience := OriginalForestAmbience.step runtime.ambience ambientWorld #[viewer] runtime.random
    let inputs : OriginalForestClock.DryWeatherInputs := { hasLifeCrystal := lifeMax ≥ 120, windGuidHash }
    match OriginalForestClock.advanceDryWeather runtime.dry ambience.random inputs with
    | .ok (dry, random) =>
      { state with weather := some { runtime with dry, random, ambience := ambience.state, skySpawn := ambience.spawn, windyDay } }
    | .error why => { state with weatherBoundary := some why }

private def scanLogs (terrain : Terrain) : Bool := Id.run do
  let header := terrain.header
  let top := if header.remixWorld then header.height - 350 else header.surface.toUInt64.toNat - 10
  let bottom := if header.remixWorld then header.rockLayer.toUInt64.toNat else 100
  if top < bottom then return false
  for i in [:(header.width - 200 + 2) / 3] do
    for j in [:(top - bottom) / 2 + 1] do
      if (terrain.tile? (100 + 3 * i) (top - 2 * j)).any (fun t => t.id == some 488 && t.liquid.isNone) then
        return true
  return false

/-- Refresh the spawn inputs read by Player.UpdateBiomes on the next tick.

This is one `SceneMetrics.Scan` of the shared `_playerSceneMetrics`. The defaults are
`Player.UpdateSceneMetrics` (no visual area, no PerspectivePlayer when `selectedItem` is 0),
which the headless host calls once per update. The desktop client instead reaches
`Main.UpdateSceneMetrics` from Lighting state 1 and supplies the visual area, the held item
type and the real `GameUpdateCount`. -/
def State.scanScene (state : State) (terrain : Terrain) (body : Body)
    (combat : OriginalCombat.State) (selectedItem : Nat := 0)
    (visualArea : Option OriginalSceneWaterStyle.Rect := none) (skyblockLowTiles : Bool := false)
    (gameUpdateCount : Option Nat := none) : State :=
  let header := terrain.header
  let world := OriginalSceneWaterStyle.World.ofTerrain terrain header.infectedSeed skyblockLowTiles
  let center := OriginalSceneWaterStyle.playerCenter (body.position.x, body.position.y) body.width body.height
  let count := gameUpdateCount.getD state.tick
  let cached := state.sceneMetrics.any fun m =>
    m.lastScanTime == count && m.center.1 == center.1 && m.center.2 == center.2
  let m := OriginalSceneWaterStyle.scan world state.sceneMetrics count center visualArea
  -- Scan's same-tick/center early return precedes AddPlayerEffects too.
  let m := if cached then m else { m with candleZones := m.candleZones.withHeld selectedItem }
  let (_, graveyard) := OriginalRecipeCatalog.biomeFlagsAt terrain body
  let dungeonWall := (terrain.tile? m.tileCenter.1.toNat m.tileCenter.2.toNat).any
    (fun tile => OriginalWallWork.Tables.wallDungeon tile.wall)
  let dungeonDepth := if header.drunkWorld then max header.surface (Float.ofInt (header.dungeon.2 + 40))
    else header.surface
  let flags := [
    ("ZoneCorrupt", m.zoneCorrupt), ("ZoneCrimson", m.zoneCrimson), ("ZoneHallow", m.zoneHallow),
    ("ZoneJungle", m.zoneJungle), ("ZoneSnow", m.zoneSnow), ("ZoneDesert", m.zoneDesert),
    ("ZoneGlowshroom", m.zoneGlowshroom), ("ZoneGraveyard", graveyard),
    ("ZoneMeteor", m.meteorTileCount ≥ 75),
    ("ZoneDungeon", dungeonWall && Float.ofInt m.tileCenter.2 > dungeonDepth && m.dungeonTileCount ≥ 250),
    -- `CalculateZones` IL_01d2..01dd: the centre tile's wall is the Lihzahrd brick wall.
    ("ZoneLihzhardTemple", (terrain.tile? m.tileCenter.1.toNat m.tileCenter.2.toNat).any (·.wall == 87)) ]
  let townNPCs := OriginalForestGuide.townCount ⟨center.1, center.2⟩ combat.residents
  -- `CalculateZones` IL_02da..033f: `WorldGen.IsSurfaceForAtmospherics(TileCenter)` (remix: between the
  -- rock layer and the underworld; else not below the surface), cleared in a remix dungeon; ZoneRain needs
  -- `Main.raining`, ZoneSandstorm a desert with `Sandstorm.Happening`, and a sandstorm clears ZoneRain.
  let (surfaceAtmospherics, zoneRain, zoneSandstorm) := if cached then
      (state.surfaceAtmospherics, state.zoneRain, state.zoneSandstorm)
    else
      let y := Float.ofInt m.tileCenter.2
      let surface := if header.remixWorld then y > header.rockLayer && m.tileCenter.2 < (header.height : Int) - 350
        else !(y > header.surface)
      let surface := surface && !(header.remixWorld && flags.lookup "ZoneDungeon" == some true)
      let raining := match state.timeCaller with
        | some tc => tc.phase.world.rain.active
        | none => header.raining
      let happening := state.timeCaller.any (·.phase.world.sandstorm.active)
      let sandstorm := m.zoneDesert && surface && happening
      (surface, raining && surface && !sandstorm, sandstorm)
  { state with
    townNPCs, playerFlags := flags, zoneBeach := m.zoneBeach
    candleZones := m.candleZones, sceneMetrics := some m
    surfaceAtmospherics, zoneRain, zoneSandstorm }

def start (terrain : Terrain) (combat : OriginalCombat.State) (body : Body)
    (critters : OriginalForestClock.CritterChances) : State :=
  let h := terrain.header
  let seed := OriginalWorld.numericSeed h.seed
  let state : State := {
    clock := ⟨h.time, h.dayTime, h.moonPhase⟩
    critters, spawnRandom := Rng.seed seed, timeRandom := UnifiedRandom.seed seed
    slimeRainTime := h.slimeRainTime
    fairyLog := scanLogs terrain }
  state.scanScene terrain body combat

private def tileSolid : Array Bool := Array.ofFn (n := 754) fun id =>
  id.val != 379 && (OriginalTileRules.collision id.val.toUInt16).solid

private def tileSolidTop : Array Bool := Array.ofFn (n := 754) fun id =>
  (OriginalTileRules.collision id.val.toUInt16).solidTop

private def wallHouse : Array Bool := Array.ofFn (n := OriginalWallWork.Tables.wallCount) fun id =>
  OriginalWallWork.Tables.wallHouse id.val.toUInt16

private def inheritBelow : Array Bool :=
  ((Array.replicate 754 false).set! 421 true).set! 422 true

private def desertWalls : Array Bool := Array.ofFn (n := OriginalWallWork.Tables.wallCount) fun id =>
  id.val == 187 || (216 ≤ id.val && id.val ≤ 223)

private def spawnTile (terrain : Terrain) (x y : Int) : Option OriginalForestSpawn.Tile := do
  if x < 0 || y < 0 then none else
  let tile ← terrain.tile? x.toNat y.toNat
  let liquidType := match tile.liquid.map (·.kind) with
    | some .lava => 1
    | some .honey => 2
    | some .shimmer => 3
    | _ => 0
  return { type := (tile.id.getD 0).toNat, active := tile.id.isSome, inActive := tile.actuated
           liquid := (tile.liquid.map (·.amount.toNat)).getD 0, liquidType, wall := tile.wall.toNat }

/-- Month and day of a local `DateTime` given as .NET ticks (proleptic Gregorian, H. Hinnant). -/
def civilDate (ticks : Int) : Nat × Nat :=
  let z := ticks / 864000000000 - 719162 + 719468
  let era := (if z ≥ 0 then z else z - 146096) / 146097
  let doe := z - era * 146097
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let day := doy - (153 * mp + 2) / 5 + 1
  let month := if mp < 10 then mp + 3 else mp - 9
  (month.toNat, day.toNat)

/-- `Main.checkHalloween` / `checkXMas` for a local `DateTime.Now` given as .NET ticks
(IL `.lake/forest-desktop-integration/death-runtime/il/Main.{isHalloweenDateNow,checkHalloween,checkXMas}.il`):
Halloween from 10 October through 1 November, Christmas from 15 to 31 December, or forced by the world
flags `LoadWorldFlags` read. -/
def seasonAt (ticks : Int) (header : OriginalWorld.Header) : Bool × Bool :=
  -- days since 0001-01-01, then the proleptic Gregorian civil date (days from 1970-01-01, H. Hinnant)
  let z := ticks / 864000000000 - 719162 + 719468
  let era := (if z ≥ 0 then z else z - 146096) / 146097
  let doe := z - era * 146097
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let day := doy - (153 * mp + 2) / 5 + 1
  let month := if mp < 10 then mp + 3 else mp - 9
  let halloweenDate := (day ≥ 10 && month == 10) || (day ≤ 1 && month == 11)
  let xMasDate := day ≥ 15 && month == 12
  (halloweenDate || header.forceHalloweenForToday || header.forceHalloweenForever,
   xMasDate || header.forceXMasForToday || header.forceXMasForever)

private def World.ofTerrain (state : State) (terrain : Terrain)
    (combat : OriginalCombat.State) (statLifeMax : Option Int := none) : OriginalForestSpawn.World :=
  let h := terrain.header
  let flags := [
    ("bloodMoon", h.bloodMoon), ("downedGoblins", h.downedGoblins),
    ("drunkWorld", h.drunkWorld), ("eclipse", h.eclipse), ("getGoodWorld", h.forTheWorthy),
    ("dontStarveWorld", h.dontStarveWorld), ("hardMode", h.hardMode), ("infectedSeed", h.infectedSeed),
    ("skyblock", h.skyblock), ("raining", state.raining h), ("remixWorld", h.remixWorld),
    ("slimeRain", state.slimeRainTime > 0), ("tenthAnniversaryWorld", h.tenthAnniversary),
    ("zenithWorld", h.zenithWorld), ("invasionType!=0", h.invasionType != 0),
    ("gameMode!=Classic", h.gameMode != 0) ]
  { tile := spawnTile terrain, maxTilesX := h.width, maxTilesY := h.height,
    worldSurface := h.surface, rockLayer := h.rockLayer, underworldLayer := (h.height : Int) - 200
    spawnTileX := h.spawnX, sWidth := 1920, sHeight := 1200
    dayTime := state.clock.dayTime, time := state.clock.time,
    windSpeedTarget := (state.weather.map (·.dry.windTarget)).getD h.windSpeedTarget
    windyDay := state.weather.any (·.windyDay)
    cloudCount := (state.weather.map (·.dry.clouds)).getD h.cloudCount
    cloudBackground := (state.weather.map (·.dry.cloudBackground.active)).getD state.loadedCloudBackground
    starfallBoost := state.starfallBoost
    oceanDistance := 250, beachDistance := 380, savedAngler := h.savedAngler
    savedMechanic := h.savedMechanic || combat.mechanicRescued
    downedSkeletron := h.downedSkeletron || combat.skeletronDefeated
    unlockedSlimeOldSpawn := h.unlockedSlimeOldSpawn
    stinkBugChance := state.critters.stinkBugChance, butterflyChance := state.critters.butterflyChance
    fireFlyFriendly := state.critters.fireFlyFriendly, fireFlyMultiple := state.critters.fireFlyMultiple
    goldCritterChance := OriginalForestClock.goldCritterChance, fairyLog := state.fairyLog
    readyToFindBartender := false, isThereAWorldSurface := true, expertMode := false
    activeNPCTypes := (combat.actors.filter (·.active)).toList.map (·.typeId)
    nearSpikeBall := fun x y => OriginalDungeonSpikeBall.nearSpikeBall x y
      (combat.actors.filterMap (·.dungeonSpikeBall?))
    noSpawnCycle := combat.noSpawnCycle, tileSolid, tileSolidTop, wallHouse
    inheritTypeOfTileBelow := inheritBelow, desertWalls, offFlags := flags
    night := do
      let (halloween, xMas) ← state.season
      let lifeMax ← statLifeMax
      return { moonPhase := state.clock.moonPhase, fireFlyChance := state.critters.fireFlyChance
               halloween, xMas, playerStatLifeMax := lifeMax } }

def critterWorld (world : OriginalGreenSlime.World) (header : OriginalWorld.Header) (raining : Bool := header.raining) :
    OriginalForestCritter.World :=
  { grid := world.grid, worldSurface := world.worldSurface, bottomWorld := (header.height * 16).toFloat32
    dayTime := world.dayTime, raining, eclipse := header.eclipse,
    slimeRain := header.slimeRainTime > 0, expertMode := header.gameMode == 1,
    masterMode := header.gameMode == 2, frameHeight := 40 }

private def State.applySpawns (state : State) (combat : OriginalCombat.State)
    (world : OriginalGreenSlime.World) (header : OriginalWorld.Header) (spawns : List (Int × Int × Int)) :
    State × OriginalCombat.State := Id.run do
  let mut combat := combat
  for (id, x, y) in spawns do
    let some slot := combat.firstSpawnSlot | continue
    match id with
    | -3 | 1 | -7 | -8 | -9 =>
      combat := combat.setActor slot (.greenSlime (OriginalGreenSlime.spawn world x y (netId := id)))
    | 32 =>
      if let some enemy := OriginalDarkCaster.spawn id x y then
        combat := combat.setActor slot (.darkCaster enemy)
    | 71 => combat := combat.setActor slot (.dungeonSlime (OriginalDungeonSlime.spawn x y))
    | 70 => combat := combat.setActor slot (.dungeonSpikeBall (OriginalDungeonSpikeBall.spawn x y))
    | 34 =>
      if let some enemy := OriginalCursedSkull.spawn id x y then
        combat := combat.setActor slot (.cursedSkull enemy)
    | 46 => combat := combat.setActor slot (.forestBunny (OriginalForestCritter.spawn (critterWorld world header (state.raining header)) slot x y))
    | 359 => combat := combat.setActor slot (.forestSnail (OriginalForestSnail.spawn (critterWorld world header (state.raining header)) slot x y))
    | 68 => combat := combat.setActor slot (.skeletronPart (OriginalSkeletron.spawnGuardian slot x y world.activeTime.toNat))
    | 123 => combat := combat.setActor slot (.boundMechanic (OriginalDungeonMechanic.spawn world x y))
    | 685 => combat := combat.setActor slot (.oldShakingChest (OriginalOldSlime.spawn x y))
    -- Base Zombie / Demon Eye only; variants (other net IDs) stop below.
    | 3 | 2 =>
      if let some enemy := OriginalNightEnemies.spawn world.grid id x y then
        combat := combat.setActor slot (.nightEnemy enemy)
    | _ =>
      if let some enemy := OriginalAngryBones.spawn id x y then
        combat := combat.setActor slot (.angryBones enemy)
      else if let some kind := OriginalForestBird.Kind.ofTypeId id then
        combat := combat.setActor slot (.forestBird (OriginalForestBird.spawn world kind x y))
      else
        return ({ state with stopped := state.stopped.or (some s!"NPC {id} at ({x},{y}) has no opening update") }, combat)
  return (state, combat)

def State.trackAfk (state : State) (body : Body) (dead : Bool) (itemAnimation : Nat)
    (input : Controls) : State :=
  let afkCounter := if !dead && body.isConsideredStandingStill && itemAnimation == 0 &&
    !(input.left || input.right || input.jump || input.down) then state.afkCounter + 1
    else if dead then state.afkCounter else 0
  { state with afkCounter }

def State.spawn (state : State) (terrain : Terrain) (world : OriginalGreenSlime.World)
    (body : Body) (dead : Bool) (heldItem : UInt16)
    (combat : OriginalCombat.State) (luck : Float32 := 0) (statLifeMax : Option Int := none) :
    State × OriginalCombat.State :=
  let previous := combat
  let combat := { combat with noSpawnCycle := false }
  let state := { state with
    tick := state.tick + 1, lastSpawn := none
    stopped := state.stopped.or (combat.npcBoundary.map fun (slot, why) => s!"NPC update at slot {slot}: {why}") }
  let player : OriginalForestSpawn.Player := {
    whoAmI := 0, active := true, dead, position := (body.position.x, body.position.y)
    width := body.width, height := body.height, nearbyActiveNPCs := combat.nearbyActiveNPCs
    townNPCs := state.townNPCs, heldItem := heldItem.toNat, afkCounter := state.afkCounter, luck
    candleZones := state.candleZones
    offFlags := state.playerFlags }
  let (result, trace) := OriginalForestSpawn.attempt (World.ofTerrain state terrain previous statLifeMax) player state.spawnRandom
  let state := { state with spawnRandom := trace.rng }
  match result with
  | .error why => ({ state with stopped := state.stopped.or (some (reprStr why)) }, combat)
  | .ok outcome =>
    let state := { state with lastSpawn := some outcome }
    match outcome with
    | .spawned spawns => state.applySpawns combat world terrain.header spawns
    | _ => (state, combat)

def State.advanceTime (state : State) (header : OriginalWorld.Header)
    (lifeMax travelTownCount : Nat) : State :=
  let (clock, transition) := OriginalForestClock.advance state.clock
  -- UpdateTime_SpawnTownNPCs runs on the daytime branch, including the tick
  -- that starts night. The supported opening uses a world update rate of one.
  let townSpawnCheckTicks := match transition with
    | .dawn => 0
    | .dusk => 1
    | .none => state.townSpawnCheckTicks + (if state.clock.dayTime then 1 else 0)
  let checkTownSpawns := townSpawnCheckTicks == 7200
  let state := { state with
    townSpawnCheckTicks := if checkTownSpawns then 0 else townSpawnCheckTicks
    ranTownCensus := false
    -- `UpdateTime_StartDay` calls `checkHalloween` / `checkXMas` again with the date of this update
    season := if transition == OriginalForestClock.Transition.dawn then none else state.season }
  if state.timeBoundary.isSome then { state with clock }
  else
    let windCurrent := (state.weather.map (·.dry.windCurrent)).getD header.windSpeedTarget
    let input : OriginalForestClock.DryTimeInputs := { lifeMax, windCurrent, slimeRainTime := state.slimeRainTime, travelTownCount }
    let advance := if header.raining then .error .weather
      else OriginalForestClock.advanceForestTime header state.clock input state.timeRandom
    match advance with
    | .ok step =>
      -- The census rolls for Party Girl even when the population is below 20.
      let timeRandom := if checkTownSpawns then (step.random.next 0 40).2 else step.random
      { state with
        clock := step.clock, slimeRainTime := step.slimeRainTime, timeRandom
        ranTownCensus := checkTownSpawns
        critters := step.duskCritters.getD state.critters
        starfallBoost := step.starfallBoost.getD state.starfallBoost
        anglerQuest := step.anglerQuest.or state.anglerQuest }
    | .error why =>
      { state with
        clock, timeBoundary := some why
        stopped := state.stopped.or (some s!"UpdateTime: {reprStr why}") }

end Lean4Terria.OriginalForest
