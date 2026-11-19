import Init

/-!
# Ordinary surface night spawn selection (Terraria desktop 1.4.5.8)

`NPC.Spawner.SpawnAnNPC(x, y, tileType, wallType, inSafeX, target)` from IL_45d5 (the point after
the dungeon branch) to its end, for a NON-friendly spawn at NIGHT whose tile is on the surface
(`y ≤ Main.worldSurface`). Transcribed by hand from the current desktop CIL
(`.lake/original-spawn-luck/dump-9vsft6_v/il/Spawner.SpawnAnNPC-100679344.il`, desktop sha
`d52c7ea7…3e0`), with `GetZombieSettings`, `GetGnomeChance`, `CheckToSpawnUndergroundFairy` and
`CheckToSpawnUndergroundGnomes`. Evidence: `.lake/original-surface-night-spawn/`.

Order of the night chain (each site named by its IL offset):
`IL_45d5` meteor / DD2 → `IL_4630` snow moon → `IL_5de5` pumpkin moon → `IL_7066` eclipse (day
only) → `IL_747f` fairy check → `IL_74f1` living-tree gnome (night form) → `IL_75ac` gnome check →
tile / wall / zone branches `IL_7629 … IL_96a4` → `IL_96af` surface (`surfaceSpawn`) → graveyard,
snow zone, hardmode → `IL_98c1` night jumps to `IL_aa39` → fireflies → Halloween 301 → Demon Eye or
zombie families. Branches outside the ordinary dry inland forest night are typed `Stop.boundary`
values naming their site, never silently skipped.

The module does not own the random stream, luck, NPC counts or the NPC allocator: the caller passes
them as `Ops` and runs them, in the order requested, on the real `Main.rand` that `NPC.SpawnNPC`
swapped in. Every birth is requested at the moment the original calls `Spawner.SpawnNPC`, so draws
made by the allocator or by later rolls stay in original order.
-/
namespace Lean4Terria.OriginalSurfaceNightSpawn

/-! ## Caller operations -/

/-- One `Spawner.SpawnNPC(x, y, type, 0, ai0, ai1, ai2, ai3, 255)` call: pixel coordinates are the
NPC's `Bottom`. `targetClosest` is the fairy's following `NPC.TargetClosest(true)`. -/
structure Birth where
  typeId : Int
  x : Int
  y : Int
  ai2 : Float32 := 0
  targetClosest : Bool := false
  deriving Repr, BEq

/-- The requests the leaf makes, performed immediately by the caller on its real state:
`next` = `Main.rand.Next(max)`, `nextRange` = `Main.rand.Next(min, max)`, `rollLuck` /
`rollOnlyBadLuck` = `Spawner.RollLuck` / `RollOnlyBadLuck` (`Terraria.GameContent.Luck` with
`Spawner.luck`), `countNPCs t` = `NPC.CountNPCS(t)` now, `anyHelpfulFairies` =
`NPC.AnyHelpfulFairies()` now, `spawn site b` = `Spawner.SpawnNPC` now (including its own
`RollLuck(180)` pinky roll and `NPC.NewNPC`; none of the types here has netID type 1). -/
structure Ops (m : Type → Type) where
  next : String → Int → m Int
  nextRange : String → Int → Int → m Int
  rollLuck : String → Int → m Int
  rollOnlyBadLuck : String → Int → m Int
  countNPCs : Int → m Int
  anyHelpfulFairies : m Bool
  spawn : String → Birth → m Unit

/-! ## `GetZombieSettings` -/

/-- The four out-parameters of `Spawner.GetZombieSettings`. -/
structure ZombieSettings where
  family : Int
  canSpawnArmed : Bool
  torchZombieChance : Int
  maggotZombieChance : Int
  deriving Repr, BEq

/-- `GetZombieSettings` after its `Main.rand.Next(7)` (IL_0016), which `SpawnAnNPC` makes at IL_0009,
before any other gate, day or night; the caller makes that ONE draw and passes its result here.
Requires `Skyblock.lowTiles` false (its extra `Next(3)` draws follow IL_0016 immediately and are a
boundary). `playerHasStartingHealth` is `Spawner.playerHasStartingHealth`
(`SetSpawnFlags` IL_035f: `!(player.statLifeMax > 100)`); `numberOfActivePlayers` is
`Spawner.numberOfActivePlayers` (`Main.CurrentFrameFlags.ActivePlayersCount` at construction). -/
def zombieSettings (family : Int) (playerHasStartingHealth : Bool) (numberOfActivePlayers : Int) :
    ZombieSettings :=
  let torch := if playerHasStartingHealth then
      let t := 5 - numberOfActivePlayers.tdiv 2
      if t < 2 then 2 else t
    else 12
  { family, canSpawnArmed := true, torchZombieChance := torch, maggotZombieChance := 20 }

/-! ## Context -/

/-- Everything the night chain reads besides the stream. `Spawner.*` fields are those
`SetSpawnFlags` / `SetSpawnFlagsForChosenTile` left for this attempt. -/
structure Context where
  /-- Spawn tile (`x`, `y`) and the `tileType` / `wallType` arguments of `SpawnAnNPC`. -/
  x : Int
  y : Int
  tileType : Nat
  wallType : Nat
  /-- `Main.tile[x, y].wall` (read by `CheckToSpawnUndergroundGnomes`). -/
  wallAtTile : Nat
  worldSurface : Float
  rockLayer : Float
  maxTilesY : Int
  /-- `Main.dayTime`; the leaf refuses to run by day. -/
  dayTime : Bool
  /-- `Spawner.waterTile`, `Spawner.livingTree`, `Spawner.raining` (= `Main.raining` at SetSpawnFlags). -/
  waterTile : Bool
  livingTree : Bool
  raining : Bool
  /-- `Spawner.fairyLog`. -/
  fairyLog : Bool
  moonPhase : Int
  halloween : Bool
  xMas : Bool
  expertMode : Bool
  /-- `NPC.TooWindyForButterflies` (SpawnAnNPC IL_0042). -/
  tooWindyForButterflies : Bool
  fireFlyChance : Int
  fireFlyMultiple : Int
  /-- `TileID.Sets.IcesSnow[tileType]`. -/
  icesSnow : Bool
  /-- `Spawner.ZoneCorrupt`, `ZoneCrimson` (read at IL_758e, IL_7a07, IL_9084, IL_9437). -/
  zoneCorrupt : Bool := false
  zoneCrimson : Bool := false
  /-- Settings from the entry draw. -/
  settings : ZombieSettings
  /-- Flags that must be false in this scope, by game name: `Main.hardMode`, `Main.bloodMoon`,
  `Main.snowMoon`, `Main.pumpkinMoon`, `Main.eclipse`, `Main.remixWorld`,
  `Main.tenthAnniversaryWorld`, `WorldGen.Skyblock.lowTiles`, `Spawner.ZoneGraveyard`,
  `Spawner.ZoneMeteor`, `Spawner.ZoneSnow`, `Spawner.ZoneJungle`, `Spawner.ZoneLihzhardTemple`,
  `DD2Event.Ongoing && Spawner.ZoneOldOneArmy`, `Sandstorm.Happening && Spawner.ZoneSandstorm`,
  `Spawner.spawnFriendly`, `Spawner.ZoneDungeon`. -/
  offFlags : List (String × Bool) := []

inductive Stop where
  /-- A branch outside this leaf (other biome, event, hardmode, special seed, day, below surface). -/
  | boundary (site : String) (why : String)
  deriving Repr, BEq

inductive Outcome where
  /-- The births requested, in order (each was already performed through `Ops.spawn`). -/
  | spawned (births : List Birth)
  /-- `SpawnAnNPC` returned without a birth. -/
  | nothing (site : String)
  deriving Repr, BEq

private def site (il : String) : String := s!"Spawner::SpawnAnNPC {il}"

/-- The tile / wall / zone branches `IL_7629 … IL_9468` an ordinary forest tile falls through. Each
returns the site and reason of the boundary it enters, in CIL order, or none. -/
def tileBoundary (c : Context) : Option (String × String) :=
  let t := c.tileType
  if t == 70 then some ("IL_762c", "mushroom tile")
  else if c.zoneCorrupt then some ("IL_7a0d", "corruption zone")
  else if t == 60 then some ("IL_7e7f", "jungle tile")
  else if t == 225 then some ("IL_82ba", "hive tile")
  else if t == 226 || t == 232 then some ("IL_871f", "temple tile")
  else if c.wallType == 86 then some ("IL_87b3", "hive wall")
  else if (t == 204 && c.zoneCrimson) || [199, 200, 203, 234, 662].contains t then some ("IL_9084", "crimson tile")
  else if (t == 22 && c.zoneCorrupt) || [23, 25, 112, 163, 661].contains t then some ("IL_9437", "corruption tile")
  else none

/-- `Spawner.CheckToSpawnUndergroundFairy(x, y)` with `tenthAnniversaryWorld` and `hardMode` off:
`RollLuck(500)` when `fairyLog`, then the depth band and `NPC.AnyHelpfulFairies`. -/
def checkFairy [Monad m] (ops : Ops m) (c : Context) : m Bool := do
  if !c.fairyLog then return false
  if (← ops.rollLuck "Spawner::CheckToSpawnUndergroundFairy IL_0036" 500) != 0 then return false
  if Float.ofInt c.y < (c.worldSurface + c.rockLayer) / 2 then return false
  if !(c.y < c.maxTilesY - 300) then return false
  return !(← ops.anyHelpfulFairies)

/-- `Spawner.CheckToSpawnUndergroundGnomes(x, y, allowed, gnomeChance)` at night (eclipse, blood
moon, remix off). -/
def checkGnomes [Monad m] (ops : Ops m) (c : Context) (allowed : Bool) (gnomeChance : Int) : m Bool := do
  if !allowed then return false
  if (← ops.rollLuck "Spawner::CheckToSpawnUndergroundGnomes IL_0018" gnomeChance) != 0 then return false
  let low := c.worldSurface * 0.800000011920929
  let high := c.worldSurface * 1.100000023841858
  if Float.ofInt c.y < low || Float.ofInt c.y > high then return false
  let count ← ops.countNPCs 624
  let roll ← ops.next "Spawner::CheckToSpawnUndergroundGnomes IL_008b" 3
  if count > roll then return false
  -- IL_00a6: the day-only "no wall" rejection is skipped at night; the wall set test remains
  let w := c.wallAtTile
  return w == 2 || w == 63 || (196 ≤ w && w ≤ 199)

/-- The selection from IL_aa39 (fireflies, Halloween 301, Demon Eyes, zombies). Reached only through
`surfaceNight`, which has already stopped for tiles below the surface. -/
private def select [Monad m] (ops : Ops m) (c : Context) : m Outcome := do
  let bx := c.x * 16 + 8
  let by' := c.y * 16
  let born := fun (il : String) (b : Birth) => do ops.spawn (site il) b; pure b
  -- IL_aa49 … IL_abe9: fireflies on grass when calm and dry
  let grass := [2, 477, 109, 492].contains c.tileType
  if !c.tooWindyForButterflies && grass && !c.raining then
    if (← ops.next (site "IL_aa8c") c.fireFlyChance) == 0 && !(Float.ofInt c.y > c.worldSurface) then
      let id : Int := if c.tileType == 109 then 358 else 355
      let mut births := [← born "IL_aab5" { typeId := id, x := bx, y := by' }]
      for (il, spawnIl, dx, dy) in [("IL_aaec", "IL_aaf3", -16, 0), ("IL_ab2d", "IL_ab34", 16, 0),
          ("IL_ab6e", "IL_ab75", 0, -16), ("IL_abaf", "IL_abb9", 0, 16)] do
        if (← ops.next (site il) c.fireFlyMultiple) == 0 then
          births := births ++ [← born spawnIl { typeId := id, x := bx + dx, y := by' + dy }]
      return .spawned births
  -- IL_abea: Halloween Next(12) → 301 (graveyard is a boundary)
  if c.halloween then
    if (← ops.next (site "IL_ac00") 12) == 0 then
      return .spawned [← born "IL_ac07" { typeId := 301, x := bx, y := by' }]
  -- IL_acd4: Demon Eye on Next(6) == 0, or on a new moon (phase 4) also on Next(2) == 0
  let mut eye := (← ops.next (site "IL_acda") 6) == 0
  if !eye && c.moonPhase == 4 then eye := (← ops.next (site "IL_acf2") 2) == 0
  if eye then
    if c.halloween then
      if (← ops.next (site "IL_ad4e") 2) == 0 then
        let t ← ops.nextRange (site "IL_ad5c") 317 319
        return .spawned [← born "IL_ad8e" { typeId := t, x := bx, y := by' }]
    if (← ops.next (site "IL_ad9b") 2) == 0 then
      let t : Int := if (← ops.next (site "IL_ada8") 4) == 0 then -43 else 2
      return .spawned [← born (if t == -43 then "IL_adaf" else "IL_addd") { typeId := t, x := bx, y := by' }]
    let v ← ops.next (site "IL_ae10") 5
    let table : List (Int × String × Int × String × Int × String) :=
      [(0, "IL_ae21", -38, "IL_ae28", 190, "IL_ae55"), (1, "IL_ae91", -39, "IL_ae98", 191, "IL_aec5"),
       (2, "IL_af01", -40, "IL_af08", 192, "IL_af35"), (3, "IL_af71", -41, "IL_af78", 193, "IL_afa5"),
       (4, "IL_afe4", -42, "IL_afeb", 194, "IL_b018")]
    let some (_, rollIl, small, smallIl, base, baseIl) := table.find? (·.1 == v) | return .nothing (site "IL_afd9")
    -- the small eye is an extra call before the base eye at the same position
    if (← ops.next (site rollIl) 3) == 0 then
      let a ← born smallIl { typeId := small, x := bx, y := by' }
      let b ← born baseIl { typeId := base, x := bx, y := by' }
      return .spawned [a, b]
    return .spawned [← born baseIl { typeId := base, x := bx, y := by' }]
  -- IL_b09c / IL_b0e6: two RollOnlyBadLuck(300) draws (their spawns need blood moon or graveyard)
  let _ ← ops.rollOnlyBadLuck (site "IL_b0a2") 300
  let _ ← ops.rollOnlyBadLuck (site "IL_b0ec") 300
  let s := c.settings
  -- IL_b253: ice and snow tiles (or 162)
  if c.icesSnow || c.tileType == 162 then
    if s.canSpawnArmed && c.expertMode then
      if (← ops.next (site "IL_b311") 2) == 0 then
        return .spawned [← born "IL_b318" { typeId := 431, x := bx, y := by' }]
    return .spawned [← born "IL_b349" { typeId := 161, x := bx, y := by' }]
  -- IL_b37a: rain
  if c.raining then
    if (← ops.next (site "IL_b38b") 2) == 0 then
      if (← ops.next (site "IL_b39b") 3) != 0 then
        return .spawned [← born "IL_b3a2" { typeId := 223, x := bx, y := by' }]
      let t : Int := if (← ops.next (site "IL_b3d9") 2) == 0 then -54 else -55
      return .spawned [← born (if t == -54 then "IL_b3e0" else "IL_b40e") { typeId := t, x := bx, y := by' }]
  -- IL_b5cc: torch zombie
  if (← ops.next (site "IL_b5d2") s.torchZombieChance) == 0 then
    if s.canSpawnArmed && c.expertMode then
      if (← ops.next (site "IL_b5e9") 2) == 0 then
        return .spawned [← born "IL_b5f0" { typeId := 591, x := bx, y := by' }]
    return .spawned [← born "IL_b621" { typeId := 590, x := bx, y := by' }]
  -- IL_b652 / IL_b6a6: holiday zombies
  if c.halloween then
    if (← ops.next (site "IL_b65f") 2) == 0 then
      let t ← ops.nextRange (site "IL_b66d") 319 322
      return .spawned [← born "IL_b6a0" { typeId := t, x := bx, y := by' }]
  if c.xMas then
    if (← ops.next (site "IL_b6b3") 2) == 0 then
      let t ← ops.nextRange (site "IL_b6c1") 331 333
      return .spawned [← born "IL_b6f4" { typeId := t, x := bx, y := by' }]
  -- IL_b6fa: armed zombies in expert mode (not for family 1)
  if s.canSpawnArmed && s.family != 1 && c.expertMode then
    if (← ops.next (site "IL_b717") 3) == 0 then
      let t : Int := match s.family with
        | 2 => 432 | 3 => 433 | 4 => 434 | 5 => 435 | 6 => 436 | _ => 430
      return .spawned [← born "IL_b780" { typeId := t, x := bx, y := by' }]
  -- IL_b7ae … IL_b872: the family's zombie, small or big on Next(3) == 0
  let (base, small, big) : Int × Int × Int := match s.family with
    | 1 => (132, -28, -29) | 2 => (186, -30, -31) | 3 => (187, -32, -33) | 4 => (188, -34, -35)
    | 5 => (189, -36, -37) | 6 => (200, -44, -45) | _ => (3, -26, -27)
  let mut t := base
  if (← ops.next (site "IL_b854") 3) == 0 then
    t := if (← ops.next (site "IL_b861") 2) == 0 then small else big
  return .spawned [← born "IL_b872" { typeId := t, x := bx, y := by' }]

/-- `SpawnAnNPC` from IL_45d5 for a non-friendly, non-dungeon attempt at night with a surface tile.
`gnomeChance` is `GetGnomeChance()` (10 without Skyblock / remix). -/
def surfaceNight [Monad m] (ops : Ops m) (c : Context) : m (Except Stop Outcome) := do
  for (name, v) in c.offFlags do
    if v then return .error (.boundary "Spawner::SpawnAnNPC" s!"{name} is set")
  if c.dayTime then return .error (.boundary (site "IL_4630") "daytime")
  if Float.ofInt c.y > c.worldSurface then return .error (.boundary (site "IL_462b") "tile below the surface")
  -- IL_747f
  if ← checkFairy ops c then
    return .error (.boundary (site "IL_7486") "fairy spawn (unreachable on the surface)")
  -- IL_74f1 … IL_758a: night living-tree gnome
  if !c.waterTile && c.livingTree then
    if (← ops.rollLuck (site "IL_753c") (10 * 3)) == 0 then
      let count ← ops.countNPCs 624
      let roll ← ops.next (site "IL_7553") 3
      if !(count > roll) then
        ops.spawn (site "IL_755a") { typeId := 624, x := c.x * 16 + 8, y := c.y * 16 }
        return .ok (.spawned [{ typeId := 624, x := c.x * 16 + 8, y := c.y * 16 }])
  -- IL_758b … IL_75e3
  if ← checkGnomes ops c (!c.zoneCorrupt && !c.zoneCrimson && !c.waterTile) 10 then
    ops.spawn (site "IL_75b3") { typeId := 624, x := c.x * 16 + 8, y := c.y * 16 }
    return .ok (.spawned [{ typeId := 624, x := c.x * 16 + 8, y := c.y * 16 }])
  if let some (il, why) := tileBoundary c then return .error (.boundary (site il) why)
  -- IL_96af … IL_98c6: surface; outer third / AnyDanger only matter in hardmode; night → IL_aa39
  return .ok (← select ops c)

end Lean4Terria.OriginalSurfaceNightSpawn
