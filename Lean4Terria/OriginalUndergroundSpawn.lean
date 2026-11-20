import Lean4Terria.OriginalSurfaceNightSpawn

/-!
# Ordinary underground spawn selection (Terraria desktop 1.4.5.8)

`NPC.Spawner.SpawnAnNPC(x, y, tileType, wallType, inSafeX, target)` from IL_45d5 (the point after
the dungeon branch) to its end, for a NON-friendly spawn, by DAY OR NIGHT, whose tile lies below
the surface line (`y > Main.worldSurface`), in an ordinary pre-hardmode forest underground (no
biome zone). Transcribed by hand from the current desktop CIL
(`.lake/original-spawn-luck/dump-9vsft6_v/il/Spawner.SpawnAnNPC-100679344.il`, desktop sha
`d52c7ea7…3e0`), with `CheckToSpawnUndergroundFairy`, `CheckToSpawnUndergroundGnomes`,
`GetGnomeChance`, `GetBasicSlimeToSpawn` and `CheckToSpawnRockGolem`. Evidence:
`.lake/original-underground-spawn/`.

Below the surface the night and day chains are the same except for one wall test: `IL_462b`,
`IL_5de0` and `IL_706d` all jump to `IL_747c` for a tile below the surface (the surface night,
pumpkin-moon and eclipse blocks are skipped), then:
`IL_747f` fairy → `IL_74f1` living-tree gnome (by day only with a wall behind the tile) →
`IL_75ac` gnome check (its own day wall test) → tile / wall branches (jungle, hive, hive wall,
crimson, corruption tiles are boundaries) → `IL_7f10 … IL_812c` underground critters (worm,
mouse, snail with their gold forms) → `IL_96aa` (not `surfaceSpawn`) → `IL_b8a0`
`Spawner.underGround` (`y ≤ rockLayer`): the dirt layer (giant worm, ice slime, basic slime);
otherwise the cavern layer from `IL_bb4f` (underworld boundary, rock golem, giant shelly 217,
giant worm, mother / black slimes, skeleton merchant, Nymph 195, Tim, marble / granite,
Halloween ghost, undead miner, ice cavern enemies, `NPC.cavernMonsterType`, Halloween
skeletons, expert armed skeletons, the four skeleton families, cave and ice bats).

The two `SetSpawnFlagsForChosenTile` / `SpawnAnNPC` pieces that run BEFORE IL_45d5 and decide the
cavern spiders are here too, as separate entries for the caller: `spiderScan`
(`SetSpawnFlagsForChosenTile` IL_0312 … IL_03f2, the spider-cave wall scan below the rock layer)
and `spiderBranch` (`SpawnAnNPC` IL_107e … IL_11c9).

The module does not own the random stream, luck, NPC counts or the allocator: the caller runs the
`Ops` requests in order on the real `Main.rand` that `NPC.SpawnNPC` swapped in. Every birth is
requested at the moment the original calls `Spawner.SpawnNPC`.
-/
namespace Lean4Terria.OriginalUndergroundSpawn

open OriginalSurfaceNightSpawn (Birth Stop Outcome)

/-! ## Caller operations -/

/-- The released night `Ops` (`next`, `nextRange`, `rollLuck`, `rollOnlyBadLuck`, `countNPCs`,
`anyHelpfulFairies`, `spawn`) plus `anyNPCs t` = `NPC.AnyNPCs(t)` at that moment. `spawn` is
`Spawner.SpawnNPC` including its own `RollLuck(180)` pinky roll for net IDs whose
`NPCID.FromNetId` is 1 (the basic slimes here) and `NPC.NewNPC`; a birth with `targetClosest`
is followed by `NPC.TargetClosest(true)` on the new NPC. -/
structure Ops (m : Type → Type) extends OriginalSurfaceNightSpawn.Ops m where
  anyNPCs : Int → m Bool

/-! ## Context -/

/-- Everything the underground chain reads besides the stream. `Spawner.*` fields are those
`SetSpawnFlags` / `SetSpawnFlagsForChosenTile` left for this attempt. -/
structure Context where
  /-- Spawn tile and the `tileType` / `wallType` arguments of `SpawnAnNPC`. -/
  x : Int
  y : Int
  tileType : Nat
  wallType : Nat
  /-- `Main.tile[x, y].wall` (day wall tests IL_750d and `CheckToSpawnUndergroundGnomes` IL_00a6). -/
  wallAtTile : Nat
  worldSurface : Float
  rockLayer : Float
  maxTilesY : Int
  dayTime : Bool
  /-- `Spawner.waterTile`, `Spawner.livingTree`, `Spawner.noWorms`. -/
  waterTile : Bool
  livingTree : Bool
  noWorms : Bool
  /-- `Spawner.underGround` (`SetSpawnFlagsForChosenTile` IL_02d6: `!(y > rockLayer)`, remix off). -/
  underGround : Bool
  /-- `Spawner.nearMarble` / `nearGranite` (`SetSpawnFlagsForChosenTile` IL_006f … IL_02d1). -/
  nearMarble : Bool := false
  nearGranite : Bool := false
  /-- `Spawner.offensiveToTim` (`SetSpawnFlags`), `Spawner.ZoneShadowCandle`. -/
  offensiveToTim : Bool := false
  zoneShadowCandle : Bool := false
  /-- `Spawner.fairyLog`, `Main.halloween`, `Main.expertMode`. -/
  fairyLog : Bool := false
  halloween : Bool := false
  expertMode : Bool := false
  /-- `NPC.goldCritterChance`. -/
  goldCritterChance : Int
  /-- `NPC.cavernMonsterType[a, b]` (`a < 2`, `b < 3`), chosen per world by `NPC.setWorldMonsters`. -/
  cavernMonsterType : Int → Int → Int
  /-- Flags that must be false in this scope, by game name: `Main.hardMode`, `Main.remixWorld`,
  `Main.tenthAnniversaryWorld`, `Main.getGoodWorld` (with tenth), `Main.bloodMoon`, `Main.eclipse`,
  `WorldGen.Skyblock.lowTiles`, `Spawner.ZoneSnow`, `ZoneJungle`, `ZoneCorrupt`, `ZoneCrimson`,
  `ZoneHallow`, `ZoneGlowshroom`, `ZoneGraveyard`, `ZoneMeteor`, `ZoneLihzhardTemple`,
  `ZoneDungeon`, `DD2Event.Ongoing && ZoneOldOneArmy`, `Sandstorm.Happening && ZoneSandstorm`,
  `Spawner.spawnFriendly`. (Snow / pumpkin moons are skipped below the surface; listing them is
  harmless.) -/
  offFlags : List (String × Bool) := []

private def site (il : String) : String := s!"Spawner::SpawnAnNPC {il}"

/-- `GetGnomeChance()` with `Skyblock.lowTiles` and `remixWorld` off. -/
def gnomeChance : Int := 10

/-! ## Boundaries -/

/-- The tile / wall branches between IL_7629 and IL_96a4 that an ordinary underground tile must
not enter, as the site and reason of the first one in CIL order. Tile 70 (mushroom grass), temple
tiles without `ZoneLihzhardTemple`, 22 / 204 without their zones, and hallow / ice tiles in
pre-hardmode take the ordinary path and are NOT boundaries. -/
def tileBoundary (c : Context) : Option (String × String) :=
  let t := c.tileType
  if t == 60 then some ("IL_7e7f", "jungle tile")
  else none

/-- The tile / wall boundaries after the underground critters (IL_812d … IL_9468). -/
def lateBoundary (c : Context) : Option (String × String) :=
  let t := c.tileType
  if t == 225 then some ("IL_82ba", "hive tile")
  else if c.wallType == 86 then some ("IL_87b3", "hive wall")
  else if [199, 200, 203, 234, 662].contains t then some ("IL_909a", "crimson tile")
  else if [23, 25, 112, 163, 661].contains t then some ("IL_9447", "corruption tile")
  else none

/-! ## Helpers -/

/-- `Spawner.CheckToSpawnUndergroundFairy(x, y)` (`tenthAnniversaryWorld`, `hardMode` off). -/
def checkFairy [Monad m] (ops : Ops m) (c : Context) : m Bool := do
  if !c.fairyLog then return false
  if (← ops.rollLuck "Spawner::CheckToSpawnUndergroundFairy IL_0036" 500) != 0 then return false
  if Float.ofInt c.y < (c.worldSurface + c.rockLayer) / 2 then return false
  if !(c.y < c.maxTilesY - 300) then return false
  return !(← ops.anyHelpfulFairies)

/-- `Spawner.CheckToSpawnUndergroundGnomes(x, y, allowed, gnomeChance)` (eclipse, blood moon,
remix off), day or night. -/
def checkGnomes [Monad m] (ops : Ops m) (c : Context) (allowed : Bool) : m Bool := do
  if !allowed then return false
  if (← ops.rollLuck "Spawner::CheckToSpawnUndergroundGnomes IL_0018" gnomeChance) != 0 then return false
  let low := c.worldSurface * 0.800000011920929
  let high := c.worldSurface * 1.100000023841858
  if Float.ofInt c.y < low || !(Float.ofInt c.y ≤ high) then return false
  let count ← ops.countNPCs 624
  let roll ← ops.next "Spawner::CheckToSpawnUndergroundGnomes IL_008b" 3
  if count > roll then return false
  -- IL_00a6: by day a tile without a wall gets no gnome
  if c.dayTime && c.wallAtTile ≤ 0 then return false
  let w := c.wallAtTile
  return w == 2 || w == 63 || (196 ≤ w && w ≤ 199)

/-- `GetBasicSlimeToSpawn(false, tileType, -1)`: with the first argument false only `Next(5)` /
`Next(2)` are read. -/
def basicSlime [Monad m] (ops : Ops m) : m Int := do
  if (← ops.next "Spawner::GetBasicSlimeToSpawn IL_00b8" 5) == 0 then return -9
  if (← ops.next "Spawner::GetBasicSlimeToSpawn IL_00ca" 2) == 0 then return 1
  return -8

/-! ## The dirt layer (`Spawner.underGround`) -/

private def dirtLayer [Monad m] (ops : Ops m) (c : Context)
    (born : String → Int → m Outcome) : m Outcome := do
  -- IL_b8ab: giant worm (ZoneSnow, hardMode off)
  if !c.noWorms then
    if (← ops.next (site "IL_b8bd") 50) == 0 then return ← born "IL_b969" 10
  -- IL_bace: ice slime on 147 / 161 (ZoneSnow off)
  if c.tileType == 147 || c.tileType == 161 then return ← born "IL_bb10" 147
  -- IL_bb17
  let slime ← basicSlime ops
  born "IL_bb48" slime

/-! ## The cavern layer -/

/-- `IL_c3ed … IL_cded`: the half of the cavern chain after `Next(2) == 0` at IL_c3e3. -/
private def cavernRare [Monad m] (ops : Ops m) (c : Context)
    (born : String → Int → m Outcome) : m Outcome := do
  -- IL_c3ed: skeleton merchant (one at a time)
  if (← ops.next (site "IL_c3f4") 35) == 0 && !c.zoneShadowCandle && !c.waterTile then
    if (← ops.countNPCs 453) == 0 then return ← born "IL_c441" 453
  -- IL_c448: Nymph
  if (← ops.next (site "IL_c44f") 80) == 0 then return ← born "IL_c480" 195
  -- IL_c4f1: Tim, deeper than halfway between the rock layer and the bottom
  if Float.ofInt c.y > (c.rockLayer + Float.ofInt c.maxTilesY) / 2 then
    -- (sequential on purpose: a nested `(← …)` in an `else if` would be hoisted and always drawn)
    let mut tim := (← ops.next (site "IL_c51c") 200) == 0
    if !tim && c.offensiveToTim then
      tim := (← ops.next (site "IL_c532") 50) == 0
    if tim then return ← born "IL_c560" 45
  -- IL_c567: marble (hardmode Medusa off)
  if c.nearMarble then
    if (← ops.next (site "IL_c578") 4) != 0 then
      if (← ops.next (site "IL_c588") 6) != 0 then
        let _ ← ops.anyNPCs 480
      return ← born "IL_c5fd" 481
  -- IL_c604: granite
  if c.nearGranite then
    if (← ops.next (site "IL_c615") 5) != 0 then
      if (← ops.next (site "IL_c622") 6) != 0 then
        if !(← ops.anyNPCs 483) then return ← born "IL_c65f" 483
      return ← born "IL_c690" 482
  -- IL_c7ec: Halloween ghost (graveyard off)
  if !c.noWorms && c.halloween then
    if (← ops.next (site "IL_c80a") 30) == 0 then return ← born "IL_c83b" 316
  -- IL_c842: undead miner
  if (← ops.next (site "IL_c849") 20) == 0 then return ← born "IL_c877" 44
  -- IL_c87e: ice tiles (ZoneSnow off)
  if c.tileType == 147 || c.tileType == 161 || c.tileType == 162 then
    if (← ops.next (site "IL_c89d") 15) == 0 then return ← born "IL_c8ce" 185
    return ← born "IL_c8ff" 167
  -- IL_c93f: the world's cavern monster
  if (← ops.next (site "IL_c945") 3) == 0 then
    let a ← ops.next (site "IL_c962") 2
    let b ← ops.next (site "IL_c96d") 3
    return ← born "IL_c991" (c.cavernMonsterType a b)
  -- IL_c9de: Halloween skeletons (ZoneGlowshroom off)
  if c.halloween then
    if (← ops.next (site "IL_c9eb") 2) == 0 then
      let t ← ops.nextRange (site "IL_ca0c") 322 325
      return ← born "IL_ca2b" t
  -- IL_ca32: expert armed skeletons. The original tests `Next(4) != 0` three times in a row, so
  -- only 449 (on 0) and 452 are reachable.
  if c.expertMode then
    if (← ops.next (site "IL_ca42") 3) == 0 then
      if (← ops.next (site "IL_ca52") 4) == 0 then return ← born "IL_ca87" 449
      return ← born "IL_cb22" 452
  -- IL_cb29: the four skeleton families, each with a small / big form on Next(3) == 0
  let v ← ops.next (site "IL_cb2f") 4
  let fam : List (Int × String × String × Int × String × String × Int × String × Int × String) :=
    [(0, "IL_cb43", "IL_cb7e", 21, "IL_cb71", "IL_cbac", -47, "IL_cbda", -46, ""),
     (1, "IL_cbef", "IL_cc2d", 201, "IL_cc20", "IL_cc5b", -49, "IL_cc89", -48, ""),
     (2, "IL_cc9e", "IL_ccdc", 202, "IL_cccf", "IL_cd0a", -51, "IL_cd38", -50, ""),
     (3, "IL_cd4d", "IL_cd8b", 203, "IL_cd7e", "IL_cdb9", -53, "IL_cde7", -52, "")]
  let some (_, rollIl, pickIl, base, baseIl, aIl, a, bIl, b, _) := fam.find? (·.1 == v)
    | return .nothing (site "IL_d046")
  if (← ops.next (site rollIl) 3) != 0 then return ← born baseIl base
  if (← ops.next (site pickIl) 2) == 0 then return ← born aIl a
  born bIl b

private def cavernLayer [Monad m] (ops : Ops m) (c : Context)
    (born : String → Int → m Outcome) : m (Except Stop Outcome) := do
  -- IL_bb4f: the underworld rows
  if c.y > c.maxTilesY - 190 then
    return .error (.boundary (site "IL_bb5b") "underworld row")
  -- IL_be33 CheckToSpawnRockGolem: false before hardmode, no draw
  -- IL_be6f: giant shelly / Cochineal (ZoneSnow off) → 217
  if (← ops.next (site "IL_be76") 60) == 0 then return .ok (← born "IL_bee0" 217)
  -- IL_bee7 … IL_c0d5: hallow / corrupt / crimson worms, hardmode only
  -- IL_c123: giant worm (ZoneHallow, hardMode, ZoneSnow off)
  if !c.noWorms then
    if (← ops.next (site "IL_c135") 100) == 0 then return .ok (← born "IL_c1df" 10)
  -- IL_c22d: mother slime / black slime on Next(10) == 0 (ZoneSnow off: ice tiles give 184)
  let ice := c.tileType == 161 || c.tileType == 147
  if (← ops.next (site "IL_c23b") 10) == 0 then
    if ice then return .ok (← born "IL_c29f" 184)
    if (← ops.next (site "IL_c2ac") 3) == 0 then return .ok (← born "IL_c2da" (-6))
    return .ok (← born "IL_c308" 16)
  -- IL_c320: black slime on Next(4) == 0 (ZoneJungle off)
  if (← ops.next (site "IL_c326") 4) == 0 then
    if ice then return .ok (← born "IL_c3a8" 184)
    return .ok (← born "IL_c3d6" (-6))
  -- IL_c3dd
  if (← ops.next (site "IL_c3e3") 2) == 0 then return .ok (← cavernRare ops c born)
  -- IL_cdee … IL_cf98: bats (hallow, jungle, glowshroom, hardmode off)
  if c.tileType == 147 || c.tileType == 161 || c.tileType == 162 then
    return .ok (← born "IL_d012" 150)
  return .ok (← born "IL_d040" 49)

/-! ## Entry -/

/-- `SpawnAnNPC` from IL_45d5 for a non-friendly, non-dungeon attempt (day or night) with a tile
below the surface. -/
def underground [Monad m] (ops : Ops m) (c : Context) : m (Except Stop Outcome) := do
  for (name, v) in c.offFlags do
    if v then return .error (.boundary "Spawner::SpawnAnNPC" s!"{name} is set")
  if !(Float.ofInt c.y > c.worldSurface) then
    return .error (.boundary (site "IL_706d") "tile on the surface")
  let bx := c.x * 16 + 8
  let by' := c.y * 16
  let born := fun (il : String) (t : Int) => do
    ops.spawn (site il) { typeId := t, x := bx, y := by' }
    pure (Outcome.spawned [{ typeId := t, x := bx, y := by' }])
  -- IL_747f: the fairy (Next(583, 586), ai2 = 2, then TargetClosest(true))
  if ← checkFairy ops c then
    let t ← ops.nextRange (site "IL_7495") 583 586
    let b : Birth := { typeId := t, x := bx, y := by', ai2 := 2, targetClosest := true }
    ops.spawn (site "IL_74e5") b
    return .ok (.spawned [b])
  -- IL_74f1 … IL_758a: living-tree gnome; by day only with a wall behind the tile
  if !c.waterTile && !(c.dayTime && c.wallAtTile ≤ 0) && c.livingTree then
    if (← ops.rollLuck (site "IL_753c") (gnomeChance * 3)) == 0 then
      let count ← ops.countNPCs 624
      let roll ← ops.next (site "IL_7553") 3
      if !(count > roll) then return .ok (← born "IL_7584" 624)
  -- IL_758b … IL_75e3 (ZoneCorrupt / ZoneCrimson are off-flags)
  if ← checkGnomes ops c !c.waterTile then return .ok (← born "IL_75dd" 624)
  if let some (il, why) := tileBoundary c then return .error (.boundary (site il) why)
  -- IL_7f10 … IL_812c: underground critters (zones off)
  if c.y < c.maxTilesY - 210 then
    if (← ops.next (site "IL_7f67") 8) == 0 then
      if (← ops.rollLuck (site "IL_7f74") c.goldCritterChance) == 0 then return .ok (← born "IL_7fa5" 448)
      return .ok (← born "IL_7fd6" 357)
  if c.y < c.maxTilesY - 210 then
    if (← ops.next (site "IL_8035") 13) == 0 then
      if (← ops.rollLuck (site "IL_8042") c.goldCritterChance) == 0 then return .ok (← born "IL_8073" 447)
      return .ok (← born "IL_80a4" 300)
  if Float.ofInt c.y < (c.rockLayer + Float.ofInt c.maxTilesY) / 2 then
    if (← ops.next (site "IL_80f5") 13) == 0 then return .ok (← born "IL_8126" 359)
  if let some (il, why) := lateBoundary c then return .error (.boundary (site il) why)
  -- IL_96a4: not surfaceSpawn → IL_b8a0
  if c.underGround then return .ok (← dirtLayer ops c born)
  cavernLayer ops c born

/-! ## Spiders (before IL_45d5) -/

/-- Inputs of the spider pieces. -/
structure SpiderContext where
  x : Int
  y : Int
  /-- `Main.tile[i, j].wall`. -/
  wallAt : Int → Int → Nat
  /-- `Spawner.pX`, `Spawner.pY` (the player's tile). -/
  pX : Int
  pY : Int
  rockLayer : Float
  maxTilesX : Int
  maxTilesY : Int
  /-- `Main.UnderworldLayer` (`maxTilesY - 200`). -/
  underworldLayer : Int
  /-- `Spawner.ZoneDungeon`, `Spawner.invaders` (either skips the scan). -/
  zoneDungeon : Bool := false
  invaders : Bool := false
  /-- `Spawner.deeperThanRockLayer` (`!(y < rockLayer)`), `Spawner.waterTile`. -/
  deeperThanRockLayer : Bool := false
  waterTile : Bool := false
  /-- `NPC.savedStylist`. -/
  savedStylist : Bool := false

/-- `SetSpawnFlagsForChosenTile` IL_0312 … IL_03f2 (`dontStarveWorld` off): below the rock layer
and above the underworld, `Next(3) == 0` scans a square of half-size `Next(5, 15)` around the tile
for spider wall 62 (only when it fits horizontally); otherwise only the player's tile is checked.
Returns `Spawner.spawnSpider`. -/
def spiderScan [Monad m] (ops : Ops m) (c : SpiderContext) : m Bool := do
  let s := "Spawner::SetSpawnFlagsForChosenTile"
  if !(Float.ofInt c.y > c.rockLayer && c.y < c.underworldLayer) then return false
  if c.zoneDungeon || c.invaders then return false
  if (← ops.next s!"{s} IL_0359" 3) == 0 then
    let r ← ops.nextRange s!"{s} IL_0368" 5 15
    if c.x - r < 0 || !(c.x + r < c.maxTilesX) then return false
    let mut found := false
    for i in [0:(2 * r).toNat] do
      for j in [0:(2 * r).toNat] do
        if c.wallAt (c.x - r + i) (c.y - r + j) == 62 then found := true
    return found
  return c.wallAt c.pX c.pY == 62

/-- `SpawnAnNPC` IL_107e … IL_11c9: on wall 62 or with `spawnSpider` (and `CheckToSpawnSpider`
true, as it is outside not-the-bees / FTW worlds), the Stylist (354, once, deep enough, not
saved, on wall 62 with `Next(8) == 0` off water) else a Wall Creeper (164; hardmode off).
`none` means the branch is not entered. -/
def spiderBranch [Monad m] (ops : Ops m) (c : SpiderContext) (spawnSpider : Bool) :
    m (Option Outcome) := do
  let onWall := c.wallAt c.x c.y == 62
  if !(onWall || spawnSpider) then return none
  let deep := c.deeperThanRockLayer && c.y < c.maxTilesY - 210
  let bx := c.x * 16 + 8
  let by' := c.y * 16
  let born := fun (il : String) (t : Int) => do
    ops.spawn (site il) { typeId := t, x := bx, y := by' }
    pure (some (Outcome.spawned [{ typeId := t, x := bx, y := by' }]))
  -- IL_10dc: the Next(8) is drawn whenever the tile has wall 62 (the `and` is not short-circuit)
  let mut roll := false
  if onWall then
    roll := (← ops.next (site "IL_10f7") 8) == 0 && !c.waterTile
  if roll && deep && !c.savedStylist then
    if !(← ops.anyNPCs 354) then return ← born "IL_114c" 354
  born "IL_11c3" 164

end Lean4Terria.OriginalUndergroundSpawn
