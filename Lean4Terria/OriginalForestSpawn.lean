import Std.Data.HashMap
import Lean4Terria.OriginalCandleBuffs
import Lean4Terria.OriginalSurfaceNightSpawn

/-!
# Natural spawn of the forest Green Slime (Terraria 1.4.5.8)

A transcription of one `NPC.SpawnNPC()` call (`Main.UpdateWorld_SpawnNPCs` → `NPC.SpawnNPC` →
`Spawner.SpawnNPC` → `TrySpawnAnNPC` → `SpawnAnNPC` → `GetBasicSlimeToSpawn` → `Spawner.SpawnNPC(int…)`)
for one player in a single-player Classic world with a normal seed, during the day, with the spawn
tile on the surface. Evidence and the replay against the vanilla dedicated server live in
`.lake/original-forest-spawn/1.4.5.8/` (REPORT.md).

The random stream is the one `Main.SwapRandom("SpawnNPC")` selects; every draw is logged with the
method and IL offset that makes it, so a replay can compare draw count and the final table.

Scope. Each branch the transcription does not follow stops with `Stop.unsupported` (naming the flag
or IL offset) instead of guessing; a branch that picks a different NPC (critters, birds) stops with
`Stop.other`, whose random stream after that point is not modelled. World and player flags the path
tests and that must be off for this scope are passed as named lists (`World.offFlags`,
`Player.offFlags`); any of them set makes the whole call `unsupported`.

`NPC.NewNPC` itself is not transcribed: each `(type, x, y)` of `Outcome.spawned` is one call
`NPC.NewNPC(source, x, y, type, 0, 0, 0, 0, 0, 255)`, i.e. that NPC's `Bottom` is `(x, y)`; the
captures check that `NewNPC` draws nothing from the stream for every type met here.
-/

namespace Lean4Terria.OriginalForestSpawn

/-! ## `Terraria.Utilities.UnifiedRandom` -/

/-- The subtractive generator with its table visible (`inext` = last used slot, 0 after seeding). -/
structure Rng where
  table : Array Int32
  inext : Nat
  deriving BEq, Repr, Inhabited

namespace Rng

private def limit : Int32 := 2147483647

/-- `UnifiedRandom(int seed)`. -/
def seed (value : Int) : Rng := Id.run do
  let value := Int32.ofInt value
  let magnitude := if value == -2147483648 then limit else if value < 0 then -value else value
  let mut previous := 161803398 - magnitude
  let mut next : Int32 := 1
  let mut table := (Array.replicate 56 (0 : Int32)).set! 55 previous
  for i in [1:55] do
    let index := (21 * i) % 55
    table := table.set! index next
    next := previous - next
    if next < 0 then next := next + limit
    previous := table[index]!
  for _ in [:4] do
    for i in [1:56] do
      let value := table[i]! - table[1 + (i + 30) % 55]!
      table := table.set! i (if value < 0 then value + limit else value)
  return ⟨table, 0⟩

/-- `InternalSample`. -/
def internalSample (rng : Rng) : Int32 × Rng :=
  let x := if rng.inext + 1 > 55 then 1 else rng.inext + 1
  let y := if x + 21 > 55 then x + 21 - 55 else x + 21
  let value := rng.table[x]! - rng.table[y]!
  let value := if value == limit then value - 1 else value
  let value := if value < 0 then value + limit else value
  (value, ⟨rng.table.set! x value, x⟩)

/-- `Sample()`: `InternalSample() * (1.0 / int.MaxValue)`. -/
def sample (rng : Rng) : Float × Rng :=
  let (value, rng) := rng.internalSample
  (Float.ofInt value.toInt * 4.656612875245797e-10, rng)

/-- `Next(maxValue)`, `maxValue ≥ 0`: `(int)(Sample() * maxValue)`. -/
def next (rng : Rng) (maxValue : Nat) : Nat × Rng :=
  let (value, rng) := rng.sample
  ((value * maxValue.toFloat).toUInt64.toNat, rng)

/-- `Next(minValue, maxValue)` for a range below `int.MaxValue`. -/
def nextRange (rng : Rng) (minValue maxValue : Int) : Int × Rng :=
  let (value, rng) := rng.sample
  ((value * Float.ofInt (maxValue - minValue)).toUInt64.toNat + minValue, rng)

end Rng

/-! ## Numbers as the CLR computes them -/

/-- `conv.i4` of a double: truncation toward zero (values here are far inside `int`). -/
def truncF (x : Float) : Int :=
  if x < 0 then -((-x).floor.toUInt64.toNat : Int) else (x.floor.toUInt64.toNat : Int)

/-- `conv.r4` of an `int`. -/
def i2f (n : Int) : Float32 := (Float.ofInt n).toFloat32

/-- `conv.i4` of a float. -/
def truncF32 (x : Float32) : Int := truncF x.toFloat

/-- An `ldc.r4` constant, given as the exact double the disassembly prints. -/
def r4 (x : Float) : Float32 := x.toFloat32

/-- C# `int / int` (toward zero). -/
def idiv (a b : Int) : Int := a.tdiv b

/-! ## World and player -/

/-- One `Main.tile[x, y]`, the fields the spawn code reads. -/
structure Tile where
  type : Nat := 0
  active : Bool := false
  inActive : Bool := false
  liquid : Nat := 0
  /-- `Tile.liquidType()`: 0 water, 1 lava, 2 honey, 3 shimmer. -/
  liquidType : Nat := 0
  wall : Nat := 0
  deriving Repr, Inhabited, BEq

namespace Tile
/-- `Tile.nactive()`: active and not actuated. -/
def nactive (t : Tile) : Bool := t.active && !t.inActive
def lava (t : Tile) : Bool := t.liquidType == 1
def honey (t : Tile) : Bool := t.liquidType == 2
def shimmer (t : Tile) : Bool := t.liquidType == 3
/-- `Tile.anyLava()`: `liquid > 0 && lava()`. -/
def anyLava (t : Tile) : Bool := t.liquid > 0 && t.lava
end Tile

/-- An `xna Rectangle`. -/
structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving Repr, BEq, Inhabited

namespace Rect
def left (r : Rect) : Int := r.x
def right (r : Rect) : Int := r.x + r.w
def top (r : Rect) : Int := r.y
def bottom (r : Rect) : Int := r.y + r.h
def contains (r : Rect) (x y : Int) : Bool := r.x ≤ x && x < r.right && r.y ≤ y && y < r.bottom
def intersects (a b : Rect) : Bool :=
  b.x < a.right && a.x < b.right && b.y < a.bottom && a.y < b.bottom
/-- `Utils.CenteredRectangle(Point center, Point size)`. -/
def centered (cx cy sx sy : Int) : Rect := ⟨cx - idiv sx 2, cy - idiv sy 2, sx, sy⟩
end Rect

/-- What the surface-night selection (`OriginalSurfaceNightSpawn`, released) reads besides the
stream and the world fields below, from the caller's real state. -/
structure NightInputs where
  /-- `Main.moonPhase`. -/
  moonPhase : Int
  /-- `NPC.fireFlyChance` of `NPC.setFireFlyChance`. -/
  fireFlyChance : Int
  /-- `Main.halloween`, `Main.xMas` as `checkHalloween` / `checkXMas` last set them. -/
  halloween : Bool
  xMas : Bool
  /-- `Player.statLifeMax` (base, `SetSpawnFlags` IL_035f `playerHasStartingHealth`). -/
  playerStatLifeMax : Int
  deriving Repr

/-- World statics read by the transcribed path. The bool tables are `Main.tileSolid`,
`Main.tileSolidTop`, `Main.wallHouse`, `TileID.Sets.InheritTypeOfTileBelowForNPCSpawning` and
`WallID.Sets.AllowsUndergroundDesertEnemiesToSpawn`.

Tiles are read one coordinate at a time through `tile`, as the game reads `Main.tile[x, y]`; the
path touches at most a few thousand cells per attempt (about ±140 × ±100 tiles around the player),
so a caller can answer from its own world storage without building a snapshot. `none` means the
caller cannot answer (outside its data) and makes the attempt `unsupported`. -/
structure World where
  tile : Int → Int → Option Tile
  maxTilesX : Int
  maxTilesY : Int
  worldSurface : Float
  rockLayer : Float
  underworldLayer : Int
  spawnTileX : Int
  /-- `NPC.sWidth`, `NPC.sHeight` (constant properties in 1.4.5.8: 1920 × 1200). -/
  sWidth : Int
  sHeight : Int
  dayTime : Bool
  time : Float
  windSpeedTarget : Float32
  windyDay : Bool := false
  cloudCount : Int := 0
  cloudBackground : Float32 := 0
  starfallBoost : Float32 := 1
  defaultSpawnRate : Int := 600
  defaultMaxSpawns : Int := 5
  /-- `Spawner.spawnSpaceX/Y` (static initialiser: 2 and 3). -/
  spawnSpaceX : Int := 2
  spawnSpaceY : Int := 3
  oceanDistance : Int
  beachDistance : Int
  /-- `NPC.stinkBugChance`, `butterflyChance`, `goldCritterChance`: per-day values drawn by
  `NPC.setFireFlyChance` at world load and at nightfall. -/
  stinkBugChance : Nat
  butterflyChance : Nat
  goldCritterChance : Nat
  fireFlyFriendly : Nat := 999999
  fireFlyMultiple : Nat := 999999
  /-- `Spawner.fairyLog`. -/
  fairyLog : Bool
  savedAngler : Bool
  savedMechanic : Bool := false
  downedSkeletron : Bool := false
  unlockedSlimeOldSpawn : Bool := false
  /-- `DD2Event.ReadyToFindBartender` (read only while `NPC.savedBartender` is false). -/
  readyToFindBartender : Bool
  isThereAWorldSurface : Bool
  expertMode : Bool
  /-- Types of the active NPCs (`NPC.AnyNPCs` / `NPC.CountNPCS`). -/
  activeNPCTypes : List Int := []
  /-- NPC.NearSpikeBall queries AI20 anchor rectangles, including new births. -/
  nearSpikeBall : Int → Int → Bool := fun _ _ => false
  /-- `NPC.noSpawnCycle`: set when an NPC despawned (`NPC.CheckActive`) or died (`NPC.checkDead`)
  during the previous frame's NPC updates; `NPC.SpawnNPC` then only clears it. -/
  noSpawnCycle : Bool := false
  /-- `Player.AFKTimeNeededForNoWormSpawns` (300 frames in 1.4.5.8). -/
  afkTimeNeededForNoWormSpawns : Int := 300
  /-- Another active player besides the one passed to `attempt`. -/
  otherActivePlayers : Bool := false
  tileSolid : Array Bool
  tileSolidTop : Array Bool
  wallHouse : Array Bool
  inheritTypeOfTileBelow : Array Bool
  desertWalls : Array Bool
  /-- World flags the path tests and that must be false here, by their game names
  (`Main.hardMode`, `Main.remixWorld`, `Main.raining`, …). -/
  offFlags : List (String × Bool) := []
  /-- Night inputs; `none` keeps the night branch (IL_4635) a boundary. -/
  night : Option NightInputs := none

/-- The player the spawn is attempted near (`Main.player[i]`). -/
structure Player where
  whoAmI : Nat
  active : Bool
  dead : Bool
  position : Float32 × Float32
  width : Int := 20
  height : Int := 42
  /-- `Player.luck` as `Spawner.SetSpawnFlags` IL_002c copies it: the value `Player.UpdateLuck`
  (`Player.Update` IL_24cb, after `UpdateEquips` IL_248f) left this frame; see `OriginalLuck`.
  Any value is supported; every luck roll on the transcribed path is exact. -/
  luck : Float32 := 0
  nearbyActiveNPCs : Float32 := 0
  /-- `Player.townNPCs`, an `int`: `SceneMetrics.TownNPCCount` copied by `Player.UpdateBiomes`. -/
  townNPCs : Int := 0
  candleZones : OriginalCandleBuffs.Zones := {}
  heldItem : Nat := 0
  /-- `Player.afkCounter`: frames without input, counted by `Player.Update`. -/
  afkCounter : Int := 0
  /-- Player flags that must be false here (zones, buffs, `scope`, …). -/
  offFlags : List (String × Bool) := []

def Player.center (p : Player) : Float32 × Float32 :=
  (p.position.1 + i2f p.width / 2, p.position.2 + i2f p.height / 2)

def Player.zoneDungeon (p : Player) : Bool :=
  (p.offFlags.lookup "ZoneDungeon").getD false

/-! ## Results and the stepping monad -/

/-- One `UnifiedRandom` call. -/
structure Draw where
  site : String
  call : String
  result : Int
  deriving Repr, Inhabited

/-- One `FindSpawnTile` pick. `ground` is the row after the downward scan (if it got there). -/
structure Pick where
  x : Int
  y : Int
  ground : Option Int
  verdict : String
  deriving Repr, Inhabited

inductive Outcome where
  /-- `NPC.noSpawnCycle` was set: the flag is cleared and nothing is drawn this frame. -/
  | noSpawnCycle
  /-- `CanSpawnEnemiesNear` is false (inactive or dead player). -/
  | notEligible
  /-- `player.nearbyActiveNPCs >= maxSpawns`: rejected before any draw. -/
  | tooManyNPCs
  /-- `Main.rand.Next(spawnRate) != 0`. -/
  | rateRoll (value : Int)
  /-- 50 picks without a usable tile. -/
  | noSpawnTile
  /-- `CheckNotSpawningOnScreen` refused the chosen tile. -/
  | onScreen (x y : Int)
  /-- `PostCheckChosenSpawnTile` refused it. -/
  | postCheck (why : String)
  /-- The `NPC.NewNPC(…, x, y, type, …)` calls, in order (critter groups make several). -/
  | spawned (npcs : List (Int × Int × Int))
  /-- `SpawnAnNPC` returned without spawning; `TrySpawnAnNPC` still reports success. -/
  | nothing (site : String)
  deriving Repr, BEq, Inhabited

inductive Stop where
  | unsupported (why : String)
  /-- The path chose something other than a slime; draws after `site` are not modelled. -/
  | other (site : String) (what : String)
  deriving Repr, Inhabited

/-- Everything the attempt computed, for a replay or a debugger. -/
structure Trace where
  rng : Rng
  draws : Array Draw := #[]
  spawnRate : Option Int := none
  maxSpawns : Option Int := none
  spawnRect : Option Rect := none
  safeRect : Option Rect := none
  picks : Array Pick := #[]
  chosen : Option (Int × Int × Bool) := none
  screenRect : Option Rect := none
  groundY : Option Int := none
  tileType : Option Nat := none
  wallType : Option Nat := none
  waterTile : Option Bool := none
  slimeChoice : Option Int := none
  deriving Inhabited

abbrev M := ExceptT Stop (StateM Trace)

def unsupported {α} (why : String) : M α := throw (.unsupported why)

private def draw (site call : String) (f : Rng → Int × Rng) : M Int := do
  let t ← get
  let (v, rng) := f t.rng
  set { t with rng := rng, draws := t.draws.push ⟨site, call, v⟩ }
  return v

def nextN (site : String) (n : Int) : M Int :=
  draw site s!"Next({n})" fun r => let (v, r) := r.next n.toNat; (v, r)

def nextR (site : String) (lo hi : Int) : M Int :=
  draw site s!"Next({lo}, {hi})" fun r => r.nextRange lo hi

/-- `Utils.NextFloat(rand)`: `(float)rand.NextDouble()`, i.e. `conv.r4` of `Sample()`. The draw is
logged with its Single bits as the result. -/
def nextFloat (site : String) : M Float32 := do
  let t ← get
  let (v, rng) := t.rng.sample
  let f := v.toFloat32
  set { t with rng := rng, draws := t.draws.push ⟨site, "NextFloat()", f.toBits.toNat⟩ }
  return f

/-! ### `Terraria.GameContent.Luck` (desktop 1.4.5.8, `Luck.Roll*` 100672604‥100672608)

Every comparison is the CIL `bge.un` / `ble.un` pair: a positive-luck roll happens only when
`luck > 0` and the float drawn is `< luck`; a negative-luck roll only when `luck < 0` and the
float is `< -luck` (Single negation). `Next(a, b)` is evaluated before the outer `Next`. -/

/-- `Luck.RollLuck(luck, range)`: lucky `Next(Next(range/2, range))`, unlucky
`Next(Next(range, range*2))`, otherwise `Next(range)`. -/
def rollLuck (site : String) (luck : Float32) (range : Int) : M Int := do
  if luck > 0 then
    if (← nextFloat s!"{site} NextFloat") < luck then
      return ← nextN site (← nextR s!"{site} inner" (idiv range 2) range)
  if luck < 0 then
    if (← nextFloat s!"{site} NextFloat") < -luck then
      return ← nextN site (← nextR s!"{site} inner" range (range * 2))
  nextN site range

/-- `Luck.RollBadLuck(luck, range)`: the mirror of `RollLuck`. -/
def rollBadLuck (site : String) (luck : Float32) (range : Int) : M Int := do
  if luck > 0 then
    if (← nextFloat s!"{site} NextFloat") < luck then
      return ← nextN site (← nextR s!"{site} inner" range (range * 2))
  if luck < 0 then
    if (← nextFloat s!"{site} NextFloat") < -luck then
      return ← nextN site (← nextR s!"{site} inner" (idiv range 2) range)
  nextN site range

/-- `Luck.RollBadLuckExtreme(luck, range)`: lucky `Next(range*10)`, unlucky `Next(range/10)`. -/
def rollBadLuckExtreme (site : String) (luck : Float32) (range : Int) : M Int := do
  if luck > 0 then
    if (← nextFloat s!"{site} NextFloat") < luck then return ← nextN site (range * 10)
  if luck < 0 then
    if (← nextFloat s!"{site} NextFloat") < -luck then return ← nextN site (idiv range 10)
  nextN site range

/-- `Luck.RollOnlyBadLuck(luck, range)`: only bad luck changes the roll. -/
def rollOnlyBadLuck (site : String) (luck : Float32) (range : Int) : M Int := do
  if luck < 0 then
    if (← nextFloat s!"{site} NextFloat") < -luck then
      return ← nextN site (← nextR s!"{site} inner" (idiv range 2) range)
  nextN site range

/-- `Luck.RollOnlyBadLuckExtreme(luck, range)`: `Next(range/10)` when bad luck hits, otherwise
`-1` with no further draw (no draw at all when `luck ≥ 0`). -/
def rollOnlyBadLuckExtreme (site : String) (luck : Float32) (range : Int) : M Int := do
  if luck < 0 then
    if (← nextFloat s!"{site} NextFloat") < -luck then return ← nextN site (idiv range 10)
  return -1

def tileAt (w : World) (x y : Int) : M Tile := do
  match w.tile x y with
  | some t => return t
  | none => unsupported s!"tile ({x}, {y}) not available"

/-- A `World.tile` over a captured box: cells inside the box and absent from the map are air. -/
def boxLookup (cells : Std.HashMap (Int × Int) Tile) (box : Int × Int × Int × Int) (x y : Int) : Option Tile :=
  let (x0, y0, x1, y1) := box
  if x < x0 || x > x1 || y < y0 || y > y1 then none else some ((cells.get? (x, y)).getD {})

def flagIn (table : Array Bool) (i : Nat) : Bool := table.getD i false

private def sandTile (tileType : Nat) : Bool :=
  (#[53, 112, 116, 234] : Array Nat).contains tileType

/-- `WorldGen.InWorld(x, y, fluff)`. -/
def inWorld (w : World) (x y fluff : Int) : Bool :=
  x ≥ fluff && x < w.maxTilesX - fluff && y ≥ fluff && y < w.maxTilesY - fluff

/-- `WorldGen.SolidTile3(x, y)`. -/
def solidTile3 (w : World) (x y : Int) : M Bool := do
  if !inWorld w x y 1 then return false
  let t ← tileAt w x y
  return t.nactive && flagIn w.tileSolid t.type && !flagIn w.tileSolidTop t.type

def anyNPC (w : World) (type : Int) : Bool := w.activeNPCTypes.contains type
def countNPC (w : World) (type : Int) : Int := (w.activeNPCTypes.filter (· == type)).length

/-! ## `Spawner.GetSpawnRate` -/

/-- `GetSpawnRate` for an event-free, pre-hardmode world with environmental candles:
base rate, depth tier, nearby-NPC tiers, candle factors, clamps and the town-NPC
branch. Returns `(spawnRate, maxSpawns, spawnFriendly)`. -/
def spawnRates (w : World) (p : Player) : M (Int × Int × Bool) := do
  let mut sr := w.defaultSpawnRate
  let mut ms := w.defaultMaxSpawns
  let y := p.position.2
  -- IL_0031 … IL_0182: depth tiers (hardMode / remixWorld are off)
  if y > i2f (w.underworldLayer * 16) then
    ms := truncF32 (i2f ms * r4 2.0)
  else if y.toFloat > w.rockLayer * 16.0 + Float.ofInt w.sHeight then
    sr := truncF (Float.ofInt sr * 0.4)
    ms := truncF32 (i2f ms * r4 1.899999976158142)
  else if y.toFloat > w.worldSurface * 16.0 + Float.ofInt w.sHeight then
    sr := truncF (Float.ofInt sr * 0.5)
    ms := truncF32 (i2f ms * r4 1.7000000476837158)
  else if !w.dayTime then
    sr := truncF (Float.ofInt sr * 0.6)
    ms := truncF32 (i2f ms * r4 1.2999999523162842)
  if p.zoneDungeon then
    sr := truncF (Float.ofInt sr * 0.3)
    ms := truncF32 (i2f ms * r4 1.7999999523162842)
  -- Blood moon, eclipse and moon events are outside these flags.
  -- IL_069b: the "existing NPC" tiers
  let n := p.nearbyActiveNPCs.toFloat
  let msd := Float.ofInt ms
  if n < msd * 0.2 then sr := truncF32 (i2f sr * r4 0.6000000238418579)
  else if n < msd * 0.4 then sr := truncF32 (i2f sr * r4 0.699999988079071)
  else if n < msd * 0.6 then sr := truncF32 (i2f sr * r4 0.800000011920929)
  else if n < msd * 0.8 then sr := truncF32 (i2f sr * r4 0.8999999761581421)
  -- IL_0729: deeper than halfway to the rock layer
  if (y / i2f 16).toFloat > (w.worldSurface + w.rockLayer) / 2.0 then
    if n < msd * 0.2 then sr := truncF32 (i2f sr * r4 0.699999988079071)
    else if n < msd * 0.4 then sr := truncF32 (i2f sr * r4 0.8999999761581421)
  let candleNumbers := OriginalCandleBuffs.spawnFactors p.candleZones ⟨sr, ms⟩
  sr := OriginalCandleBuffs.waterSkyFactor p.candleZones y w.worldSurface candleNumbers.rate
  ms := candleNumbers.maximum
  -- IL_093f: floor and cap
  if Float.ofInt sr < Float.ofInt w.defaultSpawnRate * 0.1 then
    sr := truncF (Float.ofInt w.defaultSpawnRate * 0.1)
  if ms > w.defaultMaxSpawns * 3 then ms := w.defaultMaxSpawns * 3
  if p.zoneDungeon && !w.downedSkeletron then sr := 10
  -- IL_0abb … IL_0e56: town NPCs near the player (invaders, moons, eclipse, corruption, meteor, OOA: off)
  let mut friendly := false
  let s := "Spawner::GetSpawnRate"
  let townNPCs := if p.candleZones.shadow then 0 else p.townNPCs
  if (p.center.2 / i2f 16) > i2f w.underworldLayer then
    if townNPCs != 0 then unsupported s!"{s} IL_0b34: town NPCs in the underworld"
  else if townNPCs == 1 then
    -- IL_0c65: noWorms; (Skyblock.lowTiles, graveyard: off)
    if (← nextN s!"{s} IL_0ce5" 3) == 1 then
      friendly := true
      ms := truncF ((i2f ms).toFloat * 0.6)
    else sr := truncF32 (i2f sr * r4 2.0)
  else if townNPCs == 2 then
    -- IL_0d1b
    if (← nextN s!"{s} IL_0d90" 3) != 0 then
      friendly := true
      ms := truncF ((i2f ms).toFloat * 0.6)
    else sr := truncF32 (i2f sr * r4 3.0)
  else if townNPCs ≥ 3 then
    -- IL_0dc5 / IL_0e29: always friendly outside expert mode
    if w.expertMode then
      if (← nextN s!"{s} IL_0e37" 30) != 0 then friendly := true
    else friendly := true
    ms := truncF ((i2f ms).toFloat * 0.6)
  -- IL_0e56 … IL_0e7f: when not friendly, RollOnlyBadLuckExtreme(50) == 0 (only reachable with
  -- luck < 0) gives spawnRate = (int)((float)spawnRate * 0.85f), maxSpawns = (int)((float)maxSpawns * 1.15f)
  if !friendly then
    if (← rollOnlyBadLuckExtreme s!"{s} IL_0e61" p.luck 50) == 0 then
      sr := truncF32 (i2f sr * (0.85 : Float32))
      ms := truncF32 (i2f ms * (1.15 : Float32))
  return (sr, ms, friendly)

/-! ## `GetSpawnArea`, `FindSpawnTile`, `CheckNotSpawningOnScreen` -/

structure Area where
  spawn : Rect
  safe : Rect
  safeRangeX : Int
  safeRangeY : Int

/-- `GetSpawnArea` (no rifle scope / binoculars / `player.scope`). -/
def spawnArea (w : World) (p : Player) : M Area := do
  if p.heldItem == 1254 || p.heldItem == 1299 then unsupported "GetSpawnArea: rifle scope or binoculars held"
  let sw := Float.ofInt (idiv w.sWidth 16)
  let sh := Float.ofInt (idiv w.sHeight 16)
  let rx := truncF (sw * 0.7)
  let ry := truncF (sh * 0.7)
  let sx := truncF (sw * 0.52)
  let sy := truncF (sh * 0.52)
  -- Utils.ToTileCoordinates: (int)X >> 4
  let tx := truncF32 p.position.1 / 16
  let ty := truncF32 p.position.2 / 16
  let r := Rect.centered tx ty (rx * 2) (ry * 2)
  -- WorldUtils.ClampToWorld(r, 0)
  let clampX (v : Int) := max 0 (min v w.maxTilesX)
  let clampY (v : Int) := max 0 (min v w.maxTilesY)
  let l := clampX r.left
  let t := clampY r.top
  let spawn : Rect := ⟨l, t, clampX r.right - l, clampY r.bottom - t⟩
  return ⟨spawn, Rect.centered tx ty (sx * 2) (sy * 2), sx, sy⟩

/-- `HasTileSpawnSpace(x, y)`. -/
def hasTileSpawnSpace (w : World) (x y : Int) : M Bool := do
  let r : Rect := ⟨x - idiv w.spawnSpaceX 2, y - w.spawnSpaceY, w.spawnSpaceX, w.spawnSpaceY⟩
  if !(r.x ≥ 0 && r.right < w.maxTilesX && r.y ≥ 0 && r.bottom < w.maxTilesY) then return false
  for i in [0:r.w.toNat] do
    for j in [0:r.h.toNat] do
      let t ← tileAt w (r.x + i) (r.y + j)
      if t.nactive && flagIn w.tileSolid t.type then return false
      if t.anyLava then return false
  return true

/-- `FindSpawnTile`: up to 50 picks of `(Next(Left, Right), Next(Top, Bottom))`. -/
def findSpawnTile (w : World) (a : Area) (friendly : Bool) : M (Option (Int × Int × Bool)) := do
  for _ in [0:50] do
    let x ← nextR "Spawner::FindSpawnTile IL_0024" a.spawn.left a.spawn.right
    let y0 ← nextR "Spawner::FindSpawnTile IL_003d" a.spawn.top a.spawn.bottom
    let pick (g : Option Int) (v : String) : M Unit :=
      modify fun t => { t with picks := t.picks.push ⟨x, y0, g, v⟩ }
    let t ← tileAt w x y0
    if t.nactive && flagIn w.tileSolid t.type then
      pick none "solid tile"
      continue
    if flagIn w.wallHouse t.wall then
      pick none "house wall"
      continue
    if !friendly && Float.ofInt y0 < w.worldSurface * 0.3499999940395355 &&
        (Float.ofInt x < Float.ofInt w.maxTilesX * 0.45 || Float.ofInt x > Float.ofInt w.maxTilesX * 0.55) then
      unsupported "FindSpawnTile IL_00ed: sky pick (skyMob)"
    -- IL_016f: scan down to the first solid tile, at most to the rectangle's bottom
    let mut y := y0
    let mut found := false
    while y < w.maxTilesY && y < a.spawn.bottom do
      let g ← tileAt w x y
      if g.nactive && flagIn w.tileSolid g.type then found := true; break
      y := y + 1
    let _ := found
    if y ≥ a.spawn.bottom then
      pick none "no ground above the rectangle's bottom"
      continue
    if a.safe.contains x y then
      pick (some y) "inside the safe rectangle"
      continue
    if !(← hasTileSpawnSpace w x y) then
      pick (some y) "no spawn space (solid or lava above)"
      continue
    pick (some y) "chosen"
    return some (x, y, a.safe.left ≤ x && x < a.safe.right)
  return none

/-- `CheckNotSpawningOnScreen(x, y)` for the one active player. -/
def notOnScreen (w : World) (p : Player) (a : Area) (x y : Int) : M Bool := do
  let (cx, cy) := p.center
  let hw := idiv w.sWidth 2
  let hh := idiv w.sHeight 2
  let r : Rect := ⟨truncF32 (cx - i2f hw - i2f a.safeRangeX), truncF32 (cy - i2f hh - i2f a.safeRangeY),
    w.sWidth + a.safeRangeX * 2, w.sHeight + a.safeRangeY * 2⟩
  modify fun t => { t with screenRect := some r }
  return !(Rect.intersects ⟨x * 16, y * 16, 16, 16⟩ r)

/-! ## The chosen tile -/

/-- `FindGroundTile(x, y)`. -/
def findGround (w : World) (x y : Int) : M Int := do
  let mut g := y
  let t ← tileAt w x y
  if flagIn w.inheritTypeOfTileBelow t.type && (← solidTile3 w x (y + 1)) then g := y + 1
  if !(← solidTile3 w x y) then
    for k in [1:30] do
      if (← solidTile3 w x (y + k)) then return y + k
  return g

/-- `GetSpawnWallType(x, y)`. -/
def spawnWallType (w : World) (x y : Int) : M Nat := do
  let above ← tileAt w x (y - 1)
  if (← tileAt w x (y - 2)).wall == 244 || (← tileAt w x y).wall == 244 then return 244
  return above.wall

/-- `Main.tileDungeon` in ordinary worlds, including the alternate brick forms. -/
def dungeonTile (tileType : Nat) : Bool :=
  #[41, 43, 44, 677, 678, 679].contains tileType

/-- `PostCheckChosenSpawnTile`; `none` = accepted. -/
def postCheck (w : World) (x y : Int) (tileType wallType : Nat) (dungeon : Bool) : M (Option String) := do
  if dungeon && (!dungeonTile tileType || wallType == 0) then return some "outside dungeon floor"
  let a1 ← tileAt w x (y - 1)
  let a2 ← tileAt w x (y - 2)
  if a1.liquid > 0 && a2.liquid > 0 && !a1.lava then
    if a1.shimmer then return some "shimmer above"
    if a1.honey then return some "honey above"
  if tileType == 477 || tileType == 492 then
    -- no event is running in this scope
    if (← nextN "Spawner::PostCheckChosenSpawnTile IL_00e6" 100) < 10 then return some "mowed grass roll"
  return none

/-- The flags `SetSpawnFlagsForChosenTile` sets that the transcribed path reads. -/
structure TileFlags where
  waterTile : Bool
  /-- `Spawner.nearMarble` / `nearGranite` (`SetSpawnFlags` IL_006a / IL_0071 clear them per call).
  `SpawnAnNPC` reads them only at IL_c568 / IL_c605, which every CFG path reaches with
  `spawnFriendly`, `ZoneDungeon`, `surfaceSpawn` and `underGround` all false — inside the cavern
  branch that this transcription stops at (IL_b8a6). -/
  nearMarble : Bool
  nearGranite : Bool
  undergroundDesert : Bool
  surfaceSpawn : Bool
  deeperThanRockLayer : Bool
  isOcean : Bool
  isBeach : Bool

/-- `SetSpawnFlagsForChosenTile(x, y, groundY, tileType, wallType)` (no remix / dual-dungeon seed).
`pX`, `pY` are `Spawner.pX/pY`: the player's centre in tiles. -/
def chosenTileFlags (w : World) (pX pY x y : Int) (tileType : Nat) (dungeon := false) : M TileFlags := do
  let s := "Spawner::SetSpawnFlagsForChosenTile"
  let t ← tileAt w x y
  let a1 ← tileAt w x (y - 1)
  let a2 ← tileAt w x (y - 2)
  let waterTile := a1.liquid > 0 && a2.liquid > 0 && a1.liquidType == 0
  -- IL_006f … IL_02d1: marble (367) / granite (368) under the tile, else under the player, else
  -- two strided scans (always both, with all their draws) that may set either flag or both
  let mut nearMarble := t.type == 367
  let mut nearGranite := t.type == 368
  if !nearMarble && !nearGranite then
    let pt ← tileAt w pX pY
    nearMarble := pt.type == 367
    nearGranite := pt.type == 368
    if !nearMarble && !nearGranite then
      let mut r ← nextR s!"{s} IL_0106" 20 31
      let step ← nextR s!"{s} IL_0113" 1 4
      if x - r < 0 then r := x
      if y - r < 0 then r := y
      if x + r ≥ w.maxTilesX then r := w.maxTilesX - x - 1
      if y + r ≥ w.maxTilesY then r := w.maxTilesY - y - 1
      let mut i := x - r
      while i ≤ x + r do
        let stepY ← nextR s!"{s} IL_015f" 1 4
        let mut j := y - r
        while j ≤ y + r do
          let c ← tileAt w i j
          if c.type == 367 then nearMarble := true
          if c.type == 368 then nearGranite := true
          j := j + stepY
        i := i + step
      let mut r2 ← nextR s!"{s} IL_01d3" 30 61
      let step2 ← nextR s!"{s} IL_01e0" 3 7
      if pX - r2 < 0 then r2 := pX
      if pY - r2 < 0 then r2 := pY
      if pX + r2 ≥ w.maxTilesX then r2 := w.maxTilesX - pX - 2
      if pY + r2 ≥ w.maxTilesY then r2 := w.maxTilesY - pY - 2
      let mut i2 := pX - r2
      while i2 ≤ pX + r2 do
        let stepY ← nextR s!"{s} IL_0259" 3 7
        let mut j := pY - r2
        while j ≤ pY + r2 do
          let c ← tileAt w i2 j
          if c.type == 367 then nearMarble := true
          if c.type == 368 then nearGranite := true
          j := j + stepY
        i2 := i2 + step2
  -- IL_0312: spider caves only below the rock layer
  if !dungeon && Float.ofInt y > w.rockLayer then unsupported s!"{s} IL_0319: spawn tile below the rock layer"
  -- IL_03f3: underground-desert walls, for every row 200 < y < rockLayer (surface included)
  let mut desert := false
  if !dungeon && Float.ofInt y < w.rockLayer && y > 200 then
    if (← nextN s!"{s} IL_0426" 3) == 0 then
      let r ← nextR s!"{s} IL_0435" 5 15
      if x - r ≥ 0 && x + r < w.maxTilesX then
        for i in [0:(2 * r).toNat] do
          for j in [0:(2 * r).toNat] do
            if flagIn w.desertWalls (← tileAt w (x - r + i) (y - r + j)).wall then desert := true
    else
      if flagIn w.desertWalls (← tileAt w pX pY).wall then desert := true
  let surfaceSpawn := !(Float.ofInt y > w.worldSurface)
  let deeper := !(Float.ofInt y < w.rockLayer)
  -- IL_050b: isOcean / isBeach
  let isOcean := (x < w.oceanDistance || x > w.maxTilesX - w.oceanDistance) &&
    sandTile tileType && Float.ofInt y < w.rockLayer
  if !isOcean && tileType == 53 then
    unsupported s!"{s} IL_053d: sand tile (WorldGen.oceanDepths)"
  let isBeach := !(Float.ofInt y > w.worldSurface) && (x < w.beachDistance || x > w.maxTilesX - w.beachDistance)
  return ⟨waterTile, nearMarble, nearGranite, desert, surfaceSpawn, deeper, isOcean, isBeach⟩

/-! ## `SpawnAnNPC`: the surface daytime path -/

/-- `GetBasicSlimeToSpawn(true, tileType, distFromSpawn)` without the holiday rolls. -/
def basicSlime (w : World) (tileType : Nat) (dist : Int) : M Int := do
  let s := "Spawner::GetBasicSlimeToSpawn"
  if tileType == 60 then return -10
  if tileType == 161 || tileType == 147 then return 147
  if (← nextN s!"{s} IL_0075" 3) == 0 then return -3
  if dist < 200 && !w.expertMode then return -3
  if (← nextN s!"{s} IL_0097" 10) == 0 && (dist > 400 || w.expertMode) then return -7
  return 1

/-- `Spawner.SpawnNPC(x, y, type, …)`: the 1/180 pinky roll for every netID whose type is 1. -/
def spawnOne (x y type : Int) (luck : Float32) : M (Int × Int × Int) := do
  let mut type := type
  -- NPCID.FromNetId(type) == 1: blue 1, green −3, purple −7, red −8, yellow −9, jungle −10
  if type == 1 || type == -3 || type == -7 || type == -8 || type == -9 || type == -10 then
    if (← rollLuck "Spawner::SpawnNPC IL_000f" luck 180) == 0 then type := -4
  return (type, x, y)

def spawnNPC (x y type : Int) (luck : Float32) : M Outcome := do
  return .spawned [← spawnOne x y type luck]

private def dungeonSpawn (w : World) (p : Player) (f : TileFlags) (x y : Int) : M Outcome := do
  let site := "Spawner::SpawnAnNPC"
  let born := fun kind => spawnNPC (x * 16 + 8) (y * 16) kind p.luck
  let lower ← tileAt w x y
  let upper ← tileAt w x (y - 1)
  let hasWall := fun (ids : Array Nat) => ids.contains lower.wall || ids.contains upper.wall
  let mut wallStyle := if hasWall #[95, 97, 99] then 2 else if hasWall #[94, 96, 98] then 1 else 0
  if (← rollLuck s!"{site} IL_3eca" p.luck 7) == 0 then
    wallStyle := ← nextN s!"{site} IL_3ed7" 3
  if !w.downedSkeletron then return ← born 68
  if !w.savedMechanic then
    if (← nextN s!"{site} IL_3f38" 5) == 0 &&
        !f.waterTile && !anyNPC w 123 && Float.ofInt y > (4 * w.worldSurface + w.rockLayer) / 5 then
      return ← born 123
  if (← rollLuck s!"{site} IL_425d" p.luck 35) == 0 then return ← born 71
  if wallStyle == 1 then
    if (← nextN s!"{site} IL_429d" 3) == 0 then
      if !w.nearSpikeBall x y then return ← born 70
  if wallStyle == 2 then
    if (← nextN s!"{site} IL_42e6" 5) == 0 then return ← born 72
  if wallStyle == 0 then
    if (← nextN s!"{site} IL_4325" 7) == 0 then return ← born 34
  if (← nextN s!"{site} IL_4360" 7) == 0 then return ← born 32
  let bookBranch ← if (← nextN s!"{site} IL_439e" 8) == 0 then pure true
    else pure ((← nextN s!"{site} IL_4413" 10) == 0)
  if bookBranch then
    for dx in [:32] do
      for dy in [:32] do
        let tx := x + (dx : Int) - 16
        let ty := y + (dy : Int) - 16
        if inWorld w tx ty 0 then
          let candidate ← tileAt w tx ty
          if candidate.active && candidate.type == 50 then
            unsupported s!"{site} IL_43c1: dungeon book check"
  let variant ← nextN s!"{site} IL_4482" 5
  if variant < 3 then return ← born (294 + variant)
  if (← nextN s!"{site} IL_4537" 4) == 0 then return ← born (-14)
  if (← nextN s!"{site} IL_4572" 5) == 0 then return ← born (-13)
  born 31

/-- The bird picks shared by `IL_a1bc`, `IL_a2ee`, `IL_3583` and `IL_3a1d`: `k = Next(4)`, then
`RollLuck(goldCritterChance) == 0` → gold bird 442, else 297 / 298 / 74 by `k`. -/
def birds (w : World) (p : Player) (site goldSite : String) (bx by' : Int) : M Outcome := do
  let k ← nextN site 4
  if (← rollLuck goldSite p.luck w.goldCritterChance) == 0 then return .spawned [(442, bx, by')]
  return .spawned [(if k == 0 then 297 else if k == 1 then 298 else 74, bx, by')]

/-- A critter group: the first NPC, then `Next(4) == 0` adds one 16 px to the left and an
independent second `Next(4) == 0` adds one 16 px to the right (`IL_9a19`/`IL_9a59` and
`IL_36c1`/`IL_3701` stink bugs, `IL_9b47`/`IL_9b87` and `IL_37e4`/`IL_3824` butterflies). -/
def group (site1 site2 : String) (first : Int × Int × Int) (other : Int) : M Outcome := do
  let (_, bx, by') := first
  let mut out := [first]
  if (← nextN site1 4) == 0 then out := out ++ [(other, bx - 16, by')]
  if (← nextN site2 4) == 0 then out := out ++ [(other, bx + 16, by')]
  return .spawned out

/-- `SelectRandom(299, 538)`: `Next(2)` picks the squirrel. -/
def squirrel (site : String) (bx by' : Int) : M Outcome := do
  let k ← nextN site 2
  return .spawned [(if k == 0 then 299 else 538, bx, by')]

/-- The friendly-critter branch (`IL_25ad` … `IL_3e43`) for a dry tile above the rock layer. -/
def friendlyBranch (w : World) (p : Player) (f : TileFlags) (y : Int) (tileType : Nat)
    (tooWindy inSafeX : Bool) (bx by' : Int) : M Outcome := do
  let s := "Spawner::SpawnAnNPC"
  if !inSafeX && f.isBeach then unsupported s!"{s} IL_263c: beach critters"
  if f.waterTile then unsupported s!"{s} IL_2a28: friendly water critters"
  if tileType == 147 || tileType == 161 || tileType == 60 || tileType == 53 then
    unsupported s!"{s} IL_2f24: friendly critters on tile {tileType}"
  -- IL_3128: grass of any kind, or any tile below the surface line, continues; other surface tiles spawn nothing
  if !(tileType == 2 || tileType == 477 || tileType == 109 || tileType == 492) && !(Float.ofInt y > w.worldSurface) then
    return .nothing s!"{s} IL_3148"
  -- IL_3155..32e1: in the rain (above the underworld) every friendly spawn is a gem critter (below the
  -- rock layer), a worm or a goldfish
  let raining := (w.offFlags.lookup "raining").getD false
  if raining && !(y > w.underworldLayer) then
    if f.deeperThanRockLayer then unsupported s!"{s} IL_316c: rain gem critters below the rock layer"
    if (← rollLuck s!"{s} IL_31fd" p.luck w.goldCritterChance) == 0 then return .spawned [(448, bx, by')]
    if (← nextN s!"{s} IL_323b" 3) != 0 then return .spawned [(357, bx, by')]
    if (← rollLuck s!"{s} IL_3279" p.luck w.goldCritterChance) == 0 then return .spawned [(593, bx, by')]
    return .spawned [(230, bx, by')]
  -- below the surface the same chain runs with surfaceSpawn = false: its rolls are drawn but only the
  -- gold bunny and the bunny can spawn there
  let surface := f.surfaceSpawn
  if !w.dayTime then
    if w.cloudCount ≤ 55 && w.cloudBackground == 0 && w.starfallBoost > 3 && surface then
      if (← rollLuck s!"{s} IL_3314" p.luck 2) == 0 then return .spawned [(484, bx, by')]
    if !tooWindy then
      if (← nextN s!"{s} IL_3361" w.fireFlyFriendly) == 0 && surface then
        let fly := if tileType == 109 then 358 else 355
        let mut spawns := [(fly, bx, by')]
        for (site, dx, dy) in #[("IL_33be", -16, 0), ("IL_33ff", 16, 0), ("IL_3440", 0, -16), ("IL_3481", 0, 16)] do
          if (← nextN s!"{s} {site}" w.fireFlyMultiple) == 0 then
            spawns := spawns ++ [(fly, bx + dx, by' + dy)]
        return .spawned spawns
    if (← nextN s!"{s} IL_34d1" 5) == 0 && surface then
      let nocturnal ← rollBadLuckExtreme s!"{s} IL_34e4" p.luck 100
      return .spawned [(if nocturnal == 0 then 689 else 611, bx, by')]
  -- IL_354d: morning birds
  if w.dayTime && w.time < 18000.0 then
    if (← nextN s!"{s} IL_356a" 3) > 0 && surface then
      return ← birds w p s!"{s} IL_3583" s!"{s} IL_3590" bx by'
  -- IL_3661: stink bugs
  if !tooWindy && w.dayTime then
    if (← nextN s!"{s} IL_367e" w.stinkBugChance) == 0 && surface then
      return ← group s!"{s} IL_36c7" s!"{s} IL_3707" (669, bx, by') 669
  -- IL_3745: butterflies
  if !tooWindy && w.dayTime then
    if (← nextN s!"{s} IL_3762" w.butterflyChance) == 0 && surface then
      let first := if (← rollLuck s!"{s} IL_377b" p.luck w.goldCritterChance) == 0 then 444 else 356
      return ← group s!"{s} IL_37ea" s!"{s} IL_382a" (first, bx, by') 356
  -- IL_3868: windy butterflies
  if tooWindy && w.dayTime then unsupported s!"{s} IL_3887: windy-day butterflies"
  -- IL_3a01: birds
  if (← nextN s!"{s} IL_3a07" 2) == 0 && surface then
    return ← birds w p s!"{s} IL_3a1d" s!"{s} IL_3a2a" bx by'
  -- IL_3afb: between the rock layer and the underworld (not on the surface)
  if Float.ofInt y ≥ w.rockLayer then unsupported s!"{s} IL_3afb: friendly cavern critters"
  -- IL_3bcb / IL_3c09: gold bunny, gold squirrel
  if (← rollLuck s!"{s} IL_3bd1" p.luck w.goldCritterChance) == 0 then return .spawned [(443, bx, by')]
  if (← rollLuck s!"{s} IL_3c0f" p.luck w.goldCritterChance) == 0 && surface then return .spawned [(539, bx, by')]
  -- (Halloween, Christmas, party: off) IL_3d28
  if (← nextN s!"{s} IL_3d2e" 3) != 0 then return .spawned [(46, bx, by')]
  if surface then return ← squirrel s!"{s} IL_3daa" bx by'
  return .nothing s!"{s} IL_3d7f"

/-- The non-friendly critter branch after `IL_98f9 Next(15) == 0` (`IL_9903` … `IL_9f52`). -/
def critterBranch (w : World) (p : Player) (f : TileFlags) (y : Int) (tileType : Nat) (tooWindy : Bool)
    (bx by' : Int) : M (Option Outcome) := do
  let s := "Spawner::SpawnAnNPC"
  if !(tileType == 2 || tileType == 477 || tileType == 109 || tileType == 492 || tileType == 147 || tileType == 161) then
    return none
  if tileType == 147 || tileType == 161 then
    return some (.spawned [(if (← nextN s!"{s} IL_9945" 2) == 0 then 148 else 149, bx, by')])
  let raining := (w.offFlags.lookup "raining").getD false
  -- IL_99ae: stink bugs (IL_99b6: not in the rain)
  if !tooWindy && !raining && w.dayTime then
    if (← nextN s!"{s} IL_99d4" w.stinkBugChance) == 0 && f.surfaceSpawn then
      return some (← group s!"{s} IL_9a1f" s!"{s} IL_9a5f" (669, bx, by') 669)
  -- IL_9a9d: butterflies (IL_9aa5: not in the rain)
  if !tooWindy && !raining && w.dayTime then
    if (← nextN s!"{s} IL_9ac3" w.butterflyChance) == 0 && f.surfaceSpawn then
      let first := if (← rollLuck s!"{s} IL_9ade" p.luck w.goldCritterChance) == 0 then 444 else 356
      return some (← group s!"{s} IL_9b4d" s!"{s} IL_9b8d" (first, bx, by') 356)
  -- IL_9bcb: windy butterflies
  if tooWindy && !raining && w.dayTime then unsupported s!"{s} IL_9bf3: windy-day butterflies"
  -- IL_9d6f / IL_9dad: gold bunny, gold squirrel
  if (← rollLuck s!"{s} IL_9d75" p.luck w.goldCritterChance) == 0 then return some (.spawned [(443, bx, by')])
  if (← rollLuck s!"{s} IL_9db3" p.luck w.goldCritterChance) == 0 && !(Float.ofInt y > w.worldSurface) then
    return some (.spawned [(539, bx, by')])
  -- (holidays, party: off) IL_9ec3
  if (← nextN s!"{s} IL_9ec9" 3) == 0 && !(Float.ofInt y > w.worldSurface) then
    return some (← squirrel s!"{s} IL_9eff" bx by')
  return some (.spawned [(46, bx, by')])

/-- `SpawnAnNPC` from IL_45d5 at night: the released `OriginalSurfaceNightSpawn.surfaceNight`, its
requests performed on this attempt's stream and luck. Its births are collected in request order;
`NPC.AnyHelpfulFairies` (asked only for the fairy, unreachable on the surface) is a boundary. -/
private def surfaceNightBranch (w : World) (p : Player) (f : TileFlags) (x y : Int) (tileType wallType : Nat)
    (livingTree tooWindy : Bool) (family : Int) : M Outcome := do
  let s := "Spawner::SpawnAnNPC"
  let some n := w.night | unsupported s!"{s} IL_4635: night (no night inputs from the caller)"
  let flag := fun (name : String) => (p.offFlags.lookup name).getD false
  let c : OriginalSurfaceNightSpawn.Context := {
    x, y, tileType, wallType, wallAtTile := (← tileAt w x y).wall
    worldSurface := w.worldSurface, rockLayer := w.rockLayer, maxTilesY := w.maxTilesY, dayTime := w.dayTime
    waterTile := f.waterTile, livingTree
    -- `run` has already stopped on `Main.raining` (a world off-flag)
    raining := (w.offFlags.lookup "raining").getD false
    fairyLog := w.fairyLog, moonPhase := n.moonPhase, halloween := n.halloween, xMas := n.xMas
    expertMode := w.expertMode, tooWindyForButterflies := tooWindy
    fireFlyChance := n.fireFlyChance, fireFlyMultiple := Int.ofNat w.fireFlyMultiple
    -- `TileID.Sets.IcesSnow`, read from the running original (`tile_sets_survey`)
    icesSnow := [147, 161, 163, 164, 200].contains tileType
    zoneCorrupt := flag "ZoneCorrupt", zoneCrimson := flag "ZoneCrimson"
    -- one active player (`run` stops on another)
    settings := OriginalSurfaceNightSpawn.zombieSettings family (!(n.playerStatLifeMax > 100)) 1
    offFlags := w.offFlags ++ p.offFlags.filter (·.1 != "ZoneDungeon") }
  let ops : OriginalSurfaceNightSpawn.Ops (StateT (Array OriginalSurfaceNightSpawn.Birth) M) := {
    next := fun site max => (nextN site max : M Int)
    nextRange := fun site lo hi => (nextR site lo hi : M Int)
    rollLuck := fun site range => (rollLuck site p.luck range : M Int)
    rollOnlyBadLuck := fun site range => (rollOnlyBadLuck site p.luck range : M Int)
    countNPCs := fun t => do
      let born ← get
      return countNPC w t + Int.ofNat (born.filter (·.typeId == t)).size
    anyHelpfulFairies := (unsupported s!"{s}: NPC.AnyHelpfulFairies (fairy branch)" : M Bool)
    spawn := fun _ birth => modify (·.push birth) }
  let (result, _) ← (OriginalSurfaceNightSpawn.surfaceNight ops c).run #[]
  match result with
  | .error (.boundary site why) => unsupported s!"{site}: {why}"
  | .ok (.nothing site) => return .nothing site
  | .ok (.spawned births) =>
    if births.any (fun b => b.ai2 != 0 || b.targetClosest) then
      unsupported s!"{s}: a night birth with ai2 / TargetClosest"
    return .spawned (births.map fun b => (b.typeId, b.x, b.y))

/-- `SpawnAnNPC(x, y, tileType, wallType, inSafeX, player)` for a surface tile in the day. -/
def spawnAnNPC (w : World) (p : Player) (f : TileFlags) (x y : Int) (tileType wallType : Nat)
    (inSafeX friendly livingTree noWorms : Bool) : M Outcome := do
  let s := "Spawner::SpawnAnNPC"
  let ty := Int.ofNat tileType
  let waterTile := f.waterTile
  let bx := x * 16 + 8
  let by' := y * 16
  -- IL_0009 GetZombieSettings: Next(7) (Skyblock.lowTiles off)
  let zombieFamily ← nextN "Spawner::GetZombieSettings IL_0016" 7
  -- IL_000e GetGnomeChance = 10
  let gnomeChance : Int := 10
  -- IL_0042 TooWindyForButterflies: |windSpeedTarget| >= 0.4f
  let tooWindy := !(w.windSpeedTarget.abs < r4 0.4000000059604645)
  -- IL_0070 … IL_0c36: tower zones, sky, invaders, and dual dungeon are off. The marble / granite
  -- flags are not read here: their only reads (IL_c568, IL_c605) lie behind the cavern guard below.
  if wallType == 244 then unsupported s!"{s} IL_0c36: living-tree wall"
  if w.readyToFindBartender then unsupported s!"{s} IL_1029: bartender"
  if (← tileAt w x y).wall == 62 then unsupported s!"{s} IL_1091: spider wall"
  -- IL_11d1 SpawnTileOrAboveHasAnyWallInSet(x, y, AllowsUndergroundDesertEnemiesToSpawn)
  if inWorld w x y 2 then
    let a := (← tileAt w x y).wall
    let b := (← tileAt w x (y - 1)).wall
    if flagIn w.desertWalls a || flagIn w.desertWalls b then unsupported s!"{s} IL_11d6: desert wall"
  if f.undergroundDesert then unsupported s!"{s} IL_11de: underground desert wall nearby"
  if !w.isThereAWorldSurface then unsupported s!"{s} IL_1640: no world surface"
  -- IL_171c: a friendly spawn skips the ocean-water check when the angler is saved or present (no draw either way)
  if waterTile && f.isOcean then unsupported s!"{s} IL_174e: ocean water"
  if !waterTile && !w.savedAngler && !anyNPC w 376 &&
      (x < w.beachDistance || x > w.maxTilesX - w.beachDistance) &&
      sandTile tileType && Float.ofInt y < w.worldSurface then
    unsupported s!"{s} IL_1c69: sleeping angler"
  -- IL_1cb5 / IL_1f20: skipped when friendly
  if !friendly && waterTile && (f.deeperThanRockLayer || tileType == 60) then unsupported s!"{s} IL_1cd1: deep or jungle water"
  if !friendly && waterTile && Float.ofInt y > w.worldSurface then unsupported s!"{s} IL_1f3d: underground water"
  if waterTile then
    if (← nextN s!"{s} IL_1fce" 4) == 0 then unsupported s!"{s} IL_1fd3: water critter branch"
  -- IL_252f: the friendly Old Shaking Chest precedes the Elder Slime unlock.
  if w.downedSkeletron then
    if (← rollLuck s!"{s} IL_2539" p.luck 20) == 0 &&
        !waterTile && f.deeperThanRockLayer && y < w.maxTilesY - 210 &&
        !w.unlockedSlimeOldSpawn && !anyNPC w 685 then
      return ← spawnNPC bx by' 685 p.luck
  if friendly then return ← friendlyBranch w p f y tileType tooWindy inSafeX bx by'
  if p.zoneDungeon then return ← dungeonSpawn w p f x y
  -- IL_45d5 … IL_7094: meteor and DD2 are off; the surface-night and cavern-remix branches are
  -- skipped by day (IL_4635, IL_5dea) or below the surface line (IL_462b, IL_5de0, IL_706d)
  if !w.dayTime then return ← surfaceNightBranch w p f x y tileType wallType livingTree tooWindy zombieFamily
  let below := Float.ofInt y > w.worldSurface
  -- IL_747f CheckToSpawnUndergroundFairy
  if w.fairyLog then
    if (← rollLuck "Spawner::CheckToSpawnUndergroundFairy IL_0036" p.luck 500) == 0 then
      if !(Float.ofInt y < (w.worldSurface + w.rockLayer) / 2.0) && y < w.maxTilesY - 300 then
        unsupported "CheckToSpawnUndergroundFairy IL_0068: fairy height"
  -- IL_74f6 … IL_7521: by day a wall behind the tile matters only when the player stands on a
  -- living-tree wall (Spawner.livingTree): then a gnome roll follows
  if !waterTile && (← tileAt w x y).wall > 0 && livingTree then unsupported s!"{s} IL_7527: living-tree gnome"
  -- IL_75ac CheckToSpawnUndergroundGnomes(x, y, !corrupt && !crimson && !waterTile, 10)
  if !waterTile then
    if (← rollLuck "Spawner::CheckToSpawnUndergroundGnomes IL_0018" p.luck gnomeChance) == 0 then
      if !(Float.ofInt y < w.worldSurface * 0.800000011920929) && !(Float.ofInt y > w.worldSurface * 1.100000023841858) then
        let count := countNPC w 624
        let n ← nextN "Spawner::CheckToSpawnUndergroundGnomes IL_008b" 3
        if !(count > n) then
          let wall := (← tileAt w x y).wall
          -- IL_00a6: by day a tile without a wall never gets a gnome; walls 2, 63, 196…199 do
          if !(w.dayTime && wall ≤ 0) && (wall == 2 || wall == 63 || (196 ≤ wall && wall ≤ 199)) then
            return ← spawnNPC bx by' 624 p.luck
  -- IL_762c / IL_781c / IL_7e7f / IL_7ec6: mushroom (70) and jungle (60) tiles branch off first
  if tileType == 70 || tileType == 60 then unsupported s!"{s} IL_762c: tile type {tileType} has its own branch"
  -- IL_7f10 / IL_7fdd / IL_80ab: underground critters (snow, crimson, corruption, jungle, hallow zones are off)
  if below && y < w.maxTilesY - 210 then
    if (← nextN s!"{s} IL_7f67" 8) == 0 then
      return .spawned [(if (← rollLuck s!"{s} IL_7f74" p.luck w.goldCritterChance) == 0 then 448 else 357, bx, by')]
  if below && y < w.maxTilesY - 210 then
    if (← nextN s!"{s} IL_8035" 13) == 0 then
      return .spawned [(if (← rollLuck s!"{s} IL_8042" p.luck w.goldCritterChance) == 0 then 447 else 300, bx, by')]
  if below && Float.ofInt y < (w.rockLayer + Float.ofInt w.maxTilesY) / 2.0 then
    if (← nextN s!"{s} IL_80f5" 13) == 0 then return .spawned [(359, bx, by')]
  -- IL_75e9 … IL_96aa: tile-type branches (mushroom, jungle, crimson, corruption, sand …)
  for bad in [70, 60, 225, 226, 232, 204, 199, 200, 203, 234, 662, 22, 23, 25, 112, 163, 661] do
    if ty == bad then unsupported s!"{s}: tile type {bad} has its own branch"
  if wallType == 86 then unsupported s!"{s} IL_87b3: wall 86"
  if !f.surfaceSpawn then
    -- IL_b8a0: the underground branch; only the layer above the rock layer (Spawner.underGround) is transcribed
    if Float.ofInt y > w.rockLayer then unsupported s!"{s} IL_b8a6: cavern enemy branch"
    -- IL_b8ab: giant worm (ZoneSnow, hardMode: off)
    if !noWorms then
      if (← nextN s!"{s} IL_b8bd" 50) == 0 then return .spawned [(10, bx, by')]
    -- IL_bace: ice slime on snow and ice, else GetBasicSlimeToSpawn(false, tileType, -1)
    if tileType == 147 || tileType == 161 then return ← spawnNPC bx by' 147 p.luck
    let g := "Spawner::GetBasicSlimeToSpawn"
    -- (sequential on purpose: a nested `(← …)` in an `else if` would be hoisted and drawn every time)
    let mut slime : Int := -9
    if (← nextN s!"{g} IL_00b8" 5) != 0 then
      slime := if (← nextN s!"{g} IL_00ca" 2) == 0 then 1 else -8
    modify fun t => { t with slimeChoice := some slime }
    return ← spawnNPC bx by' slime p.luck
  -- IL_96af: far from the world centre
  let off := i2f ((x - idiv w.maxTilesX 2).natAbs : Int) / i2f (idiv w.maxTilesX 2)
  if off > r4 0.33000001311302185 then unsupported s!"{s} IL_96d2: outer third of the world"
  -- IL_98cb: the daytime surface branch
  let dist := Int.ofNat (Int.natAbs (x - w.spawnTileX))
  if !waterTile && dist < idiv w.maxTilesX 2 then
    if (← nextN s!"{s} IL_98f9" 15) == 0 then
      if let some out ← critterBranch w p f y tileType tooWindy bx by' then return out
  if !waterTile && x > w.beachDistance && x < w.maxTilesX - w.beachDistance then
    if (← nextN s!"{s} IL_9f78" 12) == 0 && tileType == 53 then unsupported s!"{s} IL_9f82: sand"
  let raining := (w.offFlags.lookup "raining").getD false
  if tileType == 2 || tileType == 477 || tileType == 53 then
    -- IL_9fe0: not in the rain
    if !tooWindy && !raining && w.dayTime then
      if (← nextN s!"{s} IL_9ffa" 3) != 0 then
        if Float.ofInt y > w.worldSurface then unsupported s!"{s} IL_a00b"
        -- NPC.FindCattailTop(x, y, …): cattails (519) within x ± 30, y ± 20
        if inWorld w x y 31 then
          for i in [0:61] do
            for j in [0:41] do
              let c ← tileAt w (x - 30 + i) (y - 20 + j)
              if c.active && c.type == 519 then unsupported s!"{s} IL_a01d: cattail nearby"
  -- IL_a125: morning birds (before 18000 ticks after dawn)
  let grass := tileType == 2 || tileType == 477 || tileType == 109 || tileType == 492
  if !waterTile && dist < idiv w.maxTilesX 3 && w.dayTime && w.time < 18000.0 && grass then
    if (← nextN s!"{s} IL_a17d" 4) == 0 && !(Float.ofInt y > w.worldSurface) &&
        countNPC w 74 + countNPC w 297 + countNPC w 298 < 6 then
      return ← birds w p s!"{s} IL_a1bc" s!"{s} IL_a1c9" bx by'
  -- IL_a29a: birds
  if !waterTile && dist < idiv w.maxTilesX 3 then
    if (← nextN s!"{s} IL_a2ba" 15) == 0 && (grass || tileType == 147) then
      return ← birds w p s!"{s} IL_a2ee" s!"{s} IL_a2fb" bx by'
  if !waterTile && dist > idiv w.maxTilesX 3 then unsupported s!"{s} IL_a3dd: far from the spawn point"
  if !waterTile && dist > idiv w.maxTilesX 8 then unsupported s!"{s} IL_a42f: far from the spawn point"
  if !inSafeX && tileType == 53 then unsupported s!"{s} IL_a51c"
  if tileType == 53 then unsupported s!"{s} IL_a7e5"
  -- IL_a88e..a921: the rain: a Flying Fish (1 in 4), else off water an Umbrella Slime (1 in 2)
  if raining then
    if (← nextN s!"{s} IL_a89c" 4) == 0 then return ← spawnNPC bx by' 224 p.luck
  if !waterTile && raining then
    if (← nextN s!"{s} IL_a8ea" 2) == 0 then return ← spawnNPC bx by' 225 p.luck
  -- The windy-day choices read the direction from SetSpawnFlagsForChosenTile and the stored wall type.
  let playerX := idiv (truncF32 p.center.1) 16
  let inWindDirection := i2f (playerX - x) * w.windSpeedTarget > 0
  if !waterTile && wallType == 0 && w.windyDay && inWindDirection then
    if (← nextN s!"{s} IL_a943" 3) != 0 then return ← spawnNPC bx by' 594 p.luck
    if tileType == 2 || tileType == 477 then
      if (← nextN s!"{s} IL_a9a9" 10) != 0 && (← tileAt w x y).type == tileType then
        return ← spawnNPC bx by' 628 p.luck
  if waterTile then return .nothing s!"{s} IL_a9fb"
  let slime ← basicSlime w tileType dist
  modify fun t => { t with slimeChoice := some slime }
  spawnNPC bx by' slime p.luck

/-! ## One `NPC.SpawnNPC()` call -/

/-- `NPC.SpawnNPC()` with `noSpawnCycle` false and one active player `p`. -/
def run (w : World) (p : Player) : M Outcome := do
  if w.noSpawnCycle then return .noSpawnCycle
  -- `Main.raining` (Spawner.raining, SetSpawnFlags IL_0042) is read only inside SpawnAnNPC, where the
  -- transcribed paths take it (rain day critters, Flying Fish, Umbrella Slime; the night surface path)
  for (name, v) in w.offFlags do
    if v && name != "raining" then unsupported s!"world flag {name} is set"
  for (name, v) in p.offFlags do
    if v && name != "ZoneDungeon" then unsupported s!"player flag {name} is set"
  if w.otherActivePlayers then unsupported "more than one active player"
  -- Spawner.SpawnNPC: CanSpawnEnemiesNear
  if !p.active || p.dead then return .notEligible
  -- TrySpawnAnNPC
  let (sr, ms, friendly) ← spawnRates w p
  modify fun t => { t with spawnRate := some sr, maxSpawns := some ms }
  if !(p.nearbyActiveNPCs < i2f ms) then return .tooManyNPCs
  let roll ← nextN "Spawner::TrySpawnAnNPC IL_0024" sr
  if roll != 0 then return .rateRoll roll
  let a ← spawnArea w p
  modify fun t => { t with spawnRect := some a.spawn, safeRect := some a.safe }
  let some (x, y, inSafeX) ← findSpawnTile w a friendly | return .noSpawnTile
  modify fun t => { t with chosen := some (x, y, inSafeX) }
  if !(← notOnScreen w p a x y) then return .onScreen x y
  let g ← findGround w x y
  let tileType := (← tileAt w x g).type
  let wallType ← spawnWallType w x y
  modify fun t => { t with groundY := some g, tileType := some tileType, wallType := some wallType }
  if let some why ← postCheck w x y tileType wallType p.zoneDungeon then return .postCheck why
  let (cx, cy) := p.center
  let pX := idiv (truncF32 cx) 16
  let pY := idiv (truncF32 cy) 16
  let f ← chosenTileFlags w pX pY x y tileType p.zoneDungeon
  modify fun t => { t with waterTile := some f.waterTile }
  if f.isBeach then unsupported "beach"
  -- Spawner.livingTree (SetSpawnFlags IL_02f8): the player's tile has wall 244
  let livingTree := inWorld w pX pY 0 && (← tileAt w pX pY).wall == 244
  -- Spawner.noWorms: a house wall at the player's tile (SetSpawnFlags IL_0220), any town NPC nearby
  -- (GetSpawnRate IL_0c71 / IL_0d27 / IL_0dd1) or an AFK player (noGroundWorms, SetSpawnFlagsForChosenTile IL_0012)
  let houseWall := inWorld w pX pY 0 && flagIn w.wallHouse (← tileAt w pX pY).wall
  -- ShadowCandle clears townNPCs, noWorms and noGroundWorms at the end
  -- of SetSpawnFlags, including the AFK restriction established earlier.
  let noWorms := !p.candleZones.shadow &&
    (houseWall || p.townNPCs ≥ 1 || p.afkCounter ≥ w.afkTimeNeededForNoWormSpawns)
  spawnAnNPC w p f x y tileType wallType inSafeX friendly livingTree noWorms

/-- Run one attempt from a random-stream state; the trace keeps everything computed so far even
when the result is `unsupported` or `other`. -/
def attempt (w : World) (p : Player) (rng : Rng) : Except Stop Outcome × Trace :=
  (run w p).run.run { rng := rng }

end Lean4Terria.OriginalForestSpawn
