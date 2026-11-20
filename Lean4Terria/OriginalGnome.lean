import Lean4Terria.OriginalSurfaceNightSpawn

/-!
# Gnome (NPC 624) — spawn selection and Classic defaults (Terraria 1.4.5.8 desktop)

STAGE 1 of the Gnome leaf (see `.lake/original-gnome/STATUS.md`): the two gnome decisions of
`Spawner.SpawnAnNPC`, valid by day AND night, and the `NPC.SetDefaults` / `NewNPC` fields of the born gnome.
Evidence: `.lake/original-gnome/`.

* `livingTreeGnome` = `SpawnAnNPC` IL_74f1 … IL_758a (the player stands on a living-tree wall; by day the spawn tile
  needs a wall > 0).
* `undergroundGnome` = IL_758b … IL_75e3, i.e. `Spawner.CheckToSpawnUndergroundGnomes` and its birth. Its day-only
  rule (no wall → no gnome) is IL_00a6; the `RollLuck(chance)` and `Next(3)` draws happen before that rule.
* Both use the shared stream through `OriginalSurfaceNightSpawn.Ops`, in the order of the CIL, so the caller can run them
  from IL_74f1 for either time of day (the night leaf `surfaceNight` keeps its own night-only copy of the same code).
-/

namespace Lean4Terria.OriginalGnome

open OriginalSurfaceNightSpawn (Ops Birth)

structure Context where
  x : Int
  y : Int
  /-- `Main.tile[x, y].wall` -/
  wallAtTile : Nat
  worldSurface : Float
  dayTime : Bool
  waterTile : Bool
  /-- `Spawner.livingTree` -/
  livingTree : Bool
  /-- `Spawner.ZoneCorrupt` / `ZoneCrimson` -/
  zoneCorrupt : Bool := false
  zoneCrimson : Bool := false
  /-- `GetGnomeChance()` (10 outside Skyblock / remix) -/
  gnomeChance : Int := 10
  /-- `Main.eclipse`, `Main.bloodMoon`, `Main.remixWorld`: this scope needs them false -/
  eclipse : Bool := false
  bloodMoon : Bool := false
  remixWorld : Bool := false

private def site (il : String) : String := "Spawner::SpawnAnNPC " ++ il

private def gnomeBirth (c : Context) : Birth := { typeId := 624, x := c.x * 16 + 8, y := c.y * 16 }

/-- IL_74f1 … IL_758a. By day the wall behind the tile must exist (IL_750d). -/
def livingTreeGnome [Monad m] (ops : Ops m) (c : Context) : m (Option Birth) := do
  if c.remixWorld || c.waterTile then return none
  if c.dayTime && c.wallAtTile == 0 then return none
  if !c.livingTree || c.eclipse || c.bloodMoon then return none
  if (← ops.rollLuck (site "IL_753c") (c.gnomeChance * 3)) != 0 then return none
  let count ← ops.countNPCs 624
  let roll ← ops.next (site "IL_7553") 3
  if count > roll then return none
  ops.spawn (site "IL_755a") (gnomeBirth c)
  return some (gnomeBirth c)

/-- `Spawner.CheckToSpawnUndergroundGnomes(x, y, allowed, chance)` for any time of day. -/
def checkGnomes [Monad m] (ops : Ops m) (c : Context) (allowed : Bool) : m Bool := do
  if !allowed then return false
  if c.eclipse || c.bloodMoon then return false
  if (← ops.rollLuck "Spawner::CheckToSpawnUndergroundGnomes IL_0018" c.gnomeChance) != 0 then return false
  let low := c.worldSurface * 0.800000011920929
  let high := c.worldSurface * 1.100000023841858
  if Float.ofInt c.y < low || Float.ofInt c.y > high then return false
  let count ← ops.countNPCs 624
  let roll ← ops.next "Spawner::CheckToSpawnUndergroundGnomes IL_008b" 3
  if count > roll then return false
  -- IL_00a6: by day a tile without a wall never gets a gnome
  if c.dayTime && c.wallAtTile == 0 then return false
  let w := c.wallAtTile
  return w == 2 || w == 63 || (196 ≤ w && w ≤ 199)

/-- IL_758b … IL_75e3: the birth after a successful check. -/
def undergroundGnome [Monad m] (ops : Ops m) (c : Context) : m (Option Birth) := do
  if c.remixWorld then return none
  if ← checkGnomes ops c (!c.zoneCorrupt && !c.zoneCrimson && !c.waterTile) then
    ops.spawn (site "IL_75b3") (gnomeBirth c)
    return some (gnomeBirth c)
  return none

/-- Both decisions, in CIL order: the living-tree gnome first, then the underground check. -/
def gnomeSelection [Monad m] (ops : Ops m) (c : Context) : m (Option Birth) := do
  if let some b ← livingTreeGnome ops c then return some b
  undergroundGnome ops c

/-! ## Defaults of the born gnome

`NPC.NewNPC(624)` = `SetDefaults(624)` plus the `NewNPC` assignments, observed on every native birth (Classic and Expert).
Expert (`GameMode` 1) doubles life and damage and sets the knockback resistance to 0.9. -/

structure Defaults where
  width : Int := 14
  height : Int := 30
  aiStyle : Int := 3
  life : Int
  lifeMax : Int
  damage : Int
  defense : Int := 0
  /-- Single bits of `knockBackResist` -/
  knockBackResist : Float32
  rarity : Int := 1
  scale : Float32 := 1
  alpha : Int := 0
  value : Float32 := 0
  npcSlots : Float32 := 1
  direction : Int := 0
  spriteDirection : Int := -1
  target : Int := 255
  timeLeft : Int := 937
  deriving Repr, BEq

/-- Classic (`expert = false`) and Expert defaults of a freshly born gnome. -/
def defaults (expert : Bool) : Defaults :=
  if expert then { life := 50, lifeMax := 50, damage := 20, knockBackResist := 0.9 }
  else { life := 25, lifeMax := 25, damage := 10, knockBackResist := 1 }

end Lean4Terria.OriginalGnome
