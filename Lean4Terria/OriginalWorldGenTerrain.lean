import Lean4Terria.OriginalWorldGenPlan

/-!
# World-generation terrain primitives of desktop Terraria 1.4.5.8 (`Terraria.exe` sha256 `d52c7ea7…03e0`)

Hand transcription of the real desktop CIL (`.lake/original-world-gen-terrain/harness/dumpsel-*/il/`):

* `WorldGen.TileRunner(i, j, strength, steps, type, addTile, speedX, speedY, noYChange, overRide,
  ignoreTileType)` 100668338 — every branch: the drunk / remix / getGood strength and step
  scaling, the `num3` liquid kind roll (notTheBees + dontStarve, drunk + tenth + remix,
  getGood), the per-cell diamond test, `GenVars.mudWall` walls 15 / 64, negative types (clear,
  `-2` liquid fill with lava below `lavaLine` / remix ocean rule), walls 180 / 178 for types
  368 / 367, the `overRide` protection switch (stone, `CanBeClearedDuringGeneration`, sand 53,
  ice 147, 45, 189‥190, 196, 460, 717‥719, 367‥368, 396‥397), `ClearSlope` unless
  `SaveSlopes`, `addTile`, `noYChange` dirt-wall 2, jungle (59) water removal, the drunk
  jitter, the 12-level speed-up chain (50 … 900), velocity clamps and the jungle vertical rule.
* `WorldGen.PlaceWall` 100668324 (bounds, null tile, wall only when empty, `SquareWallFrame`
  100668361 = 9 `Framing.WallFrame` calls; `mute` is always true from TileRunner so no sound).
  `Framing.WallFrame` 100668514 is transcribed (`wallFrame`, data in `WallFrameTables`) and passed
  as `Callees.wallFrame`: it draws from `WorldGen.genRand`, so the random stream is threaded through it.
* `WorldGen.CanBeClearedDuringGeneration` 100668358, `oceanDepths` 100667769, `oceanLevel`
  100667768, `GetWorldSize` 100667688; the `Tile` bit accessors used (`active`, `lava`, `honey`,
  `shimmer`, `liquidType`, `slope`, `halfBrick`, `ClearSlope`).
* `TerrainPass.ApplyPass` 100677723, `GenerateWorldSurfaceOffset` 100677726, `FillColumn`
  100677724, `RetargetColumn` 100677725, `RetargetSurfaceHistory` 100677727 and
  `TerrainPass/SurfaceHistory` (ring buffer of 500 heights).
* `UnifiedRandom.Next(max)` / `Next(min, max)` / `GetSampleForLargeRange` 100669669 on top of the
  released `OriginalWorldGenPlan.UnifiedRandom` (`genRand` is `Main.rand`).

Doubles are Lean `Float` (IEEE binary64, the CIL `r8` arithmetic); `conv.i4` of a double
truncates and faults when NaN / out of range (runtime-dependent natively).
-/
namespace Lean4Terria.OriginalWorldGenTerrain

open Lean4Terria.OriginalWorldGenPlan (UnifiedRandom GenerationProgress)

inductive Fault where
  | indexOutOfRange (array : String) (detail : String)
  | argumentOutOfRange (detail : String)
  | runtimeDependentConversion (value : Float)
  | callee (name : String) (detail : String)
deriving Repr, Inhabited

/-! ## Random numbers (`UnifiedRandom`) -/

/-- `conv.i4` of a `double`. -/
def convI4 (v : Float) : Except Fault Int32 :=
  if v.isNaN || v ≤ -2147483649 || v ≥ 2147483648 then .error (.runtimeDependentConversion v)
  else
    let t := if v < 0 then -(((-v).floor).toUInt64.toNat : Int) else ((v.floor).toUInt64.toNat : Int)
    .ok (Int32.ofInt t)

/-- `conv.i8` of a `double` (only used by `Next(min, max)` large ranges). -/
def convI8 (v : Float) : Except Fault Int :=
  if v.isNaN || v ≤ -9.3e18 || v ≥ 9.2e18 then .error (.runtimeDependentConversion v)
  else .ok (if v < 0 then -(((-v).floor).toUInt64.toNat : Int) else ((v.floor).toUInt64.toNat : Int))

/-- `GetSampleForLargeRange()`. -/
def sampleLarge (r : UnifiedRandom) : Float × UnifiedRandom :=
  let (a, r) := r.internalSample
  let (b, r) := r.internalSample
  let a := if b % 2 == 0 then -a else a
  ((Float.ofInt a.toInt + 2147483646) / 4294967293, r)

/-- `Next(maxValue)`. -/
def nextMax (r : UnifiedRandom) (maxValue : Int32) : Except Fault (Int32 × UnifiedRandom) := do
  if maxValue < 0 then throw (.argumentOutOfRange "maxValue")
  let (s, r) := r.sample
  return (← convI4 (s * Float.ofInt maxValue.toInt), r)

/-- `Next(minValue, maxValue)`. -/
def nextRange (r : UnifiedRandom) (lo hi : Int32) : Except Fault (Int32 × UnifiedRandom) := do
  if lo > hi then throw (.argumentOutOfRange "minValue")
  let range : Int := hi.toInt - lo.toInt
  if range ≤ 2147483647 then
    let (s, r) := r.sample
    return ((← convI4 (s * Float.ofInt range)) + lo, r)
  let (s, r) := sampleLarge r
  return (Int32.ofInt ((← convI8 (s * Float.ofInt range)) + lo.toInt), r)

/-! ## Tiles and the world -/

/-- The fields of `Terraria.Tile`. -/
structure Tile where
  type : UInt16
  wall : UInt16
  liquid : UInt8
  sTileHeader : UInt16
  bTileHeader : UInt8
  bTileHeader2 : UInt8
  bTileHeader3 : UInt8
  frameX : Int16
  frameY : Int16
deriving Inhabited, Repr, BEq

namespace Tile
/-- `new Tile()`: every field 0. -/
def empty : Tile := ⟨0, 0, 0, 0, 0, 0, 0, 0, 0⟩
def active (t : Tile) : Bool := t.sTileHeader &&& 32 == 32
def setActive (t : Tile) (b : Bool) : Tile :=
  { t with sTileHeader := if b then t.sTileHeader ||| 32 else t.sTileHeader &&& 65503 }
def setLava (t : Tile) (b : Bool) : Tile :=
  { t with bTileHeader := if b then (t.bTileHeader &&& 159) ||| 32 else t.bTileHeader &&& 223 }
def setHoney (t : Tile) (b : Bool) : Tile :=
  { t with bTileHeader := if b then (t.bTileHeader &&& 159) ||| 64 else t.bTileHeader &&& 191 }
def setShimmer (t : Tile) (b : Bool) : Tile :=
  { t with bTileHeader := if b then (t.bTileHeader &&& 159) ||| 96 else t.bTileHeader &&& 159 }
/-- `liquidType(int)`: 0 clears the kind bits, 1 lava, 2 honey, 3 shimmer, others no-op. -/
def setLiquidType (t : Tile) (k : Int32) : Tile :=
  if k == 0 then { t with bTileHeader := t.bTileHeader &&& 159 }
  else if k == 1 then t.setLava true else if k == 2 then t.setHoney true
  else if k == 3 then t.setShimmer true else t
def setSlope (t : Tile) (s : UInt8) : Tile :=
  { t with sTileHeader := (t.sTileHeader &&& 36863) ||| (((s &&& 7).toUInt16) <<< (12 : UInt16)) }
def setHalfBrick (t : Tile) (b : Bool) : Tile :=
  { t with sTileHeader := if b then t.sTileHeader ||| 1024 else t.sTileHeader &&& 64511 }
/-- `ClearSlope()`: `slope(0)`, `halfBrick(false)`. -/
def clearSlope (t : Tile) : Tile := (t.setSlope 0).setHalfBrick false
end Tile

/-- `Main.tile` (a `Tile[,]` of `width × height`, row-major on the last index) and the world size
statics `Main.maxTilesX/Y`. The array may be larger than the world, as natively. -/
structure World where
  width : Int32
  height : Int32
  tiles : Array Tile
  maxTilesX : Int32
  maxTilesY : Int32
deriving Inhabited

namespace World
def create (width height maxX maxY : Int32) : World :=
  { width, height, tiles := Array.replicate (width.toInt.toNat * height.toInt.toNat) Tile.empty,
    maxTilesX := maxX, maxTilesY := maxY }
def index (w : World) (x y : Int32) : Except Fault Nat :=
  if x < 0 || x ≥ w.width || y < 0 || y ≥ w.height then .error (.indexOutOfRange "Tile[,]" s!"{x},{y}")
  else .ok (x.toInt.toNat * w.height.toInt.toNat + y.toInt.toNat)
def get (w : World) (x y : Int32) : Except Fault Tile := do
  return w.tiles[← w.index x y]!
def set (w : World) (x y : Int32) (t : Tile) : Except Fault World := do
  let i ← w.index x y
  let tiles := w.tiles
  let w := { w with tiles := #[] }
  return { w with tiles := tiles.set! i t }
def modify (w : World) (x y : Int32) (f : Tile → Tile) : Except Fault World := do
  w.set x y (f (← w.get x y))
end World

/-! ## Inputs -/

/-- Static `bool[]` tile-property tables TileRunner reads (`Main.tileStone`, `tileFrameImportant`,
`tileCut`, `TileID.Sets.SaveSlopes`, `Sets.Ore`, `Sets.CanBeClearedDuringGeneration`). -/
structure TileSets where
  tileStone : Array Bool
  tileFrameImportant : Array Bool
  tileCut : Array Bool
  saveSlopes : Array Bool
  ore : Array Bool
  canBeClearedDuringGeneration : Array Bool
deriving Inhabited

def flag (name : String) (a : Array Bool) (i : Int) : Except Fault Bool :=
  if 0 ≤ i && i < a.size then .ok a[i.toNat]! else .error (.indexOutOfRange name s!"{i}")

/-- World-generation flags and statics TileRunner / Terrain read. -/
structure GenEnv where
  drunkWorldGen : Bool
  remixWorldGen : Bool
  getGoodWorldGen : Bool
  notTheBees : Bool
  dontStarveWorldGen : Bool
  tenthAnniversaryWorldGen : Bool
  /-- `WorldGen.isGeneratingOrLoadingWorld`. -/
  isGeneratingOrLoadingWorld : Bool
  /-- `SecretSeed.dualDungeons.Enabled`, `noSurface`, `surfaceIsInSpace`. -/
  dualDungeons : Bool
  noSurface : Bool
  surfaceIsInSpace : Bool
  /-- `WorldGen.beachDistance`. -/
  beachDistance : Int32
deriving Inhabited, Repr

/-- `GenVars` / `Main` doubles and lines TileRunner reads (set by earlier passes). -/
structure GenVars where
  mudWall : Bool
  waterLine : Int32
  lavaLine : Int32
  /-- `GenVars.UndergroundDesertLocation` (x, y, w, h). -/
  undergroundDesert : Int32 × Int32 × Int32 × Int32
  worldSurface : Float
  rockLayer : Float
deriving Inhabited, Repr

/-- Callees. `wallFrame w x y resetFrame r` is `Framing.WallFrame(x, y, resetFrame)` (it may draw
from `genRand`); the dungeon pair is only reached on dual-dungeon seeds. -/
structure Callees where
  wallFrame : World → Int32 → Int32 → Bool → UnifiedRandom → Except Fault (World × UnifiedRandom)
  inAnyPotentialDungeonBounds : Int32 → Int32 → Except Fault Bool
  isConsideredDungeonTile : Int32 → Except Fault Bool

/-- `WorldGen.CanBeClearedDuringGeneration(type, x, y, ignoreDungeons)`. -/
def canBeClearedDuringGeneration (c : Callees) (env : GenEnv) (sets : TileSets) (type x y : Int32)
    (ignoreDungeons : Bool) : Except Fault Bool := do
  if env.isGeneratingOrLoadingWorld && !ignoreDungeons && env.dualDungeons then
    if ← c.inAnyPotentialDungeonBounds x y then
      if ← c.isConsideredDungeonTile type then return false
  flag "CanBeClearedDuringGeneration" sets.canBeClearedDuringGeneration type.toInt

/-- `WorldGen.oceanLevel`. -/
def oceanLevel (gv : GenVars) : Float := (gv.worldSurface + gv.rockLayer) / 2 + 40

/-- `WorldGen.oceanDepths(x, y)`: `y > oceanLevel` → false (`ble.un`, so NaN continues), else
`x < beachDistance || x > maxTilesX - beachDistance`. -/
def oceanDepths (env : GenEnv) (gv : GenVars) (w : World) (x y : Int32) : Bool :=
  if Float.ofInt y.toInt > oceanLevel gv then false
  else x < env.beachDistance || x > w.maxTilesX - env.beachDistance

/-- `WorldGen.PlaceWall(x, y, type, mute = true)`. -/
def placeWall (c : Callees) (w : World) (r : UnifiedRandom) (x y : Int32) (type : Int32) :
    Except Fault (World × UnifiedRandom) := do
  if x ≤ 1 || y ≤ 1 || x ≥ w.maxTilesX - 2 || !(y < w.maxTilesY - 2) then return (w, r)
  let t ← w.get x y
  if t.wall != 0 then return (w, r)
  let w ← w.set x y { t with wall := type.toUInt32.toUInt16 }
  -- SquareWallFrame(x, y, true): nine WallFrame calls, the centre with resetFrame
  let mut w := w
  let mut r := r
  for (dx, dy, reset) in [((-1 : Int32), (-1 : Int32), false), (-1, 0, false), (-1, 1, false), (0, -1, false), (0, 0, true),
      (0, 1, false), (1, -1, false), (1, 0, false), (1, 1, false)] do
    (w, r) ← c.wallFrame w (x + dx) (y + dy) reset r
  return (w, r)

/-! ## `Framing.WallFrame` (100668514) -/

/-- Data `Framing.WallFrame` reads: `WallID.Count`, `TileID.Sets.TruncatesWalls`,
`Main.wallLargeFrames`, and the `Framing.Initialize` lookups (`phlebasTileFrameNumberLookup[4][3]`,
`lazureTileFrameNumberLookup[2][2]`, `centerWallFrameLookup[3][3]`, `wallFrameLookup[20][3]` of
`Point16`), plus `Main.ShouldShowInvisibleBlocksAndWalls()` (scene state). Supplied by the caller
(native values in the harness corpus). -/
structure WallFrameTables where
  wallCount : UInt16
  truncatesWalls : Array Bool
  wallLargeFrames : Array UInt8
  phlebas : Array (Array Int32)
  lazure : Array (Array Int32)
  center : Array (Array Int32)
  wallFrameLookup : Array (Array (Int32 × Int32))
  showInvisibleBlocksAndWalls : Bool
deriving Inhabited

def at2 {α : Type} [Inhabited α] (name : String) (a : Array (Array α)) (i j : Int32) : Except Fault α :=
  if 0 ≤ i && i.toInt < a.size then
    let row := a[i.toInt.toNat]!
    if 0 ≤ j && j.toInt < row.size then .ok row[j.toInt.toNat]! else .error (.indexOutOfRange name s!"{i},{j}")
  else .error (.indexOutOfRange name s!"{i},{j}")

namespace Tile
def invisibleWall (t : Tile) : Bool := t.bTileHeader3 &&& 64 == 64
def wallFrameNumber (t : Tile) : UInt8 := (t.bTileHeader2 &&& 192) >>> (6 : UInt8)
def setWallFrameNumber (t : Tile) (n : UInt8) : Tile := { t with bTileHeader2 := (t.bTileHeader2 &&& 63) ||| ((n &&& 3) <<< (6 : UInt8)) }
/-- `wallFrameX(int)`: `(bTH2 & 240) | ((v / 36) & 15)`. -/
def setWallFrameX (t : Tile) (v : Int32) : Tile := { t with bTileHeader2 := (t.bTileHeader2 &&& 240) ||| ((v / 36) &&& 15).toUInt32.toUInt8 }
/-- `wallFrameY(int)`: `(bTH3 & 248) | ((v / 36) & 7)`. -/
def setWallFrameY (t : Tile) (v : Int32) : Tile := { t with bTileHeader3 := (t.bTileHeader3 &&& 248) ||| ((v / 36) &&& 7).toUInt32.toUInt8 }
/-- `ClearWallPaintAndCoating()`: `wallColor(0)`, `fullbrightWall(false)`, `invisibleWall(false)`. -/
def clearWallPaintAndCoating (t : Tile) : Tile :=
  { t with bTileHeader := t.bTileHeader &&& 224, sTileHeader := t.sTileHeader &&& 32767, bTileHeader3 := t.bTileHeader3 &&& 191 }
end Tile

/-- `Framing.WallFrame(i, j, resetFrame)` with `genRand = r`. -/
def wallFrame (tb : WallFrameTables) (w : World) (i j : Int32) (resetFrame : Bool) (r : UnifiedRandom) :
    Except Fault (World × UnifiedRandom) := do
  if i ≤ 0 || j ≤ 0 || i ≥ w.maxTilesX - 1 || j ≥ w.maxTilesY - 1 then return (w, r)
  let mut t ← w.get i j
  if !(t.wall < tb.wallCount) then t := { t with wall := 0 }
  if t.wall == 0 then
    -- wallColor(0) then ClearWallPaintAndCoating()
    return (← w.set i j (({ t with bTileHeader := t.bTileHeader &&& 224 }).clearWallPaintAndCoating), r)
  let mut num : Int32 := 0
  let showInv := tb.showInvisibleBlocksAndWalls
  let side (x y : Int32) (bit : Int32) (num : Int32) : Except Fault Int32 := do
    let n ← w.get x y
    let solid ← if n.wall > 0 then pure true else if n.active then flag "TruncatesWalls" tb.truncatesWalls n.type.toNat else pure false
    return if solid && (showInv || !n.invisibleWall) then num ||| bit else num
  if j - 1 ≥ 0 then num ← side i (j - 1) 1 num
  if i - 1 ≥ 0 then num ← side (i - 1) j 2 num
  if !(i + 1 > w.maxTilesX - 1) then num ← side (i + 1) j 4 num
  if !(j + 1 > w.maxTilesY - 1) then num ← side i (j + 1) 8 num
  let large ← if t.wall.toNat < tb.wallLargeFrames.size then pure tb.wallLargeFrames[t.wall.toNat]!
    else throw (.indexOutOfRange "wallLargeFrames" s!"{t.wall}")
  let mut r := r
  let mut num2 : Int32 := 0
  if large == 1 then
    num2 := (← at2 "phlebasTileFrameNumberLookup" tb.phlebas (j % 4) (i % 3)) - 1
    t := t.setWallFrameNumber num2.toUInt32.toUInt8
  else if large == 2 then
    num2 := (← at2 "lazureTileFrameNumberLookup" tb.lazure (i % 2) (j % 2)) - 1
    t := t.setWallFrameNumber num2.toUInt32.toUInt8
  else if resetFrame then
    let (n, r2) ← nextRange r 0 3
    r := r2
    num2 := n
    if t.wall == 21 then
      let (m, r3) ← nextMax r 2
      r := r3
      if m == 0 then num2 := 2
    t := t.setWallFrameNumber num2.toUInt32.toUInt8
  else num2 := t.wallFrameNumber.toNat.toInt32
  if num == 15 then num := num + (← at2 "centerWallFrameLookup" tb.center (i % 3) (j % 3))
  let (px, py) ← at2 "wallFrameLookup" tb.wallFrameLookup num num2
  t := (t.setWallFrameX px).setWallFrameY py
  return (← w.set i j t, r)

/-! ## `WorldGen.TileRunner` -/

/-- The arguments of `TileRunner` (defaults as in the C# signature). -/
structure RunnerArgs where
  i : Int32
  j : Int32
  strength : Float
  steps : Int32
  type : Int32
  addTile : Bool := false
  speedX : Float := 0
  speedY : Float := 0
  noYChange : Bool := false
  overRide : Bool := true
  ignoreTileType : Int32 := -1
deriving Inhabited, Repr

/-- Distance test `|k - pos.X| + |l - pos.Y| < strength * factor * (1 + Next(-10, 11) * scale)`. -/
def within (r : UnifiedRandom) (k l : Int32) (px py strength factor scale : Float) :
    Except Fault (Bool × UnifiedRandom) := do
  let (n, r) ← nextRange r (-10) 11
  let d := Float.abs (Float.ofInt k.toInt - px) + Float.abs (Float.ofInt l.toInt - py)
  return (d < strength * factor * (1 + Float.ofInt n.toInt * scale), r)

/-- The per-cell body for `type ≥ 0` after the 368 / 367 walls (`IL_0814‥IL_09f8`). -/
def paintCell (c : Callees) (env : GenEnv) (gv : GenVars) (sets : TileSets) (a : RunnerArgs)
    (clearSlope : Bool) (w : World) (r : UnifiedRandom) (k l : Int32) : Except Fault (World × UnifiedRandom) := do
  let mut r := r
  let tile ← w.get k l
  let ty := a.type
  let mut keep := false
  if a.overRide && tile.active then
    keep := if ← flag "tileStone" sets.tileStone ty.toInt then tile.type != 1 else false
    if !(← canBeClearedDuringGeneration c env sets tile.type.toNat.toInt32 k l false) then keep := true
    let t := tile.type.toNat
    if t == 1 then
      if ty == 59 then
        let (n, r2) ← nextRange r (-50) 50
        r := r2
        if Float.ofInt l.toInt < gv.worldSurface + Float.ofInt n.toInt then keep := true
    else if t == 45 || t == 147 || t == 189 || t == 190 || t == 196 || t == 460 || (t ≥ 717 && t ≤ 719) then
      keep := true
    else if t == 53 then
      let (dx, dy, dw, dh) := gv.undergroundDesert
      if ty == 59 && dx ≤ k && k < dx + dw && dy ≤ l && l < dy + dh then keep := true
      if ty == 40 then keep := true
      if Float.ofInt l.toInt < gv.worldSurface && ty != 59 then keep := true
    else if t == 367 || t == 368 then
      if ty == 59 then keep := true
    else if t == 396 || t == 397 then
      keep := !(← flag "Ore" sets.ore ty.toInt)
  let mut tile := tile
  if !keep then
    tile := { tile with type := ty.toUInt32.toUInt16 }
    if clearSlope then tile := tile.clearSlope
  if a.addTile then
    tile := ({ tile.setActive true with liquid := 0 }).setLava false
  if a.noYChange && Float.ofInt l.toInt < gv.worldSurface && ty != 59 then tile := { tile with wall := 2 }
  if ty == 59 && l > gv.waterLine && tile.liquid > 0 then
    tile := { tile.setLava false with liquid := 0 }
  return (← w.set k l tile, r)

/-- Read-only per-step context of `TileRunner`'s cell loops. -/
structure RunnerBox where
  c : Callees
  env : GenEnv
  gv : GenVars
  sets : TileSets
  a : RunnerArgs
  clearSlope : Bool
  is368 : Bool
  is367 : Bool
  px : Float
  py : Float
  strength : Float

/-- One cell `(k, l)` of `TileRunner`'s inner loop (`IL_03a3‥IL_09f8`). The world and the random
generator are threaded by ownership so writes stay in place. -/
def runnerCell (b : RunnerBox) (num3 : Int32) (k l : Int32) (w : World) (r : UnifiedRandom) :
    Except Fault (World × UnifiedRandom) := do
  let c := b.c
  let env := b.env
  let gv := b.gv
  let sets := b.sets
  let a := b.a
  let maxY := w.maxTilesY
  let skipDrunk := env.drunkWorldGen && l < maxY - 300 && a.type == 57
  if skipDrunk then return (w, r)
  let tile ← w.get k l
  let skipFrame ← if tile.active then (do
      if ← flag "tileFrameImportant" sets.tileFrameImportant tile.type.toNat then
        return !(← flag "tileCut" sets.tileCut tile.type.toNat)
      else return false) else pure false
  if skipFrame then return (w, r)
  if a.ignoreTileType ≥ 0 && tile.active && tile.type.toNat.toInt32 == a.ignoreTileType then return (w, r)
  let (inside, r) ← within r k l b.px b.py b.strength 0.5 0.015
  if !inside then return (w, r)
  let mut w := w
  let mut r := r
  -- mud walls
  if gv.mudWall && Float.ofInt l.toInt > gv.worldSurface && (← w.get k (l - 1)).wall != 2 then
    let (n, r5) ← nextMax r 3
    r := r5
    if l < maxY - 210 - n then
      let (ins2, r6) ← within r k l b.px b.py b.strength 0.45 0.01
      r := r6
      if ins2 then
        let (n4, r7) ← nextRange r 0 4
        r := r7
        if l > gv.lavaLine - n4 - 50 then
          if (← w.get k (l - 1)).wall != 64 && (← w.get k (l + 1)).wall != 64 &&
              (← w.get (k - 1) l).wall != 64 && (← w.get (k + 1) l).wall != 64 then
            (w, r) ← placeWall c w r k l 15
        else
          if (← w.get k (l - 1)).wall != 15 && (← w.get k (l + 1)).wall != 15 &&
              (← w.get (k - 1) l).wall != 15 && (← w.get (k + 1) l).wall != 15 then
            (w, r) ← placeWall c w r k l 64
  if a.type < 0 then
    let t ← w.get k l
    if t.active && t.type == 53 then return (w, r)
    if a.type == -2 && t.active && (l < gv.waterLine || l > gv.lavaLine) then
      w ← w.modify k l fun t => ({ t with liquid := 255 }).setLiquidType num3
      if env.remixWorldGen then
        if l > gv.lavaLine && (Float.ofInt l.toInt < gv.rockLayer - 80 || l > maxY - 350)
            && !oceanDepths env gv w k l then
          w ← w.modify k l fun t => t.setLava true
      else if l > gv.lavaLine then
        w ← w.modify k l fun t => t.setLava true
    w ← w.modify k l fun t => t.setActive false
    return (w, r)
  if b.is368 then
    let (ins3, r5) ← within r k l b.px b.py b.strength 0.3 0.01
    r := r5
    if ins3 then (w, r) ← placeWall c w r k l 180
  if b.is367 then
    let (ins3, r5) ← within r k l b.px b.py b.strength 0.3 0.01
    r := r5
    if ins3 then (w, r) ← placeWall c w r k l 178
  paintCell c env gv sets a b.clearSlope w r k l

/-- Rows `l ∈ [l, y1)` of column `k` (`IL_039b‥IL_09fe`). -/
def runnerRows (b : RunnerBox) (num3 : Int32) (k : Int32) (y1 : Int32) :
    Nat → Int32 → World → UnifiedRandom → Except Fault (World × UnifiedRandom)
  | 0, _, w, r => .ok (w, r)
  | fuel + 1, l, w, r =>
    if l < y1 then do
      let (w, r) ← runnerCell b num3 k l w r
      runnerRows b num3 k y1 fuel (l + 1) w r
    else .ok (w, r)

/-- Columns `k ∈ [k, x1)` (`IL_037a‥IL_0a0d`), with the per-column beach reset of `num3`. -/
def runnerColumns (b : RunnerBox) (x1 y0 y1 : Int32) :
    Nat → Int32 → World → UnifiedRandom → Int32 → Except Fault (World × UnifiedRandom × Int32)
  | 0, _, w, r, num3 => .ok (w, r, num3)
  | fuel + 1, k, w, r, num3 =>
    if k < x1 then do
      let maxX := w.maxTilesX
      let num3 := if k < b.env.beachDistance + 50 || !(k < maxX - b.env.beachDistance - 50) then 0 else num3
      let (w, r) ← runnerRows b num3 k y1 ((y1 - y0).toInt.toNat + 1) y0 w r
      runnerColumns b x1 y0 y1 fuel (k + 1) w r num3
    else .ok (w, r, num3)

/-- `WorldGen.TileRunner(...)` on world `w` with `genRand = r`. -/
def tileRunner (c : Callees) (env : GenEnv) (gv : GenVars) (sets : TileSets) (a : RunnerArgs)
    (w : World) (r : UnifiedRandom) (fuel : Nat := 100000) : Except Fault (World × UnifiedRandom) := do
  let mut r := r
  let mut w := w
  let mut strength := a.strength
  let mut steps := a.steps
  if !gv.mudWall then
    if env.drunkWorldGen then
      let (n, r2) ← nextRange r (-80) 81
      strength := strength * (1 + Float.ofInt n.toInt * 0.01)
      let (n2, r3) ← nextRange r2 (-80) 81
      r := r3
      steps ← convI4 (Float.ofInt steps.toInt * (1 + Float.ofInt n2.toInt * 0.01))
    else if env.remixWorldGen then
      let (n, r2) ← nextRange r (-50) 51
      r := r2
      strength := strength * (1 + Float.ofInt n.toInt * 0.01)
    else if env.getGoodWorldGen && a.type != 57 then
      let (n, r2) ← nextRange r (-80) 81
      strength := strength * (1 + Float.ofInt n.toInt * 0.015)
      let (n2, r3) ← nextMax r2 3
      r := r3
      steps := steps + n2
  let mut num := strength
  let mut num2 : Float := Float.ofInt steps.toInt
  let mut px : Float := Float.ofInt a.i.toInt
  let mut py : Float := Float.ofInt a.j.toInt
  let (vx0, r2) ← nextRange r (-10) 11
  let (vy0, r3) ← nextRange r2 (-10) 11
  r := r3
  let mut vx : Float := Float.ofInt vx0.toInt * 0.1
  let mut vy : Float := Float.ofInt vy0.toInt * 0.1
  if a.speedX != 0 || a.speedY != 0 then
    vx := a.speedX
    vy := a.speedY
  let clearSlope ← if a.type ≥ 0 then (do return !(← flag "SaveSlopes" sets.saveSlopes a.type.toInt)) else pure false
  let is368 := a.type == 368
  let is367 := a.type == 367
  let mut num3 : Int32 := 0
  let mut decided := false
  if env.notTheBees && env.dontStarveWorldGen && !env.remixWorldGen then
    let (n, r4) ← nextMax r 3
    r := r4
    if n == 0 then
      num3 := 2
      decided := true
  if !decided then
    let (n, r4) ← nextMax r 4
    r := r4
    if n == 0 then
      if env.drunkWorldGen && env.tenthAnniversaryWorldGen && env.remixWorldGen && !env.notTheBees then num3 := 3
      else if env.getGoodWorldGen then num3 := 1
  let maxX := w.maxTilesX
  let maxY := w.maxTilesY
  for _ in [0:fuel] do
    if !(num > 0) || !(num2 > 0) then break
    if env.drunkWorldGen then
      let (n, r4) ← nextMax r 30
      r := r4
      if n == 0 then
        let (a1, r5) ← nextRange r (-100) 101
        px := px + Float.ofInt a1.toInt * 0.05
        let (a2, r6) ← nextRange r5 (-100) 101
        py := py + Float.ofInt a2.toInt * 0.05
        r := r6
    if num3 == 1 && env.tenthAnniversaryWorldGen && px > Float.ofInt maxX.toInt * 0.45
        && py < Float.ofInt maxX.toInt * 0.55 && py < gv.worldSurface then num3 := 0
    if py < 0 && num2 > 0 && a.type == 59 then num2 := 0
    num := strength * (num2 / Float.ofInt steps.toInt)
    num2 := num2 - 1
    let mut x0 ← convI4 (px - num * 0.5)
    let mut x1 ← convI4 (px + num * 0.5)
    let mut y0 ← convI4 (py - num * 0.5)
    let mut y1 ← convI4 (py + num * 0.5)
    if x0 < 1 then x0 := 1
    if x1 > maxX - 1 then x1 := maxX - 1
    if y0 < 1 then y0 := 1
    if y1 > maxY - 1 then y1 := maxY - 1
    let rb : RunnerBox := { c, env, gv, sets, a, clearSlope, is368, is367, px, py, strength }
    (w, r, num3) ← runnerColumns rb x1 y0 y1 (x1 - x0).toInt.toNat x0 w r num3
    px := px + vx
    py := py + vy
    let mut chain := true
    if env.drunkWorldGen then
      let (n, r4) ← nextMax r 3
      r := r4
      if n == 0 then chain := false
    if chain then
      for threshold in [50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900] do
        if !(num > (threshold : Float)) then break
        px := px + vx
        py := py + vy
        num2 := num2 - 1
        let (a1, r5) ← nextRange r (-10) 11
        vy := vy + Float.ofInt a1.toInt * 0.05
        let (a2, r6) ← nextRange r5 (-10) 11
        vx := vx + Float.ofInt a2.toInt * 0.05
        r := r6
    let (a1, r4) ← nextRange r (-10) 11
    r := r4
    vx := vx + Float.ofInt a1.toInt * 0.05
    if env.drunkWorldGen then
      let (a2, r5) ← nextRange r (-10) 11
      r := r5
      vx := vx + Float.ofInt a2.toInt * 0.25
    if vx > 1 then vx := 1
    if vx < -1 then vx := -1
    if !a.noYChange then
      let (a2, r5) ← nextRange r (-10) 11
      r := r5
      vy := vy + Float.ofInt a2.toInt * 0.05
      if vy > 1 then vy := 1
      if vy < -1 then vy := -1
    else if a.type != 59 && num < 3 then
      if vy > 1 then vy := 1
      if vy < -1 then vy := -1
    if a.type == 59 && !a.noYChange then
      if vy > 0.5 then vy := 0.5
      if vy < -0.5 then vy := -0.5
      if py < gv.rockLayer + 100 then vy := 1
      if py > Float.ofInt (maxY - 300).toInt then vy := -1
  return (w, r)

/-! ## `TerrainPass` -/

/-- `TerrainPass/SurfaceHistory` (`_heights : double[]`, `_index`). -/
structure SurfaceHistory where
  heights : Array Float
  index : Nat
deriving Inhabited

namespace SurfaceHistory
def create (n : Nat) : SurfaceHistory := ⟨Array.replicate n 0, 0⟩
def length (h : SurfaceHistory) : Nat := h.heights.size
def record (h : SurfaceHistory) (v : Float) : SurfaceHistory :=
  ⟨h.heights.set! h.index v, (h.index + 1) % h.heights.size⟩
def get (h : SurfaceHistory) (i : Nat) : Float := h.heights[(i + h.index) % h.heights.size]!
def set (h : SurfaceHistory) (i : Nat) (v : Float) : SurfaceHistory :=
  { h with heights := h.heights.set! ((i + h.index) % h.heights.size) v }
end SurfaceHistory

/-- `TerrainPass/TerrainFeatureType` (0 Plateau, 1 Hill, 2 Dale, 3 Mountain, 4 Valley). -/
abbrev FeatureType := Int32

/-- `while (Next(0, n) == 0) body` (or `!= 0` when `whileNonZero`), with `delta` added each time. -/
def walk (r : UnifiedRandom) (n : Int32) (whileNonZero : Bool) (acc delta : Float) (fuel : Nat := 1000000) :
    Except Fault (Float × UnifiedRandom) := do
  let mut r := r
  let mut acc := acc
  for _ in [0:fuel] do
    let (v, r2) ← nextRange r 0 n
    r := r2
    if (v == 0) == whileNonZero then break
    acc := acc + delta
  return (acc, r)

/-- `TerrainPass.GenerateWorldSurfaceOffset(featureType)`. -/
def generateWorldSurfaceOffset (env : GenEnv) (r : UnifiedRandom) (f : FeatureType) :
    Except Fault (Float × UnifiedRandom) := do
  let mut r := r
  let mut alt := false
  if env.drunkWorldGen || env.getGoodWorldGen || env.remixWorldGen then
    let (n, r2) ← nextMax r 2
    r := r2
    alt := n == 0
  -- (first loop modulus, first while-non-zero, first delta, second modulus, second delta)
  let plan : Option (Int32 × Bool × Float × Int32 × Float) := match alt, f.toInt with
    | true, 1 => some (3, false, -1, 10, 1)
    | true, 2 => some (3, false, 1, 10, -1)
    | true, 3 => some (3, true, -1, 6, 1)
    | true, 4 => some (3, true, 1, 5, -1)
    | false, 1 => some (4, false, -1, 10, 1)
    | false, 2 => some (4, false, 1, 10, -1)
    | false, 3 => some (2, false, -1, 6, 1)
    | false, 4 => some (2, false, 1, 5, -1)
    | _, _ => none
  if f == 0 then
    -- while (Next(0, alt ? 6 : 7) == 0) num += Next(-1, 2)
    let n : Int32 := if alt then 6 else 7
    let mut acc : Float := 0
    for _ in [0:1000000] do
      let (v, r2) ← nextRange r 0 n
      r := r2
      if v != 0 then break
      let (d, r3) ← nextRange r (-1) 2
      r := r3
      acc := acc + Float.ofInt d.toInt
    return (acc, r)
  match plan with
  | none => return (0, r)
  | some (n1, nz1, d1, n2, d2) =>
    let (acc, r1) ← walk r n1 nz1 0 d1
    walk r1 n2 false acc d2

/-- `TerrainPass.FillColumn(x, worldSurface, rockLayer)`. -/
def fillColumn (w : World) (x : Int32) (ws rl : Float) : Except Fault World := do
  let mut w := w
  let mut y : Int32 := 0
  while Float.ofInt y.toInt < ws do
    w ← w.modify x y fun t => { t.setActive false with frameX := -1, frameY := -1 }
    y := y + 1
  y ← convI4 ws
  while y < w.maxTilesY do
    let ty : UInt16 := if Float.ofInt y.toInt < rl then 0 else 1
    w ← w.modify x y fun t => { t.setActive true with type := ty, frameX := -1, frameY := -1 }
    y := y + 1
  return w

/-- `TerrainPass.RetargetColumn(x, worldSurface)`. -/
def retargetColumn (w : World) (x : Int32) (ws : Float) : Except Fault World := do
  let mut w := w
  let mut y : Int32 := 0
  while Float.ofInt y.toInt < ws do
    w ← w.modify x y fun t => { t.setActive false with frameX := -1, frameY := -1 }
    y := y + 1
  y ← convI4 ws
  while y < w.maxTilesY do
    let t ← w.get x y
    if !(t.type == 1 && t.active) then
      w ← w.set x y { t.setActive true with type := 0, frameX := -1, frameY := -1 }
    y := y + 1
  return w

/-- `TerrainPass.RetargetSurfaceHistory(history, targetX, targetHeight)`. -/
def retargetSurfaceHistory (w : World) (h : SurfaceHistory) (targetX : Int32) (target : Float) :
    Except Fault (World × SurfaceHistory) := do
  let len := h.length
  let mut h := h
  for i in [0:len / 2] do
    if h.get (len - 1) ≤ target then break
    for j in [0:len - i * 2] do
      let v := h.get (len - j - 1) - 1
      h := h.set (len - j - 1) v
      if v ≤ target then break
  let mut w := w
  for k in [0:len] do
    w ← retargetColumn w (targetX - Int32.ofNat k) (h.get (len - k - 1))
  return (w, h)

/-- Inputs of `TerrainPass.ApplyPass` beyond the world: `GenVars.leftBeachEnd/rightBeachStart`
(set by `WorldGen.Reset`) and `configuration.Get<int>("FlatBeachPadding")`. -/
structure TerrainIn where
  leftBeachEnd : Int32
  rightBeachStart : Int32
  flatBeachPadding : Int32
deriving Inhabited, Repr

/-- What `ApplyPass` writes besides tiles: `Main.worldSurface/rockLayer` and the `GenVars` fields. -/
structure TerrainOut where
  mainWorldSurface : Float
  mainRockLayer : Float
  rockLayer : Float
  rockLayerHigh : Float
  rockLayerLow : Float
  worldSurface : Float
  worldSurfaceHigh : Float
  worldSurfaceLow : Float
  waterLine : Int32
  lavaLine : Int32
  remixMushroomLayerLow : Int32
  remixMushroomLayerHigh : Int32
  remixSurfaceLayerLow : Int32
  remixSurfaceLayerHigh : Int32
deriving Inhabited, Repr

/-- `Utils.Clamp<double>(v, lo, hi)`. -/
def clampDouble (v lo hi : Float) : Float :=
  if GenerationProgress.compareDouble v hi > 0 then hi
  else if GenerationProgress.compareDouble v lo < 0 then lo else v

/-- Mono corlib `Math.Min/Max(double, double)` on non-NaN values (the Terrain values are finite). -/
def fmin (a b : Float) : Float := if a < b then a else if a.isNaN then a else b
def fmax (a b : Float) : Float := if a > b then a else if a.isNaN then a else b

/-- `TerrainPass.ApplyPass(progress, configuration)` with `genRand = r` (`progress.Set(x / maxX)`
is applied to `progress`; the localized message is not modelled). -/
def terrainApplyPass (env : GenEnv) (inp : TerrainIn) (w : World) (r : UnifiedRandom) (progress : GenerationProgress) :
    Except Fault (World × UnifiedRandom × GenerationProgress × TerrainOut) := do
  let mut r := r
  let mut w := w
  let mut progress := progress
  let pad := inp.flatBeachPadding
  let maxX := w.maxTilesX
  let maxY := w.maxTilesY
  let fY := Float.ofInt maxY.toInt
  let mut feature : FeatureType := 0
  let mut count : Int32 := 0
  let mut ws := fY * 0.3
  let (n, r2) ← nextRange r 90 110
  ws := ws * (Float.ofInt n.toInt * 0.005)
  let mut rl := ws + fY * 0.2
  let (n, r3) ← nextRange r2 90 110
  r := r3
  rl := rl * (Float.ofInt n.toInt * 0.01)
  if env.remixWorldGen then
    rl := fY * 0.5
    if maxX > 2500 then rl := fY * 0.6
    let (n, r4) ← nextRange r 95 106
    r := r4
    rl := rl * (Float.ofInt n.toInt * 0.01)
  let mut wsLow := ws
  let mut wsHigh := ws
  let mut rlLow := rl
  let mut rlHigh := rl
  if env.noSurface then
    ws := 25
    rl := fY * 0.4
    let (n, r4) ← nextRange r 90 110
    r := r4
    rl := rl * (Float.ofInt n.toInt * 0.01)
  let cap := fY * 0.23
  let mut hist := SurfaceHistory.create 500
  count := inp.leftBeachEnd + pad
  let mut x : Int32 := 0
  while x < maxX do
    progress := progress.setValue (Float.ofInt x.toInt / Float.ofInt maxX.toInt)
    wsLow := fmin ws wsLow
    wsHigh := fmax ws wsHigh
    rlLow := fmin rl rlLow
    rlHigh := fmax rl rlHigh
    if count ≤ 0 then
      let (f, r4) ← nextRange r 0 5
      let (c, r5) ← nextRange r4 5 40
      r := r5
      feature := f
      count := c
      if feature == 0 then
        let (m, r6) ← nextRange r 5 30
        r := r6
        count := count * (← convI4 (Float.ofInt m.toInt * 0.2))
    count := count - 1
    let fx := Float.ofInt x.toInt
    let fX := Float.ofInt maxX.toInt
    if fx > fX * 0.45 && fx < fX * 0.55 && (feature == 3 || feature == 4) then
      let (f, r4) ← nextMax r 3
      r := r4
      feature := f
    if fx > fX * 0.48 && fx < fX * 0.52 then feature := 0
    if !env.noSurface then
      let (off, r4) ← generateWorldSurfaceOffset env r feature
      r := r4
      ws := ws + off
    let mut lo := 0.17
    let mut hi := 0.26
    if env.surfaceIsInSpace then hi := 0.2199999988079071
    else if env.drunkWorldGen then
      lo := 0.15
      hi := 0.28
    if maxX ≤ 4200 then lo := lo + 0.02
    if !env.noSurface then
      if x < inp.leftBeachEnd + pad || x > inp.rightBeachStart - pad then
        ws := clampDouble ws (fY * lo) cap
      else if ws < fY * lo then
        ws := fY * lo
        count := 0
      else if ws > fY * hi then
        ws := fY * hi
        count := 0
    for _ in [0:1000000] do
      let (v, r4) ← nextRange r 0 3
      r := r4
      if v != 0 then break
      let (d, r5) ← nextRange r (-2) 3
      r := r5
      rl := rl + Float.ofInt d.toInt
    if env.noSurface then
      if rl < ws + fY * 0.35 then rl := rl + 1
      if rl > ws + fY * 0.45 then rl := rl - 1
    else if env.remixWorldGen then
      if maxX > 2500 then
        if rl > fY * 0.7 then rl := rl - 1
      else if rl > fY * 0.6 then rl := rl - 1
    else
      if rl < ws + fY * 0.06 then rl := rl + 1
      if rl > ws + fY * 0.35 then rl := rl - 1
    hist := hist.record ws
    if env.surfaceIsInSpace && !env.noSurface then
      w ← fillColumn w x (ws - fY * 0.08) rl
    else
      w ← fillColumn w x ws rl
      if x == inp.rightBeachStart - pad then
        if ws > cap then
          (w, hist) ← retargetSurfaceHistory w hist x cap
        feature := 0
        count := maxX - x
    x := x + 1
  let mut mainWs := Float.ofInt (← convI4 (wsHigh + 25)).toInt
  if env.noSurface then mainWs := 25
  let mainRl0 := rlHigh
  let step := Float.ofInt ((← convI4 ((mainRl0 - mainWs) / 6)) * 6).toInt
  let mainRl := Float.ofInt (← convI4 (mainWs + step)).toInt
  let (d1, r4) ← nextRange r (-100) 20
  let waterLine := (← convI4 (mainRl + fY)) / 2 + d1
  let (d2, r5) ← nextRange r4 50 80
  r := r5
  let mut lavaLine := waterLine + d2
  if env.remixWorldGen then lavaLine := (← convI4 (mainWs * 4 + rl)) / 5
  if rlLow < wsHigh + 20 then
    let mid := (rlLow + wsHigh) / 2
    let mut gap := Float.abs (rlLow - wsHigh)
    if gap < 20 then gap := 20
    rlLow := mid + gap / 2
    wsHigh := mid - gap / 2
  let mushLow := maxY - 350
  let out : TerrainOut := {
    mainWorldSurface := mainWs, mainRockLayer := mainRl, rockLayer := rl, rockLayerHigh := rlHigh,
    rockLayerLow := rlLow, worldSurface := ws, worldSurfaceHigh := wsHigh, worldSurfaceLow := wsLow,
    waterLine, lavaLine, remixMushroomLayerLow := mushLow, remixMushroomLayerHigh := maxY - 200,
    remixSurfaceLayerLow := ← convI4 rlLow, remixSurfaceLayerHigh := mushLow }
  return (w, r, progress, out)

end Lean4Terria.OriginalWorldGenTerrain
