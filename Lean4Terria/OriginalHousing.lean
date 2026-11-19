import Lean4Terria.OriginalWorld

/-!
# Town NPC home re-validation (Terraria desktop 1.4.5.8)

`Terraria.WorldGen.QuickFindHome(int npc)` and its whole call tree, transcribed by hand from the
desktop CIL. The town-NPC census calls it (every 7200 ticks) for each housed town NPC to check that
its home is still valid.

CIL (one method per file): `.lake/forest-desktop-integration/time-runtime/il/housing/`
* `QuickFindHome.il`, `StartRoomCheck.il`, `CheckRoom.il`, `RoomNeeds.il`, `ScoreRoom.il`,
  `Housing_GetTestedRoomBounds.il`, `Housing_CheckIfInRoom.il`, `IsOpenDoorAnchorFrame.il`,
  `ScoreRoom_CanBeHomeSpot.il`, `ScoreRoom_IsThisRoomOccupiedBySomeone.il`,
  `IsRoomConsideredOccupiedForNPCIndex.il`, `CheckSpecialTownNPCSpawningConditions.il`,
  `CountTileTypesInArea.il`, `GetTileTypeCountByCategory.il` (+ `_switch.il` with the switch
  targets), `Collision_SolidTiles.il` (MethodDef 1840 is the `(int,int,int,int)` overload used
  here), `Tile_nactive.il`, `Tile_active.il`, `BitSet2D.il`,
  `TownRoomManager_CanNPCsLiveWithEachOther.il`, `Main_get_NoFunctionalSurface.il`,
  `NoRoomCheckFeedback_get_StopOnFail.il`, `WorldGen_cctor_head.il`.

There is no random stream anywhere in this chain.

Notes that hold for the whole module:
* Every room check is started with a `null` feedback, which `StartRoomCheck` IL_0000..0008 and
  `ScoreRoom` IL_0000..0008 replace with `NoRoomCheckFeedback.WithText`. All its callbacks
  (`TooCloseToWorldEdge`, `RoomTooBig`, `BlockingWall`, `ReportScore`, `SetAsHighScore`, …) share
  one body that is a bare `ret` (RVA 0x9ece), and `get_StopOnFail` is `ldc.i4.1; ret`
  (RVA 0x386fd). So callbacks are omitted and `StopOnFail` is the constant `true`.
* `Tile.active()` is `(sTileHeader & 32) == 32`; `Tile.nactive()` is `(sTileHeader & 96) == 32`,
  i.e. active and not `inActive()` (actuated). With `OriginalWorld.Tile`: `active` is
  `id.isSome`, `inActive` is `actuated`. `Tile.type` is only ever read after an `active`/`nactive`
  test, so an inactive tile's stale type is never needed.
* `Main.tile` is a `Tile[,]`: an access outside the array throws, and a `null` element throws on
  member access (except in `Collision.SolidTiles`, which tests for null and returns false). Both
  are reported as `Except.error` boundaries naming the IL site; so is any out-of-range index into a
  static table (`IndexOutOfRangeException`).
* `WorldGen._roomCheckStack` (a `Stack<Point>`) is cleared at each `StartRoomCheck` IL_00ed before
  use, so its leftover content is not observable and it is a local here.
-/
namespace Lean4Terria.OriginalHousing

open OriginalWorld (Tile)

/-- Two's-complement int32 wrap, as CIL `add`/`sub`/`mul`/`neg` on int32 produce. -/
def i32 (v : Int) : Int :=
  let m := v % 4294967296
  if m ≥ 2147483648 then m - 4294967296 else m

/-! ## Inputs -/

/-- Static tables, all read natively from the running game. None are hard-coded here. -/
structure Tables where
  /-- `Terraria.Main::tileSolid` (bool[]), as it is BEFORE `QuickFindHome` IL_000c sets index 379. -/
  tileSolid : Array Bool
  /-- `Terraria.Main::tileSolidTop` (bool[]); read by `Collision.SolidTiles` IL_007a. -/
  tileSolidTop : Array Bool
  /-- `Terraria.Main::wallHouse` (bool[], indexed by wall id); `CheckRoom` IL_02e4, IL_034e. -/
  wallHouse : Array Bool
  /-- `Terraria.ID.TileID/Sets::HousingWalls` (bool[]); `CheckRoom` IL_0331, IL_039b. -/
  housingWalls : Array Bool
  /-- `Terraria.ID.TileID/Sets::IgnoredInHouseScore` (bool[]); `ScoreRoom` IL_01da. -/
  ignoredInHouseScore : Array Bool
  /-- `Terraria.ID.TileID/Sets::BasicChest` (bool[]); `ScoreRoom` IL_0210. -/
  basicChest : Array Bool
  /-- `Terraria.ID.TileID/Sets/RoomNeeds::CountsAsChairTypes` (int[], in array order). -/
  countsAsChairTypes : Array Int
  /-- `Terraria.ID.TileID/Sets/RoomNeeds::CountsAsTableTypes` (int[]). -/
  countsAsTableTypes : Array Int
  /-- `Terraria.ID.TileID/Sets/RoomNeeds::CountsAsTorchTypes` (int[]). -/
  countsAsTorchTypes : Array Int
  /-- `Terraria.ID.TileID/Sets/RoomNeeds::CountsAsDoorTypes` (int[]). -/
  countsAsDoorTypes : Array Int
  /-- `Terraria.ID.TileID::Count`: `StartRoomCheck` IL_005b loop bound, `ScoreRoom` IL_0050 array size. -/
  tileIdCount : Int

/-- The world and static scalars the chain reads. -/
structure World where
  /-- `Main.tile.GetLength(0)` / `GetLength(1)`: the allocated `Tile[,]` bounds (not the world size). -/
  tileArrayWidth : Int
  tileArrayHeight : Int
  /-- `Main.tile[x, y]` for in-array coordinates; `none` is a null element. -/
  tileAt : Int → Int → Option Tile
  /-- `Terraria.Main::maxTilesX`, `Terraria.Main::maxTilesY`. -/
  maxTilesX : Int
  maxTilesY : Int
  /-- `Terraria.WorldGen::lastMaxTilesY` (CheckRoom/StartRoomCheck bounds use it, not maxTilesY). -/
  lastMaxTilesY : Int
  /-- `Terraria.Main::worldSurface` (double). -/
  worldSurface : Float
  /-- `Terraria.NPC::unlockedTruffleSpawn`. -/
  unlockedTruffleSpawn : Bool
  /-- `Terraria.SceneMetrics::MushroomTileThreshold` (static int field, not a literal). -/
  mushroomTileThreshold : Int
  /-- `Terraria.Main::maxNPCs`. -/
  maxNPCs : Int
  /-- `Terraria.WorldGen::maxRoomTiles` (750 after `WorldGen..cctor` IL_0076; a mutable static). -/
  maxRoomTiles : Int
  /-- `Terraria.WorldGen::maxRoomSize` (100 after `WorldGen..cctor` IL_008a; a mutable static). -/
  maxRoomSize : Int
  /-- `ContentSamples.NpcsByNetId.TryGetValue(netId, out sample)` → `sample.housingCategory`;
  `none` when the key is absent (`TownRoomManager.CanNPCsLiveWithEachOther(int, NPC)` IL_0000..0010). -/
  npcSampleHousingCategory : Int → Option Int
  tables : Tables

/-- The fields of `Main.npc[i]` the chain reads or writes. The array index is the slot. -/
structure Npc where
  active : Bool
  type : Int
  townNPC : Bool
  homeless : Bool
  homelessDespawn : Bool
  homeTileX : Int
  homeTileY : Int
  /-- `NPC::housingCategory` (int), compared by `CanNPCsLiveWithEachOther(NPC, NPC)`. -/
  housingCategory : Int
  deriving BEq, Repr, Inhabited

/-! ## WorldGen static state -/

/-- `Terraria.Utilities.BitSet2D` (`WorldGen.roomTiles`). `bits` holds one Bool per bit of the
`Bits64[] bits` field (64 per element); `none` is the field while still null. -/
structure BitSet2D where
  size : Int
  offsetX : Int
  offsetY : Int
  bits : Option (Array Bool)
  deriving BEq, Repr

/-- `new BitSet2D()` (`.ctor` only calls `Object..ctor`): every field zero / null. -/
def BitSet2D.fresh : BitSet2D := { size := 0, offsetX := 0, offsetY := 0, bits := none }

/-- `BitSet2D.Reset(Point center, int maxDist)` IL_0000..006c. -/
def BitSet2D.reset (b : BitSet2D) (cx cy maxDist : Int) : Except String BitSet2D := do
  -- IL_0000..0006: size = maxDist * 2 + 1
  let size := i32 (i32 (maxDist * 2) + 1)
  -- IL_000b..0021: offset = new Point(center.X - maxDist, center.Y - maxDist)
  let offX := i32 (cx - maxDist)
  let offY := i32 (cy - maxDist)
  -- IL_0026..0038: words = (size * size + 63) >> 6 (arithmetic shift = floor division)
  let words := Int.fdiv (i32 (i32 (size * size) + 63)) 64
  -- IL_0039..0053: Array.Resize(ref bits, words) when bits is null or bits.Length < words
  let current := b.bits.getD #[]
  let resize := b.bits.isNone || Int.ofNat (current.size / 64) < words
  if resize && words < 0 then
    throw s!"BitSet2D.Reset IL_0053: Array.Resize with negative length {words}"
  let len := if resize then words.toNat * 64 else current.size
  -- IL_0058..006c: Array.Clear(bits, 0, bits.Length)
  return { size, offsetX := offX, offsetY := offY, bits := some (Array.replicate len false) }

/-- `BitSet2D.InBounds(Point)` IL_0000..0042. -/
def BitSet2D.inBounds (b : BitSet2D) (x y : Int) : Bool :=
  let dx := i32 (x - b.offsetX)
  let dy := i32 (y - b.offsetY)
  0 ≤ dx && dx < b.size && 0 ≤ dy && dy < b.size

/-- `BitSet2D.Coord(Point)`: `(p.Y - offset.Y) * size + (p.X - offset.X)`. -/
def BitSet2D.coord (b : BitSet2D) (x y : Int) : Int :=
  i32 (i32 (i32 (y - b.offsetY) * b.size) + i32 (x - b.offsetX))

private def BitSet2D.bitIndex (b : BitSet2D) (site : String) (x y : Int) :
    Except String (Array Bool × Nat) := do
  let c := b.coord x y
  let some bits := b.bits | throw s!"{site}: BitSet2D.bits is null"
  -- `bits[c >> 6]` then bit `c & 63`: one flat Bool index `c` when 0 ≤ c.
  if c < 0 || c ≥ bits.size then throw s!"{site}: BitSet2D bit {c} outside bits"
  return (bits, c.toNat)

/-- `BitSet2D.get_Item(Point)`. -/
def BitSet2D.get (b : BitSet2D) (site : String) (x y : Int) : Except String Bool := do
  let (bits, i) ← b.bitIndex site x y
  return bits[i]!

/-- `BitSet2D.Add(Point)` IL_0000..003c: false if the bit was already set, else set it. -/
def BitSet2D.add (b : BitSet2D) (site : String) (x y : Int) : Except String (Bool × BitSet2D) := do
  let (bits, i) ← b.bitIndex site x y
  if bits[i]! then return (false, b)
  return (true, { b with bits := some (bits.set! i true) })

/-- The `WorldGen` statics this chain reads and writes, threaded explicitly. -/
structure RoomState where
  canSpawn : Bool
  roomX1 : Int
  roomX2 : Int
  roomY1 : Int
  roomY2 : Int
  numRoomTiles : Int
  roomTiles : BitSet2D
  /-- `WorldGen.houseTile` (bool[TileID.Count]). -/
  houseTile : Array Bool
  /-- `WorldGen.roomCheckFailureReason` (enum `TownNPCRoomCheckFailureReason`, as its int value). -/
  roomCheckFailureReason : Int
  roomHasStinkbug : Bool
  roomHasEchoStinkbug : Bool
  roomChair : Bool
  roomDoor : Bool
  roomTable : Bool
  roomTorch : Bool
  roomOccupied : Bool
  roomHasStandingSpace : Bool
  roomEvil : Bool
  sharedRoomX : Int
  hiScore : Int
  bestX : Int
  bestY : Int
  deriving BEq, Repr

/-- The statics right after `WorldGen..cctor` (CLR zero-initialisation, then IL_0091..00a5
`roomTiles = new BitSet2D()`, `houseTile = new bool[TileID.Count]`, IL_00d9 failure reason 0).
A live game should pass its current statics instead. -/
def RoomState.afterCctor (tileIdCount : Nat) : RoomState :=
  { canSpawn := false, roomX1 := 0, roomX2 := 0, roomY1 := 0, roomY2 := 0, numRoomTiles := 0,
    roomTiles := BitSet2D.fresh, houseTile := Array.replicate tileIdCount false,
    roomCheckFailureReason := 0, roomHasStinkbug := false, roomHasEchoStinkbug := false,
    roomChair := false, roomDoor := false, roomTable := false, roomTorch := false,
    roomOccupied := false, roomHasStandingSpace := false, roomEvil := false, sharedRoomX := 0,
    hiScore := 0, bestX := 0, bestY := 0 }

/-! ## Helpers -/

abbrev M := StateT RoomState (Except String)

/-- `NoRoomCheckFeedback.WithText.StopOnFail` (`get_StopOnFail` RVA 0x386fd: `ldc.i4.1; ret`). -/
def stopOnFail : Bool := true

def readBool (site name : String) (a : Array Bool) (i : Int) : Except String Bool :=
  if 0 ≤ i && i < a.size then .ok a[i.toNat]!
  else .error s!"{site}: IndexOutOfRangeException {name}[{i}] (length {a.size})"

def readInt (site name : String) (a : Array Int) (i : Int) : Except String Int :=
  if 0 ≤ i && i < a.size then .ok a[i.toNat]!
  else .error s!"{site}: IndexOutOfRangeException {name}[{i}] (length {a.size})"

/-- `Main.tile[x, y]` followed by a member access. -/
def getTile (w : World) (site : String) (x y : Int) : Except String Tile := do
  if x < 0 || x ≥ w.tileArrayWidth || y < 0 || y ≥ w.tileArrayHeight then
    throw s!"{site}: IndexOutOfRangeException Main.tile[{x}, {y}]"
  match w.tileAt x y with
  | some t => return t
  | none => throw s!"{site}: NullReferenceException, Main.tile[{x}, {y}] is null"

def npcAt (npcs : Array Npc) (site : String) (i : Int) : Except String Npc :=
  if 0 ≤ i && i < npcs.size then .ok npcs[i.toNat]!
  else .error s!"{site}: Main.npc[{i}] outside the supplied NPC array (size {npcs.size})"

/-- `Tile.active()`. -/
def active (t : Tile) : Bool := t.id.isSome

/-- `Tile.nactive()`: `(sTileHeader & 96) == 32`. -/
def nactive (t : Tile) : Bool := t.id.isSome && !t.actuated

/-- `Tile.type` of an active tile. -/
def activeType? (t : Tile) : Option Int := t.id.map (·.toNat)

/-- `Tile.type` of an `nactive` tile. -/
def nactiveType? (t : Tile) : Option Int := if t.actuated then none else activeType? t

def frameXY (site : String) (t : Tile) : Except String (Int × Int) :=
  match t.frame with
  | some f => .ok f
  | none => .error s!"{site}: tile {repr t.id} has no frame in OriginalWorld.Tile"

/-- `for (v = lo; v < hi; v++)` -/
def upTo (lo hi : Int) : List Int := (List.range (hi - lo).toNat).map (fun i => lo + Int.ofNat i)

/-- `for (v = lo; v <= hi; v += 2)` -/
def stepTwoThrough (lo hi : Int) : List Int :=
  if hi < lo then [] else
    (List.range ((hi - lo) / 2 + 1).toNat).map (fun i => lo + 2 * Int.ofNat i)

/-- `TownRoomManager.CanNPCsLiveWithEachOther(NPC npc1, NPC npc2)` (MethodDef 9815):
`npc1.housingCategory != npc2.housingCategory`. -/
def canLiveWithNpc (a b : Npc) : Bool := a.housingCategory != b.housingCategory

/-- `TownRoomManager.CanNPCsLiveWithEachOther(int npcType, NPC npc)` (MethodDef 9814). -/
def canLiveWithType (w : World) (npcType : Int) (n : Npc) : Bool :=
  -- IL_0000..0010: no sample for this net id → true
  match w.npcSampleHousingCategory npcType with
  | none => true
  -- IL_0011..0019: CanNPCsLiveWithEachOther(sample, npc)
  | some category => category != n.housingCategory

/-! ## Spread: `StartRoomCheck` / `CheckRoom` -/

/-- `WorldGen.CheckRoom(int x, int y, int stackCount, IRoomCheckFeedback feedback)`
(MethodDef 4387). `stackCount` only reaches feedback callbacks. -/
def checkRoom (w : World) (x y : Int) (_stackCount : Int) : M Bool := do
  let tb := w.tables
  -- IL_0000..0006: stopOnFail = feedback.StopOnFail
  -- IL_0007..003d: too close to the world edge
  if x < 10 || y < 10 || x ≥ w.maxTilesX - 10 || y ≥ w.lastMaxTilesY - 10 then
    modify fun s => { s with roomCheckFailureReason := 1, canSpawn := false }
    return false
  -- IL_003e..0067: outside roomTiles → RoomTooBig
  if !(← get).roomTiles.inBounds x y then
    modify fun s => { s with roomCheckFailureReason := 2, canSpawn := false }
    return false
  -- IL_0068..007c: roomTiles.Add; already visited → false
  let (added, rt) ← (← get).roomTiles.add "CheckRoom IL_0074" x y
  if !added then return false
  -- IL_007d..0088
  modify fun s => { s with roomTiles := rt, numRoomTiles := i32 (s.numRoomTiles + 1) }
  -- IL_0089..00ae
  if stopOnFail && (← get).numRoomTiles ≥ w.maxRoomTiles then
    modify fun s => { s with roomCheckFailureReason := 2, canSpawn := false }
    return false
  -- IL_00af..00e6: grow the bounds
  modify fun s => { s with
    roomX1 := if x < s.roomX1 then x else s.roomX1
    roomX2 := if x > s.roomX2 then x else s.roomX2
    roomY1 := if y < s.roomY1 then y else s.roomY1
    roomY2 := if y > s.roomY2 then y else s.roomY2 }
  -- IL_00e7..0124
  let s ← get
  if stopOnFail && (i32 (s.roomX2 - s.roomX1) ≥ w.maxRoomSize ||
      i32 (s.roomY2 - s.roomY1) ≥ w.maxRoomSize) then
    modify fun s => { s with roomCheckFailureReason := 2, canSpawn := false }
    return false
  -- IL_0125: AnyBlockScannedHere (no-op)
  let here ← getTile w "CheckRoom IL_0135" x y
  -- IL_012e..013f: nactive → IL_0144, else IL_02d8
  if let some ty := nactiveType? here then
    -- IL_0144..015b: houseTile[type] = true
    let ht := (← get).houseTile
    if !(0 ≤ ty && ty < ht.size) then
      throw s!"CheckRoom IL_015b: IndexOutOfRangeException WorldGen.houseTile[{ty}]"
    modify fun s => { s with houseTile := s.houseTile.set! ty.toNat true }
    -- IL_015c..017f: solid → BlockingWall, return false (canSpawn untouched)
    if (← readBool "CheckRoom IL_0172" "Main.tileSolid" tb.tileSolid ty) then return false
    -- IL_0180..01f1: door 11 with frameX 0, 54, 72 or 126 → BlockingOpenGate
    if ty == 11 then
      let (fx, _) ← frameXY "CheckRoom IL_01a1" here
      if fx == 0 || fx == 54 || fx == 72 || fx == 126 then return false
    -- IL_01f2..0214: 389 → BlockingOpenGate
    if ty == 389 then return false
    -- IL_0215..0289: 386 with (frameX < 36 && frameY == 18) || (frameX >= 36 && frameY == 0)
    if ty == 386 then
      let (fx, fy) ← frameXY "CheckRoom IL_0239" here
      if (fx < 36 && fy == 18) || (fx ≥ 36 && fy == 0) then return false
    -- IL_028a..02b0: 630 → roomHasStinkbug
    if ty == 630 then modify fun s => { s with roomHasStinkbug := true }
    -- IL_02b1..02d3: 631 → roomHasEchoStinkbug
    if ty == 631 then modify fun s => { s with roomHasEchoStinkbug := true }
  -- IL_02d8..03be: for (k = -2; k < 3; k++) walls / solid / HousingWalls along the row and column
  let mut flagX := false
  let mut flagY := false
  for k in upTo (-2) 3 do
    -- IL_02e4..0300: wallHouse[tile(x + k, y).wall]
    let a ← getTile w "CheckRoom IL_02f2" (x + k) y
    if (← readBool "CheckRoom IL_02fc" "Main.wallHouse" tb.wallHouse a.wall.toNat) then flagX := true
    -- IL_0301..034d
    if let some aty := nactiveType? a then
      if (← readBool "CheckRoom IL_032e" "Main.tileSolid" tb.tileSolid aty) then flagX := true
      else if (← readBool "CheckRoom IL_0349" "TileID.Sets.HousingWalls" tb.housingWalls aty) then
        flagX := true
    -- IL_034e..036a: wallHouse[tile(x, y + k).wall]
    let b ← getTile w "CheckRoom IL_035c" x (y + k)
    if (← readBool "CheckRoom IL_0366" "Main.wallHouse" tb.wallHouse b.wall.toNat) then flagY := true
    -- IL_036b..03b7
    if let some bty := nactiveType? b then
      if (← readBool "CheckRoom IL_0398" "Main.tileSolid" tb.tileSolid bty) then flagY := true
      else if (← readBool "CheckRoom IL_03b3" "TileID.Sets.HousingWalls" tb.housingWalls bty) then
        flagY := true
  -- IL_03c3..0404
  if !(flagX && flagY) then
    if here.wall > 0 then
      -- IL_03dd..03ec: UnsafeWall
      modify fun s => { s with roomCheckFailureReason := 6 }
    else
      -- IL_03ee..03f8: MissingAWall
      modify fun s => { s with roomCheckFailureReason := 4 }
    modify fun s => { s with canSpawn := false }
    return false
  -- IL_0405
  return true

/-- `WorldGen.StartRoomCheck(int x, int y, IRoomCheckFeedback feedback)` (MethodDef 4386), with a
`null` feedback. -/
def startRoomCheck (w : World) (x y : Int) : M Bool := do
  let tb := w.tables
  -- IL_0000..0008: feedback = NoRoomCheckFeedback.WithText
  -- IL_000a..0028
  modify fun s => { s with
    roomHasStinkbug := false, roomHasEchoStinkbug := false,
    roomX1 := x, roomX2 := x, roomY1 := y, roomY2 := y }
  -- IL_002e..003f: roomTiles.Reset(new Point(x, y), maxRoomSize)
  let rt ← (← get).roomTiles.reset x y w.maxRoomSize
  -- IL_0044..0045
  modify fun s => { s with roomTiles := rt, numRoomTiles := 0 }
  -- IL_004a..0060: for (i = 0; i < TileID.Count; i++) houseTile[i] = false
  let ht := (← get).houseTile
  if tb.tileIdCount > ht.size then
    throw s!"StartRoomCheck IL_0055: IndexOutOfRangeException WorldGen.houseTile[{ht.size}]"
  let cleared := (List.range tb.tileIdCount.toNat).foldl (fun a i => a.set! i false) ht
  -- IL_0062..0063
  modify fun s => { s with houseTile := cleared, canSpawn := true }
  -- IL_0068..009e: too close to the world edge
  if x < 10 || y < 10 || x ≥ w.maxTilesX - 10 || y ≥ w.lastMaxTilesY - 10 then
    modify fun s => { s with roomCheckFailureReason := 1, canSpawn := false }
    return false
  -- IL_009f..00e0: started in a solid tile
  let start ← getTile w "StartRoomCheck IL_00a6" x y
  if let some ty := nactiveType? start then
    if (← readBool "StartRoomCheck IL_00c8" "Main.tileSolid" tb.tileSolid ty) then
      modify fun s => { s with roomCheckFailureReason := 5, canSpawn := false }
      return false
  -- IL_00e1..00e2
  modify fun s => { s with roomCheckFailureReason := 0 }
  -- IL_00e7..00fb: stack.Clear(); stack.Push(new Point(x, y))
  let mut stack : Array (Int × Int) := #[(x, y)]
  -- Termination bound for IL_0102..0178: every iteration pops one point; points are pushed only
  -- (8 at a time) after a CheckRoom that returned true, which needs a successful roomTiles.Add of
  -- a new in-bounds bit, of which there are at most size * size. So at most 1 + 8 * size * size
  -- iterations happen; running out of fuel would be a transcription error.
  let size := (← get).roomTiles.size
  let fuel := 2 + 8 * (if size > 0 then (size * size).toNat else 0)
  let mut finished := false
  for _ in [0:fuel] do
    -- IL_0171..0178: while (stack.Count > 0)
    if stack.size == 0 then
      finished := true
      break
    -- IL_0102..010f: if (!canSpawn && feedback.StopOnFail) break
    if !(← get).canSpawn && stopOnFail then
      finished := true
      break
    -- IL_0111..0117: Point p = stack.Pop()
    let p := stack.back!
    stack := stack.pop
    -- IL_0118..0130: CheckRoom(p.X, p.Y, stack.Count, feedback)
    if (← checkRoom w p.1 p.2 stack.size) then
      -- IL_0132..016f: for (dx = -1..1) for (dy = -1..1) if (dx != 0 || dy != 0) push
      for dx in [-1, 0, 1] do
        for dy in [-1, 0, 1] do
          if dx != 0 || dy != 0 then
            stack := stack.push (p.1 + dx, p.2 + dy)
  if !finished then
    throw "StartRoomCheck IL_0102: spread exceeded its IL-derived iteration bound (transcription error)"
  -- IL_017a..0182
  if !(← get).canSpawn then return false
  -- IL_0183..0199: fewer than 60 tiles → RoomTooSmall
  if (← get).numRoomTiles < 60 then
    modify fun s => { s with roomCheckFailureReason := 3, canSpawn := false }
    return false
  -- IL_019a..01a4
  return (← get).canSpawn

/-! ## `RoomNeeds` -/

/-- `WorldGen.RoomNeeds()` (MethodDef 4376). -/
def roomNeeds (w : World) : M Bool := do
  let tb := w.tables
  -- IL_0000..0013
  modify fun s => { s with
    roomChair := false, roomDoor := false, roomTable := false, roomTorch := false }
  let ht := (← get).houseTile
  -- IL_0018..0040: chair
  for ty in tb.countsAsChairTypes do
    if (← readBool "RoomNeeds IL_002c" "WorldGen.houseTile" ht ty) then
      modify fun s => { s with roomChair := true }
      break
  -- IL_0041..0069: table
  for ty in tb.countsAsTableTypes do
    if (← readBool "RoomNeeds IL_0055" "WorldGen.houseTile" ht ty) then
      modify fun s => { s with roomTable := true }
      break
  -- IL_006a..0094: torch
  for ty in tb.countsAsTorchTypes do
    if (← readBool "RoomNeeds IL_0080" "WorldGen.houseTile" ht ty) then
      modify fun s => { s with roomTorch := true }
      break
  -- IL_0095..00bf: door
  for ty in tb.countsAsDoorTypes do
    if (← readBool "RoomNeeds IL_00ab" "WorldGen.houseTile" ht ty) then
      modify fun s => { s with roomDoor := true }
      break
  -- IL_00c0..00ef
  let s ← get
  let ok := s.roomChair && s.roomTable && s.roomDoor && s.roomTorch
  set { s with canSpawn := ok }
  return ok

/-! ## Scoring -/

/-- `WorldGen.Housing_GetTestedRoomBounds(out startX, out endX, out startY, out endY)`
(MethodDef 4383); returns `(startX, endX, startY, endY)`. -/
def getTestedRoomBounds (w : World) (s : RoomState) : Int × Int × Int × Int := Id.run do
  -- IL_0000..0006: num = 40, num2 = 5, num3 = 3
  -- IL_0007..003a
  let mut startX := i32 (s.roomX1 - 5 - 1 - 40)
  let mut endX := i32 (s.roomX2 + 5 + 1 + 40)
  let mut startY := i32 (s.roomY1 - 3 - 1 - 40)
  let mut endY := i32 (s.roomY2 + 3 + 1 + 40)
  -- IL_003b..0042
  if startX < 5 then startX := 5
  -- IL_0043..0056
  if endX ≥ w.maxTilesX - 5 then endX := w.maxTilesX - 6
  -- IL_0057..005e
  if startY < 5 then startY := 5
  -- IL_005f..0072 (Main.maxTilesY here)
  if endY ≥ w.maxTilesY - 5 then endY := w.maxTilesY - 6
  return (startX, endX, startY, endY)

/-- `WorldGen.Housing_CheckIfInRoom(int x, int y)` (MethodDef 4385). -/
def checkIfInRoom (s : RoomState) (x y : Int) : Except String Bool :=
  -- IL_0009..0014: InBounds, else IL_0022 false
  if s.roomTiles.inBounds x y then s.roomTiles.get "Housing_CheckIfInRoom IL_001c" x y
  else .ok false

/-- `WorldGen.CountTileTypesInArea(int[] tileTypeCounts, startX, endX, startY, endY)`
(MethodDef 4380): bounds inclusive, x outer. -/
def countTileTypesInArea (w : World) (counts : Array Int) (startX endX startY endY : Int) :
    Except String (Array Int) := do
  let mut counts := counts
  for x in upTo startX (endX + 1) do
    for y in upTo startY (endY + 1) do
      -- IL_0008..0019
      let t ← getTile w "CountTileTypesInArea IL_000f" x y
      if let some ty := activeType? t then
        -- IL_001b..0036: tileTypeCounts[type]++
        if !(0 ≤ ty && ty < counts.size) then
          throw s!"CountTileTypesInArea IL_002d: IndexOutOfRangeException counts[{ty}]"
        counts := counts.set! ty.toNat (i32 (counts[ty.toNat]! + 1))
  return counts

/-- The sum of `GetTileTypeCountByCategory` for groups 1 (IL_0021..0059), 2 (IL_005a..0098),
3 (IL_0099..00c9). -/
private def categoryCount (counts : Array Int) (group : Int) : Except String Int := do
  let r (i : Int) := readInt "GetTileTypeCountByCategory" "tileTypeCounts" counts i
  match group with
  | 1 =>
    -- 23 + 24 + 25 + 32 + 112 + 163 + 400 + 398 + (-5) * 27
    let mut v := 0
    for i in [23, 24, 25, 32, 112, 163, 400, 398] do v := i32 (v + (← r i))
    return i32 (v + i32 (-5 * (← r 27)))
  | 2 =>
    -- 199 + 203 + 200 + 401 + 399 + 234 + 352 - 5 * 27
    let mut v := 0
    for i in [199, 203, 200, 401, 399, 234, 352] do v := i32 (v + (← r i))
    return i32 (v - i32 (5 * (← r 27)))
  | 3 =>
    -- 109 + 110 + 113 + 117 + 116 + 164 + 403 + 402
    let mut v := 0
    for i in [109, 110, 113, 117, 116, 164, 403, 402] do v := i32 (v + (← r i))
    return v
  | _ => return 0

/-- `WorldGen.GetTileTypeCountByCategory(int[] tileTypeCounts, TileScanGroup group)`
(MethodDef 4381). Switch (IL_0001): 0 → IL_001f, 1 → IL_0021, 2 → IL_005a, 3 → IL_0099,
4 → IL_00ca, default → IL_00ef. -/
def getTileTypeCountByCategory (counts : Array Int) (group : Int) : Except String Int := do
  match group with
  | 0 => return 0
  | 1 | 2 | 3 => categoryCount counts group
  | 4 =>
    -- IL_00ca..00ee: hallow - (corruption + crimson + 5 * counts[27])
    let hallow ← categoryCount counts 3
    let corruption ← categoryCount counts 1
    let crimson ← categoryCount counts 2
    let sunflower := i32 (5 * (← readInt "GetTileTypeCountByCategory IL_00e4" "tileTypeCounts" counts 27))
    return i32 (hallow - i32 (i32 (corruption + crimson) + sunflower))
  | _ => return 0

/-- `Collision.SolidTiles(int startX, int endX, int startY, int endY)` (MethodDef 1840). -/
def solidTiles (w : World) (startX endX startY endY : Int) : Except String Bool := do
  let tb := w.tables
  -- IL_0000..0022
  if startX < 0 then return true
  if endX ≥ w.maxTilesX then return true
  if startY < 0 then return true
  if endY ≥ w.maxTilesY - 40 then return true
  -- IL_0023..00ad: x outer, both bounds inclusive
  for x in upTo startX (endX + 1) do
    for y in upTo startY (endY + 1) do
      if x < 0 || x ≥ w.tileArrayWidth || y < 0 || y ≥ w.tileArrayHeight then
        throw s!"Collision.SolidTiles IL_0032: IndexOutOfRangeException Main.tile[{x}, {y}]"
      match w.tileAt x y with
      -- IL_0037..003a: null tile → return false
      | none => return false
      | some t =>
        -- IL_003b..0094: active && !inActive && tileSolid && !tileSolidTop → true
        if let some ty := activeType? t then
          if !t.actuated then
            if (← readBool "Collision.SolidTiles IL_0077" "Main.tileSolid" tb.tileSolid ty) then
              if !(← readBool "Collision.SolidTiles IL_0090" "Main.tileSolidTop" tb.tileSolidTop ty) then
                return true
  return false

/-- `WorldGen.ScoreRoom_CanBeHomeSpot(int x, int y)` (MethodDef 4384). -/
def scoreRoomCanBeHomeSpot (w : World) (x y : Int) : Except String Bool := do
  let t ← getTile w "ScoreRoom_CanBeHomeSpot IL_0007" x y
  -- IL_000d..0027: active && type == 379 → false
  match activeType? t with
  | some 379 => return false
  | _ => return true

/-- `WorldGen.IsOpenDoorAnchorFrame(int x, int y)` (MethodDef 4939). -/
def isOpenDoorAnchorFrame (w : World) (x y : Int) : Except String Bool := do
  let t ← getTile w "IsOpenDoorAnchorFrame IL_0007" x y
  -- IL_000d..0020: !active || type != 11 → false
  if activeType? t != some 11 then return false
  -- IL_0021..002a: num = frameX % 72 (C# remainder, sign of the dividend)
  let (fx, _) ← frameXY "IsOpenDoorAnchorFrame IL_0022" t
  let r := fx.tmod 72
  -- IL_002b..003a: num < 18 → true, else num >= 54
  if r < 18 then return true
  return r ≥ 54

/-- `WorldGen.ScoreRoom_IsThisRoomOccupiedBySomeone(int ignoreNPC, int npcTypeAsking)`
(MethodDef 4379). Sets `sharedRoomX` for every compatible occupant it passes. -/
def isThisRoomOccupiedBySomeone (w : World) (npcs : Array Npc) (ignoreNPC npcTypeAsking : Int) :
    M Bool := do
  for i in upTo 0 w.maxNPCs do
    let n ← npcAt npcs "ScoreRoom_IsThisRoomOccupiedBySomeone IL_000a" i
    -- IL_000c..0026
    if !n.active || !n.townNPC || ignoreNPC == i || n.homeless then continue
    -- IL_0028..004e: both home tiles inside the tested room
    if !(← checkIfInRoom (← get) n.homeTileX n.homeTileY) then continue
    if !(← checkIfInRoom (← get) n.homeTileX (i32 (n.homeTileY - 1))) then continue
    -- IL_0050..005f: incompatible occupant → true
    if !canLiveWithType w npcTypeAsking n then return true
    -- IL_0060..0066
    modify fun s => { s with sharedRoomX := n.homeTileX }
  -- IL_0077
  return false

/-- The per-spot body of `ScoreRoom` (IL_00ce..03d9) for the floor-standing spot `(x, y)`. -/
def scoreSpot (w : World) (x y baseScore : Int) : M Unit := do
  let tb := w.tables
  -- IL_00ce..00e1: nactive
  let here ← getTile w "ScoreRoom IL_00d7" x y
  let some ty := nactiveType? here | return
  -- IL_00e6..00ef
  if !(← scoreRoomCanBeHomeSpot w x y) then return
  -- IL_00f4..010d
  if !(← readBool "ScoreRoom IL_010c" "Main.tileSolid" tb.tileSolid ty) then return
  -- IL_0112..0127: Collision.SolidTiles(x - 1, x + 1, y - 3, y - 1) → skip
  if (← solidTiles w (x - 1) (x + 1) (y - 3) (y - 1)) then return
  -- IL_012c..0161: left neighbour nactive and solid
  let left ← getTile w "ScoreRoom IL_0137" (x - 1) y
  let some lty := nactiveType? left | return
  if !(← readBool "ScoreRoom IL_0160" "Main.tileSolid" tb.tileSolid lty) then return
  -- IL_0166..019b: right neighbour nactive and solid
  let right ← getTile w "ScoreRoom IL_0171" (x + 1) y
  let some rty := nactiveType? right | return
  if !(← readBool "ScoreRoom IL_019a" "Main.tileSolid" tb.tileSolid rty) then return
  -- IL_01a0..01ae
  let mut score := baseScore       -- loc0
  let mut sameColumn : Int := 0    -- loc9
  let mut chests : Int := 0        -- loc10
  -- IL_01a8..029b: for (x11 = x - 2; x11 < x + 3; x11++) for (y12 = y - 4; y12 < y; y12++)
  for x11 in upTo (x - 2) (x + 3) do
    for y12 in upTo (y - 4) y do
      let c ← getTile w "ScoreRoom IL_01c7" x11 y12
      -- IL_01ce..01d5
      let some cty := nactiveType? c | continue
      -- IL_01da..01e7
      if (← readBool "ScoreRoom IL_01e6" "TileID.Sets.IgnoredInHouseScore" tb.ignoredInHouseScore cty) then
        continue
      -- IL_01ec..0200: a door 11 counts only on its open anchor frame
      if cty == 11 then
        if !(← isOpenDoorAnchorFrame w x11 y12) then continue
      -- IL_0202..020e: same column as the spot
      if x11 == x then
        sameColumn := i32 (sameColumn + 1)
        continue
      -- IL_0210..0225: BasicChest
      if (← readBool "ScoreRoom IL_021c" "TileID.Sets.BasicChest" tb.basicChest cty) then
        chests := i32 (chests + 1)
        continue
      -- IL_0227..0245: closed door 10 or 388 → -20
      if cty == 10 || cty == 388 then
        score := i32 (score - 20)
        continue
      -- IL_0247..0265: open-door anchor frame or 389 → -20
      let openAnchor ← isOpenDoorAnchorFrame w x11 y12
      if openAnchor || cty == 389 then
        score := i32 (score - 20)
        continue
      -- IL_0267..027f: solid → -5, else +5
      if (← readBool "ScoreRoom IL_0273" "Main.tileSolid" tb.tileSolid cty) then
        score := i32 (score - 5)
      else
        score := i32 (score + 5)
  -- IL_02a0..02a9
  if score > 0 then modify fun s => { s with roomHasStandingSpace := true }
  -- IL_02aa..02c7: shared room within 3 columns → 1
  let shared := (← get).sharedRoomX
  if shared ≥ 0 && score ≥ 1 then
    let d := i32 (shared - x)
    if d == -2147483648 then
      throw "ScoreRoom IL_02be: Math.Abs(int.MinValue) OverflowException"
    if d.natAbs < 3 then score := 1
  -- IL_02c8..02de: chests
  if score > 0 && chests > 0 then
    score := i32 (score - i32 (30 * chests))
    if score < 1 then score := 1
  -- IL_02df..02f5: same-column furniture
  if score > 0 && sameColumn > 0 then
    score := i32 (score - i32 (15 * sameColumn))
    if score < 1 then score := 1
  -- IL_02f6..02fc: ReportScore (no-op)
  -- IL_0301..0307
  if score ≤ (← get).hiScore then return
  -- IL_030c..0315
  let mut inRoom ← checkIfInRoom (← get) x y
  -- IL_0317..0381: three tiles above the floor
  let mut clear : Array Bool := #[false, false, false]
  for k in upTo 1 4 do
    let a ← getTile w "ScoreRoom IL_0330" x (y - k)
    -- IL_0324..0361: !active || !tileSolid → true
    match activeType? a with
    | none => clear := clear.set! (k - 1).toNat true
    | some aty =>
      if !(← readBool "ScoreRoom IL_0357" "Main.tileSolid" tb.tileSolid aty) then
        clear := clear.set! (k - 1).toNat true
    -- IL_0362..0377: outside the room → false
    if !(← checkIfInRoom (← get) x (y - k)) then clear := clear.set! (k - 1).toNat false
  -- IL_0383..03a4: any false → flag = false
  for b in clear do
    if !b then
      inRoom := false
      break
  -- IL_03a6..03b8
  if !inRoom then return
  if !(← checkIfInRoom (← get) x (y - 1)) then return
  -- IL_03ba..03d4: SetAsHighScore (no-op), then the new best spot
  modify fun s => { s with hiScore := score, bestX := x, bestY := y }

/-- `WorldGen.ScoreRoom(int ignoreNPC, int npcTypeAskingToScoreRoom, IRoomCheckFeedback feedback)`
(MethodDef 4382), with a `null` feedback. -/
def scoreRoom (w : World) (npcs : Array Npc) (ignoreNPC npcTypeAsking : Int) : M Unit := do
  -- IL_0000..0008: feedback = NoRoomCheckFeedback.WithText
  -- IL_000a..0021
  modify fun s => { s with
    roomOccupied := false, roomHasStandingSpace := false, roomEvil := false, sharedRoomX := -1 }
  -- IL_0022..0037
  if (← isThisRoomOccupiedBySomeone w npcs ignoreNPC npcTypeAsking) then
    modify fun s => { s with roomOccupied := true, hiScore := -1 }
    return
  -- IL_0038..0042: hiScore = 0, num = 0, num2 = 50
  modify fun s => { s with hiScore := 0 }
  let mut num2 : Int := 50
  -- IL_0043..004b
  let (startX, endX, startY, endY) := getTestedRoomBounds w (← get)
  -- IL_0050..0055: new int[TileID.Count]
  if w.tables.tileIdCount < 0 then
    throw "ScoreRoom IL_0055: newarr with negative TileID.Count (OverflowException)"
  let counts0 := Array.replicate w.tables.tileIdCount.toNat (0 : Int)
  -- IL_005a..0069
  let counts ← countTileTypesInArea w counts0 (i32 (startX + 1)) (i32 (endX - 1))
    (i32 (startY + 2)) (i32 (endY + 1))
  -- IL_006e..0075: num3 = -GetTileTypeCountByCategory(counts, TotalGoodEvil)
  let mut evil := i32 (-(← getTileTypeCountByCategory counts 4))
  -- IL_0077..007e
  if evil < 50 then evil := 0
  -- IL_0080..0084
  num2 := i32 (num2 - evil)
  -- IL_0085..008f
  if evil > 0 then modify fun s => { s with roomEvil := true }
  -- IL_0090..009e
  if num2 ≤ -250 then
    modify fun s => { s with hiScore := num2 }
    return
  -- IL_009f..00b7: the loop reuses the bound locals with the room's own bounds
  let s ← get
  let x1 := s.roomX1
  let x2 := s.roomX2
  let y1 := s.roomY1
  let y2 := s.roomY2
  -- IL_00b9..03f3: for (x = x1 + 1; x < x2; x++) for (y = y1 + 2; y < y2 + 2; y++)
  for x in upTo (x1 + 1) x2 do
    for y in upTo (y1 + 2) (y2 + 2) do
      scoreSpot w x y num2

/-- `WorldGen.IsRoomConsideredOccupiedForNPCIndex(int npc)` (MethodDef 4378). -/
def isRoomConsideredOccupiedForNPCIndex (w : World) (npcs : Array Npc) (index : Int) : M Bool := do
  let s ← get
  -- IL_0000..0066
  for i in upTo 0 w.maxNPCs do
    if i == index then continue
    let n ← npcAt npcs "IsRoomConsideredOccupiedForNPCIndex IL_0010" i
    if !n.active || !n.townNPC || n.homeless then continue
    if n.homeTileX != s.bestX || n.homeTileY != s.bestY then continue
    -- IL_0044..005a: CanNPCsLiveWithEachOther(Main.npc[npc], other) false → result false
    let me ← npcAt npcs "IsRoomConsideredOccupiedForNPCIndex IL_004f" index
    if !canLiveWithNpc me n then return false
  -- IL_0068
  return true

/-- `Main.NoFunctionalSurface` getter: `!(worldSurface > 30.0)` with `cgt.un` (NaN → false). -/
def noFunctionalSurface (w : World) : Bool :=
  !(w.worldSurface > 30.0 || w.worldSurface.isNaN)

/-- `WorldGen.CheckSpecialTownNPCSpawningConditions(int type)` (MethodDef 4366). -/
def checkSpecialTownNPCSpawningConditions (w : World) (type : Int) : M Bool := do
  -- IL_0000..0006: only the Truffle (160)
  if type != 160 then return true
  let s ← get
  -- IL_000b..0027: below the surface without the unlock → false
  if !w.unlockedTruffleSpawn then
    -- IL_0012..001d: `ble.un` falls through only when roomY2 > worldSurface (ordered)
    if Float.ofInt s.roomY2 > w.worldSurface then
      if !noFunctionalSurface w then return false
  -- IL_0028..0030
  let (startX, endX, startY, endY) := getTestedRoomBounds w s
  -- IL_0035..00aa: for (x = startX + 1; x < endX; x++) for (y = startY + 2; y < endY + 2; y++)
  let mut count : Int := 0
  for x in upTo (startX + 1) endX do
    for y in upTo (startY + 2) (endY + 2) do
      let t ← getTile w "CheckSpecialTownNPCSpawningConditions IL_004f" x y
      match activeType? t with
      | some 70 | some 71 | some 72 | some 528 => count := i32 (count + 1)
      | _ => pure ()
  -- IL_00ac..00b8
  return count ≥ w.mushroomTileThreshold

/-! ## `QuickFindHome` -/

inductive Event where
  /-- `AchievementsHelper.NotifyProgressionEvent(int)` (IL_0203..0204 passes 8). -/
  | notifyProgressionEvent (eventId : Int)
  deriving BEq, Repr

structure Result where
  /-- `Main.npc[index]` afterwards (only `homeTileX`, `homeTileY`, `homeless`, `homelessDespawn`
  can change). -/
  npc : Npc
  state : RoomState
  events : Array Event
  /-- `Main.tileSolid[379]` after IL_022d..0238 restored it from `loc.0`; equal to the input
  table's entry. -/
  tileSolid379AfterReturn : Bool

private def quickFindHomeBody (w : World) (npcs : Array Npc) (index : Int) :
    M (Npc × Array Event) := do
  let npc ← npcAt npcs "QuickFindHome IL_001e" index
  let hx := npc.homeTileX
  let hy := npc.homeTileY
  -- IL_0018..0068: home inside the margins, else straight to IL_022d
  if !(hx > 10 && hy > 10 && hx < w.maxTilesX - 10 && hy < w.maxTilesY) then
    return (npc, #[])
  -- IL_006d..0093
  modify fun s => { s with canSpawn := false }
  let _ ← startRoomCheck w hx (hy - 1)
  -- IL_0094..00ef: for (x = hx - 1; x < hx + 2; x++) for (y = hy - 1; y < hy + 2; y++);
  -- a successful check ends only the inner loop (IL_00c5 → IL_00dc)
  if !(← get).canSpawn then
    for x in upTo (hx - 1) (hx + 2) do
      for y in upTo (hy - 1) (hy + 2) do
        if (← startRoomCheck w x y) then break
  -- IL_00f1..0159: for (x = hx - 10; x <= hx + 10; x += 2) for (y = hy - 10; y <= hy + 10; y += 2)
  if !(← get).canSpawn then
    for x in stepTwoThrough (hx - 10) (hx + 10) do
      for y in stepTwoThrough (hy - 10) (hy + 10) do
        if (← startRoomCheck w x y) then break
  -- IL_015b..016f
  if !(← checkSpecialTownNPCSpawningConditions w npc.type) then
    modify fun s => { s with canSpawn := false }
  -- IL_0174..0179, IL_0220..0228
  if !(← get).canSpawn then
    return ({ npc with homeless := true }, #[])
  -- IL_017e..0183
  let _ ← roomNeeds w
  -- IL_0184..0199
  if (← get).canSpawn then scoreRoom w npcs index npc.type
  -- IL_019e..01b3
  let s ← get
  if s.canSpawn && s.hiScore > 0 then
    let occupied ← isRoomConsideredOccupiedForNPCIndex w npcs index
    modify fun s => { s with canSpawn := occupied }
  -- IL_01b8..020f
  let s ← get
  if s.canSpawn && s.hiScore > 0 then
    let housed := { npc with
      homeTileX := s.bestX, homeTileY := s.bestY, homeless := false, homelessDespawn := false }
    modify fun s => { s with canSpawn := false }
    return (housed, #[.notifyProgressionEvent 8])
  -- IL_0211..021e
  return ({ npc with homeless := true }, #[])

/-- `WorldGen.QuickFindHome(int npc)` (MethodDef 4377). `npcs` is `Main.npc` by slot; only
`npcs[index]` changes, returned as `Result.npc`.

Coordinate arithmetic is plain `Int` except where noted; it cannot leave int32 because the entry
guard below limits the world dimensions (an explicit boundary, not an IL branch). -/
def quickFindHome (w : World) (npcs : Array Npc) (index : Nat) (state : RoomState) :
    Except String Result := do
  let limit : Int := 1 <<< 24
  for (name, v) in [("Main.maxTilesX", w.maxTilesX), ("Main.maxTilesY", w.maxTilesY),
      ("WorldGen.lastMaxTilesY", w.lastMaxTilesY), ("Main.maxNPCs", w.maxNPCs),
      ("Main.tile width", w.tileArrayWidth), ("Main.tile height", w.tileArrayHeight)] do
    if v < 0 || v > limit then
      throw s!"QuickFindHome: {name} = {v} outside the modelled range 0..{limit}"
  if w.maxRoomSize < -4096 || w.maxRoomSize > 4096 then
    throw s!"QuickFindHome: WorldGen.maxRoomSize = {w.maxRoomSize} outside the modelled range ±4096"
  -- IL_0000..000b: bool flag = Main.tileSolid[379]
  let saved ← readBool "QuickFindHome IL_000a" "Main.tileSolid" w.tables.tileSolid 379
  -- IL_000c..0017: Main.tileSolid[379] = true for everything below
  let w' := { w with tables := { w.tables with tileSolid := w.tables.tileSolid.set! 379 true } }
  let ((npc, events), st) ← (quickFindHomeBody w' npcs index).run state
  -- IL_022d..0239: Main.tileSolid[379] = flag; `w` itself was never changed
  return { npc, state := st, events, tileSolid379AfterReturn := saved }

/-!
## Inputs this transcription needs (exact CIL names)

Static tables (`Tables`), all indexed by tile id unless noted, read natively:
* `Terraria.Main::tileSolid` (bool[]) — `Main.tileSolid[379]` is forced to `true` for the duration
  (QuickFindHome IL_000c..0017) and restored at IL_022d..0238.
* `Terraria.Main::tileSolidTop` (bool[]) — `Collision.SolidTiles` IL_007a.
* `Terraria.Main::wallHouse` (bool[], by wall id) — `CheckRoom` IL_02e4, IL_034e.
* `Terraria.ID.TileID/Sets::HousingWalls` (bool[]) — `CheckRoom` IL_0331, IL_039b.
* `Terraria.ID.TileID/Sets::IgnoredInHouseScore` (bool[]) — `ScoreRoom` IL_01da.
* `Terraria.ID.TileID/Sets::BasicChest` (bool[]) — `ScoreRoom` IL_0210.
* `Terraria.ID.TileID/Sets/RoomNeeds::CountsAsChairTypes`, `CountsAsTableTypes`,
  `CountsAsTorchTypes`, `CountsAsDoorTypes` (int[], iterated in array order) — `RoomNeeds`.
* `Terraria.ID.TileID::Count` — `StartRoomCheck` IL_005b, `ScoreRoom` IL_0050.

Static scalars (`World`): `Terraria.Main::maxTilesX`, `Terraria.Main::maxTilesY`,
`Terraria.WorldGen::lastMaxTilesY`, `Terraria.Main::worldSurface` (double),
`Terraria.NPC::unlockedTruffleSpawn`, `Terraria.SceneMetrics::MushroomTileThreshold`,
`Terraria.Main::maxNPCs`, `Terraria.WorldGen::maxRoomTiles`, `Terraria.WorldGen::maxRoomSize`,
`Terraria.ID.ContentSamples::NpcsByNetId` (only `TryGetValue(type)` → `housingCategory`),
`Terraria.Main::tile` (`Tile[,]`: per cell `type`, `wall`, `frameX`, `frameY`, active and inActive
bits of `sTileHeader`; plus the array bounds).

NPC fields (`Npc`, per `Main.npc` slot): `active`, `type`, `townNPC`, `homeless`,
`homelessDespawn` (written only), `homeTileX`, `homeTileY`, `housingCategory`.

WorldGen statics (`RoomState`, in and out): `canSpawn`, `roomX1`, `roomX2`, `roomY1`, `roomY2`,
`numRoomTiles`, `roomTiles` (BitSet2D: `size`, `offset`, `bits`), `houseTile`,
`roomCheckFailureReason`, `roomHasStinkbug`, `roomHasEchoStinkbug`, `roomChair`, `roomDoor`,
`roomTable`, `roomTorch`, `roomOccupied`, `roomHasStandingSpace`, `roomEvil`, `sharedRoomX`,
`hiScore`, `bestX`, `bestY`. (`_roomCheckStack` is cleared before every use and is a local.)

Side effect outside the state: `AchievementsHelper.NotifyProgressionEvent(8)` (`Event`).
`WorldGen.IsRoomConsideredAlreadyOccupied` is not reached from `QuickFindHome`.
-/

end Lean4Terria.OriginalHousing
