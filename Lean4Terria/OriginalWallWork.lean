import Lean4Terria.OriginalItems
import Lean4Terria.OriginalWallWork.Catalog
import Lean4Terria.OriginalWallWork.Frames

/-!
# Background walls: placement and hammering (Terraria 1.4.5.8 Classic, single player)

Hand translation of the vanilla item phase for wall items and hammers.

**Placement** (`Player.PlaceThing_Walls`, item with `createWall ≥ 0`):
1. reach `IsInTileInteractionRange(tileTarget, Simple, tileBoost + blockRange)` (5 × 3 tiles from the
   player's box, as for blocks), `ItemTimeIsZero`, `itemAnimation > 0`, use held;
2. support: one of the four neighbours has an active tile or a wall;
3. the target's wall is not already `createWall`;
4. block swap (`TileReplacementEnabled`, on by default): `PlaceThing_TryReplacingWalls` replaces a
   wall ≠ 350 when `WorldGen.NearFriendlyWall` (the target or a 4-neighbour has no wall or a
   `wallHouse` wall): `ReplaceWall` drops the old wall's item (16 × 16 at the cell), sets the type,
   clears its paint and `SquareWallFrame(x, y, true)`;
5. otherwise `WorldGen.PlaceWall` fills an empty wall cell (inside a 2-cell world margin) and calls
   `SquareWallFrame(x, y, true)`;
6. on success `ApplyItemTime(item, wallSpeed)` (`useTime` 7 for Wood Wall), one item is used, and
   `PlaceThing_Walls_FillEmptySpace` (stack > 1) fills each wall-less 4-neighbour whose four
   neighbours all hold `createWall`, using one more item each.

**Hammer** (`ItemCheck_UseMiningTools` → `TryFindingWallToHammer` → `TryHittingWall` → `PickWall` →
`KillWall`): see `hammer`.

Random stream: the samples drawn here are the wall-frame variants of `SquareWallFrame`'s centre
(1 per placed or replaced wall), `HitTileObject.AddDamage` (1 per hammer hit) and item drops (2).
Desktop sounds and on-screen dust draw more (see `OriginalBankMining`).
-/

namespace Lean4Terria.OriginalWallWork

open OriginalCollision (Grid Vector single)
open OriginalItems (Stack WorldItems)
open OriginalItemUse (Timing)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Terrain Tile)

/-- Walls, their frames, world items and the random stream for one item phase. -/
structure Work where
  terrain : Terrain
  frames : WallFrames
  random : UnifiedRandom
  items : WorldItems := #[]
  draws : Nat := 0
  /-- Cells whose wall type changed this phase (for redrawing / F5 dirty tracking). -/
  changed : Array (Nat × Nat) := #[]

/-- Progress flags `KillWall_CheckFailure` reads. -/
structure Progress where
  downedBoss3 : Bool := false
  downedGolemBoss : Bool := false

private def Work.tile (w : Work) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (w.terrain.tile? x.toNat y.toNat).getD default

private def Work.setWall (w : Work) (x y : Nat) (wall : UInt16) : Work :=
  match w.terrain.tile? x y with
  | none => w
  | some t =>
    let t := { t with wall, wallPaint := 0, invisibleWall := false, fullbrightWall := false }
    match w.terrain.replaceTile x y t with
    | some terrain => { w with terrain, changed := if w.changed.contains (x, y) then w.changed else w.changed.push (x, y) }
    | none => w

private def Work.square (w : Work) (stored : Nat → Nat → Option Nat) (x y : Int) : Work :=
  let s := squareWallFrame w.terrain stored ⟨w.frames, w.random, 0⟩ x y true
  { w with frames := s.frames, random := s.random, draws := w.draws + s.draws }

private def Work.dropAt (w : Work) (cell : Cell) (item : UInt16) : Work :=
  let grid := Grid.ofTerrain w.terrain OriginalTileRules.collision
  let (items, random) := OriginalItems.spawnStack w.items cell (.ofItem item) w.random (some grid)
  { w with items, random, draws := w.draws + 2 }

/-- `WorldGen.KillWall_CheckFailure`: dungeon walls before Skeletron, Lihzahrd brick wall (87) before Golem. -/
def killWallFails (progress : Progress) (wall : UInt16) : Bool :=
  (Tables.wallDungeon wall && !progress.downedBoss3) || (wall == 87 && !progress.downedGolemBoss)

/-- `WorldGen.NearFriendlyWall`: true near the world edge, or when the cell or a 4-neighbour has no
wall or a player-made wall. -/
def nearFriendlyWall (terrain : Terrain) (x y : Nat) : Bool :=
  if x < 2 || x ≥ terrain.header.width - 2 || y < 2 || y ≥ terrain.header.height - 2 then true
  else [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)].any fun ((dx, dy) : Int × Int) =>
    let t := (terrain.tile? (x + dx).toNat (y + dy).toNat).getD default
    t.wall == 0 || Tables.wallHouse t.wall

/-- `WorldGen.PlaceWall(x, y, type)`; `none` when nothing was placed. -/
def placeWall (w : Work) (stored : Nat → Nat → Option Nat) (x y : Nat) (wall : UInt16) : Option Work := do
  guard (x > 1 && y > 1 && x < w.terrain.header.width - 2 && y < w.terrain.header.height - 2)
  guard ((w.tile x y).wall == 0)
  return (w.setWall x y wall).square stored x y

/-- `WorldGen.ReplaceWall(x, y, type)`; `none` when refused. -/
def replaceWall (w : Work) (stored : Nat → Nat → Option Nat) (progress : Progress) (x y : Nat) (wall : UInt16) :
    Option Work := do
  guard (wall.toNat < Tables.wallCount)
  let old := (w.tile x y).wall
  guard (old != 0 && wall != 0 && old != 350)
  guard (!killWallFails progress old)
  let w := match Tables.wallDrop old with
    | some item => w.dropAt ⟨x, y⟩ item
    | none => w
  return (w.setWall x y wall).square stored x y

structure Placed where
  work : Work
  timing : Timing
  stack : Option Stack
  /-- Items used by this phase (the placed wall plus filled holes). -/
  used : Nat := 0

/-- One item phase with a wall item held (from an inventory slot or the mouse). -/
def place (w : Work) (stored : Nat → Nat → Option Nat) (body : Body) (timing : Timing) (stack : Stack)
    (held : Bool) (target : Option Cell) (tileReplacement : Bool := true) (progress : Progress := {}) :
    Placed := Id.run do
  let some (wall, useTime, useAnimation) := Tables.wallItem stack.item
    | return { work := w, timing, stack := some stack }
  let timing := timing.advance held useAnimation (stack.count != 0)
  let remaining := if stack.count == 0 && timing.itemAnimation == 0 then none else some stack
  let idle : Placed := { work := w, timing, stack := remaining }
  let some cell := target | return idle
  if !held || stack.count == 0 || timing.itemAnimation == 0 || timing.itemTime != 0 then return idle
  if !inToolRange body cell 5 3 0 then return idle
  let (x, y) := (cell.x, cell.y)
  let neighbors : List (Int × Int) := [(1, 0), (-1, 0), (0, 1), (0, -1)]
  if !(neighbors.any fun (dx, dy) => let t := w.tile (x + dx) (y + dy); t.id.isSome || t.wall > 0) then
    return idle
  if (w.tile x y).wall == wall then return idle
  -- block swap first; a successful swap skips PlaceWall and the hole filling
  let target := w.tile x y
  let swapped := if tileReplacement && target.wall > 0 && target.wall != 350 && nearFriendlyWall w.terrain x y then
      replaceWall w stored progress x y wall else none
  let (w, placedFresh) ← match swapped with
    | some w => pure (w, false)
    | none => match placeWall w stored x y wall with
      | some w => pure (w, true)
      | none => return idle
  let mut w := w
  let mut used := 1
  if placedFresh && stack.count > 1 then
    for (dx, dy) in [((-1 : Int), (0 : Int)), (1, 0), (0, -1), (0, 1)] do
      let (hx, hy) := (x + dx, y + dy)
      if hx < 0 || hy < 0 || (w.tile hx hy).wall != 0 then continue
      let around := neighbors.filter fun (ex, ey) => (w.tile (hx + ex) (hy + ey)).wall == wall
      -- the stack > 1 test is made once, before any of the four holes (vanilla does not re-check)
      if around.length == 4 then
        if let some w' := placeWall w stored hx.toNat hy.toNat wall then
          w := w'
          used := used + 1
  let stack := { stack with count := stack.count - used }
  let timing := timing.setTime useTime
  let timing := if stack.count == 0 then { timing with itemTime := timing.itemAnimation } else timing
  return { work := w, timing, stack := some stack, used }

/-! ## Hammer -/

/-- Wall hit records (`HitTile` entries of type 2), most recent first. Vanilla keeps tile (type 1)
and wall entries in one 500-slot `HitTile` whose `Prune` ages both; this cache holds the wall entries. -/
structure WallHit where
  cell : Cell
  damage : Nat
  timeToLive : Nat
  deriving BEq, Repr

structure WallHits where
  entries : Array WallHit := #[]
  deriving BEq, Repr

def WallHits.damage (hits : WallHits) (cell : Cell) : Nat :=
  ((hits.entries.find? (·.cell == cell)).map (·.damage)).getD 0

/-- `HitTile.AddDamage(slot, damage, true)`: time to live 60, moved to the front. -/
def WallHits.record (hits : WallHits) (cell : Cell) (damage : Nat) : WallHits :=
  let previous := hits.damage cell
  let hit : WallHit := ⟨cell, previous + damage, 60⟩
  { entries := (#[hit] ++ hits.entries.filter (fun (h : WallHit) => h.cell != cell)).extract 0 500 }

def WallHits.forget (hits : WallHits) (cell : Cell) : WallHits :=
  { entries := hits.entries.filter (·.cell != cell) }

/-- `HitTile.Prune` for wall entries: an entry ends at time to live 1 or without its wall; otherwise
it ages one step, losing 10 / 7 / 5 / 2 damage below 12 / 24 / 36 / 48. -/
def WallHits.prune (hits : WallHits) (terrain : Terrain) : WallHits :=
  { entries := hits.entries.filterMap fun hit => do
      guard (hit.timeToLive > 1)
      let timeToLive := hit.timeToLive - 1
      let decay := if timeToLive < 12 then 10 else if timeToLive < 24 then 7
        else if timeToLive < 36 then 5 else if timeToLive < 48 then 2 else 0
      guard (decay ≤ hit.damage)
      guard (((terrain.tile? hit.cell.x hit.cell.y).map (·.wall)).getD 0 != 0)
      return { hit with timeToLive, damage := hit.damage - decay } }

/-- `Player.CanPlayerSmashWall`: never wall 350; a player-made wall always; a natural wall when a
cell of its 3 × 3 block has no wall or a player-made wall. -/
def canSmashWall (terrain : Terrain) (x y : Nat) : Bool := Id.run do
  let t := (terrain.tile? x y).getD default
  if t.wall == 350 then return false
  if Tables.wallHouse t.wall then return true
  for i in [x - 1:x + 2] do
    for j in [y - 1:y + 2] do
      let u := (terrain.tile? i j).getD default
      if u.wall == 0 || Tables.wallHouse u.wall then return true
  return false

private def enclosedNatural (terrain : Terrain) (x y : Nat) : Bool := Id.run do
  let t := (terrain.tile? x y).getD default
  if t.wall == 0 then return true          -- an empty cell keeps searching
  if Tables.wallHouse t.wall then return false
  for i in [x - 1:x + 2] do
    for j in [y - 1:y + 2] do
      if ((terrain.tile? i j).getD default).wall != t.wall then return false
  return true

/-- `Math.Round` on a double: halves go to the even neighbour. -/
private def roundHalfEven (v : Float) : Float :=
  let f := v.floor
  let d := v - f
  if d < 0.5 then f else if d > 0.5 then f + 1
  else if (f / 2).floor * 2 == f then f else f + 1

/-- `ItemCheck_UseMiningTools_TryFindingWallToHammer`. A target that is empty, or a natural wall whose
3 × 3 block is all that wall, with no active tile, searches the 2 × 2 cells toward the mouse's
sub-tile position (`mouse / 16` against `Math.Round`), column by column, for the first player-made
wall or natural edge; the last cell scanned is kept when none is found. `mouse` is the world
pixel (`Main.mouseX + screenPosition.X`, …). -/
def findWallToHammer (terrain : Terrain) (target : Cell) (mouse : Float32 × Float32) : Cell := Id.run do
  let t := (terrain.tile? target.x target.y).getD default
  if !enclosedNatural terrain target.x target.y || t.id.isSome then return target
  let mx := (mouse.1 / 16).toFloat
  let my := (mouse.2 / 16).toFloat
  let dx : Int := if mx < roundHalfEven mx then 0 else -1
  let dy : Int := if my < roundHalfEven my then 0 else -1
  let mut cell := target
  for i in [0:2] do
    for j in [0:2] do
      let x := ((target.x : Int) + dx + i).toNat
      let y := ((target.y : Int) + dy + j).toNat
      cell := ⟨x, y⟩
      if !enclosedNatural terrain x y then return cell
  return cell

/-- `WorldGen.KillWall(x, y, fail)`. -/
def killWall (w : Work) (stored : Nat → Nat → Option Nat) (progress : Progress) (cell : Cell) (fail : Bool) : Work := Id.run do
  let wall := (w.tile cell.x cell.y).wall
  if wall == 0 then return w
  let fail := fail || killWallFails progress wall
  if fail then return w.square stored cell.x cell.y
  let w := match Tables.wallDrop wall with
    | some item => w.dropAt cell item
    | none => w
  -- a FramesOnKillWall tile on this cell (torches, paintings …) is re-framed by TileFrame: not covered here
  return (w.setWall cell.x cell.y 0).square stored cell.x cell.y

structure Hammered where
  work : Work
  hits : WallHits
  timing : Timing
  /-- The wall cell that was hit this phase. -/
  hit : Option Cell := none

/-- One hammer item phase for the wall part of `ItemCheck_UseMiningTools`
(hammering hammer-target tiles and pounding blocks are not covered: a target tile in
`Main.tileHammer` stops the wall path). -/
def hammer (w : Work) (stored : Nat → Nat → Option Nat) (hits : WallHits) (body : Body) (timing : Timing)
    (item : UInt16) (held : Bool) (target : Option Cell) (mouse : Float32 × Float32)
    (progress : Progress := {}) (pressed : Bool := true) : Hammered := Id.run do
  let some tool := Tables.hammerStats item | return { work := w, hits, timing }
  let timing := timing.advance (held && (tool.autoReuse || pressed)) tool.useAnimation
  let idle : Hammered := { work := w, hits, timing }
  let some target := target | return idle
  if !inToolRange body target 5 3 tool.tileBoost then return idle
  if !(held && timing.itemAnimation > 0 && timing.itemTime == 0) then return idle
  let targetTile := w.tile target.x target.y
  if targetTile.id.any Tables.tileHammer then return idle
  let cell := findWallToHammer w.terrain target mouse
  let t := w.tile cell.x cell.y
  if t.wall == 0 then return idle
  if t.id.isSome && cell == target && t.id.any Tables.tileHammer then return idle
  if !canSmashWall w.terrain cell.x cell.y then return idle
  let damage := (tool.power * 3) / 2
  -- PickWall: HitTileObject.AddDamage draws one sample
  let (_, random) := w.random.nextInt
  let w := { w with random, draws := w.draws + 1 }
  let hits := hits.record cell damage
  let (w, hits) := if hits.damage cell ≥ 100 then
      (killWall w stored progress cell false, hits.forget cell)
    else (killWall w stored progress cell true, hits)
  let hits := hits.prune w.terrain
  return { work := w, hits, timing := timing.setTime (tool.useTime / 2), hit := some cell }

end Lean4Terria.OriginalWallWork
