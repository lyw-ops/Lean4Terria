import Std.Data.HashMap
import Lean4Terria.OriginalWorld
import Lean4Terria.OriginalTileRules
import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalWallWork.Tables

/-!
# Wall frames (`Framing.WallFrame`, Terraria 1.4.5.8)

`OriginalWorld.Tile` stores the wall type and paint but not `wallFrameX/Y` or `wallFrameNumber`:
vanilla does not save them either and re-frames every wall at load with `resetFrame`, drawing the
variant from `WorldGen.genRand`. A loaded world's frames therefore come from a captured
`OriginalFraming.FramePatch`; cells this module edits keep their frames in `WallFrames`.

`Framing.WallFrame(x, y, resetFrame)`:
* outside 1 … size−2, or no wall: nothing to frame (a type ≥ `WallID.Count` is cleared first);
* neighbour mask up 1, left 2, right 4, down 8: a neighbour counts when it has any wall, or is an
  active `TruncatesWalls` tile, unless it is an invisible wall (`ShouldShowInvisibleBlocksAndWalls`
  is false without the echo view);
* variant: large-frame walls from the phlebas / lazure lookups; otherwise `resetFrame` draws
  `genRand.Next(0, 3)` (wall 21: one more `Next(2)`, 0 → variant 2), else the stored variant;
* mask 15 adds `centerWallFrameLookup[x % 3][y % 3]`; frame = `wallFrameLookup[mask][variant]`.
-/

namespace Lean4Terria.OriginalWallWork

open OriginalWorld (Terrain Tile)

structure WallFrame where
  frameX : Nat
  frameY : Nat
  number : Nat
  deriving BEq, Repr, Inhabited

/-- Frames of the cells whose wall frame is known here (edited cells, or seeded from a patch). -/
abbrev WallFrames := Std.HashMap (Nat × Nat) WallFrame

/-- The variant a captured frame implies, for seeding `WallFrames` from a `FramePatch`. -/
def numberOfFrame (frameX frameY : Nat) : Option Nat := Id.run do
  for row in Tables.wallFrameLookup do
    for n in [:row.size] do
      if row[n]! == (frameX, frameY) then return some n
  return none

private def tileAt (terrain : Terrain) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (terrain.tile? x.toNat y.toNat).getD default

private def connects (t : Tile) : Bool :=
  (t.wall > 0 || t.id.any Tables.truncatesWalls) && !t.invisibleWall

/-- The neighbour mask of `WallFrame` (before the centre variants). -/
def wallMask (terrain : Terrain) (x y : Nat) : Nat :=
  let x : Int := x
  let y : Int := y
  let w := terrain.header.width
  let h := terrain.header.height
  (if y - 1 ≥ 0 && connects (tileAt terrain x (y - 1)) then 1 else 0) +
  (if x - 1 ≥ 0 && connects (tileAt terrain (x - 1) y) then 2 else 0) +
  (if x + 1 ≤ w - 1 && connects (tileAt terrain (x + 1) y) then 4 else 0) +
  (if y + 1 ≤ h - 1 && connects (tileAt terrain x (y + 1)) then 8 else 0)

private def largeVariant (wall : UInt16) (x y : Nat) : Option Nat :=
  match Tables.wallLargeFrames wall with
  | 1 => some ((Tables.phlebasLookup[y % 4]!)[x % 3]! - 1)
  | 2 => some ((Tables.lazureLookup[x % 2]!)[y % 2]! - 1)
  | _ => none

private def frameAt (terrain : Terrain) (x y number : Nat) : WallFrame :=
  let mask := wallMask terrain x y
  let mask := if mask == 15 then mask + (Tables.centerWallFrameLookup[x % 3]!)[y % 3]! else mask
  let (frameX, frameY) := (Tables.wallFrameLookup[mask]!)[number % 3]!
  ⟨frameX, frameY, number⟩

/-- Draw the variant against the current wall and foreground neighbours. -/
def wallSpriteFrame (terrain : Terrain) (x y : Nat) (wall : UInt16) (number : Nat) : WallFrame :=
  frameAt terrain x y ((largeVariant wall x y).getD number)

/-- Stable variation for walls outside a native frame capture. -/
def fallbackWallNumber (worldId : UInt32) (x y : Nat) : Nat :=
  let a := x.toUInt32 * 73856093
  let b := y.toUInt32 * 19349663
  ((a ^^^ b ^^^ worldId) % 3).toNat

structure Framed where
  frames : WallFrames
  random : UnifiedRandom
  draws : Nat := 0

/-- `Framing.WallFrame(x, y, resetFrame)` on `frames`. `stored` gives the variant of a cell not in
`frames` (for example from a captured patch); `none` counts as variant 0. -/
def wallFrame (terrain : Terrain) (stored : Nat → Nat → Option Nat) (s : Framed) (x y : Int)
    (resetFrame : Bool) : Framed := Id.run do
  let w := terrain.header.width
  let h := terrain.header.height
  if x ≤ 0 || y ≤ 0 || x ≥ w - 1 || y ≥ h - 1 then return s
  let (xn, yn) := (x.toNat, y.toNat)
  let tile := tileAt terrain x y
  if tile.wall == 0 || tile.wall.toNat ≥ Tables.wallCount then
    return { s with frames := s.frames.erase (xn, yn) }
  let mut s := s
  let mut number := 0
  if let some variant := largeVariant tile.wall xn yn then number := variant
  else if resetFrame then
    let (n, random) := s.random.next 0 3
    s := { s with random, draws := s.draws + 1 }
    number := n.toNat
    if tile.wall == 21 then
      let (k, random) := s.random.next 0 2
      s := { s with random, draws := s.draws + 1 }
      if k == 0 then number := 2
  else
    number := (((s.frames.get? (xn, yn)).map (·.number)).orElse (fun _ => stored xn yn)).getD 0
  return { s with frames := s.frames.insert (xn, yn) (frameAt terrain xn yn number) }

/-- `WorldGen.SquareWallFrame(x, y, resetFrame)`: the 3 × 3 block column by column, only the centre
with `resetFrame`. -/
def squareWallFrame (terrain : Terrain) (stored : Nat → Nat → Option Nat) (s : Framed) (x y : Int)
    (resetFrame : Bool := true) : Framed := Id.run do
  let mut s := s
  for dx in [0:3] do
    for dy in [0:3] do
      s := wallFrame terrain stored s (x - 1 + dx) (y - 1 + dy) (resetFrame && dx == 1 && dy == 1)
  return s

end Lean4Terria.OriginalWallWork
