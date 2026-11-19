import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalTileRules

/-! The ordinary three-tile door branch of Terraria 1.4.5.8 `WorldGen.OpenDoor`.
The NPC calls this before its movement collision. The caller must publish the
edits immediately, so later NPC slots see the opened door in the same tick.
-/
namespace Lean4Terria.OriginalDoor

open OriginalCollision (Grid)
open OriginalWorld (Tile Terrain)

structure TileEdit where
  x : Nat
  y : Nat
  tile : Tile
  deriving BEq, Repr

structure Sound where
  typeId : Int
  x : Int
  y : Int
  style : Int := 1
  deriving BEq, Repr

structure Result where
  opened : Bool := false
  tileEdits : Array TileEdit := #[]
  sounds : Array Sound := #[]
  deriving Repr

/-- Use the returned Grid for the current NPC's collision and the next slot. -/
def applyGrid (grid : Grid) (edits : Array TileEdit) : Grid :=
  if edits.isEmpty then grid else
  { grid with tileAt := fun x y =>
      ((edits.findRev? fun edit => edit.x == x && edit.y == y).map (·.tile)).getD (grid.tileAt x y) }

/-- Persist exactly the same edits into the session's terrain. -/
def applyTerrain (terrain : Terrain) (edits : Array TileEdit) : Except String Terrain := do
  let mut terrain := terrain
  for edit in edits do
    let some next := terrain.replaceTile edit.x edit.y edit.tile
      | throw s!"door tile outside terrain: {edit.x},{edit.y}"
    terrain := next
  return terrain

private def tileAt (grid : Grid) (x y : Int) : Tile :=
  if x < 0 || y < 0 || x ≥ (grid.width : Int) || y ≥ (grid.height : Int) then {}
  else grid.tileAt x.toNat y.toNat

def locked (tile : Tile) : Bool :=
  let (fx, fy) := tile.frame.getD (0, 0)
  tile.id == some 10 && 594 ≤ fy && fy ≤ 646 && fx < 54

/-- The client `KillTile(fail=true)` branch for an intact type 10 door.
Locked Lihzahrd doors return before effects. Other styles request Dig and make
three `KillTile_MakeTileDust` attempts, each returning the no-dust slot 6000.
This path neither creates particles nor draws from `Main.rand`/`genRand`. -/
def failedHitSounds (tile : Tile) (x y : Int) : Array Sound :=
  if locked tile then #[] else #[⟨0, x * 16, y * 16, 1⟩]

/-- Attempt the requested direction only; an obstructed swing does not try the
other side. Ordinary doors retain each destination tile's wall, wires, actuator,
liquid and shape; each row's block paint and coatings are copied from the hinge.

The current supported swing space is empty. Cuttable plants, hanging tiles and
drips require their own `KillTile` effects and are reported at that boundary.
-/
def openDoor (grid : Grid) (x y direction : Int) : Except String Result := do
  let tile := tileAt grid x y
  if tile.id != some 10 || locked tile then return {}
  let (fx, fy) := tile.frame.getD (0, 0)
  if fx < 0 || fy < 0 then throw "door has invalid negative frames"
  let style := fy / 54 + 36 * (fx / 54)
  let top := y - (fy % 54) / 18
  if x ≤ 0 || top ≤ 0 || x + 1 ≥ (grid.width : Int) || top + 3 ≥ (grid.height : Int) then
    throw "door framing reaches world boundary"
  unless (List.range 3).all (fun row => (tileAt grid x (top + row)).id == some 10) do
    throw "door is missing a closed segment; native validation would kill remaining segments"
  -- Keep anchor destruction and frame-important neighbor chains outside this
  -- port. The validated dungeon doors have two ordinary full solid anchors.
  let anchor (tile : Tile) : Bool :=
    !tile.actuated && tile.shape == .full && tile.id.any (fun id =>
      let rule := grid.rules id
      rule.solid && !rule.solidTop)
  unless anchor (tileAt grid x (top - 1)) && anchor (tileAt grid x (top + 3)) do
    throw "door needs the supported full-solid anchors"
  let swing := if direction == -1 then x - 1 else x + 1
  for row in [:3] do
    let side := tileAt grid swing (top + row)
    if let some id := side.id then
      if OriginalTileRules.isCuttable id || id == 165 || (#[373, 374, 375, 461, 709] : Array UInt16).contains id then
        throw "opening door would kill a plant, hanging tile or drip"
      return {}
  let left := if direction == -1 then x - 1 else x
  let baseX := 72 * (fx / 54) + if direction == -1 then 36 else 0
  let baseY := (style % 36) * 54
  let mut edits := #[]
  for row in [:3] do
    let source := tileAt grid x (top + row)
    for col in [:2] do
      let destination := tileAt grid (left + col) (top + row)
      let tile := { destination with
        id := some 11
        frame := some (baseX + col * 18, baseY + row * 18)
        tilePaint := source.tilePaint
        invisibleBlock := source.invisibleBlock
        fullbrightBlock := source.fullbrightBlock }
      edits := edits.push ⟨(left + col).toNat, (top + row).toNat, tile⟩
  return {
    opened := true, tileEdits := edits
    sounds := #[⟨8, x * 16, y * 16, 1⟩] }

end Lean4Terria.OriginalDoor
