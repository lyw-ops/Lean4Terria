import Lean
import Lean4Terria.OriginalGroundAtlas
import Lean4Terria.OriginalWorld
import Lean4Terria.OriginalTileRules
import Lean4Terria.OriginalWallWork.Frames

namespace Lean4Terria.OriginalFraming

open OriginalWorld (Terrain Tile TileShape)
open OriginalCollision (slopeId)

/-- Optional sprite coordinates captured in a loaded vanilla world. A
foreground frame must still have the saved type and shape. -/
structure FramePatch where
  worldId : Nat
  left : Nat
  top : Nat
  width : Nat
  height : Nat
  cells : Array (Array Int)
  deriving Lean.FromJson

private def shapeCode (tile : Tile) : Int :=
  if tile.shape == .halfBlock then 5 else (slopeId tile.shape).toNat

private def tileCode (tile : Tile) : Int :=
  (tile.id.map fun id => (id.toNat : Int)).getD (-1)

def FramePatch.changedCells (patch : FramePatch) (terrain : Terrain) : Array (Nat × Nat) := Id.run do
  let mut changes := #[]
  for column in [:patch.width] do
    for row in [:patch.height] do
      let x := patch.left + column
      let y := patch.top + row
      let tile := (terrain.tile? x y).getD default
      let some capture := patch.cells[column * patch.height + row]? | continue
      unless capture[0]! == tileCode tile && capture[1]! == shapeCode tile &&
          capture[2]! == (tile.wall.toNat : Int) do
        changes := changes.push (x, y)
  return changes

def FramePatch.read (path : System.FilePath) (terrain : Terrain) (allowTerrainEdits := false) :
    IO FramePatch := do
  let raw ← IO.FS.readFile path
  let patch : FramePatch ← IO.ofExcept ((Lean.Json.parse raw >>= Lean.fromJson?) |>.mapError IO.userError)
  if patch.worldId != terrain.header.id.toNat || patch.width == 0 || patch.height == 0 ||
      patch.left + patch.width > terrain.header.width || patch.top + patch.height > terrain.header.height ||
      patch.cells.size != patch.width * patch.height || patch.cells.any (·.size != 7) then
    throw (IO.userError "Sprite frame patch does not match the Terraria world.")
  unless allowTerrainEdits do
    if let some (x, y) := (patch.changedCells terrain)[0]? then
      throw (IO.userError s!"Sprite frame patch differs from the world at tile ({x}, {y}).")
  return patch

private def FramePatch.cell (patch : FramePatch) (x y : Nat) : Option (Array Int) := do
  guard (patch.left ≤ x && x < patch.left + patch.width &&
    patch.top ≤ y && y < patch.top + patch.height)
  patch.cells[(x - patch.left) * patch.height + (y - patch.top)]?

def FramePatch.foreground (patch : FramePatch) (x y : Nat) (tile : Tile) : Option (Nat × Nat) := do
  let found ← patch.cell x y
  guard (tile.id.isSome && found[0]! == tileCode tile && found[1]! == shapeCode tile)
  guard (found[3]! ≥ 0 && found[4]! ≥ 0)
  return (found[3]!.toNat, found[4]!.toNat)

def FramePatch.wall (patch : FramePatch) (x y : Nat) (tile : Tile) : Option (Nat × Nat) := do
  let found ← patch.cell x y
  guard (tile.wall != 0 && found[2]! == (tile.wall.toNat : Int))
  guard (found[5]! ≥ 0 && found[6]! ≥ 0)
  return (found[5]!.toNat, found[6]!.toNat)

/-- Keep the captured variants for unchanged wall types. Adjacency is recalculated on draw. -/
def FramePatch.wallFrames (patch : FramePatch) (terrain : Terrain) : OriginalWallWork.WallFrames := Id.run do
  let mut out : OriginalWallWork.WallFrames := {}
  for x in [patch.left:patch.left + patch.width] do
    for y in [patch.top:patch.top + patch.height] do
      let some tile := terrain.tile? x y | continue
      let some (frameX, frameY) := patch.wall x y tile | continue
      let some number := OriginalWallWork.numberOfFrame frameX frameY | continue
      out := out.insert (x, y) ⟨frameX, frameY, number⟩
  return out

private def neighbor (terrain : Terrain) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (terrain.tile? x.toNat y.toNat).getD default

private def connectedType (tile other : Tile) : Option UInt16 :=
  if tile.invisibleBlock == other.invisibleBlock then other.id else none

private def isPlatform (id : Option UInt16) : Bool :=
  id.any fun id => (OriginalTileRules.collision id).platform

/-- Ground atlas input after the same-type slope and half-block adjustments.
The mixed lookup covers dirt, stone, and grass. The other sampled atlases
provide their same-material frames. -/
def terrainFrame (terrain : Terrain) (x y : Nat) (tile : Tile) (variant : Option Nat := none) :
    Option (Nat × Nat) := do
  let id ← tile.id
  let x₀ : Int := x
  let y₀ : Int := y
  let atOffset (dx dy : Int) := neighbor terrain (x₀ + dx) (y₀ + dy)
  let t := atOffset 0 (-1)
  let b := atOffset 0 1
  let l := atOffset (-1) 0
  let r := atOffset 1 0
  let slope := slopeId tile.shape
  let st := slopeId t.shape
  let sb := slopeId b.shape
  let sl := slopeId l.shape
  let sr := slopeId r.shape
  let half := tile.shape == .halfBlock
  let top := if slope == 1 || slope == 2 || st == 3 || st == 4 || half then none
    else connectedType tile t
  let bottom := if slope == 3 || slope == 4 || sb == 1 || sb == 2 || b.shape == .halfBlock then none
    else connectedType tile b
  let top := if top.isSome && !isPlatform top &&
      (slope == 3 || slope == 4 || st == 1 || st == 2 || t.shape == .halfBlock) then some id else top
  let bottom := if bottom.isSome && !isPlatform bottom &&
      (slope == 1 || slope == 2 || sb == 3 || sb == 4) then some id else bottom
  let side (other : Tile) (cut : Bool) : Option UInt16 :=
    if cut then none
    else
      let adjacent := connectedType tile other
      if adjacent.isNone then none
      else if half && other.shape == .halfBlock then some id
      else if (half || other.shape == .halfBlock) && adjacent != some id then none
      else adjacent
  let left := side l (sl == 1 || sl == 3 || slope == 2 || slope == 4)
  let right := side r (sr == 2 || sr == 4 || slope == 1 || slope == 3)
  let adjacent := #[top, bottom, left, right, connectedType tile (atOffset (-1) 1),
    connectedType tile (atOffset 1 1), connectedType tile (atOffset (-1) (-1)), connectedType tile (atOffset 1 (-1))]
  OriginalGroundAtlas.frame id adjacent (variant.getD ((x * 17 + y * 31) % 3))

end Lean4Terria.OriginalFraming
