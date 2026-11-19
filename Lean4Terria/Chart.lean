import Lean4Terria.World

namespace Lean4Terria

structure Chart where
  /-- Zero is unknown. Every other byte is the last seen tile code plus one. -/
  cells : ByteArray
  deriving BEq, Inhabited

namespace Chart

private def revealRadius : Nat := 8

private def indexAt (world : World) (position : TilePos) : Nat :=
  position.y.toNat * world.width + position.x.toNat

def empty (world : World) : Chart :=
  { cells := ByteArray.mk (Array.replicate (world.width * world.height) 0) }

def tile? (chart : Chart) (world : World) (position : TilePos) : Option Tile := do
  if !world.contains position then none else
    let code ← chart.cells[indexAt world position]?
    if code == 0 then none else Tile.ofCode? (code - 1)

private def neighbors (position : TilePos) : List TilePos :=
  [⟨position.x - 1, position.y⟩, ⟨position.x + 1, position.y⟩,
    ⟨position.x, position.y - 1⟩, ⟨position.x, position.y + 1⟩]

/-- Observe cells up to eight orthogonal steps away. Solid tiles are visible
at the boundary and stop the reveal. Previously discovered cells keep their
last observed tile until seen again. -/
def reveal (chart : Chart) (world : World) (center : TilePos) : Chart := Id.run do
  if !world.contains center then return chart
  let side := 2 * revealRadius + 1
  let left := center.x - revealRadius
  let top := center.y - revealRadius
  let localIndex (position : TilePos) : Nat :=
    (position.y - top).toNat * side + (position.x - left).toNat
  let mut cells := if chart.cells.size == world.width * world.height then chart.cells
    else (empty world).cells
  let mut seen := ByteArray.mk (Array.replicate (side * side) 0)
  seen := seen.set! (localIndex center) 1
  let mut pending : Array (TilePos × Nat) := #[(center, 0)]
  let mut cursor := 0
  while cursor < pending.size do
    let (position, distance) := pending[cursor]!
    cursor := cursor + 1
    let some tile := world.tile? position | continue
    let index := indexAt world position
    let observed := tile.code + 1
    if cells[index]! != observed then cells := cells.set! index observed
    if tile.solid || distance == revealRadius then continue
    for neighbor in neighbors position do
      if !world.contains neighbor then continue
      let index := localIndex neighbor
      if seen[index]! == 0 then
        seen := seen.set! index 1
        pending := pending.push (neighbor, distance + 1)
  return { cells }

end Chart

/-- Full-screen map camera in tile coordinates. Keep the view out of the save file. -/
structure MapCamera where
  x : Float
  y : Float
  scale : Float := 4
  deriving Inhabited

namespace MapCamera

def atTile (position : TilePos) (scale : Float := 4) : MapCamera :=
  ⟨position.x.toFloat + 0.5, position.y.toFloat + 0.5, scale⟩

def pan (camera : MapCamera) (world : World) (dx dy : Float) : MapCamera :=
  { camera with
    x := max 0.5 (min (world.width.toFloat - 0.5) (camera.x + dx / camera.scale))
    y := max 0.5 (min (world.height.toFloat - 0.5) (camera.y + dy / camera.scale)) }

/-- Keep the tile under the pointer fixed while zooming, except at the world edges. -/
def zoom (camera : MapCamera) (world : World) (factor minScale offsetX offsetY : Float) : MapCamera :=
  let scale := max minScale (min 31.18 (camera.scale * factor))
  let adjustment := scale / camera.scale - 1
  { camera with scale } |>.pan world (offsetX * adjustment) (offsetY * adjustment)

def tileAt (camera : MapCamera) (offsetX offsetY : Int) : TilePos :=
  ⟨(camera.x + offsetX.toFloat / camera.scale).floor.toInt64.toInt,
   (camera.y + offsetY.toFloat / camera.scale).floor.toInt64.toInt⟩

end MapCamera

end Lean4Terria
