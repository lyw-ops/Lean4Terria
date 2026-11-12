import Lean4Terria.World

namespace Lean4Terria

/-!
Tile lighting for the visible part of the map.

Light levels run from 0 (dark) to 15 (full daylight) and drop by one per
orthogonal step, so light flows around corners but never through walls:
solid tiles receive light from a neighbour but do not pass it on.

Each frame works on a small window, the viewport clipped to the map and
widened by the maximum light reach. Any source that can brighten a visible tile
lies inside it, so torches just off-screen still shine in. Propagation is a
breadth-first flood processed brightest-first (one bucket per level), which
visits every window cell at most once per level it holds and needs no sorting.
-/

namespace Lighting

def maxLevel : Nat := 15
def skyLevel : Nat := 15
def placedTorchLevel : Nat := 15
def heldTorchLevel : Nat := 12
def playerAmbientLevel : Nat := 6

/-- Light given off by a tile itself. -/
def emission : Tile → Nat
  | .torch => placedTorchLevel
  | _ => 0

end Lighting

/-- Light levels for the viewport of one frame. -/
structure LightField where
  /-- Map tile stored at index 0 of `levels`. -/
  originX : Int
  originY : Int
  /-- Row length of `levels`. -/
  stride : Nat
  levels : ByteArray
  /-- Half-open bounds of the queryable area: the viewport clipped to the map. -/
  minX : Int
  minY : Int
  maxX : Int
  maxY : Int
  deriving Inhabited

/-- Light level 0..15 at a tile; 0 outside the computed viewport. -/
def LightField.level (field : LightField) (pos : TilePos) : Nat :=
  if field.minX ≤ pos.x && pos.x < field.maxX && field.minY ≤ pos.y && pos.y < field.maxY then
    let index := (pos.y - field.originY).toNat * field.stride + (pos.x - field.originX).toNat
    (field.levels[index]?.getD 0).toNat
  else
    0

namespace Lighting

/-- Working state of the flood: current levels plus one queue per light level. -/
private structure Flood where
  levels : ByteArray
  buckets : Array (Array Nat)

/-- Brighten a cell to `level` if that raises it; queue it to spread when `spreads`. -/
@[inline] private def Flood.raise (flood : Flood) (index level : Nat) (spreads : Bool) : Flood :=
  match flood with
  | ⟨levels, buckets⟩ =>
    if (levels[index]?.getD 255).toNat < level then
      let levels := levels.set! index level.toUInt8
      let buckets := if spreads then buckets.modify level (·.push index) else buckets
      ⟨levels, buckets⟩
    else
      ⟨levels, buckets⟩

end Lighting

open Lighting in
/--
Compute light for a frame.

* `viewOrigin`, `viewWidth`, `viewHeight`: first tile and size of the viewport.
* `player`: the player's center tile, which glows faintly, or brightly when
  `holdingTorch`.

Open air above the generated surface (`groundHeight`) is lit as sky, down to the
first solid tile in each column, so shafts dug below the original surface stay
dark and roofs cast shade.
-/
def computeLighting (world : World) (seed : UInt64) (viewOrigin : TilePos)
    (viewWidth viewHeight : Nat) (player : TilePos) (holdingTorch : Bool) : LightField := Id.run do
  let mapWidth : Int := world.width
  let mapHeight : Int := world.height
  let minX := max viewOrigin.x 0
  let minY := max viewOrigin.y 0
  let maxX := min (viewOrigin.x + viewWidth) mapWidth
  let maxY := min (viewOrigin.y + viewHeight) mapHeight
  if maxX ≤ minX || maxY ≤ minY then
    return { originX := 0, originY := 0, stride := 0, levels := .empty,
             minX := 0, minY := 0, maxX := 0, maxY := 0 }
  -- A source further than `maxLevel` steps away cannot reach the viewport.
  let reach : Int := maxLevel
  let originX := max (minX - reach) 0
  let originY := max (minY - reach) 0
  let width := (min (maxX + reach) mapWidth - originX).toNat
  let height := (min (maxY + reach) mapHeight - originY).toNat
  let size := width * height

  -- Read the window once: which cells block light, and which emit it.
  let mut solid := ByteArray.emptyWithCapacity size
  let mut sources : Array (Nat × Nat) := #[]
  for row in [:height] do
    for col in [:width] do
      let tile := world.tile ⟨originX + col, originY + row⟩
      solid := solid.push (if tile.solid then 1 else 0)
      let level := emission tile
      if level > 0 then
        sources := sources.push (row * width + col, level)

  let mut flood : Flood :=
    { levels := ByteArray.mk (Array.replicate size 0),
      buckets := Array.replicate (maxLevel + 1) #[] }
  for (index, level) in sources do
    flood := flood.raise index level true

  -- Sky light falls straight down until the original surface or a solid tile.
  let windowBottom := (originY + height).toNat
  for col in [:width] do
    let x := (originX + col).toNat
    let stop := min (groundHeight seed x) windowBottom
    for y in [:stop] do
      if (world.tile ⟨x, y⟩).solid then break
      if (y : Int) ≥ originY then
        flood := flood.raise ((y - originY).toNat * width + col) skyLevel true

  -- The player always lights their surroundings a little.
  if originX ≤ player.x && player.x < originX + width &&
      originY ≤ player.y && player.y < originY + height then
    let index := (player.y - originY).toNat * width + (player.x - originX).toNat
    if solid[index]! == 0 then
      flood := flood.raise index (if holdingTorch then heldTorchLevel else playerAmbientLevel) true

  -- Spread brightest first; each queued cell lights its four neighbours one level dimmer.
  for step in [:maxLevel - 1] do
    let level := maxLevel - step
    let queued := flood.buckets[level]!
    flood := { flood with buckets := flood.buckets.set! level #[] }
    for index in queued do
      -- Skip entries superseded by a brighter visit, and walls (they receive but do not pass on).
      if flood.levels[index]!.toNat != level || solid[index]! != 0 then continue
      let col := index % width
      let row := index / width
      let dim := level - 1
      if col > 0 then
        flood := flood.raise (index - 1) dim (solid[index - 1]! == 0)
      if col + 1 < width then
        flood := flood.raise (index + 1) dim (solid[index + 1]! == 0)
      if row > 0 then
        flood := flood.raise (index - width) dim (solid[index - width]! == 0)
      if row + 1 < height then
        flood := flood.raise (index + width) dim (solid[index + width]! == 0)

  return { originX, originY, stride := width, levels := flood.levels, minX, minY, maxX, maxY }

end Lean4Terria
