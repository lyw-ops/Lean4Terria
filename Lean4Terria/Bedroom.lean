import Lean4Terria.Inventory
import Std.Data.HashSet.Basic

namespace Lean4Terria

inductive FurnitureKind where
  | bed | furnace | ironAnvil | sawmill | loom | leadAnvil | woodChair | woodTable | hellforge
  | mythrilAnvil | orichalcumAnvil | adamantiteForge | titaniumForge
  | woodenCrate | ironCrate | goldenCrate | pearlwoodCrate | mythrilCrate | titaniumCrate
  | biomeCrate (item : Item)
  deriving BEq, Repr, Inhabited

def FurnitureKind.width : FurnitureKind → Nat
  | .bed => 4
  | .woodChair => 1
  | .ironAnvil | .leadAnvil | .mythrilAnvil | .orichalcumAnvil => 2
  | .woodenCrate | .ironCrate | .goldenCrate | .pearlwoodCrate | .mythrilCrate | .titaniumCrate | .biomeCrate _ => 2
  | .furnace | .hellforge | .adamantiteForge | .titaniumForge | .sawmill | .loom | .woodTable => 3

def FurnitureKind.height : FurnitureKind → Nat
  | .ironAnvil | .leadAnvil | .mythrilAnvil | .orichalcumAnvil => 1
  | .sawmill => 3
  | .woodenCrate | .ironCrate | .goldenCrate | .pearlwoodCrate | .mythrilCrate | .titaniumCrate | .biomeCrate _ => 2
  | .bed | .furnace | .hellforge | .adamantiteForge | .titaniumForge | .loom | .woodChair | .woodTable => 2

def FurnitureKind.tile : FurnitureKind → Tile
  | .bed => .bed
  | .furnace => .furnace
  | .hellforge => .hellforge
  | .ironAnvil => .ironAnvil
  | .leadAnvil => .leadAnvil
  | .mythrilAnvil => .mythrilAnvil
  | .orichalcumAnvil => .orichalcumAnvil
  | .adamantiteForge => .adamantiteForge
  | .titaniumForge => .titaniumForge
  | .woodChair => .woodChair
  | .woodTable => .woodTable
  | .sawmill => .sawmill
  | .loom => .loom
  | .woodenCrate | .ironCrate | .goldenCrate | .pearlwoodCrate | .mythrilCrate | .titaniumCrate | .biomeCrate _ => .fishingCrate

structure Furniture where
  kind : FurnitureKind
  /-- The bottom-left tile of the furniture. -/
  position : TilePos
  /-- A mirrored bed has its pillow on the left. -/
  flipped : Bool := false
  deriving BEq, Repr, Inhabited

def Furniture.tile (furniture : Furniture) : Tile := furniture.kind.tile

def Furniture.cells (furniture : Furniture) : Array TilePos := Id.run do
  let mut cells := #[]
  for row in [:furniture.kind.height] do
    for column in [:furniture.kind.width] do
      cells := cells.push ⟨furniture.position.x + column, furniture.position.y - row⟩
  return cells

def Furniture.supports (furniture : Furniture) : Array TilePos :=
  (Array.range furniture.kind.width).map fun (column : Nat) =>
    ⟨furniture.position.x + column, furniture.position.y + 1⟩

private def Furniture.hasSupport (furniture : Furniture) (world : World) : Bool :=
  furniture.supports.all fun position =>
    match world.tile? position with
    | some tile => (tile.solid && !tile.isDoor) || tile == .platform ||
      (furniture.tile == .fishingCrate && tile == .fishingCrate)
    | none => false

def Furniture.canPlace (furniture : Furniture) (world : World) : Bool :=
  furniture.hasSupport world &&
    furniture.cells.all fun position => world.tile? position == some .air

def Furniture.intact (furniture : Furniture) (world : World) : Bool :=
  furniture.hasSupport world &&
    furniture.cells.all fun position => world.tile? position == some furniture.tile

/-- Add a Hellforge to the lower floor of each intact house with room for it.
For old worlds, the caller supplies the safe cells for migration. -/
def installHellforges (seed : UInt64) (initial : World) (placed : Array Furniture)
    (allowed : TilePos → Bool) : World × Array Furniture := Id.run do
  let layout := underworldLayout seed
  let mut world := initial
  let mut furniture := placed
  for tower in #[layout.westTower, layout.eastTower] ++ layout.extraTowers do
    if world.tile ⟨tower.left, tower.floor + 1⟩ != .obsidianBrick then continue
    for column in [tower.left + 5:tower.right - 2] do
      let forge : Furniture := { kind := .hellforge, position := ⟨column, tower.floor - 1⟩ }
      if forge.canPlace world && forge.cells.all allowed &&
          forge.supports.all (fun position => world.tile position == .obsidianBrick) then
        world := forge.cells.foldl (fun terrain cell => terrain.setTile cell .hellforge) world
        furniture := furniture.push forge
        break
  return (world, furniture)

def Furniture.isBedFoot (furniture : Furniture) (position : TilePos) : Bool :=
  furniture.kind == .bed && furniture.cells.contains position &&
    ((position.x < furniture.position.x + 2) != furniture.flipped)

def Furniture.isBedHead (furniture : Furniture) (position : TilePos) : Bool :=
  furniture.kind == .bed && furniture.cells.contains position &&
    !furniture.isBedFoot position

/-- Only placed wood walls have a housing-safe background wall code. -/
def hasWoodWall (world : World) (walls : ByteArray) (position : TilePos) : Bool :=
  world.contains position && walls[position.y.toNat * world.width + position.x.toNat]? == some 1

def roomBoundary (world : World) (position : TilePos) : Bool :=
  match world.tile position with
  | .platform => true
  | .workbench | .chest => false
  | tile => if tile.isDoor then (doorBase world position).isSome else tile.solid

-- In each axis the room check accepts a safe wall or frame tile within two tiles.
private def roomBacked (world : World) (walls : ByteArray) (position : TilePos) : Bool :=
  let backed := fun cell => hasWoodWall world walls cell || roomBoundary world cell
  let offsets := ([-2, -1, 0, 1, 2] : List Int)
  offsets.any (fun offset => backed { position with x := position.x + offset }) &&
    offsets.any (fun offset => backed { position with y := position.y + offset })

private def bedroomInBounds (world : World) (position : TilePos) : Bool :=
  10 ≤ position.x && position.x < (world.width : Int) - 10 &&
    10 ≤ position.y && position.y < (world.height : Int) - 10

-- Count the enclosing tiles too. Diagonal gaps let the scan escape the room.
def scanBackedRoom (world : World) (walls : ByteArray) (start : TilePos) :
    Except String (Array TilePos) := do
  if roomBoundary world start then throw "Choose a space inside the room."
  let mut visited : Std.HashSet TilePos := ({} : Std.HashSet TilePos).insert start
  let mut cells := #[start]
  let mut left := start.x
  let mut right := start.x
  let mut top := start.y
  let mut bottom := start.y
  let mut cursor := 0
  while cursor < cells.size do
    let position := cells[cursor]!
    if !bedroomInBounds world position then throw "Enclose the room away from the edge of the world."
    if cells.size >= 750 then throw "The room must have fewer than 750 tiles including its frame."
    left := min left position.x
    right := max right position.x
    top := min top position.y
    bottom := max bottom position.y
    if right - left >= 100 || bottom - top >= 100 then throw "The room cannot exceed 100 tiles in either direction."
    cursor := cursor + 1
    if roomBoundary world position then continue
    if !roomBacked world walls position then throw "Close the frame and fill gaps with background walls."
    for dx in ([-1, 0, 1] : List Int) do
      for dy in ([-1, 0, 1] : List Int) do
        let next : TilePos := ⟨position.x + dx, position.y + dy⟩
        if !visited.contains next then
          visited := visited.insert next
          cells := cells.push next
  if cells.size < 60 then throw "The room needs at least 60 tiles including its frame."
  return cells

/-- Validate the bed, its three-tile spawn clearance, and its bedroom. -/
def bedSpawn (world : World) (walls : ByteArray) (bed : Furniture) : Option Player := do
  if bed.kind != .bed || !bed.intact world || walls.size != world.width * world.height then
    none
  else
    let centerX := bed.position.x + (if bed.flipped then 1 else 2)
    let floorY := bed.position.y + 1
    let clear := ([-1, 0, 1] : List Int).all fun dx =>
      ([-3, -2, -1] : List Int).all fun dy =>
        !(world.tile ⟨centerX + dx, floorY + dy⟩).solid
    if !clear || !(scanBackedRoom world walls ⟨centerX, bed.position.y⟩).isOk then
      none
    else
      some ⟨centerX * tileUnits + (tileUnits - playerWidth) / 2,
        floorY * tileUnits - playerHeight, 0, 0, true⟩

end Lean4Terria
