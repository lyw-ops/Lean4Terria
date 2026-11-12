import Init

namespace Lean4Terria

structure TilePos where
  x : Int
  y : Int
  deriving BEq, Repr, Inhabited

inductive Tile where
  | air | dirt | stone | copper | crystal | workbench | bedrock | timber | torch
  deriving BEq, Repr, Inhabited

/-- Stable tile identifiers used in the world and save data. -/
def Tile.code : Tile → UInt8
  | .air => 0
  | .dirt => 1
  | .stone => 2
  | .copper => 3
  | .crystal => 4
  | .workbench => 5
  | .bedrock => 6
  | .timber => 7
  | .torch => 8

def Tile.ofCode? : UInt8 → Option Tile
  | 0 => some .air
  | 1 => some .dirt
  | 2 => some .stone
  | 3 => some .copper
  | 4 => some .crystal
  | 5 => some .workbench
  | 6 => some .bedrock
  | 7 => some .timber
  | 8 => some .torch
  | _ => none

def Tile.solid (tile : Tile) : Bool := tile != .air && tile != .torch

structure World where
  width : Nat
  height : Nat
  tiles : ByteArray
  deriving BEq, Inhabited

def World.contains (world : World) (pos : TilePos) : Bool :=
  0 ≤ pos.x && pos.x < (world.width : Int) &&
    0 ≤ pos.y && pos.y < (world.height : Int)

private def World.index (world : World) (pos : TilePos) : Nat :=
  pos.y.toNat * world.width + pos.x.toNat

def World.tile? (world : World) (pos : TilePos) : Option Tile := do
  if world.contains pos then
    let code ← world.tiles[world.index pos]?
    Tile.ofCode? code
  else
    none

/-- The edge of the finite map behaves as solid bedrock. -/
def World.tile (world : World) (pos : TilePos) : Tile :=
  (world.tile? pos).getD .bedrock

def World.setTile (world : World) (pos : TilePos) (tile : Tile) : World :=
  let index := world.index pos
  if world.contains pos && index < world.tiles.size then
    { world with tiles := world.tiles.set! index tile.code }
  else
    world

def defaultSeed : UInt64 := 42
def worldWidth : Nat := 256
def worldHeight : Nat := 128

/-- SplitMix64's mixing function gives deterministic samples at each grid point. -/
private def sample (seed : UInt64) (x y : Nat) : Nat :=
  let value := seed + x.toUInt64 * 0x9e3779b97f4a7c15 + y.toUInt64 * 0xd1b54a32d192ed03
  let value := (value ^^^ (value >>> 30)) * 0xbf58476d1ce4e5b9
  let value := (value ^^^ (value >>> 27)) * 0x94d049bb133111eb
  ((value ^^^ (value >>> 31)) &&& 255).toNat

private def blend (left right offset span : Nat) : Nat :=
  (left * (span - offset) + right * offset) / span

private def noise (seed : UInt64) (x y spanX spanY : Nat) : Nat :=
  let col := x / spanX
  let row := y / spanY
  let offset := x % spanX
  let upper := blend (sample seed col row) (sample seed (col + 1) row) offset spanX
  let lower := blend (sample seed col (row + 1)) (sample seed (col + 1) (row + 1)) offset spanX
  blend upper lower (y % spanY) spanY

def groundHeight (seed : UInt64) (x : Nat) : Nat :=
  -- Gradually blend the level spawn shelf into the surrounding hills.
  let distance := max (9 - x) (x - 30)
  let hill := 32 + noise seed x 0 18 1 / 16
  blend 40 hill (min distance 12) 12

private def caveFloor (x : Nat) : Nat :=
  40 + 2 * min ((x - 24) / 7) 6

private def terrainTile (seed : UInt64) (x y width height : Nat) : Tile :=
  let ground := groundHeight seed x
  let floor := caveFloor x
  if x == 0 || x + 1 == width || y + 2 ≥ height then .bedrock
  else if x == 16 && y == 39 then .workbench
  else if 35 ≤ x && x ≤ 37 && y == 42 then .copper
  else if x == 64 && 47 ≤ y && y ≤ 49 then .crystal
  -- The opening leads down through two-tile terraces that are also jumpable on return.
  else if 24 ≤ x && x ≤ 75 && floor - 6 ≤ y && y < floor then .air
  else if 24 ≤ x && x ≤ 75 && (y == floor || y == floor + 1) then .stone
  else if (x == 14 || (x ≥ 80 && sample seed x 0 < 20)) && ground - 2 ≤ y && y < ground then
    .timber
  else if y < ground then .air
  else if y < ground + 4 then .dirt
  else if y ≥ ground + 6 && noise (seed + 1) x y 15 11 < 88 then .air
  else if y ≥ 53 && noise (seed + 2) x y 6 6 < 53 then .copper
  else if y ≥ 70 && noise (seed + 3) x y 5 5 < 44 then .crystal
  else .stone

def generateTerrain (seed : UInt64) : World := Id.run do
  let mut tiles := ByteArray.emptyWithCapacity (worldWidth * worldHeight)
  for y in [:worldHeight] do
    for x in [:worldWidth] do
      tiles := tiles.push (terrainTile seed x y worldWidth worldHeight).code
  return { width := worldWidth, height := worldHeight, tiles }

end Lean4Terria
