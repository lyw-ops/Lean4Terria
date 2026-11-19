import Lean4Terria.Hardmode

namespace Lean4Terria

def Tile.hardmodeBiome : Tile → Option HardmodeBiome
  | .hallowedGrass | .pearlstone | .pearlsand | .pinkIce => some .hallow
  | .corruptGrass | .ebonstone | .ebonsand | .purpleIce => some .corruption
  | .crimsonGrass | .crimstone | .crimsand | .redIce | .viciousMushroom => some .crimson
  | _ => none

def World.hardmodeBiomeCount (world : World) (center : TilePos) (biome : HardmodeBiome)
    (horizontal vertical : Nat) : Nat := Id.run do
  let left := max 0 (center.x - horizontal)
  let right := min (world.width : Int) (center.x + horizontal + 1)
  let top := max 0 (center.y - vertical)
  let bottom := min (world.height : Int) (center.y + vertical + 1)
  let mut count := 0
  for col in [left.toNat:right.toNat] do
    for row in [top.toNat:bottom.toNat] do
      if (world.tile ⟨col, row⟩).hardmodeBiome == some biome then count := count + 1
  return count

def World.hardmodeZones (world : World) (center : TilePos) : Bool × Bool × Bool :=
  let hallow := world.hardmodeBiomeCount center .hallow 32 22
  let corrupt := world.hardmodeBiomeCount center .corruption 32 22
  let crimson := world.hardmodeBiomeCount center .crimson 32 22
  (hallow - (corrupt + crimson) ≥ 125, corrupt - hallow ≥ 300, crimson - hallow ≥ 300)

def Tile.isHallowCrystal (tile : Tile) : Bool :=
  tile == .crystalShard || tile == .gelatinCrystal

/-- Tile 129 checks floor, left wall, right wall and ceiling, in that order. -/
def World.crystalShardFrame (world : World) (pos : TilePos) : Option Nat :=
  let support (dx dy : Int) :=
    (world.tile? ⟨pos.x + dx, pos.y + dy⟩).any fun tile => tile.solid && !tile.isDoor
  if pos.x < 2 || pos.y < 2 || pos.x ≥ (world.width : Int) - 2 ||
      pos.y ≥ (world.height : Int) - 2 then none
  else if support 0 1 then some 0
  else if support (-1) 0 then some 2
  else if support 1 0 then some 3
  else if support 0 (-1) then some 1
  else none

def World.crystalShardSupport (world : World) (pos : TilePos) : Bool :=
  (world.crystalShardFrame pos).isSome

def growCrystalShardFrom (world : World) (source : TilePos) (random : UnifiedRandom) :
    World × UnifiedRandom := Id.run do
  let tile := world.tile source
  if source.y ≤ world.height / 2 || !(tile == .pearlstone || tile == .pinkIce || tile == .pearlsand) then
    return (world, random)
  let (grow, random) := random.next 0 5
  if grow != 0 then return (world, random)
  let (side, random) := random.next 0 4
  let target : TilePos := if side == 0 then ⟨source.x - 1, source.y⟩
    else if side == 1 then ⟨source.x + 1, source.y⟩ else ⟨source.x, source.y + 1⟩
  if world.tile target != .air then return (world, random)
  let mut count := 0
  for x in [:13] do
    for y in [:13] do
      if (world.tile ⟨source.x + x - 6, source.y + y - 6⟩).isHallowCrystal then
        count := count + 1
        if count ≥ 2 then return (world, random)
  let (_, random) := random.next 0 18
  let (rare, random) := random.next 0 50
  let random := if rare == 0 then (random.next 0 6).2 else random
  if !world.crystalShardSupport target then return (world, random)
  return (world.setTile target (if rare == 0 then .gelatinCrystal else .crystalShard), random)

private def hardmodeSpreadTile (world : World) (position : TilePos) (biome : HardmodeBiome) : Tile :=
  let tile := world.tile position
  if tile == .stone || tile == .sand || tile == .ice then biome.convertTile tile
  else if tile == .dirt && world.clearSkyAbove position then
    let cover := world.tile ⟨position.x, position.y - 1⟩
    if cover == .sunleaf || cover == .sunleafSprout then tile else biome.convertTile tile true
  else tile

/-- Classic hardUpdateWorld repeats a successful infection with probability
one half. A failed target ends the chain. All samples stay centered on the
original source, so there are at most 49 successes. -/
def spreadHardmodeFrom (initial : World) (source : TilePos) (random : UnifiedRandom) :
    World × UnifiedRandom := Id.run do
  let some biome := (initial.tile source).hardmodeBiome | return (initial, random)
  let mut world := initial
  let mut random := random
  for _ in [:49] do
    let (dx, next) := random.next (-3) 4
    let (dy, next) := next.next (-3) 4
    random := next
    let target : TilePos := ⟨source.x + dx, source.y + dy⟩
    if target.x < 10 || target.y < 10 || target.x ≥ (world.width : Int) - 10 ||
        target.y ≥ (world.height : Int) - 10 then break
    let tile := hardmodeSpreadTile world target biome
    if tile == world.tile target then break
    let (keepGoing, next) := random.next 0 2
    random := next
    -- This compact world's flat tiles use the same 3×3 Convert footprint.
    for x in [:3] do
      for y in [:3] do
        let pos : TilePos := ⟨target.x + x - 1, target.y + y - 1⟩
        world := world.setTile pos (hardmodeSpreadTile world pos biome)
    if keepGoing != 0 then break
  return (world, random)

/-- Compact worlds retain the per-tile native sampling rates. The engine's
clock supplies separate deterministic random streams for successive updates. -/
def advanceHardmodeSpread (initial : World) (seed : UInt64) (clock : Nat) : World := Id.run do
  let mut world := initial
  let mut random := UnifiedRandom.fromEntropy (hash (seed, clock, (4954 : Nat)))
  let surface := groundHeight seed 19
  let area := world.width * world.height
  for (count, top, bottom) in
      [(max 1 ((area * 3 + 99999) / 100000), 10, surface - 1),
        (max 1 ((area * 15 + 999999) / 1000000), surface - 1, world.height - 20)] do
    for _ in [:count] do
      let (x, next) := random.next 10 (world.width - 10 : Nat)
      let (y, next) := next.next top bottom
      let (worldWithCrystal, next) := growCrystalShardFrom world ⟨x, y⟩ next
      let (updated, next) := spreadHardmodeFrom worldWithCrystal ⟨x, y⟩ next
      world := updated
      random := next
  return world

end Lean4Terria
