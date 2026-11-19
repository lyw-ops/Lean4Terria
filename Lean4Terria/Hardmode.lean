import Lean4Terria.UnifiedRandom
import Lean4Terria.World

namespace Lean4Terria

inductive HardmodeBiome where
  | hallow | corruption | crimson
  deriving BEq, Repr, Inhabited

/-- Terraria's `GERunner` tile substitutions for materials in this world.
`grass` denotes the surface grass represented by an exposed Dirt tile. -/
def HardmodeBiome.convertTile (biome : HardmodeBiome) (tile : Tile) (grass := false) : Tile :=
  match biome, tile with
  | .hallow, .dirt => if grass then .hallowedGrass else tile
  | .hallow, .corruptGrass | .hallow, .crimsonGrass => .hallowedGrass
  | .hallow, .stone | .hallow, .ebonstone | .hallow, .crimstone => .pearlstone
  | .hallow, .sand | .hallow, .ebonsand | .hallow, .crimsand => .pearlsand
  | .hallow, .ice | .hallow, .purpleIce | .hallow, .redIce => .pinkIce
  | .corruption, .dirt => if grass then .corruptGrass else tile
  | .corruption, .hallowedGrass => .corruptGrass
  | .corruption, .stone | .corruption, .pearlstone => .ebonstone
  | .corruption, .sand | .corruption, .pearlsand => .ebonsand
  | .corruption, .ice | .corruption, .pinkIce => .purpleIce
  | .crimson, .dirt => if grass then .crimsonGrass else tile
  | .crimson, .hallowedGrass => .crimsonGrass
  | .crimson, .stone | .crimson, .pearlstone => .crimstone
  | .crimson, .sand | .crimson, .pearlsand => .crimsand
  | .crimson, .ice | .crimson, .pinkIce => .redIce
  | _, _ => tile

private structure HardmodeStripes where
  hallow : Nat
  evil : Nat
  eastward : Bool

/-- `initializeHardMode` chooses a side, then moves whichever stripe shares
the Dungeon side toward that edge. Its two percentages are separate draws. -/
private def planStripes (width : Nat) (dungeon : Int) (rng : UnifiedRandom) :
    HardmodeStripes × UnifiedRandom :=
  let (inner, rng) := rng.next 300 400
  let (outer, rng) := rng.next 200 300
  let (side, rng) := rng.next 0 2
  let inner := inner.toFloat * 0.001
  let outer := outer.toFloat * 0.001
  let column := fun fraction : Float => (width.toFloat * fraction).toUInt64.toNat
  let west := column (if dungeon < (width / 2 : Nat) then outer else inner)
  let east := column (1 - (if dungeon < (width / 2 : Nat) then inner else outer))
  if side == 0 then (⟨east, west, false⟩, rng) else (⟨west, east, true⟩, rng)

/-- Surface origins in conversion order: Hallow followed by the world's evil. -/
def hardmodeStripeStarts (world : World) (seed entropy : UInt64) : Nat × Nat :=
  let (stripes, _) := planStripes world.width (dungeonLayout seed).oldMan.x (UnifiedRandom.fromEntropy entropy)
  (stripes.hallow, stripes.evil)

private def exposedGrass (world : World) (pos : TilePos) : Bool :=
  let cover := world.tile { pos with y := pos.y - 1 }
  cover != .sunleaf && cover != .sunleafSprout && world.clearSkyAbove pos

/-- The native runner's width draw is scaled by `width / 4200`. Running it
in compact tile coordinates preserves both percentage origins and the 3:5
initial slope. Jitter uses the same x-major traversal and RNG draw order. -/
private def runStripe (world : World) (start : Nat) (velocity : Float)
    (biome : HardmodeBiome) (rng : UnifiedRandom) : World × UnifiedRandom := Id.run do
  let (nativeWidth, rng) := rng.next 200 250
  let diameter := (nativeWidth.toFloat * (world.width.toFloat / 4200)).toUInt64.toNat
  let (unusedX, rng) := rng.next (-10) 11
  let (_, rng) := rng.next (-10) 11
  let _ := unusedX -- Terraria draws both default velocities before overriding them.
  let mut rng := rng
  let mut result := world
  let half := diameter.toFloat * 0.5
  let mut x := start.toFloat
  let mut y : Nat := 0
  let mut vx := velocity
  for _ in [:world.height + diameter + 1] do
    let left := (x - half).toInt64.toInt.toNat
    let right := min world.width (x + half).toInt64.toInt.toNat
    let top := (y.toFloat - half).toInt64.toInt.toNat
    let bottom := min (world.height - 5) (y.toFloat + half).toInt64.toInt.toNat
    for col in [left:right] do
      for row in [top:bottom] do
        let (jitter, next) := rng.next (-10) 11
        rng := next
        let distance := (col.toFloat - x).abs + (row.toFloat - y.toFloat).abs
        if distance < half * (1 + jitter.toFloat * 0.015) then
          let pos : TilePos := ⟨col, row⟩
          let old := result.tile pos
          let tile := biome.convertTile old (old == .dirt && exposedGrass world pos)
          if tile != old then result := result.setTile pos tile
    x := x + vx
    y := y + 5
    let (bend, next) := rng.next (-10) 11
    rng := next
    vx := min (velocity + 1) (max (velocity - 1) (vx + bend.toFloat * 0.05))
    if x < -diameter.toFloat || x > (world.width + diameter).toFloat || y > world.height + diameter then break
  return (result, rng)

/-- Apply the first Hardmode conversion to the current world, using entropy
from the transition rather than the Wall of Flesh's defeat coordinates. -/
def convertHardmodeTerrain (world : World) (seed entropy : UInt64) : World :=
  let (stripes, rng) := planStripes world.width (dungeonLayout seed).oldMan.x (UnifiedRandom.fromEntropy entropy)
  let velocity : Float := if stripes.eastward then 3 else -3
  let evil := if altarTile seed == .crimsonAltar then HardmodeBiome.crimson else .corruption
  let (world, rng) := runStripe world stripes.hallow velocity .hallow rng
  (runStripe world stripes.evil (-velocity) evil rng).1

end Lean4Terria
