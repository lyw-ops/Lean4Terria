import Lean4Terria.Hardmode

namespace Lean4Terria

def hardmodeConversionEntropy (seed : UInt64) (clock : Nat) : UInt64 :=
  hash (seed, clock, (4578 : Nat))

def hardmodeOreTile (tier mask : Nat) : Tile :=
  let variant := mask / (2 ^ tier) % 2
  match tier with
  | 0 => if variant == 0 then .cobaltOre else .palladiumOre
  | 1 => if variant == 0 then .mythrilOre else .orichalcumOre
  | _ => if variant == 0 then .adamantiteOre else .titaniumOre

def hardmodeOreBlessing : Tile → String
  | .cobaltOre => "Cobalt"
  | .palladiumOre => "Palladium"
  | .mythrilOre => "Mythril"
  | .orichalcumOre => "Orichalcum"
  | .adamantiteOre => "Adamantite"
  | .titaniumOre => "Titanium"
  | _ => ""

private def oreReplaceable : Tile → Bool
  | .dirt | .stone | .ash | .sand | .ice | .corruptGrass | .crimsonGrass | .ebonstone | .crimstone
  | .hallowedGrass | .pearlstone | .pearlsand | .ebonsand | .crimsand | .purpleIce | .pinkIce | .redIce => true
  | _ => false

private def oreInt (n : Float) : Int := n.toInt64.toInt
private def oreFloat (n : Int) : Float := n.toFloat

/-- Classic OreRunner. The square shrinks with the remaining steps, while
the noisy Manhattan radius keeps the original strength. -/
def runHardmodeOreVein (initial : World) (ore : Tile) (origin : TilePos)
    (strength steps : Nat) (random : UnifiedRandom) : World × UnifiedRandom := Id.run do
  let (rollX, random) := random.next (-10) 11
  let (rollY, random) := random.next (-10) 11
  let mut random := random
  let mut x := oreFloat origin.x
  let mut y := oreFloat origin.y
  let mut vx := oreFloat rollX * 0.1
  let vy := oreFloat rollY * 0.1
  let mut world := initial
  for i in [:steps] do
    let width := strength.toFloat * ((steps - i).toFloat / steps.toFloat)
    let left := max 0 (oreInt (x - width * 0.5))
    let right := min (world.width : Int) (oreInt (x + width * 0.5))
    let top := max 0 (oreInt (y - width * 0.5))
    let bottom := min (world.height : Int) (oreInt (y + width * 0.5))
    for ix in [:(right - left).toNat] do
      let px := left + ix
      for iy in [:(bottom - top).toNat] do
        let py := top + iy
        let (jitter, next) := random.next (-10) 11
        random := next
        let distance := (oreFloat px - x).abs + (oreFloat py - y).abs
        if distance < strength.toFloat * 0.5 * (1 + oreFloat jitter * 0.015) then
          let cell : TilePos := ⟨px, py⟩
          let cover := world.tile ⟨px, py - 1⟩
          if oreReplaceable (world.tile cell) && cover != .sunleaf && cover != .sunleafSprout then
            world := world.setTile cell ore
    x := x + vx
    y := y + vy
    let (jitter, next) := random.next (-10) 11
    random := next
    vx := max (-1) (min 1 (vx + oreFloat jitter * 0.05))
  return (world, random)

/-- SmashAltar uses the original small-world budget before scaling the result
by the compact map's eligible area. Applying the native 85-per-tier
subtraction after shrinking width would suppress tiers two and three. -/
def hardmodeOreVeinCount (world : World) (tier round : Nat) (variant : Bool) : Nat :=
  let nativeWidth := max 4200 world.width
  let areaScale := if world.width < 4200 then world.width.toFloat / 4200 else 1
  let base := (nativeWidth.toFloat / 4200 * 310 - tier.toFloat * 85) * 0.85
  let factor := (if tier == 0 then 1.0499999523162842 else 1) *
    (if variant then 0.8999999761581421 else 1)
  (base * factor * areaScale / (max 1 round).toFloat).ceil.toUInt64.toNat

def smashHardmodeAltar (initial : World) (seed : UInt64) (clock count mask : Nat) :
    World × Nat × Tile := Id.run do
  let tier := count % 3
  let round := count / 3 + 1
  let mut random := UnifiedRandom.fromEntropy (hash (seed, clock, count, (4750 : Nat)))
  let mut mask := mask
  if round == 1 then
    let (choice, next) := random.next 0 2
    random := next
    if choice == 0 then mask := mask + 2 ^ tier
  let ore := hardmodeOreTile tier mask
  let surface := groundHeight seed 19
  let rock := initial.height / 2
  let top := if tier == 0 then surface else if tier == 1 then rock else (2 * rock + initial.height) / 3
  let bottom := max (top + 1) (initial.height - max 1 (initial.height * 150 / 1200))
  let side := max 1 (initial.width * 100 / 4200)
  let count := hardmodeOreVeinCount initial tier round (mask / (2 ^ tier) % 2 == 1)
  let mut world := initial
  for _ in [:count] do
    let (x, next) := random.next side (initial.width - side : Nat)
    let (y, next) := next.next top bottom
    let (strength, next) := next.next 5 10
    let (steps, next) := next.next 5 10
    let (updated, next) := runHardmodeOreVein world ore ⟨x, y⟩ strength.toNat steps.toNat next
    world := updated
    random := next
  return (world, mask, ore)

end Lean4Terria
