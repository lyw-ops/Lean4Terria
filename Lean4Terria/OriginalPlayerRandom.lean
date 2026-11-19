import Lean4Terria.OriginalPlayerCollision
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.UnifiedRandom

namespace Lean4Terria.OriginalPlayerRandom

open OriginalCollision (Grid single slopeId tileIndex)
open OriginalPlayerCollision (Body)

/-- The local player's first update can enter the mana-heat visual loop with zero
heat, before ResetEffects initializes the field to 1. Remap clamps the chance to
zero, so the two IL_0794 samples fail without allocating a dust particle. -/
def initialManaHeatVisuals (random : UnifiedRandom) : UnifiedRandom :=
  random.nextInt.2.nextInt.2

/-- The random prefix of CheckCrackedBrickBreak for the local player. Both velocity
samples precede the terrain check, including on ordinary dirt. Cracked-brick tile
effects are handled separately. Call at Player.Update IL_8383, before collision. -/
def crackedBrickCheck (grid : Grid) (body : Body) (shimmering : Bool)
    (random : UnifiedRandom) : UnifiedRandom :=
  let x := tileIndex (body.position.x + single body.width / 2)
  let y := tileIndex (body.position.y + single body.height / 2)
  if shimmering || x < 10 || y < 10 || x ≥ (grid.width : Int) - 10 ||
      y ≥ (grid.height : Int) - 10 then random
  else ((random.next 2 12).2.next 2 12).2

/-- Explicit MakeFloorDust rolls for dry dirt when Dust.NewDust does not consume
randomness. Use after collision, when FloorVisuals actually selects unsloped dirt.
The landing path rolls for twenty particles and samples a scale on each success. -/
def dirtFloorDustWithoutAllocation (landing : Bool) (random : UnifiedRandom) : UnifiedRandom := Id.run do
  if !landing then return ((random.next 0 3).2.next 0 100).2
  let mut random := random
  for _ in [:20] do
    let (roll, next) := random.next 0 3
    random := if roll == 0 then (next.next 0 3).2 else next
  return random

/-- The impact flag is latched after the slope/step adjustments and before tile
collision. Ordinary StepDown and normal-gravity StepUp leave Y velocity alone. -/
private def floorImpact (grid : Grid) (before : Body) : Bool :=
  let body := { before with
    velocity := ⟨max (-16) (min 16 before.velocity.x), max (-16) (min 16 before.velocity.y)⟩ }
  let body := body.slopeDownMovement grid
  let body := if body.gravityDirection == -1 && body.velocity.y ≤ body.gravity && !body.controlUp then
      let body := if body.velocity.y == body.gravity then body.stepDown grid else body
      body.stepUp grid
    else body
  body.velocity.y.abs > body.gravity

/-- `Player.MakeFloorDust` for unsloped dirt (dust 0) with a real shared dust pool (a window
client). Walking: one attempt, `Next(3) == 0` and `Next(100) <= |velocity.X| / 3 * 100`, and only
while `|velocity.X| + |velocity.Y| > 1` (`CanSpawnWalkingEffects`). Landing: twenty attempts on the
`Next(3)` roll alone. A passing attempt calls `Dust.NewDust(position + (0, height - 2), width, 6,
0, 0, 0, alpha 50)` and then changes the particle — also the dummy slot of a refused call, so the
landing scale roll is drawn either way: `noGravity`; walking: velocity × 0.2; landing: the burst
shape below; both: `position.X -= clamp(velocity.X, ±6)` (IL in `death-runtime/il/`). -/
def dirtFloorDust (landing : Bool) (body : Body) (pool : OriginalForestGuideArrow.DustPool)
    (random : UnifiedRandom) :
    OriginalForestGuideArrow.DustPool × Array OriginalForestGuideArrow.DustParticle × UnifiedRandom := Id.run do
  let count := if landing then 20 else 1
  let vx := body.velocity.x
  let clamped : Float32 := if vx > 6 then 6 else if vx < -6 then -6 else vx
  let mut pool := pool
  let mut random := random
  let mut particles := #[]
  for _ in [:count] do
    let (roll, next) := random.next 0 3
    random := next
    let mut ok := roll == 0
    if !landing then
      let (chance, next) := random.next 0 100
      random := next
      let speed : Float32 := vx.abs / 3
      if single chance > speed * 100 then ok := false
    if !ok then continue
    if !landing && !(body.velocity.x.abs + body.velocity.y.abs > 1) then continue
    let request : OriginalForestGuideArrow.DustRequest := {
      position := ⟨body.position.x, body.position.y + single body.height - 2⟩
      width := body.width, height := 6, typeId := 0, alpha := 50 }
    let (nextPool, particle, next) := pool.create request random
    pool := nextPool
    random := next
    let mut scaleRoll : Float32 := 0
    if landing then
      let (r, next) := random.next 0 3
      random := next
      scaleRoll := single r * 0.1
    if let some p := particle then
      let p := { p with noGravity := true }
      let p := if landing then
          let v : OriginalCollision.Vector := ⟨p.velocity.x * 1.2, p.velocity.y * 0.8⟩
          let v : OriginalCollision.Vector := ⟨v.x, v.y - 1⟩
          let v : OriginalCollision.Vector := ⟨v.x * 0.8, v.y * 0.8⟩
          let vx := (p.position.x - (body.position.x + single (body.width / 2))) * 0.2
          let vy := if v.y > 0 then v.y * -1 else v.y
          { p with scale := p.scale + scaleRoll, velocity := ⟨vx + clamped * 0.3, vy⟩ }
        else { p with velocity := ⟨p.velocity.x * 0.2, p.velocity.y * 0.2⟩ }
      particles := particles.push { p with position := ⟨p.position.x - clamped * 1, p.position.y⟩ }
  return (pool, particles, random)

/-- `FloorVisuals` after movement, with the client's real dust pool (`some`) or the legacy
roll-only contract. GetFloorTile examines the center column, then its left and right neighbors. -/
def floorVisualsWithPool (grid : Grid) (before after : Body) (wet : Bool)
    (pool : Option OriginalForestGuideArrow.DustPool) (random : UnifiedRandom) :
    Option OriginalForestGuideArrow.DustPool × Array OriginalForestGuideArrow.DustParticle × UnifiedRandom := Id.run do
  let none' := (pool, (#[] : Array OriginalForestGuideArrow.DustParticle), random)
  if after.velocity.y != 0 || after.hasGrapple || after.mountCart || wet then return none'
  let x := tileIndex (after.position.x + single (after.width / 2 : Nat))
  let y := if after.gravityDirection == -1 then
      (after.position.y - 0.1).toInt32.toInt.tdiv 16
    else tileIndex (after.position.y + single after.height)
  if y < 0 || y ≥ (grid.height : Int) then return none'
  let tiles := #[x, x - 1, x + 1].map fun col =>
    if col < 0 || col ≥ (grid.width : Int) then ({} : OriginalWorld.Tile)
    else grid.tileAt col.toNat y.toNat
  let some floor := tiles.find? (fun tile =>
    !tile.actuated && tile.id.any (fun id => (grid.rules id).solid)) | return none'
  if floor.id != some 0 || tiles.any (fun tile => slopeId tile.shape != 0) then return none'
  match pool with
  | none => return (none, #[], dirtFloorDustWithoutAllocation (floorImpact grid before) random)
  | some pool =>
    let (pool, particles, random) := dirtFloorDust (floorImpact grid before) after pool random
    return (some pool, particles, random)

/-- The legacy roll-only `FloorVisuals` (no dust pool). -/
def floorVisuals (grid : Grid) (before after : Body) (wet : Bool)
    (random : UnifiedRandom) : UnifiedRandom :=
  (floorVisualsWithPool grid before after wet none random).2.2

/-- ItemCheck IL_02fa rolls revolver bonus decay even for other selected items.
Use when ItemCheck reaches the non-Revolver branch, before hit and use logic. -/
def otherItemRevolverDecay (random : UnifiedRandom) : UnifiedRandom :=
  (random.next 0 3).2

end Lean4Terria.OriginalPlayerRandom
