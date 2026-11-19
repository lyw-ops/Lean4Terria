import Lean4Terria.OriginalForestGuideBow
import Lean4Terria.OriginalStarterMelee

/-!
Terraria 1.4.5.8 Classic Guide wooden arrow in dry forest terrain. Call
`spawn` during NPC updates, then `step` in projectile slot order in the
same tick, using the `UpdateProjectiles` random stream and updated NPCs.
-/
namespace Lean4Terria.OriginalForestGuideArrow

open OriginalCollision (Grid Vector single)
open OriginalStarterMelee (Npc Rect OnKill OnStrike Strike truncF32 toRotation)
open OriginalForestGuideBow (Shot)

structure Arrow where
  position : Vector
  velocity : Vector
  owner : Nat
  damage : Int
  knockback : Float32
  oldPosition : Vector := ⟨0, 0⟩
  direction : Int := 1
  oldDirection : Int := 0
  rotation : Float32 := 0
  age : Nat := 0
  timeLeft : Nat := 1200
  penetrate : Nat := 1
  active : Bool := true
  deriving Repr

def typeId : Nat := 1
def width : Nat := 10
def height : Nat := 10
def spriteDirection : Int := 1

/-- The Guide supplies the center to `NewProjectile`. This path is friendly,
`npcProj` and `noDropItem` even when the stored owner is the local player. -/
def spawn (shot : Shot) : Option Arrow := do
  guard (shot.typeId == typeId && shot.npcProj && shot.noDropItem)
  return {
    position := shot.initialPosition, velocity := shot.velocity, owner := shot.owner
    damage := shot.damage, knockback := shot.knockback
    timeLeft := shot.timeLeft, penetrate := shot.penetrate
  }

def Arrow.hitbox (arrow : Arrow) : Rect :=
  ⟨truncF32 arrow.position.x, truncF32 arrow.position.y, width, height⟩

structure Target where
  npc : Npc
  slime : Bool := false
  dustAlpha : Int := 175
  dustColor : UInt32 := 0

structure Context where
  grid : Grid
  /-- Local client world bounds, in pixels. The tile extents supply the default. -/
  bounds : Option Rect := none
  combatTextAvailable : Bool := true
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  onKill : OnKill := fun _ _ random => (#[], random)
  onStrike : OnStrike := OriginalStarterMelee.noStrikeEffects

inductive EndReason where
  | tile | enemy | expired | outsideWorld
  deriving BEq, Repr

structure Hit where
  slot : Nat
  strike : Strike
  drops : Array OriginalItems.Drop
  direction : Int
  /-- `StrikeNPC` receives the no-player sentinel for `npcProj`. -/
  fromPlayer : Nat := 255
  /-- The Gore the hit effect created, for the caller's pool. -/
  gore : Array OriginalGore.Particle := #[]

structure Step where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  arrow : Arrow
  random : UnifiedRandom
  dustPool : DustPool
  dust : Array DustParticle := #[]
  hit : Option Hit := none
  ended : Option EndReason := none

private def withinWorld (ctx : Context) (arrow : Arrow) : Bool :=
  let bounds := ctx.bounds.getD ⟨0, 0, 16 * ctx.grid.width, 16 * ctx.grid.height⟩
  arrow.position.x > single bounds.x && arrow.position.y > single bounds.y &&
    arrow.position.x + single width < single (bounds.x + bounds.w) &&
    arrow.position.y + single height < single (bounds.y + bounds.h)

private def ai (arrow : Arrow) : Arrow :=
  let age := min (arrow.age + 1) 15
  let vy := if age ≥ 15 then arrow.velocity.y + 0.1 else arrow.velocity.y
  let rotation := toRotation ⟨arrow.velocity.x, vy⟩ + 1.57
  { arrow with age, rotation, velocity := ⟨arrow.velocity.x, min vy 16⟩ }

private def move (grid : Grid) (arrow : Arrow) : Arrow × Bool := Id.run do
  let original := arrow.velocity
  let collideTiles pos velocity := (OriginalCollision.tileCollision grid pos velocity width height
    { fallThrough := true, fall2 := true, hoik := true }).displacement
  let speed := (original.x * original.x + original.y * original.y).sqrt
  let stepSize := single (min width height)
  if speed ≤ stepSize then
    let tileVelocity := collideTiles arrow.position original
    let slope := OriginalCollision.slopeCollision grid arrow.position tileVelocity width height true
    let collided := slope.position != arrow.position || slope.velocity != tileVelocity ||
      slope.velocity != original
    return ({ arrow with position := slope.position, velocity := slope.velocity }, collided)
  -- HandleMovement advances a collision probe by at most one hitbox width.
  -- The projectile itself receives slope corrections here and applies velocity later.
  let inverseSpeed : Float32 := 1 / speed
  let direction : Vector := ⟨original.x * inverseSpeed, original.y * inverseSpeed⟩
  let mut remaining := speed
  let mut total : Vector := ⟨0, 0⟩
  let mut probe := arrow.position
  let mut position := arrow.position
  let mut collided := false
  for _ in [:300] do
    if remaining ≤ 0 then break
    let distance := min remaining stepSize
    remaining := remaining - distance
    let tileVelocity := collideTiles probe ⟨direction.x * distance, direction.y * distance⟩
    probe := probe + tileVelocity
    let slope := OriginalCollision.slopeCollision grid probe tileVelocity width height true
    collided := collided || slope.position != probe || slope.velocity != tileVelocity
    position := slope.position + ⟨position.x - probe.x, position.y - probe.y⟩
    probe := slope.position
    total := total + slope.velocity
  let velocity : Vector := ⟨
    if (total.x - original.x).abs < 0.0001 then original.x else total.x,
    if (total.y - original.y).abs < 0.0001 then original.y else total.y⟩
  let slope := OriginalCollision.slopeCollision grid probe velocity width height true
  position := slope.position + ⟨position.x - probe.x, position.y - probe.y⟩
  collided := collided || slope.position != probe || slope.velocity != velocity || slope.velocity != original
  return ({ arrow with position, velocity := slope.velocity }, collided)

private def advancePosition (arrow : Arrow) : Arrow :=
  { arrow with position := arrow.position + arrow.velocity }

private def changeDirection (arrow : Arrow) : Arrow :=
  { arrow with direction := if arrow.velocity.x < 0 then -1 else 1 }

private def emitDust (step : Step) (request : DustRequest) (count : Nat) : Step := Id.run do
  let mut step := step
  for _ in [:count] do
    let (dustPool, particle, random) := step.dustPool.create request step.random
    step := { step with dustPool, random, dust := particle.elim step.dust step.dust.push }
  return step

private def kill (step : Step) (reason : EndReason) : Step :=
  let step := emitDust step { position := step.arrow.position, width, height, typeId := 7 } 10
  { step with ended := some reason, arrow := { step.arrow with active := false, timeLeft := 0 } }

private def firstTarget (targets : Array (Option Target)) (arrow : Arrow) : Option (Nat × Target) := Id.run do
  if arrow.damage ≤ 0 then return none
  let box := arrow.hitbox
  for slot in [:targets.size] do
    let some target := targets[slot]! | continue
    let npc := target.npc
    if npc.active && npc.life > 0 && !npc.friendly && !npc.dontTakeDamage && !npc.immortal &&
        box.intersects npc.rect then
      return some (slot, target)
  return none

private def hitNpc (ctx : Context) (step : Step) (slot : Nat) (target : Target) : Step := Id.run do
  let direction := step.arrow.direction
  let (rolled, afterDamageVar) := OriginalStarterMelee.damageVar step.arrow.damage step.random
  let strike := OriginalStarterMelee.strikeNpcWithText target.npc rolled step.arrow.knockback direction false afterDamageVar step.text
  let npc := strike.npc
  let damage := strike.damage
  let died := npc.life ≤ 0
  let mut result := { step with random := strike.random, text := strike.text }
  if strike.text.boundary.isSome then return result
  if target.slime then
    let count := if died then 50 else
      ((damage.toFloat / npc.lifeMax.toFloat * 100).ceil.toInt64.toInt).toNat
    result := emitDust result {
      position := npc.position, width := npc.width, height := npc.height, typeId := 4
      speed := if died then ⟨single (2 * direction), -2⟩ else ⟨single direction, -1⟩
      alpha := target.dustAlpha, color := target.dustColor
    } count
  let strike : Strike := {
    npc := { npc with active := !died }, rolled, damage, died, crit := false, random := result.random, text := result.text
  }
  let effects := ctx.onStrike result.dust slot direction strike
  let (effectDust, random) := (effects.dust, effects.random)
  let (drops, random) := if died then ctx.onKill slot npc random else (#[], random)
  let hit : Hit := { slot, strike := { strike with random }, drops, direction, gore := effects.gore }
  return { result with
    random, hit := some hit, dust := result.dust ++ effectDust
    dustPool := result.dustPool.occupy effectDust
    arrow := { step.arrow with penetrate := step.arrow.penetrate - 1 }
  }

/-- Dry `Projectile.Update`. Apply the returned NPC hit and drops before
advancing any higher projectile slot. Thread `random` and `dustPool` through
the entire projectile phase. Alive NPC hitboxes come from the current NPC
phase, and the caller owns the NPC array and shared death/loot path. -/
def step (ctx : Context) (arrow : Arrow) (targets : Array (Option Target))
    (random : UnifiedRandom) (dustPool : DustPool := {}) : Step := Id.run do
  let start : Step := { arrow, random, dustPool, text := {ctx.text with legacyAvailable := ctx.combatTextAvailable, fromPlayer := 255, fromNet := false} }
  if !arrow.active || start.text.boundary.isSome then return start
  if !withinWorld ctx arrow then
    return { start with arrow := { arrow with active := false }, ended := some .outsideWorld }
  let arrow := ai { arrow with oldPosition := arrow.position, oldDirection := arrow.direction }
  let (arrow, collided) := move ctx.grid arrow
  let arrow := advancePosition arrow
  let mut step := { start with arrow }
  if collided then
    -- Vanilla tests tile cutting before killing, including its one-in-three roll.
    step := kill { step with random := (step.random.next 0 3).2 } .tile
    return { step with arrow := changeDirection (advancePosition step.arrow) }
  step := { step with arrow := changeDirection arrow }
  if let some (slot, target) := firstTarget targets step.arrow then
    step := hitNpc ctx step slot target
  if step.text.boundary.isSome then return step
  step := { step with arrow := { step.arrow with timeLeft := step.arrow.timeLeft - 1 } }
  if step.arrow.timeLeft == 0 then return kill step .expired
  if step.arrow.penetrate == 0 then return kill step .enemy
  return step

structure Draw where
  position : Vector
  source : Rect
  origin : Vector
  rotation : Float32
  scale : Vector
  flipX : Bool
  deriving Repr

/-- Native `DrawProjDirect` geometry for `Projectile_1.xnb`. The caller
supplies the camera, lighting tint and normal sprite batch transform. -/
def draw (arrow : Arrow) (viewPosition : Vector) : Option Draw := do
  guard arrow.active
  return {
    position := ⟨arrow.position.x - viewPosition.x + 7, arrow.position.y - viewPosition.y + 5⟩
    source := ⟨0, 0, 14, 32⟩
    origin := ⟨7, 5⟩
    rotation := arrow.rotation
    scale := ⟨1, 1⟩
    flipX := false
  }

end Lean4Terria.OriginalForestGuideArrow
