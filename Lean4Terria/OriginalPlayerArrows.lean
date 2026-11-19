import Lean4Terria.OriginalForestGuideArrow
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalNpcCombatText
import Lean4Terria.OriginalLuck

/-!
# Local-player Wooden Arrow (Terraria desktop 1.4.5.8 Classic)

Projectile type 1 fired by the local player (`owner == Main.myPlayer`, not `npcProj`), in dry terrain:

* `Projectile.NewProjectile` → `NewProjectileSetup` (`allocate`, `nextKey`) → `SetDefaults(1)` → the
  aiStyle-1 spawn velocity clamp (`clampVelocity`). For type 1 none of these draw from `Main.rand`.
* `Projectile.Update` → `AI_001` → `HandleMovement` → `Damage` (`CutTiles`, `Damage_PVE`,
  `Damage_PVE_Inner`) → `Kill` (`step`), on the `UpdateProjectiles` stream, in projectile slot order.

Differences from the Guide's arrow (`OriginalForestGuideArrow`, which is `npcProj` + `noDropItem`):
the hit rolls ranged crit (`Next(1, 101) ≤ rangedCrit + bonusCritChance`) before `Main.DamageVar(damage,
player.luck)`, strikes with `fromPlayer = owner`, and still ignores `npc.immune[owner]` (single-penetrate
projectiles skip the immunity gate and never write it). `noDropItem` is false, yet 1.4.5.8 has no
Wooden-Arrow recovery anywhere (`Kill` only plays sound and makes ten dust 7; `Kill_ExplodeTiles` has no
type-1 branch), so `recoveryRequests` is always empty. Evidence: `.lake/original-player-arrows/`.
-/
namespace Lean4Terria.OriginalPlayerArrows

open OriginalCollision (Grid Vector single)
open OriginalStarterMelee (Npc Rect OnKill OnStrike Strike truncF32 toRotation)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)

def typeId : Nat := 1
def width : Nat := 10
def height : Nat := 10
def maxProjectiles : Nat := 1000

/-! ## Slot allocation (`NewProjectileSetup`) -/

/-- What `NewProjectileSetup` / `FindOldestProjectile` read from one `Main.projectile` entry. -/
structure SlotInfo where
  active : Bool := false
  netImportant : Bool := false
  timeLeft : Int := 0
  deriving Repr, BEq, Inhabited

/-- `FindOldestProjectile`: the first slot in `0..999` whose `timeLeft` is strictly the smallest among
the non-`netImportant` ones (initial bound 9999999). It does not read `active`. With none it returns
1000 — the extra `Main.projectile[1000]` entry, which the update loop never visits. -/
def findOldest (slots : Array SlotInfo) : Nat := Id.run do
  let mut best := maxProjectiles
  let mut least : Int := 9999999
  for i in [:maxProjectiles] do
    let s := slots.getD i {}
    if !s.netImportant && s.timeLeft < least then
      best := i
      least := s.timeLeft
  return best

/-- `NewProjectileSetup` with `DebugOptions.Shared_RandomizeProjectileSlots` off (no RNG): the first
inactive slot in `0..999`, otherwise `findOldest`. Missing entries count as inactive. -/
def allocate (slots : Array SlotInfo) : Nat :=
  match (List.range maxProjectiles).find? (fun i => !(slots.getD i {}).active) with
  | some i => i
  | none => findOldest slots

/-- `ProjectileKey` (spawner 8 bits, index 10 bits, generation 14 bits). -/
structure Key where
  spawner : Nat
  index : Nat
  generation : Nat
  deriving Repr, BEq, Inhabited

/-- `ProjectileKey.Pack`. -/
def Key.bits (k : Key) : UInt32 :=
  (k.spawner % 256).toUInt32 ||| ((k.index % 1024).toUInt32 <<< 8) ||| ((k.generation % 16384).toUInt32 <<< 18)

/-- `new ProjectileKey(Main.myPlayer, slot, ++slotGenerations[slot])`: the key and the new counter value. -/
def nextKey (myPlayer slot generationBefore : Nat) : Key × Nat :=
  let g := generationBefore + 1
  ({ spawner := myPlayer, index := slot, generation := g % 16384 }, g)

/-! ## Spawn (`NewProjectile`) -/

/-- The typed request the bow (`Player.ItemCheck_Shoot`, another owner) hands over: the projectile
center, velocity, final damage and knockback, and the owner. -/
structure Launch where
  center : Vector
  velocity : Vector
  damage : Int
  knockback : Float32
  owner : Nat := 0
  deriving Repr

/-- `NewProjectile` IL_012b..IL_019b for aiStyle 1: multiply by 0.97 while `vx ≥ 16`, `vx ≤ -16`,
`vy ≥ 16` or `vy < -16` (the last bound is strict). -/
def clampVelocity (v : Vector) : Vector := Id.run do
  let mut v := v
  for _ in [:10000] do
    if v.x ≥ 16 || v.x ≤ -16 || v.y ≥ 16 || v.y < -16 then
      v := ⟨v.x * 0.97, v.y * 0.97⟩
    else break
  return v

structure Arrow where
  slot : Nat
  key : Key
  position : Vector
  velocity : Vector
  owner : Nat
  damage : Int
  knockback : Float32
  oldPosition : Vector := ⟨0, 0⟩
  direction : Int := 1
  oldDirection : Int := 0
  rotation : Float32 := 0
  /-- `ai[0]`, the gravity counter. -/
  age : Nat := 0
  timeLeft : Nat := 1200
  penetrate : Nat := 1
  /-- `ai[2]` as raw bits (`AI_001_Features`): bit 0 HarpyCharmed, bit 1 HarpyCharmConsumed, … -/
  features : UInt32 := 0
  /-- `bonusCritChance` (0 from an ordinary bow source). -/
  bonusCritChance : Int := 0
  active : Bool := true
  deriving Repr

/-- The owner flags `SetDefaults(1)` leaves on a local-player arrow (native capture). -/
structure Flags where
  friendly : Bool := true
  hostile : Bool := false
  ranged : Bool := true
  arrow : Bool := true
  npcProj : Bool := false
  noDropItem : Bool := false
  tileCollide : Bool := true
  maxPenetrate : Nat := 1
  usesLocalNPCImmunity : Bool := false
  usesIDStaticNPCImmunity : Bool := false
  deriving Repr, BEq

def flags : Flags := {}

/-- `NewProjectile` for a dry spawn (`WetCollision` false): `position = center − (5, 5)`, the clamp,
then `ai` from the call (0). `slot`/`key` come from `allocate`/`nextKey`. -/
def spawn (launch : Launch) (slot : Nat) (key : Key) : Arrow :=
  { slot, key
    position := ⟨launch.center.x - single width * 0.5, launch.center.y - single height * 0.5⟩
    velocity := clampVelocity launch.velocity
    owner := launch.owner, damage := launch.damage, knockback := launch.knockback }

def Arrow.hitbox (arrow : Arrow) : Rect :=
  ⟨truncF32 arrow.position.x, truncF32 arrow.position.y, width, height⟩

/-! ## Update -/

/-- The owner's stats `Damage_PVE_Inner` reads. -/
structure Shooter where
  rangedCrit : Int := 4
  luck : Float32 := 0
  deriving Repr

structure Target where
  npc : Npc
  /-- Green/blue slime `HitEffect` (dust 4 with the NPC's alpha and color). -/
  slime : Bool := false
  dustAlpha : Int := 175
  dustColor : UInt32 := 0

structure Context where
  grid : Grid
  shooter : Shooter := {}
  bounds : Option Rect := none
  combatTextAvailable : Bool := true
  text : OriginalNpcCombatText.State := {}
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
  fromPlayer : Nat
  gore : Array OriginalGore.Particle := #[]

structure Step where
  text : OriginalNpcCombatText.State := {}
  arrow : Arrow
  random : UnifiedRandom
  dustPool : DustPool
  dust : Array DustParticle := #[]
  hit : Option Hit := none
  ended : Option EndReason := none

/-- Item requests from the arrow's own death. Empty in 1.4.5.8 for type 1 (see the module note). -/
def recoveryRequests (_ : Step) : Array OriginalItems.Drop := #[]

private def withinWorld (ctx : Context) (arrow : Arrow) : Bool :=
  let bounds := ctx.bounds.getD ⟨0, 0, 16 * ctx.grid.width, 16 * ctx.grid.height⟩
  arrow.position.x > single bounds.x && arrow.position.y > single bounds.y &&
    arrow.position.x + single width < single (bounds.x + bounds.w) &&
    arrow.position.y + single height < single (bounds.y + bounds.h)

/-- `AI_001` for type 1. -/
private def ai (arrow : Arrow) : Arrow :=
  let age := min (arrow.age + 1) 15
  let vy := if age ≥ 15 then arrow.velocity.y + 0.1 else arrow.velocity.y
  let rotation := toRotation ⟨arrow.velocity.x, vy⟩ + 1.57
  { arrow with age, rotation, velocity := ⟨arrow.velocity.x, min vy 16⟩ }

/-- `HandleMovement` (tile collision with slopes; probe stepping when faster than the hitbox). -/
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

/-- `Kill` for type 1: sound, ten dust 7 at the arrow, `active = false`, `timeLeft = 0`. -/
private def kill (step : Step) (reason : EndReason) : Step :=
  let step := emitDust step { position := step.arrow.position, width, height, typeId := 7 } 10
  { step with ended := some reason, arrow := { step.arrow with active := false, timeLeft := 0 } }

/-- `Damage_PVE` order with the single-penetrate gate: the first NPC slot that is active, alive,
hittable and intersecting. `npc.immune[owner]` is ignored (maxPenetrate 1, no local/static immunity). -/
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

/-- `Damage_PVE_Inner` for the local player's arrow: crit, `DamageVar(luck)`, the hit-particle point, `StrikeNPC` (text, life,
knockback), slime `HitEffect`, the caller's strike effects, the death path, `penetrate--`. -/
private def hitNpc (ctx : Context) (step : Step) (slot : Nat) (target : Target) : Step := Id.run do
  let direction := step.arrow.direction
  let critChance := ctx.shooter.rangedCrit + (if step.arrow.bonusCritChance > 0 then step.arrow.bonusCritChance else 0)
  -- IL_0ae9.. crit, IL_0cf5 DamageVar(luck), IL_0d96 NextVector2FromRectangle(npc.Hitbox) for the hit
  -- particle (two draws, owned projectiles only), then StrikeNPC.
  let strike := OriginalStarterMelee.rollAndStrike target.npc step.arrow.damage critChance step.arrow.knockback
    direction 2 step.random step.text ctx.shooter.luck
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
  let strike : Strike :=
    { strike with npc := { npc with active := !died }, died := died, random := result.random, text := result.text }
  let effects := ctx.onStrike result.dust slot direction strike
  let (effectDust, random) := (effects.dust, effects.random)
  let (drops, random) := if died then ctx.onKill slot npc random else (#[], random)
  let hit : Hit :=
    { slot, strike := { strike with random }, drops, direction, fromPlayer := step.arrow.owner, gore := effects.gore }
  return { result with
    random, hit := some hit, dust := result.dust ++ effectDust
    dustPool := result.dustPool.occupy effectDust
    -- IL_2c7b: an owned aiStyle-1 arrow marks ai[2] HarpyCharmed | HarpyCharmConsumed on every hit.
    arrow := { step.arrow with penetrate := step.arrow.penetrate - 1, features := step.arrow.features ||| 3 } }

/-- One dry `Projectile.Update` of a local-player arrow. Thread `random` (the `UpdateProjectiles`
stream) and `dustPool` through the whole projectile phase in slot order, and apply the returned hit
(NPC body, drops) before the next projectile. `targets` is the NPC array indexed by slot. -/
def step (ctx : Context) (arrow : Arrow) (targets : Array (Option Target))
    (random : UnifiedRandom) (dustPool : DustPool := {}) : Step := Id.run do
  let text : OriginalNpcCombatText.State :=
    { ctx.text with legacyAvailable := ctx.combatTextAvailable, fromPlayer := arrow.owner, fromNet := false }
  let start : Step := { arrow := arrow, random := random, dustPool := dustPool, text := text }
  if !arrow.active || start.text.boundary.isSome then return start
  if !withinWorld ctx arrow then
    return { start with arrow := { arrow with active := false }, ended := some .outsideWorld }
  let arrow := ai { arrow with oldPosition := arrow.position, oldDirection := arrow.direction }
  let (arrow, collided) := move ctx.grid arrow
  let arrow := advancePosition arrow
  let mut step := { start with arrow }
  if collided then
    -- HandleMovement: tile-484 cut scan, Next(3) (on 0 a second cut scan, no effect in dry terrain), Kill.
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

end Lean4Terria.OriginalPlayerArrows
