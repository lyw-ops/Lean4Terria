import Lean4Terria.UnderworldCreatures
import Lean4Terria.Wraith

namespace Lean4Terria

def maxCavernEnemies : Nat := 24
def cavernFlameDamage : Nat := 22
def cavernFlameLife : Nat := 300

inductive CavernEnemyKind where
  | illuminantBat | illuminantSlime | clinger | floatyGross
  deriving BEq, Repr, Inhabited

namespace CavernEnemyKind

def sprite : CavernEnemyKind → Nat
  | .illuminantBat => 137
  | .illuminantSlime => 138
  | .clinger => 101
  | .floatyGross => 182

def name : CavernEnemyKind → String
  | .illuminantBat => "Illuminant Bat"
  | .illuminantSlime => "Illuminant Slime"
  | .clinger => "Clinger"
  | .floatyGross => "Floaty Gross"

def width : CavernEnemyKind → Nat
  | .illuminantBat => 26
  | .illuminantSlime => 25
  | .clinger => 30
  | .floatyGross => 24

def height : CavernEnemyKind → Nat
  | .illuminantBat => 20
  | .illuminantSlime => 18
  | .clinger => 30
  | .floatyGross => 62

def scale : CavernEnemyKind → Float32
  | .illuminantSlime => 1.05
  | _ => 1

def health : CavernEnemyKind → Nat
  | .illuminantBat => 200
  | .floatyGross => 240
  | .illuminantSlime => 180
  | .clinger => 320

def damage : CavernEnemyKind → Nat
  | .illuminantBat => 75
  | .illuminantSlime | .clinger => 70
  | .floatyGross => 65

def defense (kind : CavernEnemyKind) : Nat := if kind == .floatyGross then 18 else 30

def coins : CavernEnemyKind → Nat
  | .illuminantBat | .floatyGross => 500
  | .illuminantSlime => 400
  | .clinger => 600

def frameCount : CavernEnemyKind → Nat
  | .illuminantBat => 4
  | .clinger | .floatyGross => 5
  | _ => 2

def resistance : CavernEnemyKind → Float32
  | .illuminantBat => 0.75
  | .illuminantSlime => 0.85
  | .clinger => 0.2
  | .floatyGross => 0.7

end CavernEnemyKind

/-- Native pixel positions and the persistent AI fields of styles 1, 13, 14,
and 22. `jumpClock` stores the tether or hover timer for non-jumping actors. -/
structure CavernEnemy where
  id : Nat
  kind : CavernEnemyKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  oldVx : Float32 := 0
  oldVy : Float32 := 0
  rotation : Float32 := 0
  lastJumpX : Float32 := 0
  hoverAnchorX : Float32 := 0
  hoverAnchorY : Float32 := 0
  health : Nat
  timeLeft : Nat := 937
  direction : Int := 0
  directionY : Int := 1
  spriteDirection : Int := -1
  frame : Nat := 0
  frameClock : Nat := 0
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  jumpClock : Int := 0
  retargetClock : Nat := 0
  searchClock : Nat := 0
  orbitClock : Int := 0
  anchor : Option TilePos := none
  attackClock : Nat := 0
  justHit : Bool := false
  deriving Repr, Inhabited

instance : BEq CavernEnemy where
  beq a b := a.id == b.id && a.kind == b.kind && a.health == b.health && a.timeLeft == b.timeLeft &&
    a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits && a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.oldVx.toBits == b.oldVx.toBits && a.oldVy.toBits == b.oldVy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.lastJumpX.toBits == b.lastJumpX.toBits &&
    a.hoverAnchorX.toBits == b.hoverAnchorX.toBits && a.hoverAnchorY.toBits == b.hoverAnchorY.toBits &&
    a.direction == b.direction && a.directionY == b.directionY && a.spriteDirection == b.spriteDirection &&
    a.frame == b.frame && a.frameClock == b.frameClock && a.collideX == b.collideX && a.collideY == b.collideY &&
    a.wet == b.wet && a.jumpClock == b.jumpClock && a.retargetClock == b.retargetClock &&
    a.searchClock == b.searchClock && a.orbitClock == b.orbitClock && a.anchor == b.anchor &&
    a.attackClock == b.attackClock && a.justHit == b.justHit

/-- Hostile cursed flame, projectile type 96. Position is its center. -/
structure CavernFlame where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  life : Nat := cavernFlameLife
  clock : Nat := 0
  slot : Option Nat := none
  deriving Repr, Inhabited

instance : BEq CavernFlame where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits && a.vx.toBits == b.vx.toBits &&
    a.vy.toBits == b.vy.toBits && a.life == b.life && a.clock == b.clock && a.slot == b.slot

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (value : Int) : Float32 := single value / 16
private def clamp (value low high : Float32) : Float32 := min high (max low value)
private def snap (value : Float32) : Float32 := if -0.005 < value.toFloat && value.toFloat < 0.005 then 0 else value
private def length (dx dy : Float32) : Float32 := (dx * dx + dy * dy).toFloat.sqrt.toFloat32

def CavernEnemy.centerX (enemy : CavernEnemy) : Float32 := enemy.x + single enemy.kind.width * 0.5
def CavernEnemy.centerY (enemy : CavernEnemy) : Float32 := enemy.y + single enemy.kind.height * 0.5

def CavernEnemy.hitBox (enemy : CavernEnemy) : HitBox :=
  CasterAI.npcBox enemy.x enemy.y enemy.kind.width enemy.kind.height

def CavernEnemy.contact (enemy : CavernEnemy) (player : Player) : Option Int :=
  CasterAI.contact enemy.hitBox (enemy.x + single (enemy.kind.width / 2 : Nat)) player

def CavernEnemy.clear (enemy : CavernEnemy) (world : World) : Bool :=
  !npcSolidOverlap world enemy.x enemy.y enemy.kind.width enemy.kind.height

def CavernEnemy.atAnchor (id : Nat) (anchor : TilePos) : CavernEnemy :=
  { id, kind := .clinger, x := single (anchor.x * 16 - 7), y := single (anchor.y * 16 - 7)
    health := CavernEnemyKind.clinger.health, anchor := some anchor }

def CavernEnemy.atFloor (id : Nat) (kind : CavernEnemyKind) (column floor : Int) : CavernEnemy :=
  if kind == .clinger then atAnchor id ⟨column, floor⟩
  else
    { id, kind, x := single (column * 16 + 8 - (kind.width / 2 : Nat))
      y := single (floor * 16 - kind.height), health := kind.health }

private def validPosition (x y : Float32) (world : World) : Bool := CasterAI.validMotion world x y 0 0
private def validVelocity (value : Float32) : Bool := value.isFinite && value.abs ≤ 32

def CavernEnemy.valid (enemy : CavernEnemy) (world : World) : Bool :=
  let clinger := enemy.kind == .clinger
  let bat := enemy.kind == .illuminantBat
  let gross := enemy.kind == .floatyGross
  enemy.id > 0 && 0 < enemy.health && enemy.health ≤ enemy.kind.health &&
    validPosition enemy.x enemy.y world && validPosition enemy.lastJumpX 0 world &&
    validPosition enemy.hoverAnchorX enemy.hoverAnchorY world &&
    validVelocity enemy.vx && validVelocity enemy.vy && validVelocity enemy.oldVx && validVelocity enemy.oldVy &&
    enemy.rotation.isFinite && enemy.rotation.abs ≤ 6.29 &&
    0 < enemy.timeLeft && enemy.timeLeft ≤ 937 && enemy.direction.natAbs ≤ 1 && enemy.directionY.natAbs ≤ 1 &&
    enemy.spriteDirection.natAbs == 1 && enemy.frame < enemy.kind.frameCount &&
    enemy.frameClock < (if clinger then 7 else if bat || gross then 6 else 8) &&
    enemy.retargetClock ≤ 200 && enemy.searchClock ≤ 1000 && -300 ≤ enemy.orbitClock && enemy.orbitClock ≤ 300 &&
    (if clinger then
      0 ≤ enemy.jumpClock && enemy.jumpClock ≤ 450 && enemy.attackClock < 120 &&
        enemy.anchor.any (fun anchor => world.contains anchor)
    else enemy.attackClock == 0 && enemy.anchor.isNone &&
      (if gross then -200 ≤ enemy.jumpClock && enemy.jumpClock < 60
       else -2120 ≤ enemy.jumpClock && enemy.jumpClock ≤ 0))

def CavernEnemy.knockback (enemy : CavernEnemy) (damage : Nat) (direction : Int)
    (force : Float32) (crit := false) : CavernEnemy :=
  let lift := if enemy.kind == .illuminantSlime then 0.75 else 0.5
  let (vx, vy) := npcKnockbackVelocity enemy.vx enemy.vy force enemy.kind.resistance lift
    damage enemy.kind.health direction crit
  { enemy with vx, vy, justHit := true }

def CavernFlame.hitBox (flame : CavernFlame) : HitBox :=
  CasterAI.npcBox (flame.x - 8) (flame.y - 8) 16 16

private def CavernFlame.validMotion (flame : CavernFlame) (world : World) : Bool :=
  validPosition flame.x flame.y world && flame.vx.isFinite && flame.vy.isFinite &&
    flame.vx.abs ≤ 10.00001 && flame.vy.abs ≤ 10.00001 &&
    flame.clock + flame.life == cavernFlameLife && flame.slot.all (· < maxProjectileSlots)

def CavernFlame.valid (flame : CavernFlame) (world : World) : Bool :=
  0 < flame.life && flame.validMotion world

def CavernFlame.cursedInfernoTicks (roll : Nat → Nat) : Nat :=
  if roll 0 % 6 == 0 || roll 1 % 4 == 0 then 180 else if roll 2 % 2 == 0 then 120 else 0

def CavernFlame.advance (flame : CavernFlame) (world : World) (water magma : Water) : Option CavernFlame := do
  if flame.life == 0 then failure
  let moved := moveNpc world water magma (flame.x - 8) (flame.y - 8) flame.vx flame.vy 16 16 false false
  if moved.collideX || moved.collideY then failure
  let next := { flame with x := moved.x + 8, y := moved.y + 8, clock := flame.clock + 1, life := flame.life - 1 }
  -- Projectile.Damage precedes the final timeLeft decrement.
  if next.validMotion world then some next else none

private def CavernEnemy.face (enemy : CavernEnemy) (player : Player) (dead : Bool) : CavernEnemy :=
  if dead then enemy else
    let x := single (player.x.tdiv 16 + playerWidth / 32)
    let y := single (player.y.tdiv 16 + playerHeight / 32)
    { enemy with
      direction := if x < enemy.x + single (enemy.kind.width / 2 : Nat) then -1 else 1
      directionY := if y < enemy.y + single (enemy.kind.height / 2 : Nat) then -1 else 1 }

/-- Illuminant Slime uses the ordinary wet and jump branches of style 1,
with two additional counts per grounded tick. -/
def CavernEnemy.advanceSlimeAI (enemy : CavernEnemy) (player : Player) (dead hostile : Bool)
    (embedded : CavernEnemy → Bool) : CavernEnemy × Bool := Id.run do
  let mut next := if enemy.retargetClock > 1 then { enemy with retargetClock := enemy.retargetClock - 1 }
    else enemy
  if next.wet then
    let vy : Float32 := if next.collideY then -2 else next.vy
    if vy < 0 && next.lastJumpX == next.x then
      next := { next with direction := -next.direction, retargetClock := 200 }
    let lastJumpX := if vy > 0 then next.x else next.lastJumpX
    let buoyant := if vy > 2 then vy * 0.9 else vy
    next := { next with vy := max (-4) (buoyant - 0.5), lastJumpX }
    if next.retargetClock == 1 && hostile then next := next.face player dead
  if next.retargetClock == 0 then
    next := { (next.face player dead) with retargetClock := 1, jumpClock := -100 }
  if next.vy == 0 then
    if next.collideY && next.oldVy != 0 && embedded next then
      next := { next with x := next.x - (next.vx + single next.direction) }
    if next.lastJumpX == next.x then
      next := { next with direction := -next.direction, retargetClock := 200 }
    let slowed := next.vx * 0.8
    let vx := if -0.1 < slowed.toFloat && slowed.toFloat < 0.1 then 0 else slowed
    let jumpClock := next.jumpClock + if hostile then 4 else 3
    next := { next with vx, jumpClock, lastJumpX := 0 }
    let jump := if jumpClock ≥ 0 then 1 else if -1000 ≤ jumpClock && jumpClock ≤ -500 then 2
      else if -2000 ≤ jumpClock && jumpClock ≤ -1500 then 3 else 0
    if jump == 0 then return (next, jumpClock ≥ -30)
    if next.retargetClock == 1 && hostile then next := next.face player dead
    let side := single next.direction
    if jump == 3 then
      return ({ next with vx := next.vx + 3 * side, vy := -8, jumpClock := -200, lastJumpX := next.x }, false)
    return ({ next with vx := next.vx + 2 * side, vy := -6,
                        jumpClock := if jump == 1 then -1120 else -2120 }, false)
  let side := single next.direction
  if next.direction != 0 && side * next.vx < 3 then
    if next.collideX && next.vx.abs == 0.2 then next := { next with x := next.x - 1.4 * side }
    if next.collideY && next.oldVy != 0 && embedded next then next := { next with x := next.x - (next.vx + side) }
    let vx := if side.toFloat * next.vx.toFloat > -0.01 then next.vx + 0.2 * side else next.vx * 0.93
    next := { next with vx }
  return (next, false)

def CavernEnemy.advanceBatAI (enemy : CavernEnemy) (player : Player)
    (dead lineOfSight targetWet : Bool) : CavernEnemy :=
  let mover : UnderworldFlyer := {
    id := enemy.id, kind := .hellbat, x := enemy.x, y := enemy.y
    vx := enemy.vx, vy := enemy.vy, oldVx := enemy.oldVx, oldVy := enemy.oldVy,
    health := enemy.health, direction := enemy.direction, directionY := enemy.directionY,
    collideX := enemy.collideX, collideY := enemy.collideY, wet := enemy.wet,
    searchClock := enemy.searchClock, orbitClock := enemy.orbitClock }
  let next := mover.advanceAI player dead lineOfSight targetWet (illuminant := true)
  { enemy with
    vx := next.vx, vy := next.vy, direction := next.direction, directionY := next.directionY
    searchClock := next.searchClock, orbitClock := next.orbitClock }

def CavernEnemy.advanceGrossAI (enemy : CavernEnemy) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) : CavernEnemy :=
  let mover : Wraith := {
    id := enemy.id, x := enemy.x, y := enemy.y, vx := enemy.vx, vy := enemy.vy
    anchorX := enemy.hoverAnchorX, anchorY := enemy.hoverAnchorY, stuckClock := enemy.jumpClock,
    direction := enemy.direction, directionY := enemy.directionY, justHit := enemy.justHit }
  let next := mover.advanceAI world water magma player dead (height := enemy.kind.height)
  { enemy with
    vx := next.vx, vy := next.vy, direction := next.direction, directionY := next.directionY
    hoverAnchorX := next.anchorX, hoverAnchorY := next.anchorY, jumpClock := next.stuckClock, justHit := false }

private def tetherAxis (position velocity target offset : Float32) : Float32 :=
  let acceleration : Float32 := 0.035
  if position < target then
    let next := velocity + acceleration
    if next < 0 && offset > 0 then next + acceleration * 1.5 else next
  else if position > target then
    let next := velocity - acceleration
    if next > 0 && offset < 0 then next - acceleration * 1.5 else next
  else velocity

private def clingerRebound (velocity : Float32) : Float32 :=
  let rebound := velocity * -0.7
  if 0 < rebound && rebound < 2 then 2 else if -2 < rebound && rebound < 0 then -2 else rebound

/-- Style 13 tethers the body to a solid tile and retries a blocked cast
after 20 ticks. Projectile aim jitter is two ordered native integer draws. -/
def CavernEnemy.advanceClingerAI (enemy : CavernEnemy) (anchor : TilePos) (player : Player)
    (dead clearSight : Bool) (roll : Nat → Nat) : CavernEnemy × Option CavernFlame := Id.run do
  let next := enemy.face player dead
  let jumpClock := next.jumpClock + 1
  let range : Float32 := if jumpClock > 300 then 227 else 175
  let ax := single (anchor.x * 16 + 8)
  let ay := single (anchor.y * 16 + 8)
  let px := pixels player.x + single (playerWidth / 16) * 0.5
  let py := pixels player.y + single (playerHeight / 16) * 0.5
  let dx := px - 15 - ax
  let dy := py - 15 - ay
  let distance := length dx dy
  let scale := if distance > range then range / distance else 1
  let dx := dx * scale
  let dy := dy * scale
  let vx := if next.collideX then clingerRebound next.oldVx else clamp (tetherAxis next.x next.vx (ax + dx) dx) (-2) 2
  let vy := if next.collideY then clingerRebound next.oldVy else clamp (tetherAxis next.y next.vy (ay + dy) dy) (-2) 2
  let rotation := if dx == 0 then next.rotation else
    (Float.atan2 dy.toFloat dx.toFloat).toFloat32 + (if dx < 0 then 3.14 else 0)
  let spriteDirection := if dx < 0 then -1 else if dx > 0 then 1 else next.spriteDirection
  let clock := if dead then next.attackClock else if next.justHit then 1 else next.attackClock + 1
  let shoot := !dead && clock ≥ 120
  let clock := if !shoot then clock else if clearSight then 0 else 100
  let next := { next with
    vx, vy, rotation, spriteDirection, attackClock := clock
    jumpClock := if jumpClock > 450 then 0 else jumpClock }
  if !shoot || !clearSight then return (next, none)
  let dx := px - next.centerX + single ((roll 0 % 21 : Nat) - (10 : Int))
  let dy := py - next.centerY + single ((roll 1 % 21 : Nat) - (10 : Int))
  let distance := length dx dy
  let shot := if distance == 0 then none else
    some { x := next.centerX, y := next.centerY, vx := dx * (10 / distance), vy := dy * (10 / distance) }
  return (next, shot)

private def CavernEnemy.animateFlyer (enemy : CavernEnemy) : CavernEnemy :=
  let clock := (enemy.frameClock + 1) % 6
  let frame := if clock == 0 then (enemy.frame + 1) % enemy.kind.frameCount else enemy.frame
  let spriteDirection := if enemy.vx < 0 then -1 else if enemy.vx > 0 then 1 else enemy.spriteDirection
  { enemy with frame, frameClock := clock, spriteDirection, rotation := enemy.vx * 0.1 }

private def CavernEnemy.checkActive (enemy : CavernEnemy) (player : Player) : Option CavernEnemy :=
  (npcActiveTime player enemy.x enemy.y enemy.kind.width enemy.kind.height enemy.timeLeft).map
    fun timeLeft => { enemy with timeLeft }

def CavernEnemy.advance (enemy : CavernEnemy) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) (surface : Nat) (roll : Nat → Nat := fun _ => 10) :
    Option CavernEnemy × Option CavernFlame := Id.run do
  if enemy.health == 0 then return (none, none)
  let kind := enemy.kind
  let sight := fun () => npcCanSee world enemy.x enemy.y kind.width kind.height player
  if kind == .floatyGross then
    let next := enemy.advanceGrossAI world water magma player dead
    let vx := snap next.vx
    let moved := { next with x := next.x + vx, y := next.y + next.vy, vx }
    return (moved.animateFlyer.checkActive player, none)
  if kind == .clinger then
    let some anchor := enemy.anchor | return (none, none)
    if world.tile anchor == .air then return (none, none)
    let dx := enemy.centerX.toInt32.toInt - (pixels player.x + single (playerWidth / 16) * 0.5).toInt32.toInt
    let dy := enemy.centerY.toInt32.toInt - (pixels player.y + single (playerHeight / 16) * 0.5).toInt32.toInt
    let inRange := -910 ≤ dx && dx < 910 && -550 ≤ dy && dy < 550
    let clearSight := !dead && !enemy.justHit && enemy.attackClock + 1 ≥ 120 &&
      inRange && enemy.clear world && sight ()
    let (next, shot) := enemy.advanceClingerAI anchor player dead clearSight roll
    let vx := snap next.vx
    let clock := (next.frameClock + 1) % 7
    let moved := { next with x := next.x + vx, y := next.y + next.vy, vx,
                             frame := if clock == 0 then (next.frame + 2) % 4 else next.frame,
                             frameClock := clock, justHit := false }
    return (moved.checkActive player, shot)
  let bat := kind == .illuminantBat
  let (next, crouching) := if bat then
      (enemy.advanceBatAI player dead (enemy.searchClock ≥ 200 && sight ())
        (enemy.searchClock ≥ 200 && underworldTargetWet world water magma player), false)
    else enemy.advanceSlimeAI player dead true (fun actor => !actor.clear world)
  let vx := snap next.vx
  let widthRatio := single world.width / 4200
  let depth := ((enemy.y / 16 - (60 + 10 * (widthRatio * widthRatio))).toFloat / (surface.toFloat / 6)).toFloat32
  let gravity := if enemy.wet then 0.2 else 0.3 * max 0.25 (min 1 depth)
  let vy := if bat then next.vy else min (if enemy.wet then 7 else 10) (next.vy + gravity)
  let moved := moveNpc world water magma next.x next.y vx vy kind.width kind.height enemy.wet (!bat)
  let next := { next with
    x := moved.x, y := moved.y, vx := moved.vx, vy := moved.vy
    oldVx := moved.oldVx, oldVy := moved.oldVy, collideX := moved.collideX, collideY := moved.collideY,
    wet := moved.wet, justHit := false }
  if bat then return (next.animateFlyer.checkActive player, none)
  let clock := next.frameClock + if crouching then 3 else if moved.vx != 0 || moved.vy != 0 then 2 else 1
  let next := { next with
    frameClock := if clock ≥ 8 then 0 else clock
    frame := if clock ≥ 8 then (next.frame + 1) % kind.frameCount else next.frame }
  return (next.checkActive player, none)

end Lean4Terria
