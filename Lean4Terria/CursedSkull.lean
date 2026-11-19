import Lean4Terria.Ranged

namespace Lean4Terria

def cursedSkullHealth : Nat := 40
def cursedSkullDefense : Nat := 6
def cursedSkullDamage : Nat := 35
def cursedSkullWidth : Nat := 26
def cursedSkullHeight : Nat := 28
def cursedSkullCoins : Nat := 150
def cursedSkullSlotsQuarters : Nat := 3
def cursedSkullNpcSlots : Float32 := 0.75
def cursedSkullFrameCount : Nat := 3

/-- Type 34's original NPC motion, using pixels and Single precision. The
ordinary Classic branch uses ai[0] for orbit and ai[1] for the lunge clock. -/
structure CursedSkull where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  orbit : Float32 := 0
  clock : Nat := 0
  health : Nat := cursedSkullHealth
  timeLeft : Nat := 937
  frame : Nat := 0
  frameClock : Nat := 0
  direction : Int := 1
  spriteDirection : Int := -1
  deriving Repr, Inhabited

instance : BEq CursedSkull where
  beq a b :=
    a.id == b.id && a.health == b.health && a.timeLeft == b.timeLeft &&
    a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.orbit.toBits == b.orbit.toBits &&
    a.clock == b.clock && a.frame == b.frame && a.frameClock == b.frameClock &&
    a.direction == b.direction && a.spriteDirection == b.spriteDirection

private def asFloat (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (units : Int) : Float32 := asFloat units / 16
private def truncate (value : Float32) : Int := value.toInt32.toInt

def CursedSkull.centerX (skull : CursedSkull) : Float32 := skull.x + 13
def CursedSkull.centerY (skull : CursedSkull) : Float32 := skull.y + 14

def CursedSkull.atFloor (id : Nat) (column floor : Int) : CursedSkull :=
  { id, x := asFloat (column * 16 - 5), y := asFloat (floor * 16 - 28) }

def CursedSkull.hitBox (skull : CursedSkull) : HitBox :=
  let left := truncate skull.x * 16
  let top := truncate skull.y * 16
  ⟨left, top, left + 16 * cursedSkullWidth, top + 16 * cursedSkullHeight⟩

def CursedSkull.contact (skull : CursedSkull) (player : Player) : Option Int :=
  if skull.hitBox.intersects (HitBox.ofNpcTarget player) then
    some (if skull.centerX < pixels player.x + asFloat (playerWidth / 32) then 1 else -1)
  else none

def CursedSkull.valid (skull : CursedSkull) (world : World) : Bool :=
  let margin : Float32 := 4096
  let validPosition := fun (position : Float32) (tiles : Nat) =>
    position.isFinite && -margin ≤ position && position < asFloat ((tiles : Int) * 16) + margin
  skull.id > 0 && skull.health > 0 && skull.health ≤ cursedSkullHealth &&
    validPosition skull.x world.width && validPosition skull.y world.height &&
    skull.vx.isFinite && skull.vy.isFinite && skull.vx.abs ≤ 32 && skull.vy.abs ≤ 32 &&
    skull.orbit.isFinite && -200 ≤ skull.orbit && skull.orbit ≤ 200 &&
    skull.rotation.isFinite && skull.rotation.abs ≤ 6.283186 &&
    skull.clock ≤ 650 && skull.timeLeft > 0 && skull.timeLeft ≤ 937 &&
    skull.frame < cursedSkullFrameCount && skull.frameClock < 4 &&
    (skull.direction == -1 || skull.direction == 1) &&
    (skull.spriteDirection == -1 || skull.spriteDirection == 1)

/-- Original NPC.StrikeNPC_Inner knockback for a no-gravity NPC with resistance
0.2. The small-hit branch applies resistance again to both components. -/
def CursedSkull.knockback (skull : CursedSkull) (damage : Nat) (direction : Int)
    (force : Float32) (crit := false) : CursedSkull :=
  if force ≤ 0 then skull else
  let resist : Float32 := 0.2
  let reduce := fun (value cap coefficient : Float32) =>
    if value > cap then cap + (value - cap) * coefficient else value
  let impulse := reduce (reduce (reduce (reduce (force * resist) 8 0.9) 10 0.8) 12 0.7) 14 0.6
  let impulse := min impulse 16
  let impulse := if crit then impulse * 1.4 else impulse
  let upward := -impulse * 0.5
  let side := asFloat direction
  if damage * 10 ≤ cursedSkullHealth then
    { skull with vx := impulse * side * resist, vy := upward * resist }
  else
    let vx := if direction != 0 && side * skull.vx < impulse then
        let vx := if side * skull.vx < 0 then skull.vx + side * impulse else skull.vx
        side * min impulse (side * (vx + side * impulse))
      else skull.vx
    let vy := if skull.vy > upward then max upward (skull.vy + upward) else skull.vy
    { skull with vx, vy }

private def steer (current desired acceleration : Float32) : Float32 :=
  if current < desired then current + acceleration
  else if current > desired then current - acceleration
  else current

/-- Ordinary type 34 uses the common style-10 branch. Distances above 250
override the lunge speed and acceleration. An exact center overlap retains
the orbit-adjusted velocity until a nonzero target direction is available. -/
def CursedSkull.advanceAI (skull : CursedSkull) (player : Player) (dead : Bool) : CursedSkull := Id.run do
  let dx := pixels (bodyCenterX player) - skull.centerX
  let dy := pixels (bodyCenterY player) - skull.centerY
  let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let clock := skull.clock + 1
  let lunging := clock > 600
  let direction := if dead then skull.direction
    else if asFloat (player.x.tdiv 16 + playerWidth / 32) < skull.centerX then -1 else 1
  let mut next := { skull with clock := if clock > 650 then 0 else clock, direction }
  if !lunging && distance < 250 then
    let orbit := next.orbit + 0.9
    next := { next with
      orbit := if orbit > 200 then -200 else orbit
      vx := if orbit < -100 || orbit > 100 then next.vx + 0.019 else next.vx - 0.019
      vy := if orbit > 0 then next.vy + 0.019 else next.vy - 0.019 }
  let (speed, acceleration) : Float32 × Float32 :=
    if distance > 350 then (5, 0.3)
    else if distance > 300 then (3, 0.2)
    else if distance > 250 then (1.5, 0.1)
    else if lunging then (4, 0.011 * 8)
    else (1, 0.011)
  let scale := if distance == 0 then 0 else speed / distance
  let targetX := dx * scale
  let targetY := dy * scale
  if dead || distance != 0 then
    let desiredX := if dead then asFloat direction * speed / 2 else targetX
    let desiredY := if dead then -speed / 2 else targetY
    next := { next with vx := steer next.vx desiredX acceleration, vy := steer next.vy desiredY acceleration }
  if targetX > 0 then
    next := { next with spriteDirection := -1, rotation := (Float.atan2 targetY.toFloat targetX.toFloat).toFloat32 }
  else if targetX < 0 then
    let angle := (Float.atan2 targetY.toFloat targetX.toFloat).toFloat32
    next := { next with spriteDirection := 1, rotation := angle + 3.1415927410125732 }
  return next

private def CursedSkull.checkActive (skull : CursedSkull) (player : Player) : Option CursedSkull := do
  let target := HitBox.ofNpcTarget player
  let countLeft := truncate (skull.centerX - 4032) * 16
  let countTop := truncate (skull.centerY - 2520) * 16
  let countArea : HitBox := ⟨countLeft, countTop, countLeft + 8064 * 16, countTop + 5040 * 16⟩
  if !countArea.intersects target then failure
  let viewLeft := (skull.centerX.toFloat - 986).toInt64.toInt * 16
  let viewTop := (skull.centerY.toFloat - 628).toInt64.toInt * 16
  let view : HitBox := ⟨viewLeft, viewTop, viewLeft + 1972 * 16, viewTop + 1256 * 16⟩
  let timeLeft := (if view.intersects target then 750 else skull.timeLeft) - 1
  if timeLeft == 0 then failure
  return { skull with timeLeft }

/-- Style 10, generic no-gravity/no-tile-collision motion, FindFrame, then
CheckActive, in the same order as NPC.UpdateNPC. -/
def CursedSkull.advance (skull : CursedSkull) (player : Player) (dead : Bool) : Option CursedSkull := do
  if skull.health == 0 then failure
  let next := skull.advanceAI player dead
  let vx := if -0.005 < next.vx.toFloat && next.vx.toFloat < 0.005 then 0 else next.vx
  let frameClock := (next.frameClock + 1) % 4
  let frame := if frameClock == 0 then (next.frame + 1) % cursedSkullFrameCount else next.frame
  ({ next with x := next.x + vx, y := next.y + next.vy, vx, frame, frameClock } : CursedSkull).checkActive player

end Lean4Terria
