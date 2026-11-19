import Lean4Terria.Ranged
import Lean4Terria.Equipment

namespace Lean4Terria

def eyeMaxHealth : Nat := 2800
def eyeSecondFormThreshold : Nat := eyeMaxHealth / 2
def eyeServantMaxHealth : Nat := 8
def eyeServantContactDamage : Nat := 12
def eyeMaxServants : Nat := 24
def eyeServantInterval : Nat := 110
def eyeChargeCruiseTicks : Nat := 40
def eyeMaxSpeed : Int := 120
def eyeServantMaxSpeed : Int := 80

-- One Terraria pixel is sixteen units in the tile coordinate system.
private def npcPixel : Int := tileUnits / 16

def eyeHalfWidth : Int := 50 * npcPixel
def eyeHalfHeight : Int := 55 * npcPixel
def eyeServantHalfWidth : Int := 10 * npcPixel
def eyeServantHalfHeight : Int := 10 * npcPixel

private def eyeActiveRange : Int := 100 * tileUnits
private def eyeWorldMargin : Int := 64 * tileUnits

structure EyeMotion where
  /-- Center of the collision box in world subpixels. -/
  x : Int
  y : Int
  vx : Int
  vy : Int
  deriving BEq, Repr, Inhabited

inductive EyePhase where
  | firstHover | firstCharge | transformSpinUp | transformSpinDown | secondHover | secondCharge | retreat
  deriving BEq, Repr, Inhabited

def EyePhase.duration : EyePhase → Nat
  | .firstHover => 600
  | .firstCharge => 151
  | .transformSpinUp | .transformSpinDown => 100
  | .secondHover => 200
  | .secondCharge => 131
  | .retreat => 10

def EyePhase.charging : EyePhase → Bool
  | .firstCharge | .secondCharge => true
  | _ => false

def EyePhase.transforming : EyePhase → Bool
  | .transformSpinUp | .transformSpinDown => true
  | _ => false

def EyePhase.secondForm : EyePhase → Bool
  | .secondHover | .secondCharge => true
  | _ => false

structure EyeServant where
  motion : EyeMotion
  health : Nat
  id : Nat := 0
  deriving BEq, Repr, Inhabited

structure EyeOfCthulhu where
  motion : EyeMotion
  health : Nat
  phase : EyePhase
  /-- Elapsed ticks in the current phase, including the charge's launch tick. -/
  ticks : Nat
  /-- Charges already completed in the current set of three. -/
  charges : Nat
  servants : Array EyeServant
  /-- Counts only hover ticks above and near enough to the player. -/
  servantTicks : Nat := 0
  nextServantId : Nat := 1
  deriving BEq, Repr, Inhabited

def EyeMotion.valid (motion : EyeMotion) (world : World) (maxSpeed := eyeMaxSpeed) : Bool :=
  -eyeWorldMargin ≤ motion.x && motion.x < world.width * tileUnits + eyeWorldMargin &&
    -eyeWorldMargin ≤ motion.y && motion.y < world.height * tileUnits + eyeWorldMargin &&
    motion.vx.natAbs ≤ maxSpeed.toNat && motion.vy.natAbs ≤ maxSpeed.toNat

def EyeServant.valid (servant : EyeServant) (world : World) : Bool :=
  0 < servant.health && servant.health ≤ eyeServantMaxHealth &&
    servant.motion.valid world eyeServantMaxSpeed

private def EyeOfCthulhu.validServantIds (eye : EyeOfCthulhu) : Bool := Id.run do
  if eye.nextServantId == 0 then return false
  let mut previous := 0
  for servant in eye.servants do
    if servant.id ≤ previous || eye.nextServantId ≤ servant.id then return false
    previous := servant.id
  return true

def EyeOfCthulhu.valid (eye : EyeOfCthulhu) (world : World) : Bool :=
  0 < eye.health && eye.health ≤ eyeMaxHealth && eye.motion.valid world &&
    eye.ticks < eye.phase.duration &&
    (if eye.phase.charging then eye.charges < 3 else eye.charges == 0) &&
    (if eye.phase == .firstHover then eye.servantTicks < eyeServantInterval && eye.servantTicks ≤ eye.ticks
      else eye.servantTicks == 0) &&
    (if eye.phase == .retreat then eye.servants.isEmpty
      else if eye.phase.secondForm || eye.phase.transforming then eye.health < eyeSecondFormThreshold
      else eyeSecondFormThreshold ≤ eye.health) &&
    eye.servants.size ≤ eyeMaxServants && eye.validServantIds && eye.servants.all (·.valid world)

def EyeOfCthulhu.defense (eye : EyeOfCthulhu) : Nat :=
  if eye.phase.secondForm || eye.phase == .retreat then 0 else 12

def EyeOfCthulhu.contactDamage (eye : EyeOfCthulhu) : Nat :=
  if eye.phase == .retreat then 0 else if eye.phase.secondForm then 23 else 15

private def EyeMotion.hitBox (motion : EyeMotion) (halfWidth halfHeight : Int) : HitBox :=
  ⟨motion.x - halfWidth, motion.y - halfHeight, motion.x + halfWidth, motion.y + halfHeight⟩

def EyeOfCthulhu.hitBox (eye : EyeOfCthulhu) : HitBox :=
  eye.motion.hitBox eyeHalfWidth eyeHalfHeight

def EyeServant.hitBox (servant : EyeServant) : HitBox :=
  servant.motion.hitBox eyeServantHalfWidth eyeServantHalfHeight

/-- Raw contact damage for the boss or any servant overlapping the player. -/
def EyeOfCthulhu.contact (eye : EyeOfCthulhu) (player : Player) : Nat :=
  if eye.phase == .retreat then 0
  else if eye.hitBox.overlapsPlayer player then eye.contactDamage
  else if eye.servants.any (fun servant => servant.hitBox.overlapsPlayer player) then
    eyeServantContactDamage
  else 0

private def EyeOfCthulhu.enterPhase (eye : EyeOfCthulhu) (phase : EyePhase)
    (charges := 0) : EyeOfCthulhu :=
  { eye with phase, ticks := 0, charges, servantTicks := 0 }

/-- Apply a raw hit and start the transformation as soon as health drops below half. -/
def EyeOfCthulhu.takeHit (eye : EyeOfCthulhu) (damage : Nat) : EyeOfCthulhu :=
  if damage == 0 || eye.phase == .retreat then eye
  else
    let eye := { eye with health := eye.health - damageAfterDefense damage (eye.defense / 2) }
    if 0 < eye.health && eye.health < eyeSecondFormThreshold &&
        (eye.phase == .firstHover || eye.phase == .firstCharge) then
      eye.enterPhase .transformSpinUp
    else eye

def EyeOfCthulhu.damageServant (eye : EyeOfCthulhu) (index damage : Nat) : EyeOfCthulhu :=
  match eye.servants[index]? with
  | none => eye
  | some servant =>
    let health := servant.health - damage
    let servants := if health == 0 then eye.servants.eraseIdxIfInBounds index
      else eye.servants.set! index { servant with health }
    { eye with servants }

def spawnEye (world : World) (player : Player) : EyeOfCthulhu :=
  let centerX := bodyCenterX player
  let side := if centerX + 8 * tileUnits < world.width * tileUnits then 1 else -1
  { motion := ⟨centerX + side * 6 * tileUnits, bodyCenterY player - 8 * tileUnits, 0, 0⟩
    health := eyeMaxHealth, phase := .firstHover, ticks := 0, charges := 0, servants := #[] }

private def EyeMotion.advance (motion : EyeMotion) : EyeMotion :=
  { motion with x := motion.x + motion.vx, y := motion.y + motion.vy }

private def EyeMotion.near (motion : EyeMotion) (player : Player) : Bool :=
  (motion.x - bodyCenterX player).natAbs ≤ eyeActiveRange.toNat &&
    (motion.y - bodyCenterY player).natAbs ≤ eyeActiveRange.toNat

-- The fractional part is distributed across ticks instead of accumulating an
-- extra fractional velocity field in the save state.
private def accelerationAt (numerator ticks : Nat) : Int :=
  (numerator * (ticks + 1) / 25 - numerator * ticks / 25 : Nat)

private def approachVelocity (current target acceleration : Int) : Int :=
  let step := if current * target < 0 then 2 * acceleration else acceleration
  current + max (-step) (min step (target - current))

private def EyeMotion.steer (motion : EyeMotion) (targetX targetY speed acceleration : Int) : EyeMotion :=
  let (vx, vy) := (aimVelocity speed (targetX - motion.x) (targetY - motion.y)).getD (0, 0)
  { motion with
    vx := approachVelocity motion.vx vx acceleration
    vy := approachVelocity motion.vy vy acceleration }

private def dampComponent (velocity : Int) (percent ticks : Nat) : Int :=
  let product := velocity.natAbs * percent
  let rounded := product / 100 + if ticks * 37 % 100 < product % 100 then 1 else 0
  if rounded ≤ 1 then 0 else if velocity < 0 then -(rounded : Int) else rounded

private def EyeMotion.damp (motion : EyeMotion) (percent ticks : Nat) : EyeMotion :=
  { motion with vx := dampComponent motion.vx percent ticks, vy := dampComponent motion.vy percent ticks }

private def EyeServant.advance (servant : EyeServant) (player : Player) (ticks : Nat) : EyeServant :=
  let motion := servant.motion.steer (bodyCenterX player) (bodyCenterY player)
    eyeServantMaxSpeed (accelerationAt 12 ticks)
  { servant with motion := motion.advance }

private def spawnServant (motion : EyeMotion) (player : Player) (id : Nat) : EyeServant :=
  let (vx, vy) := (aimVelocity eyeServantMaxSpeed (bodyCenterX player - motion.x)
    (bodyCenterY player - motion.y)).getD (0, 0)
  -- Terraria's NPC spawn point denotes the bottom center of the servant.
  { motion := ⟨motion.x + 10 * vx, motion.y + 10 * vy - eyeServantHalfHeight, vx, vy⟩
    health := eyeServantMaxHealth, id }

private def EyeOfCthulhu.hover (eye : EyeOfCthulhu) (player : Player) : EyeOfCthulhu := Id.run do
  let second := eye.phase.secondForm
  let targetX := bodyCenterX player
  let targetY := bodyCenterY player - (if second then 120 else 200) * npcPixel
  let motion := (eye.motion.steer targetX targetY ((if second then 6 else 5) * npcPixel)
    (accelerationAt (if second then 28 else 16) eye.ticks)).advance
  let ticks := eye.ticks + 1
  let updated := { eye with motion, ticks }
  if ticks == eye.phase.duration then
    return updated.enterPhase (if second then .secondCharge else .firstCharge)
  let distance := (eye.motion.x - targetX)^2 + (eye.motion.y - targetY)^2
  if second || eye.motion.y + eyeHalfHeight ≥ player.y || distance ≥ (500 * npcPixel)^2 then
    return updated
  let servantTicks := eye.servantTicks + 1
  if servantTicks < eyeServantInterval then return { updated with servantTicks }
  if eye.servants.size ≥ eyeMaxServants then return { updated with servantTicks := 0 }
  let servant := spawnServant eye.motion player eye.nextServantId
  return { updated with
    servants := eye.servants.push servant, servantTicks := 0, nextServantId := eye.nextServantId + 1 }

private def EyeOfCthulhu.charge (eye : EyeOfCthulhu) (player : Player) : EyeOfCthulhu :=
  let second := eye.phase.secondForm
  let motion := if eye.ticks == 0 then
      let speed := if second then 109 else 6 * npcPixel
      let (vx, vy) := (aimVelocity speed (bodyCenterX player - eye.motion.x)
        (bodyCenterY player - eye.motion.y)).getD (0, 0)
      { eye.motion with vx, vy }
    else if eye.ticks ≥ eyeChargeCruiseTicks then
      eye.motion.damp (if second then 97 else 98) eye.ticks
    else eye.motion
  let updated := { eye with motion := motion.advance, ticks := eye.ticks + 1 }
  if updated.ticks < eye.phase.duration then updated
  else if eye.charges < 2 then updated.enterPhase eye.phase (eye.charges + 1)
  else updated.enterPhase (if second then .secondHover else .firstHover)

private def EyeOfCthulhu.spin (eye : EyeOfCthulhu) : EyeOfCthulhu :=
  let down := eye.phase == .transformSpinDown
  let ticks := eye.ticks + if down then EyePhase.transformSpinUp.duration else 0
  let updated := { eye with motion := (eye.motion.damp 98 ticks).advance, ticks := eye.ticks + 1 }
  if updated.ticks < eye.phase.duration then updated
  else updated.enterPhase (if down then .secondHover else .transformSpinDown)

/-- Advance one Classic tick. The boss and its servants move freely through tiles. -/
def advanceEye (world : World) (player : Player) (eye : EyeOfCthulhu)
    (retreat : Bool) : Option EyeOfCthulhu := do
  if eye.health == 0 then failure
  let eye := if eye.phase != .retreat && (retreat || !eye.motion.near player) then
      ({ eye with servants := #[] } : EyeOfCthulhu).enterPhase .retreat
    else eye
  let next ← if eye.phase == .retreat then
      if eye.ticks + 1 ≥ EyePhase.retreat.duration then none
      else
        let motion := { eye.motion with vy := eye.motion.vy - accelerationAt 16 eye.ticks }
        some { eye with motion := motion.advance, ticks := eye.ticks + 1 }
    else
      let servants := (eye.servants.map (·.advance player eye.ticks)).filter fun servant =>
        servant.motion.valid world eyeServantMaxSpeed && servant.motion.near player
      let eye := { eye with servants }
      match eye.phase with
      | .firstHover | .secondHover => some (eye.hover player)
      | .firstCharge | .secondCharge => some (eye.charge player)
      | .transformSpinUp | .transformSpinDown => some eye.spin
      | .retreat => none
  if next.valid world then some next else none

end Lean4Terria
