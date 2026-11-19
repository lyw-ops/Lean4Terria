import Lean4Terria.UnderworldShots

namespace Lean4Terria

inductive PrimeAttackKind where
  | bomb | laser
  deriving BEq, Repr, Inhabited

def PrimeAttackKind.sprite : PrimeAttackKind → Nat
  | .bomb => 102
  | .laser => 100

def PrimeAttackKind.updates : PrimeAttackKind → Nat
  | .bomb => 1
  | .laser => 3

def PrimeAttackKind.lifetime : PrimeAttackKind → Nat
  | .bomb => 3600
  | .laser => 2700

def PrimeAttackKind.size : PrimeAttackKind → Nat
  | .bomb => 22
  | .laser => 7

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32

private def PrimeAttackKind.halfSize (kind : PrimeAttackKind) : Float32 := single kind.size / 2

/-- Position at launch is the center passed to the native projectile constructor.
Damage is the value before the hostile projectile multiplier. -/
structure PrimeAttack where
  kind : PrimeAttackKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  deriving Repr, Inhabited

/-- Positions are native top-left pixels. Age and lifetime count subupdates. -/
structure PrimeShot where
  kind : PrimeAttackKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  life : Nat
  rotation : Float32 := 0
  age : Nat := 0
  slot : Option Nat := none
  honeyWet : Bool := false
  deriving Repr

instance : BEq PrimeShot where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.damage == b.damage && a.life == b.life && a.age == b.age && a.slot == b.slot && a.honeyWet == b.honeyWet

def PrimeShot.centerX (shot : PrimeShot) : Float32 := shot.x + shot.kind.halfSize

def PrimeShot.centerY (shot : PrimeShot) : Float32 := shot.y + shot.kind.halfSize

def PrimeShot.frame (shot : PrimeShot) : Nat :=
  if shot.kind == .bomb then shot.age / 4 % 2 else 0

def PrimeShot.alpha (shot : PrimeShot) : Nat :=
  if shot.kind == .laser then 255 - 15 * shot.age else 0

def PrimeShot.fire (attack : PrimeAttack) (slot : Nat) : PrimeShot :=
  let half := attack.kind.halfSize
  { kind := attack.kind, x := attack.x - half, y := attack.y - half, vx := attack.vx, vy := attack.vy
    damage := attack.damage, life := attack.kind.lifetime, slot := some slot }

private def PrimeShot.inWorld (shot : PrimeShot) (world : World) : Bool :=
  shot.x.isFinite && shot.y.isFinite && 0 < shot.x && 0 < shot.y &&
    shot.x + 2 * shot.kind.halfSize < single (world.width * 16) &&
    shot.y + 2 * shot.kind.halfSize < single (world.height * 16)

def PrimeShot.valid (shot : PrimeShot) (world : World) : Bool :=
  shot.inWorld world && shot.vx.isFinite && shot.vy.isFinite && shot.rotation.isFinite &&
    shot.vx.abs ≤ 13 && shot.vy.abs ≤ 13 &&
    shot.life > 0 && shot.age + shot.life == shot.kind.lifetime &&
    shot.slot.any (· < maxProjectileSlots) &&
    shot.damage == (if shot.kind == .bomb then 0 else 25)

private def projectileBox (x y : Float32) (size : Nat) : HitBox :=
  let left := x.toInt32.toInt * 16
  let top := y.toInt32.toInt * 16
  ⟨left, top, left + 16 * (size : Int), top + 16 * (size : Int)⟩

structure PrimeShotStep where
  next : Option PrimeShot := none
  hit : Option HitBox := none
  damage : Nat := 0
  detonated : Bool := false
  launchSound : Bool := false
  impactSound : Bool := false
  deriving Repr

private def PrimeShot.detonate (shot : PrimeShot) (launchSound : Bool) : PrimeShotStep :=
  { hit := some (projectileBox (shot.centerX - 64) (shot.centerY - 64) 128)
    damage := 40, detonated := true, launchSound }

private def PrimeShot.onPlatform (shot : PrimeShot) (world : World) : Bool :=
  let tile : TilePos := ⟨(shot.centerX / 16).toInt32.toInt, (shot.centerY / 16).toInt32.toInt⟩
  world.tile tile == .platform

private def tileIndex (pixel : Float32) : Int := (pixel / 16).toInt32.toInt

private def PrimeShot.wetContact (shot : PrimeShot) (world : World) (water magma honey : Water) :
    Option Bool := Id.run do
  let size := shot.kind.size
  let width := min 10 size
  let height := size / 2
  let left := shot.x + single (size / 2 - width / 2 : Nat)
  let top := shot.y + single (size / 2 - height / 2 : Nat)
  let right := left + single width
  let bottom := top + single height
  let firstX := tileIndex left
  let firstY := tileIndex top - 1
  for column in [:(tileIndex right - firstX + 1).toNat] do
    for row in [:(tileIndex bottom - firstY + 1).toNat] do
      let pos : TilePos := ⟨firstX + column, firstY + row⟩
      let honeyLevel := honey.level world pos
      let level := max honeyLevel (max (water.level world pos) (magma.level world pos))
      if level == 0 then continue
      let tx := single (pos.x * 16)
      let offset := single (256 - level : Nat) / 16
      let surface := single (pos.y * 16) + offset
      let depth := 16 - single offset.toInt32.toInt
      if right > tx && left < tx + 16 && bottom > surface && top < surface + depth then
        return some (honeyLevel > 0)
  return none

private def PrimeShot.wetMotion (shot : PrimeShot) (world : World) (water magma honey : Water) :
    Float32 × Bool :=
  match shot.wetContact world water magma honey with
  | none => (1, false)
  | some touchesHoney =>
    let honeyWet := touchesHoney || shot.honeyWet
    (if honeyWet then 0.25 else 0.5, honeyWet)

private def laserRotation (vx vy : Float32) : Float32 :=
  (Float.atan2 vy.toFloat vx.toFloat).toFloat32 + 1.57

/-- One projectile update. Native cannon bombs are harmless before they resize
for their blast. A floor collision can stop the bomb's vertical movement,
which triggers the explosion at the start of the following AI update. -/
def PrimeShot.advanceSubstep (shot : PrimeShot) (world : World) (water magma honey : Water) :
    PrimeShotStep := Id.run do
  if shot.life == 0 || !shot.inWorld world then return {}
  let bomb := shot.kind == .bomb
  if bomb && shot.onPlatform world then return shot.detonate false
  let launchSound := shot.age == 0
  if bomb && (shot.life ≤ 3 || shot.vy == 0) then return shot.detonate launchSound

  let vx := shot.vx
  let vy := if bomb then min shot.vy 10 + (if shot.age ≥ 5 then 0.2 else 0) else shot.vy
  let rotation := if bomb then shot.rotation + vx * 0.1 else laserRotation vx vy
  let half := shot.kind.halfSize
  let (dx, dy) := clipShotMotion world shot.centerX shot.centerY vx vy half false
  let blockedX := dx != vx
  let blockedY := dy != vy
  if !bomb && (blockedX || blockedY) then return { launchSound, impactSound := true }
  let dy := if blockedY && vy < 0 then dy + 0.01 else dy

  let (speed, honeyWet) := shot.wetMotion world water magma honey
  let nextVx := if blockedX then vx * (-0.4) else dx
  let moveX := if speed == 1 then nextVx else if blockedX then dx else dx * speed
  let moveY := if blockedY then dy else dy * speed
  let moved := { shot with
    x := shot.x + moveX
    y := shot.y + moveY
    vx := nextVx
    vy := dy
    rotation
    honeyWet
    age := shot.age + 1
    life := shot.life - 1 }
  let next := if moved.life > 0 && moved.inWorld world then some moved else none
  let hit := if bomb then none else some (projectileBox moved.x moved.y shot.kind.size)
  return { next, hit, damage := shot.damage, launchSound, impactSound := !bomb && moved.life == 0 }

end Lean4Terria
