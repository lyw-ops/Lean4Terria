import Lean4Terria.Twins
import Lean4Terria.UnderworldShots

namespace Lean4Terria

def maxTwinShots : Nat := 128

def TwinAttackKind.isLaser : TwinAttackKind → Bool
  | .eyeLaser | .deathLaser | .destroyerLaser | .probeLaser => true
  | _ => false

def TwinAttackKind.updates : TwinAttackKind → Nat
  | .eyeLaser | .deathLaser | .destroyerLaser | .probeLaser => 3
  | .cursedFlame => 1
  | .eyeFire => 4

def TwinAttackKind.lifetime : TwinAttackKind → Nat
  | .eyeLaser | .probeLaser => 600
  | .destroyerLaser => 300
  | .deathLaser => 2700
  | .cursedFlame => 3600
  | .eyeFire => 60

def TwinAttackKind.halfSize : TwinAttackKind → Nat
  | .eyeLaser | .deathLaser | .destroyerLaser | .probeLaser => 2
  | .cursedFlame => 8
  | .eyeFire => 3

def TwinAttackKind.sprite : TwinAttackKind → Nat
  | .eyeLaser => 83
  | .deathLaser | .destroyerLaser => 100
  | .probeLaser => 84
  | .cursedFlame => 96
  | .eyeFire => 101

/-- The hostile projectiles use a shared NPC-to-player damage path.
`life` and `age` are measured in projectile updates. -/
structure TwinShot where
  kind : TwinAttackKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  life : Nat
  age : Nat := 0
  slot : Option Nat := none
  penetration : Nat := 0
  deriving Repr, Inhabited

instance : BEq TwinShot where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.damage == b.damage &&
    a.life == b.life && a.age == b.age && a.slot == b.slot && a.penetration == b.penetration

def TwinShot.fire (attack : TwinAttack) (slot : Nat) : TwinShot :=
  { kind := attack.kind, x := attack.x, y := attack.y, vx := attack.vx, vy := attack.vy,
    damage := attack.damage, life := attack.kind.lifetime,
    slot := some slot, penetration := if attack.kind.isLaser then 3 else 0 }

def TwinShot.hitBox (shot : TwinShot) : HitBox :=
  let half := shot.kind.halfSize
  let n := (Int32.ofNat half).toFloat32
  let x := (shot.x - n).toInt32.toInt * 16
  let y := (shot.y - n).toInt32.toInt * 16
  ⟨x, y, x + 32 * half, y + 32 * half⟩

private def TwinShot.inBounds (shot : TwinShot) (world : World) : Bool :=
  shot.x.isFinite && shot.y.isFinite && -16 ≤ shot.x && -16 ≤ shot.y &&
    shot.x < (Int32.ofNat (world.width * 16 + 16)).toFloat32 &&
    shot.y < (Int32.ofNat (world.height * 16 + 16)).toFloat32

def TwinShot.valid (shot : TwinShot) (world : World) : Bool :=
  shot.inBounds world && shot.vx.isFinite && shot.vy.isFinite &&
    shot.vx.abs ≤ 14 && shot.vy.abs ≤ 14 &&
    shot.life > 0 && shot.age + shot.life == shot.kind.lifetime &&
    shot.slot.any (· < maxProjectileSlots) &&
    (if shot.kind.isLaser then 0 < shot.penetration && shot.penetration ≤ 3 else shot.penetration == 0) &&
    match shot.kind with
    | .eyeLaser => shot.damage == 20
    | .deathLaser => shot.damage == 18 || shot.damage == 25
    | .destroyerLaser => shot.damage == 22
    | .probeLaser => shot.damage == 25
    | .cursedFlame => shot.damage == 25
    | .eyeFire => shot.damage == 30

/-- Tile collision is resolved before player collision. The last subupdate
can still hit, since the native lifetime decrement happens after Damage. -/
def TwinShot.advanceSubstep (shot : TwinShot) (world : World) (water magma : Water) : Option TwinShot := do
  if shot.life == 0 then failure
  let half := (Int32.ofNat shot.kind.halfSize).toFloat32
  let (dx, dy) := clipShotMotion world shot.x shot.y shot.vx shot.vy half false
  if dx != shot.vx || dy != shot.vy then failure
  let wetHalfHeight := max 1 (half / 2)
  let wet := shotLiquidContact world water shot.x shot.y half wetHalfHeight ||
    shotLiquidContact world magma shot.x shot.y half wetHalfHeight
  let speed : Float32 := if wet then 0.5 else 1
  let moved := { shot with
    x := shot.x + dx * speed, y := shot.y + dy * speed
    life := shot.life - 1, age := shot.age + 1 }
  if moved.inBounds world then some moved else none

end Lean4Terria
