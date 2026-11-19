import Lean4Terria.UnderworldShots

namespace Lean4Terria

def demonSickleHostileLife : Nat := 300
def demonSickleFriendlyLife : Nat := 3600
def demonSickleContactDamage : Nat := 42
def demonScytheDamage : Nat := 35
def demonScytheManaCost : Nat := 14
def demonScytheUseTime : Nat := 20
def demonScytheSlowUseTime : Nat := 50

structure DemonSickle where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  rotation : Float32 := 0
  life : Nat
  hits : Nat := 0
  hostile : Bool
  slot : Option Nat := none
  deriving Repr

instance : BEq DemonSickle where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.life == b.life &&
    a.hits == b.hits && a.hostile == b.hostile && a.slot == b.slot

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixel (units : Int) : Float32 := single units / 16

def DemonSickle.fire (x y dx dy : Float32) (facing : Int) (hostile : Bool) : DemonSickle :=
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let (vx, vy) := if length == 0 then (single facing * 0.2, 0) else
    let factor := 0.2 / length
    (dx * factor, dy * factor)
  let life := if hostile then demonSickleHostileLife else demonSickleFriendlyLife
  { x, y, vx, vy, life, hostile }

def DemonSickle.cast (player : Player) (aim : WorldAim) (facing : Int) : DemonSickle :=
  let x := pixel (bodyCenterX player)
  let y := pixel (bodyCenterY player)
  DemonSickle.fire x y (pixel aim.x - x) (pixel aim.y - y) facing false

def DemonSickle.lifeLimit (shot : DemonSickle) : Nat :=
  if shot.hostile then demonSickleHostileLife else demonSickleFriendlyLife

def DemonSickle.hitBox (shot : DemonSickle) : HitBox :=
  let left := (shot.x - 21.5).toInt32.toInt * 16
  let top := (shot.y - 21.5).toInt32.toInt * 16
  ⟨left, top, left + 688, top + 688⟩

def DemonSickle.valid (shot : DemonSickle) (world : World) : Bool :=
  shot.x.isFinite && shot.y.isFinite && shot.vx.isFinite && shot.vy.isFinite && shot.rotation.isFinite &&
    -24 ≤ shot.x && shot.x < single (world.width * 16 + 24 : Nat) &&
    -24 ≤ shot.y && shot.y < single (world.height * 16 + 24 : Nat) &&
    shot.vx.abs ≤ 12 && shot.vy.abs ≤ 12 && shot.rotation.abs ≤ 2881 &&
    0 < shot.life && shot.life ≤ shot.lifeLimit &&
    (if shot.hostile then shot.hits == 0 else shot.hits < 5) && shot.slot.all (· < maxProjectileSlots)

/-- Style 18 rotates before the movement step. Only ages 30 through 99
accelerate. A terrain impact still supplies the contact rectangle for damage. -/
def DemonSickle.advance (shot : DemonSickle) (world : World) (water magma : Water) :
    Option (DemonSickle × Bool) := do
  if shot.life == 0 then failure
  let age := shot.lifeLimit - shot.life + 1
  let acceleration : Float32 := if 30 ≤ age && age < 100 then 1.06 else 1
  let vx := shot.vx * acceleration
  let vy := shot.vy * acceleration
  let rotation := shot.rotation + (if vx < 0 then -0.8 else 0.8)
  let (dx, dy) := clipShotMotion world shot.x shot.y vx vy 21.5 false
  let blocked := dx != vx || dy != vy
  let x := (shot.x * 16).toInt32.toInt
  let y := (shot.y * 16).toInt32.toInt
  let wet := water.submerged world x y || magma.submerged world x y
  let scale : Float32 := if wet && !blocked then 0.5 else 1
  let x := shot.x + dx * scale
  let y := shot.y + dy * scale
  if x < -24 || y < -24 || x ≥ single (world.width * 16 + 24 : Nat) ||
      y ≥ single (world.height * 16 + 24 : Nat) then failure
  return ({ shot with x, y, vx, vy, rotation, life := shot.life - 1 }, blocked)

end Lean4Terria
