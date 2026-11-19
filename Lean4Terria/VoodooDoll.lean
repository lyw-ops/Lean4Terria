import Lean4Terria.UnderworldShots
namespace Lean4Terria

def maxDroppedVoodooDolls : Nat := 400

/-- A world item in simulation subpixels, with the original 14 × 26 hitbox. -/
structure DroppedVoodooDoll where
  x : Int
  y : Int
  vx : Int
  vy : Int
  count : Nat
  pickupDelay : Nat
  deriving BEq, Repr, Inhabited

namespace DroppedVoodooDoll

private def px (units : Int) : Float32 := (Int32.ofInt units).toFloat32 / 16

def hitBox (doll : DroppedVoodooDoll) : HitBox :=
  ⟨doll.x, doll.y, doll.x + 224, doll.y + 416⟩

def position (doll : DroppedVoodooDoll) : TilePos :=
  ⟨(doll.x + 112) / tileUnits, (doll.y + 208) / tileUnits⟩

def launch (player : Player) (facing : Int) (count : Nat) : DroppedVoodooDoll :=
  ⟨bodyCenterX player - 112, bodyCenterY player - 208, 64 * facing + player.vx, -32, count, 100⟩

def valid (doll : DroppedVoodooDoll) (world : World) : Bool :=
  world.contains doll.position && 0 < doll.count && doll.count ≤ 9999 &&
    doll.pickupDelay ≤ 100 && doll.vx.natAbs ≤ 512 && doll.vy.natAbs ≤ 112

def touchesLava (doll : DroppedVoodooDoll) (world : World) (magma : Water) : Bool := Id.run do
  let box := doll.hitBox
  for pos in projectileBoxCells box do
    let level := magma.level world pos
    let surface := pos.y * tileUnits + (256 - level : Nat)
    if level > 0 && box.left < (pos.x + 1) * tileUnits && pos.x * tileUnits < box.right &&
        box.top < (pos.y + 1) * tileUnits && surface < box.bottom then return true
  return false

def advance (doll : DroppedVoodooDoll) (world : World) (water : Water) : DroppedVoodooDoll :=
  let wet := water.submerged world (doll.x + 112) (doll.y + 208)
  let falling := min (if wet then 80 else 112) (doll.vy + 2)
  let (dx, dy) := clipShotMotion world (px (doll.x + 112)) (px (doll.y + 208))
    (px doll.vx) (px falling) 7 true 13
  let dx := (dx * 16).toInt32.toInt
  let dy := (dy * 16).toInt32.toInt
  let vx := if dx != doll.vx then 0 else if dy != falling then (dx * 95).tdiv 100 else dx
  let vy := if dy != falling then 0 else dy
  { doll with x := doll.x + dx, y := doll.y + dy, vx, vy, pickupDelay := doll.pickupDelay - 1 }

end DroppedVoodooDoll

end Lean4Terria
