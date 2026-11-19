import Lean4Terria.Shortsword

namespace Lean4Terria

def darkLanceDamage : Nat := 34
def darkLanceKnockback : Float32 := 5
def darkLanceUseTime : Nat := 22
def darkLanceShootSpeed : Float32 := 6

/-- Projectile 46. The first active update has age one and uses the full
item animation. The base and tip both follow the current owner position. -/
structure DarkLance where
  vx : Float32
  vy : Float32
  age : Nat := 0
  animation : Nat := darkLanceUseTime
  facing : Int
  slot : Option Nat := none
  deriving Repr

instance : BEq DarkLance where
  beq a b := a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.age == b.age && a.animation == b.animation && a.facing == b.facing && a.slot == b.slot

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixel (n : Int) : Float32 := single n / 16

private def length (x y : Float32) : Float32 :=
  (x * x + y * y).toFloat.sqrt.toFloat32

def DarkLance.launch (player : Player) (aim : WorldAim) (facing : Int) (assassinPants := false) : DarkLance :=
  let dx := pixel aim.x - pixel (bodyCenterX player)
  let dy := pixel aim.y - pixel (bodyCenterY player)
  let speed := if assassinPants then darkLanceShootSpeed * 1.1 else darkLanceShootSpeed
  let animation := if assassinPants then darkLanceUseTime * 100 / 110 else darkLanceUseTime
  if dx == 0 && dy == 0 then
    { vx := single facing * speed, vy := 0, facing, animation }
  else
    let scale := speed / length dx dy
    { vx := dx * scale, vy := dy * scale, facing, animation }

def DarkLance.valid (lance : DarkLance) : Bool :=
  let square := lance.vx * lance.vx + lance.vy * lance.vy
  let usual := lance.animation == darkLanceUseTime
  let expected : Float32 := if usual then 36 else 43.56
  lance.vx.isFinite && lance.vy.isFinite && (square - expected).abs < 0.0001 &&
    (usual || lance.animation == 20) && lance.age < lance.animation && (lance.facing == -1 || lance.facing == 1) &&
    lance.slot.all (· < maxProjectileSlots)

def DarkLance.advance (lance : DarkLance) (ownerAlive := true) : Option DarkLance :=
  if ownerAlive && lance.age + 1 < lance.animation then some { lance with age := lance.age + 1 }
  else none

private def DarkLance.animationRemaining (lance : DarkLance) : Nat :=
  lance.animation + 1 - max 1 lance.age

private def DarkLance.offset (lance : DarkLance) : Float32 :=
  let remaining := lance.animationRemaining
  let retractWindow := lance.animation / 3
  let retract := min remaining retractWindow
  let extension := lance.animation - retractWindow - (remaining - retract)
  3 + 1.4 * single extension - 1.6 * single (retractWindow - retract)

-- The native 20px body is scaled to 22px in SetDefaults.
private def bodySize : Int := 22
private def halfBody : Float32 := single bodySize / 2

private def DarkLance.topLeft (lance : DarkLance) (player : Player) : Float32 × Float32 :=
  let x := pixel (bodyCenterX player) - halfBody
  let y := pixel (bodyCenterY player) - halfBody
  if lance.age == 0 then (x, y) else
    let offset := lance.offset
    -- AI writes top-left plus the spear offset; UpdatePosition then adds
    -- velocity once. Keep both Float32 additions in their native order.
    ((x + lance.vx * offset) + lance.vx, (y + lance.vy * offset) + lance.vy)

def DarkLance.center (lance : DarkLance) (player : Player) : Float32 × Float32 :=
  let (x, y) := lance.topLeft player
  (x + halfBody, y + halfBody)

private def toHitBox (x y width height : Int) : HitBox :=
  ⟨x * 16, y * 16, (x + width) * 16, (y + height) * 16⟩

def DarkLance.hitBox (lance : DarkLance) (player : Player) : HitBox :=
  let (x, y) := lance.topLeft player
  toHitBox x.toInt32.toInt y.toInt32.toInt bodySize bodySize

private def centeredBox (x y size : Float32) : HitBox :=
  let side := size.toInt32.toInt
  toHitBox (x - size / 2).toInt32.toInt (y - size / 2).toInt32.toInt side side

private def DarkLance.angle (lance : DarkLance) : Float32 :=
  (Float.atan2 lance.vy.toFloat lance.vx.toFloat).toFloat32

def DarkLance.spriteDirection (lance : DarkLance) : Int := -lance.facing

/-- After AI sets the sprite orientation, generic movement updates the
projectile hit direction from velocity. Vertical thrusts therefore hit right. -/
def DarkLance.hitDirection (lance : DarkLance) : Int :=
  if lance.vx < 0 then -1 else 1

def DarkLance.rotation (lance : DarkLance) : Float32 :=
  let rotation := lance.angle + 1.5707963705062866 + 0.7853981852531433
  if lance.spriteDirection == -1 then rotation - 1.5707963705062866 else rotation

/-- The extra rectangle at the tip only exists during the outward stroke. -/
def DarkLance.extensionHitBox (lance : DarkLance) (player : Player) : Option HitBox := do
  let remaining := lance.animationRemaining
  let retractWindow := lance.animation / 3
  if lance.age == 0 || remaining < retractWindow then failure
  let progress := (single remaining - single lance.animation) /
    (single retractWindow - single lance.animation)
  let reach := 10 + 50 * progress
  let size := 10 + 15 * progress
  let (x, y) := lance.center player
  let angle := lance.angle.toFloat
  return centeredBox (x + angle.cos.toFloat32 * reach) (y + angle.sin.toFloat32 * reach) size

/-- Native style 19 checks the 22px base, the extending tip, and boxes
spaced between the current center and the integer tip rectangle center. -/
def DarkLance.intersects (lance : DarkLance) (player : Player) (target : HitBox) : Bool := Id.run do
  if (lance.hitBox player).intersects target then return true
  let some tip := lance.extensionHitBox player | return false
  if tip.intersects target then return true
  let size := (tip.right - tip.left).tdiv 16
  let tipX := single (tip.left.tdiv 16 + size.tdiv 2)
  let tipY := single (tip.top.tdiv 16 + size.tdiv 2)
  let (x, y) := lance.center player
  let dx := tipX - x
  let dy := tipY - y
  let distance := length dx dy
  let stride := single (max 12 size)
  for i in [:(distance / stride).toUInt32.toNat] do
    let traveled := single (i + 1 : Nat) * stride
    if traveled ≥ distance then break
    let t := traveled / distance
    if (centeredBox (x + dx * t) (y + dy * t) (single size)).intersects target then return true
  return false

/-- Vanilla skips the owner sight test for NPCs with noTileCollide. -/
def DarkLance.canHit (lance : DarkLance) (world : World) (player : Player) (target : HitBox)
    (ignoreTerrain := false) : Bool :=
  lance.age > 0 && lance.age < darkLanceUseTime && lance.intersects player target &&
    (ignoreTerrain || meleeOwnerCanHit world player lance.facing target (lance.center player))

private def coveredTiles (x y size : Float32) : Array TilePos := Id.run do
  let left := max 0 (x / 16).toInt32.toInt
  let top := max 0 (y / 16).toInt32.toInt
  let right := ((x + size) / 16).toInt32.toInt
  let bottom := ((y + size) / 16).toInt32.toInt
  let mut tiles := #[]
  for column in [:(right - left + 1).toNat] do
    for row in [:(bottom - top + 1).toNat] do
      tiles := tiles.push ⟨left + column, top + row⟩
  return tiles

/-- The terrain cut visits the tip first, then the physical projectile.
Intermediate collision samples only apply to NPC hits. -/
def DarkLance.cutTiles (lance : DarkLance) (player : Player) : Array TilePos :=
  let tip := (lance.extensionHitBox player).map fun box =>
    coveredTiles (pixel box.left) (pixel box.top) (pixel (box.right - box.left))
  let (x, y) := lance.topLeft player
  tip.getD #[] ++ coveredTiles x y (single bodySize)

end Lean4Terria
