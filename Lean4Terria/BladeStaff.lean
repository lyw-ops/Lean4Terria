import Lean4Terria.Ranged

namespace Lean4Terria

def bladeStaffDamage : Nat := 6
def bladeStaffUseTicks : Nat := 36
def bladeStaffReuseDelay : Nat := 2
def bladeStaffArmorPenetration : Nat := 25
def bladeStaffLocalImmunity : Nat := 10
def bladeStaffRecovery : Nat := 9
def bladeStaffTrailLength : Nat := 12
def bladeStaffAcquireRange : Int := 800 * 16
def bladeStaffReturnRange : Int := 900 * 16

def bladeStaffHitDamage (summonBonus defense roll : Nat) : Nat :=
  let base := bladeStaffDamage * (100 + summonBonus) / 100
  let varied := (classicNpcContactDamage base roll 0).1
  let penetration := min defense bladeStaffArmorPenetration / 2
  max 1 (varied + penetration - (defense + 1) / 2)

structure DaggerPose where
  x : Float32
  y : Float32
  rotation : Float32
  deriving Repr, Inhabited

instance : BEq DaggerPose where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.rotation.toBits == b.rotation.toBits

def DaggerPose.position (pose : DaggerPose) : Int × Int :=
  ((pose.x * 16).toInt32.toInt, (pose.y * 16).toInt32.toInt)

def DaggerPose.valid (pose : DaggerPose) (world : World) : Bool :=
  let (x, y) := pose.position
  pose.x.isFinite && pose.y.isFinite && pose.rotation.isFinite && pose.rotation.abs ≤ 128 &&
    -64 * 16 ≤ x && x ≤ world.width * tileUnits + 64 * 16 &&
    -64 * 16 ≤ y && y ≤ world.height * tileUnits + 64 * 16

structure EnchantedDagger where
  pose : DaggerPose
  vx : Float32 := 0
  vy : Float32 := 0
  trail : Array DaggerPose := #[]
  deriving Repr, Inhabited

instance : BEq EnchantedDagger where
  beq a b := a.pose == b.pose && a.vx.toBits == b.vx.toBits &&
    a.vy.toBits == b.vy.toBits && a.trail == b.trail

private def bladePixels (subpixels : Int) : Float32 := (Int32.ofInt subpixels).toFloat32 / 16

def EnchantedDagger.spawn (x y : Int) : EnchantedDagger :=
  { pose := ⟨bladePixels x, bladePixels y, 0⟩ }

def EnchantedDagger.valid (blade : EnchantedDagger) (world : World) : Bool :=
  blade.pose.valid world && blade.vx.isFinite && blade.vy.isFinite &&
    blade.vx.abs ≤ 44 && blade.vy.abs ≤ 44 &&
    blade.trail.size ≤ bladeStaffTrailLength && blade.trail.all (·.valid world)

def EnchantedDagger.hitBox (blade : EnchantedDagger) : HitBox :=
  let x := (blade.pose.x - 5).toInt32.toInt * 16
  let y := (blade.pose.y - 5).toInt32.toInt * 16
  ⟨x, y, x + 160, y + 160⟩

private def bladeLength (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32

private def bladeTurn (old desired amount : Float32) : Float32 :=
  let pi : Float32 := 3.1415927410125732
  let delta := desired - old
  let wraps := ((delta + pi) / (2 * pi)).toFloat.floor.toFloat32
  old + (delta - wraps * 2 * pi) * amount

private def EnchantedDagger.move (blade : EnchantedDagger) (vx vy rotation : Float32) :
    EnchantedDagger :=
  { pose := ⟨blade.pose.x + vx, blade.pose.y + vy, rotation⟩
    vx, vy
    trail := (#[blade.pose] ++ blade.trail).extract 0 bladeStaffTrailLength }

def EnchantedDagger.coast (blade : EnchantedDagger) : EnchantedDagger :=
  blade.move blade.vx blade.vy blade.pose.rotation

private def EnchantedDagger.approach (blade : EnchantedDagger) (dx dy distance speed amount : Float32)
    (upright := false) : EnchantedDagger :=
  let scale := if distance > speed then speed / distance else 1
  let angle := if upright || distance == 0 then 3.1415927410125732
    else (Float.atan2 dy.toFloat dx.toFloat).toFloat32 + 1.5707963705062866
  blade.move (dx * scale) (dy * scale) (bladeTurn blade.pose.rotation angle amount)

/-- AI 169 centers a lone dagger 30 pixels above the player's top edge.
Its return speed increases from 10 to 40 pixels per tick between 200 and 600 pixels away. -/
def EnchantedDagger.follow (blade : EnchantedDagger) (ownerX ownerTop : Int) : EnchantedDagger :=
  let x := (bladePixels ownerX).toFloat.floor.toFloat32
  let y := (bladePixels ownerTop - 30).toFloat.floor.toFloat32
  let dx := x - blade.pose.x
  let dy := y - blade.pose.y
  let distance := bladeLength dx dy
  let speed := 10 + 30 * max 0 (min 1 ((distance - 200) / 400))
  let blade := if distance < 3000 then blade else
    { blade with pose := { blade.pose with x, y }, trail := #[] }
  blade.approach dx dy distance speed 0.2 (distance < 40)

def EnchantedDagger.chase (blade : EnchantedDagger) (centerX centerY : Int) : EnchantedDagger :=
  let dx := bladePixels centerX - blade.pose.x
  let dy := bladePixels centerY - blade.pose.y
  blade.approach dx dy (bladeLength dx dy) 16 0.4

/-- The first recoil frame adds one random four-pixel vector; the next
eight frames continue spinning and damp velocity by 0.92 per frame. -/
def EnchantedDagger.recoil (blade : EnchantedDagger) (angle : Option Float32) : EnchantedDagger :=
  let vx := blade.vx + (angle.map (fun a => 4 * a.toFloat.cos.toFloat32)).getD 0
  let vy := blade.vy + (angle.map (fun a => 4 * a.toFloat.sin.toFloat32)).getD 0
  blade.move (vx * 0.92) (vy * 0.92) (bladeTurn 0 (blade.pose.rotation + vx * 0.1 + vy * 0.05) 1)

end Lean4Terria
