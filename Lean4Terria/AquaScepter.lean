import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

def baseMana : Nat := 20
def aquaManaCost : Nat := 7
def aquaManaDelay : Nat := 4
def manaRegenUnit : Nat := 120
def aquaMaxAnimation : Nat := 39
def aquaMaxShotDelay : Nat := 20
def aquaDamage : Nat := 27
def maxAquaStreams : Nat := 8
def aquaLifetime : Nat := 3600
def aquaLavaSeedLimit : Nat := 20000

/-- Desktop 1.4.5.8 enables ManaV2 by default. At the starting capacity,
the Classic delay is capped to four frames and fractional progress is retained. -/
def regenerateMana (mana delay progress : Nat) (stationary : Bool) : Nat × Nat × Nat :=
  let delay := delay - if stationary then 2 else 1
  let rate := (baseMana / 3 + 1) * if stationary then 2 else 1
  let ratio : Float32 := (Int32.ofNat mana).toFloat32 / (Int32.ofNat baseMana).toFloat32
  let factor : Float32 := 0.5 + 0.5 * ratio
  let factor := if delay > 0 then factor * 0.05 else factor
  let rate := max 2 ((Int32.ofNat rate).toFloat32 * factor).toUInt32.toNat
  let progress := progress + rate
  (min baseMana (mana + progress / manaRegenUnit), delay, progress % manaRegenUnit)

/-- Original projectile type 22. Position is its center in original pixels.
The bounce age is the subupdate that entered the lava branch. Gravity and
the extra-update schedule switch on the following subupdate. -/
structure AquaStream where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  age : Nat := 0
  hitsLeft : Nat := 5
  lavaSeed : UInt32 := 0
  lavaBounceAge : Option Nat := none
  slot : Option Nat := none
  deriving Repr, Inhabited

instance : BEq AquaStream where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.age == b.age && a.hitsLeft == b.hitsLeft &&
    a.lavaSeed == b.lavaSeed && a.lavaBounceAge == b.lavaBounceAge && a.slot == b.slot

private def pixels (units : Int) : Float32 := (Int32.ofInt units).toFloat32 / 16
private def subpixels (value : Float32) : Int := (value * 16).toInt32.toInt

def AquaStream.tile (stream : AquaStream) : TilePos :=
  ⟨subpixels stream.x / tileUnits, subpixels stream.y / tileUnits⟩

def AquaStream.hitBox (stream : AquaStream) : HitBox :=
  let left := (stream.x - 9).toInt32.toInt * 16
  let top := (stream.y - 9).toInt32.toInt * 16
  ⟨left, top, left + 288, top + 288⟩

def AquaStream.steam (stream : AquaStream) : Bool :=
  stream.lavaBounceAge.any (· < stream.age)

def AquaStream.scale (stream : AquaStream) : Float32 := Id.run do
  let mut scale : Float32 := 1
  for _ in [:stream.lavaBounceAge.getD stream.age] do scale := scale - 0.01
  return scale

def AquaStream.valid (stream : AquaStream) (world : World) : Bool :=
  stream.x.isFinite && stream.y.isFinite && stream.vx.isFinite && stream.vy.isFinite &&
    stream.lavaSeed.toNat < aquaLavaSeedLimit && 0 < stream.hitsLeft && stream.hitsLeft ≤ 5 &&
    world.contains stream.tile &&
    (match stream.lavaBounceAge with
    | none => stream.age ≤ 100 && stream.vx.abs ≤ 12.50001 && -12.50001 ≤ stream.vy && stream.vy ≤ 27
    | some bounce => 0 < bounce && bounce ≤ 100 && bounce ≤ stream.age && stream.age < aquaLifetime &&
        stream.vx.abs ≤ 2.5 && -8 ≤ stream.vy && stream.vy ≤ 270)

def AquaStream.fire (player : Player) (aim : WorldAim) (facing : Int) (lavaSeed : UInt32) : AquaStream :=
  let x := pixels (bodyCenterX player)
  let y := pixels (bodyCenterY player)
  let dx := pixels aim.x - x
  let dy := pixels aim.y - y
  let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if distance == 0 then { x, y, vx := if facing < 0 then -12.5 else 12.5, vy := 0, lavaSeed }
  else
    let scale := 12.5 / distance
    { x, y, vx := dx * scale, vy := dy * scale, lavaSeed }

private def nextLavaSeed (seed : UInt32) : UInt32 := seed * 2438952949 + 1

private def lavaSample (seed : UInt32) : Float32 :=
  (seed.toFloat / 4294967296).toFloat32

/-- Style 12 stays straight for four updates, then adds 0.15 vertical
velocity each update. A descending stream checks the center tile before moving.
It uses water gravity on the bounce subupdate and steam gravity thereafter. -/
def AquaStream.advance (stream : AquaStream) (world : World) (magma : Water := default) : Option AquaStream := do
  if stream.age ≥ aquaLifetime || stream.hitsLeft == 0 then failure
  let age := stream.age + 1
  if stream.lavaBounceAge.isNone && age > 100 then failure
  let (vx, vy, lavaBounceAge) :=
    if stream.lavaBounceAge.isNone && stream.vy > 0 && magma.level world stream.tile > 0 then
      let vertical := nextLavaSeed stream.lavaSeed
      let horizontal := nextLavaSeed vertical
      ((lavaSample horizontal - 0.5) * 5, -4 * lavaSample vertical - 4, some age)
    else (stream.vx, stream.vy, stream.lavaBounceAge)
  let gravity : Float32 := if stream.lavaBounceAge.isSome then 0.075 else 0.15
  let vy := if stream.age ≥ 4 then vy + gravity else vy
  let x := stream.x + vx
  let y := stream.y + vy
  if squareProjectileBlocked world (subpixels stream.x) (subpixels stream.y) (subpixels x) (subpixels y) 144 then
    failure
  return { stream with x, y, vx, vy, age, lavaBounceAge }

end Lean4Terria
