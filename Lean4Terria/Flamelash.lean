import Lean4Terria.MagicMissile
import Lean4Terria.Water

namespace Lean4Terria

def flamelashDamage : Nat := 32
def flamelashKnockback : Float32 := 6.5
def flamelashManaCost : Nat := 21
def flamelashUseTime : Nat := 30
def flamelashHitCooldown : Nat := 12

def flamelashOnFireDuration (chanceRoll durationRoll : Nat) : Nat :=
  if chanceRoll % 2 == 0 then 240 + durationRoll % 240 else 0

/-- Desktop projectile 34 shares style-9 motion with the Magic Missile.
`wet` is the water flag from the previous update. Frame and empowerment
timers belong to Flamelash and advance independently of its remaining life. -/
structure Flamelash where
  missile : MagicMissile
  hitsLeft : Nat := 2
  wet : Bool := false
  frame : Nat := 0
  frameTicks : Nat := 0
  empoweredTicks : Nat := 0
  deriving Repr, Inhabited, BEq

structure FlamelashStep (α : Type) where
  projectile : Flamelash
  target : Option α
  hitTerrain : Bool
  extinguished : Bool
  deactivated : Bool
  hitDirection : Int
  deriving Repr

private def frameCount : Nat := 6
private def frameDelay : Nat := 4
private def floatCounterLimit : Nat := 16777216

def Flamelash.launch (player : Player) (aim : WorldAim) (facing : Int) : Flamelash :=
  { missile := MagicMissile.launch player aim facing }

def Flamelash.tile (projectile : Flamelash) : TilePos := projectile.missile.tile

def Flamelash.hitBox (projectile : Flamelash) : HitBox := projectile.missile.hitBox

def Flamelash.blastBox (projectile : Flamelash) : HitBox :=
  let left := (projectile.missile.x - 48).toInt32.toInt * 16
  let top := (projectile.missile.y - 48).toInt32.toInt * 16
  ⟨left, top, left + 96 * 16, top + 96 * 16⟩

def Flamelash.cutTiles (projectile : Flamelash) (blast := false) : Array TilePos := Id.run do
  if !blast then return projectile.missile.cutTiles
  let tile (pixel : Float32) : Int := (pixel / 16).toInt32.toInt
  let left := max 0 (tile (projectile.missile.x - 48))
  let top := max 0 (tile (projectile.missile.y - 48))
  let right := tile (projectile.missile.x + 48)
  let bottom := tile (projectile.missile.y + 48)
  let mut tiles := #[]
  for x in [:(right - left + 1).toNat] do
    for y in [:(bottom - top + 1).toNat] do
      tiles := tiles.push ⟨left + x, top + y⟩
  return tiles

def Flamelash.valid (projectile : Flamelash) (world : World) : Bool :=
  projectile.missile.valid world && projectile.missile.soundDelay == 0 &&
    (projectile.hitsLeft == 1 || projectile.hitsLeft == 2) &&
    projectile.frame < frameCount && projectile.frameTicks ≤ frameDelay &&
    projectile.empoweredTicks ≤ floatCounterLimit &&
    (projectile.hitsLeft == 1 || projectile.empoweredTicks == 0)

def Flamelash.registerHit (projectile : Flamelash) : Flamelash :=
  { projectile with hitsLeft := projectile.hitsLeft - 1 }

/-- A first impact clears the released projectile's homing lock. Its
next update acquires again from the projectile's new position. -/
def Flamelash.clearsTargetOnHit (projectile : Flamelash) : Bool :=
  projectile.hitsLeft == 2 && !projectile.missile.guided

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def tileAtPixel (value : Float32) : Int := (value / 16).toInt32.toInt

private def touchesLiquid (world : World) (liquid : Water) (left top width height : Float32) : Bool := Id.run do
  let firstX := tileAtPixel left
  let firstY := tileAtPixel top
  for column in [:(tileAtPixel (left + width) - firstX + 1).toNat] do
    for row in [:(tileAtPixel (top + height) - firstY + 1).toNat] do
      let pos : TilePos := ⟨firstX + column, firstY + row⟩
      let fill := liquid.level world pos
      if fill == 0 then continue
      let x := single (pos.x * 16)
      let gap := single (Water.capacity + 1 - fill : Nat) / 16
      let y := single (pos.y * 16) + gap
      let bottom := y + 16 - single gap.toInt32.toInt
      if left < x + 16 && x < left + width && top < bottom && y < top + height then
        return true
  return false

/-- Item use rejects a wet owner outside lava. Terraria tests water with
a centered probe and lava against the whole body. -/
def Flamelash.canCast (world : World) (player : Player) (water magma : Water) : Bool :=
  let left := single player.x / 16
  let top := single player.y / 16
  let width := playerWidth.tdiv 16
  let height := playerHeight.tdiv 16
  let wetWidth := min 10 width
  let wetHeight := height.tdiv 2
  let wetX := left + single (width.tdiv 2 - wetWidth.tdiv 2)
  let wetY := top + single (height.tdiv 2 - wetHeight.tdiv 2)
  !touchesLiquid world water wetX wetY (single wetWidth) (single wetHeight) ||
    touchesLiquid world magma left top (single width) (single height)

/-- AI runs before the old wet flag can extinguish the shot. The first
water-contact update moves at half speed. Lava clears wet and does not
affect motion. A terrain or water kill returns before the ordinary hit scan. -/
def Flamelash.advance [BEq α] (projectile : Flamelash) (world : World) (player : Player)
    (aim : WorldAim) (held : Bool) (targets : Array (α × HitBox)) (target : Option α)
    (water magma : Water) : FlamelashStep α := Id.run do
  let missile := projectile.missile
  let lava := touchesLiquid world magma (missile.x - 16) (missile.y - 16) 32 32
  let wet := !lava && touchesLiquid world water (missile.x - 5) (missile.y - 8) 10 16
  let extinguished := projectile.wet && !lava
  let flight := missile.advance world player aim held targets target
    (waterMotion := wet) (stopBeforeMovement := extinguished)
  if flight.deactivated then
    return {
      projectile, target := none, hitTerrain := false, extinguished := false,
      deactivated := true, hitDirection := flight.hitDirection }
  let (frame, frameTicks) := if projectile.frameTicks == frameDelay then
      ((projectile.frame + 1) % frameCount, 0)
    else (projectile.frame, projectile.frameTicks + 1)
  let empoweredTicks := if projectile.hitsLeft == 1 then
      min floatCounterLimit (projectile.empoweredTicks + 1)
    else 0
  let projectile := { projectile with
    missile := { flight.missile with soundDelay := 0 }, wet,
    frame, frameTicks, empoweredTicks }
  return {
    projectile, target := flight.target, hitTerrain := flight.hitTerrain, extinguished,
    deactivated := false, hitDirection := flight.hitDirection }

end Lean4Terria
