import Lean4Terria.EyeOfCthulhu

namespace Lean4Terria

def maxEvilFlyers : Nat := 3
def evilFlyerHalfSize : Int := 15 * (tileUnits / 16)
private def hoverTicks : Nat := 180
private def diveTicks : Nat := 50

inductive EvilFlyerKind where
  | eaterOfSouls | crimera
  deriving BEq, Repr, Inhabited

def EvilFlyerKind.name : EvilFlyerKind → String
  | .eaterOfSouls => "Eater of Souls"
  | .crimera => "Crimera"

def EvilFlyerKind.maxHealth (_ : EvilFlyerKind) : Nat := 40

structure EvilFlyer where
  id : Nat
  kind : EvilFlyerKind
  motion : EyeMotion
  health : Nat
  ticks : Nat
  diving : Bool
  deriving BEq, Repr

def EvilFlyer.hitBox (flyer : EvilFlyer) : HitBox :=
  let m := flyer.motion
  ⟨m.x - evilFlyerHalfSize, m.y - evilFlyerHalfSize, m.x + evilFlyerHalfSize, m.y + evilFlyerHalfSize⟩

def EvilFlyer.valid (flyer : EvilFlyer) (world : World) : Bool :=
  flyer.id < maxEvilFlyers && 0 < flyer.health && flyer.health ≤ flyer.kind.maxHealth &&
    flyer.motion.valid world 128 && flyer.ticks < (if flyer.diving then diveTicks else hoverTicks)

private def roomForFlyer (world : World) (motion : EyeMotion) : Bool := Id.run do
  let left := (motion.x - evilFlyerHalfSize) / tileUnits
  let right := (motion.x + evilFlyerHalfSize - 1) / tileUnits
  let top := (motion.y - evilFlyerHalfSize) / tileUnits
  let bottom := (motion.y + evilFlyerHalfSize - 1) / tileUnits
  for column in [:(right - left + 1).toNat] do
    for row in [:(bottom - top + 1).toNat] do
      if (world.tile ⟨left + column, top + row⟩).solid then return false
  return true

def spawnEvilFlyer (world : World) (seed : UInt64) (player : Player) (id clock : Nat)
    (kind : EvilFlyerKind) : Option EvilFlyer := Id.run do
  let center := player.centerTile
  let side : Int := if (seed.toNat + clock + id) % 2 == 0 then 1 else -1
  for distance in [7:15] do
    for direction in [side, -side] do
      for dy in [(-5 : Int), -3, -1, 0, 2, 4] do
        let pos : TilePos := ⟨center.x + direction * distance, center.y + dy⟩
        if !inEvilBiome seed pos then continue
        let motion : EyeMotion := ⟨pos.x * tileUnits + tileUnits / 2,
          pos.y * tileUnits + tileUnits / 2, -direction * 20, 0⟩
        if roomForFlyer world motion then return some ⟨id, kind, motion, kind.maxHealth, 0, false⟩
  return none

private def EvilFlyer.move (flyer : EvilFlyer) (world : World) (vx vy : Int) : EyeMotion :=
  let m := flyer.motion
  let across := { m with x := m.x + vx, vx }
  let across := if roomForFlyer world across then across else { m with vx := -(vx / 2) }
  let down := { across with y := across.y + vy, vy }
  if roomForFlyer world down then down else { across with vy := -(vy / 2) }

def advanceEvilFlyer (world : World) (player : Player) (flyer : EvilFlyer) : Option EvilFlyer := Id.run do
  let m := flyer.motion
  let dx := bodyCenterX player - m.x
  let dy := bodyCenterY player - m.y
  if dx.natAbs > (34 * tileUnits).toNat || dy.natAbs > (24 * tileUnits).toNat then return none
  let visible := terrainLineClear world m.x m.y (bodyCenterX player) (bodyCenterY player)
  let charging := flyer.diving || (flyer.ticks + 1 ≥ hoverTicks && visible)
  let side : Int := if m.x < bodyCenterX player then -1 else 1
  let (vx, vy) := if flyer.diving then (m.vx, m.vy)
    else if charging then (aimVelocity 96 dx dy).getD (0, 0)
    else
      let pass := if flyer.ticks < hoverTicks / 2 then side else -side
      let (wantedX, wantedY) := (aimVelocity 64 (dx + pass * 3 * tileUnits)
        (dy - 2 * tileUnits)).getD (0, 0)
      (m.vx + max (-2) (min 2 (wantedX - m.vx)), m.vy + max (-2) (min 2 (wantedY - m.vy)))
  let motion := flyer.move world vx vy
  let ticks := if charging && !flyer.diving then 1 else flyer.ticks + 1
  let limit := if charging then diveTicks else hoverTicks
  return some { flyer with motion, ticks := ticks % limit, diving := charging && ticks < limit }

end Lean4Terria
