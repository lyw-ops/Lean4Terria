import Lean4Terria.Ranged

namespace Lean4Terria

def bombFuseTicks : Nat := 180
def bombUseTime : Nat := 25
def bombRadius : Int := 4
def bombDamage : Nat := 100
def dynamiteFuseTicks : Nat := 300
def dynamiteUseTime : Nat := 40
def maxActiveBombs : Nat := 16

private def bombLaunchSpeed : Int := 5 * tileUnits / 16
private def bombFallSpeed : Int := 10 * tileUnits / 16

structure ActiveBomb where
  x : Int
  y : Int
  vx : Int
  vy : Int
  fuse : Nat
  sticky : Bool
  attached : Option TilePos := none
  slot : Option Nat := none
  dynamite : Bool := false
  deriving BEq, Repr

def ActiveBomb.position (bomb : ActiveBomb) : TilePos :=
  ⟨bomb.x / tileUnits, bomb.y / tileUnits⟩

def ActiveBomb.fuseLimit (bomb : ActiveBomb) : Nat :=
  if bomb.dynamite then dynamiteFuseTicks else bombFuseTicks

def ActiveBomb.damage (bomb : ActiveBomb) : Nat := if bomb.dynamite then 250 else bombDamage

def ActiveBomb.radius (bomb : ActiveBomb) : Int := if bomb.dynamite then 7 else bombRadius

def ActiveBomb.valid (bomb : ActiveBomb) (world : World) : Bool :=
  world.contains bomb.position && 0 < bomb.fuse && bomb.fuse ≤ bomb.fuseLimit &&
    (!bomb.dynamite || !bomb.sticky) &&
    bomb.vx.natAbs ≤ (if bomb.dynamite then 64 else bombLaunchSpeed.toNat) && bomb.vy.natAbs ≤ bombFallSpeed.toNat &&
    bomb.attached.all (fun support => bomb.sticky && bomb.vx == 0 && bomb.vy == 0 &&
      (support.x - bomb.position.x).natAbs + (support.y - bomb.position.y).natAbs ≤ 1)

def throwBomb (world : World) (player : Player) (target : TilePos) (sticky : Bool)
    (dynamite := false) : Option ActiveBomb := do
  if !world.contains target then failure
  let x := bodyCenterX player
  let y := bodyCenterY player
  let targetX := target.x * tileUnits + tileUnits / 2
  let targetY := target.y * tileUnits + tileUnits / 2
  let speed := if dynamite then 4 * tileUnits / 16 else bombLaunchSpeed
  let (vx, vy) ← aimVelocity speed (targetX - x) (targetY - y)
  return { x, y, vx, vy, fuse := if dynamite then dynamiteFuseTicks else bombFuseTicks, sticky, dynamite }

private def bounce (speed : Int) : Int :=
  let magnitude := speed.natAbs * 2 / 5
  if magnitude < 5 then 0 else if speed < 0 then magnitude else -(magnitude : Int)

private def ActiveBomb.collide (bomb : ActiveBomb) (hit : TerrainHit) (horizontal : Bool) : ActiveBomb :=
  let delta := if horizontal then bomb.vx else bomb.vy
  let offset : Int := if delta > 0 then 1 else -1
  let bomb := if horizontal then { bomb with x := hit.x - offset, vx := bounce bomb.vx }
    else { bomb with y := hit.y - offset, vy := bounce bomb.vy, vx := bomb.vx * 4 / 5 }
  if bomb.sticky then { bomb with vx := 0, vy := 0, attached := some hit.tile } else bomb

/-- Advance a lit bomb once. A zero fuse marks the tick on which it detonates. -/
def advanceBomb (world : World) (bomb : ActiveBomb) : ActiveBomb := Id.run do
  let bomb := { bomb with fuse := bomb.fuse - 1 }
  if bomb.fuse == 0 then return bomb
  if bomb.attached.any (fun cell => (world.tile cell).solid) then return bomb
  let mut moving := { bomb with attached := none, vy := min bombFallSpeed (bomb.vy + 3) }
  if moving.vx != 0 then
    if let some hit := terrainHit world moving.x moving.y moving.vx 0 then
      moving := moving.collide hit true
    else
      moving := { moving with x := moving.x + moving.vx }
  if moving.vy == 0 then return moving
  if let some hit := terrainHit world moving.x moving.y 0 moving.vy then
    return moving.collide hit false
  return { moving with y := moving.y + moving.vy }

private def ActiveBomb.includesPoint (bomb : ActiveBomb) (x y : Int) : Bool :=
  (x - bomb.x)^2 + (y - bomb.y)^2 ≤ (bomb.radius * tileUnits)^2

def ActiveBomb.hitsBox (bomb : ActiveBomb) (box : HitBox) : Bool :=
  let radius := if bomb.dynamite then 125 * tileUnits / 16 else bombRadius * tileUnits
  let blast : HitBox := ⟨bomb.x - radius, bomb.y - radius, bomb.x + radius, bomb.y + radius⟩
  blast.intersects box

def ActiveBomb.blastTiles (bomb : ActiveBomb) (world : World) : Array TilePos := Id.run do
  let center := bomb.position
  let mut cells := #[]
  let diameter := 2 * bomb.radius.toNat + 1
  for row in [:diameter] do
    for column in [:diameter] do
      let pos : TilePos := ⟨center.x + column - bomb.radius, center.y + row - bomb.radius⟩
      if world.contains pos && bomb.includesPoint (pos.x * tileUnits + tileUnits / 2)
          (pos.y * tileUnits + tileUnits / 2) then cells := cells.push pos
  return cells

end Lean4Terria
