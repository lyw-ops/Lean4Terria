import Lean4Terria.Magma
import Lean4Terria.Ranged

namespace Lean4Terria

def flowerFireDamage : Nat := 48
def flowerFireManaCost : Nat := 9
def flowerFireUseTime : Nat := 16
def flowerFireSlowUseTime : Nat := 40
def flowerFireLife : Nat := 3600
def hellwingShotLife : Nat := 1200

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixel (subpixels : Int) : Float32 := single subpixels / 16
private def tileIndex (value : Float32) : Int := (value / 16).toFloat.floor.toInt64.toInt

private def shotCenter (player : Player) : Float32 × Float32 :=
  (pixel (bodyCenterX player), pixel (bodyCenterY player))

private def shotVelocity (player : Player) (aim : WorldAim) (facing : Int)
    (speed : Float32) : Float32 × Float32 :=
  let dx := pixel (aim.x - bodyCenterX player)
  let dy := pixel (aim.y - bodyCenterY player)
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if length == 0 then (single facing * speed, 0)
  else let factor := speed / length
    (dx * factor, dy * factor)

private def projectileBox (x y : Float32) (half : Int) : HitBox :=
  let left := (x - single half).toInt32.toInt * 16
  let top := (y - single half).toInt32.toInt * 16
  ⟨left, top, left + half * 32, top + half * 32⟩

def projectileBoxCells (box : HitBox) : Array TilePos := Id.run do
  let left := box.left / tileUnits
  let top := box.top / tileUnits
  let mut cells := #[]
  for y in [:((box.bottom / tileUnits) - top + 1).toNat] do
    for x in [:((box.right / tileUnits) - left + 1).toNat] do
      cells := cells.push ⟨left + x, top + y⟩
  return cells

private def shotInWorld (world : World) (x y : Float32) : Bool :=
  x.isFinite && y.isFinite && 0 ≤ x && x < single (world.width * 16 : Nat) &&
    0 ≤ y && y < single (world.height * 16 : Nat)

def shotLiquidContact (world : World) (liquid : Water) (x y halfWidth halfHeight : Float32) : Bool := Id.run do
  let left := tileIndex (x - halfWidth)
  let top := tileIndex (y - halfHeight)
  let right := tileIndex (x + halfWidth)
  let bottom := tileIndex (y + halfHeight)
  for row in [:(bottom - top + 1).toNat] do
    for column in [:(right - left + 1).toNat] do
      let tx := left + column
      let ty := top + row
      let level := liquid.level world ⟨tx, ty⟩
      let surface := single (ty * 16) + single (256 - level : Nat) / 16
      if level > 0 && single (tx * 16) < x + halfWidth && x - halfWidth < single (tx * 16 + 16) &&
          surface < y + halfHeight && y - halfHeight < single (ty * 16 + 16) then return true
  return false

/-- Clip the requested motion against flat terrain and top surfaces of
platforms. The caller applies the projectile type's response to each axis. -/
def clipShotMotion (world : World) (x y vx vy half : Float32)
    (platforms : Bool) (halfHeight := half) : Float32 × Float32 := Id.run do
  let left := x - half
  let top := y - halfHeight
  let right := x + half
  let bottom := y + halfHeight
  let col := tileIndex (left + vx)
  let row := tileIndex (top + vy)
  let mut dx := vx
  let mut dy := vy
  for iy in [:(tileIndex (bottom + vy) - row + 1).toNat] do
    let ty := row + iy
    for ix in [:(tileIndex (right + vx) - col + 1).toNat] do
      let tx := col + ix
      let tile := world.tile ⟨tx, ty⟩
      let edgeX := single (tx * 16)
      let edgeY := single (ty * 16)
      if !(tile.solid || (platforms && tile == .platform)) ||
          right + vx ≤ edgeX || edgeX + 16 ≤ left + vx ||
          bottom + vy ≤ edgeY || edgeY + 16 ≤ top + vy then continue
      if bottom ≤ edgeY then dy := min dy (edgeY - bottom)
      else if tile.solid then
        if right ≤ edgeX then dx := min dx (edgeX - right)
        else if left ≥ edgeX + 16 then dx := max dx (edgeX + 16 - left)
        else if top ≥ edgeY + 16 then dy := max dy (edgeY + 16 - top)
  return (dx, dy)

/-- Projectile 15, fired by the Flower of Fire. Positions are the native
top-left with the 8px radius added, stored as Float32. -/
structure FlowerFireball where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  life : Nat := flowerFireLife
  bounces : Nat := 0
  wet : Bool := false
  slot : Option Nat := none
  deriving Repr

instance : BEq FlowerFireball where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.life == b.life &&
    a.bounces == b.bounces && a.wet == b.wet && a.slot == b.slot

def FlowerFireball.launch (player : Player) (aim : WorldAim) (facing : Int) : FlowerFireball :=
  let (x, y) := shotCenter player
  let (vx, vy) := shotVelocity player aim facing 7.5
  { x, y, vx, vy }

def FlowerFireball.hitBox (shot : FlowerFireball) : HitBox := projectileBox shot.x shot.y 8

def FlowerFireball.valid (shot : FlowerFireball) (world : World) : Bool :=
  shotInWorld world shot.x shot.y && shot.vx.isFinite && shot.vy.isFinite &&
    shot.vx.abs ≤ 7.5001 && shot.vy.abs ≤ 16 && 0 < shot.life && shot.life ≤ flowerFireLife &&
    shot.bounces < 6 && shot.slot.all (· < maxProjectileSlots)

def FlowerFireball.advance (shot : FlowerFireball) (world : World) (water magma : Water) :
    Option FlowerFireball × Bool := Id.run do
  if shot.life == 0 then return (none, false)
  let inLava := shotLiquidContact world magma shot.x shot.y 8 8
  if shot.wet && !inLava then return (none, false)
  let inWater := shotLiquidContact world water shot.x shot.y 5 4
  let wet := inWater || shotLiquidContact world magma shot.x shot.y 5 4
  let age := flowerFireLife - shot.life + 1
  let fallingVy := if age ≥ 20 then min 16 (shot.vy + 0.2) else shot.vy
  let (dx, dy) := clipShotMotion world shot.x shot.y shot.vx fallingVy 8 true
  let hitX := dx != shot.vx
  let hitY := dy != fallingVy
  let collision := hitX || hitY
  let bounces := shot.bounces + if collision then 1 else 0
  if bounces ≥ 6 then return (none, true)
  let vx := if hitX then -shot.vx else shot.vx
  let vy := if hitY then -fallingVy * (if dy > 4 then 0.8 else 1) else fallingVy
  let (stepX, stepY) := if wet then
      ((if hitX then dx else dx * 0.5), (if hitY then dy else dy * 0.5))
    else (vx, vy)
  let x := shot.x + stepX
  let y := shot.y + stepY
  if !shotInWorld world x y then return (none, collision)
  return (some { shot with x, y, vx, vy, bounces, life := shot.life - 1, wet := inWater && !inLava }, collision)

/-- The bow retains normal ammunition projectiles when the arrow is not
wooden. Both native types last 1200 ticks and use the bow's launch scatter. -/
structure HellwingShot where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  goalVx : Float32
  goalVy : Float32
  life : Nat := hellwingShotLife
  hits : Nat := 0
  unholy : Bool := false
  slot : Option Nat := none
  deriving Repr

instance : BEq HellwingShot where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.goalVx.toBits == b.goalVx.toBits && a.goalVy.toBits == b.goalVy.toBits &&
    a.life == b.life && a.hits == b.hits && a.unholy == b.unholy && a.slot == b.slot

private def turn (vx vy : Float32) (angle : Float) : Float32 × Float32 :=
  let cosine := angle.cos
  let sine := angle.sin
  ((vx.toFloat * cosine - vy.toFloat * sine).toFloat32, (vx.toFloat * sine + vy.toFloat * cosine).toFloat32)

def HellwingShot.launch (player : Player) (aim : WorldAim) (facing : Int)
    (unholy : Bool) (goalRoll initialRoll : Float) : HellwingShot :=
  let (x, y) := shotCenter player
  let (vx, vy) := shotVelocity player aim facing (if unholy then 9.4 else 9)
  let (goalVx, goalVy) := turn vx vy (goalRoll * 0.39269909262657166)
  let (vx, vy) := turn vx vy (initialRoll * 0.19634954631328583)
  { x, y, vx, vy, goalVx, goalVy, unholy }

def HellwingShot.hitBox (shot : HellwingShot) : HitBox :=
  projectileBox shot.x shot.y (if shot.unholy then 5 else 12)

def HellwingShot.frame (shot : HellwingShot) : Nat := (hellwingShotLife - shot.life) / 2 % 5

def HellwingShot.damage (shot : HellwingShot) : Nat :=
  if shot.unholy then (List.range shot.hits).foldl (fun damage _ => damage * 95 / 100) 34 else 27

def HellwingShot.knockback (shot : HellwingShot) : Float32 := if shot.unholy then 8.5 else 7.5

def HellwingShot.valid (shot : HellwingShot) (world : World) : Bool :=
  shotInWorld world shot.x shot.y && shot.vx.isFinite && shot.vy.isFinite &&
    shot.goalVx.isFinite && shot.goalVy.isFinite && shot.vx.abs ≤ 9.401 && shot.vy.abs ≤ 16 &&
    shot.goalVx.abs ≤ 9.401 && shot.goalVy.abs ≤ 9.401 && 0 < shot.life && shot.life ≤ hellwingShotLife &&
    (if shot.unholy then shot.hits < 5 else shot.hits == 0) && shot.slot.all (· < maxProjectileSlots)

def HellwingShot.advance (shot : HellwingShot) (world : World) (water magma : Water) :
    Option HellwingShot := do
  if shot.life == 0 then failure
  let age := hellwingShotLife - shot.life + 1
  -- The bow passes the wider spread vector to ai[0..1] even for normal
  -- ammunition. Style 1 increments that initial horizontal value to 15.
  let falling := shot.unholy &&
    (age ≥ 25 || (List.range age).foldl (fun ticks _ => ticks + 1) shot.goalVx ≥ 15)
  let (vx, vy) := if shot.unholy then (shot.vx, if falling then min 16 (shot.vy + 0.1) else shot.vy)
    else ((shot.vx * 39 + shot.goalVx) / 40, (shot.vy * 39 + shot.goalVy) / 40)
  let (dx, dy) := clipShotMotion world shot.x shot.y vx vy (if shot.unholy then 5 else 12) false
  if dx != vx || dy != vy then failure
  let halfHeight : Float32 := if shot.unholy then 2.5 else 6
  let wet := shotLiquidContact world water shot.x shot.y 5 halfHeight ||
    shotLiquidContact world magma shot.x shot.y 5 halfHeight
  let scale : Float32 := if wet then 0.5 else 1
  let x := shot.x + vx * scale
  let y := shot.y + vy * scale
  if !shotInWorld world x y then failure
  return { shot with x, y, vx, vy, life := shot.life - 1 }

end Lean4Terria
