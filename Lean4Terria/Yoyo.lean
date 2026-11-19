import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

/-- The three ProjectileID tables consumed by the common yoyo pursuit AI. -/
structure YoyoStats where
  maxRange : Float32
  topSpeed : Float32
  lifetimeSeconds : Float32

def valorYoyoStats : YoyoStats := ⟨225, 14, 11⟩
def valorYoyoDamage : Nat := 28
def valorYoyoKnockback : Float32 := 3.85

/-- Projectile 564. Motion and the retained cursor target use original
pixels. Age is the style-99 local timer, including recoil surcharges. -/
structure Yoyo where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  rotation : Float32 := 0
  aimX : Float32 := 0
  aimY : Float32 := 0
  age : Nat := 0
  returning : Bool := false
  slot : Option Nat := none
  recoilStalled : Bool := false
  deriving Repr, Inhabited

instance : BEq Yoyo where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.aimX.toBits == b.aimX.toBits &&
    a.aimY.toBits == b.aimY.toBits && a.age == b.age && a.returning == b.returning &&
    a.slot == b.slot && a.recoilStalled == b.recoilStalled

private def asSingle (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (n : Int) : Float32 := asSingle n / 16
private def tileAtPixel (value : Float32) : Int := (value / 16).toInt32.toInt
private def maxNativeCounter : Nat := 16777216

private structure Vector where
  x : Float32
  y : Float32

private def Vector.add (a b : Vector) : Vector := ⟨a.x + b.x, a.y + b.y⟩
private def Vector.sub (a b : Vector) : Vector := ⟨a.x - b.x, a.y - b.y⟩
private def Vector.scale (a : Vector) (factor : Float32) : Vector := ⟨a.x * factor, a.y * factor⟩
private def Vector.div (a : Vector) (divisor : Float32) : Vector := ⟨a.x / divisor, a.y / divisor⟩
private def Vector.length (v : Vector) : Float32 :=
  (v.x * v.x + v.y * v.y).toFloat.sqrt.toFloat32

private def Vector.unit (v : Vector) : Vector :=
  if v.x == 0 && v.y == 0 then ⟨0, 0⟩ else v.scale (1 / v.length)

private def ownerCenter (player : Player) : Vector :=
  ⟨pixels (bodyCenterX player), pixels (bodyCenterY player)⟩

private def Yoyo.center (yoyo : Yoyo) : Vector := ⟨yoyo.x, yoyo.y⟩
private def Yoyo.velocity (yoyo : Yoyo) : Vector := ⟨yoyo.vx, yoyo.vy⟩
private def Yoyo.withVelocity (yoyo : Yoyo) (velocity : Vector) : Yoyo :=
  { yoyo with vx := velocity.x, vy := velocity.y }

-- SetDefaults truncates 16 × 1.075 to 17 for both collision boxes.
private def yoyoSize : Float32 := 17
private def yoyoHalf : Float32 := yoyoSize / 2

def Yoyo.launch (player : Player) (aim : WorldAim) (facing : Int) : Yoyo :=
  let center := ownerCenter player
  let delta := (⟨pixels aim.x, pixels aim.y⟩ : Vector).sub center
  let length := delta.length
  let velocity := if length == 0 then ⟨asSingle facing * 16, 0⟩ else delta.scale (16 / length)
  { x := center.x, y := center.y, vx := velocity.x, vy := velocity.y }

def Yoyo.tile (yoyo : Yoyo) : TilePos := ⟨tileAtPixel yoyo.x, tileAtPixel yoyo.y⟩

def Yoyo.hitBox (yoyo : Yoyo) : HitBox :=
  let left := (yoyo.x - yoyoHalf).toInt32.toInt * 16
  let top := (yoyo.y - yoyoHalf).toInt32.toInt * 16
  ⟨left, top, left + 272, top + 272⟩

/-- Damage cuts the eligible tiles under the projectile's current 17px body. -/
def Yoyo.cutTiles (yoyo : Yoyo) : Array TilePos := Id.run do
  let left := max 0 (tileAtPixel (yoyo.x - yoyoHalf))
  let top := max 0 (tileAtPixel (yoyo.y - yoyoHalf))
  let right := tileAtPixel (yoyo.x - yoyoHalf + yoyoSize)
  let bottom := tileAtPixel (yoyo.y - yoyoHalf + yoyoSize)
  let mut tiles := #[]
  for x in [:(right - left + 1).toNat] do
    for y in [:(bottom - top + 1).toNat] do
      tiles := tiles.push ⟨left + x, top + y⟩
  return tiles

def Yoyo.valid (yoyo : Yoyo) (world : World) : Bool :=
  yoyo.x.isFinite && yoyo.y.isFinite && yoyo.vx.isFinite && yoyo.vy.isFinite &&
    yoyo.rotation.isFinite && yoyo.rotation ≥ 0 && yoyo.rotation ≤ asSingle maxNativeCounter &&
    yoyo.aimX.isFinite && yoyo.aimY.isFinite &&
    -32 ≤ yoyo.x && yoyo.x ≤ asSingle (world.width * 16 + 32 : Nat) &&
    -32 ≤ yoyo.y && yoyo.y ≤ asSingle (world.height * 16 + 32 : Nat) &&
    -225 ≤ yoyo.aimX && yoyo.aimX ≤ asSingle (world.width * 16 + 225 : Nat) &&
    -225 ≤ yoyo.aimY && yoyo.aimY ≤ asSingle (world.height * 16 + 225 : Nat) &&
    yoyo.velocity.length ≤ 32.001 && yoyo.age ≤ maxNativeCounter &&
    yoyo.slot.all (· < maxProjectileSlots) &&
    (!yoyo.recoilStalled || (!yoyo.returning && yoyo.vx == 0 && yoyo.vy == 0))

private def Yoyo.target (yoyo : Yoyo) (owner : Vector) (aim : WorldAim) (range : Float32) : Vector :=
  let cursor : Vector := ⟨pixels aim.x, pixels aim.y⟩
  let offset := cursor.sub owner
  let bounded := if offset.length > range then owner.add (offset.unit.scale range) else cursor
  if bounded.x == yoyo.aimX && bounded.y == yoyo.aimY then ⟨yoyo.aimX, yoyo.aimY⟩
  else
    let offset := bounded.sub owner
    if offset.length > range - 1 then owner.add (offset.unit.scale (range - 1)) else bounded

private def blendVelocity (old desired : Vector) (inertia : Float32) : Vector :=
  ((old.scale (inertia - 1)).add desired).div inertia

private def Yoyo.pursuit (yoyo : Yoyo) (velocity owner : Vector) (aim : WorldAim) (stats : YoyoStats) :
    Yoyo := Id.run do
  let offset := yoyo.center.sub owner
  let stretched := offset.length > stats.maxRange
  let target := yoyo.target owner aim stats.maxRange
  let delta := target.sub yoyo.center
  let distance := delta.length
  let inertia := if stretched then 1 else max 1 (14 - stats.topSpeed / 2)
  let speed := if stretched then stats.topSpeed * 2 else stats.topSpeed
  let velocity : Vector := if stretched then
      ⟨if offset.x * velocity.x > 0 then velocity.x * 0.5 else velocity.x,
       if offset.y * velocity.y > 0 then velocity.y * 0.5 else velocity.y⟩
    else velocity
  let velocity := if distance > 5 + stats.topSpeed / 2 then
      let speed := min (distance / 2) (if stretched then speed / 2 else speed)
      blendVelocity velocity (delta.unit.scale speed) inertia
    else velocity.scale 0.8
  return { yoyo.withVelocity velocity with aimX := target.x, aimY := target.y }

private def Yoyo.control (yoyo : Yoyo) (owner : Vector) (aim : WorldAim) (held : Bool) (stats : YoyoStats) :
    Option Yoyo := do
  let age := min maxNativeCounter (yoyo.age + 1)
  let timedOut := stats.lifetimeSeconds != -1 && asSingle age / 60 > stats.lifetimeSeconds
  let back := owner.sub yoyo.center
  let distance := back.length
  let speed := stats.topSpeed
  let velocity := yoyo.velocity
  let velocity := if !yoyo.returning && !timedOut && velocity.length > speed then
      velocity.scale 0.98
    else velocity
  let returning := yoyo.returning || timedOut || !held ||
    distance.toFloat > stats.maxRange.toFloat * 1.3
  let next := { yoyo with age, returning, rotation := yoyo.rotation + 0.45 }
  if !returning then return next.pursuit velocity owner aim stats
  let returnSpeed := speed * 1.5
  if distance < returnSpeed + 10 || distance > 2000 then failure
  let inertia := max 1 (14 - speed / 2) * 0.8
  let velocity := blendVelocity velocity (back.unit.scale returnSpeed) inertia
  let aimY := if yoyo.returning || timedOut then yoyo.aimY
    else if held then (yoyo.target owner aim stats.maxRange).y else 0
  return { next.withVelocity velocity with aimX := -1, aimY }

/-- Full-tile part of native TileCollision. Valor's terrain box is its body,
and the normal style-99 projectile passes through platforms. -/
private def clipTerrain (world : World) (position velocity : Vector) : Vector := Id.run do
  let goal := position.add velocity
  let left := max 0 (tileAtPixel position.x - 1)
  let right := min (world.width : Int) (tileAtPixel (position.x + yoyoSize) + 2)
  let top := max 0 (tileAtPixel position.y - 1)
  let bottom := min (world.height : Int) (tileAtPixel (position.y + yoyoSize) + 2)
  let mut clipped := velocity
  let mut wall : TilePos := ⟨-1, -1⟩
  let mut surface : TilePos := ⟨-1, -1⟩
  let mut closestFloor := asSingle ((bottom + 3) * 16)
  for column in [:(right - left).toNat] do
    for row in [:(bottom - top).toNat] do
      let tile : TilePos := ⟨left + column, top + row⟩
      if !(world.tile tile).solid then continue
      let x := asSingle (tile.x * 16)
      let y := asSingle (tile.y * 16)
      if goal.x + yoyoSize ≤ x || goal.x ≥ x + 16 ||
          goal.y + yoyoSize ≤ y || goal.y ≥ y + 16 then continue
      if position.y + yoyoSize ≤ y then
        if y < closestFloor then
          surface := tile
          if surface.x != wall.x then
            clipped := { clipped with y := y - (position.y + yoyoSize) }
            closestFloor := y
      else if position.x + yoyoSize ≤ x || position.x ≥ x + 16 then
        wall := tile
        if wall.y != surface.y then
          let dx := if position.x + yoyoSize ≤ x then x - (position.x + yoyoSize)
            else x + 16 - position.x
          clipped := { clipped with x := dx }
        if wall.x == surface.x then clipped := { clipped with y := velocity.y }
      else if position.y ≥ y + 16 then
        surface := tile
        clipped := { clipped with y := y + 16 - position.y + 0.01 }
        if surface.y == wall.y then clipped := { clipped with x := velocity.x }
  return clipped

private def dryDisplacement (world : World) (position velocity : Vector) : Vector := Id.run do
  let direct := clipTerrain world position velocity
  let speed := velocity.length
  if speed ≤ 16 then return direct
  let direction := velocity.unit
  let direction := if direct.y == 0 then { direction with y := 0 } else direction
  let mut remainder := speed
  let mut moved : Vector := ⟨0, 0⟩
  for _ in [:3] do
    if remainder ≤ 0 then break
    let stride := min 16 remainder
    remainder := remainder - stride
    moved := moved.add (clipTerrain world (position.add moved) (direction.scale stride))
  return ⟨if (moved.x - velocity.x).abs < 0.0001 then velocity.x else moved.x,
          if (moved.y - velocity.y).abs < 0.0001 then velocity.y else moved.y⟩

private def overlapsLiquid (world : World) (position : Vector) (liquid : Water) : Bool := Id.run do
  -- Collision.WetCollision uses a centered 10×8 box with integer half-width.
  let left := position.x + 3
  let top := position.y + 4
  for x in [:(tileAtPixel (left + 10) - tileAtPixel left + 1).toNat] do
    for y in [:(tileAtPixel (top + 8) - tileAtPixel top + 1).toNat] do
      let tile : TilePos := ⟨tileAtPixel left + x, tileAtPixel top + y⟩
      let fill := liquid.level world tile
      if fill == 0 then continue
      let tileX := asSingle (tile.x * 16)
      let gap := asSingle (Water.capacity + 1 - fill : Nat) / 16
      let liquidTop := asSingle (tile.y * 16) + gap
      let liquidBottom := liquidTop + 16 - asSingle gap.toInt32.toInt
      if left < tileX + 16 && tileX < left + 10 && top < liquidBottom && liquidTop < top + 8 then
        return true
  return false

private def Yoyo.move (yoyo : Yoyo) (world : World) (preAI : Vector) (water magma : Water) : Yoyo :=
  let position := yoyo.center.sub ⟨yoyoHalf, yoyoHalf⟩
  let wet := overlapsLiquid world position water || overlapsLiquid world position magma
  let velocity := yoyo.velocity
  let clipped := if yoyo.returning then velocity else if wet then clipTerrain world position velocity
    else dryDisplacement world position velocity
  -- Type 564 leaves the clipped components unchanged. In liquid, generic
  -- movement halves each unblocked component and leaves blocked axes exact.
  let delta := if !wet then clipped else if yoyo.returning then preAI else
    ⟨if clipped.x == velocity.x then clipped.x * 0.5 else clipped.x,
     if clipped.y == velocity.y then clipped.y * 0.5 else clipped.y⟩
  let center := (position.add delta).add ⟨yoyoHalf, yoyoHalf⟩
  { yoyo.withVelocity clipped with x := center.x, y := center.y }

/-- Run style 99, then normal movement and collision. The caller scans
contacts after this step and applies each recoil in NPC order. -/
def Yoyo.advance (yoyo : Yoyo) (world : World) (player : Player) (aim : WorldAim) (facing : Int)
    (held : Bool) (water : Water := default) (magma : Water := default)
    (ownerAlive := true) (stats := valorYoyoStats) : Option (Yoyo × Int) := do
  if !ownerAlive || yoyo.recoilStalled || yoyo.x ≤ yoyoHalf || yoyo.y ≤ yoyoHalf ||
      yoyo.x + yoyoHalf ≥ asSingle (world.width * 16 : Nat) ||
      yoyo.y + yoyoHalf ≥ asSingle (world.height * 16 : Nat) then failure
  let owner := ownerCenter player
  let facing := if yoyo.x > owner.x + 2 then 1 else if yoyo.x < owner.x - 2 then -1 else facing
  let next ← yoyo.control owner aim held stats
  return (next.move world yoyo.velocity water magma, facing)

private def boxTile (world : World) (box : HitBox) : TilePos :=
  let x := (box.left.tdiv 16 + ((box.right - box.left).tdiv 16).tdiv 2).tdiv 16
  let y := (box.top.tdiv 16 + ((box.bottom - box.top).tdiv 16).tdiv 2).tdiv 16
  ⟨max 1 (min (world.width - 1 : Nat) x), max 1 (min (world.height - 1 : Nat) y)⟩

private def ownerCanSee (yoyo : Yoyo) (world : World) (player : Player) : Bool := Id.run do
  let start := boxTile world yoyo.hitBox
  let target := boxTile world (HitBox.ofNpcTarget player)
  let mut x := start.x
  let mut y := start.y
  let solid (column row : Int) := (world.tile ⟨column, row⟩).solid
  for _ in [:(target.x - x).natAbs + (target.y - y).natAbs] do
    if (target.x - x).natAbs > (target.y - y).natAbs then
      x := x + (if target.x > x then 1 else -1)
      if solid x (y - 1) && solid x (y + 1) then return false
    else
      y := y + (if target.y > y then 1 else -1)
      if solid (x - 1) y && solid (x + 1) y then return false
    if solid x y then return false
  return true

/-- A native NPC hit changes velocity and advances the deploy timer only
while the yoyo is out. Knockback points from owner to target in either phase. -/
def Yoyo.strike (yoyo : Yoyo) (world : World) (player : Player) (box : HitBox)
    (damage := valorYoyoDamage) : Yoyo × Nat × Int :=
  let target : Vector := ⟨pixels box.left + pixels (box.right - box.left) / 2,
    pixels box.top + pixels (box.bottom - box.top) / 2⟩
  let direction : Int := if target.x < (ownerCenter player).x then -1 else 1
  if yoyo.returning then (yoyo, damage, direction)
  else
    let visible := ownerCanSee yoyo world player
    let age := min maxNativeCounter (yoyo.age + if visible then 20 else 60)
    let offset := yoyo.center.sub target
    -- Native normalization of a zero offset produces NaNs, then kills on
    -- the next AI update. Keep the intervening save state finite.
    let recoilStalled := yoyo.recoilStalled || (offset.x == 0 && offset.y == 0)
    let velocity := if recoilStalled then ⟨0, 0⟩
      else (yoyo.velocity.scale (-0.5)).add (offset.unit.scale 16)
    let yoyo := { yoyo.withVelocity velocity with age, recoilStalled }
    (yoyo, if visible then damage else damage * 3 / 4, direction)

def Yoyo.recoil (yoyo : Yoyo) (world : World) (player : Player) (box : HitBox) : Yoyo :=
  (yoyo.strike world player box).1

end Lean4Terria
