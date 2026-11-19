import Lean4Terria.Ranged

namespace Lean4Terria

def magicMissileLifetime : Nat := 3600
def magicMissileTrailLength : Nat := 30

structure MagicMissileTrailPoint where
  x : Float32
  y : Float32
  rotation : Float32
  deriving Repr, Inhabited

instance : BEq MagicMissileTrailPoint where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.rotation.toBits == b.rotation.toBits

/-- Terraria Desktop projectile type 16. Positions and velocities use
original pixels and the trail starts with its most recent sample. -/
structure MagicMissile where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  rotation : Float32 := 0
  life : Nat := magicMissileLifetime
  guided : Bool := true
  slot : Option Nat := none
  trail : Array MagicMissileTrailPoint := #[]
  soundDelay : Nat := 0
  deriving Repr, Inhabited

instance : BEq MagicMissile where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.life == b.life &&
    a.guided == b.guided && a.slot == b.slot && a.trail == b.trail &&
    a.soundDelay == b.soundDelay

/-- Terrain kills before the direct-hit scan. A time expiry, indicated by
zero life, instead occurs after that scan. World edge removal is silent. -/
structure MagicMissileStep (α : Type) where
  missile : MagicMissile
  target : Option α
  hitTerrain : Bool := false
  deactivated : Bool := false
  loopSound : Bool := false
  hitDirection : Int := 1
  deriving Repr

private def asSingle (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (n : Int) : Float32 := asSingle n / 16
private def trunc (n : Float32) : Int := n.toInt32.toInt

private structure Vector where
  x : Float32
  y : Float32

private def Vector.add (a b : Vector) : Vector := ⟨a.x + b.x, a.y + b.y⟩
private def Vector.sub (a b : Vector) : Vector := ⟨a.x - b.x, a.y - b.y⟩
private def Vector.scale (a : Vector) (factor : Float32) : Vector := ⟨a.x * factor, a.y * factor⟩
private def Vector.length (a : Vector) : Float32 :=
  (a.x * a.x + a.y * a.y).toFloat.sqrt.toFloat32

private def Vector.unit (v : Vector) (fallback : Vector := ⟨0, 0⟩) : Vector :=
  if v.x == 0 && v.y == 0 then fallback else v.scale (1 / v.length)

private def Vector.lerp (a b : Vector) (amount : Float32) : Vector :=
  a.add ((b.sub a).scale amount)

private def Vector.moveTowards (a b : Vector) (amount : Float32) : Vector :=
  let delta := b.sub a
  if delta.length < amount then b else a.add (delta.unit.scale amount)

private def Vector.angle (v : Vector) : Float32 :=
  (Float.atan2 v.y.toFloat v.x.toFloat).toFloat32

private def Vector.turn (v : Vector) (angle : Float) : Vector :=
  let sin := angle.sin.toFloat32
  let cos := angle.cos.toFloat32
  ⟨v.x * cos - v.y * sin, v.x * sin + v.y * cos⟩

private def pi : Float32 := 3.1415927410125732
private def tau : Float32 := 6.2831854820251465
private def quarterTurn : Float32 := 0.7853981852531433

private def wrapAngle (angle : Float32) : Float32 :=
  if angle ≤ -pi then angle + tau else if angle > pi then angle - tau else angle

private def angleTowards (angle goal : Float32) : Float32 :=
  let angle := wrapAngle angle
  let goal := wrapAngle goal
  let angle := if goal - angle > pi then angle + tau
    else if angle - goal > pi then angle - tau else angle
  wrapAngle (angle + max (-quarterTurn) (min quarterTurn (goal - angle)))

private def angleToZero (angle : Float32) : Float32 :=
  let goal := if angle == pi then tau else 0
  wrapAngle (angle + (goal - angle) * 0.2)

private def ownerCenter (player : Player) : Vector :=
  ⟨pixels (bodyCenterX player), pixels (bodyCenterY player)⟩

private def aimPoint (aim : WorldAim) : Vector := ⟨pixels aim.x, pixels aim.y⟩
private def MagicMissile.center (missile : MagicMissile) : Vector := ⟨missile.x, missile.y⟩
private def MagicMissile.velocity (missile : MagicMissile) : Vector := ⟨missile.vx, missile.vy⟩

def MagicMissile.launch (player : Player) (aim : WorldAim) (facing : Int) : MagicMissile :=
  let origin := ownerCenter player
  let delta := (aimPoint aim).sub origin
  let speed := delta.length
  let velocity : Vector := if speed == 0 then ⟨asSingle facing * 6, 0⟩ else delta.scale (6 / speed)
  { x := origin.x, y := origin.y, vx := velocity.x, vy := velocity.y }

def MagicMissile.tile (missile : MagicMissile) : TilePos :=
  ⟨(trunc missile.x).tdiv 16, (trunc missile.y).tdiv 16⟩

private def MagicMissile.centeredBox (missile : MagicMissile) (width : Int) : HitBox :=
  let radius := asSingle width / 2
  let left := trunc (missile.x - radius) * 16
  let top := trunc (missile.y - radius) * 16
  ⟨left, top, left + width * 16, top + width * 16⟩

def MagicMissile.hitBox (missile : MagicMissile) : HitBox := missile.centeredBox 32
def MagicMissile.blastBox (missile : MagicMissile) : HitBox := missile.centeredBox 128

/-- Damage runs the generic tile-cut pass over the projectile's current
size. Kill temporarily resizes the same projectile before calling Damage. -/
def MagicMissile.cutTiles (missile : MagicMissile) (blast := false) : Array TilePos := Id.run do
  let radius : Float32 := if blast then 64 else 16
  let tileAt (pixel : Float32) := trunc (pixel / 16)
  let left := max 0 (tileAt (missile.x - radius))
  let top := max 0 (tileAt (missile.y - radius))
  let right := tileAt (missile.x - radius + radius * 2)
  let bottom := tileAt (missile.y - radius + radius * 2)
  let mut tiles := #[]
  for x in [:(right - left + 1).toNat] do
    for y in [:(bottom - top + 1).toNat] do
      tiles := tiles.push ⟨left + x, top + y⟩
  return tiles

private def nearWorld (world : World) (x y : Float32) : Bool :=
  x.isFinite && y.isFinite && -32 ≤ x && x ≤ asSingle (world.width * 16 + 32 : Nat) &&
    -32 ≤ y && y ≤ asSingle (world.height * 16 + 32 : Nat)

def MagicMissile.valid (missile : MagicMissile) (world : World) : Bool :=
  nearWorld world missile.x missile.y && missile.vx.isFinite && missile.vy.isFinite &&
    missile.velocity.length ≤ 32.001 && missile.rotation.isFinite && missile.rotation.abs ≤ pi &&
    0 < missile.life && missile.life ≤ magicMissileLifetime && missile.soundDelay ≤ 10 &&
    missile.slot.all (· < maxProjectileSlots) && missile.trail.size ≤ magicMissileTrailLength &&
    missile.trail.all fun point =>
      nearWorld world point.x point.y && point.rotation.isFinite && point.rotation.abs ≤ pi

private def reachableAim (world : World) (owner : Vector) (point : Vector) : Vector :=
  -- Move the original 1920×1200 view rectangle inside the playable bounds.
  let width : Int := min 1920 (world.width * 16 : Int)
  let height : Int := min 1200 (world.height * 16 : Int)
  let halfWidth := asSingle width / 2
  let halfHeight := asSingle height / 2
  let left := max 0 (min (world.width * 16 - width : Int) (trunc (owner.x - halfWidth)))
  let top := max 0 (min (world.height * 16 - height : Int) (trunc (owner.y - halfHeight)))
  let center : Vector := ⟨asSingle (left + width.tdiv 2), asSingle (top + height.tdiv 2)⟩
  let delta := point.sub center
  let scaleX := if delta.x.abs > halfWidth then halfWidth / delta.x.abs else 1
  let scaleY := if delta.y.abs > halfHeight then halfHeight / delta.y.abs else 1
  center.add (delta.scale (min scaleX scaleY))

private def targetCenter (box : HitBox) : Vector :=
  ⟨pixels box.left + pixels (box.right - box.left) / 2,
   pixels box.top + pixels (box.bottom - box.top) / 2⟩

private def sightTile (world : World) (box : HitBox) : TilePos :=
  let x := (box.left.tdiv 16 + ((box.right - box.left).tdiv 16).tdiv 2).tdiv 16
  let y := (box.top.tdiv 16 + ((box.bottom - box.top).tdiv 16).tdiv 2).tdiv 16
  ⟨max 1 (min (world.width - 1 : Nat) x), max 1 (min (world.height - 1 : Nat) y)⟩

/-- The full-tile portion of Collision.CanHit, used for the initial NPC
search. Existing locks continue to steer through loss of line of sight. -/
private def targetVisible (world : World) (origin destination : TilePos) : Bool := Id.run do
  let mut x := origin.x
  let mut y := origin.y
  let solid (column row : Int) := (world.tile ⟨column, row⟩).solid
  for _ in [:(destination.x - x).natAbs + (destination.y - y).natAbs] do
    if (destination.x - x).natAbs > (destination.y - y).natAbs then
      x := x + (if destination.x > x then 1 else -1)
      if solid x (y - 1) && solid x (y + 1) then return false
    else
      y := y + (if destination.y > y then 1 else -1)
      if solid (x - 1) y && solid (x + 1) y then return false
    if solid x y then return false
  return true

private def MagicMissile.findTarget (missile : MagicMissile) (world : World)
    (targets : Array (α × HitBox)) : Option (α × HitBox) := Id.run do
  let origin := sightTile world missile.hitBox
  let mut distance : Float32 := 800
  let mut result := none
  for entry in targets do
    let current := ((targetCenter entry.2).sub missile.center).length
    if current < distance && targetVisible world origin (sightTile world entry.2) then
      result := some entry
      distance := current
  return result

private def steer (velocity delta : Vector) (amount : Float32) : Vector × Bool :=
  let distance := delta.length
  if distance < 64 then
    let velocity := (velocity.scale 0.3).add (delta.scale 0.3)
    (velocity, velocity.length ≥ 2)
  else
    let target := delta.unit.scale (min 32 distance)
    let velocity := if velocity.length < 4 then
        velocity.add (((velocity.unit.turn quarterTurn.toFloat).unit).scale 4)
      else velocity
    (velocity.lerp target amount, true)

/-- Calculate AI_009 before movement. Release onto a target coasts for one
tick, and a lost lock takes a tick to clear before another target search. -/
private def MagicMissile.control [BEq α] (missile : MagicMissile) (world : World) (player : Player)
    (aim : WorldAim) (held : Bool) (targets : Array (α × HitBox)) (target : Option α) :
    Vector × Bool × Option α × Nat × Bool :=
  let velocity := missile.velocity
  if missile.guided then
    if held then
      let point := reachableAim world (ownerCenter player) (aimPoint aim)
      if point.x > 0 && point.y > 0 then
        let (velocity, rotate) := steer velocity (point.sub missile.center) 1
        (velocity, true, none, max 60 missile.life, rotate)
      else (velocity, true, none, missile.life, false)
    else
      let target := (missile.findTarget world targets).map Prod.fst
      let velocity := if target.isSome then velocity else
        let direction := if velocity.length < 2 then
            let away := missile.center.sub (ownerCenter player)
            away.scale (1 / away.length)
          else velocity.unit
        direction.scale 32
      (velocity, false, target, missile.life, false)
  else
    let locked := match target with
      | some id => targets.find? (fun entry => entry.1 == id)
      | none => missile.findTarget world targets
    match locked with
    | some (id, box) =>
      -- Native's clamped GetLerpValue(200, 20, 1 - f) yields 1 for every f in [0,1].
      let (velocity, rotate) := steer velocity ((targetCenter box).sub missile.center) 0.2
      (velocity, false, some id, max 60 missile.life, rotate)
    | none =>
      let velocity := if velocity.length == 32 then velocity
        else velocity.moveTowards ((velocity.unit ⟨0, 1⟩).scale 32) 4
      (velocity, false, none, min 300 missile.life, true)

-- In the native movement pass style 9 shrinks terrain collision to 4×4,
-- allows platforms through and tests fast movement in four-pixel steps.
private def clipTerrain (world : World) (position velocity : Vector) : Vector := Id.run do
  let goal := position.add velocity
  let left := max 0 ((trunc position.x).tdiv 16 - 1)
  let right := min (world.width : Int) ((trunc (position.x + 4)).tdiv 16 + 2)
  let top := max 0 ((trunc position.y).tdiv 16 - 1)
  let bottom := min (world.height : Int) ((trunc (position.y + 4)).tdiv 16 + 2)
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
      if goal.x + 4 ≤ x || goal.x ≥ x + 16 || goal.y + 4 ≤ y || goal.y ≥ y + 16 then continue
      if position.y + 4 ≤ y then
        if y < closestFloor then
          surface := tile
          if surface.x != wall.x then
            clipped := { clipped with y := y - (position.y + 4) }
            closestFloor := y
      else if position.x + 4 ≤ x || position.x ≥ x + 16 then
        wall := tile
        if wall.y != surface.y then
          let dx := if position.x + 4 ≤ x then x - (position.x + 4) else x + 16 - position.x
          clipped := { clipped with x := dx }
        if wall.x == surface.x then clipped := { clipped with y := velocity.y }
      else if position.y ≥ y + 16 then
        surface := tile
        clipped := { clipped with y := y + 16 - position.y + 0.01 }
        if surface.y == wall.y then clipped := { clipped with x := velocity.x }
  return clipped

private def MagicMissile.blocked (missile : MagicMissile) (world : World) (velocity : Vector)
    (waterMotion : Bool) : Bool := Id.run do
  let origin := missile.center.sub ⟨2, 2⟩
  let direct := clipTerrain world origin velocity
  let speed := velocity.length
  if waterMotion || speed ≤ 4 then return direct.x != velocity.x || direct.y != velocity.y
  let direction := velocity.unit
  let direction := if direct.y == 0 then { direction with y := 0 } else direction
  let mut remainder := speed
  let mut moved : Vector := ⟨0, 0⟩
  for _ in [:9] do
    if remainder ≤ 0 then break
    let stride := min 4 remainder
    remainder := remainder - stride
    moved := moved.add (clipTerrain world (origin.add moved) (direction.scale stride))
  return (moved.x - velocity.x).abs ≥ 0.0001 || (moved.y - velocity.y).abs ≥ 0.0001

private def MagicMissile.recordTrail (missile : MagicMissile) : MagicMissile := Id.run do
  let mut trail := #[⟨missile.x, missile.y, missile.rotation⟩] ++
    missile.trail.extract 0 (magicMissileTrailLength - 1)
  for offset in [:trail.size - 1] do
    let index := trail.size - 1 - offset
    let previous := trail[index - 1]!
    let point := trail[index]!
    let a : Vector := ⟨point.x - 16, point.y - 16⟩
    let b : Vector := ⟨previous.x - 16, previous.y - 16⟩
    let adjusted := if (a.sub b).length > 2 then a.lerp b 0.65 else a
    let rotation := (b.sub adjusted).unit.angle
    trail := trail.set! index ⟨adjusted.x + 16, adjusted.y + 16, rotation⟩
  return { missile with trail }

private def hitDirection (vx : Float32) : Int := if vx < 0 then -1 else 1

def MagicMissile.advance [BEq α] (missile : MagicMissile) (world : World) (player : Player)
    (aim : WorldAim) (held : Bool) (targets : Array (α × HitBox)) (target : Option α)
    (waterMotion := false) (stopBeforeMovement := false) :
    MagicMissileStep α := Id.run do
  if missile.x ≤ 16 || missile.y ≤ 16 || missile.x + 16 ≥ asSingle (world.width * 16 : Nat) ||
      missile.y + 16 ≥ asSingle (world.height * 16 : Nat) then
    return { missile, target := none, deactivated := true }
  let (velocity, guided, target, life, rotate) := missile.control world player aim held targets target
  if !velocity.x.isFinite || !velocity.y.isFinite then
    -- Releasing a stationary missile exactly at the owner yields a
    -- non-finite native position and a silent removal.
    return { missile, target := none, deactivated := true }
  let rotation := if rotate && (velocity.x != 0 || velocity.y != 0) then
      angleTowards missile.rotation velocity.angle
    else angleToZero missile.rotation
  let soundDelay := missile.soundDelay - 1
  let loopSound := soundDelay == 0 && velocity.x.abs + velocity.y.abs > 2
  let soundDelay := if loopSound then 10 else soundDelay
  let next := { missile with vx := velocity.x, vy := velocity.y, guided, life, rotation, soundDelay }
  if stopBeforeMovement then
    return { missile := next, target, loopSound, hitDirection := hitDirection missile.vx }
  if missile.blocked world velocity waterMotion then
    return { missile := next, target, hitTerrain := true, loopSound, hitDirection := hitDirection missile.vx }
  let delta := if waterMotion then velocity.scale 0.5 else velocity
  let x := (missile.x - 16) + delta.x + 16
  let y := (missile.y - 16) + delta.y + 16
  let moved := { next with x, y, life := life - 1 }
  return { missile := moved.recordTrail, target, loopSound, hitDirection := hitDirection velocity.x }

end Lean4Terria
