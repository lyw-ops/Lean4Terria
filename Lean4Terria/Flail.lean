import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

/-- Projectile type 26 uses these states in order in AI style 15. -/
inductive FlailPhase where
  | spinning | outbound | returning | tethered | forcedReturn | ricochet | dropped
  deriving BEq, Repr, Inhabited

/-- Blue Moon's center and velocity are measured in original pixels. One AI
update and one movement pass run per simulation tick. -/
structure Flail where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  phase : FlailPhase := .spinning
  phaseTicks : Nat := 0
  spinTicks : Nat := 0
  collisionCount : Nat := 0
  slot : Option Nat := none
  rotation : Float32 := 0
  tileCollide : Bool := true
  spriteDirection : Int := 1
  deriving Repr, Inhabited

instance : BEq Flail where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.phase == b.phase && a.phaseTicks == b.phaseTicks && a.spinTicks == b.spinTicks &&
    a.collisionCount == b.collisionCount && a.slot == b.slot &&
    a.rotation.toBits == b.rotation.toBits && a.tileCollide == b.tileCollide &&
    a.spriteDirection == b.spriteDirection

def blueMoonDamage : Nat := 27
def blueMoonThrowTicks : Nat := 15

inductive FlailKind where
  | blueMoon | sunfury
  deriving BEq, Repr

private def FlailKind.baseDamage : FlailKind → Nat
  | .blueMoon => blueMoonDamage
  | .sunfury => 32

private def FlailKind.baseKnockback : FlailKind → Float32
  | .blueMoon => 6
  | .sunfury => 6.75

private def maxNativeCounter : Nat := 16777216

private def asSingle (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (n : Int) : Float32 := asSingle n / 16
private def tileAtPixel (n : Float32) : Int := (n / 16).toInt32.toInt

private structure Vector where
  x : Float32
  y : Float32

private def Vector.add (a b : Vector) : Vector := ⟨a.x + b.x, a.y + b.y⟩
private def Vector.sub (a b : Vector) : Vector := ⟨a.x - b.x, a.y - b.y⟩
private def Vector.scale (a : Vector) (k : Float32) : Vector := ⟨a.x * k, a.y * k⟩
private def Vector.dot (a b : Vector) : Float32 := a.x * b.x + a.y * b.y
private def Vector.length (a : Vector) : Float32 := (a.dot a).toFloat.sqrt.toFloat32
private def Vector.unit (a : Vector) (fallback : Vector := ⟨0, 0⟩) : Vector :=
  if a.x == 0 && a.y == 0 then fallback else a.scale (1 / a.length)

private def Vector.moveTowards (current target : Vector) (acceleration : Float32) : Vector :=
  let delta := target.sub current
  if delta.length < acceleration then target else current.add (delta.unit.scale acceleration)

private def Vector.rotation (v : Vector) : Float32 :=
  (Float.atan2 v.y.toFloat v.x.toFloat).toFloat32

private def ownerCenter (player : Player) : Vector :=
  ⟨pixels (bodyCenterX player), pixels (bodyCenterY player)⟩

private def Flail.center (flail : Flail) : Vector := ⟨flail.x, flail.y⟩
private def Flail.velocity (flail : Flail) : Vector := ⟨flail.vx, flail.vy⟩
private def Flail.withVelocity (flail : Flail) (v : Vector) : Flail :=
  { flail with vx := v.x, vy := v.y }

def Flail.launch (player : Player) : Flail :=
  let center := ownerCenter player
  { x := center.x, y := center.y }

def Flail.tile (flail : Flail) : TilePos :=
  ⟨tileAtPixel flail.x, tileAtPixel flail.y⟩

def Flail.localCooldown (flail : Flail) : Nat :=
  if flail.phase == .spinning then 15 else 10

def Flail.canDamage (flail : Flail) : Bool :=
  flail.phase != .spinning || flail.spinTicks > 12

def Flail.damage (flail : Flail) (kind : FlailKind := .blueMoon) (paired := false) : Nat :=
  match flail.phase with
  | .spinning => if paired then kind.baseDamage * 24 / 25 else kind.baseDamage * 6 / 5
  | .outbound | .returning => kind.baseDamage * 2
  | _ => kind.baseDamage

def Flail.knockback (flail : Flail) (kind : FlailKind := .blueMoon) : Float32 :=
  match flail.phase with
  | .spinning => kind.baseKnockback * 0.35
  | .dropped => kind.baseKnockback * 0.5
  | _ => kind.baseKnockback

-- SetDefaults scales the 22-pixel projectile dimensions by 0.8 and truncates.
private def flailSize : Float32 := 17
private def flailHalf : Float32 := flailSize / 2

def Flail.hitBox (flail : Flail) : HitBox :=
  let left := (flail.x - flailHalf).toInt32.toInt * 16
  let top := (flail.y - flailHalf).toInt32.toInt * 16
  ⟨left, top, left + 272, top + 272⟩

/-- The ordinary projectile tile-cut pass scans the head rectangle,
including the tile at the right and bottom edges. -/
def Flail.cutTiles (flail : Flail) : Array TilePos := Id.run do
  if !flail.canDamage then return #[]
  let left := tileAtPixel (flail.x - flailHalf)
  let top := tileAtPixel (flail.y - flailHalf)
  let right := tileAtPixel (flail.x - flailHalf + flailSize)
  let bottom := tileAtPixel (flail.y - flailHalf + flailSize)
  let mut tiles := #[]
  for x in [:(right - left + 1).toNat] do
    for y in [:(bottom - top + 1).toNat] do
      tiles := tiles.push ⟨left + x, top + y⟩
  return tiles

/-- Spinning damage fills an ellipse around the owner. The image's orbit and
the extended flail's collision rectangle are distinct from that ellipse. -/
def Flail.intersects (flail : Flail) (player : Player) (target : HitBox) : Bool :=
  if flail.phase != .spinning then flail.hitBox.intersects target
  else
    let owner := ownerCenter player
    let dx := max (pixels target.left) (min owner.x (pixels target.right)) - owner.x
    let dy := max (pixels target.top) (min owner.y (pixels target.bottom)) - owner.y
    (⟨dx, dy / (if dy > 0 then 0.4 else 0.8)⟩ : Vector).length ≤ 55

def Flail.valid (flail : Flail) (world : World) : Bool :=
  flail.x.isFinite && flail.y.isFinite && flail.vx.isFinite && flail.vy.isFinite &&
    flail.rotation.isFinite && world.contains flail.tile && 0 ≤ flail.x && 0 ≤ flail.y &&
    flail.velocity.length ≤ 64 && flail.rotation.abs ≤ 10 &&
    flail.spinTicks ≤ maxNativeCounter && flail.collisionCount ≤ maxNativeCounter &&
    (flail.spriteDirection == -1 || flail.spriteDirection == 1) &&
    (flail.slot.all (· < maxProjectileSlots)) &&
    (match flail.phase with
    | .spinning => flail.phaseTicks == 0 && flail.vx == 0 && flail.vy == 0 && flail.tileCollide
    | .outbound | .ricochet => flail.phaseTicks ≤ blueMoonThrowTicks && flail.tileCollide
    | .tethered => flail.phaseTicks ≤ 1
    | .returning | .dropped | .forcedReturn => flail.phaseTicks == 0)

private def Flail.enter (flail : Flail) (phase : FlailPhase) : Flail :=
  { flail with phase, phaseTicks := 0 }

private def facingTowards (owner head : Vector) : Int := if head.x > owner.x then 1 else -1

private def Flail.updateAI (flail : Flail) (player : Player) (aim : WorldAim) (facing : Int)
    (held : Bool) (kind : FlailKind) (mirrored : Bool) (meleeSpeed : Float32) : Option (Flail × Int) := do
  let owner := ownerCenter player
  let head := flail.center
  let velocity := flail.velocity
  let back := owner.sub head
  let distance := back.length
  let sunfury := kind == .sunfury
  let throwSpeed : Float32 := (if sunfury then 17 else 16) * meleeSpeed
  let returnSpeed : Float32 := if sunfury then 14 else 13
  let forcedSpeed : Float32 := if sunfury then 18 else 17
  if distance > 900 then failure
  let ownerFacing := facingTowards owner head
  match flail.phase with
  | .spinning =>
    let toward := (⟨pixels aim.x - owner.x, pixels aim.y - owner.y⟩ : Vector).unit
    let toward := toward.unit ⟨asSingle facing, 0⟩
    let nextFacing : Int := if (if mirrored then toward.x < 0 else toward.x > 0) then 1 else -1
    if !held then
      let velocity := (toward.scale throwSpeed).add ⟨pixels player.vx, pixels player.vy⟩
      let velocity := if mirrored then { velocity with x := -velocity.x } else velocity
      let launched := { flail with phase := .outbound, x := owner.x, y := owner.y }
      return (launched.withVelocity velocity, nextFacing)
    let spinTicks := min maxNativeCounter (flail.spinTicks + 1)
    let direction := asSingle nextFacing
    let angle : Float32 := 31.41592788696289 * (asSingle spinTicks / 60) * direction
    let cos := angle.toFloat.cos.toFloat32
    let sin := angle.toFloat.sin.toFloat32
    -- FNA's one-argument Vector2 constructor fills both components.
    let ox := direction * cos - direction * sin
    let oy := (direction * sin + direction * cos) * 0.8
    let oy := if oy > 0 then oy * 0.5 else oy
    let radius : Float32 := if sunfury then 37 else 32
    return ({ flail with x := owner.x + ox * radius, y := owner.y + oy * radius, spinTicks }, nextFacing)
  | .outbound =>
    if held then
      return ((flail.enter .dropped).withVelocity (velocity.scale 0.2), facing)
    if flail.phaseTicks ≥ blueMoonThrowTicks || distance ≥ 800 then
      return ((flail.enter .returning).withVelocity (velocity.scale 0.3), ownerFacing)
    return ({ flail with phaseTicks := flail.phaseTicks + 1 }, ownerFacing)
  | .returning =>
    if distance ≤ returnSpeed then failure
    if held then
      return ((flail.enter .dropped).withVelocity (velocity.scale 0.2), facing)
    let velocity := (velocity.scale 0.98).moveTowards (back.unit.unit.scale returnSpeed) 3
    return (flail.withVelocity velocity, ownerFacing)
  | .tethered =>
    if !held then return (flail.enter .forcedReturn, facing)
    let tileCollide := flail.phaseTicks == 1 && distance ≤ 240
    let velocity :=
      if distance > 60 then
        let goal := back.unit.unit.scale 14
        let velocity := if distance ≥ 240 then (velocity.scale 0.5).moveTowards goal 14 else velocity
        (velocity.scale 0.98).moveTowards goal 1
      else
        let velocity := if velocity.length < 6 then ⟨velocity.x * 0.96, velocity.y + 0.2⟩ else velocity
        if player.vx == 0 then ⟨velocity.x * 0.96, velocity.y⟩ else velocity
    let phaseTicks := if tileCollide then 1 else 0
    return ({ flail.withVelocity velocity with tileCollide, phaseTicks }, ownerFacing)
  | .forcedReturn =>
    if distance ≤ forcedSpeed then failure
    let returnDirection := back.unit.unit
    let velocity := (velocity.scale 0.98).moveTowards (returnDirection.scale forcedSpeed) 6
    let afterStep := (owner.sub (head.add velocity)).unit.unit
    if returnDirection.dot afterStep < 0 then failure
    return ({ flail.withVelocity velocity with tileCollide := false }, ownerFacing)
  | .ricochet =>
    if flail.phaseTicks ≥ blueMoonThrowTicks then return (flail.enter .dropped, facing)
    let next := { flail with phaseTicks := flail.phaseTicks + 1 }
    return (next.withVelocity ⟨velocity.x * 0.95, velocity.y + 0.6⟩, ownerFacing)
  | .dropped =>
    if !held || distance > 400 then return (flail.enter .forcedReturn, facing)
    return ({ flail with vx := velocity.x * 0.95, vy := velocity.y + 0.8 }, ownerFacing)

/-- Full-tile part of native TileCollision. Blue Moon falls through one-way
platforms and the prototype has no slopes or half blocks. Native keeps track
of the horizontal and vertical obstacles to resolve touching corners. -/
private def tileDisplacement (world : World) (position velocity : Vector) : Vector := Id.run do
  let endPoint := position.add velocity
  let left := max 0 (tileAtPixel position.x - 1)
  let right := min (world.width : Int) (tileAtPixel (position.x + flailSize) + 2)
  let top := max 0 (tileAtPixel position.y - 1)
  let bottom := min (world.height : Int) (tileAtPixel (position.y + flailSize) + 2)
  let mut result := velocity
  let mut wall : TilePos := ⟨-1, -1⟩
  let mut surface : TilePos := ⟨-1, -1⟩
  let mut closestFloor := asSingle ((bottom + 3) * 16)
  for column in [:(right - left).toNat] do
    for row in [:(bottom - top).toNat] do
      let tile : TilePos := ⟨left + column, top + row⟩
      if !(world.tile tile).solid then continue
      let x := asSingle (tile.x * 16)
      let y := asSingle (tile.y * 16)
      if endPoint.x + flailSize ≤ x || endPoint.x ≥ x + 16 ||
          endPoint.y + flailSize ≤ y || endPoint.y ≥ y + 16 then continue
      if position.y + flailSize ≤ y then
        if y < closestFloor then
          surface := tile
          if surface.x != wall.x then
            result := { result with y := y - (position.y + flailSize) }
            closestFloor := y
      else if position.x + flailSize ≤ x || position.x ≥ x + 16 then
        wall := tile
        if wall.y != surface.y then
          let dx := if position.x + flailSize ≤ x then x - (position.x + flailSize)
            else x + 16 - position.x
          result := { result with x := dx }
        if surface.x == wall.x then result := { result with y := velocity.y }
      else if position.y ≥ y + 16 then
        surface := tile
        result := { result with y := y + 16 - position.y + 0.01 }
        if surface.y == wall.y then result := { result with x := velocity.x }
  return result

/-- The dry native collision pass breaks displacements longer than sixteen
pixels into steps. It accumulates their clamped displacements before bounce. -/
private def dryDisplacement (world : World) (position velocity : Vector) : Vector := Id.run do
  let speed := velocity.length
  let direct := tileDisplacement world position velocity
  if speed ≤ 16 then return direct
  let direction := velocity.unit
  let direction := if direct.y == 0 then { direction with y := 0 } else direction
  let mut remainder := speed
  let mut moved : Vector := ⟨0, 0⟩
  for _ in [:4] do
    if remainder ≤ 0 then break
    let stride := min 16 remainder
    remainder := remainder - stride
    moved := moved.add (tileDisplacement world (position.add moved) (direction.scale stride))
  return ⟨if (moved.x - velocity.x).abs < 0.0001 then velocity.x else moved.x,
          if (moved.y - velocity.y).abs < 0.0001 then velocity.y else moved.y⟩

private def overlapsLiquid (world : World) (position : Vector) (liquid : Water) : Bool := Id.run do
  -- WetCollision checks a centered 10×8 box using the integer half-width.
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

private def Flail.move (flail : Flail) (world : World) (oldVelocity : Vector)
    (water magma : Water) : Flail := Id.run do
  let position := flail.center.sub ⟨flailHalf, flailHalf⟩
  let wet := overlapsLiquid world position water || overlapsLiquid world position magma
  let velocity := flail.velocity
  let mut next := flail
  let mut position := position
  let mut delta := if wet then oldVelocity else velocity
  if flail.tileCollide then
    let clipped := if wet then tileDisplacement world position velocity
      else dryDisplacement world position velocity
    let hitX := clipped.x != velocity.x
    let hitY := clipped.y != velocity.y
    if hitX || hitY then
      let bounce : Float32 := match flail.phase with
        | .outbound | .ricochet => 0.4
        | .dropped => 0
        | _ => 0.2
      delta := ⟨if hitX then -velocity.x * bounce else clipped.x,
                if hitY then -velocity.y * bounce else clipped.y⟩
      let hitAxes := (if hitX then 1 else 0) + (if hitY then 1 else 0)
      next := { flail.withVelocity delta with
        collisionCount := min maxNativeCounter (flail.collisionCount + hitAxes) }
      if next.phase == .outbound then
        position := position.sub clipped
        next := { next with phase := .ricochet }
      if next.phase == .returning && next.collisionCount ≥ 10 then
        next := next.enter .forcedReturn
    else if wet then
      delta := velocity.scale 0.5
  return { next with x := position.x + delta.x + flailHalf, y := position.y + delta.y + flailHalf }

/-- The owner and cursor are sampled after the player's movement. Collision
can change the phase before the caller tests damage, while the owner terrain
gate is determined by the phase on entry to this update. -/
def Flail.advance (flail : Flail) (world : World) (player : Player) (aim : WorldAim)
    (facing : Int) (held : Bool) (water : Water := default) (magma : Water := default)
    (kind : FlailKind := .blueMoon) (mirrored := false) (meleeSpeed : Float32 := 1) :
    Option (Flail × Int) := do
  let (next, nextFacing) ← flail.updateAI player aim facing held kind mirrored meleeSpeed
  let velocity := next.velocity
  let rotation := if velocity.length > 1 then velocity.rotation + velocity.x * 0.1
    else flail.rotation + velocity.x * 0.1
  let next := { next with rotation, spriteDirection := if velocity.x > 0 then 1 else -1 }
  let moved := next.move world flail.velocity water magma
  if !world.contains moved.tile || moved.x < 0 || moved.y < 0 then failure
  return (moved, nextFacing)

end Lean4Terria
