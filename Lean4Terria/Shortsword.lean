import Lean4Terria.Ranged

namespace Lean4Terria

/-- Copper Shortsword projectile type 938. Velocity is in Terraria pixels
per subupdate. The projectile follows the owner, so age determines its offset. -/
structure ShortswordThrust where
  vx : Float32
  vy : Float32
  age : Nat := 0
  damage : Nat
  slot : Option Nat := none
  deriving Repr

instance : BEq ShortswordThrust where
  beq a b := a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.age == b.age && a.damage == b.damage && a.slot == b.slot

def shortswordSubupdates : Nat := 2
def shortswordLifetime : Nat := 16
def shortswordShootSpeed : Float32 := 2.1

private def asSingle (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (n : Int) : Float32 := asSingle n / 16

private def magnitude (x y : Float32) : Float32 :=
  (x * x + y * y).toFloat.sqrt.toFloat32

def ShortswordThrust.launch (player : Player) (targetX targetY : Int)
    (facing : Int) (damage : Nat) : ShortswordThrust :=
  let dx := pixels targetX - pixels (bodyCenterX player)
  let dy := pixels targetY - pixels (bodyCenterY player)
  if dx == 0 && dy == 0 then
    { vx := if facing < 0 then -shortswordShootSpeed else shortswordShootSpeed
      vy := 0, damage }
  else
    let scale := shortswordShootSpeed / magnitude dx dy
    { vx := dx * scale, vy := dy * scale, damage }

def ShortswordThrust.valid (thrust : ShortswordThrust) : Bool :=
  let speedSquared := thrust.vx * thrust.vx + thrust.vy * thrust.vy
  thrust.vx.isFinite && thrust.vy.isFinite && 4.40999 ≤ speedSquared &&
    speedSquared ≤ 4.41001 && thrust.age < shortswordLifetime && thrust.damage > 0

/-- Advance one AI_161 subupdate. The terminal age never runs damage. -/
def ShortswordThrust.advance (thrust : ShortswordThrust) : Option ShortswordThrust :=
  if thrust.age + 1 < shortswordLifetime then some { thrust with age := thrust.age + 1 }
  else none

/-- The center used for damage, after AI sets the 18-pixel projectile's
position and the generic movement pass adds one velocity step. -/
def ShortswordThrust.center (thrust : ShortswordThrust) (owner : Player) : Float32 × Float32 :=
  let x := pixels (bodyCenterX owner)
  let y := pixels (bodyCenterY owner)
  if thrust.age == 0 then (x, y) else
    let t := asSingle (thrust.age - 1 : Nat)
    ((x + thrust.vx * t - 9) + thrust.vx + 9,
     (y + thrust.vy * t - 9) + thrust.vy + 9)

def ShortswordThrust.rotation (thrust : ShortswordThrust) : Float32 :=
  (Float.atan2 thrust.vy.toFloat thrust.vx.toFloat).toFloat32 + 1.5707963705062866

def ShortswordThrust.opacity (thrust : ShortswordThrust) : Float32 :=
  let t := asSingle thrust.age
  let fadeIn := min 1 (t / 7)
  let fadeOut := max 0 (min 1 ((t - 16) / (-4)))
  -- Terraria stores the fade in a truncated inverse alpha channel.
  1 - (asSingle ((255 * (1 - fadeIn * fadeOut)).toInt32.toInt)) / 255

def ShortswordThrust.spriteDirection (thrust : ShortswordThrust) : Int :=
  if thrust.vx < 0 then -1 else 1

/-- PlotTileLine follows a ten-pixel path with a ten-pixel cross-section.
Each path spans at most one tile per axis. PlotLine excludes its endpoint,
and the cross-section includes a side step when both tile coordinates change. -/
def ShortswordThrust.cutTiles (thrust : ShortswordThrust) (owner : Player) : Array TilePos :=
  let (x, y) := thrust.center owner
  let speed := magnitude thrust.vx thrust.vy
  let (unitX, unitY) := if speed == 0 then (0, -1)
    else (thrust.vx * (1 / speed), thrust.vy * (1 / speed))
  let endX := x + unitX * 10
  let endY := y + unitY * 10
  let dx := endX.toFloat - x.toFloat
  let dy := endY.toFloat - y.toFloat
  let length := (dx * dx + dy * dy).sqrt
  let offX := -(dy / length) * 5
  let offY := (dx / length) * 5
  let tile (px py : Float) : TilePos :=
    ⟨px.toInt64.toInt / 16, py.toInt64.toInt / 16⟩
  let first := tile (x.toFloat - offX) (y.toFloat - offY)
  let last := tile (x.toFloat + offX) (y.toFloat + offY)
  if first.x != last.x && first.y != last.y then #[first, ⟨first.x, last.y⟩]
  else #[first]

private structure Point where
  x : Float32
  y : Float32

private def Point.sub (a b : Point) : Point := ⟨a.x - b.x, a.y - b.y⟩

private def Point.cross (a b : Point) : Float32 := a.x * b.y - a.y * b.x

private def segmentCrosses (origin direction a b : Point) : Bool :=
  let edge := b.sub a
  let denominator := direction.cross edge
  if denominator == 0 then false else
    let delta := a.sub origin
    let t := delta.cross edge / denominator
    let u := delta.cross direction / denominator
    0 ≤ t && t ≤ 1 && 0 ≤ u && u ≤ 1

/-- The original CheckAABBvLineCollision test used by Colliding for type 938.
It tests corners inside the oriented strip and crossings of its two long edges. -/
def ShortswordThrust.intersects (thrust : ShortswordThrust) (owner : Player) (target : HitBox) : Bool :=
  let (x, y) := thrust.center owner
  let dx := (x + 6 * thrust.vx) - x
  let dy := (y + 6 * thrust.vy) - y
  let left := pixels target.left
  let top := pixels target.top
  let width := pixels (target.right - target.left)
  let height := pixels (target.bottom - target.top)
  let broadX := x + (if dx > 0 then 0 else dx) - 5
  let broadY := y + (if dy > 0 then 0 else dy) - 5
  if left ≥ broadX + (dx.abs + 10) || top ≥ broadY + (dy.abs + 10) ||
      left + width ≤ broadX || top + height ≤ broadY then false else
    let angle := -(Float.atan2 dy.toFloat dx.toFloat).toFloat32
    let cos := angle.toFloat.cos.toFloat32
    let sin := angle.toFloat.sin.toFloat32
    let length := magnitude dx dy
    let rotate (px py : Float32) : Point :=
      ⟨px * cos - py * sin, px * sin + py * cos⟩
    let minX := left - x
    let minY := top - y
    let maxX := minX + width
    let maxY := minY + height
    let corners := [rotate minX minY, rotate maxX minY, rotate maxX maxY, rotate minX maxY]
    let inside := corners.any (fun corner => corner.y.abs < 5 && 0 ≤ corner.x && corner.x < length)
    let edges := corners.zip (corners.drop 1 ++ corners.take 1)
    inside || edges.any fun (a, b) =>
      segmentCrosses ⟨0, 5⟩ ⟨length, 0⟩ a b || segmentCrosses ⟨0, -5⟩ ⟨length, 0⟩ a b

private def tileAtPoint (world : World) (point : Point) : TilePos :=
  let column := (point.x / 16).toInt32.toInt
  let row := (point.y / 16).toInt32.toInt
  -- The compact map uses its lower rows for playable rooms.
  ⟨max 1 (min (world.width - 1 : Nat) column), max 1 (min (world.height - 1 : Nat) row)⟩

private def solid (world : World) (x y : Int) : Bool := (world.tile ⟨x, y⟩).solid

private def direction (delta : Int) : Int :=
  if delta < 0 then -1 else if delta > 0 then 1 else 0

def canHitTiles (world : World) (origin destination : TilePos) : Bool := Id.run do
  let mut x := origin.x
  let mut y := origin.y
  for _ in [:(destination.x - x).natAbs + (destination.y - y).natAbs] do
    if (destination.x - x).natAbs > (destination.y - y).natAbs then
      x := x + direction (destination.x - x)
      if solid world x (y - 1) && solid world x (y + 1) then return false
    else
      y := y + direction (destination.y - y)
      if solid world (x - 1) y && solid world (x + 1) y then return false
    if solid world x y then return false
  return true

private def canHitLine (world : World) (origin destination : TilePos) : Bool := Id.run do
  if origin == destination then return true
  let mut x := origin.x
  let mut y := origin.y
  let stepX := direction (destination.x - x)
  let stepY := direction (destination.y - y)
  let mut columns : Int := (destination.x - x).natAbs
  let mut rows : Int := (destination.y - y).natAbs
  let ratioX := if columns == 0 then 0 else if rows > 0 && columns > rows then
    asSingle columns / asSingle rows else 1
  let ratioY := if rows == 0 then 0 else if columns > 0 && rows > columns then
    asSingle rows / asSingle columns else 1
  let mut remainderX : Float32 := 0
  let mut remainderY : Float32 := 0
  let mut horizontal : Bool := y < destination.y
  for _ in [:2 * (columns.natAbs + rows.natAbs + 1)] do
    let accumulated := if horizontal then remainderX + ratioX else remainderY + ratioY
    let steps := accumulated.toUInt32.toNat
    if horizontal then remainderX := accumulated - asSingle steps
    else remainderY := accumulated - asSingle steps
    for _ in [:steps] do
      if solid world x y || (if horizontal then solid world x (y - 1) || solid world x (y + 1)
        else solid world (x - 1) y || solid world (x + 1) y) then return false
      if columns == 0 && rows == 0 then return true
      if horizontal then
        x := x + stepX
        columns := columns - 1
      else
        y := y + stepY
        rows := rows - 1
      if columns == 0 && rows == 0 && steps == 1 then return !solid world x y
    if horizontal && rows != 0 then horizontal := false
    else if !horizontal && columns != 0 then horizontal := true
    if solid world x y then return false
  return false

def meleeOwnerCanHit (world : World) (owner : Player) (facing : Int) (target : HitBox)
    (center : Float32 × Float32) : Bool :=
  let ownerWidth := playerWidth.tdiv 16
  let ownerHeight := playerHeight.tdiv 16
  let ownerCenter : Point :=
    ⟨pixels owner.x + asSingle ownerWidth / 2, pixels owner.y + asSingle ownerHeight / 2⟩
  let targetWidth := (target.right - target.left).tdiv 16
  let targetHeight := (target.bottom - target.top).tdiv 16
  let targetCenter : Point :=
    ⟨pixels target.left + asSingle targetWidth / 2, pixels target.top + asSingle targetHeight / 2⟩
  let integerCenterTile (x y width height : Int) :=
    tileAtPoint world ⟨asSingle (x.tdiv 16 + width.tdiv 2), asSingle (y.tdiv 16 + height.tdiv 2)⟩
  let ownerTile := integerCenterTile owner.x owner.y ownerWidth ownerHeight
  let targetHitTile := integerCenterTile target.left target.top targetWidth targetHeight
  let targetTile := tileAtPoint world targetCenter
  if magnitude (targetCenter.x - center.1) (targetCenter.y - center.2) > 1000 then false
  else if canHitTiles world ownerTile targetHitTile then true else
    let front := ownerCenter.x + asSingle ((facing * ownerWidth).tdiv 2)
    let shoulder := tileAtPoint world ⟨front, ownerCenter.y - asSingle ownerHeight / 3⟩
    let side := tileAtPoint world ⟨front, ownerCenter.y⟩
    let targetTop := tileAtPoint world ⟨targetCenter.x, targetCenter.y - asSingle (targetHeight / 3)⟩
    let targetBottom := tileAtPoint world ⟨targetCenter.x, targetCenter.y + asSingle (targetHeight / 3)⟩
    canHitLine world shoulder targetTop || canHitLine world shoulder targetTile ||
      canHitLine world side targetBottom

/-- The owner-to-target sight check applies to terrain-colliding NPCs.
The thrust continues to advance through its animation on a blocked hit. -/
def ShortswordThrust.reaches (thrust : ShortswordThrust) (world : World) (owner : Player)
    (facing : Int) (target : HitBox) (ignoreTerrain := false) : Bool :=
  thrust.age > 0 && thrust.age < shortswordLifetime && thrust.intersects owner target &&
    (ignoreTerrain || meleeOwnerCanHit world owner facing target (thrust.center owner))

end Lean4Terria
