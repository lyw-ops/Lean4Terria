import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

inductive AngryBonesKind where
  | regular | short | bigBoned | big | bigMuscle | bigHelmet
  deriving BEq, Repr, Inhabited

def AngryBonesKind.sprite : AngryBonesKind → Nat
  | .regular | .short | .bigBoned => 31
  | .big => 294
  | .bigMuscle => 295
  | .bigHelmet => 296

def AngryBonesKind.scale : AngryBonesKind → Float32
  | .short => 0.9
  | .bigBoned => 1.15
  | _ => 1

def AngryBonesKind.width : AngryBonesKind → Nat
  | .short => 16
  | .bigBoned => 20
  | _ => 18

def AngryBonesKind.height : AngryBonesKind → Nat
  | .short => 36
  | .bigBoned => 46
  | _ => 40

def AngryBonesKind.health : AngryBonesKind → Nat
  | .regular => 80
  | .short => 72
  | .bigBoned => 101
  | .big | .bigMuscle => 70
  | .bigHelmet => 120

def AngryBonesKind.damage : AngryBonesKind → Nat
  | .regular => 26
  | .short => 23
  | .bigBoned => 32
  | .big => 34
  | .bigMuscle => 28
  | .bigHelmet => 24

def AngryBonesKind.defense : AngryBonesKind → Nat
  | .regular => 8
  | .short => 7
  | .bigBoned => 9
  | .big => 6
  | .bigMuscle => 12
  | .bigHelmet => 14

def AngryBonesKind.coins : AngryBonesKind → Nat
  | .short => 117
  | .bigBoned => 149
  | _ => 130

def AngryBonesKind.npcSlots : AngryBonesKind → Nat
  | .bigBoned => 2
  | _ => 1

def AngryBonesKind.frameCount : AngryBonesKind → Nat
  | .big => 16
  | _ => 15

def AngryBonesKind.walkSpeed : AngryBonesKind → Float32
  | .big => 2
  | .bigMuscle => 1.75
  | .bigHelmet => 1.25
  | _ => 1.5

def AngryBonesKind.knockbackMultiplier : AngryBonesKind → Float32
  | .bigBoned => 0.8 * (2 - 1.15)
  | .big => 0.9
  | .bigMuscle => 0.7
  | .bigHelmet => 0.6
  | _ => 0.8

/-- Position and velocity use original pixel units. The four AI slots preserve
the fighter's idle turn, door progress, door cadence and stuck timer. -/
structure AngryBones where
  id : Nat
  kind : AngryBonesKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := kind.health
  timeLeft : Nat := 937
  direction : Int := 1
  directionY : Int := -1
  spriteDirection : Int := -1
  oldX : Float32 := 0
  ai0 : Nat := 0
  ai1 : Nat := 0
  ai2 : Nat := 0
  ai3 : Nat := 0
  justHit : Bool := false
  wet : Bool := false
  frame : Nat := 0
  frameClock : Float := 0
  deriving Repr, Inhabited

instance : BEq AngryBones where
  beq a b :=
    a.id == b.id && a.kind == b.kind && a.health == b.health && a.timeLeft == b.timeLeft &&
    a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.oldX.toBits == b.oldX.toBits &&
    a.direction == b.direction && a.directionY == b.directionY && a.spriteDirection == b.spriteDirection &&
    a.ai0 == b.ai0 && a.ai1 == b.ai1 && a.ai2 == b.ai2 && a.ai3 == b.ai3 &&
    a.justHit == b.justHit && a.wet == b.wet && a.frame == b.frame &&
    a.frameClock.toBits == b.frameClock.toBits

private def pixels (units : Int) : Float32 := (Int32.ofInt units).toFloat32 / 16
private def asFloat (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def truncate (value : Float32) : Int := value.toInt32.toInt
private def edge (tile : Int) : Float32 := asFloat (tile * 16)
private def tileFloor (value : Float32) : Int := truncate ((value / 16).floor)
private def tileLast (value : Float32) : Int := truncate ((value / 16).ceil) - 1

def AngryBones.centerX (bones : AngryBones) : Float32 :=
  bones.x + asFloat (bones.kind.width / 2)

def AngryBones.centerY (bones : AngryBones) : Float32 :=
  bones.y + asFloat (bones.kind.height / 2)

private def AngryBones.bottom (bones : AngryBones) : Float32 := bones.y + asFloat bones.kind.height

def AngryBones.atFloor (id : Nat) (kind : AngryBonesKind) (column floor : Int) : AngryBones :=
  { id, kind, x := edge column + 8 - asFloat (kind.width / 2), y := edge floor - asFloat kind.height }

def AngryBones.hitBox (bones : AngryBones) : HitBox :=
  let left := truncate bones.x * 16
  let top := truncate bones.y * 16
  ⟨left, top, left + 16 * bones.kind.width, top + 16 * bones.kind.height⟩

def AngryBones.contact (bones : AngryBones) (player : Player) : Option Int :=
  if bones.hitBox.intersects (HitBox.ofNpcTarget player) then
    some (if bones.centerX < pixels player.x + asFloat (playerWidth / 32) then 1 else -1)
  else none

def AngryBones.valid (bones : AngryBones) (world : World) : Bool :=
  let margin : Float32 := 4096
  let validPosition := fun (position limit : Float32) =>
    position.isFinite && -margin ≤ position && position < limit + margin
  let horizontal := edge world.width
  let vertical := edge world.height
  bones.id > 0 && bones.health > 0 && bones.health ≤ bones.kind.health &&
    validPosition bones.x horizontal && validPosition bones.oldX horizontal && validPosition bones.y vertical &&
    bones.vx.isFinite && bones.vy.isFinite && bones.vx.abs ≤ 32 && bones.vy.abs ≤ 32 &&
    (bones.direction == -1 || bones.direction == 1) &&
    (bones.directionY == -1 || bones.directionY == 1) &&
    (bones.spriteDirection == -1 || bones.spriteDirection == 1) &&
    bones.ai0 ≤ 1 && bones.ai1 ≤ 10 && bones.ai2 < 60 && bones.ai3 ≤ 600 &&
    bones.frame < bones.kind.frameCount && bones.frameClock.isFinite &&
    0 ≤ bones.frameClock && bones.frameClock ≤ 6 && bones.timeLeft > 0 && bones.timeLeft ≤ 937

/-- NPC.StrikeNPC_Inner 1425–2101, for grounded fighter types. -/
def AngryBones.knockback (bones : AngryBones) (damage : Nat) (direction : Int) (force : Float32)
    (crit := false) : AngryBones :=
  let bones := { bones with justHit := true }
  if force ≤ 0 then bones else
  let resist := bones.kind.knockbackMultiplier
  let reduce := fun (value cap coefficient : Float32) =>
    if value > cap then cap + (value - cap) * coefficient else value
  let impulse := reduce (reduce (reduce (reduce (force * resist) 8 0.9) 10 0.8) 12 0.7) 14 0.6
  let impulse := min impulse 16
  let impulse := if crit then impulse * 1.4 else impulse
  let upward := -impulse * 0.75
  let side := asFloat direction
  if damage * 10 ≤ bones.kind.health then
    { bones with vx := impulse * side * resist, vy := upward * resist }
  else
    let vx := if direction != 0 && side * bones.vx < impulse then
        let vx := if side * bones.vx < 0 then bones.vx + side * impulse else bones.vx
        side * min impulse (side * (vx + side * impulse))
      else bones.vx
    let vy := if bones.vy > upward then max upward (bones.vy + upward) else bones.vy
    { bones with vx, vy }

private def AngryBones.retarget (bones : AngryBones) (player : Player) : AngryBones :=
  let targetX := asFloat (player.x.tdiv 16 + playerWidth / 32)
  let targetY := pixels (bodyCenterY player)
  { bones with
    direction := if targetX < bones.centerX then -1 else 1
    directionY := if targetY > bones.bottom then 1 else -1 }

private def AngryBones.chase (bones : AngryBones) (player : Player) (dead daylight : Bool)
    (surface : Nat) : AngryBones := Id.run do
  let opposing := bones.vy == 0 && bones.vx * asFloat bones.direction < 0
  let stalled := bones.x == bones.oldX || bones.ai3 ≥ 60 || opposing
  let stuck := if stalled then bones.ai3 + 1 else if bones.vx.abs.toFloat > 0.9 then bones.ai3 - 1 else bones.ai3
  let ai3 := if stuck > 600 || bones.justHit || bones.hitBox.intersects (HitBox.ofNpcTarget player)
    then 0 else stuck
  let directionY := if pixels (player.y + playerHeight) == bones.bottom then -1 else bones.directionY
  let mut next := { bones with ai3, directionY }
  if next.ai3 < 60 then
    if !dead then next := next.retarget player
  else
    if daylight && next.y < edge surface then
      next := { next with timeLeft := min 10 next.timeLeft }
    if next.vx != 0 then
      next := { next with ai0 := 0 }
    else if next.vy == 0 then
      if next.ai0 + 1 ≥ 2 then
        next := { next with ai0 := 0, direction := -next.direction, spriteDirection := -next.direction }
      else
        next := { next with ai0 := next.ai0 + 1 }
  let speed := next.kind.walkSpeed
  if next.vx.abs > speed then
    if next.vy == 0 then next := { next with vx := next.vx * 0.8 }
  else
    next := { next with vx := max (-speed) (min speed (next.vx + asFloat next.direction * 0.07)) }
  return next

private def solidAt (world : World) (x y : Int) : Bool :=
  (world.tile ⟨x, y⟩).solid

private def supportAt (world : World) (x y : Int) : Bool :=
  let tile := world.tile ⟨x, y⟩
  tile.solid || tile == .platform

/-- A normal step is evaluated against the projected horizontal position before
the jump probes. Half blocks and slopes have no corresponding world tiles. -/
private def AngryBones.stepUp (bones : AngryBones) (world : World) : AngryBones :=
  let width := asFloat bones.kind.width
  let x := bones.x + bones.vx
  let side : Int := if bones.vx < 0 then -1 else if bones.vx > 0 then 1 else 0
  let half : Int := bones.kind.width / 2
  let col := truncate ((x + asFloat half + asFloat ((half + 1) * side)) / 16)
  let row := truncate ((bones.bottom - 1) / 16)
  let stepTop := edge row
  if bones.vy < 0 || !world.contains ⟨col, row⟩ || edge col ≥ x + width || edge (col + 1) ≤ x ||
      !solidAt world col row || solidAt world col (row - 1) || solidAt world col (row - 2) ||
      solidAt world col (row - 3) || supportAt world (col - side) (row - 3) ||
      stepTop ≥ bones.bottom || bones.bottom - stepTop > 16.1 then bones
  else { bones with y := stepTop - asFloat bones.kind.height }

private def openDoorToward (world : World) (position : TilePos) (direction : Int) : Option World := do
  let base ← doorBase world position
  let cells := doorCells world base
  if !doorSupportValid world base ||
      !cells.all (fun pos => world.tile? { pos with x := pos.x + direction } == some .air) then failure
  return cells.foldl (fun world cell =>
    let openTile := match world.tile cell with
      | .doorClosed => Tile.doorOpen
      | .doorClosedMiddle => .doorOpenMiddle
      | _ => .doorOpenTop
    world.setTile cell openTile) world

private def AngryBones.isSupported (bones : AngryBones) (world : World) : Bool :=
  let left := truncate (bones.x + 8) |>.tdiv 16
  let right := truncate (bones.x + asFloat bones.kind.width - 8) |>.tdiv 16
  let floor := (truncate (bones.bottom + 7)).tdiv 16
  let roof := (truncate (bones.y - 9)).tdiv 16
  let columns := List.range' left.toNat (right - left + 1).toNat
  bones.vy == 0 && columns.any (fun col => supportAt world col floor) &&
    !columns.any (fun col => solidAt world col roof)

private def AngryBones.jumpNearPlayer (bones : AngryBones) (player : Player) : AngryBones :=
  let px := pixels player.x + asFloat (playerWidth / 32)
  let py := pixels player.y + asFloat (playerHeight / 32)
  if bones.vy == 0 && (bones.centerX - px).abs < 100 && (bones.centerY - py).abs < 50 &&
      bones.vx * asFloat bones.direction ≥ 1 then
    { bones with vx := max (-3) (min 3 (bones.vx * 2)), vy := -4 }
  else bones

/-- Classic style 3 for NPC 31 and 294–296 before the generic NPC collision
pass. The world represents full blocks, one-way platforms and wooden doors. -/
def AngryBones.advanceAI (bones : AngryBones) (world : World) (player : Player)
    (dead daylight : Bool) (surface : Nat) : AngryBones × World := Id.run do
  let wasStationary := bones.vx == 0 && !bones.justHit
  let next := bones.chase player dead daylight surface
  let supported := next.isSupported world
  let mut next := next.stepUp world
  if !supported then return ({ next with ai1 := 0, ai2 := 0 }, world)
  let col := truncate ((next.centerX + asFloat (15 * next.direction)) / 16)
  let row := truncate ((next.bottom - 15) / 16)
  let door : TilePos := ⟨col, row - 1⟩
  if (world.tile door).isDoor && (world.tile door).solid then
    if next.ai2 + 1 < 60 then
      return ({ next with ai2 := next.ai2 + 1, ai3 := 0 }, world)
    next := { next with ai1 := 10, ai2 := 0, ai3 := 0, vx := -0.5 * asFloat next.direction }
    return match openDoorToward world door next.direction with
      | some opened => (next, opened)
      | none => ({ next with ai3 := 60 }, world)
  if next.vx * asFloat next.spriteDirection > 0 then
    if next.kind.height ≥ 32 && solidAt world col (row - 2) then
      next := { next with vy := if solidAt world col (row - 3) then -8 else -7 }
    else if solidAt world col (row - 1) then
      next := { next with vy := -6 }
    else if next.bottom - edge row > 20 && solidAt world col row then
      next := { next with vy := -5 }
    else if next.directionY < 0 && !supportAt world col (row + 1) &&
        !supportAt world (col + next.direction) (row + 1) then
      next := { next with vy := -8, vx := next.vx * 1.5 }
    else
      next := { next with ai1 := 0, ai2 := 0 }
    if next.vy == 0 && wasStationary && next.ai3 == 1 then
      next := { next with vy := -5 }
  return (next.jumpNearPlayer player, world)

private def AngryBones.move (bones : AngryBones) (world : World) (water : Water)
    (gravity maxFall : Float32) : AngryBones := Id.run do
  let width := asFloat bones.kind.width
  let vx := if -0.005 < bones.vx.toFloat && bones.vx.toFloat < 0.005 then 0 else bones.vx
  let vy := min maxFall (bones.vy + gravity)
  let wet := water.submerged world (truncate (bones.centerX * 16)) (truncate (bones.centerY * 16))
  let mut dx := vx
  if vx != 0 then
    let right := vx > 0
    let first := if right then tileFloor (bones.x + width) else tileLast bones.x
    let last := if right then tileLast (bones.x + width + vx) else tileFloor (bones.x + vx)
    let count := if right then (last - first + 1).toNat else (first - last + 1).toNat
    let top := tileFloor bones.y
    for offset in [:count] do
      let col := if right then first + offset else first - offset
      if (List.range (tileLast bones.bottom - top + 1).toNat).any (fun row => solidAt world col (top + row)) then
        dx := if right then max 0 (edge col - width - bones.x) else min 0 (edge (col + 1) - bones.x)
        break
  let x := bones.x + if wet && dx == vx then dx * 0.5 else dx
  let mut dy := vy
  if vy != 0 then
    let falling := vy > 0
    let first := if falling then tileFloor bones.bottom else tileLast bones.y
    let last := if falling then tileLast (bones.bottom + vy) else tileFloor (bones.y + vy)
    let count := if falling then (last - first + 1).toNat else (first - last + 1).toNat
    let left := tileFloor x
    for offset in [:count] do
      let row := if falling then first + offset else first - offset
      let landOnPlatform := falling && bones.directionY != 1 && bones.bottom ≤ edge row
      if (List.range (tileLast (x + width) - left + 1).toNat).any (fun col =>
          solidAt world (left + col) row || (landOnPlatform && world.tile ⟨left + col, row⟩ == .platform)) then
        dy := if falling then max 0 (edge row - bones.bottom) else 0.01
        break
  let y := bones.y + if wet && dy == vy then dy * 0.5 else dy
  return { bones with x, y, vx := dx, vy := dy, oldX := bones.x, wet, justHit := false }

private def AngryBones.animate (bones : AngryBones) : AngryBones :=
  if bones.vy != 0 then { bones with frame := 0, frameClock := 0 }
  else
    let bones := { bones with spriteDirection := bones.direction }
    if bones.vx == 0 then { bones with frame := 0, frameClock := 0 }
    else
      let animationRate : Float32 := 2 * bones.vx.abs
      let clock := bones.frameClock + animationRate.toFloat + 1
      let frame := max 2 bones.frame
      if clock > 6 then
        { bones with frame := if frame + 1 ≥ bones.kind.frameCount then 2 else frame + 1, frameClock := 0 }
      else { bones with frame, frameClock := clock }

private def AngryBones.checkActive (bones : AngryBones) (player : Player) : Option AngryBones := do
  let area := fun (w h : Int) =>
    let left := truncate (bones.centerX - asFloat w) * 16
    let top := truncate (bones.centerY - asFloat h) * 16
    (⟨left, top, left + 32 * w, top + 32 * h⟩ : HitBox)
  let target := HitBox.ofNpcTarget player
  if !(area 4032 2520).intersects target then failure
  let visible := area (960 + bones.kind.width) (600 + bones.kind.height)
  let timeLeft := (if visible.intersects target then 750 else bones.timeLeft) - 1
  if timeLeft == 0 then failure
  return { bones with timeLeft }

def AngryBones.advance (bones : AngryBones) (world : World) (water : Water) (player : Player)
    (dead : Bool) (surface : Nat) (daylight := false) : Option AngryBones × World :=
  let size := asFloat world.width / 4200
  let surfacePixels : Float := (asFloat (max 1 surface)).toFloat / 6
  let altitude := ((bones.y / 16 - (60 + 10 * (size * size))).toFloat / surfacePixels).toFloat32
  let gravity := if bones.wet then 0.2 else 0.3 * max 0.25 (min 1 altitude)
  let maxFall := if bones.wet then 7 else 10
  let (next, world) := bones.advanceAI world player dead daylight surface
  let moved := (next.move world water gravity maxFall).animate
  (moved.checkActive player, world)

end Lean4Terria
