import Lean4Terria.DarkCaster
import Lean4Terria.UnderworldShots

namespace Lean4Terria

inductive UnderworldCritterKind where
  | hellButterfly | lavafly | magmaSnail
  deriving BEq, Inhabited, Repr

namespace UnderworldCritterKind

def name : UnderworldCritterKind → String
  | .hellButterfly => "Hell Butterfly"
  | .lavafly => "Lavafly"
  | .magmaSnail => "Magma Snail"

def sprite : UnderworldCritterKind → Nat
  | .hellButterfly => 653
  | .lavafly => 654
  | .magmaSnail => 655

def itemId (kind : UnderworldCritterKind) : Nat := 4845 + 2 * (kind.sprite - 653)
def baitPower (kind : UnderworldCritterKind) : Nat := 15 + 10 * (kind.sprite - 653)
def width (kind : UnderworldCritterKind) : Nat := if kind == .magmaSnail then 14 else 10
def height (kind : UnderworldCritterKind) : Nat := kind.width

end UnderworldCritterKind

def underworldCritterHealth : Nat := 5

/-- Native pixel coordinates and the style 64/65/67 AI registers. Only the
Butterfly animation uses fractional increments in the double frame counter. -/
structure UnderworldCritter where
  id : Nat
  kind : UnderworldCritterKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  health : Nat := underworldCritterHealth
  timeLeft : Nat := 937
  direction : Int := 1
  directionY : Int := 1
  spriteDirection : Int := -1
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  frame : Nat := 0
  frameClock : Float := 0
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  ai2 : Float32 := 0
  ai3 : Float32 := 0
  local0 : Float32 := 0
  local1 : Float32 := 0
  local2 : Float32 := 0
  local3 : Float32 := 0
  deriving Inhabited, Repr

instance : BEq UnderworldCritter where
  beq a b := a.id == b.id && a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.health == b.health && a.timeLeft == b.timeLeft && a.direction == b.direction &&
    a.directionY == b.directionY && a.spriteDirection == b.spriteDirection && a.collideX == b.collideX &&
    a.collideY == b.collideY && a.wet == b.wet && a.frame == b.frame && a.frameClock.toBits == b.frameClock.toBits &&
    a.ai0.toBits == b.ai0.toBits && a.ai1.toBits == b.ai1.toBits && a.ai2.toBits == b.ai2.toBits &&
    a.ai3.toBits == b.ai3.toBits && a.local0.toBits == b.local0.toBits && a.local1.toBits == b.local1.toBits &&
    a.local2.toBits == b.local2.toBits && a.local3.toBits == b.local3.toBits

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixel (units : Int) : Float32 := single units / 16
private def tileCoord (value : Float32) : Int := value.toInt32.toInt.tdiv 16

def UnderworldCritter.centerX (critter : UnderworldCritter) : Float32 := critter.x + single (critter.kind.width / 2 : Nat)
def UnderworldCritter.centerY (critter : UnderworldCritter) : Float32 := critter.y + single (critter.kind.height / 2 : Nat)

def UnderworldCritter.hitBox (critter : UnderworldCritter) : HitBox :=
  CasterAI.npcBox critter.x critter.y critter.kind.width critter.kind.height

def UnderworldCritter.scale (critter : UnderworldCritter) : Float32 :=
  if critter.kind == .magmaSnail || critter.ai3 == 0 then 1 else critter.ai3

def UnderworldCritter.valid (critter : UnderworldCritter) (world : World) : Bool :=
  let direction (value : Int) := value == 1 || value == -1
  let limits := match critter.kind with
    | .hellButterfly => critter.frame ≤ 2 && critter.frameClock < 27 && critter.local0 ≤ 239
    | .lavafly => critter.frame ≤ 3 && critter.frameClock < 7 && critter.local0 ≤ 179
    | .magmaSnail => critter.frame ≤ 5 && critter.frameClock < 59 && critter.ai2 ≤ 2
  critter.id > 0 && CasterAI.validMotion world critter.x critter.y critter.vx critter.vy &&
    critter.health > 0 && critter.health ≤ underworldCritterHealth && critter.timeLeft > 0 && critter.timeLeft ≤ 937 &&
    critter.rotation.isFinite && critter.rotation.abs ≤ 512 && critter.frameClock.isFinite && 0 ≤ critter.frameClock && limits &&
    direction critter.direction && direction critter.directionY && direction critter.spriteDirection &&
    [critter.ai0, critter.ai1, critter.ai2, critter.ai3, critter.local0, critter.local2, critter.local3].all
      (fun value => value.isFinite && value.abs ≤ 1024) &&
    critter.local1.isFinite && -1 ≤ critter.local1 && critter.local1 ≤ single (world.width * 16 + 4096 : Nat)

def UnderworldCritter.atFloor (id : Nat) (kind : UnderworldCritterKind) (column floor : Int) : UnderworldCritter :=
  { id, kind, x := single (column * 16 + 8 - (kind.width / 2 : Nat)),
    y := single (floor * 16 - kind.height) }

private def drawRoll (roll : Nat → Nat) (index : Nat) (low high : Int) : Int :=
  low + (roll index % (high - low).toNat : Nat)

private def speedOf (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32

private def wantedVelocity (dx dy : Int) (speed : Float32) : Float32 × Float32 :=
  let length := (single (dx * dx + dy * dy)).toFloat.sqrt.toFloat32
  if length == 0 then (0, 0) else
    let scale := speed / length
    (single dx * scale, single dy * scale)

private def facePlayer (critter : UnderworldCritter) (player : Player) (dead : Bool) : Int :=
  if dead then critter.direction else if pixel (bodyCenterX player) < critter.centerX then -1 else 1

private def chooseFlight (critter : UnderworldCritter) (player : Player) (dead butterfly : Bool)
    (roll : Nat → Nat) : UnderworldCritter :=
  let ai3 := if critter.ai3 == 0 then single (drawRoll roll 0 75 111) * 0.01 else critter.ai3
  let ai2 := if butterfly && critter.ai2 == 0 then
      let color := roll 10 % 100
      if color == 0 then 6 else if color < 3 then 2 else if color < 9 then 3 else
        if color < 19 then 8 else if color < 34 then 4 else if color < 53 then 7 else if color < 75 then 5 else 1
    else critter.ai2
  let local0 := critter.local0 - 1
  if local0 > 0 then { critter with local0, ai2, ai3 }
  else
    let local0 := single (if butterfly then drawRoll roll 1 90 240 else drawRoll roll 1 60 180)
    let direction := facePlayer critter player dead
    let distance := (critter.centerX - pixel (bodyCenterX player)).abs
    if distance > 700 && critter.local3 == 0 then
      let speed := if distance > 1000 then drawRoll roll 2 150 201
        else if distance > 850 then drawRoll roll 2 100 151 else drawRoll roll 2 50 151
      let dx := direction * drawRoll roll 3 100 251
      let dy := drawRoll roll 4 (-50) 51 - (if critter.y > pixel player.y - 100 then drawRoll roll 5 100 251 else 0)
      let (ai0, ai1) := wantedVelocity dx dy (single speed * 0.01)
      { critter with ai0, ai1, ai2, ai3, local0, direction }
    else
      let speed := if butterfly then drawRoll roll 2 26 301 else drawRoll roll 2 5 151
      let (ai0, ai1) := wantedVelocity (drawRoll roll 3 (-100) 101) (drawRoll roll 4 (-100) 101) (single speed * 0.01)
      { critter with ai0, ai1, ai2, ai3, local0, local3 := 1, direction }

private def flightHeight (critter : UnderworldCritter) (world : World) (water magma : Water)
    (lookahead : Nat) : UnderworldCritter := Id.run do
  let col := tileCoord critter.centerX
  let row := tileCoord critter.centerY
  let solid (pos : TilePos) := (world.tile? pos).any (fun tile => tile.solid || tile == .platform)
  let mut ai1 := critter.ai1
  let mut vy := critter.vy
  if vy > 0 then
    for offset in [:lookahead] do
      let p : TilePos := ⟨col, row + offset⟩
      if solid p || water.level world p > 0 || magma.level world p > 0 then
        ai1 := -ai1
        vy := vy * 0.9
  if vy < 0 && !(List.range 30).any (fun offset => solid ⟨col, row + offset⟩) then
    ai1 := -ai1
    vy := vy * 0.9
  return { critter with ai1, vy }

private def fleeThreats (critter : UnderworldCritter) (threats : Array HitBox) : UnderworldCritter := Id.run do
  if critter.local1 > 0 then return { critter with local1 := critter.local1 - 1 }
  let x := critter.centerX
  let y := critter.centerY
  let mut sumX : Float32 := 0
  let mut sumY : Float32 := 0
  let mut count : Float32 := 0
  for box in threats do
    let left := pixel box.left
    let right := pixel box.right
    let top := pixel box.top
    let bottom := pixel box.bottom
    if speedOf (x - max left (min right x)) (y - max top (min bottom y)) > 100 then continue
    let dx := x - (left + right) / 2
    let dy := y - (top + bottom) / 2
    let length := speedOf dx dy
    count := count + 1
    if length > 0 then
      sumX := sumX + dx / length
      sumY := sumY + dy / length
  if count == 0 then return { critter with local1 := 15 }
  let vx := critter.vx + sumX / count * 2
  let vy := critter.vy + sumY / count * 2
  let speed := speedOf vx vy
  let (vx, vy) := if speed > 16 then (vx / speed * 16, vy / speed * 16) else (vx, vy)
  return { critter with vx, vy, local1 := 15 }

private def advanceFlight (critter : UnderworldCritter) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) (roll : Nat → Nat) (threats : Array HitBox) : UnderworldCritter :=
  let butterfly := critter.kind == .hellButterfly
  let critter := chooseFlight critter player dead butterfly roll
  let critter := if butterfly then critter else
    if critter.local2 > 0 then { critter with local2 := critter.local2 - 1 }
    else if critter.local1 > 0 then { critter with local1 := critter.local1 - 1 }
    else { critter with local1 := single (drawRoll roll 6 30 180), local2 := single (drawRoll roll 7 10 30) }
  let inertia : Float32 := if butterfly then 60 else 80
  let vx := (critter.vx * (inertia - 1) + critter.ai0) / inertia
  let vy := (critter.vy * (inertia - 1) + critter.ai1) / inertia
  let critter := flightHeight { critter with vx, vy } world water magma (if butterfly then 3 else 4)
  let critter := if butterfly then fleeThreats critter threats else critter
  let (vx, ai0) := if critter.collideX then
      (critter.vx * (-0.2), if critter.vx < 0 then critter.ai0.abs else -critter.ai0.abs)
    else (critter.vx, critter.ai0)
  let direction := if vx < 0 then -1 else if vx > 0 then 1 else critter.direction
  { critter with vx, ai0, direction }

private def easeSnailAngle (rotation target : Float32) : Float32 :=
  let rotation := if rotation.toFloat > 6.28 then rotation - 6.28 else rotation
  let rotation := if rotation < 0 then rotation + 6.28 else rotation
  let across := (rotation - target).abs.toFloat > 3.14
  let rotation := if rotation > target then
      if across then rotation + 0.1 else max target (rotation - 0.1)
    else rotation
  if rotation < target then
    if across then rotation - 0.1 else min target (rotation + 0.1)
  else rotation

private def snailPose (critter : UnderworldCritter) : UnderworldCritter :=
  let left := critter.direction < 0
  let up := critter.directionY < 0
  let (angle, direction) : Float32 × Int :=
    if critter.collideX && (left || !critter.collideY) then
      (if left then 1.57 else 4.71, if left == up then -1 else 1)
    else if critter.collideY then
      (if up then 3.14 else 0, if left == up then 1 else -1)
    else (critter.rotation, critter.spriteDirection)
  { critter with rotation := easeSnailAngle critter.rotation angle, spriteDirection := direction }

private def advanceSnail (critter : UnderworldCritter) (world : World) (player : Player) (dead : Bool)
    (roll : Nat → Nat) : UnderworldCritter × Bool := Id.run do
  let ai3 := if critter.ai3 == 0 then single (drawRoll roll 0 80 111) * 0.01 else critter.ai3
  let mut next := { critter with ai3 }
  if next.ai0 == 0 then
    let direction := facePlayer next player dead
    let spriteDirection := if direction > 0 then 1 else next.spriteDirection
    next := { next with ai0 := 1, direction, directionY := 1, spriteDirection }
  let local3 := if next.collideX || next.collideY then 0 else next.local3 + 1
  let release := (next.ai2 == 0 && roll 9 % 7200 == 0) || local3 > 5
  next := { next with local3, ai2 := if release then 2 else next.ai2 }
  let mut skipPose := false
  if next.ai2 > 0 then
    let rotation := if next.vy > 0.6 then next.rotation + single next.direction * 0.1 else 0
    next := { next with
      ai0 := 1, ai1 := 0, directionY := 1, rotation
      spriteDirection := next.direction, vx := 0.6 * single next.direction }
    let foot := (world.tile? ⟨tileCoord (next.centerX - 7 * single next.direction), tileCoord (next.y + 22)⟩).isSome
    if foot && next.collideY then next := { next with ai2 := next.ai2 - 1 }
    if next.collideX && next.vy == 0 then
      next := { next with ai2 := 0, directionY := -1, ai1 := 1 }
      skipPose := true
    if next.vy == 0 then
      if next.local1 != next.x then next := { next with local1 := next.x, local2 := 0 }
      else if next.local2 + 1 > 10 then next := { next with direction := 1, vx := 0.6, local2 := 0 }
      else next := { next with local2 := next.local2 + 1 }
  if next.ai2 > 0 then return (next, true)
  if next.ai1 == 0 then
    if next.collideY then next := { next with ai0 := 2 }
    else if next.ai0 == 2 then next := { next with direction := -next.direction, ai1 := 1, ai0 := 1 }
    if next.collideX then next := { next with directionY := -next.directionY, ai1 := 1 }
  else
    if next.collideX then next := { next with ai0 := 2 }
    else if next.ai0 == 2 then next := { next with directionY := -next.directionY, ai1 := 0, ai0 := 1 }
    if next.collideY then next := { next with direction := -next.direction, ai1 := 0 }
  if !skipPose then next := snailPose next
  return ({ next with vx := 0.6 * single next.direction, vy := 0.6 * single next.directionY }, false)

private def advanceFrame (critter : UnderworldCritter) : UnderworldCritter :=
  let age := critter.frameClock + 1
  match critter.kind with
  | .hellButterfly =>
    let age := age + ((critter.vx.abs + critter.vy.abs) / 2).toFloat
    let frame := if age < 7 then 0 else if age < 14 then 1 else if age < 21 then 2 else 1
    { critter with
      frame, frameClock := if age ≥ 27 then 0 else age
      spriteDirection := critter.direction, rotation := critter.vx * 0.3 }
  | .lavafly =>
    let frame := (if age < 4 then 0 else 1) + (if critter.local2 > 0 then 0 else 2)
    { critter with frame, frameClock := if age ≥ 7 then 0 else age, spriteDirection := critter.direction }
  | .magmaSnail =>
    if critter.vy > 1 then { critter with frame := if age < 6 then 4 else 5, frameClock := if age ≥ 11 then 0 else age }
    else if critter.vx != 0 || critter.vy != 0 then
      let frame := if age < 10 then 0 else if age < 20 then 1 else if age < 30 then 2
        else if age < 40 then 3 else if age < 50 then 2 else 1
      { critter with frame, frameClock := if age ≥ 59 then 0 else age }
    else critter

def UnderworldCritter.advance (critter : UnderworldCritter) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) (roll : Nat → Nat) (threats : Array HitBox := #[]) : Option UnderworldCritter := do
  let (next, falls) := if critter.kind == .magmaSnail then advanceSnail critter world player dead roll
    else (advanceFlight critter world water magma player dead roll threats, false)
  let vx := if -0.005 < next.vx && next.vx < 0.005 then 0 else next.vx
  let wet := water.submerged world (next.centerX * 16).toInt32.toInt (next.centerY * 16).toInt32.toInt ||
    magma.submerged world (next.centerX * 16).toInt32.toInt (next.centerY * 16).toInt32.toInt
  let vy := if falls then min (if next.wet then 7 else 10) (next.vy + if next.wet then 0.2 else 0.3) else next.vy
  let (dx, dy) := clipShotMotion world next.centerX next.centerY vx vy (single (next.kind.width / 2 : Nat)) true
  let x := next.x + (if wet && dx == vx then dx * 0.5 else dx)
  let y := next.y + (if wet && dy == vy then dy * 0.5 else dy)
  let collideX := dx != vx
  let collideY := dy != vy
  let moved := advanceFrame { next with x, y, vx := dx, vy := dy, collideX, collideY, wet }
  let timeLeft ← CasterAI.checkActive moved.centerX moved.centerY moved.kind.width moved.kind.height player moved.timeLeft
  return { moved with timeLeft }

end Lean4Terria
