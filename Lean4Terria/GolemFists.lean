import Lean4Terria.DarkCaster
import Lean4Terria.OriginalCollision

namespace Lean4Terria

inductive GolemFistKind where
  | left | right
  deriving BEq, Repr, Inhabited

def GolemFistKind.id : GolemFistKind → Nat
  | .left => 247
  | .right => 248

def GolemFistKind.width (_ : GolemFistKind) : Nat := 40
def GolemFistKind.height (_ : GolemFistKind) : Nat := 30
def GolemFistKind.health (_ : GolemFistKind) : Nat := 10000
def GolemFistKind.damage (_ : GolemFistKind) : Nat := 59
def GolemFistKind.defense (_ : GolemFistKind) : Nat := 28

def GolemFistKind.mountX : GolemFistKind → Int
  | .left => -84
  | .right => 78

/-- Classic NPCs 247 and 248. Position is the top-left of the native hitbox
in pixels. The two integral AI fields are the phase and its clock. -/
structure GolemFist where
  kind : GolemFistKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  health : Nat := 10000
  phase : Nat := 0
  timer : Nat := 0
  alpha : Nat := 255
  noTileCollide : Bool := false
  collideX : Bool := false
  collideY : Bool := false
  deriving Repr, Inhabited

instance : BEq GolemFist where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.health == b.health && a.phase == b.phase && a.timer == b.timer && a.alpha == b.alpha &&
    a.noTileCollide == b.noTileCollide && a.collideX == b.collideX && a.collideY == b.collideY

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (value : Int) : Float32 := single value / 16
private def length (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32
private def angle (x y : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32

def GolemFist.spawn (kind : GolemFistKind) (bodyCenterX bodyCenterY : Float32) : GolemFist :=
  { kind
    x := single (bodyCenterX.toInt32.toInt + kind.mountX - 20)
    y := single (bodyCenterY.toInt32.toInt - 9 - 30) }

def GolemFist.centerX (fist : GolemFist) : Float32 := fist.x + 20
def GolemFist.centerY (fist : GolemFist) : Float32 := fist.y + 15

def GolemFist.hitBox (fist : GolemFist) : HitBox :=
  CasterAI.npcBox fist.x fist.y fist.kind.width fist.kind.height

def GolemFist.contact (fist : GolemFist) (player : Player) : Option Int :=
  CasterAI.contact fist.hitBox fist.centerX player

def GolemFist.valid (fist : GolemFist) (world : World) : Bool :=
  CasterAI.validMotion world fist.x fist.y 0 0 &&
    fist.vx.isFinite && fist.vy.isFinite && fist.vx.abs ≤ 48 && fist.vy.abs ≤ 48 &&
    fist.rotation.isFinite && fist.rotation.abs ≤ 3.15 && 0 < fist.health &&
    fist.health ≤ fist.kind.health && fist.phase ≤ 3 && fist.timer ≤ 720 && fist.alpha ≤ 255 &&
    (fist.phase != 1 || fist.timer < 30)

private def GolemFist.halfLife (fist : GolemFist) : Bool := fist.health < fist.kind.health / 2
private def GolemFist.quarterLife (fist : GolemFist) : Bool := fist.health < fist.kind.health / 4

private def speedFactor (enraged : Bool) : Float32 := if enraged then 1.25 else 1

private def GolemFist.retract (fist : GolemFist) (dx dy distance targetX : Float32)
    (bodyHurt enraged : Bool) : GolemFist :=
  let speed : Float32 := 14 + (if fist.halfLife then 3 else 0) +
    (if fist.quarterLife then 3 else 0) + (if bodyHurt then 8 else 0)
  let speed := min 32 (speed * speedFactor enraged)
  if distance < 12 + speed then
    let beat := if enraged then 2 else 1
    let timer := fist.timer + beat + (if fist.halfLife then beat else 0) +
      (if fist.quarterLife then beat else 0) + (if bodyHurt then 10 * beat else 0)
    let facingTarget := if fist.kind == .left then fist.centerX + 100 > targetX
      else fist.centerX - 100 < targetX
    { fist with
      vx := dx, vy := dy, rotation := 0, noTileCollide := true
      phase := if timer ≥ 60 && facingTarget then 1 else 0
      timer := if timer ≥ 60 then 0 else timer }
  else
    let factor := speed / distance
    let vx := dx * factor
    let vy := dy * factor
    let rotation := if fist.kind == .left then angle vx vy else angle (0 - vx) (0 - vy)
    { fist with vx, vy, rotation, noTileCollide := true }

private def GolemFist.punchRotation (fist : GolemFist) (vx vy : Float32) : Float32 :=
  if fist.kind == .left then angle (0 - vx) (0 - vy) else angle vx vy

private def GolemFist.windUp (fist : GolemFist) (anchorX anchorY targetX targetY : Float32)
    (bodyHurt enraged : Bool) : GolemFist :=
  let fist := { fist with
    x := anchorX - 20, y := anchorY - 15, vx := 0, vy := 0, rotation := 0, timer := fist.timer + 1 }
  if fist.timer < 30 then fist
  else
    let dx := targetX - fist.centerX
    let dy := targetY - fist.centerY
    let distance := length dx dy
    let speed : Float32 := 12 + (if fist.halfLife then 4 else 0) +
      (if fist.quarterLife then 4 else 0) + (if bodyHurt then 10 else 0)
    let speed := min 48 (speed * speedFactor enraged)
    let ratio := if distance == 0 then 0 else speed / distance
    let vx := dx * ratio
    let vy := dy * ratio
    { fist with
      vx, vy, rotation := fist.punchRotation vx vy
      phase := if distance == 0 then 0 else 2
      timer := 0, noTileCollide := true, collideX := false, collideY := false }

private def accelerated (velocity desired : Float32) : Float32 :=
  if velocity < desired then
    let faster := velocity + 0.4
    if faster < 0 && desired > 0 then faster + 0.8 else faster
  else if velocity > desired then
    let slower := velocity - 0.4
    if slower > 0 && desired < 0 then slower - 0.8 else slower
  else velocity

/-- Classic single-player AI. The Boolean is the Item14 launch cue on the
first subsequent AI tick of an extending punch. -/
def GolemFist.advanceAI (fist : GolemFist) (bodyX bodyY bodyVX bodyVY : Float32)
    (bodyHurt : Bool) (targetX targetY : Float32) (enraged : Bool) : GolemFist × Bool :=
  let fist := if fist.alpha == 0 then fist else { fist with alpha := fist.alpha - 10, timer := 0 }
  let anchorX := bodyX + bodyVX + single fist.kind.mountX
  let anchorY := bodyY + bodyVY - 9
  let dx := anchorX - fist.centerX
  let dy := anchorY - fist.centerY
  let distance := length dx dy
  match fist.phase with
  | 0 => (fist.retract dx dy distance targetX bodyHurt enraged, false)
  | 1 => (fist.windUp anchorX anchorY targetX targetY bodyHurt enraged, false)
  | 2 =>
    let passedTarget :=
      if fist.vx.abs > fist.vy.abs then
        (fist.vx > 0 && fist.centerX > targetX) || (fist.vx < 0 && fist.centerX < targetX)
      else (fist.vy > 0 && fist.centerY > targetY) || (fist.vy < 0 && fist.centerY < targetY)
    let returning := distance > 700 || fist.collideX || fist.collideY
    ({ fist with
      phase := if returning then 0 else 2
      timer := fist.timer + 1
      noTileCollide := returning || (fist.noTileCollide && !passedTarget) }, fist.timer == 0)
  | 3 =>
    let dx := targetX - fist.centerX
    let dy := targetY - fist.centerY
    let distance := length dx dy
    let ratio := if distance == 0 then 0 else 12 / distance
    let vx := accelerated fist.vx (dx * ratio)
    let vy := accelerated fist.vy (dy * ratio)
    ({ fist with vx, vy, rotation := fist.punchRotation vx vy, noTileCollide := true }, false)
  | _ => (fist, false)

open OriginalCollision (Grid Vector)

/-- Dry NPC locomotion. The native fist passes through platforms, skips
the post-collision slope correction, and remembers blocked velocity for its next AI frame. -/
def GolemFist.moveOnGrid (fist : GolemFist) (grid : Grid) : GolemFist :=
  let vx := if -0.005 < fist.vx.toFloat && fist.vx.toFloat < 0.005 then 0 else fist.vx
  if fist.noTileCollide then { fist with x := fist.x + vx, y := fist.y + fist.vy, vx }
  else
    let pos : Vector := ⟨fist.x, fist.y⟩
    let velocity := OriginalCollision.walkDownSlope grid pos ⟨vx, fist.vy⟩ 40 30 0.3
    let collision := OriginalCollision.tileCollision grid pos velocity 40 30
      { fallThrough := true, fall2 := true }
    let vx := collision.displacement.x
    let vy := if collision.hitAbove then 0.01 else collision.displacement.y
    { fist with
      x := pos.x + vx, y := pos.y + vy, vx, vy
      collideX := vx != velocity.x, collideY := vy != velocity.y }

private def compactGrid (world : World) : Grid :=
  { width := world.width, height := world.height
    tileAt := fun x y =>
      let tile := world.tile ⟨x, y⟩
      if tile == .platform then { id := some 19, frame := some (0, 0) }
      else if tile.solid then { id := some 1 } else {}
    rules := fun id =>
      if id == 19 then { solid := true, solidTop := true, platform := true }
      else { solid := id == 1 } }

def GolemFist.advance (fist : GolemFist) (world : World) (bodyX bodyY bodyVX bodyVY : Float32)
    (bodyHurt : Bool) (player : Player) (enraged : Bool) : GolemFist × Bool :=
  let targetX := pixels (bodyCenterX player)
  let targetY := pixels (bodyCenterY player)
  let (next, launchCue) := fist.advanceAI bodyX bodyY bodyVX bodyVY bodyHurt targetX targetY enraged
  (next.moveOnGrid (compactGrid world), launchCue)

private def GolemFistKind.chainOffset : GolemFistKind → Float32
  | .left => -70
  | .right => 66

/-- Center and rotation of each native Chain21 sprite, from the fist back
to the visual shoulder anchor. -/
def GolemFist.chainLinks (fist : GolemFist) (bodyX bodyY : Float32) :
    Array (Float32 × Float32 × Float32) := Id.run do
  let mut x := fist.centerX
  let mut y := fist.centerY
  let mut dx := bodyX - x + fist.kind.chainOffset
  let mut dy := bodyY - y - 7
  let tilt := angle dx dy - 1.57
  let mut links := #[]
  repeat
    let distance := length dx dy
    if !distance.isFinite || distance < 16 then break
    let factor := 16 / distance
    x := x + dx * factor
    y := y + dy * factor
    links := links.push (x, y, tilt)
    dx := bodyX - x + fist.kind.chainOffset
    dy := bodyY - y - 7
  return links

end Lean4Terria
