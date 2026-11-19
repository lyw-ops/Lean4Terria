import Lean4Terria.WallOfFlesh
import Lean4Terria.BoneSerpent
import Lean4Terria.UnderworldShots

namespace Lean4Terria

def maxWallHungry : Nat := 22
def maxWallLeeches : Nat := 20
def maxWallLeechSegments : Nat := 7

structure WallHungry where
  id : Nat
  fraction : Float32
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  health : Nat := 240
  hitPause : Nat := 0
  clock : Nat := 0
  attached : Bool := true
  deriving Repr, Inhabited

structure WallLeech where
  id : Nat
  segments : Array BoneSerpentSegment
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := 60
  timeLeft : Nat := 937
  deriving Repr, Inhabited

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixel (units : Int) : Float32 := single units / 16
private def whole (v : Float32) : Int := v.toInt32.toInt
private def tilePixel (v : Float32) : Float32 := single (whole (v / 16) * 16)
private def length (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32
private def angle (y x : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32

private def minionBox (x y : Float32) (width height : Int) : HitBox :=
  let x := whole x * 16
  let y := whole y * 16
  ⟨x, y, x + width * 16, y + height * 16⟩

private def validPosition (x y : Float32) (world : World) : Bool :=
  x.isFinite && y.isFinite && -4096 ≤ x && x < single (world.width * 16 : Nat) + 4096 &&
    -4096 ≤ y && y < single (world.height * 16 : Nat) + 4096

private def velocityValid (vx vy : Float32) : Bool := vx.isFinite && vy.isFinite && vx.abs ≤ 32 && vy.abs ≤ 32

def WallHungry.hitBox (hungry : WallHungry) : HitBox :=
  minionBox hungry.x hungry.y 30 (if hungry.attached then 30 else 32)

def WallHungry.position (hungry : WallHungry) : TilePos :=
  ⟨whole ((hungry.x + 15) / 16), whole ((hungry.y + 15) / 16)⟩

def WallHungry.damage (hungry : WallHungry) (wall : Option WallOfFlesh) : Nat :=
  let life := (wall.map (·.health)).getD 8000
  if !hungry.attached then 30 else if life < 4000 then 60 else if life < 6000 then 45 else 30

def WallHungry.defense (hungry : WallHungry) (wall : Option WallOfFlesh) : Nat :=
  if !hungry.attached then 6
  else
    let life := (wall.map (·.health)).getD 8000
    if life < 4000 then 30 else if life < 6000 then 20 else 10

def WallHungry.valid (hungry : WallHungry) (world : World) : Bool :=
  0 < hungry.id && 0 < hungry.health && hungry.health ≤ (if hungry.attached then 240 else 80) &&
    hungry.fraction.isFinite && -0.051 ≤ hungry.fraction && hungry.fraction ≤ 0.951 &&
    hungry.clock ≤ 200 && hungry.hitPause ≤ 10 && hungry.rotation.isFinite && hungry.rotation.abs ≤ 6.3 &&
    validPosition hungry.x hungry.y world && velocityValid hungry.vx hungry.vy

def WallHungry.at (id index : Nat) (x y : Float32) : WallHungry :=
  { id, fraction := single index * 0.1 - 0.05, x := x - 15, y := y - 30 }

def WallHungry.detach (hungry : WallHungry) (id : Nat) : WallHungry :=
  { hungry with id, attached := false, health := 80, vx := 0, vy := 0, hitPause := 0, clock := 0 }

private def hungryAxis (position velocity anchor offset : Float32) : Float32 :=
  if position < anchor + offset then
    let speed := velocity + 0.1
    if speed < 0 && offset > 0 then speed + 0.25 else speed
  else if position > anchor + offset then
    let speed := velocity - 0.1
    if speed > 0 && offset < 0 then speed - 0.25 else speed
  else velocity

/-- Classic style 29 reads the wall's draw bounds and uses the player's
top-left adjusted for the Hungry's size, before clipping its tether. -/
private def WallHungry.advanceAttached (hungry : WallHungry) (wall : WallOfFlesh) (player : Player) : WallHungry :=
  let (ax, ay) := wall.attachment hungry.fraction
  let nextClock := hungry.clock + 1
  let range : Float32 := if wall.health < 4000 then 700 else if wall.health < 6000 then 500 else 300
  let range := if nextClock > 100 then range * 1.3 else range
  let dx := pixel (bodyCenterX player) - 15 - ax
  let dy := pixel (bodyCenterY player) - 15 - ay
  let distance := length dx dy
  let scale := if distance > range then range / distance else 1
  let dx := dx * scale
  let dy := dy * scale
  let vx := if hungry.hitPause > 0 then hungry.vx else max (-4) (min 4 (hungryAxis hungry.x hungry.vx ax dx))
  let vy := if hungry.hitPause > 0 then hungry.vy else max (-4) (min 4 (hungryAxis hungry.y hungry.vy ay dy))
  let rotation := if dx == 0 then hungry.rotation else angle dy dx + (if dx < 0 then 3.14 else 0)
  { hungry with
    x := hungry.x + vx, y := hungry.y + vy, vx, vy, rotation
    hitPause := hungry.hitPause - 1, clock := if nextClock > 200 then 0 else nextClock }

private def flyingAxis (v target cap accel reverse outer : Float32) : Float32 :=
  let direction : Float32 := if target < 0 then -1 else 1
  let along := v * direction
  if along ≥ cap then v
  else
    let moved := along + accel
    let pushed := moved + (if moved < -cap then outer else if moved < 0 then reverse else 0)
    direction * min pushed cap

/-- The type-116 branch of style 2 uses different caps and reversal
acceleration for each axis, then the ordinary NPC tile collision. -/
private def WallHungry.advanceFree (hungry : WallHungry) (world : World) (player : Player)
    (dead : Bool) : Option WallHungry := do
  let dx := pixel (bodyCenterX player) - (hungry.x + 15)
  let dy := pixel (bodyCenterY player) - (hungry.y + 16)
  if dx.abs > 2200 || dy.abs > 1700 then failure
  let vx := flyingAxis hungry.vx (if dead then hungry.vx else dx) 6 0.1 0.2 0.1
  let vy := flyingAxis hungry.vy (if dead then -1 else dy) 2.5 0.04 0.15 0.05
  let (mx, my) := clipShotMotion world (hungry.x + 15) (hungry.y + 16) vx vy 15 false 16
  return { hungry with
    x := hungry.x + mx, y := hungry.y + my
    vx := if mx != vx then -vx * 0.5 else vx
    vy := if my != vy then -vy * 0.5 else vy
    rotation := if dx < 0 then -vx * 0.1 else vx * 0.1 }

def WallHungry.advance (hungry : WallHungry) (world : World) (wall : Option WallOfFlesh)
    (player : Player) (dead : Bool) : Option WallHungry :=
  if hungry.attached then wall.map (fun body => hungry.advanceAttached body player)
  else hungry.advanceFree world player dead

def WallLeech.targets (leech : WallLeech) : Array (Nat × HitBox) :=
  leech.segments.mapIdx (fun i segment => (i, minionBox segment.x segment.y 14 14))

def WallLeech.damage (leech : WallLeech) (part : Nat) : Nat :=
  if part == 0 then 26 else if part + 1 == leech.segments.size then 18 else 22

def WallLeech.defense (leech : WallLeech) (part : Nat) : Nat :=
  if part == 0 then 2 else if part + 1 == leech.segments.size then 10 else 6

def WallLeech.valid (leech : WallLeech) (world : World) : Bool :=
  0 < leech.id && 0 < leech.health && leech.health ≤ 60 && 0 < leech.timeLeft && leech.timeLeft ≤ 937 &&
    5 ≤ leech.segments.size && leech.segments.size ≤ maxWallLeechSegments &&
    velocityValid leech.vx leech.vy && leech.segments.all (fun part =>
      validPosition part.x part.y world && part.rotation.isFinite && part.rotation.abs ≤ 6.3)

def WallLeech.at (id : Nat) (x y vx : Float32) (roll : Nat) : WallLeech :=
  let head : BoneSerpentSegment := { x := x - 7, y := y - 14 }
  { id, segments := Array.replicate (5 + roll % 3) head, vx }

/-- Style 6 shares linked-segment geometry with the Bone Serpent.
NPC type 117 uses a 14-pixel link and its own health-based acceleration. -/
def WallLeech.advance (leech : WallLeech) (world : World) (water magma : Water)
    (wall : Option WallOfFlesh) (player : Player) (dead : Bool) : Option WallLeech := do
  let head ← leech.segments[0]?
  let life := (wall.map (·.health)).getD 8000
  let speed : Float32 := 8 + (if life < 4000 then 1 else 0) +
    (if life < 2000 then 1 else 0) + (if life < 800 then 2 else 0)
  let acceleration : Float32 := 0.07 + (if life < 4000 then 0.1 else 0) +
    (if life < 2000 then 0.1 else 0) + (if life < 800 then 0.1 else 0)
  let px := pixel (bodyCenterX player)
  let py := pixel (bodyCenterY player)
  let far := (head.x - px).abs > 1000 || (head.y - py).abs > 1000
  let inTerrain := head.inTerrain world water magma 14 || far
  if dead && inTerrain && head.y > single (world.height * 12 : Nat) then failure
  let timeLeft := if dead || far then leech.timeLeft - 1 else 937
  if timeLeft == 0 then failure
  let dx := tilePixel px - tilePixel (head.x + 7)
  let dy := tilePixel py - tilePixel (head.y + 7)
  let distance := length dx dy
  let (vx, vy) := if inTerrain && distance > 0 then
      let scale := speed / distance
      wormSteer speed acceleration leech.vx leech.vy dx dy
        (if dead then 0 else dx * scale) (if dead then speed else dy * scale)
    else wormFall speed acceleration leech.vx leech.vy dx
  let head := { x := head.x + vx, y := head.y + vy, rotation := angle vy vx + 1.5707963705062866 }
  let segments := (List.range (leech.segments.size - 1)).foldl (fun parts index =>
    parts.push (leech.segments[index + 1]!.follow parts[index]! 14)) #[head]
  return { leech with segments, vx, vy, timeLeft }

end Lean4Terria
