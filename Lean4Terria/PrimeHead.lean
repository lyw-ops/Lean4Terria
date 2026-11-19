import Lean4Terria.DarkCaster

namespace Lean4Terria

def primeHeadWidth : Nat := 80
def primeHeadHeight : Nat := 102
def primeHeadHealth : Nat := 28000
def primeHoverTicks : Nat := 600
def primeSpinTicks : Nat := 400

inductive PrimePhase where
  | hover | spin | guardian | retreat
  deriving BEq, Repr, Inhabited

/-- Classic NPC 127. Positions and velocities are in native pixels.
The active damage and defense record the branch that ran this update. -/
structure PrimeHead where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  phase : PrimePhase := .hover
  timer : Nat := 0
  health : Nat := primeHeadHealth
  timeLeft : Nat := 750
  direction : Int := 1
  defense : Nat := 24
  contactDamage : Nat := 47
  initialized : Bool := false
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq PrimeHead where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.phase == b.phase && a.timer == b.timer &&
    a.health == b.health && a.timeLeft == b.timeLeft && a.direction == b.direction &&
    a.defense == b.defense && a.contactDamage == b.contactDamage &&
    a.initialized == b.initialized && a.frame == b.frame && a.frameClock == b.frameClock

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (units : Int) : Float32 := single units / 16

structure PrimeTarget where
  x : Float32
  y : Float32
  width : Nat
  height : Nat
  deriving Repr, Inhabited

def PrimeTarget.ofPlayer (player : Player) : PrimeTarget :=
  ⟨pixels player.x, pixels player.y, playerWidth.toNat / 16, playerHeight.toNat / 16⟩

def PrimeTarget.centerX (target : PrimeTarget) : Float32 := target.x + single target.width / 2
def PrimeTarget.centerY (target : PrimeTarget) : Float32 := target.y + single target.height / 2

def PrimeHead.centerX (head : PrimeHead) : Float32 := head.x + 40
def PrimeHead.centerY (head : PrimeHead) : Float32 := head.y + 51

def PrimeHead.atCenter (x y : Float32) : PrimeHead :=
  { x := x - 40, y := y - 51 }

def PrimeHead.hitBox (head : PrimeHead) : HitBox :=
  CasterAI.npcBox head.x head.y primeHeadWidth primeHeadHeight

def PrimeHead.contact (head : PrimeHead) (player : Player) : Option Int :=
  CasterAI.contact head.hitBox head.centerX player

def PrimeHead.valid (head : PrimeHead) (world : World) : Bool :=
  head.x.isFinite && head.y.isFinite &&
    -32768 ≤ head.x && head.x ≤ single (world.width * 16 + 32768 : Nat) &&
    -32768 ≤ head.y && head.y ≤ single (world.height * 16 + 32768 : Nat) &&
    head.vx.isFinite && head.vy.isFinite && head.rotation.isFinite &&
    head.vx.abs ≤ 33 && head.vy.abs ≤ 100 &&
    head.health > 0 && head.health ≤ primeHeadHealth &&
    head.timeLeft > 0 && head.timeLeft ≤ 15000 &&
    (head.direction == -1 || head.direction == 1) &&
    head.frame ≤ 2 && head.frameClock < 12 &&
    head.timer < (if head.phase == .spin then primeSpinTicks else primeHoverTicks) &&
    [(24, 47), (48, 94), (9999, 9999)].contains (head.defense, head.contactDamage)

private def PrimeHead.face (head : PrimeHead) (target : PrimeTarget) : Int :=
  let targetCenter := single (target.x.toInt32.toInt + (target.width / 2 : Nat))
  if targetCenter < head.centerX then -1 else 1

private def hoverVelocity (speed position upper lower limit : Float32) : Float32 :=
  if position > upper then
    min limit ((if speed > 0 then speed * 0.98 else speed) - 0.1)
  else if position < lower then
    max (-limit) ((if speed < 0 then speed * 0.98 else speed) + 0.1)
  else speed

private def PrimeHead.hover (head : PrimeHead) (target : PrimeTarget) : PrimeHead :=
  let vx := hoverVelocity head.vx head.centerX (target.centerX + 100) (target.centerX - 100) 8
  let vy := hoverVelocity head.vy head.y (target.y - 200) (target.y - 500) 2
  let moved := { head with vx, vy, rotation := head.vx / 15 }
  let timer := head.timer + 1
  if timer ≥ primeHoverTicks then
    { moved with phase := .spin, timer := 0, direction := head.face target }
  else { moved with timer }

private def PrimeHead.chase (head : PrimeHead) (target : PrimeTarget) (guardian : Bool) :
    PrimeHead :=
  let dx := target.centerX - head.centerX
  let dy := target.centerY - head.centerY
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let length := if length > 0 then length else 1
  let speed : Float32 := if guardian then min 32 (max 8 (10 + length / 100)) else 2
  let scale := speed / length
  { head with vx := dx * scale, vy := dy * scale,
              rotation := head.rotation + single head.direction * 0.3 }

/-- The ordinary single-player branch of NPC.AI style 32 in desktop 1.4.5.8.
The caller creates the four arms from the pre-update position on the first frame. -/
def PrimeHead.advanceAI (head : PrimeHead) (target : PrimeTarget) (daylight dead : Bool) : PrimeHead :=
  let far := (head.x - target.x).abs > 6000 || (head.y - target.y).abs > 6000
  let direction := if !dead && (!head.initialized || far) then head.face target else head.direction
  let phase := if dead || far then .retreat
    else if daylight && head.phase != .retreat then .guardian else head.phase
  let head := { head with direction, phase, defense := 24, contactDamage := 47, initialized := true }
  match phase with
  | .hover => head.hover target
  | .spin =>
    let moved := { head.chase target false with defense := 48, contactDamage := 94 }
    let timer := head.timer + 1
    if timer ≥ primeSpinTicks then { moved with phase := .hover, timer := 0 }
    else { moved with timer }
  | .guardian => { head.chase target true with defense := 9999, contactDamage := 9999 }
  | .retreat =>
    let vy : Float32 := head.vy + 0.1
    { head with vx := head.vx * 0.95, vy := if vy < 0 then vy * 0.95 else vy,
                timeLeft := min 500 head.timeLeft }

private def PrimeHead.checkActive (head : PrimeHead) (player : Player) : Nat :=
  let halfWidth := 960 + primeHeadWidth
  let halfHeight := 600 + primeHeadHeight
  let left := (head.centerX.toFloat - halfWidth.toFloat).toInt32.toInt * 16
  let top := (head.centerY.toFloat - halfHeight.toFloat).toInt32.toInt * 16
  let area : HitBox := ⟨left, top, left + 32 * halfWidth, top + 32 * halfHeight⟩
  (if area.intersects (HitBox.ofNpcTarget player) then 750 else head.timeLeft) - 1

private def PrimeHead.advanceFrame (head : PrimeHead) : PrimeHead :=
  if head.phase != .hover then
    { head with frame := 2, frameClock := 0 }
  else if head.frameClock == 11 then
    { head with frame := if head.frame == 0 then 1 else 0, frameClock := 0 }
  else
    { head with frameClock := head.frameClock + 1 }

/-- Complete the head's frame, including the native horizontal dead zone,
no-tile-collision movement, animation, and activity countdown. -/
def PrimeHead.advance (head : PrimeHead) (world : World) (player : Player) (daylight dead : Bool) :
    Option PrimeHead := do
  if head.health == 0 then failure
  let next := head.advanceAI (.ofPlayer player) daylight dead
  let vx := if next.vx.toFloat.abs < 0.005 then 0 else next.vx
  let moved := { next with x := next.x + vx, y := next.y + next.vy, vx }.advanceFrame
  let moved := { moved with timeLeft := moved.checkActive player }
  if moved.valid world then some moved else none

/-- The compact world's spawn and safe rectangles retain the native ground
search. A spot above and to the side is available when the compact scan
cannot find suitable ground, including a player at the edge of the map. -/
def PrimeHead.spawn (world : World) (player : Player) (roll : Nat → Nat) : PrimeHead := Id.run do
  let center := player.centerTile
  let left := max 4 (center.x - 50)
  let right := min ((world.width : Int) - 5) (center.x + 50)
  let top := max 9 (center.y - 31)
  let bottom := min ((world.height : Int) - 3) (center.y + 31)
  for attempt in [:128] do
    let x := left + roll (attempt * 2) % ((right - left).toNat + 1)
    let y := top + roll (attempt * 2 + 1) % ((bottom - top).toNat + 1)
    if (x - center.x).natAbs ≤ 24 && (y - center.y).natAbs ≤ 15 then continue
    if (world.tile ⟨x, y⟩).solid then continue
    for floor in [y.toNat:bottom.toNat + 1] do
      if (world.tile ⟨x, floor⟩).solid then
        if (x - center.x).natAbs > 24 || ((floor : Int) - center.y).natAbs > 15 then
          return { PrimeHead.atCenter (single (x * 16 + 8)) (single (floor * 16 : Nat) - 51) with
            timeLeft := 15000 }
        break
  let side : Int := if center.x < world.width / 2 then 32 else -32
  let x := max 4 (min ((world.width : Int) - 5) (center.x + side))
  let y := max 9 (center.y - 19)
  return { PrimeHead.atCenter (single (x * 16 + 8)) (single (y * 16 + 8)) with
    timeLeft := 15000 }

end Lean4Terria
