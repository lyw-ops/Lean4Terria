import Lean4Terria.DarkCaster

namespace Lean4Terria

def maxWraiths : Nat := 16
def wraithWidth : Nat := 24
def wraithHeight : Nat := 44
def wraithHealth : Nat := 160
def wraithDamage : Nat := 65
def wraithDefense : Nat := 16
def wraithCoins : Nat := 500
def wraithFrameCount : Nat := 4

/-- Native NPC 82 uses style 22 and retains its last position for stuck
detection. Coordinates and velocities use the original pixel scale. -/
structure Wraith where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  anchorX : Float32 := 0
  anchorY : Float32 := 0
  health : Nat := wraithHealth
  timeLeft : Nat := 937
  direction : Int := 0
  directionY : Int := 1
  spriteDirection : Int := -1
  stuckClock : Int := 0
  justHit : Bool := false
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq Wraith where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.anchorX.toBits == b.anchorX.toBits && a.anchorY.toBits == b.anchorY.toBits &&
    a.health == b.health && a.timeLeft == b.timeLeft && a.direction == b.direction &&
    a.directionY == b.directionY && a.spriteDirection == b.spriteDirection &&
    a.stuckClock == b.stuckClock && a.justHit == b.justHit && a.frame == b.frame && a.frameClock == b.frameClock

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (value : Int) : Float32 := single value / 16
private def tile (value : Float32) : Int := (value / 16).toInt32.toInt

def Wraith.centerX (wraith : Wraith) : Float32 := wraith.x + 12
def Wraith.centerY (wraith : Wraith) : Float32 := wraith.y + 22

def Wraith.atFloor (id : Nat) (column floor : Int) : Wraith :=
  { id, x := single (column * 16 - 4), y := single (floor * 16 - 44) }

def Wraith.hitBox (wraith : Wraith) : HitBox :=
  CasterAI.npcBox wraith.x wraith.y wraithWidth wraithHeight

def Wraith.contact (wraith : Wraith) (player : Player) : Option Int :=
  CasterAI.contact wraith.hitBox wraith.centerX player

def Wraith.valid (wraith : Wraith) (world : World) : Bool :=
  wraith.id > 0 && 0 < wraith.health && wraith.health ≤ wraithHealth &&
    CasterAI.validMotion world wraith.x wraith.y 0 0 &&
    CasterAI.validMotion world wraith.anchorX wraith.anchorY 0 0 &&
    wraith.vx.isFinite && wraith.vx.abs ≤ 22.4 && wraith.vy.isFinite && wraith.vy.abs ≤ 11.2 &&
    wraith.rotation.isFinite && wraith.rotation.abs ≤ 2.24 &&
    0 < wraith.timeLeft && wraith.timeLeft ≤ 937 &&
    wraith.direction.natAbs ≤ 1 && wraith.directionY.natAbs ≤ 1 && wraith.spriteDirection.natAbs == 1 &&
    -200 ≤ wraith.stuckClock && wraith.stuckClock < 60 &&
    wraith.frame < wraithFrameCount && wraith.frameClock < 6

def Wraith.knockback (wraith : Wraith) (damage : Nat) (direction : Int)
    (force : Float32) (crit := false) : Wraith :=
  let wraith := { wraith with justHit := true }
  if force ≤ 0 then wraith else
  let resist : Float32 := 0.7
  let reduce := fun (value cap coefficient : Float32) =>
    if value > cap then cap + (value - cap) * coefficient else value
  let impulse := reduce (reduce (reduce (reduce (force * resist) 8 0.9) 10 0.8) 12 0.7) 14 0.6
  let impulse := min impulse 16
  let impulse := if crit then impulse * 1.4 else impulse
  let upward := -impulse * 0.5
  let side := single direction
  if damage * 10 ≤ wraithHealth then
    { wraith with vx := impulse * side * resist, vy := upward * resist }
  else
    let vx := if direction != 0 && side * wraith.vx < impulse then
        let vx := if side * wraith.vx < 0 then wraith.vx + side * impulse else wraith.vx
        side * min impulse (side * (vx + side * impulse))
      else wraith.vx
    let vy := if wraith.vy > upward then max upward (wraith.vy + upward) else wraith.vy
    { wraith with vx, vy }

private def Wraith.track (wraith : Wraith) (player : Player) (dead : Bool) (height : Nat) : Wraith :=
  if dead then wraith else
    let x := single (player.x.tdiv 16 + playerWidth / 32)
    let y := single (player.y.tdiv 16 + playerHeight / 32)
    { wraith with direction := if x < wraith.centerX then -1 else 1
                  directionY := if y < wraith.y + single (height / 2 : Nat) then -1 else 1 }

private def Wraith.checkStuck (wraith : Wraith) (player : Player) (dead : Bool) (height : Nat) : Wraith :=
  let clock := if wraith.justHit then 0 else wraith.stuckClock
  if clock < 0 then
    let x := pixels player.x + single (playerWidth / 16) * 0.5
    { wraith with stuckClock := clock + 1, direction := if x > wraith.centerX then -1 else 1 }
  else
    let nearX := wraith.anchorX - 16 < wraith.x && wraith.x < wraith.anchorX + 16
    let nearY := wraith.anchorY - 40 < wraith.y && wraith.y < wraith.anchorY + 40
    let reversing := (wraith.vx < 0 && wraith.direction > 0) || (wraith.vx > 0 && wraith.direction < 0)
    let wraith :=
      if !(nearY && (nearX || reversing)) then
        { wraith with anchorX := wraith.x, anchorY := wraith.y, stuckClock := 0 }
      else if clock + 1 ≥ 60 then
        { wraith with stuckClock := -200, direction := -wraith.direction, vx := -wraith.vx }
      else { wraith with stuckClock := clock + 1 }
    wraith.track player dead height

private def accelerate (velocity : Float32) (direction : Int) (cap step reverse brake : Float32) : Float32 :=
  let sign := single direction
  let aligned := velocity * sign
  if direction == 0 || aligned ≥ cap then velocity else
    let moved := aligned + step
    let moved := if moved < -cap then moved + reverse else if moved < 0 then moved - brake else moved
    sign * min cap moved

/-- The Wraith falls when its three-tile forward column has neither ground
nor liquid and it is below the player's top. It has no tile collision. -/
def Wraith.advanceAI (wraith : Wraith) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) (height := wraithHeight) : Wraith := Id.run do
  let wraith := wraith.checkStuck player dead height
  let bottom := wraith.y + single height
  let column := tile wraith.centerX + wraith.direction * 2
  let row := tile bottom
  let mut airborne := true
  if bottom > pixels player.y then
    for offset in [:3] do
      let pos : TilePos := ⟨column, row + offset⟩
      let ground := world.tile pos
      if ground.solid || ground == .platform || water.level world pos > 0 || magma.level world pos > 0 then
        airborne := false
        break
  let vy := if airborne then min 3 (wraith.vy + 0.1)
    else max (-4) (if wraith.directionY < 0 && wraith.vy > 0 then wraith.vy - 0.1 else wraith.vy)
  let vx := accelerate wraith.vx wraith.direction 2 0.1 0.1 0.05
  let vy := accelerate vy wraith.directionY 1.5 0.04 0.05 0.03
  return { wraith with vx, vy, justHit := false }

def Wraith.advance (wraith : Wraith) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) : Option Wraith := do
  if wraith.health == 0 then failure
  let next := wraith.advanceAI world water magma player dead
  let vx := if -0.005 < next.vx.toFloat && next.vx.toFloat < 0.005 then 0 else next.vx
  let spriteDirection := if vx < 0 then -1 else if vx > 0 then 1 else next.spriteDirection
  let frameClock := (next.frameClock + 1) % 6
  let frame := if frameClock == 0 then (next.frame + 1) % wraithFrameCount else next.frame
  let moved := { next with x := next.x + vx, y := next.y + next.vy, vx,
                            rotation := vx * 0.1, spriteDirection, frame, frameClock }
  let timeLeft ← CasterAI.checkActive moved.centerX moved.centerY wraithWidth wraithHeight player moved.timeLeft
  return { moved with timeLeft }

end Lean4Terria
