import Lean4Terria.OriginalPlayerCollision

namespace Lean4Terria.OriginalPlayerMovement

open OriginalCollision (Grid single)
open OriginalPlayerCollision (Body ContactEffects)

structure Controls where
  left : Bool := false
  right : Bool := false
  jump : Bool := false
  down : Bool := false
  deriving BEq, Repr

structure State where
  body : Body
  direction : Int := 1
  releaseJump : Bool := false
  justJumped : Bool := false
  rocketRelease : Bool := false
  sliding : Bool := false
  slideDir : Int := 0
  autoJump : Bool := false
  runAcceleration : Float32 := 0.08
  runSlowdown : Float32 := 0.2
  maxRunSpeed : Float32 := 3
  accRunSpeed : Float32 := 0
  maxFallSpeed : Float32 := 10
  jumpHeight : Int := 15
  jumpSpeed : Float32 := 5.01
  dashDelay : Int := 0
  onWrongGround : Bool := false
  deriving BEq, Repr

private def decelerate (speed slowdown : Float32) : Float32 :=
  if speed > slowdown then speed - slowdown
  else if speed < -slowdown then speed + slowdown
  else 0

/-- The upper-world gravity multiplier depends on position and world size. -/
def gravityScale (positionY : Float32) (worldWidth : Nat) (worldSurface : Float)
    (remixWorld : Bool) : Float32 :=
  let size := single worldWidth / 4200
  let relativeY := positionY / 16 - (60 + 10 * (size * size))
  let scale := (relativeY.toFloat / (worldSurface / (if remixWorld then 1 else 6))).toFloat32
  min 1 (max (if remixWorld then 0.1 else 0.25) scale)

/-- Reset the effective movement stats for a dry player without equipment. -/
def State.prepareDryTick (state : State) (worldWidth : Nat) (worldSurface : Float)
    (remixWorld : Bool := false) : State :=
  let gravity := 0.4 * gravityScale state.body.position.y worldWidth worldSurface remixWorld
  { state with
    body := { state.body with gravity }
    runAcceleration := 0.08
    runSlowdown := 0.2
    maxRunSpeed := 3
    accRunSpeed := 3
    maxFallSpeed := 10 + 0.01
    jumpHeight := 15
    jumpSpeed := 5.01
    onWrongGround := false }

/-- On-foot horizontal movement. The first acceleration band may cross the
speed threshold. The second band applies only a fifth of the acceleration. -/
def State.horizontalMovement (state : State) (input : Controls) (canTurn : Bool := true) : State :=
  Id.run do
    let x := state.body.velocity.x
    let grounded := state.body.velocity.y == 0
    let slowdown := state.runSlowdown
    let towardLeft := if x > slowdown then x - slowdown else x
    let towardRight := if x < -slowdown then x + slowdown else x
    let direction :=
      if canTurn && input.left != input.right then (if input.left then -1 else 1)
      else state.direction
    let nextX : Float32 := Id.run do
      if input.left && x > -state.maxRunSpeed then
        let speed := towardLeft - state.runAcceleration
        return if !state.onWrongGround then speed
          else if speed < -slowdown then speed + slowdown else 0
      if input.right && x < state.maxRunSpeed then
        let speed := towardRight + state.runAcceleration
        return if !state.onWrongGround then speed
          else if speed > slowdown then speed - slowdown else 0
      if input.left && x > -state.accRunSpeed && state.dashDelay ≥ 0 then
        let speed := if grounded then towardLeft - state.runAcceleration * 0.2 else x
        return if !state.onWrongGround then speed
          else if speed < slowdown then speed + slowdown else 0
      if input.right && x < state.accRunSpeed && state.dashDelay ≥ 0 then
        let speed := if grounded then towardRight + state.runAcceleration * 0.2 else x
        return if !state.onWrongGround then speed
          else if speed > slowdown then speed - slowdown else 0
      return decelerate x (if grounded then slowdown else slowdown * 0.5)
    return { state with direction, body := { state.body with velocity := { state.body.velocity with x := nextX } } }

/-- Jump-key handling for an ordinary player on foot. The caller applies
gravity and collision after this phase. -/
def State.jumpMovement (state : State) (input : Controls) : State := Id.run do
  let body := state.body
  if !input.jump then
    return { state with body := { body with jump := 0 }, releaseJump := true, rocketRelease := true }
  let autoJump := state.autoJump && !state.sliding
  let pressed := { state with releaseJump := false, autoJump }
  let grounded := body.velocity.y == 0
  let lift := -state.jumpSpeed * body.gravityDirection
  if body.jump > 0 then
    let continuation := if grounded then { body with jump := 0 }
      else { body with jump := body.jump - 1, velocity := { body.velocity with y := lift } }
    return { pressed with body := continuation }
  if (grounded || state.sliding) && (state.releaseJump || autoJump) then
    let x := if state.sliding then single (-3 * state.slideDir) else body.velocity.x
    let takeoff := { body with jump := state.jumpHeight, velocity := ⟨x, lift⟩ }
    return { pressed with body := takeoff, justJumped := true, rocketRelease := false }
  return pressed

/-- At this point in Player.Update, GrabItems reads the player's current
position and the velocity for the upcoming movement. -/
def State.beginOnFoot (initial : State) (input : Controls) (canTurn : Bool := true) : State :=
  let state := { initial with justJumped := false }
  let state := (state.horizontalMovement input canTurn).jumpMovement input
  let body := state.body
  let gravityDir := body.gravityDirection
  let fallingSpeed := body.velocity.y + body.gravity * gravityDir
  let y := if gravityDir == 1 then min fallingSpeed state.maxFallSpeed
    else max fallingSpeed (-state.maxFallSpeed)
  { state with body := { body with velocity.y := y, controlDown := input.down } }

def State.finishOnFootWith (state : State) (grid : Grid) (collide : Body → Bool → Body)
    (effects : ContactEffects) : State := Id.run do
  let mut body := state.body
  let gravityDir := body.gravityDirection
  let x := body.velocity.x
  let y := body.velocity.y
  let offsetStep := (1 + x.abs / 3) * body.stepSpeed
  body := { body with
    gfxOffY := max (-32) (min 32 (decelerate body.gfxOffY offsetStep))
    velocity := ⟨max (-16) (min 16 x), max (-16) (min 16 y)⟩ }
  body := body.slopeDownMovement grid
  if body.velocity.y == body.gravity then
    body := body.stepDown grid
  let canStepUp := if gravityDir == -1 then body.velocity.y ≤ body.gravity && !body.controlUp
    else body.velocity.y ≥ body.gravity && !body.controlDown
  if canStepUp then body := body.stepUp grid
  let beforeCollision := body.velocity
  let fallThrough := body.controlDown
  body := collide body fallThrough
  body := effects.onTouchingTiles body (beforeCollision.y.abs > body.gravity)
  body := (body.slopingCollision grid fallThrough false).1
  body := effects.onConveyor body
  let slideDir := if beforeCollision.x == body.velocity.x then 0
    else if beforeCollision.x < 0 then -1 else if beforeCollision.x > 0 then 1 else 0
  if (gravityDir == 1 && body.collision.up) || (gravityDir == -1 && body.collision.down) then
    body := { body with jump := 0, velocity := { body.velocity with y := 0.01 * gravityDir } }
  return { state with body, slideDir }

def State.finishOnFoot (state : State) (grid : Grid) (effects : ContactEffects := {}) : State :=
  state.finishOnFootWith grid (fun body fallThrough => body.dryCollision grid fallThrough false effects)
    effects

/-- Dry, unequipped on-foot movement through the terrain. The caller supplies
the effective gravity and run stats after environment and equipment updates.
Local input gathering, fall damage, and world-border handling occur separately. -/
def State.stepOnFoot (state : State) (grid : Grid) (input : Controls)
    (effects : ContactEffects := {}) : State :=
  (state.beginOnFoot input).finishOnFoot grid effects

end Lean4Terria.OriginalPlayerMovement
