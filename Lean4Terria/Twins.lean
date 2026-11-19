import Lean4Terria.UnderworldCreatures
import Lean4Terria.DarkCaster

namespace Lean4Terria

inductive TwinKind where
  | retinazer | spazmatism
  deriving BEq, Repr, Inhabited

def TwinKind.name : TwinKind → String
  | .retinazer => "Retinazer"
  | .spazmatism => "Spazmatism"

def TwinKind.sprite : TwinKind → Nat
  | .retinazer => 125
  | .spazmatism => 126

def TwinKind.health : TwinKind → Nat
  | .retinazer => 20000
  | .spazmatism => 23000

def TwinKind.width (_ : TwinKind) : Nat := 100
def TwinKind.height (_ : TwinKind) : Nat := 110

inductive TwinAttackKind where
  | eyeLaser | deathLaser | cursedFlame | eyeFire | destroyerLaser | probeLaser
  deriving BEq, Repr, Inhabited

/-- Original projectile spawn values in pixels and pre-difficulty damage. -/
structure TwinAttack where
  kind : TwinAttackKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  deriving Repr, Inhabited

/-- The Classic NPC 125/126 state. Positions are top-left native pixels.
The AI retains original floating-point clocks and phase values. -/
structure Twin where
  kind : TwinKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  ai2 : Float32 := 0
  ai3 : Float32 := 0
  localAI1 : Float32 := 0
  localAI2 : Float32 := 0
  powered : Bool := false
  health : Nat
  timeLeft : Nat := 937
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq Twin where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.ai0.toBits == b.ai0.toBits && a.ai1.toBits == b.ai1.toBits && a.ai2.toBits == b.ai2.toBits &&
    a.ai3.toBits == b.ai3.toBits && a.localAI1.toBits == b.localAI1.toBits &&
    a.localAI2.toBits == b.localAI2.toBits && a.powered == b.powered && a.health == b.health &&
    a.timeLeft == b.timeLeft && a.frame == b.frame && a.frameClock == b.frameClock

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (value : Int) : Float32 := single value / 16

def Twin.centerX (twin : Twin) : Float32 := twin.x + 50
def Twin.centerY (twin : Twin) : Float32 := twin.y + 55

def Twin.atCenter (kind : TwinKind) (x y : Float32) : Twin :=
  { kind, x := x - 50, y := y - 55, health := kind.health }

def Twin.hitBox (twin : Twin) : HitBox := CasterAI.npcBox twin.x twin.y 100 110

def Twin.contact (twin : Twin) (player : Player) : Option Int :=
  CasterAI.contact twin.hitBox twin.centerX player

def Twin.secondForm (twin : Twin) : Bool := twin.ai0 == 3

def Twin.transforming (twin : Twin) : Bool := twin.ai0 == 1 || twin.ai0 == 2

def Twin.contactDamage (twin : Twin) : Nat :=
  match twin.kind with
  | .retinazer => if twin.powered then 67 else 45
  | .spazmatism => if twin.powered then 75 else 50

def Twin.defense (twin : Twin) : Nat :=
  if !twin.powered then 10 else
    match twin.kind with
    | .retinazer => 20
    | .spazmatism => 28

private def clockValid (value : Float32) (bound : Nat) : Bool :=
  value ≥ 0 && value < single bound && value == value.floor

private def Twin.phaseValid (twin : Twin) : Bool :=
  let ret := twin.kind == TwinKind.retinazer
  if twin.ai0 == 0 then
    !twin.powered && twin.localAI1 == 0 && twin.localAI2 == 0 &&
      if twin.ai1 == 0 then clockValid twin.ai2 600 && clockValid twin.ai3 60
      else if twin.ai1 == 1 || twin.ai1 == 2 then
        clockValid twin.ai3 (if ret then 4 else 10) &&
          clockValid twin.ai2 (if twin.ai1 == 1 then 1 else if ret then 70 else 42)
      else false
  else if twin.transforming then
    !twin.powered && clockValid twin.ai1 100 && twin.ai2 ≥ 0 && twin.ai2 ≤ 0.5 && twin.ai3 == 0 &&
      twin.localAI1 == 0 && twin.localAI2 == 0
  else if twin.secondForm then
    if !twin.powered then twin.ai1 == 0 && twin.ai2 == 0 && twin.ai3 == 0 &&
      twin.localAI1 == 0 && twin.localAI2 == 0
    else if ret then
      twin.ai3 == 0 && twin.localAI2 == 0 &&
        ((twin.ai1 == 0 && clockValid twin.ai2 300) || (twin.ai1 == 1 && clockValid twin.ai2 180))
    else
      clockValid twin.localAI2 23 &&
        if twin.ai1 == 0 then clockValid twin.ai2 400 && twin.ai3 == 0
        else if twin.ai1 == 1 || twin.ai1 == 2 then
          clockValid twin.ai3 6 && clockValid twin.ai2 (if twin.ai1 == 1 then 1 else 80)
        else false
  else false

def Twin.valid (twin : Twin) (world : World) : Bool :=
  CasterAI.validMotion world twin.x twin.y 0 0 && twin.vx.isFinite && twin.vx.abs ≤ 16 &&
    twin.vy.isFinite && twin.vy.abs ≤ 96 && twin.rotation.isFinite && twin.rotation.abs < 7 &&
    0 < twin.health && twin.health ≤ twin.kind.health && 0 < twin.timeLeft && twin.timeLeft ≤ 937 &&
    twin.frameClock < 21 && twin.frame == twin.frameClock / 7 + (if twin.ai0 > 1 then 3 else 0) &&
    twin.localAI1 ≥ 0 && twin.localAI1 ≤ 16777220 && twin.phaseValid

/-- Player position and integer native hitbox dimensions used by AI styles 30/31. -/
structure TwinTarget where
  x : Float32
  y : Float32
  width : Nat := 11
  height : Nat := 22
  deriving Repr, Inhabited

def TwinTarget.ofPlayer (player : Player) : TwinTarget :=
  { x := pixels player.x, y := pixels player.y, width := playerWidth.toNat / 16,
    height := playerHeight.toNat / 16 }

private def TwinTarget.centerX (target : TwinTarget) : Float32 := target.x + single (target.width / 2 : Nat)
private def TwinTarget.centerY (target : TwinTarget) : Float32 := target.y + single (target.height / 2 : Nat)

private def angleOf (y x : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32

private def wrapAngle (angle : Float32) : Float32 :=
  if angle < 0 then angle + 6.283
  else if angle.toFloat > 6.283 then angle - 6.283
  else angle

private def turnToward (rotation target step : Float32) : Float32 :=
  let rotation := if rotation < target then
      if (target - rotation).toFloat > 3.1415 then rotation - step else rotation + step
    else if rotation > target then
      if (rotation - target).toFloat > 3.1415 then rotation + step else rotation - step
    else rotation
  let snap := fun (angle : Float32) => if target - step < angle && angle < target + step then target else angle
  snap (wrapAngle (snap rotation))

private def Twin.lookAngle (twin : Twin) (target : TwinTarget) : Float32 :=
  let dx := twin.centerX - target.x - single (target.width / 2 : Nat)
  let dy := twin.y + 110 - 59 - target.y - single (target.height / 2 : Nat)
  wrapAngle (angleOf dy dx + 1.57)

private def length (dx dy : Float32) : Float32 := (dx * dx + dy * dy).toFloat.sqrt.toFloat32

private def twinAimVelocity (dx dy speed : Float32) : Float32 × Float32 :=
  let distance := length dx dy
  if distance == 0 then (0, 0) else
    let scale := speed / distance
    (dx * scale, dy * scale)

private def steerAxis (velocity aim step : Float32) : Float32 :=
  if velocity < aim then
    let moved := velocity + step
    if moved < 0 && aim > 0 then moved + step else moved
  else if velocity > aim then
    let moved := velocity - step
    if moved > 0 && aim < 0 then moved - step else moved
  else velocity

private def Twin.steer (twin : Twin) (dx dy speed step : Float32) : Twin :=
  let (vx, vy) := twinAimVelocity dx dy speed
  { twin with vx := steerAxis twin.vx vx step, vy := steerAxis twin.vy vy step }

private def dampAxis (velocity factor : Float32) : Float32 :=
  let damped := velocity * factor
  if -0.1 < damped.toFloat && damped.toFloat < 0.1 then 0 else damped

private def Twin.damp (twin : Twin) (factor : Float32) : Twin :=
  { twin with vx := dampAxis twin.vx factor, vy := dampAxis twin.vy factor }

private def Twin.horizontalOffset (twin : Twin) (target : TwinTarget) (offset : Float32) : Float32 :=
  if twin.centerX < target.x + single target.width then -offset else offset

private def Twin.aim (twin : Twin) (target : TwinTarget) (speed : Float32) : Float32 × Float32 :=
  twinAimVelocity (target.centerX - twin.centerX) (target.centerY - twin.centerY) speed

private def jitter (roll : Nat → Nat) (sample : Nat) (scale : Float32) : Float32 :=
  single ((roll sample % 81 : Nat) - (40 : Int)) * scale

private def Twin.shot (twin : Twin) (kind : TwinAttackKind) (damage : Nat)
    (vx vy muzzle : Float32) : TwinAttack :=
  { kind, x := twin.centerX + vx * muzzle, y := twin.centerY + vy * muzzle, vx, vy, damage }

private def Twin.chargeStart (twin : Twin) (target : TwinTarget) (angle speed : Float32) : Twin :=
  let (vx, vy) := twin.aim target speed
  { twin with ai1 := 2, vx, vy, rotation := angle }

private def Twin.charge (twin : Twin) (angle fullSpeed duration count factor : Float32) : Twin :=
  let ticks := twin.ai2 + 1
  let next := if ticks < fullSpeed then { twin with rotation := angleOf twin.vy twin.vx - 1.57 }
    else twin.damp factor
  if ticks < duration then { next with ai2 := ticks }
  else
    let charges := next.ai3 + 1
    if charges ≥ count then { next with ai1 := 0, ai2 := 0, ai3 := 0, rotation := angle }
    else { next with ai1 := 1, ai2 := 0, ai3 := charges, rotation := angle }

private def Twin.firstHover (twin : Twin) (target : TwinTarget) (roll : Nat → Nat) : Twin × Array TwinAttack :=
  let ret := twin.kind == TwinKind.retinazer
  let offset := twin.horizontalOffset target (if ret then 300 else 400)
  let dx := target.centerX + offset - twin.centerX
  let dy := (if ret then target.centerY - 300 else target.centerY) - twin.centerY
  let next := twin.steer dx dy (if ret then 7 else 12) (if ret then 0.1 else 0.4)
  let ticks := next.ai2 + 1
  if ticks ≥ 600 then ({ next with ai1 := 1, ai2 := 0, ai3 := 0 }, #[])
  else
    let next := { next with ai2 := ticks }
    if ret && (twin.y + 110 ≥ target.y || length dx dy ≥ 400) then (next, #[])
    else
      let fire := next.ai3 + 1
      if fire < 60 then ({ next with ai3 := fire }, #[])
      else
        let (vx, vy) := twin.aim target (if ret then 9 else 12)
        let spread := if ret then 0.08 else 0.05
        let shot := twin.shot (if ret then .eyeLaser else .cursedFlame) (if ret then 20 else 25)
          (vx + jitter roll 0 spread) (vy + jitter roll 1 spread) (if ret then 15 else 4)
        ({ next with ai3 := 0 }, #[shot])

private def Twin.firstForm (twin : Twin) (target : TwinTarget) (angle : Float32)
    (roll : Nat → Nat) : Twin × Array TwinAttack :=
  let ret := twin.kind == TwinKind.retinazer
  let (next, shots) := if twin.ai1 == 0 then twin.firstHover target roll
    else if twin.ai1 == 1 then (twin.chargeStart target angle (if ret then 12 else 13), #[])
    else if ret then (twin.charge angle 25 70 4 0.96, #[])
    else (twin.charge angle 8 42 10 0.9, #[])
  if next.health * 5 < next.kind.health * 2 then
    ({ next with ai0 := 1, ai1 := 0, ai2 := 0, ai3 := 0 }, shots)
  else (next, shots)

private def Twin.transform (twin : Twin) : Twin :=
  let spin := if twin.ai0 == 1 then min 0.5 (twin.ai2 + 0.005) else max 0 (twin.ai2 - 0.005)
  let next : Twin := { twin.damp 0.98 with ai1 := twin.ai1 + 1, ai2 := spin, rotation := twin.rotation + spin }
  if next.ai1 < 100 then next else
    { next with ai0 := twin.ai0 + 1, ai1 := 0, ai2 := if twin.ai0 == 2 then 0 else spin }

private def Twin.fireClock (twin : Twin) (rapid : Bool) : Float32 :=
  let maxHealth := twin.kind.health
  let clock := twin.localAI1 + 1
  let clock := if twin.health * 4 < maxHealth * 3 then clock + (if rapid then 0.5 else 1) else clock
  let clock := if twin.health * 2 < maxHealth then clock + (if rapid then 0.75 else 1) else clock
  let clock := if twin.health * 4 < maxHealth then clock + 1 else clock
  if twin.health * 10 < maxHealth then clock + (if rapid then 1.5 else 2) else clock

private def Twin.retinazerSecond (twin : Twin) (target : TwinTarget) (lineOfSight : Bool) : Twin × Array TwinAttack :=
  let rapid := twin.ai1 == 1
  let offset := if rapid then twin.horizontalOffset target 340 else 0
  let dx := target.centerX + offset - twin.centerX
  let dy := (if rapid then target.centerY else target.centerY - 300) - twin.centerY
  let next := twin.steer dx dy 8 (if rapid then 0.2 else 0.15)
  let ticks := twin.ai2 + 1
  let next := if ticks ≥ (if rapid then 180 else 300) then
      { next with ai1 := if rapid then 0 else 1, ai2 := 0, ai3 := 0 }
    else { next with ai2 := ticks }
  let rotation := angleOf (target.centerY - twin.centerY) (target.centerX - twin.centerX) - 1.57
  let fire := twin.fireClock rapid
  let next := { next with rotation, localAI1 := fire }
  if fire ≤ (if rapid then 60 else 180) || !lineOfSight then (next, #[])
  else
    let (vx, vy) := twin.aim target (if rapid then 9 else 8.5)
    ({ next with localAI1 := 0 }, #[twin.shot .deathLaser (if rapid then 18 else 25) vx vy 15])

private def Twin.spazmatismSecond (twin : Twin) (target : TwinTarget) (angle : Float32)
    (lineOfSight : Bool) (roll : Nat → Nat) : Twin × Array TwinAttack :=
  if twin.ai1 == 1 then (twin.chargeStart target angle 14, #[])
  else if twin.ai1 == 2 then (twin.charge angle 50 80 6 0.93, #[])
  else
    let dx := target.centerX + twin.horizontalOffset target 180 - twin.centerX
    let next := twin.steer dx (target.centerY - twin.centerY) 4 0.1
    let ticks := twin.ai2 + 1
    let next := if ticks ≥ 400 then { next with ai1 := 1, ai2 := 0, ai3 := 0 }
      else { next with ai2 := ticks }
    if ticks ≥ 400 || !lineOfSight then (next, #[]) else
    let fire := twin.fireClock false
    let soundClock := twin.localAI2 + 1
    let next := { next with localAI1 := fire, localAI2 := if soundClock > 22 then 0 else soundClock }
    if fire ≤ 8 then (next, #[]) else
    let (vx, vy) := twin.aim target 6
    let vx := vx + jitter roll 1 0.01 + next.vx * 0.5
    let vy := vy + jitter roll 0 0.01 + next.vy * 0.5
    let shot : TwinAttack := {
      kind := .eyeFire, x := twin.centerX - vx, y := twin.centerY - vy,
      vx, vy, damage := 30 }
    ({ next with localAI1 := 0 }, #[shot])

/-- Classic single-player branches of NPC.AI styles 30 and 31, before
NPC.UpdateNPC integrates velocity. `roll 0` and `roll 1` select the two
jitter draws for an attack. Cosmetic dust and sound are presentation state. -/
def Twin.advanceAI (twin : Twin) (target : TwinTarget) (daylight dead lineOfSight : Bool)
    (partnerTimeLeft : Nat) (roll : Nat → Nat := fun _ => 40) : Twin × Array TwinAttack :=
  let angle := twin.lookAngle target
  let step := if twin.kind == TwinKind.retinazer then 0.1 else 0.15
  let twin := { twin with rotation := turnToward twin.rotation angle step }
  if daylight || dead then ({ twin with vy := twin.vy - 0.04, timeLeft := min 10 twin.timeLeft }, #[])
  else
    let twin := if twin.timeLeft < 10 then { twin with timeLeft := max twin.timeLeft (partnerTimeLeft - 1) }
      else twin
    if twin.ai0 == 0 then twin.firstForm target angle roll
    else if twin.transforming then (twin.transform, #[])
    else
      let twin := { twin with powered := true }
      if twin.kind == TwinKind.retinazer then twin.retinazerSecond target lineOfSight
      else twin.spazmatismSecond target angle lineOfSight roll

private def Twin.checkActive (twin : Twin) (player : Player) : Nat :=
  let left := (twin.centerX.toFloat - 1060).toInt32.toInt * 16
  let top := (twin.centerY.toFloat - 710).toInt32.toInt * 16
  let view : HitBox := ⟨left, top, left + 2120 * 16, top + 1420 * 16⟩
  (if view.intersects (HitBox.ofNpcTarget player) then 750 else twin.timeLeft) - 1

/-- Advance one simulation tick, including no-tile-collision movement and
CheckActive. The partner's countdown is the value at this NPC's update slot. -/
def Twin.advance (twin : Twin) (world : World) (player : Player) (daylight dead partnerAlive : Bool)
    (roll : Nat → Nat := fun _ => 40) (partnerTimeLeft : Nat := 750) : Option Twin × Array TwinAttack :=
  if twin.health == 0 then (none, #[])
  else
    let lineOfSight := npcCanSee world twin.x twin.y 100 110 player
    let (next, shots) := twin.advanceAI (TwinTarget.ofPlayer player) daylight dead lineOfSight
      (if partnerAlive then partnerTimeLeft else 0) roll
    let vx := if -0.005 < next.vx.toFloat && next.vx.toFloat < 0.005 then 0 else next.vx
    let moved := { next with x := next.x + vx, y := next.y + next.vy, vx }
    let timeLeft := moved.checkActive player
    let frameClock := (moved.frameClock + 1) % 21
    let frame := frameClock / 7 + (if moved.ai0 > (1 : Float32) then 3 else 0)
    if timeLeft == 0 then (none, shots) else (some { moved with timeLeft, frameClock, frame }, shots)

/-- Spawn off the compact screen by sampling air outside the player's safe
rectangle, then search down to the first solid tile as in SpawnOnPlayer. -/
private def spawnTwin (world : World) (player : Player) (kind : TwinKind) (roll : Nat → Nat)
    (offset : Nat) : Twin := Id.run do
  let pos := player.centerTile
  let left := max 4 (pos.x - 50)
  let right := min ((world.width : Int) - 5) (pos.x + 50)
  let top := max 9 (pos.y - 31)
  let bottom := min ((world.height : Int) - 3) (pos.y + 31)
  for attempt in [:128] do
    let x := left + roll (offset + attempt * 2) % ((right - left).toNat + 1)
    let y := top + roll (offset + attempt * 2 + 1) % ((bottom - top).toNat + 1)
    if (x - pos.x).natAbs ≤ 24 && (y - pos.y).natAbs ≤ 15 then continue
    if (world.tile ⟨x, y⟩).solid then continue
    for floor in [y.toNat:world.height - 1] do
      if (world.tile ⟨x, floor⟩).solid then
        if (x - pos.x).natAbs > 24 || ((floor : Int) - pos.y).natAbs > 15 then
          return Twin.atCenter kind (single (x * 16 + 8)) (single floor * 16 - 55)
        break
  let side : Int := if kind == TwinKind.retinazer then -32 else 32
  let x := max 4 (min ((world.width : Int) - 5) (pos.x + side))
  let y := max 9 (pos.y - 19)
  return Twin.atCenter kind (single (x * 16 + 8)) (single (y * 16 + 8))

def Twin.spawnPair (world : World) (player : Player) (roll : Nat → Nat) : Twin × Twin :=
  (spawnTwin world player .retinazer roll 0, spawnTwin world player .spazmatism roll 256)

end Lean4Terria
