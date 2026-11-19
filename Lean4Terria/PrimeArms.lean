import Lean4Terria.PrimeHead
import Lean4Terria.PrimeShots
import Lean4Terria.DarkCaster

namespace Lean4Terria

inductive PrimeArmKind where
  | cannon | saw | vice | laser
  deriving BEq, Repr, Inhabited

def PrimeArmKind.all : Array PrimeArmKind := #[.cannon, .saw, .vice, .laser]

def PrimeArmKind.id : PrimeArmKind → Nat
  | .cannon => 128
  | .saw => 129
  | .vice => 130
  | .laser => 131

def PrimeArmKind.width (_ : PrimeArmKind) : Nat := 52
def PrimeArmKind.height (_ : PrimeArmKind) : Nat := 52

def PrimeArmKind.health : PrimeArmKind → Nat
  | .cannon => 7000
  | .saw | .vice => 9000
  | .laser => 6000

def PrimeArmKind.damage : PrimeArmKind → Nat
  | .cannon => 30
  | .saw => 56
  | .vice => 52
  | .laser => 29

def PrimeArmKind.defense : PrimeArmKind → Nat
  | .cannon => 23
  | .saw => 38
  | .vice => 34
  | .laser => 20

def PrimeArmKind.side : PrimeArmKind → Int
  | .cannon | .vice => -1
  | .saw | .laser => 1

/-- Ordinary Classic arm state from NPC AI styles 33 through 36. `phase`,
`timer` and `shotClock` hold the integral values of ai[2], ai[3] and localAI[0].
The position is the NPC's top-left in native pixels. -/
structure PrimeArm where
  kind : PrimeArmKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  health : Nat
  phase : Nat := 0
  timer : Nat := 0
  shotClock : Nat := 0
  direction : Int := 0
  facing : Int := 1
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq PrimeArm where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.health == b.health && a.phase == b.phase && a.timer == b.timer && a.shotClock == b.shotClock &&
    a.direction == b.direction && a.facing == b.facing && a.frame == b.frame && a.frameClock == b.frameClock

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (n : Int) : Float32 := single n / 16

def PrimeArm.atHead (kind : PrimeArmKind) (head : PrimeHead) : PrimeArm :=
  { kind
    x := single (head.centerX.toInt32.toInt - 26)
    y := single (head.y.toInt32.toInt + 51 - 52)
    health := kind.health
    timer := if kind == .vice || kind == .laser then 150 else 0
    facing := -kind.side }

def PrimeArm.centerX (arm : PrimeArm) : Float32 := arm.x + 26
def PrimeArm.centerY (arm : PrimeArm) : Float32 := arm.y + 26

def PrimeArm.hitBox (arm : PrimeArm) : HitBox :=
  CasterAI.npcBox arm.x arm.y arm.kind.width arm.kind.height

def PrimeArm.contact (arm : PrimeArm) (player : Player) : Option Int :=
  CasterAI.contact arm.hitBox arm.centerX player

def PrimeArmKind.frameCount : PrimeArmKind → Nat
  | .saw | .vice => 2
  | .cannon | .laser => 1

private def PrimeArmKind.frameDelay : PrimeArmKind → Nat
  | .saw => 2
  | .vice => 8
  | .cannon | .laser => 1

private def PrimeArm.animate (arm : PrimeArm) : PrimeArm :=
  let facing := if arm.kind.frameCount > 1 && arm.vy == 0 then arm.direction else -arm.kind.side
  let frameClock := arm.frameClock + 1
  if frameClock < arm.kind.frameDelay then { arm with facing, frameClock }
  else { arm with facing, frame := (arm.frame + 1) % arm.kind.frameCount, frameClock := 0 }

private def PrimeArm.stateValid (arm : PrimeArm) : Bool :=
  match arm.kind with
  | .saw | .vice => (arm.phase ≤ 5 || arm.phase == 99) && arm.timer < 600 && arm.shotClock == 0
  | .cannon => arm.phase ≤ 1 && arm.timer < 1100 && arm.shotClock ≤ 140
  | .laser => arm.phase ≤ 1 && arm.timer < 800 && arm.shotClock ≤ 200

def PrimeArm.valid (arm : PrimeArm) (world : World) : Bool :=
  CasterAI.validMotion world arm.x arm.y 0 0 && arm.vx.isFinite && arm.vy.isFinite &&
    arm.vx.abs ≤ 80 && arm.vy.abs ≤ 80 && arm.rotation.isFinite && -5 ≤ arm.rotation && arm.rotation ≤ 5 &&
    0 < arm.health && arm.health ≤ arm.kind.health &&
    arm.direction.natAbs ≤ 1 && arm.facing.natAbs ≤ 1 &&
    arm.frame < arm.kind.frameCount && arm.frameClock < arm.kind.frameDelay && arm.stateValid

private def length (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32

private def normalized (x y speed : Float32) : Float32 × Float32 :=
  let distance := length x y
  if distance == 0 then (0, 0)
  else
    let factor := speed / distance
    (x * factor, y * factor)

private def angle (x y : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32

private def PrimeArm.linkVector (arm : PrimeArm) (head : PrimeHead) (reach : Float32 := 200) :
    Float32 × Float32 :=
  (head.centerX - reach * single arm.kind.side - arm.centerX, head.y + 230 - arm.centerY)

private def PrimeArm.linkRotation (arm : PrimeArm) (head : PrimeHead) (reach : Float32 := 200) : Float32 :=
  let (dx, dy) := arm.linkVector head reach
  angle dx dy + 1.57

private def PrimeArm.playerVector (arm : PrimeArm) (player : Player) : Float32 × Float32 :=
  (pixels (bodyCenterX player) - arm.centerX, pixels (bodyCenterY player) - arm.centerY)

private def PrimeArm.targetDirection (arm : PrimeArm) (player : Player) (dead : Bool) : Int :=
  if dead then arm.direction
  else
    let targetX := (pixels player.x).toInt32.toInt + playerWidth / 32
    if single targetX < arm.centerX then -1 else 1

private def dampPositive (velocity brake : Float32) : Float32 :=
  if velocity > 0 then velocity * brake else velocity

private def dampNegative (velocity brake : Float32) : Float32 :=
  if velocity < 0 then velocity * brake else velocity

-- The source caps the velocity still moving away after applying the correction.
private def forceNegative (velocity brake step limit : Float32) : Float32 :=
  min limit (dampPositive velocity brake - step)

private def forcePositive (velocity brake step limit : Float32) : Float32 :=
  max (-limit) (dampNegative velocity brake + step)

private def steerBand (position velocity low high brake step limit : Float32) : Float32 :=
  if position > high then forceNegative velocity brake step limit
  else if position < low then forcePositive velocity brake step limit
  else velocity

-- These are consecutive comparisons in the assembly, so the second comparison
-- observes the velocity modified by the first on an overshoot.
private def chaseAxis (velocity wanted brake step : Float32) : Float32 :=
  let velocity := if velocity > wanted then dampPositive velocity brake - step else velocity
  if velocity < wanted then dampNegative velocity brake + step else velocity

private def PrimeArm.chase (arm : PrimeArm) (wanted : Float32 × Float32)
    (brake xStep yStep : Float32) : PrimeArm :=
  { arm with
    vx := chaseAxis arm.vx wanted.1 brake xStep
    vy := chaseAxis arm.vy wanted.2 brake yStep }

private def PrimeArm.countPhase (arm : PrimeArm) (limit next : Nat) : PrimeArm :=
  if arm.timer + 1 ≥ limit then { arm with phase := next, timer := 0 }
  else { arm with timer := arm.timer + 1 }

private def PrimeArm.recallPhase (arm : PrimeArm) (head : PrimeHead) : PrimeArm :=
  let (dx, dy) := arm.linkVector head
  let distance := length dx dy
  if arm.phase != 99 && distance > 800 then { arm with phase := 99 }
  else if arm.phase == 99 && distance < 400 then { arm with phase := 0 }
  else arm

private def PrimeArm.returnToHead (arm : PrimeArm) (head : PrimeHead) : PrimeArm :=
  { arm with
    vx := steerBand arm.centerX arm.vx head.centerX head.centerX 0.96 0.5 12
    vy := steerBand arm.y arm.vy head.y head.y 0.96 0.1 8 }

private def PrimeArm.fallAfterPlayerDeath (arm : PrimeArm) : PrimeArm :=
  { arm with vy := min 16 (arm.vy + 0.1) }

private def PrimeArm.advanceSaw (arm : PrimeArm) (head : PrimeHead) (player : Player) (dead : Bool) :
    PrimeArm :=
  let arm := arm.recallPhase head
  let link := arm.linkRotation head
  match arm.phase with
  | 99 => arm.returnToHead head
  | 0 | 3 =>
    let arm := if head.phase != .hover then
        let arm := if dead then arm.fallAfterPlayerDeath
          else
            let (dx, dy) := arm.playerVector player
            let arm := { arm with direction := arm.targetDirection player dead }
            arm.chase (normalized dx dy 7) 0.97 0.05 0.05
        arm.countPhase 600 0
      else
        let arm := arm.countPhase 300 (arm.phase + 1)
        { arm with
          vx := steerBand arm.centerX arm.vx (head.centerX - 250) head.centerX 0.96 0.3 12
          vy := steerBand arm.y arm.vy (head.y + 260) (head.y + 320) 0.96 0.04 3 }
    { arm with rotation := link }
  | 1 =>
    if arm.y < head.y - 200 then
      let (dx, dy) := arm.playerVector player
      let (vx, vy) := normalized dx dy 22
      { arm with phase := 2, rotation := link, vx, vy, direction := arm.targetDirection player dead }
    else
      { arm with vx := arm.vx * 0.95, vy := max (-8) (arm.vy - 0.1), rotation := link }
  | 2 =>
    if arm.y > pixels player.y || arm.vy < 0 then { arm with phase := 3 } else arm
  | 4 =>
    let (dx, dy) := arm.playerVector player
    let arm := arm.chase (normalized dx dy 7) 0.97 0.05 0.05
    { arm.countPhase 600 0 with rotation := link, direction := arm.targetDirection player dead }
  | 5 =>
    let targetX := pixels (bodyCenterX player)
    if (arm.vx > 0 && arm.centerX > targetX) || (arm.vx < 0 && arm.centerX < targetX) then
      { arm with phase := 0 }
    else arm
  | _ => arm

private def PrimeArm.advanceVice (arm : PrimeArm) (head : PrimeHead) (player : Player) (dead : Bool) :
    PrimeArm :=
  let arm := arm.recallPhase head
  let link := arm.linkRotation head
  match arm.phase with
  | 99 => arm.returnToHead head
  | 0 | 3 =>
    let arm := if head.phase != .hover then
        if dead then arm.fallAfterPlayerDeath
        else
          let arm := if arm.vx.abs + arm.vy.abs < 2 then
              let (dx, dy) := arm.playerVector player
              let (vx, vy) := normalized dx dy 12
              { arm with vx, vy }
            else { arm with vx := arm.vx * 0.97, vy := arm.vy * 0.97 }
          { arm.countPhase 600 0 with direction := arm.targetDirection player dead }
      else
        let arm := arm.countPhase 600 (arm.phase + 1)
        let vx := if arm.centerX > head.centerX + 250 then forceNegative arm.vx 0.94 0.3 9
          else if arm.centerX < head.centerX then forcePositive arm.vx 0.94 0.2 8
          else arm.vx
        { arm with vx, vy := steerBand arm.y arm.vy (head.y + 230) (head.y + 300) 0.96 0.1 3 }
    { arm with rotation := link }
  | 1 =>
    let rotation := arm.linkRotation head 280
    if arm.y < head.y - 280 then
      let (dx, dy) := arm.playerVector player
      let (vx, vy) := normalized dx dy 20
      { arm with phase := 2, rotation, vx, vy, direction := arm.targetDirection player dead }
    else
      { arm with
        rotation
        vx := (arm.vx * 5 + head.vx) / 6 + 0.5
        vy := max (-9) (dampPositive arm.vy 0.9 - 0.5) }
  | 2 =>
    if arm.y > pixels player.y || arm.vy < 0 then
      if arm.timer ≥ 4 then { arm with phase := 3, timer := 0 }
      else { arm with phase := 1, timer := arm.timer + 1 }
    else arm
  | 4 =>
    if arm.centerX < head.centerX - 500 || arm.centerX > head.centerX + 500 then
      let (dx, dy) := arm.playerVector player
      let (vx, vy) := normalized dx dy 17
      { arm with phase := 5, vx, vy, rotation := link, direction := arm.targetDirection player dead }
    else
      { arm with
        rotation := link
        vx := min 12 (arm.vx + 0.5)
        vy := (arm.vy * 5 + head.vy) / 6 }
  | 5 =>
    if arm.centerX < pixels (bodyCenterX player) - 100 then
      if arm.timer ≥ 4 then { arm with phase := 0, timer := 0 }
      else { arm with phase := 4, timer := arm.timer + 1 }
    else arm
  | _ => arm

private def PrimeArm.fire (arm : PrimeArm) (kind : PrimeAttackKind) (dx dy speed spread offset : Float32)
    (damage limit : Nat) (roll : Nat → Nat) : PrimeArm × Option PrimeAttack :=
  if arm.shotClock < limit then ({ arm with shotClock := arm.shotClock + 1 }, none)
  else
    let sample (draw : Nat) : Float32 := single ((roll draw % 81 : Nat) - (40 : Int))
    let (dx, dy) := normalized dx dy speed
    let vx := dx + sample 0 * spread
    let vy := dy + sample 1 * spread
    ({ arm with shotClock := 0 },
      some ⟨kind, arm.centerX + vx * offset, arm.centerY + vy * offset, vx, vy, damage⟩)

private def PrimeArm.aggressiveSupport (arm : PrimeArm) (head : PrimeHead) (bonus : Nat) : PrimeArm :=
  let x := head.centerX - 120 * single arm.kind.side
  let y := head.y - 100
  { arm with
    vx := steerBand arm.centerX arm.vx x x 0.96 0.1 8
    vy := steerBand arm.y arm.vy y y 0.96 0.07 6
    shotClock := arm.shotClock + bonus }

private def PrimeArm.advanceCannon (arm : PrimeArm) (head : PrimeHead) (player : Player) (dead : Bool)
    (roll : Nat → Nat) : PrimeArm × Option PrimeAttack :=
  match arm.phase with
  | 0 =>
    let arm := if head.phase != .hover then arm.aggressiveSupport head 2
      else
        let arm := if arm.timer + 1 ≥ 1100 then { arm with phase := 1, timer := 0, shotClock := 0 }
          else { arm with timer := arm.timer + 1 }
        let y := head.y - 150
        { arm with
          vx := steerBand arm.centerX arm.vx (head.centerX + 160) (head.centerX + 200) 0.96 0.2 8
          vy := steerBand arm.y arm.vy y y 0.96 0.04 3 }
    let (dx, dy) := arm.linkVector head
    let arm := { arm with rotation := angle dx dy + 1.57 }
    arm.fire .bomb (0 - dx) (0 - dy) 12 0.01 4 0 140 roll
  | 1 =>
    let arm := if arm.timer + 1 ≥ 300 then { arm with phase := 0, timer := 0, shotClock := 0 }
      else { arm with timer := arm.timer + 1 }
    let wanted := normalized (head.centerX - arm.centerX) (pixels (bodyCenterY player) - 80 - arm.centerY) 6
    let arm := arm.chase wanted 0.9 0.04 0.08
    let (dx, dy) := arm.playerVector player
    let arm := { arm with rotation := angle dx dy - 1.57, direction := arm.targetDirection player dead }
    arm.fire .bomb dx dy 10 0.01 4 0 40 roll
  | _ => (arm, none)

private def PrimeArm.advanceLaser (arm : PrimeArm) (head : PrimeHead) (player : Player) (dead : Bool)
    (roll : Nat → Nat) : PrimeArm × Option PrimeAttack :=
  match arm.phase with
  | 0 | 3 =>
    let arm := if head.phase != .hover then arm.aggressiveSupport head 3
      else
        let arm := arm.countPhase 800 (arm.phase + 1)
        let x := head.centerX - 180 * single arm.kind.side
        let y := head.y - 100
        { arm with
          vx := steerBand arm.centerX arm.vx x x 0.96 0.14 8
          vy := steerBand arm.y arm.vy y y 0.96 0.1 3 }
    let (dx, dy) := arm.playerVector player
    let arm := { arm with rotation := angle dx dy - 1.57, direction := arm.targetDirection player dead }
    arm.fire .laser dx dy 8 0.05 8 25 200 roll
  | 1 =>
    let arm := if arm.timer + 1 ≥ 200 then { arm with phase := 0, timer := 0, shotClock := 0 }
      else { arm with timer := arm.timer + 1 }
    let wanted := normalized (pixels (bodyCenterX player) - 350 - arm.centerX)
      (pixels (bodyCenterY player) - 20 - arm.centerY) 7
    let arm := arm.chase wanted 0.9 0.1 0.03
    let (dx, dy) := arm.playerVector player
    let arm := { arm with rotation := angle dx dy - 1.57, direction := arm.targetDirection player dead }
    arm.fire .laser dx dy 10 0.05 8 25 80 roll
  | _ => (arm, none)

/-- The head is updated before the four arms. An absent head removes all four
arms in single-player, so callers only advance surviving arms with a head. -/
def PrimeArm.advance (arm : PrimeArm) (head : PrimeHead) (player : Player) (dead : Bool)
    (roll : Nat → Nat) : PrimeArm × Option PrimeAttack :=
  let (arm, shot) := match arm.kind with
    | .cannon => arm.advanceCannon head player dead roll
    | .saw => (arm.advanceSaw head player dead, none)
    | .vice => (arm.advanceVice head player dead, none)
    | .laser => arm.advanceLaser head player dead roll
  let vx := if arm.vx.toFloat.abs < 0.005 then 0 else arm.vx
  ({ arm with x := arm.x + vx, y := arm.y + arm.vy, vx }.animate, shot)

end Lean4Terria
