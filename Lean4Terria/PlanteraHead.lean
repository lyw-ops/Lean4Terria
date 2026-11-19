import Lean4Terria.DarkCaster
import Lean4Terria.PlanteraShots

namespace Lean4Terria

def planteraHeadWidth : Nat := 86
def planteraHeadHeight : Nat := 86
def planteraHeadHealth : Nat := 30000

/-- NPC 262 uses top-left positions in native pixels. `stage` records which
attached NPCs have already spawned. The signed AI clock also holds the
recovery intervals after poison seeds and thorn balls. -/
structure PlanteraHead where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  health : Nat := planteraHeadHealth
  attackClock : Int := 0
  stage : Nat := 0
  timeLeft : Nat := 750
  defense : Nat := 14
  contactDamage : Nat := 50
  enraged : Bool := false
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq PlanteraHead where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.health == b.health && a.attackClock == b.attackClock && a.stage == b.stage &&
    a.timeLeft == b.timeLeft && a.defense == b.defense && a.contactDamage == b.contactDamage &&
    a.enraged == b.enraged && a.frame == b.frame && a.frameClock == b.frameClock

structure PlanteraSporeLaunch where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  deriving Repr, Inhabited

structure PlanteraHeadStep where
  head : PlanteraHead
  spawnHooks : Bool := false
  spawnTentacles : Bool := false
  projectile : Option PlanteraAttack := none
  spore : Option PlanteraSporeLaunch := none
  deriving Repr, Inhabited

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (units : Int) : Float32 := single units / 16
private def length (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32
private def truncated (v : Float32) : Float32 := single v.toInt32.toInt

structure PlanteraTarget where
  x : Float32
  y : Float32
  width : Nat
  height : Nat
  deriving Repr, Inhabited

def PlanteraTarget.ofPlayer (player : Player) : PlanteraTarget :=
  ⟨pixels player.x, pixels player.y, playerWidth.toNat / 16, playerHeight.toNat / 16⟩

def PlanteraTarget.centerX (target : PlanteraTarget) : Float32 := target.x + single target.width * 0.5
def PlanteraTarget.centerY (target : PlanteraTarget) : Float32 := target.y + single target.height * 0.5

def PlanteraHead.centerX (head : PlanteraHead) : Float32 := head.x + 43
def PlanteraHead.centerY (head : PlanteraHead) : Float32 := head.y + 43

def PlanteraHead.atCenter (x y : Float32) : PlanteraHead :=
  { x := x - 43, y := y - 43 }

def PlanteraHead.hitBox (head : PlanteraHead) : HitBox :=
  CasterAI.npcBox head.x head.y planteraHeadWidth planteraHeadHeight

def PlanteraHead.contact (head : PlanteraHead) (player : Player) : Option Int :=
  CasterAI.contact head.hitBox head.centerX player

def PlanteraHead.secondForm (head : PlanteraHead) : Bool :=
  head.health ≤ planteraHeadHealth / 2

def PlanteraHead.valid (head : PlanteraHead) (world : World) : Bool :=
  head.x.isFinite && head.y.isFinite && head.vx.isFinite && head.vy.isFinite && head.rotation.isFinite &&
    -32768 ≤ head.x && head.x ≤ single (world.width * 16 + 32768 : Nat) &&
    -32768 ≤ head.y && head.y ≤ single (world.height * 16 + 32768 : Nat) &&
    head.vx.abs ≤ 32 && head.vy.abs ≤ 32 &&
    0 < head.health && head.health ≤ planteraHeadHealth &&
    -120 ≤ head.attackClock && head.attackClock < 350 && head.stage ≤ 2 &&
    0 < head.timeLeft && head.timeLeft ≤ 18740 && head.frame ≤ 7 && head.frameClock < 7 &&
    [(14, 50), (36, 50), (72, 100), (10, 70), (40, 140)].contains (head.defense, head.contactDamage)

private def steer (speed wanted acceleration : Float32) : Float32 :=
  if speed < wanted then
    let next := speed + acceleration
    if next < 0 && wanted > 0 then next + acceleration * 2 else next
  else if speed > wanted then
    let next := speed - acceleration
    if next > 0 && wanted < 0 then next - acceleration * 2 else next
  else speed

private def PlanteraHead.hookMean (head : PlanteraHead) (anchors : Array (Float32 × Float32)) :
    Float32 × Float32 :=
  let spawnX := truncated head.centerX
  let spawnY := truncated head.centerY - 20
  if head.stage == 0 then ((spawnX + spawnX + spawnX) / 3, (spawnY + spawnY + spawnY) / 3)
  else if anchors.isEmpty then (head.centerX, head.centerY)
  else
    let used := anchors.extract 0 3
    let (x, y) := used.foldl (fun (x, y) (cx, cy) => (x + cx, y + cy)) (0, 0)
    let count := single used.size
    (x / count, y / count)

private def healthSpeed (head : PlanteraHead) : Float32 :=
  if head.health < 7500 then 7 else if head.health < 15000 then 5 else 2.5

private def PlanteraHead.steerToward (head : PlanteraHead) (target : PlanteraTarget)
    (anchors : Array (Float32 × Float32)) (jungle dead : Bool) : PlanteraHead :=
  let (anchorX, anchorY) := head.hookMean anchors
  let enrage := !jungle || dead
  let timeLeft := if !dead && head.timeLeft < 750 &&
      length (head.centerX - target.centerX) (head.centerY - target.centerY) < 4800 then 750
    else head.timeLeft
  let speed := healthSpeed head + (if jungle then 0 else 8) + (if dead then 8 else 0)
  let acceleration : Float32 := if !jungle then 0.15 else if head.health < 15000 then 0.05 else 0.025
  let dx := target.centerX - anchorX
  let dy := target.centerY - anchorY
  let (dx, dy) := if dead then (-dx, -dy) else (dx, dy)
  let limit : Float32 := if enrage then 850 else 500
  let distance := length dx dy
  let (dx, dy) := if distance ≥ limit then
      let scale := limit / distance
      (dx * scale, dy * scale)
    else (dx, dy)
  let wantedX := (anchorX + dx) - head.centerX
  let wantedY := (anchorY + dy) - head.centerY
  let distance := length wantedX wantedY
  let (wantedX, wantedY) := if distance < speed then (head.vx, head.vy) else
    let scale := speed / distance
    (wantedX * scale, wantedY * scale)
  let rotation := (Float.atan2 (target.centerY - head.centerY).toFloat
    (target.centerX - head.centerX).toFloat).toFloat32 + 1.57
  { head with
    vx := steer head.vx wantedX acceleration
    vy := steer head.vy wantedY acceleration
    timeLeft
    rotation
    enraged := enrage }

private def PlanteraHead.seedClock (head : PlanteraHead) (enraged : Bool) : Int :=
  head.attackClock + 1 + (if head.health < 27000 then 1 else 0) +
    (if head.health < 24000 then 1 else 0) + (if head.health < 21000 then 1 else 0) +
    (if head.health < 18000 then 1 else 0) + (if enraged then 3 else 0)

def PlanteraHead.needsLineOfSight (head : PlanteraHead) (jungle dead : Bool) : Bool :=
  !head.secondForm && head.seedClock (!jungle || dead) > 80

private def PlanteraHead.fireSeed (head : PlanteraHead) (target : PlanteraTarget)
    (visible : Bool) (roll : Nat → Nat) : PlanteraHead × Option PlanteraAttack := Id.run do
  let clock := head.seedClock head.enraged
  if clock ≤ 80 then return ({ head with attackClock := clock }, none)
  if !visible then return ({ head with attackClock := 0 }, none)
  let (kind, damage, clock) : PlanteraShotKind × Nat × Int :=
    if head.health ≥ 24000 then (.seed, 22, 0)
    else if roll 0 % 4 == 0 then (.poisonSeed, 27, -30)
    else if roll 1 % 8 == 0 then (.thornBall, 31, -120)
    else (.seed, 22, 0)
  let dx := target.centerX - head.centerX
  let dy := target.centerY - head.centerY
  let distance := length dx dy
  let head := { head with attackClock := clock }
  if distance == 0 then return (head, none)
  let scale := 15 / distance
  let vx := dx * scale
  let vy := dy * scale
  let damage := if head.enraged then damage * 2 else damage
  return (head, some ⟨kind, head.centerX + vx * 3, head.centerY + vy * 3, vx, vy, damage⟩)

private def PlanteraHead.fireSpore (head : PlanteraHead) (target : PlanteraTarget) (roll : Nat → Nat) :
    PlanteraHead × Option PlanteraSporeLaunch := Id.run do
  let clock := head.attackClock + 1 + (if head.health < 12000 then 1 else 0) +
    (if head.health < 9000 then 1 else 0) + (if head.health < 6000 then 1 else 0) +
    (if head.health < 3000 then 1 else 0)
  if clock < 350 then return ({ head with attackClock := clock }, none)
  let noise (draw : Nat) : Float32 := single ((roll draw % 21 : Nat) - (10 : Int))
  let dx := target.centerX - head.centerX + noise 0
  let dy := target.centerY - head.centerY + noise 1
  let dy := dy - if dy > 0 then 0 else (dx * 0.2).abs
  let distance := length dx dy
  let head := { head with attackClock := 0 }
  if distance == 0 then return (head, none)
  let scale := 8 / distance
  return (head, some ⟨truncated head.centerX, truncated head.centerY, dx * scale, dy * scale⟩)

/-- Ordinary single-player NPC.AI style 51 from 1.4.5.8. The supplied anchors
are the first three active hook centers in update order. The caller creates
new NPCs from the pre-movement head position. RNG inputs are attack samples
after cosmetic effects. -/
def PlanteraHead.advanceAI (head : PlanteraHead) (target : PlanteraTarget)
    (anchors : Array (Float32 × Float32)) (jungle dead visible : Bool) (roll : Nat → Nat) :
    PlanteraHeadStep :=
  let spawnHooks := head.stage == 0
  let head := head.steerToward target anchors jungle dead
  let head := { head with stage := max 1 head.stage }
  if head.secondForm then
    let spawnTentacles := head.stage == 1
    let head := { head with
      stage := 2
      defense := if head.enraged then 40 else 10
      contactDamage := if head.enraged then 140 else 70 }
    let (head, spore) := head.fireSpore target roll
    { head, spawnHooks, spawnTentacles, spore }
  else
    let head := { head with
      defense := if head.enraged then 72 else 36
      contactDamage := if head.enraged then 100 else 50 }
    let (head, projectile) := head.fireSeed target visible roll
    { head, spawnHooks, projectile }

private def PlanteraHead.advanceFrame (head : PlanteraHead) : PlanteraHead :=
  let frameClock := head.frameClock + 1
  let frame := head.frame + if frameClock > 6 then 1 else 0
  let frame := if head.secondForm then if frame < 4 || frame > 7 then 4 else frame
    else if frame > 3 then 0 else frame
  { head with frame, frameClock := if frameClock > 6 then 0 else frameClock }

private def PlanteraHead.checkActive (head : PlanteraHead) (target : PlanteraTarget) : Nat :=
  let halfWidth := 960 + planteraHeadWidth
  let halfHeight := 600 + planteraHeadHeight
  let left := (head.centerX.toFloat - halfWidth.toFloat).toInt32.toInt * 16
  let top := (head.centerY.toFloat - halfHeight.toFloat).toInt32.toInt * 16
  let area : HitBox := ⟨left, top, left + 32 * halfWidth, top + 32 * halfHeight⟩
  let nearby := area.intersects (CasterAI.npcBox target.x target.y target.width target.height)
  (if nearby then 750 else head.timeLeft) - 1

/-- A complete head update through no-tile motion, animation, and inactivity.
Elapsed despawn time is measured by the same counter as the original NPC. -/
def PlanteraHead.advance (head : PlanteraHead) (world : World) (player : Player)
    (anchors : Array (Float32 × Float32)) (jungle dead visible : Bool) (roll : Nat → Nat) :
    Option PlanteraHeadStep := do
  if head.health == 0 then failure
  let target := PlanteraTarget.ofPlayer player
  let result := head.advanceAI target anchors jungle dead visible roll
  let head := result.head
  let vx := if head.vx.toFloat.abs < 0.005 then 0 else head.vx
  let moved := { head with x := head.x + vx, y := head.y + head.vy, vx }.advanceFrame
  let moved := { moved with timeLeft := moved.checkActive target }
  if moved.valid world then some { result with head := moved } else none

/-- Scale SpawnOnPlayer's ground search to the compact map. A nearby clear
edge of the safe area is also usable where the native search area is clipped. -/
def PlanteraHead.spawn (world : World) (player : Player) (roll : Nat → Nat) : PlanteraHead := Id.run do
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
        return { PlanteraHead.atCenter (single (x * 16 + 8)) (single (floor * 16 : Nat) - 43) with
          timeLeft := 18740 }
  let side : Int := if center.x < world.width / 2 then 32 else -32
  let x := max 4 (min ((world.width : Int) - 5) (center.x + side))
  let y := max 9 (center.y - 19)
  return { PlanteraHead.atCenter (single (x * 16 + 8)) (single (y * 16 + 8)) with
    timeLeft := 18740 }

end Lean4Terria
