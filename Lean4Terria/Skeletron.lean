import Lean4Terria.EyeOfCthulhu

namespace Lean4Terria

private def npcPixel : Int := tileUnits / 16

def skeletronMaxHealth : Nat := 4400
def skeletronHandMaxHealth : Nat := 600
def skeletronTotalMaxHealth : Nat := skeletronMaxHealth + 2 * skeletronHandMaxHealth
def skeletronHeadHalfWidth : Int := 40 * npcPixel
def skeletronHeadHalfHeight : Int := 51 * npcPixel
def skeletronHandHalfSize : Int := 26 * npcPixel
def skeletronHeadMaxSpeed : Int := 64 * npcPixel
private def skeletronHandMaxSpeed : Int := 64 * npcPixel
def skeletronHoverTicks : Nat := 800
def skeletronSpinTicks : Nat := 400
def skeletronRetreatTicks : Nat := 50
def skeletronHandWaitTicks : Nat := 300
def skeletronActiveTime : Nat := 750
def skeletronSpawnTime : Nat := 937

inductive SkeletronPhase where
  | hover | spin | enraged | retreat
  deriving BEq, Repr, Inhabited

inductive SkeletronHandPhase where
  | hover | lift | slam | reset | windup | swipe
  deriving BEq, Repr, Inhabited

inductive SkeletronPart where
  | head | leftHand | rightHand
  deriving BEq, Repr, Inhabited

structure SkeletronHand where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  rotation : Float32 := 0
  health : Nat
  phase : SkeletronHandPhase
  /-- Only the two tracking phases count up to the next windup. -/
  ticks : Nat
  deriving Repr, Inhabited

instance : BEq SkeletronHand where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.health == b.health && a.phase == b.phase && a.ticks == b.ticks

/-- The vanilla head keeps its top-left position, velocity and rotation in binary32. -/
structure SkeletronHead where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  rotation : Float32 := 0
  direction : Int := 1
  deriving Repr, Inhabited

instance : BEq SkeletronHead where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.direction == b.direction

private def pixels (units : Int) : Float32 :=
  (Int32.ofInt units).toFloat32 / 16

private def subpixels (value : Float32) : Int :=
  (value * 16).toInt32.toInt

def SkeletronHead.ofMotion (motion : EyeMotion) (direction : Int := 1) : SkeletronHead :=
  { x := pixels (motion.x - skeletronHeadHalfWidth), y := pixels (motion.y - skeletronHeadHalfHeight)
    vx := pixels motion.vx, vy := pixels motion.vy, rotation := pixels motion.vx / 15, direction }

private def SkeletronHead.centerX (head : SkeletronHead) : Float32 := head.x + 40
private def SkeletronHead.centerY (head : SkeletronHead) : Float32 := head.y + 51

def SkeletronHand.centerX (hand : SkeletronHand) : Float32 := hand.x + 26
def SkeletronHand.centerY (hand : SkeletronHand) : Float32 := hand.y + 26

private def SkeletronHand.aimRotation (hand : SkeletronHand) (head : SkeletronHead)
    (side : Float32) : Float32 :=
  let dx := head.centerX - 200 * (-side) - hand.centerX
  let dy := head.y + 230 - hand.centerY
  (Float.atan2 dy.toFloat dx.toFloat).toFloat32 + 1.57

def SkeletronHand.ofMotion (motion : EyeMotion) (health : Nat) (phase : SkeletronHandPhase)
    (ticks : Nat) (head : SkeletronHead) (side : Int) : SkeletronHand :=
  let hand : SkeletronHand :=
    { x := pixels (motion.x - skeletronHandHalfSize), y := pixels (motion.y - skeletronHandHalfSize)
      vx := pixels motion.vx, vy := pixels motion.vy, health, phase, ticks }
  { hand with rotation := hand.aimRotation head (Int32.ofInt side).toFloat32 }

def SkeletronHand.motion (hand : SkeletronHand) : EyeMotion :=
  ⟨subpixels hand.centerX, subpixels hand.centerY, subpixels hand.vx, subpixels hand.vy⟩

private def SkeletronHead.face (head : SkeletronHead) (player : Player) : Int :=
  let left := (pixels player.x).toInt32.toInt
  let halfWidth := (pixels playerWidth).toInt32.toInt / 2
  if (Int32.ofInt (left + halfWidth)).toFloat32 < head.centerX then -1 else 1

structure Skeletron where
  head : SkeletronHead
  health : Nat
  phase : SkeletronPhase
  ticks : Nat
  leftHand : Option SkeletronHand
  rightHand : Option SkeletronHand
  headDefense : Nat := 10
  headDamage : Nat := 32
  timeLeft : Nat := skeletronSpawnTime
  deriving BEq, Repr, Inhabited

def Skeletron.motion (boss : Skeletron) : EyeMotion :=
  ⟨subpixels boss.head.centerX, subpixels boss.head.centerY,
    subpixels boss.head.vx, subpixels boss.head.vy⟩

private def npcBox (x y : Float32) (halfWidth halfHeight : Int) : HitBox :=
  let left := x.toInt32.toInt * npcPixel
  let top := y.toInt32.toInt * npcPixel
  ⟨left, top, left + 2 * halfWidth, top + 2 * halfHeight⟩

def Skeletron.hitBox (boss : Skeletron) : HitBox :=
  npcBox boss.head.x boss.head.y skeletronHeadHalfWidth skeletronHeadHalfHeight

def SkeletronHand.hitBox (hand : SkeletronHand) : HitBox :=
  npcBox hand.x hand.y skeletronHandHalfSize skeletronHandHalfSize

def Skeletron.targets (boss : Skeletron) : Array (SkeletronPart × HitBox) :=
  if boss.health == 0 then #[] else
    #[(.head, boss.hitBox)] ++
      (boss.rightHand.map fun hand => (SkeletronPart.rightHand, hand.hitBox)).toArray ++
      (boss.leftHand.map fun hand => (SkeletronPart.leftHand, hand.hitBox)).toArray

def Skeletron.totalHealth (boss : Skeletron) : Nat :=
  boss.health + (boss.leftHand.map (·.health)).getD 0 + (boss.rightHand.map (·.health)).getD 0

def Skeletron.defense (boss : Skeletron) : SkeletronPart → Nat
  | .head => boss.headDefense
  | .leftHand | .rightHand => 14

/-- NPC slots are visited in spawn order. The first hit grants General immunity. -/
def Skeletron.contact (boss : Skeletron) (player : Player) : Option (Nat × Int) := do
  let playerBox := HitBox.ofNpcTarget player
  let (part, _) ← boss.targets.find? (fun (_, box) => box.intersects playerBox)
  let center := match part with
    | .head => boss.head.centerX
    | .rightHand => (boss.rightHand.map (·.centerX)).getD boss.head.centerX
    | .leftHand => (boss.leftHand.map (·.centerX)).getD boss.head.centerX
  let damage := if part == .head then boss.headDamage else 20
  return (damage, if center < pixels (bodyCenterX player) then 1 else -1)

def Skeletron.takeHit (boss : Skeletron) (part : SkeletronPart) (raw : Nat) : Skeletron :=
  if boss.health == 0 || raw == 0 then boss else
  let damage := damageAfterDefense raw (boss.defense part / 2)
  let hitHand := fun (hand : SkeletronHand) =>
    if hand.health ≤ damage then none else some { hand with health := hand.health - damage }
  match part with
  | .head =>
    let health := boss.health - damage
    if health > 0 then { boss with health }
    else { boss with health := 0, leftHand := none, rightHand := none }
  | .leftHand => { boss with leftHand := boss.leftHand.bind hitHand }
  | .rightHand => { boss with rightHand := boss.rightHand.bind hitHand }

private def skeletronMotionValid (motion : EyeMotion) (world : World) (speed : Int) : Bool :=
  -- NPCs may leave the tile map while the inactivity timer counts down.
  let margin := 4096 * npcPixel
  (-margin ≤ motion.x) && motion.x < world.width * tileUnits + margin &&
    (-margin ≤ motion.y) && motion.y < world.height * tileUnits + margin &&
    motion.vx.natAbs ≤ speed.toNat && motion.vy.natAbs ≤ speed.toNat

private def SkeletronHand.valid (hand : SkeletronHand) (world : World) : Bool :=
  0 < hand.health && hand.health ≤ skeletronHandMaxHealth &&
    hand.x.isFinite && hand.y.isFinite && hand.vx.isFinite && hand.vy.isFinite && hand.rotation.isFinite &&
    skeletronMotionValid hand.motion world skeletronHandMaxSpeed &&
    (match hand.phase with
    | .hover | .reset => hand.ticks < skeletronHandWaitTicks
    | _ => hand.ticks == 0)

def Skeletron.valid (boss : Skeletron) (world : World) : Bool :=
  let bounds : Bool × Int := match boss.phase with
    | .hover => (boss.ticks < skeletronHoverTicks &&
        (boss.headDefense == 10 || (boss.ticks == 0 && boss.headDefense == 0)) && boss.headDamage == 32,
        skeletronHeadMaxSpeed)
    | .spin => (boss.ticks < skeletronSpinTicks && boss.headDamage == 32 &&
        boss.headDefense == (if boss.ticks == 0 then 10 else 0),
        if boss.ticks == 0 then skeletronHeadMaxSpeed else 24)
    | .enraged => (boss.ticks < skeletronHoverTicks && boss.headDefense == 9999 && boss.headDamage == 9999,
        8 * npcPixel)
    | .retreat => (boss.ticks < skeletronHoverTicks && boss.headDefense == 10 &&
        (boss.headDamage == 32 || boss.headDamage == 9999), skeletronHeadMaxSpeed)
  let head := boss.head
  0 < boss.health && boss.health ≤ skeletronMaxHealth && bounds.1 &&
    0 < boss.timeLeft && boss.timeLeft ≤ skeletronSpawnTime &&
    head.x.isFinite && head.y.isFinite && head.vx.isFinite && head.vy.isFinite && head.rotation.isFinite &&
    (head.direction == -1 || head.direction == 1) &&
    skeletronMotionValid boss.motion world bounds.2 &&
    boss.leftHand.all (·.valid world) && boss.rightHand.all (·.valid world)

def SkeletronHead.spawnHand (head : SkeletronHead) (ticks : Nat) : SkeletronHand :=
  -- NewNPC takes the bottom point, 51 pixels below the truncated head top.
  { x := head.centerX.toInt32.toFloat32 - 26, y := head.y.toInt32.toFloat32 - 1
    vx := 0, vy := 0, health := skeletronHandMaxHealth, phase := .hover, ticks }

/-- The Old Man's center becomes the new head's bottom. The hand attack clocks
start half a tracking cycle apart, as they do in the desktop encounter. -/
def spawnSkeletron (oldMan : TilePos) (target : Option Player := none) : Skeletron :=
  let motion : EyeMotion := ⟨oldMan.x * tileUnits + tileUnits / 2,
    oldMan.y * tileUnits + tileUnits / 2 - skeletronHeadHalfHeight, 0, 0⟩
  let head := SkeletronHead.ofMotion motion
  let head := { head with direction := (target.map (head.face ·)).getD 1 }
  { head, health := skeletronMaxHealth, phase := .hover, ticks := 0,
    leftHand := some (head.spawnHand 150), rightHand := some (head.spawnHand 0) }

-- Classic tracking damps velocity when it points away, then limits the
-- remaining backwards momentum. Forward motion may carry past the target.
private def trackVelocity (velocity position target acceleration limit drag : Float32) : Float32 :=
  if position > target then
    let velocity := (if velocity > 0 then velocity * drag else velocity) - acceleration
    if velocity > limit then limit else velocity
  else if position < target then
    let velocity := (if velocity < 0 then velocity * drag else velocity) + acceleration
    if velocity < -limit then -limit else velocity
  else velocity

private def npcHorizontalVelocity (vx : Float32) : Float32 :=
  if vx.toFloat > -0.005 && vx.toFloat < 0.005 then 0 else vx

def SkeletronHead.integrate (head : SkeletronHead) : SkeletronHead :=
  -- NPC.UpdateNPC clears near-zero horizontal speed after AI, before integration.
  let vx := npcHorizontalVelocity head.vx
  { head with x := head.x + vx, y := head.y + head.vy, vx }

/-- Styles 11 and 12 use both the player's floating-point top-left and center. -/
structure SkeletronHandTarget where
  x : Float32
  y : Float32
  centerX : Float32
  centerY : Float32

private def SkeletronHandTarget.ofPlayer (player : Player) : SkeletronHandTarget :=
  ⟨pixels player.x, pixels player.y, pixels (bodyCenterX player), pixels (bodyCenterY player)⟩

private def SkeletronHead.chase (head : SkeletronHead) (target : SkeletronHandTarget)
    (speed : Float32) : SkeletronHead :=
  let dx := target.centerX - head.centerX
  let dy := target.centerY - head.centerY
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let scale := speed / (if length == 0 then 1 else length)
  let rotation := head.rotation + (Int32.ofInt head.direction).toFloat32 * 0.3
  { head with vx := dx * scale, vy := dy * scale, rotation }

/-- Classic single-player head branch of the 1.4.5.8 NPC AI style 11.
The phase flag changes before the old branch finishes its frame. -/
def Skeletron.advanceHeadAI (boss : Skeletron) (target : SkeletronHandTarget)
    (directionAtSpinStart : Int) : Skeletron :=
  let head := boss.head
  let ticks := boss.ticks + 1
  match boss.phase with
  | .hover =>
    let targetX := target.centerX
    let targetY := target.y - 250
    let vx := trackVelocity head.vx head.centerX targetX 0.05 8 0.98
    let vy := trackVelocity head.vy head.y targetY 0.02 2 0.98
    let moved := { head with vx, vy, rotation := head.vx / 15 }
    if ticks ≥ skeletronHoverTicks then
      { boss with head := { moved with direction := directionAtSpinStart }, phase := .spin, ticks := 0, headDefense := 10 }
    else { boss with head := moved, ticks, headDefense := 10 }
  | .spin =>
    let head := head.chase target 1.5
    if ticks ≥ skeletronSpinTicks then { boss with head, phase := .hover, ticks := 0, headDefense := 0 }
    else { boss with head, ticks, headDefense := 0 }
  | .enraged =>
    { boss with head := head.chase target 8, headDefense := 9999, headDamage := 9999 }
  | .retreat =>
    let vy := head.vy + 0.1
    let vy := if vy < 0 then vy * 0.95 else vy
    { boss with
      head := { head with vx := head.vx * 0.95, vy }
      headDefense := 10
      timeLeft := min boss.timeLeft skeletronRetreatTicks }

def Skeletron.advanceHead (boss : Skeletron) (player : Player) : Skeletron :=
  let next := boss.advanceHeadAI (.ofPlayer player) (boss.head.face player)
  { next with head := next.head.integrate }

private def vectorLength (x y : Float32) : Float32 :=
  (x * x + y * y).toFloat.sqrt.toFloat32

private def SkeletronHand.launch (hand : SkeletronHand) (target : SkeletronHandTarget) (speed : Float32)
    (phase : SkeletronHandPhase) : SkeletronHand :=
  let dx := target.centerX - hand.centerX
  let dy := target.centerY - hand.centerY
  let scale := speed / max 0.01 (vectorLength dx dy)
  { hand with vx := dx * scale, vy := dy * scale, phase }

private def SkeletronHand.passedTarget (hand : SkeletronHand) (target : SkeletronHandTarget) : Bool :=
  let dx := target.centerX - hand.centerX
  let dy := target.centerY - hand.centerY
  let distance := vectorLength (target.x - hand.centerX) (target.y - hand.centerY)
  hand.vx * dx + hand.vy * dy ≤ 0 || distance > 2000

/-- Desktop 1.4.5.8 Classic AI style 12. The hand observes the head's updated state,
and the hand's phase changes before its velocity is applied to its position. -/
def SkeletronHand.advance (hand : SkeletronHand) (boss : Skeletron)
    (target : SkeletronHandTarget) (screenSide : Int) : SkeletronHand :=
  let head := boss.head
  let side := (Int32.ofInt screenSide).toFloat32
  let next : SkeletronHand := match hand.phase with
    | .hover | .reset =>
      let hovering := boss.phase == SkeletronPhase.hover
      let targetX := head.centerX - (if hovering then 200 else 120) * (-side)
      let targetY := if hovering then head.y + 230 else head.y - 100
      let vx := trackVelocity hand.vx hand.centerX targetX (if hovering then 0.07 else 0.1) 8 0.96
      let vy := trackVelocity hand.vy hand.y targetY (if hovering then 0.04 else 0.07)
        (if hovering then 3 else 6) 0.96
      let ticks := hand.ticks + if hovering then 1 else 0
      let next := { hand with vx, vy, ticks, rotation := hand.aimRotation head side }
      if ticks < skeletronHandWaitTicks then next
      else { next with ticks := 0, phase := if hand.phase == SkeletronHandPhase.hover then .lift else .windup }
    | .lift =>
      let next := { hand with
        vx := hand.vx * 0.95
        vy := max (-8) (hand.vy - 0.1)
        rotation := hand.aimRotation head side }
      if hand.y < head.y - 200 then next.launch target 18 .slam else next
    | .slam =>
      if hand.y > target.y || hand.passedTarget target || hand.vy < 0 then
        { hand with phase := .reset }
      else hand
    | .windup =>
      let next := { hand with
        vx := max (-8) (min 8 (hand.vx + 0.1 * side))
        vy := hand.vy * 0.95
        rotation := hand.aimRotation head side }
      if hand.centerX < head.centerX - 500 || hand.centerX > head.centerX + 500 then
        next.launch target 17 .swipe
      else next
    | .swipe =>
      if (hand.vx > 0 && hand.centerX > target.centerX) || (hand.vx < 0 && hand.centerX < target.centerX) ||
          hand.passedTarget target then { hand with phase := .hover }
      else hand
  let vx := npcHorizontalVelocity next.vx
  { next with x := hand.x + vx, y := hand.y + next.vy, vx }

/-- CheckActive refreshes the countdown for a nearby active player, including
one who is dead. The rectangle is constructed after the head has moved. -/
private def SkeletronHead.checkActive (head : SkeletronHead) (player : Player) (timeLeft : Nat) : Nat :=
  let left := (head.centerX.toFloat - 1040).toInt32.toInt * npcPixel
  let top := (head.centerY.toFloat - 702).toInt32.toInt * npcPixel
  let view : HitBox := ⟨left, top, left + 2080 * npcPixel, top + 1404 * npcPixel⟩
  (if view.intersects (HitBox.ofNpcTarget player) then skeletronActiveTime else timeLeft) - 1

/-- The head and hands fly through blocks. Dawn enrage persists until the
encounter ends, and the head withdraws after losing its last nearby target. -/
def advanceSkeletron (player : Player) (boss : Skeletron)
    (daylight playerDead : Bool) : Option Skeletron := do
  if boss.health == 0 then failure
  let far := (boss.head.x - pixels player.x).abs > 2000 ||
    (boss.head.y - pixels player.y).abs > 2000
  let boss := if far && !playerDead then
      { boss with head := { boss.head with direction := boss.head.face player } }
    else boss
  let boss := if boss.phase == .retreat then boss
    else if playerDead || far then { boss with phase := .retreat }
    else if daylight then { boss with phase := .enraged }
    else boss
  let advanced := boss.advanceHead player
  let timeLeft := advanced.head.checkActive player advanced.timeLeft
  if timeLeft == 0 then failure
  let target := SkeletronHandTarget.ofPlayer player
  return { advanced with
    timeLeft
    leftHand := boss.leftHand.map (fun hand => hand.advance advanced target (-1))
    rightHand := boss.rightHand.map (fun hand => hand.advance advanced target 1) }

end Lean4Terria
