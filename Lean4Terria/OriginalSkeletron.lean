import Lean4Terria.OriginalForestOldManCurse

/-! The native NPC slot representation for the active Classic Skeletron fight. -/
namespace Lean4Terria.OriginalSkeletron

open OriginalCollision (Vector single)
open OriginalForestOldManCurse (Part LocalTarget Effect headType handType)

def guardianType : Nat := 68

def spawnGuardian (slot : Nat) (x y : Int) (activeTime : Nat) : Part :=
  { slot, typeId := guardianType, position := ⟨single (x - 40), single (y - 102)⟩
    width := 80, height := 102, life := 9999, lifeMax := 9999, damage := 9999, defense := 9999
    aiStyle := 11, timeLeft := activeTime * 5 / 4 }

def Part.center (part : Part) : Vector :=
  part.position + ⟨single (part.width / 2), single (part.height / 2)⟩

def headPhase (part : Part) : SkeletronPhase :=
  match part.ai1.toUInt32 with
  | 1 => .spin
  | 2 => .enraged
  | 3 => .retreat
  | _ => .hover

private def headPhaseId : SkeletronPhase → Float32
  | .hover => 0
  | .spin => 1
  | .enraged => 2
  | .retreat => 3

private def handPhase (part : Part) : SkeletronHandPhase :=
  match part.ai2.toUInt32 with
  | 1 => .lift
  | 2 => .slam
  | 3 => .reset
  | 4 => .windup
  | 5 => .swipe
  | _ => .hover

private def handPhaseId : SkeletronHandPhase → Float32
  | .hover => 0
  | .lift => 1
  | .slam => 2
  | .reset => 3
  | .windup => 4
  | .swipe => 5

private def headMotion (part : Part) : SkeletronHead :=
  { x := part.position.x, y := part.position.y, vx := part.velocity.x, vy := part.velocity.y
    rotation := part.rotation, direction := part.direction }

private def fight (head : Part) : Skeletron :=
  { head := headMotion head, health := head.life.toNat, phase := headPhase head
    ticks := head.ai2.toUInt32.toNat, leftHand := none, rightHand := none
    headDefense := head.defense, headDamage := head.damage, timeLeft := head.timeLeft }

private def faceTarget (part : Part) (target : LocalTarget) : Int × Int :=
  let center := Part.center part
  let tx := single (target.position.x.toInt32.toInt + target.width / 2)
  let ty := single (target.position.y.toInt32.toInt + target.height / 2)
  (if tx < center.x then -1 else 1, if ty < center.y then -1 else 1)

private def refreshHeadLife (head : Part) (world : OriginalGreenSlime.World) (target : LocalTarget) : Part :=
  let center := Part.center head
  let w := world.screenWidth / 2 + head.width
  let h := world.screenHeight / 2 + head.height
  let screen : OriginalForestGuideDialogue.Rect :=
    ⟨(center.x.toFloat - (Int.toFloat w)).toInt64.toInt, (center.y.toFloat - (Int.toFloat h)).toInt64.toInt,
      2 * w, 2 * h⟩
  let player : OriginalForestGuideDialogue.Rect :=
    ⟨target.position.x.toInt32.toInt, target.position.y.toInt32.toInt, target.width, target.height⟩
  let timeLeft := (if screen.intersects player then world.activeTime.toNat else head.timeLeft) - 1
  { head with timeLeft, active := timeLeft > 0 }

structure HeadUpdate where
  part : Part
  effects : Array Effect
  roar : Bool

private def moveHead (head : Part) (target : LocalTarget) (enrage playerDead : Bool) : HeadUpdate :=
  let originalPhase := headPhase head
  let far := (head.position.x - target.position.x).abs > 2000 ||
    (head.position.y - target.position.y).abs > 2000
  let targetDirection := faceTarget head target
  let reacquire := far && !playerDead
  let phase := if originalPhase == .retreat || playerDead || far then .retreat
    else if enrage then .enraged else originalPhase
  let boss := fight head
  let motion := if reacquire then { boss.head with direction := targetDirection.1 } else boss.head
  let ai := ({ boss with phase, head := motion } : Skeletron).advanceHeadAI target.motion targetDirection.1
  let next := { ai with head := ai.head.integrate }
  let turn := reacquire || (phase == .hover && next.phase == .spin)
  let advanced := { head with
    position := ⟨next.head.x, next.head.y⟩, velocity := ⟨next.head.vx, next.head.vy⟩
    rotation := next.head.rotation, ai1 := headPhaseId next.phase, ai2 := single next.ticks
    direction := next.head.direction, directionY := if turn then targetDirection.2 else head.directionY
    damage := next.headDamage, defense := next.headDefense, timeLeft := next.timeLeft
    netUpdate := false, immune := head.immune - 1 }
  let effects := if next.phase == .hover || next.phase == .spin then head.blood ⟨ai.head.vx, ai.head.vy⟩ else #[]
  ⟨advanced, effects, phase == .spin && boss.ticks == 0⟩

/-- Style 11 after its one-time hand allocation. `CheckActive` runs after movement. -/
def advanceHead (head : Part) (world : OriginalGreenSlime.World) (target : LocalTarget)
    (playerDead : Bool) : HeadUpdate :=
  let result := moveHead head target world.dayTime playerDead
  { result with part := refreshHeadLife result.part world target }

/-- A Guardian enters style 11 without hands and uses the enraged branch at
all times of day. It still counts as one nearby normal NPC. -/
def advanceGuardian (guardian : Part) (world : OriginalGreenSlime.World)
    (player : OriginalGreenSlime.Player) (target : LocalTarget) : HeadUpdate × Float32 :=
  let born := guardian.ai0 == 0
  let guardian := if born then
    let (direction, directionY) := faceTarget guardian target
    { guardian with ai0 := 1, target := player.index, direction, directionY }
  else guardian
  let result := moveHead guardian target true player.dead
  let part := result.part
  let (active, nearby) := OriginalGreenSlime.checkActive world #[player] {
    position := part.position, width := part.width, height := part.height
    lifeMax := part.lifeMax, timeLeft := part.timeLeft, npcSlots := 1 }
  ({ result with
    part := { part with active := active.active, timeLeft := active.timeLeft.toNat }
    roar := born }, nearby.foldl (fun count (_, slots) => count + slots) 0)

/-- Style 12 reads the head from its assigned slot, which may have already
advanced during this NPC pass. Hands do not use the ordinary timeLeft countdown. -/
def advanceHand (hand : Part) (head : Option Part) (target : LocalTarget) : Part :=
  match head with
  | none => { hand with
      position := hand.position + hand.velocity
      active := false, life := -1, ai2 := 10, netUpdate := false }
  | some head =>
    let previous := handPhase hand
    let nativeHand : SkeletronHand := {
      x := hand.position.x, y := hand.position.y, vx := hand.velocity.x, vy := hand.velocity.y
      rotation := hand.rotation, health := hand.life.toNat, phase := previous, ticks := hand.ai3.toUInt32.toNat }
    let side := -hand.ai0.toInt32.toInt
    let moved := nativeHand.advance (fight head) target.motion side
    let targeted := (previous == .lift && moved.phase == .slam) || (previous == .windup && moved.phase == .swipe)
    let direction := if targeted then faceTarget hand target else (hand.direction, hand.directionY)
    { hand with
      position := ⟨moved.x, moved.y⟩, velocity := ⟨moved.vx, moved.vy⟩, rotation := moved.rotation
      ai2 := handPhaseId moved.phase, ai3 := single moved.ticks
      direction := direction.1, directionY := direction.2, spriteDirection := side
      timeLeft := if headPhase head == .retreat then min hand.timeLeft 10 else hand.timeLeft
      netUpdate := false, immune := hand.immune - 1 }

/-- Type 35 and 36 branch of `NPC.HitEffect`, including the zero-direction
death effect for hands that have lost their head. -/
def hitEffects (part : Part) (direction damage : Int) : Array Effect :=
  if part.life > 0 then
    let count := (damage.toFloat / part.lifeMax.toFloat * 100).ceil.toUInt64.toNat
    Array.replicate count (.dust 26 part.position part.width part.height ⟨single direction, -1⟩ 1)
  else
    let bones := if part.typeId == handType then #[56, 57, 57, 57] else #[54, 55]
    Array.replicate 150 (.dust 26 part.position part.width part.height ⟨2.5 * single direction, -2.5⟩ 1) ++
      (bones.map fun kind => .gore kind part.position part.velocity)

/-- Orphaned hands run their death effect before the last move. -/
def orphanHandEffects (hand : Part) : Array Effect :=
  hitEffects { hand with life := -1 } 0 10

end Lean4Terria.OriginalSkeletron
