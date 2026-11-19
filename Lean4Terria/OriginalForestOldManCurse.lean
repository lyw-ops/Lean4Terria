import Lean4Terria.OriginalForestGuideDialogue
import Lean4Terria.Skeletron

/-!
Terraria desktop 1.4.5.8, first Old Man Curse in a Classic single-player
world. The button runs in the interface draw pass. The head creates the
hands during its first turn in the following NPC phase.
-/
namespace Lean4Terria.OriginalForestOldManCurse

open OriginalCollision (Vector single)
open OriginalForestGuideDialogue (Input State Result Rng Line)

def oldManType : Nat := 37
def headType : Nat := 35
def handType : Nat := 36

structure Time where
  dayTime : Bool
  remixWorld : Bool := false
  deriving Repr

def Time.isDay (time : Time) : Bool := time.dayTime && !time.remixWorld

structure PlayerStats where
  /-- Base maximum life (`statLifeMax`). -/
  lifeMax : Int
  /-- Current total defense (`statDefense`). -/
  defense : Int
  deriving Repr

/-- `NPC.GetChat` for type 37. The stats choose the dialogue pool. -/
def getChat (time : Time) (stats : PlayerStats) (random : Rng) : Line × Rng :=
  let (first, count) :=
    if time.isDay then (82, 3)
    else if stats.lifeMax < 300 || stats.defense < 10 then (85, 4)
    else (89, 4)
  let (roll, random) := random.next count
  (.legacy (first + roll), random)

/-- Reuses the ordinary right-click hitbox, reach and release tests. The
Guide dialogue code stops at NPC-specific dialogue for an eligible Old Man. -/
def hover (input : Input) (player : OriginalForestGuideDialogue.Player) (npcs : Array OriginalForestGuideDialogue.Npc)
    (time : Time) (stats : PlayerStats) (random : Rng) (state : State)
    (guideWorld : OriginalForestGuideDialogue.ChatWorld := { dayTime := time.dayTime })
    (otherChat : OriginalForestGuideDialogue.Npc → Rng → Except String (Line × Rng) :=
      fun n _ => .error s!"GetChat for type {n.type}") : Result :=
  let result := OriginalForestGuideDialogue.hover input player npcs guideWorld random state otherChat
  match result.effects.hovering with
  | some slot =>
    if result.unsupported.isSome && npcs.any (fun npc => npc.index == slot && npc.type == oldManType) then
      let (text, rng) := getChat time stats result.rng
      { state := { result.state with talkNPC := slot, text }, rng
        effects := { result.effects with closeWindows := true, sound := some 24, talkedTo := some slot } }
    else result
  | none => result

structure Slot where
  typeId : Nat := 0
  active : Bool := false
  protectedTicks : Nat := 0
  replaceable : Bool := false
  position : Vector := ⟨0, 0⟩
  width : Nat := 0
  height : Nat := 0
  /-- Type 37 uses `ai[3] = 1` to schedule its removal in NPC AI. -/
  ai3 : Float32 := 0
  deriving Repr, Inhabited

/-- `NPCChatPanel.PrepareInteractions` filters the registered Old Man
button through `OldManCurse.Condition`. The input phase owns closing an
inactive or out-of-range conversation. -/
def curseVisible (time : Time) (state : State) (slots : Array Slot) : Bool :=
  !time.isDay && state.talkNPC ≥ 0 && (slots[state.talkNPC.toNat]?.any (·.typeId == oldManType))

/-- Terraria's slot search for type 35 and 36. The second pass can replace
an NPC flagged `CanBeReplacedByOtherNPCs`. -/
private def findSlot (slots : Array Slot) (start : Nat) : Option Nat := Id.run do
  for i in [start:slots.size] do
    let slot := slots[i]!
    if !slot.active && slot.protectedTicks == 0 then return some i
  for i in [start:slots.size] do
    if slots[i]!.replaceable then return some i
  return none

structure Part where
  slot : Nat
  typeId : Nat
  position : Vector
  velocity : Vector := ⟨0, 0⟩
  width : Nat
  height : Nat
  life : Int
  lifeMax : Nat
  damage : Nat
  defense : Nat
  aiStyle : Nat
  target : Nat := 255
  direction : Int := 0
  directionY : Int := 1
  spriteDirection : Int := -1
  rotation : Float32 := 0
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  ai2 : Float32 := 0
  ai3 : Float32 := 0
  timeLeft : Nat
  netUpdate : Bool := true
  active : Bool := true
  immune : Nat := 0
  deriving Repr

structure Spawn where
  part : Part
  /-- The original `NewNPC` bottom point and search start slot. -/
  x : Int
  y : Int
  start : Nat
  deriving Repr

private def spawnPart (typeId slot start : Nat) (x y : Int) (activeTime : Nat) : Spawn :=
  let head := typeId == headType
  let width : Nat := if head then 80 else 52
  let height : Nat := if head then 102 else 52
  let life := if head then skeletronMaxHealth else skeletronHandMaxHealth
  let part : Part := {
    slot, typeId, width, height, life, lifeMax := life
    position := ⟨single x - single width * 0.5, single y - single height⟩
    damage := if head then 32 else 20
    defense := if head then 10 else 14
    aiStyle := if head then 11 else 12
    timeLeft := activeTime * 5 / 4
  }
  { part, x, y, start }

private def Part.occupy (part : Part) : Slot :=
  { typeId := part.typeId, active := true, protectedTicks := 2
    position := part.position, width := part.width, height := part.height, ai3 := part.ai3 }

structure Click where
  /-- Native mouse hit test for the Curse button. -/
  hovered : Bool
  leftDown : Bool
  leftRelease : Bool
  mouseInterfaceAllowed : Bool := true
  deriving Repr

structure ClickResult where
  state : State
  slots : Array Slot
  random : Rng
  /-- The input layer must consume the left-button release on interaction. -/
  consumed : Bool := false
  spawn : Option Spawn := none
  /-- Type 37 slots marked for removal. They remain active until their NPC turn. -/
  markedOldMan : Array Nat := #[]
  /-- `Announcement.HasAwoken` with the localized name of NPC 35. -/
  announce : Bool := false

/-- `NPCChatPanel.DrawButtons` → `OldManCurse.Interact` →
`NPC.SpawnSkeletron(myPlayer, false)`. Ordinary Classic takes no random
sample for the button or the head allocation. -/
def click (time : Time) (input : Click) (state : State) (slots : Array Slot) (random : Rng)
    (activeTime : Nat := 750) : ClickResult := Id.run do
  if !input.hovered || !input.leftDown || !input.leftRelease || !input.mouseInterfaceAllowed ||
      !curseVisible time state slots then
    return { state, slots, random }
  let mut result : ClickResult := { state := { state with text := .none }, slots, random, consumed := true }
  let existing := slots.any (fun slot => slot.active && slot.typeId == headType)
  let mut source := none
  for index in [:slots.size] do
    let slot := slots[index]!
    if slot.active && slot.typeId == oldManType then
      source := some slot
      result := { result with
        slots := result.slots.set! index { slot with ai3 := 1 }
        markedOldMan := result.markedOldMan.push index }
  let some oldMan := source | return result
  if existing then return result
  let some index := findSlot result.slots 0 | return { result with announce := true }
  let spawn := spawnPart headType index 0
    (oldMan.position.x.toInt32.toInt + oldMan.width / 2)
    (oldMan.position.y.toInt32.toInt + oldMan.height / 2) activeTime
  return { result with slots := result.slots.set! index spawn.part.occupy, spawn := some spawn, announce := true }

inductive BloodMotion where
  | ordinary
  | skeletronMouth (headVelocity : Vector)
  | skeletronTrail
  deriving Repr

/-- The draw-side effects that advance the named NPC stream on these paths. -/
inductive Effect where
  | dust (typeId : Nat) (position : Vector) (width height : Nat) (speed : Vector) (scale : Float32)
      (motion : BloodMotion := .ordinary)
  | gore (typeId : Nat) (position velocity : Vector)
  deriving Repr

abbrev EmitEffect := Effect → Rng → Rng

/-- Ordinary blood dust with a free slot and gore enabled. The caller can
use its shared dust pool and viewport in place of this sampler. -/
def sampleEffect (visible : Vector → Bool) (effect : Effect) (random : Rng) : Rng :=
  match effect with
  | .dust _ position width height .. =>
    if !visible position then random else
      let (_, random) := random.next (max 5 width - 4)
      let (_, random) := random.next (max 5 height - 4)
      let (_, random) := random.nextRange (-20) 21
      let (_, random) := random.nextRange (-20) 21
      let (_, random) := random.next 3
      (random.nextRange (-20) 21).2
  | .gore .. =>
    let (_, random) := random.nextRange 10 31
    (random.nextRange (-20) 21).2

structure OldManEnd where
  slot : Slot
  life : Int
  random : Rng
  effects : Array Effect
  sound : Nat := 15

/-- The marked resident disappears on its own NPC turn. The caller discards
the resident from its actor table and leaves slot protection to the slot phase. -/
def retireOldMan (slot : Slot) (velocity : Vector) (emit : EmitEffect) (random : Rng) :
    Option OldManEnd := do
  guard (slot.active && slot.typeId == oldManType && slot.ai3 == 1)
  let effects : Array Effect := Array.replicate 50 (.dust 5 slot.position slot.width slot.height ⟨0, -2.5⟩ 1) ++
    (#[(58, 0), (59, 20), (59, 20), (60, 34), (60, 34)].map fun (kind, y) =>
      .gore kind ⟨slot.position.x, slot.position.y + single y⟩ velocity)
  return {
    slot := { slot with active := false }
    life := -1
    effects
    random := effects.foldl (fun rng effect => emit effect rng) random
  }

structure LocalTarget where
  slot : Nat := 0
  position : Vector
  width : Nat := 20
  height : Nat := 42

def LocalTarget.center (target : LocalTarget) : Vector :=
  ⟨target.position.x + single (target.width / 2), target.position.y + single (target.height / 2)⟩

def LocalTarget.motion (target : LocalTarget) : SkeletronHandTarget :=
  ⟨target.position.x, target.position.y, target.center.x, target.center.y⟩

def Part.blood (head : Part) (velocity : Vector) : Array Effect :=
  let centerX := head.position.x + single (head.width / 2)
  let mouth := Effect.dust 5 ⟨centerX - 15 - velocity.x * 5, head.position.y + single head.height - 2⟩
    30 10 ⟨-velocity.x * 0.2, 3⟩ 2 (.skeletronMouth velocity)
  let below := Effect.dust 5 ⟨head.position.x, head.position.y + 120⟩ head.width 60 velocity 2 .skeletronTrail
  #[mouth, below, below]

structure HeadStart where
  head : Part
  hands : Array Spawn
  slots : Array Slot
  random : Rng
  effects : Array Effect := #[]

/-- Style 11 opens the fight for a nearby live player during the first night
NPC phase. It allocates both hands before moving the head and emitting dust.
Higher hand slots receive their own turns later in the same NPC loop. -/
def firstHeadTurn (head : Part) (slots : Array Slot) (target : LocalTarget)
    (emit : EmitEffect) (random : Rng) (activeTime : Nat := 750) : HeadStart := Id.run do
  if head.typeId != headType || head.ai0 != 0 then return { head, hands := #[], slots, random }
  let centerX := head.position.x + single (head.width / 2)
  let centerY := head.position.y + single (head.height / 2)
  let targetX := single (target.position.x.toInt32.toInt + target.width / 2)
  let targetY := single (target.position.y.toInt32.toInt + target.height / 2)
  let direction := if targetX < centerX then -1 else 1
  let directionY := if targetY < centerY then -1 else 1
  let mut slots := slots
  let mut hands := #[]
  for (side, timer) in #[((-1 : Int), (0 : Nat)), (1, 150)] do
    if let some index := findSlot slots head.slot then
      let spawn := spawnPart handType index head.slot centerX.toInt32.toInt
        (head.position.y.toInt32.toInt + head.height / 2) activeTime
      let part := { spawn.part with
        ai0 := single side, ai1 := single head.slot, ai3 := single timer, target := target.slot }
      slots := slots.set! index part.occupy
      hands := hands.push { spawn with part }
  let hoverY := target.position.y - 250
  let vx := if centerX < target.center.x then 0.05 else if centerX > target.center.x then -0.05 else 0
  let vy := if head.position.y < hoverY then 0.02 else if head.position.y > hoverY then -0.02 else 0
  let velocity : Vector := ⟨vx, vy⟩
  let effects := head.blood velocity
  let head := { head with
    position := head.position + velocity, velocity
    direction, directionY, target := target.slot, ai0 := 1, ai2 := 1
    timeLeft := activeTime - 1, netUpdate := false }
  slots := slots.modify head.slot fun slot => { slot with position := head.position }
  return { head, slots, hands, effects, random := effects.foldl (fun rng effect => emit effect rng) random }

/-- The first dry hand update uses Classic style 12 with the updated head.
Further turns can use the existing Skeletron state machine. -/
def firstHandTurn (hand head : Part) (target : LocalTarget) : Part :=
  let nativeHead : SkeletronHead := {
    x := head.position.x, y := head.position.y, vx := head.velocity.x, vy := head.velocity.y
    direction := head.direction }
  let boss : Skeletron := {
    head := nativeHead, health := head.life.toNat, phase := .hover, ticks := 1
    leftHand := none, rightHand := none }
  let nativeHand : SkeletronHand := {
    x := hand.position.x, y := hand.position.y, vx := hand.velocity.x, vy := hand.velocity.y
    health := hand.life.toNat, phase := .hover, ticks := hand.ai3.toInt32.toInt.toNat }
  let handSide := -hand.ai0.toInt32.toInt
  let moved := nativeHand.advance boss target.motion handSide
  { hand with
    position := ⟨moved.x, moved.y⟩, velocity := ⟨moved.vx, moved.vy⟩
    rotation := moved.rotation, ai3 := single moved.ticks, spriteDirection := handSide, netUpdate := false }

end Lean4Terria.OriginalForestOldManCurse
