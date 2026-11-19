import Lean4Terria.OriginalCollision

namespace Lean4Terria.OriginalForestGuide

open OriginalCollision (Vector single)

inductive Kind where
  | guide
  | oldMan
  | mechanic
  deriving BEq, Repr, Inhabited

def Kind.typeId : Kind → Int
  | .guide => 22
  | .oldMan => 37
  | .mechanic => 124

def Kind.typeName : Kind → String
  | .guide => "Guide"
  | .oldMan => "Old Man"
  | .mechanic => "Mechanic"

def Kind.textureName (kind : Kind) : String := s!"NPC_{kind.typeId}"

def Kind.frameCount : Kind → Nat
  | .guide => 26
  | .oldMan => 21
  | .mechanic => 23

def Kind.extraFrameCount : Kind → Nat
  | .guide => 10
  | .oldMan => 5
  | .mechanic => 9

inductive Activity where
  | idle
  | walking
  | pausing
  | talking
  | cornered
  deriving BEq, Repr, Inhabited

def Activity.aiValue : Activity → Float32
  | .idle => 0
  | .walking => 1
  | .pausing => 2
  | .talking => 7
  | .cornered => 8

structure Emote where
  id : Nat
  duration : Nat
  timeLeft : Nat
  deriving BEq, Repr

structure Resident where
  kind : Kind
  name : String := ""
  position : Vector := ⟨0, 0⟩
  homeTile : Int × Int := (0, 0)
  homeless : Bool := false
  variation : Int := 0
  homelessDespawn : Bool := false
  active : Bool := true
  velocity : Vector := ⟨0, 0⟩
  oldVelocity : Vector := ⟨0, 0⟩
  oldPosition : Vector := ⟨0, 0⟩
  direction : Int := 0
  directionY : Int := 1
  spriteDirection : Int := -1
  frameRow : Nat := 0
  frameCounter : Float := 0
  target : Int := 255
  activity : Activity := .idle
  sitting : Bool := false
  talkTarget : Nat := 0
  timer : Float32 := 0
  turnTimer : Float32 := 0
  stuckTimer : Nat := 0
  collideX : Bool := false
  collideY : Bool := false
  stairFall : Bool := false
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  life : Int := 250
  friendlyRegen : Nat := 0
  hostileImmunity : Nat := 0
  curseMarked : Bool := false
  wet : Bool := false
  emote : Option Emote := none
  deriving BEq, Repr, Inhabited

/-- Source rectangle in the locally installed original NPC XNB. -/
def Resident.textureFrame (n : Resident) : Nat × Nat × Nat × Nat :=
  let height := if n.kind == .mechanic then 58 else 56
  (0, n.frameRow * height, 40, height)

def Resident.walking (n : Resident) : Bool := n.activity == .walking

def Resident.aiValue (n : Resident) : Float32 := if n.sitting then 5 else n.activity.aiValue

def Resident.width (_ : Resident) : Nat := 18
def Resident.height (_ : Resident) : Nat := 40
def Resident.lifeMax (_ : Resident) : Int := 250
def Resident.damage (_ : Resident) : Int := 10
def Resident.defense (n : Resident) : Int := if n.kind == .guide then 30 else 15
def Resident.timeLeft (_ : Resident) : Int := 750
def Resident.npcSlots (_ : Resident) : Float32 := 1

def Resident.center (n : Resident) : Vector :=
  n.position + ⟨single (n.width / 2 : Nat), single (n.height / 2 : Nat)⟩

/-- `CheckActive` returns early for a resident. Its occupied NPC slot and its
scene-metrics contribution are independent of the nearby hostile-slot budget. -/
def Resident.nearbyActiveSlots (_ : Resident) : Float32 := 0

end Lean4Terria.OriginalForestGuide
