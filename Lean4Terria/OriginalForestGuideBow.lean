import Lean4Terria.OriginalForestCritter
import Lean4Terria.OriginalForestGuide.Resident
import Lean4Terria.OriginalStarterMelee.LineOfSight
import Lean4Terria.UnifiedRandom

/-!
Terraria 1.4.5.8 Guide bow attack for a new Classic world. The caller supplies
the town AI's previously scanned targets and runs the normal NPC movement
between the AI and animation stages.
-/

namespace Lean4Terria.OriginalForestGuideBow

open OriginalCollision (Grid Vector single)
open OriginalForestCritter (Other vecDistance)
open OriginalForestGuide (Activity Resident)

/-- The town NPC scan chooses one danger side. Entry can try the other side
if its line test fails. During the attack the scan still uses 700 pixels. -/
structure Targets where
  danger : Bool := false
  stinky : Bool := false
  nearest : Option Other := none
  opposite : Option Other := none

structure State where
  /-- `localAI[2]` while the Guide is in `ai[0] = 12`. -/
  previousActivity : Option Activity := none
  /-- `localAI[1]` is decremented in the shared town AI tail. -/
  cooldown : Nat := 0
  /-- `localAI[3]` counts the attack frames and is repurposed on exit. -/
  localTimer : Nat := 0
  deriving BEq, Repr, Inhabited

def State.active (state : State) : Bool := state.previousActivity.isSome

def State.afterTownCooldown (state : State) : State :=
  { state with cooldown := state.cooldown - 1 }

def State.activityValue (state : State) (resident : Resident) : Float32 :=
  if state.active then 12 else resident.aiValue

/-- Arguments to the default `NewProjectile` overload and the arrow's NPC flags.
`center` is the spawn coordinate before the projectile subtracts half its size. -/
structure Shot where
  center : Vector
  velocity : Vector
  owner : Nat
  typeId : Nat := 1
  damage : Int := 12
  knockback : Float32 := 2.75
  npcProj : Bool := true
  noDropItem : Bool := true
  deriving BEq, Repr

def Shot.width (_ : Shot) : Nat := 10
def Shot.height (_ : Shot) : Nat := 10
def Shot.timeLeft (_ : Shot) : Nat := 1200
def Shot.penetrate (_ : Shot) : Nat := 1

def Shot.initialPosition (shot : Shot) : Vector :=
  ⟨shot.center.x - single shot.width * 0.5, shot.center.y - single shot.height * 0.5⟩

structure Step where
  resident : Resident
  state : State
  random : UnifiedRandom
  shot : Option Shot := none
  netUpdate : Bool := false

def attackTime : Nat := 30
def attemptChance : Int := 60

private def directionTo (origin target : Vector) : Option Vector := do
  let length := vecDistance target origin
  guard (length > 0)
  let inverse := 1 / length
  return ⟨(target.x - origin.x) * inverse, (target.y - origin.y) * inverse⟩

/-- Entry following a successful chance roll. The aim angle is saved before
movement and `spriteDirection` is updated later by `findFrame`. -/
def enter (grid : Grid) (resident : Resident) (state : State) (targets : Targets) : Option (Resident × State) := do
  let nearest ← targets.nearest
  let visible (target : Other) :=
    OriginalStarterMelee.LineOfSight.canHitLine grid resident.center 0 0 target.center 0 0
  let target ← if visible nearest then some nearest else targets.opposite.filter visible
  let aim ← directionTo resident.center target.center
  guard (aim.y.abs ≤ 0.5)
  let direction := if resident.position.x < target.position.x then 1 else -1
  return ({ resident with direction, timer := single attackTime, turnTimer := aim.y },
    { state with previousActivity := some resident.activity, localTimer := 0 })

/-- The bow roll in the shared town AI tail, after that tail decrements the
cooldown. A failed roll, obstruction or excessive angle still uses the roll. -/
def attempt (grid : Grid) (resident : Resident) (state : State) (targets : Targets)
    (random : UnifiedRandom) : Step := Id.run do
  if resident.kind != .guide || state.active || state.cooldown > 0 ||
      resident.activity == .pausing || resident.velocity.y != 0 ||
      !targets.danger || targets.stinky then
    return { resident, state, random }
  let (roll, random) := random.next 0 attemptChance
  if roll == 0 then
    if let some (resident, state) := enter grid resident state targets then
      return { resident, state, random, netUpdate := true }
  return { resident, state, random }

private def launch (resident : Resident) (targets : Targets) (owner : Nat) (random : UnifiedRandom) :
    Shot × UnifiedRandom :=
  let center := resident.center
  let facing := single resident.spriteDirection
  let aim := do
    let target ← targets.nearest
    let targetCenter := target.center
    let side := if targetCenter.x > center.x then 1 else if targetCenter.x < center.x then -1 else 0
    guard (side == resident.spriteDirection)
    directionTo center ⟨targetCenter.x, targetCenter.y - 4⟩
  let aim := aim.getD ⟨facing, 0⟩
  let (x, random) := random.sample
  let (y, random) := random.sample
  let spread := fun (roll : Float) => (0.7 - (-0.7) : Float32) * roll.toFloat32 - 0.7
  ({ center := ⟨center.x + single (resident.spriteDirection * 16), center.y - 2⟩
     velocity := ⟨aim.x * 10 + spread x, aim.y * 10 + spread y⟩, owner }, random)

/-- The `ai[0] = 12` block. It runs on ticks following entry. Run the common
town AI tail and normal NPC collision afterwards, then apply `findFrame`
while still active. The exit hands both fresh cooldown values to the tail. -/
def advance (resident : Resident) (state : State) (targets : Targets) (owner : Nat)
    (random : UnifiedRandom) : Option Step := do
  let previousActivity ← state.previousActivity
  let starting := resident.timer == single attackTime
  let state := { state with localTimer := if starting then 1 else state.localTimer + 1 }
  let resident := { resident with
    timer := resident.timer - 1, velocity.x := resident.velocity.x * 0.8
    frameCounter := if starting then 0 else resident.frameCounter }
  let (shot, random) := if state.localTimer == 1 then
    let (shot, random) := launch resident targets owner random
    (some shot, random)
    else (none, random)
  if resident.timer > 0 then return { resident, state, random, shot }
  let (wait, random) := random.next 0 20
  let (cooldown, random) := random.next 0 20
  let cooldown := 15 + cooldown.toNat
  let activity := if previousActivity == .cornered && targets.danger then .cornered else .idle
  return { resident := { resident with activity, timer := single (30 + wait), turnTimer := 0 }
           state := { previousActivity := none, cooldown, localTimer := cooldown }
           random, shot, netUpdate := true }

/-- Native Guide bow rows in the 26-row `NPC_22` sheet. The animation runs
after collision. Use ordinary resident animation after the bow state exits. -/
def findFrame (resident : Resident) : Resident :=
  if resident.velocity.y != 0 then { resident with frameRow := 1, frameCounter := 0 }
  else
    let row := if resident.turnTimer < -0.1 then 21
      else if resident.turnTimer > 0.3 then 25
      else if resident.turnTimer > 0.1 then 24
      else 23
    let reset := resident.frameRow != 0 && (resident.frameRow < 21 || resident.frameRow > 25)
    let frameCounter := if reset then 0 else resident.frameCounter + 1
    { resident with frameRow := row, frameCounter, spriteDirection := resident.direction }

end Lean4Terria.OriginalForestGuideBow
