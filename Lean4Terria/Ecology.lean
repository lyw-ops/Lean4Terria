import Lean4Terria.Combat
import Lean4Terria.Equipment

namespace Lean4Terria

inductive CaveMobKind where
  | slime | beetle | skitter | oreback
  deriving BEq, Repr

def CaveMobKind.name : CaveMobKind → String
  | .slime => "Cave slime"
  | .beetle => "Frost beetle"
  | .skitter => "Quarry skitter"
  | .oreback => "Oreback"

def CaveMobKind.maxHealth : CaveMobKind → Nat
  | .slime => 3
  | .beetle => 4
  | .skitter => 6
  | .oreback => 8

def CaveMobKind.maxSpeed : CaveMobKind → Int
  | .slime => 16
  | .beetle => 13
  | .skitter => 28
  | .oreback => 23

def CaveMobKind.maxHopIn : CaveMobKind → Nat
  | .slime => 36
  | .beetle => 72
  | .skitter => 30
  | .oreback => 48

private def CaveMobKind.contactDamage : CaveMobKind → Nat
  | .slime => 1
  | .beetle => 2
  | .skitter | .oreback => 3

structure CaveResident where
  body : Player
  kind : CaveMobKind
  health : Nat
  hopIn : Nat
  deriving BEq, Repr

structure CaveHabitat where
  kind : CaveMobKind
  mouth : TilePos
  cache : TilePos
  deriving BEq, Repr

private def CaveHabitat.inward (habitat : CaveHabitat) : Int :=
  if habitat.cache.x < habitat.mouth.x then -1 else 1

/-- The two entrance steps and the main corridor lie outside the resident's room. -/
def CaveHabitat.contains (habitat : CaveHabitat) (position : TilePos) : Bool :=
  let offset := (position.x - habitat.mouth.x) * habitat.inward
  let extent := (habitat.cache.x - habitat.mouth.x).natAbs + 1
  2 ≤ offset && offset ≤ (extent : Int) &&
    habitat.mouth.y + 2 ≤ position.y && position.y ≤ habitat.mouth.y + 11

inductive CaveSite where
  | waiting (cooldown : Nat)
  | alive (mob : CaveResident)
  | loot (position : TilePos)
  deriving BEq, Repr

structure CaveEcology where
  habitats : Array CaveHabitat := #[]
  sites : Array CaveSite := #[]
  deriving BEq, Repr, Inhabited

inductive CaveEvent where
  | hit | defeated | hurt | fell
  deriving BEq, Repr

def caveRespawnTicks : Nat := 1800

def initialCaveEcology (seed : UInt64) : CaveEcology := Id.run do
  let pockets := supplyPockets seed
  let kinds : Array CaveMobKind := #[.slime, .beetle, .skitter]
  let mut habitats := #[]
  for index in [:min pockets.size kinds.size] do
    if let some pocket := pockets[index]? then
      let kind := kinds[index]?.getD .skitter
      habitats := habitats.push { kind, mouth := pocket.mouth, cache := pocket.cache : CaveHabitat }
  let room : CaveHabitat := {
    kind := .oreback
    mouth := ⟨(deepsteelRoomStart : Int) - 2, (deepsteelFloor seed : Int) - 8⟩
    cache := deepsteelMarker seed }
  habitats := habitats.push room
  return { habitats, sites := Array.replicate habitats.size (.waiting 0) }

private def residentStanding (world : World) (player : Player) (habitat : CaveHabitat) : Option Player :=
  let anchor := habitat.cache.x - 2 * habitat.inward
  findStandingNear world anchor (habitat.cache.y + 1) 10 6 fun candidate =>
    let position := candidate.centerTile
    habitat.contains position && position != habitat.cache &&
      (candidate.x - player.x).natAbs ≥ 3 * tileUnits.toNat

private def CaveResident.move (mob : CaveResident) (world : World) (player : Player)
    (habitat : CaveHabitat) : CaveResident :=
  let dx := player.x - mob.body.x
  let speed := max (-mob.kind.maxSpeed) (min dx mob.kind.maxSpeed)
  let walked := moveBody world mob.body speed false
  let blocked := speed != 0 && walked.vx == 0
  let wantsHop := match mob.kind with
    | .slime => dx.natAbs ≥ tileUnits.toNat || blocked
    | .beetle | .oreback => blocked
    | .skitter => dx.natAbs ≥ 3 * tileUnits.toNat || blocked
  let hop := mob.body.grounded && mob.hopIn == 0 && wantsHop
  let moved := if hop then moveBody world mob.body speed true else walked
  let body := if habitat.contains moved.centerTile then moved else
    let stays := moveBody world mob.body 0 false
    if habitat.contains stays.centerTile then stays
    else
      -- If a player excavates below the habitat, find footing back inside it.
      (residentStanding world player habitat).getD { mob.body with vx := 0, vy := 0, grounded := false }
  { mob with body, hopIn := if hop then mob.kind.maxHopIn else mob.hopIn - 1 }

/-- A defeated resident leaves a pending drop. The pickup starts the respawn timer. -/
def damageCaveResident (ecology : CaveEcology) (index damage : Nat) :
    CaveEcology × Option CaveEvent := Id.run do
  let some (.alive mob) := ecology.sites[index]? | return (ecology, none)
  if damage == 0 then return (ecology, none)
  if mob.health ≤ damage then
    return ({ ecology with sites := ecology.sites.set! index (.loot mob.body.centerTile) }, some .defeated)
  let mob := { mob with health := mob.health - damage }
  return ({ ecology with sites := ecology.sites.set! index (.alive mob) }, some .hit)

private def tickWaiting : CaveSite → CaveSite
  | .waiting ticks => .waiting (ticks - 1)
  | site => site

/-- Advance the resident in the room the player is visiting. Cooldowns elapse
across the world, while an uncollected drop and a resident outside the active room persist. -/
def advanceCaveEcology (world : World) (player : Player) (combat : CombatState)
    (ecology : CaveEcology) (defense : Nat) (strike : Option Nat)
    (canStrike : Nat → Bool := fun _ => true) :
    Player × CombatState × CaveEcology × Option CaveEvent := Id.run do
  let mut ecology := { ecology with sites := ecology.sites.map tickWaiting }
  let mut event := none
  if let some damage := strike then
    for index in [:ecology.sites.size] do
      let some (CaveSite.alive mob) := ecology.sites[index]? | continue
      if !canStrike index || !swordReaches world player mob.body combat.facing true then continue
      let (next, hit) := damageCaveResident ecology index damage
      ecology := next
      event := hit
      if hit == some .defeated then return (player, combat, ecology, event)
      break
  let some index := ecology.habitats.findIdx? (fun room => room.contains player.centerTile)
    | return (player, combat, ecology, event)
  let some habitat := ecology.habitats[index]? | return (player, combat, ecology, event)
  let some site := ecology.sites[index]? | return (player, combat, ecology, event)
  match site with
  | .waiting 0 =>
    if habitat.kind == .oreback && world.tile habitat.cache != .mineMarker then
      return (player, combat, ecology, event)
    if let some body := residentStanding world player habitat then
      let mob : CaveResident := { body, kind := habitat.kind, health := habitat.kind.maxHealth, hopIn := 0 }
      ecology := { ecology with sites := ecology.sites.set! index (.alive mob) }
    return (player, combat, ecology, event)
  | .waiting _ | .loot _ => return (player, combat, ecology, event)
  | .alive mob =>
    let mob := mob.move world player habitat
    ecology := { ecology with sites := ecology.sites.set! index (.alive mob) }
    if combat.health > 0 && combat.invulnerable == 0 && bodiesOverlap player mob.body then
      let damage := damageAfterDefense mob.kind.contactDamage defense * healthPerHeart
      if combat.health ≤ damage then
        return (player, combat.afterDefeat, ecology, some .fell)
      let combat := combat.hurt damage
      return (player, combat, ecology, some .hurt)
    return (player, combat, ecology, event)

end Lean4Terria
