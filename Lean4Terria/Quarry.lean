import Lean4Terria.Combat

namespace Lean4Terria

def quarryCreatureMaxHealth : Nat := 5
def quarryCreatureHopDelay : Nat := 65

structure QuarryCrawler where
  body : Player
  health : Nat
  hopIn : Nat
  deriving BEq, Repr

structure QuarryState where
  creature : Option QuarryCrawler := none
  cleared : Bool := false
  deriving BEq, Repr, Inhabited

inductive QuarryEvent where
  | hit | defeated | hurt | fell
  deriving BEq, Repr

def inQuarryCreatureRange (position : TilePos) : Bool :=
  292 ≤ position.x && position.x ≤ quarryEnd && 0 ≤ position.y && position.y < worldHeight

def initialQuarry (world : World) : QuarryState :=
  let creature := (findStandingNear world 298 66 5 4 (inQuarryCreatureRange ·.centerTile)).map
    fun body => { body, health := quarryCreatureMaxHealth, hopIn := quarryCreatureHopDelay }
  { creature }

private def QuarryCrawler.move (crawler : QuarryCrawler) (world : World) (player : Player) :
    QuarryCrawler :=
  let body := crawler.body
  let dx := player.x - body.x
  let dy := player.y - body.y
  let chasing := dx.natAbs ≤ 10 * tileUnits.toNat && dy.natAbs ≤ 4 * tileUnits.toNat
  let patrolSpeed := if body.vx < 0 then -12 else 12
  let speed := if chasing then max (-maxHorizontalSpeed) (min dx maxHorizontalSpeed) else patrolSpeed
  let jump := chasing && crawler.hopIn == 0 && body.grounded && dx.natAbs ≥ tileUnits.toNat
  let moved := moveBody world body speed jump
  let body :=
    if inQuarryCreatureRange moved.centerTile && (chasing || moved.vx != 0) then moved
    else if !chasing then
      let reflected := moveBody world body (-patrolSpeed) false
      if inQuarryCreatureRange reflected.centerTile then reflected else moveBody world body 0 false
    else moveBody world body 0 jump
  { crawler with body, hopIn := if jump then quarryCreatureHopDelay else crawler.hopIn - 1 }

private def quarryRespawn (world : World) (player : Player) (crawler : Option Crawler) : Player :=
  match findStandingNear world 280 66 5 4 (fun body => body.centerTile.x < 292) with
  | some body => body
  | none => respawn world player crawler

/-- Advance the quarry creature after the shared sword and hurt cooldowns. -/
def advanceQuarry (world : World) (player : Player) (combat : CombatState) (quarry : QuarryState)
    (attack : Option Nat) : Player × CombatState × QuarryState × Option QuarryEvent := Id.run do
  let some creature := quarry.creature | return (player, combat, quarry, none)
  let mut creature := creature
  let mut event := none
  if let some damage := attack then
    if damage > 0 && swordReaches world player creature.body combat.facing true then
      if creature.health ≤ damage then
        return (player, combat, { creature := none, cleared := true }, some .defeated)
      creature := { creature with health := creature.health - damage }
      event := some .hit
  creature := creature.move world player
  let quarry := { quarry with creature := some creature }
  if combat.health > 0 && combat.invulnerable == 0 && bodiesOverlap player creature.body then
    if combat.health ≤ healthPerHeart then
      return (player, combat.afterDefeat, quarry, some .fell)
    let combat := combat.hurt healthPerHeart
    return (player, combat, quarry, some .hurt)
  return (player, combat, quarry, event)

end Lean4Terria
