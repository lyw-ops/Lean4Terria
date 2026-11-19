import Lean4Terria.Equipment
import Lean4Terria.Ranged

namespace Lean4Terria

def raidWaveCount : Nat := 2
def raidCountdownTicks : Nat := 120
def raidMaxFoes : Nat := 3

inductive RaidFoeKind where
  | runner | slinger | brute
  deriving BEq, Repr

def RaidFoeKind.maxHealth : RaidFoeKind → Nat
  | .runner | .slinger => 4
  | .brute => 7

def RaidFoeKind.maxCooldown : RaidFoeKind → Nat
  | .runner => 40
  | .slinger => 90
  | .brute => 54

def RaidFoeKind.maxSpeed : RaidFoeKind → Int
  | .runner => 27
  | .slinger => 16
  | .brute => 19

private def RaidFoeKind.contactDamage : RaidFoeKind → Nat
  | .runner | .slinger => 2
  | .brute => 3

structure Raider where
  body : Player
  health : Nat
  cooldown : Nat
  kind : RaidFoeKind
  deriving BEq, Repr

structure RaidWave where
  number : Nat
  countdown : Nat
  foes : Array Raider
  deriving BEq, Repr

inductive RaidEncounter where
  | ready
  | active (wave : RaidWave)
  | reward (position : TilePos)
  | cleared
  deriving BEq, Repr

inductive RaidEvent where
  | started | blocked | withdrew | nextWave | hit | defeated | hurt | fell
  deriving BEq, Repr

def inRaidFoeRange (position : TilePos) : Bool :=
  amberBluffsStart ≤ position.x && position.x ≤ amberBluffsEnd &&
    0 ≤ position.y && position.y < worldHeight

def inRaidRewardRange (position : TilePos) : Bool := inRaidFoeRange position

def raidPlayerInRange (seed : UInt64) (player : Player) : Bool :=
  let position := player.centerTile
  let surface : Int := amberBluffSurface seed position.x.toNat
  (amberBluffsStart : Int) - 4 ≤ position.x && position.x ≤ (amberBluffsEnd : Int) + 4 &&
    surface - 7 ≤ position.y && position.y ≤ surface + 4

private def hasOpenSky (world : World) (position : TilePos) : Bool := Id.run do
  for y in [:position.y.toNat] do
    if (world.tile ⟨position.x, y⟩).solid then return false
  return true

def canStartRaid (world : World) (seed : UInt64) (player : Player) : Bool :=
  let position := player.centerTile
  raidPlayerInRange seed player && inAmberBluffs position &&
    position.y ≤ (amberBluffSurface seed position.x.toNat : Int) + 1 && hasOpenSky world position

private def spawnWave (world : World) (seed : UInt64) (player : Player) (number : Nat) :
    Option RaidWave := do
  let groups : List (Nat × RaidFoeKind) := if number == 1 then
      [(176, .runner), (194, .slinger)]
    else [(174, .brute), (192, .runner), (196, .slinger)]
  let mut foes : Array Raider := #[]
  for (column, kind) in groups do
    let body ← findStandingNear world column (amberBluffSurface seed column) 4 5 fun candidate =>
      let position := candidate.centerTile
      inRaidFoeRange position && hasOpenSky world position &&
        (candidate.x - player.x).natAbs ≥ 3 * tileUnits.toNat &&
        !foes.any (fun foe => bodiesOverlap foe.body candidate)
    let cooldown := if kind == .slinger then 42 else 0
    foes := foes.push { body, kind, cooldown, health := kind.maxHealth }
  return { number, countdown := 0, foes }

def raidRespawn (world : World) (seed : UInt64) (player : Player) (crawler : Option Crawler) : Player :=
  match findStandingNear world 166 (groundHeight seed 166) 3 5 fun candidate =>
      let position := candidate.centerTile
      position.x < amberBluffsStart && hasOpenSky world position &&
        !crawler.any (fun enemy => bodiesOverlap enemy.body candidate) with
  | some body => body
  | none => respawn world player crawler

/-- Resolve a single weapon hit. Intermission begins as soon as the first wave is cleared. -/
def damageRaider (encounter : RaidEncounter) (index damage : Nat) : RaidEncounter × Option RaidEvent := Id.run do
  let .active wave := encounter | return (encounter, none)
  let some foe := wave.foes[index]? | return (encounter, none)
  if damage == 0 then return (encounter, none)
  if damage < foe.health then
    let foes := wave.foes.set! index { foe with health := foe.health - damage }
    return (.active { wave with foes }, some .hit)
  let foes := wave.foes.eraseIdxIfInBounds index
  if !foes.isEmpty then return (.active { wave with foes }, some .hit)
  if wave.number < raidWaveCount then
    return (.active { number := wave.number + 1, countdown := raidCountdownTicks, foes }, some .nextWave)
  return (.reward foe.body.centerTile, some .defeated)

private def advanceRaider (world : World) (player : Player) (foe : Raider) : Raider × Option Projectile := Id.run do
  let body := foe.body
  let dx := player.x - body.x
  let slinger := foe.kind == .slinger
  let speed := if slinger && dx.natAbs ≤ 8 * tileUnits.toNat then 0
    else max (-foe.kind.maxSpeed) (min dx foe.kind.maxSpeed)
  let walked := moveBody world body speed false
  let hop := speed != 0 && walked.vx == 0 && body.grounded && (slinger || foe.cooldown == 0)
  let moved := if hop then moveBody world body speed true else walked
  let body := if inRaidFoeRange moved.centerTile then moved else moveBody world body 0 false
  let cooldown := foe.cooldown - 1
  if slinger then
    let dx := player.x - body.x
    let dy := player.y - body.y
    if cooldown == 0 && dx.natAbs ≤ 12 * tileUnits.toNat && dy.natAbs ≤ 5 * tileUnits.toNat then
      if let some shot := aimShot .raidBolt (bodyCenterX body) (bodyCenterY body)
          (bodyCenterX player) (bodyCenterY player) then
        return ({ foe with body, cooldown := foe.kind.maxCooldown }, some shot)
  let cooldown := if hop && !slinger then foe.kind.maxCooldown else cooldown
  return ({ foe with body, cooldown }, none)

/-- Advance the raid after player movement and the shared weapon and hurt cooldowns. -/
def advanceRaid (world : World) (seed : UInt64) (player : Player) (combat : CombatState)
    (encounter : RaidEncounter) (begin : Bool) (defense : Nat) (strike : Option Nat)
    (canStrike : RaidFoeKind → Bool := fun _ => true) :
    Player × CombatState × RaidEncounter × Array Projectile × Option RaidEvent := Id.run do
  let .active starting := encounter
    | if encounter == .ready && begin then
        if canStartRaid world seed player then
          if let some wave := spawnWave world seed player 1 then
            return (player, combat, .active wave, #[], some .started)
        return (player, combat, .ready, #[], some .blocked)
      else return (player, combat, encounter, #[], none)
  if !raidPlayerInRange seed player then
    return (player, combat, .ready, #[], some .withdrew)
  if starting.countdown > 1 then
    return (player, combat, .active { starting with countdown := starting.countdown - 1 }, #[], none)
  if starting.countdown == 1 then
    let some wave := spawnWave world seed player starting.number
      | return (player, combat, .ready, #[], some .blocked)
    return (player, combat, .active wave, #[], none)
  let mut wave := starting
  let mut event := none
  if let some damage := strike then
    if let some index := wave.foes.findIdx? (fun foe =>
        canStrike foe.kind && swordReaches world player foe.body combat.facing true) then
      let (next, hit) := damageRaider (.active wave) index damage
      let .active updated := next | return (player, combat, next, #[], hit)
      if updated.countdown > 0 then return (player, combat, next, #[], hit)
      wave := updated
      event := hit
  let mut shots := #[]
  let mut foes := #[]
  let mut contact := 0
  for foe in wave.foes do
    let (foe, shot) := advanceRaider world player foe
    if let some shot := shot then shots := shots.push shot
    foes := foes.push foe
    if bodiesOverlap player foe.body then contact := max contact foe.kind.contactDamage
  let encounter := RaidEncounter.active { wave with foes }
  if combat.health > 0 && combat.invulnerable == 0 && contact > 0 then
    let damage := damageAfterDefense contact defense * healthPerHeart
    if combat.health ≤ damage then
      return (player, combat.afterDefeat, .ready, #[], some .fell)
    let combat := combat.hurt damage
    return (player, combat, encounter, shots, some .hurt)
  return (player, combat, encounter, shots, event)

end Lean4Terria
