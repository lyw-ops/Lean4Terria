import Lean4Terria.Ranged

namespace Lean4Terria

def cavernMaxHealth : Nat := 24
def cavernBossColumn : Nat := 470
def cavernLowBeamY : Int := (cavernArenaFloor : Int) * tileUnits - 48
def cavernHighBeamY : Int := (cavernArenaFloor : Int) * tileUnits - 2 * tileUnits
def cavernHighBeamDelay : Nat := 42

inductive CavernPhase where
  | windup | beam | recover
  deriving BEq, Repr

structure DeepstoneColossus where
  body : Player
  health : Nat
  phase : CavernPhase
  ticks : Nat
  deriving BEq, Repr

inductive CavernEncounter where
  | ready
  | fighting (boss : DeepstoneColossus)
  | reward (position : TilePos)
  | cleared
  deriving BEq, Repr

def CavernEncounter.active : CavernEncounter → Bool
  | .fighting _ => true
  | _ => false

inductive CavernEvent where
  | started | blocked | withdrew | hit | defeated
  deriving BEq, Repr

def DeepstoneColossus.phaseDuration (boss : DeepstoneColossus) : Nat :=
  match boss.phase with
  | .windup => 66
  | .beam => 58
  | .recover => 84

def cavernPlayerInRange (player : Player) : Bool :=
  let position := player.centerTile
  cavernArenaStart ≤ position.x && position.x ≤ cavernArenaEnd + 2 &&
    cavernArenaFloor - 14 ≤ position.y && position.y < cavernArenaFloor + 4

/-- The Colossus remains centered on its original tile and on the arena floor. -/
def cavernBossPositionValid (body : Player) : Bool :=
  let column := body.centerTile.x
  (column - (cavernBossColumn : Int)).natAbs ≤ 5 &&
    body.x == column * tileUnits + (tileUnits - playerWidth) / 2 &&
    body.y == (cavernArenaFloor : Int) * tileUnits - playerHeight &&
    body.vx == 0 && body.vy == 0 && body.grounded

private def canSummon (world : World) (player : Player) : Bool :=
  let position := player.centerTile
  world.tile cavernAltar == .cavernAltar && inCavernArena position &&
    (position.x - cavernAltar.x).natAbs + (position.y - cavernAltar.y).natAbs ≤ 5

private def enterPhase (boss : DeepstoneColossus) (phase : CavernPhase) : DeepstoneColossus :=
  let boss := { boss with phase }
  { boss with ticks := boss.phaseDuration }

private def spawnCavern (world : World) (player : Player) : Option DeepstoneColossus := do
  let body ← findStandingNear world cavernBossColumn cavernArenaFloor 5 0 fun candidate =>
    cavernBossPositionValid candidate && !bodiesOverlap candidate player
  return enterPhase { body, health := cavernMaxHealth, phase := .windup, ticks := 0 } .windup

def cavernRespawn (world : World) (player : Player) (crawler : Option Crawler) : Player :=
  let checkpoint := findStandingNear world cavernCheckpointColumn cavernCheckpointFloor 2 2 fun body =>
    let x := body.centerTile.x
    cavernArenaEnd + 3 ≤ x && x ≤ cavernCheckpointColumn + 2 &&
      !cavernPlayerInRange body && !crawler.any (fun enemy => bodiesOverlap body enemy.body)
  match checkpoint with
  | some body => body
  | none => respawn world player crawler

/-- Melee and ranged attacks share the same recovery-only vulnerability. -/
def damageCavern (encounter : CavernEncounter) (damage : Nat) :
    CavernEncounter × Option CavernEvent :=
  match encounter with
  | .fighting boss =>
    if boss.phase != .recover || damage == 0 then (encounter, none)
    else if boss.health ≤ damage then (.reward boss.body.centerTile, some .defeated)
    else (.fighting { boss with health := boss.health - damage }, some .hit)
  | _ => (encounter, none)

private def beamsAt (world : World) (boss : DeepstoneColossus) (y : Int) : Array Projectile :=
  let x := bodyCenterX boss.body
  if (world.tile ⟨x / tileUnits, y / tileUnits⟩).solid then #[] else
    let kind := ShotKind.cavernBeam
    let beam : Projectile := { x, y, vx := kind.speed, vy := 0, life := kind.lifetime, kind }
    #[{ beam with vx := -beam.vx }, beam]

private def tickCavern (world : World) (boss : DeepstoneColossus) :
    DeepstoneColossus × Array Projectile :=
  let next := { boss with ticks := boss.ticks - 1 }
  match boss.phase with
  | .windup => (if next.ticks == 0 then enterPhase next .beam else next, #[])
  | .beam =>
    let shots :=
      if boss.ticks == boss.phaseDuration then beamsAt world boss cavernLowBeamY
      else if boss.ticks + cavernHighBeamDelay == boss.phaseDuration then
        beamsAt world boss cavernHighBeamY
      else #[]
    (if next.ticks == 0 then enterPhase next .recover else next, shots)
  | .recover => (if next.ticks == 0 then enterPhase next .windup else next, #[])

/-- Jump over the low beam, then return to the floor for the high beam. -/
def advanceCavern (world : World) (player : Player) (combat : CombatState)
    (encounter : CavernEncounter) (summon : Bool) (strike : Option Nat) :
    CavernEncounter × Array Projectile × Option CavernEvent := Id.run do
  let .fighting boss := encounter
    | if encounter == .ready && summon then
        if canSummon world player then
          if let some boss := spawnCavern world player then return (.fighting boss, #[], some .started)
        return (.ready, #[], some .blocked)
      else return (encounter, #[], none)
  let footing : TilePos := ⟨boss.body.centerTile.x, cavernArenaFloor⟩
  if !cavernPlayerInRange player || world.tile cavernAltar != .cavernAltar ||
      !cavernBossPositionValid boss.body || boss.body.collides world || !(world.tile footing).solid then
    return (.ready, #[], some .withdrew)
  let (encounter, event) := match strike with
    | some damage =>
      if boss.phase == CavernPhase.recover && swordReaches world player boss.body combat.facing true then
        damageCavern encounter damage
      else (encounter, none)
    | none => (encounter, none)
  let .fighting boss := encounter | return (encounter, #[], event)
  let (boss, shots) := tickCavern world boss
  return (.fighting boss, shots, event)

end Lean4Terria
