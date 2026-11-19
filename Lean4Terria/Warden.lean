import Lean4Terria.Combat

namespace Lean4Terria

def wardenMaxHealth : Nat := 12
def wardenMaxSpeed : Int := 76

inductive WardenPhase where
  | approach | windup | charge | recover
  deriving BEq, Repr

structure Warden where
  body : Player
  health : Nat
  phase : WardenPhase
  ticks : Nat
  facing : Int
  deriving BEq, Repr

inductive WardenEncounter where
  | ready
  | fighting (boss : Warden)
  | reward (position : TilePos)
  | cleared
  deriving BEq, Repr

inductive WardenEvent where
  | started | blocked | withdrew | hit | defeated | hurt | fell
  deriving BEq, Repr

def Warden.enraged (boss : Warden) : Bool := boss.health ≤ wardenMaxHealth / 2

def Warden.phaseDuration (boss : Warden) : Nat :=
  match boss.phase with
  | .approach => 0
  | .windup => if boss.enraged then 31 else 42
  | .charge => if boss.enraged then 27 else 26
  | .recover => if boss.enraged then 46 else 55

private def Warden.enterPhase (boss : Warden) (phase : WardenPhase) : Warden :=
  let boss := { boss with phase }
  { boss with ticks := boss.phaseDuration }

private def facingPlayer (boss player : Player) : Int :=
  if player.x < boss.x then -1 else 1

private def spawnWarden (world : World) (player : Player) : Option Warden := do
  let body ← findStandingNear world wardenSpawnColumn wardenArenaFloor 8 5 fun candidate =>
    inWardenArena candidate.centerTile && (candidate.x - player.x).natAbs ≥ 3 * tileUnits.toNat
  return {
    body, health := wardenMaxHealth, phase := .approach, ticks := 0
    facing := facingPlayer body player }

private def playerInRange (player : Player) : Bool :=
  let pos := player.centerTile
  (wardenFightStart : Int) - 2 ≤ pos.x && pos.x ≤ (wardenArenaEnd : Int) + 2 &&
    51 ≤ pos.y && pos.y < (wardenArenaFloor : Int) + 4

private def moveInChamber (world : World) (body : Player) (speed : Int) : Player :=
  let moved := moveBody world body speed false
  let column := moved.centerTile.x
  if column < wardenFightStart || column ≥ wardenArenaEnd then
    moveBody world body 0 false
  else moved

private def tickWarden (world : World) (player : Player) (boss : Warden) : Warden :=
  match boss.phase with
  | .approach =>
    let facing := facingPlayer boss.body player
    let column := boss.body.centerTile.x
    let leftRunway : Int := (wardenFightStart : Int) + 4
    let rightRunway : Int := (wardenArenaEnd : Int) - 5
    -- After charging into an edge, step back into the hall before winding up again.
    let retreat : Int := if column < leftRunway then 1 else if column > rightRunway then -1 else 0
    let atRunway := (column == leftRunway && facing < 0) || (column == rightRunway && facing > 0)
    let close := atRunway || (player.x - boss.body.x).natAbs ≤ 4 * tileUnits.toNat
    let direction := if retreat != 0 then retreat else if close then 0 else facing
    let speed := if boss.enraged then 26 else 20
    let body := moveInChamber world boss.body (direction * speed)
    if body.grounded && body.vx == 0 then
      ({ boss with body, facing }).enterPhase .windup
    else
      { boss with body, facing := if body.vx < 0 then -1 else if body.vx > 0 then 1 else facing }
  | .windup =>
    let boss := { boss with body := moveInChamber world boss.body 0, ticks := boss.ticks - 1 }
    if boss.ticks == 0 then boss.enterPhase .charge else boss
  | .charge =>
    let speed := if boss.enraged then wardenMaxSpeed else 58
    let body := moveInChamber world boss.body (boss.facing * speed)
    let boss := { boss with body, ticks := boss.ticks - 1 }
    if boss.ticks == 0 || body.vx == 0 then boss.enterPhase .recover else boss
  | .recover =>
    let boss := { boss with body := moveInChamber world boss.body 0, ticks := boss.ticks - 1 }
    if boss.ticks == 0 then boss.enterPhase .approach else boss

private def wardenRespawn (world : World) (player : Player) (crawler : Option Crawler) : Player :=
  let checkpoint := findStandingNear world wardenCheckpointColumn wardenArenaFloor 4 5 fun body =>
    body.centerTile.x < (wardenFightStart : Int) - 2 &&
      !crawler.any (fun enemy => bodiesOverlap body enemy.body)
  match checkpoint with
  | some body => body
  | none => respawn world player crawler

/-- Apply one tick after player movement and the shared swing and hurt cooldowns. -/
def advanceWarden (world : World) (player : Player) (combat : CombatState)
    (encounter : WardenEncounter) (attack : Option Nat) :
    Player × CombatState × WardenEncounter × Option WardenEvent := Id.run do
  let .fighting initialBoss := encounter
    | if encounter == .ready && inWardenArena player.centerTile then
        match spawnWarden world player with
        | some boss => return (player, combat, .fighting boss, some .started)
        | none => return (player, combat, .ready, some .blocked)
      else return (player, combat, encounter, none)
  if !playerInRange player then
    return (player, combat, .ready, some .withdrew)
  let mut boss := initialBoss
  let mut event := none
  if let some damage := attack then
    if damage > 0 && boss.phase == .recover &&
        swordReaches world player boss.body combat.facing true then
      if boss.health ≤ damage then
        return (player, combat, .reward boss.body.centerTile, some .defeated)
      boss := { boss with health := boss.health - damage }
      boss := { boss with ticks := min boss.ticks boss.phaseDuration }
      event := some .hit
  let charged := boss.phase == .charge
  boss := tickWarden world player boss
  if !inWardenArena boss.body.centerTile then
    return (player, combat, .ready, some .withdrew)
  if charged && combat.health > 0 && combat.invulnerable == 0 && bodiesOverlap player boss.body then
    if combat.health ≤ healthPerHeart then
      return (player, combat.afterDefeat, .ready, some .fell)
    let combat := combat.hurt healthPerHeart
    return (player, combat, .fighting boss, some .hurt)
  return (player, combat, .fighting boss, event)

end Lean4Terria
