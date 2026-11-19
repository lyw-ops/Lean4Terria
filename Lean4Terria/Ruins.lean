import Lean4Terria.Ranged

namespace Lean4Terria

def ruinMaxHealth : Nat := 18

inductive RuinPhase where
  | windup | slam | recover
  deriving BEq, Repr

structure RuinSentinel where
  body : Player
  health : Nat
  phase : RuinPhase
  ticks : Nat
  targetX : Int
  deriving BEq, Repr

inductive RuinEncounter where
  | ready
  | fighting (boss : RuinSentinel)
  | reward (position : TilePos)
  | cleared
  deriving BEq, Repr

inductive RuinEvent where
  | started | blocked | withdrew | hit | defeated
  deriving BEq, Repr

def RuinSentinel.enraged (boss : RuinSentinel) : Bool := boss.health ≤ ruinMaxHealth / 2

def RuinSentinel.phaseDuration (boss : RuinSentinel) : Nat :=
  match boss.phase with
  | .windup => if boss.enraged then 57 else 76
  | .slam => if boss.enraged then 14 else 18
  | .recover => if boss.enraged then 50 else 66

def RuinSentinel.targetLocked (boss : RuinSentinel) : Bool :=
  boss.phase != .windup || boss.ticks ≤ (if boss.enraged then 20 else 28)

def inRuinArena (position : TilePos) : Bool :=
  ruinArenaStart ≤ position.x && position.x ≤ ruinArenaEnd &&
    84 ≤ position.y && position.y < ruinArenaFloor

def ruinPlayerInRange (player : Player) : Bool :=
  let position := player.centerTile
  (ruinArenaStart : Int) - 2 ≤ position.x && position.x ≤ (ruinArenaEnd : Int) + 3 &&
    82 ≤ position.y && position.y < (ruinArenaFloor : Int) + 4

private def canSummon (world : World) (player : Player) : Bool :=
  let position := player.centerTile
  world.tile ruinAltar == .ruinAltar && inRuinArena position &&
    (position.x - ruinAltar.x).natAbs + (position.y - ruinAltar.y).natAbs ≤ 5

private def targetFor (player : Player) : Int :=
  let left := (ruinArenaStart : Int) * tileUnits
  let right := ((ruinArenaEnd : Int) + 1) * tileUnits - 1
  max left (min right (bodyCenterX player))

private def enterPhase (boss : RuinSentinel) (phase : RuinPhase) : RuinSentinel :=
  let boss := { boss with phase }
  { boss with ticks := boss.phaseDuration }

private def spawnRuin (world : World) (player : Player) : Option RuinSentinel := do
  let body ← findStandingNear world 351 ruinArenaFloor 7 5 fun candidate =>
    inRuinArena candidate.centerTile && !bodiesOverlap candidate player
  let boss : RuinSentinel :=
    { body, health := ruinMaxHealth, phase := .windup, ticks := 0, targetX := targetFor player }
  return enterPhase boss .windup

def ruinRespawn (world : World) (player : Player) (crawler : Option Crawler) : Player :=
  let checkpoint := findStandingNear world ruinCheckpointColumn ruinShaftFloor 3 4 fun body =>
    let position := body.centerTile
    (ruinArenaEnd : Int) + 4 ≤ position.x && position.x ≤ ruinShaftColumn &&
      !crawler.any (fun enemy => bodiesOverlap body enemy.body)
  match checkpoint with
  | some body => body
  | none => respawn world player crawler

/-- Both melee and arrows can damage the Sentinel during recovery. -/
def damageRuin (encounter : RuinEncounter) (damage : Nat) : RuinEncounter × Option RuinEvent :=
  match encounter with
  | .fighting boss =>
    if boss.phase != .recover || damage == 0 then (encounter, none)
    else if boss.health ≤ damage then (.reward boss.body.centerTile, some .defeated)
    else
      let injured := { boss with health := boss.health - damage }
      let injured := { injured with ticks := min injured.ticks injured.phaseDuration }
      (.fighting injured, some .hit)
  | _ => (encounter, none)

private def groundWaves (world : World) (boss : RuinSentinel) : Array Projectile :=
  let x := boss.targetX
  let y := (ruinArenaFloor : Int) * tileUnits - 64
  if (world.tile ⟨x / tileUnits, y / tileUnits⟩).solid then #[] else
    let kind := ShotKind.ruinWave
    let wave : Projectile := { x, y, vx := kind.speed, vy := 0, life := kind.lifetime, kind }
    #[{ wave with vx := -wave.vx }, wave]

private def tickRuin (world : World) (player : Player) (boss : RuinSentinel) :
    RuinSentinel × Array Projectile :=
  let next := { boss with ticks := boss.ticks - 1 }
  match boss.phase with
  | .windup =>
    let next := if boss.targetLocked then next else { next with targetX := targetFor player }
    (if next.ticks == 0 then enterPhase next .slam else next, #[])
  | .slam =>
    let shots := if boss.ticks == boss.phaseDuration then groundWaves world boss else #[]
    (if next.ticks == 0 then enterPhase next .recover else next, shots)
  | .recover =>
    (if next.ticks == 0 then enterPhase { next with targetX := targetFor player } .windup else next, #[])

/-- The Sentinel stays on its footing while the marked spot releases two low waves. -/
def advanceRuin (world : World) (player : Player) (combat : CombatState)
    (encounter : RuinEncounter) (summon : Bool) (strike : Option Nat) :
    RuinEncounter × Array Projectile × Option RuinEvent := Id.run do
  let .fighting initial := encounter
    | if encounter == .ready && summon then
        if canSummon world player then
          if let some boss := spawnRuin world player then return (.fighting boss, #[], some .started)
        return (.ready, #[], some .blocked)
      else return (encounter, #[], none)
  if !ruinPlayerInRange player || initial.body.collides world then return (.ready, #[], some .withdrew)
  let body := moveBody world initial.body 0 false
  if !inRuinArena body.centerTile then return (.ready, #[], some .withdrew)
  let boss := { initial with body }
  let (encounter, event) := match strike with
    | some damage =>
      if boss.phase == RuinPhase.recover && swordReaches world player boss.body combat.facing true then
        damageRuin (.fighting boss) damage
      else (.fighting boss, none)
    | none => (.fighting boss, none)
  let .fighting boss := encounter | return (encounter, #[], event)
  let (boss, shots) := tickRuin world player boss
  return (.fighting boss, shots, event)

end Lean4Terria
