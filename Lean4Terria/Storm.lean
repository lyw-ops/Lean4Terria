import Lean4Terria.Ranged

namespace Lean4Terria

def stormMaxHealth : Nat := 14

inductive StormPhase where
  | aim | barrage | recover
  deriving BEq, Repr

structure StormHerald where
  body : Player
  health : Nat
  phase : StormPhase
  ticks : Nat
  facing : Int
  deriving BEq, Repr

inductive StormEncounter where
  | ready
  | fighting (boss : StormHerald)
  | reward (position : TilePos)
  | cleared
  deriving BEq, Repr

inductive StormEvent where
  | started | blocked | withdrew | hit | defeated
  deriving BEq, Repr

def StormHerald.enraged (boss : StormHerald) : Bool := boss.health ≤ stormMaxHealth / 2

def StormHerald.phaseDuration (boss : StormHerald) : Nat :=
  match boss.phase with
  | .aim => if boss.enraged then 34 else 52
  | .barrage => if boss.enraged then 55 else 48
  | .recover => if boss.enraged then 48 else 66

def inStormArena (position : TilePos) : Bool :=
  stormArenaStart ≤ position.x && position.x ≤ stormArenaEnd &&
    49 ≤ position.y && position.y < stormArenaFloor

def stormPlayerInRange (player : Player) : Bool :=
  let position := player.centerTile
  (stormArenaStart : Int) - 2 ≤ position.x && position.x ≤ (stormArenaEnd : Int) + 2 &&
    47 ≤ position.y && position.y < (stormArenaFloor : Int) + 4

private def canSummon (world : World) (player : Player) : Bool := Id.run do
  let position := player.centerTile
  if !inStormArena position then return false
  for col in [:11] do
    let dx : Int := (col : Int) - 5
    let reach : Int := 5 - dx.natAbs
    for row in [:(2 * reach + 1).toNat] do
      let target : TilePos := ⟨position.x + dx, position.y + (row : Int) - reach⟩
      if world.tile target == .stormAltar then return true
  return false

private def aimSide (body player : Player) : Int :=
  if player.x < body.x then -1 else 1

private def enterPhase (boss : StormHerald) (phase : StormPhase) : StormHerald :=
  let boss := { boss with phase }
  { boss with ticks := boss.phaseDuration }

private def spawnStorm (world : World) (player : Player) : Option StormHerald := do
  let body ← findStandingNear world 352 stormArenaFloor 8 5 fun candidate =>
    inStormArena candidate.centerTile && !bodiesOverlap candidate player
  let boss : StormHerald :=
    { body, health := stormMaxHealth, phase := .aim, ticks := 0, facing := aimSide body player }
  return enterPhase boss .aim

def stormRespawn (world : World) (player : Player) (crawler : Option Crawler) : Player :=
  let checkpoint := findStandingNear world 327 stormArenaFloor 2 4 fun body =>
    let position := body.centerTile
    325 ≤ position.x && position.x < stormArenaStart &&
      !crawler.any (fun enemy => bodiesOverlap body enemy.body)
  match checkpoint with
  | some body => body
  | none => respawn world player crawler

/-- Both melee and arrows can damage the Herald during recovery. -/
def damageStorm (encounter : StormEncounter) (damage : Nat) : StormEncounter × Option StormEvent :=
  match encounter with
  | .fighting boss =>
    if boss.phase != .recover || damage == 0 then (encounter, none)
    else if boss.health ≤ damage then (.reward boss.body.centerTile, some .defeated)
    else
      let injured := { boss with health := boss.health - damage }
      let injured := { injured with ticks := min injured.ticks injured.phaseDuration }
      (.fighting injured, some .hit)
  | _ => (encounter, none)

private def volley (boss : StormHerald) (player : Player) (elapsed : Nat) : Array Projectile :=
  let x := bodyCenterX boss.body
  let y := bodyCenterY boss.body
  let targetX := bodyCenterX player
  -- Once the barrage starts, a player who crosses behind the Herald escapes its firing arc.
  let targetX := if boss.facing * (targetX - x) > 0 then targetX
    else x + boss.facing * 6 * tileUnits
  let targetY := bodyCenterY player
  let offsets : Array Int := if boss.enraged && elapsed % 22 == 0 then #[0, -192, 192] else #[0]
  offsets.filterMap fun offset => aimShot .herald x y targetX (targetY + offset)

private def tickStorm (player : Player) (boss : StormHerald) : StormHerald × Array Projectile :=
  let next := { boss with ticks := boss.ticks - 1 }
  match boss.phase with
  | .aim =>
    let next := { next with facing := aimSide boss.body player }
    (if next.ticks == 0 then enterPhase next .barrage else next, #[])
  | .barrage =>
    let elapsed := boss.phaseDuration - boss.ticks
    let cadence := if boss.enraged then 11 else 16
    let shots := if elapsed % cadence == 0 then volley boss player elapsed else #[]
    (if next.ticks == 0 then enterPhase next .recover else next, shots)
  | .recover =>
    (if next.ticks == 0 then enterPhase next .aim else next, #[])

/-- Advance the Herald after player movement and return the bolts to be simulated this tick. -/
def advanceStorm (world : World) (player : Player) (combat : CombatState)
    (encounter : StormEncounter) (summon : Bool) (strike : Option Nat) :
    StormEncounter × Array Projectile × Option StormEvent := Id.run do
  let .fighting initial := encounter
    | if encounter == .ready && summon then
        if canSummon world player then
          if let some boss := spawnStorm world player then return (.fighting boss, #[], some .started)
        return (.ready, #[], some .blocked)
      else return (encounter, #[], none)
  if !stormPlayerInRange player then return (.ready, #[], some .withdrew)
  let body := movePlayer world initial.body 0 false
  if !inStormArena body.centerTile then return (.ready, #[], some .withdrew)
  let boss := { initial with body }
  let (encounter, event) := match strike with
    | some damage =>
      if boss.phase == StormPhase.recover && swordReaches world player boss.body combat.facing true then
        damageStorm (.fighting boss) damage
      else (.fighting boss, none)
    | none => (.fighting boss, none)
  let .fighting boss := encounter | return (encounter, #[], event)
  let (boss, shots) := tickStorm player boss
  return (.fighting boss, shots, event)

end Lean4Terria
