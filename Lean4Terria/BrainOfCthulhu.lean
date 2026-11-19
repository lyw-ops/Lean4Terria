import Lean4Terria.EyeOfCthulhu

namespace Lean4Terria

def brainMaxHealth : Nat := 1250
def brainCreeperMaxHealth : Nat := 100
def brainCreeperCount : Nat := 20
def brainTotalMaxHealth : Nat := brainMaxHealth + brainCreeperCount * brainCreeperMaxHealth
def brainHalfWidth : Int := 80 * (tileUnits / 16)
def brainHalfHeight : Int := 55 * (tileUnits / 16)
def brainCreeperHalfSize : Int := 15 * (tileUnits / 16)

private def brainRetreatTicks : Nat := 48
private def brainRushTicks : Nat := 84

inductive BrainPhase where
  | shielded | exposed | retreat
  deriving BEq, Repr, Inhabited

private def BrainPhase.fadeTicks : BrainPhase → Nat
  | .shielded => 51
  | .exposed => 17
  | .retreat => 0

structure BrainCreeper where
  id : Nat
  motion : EyeMotion
  health : Nat
  rush : Nat
  deriving BEq, Repr

structure BrainOfCthulhu where
  motion : EyeMotion
  health : Nat
  phase : BrainPhase
  /-- Time spent approaching in the current teleport cycle, or time retreating. -/
  timer : Nat
  /-- Zero while approaching, otherwise the elapsed fade-out and fade-in time. -/
  warp : Nat
  serial : Nat
  creepers : Array BrainCreeper
  deriving BEq, Repr

def BrainCreeper.hitBox (creeper : BrainCreeper) : HitBox :=
  let m := creeper.motion
  ⟨m.x - brainCreeperHalfSize, m.y - brainCreeperHalfSize,
    m.x + brainCreeperHalfSize, m.y + brainCreeperHalfSize⟩

def BrainOfCthulhu.hitBox (brain : BrainOfCthulhu) : HitBox :=
  let m := brain.motion
  ⟨m.x - brainHalfWidth, m.y - brainHalfHeight, m.x + brainHalfWidth, m.y + brainHalfHeight⟩

def BrainOfCthulhu.totalHealth (brain : BrainOfCthulhu) : Nat :=
  brain.creepers.foldl (fun health creeper => health + creeper.health) brain.health

def BrainOfCthulhu.visibleAlpha (brain : BrainOfCthulhu) : Nat :=
  if brain.phase == .retreat then 255 * (brainRetreatTicks - brain.timer) / brainRetreatTicks
  else
    let base := if brain.phase == .shielded then 190 else 255
    let fade := brain.phase.fadeTicks
    let amount := if brain.warp ≤ fade then brain.warp else 2 * fade - brain.warp
    base * (fade - amount) / fade

def BrainOfCthulhu.valid (brain : BrainOfCthulhu) (world : World) : Bool := Id.run do
  if brain.health == 0 || brain.health > brainMaxHealth || !brain.motion.valid world 144 ||
      brain.serial ≥ 65536 || brain.warp > 2 * brain.phase.fadeTicks then return false
  if brain.phase == .retreat then return brain.timer < brainRetreatTicks && brain.creepers.isEmpty
  if brain.timer > 300 || (brain.phase == .shielded) != !brain.creepers.isEmpty ||
      (brain.phase == .shielded && brain.health != brainMaxHealth) then return false
  let mut seen : Array Nat := #[]
  for creeper in brain.creepers do
    if creeper.id ≥ brainCreeperCount || seen.contains creeper.id || creeper.health == 0 ||
        creeper.health > brainCreeperMaxHealth || creeper.rush > brainRushTicks ||
        !creeper.motion.valid world 144 then return false
    seen := seen.push creeper.id
  return true

private def advanceMotion (motion : EyeMotion) : EyeMotion :=
  { motion with x := motion.x + motion.vx, y := motion.y + motion.vy }

private def approach (current target divisor : Int) : Int :=
  let gap := target - current
  let step : Int := max 1 (gap.natAbs / divisor.toNat)
  if gap == 0 then current else if gap > 0 then current + step else current - step

def spawnBrain (world : World) (player : Player) : BrainOfCthulhu := Id.run do
  let side : Int := if player.centerTile.x + 12 < (world.width : Int) then 1 else -1
  let motion : EyeMotion := ⟨bodyCenterX player + side * 12 * tileUnits,
    bodyCenterY player - 5 * tileUnits, 0, 0⟩
  let mut creepers := #[]
  for id in [:brainCreeperCount] do
    let x : Int := ((id * 269 + 73) % 257 : Nat)
    let y : Int := ((id * 173 + 9) % 183 : Nat)
    let offsetX := (x - 128) * 16
    let offsetY := (y - 91) * 16
    let (vx, vy) := (aimVelocity 64 offsetX offsetY).getD (0, 0)
    creepers := creepers.push ⟨id, ⟨motion.x + offsetX, motion.y + offsetY, vx, vy⟩,
      brainCreeperMaxHealth, 0⟩
  return ⟨motion, brainMaxHealth, .shielded, 0, 0, 0, creepers⟩

def BrainOfCthulhu.contact (brain : BrainOfCthulhu) (player : Player) : Nat :=
  if brain.phase == .retreat then 0
  else if brain.hitBox.overlapsPlayer player then 30
  else if brain.creepers.any (fun creeper => creeper.hitBox.overlapsPlayer player) then 20
  else 0

def BrainOfCthulhu.takeHit (brain : BrainOfCthulhu) (player : Player) (damage : Nat) : BrainOfCthulhu :=
  if brain.phase != .exposed || damage == 0 then brain
  else
    let m := brain.motion
    let (vx, vy) := (aimVelocity 54 (m.x - bodyCenterX player) (m.y - bodyCenterY player)).getD (0, -32)
    { brain with
      health := brain.health - damageAfterDefense damage 7
      timer := brain.timer - 2
      motion := { m with vx, vy } }

def BrainOfCthulhu.damageCreeper (brain : BrainOfCthulhu) (id damage : Nat) : BrainOfCthulhu := Id.run do
  if brain.phase != .shielded || damage == 0 then return brain
  let creepers := brain.creepers.filterMap fun creeper =>
    if creeper.id != id then some creeper
    else
      let health := creeper.health - damageAfterDefense damage 5
      if health == 0 then none else some { creeper with health, rush := 0 }
  if creepers.isEmpty then return { brain with creepers, phase := .exposed, timer := 0, warp := 0 }
  return { brain with creepers }

private def BrainOfCthulhu.teleport (brain : BrainOfCthulhu) (world : World) (player : Player) : EyeMotion := Id.run do
  let center := player.centerTile
  let minDistance := if brain.phase == .shielded then 12 else 10
  let maxDistance := if brain.phase == .shielded then 40 else 12
  let mut fallback := { brain.motion with vx := 0, vy := 0 }
  for attempt in [:100] do
    let roll := (hash (center, brain.serial, attempt, (826 : Nat))).toNat
    let x : Int := minDistance + roll % (maxDistance - minDistance + 1)
    let y : Int := minDistance + (roll / 37) % (maxDistance - minDistance + 1)
    let target : TilePos := ⟨center.x + (if roll % 2 == 0 then x else -x),
      center.y + (if roll / 2 % 2 == 0 then y else -y)⟩
    if world.contains target then
      let motion : EyeMotion := ⟨target.x * tileUnits + tileUnits / 2, target.y * tileUnits + tileUnits / 2, 0, 0⟩
      if !(world.tile target).solid then return motion
      fallback := motion
  return fallback

private def BrainCreeper.advance (creeper : BrainCreeper) (brain : BrainOfCthulhu) (player : Player) : BrainCreeper := Id.run do
  let m := creeper.motion
  if creeper.rush > 0 then return { creeper with motion := advanceMotion m, rush := creeper.rush - 1 }
  let center := brain.motion
  let dx := center.x - m.x
  let dy := center.y - m.y
  let far := dx^2 + dy^2 > (90 * (tileUnits / 16))^2
  let roll := (hash (creeper.id, brain.serial, brain.timer, m.x, m.y)).toNat
  if !far && roll % 100 == 0 then
    let (vx, vy) := (aimVelocity 128 (bodyCenterX player - m.x) (bodyCenterY player - m.y)).getD (0, 0)
    return { creeper with motion := advanceMotion { m with vx, vy }, rush := brainRushTicks }
  let (vx, vy) := (aimVelocity 128 dx dy).getD (0, 0)
  let steered := if far then { m with vx, vy }
    else { m with vx := approach m.vx vx 12, vy := approach m.vy vy 12 }
  return { creeper with motion := advanceMotion steered }

def advanceBrain (world : World) (player : Player) (brain : BrainOfCthulhu)
    (retreat : Bool) : Option BrainOfCthulhu := Id.run do
  if retreat || brain.phase == .retreat then
    let timer := if brain.phase == .retreat then brain.timer + 1 else 1
    let motion := advanceMotion { brain.motion with vx := 0, vy := min 144 (brain.motion.vy + 4) }
    if timer ≥ brainRetreatTicks || !motion.valid world 144 then return none
    return some { brain with motion, phase := .retreat, timer, warp := 0, creepers := #[] }
  let fade := brain.phase.fadeTicks
  let mut next := brain
  if brain.warp > 0 then
    let warp := brain.warp + 1
    let motion := if warp == fade then brain.teleport world player else brain.motion
    next := if warp > 2 * fade then { brain with motion, timer := 0, warp := 0, serial := (brain.serial + 1) % 65536 }
      else { brain with motion, warp }
  else
    let m := brain.motion
    let speed := if brain.phase == .shielded then 48 else 128
    let (vx, vy) := (aimVelocity speed (bodyCenterX player - m.x) (bodyCenterY player - m.y)).getD (0, 0)
    let motion := advanceMotion (if brain.phase == .shielded then { m with vx, vy }
      else { m with vx := approach m.vx vx 51, vy := approach m.vy vy 51 })
    let timer := brain.timer + 1
    let roll := (hash (player.centerTile, brain.serial, timer, (923 : Nat))).toNat
    let limit := if brain.phase == .shielded then 120 + roll % 180 else 60 + roll % 60
    next := { brain with motion, timer, warp := if timer > limit then 1 else 0 }
  let creepers := next.creepers.map (fun creeper => creeper.advance next player)
  return some { next with creepers }

end Lean4Terria
