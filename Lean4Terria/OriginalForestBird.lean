import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalStarterMelee

/-!
The dry Classic 1.4.5.8 update for Bird (74) and Blue Jay (297). The caller supplies
the shared NPC random stream and the current clock, before the frame's time update.
-/

namespace Lean4Terria.OriginalForestBird

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Player Rng World)

inductive Kind where
  | bird
  | blueJay
  deriving BEq, Repr

def Kind.typeId : Kind → Int
  | .bird => 74
  | .blueJay => 297

def Kind.ofTypeId : Int → Option Kind
  | 74 => some .bird
  | 297 => some .blueJay
  | _ => none

def Kind.textureName (kind : Kind) : String := s!"NPC_{kind.typeId}"

structure Bird where
  kind : Kind
  active : Bool := true
  position : Vector
  velocity : Vector := ⟨0, 0⟩
  oldPosition : Vector := ⟨0, 0⟩
  oldVelocity : Vector := ⟨0, 0⟩
  width : Nat := 14
  height : Nat := 14
  life : Int := 5
  lifeMax : Int := 5
  npcSlots : Float32 := 0.4
  timeLeft : Int := 0
  friendly : Bool := true
  catchableNPCTempImmunityCounter : Int := 90
  releaseOwner : Nat := 255
  direction : Int := 0
  directionY : Int := 1
  oldDirection : Int := 0
  oldDirectionY : Int := 0
  spriteDirection : Int := -1
  target : Int := 255
  oldTarget : Int := 255
  ai0 : Float32 := 0
  /-- `localAI[3]` guards the one-time overhead projectile. -/
  dropped : Bool := false
  noGravity : Bool := false
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  stairFall : Bool := false
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  rotation : Float32 := 0
  frameY : Nat := 0
  frameCounter : Float := 0
  soundDelay : Int := 0
  netUpdate : Bool := true
  justHit : Bool := false
  despawnEncouraged : Bool := false
  deriving Repr

def Bird.textureFrame (bird : Bird) : Nat × Nat × Nat × Nat :=
  (0, 24 * bird.frameY, 30, 24)

def spawn (world : World) (kind : Kind) (x y : Int) : Bird :=
  { kind, position := ⟨single x - 7, single y - 14⟩
    timeLeft := (Float.ofInt world.activeTime * 1.25).toInt64.toInt }

/-- The fields shared by target selection, dry tile collision, and the active-range check. -/
private def Bird.common (n : Bird) : OriginalGreenSlime.Slime :=
  { active := n.active, position := n.position, velocity := n.velocity,
    width := n.width, height := n.height, direction := n.direction, directionY := n.directionY,
    oldTarget := n.oldTarget, target := n.target, oldDirection := n.oldDirection,
    oldDirectionY := n.oldDirectionY, lifeMax := n.lifeMax, collideX := n.collideX, collideY := n.collideY,
    npcSlots := n.npcSlots, timeLeft := n.timeLeft,
    netUpdate := n.netUpdate, stairFall := n.stairFall, gfxOffY := n.gfxOffY, stepSpeed := n.stepSpeed,
    despawnEncouraged := n.despawnEncouraged }

private def rangeThreat (n : Bird) (p : Player) : Bool :=
  let x := n.position.x.toInt32.toInt
  let y := n.position.y.toInt32.toInt
  let px := p.position.x.toInt32.toInt
  let py := p.position.y.toInt32.toInt
  px < x + n.width + 100 && x - 100 < px + p.width &&
    py < y + n.height + 100 && y - 100 < py + p.height

private def floorAhead (grid : Grid) (n : Bird) : Option Nat := Id.run do
  let x := tileIndex (n.position.x + single (n.width / 2 : Nat)) + n.direction
  let bottom := tileIndex (n.position.y + single n.height)
  for distance in [:15] do
    let y := bottom + (distance : Int)
    if 0 ≤ x && x < grid.width && 0 ≤ y && y < grid.height then
      let tile := grid.tileAt x.toNat y.toNat
      if (tile.liquid.any (·.amount > 0)) ||
          (!tile.actuated && tile.id.any (fun id => (grid.rules id).solid)) then
        return some distance
  return none

private def steerHorizontally (direction : Int) (vx : Float32) : Float32 :=
  if direction == -1 && vx > -3 then
    let vx := vx - 0.1
    let vx := if vx > 3 then vx - 0.1 else if vx > 0 then vx - 0.05 else vx
    if vx < -3 then -3 else vx
  else if direction == 1 && vx < 3 then
    let vx := vx + 0.1
    let vx := if vx < -3 then vx + 0.1 else if vx < 0 then vx + 0.05 else vx
    if vx > 3 then 3 else vx
  else vx

private def flying (grid : Grid) (n : Bird) : Bird := Id.run do
  let mut n := n
  if n.collideX then
    let direction := -n.direction
    let vx := n.oldVelocity.x * -0.5
    let vx := if direction == -1 && vx > 0 && vx < 2 then 2
      else if direction == 1 && vx < 0 && vx > -2 then -2 else vx
    n := { n with direction, velocity.x := vx }
  if n.collideY then
    let vy := n.oldVelocity.y * -0.5
    let vy := if vy > 0 && vy < 1 then 1 else if vy < 0 && vy > -1 then -1 else vy
    n := { n with velocity.y := vy }
  let vx := steerHorizontally n.direction n.velocity.x
  let vy := match floorAhead grid n with
    | none => n.velocity.y + 0.05
    | some distance =>
      let vy := n.velocity.y - 0.1
      if distance < 5 then vy - 0.2 else vy
  return { n with velocity := ⟨vx, if vy > 2 then 2 else if vy < -4 then -4 else vy⟩ }

private def ai (world : World) (players : Array Player) (n : Bird) : Bird := Id.run do
  if n.ai0 == 0 then
    let faced := OriginalGreenSlime.targetClosest (players.map ({ · with noAggro := false })) n.common
    let n := { n with target := faced.target, direction := faced.direction, directionY := faced.directionY,
                       netUpdate := faced.netUpdate, noGravity := false }
    if n.releaseOwner != 255 || n.velocity.x != 0 || n.velocity.y < 0 || n.velocity.y.toFloat > 0.3 then
      return { n with ai0 := 1, direction := -n.direction, netUpdate := true }
    if rangeThreat n (OriginalGreenSlime.playerAt players n.target) || n.life < n.lifeMax then
      return { n with ai0 := 1, direction := -n.direction, velocity.y := n.velocity.y - 6, netUpdate := true }
    return n
  let n := { n with noGravity := true }
  if n.ai0 == 2 then
    let grounded := n.velocity.y == 0
    let vy := n.velocity.y + 0.05
    return { n with
      ai0 := if grounded then 0 else n.ai0
      velocity := ⟨if grounded then 0 else n.velocity.x * 0.98, if vy > 2 then 2 else vy⟩ }
  if (OriginalGreenSlime.playerAt players n.target).dead then return n
  return flying world.grid n

private def findFrame (n : Bird) : Bird :=
  let n := { n with spriteDirection := n.direction, rotation := n.velocity.x * 0.1 }
  if n.velocity.x == 0 && n.velocity.y == 0 then
    { n with frameY := 4, frameCounter := 0 }
  else
    let counter := n.frameCounter + 1
    let row := if counter ≥ 4 then n.frameY + 1 else n.frameY
    { n with frameY := if row ≥ 4 then 0 else row, frameCounter := if counter ≥ 4 then 0 else counter }

structure Chirp where
  position : Vector
  /-- Style of Terraria's sound type 32. -/
  style : Nat
  /-- LegacySoundPlayer draws a pitch only when it creates a new sound instance. -/
  pitch : Option Float32 := none
  deriving Repr

private def critterSounds (world : World) (time : Float) (n : Bird) (rng : Rng) : Option Chirp × Rng :=
  if !world.dayTime || time ≥ 18000 then (none, rng)
  else
    let (chance, rng) := rng.next 400
    if chance != 0 then (none, rng)
    else match n.kind with
      | .bird =>
        let (choice, rng) := rng.next 3
        (some { position := n.position, style := if choice == 0 then 18 else 14 }, rng)
      | .blueJay => (some { position := n.position, style := 16 }, rng)

structure DropTarget where
  luck : Float32 := 0
  isCenx : Bool := false

/-- Original `Player.FindClosest` in the two bird types' overhead projectile branch. -/
private def closestPlayer (players : Array Player) (center : Vector) : Option Player := Id.run do
  let mut first := none
  let mut closest := none
  let mut shortest : Float32 := 0
  for player in players.qsort (·.index < ·.index) do
    if !player.active || player.index ≥ 255 then continue
    if first.isNone then first := some player
    if player.dead then continue
    let px := player.position.x + single (player.width / 2 : Nat)
    let py := player.position.y + single (player.height / 2 : Nat)
    let distance := (px - center.x).abs + (py - center.y).abs
    if closest.isNone || distance < shortest then
      closest := some player
      shortest := distance
  return closest.or first

/-- Style 24 can spawn the zero-damage projectile 1080 once when a player is directly
below a flying bird. The caller owns projectile slots and their updates. -/
private def overheadDrop (grid : Grid) (players : Array Player) (target : Nat → DropTarget)
    (n : Bird) (rng : Rng) : Option Vector × Rng := Id.run do
  if n.ai0 != 1 || n.dropped then return (none, rng)
  let center := n.position + ⟨single n.width / 2, single n.height / 2⟩
  let some player := closestPlayer players center | return (none, rng)
  if center.x < player.position.x || center.x > player.position.x + single player.width ||
      player.position.y ≤ n.position.y + single n.height then return (none, rng)
  let config := target player.index
  if config.luck ≥ 0 && !config.isCenx then return (none, rng)
  if !OriginalStarterMelee.LineOfSight.canHit grid ⟨n.position, n.width, n.height⟩
      ⟨player.position, player.width, player.height⟩ then return (none, rng)
  let mut rng := rng
  let mut rolled := false
  if config.luck < 0 then
    let (chance, next) := rng.sample
    rng := next
    if chance.toFloat32 < -config.luck then
      let (choice, next) := rng.next 5
      rng := next
      rolled := choice == 0
  if rolled || config.isCenx then
    return (some ⟨player.position.x + single player.width / 2, center.y⟩, rng)
  return (none, rng)

structure Step where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  bird : Bird
  rng : Rng
  nearby : Array (Nat × Float32) := #[]
  noSpawnCycle : Bool := false
  chirp : Option Chirp := none
  /-- The world-pixel origin passed to `NewProjectile` for type 1080. -/
  overheadProjectile : Option Vector := none
  unsupported : Option String := none

/-- The contact strike portion, before tile movement. The returned text state
must be committed before the next NPC slot is updated. -/
def strikeByHostileWithText (n : Bird) (damage : Int) (attackerX : Float32)
    (loot : OriginalCombat.Loot.Context) (rng : Rng) (text : OriginalNpcCombatText.State) :
    Bird × Rng × OriginalNpcCombatText.State := Id.run do
  if text.boundary.isSome then return (n,rng,text)
  let text := {text with fromPlayer := 255, fromNet := false}
  let npc : OriginalStarterMelee.Npc := {
    position := n.position, velocity := n.velocity, width := n.width, height := n.height
    life := n.life, lifeMax := n.lifeMax, defense := 0, knockBackResist := 0.8, noGravity := n.noGravity, friendly := n.friendly }
  let direction := if attackerX ≤ npc.box.center.x then 1 else -1
  let (rolled, random) := OriginalStarterMelee.damageVar damage ⟨rng.table, rng.inext⟩
  let strike := OriginalStarterMelee.strikeNpcWithText npc rolled 6 direction false random text
  let struck := strike.npc
  let random := strike.random
  if strike.text.boundary.isSome then
    let (table, cursor) := random.state
    return (n, ⟨table, cursor⟩, strike.text)
  let (n, random) := if struck.life ≤ 0 then
      -- Both bird types create one gore particle on death, then run the ordinary loot rolls.
      let random := ((random.next 10 31).2.next (-20) 21).2
      let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
      let money := OriginalCombat.Loot.dropMoneyAt loot position 0 { random }
      let drops := OriginalCombat.Loot.dropHealsAt loot position false money
      ({ n with active := false, life := 0, velocity := struck.velocity }, drops.random)
    else ({ n with life := struck.life, velocity := struck.velocity, justHit := true }, random)
  let (table, cursor) := random.state
  return (n, ⟨table, cursor⟩, strike.text)

/-- The dry NPC stage for one shared slot. The chirp event uses the pre-motion position. `playChirp`
reports whether the sound backend creates a fresh instance, including its spatial and
already-playing checks. The headless default leaves the pitch draw disabled.
`hostileHit` queries the live slot array before tile movement. -/
def update (world : World) (players : Array Player) (rng : Rng) (n : Bird) (time : Float)
    (dropTarget : Nat → DropTarget := fun _ => {})
    (playChirp : Chirp → Bool := fun _ => false)
    (hostileHit : Bird → Option (Int × Float32) := fun _ => none)
    (loot : OriginalCombat.Loot.Context := {})
    (text : OriginalNpcCombatText.State := {fromPlayer := 255}) : Step := Id.run do
  if !n.active || text.boundary.isSome then return { bird := n, rng, text }
  if n.wet || (OriginalPlayerWet.wetCollision world.grid n.position n.width n.height).wet then
    return { bird := n, rng, text, unsupported := some "Bird liquid collision" }
  let (chirp, rng) := critterSounds world time n rng
  let (chirp, rng) := match chirp with
    | some call =>
      if playChirp call then
        let (pitch, next) := rng.nextRange (-70) 26
        (some { call with pitch := some (single pitch * 0.01) }, next)
      else (chirp, rng)
    | none => (none, rng)
  if n.life ≤ 0 then
    return { bird := n, rng, chirp, text, unsupported := some "Bird death" }
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world n.common
  let mut n := { n with
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay }
  let ease := 1 + n.velocity.x.abs / 3
  let offset := if n.gfxOffY > 0 then max 0 (n.gfxOffY - ease * n.stepSpeed)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + ease * n.stepSpeed) else 0
  n := { n with gfxOffY := max (-16) (min 16 offset) }
  let (overheadProjectile, rng) := overheadDrop world.grid players dropTarget n rng
  if overheadProjectile.isSome then n := { n with dropped := true }
  n := ai world players n
  n := { n with
    friendly := n.catchableNPCTempImmunityCounter > 0
    catchableNPCTempImmunityCounter := max 0 (n.catchableNPCTempImmunityCounter - 1) }
  if !n.noGravity then
    n := { n with velocity.y := min maxFall (n.velocity.y + gravity) }
  if n.velocity.x.toFloat > -0.005 && n.velocity.x.toFloat < 0.005 then
    n := { n with velocity.x := 0 }
  -- NPC.UpdateNPC still runs tile collision after a fatal strike.
  let (hurtBird, rng, text) := match hostileHit n with
    | some (damage, attackerX) => strikeByHostileWithText n damage attackerX loot rng text
    | none => (n, rng, text)
  if text.boundary.isSome then return {bird := hurtBird, rng, text, chirp, overheadProjectile}
  n := hurtBird
  let moved := OriginalGreenSlime.updateCollision world.grid gravity n.common
  n := { n with
    position := moved.position, velocity := moved.velocity, oldPosition := moved.oldPosition
    oldVelocity := moved.oldVelocity, oldDirection := moved.oldDirection, collideX := moved.collideX
    collideY := moved.collideY, gfxOffY := moved.gfxOffY, stepSpeed := moved.stepSpeed, stairFall := moved.stairFall }
  n := findFrame n
  if !n.active then
    return {
      bird := { n with justHit := false, netUpdate := false }
      rng
      text
      noSpawnCycle := true
      chirp
      overheadProjectile }
  let (checked, nearby) := OriginalGreenSlime.checkActive world players n.common
  n := { n with
    active := checked.active, timeLeft := checked.timeLeft
    despawnEncouraged := checked.despawnEncouraged, justHit := false, netUpdate := false }
  return {
    bird := n, rng, text, nearby := if n.releaseOwner == 255 then nearby else #[]
    noSpawnCycle := !n.active, chirp, overheadProjectile }

end Lean4Terria.OriginalForestBird
