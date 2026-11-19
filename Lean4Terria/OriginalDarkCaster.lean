import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalAngryBones
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalWallWork.Tables

namespace Lean4Terria.OriginalDarkCaster

open OriginalCollision (Grid Vector single tileIndex)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)
open OriginalGreenSlime (Slime)

inductive Kind where
  | caster
  | waterSphere
  deriving BEq, Repr

structure Enemy where
  kind : Kind
  body : Slime
  deriving Repr

def Enemy.npcType (enemy : Enemy) : Int :=
  if enemy.kind == .caster then 32 else 33

def Enemy.toCombatNpc (enemy : Enemy) (playerSlot : Nat := 0) : OriginalStarterMelee.Npc :=
  let n := enemy.body
  { active := n.active, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax, defense := n.defense
    knockBackResist := n.knockBackResist, justHit := n.justHit, noGravity := enemy.kind == .waterSphere
    noTileCollide := enemy.kind == .waterSphere
    immune := (n.immune[playerSlot]?.getD 0).toNat }

/-- Write back a landed hit. The style 8 branch of `StrikeNPC_Inner` jumps the
clock past the remaining charge opportunities and immediately retargets. -/
def Enemy.applyStrike (enemy : Enemy) (hit : OriginalStarterMelee.Npc) (players : Array OriginalGreenSlime.Player)
    (playerSlot : Nat := 0) (credited : Bool := true) : Enemy :=
  let n := enemy.body
  let n := { n with
    active := hit.active && hit.life > 0, life := hit.life, velocity := hit.velocity, justHit := hit.justHit
    immune := n.immune.set! playerSlot hit.immune, playerInteraction := n.playerInteraction || credited }
  let n := if enemy.kind == .caster then OriginalGreenSlime.targetClosest players { n with ai0 := 400 } else n
  { enemy with body := n }

/-- Both enemies are allocated by `NPC.NewNPC`, whose coordinates are the bottom center. -/
def spawn (netId x y : Int) : Option Enemy := do
  let kind ← if netId == 32 then some Kind.caster else if netId == 33 then some .waterSphere else none
  let caster := kind == .caster
  let (width, height, life) : Nat × Nat × Int := if caster then (18, 40, 50) else (16, 16, 1)
  let defense := if caster then 2 else 0
  return { kind, body := {
    position := ⟨single x - single width / 2, single (y - (height : Int))⟩
    width, height, life, lifeMax := life
    damage := 20, defDamage := 20, defense, defDefense := defense, scale := 1
    knockBackResist := if caster then 0.6 else 0
    value := if caster then 140 else 0
    npcSlots := if caster then 2 else 1
    alpha := if caster then 0 else 255
    color := ⟨0, 0, 0, 0⟩
    timeLeft := 937 } }

structure Arena extends OriginalGreenSlime.World where
  /-- `Main.MaxWorldViewSize`, used for the caster's firing gate. -/
  firingWidth : Int := 1920
  firingHeight : Int := 1200
  skeletronActive : Bool := false
  playerVelocity : Nat → Vector := fun _ => ⟨0, 0⟩

structure SphereSpawn where
  x : Int
  y : Int
  deriving BEq, Repr

def SphereSpawn.enemy (request : SphereSpawn) : Option Enemy :=
  spawn 33 request.x request.y

structure Step where
  enemy : Enemy
  random : UnifiedRandom
  dustPool : DustPool
  dust : Array DustParticle := #[]
  nearby : Array (Nat × Float32) := #[]
  sphere : Option SphereSpawn := none
  /-- Positions at which style 8 requests the Item8 sound. -/
  castSounds : Array Vector := #[]

private def rectX (v : Vector) : Int := v.x.toInt32.toInt
private def rectY (v : Vector) : Int := v.y.toInt32.toInt

private def clearance (grid : Grid) (x y : Int) : Bool := Id.run do
  if x < 1 || x + 1 ≥ grid.width || y < 4 || y - 1 ≥ (grid.height : Int) - 40 then
    return false
  for i in [(x - 1).toNat:(x + 2).toNat] do
    for j in [(y - 4).toNat:y.toNat] do
      let tile := grid.tileAt i j
      if !tile.actuated && tile.id.any (fun id => let rule := grid.rules id; rule.solid && !rule.solidTop) then
        return false
  return true

private def allowsPlayer (arena : Arena) (p : OriginalGreenSlime.Player) (x y : Int) : Bool :=
  if !p.active || p.dead || p.index ≥ 255 then true
  else
    let dx := (arena.playerVelocity p.index).x * 20
    let dy := (arena.playerVelocity p.index).y * 20
    let vx := dx.toInt32.toInt
    let vy := dy.toInt32.toInt
    let px := rectX p.position
    let py := rectY p.position
    let left := (x - 5) * 16
    let right := (x + 6) * 16
    let top := (y - 5) * 16
    let bottom := (y + 6) * 16
    right ≤ px + min 0 vx || px + (p.width : Int) + max 0 vx ≤ left ||
      bottom ≤ py + min 0 vy || py + (p.height : Int) + max 0 vy ≤ top

/-- Style 8's `AI_AttemptToFindTeleportSpot` for type 32. Applies the native
search order, wall and lava checks, clearance, and swept player exclusion. -/
def findTeleport (arena : Arena) (players : Array OriginalGreenSlime.Player) (npc : Slime) (target : OriginalGreenSlime.Player)
    (random : UnifiedRandom) : Option (Int × Int) × UnifiedRandom := Id.run do
  let ownX := tileIndex npc.center.x
  let ownY := tileIndex npc.center.y
  let tx := tileIndex target.center.x
  let ty := tileIndex target.center.y
  if 16 * ((ownX - tx).natAbs + (ownY - ty).natAbs) > 2000 then return (none, random)
  let mut stream := random
  for _ in [:100] do
    let (x, next) := stream.next (tx - 20) (tx + 21)
    let (start, next) := next.next (ty - 20) (ty + 21)
    stream := next
    if x < 0 || x ≥ arena.grid.width then continue
    for row in [(max 1 start).toNat:(min (ty + 20) (arena.grid.height : Int)).toNat] do
      let y : Int := row
      if (x - ownX).natAbs ≤ 1 && (y - ownY).natAbs ≤ 1 then continue
      let bottom := arena.grid.tileAt x.toNat row
      if bottom.actuated || !bottom.id.any (fun id => (arena.grid.rules id).solid) then continue
      let above := arena.grid.tileAt x.toNat (row - 1)
      if OriginalWallWork.Tables.wallDungeon above.wall then
        if above.liquid.any (·.kind == .lava) then continue
      else if !arena.skeletronActive then continue
      if clearance arena.grid x y && players.all (allowsPlayer arena · x y) then return (some (x, y), stream)
  return (none, stream)

private def inFiringRange (arena : Arena) (pos target : Vector) : Bool :=
  let left := rectX target - arena.firingWidth / 2 + 50
  let top := rectY target - arena.firingHeight / 2 + 50
  let x := rectX pos
  let y := rectY pos
  left ≤ x && x < left + arena.firingWidth - 100 &&
    top ≤ y && y < top + arena.firingHeight - 100

private def emit (step : Step) (request : DustRequest)
    (adjust : DustParticle → DustParticle) : Step :=
  let (pool, dust, random) := step.dustPool.create request step.random
  match dust with
  | none => { step with dustPool := pool, random }
  | some p =>
    let bank := request.typeId / 100
    let p := { p with frame.x := p.frame.x - 1000 * bank, frame.y := p.frame.y + 30 * bank }
    { step with dustPool := pool, dust := step.dust.push (adjust p), random }

private def teleDust (step : Step) (npc : Slime) : Step := Id.run do
  let request : DustRequest := {
    position := npc.position, width := npc.width, height := npc.height, typeId := 172, alpha := 100, scale := 1.5 }
  let mut step := { step with castSounds := step.castSounds.push npc.position }
  for _ in [:50] do
    step := emit step request fun p =>
      { p with velocity := ⟨p.velocity.x * 3, p.velocity.y * 3⟩, noGravity := true }
  return step

private def casterAI (arena : Arena) (players : Array OriginalGreenSlime.Player) (step : Step) : Step := Id.run do
  let n := OriginalGreenSlime.targetClosest players step.enemy.body
  let target := OriginalGreenSlime.playerAt players n.target
  let vx := n.velocity.x * 0.93
  let mut n := { n with
    velocity.x := if vx.abs < 0.1 then 0 else vx
    ai0 := if n.ai0 == 0 then 500 else n.ai0 }
  let mut step := step
  if n.ai2 != 0 && n.ai3 != 0 then
    step := teleDust step n
    let position : Vector := ⟨n.ai2 * 16 - single (n.width / 2) + 8, n.ai3 * 16 - single n.height⟩
    n := { n with position, velocity := ⟨0, 0⟩, ai2 := 0, ai3 := 0 }
    step := teleDust step n
  n := { n with ai0 := n.ai0 + 1 }
  if (n.ai0 == 100 || n.ai0 == 200 || n.ai0 == 300) && inFiringRange arena n.center target.center then
    n := { n with ai1 := 30, netUpdate := true }
  if n.ai0 ≥ 650 then
    let (dest, random) := findTeleport arena players n target step.random
    n := { n with ai0 := 1, netUpdate := true }
    if let some (x, y) := dest then
      n := { n with ai1 := 20, ai2 := single x, ai3 := single y }
    step := { step with random }
  if n.ai1 > 0 then
    n := { n with ai1 := n.ai1 - 1 }
    if n.ai1 == 25 then
      let sphere : SphereSpawn := ⟨rectX n.position + n.width / 2, rectY n.position - 8⟩
      step := { step with sphere := some sphere, castSounds := step.castSounds.push n.position }
  let (roll, random) := step.random.next 0 3
  step := { step with random }
  if roll != 0 then
    step := emit step {
      position := n.position + ⟨0, 2⟩, width := n.width, height := n.height
      typeId := 172, alpha := 100, scale := 0.9, speed := ⟨n.velocity.x * 0.2, n.velocity.y * 0.2⟩ } fun p =>
        { p with noGravity := true, velocity := ⟨p.velocity.x * 0.3, p.velocity.y * 0.2 - 1⟩ }
  return { step with enemy.body := n }

private def sphereAI (players : Array OriginalGreenSlime.Player) (step : Step) : Step := Id.run do
  let mut n := step.enemy.body
  if n.target == 255 then
    n := OriginalGreenSlime.targetClosest players n
    let target := (OriginalGreenSlime.playerAt players n.target).center
    let x := target.x - n.center.x
    let y := target.y - n.center.y
    let distance := (x * x + y * y).toFloat.sqrt.toFloat32
    let speed : Float32 := 6 / if distance > 0 then distance else 1
    n := { n with velocity := ⟨x * speed, y * speed⟩ }
  n := { n with timeLeft := min 100 n.timeLeft, despawnEncouraged := true }
  let mut step := step
  let request : DustRequest := {
    position := n.position + ⟨2, 2⟩, width := n.width - 4, height := n.height - 4,
    typeId := 172, alpha := 100, scale := 1.2 }
  let carry : Vector := ⟨n.velocity.x * 0.5, n.velocity.y * 0.5⟩
  for i in [:2] do
    let offset : Vector := ⟨n.velocity.x / 3 * single i, n.velocity.y / 3 * single i⟩
    for _ in [:3] do
      step := emit step request fun p =>
        { p with
          noGravity := true
          position := ⟨p.position.x - offset.x, p.position.y - offset.y⟩
          velocity := ⟨p.velocity.x * 0.1, p.velocity.y * 0.1⟩ + carry }
    let (roll, random) := step.random.next 0 5
    step := { step with random }
    if roll == 0 then
      step := emit step { request with scale := 0.6 } fun p =>
        { p with velocity := ⟨p.velocity.x * 0.25, p.velocity.y * 0.25⟩ + carry }
  return { step with enemy.body := { n with rotation := n.rotation + 0.4 * single n.direction } }

private def prepare (n : Slime) : Slime :=
  let step := (1 + n.velocity.x.abs / 3) * n.stepSpeed
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - step)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + step) else n.gfxOffY
  { n with
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay
    gfxOffY := max (-16) (min 16 gfx) }

/-- One ordinary Classic NPC slot update in a dry dungeon. The caller
allocates a returned water sphere immediately in the same shared NPC array. -/
def update (arena : Arena) (players : Array OriginalGreenSlime.Player) (random : UnifiedRandom) (enemy : Enemy)
    (dustPool : DustPool := { enabled := false }) : Except String Step := do
  let initial : Step := { enemy, random, dustPool }
  if !enemy.body.active then return initial
  let n := enemy.body
  if n.life ≤ 0 then throw "Caster or water sphere death belongs to the combat phase"
  if n.wet then throw "Caster or water sphere is already wet"
  let caster := enemy.kind == .caster
  let initial := { initial with enemy.body := prepare n }
  let step := if caster then casterAI arena players initial else sphereAI players initial
  let mut n := step.enemy.body
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  if n.velocity.x.abs < 0.005 then n := { n with velocity.x := 0 }
  if caster then
    let (gravity, maxFall) := OriginalGreenSlime.gravityFor arena.toWorld enemy.body
    n := { n with velocity.y := min maxFall (n.velocity.y + gravity) }
    if (OriginalPlayerWet.wetCollision arena.grid n.position n.width n.height).wet then
      throw "Dark Caster entered liquid"
    n := OriginalGreenSlime.updateCollision arena.grid gravity n
    n := { n with
      frameY := if n.velocity.y != 0 then 2 else if n.ai1 > 0 then 1 else 0
      spriteDirection := if n.velocity.y == 0 && (n.direction == 1 || n.direction == -1)
        then n.direction else n.spriteDirection }
  else
    n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  let (checked, nearby) := OriginalGreenSlime.checkActive arena.toWorld players n
  return { step with
    enemy.body := { checked with netUpdate := false, justHit := false }
    nearby := if caster then nearby else #[] }

/-! ## Ordinary Classic combat -/

def Enemy.contactSource (enemy : Enemy) : Option (Int × String) :=
  if !enemy.body.active || enemy.body.damage ≤ 0 then none else
  some (enemy.body.damage, if enemy.kind == .caster then "Dark Caster" else "Water Sphere")

/-- Both kinds use the same ordinary player-hurt branch as skeletons.
Contact leaves the NPC slot unchanged, including a water sphere's flight. -/
def contact (enemy : Enemy) (body : OriginalPlayerCollision.Body)
    (health : OriginalPlayerLiquidHealth.Health) (defense : Int)
    (pool : DustPool) (random : UnifiedRandom)
    (text : Option OriginalCombatTextChannel.Channel := none)
    (client : OriginalPlayerHurt.Client := {}) : Option OriginalAngryBones.ContactHit :=
  OriginalAngryBones.contactNpc enemy.toCombatNpc enemy.body.damage body health defense pool random text client

def bannerId : Nat := 68
def bannerItem : UInt16 := 1682

/-- Water spheres share the caster's banner for effects, but the NPC death
tally excludes `NPCID.Sets.ProjectileNPC`. -/
def Enemy.recordBannerKill (enemy : Enemy) (credited : Bool)
    (banner : OriginalAngryBones.BannerProgress) : OriginalAngryBones.BannerProgress :=
  if enemy.kind == .caster && credited then banner.recordKill else banner

open OriginalCombat.Loot (Context Result)

private def appendDrop (env : Context) (position : Vector) (item : UInt16) (stack : Nat)
    (result : Result) : Result :=
  let (prefixId, random) := OriginalRecipeCatalog.rollNaturalPrefix item result.random
  let (drop, random) := OriginalItems.rollDropAt position
    { OriginalItems.Stack.ofItem item stack with prefixId } random env.grid
  { drops := result.drops.push drop, random }

private def commonDrop (env : Context) (position : Vector) (item : UInt16)
    (chance : Int) (result : Result) : Bool × Result :=
  let (roll, random) := result.random.next 0 chance
  if roll != 0 then (false, { result with random })
  else
    let (_, random) := random.next 1 2
    (true, appendDrop env position item 1 { result with random })

private def casterDrops (env : Context) (position : Vector) (random : UnifiedRandom) : Result := Id.run do
  let mut result : Result := { random }
  for (item, chance) in #[(959, 450), (1307, 300), (5632, 150)] do
    result := (commonDrop env position item chance result).2
  for (item, chance) in #[(932, 250), (3095, 100), (327, 65)] do
    let (dropped, next) := commonDrop env position item chance result
    if dropped then return next
    result := next
  let (_, random) := result.random.next 0 1
  let (stack, random) := random.next 1 4
  return appendDrop env position 154 stack.toNat { result with random }

/-- The normal zero-luck, pre-Hardmode dungeon `NPCLoot` after hit effects,
outside global drop conditions. The sphere still uses the coin and recovery
paths with value zero and recovery disabled, advancing the native stream. -/
def dropLoot (env : Context) (enemy : Enemy) (random : UnifiedRandom) : Result :=
  let n := enemy.body
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let specific := if enemy.kind == .caster then casterDrops env position random else { random }
  let coins := OriginalCombat.Loot.dropMoneyAt env position n.value specific
  OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) coins

/-- Type 32 uses the skeleton effect leaf. Type 33 instead produces twenty
pairs of blue particles with the flight velocity as input to their birth. -/
def hitEffects (enemy : Enemy) (pool : DustPool) (freeGoreSlots : Nat)
    (direction damage : Int) (hit : OriginalStarterMelee.Npc) (random : UnifiedRandom) :
    OriginalAngryBones.HitEffects := Id.run do
  if enemy.kind == .caster then return OriginalAngryBones.hitEffects pool freeGoreSlots direction damage hit random
  let request : DustRequest := {
    position := hit.position, width := hit.width, height := hit.height
    typeId := 172, alpha := 100, scale := 1.2
    speed := ⟨-hit.velocity.x * 0.2, -hit.velocity.y * 0.2⟩ }
  let mut step : Step := { enemy, dustPool := pool, random }
  for _ in [:20] do
    step := emit step request fun p =>
      { p with noGravity := true, velocity := ⟨p.velocity.x * 2, p.velocity.y * 2⟩ }
    step := emit step { request with scale := 0.6 } fun p =>
      { p with velocity := ⟨p.velocity.x * 2, p.velocity.y * 2⟩ }
  return { dust := step.dust, pool := step.dustPool, random := step.random }

end Lean4Terria.OriginalDarkCaster
