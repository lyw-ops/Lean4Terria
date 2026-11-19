import Lean4Terria.OriginalBankMining
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalGhostSpawn
import Lean4Terria.OriginalItems
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalTileRules
import Lean4Terria.UnifiedRandom

/-!
# Local Classic death effects

The ordinary 1.4.5.8 PvE path samples the corpse velocities, creates the Classic
coin drops, then launches a gravestone with the pre-death coin value. The inputs
are the body at `KillMe`, the hit direction passed by `Hurt`, and the player RNG
after the damage and combat-text rolls. The fixtures use a dry ordinary world
with the death dust outside the active viewport.
-/

namespace Lean4Terria.OriginalPlayerDeath

open OriginalCollision (Vector single tileIndex)
open OriginalItems (Drop Inventory WorldItems)
open OriginalMining (Cell HitCache WorldRules)
open OriginalPlayerCollision (Body)
open OriginalWorld (Sign Terrain Tile)

structure CorpseVelocities where
  head : Vector
  body : Vector
  legs : Vector
  deriving BEq, Repr

/-- The normal-world velocities at `KillMe` IL_04f8–IL_05bc. -/
def corpseVelocities (hitDirection : Int) (random : UnifiedRandom) :
    CorpseVelocities × UnifiedRandom :=
  let (headY, random) := random.next (-40) (-10)
  let (bodyY, random) := random.next (-40) (-10)
  let (legsY, random) := random.next (-40) (-10)
  let (headX, random) := random.next (-20) 21
  let (bodyX, random) := random.next (-20) 21
  let (legsX, random) := random.next (-20) 21
  let push := single (2 * hitDirection)
  let velocity (x y : Int) : Vector := ⟨single x * 0.1 + push, single y * 0.1⟩
  (⟨velocity headX headY, velocity bodyX bodyY, velocity legsX legsY⟩, random)

/-- The dead player's head, body and legs as `Player.UpdateDead` moves them, and the fade counter.
Positions are offsets from the player's position and rotations are in radians; `Player.Spawn`
zeroes them (IL_0122..0159), so a first death starts from zero. The window does not draw the
parts yet: this is the state the drawing needs, kept exact. -/
structure CorpseParts where
  headPosition : Vector := ⟨0, 0⟩
  bodyPosition : Vector := ⟨0, 0⟩
  legPosition : Vector := ⟨0, 0⟩
  headVelocity : Vector
  bodyVelocity : Vector
  legVelocity : Vector
  headRotation : Float32 := 0
  bodyRotation : Float32 := 0
  legRotation : Float32 := 0
  /-- `Player.immuneAlpha`: 0 at KillMe (IL_061d), +2 every dead update up to 255. -/
  immuneAlpha : Int := 0
  deriving BEq, Repr

def CorpseParts.ofVelocities (corpse : CorpseVelocities) : CorpseParts :=
  { headVelocity := corpse.head, bodyVelocity := corpse.body, legVelocity := corpse.legs }

/-- `Player.UpdateDead` IL_0318..0450, in its order: the fade, positions by the old velocities,
rotations by a tenth of the horizontal velocity, then gravity 0.1 and horizontal drag 0.99. -/
def CorpseParts.updateDead (p : CorpseParts) : CorpseParts :=
  let fall := fun (v : Vector) => (⟨v.x * 0.99, v.y + 0.1⟩ : Vector)
  { headPosition := p.headPosition + p.headVelocity
    bodyPosition := p.bodyPosition + p.bodyVelocity
    legPosition := p.legPosition + p.legVelocity
    headRotation := p.headRotation + p.headVelocity.x * 0.1
    bodyRotation := p.bodyRotation + p.bodyVelocity.x * 0.1
    legRotation := p.legRotation + p.legVelocity.x * 0.1
    headVelocity := fall p.headVelocity
    bodyVelocity := fall p.bodyVelocity
    legVelocity := fall p.legVelocity
    immuneAlpha := min 255 (p.immuneAlpha + 2) }

/-- Stream advance from the low-density type-5 allocations in
`KillMe_DustExplosion`, after the corpse rolls. An ordinary death submits
100 attempts and a visible, empty pool accepts them all. Each accepted
allocation samples position X/Y, velocity X/Y, frame and scale. At higher
occupancy the caller also needs the allocator's density checks. -/
def consumeBloodDustAllocations (allocations : Nat) (random : UnifiedRandom) : UnifiedRandom :=
  (List.range (6 * allocations)).foldl (fun rng _ => rng.nextInt.2) random

/-- `Player.KillMe_DustExplosion` for the ordinary player (not stoned, no frost or bone armor):
100 calls `Dust.NewDust(position, width, height, 5, 2 * hitDirection, -2, 0, default, 1)` in the
real shared pool. The three writes after each call multiply velocity and scale by 1 and `fadeIn`
by 0, which leaves an ordinary particle unchanged. A refused call (pool full, outside the active
viewport, density) creates nothing and draws only what `NewDust` itself drew. -/
def dustExplosion (position : Vector) (width height : Nat) (hitDirection : Int)
    (pool : OriginalForestGuideArrow.DustPool) (random : UnifiedRandom) :
    Array OriginalForestGuideArrow.DustParticle × UnifiedRandom := Id.run do
  let request : OriginalForestGuideArrow.DustRequest := {
    position, width, height, typeId := 5, speed := ⟨single (2 * hitDirection), -2⟩ }
  let mut pool := pool
  let mut random := random
  let mut particles := #[]
  for _ in [:100] do
    let (next, particle, after) := pool.create request random
    pool := next
    random := after
    if let some particle := particle then particles := particles.push particle
  return (particles, random)

/-- `Lang.CreateDeathMessage` for `PlayerDeathReason.ByOther(index)`: after the generic category roll
(made for every reason, IL_00bf) the reasons with numbered texts roll which one. Returns the
localization key. 0 fell (9), 1 drowned (7), 2 lava (5), 7 suffocated (2), 8 burned (4); the other
indices the session can reach have a single text. -/
def otherDeathTextKey (index : Nat) (random : UnifiedRandom) : String × UnifiedRandom :=
  let numbered (stem : String) (count : Nat) : String × UnifiedRandom :=
    let (roll, random) := random.next 0 count
    (s!"{stem}{roll + 1}", random)
  match index with
  | 0 => numbered "DeathText.Fell_" 9
  | 1 => numbered "DeathText.Drowned_" 7
  | 2 => numbered "DeathText.Lava_" 5
  | 3 => ("DeathText.Default", random)
  | 4 => ("DeathText.Slain", random)
  | 6 => ("DeathText.Stabbed", random)
  | 7 => numbered "DeathText.Suffocated_" 2
  | 8 => numbered "DeathText.Burned_" 4
  | 9 => ("DeathText.Poisoned", random)
  | _ => ("DeathText.Slain", random)

/-- `Player.PlayDeathSound` for the ordinary player (no voice override, not a Don't Starve world):
`SoundEngine.PlaySound(5, position, 1)` — positioned even for the local player, one asset, no draw.
Returns the canonical request and, when it is heard, the resolved instance (variation 0, pitch 0). -/
def playDeathSound (context : OriginalTileHitEffects.Context) (position : Vector) :
    OriginalTileHitEffects.SoundRequest × Option OriginalTileHitEffects.PlayedSound := Id.run do
  let request : OriginalTileHitEffects.SoundRequest :=
    {typeId := 5, x := position.x.toInt32.toInt, y := position.y.toInt32.toInt, style := 1}
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then return (request, none)
  if context.generating || context.netMode == 2 then return (request, none)
  let dx := single request.x - context.cameraCenter.x
  let dy := single request.y - context.cameraCenter.y
  let distance := Float32.sqrt (dx * dx + dy * dy)
  if !(distance < context.attenuationDistance) then return (request, none)
  let clamp := fun (lo hi value : Float32) => if value < lo then lo else if value > hi then hi else value
  let pan := clamp (-1) 1 (dx / (context.maxWorldViewWidth * 0.5))
  let volume := min 1 (clamp 0 1 (1 - distance / context.attenuationDistance) * context.soundVolume)
  if !(volume > 0) then return (request, none)
  return (request, some {request, variation := 0, volume := volume * request.volumeScale, pan, pitch := 0})

/-- Sample the index in the loaded `DeathTextGeneric` category after the
death dust and before any coin drops. An absent category leaves the RNG as is. -/
def deathTextIndex (categorySize : Nat) (random : UnifiedRandom) : Option Nat × UnifiedRandom :=
  if categorySize == 0 then (none, random)
  else
    let (index, random) := random.next 0 categorySize
    (some index.toNat, random)

structure ClassicCoinDrops where
  inventory : Inventory
  drops : Array Drop := #[]
  carriedCopper : Nat := 0
  lostCopper : Nat := 0
  random : UnifiedRandom

/-- `DropCoins` traverses the player's carried slots in order. In Classic the
retained count is rounded down independently for each stack. -/
def dropClassicCoins (body : Body) (inventory : Inventory) (random : UnifiedRandom) :
    ClassicCoinDrops := Id.run do
  let position : Vector :=
    ⟨single (body.position.x.toInt32.toInt + (body.width / 2 : Nat) - 8),
     single (body.position.y.toInt32.toInt + (body.height / 2 : Nat) - 8)⟩
  let mut result : ClassicCoinDrops := { inventory, random }
  for index in [:inventory.size] do
    let some stack := inventory[index]! | continue
    if !stack.isCoin || stack.count == 0 then continue
    let retained := stack.count / 2
    let dropped := stack.count - retained
    let copperPerCoin := 100 ^ (stack.item.toNat - 71)
    let (vx, random) := result.random.next (-20) 21
    let (vy, random) := random.next (-20) 1
    let drop := OriginalItems.droppedStack position ⟨single vx * 0.2, single vy * 0.2⟩
      { stack with count := dropped, favorite := false }
    result := {
      inventory := result.inventory.set! index (if retained == 0 then none else some { stack with count := retained })
      drops := result.drops.push drop
      carriedCopper := result.carriedCopper + stack.count * copperPerCoin
      lostCopper := result.lostCopper + dropped * copperPerCoin
      random }
  return result

structure Gravestone where
  projectileType : UInt16
  position : Vector
  velocity : Vector
  rotation : Float32 := 0
  direction : Int := 1
  timeLeft : Nat := 3600
  inscription : String := ""
  /-- `Entity.wet` and its kind flags, as `NewProjectile` and every `Projectile.Update` keep them. -/
  wet : Bool := false
  lavaWet : Bool := false
  honeyWet : Bool := false
  shimmerWet : Bool := false
  wetCount : Nat := 0
  deriving BEq, Repr

/-- `DropTombstone` IL_000b–IL_01ad in an ordinary world. The bias uses
the damage direction. The stored projectile is 24 by 24 pixels. -/
def throwGravestone (body : Body) (hitDirection : Int) (carriedCopper : Nat)
    (random : UnifiedRandom) (inscription : String := "") : Gravestone × UnifiedRandom := Id.run do
  let (initial, random) := random.next (-35) 36
  let mut jitter := single initial * 0.1
  let mut stream := random
  while -2 < jitter && jitter < 2 do
    let (adjustment, next) := stream.next (-30) 31
    jitter := jitter + single adjustment * 0.1
    stream := next
  let (normal, random) := stream.next 0 6
  let (kind, random) := if carriedCopper > 100000 then
      let (gold, next) := random.next 0 5
      (527 + gold, next)
    else (if normal == 0 then 43 else 200 + normal, random)
  let (vx, random) := random.next 10 30
  let (vy, random) := random.next (-40) (-20)
  let position : Vector :=
    ⟨body.position.x + single (body.width / 2 : Nat) - 12,
     body.position.y + single (body.height / 2 : Nat) - 12⟩
  let velocity : Vector := ⟨single vx * 0.1 * single hitDirection + jitter, single vy * 0.1⟩
  ({ projectileType := kind.toNat.toUInt16, position, velocity, inscription }, random)

/-- `DropTombstone` joins the resolved death message and the already formatted
local date (English `MMMM d, yyy`, other cultures `D`) with one line feed. -/
def gravestoneInscription (deathMessage localDate : String) : String :=
  deathMessage ++ "\n" ++ localDate

/-- AI style 17 adjusts velocity and rotation before attempting to place the
two-by-two tile object and before normal projectile movement. -/
def Gravestone.advanceAI (grave : Gravestone) : Gravestone :=
  let vx := if grave.velocity.y == 0 then grave.velocity.x * 0.98 else grave.velocity.x
  { grave with velocity := ⟨vx, grave.velocity.y + 0.2⟩, rotation := grave.rotation + vx * 0.1 }

/-- The origin for tile 85 in AI style 17, one tile below the object's top left. -/
def Gravestone.placementOrigin (grave : Gravestone) : Int × Int :=
  (tileIndex (grave.position.x + 12), tileIndex (grave.position.y + 24 - 4))

def Gravestone.tileStyle (grave : Gravestone) : Nat :=
  let kind := grave.projectileType.toNat
  if 201 ≤ kind && kind ≤ 205 then kind - 200
  else if 527 ≤ kind && kind ≤ 531 then 6 + (kind - 527)
  else 0

structure GravestonePlacement where
  /-- The upper-left tile, also the coordinates stored by `Sign.ReadSign`. -/
  x : Nat
  y : Nat
  style : Nat
  inscription : String
  deriving BEq, Repr

/-- The four cells and horizontal style frames that `TileObject.Place` writes. -/
def GravestonePlacement.frames (placement : GravestonePlacement) : Array (Nat × Nat × Int × Int) :=
  let x := placement.x
  let y := placement.y
  let frameX : Int := 36 * placement.style
  #[(x, y, frameX, 0), (x, y + 1, frameX, 18),
    (x + 1, y, frameX + 18, 0), (x + 1, y + 1, frameX + 18, 18)]

private def bottomAnchor (grid : OriginalCollision.Grid) (tile : Tile) : Bool := Id.run do
  let some id := tile.id | return false
  if tile.actuated then return false
  let rules := grid.rules id
  if rules.platform then
    let frame := (tile.frame.getD (0, 0)).1.tdiv 18
    return tile.shape != .halfBlock &&
      ((0 ≤ frame && frame ≤ 7) || (12 ≤ frame && frame ≤ 16) || frame == 25 || frame == 26)
  if rules.solid && rules.solidTop then return true
  if rules.solid then
    if tile.shape == .full && !OriginalTileRules.preventsSolidAnchor id then return true
    if tile.shape == .slope 3 || tile.shape == .slope 4 then return true
  return tile.shape == .full && OriginalTileRules.isTable id

/-- Local style-17 placement into a dry footprint. AI checks this at
the starting position even while rising. Both bottom anchors must be present. -/
def Gravestone.findPlacement (grid : OriginalCollision.Grid) (grave : Gravestone) :
    Option GravestonePlacement := Id.run do
  let (originX, originY) := grave.placementOrigin
  if originX < 0 || originY < 1 then return none
  let x := originX.toNat
  let y := originY.toNat - 1
  if x + 1 ≥ grid.width || y + 2 ≥ grid.height then return none
  for column in [x:x + 2] do
    if !bottomAnchor grid (grid.tileAt column (y + 2)) then return none
    for row in [y:y + 2] do
      let tile := grid.tileAt column row
      if tile.id.any (fun id => !OriginalTileRules.breakableInObjectPlacement id) ||
          tile.liquid.isSome then return none
  return some ⟨x, y, grave.tileStyle, grave.inscription⟩

/-- `Projectile.NewProjectile` IL_00de..00fd: a projectile created inside a liquid is wet at once,
so its first update makes no entry splash. -/
def Gravestone.inWorld (grid : OriginalCollision.Grid) (grave : Gravestone) : Gravestone :=
  let hit := OriginalPlayerWet.wetCollision grid grave.position 24 24
  { grave with wet := hit.wet, honeyWet := hit.honey }

structure GravestoneStep where
  projectile : Gravestone
  placement : Option GravestonePlacement
  collided : Bool := false
  /-- The update crossed a liquid surface (or is in shimmer): the original makes splash dust and a
  sound there, which this path does not own. The caller must surface it, not continue silently. -/
  unsupported : Option String := none
  deriving BEq, Repr

/-- One local style-17 update: AI (with the placement attempt), the liquid state of
`Projectile.Update` IL_0616..119c, then `HandleMovement`. Inactive projectiles have zero time
left. When the result carries a placement, apply its frames and set the sign at its upper-left
tile. Movement still runs on that last update.
In water or lava the position advances by half the collided velocity, in honey by a quarter
(`HandleMovement` IL_053f..06fc) — except on an axis the tile collision changed, which keeps the
full collided value; the bounce rules act on the velocity as when dry. -/
def Gravestone.step (grid : OriginalCollision.Grid) (grave : Gravestone) : GravestoneStep :=
  if grave.timeLeft == 0 then { projectile := grave, placement := none } else
  let grave := grave.advanceAI
  let placement := grave.findPlacement grid
  -- liquid state, at the position the AI left
  let lava := OriginalPlayerLiquidHealth.lavaCollision grid grave.position 24 24
  let hit := OriginalPlayerWet.wetCollision grid grave.position 24 24
  let lavaWet := grave.lavaWet || lava
  let honeyWet := grave.honeyWet || hit.honey
  let shimmerWet := grave.shimmerWet || hit.shimmer
  let entering := hit.wet && grave.wetCount == 0 && !grave.wet
  let leaving := !hit.wet && grave.wet && grave.wetCount == 0
  let wet := hit.wet
  let wetCount := if leaving then 10 else grave.wetCount
  let (lavaWet, honeyWet, shimmerWet) := if wet then (lavaWet, honeyWet, shimmerWet) else (false, false, false)
  let wetCount := if wetCount > 0 then wetCount - 1 else 0
  let unsupported : Option String :=
    if shimmerWet then some "gravestone in shimmer"
    else if entering then some "gravestone entering a liquid: splash dust and sound"
    else if leaving then some "gravestone leaving a liquid: splash dust and sound"
    else none
  let incoming := grave.velocity
  let collision := OriginalCollision.tileCollision grid grave.position incoming 24 24
    { fallThrough := true, fall2 := true, hoik := true }
  let slope := OriginalCollision.slopeCollision grid grave.position collision.displacement 24 24 true
  let vx := if slope.velocity.x != incoming.x then incoming.x * -0.75 else incoming.x
  let vy := if slope.velocity.y != incoming.y && incoming.y > 1.5
    then incoming.y * -0.7 else slope.velocity.y
  let velocity : Vector := ⟨vx, vy⟩
  let collided := slope.position != grave.position || slope.velocity != incoming
  let factor : Float32 := if honeyWet then 0.25 else 0.5
  let moved : Vector := if wet then
      let hitVelocity := collision.displacement
      ⟨if hitVelocity.x != incoming.x then hitVelocity.x else hitVelocity.x * factor,
       if hitVelocity.y != incoming.y then hitVelocity.y else hitVelocity.y * factor⟩
    else velocity
  { projectile := { grave with
      position := slope.position + moved
      velocity
      direction := if vx < 0 then -1 else 1
      wet, lavaWet, honeyWet, shimmerWet, wetCount
      timeLeft := if placement.isSome then 0 else grave.timeLeft - 1 }
    placement, collided, unsupported }

/-- Breaking an ordinary or golden tile-85 gravestone returns its style's item. -/
def itemForTileStyle (style : Nat) : Option UInt16 :=
  if style == 0 then some 321
  else if style ≤ 5 then some (1172 + style).toUInt16
  else if style ≤ 10 then some (3223 + style).toUInt16
  else none

/-! ## Tile 85 mining and support framing -/

structure GravestoneRemoval where
  /-- Left-top corner passed to `Sign.KillSign`. -/
  origin : Cell
  /-- The surviving cell whose frame check detected the broken object. It
  supplies the pixel origin of the 32×32 item spawn rectangle. -/
  trigger : Cell
  /-- Cells still present after the initial kill, cleared in column order. -/
  cleared : Array Cell
  item : UInt16
  deriving BEq, Repr

structure GravestoneMiningWorld where
  terrain : Terrain
  items : WorldItems
  random : UnifiedRandom
  signs : Array Sign := #[]
  activeGhosts : Nat := 0
  spawnedGhosts : Array OriginalGhostSpawn.Ghost := #[]
  removals : Array GravestoneRemoval := #[]
  /-- Cells requesting NPC 316 from `NPC.SpawnOnPlayer` on the nearest
  player. Requests are made after dropping the tombstone item. -/
  ghostRequests : Array Cell := #[]

/-- Classic framing makes the spawn decision. When NPC simulation is
available, the caller supplies its immediate effect on the NPC count and
the same random stream used for subsequent frames. -/
abbrev SpawnGravestoneGhost := GravestoneMiningWorld → Cell → GravestoneMiningWorld

def deferGravestoneGhost : SpawnGravestoneGhost := fun world _ => world

private def GravestoneMiningWorld.clear (world : GravestoneMiningWorld) (cell : Cell) :
    GravestoneMiningWorld :=
  let tile := (world.terrain.tile? cell.x cell.y).getD default
  let terrain := (world.terrain.replaceTile cell.x cell.y (OriginalMining.clearGround tile))
    |>.getD world.terrain
  { world with terrain }

/-- The tile-85 branch of `CheckSign` (single-player Classic). A surviving
tile determines the style, even if the upper-left tile was already removed. -/
def GravestoneMiningWorld.frameTile (world : GravestoneMiningWorld) (trigger : Cell)
    (spawnGhost : SpawnGravestoneGhost := deferGravestoneGhost) :
    GravestoneMiningWorld := Id.run do
  let terrain := world.terrain
  if trigger.x ≤ 5 || trigger.y ≤ 5 || trigger.x ≥ terrain.header.width - 5 ||
      trigger.y ≥ terrain.header.height - 5 then return world
  let grid := OriginalCollision.Grid.ofTerrain terrain OriginalTileRules.collision
  let tile := grid.tileAt trigger.x trigger.y
  if tile.id != some 85 then return world
  let (frameX, frameY) := tile.frame.getD (0, 0)
  let column := frameX.tdiv 18
  let row := frameY.tdiv 18
  let x := (Int.ofNat trigger.x - column.tmod 2).toNat
  let y := (Int.ofNat trigger.y - row.tmod 2).toNat
  let (baseX, baseY) := (grid.tileAt x y).frame.getD (0, 0)
  let styleX := (baseX.tdiv 18).tdiv 2
  let styleY := (baseY.tdiv 18).tdiv 2
  let footprint : Array Cell := #[⟨x, y⟩, ⟨x, y + 1⟩, ⟨x + 1, y⟩, ⟨x + 1, y + 1⟩]
  let anchors : OriginalBankMining.World := { terrain, items := world.items, random := world.random }
  let intact := footprint.all fun cell =>
    let current := grid.tileAt cell.x cell.y
    current.id == some 85 && current.frame.any fun (fx, fy) =>
      fx.tdiv 18 == 2 * styleX + (cell.x - x : Nat) &&
      fy.tdiv 18 == 2 * styleY + (cell.y - y : Nat)
  if intact && OriginalBankMining.solidTileAllowBottomSlope anchors x (y + 2) &&
      OriginalBankMining.solidTileAllowBottomSlope anchors (x + 1 : Nat) (y + 2) then return world
  let cleared := footprint.filter fun cell => (grid.tileAt cell.x cell.y).id == some 85
  let world := cleared.foldl GravestoneMiningWorld.clear world
  let signs := world.signs.filter fun sign => sign.x != (x : Int) || sign.y != (y : Int)
  let item := (itemForTileStyle (column.tdiv 2).toNat).getD 321
  let position : Vector := ⟨single (trigger.x * 16 + 8 : Nat), single (trigger.y * 16 + 8 : Nat)⟩
  let (items, random) := OriginalItems.spawnStackAt world.items position (.ofItem item) world.random
    (some (OriginalCollision.Grid.ofTerrain world.terrain OriginalTileRules.collision))
  let (roll, random) := random.next 0 8
  let removal : GravestoneRemoval := { origin := ⟨x, y⟩, trigger, cleared, item }
  let mut world := { world with items, random, signs, removals := world.removals.push removal }
  if roll == 0 && world.activeGhosts < 2 then
    world := spawnGhost { world with ghostRequests := world.ghostRequests.push trigger } trigger
  return world

/-- Visit the gravestone cells in `SquareTileFrame` order after the caller
has applied the initial cell change and spawned its drop. The caller's other
tile frame rules can instead invoke `frameTile` at each visit. A reset of a
surviving unframed center takes its cosmetic variant roll in sequence. -/
def GravestoneMiningWorld.squareFrame (world : GravestoneMiningWorld) (center : Cell)
    (resetCenter : Bool := false) (spawnGhost : SpawnGravestoneGhost := deferGravestoneGhost) :
    GravestoneMiningWorld := Id.run do
  let mut world := world
  for x in [center.x - 1:center.x + 2] do
    for y in [center.y - 1:center.y + 2] do
      if resetCenter && x == center.x && y == center.y && 5 < x && 5 < y &&
          x < world.terrain.header.width - 5 && y < world.terrain.header.height - 5 then
        let tile := (world.terrain.tile? x y).getD default
        if tile.id.any (· != 85) && tile.frame.isNone then
          world := { world with random := world.random.nextInt.2 }
      world := world.frameTile ⟨x, y⟩ spawnGhost
  return world

structure GravestoneStrike where
  world : GravestoneMiningWorld
  hits : HitCache

/-- The direct `PickTile` path for tile 85, including failed-hit framing.
Pass the regular pickaxe power (35 for an unmodified Copper Pickaxe). -/
def hitGravestone (rules : WorldRules) (world : GravestoneMiningWorld)
    (hits : HitCache) (cell : Cell) (power : Nat)
    (spawnGhost : SpawnGravestoneGhost := deferGravestoneGhost) : GravestoneStrike := Id.run do
  let grid := OriginalCollision.Grid.ofTerrain world.terrain OriginalTileRules.collision
  let tile := grid.tileAt cell.x cell.y
  if tile.id != some 85 || power == 0 then return ⟨world, hits⟩
  let damage := OriginalMining.pickDamage grid rules cell tile power
  let hits := hits.record cell damage
  let world := { world with random := world.random.nextInt.2 }
  let (world, hits) := if hits.damage cell ≥ 100 then
    (world.clear cell, (hits.forget cell).prune grid)
    else (world, hits)
  let world := world.squareFrame cell true spawnGhost
  let grid := OriginalCollision.Grid.ofTerrain world.terrain OriginalTileRules.collision
  return ⟨world, hits.prune grid⟩

/-- The ordinary stone/dirt support hit used by the Classic mining replay.
Other tool paths can call `frameTile` from their native framing traversal. -/
def hitGravestoneSupport (rules : WorldRules) (world : GravestoneMiningWorld)
    (hits : HitCache) (cell : Cell) (power : Nat)
    (spawnGhost : SpawnGravestoneGhost := deferGravestoneGhost) : GravestoneStrike := Id.run do
  let grid := OriginalCollision.Grid.ofTerrain world.terrain OriginalTileRules.collision
  if cell.y == 0 || (grid.tileAt cell.x (cell.y - 1)).id != some 85 then return ⟨world, hits⟩
  let result := OriginalMining.hitGround grid rules hits cell power
  if result.cache == hits && result.changed.isNone then return ⟨world, hits⟩
  let terrain := result.apply world.terrain cell
  let world := { world with terrain, random := world.random.nextInt.2 }
  let world := match result.drop with
    | none => world
    | some item =>
      let (items, random) := OriginalItems.spawnBlock world.items cell item world.random (some grid)
      { world with items, random }
  let firstRemoval := world.removals.size
  let world := world.squareFrame cell true spawnGhost
  let hits := (world.removals.extract firstRemoval world.removals.size).foldl (init := result.cache) fun cache removal =>
    removal.cleared.foldl HitCache.forget cache
  return ⟨world, hits⟩

end Lean4Terria.OriginalPlayerDeath
