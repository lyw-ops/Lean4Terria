import Lean4Terria.AngryBones
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalDoor
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalPlayerHurt

namespace Lean4Terria.OriginalAngryBones

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Rng Slime)

def variant? : Int → Option AngryBonesKind
  | 31 => some .regular
  | 294 => some .big
  | 295 => some .bigMuscle
  | 296 => some .bigHelmet
  | -13 => some .short
  | -14 => some .bigBoned
  | _ => none

structure Enemy where
  netId : Int
  body : Slime
  lastInteraction : Int := 255
  deriving Repr

def Enemy.npcType (n : Enemy) : Int :=
  ((variant? n.netId).map (fun kind => (kind.sprite : Int))).getD 31

/-- Classic `NPC.NewNPC` defaults and positioning for the six Angry Bones net IDs.
The caller supplies the pixel anchor and allocates the shared slot. -/
def spawn (netId x y : Int) : Option Enemy := do
  let kind ← variant? netId
  return { netId, body :=
    { position := ⟨single x - single kind.width / 2, single (y - kind.height)⟩
      width := kind.width, height := kind.height, scale := kind.scale, npcSlots := single kind.npcSlots
      life := kind.health, lifeMax := kind.health, damage := kind.damage, defDamage := kind.damage
      defense := kind.defense, defDefense := kind.defense, knockBackResist := kind.knockbackMultiplier
      value := single kind.coins, alpha := 0, timeLeft := 937, color := ⟨0, 0, 0, 0⟩ } }

structure Step where
  enemy : Enemy
  rng : Rng
  nearby : Array (Nat × Float32) := #[]
  /-- Position for the original ambient sound when `Next(1000)` returns zero. -/
  ambientSound : Option Vector := none
  /-- The instance a client's sound channel resolved for that ambient sound (type 14 style 1). -/
  ambientPlayed : Option OriginalTileHitEffects.PlayedSound := none
  /-- Publish these immediately after this slot, before updating the next NPC. -/
  tileEdits : Array OriginalDoor.TileEdit := #[]
  doorSounds : Array OriginalDoor.Sound := #[]
  deriving Repr

private def tileAt (grid : Grid) (x y : Int) : OriginalWorld.Tile :=
  if x < 0 || y < 0 || x ≥ (grid.width : Int) || y ≥ (grid.height : Int) then {}
  else grid.tileAt x.toNat y.toNat

private def solidTile (grid : Grid) (t : OriginalWorld.Tile) (allowSolidTop : Bool := false) : Bool :=
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    rule.solid && (allowSolidTop || !rule.solidTop))

private def topSlope (t : OriginalWorld.Tile) : Bool :=
  t.shape == .slope 1 || t.shape == .slope 2

private def solidNoPlatforms (grid : Grid) (x y : Int) : Bool :=
  let t := tileAt grid x y
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    (rule.solid || rule.solidTop) && !rule.platform)

private def solidAllowBottomSlope (grid : Grid) (x y : Int) : Bool :=
  let t := tileAt grid x y
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    let frame := ((t.frame.map Prod.fst).getD (-18)).tdiv 18
    let platformTop := rule.platform &&
      ((0 ≤ frame && frame ≤ 7) || (12 ≤ frame && frame ≤ 16) || (25 ≤ frame && frame ≤ 26))
    (rule.solid || rule.solidTop) && t.shape != .halfBlock && (!topSlope t || platformTop))

/-- The fighter tests the loaded tiles below its feet even when `collideY` is set.
A real solid tile at head height invalidates the grounded jump branch. -/
private def grounded (grid : Grid) (n : Slime) : Bool := Id.run do
  if n.velocity.y != 0 then return false
  let left := tileIndex n.position.x
  let right := tileIndex (n.position.x + single n.width)
  let first := tileIndex (n.position.x + 8)
  let last := tileIndex (n.position.x + single n.width - 8)
  let feet := tileIndex (n.position.y + single n.height + 7)
  let head := tileIndex (n.position.y - 9)
  let mut supported := false
  for x in [first.toNat:last.toNat + 1] do
    if solidNoPlatforms grid x head then return false
    if left ≤ (x : Int) && (x : Int) ≤ right then
      let tile := tileAt grid x feet
      if !tile.actuated && (tile.id.any fun id => (grid.rules id).solid) then supported := true
  return supported

private def track (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
  let n := OriginalGreenSlime.targetClosest players n
  let target := OriginalGreenSlime.playerAt players n.target
  if n.directionY > 0 && target.center.y ≤ n.position.y + single n.height then
    { n with directionY := -1 }
  else n

private def countStuck (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
  let vx := n.velocity.x
  let reversed := n.velocity.y == 0 &&
    ((vx > 0 && n.direction < 0) || (vx < 0 && n.direction > 0))
  let counter := if n.position.x == n.oldPosition.x || n.ai3 ≥ 60 || reversed then n.ai3 + 1
    else if vx.abs.toFloat > 0.9 && n.ai3 > 0 then n.ai3 - 1 else n.ai3
  let p := OriginalGreenSlime.playerAt players n.target
  let x := n.position.x.toInt32.toInt
  let y := n.position.y.toInt32.toInt
  let px := p.position.x.toInt32.toInt
  let py := p.position.y.toInt32.toInt
  let overlaps := x < px + p.width && px < x + n.width && y < py + p.height && py < y + n.height
  { n with ai3 := if counter > 600 || n.justHit || overlaps then 0 else counter }


/-- The fighter keeps its previous target while it attempts to turn away from an obstruction. -/
private def seek (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
  let target := OriginalGreenSlime.playerAt players n.target
  let n := if target.position.y + single target.height == n.position.y + single n.height then
    { n with directionY := -1 } else n
  if n.ai3 < 60 then track players n
  else
    let n := if n.velocity.x != 0 then { n with ai0 := 0 }
      else if n.velocity.y != 0 then n
      else if n.ai0 + 1 < 2 then { n with ai0 := n.ai0 + 1 }
      else { n with direction := -n.direction, spriteDirection := -n.direction, ai0 := 0 }
    if n.direction == 0 then { n with direction := 1 } else n

private def accelerate (cap : Float32) (n : Slime) : Slime :=
  let vx := n.velocity.x
  if vx.abs > cap then
    if n.velocity.y == 0 then { n with velocity := ⟨vx * 0.8, n.velocity.y * 0.8⟩ } else n
  else if n.direction == 1 && vx < cap then { n with velocity.x := min cap (vx + 0.07) }
  else if n.direction == -1 && vx > -cap then { n with velocity.x := max (-cap) (vx - 0.07) }
  else n

/-- The inline fighter step in `AI_003` checks three rows even for a small
variant. The two leading tiles' upper slopes cannot serve as a ledge. -/
private def stepUp (grid : Grid) (n : Slime) : Slime := Id.run do
  if n.velocity.y < 0 then return n
  let direction : Int := if n.velocity.x < 0 then -1 else if n.velocity.x > 0 then 1 else 0
  let nextX := n.position.x + n.velocity.x
  let half : Int := n.width / 2
  let x := tileIndex (nextX + single half + single ((half + 1) * direction))
  let bottom := n.position.y + single n.height
  let y := tileIndex (bottom - 1)
  if x < 4 || y < 4 || x ≥ (grid.width : Int) - 4 || y ≥ (grid.height : Int) - 4 then return n
  let left := single (x * 16)
  if left ≥ nextX + single n.width || left + 16 ≤ nextX then return n
  let atY := tileAt grid x
  let ahead := atY y
  let above := atY (y - 1)
  let upperHalf := above.id.isSome && !above.actuated && above.shape == .halfBlock
  let ledge := solidTile grid ahead && !topSlope ahead && !topSlope above
  if !ledge && !upperHalf then return n
  if solidTile grid above && (!upperHalf || solidTile grid (atY (y - 4))) then return n
  if solidTile grid (atY (y - 2)) || solidTile grid (atY (y - 3)) ||
      solidTile grid (tileAt grid (x - direction) (y - 3)) true then return n
  let surface : Float32 := single (y * 16) + (if ahead.shape == .halfBlock then 8 else 0) -
    (if above.shape == .halfBlock then 8 else 0)
  let rise := bottom - surface
  if rise ≤ 0 || rise > 16.1 then return n
  return { n with
    position.y := surface - single n.height
    gfxOffY := n.gfxOffY + rise
    stepSpeed := if rise < 9 then 1 else 2 }

private def doorAhead (grid : Grid) (n : Slime) : Option (Int × Int) :=
  let x := tileIndex (n.position.x + single (n.width / 2) + single (15 * n.direction))
  let y := tileIndex (n.position.y + single n.height - 15) - 1
  let ahead := tileAt grid x y
  if !ahead.actuated && (ahead.id == some 10 || ahead.id == some 388) then some (x, y) else none

/-- `AI_003` waits sixty grounded ticks. Angry Bones adds 5 + 6 to ai[1],
clamps it to ten, recoils and performs `KillTile(fail=true)` before `OpenDoor`.
For tile 10 the failed hit requests Dig, creates no dust and draws no RNG.
The closed-door branch skips the ordinary jumping/leaping code this tick. -/
private def tryDoor (grid : Grid) (n : Slime) (x y : Int) : Except String (Slime × OriginalDoor.Result) := do
  if (tileAt grid x y).id == some 388 then throw "Angry Bones tall-gate interaction"
  let n := { n with ai2 := n.ai2 + 1, ai3 := 0 }
  if n.ai2 < 60 then return (n, {})
  let n := { n with
    ai1 := min 10 (n.ai1 + 5 + 6), ai2 := 0
    velocity.x := 0.5 * single (-n.direction) }
  let opened ← OriginalDoor.openDoor grid x y n.direction
  let n := if opened.opened then n else { n with ai3 := 60, netUpdate := true }
  -- CheckTileBreakability returns 2 for the locked Lihzahrd door before
  -- KillTile can request sound or dust. OpenDoor subsequently refuses it too.
  let knock := OriginalDoor.failedHitSounds (tileAt grid x y) x y
  return (n, { opened with sounds := knock ++ opened.sounds })

private def jumpOver (grid : Grid) (n : Slime) : Except String Slime := do
  let x := tileIndex (n.position.x + single (n.width / 2) + single (15 * n.direction))
  let bottom := n.position.y + single n.height
  let y := tileIndex (bottom - 15)
  unless (n.spriteDirection == -1 && n.velocity.x < 0) ||
      (n.spriteDirection == 1 && n.velocity.x > 0) do return n
  if n.height ≥ 32 && solidNoPlatforms grid x (y - 2) then
    return { n with velocity.y := if solidNoPlatforms grid x (y - 3) then -8 else -7, netUpdate := true }
  if solidNoPlatforms grid x (y - 1) then
    return { n with velocity.y := -6, netUpdate := true }
  if bottom - single (y * 16) > 20 && !topSlope (tileAt grid x y) && solidNoPlatforms grid x y then
    return { n with velocity.y := -5, netUpdate := true }
  if n.directionY < 0 && !solidAllowBottomSlope grid x (y + 1) &&
      !solidAllowBottomSlope grid (x + n.direction) (y + 1) then
    return { n with velocity := ⟨n.velocity.x * 1.5, -8⟩, netUpdate := true }
  -- AI_003 IL_dc1d: an unobstructed, forward-facing grounded step ends
  -- any preceding door attempt. Airborne updates clear these timers too.
  return { n with ai1 := 0, ai2 := 0 }


private def leapAtTarget (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
  let p := OriginalGreenSlime.playerAt players n.target
  let center := n.position + ⟨single (n.width / 2), single (n.height / 2)⟩
  let target := p.position + ⟨single (p.width / 2), single (p.height / 2)⟩
  if n.velocity.y == 0 && (target.x - center.x).abs < 100 && (target.y - center.y).abs < 50 &&
      ((n.direction > 0 && n.velocity.x ≥ 1) || (n.direction < 0 && n.velocity.x ≤ -1)) then
    { n with velocity := ⟨max (-3) (min 3 (n.velocity.x * 2)), -4⟩, netUpdate := true }
  else n

private def prepare (n : Slime) : Slime :=
  let step := (1 + n.velocity.x.abs / 3) * n.stepSpeed
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - step)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + step) else n.gfxOffY
  { n with
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    gfxOffY := max (-16) (min 16 gfx) }

private def move (grid : Grid) (gravity : Float32) (n : Slime) : Slime :=
  let fallThrough := n.directionY == 1
  let velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height gravity
  let hit := OriginalCollision.tileCollision grid n.position velocity n.width n.height
    { fallThrough, fall2 := fallThrough, hoik := true }
  let displacement := if hit.hitAbove then { hit.displacement with y := 0.01 } else hit.displacement
  let position := n.position + displacement
  let slope := OriginalCollision.slopeCollision grid position displacement n.width n.height
    (fallThrough || n.stairFall)
  let dy := slope.position.y - position.y
  let step := slope.stair && dy.abs > 8
  { n with
    position := slope.position + ⟨0, 0⟩, velocity := slope.velocity
    oldPosition := n.position, oldVelocity := velocity, oldDirection := n.direction
    collideX := velocity.x != displacement.x, collideY := velocity.y != displacement.y
    stairFall := fallThrough || slope.stairFall
    gfxOffY := if step then n.gfxOffY - dy else n.gfxOffY
    stepSpeed := if step then 2 else n.stepSpeed }


private def animate (kind : AngryBonesKind) (n : Slime) : Slime :=
  if n.velocity.y != 0 then { n with frameY := 0, frameCounter := 0 }
  else
    let sprite := if n.direction == 1 || n.direction == -1 then n.direction else n.spriteDirection
    if n.velocity.x == 0 then { n with frameY := 0, frameCounter := 0, spriteDirection := sprite }
    else
      let counter := (n.frameCounter + (2 * n.velocity.x.abs).toFloat) + 1
      let frame := max n.frameY 2
      if counter > 6 then
        let row := if frame + 1 ≥ kind.frameCount then 2 else frame + 1
        { n with frameY := row, frameCounter := 0, spriteDirection := sprite }
      else { n with frameY := frame, frameCounter := counter, spriteDirection := sprite }

/-- One `NPC.UpdateNPC` for ordinary dry Classic Angry Bones below the surface.
Consumes the caller's `UpdateNPCs` stream. The caller owns combat, slot allocation,
and persistence of the returned door edits. The current NPC already collides
against its opened door; later slots must receive the same updated Grid. -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (rng : Rng) (enemy : Enemy)
    (playAmbient : Option (Vector → Rng → Rng × Option OriginalTileHitEffects.PlayedSound) := none) :
    Except String Step := do
  if !enemy.body.active then return { enemy, rng }
  let some kind := variant? enemy.netId | throw s!"unknown Angry Bones netID {enemy.netId}"
  if enemy.body.position.y.toFloat ≤ world.worldSurface * 16 then throw "Angry Bones above the dungeon surface"
  if enemy.body.life ≤ 0 then throw "Angry Bones death is handled by the strike phase"
  if enemy.body.wet then throw "Angry Bones already in liquid"
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world enemy.body
  let n := countStuck players (prepare enemy.body)
  let (ambientSound, rng) :=
    if n.ai3 ≥ 60 then (none, rng)
    else
      let (roll, rng) := rng.next 1000
      (if roll == 0 then some n.position else none, rng)
  -- `SoundEngine.PlaySound(14, position)` right at the roll (AI_003_Fighters): an audible one draws its
  -- variation from this same stream before the rest of the AI.
  let (rng, ambientPlayed) := match ambientSound, playAmbient with
    | some position, some play => play position rng
    | _, _ => (rng, none)
  let stationary := n.velocity.x == 0 && !n.justHit
  let n := accelerate kind.walkSpeed (seek players n)
  let canJump := grounded world.grid n
  let n := stepUp world.grid n
  let (n, door) ← if canJump then do
      if let some (x, y) := doorAhead world.grid n then tryDoor world.grid n x y
      else do
        let wasFacing := (n.spriteDirection == -1 && n.velocity.x < 0) ||
          (n.spriteDirection == 1 && n.velocity.x > 0)
        let n ← jumpOver world.grid n
        let n := if wasFacing && n.velocity.y == 0 && stationary && n.ai3 == 1 then
          { n with velocity.y := -5 } else n
        pure (leapAtTarget players n, ({} : OriginalDoor.Result))
    else pure ({ n with ai1 := 0, ai2 := 0 }, ({} : OriginalDoor.Result))
  let grid := OriginalDoor.applyGrid world.grid door.tileEdits
  let n := { n with
    immune := n.immune.map fun t => if t > 0 then t - 1 else t
    velocity.y := min maxFall (n.velocity.y + gravity) }
  let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then
      { n with velocity.x := 0 } else n
  if (OriginalPlayerWet.wetCollision grid n.position n.width n.height).wet ||
      OriginalPlayerLiquidHealth.lavaCollision grid n.position n.width n.height then
    throw "Angry Bones entered liquid"
  let n := move grid gravity n
  let (n, nearby) := OriginalGreenSlime.checkActive world players (animate kind n)
  let n := { n with netUpdate := false, justHit := false }
  return {
    enemy := { enemy with body := n }, rng, nearby, ambientSound, ambientPlayed
    tileEdits := door.tileEdits, doorSounds := door.sounds }

/-! ## Combat -/

open OriginalStarterMelee (Npc)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)
open OriginalPlayerLiquidHealth (Health)

def Enemy.toCombatNpc (n : Enemy) (playerSlot : Nat := 0) : Npc :=
  { active := n.body.active, position := n.body.position, velocity := n.body.velocity
    width := n.body.width, height := n.body.height, life := n.body.life, lifeMax := n.body.lifeMax
    defense := n.body.defense, knockBackResist := n.body.knockBackResist, justHit := n.body.justHit
    immune := (n.body.immune[playerSlot]?.getD 0).toNat }

/-- Write back a landed Classic hit. `NPC.UpdateNPC` clears `justHit` after
style 3 has reset its stuck timer and processed the post-hit velocity. -/
def Enemy.applyHit (n : Enemy) (hit : Npc) (playerSlot : Nat := 0) (credited : Bool := true) : Enemy :=
  { n with
    body := { n.body with
      active := hit.active && hit.life > 0
      life := hit.life, velocity := hit.velocity, justHit := hit.justHit
      immune := n.body.immune.set! playerSlot hit.immune
      playerInteraction := n.body.playerInteraction || credited }
    lastInteraction := if credited then playerSlot else n.lastInteraction }

def Enemy.contactSource (n : Enemy) : Option (Int × String) :=
  if !n.body.active || n.body.damage ≤ 0 then none
  else some (n.body.damage, "Angry Bones")

/-- The shared world banner entry for all six net IDs. The item is awarded
as a claim in the banner system at each multiple of 50. -/
def bannerId : Nat := 247
def bannerItem : UInt16 := 3451
def killsPerBanner : Nat := 50

structure BannerProgress where
  kills : Nat := 0
  claimable : Nat := 0
  fresh : Bool := false
  deriving BEq, Repr

/-- One `BannerSystem.AddNPCKillBy` after the native death eligibility check. -/
def BannerProgress.recordKill (before : BannerProgress) : BannerProgress :=
  let kills := before.kills + 1
  if kills % killsPerBanner == 0 && before.claimable < 9999 then
    { kills, claimable := before.claimable + 1, fresh := before.fresh || before.claimable == 0 }
  else { before with kills }

private def appendLoot (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16)
    (count : Nat) (loot : OriginalCombat.Loot.Result) : OriginalCombat.Loot.Result :=
  let (prefixId, random) := OriginalRecipeCatalog.rollNaturalPrefix item loot.random
  let (drop, random) := OriginalItems.rollDropAt position
    { OriginalItems.Stack.ofItem item count with prefixId } random env.grid
  { drops := loot.drops.push drop, random }

private def commonLoot (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16)
    (denominator : Int) (loot : OriginalCombat.Loot.Result) : Bool × OriginalCombat.Loot.Result :=
  let (roll, random) := loot.random.next 0 denominator
  if roll != 0 then (false, { loot with random })
  else
    let (_, random) := random.next 1 2
    (true, appendLoot env position item 1 { loot with random })

private def dropSpecific (env : OriginalCombat.Loot.Context) (position : Vector) (rng : UnifiedRandom) : OriginalCombat.Loot.Result := Id.run do
  let mut drops : OriginalCombat.Loot.Result := { random := rng }
  drops := (commonLoot env position 959 450 drops).2
  drops := (commonLoot env position 1307 300 drops).2
  for (item, denominator) in #[(932, 250), (3095, 100), (327, 65)] do
    let (dropped, next) := commonLoot env position item denominator drops
    if dropped then return next
    drops := next
  let (_, random) := drops.random.next 0 1
  let (count, random) := random.next 1 4
  return appendLoot env position 154 count.toNat { drops with random }

/-- Ordinary pre-Hardmode, zero-luck `NPC.NPCLoot` after hit effects, outside
global drop conditions such as the deep Cascade zone or seasonal events.
Banner progress belongs to the death event and uses no random draw. -/
def dropLoot (env : OriginalCombat.Loot.Context) (n : Enemy) (rng : UnifiedRandom) : OriginalCombat.Loot.Result :=
  let body := n.body
  let position := OriginalCombat.Loot.dropPosition body.position body.width body.height
  let items := dropSpecific env position rng
  let coins := OriginalCombat.Loot.dropMoneyAt env position body.value items
  OriginalCombat.Loot.dropHealsAt env position (body.lifeMax > 1 && body.damage > 0) coins

structure HitEffects where
  dust : Array DustParticle
  pool : DustPool
  random : UnifiedRandom

/-- The ordinary skeleton `HitEffect` leaf. Dust consumes its normal
viewport/pool-dependent random draws. For a lethal hit, caller supplies
the count of free gore slots so the five fragment requests can advance
the same stream even in a preview that omits those sprites. -/
def hitEffects (pool : DustPool) (freeGoreSlots : Nat) (hitDirection dealt : Int)
    (n : Npc) (rng : UnifiedRandom) : HitEffects := Id.run do
  let dead := n.life ≤ 0
  let count := if dead then 20
    else ((Float.ofInt dealt / Float.ofInt n.lifeMax * 50).ceil).toUInt64.toNat
  let speed : Vector := if dead then ⟨2.5 * single hitDirection, -2.5⟩ else ⟨single hitDirection, -1⟩
  let request : DustRequest := { position := n.position, width := n.width, height := n.height, typeId := 26, speed }
  let mut state : HitEffects := { dust := #[], pool, random := rng }
  for _ in [:count] do
    let (pool, particle, random) := state.pool.create request state.random
    state := { dust := particle.elim state.dust state.dust.push, pool, random }
  if dead then
    for _ in [:min 5 freeGoreSlots] do
      let (_, random) := state.random.nextInt
      let (_, random) := random.nextInt
      state := { state with random }
  return state

structure ContactHit where
  body : OriginalPlayerCollision.Body
  health : Health
  damage : Int
  effects : HitEffects
  text : Option OriginalCombatTextChannel.Channel := none
  /-- The accepted Hurt: life at the sound, the sound call, blood calls, lethal. -/
  hurt : Option OriginalPlayerHurt.Record := none
  /-- The client's sound side after this hit; commit it whenever it is present. -/
  sound : Option OriginalPlayerHurt.SoundClient := none
  /-- `hitDirection` as `Update_NPCCollision` passed it to Hurt (and Hurt to KillMe). -/
  hitDirection : Int := 0
  boundary : Option String := none

/-- The ordinary local player contact branch with no banner, armor-set or special-hurt
effects: `Update_NPCCollision`'s damage roll, then `Player.Hurt` in its own order — text,
signed life, immunity, knockback unless `noKnockback`, PlayHurtSound, then blood or the
death branch (`OriginalPlayerHurt.finish`). A lethal contact reports zero life so the caller
can run the usual player death path. A text boundary leaves body and health untouched; a
sound or blood boundary keeps everything written before it, the signed life included. -/
def contactNpc (npc : Npc) (base : Int) (body : OriginalPlayerCollision.Body) (health : Health)
    (defense : Int) (pool : DustPool) (rng : UnifiedRandom)
    (text : Option OriginalCombatTextChannel.Channel := none)
    (client : OriginalPlayerHurt.Client := {}) (luck : Float32 := 0) : Option ContactHit := do
  if health.immune || health.dead || !npc.active || base ≤ 0 then failure
  let bounds : OriginalStarterMelee.Rect :=
    ⟨body.position.x.toInt32.toInt, body.position.y.toInt32.toInt, body.width, body.height⟩
  if !(bounds.intersects npc.rect) then failure
  -- `Player.Update_NPCCollision` IL_02a7..02ae: `Main.DamageVar(damage, -luck)`.
  let (rolled, rng) := OriginalStarterMelee.damageVar base rng (-luck)
  let text := text.map fun channel => {channel with rect :=
    ⟨body.position.x.toInt32.toInt,body.position.y.toInt32.toInt,body.width,body.height⟩}
  let head := OriginalPlayerHurtText.ordinaryPrefix
    {damage := rolled, defense, life := health.statLife, lifeMax := health.statLifeMax2} text rng
  let damage := head.damage
  let rng := head.text.random
  if head.boundary.isSome || !head.accepted then
    return {body,health,damage,effects := {dust := #[],pool,random := rng},text := head.text.text
            sound := client.sound,boundary := head.boundary}
  let px := body.position.x + single (body.width / 2)
  let direction : Int := if npc.center.x < px then 1 else -1
  -- IL_0d7d..0dd8: velocity and fallStart, skipped by `noKnockback`.
  let body := if client.noKnockback then body else
    { body with velocity := ⟨4.5 * single direction, -3.5⟩, fallStart := tileIndex body.position.y }
  let tail := OriginalPlayerHurt.finish {
    position := body.position, width := body.width, height := body.height
    damage, rawLife := head.rawLifeAfter, lifeMax2 := health.statLifeMax2
    hitDirection := direction } client.sound pool rng
  let health := { health with
    statLife := tail.life, immune := head.immune, immuneTime := head.immuneTime, lifeRegenTime := 0 }
  return { body, health, damage, text := head.text.text
           effects := { dust := tail.particles, pool := tail.pool, random := tail.random }
           hurt := some tail.record, sound := tail.sound, hitDirection := direction, boundary := tail.boundary }

def contact (enemy : Enemy) (body : OriginalPlayerCollision.Body) (health : Health)
    (defense : Int) (pool : DustPool) (rng : UnifiedRandom)
    (text : Option OriginalCombatTextChannel.Channel := none)
    (client : OriginalPlayerHurt.Client := {}) : Option ContactHit :=
  contactNpc enemy.toCombatNpc enemy.body.damage body health defense pool rng text client

end Lean4Terria.OriginalAngryBones
