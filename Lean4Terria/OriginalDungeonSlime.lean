import Lean4Terria.OriginalAngryBones
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalStarterMelee

/-!
# Dungeon Slime (Terraria 1.4.5.8, Classic)

NPC 71, `aiStyle = 1`. The original `AI_001_Slimes` reaches type 71 only through the
type-independent slime branches plus one type test (`IL_4b52`: `ai[0] += 3` on every grounded
frame). The four type-1-only blocks (`ai[1] == 75` light, the item roll inside the
`SlimeCanContainItems` gate, the colour and paint block `IL_0893`–`IL_154f`, and the
`direction == 0` fix at `IL_29f5`) are skipped. `NPCID.Sets.SlimeCanContainItems[71]` is false
in 1.4.5.8, so `ai[1]` is never written or read by the AI and the slime never carries an
`ai[1]` item. The golden key seen inside it is drawn unconditionally by `Main.DrawNPCDirect`
(`IL_1c8b`) and is dropped by the type-specific rule `CommonDrop(327, 1/1)`.

`UpdateNPC`, gravity, tile collision, `FindFrame` (the slime case of the type switch) and
`CheckActive` are the same code as for the Green Slime and are reused from
`OriginalGreenSlime`. Inputs and outputs follow `OriginalGreenSlime.update`.

Not covered (the step reports `unsupported`): liquids, the `ai[0] = -999` statue frame,
bee hurt (`CanBeHurtByBees` with a nearby bee NPC), multiplayer, special seeds.
-/

namespace Lean4Terria.OriginalDungeonSlime

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Rng Slime targetClosest solidCollision)

def typeId : Int := 71
/-- Native `Main.npcFrameCount[71]`; the desktop `NPC_71.xnb` is 44 x 68, so a frame is 34 high. -/
def frameCount : Int := 2
def desktopFrameHeight : Int := 34
def textureWidth : Nat := 44
def goldenKey : UInt16 := 327
def bannerId : Nat := 107
def bannerItem : UInt16 := 2916

/-- `NPC.NewNPC(source, x, y, 71)`: `SetDefaults(71)` (36 x 24 scaled by 1.25 to 45 x 30),
`Bottom = (x, y)` with the float half width, `timeLeft = (int)(750 * 1.25)`. No draws. -/
def spawn (x y : Int) : Slime :=
  let width : Nat := 45
  let height : Nat := 30
  { position := ⟨single x - single width / 2, single (y - (height : Int))⟩
    width, height, life := 150, lifeMax := 150, damage := 30, defDamage := 30
    defense := 7, defDefense := 7, knockBackResist := 0.6, value := 150, scale := 1.25
    alpha := 60, npcSlots := 2, color := ⟨0, 0, 0, 0⟩, timeLeft := 937 }

/-- The world for a desktop client (`frameHeight = 34`) or a dedicated server (`1`). -/
def desktopWorld (grid : Grid) (worldSurface rockLayer : Float) : OriginalGreenSlime.World :=
  { grid, worldSurface, rockLayer, frameHeight := desktopFrameHeight, frameCount }

structure Step where
  slime : Slime
  rng : Rng
  gravity : Option Float32
  nearby : Array (Nat × Float32) := #[]
  unsupported : Option String := none
  deriving Repr

/-- `AI_001_Slimes` for `type = 71`. -/
def ai (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (rng : Rng) (n : Slime) :
    Slime × Rng × Option String := Id.run do
  let grid := world.grid
  let mut n := n
  if n.ai0 == -999 then
    return ({ n with frameY := 0, frameCounter := 0, rotation := 0 }, rng, some "ai[0] = -999 (statue frame)")
  let aggressive := !(world.dayTime && n.life == n.lifeMax &&
    !(n.position.y.toFloat > world.worldSurface * 16)) || world.slimeRain
  if n.ai2 > 1 then n := { n with ai2 := n.ai2 - 1 }
  if n.wet then return (n, rng, some "wet Dungeon Slime")
  n := { n with aiAction := 0 }
  if n.ai2 == 0 then
    n := { n with ai0 := -100, ai2 := 1 }
    n := targetClosest players n
  if n.velocity.y == 0 then
    if n.collideY && n.oldVelocity.y != 0 && solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if n.ai3 == n.position.x then
      n := { n with direction := -n.direction, ai2 := 200 }
    n := { n with ai3 := 0 }
    n := { n with velocity.x := n.velocity.x * 0.8 }
    if n.velocity.x.toFloat > -0.1 && n.velocity.x.toFloat < 0.1 then n := { n with velocity.x := 0 }
    if aggressive then n := { n with ai0 := n.ai0 + 1 }
    n := { n with ai0 := n.ai0 + 1 }
    -- `IL_4b52`: the only type-71 branch of the slime AI.
    n := { n with ai0 := n.ai0 + 3 }
    let threshold : Float32 := -1000
    let mut jump : Nat := 0
    if n.ai0 ≥ 0 then jump := 1
    if n.ai0 ≥ threshold && n.ai0 ≤ threshold * 0.5 then jump := 2
    if n.ai0 ≥ threshold * 2 && n.ai0 ≤ threshold * 1.5 then jump := 3
    if jump > 0 then
      n := { n with netUpdate := true }
      if aggressive && n.ai2 == 1 then n := targetClosest players n
      if jump == 3 then
        n := { n with velocity.y := -8, velocity.x := n.velocity.x + single (3 * n.direction), ai0 := -200,
                      ai3 := n.position.x }
      else
        n := { n with velocity.y := -6, velocity.x := n.velocity.x + single (2 * n.direction), ai0 := -120 }
        n := { n with ai0 := if jump == 1 then n.ai0 + threshold else n.ai0 + threshold * 2 }
    else if n.ai0 ≥ -30 then
      n := { n with aiAction := 1 }
  else if n.target < 255 && ((n.direction == 1 && n.velocity.x < 3) || (n.direction == -1 && n.velocity.x > -3)) then
    if n.collideX && n.velocity.x.abs == 0.2 then
      n := { n with position.x := n.position.x - 1.4 * single n.direction }
    if n.collideY && n.oldVelocity.y != 0 && solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if (n.direction == -1 && n.velocity.x.toFloat < 0.01) || (n.direction == 1 && n.velocity.x.toFloat > -0.01) then
      n := { n with velocity.x := n.velocity.x + 0.2 * single n.direction }
    else
      n := { n with velocity.x := n.velocity.x * 0.93 }
  return (n, rng, none)

/-- One `NPC.UpdateNPC(i)` call for an active, dry Dungeon Slime. Thread `Step.rng` to the next
NPC slot; `nearby` holds the `Player.nearbyActiveNPCs` increments (2 per player in range). An
inactive result is the original off-screen despawn (`CheckActive`). -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (rng : Rng) (n : Slime) : Step := Id.run do
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world n
  if !n.active then return { slime := n, rng, gravity := none }
  let mut n := n
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  if n.life ≤ 0 then
    return { slime := { n with active := false, netUpdate := false, justHit := false }, rng, gravity := some gravity }
  n := { n with oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY }
  let ease := 1 + n.velocity.x.abs / 3
  if n.gfxOffY > 0 then
    n := { n with gfxOffY := n.gfxOffY - ease * n.stepSpeed }
    if n.gfxOffY < 0 then n := { n with gfxOffY := 0 }
  else if n.gfxOffY < 0 then
    n := { n with gfxOffY := n.gfxOffY + ease * n.stepSpeed }
    if n.gfxOffY > 0 then n := { n with gfxOffY := 0 }
  if n.gfxOffY > 16 then n := { n with gfxOffY := 16 }
  if n.gfxOffY < -16 then n := { n with gfxOffY := -16 }
  let (next, rng, unsupported) := ai world players rng n
  if unsupported.isSome then return { slime := next, rng, gravity := some gravity, unsupported }
  n := next
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  n := { n with velocity.y := n.velocity.y + gravity }
  if n.velocity.y > maxFall then n := { n with velocity.y := maxFall }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  if (OriginalPlayerWet.wetCollision world.grid n.position n.width n.height).wet then
    return { slime := n, rng, gravity := some gravity, unsupported := some "Dungeon Slime entered liquid" }
  n := OriginalGreenSlime.updateCollision world.grid gravity n
  n := OriginalGreenSlime.findFrame world n
  let (checked, nearby) := OriginalGreenSlime.checkActive world players n
  n := { checked with netUpdate := false, justHit := false }
  return { slime := n, rng, gravity := some gravity, nearby }

/-! ## Combat -/

open OriginalStarterMelee (Npc)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)

def toCombatNpc (n : Slime) (playerSlot : Nat := 0) : Npc :=
  { active := n.active, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax
    defense := n.defense, knockBackResist := n.knockBackResist, justHit := n.justHit
    immune := (n.immune[playerSlot]?.getD 0).toNat }

/-- Write back a landed hit. `StrikeNPC_Inner` has no slime-specific write-back for type 71
(its `type == 1` block is the Green Slime's), so the slot keeps its AI timers. -/
def applyHit (n : Slime) (hit : Npc) (playerSlot : Nat := 0) (credited : Bool := true) : Slime :=
  { n with
    active := hit.active && hit.life > 0, life := hit.life, velocity := hit.velocity, justHit := hit.justHit
    immune := n.immune.set! playerSlot hit.immune, playerInteraction := n.playerInteraction || credited }

def contactSource (n : Slime) : Option (Int × String) :=
  if !n.active || n.damage ≤ 0 then none else some (n.damage, "Dungeon Slime")

/-- Ordinary local-player contact (no banner or armour effects). A lethal result has zero life;
the caller runs the usual player death. -/
def contact (n : Slime) (body : OriginalPlayerCollision.Body)
    (health : OriginalPlayerLiquidHealth.Health) (defense : Int)
    (pool : DustPool) (rng : UnifiedRandom)
    (text : Option OriginalCombatTextChannel.Channel := none)
    (client : OriginalPlayerHurt.Client := {}) : Option OriginalAngryBones.ContactHit :=
  OriginalAngryBones.contactNpc (toCombatNpc n) n.damage body health defense pool rng text client

/-- The slime leaf of `NPC.HitEffect` (`IL_e131`, types 1, 16, 71, 244, 535): type 4 dust in the
NPC's alpha and colour, `ceil(damage / lifeMax * 100)` on a hit (the loop runs while
`i < damage / lifeMax * 100` in doubles) or 50 on death, with no gore and no extra draws. -/
def hitEffects (n : Slime) (pool : DustPool) (direction damage : Int) (hit : Npc) (rng : UnifiedRandom) :
    OriginalAngryBones.HitEffects := Id.run do
  let died := hit.life ≤ 0
  let count : Nat := if died then 50 else
    ((damage.toFloat / hit.lifeMax.toFloat * 100).ceil.toUInt64.toNat)
  let request : DustRequest := {
    position := hit.position, width := hit.width, height := hit.height, typeId := 4
    speed := if died then ⟨single (2 * direction), -2⟩ else ⟨single direction, -1⟩
    alpha := n.alpha, color := n.color.rgba }
  let mut state : OriginalAngryBones.HitEffects := { dust := #[], pool, random := rng }
  for _ in [:count] do
    let (pool, particle, random) := state.pool.create request state.random
    state := { dust := particle.elim state.dust state.dust.push, pool, random }
  return state

open OriginalCombat.Loot (Context Result)

/-- Gate at the top of `NPC.NPCLoot` for type 71: no loot (and no draws) on a multiplayer
client, or before Skeletron in a world without a surface (`worldSurface <= 50`) or in a
For-the-Worthy world, unless the world uses only shimmer oceans. -/
structure LootWorld where
  client : Bool := false
  getGoodWorld : Bool := false
  worldSurface : Float := 323
  downedBoss3 : Bool := true
  onlyShimmerOceanWorlds : Bool := false

def LootWorld.allowsLoot (w : LootWorld) : Bool :=
  !w.client && !((w.getGoodWorld || !(w.worldSurface > 50)) && !w.downedBoss3 && !w.onlyShimmerOceanWorlds)

/-- Ordinary Classic, pre-Hardmode, zero-luck `NPCLoot` for type 71 after the hit effects.
The global rules make no draws in this state (no Hardmode, event, biome key or Cascade
condition holds, and `SlimeBodyItemDropRule` sees `ai[1] = 0`). The type rule is
`CommonDrop(327, 1, 1, 1)`: `RollLuck(1)` and `Next(1, 2)` always succeed, then
`Item.NewItem`. Coins use the base value 150, then hearts and stars. Neither a statue
spawn nor missing player credit changes this (`NoEarlymodeLootWhenSpawnedFromStatue[71]`
is false and `StatueSpawnedDropRarity[71]` is -1). -/
def dropLoot (env : Context) (n : Slime) (random : UnifiedRandom) (world : LootWorld := {}) : Result := Id.run do
  if !world.allowsLoot then return { random }
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let (_, random) := random.next 0 1
  let (_, random) := random.next 1 2
  let (prefixId, random) := OriginalRecipeCatalog.rollNaturalPrefix goldenKey random
  let (key, random) := OriginalItems.rollDropAt position
    { OriginalItems.Stack.ofItem goldenKey 1 with prefixId } random env.grid
  let coins := OriginalCombat.Loot.dropMoneyAt env position n.value { drops := #[key], random }
  return OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) coins

/-- Kill tally for banner 107 (item 2916, every 50 kills); only credited kills count. -/
def recordBannerKill (credited : Bool) (banner : OriginalAngryBones.BannerProgress) :
    OriginalAngryBones.BannerProgress :=
  if credited then banner.recordKill else banner

/-! ## Desktop drawing of the key inside the body -/

structure Sprite where
  position : Vector
  source : Int × Int × Int × Int
  origin : Vector
  rotation : Float32
  scale : Float32
  deriving BEq, Repr

/-- `DrawNPCDirect` `IL_1c8b`: before the body, the golden key (`Item_327`, 14 x 24) is drawn
at the body centre, pulled back by the velocity, in the NPC's lit colour (not its alpha).
`frameY` is the pixel `frame.Y` (the 120/360/480 tests never hold for NPC 71's 0 and 34). -/
def keySprite (n : Slime) (screen : Vector) (frameY : Int := n.frameY) : Sprite := Id.run do
  let mut offset : Vector := ⟨0, 0⟩
  offset := { offset with y := offset.y - n.velocity.y * 0.3 }
  offset := { offset with x := offset.x - n.velocity.x * 0.6 }
  let rotation : Float32 := 0 + n.velocity.x * 0.09
  if frameY == 120 then offset := { offset with y := offset.y + 2 }
  if frameY == 360 then offset := { offset with y := offset.y - 2 }
  if frameY == 480 then offset := { offset with y := offset.y - 6 }
  let x := n.position.x - screen.x + single (n.width / 2 : Nat) + offset.x
  let y := n.position.y - screen.y + single (n.height / 2 : Nat) + offset.y
  return { position := ⟨x, y⟩, source := (0, 0, 14, 24), origin := ⟨7, 12⟩, rotation, scale := 1 }

/-- `NPC.GetAlpha` for type 71: each colour channel times `(255 - alpha) / 255f`, truncated,
and the alpha channel minus `alpha`, clamped to 0..255. -/
def litColor (n : Slime) (light : OriginalGreenSlime.Color) : OriginalGreenSlime.Color :=
  let factor : Float32 := single (255 - (n.alpha : Int)) / 255
  let channel (c : Nat) : Nat := (single c * factor).toInt32.toInt.toNat
  let a := max 0 (min 255 ((light.a : Int) - n.alpha))
  ⟨channel light.r, channel light.g, channel light.b, a.toNat⟩

structure BodySprite where
  source : Int × Int × Int × Int
  position : Vector
  origin : Vector
  color : OriginalGreenSlime.Color
  rotation : Float32
  scale : Float32
  flip : Bool
  deriving BEq, Repr

/-- The ordinary `DrawNPCDirect` body draw of `NPC_71` (44 x 68, two 34-pixel frames) after
the key. `Slime.frameY` is the pixel `frame.Y` that `FindFrame` advances by `World.frameHeight`
(0 or 34 on the desktop), so it is the source rectangle's Y as is. -/
def bodySprite (n : Slime) (screen : Vector) (light : OriginalGreenSlime.Color) : BodySprite :=
  let frameHeight : Nat := desktopFrameHeight.toNat
  let ox := single textureWidth / 2
  let oy := single frameHeight / 2
  let x := n.position.x - screen.x + single (n.width / 2)
  let y := n.position.y - screen.y + single n.height - single frameHeight * n.scale + oy * n.scale + 4 + n.gfxOffY
  { source := (0, n.frameY, textureWidth, frameHeight), position := ⟨x, y⟩, origin := ⟨ox, oy⟩
    color := litColor n light, rotation := n.rotation, scale := n.scale, flip := n.spriteDirection == 1 }

end Lean4Terria.OriginalDungeonSlime
