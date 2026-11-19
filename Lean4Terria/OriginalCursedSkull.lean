import Lean4Terria.CursedSkull
import Lean4Terria.OriginalAngryBones

namespace Lean4Terria.OriginalCursedSkull

open OriginalCollision (Vector single)
open OriginalGreenSlime (Slime)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)

structure Enemy where
  body : Slime
  deriving Repr

def npcType : Int := 34

def Enemy.npcType (_ : Enemy) : Int := OriginalCursedSkull.npcType

/-- Classic `NPC.NewNPC` uses a bottom-center pixel anchor. -/
def spawn (netId x y : Int) : Option Enemy :=
  if netId != npcType then none else
  some { body := {
    position := ⟨single x - single cursedSkullWidth / 2, single (y - cursedSkullHeight)⟩
    width := cursedSkullWidth, height := cursedSkullHeight
    life := cursedSkullHealth, lifeMax := cursedSkullHealth
    damage := cursedSkullDamage, defDamage := cursedSkullDamage
    defense := cursedSkullDefense, defDefense := cursedSkullDefense
    knockBackResist := 0.2, value := single cursedSkullCoins, npcSlots := cursedSkullNpcSlots
    scale := 1, alpha := 0, timeLeft := 937, color := ⟨0, 0, 0, 0⟩ } }

def Enemy.toCombatNpc (enemy : Enemy) (playerSlot : Nat := 0) : OriginalStarterMelee.Npc :=
  let n := enemy.body
  { active := n.active, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax
    defense := n.defense, knockBackResist := n.knockBackResist
    noGravity := true, noTileCollide := true, justHit := n.justHit
    immune := (n.immune[playerSlot]?.getD 0).toNat }

def Enemy.applyStrike (enemy : Enemy) (hit : OriginalStarterMelee.Npc)
    (playerSlot : Nat := 0) (credited : Bool := true) : Enemy :=
  let n := enemy.body
  { body := { n with
    active := hit.active && hit.life > 0, life := hit.life, velocity := hit.velocity
    justHit := hit.justHit, immune := n.immune.set! playerSlot hit.immune
    playerInteraction := n.playerInteraction || credited } }

structure Step where
  enemy : Enemy
  random : UnifiedRandom
  nearby : Array (Nat × Float32) := #[]

private def steer (current desired acceleration : Float32) : Float32 :=
  if current < desired then current + acceleration
  else if current > desired then current - acceleration else current

/-- Ordinary type 34's style-10 orbit and fifty-tick acceleration phase.
All arithmetic before `Sqrt` and `Atan2` retains native Single precision. -/
def ai (players : Array OriginalGreenSlime.Player) (initial : Slime) : Slime := Id.run do
  let mut n := OriginalGreenSlime.targetClosest players initial
  let target := OriginalGreenSlime.playerAt players n.target
  let x := target.center.x - n.center.x
  let y := target.center.y - n.center.y
  let distance := (x * x + y * y).toFloat.sqrt.toFloat32
  n := { n with ai1 := n.ai1 + 1 }
  let lunging := n.ai1 > 600
  if !lunging && distance < 250 then
    let orbit := n.ai0 + 0.9
    n := { n with
      ai0 := if orbit > 200 then -200 else orbit
      velocity := ⟨n.velocity.x + (if orbit < -100 || orbit > 100 then 0.019 else -0.019),
                   n.velocity.y + (if orbit > 0 then 0.019 else -0.019)⟩ }
  if n.ai1 > 650 then n := { n with ai1 := 0 }
  let (speed, acceleration) : Float32 × Float32 :=
    if distance > 350 then (5, 0.3)
    else if distance > 300 then (3, 0.2)
    else if distance > 250 then (1.5, 0.1)
    else if lunging then (4, 0.011 * 8)
    else (1, 0.011)
  let factor := speed / distance
  let desired : Vector := ⟨x * factor, y * factor⟩
  let motion := if !target.active || target.dead then
      (⟨single n.direction * speed / 2, -speed / 2⟩ : Vector) else desired
  n := { n with velocity := ⟨steer n.velocity.x motion.x acceleration, steer n.velocity.y motion.y acceleration⟩ }
  if desired.x > 0 then
    n := { n with
      spriteDirection := -1
      rotation := (Float.atan2 desired.y.toFloat desired.x.toFloat).toFloat32 }
  else if desired.x < 0 then
    n := { n with
      spriteDirection := 1
      rotation := (Float.atan2 desired.y.toFloat desired.x.toFloat).toFloat32 + 3.1415927410125732 }
  return n

private def prepare (n : Slime) : Slime :=
  let ease := (1 + n.velocity.x.abs / 3) * n.stepSpeed
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - ease)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + ease) else n.gfxOffY
  { n with
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay
    gfxOffY := max (-16) (min 16 gfx) }

/-- One ordinary dry `NPC.UpdateNPC` slot. `frameHeight` is one for the
dedicated-server oracle and thirty for the desktop NPC_34 texture. -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player)
    (random : UnifiedRandom) (enemy : Enemy) :
    Except String Step := do
  if !enemy.body.active then return { enemy, random }
  if enemy.body.life ≤ 0 then throw "Cursed Skull death belongs to the combat phase"
  if enemy.body.ai2 != 0 || enemy.body.ai3 != 0 then
    throw "Cursed Skull has nonordinary style-10 AI parameters"
  let mut n := ai players (prepare enemy.body)
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  if n.velocity.x.abs < 0.005 then n := { n with velocity.x := 0 }
  n := { n with
    oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity
    frameCounter := n.frameCounter + 1 }
  if n.frameCounter ≥ 4 then n := { n with frameCounter := 0, frameY := n.frameY + world.frameHeight }
  if n.frameY ≥ world.frameHeight * cursedSkullFrameCount then n := { n with frameY := 0 }
  let (checked, nearby) := OriginalGreenSlime.checkActive world players n
  return { enemy := { body := { checked with justHit := false, netUpdate := false } }, random, nearby }

def Enemy.contactSource (enemy : Enemy) : Option (Int × String) :=
  if enemy.body.active && enemy.body.damage > 0 then some (enemy.body.damage, "Cursed Skull") else none

/-- DrawNPCDirect arguments. `source.y` is the runtime frame.Y in pixels. -/
structure NpcSprite where
  source : OriginalForestGuideArrow.DustFrame
  position : Vector
  origin : Vector
  rgba : UInt32 := 0xffffffff
  rotation : Float32
  scale : Float32
  flip : Bool

/-- NPC_34 is 28 × 90, with three 28 × 30 frames. Update uses pixel frame.Y. -/
def npcSprite (enemy : Enemy) (screen : Vector) : NpcSprite :=
  let n := enemy.body
  let x := n.position.x - screen.x + single (n.width / 2)
  let y := n.position.y - screen.y + single n.height - 30 * n.scale +
    15 * n.scale + 4 + n.gfxOffY
  { source := ⟨0, n.frameY, 28, 30⟩
    position := ⟨x, y⟩, origin := ⟨14, 15⟩
    rotation := n.rotation, scale := n.scale, flip := n.spriteDirection == 1 }

def cursedBuff : Nat := 23

private def nativeDebuffs : Array Nat :=
  #[20, 21, 22, 23, 24, 25, 28, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 43, 44, 46, 47,
    67, 68, 69, 70, 72, 80, 86, 87, 88, 89, 94, 103, 119, 120, 137, 144, 145, 146, 147, 148,
    149, 153, 156, 157, 158, 160, 163, 164, 169, 183, 186, 189, 194, 195, 196, 197, 199, 203,
    204, 215, 320, 321, 323, 324, 332, 333, 334, 344, 350, 353, 395, 397, 398, 399, 400]

/-- Native `AddBuff` retains existing debuffs and evicts the first ordinary
buff when all slots are full. A full array of debuffs rejects the new buff. -/
private def addCurse (health : OriginalPlayerLiquidHealth.Health) : OriginalPlayerLiquidHealth.Health :=
  if health.buffType.contains cursedBuff || health.buffType.contains 0 then health.addBuff cursedBuff 240
  else match health.buffType.findIdx? (fun id => !nativeDebuffs.contains id) with
    | some index =>
      (health.delBuff index).addBuff cursedBuff 240
    | none => health

/-- `StatusFromNPC` rolls after an effective, nonlethal contact. Immunity to
Cursed blocks `AddBuff` after the roll, preserving that random consumption. -/
def contact (enemy : Enemy) (body : OriginalPlayerCollision.Body)
    (health : OriginalPlayerLiquidHealth.Health) (defense : Int) (pool : DustPool)
    (random : UnifiedRandom) (immuneToCursed : Bool := false)
    (text : Option OriginalCombatTextChannel.Channel := none)
    (client : OriginalPlayerHurt.Client := {}) : Option OriginalAngryBones.ContactHit := do
  let hit ← OriginalAngryBones.contactNpc enemy.toCombatNpc enemy.body.damage body health defense pool random text client
  if hit.boundary.isSome || hit.health.statLife ≤ 0 then return hit
  let (roll, random) := hit.effects.random.next 0 3
  let health := if roll == 0 && !immuneToCursed then addCurse hit.health else hit.health
  return { hit with health, effects.random := random }

def bannerId : Nat := 102
def bannerItem : UInt16 := 2911

def Enemy.recordBannerKill (_ : Enemy) (credited : Bool)
    (banner : OriginalAngryBones.BannerProgress) : OriginalAngryBones.BannerProgress :=
  if credited then banner.recordKill else banner

open OriginalCombat.Loot (Context Result)

private def dropItem (env : Context) (position : Vector) (item : UInt16) (stack : Nat)
    (result : Result) : Result :=
  -- Nazar uses the accessory pool already present for the Tally Counter.
  let prefixItem := if item == 891 then 3095 else item
  let (prefixId, random) := OriginalRecipeCatalog.rollNaturalPrefix prefixItem result.random
  let (drop, random) := OriginalItems.rollDropAt position
    { OriginalItems.Stack.ofItem item stack with prefixId } random env.grid
  { drops := result.drops.push drop, random }

private def commonDrop (env : Context) (position : Vector) (item : UInt16)
    (denominator : Int) (result : Result) : Bool × Result :=
  let (roll, random) := result.random.next 0 denominator
  if roll != 0 then (false, { result with random }) else
  let (_, random) := random.next 1 2
  (true, dropItem env position item 1 { result with random })

private def boneDrops (env : Context) (position : Vector) (result : Result) : Result := Id.run do
  let mut result := result
  for (item, denominator) in #[(932, 250), (3095, 100), (327, 65)] do
    let (dropped, next) := commonDrop env position item denominator result
    if dropped then return next
    result := next
  let (_, random) := result.random.next 0 1
  let (stack, random) := random.next 1 4
  return dropItem env position 154 stack.toNat { result with random }

/-- Ordinary zero-luck Classic loot after hit effects, with no statue,
seasonal or deep-world global drop conditions. Nazar is its own final roll. -/
def dropLoot (env : Context) (enemy : Enemy) (random : UnifiedRandom) : Result :=
  let n := enemy.body
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let creamSoda := (commonDrop env position 4018 70 { random }).2
  let bones := boneDrops env position creamSoda
  let nazar := (commonDrop env position 891 100 bones).2
  let coins := OriginalCombat.Loot.dropMoneyAt env position n.value nazar
  OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) coins

structure HitEffects extends OriginalAngryBones.HitEffects where
  /-- Native `HitEffect` sets `noLight` on every emitted particle. -/
  noLight : Bool := true

/-- Each hit emits paired blue and bone dust. The first of each pair has no
gravity. Lethal hits emit fifteen pairs and use no gore slots. -/
def hitEffects (pool : DustPool) (dealt : Int)
    (hit : OriginalStarterMelee.Npc) (random : UnifiedRandom) : HitEffects := Id.run do
  let count := if hit.life ≤ 0 then 15
    else ((Float.ofInt dealt / Float.ofInt hit.lifeMax * 30).ceil).toUInt64.toNat
  let speed : Vector := ⟨-hit.velocity.x * 0.2, -hit.velocity.y * 0.2⟩
  let mut result : HitEffects := { dust := #[], pool, random }
  for _ in [:count] do
    for (typeId, alpha, scale, noGravity) in #[(15, 100, 1.8, true), (26, 0, 0.9, false)] do
      let request : DustRequest := {
        position := hit.position, width := hit.width, height := hit.height
        typeId, alpha, scale, speed }
      let (pool, particle, random) := result.pool.create request result.random
      let particle := particle.map fun p =>
        { p with velocity := ⟨p.velocity.x * 1.3, p.velocity.y * 1.3⟩, noGravity }
      result := { result with dust := particle.elim result.dust result.dust.push, pool, random }
  return result

end Lean4Terria.OriginalCursedSkull
