import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalGore
import Lean4Terria.OriginalStarterMelee

/-!
# Friendly NPCs hurt by hostile NPCs (Terraria 1.4.5.8, Classic, single player)

The part of `NPC.UpdateNPC` that every friendly NPC runs after its AI, the `immune[]` countdown
and gravity, and before `UpdateCollision`: `CheckLifeRegen` and `GetHurtByOtherNPCs`
(`BeHurtByOtherNPC` → `StrikeNPCNoInteraction` → `HitEffect` → `checkDead` → `NPCLoot`).
It is shared by the bound Mechanic (type 123), the rescued Mechanic (124) and the Guide (22).

- `CheckLifeRegen`: below full life `friendlyRegen` grows by 1 (Guide 22: +5 more, 209: +9 more);
  above 180 it resets and the NPC regains 1 life.
- `GetHurtByOtherNPCs`: skipped while `immune[255] != 0`; otherwise the first active, non-friendly
  NPC with damage whose hitbox intersects, in slot order, deals `DamageVar(damage)` through
  `StrikeNPC(damage, 6, away from the attacker's centre)` and sets `immune[255] = 30`.
- A town NPC also rolls the panic timer `ai[1] = 300 + Next(300)` after the combat text.
- `HitEffect` (types 22, 123, 124): type 5 blood, `⌈damage / lifeMax × 100⌉` on a hit, 50 and the
  gores 151, 152, 152, 153, 153 on death. A live pool allocates each request before its two draws.
- Death (`checkDead`): a town NPC (124) sets `noSpawnCycle`, announces the death (`Lang.misc[19]`)
  and, only when every active player is Hardcore, drops a tombstone projectile; then
  `NPCDeath1` and `NPCLoot`. The NPC is inactive but the same update still runs its collision.

The Guide's death (ladybug luck, Guide drops) and the talking-partner panic are not modelled; the
callers report them as boundaries.
-/

namespace Lean4Terria.OriginalTownCombat

open OriginalCollision (Vector single)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)

/-- An NPCHit / NPCDeath instance `LegacySoundPlayer` resolved at its original call site. The
values are final: the device neither rolls nor scales them again. -/
abbrev PlayedSound := OriginalStarterMelee.NpcSound

/-- The client's `SoundEngine.PlaySound(type, position, style)` during a strike: the stream after
the sound's own draws (an audible NPCHit or NPCDeath draws its pitch, `Next(-10, 11)`, from the
stream that is current, here UpdateNPCs) and the instance, if one is heard. -/
abbrev SoundHook := Nat → Nat → Vector → UnifiedRandom → UnifiedRandom × Option PlayedSound

/-- Effects that depend on the client: the dust pool (disabled = no dust, as on the headless host
and in the previous preview), the loot context, and whether every active player is Hardcore. -/
structure Effects where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  /-- None is the headless contract: the requests are made but nothing is resolved or drawn. -/
  playSound : Option SoundHook := none
  dustPool : DustPool := { enabled := false }
  loot : OriginalCombat.Loot.Context := {}
  allHardcore : Bool := false
  /-- With a live pool, NewGore reserves a slot before drawing its velocity.
  Without one this API retains the event-only, empty-pool fixture interface. -/
  goreContext : Option (OriginalCollision.Grid × OriginalGore.Pool) := none

/-- One other NPC slot as `GetHurtByOtherNPCs` reads it. -/
structure Other where
  slot : Nat
  typeId : Int
  active : Bool := true
  friendly : Bool := false
  damage : Int := 0
  position : Vector
  width : Nat
  height : Nat
  deriving Repr

def Other.center (o : Other) : Vector :=
  ⟨o.position.x + single o.width / 2, o.position.y + single o.height / 2⟩

/-- Types whose `GetMeleeCollisionData` replaces the hitbox (none is a dungeon enemy). -/
def customMeleeHitbox (typeId : Int) : Bool :=
  #[430, 436, 591, 494, 495, 400, 460, 417, 466, 576, 577, 552, 553, 554, 668].contains typeId

structure Gore where
  typeId : Nat
  position : Vector
  velocity : Vector
  deriving BEq, Repr

/-- `DropTombstoneTownNPC`: a tombstone projectile request (`NewProjectile`, owner `myPlayer`). -/
structure Tombstone where
  typeId : Nat
  /-- Center passed to `NewProjectile`; the created projectile stores a top-left position. -/
  position : Vector
  velocity : Vector
  deriving BEq, Repr

structure Hit where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  attacker : Nat
  rolled : Int
  damage : Int
  direction : Int
  dust : Array DustParticle := #[]
  /-- `NPCHit1`, and `NPCDeath1` after it, play at the NPC. The NPC is inactive afterwards. -/
  killed : Bool := false
  gores : Array Gore := #[]
  drops : Array OriginalItems.Drop := #[]
  /-- Town NPCs: `ai[1] = 300 + Next(300)` (this value), `ai[0] = 1`, `ai[2] = localAI[3] = 0`. -/
  panic : Option Int := none
  tombstone : Option Tombstone := none
  /-- Town NPC death: the `Lang.misc[19]` announcement ("{0} was slain...") for this NPC. -/
  deathText : Option String := none
  gorePool : Option OriginalGore.Pool := none
  /-- What a client's sound hook resolved for this strike, in call order: `HitSound` after
  `HitEffect` (StrikeNPC_Inner IL_0947), `DeathSound` in `checkDead` before `NPCLoot` (IL_05d0). -/
  sounds : Array PlayedSound := #[]
  /-- `SoundEngine.PlaySound(LegacySoundStyle, …)` calls of this strike (HitSound, DeathSound).
  Each one takes a sample from the client-wide `SoundStyle._random` in `GetRandomPitch`, before
  any audio gate and whatever its pitch variance (0 for NPCHit1 / NPCDeath1). -/
  styleCalls : Nat := 0
  deriving Repr

/-- The friendly NPC fields this path reads and writes. -/
structure Target where
  typeId : Int
  townNPC : Bool
  position : Vector
  velocity : Vector
  width : Nat
  height : Nat
  life : Int
  lifeMax : Int := 250
  damage : Int := 10
  defense : Int
  knockBackResist : Float32 := 0.5
  direction : Int
  immune255 : Int
  friendlyRegen : Int
  deriving Repr

/-- `CheckLifeRegen` (no Dryad's Ward). -/
def lifeRegen (t : Target) : Target :=
  if t.life ≥ t.lifeMax then t else
  let bonus : Int := if t.typeId == 22 then 5 else if t.typeId == 209 then 9 else 0
  let regen := t.friendlyRegen + 1 + bonus
  if regen > 180 then { t with life := t.life + 1, friendlyRegen := 0 } else { t with friendlyRegen := regen }

private def pixel (v : Float32) : Int := v.toInt32.toInt

private def intersects (a b : Vector) (aw ah bw bh : Nat) : Bool :=
  pixel b.x < pixel a.x + aw && pixel a.x < pixel b.x + bw &&
    pixel b.y < pixel a.y + ah && pixel a.y < pixel b.y + bh

private def emitDust (pool : DustPool) (rng : UnifiedRandom) (request : DustRequest) (count : Nat) :
    DustPool × UnifiedRandom × Array DustParticle := Id.run do
  let mut pool := pool
  let mut rng := rng
  let mut dust := #[]
  for _ in [:count] do
    let (next, particle, nextRng) := pool.create request rng
    pool := next
    rng := nextRng
    if let some p := particle then dust := dust.push p
  return (pool, rng, dust)

/-- `DropTombstoneTownNPC` (single player). -/
def tombstone (t : Target) (rng : UnifiedRandom) : Tombstone × UnifiedRandom := Id.run do
  let (sideRoll, rng) := rng.next 0 2
  let side : Float32 := if sideRoll == 0 then -0.1 else 0.1
  let (base, rng) := rng.next (-35) 36
  let mut push : Float32 := single base * 0.1
  let mut rng := rng
  while push < 2 && push > -2 do
    let (more, next) := rng.next (-30) 31
    rng := next
    push := push + single more * 0.1
  let (style, afterStyle) := rng.next 0 6
  -- Types 17 and 441 (merchants) use 527 + Next(5); no dungeon resident is one of them.
  let typeId : Nat := if style == 0 then 43 else 200 + style.toNat
  let (sx, afterX) := afterStyle.next 10 30
  let (sy, afterY) := afterX.next (-40) (-20)
  let position : Vector := ⟨t.position.x + single (t.width / 2 : Nat), t.position.y + single (t.height / 2 : Nat)⟩
  return ({ typeId, position, velocity := ⟨single sx * side + push, single sy * 0.1⟩ }, afterY)

/-- The type-specific rules of `NPCLoot` for the deaths supported here. -/
private def typeDrops (typeId : Int) : Array (UInt16 × Int) :=
  if typeId == 124 then #[(4818, 8)] else #[]

/-- `Item.GetRollablePrefixes` for Combat Wrench (4818), in native order.
This non-recipe item is absent from the crafting catalog's prefix metadata.
Its fourteen rolls all survive the item's stat-rounding validation. -/
def combatWrenchPrefixes : Array UInt8 :=
  #[36, 37, 38, 53, 54, 55, 39, 40, 56, 41, 57, 59, 60, 61]

/-- The actual natural prefix roll for the Mechanic's Combat Wrench drop.
After the one-in-four plain roll, draw an index into the native pool. The
four reduced-chance prefixes (39, 40, 41, 56) require an additional zero
`Next(3)` roll; the other two results leave the item unprefixed. -/
def rollDroppedPrefix (item : UInt16) (rng : UnifiedRandom) : UInt8 × UnifiedRandom :=
  if item != 4818 then OriginalRecipeCatalog.rollNaturalPrefix item rng else
  let (plain, rng) := rng.next 0 4
  if plain == 0 then (0, rng) else
  let (index, rng) := rng.next 0 combatWrenchPrefixes.size
  let chosen := combatWrenchPrefixes[index.toNat]!
  if (#[39, 40, 41, 56] : Array UInt8).contains chosen then
    let (keep, rng) := rng.next 0 3
    (if keep == 0 then chosen else 0, rng)
  else (chosen, rng)

private def commonDrop (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16) (chance : Int)
    (result : OriginalCombat.Loot.Result) : OriginalCombat.Loot.Result :=
  let (roll, random) := result.random.next 0 chance
  if roll != 0 then { result with random } else
  let (_, random) := random.next 1 2
  let (prefixId, random) := rollDroppedPrefix item random
  let (drop, random) := OriginalItems.rollDropAt position { OriginalItems.Stack.ofItem item 1 with prefixId } random env.grid
  { drops := result.drops.push drop, random }

/-- The death branch: `HitEffect` dust and gores, then `checkDead` (town announcement and
tombstone, `NPCLoot`). The caller has already applied the strike. -/
def die (effects : Effects) (pool : DustPool) (rng : UnifiedRandom) (t : Target) (direction : Int) :
    Except String (DustPool × UnifiedRandom × Hit) := do
  let request : DustRequest := {
    position := t.position, width := t.width, height := t.height, typeId := 5
    speed := ⟨2.5 * single direction, -2.5⟩ }
  let (pool, rng, dust) := emitDust pool rng request 50
  let mut rng := rng
  let mut gores : Array Gore := #[]
  let mut context := effects.goreContext
  for (typeId, dy) in #[((151 : Nat), (0 : Float32)), (152, 20), (152, 20), (153, 34), (153, 34)] do
    let position : Vector := ⟨t.position.x, t.position.y + dy⟩
    match context with
    | some (grid, particles) =>
      let (particles, slot, next) ← particles.spawn grid { typeId, position, velocity := t.velocity } rng
      context := some (grid, particles)
      rng := next
      if let some p := particles.particles[slot]? then
        gores := gores.push { typeId, position := p.position, velocity := p.velocity }
    | none =>
      let (up, next) := rng.next 10 31
      let (side, next) := next.next (-20) 21
      rng := next
      gores := gores.push { typeId, position
                            velocity := ⟨t.velocity.x + single side * 0.1, t.velocity.y - single up * 0.1⟩ }
  -- StrikeNPC_Inner IL_0b1a..0b5d: HitSound (NPCHit1) after HitEffect, before checkDead.
  let mut sounds : Array PlayedSound := #[]
  if let some play := effects.playSound then
    let (next, played) := play 3 1 t.position rng
    rng := next
    sounds := sounds ++ played.toArray
  let mut tomb : Option Tombstone := none
  if t.townNPC && effects.allHardcore then
    let (stone, next) := tombstone t rng
    tomb := some stone
    rng := next
  -- checkDead IL_05d0: DeathSound (NPCDeath1) before NPCLoot.
  if let some play := effects.playSound then
    let (next, played) := play 4 1 t.position rng
    rng := next
    sounds := sounds ++ played.toArray
  let position := OriginalCombat.Loot.dropPosition t.position t.width t.height
  let mut loot : OriginalCombat.Loot.Result := { random := rng }
  for (item, chance) in typeDrops t.typeId do
    loot := commonDrop effects.loot position item chance loot
  let coins := OriginalCombat.Loot.dropMoneyAt effects.loot position 0 loot
  let heals := OriginalCombat.Loot.dropHealsAt effects.loot position (t.lifeMax > 1 && t.damage > 0) coins
  return (pool, heals.random, { attacker := 0, rolled := 0, damage := 0, direction, dust, killed := true, gores
                                drops := heals.drops, tombstone := tomb, gorePool := context.map (·.2), sounds
                                styleCalls := 2 })

/-- `GetHurtByOtherNPCs` → `BeHurtByOtherNPC` for the friendly NPC in `slot`. The returned
target holds the new life, velocity, direction and `immune255`. A Guide death is reported. -/
def hurt (slot : Nat) (others : Array Other) (effects : Effects) (pool : DustPool) (rng : UnifiedRandom)
    (t : Target) : Except String (Target × Option Hit × DustPool × UnifiedRandom) := do
  if effects.text.boundary.isSome || t.immune255 != 0 then return (t, none, pool, rng)
  let some attacker := (others.qsort (·.slot < ·.slot)).find? fun o =>
      o.slot != slot && o.active && !o.friendly && o.damage > 0 &&
      intersects t.position o.position t.width t.height o.width o.height &&
      o.typeId != 624 && o.typeId != 690
    | return (t, none, pool, rng)
  if customMeleeHitbox attacker.typeId then throw s!"attacker type {attacker.typeId} uses a custom melee hitbox"
  let (rolled, rng) := OriginalStarterMelee.damageVar attacker.damage rng
  let center : Vector := ⟨t.position.x + single t.width / 2, t.position.y + single t.height / 2⟩
  let direction : Int := if attacker.center.x > center.x then -1 else 1
  let npc : OriginalStarterMelee.Npc := {
    position := t.position, velocity := t.velocity, width := t.width, height := t.height
    life := t.life, lifeMax := t.lifeMax, defense := t.defense, knockBackResist := t.knockBackResist
    friendly := true }
  let strike := OriginalStarterMelee.strikeNpcWithText npc rolled 6 direction false rng {effects.text with fromPlayer := 255, fromNet := false}
  let struck := strike.npc
  let damage := strike.damage
  let rng := strike.random
  if strike.text.boundary.isSome then
    return (t, some {attacker := attacker.slot, rolled, damage, direction, text := strike.text}, pool, rng)
  let (panic, rng) := if t.townNPC then
      let (roll, rng) := rng.next 0 300
      (some (300 + roll), rng)
    else (none, rng)
  let t := { t with life := struck.life, velocity := struck.velocity
                    direction := if t.townNPC then direction else t.direction }
  if struck.life ≤ 0 then
    if t.typeId == 22 then throw "the Guide was killed by another NPC (NPCLuck and Guide drops not ported)"
    let (pool, rng, hit) ← die effects pool rng t direction
    return ({ t with immune255 := 30 },
      some { hit with attacker := attacker.slot, rolled, damage, panic, text := strike.text
                      deathText := if t.townNPC then some "LegacyMisc.19" else none }, pool, rng)
  let count := ((damage.toFloat / t.lifeMax.toFloat * 100).ceil.toUInt64.toNat)
  let request : DustRequest := {
    position := t.position, width := t.width, height := t.height, typeId := 5
    speed := ⟨single direction, -1⟩ }
  let (pool, rng, dust) := emitDust pool rng request count
  -- StrikeNPC_Inner IL_0b1a..0b5d: HitSound (NPCHit1) after HitEffect's dust.
  let (rng, played) := match effects.playSound with
    | some play => play 3 1 t.position rng
    | none => (rng, none)
  return ({ t with immune255 := 30 }, some { attacker := attacker.slot, rolled, damage, direction, dust, panic, text := strike.text, sounds := played.toArray, styleCalls := 1 }, pool, rng)

end Lean4Terria.OriginalTownCombat
