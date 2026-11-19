import Lean4Terria.OriginalAngryBones
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalDarkCaster
import Lean4Terria.OriginalDungeonMechanic
import Lean4Terria.OriginalCursedSkull
import Lean4Terria.OriginalDungeonSlime
import Lean4Terria.OriginalDungeonSpikeBall
import Lean4Terria.OriginalOldSlime
import Lean4Terria.OriginalNightEnemies
import Lean4Terria.OriginalNpcSound
import Lean4Terria.OriginalManaHeat
import Lean4Terria.OriginalManaRefillEffects
import Lean4Terria.OriginalForestBird
import Lean4Terria.OriginalForestCritter
import Lean4Terria.OriginalForestGuide
import Lean4Terria.OriginalForestGuideArrow
import Lean4Terria.OriginalForestSnail
import Lean4Terria.OriginalGhostSpawn
import Lean4Terria.OriginalPlayerCollision
import Lean4Terria.OriginalPlayerDeath
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalCombatTextChannel
import Lean4Terria.OriginalSkeletron
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalWaterDust

namespace Lean4Terria.OriginalCombat

open OriginalCollision (Grid single tileIndex)
open OriginalForestBird (Bird)
open OriginalForestCritter (Critter)
open OriginalForestGuide (Resident)
open OriginalForestSnail (Snail)
open OriginalGhostSpawn (Ghost)
open OriginalGreenSlime (Slime)
open OriginalItems (Drop)
open OriginalPlayerCollision (Body)
open OriginalPlayerLiquidHealth (Health Event)
open OriginalStarterMelee (Npc Owner Stab Use Weapon)
open OriginalForestOldManCurse (Part headType handType)

inductive Actor where
  | empty
  | greenSlime (slime : Slime)
  | tombstoneGhost (ghost : Ghost)
  | townResident (resident : Resident) (bow : OriginalForestGuideBow.State := {})
  | boundMechanic (mechanic : OriginalDungeonMechanic.Bound) (friendlyRegen : Int := 0)
  | angryBones (enemy : OriginalAngryBones.Enemy)
  | darkCaster (enemy : OriginalDarkCaster.Enemy)
  | dungeonSlime (slime : Slime)
  | cursedSkull (enemy : OriginalCursedSkull.Enemy)
  | oldShakingChest (enemy : OriginalOldSlime.Enemy)
  | dungeonSpikeBall (enemy : OriginalDungeonSpikeBall.Enemy)
  /-- Zombie 3 / Demon Eye 2 (`OriginalNightEnemies`, released scoped module). -/
  | nightEnemy (enemy : OriginalNightEnemies.Enemy)
  | forestBunny (critter : Critter)
  | forestBird (bird : Bird) (immune : Nat := 0)
  | forestSnail (snail : Snail)
  | skeletronPart (part : Part)
  | savedNpc (typeId : Int)
  deriving Inhabited

def Actor.active : Actor → Bool
  | .empty => false
  | .greenSlime slime => slime.active
  | .tombstoneGhost ghost => ghost.active
  | .townResident resident _ => resident.active
  | .boundMechanic mechanic _ => mechanic.body.active
  | .angryBones enemy => enemy.body.active
  | .darkCaster enemy => enemy.body.active
  | .dungeonSlime slime => slime.active
  | .cursedSkull enemy => enemy.body.active
  | .oldShakingChest enemy => enemy.body.active
  | .dungeonSpikeBall enemy => enemy.body.active
  | .nightEnemy enemy => enemy.body.active
  | .forestBunny critter => critter.active
  | .forestBird bird _ => bird.active
  | .forestSnail snail => snail.active
  | .skeletronPart part => part.active
  | .savedNpc _ => true

def Actor.typeId : Actor → Int
  | .empty => 0
  | .greenSlime _ => 1
  | .tombstoneGhost ghost => ghost.npcType.toNat
  | .townResident resident _ => resident.kind.typeId
  | .boundMechanic _ _ => OriginalDungeonMechanic.boundType
  | .angryBones enemy => enemy.npcType
  | .darkCaster enemy => enemy.npcType
  | .dungeonSlime _ => OriginalDungeonSlime.typeId
  | .cursedSkull _ => OriginalCursedSkull.npcType
  | .oldShakingChest _ => OriginalOldSlime.typeId
  | .dungeonSpikeBall _ => OriginalDungeonSpikeBall.typeId
  | .nightEnemy enemy => enemy.npcType
  | .forestBunny critter => critter.type
  | .forestBird bird _ => bird.kind.typeId
  | .forestSnail snail => snail.type
  | .skeletronPart part => part.typeId
  | .savedNpc typeId => typeId

inductive Projectile where
  | empty
  | stab (stab : Stab)
  | guideArrow (arrow : OriginalForestGuideArrow.Arrow)
  | gravestone (grave : OriginalPlayerDeath.Gravestone)
  deriving Inhabited

def Projectile.active : Projectile → Bool
  | .empty => false
  | .stab blade => blade.active
  | .guideArrow arrow => arrow.active
  | .gravestone grave => grave.timeLeft > 0

private def Projectile.direction : Projectile → Int
  | .empty => 1
  | .stab blade => blade.direction
  | .guideArrow arrow => arrow.direction
  | .gravestone grave => grave.direction

private def Projectile.reuseSlot (projectile previous : Projectile) : Projectile :=
  match projectile with
  | .guideArrow arrow => .guideArrow { arrow with direction := previous.direction }
  | _ => projectile

inductive AngryBonesSound where
  | hit
  | killed

/-- Each phase keeps the random stream used by the corresponding vanilla `SwapRandom` call.
The player phase shares its stream with the rest of `Player.Update`. -/
structure State where
  /-- The one world CombatText pool; retained through player death and drawing. -/
  texts : OriginalCombatText.State := {}
  textFonts : Option OriginalCombatText.Fonts := none
  textContext : OriginalCombatText.Context := {}
  textBoundary : Option String := none
  use : Use := {}
  weapon : Option Nat := none
  projectiles : Array Projectile := #[]
  guideDust : Array OriginalForestGuideArrow.DustParticle := #[]
  /-- Main.maxDustToDraw; physical allocation still has6000 slots. -/
  dustDrawBudget : Nat := 6000
  manaDust : Array OriginalManaHeat.Particle := #[]
  manaRefillDust : Array OriginalManaRefillEffects.Particle := #[]
  /-- The `Dust.dustWater()` cohort (rain splashes; `OriginalWaterDust`): its slot records, ascending, and
  the global `Main.dust[6000]` that a refused `NewDust` returns. -/
  waterDust : Array OriginalWaterDust.Owned := #[]
  dustSentinel : OriginalWaterDust.Particle := {}
  skeletronRoared : Bool := false
  oldManRetired : Bool := false
  oldManChanged : Bool := false
  skeletronDefeated : Bool := false
  /-- Persistent world progress for savedMechanic and rescued resident export. -/
  mechanicRescued : Bool := false
  mechanicNames : Array String := #[]
  /-- Friendly NPC strikes this tick (`NPCHit1` / `NPCDeath1`). -/
  mechanicSounds : Array AngryBonesSound := #[]
  /-- NPCHit / NPCDeath instances a client's sound gates resolved in this update's NPC phase,
  in call order. With a client these replace the fixed `mechanicSounds`. -/
  npcSounds : Array OriginalTownCombat.PlayedSound := #[]
  /-- `PlaySound(LegacySoundStyle, …)` calls of this update's NPC phase: the session advances the
  client-wide `SoundStyle._random` by one sample for each. -/
  npcStyleCalls : Nat := 0
  /-- HitSound / DeathSound instances of this update's strikes by the player (melee in the player
  phase, the stab in the projectile phase), resolved at the strike, in order. -/
  strikeSounds : Array OriginalStarterMelee.NpcSound := #[]
  /-- `PlaySound(LegacySoundStyle, …)` calls of those strikes, for the client-wide style stream. -/
  strikeStyleCalls : Nat := 0
  /-- Drops created during `UpdateNPCs`, consumed before `UpdateProjectiles`. -/
  npcDrops : Array Drop := #[]
  /-- Original Mechanic death fragment requests emitted this tick. -/
  mechanicGores : Array OriginalDungeonMechanic.Gore := #[]
  gorePool : OriginalGore.Pool := {}
  /-- DrawGore's texture state persists after a particle disappears. -/
  goreLoadedTypes : Array Nat := #[]
  npcTileEdits : Array OriginalDoor.TileEdit := #[]
  doorSounds : Array OriginalDoor.Sound := #[]
  angryBonesBanner : OriginalAngryBones.BannerProgress := {}
  angryBonesSounds : Array AngryBonesSound := #[]
  darkCasterBanner : OriginalAngryBones.BannerProgress := {}
  darkCasterSounds : Array OriginalCollision.Vector := #[]
  dungeonSlimeBanner : OriginalAngryBones.BannerProgress := {}
  /-- NPC 71 strikes this tick, in order: `NPCHit1` for `.hit`, `NPCDeath1` for `.killed`. -/
  dungeonSlimeSounds : Array AngryBonesSound := #[]
  cursedSkullBanner : OriginalAngryBones.BannerProgress := {}
  /-- Banners of the night enemies by `Enemy.bannerInfo` id (Zombie 3, Demon Eye 2). -/
  zombieBanner : OriginalAngryBones.BannerProgress := {}
  demonEyeBanner : OriginalAngryBones.BannerProgress := {}
  /-- NPC 34 strikes this tick: `NPCHit2` / `NPCDeath2`. -/
  cursedSkullSounds : Array AngryBonesSound := #[]
  waterSphereHit : Bool := false
  waterSphereKilled : Bool := false
  /-- Indices are native NPC slots, including gaps and inactive occupants. -/
  actors : Array Actor := #[]
  protectedSlots : Array Nat := Array.replicate 200 0
  nearbyActiveNPCs : Float32 := 0
  noSpawnCycle : Bool := false
  npcBoundary : Option (Nat × String) := none
  npcRandom : OriginalGreenSlime.Rng := OriginalGreenSlime.Rng.seed 0
  projectileRandom : UnifiedRandom := UnifiedRandom.seed 0

def owner (body : OriginalPlayerCollision.Body) : Owner :=
  { position := body.position, width := body.width, height := body.height, gfxOffY := body.gfxOffY }

private def localNpcTarget (body : Body) (dead : Bool) (itemAnimation : Nat := 0) :
    OriginalGreenSlime.Player :=
  { index := 0, dead, position := body.position, width := body.width, height := body.height
    itemAnimation }

def State.insertProjectile (state : State) (projectile : Projectile) : State :=
  if let some index := state.projectiles.findIdx? (fun slot => !slot.active) then
    let projectile := projectile.reuseSlot state.projectiles[index]!
    { state with projectiles := state.projectiles.set! index projectile }
  else if state.projectiles.size < 1000 then
    { state with projectiles := state.projectiles.push projectile }
  else state

def State.clearStabs (state : State) : State :=
  { state with projectiles := state.projectiles.map fun
      | .stab stab => .stab { stab with active := false, timeLeft := 0 }
      | slot => slot }

def Actor.slime? : Actor → Option Slime
  | .greenSlime slime => some slime
  | _ => none

def Actor.ghost? : Actor → Option Ghost
  | .tombstoneGhost ghost => some ghost
  | _ => none

def Actor.resident? : Actor → Option Resident
  | .townResident resident _ => some resident
  | _ => none

def Actor.boundMechanic? : Actor → Option OriginalDungeonMechanic.Bound
  | .boundMechanic mechanic _ => some mechanic
  | _ => none

def Actor.boundMechanicState? : Actor → Option OriginalDungeonMechanic.HurtBound
  | .boundMechanic mechanic friendlyRegen => some { bound := mechanic, friendlyRegen }
  | _ => none

def Actor.angryBones? : Actor → Option OriginalAngryBones.Enemy
  | .angryBones enemy => some enemy
  | _ => none

def Actor.darkCaster? : Actor → Option OriginalDarkCaster.Enemy
  | .darkCaster enemy => some enemy
  | _ => none

def Actor.cursedSkull? : Actor → Option OriginalCursedSkull.Enemy
  | .cursedSkull enemy => some enemy
  | _ => none

def Actor.dungeonSlime? : Actor → Option Slime
  | .dungeonSlime slime => some slime
  | _ => none

def Actor.oldShakingChest? : Actor → Option OriginalOldSlime.Enemy
  | .oldShakingChest enemy => some enemy
  | _ => none

def Actor.dungeonSpikeBall? : Actor → Option OriginalDungeonSpikeBall.Enemy
  | .dungeonSpikeBall enemy => some enemy
  | _ => none

def Actor.nightEnemy? : Actor → Option OriginalNightEnemies.Enemy
  | .nightEnemy enemy => some enemy
  | _ => none

def Actor.bow? : Actor → Option OriginalForestGuideBow.State
  | .townResident _ bow => some bow
  | _ => none

def Actor.bunny? : Actor → Option Critter
  | .forestBunny critter => some critter
  | _ => none

def Actor.bird? : Actor → Option Bird
  | .forestBird bird _ => some bird
  | _ => none

def Actor.snail? : Actor → Option Snail
  | .forestSnail snail => some snail
  | _ => none

def Actor.skeletron? : Actor → Option Part
  | .skeletronPart part => some part
  | _ => none

private def Actor.toNpc : Actor → Npc
  | .empty | .savedNpc _ =>
    { active := false, position := ⟨0, 0⟩, width := 0, height := 0, life := 0, lifeMax := 0, defense := 0 }
  | .townResident resident _ =>
    { active := resident.active, position := resident.position, velocity := resident.velocity
      width := resident.width, height := resident.height, life := resident.life, lifeMax := resident.lifeMax
      defense := resident.defense, friendly := true, dontTakeDamage := true }
  | .boundMechanic mechanic _ =>
    let n := mechanic.body
    { active := n.active, position := n.position, velocity := n.velocity
      width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax
      defense := n.defense, friendly := true }
  | .angryBones enemy => enemy.toCombatNpc
  | .darkCaster enemy => enemy.toCombatNpc
  | .dungeonSlime slime => OriginalDungeonSlime.toCombatNpc slime
  | .cursedSkull enemy => enemy.toCombatNpc
  | .oldShakingChest enemy => enemy.toCombatNpc
  | .dungeonSpikeBall enemy => enemy.toCombatNpc
  | .nightEnemy enemy => enemy.toCombatNpc
  | .forestBunny critter =>
    { active := critter.active, position := critter.position, velocity := critter.velocity
      width := critter.width, height := critter.height, life := critter.life, lifeMax := critter.lifeMax
      defense := critter.defense, friendly := critter.friendly, dontTakeDamage := critter.dontTakeDamage
      immune := (critter.immune[0]?.getD 0).toNat, justHit := critter.justHit }
  | .forestBird bird immune =>
    { active := bird.active, position := bird.position, velocity := bird.velocity
      width := bird.width, height := bird.height, life := bird.life, lifeMax := bird.lifeMax
      defense := 0, friendly := bird.friendly, immune, justHit := bird.justHit, noGravity := bird.noGravity }
  | .forestSnail snail =>
    { active := snail.active, position := snail.position, velocity := snail.velocity
      width := snail.width.toNat, height := snail.height.toNat, life := snail.life, lifeMax := snail.lifeMax
      defense := 0, friendly := snail.friendly, dontTakeDamage := snail.dontTakeDamage
      immune := (snail.immune[0]?.getD 0).toNat, justHit := snail.justHit, noGravity := snail.noGravity
      knockBackResist := snail.knockBackResist, noTileCollide := snail.noTileCollide }
  | .skeletronPart part =>
    { active := part.active, position := part.position, velocity := part.velocity
      width := part.width, height := part.height, life := part.life, lifeMax := part.lifeMax
      defense := part.defense, knockBackResist := 0, noGravity := true, noTileCollide := true
      immune := part.immune }
  | .greenSlime slime =>
    { active := slime.active, position := slime.position, velocity := slime.velocity
      width := slime.width, height := slime.height, life := slime.life, lifeMax := slime.lifeMax
      defense := slime.defense, knockBackResist := slime.knockBackResist, justHit := slime.justHit
      immune := (slime.immune[0]?.getD 0).toNat }
  | .tombstoneGhost ghost =>
    { active := ghost.active, position := ghost.position, velocity := ghost.velocity
      width := ghost.width, height := ghost.height, life := ghost.life, lifeMax := ghost.lifeMax
      defense := ghost.defense, knockBackResist := ghost.knockbackResist, justHit := ghost.justHit
      noGravity := ghost.noGravity, noTileCollide := ghost.noTileCollide, immune := ghost.immune }

private def Actor.applyHit (actor : Actor) (npc : Npc) (target : OriginalGreenSlime.Player)
    (playerInteraction : Bool := false) : Actor :=
  let active := npc.active && npc.life > 0
  match actor with
    | .empty | .savedNpc _ | .townResident _ _ | .boundMechanic _ _ | .oldShakingChest _ | .dungeonSpikeBall _ => actor
    | .angryBones enemy => .angryBones (enemy.applyHit npc (credited := playerInteraction))
    | .darkCaster enemy => .darkCaster (enemy.applyStrike npc #[target] (credited := playerInteraction))
    | .dungeonSlime slime => .dungeonSlime (OriginalDungeonSlime.applyHit slime npc (credited := playerInteraction))
    | .cursedSkull enemy => .cursedSkull (enemy.applyStrike npc (credited := playerInteraction))
    -- The module keeps `active` through HitEffect; the death path has run by now.
    | .nightEnemy enemy => .nightEnemy (enemy.applyHit { npc with active } (credited := playerInteraction))
    | .forestBunny critter => .forestBunny { critter with
        active, life := npc.life, velocity := npc.velocity
        immune := critter.immune.set! 0 npc.immune, justHit := npc.justHit }
    | .forestBird bird _ => .forestBird { bird with
        active, life := npc.life, velocity := npc.velocity, justHit := npc.justHit } npc.immune
    | .forestSnail snail => .forestSnail { snail with
        active, life := npc.life, velocity := npc.velocity
        immune := snail.immune.set! 0 npc.immune, justHit := npc.justHit }
    | .skeletronPart part => .skeletronPart { part with
        active, life := npc.life, velocity := npc.velocity, immune := npc.immune }
    | .greenSlime slime => .greenSlime { slime with
        active, life := npc.life, velocity := npc.velocity
        immune := slime.immune.set! 0 npc.immune, justHit := npc.justHit
        playerInteraction := slime.playerInteraction || playerInteraction }
    | .tombstoneGhost ghost => .tombstoneGhost { ghost with
        active, life := npc.life.toNat, velocity := npc.velocity
        immune := npc.immune, justHit := npc.justHit }

def State.slimes (state : State) : Array Slime :=
  state.actors.filterMap Actor.slime?

def State.ghosts (state : State) : Array Ghost :=
  state.actors.filterMap Actor.ghost?

def State.residents (state : State) : Array Resident :=
  state.actors.filterMap Actor.resident?

def State.bunnies (state : State) : Array Critter :=
  state.actors.filterMap Actor.bunny?

def State.birds (state : State) : Array Bird :=
  state.actors.filterMap Actor.bird?

def State.snails (state : State) : Array Snail :=
  state.actors.filterMap Actor.snail?

/-- Load saved NPCs in archive order. Unsupported saved residents still occupy their slots. -/
def State.loadNpcs (state : State) (saved : OriginalWorld.SavedNpcs) : State :=
  let towns := saved.town.map fun record =>
    match Resident.ofSaved record with
    | some resident => if !saved.shimmeredTypes.contains record.typeId then Actor.townResident resident
      else .savedNpc record.typeId
    | none => .savedNpc record.typeId
  let actors := (towns ++ saved.persistent.map (fun record => Actor.savedNpc record.typeId)).extract 0 200
  let npcBoundary := actors.findIdx? (fun | .savedNpc _ => true | _ => false) |>.map fun slot =>
    (slot, s!"saved NPC {actors[slot]!.typeId}")
  { state with actors, npcBoundary }

def State.setActor (state : State) (slot : Nat) (actor : Actor) : State :=
  if slot ≥ 200 then state else
  let state := { state with protectedSlots := state.protectedSlots.set! slot 2 }
  if slot < state.actors.size then { state with actors := state.actors.set! slot actor }
  else
    { state with actors := (state.actors ++ Array.replicate (slot - state.actors.size) Actor.empty).push actor }

/-- `NPC.UpdateProtectedSpawnSlots`, before `Main.DoUpdateInWorld`. -/
def State.updateProtectedSpawnSlots (state : State) : State :=
  { state with
    angryBonesSounds := #[]
    dungeonSlimeSounds := #[]
    cursedSkullSounds := #[]
    mechanicSounds := #[]
    npcSounds := #[]
    npcStyleCalls := 0
    strikeSounds := #[]
    strikeStyleCalls := 0
    mechanicGores := #[]
    doorSounds := #[]
    waterSphereHit := false
    waterSphereKilled := false
    protectedSlots := state.protectedSlots.mapIdx fun i protection =>
      if state.actors[i]?.any (·.active) then 2 else protection - 1 }

/-- The ordinary free-slot pass of `NPC.GetAvailableNPCSlot`. -/
def State.firstSpawnSlot (state : State) : Option Nat := Id.run do
  for index in [:200] do
    if !(state.actors[index]?.any (·.active)) && state.protectedSlots[index]?.getD 0 == 0 then
      return some index
  return none

private def Actor.curseSlot (actor : Actor) (protectedTicks : Nat) : OriginalForestOldManCurse.Slot :=
  let npc := actor.toNpc
  { typeId := actor.typeId.toNat, active := actor.active, protectedTicks
    position := npc.position, width := npc.width, height := npc.height
    ai3 := if actor.resident?.any (·.curseMarked) then 1 else 0 }

private def curseSlots (actors : Array Actor) (protection : Array Nat) : Array OriginalForestOldManCurse.Slot :=
  (Array.range 200).map fun index =>
    (actors[index]?.getD .empty).curseSlot (protection[index]?.getD 0)

/-- Apply the draw-pass Curse interaction. The old resident stays in its slot
until its regular NPC update. -/
def State.curse (state : State) (time : OriginalForestOldManCurse.Time) (dialogue : OriginalForestGuideDialogue.State)
    (random : OriginalGreenSlime.Rng) : State × OriginalForestGuideDialogue.State × Bool :=
  let result := OriginalForestOldManCurse.click time
    { hovered := true, leftDown := true, leftRelease := true } dialogue
    (curseSlots state.actors state.protectedSlots) random
  let actors := result.markedOldMan.foldl (init := state.actors) fun actors slot =>
    match actors[slot]?.getD .empty with
    | .townResident resident bow => actors.set! slot (.townResident { resident with curseMarked := true } bow)
    | _ => actors
  let state := { state with actors }
  let state := (result.spawn.map fun spawn => state.setActor spawn.part.slot (.skeletronPart spawn.part)).getD state
  (state, result.state, result.announce)

def State.addSlime (state : State) (world : OriginalGreenSlime.World) (x y : Int) : State :=
  match state.firstSpawnSlot with
  | none => state
  | some slot => state.setActor slot (.greenSlime (OriginalGreenSlime.spawn world x y))

/-- The daytime town census recreates the Old Man at the dungeon once his slot is vacant. -/
def State.trySpawnOldMan (state : State) (dungeon : Int × Int) : State := Id.run do
  if dungeon.1 < 0 || dungeon.2 < 0 || state.actors.any (fun actor => actor.active && actor.typeId == 37) then
    return state
  let some slot := state.firstSpawnSlot | return state
  let resident : Resident := {
    kind := .oldMan, position := ⟨single (16 * dungeon.1 - 1), single (16 * dungeon.2 - 40)⟩
    homeTile := dungeon }
  return { state.setActor slot (.townResident resident) with oldManChanged := true }

private def Actor.contactSource : Actor → Option (Int × String)
  | .empty | .savedNpc _ | .townResident _ _ | .boundMechanic _ _ | .oldShakingChest _ | .forestBunny _ | .forestBird _ _ | .forestSnail _ => none
  | .greenSlime slime => some (slime.damage, match slime.netId with
      | 1 => "Blue Slime" | -7 => "Purple Slime" | -8 => "Red Slime" | -9 => "Yellow Slime" | _ => "Green Slime")
  | .angryBones enemy => enemy.contactSource
  | .darkCaster enemy => enemy.contactSource
  | .dungeonSlime slime => OriginalDungeonSlime.contactSource slime
  | .cursedSkull enemy => enemy.contactSource
  | .dungeonSpikeBall enemy => enemy.contactSource
  | .nightEnemy enemy => enemy.contactSource
  | .tombstoneGhost ghost => some (ghost.damage, "Ghost")
  | .skeletronPart part =>
    some (part.damage, if part.typeId == OriginalSkeletron.guardianType then "Dungeon Guardian" else "Skeletron")

structure ContactResult where
  body : Body
  health : Health
  random : UnifiedRandom
  events : Array Event := #[]
  killer : Option String := none
  dust : Array OriginalForestGuideArrow.DustParticle := #[]
  text : Option OriginalCombatTextChannel.Channel := none
  /-- The accepted `Player.Hurt` of this contact, if any. -/
  hurt : Option OriginalPlayerHurt.Record := none
  /-- The client's sound side after the contact; none when the caller gave none or nothing hit. -/
  sound : Option OriginalPlayerHurt.SoundClient := none
  /-- The hit direction of the accepted contact, for the death owner. -/
  hitDirection : Int := 0
  boundary : Option String := none

private def contactResult (body : Body) (health : Health) (random : UnifiedRandom) (damage : Int)
    (source : String) (dust : Array OriginalForestGuideArrow.DustParticle := #[]) : ContactResult :=
  if health.statLife == 0 then
    { body, health := { health with dead := true, respawnTimer := 600 }, random,
      events := #[.npcHit damage, .died .npc], killer := some source, dust }
  else { body, health, random, events := #[.npcHit damage], dust }

/-- Mark the active slots of the water dust cohort in the shared table view. -/
def _root_.Lean4Terria.OriginalForestGuideArrow.DustPool.occupyWater (pool : OriginalForestGuideArrow.DustPool)
    (water : Array OriginalWaterDust.Owned) : OriginalForestGuideArrow.DustPool :=
  let occupied := water.foldl (init := pool.occupied) fun slots o =>
    if !o.dust.active then slots else
    let slots := slots ++ Array.replicate (o.slot + 1 - slots.size) false
    slots.set! o.slot true
  { pool with occupied }

/-- Shared active dust slots, including the separately updated mana-heat particles. -/
def State.dustPool (state : State) (view : Option OriginalForestGuideArrow.Viewport) :
    OriginalForestGuideArrow.DustPool :=
  ((({ viewport := view, drawBudget := state.dustDrawBudget } : OriginalForestGuideArrow.DustPool).occupy state.guideDust).occupy
    (state.manaDust.map (·.allocation))).occupy
    (state.manaRefillDust.filterMap fun p => if p.active then some p.allocation else none) |>.occupyWater state.waterDust

def State.textChannel (state : State) (rect : OriginalForestChatLayout.Rect) :
    Option OriginalCombatTextChannel.Channel :=
  state.textFonts.map fun fonts =>
    {state := state.texts,context := state.textContext,rect,measure := fonts.measure}

private def State.npcText (state : State) (fromPlayer : Int := 0) : OriginalNpcCombatText.State :=
  {channel := state.textChannel ⟨0,0,0,0⟩,fromPlayer,netMode := state.textContext.netMode,
   boundary := state.textBoundary}

private def State.acceptNpcText (state : State) (text : OriginalNpcCombatText.State) : State :=
  {state with
    texts := (text.channel.map (·.state)).getD state.texts
    textBoundary := state.textBoundary.or text.boundary
    npcBoundary := state.npcBoundary.or (text.boundary.map fun why => (0,why))}

/-- The ordinary local-player General immunity branch of `Update_NPCCollision` and `Hurt`.
The actor's tile collision later in the player update resolves the new knockback velocity. -/
def State.contact (state : State) (body : Body) (health : Health) (random : UnifiedRandom)
    (defense : Int := 0) (view : Option OriginalForestGuideArrow.Viewport := none)
    (immuneToCursed : Bool := false) (client : OriginalPlayerHurt.Client := {}) (luck : Float32 := 0) :
    ContactResult := Id.run do
  if health.immune || health.dead then return { body, health, random }
  let bounds : OriginalStarterMelee.Rect :=
    ⟨body.position.x.toInt32.toInt, body.position.y.toInt32.toInt, body.width, body.height⟩
  let some (actor, npc, baseDamage, source) := state.actors.findSome? (fun actor => do
    let (damage, source) ← actor.contactSource
    let npc := actor.toNpc
    if npc.active && damage > 0 && bounds.intersects npc.rect then some (actor, npc, damage, source)
    else none)
    | return { body, health, random }
  let pool := state.dustPool view
  let text := state.textChannel ⟨bounds.x,bounds.y,bounds.w,bounds.h⟩
  -- `Update_NPCCollision` rolls `DamageVar(damage, -luck)`. Every module's contact is the ordinary
  -- `contactNpc` of its strike view and damage; the Cursed Skull adds its curse roll after it.
  if luck != 0 then
    if let .cursedSkull _ := actor then
      return {body, health, random, boundary := some s!"Cursed Skull contact with Player.luck {luck}: its curse wrapper is ported at luck 0"}
    let some hit := OriginalAngryBones.contactNpc npc baseDamage body health defense pool random text client luck
      | return {body, health, random}
    if hit.boundary.isSome then
      return {body := hit.body,health := hit.health,random := hit.effects.random,dust := hit.effects.dust
              text := hit.text,hurt := hit.hurt,sound := hit.sound,boundary := hit.boundary}
    return {contactResult hit.body hit.health hit.effects.random hit.damage source hit.effects.dust with
      text := hit.text,hurt := hit.hurt,sound := hit.sound,hitDirection := hit.hitDirection}
  let dungeonHit := match actor with
    | .angryBones enemy => OriginalAngryBones.contact enemy body health defense pool random text client
    | .darkCaster enemy => OriginalDarkCaster.contact enemy body health defense pool random text client
    | .dungeonSlime slime => OriginalDungeonSlime.contact slime body health defense pool random text client
    | .cursedSkull enemy => OriginalCursedSkull.contact enemy body health defense pool random immuneToCursed text client
    | .dungeonSpikeBall enemy => OriginalDungeonSpikeBall.contact enemy body health defense pool random text client
    | _ => OriginalAngryBones.contactNpc npc baseDamage body health defense pool random text client
  if let some hit := dungeonHit then
    -- A text boundary left body and health as they were; a sound or blood boundary inside
    -- Hurt keeps what was written before it (signed life, immunity, knockback, dust).
    if hit.boundary.isSome then
      return {body := hit.body,health := hit.health,random := hit.effects.random,dust := hit.effects.dust
              text := hit.text,hurt := hit.hurt,sound := hit.sound,boundary := hit.boundary}
    return {contactResult hit.body hit.health hit.effects.random hit.damage source hit.effects.dust with
      text := hit.text,hurt := hit.hurt,sound := hit.sound,hitDirection := hit.hitDirection}
  return {body,health,random}

private def onKill (state : State) (env : Loot.Context) : OriginalStarterMelee.OnKill :=
  fun index npc random =>
    match state.actors[index]! with
    | .greenSlime slime =>
      let loot := Loot.roll env { slime with position := npc.position } random
      (loot.drops, loot.random)
    | .angryBones enemy =>
      let loot := OriginalAngryBones.dropLoot env { enemy with body.position := npc.position } random
      (loot.drops, loot.random)
    | .darkCaster enemy =>
      let loot := OriginalDarkCaster.dropLoot env { enemy with body.position := npc.position } random
      (loot.drops, loot.random)
    | .dungeonSlime slime =>
      let loot := OriginalDungeonSlime.dropLoot env { slime with position := npc.position } random
      (loot.drops, loot.random)
    | .cursedSkull enemy =>
      let loot := OriginalCursedSkull.dropLoot env { enemy with body.position := npc.position } random
      (loot.drops, loot.random)
    | .nightEnemy enemy =>
      -- An unknown Eye condition is a boundary of the caller (`lootBoundary`); its loot is not used.
      let loot := OriginalNightEnemies.dropLoot env { enemy with body.position := npc.position } random
        (env.eyeDefeatedNoAltars.getD false)
      (loot.drops, loot.random)
    | .tombstoneGhost ghost =>
      let position := Loot.dropPosition npc.position npc.width npc.height
      let coins := Loot.dropMoneyAt env position 90 { random }
      let loot := Loot.dropHealsAt env position (ghost.lifeMax > 1 && ghost.damage > 0) coins
      (loot.drops, loot.random)
    | .skeletronPart part =>
      let position := Loot.dropPosition npc.position npc.width npc.height
      let loot := if part.typeId == OriginalSkeletron.guardianType then Loot.dungeonGuardian env position random
        else Loot.skeletron env position (part.typeId == headType) (npc.lifeMax > 1 && part.damage > 0) random
      (loot.drops, loot.random)
    | _ => (#[], random)

private def combatEffects (pool : OriginalForestGuideArrow.DustPool)
    (effects : Array OriginalForestOldManCurse.Effect) (random : UnifiedRandom) :
    Array OriginalForestGuideArrow.DustParticle × UnifiedRandom := Id.run do
  let mut pool := pool
  let mut random := random
  let mut dust := #[]
  for effect in effects do
    match effect with
    | .gore .. =>
      random := (random.next 10 31).2
      random := (random.next (-20) 21).2
    | .dust typeId position width height speed scale motion =>
      let (nextPool, particle, nextRng) := pool.create { position, width, height, typeId, speed, scale } random
      pool := nextPool
      random := nextRng
      if let some particle := particle then
        let particle := match motion with
          | .ordinary => particle
          | .skeletronMouth velocity => { particle with
              noGravity := true
              velocity := ⟨particle.velocity.x * 1.3 + velocity.x * 0.4, particle.velocity.y + (2 + velocity.y)⟩ }
          | .skeletronTrail => { particle with
              noGravity := true
              velocity := ⟨particle.velocity.x - speed.x, (particle.velocity.y - speed.y) + 5⟩ }
        dust := dust.push particle
  return (dust, random)

/-- Texture sizes of every Gore type a pool particle can have: the 151..153 path of
`OriginalGore` and the night enemies' 1..5. -/
def goreTextureSize (typeId : Nat) : Option (Nat × Nat) :=
  OriginalGore.textureSize typeId <|> OriginalNightEnemies.goreTextureSize typeId

private def toNightGore (p : OriginalGore.Particle) : OriginalNightEnemies.Gore :=
  { slot := 0, typeId := p.typeId, position := p.position, velocity := p.velocity
    rotation := p.rotation, scale := p.scale, alpha := p.alpha, timeLeft := p.timeLeft
    sticky := p.sticky, behindTiles := p.behindTiles, active := p.active }

/-- `Gore.Update` of one pool slot, by its type's released transcription. -/
def updateGoreParticle (grid : Grid) (p : OriginalGore.Particle) (loaded : Bool) :
    Except String OriginalGore.Particle := do
  if p.active && (OriginalNightEnemies.goreTextureSize p.typeId).isSome then
    let g ← OriginalNightEnemies.updateGore grid (toNightGore p) loaded
    return { p with position := g.position, velocity := g.velocity, rotation := g.rotation
                    alpha := g.alpha, timeLeft := g.timeLeft, active := g.active }
  OriginalGore.update grid p loaded

/-- The world Gore phase over all 600 slots (`OriginalGore.Pool.advance` for every type). -/
def advanceGorePool (pool : OriginalGore.Pool) (grid : Grid) (loadedTypes : Array Nat) :
    Except String OriginalGore.Pool := do
  return { particles := ← pool.particles.mapM fun p => updateGoreParticle grid p (loadedTypes.contains p.typeId) }

/-- `DrawGore` of one particle, by its type's released transcription. -/
def goreSprite (p : OriginalGore.Particle) (camera : OriginalCollision.Vector)
    (light : OriginalForestChatLayout.Color) : Except String (Option OriginalForestChatLayout.Sprite) :=
  if (OriginalNightEnemies.goreTextureSize p.typeId).isSome then
    OriginalNightEnemies.goreSprite (toNightGore p) camera light
  else OriginalGore.sprite p camera light

/-- Place the Gore of this phase's hit effects, in `NewGore` order, each in the first free slot
(the slot `NewGore` took). The effects were rolled against the pool as the phase began; if the real
pool has no free slot left for one, `NewGore` would have drawn nothing — a boundary. -/
def State.absorbGore (state : State) (gore : Array OriginalGore.Particle) (slot : Nat := 0) : State := Id.run do
  let mut particles := state.gorePool.particles
  for p in gore do
    let some free := particles.findIdx? (!·.active)
      | return { state with gorePool := { particles }
                            npcBoundary := state.npcBoundary.or (some (slot, "Gore pool full during a hit effect")) }
    particles := particles.set! free p
  return { state with gorePool := { particles } }

/-- What the ported NPC death path cannot do yet for this killer and NPC. -/
def lootBoundary (env : Loot.Context) (actor : Actor) : Option String :=
  -- The shared slime, coin and recovery rules take the luck (`Loot.rollLuck`); the NPC-specific rules
  -- of the other modules still roll their `RollLuck` at luck 0.
  let luckAware := match actor with
    | .greenSlime _ | .tombstoneGhost _ => true
    | _ => false
  if env.luck != 0 && !luckAware then
    some s!"loot of NPC type {actor.typeId} with Player.luck {env.luck}: its drop rules roll RollLuck at luck 0"
  else if actor.typeId == 2 && env.eyeDefeatedNoAltars.isNone then
    some "Demon Eye loot: EyeOfCthulhuDefeatedAndNoAltarsInWorld (Skyblock.noAltars) unknown"
  else none

/-- The Gore slots of the shared pool that are taken (`Main.gore[i].active`). -/
def State.goreOccupied (state : State) : Array Bool :=
  state.gorePool.particles.map (·.active)

private def nightGoreParticle (g : OriginalNightEnemies.Gore) : OriginalGore.Particle :=
  { typeId := g.typeId, active := g.active, position := g.position, velocity := g.velocity
    rotation := g.rotation, scale := g.scale, alpha := g.alpha, timeLeft := g.timeLeft
    sticky := g.sticky, behindTiles := g.behindTiles }

private def onStrike (state : State) (grid : Grid) (view : Option OriginalForestGuideArrow.Viewport) :
    OriginalStarterMelee.OnStrike :=
  fun priorDust index direction strike =>
    let plain := fun (out : Array OriginalForestGuideArrow.DustParticle × UnifiedRandom) =>
      ({ dust := out.1, random := out.2 } : OriginalStarterMelee.StrikeEffects)
    match state.actors[index]?.getD .empty with
    | .skeletronPart part =>
      let part := { part with life := strike.npc.life, velocity := strike.npc.velocity }
      plain <| combatEffects ((state.dustPool view).occupy priorDust)
        (OriginalSkeletron.hitEffects part direction strike.damage) strike.random
    | .angryBones _ =>
      let effects := OriginalAngryBones.hitEffects ((state.dustPool view).occupy priorDust) 5
        direction strike.damage strike.npc strike.random
      plain (effects.dust, effects.random)
    | .darkCaster enemy =>
      let effects := OriginalDarkCaster.hitEffects enemy ((state.dustPool view).occupy priorDust) 5
        direction strike.damage strike.npc strike.random
      plain (effects.dust, effects.random)
    | .dungeonSlime slime =>
      let effects := OriginalDungeonSlime.hitEffects slime ((state.dustPool view).occupy priorDust)
        direction strike.damage strike.npc strike.random
      plain (effects.dust, effects.random)
    | .cursedSkull _ =>
      let effects := OriginalCursedSkull.hitEffects ((state.dustPool view).occupy priorDust)
        strike.damage strike.npc strike.random
      plain (effects.dust, effects.random)
    | .greenSlime slime =>
      -- `NPC.HitEffect` of the ordinary slimes (as the Guide's arrow path has it): dust 4 in the slime's
      -- alpha and colour, `dealt / lifeMax * 100` of them (rounded up), or 50 on the kill.
      let npc := strike.npc
      let died := npc.life ≤ 0
      let count := if died then 50 else
        ((Float.ofInt strike.damage / Float.ofInt npc.lifeMax * 100).ceil.toInt64.toInt).toNat
      let request : OriginalForestGuideArrow.DustRequest := {
        position := npc.position, width := npc.width, height := npc.height, typeId := 4
        speed := if died then ⟨single (2 * direction), -2⟩ else ⟨single direction, -1⟩
        alpha := slime.alpha, color := slime.color.rgba }
      Id.run do
        let mut pool := (state.dustPool view).occupy priorDust
        let mut random := strike.random
        let mut dust := #[]
        for _ in [:count] do
          let (next, particle, nextRandom) := pool.create request random
          pool := next
          random := nextRandom
          dust := particle.elim dust dust.push
        return { dust, random }
    | .nightEnemy enemy =>
      -- `HitEffect` sees the struck NPC still active, with the strike's life and knockback.
      let struck := { enemy with body := { enemy.body with
        life := strike.npc.life, velocity := strike.npc.velocity, active := true } }
      -- Gore slots are taken against the pool as this phase began; `State.absorbGore` places the
      -- particles in order and stops where the pool's real occupancy would have differed.
      let effects := OriginalNightEnemies.hitEffects grid ((state.dustPool view).occupy priorDust)
        state.goreOccupied struck direction (Float.ofInt strike.damage) strike.random
      { dust := effects.dust, gore := effects.gore.map nightGoreParticle, random := effects.random }
    | _ => { random := strike.random }

private def dropsFromHits (hits : Array OriginalStarterMelee.MeleeHit) : Array Drop :=
  hits.flatMap (·.drops)

private def State.recordNpcStrike (state : State) (index : Nat) (killed credited : Bool) : State :=
  match state.actors[index]?.getD .empty with
  | .angryBones enemy =>
    if !enemy.body.active then state else
    let tally := killed && (credited || enemy.body.playerInteraction)
    { state with
      angryBonesBanner := if tally then state.angryBonesBanner.recordKill else state.angryBonesBanner
      angryBonesSounds := state.angryBonesSounds.push (if killed then .killed else .hit) }
  | .darkCaster enemy =>
    if !enemy.body.active then state else
    let tally := killed && (credited || enemy.body.playerInteraction)
    let state := { state with darkCasterBanner := enemy.recordBannerKill tally state.darkCasterBanner }
    if enemy.kind == .caster then
      { state with angryBonesSounds := state.angryBonesSounds.push (if killed then .killed else .hit) }
    else
      { state with waterSphereHit := true, waterSphereKilled := state.waterSphereKilled || killed }
  | .dungeonSlime slime =>
    if !slime.active then state else
    -- NPCLoot counts the kill only after GetWereThereAnyInteractions().
    let tally := killed && (credited || slime.playerInteraction)
    { state with
      dungeonSlimeBanner := OriginalDungeonSlime.recordBannerKill tally state.dungeonSlimeBanner
      dungeonSlimeSounds := state.dungeonSlimeSounds.push (if killed then .killed else .hit) }
  | .cursedSkull enemy =>
    if !enemy.body.active then state else
    let tally := killed && (credited || enemy.body.playerInteraction)
    { state with
      cursedSkullBanner := enemy.recordBannerKill tally state.cursedSkullBanner
      cursedSkullSounds := state.cursedSkullSounds.push (if killed then .killed else .hit) }
  | .nightEnemy enemy =>
    if !enemy.body.active || !killed || !(credited || enemy.body.playerInteraction) then state
    else if enemy.npcType == 3 then { state with zombieBanner := state.zombieBanner.recordKill }
    else { state with demonEyeBanner := state.demonEyeBanner.recordKill }
  | _ => state

/-- The sounds of a strike by the player on the NPC in `index` (`OriginalStarterMelee.OnSound`):
`HitSound` after the hit effect, `DeathSound` before the loot. Every NPC in the plain table has both,
so a `PlaySound(LegacySoundStyle, …)` call is counted whether or not a client hears it; a client's
audible one draws its pitch from the stream of the phase the strike is in. -/
private def onStrikeSound (state : State) (audio : Option OriginalTileHitEffects.Context) :
    OriginalStarterMelee.OnSound :=
  fun death index npc random =>
    let id := (state.actors[index]?.getD .empty).typeId
    match OriginalNpcSoundTable.styles id with
    | none => (none, false, random)
    | some _ =>
      match audio with
      | none => (none, true, random)
      | some context =>
        match OriginalNpcSound.strike death id npc.position context random with
        | .ok (played, random) => (played, true, random)
        | .error _ => (none, false, random)

private def State.withHits (state : State) (npcs : Array Npc)
    (hits : Array OriginalStarterMelee.MeleeHit) (body : Body) (client : Bool := false)
    (loot : Loot.Context := {}) : State :=
  let hits := hits.filter (·.strike.text.boundary.isNone)
  let died := hits.filter (·.strike.died)
  let unsupported := died.findSome? fun hit =>
    match state.actors[hit.index]?.getD .empty with
    | .forestBunny _ => some (hit.index, "bunny death and loot")
    | .forestBird _ _ => some (hit.index, "bird death and loot")
    | .forestSnail _ => some (hit.index, "snail death and loot")
    | actor => (lootBoundary loot actor).map (hit.index, ·)

  -- A client must not lose a strike sound silently: an NPC outside the plain sound table, or a
  -- style the resolver refuses, is a boundary.
  let unsupported := unsupported.or <| if !client then none else hits.findSome? fun hit =>
    let id := (state.actors[hit.index]?.getD .empty).typeId
    match OriginalNpcSoundTable.styles id with
    | none => some (hit.index, s!"strike sound of NPC type {id} is outside the plain legacy styles")
    | some (hitStyle, deathStyle) =>
      if hitStyle == 57 || (hit.strike.died && deathStyle == 10) then
        some (hit.index, s!"strike sound of NPC type {id} depends on the playing state of its instance")
      else none
  let state : State := { state with
    strikeSounds := state.strikeSounds ++ hits.flatMap (fun (hit : OriginalStarterMelee.MeleeHit) => hit.sounds)
    strikeStyleCalls := state.strikeStyleCalls +
      hits.foldl (fun count (hit : OriginalStarterMelee.MeleeHit) => count + hit.styleCalls) 0 }
  let state := hits.foldl (fun state hit => state.recordNpcStrike hit.index hit.strike.died true) state
  let target := localNpcTarget body false
  let state : State := { state with
    actors := state.actors.mapIdx fun index actor =>
      let hit := hits.any (·.index == index)
      if actor.darkCaster?.isSome && !hit then actor else actor.applyHit npcs[index]! target hit
    guideDust := state.guideDust ++ hits.flatMap (·.dust)
    noSpawnCycle := state.noSpawnCycle || !died.isEmpty
    skeletronDefeated := state.skeletronDefeated ||
      died.any (fun hit => state.actors[hit.index]?.any (fun actor => actor.typeId.toNat == headType))
    npcBoundary := state.npcBoundary.or unsupported }
  state.absorbGore (hits.flatMap (·.gore)) ((hits[0]?.map (·.index)).getD 0)

def State.useWeapon (state : State) (weapon : Weapon) (body : OriginalPlayerCollision.Body)
    (direction : Int) (held : Bool) (mouse : OriginalCollision.Vector) (grid : Grid)
    (random : UnifiedRandom) (loot : Loot.Context)
    (view : Option OriginalForestGuideArrow.Viewport := none) (noItems : Bool := false)
    (audio : Option OriginalTileHitEffects.Context := none) : State × Array Drop × UnifiedRandom :=
  let use := { state.use with direction }.beginFrame
  -- Cursed prevents a new start. An existing swing and its projectile phase continue.
  let itemInput := held && (!noItems || use.itemAnimation > 0)
  let result := OriginalStarterMelee.useFrame weapon { owner body with luck := loot.luck } use itemInput mouse
    (state.actors.map Actor.toNpc) (some grid) random (onKill state loot) (onStrike state grid view)
    (state.npcText) (onStrikeSound state audio)
  -- Failed starts still consume the physical release latch in native ItemCheck.
  let use : Use := { result.use with releaseUseItem := !held }
  let weapon := if use.itemAnimation > 0 then some weapon.type else none
  let state := ({ state.withHits result.npcs result.hits body audio.isSome loot with use, weapon }).acceptNpcText result.text
  let state := result.stab.elim state (fun stab => state.insertProjectile (.stab stab))
  (state, dropsFromHits result.hits, result.random)

private def nearbyAmount (nearby : Array (Nat × Float32)) : Float32 :=
  nearby.foldl (fun count (_, slots) => count + slots) 0

private def updateGhost (world : OriginalGreenSlime.World) (body : Body)
    (player : OriginalGreenSlime.Player) (ghost : Ghost) (random : OriginalGreenSlime.Rng) :
    Ghost × OriginalGreenSlime.Rng × Float32 × Bool :=
  if !ghost.active then (ghost, random, 0, false) else
  let (ghost, random) := if player.dead || ghost.targetOutOfRange body then
      let (roll, next) := if ghost.velocity.x == 0 then random.nextRange (-1) 2 else (0, random)
      (ghost.advanceFade world.grid body roll, next)
    else (ghost.advancePursuit world.grid body ghost.justHit, random)
  let stillHere := if ghost.active then
    some (OriginalGreenSlime.checkActive world #[player]
      { position := ghost.position, width := ghost.width, height := ghost.height
        lifeMax := ghost.lifeMax, timeLeft := ghost.timeLeft })
    else none
  let despawned := stillHere.any (fun (npc, _) => !npc.active)
  ({ ghost with
    active := ghost.active && !despawned
    timeLeft := (stillHere.map (·.1.timeLeft.toNat)).getD ghost.timeLeft
    immune := ghost.immune - 1, justHit := false }, random,
    (stillHere.map (nearbyAmount ∘ Prod.snd)).getD 0, despawned)

private def critterNeighbors (actors : Array Actor) : Array OriginalForestCritter.Other :=
  actors.zipIdx.filterMap fun (actor, index) =>
    let npc := actor.toNpc
    if !npc.active then none else
    some { index, type := actor.typeId.toNat, friendly := npc.friendly,
           damage := (actor.contactSource.map Prod.fst).getD 0,
           position := npc.position, width := npc.width, height := npc.height,
           noTileCollide := npc.noTileCollide }

private def birdHostileHit (actors : Array Actor) (bird : Bird) : Option (Int × Float32) :=
  let bounds : OriginalStarterMelee.Rect :=
    ⟨bird.position.x.toInt32.toInt, bird.position.y.toInt32.toInt, bird.width, bird.height⟩
  actors.findSome? fun actor => do
    let (damage, _) ← actor.contactSource
    let npc := actor.toNpc
    if npc.active && damage > 0 && bounds.intersects npc.rect then some (damage, npc.box.center.x)
    else none

private def State.curseEffects (state : State) (effects : Array OriginalForestOldManCurse.Effect)
    (view : Option OriginalForestGuideArrow.Viewport) (rng : OriginalGreenSlime.Rng) :
    State × OriginalGreenSlime.Rng :=
  if effects.isEmpty then (state, rng) else
  let (dust, random) := combatEffects (state.dustPool view) effects ⟨rng.table, rng.inext⟩
  let (table, cursor) := random.state
  ({ state with guideDust := state.guideDust ++ dust }, ⟨table, cursor⟩)

private def hostileNeighbors (actors : Array Actor) : Array OriginalDungeonMechanic.Other :=
  actors.mapIdx fun slot actor =>
    let n := actor.toNpc
    { slot, typeId := actor.typeId, active := n.active, friendly := n.friendly
      damage := (actor.contactSource.map (·.1)).getD 0
      position := n.position, width := n.width, height := n.height }

/-- `LegacySoundPlayer` for the NPCHit1 / NPCDeath1 of a struck friendly NPC, at the strike's own
call sites: the released night-enemy channel (`OriginalNightEnemies.playAudio`), whose audible
type 3 and 4 instances draw their pitch from the current stream. -/
def npcSoundHook (audio : OriginalNightEnemies.AudioContext) : OriginalTownCombat.SoundHook :=
  fun typeId style position random =>
    let (table, index) := random.state
    match OriginalNightEnemies.playAudio audio {} ⟨typeId, style, position⟩ ⟨table, index⟩ with
    | .ok out =>
      (⟨out.random.table, out.random.inext⟩,
        out.played.map fun p => {typeId, style, volume := p.volume, pan := p.pan, pitch := p.pitch})
    | .error _ => (random, none)

/-- The ambient groan of a fighter AI (`PlaySound(14, position)`, style 1) through the same released
channel: an audible one draws its variation (`Next(3)`) from the NPC stream; volume scale 0.4. -/
def npcAmbientHook (audio : OriginalNightEnemies.AudioContext) :
    OriginalCollision.Vector → OriginalGreenSlime.Rng →
      OriginalGreenSlime.Rng × Option OriginalTileHitEffects.PlayedSound :=
  fun position random =>
    match OriginalNightEnemies.playAudio audio {} ⟨14, 1, position⟩ random with
    | .ok out =>
      (out.random, out.played.map fun p => ({
        request := {typeId := 14, x := position.x.toInt32.toInt, y := position.y.toInt32.toInt, style := 1}
        -- the channel names the asset `Sounds/Zombie_N`
        variation := (p.asset.back.toNat - '0'.toNat), volume := p.volume, pan := p.pan, pitch := p.pitch } :
        OriginalTileHitEffects.PlayedSound))
    | .error _ => (random, none)

private def State.mechanicHit (state : State) (hit : Option OriginalDungeonMechanic.Hit)
    (client : Bool := false) : State :=
  match hit with
  | none => state
  | some hit => { state with
      guideDust := state.guideDust ++ hit.dust
      npcSounds := state.npcSounds ++ hit.sounds
      npcStyleCalls := state.npcStyleCalls + hit.styleCalls
      mechanicSounds := if client then state.mechanicSounds
        else (state.mechanicSounds.push .hit) ++ (if hit.killed then #[.killed] else #[])
      mechanicGores := state.mechanicGores ++ hit.gores
      npcDrops := state.npcDrops ++ hit.drops
      gorePool := hit.gorePool.getD state.gorePool }

/-- Update NPC slots in order. Projectiles created by their AI occupy a slot
immediately, so the upcoming projectile phase sees them on the same frame. -/
def State.advanceNpcs (state : State) (world : OriginalGreenSlime.World) (body : OriginalPlayerCollision.Body)
    (dead : Bool) (itemAnimation : Nat) (loot : Loot.Context)
    (townWorld : Option OriginalForestGuide.World := none)
    (critterWorld : Option OriginalForestCritter.World := none)
    (time : Float := 0) (view : Option OriginalForestGuideArrow.Viewport := none) (talkNPC : Int := -1)
    (audio : Option OriginalNightEnemies.AudioContext := none)
    (graveyard : Bool := false) (eclipse : Bool := false) : State := Id.run do
  let playSound := audio.map npcSoundHook
  let localPlayer := localNpcTarget body dead itemAnimation
  let mut world := world
  let critterPlayer : OriginalForestCritter.Player :=
    { index := 0, dead, position := body.position, width := body.width, height := body.height
      itemAnimation := itemAnimation }
  let mut random := state.npcRandom
  let mut actors := state.actors
  let mut nearbyActiveNPCs : Float32 := 0
  let mut noSpawnCycle := state.noSpawnCycle
  let mut npcBoundary := state.npcBoundary
  let mut combat := { state with
    skeletronRoared := false, oldManRetired := false, darkCasterSounds := #[], npcDrops := #[], npcTileEdits := #[] }
  let spawnsDuringUpdate := actors.any (fun actor => actor.active && (actor.typeId == headType || actor.typeId == 32))
  let maxSlot := if spawnsDuringUpdate then 200 else actors.size
  let target : OriginalForestOldManCurse.LocalTarget := { position := body.position, width := body.width, height := body.height }
  for index in [:maxSlot] do
    if combat.textBoundary.isSome then break
    let loot := { loot with grid := loot.grid.map fun _ => world.grid }
    match actors[index]?.getD .empty with
    | .greenSlime slime => if slime.active then
      let result := OriginalGreenSlime.update world #[localPlayer] random slime
      actors := actors.set! index (.greenSlime result.slime)
      random := result.rng
      nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount result.nearby
      noSpawnCycle := noSpawnCycle || !result.slime.active
      npcBoundary := npcBoundary.or (result.unsupported.map (index, ·))
    | .tombstoneGhost ghost =>
      let (ghost, next, nearby, despawned) := updateGhost world body localPlayer ghost random
      actors := actors.set! index (.tombstoneGhost ghost)
      random := next
      nearbyActiveNPCs := nearbyActiveNPCs + nearby
      noSpawnCycle := noSpawnCycle || despawned
    | .townResident resident bow =>
      if resident.active && resident.curseMarked then
        let slot := (actors[index]!).curseSlot (combat.protectedSlots[index]?.getD 0)
        if let some retired := OriginalForestOldManCurse.retireOldMan slot resident.velocity (fun _ rng => rng) random then
          actors := actors.set! index (.townResident { resident with active := false, life := retired.life } bow)
          let (next, nextRandom) := combat.curseEffects retired.effects view random
          combat := { next with oldManRetired := true, oldManChanged := true }
          random := nextRandom
          continue
      if let some env := townWorld then
        let otherResidents := actors.zipIdx.filterMap fun (actor, slot) =>
          if slot == index then none else actor.resident?
        let threats := (critterNeighbors actors).filter (·.index != index)
        let env := { env with otherResidents, threats, grid := world.grid }
        -- AI_007 IL_0eb6: after Skeletron the Old Man's ordinary update ends with ai[3] = 1,
        -- the same mark the Curse sets; his next update retires him through the branch above.
        let skeletronMark := env.downedSkeletron && resident.kind == .oldMan
        let env := if skeletronMark then { env with downedSkeletron := false } else env
        let effects : OriginalTownCombat.Effects := {
          dustPool := { combat.dustPool view with enabled := view.isSome }, loot
          text := combat.npcText 255, playSound
          goreContext := some (world.grid, combat.gorePool) }
        match OriginalForestGuide.step env ⟨random.table, random.inext⟩ resident bow effects with
        | .ok next =>
          combat := combat.acceptNpcText next.text
          let marked := if skeletronMark then { next.resident with curseMarked := true } else next.resident
          actors := actors.set! index (.townResident marked next.bow)
          combat := combat.mechanicHit (next.hit.filter (·.text.boundary.isNone)) audio.isSome
          noSpawnCycle := noSpawnCycle || next.noSpawnCycle
          if let some arrow := next.shot.bind OriginalForestGuideArrow.spawn then
            combat := combat.insertProjectile (.guideArrow arrow)
          let (table, cursor) := next.random.state
          random := ⟨table, cursor⟩
        | .error why =>
          npcBoundary := npcBoundary.or (some (index, reprStr why))
    | .boundMechanic mechanic friendlyRegen =>
      if !mechanic.body.active then continue
      if talkNPC == (index : Int) && !dead then
        let dust := { combat.dustPool view with enabled := view.isSome }
        let next := OriginalDungeonMechanic.rescueAmong world localPlayer index
          (hostileNeighbors actors) combat.mechanicNames random { bound := mechanic, friendlyRegen } dust loot false
          (some (world.grid, combat.gorePool)) (combat.npcText 255) playSound
        actors := actors.set! index (.townResident next.resident)
        random := next.rng
        combat := { (combat.acceptNpcText next.text).mechanicHit (next.hit.filter (·.text.boundary.isNone)) audio.isSome with mechanicRescued := true }
        noSpawnCycle := noSpawnCycle || !next.resident.active
        npcBoundary := npcBoundary.or (next.unsupported.map (index, ·))
      else
        let dust := { combat.dustPool view with enabled := view.isSome }
        let next := OriginalDungeonMechanic.advanceAmong world localPlayer index
          (hostileNeighbors actors) random { bound := mechanic, friendlyRegen } dust loot
          (some (world.grid, combat.gorePool)) (combat.npcText 255) playSound
        actors := actors.set! index (.boundMechanic next.bound.bound next.bound.friendlyRegen)
        random := next.rng
        combat := (combat.acceptNpcText next.text).mechanicHit (next.hit.filter (·.text.boundary.isNone)) audio.isSome
        nearbyActiveNPCs := nearbyActiveNPCs + next.nearby
        noSpawnCycle := noSpawnCycle || !next.bound.bound.body.active
        npcBoundary := npcBoundary.or (next.unsupported.map (index, ·))
    | .dungeonSpikeBall enemy =>
      if !enemy.body.active then continue
      match OriginalDungeonSpikeBall.update world #[localPlayer] random enemy with
      | .ok next =>
        actors := actors.set! index (.dungeonSpikeBall next.enemy)
        random := next.rng
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || !next.enemy.body.active
      | .error why => npcBoundary := npcBoundary.or (some (index, why))
    | .oldShakingChest enemy =>
      if !enemy.body.active then continue
      match OriginalOldSlime.update world #[localPlayer] random enemy (hostileNeighbors actors) with
      | .ok next =>
        actors := actors.set! index (.oldShakingChest next.enemy)
        random := next.rng
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || !next.enemy.body.active
      | .error why => npcBoundary := npcBoundary.or (some (index, why))
    | .angryBones enemy =>
      if !enemy.body.active then continue
      match OriginalAngryBones.update world #[localPlayer] random enemy (audio.map npcAmbientHook) with
      | .ok next =>
        actors := actors.set! index (.angryBones next.enemy)
        world := { world with grid := OriginalDoor.applyGrid world.grid next.tileEdits }
        combat := { combat with
          npcSounds := combat.npcSounds ++ (next.ambientPlayed.map fun p =>
            ({typeId := 14, style := 1, variation := p.variation, volume := p.volume, pan := p.pan, pitch := p.pitch} :
              OriginalTownCombat.PlayedSound)).toArray
          npcTileEdits := combat.npcTileEdits ++ next.tileEdits
          doorSounds := combat.doorSounds ++ next.doorSounds }
        random := next.rng
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || !next.enemy.body.active
      | .error why =>
        npcBoundary := npcBoundary.or (some (index, why))
    | .darkCaster enemy =>
      if !enemy.body.active then continue
      let arena : OriginalDarkCaster.Arena := {
        toWorld := world
        skeletronActive := actors.any (fun actor => actor.active && actor.typeId == headType)
        playerVelocity := fun _ => body.velocity }
      let dust := { combat.dustPool view with enabled := view.isSome }
      match OriginalDarkCaster.update arena #[localPlayer] ⟨random.table, random.inext⟩ enemy dust with
      | .ok next =>
        actors := actors.set! index (.darkCaster next.enemy)
        let (table, cursor) := next.random.state
        random := ⟨table, cursor⟩
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || !next.enemy.body.active
        combat := { combat with
          guideDust := combat.guideDust ++ next.dust
          darkCasterSounds := combat.darkCasterSounds ++ next.castSounds }
        if let some sphere := next.sphere.bind OriginalDarkCaster.SphereSpawn.enemy then
          combat := { combat with actors }
          if let some slot := combat.firstSpawnSlot then
            combat := combat.setActor slot (.darkCaster sphere)
            actors := combat.actors
      | .error why =>
        npcBoundary := npcBoundary.or (some (index, why))
    | .cursedSkull enemy =>
      if !enemy.body.active then continue
      let skullWorld := { world with frameHeight := 30, frameCount := 3 }
      match OriginalCursedSkull.update skullWorld #[localPlayer] ⟨random.table, random.inext⟩ enemy with
      | .ok next =>
        actors := actors.set! index (.cursedSkull next.enemy)
        let (table, cursor) := next.random.state
        random := ⟨table, cursor⟩
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || !next.enemy.body.active
      | .error why => npcBoundary := npcBoundary.or (some (index, why))
    | .dungeonSlime slime => if slime.active then
      -- `FindFrame` reads the 34-pixel frames of the desktop `NPC_71` texture.
      let slimeWorld := { world with frameHeight := OriginalDungeonSlime.desktopFrameHeight,
                                     frameCount := OriginalDungeonSlime.frameCount }
      let result := OriginalDungeonSlime.update slimeWorld #[localPlayer] random slime
      actors := actors.set! index (.dungeonSlime result.slime)
      random := result.rng
      nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount result.nearby
      noSpawnCycle := noSpawnCycle || !result.slime.active
      npcBoundary := npcBoundary.or (result.unsupported.map (index, ·))
    | .nightEnemy enemy =>
      if !enemy.body.active then continue
      let dust := { combat.dustPool view with enabled := view.isSome }
      -- Sounds resolve at their AI / door / water sites on this stream. `Splash_0`'s physical
      -- playing state is not tracked here: a Splash request on a client is a boundary.
      let result := match audio with
        | some context => OriginalNightEnemies.updateWithAudio world #[localPlayer] random enemy dust context {}
            graveyard eclipse
        | none => (OriginalNightEnemies.update world #[localPlayer] random enemy dust (graveyard := graveyard)
            (eclipse := eclipse)).map fun step => (step, {}, #[])
      match result with
      | .ok (next, _, played) =>
        actors := actors.set! index (.nightEnemy next.enemy)
        random := next.rng
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || !next.enemy.body.active
        combat := { combat with
          guideDust := combat.guideDust ++ next.dust
          npcSounds := combat.npcSounds ++ played.filterMap fun p =>
            let digit := (p.asset.back.toNat - '0'.toNat)
            -- Zombie_N / Dig_N carry their variation; NPC_Hit_1 / NPC_Killed_N are single assets.
            let variation := if p.request.typeId == 14 || p.request.typeId == 0 then digit else 0
            some ({ typeId := p.request.typeId.toNat, style := p.request.style.toNat, variation
                    volume := p.volume, pan := p.pan, pitch := p.pitch } : OriginalTownCombat.PlayedSound) }
        if audio.isSome && next.sounds.any (·.typeId == 19) then
          npcBoundary := npcBoundary.or (some (index, "night enemy Splash: Splash_0 playing state not tracked"))
      | .error why => npcBoundary := npcBoundary.or (some (index, why))
    | .forestBunny critter =>
      if let some env := critterWorld then
        let env := { env with grid := world.grid }
        let next := OriginalForestCritter.update env #[critterPlayer] (critterNeighbors actors) random critter
        actors := actors.set! index (.forestBunny next.critter)
        random := next.rng
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || next.noSpawnCycle
        npcBoundary := npcBoundary.or (next.unsupported.map (index, ·))
    | .forestBird bird immune =>
      let next := OriginalForestBird.update world #[localPlayer] random bird time
        (hostileHit := birdHostileHit actors) (loot := loot) (text := combat.npcText 255)
      combat := combat.acceptNpcText next.text
      actors := actors.set! index (.forestBird next.bird (immune - 1))
      random := next.rng
      nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
      noSpawnCycle := noSpawnCycle || next.noSpawnCycle
      npcBoundary := npcBoundary.or (next.unsupported.map (index, ·))
    | .forestSnail snail =>
      if let some env := critterWorld then
        let env := { env with grid := world.grid }
        let next := OriginalForestSnail.update env false #[critterPlayer] (critterNeighbors actors) random snail
        actors := actors.set! index (.forestSnail next.snail)
        random := next.rng
        nearbyActiveNPCs := nearbyActiveNPCs + nearbyAmount next.nearby
        noSpawnCycle := noSpawnCycle || next.noSpawnCycle
        npcBoundary := npcBoundary.or (next.unsupported.map (index, ·))
    | .skeletronPart part =>
      if !part.active then continue
      if part.typeId == OriginalSkeletron.guardianType then
        let (guardian, slots) := OriginalSkeletron.advanceGuardian part world localPlayer target
        actors := actors.set! index (.skeletronPart guardian.part)
        nearbyActiveNPCs := nearbyActiveNPCs + slots
        noSpawnCycle := noSpawnCycle || !guardian.part.active
        combat := { combat with skeletronRoared := combat.skeletronRoared || guardian.roar }
      else if part.typeId == headType then
        if part.ai0 == 0 then
          let start := OriginalForestOldManCurse.firstHeadTurn part (curseSlots actors combat.protectedSlots)
            target (fun _ rng => rng) random world.activeTime.toNat
          combat := { combat with actors := actors.set! index (.skeletronPart start.head) }
          for hand in start.hands do
            combat := combat.setActor hand.part.slot (.skeletronPart hand.part)
          actors := combat.actors
          let (next, nextRandom) := combat.curseEffects start.effects view random
          combat := next
          random := nextRandom
        else
          let head := OriginalSkeletron.advanceHead part world target dead
          actors := actors.set! index (.skeletronPart head.part)
          let (next, nextRandom) := combat.curseEffects head.effects view random
          combat := { next with skeletronRoared := next.skeletronRoared || head.roar }
          random := nextRandom
        if actors[index]!.active then nearbyActiveNPCs := nearbyActiveNPCs + 6
      else
        let head := (actors[part.ai1.toUInt32.toNat]?.bind Actor.skeletron?).filter
          (fun head => head.active && head.typeId == headType)
        let hand := OriginalSkeletron.advanceHand part head target
        if head.isNone then
          let (next, nextRandom) := combat.curseEffects (OriginalSkeletron.orphanHandEffects part) view random
          combat := next
          random := nextRandom
        actors := actors.set! index (.skeletronPart hand)
    | .empty | .savedNpc _ => pure ()
  return { combat with
    actors, npcRandom := random, nearbyActiveNPCs, noSpawnCycle
    npcBoundary := npcBoundary.or combat.npcBoundary }

private def Actor.arrowTarget (actor : Actor) : Option OriginalForestGuideArrow.Target := do
  guard actor.active
  if let some slime := actor.slime? <|> actor.dungeonSlime? then
    return { npc := actor.toNpc, slime := true, dustAlpha := slime.alpha, dustColor := slime.color.rgba }
  return { npc := actor.toNpc }

def State.advanceProjectile (state : State) (index : Nat) (grid : Grid) (body : Body)
    (dead : Bool) (loot : Loot.Context) (view : Option OriginalForestGuideArrow.Viewport := none)
    (audio : Option OriginalTileHitEffects.Context := none) :
    State × Array Drop := Id.run do
  if state.textBoundary.isSome then return (state,#[])
  match state.projectiles[index]?.getD .empty with
  | .stab stab =>
    if !stab.active then return (state, #[])
    if dead then
      return ({ state with projectiles := state.projectiles.set! index (.stab { stab with active := false, timeLeft := 0 }) }, #[])
    let (stab, npcs, hits, projectileRandom, _, text) := stab.updateFrameWithText { owner body with luck := loot.luck } state.use.direction 4
      (state.actors.map Actor.toNpc) (some grid) state.projectileRandom (onKill state loot) (onStrike state grid view)
      (state.npcText) (onStrikeSound state audio)
    let state := state.acceptNpcText text
    return ({ state.withHits npcs hits body audio.isSome loot with
      projectiles := state.projectiles.set! index (.stab stab)
      projectileRandom }, dropsFromHits hits)
  | .guideArrow arrow =>
    if !arrow.active then return (state, #[])
    let result := OriginalForestGuideArrow.step {
      grid, onKill := onKill state loot, onStrike := onStrike state grid view,text := state.npcText 255 }
      arrow (state.actors.map Actor.arrowTarget) state.projectileRandom
      { state.dustPool view with enabled := view.isSome }
    let state := { state.acceptNpcText result.text with
      projectiles := state.projectiles.set! index (.guideArrow result.arrow)
      guideDust := state.guideDust ++ result.dust
      projectileRandom := result.random }
    let some hit := result.hit | return (state, #[])
    let state := state.recordNpcStrike hit.slot hit.strike.died false
    -- The Guide's arrow is an NPC projectile: `Damage_PVE_Inner` IL_0cfe rolls DamageVar at luck 0,
    -- but the loot of its kill still asks the closest player's luck.
    let stop := if hit.strike.died then (lootBoundary loot (state.actors[hit.slot]?.getD .empty)).map (hit.slot, ·)
      else none
    let state := State.absorbGore { state with npcBoundary := state.npcBoundary.or stop } hit.gore hit.slot
    let actor := state.actors[hit.slot]!.applyHit hit.strike.npc (localNpcTarget body dead)
    return ({ state with
      actors := state.actors.set! hit.slot actor
      noSpawnCycle := state.noSpawnCycle || hit.strike.died
      skeletronDefeated := state.skeletronDefeated || (hit.strike.died && actor.typeId.toNat == headType) }, hit.drops)
  | .gravestone _ | .empty => return (state, #[])

/-- Headless combat-only caller. The archive session also updates terrain and
items between projectile slots when a gravestone lands. -/
def State.advance (state : State) (world : OriginalGreenSlime.World) (body : Body)
    (dead : Bool) (itemAnimation : Nat) (loot : Loot.Context)
    (townWorld : Option OriginalForestGuide.World := none)
    (critterWorld : Option OriginalForestCritter.World := none)
    (time : Float := 0) : State × Array Drop := Id.run do
  let mut combat := state.advanceNpcs world body dead itemAnimation loot townWorld critterWorld time
  let mut drops := combat.npcDrops
  let grid := OriginalDoor.applyGrid world.grid combat.npcTileEdits
  combat := { combat with npcDrops := #[] }
  combat := match advanceGorePool combat.gorePool grid combat.goreLoadedTypes with
    | .ok gorePool => { combat with gorePool }
    | .error why => { combat with npcBoundary := combat.npcBoundary.or (some (0, why)) }
  for index in [:combat.projectiles.size] do
    let (next, more) := combat.advanceProjectile index grid body dead { loot with grid := loot.grid.map fun _ => grid }
    combat := next
    drops := drops ++ more
  return (combat, drops)

end Lean4Terria.OriginalCombat
