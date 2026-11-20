import Lean4Terria.OriginalBankMining
import Lean4Terria.OriginalBloodDust
import Lean4Terria.OriginalChestMining
import Lean4Terria.OriginalChests
import Lean4Terria.OriginalCombat
import Lean4Terria.OriginalHousing
import Lean4Terria.OriginalHousingTables
import Lean4Terria.OriginalDarkCasterEffects
import Lean4Terria.OriginalCrafting
import Lean4Terria.OriginalForest
import Lean4Terria.OriginalHammerForeground
import Lean4Terria.OriginalHeldItemLight
import Lean4Terria.OriginalItemThrow
import Lean4Terria.OriginalItemUseSound
import Lean4Terria.OriginalPickupSound
import Lean4Terria.OriginalLiquid
import Lean4Terria.OriginalLiquidBuckets
import Lean4Terria.OriginalPlacement
import Lean4Terria.OriginalCandlePlacement
import Lean4Terria.OriginalCandleInteraction
import Lean4Terria.OriginalPlayerDeath
import Lean4Terria.OriginalPlayerEquipment
import Lean4Terria.OriginalPlayerFall
import Lean4Terria.OriginalPlayerFile
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalPlayerRandom
import Lean4Terria.OriginalManaHeat
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalRecipeCraft
-- BEGIN B-ZONE lighting-import (S01)
import Lean4Terria.OriginalLightingRuntime
import Lean4Terria.OriginalLightingRuntime.Statics
-- END B-ZONE lighting-import
-- BEGIN B-ZONE actions-import (S02)
import Lean4Terria.OriginalPlayerActions
-- END B-ZONE actions-import
import Lean4Terria.OriginalSelectedItemState
import Lean4Terria.OriginalSpawn
import Lean4Terria.OriginalStorage
import Lean4Terria.OriginalTileRules
import Lean4Terria.OriginalTree
import Lean4Terria.OriginalWallWork
import Lean4Terria.OriginalWorldItemLiquid
import Lean4Terria.OriginalLuck
import Lean4Terria.OriginalTorchLuck

namespace Lean4Terria.OriginalSession

open OriginalCollision (Grid single tileIndex)
open OriginalBanks (Stores)
open OriginalItemUse (Timing)
open OriginalItems (WorldItems)
open OriginalMining (Cell CopperPick HitCache)
open OriginalPlayerMovement (Controls)
open OriginalPlayerLiquidHealth (Health Event)
open OriginalWorld (Chest Sign Terrain)
open OriginalPlayerDeath (GravestonePlacement)

inductive OpenStorage where
  | chest (index : Nat)
  | bank (furniture : OriginalBanks.Placed)
  deriving BEq, Repr

private def OpenStorage.source : OpenStorage → OriginalCrafting.Source
  | .chest index => .chest index
  | .bank furniture => .bank furniture.kind

structure ThrowInput where
  controls : OriginalItemThrow.Controls
  /-- Hotbar selection before processing this frame's hotbar inputs. -/
  priorSelected : Nat

/-- `Main.Camera.ScaledPosition` / `ScaledSize` for `Main.GetAreaToLight` and `Main.BlackFadeDist`. -/
structure SceneCamera where
  position : Float32 × Float32
  size : Float32 × Float32
  /-- The world-load skyblock threshold switch of the scene scan. -/
  skyblockLowTiles : Bool := false

/-- What the desktop client's input phase leaves for one local `Player.Update`. -/
structure ClientInput where
  /-- Actual `SelectedItemState.Select` calls this update reaches before IL_1678, in call order:
  the hotbar click's `changeItem` (IL_1308), a held number key (IL_1535) and a nonzero hotbar
  scroll (IL_164f). A repeated request for the current slot is still an event. -/
  selects : Array Nat := #[]
  /-- Physical `TriggersSet.MouseLeft`, before any interface filter. -/
  mouseLeft : Bool := false
  /-- Physical right button, distinct from synthetic cursor-drop input. -/
  mouseRight : Bool := false
  /-- `Player.mouseInterface` as the preceding draw left it. -/
  mouseInterface : Bool := false
  tileUsageLocked : Bool := false
  buildingMode : Bool := false
  /-- ReferenceEquals(Main.InGameUI.CurrentState, Main.BestiaryUI).
  Historical dedicated fixtures can have both references null. -/
  bestiaryReferenceEqual : Bool := false
  currentNPCShowingChatBubble : Int := -1
  talkNPC : Int := -1
  npcChatRelease : Bool := true
  nonTileInteraction : Bool := false
  /-- Actual client sound gates/camera. Headless callers omit the client tile effects. -/
  tileHitContext : Option OriginalTileHitEffects.Context := none
  /-- The local wall clock as .NET `DateTime.Now` ticks, for `Player.lastDeathTime`. -/
  clockTicks : Option Int := none
  /-- The actual loaded original fonts; absent only for legacy/headless fixtures. -/
  combatTextFonts : Option OriginalCombatText.Fonts := none
  camera : Option SceneCamera := none
  /-- `Item.GetDrawHitbox` of a held light item: the size of its whole item texture. Only the
  window client has textures; it places `Player.itemLocation` for the held light's dust. -/
  heldItemFrame : Nat → OriginalHeldItemLight.ItemFrame := fun _ => ⟨0, 0⟩
  /-- `Main.gfxQuality` (rain allocation and splashes): the static initializer's 1 unless automatic
  quality lowered it. -/
  gfxQuality : Float32 := 1
  /-- `Main.FlashyEffectsWorld` (lightning), an option; the static initializer's value is true. -/
  flashyEffectsWorld : Bool := true
  -- BEGIN B-ZONE actions-client-keys (S02)
  /-- `Player.controlQuickHeal` / `controlQuickMana` (`PlayerInput.Triggers.Current.QuickHeal/QuickMana`,
  default keys H / B). A driver that does not declare them leaves both false. -/
  controlQuickHeal : Bool := false
  controlQuickMana : Bool := false
  -- END B-ZONE actions-client-keys

structure ContactDeathText where
  playerName : String := "Player"
  templates : Array String := #[]
  date : String := ""
  /-- Localized texts of the other death reasons by key (`DeathText.Lava_3` → "{0} tried to swim in lava."). -/
  named : Array (String × String) := #[]

structure State where
  terrain : Terrain
  player : OriginalPlayerMovement.State
  playerLiquid : OriginalPlayerWet.Liquid := {}
  health : Health := {}
  /-- What the player wears (armor, accessories, dyes, misc items, loadouts), from the character file. -/
  equipment : OriginalPlayerEquipment.Equipment := {}
  /-- `voiceVariant` / `voicePitchOffset` of the character file. -/
  voice : OriginalPlayerFile.Voice := {}
  /-- `UpdateEquips` and `UpdateArmorSets` of the latest living update; `none` before any. -/
  equipmentEffects : Option OriginalPlayerEquipment.Effects := none
  healthEvents : Array Event := #[]
  /-- The accepted `Player.Hurt` calls of the latest update, in order (contact, then lava): life at
  the sound, the canonical sound request and, when audible, the resolved instance to play. -/
  hurts : Array OriginalPlayerHurt.Record := #[]
  /-- `Player.PlayDeathSound` of this update's KillMe: the canonical request (also when unheard)
  and the resolved instance for the window. -/
  deathSoundRequest : Option OriginalTileHitEffects.SoundRequest := none
  deathSound : Option OriginalTileHitEffects.PlayedSound := none
  /-- KillMe's death message went through `Main.NewTextMultiline`, which plays MenuTick (type 12). -/
  deathMessageTick : Bool := false
  /-- Sounds resolved in this update's projectile phase (a gravestone's placement Dig), in order. -/
  projectileSoundRequests : Array OriginalTileHitEffects.SoundRequest := #[]
  projectileSounds : Array OriginalTileHitEffects.PlayedSound := #[]
  /-- `LegacySoundPlayer`'s hurt instance slots and `SoundStyle._random`. Client statics in the
  original: never reset per hit, update or world. The window declares the pitch stream. The
  stream is shared by every `PlaySound(LegacySoundStyle, …)` call of the client: each takes one
  sample. The session advances it for the default hurt voice and for the NPC-phase NPCHit1 /
  NPCDeath1; other such call sites (item use sounds, strikes by the player) do not advance it yet. -/
  hurtSound : OriginalPlayerHurtEffects.State := {}
  /-- `PlaySound(LegacySoundStyle, …)` calls of the item phase whose `SoundStyle._random` samples
  are still to be taken (their values are not used: these sounds have no pitch variance). -/
  pendingStyleSamples : Nat := 0
  corpse : Option OriginalPlayerDeath.CorpseVelocities := none
  /-- Head, body and legs of the dead player with the fade counter, advanced by every dead update
  (`Player.UpdateDead`). Not drawn by the window yet. -/
  corpseParts : Option OriginalPlayerDeath.CorpseParts := none
  /-- `Player.lastDeathPostion` (the player's Center at KillMe, IL_00ee..00f5) and `showLastDeath`
  (IL_0105), read by the map's death marker. `Player.Spawn` leaves both. -/
  lastDeathPosition : OriginalCollision.Vector := ⟨0, 0⟩
  showLastDeath : Bool := false
  /-- `Player.lastDeathTime` (`DateTime.Now` at KillMe, IL_00fb) as .NET ticks, when the window gave
  its clock (`ClientInput.clockTicks`); the session has no clock of its own. -/
  lastDeathTime : Option Int := none
  /-- `Player.luck` as this session's latest `UpdateLuck` left it (read by the natural spawner). -/
  luck : Float32 := 0
  /-- Achievements the player profile has completed (`AchievementManager`), by name; `none` when the caller
  does not know the profile. Progression events that complete one need it. -/
  achievements : Option (Array String) := none
  /-- Achievements completed in this update (`Main.NewText` + MenuTick through a `SoundStyle`). -/
  achievementsCompleted : Array String := #[]
  /-- `WorldGen`'s room-check statics, persisted between `QuickFindHome` calls (`none`: as after
  `WorldGen..cctor`, which the survey showed is also their state after load). -/
  housingRoom : Option OriginalHousing.RoomState := none
  /-- Torch luck: the counters and remembered centre of `TryRecalculatingTorchLuck`. -/
  torchLuck : OriginalTorchLuck.State := {}
  /-- `Player.usedGalaxyPearl`, from the character file. -/
  usedGalaxyPearl : Bool := false
  contactDeathText : ContactDeathText := {}
  /-- Accepted type-5 dust allocations in the local player phase. -/
  contactDeathDust : Nat := 0
  itemUse : Timing := {}
  combat : OriginalCombat.State := {}
  forest : Option OriginalForest.State := none
  -- BEGIN B-ZONE actions-hit-tile (S02)
  /-- `Player.hitTile`: the ONE native table that pickaxes, hammers and axes share (tile entries type 1,
  wall entries type 2). Owners still written against `OriginalMining.HitCache` (the copper pick with its
  chest/bank/gravestone routing, the copper axe, the block-swap drop) read and write its tile entries
  through `hitCacheOf` / `adoptHitCache`. -/
  hits : OriginalHammerForeground.HitTile := {}
  /-- `Player.hitReplace`: natively a SEPARATE `HitTile` (block replacement, `TryReplacingTiles`), not part
  of `hitTile`. Kept as the block-swap owner's `HitCache`; `OriginalMiningTools.State.hitReplace` takes it
  through the same adapter. -/
  swapHits : HitCache := {}
  -- END B-ZONE actions-hit-tile
  wallFrames : OriginalWallWork.WallFrames := {}
  poundRelease : Bool := false
  tileFrameNumbers : OriginalHammerForeground.TileFrameNumbers := {}
  wallUseHeld : Bool := false
  tileReplacement : Bool := true
  throwRelease : Bool := false
  throwNoThrow : Nat := 0
  candleInteraction : OriginalCandleInteraction.State := {}
  throwCursorSelected : Bool := false
  throwAnimation : Bool := false
  throwStarted : Bool := false
  throwDelayUse : Bool := false
  /-- `Player.selectedItemState`. Only the client entry `stepClient` drives it. -/
  selection : OriginalSelectedItemState.State := {}
  /-- The client entry owns `selection`. Headless drivers declare the slot on every step instead. -/
  selectionLive : Bool := false
  /-- `Main.blockMouse`: sticky until the physical left button is released. -/
  blockMouse : Bool := false
  lastItemUseAttemptSuccess : Bool := false
  /-- The latest client update reached `SelectedItemState.Select`'s `PlaySound(12)`. -/
  selectionMenuTick : Bool := false
  -- BEGIN B-ZONE lighting-state (S01)
  /-- `Lighting._activeEngine` (new lighting) with the `Main` statics it writes (`mapDelay`,
  `renderCount`, `updateMap`), `Lighting.GlobalBrightness` and the declared worker `Main.rand`.
  The engine's `_state` is the only lighting phase counter of the session. Created by `start` from
  the caller's `LightingSeeds`; there is deliberately no default. -/
  lighting : OriginalLightingRuntime.Runtime
  /-- `Main.tileColor` as the latest world draw's `ApplyColorOfTheSkiesToTiles` (DoDraw IL_0735) left
  it; `none` before the first world draw. The renderNow `LightTiles` passes (IL_039e/03a8) run
  before that draw's `SetBackColor` and read this value. -/
  tileColor : Option OriginalLightingRuntime.SColor := none
  /-- `Main.demonTorch` / `demonTorchDir` (`DoUpdate_AnimateTileGlows`, DoUpdate IL_0873). Starts at
  the `Main..cctor` state; BOUNDARY: the menu updates before world entry are not counted. -/
  tileGlows : OriginalLightingRuntime.Statics.TileGlows := {}
  /-- `Main.timeForVisualEffects` (DoUpdate IL_08cc). Starts at the `.cctor` 0; BOUNDARY: the menu
  updates before world entry are not counted. -/
  timeForVisualEffects : Float := 0
  /-- A `LightingEngine` fault (natively a worker exception that crashes the game). Fatal: once set,
  no further update or `LightTiles` call runs and the driver must stop with this text. -/
  lightingFault : Option String := none
  -- END B-ZONE lighting-state
  -- BEGIN B-ZONE actions-state (S02)
  /-- `Player.potionDelay` / `manaPotionDelay` (counted down at Player.Update IL_0a7a..0aa8; `potionDelay`
  mirrors a live Potion Sickness buff in UpdateBuffs). `Player..ctor` leaves both 0. -/
  potionDelay : Int := 0
  manaPotionDelay : Int := 0
  /-- `Player.releaseQuickHeal` / `releaseQuickMana`: `Player..ctor` does not write them (CLR false). -/
  releaseQuickHeal : Bool := false
  releaseQuickMana : Bool := false
  /-- This update's QuickHeal / QuickMana `PlaySound(item.UseSound, position)` requests and, for a window
  client with audio, their resolved instances (in call order). -/
  quickSoundRequests : Array OriginalTileHitEffects.SoundRequest := #[]
  quickSounds : Array OriginalTileHitEffects.PlayedSound := #[]
  -- END B-ZONE actions-state
  -- BEGIN B-ZONE projectiles-state (S03)
  /-- `Player.revolverCritChanceBonus` (ItemCheck IL_02f4 lowers it on a `Next(3) == 0` for every
  non-Revolver held item the bow path runs); `Player..ctor` leaves 0. -/
  revolverCritChanceBonus : Int := 0
  -- END B-ZONE projectiles-state
  /-- `Main.renderNow`. -/
  renderNow : Bool := false
  /-- `DoUpdateInWorld_Inner` calls since the world was entered (`GameUpdateCount - 65536`). -/
  updateCount : Nat := 0
  /-- `_playerSceneMetrics` of a session without a forest phase. -/
  plainSceneMetrics : Option OriginalSceneWaterStyle.Metrics := none
  /-- `Main.BlackFadeCameraTeleport` ran during the latest update. -/
  blackFade : Bool := false
  /-- `Player.itemLocation` as the latest held-light `ItemCheck` left it. -/
  itemLocation : OriginalCollision.Vector := ⟨0, 0⟩
  /-- `Player.spelunkerTimer` (Spelunker Glowstick). -/
  spelunkerTimer : Nat := 0
  inventory : OriginalItems.Inventory := OriginalItems.starterInventory
  coinAndAmmo : OriginalItems.Inventory := Array.replicate 8 none
  chests : Array Chest := #[]
  signs : Array Sign := #[]
  signsChanged : Bool := false
  banks : Stores := {}
  openStorage : Option OpenStorage := none
  mouseItem : Option OriginalItems.Stack := none
  inventoryRefreshTicks : Nat := 0
  worldItems : WorldItems := #[]
  itemRandom : UnifiedRandom := UnifiedRandom.seed 0
  /-- Previous UpdateBuffs heat plus the latest pickup's bonus/duration. -/
  manaHeat : OriginalManaHeat.State := {}
  /-- This update's full-mana sound request (vanilla event25, not a desktop sound slot). -/
  manaRefillSound : Bool := false
  manaRefillLights : Array OriginalManaRefillEffects.Light := #[]
  /-- Ordered AddLight requests from the Dust phase; propagation is separate. -/
  dustLights : Array OriginalManaRefillEffects.Light := #[]
  heldItemLights : Array OriginalHeldItemLight.Light := #[]
  /-- One item phase's ordered prefix effects; cleared at the next world update. -/
  tileHitEffects : Option OriginalTileHitRuntime.State := none
  /-- Slot-ordered pickup effects occur before contact/movement/ItemCheck. -/
  pickupSoundRequests : Array OriginalTileHitEffects.SoundRequest := #[]
  pickupSounds : Array OriginalTileHitEffects.PlayedSound := #[]
  /-- OwnerOnlyCode's place sound, resolved after the earlier held-light stage. -/
  candlePlacementSound : Option OriginalCandlePlacement.Sound := none
  craftRandom : UnifiedRandom := UnifiedRandom.seed 0
  unlockedBiomeTorches : Bool := false
  ateArtisanBread : Bool := false
  liquids : Option OriginalLiquid.Runtime := none
  bucketSplash : Option OriginalLiquidBuckets.Sound := none
  collected : Bool := false
  foregroundEdits : Array (Nat × Nat) := #[]

private def State.applyLiquidChanges (state : State) (runtime : OriginalLiquid.Runtime)
    (changes : Array OriginalLiquid.Change) : State := Id.run do
  let (drops, runtime) := runtime.takeDrops
  let (_, runtime) := runtime.takeSpawns
  let mut worldItems := state.worldItems
  for drop in drops do
    let position := ⟨single (drop.x * 16), single (drop.y * 16)⟩
    let velocity := ⟨single drop.drawX * 0.1, single drop.drawY * 0.1⟩
    worldItems := OriginalItems.spawnStackWithVelocity worldItems position velocity
      (.ofItem drop.item.toUInt16 drop.stack) drop.wet
  let edits := changes.filterMap fun change =>
    if change.before.id != change.after.id || change.before.frame != change.after.frame ||
        change.before.shape != change.after.shape || change.before.actuated != change.after.actuated then
      some (change.x, change.y)
    else none
  let tileFrameNumbers := changes.foldl (fun frames change =>
    if change.before.id != change.after.id then frames.erase (change.x, change.y) else frames)
    state.tileFrameNumbers
  return { state with
    terrain := runtime.terrain
    liquids := some runtime
    worldItems, tileFrameNumbers
    foregroundEdits := state.foregroundEdits ++ edits }

private def State.commitLiquids (state : State) (runtime : OriginalLiquid.Runtime) : State :=
  let (changes, runtime) := runtime.takeChanges
  state.applyLiquidChanges runtime changes

private def State.trackTerrain (state : State) : Terrain :=
  if state.liquids.isSome || !state.tileFrameNumbers.isEmpty then state.terrain.trackEdits else state.terrain

/-- `Main.expertMode` / `Main.masterMode` of the world, and the player's base maxima. -/
private def State.equipmentContext (state : State) (health : Health) : OriginalPlayerEquipment.Context :=
  let mode := state.terrain.header.gameMode
  { expertMode := mode == 1 || mode == 2, masterMode := mode == 2
    statLifeMax := health.statLifeMax, statManaMax := health.statManaMax }

/-- `statDefense` as the rest of `Player.Update` sees it (clamp at IL_3099..30a4). -/
def State.statDefense (state : State) : Int :=
  (state.equipmentEffects.map fun effects => max 0 effects.statDefense).getD 0

/-- `statManaMax2` (clamp at IL_3081..3094); the base maximum before the first update. -/
def State.statManaMax2 (state : State) : Int :=
  (state.equipmentEffects.map fun effects => min 400 effects.statManaMax2).getD state.health.statManaMax

/-- Wear the equipment and take the voice of a character file. No effect is computed here: in the
original every equipment field keeps its default until the first `UpdateEquips`, and the first
update's fall block (IL_188d) still reads those defaults — a Lucky Horseshoe does not protect in
update 1 (native: `hurt_fall_horseshoe`, 30 damage at update 1). -/
def State.withCharacter (state : State) (equipment : OriginalPlayerEquipment.Equipment)
    (voice : OriginalPlayerFile.Voice) : State :=
  { state with equipment, voice, equipmentEffects := none }

/-- Worn things the session does not act on, so that nothing is dropped silently: items and set
bonuses outside the translated list, and translated outputs that have no consumer here yet. -/
def State.equipmentBoundaries (state : State) : Array String := Id.run do
  -- Before the first update the list is what that update will compute.
  let e := state.equipmentEffects.getD
    (OriginalPlayerEquipment.update state.equipment (state.equipmentContext state.health))
  let mut out := e.unsupported.map fun entry =>
    match entry.slot with
    | some slot => s!"armor[{slot}] item {entry.item}: {entry.reason}"
    | none => entry.reason
  let flags : Array (Bool × String) := #[
    (e.fireWalk, "fireWalk"),
    (e.hasRaisableShield, "hasRaisableShield"), (e.hasJumpOption_Cloud, "hasJumpOption_Cloud"),
    (e.hasJumpOption_Blizzard, "hasJumpOption_Blizzard"), (e.hasJumpOption_Sandstorm, "hasJumpOption_Sandstorm"),
    (e.autoJump, "autoJump"), (e.accFlipper, "accFlipper"), (e.autoReuseGlove, "autoReuseGlove"),
    (e.accDivingHelm, "accDivingHelm"), (e.hasLuck_LuckyHorseshoe, "equipment luck"),
    (e.cactusThorns, "cactusThorns"), (e.ashWoodBonus, "ashWoodBonus"), (e.ammoCost80, "ammoCost80"),
    (e.spaceGun, "spaceGun"), (e.shadowArmor, "shadowArmor"), (e.crimsonRegen, "crimsonRegen"),
    (e.anglerSetSpawnReduction, "anglerSetSpawnReduction"),
    (e.spikedBoots != 0, "spikedBoots"), (e.rocketBoots != 0, "rocketBoots"),
    (e.meleeSpeed.toBits != (1 : Float32).toBits, "meleeSpeed"),
    -- BEGIN B-ZONE weapons-equipment-flags (S04): melee damage/crit reach the starter weapons and ranged damage the
    -- bow through weaponNumbers / BowUse; magic and minion damage have no consumer
    (e.magicDamage.toBits != (1 : Float32).toBits || e.minionDamage.toBits != (1 : Float32).toBits, "magic/minion damage multipliers"),
    (e.magicCrit != 4, "magic critical strike chance"),
    -- END B-ZONE weapons-equipment-flags
    (e.manaCost.toBits != (1 : Float32).toBits, "manaCost"), (e.pickSpeed.toBits != (1 : Float32).toBits, "pickSpeed")]
  for (active, name) in flags do
    if active then out := out.push s!"{name}: computed from the equipment, no consumer in the session yet"
  -- BEGIN B-ZONE weapons-ranged-crit (S04): consumed by the bow's arrows, but the equipment leaf's ranged crit from
  -- armor is an unverified coverage hole (Root: effects-minion-crit)
  if e.rangedCrit != 4 then
    out := out.push s!"rangedCrit {e.rangedCrit}: from the equipment, used by arrows, not covered natively (effects-minion-crit)"
  -- END B-ZONE weapons-ranged-crit
  return out

/-- The movement statistics equipment changes: `Player.Update` IL_331a..333b (`runAcceleration` and
`maxRunSpeed` times `moveSpeed`; `accRunSpeed` is whatever `UpdateEquips` left) and
`UpdateJumpHeight` IL_0000..00ee (`jumpBoost`, then `jumpSpeed += jumpSpeedBoost`). -/
private def equipMovement (e : OriginalPlayerEquipment.Effects) (move : OriginalPlayerMovement.State) :
    OriginalPlayerMovement.State :=
  let jumpHeight := if e.jumpBoost then max move.jumpHeight 20 else move.jumpHeight
  let jumpSpeed := if e.jumpBoost then max move.jumpSpeed 6.51 else move.jumpSpeed
  { move with
    runAcceleration := move.runAcceleration * e.moveSpeed
    maxRunSpeed := move.maxRunSpeed * e.moveSpeed
    accRunSpeed := e.accRunSpeed
    jumpHeight, jumpSpeed := jumpSpeed + e.jumpSpeedBoost }

private def State.adoptTerrain (state : State) (terrain : Terrain)
    (frameCells : Option (List (Nat × Nat)) := none) : State := Id.run do
  let (terrain, written) := terrain.finishEdits
  let tileFrameNumbers := written.foldl (fun frames (x, y) =>
    if (state.terrain.tile? x y).bind (·.id) != (terrain.tile? x y).bind (·.id) then
      frames.erase (x, y) else frames) state.tileFrameNumbers
  let state := { state with terrain, tileFrameNumbers }
  let some runtime := state.liquids | return state
  let cells := written.toList.eraseDups.filter fun (x, y) =>
    terrain.tile? x y != runtime.terrain.tile? x y
  if cells.isEmpty then return state
  let (_, runtime, itemRandom) := runtime.adoptEditWithRandom (fun _ => terrain) cells state.itemRandom frameCells
  let state := { state with
    itemRandom
    foregroundEdits := state.foregroundEdits ++ cells.toArray }
  return state.commitLiquids runtime

def State.wallHits (state : State) : OriginalWallWork.WallHits := state.hits.walls

-- BEGIN B-ZONE actions-hit-adapter (S02)
/-- The live tile entries (type 1, time to live > 0) of a `HitTile` as an `OriginalMining.HitCache`, most
recently sorted first (`order[1]` is the latest `SortSlots`). The prepared buffer slot is not live. -/
def hitCacheOf (h : OriginalHammerForeground.HitTile) : HitCache :=
  { entries := h.order.filterMap fun slot => do
      let e ← h.data[slot]?
      guard (e.type == 1 && e.timeToLive > 0 && e.x ≥ 0 && e.y ≥ 0)
      return ⟨⟨e.x.toNat, e.y.toNat⟩, e.damage.toNat, e.timeToLive.toNat⟩ }

/-- Write a legacy owner's resulting `HitCache` back into the tile entries of the native table: entries the
owner dropped are `Clear`ed, changed ones take its damage and time to live (a re-recorded entry, time to live
60, is `SortSlots`ed like `AddDamage`), new ones are placed with `HitObject` then `SortSlots`, oldest first.
Wall entries are untouched. BOUNDARY: the slot order these owners imply is approximated (their caches have
no slots); damage and time to live are theirs exactly. -/
def adoptHitCache (h : OriginalHammerForeground.HitTile) (cache : HitCache) : OriginalHammerForeground.HitTile := Id.run do
  let mut h := h
  for slot in [:h.data.size] do
    let e := h.data[slot]!
    if e.type != 1 || e.timeToLive ≤ 0 then continue
    match cache.entries.find? fun hit => (hit.cell.x : Int) == e.x && (hit.cell.y : Int) == e.y with
    | none => h := OriginalMiningTools.Hits.clear h slot
    | some hit =>
      if (hit.damage : Int) != e.damage || (hit.timeToLive : Int) != e.timeToLive then
        h := { h with data := h.data.set! slot { e with damage := hit.damage, timeToLive := hit.timeToLive } }
        if hit.timeToLive == 60 then h := OriginalMiningTools.Hits.sortSlots h slot
  for hit in cache.entries.reverse do
    let live := h.data.any fun e => e.type == 1 && e.timeToLive > 0 &&
      e.x == (hit.cell.x : Int) && e.y == (hit.cell.y : Int)
    if live then continue
    let (h', slot) := OriginalMiningTools.Hits.hitObject h hit.cell.x hit.cell.y 1
    let e := h'.data[slot]!
    h := OriginalMiningTools.Hits.sortSlots
      { h' with data := h'.data.set! slot { e with damage := hit.damage, timeToLive := hit.timeToLive } } slot
  return h

/-- `HitTile.Prune()` applied to the wall entries only (the legacy copper pick prunes its tile entries itself). -/
private def pruneWallHits (h : OriginalHammerForeground.HitTile) (terrain : Terrain) : OriginalHammerForeground.HitTile :=
  let pruned := h.prune terrain
  { h with data := h.data.mapIdx fun slot e => if e.type == 2 then pruned.data[slot]! else e }
-- END B-ZONE actions-hit-adapter

def State.carriedItems (state : State) : OriginalItems.Inventory := state.inventory ++ state.coinAndAmmo

/-- Take `count` samples from the client-wide `SoundStyle._random` (one per
`PlaySound(LegacySoundStyle, …)` call whose pitch variance is 0). -/
private def State.takeStyleSamples (state : State) (count : Nat) : State :=
  if count == 0 then state else
  { state with hurtSound := { state.hurtSound with
      pitchRandom := state.hurtSound.pitchRandom.map fun stream =>
        (List.range count).foldl (fun stream _ => stream.sample.2) stream } }

/-- What this update's `Player.Hurt` calls get from the client: `noKnockback` as `UpdateEquips`
just left it, and — only for a window client, which has sound gates — the player's saved voice
with the process-wide sound slots. Headless callers model no PlayHurtSound call. -/
private def State.hurtClient (state : State) (client : Option ClientInput) : OriginalPlayerHurt.Client :=
  let header := state.terrain.header
  { noKnockback := state.equipmentEffects.any (·.noKnockback)
    sound := (client.bind (·.tileHitContext)).map fun context =>
      { context, state := state.hurtSound
        voice := {
          position := state.player.body.position
          voiceVariant := state.voice.loadedVariant
          voicePitchOffset := state.voice.pitchOffset
          dontStarveWorld := header.dontStarveWorld, remixWorld := header.remixWorld } } }

private def State.playerTextRng (state : State) : OriginalPlayerLiquidHealth.Rng :=
  let body := state.player.body
  { stream := state.itemRandom
    text := state.combat.textChannel
      ⟨body.position.x.toInt32.toInt,body.position.y.toInt32.toInt,body.width,body.height⟩ }

private def State.acceptTextRng (state : State) (rng : OriginalPlayerLiquidHealth.Rng) : State :=
  { state with
    itemRandom := rng.stream
    combat := {state.combat with
      texts := (rng.text.map (·.state)).getD state.combat.texts
      textBoundary := state.combat.textBoundary.or rng.boundary
      npcBoundary := state.combat.npcBoundary.or (rng.boundary.map fun why => (0,why))} }

def State.setCarriedItems (state : State) (items : OriginalItems.Inventory) : State :=
  { state with inventory := items.extract 0 state.inventory.size
               coinAndAmmo := items.extract state.inventory.size items.size }

private def isSignTile (tile : OriginalWorld.Tile) : Bool :=
  tile.id.any (fun id => id == 55 || id == 85 || id == 425 || id == 573)

/-- `Sign.ReadSign` reduces either column to the sign's saved upper-left tile. -/
def State.signOrigin (state : State) (cell : Cell) : Option Cell := do
  let tile ← state.terrain.tile? cell.x cell.y
  guard (isSignTile tile)
  let (fx, fy) ← tile.frame
  let x := (cell.x : Int) - (fx.tdiv 18).tmod 2
  let y := (cell.y : Int) - fy.tdiv 18
  guard (x ≥ 0 && y ≥ 0)
  let origin : Cell := ⟨x.toNat, y.toNat⟩
  guard ((state.terrain.tile? origin.x origin.y).any isSignTile)
  return origin

def State.signAt (state : State) (origin : Cell) : Option Sign :=
  state.signs.find? fun sign => sign.x == Int.ofNat origin.x && sign.y == Int.ofNat origin.y

/-- Allocate a blank record on first interaction, or replace the text of an existing sign. -/
def State.writeSign (state : State) (origin : Cell) (text : String) : State :=
  if state.signOrigin origin != some origin then state else
  let record : Sign := ⟨text, origin.x, origin.y⟩
  let signs := match state.signs.findIdx? (fun sign => sign.x == record.x && sign.y == record.y) with
    | some slot => state.signs.set! slot record
    | none => if state.signs.size < 32000 then state.signs.push record else state.signs
  { state with signs, signsChanged := state.signsChanged || signs != state.signs }

def State.chestAt (state : State) (cell : Cell) : Option Nat :=
  OriginalChests.atTarget state.terrain state.chests state.player.body cell

def State.openChest (state : State) : Option Nat :=
  match state.openStorage with
  | some (.chest index) => some index
  | _ => none

/-- Commit a rename only while the named world container is still open. -/
def State.renameOpenChest (state : State) (index : Nat) (name : String) : State := Id.run do
  if state.openChest != some index then return state
  let some chest := state.chests[index]? | return state
  if chest.isVacant then return state
  return { state with chests := state.chests.set! index { chest with name } }

def State.storageAt (state : State) (cell : Cell) : Option OpenStorage :=
  (state.chestAt cell).map .chest |>.orElse fun _ =>
    (OriginalBanks.atTarget state.terrain state.player.body cell).map .bank

def State.toggleStorage (state : State) (target : OpenStorage) : State :=
  let opened := state.openStorage.map OpenStorage.source
  { state with openStorage := if opened == some target.source then none else some target }

/-- TileInteractionsUse tests slots 0–58 first, including the cursor,
then the active Void Bag. The Shadow Key is reusable. -/
def State.unlockChest (state : State) (cell : Cell) : Option State := do
  let index ← OriginalChests.atTarget state.terrain state.chests state.player.body cell
    (includeLocked := true)
  let chest ← state.chests[index]?
  let key ← OriginalChests.unlockKey state.terrain chest
  let carried := state.carriedItems
  let isKey (slot : Option OriginalItems.Stack) := slot.any (fun stack => stack.item == key && stack.count > 0)
  let consume (slot : Option OriginalItems.Stack) := slot.bind fun stack =>
    if stack.count > 1 then some { stack with count := stack.count - 1 } else none
  if let some slot := carried.findIdx? isKey then
    let terrain ← OriginalChests.unlock state.trackTerrain chest
    let state := if key == 329 then state
      else state.setCarriedItems (carried.set! slot (consume carried[slot]!))
    return ({ state with foregroundEdits := #[] } : State).adoptTerrain terrain
  if isKey state.mouseItem then
    let terrain ← OriginalChests.unlock state.trackTerrain chest
    let mouseItem := if key == 329 then state.mouseItem else consume state.mouseItem
    return ({ state with mouseItem, foregroundEdits := #[] } : State).adoptTerrain terrain
  guard (OriginalBanks.hasBag carried OriginalBanks.openVoidBag)
  let vault := state.banks.items .voidVault
  let slot ← vault.findIdx? isKey
  let terrain ← OriginalChests.unlock state.trackTerrain chest
  let banks := if key == 329 then state.banks
    else state.banks.replace .voidVault (vault.set! slot (consume vault[slot]!))
  return ({ state with banks, foregroundEdits := #[] } : State).adoptTerrain terrain

inductive StorageInteraction where
  | toggled
  | unlocked

def State.interactStorage (state : State) (cell : Cell) : Option (State × StorageInteraction) := do
  if let some target := state.storageAt cell then
    return (state.toggleStorage target, .toggled)
  return (← state.unlockChest cell, .unlocked)

def State.openStorageItems (state : State) : Option OriginalItems.Inventory := do
  match ← state.openStorage with
  | .bank furniture => return state.banks.items furniture.kind
  | .chest index =>
    let chest ← state.chests[index]?
    guard (chest.items.all (·.all (fun item => (OriginalChests.stack? item).isSome)))
    return chest.items.map (·.bind OriginalChests.stack?)

def State.replaceOpenStorage (state : State) (slots : OriginalItems.Inventory) : State := Id.run do
  match state.openStorage with
  | some (.bank furniture) =>
    return { state with banks := state.banks.replace furniture.kind slots }
  | some (.chest index) =>
    let some chest := state.chests[index]? | return state
    let chest := { chest with items := slots.map (·.map OriginalChests.ofStack) }
    return { state with chests := state.chests.set! index chest }
  | none => return state

def State.storageActionWithGlow (state : State) (action : OriginalStorage.Action)
    (multipliers : OriginalItemSorting.DamageMultipliers := {}) : State × Array (Nat × Float32) := Id.run do
  let some slots := state.openStorageItems | return (state, #[])
  if action == .sortItems then
    let sorted := OriginalItemSorting.sortChestWithGlow multipliers slots
    return (state.replaceOpenStorage sorted.slots, sorted.glow)
  let preserveFavorites := match state.openStorage with
    | some (.bank furniture) => furniture.kind.keepsFavorites
    | _ => false
  if action == .lootAll then
    let result := OriginalStorage.lootAll state.carriedItems slots state.banks.voidVault
      (state.openChest.isSome && state.banks.voidVaultEnabled) preserveFavorites
    let state := state.setCarriedItems result.inventory
    let state := { state with banks := state.banks.replace .voidVault result.vault }
    return (state.replaceOpenStorage result.container, #[])
  let (inventory, slots) :=
    OriginalStorage.apply action state.carriedItems slots state.openChest.isNone
  return ((state.setCarriedItems inventory).replaceOpenStorage slots, #[])

def State.storageAction (state : State) (action : OriginalStorage.Action) : State :=
  (state.storageActionWithGlow action).1

def State.shiftStorage (state : State) (slot : Nat) (intoStorage : Bool) : Option State := do
  let slots ← state.openStorageItems
  let carried := state.carriedItems
  let (inventory, updated) := if intoStorage then OriginalStorage.deposit carried slots slot
    else OriginalStorage.loot carried slots slot
  guard (updated != slots || inventory != carried)
  return (state.setCarriedItems inventory).replaceOpenStorage updated

def State.craftingChests (state : State) (nearbyEnabled : Bool) : Array Nat :=
  OriginalChests.craftingSources state.terrain state.player.body.position state.chests
    state.openChest nearbyEnabled

/-- The open container precedes the open Void Bag, nearby personal banks,
and nearby world chests. One personal inventory is counted only once. -/
def State.craftingStorage (state : State) (nearbyEnabled : Bool) : OriginalCrafting.Storage := Id.run do
  let inventory := state.carriedItems
  let opened := state.openStorage.map OpenStorage.source
  let mut order := opened.toArray
  let voidSource := OriginalCrafting.Source.bank .voidVault
  if opened != some voidSource && OriginalBanks.hasBag inventory OriginalBanks.openVoidBag then
    order := order.push voidSource
  if nearbyEnabled then
    for kind in OriginalBanks.nearby state.terrain state.player.body inventory do
      let source := OriginalCrafting.Source.bank kind
      if !order.contains source then order := order.push source
    for index in state.craftingChests true do
      if state.openChest != some index then order := order.push (.chest index)
  return { chests := state.chests, banks := state.banks, order }

def State.craftingEnvironment (state : State) : OriginalRecipeCatalog.Env :=
  let body := state.player.body
  let (zoneSnow, zoneGraveyard) := OriginalRecipeCatalog.biomeFlagsAt state.terrain body
  let header := state.terrain.header
  OriginalRecipeCatalog.catalog.envAt state.terrain body.position.x body.position.y body.width body.height
    { zoneSnow, zoneGraveyard, unlockedBiomeTorches := state.unlockedBiomeTorches,
      mechdusa := header.remixWorld && header.forTheWorthy }
    (extra := if state.ateArtisanBread then 4 else 0)

def State.availableRecipes (state : State) (nearbyEnabled : Bool) : Array Nat :=
  OriginalRecipeCatalog.catalog.query state.craftingEnvironment state.carriedItems
    (state.craftingStorage nearbyEnabled)

/-- The global generator restored between named update phases. -/
def State.outerRandom (state : State) : UnifiedRandom :=
  ((state.forest.bind (·.weather)).map (·.random)).getD state.craftRandom

def State.withOuterRandom (state : State) (random : UnifiedRandom) : State :=
  { state with craftRandom := random, forest := state.forest.map fun forest =>
      { forest with weather := forest.weather.map fun weather => { weather with random } } }

def State.craftRecipe (state : State) (nearbyEnabled : Bool) (index requested : Nat) : Option State := do
  let (result, craftRandom) := OriginalRecipeCatalog.catalog.craft state.craftingEnvironment
    state.carriedItems state.mouseItem (state.craftingStorage nearbyEnabled) index requested state.outerRandom
  guard result.crafted
  return { (state.setCarriedItems result.inventory).withOuterRandom craftRandom with
    mouseItem := result.mouseItem, chests := result.chests, banks := result.banks }

def State.copperPick (state : State) : CopperPick :=
  ⟨state.itemUse, hitCacheOf state.hits⟩

/-- `inventory[index]` of the original 59-slot array. Slot 58 is the cursor mirror that
`dropItemCheck` refreshes from `Main.mouseItem`; this session keeps the one cursor stack. -/
def State.inventorySlot (state : State) (index : Nat) : Option OriginalItems.Stack :=
  if index == 58 then state.mouseItem else state.carriedItems[index]?.join

/-- The slot a headless driver's declared hotbar choice stands for. A client session passes the
real `selectedItem` and reads that slot alone. -/
private def State.heldIndex (state : State) (selected : Nat) : Nat :=
  if state.selectionLive then selected else if state.mouseItem.isSome then 58 else selected

/-- `Player.HeldItem`. For a client session `selected` is the effective `selectedItem`; a cursor
item only counts once `SelectedItemState.Update` has moved the selection to slot 58. -/
def State.equipped (state : State) (selected : Nat) : Option OriginalItems.Stack :=
  if state.selectionLive then state.inventorySlot selected
  else state.mouseItem.or (state.inventory[selected]?.join)

/-- The accessories whose single-wear native survey changes a luck field
(`.lake/forest-desktop-integration/luck-runtime/luck-items-from-equipment-survey.json`, from
`.lake/original-player-equipment/survey-*/native.json`, all 1,162 equippable items). -/
def luckItems : Array UInt16 := #[158, 396, 855, 1250, 1251, 1252, 3034, 3035, 3250, 3251, 3252, 5331]

/-- Luck contributions the session has no producer for, so they keep the value a freshly loaded
player has (none of them is in the character file): ladybug luck (catching or killing a ladybug),
coin luck (coins in shimmer), broken-mirror bad luck, kite luck (flying a kite). The lantern-night
state is the time owner's (package 3) and is not tracked yet. Shown with the equipment boundaries. -/
def luckBoundaries : Array String := #[
  "luck: ladybugLuck, coinLuck, brokenMirrorBadLuck, kiteLuckLevel have no producer in the session (0 as loaded)",
  "luck: lanternsUp is not tracked (LanternNight state belongs to the time / weather owner)"]

/-- `Player.UpdateLuck` (`Player.Update` IL_24cb): torch luck for the local player, then
`RecalculateLuck` from what this update's state shows. Equipment luck is the translated
`equipmentBasedLuckBonus`; a worn luck accessory the equipment model does not translate, or a
luck potion buff, is a boundary, not a silent zero. -/
private def State.updateLuck (state : State) (selected : Nat) : State × Option String := Id.run do
  let some forest := state.forest | return (state, none)
  let some effects := state.equipmentEffects | return (state, none)
  if let some entry := effects.unsupported.find? (fun entry => luckItems.contains entry.item) then
    return (state, some s!"luck: worn item {entry.item} changes luck and is not translated")
  let buffs := state.health.buffType.zip state.health.buffTime
  if buffs.any (fun (id, time) => id == 257 && time > 0) then
    return (state, some "luck: Luck Potion buff (257) not ported")
  let zone := fun name => (forest.playerFlags.lookup name).getD false
  let body := state.player.body
  let center : OriginalCollision.Vector :=
    ⟨body.position.x + single (body.width / 2), body.position.y + single (body.height / 2)⟩
  let header := state.terrain.header
  let world := OriginalSceneWaterStyle.World.ofTerrain state.terrain header.infectedSeed
  let torchStyle := (state.equipped selected).bind fun stack =>
    if OriginalHeldItemLight.torches.contains stack.item.toNat then
      some (OriginalHeldItemLight.torchStyle stack.item.toNat) else none
  let (torch, boundary) := OriginalTorchLuck.step state.torchLuck {
    center, position := body.position, dead := state.health.dead, selectedTorchStyle := torchStyle
    zones := {
      dungeon := zone "ZoneDungeon", lihzhardTemple := zone "ZoneLihzhardTemple", snow := zone "ZoneSnow"
      desert := zone "ZoneDesert", jungle := zone "ZoneJungle", glowshroom := zone "ZoneGlowshroom"
      corrupt := zone "ZoneCorrupt", crimson := zone "ZoneCrimson", hallow := zone "ZoneHallow" }
    oceanDepths := OriginalSceneWaterStyle.oceanDepths world (center.x.toInt32.toInt.tdiv 16) (center.y.toInt32.toInt.tdiv 16)
    unlockedBiomeTorches := state.unlockedBiomeTorches
    hasTorchGodsFavor := (state.carriedItems.push state.mouseItem).any (·.any (·.item == 5043))
    maxTilesX := header.width, maxTilesY := header.height, worldSurface := header.surface
    tileAt := fun x y => if x < 0 || y < 0 then none else state.terrain.tile? x.toNat y.toNat }
  if (boundary : Option String).isSome then return ({ state with torchLuck := torch }, boundary)
  let luck := OriginalLuck.recalculate {
    torchLuck := torch.torchLuck
    usedGalaxyPearl := state.usedGalaxyPearl
    hasGardenGnomeNearby := (forest.sceneMetrics.map fun (m : OriginalSceneWaterStyle.Metrics) => m.hasGardenGnome).getD false
    stinky := buffs.any fun (id, time) => id == 120 && time > 0
    equipmentBasedLuckBonus := effects.equipmentBasedLuckBonus }
  return ({ state with torchLuck := torch, luck }, none)

private def State.playerDropPosition (state : State) : OriginalCollision.Vector :=
  let body := state.player.body
  let center := body.position + ⟨single body.width / 2, single body.height / 2⟩
  ⟨center.x - 8, center.y - 8⟩

private def State.spawnItemAtPlayer (state : State) (stack : OriginalItems.Stack) : State :=
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  let (worldItems, itemRandom) := OriginalItems.spawnStackAt state.worldItems
    state.playerDropPosition stack state.itemRandom (some grid)
  { state with worldItems, itemRandom }

/-- Return an item held by a closing interface, throwing any remainder at the player. -/
def State.returnInterfaceItem (state : State) (stack : OriginalItems.Stack) : State :=
  let received := OriginalItems.receive state.carriedItems state.banks.voidVault
    state.banks.voidVaultEnabled stack
  let state := { state.setCarriedItems received.carried with
    banks := state.banks.replace .voidVault received.vault }
  received.remainder.map (state.spawnItemAtPlayer ·) |>.getD state

/-- Native SortCoins returns leftover coins through GetOrDropItem before SortAmmo. -/
def State.sortCarried (state : State) (multipliers : OriginalItemSorting.DamageMultipliers := {})
    (locked : Bool := false) : State × Array (Nat × Float32) := Id.run do
  if locked then return (state, #[])
  let (inventory, coins) := OriginalItemSorting.prepareInventoryCoins state.carriedItems
  let state := coins.foldl State.returnInterfaceItem (state.setCarriedItems inventory)
  let sorted := OriginalItemSorting.sortInventorySlots multipliers state.carriedItems
  return (state.setCarriedItems sorted.slots, sorted.glow)

private def worldSpawn (terrain : Terrain) : OriginalSpawn.Spawn :=
  let header := terrain.header
  let grid := Grid.ofTerrain terrain OriginalTileRules.collision
  OriginalSpawn.worldSpawn grid OriginalTileRules.canHurtAtSpawn header.spawnX header.spawnY

private def spawnBody (spawn : OriginalSpawn.Spawn) : OriginalPlayerCollision.Body :=
  let position := spawn.position 20 42
  { position, velocity := ⟨0, 0⟩, fallStart := tileIndex position.y }

-- BEGIN B-ZONE lighting-seeds (S01)
/-- The non-reproducible inputs of `new LightingEngine()` (`Lighting.Initialize`).

Seed policy: natively the three `Guid.NewGuid().GetHashCode()` values (scanner `_random`, active
map, working map `FastRandom.CreateWithRandomSeed`) and the worker threads' `[ThreadStatic]`
`Main.rand` (seeded from `Environment.TickCount` on first use, scheduling-dependent) are fresh per
process and not reproducible. A caller with entropy passes its own (the desktop: three
`IO.getRandomBytes` hashes like its wind hash, and the monotonic millisecond clock for the worker
stream). Headless and replay sessions pass `LightingSeeds.declared`. The replays compare only the
lighting phase counters (`_state`, `Main.renderCount`, `Main.mapDelay`, `Main.renderNow`), never
light values, so the declared seeds do not affect any replay result. -/
structure LightingSeeds where
  scannerHash : Int32
  activeHash : Int32
  workingHash : Int32
  /-- Seed of the declared worker-thread `Main.rand` stream (`UnifiedRandom.seed`). -/
  scanRandomSeed : Int

/-- The declared seeds of a headless / replay session (all zero; see `LightingSeeds`). -/
def LightingSeeds.declared : LightingSeeds :=
  { scannerHash := 0, activeHash := 0, workingHash := 0, scanRandomSeed := 0 }

/-- `new LightingEngine()` with `Main.mapDelay`/`renderCount` at their static zero and no
`Main.updateMap`; world entry (`enterClient`) then writes `mapDelay := 2`. -/
def LightingSeeds.create (seeds : LightingSeeds) : OriginalLightingRuntime.Runtime :=
  OriginalLightingRuntime.create seeds.scannerHash seeds.activeHash seeds.workingHash
    { mapDelay := 0, renderCount := 0, updateMap := none } (UnifiedRandom.seed seeds.scanRandomSeed)
-- END B-ZONE lighting-seeds

/-- Start a regular-world character at the world's shared spawn. The caller
can apply spawn clearance before retrying when the spawn is blocked. -/
def start (terrain : Terrain) (lightingSeeds : LightingSeeds) : Except String State := do
  let spawn := worldSpawn terrain
  if spawn.requiresClearance then throw "World spawn requires clearance."
  let seed := OriginalWorld.numericSeed terrain.header.seed
  let itemRandom := UnifiedRandom.seed seed
  return { terrain, itemRandom, player := { body := spawnBody spawn }, health := ({} : Health).spawn true
           combat := { npcRandom := OriginalGreenSlime.Rng.seed seed, projectileRandom := itemRandom }
           lighting := lightingSeeds.create }

/-- The desktop world entry settles liquids before finding the spawn and
starts the per-world `UpdateLiquid` stream. -/
def startWithLiquids (terrain : Terrain) (loaderRng : UnifiedRandom) (lightingSeeds : LightingSeeds) :
    Except String State := do
  let seed := OriginalWorld.numericSeed terrain.header.seed
  let (runtime, report) := OriginalLiquid.Runtime.load terrain seed loaderRng
    (hooks := OriginalLiquid.objectHooks)
  let (changes, runtime) := runtime.takeChanges
  let state ← start runtime.terrain lightingSeeds
  return { state.applyLiquidChanges runtime changes with craftRandom := report.loaderRandom }

/-- Load the resident slots and the ordinary forest's independent phase streams.
The desktop caller supplies the independent wind hash to enable the outer weather update. -/
def startArchive (archive : OriginalWorld.Archive) (loaderRng : UnifiedRandom) (lightingSeeds : LightingSeeds)
    (windGuidHash : Option Int32 := none) : Except String State := do
  let header := archive.terrain.header
  let (background, loaderRng) := OriginalForestClock.rollCloudBackground header.cloudBackgroundActive loaderRng
  let state ← startWithLiquids archive.terrain loaderRng lightingSeeds
  let combat := state.combat.loadNpcs (← archive.readNpcs)
  let banner (id : Nat) : OriginalAngryBones.BannerProgress :=
    { kills := header.bannerKills.getD id 0, claimable := header.claimableBanners.getD id 0 }
  let combat := { combat with
    angryBonesBanner := banner OriginalAngryBones.bannerId
    darkCasterBanner := banner OriginalDarkCaster.bannerId
    dungeonSlimeBanner := banner OriginalDungeonSlime.bannerId
    cursedSkullBanner := banner OriginalCursedSkull.bannerId
    zombieBanner := banner 3
    demonEyeBanner := banner 2 }
  let (weather, craftRandom) := OriginalForestClock.finishOrdinaryLoad state.craftRandom header.tenthAnniversary
  let forest := OriginalForest.start state.terrain combat state.player.body weather.critters
  let weatherState : Option OriginalForest.WeatherState := windGuidHash.map fun hash =>
    let saved : OriginalForestClock.SavedWeather := ⟨header.cloudBackgroundActive, header.cloudCount, header.windSpeedTarget⟩
    let dry := OriginalForestClock.dryWeatherFromLoad saved background weather hash
    let ambience := OriginalForestAmbience.desktopStart craftRandom header.tenthAnniversary
    { dry, ambience := ambience.ambience, random := ambience.random }
  let forest := { forest with weather := weatherState, loadedCloudBackground := background.active }
  -- The full UpdateTime caller starts from the saved weather (`WorldFile.SetOngoingToTemps`). Its clock,
  -- stream, critter table and starfall are the forest's as loaded above. 697 = `NPCID.Count`
  -- (`.lake/original-player-death-client/npc-sets.json`).
  let timeCaller := (OriginalForestClock.readSavedWorldWeather archive.bytes header).toOption.map fun saved =>
    OriginalTimeCaller.initial header saved forest.clock forest.timeRandom forest.critters forest.starfallBoost
      forest.fairyLog 697
  let forest := { forest with timeCaller }
  return { state with combat, forest := some forest, craftRandom }

def State.dayTime (state : State) : Bool :=
  (state.forest.map (·.clock.dayTime)).getD state.terrain.header.dayTime

def State.downedSkeletron (state : State) : Bool :=
  state.terrain.header.downedSkeletron || state.combat.skeletronDefeated

private def residentRecord (n : OriginalForestGuide.Resident) : OriginalWorld.SavedTownNpc :=
  { typeId := n.kind.typeId, name := n.name, position := (n.position.x, n.position.y)
    homeless := n.homeless, homeTile := n.homeTile, variation := n.variation
    homelessDespawn := n.homelessDespawn }

def State.saveProgress (state : State) : OriginalWorld.SaveProgress :=
  let clock := state.forest.map fun forest =>
    (forest.clock.time, forest.clock.dayTime, forest.clock.moonPhase)
  let weather := (state.forest.bind (·.weather)).map fun runtime =>
    (runtime.dry.cloudBackground.active.toInt32.toInt, runtime.dry.clouds, runtime.dry.windTarget)
  let residents := state.combat.actors.zipIdx.filterMap fun (actor, slot) => do
    let n ← actor.resident?
    guard (!(state.combat.oldManChanged && n.kind == .oldMan))
    guard (!(state.combat.mechanicRescued && n.kind == .mechanic))
    return (slot, residentRecord n)
  let newResidents := if state.combat.mechanicRescued then
      state.combat.residents.filterMap fun n => if n.active && n.kind == .mechanic then some (residentRecord n) else none
    else #[]
  let oldMan := if state.combat.oldManChanged then
      some ((state.combat.residents.find? (fun n => n.active && n.kind == .oldMan)).map residentRecord)
    else none
  let header := state.terrain.header
  let banners := #[(OriginalAngryBones.bannerId, state.combat.angryBonesBanner),
      (OriginalDarkCaster.bannerId, state.combat.darkCasterBanner),
      (OriginalDungeonSlime.bannerId, state.combat.dungeonSlimeBanner),
      (OriginalCursedSkull.bannerId, state.combat.cursedSkullBanner),
      (3, state.combat.zombieBanner), (2, state.combat.demonEyeBanner)].filterMap fun (id, banner) =>
    if banner.kills != header.bannerKills.getD id 0 || banner.claimable != header.claimableBanners.getD id 0 then
      some (id, banner.kills, banner.claimable)
    else none
  { clock, weather, anglerQuest := state.forest.bind (·.anglerQuest), residents, newResidents, oldMan
    downedSkeletron := state.combat.skeletronDefeated, savedMechanic := state.combat.mechanicRescued, banners }

private def State.beginMove (state : State) (input : Controls) : State :=
  let header := state.terrain.header
  let player : OriginalPlayerWet.Player := ⟨state.player, state.playerLiquid⟩
  let prepared := player.prepare header.width header.surface
  let move := (state.equipmentEffects.map (equipMovement · prepared.move)).getD prepared.move
  let player := move.beginOnFoot input (state.combat.use.itemAnimation == 0)
  let use := state.combat.use.changeDir (OriginalCombat.owner state.player.body) player.direction
  { state with player, combat := { state.combat with use } }

private def combatWorld (state : State) (grid : Grid) : OriginalGreenSlime.World :=
  { grid, dayTime := state.dayTime, worldSurface := state.terrain.header.surface
    rockLayer := state.terrain.header.rockLayer, frameHeight := 26 }

/-- Spawn an ordinary Green Slime at a world-pixel point, with its feet at that point. -/
def State.addGreenSlime (state : State) (x y : Int) : State :=
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  { state with combat := state.combat.addSlime (combatWorld state grid) x y }

private def State.finishMove (state : State) : State :=
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  let player : OriginalPlayerWet.Player := ⟨state.player, state.playerLiquid⟩
  let player := (player.contact grid).finish grid
  { state with player := player.move, playerLiquid := player.liquid }

def State.move (state : State) (input : Controls) : State :=
  (state.beginMove input).finishMove

private def State.spawnGroundDrop (state : State) (cell : Cell) (item : UInt16) : State :=
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  let (worldItems, itemRandom) := OriginalItems.spawnBlock state.worldItems cell item state.itemRandom (some grid)
  { state with worldItems, itemRandom }

private def State.spawnGravestoneGhost (state : State) :
    OriginalPlayerDeath.SpawnGravestoneGhost := fun mining _ =>
  let player : OriginalGhostSpawn.Player := { slot := 0, body := state.player.body, heldItem := 3509 }
  let world : OriginalGhostSpawn.World := {
    grid := Grid.ofTerrain mining.terrain OriginalTileRules.collision
    houseWall := OriginalWallWork.Tables.wallHouse
    players := #[player]
    npcSlot := fun index =>
      if mining.spawnedGhosts.any (·.slot == index) then { active := true }
      else
        let actor := state.combat.actors[index]?.getD .empty
        { active := actor.active, protection := state.combat.protectedSlots[index]?.getD 0 } }
  let result := OriginalGhostSpawn.spawnOnPlayer world player mining.random
  match result.ghost with
  | none => { mining with random := result.random }
  | some ghost => { mining with
      random := result.random
      activeGhosts := mining.activeGhosts + 1, spawnedGhosts := mining.spawnedGhosts.push ghost }

private def State.useGravestonePick (state : State) (held : Bool) (cell : Cell)
    (id : UInt16) : State × Option UInt16 := Id.run do
  let timing := state.itemUse.advance held 23
  let updated := { state with itemUse := timing }
  if !(held && timing.itemAnimation > 0 && timing.itemTime == 0 &&
      OriginalMining.inToolRange state.player.body cell 5 3 (-1)) then return (updated, none)
  let world : OriginalPlayerDeath.GravestoneMiningWorld := {
    terrain := state.trackTerrain, items := state.worldItems, random := state.itemRandom, signs := state.signs
    activeGhosts := state.combat.ghosts.countP (·.active) }
  let rules : OriginalMining.WorldRules := { surface := state.terrain.header.surface }
  let spawnGhost := state.spawnGravestoneGhost
  let strike := if id == 85 then OriginalPlayerDeath.hitGravestone rules world (hitCacheOf state.hits) cell 35 spawnGhost
    else OriginalPlayerDeath.hitGravestoneSupport rules world (hitCacheOf state.hits) cell 35 spawnGhost
  let mined := strike.world
  let broken := (mined.terrain.tile? cell.x cell.y).all (·.id.isNone)
  let drop := if !broken then none
    else if id == 85 then mined.removals[0]?.map (·.item)
    else OriginalMining.itemForGround id
  let terrain := if broken then OriginalPlacement.reframeAfterRemoval mined.terrain cell else mined.terrain
  let combat := mined.spawnedGhosts.foldl (fun combat ghost => combat.setActor ghost.slot (.tombstoneGhost ghost)) state.combat
  let updated := { updated with
    itemUse := { timing with itemTime := 15 }, hits := adoptHitCache state.hits strike.hits, combat
    worldItems := mined.items, itemRandom := mined.random
    signs := mined.signs, signsChanged := state.signsChanged || mined.signs != state.signs }
  return (updated.adoptTerrain terrain, drop)

/-- Use the selected Copper Pickaxe on the current terrain. The item result
describes the direct break. Further objects may drop when they lose support. -/
def State.useCopperPick (state : State) (held : Bool) (target : Option Cell) :
    State × Option UInt16 := Id.run do
  if state.terrain.header.gameMode == 0 then
    if let some cell := target then
      if let some id := (state.terrain.tile? cell.x cell.y).bind (·.id) then
        let support := (id == 0 || id == 1) && cell.y > 0 &&
          (state.terrain.tile? cell.x (cell.y - 1)).any (·.id == some 85)
        if id == 85 || support then return state.useGravestonePick held cell id
  let world : OriginalChestMining.World := {
    terrain := state.trackTerrain, items := state.worldItems, random := state.itemRandom, chests := state.chests
    effects := state.tileHitEffects,frameNumbers := state.tileFrameNumbers }
  let (copperPick, mined) :=
    OriginalChestMining.advance state.copperPick state.player.body
      { surface := state.terrain.header.surface } world held target
  let updated := { state with
    -- ActuallyUseMiningTool (IL_0413..0423) stores itemTime directly: itemTimeMax keeps its value.
    itemUse := { copperPick.toTiming with itemTimeMax := state.itemUse.itemTimeMax }
    hits := adoptHitCache state.hits copperPick.hits
    worldItems := mined.items
    itemRandom := mined.random
    chests := mined.chests
    tileFrameNumbers := mined.frameNumbers
    tileHitEffects := mined.effects
    openStorage := state.openStorage.filter fun storage =>
      match storage with
      | .chest index => !mined.freed.contains index
      | .bank _ => true }
  let some cell := target | return (updated.adoptTerrain mined.terrain, none)
  let some id := (state.terrain.tile? cell.x cell.y).bind (·.id)
    | return (updated.adoptTerrain mined.terrain, none)
  if (mined.terrain.tile? cell.x cell.y).any (·.id.isSome) then
    return (updated.adoptTerrain mined.terrain, none)
  let terrain := OriginalPlacement.reframeAfterRemoval mined.terrain cell
  let drop := if OriginalChestMining.isContainerTile id then mined.drops[0]?.map (·.1)
    else if OriginalBankMining.owns id then
      (OriginalBankMining.dropFor id 0).filter fun item => mined.drops.any (·.1 == item)
    else OriginalMining.itemForGround id
  return (updated.adoptTerrain terrain, drop)

private def State.grabItems (state : State) (selected : Nat)
    (context : Option OriginalTileHitEffects.Context := none)
    (preventAllItemPickups : Bool := false) : State := Id.run do
  let slotEmpty := (state.equipped selected).isNone
  let mut state := state
  -- PickupItem executes each slot's effects before reaching the next world
  -- item. In particular, a font failure leaves the current heart/star active
  -- and must not heal, transfer or remove any later slot.
  for index in [:state.worldItems.size] do
    if state.combat.textBoundary.isSome then return state
    if (state.worldItems[index]?.join).isNone then continue
    let result := OriginalItems.grabInto state.player.body
      (slotEmpty && state.itemUse.itemAnimation > 0) state.banks.voidVaultEnabled
      state.carriedItems state.banks.voidVault state.worldItems
      (state.statManaMax2 - state.health.statMana).toNat
      (preventAllItemPickups := preventAllItemPickups) (slotRange := some (index,index+1))
    let mut textRng := state.playerTextRng
    let mut health := state.health
    let mut manaHeat := state.manaHeat
    let mut pickupSoundRequests := state.pickupSoundRequests
    let mut pickupSounds := state.pickupSounds
    for effect in result.effects do
      if textRng.boundary.isSome then break
      match effect with
      | .healText =>
        health := {health with statLife := health.statLife + 20}
        textRng := textRng.combatNumber 20 OriginalCombatText.healLife false false
        if textRng.boundary.isNone then
          health := {health with statLife := min health.statLifeMax2 health.statLife}
      | .manaText =>
        health := {health with statMana := health.statMana + 100}
        let applied := manaHeat.apply 0.2 2
        manaHeat := applied.state
        health := applied.buffTime.elim health fun time => health.addBuff 396 time
        textRng := textRng.combatNumber 100 OriginalCombatText.healMana false false
        if textRng.boundary.isNone then
          health := {health with statMana := min state.statManaMax2 health.statMana}
      | .sound typeId =>
        if let some context := context then
          let request : OriginalTileHitEffects.SoundRequest := {typeId,x := -1,y := -1}
          let (played,random) := if typeId == 38 then
              OriginalPickupSound.playCoin context textRng.stream
            else OriginalPickupSound.playGrab context textRng.stream
          textRng := {textRng with stream := random}
          pickupSoundRequests := pickupSoundRequests.push request
          pickupSounds := pickupSounds ++ played.toArray
    state := {state.acceptTextRng textRng with health,manaHeat,pickupSoundRequests,pickupSounds}
    if textRng.boundary.isSome then return state
    state := {state.setCarriedItems result.carried with
      banks := state.banks.replace .voidVault result.vault
      worldItems := result.items
      collected := state.collected || result.collected}
  return state

private def State.refreshInventory (state : State) : State :=
  let ticks := (state.inventoryRefreshTicks + 1) % 10
  let inventory := if ticks == 0 then
      state.inventory.map (fun slot => slot.filter (·.count > 0))
    else state.inventory
  let mouseItem := if ticks == 0 && state.selectionLive then state.mouseItem.filter (·.count > 0)
    else state.mouseItem
  { state with inventory, mouseItem, inventoryRefreshTicks := ticks }

private def State.replaceUsedStack (state : State) (selected : Nat) (stack : Option OriginalItems.Stack) : State :=
  if state.heldIndex selected == 58 then
    -- A client's cursor keeps the emptied `(type, 0)` stack like any inventory slot; the periodic
    -- cleanup of `refreshInventory` turns it to air.
    { state with mouseItem := if state.selectionLive then stack else stack.filter (·.count > 0) }
  else { state with inventory := state.inventory.set! selected stack }

private def State.spawnBucketOverflow (state : State)
    (overflow : Option OriginalLiquidBuckets.Overflow) : State :=
  match overflow with
  | none => state
  | some drop =>
    let velocity : OriginalCollision.Vector := ⟨single drop.drawX * 0.1, single drop.drawY * 0.1⟩
    let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
    let position := state.playerDropPosition
    let worldItems := OriginalItems.spawnStackWithVelocity state.worldItems
      position velocity drop.stack (OriginalItems.spawnWet grid position)
    { state with worldItems }

private def State.useBucket (state : State) (selected : Nat) (stack : OriginalItems.Stack) (held : Bool)
    (target : Option Cell) (soundStarts : Bool) : State := Id.run do
  let some runtime := state.liquids
    | return { state with itemUse := state.itemUse.advance false 0 false }
  let carry : OriginalLiquidBuckets.Carry :=
    { inventory := state.carriedItems, mouse := state.mouseItem
      selected := state.heldIndex selected }
  let result := OriginalLiquidBuckets.advance runtime state.itemRandom state.itemUse carry
    { held, target, body := state.player.body, soundStarts }
  if !result.used then return { state with itemUse := result.timing }
  let state := { state.setCarriedItems (result.carry.inventory.map (·.filter (·.count > 0))) with
    mouseItem := result.carry.mouse.filter (·.count > 0)
    itemUse := result.timing
    itemRandom := result.playerRandom
    bucketSplash := result.sound }
  -- On pickup GetOrDropItem runs before framing. On pour it runs afterwards.
  if stack.item == 205 then
    return (state.spawnBucketOverflow result.overflow).commitLiquids result.liquids
  return (state.commitLiquids result.liquids).spawnBucketOverflow result.overflow

private def State.acceptWallWork (state : State) (work : OriginalWallWork.Work) : State :=
  let state := { state with wallFrames := work.frames, itemRandom := work.random, worldItems := work.items }
  if work.changed.isEmpty then state
  else { state with
    terrain := work.terrain
    liquids := state.liquids.map fun runtime => runtime.adoptWallEdits work.terrain work.changed.toList }

private def State.emitToolSwing (state : State) (starts : Bool) : State :=
  if !starts then state else
  match state.tileHitEffects with
  | none => state
  | some effects =>
    if effects.context.dedicatedServer then state else
    let body := state.player.body
    let x := (body.position.x + single body.width * 0.5).toInt32.toInt
    let y := (body.position.y + single body.height * 0.5).toInt32.toInt
    let (played,itemRandom) := OriginalItemUseSound.playSwing x y effects.context state.itemRandom
    let request : OriginalTileHitEffects.SoundRequest := {typeId := 2,x,y,style := 1}
    -- `PlaySound(item.UseSound, Center)` is the LegacySoundStyle overload: one sample of the
    -- client-wide `SoundStyle._random`, before any gate.
    { state with itemRandom, pendingStyleSamples := state.pendingStyleSamples + 1, tileHitEffects := some { effects with
        soundRequests := effects.soundRequests.push request
        resolvedSounds := effects.resolvedSounds ++ played.toArray } }

private def State.useWallItem (state : State) (selected : Nat) (stack : OriginalItems.Stack) (held pressed : Bool)
    (target : Option Cell) (mouse : Option (Float32 × Float32)) : State := Id.run do
  let header := state.terrain.header
  let progress : OriginalWallWork.Progress := ⟨state.downedSkeletron, header.downedGolem⟩
  let stored x y := some (OriginalWallWork.fallbackWallNumber header.id x y)
  let work : OriginalWallWork.Work := {
    terrain := state.terrain, frames := state.wallFrames, random := state.itemRandom, items := state.worldItems }
  if let some tool := OriginalWallWork.Tables.hammerStats stack.item then
    let state := state.emitToolSwing (state.itemUse.willStart (held && (tool.autoReuse || pressed)))
    let work := { work with random := state.itemRandom }
    let mouse := mouse.getD <| (target.map fun cell =>
      (single (16 * cell.x + 8 : Nat), single (16 * cell.y + 8 : Nat))).getD (0, 0)
    let hammered := OriginalHammerForeground.hammer { work with terrain := work.terrain.trackEdits }
      stored state.tileFrameNumbers state.hits state.poundRelease state.player.body
      state.itemUse stack.item { held, pressed, target, mouse } progress
      (effectsState := state.tileHitEffects)
    let state := state.acceptWallWork { hammered.work with terrain := hammered.work.terrain.finishEdits.1 }
    let state := { state with tileFrameNumbers := hammered.frames, tileHitEffects := hammered.effectsState }
    let state := if let some cell := hammered.shaped then
        let pos := (cell.x, cell.y)
        let shaped := state.adoptTerrain hammered.work.terrain (some [pos])
        if shaped.foregroundEdits.contains pos then shaped
        else { shaped with foregroundEdits := shaped.foregroundEdits.push pos }
      else state
    let body := state.player.body
    let body := match hammered.push with
      | none => body
      | some push => { body with
          position := { body.position with y := push.y }
          gfxOffY := body.gfxOffY + push.gfxOffY }
    -- The hammer works on the shared `Player.hitTile` itself (its ClearMiningCacheAt and prunes reach the
    -- pickaxe entries natively); ClearMiningCacheAt also clears the cell in `hitReplace`.
    let swapHits := match hammered.cleared with
      | none => state.swapHits
      | some cell => state.swapHits.forget cell
    return { state with
      player := { state.player with body }
      hits := hammered.hits, itemUse := hammered.timing, poundRelease := hammered.poundRelease
      swapHits }
  let placed := OriginalWallWork.place work stored state.player.body state.itemUse stack held target
    state.tileReplacement progress
  let state := { state.acceptWallWork placed.work with itemUse := placed.timing }
  let state := state.replaceUsedStack selected placed.stack
  return state

-- BEGIN B-ZONE actions-pickaxe (S02)
/-- `ItemCheck` with a pickaxe through `OriginalPlayerActions.usePickaxe` (the MiningTools leaf's verified
caller recipe, then `ItemCheck_UseMiningTools`) on the shared `Player.hitTile`.

* The swing sound (`ItemCheck_StartActualUse`, the session's `emitToolSwing`) is drawn first, as for the
  copper pick; the leaf's own draws (AddDamage, KillTile sound/dust, frame re-roll, drops) follow on the
  same `Main.rand` (`itemRandom`). Sounds and dust are already resolved inside the shared
  `OriginalTileHitRuntime.State` (`tileHitEffects`) and reach the window through it: nothing is drawn again.
* `outcome.unsupported` (a target or neighbour outside the ordinary-block scope: chests, banks,
  gravestones, trees, platforms, …) is never accepted, whether the leaf refused before any change or
  stopped partway. The existing owners of those targets are written for the copper pick's power and
  timing (35, 23 frames), so there is no owner for this pickaxe: the update stops with a boundary.
* BOUNDARY (declared): `Player.noBuilding` false (no Creative Shock / zone flag in the session);
  `notTheBeesWorld` / `noTrapsWorld` false (not in the header; only read for tile 137, outside the leaf's
  scope); the Nightmare Pickaxe's swing dust (8 `Main.rand` draws outside `UseMiningTools`) is not modelled,
  so a live Nightmare swing stops with a boundary. `cursorItemIconEnabled` is not consumed (as for the copper
  pick). -/
private def State.usePickaxe (state : State) (pick : OriginalMiningTools.Pickaxe) (held : Bool)
    (target : Option Cell) (mouse : Option (Float32 × Float32)) : State := Id.run do
  let stop (state : State) (why : String) : State := { state with combat := { state.combat with
      npcBoundary := state.combat.npcBoundary.or (some (0, s!"pickaxe {pick.itemType}: {why}")) } }
  -- `Player.tileTargetX/Y`: the target cell, else the cursor's world tile
  let tileTarget : Option (Int × Int) := match target, mouse with
    | some cell, _ => some (cell.x, cell.y)
    | none, some (x, y) => some ((x / 16).toInt32.toInt, (y / 16).toInt32.toInt)
    | none, none => none
  let some tileTarget := tileTarget
    | return stop state "no Player.tileTargetX/Y (neither a target cell nor a cursor position)"
  let state := state.emitToolSwing (state.itemUse.willStart held)
  let header := state.terrain.header
  let rules : OriginalMiningTools.Rules := {
    maxTilesX := header.width, maxTilesY := header.height, worldSurface := header.surface
    hardMode := header.hardMode, getGoodWorld := header.forTheWorthy, infectedSeed := header.infectedSeed
    notTheBeesWorld := false, noTrapsWorld := false, tenthAnniversaryWorld := header.tenthAnniversary
    netMode := 0, generatingWorld := false, tileRangeX := 5, tileRangeY := 3
    gamepadDisableCursorItemIcon := false }
  let body := state.player.body
  let input : OriginalPlayerActions.PickaxeInput := {
    timing := state.itemUse, held, poundRelease := state.poundRelease
    position := body.position, width := body.width, height := body.height
    pickSpeed := (state.equipmentEffects.map (·.pickSpeed)).getD 1
    noBuilding := false, target := tileTarget }
  let leafState : OriginalMiningTools.State := {
    terrain := state.trackTerrain, items := state.worldItems, random := state.itemRandom
    hits := state.hits, hitReplace := adoptHitCache {} state.swapHits
    frameNumbers := state.tileFrameNumbers, effects := state.tileHitEffects }
  let result := OriginalPlayerActions.usePickaxe rules pick input leafState
  let outcome := result.outcome
  if let some why := outcome.unsupported then
    return stop state s!"{why} (no owner for this pickaxe; nothing of the leaf's result was accepted)"
  if pick.itemType == 103 && result.swing.itemAnimation > 0 then
    return stop state "Nightmare Pickaxe swing dust (8 Main.rand draws outside UseMiningTools) not modelled"
  let mined := outcome.state
  let state := { state with
    -- a returned itemTime was stored directly; itemTimeMax keeps its value
    itemUse := result.timing, poundRelease := outcome.poundRelease
    worldItems := mined.items, itemRandom := mined.random
    hits := mined.hits, swapHits := hitCacheOf mined.hitReplace
    tileFrameNumbers := mined.frameNumbers, tileHitEffects := mined.effects }
  -- the leaf leaves frameX/frameY of re-framed cells to the framing owner (as the copper pick does)
  let terrain := outcome.events.foldl (init := mined.terrain) fun terrain event =>
    match event with
    | .kill x y false =>
      if x ≥ 0 && y ≥ 0 && (terrain.tile? x.toNat y.toNat).any (·.id.isNone) then
        OriginalPlacement.reframeAfterRemoval terrain ⟨x.toNat, y.toNat⟩
      else terrain
    | _ => terrain
  return state.adoptTerrain terrain
-- END B-ZONE actions-pickaxe

-- BEGIN B-ZONE projectiles-bow (S03)
/-- `Player.ItemCheck` with a bow: `OriginalPlayerActions.useBow` (the released `OriginalBowUse.itemCheck` on the
UpdatePlayers stream, then the same frame's `NewProjectile` into the session's pool). The launched Wooden Arrow is
re-entered in its slot as `OriginalCombat.Projectile.woodenArrow` (slot and generation as allocated), so it updates
in the one projectile loop with the other kinds. `releaseUseItem` is the value the previous ItemCheck left
(`priorRelease`). BOUNDARY (stops the update, `combat.npcBoundary`): a spawn of a type no module owns (Flaming
Arrow → `unsupportedSpawn`; its slot is still allocated), a frame without a cursor/camera, a stored prefix that
`Item.Prefix` rejects, an audible use sound (its `Main.rand` pitch sample falls between the leaf's draws and the
window has no Item_5 voice). Declared: `reuseDelay` 0, `pendingItemReuse`/`channel` false, `attackCD` 0, ammo
flags (quivers, ammo box/potion, archery…) false — no producer in the session. -/
private def State.useBowItem (state : State) (bow : OriginalBowUse.Bow) (stack : OriginalItems.Stack)
    (held priorRelease : Bool) (mouse : Option (Float32 × Float32))
    (viewport : Option OriginalForestGuideArrow.Viewport) : State := Id.run do
  let stop (state : State) (why : String) : State := { state with combat := { state.combat with
      npcBoundary := state.combat.npcBoundary.or (some (0, s!"bow {stack.item}: {why}")) } }
  let some view := viewport | return stop state "no camera (Main.screenPosition) for the bow's aim"
  let some (mx, my) := mouse | return stop state "no cursor position for the bow's aim"
  -- a prefixed bow: its `Item` with the prefix applied; a rejected prefix would re-roll on Main.rand
  let bow ← if stack.prefixId == 0 then pure bow else
    match OriginalPlayerActions.bowFor stack.item stack.prefixId.toNat state.itemRandom with
    | some (b, rng) =>
      if rng.state != state.itemRandom.state then return stop state s!"prefix {stack.prefixId} rejected (re-roll)"
      pure b
    | none => return stop state s!"prefix {stack.prefixId} unknown"
  let slotOf (s : Option OriginalItems.Stack) : OriginalBowUse.Slot :=
    match s with
    | some st => { type := st.item.toNat, stack := st.count }
    | none => {}
  let live (id : Nat) := (state.health.buffType.zip state.health.buffTime).any fun (b, t) => b == id && t > 0
  let body := state.player.body
  let frame : OriginalBowUse.Frame := {
    controlUseItem := held, ccd := live 47 || live 353 || live 149 || live 156
    mouseX := (mx - view.position.x).toInt32.toInt, mouseY := (my - view.position.y).toInt32.toInt
    screenPosition := view.position, screenHeight := view.height
    position := body.position, width := body.width, height := body.height, gfxOffY := body.gfxOffY, gravDir := 1
    stats := (state.equipmentEffects.map OriginalWeaponStats.Stats.ofEffects).getD {}, flags := {} }
  let timing := state.itemUse
  let leafState : OriginalBowUse.State := {
    itemAnimation := timing.itemAnimation, itemAnimationMax := timing.itemAnimationMax
    itemTime := timing.itemTime, itemTimeMax := timing.itemTimeMax, reuseDelay := 0
    releaseUseItem := priorRelease, pendingItemReuse := false, channel := false
    lastItemUseAttemptSuccess := state.lastItemUseAttemptSuccess, attackCD := 0
    direction := state.player.direction, itemRotation := state.combat.use.itemRotation
    revolverCritChanceBonus := state.revolverCritChanceBonus
    inventory := (state.carriedItems.map slotOf).push (slotOf state.mouseItem) }
  let result := OriginalPlayerActions.useBow bow frame leafState state.itemRandom state.combat.projectiles 0
  let out := result.output.state
  -- the launched arrow joins the one loop as a session projectile kind (same slot, same generation)
  let mut combat := { state.combat with projectiles := result.pool }
  if let some arrow := result.launched then
    combat := combat.setProjectile arrow.slot (.woodenArrow arrow)
  let back (old : Option OriginalItems.Stack) (slot : OriginalBowUse.Slot) : Option OriginalItems.Stack :=
    if slotOf old == slot then old
    else if slot.type == 0 || slot.stack ≤ 0 then none
    else old.map fun st => { st with count := slot.stack.toNat }
  let carried := state.carriedItems.mapIdx fun i old => back old (out.inventory[i]?.getD {})
  let mouseItem := back state.mouseItem (out.inventory[carried.size]?.getD {})
  let state := { state.setCarriedItems carried with
    mouseItem, itemRandom := result.output.random
    itemUse := { itemAnimation := out.itemAnimation.toNat, itemAnimationMax := out.itemAnimationMax.toNat
                 itemTime := out.itemTime.toNat, itemTimeMax := out.itemTimeMax.toNat }
    lastItemUseAttemptSuccess := out.lastItemUseAttemptSuccess
    revolverCritChanceBonus := out.revolverCritChanceBonus
    player := { state.player with direction := out.direction }
    combat := { combat with use := { combat.use with
      releaseUseItem := out.releaseUseItem, itemRotation := out.itemRotation, direction := out.direction } } }
  let state := match result.unsupportedSpawn with
    | some spawn => stop state s!"NewProjectile type {spawn.projectileType} (ammo {spawn.usedAmmoItemId}) has no owner"
    | none => state
  -- ItemCheck_StartActualUse: PlaySound(item.UseSound) (LegacySoundStyle Item5): one SoundStyle._random sample;
  -- an audible type-2 call also samples Main.rand for its pitch between the leaf's own draws
  if result.output.useSound then
    if let some effects := state.tileHitEffects then
      if !effects.context.dedicatedServer then
        let state := { state with pendingStyleSamples := state.pendingStyleSamples + 1 }
        if effects.context.audioSupported && effects.context.soundVolume != 0 then
          return stop state "audible use sound: its Main.rand pitch sample is not interleaved with the leaf's draws"
        return state
  return state
-- END B-ZONE projectiles-bow

private def State.useSelected (state : State) (selected : Nat)
    (held : Bool) (target : Option Cell) (bucketSplashReady : Bool) (mouse : Option (Float32 × Float32))
    (priorRelease : Bool := false) (viewport : Option OriginalForestGuideArrow.Viewport := none) : State := Id.run do
  let pressed := held && !state.wallUseHeld
  let state := { state with wallUseHeld := held }
  let some stack := state.equipped selected
    | return { state with itemUse := state.itemUse.advance false 0 false }
  if OriginalLiquidBuckets.isBucket stack.item then
    return state.useBucket selected stack held target bucketSplashReady
  -- BEGIN B-ZONE actions-dispatch (S02)
  -- The copper pick (3509) keeps its verified session path (chest/bank/gravestone routing); the other
  -- pickaxes go through OriginalPlayerActions.
  if let .pickaxe pick := OriginalPlayerActions.actionOf stack.item.toNat then
    if stack.item != OriginalItems.copperPickaxe then
      return state.usePickaxe pick held target mouse
  -- END B-ZONE actions-dispatch
  -- BEGIN B-ZONE projectiles-dispatch (S03)
  if let .bow bow := OriginalPlayerActions.actionOf stack.item.toNat then
    return state.useBowItem bow stack held priorRelease mouse viewport
  -- END B-ZONE projectiles-dispatch
  if stack.item == OriginalItems.copperPickaxe then
    let state := state.emitToolSwing (state.itemUse.willStart held)
    let used := (state.useCopperPick held target).1
    if state.itemUse.itemTime ≤ 1 && used.itemUse.itemTime > 0 then
      return { used with hits := pruneWallHits used.hits used.terrain }
    return used
  if stack.item == OriginalItems.copperAxe then
    let state := state.emitToolSwing (state.itemUse.willStart held)
    let strike := OriginalTree.advance state.trackTerrain state.player.body state.itemUse
      (hitCacheOf state.hits) state.worldItems state.itemRandom held target
    let state := { state with
      worldItems := strike.felled.items
      itemRandom := strike.felled.random
      itemUse := strike.timing
      hits := adoptHitCache state.hits strike.hits }
    return state.adoptTerrain strike.felled.terrain
  if (OriginalWallWork.Tables.wallItem stack.item).isSome || (OriginalWallWork.Tables.hammerStats stack.item).isSome then
    return state.useWallItem selected stack held pressed target mouse
  if (OriginalChestMining.placeFor stack.item).isSome then
    let placed := OriginalChestMining.place state.trackTerrain state.chests state.player.body
      state.itemUse stack held target
    let state := { state with chests := placed.chests, itemUse := placed.timing }
    return (state.replaceUsedStack selected placed.stack).adoptTerrain placed.terrain
  if OriginalCandlePlacement.isCandleItem stack.item then
    -- Water, Peace and Shadow Candles: the 15-frame use starts whether or not a table top is in
    -- reach; `advance` draws no randoms and keeps the emptied stack until the animation ends.
    let placed := OriginalCandlePlacement.advance state.trackTerrain state.player.body state.itemUse stack held target
    let state := { state with itemUse := placed.timing, candlePlacementSound := placed.sound }
    let state := match placed.boundary with
      | some why => { state with combat := { state.combat with
          npcBoundary := state.combat.npcBoundary.or (some (0, s!"candle placement: {why}")) } }
      | none => state
    return (state.replaceUsedStack selected placed.stack).adoptTerrain placed.terrain
  let placementTile := match stack.item with
    | 36 => some 18
    | 94 => some 19
    | item => (OriginalBanks.tileForItem item).orElse fun _ => OriginalMining.stableTileForItem item
  let some tile := placementTile
    | return { state with itemUse := state.itemUse.advance false 0 false }
  let hasCopperPick := state.inventory.any fun slot =>
    slot.any (fun stack => stack.item == OriginalItems.copperPickaxe && stack.count > 0)
  let result := OriginalPlacement.advance state.trackTerrain state.player.body state.itemUse stack tile
    held target hasCopperPick state.swapHits
  let state := { state with
    itemUse := result.timing
    swapHits := result.swapHits }
  let state := state.replaceUsedStack selected result.stack
  let state := if let (some cell, some drop) := (target, result.drop) then
      { state.spawnGroundDrop cell drop with hits := adoptHitCache state.hits ((hitCacheOf state.hits).forget cell) }
    else state
  return state.adoptTerrain result.terrain

private def State.dropDeathCoins (state : State) (grid : Grid) : State := Id.run do
  let body := state.player.body
  let position := ⟨single body.position.x.toInt32.toInt + single (body.width / 2) - 8,
    single body.position.y.toInt32.toInt + single (body.height / 2) - 8⟩
  let wet := OriginalItems.spawnWet grid position
  let mut items := state.worldItems
  let mut rng := state.itemRandom
  let mut lostCoins := 0
  let mut inventory := state.carriedItems.push state.mouseItem
  for slot in [:inventory.size] do
    let some stack := inventory[slot]! | continue
    if !stack.isCoin || stack.count == 0 then continue
    let keep := match state.terrain.header.gameMode with
      | 1 => stack.count / 4
      | 2 => 0
      | _ => stack.count / 2
    let (vx, next) := rng.next (-20) 21
    let (vy, next) := next.next (-20) 1
    rng := next
    let dropped := { stack with count := stack.count - keep, favorite := false }
    lostCoins := lostCoins + dropped.count * 100 ^ (stack.item.toNat - 71)
    items := OriginalItems.spawnStackWithVelocity items position ⟨single vx * 0.2, single vy * 0.2⟩ dropped wet
    inventory := inventory.set! slot (if keep > 0 then some { stack with count := keep } else none)
  let cursor := inventory[inventory.size - 1]!
  let state := state.setCarriedItems (inventory.extract 0 (inventory.size - 1))
  return { state with
    worldItems := items, itemRandom := rng, mouseItem := cursor
    health := { state.health with lostCoins } }

private def ContactDeathText.inscription (text : ContactDeathText) (worldName killer : String)
    (index : Option Nat) : String :=
  let line := (index.bind (text.templates[·]?)).map (fun template =>
    let generic := String.intercalate text.playerName <|
      (template.splitOn "{0}").map fun fragment => fragment.replace "{1}" worldName
    s!"{generic} by {killer}.")
  OriginalPlayerDeath.gravestoneInscription (line.getD "DeathSource.NPC") text.date

/-- The gravestone text of a death by `ByOther(index)`: the localized text of `key` with the
player's name, or the key itself when the window has no such text. -/
private def ContactDeathText.otherInscription (text : ContactDeathText) (key : String) : String :=
  let line := ((text.named.find? (·.1 == key)).map fun (_, template) => template.replace "{0}" text.playerName).getD key
  OriginalPlayerDeath.gravestoneInscription line text.date

/-- The local player's `KillMe`, in its own order (IL in `death-runtime/il/`): death sound →
corpse velocities → the dust explosion → death text rolls and the chat message → DropCoins →
the cursor item → DropTombstone. `contact` is a death by an NPC (hit direction, its name).
`other` is `PlayerDeathReason.ByOther(index)` with hit direction 0 (0 fell, 1 drowned, 2 lava,
8 burned): a window client runs the same complete KillMe for it; without a client such a death
keeps the legacy contract (coins and counters only), as the earlier headless evidence had it. -/
private def State.registerDeath (state : State) (grid : Grid)
    (contact : Option (Int × String) := none)
    (viewport : Option OriginalForestGuideArrow.Viewport := none)
    (other : Option Nat := none) (client : Option ClientInput := none) : State := Id.run do
  let carriedCopper := (state.carriedItems.push state.mouseItem).foldl (init := 0) (fun amount slot =>
    match slot with
    | some stack => if stack.isCoin then amount + stack.count * 100 ^ (stack.item.toNat - 71) else amount
    | none => amount)
  -- KillMe IL_00c8..0107: the PvE death counter, the last-death position, time and marker are
  -- committed before the death sound and everything after it.
  let body := state.player.body
  let state := { state with
    health := { state.health with deathsPVE := state.health.deathsPVE + 1 }
    lastDeathPosition := ⟨body.position.x + single (body.width / 2), body.position.y + single (body.height / 2)⟩
    lastDeathTime := (client.bind (·.clockTicks)).orElse fun _ => state.lastDeathTime
    showLastDeath := true }
  let cause : Option (Int × Option String × Nat) := match contact with
    | some (direction, name) => some (direction, some name, 0)
    | none => if client.isSome then other.map fun index => (0, none, index) else none
  let (state, inscription) := match cause with
    | some (direction, killer, index) =>
      -- PlayDeathSound (IL_01a2): before every roll of KillMe; it draws nothing itself.
      let body := state.player.body
      let state := match client.bind (·.tileHitContext) with
        | some context =>
          let (request, played) := OriginalPlayerDeath.playDeathSound context body.position
          { state with deathSoundRequest := some request, deathSound := played }
        | none => state
      let (corpse, itemRandom) := OriginalPlayerDeath.corpseVelocities direction state.itemRandom
      -- KillMe_DustExplosion (IL_05ed): 100 type-5 NewDust calls in the real shared pool when the
      -- client has a viewport; each call's own allocation decides its draws. A caller without a
      -- viewport keeps the legacy declared count of accepted allocations.
      let (dust, itemRandom) := match viewport with
        | some _ =>
          let burst := OriginalPlayerDeath.dustExplosion body.position body.width body.height direction
            (state.combat.dustPool viewport) itemRandom
          (burst.1, burst.2)
        | none => (#[], OriginalPlayerDeath.consumeBloodDustAllocations state.contactDeathDust itemRandom)
      -- CreateDeathMessage: the generic category roll for every reason, then the reason's own.
      let (message, itemRandom) := OriginalPlayerDeath.deathTextIndex state.contactDeathText.templates.size itemRandom
      let (inscription, itemRandom) := match killer with
        | some name => (state.contactDeathText.inscription state.terrain.header.name name message, itemRandom)
        | none =>
          let (key, itemRandom) := OriginalPlayerDeath.otherDeathTextKey index itemRandom
          (state.contactDeathText.otherInscription key, itemRandom)
      ({ state with corpse := some corpse, itemRandom
                    corpseParts := some (OriginalPlayerDeath.CorpseParts.ofVelocities corpse)
                    -- BroadcastChatMessage → Main.NewTextMultiline plays MenuTick on a client.
                    deathMessageTick := client.isSome
                    combat := { state.combat with guideDust := state.combat.guideDust ++ dust } }, some inscription)
    | none => (state, none)
  let state := state.dropDeathCoins grid
  let state := match state.mouseItem with
    | none => state
    | some stack =>
      let (items, remainder) := OriginalItems.store state.carriedItems stack
      { state.setCarriedItems items with mouseItem := remainder }
  let state := { state with openStorage := none }
  let some (direction, _, _) := cause | return state
  let (grave, itemRandom) := OriginalPlayerDeath.throwGravestone state.player.body direction carriedCopper
    state.itemRandom (inscription.getD "")
  -- NewProjectile: wet at once when created inside a liquid.
  return { state with combat := state.combat.insertProjectile (.gravestone (grave.inWorld grid)), itemRandom }

private def State.breakGravestoneObstacle (state : State) (grid : Grid)
    (x y : Nat) (tile : OriginalWorld.Tile) : State := Id.run do
  let some id := tile.id | return state
  let random := state.combat.projectileRandom
  let (frameX, frameY) := tile.frame.getD (0, 0)
  let activeCount := fun (typeId : Int) => state.combat.actors.countP
    fun (actor : OriginalCombat.Actor) => actor.active && actor.typeId == typeId
  let graveyard := state.forest.any fun forest => (forest.playerFlags.lookup "ZoneGraveyard").getD false
  let wormType := if graveyard then 606 else 357
  let mut remaining := random
  let mut item : Option UInt16 := none
  let mut unsupported : Option String := none
  if id == 3 || id == 185 then
    let pileWithBait := (frameY == 0 && frameX < 214) ||
      (frameY == 18 && (frameX < 214 || frameX ≥ 1368))
    if (id == 3 || pileWithBait) && activeCount wormType < 5 then
      let (worm, next) := remaining.next 0 (if id == 3 then 400 else 6)
      remaining := next
      if worm == 0 then unsupported := some "worm from a gravestone footprint"
    if id == 3 then
      if activeCount 377 < 5 then
        let (grasshopper, next) := remaining.next 0 100
        remaining := next
        if grasshopper == 0 then unsupported := some "grasshopper from a gravestone footprint"
      if frameX == 144 then item := some 5 else
        let (seeds, next) := remaining.next 0 2
        remaining := next
        if seeds == 0 && state.carriedItems.any (Option.any fun stack => stack.item == 281 || stack.item == 986) then
          item := some 283
  else
    unsupported := some s!"tile {id} broken by gravestone placement"
  let (items, random) := match item with
    | none => (state.worldItems, remaining)
    | some item =>
      let position := ⟨single (16 * x), single (16 * y)⟩
      let (drop, random) := OriginalItems.rollDropAt position (OriginalItems.Stack.ofItem item 1) remaining (some grid)
      (OriginalItems.insertDrop state.worldItems drop, random)
  { state with worldItems := items, combat := { state.combat with
      projectileRandom := random, npcBoundary := state.combat.npcBoundary.or (unsupported.map (0, ·)) } }

private def State.placeGravestone (initial : State) (placement : GravestonePlacement) : State := Id.run do
  let mut state := initial
  let mut terrain := state.terrain.trackEdits
  for (x, y, frameX, frameY) in placement.frames do
    let some tile := terrain.tile? x y | return state
    state := state.breakGravestoneObstacle (Grid.ofTerrain terrain OriginalTileRules.collision) x y tile
    let some next := terrain.replaceTile x y
      { OriginalMining.clearGround tile with id := some 85, frame := some (frameX, frameY) } | return state
    terrain := next
  -- The placement runs in UpdateProjectiles, including terrain framing it triggers.
  let playerRandom := state.itemRandom
  let edited := ({ state with itemRandom := state.combat.projectileRandom } : State).adoptTerrain terrain
  let edited := edited.writeSign ⟨placement.x, placement.y⟩ placement.inscription
  return { edited with
    combat := { edited.combat with projectileRandom := edited.itemRandom }
    itemRandom := playerRandom }

private def State.lootContext (state : State) (grid : Grid) : OriginalCombat.Loot.Context :=
  { grid := some grid, needsLife := state.health.statLife < state.health.statLifeMax2
    needsMana := state.health.statMana < state.statManaMax2
    bloodMoon := state.terrain.header.bloodMoon
    luck := state.luck
    -- `NPC.downedBoss1 && Skyblock.noAltars`: false before the Eye; after it the altar scan
    -- (`Skyblock.Calculate`, tile 26) is not tracked.
    eyeDefeatedNoAltars := if state.terrain.header.downedEye then none else some false }

-- BEGIN B-ZONE projectiles-slot (S03)
/-- One `Projectile.Update` of slot `index` (the body of the former per-kind loop): a gravestone lands
through the session (terrain, items, Dig sound); every other kind is `OriginalCombat.advanceProjectile`. -/
private def State.advanceProjectileSlot (initial : State) (index : Nat)
    (viewport : Option OriginalForestGuideArrow.Viewport)
    (audio : Option OriginalTileHitEffects.Context := none) : State := Id.run do
  let mut state := initial
  do
    let slot := state.combat.projectileAt index
    if !slot.active then return state
    let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
    if let .gravestone grave := slot then
      let result := grave.step grid
      if let some placement := result.placement then
        state := state.placeGravestone placement
        -- AI style 17 after TileObject.Place: `PlaySound(0, x * 16, y * 16)` at the AI's own cell
        -- (one below the object's top left). An audible Dig draws variation and pitch from the
        -- projectile phase's stream.
        if let some context := audio then
          let request : OriginalTileHitEffects.SoundRequest :=
            {typeId := 0, x := (placement.x : Int) * 16, y := ((placement.y : Int) + 1) * 16}
          let (played, random) := OriginalTileHitEffects.playDig request context state.combat.projectileRandom
          state := { state with
            projectileSoundRequests := state.projectileSoundRequests.push request
            projectileSounds := state.projectileSounds ++ played.toArray
            combat := { state.combat with projectileRandom := random } }
      let slot := OriginalCombat.Projectile.gravestone result.projectile
      if let some why := result.unsupported then
        state := { state with combat := { state.combat with
          npcBoundary := state.combat.npcBoundary.or (some (index, why)) } }
      let combat := state.combat
      let random := if result.collided then (combat.projectileRandom.next 0 3).2 else combat.projectileRandom
      let combat := combat.setProjectile index slot
      let combat := { combat with projectileRandom := random }
      state := { state with combat }
    else
      let shooter : OriginalPlayerArrows.Shooter :=
        { rangedCrit := (state.equipmentEffects.map (·.rangedCrit)).getD 4, luck := state.luck }
      let (combat, drops) := state.combat.advanceProjectile index grid state.player.body
        state.health.dead (state.lootContext grid) viewport audio shooter
      state := { state with combat, worldItems := drops.foldl OriginalItems.insertDrop state.worldItems }
  return state

/-- `Main.UpdateProjectiles` through ONE `OriginalProjectiles.Pool.update` loop (slots 0..999 ascending,
the `UpdateProjectiles` stream threaded): every slot is `.other` and runs `advanceProjectileSlot` as the
`World.updateOther` callback on the whole session, so each kind sees what earlier slots did (NPCs, items,
terrain, dust, CombatText). A text boundary ends the phase as before (later slots do nothing). An `unowned`
slot or a projectile boundary stops the update (`combat.npcBoundary`). -/
private def State.advanceProjectiles (initial : State)
    (viewport : Option OriginalForestGuideArrow.Viewport)
    (audio : Option OriginalTileHitEffects.Context := none) : State := Id.run do
  let world : OriginalProjectiles.World OriginalCombat.Projectile State := {
    targets := fun _ => #[]
    applyHit := fun state _ => state
    updateOther := fun index _ _ state random dustPool =>
      let state := { state with combat := { state.combat with projectileRandom := random } }
      let state := if state.combat.textBoundary.isSome then state
        else state.advanceProjectileSlot index viewport audio
      let next := state.combat.projectileAt index
      (some (next.info, next), state, state.combat.projectileRandom, dustPool) }
  let grid := Grid.ofTerrain initial.terrain OriginalTileRules.collision
  let phase := initial.combat.projectiles.update { grid } world initial initial.combat.projectileRandom {}
  let state := phase.world
  let unowned := phase.events.findSome? fun
    | .unowned slot type => some s!"projectile slot {slot}: type {type} has no owner (Projectile.Update not modelled)"
    | _ => none
  let boundary := (state.combat.projectileBoundary.or unowned).map (0, ·)
  { state with combat := { state.combat with
      projectileRandom := phase.random
      npcBoundary := state.combat.npcBoundary.or boundary } }
-- END B-ZONE projectiles-slot

/-- `Player.HeldItem.type` as `SceneMetrics.AddPlayerEffects` reads it for the PerspectivePlayer. -/
private def State.heldType (state : State) : Nat :=
  ((state.equipped state.selection.selected).map (·.item.toNat)).getD 0

/-- The shared `_playerSceneMetrics` (`Main.SceneMetrics` without a separate tracking camera). -/
def State.sceneMetrics (state : State) : Option OriginalSceneWaterStyle.Metrics :=
  match state.forest with
  | some forest => forest.sceneMetrics
  | none => state.plainSceneMetrics

/-- `Main.UpdateSceneMetrics` for the local player: the visual area around the camera, the
biome centre at `LocalPlayer.Center`, NPC positions and the PerspectivePlayer's held item.
`SceneMetrics.Scan` returns its cache when both `GameUpdateCount` and the centre repeat, before
`AddPlayerEffects` can look at a newly selected item. Capture and the object-tracking camera are
outside this session. -/
def State.mainUpdateSceneMetrics (state : State) (camera : SceneCamera)
    (gameUpdateCount : Option Nat := none) : State :=
  let area := OriginalSceneWaterStyle.visualScanArea camera.position camera.size
  -- Hooks.OnWorldLoad sets the counter to 65536; DoUpdateInWorld_Inner increments it first.
  let count := gameUpdateCount.getD (65536 + state.updateCount)
  match state.forest with
  | some forest =>
    { state with forest := some (forest.scanScene state.terrain state.player.body state.combat
        state.heldType (some area) camera.skyblockLowTiles (some count)) }
  | none =>
    let header := state.terrain.header
    let world := OriginalSceneWaterStyle.World.ofTerrain state.terrain header.infectedSeed camera.skyblockLowTiles
    let body := state.player.body
    let center := OriginalSceneWaterStyle.playerCenter (body.position.x, body.position.y) body.width body.height
    let metrics := OriginalSceneWaterStyle.scan world state.plainSceneMetrics count center (some area)
    { state with plainSceneMetrics := some metrics }

-- BEGIN B-ZONE lighting-draw (S01)
/-- The renderer-owned `Main` statics one world draw's `LightTiles` calls read (the session owns the
rest, see `State.lightingGlobals`). Every field is the actual value of that draw. -/
structure LightingDraw where
  /-- `Main.tileColor` written by this draw's `ApplyColorOfTheSkiesToTiles` (DoDraw IL_0735):
  `OriginalLightingRuntime.tileColorOfSkies` of `ColorOfTheSkies` right after `SetBackColor`
  (`OriginalForestSkyLighting.Lighting.beforeAtmosphere`, not the post-atmosphere colour). -/
  tileColor : OriginalLightingRuntime.SColor
  /-- `Main.mouseTextColor`. -/
  mouseTextColor : UInt8
  /-- `Main.DiscoR` / `DiscoG` / `DiscoB`. -/
  discoR : Int32
  discoG : Int32
  discoB : Int32
  /-- `Main.GlobalTimeWrappedHourly` as the latest `DoUpdate` wrote it. -/
  globalTimeWrappedHourly : Float32
  /-- `Main.liquidAlpha` (15 entries) as the previous draw's liquid fade left it. -/
  liquidAlpha : Array Float32
  /-- `Main.waterStyle` as the `LightTiles` calls of this draw read it (`UpdateLightDecay`). -/
  waterStyle : Int32
  /-- `(frameX, frameY)` of active tiles whose frames the world file does not store: the
  renderer's framing (`OriginalLightingRuntime.World.frameOf`). -/
  frameOf : Nat → Nat → OriginalWorld.Tile → Option (Int × Int)

/-- The `Globals` of one `LightTiles` call: the draw's renderer statics plus the session's own
producers. Fields without an owner in this session are explicit declared values, each a BOUNDARY:

* `shimmerDarken` 0: the `WorldGen.clearWorld` value. `Statics.Shimmer.update` needs the
  shimmer monolith state, `ZoneShimmer`, the monolith shader and `Player.shimmering`, none of which
  the session tracks; exact while none of them is ever on.
* `jellyfishCageMode` all 0: the `.cctor` array. Its only writer `Main.AnimateTiles_CritterCages`
  has no owner; jellyfish jars (tiles 316–318) light wrongly.
* `echoMonolith` false (`SceneMetrics.EchoMonolith` is not scanned), `canSeeInvisibleBlocks`
  `some false` (the perspective player's Spectre-goggles flags are not tracked), `localPlayerBlind`
  and the decay flags `nightVision`/`blind`/`blackout`/`headcovered` false (`Player.UpdateBuffs` /
  equipment flags not tracked), `airLightDecay`/`solidLightDecay` 1 (`SceneState` constructor
  value; writers not traced).
* `sceneArea` the whole world (`MapHelper.sceneArea` producer not located); it only bounds the
  minimap exports, which have no writer yet.
* `mapEnabled` true (the `Main` static's value; the session has no map option), `isCapturing`
  false (no capture), `netMode` 0 / `myPlayer` 0 (single player). -/
def State.lightingGlobals (state : State) (draw : LightingDraw) (tileColor : OriginalLightingRuntime.SColor) :
    OriginalLightingRuntime.Globals :=
  let header := state.terrain.header
  let timeCaller := state.forest.bind (·.timeCaller)
  { tileColor
    mouseTextColor := draw.mouseTextColor, discoR := draw.discoR, discoG := draw.discoG, discoB := draw.discoB
    demonTorch := state.tileGlows.demonTorch
    globalTimeWrappedHourly := draw.globalTimeWrappedHourly
    timeForVisualEffects := state.timeForVisualEffects
    -- the world clock as the UpdateTime caller leaves it (the header's saved clock without a forest)
    dayTime := state.dayTime
    time := (state.forest.map (·.clock.time)).getD header.time
    raining := (state.forest.map (·.raining header)).getD header.raining
    sundialCooldown := Int32.ofInt ((timeCaller.map (·.phase.sundialCooldown)).getD header.timeFlags.sundialCooldown)
    moondialCooldown := Int32.ofInt ((timeCaller.map (·.phase.moondialCooldown)).getD header.timeFlags.moondialCooldown)
    shimmerDarken := 0
    liquidAlpha := draw.liquidAlpha
    jellyfishCageMode := Array.replicate 75 0
    netMode := 0, myPlayer := 0
    echoMonolith := false
    canSeeInvisibleBlocks := some false
    localPlayerBlind := false
    decay := { waterStyle := draw.waterStyle, nightVision := false, blind := false, blackout := false
               headcovered := false, airLightDecay := 1, solidLightDecay := 1 }
    -- Hooks.OnWorldLoad sets GameUpdateCount to 65536; DoUpdateInWorld_Inner increments it.
    gameUpdateCount := (65536 + state.updateCount).toUInt32
    isCapturing := false
    mapEnabled := true
    sceneArea := ⟨0, 0, Int32.ofNat header.width, Int32.ofNat header.height⟩ }

/-- One actual `Lighting.LightTiles(Main.GetAreaToLight())` call (`OriginalLightingRuntime.lightTilesAt`).
The engine's `_state` 1 requests `Main.UpdateSceneMetrics`, which runs right after the call (phase 1
does nothing else). A `Fault` is fatal: it is recorded in `lightingFault`, never substituted. -/
def State.lightTiles (state : State) (camera : SceneCamera) (draw : LightingDraw)
    (tileColor : OriginalLightingRuntime.SColor) : State :=
  if state.lightingFault.isSome then state else
  let world : OriginalLightingRuntime.World := { terrain := state.terrain, frameOf := draw.frameOf }
  match OriginalLightingRuntime.lightTilesAt state.lighting world (state.lightingGlobals draw tileColor)
      ⟨camera.position, camera.size⟩ with
  | .error f => { state with lightingFault := some s!"Lighting.LightTiles: {reprStr f}" }
  | .ok (lighting, outcome) =>
    let state := { state with lighting }
    if outcome.updateSceneMetrics then state.mainUpdateSceneMetrics camera else state

/-- The lighting calls of one world `Main.DoDraw`. `renderNow` first runs two extra passes
(IL_0390..03bf, writing `Main.renderCount = 99` around them) and is cleared at IL_03cd; they run
before this draw's `SetBackColor` and read the previous draw's `Main.tileColor`. The ordinary pass
follows at IL_0eac with this draw's `tileColor`. No pause, map, inventory or death branch precedes
it. A renderNow scan pass before any world draw wrote `Main.tileColor` would read the menu's value,
which the session does not know: that is recorded as a fatal boundary (the first world draw's
renderNow passes are engine states 0 and 1, which do not read it). -/
def State.drawLighting (state : State) (camera : SceneCamera) (draw : LightingDraw) : State :=
  let state := if state.renderNow then
      let marked (state : State) : State :=
        { state with lighting := { state.lighting with
            statics := { state.lighting.statics with renderCount := 99 } } }
      let pass (state : State) : State :=
        match state.tileColor with
        | some previous => state.lightTiles camera draw previous
        | none =>
          if state.lighting.lighting.engine.state == 2 && state.lightingFault.isNone then
            let why := "BOUNDARY: renderNow scan before any world draw wrote Main.tileColor (menu value unknown)"
            { state with lightingFault := some why }
          -- states 0, 1 and 3 never read Main.tileColor
          else state.lightTiles camera draw draw.tileColor
      marked (pass (pass (marked state)))
    else state
  ({ state with renderNow := false, tileColor := some draw.tileColor } : State).lightTiles camera draw draw.tileColor
-- END B-ZONE lighting-draw

/-- `Main.StartCameraTransitionForPlayerTeleport` as `Player.Spawn` calls it for the local
player with no camera pan. At `Main.BlackFadeDist` or beyond, `BlackFadeCameraTeleport` sets
`renderNow`, clears the light buffers without touching the engine state, and calls
`Main.UpdateSceneMetrics` directly; `Main.screenPosition` is still the camera of the place the
player left. The following `UpdateBiomesIfMovedEnoughForBlackFade` rescans only strictly
beyond that distance, which always repeats the count and centre of the scan above. A nearer
respawn pans the camera instead; that pan is not part of this session. -/
private def State.cameraTeleport (state : State) (origin : OriginalCollision.Vector)
    (camera : SceneCamera) : State :=
  let position := state.player.body.position
  let dx := origin.x - position.x
  let dy := origin.y - position.y
  let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let diagonal := (camera.size.1 * camera.size.1 + camera.size.2 * camera.size.2).toFloat.sqrt.toFloat32
  if distance ≥ diagonal / 2 + 100 then
    -- BEGIN B-ZONE lighting-black-fade (S01)
    -- `Lighting.Clear()` → `_activeEngine.Clear()`: maps and per-frame lights, `_state` kept.
    let lighting := { state.lighting with lighting := OriginalLightingEngine.clear state.lighting.lighting }
    -- END B-ZONE lighting-black-fade
    ({ state with renderNow := true, blackFade := true, lighting } : State).mainUpdateSceneMetrics camera
  else state

private def State.revive (state : State) (camera : Option SceneCamera := none) : State :=
  let spawn := worldSpawn state.terrain
  -- Player.Spawn resets RabbitOrderFrameHelper and chooses the idle frame and delay.
  let (_, random) := state.itemRandom.next 1 4
  let (_, random) := random.next 180 3600
  let revived : State := { state with
    -- The update that respawns began like every other (IL_0054..0086): maxRunSpeed 3 and
    -- accRunSpeed = maxRunSpeed, before the dead branch.
    player := { body := spawnBody spawn, direction := state.player.direction, accRunSpeed := 3 }
    playerLiquid := {}
    corpse := none
    corpseParts := none
    itemUse := {}
    -- Player.Spawn does not touch releaseUseItem: a client keeps what the last ItemCheck left.
    combat := { state.combat.clearStabs with
      use := if state.selectionLive then { releaseUseItem := state.combat.use.releaseUseItem } else {}
      weapon := none }
    throwAnimation := false
    throwDelayUse := false
    itemRandom := random
    health := state.health.spawn false }
  match camera with
  | some camera => revived.cameraTeleport state.player.body.position camera
  | none => revived

/-- `SelectedItemState` inputs from the session: IsAir of inventory 0..57 and of the cursor
mirror, plus the item-use fields the state machine reads and writes. Reuse delay, channelling
and pending reuse belong to items this session does not run. -/
private def State.selectionInput (state : State) (controlUseItem : Bool) :
    OriginalSelectedItemState.Input :=
  let air (slot : Option OriginalItems.Stack) : Bool := slot.all fun stack => stack.count == 0 || stack.item == 0
  let carried := state.carriedItems
  { context := {
      itemAnimation := state.itemUse.itemAnimation, itemTime := state.itemUse.itemTime
      itemTimeMax := state.itemUse.itemTimeMax, controlUseItem
      releaseUseItem := state.combat.use.releaseUseItem, blockMouse := state.blockMouse
      lastItemUseAttemptSuccess := state.lastItemUseAttemptSuccess }
    inventoryAir := ((Array.range 58).map fun index => air carried[index]?.join).push (air state.mouseItem)
    mouseAir := air state.mouseItem }

private def State.selectionBoundary (state : State) (why : String) : State :=
  { state with combat := { state.combat with
      npcBoundary := state.combat.npcBoundary.or (some (0, s!"selected item state: {why}")) } }

/-- The local control phase before `dropItemCheck`: `TriggersSet.CopyInto`'s MouseLeft branch
(IL_01c9..01ee), then this update's `SelectedItemState.Select` calls. Returns `controlUseItem`
as that branch leaves it. -/
private def State.clientSelects (state : State) (client : ClientInput) : State × Bool := Id.run do
  let (blockMouse, control) :=
    OriginalSelectedItemState.mouseControls state.blockMouse client.mouseLeft client.mouseInterface
  let mut state := { state with blockMouse }
  for slot in client.selects do
    match OriginalSelectedItemState.select state.selection (state.selectionInput control) slot with
    | .ok out =>
      state := { state with
        selection := out.state, selectionMenuTick := state.selectionMenuTick || out.menuTick }
    | .error why => state := state.selectionBoundary why
  return (state, control)

/-- `SelectedItemState.Update` at Player.Update IL_1678, after `dropItemCheck` refreshed the
cursor mirror and before ResetEffects' readers. `delayUseItem` has already cleared
`controlUseItem` in CopyInto (IL_022b..0244); IL_1c0c arms it again while the cursor is over an
interface. Returns the `controlUseItem` that `ItemCheck` will see. -/
private def State.clientSelectionUpdate (state : State) (client : ClientInput) (control : Bool) :
    State × Bool :=
  let control := control && !state.throwDelayUse
  let delayed (state : State) : State :=
    { state with throwDelayUse := state.throwDelayUse || client.mouseInterface }
  match OriginalSelectedItemState.update state.selection (state.selectionInput control) with
  | .error why => (delayed (state.selectionBoundary why), control)
  | .ok out =>
    let context := out.context
    let itemUse := if out.changed then
        { state.itemUse with itemTime := context.itemTime.toNat, itemTimeMax := context.itemTimeMax.toNat }
      else state.itemUse
    let use := { state.combat.use with releaseUseItem := context.releaseUseItem }
    let use := if out.changed then
        { use with itemTime := context.itemTime.toNat, itemTimeMax := context.itemTimeMax.toNat }
      else use
    (delayed { state with
      selection := out.state, itemUse, blockMouse := context.blockMouse
      lastItemUseAttemptSuccess := context.lastItemUseAttemptSuccess
      combat := { state.combat with use } }, context.controlUseItem)

private def State.throwEnvironment (state : State) (grid : Grid) : OriginalItemThrow.Env := {
  grid
  body := state.player.body
  direction := state.player.direction
  world := state.worldItems
  rng := state.itemRandom }

private def State.applyDroppedItems (state : State) (drop : OriginalItemThrow.Result) : State :=
  { state.setCarriedItems drop.state.inventory with
    mouseItem := drop.state.mouse
    itemRandom := drop.rng
    worldItems := drop.world
    candleInteraction := if drop.consumedRightClick then
      { state.candleInteraction with releaseUseTile := false } else state.candleInteraction }

/-- Explicitly return the cursor item when closing an inventory outside the frame loop. -/
def State.returnMouseItem (state : State) : State :=
  if state.mouseItem.isNone then state
  else
    let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
    let drop := OriginalItemThrow.dropItemCheck {} (state.throwEnvironment grid) {
      inventory := state.carriedItems
      mouse := state.mouseItem }
    state.applyDroppedItems drop

private def State.checkThrow (state : State) (grid : Grid) (input : ThrowInput) (held : Bool) : State :=
  let canDrop := state.itemUse.itemAnimation == 0 && state.itemUse.itemTime == 0
  let drop := OriginalItemThrow.step canDrop input.controls (state.throwEnvironment grid) {
      inventory := state.carriedItems
      mouse := state.mouseItem
      selected := if state.selectionLive then state.selection.selected
        else if state.throwCursorSelected then 58 else input.priorSelected
      releaseThrow := state.throwRelease
      noThrow := state.throwNoThrow }
  let itemUse := if drop.startedAnimation then
      { state.itemUse with itemAnimation := 10, itemAnimationMax := 10 }
    else state.itemUse
  { state.applyDroppedItems drop with
    itemUse
    throwRelease := drop.state.releaseThrow
    throwNoThrow := drop.state.noThrow
    throwCursorSelected := if canDrop && !drop.startedAnimation then drop.state.mouse.isSome
      else state.throwCursorSelected
    throwAnimation := drop.startedAnimation || state.throwAnimation
    throwStarted := drop.startedAnimation
    throwDelayUse := drop.blockedItemUse || (held && state.throwDelayUse) }

-- BEGIN B-ZONE weapons-numbers (S04)
/-- Buffs whose `UpdateBuffs` changes a `Player` field `GetWeaponDamage/Crit/Knockback` read (Archery 16,
Tipsy 25, Well Fed 26/206/207, Clairvoyance 29, Magic Power 7, Titan 114, Rage 115, Wrath 117). Declared list,
not read from the CIL: the session has no producer for these fields, so a live one is a boundary. -/
def weaponStatBuffs : Array Nat := #[7, 16, 25, 26, 29, 114, 115, 117, 206, 207]

/-- `Player.GetWeaponDamage/Crit/Knockback` of the held stack through `OriginalPlayerActions.weaponNumbers`.

* The item instance is `SetDefaults(type)` + `Item.Prefix(prefix byte)` (`OriginalPlayerActions.weaponItem`) with the
  prefix byte the inventory holds (from the player file). A valid prefix draws nothing. A prefix `Prefix` rejects
  re-rolls natively when the player file is loaded (`Player.LoadPlayer`, on the menu thread's global `Main.rand`, a
  stream whose state at the character list the session does not know): that is refused as a boundary here, never
  re-rolled on a session stream.
* `Stats` = `OriginalWeaponStats.Stats.ofEffects` of this update's `UpdateEquips`; buff / glove / stealth fields keep
  their `ResetEffects` values, so a live `weaponStatBuffs` buff is a boundary.
* Root note: magic / ranged crit from equipment (effects-minion-crit) is a coverage hole of the equipment leaf; this
  path is wired for the melee starter weapons and is not evidence for magic/ranged crit. -/
def State.heldWeaponNumbers (state : State) (stack : OriginalItems.Stack) :
    Except String OriginalPlayerActions.WeaponNumbers := do
  let some (item, rng) := OriginalPlayerActions.weaponItem stack.item stack.prefixId.toNat state.itemRandom
    | throw s!"item {stack.item} prefix {stack.prefixId}: no SetDefaults/Prefix result"
  if rng.state != state.itemRandom.state then
    throw s!"item {stack.item} prefix {stack.prefixId}: rejected at load (re-roll on the menu-thread Main.rand is not modelled)"
  let live (id : Nat) := (state.health.buffType.zip state.health.buffTime).any fun (b, t) => b == id && t > 0
  if let some id := weaponStatBuffs.find? live then
    throw s!"buff {id} changes weapon stats; its effect is not applied"
  let stats := (state.equipmentEffects.map OriginalWeaponStats.Stats.ofEffects).getD {}
  return OriginalPlayerActions.weaponNumbers stats item
-- END B-ZONE weapons-numbers

private def State.updateSelectedItem (state : State) (grid : Grid) (selected : Nat)
    (held : Bool) (target : Option Cell) (bucketSplashReady : Bool)
    (mouse : Option (Float32 × Float32)) (viewport : Option OriginalForestGuideArrow.Viewport) : State :=
  let mayUse := held && !state.throwDelayUse
  -- ItemCheck's start gate (IL_0451..0488) and ItemCheck_AutoReuseLogic (IL_0027) both stop at
  -- HasBufferedChange, like Cursed: the running animation ends and nothing new starts.
  let noStart := state.health.noItems || (state.selectionLive && state.selection.hasBuffered)
  let stack := state.equipped selected
  match stack.bind (fun item => OriginalStarterMelee.weaponOf? item.item.toNat) with
  | some weapon =>
    -- BEGIN B-ZONE weapons-melee (S04): damage / crit / knockback from weaponNumbers. The leaf applies
    -- `meleeDamage` itself (`(int)(damage * meleeDamage + 5e-6f)`), so it gets the final damage with
    -- `meleeDamage` 1 (the same expression), the final knockback with multiplier 1 and the crit chance.
    let (state, weapon, meleeCrit) := match stack.map state.heldWeaponNumbers with
      | some (.ok n) => (state, { weapon with damage := n.damage.toNat, knockBack := n.knockback }, n.crit)
      | some (.error why) => ({ state with combat := { state.combat with
          npcBoundary := state.combat.npcBoundary.or (some (0, s!"weapon numbers: {why}")) } }, weapon, 4)
      | none => (state, weapon, 4)
    -- END B-ZONE weapons-melee
    let body := state.player.body
    let position := match mouse with
      | some (x, y) => ⟨x, y⟩
      | none => (OriginalCombat.owner body).center + ⟨single state.player.direction, 0⟩
    let audio := state.tileHitEffects.map (·.context)
    -- ItemCheck_StartActualUse plays the item's UseSound before anything of the use itself draws:
    -- when this update starts a use, the swing's own draws come first. The start is decided on
    -- the stream as it is now; nothing of that probe is kept.
    let probe := state.combat.useWeapon weapon body state.player.direction
      mayUse position grid state.itemRandom (state.lootContext grid) viewport (noItems := noStart) (meleeCrit := meleeCrit)
    let state := state.emitToolSwing (probe.1.use.itemAnimation > state.combat.use.itemAnimation)
    let (combat, drops, itemRandom) := state.combat.useWeapon weapon body state.player.direction
      mayUse position grid state.itemRandom (state.lootContext grid) viewport (noItems := noStart) (audio := audio)
      (meleeCrit := meleeCrit)
    let use := combat.use
    { state with
      combat, itemRandom, wallUseHeld := mayUse && target.isSome
      worldItems := drops.foldl OriginalItems.insertDrop state.worldItems
      player := { state.player with direction := use.direction }
      itemUse := { itemAnimation := use.itemAnimation, itemAnimationMax := use.itemAnimationMax
                   itemTime := use.itemTime, itemTimeMax := use.itemTimeMax } }
  | none =>
    -- BEGIN B-ZONE projectiles-release (S03): the release latch as the previous ItemCheck left it
    let priorRelease := state.combat.use.releaseUseItem
    -- END B-ZONE projectiles-release
    let use := { state.combat.use with releaseUseItem := !mayUse, itemAnimation := 0 }
    let state := { state with combat := { state.combat with use, weapon := none } }
    -- Tool helpers renew at animation 1. Block that renewal while finishing the old swing.
    let canContinue := !noStart || state.itemUse.itemAnimation > 1
    let before := state.itemUse
    let used := state.useSelected selected (mayUse && target.isSome && canContinue) target bucketSplashReady mouse
      priorRelease viewport
    let timing := if noStart && before.itemAnimation == 1 then
        { used.itemUse with itemAnimationMax := before.itemAnimationMax }
      else used.itemUse
    { used with itemUse := timing, wallUseHeld := mayUse && target.isSome }

/-- `ItemCheck` IL_0833..0852: `AnimatePlayerAndGetItemFrame` places `Player.itemLocation`, then
`ItemCheck_EmitHeldItemLight(inventory[selectedItem])` runs after this update's start and
`itemAnimation` decrement and before the owner-only use code. Torches and the water and peace
candles draw from the UpdatePlayers stream on every update, also on a host without a window,
where `Dust.NewDust` returns its sentinel. Mounts, pulleys, stealth, reversed gravity and the
shimmer, temple and underground-desert torch styles are outside this session. -/
private def State.emitHeldItemLight (state : State) (selected : Nat)
    (viewport : Option OriginalForestGuideArrow.Viewport)
    (frame : Nat → OriginalHeldItemLight.ItemFrame) : State :=
  let item := ((state.equipped selected).map (·.item.toNat)).getD 0
  match OriginalHeldItemLight.coverage item with
  | .untouched => state
  | _ =>
  let body := state.player.body
  let header := state.terrain.header
  let zone := fun name => (state.forest.bind (·.playerFlags.lookup name)).getD false
  let holder : OriginalHeldItemLight.Holder := {
    position := body.position, width := body.width, height := body.height, velocity := body.velocity
    direction := state.player.direction, gfxOffY := body.gfxOffY, wet := state.playerLiquid.wet
    itemAnimation := state.itemUse.itemAnimation, itemLocation := state.itemLocation
    spelunkerTimer := state.spelunkerTimer }
  let ambient : OriginalHeldItemLight.Ambient := {
    worldSurface := header.surface, underworldLayer := (header.height : Int) - 200
    remixWorld := header.remixWorld }
  let biome : OriginalHeldItemLight.Biome := {
    usingBiomeTorches := state.unlockedBiomeTorches
    dungeon := zone "ZoneDungeon", glowshroom := zone "ZoneGlowshroom", hallow := zone "ZoneHallow"
    corrupt := zone "ZoneCorrupt", crimson := zone "ZoneCrimson", snow := zone "ZoneSnow"
    jungle := zone "ZoneJungle", desert := zone "ZoneDesert" }
  let pool := { state.combat.dustPool viewport with enabled := viewport.isSome }
  let out := OriginalHeldItemLight.update viewport.isNone item (frame item) holder
    state.itemUse.itemAnimationMax ambient biome pool state.itemRandom
  { state with
    itemRandom := out.random, itemLocation := out.itemLocation, spelunkerTimer := out.spelunkerTimer
    heldItemLights := state.heldItemLights ++ out.light.toArray
    combat := { state.combat with guideDust := state.combat.guideDust ++ out.dust } }

private def State.lookForCandleInteractions (state : State) (client : ClientInput)
    (mouse : Option (Float32 × Float32)) (crowdControlled : Bool) : State := Id.run do
  let point : OriginalCandleInteraction.Point := match mouse with
    | some (x,y) =>
      ⟨max 5 (min ((state.terrain.header.width : Int) - 5) (x / 16).toInt32.toInt),
       max 5 (min ((state.terrain.header.height : Int) - 5) (y / 16).toInt32.toInt)⟩
    | none => ⟨-100000, -100000⟩
  let tileAt : OriginalCandleInteraction.Point → Option OriginalCandleInteraction.Tile := fun p => do
    if p.x < 0 || p.y < 0 then none else do
      let tile ← state.terrain.tile? p.x.toNat p.y.toNat
      some { active := tile.id.isSome
             type := (tile.id.getD 0).toNat
             frameX := (tile.frame.getD (0,0)).1 }
  let controlUseTile := client.mouseRight && !client.mouseInterface && !state.blockMouse &&
    !client.tileUsageLocked && !client.buildingMode && !crowdControlled
  let output := OriginalCandleInteraction.look
    { state.candleInteraction with noThrow := state.throwNoThrow } {
      body := state.player.body, tileTarget := point, tileAt, controlUseTile
      bestiaryOpen := client.bestiaryReferenceEqual
      currentNPCShowingChatBubble := client.currentNPCShowingChatBubble
      talkNPC := client.talkNPC, npcChatRelease := client.npcChatRelease
      hasInteractableObjectThatIsNotATile := client.nonTileInteraction }
  let state := { state with candleInteraction := output.state, throwNoThrow := output.state.noThrow.toNat }
  let some edit := output.edit | return state
  let some tile := state.terrain.tile? edit.point.x.toNat edit.point.y.toNat | return state
  let terrain := state.trackTerrain
  let terrain := (terrain.replaceTile edit.point.x.toNat edit.point.y.toNat
    { tile with frame := some (edit.after.frameX, (tile.frame.getD (0,0)).2) }).getD terrain
  return state.adoptTerrain terrain

private def State.finishCandleInteractions (state : State) (client : Option ClientInput) : State :=
  match client with
  | none => state
  | some input => { state with candleInteraction :=
      OriginalCandleInteraction.endUpdate state.candleInteraction input.mouseInterface }

private def State.acceptTileHitEffects (state : State) : State :=
  match state.tileHitEffects with
  | none => state
  | some effects =>
    let boundary := effects.boundary.map fun b =>
      (0, s!"tile effects at ({b.x},{b.y}): {b.reason}")
    { state with combat := { state.combat with
        guideDust := state.combat.guideDust ++ effects.particles
        npcBoundary := state.combat.npcBoundary.or boundary } }

private def State.resolveCandlePlacementSound (state : State) : State :=
  match state.candlePlacementSound, state.tileHitEffects with
  | some sound, some effects =>
    let request : OriginalTileHitEffects.SoundRequest := {
      x := sound.x, y := sound.y, style := sound.style.toNat }
    let (played, itemRandom) := OriginalTileHitEffects.playDig request effects.context state.itemRandom
    { state with itemRandom, tileHitEffects := some { effects with
        soundRequests := effects.soundRequests.push request
        resolvedSounds := effects.resolvedSounds ++ played.toArray } }
  | _, _ => state

/-- OwnerOnlyCode's placement preview uses the raw tile target, including over UI.
It only writes Enabled; an earlier hover's explicit ID/Text/Push must survive. -/
private def State.enableCandlePlacementIcon (state : State) (selected : Nat)
    (mouse : Option (Float32 × Float32)) : State :=
  let target := mouse.map fun (x,y) =>
    ({ x := (max 5 (min ((state.terrain.header.width : Int) - 5) (x / 16).toInt32.toInt)).toNat
       y := (max 5 (min ((state.terrain.header.height : Int) - 5) (y / 16).toInt32.toInt)).toNat } : Cell)
  let item := ((state.equipped selected).map (·.item)).getD 0
  if OriginalCandlePlacement.enableCursorIcon { body := state.player.body, item, target } then
    { state with candleInteraction := { state.candleInteraction with cursorItemIconEnabled := true } }
  else state

/-- `Player.Update` IL_188d..1c06, the fall bookkeeping of the local player: after `dropItemCheck`
and before `UpdateImmunity` / `ResetEffects`. `before` is the health record as the update found
it; the session has already applied those two steps to `state.health`, so an accepted fall
`Hurt` is made on `before` and its four fields are written back as the later steps leave them.
`extraFall`, `noFallDmg`, `statDefense` and `statLifeMax2` are still the previous update's. -/
private def State.fallBlock (state : State) (grid : Grid) (before : Health)
    (client : Option ClientInput) (viewport : Option OriginalForestGuideArrow.Viewport) : State := Id.run do
  let body := state.player.body
  let header := state.terrain.header
  let effects := state.equipmentEffects
  let fall := OriginalPlayerFall.step {
    position := body.position, velocity := body.velocity, width := body.width, height := body.height
    fallStart := body.fallStart, jump := body.jump, wet := state.playerLiquid.wet
    extraFall := (effects.map (·.extraFall)).getD 0, noFallDmg := effects.any (·.noFallDmg)
    gravityFactor := OriginalPlayerFall.gravityFactor body.position.y header.width header.surface header.remixWorld }
    fun x y => if x < 0 || y < 0 then none else state.terrain.tile? x.toNat y.toNat
  let state := { state with player := { state.player with body := { body with fallStart := fall.fallStart } } }
  let some damage := fall.damage | return state
  -- IL_1b2b clears `immune`; Hurt(ByOther(0), damage, 0, General): no knockback.
  let rect : OriginalForestChatLayout.Rect :=
    ⟨body.position.x.toInt32.toInt,body.position.y.toInt32.toInt,body.width,body.height⟩
  let head := OriginalPlayerHurtText.ordinaryPrefix
    {damage, defense := state.statDefense, life := before.statLife, lifeMax := before.statLifeMax2}
    (state.combat.textChannel rect) state.itemRandom
  let state := { state with
    itemRandom := head.text.random
    combat := { state.combat with
      texts := (head.text.text.map (·.state)).getD state.combat.texts
      textBoundary := state.combat.textBoundary.or head.boundary
      npcBoundary := state.combat.npcBoundary.or (head.boundary.map fun why => (0,why)) } }
  if head.boundary.isSome then return state
  let tail := OriginalPlayerHurt.finish {
    position := body.position, width := body.width, height := body.height
    damage := head.damage, rawLife := head.rawLifeAfter, lifeMax2 := before.statLifeMax2
    hitDirection := 0 } (state.hurtClient client).sound (state.combat.dustPool viewport) state.itemRandom
  -- What UpdateImmunity (IL_1f08) makes of the immunity Hurt has just set.
  let later := ({ before with immune := head.immune, immuneTime := head.immuneTime } : Health).resetEffects
  let health := { state.health with
    statLife := tail.life, lifeRegenTime := 0
    immune := later.immune, immuneTime := later.immuneTime, immuneNoBlink := later.immuneNoBlink }
  let health := if tail.record.lethal then { health with dead := true, respawnTimer := 600 } else health
  let state := { state with
    health, itemRandom := tail.random
    hurts := state.hurts.push tail.record
    hurtSound := (tail.sound.map fun (side : OriginalPlayerHurt.SoundClient) => side.state).getD state.hurtSound
    healthEvents := state.healthEvents ++ #[.fallHit head.damage] ++ (if tail.record.lethal then #[.died .fall] else #[])
    combat := { state.combat with
      guideDust := state.combat.guideDust ++ tail.particles
      textBoundary := state.combat.textBoundary.or tail.boundary
      npcBoundary := state.combat.npcBoundary.or (tail.boundary.map fun why => (0,why)) } }
  if tail.boundary.isSome then return state
  return if tail.record.lethal then state.registerDeath grid none viewport (some 0) client else state

-- BEGIN B-ZONE actions-quick (S02)
/-- `LegacySoundPlayer.PlaySound(2, x, y, style)` for the potion use sounds (Item2 / Item3): the same gates
and volume/pan as the verified swing (`OriginalItemUseSound.playSwing`), no voice choice (that is style 1
only, IL_04ab), then the ordinary pitch sample `Next(-6, 7) * 0.01` (IL_0738..074b). Styles with their own
volume / pitch branches are refused by the caller. -/
private def playPotionSound (style : Nat) (x y : Int) (context : OriginalTileHitEffects.Context)
    (random : UnifiedRandom) : Option OriginalTileHitEffects.PlayedSound × UnifiedRandom := Id.run do
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then
    return (none, random)
  let global := x == -1 || y == -1
  if !global && (context.generating || context.netMode == 2) then return (none, random)
  let offset : OriginalCollision.Vector := ⟨single x - context.cameraCenter.x, single y - context.cameraCenter.y⟩
  let distance := Float32.sqrt (offset.x * offset.x + offset.y * offset.y)
  if !global && !(distance < context.attenuationDistance) then return (none, random)
  let clamp (lo hi v : Float32) : Float32 := if v < lo then lo else if v > hi then hi else v
  let pan := if global then 0 else clamp (-1) 1 (offset.x / (context.maxWorldViewWidth * 0.5))
  let volume := if global then 1 else 1 - distance / context.attenuationDistance
  let volume := min 1 (clamp 0 1 volume * context.soundVolume)
  if !(volume > 0) then return (none, random)
  let (pitch, random) := random.next (-6) 7
  return (some { request := { typeId := 2, x, y, style }, variation := 0, volume, pan
                 pitch := clamp (-1) 1 (single pitch * 0.01) }, random)

/-- `Player.Update` IL_0d87..0dd3 through `OriginalPlayerActions.quickKeys`: the QuickHeal gate, then the
QuickMana gate. The two gates are independent (each reads and writes only its own control/release pair), so
the session runs them as two `quickKeys` calls, one with only that key's control, and takes each gate's own
release from its call. Each `QuickHeal` / `QuickMana` call's first effect is `PlaySound(item.UseSound,
position)`; with window audio that call draws its pitch from `Main.rand` BEFORE the call's own draws, so a
call that turns out to play a sound is evaluated again from the stream after the pitch sample (the leaf
draws nothing before its sound). It also takes one `SoundStyle._random` sample (the LegacySoundStyle
overload), like `emitToolSwing`.

The `HealingPotions.Player` is assembled from the session: life/mana/buffs (`health`, flags as the previous
`UpdateBuffs` left them), `statManaMax2`, the delays, inventory 0–57 plus the cursor as slot 58, the Void Vault
as `bank4`, mana heat and the body. BOUNDARY (declared): `pStone` false (Philosopher's Stone / Charm of Myths
are not modelled by the equipment owner), `pending` false (no pending inventory actions in the session), the
crowd-control flags from the live buffs 47/353, 149, 156 and `manaSick` from 94 (a buff that ran out in the
previous update is not seen); an `achievement` effect (Bottled Water at breath 0) stops the update. -/
private def State.quickKeys (state : State) (client : Option ClientInput) : State := Id.run do
  let controlHeal := (client.map (·.controlQuickHeal)).getD false
  let controlMana := (client.map (·.controlQuickMana)).getD false
  let stop (state : State) (why : String) : State := { state with combat := { state.combat with
      npcBoundary := state.combat.npcBoundary.or (some (0, s!"QuickHeal/QuickMana: {why}")) } }
  let slotOf (s : Option OriginalItems.Stack) : OriginalHealingPotions.Slot :=
    match s with
    | some st => { type := st.item.toNat, stack := st.count }
    | none => {}
  let live (id : Nat) := (state.health.buffType.zip state.health.buffTime).any fun (b, t) => b == id && t > 0
  let body := state.player.body
  let player : OriginalHealingPotions.Player := {
    health := state.health, statManaMax2 := state.statManaMax2
    potionDelay := state.potionDelay, manaPotionDelay := state.manaPotionDelay
    pStone := false, pending := false
    frozen := live 47 || live 353, webbed := live 149, stoned := live 156, manaSick := live 94
    inventory := (state.carriedItems.map slotOf).push (slotOf state.mouseItem)
    bank4 := state.banks.voidVault.map slotOf
    manaHeat := state.manaHeat
    position := body.position, width := body.width, height := body.height }
  let audio := state.tileHitEffects.map (·.context) |>.filter (!·.dedicatedServer)
  -- one gate; `heal` selects which key's control this call carries
  let gate (heal : Bool) (player : OriginalHealingPotions.Player) (rng : OriginalPlayerLiquidHealth.Rng) :
      OriginalPlayerActions.QuickResult :=
    OriginalPlayerActions.quickKeys player rng
      { controlQuickHeal := heal && controlHeal, releaseQuickHeal := state.releaseQuickHeal
        controlQuickMana := !heal && controlMana, releaseQuickMana := state.releaseQuickMana }
  let mut state := state
  let mut player := player
  let mut rng := state.playerTextRng
  let mut releaseHeal := state.releaseQuickHeal
  let mut releaseMana := state.releaseQuickMana
  let mut called := false
  for heal in [true, false] do
    let first := gate heal player rng
    if !first.effects.isEmpty then called := true
    let mut result := first
    for effect in first.effects do
      match effect with
      | .sound soundId style position volumeScale pitchOffset =>
        if soundId != 2 || (style != 2 && style != 3) || volumeScale != 1 || pitchOffset != 0 then
          return stop state s!"PlaySound({soundId}, style {style}, volume {volumeScale}, pitch {pitchOffset}) not resolved"
        let x := position.x.toInt32.toInt
        let y := position.y.toInt32.toInt
        state := { state with quickSoundRequests := state.quickSoundRequests.push { typeId := 2, x, y, style } }
        if let some context := audio then
          state := { state with pendingStyleSamples := state.pendingStyleSamples + 1 }
          let (played, stream) := playPotionSound style x y context rng.stream
          if let some played := played then
            state := { state with quickSounds := state.quickSounds.push played }
            result := gate heal player { rng with stream, draws := rng.draws + 1 }
      | .achievement event =>
        return stop state s!"achievement event {event} not wired"
      | _ => pure ()
    player := result.player
    rng := result.rng
    if heal then releaseHeal := result.keys.releaseQuickHeal else releaseMana := result.keys.releaseQuickMana
  -- no QuickHeal/QuickMana got past its gates: only the latches change
  if !called then return { state with releaseQuickHeal := releaseHeal, releaseQuickMana := releaseMana }
  -- write the potion player back: life/mana/buffs, delays, heat, the consumed slot
  let back (old : Option OriginalItems.Stack) (slot : OriginalHealingPotions.Slot) : Option OriginalItems.Stack :=
    if slotOf old == slot then old
    else if slot.type == 0 || slot.stack ≤ 0 then none
    else old.map fun st => { st with count := slot.stack.toNat }
  let carried := state.carriedItems.mapIdx fun i old => back old (player.inventory[i]?.getD {})
  let mouseItem := back state.mouseItem (player.inventory[carried.size]?.getD {})
  let voidVault := state.banks.voidVault.mapIdx fun i old => back old (player.bank4[i]?.getD {})
  let written := { state.setCarriedItems carried with
    health := player.health, potionDelay := player.potionDelay, manaPotionDelay := player.manaPotionDelay
    manaHeat := player.manaHeat, mouseItem, banks := { state.banks with voidVault }
    releaseQuickHeal := releaseHeal, releaseQuickMana := releaseMana }
  return written.acceptTextRng rng
-- END B-ZONE actions-quick

private def State.updatePlayer (state : State) (grid : Grid) (input : Controls) (selected : Nat)
    (held : Bool) (target : Option Cell) (bucketSplashReady : Bool) (mouse : Option (Float32 × Float32))
    (throwInput : Option ThrowInput) (viewport : Option OriginalForestGuideArrow.Viewport)
    (client : Option ClientInput := none) : State := Id.run do
  let state := if client.isSome then
      { state with candleInteraction :=
        OriginalCandleInteraction.beginUpdate (OriginalCandleInteraction.clearMouseOver state.candleInteraction) }
    else state
  let visual := state.manaHeat.visuals state.itemRandom
    { state.combat.dustPool viewport with enabled := viewport.isSome }
    state.player.body.position state.player.body.width state.player.body.height
  let state := { state with
    itemRandom := visual.random
    combat := { state.combat with manaDust := state.combat.manaDust ++ visual.particles } }
  -- BEGIN B-ZONE actions-potion-delay (S02)
  -- Player.Update IL_0a7a..0aa8, before the dead branch: the potion delays count down.
  let state := { state with
    potionDelay := if state.potionDelay > 0 then state.potionDelay - 1 else state.potionDelay
    manaPotionDelay := if state.manaPotionDelay > 0 then state.manaPotionDelay - 1 else state.manaPotionDelay }
  -- END B-ZONE actions-potion-delay
  if state.health.dead then
    let (health, event) := state.health.deadFrame
    let state := { state with
      health, healthEvents := event.toArray
      corpseParts := state.corpseParts.map (·.updateDead)
      manaHeat := state.manaHeat.updateEffects 0 }
    -- The local control block (IL_0bb0..1882) is not reached: no CopyInto, Select or Update.
    return if event.isSome then state.revive (client.bind (·.camera)) else state
  -- BEGIN B-ZONE actions-quick-keys (S02)
  -- IL_0d87..0dd3: after the (unmodelled) isOperatingAnotherEntity control clear, before
  -- controlCreativeMenu, Select and dropItemCheck; flags are still the previous UpdateBuffs'.
  let quick := state.quickKeys client
  if quick.combat.textBoundary.isSome ||
      (quick.combat.npcBoundary.isSome && state.combat.npcBoundary.isNone) then return quick
  let state := quick
  -- END B-ZONE actions-quick-keys
  -- UpdateBuffs applies the control flags even on the tick it decrements a buff to zero.
  let healthBeforeReset := state.health
  let state := { state with health := state.health.resetEffects }
  -- UpdateBiomes uses the preceding scene snapshot. Do not OR the newly selected
  -- item here: held candles enter that snapshot only when SceneMetrics scans.
  let zones := (state.forest.map (·.candleZones)).getD {}
  let state := match OriginalCandleBuffs.refresh
      { types := state.health.buffType, times := state.health.buffTime } { zones } with
    | .ok slots => { state with health := { state.health with buffType := slots.types, buffTime := slots.times } }
    | .error why => { state with combat := { state.combat with
        npcBoundary := state.combat.npcBoundary.or (some (0, s!"candle Buff refresh: {why}")) } }
  let activeBuffs := state.health.buffType.zip state.health.buffTime
  let manaHeatTime := ((activeBuffs.find? fun (id,time) => id == 396 && time > 0).map (·.2)).getD 0
  let state := { state with manaHeat := state.manaHeat.updateEffects manaHeatTime }
  let shimmering := activeBuffs.any fun (id, time) => id == 353 && time > 0
  let crowdControlled := activeBuffs.any fun (id, time) =>
    time > 0 && (id == 47 || id == 149 || id == 156 || id == 353)
  let (state, held) := match client with
    | some client => state.clientSelects client
    | none => (state, held)
  let state := match throwInput with
    | some throwInput => state.checkThrow grid throwInput held
    | none => state
  let (state, held) := match client with
    | some client => state.clientSelectionUpdate client held
    | none => (state, held)
  -- IL_188d: landing, fall damage and `fallStart`, after dropItemCheck (IL_166d) and the
  -- selection update (IL_1678), before everything UpdateImmunity and ResetEffects start.
  let state := state.fallBlock grid healthBeforeReset client viewport
  if state.combat.textBoundary.isSome then return state
  -- The AFK test (IL_202f..20a9) follows ResetEffects: Select has zeroed the counter (IL_0089),
  -- and a drop's SetItemAnimation(10) inside dropItemCheck already counts as item use.
  let forest := state.forest.map fun forest =>
    let forest := if client.any (!·.selects.isEmpty) then { forest with afkCounter := 0 } else forest
    forest.trackAfk state.player.body false state.itemUse.itemAnimation input
  let state := { state with forest }
  let selected := if state.selectionLive then state.selection.selected
    else if state.throwStarted then (throwInput.map (·.priorSelected)).getD selected else selected
  -- `UpdateEquips` and `UpdateArmorSets` (Player.Update IL_248f..264b) run between UpdateBuffs and
  -- UpdateLifeRegen: the equipment's `lifeRegen` and `statLifeMax2` are in place when life regenerates.
  -- BEGIN B-ZONE actions-potion-sickness (S02)
  -- UpdateBuffs IL_3f89..3fa3: a live Potion Sickness (21) writes its decremented time into potionDelay.
  let state := match (state.health.buffType.zip state.health.buffTime).find? (fun (b, t) => b == 21 && t > 0) with
    | some (_, t) => { state with potionDelay := t - 1 }
    | none => state
  -- END B-ZONE actions-potion-sickness
  let buffed := state.health.updateBuffs
  let effects := OriginalPlayerEquipment.update state.equipment (state.equipmentContext buffed)
  let state := { state with equipmentEffects := some effects }
  -- UpdateLuck (IL_24cb) follows UpdateEquips and UpdateArmorSets; the spawner reads its result.
  let (state, luckBoundary) := state.updateLuck selected
  let state := { state with combat := { state.combat with
    npcBoundary := state.combat.npcBoundary.or (luckBoundary.map (0, ·)) } }
  let buffed := { buffed with lifeRegen := buffed.lifeRegen + effects.lifeRegen, statLifeMax2 := effects.statLifeMax2 }
  let (health, regenEvent, rng) := buffed.updateLifeRegen state.player.body.velocity.x
    state.playerTextRng
  let body := state.player.body
  let manaBuff := activeBuffs.any fun (id, time) => id == 6 && time > 0
  let manaBefore := health.statMana
  -- UpdateManaRegen works on `statManaMax2`; IL_32f3..3302 then caps `statMana` at it.
  let manaMax2 := state.statManaMax2
  let (regenerated, rng) := ({ health with statManaMax := manaMax2 }).advanceMana
    (body.isConsideredStandingStill || body.hasGrapple) manaBuff rng
    (isUsingItem := state.itemUse.itemAnimation > 0) (consumeRefillScaleRolls := false)
    (evenWhenDead := client.isSome)
  let health := { regenerated with statManaMax := health.statManaMax, statMana := min regenerated.statMana manaMax2 }
  -- emit owns both the five scale rolls and successful NewDust allocations. With a
  -- disabled headless pool it naturally retains precisely the five scale rolls.
  let refill := OriginalManaRefillEffects.emit {
    restored := health.statMana > manaBefore, mana := health.statMana, maximum := manaMax2
    itemAnimation := state.itemUse.itemAnimation, position := body.position
    width := body.width, height := body.height }
    { state.combat.dustPool viewport with enabled := viewport.isSome } rng.stream
  let state := state.acceptTextRng rng
  let state := { state with
    health, itemRandom := refill.random, healthEvents := state.healthEvents ++ regenEvent.toArray
    manaRefillSound := refill.sound.isSome
    combat := { state.combat with manaRefillDust := state.combat.manaRefillDust ++ refill.particles } }
  if state.combat.textBoundary.isSome then return state
  let state := if regenEvent.isSome then state.registerDeath grid none viewport (some 8) client else state
  -- UpdateEquips latches this by type before the gemCount inventory cleanup.
  -- A typed-zero stone still blocks this tick even when cleanup turns it to air.
  -- Mouse58 is excluded; WorksInVoidBag[4346] is false in this desktop build.
  let preventAllItemPickups := state.carriedItems.any (·.any (fun stack => stack.item == 4346))
  let state := (state.refreshInventory.beginMove input).grabItems selected
    (client.bind (·.tileHitContext)) preventAllItemPickups
  if state.combat.textBoundary.isSome then return state
  -- LookForTileInteractions is after dropItemCheck/GrabItems and before
  -- NPC contact, collision movement and ItemCheck. Hover protects the next drop.
  let state := match client with
    | some input => state.lookForCandleInteractions input mouse crowdControlled
    | none => state
  let hurtClient := state.hurtClient client
  let contact := state.combat.contact state.player.body state.health state.itemRandom
    (defense := state.statDefense) (view := viewport) (client := hurtClient)
    -- `Update_NPCCollision` (IL_7186) runs after this update's `UpdateLuck` (IL_24cb)
    (luck := state.luck)
  let body := contact.body
  let state := { state with
    hurts := state.hurts ++ contact.hurt.toArray
    hurtSound := (contact.sound.map fun (side : OriginalPlayerHurt.SoundClient) => side.state).getD state.hurtSound
    player := { state.player with body }
    combat := { state.combat with
      guideDust := state.combat.guideDust ++ contact.dust
      texts := (contact.text.map (·.state)).getD state.combat.texts
      textBoundary := state.combat.textBoundary.or contact.boundary
      npcBoundary := state.combat.npcBoundary.or (contact.boundary.map fun why => (0,why))}
    health := contact.health, healthEvents := state.healthEvents ++ contact.events, itemRandom := contact.random }
  if state.combat.textBoundary.isSome then return state
  -- The current contact branch always assigns the signed horizontal knockback before KillMe.
  let state := match contact.killer with
    | some name => state.registerDeath grid (some (contact.hitDirection, name)) viewport none client
    | none => state
  let drowning := OriginalPlayerLiquidHealth.drownCollision grid body.position body.width body.height
  let lava := OriginalPlayerLiquidHealth.lavaCollision grid body.position body.width body.height
  -- The lava Hurt of a window client plays its sound and allocates its blood in the shared
  -- pool, after the contact's particles. Headless callers keep the legacy prefix-draw contract.
  let lavaClient : Option OriginalPlayerLiquidHealth.HurtClient :=
    (client.bind (·.tileHitContext)).map fun _ =>
      { hurt := state.hurtClient client, pool := state.combat.dustPool viewport
        position := body.position, width := body.width, height := body.height }
  let liquid := state.health.liquidChecksWith drowning lava state.statDefense state.playerTextRng lavaClient
  let (health, events, rng) := (liquid.health, liquid.events, liquid.rng)
  let state := { state with
    hurts := state.hurts ++ liquid.hurt.toArray
    hurtSound := (liquid.sound.map fun (side : OriginalPlayerHurt.SoundClient) => side.state).getD state.hurtSound
    combat := { state.combat with guideDust := state.combat.guideDust ++ liquid.dust } }
  let hit := OriginalPlayerWet.wetCollision grid body.position body.width body.height
  -- A boundary inside the lava Hurt stops the update with what Hurt had written, life included.
  let health := if rng.boundary.isSome then health else health.afterWet hit
  let state := { state.acceptTextRng rng with health, healthEvents := state.healthEvents ++ events }
  if state.combat.textBoundary.isSome then return state
  -- Drowning (ByOther(1), CheckDrowning) or lava (ByOther(2)) of this update.
  let liquidReason := if events.any (fun | .died .lava => true | _ => false) then 2 else 1
  -- Only a death of this very step: a player already killed in this update (fall, contact, a
  -- debuff) is not killed a second time here. Headless callers keep their earlier condition.
  let diedInLiquid := if client.isSome then events.any (fun | .died _ => true | _ => false)
    else regenEvent.isNone && contact.killer.isNone && health.dead
  let state := if diedInLiquid then
      state.registerDeath grid none viewport (some liquidReason) client
    else state
  let player : OriginalPlayerWet.Player := ⟨state.player, { state.playerLiquid with lavaWet := lava }⟩
  let player := player.contactWith hit
  let before := player.move.body
  let itemRandom := OriginalPlayerRandom.crackedBrickCheck grid before shimmering state.itemRandom
  let player := player.finish grid
  -- FloorVisuals: a window client's dirt dust goes into the real shared pool.
  let floorPool := viewport.map fun _ => state.combat.dustPool viewport
  let (_, floorDust, itemRandom) := OriginalPlayerRandom.floorVisualsWithPool grid before player.move.body
    player.liquid.wet floorPool itemRandom
  let state := { state with player := player.move, playerLiquid := player.liquid, itemRandom
                            combat := { state.combat with guideDust := state.combat.guideDust ++ floorDust } }
  let state := if !crowdControlled && (state.equipped selected).all (·.item != 2269) then
      { state with itemRandom := OriginalPlayerRandom.otherItemRevolverDecay state.itemRandom }
    else state
  -- A player who died earlier in this update (contact, lava, fall) still reaches ItemCheck
  -- (IL_8e4e): the method does not test `dead`. Only the next update takes the UpdateDead branch.
  -- A headless caller keeps the earlier contract and stops here.
  if health.dead && client.isNone then return state.finishCandleInteractions client
  -- The prefix pool includes all earlier player effects. Each actual KillTile then
  -- reserves its slots before drops and nested framing consume the same RNG.
  let state := { state with tileHitEffects := (client.bind (·.tileHitContext)).map fun context =>
    { context, pool := { state.combat.dustPool viewport with enabled := viewport.isSome } } }
  let state := if crowdControlled then
      let itemUse := { state.itemUse with itemAnimation := 0, itemAnimationMax := 0 }
      let use := { state.combat.use with itemAnimation := 0, itemAnimationMax := 0 }
      { state with itemUse, combat := { state.combat with use, weapon := none } }
    else if state.throwAnimation then
      let itemUse := state.itemUse.advance false 0 false
      let use := { state.combat.use with releaseUseItem := !held, itemAnimation := 0 }
      { state with
        itemUse, throwAnimation := itemUse.itemAnimation > 0, wallUseHeld := held
        combat := { state.combat with use, weapon := none } }
    else
      let before := state.itemUse.itemAnimation
      let used := state.updateSelectedItem grid selected held target bucketSplashReady mouse viewport
      -- ItemCheck IL_04b3..04d4 stores ItemCheck_TryStartUse's result when the start gate passes.
      -- Every implemented item starts once it passes, either from rest or renewed at animation 1.
      let used := if before ≤ 1 && used.itemUse.itemAnimation > before then
          { used with lastItemUseAttemptSuccess := true }
        else used
      -- IL_083a..0852: not on an update that dropped an item (JustDroppedAnItem), nor when CCed.
      let frame := (client.map (·.heldItemFrame)).getD fun _ => ⟨0, 0⟩
      let used := used.acceptTileHitEffects
      let used := if client.isSome then used.enableCandlePlacementIcon selected mouse else used
      (used.emitHeldItemLight selected viewport frame).resolveCandlePlacementSound
  let state := state.finishCandleInteractions client
  -- The item phase's `PlaySound(LegacySoundStyle, …)` calls (use sound, sounds of melee strikes),
  -- after every Hurt of this update, which all come before ItemCheck.
  -- KillTile_PlaySounds plays snow / ice (Item48..50) through the LegacySoundStyle overload too
  -- (native: one sample per call, heard or not); Dig and Tink use the int overload and take none.
  let killTileStyle := ((state.tileHitEffects.map (·.soundRequests)).getD #[]).countP fun request =>
    request.typeId == 2 && 48 ≤ request.style && request.style ≤ 50
  let state := (state.takeStyleSamples (state.pendingStyleSamples + state.combat.strikeStyleCalls + killTileStyle))
  let state := { state with pendingStyleSamples := 0, combat := { state.combat with strikeStyleCalls := 0 } }
  return { state with health := { state.health with statLife := min health.statLife health.statLifeMax2 } }

/-- `AchievementInitializer.Load`: the achievement completed by each `ProgressionEventCondition` event id
(`.lake/forest-desktop-integration/time-runtime/il/AchievementInitializer.Load.il`). -/
private def progressionAchievement : Nat → Option String
  | 1 => some "YOU_CAN_DO_IT" | 3 => some "KILL_THE_SUN" | 5 => some "BLOODBATH" | 6 => some "BEGONE_EVIL"
  | 7 => some "SMASHING_POPPET" | 8 => some "NO_HOBO" | 9 => some "ITS_HARD" | 10 => some "GOBLIN_PUNTER"
  | 11 => some "WALK_THE_PLANK" | 12 => some "DO_YOU_WANT_TO_SLAY_A_SNOWMAN" | 13 => some "TIN_FOIL_HATTER"
  | 14 => some "ICE_SCREAM" | 15 => some "BALEFUL_HARVEST" | 16 => some "STICKY_SITUATION"
  | 17 => some "REAL_ESTATE_AGENT" | 18 => some "IT_CAN_TALK" | 19 => some "DUNGEON_HEIST" | 20 => some "BIG_BOOTY"
  | 21 => some "MECHA_MAYHEM" | 22 => some "TEMPLE_RAIDER" | 23 => some "DEFEAT_OLD_ONES_ARMY_TIER3"
  | 24 => some "TURN_GNOME_TO_STATUE" | 25 => some "THROW_A_PARTY" | 26 => some "ALL_TOWN_SLIMES"
  | 27 => some "TRANSMUTE_ITEM" | 29 => some "BOOK_WORM" | 30 => some "BOULDER_LORD" | 31 => some "QUEEN_MACHINE"
  | 32 => some "ROLLIN_IN_YOUR_GRAVE" | 33 => some "FEAR_THE_SUN" | 34 => some "ITS_SHALING_OUTSIDE"
  | 35 => some "EXTRA_LIFE" | 36 => some "GRAVE_MISTAKE" | 37 => some "SPICY_LICKS" | 38 => some "ON_FLEEK"
  | 39 => some "FORTUNE_FAVORS_THE_BOULD" | 40 => some "TRAINING_DAY" | 41 => some "MINI_ME" | 42 => some "TERRARIST"
  | 43 => some "NEW_DIGS" | 44 => some "MY_PEOPLE_NEED_ME" | 45 => some "TRASH_COMPACTOR"
  | 46 => some "INTERDIMENSIONAL_RECYCLING"
  | _ => none

/-- Static NPC data read by the time caller, from the running original
(`.lake/original-player-death-client/npc-sets.json`, capture `npc_sets_survey`). -/
private def bossTypes : Array Nat := #[4, 35, 50, 113, 125, 126, 127, 134, 222, 245, 262, 266, 370, 395, 396, 397,
  398, 439, 636, 657, 664, 668]
private def dangerTypes : Array Nat := #[13, 14, 15, 399, 422, 493, 507, 517]
private def townPetTypes : Array Nat := #[637, 638, 656, 670, 678, 679, 680, 681, 682, 683, 684]

/-- One `Main.UpdateTime` through `OriginalTimeCaller` (Main.UpdateWorld_Time's SwapRandom stream is the
forest's `timeRandom`, carried inside the caller's state). -/
private def State.advanceTimeCaller (state : State) (header : OriginalWorld.Header) (client : Option ClientInput) :
    State := Id.run do
  let some forest := state.forest | return state
  let some tc := forest.timeCaller | return state
  let ticks := client.bind (·.clockTicks)
  -- checkHalloween / checkXMas at load: the first update that knows the local date
  let mut forest := forest
  let mut tc := tc
  -- `Main.numClouds` / `cloudBGActive` are one pair of statics: the weather block (before UpdateTime)
  -- moves them and UpdateTime's rain stage reads (and under lanterns clamps) them.
  if let some w := forest.weather then
    let rain := tc.phase.world.rain
    tc := { tc with phase := { tc.phase with world := { tc.phase.world with
      rain := { rain with clouds := w.dry.clouds, cloudBackgroundActive := w.dry.cloudBackground.active } } } }
  if forest.season.isNone then
    if let some t := ticks then
      let (halloween, xmas) := OriginalForest.seasonAt t header
      forest := { forest with season := some (halloween, xmas) }
      tc := { tc with phase := { tc.phase with halloween, xmas } }
  let calendar : Option OriginalWorldTimePhase.Calendar := ticks.map fun t =>
    let (month, day) := OriginalForest.civilDate t
    { month, day }
  let body := state.player.body
  let flag := fun (name : String) => (forest.playerFlags.lookup name).getD false
  let actors := state.combat.actors
  let npcs : Array OriginalWorldTimePhase.Npc := (Array.range 200).map fun i =>
    let actor := actors[i]?.getD .empty
    let resident := actor.resident?
    let typeId := actor.typeId.toNat
    { index := i, typeId, active := actor.active, town := resident.isSome
      boss := bossTypes.contains typeId, homeless := (resident.map (·.homeless)).getD false
      -- aiStyle is read only for town NPCs (BirthdayParty.CanNPCParty): theirs is 7
      aiStyle := if resident.isSome then 7 else 0, townPet := townPetTypes.contains typeId }
  let anyDanger := header.invasionType > 0 || npcs.any (fun n => n.active && (n.boss || dangerTypes.contains n.typeId))
  let inventory : Array OriginalWorldTimePhase.Item := state.inventory.map fun slot =>
    match slot with
    | some stack => { typeId := stack.item.toNat, stack := stack.count }
    | none => {}
  let tf := header.timeFlags
  let progress : OriginalWorldTimePhase.Progress := {
    eye := header.downedEye, evil := header.downedEvilBoss, skeletron := state.downedSkeletron
    slimeKing := header.downedSlimeKing, queenBee := header.downedQueenBee
    mech1 := header.downedMech[0]!, mech2 := header.downedMech[1]!, mech3 := header.downedMech[2]!
    mechAny := header.downedMech[3]!, plant := header.downedPlantBoss, moonLord := tf.downedMoonlord
    deerclops := tf.downedDeerclops, goblins := header.downedGoblins, pirates := tf.downedPirates
    frost := tf.downedFrost, savedStylist := tf.savedStylist, savedAngler := header.savedAngler
    savedGoblin := tf.savedGoblin, savedTaxCollector := tf.savedTaxCollector, savedWizard := tf.savedWizard
    savedMech := header.savedMechanic || state.combat.mechanicRescued, savedGolfer := tf.savedGolfer
    boughtCat := tf.boughtCat, boughtDog := tf.boughtDog, boughtBunny := tf.boughtBunny
    unlocked := ((#[(17, tf.unlockedMerchantSpawn), (18, tf.unlockedNurseSpawn), (19, tf.unlockedArmsDealerSpawn),
      (38, tf.unlockedDemolitionistSpawn), (207, tf.unlockedDyeTraderSpawn), (208, tf.unlockedPartyGirlSpawn),
      (663, tf.unlockedPrincessSpawn), (670, tf.unlockedSlimeBlueSpawn), (678, tf.unlockedSlimeGreenSpawn),
      (679, header.unlockedSlimeOldSpawn)] ++
      (#[680, 681, 682, 683, 684].zip tf.unlockedSlimeOtherSpawns) : Array (Nat × Bool))).filterMap
        fun (id, unlocked) => if unlocked then some id else none }
  let width := header.width
  let player : OriginalWorldTimePhase.Player :=
    { index := 0, active := true, dead := state.health.dead, lifeMax := state.health.statLifeMax,
      lifeMax2 := state.health.statLifeMax2, defense := state.statDefense, y := body.position.y,
      zoneSnow := flag "ZoneSnow", townNPCs := forest.townNPCs, canSpawnEnemiesNear := !state.health.dead,
      inventory := inventory }
  let logTile : Int → Int → OriginalWorldTimePhase.LogTile := fun x y =>
    if x < 0 || y < 0 then {} else
    match state.terrain.tile? x.toNat y.toNat with
    | some t =>
      { active := t.id.isSome, typeId := (t.id.getD 0).toNat,
        liquid := (t.liquid.map (·.amount.toNat)).getD 0,
        frameX := (t.frame.map (·.1)).getD 0, frameY := (t.frame.map (·.2)).getD 0 }
    | none => {}
  let context : OriginalWorldTimePhase.Context := {
    netMode := 0, worldUpdateRate := 1, worldWidth := width, worldSurface := header.surface
    rockLayer := header.rockLayer, spawnTileY := header.spawnY, dungeonX := header.dungeon.1
    dungeonY := header.dungeon.2
    players := #[player]
    npcs, progress, hardMode := header.hardMode, anniversary := header.tenthAnniversary
    good := header.forTheWorthy, remix := header.remixWorld, drunk := header.drunkWorld, crimson := header.crimson
    surface := header.surface > 50, lowTiles := false, noAltars := false, altarCount := tf.altarCount
    shadowOrbSmashed := header.shadowOrbSmashed
    bossesKeepSpawning := header.forTheWorthy && header.dontStarveWorld && !header.tenthAnniversary
    mechdusa := header.remixWorld && header.forTheWorthy, dd2 := false
    lunarApocalypse := tf.lunarApocalypseIsUp, moonLordCountdown := 0, anyDanger
    infected := header.infectedSeed, vampire := tf.vampireSeed
    forceHalloweenForever := header.forceHalloweenForever, forceXmasForever := header.forceXMasForever
    npcTypeCount := 697, xmasCalendar := calendar, halloweenCalendar := calendar
    logGrid := some { width := header.width, height := header.height, tileAt := logTile } }
  let live : OriginalTimeCaller.Live := {
    windCurrent := (forest.weather.map (·.dry.windCurrent)).getD header.windSpeedTarget
    expertMode := header.gameMode == 1, hasWorldSurface := header.surface > 50, lowTiles := false
    downedSlimeKing := header.downedSlimeKing, worldWidth := width
    weatherPlayers := #[{ lifeMax := state.health.statLifeMax, defense := state.statDefense, luck := state.luck }]
    phase := context, downedGolem := header.downedGolem, downedSkeletron := state.downedSkeletron
    anyDanger, taxCollector := actors.any (fun a => a.active && a.typeId == 441)
    remixWorld := header.remixWorld
    eligibleTownCount := state.combat.residents.countP fun n => n.active && n.kind != OriginalForestGuide.Kind.oldMan
    homelessActorPresent := state.combat.residents.any fun n => n.active && n.homeless }
  if header.skyblock then
    return { state with forest := some { forest with stopped := forest.stopped.or (some "UpdateTime: Skyblock.lowTiles not tracked") } }
  -- callees performed on the session's combat state, the room statics and the progression events
  let housingWorld : OriginalHousing.World := {
    tileArrayWidth := header.width, tileArrayHeight := header.height
    tileAt := fun x y => if x < 0 || y < 0 then none else state.terrain.tile? x.toNat y.toNat
    maxTilesX := header.width, maxTilesY := header.height, lastMaxTilesY := header.height
    worldSurface := header.surface, unlockedTruffleSpawn := tf.unlockedTruffleSpawn
    mushroomTileThreshold := OriginalHousingTables.mushroomTileThreshold
    maxNPCs := OriginalHousingTables.maxNPCs, maxRoomTiles := OriginalHousingTables.maxRoomTiles
    maxRoomSize := OriginalHousingTables.maxRoomSize
    npcSampleHousingCategory := OriginalHousingTables.housingCategory
    tables := {
      tileSolid := OriginalHousingTables.tileSolid, tileSolidTop := OriginalHousingTables.tileSolidTop
      wallHouse := OriginalHousingTables.wallHouse, housingWalls := OriginalHousingTables.housingWalls
      ignoredInHouseScore := OriginalHousingTables.ignoredInHouseScore
      basicChest := OriginalHousingTables.basicChest
      countsAsChairTypes := OriginalHousingTables.countsAsChairTypes
      countsAsTableTypes := OriginalHousingTables.countsAsTableTypes
      countsAsTorchTypes := OriginalHousingTables.countsAsTorchTypes
      countsAsDoorTypes := OriginalHousingTables.countsAsDoorTypes
      tileIdCount := OriginalHousingTables.tileCount } }
  let housingNpcs := fun (combat : OriginalCombat.State) => (Array.range 200).map fun i =>
    let actor := combat.actors[i]?.getD .empty
    let r := actor.resident?
    ({ active := actor.active, type := actor.typeId, townNPC := r.isSome
       homeless := (r.map (·.homeless)).getD false, homelessDespawn := (r.map (·.homelessDespawn)).getD false
       homeTileX := (r.map (·.homeTile.1)).getD 0, homeTileY := (r.map (·.homeTile.2)).getD 0
       housingCategory := (OriginalHousingTables.housingCategory actor.typeId).getD 0 } : OriginalHousing.Npc)
  let callee : OriginalTimeCaller.Callee (OriginalCombat.State × OriginalHousing.RoomState × Array Nat) :=
    fun call (combat, room, events) =>
    match call with
    | .quickFindHome slot =>
      match OriginalHousing.quickFindHome housingWorld (housingNpcs combat) slot room with
      | .error why => .error s!"WorldGen.QuickFindHome: {why}"
      | .ok result =>
        let n := result.npc
        let combat := match combat.actors[slot]? with
          | some (.townResident r bow) =>
            let r := { r with homeless := n.homeless, homelessDespawn := n.homelessDespawn
                              homeTile := (n.homeTileX, n.homeTileY) }
            { combat with actors := combat.actors.set! slot (.townResident r bow) }
          | _ => combat
        let events := events ++ result.events.map fun | .notifyProgressionEvent e => e.toNat
        .ok ((combat, result.state, events), .unit)
    | .spawnOldMan x y =>
      -- NPC.NewNPC(GetSpawnSourceForTownSpawn, x, y, 37): Bottom-centre anchor, Old Man 18 x 40
      match combat.firstSpawnSlot with
      | none => .error "spawnOldMan: no free NPC slot"
      | some slot =>
        let resident : OriginalForestGuide.Resident := {
          kind := .oldMan, position := ⟨OriginalCollision.single (x - 9), OriginalCollision.single (y - 40)⟩ }
        .ok (({ combat.setActor slot (.townResident resident) with oldManChanged := true }, room, events), .npcSlot slot)
    | other => .error s!"UpdateTime callee {reprStr other} not ported"
  let room := state.housingRoom.getD (OriginalHousing.RoomState.afterCctor OriginalHousingTables.tileCount)
  match OriginalTimeCaller.step tc live callee (state.combat, room, #[]) with
  | .error why =>
    return { state with forest := some { forest with stopped := forest.stopped.or (some s!"UpdateTime: {why}") } }
  | .ok out =>
    let phase := out.state.phase
    -- effects: Old Man home assignment and progression achievements; chat / sync have no session consumer yet
    let (combatOut, room, housingEvents) := out.world
    let mut combat := combatOut
    let mut achievements := state.achievements
    let mut completed : Array String := #[]
    -- QuickFindHome's NotifyProgressionEvent(8) happened inside the census, before the later effects
    let progression := housingEvents.map OriginalWorldTimePhase.Effect.achievement
    for effect in progression ++ out.effects do
      if let .achievement event := effect then
        if let some name := progressionAchievement event then
          match achievements with
          | none =>
            let why := s!"UpdateTime: progression event {event} ({name}) with the profile's achievements unknown"
            return { state with forest := some { forest with stopped := forest.stopped.or (some why) } }
          | some done =>
            if !done.contains name then
              achievements := some (done.push name)
              completed := completed.push name
      if let .npcHome slot x y := effect then
        if let some (.townResident r bow) := combat.actors[slot]? then
          let housed : OriginalCombat.Actor := .townResident { r with homeless := false, homeTile := (x, y) } bow
          combat := { combat with actors := combat.actors.set! slot housed }
    let rainAfter := phase.world.rain
    let forestAfter := { forest with
      weather := forest.weather.map fun w => { w with dry := { w.dry with
        clouds := rainAfter.clouds, cloudBackground := { w.dry.cloudBackground with active := rainAfter.cloudBackgroundActive } } }
      timeCaller := some out.state, clock := phase.world.clock, timeRandom := phase.world.random
      critters := phase.critters, starfallBoost := phase.starfallBoost, slimeRainTime := phase.world.slime.time
      fairyLog := phase.fairyLog, anglerQuest := some phase.anglerQuest
      season := forest.season.map fun _ => (phase.halloween, phase.xmas)
      ranTownCensus := false }
    -- each completion: Main.NewText → PlaySound(SoundID.MenuTick), one SoundStyle sample
    let state := { state with forest := some forestAfter, combat, achievements, achievementsCompleted := completed
                              housingRoom := some room }
    return state.takeStyleSamples completed.size

/-- `Main.AnyDanger(true, false, false)` as rain reads it: the boss-NPC cache of the last NPC update
(the active actors) or an invasion; Moon Lord countdown, moons and DD2 are not part of this session. -/
def State.rainAnyDanger (state : State) : Bool :=
  state.terrain.header.invasionType > 0 ||
    state.combat.actors.any fun a => a.active && bossTypes.contains a.typeId.toNat

/-- DoDraw IL_073a..074d: `UpdateAtmosphereTransparencyToSkyColor(screenPosition.Y + screenHeight / 2)`
(il/Main.UpdateAtmosphereTransparencyToSkyColor.il), only its `Main.atmo` (the next update's MakeRain and
lightning read it; the sky colours are the renderer's). -/
def State.drawAtmosphere (state : State) (camera : SceneCamera) : State :=
  let header := state.terrain.header
  let k : Float32 := (header.width : Nat).toFloat.toFloat32 / 4200
  let k := k * k
  let y := camera.position.2 + OriginalCollision.single (camera.size.2.toUInt64.toNat / 2 : Nat)
  let atmo := ((y / 16 - (65 + 10 * k)).toFloat / (header.surface / 5)).toFloat32
  let atmo := if atmo < 0 then 0 else atmo
  let atmo := if atmo > 1 then 1 else atmo
  { state with forest := state.forest.map fun forest =>
      { forest with weather := forest.weather.map fun w => { w with atmo } } }

/-- DoDraw IL_0516..05c9 (`OriginalRainAtmosphere.drawCloudAlpha`): `Main.cloudAlpha` moves toward
`maxRaining` while it rains and decays after, once per draw. -/
def State.drawCloudAlpha (state : State) : State :=
  { state with forest := state.forest.map fun forest =>
      let rate := forest.dayRate
      { forest with timeCaller := forest.timeCaller.map fun tc =>
          let rain := tc.phase.world.rain
          let cloudAlpha := OriginalRainAtmosphere.drawCloudAlpha rain.cloudAlpha rain.strength rain.active false rate
          { tc with phase := { tc.phase with world := { tc.phase.world with rain := { rain with cloudAlpha } } } } } }

/-- What the desktop's draw computed for the weather: `Main.waterStyle` (the scene water style chain) and the
ambient waterfall / lavafall sources the last `DrawWaterfall` wrote (`none`: no DrawWaterfall call this draw,
the statics keep their values). -/
def State.noteDrawnScene (state : State) (waterStyle : Int) (ambient : Option OriginalForest.AmbientSources) : State :=
  { state with forest := state.forest.map fun forest =>
      { forest with waterStyle := some waterStyle, weather := forest.weather.map fun w =>
          match ambient with
          | some sources => { w with ambientSources := sources }
          | none => w } }

/-- DoDraw IL_13ff `Main.DrawRain` (il/Main.DrawRain.il): every active drop's sprite, then `Rain.Update`
on the unscoped `Main.rand` (the weather stream); a drop's splash is one `Dust.NewDust` of the water cohort
with the draw-time camera and the `Rain.Update` post-block (`OriginalWaterDust.splash`). `lightAt` is
`Lighting.GetColor` (packed R | G<<8 | B<<16 | A<<24), used only for the sprite colours. -/
def State.drawRain (state : State) (camera : SceneCamera) (lightAt : Int → Int → UInt32)
    (selected : Bool := true) (gfxQuality : Float32 := 1) : State × Array OriginalRainAtmosphere.Sprite := Id.run do
  let some forest := state.forest | return (state, #[])
  let some w := forest.weather | return (state, #[])
  let some tc := forest.timeCaller | return (state, #[])
  if forest.stopped.isSome then return (state, #[])
  let stop := fun (why : String) =>
    ({ state with forest := some { forest with stopped := forest.stopped.or (some s!"DrawRain: {why}") } }, #[])
  let header := state.terrain.header
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  let screen : OriginalCollision.Vector := ⟨camera.position.1, camera.position.2⟩
  let width : Int := camera.size.1.toUInt64.toNat
  let height : Int := camera.size.2.toUInt64.toNat
  let ctx := forest.rainContext header state.player.body w screen width height state.rainAnyDanger gfxQuality
    tc.phase.world.rain.cloudAlpha
  let birth : OriginalWaterDust.BirthContext := { screenPosition := screen, screenWidth := width, screenHeight := height }
  let mut water : OriginalWaterDust.State := {
    pool := state.combat.dustPool none, water := state.combat.waterDust, sentinel := state.combat.dustSentinel }
  let mut sprites := #[]
  let mut result ← match OriginalRainAtmosphere.drawFrom w.rain ctx grid selected lightAt w.wetFlags w.random with
    | .ok r => pure r
    | .error why => return stop why
  for _ in [:w.rain.maxRain + 1] do
    sprites := sprites ++ result.sprites
    let some pending := result.pending | break
    if forest.waterStyle.isNone then return stop "Main.waterStyle unknown at a splash"
    match OriginalWaterDust.splash water birth pending.splash result.random with
    | .error why => return stop why
    | .ok out =>
      water := out.state
      match OriginalRainAtmosphere.resumeDraw result.pool ctx grid selected lightAt result.wetFlags out.random
          pending.continuation with
      | .ok r => result := { r with sprites := r.sprites }
      | .error why => return stop why
  if result.pending.isSome then return stop "more splashes than drops"
  let combat := { state.combat with waterDust := water.water.filter (·.dust.active), dustSentinel := water.sentinel }
  let forest := { forest with weather := some { w with rain := result.pool, random := result.random, wetFlags := result.wetFlags } }
  return ({ state with forest := some forest, combat }, sprites)

private def State.stepWith (state : State) (input : Controls) (selected : Nat)
    (held : Bool) (target : Option Cell) (bucketSplashReady : Bool)
    (mouse : Option (Float32 × Float32)) (throwInput : Option ThrowInput)
    (windGuidHash : Option Int32)
    (projectileViewport : Option OriginalForestGuideArrow.Viewport) (talkNPC : Int)
    (client : Option ClientInput) : State :=
  if state.combat.textBoundary.isSome then state else
  -- BEGIN B-ZONE lighting-update-statics (S01)
  -- A lighting fault is fatal (no further update). Main.DoUpdate IL_0873 `DoUpdate_AnimateTileGlows`
  -- (demonTorch) and IL_08cc `timeForVisualEffects`, before DoUpdateInWorld; neither reads the world.
  if state.lightingFault.isSome then state else
  let state := { state with
    tileGlows := state.tileGlows.update
    timeForVisualEffects := OriginalLightingRuntime.Statics.stepTimeForVisualEffects state.timeForVisualEffects }
  -- END B-ZONE lighting-update-statics
  let openStorage := state.openStorage.filter fun storage => match storage with
    | .chest index => state.chests[index]?.any (OriginalChests.inOpenRange state.terrain state.player.body)
    | .bank furniture => OriginalBanks.inOpenRange state.terrain state.player.body furniture
  let banks := state.banks.updateVoidVaultInfo state.carriedItems
  let state := { state with
    openStorage, banks, foregroundEdits := #[], bucketSplash := none
    collected := false, healthEvents := #[], hurts := #[], throwStarted := false
    deathSoundRequest := none, deathSound := none, deathMessageTick := false
    projectileSoundRequests := #[], projectileSounds := #[]
    manaRefillSound := false, manaRefillLights := #[]
    dustLights := #[], heldItemLights := #[]
    tileHitEffects := none
    pickupSoundRequests := #[], pickupSounds := #[]
    candlePlacementSound := none
    selectionMenuTick := false, blackFade := false
    -- BEGIN B-ZONE actions-update-reset (S02)
    quickSoundRequests := #[], quickSounds := #[]
    -- END B-ZONE actions-update-reset
    -- DoUpdateInWorld_Inner increments GameUpdateCount before anything else.
    updateCount := state.updateCount + 1
    combat := {state.combat.updateProtectedSpawnSlots with
      textFonts := (client.bind (·.combatTextFonts)).or state.combat.textFonts} }
  let frame : Option OriginalForest.FrameInputs := (client.bind (·.camera)).map fun camera =>
    { grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
      screenPosition := ⟨camera.position.1, camera.position.2⟩
      screenWidth := camera.size.1.toUInt64.toNat, screenHeight := camera.size.2.toUInt64.toNat
      anyDanger := state.rainAnyDanger
      gfxQuality := (client.map (·.gfxQuality)).getD 1
      flashyEffectsWorld := (client.map (·.flashyEffectsWorld)).getD true }
  let forest := state.forest.map fun forest =>
    let body := state.player.body
    forest.advanceWeather state.terrain.header body state.health.statLifeMax.toNat windGuidHash frame
  let state := { state with forest }
  let previousPlayerPosition := state.player.body.position
  let state := state.updatePlayer (Grid.ofTerrain state.terrain OriginalTileRules.collision)
    input selected held target bucketSplashReady mouse throwInput projectileViewport client
  if state.combat.textBoundary.isSome then state else
  -- Later readers of HeldItem see the selection SelectedItemState.Update just left.
  let selected := if state.selectionLive then state.selection.selected else selected
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  let header := state.terrain.header
  let world := combatWorld state grid
  let body := state.player.body
  let townWorld : Option OriginalForestGuide.World := state.forest.map fun forest =>
    { grid, dungeon := header.dungeon, dayTime := state.dayTime, eclipse := header.eclipse
      downedSkeletron := state.downedSkeletron, downedEye := header.downedEye
      downedEvilBoss := header.downedEvilBoss, downedQueenBee := header.downedQueenBee
      downedGoblins := header.downedGoblins, crimson := header.crimson
      time := forest.clock.time, surface := header.surface, rockLayer := header.rockLayer
      cloudBackgroundActive := forest.weather.any (fun weather => weather.dry.cloudBackground.active > 0)
      raining := forest.raining header, hardMode := header.hardMode
      players := #[{ position := body.position, width := body.width, height := body.height,
                     canBeTalkedTo := !state.health.dead, life := state.health.statLife,
                     lifeMax := state.health.statLifeMax2,
                     selectedItem := ((state.equipped selected).map (fun (stack : OriginalItems.Stack) => stack.item.toNat)).getD 0,
                     zones := forest.playerFlags }] }
  let state : State := match state.forest with
    | none => state
    | some forest =>
      let heldItem := ((state.equipped selected).map (fun (stack : OriginalItems.Stack) => stack.item)).getD 0
      -- `Spawner.SetSpawnFlags` IL_002c copies `Player.luck` from this update's UpdateLuck.
      -- `SetSpawnFlags` IL_0372 offensiveToTim: a robe in GetEffectiveArmor(1), no Wizard Hat in GetEffectiveArmor(0)
      let (head, headOpen) := OriginalPlayerEquipment.effectiveArmor state.equipment 0
      let (bodyArmor, bodyOpen) := OriginalPlayerEquipment.effectiveArmor state.equipment 1
      let offensiveToTim : Option Bool := if headOpen.isSome || bodyOpen.isSome then none else
        let robe := bodyArmor.item == 4256 || (1282 ≤ bodyArmor.item && bodyArmor.item ≤ 1287)
        some (robe && !bodyArmor.isAir && head.item != 238)
      let (forest, combat) := forest.spawn state.terrain world body state.health.dead heldItem state.combat state.luck
        (some state.health.statLifeMax) offensiveToTim
      { state with forest := some forest, combat }
  let combat := state.combat.advanceNpcs world body state.health.dead
    state.itemUse.itemAnimation (state.lootContext grid) townWorld
    (state.forest.map fun forest => OriginalForest.critterWorld world header (forest.raining header))
    ((state.forest.map (·.clock.time)).getD header.time) projectileViewport talkNPC
    -- A window client's sound gates reach the NPC phase too: an audible NPCHit / NPCDeath draws
    -- its pitch from the UpdateNPCs stream at the strike.
    ((client.bind (·.tileHitContext)).map fun context => {
      dedicatedServer := context.dedicatedServer, supported := context.audioSupported
      generating := context.generating, netMode := context.netMode, volume := context.soundVolume
      cameraCenter := context.cameraCenter, viewWidth := context.maxWorldViewWidth
      attenuationDistance := context.attenuationDistance })
    -- The targeted (local) player's `ZoneGraveyard` from this update's biome scan.
    ((state.forest.bind fun forest => forest.playerFlags.lookup "ZoneGraveyard").getD false)
    header.eclipse
  -- Every `PlaySound(LegacySoundStyle, …)` of the NPC phase took one `SoundStyle._random` sample.
  let state := state.takeStyleSamples combat.npcStyleCalls
  if combat.textBoundary.isSome then {state with combat} else
  let (state, combat) : State × OriginalCombat.State := match OriginalDoor.applyTerrain state.trackTerrain combat.npcTileEdits with
    | .ok terrain => (state.adoptTerrain terrain (some (combat.npcTileEdits.toList.map fun (e : OriginalDoor.TileEdit) => (e.x, e.y))), combat)
    | .error why => (state, { combat with npcBoundary := combat.npcBoundary.or (some (0, why)) })
  -- Main.DoUpdateInWorld_Inner runs UpdateGores between NPCs and projectiles.
  let combat := match OriginalCombat.advanceGorePool combat.gorePool
      (Grid.ofTerrain state.terrain OriginalTileRules.collision) combat.goreLoadedTypes with
    | .ok gorePool => { combat with gorePool }
    | .error why => { combat with npcBoundary := combat.npcBoundary.or (some (0, why)) }
  let state := ({ state with
    worldItems := combat.npcDrops.foldl OriginalItems.insertDrop state.worldItems
    combat := { combat with npcDrops := #[] } } : State).advanceProjectiles projectileViewport
      (client.bind (·.tileHitContext))
  -- Strikes of the projectile phase (the stab): their HitSound / DeathSound style samples.
  let state := state.takeStyleSamples state.combat.strikeStyleCalls
  let state := { state with combat := { state.combat with strikeStyleCalls := 0 } }
  if state.combat.textBoundary.isSome then state else
  let grid := Grid.ofTerrain state.terrain OriginalTileRules.collision
  let state := { state with worldItems :=
    OriginalWorldItemLiquid.update grid state.worldItems (state.dayTime && !header.remixWorld) }
  let guideUpdates : Array (Nat × Option OriginalForestGuideArrow.DustParticle × Option OriginalManaRefillEffects.Light) := match projectileViewport with
    | some view => (state.combat.guideDust.qsort (fun a b => a.slot < b.slot)).map fun dust =>
      if dust.slot ≥ state.combat.dustDrawBudget then (dust.slot,none,none)
      else if dust.request.typeId == 172 || dust.request.typeId == 6 then
        let next := OriginalDarkCaster.advanceDust dust view
        (dust.slot, if next.active then some next.particle else none,
          some { x := next.light.x,y := next.light.y,r := next.light.r,g := next.light.g,b := next.light.b })
      else if OriginalHeldItemLight.handlesDust dust.request.typeId then
        -- The other flame and torch dust a held light item creates.
        let next := OriginalHeldItemLight.advance dust view
          { worldSurface := header.surface, underworldLayer := (header.height : Int) - 200
            remixWorld := header.remixWorld }
        (dust.slot, if next.active then some next.particle else none,
          next.light.map fun l => {x := l.x,y := l.y,r := l.r,g := l.g,b := l.b})
      else if OriginalForestGuideArrow.supportsBasicGround dust.request.typeId then
        (dust.slot,(OriginalForestGuideArrow.advanceParticles #[dust]
          state.combat.dustDrawBudget view)[0]?,none)
      else if OriginalSnowIceDust.supports dust.request.typeId then
        (dust.slot,(OriginalSnowIceDust.advanceParticles #[dust]
          state.combat.dustDrawBudget view)[0]?,none)
      else if OriginalBloodDust.supports dust.request.typeId then
        -- Hurt blood (type 5; stoned type 1 is already in the basic ground cohort).
        (dust.slot,(OriginalBloodDust.advanceParticles #[dust]
          state.combat.dustDrawBudget view)[0]?,none)
      else (dust.slot,dust.advance view,none)
    | none => #[]
  let guideDust := guideUpdates.filterMap (·.2.1)
  let guideLights := guideUpdates.filterMap fun p => p.2.2.map fun light => (p.1,light)
  let manaUpdates : Array (Nat × OriginalManaHeat.Advanced) := match projectileViewport with
    | some view => ((state.combat.manaDust.filter (·.slot < state.combat.dustDrawBudget)).qsort
        (fun a b => a.slot < b.slot)).map fun p =>
      let position := state.player.body.position
      let next := p.advance ⟨position.x - previousPlayerPosition.x, position.y - previousPlayerPosition.y⟩ view
      (p.slot,next)
    | none => #[]
  let manaDust := manaUpdates.filterMap fun p => if p.2.particle.active then some p.2.particle else none
  let manaLights := manaUpdates.map fun p =>
    let (position,rgb) := p.2.emittedLight
    (p.1,({ x := tileIndex position.x,y := tileIndex position.y
            r := rgb.1,g := rgb.2.1,b := rgb.2.2 } : OriginalManaRefillEffects.Light))
  let refill : Array OriginalManaRefillEffects.Advanced := match projectileViewport with
    | some view => ((state.combat.manaRefillDust.filter (·.dust.slot < state.combat.dustDrawBudget)).qsort
        (fun a b => a.dust.slot < b.dust.slot)).map fun p =>
        p.advance state.player.body.velocity state.playerLiquid.wet view
    | none => #[]
  let manaRefillDust := refill.filterMap fun p => if p.particle.active then some p.particle else none
  let manaRefillLights := refill.filterMap (·.light)
  let refillLights := refill.filterMap fun p => p.light.map fun light => (p.particle.dust.slot,light)
  let dustLights := ((guideLights ++ manaLights ++ refillLights).qsort (fun a b => a.1 < b.1)).map (·.2)
  -- The water dust cohort (rain splashes) in the same UpdateDust pass: the table as it was before the
  -- pass (every cohort's slots), the update-time camera, Collision.honey/shimmer carried by the weather.
  let (state, waterDust) : State × Array OriginalWaterDust.Owned := match projectileViewport with
    | some view =>
      if state.combat.waterDust.isEmpty then (state, state.combat.waterDust) else
      let wetFlags := (state.forest.bind (·.weather) |>.map (·.wetFlags)).getD {}
      let water : OriginalWaterDust.State := {
        pool := state.combat.dustPool projectileViewport, water := state.combat.waterDust
        sentinel := state.combat.dustSentinel }
      match OriginalWaterDust.update water grid { screenPosition := view.position, screenHeight := view.height } wetFlags with
      | .ok out =>
        let forest := state.forest.map fun forest =>
          { forest with weather := forest.weather.map fun w => { w with wetFlags := out.wetFlags } }
        ({ state with forest }, out.state.water.filter (·.dust.active))
      | .error why =>
        ({ state with forest := state.forest.map fun forest =>
            { forest with stopped := forest.stopped.or (some s!"UpdateDust (water): {why}") } },
          state.combat.waterDust)
    | none => (state, state.combat.waterDust)
  let combat := { state.combat with guideDust, manaDust, manaRefillDust, waterDust }
  -- DoUpdateInWorld_Inner: Dust -> LeashedEntities -> FloatingText -> Time.
  -- CombatText.UpdateCombatText is RNG-free; PopupText owns the rest of that
  -- separate random stage. Never advance text once per player or once per draw.
  let combat := if combat.textContext.netMode == (2 : Int) then combat
    else {combat with texts := combat.texts.advance}
  let state := { state with combat, manaRefillLights, dustLights }
  -- BEGIN B-ZONE lighting-add-lights (S01)
  -- `Lighting.AddLight` enqueues `_perFrameLights` at the call: the held item's lights of
  -- Player.Update (`AddLight(Vector2, …)`) before the Dust phase's (`AddLight(int, int, …)`).
  -- `Main.gamePaused` false and `netMode` 0 (single-player session without a pause).
  let state : State := Id.run do
    let mut rt := state.lighting
    for light in state.heldItemLights do
      match OriginalLightingRuntime.addLightAt rt false 0 (light.position.x, light.position.y)
          light.r light.g light.b with
      | .ok next => rt := next
      | .error f => return { state with lightingFault := some s!"Lighting.AddLight(Vector2): {reprStr f}" }
    for light in dustLights do
      rt := OriginalLightingRuntime.addLight rt false 0 (Int32.ofInt light.x) (Int32.ofInt light.y)
        light.r light.g light.b
    return { state with lighting := rt }
  if state.lightingFault.isSome then state else
  -- END B-ZONE lighting-add-lights
  let state := if state.forest.any (·.timeCaller.isSome) then state.advanceTimeCaller header client else
    let state := { state with
      forest := state.forest.map fun forest =>
        let townCount := state.combat.residents.countP fun (n : OriginalForestGuide.Resident) =>
          n.active && n.kind != OriginalForestGuide.Kind.oldMan
        let forest := OriginalForest.State.advanceTime forest header state.health.statLifeMax2.toNat townCount
        -- `checkHalloween` / `checkXMas` with the window's local date: on the first update (world load)
        -- and again after a dawn cleared them
        match forest.season, client.bind (·.clockTicks) with
        | none, some ticks => { forest with season := some (OriginalForest.seasonAt ticks header) }
        | _, _ => forest }
    if state.forest.any (·.ranTownCensus) && !state.downedSkeletron then
      { state with combat := state.combat.trySpawnOldMan header.dungeon }
    else state
  let state := match state.liquids with
    | none => state
    | some runtime => state.commitLiquids runtime.tickFrame
  -- The client scans from the draw's Lighting clock (`drawLighting`). A headless host has no
  -- draw: its driver's one scene scan per update is kept here.
  if client.isSome then state else
  { state with forest := state.forest.map fun forest =>
      let selectedItem := ((state.equipped selected).map (fun stack => stack.item.toNat)).getD 0
      OriginalForest.State.scanScene forest state.terrain state.player.body state.combat selectedItem }

/-- One Classic preview tick, from player input through world item motion. The caller declares
`Player.selectedItem` and `controlUseItem` for this update, as a headless host does. -/
def State.step (state : State) (input : Controls) (selected : Nat)
    (held : Bool) (target : Option Cell) (bucketSplashReady : Bool := false)
    (mouse : Option (Float32 × Float32) := none) (throwInput : Option ThrowInput := none)
    (windGuidHash : Option Int32 := none)
    (projectileViewport : Option OriginalForestGuideArrow.Viewport := none) (talkNPC : Int := -1) : State :=
  state.stepWith input selected held target bucketSplashReady mouse throwInput windGuidHash
    projectileViewport talkNPC none

/-- The state `Player.Spawn(SpawningIntoWorld)` leaves for the first world draw: the entry
`BlackFadeCameraTeleport` set `renderNow` and, given the camera, scanned the scene with the held
item; Lighting.Initialize left the engine at state 0. From here the session owns the real
`SelectedItemState`. -/
def State.enterClient (state : State) (camera : Option SceneCamera := none) : State :=
  -- WorldGen.clearWorld leaves Main.mapDelay = 2 on every world load (IL_0057).
  -- A new Player's releaseUseItem is false until its first ItemCheck writes `!controlUseItem`.
  let state := { state with
    selectionLive := true, renderNow := true
    -- BEGIN B-ZONE lighting-enter (S01)
    lighting := { state.lighting with statics := { state.lighting.statics with mapDelay := 2 } }
    -- END B-ZONE lighting-enter
    combat := { state.combat with use := { state.combat.use with releaseUseItem := false } } }
  match camera with
  | none => state
  | some camera =>
    -- Main.DrawMenu reset the metrics; the entry BlackFadeCameraTeleport then scans with the
    -- PerspectivePlayer while GameUpdateCount still holds its pre-load value, 0 in a fresh process.
    let forest := state.forest.map fun forest => { forest with sceneMetrics := none }
    let state := { state with forest, plainSceneMetrics := none }
    state.mainUpdateSceneMetrics camera (some 0)

/-- One desktop-client update. `SelectedItemState` runs inside `Player.Update` from the actual
Select events, the physical mouse button and the previous draw's `mouseInterface`; item use,
drops and every later HeldItem reader take the effective `selectedItem`. The scene metrics are
left to the draw's `drawLighting`. -/
def State.stepClient (state : State) (input : Controls) (client : ClientInput)
    (target : Option Cell) (throwControls : OriginalItemThrow.Controls)
    (bucketSplashReady : Bool := false) (mouse : Option (Float32 × Float32) := none)
    (windGuidHash : Option Int32 := none)
    (projectileViewport : Option OriginalForestGuideArrow.Viewport := none) (talkNPC : Int := -1) : State :=
  let state := { state with selectionLive := true }
  let selected := state.selection.selected
  state.stepWith input selected client.mouseLeft target bucketSplashReady mouse
    (some ⟨throwControls, selected⟩) windGuidHash projectileViewport talkNPC (some client)

end Lean4Terria.OriginalSession
