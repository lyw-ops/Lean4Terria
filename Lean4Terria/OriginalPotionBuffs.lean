import Lean4Terria.OriginalCandleBuffs
import Lean4Terria.OriginalHealingPotions

/-!
# Ordinary buff potions (Terraria 1.4.5.8 desktop, single player, Classic)

Hand port of the installed 1.4.5.8 CIL (`Terraria.exe`
d52c7ea7e4e312d8937fd20d1f2609f0d6bd26daddf5bfab2d106ca9077103e0):

* `Player.AddBuff` 100665465 with `AddBuff_DetermineBuffTimeToAdd` 100665471. The rest of the chain
  (`AddBuff_TryUpdatingExistingBuffTime` 100665470, `AddBuff_ActuallyTryToAddTheBuff` 100665467, `DelBuff`
  100665472) is the released `OriginalHealingPotions.addBuffChain`; the expiry loop is the released
  `OriginalCandleBuffs.clearExpired`.
* `Player.UpdateBuffs` 100665555 — the timer decrement and the effect branches of the buffs in `effectBuffs`.
* The `Player.Update` slice around it: `ResetEffects` (the fields in `Stats`), the `buffImmune` clear
  (IL_225a..2276), the expiry loop (IL_23c5..2406) and the immunity loop (IL_3361..33a5).
* `Player.ItemCheck` 100666014 for an ordinary buff potion (or an empty slot) in the selected slot: the use
  state machine (IL_0000..07ce), the buff branch IL_0928..096c and the consumption IL_188a..1993, with the callees
  `ItemCheck_AutoReuseLogic`, `ApplyReuseDelay`, `ItemCheck_TryStartUse`, `ItemCheck_StartActualUse`,
  `ApplyItemAnimation`, `SetItemAnimation`, `ApplyItemTime`, `SetItemTime`, `Item.TurnToAir`.

Findings from the installed binary (read in the CIL and checked against native runs):
* Every potion in `potionItem?` has useStyle 9, useTime = useAnimation = 17, consumable, no autoReuse / reuseDelay /
  shoot / mana / heal / hair dye, damage −1, not melee/ranged/summon, no ForceConsumption entry.
* For these items `ItemCheck_TryStartUse` reduces to `!noItems` (the Cursed buff); `ItemCheck_ApplyPetBuffs`,
  `TryAllowingItemReuse`, `ItemCheck_HackHoldStyles` and the start-time mount/pet/fox branches do nothing.
* `BuffID.Sets.BuffTimeIsExtendedByDeadCellsPotionStationBuff` holds 1..18, 104..117, 121..124, 257, 343: with the
  `deadCellsPotionStation` flag a non-quiet AddBuff of those buffs lasts `(int)(time * 1.2f)`.
  `ResetEffects` clears the flag, so inside a normal frame it is false.
* Obsidian Skin sets `buffImmune[24]` in `UpdateBuffs`; the later immunity loop of `Player.Update` then deletes an
  On Fire! buff (24) in the same frame.
* ItemCheck draws from `Main.rand` (the revolver crit roll IL_02f4 every call, drink dust while animating). Those
  draws feed only `revolverCritChanceBonus` and dust, which this leaf does not model; nothing here reads
  `Main.rand`, and the native corpus randomises the seed per case.

Not modelled (explicit `OutOfScope` or documented caller duties): multiplayer, expert difficulty scaling, mounts,
grappling, other held items, buffs outside `effectBuffs`, sound requests, dust, the use animation frame.
-/

namespace Lean4Terria.OriginalPotionBuffs

open OriginalCandleBuffs (Slots)

/-- Why a request is not modelled by this leaf. -/
inductive OutOfScope where
  /-- A live slot holds a buff whose `UpdateBuffs` branch is not transcribed here. -/
  | liveBuff (type : Nat)
  /-- `AddBuff` of a type whose pet / melee / fed / difficulty facts are not checked here. -/
  | addBuffType (type : Nat)
  /-- The selected slot holds an item that is not an ordinary buff potion (or air). -/
  | heldItem (type : Nat)
  /-- The buff arrays do not have the 44 native slots. -/
  | slotCount
  deriving Repr, BEq, Inhabited

/-! ## Buff ids and native facts -/

def obsidianSkin : Nat := 1
def regeneration : Nat := 2
def swiftness : Nat := 3
def gills : Nat := 4
def ironskin : Nat := 5
def manaRegeneration : Nat := 6
def magicPower : Nat := 7
def featherfall : Nat := 8
def spelunker : Nat := 9
def invisibility : Nat := 10
def shine : Nat := 11
def nightOwl : Nat := 12
def battle : Nat := 13
def thorns : Nat := 14
def waterWalking : Nat := 15
def archery : Nat := 16
def hunter : Nat := 17
def gravitation : Nat := 18
def cursed : Nat := 23
def onFire : Nat := 24
def mining : Nat := 104
def flipper : Nat := 109

/-- Buffs whose `UpdateBuffs` branch is transcribed (`tick` refuses any other live buff). -/
def effectBuffs : List Nat :=
  [obsidianSkin, regeneration, swiftness, gills, ironskin, manaRegeneration, magicPower, featherfall, spelunker,
   invisibility, shine, nightOwl, battle, thorns, waterWalking, archery, hunter, gravitation, cursed, onFire,
   mining, flipper]

/-- Types `addBuff` accepts: for all of them the installed image has `Main.lightPet`, `Main.vanityPet`,
`Main.meleeBuff`, `BuffID.Sets.IsFedState` false, `AddBuffTimeAdditivelyToCap` 0, and none is 46, 47 or 124 — so
`AddBuff_RemoveOldPet/MeleeBuffsOfMatchingType`, `AddBuff_ReduceTimeOfRelatedBuffs`, the fed-state purge and the
cold-resistance divisions do nothing. -/
def addBuffTypes : List Nat := effectBuffs

/-- `BuffID.Sets.BuffTimeIsExtendedByDeadCellsPotionStationBuff` (installed image). -/
def extendedByPotionStation (type : Nat) : Bool :=
  (1 ≤ type && type ≤ 18) || (104 ≤ type && type ≤ 117) || (121 ≤ type && type ≤ 124) || type == 257 || type == 343

/-- `Main.debuff` of the installed image (same table as the released `OriginalHealingPotions.isDebuff`). -/
def isDebuff (type : Nat) : Bool := OriginalHealingPotions.isDebuff type

/-! ## AddBuff -/

structure AddContext where
  /-- `Player.buffImmune` (only read at `type`). -/
  buffImmune : Nat → Bool := fun _ => false
  /-- `Player.deadCellsPotionStation`. -/
  deadCellsPotionStation : Bool := false

/-- `AddBuff_DetermineBuffTimeToAdd` in Classic (`Main.expertMode` false; `resistCold` only touches 46/47). -/
def determineBuffTime (ctx : AddContext) (type : Nat) (time : Int) : Int :=
  if ctx.deadCellsPotionStation && extendedByPotionStation type then
    (Float32.ofInt time * Float32.ofBits 0x3F99999A).toInt32.toInt  -- (int)(time * 1.2f)
  else time

/-- `Player.AddBuff(type, time, quiet)` for the local player in single player. -/
def addBuff (ctx : AddContext) (slots : Slots) (type : Nat) (time : Int) (quiet : Bool := false) :
    Except OutOfScope Slots := do
  unless slots.types.size == 44 && slots.times.size == 44 do throw .slotCount
  unless addBuffTypes.contains type do throw (.addBuffType type)
  -- IL_0000: an immune player ignores the request
  if ctx.buffImmune type then return slots
  -- IL_000b: the remote-player PvP branch needs netMode 1; IL_0046: no in-scope type is a fed state
  let time := if quiet then time else determineBuffTime ctx type time
  -- IL_0083..00ab: TryUpdatingExistingBuffTime, then (no-op exclusions, see `addBuffTypes`) ActuallyTryToAddTheBuff
  let h := OriginalHealingPotions.addBuffChain
    { (default : OriginalPlayerLiquidHealth.Health) with buffType := slots.types, buffTime := slots.times } type time
  return { types := h.buffType, times := h.buffTime }

/-! ## ResetEffects + UpdateBuffs -/

/-- The player fields written by the transcribed `UpdateBuffs` branches (plus the `UpdateBuffs` tail). -/
structure Stats where
  statDefense : Int := 0
  lifeRegen : Int := 0
  moveSpeed : Float32 := 1
  magicDamage : Float32 := 1
  thorns : Float32 := 0
  arrowDamage : Float32 := 1
  pickSpeed : Float32 := 1
  /-- Not reset by `ResetEffects`; eased by the `UpdateBuffs` tail. -/
  lavaOpacity : Float32 := 1
  lavaImmune : Bool := false
  fireWalk : Bool := false
  gills : Bool := false
  manaRegenBuff : Bool := false
  slowFall : Bool := false
  findTreasure : Bool := false
  invis : Bool := false
  nightVision : Bool := false
  enemySpawns : Bool := false
  waterWalk : Bool := false
  archery : Bool := false
  detectCreature : Bool := false
  gravControl : Bool := false
  ignoreWater : Bool := false
  accFlipper : Bool := false
  noItems : Bool := false
  cursed : Bool := false
  onFire : Bool := false
  /-- Not reset by `ResetEffects`. -/
  luckNeedsSync : Bool := false
  /-- `buffImmune[24]` — the only immunity a transcribed buff grants. -/
  immuneOnFire : Bool := false
  /-- Not reset by `ResetEffects`. -/
  oldLuckPotion : Nat := 0
  luckPotion : Nat := 0
  deriving Repr, BEq, Inhabited

/-- `ResetEffects` for the `Stats` fields, then `Player.Update` IL_225a..2276 (`buffImmune` cleared). -/
def Stats.reset (s : Stats) : Stats :=
  { s with statDefense := 0, lifeRegen := 0, moveSpeed := 1, magicDamage := 1, thorns := 0, arrowDamage := 1,
           pickSpeed := 1, lavaImmune := false, fireWalk := false, gills := false, manaRegenBuff := false,
           slowFall := false, findTreasure := false, invis := false, nightVision := false, enemySpawns := false,
           waterWalk := false, archery := false, detectCreature := false, gravControl := false,
           ignoreWater := false, accFlipper := false, noItems := false, cursed := false, onFire := false,
           luckPotion := 0,
           immuneOnFire := false }  -- Update IL_225a: buffImmune[] := false

/-- A `Lighting.AddLight(int, int, float, float, float)` request that reached the lighting engine. -/
structure Light where
  x : Int
  y : Int
  r : Float32
  g : Float32
  b : Float32
  deriving Repr, Inhabited

/-- What `UpdateBuffs` needs from the rest of the player and world. -/
structure Body where
  positionX : Float32
  positionY : Float32
  width : Int := 20
  height : Int := 42
  /-- `Main.gamePaused` (Lighting.AddLight returns early; netMode 2 never applies here). -/
  gamePaused : Bool := false
  deriving Repr, Inhabited

/-- `ResetEffects` IL_0e73 → `Player.ResizeHitbox` for an unmounted player (the portable stool was just reset, so
`HeightOffsetBoost` is 0): keep the feet where they are and restore the 20 × 42 hitbox. Two float32 roundings. -/
def Body.resizeHitbox (b : Body) : Body :=
  let feet := b.positionY + Float32.ofInt b.height
  { b with positionY := feet - Float32.ofInt 42, width := 20, height := 42 }

/-- UpdateBuffs IL_0430..0471: the Shine light at the body centre tile. -/
def shineLight (b : Body) : Light :=
  let cx := (b.positionX + Float32.ofInt (b.width.tdiv 2)).toInt32.toInt
  let cy := (b.positionY + Float32.ofInt (b.height.tdiv 2)).toInt32.toInt
  { x := cx.tdiv 16, y := cy.tdiv 16, r := Float32.ofBits 0x3F4CCCCD, g := Float32.ofBits 0x3F733333,
    b := Float32.ofBits 0x3F800000 }

/-- One transcribed effect branch of the `UpdateBuffs` if-chain. -/
def applyEffect (b : Body) (type : Nat) (s : Stats) (lights : Array Light) :
    Except OutOfScope (Stats × Array Light) :=
  if type == obsidianSkin then pure ({ s with lavaImmune := true, fireWalk := true, immuneOnFire := true }, lights)
  else if type == regeneration then pure ({ s with lifeRegen := s.lifeRegen + 4 }, lights)
  else if type == swiftness then pure ({ s with moveSpeed := s.moveSpeed + Float32.ofBits 0x3E800000 }, lights)
  else if type == gills then pure ({ s with gills := true }, lights)
  else if type == ironskin then pure ({ s with statDefense := s.statDefense + 8 }, lights)
  else if type == manaRegeneration then pure ({ s with manaRegenBuff := true }, lights)
  -- IL_039c: 0.2f, and DebugOptions.ManaV2 (on in the shipped game) picks the same 0.2f
  else if type == magicPower then pure ({ s with magicDamage := s.magicDamage + Float32.ofBits 0x3E4CCCCD }, lights)
  else if type == featherfall then pure ({ s with slowFall := true }, lights)
  else if type == spelunker then pure ({ s with findTreasure := true }, lights)
  else if type == invisibility then pure ({ s with invis := true }, lights)
  else if type == shine then pure (s, if b.gamePaused then lights else lights.push (shineLight b))
  else if type == nightOwl then pure ({ s with nightVision := true }, lights)
  else if type == battle then pure ({ s with enemySpawns := true }, lights)
  -- IL_04b7: bge.un skips when thorns ≥ 1 or NaN
  else if type == thorns then pure (if s.thorns < 1 then { s with thorns := 1 } else s, lights)
  else if type == waterWalking then pure ({ s with waterWalk := true }, lights)
  else if type == archery then
    pure ({ s with archery := true, arrowDamage := s.arrowDamage * Float32.ofBits 0x3F8CCCCD }, lights)
  else if type == hunter then pure ({ s with detectCreature := true }, lights)
  else if type == gravitation then pure ({ s with gravControl := true }, lights)
  else if type == cursed then pure ({ s with noItems := true, cursed := true }, lights)
  else if type == onFire then pure ({ s with onFire := true }, lights)
  else if type == mining then pure ({ s with pickSpeed := s.pickSpeed - Float32.ofBits 0x3E800000 }, lights)
  else if type == flipper then pure ({ s with ignoreWater := true, accFlipper := true }, lights)
  else throw (.liveBuff type)

/-- `Player.UpdateBuffs(0)` for the local player (soulDrain 0, not a Don't Starve world). Every live slot
(type > 0, time > 0) loses one tick (no transcribed buff is in `TimeLeftDoesNotDecrease`; their `MountType` is −1)
and applies its effect; then `UpdateHungerBuffs` (no transcribed buff is a fed state), the luck sync and the lava
opacity tail (lavaVision is false after ResetEffects and no transcribed buff sets it). -/
def updateBuffs (b : Body) (slots : Slots) (s : Stats) : Except OutOfScope (Slots × Stats × Array Light) := do
  unless slots.types.size == 44 && slots.times.size == 44 do throw .slotCount
  let mut slots := slots
  let mut s := s
  let mut lights : Array Light := #[]
  for i in [:44] do
    let type := slots.types[i]!
    if type > 0 && slots.times[i]! > 0 then
      slots := { slots with times := slots.times.set! i (slots.times[i]! - 1) }
      let (s', lights') ← applyEffect b type s lights
      s := s'
      lights := lights'
  -- IL_4b00: local luck sync
  if s.luckPotion != s.oldLuckPotion then s := { s with luckNeedsSync := true, oldLuckPotion := s.luckPotion }
  -- IL_4b76..4bad (lavaVision false): ease towards 1 by 0.04f, clamp at 1
  if s.lavaOpacity < 1 then
    s := { s with lavaOpacity := s.lavaOpacity + Float32.ofBits 0x3D23D70A }
    if s.lavaOpacity > 1 then s := { s with lavaOpacity := 1 }
  return (slots, s, lights)

/-- `Player.Update` IL_3361..33a5: delete every live buff the player is now immune to (no compaction rewind,
like the expiry loop). Only the Obsidian Skin immunity to On Fire! is produced inside this leaf. -/
def removeImmune (slots : Slots) (immune : Nat → Bool) : Slots := Id.run do
  let mut slots := slots
  for i in [:44] do
    if slots.types[i]! > 0 && slots.times[i]! > 0 && immune slots.types[i]! then
      slots := slots.remove i
  return slots

/-! ## ItemCheck for a buff potion -/

/-- `Item.SetDefaults` facts of an ordinary buff potion (installed image; all have useStyle 9,
useTime = useAnimation = 17, consumable). -/
structure PotionItem where
  buffType : Nat
  buffTime : Int
  useTime : Int := 17
  useAnimation : Int := 17
  deriving Repr, Inhabited

def potionItem? (type : Nat) : Option PotionItem :=
  match type with
  | 288 => some { buffType := 1, buffTime := 21600 }
  | 289 => some { buffType := 2, buffTime := 28800 }
  | 290 => some { buffType := 3, buffTime := 28800 }
  | 291 => some { buffType := 4, buffTime := 14400 }
  | 292 => some { buffType := 5, buffTime := 28800 }
  | 293 => some { buffType := 6, buffTime := 28800 }
  | 294 => some { buffType := 7, buffTime := 14400 }
  | 295 => some { buffType := 8, buffTime := 36000 }
  | 296 => some { buffType := 9, buffTime := 18000 }
  | 297 => some { buffType := 10, buffTime := 10800 }
  | 298 => some { buffType := 11, buffTime := 36000 }
  | 299 => some { buffType := 12, buffTime := 36000 }
  | 300 => some { buffType := 13, buffTime := 25200 }
  | 301 => some { buffType := 14, buffTime := 28800 }
  | 302 => some { buffType := 15, buffTime := 36000 }
  | 303 => some { buffType := 16, buffTime := 28800 }
  | 304 => some { buffType := 17, buffTime := 28800 }
  | 305 => some { buffType := 18, buffTime := 10800 }
  | 2322 => some { buffType := 104, buffTime := 36000 }
  | 2327 => some { buffType := 109, buffTime := 28800 }
  | _ => none

/-- The selected inventory slot. -/
structure Held where
  type : Nat := 0
  stack : Int := 0
  deriving Repr, BEq, Inhabited

/-- `Item.IsAir`. -/
def Held.isAir (h : Held) : Bool := h.type == 0 || h.stack ≤ 0

/-- The item-use fields of `Player` that `ItemCheck` reads and writes for these items. -/
structure Use where
  itemAnimation : Int := 0
  itemAnimationMax : Int := 0
  itemTime : Int := 0
  itemTimeMax : Int := 0
  reuseDelay : Int := 0
  altFunctionUse : Int := 0
  controlUseItem : Bool := false
  releaseUseItem : Bool := false
  pendingItemReuse : Bool := false
  lastItemUseAttemptSuccess : Bool := false
  deriving Repr, BEq, Inhabited

/-- `Player.ItemCheck()` for the local player holding `held` (an ordinary buff potion or air) in a selected
hotbar slot, not crowd-controlled, unmounted, no grappling hook, no buffered hotbar change, no pending inventory
action, no fast use, `IsAllowedToHoldItems` and not `JustDroppedAnItem`. `noItems` is the current Cursed flag. -/
def itemCheck (ctx : AddContext) (noItems : Bool) (use : Use) (held : Held) (slots : Slots) :
    Except OutOfScope (Use × Held × Slots) := do
  let item? := potionItem? held.type
  if held.type != 0 && item?.isNone then throw (.heldItem held.type)
  let mut u := { use with pendingItemReuse := false }                                    -- IL_0000
  let mut held := held
  let mut slots := slots
  -- IL_01b2 ItemCheck_AutoReuseLogic: air returns at once; potions are not autoReuse and have damage −1, so
  -- TryAllowingItemReuse leaves releaseUseItem alone.  IL_01b9 HackHoldStyles: nothing for these items.
  if u.itemAnimation < 0 then u := { u with itemAnimation := 0 }                         -- IL_01be
  if u.itemTime < 0 then u := { u with itemTime := 0 }
  if u.itemAnimation == 0 then u := { u with itemAnimationMax := 0 }
  if u.itemAnimation == 0 && u.reuseDelay > 0 then                                       -- ApplyReuseDelay
    u := { u with itemAnimation := u.reuseDelay, itemTime := u.reuseDelay, reuseDelay := 0 }
  if u.itemAnimation == 0 && u.altFunctionUse == 2 then u := { u with altFunctionUse := 0 }  -- IL_020b
  -- IL_0451: start a use (useStyle 9 for a potion, 0 for air)
  if u.controlUseItem && u.releaseUseItem && u.itemAnimation == 0 && item?.isSome then
    if u.altFunctionUse == 1 then u := { u with altFunctionUse := 2 }
    let canUse := !noItems                                                               -- ItemCheck_TryStartUse
    if canUse != u.lastItemUseAttemptSuccess then u := { u with lastItemUseAttemptSuccess := canUse }
    if canUse then
      let item := item?.get!
      -- ItemCheck_StartActualUse → ApplyItemAnimation → SetItemAnimation(useAnimation); reuseDelay := 0
      u := { u with itemAnimation := item.useAnimation, itemAnimationMax := item.useAnimation, reuseDelay := 0 }
  -- IL_071b: animation tick
  if u.itemAnimation > 0 then
    u := { u with itemAnimation := u.itemAnimation - 1 }
    if u.itemAnimation == 0 && u.reuseDelay == 0 && u.controlUseItem && u.releaseUseItem then
      u := { u with pendingItemReuse := true }
  -- IL_0783: Item.TurnToAir once the animation is over
  if u.itemAnimation == 0 && held.isAir && held.type != 0 then
    held := {}
    u := { u with pendingItemReuse := false }
  u := { u with releaseUseItem := !u.controlUseItem }                                     -- IL_07a8
  if u.itemTime > 0 then u := { u with itemTime := u.itemTime - 1 }                       -- IL_07ce
  -- IL_087f..096c: the use frame of a buff item
  if u.itemTime == 0 && u.itemAnimation > 0 then
    if let some item := potionItem? held.type then
      slots ← addBuff ctx slots item.buffType item.buffTime
      u := { u with itemTime := item.useTime, itemTimeMax := item.useTime }              -- ApplyItemTime
  -- IL_188a..1993: consumption (not ranged, not a coin, CanConsumeConsumableItem, no ForceConsumption entry)
  if u.itemTimeMax != 0 && u.itemTime == u.itemTimeMax && (potionItem? held.type).isSome then
    if held.stack > 0 then held := { held with stack := held.stack - 1 }
    if held.stack ≤ 0 then u := { u with itemTime := u.itemAnimation }
  return (u, held, slots)

/-! ## One frame -/

structure Player where
  slots : Slots := {}
  stats : Stats := {}
  use : Use := {}
  held : Held := {}
  body : Body
  deriving Repr, Inhabited

/-- Intermediate observations of one frame, in native order. -/
structure Trace where
  /-- After `UpdateBuffs`. -/
  stats : Stats
  lights : Array Light
  slotsAfterUpdate : Slots
  /-- After the expiry and immunity loops. -/
  slotsAfterCleanup : Slots
  deriving Inhabited

/-- One `Player.Update` slice for the local player: `ResetEffects` (its hitbox resize, + `buffImmune` cleared), `UpdateBuffs`, the
expiry loop, the immunity loop and `ItemCheck` with this frame's `controlUseItem`. Everything else `Update` does in
between (movement, equipment, regeneration, ...) stays with the caller; with no equipment it does not touch
these fields. -/
def frame (p : Player) (controlUseItem : Bool) : Except OutOfScope (Player × Trace) := do
  let body := p.body.resizeHitbox
  let (slots, stats, lights) ← updateBuffs body p.slots p.stats.reset
  let afterUpdate := slots
  let slots := OriginalCandleBuffs.clearExpired slots
  let slots := removeImmune slots (fun t => t == onFire && stats.immuneOnFire)
  let ctx : AddContext := { buffImmune := fun t => t == onFire && stats.immuneOnFire }
  let (use, held, slots') ← itemCheck ctx stats.noItems { p.use with controlUseItem } p.held slots
  return ({ p with slots := slots', stats, use, held, body },
          { stats, lights, slotsAfterUpdate := afterUpdate, slotsAfterCleanup := slots })

end Lean4Terria.OriginalPotionBuffs
