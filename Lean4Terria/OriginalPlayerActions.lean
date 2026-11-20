import Lean4Terria.OriginalMiningTools
import Lean4Terria.OriginalHealingPotions
import Lean4Terria.OriginalItemUse
import Lean4Terria.OriginalBowUse
import Lean4Terria.OriginalProjectiles
import Lean4Terria.OriginalPotionBuffs

/-!
# Player item actions: the `ItemCheck` / `Player.Update` dispatch owned by main B

Glue between the session's per-update player step and the released action leaves. Every action keeps the
leaf's own state types; the session owns the fields and calls these entries at the native positions.

* Pickaxes (`OriginalMiningTools`, Root ACCEPT-WITH-NOTES, sources `9d63d03b…`/`c9fdd8c2…`): the
  `ItemCheck` caller recipe that the leaf's native replay verified on all 4544 + 680 frames —
  `Timing.advance held useAnimation` (start/renew, then both decrements), `toolTime := itemTime`,
  `releaseUseItem := !controlUseItem`, then `ItemCheck_UseMiningTools`; a returned `itemTime` is stored
  directly (`itemTimeMax` untouched).
* Quick heal / quick mana (`OriginalHealingPotions`, Root ACCEPT-WITH-NOTES, `165e5dbd…`): the
  `Player.Update` key gates IL_0d87..0dd3 (desktop `Player.Update`, `.lake/original-mining/1.4.5.8/il`):
  `if controlQuickHeal { if releaseQuickHeal QuickHeal(); releaseQuickHeal = false } else releaseQuickHeal = true`,
  then the same for mana. The radial-quickbar path (IL_14e9/14f9, gamepad) is not wired.

Outside this module (the session decides): routing a pickaxe target that the leaf reports `unsupported`
(chests, banks, trees, gravestones, …) to its owner before accepting any partial result, sharing ONE
`Player.hitTile` table between pickaxe/hammer/axe (`OriginalMiningTools.State.hits`), and the swing sound /
swing dust draws of `ItemCheck` outside `ItemCheck_UseMiningTools`.
-/
namespace Lean4Terria.OriginalPlayerActions

open OriginalCollision (Vector)

/-! ## Dispatch -/

/-- What `ItemCheck` does with the held item type, for the actions wired here. -/
inductive Action where
  | pickaxe (pick : OriginalMiningTools.Pickaxe)
  | bow (bow : OriginalBowUse.Bow)
  /-- A buff potion `ItemCheck` drinks (288–305, 2322, 2327). -/
  | buffPotion (item : OriginalPotionBuffs.PotionItem)
  /-- `ItemCheck_UseBossSpawners` (43 Suspicious Looking Eye: `OriginalPlayerActions.BossSpawners`). -/
  | bossSpawner (item : Nat)
  | none
deriving Inhabited

/-- `Item.SetDefaults` stats of the pickaxes the mining leaf verified natively. -/
def actionOf (itemType : Nat) : Action :=
  match itemType with
  | 3509 => .pickaxe OriginalMiningTools.copperPickaxe
  | 1 => .pickaxe OriginalMiningTools.ironPickaxe
  | 3521 => .pickaxe OriginalMiningTools.goldPickaxe
  | 103 => .pickaxe OriginalMiningTools.nightmarePickaxe
  | 43 => .bossSpawner 43
  | 39 => match OriginalBowUse.woodenBow with
    | some b => .bow b
    | none => .none
  | t => match OriginalPotionBuffs.potionItem? t with
    | some item => .buffPotion item
    | none => .none

/-! ## Pickaxe use -/

/-- The local player's fields the pickaxe path reads, as they are when `ItemCheck` starts this frame. -/
structure PickaxeInput where
  timing : OriginalItemUse.Timing
  /-- `controlUseItem` of this update. -/
  held : Bool
  poundRelease : Bool
  position : Vector
  width : Nat := 20
  height : Nat := 42
  pickSpeed : Float32
  noBuilding : Bool
  /-- `Player.tileTargetX/Y`. -/
  target : Int × Int

structure PickaxeResult where
  /-- The player as `ItemCheck_UseMiningTools` saw it on entry (after the swing bookkeeping). -/
  swing : OriginalMiningTools.Swing
  timing : OriginalItemUse.Timing
  /-- `Player.releaseUseItem` after `ItemCheck` (`!controlUseItem`). -/
  releaseUseItem : Bool
  outcome : OriginalMiningTools.Outcome

/-- One `ItemCheck` of a held pickaxe (the verified caller recipe, then `ItemCheck_UseMiningTools`). -/
def usePickaxe (rules : OriginalMiningTools.Rules) (pick : OriginalMiningTools.Pickaxe) (input : PickaxeInput)
    (state : OriginalMiningTools.State) : PickaxeResult :=
  let timing := input.timing.advance input.held pick.useAnimation
  let swing : OriginalMiningTools.Swing :=
    { position := input.position, width := input.width, height := input.height
      itemAnimation := timing.itemAnimation, toolTime := timing.itemTime
      controlUseItem := input.held, releaseUseItem := !input.held, poundRelease := input.poundRelease
      noBuilding := input.noBuilding, pickSpeed := input.pickSpeed, target := input.target }
  let outcome := OriginalMiningTools.useMiningTools rules pick swing state
  let timing := match outcome.itemTime with
    | some t => { timing with itemTime := t }
    | none => timing
  { swing, timing, releaseUseItem := !input.held, outcome }

/-! ## Weapon stats (`OriginalWeaponStats`) -/

/-- `Item.SetDefaults(type)` then `Item.Prefix(pre)`: how a stored prefix byte is realised on an item instance
(player load, reforge). A valid prefix draws nothing; a rejected one re-rolls from `rng` (`Main.rand`), which is
returned for the caller to store back. -/
def weaponItem (type : UInt16) (pre : Int) (rng : UnifiedRandom) : Option (OriginalWeaponStats.Item × UnifiedRandom) := do
  let base ← OriginalWeaponStats.Item.base type
  let r ← base.prefix? pre rng
  return (r.item, r.rng)

/-- The bow `ItemCheck` runs with: the `actionOf` bow shape with the prefixed `Item` stats. -/
def bowFor (type : UInt16) (pre : Int) (rng : UnifiedRandom) : Option (OriginalBowUse.Bow × UnifiedRandom) :=
  match actionOf type.toNat with
  | .bow b => (weaponItem type pre rng).map fun (item, rng) => ({ b with stats := item }, rng)
  | _ => none

/-- Damage / crit / knockback of the held weapon as `Player.GetWeaponDamage/Crit/Knockback` compute them from the
player's `Stats` (`OriginalWeaponStats.Stats.ofEffects` of the equipment update, buffs on top). -/
structure WeaponNumbers where
  damage : Int
  crit : Int
  knockback : Float32
deriving Repr, BEq

def weaponNumbers (stats : OriginalWeaponStats.Stats) (item : OriginalWeaponStats.Item) : WeaponNumbers :=
  { damage := OriginalWeaponStats.weaponDamage stats item, crit := OriginalWeaponStats.weaponCrit stats item
    knockback := OriginalWeaponStats.weaponKnockback stats item item.knockBack }

/-! ## Bows (`OriginalBowUse` → `OriginalProjectiles`) -/

structure BowResult (κ : Type) where
  output : OriginalBowUse.Output
  pool : OriginalProjectiles.Pool κ
  /-- The wooden arrow `NewProjectile` placed this frame. -/
  launched : Option OriginalPlayerArrows.Arrow
  /-- A spawn request of a projectile type the pool does not own yet (e.g. Flaming Arrow, type 2). Its slot
  is allocated as `Entry.unowned` (allocation/keys stay exact); the projectile phase reports it as a
  boundary. The caller must route it or stop, never drop it silently. -/
  unsupportedSpawn : Option OriginalBowUse.Spawn

/-- One `Player.ItemCheck` with a bow: the released `OriginalBowUse.itemCheck` (player stream), then in the
same frame `NewProjectile` for its spawn request (no random draws). The arrow updates in this frame's
projectile phase (`OriginalProjectiles.Pool.update`). -/
def useBow {κ : Type} (bow : OriginalBowUse.Bow) (frame : OriginalBowUse.Frame) (state : OriginalBowUse.State)
    (random : UnifiedRandom) (pool : OriginalProjectiles.Pool κ) (myPlayer : Nat) : BowResult κ :=
  let output := OriginalBowUse.itemCheck bow frame state random
  match output.spawn with
  | some s =>
    if s.projectileType == OriginalPlayerArrows.typeId then
      let (pool, arrow) := pool.launchArrow myPlayer s.launch
      { output, pool, launched := some arrow, unsupportedSpawn := none }
    else
      -- natively NewProjectile still takes a slot and bumps slotGenerations; keep that exact.
      -- timeLeft 1200: the arrow aiStyle value seen for type 1 (unverified for other types).
      let (pool, _) := pool.newUnowned myPlayer s.projectileType 1200
      { output, pool, launched := none, unsupportedSpawn := some s }
  | none => { output, pool, launched := none, unsupportedSpawn := none }

/-! ## Buff potions (`OriginalPotionBuffs`, source fe544dcc…) -/

/-- The buff part of `Player.Update` that runs every frame whatever is held: `ResetEffects` (hitbox resize, stats
reset, `buffImmune` clear), `UpdateBuffs`, the expiry loop, the immunity loop. -/
structure BuffUpdate where
  body : OriginalPotionBuffs.Body
  stats : OriginalPotionBuffs.Stats
  lights : Array OriginalPotionBuffs.Light
  slotsAfterUpdate : OriginalCandleBuffs.Slots
  slots : OriginalCandleBuffs.Slots

def buffUpdate (p : OriginalPotionBuffs.Player) : Except OriginalPotionBuffs.OutOfScope BuffUpdate := do
  let body := p.body.resizeHitbox
  let (slots, stats, lights) ← OriginalPotionBuffs.updateBuffs body p.slots p.stats.reset
  let cleaned := OriginalCandleBuffs.clearExpired slots
  let cleaned := OriginalPotionBuffs.removeImmune cleaned (fun t => t == OriginalPotionBuffs.onFire && stats.immuneOnFire)
  return { body, stats, lights, slotsAfterUpdate := slots, slots := cleaned }

/-- `ItemCheck` when the held item is air or a buff potion (`actionOf = .buffPotion`), after `buffUpdate`. -/
def useBuffPotion (p : OriginalPotionBuffs.Player) (u : BuffUpdate) (controlUseItem : Bool) :
    Except OriginalPotionBuffs.OutOfScope OriginalPotionBuffs.Player := do
  let ctx : OriginalPotionBuffs.AddContext :=
    { buffImmune := fun t => t == OriginalPotionBuffs.onFire && u.stats.immuneOnFire }
  let (use, held, slots) ← OriginalPotionBuffs.itemCheck ctx u.stats.noItems { p.use with controlUseItem } p.held u.slots
  return { p with slots, stats := u.stats, use, held, body := u.body }

/-- The whole slice (`OriginalPotionBuffs.frame`) through the two entries above. -/
def buffFrame (p : OriginalPotionBuffs.Player) (controlUseItem : Bool) :
    Except OriginalPotionBuffs.OutOfScope (OriginalPotionBuffs.Player × OriginalPotionBuffs.Trace) := do
  let u ← buffUpdate p
  let after ← useBuffPotion p u controlUseItem
  return (after, { stats := u.stats, lights := u.lights, slotsAfterUpdate := u.slotsAfterUpdate, slotsAfterCleanup := u.slots })

/-! ## Quick heal / quick mana -/

structure QuickKeys where
  controlQuickHeal : Bool
  releaseQuickHeal : Bool
  controlQuickMana : Bool
  releaseQuickMana : Bool
deriving BEq, Repr, Inhabited

structure QuickResult where
  player : OriginalHealingPotions.Player
  rng : OriginalPlayerLiquidHealth.Rng
  keys : QuickKeys
  /-- Ordered effects of the QuickHeal call, then of the QuickMana call. -/
  effects : Array OriginalHealingPotions.Effect

/-- `Player.Update` IL_0d87..0dd3: the QuickHeal gate, then the QuickMana gate. -/
def quickKeys (player : OriginalHealingPotions.Player) (rng : OriginalPlayerLiquidHealth.Rng)
    (keys : QuickKeys) : QuickResult := Id.run do
  let mut player := player
  let mut rng := rng
  let mut keys := keys
  let mut effects : Array OriginalHealingPotions.Effect := #[]
  if keys.controlQuickHeal then
    if keys.releaseQuickHeal then
      let o := OriginalHealingPotions.quickHeal player rng
      player := o.player; rng := o.rng; effects := effects ++ o.effects
    keys := { keys with releaseQuickHeal := false }
  else keys := { keys with releaseQuickHeal := true }
  if keys.controlQuickMana then
    if keys.releaseQuickMana then
      let o := OriginalHealingPotions.quickMana player rng
      player := o.player; rng := o.rng; effects := effects ++ o.effects
    keys := { keys with releaseQuickMana := false }
  else keys := { keys with releaseQuickMana := true }
  return { player, rng, keys, effects }

end Lean4Terria.OriginalPlayerActions
