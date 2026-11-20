import Lean4Terria.OriginalSelectedItemState
import Lean4Terria.OriginalPlayerEquipment
import Lean4Terria.OriginalWeaponStats
import Lean4Terria.OriginalPlayerArrows

/-!
# Wooden Bow use (Terraria desktop 1.4.5.8 Classic)

One `Player.ItemCheck()` of the local player holding a bow (useStyle 5, `useAmmo` = arrows, no autoReuse),
on the `UpdatePlayers` stream, translated from the shipped IL:

* `ItemCheck` IL_0000..IL_0880: CCed early return, `GetWeaponDamage`, clamps, the per-frame
  `Main.rand.Next(3)` of the non-revolver branch (IL_02f4: on 0, `revolverCritChanceBonus -= 2`), the start
  of a use (`ItemCheck_TryStartUse` → `ItemCheck_CheckCanUse_Inner` → `HasAmmo`, then
  `ItemCheck_StartActualUse` → `ApplyItemAnimation`), the animation / item-time countdown,
  `pendingItemReuse`, `releaseUseItem`;
* `ItemCheck_OwnerOnlyCode` IL_0246..IL_02fd: shoot when `itemAnimation > 0` and `itemTime == 0`;
* `ItemCheck_Shoot` (the path a bow takes: `PickAmmo`, `GetWeaponKnockback`, `ApplyItemTime`,
  `RotatedRelativePoint(MountedCenter)`, `ChangeDir` from the mouse, the useStyle-5 `itemRotation`, the
  velocity, then the generic `NewProjectile` at IL_83c6);
* `PickAmmo`, `PickAmmo_PickAmmoItem`, `PickAmmo_IterateRange` (ammo cycling mode 0),
  `PickAmmo_TryFindingSpecificMatches` (no bow entry), the conservation rolls and the consumption.

The spawn is handed over as an `OriginalPlayerArrows.Launch` (the arrow module allocates the slot and owns
the flight). Sound: `StartActualUse` plays the item's use sound, `PlaySound(UseSound, Center, useSoundPitch, 1)`.
That call always takes one `SoundStyle._random` sample (client-wide, not `Main.rand`), and an audible Item_5 also draws
its pitch `Main.rand.Next(-6, 7)` from the current (`UpdatePlayers`) stream, between the per-frame `Next(3)` and the
ammo rolls of the same frame. `itemCheckM` hands the request to the caller at exactly that point (`playUseSound` is
the original channel for it); `itemCheck` is the silent case. Evidence: `.lake/original-bow-use/` and, for the
audible sound, `.lake/original-bow-use/audio/`.
-/
namespace Lean4Terria.OriginalBowUse

open OriginalCollision (Vector single)
open OriginalWeaponStats (Item Stats weaponDamage weaponDamageMultiplier weaponKnockback)

/-! ## Items -/

/-- An inventory slot (`Item.type`, `Item.stack`). -/
structure Slot where
  type : Nat := 0
  stack : Int := 0
  deriving Repr, BEq, Inhabited

/-- The bow fields `ItemCheck` reads beyond `OriginalWeaponStats.Item` (damage, use times, shootSpeed,
knockBack, useAmmo, ranged kind come from there, with the prefix applied by `OriginalWeaponStats`). -/
structure Bow where
  stats : Item
  shoot : Nat := 1
  useStyle : Nat := 5
  autoReuse : Bool := false
  useSound : Bool := true
  deriving Inhabited

/-- `SetDefaults(39)`: Wooden Bow. -/
def woodenBow : Option Bow := do
  let stats ← Item.base 39
  return { stats }

/-- The ammo fields `PickAmmo` reads. `shoot`/`consumable` are not part of `OriginalWeaponStats.Item`. -/
structure Ammo where
  stats : Item
  shoot : Nat
  consumable : Bool := true
  deriving Inhabited

/-- Arrows whose `SetDefaults` was read natively: Wooden Arrow (40 → projectile 1), Flaming Arrow (41 → 2). -/
def ammo? (type : Nat) : Option Ammo := do
  let shoot ← match type with | 40 => some 1 | 41 => some 2 | _ => none
  let stats ← Item.base type.toUInt16
  return { stats, shoot }

/-- `Player.AmmoSlotOrder_Default` (static initialiser, read natively): ammo slots 50..57, then 0..53. -/
def ammoOrder : Array Nat := (List.range 8).toArray.map (· + 50) ++ (List.range 54).toArray

/-- `HasAmmo`: any of inventory 0..57 (not the cursor slot 58) with `ammo == useAmmo` and `stack > 0`. -/
def hasAmmo (inventory : Array Slot) (useAmmo : Int) : Bool :=
  (List.range 58).any fun i =>
    let s := inventory.getD i {}
    s.stack > 0 && ((ammo? s.type).map (·.stats.ammo == useAmmo)).getD false

/-- `PickAmmo_PickAmmoItem` with `ammoCyclingMode = 0`: the first slot in `ammoOrder`. -/
def pickAmmoSlot (inventory : Array Slot) (useAmmo : Int) : Option (Nat × Ammo) :=
  ammoOrder.toList.findSome? fun i =>
    let s := inventory.getD i {}
    match ammo? s.type with
    | some a => if a.stats.ammo == useAmmo && s.stack > 0 then some (i, a) else none
    | none => none

/-! ## Player -/

/-- Player fields `ItemCheck` carries from frame to frame. -/
structure State where
  itemAnimation : Int := 0
  itemAnimationMax : Int := 0
  itemTime : Int := 0
  itemTimeMax : Int := 0
  reuseDelay : Int := 0
  releaseUseItem : Bool := false
  pendingItemReuse : Bool := false
  channel : Bool := false
  lastItemUseAttemptSuccess : Bool := false
  attackCD : Int := 0
  direction : Int := 1
  itemRotation : Float32 := 0
  revolverCritChanceBonus : Int := 0
  /-- inventory 0..58 (58 = the cursor item). -/
  inventory : Array Slot := Array.replicate 59 {}
  deriving Repr

/-- Accessory / buff flags `PickAmmo` reads (as `ResetEffects` + equipment leave them this frame). -/
structure AmmoFlags where
  magicQuiver : Bool := false
  ammoBox : Bool := false
  ammoPotion : Bool := false
  huntressAmmoCost90 : Bool := false
  chloroAmmoCost80 : Bool := false
  ammoCost80 : Bool := false
  ammoCost75 : Bool := false
  archery : Bool := false
  deriving Repr, Inhabited

/-- Everything else one frame reads. Scope: local player, no mount/pulley/sleeping/sitting/portable stool,
`fullRotation = 0`, singleplayer, no buffered hotbar change, no pending inventory action. -/
structure Frame where
  controlUseItem : Bool
  /-- `frozen || webbed || stoned`. -/
  ccd : Bool := false
  mouseX : Int
  mouseY : Int
  screenPosition : Vector
  screenHeight : Int
  position : Vector
  width : Nat := 20
  height : Nat := 42
  gfxOffY : Float32 := 0
  gravDir : Float32 := 1
  stats : Stats := {}
  flags : AmmoFlags := {}

/-- What `ItemCheck_Shoot` hands to `Projectile.NewProjectile` (ai 0, 0, 0; the source is
`EntitySource_ItemUse_WithAmmo(player, bow, usedAmmoItemId)`). -/
structure Spawn where
  launch : OriginalPlayerArrows.Launch
  projectileType : Nat
  usedAmmoItemId : Nat
  ammoSlot : Nat
  deriving Repr

structure Output where
  state : State
  random : UnifiedRandom
  /-- `ItemCheck_TryStartUse` ran this frame, with its result. -/
  startAttempt : Option Bool := none
  /-- `ItemCheck_StartActualUse` requested the bow's use sound (pitch from `SoundStyle._random`). -/
  useSound : Bool := false
  spawn : Option Spawn := none
  /-- `ItemCheck_Shoot` ran but `PickAmmo` found nothing (no time applied, no projectile). -/
  dryShot : Bool := false

private def roll (random : UnifiedRandom) (n : Int) : Bool × UnifiedRandom :=
  let (v, random) := random.next 0 n
  (v == 0, random)

structure Picked where
  slot : Nat
  ammo : Ammo
  projectile : Nat
  speed : Float32
  damage : Int
  knockback : Float32
  inventory : Array Slot
  random : UnifiedRandom

/-- `PickAmmo` for a bow (`dontConsume = false`; no type-specific launcher branch applies). -/
def pickAmmo (bow : Bow) (stats : Stats) (flags : AmmoFlags) (inventory : Array Slot) (weaponDamage : Int)
    (random : UnifiedRandom) : Option Picked := do
  let (slot, ammo) ← pickAmmoSlot inventory bow.stats.useAmmo
  let projectile := if ammo.shoot > 0 then ammo.shoot else bow.shoot
  let mut speed := bow.stats.shootSpeed + ammo.stats.shootSpeed
  let mut kb := bow.stats.knockBack
  let isArrow := ammo.stats.ammo == 40
  let arrowLauncher := bow.stats.useAmmo == 40
  if flags.magicQuiver && arrowLauncher then
    kb := kb * 1.1
    speed := speed * 1.1
  let num := ammo.stats.damage
  let mut damage := weaponDamage
  if num > 0 then
    damage := damage + (Float32.ofInt num * weaponDamageMultiplier stats ammo.stats).toInt32.toInt
  if isArrow && flags.archery && speed < 20 then
    speed := speed * 1.2
    if speed > 20 then speed := 20
  kb := kb + ammo.stats.knockBack
  -- The conservation rolls, each drawn whatever an earlier one decided (IL_0362..IL_0518).
  let mut random := random
  let mut save := false
  for (on, n) in [(flags.magicQuiver && arrowLauncher, 5), (flags.ammoBox, 5), (flags.ammoPotion, 5),
      (flags.huntressAmmoCost90, 10), (flags.chloroAmmoCost80, 5), (flags.ammoCost80, 5), (flags.ammoCost75, 4)] do
    if on then
      let (hit, next) := roll random n
      random := next
      if hit then save := true
  let mut inventory := inventory
  if !save && ammo.consumable then
    let s := inventory[slot]!
    inventory := inventory.set! slot (if s.stack - 1 ≤ 0 then {} else { s with stack := s.stack - 1 })
  return { slot, ammo, projectile, speed, damage, knockback := kb, inventory, random }

/-- `RotatedRelativePoint(MountedCenter, false, true)` with `fullRotation = 0` and no mount, in the
original's order of Single operations. -/
def shootPoint (frame : Frame) : Vector :=
  let half := single (frame.width / 2 : Nat)
  let center : Vector := ⟨frame.position.x + half, frame.position.y + single frame.height / 2 + 0⟩
  let anchor : Vector := ⟨frame.position.x + single frame.width / 2 + 0, frame.position.y + single frame.height + frame.gfxOffY⟩
  let c : Vector := ⟨center.x, center.y + frame.gfxOffY⟩
  -- loc1 + RotatedBy(c − loc1, 0) + ((0, −4) + RotatedBy((0, 4), 0))
  let dx := c.x - anchor.x
  let dy := c.y - anchor.y
  let rx : Float32 := 0 + (dx * 1 - dy * 0)
  let ry : Float32 := 0 + (dx * 0 + dy * 1)
  let ox : Float32 := 0 + (0 + ((0 : Float32) * 1 - (4 : Float32) * 0))
  let oy : Float32 := -4 + (0 + ((0 : Float32) * 0 + (4 : Float32) * 1))
  ⟨anchor.x + rx + ox, anchor.y + ry + oy⟩

/-! ## The use sound -/

/-- `ItemCheck_StartActualUse` IL_013c..0153: `SoundEngine.PlaySound(item.UseSound, Center, item.useSoundPitch, 1f)`.
`position` is `Entity.Center` (`position + (width / 2f, height / 2f)`); the engine truncates it to ints. -/
structure UseSound where
  typeId : Nat := 2
  style : Nat := 5
  position : Vector
  pitchOffset : Float32 := 0
  deriving Repr

def useSoundAt (frame : Frame) : UseSound :=
  { position := ⟨frame.position.x + single (frame.width : Nat) / 2, frame.position.y + single (frame.height : Nat) / 2⟩ }

/-- `LegacySoundPlayer.PlaySound` type 2 style 5 (Item_5) with ready assets, as the caller's sound channel:
gates (`dedServ`, audio, `soundVolume`, world generation, server), positional volume and pan, then the pitch
`Main.rand.Next(-6, 7) × 0.01` (IL_0730) from `random`, plus `pitchOffset`, clamped to ±1. The previous Item_5 instance is
stopped (no draw). `none`: not heard, nothing drawn. Only style 5 is covered. -/
def playUseSound (context : OriginalTileHitEffects.Context) (sound : UseSound) (random : UnifiedRandom) :
    Except String (Option OriginalStarterMelee.NpcSound × UnifiedRandom) := do
  unless sound.typeId == 2 && sound.style == 5 do throw s!"use sound {sound.typeId}/{sound.style} not covered"
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then return (none, random)
  if context.generating || context.netMode == 2 then return (none, random)
  let dx := single sound.position.x.toInt32.toInt - context.cameraCenter.x
  let dy := single sound.position.y.toInt32.toInt - context.cameraCenter.y
  let distance := Float32.sqrt (dx * dx + dy * dy)
  if !(distance < context.attenuationDistance) then return (none, random)
  let clamp (lo hi v : Float32) : Float32 := if v < lo then lo else if v > hi then hi else v
  let pan := clamp (-1) 1 (dx / (context.maxWorldViewWidth * 0.5))
  let volume := min 1 (clamp 0 1 (1 - distance / context.attenuationDistance) * context.soundVolume)
  if !(volume > 0) then return (none, random)
  let (pitch, random) := random.next (-6) 7
  return (some { typeId := 2, style := 5, volume, pan, pitch := clamp (-1) 1 (single pitch * 0.01 + sound.pitchOffset) }, random)

/-- `ItemCheck_Shoot` for the bow (`shouldApplyItemTime = true`). -/
def shoot (bow : Bow) (frame : Frame) (state : State) (weaponDamage : Int) (random : UnifiedRandom) :
    State × UnifiedRandom × Option Spawn := Id.run do
  let some p := pickAmmo bow frame.stats frame.flags state.inventory weaponDamage random
    | return (state, random, none)
  let kb := weaponKnockback frame.stats bow.stats p.knockback
  let mut state := { state with inventory := p.inventory, itemTime := bow.stats.useTime, itemTimeMax := bow.stats.useTime }
  let point := shootPoint frame
  -- ChangeDir from Main.MouseWorld − point (Dot with UnitX: the sign of x; zero or NaN → −1)
  let worldY := if frame.gravDir == -1 then frame.screenPosition.y + single frame.screenHeight - single frame.mouseY
    else single frame.mouseY + frame.screenPosition.y
  let aimX := single frame.mouseX + frame.screenPosition.x - point.x
  let aimY := worldY - point.y
  let len := (aimX * aimX + aimY * aimY).sqrt
  let dot := if aimX == 0 && aimY == 0 then aimX else aimX * (1 / len)
  state := { state with direction := if dot > 0 then 1 else -1 }
  -- Velocity (IL_063f..IL_0781)
  let mut vx := single frame.mouseX + frame.screenPosition.x - point.x
  let mut vy := single frame.mouseY + frame.screenPosition.y - point.y
  if frame.gravDir == -1 then
    vy := frame.screenPosition.y + single frame.screenHeight - single frame.mouseY - point.y
  let dist : Float32 := (vx * vx + vy * vy).toFloat.sqrt.toFloat32
  let mut scale := p.speed / dist
  if (vx.isNaN && vy.isNaN) || (vx == 0 && vy == 0) then
    vx := single state.direction
    vy := 0
    scale := p.speed
  vx := vx * scale
  vy := vy * scale
  -- useStyle 5 rotation (IL_0b30)
  let dir := single state.direction
  state := { state with itemRotation := (Float.atan2 (vy * dir).toFloat (vx * dir).toFloat).toFloat32 - 0 }
  let launch : OriginalPlayerArrows.Launch := { center := point, velocity := ⟨vx, vy⟩, damage := p.damage, knockback := kb }
  return (state, p.random, some { launch, projectileType := p.projectile, usedAmmoItemId := p.ammo.stats.type.toNat, ammoSlot := p.slot })

/-- One `Player.ItemCheck()` of the local player holding `bow` in the selected slot, with the use sound performed by
`play` at its original place (after `ApplyItemAnimation`, before the countdown and `ItemCheck_Shoot`): `play` gets the
request and the `UpdatePlayers` stream and returns the stream after the sound's own draws. -/
def itemCheckM [Monad m] (bow : Bow) (frame : Frame) (state : State) (random : UnifiedRandom)
    (play : UseSound → UnifiedRandom → m UnifiedRandom) : m Output := do
  let mut state := { state with pendingItemReuse := false }
  if frame.ccd then
    return { state := { state with channel := false, itemAnimationMax := 0, itemAnimation := 0 }, random }
  let weaponDamage := weaponDamage frame.stats bow.stats
  if state.itemAnimation < 0 then state := { state with itemAnimation := 0 }
  if state.itemTime < 0 then state := { state with itemTime := 0 }
  if state.itemAnimation == 0 then state := { state with itemAnimationMax := 0 }
  -- IL_01ee: reuseDelay > 0 with no animation (a bow has reuseDelay 0)
  if state.itemAnimation == 0 && state.reuseDelay > 0 then
    state := { state with itemAnimation := state.reuseDelay, itemTime := state.reuseDelay, reuseDelay := 0 }
  -- IL_02f4: not a Revolver
  let (hit, random) := roll random 3
  let mut random := random
  if hit then state := { state with revolverCritChanceBonus := state.revolverCritChanceBonus - 2 }
  let mut out : Output := { state, random }
  -- IL_0451: start a use
  if frame.controlUseItem && state.releaseUseItem && state.itemAnimation == 0 && bow.useStyle != 0 then
    let canUse := hasAmmo state.inventory bow.stats.useAmmo
    out := { out with startAttempt := some canUse }
    state := { state with lastItemUseAttemptSuccess := canUse }
    if canUse then
      let animation := bow.stats.useAnimation +
        (if bow.autoReuse && bow.stats.reuseDelay == 0 && bow.stats.useTime ≤ bow.stats.useAnimation &&
            bow.shoot > 0 && bow.useStyle == 5 then 1 else 0)
      state := { state with attackCD := 0, itemAnimation := animation, itemAnimationMax := animation, reuseDelay := bow.stats.reuseDelay }
      out := { out with useSound := bow.useSound }
      if bow.useSound then random ← play (useSoundAt frame) random
  if !frame.controlUseItem then state := { state with channel := false }
  if state.itemAnimation > 0 then
    state := { state with itemAnimation := state.itemAnimation - 1 }
    if state.itemAnimation == 0 && state.reuseDelay == 0 && frame.controlUseItem && state.releaseUseItem then
      state := { state with pendingItemReuse := true }
  state := { state with releaseUseItem := !frame.controlUseItem }
  if state.itemTime > 0 then state := { state with itemTime := state.itemTime - 1 }
  -- ItemCheck_OwnerOnlyCode IL_0246: shoot
  if state.itemAnimation > 0 && state.itemTime == 0 && bow.shoot > 0 then
    let (next, rnd, spawn) := shoot bow frame state weaponDamage random
    state := next
    random := rnd
    out := { out with spawn, dryShot := spawn.isNone }
  return { out with state, random }

/-- `itemCheckM` with an inaudible use sound (no `Main.rand` draw), as before. -/
def itemCheck (bow : Bow) (frame : Frame) (state : State) (random : UnifiedRandom) : Output :=
  Id.run (itemCheckM bow frame state random fun _ random => pure random)

end Lean4Terria.OriginalBowUse
