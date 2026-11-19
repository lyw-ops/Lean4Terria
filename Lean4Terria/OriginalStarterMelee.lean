import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalGore
import Lean4Terria.OriginalItems
import Lean4Terria.OriginalNpcCombatText
import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalStarterMelee.LineOfSight

/-!
# Starter melee weapons: Copper Shortsword (3507) and Wooden Sword (24), Terraria 1.4.5.8 Classic

Hand translation of the vanilla left-click path for the two starting weapons without bonuses
(no prefix, no buffs, no accessories, `meleeSpeed = meleeDamage = 1`, `meleeCrit = 4`, `luck = 0`,
`gravDir = 1`, not mounted, `fullRotation = 0`). Every formula below cites the vanilla method it
comes from; the per-frame differential replays live in `.lake/original-starter-melee/1.4.5.8`.

**One frame of the local player** (`Player.Update` → `ItemCheck`, in this order):
1. `Update` IL_0a54: `attackCD > 0 → attackCD--`; `itemAnimation == 0 → attackCD = 0` (`beginFrame`).
2. `ItemCheck` (`useFrame`): `ItemCheck_AutoReuseLogic` / `TryAllowingItemReuse` (the two weapons have
   `autoReuse = false`; with `Main.SettingsEnabled_AutoReuseAllItems` the player's
   `autoReuseAllWeapons` sets `releaseUseItem = true` every frame); clamp the counters;
   `itemAnimation == 0 → itemAnimationMax = 0`; start when `controlUseItem && releaseUseItem &&
   itemAnimation == 0` (`shoot == 0 → itemRotation = 0`, `ItemCheck_StartActualUse`: `attackCD = 0`,
   `ResetMeleeHitCooldowns`, `ApplyItemAnimation` → `SetItemAnimation(useAnimation, meleeSpeed)`);
   `itemAnimation > 0 → itemAnimation--`; `releaseUseItem = !controlUseItem`; `itemTime > 0 →
   itemTime--`; `AnimatePlayerAndGetItemFrame` (front/back composite arms off, then
   `ItemCheck_ApplyUseStyle` while animating, else the hold style, which does nothing for
   `holdStyle 0`); `ItemCheck_OwnerOnlyCode`: shoot when `shoot > 0 && itemAnimation > 0 &&
   ItemTimeIsZero` (`ItemCheck_Shoot`), melee when `!noMelee && itemAnimation > 0`
   (`ItemCheck_GetMeleeHitbox`, `UpdateMeleeHitCooldowns`, `ItemCheck_MeleeHitNPCs`).
3. The held item uses the item's texture frame (`Item.GetDrawHitbox`): both weapons are a single
   32 × 32 frame (`Content/Images/Item_24.xnb`, `Item_3507.xnb`).

The player position used by `ItemCheck` is the position after this frame's movement (the native
traces show `itemLocation` derived from the recorded end-of-frame position).

**Copper Shortsword stab** (`ProjectileID 938`, `aiStyle 161`, `Projectile.AI_161_RapierStabs`,
`extraUpdates = 1`, 18 × 18, `timeLeft 360`, `penetrate -1`, `tileCollide false`, `ownerHitCheck`):
spawned in `ItemCheck_Shoot` at `RotatedRelativePoint(MountedCenter)` (= the player's center plus
`gfxOffY` when not rotated) with velocity `shootSpeed` toward the cursor; updated twice per frame
in `Main.UpdateWorld_Projectiles` (after NPCs): `ai[0]++`, rotation, opacity, re-centred on the
owner, killed at `ai[0] ≥ 16` (8 frames), then `HandleMovement` (`position += velocity`), then
`Projectile.Damage` against NPCs.

**Hits**: `Player.ProcessHitAgainstNPC` (sword) and `Projectile.Damage_PVE_Inner` (stab) roll
`Main.rand.Next(1, 101) ≤ meleeCrit`, `Main.DamageVar` (`Next(-15, 16)`), the stab adds
`NextVector2FromRectangle(npc.Hitbox)` (two draws), then `NPC.StrikeNPC_Inner` (defense, crit ×2,
`CombatText.NewText` (shared allocation, two draws or three for crit; zero when gated/full), knockback, life); `SetMeleeHitCooldown(npc,
itemAnimation)` + `ApplyAttackCooldown` for the sword, `npc.immune[owner] = 10` for the stab.
Killing an NPC runs `checkDead` / loot, which is outside this module (see `Strike.died`).

Line of sight (`Player.CanHit`, `Projectile.CanHitWithMeleeWeapon`) comes from
`OriginalStarterMelee.LineOfSight` on the caller's tile grid (`none` = open air, always visible).
Tile cutting (`ItemCheck_CutTiles`, `Projectile.CutTiles`) is not part of this module.
-/

namespace Lean4Terria.OriginalStarterMelee

open OriginalCollision (Vector Grid)

instance : Inhabited Vector := ⟨⟨0, 0⟩⟩

/-! ## Numeric helpers (CLR semantics) -/

/-- `conv.i4` of a float32: truncation toward zero. -/
def truncF32 (x : Float32) : Int := x.toInt64.toInt
/-- `conv.i4` of a double. -/
def truncF (x : Float) : Int := x.toInt64.toInt
/-- `(float)int`. -/
def f32 (n : Int) : Float32 := (Int32.ofInt n).toFloat32
/-- `System.Math.Round(double)`: round half to even. -/
def roundHalfEven (x : Float) : Int :=
  let f := x.floor
  let d := x - f
  let n := truncF f
  if d > 0.5 then n + 1 else if d < 0.5 then n else if n % 2 == 0 then n else n + 1
/-- `(float)Math.Atan2((double)y, (double)x)` = `Utils.ToRotation`. -/
def toRotation (v : Vector) : Float32 := (Float.atan2 v.y.toFloat v.x.toFloat).toFloat32
def halfPi : Float32 := 1.5707963705062866
/-- `Utils.GetLerpValue(from, to, t, clamped: true)` (float overload). -/
def lerpValue (fromV toV t : Float32) : Float32 :=
  if fromV < toV then
    if t < fromV then 0 else if t > toV then 1 else (t - fromV) / (toV - fromV)
  else
    if t < toV then 1 else if t > fromV then 0 else (t - fromV) / (toV - fromV)

/-- `Vector2 * float`. -/
def scaleV (v : Vector) (s : Float32) : Vector := ⟨v.x * s, v.y * s⟩

/-- `Rectangle` with `Intersects` (`Left < r.Right && r.Left < Right && Top < r.Bottom && r.Top < Bottom`). -/
structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving BEq, Repr, Inhabited

def Rect.intersects (a b : Rect) : Bool :=
  b.x < a.x + a.w && a.x < b.x + b.w && b.y < a.y + a.h && a.y < b.y + b.h

/-! ## Weapons -/

structure Weapon where
  type : Nat
  useStyle : Nat
  useAnimation : Nat
  useTime : Nat
  damage : Nat
  knockBack : Float32
  shoot : Nat := 0
  shootSpeed : Float32 := 0
  scale : Float32 := 1
  autoReuse : Bool := false
  noMelee : Bool := false
  noUseGraphic : Bool := false
  /-- `Item.GetDrawHitbox`: the texture frame (`Content/Images/Item_<type>.xnb`). -/
  frameWidth : Nat := 32
  frameHeight : Nat := 32
  /-- `ItemID.Sets.UsesBetterMeleeItemLocation[type]` (false for both, see the native traces). -/
  betterMeleeItemLocation : Bool := false
  deriving Repr

/-- `Item.SetDefaults(3507)` (native `item_stats.py`): useStyle 13 (rapier), 13/13 ticks, 5 damage,
knockBack 4, shoot 938, shootSpeed 2.1 (`0x40066666`), scale 0.8, noMelee, noUseGraphic. -/
def copperShortsword : Weapon :=
  { type := 3507, useStyle := 13, useAnimation := 13, useTime := 13, damage := 5, knockBack := 4,
    shoot := 938, shootSpeed := Float32.ofBits 0x40066666, scale := Float32.ofBits 0x3f4ccccd,
    noMelee := true, noUseGraphic := true }

/-- `Item.SetDefaults(24)`: useStyle 1 (swing), 20/20 ticks, 7 damage, knockBack 5, scale 1. -/
def woodenSword : Weapon :=
  { type := 24, useStyle := 1, useAnimation := 20, useTime := 20, damage := 7, knockBack := 5 }

def weaponOf? (type : Nat) : Option Weapon :=
  if type == 3507 then some copperShortsword else if type == 24 then some woodenSword else none

/-! ## Player side -/

/-- The owner's stats and body as `ItemCheck` sees them this frame. -/
structure Owner where
  position : Vector
  width : Nat := 20
  height : Nat := 42
  gfxOffY : Float32 := 0
  /-- `Player.HeightOffsetHitboxCenter` (the mount offset passed to the use styles). -/
  mountOffset : Float32 := 0
  meleeSpeed : Float32 := 1
  meleeDamage : Float32 := 1
  meleeCrit : Int := 4
  meleeScale : Float32 := 1
  /-- Knockback multiplier of `ItemCheck_OwnerOnlyCode` (`1 + kbGlove + 0.5·kbBuff`). -/
  knockBackMultiplier : Float32 := 1
  /-- `Player.autoReuseAllWeapons` (`Main.SettingsEnabled_AutoReuseAllItems`). -/
  autoReuseAllWeapons : Bool := false
  /-- `Player.luck` of this update: `ProcessHitAgainstNPC` IL_02f5 and, for the owned stab,
  `Projectile.Damage_PVE_Inner` IL_0cea pass it to `Main.DamageVar`. -/
  luck : Float32 := 0
  deriving Repr

/-- `Entity.Center` = `MountedCenter` when not mounted. -/
def Owner.center (o : Owner) : Vector :=
  ⟨o.position.x + f32 (o.width / 2 : Nat), o.position.y + f32 (o.height / 2 : Nat)⟩

/-- `RotatedRelativePoint(MountedCenter, false, true)` with `fullRotation = 0`, not sleeping or sitting. -/
def Owner.rotatedCenter (o : Owner) : Vector :=
  let c := o.center
  ⟨c.x, c.y + o.gfxOffY⟩

structure Arm where
  enabled : Bool := false
  /-- `CompositeArmStretchAmount` (0 full, 1 three quarters, 2 quarter, 3 none). -/
  stretch : Nat := 0
  rotation : Float32 := 0
  deriving BEq, Repr

structure Use where
  direction : Int := 1
  itemAnimation : Nat := 0
  itemAnimationMax : Nat := 0
  itemTime : Nat := 0
  itemTimeMax : Nat := 0
  releaseUseItem : Bool := true
  itemRotation : Float32 := 0
  itemLocation : Vector := ⟨0, 0⟩
  frontArm : Arm := {}
  backArm : Arm := {}
  attackCD : Nat := 0
  /-- `meleeNPCHitCooldown[npc]` (decremented every swing frame, may go negative). -/
  hitCooldown : Array Int := Array.replicate 200 0
  deriving Repr

/-- `Player.Update` IL_0a54-0a75, before `ItemCheck`. -/
def Use.beginFrame (u : Use) : Use :=
  let cd := if u.attackCD > 0 then u.attackCD - 1 else 0
  { u with attackCD := if u.itemAnimation == 0 then 0 else cd }

/-- `Player.ChangeDir(dir)`: flips the arms, `itemRotation` and `itemLocation.X` around the mounted center. -/
def Use.changeDir (u : Use) (o : Owner) (dir : Int) : Use :=
  if dir == u.direction then u else
    let cx := o.center.x
    { u with direction := dir
             frontArm := { u.frontArm with rotation := u.frontArm.rotation * -1 }
             backArm := { u.backArm with rotation := u.backArm.rotation * -1 }
             itemRotation := u.itemRotation * -1
             itemLocation := { u.itemLocation with x := cx + (cx - u.itemLocation.x) } }

private def ge333 (anim max : Nat) (k : Float) : Bool := anim.toFloat >= max.toFloat * k

/-- `Player.ItemCheck_ApplyUseStyle` for `useStyle 1` (swing, non-claw) and `13` (rapier). -/
def applyUseStyle (w : Weapon) (o : Owner) (u : Use) : Use :=
  let dir := f32 u.direction
  let fw := w.frameWidth
  let fh := w.frameHeight
  let px := o.position.x
  let py := o.position.y
  let half := f32 o.width * 0.5
  if w.useStyle == 1 then
    let (loc, vec) :=
      if !ge333 u.itemAnimation u.itemAnimationMax 0.333 then
        let n : Float32 := if fw ≥ 92 then 38 else if fw ≥ 64 then 28 else if fw ≥ 52 then 24
          else if fw > 32 then 14 else 10
        ((⟨px + half + (f32 fw * 0.5 - n) * dir, py + 24 + o.mountOffset⟩ : Vector), (⟨-4, 1⟩ : Vector))
      else if !ge333 u.itemAnimation u.itemAnimationMax 0.666 then
        let n : Float32 := if fw ≥ 92 then 38 else if fw ≥ 64 then 28 else if fw ≥ 52 then 24
          else if fw > 32 then 18 else 10
        let m : Float32 := if fh > 64 then 14 else if fh > 52 then 12 else if fh > 32 then 8 else 10
        (⟨px + half + (f32 fw * 0.5 - n) * dir, py + m + o.mountOffset⟩, ⟨-6, -4⟩)
      else
        let n : Float32 := if fw ≥ 92 then 38 else if fw ≥ 64 then 28 else if fw ≥ 52 then 24
          else if fw ≥ 48 then 18 else if fw > 32 then 14 else 6
        let m : Float32 := if fh > 64 then 14 else if fh > 52 then 12 else 10
        (⟨px + half - (f32 fw * 0.5 - n) * dir, py + m + o.mountOffset⟩, ⟨4, -2⟩)
    let loc := if w.betterMeleeItemLocation then ⟨loc.x + vec.x * dir, loc.y + vec.y * 1⟩ else loc
    let rot := ((f32 u.itemAnimation / f32 u.itemAnimationMax) - 0.5) * f32 (-u.direction) * 3.5 - dir * 0.3
    { u with itemLocation := loc, itemRotation := rot }
  else if w.useStyle == 13 then
    let num := if u.itemTimeMax != 0 then u.itemTimeMax else if u.itemAnimationMax != 0 then u.itemAnimationMax
      else w.useAnimation
    let progress : Float32 := 1 - f32 (u.itemAnimation % num : Nat) / f32 num
    let stretch := if progress > 0.66 && progress ≤ 1 then 0
      else if progress > 0.33 && progress ≤ 0.66 then 3 else 2
    { u with frontArm := { enabled := true, stretch, rotation := u.itemRotation * 1 - halfPi * dir } }
  else u

/-- `Player.ItemCheck_GetMeleeHitbox` (client branch, `gravDir = 1`): `(dontAttack, itemRectangle)`. -/
def meleeHitbox (w : Weapon) (o : Owner) (u : Use) : Bool × Rect := Id.run do
  let mut r : Rect := ⟨truncF32 u.itemLocation.x, truncF32 u.itemLocation.y, w.frameWidth, w.frameHeight⟩
  let scale := w.scale * o.meleeScale
  r := { r with w := truncF32 (f32 r.w * scale), h := truncF32 (f32 r.h * scale) }
  if u.direction == -1 then r := { r with x := r.x - r.w }
  r := { r with y := r.y - r.h }
  let anim := u.itemAnimation.toFloat
  let max := u.itemAnimationMax.toFloat
  if w.useStyle == 1 then
    if anim < max * 0.333 then
      if u.direction == -1 then r := { r with x := r.x - truncF (r.w.toFloat * 1.4 - r.w.toFloat) }
      r := { r with w := truncF (r.w.toFloat * 1.4) }
      r := { r with y := r.y + truncF (r.h.toFloat * 0.5 * 1.0) }
      r := { r with h := truncF (r.h.toFloat * 1.1) }
    else if anim >= max * 0.666 then
      if u.direction == 1 then r := { r with x := r.x - truncF (r.w.toFloat * 1.2) }
      r := { r with w := r.w * 2 }
      r := { r with y := r.y - truncF ((r.h.toFloat * 1.4 - r.h.toFloat) * 1.0) }
      r := { r with h := truncF (r.h.toFloat * 1.4) }
  return (false, r)

/-! ## NPC side -/

/-- The NPC fields a hit reads and writes. -/
structure Npc where
  active : Bool := true
  position : Vector
  velocity : Vector := ⟨0, 0⟩
  width : Nat
  height : Nat
  life : Int
  lifeMax : Int
  defense : Int
  knockBackResist : Float32 := 1
  noGravity : Bool := false
  noTileCollide : Bool := false
  friendly : Bool := false
  dontTakeDamage : Bool := false
  immortal : Bool := false
  takenDamageMultiplier : Float32 := 1
  onFire2 : Bool := false
  /-- `immune[owner]` (the local player). -/
  immune : Nat := 0
  justHit : Bool := false
  deriving Repr, Inhabited

/-- `NPC.getRect()` / `Hitbox`: `(int)position`, width, height. -/
def Npc.rect (n : Npc) : Rect := ⟨truncF32 n.position.x, truncF32 n.position.y, n.width, n.height⟩
def Npc.center (n : Npc) : Vector :=
  ⟨n.position.x + f32 (n.width / 2 : Nat), n.position.y + f32 (n.height / 2 : Nat)⟩

def Owner.box (o : Owner) : LineOfSight.Box := ⟨o.position, o.width, o.height⟩
def Npc.box (n : Npc) : LineOfSight.Box := ⟨n.position, n.width, n.height⟩

/-- `Player.CanHit(npc)` (`gravDir = 1`); `none` = no tiles in the way. -/
def playerSees (grid : Option Grid) (o : Owner) (direction : Int) (n : Npc) : Bool :=
  match grid with
  | none => true
  | some g => LineOfSight.playerCanHit g o.box direction 1 n.box

structure Strike where
  npc : Npc
  /-- `StrikeNPC` return value (damage done). -/
  damage : Int
  /-- `Main.DamageVar` result before defense. -/
  rolled : Int
  crit : Bool
  died : Bool
  random : UnifiedRandom
  text : OriginalNpcCombatText.State := {}

/-- `Main.DamageVar(dmg, luck)` (IL `.lake/forest-desktop-integration/death-runtime/il/Main.DamageVar.il`):
`num = dmg * (1 + Next(-15, 16) * 0.01f)`; with `luck > 0` and `NextFloat() < luck` a second roll
replaces it when larger; with `luck < 0` and `NextFloat() < -luck` a second roll replaces it when
smaller; then `(int)Math.Round((double)num)`. `NextFloat` is `(float)NextDouble()`. -/
def damageVar (dmg : Int) (rng : UnifiedRandom) (luck : Float32 := 0) : Int × UnifiedRandom := Id.run do
  let roll := fun (rng : UnifiedRandom) =>
    let (k, rng) := rng.next (-15) 16
    ((f32 dmg * (1 + f32 k * 0.01) : Float32), rng)
  let (first, rng) := roll rng
  let mut num := first
  let mut rng := rng
  if luck > 0 then
    let (sample, next) := rng.sample
    rng := next
    if sample.toFloat32 < luck then
      let (other, next) := roll rng
      rng := next
      if other > num then num := other
  else if luck < 0 then
    let (sample, next) := rng.sample
    rng := next
    if sample.toFloat32 < -luck then
      let (other, next) := roll rng
      rng := next
      if other < num then num := other
  return (roundHalfEven num.toFloat, rng)

/-- `CombatText.NewText`: two `Main.rand` draws, a third for dramatic (crit) text. -/
def combatTextDraws (rng : UnifiedRandom) (dramatic : Bool) : UnifiedRandom :=
  let (_, rng) := rng.next 0 1
  let (_, rng) := rng.next 0 1
  if dramatic then (rng.next 0 1).2 else rng

/-- The soft-capped knockback speed of `StrikeNPC_Inner` (`knockBack × knockBackResist`, on-fire ×1.1, caps at
8/10/12/14/16, crit ×1.4). -/
def knockbackSpeed (n : Npc) (knockBack : Float32) (crit : Bool) : Float32 := Id.run do
  let mut k := knockBack * n.knockBackResist
  if n.onFire2 then k := k * 1.1
  if k > 8 then k := 8 + (k - 8) * 0.9
  if k > 10 then k := 10 + (k - 10) * 0.8
  if k > 12 then k := 12 + (k - 12) * 0.7
  if k > 14 then k := 14 + (k - 14) * 0.6
  if k > 16 then k := 16
  if crit then k := k * 1.4
  return k

/-- `StrikeNPC_Inner` knockback for a hit that would take more than a tenth of `lifeMax`: the speed is
added toward `hitDirection` and capped, and the NPC is lifted. -/
def heavyKnockback (n : Npc) (v : Vector) (k : Float32) (hitDirection : Int) : Vector := Id.run do
  let mut v := v
  if hitDirection < 0 && v.x > -k then
    if v.x > 0 then v := { v with x := v.x - k }
    v := { v with x := v.x - k }
    if v.x < -k then v := { v with x := -k }
  else if hitDirection > 0 && v.x < k then
    if v.x < 0 then v := { v with x := v.x + k }
    v := { v with x := v.x + k }
    if v.x > k then v := { v with x := k }
  let ky := if n.noGravity then k * -0.5 else k * -0.75
  if v.y > ky then
    v := { v with y := v.y + ky }
    if v.y < ky then v := { v with y := ky }
  return v

/-- `NPC.StrikeNPC` → `StrikeNPC_Inner` for a normal NPC (no `realLife`, Classic: `num7 = damage × 10`).
Returns the strike and its shared text state after `CombatText.NewText`. A
font boundary stops before justHit, life, knockback and caller effects. -/
def strikeNpcWithText (n : Npc) (dmg : Int) (knockBack : Float32) (hitDirection : Int) (crit : Bool)
    (rng : UnifiedRandom) (text : OriginalNpcCombatText.State := {}) : Strike :=
  let taken : Float := let v := dmg.toFloat - n.defense.toFloat * 0.5; if v < 1 then 1 else v
  let num0 := truncF (max 1.0 (taken * (if crit then 2 else 1)))
  let num1 := if n.takenDamageMultiplier > 1 then truncF32 (f32 num0 * n.takenDamageMultiplier) else num0
  let num := max 1 num1
  let (text, rng) := text.strike ⟨n.rect.x, n.rect.y, n.rect.w, n.rect.h⟩ n.friendly n.lifeMax dmg num crit rng
  if text.boundary.isSome then
    {npc := n, damage := num, rolled := dmg, crit, died := false, random := rng, text}
  else
    let n := { n with justHit := true, life := if n.immortal then n.life else n.life - num }
    let n :=
      if knockBack > 0 && n.knockBackResist > 0 then
        let k := knockbackSpeed n knockBack crit
        let v :=
          if num * 10 > n.lifeMax then heavyKnockback n n.velocity k hitDirection
          else ⟨k * f32 hitDirection * n.knockBackResist,
                if n.noGravity then -k * 0.5 * n.knockBackResist else -k * 0.75 * n.knockBackResist⟩
        { n with velocity := v }
      else n
    {npc := n, damage := num, rolled := dmg, crit, died := n.life ≤ 0 && !n.immortal, random := rng, text}

/-- Compatibility entry point: no font channel, assumed free text pool. -/
def strikeNpc (n : Npc) (dmg : Int) (knockBack : Float32) (hitDirection : Int) (crit : Bool)
    (rng : UnifiedRandom) : Npc × Int × UnifiedRandom :=
  let out := strikeNpcWithText n dmg knockBack hitDirection crit rng
  (out.npc, out.damage, out.random)

/-- Crit roll, `DamageVar`, optional extra draws (the stab's `NextVector2FromRectangle`), then the strike. -/
def rollAndStrike (n : Npc) (baseDamage : Int) (critChance : Int) (knockBack : Float32) (hitDirection : Int)
    (extraDraws : Nat) (rng : UnifiedRandom) (text : OriginalNpcCombatText.State := {}) (luck : Float32 := 0) :
    Strike := Id.run do
  if text.boundary.isSome then
    return {npc:=n,damage:=0,rolled:=0,crit:=false,died:=false,random:=rng,text}
  let (c, rng0) := rng.next 1 101
  let crit := c ≤ critChance
  let (rolled, rng1) := damageVar baseDamage rng0 luck
  let mut r := rng1
  for _ in [0:extraDraws] do r := (r.next 0 1).2
  return strikeNpcWithText n rolled knockBack hitDirection crit r text

/-! ## The sword swing against NPCs -/

/-- An NPCHit / NPCDeath (or ambient) instance `LegacySoundPlayer` resolved at its original call
site. The values are final: the device neither rolls nor scales them again. -/
structure NpcSound where
  typeId : Nat
  style : Nat
  /-- The asset variation `LegacySoundPlayer` chose (0 for the single-asset NPCHit / NPCDeath). -/
  variation : Nat := 0
  volume : Float32
  pan : Float32
  pitch : Float32
  deriving BEq, Repr

structure MeleeHit where
  index : Nat
  strike : Strike
  drops : Array OriginalItems.Drop := #[]
  dust : Array OriginalForestGuideArrow.DustParticle := #[]
  /-- The Gore `HitEffect` created, in `NewGore` order; the caller places them in its pool. -/
  gore : Array OriginalGore.Particle := #[]
  /-- What a client's sound channel resolved for this strike, in call order. -/
  sounds : Array NpcSound := #[]
  /-- `SoundEngine.PlaySound(LegacySoundStyle, …)` calls of this strike (HitSound, DeathSound):
  each takes one sample from the client-wide `SoundStyle._random`, heard or not. -/
  styleCalls : Nat := 0

/-- Runs the caller's NPC death path before any subsequent hit can draw from Main.rand. -/
abbrev OnKill := Nat → Npc → UnifiedRandom → Array OriginalItems.Drop × UnifiedRandom

private def noKillDrops : OnKill := fun _ _ random => (#[], random)

/-- What one `HitEffect` produced: its dust, the Gore it created (in `NewGore` order, each
already rolled from the stream) and the stream after it. -/
structure StrikeEffects where
  dust : Array OriginalForestGuideArrow.DustParticle := #[]
  gore : Array OriginalGore.Particle := #[]
  random : UnifiedRandom

/-- The local hit effect runs after combat text and before the death path.
The dust from earlier hits is still occupying its slots in this phase. -/
abbrev OnStrike := Array OriginalForestGuideArrow.DustParticle → Nat → Int → Strike → StrikeEffects

def noStrikeEffects : OnStrike := fun _ _ _ strike => { random := strike.random }

/-- The sounds of `StrikeNPC_Inner` for the struck slot: called with `false` for `HitSound`, right
after `HitEffect` (IL_0b1a..0b5d, whenever the NPC has one), and with `true` for `DeathSound` in
`checkDead` before `NPCLoot` (IL_05d0) when the strike killed. Returns what a client resolved,
whether the NPC has that sound at all (a `PlaySound(LegacySoundStyle, …)` call was made), and
the stream after the sound's own draws. -/
abbrev OnSound := Bool → Nat → Npc → UnifiedRandom → Option NpcSound × Bool × UnifiedRandom

def noStrikeSound : OnSound := fun _ _ _ random => (none, false, random)

private def hitResult (onKill : OnKill) (onStrike : OnStrike) (onSound : OnSound)
    (priorDust : Array OriginalForestGuideArrow.DustParticle) (index : Nat)
    (direction : Int) (strike : Strike) : MeleeHit × UnifiedRandom :=
  if strike.text.boundary.isSome then ({index, strike}, strike.random) else
  let effects := onStrike priorDust index direction strike
  let (dust, gore, random) := (effects.dust, effects.gore, effects.random)
  let (hitSound, hitCalled, random) := onSound false index strike.npc random
  let (deathSound, deathCalled, random) :=
    if strike.died then onSound true index strike.npc random else (none, false, random)
  let (drops, random) := if strike.died then onKill index strike.npc random else (#[], random)
  ({ index, strike, drops, dust, gore, sounds := hitSound.toArray ++ deathSound.toArray
     styleCalls := (if hitCalled then 1 else 0) + (if deathCalled then 1 else 0) }, random)

/-- `Player.ItemCheck_MeleeHitNPCs` + `ProcessHitAgainstNPC` for an ordinary hostile NPC.
`Player.CanHit(npc)` on `grid` is asked only when the NPC collides with tiles. -/
def meleeHitNpcsWithText (w : Weapon) (o : Owner) (u : Use) (box : Rect) (npcs : Array Npc) (grid : Option Grid)
    (rng : UnifiedRandom) (onKill : OnKill := noKillDrops) (onStrike : OnStrike := noStrikeEffects)
    (text : OriginalNpcCombatText.State := {}) (onSound : OnSound := noStrikeSound) :
    Use × Array Npc × Array MeleeHit × UnifiedRandom × OriginalNpcCombatText.State := Id.run do
  let mut u := u
  let mut npcs := npcs
  let mut hits := #[]
  let mut rng := rng
  let mut text := text
  let damage : Int := if w.damage > 0 then truncF32 (f32 w.damage * o.meleeDamage + 4.999999873689376e-06) else w.damage
  let knockBack := w.knockBack * o.knockBackMultiplier
  for i in [0:npcs.size] do
    if text.boundary.isSome then break
    let n := npcs[i]!
    if !n.active || n.immune != 0 || u.hitCooldown[i]! > 0 || u.attackCD > 0 then continue
    if n.dontTakeDamage || n.friendly then continue
    if !(n.rect.intersects box) then continue
    if !n.noTileCollide && !playerSees grid o u.direction n then continue
    let s := rollAndStrike n damage o.meleeCrit knockBack u.direction 0 rng text o.luck
    let (hit, next) := hitResult onKill onStrike onSound #[] i u.direction s
    rng := next
    text := s.text
    if text.boundary.isSome then
      hits := hits.push hit
      break
    npcs := npcs.set! i s.npc
    hits := hits.push hit
    u := { u with hitCooldown := u.hitCooldown.set! i u.itemAnimation }
    u := { u with attackCD := (max 1 (truncF (u.itemAnimationMax.toFloat * 0.33))).toNat }
  return (u, npcs, hits, rng, text)

/-- The legacy result shape, with no shared text channel. -/
def meleeHitNpcs (w : Weapon) (o : Owner) (u : Use) (box : Rect) (npcs : Array Npc) (grid : Option Grid)
    (rng : UnifiedRandom) (onKill : OnKill := noKillDrops) (onStrike : OnStrike := noStrikeEffects) :
    Use × Array Npc × Array MeleeHit × UnifiedRandom :=
  let (u, ns, hs, r, _) := meleeHitNpcsWithText w o u box npcs grid rng onKill onStrike
  (u, ns, hs, r)

/-! ## The stab projectile -/

structure Stab where
  active : Bool := true
  position : Vector
  velocity : Vector
  width : Nat := 18
  height : Nat := 18
  timeLeft : Int := 360
  ai0 : Float32 := 0
  rotation : Float32 := 0
  alpha : Int := 0
  direction : Int := 1
  spriteDirection : Int := 1
  damage : Int
  knockBack : Float32
  deriving Repr

def Stab.center (s : Stab) : Vector :=
  ⟨s.position.x + f32 (s.width / 2 : Nat), s.position.y + f32 (s.height / 2 : Nat)⟩
def Stab.rect (s : Stab) : Rect := ⟨truncF32 s.position.x, truncF32 s.position.y, s.width, s.height⟩

/-- `Collision.CheckAABBvAABBCollision(pos1, dim1, pos2, dim2)`. -/
def aabbVsAabb (p1 d1 p2 d2 : Vector) : Bool :=
  p1.x < p2.x + d2.x && p1.y < p2.y + d2.y && p1.x + d1.x > p2.x && p1.y + d1.y > p2.y

/-- `Utils.RotatedBy(v, radians, Vector2.Zero)`: `(float)Math.Cos` / `(float)Math.Sin`. -/
def rotatedBy (v : Vector) (radians : Float) : Vector :=
  let c := radians.cos.toFloat32
  let s := radians.sin.toFloat32
  ⟨0 + (v.x * c - v.y * s), 0 + (v.x * s + v.y * c)⟩

/-- `Collision.CheckAABBvLineCollision(aabbPosition, aabbDimensions, lineStart, lineEnd, lineWidth,
ref collisionPoint)` (MethodDef 205): the result flag (the collision point is not needed here). -/
def aabbVsLine (pos dim a b : Vector) (lineWidth : Float32) : Bool := Id.run do
  let num := lineWidth * 0.5
  let mut p2 := a
  let mut d2 : Vector := ⟨b.x - a.x, b.y - a.y⟩
  if d2.x > 0 then
    d2 := { d2 with x := d2.x + lineWidth }
    p2 := { p2 with x := p2.x - num }
  else
    p2 := { p2 with x := p2.x + (d2.x - num) }
    d2 := { d2 with x := -d2.x + lineWidth }
  if d2.y > 0 then
    d2 := { d2 with y := d2.y + lineWidth }
    p2 := { p2 with y := p2.y - num }
  else
    p2 := { p2 with y := p2.y + (d2.y - num) }
    d2 := { d2 with y := -d2.y + lineWidth }
  if !aabbVsAabb pos dim p2 d2 then return false
  let v3 : Vector := ⟨pos.x - a.x, pos.y - a.y⟩
  let v4 : Vector := ⟨v3.x + dim.x, v3.y + dim.y⟩
  let v5 : Vector := ⟨v3.x, v4.y⟩
  let v6 : Vector := ⟨v4.x, v3.y⟩
  let v7 : Vector := ⟨b.x - a.x, b.y - a.y⟩
  let len : Float32 := (v7.x * v7.x + v7.y * v7.y).toFloat.sqrt.toFloat32
  let ang : Float32 := (Float.atan2 v7.y.toFloat v7.x.toFloat).toFloat32
  let r := (-ang).toFloat
  let corners := #[rotatedBy v3 r, rotatedBy v6 r, rotatedBy v4 r, rotatedBy v5 r]
  let mut point := len
  let mut flag := false
  for p in corners do
    if p.y.abs < num && p.x < point && p.x >= 0 then
      point := p.x
      flag := true
  let v12 : Vector := ⟨0, num⟩
  let v13 : Vector := ⟨len, num⟩
  let v14 : Vector := ⟨0, -num⟩
  let v15 : Vector := ⟨len, -num⟩
  let within (t : Float32) : Bool := !(t < 0) && !(t > 1) && !t.isNaN
  for i in [0:4] do
    let j := (i + 1) % 4
    let ci := corners[i]!
    let cj := corners[j]!
    let v21 : Vector := ⟨cj.x - ci.x, cj.y - ci.y⟩
    let v20 : Vector := ⟨v13.x - v12.x, v13.y - v12.y⟩
    let d := v20.x * v21.y - v20.y * v21.x
    if d != 0 then
      let v23 : Vector := ⟨ci.x - v12.x, ci.y - v12.y⟩
      let n5 := (v23.x * v21.y - v23.y * v21.x) / d
      if within n5 then
        let n6 := (v23.x * v20.y - v23.y * v20.x) / d
        if within n6 then
          flag := true
          point := min point (v12.x + n5 * v20.x)
    let w20 : Vector := ⟨v15.x - v14.x, v15.y - v14.y⟩
    let d' := w20.x * v21.y - w20.y * v21.x
    if d' != 0 then
      let v26 : Vector := ⟨ci.x - v14.x, ci.y - v14.y⟩
      let n7 := (v26.x * v21.y - v26.y * v21.x) / d'
      if within n7 then
        let n8 := (v26.x * w20.y - v26.y * w20.x) / d'
        if within n8 then
          flag := true
          point := min point (v14.x + n7 * w20.x)
  return flag

/-- The box/line test of `Projectile.Colliding` for the rapier stabs. -/
def aabbLine (box : Rect) (a b : Vector) (lineWidth : Float32) : Bool :=
  aabbVsLine ⟨f32 box.x, f32 box.y⟩ ⟨f32 box.w, f32 box.h⟩ a b lineWidth

/-- `Projectile.Colliding` for 938: hitbox intersection, else the stab line `Center → Center + 6·velocity`
of width `10·scale` (`CheckAABBvLineCollision`). -/
def Stab.colliding (s : Stab) (target : Rect) : Bool :=
  s.rect.intersects target || aabbLine target s.center (s.center + scaleV s.velocity 6) 10

/-- `ItemCheck_Shoot` for the shortsword: `(newDirection, spawn)`. `mouse` is `Main.MouseWorld`. -/
def shootStab (w : Weapon) (o : Owner) (mouse : Vector) : Int × Stab :=
  let speed := w.shootSpeed / o.meleeSpeed
  let point := o.rotatedCenter
  let aim : Vector := ⟨mouse.x - point.x, mouse.y - point.y⟩
  -- `Normalize` (skipped for a zero vector) keeps the sign of X; `Dot(UnitX, aim) > 0` → face right, else left
  let dir := if aim.x > 0 then 1 else -1
  let dx := mouse.x - point.x
  let dy := mouse.y - point.y
  let len : Float32 := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let (dx, dy, k) :=
    if (dx.isNaN && dy.isNaN) || (dx == 0 && dy == 0) then (f32 dir, (0 : Float32), speed)
    else (dx, dy, speed / len)
  let damage := truncF32 (f32 w.damage * o.meleeDamage + 4.999999873689376e-06)
  (dir, { position := ⟨point.x - f32 (18 / 2 : Nat), point.y - f32 (18 / 2 : Nat)⟩,
          velocity := ⟨dx * k, dy * k⟩, damage, knockBack := w.knockBack })

abbrev StabHit := MeleeHit

/-- One `Projectile.Update` pass (one of the two per frame). `owner` is the player now (after its update);
`Projectile.CanHitWithMeleeWeapon(npc)` uses `grid` (`ownerHitCheckDistance` 1000, the owner's direction).
Returns `(stab, npcs, hits, rng, heldProj)`. -/
def Stab.updateOnceWithText (s : Stab) (owner : Owner) (ownerDirection : Int) (critChance : Int) (npcs : Array Npc)
    (grid : Option Grid)
    (rng : UnifiedRandom) (onKill : OnKill := noKillDrops) (onStrike : OnStrike := noStrikeEffects)
    (priorDust : Array OriginalForestGuideArrow.DustParticle := #[])
    (text : OriginalNpcCombatText.State := {}) (onSound : OnSound := noStrikeSound) :
    Stab × Array Npc × Array StabHit × UnifiedRandom × Bool × OriginalNpcCombatText.State := Id.run do
  if !s.active || text.boundary.isSome then return (s, npcs, #[], rng, false, text)
  -- AI_161_RapierStabs
  let mut s := { s with rotation := toRotation s.velocity + halfPi, ai0 := s.ai0 + 1 }
  let opacity := lerpValue 0 7 s.ai0 * lerpValue 16 12 s.ai0
  let a := (1 - opacity) * 255
  s := { s with alpha := truncF32 (if a > 255 then 255 else if a < 0 then 0 else a) }
  let c := owner.rotatedCenter + scaleV s.velocity (s.ai0 - 1)
  s := { s with position := ⟨c.x - f32 (s.width / 2 : Nat), c.y - f32 (s.height / 2 : Nat)⟩ }
  s := { s with spriteDirection := if s.velocity.x * 1 + s.velocity.y * 0 >= 0 then 1 else -1 }
  let killed := !(s.ai0 < 16)
  if killed then s := { s with active := false, timeLeft := 0 }
  -- HandleMovement (tileCollide = false), then AutomaticallyChangesDirection
  s := { s with position := s.position + s.velocity, direction := if s.velocity.x < 0 then -1 else 1 }
  if !s.active then return (s, npcs, #[], rng, false, text)
  -- Damage → Damage_PVE → Damage_PVE_Inner
  let mut npcs := npcs
  let mut hits := #[]
  let mut rng := rng
  let mut text := text
  let mut dust := priorDust
  for i in [0:npcs.size] do
    if text.boundary.isSome then break
    let n := npcs[i]!
    if !n.active || n.dontTakeDamage || n.friendly || n.immune != 0 then continue
    let sees := match grid with
      | none => true
      | some g => LineOfSight.projectileCanHitWithMeleeWeapon g ⟨s.position, s.width, s.height⟩ 1000 owner.box
          ownerDirection 1 n.box
    if !n.noTileCollide && !sees then continue
    if !s.colliding n.rect then continue
    let st := rollAndStrike n s.damage critChance s.knockBack s.direction 2 rng text owner.luck
    let (hit, next) := hitResult onKill onStrike onSound dust i s.direction st
    rng := next
    text := st.text
    if text.boundary.isSome then
      return (s, npcs, hits.push hit, rng, true, text)
    dust := dust ++ hit.dust
    npcs := npcs.set! i { st.npc with immune := 10 }
    hits := hits.push hit
  s := { s with timeLeft := s.timeLeft - 1 }
  if s.timeLeft ≤ 0 then s := { s with active := false }
  return (s, npcs, hits, rng, true, text)

/-- Legacy result shape. -/
def Stab.updateOnce (s : Stab) (owner : Owner) (ownerDirection : Int) (critChance : Int) (npcs : Array Npc)
    (grid : Option Grid) (rng : UnifiedRandom) (onKill : OnKill := noKillDrops)
    (onStrike : OnStrike := noStrikeEffects) (priorDust : Array OriginalForestGuideArrow.DustParticle := #[]) :
    Stab × Array Npc × Array StabHit × UnifiedRandom × Bool :=
  let (s, ns, hs, r, held, _) := s.updateOnceWithText owner ownerDirection critChance npcs grid rng onKill onStrike priorDust
  (s, ns, hs, r, held)

/-- Two projectile passes; text allocation from the first pass precedes all
second-pass hit effects and allocations. -/
def Stab.updateFrameWithText (s : Stab) (owner : Owner) (ownerDirection : Int) (critChance : Int) (npcs : Array Npc)
    (grid : Option Grid) (rng : UnifiedRandom) (onKill : OnKill := noKillDrops)
    (onStrike : OnStrike := noStrikeEffects) (text : OriginalNpcCombatText.State := {})
    (onSound : OnSound := noStrikeSound) :
    Stab × Array Npc × Array StabHit × UnifiedRandom × Bool × OriginalNpcCombatText.State := Id.run do
  let (s, npcs, h1, rng, held1, text) := s.updateOnceWithText owner ownerDirection critChance npcs grid rng onKill onStrike #[] text onSound
  if !s.active || text.boundary.isSome then return (s, npcs, h1, rng, held1, text)
  let (s, npcs, h2, rng, held2, text) :=
    s.updateOnceWithText owner ownerDirection critChance npcs grid rng onKill onStrike (h1.flatMap (·.dust)) text onSound
  return (s, npcs, h1 ++ h2, rng, held1 || held2, text)

/-- Legacy result shape. -/
def Stab.updateFrame (s : Stab) (owner : Owner) (ownerDirection : Int) (critChance : Int) (npcs : Array Npc)
    (grid : Option Grid) (rng : UnifiedRandom) (onKill : OnKill := noKillDrops)
    (onStrike : OnStrike := noStrikeEffects) :
    Stab × Array Npc × Array StabHit × UnifiedRandom × Bool :=
  let (s, ns, hs, r, held, _) := s.updateFrameWithText owner ownerDirection critChance npcs grid rng onKill onStrike
  (s, ns, hs, r, held)

/-! ## One ItemCheck frame -/

structure Frame where
  use : Use
  started : Bool := false
  stab : Option Stab := none
  /-- The swing hitbox when `ItemCheck_MeleeHitNPCs` ran this frame. -/
  hitbox : Option Rect := none
  hits : Array MeleeHit := #[]
  npcs : Array Npc
  random : UnifiedRandom
  text : OriginalNpcCombatText.State := {}

/-- `ItemCheck` for the two weapons (after `beginFrame`). `mouse` is `Main.MouseWorld`, `grid` is the tile grid
for `Player.CanHit` (`none` = open air), `rng` is `Main.rand` (the `UpdatePlayers` stream) at the start of `ItemCheck`. -/
def useFrame (w : Weapon) (o : Owner) (u : Use) (controlUseItem : Bool) (mouse : Vector) (npcs : Array Npc)
    (grid : Option Grid) (rng : UnifiedRandom) (onKill : OnKill := noKillDrops)
    (onStrike : OnStrike := noStrikeEffects) (text : OriginalNpcCombatText.State := {})
    (onSound : OnSound := noStrikeSound) : Frame := Id.run do
  if text.boundary.isSome then return {use:=u,npcs,random:=rng,text}
  let mut u := u
  -- ItemCheck_AutoReuseLogic / TryAllowingItemReuse
  if w.autoReuse then u := { u with releaseUseItem := true }
  if o.autoReuseAllWeapons && w.damage > 0 then u := { u with releaseUseItem := true }
  if u.itemAnimation == 0 then u := { u with itemAnimationMax := 0 }
  let started := controlUseItem && u.releaseUseItem && u.itemAnimation == 0 && w.useStyle != 0
  if started then
    if w.shoot == 0 then u := { u with itemRotation := 0 }
    let n := max 1 (truncF32 (f32 w.useAnimation * o.meleeSpeed)) |>.toNat
    u := { u with attackCD := 0, hitCooldown := Array.replicate 200 0, itemAnimation := n, itemAnimationMax := n }
  if u.itemAnimation > 0 then u := { u with itemAnimation := u.itemAnimation - 1 }
  u := { u with releaseUseItem := !controlUseItem }
  if u.itemTime > 0 then u := { u with itemTime := u.itemTime - 1 }
  -- AnimatePlayerAndGetItemFrame
  u := { u with frontArm := { u.frontArm with enabled := false }, backArm := { u.backArm with enabled := false } }
  if u.itemAnimation > 0 then u := applyUseStyle w o u
  -- ItemCheck_OwnerOnlyCode
  let mut stab := none
  if w.shoot > 0 && u.itemAnimation > 0 && u.itemTime == 0 then
    u := { u with itemTime := w.useTime, itemTimeMax := w.useTime }
    let (dir, s) := shootStab w o mouse
    u := u.changeDir o dir
    -- ItemCheck_Shoot IL_0b9d (useStyle 13): itemRotation = (float)Atan2(vy·dir, vx·dir) − fullRotation
    if w.useStyle == 13 then
      let d := f32 u.direction
      u := { u with itemRotation := (Float.atan2 (s.velocity.y * d).toFloat (s.velocity.x * d).toFloat).toFloat32 - 0 }
    stab := some s
  let mut hitbox := none
  let mut hits := #[]
  let mut npcs := npcs
  let mut rng := rng
  let mut text := text
  if !w.noMelee && u.itemAnimation > 0 then
    let (dontAttack, box) := meleeHitbox w o u
    if !dontAttack && w.damage > 0 then
      u := { u with hitCooldown := u.hitCooldown.map (· - 1) }
      let (u', npcs', hs, rng', text') := meleeHitNpcsWithText w o u box npcs grid rng onKill onStrike text onSound
      u := u'
      npcs := npcs'
      hits := hs
      rng := rng'
      text := text'
      hitbox := some box
  return { use := u, started, stab, hitbox, hits, npcs, random := rng, text }

end Lean4Terria.OriginalStarterMelee
