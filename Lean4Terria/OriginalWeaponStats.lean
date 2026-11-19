import Lean4Terria.OriginalWeaponStats.Model
import Lean4Terria.OriginalWeaponStats.Tables
import Lean4Terria.OriginalPlayerEquipment
import Lean4Terria.UnifiedRandom

/-!
# Weapon stats: explicit prefixes and the `GetWeapon*` methods (desktop 1.4.5.8)

Translated from the shipped IL:

* `Item.Prefix(int)` (→ `Item.Prefix(int, ref bool)`) for an explicit prefix id, including the path
  where the item rejects the prefix and the original falls back to a random roll of the item's own
  prefix pool on `Main.rand`;
* `Item.TryGetPrefixStatMultipliersForItem`, `Item.CanHavePrefixes`, `Item.RollAPrefix`;
* `Player.GetWeaponDamage`, `GetWeaponDamageMultiplier`, `GetWeaponCrit`, `GetWeaponKnockback`.

Natural rolls (`Prefix` with -1 or -2, reduced natural chance) stay with the crafting and loot owners;
`prefix` answers `none` for those modes.
-/

namespace Lean4Terria.OriginalWeaponStats

open Lean4Terria OriginalPlayerEquipment

/-! ## The item -/

/-- Which `Player` damage factor a ranged weapon uses (`GetWeaponDamageMultiplier`). -/
inductive RangedKind where
  | plain | arrow | bullet | specialist
  deriving BEq, Repr, Inhabited

/-- The fields of `Terraria.Item` these methods read or write. -/
structure Item where
  type : UInt16
  /-- `Item.prefix`. -/
  prefixId : UInt8 := 0
  damage : Int := 0
  useAnimation : Int := 0
  useTime : Int := 0
  reuseDelay : Int := 0
  mana : Int := 0
  knockBack : Float32 := 0
  scale : Float32 := 1
  shootSpeed : Float32 := 0
  crit : Int := 0
  bonusTagDamage : Int := 0
  armorPenetration : Int := 0
  rare : Int := 0
  value : Int := 0
  melee : Bool := false
  ranged : Bool := false
  magic : Bool := false
  summon : Bool := false
  accessory : Bool := false
  vanity : Bool := false
  useAmmo : Int := 0
  ammo : Int := 0
  /-- Index into `Tables.pools` of the category `Item.GetRollablePrefixes` returns; `none` for null. -/
  pool : Option Nat := none
  rangedKind : RangedKind := .plain
  deriving Inhabited

private def parseItem (line : String) : Option (Nat × Item) :=
  match (line.splitOn ",").map String.toInt? with
  | [some id, some type, some damage, some animation, some time, some delay, some mana, some kb, some scale, some speed,
      some crit, some tag, some pen, some rare, some value, some flags, some useAmmo, some ammo, some pool, some kind] =>
    some (id.toNat, {
      type := type.toNat.toUInt16, damage, useAnimation := animation, useTime := time, reuseDelay := delay, mana,
      knockBack := Float32.ofBits kb.toNat.toUInt32, scale := Float32.ofBits scale.toNat.toUInt32,
      shootSpeed := Float32.ofBits speed.toNat.toUInt32, crit, bonusTagDamage := tag, armorPenetration := pen,
      rare, value, melee := flags % 2 == 1, ranged := flags / 2 % 2 == 1, magic := flags / 4 % 2 == 1,
      summon := flags / 8 % 2 == 1, accessory := flags / 16 % 2 == 1, vanity := flags / 32 % 2 == 1,
      useAmmo, ammo, pool := if pool < 0 then none else some pool.toNat,
      rangedKind := match kind with | 1 => .arrow | 2 => .bullet | 3 => .specialist | _ => .plain })
  | _ => none

private def baseItems : Array (Option Item) :=
  let rows := (Tables.itemRows.splitOn "\n").toArray.filterMap parseItem
  rows.foldl (init := Array.replicate Tables.itemCount none) fun table (id, item) => table.set! id (some item)

/-- `Item.SetDefaults(id)` for the fields modelled here; `none` for id 0 and unknown ids. A few legacy ids
realise another type (`SetDefaults(226)` gives type 227); the result carries the realised `type`. -/
def Item.base (type : UInt16) : Option Item := (baseItems[type.toNat]?).getD none

/-- `Item.CanHavePrefixes`: `GetRollablePrefixes() != null`. -/
def Item.canHavePrefixes (item : Item) : Bool := item.pool.isSome

/-! ## Item.TryGetPrefixStatMultipliersForItem -/

private def f (bits : UInt32) : Float32 := Float32.ofBits bits

/-- `Math.Round(double)`: round half to even. -/
private def roundEven (x : Float) : Float :=
  let lower := x.floor
  let rest := x - lower
  if rest < 0.5 then lower
  else if rest > 0.5 then lower + 1
  else if (lower / 2).floor * 2 == lower then lower else lower + 1

/-- `(int)Math.Round((double)((float)n * mult))`. -/
private def scaleRound (n : Int) (mult : Float32) : Int :=
  (roundEven ((Float32.ofInt n * mult).toFloat)).toInt32.toInt

/-- The multiplier chain of the original, before the item-specific checks. -/
def Tables.multipliers (pre : Int) : Mult :=
  match (Tables.chain.find? fun entry => (entry.1 : Int) == pre) with
  | some entry => entry.2
  | none => {}

/-- The value factor that follows the chain (`ref float` argument 11), in the original's order of
float operations. -/
def valueFactor (pre : Int) (m : Mult) : Float32 :=
  let v := 1.0 * m.dmg * (2.0 - m.useTime) * (2.0 - m.mana) * m.scale * m.kb * m.shootSpeed *
    (1.0 + Float32.ofInt m.crit * f 1017370378) * (1.0 + Float32.ofInt m.armorPen * f 1014350479) *
    (1.0 + Float32.ofInt m.tag * f 1022739087)
  let v := if pre == 62 || pre == 69 || pre == 73 || pre == 77 then v * f 1065772646 else v
  let v := if pre == 63 || pre == 70 || pre == 74 || pre == 78 || pre == 67 then v * f 1066192077 else v
  let v := if pre == 64 || pre == 71 || pre == 75 || pre == 79 || pre == 66 then v * f 1066611507 else v
  if pre == 65 || pre == 72 || pre == 76 || pre == 80 || pre == 68 then v * f 1067030938 else v

/-- `Item.TryGetPrefixStatMultipliersForItem(pre, …)`: `none` when the prefix is rejected for this
item (a multiplier that would leave damage, use animation or mana unchanged after rounding, or a
knockback multiplier on an item without knockback), otherwise the multipliers and the value factor. -/
def tryMultipliers (item : Item) (pre : Int) : Option (Mult × Float32) :=
  let m := Tables.multipliers pre
  let value := valueFactor pre m
  if m.dmg != 1 && (roundEven ((Float32.ofInt item.damage * m.dmg).toFloat)) == (Float32.ofInt item.damage).toFloat then none
  else if m.useTime != 1 && (roundEven ((Float32.ofInt item.useAnimation * m.useTime).toFloat)) == (Float32.ofInt item.useAnimation).toFloat then none
  else if m.mana != 1 && (roundEven ((Float32.ofInt item.mana * m.mana).toFloat)) == (Float32.ofInt item.mana).toFloat then none
  else if m.kb != 1 && item.knockBack == 0 then none
  else some (m, value)

/-! ## Item.Prefix(int) -/

/-- The part of `Item.Prefix` after the accepted multipliers are known (IL_012a..IL_02c0). -/
def Item.applyPrefix (item : Item) (cur : Int) (m : Mult) (value : Float32) : Item :=
  let rare := item.rare
  let rare :=
    if value.toFloat ≥ 1.2 then rare + 2
    else if value.toFloat ≥ 1.05 then rare + 1
    else if value.toFloat ≤ 0.8 then rare - 2
    else if value.toFloat ≤ 0.95 then rare - 1
    else rare
  let rare := if rare > -11 then max (-1) rare |> min 11 else rare
  let squared := value * value
  { item with
    damage := scaleRound item.damage m.dmg
    useAnimation := scaleRound item.useAnimation m.useTime
    useTime := scaleRound item.useTime m.useTime
    reuseDelay := scaleRound item.reuseDelay m.useTime
    mana := scaleRound item.mana m.mana
    knockBack := item.knockBack * m.kb
    scale := item.scale * m.scale
    shootSpeed := item.shootSpeed * m.shootSpeed
    crit := item.crit + m.crit
    bonusTagDamage := item.bonusTagDamage + m.tag
    armorPenetration := item.armorPenetration + m.armorPen
    rare
    value := (Float32.ofInt item.value * squared).toInt32.toInt
    prefixId := (cur % 256).toNat.toUInt8 }

structure PrefixResult where
  /-- The returned `bool`. -/
  ok : Bool
  item : Item
  rng : UnifiedRandom

/-- `Item.Prefix(pre)` for an explicit `pre` (`pre > 0`, `0` or `-3`) with `Main.rand` as `rng`
(`WorldGen.isGeneratingOrLoadingWorld` false). A rejected prefix restarts the loop with a one-in-four
plain roll and a roll in the item's pool, exactly as the original does; the reduced natural chance
check belongs to `pre == -1` only. `none` for the natural modes -1, -2, -4 and below. -/
def Item.prefix? (item : Item) (pre : Int) (rng : UnifiedRandom) : Option PrefixResult :=
  if pre == 0 then some { ok := false, item, rng }
  else if !item.canHavePrefixes then some { ok := false, item, rng }
  else if pre == -3 then some { ok := true, item, rng }
  else if pre < 0 then none
  else Id.run do
    let pool := Tables.pools[item.pool.getD 0]!
    let mut cur : Int := pre
    let mut rng := rng
    -- at most one pass per rejected candidate; the pools always contain an accepted prefix or 0
    for _ in [:100000] do
      if cur == -1 then
        let (plain, next) := rng.next 0 4
        rng := next
        if plain == 0 then cur := 0
      if cur == -1 then
        let (index, next) := rng.next 0 pool.size
        rng := next
        cur := pool[index.toNat]!
      match tryMultipliers item cur with
      | none => cur := -1
      | some (m, value) => return some { ok := true, item := item.applyPrefix cur m value, rng }
    return none

/-! ## The player side -/

/-- The `Terraria.Player` fields the three methods read; defaults are what `ResetEffects` leaves. -/
structure Stats where
  meleeDamage : Float32 := 1
  rangedDamage : Float32 := 1
  magicDamage : Float32 := 1
  minionDamage : Float32 := 1
  rangedMultDamage : Float32 := 1
  arrowDamageAdditiveStack : Float32 := 0
  arrowDamage : Float32 := 1
  bulletDamage : Float32 := 1
  rocketDamage : Float32 := 1
  meleeCrit : Int := 4
  rangedCrit : Int := 4
  magicCrit : Int := 4
  minionKB : Float32 := 0
  kbGlove : Bool := false
  kbBuff : Bool := false
  shroomiteStealth : Bool := false
  setVortex : Bool := false
  stealth : Float32 := 1

/-- The fields the translated equipment writes (`OriginalPlayerEquipment.Effects`); the rest keep their
`ResetEffects` values and can be overridden by the caller (buffs, set bonuses not translated). -/
def Stats.ofEffects (effects : Effects) : Stats :=
  { meleeDamage := effects.meleeDamage, rangedDamage := effects.rangedDamage,
    magicDamage := effects.magicDamage, minionDamage := effects.minionDamage,
    meleeCrit := effects.meleeCrit, rangedCrit := effects.rangedCrit, magicCrit := effects.magicCrit }

/-- `Player.GetWeaponDamageMultiplier(Item)`. -/
def weaponDamageMultiplier (stats : Stats) (item : Item) : Float32 :=
  if item.melee then stats.meleeDamage
  else if item.ranged then
    match item.rangedKind with
    | .arrow => ((stats.rangedDamage / stats.rangedMultDamage + stats.arrowDamageAdditiveStack) *
        stats.rangedMultDamage) * stats.arrowDamage
    | .bullet => stats.rangedDamage * stats.bulletDamage
    | .specialist => stats.rangedDamage * stats.rocketDamage
    | .plain => stats.rangedDamage
  else if item.magic then stats.magicDamage
  else if item.summon then stats.minionDamage
  else 1

/-- `Player.GetWeaponDamage(Item)`: `(int)((float)damage * multiplier + 5e-6f)`. -/
def weaponDamage (stats : Stats) (item : Item) : Int :=
  if item.damage ≤ 0 then item.damage
  else (Float32.ofInt item.damage * weaponDamageMultiplier stats item + f 916964780).toInt32.toInt

/-- `Player.GetWeaponCrit(Item)`. -/
def weaponCrit (stats : Stats) (item : Item) : Int :=
  if item.melee then stats.meleeCrit
  else if item.ranged then stats.rangedCrit
  else if item.magic then stats.magicCrit
  else 0

/-- `Player.GetWeaponKnockback(Item, float)`, in the original's order of float operations. -/
def weaponKnockback (stats : Stats) (item : Item) (knockback : Float32) : Float32 :=
  let kb := if item.summon then knockback + stats.minionKB else knockback
  let kb := if item.melee && stats.kbGlove then kb * 2.0 else kb
  let kb := if stats.kbBuff then kb * 1.5 else kb
  let kb := if item.ranged && stats.shroomiteStealth then kb * (1.0 + (1.0 - stats.stealth) * 0.5) else kb
  if item.ranged && stats.setVortex then kb * (1.0 + (1.0 - stats.stealth) * 0.5) else kb

end Lean4Terria.OriginalWeaponStats
