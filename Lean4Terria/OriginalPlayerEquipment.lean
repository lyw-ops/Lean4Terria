import Lean4Terria.OriginalPlayerEquipment.Model
import Lean4Terria.OriginalPlayerEquipment.Tables

/-! What a desktop 1.4.5.8 player's equipment does to the player each update: the equipment part
of `Player.ResetEffects`, `Player.UpdateEquips` and `Player.UpdateArmorSets`, hand-translated.

Covered completely: slot gating (`IsItemSlotUnlockedAndUsable`, `UpdateEquips_CanItemGrantBenefits`,
`expertOnly`), `GetEffectiveArmor` for the three armor slots (a favourited piece of another loadout
fills an empty slot), the generic part of `GrantArmorBenefits` (`defense`, `lifeRegen`, shields),
`GrantPrefixBenefits`, the set lookup of `ArmorSetBonuses.GetCompleteSet` over the whole registry,
and the type-specific effects of the items and set bonuses listed below. Everything else an
equipped item would do is reported in `Effects.unsupported` instead of being dropped silently.
Evidence, the supported list and the limits: `.lake/original-player-equipment/REPORT.md`. -/
namespace Lean4Terria.OriginalPlayerEquipment

/-- The `Item.SetDefaults` fields the equipment code reads. -/
structure ItemStats where
  defense : Nat := 0
  lifeRegen : Nat := 0
  head : Bool := false
  body : Bool := false
  legs : Bool := false
  accessory : Bool := false
  expertOnly : Bool := false
  /-- `shieldSlot > 0`. -/
  shield : Bool := false
  vanity : Bool := false
  deriving BEq, Repr, Inhabited

private def packedStats : Array UInt32 := Id.run do
  let mut dense := Array.replicate Tables.itemCount (0 : UInt32)
  for (item, defense, lifeRegen, flags) in Tables.equippable do
    dense := dense.set! item ((1 : UInt32) ||| (flags.toUInt32 <<< 1) ||| (defense.toUInt32 <<< 8) ||| (lifeRegen.toUInt32 <<< 16))
  return dense

private def membership (items : Array Nat) : Array Bool :=
  items.foldl (fun dense item => dense.set! item true) (Array.replicate Tables.itemCount false)

private def plainItems : Array Bool := membership Tables.plain
private def dualEquip : Array Bool := membership Tables.dualEquipArmor
private def incompatibilityTyped : Array Bool := membership Tables.incompatibilityTyped

/-- `none` for an item that fits no armor or accessory slot. -/
def stats (item : UInt16) : Option ItemStats :=
  let packed := packedStats[item.toNat]?.getD 0
  if packed &&& 1 == 0 then none
  else
    let flag := fun (bit : UInt32) => (packed >>> (1 + bit)) &&& 1 == 1
    some { defense := ((packed >>> 8) &&& 255).toNat, lifeRegen := ((packed >>> 16) &&& 255).toNat
           head := flag 0, body := flag 1, legs := flag 2, accessory := flag 3
           expertOnly := flag 4, shield := flag 5, vanity := flag 6 }

/-- World and player facts the equipment code reads. -/
structure Context where
  /-- `Main.expertMode`. -/
  expertMode : Bool := false
  /-- `Main.masterMode`. -/
  masterMode : Bool := false
  /-- `Main.gameMenu`. -/
  gameMenu : Bool := false
  /-- `whoAmI == Main.myPlayer`. -/
  localPlayer : Bool := true
  statLifeMax : Int := 100
  statManaMax : Int := 20
  /-- `accRunSpeed` as `Player.Update` (IL_0080) leaves it before `UpdateEquips`: `maxRunSpeed`. -/
  accRunSpeed : Float32 := 3

/-- An equipped item, a shared item or a complete set whose effect is not translated. -/
structure Unsupported where
  /-- `armor` index, or `none` for a set bonus. -/
  slot : Option Nat
  item : UInt16 := 0
  reason : String
  deriving BEq, Repr

/-- The player fields the supported equipment writes, after `ResetEffects`, `UpdateEquips` and
`UpdateArmorSets`. Names are the fields of `Terraria.Player`. -/
structure Effects where
  statDefense : Int := 0
  lifeRegen : Int := 0
  statLifeMax2 : Int := 100
  statManaMax2 : Int := 20
  moveSpeed : Float32 := 1
  meleeSpeed : Float32 := 1
  meleeDamage : Float32 := 1
  rangedDamage : Float32 := 1
  magicDamage : Float32 := 1
  minionDamage : Float32 := 1
  meleeCrit : Int := 4
  rangedCrit : Int := 4
  magicCrit : Int := 4
  manaCost : Float32 := 1
  pickSpeed : Float32 := 1
  accRunSpeed : Float32 := 3
  jumpSpeedBoost : Float32 := 0
  extraFall : Int := 0
  spikedBoots : Int := 0
  rocketBoots : Int := 0
  equipmentBasedLuckBonus : Float32 := 0
  noKnockback : Bool := false
  fireWalk : Bool := false
  noFallDmg : Bool := false
  hasRaisableShield : Bool := false
  hasJumpOption_Cloud : Bool := false
  hasJumpOption_Blizzard : Bool := false
  hasJumpOption_Sandstorm : Bool := false
  autoJump : Bool := false
  jumpBoost : Bool := false
  accFlipper : Bool := false
  autoReuseGlove : Bool := false
  accDivingHelm : Bool := false
  hasLuck_LuckyHorseshoe : Bool := false
  cactusThorns : Bool := false
  ashWoodBonus : Bool := false
  ammoCost80 : Bool := false
  spaceGun : Bool := false
  shadowArmor : Bool := false
  crimsonRegen : Bool := false
  anglerSetSpawnReduction : Bool := false
  /-- `ResetEffects` IL_003e..005f: 1 when the Demon Heart slot is open. -/
  extraAccessorySlots : Nat := 0
  /-- `ArmorSetBonuses.Benefits` method of the complete set, if any. -/
  armorSet : Option String := none
  unsupported : Array Unsupported := #[]
  deriving Repr

/-- `Player.IsItemSlotUnlockedAndUsable`. -/
def slotUsable (equipment : Equipment) (context : Context) (slot : Nat) : Bool :=
  if slot == 8 || slot == 18 then equipment.extraAccessory && (context.expertMode || context.gameMenu)
  else if slot == 9 || slot == 19 then context.masterMode || context.gameMenu
  else true

/-- `ItemSlot.CanEquipInArmorSlot` for a functional armor slot (0–2): the piece must be of that
kind, and unless it is `DualEquipArmor` the same type must not sit in the vanity armor slots. `none`
when the test would need the accessory incompatibility table. -/
private def canShareArmorPiece (equipment : Equipment) (slot : Nat) (item : UInt16) : Option Bool :=
  match stats item with
  | none => some false
  | some info =>
    let kind := if slot == 0 then info.head else if slot == 1 then info.body else info.legs
    if !kind then some false
    else if dualEquip[item.toNat]?.getD false then some true
    else if incompatibilityTyped[item.toNat]?.getD false ||
        (List.range 3).any (fun k => incompatibilityTyped[(equipment.armor[10 + k]!).item.toNat]?.getD false) then none
    else some !((List.range 3).any fun k => !(equipment.armor[10 + k]!).isAir && (equipment.armor[10 + k]!).item == item)

/-- `Player.GetEffectiveArmor(slot)`: the slot's own item, or for an empty slot the first
favourited item in that slot of `Loadouts[0..2]`, if it may be shared; the search stops at the
first favourited candidate either way. The second component reports a candidate this module does
not decide (accessory slots, and armor needing the incompatibility table). -/
def effectiveArmor (equipment : Equipment) (slot : Nat) : Slot × Option Unsupported :=
  let own := equipment.armor[slot]?.getD {}
  if !own.isAir then (own, none)
  else
    match equipment.loadouts.findSome? fun loadout =>
        (loadout.armor[slot]?).filter fun candidate => !candidate.isAir && candidate.favorited with
    | none => (own, none)
    | some candidate =>
      if slot ≥ 3 then
        (own, some { slot := some slot, item := candidate.item, reason := "favourited accessory of another loadout (Player.CanShareArmor)" })
      else match canShareArmorPiece equipment slot candidate.item with
        | some true => (candidate, none)
        | some false => (own, none)
        | none => (own, some { slot := some slot, item := candidate.item, reason := "shared armor with an accessory incompatibility type" })

/-- `Player.UpdateEquips_CanItemGrantBenefits`. -/
private def canGrant (slot : Nat) (info : ItemStats) : Bool :=
  if slot == 0 then info.head else if slot == 1 then info.body else if slot == 2 then info.legs else info.accessory

/-- `Player.GrantPrefixBenefits` (accessory prefixes 62–80). -/
def grantPrefix (prefixId : UInt8) (e : Effects) : Effects :=
  let damage := fun (bonus : Float32) => { e with
    meleeDamage := e.meleeDamage + bonus, rangedDamage := e.rangedDamage + bonus
    magicDamage := e.magicDamage + bonus, minionDamage := e.minionDamage + bonus }
  let crit := fun (bonus : Int) => { e with
    meleeCrit := e.meleeCrit + bonus, rangedCrit := e.rangedCrit + bonus, magicCrit := e.magicCrit + bonus }
  match prefixId with
  | 62 => { e with statDefense := e.statDefense + 1 }
  | 63 => { e with statDefense := e.statDefense + 2 }
  | 64 => { e with statDefense := e.statDefense + 3 }
  | 65 => { e with statDefense := e.statDefense + 4 }
  | 66 => { e with statManaMax2 := e.statManaMax2 + 20 }
  | 67 => crit 2
  | 68 => crit 4
  | 69 => damage 0.01
  | 70 => damage 0.02
  | 71 => damage 0.03
  | 72 => damage 0.04
  | 73 => { e with moveSpeed := e.moveSpeed + 0.01 }
  | 74 => { e with moveSpeed := e.moveSpeed + 0.02 }
  | 75 => { e with moveSpeed := e.moveSpeed + 0.03 }
  | 76 => { e with moveSpeed := e.moveSpeed + 0.04 }
  | 77 => { e with meleeSpeed := e.meleeSpeed + 0.01 }
  | 78 => { e with meleeSpeed := e.meleeSpeed + 0.02 }
  | 79 => { e with meleeSpeed := e.meleeSpeed + 0.03 }
  | 80 => { e with meleeSpeed := e.meleeSpeed + 0.04 }
  | _ => e

/-- The type-specific part of `GrantArmorBenefits` for the supported items that have one. -/
private def armorSpecific (item : UInt16) : Option (Effects → Effects) :=
  match item with
  | 268 => some fun e => { e with accDivingHelm := true }               -- Diving Helmet
  | _ => none

/-- The type-specific part of `ApplyEquipFunctional` for the supported accessories. -/
private def functional (item : UInt16) : Option (Effects → Effects) :=
  match item with
  | 54 => some fun e => { e with accRunSpeed := 6 }                                   -- Hermes Boots
  | 405 => some fun e => { e with accRunSpeed := 6, rocketBoots := 2 }  -- Spectre Boots
  | 128 => some fun e => { e with rocketBoots := 1 }          -- Rocket Boots
  | 53 => some fun e => { e with hasJumpOption_Cloud := true }                        -- Cloud in a Bottle
  | 987 => some fun e => { e with hasJumpOption_Blizzard := true }                    -- Blizzard in a Bottle
  | 857 => some fun e => { e with hasJumpOption_Sandstorm := true }                   -- Sandstorm in a Bottle
  | 111 => some fun e => { e with statManaMax2 := e.statManaMax2 + 40 }               -- Band of Starpower
  | 158 => some fun e => { e with noFallDmg := true, hasLuck_LuckyHorseshoe := true } -- Lucky Horseshoe
  | 285 => some fun e => { e with moveSpeed := e.moveSpeed + 0.05 }                   -- Aglet
  | 212 => some fun e => { e with moveSpeed := e.moveSpeed + 0.1 }                    -- Anklet of the Wind
  | 211 => some fun e => { e with autoReuseGlove := true, meleeSpeed := e.meleeSpeed + 0.12 }  -- Feral Claws
  | 156 => some fun e => { e with noKnockback := true }                               -- Cobalt Shield
  | 193 => some fun e => { e with fireWalk := true }                                  -- Obsidian Skull
  | 159 => some fun e => { e with jumpBoost := true }                                 -- Shiny Red Balloon
  | 187 => some fun e => { e with accFlipper := true }                                -- Flipper
  | 953 | 975 => some fun e => { e with spikedBoots := e.spikedBoots + 1 }            -- Climbing Claws, Shoe Spikes
  | 223 => some fun e => { e with manaCost := e.manaCost - 0.06 }                     -- Nature's Gift
  | 2423 => some fun e => { e with autoJump := true, jumpSpeedBoost := e.jumpSpeedBoost + 1.6
                                   extraFall := e.extraFall + 10 }                    -- Frog Leg
  | _ => none

/-- Supported items whose only effect is the generic one although their type number is compared
somewhere in the scanned methods for another reason. -/
private def genericOnly (item : UInt16) : Bool := item == 49                          -- Band of Regeneration

/-- Whether every effect of this equipped item is translated. -/
def supported (item : UInt16) : Bool :=
  (plainItems[item.toNat]?.getD false) || genericOnly item || (armorSpecific item).isSome || (functional item).isSome

/-- `ArmorSetBonuses.Benefits.*` for the translated set bonuses. -/
private def benefit (name : String) : Option (Effects → Effects) :=
  match name with
  | "Wood" => some fun e => { e with statDefense := e.statDefense + 1 }
  | "MetalTier1" => some fun e => { e with statDefense := e.statDefense + 2 }
  | "MetalTier2" => some fun e => { e with statDefense := e.statDefense + 3 }
  | "Platinum" => some fun e => { e with statDefense := e.statDefense + 4 }
  | "AshWood" => some fun e => { e with ashWoodBonus := true }
  | "Cactus" => some fun e => { e with cactusThorns := true }
  | "Pumpkin" => some fun e => { e with
      meleeDamage := e.meleeDamage + 0.1, magicDamage := e.magicDamage + 0.1
      rangedDamage := e.rangedDamage + 0.1, minionDamage := e.minionDamage + 0.1 }
  | "Gladiator" => some fun e => { e with noKnockback := true }
  | "Ninja" => some fun e => { e with moveSpeed := e.moveSpeed + 0.2 }
  | "Fossil" => some fun e => { e with ammoCost80 := true }
  | "Necro" => some fun e => { e with rangedCrit := e.rangedCrit + 10 }
  | "Jungle" => some fun e => { e with manaCost := e.manaCost - 0.16 }
  | "Meteor" => some fun e => { e with spaceGun := true }
  | "ShadowScale" => some fun e => { e with shadowArmor := true }
  | "Crimson" => some fun e => { e with crimsonRegen := true }
  | "Mining" => some fun e => { e with pickSpeed := e.pickSpeed - 0.1 }
  | "Angler" => some fun e => { e with anglerSetSpawnReduction := true }
  | "Wizard" => some fun e => { e with magicCrit := e.magicCrit + 10 }
  | "MagicHat" => some fun e => { e with statManaMax2 := e.statManaMax2 + 60 }
  | _ => none

/-- `ArmorSetBonuses.GetCompleteSet`: the sets that list the head type (as head, then as body,
then as legs, each in registration order), then those that list the body type; the first one
whose required parts are all worn. A part 0 in a set is not required. Returns the benefit name. -/
def completeSet (head body legs : UInt16) : Option String :=
  let complete := fun (set : Nat × Nat × Nat × Nat) =>
    let (h, b, l, _) := set
    (h == 0 || h == head.toNat) && (b == 0 || b == body.toNat) && (l == 0 || l == legs.toNat)
  let containing := fun (type : UInt16) =>
    if type == 0 then #[] else
    Tables.sets.filter (fun set => set.1 == type.toNat) ++ Tables.sets.filter (fun set => set.2.1 == type.toNat) ++
      Tables.sets.filter (fun set => set.2.2.1 == type.toNat)
  ((containing head).find? complete <|> (containing body).find? complete).bind fun set => Tables.benefits[set.2.2.2]?

/-- The values `Player.ResetEffects` gives the modelled fields, with `accRunSpeed` as `Player.Update`
sets it before `UpdateEquips`. -/
def reset (equipment : Equipment) (context : Context := {}) : Effects := {
    statLifeMax2 := context.statLifeMax, statManaMax2 := context.statManaMax, accRunSpeed := context.accRunSpeed
    extraAccessorySlots := if equipment.extraAccessory && (context.expertMode || context.gameMenu) then 1 else 0 }

/-- `UpdateEquips(i)` and `UpdateArmorSets(i)` applied to `start`, the state after `ResetEffects` and
whatever `Player.Update` ran before `UpdateEquips` (buffs add to the same fields first, and float
additions keep their order). Order as in the original: slots 0–9 in order, each granting its
accessory prefix and then `GrantArmorBenefits`; slots 3–9 in order, `ApplyEquipFunctional`; the luck
tail of `UpdateEquips`; the complete set's benefit. -/
def apply (start : Effects) (equipment : Equipment) (context : Context := {}) : Effects := Id.run do
  let mut e := start
  let mut worn : Array Slot := #[]
  for slot in [:10] do
    let (item, shared) := effectiveArmor equipment slot
    if let some entry := shared then e := { e with unsupported := e.unsupported.push entry }
    worn := worn.push item
    if item.isAir || !slotUsable equipment context slot then continue
    let some info := stats item.item
      | e := { e with unsupported := e.unsupported.push { slot := some slot, item := item.item, reason := "not an equippable item" } }
    if info.expertOnly && !context.expertMode then continue
    if !canGrant slot info then continue
    if info.accessory then e := grantPrefix item.prefixId e
    -- GrantArmorBenefits IL_007b..00ac: defense, lifeRegen, raisable shield
    e := { e with statDefense := e.statDefense + info.defense, lifeRegen := e.lifeRegen + info.lifeRegen
                  hasRaisableShield := e.hasRaisableShield || info.shield }
    if let some effect := armorSpecific item.item then e := effect e
    if !supported item.item then
      e := { e with unsupported := e.unsupported.push { slot := some slot, item := item.item, reason := "type-specific equipment effect" } }
  for slot in [3:10] do
    if !slotUsable equipment context slot then continue
    let item := worn[slot]!
    if item.isAir then continue
    -- ApplyEquipFunctional IL_0000..000f
    if ((stats item.item).any (·.expertOnly)) && !context.expertMode then continue
    if let some effect := functional item.item then e := effect e
  -- UpdateEquips IL_055d..057c
  if context.localPlayer && e.hasLuck_LuckyHorseshoe then
    e := { e with equipmentBasedLuckBonus := e.equipmentBasedLuckBonus + 0.05 }
  match completeSet worn[0]!.item worn[1]!.item worn[2]!.item with
  | none => pure ()
  | some name =>
    e := { e with armorSet := some name }
    match benefit name with
    | some effect => e := effect e
    | none => e := { e with unsupported := e.unsupported.push { slot := none, reason := s!"ArmorSetBonus {name}" } }
  return e

/-- One update with nothing but equipment: `apply (reset …) …`. -/
def update (equipment : Equipment) (context : Context := {}) : Effects :=
  apply (reset equipment context) equipment context

end Lean4Terria.OriginalPlayerEquipment
