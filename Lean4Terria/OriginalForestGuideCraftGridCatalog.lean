import Std.Data.HashMap
import Std.Data.HashSet
import Lean4Terria.OriginalRecipeCatalog
import Lean4Terria.OriginalResidentText
import Lean4Terria.OriginalForestGuideCraftGridLayout
import Lean4Terria.OriginalForestGuideCraftGridCatalog.Data

/-!
# The Guide crafting grid's production catalogue (Terraria 1.4.5.8 desktop, en-US)

Builds the complete `OriginalForestGuideCraftGridLayout.Catalog` — every recipe of `OriginalRecipeCatalog.catalog`
(3,603) and every item type — from what the desktop already loads: the recipe catalogue, the en-US dictionary of
the player's own `Terraria.exe` (`OriginalResidentText.Dict`), the runtime XNB fonts and the XNB texture sizes.
`Data.packed` carries only static item metadata captured from the vanilla runtime (numbers, flags, language keys, set
memberships); every string the grid shows is looked up in the dictionary at run time.

Transcribed (IL read, `.lake/original-forest-guide-grid/1.4.5.8/catalog-production/il`):

* the 11 filter tabs of `NewCraftingUI._filterer` (`ItemFilters.Weapon` … `Materials`, `MiscFallback`;
  `AArmor.IsAnArmorThatMatchesSocialState`, `AAccessories.IsAnAccessoryOfType` / `IsMiscEquipment`,
  `Tools._itemIdsThatAreAccepted`);
* `Main.GetRecipeRequirementsText(recipe, false)` (`Recipe.GetRequiredTileName` / `GetRequiredTileStyle`,
  `Lang.GetMapObjectName` through the map-legend keys of `Data`);
* `Item.Name` (`Lang.GetItemNameValue`, `_itemNameCache`);
* `Main.MouseText_DrawItemTooltip_GetLinesInfo` for a recipe's create item with stack 1 — the lines
  `ItemFilters.BySearch.FitsFilter` searches (`Item.GetHoverName`, `ItemSlot.EstimateDisplayStack`,
  `ItemTooltip.get_Lines` / `GetLine` / `ValidateTooltip`, `LocalizedText.get_HasValue`,
  `Player.GetWeaponDamageMultiplier`, `ApplyRapidAttackBonus`, `Item.GetVisualCritChance`,
  `ArmorSetBonus.GetTooltipForSinglePiece`, `LanguageManager.GetTextValue` / `LocalizedText.Format`);
* `Main.itemAnimations` icons: `DrawAnimationVertical.Update` driven by `Main.DoUpdate_AnimateItemIcons`.

Checked against the vanilla assembly run in the private host for every item type and every recipe
(`catalog-production/REPORT.md`).

## Boundaries (`Except String`, never a guess)

Tooltip lines of an item with `prefix > 0`, `favorited`, `wornArmor`, a `tooltipContext ≥ 0` (loadout sharing,
Journey research), `BestiaryNotes`, or the Journey-mode player (`Env.journey`); a dictionary text whose
substitutions the caller's text environment cannot supply (`OriginalResidentText.Boundary`). None of these occur for
a recipe's create item in Classic.
-/

namespace Lean4Terria.OriginalForestGuideCraftGridCatalog

open OriginalForestGuideCraftGridLayout (Catalog ItemVisual Recipe)
open OriginalForestChatLayout (Font Color)

abbrev Dict := OriginalResidentText.Dict
abbrev TextEnv := OriginalResidentText.Env

/-! ## Static item metadata (`Data.packed`) -/

/-- `Main.itemAnimations[t]`. -/
inductive Anim where
  | none
  /-- `DrawAnimationVertical`: frame count, ticks per frame, start frame, start counter, ping-pong,
  `NotActuallyAnimating` -/
  | vertical (count ticks frame counter : Nat) (pingPong notActually : Bool)
  /-- another `DrawAnimation` kind and its frame count (`DrawAnimationScryingOrb`) -/
  | other (kind : String) (count : Nat)
  deriving Repr, BEq, Inhabited

/-- `ArmorSetBonuses.SetsContaining[t][0]`: description key, `PrimaryPart` (0 none, 1 head, 2 body, 3 legs) and the
part item types. -/
structure SetBonus where
  descKey : String
  primary : Nat
  head : Int
  body : Int
  legs : Int
  deriving Repr, Inhabited

/-- What `Item.SetDefaults(t)` leaves in the fields the catalogue reads. -/
structure ItemDef where
  type : Nat
  nameKey : String
  /-- `Lang._itemTooltipCache[t]._text.Key`, `none` for `ItemTooltip.None` -/
  tipKey : Option String
  damage : Int := 0
  crit : Int := 0
  useAnimation : Int := 0
  useStyle : Int := 0
  shoot : Int := 0
  mountType : Int := -1
  buffType : Int := 0
  buffTime : Int := 0
  fishingPole : Int := 0
  bait : Int := 0
  headSlot : Int := -1
  bodySlot : Int := -1
  legSlot : Int := -1
  tileWand : Int := -1
  defense : Int := 0
  pick : Int := 0
  axe : Int := 0
  hammer : Int := 0
  tileBoost : Int := 0
  healLife : Int := 0
  healMana : Int := 0
  mana : Int := 0
  createTile : Int := -1
  createWall : Int := -1
  ammo : Int := 0
  useAmmo : Int := 0
  rare : Int := 0
  alpha : Int := 0
  glowMask : Int := -1
  tooltipContext : Int := -1
  prefixId : Int := 0
  flags : Std.HashSet String := {}
  knockBack : Float32 := 0
  tipDamageMul : Float32 := 1
  color : UInt32 := 0
  anim : Anim := .none
  setBonus : Option SetBonus := none
  deriving Inhabited

def ItemDef.flag (d : ItemDef) (f : String) : Bool := d.flags.contains f

/-- The parsed `Data.packed`. -/
structure Defs where
  count : Nat := 0
  ammoArrow : Int := 40
  ammoStake : Int := 1836
  items : Std.HashMap Nat ItemDef := {}
  sets : Std.HashMap String (Std.HashSet Int) := {}
  /-- `(tile, style)` → map-legend key -/
  legend : Std.HashMap (Nat × Nat) String := {}
  /-- `Item.SetDefaults(t)` leaving another `type` (deprecated ids: 226 → 227, 1803 … 1807 → 1533 … 1537; removed
  ids → 0, air) -/
  redirect : Std.HashMap Nat Nat := {}
  /-- `Main.itemAnimations[t]` of a redirected slot (the array is indexed by slot, not by the resulting type) -/
  slotAnim : Std.HashMap Nat Anim := {}
  /-- `ItemID.Sets.TextureCopyLoad`: `t → src` for `src ≠ −1` -/
  textureCopy : Std.HashMap Nat Nat := {}
  deriving Inhabited

def Defs.inSet (d : Defs) (name : String) (i : Int) : Bool := (d.sets.getD name {}).contains i

private def toInt! (s : String) : Except String Int :=
  match s.toInt? with
  | some v => .ok v
  | none => .error s!"Data: bad number {s}"

def parseData (text : String) : Except String Defs := do
  let mut d : Defs := {}
  for line in text.splitOn "\n" do
    let w := (line.splitOn " ").filter (· ≠ "")
    match w with
    | [] => pure ()
    | "K" :: [k, v] =>
      let v ← toInt! v
      match k with
      | "count" => d := { d with count := v.toNat }
      | "ammoArrow" => d := { d with ammoArrow := v }
      | "ammoStake" => d := { d with ammoStake := v }
      | _ => throw s!"Data: unknown constant {k}"
    | "S" :: name :: ids =>
      let ids ← ids.mapM toInt!
      d := { d with sets := d.sets.insert name (Std.HashSet.ofList ids) }
    | "R" :: [t, target] =>
      d := { d with redirect := d.redirect.insert (← toInt! t).toNat (← toInt! target).toNat }
    | "A" :: [t, v] =>
      match ← (v.splitOn ",").mapM toInt! with
      | [c, tk, fr, ct, pp, na] =>
        let slot := (← toInt! t).toNat
        let anim : Anim := .vertical c.toNat tk.toNat fr.toNat ct.toNat (pp != 0) (na != 0)
        d := { d with slotAnim := d.slotAnim.insert slot anim }
      | _ => throw s!"Data: slot animation {t} {v}"
    | "C" :: [t, src] =>
      d := { d with textureCopy := d.textureCopy.insert (← toInt! t).toNat (← toInt! src).toNat }
    | "T" :: [t, s, key] =>
      d := { d with legend := d.legend.insert ((← toInt! t).toNat, (← toInt! s).toNat) key }
    | "I" :: t :: nameKey :: tip :: fields =>
      let t := (← toInt! t).toNat
      let mut it : ItemDef := { type := t, nameKey, tipKey := if tip == "-" then none else some tip }
      for f in fields do
        match f.splitOn "=" with
        | [k, v] =>
          match k with
          | "flags" => it := { it with flags := Std.HashSet.ofList (v.splitOn ",") }
          | "anim" =>
            match ← (v.splitOn ",").mapM toInt! with
            | [c, tk, fr, ct, pp, na] =>
              it := { it with anim := .vertical c.toNat tk.toNat fr.toNat ct.toNat (pp != 0) (na != 0) }
            | _ => throw s!"Data: item {t} anim {v}"
          | "animKind" =>
            match v.splitOn "," with
            | [k, n] => it := { it with anim := .other k (← toInt! n).toNat }
            | _ => throw s!"Data: item {t} animKind {v}"
          | "setBonus" =>
            match v.splitOn "," with
            | [dk, p, h, b, l] =>
              it := { it with setBonus := some { descKey := dk, primary := (← toInt! p).toNat, head := ← toInt! h,
                                                 body := ← toInt! b, legs := ← toInt! l } }
            | _ => throw s!"Data: item {t} setBonus {v}"
          | _ =>
            let n ← toInt! v
            it ← match k with
              | "damage" => pure { it with damage := n }
              | "crit" => pure { it with crit := n }
              | "useAnimation" => pure { it with useAnimation := n }
              | "useStyle" => pure { it with useStyle := n }
              | "shoot" => pure { it with shoot := n }
              | "mountType" => pure { it with mountType := n }
              | "buffType" => pure { it with buffType := n }
              | "buffTime" => pure { it with buffTime := n }
              | "fishingPole" => pure { it with fishingPole := n }
              | "bait" => pure { it with bait := n }
              | "headSlot" => pure { it with headSlot := n }
              | "bodySlot" => pure { it with bodySlot := n }
              | "legSlot" => pure { it with legSlot := n }
              | "tileWand" => pure { it with tileWand := n }
              | "defense" => pure { it with defense := n }
              | "pick" => pure { it with pick := n }
              | "axe" => pure { it with axe := n }
              | "hammer" => pure { it with hammer := n }
              | "tileBoost" => pure { it with tileBoost := n }
              | "healLife" => pure { it with healLife := n }
              | "healMana" => pure { it with healMana := n }
              | "mana" => pure { it with mana := n }
              | "createTile" => pure { it with createTile := n }
              | "createWall" => pure { it with createWall := n }
              | "ammo" => pure { it with ammo := n }
              | "useAmmo" => pure { it with useAmmo := n }
              | "rare" => pure { it with rare := n }
              | "alpha" => pure { it with alpha := n }
              | "glowMask" => pure { it with glowMask := n }
              | "tooltipContext" => pure { it with tooltipContext := n }
              | "prefix" => pure { it with prefixId := n }
              | "knockBack" => pure { it with knockBack := Float32.ofBits n.toNat.toUInt32 }
              | "tipDamageMul" => pure { it with tipDamageMul := Float32.ofBits n.toNat.toUInt32 }
              | "color" => pure { it with color := n.toNat.toUInt32 }
              | _ => throw s!"Data: item {t}: unknown field {k}"
        | _ => throw s!"Data: item {t}: bad field {f}"
      d := { d with items := d.items.insert t it }
    | _ => throw s!"Data: bad record {line.take 40}"
  return d

/-- The 1.4.5.8 item metadata (parsed once). -/
def metadata : Except String Defs := parseData Data.packed

/-- The item `Item.SetDefaults(t)` leaves (following `redirect`; air for a removed id). -/
def Defs.item (d : Defs) (t : Nat) : Except String ItemDef :=
  let t := d.redirect.getD t t
  if t == 0 then .ok { type := 0, nameKey := "", tipKey := none } else
  match d.items[t]? with
  | some i => .ok i
  | none => .error s!"item type {t} is not a 1.4.5.8 item"

/-- The asset `TextureAssets.Item[t]` holds. `AssetInitializer.LoadTextures` walks `t = 0 … Count − 1` once:
`Item[t] = TextureCopyLoad[t] != -1 ? Item[TextureCopyLoad[t]] : Request("Images/Item_" + t)`, so an alias takes
whatever its source slot already holds (3705 → 3665 → `Images/Item_48`); a source not loaded yet would be null. -/
partial def Defs.itemTexture (d : Defs) (t : Nat) : Except String String :=
  match d.textureCopy[t]? with
  | none => .ok s!"Images/Item_{t}"
  | some src =>
    if src < t then d.itemTexture src
    else .error s!"item {t}: TextureCopyLoad source {src} is not loaded yet (TextureAssets.Item[{t}] would be null)"

/-! ## Player and world inputs -/

/-- What `MouseText_DrawItemTooltip_GetLinesInfo` and `GetRecipeRequirementsText` read besides the item. Defaults:
a fresh Classic player (what `Player.ResetEffects` leaves, no buffs) in a fresh corruption Classic world, no shop
open. -/
structure Env where
  meleeDamage : Float32 := 1
  rangedDamage : Float32 := 1
  /-- `Player.bowEffectiveDamage` (`AmmoID.Sets.IsArrow`) -/
  bowDamage : Float32 := 1
  /-- `Player.gunEffectiveDamage` (`IsBullet`) -/
  gunDamage : Float32 := 1
  /-- `Player.specialistEffectiveDamage` (`IsSpecialist`, `ItemID.Sets.IsRangedSpecialistWeapon`) -/
  specialistDamage : Float32 := 1
  magicDamage : Float32 := 1
  minionDamage : Float32 := 1
  meleeCrit : Int := 4
  rangedCrit : Int := 4
  magicCrit : Int := 4
  /-- `LocalPlayer.inventory[selectedItem]`: type and `crit` -/
  selectedItemType : Nat := 0
  selectedItemCrit : Int := 0
  revolverCritChanceBonus : Int := 0
  rapidAttackBonus : Float32 := 0
  minionKB : Float32 := 0
  magicQuiver : Bool := false
  stealth : Float32 := 1
  manaCost : Float32 := 1
  spaceGun : Bool := false
  /-- `Main.player[myPlayer].HasItem(905)` (the Coin Gun: coins show their damage) -/
  hasCoinGun : Bool := false
  downedDD2EventAnyDifficulty : Bool := false
  /-- `LocalPlayer.difficulty == 3` -/
  journey : Bool := false
  /-- `WorldGen.crimson` -/
  crimson : Bool := false
  downedPlantBoss : Bool := false
  tenthAnniversaryWorld : Bool := false
  getGoodWorld : Bool := false
  /-- `SpecialSeedFeatures.Mechdusa` -/
  mechdusa : Bool := false
  expertMode : Bool := false
  /-- `Main.npcShop > 0` -/
  npcShopOpen : Bool := false
  deriving Repr, Inhabited

/-! ## Text -/

private def text (dict : Dict) (te : TextEnv) (key : String) : Except String String :=
  (dict.getTextValue te key).mapError toString

/-- `Language.GetTextValue(key, args…)` = `String.Format(GetText(key).Value, args)`; a missing key is the key. -/
private def textFormat (dict : Dict) (te : TextEnv) (key : String) (args : Array String) : Except String String := do
  if !dict.exists key then return key
  let v ← text dict te key
  (OriginalResidentText.appendFormat key v args).mapError toString

private def tip (dict : Dict) (te : TextEnv) (n : Nat) := text dict te s!"LegacyTooltip.{n}"
private def inter (dict : Dict) (te : TextEnv) (n : Nat) := text dict te s!"LegacyInterface.{n}"
private def misc (dict : Dict) (te : TextEnv) (n : Nat) := text dict te s!"LegacyMisc.{n}"

/-- `Lang.GetItemNameValue(t)`: `_itemNameCache[t].Value` (`LocalizedText.Empty` outside `1 … Count − 1`). -/
def itemNameValue (defs : Defs) (dict : Dict) (te : TextEnv) (t : Int) : Except String String := do
  if t ≤ 0 || t ≥ defs.count then return ""
  text dict te (← defs.item t.toNat).nameKey

/-! ## Filter tabs (`NewCraftingUI._filterer.AvailableFilters`, in order) -/

/-- `Tools._itemIdsThatAreAccepted` (`ItemFilters.Tools..ctor`). -/
def toolIds : List Int :=
  [213, 5295, 509, 850, 851, 3612, 3625, 3611, 510, 849, 3620, 1071, 1543, 1072, 1544, 1100, 1545, 50, 3199, 3124,
   5358, 5359, 5360, 5361, 5437, 1326, 5335, 3384, 4263, 4819, 4262, 946, 4707, 205, 206, 207, 1128, 3031, 4820,
   5302, 5364, 4460, 4608, 4872, 3032, 5303, 5304, 1991, 4821, 3183, 779, 5134, 1299, 4711, 4049, 114, 5667, 6174]

/-- `AAccessories.IsMiscEquipment` -/
def isMiscEquipment (defs : Defs) (it : ItemDef) : Bool :=
  it.mountType != -1 ||
  (it.buffType > 0 && defs.inSet "lightPet" it.buffType) ||
  (it.buffType > 0 && defs.inSet "vanityPet" it.buffType) ||
  defs.inSet "projHook" it.shoot

/-- `AArmor.IsAnArmorThatMatchesSocialState(item, social)` -/
private def armorMatching (it : ItemDef) (social : Bool) : Bool :=
  (it.bodySlot != -1 || it.headSlot != -1 || it.legSlot != -1) && it.flag "vanity" == social

/-- The ten specific tabs: Weapon, Armor, Vanity, BuildingBlock, Furniture, Accessories, MiscAccessories,
Consumables, Tools, Materials. -/
def specificFits (defs : Defs) (it : ItemDef) : Array Bool :=
  let frameImportant := defs.inSet "tileFrameImportant" it.createTile
  let misc := isMiscEquipment defs it
  #[it.damage > 0,
    armorMatching it false,
    armorMatching it true,
    it.createWall != -1 || it.tileWand != -1 || (it.createTile != -1 && !frameImportant),
    it.createTile != -1 && frameImportant,
    !misc && it.flag "accessory",
    misc,
    it.type == 267 || it.type == 1307 ||
      (it.flag "consumable" && !(it.createTile != -1 || it.createWall != -1 || it.tileWand != -1)),
    it.pick > 0 || it.axe > 0 || it.hammer > 0 || it.fishingPole > 0 || it.tileWand != -1 || toolIds.contains it.type,
    it.flag "material"]

/-- All 11 tabs; the last is `MiscFallback` (none of the others). -/
def fitsOf (defs : Defs) (it : ItemDef) : Array Bool :=
  let s := specificFits defs it
  s.push (!s.any id)

def fits (defs : Defs) (t : Nat) : Except String (Array Bool) := do
  return fitsOf defs (← defs.item t)

/-! ## `Main.GetRecipeRequirementsText(recipe, false)` -/

def requirementsText (defs : Defs) (dict : Dict) (te : TextEnv) (env : Env) (r : OriginalRecipeCatalog.Recipe) :
    Except String String := do
  let mut parts : Array String := #[]
  if let some t := r.requiredTile then
    let style := if t == 26 then (if env.crimson then 1 else 0) else 0
    match defs.legend[(t, style)]? with
    | some key => parts := parts.push (← text dict te key)
    | none => throw s!"recipe {r.index}: no map-legend key for tile {t} style {style}"
  let c := r.conditions
  if c.needWater then parts := parts.push (← inter dict te 53)
  if c.needHoney then parts := parts.push (← inter dict te 58)
  if c.needLava then parts := parts.push (← inter dict te 56)
  if c.needSnowBiome then parts := parts.push (← inter dict te 123)
  if c.needGraveyardBiome then parts := parts.push (← inter dict te 124)
  if c.needTorchGodsFavor then parts := parts.push (← inter dict te 125)
  return (← inter dict te 22) ++ " " ++ ", ".intercalate parts.toList

/-! ## The tooltip lines `ItemFilters.BySearch` searches -/

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def truncI (f : Float32) : Int := f.toInt32.toInt

/-- `(int)Math.Round(x)` (half to even) of a finite double, as `Double.ToString()` prints an integral value. -/
private def roundStr (x : Float) : String :=
  let f := x.floor
  let d := x - f
  let i : Int := if f < 0 then -((-f).toUInt64.toNat : Int) else (f.toUInt64.toNat : Int)
  let r := if d < 0.5 then i else if d > 0.5 then i + 1 else if i % 2 == 0 then i else i + 1
  toString r

/-- `Player.GetWeaponDamageMultiplier(item)` -/
def damageMultiplier (defs : Defs) (env : Env) (it : ItemDef) : Float32 :=
  if it.flag "melee" then env.meleeDamage
  else if it.flag "ranged" then
    if defs.inSet "isArrow" it.useAmmo || defs.inSet "isArrow" it.ammo then env.bowDamage
    else if defs.inSet "isBullet" it.useAmmo || defs.inSet "isBullet" it.ammo then env.gunDamage
    else if defs.inSet "isSpecialist" it.useAmmo || defs.inSet "isSpecialist" it.ammo ||
        defs.inSet "isRangedSpecialistWeapon" it.type then env.specialistDamage
    else env.rangedDamage
  else if it.flag "magic" then env.magicDamage
  else if it.flag "summon" then env.minionDamage
  else 1

/-- `Item.GetVisualCritChance(player)` -/
private def visualCrit (env : Env) (it : ItemDef) : Int :=
  if it.type == 2269 && env.revolverCritChanceBonus > 0 then
    let v := single (it.crit + env.revolverCritChanceBonus)
    truncI (if v > 100 then 100 else if v < 0 then 0 else v)
  else it.crit

/-- `Main.MouseText_DrawItemTooltip_GetLinesInfo(item, …)` for a fresh `Item.SetDefaults(t)` with `stack = 1` (what
`BySearch.FitsFilter` passes), the first `numLines` entries. -/
def searchLinesOf (defs : Defs) (dict : Dict) (te : TextEnv) (env : Env) (it : ItemDef) :
    Except String (Array String) := do
  let t := it.type
  let tipT := tip dict te
  if it.prefixId > 0 then throw s!"item {t}: tooltip lines of a prefixed item (prefix {it.prefixId})"
  if it.tooltipContext ≥ 0 then throw s!"item {t}: tooltip lines in context {it.tooltipContext}"
  if it.flag "favorited" then throw s!"item {t}: tooltip lines of a favourite"
  if it.flag "buyOnce" then throw s!"item {t}: EstimateDisplayStack of a buy-once item"
  -- [0] Item.GetHoverName(item, EstimateDisplayStack(item)): stack 1, no prefix → Item.Name
  let mut L : Array String := #[← itemNameValue defs dict te t]   -- t = item.type after SetDefaults
  if it.flag "social" && !it.flag "vanity" && !it.flag "hasVanityEffects" then L := L.push (← tipT 61)
  if it.damage > 0 && (!it.flag "notAmmo" || it.useStyle != 0) &&
      !(71 ≤ t && t ≤ 74 && !env.hasCoinGun) then
    let mut dmg : Int := truncI (single it.damage * it.tipDamageMul)
    if defs.inSet "rapidAttackBonusDamage" t then
      let b := if env.rapidAttackBonus > 0.5 then (0.5 : Float32) else env.rapidAttackBonus
      dmg := truncI (single dmg * (1 + b))
    let shown := truncI (damageMultiplier defs env it * single dmg + (4.999999873689376e-06 : Float32))
    let cls ← if it.flag "melee" then tipT 2 else if it.flag "ranged" then tipT 3 else if it.flag "magic" then tipT 4
      else if it.flag "summon" then tipT 53 else tipT 55
    L := L.push (toString shown ++ cls)
    let crit? : Option Int :=
      if it.flag "melee" then some env.meleeCrit else if it.flag "ranged" then some env.rangedCrit
      else if it.flag "magic" then some env.magicCrit else none
    if let some c := crit? then
      L := L.push (toString (c - env.selectedItemCrit + visualCrit env it) ++ (← tipT 5))
    if it.useStyle != 0 && (!it.flag "summon" || (it.shoot ≥ 0 && defs.inSet "isAWhip" it.shoot)) then
      let a := it.useAnimation
      let n := if a ≤ 8 then 6 else if a ≤ 20 then 7 else if a ≤ 25 then 8 else if a ≤ 30 then 9
        else if a ≤ 35 then 10 else if a ≤ 45 then 11 else if a ≤ 55 then 12 else 13
      L := L.push (← tipT n)
    let mut kb := it.knockBack
    if it.flag "summon" then kb := kb + env.minionKB
    if env.magicQuiver && (it.useAmmo == defs.ammoArrow || it.useAmmo == defs.ammoStake) then
      kb := single (truncI (kb * 1.1))
    if env.selectedItemType == 3106 && t == 3106 then kb := kb + kb * (1 - env.stealth)
    let n := if kb == 0 then 14 else if kb.toFloat ≤ 1.5 then 15 else if kb ≤ 3 then 16 else if kb ≤ 4 then 17
      else if kb ≤ 6 then 18 else if kb ≤ 7 then 19 else if kb ≤ 9 then 20 else if kb ≤ 11 then 21 else 22
    L := L.push (← tipT n)
  if it.fishingPole > 0 then
    L := L.push (← textFormat dict te "GameUI.PrecentFishingPower" #[toString it.fishingPole])
    L := L.push (← text dict te "GameUI.BaitRequired")
  if it.bait > 0 then L := L.push (← textFormat dict te "GameUI.BaitPower" #[toString it.bait])
  if it.headSlot > 0 || it.bodySlot > 0 || it.legSlot > 0 || it.flag "accessory" || defs.inSet "projHook" it.shoot ||
      it.mountType != -1 ||
      (it.buffType > 0 && (defs.inSet "lightPet" it.buffType || defs.inSet "vanityPet" it.buffType)) then
    L := L.push (← if (t == 854 || t == 3035) && env.npcShopOpen then tipT 60 else tipT 23)
  if it.tileWand > 0 then L := L.push ((← tipT 52) ++ (← itemNameValue defs dict te it.tileWand))
  if it.flag "questItem" then L := L.push (← inter dict te 65)
  if it.flag "vanity" then L := L.push (← tipT 24)
  if it.defense > 0 then L := L.push (toString it.defense ++ (← tipT 25))
  if it.pick > 0 then L := L.push (toString it.pick ++ (← tipT 26))
  if it.axe > 0 then L := L.push (toString (it.axe * 5) ++ (← tipT 27))
  if it.hammer > 0 then L := L.push (toString it.hammer ++ (← tipT 28))
  if it.tileBoost != 0 then
    L := L.push ((if it.tileBoost > 0 then "+" else "") ++ toString it.tileBoost ++ (← tipT 54))
  if it.healLife > 0 then
    L := L.push (← if t == 3001 then textFormat dict te "CommonItemTooltip.RestoresLifeRange" #[toString it.healLife, "120"]
      else textFormat dict te "CommonItemTooltip.RestoresLife" #[toString it.healLife])
  if it.healMana > 0 then L := L.push (← textFormat dict te "CommonItemTooltip.RestoresMana" #[toString it.healMana])
  if it.buffType > 0 && !it.flag "summon" && !defs.inSet "lightPet" it.buffType && !defs.inSet "vanityPet" it.buffType then
    L := L.push (← text dict te "CommonItemTooltip.Buffs")
  else if it.healLife > 0 then L := L.push (← text dict te "CommonItemTooltip.Healing")
  else if it.healMana > 0 then L := L.push (← text dict te "CommonItemTooltip.ManaHealing")
  else if it.mountType > 0 && !defs.inSet "cart" it.mountType then L := L.push (← text dict te "CommonItemTooltip.Mount")
  if it.mana > 0 && !((t == 127 || t == 4347 || t == 4348 || t == 514) && env.spaceGun) then
    L := L.push (← textFormat dict te "CommonItemTooltip.UsesMana" #[toString (truncI (single it.mana * env.manaCost))])
  if it.createWall > 0 || it.createTile > -1 || t == 849 then
    if defs.inSet "placeTileOnAltUse" t || it.flag "consumable" then L := L.push (← tipT 33)
  else if it.ammo > 0 && !it.flag "notAmmo" then L := L.push (← tipT 34)
  else if it.flag "consumable" && !it.flag "chlorophyteExtractinatorConsumable" then L := L.push (← tipT 35)
  if it.flag "material" then L := L.push (← tipT 36)
  let wired := it.createTile > -1 && (defs.inSet "isATrigger" it.createTile || defs.inSet "isAMechanism" it.createTile)
  if wired && (it.createTile != 105 || defs.inSet "isWireableStatue" t) then
    L := L.push (← text dict te "CommonItemTooltip.Wireable")
  if it.createTile == 21 || it.createTile == 467 then L := L.push (← text dict te "CommonItemTooltip.Container")
  if it.createTile == 441 || it.createTile == 468 then L := L.push (← text dict te "CommonItemTooltip.WireTrigger")
  -- ItemTooltip: Lines / GetLine (ValidateTooltip: no text or !HasValue → none; else Value.Split('\n')).
  -- `Program.RunGame` sets the language (English, the default culture) before `Lang.InitializeLegacyLocalization`,
  -- so a key missing from the language becomes `LocalizedText(key, key)` whose `SetValue` writes
  -- `EnglishValue = key`: `HasValue` (`EnglishValue != Key`) is false and the item has no tooltip lines. A loaded
  -- English text keeps `EnglishValue` = its value (≠ its key).
  if let some key := it.tipKey.filter dict.exists then
    let lines := ((← text dict te key).splitOn "\n").toArray
    for i in [0:lines.size] do
      if i == 0 && defs.inSet "usesCursedByPlanteraTooltip" t && !env.downedPlantBoss then L := L.push (← tipT 59)
      else L := L.push lines[i]!
  if env.tenthAnniversaryWorld && t == 238 then L := L.push (← text dict te "CommonItemTooltip.WizardHatDuringAnniversary")
  if env.getGoodWorld && t == 1127 then L := L.push (← text dict te "CommonItemTooltip.BurningBlock")
  if env.mechdusa then
    if t == 556 || t == 557 || t == 544 then
      L := L.pop.push (← text dict te "CommonItemTooltip.MechSummonDuringEverything")
  else if t == 5334 then
    L := L.pop.pop.push (← text dict te "CommonItemTooltip.MechdusaSummonNotDuringEverything")
  if [3818, 3819, 3820, 3824, 3825, 3826, 3829, 3830, 3831, 3832, 3833, 3834].contains t &&
      !env.downedDD2EventAnyDifficulty then L := L.push (← misc dict te 104)
  if it.buffType > 0 && defs.inSet "isWellFed" it.buffType && env.expertMode then L := L.push (← misc dict te 40)
  if it.buffTime > 0 then
    let secs := it.buffTime / 60
    L := L.push (← if secs ≥ 60 then textFormat dict te "CommonItemTooltip.MinuteDuration" #[roundStr (Float.ofInt secs / 60)]
      else textFormat dict te "CommonItemTooltip.SecondDuration" #[roundStr (Float.ofInt it.buffTime / 60)])
  if [3262, 3282, 3283, 3284, 3285, 3286, 3316, 3315, 3317, 3291, 3389].contains t then L := L.push " "
  if it.flag "expert" then L := L.push (← text dict te "GameUI.Expert")
  if it.rare == -13 then L := L.push (← text dict te "GameUI.Master")
  if let some sb := it.setBonus then
    if it.flag "wornArmor" then throw s!"item {t}: worn armor set bonus (GetTooltipForWornArmor)"
    let part : Int := match sb.primary with | 1 => sb.head | 2 => sb.body | 3 => sb.legs | _ => 0
    let descKey := if sb.primary != 0 && part != t then
        (match sb.primary with | 1 => "UI.ItemSetBonusDecidedByHead" | 2 => "UI.ItemSetBonusDecidedByBody"
                               | _ => "UI.ItemSetBonusDecidedByLegs")
      else sb.descKey
    let desc ← text dict te descKey
    let te' : TextEnv := fun n => if n == "Description" then some (.str desc) else te n
    L := L.push (← text dict te' "UI.ItemSetBonusGeneral")
  if env.journey then throw s!"item {t}: Journey research lines (CreativeSacrificeNeeded) are not transcribed"
  return L

def searchLines (defs : Defs) (dict : Dict) (te : TextEnv) (env : Env) (t : Nat) : Except String (Array String) := do
  searchLinesOf defs dict te env (← defs.item t)

/-! ## Item visuals and the animation clock -/

/-- One `DrawAnimationVertical`'s mutable state. -/
structure AnimState where
  count : Nat
  ticks : Nat
  frame : Nat
  counter : Nat
  pingPong : Bool
  notActually : Bool
  deriving Repr, Inhabited

/-- `DrawAnimationVertical.Update` -/
def AnimState.update (a : AnimState) : AnimState :=
  if a.notActually then a
  else
    let c := a.counter + 1
    if c < a.ticks then { a with counter := c }
    else
      let f := a.frame + 1
      let limit := if a.pingPong then a.count * 2 - 2 else a.count
      { a with counter := 0, frame := if f ≥ limit then 0 else f }

/-- `Main.itemAnimations` of the animated item types, in type order. -/
abbrev AnimClock := Array (Nat × AnimState)

/-- The `TextureAssets.GlowMask[n]` asset. -/
def glowTexture (n : Int) : String := s!"Images/Glow_{n}"

/-- What `ItemSlot.DrawItemIcon` needs of an item, from its `SetDefaults` data and the XNB sizes. -/
def visualOf (defs : Defs) (it : ItemDef) (name : String) (texture : String) (tex : Nat × Nat)
    (textures : Std.HashMap String (Nat × Nat)) (clock : Std.HashMap Nat AnimState) : Except String ItemVisual := do
  let (animation, scryingOrb, special) : Option (Nat × Nat × Bool) × Bool × Option String := match it.anim with
    | .none => (none, false, none)
    | .vertical .. =>
      let a := clock.getD it.type default
      (some (a.count, a.frame, a.pingPong), false, none)
    | .other "DrawAnimationScryingOrb" n => (some (n, 0, false), true, none)
    | .other k _ => (none, false, some s!"Main.itemAnimations: {k}")
  let glow ← if it.glowMask == -1 then pure none else
    let g := glowTexture it.glowMask
    match textures[g]? with
    | some (w, h) => pure (some (g, w, h))
    | none => throw s!"texture size of {g} (item {it.type}'s glow mask) not supplied"
  return { type := it.type, texW := tex.1, texH := tex.2,
           color := OriginalForestGuideCraftGridLayout.Color.ofPacked it.color, alpha := it.alpha.toNat, name,
           animation, special, texture, glow, scryingOrb, shoot := it.shoot,
           iconPulse := defs.inSet "itemIconPulse" it.type, bossBag := defs.inSet "bossBag" it.type,
           trapSigned := defs.inSet "trapSigned" it.type, unsafeIndicator := defs.inSet "drawUnsafeIndicator" it.type }

/-! ## The catalogue -/

/-- The UI art the grid draws (`OriginalForestGuideCraftGridLayout`). -/
def uiTextures : Array String :=
  #["Images/UI/PanelBackground", "Images/UI/PanelBorder", "Images/UI/Bestiary/Button_Search_2",
    "Images/UI/Bestiary/Button_Search_Border", "Images/UI/SearchCancel", "Images/UI/CharCreation/Separator1",
    "Images/UI/Scrollbar2", "Images/UI/ScrollbarInner", "Images/UI/Creative/Infinite_Tabs_B_2",
    "Images/UI/Creative/Infinite_Icons", "Images/Inventory_Back", "Images/Inventory_Back4",
    "Images/Inventory_Back18", "Images/Inventory_Back24", "Images/Extra_54", "Images/UI/Craft",
    "Images/Black_Tile", "Images/UI/Craft_Toggle_2", "Images/UI/Craft_Toggle_3", "Images/UI/Craft_Toggle_4",
    "Images/UI/Craft_Toggle_5", "Images/Wires", "Images/Extra_258"]

/-- The item types the grid can draw or name: every recipe's create item and ingredients, every member of a recipe
group, and every item the guide slot accepts (`OriginalForestGuideCrafting.materialTable`'s `IsAMaterial`), plus the
tile-wand ammo items the tooltip lines name. -/
def itemTypes (rc : OriginalRecipeCatalog.Catalog) : Array Nat := Id.run do
  let mut s : Std.HashSet Nat := {}
  for r in rc.recipes do
    if r.create != 0 then s := s.insert r.create
    for i in r.ingredients do s := s.insert i.item
  for g in rc.groups do
    for i in g.items do s := s.insert i
    for i in g.validItems do s := s.insert i
  for entries in rc.lookups do
    for e in entries do
      if !rc.isGroupId e.id && e.id != 0 then s := s.insert e.id
  return (s.toArray.filter (· != 0)).qsort (· < ·)

/-- Every asset the catalogue needs: the UI art and, for each item of `itemTypes`, the texture
`TextureAssets.Item[t]` really holds (`Defs.itemTexture`), each once. -/
def textureNames (rc : OriginalRecipeCatalog.Catalog) : Except String (Array String) := do
  let defs ← metadata
  let mut seen : Std.HashSet String := {}
  let mut out := uiTextures
  for t in itemTypes rc do
    let it ← defs.item t
    let mut names := #[← defs.itemTexture it.type]
    if it.glowMask != -1 then names := names.push (glowTexture it.glowMask)
    for tex in names do
      if !seen.contains tex then
        seen := seen.insert tex
        out := out.push tex
  return out

/-- The catalogue with its animation clock and the inputs it was built from. -/
structure Built where
  catalog : Catalog
  clock : AnimClock
  defs : Defs
  rc : OriginalRecipeCatalog.Catalog
  textures : Std.HashMap String (Nat × Nat)
  env : Env

private def recipesFor (defs : Defs) (rc : OriginalRecipeCatalog.Catalog) (dict : Dict) (te : TextEnv) (env : Env) :
    Except String (Std.HashMap Nat Recipe) := do
  let mut lines : Std.HashMap Nat (Array String) := {}
  let mut fitsC : Std.HashMap Nat (Array Bool) := {}
  let mut out : Std.HashMap Nat Recipe := {}
  for r in rc.recipes do
    if r.create == 0 then break
    let ls ← match lines[r.create]? with
      | some l => pure l
      | none => do
        let l ← (searchLines defs dict te env r.create).mapError (s!"recipe {r.index}: " ++ ·)
        lines := lines.insert r.create l
        pure l
    let fs ← match fitsC[r.create]? with
      | some f => pure f
      | none => do
        let f ← fits defs r.create
        fitsC := fitsC.insert r.create f
        pure f
    out := out.insert r.index {
      index := r.index, create := r.create, createStack := r.createStack
      materials := r.ingredients.map fun i => (i.item, i.stack)
      requirements := ← requirementsText defs dict te env r
      fits := fs, searchLines := ls }
  return out

private def itemsFor (defs : Defs) (rc : OriginalRecipeCatalog.Catalog) (dict : Dict) (te : TextEnv)
    (textures : Std.HashMap String (Nat × Nat)) (clock : AnimClock) : Except String (Std.HashMap Nat ItemVisual) := do
  let cm : Std.HashMap Nat AnimState := clock.foldl (fun m (t, a) => m.insert t a) {}
  let mut out : Std.HashMap Nat ItemVisual := {}
  for t in itemTypes rc do
    let it ← defs.item t
    let texture ← defs.itemTexture it.type
    let tex ← match textures[texture]? with
      | some s => pure s
      | none => throw s!"texture size of {texture} (item {t}) not supplied"
    out := out.insert t (← visualOf defs it (← itemNameValue defs dict te it.type) texture tex textures cm)
  return out

/-- The initial `Main.itemAnimations` state (what `Main.InitializeItemAnimations` registers). -/
def initialClock (defs : Defs) : AnimClock := Id.run do
  let mut out := #[]
  for t in [1:defs.count] do
    let anim := match defs.items[t]? with
      | some it => it.anim
      | none => defs.slotAnim.getD t .none
    if let .vertical c tk f ct pp na := anim then
      out := out.push (t, { count := c, ticks := tk, frame := f, counter := ct, pingPong := pp, notActually := na })
  return out

/-- Build the complete catalogue. `te`: the dictionary's global substitutions for the current world, player and
keyboard (`OriginalResidentText.env`), which some item tooltips and names use. -/
def build (rc : OriginalRecipeCatalog.Catalog) (dict : Dict) (te : TextEnv) (mouseText itemStack : Font)
    (textures : Std.HashMap String (Nat × Nat)) (env : Env := {}) : Except String Built := do
  let defs ← metadata
  let clock := initialClock defs
  let recipes ← recipesFor defs rc dict te env
  let items ← itemsFor defs rc dict te textures clock
  let catalog : Catalog := {
    recipes, items, textures, mouseText, itemStack
    title := ← inter dict te 25, guideEmpty := ← inter dict te 24, guideShowing := ← inter dict te 21
    searchHint := ← text dict te "UI.PlayerNameSlot" }
  return { catalog, clock, defs, rc, textures, env }

/-- `build` with the sizes read through the caller's XNB header reader (`OriginalDesktopGuideGrid.textureSize`). -/
def load (rc : OriginalRecipeCatalog.Catalog) (dict : Dict) (te : TextEnv) (mouseText itemStack : Font)
    (textureSize : String → IO (Nat × Nat)) (env : Env := {}) : IO Built := do
  let mut textures : Std.HashMap String (Nat × Nat) := {}
  for name in ← IO.ofExcept ((textureNames rc).mapError IO.userError) do
    textures := textures.insert name (← textureSize name)
  IO.ofExcept ((build rc dict te mouseText itemStack textures env).mapError IO.userError)

/-- One `Main.DoUpdate` tick of what the icons read: `DoUpdate_AnimateDiscoRGB` and
`DoUpdate_AnimateVisualPlayerAura` (`catalog.icons`), then `DoUpdate_AnimateItemIcons` (one `DrawAnimation.Update()`
per registered animation). Call once per update tick (it runs while the game is paused as well). -/
def Built.tick (b : Built) : Built := Id.run do
  let b := { b with catalog := { b.catalog with icons := b.catalog.icons.updateTick } }
  let clock := b.clock.map fun (t, a) => (t, a.update)
  let mut items := b.catalog.items
  for (t, a) in clock do
    if let some v := items[t]? then
      if let some (n, _, pp) := v.animation then
        items := items.insert t { v with animation := some (n, a.frame, pp) }
  return { b with clock, catalog := { b.catalog with items } }

/-- `Main.DoDraw`'s `essScale` step (IL_139d … IL_13f3), once per drawn world frame. -/
def Built.drawTick (b : Built) (gamePaused : Bool) : Built :=
  { b with catalog := { b.catalog with icons := b.catalog.icons.drawTick gamePaused } }

/-- Set or replace the icon globals from the desktop's own state (`miscCounter`, `GlobalTimeWrappedHourly`,
`AnyoneToSpectate`, or its own Disco / gFade / essScale clock). -/
def Built.withIcons (b : Built) (f : OriginalItemIcon.IconRuntime → OriginalItemIcon.IconRuntime) : Built :=
  { b with catalog := { b.catalog with icons := f b.catalog.icons } }

/-- Recompute the requirement lines and search lines for another `Env` or text environment (player stats, world
flags, key bindings); item visuals and the clock are kept. -/
def Built.withEnv (b : Built) (dict : Dict) (te : TextEnv) (env : Env) : Except String Built := do
  let recipes ← recipesFor b.defs b.rc dict te env
  return { b with env, catalog := { b.catalog with recipes } }

end Lean4Terria.OriginalForestGuideCraftGridCatalog
