import Lean4Terria.OriginalItems
import Lean4Terria.OriginalItemSorting.Data

/-!
# Sort Items (Terraria 1.4.5.8)

`ItemSorting.SortChest` sorts a world chest or one of the four personal
stores (Piggy Bank, Safe, Defender's Forge, Void Vault) through
`ItemSorting.Sort(feedback, inv, ignoreSlots = [])`. The inventory's Sort
button runs `ItemSorting.SortInventory()`: `SortCoins`, `SortAmmo`, then the
same `Sort` with the hotbar, coin, ammo and mouse slots ignored.

`Sort` keeps favorites and ignored slots in place, merges partial stacks,
splits the remaining items into 52 layers, orders each layer with its own
comparison (equal keys keep the lower original slot first), and refills the
free slots from the lowest index. The four weapon class layers come first,
ranked by the player's current damage multipliers.

Layer membership follows `ItemSorting._layerWhiteLists`, read from the
original program (`Data.records`). Each layer's filter also runs on the
actual item, but every field it reads is fixed per item type, except `rare`
in *Last - Materials*, which only a prefix can change; no type in that
whitelist accepts a prefix. Comparators read the default instance
(`ContentSamples.ItemsByType` for the `Original*` properties) plus the live
`stack`. `value` is read live in *Last - Materials* and *Last - Trash*,
where again no type accepts a prefix, so the default value is used.
`Item.Name` is the en-US text.
-/

namespace Lean4Terria.OriginalItemSorting

open OriginalItems (Inventory Stack)
open OriginalItemStats (maxStack)

/-- The 52 sorting layers in `ItemSorting.SetupWhiteLists` order; the
position is `ItemSorting.GetSortingLayerIndex`. -/
inductive Layer where
  | weaponsMelee | weaponsRanged | weaponsMagic | weaponsMinions | weaponsAssorted | weaponsAmmo
  | toolsPicksaws | toolsHamaxes | toolsPickaxes | toolsAxes | toolsHammers | toolsTerraforming
  | toolsFishing | toolsGolf | toolsInstruments | toolsKeys | toolsKites | toolsAmmoLeftovers
  | toolsMisc | armorCombat | armorVanity | armorAccessories | equipGrapple | equipMount
  | equipCart | equipLightPet | equipVanityPet | potionsDyes | potionsHairDyes | potionsLife
  | potionsJustTheMushroom | potionsMana | potionsElixirs | potionsBuffs | potionsFood
  | miscImportants | miscPainting | miscWiring | miscMaterials | miscJustTheGlowingMushroom
  | miscRopes | miscHerbsAndSeeds | miscAcorns | miscGems | miscBossBags | miscCritters
  | miscExtractinator | lastMaterials | lastTilesImportant | lastTilesCommon | lastNotTrash
  | lastTrash
  deriving BEq, Repr, Inhabited

namespace Layer

def all : Array Layer := #[
  weaponsMelee, weaponsRanged, weaponsMagic, weaponsMinions, weaponsAssorted, weaponsAmmo,
  toolsPicksaws, toolsHamaxes, toolsPickaxes, toolsAxes, toolsHammers, toolsTerraforming,
  toolsFishing, toolsGolf, toolsInstruments, toolsKeys, toolsKites, toolsAmmoLeftovers,
  toolsMisc, armorCombat, armorVanity, armorAccessories, equipGrapple, equipMount,
  equipCart, equipLightPet, equipVanityPet, potionsDyes, potionsHairDyes, potionsLife,
  potionsJustTheMushroom, potionsMana, potionsElixirs, potionsBuffs, potionsFood,
  miscImportants, miscPainting, miscWiring, miscMaterials, miscJustTheGlowingMushroom,
  miscRopes, miscHerbsAndSeeds, miscAcorns, miscGems, miscBossBags, miscCritters,
  miscExtractinator, lastMaterials, lastTilesImportant, lastTilesCommon, lastNotTrash,
  lastTrash]

def index (layer : Layer) : Nat := (all.findIdx? (· == layer)).getD 0

/-- The whitelist key, such as `"Weapons - Melee"`. -/
def name (layer : Layer) : String := Data.layerNames[layer.index]!

end Layer

/-- Default-instance fields of one item type, decoded from `Data.records`.
Only the fields its layer's comparator reads are stored. -/
structure Info where
  layer : Option Nat := none
  rarity : Int := 0
  damage : Int := 0
  defense : Int := 0
  pick : Int := 0
  axe : Int := 0
  hammer : Int := 0
  priority : Int := 0
  fishingPole : Int := 0
  bait : Int := 0
  questItem : Int := 0
  crate : Int := 0
  basicFish : Int := 0
  vanity : Int := 0
  healLife : Int := 0
  healMana : Int := 0
  foodPriority : Int := 0
  dye : Int := 0
  hairDye : Int := 0
  paint : Int := 0
  paintCoating : Int := 0
  makeNPC : Int := 0
  value : Int := 0
  nameRank : Int := 0
  deriving Inhabited, Repr

private def Info.setField (info : Info) (key : Char) (value : Int) : Info :=
  match key with
  | 'r' => { info with rarity := value }
  | 'd' => { info with damage := value }
  | 'D' => { info with defense := value }
  | 'p' => { info with pick := value }
  | 'a' => { info with axe := value }
  | 'h' => { info with hammer := value }
  | 'P' => { info with priority := value }
  | 'f' => { info with fishingPole := value }
  | 'b' => { info with bait := value }
  | 'q' => { info with questItem := value }
  | 'c' => { info with crate := value }
  | 'B' => { info with basicFish := value }
  | 'v' => { info with vanity := value }
  | 'l' => { info with healLife := value }
  | 'm' => { info with healMana := value }
  | 'F' => { info with foodPriority := value }
  | 'y' => { info with dye := value }
  | 'Y' => { info with hairDye := value }
  | 'i' => { info with paint := value }
  | 'I' => { info with paintCoating := value }
  | 'n' => { info with makeNPC := value }
  | '$' => { info with value := value }
  | 'N' => { info with nameRank := value }
  | _ => info

/-- A record is the layer number, then letter-tagged signed decimals. -/
private def decodeRecord (record : String) : Info := Id.run do
  if record == "-" then return {}
  let mut info : Info := {}
  let mut key : Option Char := none
  let mut negative := false
  let mut digits : Nat := 0
  let mut seen := false
  let flush (info : Info) (key : Option Char) (negative : Bool) (digits : Nat) : Info :=
    let value : Int := if negative then -(digits : Int) else digits
    match key with
    | none => { info with layer := some digits }
    | some key => info.setField key value
  for c in record.toList do
    if c.isDigit then
      digits := digits * 10 + (c.toNat - '0'.toNat)
      seen := true
    else if c == '-' then
      negative := true
    else
      if seen || key.isSome then info := flush info key negative digits
      key := some c
      negative := false
      digits := 0
      seen := false
  return flush info key negative digits

/-- Per-type fields for all `Data.itemCount` item types. -/
def infos : Array Info :=
  ((Data.records.splitOn " ").filter (· != "")).toArray.map decodeRecord

def info (item : UInt16) : Info := infos[item.toNat]?.getD {}

/-- `ItemSorting.GetSortingLayer`: the layer whose whitelist holds the type. -/
def layerOf (item : UInt16) : Option Layer :=
  (info item).layer.bind (Layer.all[·]?)

/-- The live damage multipliers `SetupSortingPriorities` reads from the local player. -/
structure DamageMultipliers where
  melee : Float32 := 1
  ranged : Float32 := 1
  magic : Float32 := 1
  minion : Float32 := 1

/-- `Single.CompareTo`: NaN sorts below every number and equals NaN. -/
def compareSingle (a b : Float32) : Ordering :=
  if a < b then .lt
  else if a > b then .gt
  else if a == b then .eq
  else if a.isNaN then (if b.isNaN then .eq else .lt)
  else .gt

/-- `SetupSortingPriorities`: `AddSortingPrioritiesBasedOnPlayerDamage` ranks the
four weapon class layers by multiplier, highest first (`Descending`, ties by
class index), then the other 48 layers follow in whitelist order. -/
def layerOrder (multipliers : DamageMultipliers) : Array Layer :=
  let ranked : Array (Float32 × Nat × Layer) := #[
    (multipliers.melee, 0, .weaponsMelee), (multipliers.ranged, 1, .weaponsRanged),
    (multipliers.magic, 2, .weaponsMagic), (multipliers.minion, 3, .weaponsMinions)]
  let ranked := ranked.qsort fun (a, i, _) (b, j, _) =>
    ((compareSingle b a).then (compare i j)) == .lt
  ranked.map (·.2.2) ++ Layer.all.extract 4 Layer.all.size

/-- A sortable slot as the comparators see it. -/
structure Entry where
  slot : Nat
  stack : Stack
  info : Info

private def ascending (x y : Int) : Ordering := compare x y
private def descending (x y : Int) : Ordering := compare y x

/-- `ItemSortingLayers.CompareWithPrioritySet` for two in-range types. -/
def compareWithPrioritySet (x y : Int) : Ordering :=
  if x ≥ 0 && y < 0 then .lt
  else if x < 0 && y ≥ 0 then .gt
  else if x < 0 && y < 0 then .eq
  else compare x y

/-- The comparison each layer hands to `SortIndicesStable`, before the slot
tie-break. `x` is the left operand of the C# `Comparison<int>`. -/
def compareIn (layer : Layer) (x y : Entry) : Ordering :=
  let a := x.info
  let b := y.info
  let rarity := descending a.rarity b.rarity
  let damage := descending a.damage b.damage
  let stack := descending x.stack.count y.stack.count
  let type := ascending x.stack.item.toNat y.stack.item.toNat
  let priority := ascending a.priority b.priority
  let prioritySet := compareWithPrioritySet a.priority b.priority
  match layer with
  | .weaponsMelee | .weaponsMagic | .weaponsMinions | .weaponsAssorted | .weaponsAmmo
  | .toolsAmmoLeftovers => rarity.then damage
  | .weaponsRanged => prioritySet.then rarity |>.then damage
  | .toolsPicksaws | .toolsPickaxes => ascending a.pick b.pick
  | .toolsHamaxes | .toolsAxes => ascending a.axe b.axe
  | .toolsHammers => ascending a.hammer b.hammer
  | .toolsTerraforming | .miscImportants => priority.then stack
  | .toolsFishing =>
    prioritySet.then (descending a.fishingPole b.fishingPole)
      |>.then (descending a.bait b.bait) |>.then (descending a.questItem b.questItem)
      |>.then (descending a.crate b.crate) |>.then (descending a.basicFish b.basicFish)
      |>.then rarity |>.then stack
  | .toolsGolf | .toolsInstruments | .toolsKeys | .toolsKites | .toolsMisc =>
    priority.then rarity |>.then stack
  | .armorCombat => rarity.then (descending a.defense b.defense) |>.then type
  | .armorVanity | .equipGrapple | .equipMount | .equipCart | .equipLightPet
  | .equipVanityPet => rarity.then type
  | .armorAccessories =>
    (ascending a.vanity b.vanity).then rarity |>.then (descending a.defense b.defense) |>.then type
  | .potionsDyes =>
    rarity.then (descending a.dye b.dye) |>.then prioritySet |>.then stack
  | .potionsHairDyes => rarity.then (descending a.hairDye b.hairDye) |>.then stack
  | .potionsLife | .potionsElixirs => (descending a.healLife b.healLife).then stack
  | .potionsJustTheMushroom | .miscJustTheGlowingMushroom => stack
  | .potionsMana => (descending a.healMana b.healMana).then stack
  | .potionsBuffs => prioritySet.then rarity |>.then type |>.then stack
  | .potionsFood =>
    (descending a.foodPriority b.foodPriority).then rarity |>.then type |>.then stack
  | .miscPainting =>
    (descending a.priority b.priority).then (ascending a.paint b.paint)
      |>.then (ascending a.paintCoating b.paintCoating) |>.then stack
  | .miscWiring =>
    (descending a.priority b.priority).then rarity
      |>.then (descending x.stack.item.toNat y.stack.item.toNat) |>.then stack
  | .miscMaterials | .miscRopes | .miscHerbsAndSeeds | .miscAcorns | .miscGems
  | .miscExtractinator => (descending a.priority b.priority).then stack
  | .miscBossBags => priority.then rarity |>.then stack
  | .miscCritters => (ascending a.makeNPC b.makeNPC).then stack
  | .lastMaterials => rarity.then (descending a.value b.value) |>.then stack
  | .lastTilesImportant | .lastTilesCommon => (ascending a.nameRank b.nameRank).then stack
  | .lastNotTrash => rarity.then (ascending a.nameRank b.nameRank) |>.then stack
  | .lastTrash => (descending a.value b.value).then stack

private def isEmpty : Option Stack → Bool
  | none => true
  | some stack => stack.count == 0 || stack.item == 0

/-- `Item.CanStack`: same type and prefix. -/
private def canStack (a b : Stack) : Bool := a.item == b.item && a.prefixId == b.prefixId

/-- The first part of `Sort`: every non-favorite item tops up later partial
stacks of the same type and prefix, in slot order. A source that empties
leaves the list and the same item is tried again. -/
private def mergePartialStacks (initial : Inventory) (initialSlots : Array Nat) :
    Inventory × Array Nat := Id.run do
  let mut inv := initial
  let mut slots := initialSlots
  let mut i := 0
  while i < slots.size do
    let some a := inv[slots[i]!]?.join | i := i + 1; continue
    let limit := maxStack a.item
    if a.count ≥ limit then
      i := i + 1
      continue
    let mut room := limit - a.count
    let mut retry := false
    let mut j := i + 1
    while j < slots.size do
      let some a := inv[slots[i]!]?.join | break
      let some b := inv[slots[j]!]?.join | j := j + 1; continue
      if canStack a b && b.count != maxStack b.item then
        let moved := min room b.count
        inv := inv.set! slots[i]! (some { a with count := a.count + moved })
        room := room - moved
        if b.count - moved == 0 then
          inv := inv.set! slots[j]! none
          slots := slots.eraseIdx! j
          retry := true
          break
        inv := inv.set! slots[j]! (some { b with count := b.count - moved })
        if room == 0 then break
      j := j + 1
    if !retry then i := i + 1
  return (inv, slots)

structure Sorted where
  slots : Inventory
  glow : Array (Nat × Float32) := #[]

/-- `ItemSorting.Sort` assigns one hue to each nonempty sorting layer. -/
def sortWithGlow (order : Array Layer) (ignored : Nat → Bool) (initial : Inventory) : Sorted := Id.run do
  let mut candidates : Array Nat := #[]
  for k in [:initial.size] do
    if ignored k then continue
    let some stack := initial[k]?.join | continue
    if stack.count == 0 || stack.item == 0 || stack.favorite then continue
    candidates := candidates.push k
  let (inv, slots) := mergePartialStacks initial candidates
  let mut available := slots
  for k in [:inv.size] do
    if ignored k || slots.contains k then continue
    if isEmpty inv[k]! then available := available.push k
  available := available.qsort (· < ·)
  let entry (k : Nat) : Entry :=
    let stack := inv[k]!.getD (.ofItem 0)
    { slot := k, stack, info := info stack.item }
  let mut remaining := slots
  let mut sorted : Array Nat := #[]
  let mut counts : Array Nat := #[]
  for layer in order do
    let members := remaining.filter fun k => (entry k).info.layer == some layer.index
    if members.isEmpty then continue
    remaining := remaining.filter (!members.contains ·)
    counts := counts.push members.size
    sorted := sorted ++ members.qsort fun x y =>
      ((compareIn layer (entry x) (entry y)).then (compare x y)) == .lt
  sorted := sorted ++ remaining
  let cache := sorted.map (inv[·]!)
  let mut result := inv
  let mut glow := #[]
  let hueStep : Float32 := if counts.isEmpty then 0 else 1 / counts.size.toFloat32
  let mut hue := hueStep / 2
  let mut group := 0
  let mut groupEnd := counts[0]?.getD 0
  for k in sorted do result := result.set! k none
  for n in [:cache.size] do
    let target := available[n]!
    result := result.set! target cache[n]!
    if group < counts.size then
      glow := glow.push (target, hue)
      if n + 1 == groupEnd then
        group := group + 1
        groupEnd := groupEnd + counts[group]?.getD 0
        hue := hue + hueStep
  return ⟨result, glow⟩

def sortWith (order : Array Layer) (ignored : Nat → Bool) (initial : Inventory) : Inventory :=
  (sortWithGlow order ignored initial).slots

def sortChestWithGlow (multipliers : DamageMultipliers) (container : Inventory) : Sorted :=
  sortWithGlow (layerOrder multipliers) (fun _ => false) container

/-- `ItemSorting.SortChest` for an open world chest or personal store. -/
def sortChest (multipliers : DamageMultipliers) (container : Inventory) : Inventory :=
  (sortChestWithGlow multipliers container).slots

/-! ## Inventory Sort button -/

/-- `Item.CanFillEmptyAmmoSlot()` on the default instance. -/
def canFillEmptyAmmoSlot (item : UInt16) : Bool :=
  let id := item.toNat
  !Data.cannotFillEmptyAmmoSlot.any fun (first, last) => first ≤ id && id ≤ last

/-- The 19 slots `SortInventory` passes to `Sort`: hotbar 0–9, coins 50–53,
ammo 54–57 and the mouse slot 58. -/
def inventoryIgnored (slot : Nat) : Bool := slot < 10 || 50 ≤ slot

private def coinSlot (slot : Option Stack) : Bool :=
  slot.any fun stack => 71 ≤ stack.item && stack.item ≤ 74 && stack.count > 0

private def coinValue (item : UInt16) : Nat :=
  match item with
  | 71 => 1
  | 72 => 100
  | 73 => 10000
  | 74 => 1000000
  | _ => 0

/-- `ItemSorting.SortCoins`. Coins in slots 0–57 are counted and split into
denominations. The coin slots are cleared only when they are at least as many
as the stacks needed (99 per copper, silver or gold stack; platinum adds a
stack only for each full 9,999 it exceeds). The new stacks go highest coin
first into the first empty coin slot, else the first empty main slot. After
100 placements any rest is returned to be spawned near the player. -/
def sortCoins (initial : Inventory) : Inventory × Array Stack := Id.run do
  let mut inv := initial
  let limit := min inv.size 58
  let mut total : Nat := 0
  for k in [:limit] do
    if let some stack := inv[k]?.join then total := total + coinValue stack.item * stack.count
  let mut split : Array Nat := #[total % 100, total / 100 % 100, total / 10000 % 100, total / 1000000]
  let common := 9999
  let mut needed := 0
  for c in [:3] do
    if split[c]! > 0 then needed := needed + (split[c]! + 98) / 99
  let mut platinum := split[3]!
  while platinum > common do
    platinum := platinum - common
    needed := needed + 1
  let current := ((List.range limit).filter fun k => coinSlot inv[k]!).length
  if current < needed then return (inv, #[])
  for k in [:limit] do
    if coinSlot inv[k]! then inv := inv.set! k none
  for _ in [:100] do
    let some c := (#[3, 2, 1, 0] : Array Nat).find? (split[·]! > 0) | return (inv, #[])
    let amount := if c == 3 then min split[c]! common else split[c]!
    let empty := ((List.range 4).map (50 + ·) ++ List.range 50).find? fun k =>
      k < inv.size && isEmpty inv[k]!
    if let some k := empty then
      inv := inv.set! k (some (.ofItem (71 + c).toUInt16 amount))
      split := split.set! c (split[c]! - amount)
  let drops := (#[3, 2, 1, 0] : Array Nat).filterMap fun c =>
    if split[c]! > 0 then some (.ofItem (71 + c).toUInt16 split[c]!) else none
  return (inv, drops)

/-- `ItemSorting.RefillItemStack`: top up `target` from same-type,
non-favorite stacks in `[start, stop)`, ignoring prefixes. -/
private def refill (initial : Inventory) (target start stop : Nat) : Inventory := Id.run do
  let mut inv := initial
  let some item := inv[target]?.join | return inv
  let mut room : Int := (maxStack item.item : Int) - item.count
  if room ≤ 0 then return inv
  for k in [start:min stop inv.size] do
    let some item := inv[target]?.join | return inv
    let some other := inv[k]?.join | continue
    if other.count < 1 || other.item != item.item || other.favorite then continue
    let take := min other.count room.toNat
    room := room - take
    inv := inv.set! target (some { item with count := item.count + take })
    inv := inv.set! k (if other.count - take == 0 then none else some { other with count := other.count - take })
    if room ≤ 0 then break
  return inv

private def swap (inv : Inventory) (a b : Nat) : Inventory :=
  (inv.set! a inv[b]!).set! b inv[a]!

private def hasAmmo (slot : Option Stack) : Bool :=
  !isEmpty slot && slot.any (OriginalItemStats.hasAmmoType ·.item)

/-- `ItemSorting.ClearAmmoSlotSpaces`: partial ammo stacks pull from later ammo
slots (a favorite from all four), then non-favorites slide up into a free
earlier ammo slot. -/
def clearAmmoSlotSpaces (initial : Inventory) : Inventory := Id.run do
  let mut inv := initial
  if inv.size < 58 then return inv
  for k in [54:58] do
    let slot := inv[k]!
    let some stack := slot | continue
    if !hasAmmo slot || stack.count ≥ maxStack stack.item then continue
    inv := refill inv k (if stack.favorite then 54 else k + 1) 58
  for k in [54:58] do
    let some stack := inv[k]?.join | continue
    if stack.item == 0 || stack.favorite then continue
    if let some free := (List.range (k - 54)).map (54 + ·) |>.find? (isEmpty inv[·]!) then
      inv := swap inv free k
  return inv

/-- `ItemSorting.FillAmmoFromInventory`: refill ammo slots from the main
inventory, then move main-inventory stacks into empty ammo slots, first for
types already in an ammo slot, then any item that fits there. -/
def fillAmmoFromInventory (initial : Inventory) : Inventory := Id.run do
  let mut inv := initial
  if inv.size < 58 then return inv
  let mut accepted : Array UInt16 := #[]
  let mut empty : Array Nat := #[]
  for k in [54:58] do
    let slot := inv[k]!
    if isEmpty slot then
      empty := empty.push k
    else if hasAmmo slot then
      let item := (slot.map (·.item)).getD 0
      if !accepted.contains item then accepted := accepted.push item
      inv := refill inv k 0 50
  for pass in [0, 1] do
    if empty.isEmpty then return inv
    for k in [:50] do
      let some stack := inv[k]?.join | continue
      if stack.count < 1 || !canFillEmptyAmmoSlot stack.item || stack.favorite then continue
      let fits := if pass == 0 then accepted.contains stack.item
        else OriginalItemStats.fitsAmmoSlot stack.item
      if !fits then continue
      let target := empty[0]!
      empty := empty.eraseIdx! 0
      inv := refill (swap inv k target) target 0 50
      if empty.isEmpty then break
  return inv

def prepareInventoryCoins (inventory : Inventory) : Inventory × Array Stack :=
  let coinGun := (inventory.extract 0 58).any fun slot =>
    slot.any fun stack => stack.item == 905 && stack.count > 0
  if coinGun then (inventory, #[]) else sortCoins inventory

/-- Finishes sorting after the player has received any coins from `SortCoins`. -/
def sortInventorySlots (multipliers : DamageMultipliers) (inventory : Inventory) : Sorted :=
  let inventory := fillAmmoFromInventory (clearAmmoSlotSpaces inventory)
  let sorted := sortWithGlow (layerOrder multipliers) inventoryIgnored inventory
  -- FillAmmoFromInventory sets all four timers; ItemSlot.Draw skips air and favorites.
  let ammoGlow := (Array.range (min 58 inventory.size - 54)).map fun index => (54 + index, 0.31)
  { sorted with glow := sorted.glow ++ ammoGlow }

/-- Sort as a pure slot operation, returning any coins to give to the player. -/
def sortInventory (multipliers : DamageMultipliers) (inventory : Inventory)
    (locked : Bool := false) : Inventory × Array Stack :=
  if locked then (inventory, #[])
  else
    let (inventory, drops) := prepareInventoryCoins inventory
    ((sortInventorySlots multipliers inventory).slots, drops)

end Lean4Terria.OriginalItemSorting
