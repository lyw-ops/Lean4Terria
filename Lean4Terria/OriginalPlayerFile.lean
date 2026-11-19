import Lean4Terria.OriginalBanks
import Lean4Terria.OriginalPlayerEquipment.Model
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalSaveCopy
import Std.Time.DateTime.Timestamp

namespace Lean4Terria.OriginalPlayerFile

open OriginalItems (Inventory Stack)
open OriginalBanks (Kind Stores)
open OriginalPlayerEquipment (Equipment Loadout Slot)

private def mainSlots : Nat := 50
private def storedSlots : Nat := 58
private def slotSize : Nat := 10

structure Appearance where
  hair : UInt32 := 0
  skinVariant : UInt8 := 0
  hairColor : UInt32 := 0xd59450ff
  skinColor : UInt32 := 0xf7c392ff
  eyeColor : UInt32 := 0x5d5e75ff
  shirtColor : UInt32 := 0x62b9a9ff
  underShirtColor : UInt32 := 0x62b9a9ff
  pantsColor : UInt32 := 0x886859ff
  shoeColor : UInt32 := 0x876240ff
  deriving Inhabited

/-- `Player.voiceVariant` and `Player.voicePitchOffset` as `Player.Serialize` writes them after the
loadouts (release 280 and 281 on): one byte and one single. They do not follow the skin variant;
a new `Player` has variant 1. `Player.voiceOverride` is run-time state and is not in the file. -/
structure Voice where
  variant : UInt8 := 1
  pitchOffset : Float32 := 0
  deriving Repr, Inhabited

/-- The variant the game uses after loading: `LoadPlayer_LastMinuteFixes` (IL_0020..002e) clamps it
to 1..4. The pitch offset is used as stored. -/
def Voice.loadedVariant (voice : Voice) : Int :=
  max 1 (min 4 voice.variant.toNat)

structure Archive where
  private mk ::
  name : String
  appearance : Appearance
  inventory : Inventory
  coinAndAmmo : Inventory
  banks : Stores
  mouseItem : Option Stack
  unlockedBiomeTorches : Bool
  ateArtisanBread : Bool
  /-- `Player.usedGalaxyPearl` (Serialize IL_0167..016e, the byte after `usedArcaneCrystal`). -/
  usedGalaxyPearl : Bool := false
  health : OriginalPlayerLiquidHealth.Health
  /-- `armor`, `dye`, `miscEquips`, `miscDyes`, visibility, `extraAccessory` and the loadouts. -/
  equipment : Equipment := {}
  /-- The voice stored in the file. -/
  voice : Voice := {}
  private savedAt : Int
  private plaintext : ByteArray
  private inventoryOffset : Nat
  private deathOffset : Nat
  private temporaryOffset : Nat
  /-- Offset of `CurrentLoadoutIndex`, in the plaintext as read. -/
  private loadoutOffset : Nat := 0

private def getU32 (bytes : ByteArray) (offset : Nat) : UInt32 := Id.run do
  let mut value := 0
  for i in [:4] do
    value := value ||| (bytes[offset + i]!.toUInt32 <<< (8 * i).toUInt32)
  return value

private def putU32 (bytes : ByteArray) (offset : Nat) (value : UInt32) : ByteArray := Id.run do
  let mut bytes := bytes
  for i in [:4] do
    bytes := bytes.set! (offset + i) (value >>> (8 * i).toUInt32).toUInt8
  return bytes

private def getI32 (bytes : ByteArray) (offset : Nat) : Int :=
  (getU32 bytes offset).toInt32.toInt

private def getI64 (bytes : ByteArray) (offset : Nat) : Int :=
  let low := (getU32 bytes offset).toUInt64
  let high := (getU32 bytes (offset + 4)).toUInt64
  (low ||| (high <<< 32)).toInt64.toInt

/-- `DateTime.UtcNow.ToBinary()`, including the UTC kind bit. -/
def savedTimeNow : IO Int := do
  let utc ← Std.Time.Timestamp.now
  return utc.toNanosecondsSinceUnixEpoch.val / 100 + 621355968000000000 + 2 ^ 62

private def getColor (bytes : ByteArray) (offset : Nat) : UInt32 :=
  (bytes[offset]!.toUInt32 <<< 24) ||| (bytes[offset + 1]!.toUInt32 <<< 16) |||
    (bytes[offset + 2]!.toUInt32 <<< 8) ||| 255

private def readString (bytes : ByteArray) (offset : Nat) : Except String (String × Nat) := do
  let mut size := 0
  for i in [:5] do
    let index := offset + i
    let byte := (bytes[index]?.getD 255).toNat
    size := size + (byte % 128) * (128 ^ i)
    if byte < 128 then
      let start := index + 1
      let stop := start + size
      if size > 2147483647 || stop > bytes.size then
        throw "Invalid Terraria character string length."
      let some value := String.fromUTF8? (bytes.extract start stop)
        | throw "Invalid UTF-8 in Terraria character file."
      return (value, stop)
  throw "Invalid Terraria character string length."

private def parseStack (bytes : ByteArray) (offset : Nat) (hasFavorite : Bool) :
    Except String (Option Stack) := do
  let length := if hasFavorite then slotSize else slotSize - 1
  if offset + length > bytes.size then throw "Truncated Terraria item record."
  let item := getU32 bytes offset
  let count := getU32 bytes (offset + 4)
  if item > 65535 || count > 2147483647 then throw "Unsupported Terraria item type or stack."
  if item == 0 || count == 0 then return none
  return some {
    item := item.toUInt16
    count := count.toNat
    prefixId := bytes[offset + 8]!
    favorite := hasFavorite && bytes[offset + 9]! != 0
  }

private def slotStride (hasFavorite : Bool) : Nat := if hasFavorite then slotSize else slotSize - 1

private def readSlots (bytes : ByteArray) (start slots : Nat) (hasFavorite : Bool)
    (label : String) : Except String Inventory := do
  let stride := slotStride hasFavorite
  let mut inventory := Array.emptyWithCapacity slots
  for index in [:slots] do
    let slot ← (parseStack bytes (start + index * stride) hasFavorite).mapError fun message =>
      s!"{label} slot {index + 1}. {message}"
    inventory := inventory.push slot
  return inventory

private def bankStart (inventoryOffset : Nat) (kind : Kind) : Nat :=
  -- Five misc equipment and five dyes precede the four personal stores.
  inventoryOffset + storedSlots * slotSize + 5 * 2 * 5 + kind.index * OriginalBanks.slotCount * 9

private def voidInfoOffset (inventoryOffset : Nat) : Nat :=
  bankStart inventoryOffset .voidVault + OriginalBanks.slotCount * slotSize

/-- Locate the temporary-slot bitfield in Player.Serialize. The inventory
is followed by equipment, personal banks, buffs, world spawn entries and
player flags, then the research dictionary. -/
private def findDeathOffset (bytes : ByteArray) (buffOffset : Nat) : Except String Nat := do
  let mut offset := buffOffset + OriginalPlayerLiquidHealth.maxBuffs * 8
  for _ in [:200] do
    if offset + 4 > bytes.size then throw "Truncated Terraria spawn list."
    if getU32 bytes offset == 0xffffffff then
      offset := offset + 4
      break
    let (_, endOfSpawn) ← readString bytes (offset + 12)
    offset := endOfSpawn
  -- Hotbar lock, info display flags, angler count, D-pad bindings,
  -- builder accessory statuses, and bartender dialogue.
  offset := offset + (1 + 13 + 4 + 4 * 4 + 12 * 4 + 4)
  if offset ≥ bytes.size then throw "Truncated Terraria player flags."
  return offset

private def findTemporarySlots (bytes : ByteArray) (deathOffset : Nat) : Except String Nat := do
  let mut offset := deathOffset
  let some dead := bytes[offset]? | throw "Truncated Terraria player flags."
  offset := offset + (if dead == 0 then 1 else 5)
  -- Last-save timestamp, golf score, research format flag and item count.
  offset := offset + 8 + 4 + 1
  if offset + 4 > bytes.size then throw "Truncated Terraria research section."
  let entries := (getU32 bytes offset).toNat
  offset := offset + 4
  if entries > (bytes.size - offset) / 5 then throw "Invalid Terraria research section size."
  for _ in [:entries] do
    let (_, endOfKey) ← readString bytes offset
    offset := endOfKey + 4
  if offset ≥ bytes.size then throw "Missing Terraria temporary slots."
  return offset

/-! ## Equipment records

`Player.Serialize` (IL_0325..03c4, IL_042c..048c): `armor[20]` and `dye[10]` are `type` (int32),
`prefix` (byte), `favorited` (bool), six bytes a slot; `miscEquips[i]` and `miscDyes[i]` alternate,
`type` and `prefix`, five bytes each. `EquipmentLoadout.Serialize` writes `Item.Serialize(w, true)`
for `Armor[20]` and `Dye[10]`, ten bytes a slot with the stack, then ten `Hide` booleans. -/

private def armorSlotSize : Nat := 6
private def miscSlotSize : Nat := 5
private def loadoutSize : Nat := (20 + 10) * slotSize + 10

private def readEquipSlot (bytes : ByteArray) (offset : Nat) (favorite : Bool) (label : String) :
    Except String Slot := do
  if offset + (if favorite then armorSlotSize else miscSlotSize) > bytes.size then
    throw s!"Truncated Terraria {label} record."
  let item := getU32 bytes offset
  if item > 65535 then throw s!"Unsupported Terraria item type in {label}."
  return { item := item.toUInt16, stack := if item == 0 then 0 else 1, prefixId := bytes[offset + 4]!
           favorited := favorite && bytes[offset + 5]! != 0 }

private def writeEquipSlot (bytes : ByteArray) (offset : Nat) (slot : Slot) (favorite : Bool) : ByteArray :=
  let bytes := (putU32 bytes offset slot.item.toUInt32).set! (offset + 4) slot.prefixId
  if favorite then bytes.set! (offset + 5) (if slot.favorited then 1 else 0) else bytes

private def readLoadoutSlot (bytes : ByteArray) (offset : Nat) : Except String Slot := do
  if offset + slotSize > bytes.size then throw "Truncated Terraria loadout record."
  let item := getU32 bytes offset
  let stack := getU32 bytes (offset + 4)
  if item > 65535 || stack > 2147483647 then throw "Unsupported Terraria item type or stack in a loadout."
  return { item := item.toUInt16, stack := stack.toNat, prefixId := bytes[offset + 8]!
           favorited := bytes[offset + 9]! != 0 }

private def writeLoadoutSlot (bytes : ByteArray) (offset : Nat) (slot : Slot) : ByteArray :=
  let bytes := putU32 (putU32 bytes offset slot.item.toUInt32) (offset + 4) slot.stack.toUInt32
  (bytes.set! (offset + 8) slot.prefixId).set! (offset + 9) (if slot.favorited then 1 else 0)

/-- `IPersistentPerPlayerContent.Save` payload of the three per-player Journey powers, by the id
`CreativePowerManager.SaveToPlayer` writes: two toggles (one boolean) and one slider (one float). -/
private def creativePowerPayload : UInt32 → Option Nat
  | 5 | 11 => some 1
  | 14 => some 4
  | _ => none

/-- From the temporary-slot flags to `CurrentLoadoutIndex`: `SaveTemporaryItemSlotContents`
(a flag byte, then a nine-byte item for each of its four bits), `CreativePowerManager.SaveToPlayer`
(`true`, id, payload … `false`) and the minecart-upgrade flags. -/
private def findLoadouts (bytes : ByteArray) (temporaryOffset : Nat) : Except String Nat := do
  let some flags := bytes[temporaryOffset]? | throw "Missing Terraria temporary slots."
  let mut offset := temporaryOffset + 1
  for bit in [:4] do
    if flags &&& (1 <<< bit.toUInt8) != 0 then offset := offset + (slotSize - 1)
  for _ in [:64] do
    let some more := bytes[offset]? | throw "Truncated Terraria Journey power section."
    offset := offset + 1
    if more == 0 then
      -- unlockedSuperCart / enabledSuperCart
      offset := offset + 1
      if offset + 4 + 3 * loadoutSize + 5 > bytes.size then throw "Truncated Terraria loadouts or voice."
      return offset
    if offset + 2 > bytes.size then throw "Truncated Terraria Journey power section."
    let id := bytes[offset]!.toUInt32 ||| (bytes[offset + 1]!.toUInt32 <<< 8)
    let some size := creativePowerPayload id | throw s!"Unknown Terraria Journey power {id} in a character file."
    offset := offset + 2 + size
  throw "Invalid Terraria Journey power section."

private def readEquipment (bytes : ByteArray) (difficultyOffset inventoryOffset loadoutOffset : Nat) :
    Except String Equipment := do
  let armorOffset := inventoryOffset - (20 + 10) * armorSlotSize
  let miscOffset := inventoryOffset + storedSlots * slotSize
  let mut armor := #[]
  for i in [:20] do armor := armor.push (← readEquipSlot bytes (armorOffset + i * armorSlotSize) true "armor")
  let mut dye := #[]
  for i in [:10] do dye := dye.push (← readEquipSlot bytes (armorOffset + (20 + i) * armorSlotSize) true "dye")
  let mut miscEquips := #[]
  let mut miscDyes := #[]
  for i in [:5] do
    miscEquips := miscEquips.push (← readEquipSlot bytes (miscOffset + 2 * i * miscSlotSize) false "misc equipment")
    miscDyes := miscDyes.push (← readEquipSlot bytes (miscOffset + (2 * i + 1) * miscSlotSize) false "misc dye")
  let hidden := bytes[difficultyOffset + 15]!.toNat + 256 * bytes[difficultyOffset + 16]!.toNat
  let currentLoadout := getU32 bytes loadoutOffset
  if currentLoadout > 2 then throw "Invalid Terraria loadout index."
  let mut loadouts := #[]
  for index in [:3] do
    let start := loadoutOffset + 4 + index * loadoutSize
    let mut loadoutArmor := #[]
    for i in [:20] do loadoutArmor := loadoutArmor.push (← readLoadoutSlot bytes (start + i * slotSize))
    let mut loadoutDye := #[]
    for i in [:10] do loadoutDye := loadoutDye.push (← readLoadoutSlot bytes (start + (20 + i) * slotSize))
    let hide := (Array.range 10).map fun i => bytes[start + 30 * slotSize + i]! != 0
    loadouts := loadouts.push { armor := loadoutArmor, dye := loadoutDye, hide }
  return {
    armor, dye, miscEquips, miscDyes
    hideVisibleAccessory := (Array.range 10).map fun i => hidden / 2 ^ i % 2 == 1
    hideMisc := bytes[difficultyOffset + 17]!
    extraAccessory := bytes[difficultyOffset + 35]! != 0
    currentLoadout := currentLoadout.toNat, loadouts }

/-- Write the equipment into a plaintext that still has the layout it was read with. Only records
that differ from `original` are touched. The two unused high bits of the second visibility byte
and every byte outside these records keep their values. -/
private def writeEquipment (bytes : ByteArray) (difficultyOffset inventoryOffset loadoutOffset : Nat)
    (original updated : Equipment) : Except String ByteArray := do
  if !updated.wellFormed then throw "Equipment does not have the slot counts of Player.Serialize."
  if original == updated then return bytes
  let armorOffset := inventoryOffset - (20 + 10) * armorSlotSize
  let miscOffset := inventoryOffset + storedSlots * slotSize
  let mut bytes := bytes
  for i in [:20] do
    if original.armor[i]! != updated.armor[i]! then
      bytes := writeEquipSlot bytes (armorOffset + i * armorSlotSize) updated.armor[i]! true
  for i in [:10] do
    if original.dye[i]! != updated.dye[i]! then
      bytes := writeEquipSlot bytes (armorOffset + (20 + i) * armorSlotSize) updated.dye[i]! true
  for i in [:5] do
    if original.miscEquips[i]! != updated.miscEquips[i]! then
      bytes := writeEquipSlot bytes (miscOffset + 2 * i * miscSlotSize) updated.miscEquips[i]! false
    if original.miscDyes[i]! != updated.miscDyes[i]! then
      bytes := writeEquipSlot bytes (miscOffset + (2 * i + 1) * miscSlotSize) updated.miscDyes[i]! false
  if original.hideVisibleAccessory != updated.hideVisibleAccessory then
    let bit := fun (i : Nat) => if updated.hideVisibleAccessory[i]! then (1 : UInt8) <<< (i % 8).toUInt8 else 0
    let low := (List.range 8).foldl (fun byte i => byte ||| bit i) (0 : UInt8)
    let high := (bytes[difficultyOffset + 16]! &&& 0xfc) ||| bit 8 ||| bit 9
    bytes := (bytes.set! (difficultyOffset + 15) low).set! (difficultyOffset + 16) high
  bytes := bytes.set! (difficultyOffset + 17) updated.hideMisc
  bytes := bytes.set! (difficultyOffset + 35) (if updated.extraAccessory then 1 else 0)
  bytes := putU32 bytes loadoutOffset updated.currentLoadout.toUInt32
  for index in [:3] do
    let start := loadoutOffset + 4 + index * loadoutSize
    let before := original.loadouts[index]!
    let after := updated.loadouts[index]!
    if before == after then continue
    for i in [:20] do
      if after.armor[i]!.stack > 2147483647 then throw "Terraria loadout stack is too large."
      bytes := writeLoadoutSlot bytes (start + i * slotSize) after.armor[i]!
    for i in [:10] do
      if after.dye[i]!.stack > 2147483647 then throw "Terraria loadout stack is too large."
      bytes := writeLoadoutSlot bytes (start + (20 + i) * slotSize) after.dye[i]!
    for i in [:10] do
      bytes := bytes.set! (start + 30 * slotSize + i) (if after.hide[i]! then 1 else 0)
  return bytes

/-- `voiceVariant` (byte) and `voicePitchOffset` (single) follow the third loadout
(`Player.Serialize` IL_0866..087a). -/
private def voiceOffset (loadoutOffset : Nat) : Nat := loadoutOffset + 4 + 3 * loadoutSize

private def readVoice (bytes : ByteArray) (loadoutOffset : Nat) : Voice :=
  let offset := voiceOffset loadoutOffset
  { variant := bytes[offset]!, pitchOffset := Float32.ofBits (getU32 bytes (offset + 1)) }

private def writeVoice (bytes : ByteArray) (loadoutOffset : Nat) (original updated : Voice) : ByteArray :=
  if original.variant == updated.variant && original.pitchOffset.toBits == updated.pitchOffset.toBits then bytes
  else
    let offset := voiceOffset loadoutOffset
    putU32 (bytes.set! offset updated.variant) (offset + 1) updated.pitchOffset.toBits

/-- Terraria.Player.Serialize in 1.4.5.8 writes fixed-width appearance and
equipment after the name. The ten-byte inventory records contain item type,
stack, prefix, and favorite flag. Keep the rest of the plaintext byte for byte. -/
def decodePlain (bytes : ByteArray) : Except String Archive := do
  if bytes.size < 24 || bytes.extract 4 11 != "relogic".toUTF8 || bytes[11]! != 3 then
    throw "Expected a Terraria character file."
  let version := getU32 bytes 0
  if version != 326 then
    throw s!"Expected Terraria 1.4.5.8 character format 326, got {version}."
  let (name, difficultyOffset) ← readString bytes 24
  -- Before the colors: difficulty, play time, hair, dyes and visibility,
  -- skin variant, life and mana, upgrades, tax, and death counters.
  let colorOffset := difficultyOffset + 59
  -- After seven RGB colors: 20 armor slots and 10 dye slots, six bytes each.
  let inventoryOffset := colorOffset + 7 * 3 + (20 + 10) * 6
  if bytes.size ≤ inventoryOffset + storedSlots * slotSize then
    throw "Truncated Terraria character inventory or trailing fields."
  if bytes[difficultyOffset]! != 0 then
    throw "The original world preview supports Classic characters."
  let appearance := {
    hair := getU32 bytes (difficultyOffset + 9)
    skinVariant := bytes[difficultyOffset + 18]!
    hairColor := getColor bytes colorOffset
    skinColor := getColor bytes (colorOffset + 3)
    eyeColor := getColor bytes (colorOffset + 6)
    shirtColor := getColor bytes (colorOffset + 9)
    underShirtColor := getColor bytes (colorOffset + 12)
    pantsColor := getColor bytes (colorOffset + 15)
    shoeColor := getColor bytes (colorOffset + 18)
  }
  let inventory ← readSlots bytes inventoryOffset storedSlots true "Inventory"
  let mut banks : Stores := {}
  for kind in OriginalBanks.kinds do
    let contents ← readSlots bytes (bankStart inventoryOffset kind) OriginalBanks.slotCount
      kind.keepsFavorites kind.name
    banks := banks.replace kind contents
  let infoOffset := voidInfoOffset inventoryOffset
  let some voidVaultInfo := bytes[infoOffset]? | throw "Missing Terraria Void Vault flags."
  banks := { banks with voidVaultInfo }
  let deathOffset ← findDeathOffset bytes (infoOffset + 1)
  let temporaryOffset ← findTemporarySlots bytes deathOffset
  let dead := bytes[deathOffset]! != 0
  let maxLife := min 500 (getI32 bytes (difficultyOffset + 23))
  let buffs := Array.range OriginalPlayerLiquidHealth.maxBuffs
  let health : OriginalPlayerLiquidHealth.Health := {
    statLife := getI32 bytes (difficultyOffset + 19)
    statLifeMax := maxLife
    statLifeMax2 := maxLife
    statMana := min 400 (getI32 bytes (difficultyOffset + 27))
    statManaMax := min 200 (getI32 bytes (difficultyOffset + 31))
    usedArcaneCrystal := bytes[difficultyOffset + 42]! != 0
    deathsPVE := getI32 bytes (difficultyOffset + 51)
    dead
    respawnTimer := if dead then min 60000 (max 0 (getI32 bytes (deathOffset + 1))) else 0
    buffType := buffs.map fun i => (getI32 bytes (infoOffset + 1 + i * 8)).toNat
    buffTime := buffs.map fun i => getI32 bytes (infoOffset + 5 + i * 8) }
  let savedAt := getI64 bytes (deathOffset + (if dead then 5 else 1))
  let mouseItem ← if bytes[temporaryOffset]! &&& 1 == 0 then pure none
    else parseStack bytes (temporaryOffset + 1) false
  let loadoutOffset ← findLoadouts bytes temporaryOffset
  let equipment ← readEquipment bytes difficultyOffset inventoryOffset loadoutOffset
  let voice := readVoice bytes loadoutOffset
  let unlockedBiomeTorches := bytes[difficultyOffset + 36]! != 0
  let ateArtisanBread := bytes[difficultyOffset + 38]! != 0
  return ⟨name, appearance, inventory.extract 0 mainSlots, inventory.extract mainSlots storedSlots,
    banks, mouseItem, unlockedBiomeTorches, ateArtisanBread, bytes[difficultyOffset + 43]! != 0, health, equipment, voice,
    savedAt, bytes, inventoryOffset,
    deathOffset, temporaryOffset, loadoutOffset⟩

/-- `Player.AdjustRespawnTimerForWorldJoining`, then the life fields of `Player.Spawn`. -/
def Archive.enterWorld (archive : Archive) (now : Int) : OriginalPlayerLiquidHealth.Health :=
  let health := archive.health
  let health := if health.dead && archive.savedAt != 0 then
    let elapsed := min 60000 (max 0 (now - archive.savedAt) * 60 / 10000000)
    let respawnTimer := max 0 (health.respawnTimer - elapsed)
    { health with respawnTimer, dead := respawnTimer > 0 }
  else health
  if health.dead then health else health.spawn true

private def writeItem (bytes : ByteArray) (offset : Nat) (item : Option Stack)
    (hasFavorite : Bool) : Except String ByteArray := do
  let stack := item.getD (.ofItem 0 0)
  if stack.count > 2147483647 then throw "Terraria item stack is too large."
  let bytes := putU32 bytes offset stack.item.toUInt32
  let bytes := putU32 bytes (offset + 4) stack.count.toUInt32
  let bytes := bytes.set! (offset + 8) stack.prefixId
  return if hasFavorite then bytes.set! (offset + 9) (if stack.favorite then 1 else 0)
    else bytes

private def writeChangedSlots (initial : ByteArray) (offset : Nat) (original updated : Inventory)
    (hasFavorite : Bool) : Except String ByteArray := do
  let mut bytes := initial
  let stride := slotStride hasFavorite
  for index in [:original.size] do
    let edited := updated[index]!.filter (·.count > 0)
    if original[index]! == edited then continue
    bytes ← writeItem bytes (offset + index * stride) edited hasFavorite
  return bytes

/-- The `Main.buffNoSave` table captured from Terraria 1.4.5.8. -/
private def unsavedBuffs : Array Nat := #[
  20, 22, 23, 24, 28, 29, 30, 31, 34, 35, 37, 38, 39, 43, 44, 46, 47,
  48, 58, 59, 60, 62, 63, 64, 67, 68, 69, 70, 72, 80, 87, 88, 89, 90,
  93, 94, 95, 96, 97, 98, 99, 100, 103, 118, 119, 120, 125, 126, 128, 129, 130,
  131, 132, 133, 134, 135, 137, 139, 140, 141, 142, 143, 144, 146, 147, 149, 150, 156,
  158, 159, 161, 162, 163, 164, 166, 168, 170, 171, 172, 173, 174, 175, 176, 177, 178,
  179, 180, 181, 182, 184, 187, 188, 193, 194, 195, 196, 197, 198, 199, 205, 208, 210,
  212, 213, 214, 215, 220, 222, 224, 226, 228, 230, 231, 233, 235, 237, 239, 241, 243,
  245, 247, 249, 251, 253, 255, 263, 265, 269, 271, 272, 275, 276, 277, 278, 279, 280,
  281, 282, 283, 305, 318, 320, 321, 322, 325, 335, 338, 342, 346, 348, 353, 355, 366,
  370, 374, 377, 378, 379, 380, 381, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393,
  394, 395, 397, 398, 399, 400]

private def Archive.encodeHealth (archive : Archive) (bytes : ByteArray)
    (health : OriginalPlayerLiquidHealth.Health) (savedAt : Int) : ByteArray := Id.run do
  let difficultyOffset := archive.inventoryOffset - (59 + 7 * 3 + 30 * 6)
  let mut bytes := putU32 bytes (difficultyOffset + 19) (UInt32.ofInt health.statLife)
  bytes := putU32 bytes (difficultyOffset + 23) (UInt32.ofInt health.statLifeMax)
  bytes := putU32 bytes (difficultyOffset + 27) (UInt32.ofInt health.statMana)
  bytes := putU32 bytes (difficultyOffset + 31) (UInt32.ofInt health.statManaMax)
  bytes := putU32 bytes (difficultyOffset + 51) (UInt32.ofInt health.deathsPVE)
  let offset := voidInfoOffset archive.inventoryOffset + 1
  for i in [:OriginalPlayerLiquidHealth.maxBuffs] do
    let buff := health.buffType[i]?.getD 0
    let saved := !unsavedBuffs.contains buff
    bytes := putU32 bytes (offset + i * 8) (if saved then buff.toUInt32 else 0)
    bytes := putU32 bytes (offset + i * 8 + 4)
      (if saved then UInt32.ofInt (health.buffTime[i]?.getD 0) else 0)
  let marker := archive.deathOffset
  let timestamp := UInt64.ofInt savedAt
  let suffixStart := marker + (if archive.health.dead then 5 else 1) + 8
  let header := (bytes.extract 0 marker).push (if health.dead then 1 else 0)
  let mut record := if health.dead then putU32 ⟨Array.replicate 12 0⟩ 0 (UInt32.ofInt health.respawnTimer)
    else ⟨Array.replicate 8 0⟩
  let timestampOffset := if health.dead then 4 else 0
  record := putU32 record timestampOffset timestamp.toUInt32
  record := putU32 record (timestampOffset + 4) (timestamp >>> 32).toUInt32
  return header ++ record ++ bytes.extract suffixStart bytes.size

/-- Encode the edited inventory and optional life state into a copy of the original record. -/
def Archive.encodePlain (archive : Archive) (inventory : Inventory)
    (mouseItem : Option Stack := archive.mouseItem) (coinAndAmmo : Inventory := archive.coinAndAmmo)
    (banks : Stores := archive.banks) (health : Option OriginalPlayerLiquidHealth.Health := none)
    (savedAt : Int := archive.savedAt) (equipment : Equipment := archive.equipment)
    (voice : Voice := archive.voice) :
    Except String ByteArray := do
  if inventory.size != mainSlots then
    throw s!"Expected {mainSlots} main inventory slots."
  if coinAndAmmo.size != storedSlots - mainSlots then throw "Expected 8 coin and ammo slots."
  let original := archive.inventory ++ archive.coinAndAmmo
  let updated := inventory ++ coinAndAmmo
  let mut bytes ← writeChangedSlots archive.plaintext archive.inventoryOffset original updated true
  for kind in OriginalBanks.kinds do
    let updated := banks.items kind
    if updated.size != OriginalBanks.slotCount then throw s!"Expected 40 slots in {kind.name}."
    bytes ← writeChangedSlots bytes (bankStart archive.inventoryOffset kind)
      (archive.banks.items kind) updated kind.keepsFavorites
  bytes := bytes.set! (voidInfoOffset archive.inventoryOffset) banks.voidVaultInfo
  -- Equipment records keep their sizes, so they are written while every offset is still the one read.
  bytes ← writeEquipment bytes (archive.inventoryOffset - (59 + 7 * 3 + 30 * 6)) archive.inventoryOffset
    archive.loadoutOffset archive.equipment equipment
  bytes := writeVoice bytes archive.loadoutOffset archive.voice voice
  let mouseItem := mouseItem.filter (·.count > 0)
  if mouseItem != archive.mouseItem then
    let flagOffset := archive.temporaryOffset
    let flag := bytes[flagOffset]!
    let suffixStart := flagOffset + 1 + (if flag &&& 1 == 0 then 0 else 9)
    let headerBytes := (bytes.extract 0 flagOffset).push <|
      if mouseItem.isSome then flag ||| 1 else flag &&& 0xfe
    let headerBytes ← match mouseItem with
      | none => pure headerBytes
      | some stack =>
        writeItem (headerBytes ++ ⟨Array.replicate 9 0⟩) (flagOffset + 1) (some stack) false
    bytes := headerBytes ++ bytes.extract suffixStart bytes.size
  return (health.map (archive.encodeHealth bytes · savedAt)).getD bytes

/-- Vanilla stores its fixed 128-bit AES key and IV as UTF-16LE in Player. -/
private def encryptionKey : ByteArray := Id.run do
  let mut bytes := ByteArray.empty
  for char in "h3y_gUyZ".toUTF8 do
    bytes := (bytes.push char).push 0
  return bytes

@[extern "terria_player_crypt"]
private opaque crypt (encrypt : Bool) (key input : @& ByteArray) : IO ByteArray

def read (path : System.FilePath) : IO Archive := do
  let plaintext ← crypt false encryptionKey (← IO.FS.readBinFile path)
  IO.ofExcept ((decodePlain plaintext).mapError IO.userError)

def Archive.exportCopy (archive : Archive) (inventory : Inventory)
    (destination : System.FilePath) (mouseItem : Option Stack := archive.mouseItem)
    (coinAndAmmo : Inventory := archive.coinAndAmmo) (banks : Stores := archive.banks)
    (health : Option OriginalPlayerLiquidHealth.Health := none)
    (equipment : Equipment := archive.equipment) (voice : Voice := archive.voice) : IO Unit := do
  let savedAt ← if health.isSome then savedTimeNow else pure archive.savedAt
  let plaintext ← IO.ofExcept ((archive.encodePlain inventory mouseItem coinAndAmmo banks health savedAt equipment voice).mapError IO.userError)
  OriginalSaveCopy.write destination (← crypt true encryptionKey plaintext)

end Lean4Terria.OriginalPlayerFile
