import Init

namespace Lean4Terria.OriginalWorld

/-! Read the binary world metadata and terrain emitted by Terraria 1.4.5.8.
Terrain edits can be written to a copy using the original file as a template. -/

def formatVersion : Nat := 326

private def crcSeed (text : String) : Int := Id.run do
  let mut crc : UInt32 := 0xffffffff
  for character in text.toList do
    let point := character.toNat
    let units := if point ≤ 0xffff then [point]
      else [0xd800 + (point - 0x10000) / 1024, 0xdc00 + (point - 0x10000) % 1024]
    for unit in units do
      -- ReLogic feeds only the low byte of each UTF-16 code unit to CRC-32.
      crc := crc ^^^ (unit.toUInt32 &&& 0xff)
      for _ in [:8] do
        crc := (crc >>> 1) ^^^ (if crc &&& 1 == 0 then 0 else 0xedb88320)
  return (Int32.ofUInt32 (~~~crc)).toInt

/-- The native numeric seed used to initialize per-phase random streams. -/
def numericSeed (text : String) : Int :=
  let whitespace (ch : Char) : Bool := ch == ' ' || (ch.toNat ≥ 9 && ch.toNat ≤ 13)
  let digits := text.dropEndWhile (Char.ofNat 0)
  let trimmed := (digits.dropWhile whitespace).dropEndWhile whitespace
  let value : Option Int := if trimmed.contains '_' then none
    else if trimmed.startsWith '+' then ((trimmed.drop 1).toNat?).map Int.ofNat
    else trimmed.toInt?
  match value with
  | some value =>
    if value ≥ -2147483648 && value ≤ 2147483647 then
      if value == -2147483648 then 2147483647 else Int.ofNat value.natAbs
    else crcSeed text
  | none => crcSeed text

/-- Time / phase fields of `WorldFile.SaveWorldFlags` (IL offsets of the writes) that `Main.UpdateTime`'s
callers persist; the header parser used to step over them. -/
structure SavedTimeFlags where
  savedGoblin : Bool := false            -- IL_027c
  savedWizard : Bool := false            -- IL_0287
  downedClown : Bool := false            -- IL_02a8
  downedFrost : Bool := false            -- IL_02b3
  downedPirates : Bool := false          -- IL_02be
  spawnMeteor : Bool := false            -- IL_02d4 WorldGen.spawnMeteor
  altarCount : Int := 0                  -- IL_02eb
  invasionDelay : Int := 0               -- IL_030c
  sundialCooldown : Nat := 0             -- IL_0344
  anglerWhoFinishedToday : Array String := #[]  -- IL_0419..0442
  savedStylist : Bool := false           -- IL_0460
  savedTaxCollector : Bool := false      -- IL_046b
  savedGolfer : Bool := false            -- IL_0476
  cultistDelay : Int := 0                -- IL_048c _tempCultistDelay
  fastForwardTimeToDawn : Bool := false  -- IL_049d
  downedMoonlord : Bool := false         -- IL_04c9
  lunarApocalypseIsUp : Bool := false    -- IL_0563
  partyManual : Bool := false            -- IL_056e
  partyGenuine : Bool := false           -- IL_0579
  partyCooldown : Int := 0               -- IL_0584
  partyCelebratingNPCs : Array Int := #[] -- IL_0594..05bd
  fastForwardTimeToDusk : Bool := false  -- IL_07ca
  moondialCooldown : Nat := 0            -- IL_07d6
  meteorShowerCount : Int := 0           -- IL_080d
  coinRain : Int := 0                    -- IL_0818
  vampireSeed : Bool := false            -- IL_07f7
  /-- IL_06c2..077d, the eighteen bytes after the ore tiers, in write order. -/
  boughtCat : Bool := false
  boughtDog : Bool := false
  boughtBunny : Bool := false
  downedEmpressOfLight : Bool := false
  downedQueenSlime : Bool := false
  downedDeerclops : Bool := false
  unlockedSlimeBlueSpawn : Bool := false
  unlockedMerchantSpawn : Bool := false
  unlockedDemolitionistSpawn : Bool := false
  unlockedPartyGirlSpawn : Bool := false
  unlockedDyeTraderSpawn : Bool := false
  unlockedTruffleSpawn : Bool := false
  unlockedArmsDealerSpawn : Bool := false
  unlockedNurseSpawn : Bool := false
  unlockedPrincessSpawn : Bool := false
  unlockedSlimeGreenSpawn : Bool := false
  /-- IL_0793..07bf: purple, rainbow, red, yellow, copper slime unlocks. -/
  unlockedSlimeOtherSpawns : Array Bool := #[]
  deriving BEq, Repr

/-- `LanternNight` as `WorldFile.SaveWorldFlags` IL_0649..066a writes it. -/
structure LanternNightSave where
  cooldown : Int := 0
  genuine : Bool := false
  manual : Bool := false
  nextNightIsGenuine : Bool := false
  deriving BEq, Repr

structure Header where
  name : String
  seed : String
  generatorVersion : UInt64
  id : UInt32
  width : Nat
  height : Nat
  gameMode : UInt32
  spawnX : Nat
  spawnY : Nat
  surface : Float
  rockLayer : Float
  sectionOffsets : Array Nat
  downedPlantBoss : Bool := false
  forTheWorthy : Bool := false
  drunkWorld : Bool := false
  tenthAnniversary : Bool := false
  dontStarveWorld : Bool := false
  zenithWorld : Bool := false
  remixWorld : Bool := false
  skyblock : Bool := false
  /-- `LoadWorldFlags`: lantern night and the season forces (`Main.checkHalloween` / `checkXMas`). -/
  lanternNight : LanternNightSave := {}
  forceHalloweenForToday : Bool := false
  forceXMasForToday : Bool := false
  forceHalloweenForever : Bool := false
  forceXMasForever : Bool := false
  timeFlags : SavedTimeFlags := {}
  /-- `NPC.downedMechBoss1..3`, `downedMechBossAny`. -/
  downedMech : Array Bool := #[false, false, false, false]
  downedSkeletron : Bool := false
  unlockedSlimeOldSpawn : Bool := false
  downedEye : Bool := false
  downedEvilBoss : Bool := false
  downedQueenBee : Bool := false
  downedGolem : Bool := false
  downedSlimeKing : Bool := false
  shadowOrbSmashed : Bool := false
  crimson : Bool := false
  dungeon : Int × Int := (0, 0)
  time : Float := 13500
  dayTime : Bool := true
  moonPhase : Nat := 0
  moonType : Nat := 0
  bloodMoon : Bool := false
  eclipse : Bool := false
  hardMode : Bool := false
  downedGoblins : Bool := false
  raining : Bool := false
  slimeRainTime : Float := 0
  invasionType : Int := 0
  cloudBackgroundActive : Int := 0
  cloudCount : Int := 0
  windSpeedTarget : Float32 := 0
  savedAngler : Bool := false
  savedMechanic : Bool := false
  bannerKills : Array Nat := #[]
  claimableBanners : Array Nat := #[]
  anglerQuest : Nat := 0
  treeX : Array Int := #[]
  treeBackgrounds : Array Nat := #[]
  infectedSeed : Bool := false
  deriving BEq, Repr

inductive TileShape where
  | full | halfBlock | slope (id : UInt8)
  deriving BEq, Inhabited, Repr

inductive LiquidKind where
  | water | lava | honey | shimmer
  deriving BEq, Inhabited, Repr

structure Liquid where
  kind : LiquidKind
  amount : UInt8
  deriving BEq, Repr

structure Tile where
  id : Option UInt16 := none
  frame : Option (Int × Int) := none
  wall : UInt16 := 0
  liquid : Option Liquid := none
  shape : TileShape := .full
  tilePaint : UInt8 := 0
  wallPaint : UInt8 := 0
  /-- Red, blue, green, and yellow, from low to high bit. -/
  wires : UInt8 := 0
  actuator : Bool := false
  actuated : Bool := false
  invisibleBlock : Bool := false
  invisibleWall : Bool := false
  fullbrightBlock : Bool := false
  fullbrightWall : Bool := false
  deriving BEq, Inhabited, Repr

structure TileRun where
  endY : Nat
  tile : Tile
  deriving BEq, Inhabited

structure Terrain where
  header : Header
  columns : Array (Array TileRun)
  private editedCells : Option (Array (Nat × Nat)) := none

structure Archive where
  terrain : Terrain
  private source : ByteArray
  private tileCount : Nat
  private frameMask : ByteArray
  private columnOffsets : Array Nat
  private clockOffset : Nat
  private weatherOffset : Nat
  private anglerQuestOffset : Nat
  private savedMechanicOffset : Nat
  private bannerKillsOffset : Nat
  private claimableBannersOffset : Nat

structure ChestItem where
  typeId : Int
  count : Nat
  prefixId : UInt8
  favorite : Bool := false
  color : Option UInt32 := none
  deriving BEq, Repr

structure Chest where
  x : Int
  y : Int
  name : String
  items : Array (Option ChestItem)
  deriving BEq, Repr

def Chest.isVacant (chest : Chest) : Bool := chest.x < 0 || chest.y < 0

structure Sign where
  text : String
  x : Int
  y : Int
  deriving BEq, Repr

private def runIndex (runs : Array TileRun) (y : Nat) : Nat := Id.run do
  let mut low := 0
  let mut high := runs.size
  while low < high do
    let middle := low + (high - low) / 2
    if y < runs[middle]!.endY then
      high := middle
    else
      low := middle + 1
  return low

/-- The run's end is exclusive. Find the first run whose end lies beyond `y`. -/
def Terrain.tile? (terrain : Terrain) (x y : Nat) : Option Tile := do
  guard (y < terrain.header.height)
  let runs ← terrain.columns[x]?
  return (← runs[runIndex runs y]?).tile

/-- Collect the cells written by an edit in the order they were written. -/
def Terrain.trackEdits (terrain : Terrain) : Terrain :=
  { terrain with editedCells := some #[] }

def Terrain.finishEdits (terrain : Terrain) : Terrain × Array (Nat × Nat) :=
  ({ terrain with editedCells := none }, terrain.editedCells.getD #[])

private def appendTileRun (runs : Array TileRun) (run : TileRun) : Array TileRun :=
  if runs.back?.any (·.tile == run.tile) then runs.set! (runs.size - 1) run else runs.push run

/-- Replace one cell, splitting its vertical run and merging equal neighbors
at the edit. Coordinates and all other cells keep their original values. -/
def Terrain.replaceTile (terrain : Terrain) (x y : Nat) (tile : Tile) : Option Terrain := do
  guard (y < terrain.header.height)
  let runs ← terrain.columns[x]?
  let i := runIndex runs y
  let old ← runs[i]?
  if old.tile == tile then return terrain
  let before := runs.extract 0 i
  let startY := before.back?.map (·.endY) |>.getD 0
  let withLeft := if startY < y then before.push { old with endY := y } else before
  let withEdit := appendTileRun withLeft ⟨y + 1, tile⟩
  let withRight := if y + 1 < old.endY then appendTileRun withEdit old else withEdit
  let updated := match runs[i + 1]? with
    | some next => appendTileRun withRight next ++ runs.extract (i + 2) runs.size
    | none => withRight
  return { terrain with columns := terrain.columns.set! x updated
                        editedCells := terrain.editedCells.map (·.push (x, y)) }

private structure Cursor where
  bytes : ByteArray
  offset : Nat
  limit : Nat

private abbrev Parser := StateT Cursor (Except String)

private def takeBytes (count : Nat) : Parser (ByteArray × Nat) := do
  let cursor ← get
  let next := cursor.offset + count
  if next > cursor.limit then
    throw s!"Truncated Terraria world section at byte {cursor.offset}"
  set { cursor with offset := next }
  return (cursor.bytes, cursor.offset)

private def skipBytes (count : Nat) : Parser Unit := do
  let _ ← takeBytes count

private def readByte : Parser UInt8 := do
  let (bytes, offset) ← takeBytes 1
  return bytes[offset]!

private def readLE (count : Nat) : Parser UInt64 := do
  let (bytes, offset) ← takeBytes count
  let mut value := 0
  for i in [:count] do
    value := value ||| (bytes[offset + i]!.toUInt64 <<< (8 * i).toUInt64)
  return value

private def readU16 : Parser UInt16 := return (← readLE 2).toUInt16

private def readI16 : Parser Int := do
  let value := (← readU16).toNat
  return if value < 32768 then Int.ofNat value else Int.ofNat value - 65536

private def readU32 : Parser UInt32 := return (← readLE 4).toUInt32

private def readI32 : Parser Int := return (Int32.ofUInt32 (← readU32)).toInt

private def readString : Parser String := do
  let mut size := 0
  for i in [:5] do
    let byte := (← readByte).toNat
    size := size + (byte % 128) * (128 ^ i)
    if byte < 128 then
      if size > 2147483647 then throw "Invalid Terraria string length"
      let (bytes, offset) ← takeBytes size
      let some value := String.fromUTF8? (bytes.extract offset (offset + size))
        | throw s!"Invalid UTF-8 at byte {offset}"
      return value
  throw "Invalid Terraria string length"

private def readChestSection : Parser (Array Chest × Array Nat) := do
  let count ← readI16
  if count < 0 || count > 8000 then throw s!"Invalid Terraria chest count {count}"
  let mut chests := Array.emptyWithCapacity count.toNat
  let mut recordOffsets := Array.emptyWithCapacity (count.toNat + 1)
  for _ in [:count.toNat] do
    recordOffsets := recordOffsets.push (← get).offset
    let x ← readI32
    let y ← readI32
    let name ← readString
    let slotCount ← readI32
    let cursor ← get
    if slotCount < 0 || slotCount.toNat > (cursor.limit - cursor.offset) / 2 then
      throw s!"Invalid Terraria chest slot count {slotCount} at ({x}, {y})"
    let mut items := Array.emptyWithCapacity slotCount.toNat
    for _ in [:slotCount.toNat] do
      let stack ← readI16
      if stack == 0 then
        items := items.push none
      else
        let typeId ← readI32
        let prefixId ← readByte
        items := items.push (some { typeId, count := (max 1 stack).toNat, prefixId })
    chests := chests.push ⟨x, y, name, items⟩
  return (chests, recordOffsets.push (← get).offset)

private def Archive.chestRecords (archive : Archive) : Except String (Array Chest × Array Nat) := do
  let offsets := archive.terrain.header.sectionOffsets
  let (records, cursor) ← readChestSection.run ⟨archive.source, offsets[2]!, offsets[3]!⟩
  if cursor.offset != cursor.limit then throw "Terraria chest section has trailing bytes"
  return records

/-- The world file as loaded, for readers of sections the header does not keep
(`OriginalForestClock.readSavedWorldWeather`). -/
def Archive.bytes (archive : Archive) : ByteArray := archive.source

/-- Read the version 326 chest records in file order. Each record stores its
own slot count and each occupied slot stores its stack, item type, and prefix. -/
def Archive.readChests (archive : Archive) : Except String (Array Chest) :=
  archive.chestRecords.map Prod.fst

private def readSignSection : Parser (Array Sign) := do
  let count ← readI16
  if count < 0 || count > 32000 then throw s!"Invalid Terraria sign count {count}"
  let mut signs := Array.emptyWithCapacity count.toNat
  for _ in [:count.toNat] do
    let text ← readString
    let x ← readI32
    let y ← readI32
    signs := signs.push { text, x, y }
  return signs

/-- Read the saved sign records, including the text of placed gravestones. -/
def Archive.readSigns (archive : Archive) : Except String (Array Sign) := do
  let offsets := archive.terrain.header.sectionOffsets
  let (signs, cursor) ← readSignSection.run ⟨archive.source, offsets[3]!, offsets[4]!⟩
  if cursor.offset != cursor.limit then throw "Terraria sign section has trailing bytes"
  return signs

private def hasBit (value mask : UInt8) : Bool := value &&& mask != 0

structure SavedTownNpc where
  typeId : Int
  name : String
  position : Float32 × Float32
  homeless : Bool
  homeTile : Int × Int
  variation : Int
  homelessDespawn : Bool
  deriving BEq, Repr

structure SavedNpc where
  typeId : Int
  position : Float32 × Float32
  deriving BEq, Repr

structure SavedNpcs where
  shimmeredTypes : Array Int
  town : Array SavedTownNpc
  persistent : Array SavedNpc
  deriving BEq, Repr

private def readNpcPosition : Parser (Float32 × Float32) := do
  return (Float32.ofBits (← readU32), Float32.ofBits (← readU32))

private structure TownNpcSpan where
  left : Nat
  position : Nat
  right : Nat

private def readNpcSection : Parser (SavedNpcs × Array TownNpcSpan × Nat) := do
  let count ← readI32
  let cursor ← get
  if count < 0 || count.toNat > (cursor.limit - cursor.offset) / 4 then
    throw s!"Invalid Terraria shimmered NPC count {count}"
  let mut shimmeredTypes := Array.emptyWithCapacity count.toNat
  for _ in [:count.toNat] do shimmeredTypes := shimmeredTypes.push (← readI32)
  let mut town := #[]
  let mut spans := #[]
  repeat
    let left := (← get).offset
    if (← readByte) == 0 then break
    let typeId ← readI32
    let name ← readString
    let positionOffset := (← get).offset
    let position ← readNpcPosition
    let homeless := (← readByte) != 0
    let homeTile := (← readI32, ← readI32)
    let flags ← readByte
    let variation ← if hasBit flags 1 then readI32 else pure 0
    let homelessDespawn := (← readByte) != 0
    spans := spans.push ⟨left, positionOffset, (← get).offset⟩
    town := town.push { typeId, name, position, homeless, homeTile, variation, homelessDespawn }
  let townEnd := (← get).offset - 1
  let mut persistent := #[]
  while (← readByte) != 0 do
    let typeId ← readI32
    let position ← readNpcPosition
    persistent := persistent.push { typeId, position }
  return ({ shimmeredTypes, town, persistent }, spans, townEnd)

private def Archive.npcRecords (archive : Archive) : Except String (SavedNpcs × Array TownNpcSpan × Nat) := do
  let offsets := archive.terrain.header.sectionOffsets
  let (records, cursor) ← readNpcSection.run ⟨archive.source, offsets[4]!, offsets[5]!⟩
  if cursor.offset != cursor.limit then throw "Terraria NPC section has trailing bytes"
  return records

/-- Read the version 326 NPC section in the order used by `WorldFile.LoadNPCs`. -/
def Archive.readNpcs (archive : Archive) : Except String SavedNpcs :=
  archive.npcRecords.map Prod.fst

private def storesFrame (mask : ByteArray) (id : UInt16) : Bool :=
  hasBit mask[id.toNat / 8]! (1 <<< (id.toNat % 8).toUInt8)

private def firstSectionPointer : Nat := 26
private def revisionOffset : Nat := 12

private def readSections : Parser (Array Nat × Nat × ByteArray) := do
  let version := (← readU32).toNat
  if version != formatVersion then
    throw s!"Unsupported Terraria world version {version}; expected {formatVersion}"
  let (bytes, magicOffset) ← takeBytes 8
  if bytes.extract magicOffset (magicOffset + 7) != "relogic".toUTF8 || bytes[magicOffset + 7]! != 2 then
    throw "Expected a Re-Logic world file"
  skipBytes 12 -- Revision and file flags.
  let count := (← readU16).toNat
  if count != 11 then throw s!"Invalid Terraria section count {count}"
  let mut offsets := Array.emptyWithCapacity count
  for _ in [:count] do
    let offset := (← readU32).toNat
    if offset ≥ bytes.size || offset ≤ offsets.back?.getD 0 then
      throw s!"Invalid Terraria section offset {offset}"
    offsets := offsets.push offset
  let tileCount := (← readU16).toNat
  let maskSize := (tileCount + 7) / 8
  let (bytes, maskOffset) ← takeBytes maskSize
  if (← get).offset != offsets[0]! then throw "Terraria format header has an incorrect section offset"
  return (offsets, tileCount, bytes.extract maskOffset (maskOffset + maskSize))

private def skipHeaderArray (count : Int) (entrySize : Nat) : Parser Unit := do
  if count < 0 then throw "Invalid Terraria world header array length"
  skipBytes (count.toNat * entrySize)

private def readBannerArray (width : Nat) : Parser (Array Nat × Nat) := do
  let count ← readI16
  if count < 0 then throw "Invalid Terraria banner array length"
  let offset := (← get).offset
  let mut values := Array.emptyWithCapacity count.toNat
  for _ in [:count.toNat] do
    values := values.push (← readLE width).toNat
  return (values, offset)

private structure BackgroundFlags where
  treeBackgrounds : Array Nat
  infectedSeed : Bool
  hardMode : Bool
  downedGoblins : Bool
  raining : Bool
  slimeRainTime : Float
  invasionType : Int
  cloudBackgroundActive : Int
  cloudCount : Int
  windSpeedTarget : Float32
  savedAngler : Bool
  savedMechanic : Bool
  unlockedSlimeOldSpawn : Bool
  savedMechanicOffset : Nat
  bannerKills : Array Nat
  bannerKillsOffset : Nat
  claimableBanners : Array Nat
  claimableBannersOffset : Nat
  downedSlimeKing : Bool
  shadowOrbSmashed : Bool
  anglerQuest : Nat
  weatherOffset : Nat
  lanternNight : LanternNightSave := {}
  forceHalloweenForToday : Bool := false
  forceXMasForToday : Bool := false
  forceHalloweenForever : Bool := false
  forceXMasForever : Bool := false
  timeFlags : SavedTimeFlags := {}
  anglerQuestOffset : Nat

/-- Read the remaining flags and background ids, including the variable-length header lists. -/
private def readBackgroundFlags : Parser BackgroundFlags := do
  let downedSlimeKing := (← readByte) != 0
  let savedGoblin := (← readByte) != 0
  let savedWizard := (← readByte) != 0
  let savedMechanicOffset := (← get).offset
  let savedMechanic := (← readByte) != 0
  let downedGoblins := (← readByte) != 0
  let downedClown := (← readByte) != 0
  let downedFrost := (← readByte) != 0
  let downedPirates := (← readByte) != 0
  let shadowOrbSmashed := (← readByte) != 0
  let spawnMeteor := (← readByte) != 0
  skipBytes 1 -- Orb count.
  let altarCount ← readI32
  let hardMode := (← readByte) != 0
  skipBytes 1 -- Party of doom.
  let invasionDelay ← readI32
  skipBytes 4 -- Invasion size.
  let invasionType ← readI32
  skipBytes 8 -- Invasion X.
  let slimeRainTime := Float.ofBits (← readLE 8)
  let sundialCooldown := (← readByte).toNat
  let raining := (← readByte) != 0
  skipBytes (4 + 4 + 12) -- Rain duration and intensity, hardmode ore ids.
  let treeBG1 ← readByte
  skipBytes 7 -- Biome backgrounds.
  let weatherOffset := (← get).offset
  let cloudBackgroundActive ← readI32
  let cloudCount ← readI16
  let windSpeedTarget := Float32.ofBits (← readU32)
  let anglerCount ← readI32
  if anglerCount < 0 then throw "Invalid Terraria angler name count"
  let mut anglerWhoFinishedToday : Array String := #[]
  for _ in [:anglerCount.toNat] do anglerWhoFinishedToday := anglerWhoFinishedToday.push (← readString)
  let savedAngler := (← readByte) != 0
  let anglerQuestOffset := (← get).offset
  let anglerQuest ← readU32
  let savedStylist := (← readByte) != 0
  let savedTaxCollector := (← readByte) != 0
  let savedGolfer := (← readByte) != 0
  skipBytes 4 -- Invasion size at start.
  let cultistDelay ← readI32
  let (bannerKills, bannerKillsOffset) ← readBannerArray 4
  let (claimableBanners, claimableBannersOffset) ← readBannerArray 2
  -- IL_049d..0579: fastForwardTimeToDawn, thirteen downed flags (Moon Lord fourth), four tower flags,
  -- LunarApocalypseIsUp, then party manual and genuine
  let fastForwardTimeToDawn := (← readByte) != 0
  skipBytes 3 -- Duke Fishron, Martians, Lunatic Cultist.
  let downedMoonlord := (← readByte) != 0
  skipBytes 13 -- Pumpkin / Frost Moon bosses, pillars, active pillars.
  let lunarApocalypseIsUp := (← readByte) != 0
  let partyManual := (← readByte) != 0
  let partyGenuine := (← readByte) != 0
  let partyCooldown ← readI32
  let celebrating ← readI32
  if celebrating < 0 then throw "Invalid Terraria party NPC count"
  let mut partyCelebratingNPCs : Array Int := #[]
  for _ in [:celebrating.toNat] do partyCelebratingNPCs := partyCelebratingNPCs.push (← readI32)
  skipBytes (1 + 4 + 4 + 4 + 4) -- Sandstorm and Old One's Army.
  skipBytes 2 -- Mushroom and underworld backgrounds.
  let treeBG2 ← readByte
  let treeBG3 ← readByte
  let treeBG4 ← readByte
  skipBytes 1 -- Combat book.
  -- Lantern night (SaveWorldFlags IL_0649..066a): cooldown, genuine, manual, next night genuine.
  let lanternNight : LanternNightSave := {
    cooldown := (← readI32), genuine := (← readByte) != 0, manual := (← readByte) != 0
    nextNightIsGenuine := (← readByte) != 0 }
  skipHeaderArray (← readI32) 4 -- Treetop variants.
  -- IL_0680 / IL_068b: Main.forceHalloweenForToday, Main.forceXMasForToday.
  let forceHalloweenForToday := (← readByte) != 0
  let forceXMasForToday := (← readByte) != 0
  skipBytes 16 -- Four pre-hardmode ore ids.
  let mut eighteen : Array Bool := #[]
  for _ in [:18] do eighteen := eighteen.push ((← readByte) != 0)
  let unlockedSlimeOldSpawn := (← readByte) != 0
  let mut slimeUnlocks : Array Bool := #[]
  for _ in [:5] do slimeUnlocks := slimeUnlocks.push ((← readByte) != 0)
  let fastForwardTimeToDusk := (← readByte) != 0
  let moondialCooldown := (← readByte).toNat
  -- IL_07e1 / IL_07ec: Main.forceHalloweenForever, Main.forceXMasForever.
  let forceHalloweenForever := (← readByte) != 0
  let forceXMasForever := (← readByte) != 0
  let vampireSeed := (← readByte) != 0
  let infectedSeed := (← readByte) != 0
  let meteorShowerCount ← readI32
  let coinRain ← readI32
  let timeFlags : SavedTimeFlags :=
    { savedGoblin := savedGoblin,
      savedWizard := savedWizard,
      downedClown := downedClown,
      downedFrost := downedFrost,
      downedPirates := downedPirates,
      spawnMeteor := spawnMeteor,
      altarCount := altarCount,
      invasionDelay := invasionDelay,
      sundialCooldown := sundialCooldown,
      anglerWhoFinishedToday := anglerWhoFinishedToday,
      savedStylist := savedStylist,
      savedTaxCollector := savedTaxCollector,
      savedGolfer := savedGolfer,
      cultistDelay := cultistDelay,
      fastForwardTimeToDawn := fastForwardTimeToDawn,
      downedMoonlord := downedMoonlord,
      lunarApocalypseIsUp := lunarApocalypseIsUp,
      partyManual := partyManual,
      partyGenuine := partyGenuine,
      partyCooldown := partyCooldown,
      partyCelebratingNPCs := partyCelebratingNPCs,
      fastForwardTimeToDusk := fastForwardTimeToDusk,
      moondialCooldown := moondialCooldown,
      meteorShowerCount := meteorShowerCount,
      coinRain := coinRain,
      vampireSeed := vampireSeed,
      boughtCat := eighteen[0]!, boughtDog := eighteen[1]!, boughtBunny := eighteen[2]!,
      downedEmpressOfLight := eighteen[3]!, downedQueenSlime := eighteen[4]!, downedDeerclops := eighteen[5]!,
      unlockedSlimeBlueSpawn := eighteen[6]!, unlockedMerchantSpawn := eighteen[7]!,
      unlockedDemolitionistSpawn := eighteen[8]!, unlockedPartyGirlSpawn := eighteen[9]!,
      unlockedDyeTraderSpawn := eighteen[10]!, unlockedTruffleSpawn := eighteen[11]!,
      unlockedArmsDealerSpawn := eighteen[12]!, unlockedNurseSpawn := eighteen[13]!,
      unlockedPrincessSpawn := eighteen[14]!, unlockedSlimeGreenSpawn := eighteen[17]!,
      unlockedSlimeOtherSpawns := slimeUnlocks }
  return { treeBackgrounds := #[treeBG1.toNat, treeBG2.toNat, treeBG3.toNat, treeBG4.toNat],
           infectedSeed, hardMode, downedGoblins, raining, slimeRainTime, invasionType, unlockedSlimeOldSpawn,
           cloudBackgroundActive, cloudCount, windSpeedTarget, savedAngler, savedMechanic, savedMechanicOffset, weatherOffset,
           bannerKills, bannerKillsOffset, claimableBanners, claimableBannersOffset,
           downedSlimeKing, shadowOrbSmashed, anglerQuest := anglerQuest.toNat, anglerQuestOffset,
           lanternNight, forceHalloweenForToday, forceXMasForToday, forceHalloweenForever, forceXMasForever,
           timeFlags }

private def readHeaderSection (offsets : Array Nat) : Parser (Header × Nat × BackgroundFlags) := do
  let name ← readString
  let seed ← readString
  let generatorVersion ← readLE 8
  skipBytes 16 -- GUID.
  let id ← readU32
  skipBytes 16 -- Left, right, top, and bottom world bounds.
  let height := (← readU32).toNat
  let width := (← readU32).toNat
  if width == 0 || width > 8400 || height == 0 || height > 2400 then
    throw s!"Unsupported Terraria world dimensions {width}×{height}"
  let gameMode ← readU32
  let drunkWorld := (← readByte) != 0
  let forTheWorthy := (← readByte) != 0
  let tenthAnniversary := (← readByte) != 0
  let dontStarveWorld := (← readByte) != 0
  skipBytes 1 -- Not the bees.
  let remixWorld := (← readByte) != 0
  skipBytes 1 -- No traps.
  let zenithWorld := (← readByte) != 0
  let skyblock := (← readByte) != 0
  skipBytes 16 -- Creation and last-played timestamps.
  let moonType := (← readByte).toNat
  let treeX := #[← readI32, ← readI32, ← readI32]
  skipBytes (14 * 4) -- Tree, cave, ice, jungle, and hell backgrounds.
  let spawnX := (← readU32).toNat
  let spawnY := (← readU32).toNat
  if spawnX ≥ width || spawnY ≥ height then throw "Terraria spawn lies outside the world"
  let surface := Float.ofBits (← readLE 8)
  let rockLayer := Float.ofBits (← readLE 8)
  let clockOffset := (← get).offset
  let time := Float.ofBits (← readLE 8)
  let dayTime := (← readByte) != 0
  let moonPhase := (← readU32).toNat
  let bloodMoon := (← readByte) != 0
  let eclipse := (← readByte) != 0
  let dungeon := (← readI32, ← readI32)
  let crimson := (← readByte) != 0
  let downedEye := (← readByte) != 0
  let downedEvilBoss := (← readByte) != 0
  let downedSkeletron := (← readByte) != 0
  let downedQueenBee := (← readByte) != 0
  -- IL_022f..0250: downedMechBoss1, 2, 3, downedMechBossAny
  let mech1 := (← readByte) != 0
  let mech2 := (← readByte) != 0
  let mech3 := (← readByte) != 0
  let mechAny := (← readByte) != 0
  let downedPlantBoss := (← readByte) != 0
  let downedGolem := (← readByte) != 0
  let flags ← readBackgroundFlags
  let header : Header := {
    name, seed, generatorVersion, id, width, height, gameMode, spawnX, spawnY, surface, rockLayer
    sectionOffsets := offsets, downedPlantBoss, forTheWorthy, drunkWorld, tenthAnniversary, dontStarveWorld,
    zenithWorld, remixWorld, skyblock, downedSkeletron, downedEye, downedEvilBoss, downedQueenBee, downedGolem, crimson
    dungeon, time, dayTime, moonPhase, moonType, bloodMoon, eclipse, treeX
    treeBackgrounds := flags.treeBackgrounds, infectedSeed := flags.infectedSeed, hardMode := flags.hardMode
    downedGoblins := flags.downedGoblins, downedSlimeKing := flags.downedSlimeKing
    shadowOrbSmashed := flags.shadowOrbSmashed
    raining := flags.raining, slimeRainTime := flags.slimeRainTime, invasionType := flags.invasionType
    cloudBackgroundActive := flags.cloudBackgroundActive, cloudCount := flags.cloudCount
    windSpeedTarget := flags.windSpeedTarget, savedAngler := flags.savedAngler, savedMechanic := flags.savedMechanic
    unlockedSlimeOldSpawn := flags.unlockedSlimeOldSpawn
    bannerKills := flags.bannerKills, claimableBanners := flags.claimableBanners
    anglerQuest := flags.anglerQuest, lanternNight := flags.lanternNight
    forceHalloweenForToday := flags.forceHalloweenForToday, forceXMasForToday := flags.forceXMasForToday
    forceHalloweenForever := flags.forceHalloweenForever, forceXMasForever := flags.forceXMasForever
    timeFlags := flags.timeFlags, downedMech := #[mech1, mech2, mech3, mechAny] }
  return (header, clockOffset, flags)

private def checkFooter (header : Header) : Parser Unit := do
  if (← readByte) == 0 || (← readString) != header.name || (← readU32) != header.id then
    throw "Terraria world footer does not match its header"

private def readTile (tileCount : Nat) (frameMask : ByteArray) : Parser (Tile × Nat) := do
  let header ← readByte
  let flags ← if hasBit header 1 then readByte else pure 0
  let extended ← if hasBit flags 1 then readByte else pure 0
  let coatings ← if hasBit extended 1 then readByte else pure 0
  let (id, frame, tilePaint) ← if hasBit header 2 then do
    let id ← if hasBit header 32 then readU16 else pure (← readByte).toUInt16
    if id.toNat ≥ tileCount then throw s!"Invalid Terraria tile ID {id}"
    let framed := storesFrame frameMask id
    let frame ← if framed then do
      let x ← readI16
      let y ← readI16
      pure (some (x, y))
    else pure none
    let paint ← if hasBit extended 8 then readByte else pure 0
    pure (some id, frame, paint)
  else pure (none, none, 0)
  let hasWall := hasBit header 4
  let wallLow ← if hasWall then readByte else pure 0
  let wallPaint ← if hasWall && hasBit extended 16 then readByte else pure 0
  let liquid : Option Liquid ← match (header &&& 24) >>> 3 with
    | 0 => pure none
    | kind => do
      let amount ← readByte
      let kind : LiquidKind := if hasBit extended 128 then .shimmer
        else if kind == 1 then .water else if kind == 2 then .lava else .honey
      pure (some { kind, amount })
  let wallHigh ← if hasBit extended 64 then readByte else pure 0
  let wall := wallLow.toUInt16 ||| (wallHigh.toUInt16 <<< 8)
  let shape := match (flags &&& 112) >>> 4 with
    | 0 => .full
    | 1 => .halfBlock
    | code => .slope (code - 1)
  let wire4 : UInt8 := if hasBit extended 32 then 8 else 0
  let tile : Tile := {
    id, frame, wall, liquid, shape, tilePaint, wallPaint
    wires := ((flags &&& 14) >>> 1) ||| wire4
    actuator := hasBit extended 2
    actuated := hasBit extended 4
    invisibleBlock := hasBit coatings 2
    invisibleWall := hasBit coatings 4
    fullbrightBlock := hasBit coatings 8, fullbrightWall := hasBit coatings 16 }
  let repeats ← match header >>> 6 with
    | 0 => pure 0
    | 1 => pure (← readByte).toNat
    | _ => do
      let value ← readI16
      if value < 0 then throw "Negative Terraria tile run length"
      pure value.toNat
  return (tile, repeats + 1)

/-- Keep the source bytes and column boundaries so a later export can copy
unchanged terrain runs and sections directly. -/
def decodeArchive (bytes : ByteArray) : Except String Archive := do
  let ((offsets, tileCount, frameMask), _) ← readSections.run ⟨bytes, 0, bytes.size⟩
  let ((header, clockOffset, flags), _) ←
    (readHeaderSection offsets).run ⟨bytes, offsets[0]!, offsets[1]!⟩
  let _ ← (checkFooter header).run ⟨bytes, offsets[10]!, bytes.size⟩
  let mut cursor : Cursor := ⟨bytes, offsets[1]!, offsets[2]!⟩
  let mut columns := Array.emptyWithCapacity header.width
  let mut columnOffsets := Array.emptyWithCapacity (header.width + 1)
  for x in [:header.width] do
    columnOffsets := columnOffsets.push cursor.offset
    let mut runs := #[]
    let mut y := 0
    while y < header.height do
      let ((tile, length), next) ← (readTile tileCount frameMask).run cursor
      y := y + length
      if y > header.height then throw s!"Tile run crosses the end of Terraria column {x}"
      cursor := next
      runs := runs.push { endY := y, tile }
    columns := columns.push runs
  if cursor.offset != cursor.limit then throw "Terraria tile section has trailing bytes"
  return {
    terrain := { header, columns }
    source := bytes
    tileCount, frameMask, clockOffset, weatherOffset := flags.weatherOffset,
    anglerQuestOffset := flags.anglerQuestOffset, savedMechanicOffset := flags.savedMechanicOffset,
    bannerKillsOffset := flags.bannerKillsOffset, claimableBannersOffset := flags.claimableBannersOffset,
    columnOffsets := columnOffsets.push cursor.offset
  }

/-- Decode terrain without exposing the other world sections. -/
def decode (bytes : ByteArray) : Except String Terrain :=
  (decodeArchive bytes).map (·.terrain)

private def bit (present : Bool) (mask : UInt8) : UInt8 :=
  if present then mask else 0

private def pushU16 (bytes : ByteArray) (value : UInt16) : ByteArray :=
  (bytes.push value.toUInt8).push (value >>> 8).toUInt8

private def pushU32 (bytes : ByteArray) (value : UInt32) : ByteArray :=
  pushU16 (pushU16 bytes value.toUInt16) (value >>> 16).toUInt16

private def pushString (bytes : ByteArray) (value : String) : ByteArray := Id.run do
  let payload := value.toUTF8
  let mut size := payload.size
  let mut bytes := bytes
  while size ≥ 128 do
    bytes := bytes.push (128 + (size % 128).toUInt8)
    size := size / 128
  return (bytes.push size.toUInt8) ++ payload

private def appendSlice (bytes : ByteArray) (source : ByteArray) (start stop : Nat) : ByteArray :=
  source.copySlice start bytes bytes.size (stop - start) false

private def putU32 (bytes : ByteArray) (offset : Nat) (value : UInt32) : ByteArray := Id.run do
  let mut bytes := bytes
  for index in [:4] do
    bytes := bytes.set! (offset + index) (value >>> (8 * index).toUInt32).toUInt8
  return bytes

private def getU32 (bytes : ByteArray) (offset : Nat) : UInt32 := Id.run do
  let mut value := 0
  for index in [:4] do
    value := value ||| (bytes[offset + index]!.toUInt32 <<< (8 * index).toUInt32)
  return value

private def shapeCode (shape : TileShape) : Except String UInt8 :=
  match shape with
  | .full => .ok 0
  | .halfBlock => .ok 1
  | .slope id => if id ≥ 1 && id ≤ 4 then .ok (id + 1)
    else .error s!"Invalid Terraria slope {id}"

/-- Version 326 writes up to four chained flag bytes, followed by the
foreground, wall, liquid, high wall byte, and repeat count. -/
private def Archive.writeRun (archive : Archive) (bytes : ByteArray) (tile : Tile)
    (repeats : Nat) : Except String ByteArray := do
  let shape ← shapeCode tile.shape
  if tile.id.any (fun id => id.toNat ≥ archive.tileCount) then throw "Tile ID is outside the world format mask"
  let storesFrame := tile.id.any (storesFrame archive.frameMask)
  if storesFrame != tile.frame.isSome then throw "Tile frame does not match the world format mask"
  if tile.frame.any (fun (x, y) => x < -32768 || x > 32767 || y < -32768 || y > 32767) then
    throw "Terraria sprite frame is outside the Int16 range"
  if tile.wires > 15 || (tile.id.isNone && (shape != 0 || tile.tilePaint != 0)) ||
      (tile.wall == 0 && tile.wallPaint != 0) || tile.liquid.any (·.amount == 0) then
    throw "Tile contains invalid flags or payload fields"
  let liquid : Option LiquidKind := tile.liquid.map (·.kind)
  let liquidCode : UInt8 := match liquid with
    | none => 0
    | some .lava => 16
    | some .honey => 24
    | _ => 8
  let coatings := bit tile.invisibleBlock 2 ||| bit tile.invisibleWall 4 |||
    bit tile.fullbrightBlock 8 ||| bit tile.fullbrightWall 16
  let extended := bit (coatings != 0) 1 ||| bit tile.actuator 2 ||| bit tile.actuated 4 |||
    bit (tile.tilePaint != 0) 8 ||| bit (tile.wallPaint != 0) 16 |||
    bit (hasBit tile.wires 8) 32 ||| bit (tile.wall > 255) 64 ||| bit (liquid == some .shimmer) 128
  let flags := bit (extended != 0) 1 ||| ((tile.wires &&& 7) <<< 1) ||| (shape <<< 4)
  let wideId := tile.id.any (· > 255)
  let repeatCode : UInt8 := if repeats > 255 then 128 else bit (repeats > 0) 64
  let control := bit (flags != 0) 1 ||| bit tile.id.isSome 2 ||| bit (tile.wall != 0) 4 |||
    liquidCode ||| bit wideId 32 ||| repeatCode
  let mut bytes := bytes.push control
  if flags != 0 then bytes := bytes.push flags
  if extended != 0 then bytes := bytes.push extended
  if coatings != 0 then bytes := bytes.push coatings
  if let some id := tile.id then
    bytes := if wideId then pushU16 bytes id else bytes.push id.toUInt8
    if let some (x, y) := tile.frame then
      bytes := pushU16 (pushU16 bytes (UInt16.ofInt x)) (UInt16.ofInt y)
    if tile.tilePaint != 0 then bytes := bytes.push tile.tilePaint
  if tile.wall != 0 then
    bytes := bytes.push tile.wall.toUInt8
    if tile.wallPaint != 0 then bytes := bytes.push tile.wallPaint
  if let some liquid := tile.liquid then bytes := bytes.push liquid.amount
  if tile.wall > 255 then bytes := bytes.push (tile.wall >>> 8).toUInt8
  return if repeats > 255 then pushU16 bytes repeats.toUInt16
    else if repeats > 0 then bytes.push repeats.toUInt8 else bytes

private def Archive.runOffsets (archive : Archive) (x : Nat) : Except String (Array Nat) := do
  let stop := archive.columnOffsets[x + 1]!
  let mut cursor : Cursor := ⟨archive.source, archive.columnOffsets[x]!, stop⟩
  let mut offsets := #[cursor.offset]
  while cursor.offset < stop do
    let (_, next) ← (readTile archive.tileCount archive.frameMask).run cursor
    cursor := next
    offsets := offsets.push cursor.offset
  return offsets

private def Archive.writeChangedColumn (archive : Archive) (bytes : ByteArray) (x : Nat)
    (runs : Array TileRun) : Except String ByteArray := do
  let old := archive.terrain.columns[x]!
  let offsets ← archive.runOffsets x
  let mut originalRun := 0
  let mut startY := 0
  let mut bytes := bytes
  for run in runs do
    if run.endY ≤ startY || run.endY > archive.terrain.header.height then
      throw s!"Invalid tile run in column {x}"
    while originalRun < old.size && old[originalRun]!.endY ≤ startY do
      originalRun := originalRun + 1
    let originalStart := if originalRun == 0 then 0 else old[originalRun - 1]!.endY
    if originalStart == startY && old[originalRun]? == some run then
      bytes := appendSlice bytes archive.source offsets[originalRun]! offsets[originalRun + 1]!
    else
      let length := run.endY - startY
      -- These types are saved individually by the original writer.
      if length > 1 && run.tile.id.any (#[423, 520, 723, 724].contains ·) then
        let record ← archive.writeRun ByteArray.empty run.tile 0
        for _ in [:length] do bytes := bytes ++ record
      else
        bytes ← archive.writeRun bytes run.tile (length - 1)
    startY := run.endY
  if startY != archive.terrain.header.height then throw s!"Incomplete Terraria column {x}"
  return bytes

private def Chest.hasContainerTiles (chest : Chest) (terrain : Terrain) : Bool :=
  !chest.isVacant && #[0, 1].all fun dx => #[0, 1].all fun dy =>
    (terrain.tile? (chest.x.toNat + dx) (chest.y.toNat + dy)).any fun tile =>
      tile.id.any (#[21, 88, 467, 470, 475].contains ·)

private def writeChest (initial : ByteArray) (chest : Chest) : Except String ByteArray := do
  let position := pushU32 (pushU32 initial (Int32.ofInt chest.x).toUInt32) (Int32.ofInt chest.y).toUInt32
  let mut bytes := pushU32 (pushString position chest.name) chest.items.size.toUInt32
  for slot in chest.items do
    let some item := slot | bytes := pushU16 bytes 0; continue
    -- ItemID.Sets.ItemsThatShouldNotBeInInventory is also used by SaveChests.
    if item.count == 0 || item.typeId == 0 ||
      #[58, 184, 1734, 1735, 1867, 1868, 3453, 3454, 3455, 4143, 5013, 6135, 6136].contains item.typeId then
      bytes := pushU16 bytes 0
      continue
    if item.count > 32767 || item.typeId < -2147483648 || item.typeId > 2147483647 then
      throw s!"Invalid Terraria chest item at ({chest.x}, {chest.y})"
    bytes := (pushU32 (pushU16 bytes item.count.toUInt16) (Int32.ofInt item.typeId).toUInt32).push item.prefixId
  return bytes

private def Archive.writeChestSection (archive : Archive) (initial : ByteArray) (terrain : Terrain)
    (updated : Array Chest) : Except String ByteArray := do
  let (original, recordOffsets) ← archive.chestRecords
  let surviving := updated.zipIdx.filter (fun (chest, _) => chest.hasContainerTiles terrain)
  if surviving.size > 8000 then throw "Too many Terraria chests"
  let mut bytes := pushU16 initial surviving.size.toUInt16
  for (chest, index) in surviving do
    if original[index]? == some chest then
      bytes := appendSlice bytes archive.source recordOffsets[index]! recordOffsets[index + 1]!
    else
      bytes ← writeChest bytes chest
  return bytes

private def writeSignSection (initial : ByteArray) (terrain : Terrain) (signs : Array Sign) :
    Except String ByteArray := do
  if signs.size > 32000 then throw "Too many Terraria signs"
  let mut bytes := pushU16 initial signs.size.toUInt16
  for sign in signs do
    if sign.x < 0 || sign.y < 0 || sign.x.toNat ≥ terrain.header.width ||
        sign.y.toNat ≥ terrain.header.height then
      throw s!"Invalid Terraria sign coordinates ({sign.x}, {sign.y})"
    bytes := pushU32 (pushU32 (pushString bytes sign.text) sign.x.toNat.toUInt32) sign.y.toNat.toUInt32
  return bytes

structure SaveProgress where
  clock : Option (Float × Bool × Nat) := none
  /-- Cloud background timer, Int16 cloud count, and target wind. -/
  weather : Option (Int × Int × Float32) := none
  anglerQuest : Option Nat := none
  residents : Array (Nat × SavedTownNpc) := #[]
  newResidents : Array SavedTownNpc := #[]
  /-- If set, replace or remove the original Old Man record after an encounter. -/
  oldMan : Option (Option SavedTownNpc) := none
  downedSkeletron : Bool := false
  savedMechanic : Bool := false
  /-- Banner ID, kill count, and unclaimed banner count. -/
  banners : Array (Nat × Nat × Nat) := #[]

private def Archive.writeProgress (archive : Archive) (bytes : ByteArray)
    (progress : SaveProgress) : Except String ByteArray := do
  let mut bytes := bytes
  if let some (time, dayTime, moonPhase) := progress.clock then
    let offset := archive.clockOffset
    let bits := time.toBits
    bytes := putU32 bytes offset bits.toUInt32
    bytes := putU32 bytes (offset + 4) (bits >>> 32).toUInt32
    bytes := bytes.set! (offset + 8) (if dayTime then 1 else 0)
    bytes := putU32 bytes (offset + 9) moonPhase.toUInt32
  if let some (background, clouds, wind) := progress.weather then
    let offset := archive.weatherOffset
    bytes := putU32 bytes offset (Int32.ofInt background).toUInt32
    let count := (Int16.ofInt clouds).toUInt16
    bytes := (bytes.set! (offset + 4) count.toUInt8).set! (offset + 5) (count >>> 8).toUInt8
    bytes := putU32 bytes (offset + 6) wind.toBits
  if let some quest := progress.anglerQuest then
    if quest ≥ 41 then throw s!"Invalid Terraria angler quest index {quest}"
    bytes := putU32 bytes archive.anglerQuestOffset quest.toUInt32
  -- SaveWorldHeader places the three Classic boss flags after the clock,
  -- blood moon, eclipse, dungeon coordinates and crimson.
  if progress.downedSkeletron then bytes := bytes.set! (archive.clockOffset + 26) 1
  if progress.savedMechanic then bytes := bytes.set! archive.savedMechanicOffset 1
  for (id, kills, claimable) in progress.banners do
    let header := archive.terrain.header
    if id ≥ header.bannerKills.size || id ≥ header.claimableBanners.size then
      throw s!"Terraria world is missing the banner entry {id}"
    if kills > 4294967295 || claimable > 65535 then
      throw s!"Terraria banner entry {id} exceeds its count limit"
    bytes := putU32 bytes (archive.bannerKillsOffset + 4 * id) kills.toUInt32
    let offset := archive.claimableBannersOffset + 2 * id
    bytes := (bytes.set! offset claimable.toUInt8).set! (offset + 1) (claimable / 256).toUInt8
  return bytes

private def putTownPosition (bytes : ByteArray) (offset : Nat) (resident : SavedTownNpc) : ByteArray :=
  let bytes := putU32 (putU32 bytes offset resident.position.1.toBits)
    (offset + 4) resident.position.2.toBits
  let bytes := bytes.set! (offset + 8) (if resident.homeless then 1 else 0)
  putU32 (putU32 bytes (offset + 9) (Int32.ofInt resident.homeTile.1).toUInt32)
    (offset + 13) (Int32.ofInt resident.homeTile.2).toUInt32

private def appendTownNpc (bytes : ByteArray) (resident : SavedTownNpc) : ByteArray :=
  let bytes := pushString (pushU32 (bytes.push 1) (Int32.ofInt resident.typeId).toUInt32) resident.name
  let bytes := pushU32 (pushU32 bytes resident.position.1.toBits) resident.position.2.toBits
  let bytes := bytes.push (if resident.homeless then 1 else 0)
  let bytes := pushU32 (pushU32 bytes (Int32.ofInt resident.homeTile.1).toUInt32) (Int32.ofInt resident.homeTile.2).toUInt32
  (pushU32 (bytes.push 1) (Int32.ofInt resident.variation).toUInt32).push (if resident.homelessDespawn then 1 else 0)

private def Archive.writeNpcSection (archive : Archive) (bytes : ByteArray)
    (progress : SaveProgress) : Except String ByteArray := do
  let offsets := archive.terrain.header.sectionOffsets
  let (first, last) := (offsets[4]!, offsets[5]!)
  let source := archive.source
  if progress.residents.isEmpty && progress.oldMan.isNone && progress.newResidents.isEmpty then
    return appendSlice bytes source first last
  let (records, spans, townEnd) ← archive.npcRecords
  if progress.oldMan.join.any (fun npc => npc.typeId != 37) then throw "Expected an Old Man replacement"
  let updates := progress.residents.reverse
  for (slot, resident) in progress.residents do
    let some record := records.town[slot]? | throw s!"Invalid saved town NPC slot {slot}"
    if resident.typeId != record.typeId then throw s!"Town NPC type changed in slot {slot}"
  let townStart := (spans[0]?.map (·.left)).getD townEnd
  let mut bytes := appendSlice bytes source first townStart
  let mut replacement := progress.oldMan.join
  for (record, index) in records.town.zipIdx do
    let some span := spans[index]? | throw "Missing saved town NPC byte span"
    if record.typeId == 37 && progress.oldMan.isSome then
      if let some resident := replacement then
        bytes := appendTownNpc bytes resident
        replacement := none
    else
      let position := bytes.size + (span.position - span.left)
      bytes := appendSlice bytes source span.left span.right
      if let some (_, resident) := updates.find? (·.1 == index) then
        bytes := putTownPosition bytes position resident
  if let some resident := replacement then
    bytes := appendTownNpc bytes resident
  for resident in progress.newResidents do
    bytes := appendTownNpc bytes resident
  return appendSlice bytes source townEnd last

/-- Write the terrain and optional gameplay changes. Copy unchanged header fields
and the other original sections verbatim. -/
def Archive.encode (archive : Archive) (terrain : Terrain) (chests : Option (Array Chest) := none)
    (signs : Option (Array Sign) := none) (progress : SaveProgress := {}) : Except String ByteArray := do
  if terrain.header != archive.terrain.header || terrain.columns.size != terrain.header.width then
    throw "The edited terrain does not belong to this Terraria world"
  let offsets := archive.terrain.header.sectionOffsets
  let source := archive.source
  let mut bytes := source.extract 0 offsets[1]!
  for x in [:terrain.columns.size] do
    let runs := terrain.columns[x]!
    if runs == archive.terrain.columns[x]! then
      bytes := appendSlice bytes source archive.columnOffsets[x]! archive.columnOffsets[x + 1]!
    else
      bytes ← archive.writeChangedColumn bytes x runs
  let tilesEnd := bytes.size
  let chestEnd := offsets[3]!
  bytes ← match chests with
    | none => pure (appendSlice bytes source offsets[2]! chestEnd)
    | some updated => archive.writeChestSection bytes terrain updated
  let newChestEnd := bytes.size
  let signEnd := offsets[4]!
  bytes ← match signs with
    | none => pure (appendSlice bytes source chestEnd signEnd)
    | some updated => writeSignSection bytes terrain updated
  let newSignEnd := bytes.size
  bytes ← archive.writeNpcSection bytes progress
  let newNpcEnd := bytes.size
  let npcEnd := offsets[5]!
  if newNpcEnd + (source.size - npcEnd) > 2147483647 then
    throw "The exported Terraria world is too large for its section offsets"
  bytes := putU32 bytes (firstSectionPointer + 4 * 2) tilesEnd.toUInt32
  bytes := putU32 bytes (firstSectionPointer + 4 * 3) newChestEnd.toUInt32
  bytes := putU32 bytes (firstSectionPointer + 4 * 4) newSignEnd.toUInt32
  for index in [5:offsets.size] do
    let offset := newNpcEnd + (offsets[index]! - npcEnd)
    bytes := putU32 bytes (firstSectionPointer + 4 * index) offset.toUInt32
  bytes := putU32 bytes revisionOffset (getU32 source revisionOffset + 1)
  archive.writeProgress (appendSlice bytes source npcEnd source.size) progress

end Lean4Terria.OriginalWorld
