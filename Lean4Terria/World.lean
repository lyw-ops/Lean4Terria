import Init

namespace Lean4Terria

structure TilePos where
  x : Int
  y : Int
  deriving BEq, Hashable, Repr, Inhabited

inductive Tile where
  | air | dirt | stone | copper | crystal | workbench | bedrock | timber | torch | ice | iron
  | chest | doorClosed | doorClosedTop | doorOpen | doorOpenTop | silver | stormAltar | amber | geode
  | platform | homeBeacon | supplyCache | openedCache | sunleaf | sunleafSprout | ruinAltar | ruinBrick | rail
  | lifeCrystal | highlandBerry | deepsteel | mineMarker | cavernAltar | obsidian | magmaMarker
  | cobweb | bed | furnace | ironAnvil | sawmill | loom
  | tinOre | leadOre | tungstenOre | goldOre | platinumOre | treeTrunk | leadAnvil
  | woodChair | woodTable
  | doorClosedMiddle | doorOpenMiddle
  | demonAltar | crimsonAltar | demoniteOre | crimtaneOre | corruptGrass | crimsonGrass
  | ebonstone | crimstone | shadowOrb | crimsonHeart | clayPot
  | vileMushroom | viciousMushroom | sand | placedBottle | glassBlock
  | blueDungeonBrick | greenDungeonBrick | pinkDungeonBrick
  | dungeonKeyChest | dungeonGoldChest | dungeonOpenChest
  | ash | obsidianBrick | shadowChest | shadowOpenChest | hellstone | hellforge
  | demoniteBrick | crimtaneBrick
  | hallowedGrass | pearlstone | pearlsand | ebonsand | crimsand | purpleIce | pinkIce | redIce
  | cobaltOre | palladiumOre | mythrilOre | orichalcumOre | adamantiteOre | titaniumOre
  | mythrilAnvil | orichalcumAnvil | adamantiteForge | titaniumForge
  | crystalShard | gelatinCrystal | fishingCrate | mud | jungleGrass | honeyBlock | crispyHoneyBlock
  | planteraBulb
  | lihzahrdBrick | lihzahrdChest | lihzahrdAltar
  | lihzahrdDoorLocked0 | lihzahrdDoorLocked1 | lihzahrdDoorLocked2
  | lihzahrdDoorClosed0 | lihzahrdDoorClosed1 | lihzahrdDoorClosed2
  | lihzahrdDoorRight | lihzahrdDoorRightLeaf | lihzahrdDoorLeft | lihzahrdDoorLeftLeaf
  | lihzahrdPressurePlate | lihzahrdDartTrap | lihzahrdFlameTrap | lihzahrdSpikyBallTrap | lihzahrdSpearTrap
  | woodenSpikes
  | lihzahrdAltarUpperLeft | lihzahrdAltarUpperMiddle | lihzahrdAltarUpperRight
  | lihzahrdAltarLowerLeft | lihzahrdAltarLowerMiddle | lihzahrdAltarLowerRight
  deriving BEq, Repr, Inhabited

/-- Stable tile identifiers used in the world and save data. -/
def Tile.code : Tile → UInt8
  | .air => 0
  | .dirt => 1
  | .stone => 2
  | .copper => 3
  | .crystal => 4
  | .workbench => 5
  | .bedrock => 6
  | .timber => 7
  | .torch => 8
  | .ice => 9
  | .iron => 10
  | .chest => 11
  | .doorClosed => 12
  | .doorClosedTop => 13
  | .doorOpen => 14
  | .doorOpenTop => 15
  | .silver => 16
  | .stormAltar => 17
  | .amber => 18
  | .geode => 19
  | .platform => 20
  | .homeBeacon => 21
  | .supplyCache => 22
  | .openedCache => 23
  | .sunleaf => 24
  | .sunleafSprout => 25
  | .ruinAltar => 26
  | .ruinBrick => 27
  | .rail => 28
  | .lifeCrystal => 29
  | .highlandBerry => 30
  | .deepsteel => 31
  | .mineMarker => 32
  | .cavernAltar => 33
  | .obsidian => 34
  | .magmaMarker => 35
  | .cobweb => 36
  | .bed => 37
  | .furnace => 38
  | .ironAnvil => 39
  | .sawmill => 40
  | .loom => 41
  | .tinOre => 42
  | .leadOre => 43
  | .tungstenOre => 44
  | .goldOre => 45
  | .platinumOre => 46
  | .treeTrunk => 47
  | .leadAnvil => 48
  | .woodChair => 49
  | .woodTable => 50
  | .doorClosedMiddle => 51
  | .doorOpenMiddle => 52
  | .demonAltar => 53
  | .crimsonAltar => 54
  | .demoniteOre => 55
  | .crimtaneOre => 56
  | .corruptGrass => 57
  | .crimsonGrass => 58
  | .ebonstone => 59
  | .crimstone => 60
  | .shadowOrb => 61
  | .crimsonHeart => 62
  | .clayPot => 63
  | .vileMushroom => 64
  | .viciousMushroom => 65
  | .sand => 66
  | .placedBottle => 67
  | .glassBlock => 68
  | .blueDungeonBrick => 69
  | .greenDungeonBrick => 70
  | .pinkDungeonBrick => 71
  | .dungeonKeyChest => 72
  | .dungeonGoldChest => 73
  | .dungeonOpenChest => 74
  | .ash => 75
  | .obsidianBrick => 76
  | .shadowChest => 77
  | .shadowOpenChest => 78
  | .hellstone => 79
  | .hellforge => 80
  | .demoniteBrick => 81
  | .crimtaneBrick => 82
  | .hallowedGrass => 83
  | .pearlstone => 84
  | .pearlsand => 85
  | .ebonsand => 86
  | .crimsand => 87
  | .purpleIce => 88
  | .pinkIce => 89
  | .redIce => 90
  | .cobaltOre => 91
  | .palladiumOre => 92
  | .mythrilOre => 93
  | .orichalcumOre => 94
  | .adamantiteOre => 95
  | .titaniumOre => 96
  | .mythrilAnvil => 97
  | .orichalcumAnvil => 98
  | .adamantiteForge => 99
  | .titaniumForge => 100
  | .crystalShard => 101
  | .gelatinCrystal => 102
  | .fishingCrate => 103
  | .mud => 104
  | .jungleGrass => 105
  | .honeyBlock => 106
  | .crispyHoneyBlock => 107
  | .planteraBulb => 108
  | .lihzahrdBrick => 109
  | .lihzahrdChest => 110
  | .lihzahrdAltar => 111
  | .lihzahrdDoorLocked0 => 112
  | .lihzahrdDoorLocked1 => 113
  | .lihzahrdDoorLocked2 => 114
  | .lihzahrdDoorClosed0 => 115
  | .lihzahrdDoorClosed1 => 116
  | .lihzahrdDoorClosed2 => 117
  | .lihzahrdDoorRight => 118
  | .lihzahrdDoorRightLeaf => 119
  | .lihzahrdDoorLeft => 120
  | .lihzahrdDoorLeftLeaf => 121
  | .lihzahrdPressurePlate => 122
  | .lihzahrdDartTrap => 123
  | .lihzahrdFlameTrap => 124
  | .lihzahrdSpikyBallTrap => 125
  | .lihzahrdSpearTrap => 126
  | .woodenSpikes => 127
  | .lihzahrdAltarUpperLeft => 128
  | .lihzahrdAltarUpperMiddle => 129
  | .lihzahrdAltarUpperRight => 130
  | .lihzahrdAltarLowerLeft => 131
  | .lihzahrdAltarLowerMiddle => 132
  | .lihzahrdAltarLowerRight => 133

def Tile.ofCode? : UInt8 → Option Tile
  | 0 => some .air
  | 1 => some .dirt
  | 2 => some .stone
  | 3 => some .copper
  | 4 => some .crystal
  | 5 => some .workbench
  | 6 => some .bedrock
  | 7 => some .timber
  | 8 => some .torch
  | 9 => some .ice
  | 10 => some .iron
  | 11 => some .chest
  | 12 => some .doorClosed
  | 13 => some .doorClosedTop
  | 14 => some .doorOpen
  | 15 => some .doorOpenTop
  | 16 => some .silver
  | 17 => some .stormAltar
  | 18 => some .amber
  | 19 => some .geode
  | 20 => some .platform
  | 21 => some .homeBeacon
  | 22 => some .supplyCache
  | 23 => some .openedCache
  | 24 => some .sunleaf
  | 25 => some .sunleafSprout
  | 26 => some .ruinAltar
  | 27 => some .ruinBrick
  | 28 => some .rail
  | 29 => some .lifeCrystal
  | 30 => some .highlandBerry
  | 31 => some .deepsteel
  | 32 => some .mineMarker
  | 33 => some .cavernAltar
  | 34 => some .obsidian
  | 35 => some .magmaMarker
  | 36 => some .cobweb
  | 37 => some .bed
  | 38 => some .furnace
  | 39 => some .ironAnvil
  | 40 => some .sawmill
  | 41 => some .loom
  | 42 => some .tinOre
  | 43 => some .leadOre
  | 44 => some .tungstenOre
  | 45 => some .goldOre
  | 46 => some .platinumOre
  | 47 => some .treeTrunk
  | 48 => some .leadAnvil
  | 49 => some .woodChair
  | 50 => some .woodTable
  | 51 => some .doorClosedMiddle
  | 52 => some .doorOpenMiddle
  | 53 => some .demonAltar
  | 54 => some .crimsonAltar
  | 55 => some .demoniteOre
  | 56 => some .crimtaneOre
  | 57 => some .corruptGrass
  | 58 => some .crimsonGrass
  | 59 => some .ebonstone
  | 60 => some .crimstone
  | 61 => some .shadowOrb
  | 62 => some .crimsonHeart
  | 63 => some .clayPot
  | 64 => some .vileMushroom
  | 65 => some .viciousMushroom
  | 66 => some .sand
  | 67 => some .placedBottle
  | 68 => some .glassBlock
  | 69 => some .blueDungeonBrick
  | 70 => some .greenDungeonBrick
  | 71 => some .pinkDungeonBrick
  | 72 => some .dungeonKeyChest
  | 73 => some .dungeonGoldChest
  | 74 => some .dungeonOpenChest
  | 75 => some .ash
  | 76 => some .obsidianBrick
  | 77 => some .shadowChest
  | 78 => some .shadowOpenChest
  | 79 => some .hellstone
  | 80 => some .hellforge
  | 81 => some .demoniteBrick
  | 82 => some .crimtaneBrick
  | 83 => some .hallowedGrass
  | 84 => some .pearlstone
  | 85 => some .pearlsand
  | 86 => some .ebonsand
  | 87 => some .crimsand
  | 88 => some .purpleIce
  | 89 => some .pinkIce
  | 90 => some .redIce
  | 91 => some .cobaltOre
  | 92 => some .palladiumOre
  | 93 => some .mythrilOre
  | 94 => some .orichalcumOre
  | 95 => some .adamantiteOre
  | 96 => some .titaniumOre
  | 97 => some .mythrilAnvil
  | 98 => some .orichalcumAnvil
  | 99 => some .adamantiteForge
  | 100 => some .titaniumForge
  | 101 => some .crystalShard
  | 102 => some .gelatinCrystal
  | 103 => some .fishingCrate
  | 104 => some .mud
  | 105 => some .jungleGrass
  | 106 => some .honeyBlock
  | 107 => some .crispyHoneyBlock
  | 108 => some .planteraBulb
  | 109 => some .lihzahrdBrick
  | 110 => some .lihzahrdChest
  | 111 => some .lihzahrdAltar
  | 112 => some .lihzahrdDoorLocked0
  | 113 => some .lihzahrdDoorLocked1
  | 114 => some .lihzahrdDoorLocked2
  | 115 => some .lihzahrdDoorClosed0
  | 116 => some .lihzahrdDoorClosed1
  | 117 => some .lihzahrdDoorClosed2
  | 118 => some .lihzahrdDoorRight
  | 119 => some .lihzahrdDoorRightLeaf
  | 120 => some .lihzahrdDoorLeft
  | 121 => some .lihzahrdDoorLeftLeaf
  | 122 => some .lihzahrdPressurePlate
  | 123 => some .lihzahrdDartTrap
  | 124 => some .lihzahrdFlameTrap
  | 125 => some .lihzahrdSpikyBallTrap
  | 126 => some .lihzahrdSpearTrap
  | 127 => some .woodenSpikes
  | 128 => some .lihzahrdAltarUpperLeft
  | 129 => some .lihzahrdAltarUpperMiddle
  | 130 => some .lihzahrdAltarUpperRight
  | 131 => some .lihzahrdAltarLowerLeft
  | 132 => some .lihzahrdAltarLowerMiddle
  | 133 => some .lihzahrdAltarLowerRight
  | _ => none

def Tile.isDoor : Tile → Bool
  | .doorClosed | .doorClosedTop | .doorOpen | .doorOpenTop | .doorClosedMiddle | .doorOpenMiddle => true
  | _ => false

def Tile.isEvilAltar : Tile → Bool
  | .demonAltar | .crimsonAltar => true
  | _ => false

def Tile.isTempleAltar : Tile → Bool
  | .lihzahrdAltar | .lihzahrdAltarUpperLeft | .lihzahrdAltarUpperMiddle | .lihzahrdAltarUpperRight
  | .lihzahrdAltarLowerLeft | .lihzahrdAltarLowerMiddle | .lihzahrdAltarLowerRight => true
  | _ => false

def Tile.isDungeonBrick : Tile → Bool
  | .blueDungeonBrick | .greenDungeonBrick | .pinkDungeonBrick => true
  | _ => false

def Tile.isDungeonChest : Tile → Bool
  | .dungeonKeyChest | .dungeonGoldChest | .dungeonOpenChest => true
  | _ => false

def Tile.solid : Tile → Bool
  | .air | .torch | .doorOpen | .doorOpenTop | .doorOpenMiddle | .platform | .homeBeacon | .supplyCache | .openedCache => false
  | .sunleaf | .sunleafSprout | .ruinAltar | .rail | .lifeCrystal | .highlandBerry => false
  | .mineMarker | .cavernAltar | .magmaMarker => false
  | .cobweb | .bed | .furnace | .hellforge | .ironAnvil | .sawmill | .loom => false
  | .treeTrunk | .leadAnvil | .woodChair | .woodTable => false
  | .demonAltar | .crimsonAltar | .shadowOrb | .crimsonHeart | .clayPot => false
  | .vileMushroom | .viciousMushroom | .placedBottle => false
  | .dungeonKeyChest | .dungeonGoldChest | .dungeonOpenChest => false
  | .shadowChest | .shadowOpenChest | .mythrilAnvil | .orichalcumAnvil | .adamantiteForge | .titaniumForge => false
  | .crystalShard | .gelatinCrystal | .fishingCrate | .planteraBulb => false
  | .lihzahrdChest | .lihzahrdAltar => false
  | .lihzahrdPressurePlate
  | .lihzahrdAltarUpperLeft | .lihzahrdAltarUpperMiddle | .lihzahrdAltarUpperRight
  | .lihzahrdAltarLowerLeft | .lihzahrdAltarLowerMiddle | .lihzahrdAltarLowerRight => false
  | .lihzahrdDoorRight | .lihzahrdDoorRightLeaf | .lihzahrdDoorLeft | .lihzahrdDoorLeftLeaf => false
  | _ => true

structure World where
  width : Nat
  height : Nat
  tiles : ByteArray
  deriving BEq, Inhabited

def World.contains (world : World) (pos : TilePos) : Bool :=
  0 ≤ pos.x && pos.x < (world.width : Int) &&
    0 ≤ pos.y && pos.y < (world.height : Int)

private def World.index (world : World) (pos : TilePos) : Nat :=
  pos.y.toNat * world.width + pos.x.toNat

def World.tile? (world : World) (pos : TilePos) : Option Tile := do
  if world.contains pos then
    let code ← world.tiles[world.index pos]?
    Tile.ofCode? code
  else
    none

/-- The edge of the finite map behaves as solid bedrock. -/
def World.tile (world : World) (pos : TilePos) : Tile :=
  (world.tile? pos).getD .bedrock

/-- A tile sees the sky when no solid tile lies directly above it. -/
def World.clearSkyAbove (world : World) (position : TilePos) : Bool := Id.run do
  if !world.contains position then return false
  for y in [:position.y.toNat] do
    if (world.tile ⟨position.x, y⟩).solid then return false
  return true

def World.setTile (world : World) (pos : TilePos) (tile : Tile) : World :=
  let index := world.index pos
  if world.contains pos && index < world.tiles.size then
    { world with tiles := world.tiles.set! index tile.code }
  else
    world

/-- Resolve any tile of a complete door to its bottom. Imported doors can be two tiles tall. -/
def doorBase (world : World) (pos : TilePos) : Option TilePos :=
  let atOffset := fun dy => world.tile { pos with y := pos.y + dy }
  let bottom := fun top middle =>
    if atOffset (-1) == top || (atOffset (-1) == middle && atOffset (-2) == top) then some pos else none
  let upper := fun base middle =>
    if atOffset 1 == base then some { pos with y := pos.y + 1 }
    else if atOffset 1 == middle && atOffset 2 == base then some { pos with y := pos.y + 2 }
    else none
  let center := fun base top =>
    if atOffset 1 == base && atOffset (-1) == top then some { pos with y := pos.y + 1 } else none
  match world.tile pos with
  | .doorClosed => bottom .doorClosedTop .doorClosedMiddle
  | .doorOpen => bottom .doorOpenTop .doorOpenMiddle
  | .doorClosedTop => upper .doorClosed .doorClosedMiddle
  | .doorOpenTop => upper .doorOpen .doorOpenMiddle
  | .doorClosedMiddle => center .doorClosed .doorClosedTop
  | .doorOpenMiddle => center .doorOpen .doorOpenTop
  | _ => none

def doorCells (world : World) (position : TilePos) : Array TilePos :=
  match doorBase world position with
  | none => #[]
  | some base =>
    let above := { base with y := base.y - 1 }
    if world.tile above == .doorClosedMiddle || world.tile above == .doorOpenMiddle then
      #[base, above, { above with y := above.y - 1 }]
    else #[base, above]

def doorSupportValid (world : World) (base : TilePos) : Bool :=
  let support := fun pos => (world.tile? pos).any (fun tile => tile.solid && !tile.isDoor)
  let height := (doorCells world base).size
  height > 0 && support { base with y := base.y + 1 } &&
    (height == 2 || support { base with y := base.y - 3 })

def defaultSeed : UInt64 := 42
def legacyWorldWidth : Nat := 256
def preHighlandsWorldWidth : Nat := 384
def worldWidth : Nat := 512
def worldHeight : Nat := 128
def underworldStart : Nat := worldHeight
def underworldHeight : Nat := 176
def highlandStart : Nat := preHighlandsWorldWidth
def homeGroveStart : Nat := 6
def homeGroveEnd : Nat := 13
def frostCavernStart : Nat := 76
def frostCavernEnd : Nat := 133
def wardenArenaStart : Nat := 134
def wardenArenaEnd : Nat := 164
def wardenFightStart : Nat := 140
def wardenSpawnColumn : Nat := 156
def wardenCheckpointColumn : Nat := 136
def wardenArenaFloor : Nat := 66
def quarryStart : Nat := 164
def quarryEnd : Nat := 308
def quarrySurfaceEntrance : Nat := 202
def quarryShaftStart : Nat := 215
def quarryShaftEnd : Nat := 217
def quarrySluice : TilePos := ⟨235, 75⟩
def stormTerrainStart : Nat := 309
def stormTerrainEnd : Nat := 374
def sentrySpawnColumn : Nat := 319
def sentryFloor : Nat := 62
def stormArenaStart : Nat := 329
def stormArenaEnd : Nat := 366
def stormArenaFloor : Nat := 62
def stormAltar : TilePos := ⟨334, 57⟩
def amberBluffsStart : Nat := 168
def amberBluffsEnd : Nat := 198
def geodeGrottoStart : Nat := 289
def geodeGrottoEnd : Nat := 341
def ruinEntrance : TilePos := ⟨377, 61⟩
def ruinShaftColumn : Nat := 379
def ruinShaftFloor : Nat := 96
def ruinCheckpointColumn : Nat := 372
def ruinArenaStart : Nat := 344
def ruinArenaEnd : Nat := 367
def ruinArenaFloor : Nat := 98
def ruinAltar : TilePos := ⟨364, 97⟩
def highlandVaultPassage : TilePos := ⟨383, 63⟩

def inHighlands (position : TilePos) : Bool :=
  highlandStart ≤ position.x && position.x + 1 < worldWidth

def inAmberBluffs (position : TilePos) : Bool :=
  amberBluffsStart ≤ position.x && position.x ≤ amberBluffsEnd &&
    21 ≤ position.y && position.y < 52

def inGeodeGrotto (position : TilePos) : Bool :=
  geodeGrottoStart ≤ position.x && position.x ≤ geodeGrottoEnd &&
    68 ≤ position.y && position.y < 89

def initialPreFishingWaterLevel (position : TilePos) : Nat :=
  if 220 ≤ position.x && position.x ≤ 234 && 74 ≤ position.y && position.y ≤ 75 then 8
  else 0

def inWardenArena (position : TilePos) : Bool :=
  wardenFightStart ≤ position.x && position.x < wardenArenaEnd &&
    55 ≤ position.y && position.y < wardenArenaFloor

/-- SplitMix64's mixing function gives a deterministic word at each grid point. -/
private def terrainWord (seed : UInt64) (x y : Nat) : UInt64 :=
  let value := seed + x.toUInt64 * 0x9e3779b97f4a7c15 + y.toUInt64 * 0xd1b54a32d192ed03
  let value := (value ^^^ (value >>> 30)) * 0xbf58476d1ce4e5b9
  let value := (value ^^^ (value >>> 27)) * 0x94d049bb133111eb
  value ^^^ (value >>> 31)

private def sample (seed : UInt64) (x y : Nat) : Nat :=
  (terrainWord seed x y &&& 255).toNat

private def blend (left right offset span : Nat) : Nat :=
  (left * (span - offset) + right * offset) / span

private def noise (seed : UInt64) (x y spanX spanY : Nat) : Nat :=
  let col := x / spanX
  let row := y / spanY
  let offset := x % spanX
  let upper := blend (sample seed col row) (sample seed (col + 1) row) offset spanX
  let lower := blend (sample seed col (row + 1)) (sample seed (col + 1) (row + 1)) offset spanX
  blend upper lower (y % spanY) spanY

private def preHighlandsGroundHeight (seed : UInt64) (x : Nat) : Nat :=
  -- Gradually blend the level spawn shelf into the surrounding hills.
  let distance := max (9 - x) (x - 30)
  let hill := 32 + noise seed x 0 18 1 / 16
  blend 40 hill (min distance 12) 12

private def highlandHillHeight (seed : UInt64) (x : Nat) : Nat :=
  let seam := preHighlandsGroundHeight seed (highlandStart - 2)
  let glade := max 39 (seam - (3 + (terrainWord seed highlandStart 1).toNat % 3))
  if x ≤ 386 then seam
  else if x < 410 then blend seam glade (x - 386) 24
  else if x ≤ 430 then glade
  else
    let offset := x - 430
    let ridge (index : Nat) :=
      if index == 0 then glade
      else glade + 6 - (terrainWord seed (430 + index * 12) 3).toNat % 13
    blend (ridge (offset / 12)) (ridge (offset / 12 + 1)) (offset % 12) 12

private def highlandRampFloor (x : Nat) : Nat :=
  highlandVaultPassage.y.toNat + 1 - (x - highlandStart)

private def highlandRampExposed (x hill : Nat) : Bool :=
  let ramp := highlandRampFloor x
  highlandStart ≤ x && hill < ramp && ramp ≤ hill + 3

/-- First open stair west of the camping glade. -/
def highlandTrailMouth (seed : UInt64) : TilePos :=
  let columns := List.range' highlandStart (410 - highlandStart)
  let column := (columns.find? fun x => highlandRampExposed x (highlandHillHeight seed x)).getD 409
  ⟨column, highlandRampFloor column - 1⟩

def groundHeight (seed : UInt64) (x : Nat) : Nat :=
  if x + 1 < highlandStart then preHighlandsGroundHeight seed x
  else
    let hill := highlandHillHeight seed x
    if highlandRampExposed x hill then highlandRampFloor x else hill

def surfacePondTop (seed : UInt64) : Nat :=
  -- Align the surface with the existing east bank so swimmers can climb back out.
  groundHeight seed 6

def surfacePondContains (seed : UInt64) (position : TilePos) : Bool :=
  if position.x < 2 || 5 < position.x then false else
    let top := surfacePondTop seed
    (top : Int) ≤ position.y && position.y < top + 3

def initialWaterLevel (seed : UInt64) (position : TilePos) : Nat :=
  if surfacePondContains seed position then 8 else initialPreFishingWaterLevel position

/-- Terrain, water and a one-tile margin to protect builds during old-save migration. -/
def pondFootprint (seed : UInt64) : Array TilePos := Id.run do
  let top := surfacePondTop seed
  let upper := ([2, 3, 4, 5] : List Nat).foldl (fun low x => min low (groundHeight seed x)) top
  let mut cells := #[]
  for y in [upper - 1:top + 5] do
    for x in [:7] do
      cells := cells.push (⟨x, y⟩ : TilePos)
  return cells

private def caveFloor (x : Nat) : Nat :=
  40 + 2 * min ((x - 24) / 7) 6

private def naturalTerrainTile (width : Nat) (seed : UInt64) (x y : Nat) : Tile :=
  let ground := preHighlandsGroundHeight seed x
  let floor := caveFloor x
  if x == 0 || x + 1 == width || y + 2 ≥ worldHeight then .bedrock
  else if x == 16 && y == 39 then .workbench
  else if 35 ≤ x && x ≤ 37 && y == 42 then .copper
  else if x == 64 && 47 ≤ y && y ≤ 49 then .crystal
  -- The opening leads down through two-tile terraces that are also jumpable on return.
  else if 24 ≤ x && x ≤ 75 && floor - 6 ≤ y && y < floor then .air
  else if 24 ≤ x && x ≤ 75 && (y == floor || y == floor + 1) then .stone
  else if (x == 14 || (x ≥ 80 && sample seed x 0 < 20)) && ground - 2 ≤ y && y < ground then
    .timber
  else if y < ground then .air
  else if y < ground + 4 then .dirt
  else if y ≥ ground + 6 && noise (seed + 1) x y 15 11 < 88 then .air
  else if y ≥ 53 && noise (seed + 2) x y 6 6 < 53 then .copper
  else if y ≥ 70 && noise (seed + 3) x y 5 5 < 44 then .crystal
  else .stone

private def legacyTerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  naturalTerrainTile legacyWorldWidth seed x y

private def frostFloor (x : Nat) : Nat :=
  -- Split the descent around a wider chamber where the first iron vein is exposed.
  if x < 94 then 52 + 2 * ((x - frostCavernStart) / 6)
  else if x < 107 then 58
  else min 66 (60 + 2 * ((x - 107) / 6))

private def archRise (x left right height : Nat) : Nat :=
  min height (min (x - left) (right - x))

private def frostCeiling (x : Nat) : Nat :=
  let floor := frostFloor x
  let rise := archRise x 82 94 3 + archRise x 94 107 4 + archRise x 109 125 5
  floor - (6 + rise)

private def frostTerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  let floor := frostFloor x
  let ceiling := frostCeiling x
  let iceThickness := 2 + noise (seed + 4) x 0 5 1 / 100
  if x == 93 && y == 51 then .workbench
  else if 92 ≤ x && x ≤ 93 && y == 52 then .timber
  else if ceiling ≤ y && y < floor then .air
  else if 95 ≤ x && x ≤ 98 && y == floor then .iron
  else if 114 ≤ x && x ≤ 117 && floor ≤ y && y ≤ floor + 1 then .iron
  else if ceiling - iceThickness ≤ y && y < floor + iceThickness then .ice
  else legacyTerrainTile seed x y

private def v3TerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  if frostCavernStart ≤ x && x ≤ frostCavernEnd then frostTerrainTile seed x y
  else legacyTerrainTile seed x y

private def wardenArenaTile (x y : Nat) : Option Tile :=
  if x < wardenArenaStart || x > wardenArenaEnd then none
  else
    let isChamber := wardenFightStart ≤ x && x < wardenArenaEnd
    let isCorridor := x < wardenFightStart
    let isEastWall := x == wardenArenaEnd
    let crown := archRise x 139 152 3 + archRise x 152 165 3
    if (isChamber && 55 ≤ y && y < wardenArenaFloor) ||
        (isCorridor && 60 ≤ y && y < wardenArenaFloor) then
      if y + 1 == wardenArenaFloor && (x == 138 || x == 144 || x == 152 || x == 160) then
        some .torch
      else some .air
    else if wardenArenaFloor ≤ y && y ≤ wardenArenaFloor + 1 then
      if y == wardenArenaFloor + 1 && isChamber && x % 4 == 0 then some .crystal
      else some .ice
    else if isChamber && 54 - crown ≤ y && y ≤ 54 then
      if y == 54 && x % 4 == 0 then some .crystal
      else some .ice
    else if ((x == wardenFightStart - 1 || isEastWall) && 54 ≤ y && y ≤ 65) ||
        (isCorridor && y == 59) then some .ice
    else none

private def generateTerrainWith (width : Nat) (tileAt : Nat → Nat → Tile) : World := Id.run do
  let mut tiles := ByteArray.emptyWithCapacity (width * worldHeight)
  for y in [:worldHeight] do
    for x in [:width] do
      tiles := tiles.push (tileAt x y).code
  return { width, height := worldHeight, tiles }

/-- The terrain used by saves through version 2, before the frost cavern was added. -/
def generateLegacyTerrain (seed : UInt64) : World :=
  generateTerrainWith legacyWorldWidth (legacyTerrainTile seed)

def generateV3Terrain (seed : UInt64) : World :=
  generateTerrainWith legacyWorldWidth (v3TerrainTile seed)

private def v4TerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  match wardenArenaTile x y with
  | some tile => tile
  | none => v3TerrainTile seed x y

def generateV4Terrain (seed : UInt64) : World :=
  generateTerrainWith legacyWorldWidth (v4TerrainTile seed)

private def groveTile (seed : UInt64) (x y : Nat) : Option Tile :=
  if x < homeGroveStart || x > homeGroveEnd then none
  else
    let ground := groundHeight seed x
    let trunk := x == 6 || x == 8 || x == 10 || x == 12
    if trunk && ground - 4 ≤ y && y < ground then some .timber
    else if !trunk && y == ground - 4 then some .timber
    else none

private def v5TerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  match groveTile seed x y with
  | some tile => tile
  | none => v4TerrainTile seed x y

def generateV5Terrain (seed : UInt64) : World :=
  generateTerrainWith legacyWorldWidth (v5TerrainTile seed)

private def quarrySurfaceFloor (seed : UInt64) (x : Nat) : Nat :=
  let entry := groundHeight seed quarrySurfaceEntrance
  let steps := (65 - entry) / 2
  min 64 (entry + 2 * (min 41 (x - quarrySurfaceEntrance) * steps / 41))

private def quarryChannelFloor (x : Nat) : Nat :=
  -- The basin drains into a channel two tiles lower. Steps bring it back up to the dry hall.
  if x < 236 then 76
  else 78 - 2 * ((x - 254) / 4)

private def quarryHighFloor (x : Nat) : Nat :=
  if (239 ≤ x && x ≤ 248) || (274 ≤ x && x ≤ 277) then 68 else 66

private def quarryTile (seed : UInt64) (x y : Nat) : Option Tile := Id.run do
  if x < quarryStart || x > quarryEnd then return none
  if x == quarryStart then
    return if y == 63 then some .crystal else none
  -- Keep the old boundary column everywhere except the two through passages.
  if x + 1 == legacyWorldWidth then
    return some (if (60 ≤ y && y ≤ 65) || (70 ≤ y && y ≤ 77) then .air else .bedrock)
  if quarrySurfaceEntrance ≤ x && x ≤ 247 then
    let floor := quarrySurfaceFloor seed x
    if y == floor then return some .stone
    if floor - 6 ≤ y && y < floor then
      return some (if y + 1 == floor && (x == 203 || x == 214 || x == 229 || x == 246) then
        .torch else .air)
  if quarryShaftStart ≤ x && x ≤ 273 then
    let floor := quarryChannelFloor x
    let shaft := x ≤ quarryShaftEnd
    if y == floor || y == floor + 1 then return some .stone
    if 74 ≤ y && y ≤ 75 && (x == 218 || x == 219 || x == quarrySluice.x.toNat) then
      return some .stone
    if (if shaft then 66 else min 70 (floor - 4)) ≤ y && y < floor && y > 66 then
      return some (if (y == 73 && (x == 219 || x == quarrySluice.x.toNat)) ||
        (y == 70 && x == 242) || (y == 68 && x == 266) then .torch else .air)
    -- The western shaft and eastern stair opening are both short enough to jump over.
    if (shaft || 272 ≤ x) && y == 66 then return some .air
  let floor := quarryHighFloor x
  let arch := archRise x 165 192 3 + archRise x 261 288 3 + archRise x 278 309 4
  let ceiling := floor - 6 - arch
  if x == 281 && y == 61 then return some .workbench
  if 280 ≤ x && x ≤ 281 && y == 62 then return some .timber
  if ceiling ≤ y && y < floor then
    return some (if y + 1 == floor && (x == 173 || x == 195 || x == 212 || x == 230 ||
      x == 251 || x == 265 || x == 282 || x == 293 || x == 305) then .torch else .air)
  if floor ≤ y && y ≤ floor + 1 then
    return some (if (y == floor && 285 ≤ x && x ≤ 288) ||
      (301 ≤ x && x ≤ 305) then .silver else .stone)
  if y + 1 == ceiling then
    return some (if x % 11 == 0 then .crystal else .stone)
  return none

private def v6TerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  match quarryTile seed x y with
  | some tile => tile
  | none => if x < legacyWorldWidth then v5TerrainTile seed x y
    else naturalTerrainTile preHighlandsWorldWidth seed x y

def generateV6Terrain (seed : UInt64) : World :=
  generateTerrainWith preHighlandsWorldWidth (v6TerrainTile seed)

private def stormTerrainTile (x y : Nat) : Option Tile := Id.run do
  if x < stormTerrainStart || x > stormTerrainEnd then return none
  let floor := if x < 312 then 66 else if x < 316 then 64 else stormArenaFloor
  let chamber := stormArenaStart ≤ x && x ≤ stormArenaEnd
  let ceiling := if chamber then 52 - archRise x 329 367 3
    else if x < 326 then floor - 8 else floor - 6
  if (⟨x, y⟩ : TilePos) == stormAltar then return some .stormAltar
  if (x == 326 && y == 58) || (333 ≤ x && x ≤ 334 && y == 58) then return some .stone
  if x == 326 && y == 57 then return some .workbench
  if x == 323 && 60 ≤ y && y < floor then return some .stone
  if ceiling ≤ y && y < floor then
    return some (if y + 1 == floor && (x == 310 || x == 316 || x == 321 || x == 327 ||
      x == 335 || x == 344 || x == 353 || x == 362 || x == 370) then .torch else .air)
  if floor ≤ y && y ≤ floor + 1 then return some .stone
  if y + 1 == ceiling then return some (if chamber && x % 6 == 0 then .crystal else .stone)
  return none

private def v7TerrainTile (seed : UInt64) (x y : Nat) : Tile :=
  match stormTerrainTile x y with
  | some tile => tile
  | none => v6TerrainTile seed x y

def generateV7Terrain (seed : UInt64) : World :=
  generateTerrainWith preHighlandsWorldWidth (v7TerrainTile seed)

def amberBluffSurface (seed : UInt64) (x : Nat) : Nat :=
  groundHeight seed x - archRise x amberBluffsStart (amberBluffsEnd + 1) 6 / 2

private def amberBluffTile (seed : UInt64) (x y : Nat) : Option Tile := Id.run do
  if x < amberBluffsStart || x > amberBluffsEnd then return none
  let floor := amberBluffSurface seed x
  if floor - 6 ≤ y && y < floor then return some .air
  if floor ≤ y && y ≤ floor + 2 then
    if (183 ≤ x && x ≤ 186 && y == floor) || (190 ≤ x && x ≤ 192 && y > floor) then
      return some .amber
    if x ≤ amberBluffsStart + 2 || amberBluffsEnd - 2 ≤ x then return some .dirt
    return some .stone
  return none

private def grottoFloor (x : Nat) : Nat :=
  if x == geodeGrottoStart then 68
  else if x < 295 then 70
  else min 86 (72 + 2 * ((x - 295) / 4))

private def geodeGrottoTile (x y : Nat) : Option Tile := Id.run do
  if x < geodeGrottoStart || x > geodeGrottoEnd then return none
  if x == geodeGrottoStart && y == 65 then return some .torch
  -- The two layers of stone above the entrance remain part of the old quarry road.
  if y < 68 then return none
  let floor := grottoFloor x
  let arch := archRise x 323 (geodeGrottoEnd + 1) 4
  let ceiling := max 68 (floor - 6 - arch)
  if ceiling ≤ y && y < floor then
    return some (if y + 1 == floor && (x == 292 || x == 302 || x == 313 || x == 323 ||
      x == 328 || x == 337) then .torch else .air)
  if floor ≤ y && y ≤ floor + 2 then
    if (329 ≤ x && x ≤ 332 && y == floor) || (336 ≤ x && x ≤ 339 && y > floor) then
      return some .geode
    return some .stone
  if x ≥ 323 && y + 1 == ceiling then return some (if x % 3 == 0 then .geode else .crystal)
  return none

def generatePrePocketsTerrain (seed : UInt64) : World :=
  generateTerrainWith preHighlandsWorldWidth fun x y =>
    match amberBluffTile seed x y with
    | some tile => tile
    | none => match geodeGrottoTile x y with
      | some tile => tile
      | none => v7TerrainTile seed x y

/-- `mouth` is the first breakable cap. `footprint` covers the edited cells and
their one-tile structural margin so legacy saves can skip a locally changed pocket. -/
structure SupplyPocket where
  mouth : TilePos
  cache : TilePos
  edits : Array (TilePos × Tile)
  footprint : Array TilePos
  deriving BEq, Repr, Inhabited

private def editFootprint (edits : Array (TilePos × Tile)) : Array TilePos := Id.run do
  let mut footprint := #[]
  for (pos, _) in edits do
    for dx in [-1, 0, 1] do
      for dy in [-1, 0, 1] do
        let neighbor : TilePos := ⟨pos.x + dx, pos.y + dy⟩
        if !footprint.contains neighbor then footprint := footprint.push neighbor
  return footprint

private def makePocket (seed : UInt64) (family column mainFloor : Nat) (direction : Int) :
    SupplyPocket := Id.run do
  let roll (draw : Nat) := (terrainWord seed family draw).toNat
  let firstStep := 3 + roll 1 % 3
  let chamberStart := firstStep + 2
  let endColumn := chamberStart + 4 + roll 2 % 3
  let deep := roll 3 % 2 == 0
  let extraCap := roll 4 % 2 == 0
  let floorAt (offset : Nat) : Nat :=
    if offset == 0 then mainFloor + 2
    else if offset < firstStep then mainFloor + 4
    else if offset < chamberStart then mainFloor + 6
    else if deep then mainFloor + 8
    else if offset + 2 ≥ endColumn then mainFloor + 4
    else mainFloor + 6
  let xAt (offset : Nat) : Int := (column : Int) + direction * offset
  let mouth : TilePos := ⟨column, mainFloor⟩
  let cacheColumn := endColumn - 1
  let cache : TilePos := ⟨xAt cacheColumn, floorAt cacheColumn - 1⟩
  let mut edits : Array (TilePos × Tile) := #[(⟨column, mainFloor - 1⟩, .torch)]
  for offset in [:endColumn + 1] do
    let x := xAt offset
    let floor := floorAt offset
    if offset < 2 then
      -- Two thin caps cover a pair of two-tile steps off the main road.
      edits := edits.push (⟨x, mainFloor⟩, .stone)
      for y in [mainFloor + 1:floor] do
        let tile := if offset == 1 && extraCap && y == mainFloor + 1 then .stone else .air
        edits := edits.push (⟨x, y⟩, tile)
    else
      let arch := archRise offset chamberStart (endColumn + 1) 2
      let ceiling := max (mainFloor + 2) (floor - 5 - arch)
      edits := edits.push (⟨x, ceiling - 1⟩, .stone)
      for y in [ceiling:floor] do
        let pos : TilePos := ⟨x, y⟩
        let tile := if pos == cache then .supplyCache
          else if offset == chamberStart && y + 1 == floor then .torch else .air
        edits := edits.push (pos, tile)
    for y in [floor:floor + 2] do
      edits := edits.push (⟨x, y⟩, .stone)
  -- Seal the far wall and the side beneath the return step into ordinary cave rock.
  for x in [(column : Int) - direction, xAt (endColumn + 1)] do
    for y in [mainFloor + 1:mainFloor + 10] do
      edits := edits.push (⟨x, y⟩, .stone)
  return { mouth, cache, edits, footprint := editFootprint edits }

/-- Seeded rooms branching off the early cave, frost cavern, and quarry high road. -/
def supplyPockets (seed : UInt64) : Array SupplyPocket :=
  let seed := seed + 11
  let early := 39 + ((terrainWord seed 0 0 >>> 2).toNat % 4)
  let frost := 100 + (terrainWord seed 1 0).toNat % 3
  let quarry := 181 + (terrainWord seed 2 0).toNat % 6
  let quarryDirection := if (terrainWord seed 2 5 &&& 1) == 0 then 1 else -1
  #[makePocket seed 0 early 44 (-1), makePocket seed 1 frost 58 (-1),
    makePocket seed 2 quarry 66 quarryDirection]

def generatePreFishingTerrain (seed : UInt64) : World :=
  (supplyPockets seed).foldl (fun world pocket =>
    pocket.edits.foldl (fun world (position, tile) => world.setTile position tile) world)
    (generatePrePocketsTerrain seed)

def generatePreGardenTerrain (seed : UInt64) : World := Id.run do
  let mut world := generatePreFishingTerrain seed
  let top := surfacePondTop seed
  for y in [top:top + 4] do
    world := world.setTile ⟨1, y⟩ .stone
  for x in [2:6] do
    for y in [min (groundHeight seed x) top:top + 3] do
      world := world.setTile ⟨x, y⟩ .air
    world := world.setTile ⟨x, top + 3⟩ .stone
  return world

private def wildSite (world : World) (seed : UInt64) (column : Nat) : Option TilePos :=
  let floor := groundHeight seed column
  let position : TilePos := ⟨column, (floor - 1 : Nat)⟩
  if world.tile position == .air && world.tile ⟨column, floor⟩ == .dirt &&
      initialWaterLevel seed position == 0 then some position else none

private def wildSites (world : World) (seed : UInt64) : Array TilePos := Id.run do
  let grove := (wildSite world seed 9).toList.toArray
  for column in [38:76] do
    if let some position := wildSite world seed column then
      if world.clearSkyAbove position then return grove.push position
  return grove ++ (wildSite world seed 7).toList.toArray

/-- Wild herbs occupy open ground in the western grove and on an exposed hill. -/
def wildSunleaf (seed : UInt64) : Array TilePos :=
  wildSites (generatePreGardenTerrain seed) seed

def generatePreRuinsTerrain (seed : UInt64) : World :=
  let world := generatePreGardenTerrain seed
  (wildSites world seed).foldl (fun world position => world.setTile position .sunleaf) world

/-- The eastern descent, checkpoint passage, and chamber below the Storm Vault. -/
def ruinEdits : Array (TilePos × Tile) := Id.run do
  let mut edits := #[]
  for x in [375:383] do
    for y in [55:ruinShaftFloor + 2] do
      let pos : TilePos := ⟨x, y⟩
      let tile :=
        if y == 55 || y ≥ ruinShaftFloor || (x ≤ 377 && 62 ≤ y && y ≤ 63) then .ruinBrick
        else if pos == ruinEntrance || (x == 382 && (y == 65 || y == 73 || y == 81 || y == 89 || y == 95)) then
          .torch
        -- Alternating ledges can be stepped off and share one column for jumping back up.
        else if 64 ≤ y && y % 2 == 0 &&
            (if y % 4 == 0 then ruinShaftColumn ≤ x else x ≤ ruinShaftColumn) then .platform
        else .air
      edits := edits.push (pos, tile)
  for y in [64:89] do
    edits := edits.push (⟨374, y⟩, .ruinBrick)
  for x in [ruinArenaEnd + 1:375] do
    for y in [89:ruinShaftFloor + 2] do
      let tile := if y == 89 || y ≥ ruinShaftFloor then .ruinBrick
        else if y + 1 == ruinShaftFloor && (x == 369 || x == ruinCheckpointColumn) then .torch
        else .air
      edits := edits.push (⟨x, y⟩, tile)
  for y in [83:ruinArenaFloor + 2] do
    edits := edits.push (⟨ruinArenaStart - 1, y⟩, .ruinBrick)
  for x in [ruinArenaStart:ruinArenaEnd + 1] do
    let ceiling := 88 - archRise x ruinArenaStart (ruinArenaEnd + 1) 4
    for y in [ceiling - 1:ruinArenaFloor + 2] do
      let pos : TilePos := ⟨x, y⟩
      let tile := if pos == ruinAltar then .ruinAltar
        else if y + 1 == ceiling then (if x % 7 == 0 then .crystal else .ruinBrick)
        else if ruinArenaFloor ≤ y then .ruinBrick
        else if y + 1 == ruinArenaFloor && (x == 347 || x == 355 || x == 361) then .torch
        else .air
      edits := edits.push (pos, tile)
  return edits

/-- The ruin and its one-cell structural margin for migration into an old map. -/
def ruinFootprint : Array TilePos := editFootprint ruinEdits

def generatePreLifeCrystalTerrain (seed : UInt64) : World :=
  ruinEdits.foldl (fun world (position, tile) => world.setTile position tile)
    (generatePreRuinsTerrain seed)

/-- The footprint includes the crystal, the approach on either side, and the
floor beneath it, so migration can preserve terrain changes around the site. -/
structure LifeCrystalSite where
  position : TilePos
  footprint : Array TilePos
  deriving BEq, Repr, Inhabited

private def lifeCrystalSite (position : TilePos) : LifeCrystalSite :=
  { position, footprint := editFootprint #[(position, .lifeCrystal)] }

private def pocketCrystalSite (pocket : SupplyPocket) : LifeCrystalSite :=
  -- Leave a tile between the crystal and the cache so an opened cache alone
  -- does not prevent migration. Find the floor of that column in the pocket.
  let direction : Int := if pocket.cache.x < pocket.mouth.x then -1 else 1
  let x := pocket.cache.x - 2 * direction
  let y := pocket.edits.foldl (fun lowest (position, tile) =>
    if position.x == x && tile == .air then max lowest position.y else lowest) pocket.mouth.y
  lifeCrystalSite ⟨x, y⟩

private def earlyCrystalSite (seed : UInt64) : LifeCrystalSite := Id.run do
  let first := 56 + (terrainWord seed 56 1).toNat % 4
  -- High surface terrain can leave the shallower terraces open to the sky.
  let columns := first :: [56, 57, 58, 59, 60, 62] ++ List.range' 66 10
  for x in columns do
    let floor := caveFloor x
    if groundHeight seed x < floor - 6 then return lifeCrystalSite ⟨x, floor - 1⟩
  return lifeCrystalSite ⟨104, frostFloor 104 - 1⟩

/-- One site in each supply pocket, plus three along established cave routes. -/
def originalLifeCrystalSites (seed : UInt64) : Array LifeCrystalSite :=
  let grotto := 324 + (terrainWord seed 324 2).toNat % 4
  let ruin := (#[370, 371, 373, 374] : Array Nat)[(terrainWord seed 370 3).toNat % 4]!
  (supplyPockets seed).map pocketCrystalSite ++ #[
    earlyCrystalSite seed,
    lifeCrystalSite ⟨grotto, grottoFloor grotto - 1⟩,
    lifeCrystalSite ⟨ruin, ruinShaftFloor - 1⟩]

def generatePreHighlandsTerrain (seed : UInt64) : World :=
  (originalLifeCrystalSites seed).foldl
    (fun world site => world.setTile site.position .lifeCrystal)
    (generatePreLifeCrystalTerrain seed)

/-- Seeded patches of three berries on exposed ground east of the old border. -/
def highlandBerryPositions (seed : UInt64) : Array TilePos := Id.run do
  let mut positions := #[]
  for patch in [390, 404, 434, 462, 494] do
    let start := patch + (terrainWord seed patch 9).toNat % 3
    for x in [start:start + 3] do
      positions := positions.push ⟨x, groundHeight seed x - 1⟩
  return positions

def highlandCedarColumns (seed : UInt64) : Array Nat :=
  (#[398, 442, 452, 475, 485, 505] : Array Nat).map
    (fun x => x + (terrainWord seed x 17).toNat % 3 - 1)

/-- The crown rises above nearby uphill ground to leave room for a jump. -/
def highlandCedarCrownFloor (seed : UInt64) (center : Nat) : Nat :=
  let trunkFloor := groundHeight seed center
  let nearby := List.range' (center - 3) 7
  min trunkFloor (2 + nearby.foldl (fun upper x => min upper (groundHeight seed x)) trunkFloor)

def inCedarCrown (x y center floor : Nat) : Bool :=
  let top := floor - 12
  top ≤ y && y ≤ floor - 6 &&
    max x center - min x center ≤ (y - top) / 2

private def highlandTile (x y hill bridgeStart bridgeFloor : Nat) (berries : Array TilePos)
    (cedars : Array (Nat × Nat)) : Tile :=
  let ramp := highlandRampFloor x
  let exposed := highlandRampExposed x hill
  let floor := if exposed then ramp else hill
  if x + 1 == worldWidth || y + 2 ≥ worldHeight then .bedrock
  -- The short deck bridges the lip of the cut for surface travelers, and the
  -- stair below it stays open in both directions.
  else if exposed && bridgeStart ≤ x && y == bridgeFloor && bridgeFloor + 2 < ramp then .platform
  else if y + 1 == floor && cedars.any (·.1 == x) then .timber
  else if y + 1 == floor && berries.contains ⟨x, y⟩ then .highlandBerry
  else if hill < ramp && y == ramp then .stone
  else if hill < ramp && ramp - 5 ≤ y && y < ramp && (exposed || hill < y) then .air
  else if y ≥ hill + 4 then .stone
  else if y ≥ hill then .dirt
  else if cedars.any (fun (center, floor) => inCedarCrown x y center floor) then .timber
  else .air

/-- Extend a 384-column world, opening only untouched boundary bedrock at
the surface and Vault passage. Saves can protect occupied boundary cells. -/
def extendHighlands (seed : UInt64) (previous : World)
    (keepBoundary : TilePos → Bool := fun _ => false) : World := Id.run do
  if previous.width != preHighlandsWorldWidth || previous.height != worldHeight then
    return previous
  let seam := groundHeight seed (highlandStart - 1)
  let berries := highlandBerryPositions seed
  let cedars := (highlandCedarColumns seed).map (fun x => (x, highlandCedarCrownFloor seed x))
  let bridgeStart := (highlandTrailMouth seed).x.toNat
  let bridgeFloor := highlandHillHeight seed (bridgeStart - 1)
  let hills := (List.range' highlandStart (worldWidth - highlandStart)).toArray.map (highlandHillHeight seed)
  let mut tiles := ByteArray.emptyWithCapacity (worldWidth * worldHeight)
  for y in [:worldHeight] do
    for x in [:worldWidth] do
      let code :=
        if x < highlandStart then
          let old := previous.tiles[y * previous.width + x]?.getD Tile.bedrock.code
          let vault := highlandVaultPassage.y.toNat
          if x + 1 == highlandStart && (y < seam || (vault - 3 ≤ y && y ≤ vault)) &&
              old == Tile.bedrock.code &&
              !keepBoundary ⟨x, y⟩ then Tile.air.code
          else old
        else (highlandTile x y hills[x - highlandStart]! bridgeStart bridgeFloor berries cedars).code
      tiles := tiles.push code
  return { width := worldWidth, height := worldHeight, tiles }

def generatePreDeepsteelTerrain (seed : UInt64) : World :=
  extendHighlands seed (generatePreHighlandsTerrain seed)

def deepsteelRoomStart : Nat := 484
def deepsteelRoomEnd : Nat := 504

private def deepsteelStageStart : Nat := deepsteelRoomStart - 7

private def deepsteelMouthColumn (seed : UInt64) : Nat :=
  457 + (terrainWord seed 457 23).toNat % 2

def deepsteelMouth (seed : UInt64) : TilePos :=
  let x := deepsteelMouthColumn seed
  ⟨x, groundHeight seed x⟩

private def deepsteelPassageFloor (mouth lip x : Nat) : Nat :=
  let offset := x - mouth
  lip + if offset ≤ 3 then 2 * (offset + 1)
    else 8 + (min x deepsteelStageStart - (mouth + 3))

def deepsteelFloor (seed : UInt64) : Nat :=
  let mouth := deepsteelMouthColumn seed
  deepsteelPassageFloor mouth (groundHeight seed (mouth - 1)) deepsteelStageStart + 2

def deepsteelMarker (seed : UInt64) : TilePos :=
  ⟨deepsteelRoomEnd - 1, deepsteelFloor seed - 1⟩

private def deepsteelPassageCeiling (surface mouth x floor : Nat) : Nat :=
  if x < mouth + 4 then surface + 1
  else max (surface + 1) (floor - if x < deepsteelStageStart then 5 else 3)

private def deepsteelRoomCeiling (x floor : Nat) : Nat :=
  floor - 7 - archRise x deepsteelRoomStart (deepsteelRoomEnd + 1) 3

/-- The guarded chamber includes both endpoint columns and ends above its floor. -/
def inDeepsteelRoom (seed : UInt64) (position : TilePos) : Bool :=
  let floor := deepsteelFloor seed
  deepsteelRoomStart ≤ position.x && position.x ≤ deepsteelRoomEnd &&
    deepsteelRoomCeiling position.x.toNat floor ≤ position.y && position.y < floor

/-- The excavated corridor and lower chamber, below the Highland surface. -/
def inDeepsteelMine (seed : UInt64) (position : TilePos) : Bool :=
  let mouth := deepsteelMouthColumn seed
  if position.x < mouth then false
  else if deepsteelRoomStart ≤ position.x then inDeepsteelRoom seed position
  else
    let x := position.x.toNat
    let floor := deepsteelPassageFloor mouth (groundHeight seed (mouth - 1)) x
    deepsteelPassageCeiling (groundHeight seed x) mouth x floor ≤ position.y && position.y < floor

/-- A mine and its one-cell structural margin for preserving nearby old builds. -/
structure DeepsteelMine where
  mouth : TilePos
  caps : Array TilePos
  floor : Nat
  roomStart : Nat
  roomEnd : Nat
  ore : Array TilePos
  marker : TilePos
  edits : Array (TilePos × Tile)
  footprint : Array TilePos
  deriving BEq, Repr, Inhabited

def deepsteelMine (seed : UInt64) : DeepsteelMine := Id.run do
  let mouth := deepsteelMouth seed
  let entrance := mouth.x.toNat
  let lip := groundHeight seed (entrance - 1)
  let stageFloor := deepsteelPassageFloor entrance lip deepsteelStageStart
  let floor := stageFloor + 2
  let marker := deepsteelMarker seed
  let caps := #[mouth, ⟨entrance + 1, groundHeight seed (entrance + 1)⟩]
  let firstVein := (List.range' deepsteelStageStart 4).toArray.map
    (fun (x : Nat) => (⟨x, stageFloor - 4⟩ : TilePos))
  let rearVein := (List.range' (floor - 5) 4).toArray.map
    (fun (y : Nat) => (⟨deepsteelRoomEnd + 1, y⟩ : TilePos))
  let mut edits : Array (TilePos × Tile) := #[(⟨mouth.x, mouth.y - 1⟩, .torch)]
  for x in [entrance:deepsteelRoomStart] do
    let surface := groundHeight seed x
    let stepFloor := deepsteelPassageFloor entrance lip x
    let ceiling := deepsteelPassageCeiling surface entrance x stepFloor
    if x < entrance + 4 then
      -- A two-tile cap opens onto two timber panels. The stairs pass below.
      edits := edits.push (⟨x, surface⟩, if x < entrance + 2 then .stone else .platform)
    else if surface + 1 < ceiling then
      let roof := (⟨x, ceiling - 1⟩ : TilePos)
      edits := edits.push (roof, if firstVein.contains roof then .deepsteel else .stone)
    for y in [ceiling:stepFloor] do
      let lit := y + 1 == stepFloor && (x == entrance + 7 || x == deepsteelStageStart - 1)
      edits := edits.push (⟨x, y⟩, if lit then .torch else .air)
    for y in [stepFloor:stepFloor + 2] do
      edits := edits.push (⟨x, y⟩, .stone)
  for x in [deepsteelRoomStart:deepsteelRoomEnd + 1] do
    let ceiling := deepsteelRoomCeiling x floor
    edits := edits.push (⟨x, ceiling - 1⟩, .stone)
    for y in [ceiling:floor] do
      let position : TilePos := ⟨x, y⟩
      let lit := y + 1 == floor && (x == 485 || x == 492 || x == 500)
      let tile := if position == marker then .mineMarker else if lit then .torch else .air
      edits := edits.push (position, tile)
    for y in [floor:floor + 2] do
      edits := edits.push (⟨x, y⟩, .stone)
  for y in [floor - 10:floor + 2] do
    let position : TilePos := ⟨deepsteelRoomEnd + 1, y⟩
    edits := edits.push (position, if rearVein.contains position then .deepsteel else .stone)
  return {
    mouth, caps, floor, roomStart := deepsteelRoomStart, roomEnd := deepsteelRoomEnd
    ore := firstVein ++ rearVein, marker, edits, footprint := editFootprint edits }

def generatePreCavernTerrain (seed : UInt64) : World :=
  (deepsteelMine seed).edits.foldl
    (fun world (position, tile) => world.setTile position tile)
    (generatePreDeepsteelTerrain seed)

def cavernShaftColumn : Nat := 502
def cavernCheckpointColumn : Nat := 496
def cavernCheckpointFloor : Nat := 98
def cavernArenaStart : Nat := 450
def cavernArenaEnd : Nat := 492
def cavernArenaFloor : Nat := 100
def cavernAltar : TilePos := ⟨487, cavernArenaFloor - 1⟩

def cavernEntrance (seed : UInt64) : TilePos :=
  ⟨cavernShaftColumn - 1, deepsteelFloor seed⟩

private def cavernArenaCeiling (x : Nat) : Nat :=
  cavernArenaFloor - 9 - archRise x cavernArenaStart (cavernArenaEnd + 1) 3

private def cavernApproachFloor (x : Nat) : Nat :=
  if cavernCheckpointColumn - 1 ≤ x && x ≤ cavernCheckpointColumn + 2 then cavernCheckpointFloor
  else cavernArenaFloor

def inCavernArena (position : TilePos) : Bool :=
  cavernArenaStart ≤ position.x && position.x ≤ cavernArenaEnd &&
    cavernArenaFloor - 12 ≤ position.y && position.y < cavernArenaFloor

/-- The excavated shaft, checkpoint passage, and arena under the Deepsteel Mine. -/
def inHighlandCavern (seed : UInt64) (position : TilePos) : Bool :=
  let x := position.x.toNat
  let top := deepsteelFloor seed
  if inCavernArena position then cavernArenaCeiling x ≤ position.y
  else if cavernArenaEnd < position.x && position.x < cavernShaftColumn - 2 then
    92 ≤ position.y && position.y < cavernApproachFloor x
  else
    cavernShaftColumn - 2 ≤ position.x && position.x ≤ cavernShaftColumn + 2 &&
      position.y < cavernArenaFloor &&
      (top + 2 ≤ position.y ||
        (position.y == (top : Int) + 1 && cavernShaftColumn - 1 ≤ position.x &&
          position.x ≤ cavernShaftColumn))

/-- The entire lower cavern is installed together, including its access shaft. -/
structure HighlandCavern where
  entrance : TilePos
  caps : Array TilePos
  altar : TilePos
  edits : Array (TilePos × Tile)
  footprint : Array TilePos
  deriving BEq, Repr, Inhabited

def highlandCavern (seed : UInt64) : HighlandCavern := Id.run do
  let entrance := cavernEntrance seed
  let top := deepsteelFloor seed
  let caps : Array TilePos := #[entrance, ⟨cavernShaftColumn, top⟩]
  let mut edits : Array (TilePos × Tile) := #[(⟨entrance.x, entrance.y - 1⟩, .torch)]
  for cap in caps do
    edits := edits.push (cap, .stone)
    edits := edits.push (⟨cap.x, cap.y + 1⟩, .air)
  for x in [cavernShaftColumn - 2:cavernShaftColumn + 3] do
    for y in [top + 2:cavernArenaFloor + 2] do
      let offset := y - top
      let tile :=
        if cavernArenaFloor ≤ y then .ruinBrick
        -- Each ledge shares the middle column with the next one above it.
        else if offset % 2 == 0 &&
            (if offset % 4 == 2 then x ≤ cavernShaftColumn else cavernShaftColumn ≤ x) then .platform
        else if x == cavernShaftColumn + 2 && offset % 8 == 3 then .torch
        else .air
      edits := edits.push (⟨x, y⟩, tile)
  for x in [cavernArenaEnd + 1:cavernShaftColumn - 2] do
    let floor := cavernApproachFloor x
    for y in [91:cavernArenaFloor + 2] do
      let tile := if y == 91 || floor ≤ y then .ruinBrick
        else if x == cavernCheckpointColumn && y + 1 == floor then .torch
        else .air
      edits := edits.push (⟨x, y⟩, tile)
  for y in [87:cavernArenaFloor + 2] do
    edits := edits.push (⟨cavernArenaStart - 1, y⟩, .ruinBrick)
  for x in [cavernArenaStart:cavernArenaEnd + 1] do
    let ceiling := cavernArenaCeiling x
    for y in [ceiling - 1:cavernArenaFloor + 2] do
      let position : TilePos := ⟨x, y⟩
      let tile :=
        if position == cavernAltar then .cavernAltar
        else if y + 1 == ceiling then (if x % 7 == 0 then .crystal else .ruinBrick)
        else if cavernArenaFloor ≤ y then .ruinBrick
        else if y + 1 == cavernArenaFloor && (x == 455 || x == 466 || x == 478 || x == 488) then .torch
        else .air
      edits := edits.push (position, tile)
  return { entrance, caps, altar := cavernAltar, edits, footprint := editFootprint edits }

def generatePreMagmaTerrain (seed : UInt64) : World :=
  (highlandCavern seed).edits.foldl
    (fun world (position, tile) => world.setTile position tile)
    (generatePreCavernTerrain seed)

def magmaEntranceCaps : Array TilePos := #[⟨449, 98⟩, ⟨449, 99⟩]
def magmaWaterGate : TilePos := ⟨429, 106⟩
def magmaLedgeColumn : Nat := 432
def magmaLedgeFloor : Nat := 109
def magmaMarker : TilePos := ⟨432, 107⟩

private def magmaApproachFloor (x : Nat) : Nat :=
  if x ≤ magmaLedgeColumn + 1 then magmaLedgeFloor
  else cavernArenaFloor + min (magmaLedgeFloor - cavernArenaFloor) ((449 - x) / 2)

def inMagmaGallery (position : TilePos) : Bool :=
  let x := position.x
  let y := position.y
  if 400 ≤ x && x ≤ 430 then 96 ≤ y && y ≤ 116
  else if 431 ≤ x && x ≤ 448 then
    let floor := magmaApproachFloor x.toNat
    floor - 7 ≤ y && y ≤ floor + 1
  else magmaEntranceCaps.contains position

/-- Terrain and finite liquid supplies installed together west of Deepstone. -/
structure MagmaGallery where
  edits : Array (TilePos × Tile)
  footprint : Array TilePos
  water : Array (TilePos × Nat)
  magma : Array (TilePos × Nat)
  deriving BEq, Repr, Inhabited

private def magmaGalleryTile (x y : Nat) : Tile :=
  let position : TilePos := ⟨x, y⟩
  let floor := magmaApproachFloor x
  if x == 400 then .ruinBrick
  else if position == magmaMarker then .magmaMarker
  else if 424 ≤ x && x ≤ 430 && 101 ≤ y && y ≤ 106 then
    if y == 101 || y == 106 || x == 424 || x == 430 then
      if position == magmaWaterGate then .stone else .ruinBrick
    else .air
  else if 416 ≤ x && x ≤ 429 && magmaLedgeFloor ≤ y && y < 115 then
    if y == magmaLedgeFloor then .platform else .air
  else if (if x ≤ 430 then y == 96 else y + 7 == floor) || floor ≤ y then .ruinBrick
  else if (y + 1 == floor && (x == 405 || x == 415 || x == 430 || x == 433 ||
    x == 439 || x == 447)) || (x == 423 && y == 103) then .torch
  else .air

private def magmaGalleryEdits : Array (TilePos × Tile) := Id.run do
  let mut edits := magmaEntranceCaps.map (fun position => (position, Tile.stone))
  for x in [400:449] do
    let floor := magmaApproachFloor x
    let top := if x ≤ 430 then 96 else floor - 7
    let bottom := if x ≤ 430 then 117 else floor + 2
    for y in [top:bottom] do
      edits := edits.push (⟨x, y⟩, magmaGalleryTile x y)
  return edits

private def magmaGalleryFootprint : Array TilePos := editFootprint magmaGalleryEdits

private def liquidSupply (left right top bottom surface : Nat) : Array (TilePos × Nat) := Id.run do
  let mut cells := #[]
  for x in [left:right + 1] do
    for y in [top:bottom + 1] do
      cells := cells.push (⟨x, y⟩, if y == top then surface else 8)
  return cells

def magmaGallery (seed : UInt64) : MagmaGallery :=
  let waterTop := 4 + (terrainWord seed 429 41).toNat % 5
  let magmaTop := 6 + (terrainWord seed 429 43).toNat % 3
  { edits := magmaGalleryEdits, footprint := magmaGalleryFootprint
    water := liquidSupply 425 429 102 105 waterTop
    magma := liquidSupply 416 429 110 114 magmaTop }

def generatePreClassicTerrain (seed : UInt64) : World :=
  magmaGalleryEdits.foldl
    (fun world (position, tile) => world.setTile position tile)
    (generatePreMagmaTerrain seed)

private def crystalAlong (seed : UInt64) (world : World) (left right : Nat)
    (floorAt : Nat → Nat) : Option TilePos := Id.run do
  let width := right - left + 1
  let start := (terrainWord seed left 47).toNat % width
  for offset in [:width] do
    let x := left + (start + offset) % width
    let floor := floorAt x
    let position : TilePos := ⟨x, floor - 1⟩
    if world.tile position == .air && (world.tile ⟨x, floor⟩).solid &&
        !(world.tile ⟨x, floor - 2⟩).solid && !world.clearSkyAbove position then
      return some position
  return none

private def classicCrystalPositions (seed : UInt64) (world : World) : Array TilePos := Id.run do
  let veins := [
    (46, 49, caveFloor), (51, 53, caveFloor), (55, 58, caveFloor),
    (69, 71, caveFloor), (73, 75, caveFloor),
    (76, 78, frostFloor), (83, 85, frostFloor), (89, 91, frostFloor),
    (119, 122, frostFloor), (126, 128, frostFloor), (131, 133, frostFloor),
    (135, 139, fun _ => wardenArenaFloor),
    (169, 172, quarryHighFloor), (202, 205, quarryHighFloor),
    (259, 262, quarryHighFloor), (289, 291, quarryHighFloor),
    (307, 310, grottoFloor), (317, 320, grottoFloor), (333, 335, grottoFloor),
    (368, 371, fun _ => ruinShaftFloor),
    (488, 490, fun _ => deepsteelFloor seed), (493, 495, cavernApproachFloor)]
  let mut positions := #[]
  for (left, right, floorAt) in veins do
    if let some position := crystalAlong seed world left right floorAt then
      positions := positions.push position
  return positions

private def classicCobwebs (seed : UInt64) (world : World) : Array TilePos := Id.run do
  let clusters := [(55, 59, caveFloor), (67, 71, caveFloor), (73, 75, caveFloor),
    (76, 80, frostFloor), (81, 85, frostFloor), (86, 90, frostFloor), (94, 98, frostFloor)]
  let mut positions := #[]
  for (left, right, floorAt) in clusters do
    for x in [left:right + 1] do
      let floor := floorAt x
      let drape := (terrainWord seed x 49).toNat % 3
      for y in [floor - 6 + drape:floor - 4 + drape] do
        let position : TilePos := ⟨x, y⟩
        if world.tile position == .air && !world.clearSkyAbove position then
          positions := positions.push position
  return positions

private def classicIronPositions (seed : UInt64) (world : World) : Array TilePos := Id.run do
  let pocket := (supplyPockets seed)[1]!
  let mut standing : Array TilePos := #[]
  for x in [pocket.cache.x.toNat - 1:pocket.mouth.x.toNat - 1] do
    for y in [pocket.mouth.y.toNat + 2:pocket.mouth.y.toNat + 8] do
      let position : TilePos := ⟨x, y⟩
      if !(world.tile position).solid && !(world.tile ⟨x, y - 1⟩).solid &&
          (world.tile ⟨x, y + 1⟩).solid then
        standing := standing.push position
  -- Expose ore in the pocket's stone vault and rear wall. The second layer below
  -- the chamber floor also carries ore, leaving the walking surface intact.
  let entrySeam : TilePos := ⟨pocket.mouth.x - 1, pocket.mouth.y + 5⟩
  return pocket.edits.filterMap fun (position, tile) =>
    if tile == .stone && world.tile position == .stone && position.y > pocket.mouth.y &&
        (position.x < pocket.mouth.x - 1 || position == entrySeam) &&
        !standing.any (fun ledge => ledge.x == position.x && ledge.y + 1 == position.y) &&
        standing.any (fun ledge =>
          let distance := (ledge.x - position.x).natAbs + (ledge.y - position.y).natAbs
          distance ≤ (if position.y < ledge.y then 7 else 5)) then
      some position
    else none

private def classicTimberPositions (seed : UInt64) (world : World) : Array TilePos := Id.run do
  let mut positions := #[]
  -- Preserve the open garden plot at x8 after the lower trunk is harvested.
  for x in [6, 7, 9, 10, 11, 12, 13, 14] do
    let ground := groundHeight seed x
    let crown := if x == 7 || x == 13 then 3 else 4
    -- Leave the existing build shelf under the eastern canopy clear.
    let base := if x < 10 then 5 else 6
    for height in [base:base + crown] do
      let position : TilePos := ⟨x, ground - height⟩
      if world.tile position == .air then positions := positions.push position
  return positions

private def classicResourcesIn (seed : UInt64) (world : World) : Array (TilePos × Tile) :=
  (classicCrystalPositions seed world).map (·, .lifeCrystal) ++
    (classicCobwebs seed world).map (·, .cobweb) ++
    (classicIronPositions seed world).map (·, .iron) ++
    (classicTimberPositions seed world).map (·, .timber)

/-- Additional natural resources. Iron replaces stone and the other resources replace air. -/
def classicResources (seed : UInt64) : Array (TilePos × Tile) :=
  classicResourcesIn seed (generatePreClassicTerrain seed)

def lifeCrystalSites (seed : UInt64) : Array LifeCrystalSite :=
  originalLifeCrystalSites seed ++
    (classicCrystalPositions seed (generatePreClassicTerrain seed)).map lifeCrystalSite

/-- Frozen v24 terrain, including its pre-placed bench, classic ore and timber. -/
def generatePreEarlyTerrain (seed : UInt64) : World :=
  let previous := generatePreClassicTerrain seed
  (classicResourcesIn seed previous).foldl
    (fun world (position, tile) => world.setTile position tile) previous

structure EarlyOreSet where
  copper : Tile
  iron : Tile
  silver : Tile
  gold : Tile
  deriving BEq, Repr, Inhabited

/-- Regular worlds select one member of each of the four early ore pairs. -/
def earlyOres (seed : UInt64) : EarlyOreSet :=
  let choose (tier : Nat) (first second : Tile) :=
    let draw := terrainWord seed tier 53 ^^^ terrainWord defaultSeed tier 53
    if (draw &&& 1) == 0 then first else second
  { copper := choose 1 .copper .tinOre
    iron := choose 2 .iron .leadOre
    silver := choose 3 .silver .tungstenOre
    gold := choose 4 .goldOre .platinumOre }

def starterTreeColumns : Array Nat := #[6, 8, 10, 12, 14]

def starterTreePositions (seed : UInt64) : Array TilePos := Id.run do
  let mut positions := #[]
  for x in starterTreeColumns do
    let ground := groundHeight seed x
    let height := 10 + (terrainWord seed x 59 &&& 1).toNat
    for y in [ground - height:ground] do
      positions := positions.push ⟨x, y⟩
  return positions

private def earlyOreEdits (seed : UInt64) (world : World) (ores : EarlyOreSet) :
    Array (TilePos × Tile) := Id.run do
  let mut cells := #[]
  -- Lower-surface ore beside the spawn shelf, beneath its walking surface.
  for x in [17:25] do
    for y in [41:45] do
      if 17 < x && x < 24 || 41 < y && y < 44 then
        cells := cells.push (⟨x, y⟩, ores.copper)
  let firstIron := 44 + (terrainWord seed 44 61).toNat % 4
  for (left, right) in [(firstIron, 51), (53, 62)] do
    for x in [left:right + 1] do
      cells := cells.push (⟨x, caveFloor x + 1⟩, ores.iron)
  for x in [64:73] do
    cells := cells.push (⟨x, caveFloor x + 1⟩, ores.silver)
  for (left, right) in [(77, 89), (104, 113)] do
    for x in [left:right + 1] do
      cells := cells.push (⟨x, frostFloor x + 1⟩, ores.silver)
  for x in [76:84] do
    for depth in [1:3] do
      cells := cells.push (⟨x, frostCeiling x - depth⟩, ores.silver)
  -- The deepest Frost terraces expose both a lower seam and an overhead vein.
  for x in [119:140] do
    let floor := if x ≤ frostCavernEnd then frostFloor x else wardenArenaFloor
    cells := cells.push (⟨x, floor + 1⟩, ores.gold)
  for x in [124:134] do
    for depth in [1:3] do
      cells := cells.push (⟨x, frostCeiling x - depth⟩, ores.gold)
  return cells.filter fun (position, _) =>
    match world.tile position with
    | .dirt | .stone | .ice => true
    | _ => false

/-- New ore deposits in previously unmineralized terrain. -/
def earlyResources (seed : UInt64) : Array (TilePos × Tile) :=
  earlyOreEdits seed (generatePreEarlyTerrain seed) (earlyOres seed)

private def paintEarlyTerrain (seed : UInt64) (world : World) (ores : EarlyOreSet) : World :=
  let tiles := world.tiles.data.mapIdx fun index code =>
    match Tile.ofCode? code with
    | some .copper => ores.copper.code
    | some .iron => ores.iron.code
    | some .silver => ores.silver.code
    | some .timber =>
      let x := index % world.width
      let y := index / world.width
      if (homeGroveStart ≤ x && x ≤ 14) || highlandStart ≤ x then Tile.air.code
      else if 80 ≤ x && y < groundHeight seed x then Tile.treeTrunk.code
      else code
    | _ => code
  { world with tiles := ⟨tiles⟩ }

private def plantEarlyTrees (seed : UInt64) (world : World) : World := Id.run do
  let mut world := world
  for position in starterTreePositions seed do
    if world.tile position == .air then world := world.setTile position .treeTrunk
  for x in highlandCedarColumns seed do
    let top := highlandCedarCrownFloor seed x - 12
    for y in [top:groundHeight seed x] do
      let position : TilePos := ⟨x, y⟩
      if world.tile position == .air then world := world.setTile position .treeTrunk
  return world

/-- The exact v26 terrain, before natural altars. -/
def generatePreEyeTerrain (seed : UInt64) : World :=
  let previous := generatePreEarlyTerrain seed
  let ores := earlyOres seed
  let world := (paintEarlyTerrain seed previous ores).setTile ⟨16, 39⟩ .air
  let world := (earlyOreEdits seed previous ores).foldl
    (fun terrain (position, tile) => terrain.setTile position tile) world
  plantEarlyTrees seed world

def altarTile (seed : UInt64) : Tile :=
  if ((terrainWord seed 0 67 ^^^ terrainWord defaultSeed 0 67) &&& 1) == 0 then
    .demonAltar else .crimsonAltar

/-- The anchor is the lower-left cell of the three-wide, two-high altar. -/
def altarCells (anchor : TilePos) : Array TilePos :=
  #[anchor, ⟨anchor.x + 1, anchor.y⟩, ⟨anchor.x + 2, anchor.y⟩,
    ⟨anchor.x, anchor.y - 1⟩, ⟨anchor.x + 1, anchor.y - 1⟩, ⟨anchor.x + 2, anchor.y - 1⟩]

private def altarSupport (anchor : TilePos) : Array TilePos :=
  #[⟨anchor.x, anchor.y + 1⟩, ⟨anchor.x + 1, anchor.y + 1⟩, ⟨anchor.x + 2, anchor.y + 1⟩]

private def findEyeAltar (seed : UInt64) (world : World) (left right : Nat) (floor : Nat → Nat) :
    Option TilePos := Id.run do
  let width := right - left + 1
  let start := (terrainWord seed left 67).toNat % width
  for offset in [:width] do
    let x := left + (start + offset) % width
    let anchor : TilePos := ⟨x, floor x - 1⟩
    if (altarCells anchor).all (fun cell => world.tile cell == .air) &&
        (altarSupport anchor).all (fun cell => (world.tile cell).solid) &&
        !world.clearSkyAbove anchor then return some anchor
  return none

private def eyeAltarsIn (seed : UInt64) (world : World) : Array TilePos :=
  #[(44, 61, caveFloor), (104, 114, frostFloor), (172, 187, quarryHighFloor)].filterMap
    (fun (left, right, floor) => findEyeAltar seed world left right floor)

def eyeAltars (seed : UInt64) : Array TilePos := eyeAltarsIn seed (generatePreEyeTerrain seed)

private def World.placeEyeAltar (world : World) (anchor : TilePos) (tile : Tile) : World :=
  (altarCells anchor).foldl (fun terrain cell => terrain.setTile cell tile) world

/-- Add altars only where the old corridor, support and player edits allow the full footprint. -/
def retrofitEyeAltars (seed : UInt64) (world : World) (allowed : TilePos → Bool) : World := Id.run do
  let original := generatePreEyeTerrain seed
  let mut updated := world
  for anchor in eyeAltarsIn seed original do
    let footprint := altarCells anchor ++ altarSupport anchor
    if footprint.all (fun cell => world.tile cell == original.tile cell && allowed cell) then
      updated := updated.placeEyeAltar anchor (altarTile seed)
  return updated

/-- The exact v27 terrain, including its three original altar sites. -/
def generatePreEvilTerrain (seed : UInt64) : World :=
  let original := generatePreEyeTerrain seed
  (eyeAltarsIn seed original).foldl (fun world anchor => world.placeEyeAltar anchor (altarTile seed)) original

structure EvilLayout where
  left : Nat
  right : Nat
  entrance : TilePos
  floor : Nat
  relics : Array TilePos
  altars : Array TilePos
  pots : Array TilePos
  deriving BEq, Repr, Inhabited

private def evilLeft : Nat := 103
private def evilRight : Nat := 166
private def evilBottom : Nat := 48

private def evilCorrupt (seed : UInt64) : Bool := altarTile seed == .demonAltar

private def evilOffset (seed : UInt64) : Nat := (terrainWord seed evilLeft 83).toNat % 3

private def evilWestRimHeight (seed : UInt64) : Nat :=
  let iceThickness := 2 + noise (seed + 4) evilLeft 0 5 1 / 100
  min (groundHeight seed evilLeft) (frostCeiling evilLeft - iceThickness)

def evilSurfaceHeight (seed : UInt64) (x : Nat) : Nat :=
  let crest := 28 + (terrainWord seed evilLeft 84).toNat % 3
  if x < evilLeft || evilRight < x then groundHeight seed x
  else if x < 116 then blend (evilWestRimHeight seed) crest (x - evilLeft) (116 - evilLeft)
  else if x ≤ 154 then crest
  else blend crest (groundHeight seed evilRight) (x - 154) (evilRight - 154)

def inEvilBiome (seed : UInt64) (position : TilePos) : Bool :=
  let surface := evilSurfaceHeight seed position.x.toNat
  (evilLeft : Int) ≤ position.x && position.x ≤ evilRight &&
    (surface : Int) - 12 ≤ position.y && position.y ≤ evilBottom

def evilRelicTile (seed : UInt64) : Tile :=
  if evilCorrupt seed then .shadowOrb else .crimsonHeart

/-- Relics and pots share a two-wide, two-high footprint anchored at its top left. -/
def evilRelicCells (anchor : TilePos) : Array TilePos :=
  #[anchor, ⟨anchor.x + 1, anchor.y⟩, ⟨anchor.x, anchor.y + 1⟩, ⟨anchor.x + 1, anchor.y + 1⟩]

def evilPotCells (anchor : TilePos) : Array TilePos := evilRelicCells anchor

private def corruptShafts (seed : UInt64) : Array Nat :=
  let offset := evilOffset seed
  #[120 + offset, 135 + offset, 150 + offset]

private def inCorruptShaft (seed : UInt64) (x : Nat) : Bool :=
  (corruptShafts seed).any fun center => center - 2 ≤ x && x ≤ center + 2

def evilRelics (seed : UInt64) : Array TilePos :=
  let offset := evilOffset seed
  if evilCorrupt seed then (corruptShafts seed).map fun (x : Nat) => ⟨x + 1, 45⟩
  else #[⟨117 + offset, 45⟩, ⟨154 + offset, 36⟩, ⟨155 + offset, 45⟩]

def evilAltars (seed : UInt64) : Array TilePos :=
  let offset := evilOffset seed
  if evilCorrupt seed then #[⟨126 + offset, 41⟩, ⟨142 + offset, 41⟩]
  else #[⟨132 + offset, 45⟩, ⟨148 + offset, 46⟩]

private def potOverlap (position other : TilePos) : Bool :=
  (position.x - other.x).natAbs < 2 && (position.y - other.y).natAbs ≤ 2

def evilPots (seed : UInt64) : Array TilePos := Id.run do
  let offset := evilOffset seed
  let corrupt := evilCorrupt seed
  let floor : Int := if corrupt then 42 else 46
  let mut reachable : Array TilePos := #[]
  let mut ridge : Array TilePos := #[]
  let westLip := (if corrupt then 117 else 115) + offset
  for x in [(if corrupt then 111 else 105):155] do
    let left := evilSurfaceHeight seed x
    let right := evilSurfaceHeight seed (x + 1)
    if x < westLip && left == right then
      reachable := reachable.push ⟨x, left - 2⟩
    else if left == right &&
        (if corrupt then !inCorruptShaft seed x && !inCorruptShaft seed (x + 1)
          else 130 + offset < x) then
      ridge := ridge.push ⟨x, left - 2⟩
  for x in (if corrupt then List.range' 118 39 else List.range' (132 + offset) 8) do
    let position : TilePos := ⟨x, floor - 2⟩
    if !(evilAltars seed).any (fun altar => (altarCells altar).any (evilPotCells position).contains) then
      reachable := reachable.push position
  unless corrupt do
    for x in [143 + offset:147 + offset] do
      reachable := reachable.push ⟨x, 45⟩
  let mut pots := #[]
  -- Prefer the normal loot roll for the two guaranteed starter bomb pots.
  let bombs := reachable.filter (fun position => (hash (seed, position, (18 : Nat))).toNat % 7 == 4)
  for (candidates, limit) in [(bombs ++ reachable, 2), (reachable ++ ridge, 6)] do
    for position in candidates do
      if pots.size == limit then break
      if !pots.any (potOverlap position) then
        pots := pots.push position
  return pots

def evilPotHasBombs (seed : UInt64) (anchor : TilePos) : Bool :=
  ((evilPots seed).findIdx? (· == anchor)).any fun index =>
    index < 2 || (hash (seed, anchor, (18 : Nat))).toNat % 7 == 4

def evilLayout (seed : UInt64) : EvilLayout :=
  let corrupt := evilCorrupt seed
  let entrance := (if corrupt then 120 else 116) + evilOffset seed
  { left := evilLeft, right := evilRight, entrance := ⟨entrance, evilSurfaceHeight seed entrance - 1⟩
    floor := if corrupt then 42 else 46
    relics := evilRelics seed, altars := evilAltars seed, pots := evilPots seed }

private def evilTunnel (x y left right ceiling floor : Nat) : Bool :=
  left ≤ x && x ≤ right && ceiling ≤ y && y < floor

private def corruptCavities (seed : UInt64) (x y : Nat) : Bool :=
  let exitFloor := blend (evilWestRimHeight seed) 42 (min (x - evilLeft) 8) 8
  -- The original straight shafts feed into a common gallery. A sloping western
  -- adit gives the player a way back before they own a hook or platforms.
  evilTunnel x y evilLeft 120 (exitFloor - 4) exitFloor ||
  (inCorruptShaft seed x && y < 42) ||
  evilTunnel x y 113 158 (37 - archRise x 113 159 2) 42 ||
  (evilRelics seed).any (fun relic =>
    evilTunnel x y (relic.x.toNat - 1) (relic.x.toNat + 2) 43 47)

private def crimsonCavities (seed : UInt64) (x y : Nat) : Bool :=
  let offset := evilOffset seed
  let column := x - offset
  let surface := evilSurfaceHeight seed (116 + offset)
  let throatFloor := blend surface 44 (min (column - 116) 14) 14
  let throatRoof := if column < 119 then 0 else throatFloor - 5
  let upperFloor := 42 - min 4 ((column - 143) / 2)
  if (116 ≤ column && column ≤ 130 && y == throatFloor) ||
      (column == 142 && y == 44) || (143 ≤ column && column ≤ 146 && y == upperFloor) then false
  else
    evilTunnel column y 116 130 throatRoof throatFloor ||
    evilTunnel column y 131 146 (39 - archRise column 127 148 7) (if column == 131 || 141 ≤ column then 47 else 46) ||
    -- Three fingers branch from the rounded cavity, each behind one Crimstone seal.
    (column != 121 && evilTunnel column y 115 130 (max 43 (throatFloor + 1)) 47) ||
    (column != 153 && evilTunnel column y 141 158 (upperFloor - 4) upperFloor) ||
    (column != 154 && evilTunnel column y 143 159 43 47)

private def evilEdits (seed : UInt64) (original : World) : Array (TilePos × Tile) := Id.run do
  let layout := evilLayout seed
  let corrupt := evilCorrupt seed
  let rock := if corrupt then Tile.ebonstone else .crimstone
  let grass := if corrupt then Tile.corruptGrass else .crimsonGrass
  let relics := layout.relics.flatMap evilRelicCells
  let pots := layout.pots.flatMap evilPotCells
  let altars := layout.altars.flatMap altarCells
  let mut edits := #[]
  for x in [layout.left:layout.right + 1] do
    let surface := evilSurfaceHeight seed x
    let bottom := if x ≤ frostCavernEnd then min evilBottom (frostCeiling x - 2) else evilBottom
    for y in [surface - 3:bottom + 1] do
      let position : TilePos := ⟨x, y⟩
      let old := original.tile position
      let cavity := if corrupt then corruptCavities seed x y else crimsonCavities seed x y
      let tile := if relics.contains position then evilRelicTile seed
        else if pots.contains position then .clayPot
        else if altars.contains position then altarTile seed
        else if cavity then .air
        else if y == surface then grass
        else if surface < y then rock
        else old
      if tile != old then edits := edits.push (position, tile)
  return edits

/-- Install the entire region only where its old terrain and structural margin
are untouched. The caller can protect actors, liquids and player-built walls. -/
def retrofitEvilTerrain (seed : UInt64) (world : World) (allowed : TilePos → Bool) : World :=
  let original := generatePreEvilTerrain seed
  let edits := evilEdits seed original
  let pristine := fun (position : TilePos) =>
    world.tile? position == original.tile? position && allowed position
  if edits.all (fun (position, _) =>
    [-1, 0, 1].all fun dx => [-1, 0, 1].all fun dy =>
      pristine ⟨position.x + dx, position.y + dy⟩) then
    edits.foldl (fun terrain (position, tile) => terrain.setTile position tile) world
  else world

/-- The v28 generated world, before mushrooms and the sandy pond floor. -/
def generatePreEvilBossTerrain (seed : UInt64) : World :=
  let original := generatePreEvilTerrain seed
  (evilEdits seed original).foldl (fun world (position, tile) => world.setTile position tile) original

def Tile.evilMushroom : Tile → Option Tile
  | .corruptGrass => some .vileMushroom
  | .crimsonGrass => some .viciousMushroom
  | _ => none

def Tile.bottleSupport : Tile → Bool
  | .platform | .workbench | .woodTable => true
  | _ => false

private def evilBossResourceEdits (seed : UInt64) (original : World) : Array (TilePos × Tile) := Id.run do
  let mut edits := #[]
  let floor := surfacePondTop seed + 3
  for column in [2:6] do
    for row in [floor:floor + 2] do
      let position : TilePos := ⟨column, row⟩
      if original.tile position == .dirt || original.tile position == .stone then
        edits := edits.push (position, .sand)
  for column in [evilLeft + 3:evilRight] do
    if column % 5 != (terrainWord seed evilLeft 91).toNat % 5 then continue
    let grass : TilePos := ⟨column, evilSurfaceHeight seed column⟩
    let position : TilePos := ⟨grass.x, grass.y - 1⟩
    if original.tile position == .air then
      if let some mushroom := (original.tile grass).evilMushroom then
        edits := edits.push (position, mushroom)
  return edits

def retrofitEvilBossResources (seed : UInt64) (world : World) (allowed : TilePos → Bool) : World :=
  let original := generatePreEvilBossTerrain seed
  (evilBossResourceEdits seed original).foldl (fun terrain (position, tile) =>
    let supported := if tile == .sand then true else
      (terrain.tile ⟨position.x, position.y + 1⟩).evilMushroom == some tile
    if supported && terrain.tile position == original.tile position && allowed position then
      terrain.setTile position tile
    else terrain) world

/-- The exact v30 terrain before the Dungeon. -/
def generatePreDungeonTerrain (seed : UInt64) : World :=
  let original := generatePreEvilBossTerrain seed
  (evilBossResourceEdits seed original).foldl (fun world (position, tile) => world.setTile position tile) original

structure DungeonLayout where
  left : Nat
  right : Nat
  surface : Nat
  entrance : TilePos
  oldMan : TilePos
  keyChest : TilePos
  goldChest : TilePos
  lowerGoldChest : TilePos
  marker : TilePos
  shaft : TilePos
  lowerShaft : TilePos
  brick : Tile
  blueMoonChest : TilePos
  blueMoonEntrance : TilePos
  magicMissileChest : TilePos
  magicMissileEntrance : TilePos
  magicMissileShaft : TilePos
  valorChest : TilePos
  deepKeyChest : TilePos
  handgunChest : TilePos
  deepEntrance : TilePos
  deriving BEq, Repr, Inhabited

private def dungeonShaftColumn : Nat := 84
private def dungeonHallFloor : Nat := 82
private def dungeonVaultFloor : Nat := 88
private def dungeonLowerShaftColumn : Nat := 94
def dungeonLowerFloor : Nat := 116
def dungeonBlueMoonLeft : Nat := 15
def dungeonBlueMoonFloor : Nat := 122
private def dungeonBlueMoonDoorColumn : Nat := 49
def dungeonMagicMissileLeft : Nat := 5
private def dungeonMagicMissileRight : Nat := 44
def dungeonMagicMissileFloor : Nat := 106
def dungeonMagicMissileShaftFloor : Nat := 124
private def dungeonMagicMissileShaftColumn : Nat := 8
private def dungeonMagicMissilePassageRoof : Nat := 118
def dungeonDeepFloor : Nat := 100
private def dungeonDeepRight : Nat := 92

private def dungeonNaturalSurface (seed : UInt64) (x : Nat) : Nat :=
  let iceThickness := 2 + noise (seed + 4) x 0 5 1 / 100
  min (groundHeight seed x) (frostCeiling x - iceThickness)

def dungeonLayout (seed : UInt64) : DungeonLayout :=
  let surface := min 41 (max 34 (dungeonNaturalSurface seed 90))
  let brick := match (terrainWord seed 84 97).toNat % 3 with
    | 0 => Tile.blueDungeonBrick
    | 1 => Tile.greenDungeonBrick
    | _ => Tile.pinkDungeonBrick
  let keyColumn := 87 + (terrainWord seed 87 101).toNat % 3
  let goldColumn := 53 + (terrainWord seed 53 103).toNat % 3
  let lowerGoldColumn := 55 + (terrainWord seed 55 107).toNat % 3
  let blueMoonColumn := 22 + (terrainWord seed 22 109).toNat % 3
  let magicMissileColumn := 36 + (terrainWord seed 36 113).toNat % 3
  let valorColumn := 58 + (terrainWord seed 58 127).toNat % 3
  let deepKeyColumn := 70 + (terrainWord seed 70 131).toNat % 3
  let handgunColumn := 84 + (terrainWord seed 84 137).toNat % 3
  { left := 47, right := 102, surface, entrance := ⟨94, surface - 1⟩
    oldMan := ⟨98, surface - 1⟩, keyChest := ⟨keyColumn, surface - 2⟩
    goldChest := ⟨goldColumn, dungeonVaultFloor - 2⟩
    lowerGoldChest := ⟨lowerGoldColumn, dungeonLowerFloor - 2⟩
    marker := ⟨81, surface - 10⟩, shaft := ⟨dungeonShaftColumn, surface⟩
    lowerShaft := ⟨dungeonLowerShaftColumn, dungeonHallFloor⟩, brick
    blueMoonChest := ⟨blueMoonColumn, dungeonBlueMoonFloor - 2⟩
    blueMoonEntrance := ⟨dungeonBlueMoonDoorColumn, dungeonLowerFloor - 1⟩
    magicMissileChest := ⟨magicMissileColumn, dungeonMagicMissileFloor - 2⟩
    magicMissileEntrance := ⟨dungeonBlueMoonLeft, dungeonBlueMoonFloor - 1⟩
    magicMissileShaft := ⟨dungeonMagicMissileShaftColumn, dungeonMagicMissileFloor⟩
    valorChest := ⟨valorColumn, dungeonDeepFloor - 2⟩
    deepKeyChest := ⟨deepKeyColumn, dungeonDeepFloor - 2⟩
    handgunChest := ⟨handgunColumn, dungeonDeepFloor - 2⟩
    deepEntrance := ⟨dungeonMagicMissileRight, dungeonMagicMissileFloor - 1⟩ }

/-- Chests are anchored at the top-left cell of a two-wide, two-high footprint. -/
def dungeonChestCells (anchor : TilePos) : Array TilePos := evilRelicCells anchor

def dungeonChestAnchor (seed : UInt64) (position : TilePos) (lowerDungeon := false)
    (blueMoonDungeon := false) (magicMissileDungeon := false) (deepDungeon := false) : Option TilePos :=
  let layout := dungeonLayout seed
  let anchors := #[layout.keyChest, layout.goldChest]
  let anchors := if lowerDungeon then anchors.push layout.lowerGoldChest else anchors
  let anchors := if blueMoonDungeon then anchors.push layout.blueMoonChest else anchors
  let anchors := if magicMissileDungeon then anchors.push layout.magicMissileChest else anchors
  let anchors := if deepDungeon then anchors ++ #[layout.valorChest, layout.deepKeyChest, layout.handgunChest]
    else anchors
  anchors.find? (fun anchor => (dungeonChestCells anchor).contains position)

private def dungeonFrontageFloor (seed : UInt64) (surface x : Nat) : Nat :=
  if x < 83 then blend (dungeonNaturalSurface seed 76) surface (x - 76) 7
  else if x ≤ 99 then surface
  else blend surface (dungeonNaturalSurface seed 102) (x - 99) 3

def dungeonSurfaceHeight (seed : UInt64) (x : Nat) : Nat :=
  if 77 ≤ x && x ≤ 102 then dungeonFrontageFloor seed (dungeonLayout seed).surface x
  else groundHeight seed x

private def dungeonChamberFloor (x : Nat) : Nat :=
  dungeonHallFloor + min (dungeonVaultFloor - dungeonHallFloor) (69 - x)

private def dungeonChamberRoof (x : Nat) : Nat :=
  let height := if x ≤ 63 then 7 + archRise x 47 64 2
    else if x ≤ 69 then 5
    else 7 + archRise x 69 101 2
  dungeonChamberFloor x - height

private def dungeonLowerRoof (x : Nat) : Nat :=
  if x ≤ 72 then 108 - archRise x 49 73 2
  else if x == 73 || x == 80 then 109
  else if x == 74 || x == 79 then 110
  else if x ≤ 78 then 112
  else 108 - archRise x 80 101 2

private def dungeonLowerUnsafeWall (x y : Nat) : Bool :=
  y < dungeonLowerFloor &&
    ((dungeonLowerShaftColumn ≤ x && x ≤ dungeonLowerShaftColumn + 1 && dungeonHallFloor ≤ y) ||
     (50 ≤ x && x ≤ 100 && dungeonLowerRoof x ≤ y))

private def dungeonBlueMoonFloorAt (x : Nat) : Nat :=
  dungeonLowerFloor + min (dungeonBlueMoonFloor - dungeonLowerFloor) ((dungeonBlueMoonDoorColumn - x) / 2)

private def dungeonBlueMoonRoof (x : Nat) : Nat :=
  if x ≤ 34 then 115 - archRise x dungeonBlueMoonLeft 37 3
  else if x ≤ 36 then 116
  else dungeonBlueMoonFloorAt x - 4

private def dungeonBlueMoonUnsafeWall (x y : Nat) : Bool :=
  dungeonBlueMoonLeft < x && x ≤ dungeonBlueMoonDoorColumn &&
    dungeonBlueMoonRoof x ≤ y && y < dungeonBlueMoonFloorAt x

private def dungeonMagicMissileRoof (x : Nat) : Nat :=
  98 - archRise x dungeonMagicMissileLeft dungeonMagicMissileRight 3

private def dungeonMagicMissilePassageFloor (x : Nat) : Nat :=
  dungeonBlueMoonFloor +
    min (dungeonMagicMissileShaftFloor - dungeonBlueMoonFloor) ((dungeonBlueMoonLeft - x) / 2)

private def dungeonMagicMissileUnsafeWall (x y : Nat) : Bool :=
  (dungeonMagicMissileLeft < x && x < dungeonMagicMissileRight &&
    dungeonMagicMissileRoof x ≤ y && y < dungeonMagicMissileFloor) ||
  (dungeonMagicMissileShaftColumn ≤ x && x ≤ dungeonMagicMissileShaftColumn + 1 &&
    dungeonMagicMissileFloor ≤ y && y < dungeonMagicMissileShaftFloor) ||
  (dungeonMagicMissileShaftColumn + 2 ≤ x && x ≤ dungeonBlueMoonLeft &&
    dungeonMagicMissilePassageRoof ≤ y && y < dungeonMagicMissilePassageFloor x)

private def dungeonDeepFloorAt (x : Nat) : Nat :=
  max dungeonDeepFloor (dungeonMagicMissileFloor - ((x - dungeonMagicMissileRight + 1) / 2))

private def dungeonDeepRoof (x : Nat) : Nat :=
  if x ≤ 56 then max 94 (dungeonDeepFloorAt x - 7)
  else if x == 66 || x == 80 then 95
  else 95 - archRise x 56 dungeonDeepRight 2

private def dungeonDeepUnsafeWall (x y : Nat) : Bool :=
  dungeonMagicMissileRight ≤ x && x < dungeonDeepRight &&
    dungeonDeepRoof x ≤ y && y < dungeonDeepFloorAt x

/-- The original unsafe background is represented by the seeded underground
rooms and shaft. The Frost crossing and the entrance vestibule are safe. -/
def dungeonUnsafeWall (_seed : UInt64) (position : TilePos) (lowerDungeon := false)
    (blueMoonDungeon := false) (magicMissileDungeon := false) (deepDungeon := false) : Bool :=
  let x := position.x.toNat
  let y := position.y.toNat
  (dungeonShaftColumn ≤ x && x ≤ dungeonShaftColumn + 1 && 57 ≤ y && y < dungeonHallFloor) ||
    (48 ≤ x && x ≤ 100 && dungeonChamberRoof x ≤ y && y < dungeonChamberFloor x) ||
    (lowerDungeon && dungeonLowerUnsafeWall x y) ||
    (blueMoonDungeon && dungeonBlueMoonUnsafeWall x y) ||
    (magicMissileDungeon && dungeonMagicMissileUnsafeWall x y) ||
    (deepDungeon && dungeonDeepUnsafeWall x y)

def dungeonSpawnFloorsAt (position : TilePos) (lowerDungeon := false)
    (blueMoonDungeon := false) (magicMissileDungeon := false) (deepDungeon := false) : Array Nat :=
  let x := position.x.toNat
  let y := position.y.toNat
  if deepDungeon && dungeonDeepUnsafeWall x y then
    (List.range' dungeonDeepFloor (dungeonMagicMissileFloor - dungeonDeepFloor + 1)).toArray
  else if magicMissileDungeon && dungeonMagicMissileUnsafeWall x y then
    if y < dungeonMagicMissilePassageRoof then #[dungeonMagicMissileFloor]
    else #[dungeonBlueMoonFloor, dungeonBlueMoonFloor + 1, dungeonMagicMissileShaftFloor]
  else if blueMoonDungeon && dungeonBlueMoonUnsafeWall x y then
    (List.range' dungeonLowerFloor (dungeonBlueMoonFloor - dungeonLowerFloor + 1)).toArray
  else if lowerDungeon && dungeonLowerUnsafeWall x y then #[dungeonLowerFloor]
  else (List.range' 76 13).toArray

/-- Grounded spawn candidates outside the viewport in the compact Dungeon.
The caller checks the proposed enemy's clearance and liquid rules. -/
def findDungeonSpawnFloor (world : World) (seed : UInt64) (center : TilePos)
    (unsafeWall : TilePos → Bool) (roll : Nat) (candidate : Int → Int → Option α)
    (lowerDungeon := false) (blueMoonDungeon := false)
    (magicMissileDungeon := false) (deepDungeon := false) : Option α := Id.run do
  let layout := dungeonLayout seed
  let x := center.x.toNat
  let y := center.y.toNat
  -- The stair links the two rooms, giving the center of the new vault
  -- off-screen spawn positions even when both of its own ends are visible.
  let left := if (deepDungeon && dungeonDeepUnsafeWall x y) ||
      (magicMissileDungeon && dungeonMagicMissileUnsafeWall x y)
    then dungeonMagicMissileLeft + 1
    else if blueMoonDungeon && dungeonBlueMoonUnsafeWall x y
    then dungeonBlueMoonLeft + 1 else layout.left + 2
  let span := layout.right - left - 1
  let floors := dungeonSpawnFloorsAt center lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon
  for offset in [:span] do
    let column : Int := left + (roll + offset) % span
    if (column - center.x).natAbs < 24 then continue
    for floor in floors do
      if !unsafeWall ⟨column, (floor : Int) - 1⟩ then continue
      let support := world.tile ⟨column, floor⟩
      if !support.isDungeonBrick then continue
      if let some enemy := candidate column floor then return some enemy
  return none

/-- Desktop's Dungeon census is a 169×124 area centered on the player's tile. -/
def inDungeon (seed : UInt64) (world : World) (position : TilePos) (lowerDungeon := false)
    (blueMoonDungeon := false) (magicMissileDungeon := false) (deepDungeon := false) : Bool := Id.run do
  if !world.contains position ||
      !dungeonUnsafeWall seed position lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon ||
      position.y < (dungeonLayout seed).surface + 2 then return false
  let left := position.x.toNat - 84
  let top := position.y.toNat - 62
  let right := min world.width (position.x.toNat + 85)
  let bottom := min world.height (position.y.toNat + 62)
  let mut count := 0
  for y in [top:bottom] do
    for x in [left:right] do
      if let some tile := world.tiles[y * world.width + x]? then
        if 69 ≤ tile && tile ≤ 71 then
          count := count + 1
          if 250 ≤ count then return true
  return false

private def dungeonFrontageTile (layout : DungeonLayout) (floor x y : Nat) : Tile :=
  let height := layout.surface - y
  if (dungeonChestCells layout.keyChest).contains ⟨x, y⟩ then .dungeonKeyChest
  else if y ≥ floor then
    if x == dungeonShaftColumn || x == dungeonShaftColumn + 1 then .platform
    else if 81 ≤ x && x ≤ 95 then layout.brick else .stone
  else if ((81 ≤ x && x ≤ 95) && height == 8) ||
      ((x == 81 || x == 84 || x == 91 || x == 95) && (height == 9 || height == 10)) ||
      ((x == 81 || x == 95) && 5 ≤ height && height < 8 && y + 3 < floor) then layout.brick
  else if y + 1 == floor && (x == 82 || x == 92 || x == 99) then .torch
  else .air

private def dungeonEdits (seed : UInt64) (original : World) : Array (TilePos × Tile) := Id.run do
  let layout := dungeonLayout seed
  let mut edits := #[]
  for x in [77:102] do
    let floor := dungeonFrontageFloor seed layout.surface x
    let oldFloor := dungeonNaturalSurface seed x
    for y in [min floor layout.surface - 10:max floor oldFloor + 1] do
      let position : TilePos := ⟨x, y⟩
      let tile := dungeonFrontageTile layout floor x y
      edits := edits.push (position, tile)
  for x in [layout.left:layout.right + 1] do
    let floor := dungeonChamberFloor x
    let roof := dungeonChamberRoof x
    for y in [roof - 2:floor + 4] do
      let position : TilePos := ⟨x, y⟩
      let tile := if y < roof || floor ≤ y || x == layout.left || 101 ≤ x then layout.brick
        else if (dungeonChestCells layout.goldChest).contains position then .dungeonGoldChest
        else if y + 1 == floor && (#[50, 61, 70, 78, 87, 96] : Array Nat).contains x then .torch
        else .air
      edits := edits.push (position, tile)
  -- Preserve the Frost supply pocket east of the shaft and its original
  -- route across the shaft on platforms. The exposed seam keeps its ore.
  for y in [57:dungeonChamberRoof (dungeonShaftColumn - 1)] do
    edits := edits.push (⟨dungeonShaftColumn - 1, y⟩, layout.brick)
  for y in [69:dungeonChamberRoof (dungeonShaftColumn + 2)] do
    edits := edits.push (⟨dungeonShaftColumn + 2, y⟩, layout.brick)
  for x in [dungeonShaftColumn:dungeonShaftColumn + 2] do
    let seam := original.tile ⟨x, frostFloor x + 1⟩
    if seam == .silver || seam == .tungstenOre then
      edits := edits.push (⟨dungeonShaftColumn - 1, 58 + (x - dungeonShaftColumn)⟩, seam)
    for y in [layout.surface:dungeonHallFloor] do
      let position : TilePos := ⟨x, y⟩
      let tile := match original.tile position with
        | .lifeCrystal => Tile.lifeCrystal
        | .cobweb =>
          if x == dungeonShaftColumn && y % 2 == 0 then .platform else .cobweb
        | _ =>
          if y == layout.surface || y % 2 == 0 then .platform
          else if x == dungeonShaftColumn && (y == 61 || y == 67 || y == 73) then .torch
          else .air
      edits := edits.push (position, tile)
  -- Keep the one web displaced by the continuous climbing side of the shaft
  -- beside the original Frost crossing, still within reach of the main path.
  let crossing := frostFloor dungeonShaftColumn
  if (List.range' (crossing - 6) 4).any (fun y =>
      y % 2 == 0 && original.tile ⟨dungeonShaftColumn, y⟩ == .cobweb) then
    edits := edits.push (⟨dungeonShaftColumn + 2, crossing - 1⟩, .cobweb)
  return edits

private def installDungeon (world : World) (edits : Array (TilePos × Tile)) : World :=
  edits.foldl (fun terrain (position, tile) => terrain.setTile position tile) world

/-- Install only on pristine terrain, including the structural margin. The
caller also checks for protected occupancy, liquids and constructed walls. -/
def retrofitDungeon (seed : UInt64) (world : World) (allowed : TilePos → Bool) : Option World := do
  let original := generatePreDungeonTerrain seed
  if world.width != original.width || world.height != original.height then none else
    let edits := dungeonEdits seed original
    let generated := installDungeon original edits
    let grass := if evilCorrupt seed then Tile.corruptGrass else .crimsonGrass
    let pristine := fun (position : TilePos) =>
      let before := original.tile? position
      let current := world.tile? position
      let after := generated.tile? position
      let unchanged := after == before
      -- Evil grass can spread onto a neighboring dirt tile while the player
      -- travels, and crystals beside the shaft may already have been mined.
      -- Preserve both wherever the Dungeon leaves the original tile alone.
      allowed position && (current == before || current == after ||
        (unchanged && ((before == some .dirt && current == some grass) ||
          (before == some .lifeCrystal && current == some .air))))
    if edits.all (fun (position, _) =>
      [-1, 0, 1].all fun dx => [-1, 0, 1].all fun dy =>
        pristine ⟨position.x + dx, position.y + dy⟩) then
      some (edits.foldl (fun terrain (position, _) =>
        let tile := generated.tile position
        if tile == original.tile position then terrain else terrain.setTile position tile) world)
    else none

/-- The exact v38 world with the entrance, shaft and original Dungeon hall. -/
def generatePreLowerDungeonTerrain (seed : UInt64) : World :=
  let original := generatePreDungeonTerrain seed
  installDungeon original (dungeonEdits seed original)

private def lowerDungeonEdits (seed : UInt64) : Array (TilePos × Tile) := Id.run do
  let layout := dungeonLayout seed
  let mut edits := #[]
  let chest := dungeonChestCells layout.lowerGoldChest
  for x in [49:102] do
    let roof := dungeonLowerRoof x
    for y in [roof - 2:dungeonLowerFloor + 3] do
      let position : TilePos := ⟨x, y⟩
      let tile := if y < roof || dungeonLowerFloor ≤ y || x == 49 || x == 101 then layout.brick
        else if chest.contains position then .dungeonGoldChest
        else if y + 1 == dungeonLowerFloor && (#[52, 63, 70, 83, 90, 98] : Array Nat).contains x then .torch
        else .air
      edits := edits.push (position, tile)
  let shaft := dungeonLowerShaftColumn
  -- Line the shaft as far as it enters the eastern room. Two tiles between
  -- platform rows let the player jump all the way back to the upper hall.
  for x in [shaft - 1:shaft + 3] do
    if x == shaft - 1 || x == shaft + 2 then
      for y in [dungeonHallFloor + 4:dungeonLowerRoof x] do
        edits := edits.push (⟨x, y⟩, layout.brick)
    else
      for y in [dungeonHallFloor:dungeonLowerFloor] do
        let tile := if y % 2 == 0 then Tile.platform
          else if x == shaft && (#[87, 95, 103] : Array Nat).contains y then .torch
          else .air
        edits := edits.push (⟨x, y⟩, tile)
  return edits

private def retrofitPristineDungeonExtension (original world : World)
    (edits : Array (TilePos × Tile)) (allowed : TilePos → Bool) : Option World := do
  if world.width != original.width || world.height != original.height then failure
  if !edits.all (fun (position, _) =>
    [-1, 0, 1].all fun dx => [-1, 0, 1].all fun dy =>
      let cell : TilePos := ⟨position.x + dx, position.y + dy⟩
      allowed cell && world.tile? cell == original.tile? cell) then failure
  return installDungeon world edits

/-- Add the lower wing to an unmodified v38 footprint, including the stone
around each excavation. The caller checks walls, liquids and occupants. -/
def retrofitLowerDungeon (seed : UInt64) (world : World) (allowed : TilePos → Bool) : Option World :=
  retrofitPristineDungeonExtension (generatePreLowerDungeonTerrain seed) world (lowerDungeonEdits seed) allowed

/-- The exact v42 terrain, including the first lower Dungeon wing. -/
def generatePreBlueMoonDungeonTerrain (seed : UInt64) : World :=
  installDungeon (generatePreLowerDungeonTerrain seed) (lowerDungeonEdits seed)

private def blueMoonDungeonEdits (seed : UInt64) : Array (TilePos × Tile) := Id.run do
  let layout := dungeonLayout seed
  let chest := dungeonChestCells layout.blueMoonChest
  let mut edits := #[]
  for x in [dungeonBlueMoonLeft:dungeonBlueMoonDoorColumn] do
    let roof := dungeonBlueMoonRoof x
    let floor := dungeonBlueMoonFloorAt x
    for y in [roof - 2:floor + 3] do
      let position : TilePos := ⟨x, y⟩
      let tile := if y < roof || floor ≤ y || x == dungeonBlueMoonLeft then layout.brick
        else if chest.contains position then .dungeonGoldChest
        else if y + 1 == floor && (#[17, 30, 38, 45] : Array Nat).contains x then .torch
        else .air
      edits := edits.push (position, tile)
  -- The old lower vault's western wall keeps its original floor and roof.
  for y in [dungeonBlueMoonRoof dungeonBlueMoonDoorColumn:dungeonLowerFloor] do
    edits := edits.push (⟨dungeonBlueMoonDoorColumn, y⟩, .air)
  return edits

/-- Extend a pristine v42 lower wing, including the stone immediately
around the new excavation. The caller protects occupied and authored cells. -/
def retrofitBlueMoonDungeon (seed : UInt64) (world : World)
    (allowed : TilePos → Bool) : Option World :=
  retrofitPristineDungeonExtension (generatePreBlueMoonDungeonTerrain seed) world (blueMoonDungeonEdits seed) allowed

/-- The exact v43 terrain before the Magic Missile Dungeon branch. -/
def generatePreMagicMissileDungeonTerrain (seed : UInt64) : World :=
  installDungeon (generatePreBlueMoonDungeonTerrain seed) (blueMoonDungeonEdits seed)

private def magicMissileDungeonEdits (seed : UInt64) : Array (TilePos × Tile) := Id.run do
  let layout := dungeonLayout seed
  let chest := dungeonChestCells layout.magicMissileChest
  let mut edits := #[]
  for x in [dungeonMagicMissileLeft:dungeonMagicMissileRight + 1] do
    let roof := dungeonMagicMissileRoof x
    for y in [roof - 2:dungeonMagicMissileFloor + 3] do
      let position : TilePos := ⟨x, y⟩
      let tile := if y < roof || dungeonMagicMissileFloor ≤ y ||
          x == dungeonMagicMissileLeft || x == dungeonMagicMissileRight then layout.brick
        else if chest.contains position then .dungeonGoldChest
        else if y + 1 == dungeonMagicMissileFloor && (#[7, 14, 22, 31, 41] : Array Nat).contains x then .torch
        else .air
      edits := edits.push (position, tile)
  let shaft := dungeonMagicMissileShaftColumn
  for x in [shaft:shaft + 2] do
    for y in [dungeonMagicMissileFloor:dungeonMagicMissileShaftFloor + 2] do
      let tile := if dungeonMagicMissileShaftFloor ≤ y then layout.brick
        else if y % 2 == 0 then .platform
        else if x == shaft && (#[109, 115, 121] : Array Nat).contains y then .torch
        else .air
      edits := edits.push (⟨x, y⟩, tile)
  for y in [dungeonMagicMissileFloor + 1:dungeonMagicMissileShaftFloor + 2] do
    edits := edits.push (⟨shaft - 1, y⟩, layout.brick)
    if y < dungeonMagicMissilePassageRoof then
      edits := edits.push (⟨shaft + 2, y⟩, layout.brick)
  for x in [shaft + 2:dungeonBlueMoonLeft] do
    let floor := dungeonMagicMissilePassageFloor x
    for y in [dungeonMagicMissilePassageRoof:floor + 2] do
      let tile := if floor ≤ y then layout.brick
        else if x == 12 && y + 1 == floor then .torch
        else .air
      edits := edits.push (⟨x, y⟩, tile)
  for y in [dungeonMagicMissilePassageRoof:dungeonBlueMoonFloor] do
    edits := edits.push (⟨dungeonBlueMoonLeft, y⟩, .air)
  return edits

/-- Open the passage only where the old wing and its surrounding masonry
match their v43 generation. The caller supplies occupancy and edit protection. -/
def retrofitMagicMissileDungeon (seed : UInt64) (world : World)
    (allowed : TilePos → Bool) : Option World :=
  retrofitPristineDungeonExtension (generatePreMagicMissileDungeonTerrain seed) world
    (magicMissileDungeonEdits seed) allowed

/-- The exact v44 terrain through the Magic Missile wing. -/
def generatePreDeepDungeonTerrain (seed : UInt64) : World :=
  installDungeon (generatePreMagicMissileDungeonTerrain seed) (magicMissileDungeonEdits seed)

private def deepDungeonEdits (seed : UInt64) : Array (TilePos × Tile) := Id.run do
  let layout := dungeonLayout seed
  let locked := dungeonChestCells layout.valorChest ++ dungeonChestCells layout.handgunChest
  let wooden := dungeonChestCells layout.deepKeyChest
  let mut edits := #[]
  -- Open the old wall above its existing floor, keeping the original roof.
  for y in [dungeonDeepRoof dungeonMagicMissileRight:dungeonMagicMissileFloor] do
    edits := edits.push (⟨dungeonMagicMissileRight, y⟩, .air)
  for x in [dungeonMagicMissileRight + 1:dungeonDeepRight + 1] do
    let roof := dungeonDeepRoof x
    let floor := dungeonDeepFloorAt x
    for y in [max 92 (roof - 2):floor + 2] do
      let position : TilePos := ⟨x, y⟩
      let tile := if y < roof || floor ≤ y || x == dungeonDeepRight then layout.brick
        else if locked.contains position then .dungeonGoldChest
        else if wooden.contains position then .dungeonKeyChest
        else if y + 1 == floor && (#[48, 54, 64, 76, 89] : Array Nat).contains x then .torch
        else .air
      edits := edits.push (position, tile)
  return edits

/-- Add the eastern vault only if the v44 entrance and its full structural
margin are pristine. The caller supplies occupancy and edit protection. -/
def retrofitDeepDungeon (seed : UInt64) (world : World)
    (allowed : TilePos → Bool) : Option World :=
  retrofitPristineDungeonExtension (generatePreDeepDungeonTerrain seed) world (deepDungeonEdits seed) allowed

/-- The exact 512×128 v45 terrain through the eastern Dungeon vault. -/
def generatePreUnderworldTerrain (seed : UInt64) : World :=
  installDungeon (generatePreDeepDungeonTerrain seed) (deepDungeonEdits seed)

structure UnderworldTower where
  left : Nat
  right : Nat
  roof : Nat
  floor : Nat
  chest : TilePos
  brokenWest : Bool
  deriving BEq, Repr, Inhabited

structure UnderworldBasin where
  left : Nat
  right : Nat
  surface : Nat
  floor : Nat
  surfaceLevel : Nat
  gap : Nat
  deriving BEq, Repr, Inhabited

structure UnderworldLayout where
  westTower : UnderworldTower
  eastTower : UnderworldTower
  basins : Array UnderworldBasin
  extraTowers : Array UnderworldTower := #[]
  deriving BEq, Repr, Inhabited

private def underworldTower (seed : UInt64) (column span height : Nat) : UnderworldTower :=
  let left := column + sample seed column 151 % 6
  let right := left + span + sample seed column 157 % 4
  let floor := 154
  let chestColumn := left + 7 + sample seed column 163 % (span - 8)
  { left, right, roof := floor - height, floor, chest := ⟨chestColumn, floor - 2⟩
    brokenWest := sample seed column 167 % 2 == 0 }

private def underworldBasin (seed : UInt64) (column : Nat) : UnderworldBasin :=
  let left := column + sample seed column 173 % 7
  let right := left + 24 + sample seed column 179 % 5
  { left, right, surface := 159 + sample seed column 181 % 3
    floor := 168 + sample seed column 191 % 3
    surfaceLevel := 160 + 16 * (sample seed column 193 % 5)
    gap := left + 3 + sample seed column 197 % 5 }

def underworldLayout (seed : UInt64) : UnderworldLayout :=
  { westTower := underworldTower seed 133 16 16
    eastTower := underworldTower seed 319 19 18
    basins := (#[65, 184, 239, 370, 417] : Array Nat).map (underworldBasin seed)
    extraTowers := #[underworldTower seed 162 13 16, underworldTower seed 220 10 14,
      underworldTower seed 348 13 18] }

def UnderworldLayout.legacyShadowChests (layout : UnderworldLayout) : Array TilePos :=
  #[layout.westTower.chest, layout.eastTower.chest]

/-- Keep the original two indices for saves, then list the new sites west to east. -/
def UnderworldLayout.shadowChests (layout : UnderworldLayout) : Array TilePos :=
  layout.legacyShadowChests ++ layout.extraTowers.map (·.chest)

def shadowChestAnchor (seed : UInt64) (position : TilePos) : Option TilePos :=
  if position.y < underworldStart then none
  else (underworldLayout seed).shadowChests.find? fun anchor =>
    (dungeonChestCells anchor).contains position

private def UnderworldTower.hasWall (tower : UnderworldTower) (position : TilePos) : Bool :=
  tower.left < position.x && position.x < tower.right &&
    tower.roof < position.y && position.y < tower.floor

/-- Natural obsidian-brick background walls in the ruined houses. Supplying
terrain hides the background of new houses skipped during migration. -/
def underworldUnsafeWall (seed : UInt64) (position : TilePos) (world : Option World := none) : Bool :=
  if position.y < underworldStart then false else
    let layout := underworldLayout seed
    layout.westTower.hasWall position || layout.eastTower.hasWall position ||
      layout.extraTowers.any fun tower => tower.hasWall position && world.all fun terrain =>
        terrain.tile ⟨tower.left, tower.floor + 1⟩ == .obsidianBrick &&
        terrain.tile ⟨tower.right, tower.floor + 1⟩ == .obsidianBrick

/-- Initial lava uses native liquid bytes. The exposed surface has the same
amount across each basin so a newly loaded world need not settle horizontally. -/
def UnderworldLayout.lavaLevel (layout : UnderworldLayout) (position : TilePos) : Nat :=
  if position.y < underworldStart || position.x < 0 then 0 else
    match layout.basins.find? (fun basin => basin.left ≤ position.x && position.x ≤ basin.right &&
      basin.surface ≤ position.y && position.y < basin.floor) with
    | some basin => if position.y == basin.surface then basin.surfaceLevel else 255
    | none => 0

def underworldLavaLevel (seed : UInt64) (position : TilePos) : Nat :=
  (underworldLayout seed).lavaLevel position

private def underworldAshFloor (seed : UInt64) (layout : UnderworldLayout) (x : Nat) : Nat := Id.run do
  let floor := 152 + noise seed x 149 18 1 / 64
  if x ≤ 52 then return min floor (133 + (x - 8) / 2)
  if 460 ≤ x then return min floor (134 + (503 - x) / 2)
  for tower in #[layout.westTower, layout.eastTower] do
    if tower.left ≤ x && x ≤ tower.right then return tower.floor
    if tower.left - 4 < x && x < tower.left then
      return blend tower.floor floor (tower.left - x) 4
    if tower.right < x && x < tower.right + 4 then
      return blend tower.floor floor (x - tower.right) 4
  return floor

private def UnderworldTower.tile? (tower : UnderworldTower) (x y : Nat)
    (lowDoorHeight := 3) : Option Tile := do
  if x < tower.left || tower.right < x || y < tower.roof || tower.floor + 1 < y then none
  else
    let west := x - tower.left
    let east := tower.right - x
    let roofGap := if tower.brokenWest then 2 ≤ west && west ≤ 4 else 2 ≤ east && east ≤ 4
    let upperDeck := tower.floor - 6
    let shaft := 3 ≤ west && west ≤ 4
    let torchColumn := west == 1 || east == 1
    if tower.floor ≤ y then return .obsidianBrick
    if y == tower.roof then return if roofGap then .air else .obsidianBrick
    if west == 0 || east == 0 then
      let lowDoor := tower.floor - lowDoorHeight ≤ y
      let upperOpening := tower.roof + 3 ≤ y && y ≤ tower.roof + 4
      let brokenCorner := y == tower.roof + 1 && (west == 0) == tower.brokenWest
      return if lowDoor || upperOpening || brokenCorner then .air else .obsidianBrick
    if shaft && upperDeck ≤ y && y % 2 == 0 then return .platform
    if y == upperDeck then return .obsidianBrick
    if tower.chest.x ≤ x && x ≤ tower.chest.x + 1 && tower.chest.y ≤ y && y ≤ tower.chest.y + 1 then
      return .shadowChest
    if torchColumn && (y + 1 == tower.floor || y + 1 == upperDeck) then return .torch
    return .air

private def UnderworldBasin.tile? (basin : UnderworldBasin) (seed : UInt64)
    (x y ashFloor : Nat) : Option Tile := do
  if x < basin.left || basin.right < x || y < ashFloor then none else
    let gap := (basin.gap ≤ x && x < basin.gap + 2) ||
      (basin.gap + 11 ≤ x && x < basin.gap + 13)
    if y < basin.floor then return if y < ashFloor + 2 && !gap then .ash else .air
    let center := (basin.left + basin.right) / 2 + sample seed basin.left 199 % 5
    if y ≤ basin.floor + 2 && ((x : Int) - center).natAbs + (y - basin.floor) ≤ 3 then
      return .hellstone
    return .ash

private def UnderworldTower.hasHellstone (tower : UnderworldTower) (seed : UInt64) (x y : Nat) : Bool :=
  let center := tower.left + 2 + sample seed tower.left 211 % 4
  tower.floor + 3 ≤ y && y ≤ tower.floor + 6 &&
    ((x : Int) - center).natAbs + (y - (tower.floor + 3)) ≤ 3

private def underworldTile (seed : UInt64) (layout : UnderworldLayout) (x y : Nat) : Tile := Id.run do
  if x == 0 || x + 1 == worldWidth || y + 2 ≥ underworldHeight then return .bedrock
  let floor := underworldAshFloor seed layout x
  let naturalRoof := if x ≤ 17 || 490 ≤ x then underworldStart + 2
    else underworldStart + 2 + noise seed x 137 13 1 / 43
  if y ≤ min (floor - 4) naturalRoof then return .ash
  if let some tile := layout.westTower.tile? x y then return tile
  if let some tile := layout.eastTower.tile? x y then return tile
  if y < floor then return .air
  if let some basin := layout.basins.find? (fun basin => basin.left ≤ x && x ≤ basin.right) then
    if let some tile := basin.tile? seed x y floor then return tile
  if layout.westTower.hasHellstone seed x y || layout.eastTower.hasHellstone seed x y then
    return .hellstone
  return .ash

/-- Preserve the old world, including player edits. Only interior bedrock in
the old bottom two rows becomes mineable Ash, keeping its existing support. -/
def extendUnderworld (seed : UInt64) (previous : World) : World := Id.run do
  if previous.width != worldWidth || previous.height != underworldStart ||
      previous.tiles.size != previous.width * previous.height then return previous
  let mut tiles := previous.tiles
  for y in [underworldStart - 2:underworldStart] do
    for x in [1:worldWidth - 1] do
      let index := y * worldWidth + x
      if tiles[index]? == some Tile.bedrock.code then
        tiles := tiles.set! index Tile.ash.code
  let layout := underworldLayout seed
  for y in [underworldStart:underworldHeight] do
    for x in [:worldWidth] do
      tiles := tiles.push (underworldTile seed layout x y).code
  return { width := worldWidth, height := underworldHeight, tiles }

/-- The exact v46 world, including the original two Ruined Houses. -/
def generatePreShadowTowersTerrain (seed : UInt64) : World :=
  extendUnderworld seed (generatePreUnderworldTerrain seed)

private def shadowTowerChanges (seed : UInt64) (layout : UnderworldLayout)
    (tower : UnderworldTower) : Array (TilePos × Tile × Tile) := Id.run do
  let mut changes := #[]
  for x in [tower.left:tower.right + 1] do
    for y in [tower.roof:tower.floor + 2] do
      -- The taller openings accommodate the surrounding natural ash steps.
      let some tile := tower.tile? x y (lowDoorHeight := 5) | continue
      let previous := underworldTile seed layout x y
      if previous != tile then changes := changes.push (⟨x, y⟩, previous, tile)
  return changes

private def installShadowTower (world : World) (changes : Array (TilePos × Tile × Tile)) : World :=
  changes.foldl (fun terrain (position, _, tile) => terrain.setTile position tile) world

/-- Install one new tower if every changed cell is pristine and can be edited.
The caller protects players, active objects and their supports. -/
def retrofitShadowChest (seed : UInt64) (world : World) (anchor : TilePos)
    (allowed : TilePos → Bool) : Option World := do
  if world.width != worldWidth || world.height != underworldHeight ||
      world.tiles.size != world.width * world.height then failure
  let layout := underworldLayout seed
  let tower ← layout.extraTowers.find? (·.chest == anchor)
  let changes := shadowTowerChanges seed layout tower
  if !changes.all (fun (position, previous, _) => allowed position && world.tile? position == some previous) then
    failure
  return installShadowTower world changes

def generateTerrain (seed : UInt64) : World :=
  let layout := underworldLayout seed
  layout.extraTowers.foldl (fun world tower =>
    installShadowTower world (shadowTowerChanges seed layout tower)) (generatePreShadowTowersTerrain seed)

end Lean4Terria
