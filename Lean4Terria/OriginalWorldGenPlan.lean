/-!
# World-generation plan of desktop Terraria 1.4.5.8 (`Terraria.exe` sha256 `d52c7ea7…03e0`)

Hand transcription of the real desktop CIL (dumps in `.lake/original-world-gen-plan/harness/
dumpsel-*/il/`) of the pass registry and the pass driver; the terrain passes themselves are NOT
here (each is a typed callee, listed in `INTERFACE.md` with its weight and native method):

* `WorldGen.AddPasses` 100667780 — exact registration order, the `skyblockWorldGen`,
  `Skyblock.denyAllGeneration` (= `WorldGen.skyblockWorldGen`, 100679672) and
  `SecretSeed.dualDungeons.Enabled` branches; `WorldGen.AddGenerationPass` (three overloads,
  100667754/5/6); `WorldGenerator.Append` 100669499; `PassLegacy..ctor` (both) with
  `PassLegacy.GetWeight` 100677014 (`_weightMap`, `Single` widened to `double`, default `1f`);
  `TerrainPass` / `JunglePass` / `DitherSnakePass` constructors (weights `449.3721923828125`,
  `10154.65234375`, `1`); `GenPass..ctor` (enabled), `Disable`; `WorldGen.DisablePassesForSpecialSeeds`
  100667781 (the 12 passes the dual-dungeons seed disables).
* `WorldGenerator.GenerateWorld` 100669500 (total weight over enabled passes, abort / pause loop,
  resumption from `WorldGen.Manifest.GenPassResults.Count`, return `!aborted`) and
  `WorldGenerator.RunPass` 100669502 (skip result for a disabled pass; otherwise
  `Main.rand = new UnifiedRandom(_seed)`, `progress.Start(weight)`, `GetPassConfiguration`,
  `Apply` with exceptions reported and swallowed, `progress.End()`, the liquid invariant, and
  `RandNext = WorldGen.genRand.Next()` — `genRand` IS `Main.rand` (100667643)).
* `GenerationProgress` `.ctor`, `Start`, `End`, `set_Value` (`Utils.Clamp<double>`), `Set` (both),
  `TotalProgress`.
* `Terraria.Utilities.UnifiedRandom` `.ctor(int)` / `SetSeed` 100669664 / `InternalSample`
  100669666 / `Next()` / `Sample` (single-index Knuth variant:
  `inext`, `inext + 21` wrapped at 55) — a small direct dependency transcribed here.

The constant tables (`genPassNameIds`, `legacyWeightMap`, `addPassesLegacy`) are generated
mechanically from the `.cctor` / `AddPasses` CIL by `harness/gen_tables.py` and natively checked.
The `WorldGenerator.Controller` (debug snapshots, world hashing, pausing) and every pass body are
typed callees (`Callees m`); nothing defaults to "pass succeeded".
-/
namespace Lean4Terria.OriginalWorldGenPlan

inductive Fault where
  /-- `ArgumentOutOfRangeException` (`List<GenPass>` indexer when the manifest holds more results
  than there are passes). -/
  | argumentOutOfRange (detail : String)
  /-- A name the `GenPassNameID` table does not have (the CIL cannot produce it). -/
  | unknownNameId (field : String)
  /-- An exception raised by a callee that the native code does not catch. -/
  | callee (name : String) (detail : String)
deriving Repr, Inhabited

/-! ## `Terraria.Utilities.UnifiedRandom` -/

structure UnifiedRandom where
  seedArray : Array Int32
  inext : UInt32
deriving Inhabited, Repr, BEq

namespace UnifiedRandom
def mbig : Int32 := 2147483647
def mseed : Int32 := 161803398

/-- `.ctor(int)` → `SetSeed(seed)`. -/
def create (seed : Int32) : UnifiedRandom := Id.run do
  let mut a := Array.replicate 56 (0 : Int32)
  let num4 : Int32 := if seed == -2147483648 then mbig else if seed < 0 then -seed else seed
  let mut num2 := mseed - num4
  a := a.set! 55 num2
  let mut num3 : Int32 := 1
  for i in [1:55] do
    let idx := (21 * i) % 55
    a := a.set! idx num3
    num3 := num2 - num3
    if num3 < 0 then num3 := num3 + mbig
    num2 := a[idx]!
  for _ in [1:5] do
    for k in [1:56] do
      let v := a[k]! - a[1 + (k + 30) % 55]!
      a := a.set! k (if v < 0 then v + mbig else v)
  return ⟨a, 0⟩

/-- `InternalSample()`. -/
def internalSample (r : UnifiedRandom) : Int32 × UnifiedRandom :=
  let i := if r.inext + 1 > 55 then 1 else r.inext + 1
  let j := if i + 21 > 55 then i + 21 - 55 else i + 21
  let n := r.seedArray[i.toNat]! - r.seedArray[j.toNat]!
  let n := if n == mbig then n - 1 else n
  let n := n + ((n >>> 31) &&& mbig)
  (n, ⟨r.seedArray.set! i.toNat n, i⟩)

/-- `Next()`. -/
def next (r : UnifiedRandom) : Int32 × UnifiedRandom := r.internalSample

/-- `Sample()`. -/
def sample (r : UnifiedRandom) : Float × UnifiedRandom :=
  let (n, r) := r.internalSample
  (Float.ofInt n.toInt * 4.656612875245797e-10, r)

end UnifiedRandom

/-! ## Tables generated from the CIL (`harness/gen_tables.py`) -/

/-- `Terraria.ID.GenPassNameID` (field, value) from `.cctor` 100671617, in CIL order. -/
def genPassNameIds : Array (String × String) := #[
  ("Terrain", "Terrain"),
  ("Skyblock", "Skyblock"),
  ("DunesAndPyramidLocations", "Dunes"),
  ("OceanSand", "Ocean Sand"),
  ("SandPatches", "Sand Patches"),
  ("Tunnels", "Tunnels"),
  ("MountainCaves", "Mount Caves"),
  ("DirtWallBackgrounds", "Dirt Wall Backgrounds"),
  ("RocksInDirt", "Rocks In Dirt"),
  ("DirtInRocks", "Dirt In Rocks"),
  ("Clay", "Clay"),
  ("SmallHoles", "Small Holes"),
  ("DirtLayerCaves", "Dirt Layer Caves"),
  ("RockLayerCaves", "Rock Layer Caves"),
  ("SurfaceCaves", "Surface Caves"),
  ("WavyCaves", "Wavy Caves"),
  ("IceBiome", "Generate Ice Biome"),
  ("Grass", "Grass"),
  ("Jungle", "Jungle"),
  ("MudCavesToJungleGrass", "Mud Caves To Grass"),
  ("DesertBiome", "Full Desert"),
  ("GlowingMushroomPatches", "Mushroom Patches"),
  ("Marble", "Marble"),
  ("Granite", "Granite"),
  ("FloatingIslands", "Floating Islands"),
  ("DirtToMud", "Dirt To Mud"),
  ("Silt", "Silt"),
  ("OresAndShinies", "Shinies"),
  ("Webs", "Webs"),
  ("Underworld", "Underworld"),
  ("CorruptionAndCrimson", "Corruption"),
  ("Lakes", "Lakes"),
  ("StoneToIceAndSiltPlusMudIntoSlush", "Slush"),
  ("DualDungeonsDitherSnake", "Dual Dungeons Dither Snake"),
  ("Dungeon", "Dungeon"),
  ("MountainCaveOpenings", "Mountain Caves"),
  ("BeachesAndOceanCleanup", "Beaches"),
  ("Gems", "Gems"),
  ("GravitatingSandCleanup", "Gravitating Sand"),
  ("OceanCaves", "Create Ocean Caves"),
  ("Shimmer", "Shimmer"),
  ("DirtWallCleanup", "Clean Up Dirt"),
  ("Pyramids", "Pyramids"),
  ("DirtRockWallRunner", "Dirt Rock Wall Runner"),
  ("LivingTrees", "Living Trees"),
  ("LivingTreeWalls", "Wood Tree Walls"),
  ("DemonAndCrimsonAltars", "Altars"),
  ("SurfaceWaterInJungle", "Wet Jungle"),
  ("LihzahrdTemple", "Jungle Temple"),
  ("Beehives", "Hives"),
  ("JungleShrines", "Jungle Chests"),
  ("SettleLiquids", "Settle Liquids"),
  ("RemoveSurfaceWaterAboveSand", "Remove Water From Sand"),
  ("Oasis", "Oasis"),
  ("ShellPilesMarblePilesAndSpikePits", "Shell Piles"),
  ("SmoothWorld", "Smooth World"),
  ("Waterfalls", "Waterfalls"),
  ("FragileIceOverIceBiomeWater", "Ice"),
  ("CaveWallVariety", "Wall Variety"),
  ("LifeCrystals", "Life Crystals"),
  ("Statues", "Statues"),
  ("UndergroundHousesAndBuriedChests", "Buried Chests"),
  ("SurfaceChests", "Surface Chests"),
  ("ChestsInJungleShrines", "Jungle Chests Placement"),
  ("UnderwaterChests", "Water Chests"),
  ("SpiderCaves", "Spider Caves"),
  ("GemCaves", "Gem Caves"),
  ("MossAndMossCaves", "Moss"),
  ("LihzahrdTemplePart2", "Temple"),
  ("CaveWallsInEnclosedSpaces", "Cave Walls"),
  ("UndergroundJungleTrees", "Jungle Trees"),
  ("FloatingIslandHouses", "Floating Island Houses"),
  ("QuickCleanup", "Quick Cleanup"),
  ("PotsGraveyardsAndBoulderPiles", "Pots"),
  ("Hellforges", "Hellforge"),
  ("SpreadingGrassOnSurfaceSunflowersEvilsOnSurfaceAndLavaCleanup", "Spreading Grass"),
  ("SurfaceOreAndStone", "Surface Ore and Stone"),
  ("FallenLogsAndWaterFeatures", "Place Fallen Log"),
  ("Traps", "Traps"),
  ("Piles", "Piles"),
  ("SpawnPoint", "Spawn Point"),
  ("SurfaceDirtWallsToGrassWalls", "Grass Wall"),
  ("SpawnStarterNPCs", "Guide"),
  ("SunflowersPart2", "Sunflowers"),
  ("Trees", "Planting Trees"),
  ("AlchemyHerbs", "Herbs"),
  ("DyePlants", "Dye Plants"),
  ("WebsInSpiderCavesAndHoneyPlusSpeleothemsInBeehives", "Webs And Honey"),
  ("GrassPlantsEvilPlantsAndPumpkinsOnSurface", "Weeds"),
  ("GlowingMushroomPlantsUndergroundAndJunglePlants", "Glowing Mushrooms and Jungle Plants"),
  ("JunglePlantsPart2", "Jungle Plants"),
  ("Vines", "Vines"),
  ("Flowers", "Flowers"),
  ("Mushrooms", "Mushrooms"),
  ("ExposedGemsInIceBiome", "Gems In Ice Biome"),
  ("ExposedGemsUnderground", "Random Gems"),
  ("LongMoss", "Moss Grass"),
  ("DirtWallsIntoMudWallsInJungleAndJungleMinMax", "Muds Walls In Jungle"),
  ("BeeLarvaInBeehives", "Larva"),
  ("SettleLiquidsPart2AndNotTheBees", "Settle Liquids Again"),
  ("CactusPalmTreesAndCoral", "Cactus, Palm Trees, & Coral"),
  ("TileCleanup", "Tile Cleanup"),
  ("LihzahrdAltar", "Lihzahrd Altars"),
  ("MicroBiomes", "Micro Biomes"),
  ("LilypadsCattailsBambooAndSeaweed", "Water Plants"),
  ("SpeleothemsAndGemTrees", "Stalac"),
  ("BrokenTrapCleanup", "Remove Broken Traps"),
  ("FinalCleanup", "Final Cleanup")]

/-- `PassLegacy._weightMap` (the dictionary `GetWeight` reads) from `.cctor` 100677016: key and
`Single` bits, in `Add` order (decimal literal of each `ldc.r4` in a comment). -/
def legacyWeightMap : Array (String × UInt32) := #[
  ("Reset", 0x3F7779A7), -- 0.9667
  ("Terrain", 0x43FDAD0E), -- 507.352
  ("Dunes", 0x436FCA93), -- 239.7913
  ("Ocean Sand", 0x41269B3D), -- 10.4129
  ("Sand Patches", 0x43E25677), -- 452.6755
  ("Tunnels", 0x408B9724), -- 4.3622
  ("Mount Caves", 0x4247FF48), -- 49.9993
  ("Dirt Wall Backgrounds", 0x43A4640F), -- 328.7817
  ("Rocks In Dirt", 0x44C02EEA), -- 1537.4661
  ("Dirt In Rocks", 0x44BD675D), -- 1515.2301
  ("Clay", 0x439D6A96), -- 314.8327
  ("Small Holes", 0x4538BED0), -- 2955.9258
  ("Dirt Layer Caves", 0x436E4127), -- 238.2545
  ("Rock Layer Caves", 0x45294655), -- 2708.3958
  ("Surface Caves", 0x42298AF5), -- 42.3857
  ("Generate Ice Biome", 0x42C8028F), -- 100.005
  ("Grass", 0x41EE4ED9), -- 29.7885
  ("Jungle", 0x462F1752), -- 11205.83
  ("Marble", 0x45A77713), -- 5358.8843
  ("Granite", 0x4505EA9F), -- 2142.6638
  ("Mud Caves To Grass", 0x454F7C2D), -- 3319.761
  ("Full Desert", 0x461809A2), -- 9730.408
  ("Floating Islands", 0x44AA8B13), -- 1364.3461
  ("Mushroom Patches", 0x4439F131), -- 743.7686
  ("Dirt To Mud", 0x43AFAD0B), -- 351.3519
  ("Silt", 0x4353D70A), -- 211.84
  ("Shinies", 0x436D6E07), -- 237.4298
  ("Webs", 0x424AA88D), -- 50.6646
  ("Underworld", 0x460BA1FA), -- 8936.494
  ("Lakes", 0x4142D35B), -- 12.1766
  ("Corruption", 0x4488C796), -- 1094.237
  ("Dungeon", 0x43EE9920), -- 477.1963
  ("Slush", 0x425CBE28), -- 55.1857
  ("Mountain Caves", 0x4137B5DD), -- 11.4819
  ("Beaches", 0x40FA84B6), -- 7.8287
  ("Gems", 0x445FDB44), -- 895.426
  ("Gravitating Sand", 0x446961E3), -- 933.5295
  ("Clean Up Dirt", 0x442E41C4), -- 697.0276
  ("Pyramids", 0x40D6075F), -- 6.6884
  ("Dirt Rock Wall Runner", 0x41C61E4F), -- 24.7648
  ("Living Trees", 0x409DFBE7), -- 4.937
  ("Wood Tree Walls", 0x4299BDE7), -- 76.8709
  ("Altars", 0x42915247), -- 72.6607
  ("Wet Jungle", 0x41BBEF9E), -- 23.492
  ("Remove Water From Sand", 0x41B0B7E9), -- 22.0898
  ("Jungle Temple", 0x4414F5E7), -- 595.8422
  ("Hives", 0x43B9B22D), -- 371.392
  ("Jungle Chests", 0x3F16F007), -- 0.5896
  ("Smooth World", 0x45B68CDD), -- 5841.608
  ("Settle Liquids", 0x4612DA1A), -- 9398.525
  ("Waterfalls", 0x4580B554), -- 4118.666
  ("Ice", 0x432313E4), -- 163.0777
  ("Wall Variety", 0x45A4802C), -- 5264.0215
  ("Life Crystals", 0x40308659), -- 2.7582
  ("Statues", 0x428125BC), -- 64.5737
  ("Buried Chests", 0x4489D1B4), -- 1102.5532
  ("Surface Chests", 0x414D56D6), -- 12.8337
  ("Jungle Chests Placement", 0x3FAD6388), -- 1.3546
  ("Water Chests", 0x414991D1), -- 12.5981
  ("Spider Caves", 0x43EDB508), -- 475.4143
  ("Gem Caves", 0x42100EA5), -- 36.0143
  ("Moss", 0x4423F536), -- 655.8314
  ("Temple", 0x40B62268), -- 5.6917
  ("Ice Walls", 0x446F4207), -- 957.0317
  ("Jungle Trees", 0x444C4FBD), -- 817.2459
  ("Floating Island Houses", 0x3FC04817), -- 1.5022
  ("Quick Cleanup", 0x44ABCEF2), -- 1374.467
  ("Pots", 0x44CCD37E), -- 1638.6091
  ("Hellforge", 0x403753F8), -- 2.8645
  ("Spreading Grass", 0x42FF8426), -- 127.7581
  ("Place Fallen Log", 0x418AB39C), -- 17.3377
  ("Traps", 0x440CBA25), -- 562.9085
  ("Piles", 0x43902F0A), -- 288.3675
  ("Spawn Point", 0x3C449BA6), -- 0.012
  ("Grass Wall", 0x44173FF3), -- 604.9992
  ("Guide", 0x3C83126F), -- 0.016
  ("Sunflowers", 0x40859F56), -- 4.1757
  ("Planting Trees", 0x43A28CB6), -- 325.0993
  ("Cactus & Coral", 0x41FD1446), -- 31.6349
  ("Herbs", 0x42F05FCC), -- 120.1871
  ("Dye Plants", 0x4362A3B0), -- 226.6394
  ("Webs And Honey", 0x44183CF4), -- 608.9524
  ("Weeds", 0x433BF9D5), -- 187.9759
  ("Mud Caves To Grass 2", 0x442B9FBB), -- 686.4958
  ("Jungle Plants", 0x44A1E136), -- 1295.0378
  ("Vines", 0x448D91C2), -- 1132.5549
  ("Flowers", 0x41862DAC), -- 16.7723
  ("Mushrooms", 0x3E6AE7D5), -- 0.2294
  ("Gems In Ice Biome", 0x4124F27C), -- 10.3092
  ("Random Gems", 0x4193F0A4), -- 18.4925
  ("Moss Grass", 0x442BEF7D), -- 687.742
  ("Muds Walls In Jungle", 0x42B38C3D), -- 89.7739
  ("Larva", 0x3E5460AA), -- 0.2074
  ("Settle Liquids Again", 0x45DD0D2D), -- 7073.647
  ("Tile Cleanup", 0x44ED1854), -- 1896.7603
  ("Lihzahrd Altars", 0x3BE8A71E), -- 0.0071
  ("Micro Biomes", 0x455DB6E3), -- 3547.4304
  ("Stalac", 0x44939CFC), -- 1180.9058
  ("Remove Broken Traps", 0x44A1AD97), -- 1293.4247
  ("Final Cleanup", 0x450204B4)] -- 2080.294

/-- `AddPasses` 100667780 legacy registrations: (`GenPassNameID` field, lambda `<AddPasses>b__285_N`). -/
def addPassesLegacy : Array (String × Nat) := #[
  ("Skyblock", 0),
  ("DunesAndPyramidLocations", 1),
  ("OceanSand", 2),
  ("SandPatches", 3),
  ("Tunnels", 4),
  ("MountainCaves", 5),
  ("DirtWallBackgrounds", 6),
  ("RocksInDirt", 7),
  ("DirtInRocks", 8),
  ("Clay", 9),
  ("SmallHoles", 10),
  ("DirtLayerCaves", 11),
  ("RockLayerCaves", 12),
  ("SurfaceCaves", 13),
  ("WavyCaves", 14),
  ("IceBiome", 15),
  ("Grass", 16),
  ("MudCavesToJungleGrass", 17),
  ("DesertBiome", 18),
  ("GlowingMushroomPatches", 19),
  ("Marble", 20),
  ("Granite", 21),
  ("FloatingIslands", 22),
  ("DirtToMud", 23),
  ("Silt", 24),
  ("OresAndShinies", 25),
  ("Webs", 26),
  ("Underworld", 27),
  ("CorruptionAndCrimson", 28),
  ("Lakes", 29),
  ("StoneToIceAndSiltPlusMudIntoSlush", 30),
  ("Dungeon", 31),
  ("MountainCaveOpenings", 32),
  ("BeachesAndOceanCleanup", 33),
  ("Gems", 34),
  ("GravitatingSandCleanup", 35),
  ("OceanCaves", 36),
  ("Shimmer", 37),
  ("DirtWallCleanup", 38),
  ("Pyramids", 39),
  ("DirtRockWallRunner", 40),
  ("LivingTrees", 41),
  ("LivingTreeWalls", 42),
  ("DemonAndCrimsonAltars", 43),
  ("SurfaceWaterInJungle", 44),
  ("LihzahrdTemple", 45),
  ("Beehives", 46),
  ("JungleShrines", 47),
  ("SettleLiquids", 48),
  ("RemoveSurfaceWaterAboveSand", 49),
  ("Oasis", 50),
  ("ShellPilesMarblePilesAndSpikePits", 51),
  ("SmoothWorld", 52),
  ("Waterfalls", 53),
  ("FragileIceOverIceBiomeWater", 54),
  ("CaveWallVariety", 55),
  ("LifeCrystals", 56),
  ("Statues", 57),
  ("UndergroundHousesAndBuriedChests", 58),
  ("SurfaceChests", 59),
  ("ChestsInJungleShrines", 60),
  ("UnderwaterChests", 61),
  ("SpiderCaves", 62),
  ("GemCaves", 63),
  ("MossAndMossCaves", 64),
  ("LihzahrdTemplePart2", 65),
  ("CaveWallsInEnclosedSpaces", 66),
  ("UndergroundJungleTrees", 67),
  ("FloatingIslandHouses", 68),
  ("QuickCleanup", 69),
  ("PotsGraveyardsAndBoulderPiles", 70),
  ("Hellforges", 71),
  ("SpreadingGrassOnSurfaceSunflowersEvilsOnSurfaceAndLavaCleanup", 72),
  ("SurfaceOreAndStone", 73),
  ("FallenLogsAndWaterFeatures", 74),
  ("Traps", 75),
  ("Piles", 76),
  ("SpawnPoint", 77),
  ("SurfaceDirtWallsToGrassWalls", 78),
  ("SpawnStarterNPCs", 79),
  ("SunflowersPart2", 80),
  ("Trees", 81),
  ("AlchemyHerbs", 82),
  ("DyePlants", 83),
  ("WebsInSpiderCavesAndHoneyPlusSpeleothemsInBeehives", 84),
  ("GrassPlantsEvilPlantsAndPumpkinsOnSurface", 85),
  ("GlowingMushroomPlantsUndergroundAndJunglePlants", 86),
  ("JunglePlantsPart2", 87),
  ("Vines", 88),
  ("Flowers", 89),
  ("Mushrooms", 90),
  ("ExposedGemsInIceBiome", 91),
  ("ExposedGemsUnderground", 92),
  ("LongMoss", 93),
  ("DirtWallsIntoMudWallsInJungleAndJungleMinMax", 94),
  ("BeeLarvaInBeehives", 95),
  ("MicroBiomes", 96),
  ("SettleLiquidsPart2AndNotTheBees", 97),
  ("CactusPalmTreesAndCoral", 98),
  ("TileCleanup", 99),
  ("LihzahrdAltar", 100),
  ("LilypadsCattailsBambooAndSeaweed", 101),
  ("SpeleothemsAndGemTrees", 102),
  ("BrokenTrapCleanup", 103),
  ("FinalCleanup", 104)]

/-- `GenPassNameID.<field>`. -/
def nameId (field : String) : Except Fault String :=
  match genPassNameIds.find? (·.1 == field) with
  | some (_, v) => .ok v
  | none => .error (.unknownNameId field)

/-- `PassLegacy.GetWeight(name)`: `_weightMap[name]` or `1f`, as a `Single` (widened by the caller). -/
def legacyWeight (name : String) : Float32 :=
  match legacyWeightMap.find? (·.1 == name) with
  | some (_, bits) => Float32.ofBits bits
  | none => 1

/-! ## Passes and the generator -/

/-- What `Apply` dispatches to (`PassLegacy._method` = `WorldGen/<>c.<AddPasses>b__285_N`, or a
`GenPass` subclass). -/
inductive PassKind where
  | legacy (lambda : Nat)
  | terrain
  | jungle
  | ditherSnake
  /-- A pass registered by another caller (`AddGenerationPass(GenPass)` / custom delegate). -/
  | external (tag : String)
deriving Repr, Inhabited, BEq

/-- `Terraria.WorldBuilding.GenPass` (`Name`, `Weight : double`, `Enabled`) plus its kind. -/
structure GenPass where
  name : String
  weight : Float
  enabled : Bool
  kind : PassKind
deriving Repr, Inhabited

/-- `new PassLegacy(name, method)`: weight `(double)GetWeight(name)`. -/
def passLegacy (name : String) (kind : PassKind) : GenPass :=
  { name, weight := (legacyWeight name).toFloat, enabled := true, kind }
/-- `new PassLegacy(name, method, weight)`. -/
def passLegacyWeighted (name : String) (weight : Float) (kind : PassKind) : GenPass :=
  { name, weight, enabled := true, kind }

/-- `WorldGenerator._passes` (the only state `AddPasses` touches). -/
abbrev PassList := Array GenPass

/-- `WorldGenerator.Append(pass)`. -/
def append (ps : PassList) (p : GenPass) : PassList := ps.push p

/-- World flags `AddPasses` / `DisablePassesForSpecialSeeds` read. -/
structure SeedFlags where
  /-- `WorldGen.skyblockWorldGen` (also `Skyblock.denyAllGeneration`). -/
  skyblockWorldGen : Bool
  /-- `WorldGen.SecretSeed.dualDungeons.Enabled`. -/
  dualDungeons : Bool
deriving Repr, Inhabited

/-- `WorldGen.AddPasses()` appending to `ps` (`WorldGen._generator._passes`). -/
def addPasses (flags : SeedFlags) (ps : PassList) : Except Fault PassList := do
  let deny := flags.skyblockWorldGen
  let mut ps := append ps { name := ← nameId "Terrain", weight := 449.3721923828125, enabled := true, kind := .terrain }
  if flags.skyblockWorldGen then
    if !deny then
      ps := append ps { name := ← nameId "Jungle", weight := 10154.65234375, enabled := true, kind := .jungle }
  for (field, lam) in addPassesLegacy do
    if lam == 0 && !flags.skyblockWorldGen then continue
    ps := append ps (passLegacy (← nameId field) (.legacy lam))
    if lam == 16 && !deny then
      ps := append ps { name := ← nameId "Jungle", weight := 10154.65234375, enabled := true, kind := .jungle }
    if lam == 30 && !deny && flags.dualDungeons then
      ps := append ps { name := ← nameId "DualDungeonsDitherSnake", weight := 1, enabled := true, kind := .ditherSnake }
  return ps

/-- `WorldGen.DisablePassesForSpecialSeeds(passes)`. -/
def disablePassesForSpecialSeeds (flags : SeedFlags) (ps : PassList) : Except Fault PassList := do
  if !flags.dualDungeons then return ps
  let fields := #["IceBiome", "DesertBiome", "Jungle", "JungleShrines", "ChestsInJungleShrines", "Beehives",
    "BeeLarvaInBeehives", "LihzahrdTemple", "LihzahrdTemplePart2", "LihzahrdAltar", "CorruptionAndCrimson", "Shimmer"]
  let names ← fields.mapM nameId
  return ps.map fun p => if names.contains p.name then { p with enabled := false } else p

/-! ## `GenerationProgress` -/

structure GenerationProgress where
  message : String
  value : Float
  totalWeightedProgress : Float
  totalWeight : Float
  currentPassWeight : Float
deriving Repr, Inhabited

namespace GenerationProgress
/-- `.ctor()`. -/
def create : GenerationProgress := { message := "", value := 0, totalWeightedProgress := 0, totalWeight := 0, currentPassWeight := 1 }
def start (p : GenerationProgress) (weight : Float) : GenerationProgress := { p with currentPassWeight := weight, value := 0 }
def «end» (p : GenerationProgress) : GenerationProgress :=
  { p with totalWeightedProgress := p.totalWeightedProgress + p.currentPassWeight, value := 0 }
/-- `Double.CompareTo` (NaN below everything). -/
def compareDouble (a b : Float) : Int :=
  if a < b then -1 else if a > b then 1 else if a == b then 0 else if a.isNaN then (if b.isNaN then 0 else -1) else 1
/-- `set_Value(v)` = `Utils.Clamp<double>(v, 0, 1)`. -/
def setValue (p : GenerationProgress) (v : Float) : GenerationProgress :=
  { p with value := if compareDouble v 1 > 0 then 1 else if compareDouble v 0 < 0 then 0 else v }
/-- `Set(currentProgress, min, max)`. -/
def set3 (p : GenerationProgress) (cur lo hi : Float) : GenerationProgress := p.setValue (lo + cur * (hi - lo))
/-- `TotalProgress`. -/
def totalProgress (p : GenerationProgress) : Float :=
  if p.totalWeight == 0 then 0 else (p.value * p.currentPassWeight + p.totalWeightedProgress) / p.totalWeight
end GenerationProgress

/-! ## `WorldGenerator.GenerateWorld` / `RunPass` -/

/-- `Terraria.WorldBuilding.GenPassResult` (`DurationMs` is wall-clock and not modelled). -/
structure GenPassResult where
  name : String
  skipped : Bool
  randNext : Int32
  hash : Option UInt32
deriving Repr, Inhabited, BEq

/-- Mutable world-generation state the driver threads through: `Main.rand` (= `genRand`), the
progress object, `WorldGen.Manifest.GenPassResults` (may be non-empty: generation resumes there). -/
structure DriverState where
  mainRand : UnifiedRandom
  progress : GenerationProgress
  results : Array GenPassResult
deriving Inhabited

/-- How a pass body ended: normally, or with an exception (`RunPass` catches it). -/
inductive PassOutcome where
  | completed
  | threw (exceptionType : String)
deriving Repr, Inhabited, BEq

/-- Callees of the driver. `applyPass` is `GenPass.Apply(progress, configuration)` — the actual
terrain pass (not transcribed here); it receives and returns the driver state (it may draw from
`Main.rand`, move progress, …). The controller fields are `WorldGenerator.Controller` members. -/
structure Callees (m : Type → Type) where
  /-- `_controller.SetGenerator(this)`. -/
  setGenerator : m Unit
  /-- `_controller.PauseAfterPass != null`. -/
  pauseAfterPassSet : m Bool
  /-- `WorldGenerator.SetDebugWorldGenUIVisibility(visible)`. -/
  setDebugUIVisibility : Bool → m Unit
  /-- `_controller.QueuedAbort`. -/
  queuedAbort : m Bool
  /-- `_controller.Paused`. -/
  paused : m Bool
  /-- `_controller.OnPaused()`. -/
  onPaused : m Unit
  /-- `_configuration.GetPassConfiguration(name)` (inside the `try`; may throw). -/
  passConfiguration : String → m PassOutcome
  /-- `pass.Apply(progress, configuration)`. -/
  applyPass : GenPass → DriverState → m (DriverState × PassOutcome)
  /-- `_controller.ReportException("Exception in Pass: " + name, e, true)`. -/
  reportException : String → String → m Unit
  /-- `Liquid.numLiquid == 0 && LiquidBuffer.numLiquidBuffer == 0` (the `Invariant.Assert`). -/
  liquidSettled : m Bool
  /-- `Invariant.Assert(false, "Liquid left in flux by pass: {name}")`. -/
  invariantFailed : String → m Unit
  /-- `_controller.OnPassCompleted()` with the results so far (it may set the last result's
  `Hash`, snapshot, pause, …). -/
  onPassCompleted : DriverState → m DriverState

variable {m : Type → Type} [Monad m] [MonadExceptOf Fault m]

/-- `WorldGenerator.RunPass(pass)`. -/
def runPass (c : Callees m) (seed : Int32) (pass : GenPass) (s : DriverState) : m (GenPassResult × DriverState) := do
  if !pass.enabled then return ({ name := pass.name, skipped := true, randNext := 0, hash := none }, s)
  let mut s := { s with mainRand := UnifiedRandom.create seed, progress := s.progress.start pass.weight }
  let outcome ← match ← c.passConfiguration pass.name with
    | .threw e => pure (PassOutcome.threw e)
    | .completed => do
      let (s2, o) ← c.applyPass pass s
      s := s2
      pure o
  if let .threw e := outcome then c.reportException ("Exception in Pass: " ++ pass.name) e
  s := { s with progress := s.progress.«end» }
  if !(← c.liquidSettled) then c.invariantFailed s!"Liquid left in flux by pass: {pass.name}"
  let (n, r) := s.mainRand.next
  return ({ name := pass.name, skipped := false, randNext := n, hash := none }, { s with mainRand := r })

/-- `WorldGenerator.GenerateWorld()`; returns `!aborted` and the final state. `fuel` bounds the
pause loop (natively unbounded: `OnPaused` sleeps 10 ms and retries). -/
def generateWorld (c : Callees m) (seed : Int32) (passes : PassList) (s : DriverState) (fuel : Nat := 1000000) :
    m (Bool × DriverState) := do
  c.setGenerator
  let total := passes.foldl (fun acc p => if p.enabled then acc + p.weight else acc) 0
  let mut s := { s with progress := { s.progress with totalWeight := total } }
  if ← c.pauseAfterPassSet then c.setDebugUIVisibility true
  let mut aborted := false
  for _ in [0:fuel] do
    if ← c.queuedAbort then
      aborted := true
      break
    if ← c.paused then
      c.onPaused
      continue
    if s.results.size == passes.size then break
    if s.results.size > passes.size then throw (Fault.argumentOutOfRange s!"index {s.results.size} of {passes.size}")
    let pass := passes[s.results.size]!
    let (r, s2) ← runPass c seed pass s
    s := { s2 with results := s2.results.push r }
    s ← c.onPassCompleted s
  c.setDebugUIVisibility false
  return (!aborted, s)

end Lean4Terria.OriginalWorldGenPlan
