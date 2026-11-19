import Lean4Terria.Fishing

namespace Lean4Terria

inductive AnglerBiome where
  | forest | sky | snow | jungle | beach | desert | corruption | crimson | hallow | mushroom | honey
  deriving BEq, Repr

inductive AnglerDepth where
  | any | surface | surfaceOrSky | surfaceOrUnderground | surfaceOrDeeper | underground | cavern
  deriving BEq, Repr

structure AnglerQuest where
  fish : Item
  biome : AnglerBiome
  depth : AnglerDepth := .any
  deriving BEq, Repr

/-- Order of Main.anglerQuestItemNetIDs. Habitat requirements come from the
normal-world fishing rules. -/
def anglerQuests : Array AnglerQuest := #[
  ⟨.batfish, .forest, .underground⟩, ⟨.bumblebeeTuna, .honey, .any⟩,
  ⟨.catfish, .jungle, .surface⟩, ⟨.cloudfish, .sky, .surfaceOrSky⟩,
  ⟨.cursedfish, .corruption, .any⟩, ⟨.dirtfish, .forest, .surfaceOrUnderground⟩,
  ⟨.dynamiteFish, .forest, .surface⟩, ⟨.eaterofPlankton, .corruption, .any⟩,
  ⟨.fallenStarfish, .sky, .surfaceOrSky⟩, ⟨.theFishofCthulu, .sky, .surfaceOrSky⟩,
  ⟨.fishotron, .forest, .cavern⟩, ⟨.harpyfish, .sky, .surfaceOrSky⟩,
  ⟨.hungerfish, .forest, .cavern⟩, ⟨.ichorfish, .crimson, .any⟩,
  ⟨.jewelfish, .forest, .underground⟩, ⟨.mirageFish, .hallow, .underground⟩,
  ⟨.mutantFlinxfin, .snow, .underground⟩, ⟨.pengfish, .snow, .surfaceOrSky⟩,
  ⟨.pixiefish, .hallow, .surfaceOrSky⟩, ⟨.spiderfish, .forest, .underground⟩,
  ⟨.tundraTrout, .snow, .surface⟩, ⟨.unicornFish, .hallow, .any⟩,
  ⟨.guideVoodooFish, .forest, .cavern⟩, ⟨.wyverntail, .sky, .surfaceOrSky⟩,
  ⟨.zombieFish, .forest, .surface⟩, ⟨.amanitaFungifin, .mushroom, .any⟩,
  ⟨.angelfish, .sky, .surfaceOrSky⟩, ⟨.bloodyManowar, .crimson, .any⟩,
  ⟨.bonefish, .forest, .underground⟩, ⟨.bunnyfish, .forest, .surface⟩,
  ⟨.capnTunabeard, .beach, .any⟩, ⟨.clownfish, .beach, .any⟩,
  ⟨.demonicHellfish, .forest, .cavern⟩, ⟨.derpfish, .jungle, .surface⟩,
  ⟨.fishron, .snow, .underground⟩, ⟨.infectedScabbardfish, .corruption, .any⟩,
  ⟨.mudfish, .jungle, .surfaceOrDeeper⟩, ⟨.slimefish, .forest, .any⟩,
  ⟨.tropicalBarracuda, .jungle, .surface⟩, ⟨.scarabFish, .desert, .any⟩,
  ⟨.scorpioFish, .desert, .any⟩
]

def AnglerQuest.location (quest : AnglerQuest) : String :=
  let biome := match quest.biome with
    | .forest => "Forest"
    | .sky => "Sky"
    | .snow => "Snow"
    | .jungle => "Jungle"
    | .beach => "Ocean"
    | .desert => "Desert"
    | .corruption => "Corruption"
    | .crimson => "Crimson"
    | .hallow => "Hallow"
    | .mushroom => "Glowing Mushroom"
    | .honey => "Honey"
  let depth := match quest.depth with
    | .any => ""
    | .surface => " on the surface"
    | .surfaceOrSky => if quest.biome == .sky then "" else " on the surface or in the sky"
    | .surfaceOrUnderground => " on the surface or underground"
    | .surfaceOrDeeper => " on the surface or below"
    | .underground => " underground"
    | .cavern => " in the cavern layer"
  biome ++ depth

/-- Compact-world equivalent of the daily quest re-roll. A quest is stable
until dawn even when a boss is defeated in the middle of the day. -/
def chooseAnglerQuest (seed : UInt64) (day : Nat) (hardmode crimson bossDefeated eastern : Bool)
    (honeyAvailable := false) : Nat :=
  let eligible := (Array.range anglerQuests.size).filter fun index =>
    let quest := anglerQuests[index]?.getD ⟨.dirtfish, .forest, .surfaceOrUnderground⟩
    let fish := quest.fish
    (honeyAvailable || quest.biome != .honey) && quest.biome != .mushroom &&
      (eastern || (quest.biome != .beach && quest.biome != .desert && quest.biome != .jungle)) &&
      (quest.biome != .corruption || !crimson) && (quest.biome != .crimson || crimson) &&
      (hardmode || !([.cursedfish, .hungerfish, .ichorfish, .mirageFish, .pixiefish, .unicornFish,
        .wyverntail, .capnTunabeard, .derpfish, .fishron] : List Item).contains fish) &&
      (bossDefeated || !([.angelfish, .cloudfish, .wyverntail] : List Item).contains fish)
  let rng := UnifiedRandom.fromEntropy (hash (seed, day, (3228 : Nat)))
  let (index, _) := rng.next 0 eligible.size
  eligible[index.toNat]?.getD 5

structure AnglerState where
  rescued : Bool := false
  position : Option TilePos := none
  home : Option TilePos := none
  questIndex : Nat := 0
  completed : Nat := 0
  turnedIn : Bool := false
  deriving BEq, Repr, Inhabited

def AnglerState.quest (state : AnglerState) : AnglerQuest :=
  anglerQuests[state.questIndex]?.getD ⟨.dirtfish, .forest, .surfaceOrUnderground⟩

private abbrev RewardRoll := StateM UnifiedRandom

private def draw (lo hi : Nat) : RewardRoll Nat := do
  let (n, rng) := (← get).next lo hi
  set rng
  return n.toNat

private def fraction : RewardRoll Float := do
  let (n, rng) := (← get).sample
  set rng
  return n

private def single (n : Nat) : Float32 := (Int32.ofNat n).toFloat32

def anglerRewardMultiplier (completed : Nat) (priceAdjustment : Float32 := 1) : Float32 :=
  let rarity : Float32 :=
    if completed ≤ 50 then 1 - single completed * 0.01
    else if completed ≤ 100 then 0.5 - single (completed - 50) * 0.005
    else if completed ≤ 150 then 0.25 - single (completed - 100) * 0.002
    else 0.15
  rarity * 0.9 * ((1 + priceAdjustment) / 2)

private def denominator (n : Nat) (multiplier : Float32) : Nat :=
  (single n * multiplier).toUInt32.toNat

private def anglerAccessories : Array Item :=
  #[.highTestFishingLine, .anglerEarring, .tackleBox, .fishermansGuide, .weatherRadio, .sextant, .anglerBobber]

private def mainReward (completed : Nat) (fish : Item) (hardmode : Bool)
    (multiplier : Float32) (owns : Item → Bool) : RewardRoll Stack := do
  let milestone := match completed with
    | 5 => some Item.fuzzyCarrot
    | 10 => some .anglerHat
    | 15 => some .anglerVest
    | 20 => some .anglerPants
    | 25 => some .bottomlessWaterBucket
    | 30 => some .goldenFishingRod
    | _ => none
  if let some item := milestone then return ⟨item, 1⟩
  if fish == .bumblebeeTuna then
    if (← if hardmode then pure true else (· == 0) <$> draw 0 2) then
      return ⟨if (← draw 0 2) == 0 then .honeyAbsorbantSponge else .bottomlessHoneyBucket, 1⟩
  for (eligible, odds, item) in [
      (decide (completed > 75), 250, Item.goldenFishingRod),
      (hardmode && completed > 25, 100, .hotlineFishingHook),
      (hardmode && completed > 10, 70, .finWings),
      (decide (completed > 10), 70, .bottomlessWaterBucket),
      (decide (completed > 10), 70, .superAbsorbantSponge),
      (true, 80, .goldenBugNet), (true, 60, .anglerFishHook), (true, 60, .fishMinecart),
      (true, 80, .seashellHairpin), (true, 80, .fishCostumeMask)] do
    if eligible then
      if (← draw 0 (denominator odds multiplier)) == 0 then return ⟨item, 1⟩
  let remaining := anglerAccessories.filter (fun item => !owns item)
  let mut failure : Float32 := 1
  for odds in [40, 40, 40, 30, 30, 30, 25] do
    failure := failure * (1 - 1 / single (denominator odds multiplier))
  let chance := ((1 - failure) * 0.8).toFloat
  let accessories := if remaining.isEmpty then anglerAccessories else remaining
  if (← fraction) < chance then
    return ⟨accessories[(← draw 0 accessories.size)]!, 1⟩
  let potion := (#[.fishingPotion, .sonarPotion, .cratePotion] : Array Item)[(← draw 0 3)]!
  return ⟨potion, ← draw 2 6⟩

private def decorations : Array Item := #[
  .lifePreserver, .shipsWheel, .compassRose, .wallAnchor, .pillaginMePixels, .treasureMap,
  .goldfishTrophy, .bunnyfishTrophy, .swordfishTrophy, .sharkteethTrophy, .shipInABottle,
  .seaweedPlanter, .notSoLostInParadise, .crustography, .fangs, .whatLurksBelow, .couchGag, .silentFish, .theDuke]

/-- The native reward calls run in main, decoration, money and bait order. -/
def anglerRewards (completed : Nat) (fish : Item) (hardmode : Bool)
    (owns : Item → Bool) (rng : UnifiedRandom) (priceAdjustment : Float32 := 1) : Array Stack :=
  ((do
    let multiplier := anglerRewardMultiplier completed priceAdjustment
    let main ← mainReward completed fish hardmode multiplier owns
    let mut loot := #[main]
    if main.item == .seashellHairpin then loot := loot ++ #[⟨.mermaidAdornment, 1⟩, ⟨.mermaidTail, 1⟩]
    if main.item == .fishCostumeMask then loot := loot ++ #[⟨.fishCostumeShirt, 1⟩, ⟨.fishCostumeFinskirt, 1⟩]
    let decorationChance := (1 - multiplier) + (1 - (1 - multiplier)) * min 1 (single completed / 100)
    let giveDecoration ← if decorationChance ≥ 1 then pure true
      else fun rng => let (sample, rng) := rng.sample; (sample.toFloat32 ≤ decorationChance, rng)
    if giveDecoration then
      loot := loot.push ⟨decorations[(← draw 0 decorations.size)]!, 1⟩
    let silver := (single (((completed + 50) / 2) * (← draw 50 201)) * 0.015).toUInt32.toNat
    let silver := (single silver * 1.5).toUInt32.toNat * (if hardmode then 2 else 1)
    loot := loot.push (if silver > 100 then ⟨.goldCoin, min 10 (silver / 100)⟩ else ⟨.silverCoin, max 1 (min 99 silver)⟩)
    if (← draw 0 (denominator 100 multiplier)) ≤ 50 then
      let master ← draw 0 (denominator 15 multiplier)
      let item ← if master == 0 then pure Item.masterBait else do
        let journeyman ← draw 0 (denominator 5 multiplier)
        pure (if journeyman == 0 then .journeymanBait else .apprenticeBait)
      let mut count := 1
      for odds in [25, 50, 100, 150, 200, 250] do
        if (← draw 0 odds) ≤ completed then count := count + 1
      loot := loot.push ⟨item, count⟩
    return loot : RewardRoll (Array Stack)).run rng).1

end Lean4Terria
