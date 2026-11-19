import Lean4Terria.Hardmode
import Lean4Terria.Inventory

namespace Lean4Terria

private abbrev CrateRoll := StateM UnifiedRandom

private def sample (lower upper : Nat) : CrateRoll Nat := do
  let (value, rng) := (← get).next lower upper
  set rng
  return value.toNat

private def chance (denominator : Nat) : CrateRoll Bool := do
  return (← sample 0 denominator) == 0

private def choose (items : Array Item) : CrateRoll Item := do
  return items[(← sample 0 items.size)]!

private def bundle (item : Item) (lower upper : Nat) : CrateRoll Stack := do
  return ⟨item, ← sample lower upper⟩

private def supplies (items : Array Item) (lower upper : Nat) : CrateRoll Stack := do
  bundle (← choose items) lower upper

private def reward (item : Item) : Stack := ⟨item, 1⟩

private def copperOres : Array Item := #[.copper, .tinOre, .iron, .leadOre]
private def silverOres : Array Item := #[.silver, .tungstenOre]
private def goldOres : Array Item := #[.goldOre, .platinumOre]
private def cobaltOres : Array Item := #[.cobaltOre, .palladiumOre]
private def mythrilOres : Array Item := #[.mythrilOre, .orichalcumOre]
private def adamantiteOres : Array Item := #[.adamantiteOre, .titaniumOre]
private def copperBars : Array Item := #[.copperBar, .tinBar, .ironBar, .leadBar]
private def silverBars : Array Item := #[.silverBar, .tungstenBar]
private def cobaltBars : Array Item := #[.cobaltBar, .palladiumBar]
private def mythrilBars : Array Item := #[.mythrilBar, .orichalcumBar]

private def woodenRoll (hardmode : Bool) : CrateRoll (Array Stack) := do
  let mut loot := #[]
  if hardmode then
    if ← chance 200 then loot := loot.push (reward .enchantedSundial)
  if loot.isEmpty then
    if ← chance 40 then loot := loot.push (reward .sailfishBoots)
  if loot.isEmpty then
    if ← chance 40 then loot := loot.push (reward .tsunamiInABottle)
  if hardmode && loot.isEmpty then
    if ← chance 25 then loot := loot.push (reward .anchor)
  if ← chance 20 then
    loot := loot.push (reward (← choose #[.aglet, .climbingClaws, .stepStool, .cordageGuide, .radar, .poisonBarb]))
  if !hardmode && loot.isEmpty then
    if ← chance 50 then loot := loot.push (reward .extractinator)
  if ← chance 7 then
    let coins ← if ← chance 3 then bundle .goldCoin 1 6 else bundle .silverCoin 20 91
    loot := loot.push coins
  if ← chance 7 then
    let early ← choose copperOres
    let upgrade ← chance 2
    let item ← if hardmode && upgrade then choose cobaltOres else pure early
    loot := loot.push (← bundle item 4 16)
  else if ← chance 8 then
    let early ← choose copperBars
    let count ← sample 2 6
    let upgrade ← chance 2
    if hardmode && upgrade then
      loot := loot.push (← supplies cobaltBars 2 4)
    else loot := loot.push ⟨early, count⟩
  if ← chance 7 then
    loot := loot.push (← supplies #[.obsidianSkinPotion, .swiftnessPotion, .ironskinPotion, .nightOwlPotion,
      .shinePotion, .hunterPotion, .gillsPotion, .miningPotion, .heartreachPotion, .trapsightPotion] 1 4)
  return loot

private def ironRoll (hardmode : Bool) : CrateRoll (Array Stack) := do
  let mut loot := #[]
  if hardmode then
    if ← chance 60 then loot := loot.push (reward .enchantedSundial)
  for (item, odds) in [(.gingerBeard, 25), (.tartarSauce, 20), (.falconBlade, 15),
      (.sailfishBoots, 20), (.tsunamiInABottle, 20)] do
    if loot.isEmpty then
      if ← chance odds then loot := loot.push (reward item)
  if ← chance 4 then loot := loot.push (← bundle .goldCoin 5 11)
  if ← chance 6 then
    let early ← choose (copperOres ++ silverOres)
    let upgrade ← chance 2
    let item ← if hardmode && upgrade then choose (cobaltOres ++ mythrilOres) else pure early
    loot := loot.push (← bundle item 12 22)
  else if ← chance 4 then
    let early ← choose (copperBars ++ silverBars)
    let count ← sample 4 8
    let upgrade ← sample 0 3
    if hardmode && upgrade > 0 then
      let item ← choose (cobaltBars ++ mythrilBars)
      loot := loot.push ⟨item, count - (← sample 0 2)⟩
    else loot := loot.push ⟨early, count⟩
  if ← chance 4 then
    loot := loot.push (← supplies #[.obsidianSkinPotion, .spelunkerPotion, .hunterPotion, .gravitationPotion,
      .miningPotion, .heartreachPotion, .calmingPotion, .flipperPotion] 2 5)
  return loot

private def goldenRoll (hardmode : Bool) : CrateRoll (Array Stack) := do
  let mut loot := #[]
  if hardmode then
    if ← chance 20 then loot := loot.push (reward .enchantedSundial)
  if loot.isEmpty then
    if ← chance 8 then loot := loot.push (reward .lifeCrystal)
  if loot.isEmpty then
    if ← chance 10 then loot := loot.push (reward .hardySaddle)
  if ← chance 3 then loot := loot.push (← bundle .goldCoin 8 21)
  if ← chance 5 then
    let early ← choose (silverOres ++ goldOres)
    let upgrade ← chance 2
    let item ← if hardmode && upgrade then choose (mythrilOres ++ adamantiteOres) else pure early
    loot := loot.push (← bundle item 25 35)
  else if ← chance 3 then
    let early ← choose #[.silverBar, .goldBar, .tungstenBar, .platinumBar]
    let upgrade ← sample 0 3
    let item ← if hardmode && upgrade > 0 then choose #[.mythrilBar, .adamantiteBar, .orichalcumBar, .titaniumBar]
      else pure early
    loot := loot.push (← bundle item 8 12)
  return loot

private def standardLoot (crate : Item) : CrateRoll (Array Stack) := do
  let hardmode := crate == .pearlwoodCrate || crate == .mythrilCrate || crate == .titaniumCrate
  let mut loot := #[]
  if crate == .woodenCrate || crate == .pearlwoodCrate then
    while loot.isEmpty do loot ← woodenRoll hardmode
    if ← chance 3 then loot := loot.push (← supplies #[.lesserHealingPotion, .lesserManaPotion] 5 16)
    if ← chance 3 then
      let item := if ← chance 3 then Item.journeymanBait else Item.apprenticeBait
      loot := loot.push (← bundle item 1 5)
  else if crate == .ironCrate || crate == .mythrilCrate then
    while loot.isEmpty do loot ← ironRoll hardmode
    if ← chance 2 then loot := loot.push (← supplies #[.healingPotion, .manaPotion] 5 16)
    if ← chance 2 then
      let item := if ← chance 3 then Item.masterBait else Item.journeymanBait
      loot := loot.push (← bundle item 2 5)
  else if crate == .goldenCrate || crate == .titaniumCrate then
    while loot.isEmpty do loot ← goldenRoll hardmode
    if ← chance 3 then
      loot := loot.push (← supplies #[.obsidianSkinPotion, .spelunkerPotion, .gravitationPotion,
        .miningPotion, .heartreachPotion] 2 6)
    if ← chance 2 then loot := loot.push (← supplies #[.healingPotion, .manaPotion] 5 21)
    if !(← chance 3) then loot := loot.push (← bundle .masterBait 3 8)
    if (← chance 30) && !hardmode then loot := loot.push (reward .enchantedSword)
    if (← chance 15) && hardmode then loot := loot.push (reward .enchantedSword)
  return loot

private def biomePrimary (crate : Item) : CrateRoll (Array Stack) := do
  let mut loot : Array Stack := #[]
  match crate with
  | .oceanCrate | .seasideCrate =>
    let common ← choose #[.breathingReed, .innerTube, .trident, .flipper]
    loot := #[reward (if ← chance 10 then .waterWalkingBoots else common)]
  | .corruptCrate | .defiledCrate =>
    loot := #[reward (← choose #[.ballOHurt, .bandOfStarpower, .musket, .shadowOrbPet, .vilethorn])]
  | .crimsonCrate | .hematicCrate =>
    loot := #[reward (← choose #[.undertaker, .rottedFork, .crimsonRod, .panicNecklace, .crimsonHeartPet])]
  | .dungeonCrate | .stockadeCrate =>
    loot := #[reward .goldenLockBox]
    if ← chance 2 then loot := loot.push (← bundle .book 5 16)
    if ← chance 2 then
      loot := loot.push (← supplies #[.blueDungeonBrick, .greenDungeonBrick, .pinkDungeonBrick] 25 51)
  | .skyCrate | .azureCrate =>
    loot := #[reward (← choose #[.luckyHorseshoe, .starfury, .shinyRedBalloon, .celestialMagnet])]
  | .jungleCrate | .brambleCrate =>
    if ← chance 20 then
      let _ ← sample 0 5
      loot := #[reward .flowerBoots]
    else
      loot := #[reward (← choose #[.ankletOfTheWind, .boomstick, .feralClaws, .staffOfRegrowth, .fiberglassFishingPole])]
  | .frozenCrate | .borealCrate =>
    loot := #[reward (← choose #[.iceBoomerang, .iceBlade, .iceSkates, .snowballCannon,
      .blizzardInABottle, .flurryBoots, .glacierFang])]
  | .oasisCrate | .mirageCrate =>
    loot := #[reward (← choose #[.ancientChisel, .scarabFishingRod, .duneriderBoots, .stormSpear,
      .thunderZapper, .bastStatue, .snakeCharmersFlute, .magicConch])]
  | .obsidianCrate | .hellstoneCrate =>
    if ← chance 20 then
      let _ ← sample 0 5
      loot := #[reward .lavaCharm]
    else
      loot := #[reward (← choose #[.flameWakerBoots, .superheatedBlood, .lavaSerpentBowl, .lavaproofFishingHook, .miniVolcano])]
    if ← chance 4 then loot := loot.push ⟨.hangingPot, 2⟩
  | _ => pure ()
  return loot

private def biomeRoll (crate : Item) (first hardmode : Bool) : CrateRoll (Array Stack) := do
  -- The first pass rolls the guaranteed biome group with Next(1).
  let mut loot ← if first && crate != .hallowedCrate && crate != .divineCrate then
      if ← chance 1 then biomePrimary crate else pure #[] else pure #[]
  let lava := crate == .obsidianCrate || crate == .hellstoneCrate
  if crate == .oasisCrate || crate == .mirageCrate then
    if ← chance 4 then loot := loot.push (← bundle .scarabBomb 4 7)
    if ← chance 2 then loot := loot.push (← bundle .sturdyFossil 10 17)
    if ← chance 35 then loot := loot.push (reward .sandstormInABottle)
  if lava then
    loot := loot.push (reward .obsidianLockBox)
    if ← chance 3 then loot := loot.push (← bundle .wetBomb 7 11)
    if ← chance 2 then
      loot := loot.push (reward (← choose #[.pottedMagmaPalm, .pottedBrimstoneBush,
        .pottedFireBrambles, .pottedLavaBulb, .pottedEmberTendrils]))
  if ← chance 4 then loot := loot.push (← bundle .goldCoin 5 13)
  if ← chance 7 then
    let early ← choose (copperOres ++ silverOres ++ goldOres)
    let upgrade ← chance 2
    let item ← if hardmode && upgrade then choose (cobaltOres ++ mythrilOres ++ adamantiteOres)
      else pure (if lava then .hellstone else early)
    loot := loot.push (← bundle item 20 36)
  if ← chance 4 then
    let early ← choose #[.ironBar, .silverBar, .goldBar, .leadBar, .tungstenBar, .platinumBar]
    let count ← sample 6 17
    let upgrade ← sample 0 3
    if hardmode && upgrade > 0 then
      let item ← choose #[.cobaltBar, .mythrilBar, .adamantiteBar, .palladiumBar, .orichalcumBar, .titaniumBar]
      loot := loot.push ⟨item, count - (← sample 0 2)⟩
    else
      loot := loot.push ⟨if lava then .hellstoneBar else early, count⟩
  return loot

private def biomeBonus (crate : Item) : CrateRoll (Array Stack) := do
  let mut loot : Array Stack := #[]
  match crate with
  | .oceanCrate | .seasideCrate =>
    if ← chance 5 then loot := loot.push (reward .ofSeaAndDreams)
    if ← chance 3 then loot := loot.push (← bundle .shellPile 20 51)
    if ← chance 10 then loot := loot.push (reward .sandcastleBucket)
    if ← chance 10 then loot := loot.push (reward .sharkBait)
  | .dungeonCrate | .stockadeCrate =>
    if ← chance 2 then
      loot := #[reward (← choose #[.greatWave, .boneWarp, .theCreationOfTheGuide, .theCursedMan,
        .theDestroyer, .poweredByBirds, .theEyeSeesTheEnd, .remnantsOfDevotion, .gloryOfTheFire,
        .goblinsPlayingPoker, .bloodMoonRising, .theGuardiansGaze, .theHangedMan, .impact,
        .thePersistencyOfEyes, .dryadisque, .theScreamer, .skellingtonJSkellingsworth, .sparky,
        .somethingEvilIsWatchingYou, .starryNight, .trioSuperHeroes, .theTwinsHaveAwoken,
        .unicornCrossingTheHallows, .facingTheCerebralMastermind])]
  | .jungleCrate | .brambleCrate =>
    if ← chance 3 then loot := loot.push (← bundle .bamboo 20 51)
    if ← chance 20 then loot := loot.push (reward .seaweedPet)
  | .frozenCrate | .borealCrate =>
    if ← chance 20 then loot := #[reward .fishPet]
  | .oasisCrate | .mirageCrate =>
    if ← chance 2 then
      loot := #[reward (← choose #[.divineEye, .watchfulAntlion, .burningSpirit, .lifeAboveTheSand,
        .theSandsOfSlime, .bandageBoy, .jawsOfDeath, .oasis, .prehistoryPreserved,
        .ancientTablet, .uluru, .visitingThePyramids, .snakesIHateSnakes, .andrewSphinx])]
  | .skyCrate | .azureCrate =>
    if ← chance 2 then
      loot := loot.push (reward (← choose #[.highPitch, .blessingFromTheHeavens, .constellation,
        .seeTheWorldForWhatItIs, .loveIsInTheTrashSlot, .eyeOfTheSun]))
    if ← chance 2 then loot := loot.push (← bundle .cloud 50 101)
    if ← chance 40 then loot := loot.push (reward .fledglingWings)
  | .obsidianCrate | .hellstoneCrate =>
    if ← chance 2 then
      loot := loot.push (reward (← choose #[.handEarth, .darkness, .demonsEye, .flowingMagma,
        .darkSoulReaper, .impFace, .lakeOfFire, .trappedGhost, .ominousPresence,
        .shiningMoon, .skelehead, .livingGore]))
    for item in [Item.hellforge, .demonicHellcart, .ornateShadowKey, .sliceOfHellCake] do
      if ← chance 20 then loot := loot.push (reward item)
  | .defiledCrate | .hematicCrate | .divineCrate =>
    if ← chance 2 then
      loot := loot.push (← bundle (if crate == .divineCrate then .soulOfLight else .soulOfNight) 2 6)
    if ← chance 2 then
      let count ← sample 2 6
      if crate == .divineCrate then loot := loot.push (← bundle .crystalShard 4 11)
      else loot := loot.push ⟨if crate == .hematicCrate then .ichor else .cursedFlame, count⟩
  | _ => pure ()
  return loot

private def biomeLoot (crate : Item) : CrateRoll (Array Stack) := do
  let hardmode := ([.defiledCrate, .hematicCrate, .stockadeCrate, .azureCrate, .divineCrate, .brambleCrate,
    .borealCrate, .mirageCrate, .hellstoneCrate, .seasideCrate] : List Item).contains crate
  let mut loot ← biomeRoll crate true hardmode
  while loot.isEmpty do loot ← biomeRoll crate false hardmode
  if ← chance 4 then
    loot := loot.push (← supplies #[.obsidianSkinPotion, .spelunkerPotion, .hunterPotion,
      .gravitationPotion, .miningPotion, .heartreachPotion] 2 5)
  if ← chance 2 then loot := loot.push (← supplies #[.healingPotion, .manaPotion] 5 18)
  if ← chance 2 then loot := loot.push (← supplies #[.masterBait, .journeymanBait] 2 7)
  return loot ++ (← biomeBonus crate)

/-- Desktop 1.4.5.8 Player.OpenFishingCrate in an ordinary world.
The crate item fixes the loot pool even if the world later enters Hardmode. -/
def openFishingCrate (crate : Item) (rng : UnifiedRandom) : Array Stack :=
  let loot := match crate with
    | .goldenLockBox => do
      let item ← choose #[.handgun, .valor, .muramasa, .cobaltShield, .aquaScepter, .blueMoon, .magicMissile]
      let mut loot := #[reward item]
      if ← chance 3 then loot := loot.push (reward .shadowKey)
      if ← chance 8 then loot := loot.push (reward .ramRune)
      if ← chance 4 then loot := loot.push (reward .silverBracer)
      return loot
    | .obsidianLockBox => do
      let item ← choose #[.darkLance, .sunfury, .flowerOfFire, .flamelash, .hellwingBow]
      let loot := #[reward item]
      return if ← chance 5 then loot.push (reward .treasureMagnet) else loot
    | item => if item.biomeFishingTextureId.isSome && item.isFishingCrate then biomeLoot item
      else standardLoot item
  (loot.run rng).1

def Item.isCrateLoot (item : Item) : Bool :=
  item.isFishingCrate || item.biomeFishingTextureId.isSome ||
    ([.vilethorn, .musket, .bandOfStarpower, .shadowOrbPet, .ballOHurt, .undertaker, .rottedFork,
      .crimsonRod, .panicNecklace, .crimsonHeartPet, .blueDungeonBrick, .greenDungeonBrick, .pinkDungeonBrick,
      .hellstone, .hellstoneBar, .hellforge, .demonicHellcart, .ornateShadowKey, .sliceOfHellCake,
      .soulOfLight, .soulOfNight, .cursedFlame, .crystalShard, .handgun, .valor, .cobaltShield,
      .aquaScepter, .blueMoon, .magicMissile, .shadowKey, .darkLance, .sunfury, .flowerOfFire,
      .flamelash, .hellwingBow, .treasureMagnet] : List Item).contains item ||
    (copperOres ++ silverOres ++ goldOres ++ cobaltOres ++ mythrilOres ++ adamantiteOres ++
      copperBars ++ silverBars ++ cobaltBars ++ mythrilBars).contains item ||
    ([.goldBar, .platinumBar, .adamantiteBar, .titaniumBar, .aglet, .climbingClaws, .stepStool,
      .cordageGuide, .radar, .poisonBarb, .sailfishBoots, .tsunamiInABottle, .anchor, .extractinator,
      .enchantedSundial, .gingerBeard, .tartarSauce, .falconBlade, .hardySaddle, .enchantedSword,
      .obsidianSkinPotion, .swiftnessPotion, .ironskinPotion, .nightOwlPotion, .shinePotion,
      .hunterPotion, .gillsPotion, .miningPotion, .heartreachPotion, .trapsightPotion, .spelunkerPotion,
      .gravitationPotion, .calmingPotion, .flipperPotion, .lesserHealingPotion, .lesserManaPotion,
      .healingPotion, .manaPotion, .apprenticeBait, .journeymanBait, .masterBait, .lifeCrystal] : List Item).contains item

def standardFishingCrate (hardmode uncommon rare veryRare legendary : Bool) : Item :=
  if legendary || veryRare then if hardmode then .titaniumCrate else .goldenCrate
  else if uncommon || rare then if hardmode then .mythrilCrate else .ironCrate
  else if hardmode then .pearlwoodCrate else .woodenCrate

end Lean4Terria
