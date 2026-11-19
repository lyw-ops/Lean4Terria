import Lean4Terria.Ranged

namespace Lean4Terria

inductive Item where
  | basicPick | copperPick | dirt | stone | copper | crystal | wood | torch | stoneSword
  | ice | iron | crystalPick | ironSword | frostbrand
  | workbench | chest | door
  | silver | silverSword
  | bow | arrow | stormBow
  | amber | geode | draught
  | geodeMail | amberTreads | watchhorn | raidCrest
  | platform | homeBeacon | recallCharm
  | caveGel | frostShell | quarryScale | hunterBow
  | fishingRod | bait | pondPerch | caveEel
  | sunleafSeed | sunleaf | gillsElixir | barkskinTonic
  | ruinHammer | rail | minecart | grapplingHook | lifeCrystal | highlandBerry
  | deepsteel | highlandCrossbow | deepstoneAegis | obsidian
  | ironBar | chain | cobweb | silk | bed | furnace | ironAnvil | sawmill | loom | woodWall | woodHammer
  | copperShortsword | copperAxe | woodSword
  | tinOre | leadOre | tungstenOre | goldOre | platinumOre
  | copperBar | tinBar | leadBar | silverBar | tungstenBar | goldBar | platinumBar | leadAnvil
  | tinPick | ironPick | leadPick | silverPick | tungstenPick | goldPick | platinumPick
  | copperSword | tinSword | leadSword | tungstenSword | goldSword | platinumSword
  | tinAxe | ironAxe | leadAxe | silverAxe | tungstenAxe | goldAxe | platinumAxe
  | woodChair | woodTable | lens | blackLens | shackle | zombieArm
  | suspiciousLookingEye | demoniteOre | crimtaneOre | corruptSeeds | crimsonSeeds
  | lesserHealingPotion | binoculars | eyeMask | eyeTrophy | unholyArrow | demoniteBar | crimtaneBar
  | copperBow | tinBow | ironBow | leadBow | silverBow | tungstenBow | goldBow | platinumBow
  | demonBow | tendonBow | heart
  | ebonstone | crimstone | musket | undertaker | musketBall | bomb | stickyBomb
  | shadowOrbPet | crimsonHeartPet | vilethorn | ballOHurt | bandOfStarpower | panicNecklace | crimsonRod | rottedFork
  | rottenChunk | vertebra | vileMushroom | viciousMushroom | vilePowder | viciousPowder
  | wormFood | bloodySpine | shadowScale | tissueSample | nightmarePickaxe | deathbringerPickaxe
  | eaterMask | eaterTrophy | brainMask | brainTrophy | eatersBone | boneRattle | sand | glass | bottle
  | copperCoin | silverCoin | goldCoin | platinumCoin
  | blueDungeonBrick | greenDungeonBrick | pinkDungeonBrick | goldenKey | cobaltShield
  | skeletronMask | skeletronTrophy | chippysCouch | boneKey | healingPotion | skeletronHand | bookOfSkulls
  | bone | nazar | aquaScepter | blueMoon | magicMissile | valor | handgun
  | shadowKey | sunfury | darkLance | ashBlock | obsidianBrick | hellstone | shadowChest | obsidianSkull
  | flowerOfFire | flamelash | hellwingBow
  | obsidianRose | magmaStone | demonScythe | guideVoodooDoll | plumbersHat | batBat
  | treasureMagnet | demonicHellcart | ornateShadowKey | sliceOfHellCake | dynamite | meteoriteBar
  | hellfireArrow | silverBullet | tungstenBullet | restorationPotion | spelunkerPotion | featherfallPotion
  | manaRegenerationPotion | obsidianSkinPotion | magicPowerPotion | invisibilityPotion | hunterPotion | heartreachPotion
  | gravitationPotion | thornsPotion | waterWalkingPotion | battlePotion | teleportationPotion | infernoPotion
  | lifeforcePotion | recallPotion | potionOfReturn | glowstick | froggyNeckband | goatsTuft
  | oldCompanionLocket | catChime | dogCollar | turkeyWattleNecklace | meanGoblinSpikes | crowsBeak
  | balloonyBeads | grimOldBarb | vampirePendant | cowBell | chickenCharm | fairyChoker
  | hellforge | hellstoneBar | bugNet | lavaproofBugNet | hellButterfly | lavafly
  | magmaSnail | cascade | boneSerpentKite | goldenBugNet
  | pwnhammer | breakerBlade | clockworkAssaultRifle | laserRifle | firecracker
  | warriorEmblem | rangerEmblem | sorcererEmblem | summonerEmblem
  | wallOfFleshMask | demoniteBrick | crimtaneBrick | wallOfFleshTrophy
  | pearlstone | pearlsand | ebonsand | crimsand | purpleIce | pinkIce | redIce
  | cobaltOre | palladiumOre | mythrilOre | orichalcumOre | adamantiteOre | titaniumOre
  | moltenPickaxe | cobaltBar | palladiumBar | mythrilBar | orichalcumBar | adamantiteBar | titaniumBar
  | cobaltPickaxe | palladiumPickaxe | mythrilPickaxe | orichalcumPickaxe | adamantitePickaxe | titaniumPickaxe
  | cobaltDrill | palladiumDrill | mythrilDrill | orichalcumDrill | adamantiteDrill | titaniumDrill
  | mythrilAnvil | orichalcumAnvil | adamantiteForge | titaniumForge
  | soulOfNight | fastClock | trifoldMap
  | soulOfLight | crystalShard | mechanicalEye | mechanicalWorm | mechanicalSkull | gelatinCrystal
  | cursedFlame | vitamins | meatGrinder | applePie | crystallize | eaterOfLife | monsterMeat
  | soulOfSight | hallowedBar | greaterHealingPotion | twinMask | retinazerTrophy | spazmatismTrophy
  | bass | armoredCavefish | specularFish | goldenCarp
  | flarefinKoi | obsidifish | seaweed | tinCan | oldShoe | jojaCola
  | canOfWorms | worm | enchantedNightcrawler | goldWorm
  | woodenCrate | ironCrate | goldenCrate | pearlwoodCrate
  | mythrilCrate | titaniumCrate | apprenticeBait | journeymanBait
  | masterBait | aglet | climbingClaws | stepStool
  | cordageGuide | radar | poisonBarb | sailfishBoots
  | tsunamiInABottle | anchor | extractinator | enchantedSundial
  | gingerBeard | tartarSauce | falconBlade | hardySaddle
  | enchantedSword | swiftnessPotion | ironskinPotion | nightOwlPotion
  | shinePotion | gillsPotion | miningPotion | trapsightPotion
  | calmingPotion | flipperPotion | lesserManaPotion | manaPotion
  | corruptCrate | crimsonCrate | dungeonCrate | skyCrate
  | hallowedCrate | jungleCrate | defiledCrate | hematicCrate
  | stockadeCrate | azureCrate | divineCrate | brambleCrate
  | frozenCrate | borealCrate | oasisCrate | mirageCrate
  | obsidianCrate | hellstoneCrate | oceanCrate | seasideCrate
  | starfury | book | luckyHorseshoe | shinyRedBalloon
  | breathingReed | flipper | feralClaws | ankletOfTheWind
  | staffOfRegrowth | trident | fishPet | iceBoomerang
  | iceBlade | cloud | sandstormInABottle | waterWalkingBoots
  | lavaCharm | iceSkates | boomstick | blizzardInABottle
  | snowballCannon | ichor | bloodMoonRising | theHangedMan
  | gloryOfTheFire | boneWarp | skellingtonJSkellingsworth | theCursedMan
  | theEyeSeesTheEnd | somethingEvilIsWatchingYou | theTwinsHaveAwoken | theScreamer
  | goblinsPlayingPoker | dryadisque | impact | poweredByBirds
  | theDestroyer | thePersistencyOfEyes | unicornCrossingTheHallows | greatWave
  | starryNight | theGuardiansGaze | darkness | darkSoulReaper
  | trappedGhost | demonsEye | handEarth | skelehead
  | facingTheCerebralMastermind | lakeOfFire | trioSuperHeroes | impFace
  | ominousPresence | shiningMoon | livingGore | flowingMagma
  | theCreationOfTheGuide | flurryBoots | celestialMagnet | fiberglassFishingPole
  | obsidianSwordfish | cratePotion | sparky | flowerBoots
  | goldenLockBox | sturdyFossil | duneriderBoots | ancientChisel
  | stormSpear | thunderZapper | shellPile | snakeCharmersFlute
  | magicConch | bastStatue | innerTube | scarabBomb
  | sharkBait | scarabFishingRod | sandcastleBucket | bamboo
  | andrewSphinx | watchfulAntlion | burningSpirit | jawsOfDeath
  | theSandsOfSlime | snakesIHateSnakes | lifeAboveTheSand | oasis
  | prehistoryPreserved | ancientTablet | uluru | visitingThePyramids
  | bandageBoy | divineEye | demonConch | bottomlessLavaBucket
  | flameWakerBoots | wetBomb | superheatedBlood | hangingPot
  | miniVolcano | lavaAbsorbantSponge | obsidianLockBox | lavaSerpentBowl
  | lavaproofFishingHook | pottedMagmaPalm | pottedBrimstoneBush | pottedFireBrambles
  | pottedLavaBulb | pottedEmberTendrils | fledglingWings | highPitch
  | remnantsOfDevotion | constellation | blessingFromTheHeavens | loveIsInTheTrashSlot
  | seeTheWorldForWhatItIs | eyeOfTheSun | ofSeaAndDreams | glacierFang
  | seaweedPet | muramasa | ramRune | silverBracer | mudBlock
  | batfish | bumblebeeTuna | catfish | cloudfish
  | cursedfish | dirtfish | dynamiteFish | eaterofPlankton
  | fallenStarfish | theFishofCthulu | fishotron | harpyfish
  | hungerfish | ichorfish | jewelfish | mirageFish
  | mutantFlinxfin | pengfish | pixiefish | spiderfish
  | tundraTrout | unicornFish | guideVoodooFish | wyverntail
  | zombieFish | amanitaFungifin | angelfish | bloodyManowar
  | bonefish | bunnyfish | capnTunabeard | clownfish
  | demonicHellfish | derpfish | fishron | infectedScabbardfish
  | mudfish | slimefish | tropicalBarracuda | scarabFish
  | scorpioFish | goldenFishingRod | hotlineFishingHook | fuzzyCarrot
  | anglerHat | anglerVest | anglerPants | finWings
  | bottomlessWaterBucket | superAbsorbantSponge | anglerFishHook | fishMinecart
  | seashellHairpin | mermaidAdornment | mermaidTail | fishCostumeMask
  | fishCostumeShirt | fishCostumeFinskirt | highTestFishingLine | anglerEarring
  | tackleBox | fishermansGuide | weatherRadio | sextant
  | anglerBobber | fishingPotion | sonarPotion | bottomlessHoneyBucket
  | honeyAbsorbantSponge | lifePreserver | shipsWheel | compassRose
  | wallAnchor | pillaginMePixels | treasureMap | goldfishTrophy
  | bunnyfishTrophy | swordfishTrophy | sharkteethTrophy | shipInABottle
  | seaweedPlanter | notSoLostInParadise | crustography | fangs
  | whatLurksBelow | couchGag | silentFish | theDuke
  | trout | salmon | atlanticCod | tuna | redSnapper
  | neonTetra | damselfish | crimsonTigerfish | frostMinnow | princessFish
  | prismite | variegatedLardfish | doubleCod | shrimp | chaosFish
  | ebonkoi | hemopiranha | rockfish | stinkfish | purpleClubberfish
  | swordfish | reaverShark | sawtoothShark | zephyrFish | frogLeg
  | scalyTruffle | blueJellyfish | greenJellyfish | pinkJellyfish | bewitchingTable
  | alchemyTable | bombFish | frostDaggerfish | crystalSerpent | toxikarp
  | bladetongue | balloonPufferfish | flounder | rockLobster | oyster
  | ladyOfTheLake | barnacleStaff | brasilianSkies | itsScragglinTime
  | honeyfin | honeyBlock | crispyHoneyBlock
  | sparkleSlimeBalloon | queenSlimeMask | crystalAssassinHood | crystalAssassinShirt
  | crystalAssassinPants | bladeStaff | gelatinousPillion | hookOfDissonance | queenSlimeTrophy
  | soulOfMight | destroyerMask | destroyerTrophy
  | soulOfFright | skeletronPrimeMask | skeletronPrimeTrophy
  | templeKey | grenadeLauncher | rocketI | venusMagnum | nettleBurst | leafBlower | flowerPow | waspGun
  | seedler | vulgarDisplayOfFlower | pygmyStaff | thornHook | theAxe | seedling | planteraMask | planteraTrophy
  | lihzahrdBrick | lihzahrdDoor | lihzahrdChest | lihzahrdPowerCell | lihzahrdFurnace
  | solarTablet | solarTabletFragment
  | jestersArrow | flamingArrow | throwingKnife | archeryPotion | titanPotion
  | lihzahrdAltar | superDartTrap | flameTrap | spikyBallTrap | spearTrap | woodenSpike | lihzahrdPressurePlate
  | lizardEgg
  | beetleHusk | picksaw | stynger | styngerBolt | possessedHatchet | sunStone | eyeOfGolem | heatRay
  | staffOfEarth | golemFist | mobiusStrip | golemMask | golemTrophy
  deriving BEq, Repr, Inhabited

def Item.name : Item → String
  | .basicPick => "Basic pick"
  | .copperPick => "Copper Pickaxe"
  | .dirt => "Dirt"
  | .stone => "Stone"
  | .copper => "Copper ore"
  | .crystal => "Crystal"
  | .wood => "Wood"
  | .torch => "Torch"
  | .stoneSword => "Stone sword"
  | .ice => "Ice"
  | .iron => "Iron ore"
  | .crystalPick => "Crystal pick"
  | .ironSword => "Iron Broadsword"
  | .frostbrand => "Frostbrand"
  | .workbench => "Work Bench"
  | .chest => "Chest"
  | .door => "Wooden Door"
  | .silver => "Silver ore"
  | .silverSword => "Silver Broadsword"
  | .bow => "Wooden bow"
  | .arrow => "Wooden Arrow"
  | .stormBow => "Storm Bow"
  | .amber => "Amber"
  | .geode => "Geode"
  | .draught => "Healing draught"
  | .geodeMail => "Geode Mail"
  | .amberTreads => "Amber Treads"
  | .watchhorn => "Watchhorn"
  | .raidCrest => "Raid Crest"
  | .platform => "Wood platform"
  | .homeBeacon => "Home Beacon"
  | .recallCharm => "Recall Charm"
  | .caveGel => "Gel"
  | .frostShell => "Frost Shell"
  | .quarryScale => "Quarry Scale"
  | .hunterBow => "Hunter Bow"
  | .fishingRod => "Wood Fishing Pole"
  | .bait => "Fishing Bait"
  | .pondPerch => "Pond Perch"
  | .caveEel => "Cave Eel"
  | .bass => "Bass"
  | .armoredCavefish => "Armored Cavefish"
  | .specularFish => "Specular Fish"
  | .goldenCarp => "Golden Carp"
  | .flarefinKoi => "Flarefin Koi"
  | .obsidifish => "Obsidifish"
  | .seaweed => "Seaweed"
  | .tinCan => "Tin Can"
  | .oldShoe => "Old Shoe"
  | .jojaCola => "Joja Cola"
  | .canOfWorms => "Can Of Worms"
  | .worm => "Worm"
  | .enchantedNightcrawler => "Enchanted Nightcrawler"
  | .goldWorm => "Gold Worm"
  | .woodenCrate => "Wooden Crate"
  | .ironCrate => "Iron Crate"
  | .goldenCrate => "Golden Crate"
  | .pearlwoodCrate => "Pearlwood Crate"
  | .mythrilCrate => "Mythril Crate"
  | .titaniumCrate => "Titanium Crate"
  | .apprenticeBait => "Apprentice Bait"
  | .journeymanBait => "Journeyman Bait"
  | .masterBait => "Master Bait"
  | .aglet => "Aglet"
  | .climbingClaws => "Climbing Claws"
  | .stepStool => "Step Stool"
  | .cordageGuide => "Guide to Plant Fiber Cordage"
  | .radar => "Radar"
  | .poisonBarb => "Poison Barb"
  | .sailfishBoots => "Sailfish Boots"
  | .tsunamiInABottle => "Tsunami in a Bottle"
  | .anchor => "Anchor"
  | .extractinator => "Extractinator"
  | .enchantedSundial => "Enchanted Sundial"
  | .gingerBeard => "Ginger Beard"
  | .tartarSauce => "Tartar Sauce"
  | .falconBlade => "Falcon Blade"
  | .hardySaddle => "Hardy Saddle"
  | .enchantedSword => "Enchanted Sword"
  | .swiftnessPotion => "Swiftness Potion"
  | .ironskinPotion => "Ironskin Potion"
  | .nightOwlPotion => "Night Owl Potion"
  | .shinePotion => "Shine Potion"
  | .gillsPotion => "Gills Potion"
  | .miningPotion => "Mining Potion"
  | .trapsightPotion => "Trapsight Potion"
  | .calmingPotion => "Calming Potion"
  | .flipperPotion => "Flipper Potion"
  | .lesserManaPotion => "Lesser Mana Potion"
  | .manaPotion => "Mana Potion"
  | .corruptCrate => "Corrupt Crate"
  | .crimsonCrate => "Crimson Crate"
  | .dungeonCrate => "Dungeon Crate"
  | .skyCrate => "Sky Crate"
  | .hallowedCrate => "Hallowed Crate"
  | .jungleCrate => "Jungle Crate"
  | .defiledCrate => "Defiled Crate"
  | .hematicCrate => "Hematic Crate"
  | .stockadeCrate => "Stockade Crate"
  | .azureCrate => "Azure Crate"
  | .divineCrate => "Divine Crate"
  | .brambleCrate => "Bramble Crate"
  | .frozenCrate => "Frozen Crate"
  | .borealCrate => "Boreal Crate"
  | .oasisCrate => "Oasis Crate"
  | .mirageCrate => "Mirage Crate"
  | .obsidianCrate => "Obsidian Crate"
  | .hellstoneCrate => "Hellstone Crate"
  | .oceanCrate => "Ocean Crate"
  | .seasideCrate => "Seaside Crate"
  | .starfury => "Starfury"
  | .book => "Book"
  | .luckyHorseshoe => "Lucky Horseshoe"
  | .shinyRedBalloon => "Shiny Red Balloon"
  | .breathingReed => "Breathing Reed"
  | .flipper => "Flipper"
  | .feralClaws => "Feral Claws"
  | .ankletOfTheWind => "Anklet of the Wind"
  | .staffOfRegrowth => "Staff of Regrowth"
  | .trident => "Trident"
  | .fishPet => "Fish"
  | .iceBoomerang => "Ice Boomerang"
  | .iceBlade => "Ice Blade"
  | .cloud => "Cloud"
  | .sandstormInABottle => "Sandstorm in a Bottle"
  | .waterWalkingBoots => "Water Walking Boots"
  | .lavaCharm => "Lava Charm"
  | .iceSkates => "Ice Skates"
  | .boomstick => "Boomstick"
  | .blizzardInABottle => "Blizzard in a Bottle"
  | .snowballCannon => "Snowball Cannon"
  | .ichor => "Ichor"
  | .bloodMoonRising => "Blood Moon Rising"
  | .theHangedMan => "The Hanged Man"
  | .gloryOfTheFire => "Glory of the Fire"
  | .boneWarp => "Bone Warp"
  | .skellingtonJSkellingsworth => "Skellington J Skellingsworth"
  | .theCursedMan => "The Cursed Man"
  | .theEyeSeesTheEnd => "The Eye Sees the End"
  | .somethingEvilIsWatchingYou => "Something Evil is Watching You"
  | .theTwinsHaveAwoken => "The Twins Have Awoken"
  | .theScreamer => "The Screamer"
  | .goblinsPlayingPoker => "Goblins Playing Poker"
  | .dryadisque => "Dryadisque"
  | .impact => "Impact"
  | .poweredByBirds => "Powered by Birds"
  | .theDestroyer => "The Destroyer"
  | .thePersistencyOfEyes => "The Persistency of Eyes"
  | .unicornCrossingTheHallows => "Unicorn Crossing the Hallows"
  | .greatWave => "Great Wave"
  | .starryNight => "Starry Night"
  | .theGuardiansGaze => "The Guardian's Gaze"
  | .darkness => "Darkness"
  | .darkSoulReaper => "Dark Soul Reaper"
  | .trappedGhost => "Trapped Ghost"
  | .demonsEye => "Demon's Eye"
  | .handEarth => "Hand Earth"
  | .skelehead => "Skelehead"
  | .facingTheCerebralMastermind => "Facing the Cerebral Mastermind"
  | .lakeOfFire => "Lake of Fire"
  | .trioSuperHeroes => "Trio Super Heroes"
  | .impFace => "Imp Face"
  | .ominousPresence => "Ominous Presence"
  | .shiningMoon => "Shining Moon"
  | .livingGore => "Living Gore"
  | .flowingMagma => "Flowing Magma"
  | .theCreationOfTheGuide => "The Creation of the Guide"
  | .flurryBoots => "Flurry Boots"
  | .celestialMagnet => "Celestial Magnet"
  | .fiberglassFishingPole => "Fiberglass Fishing Pole"
  | .obsidianSwordfish => "Obsidian Swordfish"
  | .cratePotion => "Crate Potion"
  | .sparky => "Sparky"
  | .flowerBoots => "Flower Boots"
  | .goldenLockBox => "Golden Lock Box"
  | .sturdyFossil => "Sturdy Fossil"
  | .duneriderBoots => "Dunerider Boots"
  | .ancientChisel => "Ancient Chisel"
  | .stormSpear => "Storm Spear"
  | .thunderZapper => "Thunder Zapper"
  | .shellPile => "Shell Pile"
  | .snakeCharmersFlute => "Snake Charmer's Flute"
  | .magicConch => "Magic Conch"
  | .bastStatue => "Bast Statue"
  | .innerTube => "Inner Tube"
  | .scarabBomb => "Scarab Bomb"
  | .sharkBait => "Shark Bait"
  | .scarabFishingRod => "Scarab Fishing Rod"
  | .sandcastleBucket => "Sandcastle Bucket"
  | .bamboo => "Bamboo"
  | .andrewSphinx => "Andrew Sphinx"
  | .watchfulAntlion => "Watchful Antlion"
  | .burningSpirit => "Burning Spirit"
  | .jawsOfDeath => "Jaws of Death"
  | .theSandsOfSlime => "The Sands of Slime"
  | .snakesIHateSnakes => "Snakes, I Hate Snakes"
  | .lifeAboveTheSand => "Life Above the Sand"
  | .oasis => "Oasis"
  | .prehistoryPreserved => "Prehistory Preserved"
  | .ancientTablet => "Ancient Tablet"
  | .uluru => "Uluru"
  | .visitingThePyramids => "Visiting the Pyramids"
  | .bandageBoy => "Bandage Boy"
  | .divineEye => "Divine Eye"
  | .demonConch => "Demon Conch"
  | .bottomlessLavaBucket => "Bottomless Lava Bucket"
  | .flameWakerBoots => "Flame Waker Boots"
  | .wetBomb => "Wet Bomb"
  | .superheatedBlood => "Superheated Blood"
  | .hangingPot => "Hanging Pot"
  | .miniVolcano => "Mini Volcano"
  | .lavaAbsorbantSponge => "Lava Absorbant Sponge"
  | .obsidianLockBox => "Obsidian Lock Box"
  | .lavaSerpentBowl => "Lava Serpent Bowl"
  | .lavaproofFishingHook => "Lavaproof Fishing Hook"
  | .pottedMagmaPalm => "Potted Magma Palm"
  | .pottedBrimstoneBush => "Potted Brimstone Bush"
  | .pottedFireBrambles => "Potted Fire Brambles"
  | .pottedLavaBulb => "Potted Lava Bulb"
  | .pottedEmberTendrils => "Potted Ember Tendrils"
  | .fledglingWings => "Fledgling Wings"
  | .highPitch => "High Pitch"
  | .remnantsOfDevotion => "Remnants of Devotion"
  | .constellation => "Constellation"
  | .blessingFromTheHeavens => "Blessing from the Heavens"
  | .loveIsInTheTrashSlot => "Love is in the Trash Slot"
  | .seeTheWorldForWhatItIs => "See The World For What It Is"
  | .eyeOfTheSun => "Eye of the Sun"
  | .ofSeaAndDreams => "Of Sea and Dreams"
  | .glacierFang => "Glacier Fang"
  | .seaweedPet => "Seaweed"
  | .muramasa => "Muramasa"
  | .ramRune => "Ram Rune"
  | .silverBracer => "Silver Bracer"
  | .mudBlock => "Mud Block"
  | .batfish => "Batfish"
  | .bumblebeeTuna => "Bumblebee Tuna"
  | .catfish => "Catfish"
  | .cloudfish => "Cloudfish"
  | .cursedfish => "Cursedfish"
  | .dirtfish => "Dirtfish"
  | .dynamiteFish => "Dynamite Fish"
  | .eaterofPlankton => "Eater of Plankton"
  | .fallenStarfish => "Fallen Starfish"
  | .theFishofCthulu => "The Fish of Cthulhu"
  | .fishotron => "Fishotron"
  | .harpyfish => "Harpyfish"
  | .hungerfish => "Hungerfish"
  | .ichorfish => "Ichorfish"
  | .jewelfish => "Jewelfish"
  | .mirageFish => "Mirage Fish"
  | .mutantFlinxfin => "Mutant Flinxfin"
  | .pengfish => "Pengfish"
  | .pixiefish => "Pixiefish"
  | .spiderfish => "Spiderfish"
  | .tundraTrout => "Tundra Trout"
  | .unicornFish => "Unicorn Fish"
  | .guideVoodooFish => "Guide Voodoo Fish"
  | .wyverntail => "Wyverntail"
  | .zombieFish => "Zombie Fish"
  | .amanitaFungifin => "Amanita Fungifin"
  | .angelfish => "Angelfish"
  | .bloodyManowar => "Bloody Manowar"
  | .bonefish => "Bonefish"
  | .bunnyfish => "Bunnyfish"
  | .capnTunabeard => "Cap'n Tunabeard"
  | .clownfish => "Clownfish"
  | .demonicHellfish => "Demonic Hellfish"
  | .derpfish => "Derpfish"
  | .fishron => "Fishron"
  | .infectedScabbardfish => "Infected Scabbardfish"
  | .mudfish => "Mudfish"
  | .slimefish => "Slimefish"
  | .tropicalBarracuda => "Tropical Barracuda"
  | .scarabFish => "Scarab Fish"
  | .scorpioFish => "Scorpio Fish"
  | .goldenFishingRod => "Golden Fishing Rod"
  | .hotlineFishingHook => "Hotline Fishing Hook"
  | .fuzzyCarrot => "Fuzzy Carrot"
  | .anglerHat => "Angler Hat"
  | .anglerVest => "Angler Vest"
  | .anglerPants => "Angler Pants"
  | .finWings => "Fin Wings"
  | .bottomlessWaterBucket => "Bottomless Water Bucket"
  | .superAbsorbantSponge => "Super Absorbant Sponge"
  | .anglerFishHook => "Fish Hook"
  | .fishMinecart => "Minecarp"
  | .seashellHairpin => "Seashell Hairpin"
  | .mermaidAdornment => "Mermaid Adornment"
  | .mermaidTail => "Mermaid Tail"
  | .fishCostumeMask => "Fish Costume Mask"
  | .fishCostumeShirt => "Fish Costume Shirt"
  | .fishCostumeFinskirt => "Fish Costume Finskirt"
  | .highTestFishingLine => "High Test Fishing Line"
  | .anglerEarring => "Angler Earring"
  | .tackleBox => "Tackle Box"
  | .fishermansGuide => "Fisherman's Pocket Guide"
  | .weatherRadio => "Weather Radio"
  | .sextant => "Sextant"
  | .anglerBobber => "Fishing Bobber"
  | .fishingPotion => "Fishing Potion"
  | .sonarPotion => "Sonar Potion"
  | .bottomlessHoneyBucket => "Bottomless Honey Bucket"
  | .honeyAbsorbantSponge => "Honey Absorbant Sponge"
  | .lifePreserver => "Life Preserver"
  | .shipsWheel => "Ship's Wheel"
  | .compassRose => "Compass Rose"
  | .wallAnchor => "Wall Anchor"
  | .pillaginMePixels => "Pillagin' Me Pixels"
  | .treasureMap => "Treasure Map"
  | .goldfishTrophy => "Goldfish Trophy"
  | .bunnyfishTrophy => "Bunnyfish Trophy"
  | .swordfishTrophy => "Swordfish Trophy"
  | .sharkteethTrophy => "Sharkteeth Trophy"
  | .shipInABottle => "Ship in a Bottle"
  | .seaweedPlanter => "Seaweed Planter"
  | .notSoLostInParadise => "Not So Lost In Paradise"
  | .crustography => "Crustography"
  | .fangs => "Fangs"
  | .whatLurksBelow => "What Lurks Below"
  | .couchGag => "Couch Gag"
  | .silentFish => "Silent Fish"
  | .theDuke => "The Duke"
  | .trout => "Trout"
  | .salmon => "Salmon"
  | .atlanticCod => "Atlantic Cod"
  | .tuna => "Tuna"
  | .redSnapper => "Red Snapper"
  | .neonTetra => "Neon Tetra"
  | .damselfish => "Damselfish"
  | .crimsonTigerfish => "Crimson Tigerfish"
  | .frostMinnow => "Frost Minnow"
  | .princessFish => "Princess Fish"
  | .prismite => "Prismite"
  | .variegatedLardfish => "Variegated Lardfish"
  | .doubleCod => "Double Cod"
  | .shrimp => "Shrimp"
  | .chaosFish => "Chaos Fish"
  | .ebonkoi => "Ebonkoi"
  | .hemopiranha => "Hemopiranha"
  | .rockfish => "Rockfish"
  | .stinkfish => "Stinkfish"
  | .purpleClubberfish => "Purple Clubberfish"
  | .swordfish => "Swordfish"
  | .reaverShark => "Reaver Shark"
  | .sawtoothShark => "Sawtooth Shark"
  | .zephyrFish => "Zephyr Fish"
  | .frogLeg => "Frog Leg"
  | .scalyTruffle => "Scaly Truffle"
  | .blueJellyfish => "Blue Jellyfish"
  | .greenJellyfish => "Green Jellyfish"
  | .pinkJellyfish => "Pink Jellyfish"
  | .bewitchingTable => "Bewitching Table"
  | .alchemyTable => "Alchemy Table"
  | .bombFish => "Bomb Fish"
  | .frostDaggerfish => "Frost Daggerfish"
  | .crystalSerpent => "Crystal Serpent"
  | .toxikarp => "Toxikarp"
  | .bladetongue => "Bladetongue"
  | .balloonPufferfish => "Balloon Pufferfish"
  | .flounder => "Flounder"
  | .rockLobster => "Rock Lobster"
  | .oyster => "Oyster"
  | .ladyOfTheLake => "Lady Of The Lake"
  | .barnacleStaff => "Barnacle Staff"
  | .brasilianSkies => "Brasilian Skies"
  | .itsScragglinTime => "It's Scragglin' Time"
  | .honeyfin => "Honeyfin"
  | .honeyBlock => "Honey Block"
  | .crispyHoneyBlock => "Crispy Honey Block"
  | .sparkleSlimeBalloon => "Sparkle Slime Balloon"
  | .queenSlimeMask => "Queen Slime Mask"
  | .crystalAssassinHood => "Crystal Assassin Hood"
  | .crystalAssassinShirt => "Crystal Assassin Shirt"
  | .crystalAssassinPants => "Crystal Assassin Pants"
  | .bladeStaff => "Blade Staff"
  | .soulOfMight => "Soul of Might"
  | .destroyerMask => "Destroyer Mask"
  | .destroyerTrophy => "Destroyer Trophy"
  | .soulOfFright => "Soul of Fright"
  | .skeletronPrimeMask => "Skeletron Prime Mask"
  | .skeletronPrimeTrophy => "Skeletron Prime Trophy"
  | .templeKey => "Temple Key"
  | .grenadeLauncher => "Grenade Launcher"
  | .rocketI => "Rocket I"
  | .venusMagnum => "Venus Magnum"
  | .nettleBurst => "Nettle Burst"
  | .leafBlower => "Leaf Blower"
  | .flowerPow => "Flower Pow"
  | .waspGun => "Wasp Gun"
  | .seedler => "Seedler"
  | .vulgarDisplayOfFlower => "Vulgar Display of Flower"
  | .pygmyStaff => "Pygmy Staff"
  | .thornHook => "Thorn Hook"
  | .theAxe => "The Axe"
  | .seedling => "Seedling"
  | .planteraMask => "Plantera Mask"
  | .planteraTrophy => "Plantera Trophy"
  | .lihzahrdBrick => "Lihzahrd Brick"
  | .lihzahrdDoor => "Lihzahrd Door"
  | .lihzahrdChest => "Lihzahrd Chest"
  | .lihzahrdPowerCell => "Lihzahrd Power Cell"
  | .lihzahrdFurnace => "Lihzahrd Furnace"
  | .solarTablet => "Solar Tablet"
  | .solarTabletFragment => "Solar Tablet Fragment"
  | .jestersArrow => "Jester's Arrow"
  | .flamingArrow => "Flaming Arrow"
  | .throwingKnife => "Throwing Knife"
  | .archeryPotion => "Archery Potion"
  | .titanPotion => "Titan Potion"
  | .lihzahrdAltar => "Lihzahrd Altar"
  | .superDartTrap => "Super Dart Trap"
  | .flameTrap => "Flame Trap"
  | .spikyBallTrap => "Spiky Ball Trap"
  | .spearTrap => "Spear Trap"
  | .woodenSpike => "Wooden Spike"
  | .lihzahrdPressurePlate => "Lihzahrd Pressure Plate"
  | .beetleHusk => "Beetle Husk"
  | .picksaw => "Picksaw"
  | .stynger => "Stynger"
  | .styngerBolt => "Stynger Bolt"
  | .possessedHatchet => "Possessed Hatchet"
  | .sunStone => "Sun Stone"
  | .eyeOfGolem => "Eye of the Golem"
  | .heatRay => "Heat Ray"
  | .staffOfEarth => "Staff of Earth"
  | .golemFist => "Golem Fist"
  | .mobiusStrip => "Mobius Strip"
  | .golemMask => "Golem Mask"
  | .golemTrophy => "Golem Trophy"
  | .lizardEgg => "Lizard Egg"
  | .gelatinousPillion => "Gelatinous Pillion"
  | .hookOfDissonance => "Hook of Dissonance"
  | .queenSlimeTrophy => "Queen Slime Trophy"
  | .sunleafSeed => "Sunleaf Seed"
  | .sunleaf => "Sunleaf"
  | .gillsElixir => "Gills Elixir"
  | .barkskinTonic => "Barkskin Tonic"
  | .ruinHammer => "Ruin Hammer"
  | .rail => "Rail"
  | .minecart => "Minecart"
  | .grapplingHook => "Grappling Hook"
  | .lifeCrystal => "Life Crystal"
  | .highlandBerry => "Highland Berry"
  | .deepsteel => "Deepsteel ore"
  | .highlandCrossbow => "Highland Crossbow"
  | .deepstoneAegis => "Deepstone Aegis"
  | .obsidian => "Obsidian"
  | .ironBar => "Iron Bar"
  | .chain => "Chain"
  | .cobweb => "Cobweb"
  | .silk => "Silk"
  | .bed => "Bed"
  | .furnace => "Furnace"
  | .ironAnvil => "Iron Anvil"
  | .sawmill => "Sawmill"
  | .loom => "Loom"
  | .woodWall => "Wood Wall"
  | .woodHammer => "Wooden Hammer"
  | .copperShortsword => "Copper Shortsword"
  | .copperAxe => "Copper Axe"
  | .woodSword => "Wooden Sword"
  | .tinOre => "Tin Ore"
  | .leadOre => "Lead Ore"
  | .tungstenOre => "Tungsten Ore"
  | .goldOre => "Gold Ore"
  | .platinumOre => "Platinum Ore"
  | .copperBar => "Copper Bar"
  | .tinBar => "Tin Bar"
  | .leadBar => "Lead Bar"
  | .silverBar => "Silver Bar"
  | .tungstenBar => "Tungsten Bar"
  | .goldBar => "Gold Bar"
  | .platinumBar => "Platinum Bar"
  | .leadAnvil => "Lead Anvil"
  | .tinPick => "Tin Pickaxe"
  | .ironPick => "Iron Pickaxe"
  | .leadPick => "Lead Pickaxe"
  | .silverPick => "Silver Pickaxe"
  | .tungstenPick => "Tungsten Pickaxe"
  | .goldPick => "Gold Pickaxe"
  | .platinumPick => "Platinum Pickaxe"
  | .copperSword => "Copper Broadsword"
  | .tinSword => "Tin Broadsword"
  | .leadSword => "Lead Broadsword"
  | .tungstenSword => "Tungsten Broadsword"
  | .goldSword => "Gold Broadsword"
  | .platinumSword => "Platinum Broadsword"
  | .tinAxe => "Tin Axe"
  | .ironAxe => "Iron Axe"
  | .leadAxe => "Lead Axe"
  | .silverAxe => "Silver Axe"
  | .tungstenAxe => "Tungsten Axe"
  | .goldAxe => "Gold Axe"
  | .platinumAxe => "Platinum Axe"
  | .woodChair => "Wooden Chair"
  | .woodTable => "Wooden Table"
  | .lens => "Lens"
  | .blackLens => "Black Lens"
  | .shackle => "Shackle"
  | .zombieArm => "Zombie Arm"
  | .suspiciousLookingEye => "Suspicious Looking Eye"
  | .demoniteOre => "Demonite Ore"
  | .crimtaneOre => "Crimtane Ore"
  | .corruptSeeds => "Corrupt Seeds"
  | .crimsonSeeds => "Crimson Seeds"
  | .lesserHealingPotion => "Lesser Healing Potion"
  | .binoculars => "Binoculars"
  | .eyeMask => "Eye of Cthulhu Mask"
  | .eyeTrophy => "Eye of Cthulhu Trophy"
  | .unholyArrow => "Unholy Arrow"
  | .demoniteBar => "Demonite Bar"
  | .crimtaneBar => "Crimtane Bar"
  | .copperBow => "Copper Bow"
  | .tinBow => "Tin Bow"
  | .ironBow => "Iron Bow"
  | .leadBow => "Lead Bow"
  | .silverBow => "Silver Bow"
  | .tungstenBow => "Tungsten Bow"
  | .goldBow => "Gold Bow"
  | .platinumBow => "Platinum Bow"
  | .demonBow => "Demon Bow"
  | .tendonBow => "Tendon Bow"
  | .heart => "Heart"
  | .ebonstone => "Ebonstone Block"
  | .crimstone => "Crimstone Block"
  | .musket => "Musket"
  | .undertaker => "The Undertaker"
  | .musketBall => "Musket Ball"
  | .bomb => "Bomb"
  | .stickyBomb => "Sticky Bomb"
  | .shadowOrbPet => "Shadow Orb"
  | .crimsonHeartPet => "Crimson Heart"
  | .vilethorn => "Vilethorn"
  | .ballOHurt => "Ball O' Hurt"
  | .bandOfStarpower => "Band of Starpower"
  | .panicNecklace => "Panic Necklace"
  | .crimsonRod => "Crimson Rod"
  | .rottedFork => "The Rotted Fork"
  | .rottenChunk => "Rotten Chunk"
  | .vertebra => "Vertebra"
  | .vileMushroom => "Vile Mushroom"
  | .viciousMushroom => "Vicious Mushroom"
  | .vilePowder => "Vile Powder"
  | .viciousPowder => "Vicious Powder"
  | .wormFood => "Worm Food"
  | .bloodySpine => "Bloody Spine"
  | .shadowScale => "Shadow Scale"
  | .tissueSample => "Tissue Sample"
  | .nightmarePickaxe => "Nightmare Pickaxe"
  | .deathbringerPickaxe => "Deathbringer Pickaxe"
  | .eaterMask => "Eater of Worlds Mask"
  | .eaterTrophy => "Eater of Worlds Trophy"
  | .brainMask => "Brain of Cthulhu Mask"
  | .brainTrophy => "Brain of Cthulhu Trophy"
  | .eatersBone => "Eater's Bone"
  | .boneRattle => "Bone Rattle"
  | .sand => "Sand Block"
  | .glass => "Glass"
  | .bottle => "Bottle"
  | .copperCoin => "Copper Coin"
  | .silverCoin => "Silver Coin"
  | .goldCoin => "Gold Coin"
  | .platinumCoin => "Platinum Coin"
  | .blueDungeonBrick => "Blue Brick"
  | .greenDungeonBrick => "Green Brick"
  | .pinkDungeonBrick => "Pink Brick"
  | .goldenKey => "Golden Key"
  | .cobaltShield => "Cobalt Shield"
  | .skeletronMask => "Skeletron Mask"
  | .skeletronTrophy => "Skeletron Trophy"
  | .chippysCouch => "Chippy's Couch"
  | .boneKey => "Bone Key"
  | .healingPotion => "Healing Potion"
  | .skeletronHand => "Skeletron Hand"
  | .bookOfSkulls => "Book of Skulls"
  | .bone => "Bone"
  | .nazar => "Nazar"
  | .aquaScepter => "Aqua Scepter"
  | .blueMoon => "Blue Moon"
  | .magicMissile => "Magic Missile"
  | .valor => "Valor"
  | .handgun => "Handgun"
  | .shadowKey => "Shadow Key"
  | .sunfury => "Sunfury"
  | .darkLance => "Dark Lance"
  | .ashBlock => "Ash Block"
  | .obsidianBrick => "Obsidian Brick"
  | .hellstone => "Hellstone"
  | .shadowChest => "Shadow Chest"
  | .obsidianSkull => "Obsidian Skull"
  | .flowerOfFire => "Flower of Fire"
  | .flamelash => "Flamelash"
  | .hellwingBow => "Hellwing Bow"
  | .obsidianRose => "Obsidian Rose"
  | .magmaStone => "Magma Stone"
  | .demonScythe => "Demon Scythe"
  | .guideVoodooDoll => "Guide Voodoo Doll"
  | .plumbersHat => "Plumber's Hat"
  | .batBat => "Bat Bat"
  | .treasureMagnet => "Treasure Magnet"
  | .demonicHellcart => "Demonic Hellcart"
  | .ornateShadowKey => "Ornate Shadow Key"
  | .sliceOfHellCake => "Slice of Hell Cake"
  | .dynamite => "Dynamite"
  | .meteoriteBar => "Meteorite Bar"
  | .hellfireArrow => "Hellfire Arrow"
  | .silverBullet => "Silver Bullet"
  | .tungstenBullet => "Tungsten Bullet"
  | .restorationPotion => "Restoration Potion"
  | .spelunkerPotion => "Spelunker Potion"
  | .featherfallPotion => "Featherfall Potion"
  | .manaRegenerationPotion => "Mana Regeneration Potion"
  | .obsidianSkinPotion => "Obsidian Skin Potion"
  | .magicPowerPotion => "Magic Power Potion"
  | .invisibilityPotion => "Invisibility Potion"
  | .hunterPotion => "Hunter Potion"
  | .heartreachPotion => "Heartreach Potion"
  | .gravitationPotion => "Gravitation Potion"
  | .thornsPotion => "Thorns Potion"
  | .waterWalkingPotion => "Water Walking Potion"
  | .battlePotion => "Battle Potion"
  | .teleportationPotion => "Teleportation Potion"
  | .infernoPotion => "Inferno Potion"
  | .lifeforcePotion => "Lifeforce Potion"
  | .recallPotion => "Recall Potion"
  | .potionOfReturn => "Potion of Return"
  | .glowstick => "Glowstick"
  | .froggyNeckband => "Froggy Neckband"
  | .goatsTuft => "Goat's Tuft"
  | .oldCompanionLocket => "Old Companion Locket"
  | .catChime => "Cat Chime"
  | .dogCollar => "Dog Collar"
  | .turkeyWattleNecklace => "Turkey Wattle Necklace"
  | .meanGoblinSpikes => "Mean Goblin's Spikes"
  | .crowsBeak => "Crow's Beak"
  | .balloonyBeads => "Balloony Beads"
  | .grimOldBarb => "Grim Old Barb"
  | .vampirePendant => "Vampire Pendant"
  | .cowBell => "Cow Bell"
  | .chickenCharm => "Chicken Charm"
  | .fairyChoker => "Fairy Choker"
  | .hellforge => "Hellforge"
  | .hellstoneBar => "Hellstone Bar"
  | .bugNet => "Bug Net"
  | .lavaproofBugNet => "Lavaproof Bug Net"
  | .hellButterfly => "Hell Butterfly"
  | .lavafly => "Lavafly"
  | .magmaSnail => "Magma Snail"
  | .cascade => "Cascade"
  | .boneSerpentKite => "Bone Serpent Kite"
  | .goldenBugNet => "Golden Bug Net"
  | .pwnhammer => "Pwnhammer"
  | .breakerBlade => "Breaker Blade"
  | .clockworkAssaultRifle => "Clockwork Assault Rifle"
  | .laserRifle => "Laser Rifle"
  | .firecracker => "Firecracker"
  | .warriorEmblem => "Warrior Emblem"
  | .rangerEmblem => "Ranger Emblem"
  | .sorcererEmblem => "Sorcerer Emblem"
  | .summonerEmblem => "Summoner Emblem"
  | .wallOfFleshMask => "Wall of Flesh Mask"
  | .demoniteBrick => "Demonite Brick"
  | .crimtaneBrick => "Crimtane Brick"
  | .wallOfFleshTrophy => "Wall of Flesh Trophy"
  | .pearlstone => "Pearlstone Block"
  | .pearlsand => "Pearlsand Block"
  | .ebonsand => "Ebonsand Block"
  | .crimsand => "Crimsand Block"
  | .purpleIce => "Purple Ice Block"
  | .pinkIce => "Pink Ice Block"
  | .redIce => "Red Ice Block"
  | .cobaltOre => "Cobalt Ore"
  | .palladiumOre => "Palladium Ore"
  | .mythrilOre => "Mythril Ore"
  | .orichalcumOre => "Orichalcum Ore"
  | .adamantiteOre => "Adamantite Ore"
  | .titaniumOre => "Titanium Ore"
  | .moltenPickaxe => "Molten Pickaxe"
  | .cobaltBar => "Cobalt Bar"
  | .palladiumBar => "Palladium Bar"
  | .mythrilBar => "Mythril Bar"
  | .orichalcumBar => "Orichalcum Bar"
  | .adamantiteBar => "Adamantite Bar"
  | .titaniumBar => "Titanium Bar"
  | .cobaltPickaxe => "Cobalt Pickaxe"
  | .palladiumPickaxe => "Palladium Pickaxe"
  | .mythrilPickaxe => "Mythril Pickaxe"
  | .orichalcumPickaxe => "Orichalcum Pickaxe"
  | .adamantitePickaxe => "Adamantite Pickaxe"
  | .titaniumPickaxe => "Titanium Pickaxe"
  | .cobaltDrill => "Cobalt Drill"
  | .palladiumDrill => "Palladium Drill"
  | .mythrilDrill => "Mythril Drill"
  | .orichalcumDrill => "Orichalcum Drill"
  | .adamantiteDrill => "Adamantite Drill"
  | .titaniumDrill => "Titanium Drill"
  | .mythrilAnvil => "Mythril Anvil"
  | .orichalcumAnvil => "Orichalcum Anvil"
  | .adamantiteForge => "Adamantite Forge"
  | .titaniumForge => "Titanium Forge"
  | .soulOfNight => "Soul of Night"
  | .fastClock => "Fast Clock"
  | .trifoldMap => "Trifold Map"
  | .soulOfLight => "Soul of Light"
  | .crystalShard => "Crystal Shard"
  | .mechanicalEye => "Mechanical Eye"
  | .mechanicalWorm => "Mechanical Worm"
  | .mechanicalSkull => "Mechanical Skull"
  | .gelatinCrystal => "Gelatin Crystal"
  | .cursedFlame => "Cursed Flame"
  | .vitamins => "Vitamins"
  | .meatGrinder => "Meat Grinder"
  | .applePie => "Apple Pie"
  | .crystallize => "Crystallize"
  | .eaterOfLife => "Eater Of Life"
  | .monsterMeat => "Monster Meat"
  | .soulOfSight => "Soul of Sight"
  | .hallowedBar => "Hallowed Bar"
  | .greaterHealingPotion => "Greater Healing Potion"
  | .twinMask => "Twin Mask"
  | .retinazerTrophy => "Retinazer Trophy"
  | .spazmatismTrophy => "Spazmatism Trophy"

def Item.code : Item → UInt16
  | .basicPick => 1
  | .copperPick => 2
  | .dirt => 3
  | .stone => 4
  | .copper => 5
  | .crystal => 6
  | .wood => 7
  | .torch => 8
  | .stoneSword => 9
  | .ice => 10
  | .iron => 11
  | .crystalPick => 12
  | .ironSword => 13
  | .frostbrand => 14
  | .workbench => 15
  | .chest => 16
  | .door => 17
  | .silver => 18
  | .silverSword => 19
  | .bow => 20
  | .arrow => 21
  | .stormBow => 22
  | .amber => 23
  | .geode => 24
  | .draught => 25
  | .geodeMail => 26
  | .amberTreads => 27
  | .watchhorn => 28
  | .raidCrest => 29
  | .platform => 30
  | .homeBeacon => 31
  | .recallCharm => 32
  | .caveGel => 33
  | .frostShell => 34
  | .quarryScale => 35
  | .hunterBow => 36
  | .fishingRod => 37
  | .bait => 38
  | .pondPerch => 39
  | .caveEel => 40
  | .sunleafSeed => 41
  | .sunleaf => 42
  | .gillsElixir => 43
  | .barkskinTonic => 44
  | .ruinHammer => 45
  | .rail => 46
  | .minecart => 47
  | .grapplingHook => 48
  | .lifeCrystal => 49
  | .highlandBerry => 50
  | .deepsteel => 51
  | .highlandCrossbow => 52
  | .deepstoneAegis => 53
  | .obsidian => 54
  | .ironBar => 55
  | .chain => 56
  | .cobweb => 57
  | .silk => 58
  | .bed => 59
  | .furnace => 60
  | .ironAnvil => 61
  | .sawmill => 62
  | .loom => 63
  | .woodWall => 64
  | .woodHammer => 65
  | .copperShortsword => 66
  | .copperAxe => 67
  | .woodSword => 68
  | .tinOre => 69
  | .leadOre => 70
  | .tungstenOre => 71
  | .goldOre => 72
  | .platinumOre => 73
  | .copperBar => 74
  | .tinBar => 75
  | .leadBar => 76
  | .silverBar => 77
  | .tungstenBar => 78
  | .goldBar => 79
  | .platinumBar => 80
  | .leadAnvil => 81
  | .tinPick => 82
  | .ironPick => 83
  | .leadPick => 84
  | .silverPick => 85
  | .tungstenPick => 86
  | .goldPick => 87
  | .platinumPick => 88
  | .copperSword => 89
  | .tinSword => 90
  | .leadSword => 91
  | .tungstenSword => 92
  | .goldSword => 93
  | .platinumSword => 94
  | .tinAxe => 95
  | .ironAxe => 96
  | .leadAxe => 97
  | .silverAxe => 98
  | .tungstenAxe => 99
  | .goldAxe => 100
  | .platinumAxe => 101
  | .woodChair => 102
  | .woodTable => 103
  | .lens => 104
  | .blackLens => 105
  | .shackle => 106
  | .zombieArm => 107
  | .suspiciousLookingEye => 108
  | .demoniteOre => 109
  | .crimtaneOre => 110
  | .corruptSeeds => 111
  | .crimsonSeeds => 112
  | .lesserHealingPotion => 113
  | .binoculars => 114
  | .eyeMask => 115
  | .eyeTrophy => 116
  | .unholyArrow => 117
  | .demoniteBar => 118
  | .crimtaneBar => 119
  | .copperBow => 120
  | .tinBow => 121
  | .ironBow => 122
  | .leadBow => 123
  | .silverBow => 124
  | .tungstenBow => 125
  | .goldBow => 126
  | .platinumBow => 127
  | .demonBow => 128
  | .tendonBow => 129
  | .heart => 130
  | .ebonstone => 131
  | .crimstone => 132
  | .musket => 133
  | .undertaker => 134
  | .musketBall => 135
  | .bomb => 136
  | .stickyBomb => 137
  | .shadowOrbPet => 138
  | .crimsonHeartPet => 139
  | .vilethorn => 140
  | .ballOHurt => 141
  | .bandOfStarpower => 142
  | .panicNecklace => 143
  | .crimsonRod => 144
  | .rottedFork => 145
  | .rottenChunk => 146
  | .vertebra => 147
  | .vileMushroom => 148
  | .viciousMushroom => 149
  | .vilePowder => 150
  | .viciousPowder => 151
  | .wormFood => 152
  | .bloodySpine => 153
  | .shadowScale => 154
  | .tissueSample => 155
  | .nightmarePickaxe => 156
  | .deathbringerPickaxe => 157
  | .eaterMask => 158
  | .eaterTrophy => 159
  | .brainMask => 160
  | .brainTrophy => 161
  | .eatersBone => 162
  | .boneRattle => 163
  | .sand => 164
  | .glass => 165
  | .bottle => 166
  | .copperCoin => 167
  | .silverCoin => 168
  | .goldCoin => 169
  | .platinumCoin => 170
  | .blueDungeonBrick => 171
  | .greenDungeonBrick => 172
  | .pinkDungeonBrick => 173
  | .goldenKey => 174
  | .cobaltShield => 175
  | .skeletronMask => 176
  | .skeletronTrophy => 177
  | .chippysCouch => 178
  | .boneKey => 179
  | .healingPotion => 180
  | .skeletronHand => 181
  | .bookOfSkulls => 182
  | .bone => 183
  | .nazar => 184
  | .aquaScepter => 185
  | .blueMoon => 186
  | .magicMissile => 187
  | .valor => 188
  | .handgun => 189
  | .shadowKey => 190
  | .sunfury => 191
  | .darkLance => 192
  | .ashBlock => 193
  | .obsidianBrick => 194
  | .hellstone => 195
  | .shadowChest => 196
  | .obsidianSkull => 197
  | .flowerOfFire => 198
  | .flamelash => 199
  | .hellwingBow => 200
  | .obsidianRose => 201
  | .magmaStone => 202
  | .demonScythe => 203
  | .guideVoodooDoll => 204
  | .plumbersHat => 205
  | .batBat => 206
  | .treasureMagnet => 207
  | .demonicHellcart => 208
  | .ornateShadowKey => 209
  | .sliceOfHellCake => 210
  | .dynamite => 211
  | .meteoriteBar => 212
  | .hellfireArrow => 213
  | .silverBullet => 214
  | .tungstenBullet => 215
  | .restorationPotion => 216
  | .spelunkerPotion => 217
  | .featherfallPotion => 218
  | .manaRegenerationPotion => 219
  | .obsidianSkinPotion => 220
  | .magicPowerPotion => 221
  | .invisibilityPotion => 222
  | .hunterPotion => 223
  | .heartreachPotion => 224
  | .gravitationPotion => 225
  | .thornsPotion => 226
  | .waterWalkingPotion => 227
  | .battlePotion => 228
  | .teleportationPotion => 229
  | .infernoPotion => 230
  | .lifeforcePotion => 231
  | .recallPotion => 232
  | .potionOfReturn => 233
  | .glowstick => 234
  | .froggyNeckband => 235
  | .goatsTuft => 236
  | .oldCompanionLocket => 237
  | .catChime => 238
  | .dogCollar => 239
  | .turkeyWattleNecklace => 240
  | .meanGoblinSpikes => 241
  | .crowsBeak => 242
  | .balloonyBeads => 243
  | .grimOldBarb => 244
  | .vampirePendant => 245
  | .cowBell => 246
  | .chickenCharm => 247
  | .fairyChoker => 248
  | .hellforge => 249
  | .hellstoneBar => 250
  | .bugNet => 251
  | .lavaproofBugNet => 252
  | .hellButterfly => 253
  | .lavafly => 254
  | .magmaSnail => 255
  | .cascade => 256
  | .boneSerpentKite => 257
  | .goldenBugNet => 258
  | .pwnhammer => 259
  | .breakerBlade => 260
  | .clockworkAssaultRifle => 261
  | .laserRifle => 262
  | .firecracker => 263
  | .warriorEmblem => 264
  | .rangerEmblem => 265
  | .sorcererEmblem => 266
  | .summonerEmblem => 267
  | .wallOfFleshMask => 268
  | .demoniteBrick => 269
  | .crimtaneBrick => 270
  | .wallOfFleshTrophy => 271
  | .pearlstone => 272
  | .pearlsand => 273
  | .ebonsand => 274
  | .crimsand => 275
  | .purpleIce => 276
  | .pinkIce => 277
  | .redIce => 278
  | .cobaltOre => 279
  | .palladiumOre => 280
  | .mythrilOre => 281
  | .orichalcumOre => 282
  | .adamantiteOre => 283
  | .titaniumOre => 284
  | .moltenPickaxe => 285
  | .cobaltBar => 286
  | .palladiumBar => 287
  | .mythrilBar => 288
  | .orichalcumBar => 289
  | .adamantiteBar => 290
  | .titaniumBar => 291
  | .cobaltPickaxe => 292
  | .palladiumPickaxe => 293
  | .mythrilPickaxe => 294
  | .orichalcumPickaxe => 295
  | .adamantitePickaxe => 296
  | .titaniumPickaxe => 297
  | .cobaltDrill => 298
  | .palladiumDrill => 299
  | .mythrilDrill => 300
  | .orichalcumDrill => 301
  | .adamantiteDrill => 302
  | .titaniumDrill => 303
  | .mythrilAnvil => 304
  | .orichalcumAnvil => 305
  | .adamantiteForge => 306
  | .titaniumForge => 307
  | .soulOfNight => 308
  | .fastClock => 309
  | .trifoldMap => 310
  | .soulOfLight => 311
  | .crystalShard => 312
  | .mechanicalEye => 313
  | .mechanicalWorm => 314
  | .mechanicalSkull => 315
  | .gelatinCrystal => 316
  | .cursedFlame => 317
  | .vitamins => 318
  | .meatGrinder => 319
  | .applePie => 320
  | .crystallize => 321
  | .eaterOfLife => 322
  | .monsterMeat => 323
  | .soulOfSight => 324
  | .hallowedBar => 325
  | .greaterHealingPotion => 326
  | .twinMask => 327
  | .retinazerTrophy => 328
  | .spazmatismTrophy => 329
  | .bass => 330
  | .armoredCavefish => 331
  | .specularFish => 332
  | .goldenCarp => 333
  | .flarefinKoi => 334
  | .obsidifish => 335
  | .seaweed => 336
  | .tinCan => 337
  | .oldShoe => 338
  | .jojaCola => 339
  | .canOfWorms => 340
  | .worm => 341
  | .enchantedNightcrawler => 342
  | .goldWorm => 343
  | .woodenCrate => 344
  | .ironCrate => 345
  | .goldenCrate => 346
  | .pearlwoodCrate => 347
  | .mythrilCrate => 348
  | .titaniumCrate => 349
  | .apprenticeBait => 350
  | .journeymanBait => 351
  | .masterBait => 352
  | .aglet => 353
  | .climbingClaws => 354
  | .stepStool => 355
  | .cordageGuide => 356
  | .radar => 357
  | .poisonBarb => 358
  | .sailfishBoots => 359
  | .tsunamiInABottle => 360
  | .anchor => 361
  | .extractinator => 362
  | .enchantedSundial => 363
  | .gingerBeard => 364
  | .tartarSauce => 365
  | .falconBlade => 366
  | .hardySaddle => 367
  | .enchantedSword => 368
  | .swiftnessPotion => 369
  | .ironskinPotion => 370
  | .nightOwlPotion => 371
  | .shinePotion => 372
  | .gillsPotion => 373
  | .miningPotion => 374
  | .trapsightPotion => 375
  | .calmingPotion => 376
  | .flipperPotion => 377
  | .lesserManaPotion => 378
  | .manaPotion => 379
  | .corruptCrate => 380
  | .crimsonCrate => 381
  | .dungeonCrate => 382
  | .skyCrate => 383
  | .hallowedCrate => 384
  | .jungleCrate => 385
  | .defiledCrate => 386
  | .hematicCrate => 387
  | .stockadeCrate => 388
  | .azureCrate => 389
  | .divineCrate => 390
  | .brambleCrate => 391
  | .frozenCrate => 392
  | .borealCrate => 393
  | .oasisCrate => 394
  | .mirageCrate => 395
  | .obsidianCrate => 396
  | .hellstoneCrate => 397
  | .oceanCrate => 398
  | .seasideCrate => 399
  | .starfury => 400
  | .book => 401
  | .luckyHorseshoe => 402
  | .shinyRedBalloon => 403
  | .breathingReed => 404
  | .flipper => 405
  | .feralClaws => 406
  | .ankletOfTheWind => 407
  | .staffOfRegrowth => 408
  | .trident => 409
  | .fishPet => 410
  | .iceBoomerang => 411
  | .iceBlade => 412
  | .cloud => 413
  | .sandstormInABottle => 414
  | .waterWalkingBoots => 415
  | .lavaCharm => 416
  | .iceSkates => 417
  | .boomstick => 418
  | .blizzardInABottle => 419
  | .snowballCannon => 420
  | .ichor => 421
  | .bloodMoonRising => 422
  | .theHangedMan => 423
  | .gloryOfTheFire => 424
  | .boneWarp => 425
  | .skellingtonJSkellingsworth => 426
  | .theCursedMan => 427
  | .theEyeSeesTheEnd => 428
  | .somethingEvilIsWatchingYou => 429
  | .theTwinsHaveAwoken => 430
  | .theScreamer => 431
  | .goblinsPlayingPoker => 432
  | .dryadisque => 433
  | .impact => 434
  | .poweredByBirds => 435
  | .theDestroyer => 436
  | .thePersistencyOfEyes => 437
  | .unicornCrossingTheHallows => 438
  | .greatWave => 439
  | .starryNight => 440
  | .theGuardiansGaze => 441
  | .darkness => 442
  | .darkSoulReaper => 443
  | .trappedGhost => 444
  | .demonsEye => 445
  | .handEarth => 446
  | .skelehead => 447
  | .facingTheCerebralMastermind => 448
  | .lakeOfFire => 449
  | .trioSuperHeroes => 450
  | .impFace => 451
  | .ominousPresence => 452
  | .shiningMoon => 453
  | .livingGore => 454
  | .flowingMagma => 455
  | .theCreationOfTheGuide => 456
  | .flurryBoots => 457
  | .celestialMagnet => 458
  | .fiberglassFishingPole => 459
  | .obsidianSwordfish => 460
  | .cratePotion => 461
  | .sparky => 462
  | .flowerBoots => 463
  | .goldenLockBox => 464
  | .sturdyFossil => 465
  | .duneriderBoots => 466
  | .ancientChisel => 467
  | .stormSpear => 468
  | .thunderZapper => 469
  | .shellPile => 470
  | .snakeCharmersFlute => 471
  | .magicConch => 472
  | .bastStatue => 473
  | .innerTube => 474
  | .scarabBomb => 475
  | .sharkBait => 476
  | .scarabFishingRod => 477
  | .sandcastleBucket => 478
  | .bamboo => 479
  | .andrewSphinx => 480
  | .watchfulAntlion => 481
  | .burningSpirit => 482
  | .jawsOfDeath => 483
  | .theSandsOfSlime => 484
  | .snakesIHateSnakes => 485
  | .lifeAboveTheSand => 486
  | .oasis => 487
  | .prehistoryPreserved => 488
  | .ancientTablet => 489
  | .uluru => 490
  | .visitingThePyramids => 491
  | .bandageBoy => 492
  | .divineEye => 493
  | .demonConch => 494
  | .bottomlessLavaBucket => 495
  | .flameWakerBoots => 496
  | .wetBomb => 497
  | .superheatedBlood => 498
  | .hangingPot => 499
  | .miniVolcano => 500
  | .lavaAbsorbantSponge => 501
  | .obsidianLockBox => 502
  | .lavaSerpentBowl => 503
  | .lavaproofFishingHook => 504
  | .pottedMagmaPalm => 505
  | .pottedBrimstoneBush => 506
  | .pottedFireBrambles => 507
  | .pottedLavaBulb => 508
  | .pottedEmberTendrils => 509
  | .fledglingWings => 510
  | .highPitch => 511
  | .remnantsOfDevotion => 512
  | .constellation => 513
  | .blessingFromTheHeavens => 514
  | .loveIsInTheTrashSlot => 515
  | .seeTheWorldForWhatItIs => 516
  | .eyeOfTheSun => 517
  | .ofSeaAndDreams => 518
  | .glacierFang => 519
  | .seaweedPet => 520
  | .muramasa => 521
  | .ramRune => 522
  | .silverBracer => 523
  | .mudBlock => 524
  | .batfish => 525
  | .bumblebeeTuna => 526
  | .catfish => 527
  | .cloudfish => 528
  | .cursedfish => 529
  | .dirtfish => 530
  | .dynamiteFish => 531
  | .eaterofPlankton => 532
  | .fallenStarfish => 533
  | .theFishofCthulu => 534
  | .fishotron => 535
  | .harpyfish => 536
  | .hungerfish => 537
  | .ichorfish => 538
  | .jewelfish => 539
  | .mirageFish => 540
  | .mutantFlinxfin => 541
  | .pengfish => 542
  | .pixiefish => 543
  | .spiderfish => 544
  | .tundraTrout => 545
  | .unicornFish => 546
  | .guideVoodooFish => 547
  | .wyverntail => 548
  | .zombieFish => 549
  | .amanitaFungifin => 550
  | .angelfish => 551
  | .bloodyManowar => 552
  | .bonefish => 553
  | .bunnyfish => 554
  | .capnTunabeard => 555
  | .clownfish => 556
  | .demonicHellfish => 557
  | .derpfish => 558
  | .fishron => 559
  | .infectedScabbardfish => 560
  | .mudfish => 561
  | .slimefish => 562
  | .tropicalBarracuda => 563
  | .scarabFish => 564
  | .scorpioFish => 565
  | .goldenFishingRod => 566
  | .hotlineFishingHook => 567
  | .fuzzyCarrot => 568
  | .anglerHat => 569
  | .anglerVest => 570
  | .anglerPants => 571
  | .finWings => 572
  | .bottomlessWaterBucket => 573
  | .superAbsorbantSponge => 574
  | .anglerFishHook => 575
  | .fishMinecart => 576
  | .seashellHairpin => 577
  | .mermaidAdornment => 578
  | .mermaidTail => 579
  | .fishCostumeMask => 580
  | .fishCostumeShirt => 581
  | .fishCostumeFinskirt => 582
  | .highTestFishingLine => 583
  | .anglerEarring => 584
  | .tackleBox => 585
  | .fishermansGuide => 586
  | .weatherRadio => 587
  | .sextant => 588
  | .anglerBobber => 589
  | .fishingPotion => 590
  | .sonarPotion => 591
  | .bottomlessHoneyBucket => 592
  | .honeyAbsorbantSponge => 593
  | .lifePreserver => 594
  | .shipsWheel => 595
  | .compassRose => 596
  | .wallAnchor => 597
  | .pillaginMePixels => 598
  | .treasureMap => 599
  | .goldfishTrophy => 600
  | .bunnyfishTrophy => 601
  | .swordfishTrophy => 602
  | .sharkteethTrophy => 603
  | .shipInABottle => 604
  | .seaweedPlanter => 605
  | .notSoLostInParadise => 606
  | .crustography => 607
  | .fangs => 608
  | .whatLurksBelow => 609
  | .couchGag => 610
  | .silentFish => 611
  | .theDuke => 612
  | .trout => 613
  | .salmon => 614
  | .atlanticCod => 615
  | .tuna => 616
  | .redSnapper => 617
  | .neonTetra => 618
  | .damselfish => 619
  | .crimsonTigerfish => 620
  | .frostMinnow => 621
  | .princessFish => 622
  | .prismite => 623
  | .variegatedLardfish => 624
  | .doubleCod => 625
  | .shrimp => 626
  | .chaosFish => 627
  | .ebonkoi => 628
  | .hemopiranha => 629
  | .rockfish => 630
  | .stinkfish => 631
  | .purpleClubberfish => 632
  | .swordfish => 633
  | .reaverShark => 634
  | .sawtoothShark => 635
  | .zephyrFish => 636
  | .frogLeg => 637
  | .scalyTruffle => 638
  | .blueJellyfish => 639
  | .greenJellyfish => 640
  | .pinkJellyfish => 641
  | .bewitchingTable => 642
  | .alchemyTable => 643
  | .bombFish => 644
  | .frostDaggerfish => 645
  | .crystalSerpent => 646
  | .toxikarp => 647
  | .bladetongue => 648
  | .balloonPufferfish => 649
  | .flounder => 650
  | .rockLobster => 651
  | .oyster => 652
  | .ladyOfTheLake => 653
  | .barnacleStaff => 654
  | .brasilianSkies => 655
  | .itsScragglinTime => 656
  | .honeyfin => 657
  | .honeyBlock => 658
  | .crispyHoneyBlock => 659
  | .sparkleSlimeBalloon => 660
  | .queenSlimeMask => 661
  | .crystalAssassinHood => 662
  | .crystalAssassinShirt => 663
  | .crystalAssassinPants => 664
  | .bladeStaff => 665
  | .gelatinousPillion => 666
  | .hookOfDissonance => 667
  | .queenSlimeTrophy => 668
  | .soulOfMight => 669
  | .destroyerMask => 670
  | .destroyerTrophy => 671
  | .soulOfFright => 672
  | .skeletronPrimeMask => 673
  | .skeletronPrimeTrophy => 674
  | .templeKey => 675
  | .grenadeLauncher => 676
  | .rocketI => 677
  | .venusMagnum => 678
  | .nettleBurst => 679
  | .leafBlower => 680
  | .flowerPow => 681
  | .waspGun => 682
  | .seedler => 683
  | .vulgarDisplayOfFlower => 684
  | .pygmyStaff => 685
  | .thornHook => 686
  | .theAxe => 687
  | .seedling => 688
  | .planteraMask => 689
  | .planteraTrophy => 690
  | .lihzahrdBrick => 691
  | .lihzahrdDoor => 692
  | .lihzahrdChest => 693
  | .lihzahrdPowerCell => 694
  | .lihzahrdFurnace => 695
  | .solarTablet => 696
  | .solarTabletFragment => 697
  | .jestersArrow => 698
  | .flamingArrow => 699
  | .throwingKnife => 700
  | .archeryPotion => 701
  | .titanPotion => 702
  | .lihzahrdAltar => 703
  | .superDartTrap => 704
  | .flameTrap => 705
  | .spikyBallTrap => 706
  | .spearTrap => 707
  | .woodenSpike => 708
  | .lihzahrdPressurePlate => 709
  | .beetleHusk => 710
  | .picksaw => 711
  | .stynger => 712
  | .styngerBolt => 713
  | .possessedHatchet => 714
  | .sunStone => 715
  | .eyeOfGolem => 716
  | .heatRay => 717
  | .staffOfEarth => 718
  | .golemFist => 719
  | .mobiusStrip => 720
  | .golemMask => 721
  | .golemTrophy => 722
  | .lizardEgg => 723

def Item.ofCode? : UInt16 → Option Item
  | 1 => some .basicPick
  | 2 => some .copperPick
  | 3 => some .dirt
  | 4 => some .stone
  | 5 => some .copper
  | 6 => some .crystal
  | 7 => some .wood
  | 8 => some .torch
  | 9 => some .stoneSword
  | 10 => some .ice
  | 11 => some .iron
  | 12 => some .crystalPick
  | 13 => some .ironSword
  | 14 => some .frostbrand
  | 15 => some .workbench
  | 16 => some .chest
  | 17 => some .door
  | 18 => some .silver
  | 19 => some .silverSword
  | 20 => some .bow
  | 21 => some .arrow
  | 22 => some .stormBow
  | 23 => some .amber
  | 24 => some .geode
  | 25 => some .draught
  | 26 => some .geodeMail
  | 27 => some .amberTreads
  | 28 => some .watchhorn
  | 29 => some .raidCrest
  | 30 => some .platform
  | 31 => some .homeBeacon
  | 32 => some .recallCharm
  | 33 => some .caveGel
  | 34 => some .frostShell
  | 35 => some .quarryScale
  | 36 => some .hunterBow
  | 37 => some .fishingRod
  | 38 => some .bait
  | 39 => some .pondPerch
  | 40 => some .caveEel
  | 41 => some .sunleafSeed
  | 42 => some .sunleaf
  | 43 => some .gillsElixir
  | 44 => some .barkskinTonic
  | 45 => some .ruinHammer
  | 46 => some .rail
  | 47 => some .minecart
  | 48 => some .grapplingHook
  | 49 => some .lifeCrystal
  | 50 => some .highlandBerry
  | 51 => some .deepsteel
  | 52 => some .highlandCrossbow
  | 53 => some .deepstoneAegis
  | 54 => some .obsidian
  | 55 => some .ironBar
  | 56 => some .chain
  | 57 => some .cobweb
  | 58 => some .silk
  | 59 => some .bed
  | 60 => some .furnace
  | 61 => some .ironAnvil
  | 62 => some .sawmill
  | 63 => some .loom
  | 64 => some .woodWall
  | 65 => some .woodHammer
  | 66 => some .copperShortsword
  | 67 => some .copperAxe
  | 68 => some .woodSword
  | 69 => some .tinOre
  | 70 => some .leadOre
  | 71 => some .tungstenOre
  | 72 => some .goldOre
  | 73 => some .platinumOre
  | 74 => some .copperBar
  | 75 => some .tinBar
  | 76 => some .leadBar
  | 77 => some .silverBar
  | 78 => some .tungstenBar
  | 79 => some .goldBar
  | 80 => some .platinumBar
  | 81 => some .leadAnvil
  | 82 => some .tinPick
  | 83 => some .ironPick
  | 84 => some .leadPick
  | 85 => some .silverPick
  | 86 => some .tungstenPick
  | 87 => some .goldPick
  | 88 => some .platinumPick
  | 89 => some .copperSword
  | 90 => some .tinSword
  | 91 => some .leadSword
  | 92 => some .tungstenSword
  | 93 => some .goldSword
  | 94 => some .platinumSword
  | 95 => some .tinAxe
  | 96 => some .ironAxe
  | 97 => some .leadAxe
  | 98 => some .silverAxe
  | 99 => some .tungstenAxe
  | 100 => some .goldAxe
  | 101 => some .platinumAxe
  | 102 => some .woodChair
  | 103 => some .woodTable
  | 104 => some .lens
  | 105 => some .blackLens
  | 106 => some .shackle
  | 107 => some .zombieArm
  | 108 => some .suspiciousLookingEye
  | 109 => some .demoniteOre
  | 110 => some .crimtaneOre
  | 111 => some .corruptSeeds
  | 112 => some .crimsonSeeds
  | 113 => some .lesserHealingPotion
  | 114 => some .binoculars
  | 115 => some .eyeMask
  | 116 => some .eyeTrophy
  | 117 => some .unholyArrow
  | 118 => some .demoniteBar
  | 119 => some .crimtaneBar
  | 120 => some .copperBow
  | 121 => some .tinBow
  | 122 => some .ironBow
  | 123 => some .leadBow
  | 124 => some .silverBow
  | 125 => some .tungstenBow
  | 126 => some .goldBow
  | 127 => some .platinumBow
  | 128 => some .demonBow
  | 129 => some .tendonBow
  | 130 => some .heart
  | 131 => some .ebonstone
  | 132 => some .crimstone
  | 133 => some .musket
  | 134 => some .undertaker
  | 135 => some .musketBall
  | 136 => some .bomb
  | 137 => some .stickyBomb
  | 138 => some .shadowOrbPet
  | 139 => some .crimsonHeartPet
  | 140 => some .vilethorn
  | 141 => some .ballOHurt
  | 142 => some .bandOfStarpower
  | 143 => some .panicNecklace
  | 144 => some .crimsonRod
  | 145 => some .rottedFork
  | 146 => some .rottenChunk
  | 147 => some .vertebra
  | 148 => some .vileMushroom
  | 149 => some .viciousMushroom
  | 150 => some .vilePowder
  | 151 => some .viciousPowder
  | 152 => some .wormFood
  | 153 => some .bloodySpine
  | 154 => some .shadowScale
  | 155 => some .tissueSample
  | 156 => some .nightmarePickaxe
  | 157 => some .deathbringerPickaxe
  | 158 => some .eaterMask
  | 159 => some .eaterTrophy
  | 160 => some .brainMask
  | 161 => some .brainTrophy
  | 162 => some .eatersBone
  | 163 => some .boneRattle
  | 164 => some .sand
  | 165 => some .glass
  | 166 => some .bottle
  | 167 => some .copperCoin
  | 168 => some .silverCoin
  | 169 => some .goldCoin
  | 170 => some .platinumCoin
  | 171 => some .blueDungeonBrick
  | 172 => some .greenDungeonBrick
  | 173 => some .pinkDungeonBrick
  | 174 => some .goldenKey
  | 175 => some .cobaltShield
  | 176 => some .skeletronMask
  | 177 => some .skeletronTrophy
  | 178 => some .chippysCouch
  | 179 => some .boneKey
  | 180 => some .healingPotion
  | 181 => some .skeletronHand
  | 182 => some .bookOfSkulls
  | 183 => some .bone
  | 184 => some .nazar
  | 185 => some .aquaScepter
  | 186 => some .blueMoon
  | 187 => some .magicMissile
  | 188 => some .valor
  | 189 => some .handgun
  | 190 => some .shadowKey
  | 191 => some .sunfury
  | 192 => some .darkLance
  | 193 => some .ashBlock
  | 194 => some .obsidianBrick
  | 195 => some .hellstone
  | 196 => some .shadowChest
  | 197 => some .obsidianSkull
  | 198 => some .flowerOfFire
  | 199 => some .flamelash
  | 200 => some .hellwingBow
  | 201 => some .obsidianRose
  | 202 => some .magmaStone
  | 203 => some .demonScythe
  | 204 => some .guideVoodooDoll
  | 205 => some .plumbersHat
  | 206 => some .batBat
  | 207 => some .treasureMagnet
  | 208 => some .demonicHellcart
  | 209 => some .ornateShadowKey
  | 210 => some .sliceOfHellCake
  | 211 => some .dynamite
  | 212 => some .meteoriteBar
  | 213 => some .hellfireArrow
  | 214 => some .silverBullet
  | 215 => some .tungstenBullet
  | 216 => some .restorationPotion
  | 217 => some .spelunkerPotion
  | 218 => some .featherfallPotion
  | 219 => some .manaRegenerationPotion
  | 220 => some .obsidianSkinPotion
  | 221 => some .magicPowerPotion
  | 222 => some .invisibilityPotion
  | 223 => some .hunterPotion
  | 224 => some .heartreachPotion
  | 225 => some .gravitationPotion
  | 226 => some .thornsPotion
  | 227 => some .waterWalkingPotion
  | 228 => some .battlePotion
  | 229 => some .teleportationPotion
  | 230 => some .infernoPotion
  | 231 => some .lifeforcePotion
  | 232 => some .recallPotion
  | 233 => some .potionOfReturn
  | 234 => some .glowstick
  | 235 => some .froggyNeckband
  | 236 => some .goatsTuft
  | 237 => some .oldCompanionLocket
  | 238 => some .catChime
  | 239 => some .dogCollar
  | 240 => some .turkeyWattleNecklace
  | 241 => some .meanGoblinSpikes
  | 242 => some .crowsBeak
  | 243 => some .balloonyBeads
  | 244 => some .grimOldBarb
  | 245 => some .vampirePendant
  | 246 => some .cowBell
  | 247 => some .chickenCharm
  | 248 => some .fairyChoker
  | 249 => some .hellforge
  | 250 => some .hellstoneBar
  | 251 => some .bugNet
  | 252 => some .lavaproofBugNet
  | 253 => some .hellButterfly
  | 254 => some .lavafly
  | 255 => some .magmaSnail
  | 256 => some .cascade
  | 257 => some .boneSerpentKite
  | 258 => some .goldenBugNet
  | 259 => some .pwnhammer
  | 260 => some .breakerBlade
  | 261 => some .clockworkAssaultRifle
  | 262 => some .laserRifle
  | 263 => some .firecracker
  | 264 => some .warriorEmblem
  | 265 => some .rangerEmblem
  | 266 => some .sorcererEmblem
  | 267 => some .summonerEmblem
  | 268 => some .wallOfFleshMask
  | 269 => some .demoniteBrick
  | 270 => some .crimtaneBrick
  | 271 => some .wallOfFleshTrophy
  | 272 => some .pearlstone
  | 273 => some .pearlsand
  | 274 => some .ebonsand
  | 275 => some .crimsand
  | 276 => some .purpleIce
  | 277 => some .pinkIce
  | 278 => some .redIce
  | 279 => some .cobaltOre
  | 280 => some .palladiumOre
  | 281 => some .mythrilOre
  | 282 => some .orichalcumOre
  | 283 => some .adamantiteOre
  | 284 => some .titaniumOre
  | 285 => some .moltenPickaxe
  | 286 => some .cobaltBar
  | 287 => some .palladiumBar
  | 288 => some .mythrilBar
  | 289 => some .orichalcumBar
  | 290 => some .adamantiteBar
  | 291 => some .titaniumBar
  | 292 => some .cobaltPickaxe
  | 293 => some .palladiumPickaxe
  | 294 => some .mythrilPickaxe
  | 295 => some .orichalcumPickaxe
  | 296 => some .adamantitePickaxe
  | 297 => some .titaniumPickaxe
  | 298 => some .cobaltDrill
  | 299 => some .palladiumDrill
  | 300 => some .mythrilDrill
  | 301 => some .orichalcumDrill
  | 302 => some .adamantiteDrill
  | 303 => some .titaniumDrill
  | 304 => some .mythrilAnvil
  | 305 => some .orichalcumAnvil
  | 306 => some .adamantiteForge
  | 307 => some .titaniumForge
  | 308 => some .soulOfNight
  | 309 => some .fastClock
  | 310 => some .trifoldMap
  | 311 => some .soulOfLight
  | 312 => some .crystalShard
  | 313 => some .mechanicalEye
  | 314 => some .mechanicalWorm
  | 315 => some .mechanicalSkull
  | 316 => some .gelatinCrystal
  | 317 => some .cursedFlame
  | 318 => some .vitamins
  | 319 => some .meatGrinder
  | 320 => some .applePie
  | 321 => some .crystallize
  | 322 => some .eaterOfLife
  | 323 => some .monsterMeat
  | 324 => some .soulOfSight
  | 325 => some .hallowedBar
  | 326 => some .greaterHealingPotion
  | 327 => some .twinMask
  | 328 => some .retinazerTrophy
  | 329 => some .spazmatismTrophy
  | 330 => some .bass
  | 331 => some .armoredCavefish
  | 332 => some .specularFish
  | 333 => some .goldenCarp
  | 334 => some .flarefinKoi
  | 335 => some .obsidifish
  | 336 => some .seaweed
  | 337 => some .tinCan
  | 338 => some .oldShoe
  | 339 => some .jojaCola
  | 340 => some .canOfWorms
  | 341 => some .worm
  | 342 => some .enchantedNightcrawler
  | 343 => some .goldWorm
  | 344 => some .woodenCrate
  | 345 => some .ironCrate
  | 346 => some .goldenCrate
  | 347 => some .pearlwoodCrate
  | 348 => some .mythrilCrate
  | 349 => some .titaniumCrate
  | 350 => some .apprenticeBait
  | 351 => some .journeymanBait
  | 352 => some .masterBait
  | 353 => some .aglet
  | 354 => some .climbingClaws
  | 355 => some .stepStool
  | 356 => some .cordageGuide
  | 357 => some .radar
  | 358 => some .poisonBarb
  | 359 => some .sailfishBoots
  | 360 => some .tsunamiInABottle
  | 361 => some .anchor
  | 362 => some .extractinator
  | 363 => some .enchantedSundial
  | 364 => some .gingerBeard
  | 365 => some .tartarSauce
  | 366 => some .falconBlade
  | 367 => some .hardySaddle
  | 368 => some .enchantedSword
  | 369 => some .swiftnessPotion
  | 370 => some .ironskinPotion
  | 371 => some .nightOwlPotion
  | 372 => some .shinePotion
  | 373 => some .gillsPotion
  | 374 => some .miningPotion
  | 375 => some .trapsightPotion
  | 376 => some .calmingPotion
  | 377 => some .flipperPotion
  | 378 => some .lesserManaPotion
  | 379 => some .manaPotion
  | 380 => some .corruptCrate
  | 381 => some .crimsonCrate
  | 382 => some .dungeonCrate
  | 383 => some .skyCrate
  | 384 => some .hallowedCrate
  | 385 => some .jungleCrate
  | 386 => some .defiledCrate
  | 387 => some .hematicCrate
  | 388 => some .stockadeCrate
  | 389 => some .azureCrate
  | 390 => some .divineCrate
  | 391 => some .brambleCrate
  | 392 => some .frozenCrate
  | 393 => some .borealCrate
  | 394 => some .oasisCrate
  | 395 => some .mirageCrate
  | 396 => some .obsidianCrate
  | 397 => some .hellstoneCrate
  | 398 => some .oceanCrate
  | 399 => some .seasideCrate
  | 400 => some .starfury
  | 401 => some .book
  | 402 => some .luckyHorseshoe
  | 403 => some .shinyRedBalloon
  | 404 => some .breathingReed
  | 405 => some .flipper
  | 406 => some .feralClaws
  | 407 => some .ankletOfTheWind
  | 408 => some .staffOfRegrowth
  | 409 => some .trident
  | 410 => some .fishPet
  | 411 => some .iceBoomerang
  | 412 => some .iceBlade
  | 413 => some .cloud
  | 414 => some .sandstormInABottle
  | 415 => some .waterWalkingBoots
  | 416 => some .lavaCharm
  | 417 => some .iceSkates
  | 418 => some .boomstick
  | 419 => some .blizzardInABottle
  | 420 => some .snowballCannon
  | 421 => some .ichor
  | 422 => some .bloodMoonRising
  | 423 => some .theHangedMan
  | 424 => some .gloryOfTheFire
  | 425 => some .boneWarp
  | 426 => some .skellingtonJSkellingsworth
  | 427 => some .theCursedMan
  | 428 => some .theEyeSeesTheEnd
  | 429 => some .somethingEvilIsWatchingYou
  | 430 => some .theTwinsHaveAwoken
  | 431 => some .theScreamer
  | 432 => some .goblinsPlayingPoker
  | 433 => some .dryadisque
  | 434 => some .impact
  | 435 => some .poweredByBirds
  | 436 => some .theDestroyer
  | 437 => some .thePersistencyOfEyes
  | 438 => some .unicornCrossingTheHallows
  | 439 => some .greatWave
  | 440 => some .starryNight
  | 441 => some .theGuardiansGaze
  | 442 => some .darkness
  | 443 => some .darkSoulReaper
  | 444 => some .trappedGhost
  | 445 => some .demonsEye
  | 446 => some .handEarth
  | 447 => some .skelehead
  | 448 => some .facingTheCerebralMastermind
  | 449 => some .lakeOfFire
  | 450 => some .trioSuperHeroes
  | 451 => some .impFace
  | 452 => some .ominousPresence
  | 453 => some .shiningMoon
  | 454 => some .livingGore
  | 455 => some .flowingMagma
  | 456 => some .theCreationOfTheGuide
  | 457 => some .flurryBoots
  | 458 => some .celestialMagnet
  | 459 => some .fiberglassFishingPole
  | 460 => some .obsidianSwordfish
  | 461 => some .cratePotion
  | 462 => some .sparky
  | 463 => some .flowerBoots
  | 464 => some .goldenLockBox
  | 465 => some .sturdyFossil
  | 466 => some .duneriderBoots
  | 467 => some .ancientChisel
  | 468 => some .stormSpear
  | 469 => some .thunderZapper
  | 470 => some .shellPile
  | 471 => some .snakeCharmersFlute
  | 472 => some .magicConch
  | 473 => some .bastStatue
  | 474 => some .innerTube
  | 475 => some .scarabBomb
  | 476 => some .sharkBait
  | 477 => some .scarabFishingRod
  | 478 => some .sandcastleBucket
  | 479 => some .bamboo
  | 480 => some .andrewSphinx
  | 481 => some .watchfulAntlion
  | 482 => some .burningSpirit
  | 483 => some .jawsOfDeath
  | 484 => some .theSandsOfSlime
  | 485 => some .snakesIHateSnakes
  | 486 => some .lifeAboveTheSand
  | 487 => some .oasis
  | 488 => some .prehistoryPreserved
  | 489 => some .ancientTablet
  | 490 => some .uluru
  | 491 => some .visitingThePyramids
  | 492 => some .bandageBoy
  | 493 => some .divineEye
  | 494 => some .demonConch
  | 495 => some .bottomlessLavaBucket
  | 496 => some .flameWakerBoots
  | 497 => some .wetBomb
  | 498 => some .superheatedBlood
  | 499 => some .hangingPot
  | 500 => some .miniVolcano
  | 501 => some .lavaAbsorbantSponge
  | 502 => some .obsidianLockBox
  | 503 => some .lavaSerpentBowl
  | 504 => some .lavaproofFishingHook
  | 505 => some .pottedMagmaPalm
  | 506 => some .pottedBrimstoneBush
  | 507 => some .pottedFireBrambles
  | 508 => some .pottedLavaBulb
  | 509 => some .pottedEmberTendrils
  | 510 => some .fledglingWings
  | 511 => some .highPitch
  | 512 => some .remnantsOfDevotion
  | 513 => some .constellation
  | 514 => some .blessingFromTheHeavens
  | 515 => some .loveIsInTheTrashSlot
  | 516 => some .seeTheWorldForWhatItIs
  | 517 => some .eyeOfTheSun
  | 518 => some .ofSeaAndDreams
  | 519 => some .glacierFang
  | 520 => some .seaweedPet
  | 521 => some .muramasa
  | 522 => some .ramRune
  | 523 => some .silverBracer
  | 524 => some .mudBlock
  | 525 => some .batfish
  | 526 => some .bumblebeeTuna
  | 527 => some .catfish
  | 528 => some .cloudfish
  | 529 => some .cursedfish
  | 530 => some .dirtfish
  | 531 => some .dynamiteFish
  | 532 => some .eaterofPlankton
  | 533 => some .fallenStarfish
  | 534 => some .theFishofCthulu
  | 535 => some .fishotron
  | 536 => some .harpyfish
  | 537 => some .hungerfish
  | 538 => some .ichorfish
  | 539 => some .jewelfish
  | 540 => some .mirageFish
  | 541 => some .mutantFlinxfin
  | 542 => some .pengfish
  | 543 => some .pixiefish
  | 544 => some .spiderfish
  | 545 => some .tundraTrout
  | 546 => some .unicornFish
  | 547 => some .guideVoodooFish
  | 548 => some .wyverntail
  | 549 => some .zombieFish
  | 550 => some .amanitaFungifin
  | 551 => some .angelfish
  | 552 => some .bloodyManowar
  | 553 => some .bonefish
  | 554 => some .bunnyfish
  | 555 => some .capnTunabeard
  | 556 => some .clownfish
  | 557 => some .demonicHellfish
  | 558 => some .derpfish
  | 559 => some .fishron
  | 560 => some .infectedScabbardfish
  | 561 => some .mudfish
  | 562 => some .slimefish
  | 563 => some .tropicalBarracuda
  | 564 => some .scarabFish
  | 565 => some .scorpioFish
  | 566 => some .goldenFishingRod
  | 567 => some .hotlineFishingHook
  | 568 => some .fuzzyCarrot
  | 569 => some .anglerHat
  | 570 => some .anglerVest
  | 571 => some .anglerPants
  | 572 => some .finWings
  | 573 => some .bottomlessWaterBucket
  | 574 => some .superAbsorbantSponge
  | 575 => some .anglerFishHook
  | 576 => some .fishMinecart
  | 577 => some .seashellHairpin
  | 578 => some .mermaidAdornment
  | 579 => some .mermaidTail
  | 580 => some .fishCostumeMask
  | 581 => some .fishCostumeShirt
  | 582 => some .fishCostumeFinskirt
  | 583 => some .highTestFishingLine
  | 584 => some .anglerEarring
  | 585 => some .tackleBox
  | 586 => some .fishermansGuide
  | 587 => some .weatherRadio
  | 588 => some .sextant
  | 589 => some .anglerBobber
  | 590 => some .fishingPotion
  | 591 => some .sonarPotion
  | 592 => some .bottomlessHoneyBucket
  | 593 => some .honeyAbsorbantSponge
  | 594 => some .lifePreserver
  | 595 => some .shipsWheel
  | 596 => some .compassRose
  | 597 => some .wallAnchor
  | 598 => some .pillaginMePixels
  | 599 => some .treasureMap
  | 600 => some .goldfishTrophy
  | 601 => some .bunnyfishTrophy
  | 602 => some .swordfishTrophy
  | 603 => some .sharkteethTrophy
  | 604 => some .shipInABottle
  | 605 => some .seaweedPlanter
  | 606 => some .notSoLostInParadise
  | 607 => some .crustography
  | 608 => some .fangs
  | 609 => some .whatLurksBelow
  | 610 => some .couchGag
  | 611 => some .silentFish
  | 612 => some .theDuke
  | 613 => some .trout
  | 614 => some .salmon
  | 615 => some .atlanticCod
  | 616 => some .tuna
  | 617 => some .redSnapper
  | 618 => some .neonTetra
  | 619 => some .damselfish
  | 620 => some .crimsonTigerfish
  | 621 => some .frostMinnow
  | 622 => some .princessFish
  | 623 => some .prismite
  | 624 => some .variegatedLardfish
  | 625 => some .doubleCod
  | 626 => some .shrimp
  | 627 => some .chaosFish
  | 628 => some .ebonkoi
  | 629 => some .hemopiranha
  | 630 => some .rockfish
  | 631 => some .stinkfish
  | 632 => some .purpleClubberfish
  | 633 => some .swordfish
  | 634 => some .reaverShark
  | 635 => some .sawtoothShark
  | 636 => some .zephyrFish
  | 637 => some .frogLeg
  | 638 => some .scalyTruffle
  | 639 => some .blueJellyfish
  | 640 => some .greenJellyfish
  | 641 => some .pinkJellyfish
  | 642 => some .bewitchingTable
  | 643 => some .alchemyTable
  | 644 => some .bombFish
  | 645 => some .frostDaggerfish
  | 646 => some .crystalSerpent
  | 647 => some .toxikarp
  | 648 => some .bladetongue
  | 649 => some .balloonPufferfish
  | 650 => some .flounder
  | 651 => some .rockLobster
  | 652 => some .oyster
  | 653 => some .ladyOfTheLake
  | 654 => some .barnacleStaff
  | 655 => some .brasilianSkies
  | 656 => some .itsScragglinTime
  | 657 => some .honeyfin
  | 658 => some .honeyBlock
  | 659 => some .crispyHoneyBlock
  | 660 => some .sparkleSlimeBalloon
  | 661 => some .queenSlimeMask
  | 662 => some .crystalAssassinHood
  | 663 => some .crystalAssassinShirt
  | 664 => some .crystalAssassinPants
  | 665 => some .bladeStaff
  | 666 => some .gelatinousPillion
  | 667 => some .hookOfDissonance
  | 668 => some .queenSlimeTrophy
  | 669 => some .soulOfMight
  | 670 => some .destroyerMask
  | 671 => some .destroyerTrophy
  | 672 => some .soulOfFright
  | 673 => some .skeletronPrimeMask
  | 674 => some .skeletronPrimeTrophy
  | 675 => some .templeKey
  | 676 => some .grenadeLauncher
  | 677 => some .rocketI
  | 678 => some .venusMagnum
  | 679 => some .nettleBurst
  | 680 => some .leafBlower
  | 681 => some .flowerPow
  | 682 => some .waspGun
  | 683 => some .seedler
  | 684 => some .vulgarDisplayOfFlower
  | 685 => some .pygmyStaff
  | 686 => some .thornHook
  | 687 => some .theAxe
  | 688 => some .seedling
  | 689 => some .planteraMask
  | 690 => some .planteraTrophy
  | 691 => some .lihzahrdBrick
  | 692 => some .lihzahrdDoor
  | 693 => some .lihzahrdChest
  | 694 => some .lihzahrdPowerCell
  | 695 => some .lihzahrdFurnace
  | 696 => some .solarTablet
  | 697 => some .solarTabletFragment
  | 698 => some .jestersArrow
  | 699 => some .flamingArrow
  | 700 => some .throwingKnife
  | 701 => some .archeryPotion
  | 702 => some .titanPotion
  | 703 => some .lihzahrdAltar
  | 704 => some .superDartTrap
  | 705 => some .flameTrap
  | 706 => some .spikyBallTrap
  | 707 => some .spearTrap
  | 708 => some .woodenSpike
  | 709 => some .lihzahrdPressurePlate
  | 710 => some .beetleHusk
  | 711 => some .picksaw
  | 712 => some .stynger
  | 713 => some .styngerBolt
  | 714 => some .possessedHatchet
  | 715 => some .sunStone
  | 716 => some .eyeOfGolem
  | 717 => some .heatRay
  | 718 => some .staffOfEarth
  | 719 => some .golemFist
  | 720 => some .mobiusStrip
  | 721 => some .golemMask
  | 722 => some .golemTrophy
  | 723 => some .lizardEgg
  | _ => none

def Item.queenSlimeTextureId : Item → Option Nat
  | .sparkleSlimeBalloon => some 4986
  | .queenSlimeMask => some 4959
  | .crystalAssassinHood => some 4982
  | .crystalAssassinShirt => some 4983
  | .crystalAssassinPants => some 4984
  | .bladeStaff => some 4758
  | .soulOfMight => some 548
  | .destroyerMask => some 2113
  | .destroyerTrophy => some 1366
  | .gelatinousPillion => some 4981
  | .hookOfDissonance => some 4980
  | .queenSlimeTrophy => some 4958
  | _ => none

def Item.skeletronPrimeTextureId : Item → Option Nat
  | .soulOfFright => some 547
  | .skeletronPrimeMask => some 2107
  | .skeletronPrimeTrophy => some 1367
  | _ => none

def Item.planteraTextureId : Item → Option Nat
  | .templeKey => some 1141
  | .grenadeLauncher => some 758
  | .rocketI => some 771
  | .venusMagnum => some 1255
  | .nettleBurst => some 788
  | .leafBlower => some 1178
  | .flowerPow => some 1259
  | .waspGun => some 1155
  | .seedler => some 3018
  | .vulgarDisplayOfFlower => some 5477
  | .pygmyStaff => some 1157
  | .thornHook => some 3021
  | .theAxe => some 1305
  | .seedling => some 1182
  | .planteraMask => some 2109
  | .planteraTrophy => some 1370
  | _ => none

def Item.templeTextureId : Item → Option Nat
  | .lihzahrdBrick => some 1101
  | .lihzahrdDoor => some 1137
  | .lihzahrdChest => some 1142
  | .lihzahrdPowerCell => some 1293
  | .lihzahrdFurnace => some 2195
  | .solarTablet => some 2767
  | .solarTabletFragment => some 2766
  | .jestersArrow => some 51
  | .flamingArrow => some 41
  | .throwingKnife => some 279
  | .archeryPotion => some 303
  | .titanPotion => some 2326
  | .lihzahrdAltar => some 1292
  | .superDartTrap => some 1146
  | .flameTrap => some 1147
  | .spikyBallTrap => some 1148
  | .spearTrap => some 1149
  | .woodenSpike => some 1150
  | .lihzahrdPressurePlate => some 1151
  | .beetleHusk => some 2218
  | .picksaw => some 1294
  | .stynger => some 1258
  | .styngerBolt => some 1261
  | .possessedHatchet => some 1122
  | .sunStone => some 899
  | .eyeOfGolem => some 1248
  | .heatRay => some 1295
  | .staffOfEarth => some 1296
  | .golemFist => some 1297
  | .mobiusStrip => some 6158
  | .golemMask => some 2110
  | .golemTrophy => some 1371
  | .lizardEgg => some 1172
  | _ => none

def Item.waterCatchTextureId : Item → Option Nat
  | .trout => some 2297
  | .salmon => some 2298
  | .atlanticCod => some 2299
  | .tuna => some 2300
  | .redSnapper => some 2301
  | .neonTetra => some 2302
  | .damselfish => some 2304
  | .crimsonTigerfish => some 2305
  | .frostMinnow => some 2306
  | .princessFish => some 2307
  | .prismite => some 2310
  | .variegatedLardfish => some 2311
  | .doubleCod => some 2313
  | .honeyfin => some 2314
  | .shrimp => some 2316
  | .chaosFish => some 2317
  | .ebonkoi => some 2318
  | .hemopiranha => some 2319
  | .rockfish => some 2320
  | .stinkfish => some 2321
  | .purpleClubberfish => some 2330
  | .swordfish => some 2332
  | .reaverShark => some 2341
  | .sawtoothShark => some 2342
  | .zephyrFish => some 2420
  | .frogLeg => some 2423
  | .scalyTruffle => some 2429
  | .blueJellyfish => some 2436
  | .greenJellyfish => some 2437
  | .pinkJellyfish => some 2438
  | .bewitchingTable => some 2999
  | .alchemyTable => some 3000
  | .bombFish => some 3196
  | .frostDaggerfish => some 3197
  | .crystalSerpent => some 3209
  | .toxikarp => some 3210
  | .bladetongue => some 3211
  | .balloonPufferfish => some 3225
  | .flounder => some 4401
  | .rockLobster => some 4402
  | .oyster => some 4410
  | .ladyOfTheLake => some 5274
  | .barnacleStaff => some 5463
  | .brasilianSkies => some 5490
  | .itsScragglinTime => some 5634
  | _ => none

def Item.anglerTextureId : Item → Option Nat
  | .batfish => some 2450
  | .bumblebeeTuna => some 2451
  | .catfish => some 2452
  | .cloudfish => some 2453
  | .cursedfish => some 2454
  | .dirtfish => some 2455
  | .dynamiteFish => some 2456
  | .eaterofPlankton => some 2457
  | .fallenStarfish => some 2458
  | .theFishofCthulu => some 2459
  | .fishotron => some 2460
  | .harpyfish => some 2461
  | .hungerfish => some 2462
  | .ichorfish => some 2463
  | .jewelfish => some 2464
  | .mirageFish => some 2465
  | .mutantFlinxfin => some 2466
  | .pengfish => some 2467
  | .pixiefish => some 2468
  | .spiderfish => some 2469
  | .tundraTrout => some 2470
  | .unicornFish => some 2471
  | .guideVoodooFish => some 2472
  | .wyverntail => some 2473
  | .zombieFish => some 2474
  | .amanitaFungifin => some 2475
  | .angelfish => some 2476
  | .bloodyManowar => some 2477
  | .bonefish => some 2478
  | .bunnyfish => some 2479
  | .capnTunabeard => some 2480
  | .clownfish => some 2481
  | .demonicHellfish => some 2482
  | .derpfish => some 2483
  | .fishron => some 2484
  | .infectedScabbardfish => some 2485
  | .mudfish => some 2486
  | .slimefish => some 2487
  | .tropicalBarracuda => some 2488
  | .scarabFish => some 4393
  | .scorpioFish => some 4394
  | .goldenFishingRod => some 2294
  | .hotlineFishingHook => some 2422
  | .fuzzyCarrot => some 2428
  | .anglerHat => some 2367
  | .anglerVest => some 2368
  | .anglerPants => some 2369
  | .finWings => some 2494
  | .bottomlessWaterBucket => some 3031
  | .superAbsorbantSponge => some 3032
  | .anglerFishHook => some 2360
  | .fishMinecart => some 4067
  | .seashellHairpin => some 2417
  | .mermaidAdornment => some 2418
  | .mermaidTail => some 2419
  | .fishCostumeMask => some 2498
  | .fishCostumeShirt => some 2499
  | .fishCostumeFinskirt => some 2500
  | .highTestFishingLine => some 2373
  | .anglerEarring => some 2374
  | .tackleBox => some 2375
  | .fishermansGuide => some 3120
  | .weatherRadio => some 3037
  | .sextant => some 3096
  | .anglerBobber => some 5139
  | .fishingPotion => some 2354
  | .sonarPotion => some 2355
  | .bottomlessHoneyBucket => some 5302
  | .honeyAbsorbantSponge => some 5303
  | .lifePreserver => some 2442
  | .shipsWheel => some 2443
  | .compassRose => some 2444
  | .wallAnchor => some 2445
  | .pillaginMePixels => some 2497
  | .treasureMap => some 2495
  | .goldfishTrophy => some 2446
  | .bunnyfishTrophy => some 2447
  | .swordfishTrophy => some 2448
  | .sharkteethTrophy => some 2449
  | .shipInABottle => some 2490
  | .seaweedPlanter => some 2496
  | .notSoLostInParadise => some 5235
  | .crustography => some 5252
  | .fangs => some 5256
  | .whatLurksBelow => some 5259
  | .couchGag => some 5263
  | .silentFish => some 5264
  | .theDuke => some 5265
  | _ => none

def Item.biomeFishingTextureId : Item → Option Nat
  | .corruptCrate => some 3203
  | .crimsonCrate => some 3204
  | .dungeonCrate => some 3205
  | .skyCrate => some 3206
  | .hallowedCrate => some 3207
  | .jungleCrate => some 3208
  | .defiledCrate => some 3982
  | .hematicCrate => some 3983
  | .stockadeCrate => some 3984
  | .azureCrate => some 3985
  | .divineCrate => some 3986
  | .brambleCrate => some 3987
  | .frozenCrate => some 4405
  | .borealCrate => some 4406
  | .oasisCrate => some 4407
  | .mirageCrate => some 4408
  | .obsidianCrate => some 4877
  | .hellstoneCrate => some 4878
  | .oceanCrate => some 5002
  | .seasideCrate => some 5003
  | .starfury => some 65
  | .book => some 149
  | .luckyHorseshoe => some 158
  | .shinyRedBalloon => some 159
  | .breathingReed => some 186
  | .flipper => some 187
  | .feralClaws => some 211
  | .ankletOfTheWind => some 212
  | .staffOfRegrowth => some 213
  | .trident => some 277
  | .fishPet => some 669
  | .iceBoomerang => some 670
  | .iceBlade => some 724
  | .cloud => some 751
  | .sandstormInABottle => some 857
  | .waterWalkingBoots => some 863
  | .lavaCharm => some 906
  | .iceSkates => some 950
  | .boomstick => some 964
  | .blizzardInABottle => some 987
  | .snowballCannon => some 1319
  | .ichor => some 1332
  | .bloodMoonRising => some 1372
  | .theHangedMan => some 1373
  | .gloryOfTheFire => some 1374
  | .boneWarp => some 1375
  | .skellingtonJSkellingsworth => some 1419
  | .theCursedMan => some 1420
  | .theEyeSeesTheEnd => some 1421
  | .somethingEvilIsWatchingYou => some 1422
  | .theTwinsHaveAwoken => some 1423
  | .theScreamer => some 1424
  | .goblinsPlayingPoker => some 1425
  | .dryadisque => some 1426
  | .impact => some 1433
  | .poweredByBirds => some 1434
  | .theDestroyer => some 1435
  | .thePersistencyOfEyes => some 1436
  | .unicornCrossingTheHallows => some 1437
  | .greatWave => some 1438
  | .starryNight => some 1439
  | .theGuardiansGaze => some 1441
  | .darkness => some 1475
  | .darkSoulReaper => some 1476
  | .trappedGhost => some 1478
  | .demonsEye => some 1479
  | .handEarth => some 1497
  | .skelehead => some 1499
  | .facingTheCerebralMastermind => some 1500
  | .lakeOfFire => some 1501
  | .trioSuperHeroes => some 1502
  | .impFace => some 1538
  | .ominousPresence => some 1539
  | .shiningMoon => some 1540
  | .livingGore => some 1541
  | .flowingMagma => some 1542
  | .theCreationOfTheGuide => some 1573
  | .flurryBoots => some 1579
  | .celestialMagnet => some 2219
  | .fiberglassFishingPole => some 2292
  | .obsidianSwordfish => some 2331
  | .cratePotion => some 2356
  | .sparky => some 2995
  | .flowerBoots => some 3017
  | .goldenLockBox => some 3085
  | .sturdyFossil => some 3380
  | .duneriderBoots => some 4055
  | .ancientChisel => some 4056
  | .stormSpear => some 4061
  | .thunderZapper => some 4062
  | .shellPile => some 4090
  | .snakeCharmersFlute => some 4262
  | .magicConch => some 4263
  | .bastStatue => some 4276
  | .innerTube => some 4404
  | .scarabBomb => some 4423
  | .sharkBait => some 4425
  | .scarabFishingRod => some 4442
  | .sandcastleBucket => some 4460
  | .bamboo => some 4564
  | .andrewSphinx => some 4626
  | .watchfulAntlion => some 4627
  | .burningSpirit => some 4628
  | .jawsOfDeath => some 4629
  | .theSandsOfSlime => some 4630
  | .snakesIHateSnakes => some 4631
  | .lifeAboveTheSand => some 4632
  | .oasis => some 4633
  | .prehistoryPreserved => some 4634
  | .ancientTablet => some 4635
  | .uluru => some 4636
  | .visitingThePyramids => some 4637
  | .bandageBoy => some 4638
  | .divineEye => some 4639
  | .demonConch => some 4819
  | .bottomlessLavaBucket => some 4820
  | .flameWakerBoots => some 4822
  | .wetBomb => some 4824
  | .superheatedBlood => some 4828
  | .hangingPot => some 4858
  | .miniVolcano => some 4868
  | .lavaAbsorbantSponge => some 4872
  | .obsidianLockBox => some 4879
  | .lavaSerpentBowl => some 4880
  | .lavaproofFishingHook => some 4881
  | .pottedMagmaPalm => some 4902
  | .pottedBrimstoneBush => some 4903
  | .pottedFireBrambles => some 4904
  | .pottedLavaBulb => some 4905
  | .pottedEmberTendrils => some 4906
  | .fledglingWings => some 4978
  | .highPitch => some 5226
  | .remnantsOfDevotion => some 5234
  | .constellation => some 5238
  | .blessingFromTheHeavens => some 5254
  | .loveIsInTheTrashSlot => some 5255
  | .seeTheWorldForWhatItIs => some 5258
  | .eyeOfTheSun => some 5388
  | .ofSeaAndDreams => some 5527
  | .glacierFang => some 6153
  | .seaweedPet => some 753
  | .muramasa => some 155
  | .ramRune => some 5465
  | .silverBracer => some 6156
  | .mudBlock => some 176
  | _ => none

def Item.underworldTextureId : Item → Option Nat
  | .treasureMagnet => some 5010
  | .demonicHellcart => some 4443
  | .ornateShadowKey => some 4737
  | .sliceOfHellCake => some 4551
  | .dynamite => some 167
  | .meteoriteBar => some 117
  | .hellfireArrow => some 265
  | .silverBullet => some 278
  | .tungstenBullet => some 4915
  | .restorationPotion => some 227
  | .spelunkerPotion => some 296
  | .featherfallPotion => some 295
  | .manaRegenerationPotion => some 293
  | .obsidianSkinPotion => some 288
  | .magicPowerPotion => some 294
  | .invisibilityPotion => some 297
  | .hunterPotion => some 304
  | .heartreachPotion => some 2323
  | .gravitationPotion => some 305
  | .thornsPotion => some 301
  | .waterWalkingPotion => some 302
  | .battlePotion => some 300
  | .teleportationPotion => some 2351
  | .infernoPotion => some 2348
  | .lifeforcePotion => some 2345
  | .recallPotion => some 2350
  | .potionOfReturn => some 4870
  | .glowstick => some 282
  | .froggyNeckband => some 5499
  | .goatsTuft => some 5500
  | .oldCompanionLocket => some 5501
  | .catChime => some 5502
  | .dogCollar => some 5503
  | .turkeyWattleNecklace => some 5504
  | .meanGoblinSpikes => some 5505
  | .crowsBeak => some 5506
  | .balloonyBeads => some 5507
  | .grimOldBarb => some 5508
  | .vampirePendant => some 5509
  | .cowBell => some 5484
  | .chickenCharm => some 5485
  | .fairyChoker => some 5534
  | .hellforge => some 221
  | .hellstoneBar => some 175
  | .bugNet => some 1991
  | .lavaproofBugNet => some 4821
  | .hellButterfly => some 4845
  | .lavafly => some 4847
  | .magmaSnail => some 4849
  | .cascade => some 3282
  | .boneSerpentKite => some 4610
  | .goldenBugNet => some 3183
  | _ => none

def Item.wallTextureId : Item → Option Nat
  | .pwnhammer => some 367
  | .breakerBlade => some 426
  | .clockworkAssaultRifle => some 434
  | .laserRifle => some 514
  | .firecracker => some 4912
  | .warriorEmblem => some 490
  | .rangerEmblem => some 491
  | .sorcererEmblem => some 489
  | .summonerEmblem => some 2998
  | .wallOfFleshMask => some 2105
  | .demoniteBrick => some 577
  | .crimtaneBrick => some 2793
  | .wallOfFleshTrophy => some 1365
  | _ => none

def Item.hardmodeTextureId : Item → Option Nat
  | .pearlstone => some 409
  | .pearlsand => some 408
  | .ebonsand => some 370
  | .crimsand => some 1246
  | .purpleIce => some 833
  | .pinkIce => some 834
  | .redIce => some 835
  | .cobaltOre => some 364
  | .palladiumOre => some 1104
  | .mythrilOre => some 365
  | .orichalcumOre => some 1105
  | .adamantiteOre => some 366
  | .titaniumOre => some 1106
  | .moltenPickaxe => some 122
  | .cobaltBar => some 381
  | .palladiumBar => some 1184
  | .mythrilBar => some 382
  | .orichalcumBar => some 1191
  | .adamantiteBar => some 391
  | .titaniumBar => some 1198
  | .cobaltPickaxe => some 776
  | .palladiumPickaxe => some 1188
  | .mythrilPickaxe => some 777
  | .orichalcumPickaxe => some 1195
  | .adamantitePickaxe => some 778
  | .titaniumPickaxe => some 1202
  | .cobaltDrill => some 385
  | .palladiumDrill => some 1189
  | .mythrilDrill => some 386
  | .orichalcumDrill => some 1196
  | .adamantiteDrill => some 388
  | .titaniumDrill => some 1203
  | .mythrilAnvil => some 525
  | .orichalcumAnvil => some 1220
  | .adamantiteForge => some 524
  | .titaniumForge => some 1221
  | .soulOfNight => some 521
  | .soulOfLight => some 520
  | .crystalShard => some 502
  | .mechanicalEye => some 544
  | .mechanicalWorm => some 556
  | .mechanicalSkull => some 557
  | .gelatinCrystal => some 4988
  | .cursedFlame => some 522
  | .vitamins => some 892
  | .meatGrinder => some 996
  | .applePie => some 4011
  | .crystallize => some 5488
  | .eaterOfLife => some 5489
  | .monsterMeat => some 5091
  | .soulOfSight => some 549
  | .hallowedBar => some 1225
  | .greaterHealingPotion => some 499
  | .twinMask => some 2106
  | .retinazerTrophy => some 1368
  | .spazmatismTrophy => some 1369
  | .bass => some 2290
  | .armoredCavefish => some 2303
  | .specularFish => some 2309
  | .goldenCarp => some 2308
  | .flarefinKoi => some 2312
  | .obsidifish => some 2315
  | .seaweed => some 2338
  | .tinCan => some 2339
  | .oldShoe => some 2337
  | .jojaCola => some 5275
  | .canOfWorms => some 4345
  | .worm => some 2002
  | .enchantedNightcrawler => some 3191
  | .goldWorm => some 2895
  | .woodenCrate => some 2334
  | .ironCrate => some 2335
  | .goldenCrate => some 2336
  | .pearlwoodCrate => some 3979
  | .mythrilCrate => some 3980
  | .titaniumCrate => some 3981
  | .apprenticeBait => some 2674
  | .journeymanBait => some 2675
  | .masterBait => some 2676
  | .aglet => some 285
  | .climbingClaws => some 953
  | .stepStool => some 4341
  | .cordageGuide => some 3068
  | .radar => some 3084
  | .poisonBarb => some 6165
  | .sailfishBoots => some 3200
  | .tsunamiInABottle => some 3201
  | .anchor => some 2424
  | .extractinator => some 997
  | .enchantedSundial => some 3064
  | .gingerBeard => some 2501
  | .tartarSauce => some 2587
  | .falconBlade => some 2608
  | .hardySaddle => some 2491
  | .enchantedSword => some 989
  | .swiftnessPotion => some 290
  | .ironskinPotion => some 292
  | .nightOwlPotion => some 299
  | .shinePotion => some 298
  | .gillsPotion => some 291
  | .miningPotion => some 2322
  | .trapsightPotion => some 2329
  | .calmingPotion => some 2324
  | .flipperPotion => some 2327
  | .lesserManaPotion => some 110
  | .manaPotion => some 189
  | .fastClock => some 889
  | .trifoldMap => some 893
  | _ => none

def Item.legacyPickRank : Item → Nat
  | .basicPick => 1
  | .copperPick => 2
  | .crystalPick => 3
  | _ => 0

def Item.pickPower : Item → Nat
  | .basicPick | .copperPick | .tinPick => 35
  | .ironPick => 40
  | .leadPick => 43
  | .silverPick | .crystalPick => 45
  | .tungstenPick => 50
  | .goldPick => 55
  | .platinumPick | .reaverShark => 59
  | .nightmarePickaxe => 65
  | .deathbringerPickaxe => 70
  | .moltenPickaxe => 100
  | .cobaltPickaxe | .cobaltDrill => 110
  | .palladiumPickaxe | .palladiumDrill => 130
  | .mythrilPickaxe | .mythrilDrill => 150
  | .orichalcumPickaxe | .orichalcumDrill => 165
  | .adamantitePickaxe | .adamantiteDrill => 180
  | .titaniumPickaxe | .titaniumDrill => 190
  | .picksaw => 210
  | _ => 0

def Item.axePower : Item → Nat
  | .copperAxe => 35
  | .tinAxe => 40
  | .ironAxe => 45
  | .leadAxe | .silverAxe => 50
  | .tungstenAxe | .goldAxe => 55
  | .platinumAxe => 60
  | .sawtoothShark => 70
  | .picksaw => 25
  | _ => 0

def Item.toolSpeed : Item → Nat
  | .basicPick | .copperPick | .platinumPick => 15
  | .tinPick | .deathbringerPickaxe => 14
  | .nightmarePickaxe => 15
  | .ironPick | .reaverShark => 13
  | .leadPick | .crystalPick => 12
  | .silverPick => 11
  | .tungstenPick | .ironAxe | .leadAxe => 19
  | .goldPick | .platinumAxe => 17
  | .copperAxe => 21
  | .tinAxe => 20
  | .silverAxe | .tungstenAxe | .goldAxe => 18
  | .moltenPickaxe => 18
  | .cobaltPickaxe => 13
  | .palladiumPickaxe => 12
  | .mythrilPickaxe => 10
  | .orichalcumPickaxe => 9
  | .adamantitePickaxe => 8
  | .titaniumPickaxe | .cobaltDrill | .palladiumDrill => 7
  | .mythrilDrill => 6
  | .picksaw => 6
  | .orichalcumDrill => 5
  | .adamantiteDrill | .titaniumDrill | .sawtoothShark => 4
  | .pwnhammer | .rockfish => 14
  | _ => 0

def Item.hammerPower : Item → Nat
  | .woodHammer => 25
  | .rockfish => 70
  | .pwnhammer => 80
  | _ => 0

def Item.swordDamage : Item → Nat
  | .copperShortsword => 5
  | .woodSword => 7
  | .copperSword => 9
  | .tinSword => 10
  | .ironSword => 12
  | .leadSword => 13
  | .silverSword | .tungstenSword => 14
  | .goldSword | .zombieArm => 15
  | .platinumSword => 16
  | .stoneSword => 1
  | .frostbrand => 3
  | .ruinHammer => 5
  | .blueMoon => 54
  | .valor => 28
  | .sunfury => 64
  | .darkLance => 34
  | .batBat => 36
  | .pwnhammer => 26
  | _ => 0

def Item.swordUseTime : Item → Nat
  | .copperShortsword => 13
  | .copperSword => 21
  | .woodSword | .tinSword | .ironSword | .leadSword | .silverSword => 20
  | .tungstenSword => 19
  | .goldSword => 18
  | .platinumSword => 17
  | .zombieArm => 22
  | .blueMoon => 45
  | .valor => 25
  | .sunfury => 45
  | .darkLance => 22
  | .batBat => 40
  | .pwnhammer => 27
  | _ => swingCooldown

def Item.legacySwordDamage (item : Item) : Nat :=
  match item with
  | .ironSword => 2
  | .silverSword => 4
  | _ => item.swordDamage

def Item.rangedDamage : Item → Nat
  | .bow => 4
  | .copperBow => 6
  | .tinBow => 7
  | .ironBow => 8
  | .leadBow | .silverBow => 9
  | .tungstenBow => 10
  | .goldBow => 11
  | .platinumBow => 13
  | .demonBow => 14
  | .tendonBow => 19
  | .hellwingBow => 22
  | .musket => 31
  | .undertaker => 19
  | .handgun => 26
  | _ => 0

def Item.bowUseTime : Item → Nat
  | .copperBow => 29
  | .tinBow | .ironBow => 28
  | .leadBow | .silverBow => 27
  | .tungstenBow | .goldBow => 26
  | .platinumBow | .demonBow => 25
  | .hellwingBow => 13
  | _ => 30

def Item.bowShot : Item → Option ShotKind
  | .bow => some .arrow
  | .copperBow | .tinBow | .ironBow | .leadBow | .silverBow | .tungstenBow
  | .goldBow | .platinumBow | .demonBow | .tendonBow | .hellwingBow => some .woodenArrow
  | .stormBow => some .stormArrow
  | .hunterBow => some .hunterArrow
  | .highlandCrossbow => some .highlandBolt
  | _ => none

def Item.gunShot : Item → Option ShotKind
  | .musket => some .musketBullet
  | .undertaker => some .undertakerBullet
  | .handgun => some .handgunBullet
  | _ => none

def Item.gunUseTime : Item → Nat
  | .musket => 32
  | .undertaker => 20
  | .handgun => 15
  | _ => 0

def Item.isBomb : Item → Bool
  | .bomb | .stickyBomb | .dynamite => true
  | _ => false

def Item.netUseTime : Item → Nat
  | .bugNet => 25
  | .lavaproofBugNet => 21
  | .goldenBugNet => 15
  | _ => 0

def Item.isLavaBait : Item → Bool
  | .hellButterfly | .lavafly | .magmaSnail => true
  | _ => false

def Item.fishingBaitPower : Item → Nat
  | .bait | .hellButterfly | .apprenticeBait => 15
  | .lavafly | .worm => 25
  | .journeymanBait => 30
  | .magmaSnail | .enchantedNightcrawler => 35
  | .goldWorm | .masterBait => 50
  | _ => 0

def Item.isFishingBait (item : Item) : Bool := item.fishingBaitPower > 0

def Item.isFishingCrate : Item → Bool
  | .woodenCrate | .ironCrate | .goldenCrate | .pearlwoodCrate | .mythrilCrate | .titaniumCrate
  | .corruptCrate | .crimsonCrate | .dungeonCrate | .skyCrate | .hallowedCrate
  | .jungleCrate | .defiledCrate | .hematicCrate | .stockadeCrate | .azureCrate
  | .divineCrate | .brambleCrate | .frozenCrate | .borealCrate | .oasisCrate
  | .mirageCrate | .obsidianCrate | .hellstoneCrate | .oceanCrate | .seasideCrate => true
  | _ => false

def Item.isQuestFish (item : Item) : Bool :=
  (item.anglerTextureId.map fun id => (2450 ≤ id && id ≤ 2488) || id == 4393 || id == 4394).getD false

def Item.isFishingCatch : Item → Bool
  | .bass | .armoredCavefish | .specularFish | .goldenCarp | .flarefinKoi | .obsidifish
  | .seaweed | .tinCan | .oldShoe | .jojaCola | .demonConch | .bottomlessLavaBucket
  | .lavaAbsorbantSponge | .obsidianSwordfish => true
  | item => item.isFishingCrate || item.isQuestFish || item.waterCatchTextureId.isSome

def Item.isCrateContainer (item : Item) : Bool :=
  item.isFishingCrate || item == .goldenLockBox || item == .obsidianLockBox

def Item.fishingPolePower : Item → Nat
  | .fishingRod => 5
  | .fiberglassFishingPole | .scarabFishingRod => 30
  | .goldenFishingRod => 50
  | .hotlineFishingHook => 45
  | _ => 0

def Item.fishingCastSpeed : Item → Float32
  | .fiberglassFishingPole => 14
  | .scarabFishingRod => 15
  | .goldenFishingRod => 17
  | .hotlineFishingHook => 16.5
  | _ => 9

def Item.fishingBobberTexture : Item → String
  | .fiberglassFishingPole => "Projectile_362"
  | .scarabFishingRod => "Projectile_775"
  | .goldenFishingRod => "Projectile_364"
  | .hotlineFishingHook => "Projectile_382"
  | _ => "Projectile_360"

def Item.isFishingPole (item : Item) : Bool := item.fishingPolePower > 0

def Item.coinValue : Item → Nat
  | .copperCoin => 1
  | .silverCoin => 100
  | .goldCoin => 10000
  | .platinumCoin => 1000000
  | _ => 0

def Item.isAmmunition : Item → Bool
  | .arrow | .unholyArrow | .musketBall | .hellfireArrow | .silverBullet | .tungstenBullet => true
  | _ => false

structure Stack where
  item : Item
  count : Nat
  deriving BEq, Repr

abbrev Inventory := Array (Option Stack)

namespace Inventory

def hotbarCount : Nat := 10
def mainSlotCount : Nat := 50
def coinStart : Nat := mainSlotCount
def ammoStart : Nat := coinStart + 4
def trashSlot : Nat := ammoStart + 4
def slotCount : Nat := trashSlot + 1

def acceptsSlot (index : Nat) (item : Item) : Bool :=
  if index < coinStart then true
  else if index < ammoStart then item.coinValue > 0
  else if index < trashSlot then item.isAmmunition || item.isFishingBait
  else index == trashSlot

def maxStack : Nat := 99

def stackLimit : Item → Nat
  | .basicPick | .copperPick | .stoneSword | .crystalPick | .ironSword | .frostbrand | .silverSword | .ruinHammer => 1
  | .bow | .stormBow | .hunterBow | .fishingRod | .minecart => 1
  | .grapplingHook | .highlandCrossbow | .deepstoneAegis => 1
  | .geodeMail | .amberTreads | .watchhorn | .raidCrest | .woodHammer => 1
  | .copperShortsword | .woodSword | .copperSword | .tinSword | .leadSword => 1
  | .tungstenSword | .goldSword | .platinumSword => 1
  | .tinPick | .ironPick | .leadPick | .silverPick | .tungstenPick | .goldPick | .platinumPick => 1
  | .copperAxe | .tinAxe | .ironAxe | .leadAxe | .silverAxe | .tungstenAxe | .goldAxe | .platinumAxe => 1
  | .shackle | .zombieArm | .binoculars | .eyeMask => 1
  | .copperBow | .tinBow | .ironBow | .leadBow | .silverBow | .tungstenBow
  | .goldBow | .platinumBow | .demonBow | .tendonBow => 1
  | .musket | .undertaker | .shadowOrbPet | .crimsonHeartPet | .vilethorn | .ballOHurt
  | .bandOfStarpower | .panicNecklace | .crimsonRod | .rottedFork => 1
  | .nightmarePickaxe | .deathbringerPickaxe | .eaterMask | .brainMask | .eatersBone | .boneRattle => 1
  | .cobaltShield | .skeletronMask | .boneKey | .skeletronHand | .bookOfSkulls | .nazar | .aquaScepter
  | .blueMoon | .magicMissile | .valor | .handgun | .shadowKey | .sunfury | .darkLance | .obsidianSkull => 1
  | .flowerOfFire | .flamelash | .hellwingBow => 1
  | .obsidianRose | .magmaStone | .demonScythe | .plumbersHat | .batBat => 1
  | .treasureMagnet | .demonicHellcart | .ornateShadowKey | .sliceOfHellCake
  | .bugNet | .lavaproofBugNet | .goldenBugNet | .cascade | .boneSerpentKite => 1
  | .froggyNeckband | .goatsTuft | .oldCompanionLocket | .catChime | .dogCollar
  | .turkeyWattleNecklace | .meanGoblinSpikes | .crowsBeak | .balloonyBeads | .grimOldBarb
  | .vampirePendant | .cowBell | .chickenCharm | .fairyChoker => 1
  | .dynamite | .meteoriteBar | .hellfireArrow | .silverBullet | .tungstenBullet | .restorationPotion
  | .spelunkerPotion | .featherfallPotion | .manaRegenerationPotion | .obsidianSkinPotion | .magicPowerPotion
  | .invisibilityPotion | .hunterPotion | .heartreachPotion | .gravitationPotion | .thornsPotion
  | .waterWalkingPotion | .battlePotion | .teleportationPotion | .infernoPotion | .lifeforcePotion
  | .recallPotion | .potionOfReturn | .glowstick | .hellforge | .hellstoneBar
  | .hellButterfly | .lavafly | .magmaSnail => 9999
  | .pwnhammer | .breakerBlade | .clockworkAssaultRifle | .laserRifle | .firecracker
  | .warriorEmblem | .rangerEmblem | .sorcererEmblem | .summonerEmblem | .wallOfFleshMask => 1
  | .fastClock | .trifoldMap => 1
  | .moltenPickaxe | .cobaltPickaxe | .palladiumPickaxe | .mythrilPickaxe
  | .orichalcumPickaxe | .adamantitePickaxe | .titaniumPickaxe
  | .cobaltDrill | .palladiumDrill | .mythrilDrill | .orichalcumDrill | .adamantiteDrill | .titaniumDrill => 1
  | .guideVoodooDoll | .wallOfFleshTrophy | .demoniteBrick | .crimtaneBrick
  | .pearlstone | .pearlsand | .ebonsand | .crimsand | .purpleIce | .pinkIce | .redIce
  | .cobaltOre | .palladiumOre | .mythrilOre | .orichalcumOre | .adamantiteOre | .titaniumOre
  | .cobaltBar | .palladiumBar | .mythrilBar | .orichalcumBar | .adamantiteBar | .titaniumBar
  | .mythrilAnvil | .orichalcumAnvil | .adamantiteForge | .titaniumForge | .soulOfNight => 9999
  | .soulOfLight | .crystalShard | .mechanicalEye | .mechanicalWorm | .mechanicalSkull | .gelatinCrystal => 9999
  | .cursedFlame | .meatGrinder | .applePie | .crystallize | .eaterOfLife | .monsterMeat => 9999
  | .soulOfSight | .hallowedBar | .greaterHealingPotion | .retinazerTrophy | .spazmatismTrophy => 9999
  | .sparkleSlimeBalloon => 9999
  | .queenSlimeMask | .crystalAssassinHood | .crystalAssassinShirt | .crystalAssassinPants
  | .bladeStaff | .gelatinousPillion | .hookOfDissonance => 1
  | .queenSlimeTrophy | .soulOfMight | .destroyerTrophy | .soulOfFright | .skeletronPrimeTrophy => 9999
  | .destroyerMask | .skeletronPrimeMask => 1
  | .templeKey | .rocketI | .planteraTrophy => 9999
  | .lihzahrdBrick | .lihzahrdDoor | .lihzahrdChest | .lihzahrdPowerCell | .lihzahrdFurnace
  | .solarTablet | .solarTabletFragment | .jestersArrow | .flamingArrow | .throwingKnife
  | .archeryPotion | .titanPotion => 9999
  | .lihzahrdAltar | .superDartTrap | .flameTrap | .spikyBallTrap | .spearTrap | .woodenSpike
  | .lihzahrdPressurePlate | .beetleHusk | .styngerBolt | .golemTrophy => 9999
  | .picksaw | .stynger | .possessedHatchet | .sunStone | .eyeOfGolem | .heatRay | .staffOfEarth
  | .golemFist | .mobiusStrip | .golemMask | .lizardEgg => 1
  | .grenadeLauncher | .venusMagnum | .nettleBurst | .leafBlower | .flowerPow | .waspGun
  | .seedler | .vulgarDisplayOfFlower | .pygmyStaff | .thornHook | .theAxe | .seedling | .planteraMask => 1
  | .bass | .armoredCavefish | .specularFish | .goldenCarp | .honeyfin | .honeyBlock | .crispyHoneyBlock => 9999
  | .flarefinKoi | .obsidifish | .seaweed | .tinCan | .oldShoe | .jojaCola => 9999
  | .canOfWorms | .worm | .enchantedNightcrawler | .goldWorm => 9999
  | .aglet | .climbingClaws | .stepStool | .cordageGuide | .radar | .sailfishBoots | .tsunamiInABottle
  | .anchor | .gingerBeard | .tartarSauce | .falconBlade | .hardySaddle | .enchantedSword => 1
  | .woodenCrate | .ironCrate | .goldenCrate | .pearlwoodCrate | .mythrilCrate | .titaniumCrate
  | .apprenticeBait | .journeymanBait | .masterBait | .poisonBarb | .extractinator | .enchantedSundial
  | .swiftnessPotion | .ironskinPotion | .nightOwlPotion | .shinePotion | .gillsPotion | .miningPotion
  | .trapsightPotion | .calmingPotion | .flipperPotion | .lesserManaPotion | .manaPotion => 9999
  | .corruptCrate | .crimsonCrate | .dungeonCrate | .skyCrate | .hallowedCrate
  | .jungleCrate | .defiledCrate | .hematicCrate | .stockadeCrate | .azureCrate
  | .divineCrate | .brambleCrate | .frozenCrate | .borealCrate | .oasisCrate
  | .mirageCrate | .obsidianCrate | .hellstoneCrate | .oceanCrate | .seasideCrate
  | .book | .cloud | .shellPile | .bamboo | .sturdyFossil
  | .scarabBomb | .wetBomb | .ichor | .goldenLockBox | .obsidianLockBox
  | .hangingPot | .pottedMagmaPalm | .pottedBrimstoneBush | .pottedFireBrambles | .pottedLavaBulb
  | .pottedEmberTendrils | .cratePotion | .mudBlock => 9999
  | .starfury | .luckyHorseshoe | .shinyRedBalloon | .breathingReed | .flipper
  | .feralClaws | .ankletOfTheWind | .staffOfRegrowth | .trident | .fishPet
  | .iceBoomerang | .iceBlade | .sandstormInABottle | .waterWalkingBoots | .lavaCharm
  | .iceSkates | .boomstick | .blizzardInABottle | .snowballCannon | .flurryBoots
  | .celestialMagnet | .fiberglassFishingPole | .obsidianSwordfish | .flowerBoots | .duneriderBoots
  | .ancientChisel | .stormSpear | .thunderZapper | .snakeCharmersFlute | .magicConch
  | .innerTube | .sharkBait | .scarabFishingRod | .sandcastleBucket | .demonConch
  | .bottomlessLavaBucket | .flameWakerBoots | .superheatedBlood | .lavaAbsorbantSponge | .lavaSerpentBowl
  | .lavaproofFishingHook | .fledglingWings | .glacierFang => 1
  | .twinMask => 1
  | .batfish | .bumblebeeTuna | .catfish | .cloudfish | .cursedfish
  | .dirtfish | .dynamiteFish | .eaterofPlankton | .fallenStarfish | .theFishofCthulu
  | .fishotron | .harpyfish | .hungerfish | .ichorfish | .jewelfish
  | .mirageFish | .mutantFlinxfin | .pengfish | .pixiefish | .spiderfish
  | .tundraTrout | .unicornFish | .guideVoodooFish | .wyverntail | .zombieFish
  | .amanitaFungifin | .angelfish | .bloodyManowar | .bonefish | .bunnyfish
  | .capnTunabeard | .clownfish | .demonicHellfish | .derpfish | .fishron
  | .infectedScabbardfish | .mudfish | .slimefish | .tropicalBarracuda | .scarabFish
  | .scorpioFish | .fishingPotion | .sonarPotion | .lifePreserver | .shipsWheel
  | .compassRose | .wallAnchor | .pillaginMePixels | .treasureMap | .goldfishTrophy
  | .bunnyfishTrophy | .swordfishTrophy | .sharkteethTrophy | .shipInABottle | .seaweedPlanter
  | .notSoLostInParadise | .crustography | .fangs | .whatLurksBelow | .couchGag
  | .silentFish | .theDuke => 9999
  | .goldenFishingRod | .hotlineFishingHook | .fuzzyCarrot | .anglerHat | .anglerVest
  | .anglerPants | .finWings | .bottomlessWaterBucket | .superAbsorbantSponge | .anglerFishHook
  | .fishMinecart | .seashellHairpin | .mermaidAdornment | .mermaidTail | .fishCostumeMask
  | .fishCostumeShirt | .fishCostumeFinskirt | .highTestFishingLine | .anglerEarring | .tackleBox
  | .fishermansGuide | .weatherRadio | .sextant | .anglerBobber | .bottomlessHoneyBucket
  | .honeyAbsorbantSponge => 1
  | .rockfish | .purpleClubberfish | .swordfish | .reaverShark | .sawtoothShark
  | .zephyrFish | .frogLeg | .scalyTruffle | .crystalSerpent | .toxikarp
  | .bladetongue | .balloonPufferfish | .ladyOfTheLake | .barnacleStaff | .itsScragglinTime => 1
  | .seaweedPet | .muramasa | .ramRune | .silverBracer => 1
  | .vitamins => 1
  | .musketBall | .bomb | .stickyBomb | .ebonstone | .crimstone | .wormFood | .bloodySpine
  | .rottenChunk | .vertebra | .vileMushroom | .viciousMushroom | .vilePowder | .viciousPowder
  | .shadowScale | .tissueSample | .sand | .glass | .bottle
  | .copperCoin | .silverCoin | .goldCoin | .platinumCoin => 9999
  | .blueDungeonBrick | .greenDungeonBrick | .pinkDungeonBrick | .goldenKey | .healingPotion
  | .skeletronTrophy | .chippysCouch | .ashBlock | .obsidianBrick | .hellstone | .shadowChest => 9999
  | _ => maxStack

def count (inventory : Inventory) (item : Item) : Nat :=
  inventory.foldl (fun total slot =>
    match slot with
    | some stack => if stack.item == item then total + stack.count else total
    | none => total) 0 (stop := trashSlot)

def coinBalance (inventory : Inventory) : Nat :=
  inventory.foldl (fun total slot =>
    total + (slot.map fun stack => stack.item.coinValue * stack.count).getD 0) 0 (stop := trashSlot)

def addItems (inventory : Inventory) (item : Item) (quantity : Nat) :
    Option (Inventory × Nat) := do
  if quantity == 0 then none else do
    let mut updated := inventory
    let mut remaining := quantity
    let mut first := none
    let dedicated :=
      if item.isAmmunition || (item.isFishingBait && item != .bait) then (ammoStart, trashSlot)
      else if item.coinValue > 0 then (coinStart, ammoStart)
      else (0, 0)
    for (start, stop) in [dedicated, (0, min mainSlotCount inventory.size)] do
      for fillExisting in [true, false] do
        for index in [start:min stop inventory.size] do
          if remaining == 0 then break
          let slot := updated[index]?.join
          let room := if fillExisting then
              (slot.filter (·.item == item)).map (fun stack => stackLimit item - stack.count) |>.getD 0
            else if slot.isNone then stackLimit item else 0
          let amount := min remaining room
          if amount > 0 then
            let count := (slot.map (·.count)).getD 0 + amount
            updated := updated.set! index (some ⟨item, count⟩)
            remaining := remaining - amount
            if first.isNone then first := some index
    if remaining > 0 then none else first.map (updated, ·)

def addOne (inventory : Inventory) (item : Item) : Option (Inventory × Nat) :=
  addItems inventory item 1

/-- Change currency without consuming the trash slot or altering other items. -/
def setCoinBalance (inventory : Inventory) (balance : Nat) : Option Inventory := do
  if balance == coinBalance inventory then return inventory
  let mut result := inventory
  for index in [:min trashSlot inventory.size] do
    if (result[index]?.join).any (fun stack => stack.item.coinValue > 0) then
      result := result.set! index none
  let mut remaining := balance
  for item in [Item.platinumCoin, .goldCoin, .silverCoin, .copperCoin] do
    let amount := remaining / item.coinValue
    if amount > 0 then
      let (updated, _) ← addItems result item amount
      result := updated
      remaining := remaining % item.coinValue
  return result

def creditCoins (inventory : Inventory) (amount : Nat) : Option Inventory :=
  if amount == 0 then some inventory else setCoinBalance inventory (coinBalance inventory + amount)

def removeFromSlot (inventory : Inventory) (index : Nat) (quantity : Nat) :
    Inventory :=
  match inventory[index]?.join with
  | none => inventory
  | some stack =>
    let remaining := stack.count - quantity
    inventory.set! index (if remaining == 0 then none else some { stack with count := remaining })

def transfer (source destination : Inventory) (index : Nat) : Option (Inventory × Inventory) := do
  let stack ← source[index]?.join
  let (destination, _) ← addItems destination stack.item stack.count
  return (source.set! index none, destination)

/-- Remove up to `quantity` units of `item`, starting at the first slot. -/
def remove (inventory : Inventory) (item : Item) (quantity : Nat) : Inventory := Id.run do
  let mut result := inventory
  let mut remaining := quantity
  for index in [:min trashSlot inventory.size] do
    if remaining == 0 then break
    if let some stack := result[index]?.join then
      if stack.item == item then
        let used := min remaining stack.count
        result := removeFromSlot result index used
        remaining := remaining - used
  return result

/-- Left click moves or combines a stack. Right click takes one item from it. -/
def click (inventory : Inventory) (cursor : Option Stack) (index : Nat) (split : Bool) :
    Inventory × Option Stack := Id.run do
  let some slot := inventory[index]? | return (inventory, cursor)
  if split then
    let some stack := slot | return (inventory, cursor)
    let count := (cursor.map (·.count)).getD 0
    if cursor.any (·.item != stack.item) || count ≥ stackLimit stack.item then
      return (inventory, cursor)
    return (removeFromSlot inventory index 1, some ⟨stack.item, count + 1⟩)
  let some held := cursor | return (inventory.set! index none, slot)
  if !acceptsSlot index held.item then return (inventory, cursor)
  if index == trashSlot then return (inventory.set! index cursor, none)
  if let some stack := slot then
    if stack.item == held.item then
      let amount := min held.count (stackLimit held.item - stack.count)
      let remaining := held.count - amount
      return (inventory.set! index (some { stack with count := stack.count + amount }),
        if remaining == 0 then none else some { held with count := remaining })
  return (inventory.set! index cursor, slot)

end Inventory

end Lean4Terria
