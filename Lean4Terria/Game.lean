import Lean4Terria.Warden
import Lean4Terria.Night
import Lean4Terria.Housing
import Lean4Terria.Home
import Lean4Terria.Quarry
import Lean4Terria.Storm
import Lean4Terria.Trade
import Lean4Terria.Chart
import Lean4Terria.Raid
import Lean4Terria.Ecology
import Lean4Terria.Fishing
import Lean4Terria.Angler
import Lean4Terria.Garden
import Lean4Terria.Ruins
import Lean4Terria.Cavern
import Lean4Terria.Magma
import Lean4Terria.EyeOfCthulhu
import Lean4Terria.Bedroom
import Lean4Terria.ClassicCrafting
import Lean4Terria.Rail
import Lean4Terria.Grapple
import Lean4Terria.Explosives
import Lean4Terria.EaterOfWorlds
import Lean4Terria.BrainOfCthulhu
import Lean4Terria.EvilFlyers
import Lean4Terria.Skeletron
import Lean4Terria.DungeonSlime
import Lean4Terria.DarkCaster
import Lean4Terria.AngryBones
import Lean4Terria.CursedSkull
import Lean4Terria.AquaScepter
import Lean4Terria.Shortsword
import Lean4Terria.Flail
import Lean4Terria.MagicMissile
import Lean4Terria.Yoyo
import Lean4Terria.DarkLance
import Lean4Terria.UnderworldShots
import Lean4Terria.Flamelash
import Lean4Terria.UnderworldCreatures
import Lean4Terria.FireImp
import Lean4Terria.DemonSickle
import Lean4Terria.ShadowChestLoot
import Lean4Terria.JungleTemple
import Lean4Terria.TempleWiring
import Lean4Terria.TempleEnemies
import Lean4Terria.GolemBody
import Lean4Terria.GolemHead
import Lean4Terria.GolemFists
import Lean4Terria.BoneSerpent
import Lean4Terria.UnderworldCritters
import Lean4Terria.WallMinions
import Lean4Terria.VoodooDoll
import Lean4Terria.HardmodeOres
import Lean4Terria.HardmodeSpread
import Lean4Terria.Wraith
import Lean4Terria.CavernEnemies
import Lean4Terria.Destroyer
import Lean4Terria.PrimeArms
import Lean4Terria.PlanteraBulbs
import Lean4Terria.PlanteraHead
import Lean4Terria.PlanteraParts
import Lean4Terria.QueenSlime
import Lean4Terria.WingedSlime
import Lean4Terria.BladeStaff

namespace Lean4Terria

def chestSlots : Nat := 40
def maxAngryBones : Nat := 10
def maxCursedSkulls : Nat := 14
def maxCursedTicks : Nat := 240
def maxSlowTicks : Nat := 900
def maxWeakTicks : Nat := 7200
def maxCavernFlames : Nat := 24
def maxUnderworldNpcs : Nat := 20
def maxUnderworldCritters : Nat := 200
def maxDarknessTicks : Nat := 900
def maxObsidianSkinTicks : Nat := 21600

structure Chest where
  position : TilePos
  slots : Inventory
  deriving BEq, Repr

def initialTempleChests (seed : UInt64) (layout : TempleLayout) : Array Chest :=
  layout.chestAnchors.map fun position =>
    let random := UnifiedRandom.fromEntropy (hash (seed, position))
    let loot := templeChestLoot (earlyOres seed) random
    ⟨position, loot.map some ++ Array.replicate (chestSlots - loot.size) none⟩

def initialDungeonChests (seed : UInt64) (shadowKey := false) : Array Chest :=
  let layout := dungeonLayout seed
  let empty : Inventory := Array.replicate chestSlots none
  let gold := empty.set! 0 (some ⟨.cobaltShield, 1⟩)
  let gold := if shadowKey then gold.set! 1 (some ⟨.shadowKey, 1⟩) else gold
  #[⟨layout.keyChest, empty.set! 0 (some ⟨.goldenKey, 1⟩)⟩,
    ⟨layout.goldChest, gold⟩]

def initialLowerDungeonChest (seed : UInt64) : Chest :=
  let empty : Inventory := Array.replicate chestSlots none
  ⟨(dungeonLayout seed).lowerGoldChest, empty.set! 0 (some ⟨.aquaScepter, 1⟩)⟩

def initialBlueMoonDungeonChest (seed : UInt64) : Chest :=
  let empty : Inventory := Array.replicate chestSlots none
  ⟨(dungeonLayout seed).blueMoonChest, empty.set! 0 (some ⟨.blueMoon, 1⟩)⟩

def initialMagicMissileDungeonChest (seed : UInt64) : Chest :=
  let empty : Inventory := Array.replicate chestSlots none
  ⟨(dungeonLayout seed).magicMissileChest, empty.set! 0 (some ⟨.magicMissile, 1⟩)⟩

def initialDeepDungeonChests (seed : UInt64) : Array Chest :=
  let layout := dungeonLayout seed
  let empty : Inventory := Array.replicate chestSlots none
  #[⟨layout.valorChest, empty.set! 0 (some ⟨.valor, 1⟩)⟩,
    ⟨layout.deepKeyChest, empty.set! 0 (some ⟨.goldenKey, 1⟩)⟩,
    ⟨layout.handgunChest, empty.set! 0 (some ⟨.handgun, 1⟩)⟩]

private def shadowChestLoot (positions : Array TilePos) (items : Array Item) : Array Chest :=
  let empty : Inventory := Array.replicate chestSlots none
  (positions.zip items).map (fun (position, item) =>
    ⟨position, empty.set! 0 (some ⟨item, 1⟩)⟩)

def initialLegacyShadowChests (seed : UInt64) : Array Chest :=
  let loot : Array Item := if (hash (seed, (329 : Nat))).toNat % 2 == 0 then #[.darkLance, .sunfury]
    else #[.sunfury, .darkLance]
  shadowChestLoot (underworldLayout seed).legacyShadowChests loot

-- WorldGen.Reset repeatedly draws one item from the remaining pool.
private def shadowChestPrimaryDeck (seed : UInt64) : Array Item := Id.run do
  let mut remaining : Array Item := #[.darkLance, .sunfury, .flowerOfFire, .flamelash, .hellwingBow]
  let mut deck := #[]
  for draw in [:remaining.size] do
    let index := (hash (seed, (328 : Nat), draw)).toNat % remaining.size
    deck := deck.push remaining[index]!
    remaining := remaining.eraseIdxIfInBounds index
  return deck

def initialPrimaryShadowChests (seed : UInt64) : Array Chest :=
  shadowChestLoot (underworldLayout seed).shadowChests (shadowChestPrimaryDeck seed)

def initialShadowChests (seed : UInt64) : Array Chest :=
  (initialPrimaryShadowChests seed).map fun chest =>
    let roll (draw : Nat) := (hash (seed, chest.position, draw, (330 : Nat))).toNat
    let extras := shadowChestExtras (earlyOres seed) roll
    let slots := chest.slots.mapIdx fun index primary => if index == 0 then primary else extras[index - 1]?
    { chest with slots }

def initialAdditionalShadowChests (seed : UInt64) : Array Chest :=
  let positions := (underworldLayout seed).shadowChests
  let loot := (shadowChestPrimaryDeck seed).filter (fun item => item != .darkLance && item != .sunfury)
  shadowChestLoot (positions.extract 2 positions.size) loot

def oldManReturnTick : Nat := 7200
def dungeonGuardianMaxHealth : Nat := 9999
def dungeonGuardianSpeed : Int := 8 * (tileUnits / 16)

inductive CraftError where
  | alreadyOwned | insufficientCopper | inventoryFull
  deriving BEq, Repr

def copperPickCost : Nat := 3
def crystalPickCost : Nat := 3
def ironSwordCost : Nat := 4
def startingWood : Nat := 4
def workbenchCost : Nat := 4
def chestCost : Nat := 4
def doorCost : Nat := 3
def silverSwordCost : Nat := 4
def bowCost : Nat := 5
def arrowBatch : Nat := 12
def platformWood : Nat := 1
def platformBatch : Nat := 4
def beaconWood : Nat := 4
def beaconStone : Nat := 1
def recallCopper : Nat := 1
def recallCrystal : Nat := 1
def gelTorchBatch : Nat := 6
def hunterBowWood : Nat := 4
def rodWood : Nat := 5
def rodStone : Nat := 1
def baitBatch : Nat := 4
def eelBreathTicks : Nat := 240
def gillsTicks : Nat := 1200
def barkskinTicks : Nat := 1800
def bowCooldownTicks : Nat := 15
def woodenBowCooldown : Nat := 30
def highlandCrossbowCooldown : Nat := 30
def highlandCrossbowOreCost : Nat := 4
def highlandCrossbowWoodCost : Nat := 2
def maxProjectiles : Nat := 128
def railWoodCost : Nat := 1
def railIronCost : Nat := 1
def railBatch : Nat := 48
def minecartWoodCost : Nat := 2
def minecartIronCost : Nat := 3
def maxMinecarts : Nat := 32
def grapplingHookIronCost : Nat := 2
def grapplingHookWoodCost : Nat := 1
def highlandBerryHealing : Nat := 2 * healthPerHeart

def CaveMobKind.dropItem : CaveMobKind → Item
  | .slime => .caveGel
  | .beetle => .frostShell
  | .skitter => .quarryScale
  | .oreback => .deepsteel

def tryCraft (inventory : Inventory) : Except CraftError (Inventory × Nat) :=
  if Inventory.count inventory .copperPick > 0 then
    .error .alreadyOwned
  else if Inventory.count inventory .copper < copperPickCost then
    .error .insufficientCopper
  else
    let spent := Inventory.remove inventory .copper copperPickCost
    match Inventory.addOne spent .copperPick with
    | some result => .ok result
    | none => .error .inventoryFull

theorem tryCraft_inventory_unchanged_of_insufficientCopper (inventory : Inventory)
    (h : Inventory.count inventory .copper < copperPickCost) :
    (match tryCraft inventory with
      | .ok (crafted, _) => crafted
      | .error _ => inventory) = inventory := by
  by_cases ownsPick : Inventory.count inventory .copperPick > 0 <;>
    simp [tryCraft, ownsPick, h]

def tileHitLimit : Nat := 500
def tileHitLifetime : Nat := 60
def maxToolCooldown : Nat := 21
def maxGelDrops : Nat := 128
def maxNightDrops : Nat := 128
def maxDropCoins : Nat := 999999
def potionSicknessTicks : Nat := 3600
def honeyBuffDuration : Nat := 1800
def guideMaxHealth : Nat := 250
def guideAttackCooldown : Nat := 60
def guideHurtCooldown : Nat := 30
def npcHitCooldown : Nat := 10
def maxNpcImmunity : Nat := 200
def blueMoonHitCooldown : Nat := 15
def magicMissileHitCooldown : Nat := 8
def magicMissileManaCost : Nat := 14
def magicMissileUseTime : Nat := 22
def magicMissileSlowUseTime : Nat := 55
def magicMissileDamage : Nat := 35

inductive NpcTarget where
  | crawler | warden | quarry | sentry | herald | sentinel | colossus
  | surface (kind : SurfaceKind)
  | eye | eyeServant (id : Nat)
  | eaterSegment (id : Nat) | brainCore | brainCreeper (id : Nat) | evilFlyer (id : Nat)
  | skeletron (part : SkeletronPart) | dungeonGuardian | dungeonSlime (id : Nat)
  | darkCaster (id : Nat) | waterSphere (id : Nat) | angryBones (id : Nat) | cursedSkull (id : Nat)
  | lavaSlime (id : Nat) | underworldFlyer (id : Nat) | fireImp (id : Nat) | burningSphere (id : Nat)
  | boneSerpent (id part : Nat) | underworldCritter (id : Nat)
  | wall (part : WallPart) | wallHungry (id : Nat) | wallLeech (id part : Nat)
  | wraith (id : Nat)
  | hardmodeCavern (id : Nat)
  | twin (kind : TwinKind)
  | destroyerSegment (part : Nat) | destroyerProbe (id : Nat)
  | primeHead | primeArm (kind : PrimeArmKind)
  | planteraHead | planteraTentacle (id : Nat) | planteraSpore (id : Nat)
  | golemBody | golemHead | golemLeftFist | golemRightFist
  | templeEnemy (id : Nat)
  | queenSlime | queenMinion (id : Nat)
  | raider (wave : Nat) (kind : RaidFoeKind)
  | cave (index : Nat)
  deriving BEq, Repr

structure NpcBurn where
  target : NpcTarget
  onFire : Nat := 0
  shadowflame : Nat := 0
  progress : Nat := 0
  hellfire : Nat := 0
  deriving BEq, Repr

structure BlueMoonAttack where
  flail : Flail
  aim : WorldAim
  cooldowns : Array (NpcTarget × Nat) := #[]
  deriving BEq, Repr

structure MagicMissileAttack where
  missile : MagicMissile
  aim : WorldAim
  target : Option NpcTarget := none
  deriving BEq, Repr

structure FlamelashAttack where
  projectile : Flamelash
  aim : WorldAim
  target : Option NpcTarget := none
  cooldowns : Array (NpcTarget × Nat) := #[]
  deriving BEq, Repr

def flamelashSlowUseTime : Nat := 75

structure ValorAttack where
  yoyo : Yoyo
  aim : WorldAim
  deriving BEq, Repr

inductive DaggerPhase where
  | idle
  | chase (target : NpcTarget)
  | recoil (elapsed : Nat)
  deriving BEq, Repr, Inhabited

structure DaggerSummon where
  blade : EnchantedDagger
  slot : Nat
  phase : DaggerPhase := .idle
  cooldowns : Array (NpcTarget × Nat) := #[]
  deriving BEq, Repr

structure TileHit where
  position : TilePos
  tile : Tile
  damage : Nat
  ticks : Nat
  deriving BEq, Repr, Inhabited

structure GelDrop where
  position : TilePos
  count : Nat
  deriving BEq, Repr, Inhabited

structure NightDrop where
  position : TilePos
  coins : Nat
  items : Array Stack := #[]
  deriving BEq, Repr, Inhabited

def Item.isNightDrop : Item → Bool
  | .lens | .blackLens | .shackle | .zombieArm => true
  | .demoniteOre | .crimtaneOre | .corruptSeeds | .crimsonSeeds | .lesserHealingPotion
  | .binoculars | .eyeMask | .eyeTrophy | .unholyArrow | .heart => true
  | .ebonstone | .crimstone | .musket | .undertaker | .musketBall | .bomb | .stickyBomb
  | .shadowOrbPet | .crimsonHeartPet | .vilethorn | .ballOHurt | .bandOfStarpower | .panicNecklace
  | .crimsonRod | .rottedFork | .stone | .dirt | .wood | .ice | .torch | .platform | .woodWall
  | .copper | .tinOre | .iron | .leadOre | .silver | .tungstenOre | .goldOre | .platinumOre
  | .crystal | .cobweb | .arrow => true
  | .rottenChunk | .vertebra | .vileMushroom | .viciousMushroom | .shadowScale | .tissueSample
  | .eaterMask | .eaterTrophy | .brainMask | .brainTrophy | .eatersBone | .boneRattle
  | .sand | .glass | .bottle => true
  | .skeletronMask | .skeletronTrophy | .chippysCouch | .boneKey | .healingPotion
  | .skeletronHand | .bookOfSkulls | .goldenKey | .bone | .nazar => true
  | .obsidianRose | .magmaStone | .demonScythe | .guideVoodooDoll | .plumbersHat | .batBat => true
  | .cascade | .boneSerpentKite | .hellButterfly | .lavafly | .magmaSnail => true
  | .pwnhammer | .breakerBlade | .clockworkAssaultRifle | .laserRifle | .firecracker
  | .warriorEmblem | .rangerEmblem | .sorcererEmblem | .summonerEmblem
  | .wallOfFleshMask | .wallOfFleshTrophy | .demoniteBrick | .crimtaneBrick => true
  | .dynamite | .ashBlock | .obsidianBrick => true
  | .pearlstone | .pearlsand | .ebonsand | .crimsand | .purpleIce | .pinkIce | .redIce
  | .soulOfNight | .soulOfLight | .fastClock | .crystalShard | .gelatinCrystal
  | .mechanicalWorm | .mechanicalEye | .mechanicalSkull | .caveGel | .cursedFlame | .vitamins
  | .meatGrinder | .applePie | .crystallize | .eaterOfLife | .monsterMeat => true
  | .soulOfSight | .hallowedBar | .greaterHealingPotion | .twinMask | .retinazerTrophy | .spazmatismTrophy
  | .soulOfMight | .destroyerMask | .destroyerTrophy => true
  | .soulOfFright | .skeletronPrimeMask | .skeletronPrimeTrophy => true
  | .templeKey | .grenadeLauncher | .rocketI | .venusMagnum | .nettleBurst | .leafBlower | .flowerPow | .waspGun
  | .seedler | .vulgarDisplayOfFlower | .pygmyStaff | .thornHook | .theAxe | .seedling | .planteraMask | .planteraTrophy => true
  | .lihzahrdDoor => true
  | .lihzahrdAltar | .superDartTrap | .flameTrap | .spikyBallTrap | .spearTrap
  | .woodenSpike | .lihzahrdPressurePlate | .beetleHusk | .picksaw | .stynger | .styngerBolt
  | .possessedHatchet | .sunStone | .eyeOfGolem | .heatRay | .staffOfEarth | .golemFist
  | .mobiusStrip | .golemMask | .golemTrophy | .lizardEgg => true
  | .bass | .armoredCavefish | .specularFish | .goldenCarp | .flarefinKoi | .obsidifish
  | .seaweed | .tinCan | .oldShoe | .jojaCola | .worm | .enchantedNightcrawler | .goldWorm => true
  | _ => false

/-- Recovery drops from Desktop 1.4.5.8
`NPC.DoDeathEvents_DropBossPotionsAndHearts` after choosing the potion type.
Terraria draws `Next(5, 16)` and `5 + Next(5)` independently. -/
def bossRecoveryDrops (potion : Item) (potionSample heartSample : Nat) : Array Stack :=
  #[⟨potion, 5 + potionSample % 11⟩, ⟨.heart, 5 + heartSample % 5⟩]

structure GuideNPC where
  body : Player
  health : Nat := guideMaxHealth
  direction : Int := 1
  attackCooldown : Nat := 0
  hurtCooldown : Nat := 0
  doorToClose : Option TilePos := none
  deriving BEq, Repr, Inhabited

def initialGuide (world : World) : Option GuideNPC :=
  (findStandingNear world 22 40 5 5 (fun _ => true)).map (fun body => { body })

def Tile.allowsToolCracks : Tile → Bool
  | .stone | .dirt | .ice | .timber | .treeTrunk | .copper | .tinOre | .iron | .leadOre
  | .silver | .tungstenOre | .goldOre | .platinumOre | .obsidian
  | .crystal | .deepsteel | .ruinBrick | .geode | .amber => true
  | .demoniteOre | .crimtaneOre | .corruptGrass | .crimsonGrass | .ebonstone | .crimstone => true
  | .sand | .glassBlock | .blueDungeonBrick | .greenDungeonBrick | .pinkDungeonBrick => true
  | .ash | .obsidianBrick | .hellstone | .lihzahrdBrick => true
  | .hallowedGrass | .pearlstone | .pearlsand | .ebonsand | .crimsand | .purpleIce | .pinkIce | .redIce
  | .cobaltOre | .palladiumOre | .mythrilOre | .orichalcumOre | .adamantiteOre | .titaniumOre => true
  | tile => (TempleDoor.kind tile).any (· != .locked)

def Tile.minimumPickPower : Tile → Nat
  | .obsidian => 55
  | .demoniteOre | .crimtaneOre => 55
  | .ebonstone | .crimstone => 65
  | .hellstone | .pearlstone => 65
  | .cobaltOre | .palladiumOre => 100
  | .mythrilOre | .orichalcumOre => 110
  | .adamantiteOre | .titaniumOre => 150
  | .deepsteel | .ruinBrick | .geode => 45
  | .blueDungeonBrick | .greenDungeonBrick | .pinkDungeonBrick => 100
  | .lihzahrdBrick | .lihzahrdAltar => 210
  | .lihzahrdAltarUpperLeft | .lihzahrdAltarUpperMiddle | .lihzahrdAltarUpperRight
  | .lihzahrdAltarLowerLeft | .lihzahrdAltarLowerMiddle | .lihzahrdAltarLowerRight
  | .lihzahrdDartTrap | .lihzahrdFlameTrap | .lihzahrdSpikyBallTrap | .lihzahrdSpearTrap => 210
  | .treeTrunk | .bedrock => 1000
  | _ => 1

private def Tile.pickDamage (tile : Tile) (power : Nat) : Nat :=
  if tile == .dirt || tile == .sand || tile == .corruptGrass || tile == .crimsonGrass || tile == .hallowedGrass ||
      tile == .pearlsand || tile == .ebonsand || tile == .crimsand then 2 * power
  else if tile == .hellstone then power + power / 2
  else if tile == .cobaltOre || tile == .palladiumOre then power / 2
  else if tile == .mythrilOre || tile == .orichalcumOre then power / 3
  else if tile == .adamantiteOre || tile == .titaniumOre || tile == .lihzahrdBrick then power / 4
  else if tile.allowsToolCracks then power
  else 100

structure GameState where
  seed : UInt64
  worldName : String := ""
  worldCreated : String := ""
  worldFavorite : Bool := false
  world : World
  autoTempleDoor : Option TilePos := none
  templeWires : ByteArray := .empty
  templeCooldowns : Array (TilePos × Nat) := #[]
  templeTrapShots : Array TempleTraps.Shot := #[]
  templeEnemies : Array TempleEnemy := #[]
  nextTempleEnemyId : Nat := 1
  templeEnemySound : Option (TempleEnemyKind × Bool) := none
  templeDartSound : Bool := false
  templeFlameSound : Bool := false
  hellfireTicks : Nat := 0
  hellfireProgress : Nat := 0
  bleedingTicks : Nat := 0
  spikeCooldown : Nat := 0
  golem : Option GolemBody := none
  golemHead : Option GolemHead := none
  golemFists : Array GolemFist := #[]
  golemShots : Array GolemShot := #[]
  golemDefeated : Bool := false
  golemLanded : Bool := false
  golemPunchSound : Bool := false
  golemFireballSound : Bool := false
  golemBeamSound : Bool := false
  golemImpactSound : Bool := false
  player : Player
  inventory : Inventory
  selected : Nat
  status : String
  combat : CombatState
  legacyEarly : Bool := false
  toolCooldown : Nat := 0
  tileHits : Array TileHit := #[]
  gelDrops : Array GelDrop := #[]
  nightDrops : Array NightDrop := #[]
  eye : Option EyeOfCthulhu := none
  eyeDefeated : Bool := false
  potionSickness : Nat := 0
  evilRelicsBroken : Nat := 0
  eater : Option EaterOfWorlds := none
  brain : Option BrainOfCthulhu := none
  evilBossDefeated : Bool := false
  evilEnemies : Array EvilFlyer := #[]
  dungeonInstalled : Bool := false
  lowerDungeonInstalled : Bool := false
  blueMoonDungeonInstalled : Bool := false
  magicMissileDungeonInstalled : Bool := false
  deepDungeonInstalled : Bool := false
  skeletron : Option Skeletron := none
  skeletronDefeated : Bool := false
  oldManWaiting : Bool := false
  dungeonGuardian : Option EyeMotion := none
  dungeonGuardianHealth : Nat := 0
  dungeonSlimes : Array DungeonSlime := #[]
  darkCasters : Array DarkCaster := #[]
  waterSpheres : Array WaterSphere := #[]
  nextDarkCasterId : Nat := 1
  nextWaterSphereId : Nat := 1
  angryBones : Array AngryBones := #[]
  nextAngryBonesId : Nat := 1
  cursedSkulls : Array CursedSkull := #[]
  nextCursedSkullId : Nat := 1
  cursedTicks : Nat := 0
  mana : Nat := baseMana
  manaRegenDelay : Nat := 0
  manaRegenProgress : Nat := 0
  aquaStreams : Array AquaStream := #[]
  aquaBurst : Option WorldAim := none
  aquaAnimation : Nat := 0
  aquaShotDelay : Nat := 0
  shortsword : Option ShortswordThrust := none
  blueMoon : Option BlueMoonAttack := none
  sunfury : Option BlueMoonAttack := none
  darkLance : Option DarkLance := none
  underworldSwing : Bool := false
  npcBurns : Array NpcBurn := #[]
  hotTileTicks : Nat := 0
  burningTicks : Nat := 0
  darknessTicks : Nat := 0
  obsidianSkinTicks : Nat := 0
  netSwingTicks : Nat := 0
  lavaSlimes : Array LavaSlime := #[]
  underworldFlyers : Array UnderworldFlyer := #[]
  fireImps : Array FireImp := #[]
  burningSpheres : Array BurningSphere := #[]
  boneSerpent : Option BoneSerpent := none
  wallOfFlesh : Option WallOfFlesh := none
  wallHungry : Array WallHungry := #[]
  wallLeeches : Array WallLeech := #[]
  wallLasers : Array WallLaser := #[]
  wallRoared : Bool := false
  wallDefeatedNow : Bool := false
  voodooDolls : Array DroppedVoodooDoll := #[]
  hardmode : Bool := false
  altarsSmashed : Nat := 0
  hardmodeOreMask : Nat := 0
  wraiths : Array Wraith := #[]
  nextWraithId : Nat := 1
  hardmodeCavern : Array CavernEnemy := #[]
  nextHardmodeCavernId : Nat := 1
  cavernFlames : Array CavernFlame := #[]
  cursedInfernoTicks : Nat := 0
  cursedInfernoProgress : Nat := 0
  weakTicks : Nat := 0
  retinazer : Option Twin := none
  spazmatism : Option Twin := none
  twinsDefeated : Bool := false
  twinsPending : Bool := false
  destroyer : Option Destroyer := none
  destroyerPending : Bool := false
  destroyerDefeated : Bool := false
  destroyerProbes : Array DestroyerProbe := #[]
  nextDestroyerProbeId : Nat := 1
  destroyerRoared : Bool := false
  destroyerBurrowed : Bool := false
  destroyerKilled : Bool := false
  destroyerHitSound : Bool := false
  probeShotSound : Bool := false
  probeKilled : Bool := false
  skeletronPrime : Option PrimeHead := none
  primeArms : Array PrimeArm := #[]
  primeShots : Array PrimeShot := #[]
  primePending : Bool := false
  primeDefeated : Bool := false
  primeRoared : Bool := false
  primeHitSound : Bool := false
  primeDestroyedPart : Bool := false
  primeBombLit : Bool := false
  primeBombExploded : Bool := false
  primeLaserFired : Bool := false
  primeLaserImpact : Bool := false
  plantera : Option PlanteraHead := none
  planteraHooks : Array PlanteraHook := #[]
  planteraTentacles : Array PlanteraTentacle := #[]
  planteraSpores : Array PlanteraSpore := #[]
  planteraShots : Array PlanteraShot := #[]
  nextPlanteraPartId : Nat := 1
  planteraBulbs : Array TilePos := #[]
  planteraUnlocked : Bool := false
  planteraDefeated : Bool := false
  planteraRoared : Bool := false
  planteraHitSound : Bool := false
  planteraKilledSound : Bool := false
  planteraShotSound : Bool := false
  planteraShotImpact : Bool := false
  poisonedTicks : Nat := 0
  poisonProgress : Nat := 0
  mechanicalNights : Nat := 0
  twinShots : Array TwinShot := #[]
  queenSlime : Option QueenSlime := none
  queenMinions : Array QueenMinion := #[]
  queenShots : Array QueenShot := #[]
  nextQueenMinionId : Nat := 1
  queenSlimeDefeated : Bool := false
  twinsRoared : Bool := false
  twinSlain : Bool := false
  twinsShotSound : Bool := false
  twinsFlameSound : Bool := false
  slowTicks : Nat := 0
  horrified : Bool := false
  tongued : Bool := false
  guideRespawnPending : Bool := false
  underworldCritters : Array UnderworldCritter := #[]
  nextUnderworldNpcId : Nat := 1
  demonSickles : Array DemonSickle := #[]
  underworldNpcSound : Bool := false
  shadowKeyRecovery : Bool := false
  magicMissiles : Array MagicMissileAttack := #[]
  magicMissileFlashes : Array (MagicMissile × Nat) := #[]
  magicMissileSound : Bool := false
  flowerFireballs : Array FlowerFireball := #[]
  hellwingShots : Array HellwingShot := #[]
  flamelashes : Array FlamelashAttack := #[]
  flamelashFlashes : Array (Flamelash × Nat) := #[]
  underworldMagicCast : Bool := false
  underworldMagicImpact : Bool := false
  hellwingFired : Bool := false
  valor : Option ValorAttack := none
  valorCast : Bool := false
  handgunFired : Bool := false
  openOldMan : Bool := false
  bombs : Array ActiveBomb := #[]
  bombFlash : Option (TilePos × Nat) := none
  walls : ByteArray := .empty
  furniture : Array Furniture := #[]
  bedSpawnPoint : Option TilePos := none
  respawnTicks : Nat := 0
  warden : WardenEncounter := .ready
  surface : SurfaceState := {}
  weather : Weather := {}
  crateLifeCrystals : Nat := 0
  chests : Array Chest := #[]
  openChest : Option TilePos := none
  homeDoor : Option TilePos := none
  homeAnchor : Option TilePos := none
  guide : Option TilePos := none
  townGuide : Option GuideNPC := none
  openGuide : Bool := false
  angler : AnglerState := {}
  openAngler : Bool := false
  showAnglerQuest : Bool := false
  water : Water := default
  magma : Water := default
  honey : Water := default
  honeyBuff : Nat := 0
  breath : Nat := maxBreath
  quarry : QuarryState := {}
  sentry : SentryState := {}
  storm : StormEncounter := .ready
  projectiles : Array Projectile := #[]
  npcImmunity : Array (NpcTarget × Nat) := #[]
  bowCooldown : Nat := 0
  chart : Chart := default
  cursorStack : Option Stack := none
  merchantStock : Array Nat := initialMerchantStock
  merchantDoor : Option TilePos := none
  merchant : Option TilePos := none
  openShop : Bool := false
  equipment : Equipment := {}
  raid : RaidEncounter := .ready
  ecology : CaveEcology := {}
  fishing : FishingState := .idle
  crops : Array Crop := #[]
  gills : Nat := 0
  barkskin : Nat := 0
  cratePotion : Nat := 0
  fishingPotion : Nat := 0
  sonarPotion : Nat := 0
  ruin : RuinEncounter := .ready
  cavern : CavernEncounter := .ready
  carts : Array TilePos := #[]
  ride : Option RailRide := none
  wingedSlime : Option WingedSlime := none
  dagger : Option DaggerSummon := none
  minionTarget : Option NpcTarget := none
  grapple : GrappleState := .idle
  assassinDash : AssassinDash := {}
  weaponArmorStats : Array (Nat × WeaponArmorStats) := #[]

def GameState.armedProjectileSlots (state : GameState) : Array Nat :=
  (state.projectiles.filter (fun shot => !shot.kind.hostile && shot.kind != .guideArrow)).filterMap (·.slot) ++
    state.aquaStreams.filterMap (·.slot) ++
    (state.shortsword.bind (·.slot)).toArray ++ (state.blueMoon.bind (·.flail.slot)).toArray ++
    (state.sunfury.bind (·.flail.slot)).toArray ++ (state.darkLance.bind (·.slot)).toArray ++
    state.magicMissiles.filterMap (·.missile.slot) ++ state.flowerFireballs.filterMap (·.slot) ++
    state.hellwingShots.filterMap (·.slot) ++ state.flamelashes.filterMap (·.projectile.missile.slot) ++
    (state.demonSickles.filter (!·.hostile)).filterMap (·.slot) ++ (state.valor.bind (·.yoyo.slot)).toArray

private def GameState.captureWeaponArmor (state before : GameState) : GameState :=
  let stats := state.equipment.weaponArmorStats
  if stats == {} then state
  else
    let old := before.armedProjectileSlots
    let newest := state.armedProjectileSlots.filter (!old.contains ·)
    { state with weaponArmorStats := state.weaponArmorStats ++ newest.map (·, stats) }

private def GameState.pruneWeaponArmor (state : GameState) : GameState :=
  if state.weaponArmorStats.isEmpty then state
  else
    let active := state.armedProjectileSlots
    { state with weaponArmorStats := state.weaponArmorStats.filter (active.contains ·.1) }

private def GameState.projectileSlots (state : GameState) : Array Nat :=
  state.projectiles.filterMap (·.slot) ++ state.aquaStreams.filterMap (·.slot) ++
    state.bombs.filterMap (·.slot) ++ (state.shortsword.bind (·.slot)).toArray ++
    (state.blueMoon.bind (·.flail.slot)).toArray ++ state.magicMissiles.filterMap (·.missile.slot) ++
    (state.valor.bind (·.yoyo.slot)).toArray ++ (state.sunfury.bind (·.flail.slot)).toArray ++
    (state.darkLance.bind (·.slot)).toArray ++ state.flowerFireballs.filterMap (·.slot) ++
    state.hellwingShots.filterMap (·.slot) ++ state.flamelashes.filterMap (·.projectile.missile.slot) ++
    state.demonSickles.filterMap (·.slot) ++ state.wallLasers.filterMap (·.slot) ++
    state.cavernFlames.filterMap (·.slot) ++ state.twinShots.filterMap (·.slot) ++
    state.primeShots.filterMap (·.slot) ++
    state.planteraShots.filterMap (·.slot) ++
    state.golemShots.filterMap (·.slot) ++ state.templeTrapShots.filterMap (·.slot) ++
    state.queenShots.filterMap (·.slot) ++ state.fishing.slot.toArray ++ (state.dagger.map (·.slot)).toArray

def GameState.validProjectileSlots (state : GameState) : Bool :=
  let slots := state.projectileSlots
  slots.all (· < maxProjectileSlots) && slots.toList.eraseDups.length == slots.size

private def firstFreeProjectileSlot (used : Array Nat) : Nat :=
  ((List.range (used.size + 1)).find? (fun slot => !used.contains slot)).getD used.size

private def assignSlots {α : Type} (values : Array α) (used : Array Nat)
    (slot : α → Option Nat) (setSlot : α → Nat → α) : Array α × Array Nat := Id.run do
  let mut values := values
  let mut used := used
  for index in [:values.size] do
    let some value := values[index]? | continue
    if (slot value).isNone then
      let next := firstFreeProjectileSlot used
      values := values.set! index (setSlot value next)
      used := used.push next
  return (values, used)

private def GameState.assignProjectileSlots (state : GameState) : GameState :=
  let used := state.projectileSlots
  let count := state.projectiles.size + state.aquaStreams.size + state.bombs.size + state.shortsword.toArray.size +
    state.blueMoon.toArray.size + state.magicMissiles.size + state.valor.toArray.size +
    state.sunfury.toArray.size + state.darkLance.toArray.size + state.flowerFireballs.size +
    state.hellwingShots.size + state.flamelashes.size + state.demonSickles.size + state.wallLasers.size +
    state.cavernFlames.size + state.twinShots.size + state.primeShots.size +
    state.planteraShots.size +
    state.golemShots.size + state.templeTrapShots.size +
    state.queenShots.size + state.fishing.slot.toArray.size +
    state.dagger.toArray.size
  if used.size == count then state
  else
    let (projectiles, used) := assignSlots state.projectiles used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (aquaStreams, used) := assignSlots state.aquaStreams used (·.slot) fun stream slot =>
      { stream with slot := some slot }
    let (bombs, used) := assignSlots state.bombs used (·.slot) fun bomb slot => { bomb with slot := some slot }
    let shortsword := state.shortsword.map fun thrust =>
      if thrust.slot.isSome then thrust else { thrust with slot := some (firstFreeProjectileSlot used) }
    let used := used ++ (shortsword.bind (·.slot)).toArray.filter (fun slot => !used.contains slot)
    let blueMoon := state.blueMoon.map fun attack =>
      if attack.flail.slot.isSome then attack
      else { attack with flail := { attack.flail with slot := some (firstFreeProjectileSlot used) } }
    let used := used ++ (blueMoon.bind (·.flail.slot)).toArray.filter (fun slot => !used.contains slot)
    let (magicMissiles, used) := assignSlots state.magicMissiles used (·.missile.slot) fun attack slot =>
      { attack with missile := { attack.missile with slot := some slot } }
    let valor := state.valor.map fun attack =>
      if attack.yoyo.slot.isSome then attack
      else { attack with yoyo := { attack.yoyo with slot := some (firstFreeProjectileSlot used) } }
    let used := used ++ (valor.bind (·.yoyo.slot)).toArray.filter (fun slot => !used.contains slot)
    let sunfury := state.sunfury.map fun attack =>
      if attack.flail.slot.isSome then attack
      else { attack with flail := { attack.flail with slot := some (firstFreeProjectileSlot used) } }
    let used := used ++ (sunfury.bind (·.flail.slot)).toArray.filter (fun slot => !used.contains slot)
    let darkLance := state.darkLance.map fun lance =>
      if lance.slot.isSome then lance else { lance with slot := some (firstFreeProjectileSlot used) }
    let used := used ++ (darkLance.bind (·.slot)).toArray.filter (fun slot => !used.contains slot)
    let (flowerFireballs, used) := assignSlots state.flowerFireballs used (·.slot) fun ball slot =>
      { ball with slot := some slot }
    let (hellwingShots, used) := assignSlots state.hellwingShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (flamelashes, used) := assignSlots state.flamelashes used (·.projectile.missile.slot) fun attack slot =>
      { attack with projectile := { attack.projectile with missile := { attack.projectile.missile with slot := some slot } } }
    let (demonSickles, used) := assignSlots state.demonSickles used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (wallLasers, used) := assignSlots state.wallLasers used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (cavernFlames, used) := assignSlots state.cavernFlames used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (twinShots, used) := assignSlots state.twinShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (primeShots, used) := assignSlots state.primeShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (planteraShots, used) := assignSlots state.planteraShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (golemShots, used) := assignSlots state.golemShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (templeTrapShots, used) := assignSlots state.templeTrapShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    let (queenShots, _) := assignSlots state.queenShots used (·.slot) fun shot slot =>
      { shot with slot := some slot }
    { state with
      projectiles, aquaStreams, bombs, shortsword, blueMoon, magicMissiles, valor, sunfury, darkLance
      flowerFireballs, hellwingShots, flamelashes, demonSickles, wallLasers, cavernFlames, twinShots, primeShots
      planteraShots, golemShots, templeTrapShots, queenShots }

structure TickInput where
  horizontal : Int := 0
  dashTap : Int := 0
  jump : Bool := false
  swim : Bool := false
  dropThrough : Bool := false
  mine : Option TilePos := none
  aim : Option WorldAim := none
  useHeld : Bool := false
  cancelFlail : Bool := false
  place : Option TilePos := none
  selectSlot : Option Nat := none
  swapSlot : Option Nat := none
  inventoryClick : Option (Nat × Bool) := none
  cancelCratePotion : Bool := false
  cancelMount : Bool := false
  cancelDagger : Bool := false
  minionAim : Option WorldAim := none
  gearClick : Option GearSlot := none
  chestClick : Option (Nat × Bool) := none
  closeOldMan : Bool := false
  curseOldMan : Bool := false
  sellCursor : Bool := false
  craft : Bool := false
  craftTorches : Bool := false
  craftSword : Bool := false
  craftCrystalPick : Bool := false
  dropVoodooDoll : Bool := false
  craftIronSword : Bool := false
  craftWorkbench : Bool := false
  craftChest : Bool := false
  craftDoor : Bool := false
  craftSilverSword : Bool := false
  craftBow : Bool := false
  craftArrows : Bool := false
  craftMail : Bool := false
  craftPlatforms : Bool := false
  craftBeacon : Bool := false
  craftRecall : Bool := false
  craftGelTorches : Bool := false
  craftHunterBow : Bool := false
  craftFishingRod : Bool := false
  craftBait : Bool := false
  craftGills : Bool := false
  craftBarkskin : Bool := false
  craftRails : Bool := false
  craftMinecart : Bool := false
  dismountCart : Bool := false
  quickMount : Bool := false
  craftGrapplingHook : Bool := false
  craftHighlandCrossbow : Bool := false
  craftClassic : Option ClassicRecipe := none
  craftToCursor : Bool := false
  reelHook : Bool := false
  quickGrapple : Option WorldAim := none
  equipGear : Option Nat := none
  unequipGear : Option GearSlot := none
  interact : Option TilePos := none
  claimHome : Option TilePos := none
  claimMerchant : Option TilePos := none
  buyOffer : Option Nat := none
  sellSlot : Option Nat := none
  closeShop : Bool := false
  closeGuide : Bool := false
  closeAngler : Bool := false
  askAnglerQuest : Bool := false
  chestDeposit : Option Nat := none
  chestWithdraw : Option Nat := none
  closeChest : Bool := false
  deriving Inhabited

private def initialFishingPool (world : World) (water : Water) (seed : UInt64) : World × Water := Id.run do
  let top := surfacePondTop seed
  let mut terrain := world
  let mut water := water.levels
  for y in [top + 3:top + 19] do
    for x in [2:6] do
      terrain := terrain.setTile ⟨x, y⟩ .air
      water := water.set! (y * terrain.width + x) Water.capacity.toUInt8
    for x in [1, 6] do
      if !(terrain.tile ⟨x, y⟩).solid then terrain := terrain.setTile ⟨x, y⟩ .stone
  for x in [2:6] do
    if !(terrain.tile ⟨x, top + 19⟩).solid then terrain := terrain.setTile ⟨x, top + 19⟩ .stone
  return (terrain, ⟨water⟩)

private def installBiomePonds (initial : World) (water : Water) (seed : UInt64) : World × Water := Id.run do
  let mut world := initial
  let mut levels := water.levels
  for x in [428:464] do
    let ground := groundHeight seed x
    for y in [ground:ground + 4] do
      let pos : TilePos := ⟨x, y⟩
      if world.tile pos == .dirt then
        world := world.setTile pos (if y == ground then .jungleGrass else .mud)
  for x in [469:worldWidth - 1] do
    let ground := groundHeight seed x
    for y in [ground:ground + 15] do
      let pos : TilePos := ⟨x, y⟩
      let tile := world.tile pos
      if tile == .dirt || tile == .stone then world := world.setTile pos .sand
  -- Leave the Highland camp and cave entrances on the existing route.
  let easternLandmarks := (deepsteelMine seed).footprint ++ (highlandCavern seed).footprint
  for (left, width, depth, shore) in ([(442, 9, 10, Tile.jungleGrass), (475, 9, 10, .sand),
      (498, 13, 28, .sand)] : List (Nat × Nat × Nat × Tile)) do
    let top := (List.range' (left - 1) (width + 2)).foldl
      (fun y x => max y (groundHeight seed x)) 0
    for x in [left - 1:left + width + 1] do
      let ground := groundHeight seed x
      let floor := if left != 498 then top + depth else
        easternLandmarks.foldl (fun floor pos =>
          if pos.x == (x : Int) && pos.y.toNat > ground then min floor (pos.y.toNat - 1)
          else floor) (top + depth)
      for y in [ground:floor + 1] do
        let pos : TilePos := ⟨x, y⟩
        if x < left || x ≥ left + width || y == floor then
          if !(world.tile pos).solid || (x ≥ left && x < left + width) then
            world := world.setTile pos shore
        else
          world := world.setTile pos .air
          if y ≥ top then levels := levels.set! (y * world.width + x) Water.capacity.toUInt8
  return (world, ⟨levels⟩)

/-- A compact Underground Jungle honey chamber. Keep generated landmarks and
player edits out of the excavation footprint. -/
def installHoneyPool (world : World) (water magma : Water) (seed : UInt64)
    (canEdit : TilePos → Bool := fun _ => true) : World × Water := Id.run do
  if world.width != worldWidth || world.height != underworldHeight then return (world, default)
  let top := (List.range' 429 12).foldl (fun y x => max y (groundHeight seed x)) 0
  let surface := top + 12
  let landmarks := (deepsteelMine seed).footprint ++ (highlandCavern seed).footprint
  let mut terrain := world
  let mut honey := Water.empty world
  for row in [surface - 4:surface + 6] do
    for column in [429:441] do
      let pos : TilePos := ⟨column, row⟩
      if landmarks.contains pos || !canEdit pos || water.level world pos > 0 || magma.level world pos > 0 ||
          ![Tile.stone, .dirt, .mud].contains (world.tile pos) then
        return (world, default)
      let side := column == 429 || column == 440
      let floor := row == surface + 5
      if (side && row ≥ surface) || floor then
        terrain := terrain.setTile pos .honeyBlock
      else
        terrain := terrain.setTile pos .air
        if row ≥ surface then honey := honey.setLevel world pos Water.capacity
  return (terrain, honey)

private def initialWormChests (world : World) (seed : UInt64) : World × Array Chest := Id.run do
  let mut terrain := world
  let mut chests := #[]
  for first in [7, 13, 33, 48, 69, 123, 159, 179, 204, 222, 262, 294, 310, 322, 340, 353] do
    let roll (draw : Nat) := (hash (seed, first, (4345 : Nat), draw)).toNat
    if roll 0 % 6 != 0 then continue
    for x in [first:first + 6] do
      let floor := groundHeight seed x
      let pos : TilePos := ⟨x, floor - 1⟩
      if terrain.tile pos != .air || !(terrain.tile ⟨x, floor⟩).solid then continue
      terrain := terrain.setTile pos .chest
      let count := 1 + (if roll 1 % 5 == 0 then roll 2 % 2 else 0) +
        (if roll 3 % 10 == 0 then roll 4 % 3 else 0)
      let slots : Inventory := (Array.replicate chestSlots none).set! 0 (some ⟨.canOfWorms, count⟩)
      chests := chests.push ⟨pos, slots⟩
      break
  return (terrain, chests)

def anglerCoastExists (world : World) (seed : UInt64) : Bool :=
  (List.range' (world.width - 16) 15).any fun x =>
    (List.range' (groundHeight seed x) 10).any fun y => world.tile ⟨x, y⟩ == .sand

private def startGame (seed : UInt64) (legacyEarly : Bool) : GameState :=
  let (fresh, player) := generateWorld seed
  let (world, furniture) := if legacyEarly then (generatePreEarlyTerrain seed, #[])
    else installHellforges seed fresh #[] (fun _ => true)
  let empty : Inventory := Array.replicate Inventory.slotCount none
  let inventory := if legacyEarly then
      empty.set! 0 (some ⟨.basicPick, 1⟩) |>.set! 7 (some ⟨.wood, startingWood⟩)
    else empty.set! 0 (some ⟨.copperShortsword, 1⟩) |>.set! 1 (some ⟨.copperPick, 1⟩)
      |>.set! 2 (some ⟨.copperAxe, 1⟩)
  let water := Magma.fillFromEighths (Water.initial world seed) world (magmaGallery seed).water
  let (world, water) := if legacyEarly then (world, water) else initialFishingPool world water seed
  let (world, water) := if legacyEarly then (world, water) else installBiomePonds world water seed
  let magma := Magma.initial world seed
  let (world, honey) := if legacyEarly then (world, default) else installHoneyPool world water magma seed
  let world := if legacyEarly then world else installUndergroundJungle world seed fun position =>
    water.level world position == 0 && magma.level world position == 0 && honey.level world position == 0
  let (world, wormChests) := if legacyEarly then (world, #[]) else initialWormChests world seed
  let walls := ByteArray.mk (Array.replicate (world.width * world.height) 0)
  let temple := if legacyEarly then none else TempleLayout.install world walls seed fun p =>
    water.level world p == 0 && magma.level world p == 0 && honey.level world p == 0
  let (world, walls, templeChests) := match temple with
    | some (layout, terrain, background) => (terrain, background, initialTempleChests seed layout)
    | none => (world, walls, #[])
  let dry (p : TilePos) :=
    water.level world p == 0 && magma.level world p == 0 && honey.level world p == 0
  let (world, walls, templeWires) := match temple with
    | none => (world, walls, ByteArray.empty)
    | some (layout, _, _) =>
      let (world, walls) := (layout.installGolemArena world walls dry).getD (world, walls)
      let (world, wires) := TempleWiring.install layout world dry
      (world, walls, wires)
  { seed, world, player, inventory, selected := 0, combat := initialCombat world, legacyEarly
    surface := { clock := if legacyEarly then 0 else classicStartClock }
    weather := if legacyEarly then {} else initialWeather seed
    townGuide := if legacyEarly then none else initialGuide world
    angler := if legacyEarly then {} else
      { questIndex := chooseAnglerQuest seed 0 false (altarTile seed == .crimsonAltar) false true (!honey.levels.isEmpty) }
    dungeonInstalled := !legacyEarly, lowerDungeonInstalled := !legacyEarly, blueMoonDungeonInstalled := !legacyEarly
    magicMissileDungeonInstalled := !legacyEarly, deepDungeonInstalled := !legacyEarly
    oldManWaiting := !legacyEarly
    chests := if legacyEarly then #[] else initialDungeonChests seed true ++
      #[initialLowerDungeonChest seed, initialBlueMoonDungeonChest seed, initialMagicMissileDungeonChest seed] ++
      initialDeepDungeonChests seed ++ initialShadowChests seed ++ wormChests ++ templeChests
    walls, templeWires
    water, magma, honey, furniture, quarry := initialQuarry world, sentry := initialSentry world
    merchantStock := initialMerchantStockFor legacyEarly
    chart := (Chart.empty world).reveal world player.centerTile
    ecology := initialCaveEcology seed
    status := if legacyEarly then "Make torches at the workbench before heading underground."
      else "Select the Copper Axe and chop a tree. Open the inventory with Esc to craft a Work Bench." }

def newGame (seed : UInt64) : GameState := startGame seed false

def newLegacyGame (seed : UInt64) : GameState := startGame seed true

def GameState.coins (state : GameState) : Nat := Inventory.coinBalance state.inventory

def GameState.oldManPosition (state : GameState) : Option TilePos :=
  if state.dungeonInstalled && state.oldManWaiting && !state.skeletronDefeated then
    some (dungeonLayout state.seed).oldMan
  else none

private def GameState.creditCoins (state : GameState) (amount : Nat) : GameState :=
  match Inventory.creditCoins state.inventory amount with
  | some inventory => { state with inventory }
  | none => state

private def actionReach : Nat := 5

private def GameState.tileDistance (state : GameState) (position : TilePos) : Nat :=
  let center := state.player.centerTile
  (position.x - center.x).natAbs + (position.y - center.y).natAbs

def GameState.canReach (state : GameState) (position : TilePos) : Bool :=
  state.world.contains position && state.tileDistance position ≤ actionReach

def GameState.canTalkToGuide (state : GameState) (position : TilePos) : Bool :=
  state.canReach position && state.townGuide.any (·.body.overlapsTile position)

def GameState.canTalkToAngler (state : GameState) (position : TilePos) : Bool :=
  state.canReach position && state.angler.position == some position

private def GameState.anglerShore (state : GameState) : Option TilePos :=
  (List.range' (state.world.width - 21) 7).findSome? fun x =>
    (List.range' (groundHeight state.seed x) 8).findSome? fun (y : Nat) =>
      let position : TilePos := ⟨x, (y : Int) - 1⟩
      if state.world.tile ⟨x, y⟩ == .sand && state.world.tile position == .air &&
          state.world.tile ⟨x, (y : Int) - 2⟩ == .air then some position else none

private def GameState.advanceAngler (state : GameState) (dawn : Bool) : GameState := Id.run do
  if state.legacyEarly then return state
  let mut angler := state.angler
  if dawn then
    let anyBoss := state.eyeDefeated || state.evilBossDefeated || state.skeletronDefeated || state.hardmode
    angler := { angler with
      turnedIn := false
      questIndex := chooseAnglerQuest state.seed state.weather.day state.hardmode
        (altarTile state.seed == .crimsonAltar) anyBoss (anglerCoastExists state.world state.seed)
        (state.honey.levels.data.any (· != 0)) }
  if angler.position.isNone && state.player.centerTile.x ≥ (state.world.width : Int) - 35 then
    if (hash (state.seed, state.weather.day, state.surface.clock, (376 : Nat))).toNat % 60 == 0 then
      angler := { angler with position := state.anglerShore }
  if angler.rescued && !state.openAngler then
    if let some home := angler.home then
      if dawn || angler.position.all (fun pos => !state.canReach pos) then
        angler := { angler with position := some home }
  return { state with
    angler
    openAngler := state.openAngler && angler.position.any state.canReach
    showAnglerQuest := state.showAnglerQuest && !dawn }

def GameState.canReachWithTool (state : GameState) (position : TilePos) (tool : Item) : Bool :=
  if state.legacyEarly then state.canReach position
  else
    let center := state.player.centerTile
    let penalty := if tool == .copperPick || tool == .copperAxe || tool == .sawtoothShark then 1 else 0
    let boost := if tool == .picksaw then 1 else 0
    state.world.contains position && (position.x - center.x).natAbs ≤ 5 + boost - penalty &&
      (position.y - center.y).natAbs ≤ 4 + boost - penalty

def GameState.canOpenSupplyCache (state : GameState) (position : TilePos) : Bool :=
  state.world.tile position == .supplyCache && state.tileDistance position ≤ 1

def GameState.defense (state : GameState) : Nat :=
  let defense := state.equipment.defense.toNat + if state.barkskin > 0 then 1 else 0
  defense - if state.weakTicks > 0 then 4 else 0

private def GameState.nearStation (state : GameState) (station : Tile) : Bool := Id.run do
  let center := state.player.centerTile
  let vertical := if state.legacyEarly then actionReach else 4
  for row in [:2 * vertical + 1] do
    for column in [:2 * actionReach + 1] do
      let pos : TilePos := ⟨center.x + (column : Int) - (actionReach : Int),
        center.y + (row : Int) - (vertical : Int)⟩
      if state.world.tile pos == station && (!state.legacyEarly || state.canReach pos) then
        return true
  return false

def GameState.nearWorkbench (state : GameState) : Bool :=
  state.nearStation .workbench

private def GameState.hasIngredients (state : GameState) (ingredients : List (Item × Nat)) : Bool :=
  ingredients.all (fun (material, quantity) => Inventory.count state.inventory material ≥ quantity)

private def Inventory.spend (inventory : Inventory) (ingredients : List (Item × Nat)) : Inventory :=
  ingredients.foldl (fun paid (material, quantity) => Inventory.remove paid material quantity) inventory

private def GameState.hasClassicStation (state : GameState) (recipe : ClassicRecipe) : Bool :=
  match recipe.station with
  | none => true
  | some .ironAnvil => state.nearStation .ironAnvil || state.nearStation .leadAnvil ||
      state.nearStation .mythrilAnvil || state.nearStation .orichalcumAnvil
  | some .mythrilAnvil => state.nearStation .mythrilAnvil || state.nearStation .orichalcumAnvil
  | some .furnace => state.nearStation .furnace || state.nearStation .hellforge ||
      state.nearStation .adamantiteForge || state.nearStation .titaniumForge
  | some .hellforge => state.nearStation .hellforge || state.nearStation .adamantiteForge || state.nearStation .titaniumForge
  | some .adamantiteForge => state.nearStation .adamantiteForge || state.nearStation .titaniumForge
  | some .demonAltar => state.nearStation .demonAltar || state.nearStation .crimsonAltar
  | some station => state.nearStation station

private def GameState.classicIngredients (state : GameState) (recipe : ClassicRecipe) : List (Item × Nat) :=
  if !recipe.acceptsLeadBar then recipe.ingredients
  else recipe.ingredients.flatMap fun (item, count) =>
    if item == .ironBar then
      let iron := min count (Inventory.count state.inventory .ironBar)
      [(.ironBar, iron), (.leadBar, count - iron)]
    else [(item, count)]

private def GameState.canHoldCraft (state : GameState) (recipe : ClassicRecipe) : Bool :=
  !state.cursorStack.any (·.item != recipe.item) &&
    recipe.quantity + (state.cursorStack.map (·.count)).getD 0 ≤ Inventory.stackLimit recipe.item

def GameState.canCraftClassic (state : GameState) (recipe : ClassicRecipe) (toCursor := false) : Bool :=
  let ingredients := state.classicIngredients recipe
  state.respawnTicks == 0 && state.hasClassicStation recipe && state.hasIngredients ingredients &&
    (if toCursor then state.canHoldCraft recipe
    else (Inventory.addItems (state.inventory.spend ingredients) recipe.item recipe.quantity).isSome)

def GameState.canSummonEye (state : GameState) : Bool :=
  !state.legacyEarly && state.respawnTicks == 0 && state.eye.isNone &&
    isNight state.surface.clock false

def GameState.twinsActive (state : GameState) : Bool :=
  state.retinazer.isSome || state.spazmatism.isSome

def GameState.canSummonTwins (state : GameState) : Bool :=
  !state.legacyEarly && state.respawnTicks == 0 && state.combat.health > 0 &&
    !state.twinsActive && isNight state.surface.clock false

def GameState.hasInstalledEvil (state : GameState) : Bool :=
  (evilAltars state.seed).any (fun altar => state.world.tile altar == altarTile state.seed) ||
    (state.hardmode && (state.evilRelicsBroken > 0 || state.evilBossDefeated ||
      (evilRelics state.seed).any (fun position => state.world.tile position == evilRelicTile state.seed)))

def GameState.inInstalledEvil (state : GameState) : Bool :=
  !state.legacyEarly && inEvilBiome state.seed state.player.centerTile && state.hasInstalledEvil

def GameState.canSummonEvil (state : GameState) (item : Item) : Bool :=
  state.respawnTicks == 0 && state.eater.isNone && state.brain.isNone && state.inInstalledEvil &&
    (if altarTile state.seed == .demonAltar then item == .wormFood else item == .bloodySpine)

def GameState.furnitureAt (state : GameState) (position : TilePos) : Option Furniture :=
  state.furniture.find? (fun object => object.cells.contains position)

private def GameState.bedArrival (state : GameState) : Option Player := do
  let anchor ← state.bedSpawnPoint
  let bed ← state.furniture.find? (fun object => object.position == anchor && object.kind == .bed)
  bedSpawn state.world state.walls bed

private def GameState.refreshBed (state : GameState) : GameState :=
  if state.bedSpawnPoint.isSome && state.bedArrival.isNone then
    { state with bedSpawnPoint := none, status := "Bed spawn point removed." }
  else state

private def FurnitureKind.item : FurnitureKind → Item
  | .bed => .bed
  | .furnace => .furnace
  | .hellforge => .hellforge
  | .ironAnvil => .ironAnvil
  | .leadAnvil => .leadAnvil
  | .mythrilAnvil => .mythrilAnvil
  | .orichalcumAnvil => .orichalcumAnvil
  | .adamantiteForge => .adamantiteForge
  | .titaniumForge => .titaniumForge
  | .sawmill => .sawmill
  | .loom => .loom
  | .woodChair => .woodChair
  | .woodTable => .woodTable
  | .woodenCrate => .woodenCrate
  | .ironCrate => .ironCrate
  | .goldenCrate => .goldenCrate
  | .pearlwoodCrate => .pearlwoodCrate
  | .mythrilCrate => .mythrilCrate
  | .titaniumCrate => .titaniumCrate
  | .biomeCrate item => item

private def Item.furnitureKind : Item → Option FurnitureKind
  | .bed => some .bed
  | .furnace => some .furnace
  | .hellforge => some .hellforge
  | .ironAnvil => some .ironAnvil
  | .leadAnvil => some .leadAnvil
  | .mythrilAnvil => some .mythrilAnvil
  | .orichalcumAnvil => some .orichalcumAnvil
  | .adamantiteForge => some .adamantiteForge
  | .titaniumForge => some .titaniumForge
  | .sawmill => some .sawmill
  | .loom => some .loom
  | .woodChair => some .woodChair
  | .woodTable => some .woodTable
  | .woodenCrate => some .woodenCrate
  | .ironCrate => some .ironCrate
  | .goldenCrate => some .goldenCrate
  | .pearlwoodCrate => some .pearlwoodCrate
  | .mythrilCrate => some .mythrilCrate
  | .titaniumCrate => some .titaniumCrate
  | item => if item.isFishingCrate then some (.biomeCrate item) else none

def GameState.selectedStack (state : GameState) : Option Stack :=
  state.cursorStack.orElse fun _ => state.inventory[state.selected]?.join

private def GameState.consumeSelected (state : GameState) : GameState :=
  if let some stack := state.cursorStack then
    { state with cursorStack := if stack.count ≤ 1 then none else some { stack with count := stack.count - 1 } }
  else
    { state with inventory := Inventory.removeFromSlot state.inventory state.selected 1 }

private def GameState.chestIndex (state : GameState) (position : TilePos) : Option Nat :=
  state.chests.findIdx? (·.position == position)

def GameState.chestAt (state : GameState) (position : TilePos) : Option Chest := do
  let index ← state.chestIndex position
  state.chests[index]?

def Tile.isShadowChest (tile : Tile) : Bool := tile == .shadowChest || tile == .shadowOpenChest

def Tile.isFramedChest (tile : Tile) : Bool :=
  tile.isDungeonChest || tile.isShadowChest || tile == .lihzahrdChest

def GameState.shadowChestAtCell (state : GameState) (position : TilePos) : Option TilePos :=
  (state.chests.find? fun chest => (state.world.tile chest.position).isShadowChest &&
    (dungeonChestCells chest.position).contains position).map (·.position)

def GameState.templeChestAtCell (state : GameState) (position : TilePos) : Option TilePos :=
  (state.chests.find? fun chest => state.world.tile chest.position == .lihzahrdChest &&
    (dungeonChestCells chest.position).contains position).map (·.position)

private def GameState.swapBackpackSlot (state : GameState) (index : Nat) : GameState :=
  if index < Inventory.hotbarCount || index ≥ Inventory.mainSlotCount ||
      state.selected ≥ Inventory.hotbarCount then state else
  match state.inventory[index]?, state.inventory[state.selected]? with
  | some stored, some equipped =>
    { state with inventory := state.inventory.set! index equipped |>.set! state.selected stored }
  | _, _ => state

private def GameState.dropLoot (state : GameState) (position : TilePos) (coins : Nat)
    (items : Array Stack) : GameState := Id.run do
  if coins == 0 && items.isEmpty then return state
  if let some index := state.nightDrops.findIdx? (·.position == position) then
    let old := state.nightDrops[index]!
    let mut merged := old.items
    for drop in items do
      if let some (index, stack) := (merged.findIdx? (·.item == drop.item)).bind
          (fun index => merged[index]?.map (fun stack => (index, stack))) then
        merged := merged.set! index { stack with count := min 9999 (stack.count + drop.count) }
      else merged := merged.push drop
    let updated := { old with coins := min maxDropCoins (old.coins + coins), items := merged }
    return { state with nightDrops := state.nightDrops.set! index updated }
  let retained := if state.nightDrops.size < maxNightDrops then state.nightDrops else state.nightDrops.extract 1
  return { state with nightDrops := retained.push ⟨position, coins, items⟩ }

private def GameState.grantFishingLoot (state : GameState) (loot : Array Stack) : GameState := Id.run do
  let mut inventory := state.inventory
  let mut drops := #[]
  let mut coins := 0
  for stack in loot do
    if stack.item.coinValue > 0 then
      coins := coins + stack.count * stack.item.coinValue
    else
      match Inventory.addItems inventory stack.item stack.count with
      | some (updated, _) => inventory := updated
      | none => drops := drops.push stack
  let credited := Inventory.creditCoins inventory coins
  return ({ state with inventory := credited.getD inventory } : GameState).dropLoot state.player.centerTile
    (if credited.isSome then 0 else coins) drops

private def GameState.handleAngler (state : GameState) (input : TickInput) : GameState := Id.run do
  if input.closeAngler then return { state with openAngler := false, showAnglerQuest := false }
  if !input.askAnglerQuest then return state
  let state := { state with showAnglerQuest := true }
  if state.angler.turnedIn then return state
  let quest := state.angler.quest
  let some index := (state.inventory.extract 0 Inventory.mainSlotCount).findIdx? (·.any (·.item == quest.fish))
    | return state
  let inventory := Inventory.removeFromSlot state.inventory index 1
  let completed := state.angler.completed + 1
  let owns (item : Item) := Inventory.count inventory item > 0 || state.equipment.accessory == some item
  let rng := UnifiedRandom.fromEntropy (hash (state.seed, state.weather.day, completed, (2948 : Nat)))
  let loot := anglerRewards completed quest.fish state.hardmode owns rng
  let granted := { state with
    inventory, angler := { state.angler with completed, turnedIn := true }
    status := s!"Quest complete. Received {(loot[0]?.map (fun (stack : Stack) => stack.item.name)).getD "a reward"}." }
  return granted.grantFishingLoot loot

private def GameState.openCrate (state : GameState) (item : Item) (index : Nat) : GameState := Id.run do
  if item == .goldenLockBox && Inventory.count state.inventory .goldenKey == 0 then
    return { state with status := "A Golden Key is needed to open the Golden Lock Box." }
  if item == .obsidianLockBox && Inventory.count state.inventory .shadowKey == 0 then
    return { state with status := "A Shadow Key is needed to open the Obsidian Lock Box." }
  let sample := hash (state.seed, state.weather.day, state.surface.clock, index, item.code)
  let loot := openFishingCrate item (UnifiedRandom.fromEntropy sample)
  let inventory := Inventory.removeFromSlot state.inventory index 1
  let inventory := if item == .goldenLockBox then Inventory.remove inventory .goldenKey 1 else inventory
  let crystals := loot.foldl (fun count stack =>
    count + if stack.item == .lifeCrystal then stack.count else 0) state.crateLifeCrystals
  let opened := ({ state with inventory } : GameState).grantFishingLoot loot
  return { opened with
    crateLifeCrystals := crystals
    shadowKeyRecovery := state.shadowKeyRecovery && Inventory.count opened.inventory .shadowKey == 0
    status := s!"Opened {item.name}." }

private def GameState.handleInventoryClick (state : GameState) (action : Option (Nat × Bool)) : GameState :=
  match action with
  | none => state
  | some (index, split) =>
    if split && index < Inventory.mainSlotCount && state.cursorStack.isNone &&
        state.inventory[index]?.join.any (·.item.isCrateContainer) then
      match state.inventory[index]?.join with
      | some stack => state.openCrate stack.item index
      | none => state
    else if split && index < Inventory.mainSlotCount && state.cursorStack.isNone &&
        state.inventory[index]?.join.any (·.item == .canOfWorms) then Id.run do
      let roll (draw : Nat) := (hash (state.seed, state.surface.clock, index, (4345 : Nat), draw)).toNat
      let mut inventory := Inventory.removeFromSlot state.inventory index 1
      let loot : List (Item × Nat) := [(.worm, 5 + roll 0 % 4),
        (.enchantedNightcrawler, if roll 1 % 10 < 3 then 1 + roll 2 % 2 else 0),
        (.goldWorm, if roll 3 % 20 == 0 then 1 else 0)]
      for (item, count) in loot do
        if count == 0 then continue
        let some (next, _) := Inventory.addItems inventory item count
          | return { state with status := "Make room in your inventory to open the Can Of Worms." }
        inventory := next
      return { state with inventory, status := "Opened the Can Of Worms." }
    else
      let (inventory, cursorStack) := Inventory.click state.inventory state.cursorStack index split
      let hasHook := state.equipment.hook == some .grapplingHook ||
        Inventory.count inventory .grapplingHook > 0 ||
        cursorStack.any (·.item == .grapplingHook) ||
        (inventory[Inventory.trashSlot]?.join).any (·.item == .grapplingHook)
      let grapple := if hasHook then state.grapple else match state.grapple with
        | .flying flight => if flight.kind == .basic then .idle else
            .flying { flight with held := flight.held.filter (·.kind == .dissonance) }
        | .latched anchor => if anchor.kind == .basic then .idle else state.grapple
        | .idle => .idle
      { state with inventory, cursorStack, grapple }

private def neighbors (position : TilePos) : List TilePos :=
  [⟨position.x - 1, position.y⟩, ⟨position.x + 1, position.y⟩,
    ⟨position.x, position.y - 1⟩, ⟨position.x, position.y + 1⟩]

private def grassNeighbors (position : TilePos) : List TilePos :=
  neighbors position ++ [⟨position.x - 1, position.y - 1⟩, ⟨position.x + 1, position.y - 1⟩,
    ⟨position.x - 1, position.y + 1⟩, ⟨position.x + 1, position.y + 1⟩]

private def GameState.advanceEvilGrass (state : GameState) : World × Bool := Id.run do
  if state.legacyEarly then return (state.world, false)
  let surfaceRows := min state.world.height (groundHeight state.seed 19)
  let undergroundRows := state.world.height - surfaceRows
  let sample (index start rows : Nat) : TilePos :=
    let x := (hash (state.seed, state.surface.clock, index)).toNat % state.world.width
    let y := (hash (state.seed, state.surface.clock, index + 3)).toNat % rows
    ⟨x, start + y⟩
  let mut world := if state.hardmode then advanceHardmodeSpread state.world state.seed state.surface.clock
    else state.world
  let mut changed := world.tiles != state.world.tiles
  for source in [sample 0 0 surfaceRows, sample 1 0 surfaceRows, sample 2 surfaceRows undergroundRows] do
    let grass := world.tile source
    let mushroom := grass.evilMushroom
    if mushroom.isNone && grass != .hallowedGrass then continue
    let air : TilePos := ⟨source.x, source.y - 1⟩
    if let some mushroom := mushroom then
      if (hash (state.seed, source, state.surface.clock, (11 : Nat))).toNat % 8 == 0 &&
          world.tile air == .air && state.water.level world air == 0 && state.magma.level world air == 0 then
        world := world.setTile air mushroom
        changed := true
    for target in grassNeighbors source do
      if world.tile target != .dirt then continue
      let above := world.tile ⟨target.x, target.y - 1⟩
      if above == .sunleaf || above == .sunleafSprout then continue
      let surrounding := grassNeighbors target
      let exposed := surrounding.any fun pos =>
        let tile := world.tile pos
        !tile.solid && tile != .platform
      if exposed && !surrounding.any (fun pos => state.magma.level world pos > 0) then
        world := world.setTile target grass
        changed := true
  return (world, changed)

private def hasTorchSupport (world : World) (position : TilePos) : Bool :=
  (neighbors position).any fun neighbor =>
    (world.tile? neighbor).any fun tile => tile.solid && !tile.isDoor

private def above (position : TilePos) : TilePos := { position with y := position.y - 1 }

private def below (position : TilePos) : TilePos := { position with y := position.y + 1 }

def GameState.cartIndex (state : GameState) (position : TilePos) : Option Nat :=
  state.carts.findIdx? (fun rail => position == rail || position == above rail)

def GameState.canBoardCart (state : GameState) (rail : TilePos) : Bool :=
  let body := cartBody rail
  let facing : Int := if body.x < state.player.x then -1 else 1
  state.ride.isNone && canParkCart state.world rail &&
    swordReaches state.world state.player body facing true

private def GameState.keepsCarts (state : GameState) (world : World) : Bool :=
  state.carts.all (canParkCart world) && state.ride.all (fun ride => ride.valid world state.carts)

private def GameState.parkCart (state : GameState) (ride : RailRide) : GameState :=
  { state with carts := state.carts.push ride.parkingTile, ride := none }

private def GameState.recoverCart (state : GameState) (index : Nat) : GameState := Id.run do
  if !state.selectedStack.any (fun stack => stack.item.pickPower > 0) then
    return { state with status := "Select a pick to recover the minecart." }
  let some (inventory, _) := Inventory.addOne state.inventory .minecart
    | return { state with status := "Make room in your inventory for the minecart." }
  return { state with inventory, carts := state.carts.eraseIdxIfInBounds index, status := "Minecart recovered." }

private def GameState.deployCart (state : GameState) (position : TilePos) : GameState := Id.run do
  if !canParkCart state.world position then
    return { state with status := "Set the minecart on a rail with room above it." }
  if state.carts.size + (if state.ride.isSome then 1 else 0) ≥ maxMinecarts then
    return { state with status := s!"Recover a minecart first. The world can hold {maxMinecarts}." }
  let carts := state.carts.push position
  if state.carts.contains position || !state.ride.all (fun ride => ride.valid state.world carts) then
    return { state with status := "Leave room between minecarts." }
  return { state.consumeSelected with
    carts
    status := "Minecart parked. Point at it and press Q to board." }

private def GameState.occupied (state : GameState) (position : TilePos) : Bool :=
  let left := position.x * tileUnits
  let top := position.y * tileUnits
  let box : HitBox := ⟨left, top, left + tileUnits, top + tileUnits⟩
  state.player.overlapsTile position (if state.wingedSlime.isSome then wingedSlimeHeightBoost else 0) ||
    state.angryBones.any (fun bones => bones.hitBox.intersects box) ||
    state.combat.crawler.any (fun crawler => crawler.body.overlapsTile position) ||
    state.surface.creature.any (fun creature => creature.body.overlapsTile position) ||
    state.surface.flying.any (fun creature => creature.body.overlapsTile position) ||
    state.townGuide.any (·.body.overlapsTile position) ||
    state.quarry.creature.any (fun creature => creature.body.overlapsTile position) ||
    state.sentry.creature.any (fun creature => creature.body.overlapsTile position) ||
    (match state.warden with
    | .fighting warden => warden.body.overlapsTile position
    | .reward drop => drop == position
    | _ => false) ||
    (match state.storm with
    | .fighting herald => herald.body.overlapsTile position
    | .reward drop => drop == position
    | _ => false) ||
    (match state.ruin with
    | .fighting sentinel => sentinel.body.overlapsTile position
    | .reward drop => drop == position
    | _ => false) ||
    (match state.cavern with
    | .fighting colossus => colossus.body.overlapsTile position
    | .reward drop => drop == position
    | _ => false) ||
    (match state.raid with
    | .active wave => wave.foes.any (fun foe => foe.body.overlapsTile position)
    | .reward drop => drop == position
    | _ => false) ||
    state.ecology.sites.any (fun site => match site with
      | .alive resident => resident.body.overlapsTile position
      | .loot drop => drop == position
      | .waiting _ => false)

private def displaceDoorLiquid (liquid : Water) (world : World) (base : TilePos)
    (barrier : ByteArray) : Option Water := do
  (doorCells world base).foldlM (fun current cell => current.displace world cell barrier) liquid

private def GameState.homeLanding (state : GameState) (world : World) (position : TilePos) :
    Option Player := do
  let body ← homeArrival world state.water position
  if Magma.touches state.magma world body then none else some body

private def GameState.harvestSunleaf (state : GameState) (position : TilePos) (ripe : Bool) :
    GameState := Id.run do
  if !state.selectedStack.any (fun stack => stack.item.pickPower > 0) then
    return { state with status := "Select a pick to harvest Sunleaf." }
  let seeds := if ripe then 2 else 1
  let harvest := do
    let (inventory, _) ← Inventory.addItems state.inventory .sunleafSeed seeds
    if ripe then (Inventory.addOne inventory .sunleaf).map Prod.fst else some inventory
  let some inventory := harvest
    | return { state with status := "Make room for the harvest. The plant is still there." }
  return { state with
    world := state.world.setTile position .air
    crops := state.crops.filter (·.position != position)
    inventory
    status := if ripe then "Harvested Sunleaf and two seeds." else "Recovered the Sunleaf seed." }

private def GameState.placeFurniture (state : GameState) (kind : FurnitureKind)
    (position : TilePos) : GameState := Id.run do
  let object : Furniture := { kind, position, flipped := kind == .bed && state.combat.facing > 0 }
  if !object.canPlace state.world then
    return { state with status := s!"The {kind.item.name} needs {kind.width} by {kind.height} clear tiles over a floor." }
  if object.cells.any (fun cell => state.magma.level state.world cell > 0) then
    return { state with status := "Choose a spot clear of lava." }
  let world := object.cells.foldl (fun world cell => world.setTile cell object.tile) state.world
  return { state.consumeSelected with
    world
    furniture := state.furniture.push object
    status := if kind == .bed then "Bed placed. Point at the foot and press Q to set spawn."
      else s!"Placed {kind.item.name}." }

private def GameState.recoverFurniture (state : GameState) (object : Furniture) : GameState := Id.run do
  if !state.selectedStack.any (fun stack => stack.item.pickPower > 0) then
    return { state with status := "Select a pick to remove furniture." }
  if object.kind == .hellforge && object.position.y > underworldStart &&
      !state.selectedStack.any (fun stack => stack.item.pickPower ≥ 65) then
    return { state with status := "This Hellforge requires 65% pickaxe power." }
  if object.cells.any (fun cell => state.world.tile (above cell) == .placedBottle) then
    return { state with status := "Recover the bottle before removing its support." }
  if object.tile == .fishingCrate && object.cells.any (fun cell =>
      !object.cells.contains (above cell) && state.world.tile (above cell) == .fishingCrate) then
    return { state with status := "Recover the crate above first." }
  let some (inventory, _) := Inventory.addOne state.inventory object.kind.item
    | return { state with status := "Make room for the furniture first." }
  let world := object.cells.foldl (fun world cell => world.setTile cell .air) state.world
  return { state with
    world
    inventory
    furniture := state.furniture.filter (·.position != object.position)
    status := s!"Recovered {object.kind.item.name}." }

private def GameState.placeWoodWall (state : GameState) (position : TilePos) : GameState :=
  if hasWoodWall state.world state.walls position ||
      TempleLayout.hasNaturalWall state.world state.walls position then state
  else if !(state.world.tile position).solid &&
      !(neighbors position).any (fun cell =>
        (state.world.tile? cell).any Tile.solid || hasWoodWall state.world state.walls cell) then
    { state with status := "Place a wall beside terrain or another wall." }
  else
    let index := position.y.toNat * state.world.width + position.x.toNat
    { state.consumeSelected with
      walls := state.walls.set! index 1
      status := "Placed Wood Wall." }

private def GameState.hammerWall (state : GameState) (position : TilePos) : GameState :=
  if !state.canReach position || (state.world.tile position).solid ||
      !hasWoodWall state.world state.walls position then state
  else
    match Inventory.addOne state.inventory .woodWall with
    | none => { state with status := "Make room to recover the wall." }
    | some (inventory, _) =>
      let index := position.y.toNat * state.world.width + position.x.toNat
      { state with inventory, walls := state.walls.set! index 0, status := "Recovered Wood Wall." }

private def TileHit.age (hit : TileHit) : Option TileHit :=
  let ticks := hit.ticks - 1
  let loss := if ticks < 12 then 10 else if ticks < 24 then 7 else
    if ticks < 36 then 5 else if ticks < 48 then 2 else 0
  let damage := hit.damage - loss
  if ticks == 0 || damage == 0 then none else some { hit with ticks, damage }

private def GameState.recordToolHit (state : GameState) (tool : Item) (position : TilePos)
    (tile : Tile) (strength : Nat) : GameState × Bool :=
  let prior := state.tileHits.find? fun hit => hit.position == position && hit.tile == tile
  let damage := strength + (prior.map (·.damage)).getD 0
  let remaining := state.tileHits.filterMap fun hit =>
    if hit.position == position then none else hit.age
  let updated := { state with toolCooldown := tool.toolSpeed, tileHits := remaining }
  if damage ≥ 100 then (updated, true)
  else
    ({ updated with tileHits := #[⟨position, tile, damage, tileHitLifetime - 1⟩] ++
      remaining.extract 0 (tileHitLimit - 1) }, false)

private def GameState.chopTree (state : GameState) (position : TilePos) (tool : Item) : GameState := Id.run do
  if tool.axePower == 0 then return { state with status := "Select an axe to chop the tree." }
  if state.toolCooldown > 0 then return state
  let (hit, broke) := state.recordToolHit tool position .treeTrunk (tool.axePower * 24 / 100)
  if !broke then return { hit with status := "Chopping tree…" }
  let mut world := state.world
  let mut wood := 0
  let mut cell := position
  while world.tile cell == .treeTrunk do
    let roll := (hash (state.seed, cell, state.surface.clock)).toNat % 525
    wood := wood + if roll < 2 * tool.axePower + 175 then 2 else 1
    world := world.setTile cell .air
    cell := above cell
  let some (inventory, _) := Inventory.addItems state.inventory .wood wood
    | return { state with status := "Make room for the wood." }
  return { hit with world, inventory, status := s!"Collected {wood} Wood." }

private def GameState.breakPlanteraBulb (state : GameState) (position : TilePos) : GameState := Id.run do
  let some bulb := state.planteraBulbs.find? (fun bulb => (planteraBulbCells bulb).contains position)
    | return state
  let world := (planteraBulbCells bulb).foldl (fun world cell =>
    if world.tile cell == .planteraBulb then world.setTile cell .air else world) state.world
  let next := { state with world, planteraBulbs := state.planteraBulbs.filter (· != bulb) }
  let px := state.player.x / tileUnits
  let py := state.player.y / tileUnits
  if !state.hardmode || state.plantera.isSome || state.combat.health == 0 ||
      (position.x - px).natAbs + (position.y - py).natAbs ≥ 50 then return next
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, (262 : Nat), draw)).toNat
  return { next with
    plantera := some (PlanteraHead.spawn world state.player roll)
    status := "Plantera has awoken!" }

private def GameState.updateTempleDoorSupports (state : GameState) (position : TilePos) : GameState :=
  #[above position, below position].foldl (fun next cell =>
    match TempleDoor.atCell next.world cell with
    | some (hinge, mode) =>
      if mode == .locked || TempleDoor.supports next.world hinge then next else
        let cleared := { next with
          world := TempleDoor.remove next.world hinge mode
          autoTempleDoor := next.autoTempleDoor.filter (· != hinge) }
        cleared.dropLoot hinge 0 #[⟨.lihzahrdDoor, 1⟩]
    | none => next) state

private def GameState.dropUnsupportedPressurePlate (state : GameState) (position : TilePos) : GameState :=
  let plate := above position
  if (state.world.tile position).solid || state.world.tile plate != .lihzahrdPressurePlate then state
  else ({ state with world := state.world.setTile plate .air } : GameState).dropLoot
    plate 0 #[⟨.lihzahrdPressurePlate, 1⟩]

private def GameState.mineTile (state : GameState) (position : TilePos) : GameState := Id.run do
  let tool := state.selectedStack.map (·.item)
  if !tool.any (state.canReachWithTool position) then
    return { state with status := "Move closer to mine that tile." }
  if let some index := state.cartIndex position then return state.recoverCart index
  if let some object := state.furnitureAt position then return state.recoverFurniture object
  if state.furniture.any (fun object => object.supports.contains position) then
    return { state with status := "Remove the furniture before mining its floor." }
  if state.hardmodeCavern.any (·.anchor == some position) then
    return { state with status := "The Clinger holds onto this block." }
  let tile := state.world.tile position
  if tile == .air then return state
  if tile == .planteraBulb then
    return if tool.any (fun item => item.pickPower > 0) then state.breakPlanteraBulb position else state
  if TempleDoor.protects state.world position then return state
  if let some (hinge, mode) := TempleDoor.atCell state.world position then
    let some pick := tool | return state
    if mode == .locked || pick.pickPower == 0 || state.toolCooldown > 0 then return state
    let (hit, broke) := state.recordToolHit pick position tile pick.pickPower
    if !broke then return hit
    let some (inventory, _) := Inventory.addOne state.inventory .lihzahrdDoor
      | return { state with status := "Your inventory is full." }
    return { hit with
      inventory, world := TempleDoor.remove state.world hinge mode
      autoTempleDoor := state.autoTempleDoor.filter (· != hinge)
      status := "Collected Lihzahrd Door." }
  if tile.isTempleAltar then
    let some anchor := TempleLayout.altarAnchor state.world position | return state
    let some pick := tool | return state
    if pick.pickPower < 210 || state.toolCooldown > 0 then return state
    let some (inventory, _) := Inventory.addOne state.inventory .lihzahrdAltar
      | return { state with status := "Your inventory is full." }
    let world := (TempleLayout.altarCells anchor).foldl (fun world p => world.setTile p .air) state.world
    return { state with world, inventory, toolCooldown := pick.toolSpeed, status := "Collected Lihzahrd Altar." }
  if tile.isShadowChest || tile == .lihzahrdChest then
    let temple := tile == .lihzahrdChest
    let noneOrAnchor := if temple then state.templeChestAtCell position else state.shadowChestAtCell position
    let some anchor := noneOrAnchor | return state
    if tile == .shadowChest then return { state with status := "Open the locked Shadow Chest first." }
    if !tool.any (fun item => item.pickPower > 0) then
      return { state with status := "Select a pick to remove the empty chest." }
    if (state.chestAt anchor).any (fun chest => chest.slots.any Option.isSome) then
      return { state with status := "Empty the chest before mining it." }
    let item := if temple then Item.lihzahrdChest else .shadowChest
    let some (inventory, _) := Inventory.addOne state.inventory item
      | return { state with status := "Your inventory is full." }
    let world := (dungeonChestCells anchor).foldl (fun world cell => world.setTile cell .air) state.world
    return { state with
      inventory, world, chests := state.chests.filter (·.position != anchor)
      status := s!"Collected {item.name}." }
  if tile.isDungeonChest || (state.world.tile (above position)).isFramedChest then
    return { state with status := "Use the chest to open it." }
  if tile.isEvilAltar then return { state with status := "This altar is a crafting station." }
  if (state.world.tile (above position)).isTempleAltar then
    return { state with status := "Remove the altar before mining its floor." }
  if (state.world.tile (above position)).isEvilAltar then
    return { state with status := "The altar cannot be removed before Hardmode." }
  if state.world.tile (above position) == .placedBottle then
    return { state with status := "Recover the bottle before removing its support." }
  if let some item := tool then
    if tile == .treeTrunk then return state.chopTree position item
  if state.world.tile (above position) == .treeTrunk then
    return { state with status := "Chop the tree before mining its ground." }
  if tile == .sunleaf || tile == .sunleafSprout then
    return state.harvestSunleaf position (tile == .sunleaf)
  if (tile == .dirt || tile == .hallowedGrass) && (state.world.tile (above position) == .sunleaf ||
      state.world.tile (above position) == .sunleafSprout) then
    return { state with status := "Clear the Sunleaf before mining its soil." }
  if tile == .supplyCache then
    return { state with status := "Step next to the supply cache and press Q to open it." }
  if tile == .openedCache then
    if state.selectedStack.any (fun stack => stack.item.pickPower > 0) then
      return { state with world := state.world.setTile position .air, status := "Removed the empty cache." }
    return { state with status := "Select a pick to remove the empty cache." }
  if state.world.tile (above position) == .doorClosed ||
      state.world.tile (above position) == .doorOpen then
    return { state with status := "Remove the door before mining its floor." }
  if let some base := doorBase state.world (below position) then
    if base.y == position.y + 3 && (doorCells state.world base).size == 3 then
      return { state with status := "Remove the door before mining its overhead support." }
  if (state.chestAt position).any (fun chest => chest.slots.any Option.isSome) then
    return { state with status := "Empty the chest before mining it." }
  let item := match tile with
    | .dirt | .corruptGrass | .crimsonGrass | .hallowedGrass => some Item.dirt
    | .stone | .ruinBrick => some Item.stone
    | .copper => some Item.copper
    | .tinOre => some Item.tinOre
    | .leadOre => some Item.leadOre
    | .tungstenOre => some Item.tungstenOre
    | .goldOre => some Item.goldOre
    | .platinumOre => some Item.platinumOre
    | .demoniteOre => some Item.demoniteOre
    | .crimtaneOre => some Item.crimtaneOre
    | .ebonstone => some Item.ebonstone
    | .crimstone => some Item.crimstone
    | .sand => some Item.sand
    | .mud | .jungleGrass => some Item.mudBlock
    | .pearlstone => some Item.pearlstone
    | .pearlsand => some Item.pearlsand
    | .ebonsand => some Item.ebonsand
    | .crimsand => some Item.crimsand
    | .purpleIce => some Item.purpleIce
    | .pinkIce => some Item.pinkIce
    | .redIce => some Item.redIce
    | .cobaltOre => some Item.cobaltOre
    | .palladiumOre => some Item.palladiumOre
    | .mythrilOre => some Item.mythrilOre
    | .orichalcumOre => some Item.orichalcumOre
    | .adamantiteOre => some Item.adamantiteOre
    | .titaniumOre => some Item.titaniumOre
    | .glassBlock => some Item.glass
    | .blueDungeonBrick => some Item.blueDungeonBrick
    | .greenDungeonBrick => some Item.greenDungeonBrick
    | .pinkDungeonBrick => some Item.pinkDungeonBrick
    | .placedBottle => some Item.bottle
    | .crystal => some Item.crystal
    | .crystalShard => some Item.crystalShard
    | .gelatinCrystal => some Item.gelatinCrystal
    | .lifeCrystal => some Item.lifeCrystal
    | .highlandBerry => some Item.highlandBerry
    | .deepsteel => some Item.deepsteel
    | .obsidian => some Item.obsidian
    | .honeyBlock => some Item.honeyBlock
    | .crispyHoneyBlock => some Item.crispyHoneyBlock
    | .ash => some Item.ashBlock
    | .obsidianBrick => some Item.obsidianBrick
    | .lihzahrdBrick => some Item.lihzahrdBrick
    | .lihzahrdPressurePlate => some Item.lihzahrdPressurePlate
    | .lihzahrdDartTrap => some Item.superDartTrap
    | .lihzahrdFlameTrap => some Item.flameTrap
    | .lihzahrdSpikyBallTrap => some Item.spikyBallTrap
    | .lihzahrdSpearTrap => some Item.spearTrap
    | .woodenSpikes => some Item.woodenSpike
    | .demoniteBrick => some Item.demoniteBrick
    | .crimtaneBrick => some Item.crimtaneBrick
    | .hellstone => some Item.hellstone
    | .cobweb => some Item.cobweb
    | .ice => some Item.ice
    | .iron => some Item.iron
    | .silver => some Item.silver
    | .amber => some Item.amber
    | .geode => some Item.geode
    | .timber => some Item.wood
    | .torch => some Item.torch
    | .workbench => some Item.workbench
    | .chest => some Item.chest
    | .platform => some Item.platform
    | .rail => some Item.rail
    | .homeBeacon => some Item.homeBeacon
    | .doorClosed | .doorClosedTop | .doorOpen | .doorOpenTop | .doorClosedMiddle | .doorOpenMiddle => some Item.door
    | _ => none
  let some drop := item
    | return { state with status := "That tile cannot be mined." }
  let some tool := tool
    | return { state with status := "Select a pick in your hotbar." }
  if tool.pickPower == 0 && !(tile == .cobweb && tool.swordDamage > 0) then
    return { state with status := "Select a pick in your hotbar." }
  let mut mined := state
  if state.legacyEarly then
    let rank := if tool.legacyPickRank > 0 then tool.legacyPickRank
      else if tool.pickPower ≥ 45 then 3 else if tool.pickPower > 0 then 2 else 0
    if tile == .crystal && rank < 2 then
      return { state with status := "Crystal requires a copper pick." }
    if ([.iron, .silver, .geode, .ruinBrick, .deepsteel, .obsidian] : List Tile).contains tile && rank < 3 then
      return { state with status := s!"{drop.name} requires a crystal pick." }
  else if tool.pickPower > 0 then
    let dungeonSurface := (dungeonLayout state.seed).surface
    let centerThird := state.world.width / 3 ≤ position.x.toNat &&
      position.x.toNat < 2 * state.world.width / 3
    let required := if tile.isDungeonBrick && (position.y ≤ dungeonSurface || centerThird) then 1
      else if (tile == .demoniteOre || tile == .crimtaneOre) &&
        position.y < (groundHeight state.seed position.x.toNat : Int) then 1
      else tile.minimumPickPower
    if tool.pickPower < required then
      return { state with status := s!"{drop.name} requires {required}% pickaxe power." }
    if state.toolCooldown > 0 then return state
    if tile == .corruptGrass || tile == .crimsonGrass || tile == .hallowedGrass then
      return { state with
        world := state.world.setTile position .dirt
        toolCooldown := tool.toolSpeed
        status := "Cleared the grass." }
    let (hit, broke) := state.recordToolHit tool position tile (tile.pickDamage tool.pickPower)
    if !broke then return hit
    mined := hit
  let some (collected, index) := Inventory.addOne state.inventory drop
    | return { state with status := "Your inventory is full." }
  let door := doorCells state.world position
  let mut world := if door.isEmpty then state.world.setTile position .air
    else door.foldl (fun terrain cell => terrain.setTile cell .air) state.world
  if !state.keepsCarts world then
    return { state with status := "Move or recover the minecart before changing its track." }
  let mut inventory := collected
  let mut recovered := 0
  for neighbor in neighbors position do
    if world.tile neighbor == .torch && !hasTorchSupport world neighbor then
      let some (updated, _) := Inventory.addOne inventory .torch
        | return { state with status := "Make room to recover the torch first." }
      inventory := updated
      world := world.setTile neighbor .air
      recovered := recovered + 1
    if (world.tile neighbor).isHallowCrystal && !world.crystalShardSupport neighbor then
      let item := if world.tile neighbor == .gelatinCrystal then Item.gelatinCrystal else .crystalShard
      let some (updated, _) := Inventory.addOne inventory item
        | return { state with status := "Make room to recover the crystal first." }
      inventory := updated
      world := world.setTile neighbor .air
  let removedAnchor := tile == .homeBeacon && state.homeAnchor == some position
  let status := if removedAnchor then "Home Beacon collected. Bind a new beacon to recall."
    else if recovered > 0 then
      let torches := if recovered == 1 then "a torch" else s!"{recovered} torches"
      s!"Collected {drop.name} and recovered {torches}."
    else if drop == .crystal then
      s!"Crystal: {Inventory.count inventory .crystal}/{crystalPickCost} for the next pick."
    else if drop == .iron && state.legacyEarly then
      s!"Iron ore: {Inventory.count inventory .iron}/{ironSwordCost} for an iron sword."
    else if drop == .silver && state.legacyEarly then
      s!"Silver ore: {Inventory.count inventory .silver}/{silverSwordCost} for a silver sword."
    else if index ≥ Inventory.hotbarCount then s!"Stored {drop.name} in the backpack."
    else if drop == .lifeCrystal then "Life Crystal mined. Select and left-click for a permanent heart."
    else s!"Collected {drop.name}."
  let chests := if tile == .chest then state.chests.filter (·.position != position) else state.chests
  let homeAnchor := if removedAnchor then none else state.homeAnchor
  let magma := if tile == .hellstone && position.y > underworldStart then
      Magma.fillFromEighths state.magma world #[(position, 4)]
    else state.magma
  return ({ mined with world, inventory, chests, homeAnchor, status, magma } : GameState)
    |>.updateTempleDoorSupports position |>.dropUnsupportedPressurePlate position

private def Item.placedBlock : Item → Option Tile
    | .dirt => some Tile.dirt
    | .stone => some Tile.stone
    | .obsidian => some Tile.obsidian
    | .honeyBlock => some Tile.honeyBlock
    | .crispyHoneyBlock => some Tile.crispyHoneyBlock
    | .ashBlock => some Tile.ash
    | .obsidianBrick => some Tile.obsidianBrick
    | .lihzahrdBrick => some Tile.lihzahrdBrick
    | .lihzahrdPressurePlate => some Tile.lihzahrdPressurePlate
    | .superDartTrap => some Tile.lihzahrdDartTrap
    | .flameTrap => some Tile.lihzahrdFlameTrap
    | .spikyBallTrap => some Tile.lihzahrdSpikyBallTrap
    | .spearTrap => some Tile.lihzahrdSpearTrap
    | .woodenSpike => some Tile.woodenSpikes
    | .crystalShard => some Tile.crystalShard
    | .demoniteBrick => some Tile.demoniteBrick
    | .crimtaneBrick => some Tile.crimtaneBrick
    | .hellstone => some Tile.hellstone
    | .ice => some Tile.ice
    | .copper => some Tile.copper
    | .tinOre => some Tile.tinOre
    | .iron => some Tile.iron
    | .leadOre => some Tile.leadOre
    | .silver => some Tile.silver
    | .tungstenOre => some Tile.tungstenOre
    | .goldOre => some Tile.goldOre
    | .platinumOre => some Tile.platinumOre
    | .demoniteOre => some Tile.demoniteOre
    | .crimtaneOre => some Tile.crimtaneOre
    | .ebonstone => some Tile.ebonstone
    | .crimstone => some Tile.crimstone
    | .sand => some Tile.sand
    | .mudBlock => some Tile.mud
    | .pearlstone => some Tile.pearlstone
    | .pearlsand => some Tile.pearlsand
    | .ebonsand => some Tile.ebonsand
    | .crimsand => some Tile.crimsand
    | .purpleIce => some Tile.purpleIce
    | .pinkIce => some Tile.pinkIce
    | .redIce => some Tile.redIce
    | .cobaltOre => some Tile.cobaltOre
    | .palladiumOre => some Tile.palladiumOre
    | .mythrilOre => some Tile.mythrilOre
    | .orichalcumOre => some Tile.orichalcumOre
    | .adamantiteOre => some Tile.adamantiteOre
    | .titaniumOre => some Tile.titaniumOre
    | .glass => some Tile.glassBlock
    | .blueDungeonBrick => some Tile.blueDungeonBrick
    | .greenDungeonBrick => some Tile.greenDungeonBrick
    | .pinkDungeonBrick => some Tile.pinkDungeonBrick
    | .bottle => some Tile.placedBottle
    | .wood => some Tile.timber
    | .torch => some Tile.torch
    | .workbench => some Tile.workbench
    | .chest => some Tile.chest
    | .door => some Tile.doorClosed
    | .platform => some Tile.platform
    | .rail => some Tile.rail
    | .homeBeacon => some Tile.homeBeacon
    | _ => none

def Item.canPlaceInWorld (item : Item) : Bool :=
  item.placedBlock.isSome || item.furnitureKind.isSome ||
    item == .minecart || item == .woodWall || item == .sunleafSeed ||
    item == .corruptSeeds || item == .crimsonSeeds || item == .shadowChest ||
    item == .lihzahrdChest || item == .lihzahrdDoor || item == .lihzahrdAltar

private def GameState.placeTile (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canReach position then
    return { state with status := "Move closer to place that tile." }
  if state.selectedStack.any (·.item == .minecart) then return state.deployCart position
  let some stack := state.selectedStack
    | return { state with status := "Select a block or furnishing in your hotbar." }
  if stack.item == .shadowChest || stack.item == .lihzahrdChest then
    let anchor := above position
    let cells := dungeonChestCells anchor
    let supports := #[below position, ⟨position.x + 1, position.y + 1⟩]
    if state.world.height != underworldHeight || !cells.all (fun cell => state.world.tile cell == .air) ||
        !supports.all (fun cell => (state.world.tile cell).solid || state.world.tile cell == .platform) then
      return { state with status := s!"A {stack.item.name} needs a two-tile opening and supported floor." }
    let tile := if stack.item == .lihzahrdChest then Tile.lihzahrdChest else .shadowOpenChest
    let world := cells.foldl (fun world cell => world.setTile cell tile) state.world
    return { state.consumeSelected with
      world
      chests := state.chests.push ⟨anchor, Array.replicate chestSlots none⟩, status := s!"Placed {stack.item.name}." }
  if stack.item == .lihzahrdDoor then
    let hinge := above (above position)
    let cells := TempleDoor.cells hinge
    if !TempleDoor.supports state.world hinge ||
        !cells.all (fun p => state.world.tile p == .air && !state.occupied p &&
          state.magma.level state.world p == 0 && state.water.level state.world p == 0 &&
          state.honey.level state.world p == 0) then
      return { state with status := "Clear a three-tile doorway with a solid floor and ceiling." }
    return { state.consumeSelected with
      world := TempleDoor.place state.world hinge false (fun p => (hash (state.seed, p, state.surface.clock)).toNat)
      status := "Placed Lihzahrd Door." }
  if stack.item == .lihzahrdAltar then
    let anchor : TilePos := ⟨position.x - 1, position.y - 1⟩
    let cells := TempleLayout.altarCells anchor
    if !cells.all (fun p => state.world.tile p == .air) ||
        !(cells.extract 3).all (fun p => (state.world.tile (below p)).solid) then
      return { state with status := "The altar needs a three-tile solid floor and clear space." }
    return { state.consumeSelected with
      world := TempleLayout.placeAltar state.world anchor
      status := "Placed Lihzahrd Altar." }
  if stack.item == .woodWall then return state.placeWoodWall position
  if let some kind := stack.item.furnitureKind then return state.placeFurniture kind position
  if stack.item == .corruptSeeds || stack.item == .crimsonSeeds then
    if state.world.tile position != .dirt || (state.world.tile (above position)).solid then
      return { state with status := "Plant the seeds on exposed Dirt." }
    let tile := if stack.item == .corruptSeeds then Tile.corruptGrass else .crimsonGrass
    return { state.consumeSelected with
      world := state.world.setTile position tile
      status := s!"Planted {stack.item.name}." }
  if state.world.tile position != .air then
    return { state with status := "Choose an empty tile." }
  if stack.item == .sunleafSeed then
    if !canPlantSunleaf state.world state.water position || state.magma.level state.world position > 0 then
      return { state with status := "Plant Sunleaf in dry open air directly above dirt." }
    return { state.consumeSelected with
      world := state.world.setTile position .sunleafSprout
      crops := state.crops.push ⟨position, 0⟩
      status := "Sunleaf planted. It grows in daylight." }
  let some tile := stack.item.placedBlock
    | return { state with status := "Select a block or furnishing in your hotbar." }
  if tile == .placedBottle && !(state.world.tile (below position)).bottleSupport then
    return { state with status := "Put the bottle on a work bench, table or platform." }
  if tile.solid && (state.magma.level state.world position > 0 ||
      (tile == .doorClosed && state.magma.level state.world (above position) > 0)) then
    return { state with status := "Blocks cannot be placed in lava." }
  if tile.solid && state.occupied position then
    return { state with status := "Choose a tile clear of characters and loot." }
  if tile == .torch && !hasTorchSupport state.world position then
    return { state with status := "Place the torch next to a solid tile." }
  if tile == .lihzahrdPressurePlate && !(state.world.tile (below position)).solid then
    return { state with status := "Place the pressure plate on a solid tile." }
  if tile == .crystalShard && !state.world.crystalShardSupport position then
    return { state with status := "Attach the Crystal Shard to a solid tile." }
  let mut world := state.world.setTile position tile
  if tile == .homeBeacon && (state.homeLanding world position).isNone then
    return { state with status := "The beacon needs two dry tiles over a solid floor." }
  if tile == .doorClosed then
    let top := above position
    let upper := if state.legacyEarly then #[top] else #[top, above top]
    if upper.any (fun cell => state.world.tile cell != .air || state.occupied cell) then
      return { state with status := "Clear the opening before placing the door." }
    if upper.any (fun cell => state.magma.level state.world cell > 0) then
      return { state with status := "Blocks cannot be placed in lava." }
    world := if state.legacyEarly then world.setTile top .doorClosedTop
      else world.setTile top .doorClosedMiddle |>.setTile (above top) .doorClosedTop
    if !doorSupportValid world position then
      return { state with status := if state.legacyEarly then "A door needs a solid floor."
        else "A door needs a three-tile opening between a solid floor and ceiling." }
  if !state.keepsCarts world then
    return { state with status := "Leave room for the minecart and its rider." }
  let displace (liquid other : Water) :=
    if tile == .doorClosed then displaceDoorLiquid liquid world position (Water.liquidBarrier other state.magma)
    else if tile.solid then liquid.displace world position (Water.liquidBarrier other state.magma)
    else some liquid
  let some (water, honey) := do
    let water ← displace state.water state.honey
    let honey ← displace state.honey water
    pure (water, honey)
    | return { state with status := "Make room for the liquid to drain." }
  let chests := if tile == .chest then
      state.chests.push { position, slots := Array.replicate chestSlots none }
    else state.chests
  let status := if tile == .homeBeacon then "Home Beacon placed. Point at it and press Q to bind."
    else s!"Placed {stack.item.name}."
  return { state.consumeSelected with
    world, chests, water, honey
    status }

private def GameState.equipCrafted (state : GameState) (inventory : Inventory) (index : Nat)
    (item : Item) (success : String) : GameState :=
  if index < Inventory.hotbarCount then
    { state with inventory, selected := index, status := success }
  else
    { state with inventory, status := s!"{item.name} went to the backpack. Press Esc to equip it." }

private def GameState.craftPick (state : GameState) : GameState :=
  if !state.nearWorkbench then
    { state with status := "Get closer to the workbench to craft." }
  else
    match tryCraft state.inventory with
    | .ok (inventory, index) =>
      state.equipCrafted inventory index .copperPick "Copper pick equipped. Look for crystal below."
    | .error .alreadyOwned => { state with status := "You already have a copper pick." }
    | .error .insufficientCopper =>
      let available := Inventory.count state.inventory .copper
      { state with status := s!"Copper pick: {available}/{copperPickCost} copper ore." }
    | .error .inventoryFull => { state with status := "Make room for the pick in your inventory." }

private def GameState.craftEquipment (state : GameState) (item : Item) (amount : Nat)
    (ingredients : List (Item × Nat)) (success : String) (requiresWorkbench := true)
    (unique := true) : GameState := Id.run do
  if requiresWorkbench && !state.nearWorkbench then
    return { state with status := "Get closer to the workbench to craft." }
  if unique && item != .minecart && Inventory.stackLimit item == 1 && Inventory.count state.inventory item > 0 then
    return { state with status := s!"You already have a {item.name}." }
  if !state.hasIngredients ingredients then
    let cost := String.intercalate " and " <|
      ingredients.map fun (material, quantity) => s!"{quantity} {material.name.toLower}"
    return { state with status := s!"{item.name} needs {cost}." }
  let spent := state.inventory.spend ingredients
  let some (inventory, index) := Inventory.addItems spent item amount
    | return { state with status := "Make room in your inventory before crafting." }
  return state.equipCrafted inventory index item success

private def GameState.craftClassic (state : GameState) (recipe : ClassicRecipe) (toCursor := false) : GameState :=
  if !state.hasClassicStation recipe then
    { state with status := s!"Move closer to a {recipe.stationName}." }
  else
    let ingredients := state.classicIngredients recipe
    if !state.hasIngredients ingredients then
      { state with status := s!"Missing materials for {recipe.item.name}." }
    else
      let spent := state.inventory.spend ingredients
      let crafted := s!"Crafted {recipe.quantity} × {recipe.item.name}."
      if toCursor then
        if !state.canHoldCraft recipe then
          { state with status := "Make room on the cursor before crafting." }
        else
          let quantity := recipe.quantity + (state.cursorStack.map (·.count)).getD 0
          { state with inventory := spent, cursorStack := some ⟨recipe.item, quantity⟩, status := crafted }
      else
        match Inventory.addItems spent recipe.item recipe.quantity with
        | none => { state with status := "Make room in your inventory before crafting." }
        | some (inventory, index) =>
          if state.legacyEarly then state.equipCrafted inventory index recipe.item crafted
          else { state with inventory, status := crafted }

private def GameState.openSupplyCache (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canOpenSupplyCache position then
    return { state with status := "Step next to the supply cache to open it." }
  let (supplies, message) : List (Item × Nat) × String :=
    if position.x < frostCavernStart then
      ([(.torch, 6), (.wood, 4)], "Cache opened. Found 6 torches and 4 wood.")
    else if position.x < quarryStart then
      ([(.arrow, arrowBatch), (.draught, 1)], s!"Cache opened. Found {arrowBatch} arrows and a healing draught.")
    else
      ([(.draught, 2), (.recallCharm, 1)], "Cache opened. Found 2 healing draughts and a Recall Charm.")
  let mut inventory := state.inventory
  for (item, quantity) in supplies do
    let some (added, _) := Inventory.addItems inventory item quantity
      | return { state with status := "Make room for all the supplies. The cache is still closed." }
    inventory := added
  return { state with inventory, world := state.world.setTile position .openedCache, status := message }

private def GameState.useDoor (state : GameState) (base : TilePos) : GameState := Id.run do
  let cells := doorCells state.world base
  let closing := state.world.tile base == .doorOpen
  if closing && cells.any state.occupied then
    return { state with status := "Clear the doorway before closing it." }
  if closing && cells.any (fun cell => state.magma.level state.world cell > 0) then
    return { state with status := "Lava blocks the doorway." }
  if !closing && cells.size == 3 && !([-1, 1] : List Int).any (fun dx =>
      cells.all fun cell => state.world.tile { cell with x := cell.x + dx } == .air) then
    return { state with status := "Leave a clear column on one side for the door to swing." }
  let mut world := state.world.setTile base (if closing then .doorClosed else .doorOpen)
  let next := above base
  if cells.size == 3 then
    world := world.setTile next (if closing then .doorClosedMiddle else .doorOpenMiddle)
      |>.setTile (above next) (if closing then .doorClosedTop else .doorOpenTop)
  else world := world.setTile next (if closing then .doorClosedTop else .doorOpenTop)
  if !state.keepsCarts world then
    return { state with status := "Leave room for the minecart before closing the door." }
  let displace (liquid other : Water) :=
    if closing then displaceDoorLiquid liquid world base (Water.liquidBarrier other state.magma)
    else some liquid
  let some (water, honey) := do
    let water ← displace state.water state.honey
    let honey ← displace state.honey water
    pure (water, honey)
    | return { state with status := "Make room for the liquid to drain." }
  return { state with world, water, honey, status := if closing then "Door closed." else "Door opened." }

private def GameState.openTempleDoor (state : GameState) (hinge : TilePos) (direction : Int) : GameState :=
  let canSweep (tile : Tile) := tile == .air || tile == .cobweb ||
    tile == .vileMushroom || tile == .viciousMushroom
  let openIn (side : Int) := TempleDoor.openIn state.world hinge side canSweep
  match (openIn direction).orElse (fun _ => openIn (-direction)) with
  | some world => { state with world, status := "Lihzahrd Door opened." }
  | none => { state with status := "Leave a clear column for the door to swing." }

private def GameState.closeTempleDoor (state : GameState) (hinge : TilePos)
    (mode : TempleDoor.Position) : GameState := Id.run do
  let variant (p : TilePos) := (hash (state.seed, p, state.surface.clock, state.weather.day)).toNat
  let some world := TempleDoor.close state.world hinge mode state.occupied variant
    | return { state with status := "Clear the doorway before closing it." }
  if !state.keepsCarts world || (TempleDoor.cells hinge).any (fun p => state.magma.level world p > 0) then
    return { state with status := "The doorway is obstructed." }
  let displace (liquid other : Water) :=
    (TempleDoor.cells hinge).foldlM (fun current p =>
      current.displace world p (Water.liquidBarrier other state.magma)) liquid
  let some (water, honey) := do
    let water ← displace state.water state.honey
    let honey ← displace state.honey water
    pure (water, honey)
    | return { state with status := "Make room for the liquid to drain." }
  return { state with
    world, water, honey, autoTempleDoor := state.autoTempleDoor.filter (· != hinge)
    status := "Lihzahrd Door closed." }

private def GameState.useTempleDoor (state : GameState) (hinge : TilePos)
    (mode : TempleDoor.Position) : GameState := Id.run do
  match mode with
  | .locked =>
    if Inventory.count state.inventory .templeKey == 0 then
      return { state with status := "A Temple Key is needed to unlock this door." }
    let some world := TempleDoor.unlock state.world hinge | return state
    return { state with
      world, inventory := Inventory.remove state.inventory .templeKey 1
      status := "Temple Key used. Lihzahrd Door unlocked." }
  | .closed => return state.openTempleDoor hinge state.combat.facing
  | .openLeft | .openRight => return state.closeTempleDoor hinge mode

private def GameState.useTempleAltar (state : GameState) : GameState := Id.run do
  if !state.hardmode || !state.planteraDefeated || state.golem.isSome then return state
  if Inventory.count state.inventory .lihzahrdPowerCell == 0 then return state
  let paid := { state with inventory := Inventory.remove state.inventory .lihzahrdPowerCell 1 }
  let some (x, y) := TempleLayout.golemSpawnBottom state.world state.player.centerTile | return paid
  return { paid with
    golem := some (GolemBody.atBottom (OriginalCollision.single x) (OriginalCollision.single y))
    status := "Golem has awoken!" }

private def GameState.updateTempleDoors (state : GameState) (horizontal : Int) : GameState := Id.run do
  let body := HitBox.ofPlayer state.player
  let mut next := state
  if let some hinge := next.autoTempleDoor then
    let x := hinge.x * tileUnits
    let y := hinge.y * tileUnits
    let doorway : HitBox := ⟨x - 16, y, x + tileUnits + 16, y + 3 * tileUnits⟩
    if !body.intersects doorway then
      if let some (_, mode) := TempleDoor.atCell next.world hinge then
        if mode == .openLeft || mode == .openRight then
          let closed := next.closeTempleDoor hinge mode
          if closed.world.tiles != next.world.tiles then next := closed
      next := { next with autoTempleDoor := none }
  if horizontal == 0 then return next
  -- Pixels use sixteen game units. The vanilla helper rounds the horizontal
  -- shift away from zero and trims one pixel at the player's head and feet.
  let amount := 16 * horizontal + next.player.vx
  let shift := Int.sign amount * (16 * ((amount.natAbs + 15) / 16) : Nat)
  let box : HitBox := ⟨body.left + shift, body.top + 16, body.right + shift, body.bottom - 16⟩
  for x in [(box.left / tileUnits).toNat:(box.right / tileUnits).toNat + 1] do
    for y in [(box.top / tileUnits).toNat:(box.bottom / tileUnits).toNat + 1] do
      if let some (hinge, .closed) := TempleDoor.atCell next.world ⟨x, y⟩ then
        let top := hinge.y * tileUnits
        let door : HitBox := ⟨hinge.x * tileUnits, top, (hinge.x + 1) * tileUnits, top + 4 * tileUnits⟩
        if door.intersects box && door.top ≤ box.top && box.bottom ≤ door.bottom then
          let opened := next.openTempleDoor hinge horizontal
          if opened.world.tiles != next.world.tiles then
            return { opened with autoTempleDoor := some hinge }
  return next

private def GameState.useFramedChest (state : GameState) (anchor : TilePos) : GameState :=
  let tile := state.world.tile anchor
  let shadow := tile == .shadowChest
  let key := if shadow then Item.shadowKey else Item.goldenKey
  if (state.chestAt anchor).isNone then state
  else if tile != .dungeonGoldChest && !shadow then
    { state with openChest := some anchor, status := "Chest opened." }
  else if Inventory.count state.inventory key == 0 && !state.cursorStack.any (·.item == key) then
    { state with status := s!"A {key.name} is needed to open this chest." }
  else
    let used := if shadow then state else if state.cursorStack.any (·.item == key) then state.consumeSelected
      else { state with inventory := Inventory.remove state.inventory key 1 }
    let opened := if shadow then Tile.shadowOpenChest else Tile.dungeonOpenChest
    let world := (dungeonChestCells anchor).foldl
      (fun world cell => world.setTile cell opened) state.world
    { used with
      world, openChest := some anchor
      status := if shadow then "Shadow Chest unlocked." else "Golden Key used. Chest unlocked." }

private def GameState.interactAt (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canReach position then
    return { state with status := "Move closer to interact." }
  if state.canTalkToGuide position then
    return { state with openGuide := true, status := "Talk to the Guide for help or show a material." }
  if state.canTalkToAngler position then
    return { state with
      angler := { state.angler with rescued := true }
      openAngler := true
      showAnglerQuest := false
      status := if state.angler.rescued then state.status else "The Sleeping Angler woke up." }
  if let some bed := state.furnitureAt position then
    if bed.isBedFoot position then
      if state.bedSpawnPoint == some bed.position then
        return { state with bedSpawnPoint := none, status := "Spawn point removed!" }
      if (bedSpawn state.world state.walls bed).isNone then
        return { state with status := "The bed needs an enclosed room, background walls and headroom." }
      return { state with bedSpawnPoint := some bed.position, status := "Spawn point set!" }
  if let some index := state.cartIndex position then
    let rail := state.carts[index]!
    if state.ride.isSome then return { state with status := "Press F8 to dismount first." }
    if !state.canBoardCart rail then return { state with status := "Stand beside the minecart to board." }
    let ride : RailRide := { rail, next := none, progress := 0, speed := 0 }
    return { state with
      player := ride.body, ride := some ride, carts := state.carts.eraseIdxIfInBounds index
      fishing := .idle, grapple := .idle, wingedSlime := none
      status := "Minecart boarded. Hold A or D to drive. F8 to dismount." }
  if state.world.tile position == .supplyCache then return state.openSupplyCache position
  if state.world.tile position == .openedCache then
    return { state with status := "This supply cache is empty." }
  if state.world.tile position == .homeBeacon then
    if (state.homeLanding state.world position).isNone then
      return { state with status := "Clear and dry the beacon's landing space first." }
    return { state with homeAnchor := some position, status := "Home bound. Use a Recall Charm to return." }
  if (state.world.tile position).isTempleAltar then return state.useTempleAltar
  if let some (hinge, mode) := TempleDoor.atCell state.world position then return state.useTempleDoor hinge mode
  if let some base := doorBase state.world position then return state.useDoor base
  if state.oldManPosition == some position then
    return { state with openOldMan := true }
  if (state.world.tile position).isDungeonChest then
    if let some anchor := dungeonChestAnchor state.seed position state.lowerDungeonInstalled
        state.blueMoonDungeonInstalled state.magicMissileDungeonInstalled state.deepDungeonInstalled then
      return state.useFramedChest anchor
  if (state.world.tile position).isShadowChest then
    if let some anchor := state.shadowChestAtCell position then return state.useFramedChest anchor
  if state.world.tile position == .lihzahrdChest then
    if let some anchor := state.templeChestAtCell position then return state.useFramedChest anchor
  if (state.chestAt position).isSome then
    return { state with openChest := some position, status := "Click a stack to move it." }
  if state.merchant.any (fun merchant => merchant.x == position.x && (merchant.y - position.y).natAbs ≤ 1) then
    return { state with openShop := true, status := "Merchant: Buy supplies or sell gathered materials." }
  if state.guide.any (fun guide => guide.x == position.x && (guide.y - position.y).natAbs ≤ 1) then
    return { state with status := "Guide: Close the door at night. Keep a sword ready." }
  return state

private def GameState.canReachChest (state : GameState) (position : TilePos) : Bool :=
  if (state.world.tile position).isFramedChest then
    (dungeonChestCells position).any state.canReach
  else state.canReach position

private def GameState.handleChest (state : GameState) (position : TilePos)
    (input : TickInput) : GameState := Id.run do
  if input.closeChest || !state.canReachChest position then
    return { state with openChest := none }
  let some index := state.chestIndex position | return { state with openChest := none }
  let some chest := state.chests[index]? | return { state with openChest := none }
  if let some (slot, split) := input.chestClick then
    let (slots, cursorStack) := Inventory.click chest.slots state.cursorStack slot split
    return { state with cursorStack, chests := state.chests.set! index { chest with slots } }
  if let some slot := input.chestDeposit then
    if slot == Inventory.trashSlot || (state.inventory[slot]?.join).isNone then return state
    let some (inventory, slots) := Inventory.transfer state.inventory chest.slots slot
      | return { state with status := "The chest has no room for this stack." }
    return { state with
      inventory
      chests := state.chests.set! index { chest with slots }
      status := "Stored in the chest." }
  if let some slot := input.chestWithdraw then
    if (chest.slots[slot]?.join).isNone then return state
    let some (slots, inventory) := Inventory.transfer chest.slots state.inventory slot
      | return { state with status := "Your inventory has no room for this stack." }
    return { state with
      inventory
      chests := state.chests.set! index { chest with slots }
      status := "Taken from the chest." }
  return state.handleInventoryClick input.inventoryClick

private def GameState.handleShop (state : GameState) (input : TickInput) : GameState := Id.run do
  if input.closeShop || !(state.merchant.any state.canReach) then
    return { state with openShop := false }
  if input.sellCursor then
    let some stack := state.cursorStack | return state
    let proceeds := stack.count * sellPrice stack.item
    if proceeds == 0 then return { state with status := "The merchant does not buy that item." }
    let some inventory := Inventory.creditCoins state.inventory proceeds
      | return { state with status := "Make room in your inventory for the coins." }
    return { state with inventory, cursorStack := none, status := s!"Sold stack for {proceeds} coins." }
  if let some index := input.buyOffer then
    match buyStock state.inventory state.merchantStock index state.legacyEarly with
    | .ok (inventory, stock) =>
      return { state with inventory, merchantStock := stock, status := "Purchase complete." }
    | .error reason => return { state with status := reason }
  if let some index := input.sellSlot then
    match sellStack state.inventory index with
    | .ok (inventory, proceeds) =>
      return { state with inventory, status := s!"Sold stack for {proceeds} coins." }
    | .error reason => return { state with status := reason }
  return state.handleInventoryClick input.inventoryClick

private def GameState.claimClassicHouse (state : GameState) (position : TilePos)
    (forMerchant : Bool) : GameState :=
  match inspectClassicHome state.world state.walls position with
  | .error reason => { state with status := reason }
  | .ok home =>
    let other := if forMerchant then state.guide else state.merchant
    if other == some home.stand || state.angler.home == some home.stand then
      { state with status := "A resident already lives in this room." }
    else if forMerchant then
      { state with
        merchantDoor := some home.entrance
        merchant := some home.stand
        status := "The Merchant moved in. Point at the Merchant and press Q." }
    else
      { state with
        homeDoor := some home.entrance
        guide := some home.stand
        status := "Housing assigned to the Guide." }

private def GameState.claimHouse (state : GameState) (position : TilePos) (forMerchant := false) :
    GameState := Id.run do
  if !state.canReach position then
    return { state with status := "Move closer to check the room." }
  if !state.legacyEarly then return state.claimClassicHouse position forMerchant
  let some door := doorBase state.world position
    | return { state with status := "Point at a complete door to assign the room." }
  let other := if forMerchant then state.homeDoor else state.merchantDoor
  match inspectSeparateHome state.world door other with
  | .ok resident =>
    if forMerchant then
      return { state with
        merchantDoor := some door
        merchant := some resident
        status := "The Merchant moved in. Point at the Merchant and press Q." }
    return { state with
      homeDoor := some door
      guide := some resident
      status := "The Guide moved in. Point at the Guide and press Q." }
  | .error reason => return { state with status := reason }

private def GameState.refreshHome (state : GameState) : GameState := Id.run do
  let some door := state.homeDoor | return state
  if (doorBase state.world door).isNone then
    return { state with homeDoor := none, guide := none, status := "The Guide's room lost its door." }
  match inspectHome state.world door with
  | .ok position =>
    let status := if state.guide.isNone then "The Guide returned to the repaired room." else state.status
    return { state with guide := some position, status }
  | .error reason =>
    let status := if state.guide.isSome then s!"The Guide left. {reason}" else state.status
    return { state with guide := none, status }

private def GameState.refreshMerchant (state : GameState) : GameState := Id.run do
  let some door := state.merchantDoor | return state
  if (doorBase state.world door).isNone then
    return { state with
      merchantDoor := none
      merchant := none
      openShop := false
      status := "The Merchant's room lost its door." }
  match inspectSeparateHome state.world door state.homeDoor with
  | .ok position =>
    let status := if state.merchant.isNone then "The Merchant returned to the repaired room." else state.status
    return { state with merchant := some position, status }
  | .error reason =>
    let status := if state.merchant.isSome then s!"The Merchant left. {reason}" else state.status
    return { state with merchant := none, openShop := false, status }

private def GameState.refreshClassicHousing (state : GameState) (edited : Option TilePos) : GameState := Id.run do
  let existing := fun anchor => anchor.filter (isClassicEntrance state.world)
  let homeDoor := existing state.homeDoor
  let merchantDoor := existing state.merchantDoor
  let stand := fun entrance => entrance.bind fun position =>
    (inspectClassicHome state.world state.walls position).toOption.map (·.stand)
  let guide := stand homeDoor
  let merchant := stand merchantDoor
  let merchant := merchant.filter (fun pos => some pos != guide)
  let mut next := { state with
    homeDoor, merchantDoor, guide, merchant
    openShop := state.openShop && merchant.isSome }
  if guide.isNone then
    if let some position := edited then
      if let .ok home := inspectClassicHome state.world state.walls position then
        if some home.stand != merchant && state.angler.home != some home.stand then
          next := { next with
            homeDoor := some home.entrance
            guide := some home.stand
            status := "The Guide claimed the first suitable house." }
  if state.guide.isSome && next.guide.isNone then
    return { next with status := "The Guide's house is no longer suitable." }
  return next

private def combatMessage (event : CombatEvent) (previous : String) : String :=
  match event with
  | .swing => previous
  | .hitCrawler => "You hit the cave crawler."
  | .defeatedCrawler => "Cave crawler defeated!"
  | .hurt => "The cave crawler hit you."
  | .respawned => "You were slain by a cave crawler."

private def wardenMessage : WardenEvent → String
  | .started => "Frost Warden awakens. Jump over its charge, then strike."
  | .blocked => "Clear space near the center of the ice hall."
  | .withdrew => "The Frost Warden retreated. Return to challenge it again."
  | .hit => "You struck the Frost Warden."
  | .defeated => "Frost Warden defeated! Walk up to its blade to collect it."
  | .hurt => "The Frost Warden's charge hit you."
  | .fell => "You were slain by the Frost Warden."

private def surfaceMessage : SurfaceEvent → String
  | .spawned .slime => "A night slime is nearby. Shut the door or draw a sword."
  | .spawned .greenSlime => "A Green Slime is nearby. It drops Gel for torches."
  | .spawned .blueSlime => "A Blue Slime is nearby. It drops Gel for torches."
  | .spawned .zombie => "A Zombie approaches. Closed doors keep ordinary Zombies out."
  | .spawned .demonEye => "A Demon Eye is flying nearby."
  | .spawned .cedarHare => "A Cedar Hare is foraging nearby."
  | .spawned .brambleCrawler => "A Bramble Crawler is hunting. Shut the door or draw a sword."
  | .hit kind => s!"You struck the {kind.name}."
  | .defeated .slime => "Night slime defeated."
  | .defeated kind => s!"{kind.name} defeated."
  | .hurt kind => s!"A {kind.name} hit you."
  | .fell => "You were slain by a surface creature."

private def quarryMessage : QuarryEvent → String
  | .hit => "You struck the quarry crawler."
  | .defeated => "Quarry crawler defeated."
  | .hurt => "The quarry crawler hit you."
  | .fell => "You were slain by a quarry crawler."

private def stormMessage : StormEvent → String
  | .started => "Storm Herald summoned. Build cover and watch the volley."
  | .blocked => "Clear the arena so the Storm Herald can appear."
  | .withdrew => "Storm Herald withdrew. Use the altar to challenge it again."
  | .hit => "You struck the Storm Herald."
  | .defeated => "Storm Herald defeated! Walk up to the Storm Bow."

private def ruinMessage : RuinEvent → String
  | .started => "Ruin Sentinel awakened. Move off the mark, then jump the ground wave."
  | .blocked => "Clear space near the ruin altar and inside the arena."
  | .withdrew => "The Sentinel reset. Return to the ruin altar to try again."
  | .hit => "You struck the Ruin Sentinel."
  | .defeated => "Ruin Sentinel defeated! Walk up to the Ruin Hammer."

private def cavernMessage : CavernEvent → String
  | .started => "Deepstone Colossus awakened. Jump the low beam, then land for the high beam."
  | .blocked => "Clear space near the Deepstone Altar and inside the arena."
  | .withdrew => "The Colossus reset. Return to the Deepstone Altar to try again."
  | .hit => "You struck the Colossus while its shell was open."
  | .defeated => "Deepstone Colossus defeated! Walk up to the Deepstone Aegis."

private def raidMessage : RaidEvent → String
  | .started => "Amber Raid incoming. Prepare cover and ready a weapon."
  | .blocked => "Use the Watchhorn on clear ground in Amber Bluffs."
  | .withdrew => "The raiders withdrew. Return with the Watchhorn to try again."
  | .nextWave => "Wave cleared. Another group is approaching."
  | .hit => "You struck a raider."
  | .defeated => "Amber Raid repelled! Collect the Raid Crest."
  | .hurt => "A raider hit you."
  | .fell => "You were slain during the raid."

private def caveMessage : CaveEvent → String
  | .hit => "You hit the cave creature."
  | .defeated => "Creature defeated. Move close to collect its drop."
  | .hurt => "A cave creature hit you."
  | .fell => "You were slain by a cave creature."

private def grappleMessage : GrappleEvent → String
  | .latched => "Hook attached. Hold Up to reel in, or press Space to release."
  | .teleported => "Hook of Dissonance attached. Press Space to release."
  | .missed => "The hook returned. Aim at solid terrain within reach."
  | .detached => "The hook lost its hold."

def GameState.quickHook (state : GameState) : Option Item :=
  state.equipment.hook.orElse fun _ =>
    (state.inventory.extract 0 Inventory.mainSlotCount).findSome? fun stack =>
      stack.map (·.item) |>.filter (fun item => item.gearSlot == some .hook)

def GameState.hasPillion (state : GameState) : Bool :=
  state.equipment.mount == some .gelatinousPillion ||
    (state.inventory.extract 0 Inventory.trashSlot).any (fun stack => stack.any (·.item == .gelatinousPillion))

private def GameState.dismountWingedSlime (state : GameState) : GameState :=
  if state.wingedSlime.isNone then state else
  { state with
    wingedSlime := none
    player := { state.player with
      vx := max (-state.equipment.speed) (min state.equipment.speed state.player.vx)
      vy := max (-maxVerticalSpeed) (min maxVerticalSpeed state.player.vy) } }

private def GameState.mountWingedSlime (state : GameState) : GameState :=
  if state.wingedSlime.isSome || state.tongued || state.combat.health == 0 || state.ride.isSome ||
      state.grapple.attachment.isSome then state
  else if state.player.collides state.world wingedSlimeHeightBoost then
    { state with status := "There is not enough room to mount." }
  else
    let mount := WingedSlime.start state.player
    { state with
      wingedSlime := some mount, grapple := .idle, assassinDash := {}
      player := { state.player with vx := (mount.vx * 16).toInt32.toInt, vy := (mount.vy * 16).toInt32.toInt } }

private def GameState.startGrapple (state : GameState) (item : Item) (aim : WorldAim) : GameState := Id.run do
  if item == Item.hookOfDissonance then
    if let .flying _ := state.grapple then return state
  let ready := match state.ride with
    | some ride => if item == Item.hookOfDissonance then
        let body := cartBody ride.parkingTile
        { state.parkCart ride with
          player := moveBodyDelta state.world { body with y := body.y + tileUnits } 0 0 }
      else state
    | none => state
  if ready.ride.isSome then return { state with status := "Dismount the minecart before using the hook." }
  let flight := if item == Item.hookOfDissonance then
      fireDissonanceHook ready.world ready.player aim.x aim.y ready.grapple.attachment
    else fireHook ready.world ready.player ⟨aim.x / tileUnits, aim.y / tileUnits⟩
  let some flight := flight
    | return { state with status := "Point the hook toward the surrounding terrain." }
  return { ready.dismountWingedSlime with
    grapple := .flying flight
    fishing := if ready.fishing.slot.isSome then ready.fishing else .idle
    status := "Hook launched." }

def GameState.ammoSlotFor (state : GameState) (kind : ShotKind) : Option Nat := Id.run do
  for (start, stop) in [(Inventory.ammoStart, Inventory.trashSlot), (0, Inventory.mainSlotCount)] do
    for index in [start:min stop state.inventory.size] do
      if (state.inventory[index]?.join).any (fun stack =>
          if kind.isBullet then stack.item == .musketBall || stack.item == .silverBullet || stack.item == .tungstenBullet
          else stack.item == .arrow || (kind == .woodenArrow &&
            (stack.item == .unholyArrow || stack.item == .hellfireArrow))) then
        return some index
  return none

private def GameState.spendWeaponAmmo (state : GameState) (index slot : Nat) : Inventory :=
  let roll := (hash (state.seed, state.surface.clock, index, slot, (4983 : Nat))).toNat
  if roll % 100 < state.equipment.ammoSavings then state.inventory
  else Inventory.removeFromSlot state.inventory index 1

private def GameState.fireRanged (state : GameState) (weapon : Item) (kind : ShotKind)
    (target : TilePos) (aim : Option WorldAim := none) : GameState := Id.run do
  if state.bowCooldown > 0 || state.projectiles.size ≥ maxProjectiles || !state.world.contains target then
    return state
  let classicBow := kind == .woodenArrow
  let some index := state.ammoSlotFor kind | return { state with status := if kind.isBullet then "No Musket Balls."
    else "No arrows. Craft arrows at a Work Bench." }
  let some ammo := state.inventory[index]?.join | return state
  let kind := if ammo.item == .unholyArrow then ShotKind.unholyArrow
    else if ammo.item == .hellfireArrow then .hellfireArrow else kind
  let x := state.player.x + playerWidth / 2
  let y := state.player.y + playerHeight / 2
  let precise := if kind == .handgunBullet then aim.filter (WorldAim.valid state.world) else none
  let point := precise.getD (WorldAim.atTile target)
  let targetX := if kind == .handgunBullet && point.x == x && point.y == y then x - tileUnits else point.x
  let targetY := point.y
  if precise.isNone && target == state.player.centerTile then return state
  let some shot := aimShot kind x y targetX targetY | return state
  let bonus := if classicBow then weapon.rangedDamage - 4
    else if ammo.item == .silverBullet || ammo.item == .tungstenBullet then 2 else 0
  let shot := { shot with damageBonus := bonus }
  let slot := firstFreeProjectileSlot state.projectileSlots
  if slot ≥ maxProjectileSlots then return state
  let shot := { shot with slot := some slot }
  let facing := if targetX > x then 1 else if targetX < x || kind == .handgunBullet then -1 else state.combat.facing
  return { state with
    inventory := state.spendWeaponAmmo index slot
    projectiles := state.projectiles.push shot
    handgunFired := kind == .handgunBullet
    bowCooldown := if kind.isBullet then weapon.gunUseTime else if classicBow then weapon.bowUseTime
      else if kind == .highlandBolt then highlandCrossbowCooldown else bowCooldownTicks
    combat := { state.combat with facing } }

private def GameState.fireHellwing (state : GameState) (aim : WorldAim) : GameState := Id.run do
  if state.bowCooldown > 0 || state.aquaAnimation > 0 || state.combat.swing > 1 ||
      state.shortsword.isSome || !aim.valid state.world then return state
  let some index := state.ammoSlotFor .woodenArrow
    | return { state with status := "No arrows. Craft arrows at a Work Bench." }
  let some ammo := state.inventory[index]?.join | return state
  let slot := firstFreeProjectileSlot state.projectileSlots
  if slot ≥ maxProjectileSlots then return state
  let roll (draw : Nat) : Float :=
    ((hash (state.seed, state.surface.clock, slot, draw, (485 : Nat))).toNat % 9007199254740992).toFloat /
      9007199254740992
  let facing := if aim.x < bodyCenterX state.player then -1 else 1
  if ammo.item == .hellfireArrow then
    let flight := HellwingShot.launch state.player aim facing true (roll 0 - roll 1) (roll 2 - roll 3)
    let vx := (flight.vx * (8.9 / 9.4) * 16).toInt32.toInt
    let vy := (flight.vy * (8.9 / 9.4) * 16).toInt32.toInt
    let shot : Projectile := {
      x := bodyCenterX state.player, y := bodyCenterY state.player
      vx, vy, life := ShotKind.hellfireArrow.lifetime, kind := .hellfireArrow, damageBonus := 18, slot := some slot }
    return { state with
      inventory := state.spendWeaponAmmo index slot
      projectiles := state.projectiles.push shot
      hellwingFired := true
      bowCooldown := Item.hellwingBow.bowUseTime
      combat := { state.combat with facing } }
  let shot := HellwingShot.launch state.player aim facing (ammo.item == .unholyArrow)
    (roll 0 - roll 1) (roll 2 - roll 3)
  return { state with
    inventory := state.spendWeaponAmmo index slot
    hellwingShots := state.hellwingShots.push { shot with slot := some slot }
    hellwingFired := true
    bowCooldown := Item.hellwingBow.bowUseTime
    combat := { state.combat with facing } }

private def GameState.useBomb (state : GameState) (target : TilePos) (item : Item) : GameState := Id.run do
  if state.bowCooldown > 0 || state.bombs.size ≥ maxActiveBombs then return state
  let some bomb := throwBomb state.world state.player target (item == .stickyBomb) (item == .dynamite) | return state
  let bomb := { bomb with slot := some (firstFreeProjectileSlot state.projectileSlots) }
  return { state.consumeSelected with
    bombs := state.bombs.push bomb
    bowCooldown := if bomb.dynamite then dynamiteUseTime else bombUseTime
    status := s!"{item.name} lit." }

private def GameState.launchAquaStream (state : GameState) (target : WorldAim) (delay : Nat) : GameState :=
  let seed := (hash (state.seed, state.surface.clock, (22 : Nat))).toNat % aquaLavaSeedLimit
  let shot := AquaStream.fire state.player target state.combat.facing seed.toUInt32
  let shot := { shot with slot := some (firstFreeProjectileSlot state.projectileSlots) }
  let facing := if shot.vx < 0 then -1 else if shot.vx > 0 then 1 else state.combat.facing
  { state with
    aquaStreams := state.aquaStreams.push shot
    aquaShotDelay := delay
    bowCooldown := delay
    combat := { state.combat with facing } }

private def GameState.startAquaUse (state : GameState) (target : WorldAim) : GameState :=
  if state.bowCooldown > 0 || state.aquaStreams.size ≥ maxAquaStreams || !target.valid state.world then state
  else
    -- Mana is paid once at the start of the two-shot animation.
    let cost := state.equipment.manaCost aquaManaCost
    let slow := state.mana < cost
    let started := { state with
      mana := state.mana - cost
      manaRegenDelay := aquaManaDelay
      aquaBurst := some target
      aquaAnimation := if slow then aquaMaxAnimation else 15 }
    started.launchAquaStream target (if slow then aquaMaxShotDelay else 8)

private def GameState.startBlueMoon (state : GameState) (aim : WorldAim) : GameState :=
  if state.blueMoon.isSome || state.sunfury.isSome || state.darkLance.isSome ||
      state.aquaAnimation > 0 || state.shortsword.isSome ||
      state.bowCooldown > 0 || state.combat.swing > 1 || !aim.valid state.world then state
  else
    let sunfury := state.selectedStack.any (·.item == .sunfury)
    let other := if sunfury then Item.blueMoon else Item.sunfury
    let used := state.projectileSlots
    let first := firstFreeProjectileSlot used
    if first ≥ maxProjectileSlots then state
    else
      let second := firstFreeProjectileSlot (used.push first)
      let paired := second < maxProjectileSlots && Inventory.count state.inventory other > 0
      let flail := Flail.launch state.player
      let primary : BlueMoonAttack := { flail := { flail with slot := some (if paired then second else first) }, aim }
      let companion : Option BlueMoonAttack := if paired then some { flail := { flail with slot := some first }, aim } else none
      { state with
        blueMoon := if sunfury then companion else some primary
        sunfury := if sunfury then some primary else companion
        underworldSwing := sunfury }

private def GameState.startDarkLance (state : GameState) (aim : WorldAim) : GameState :=
  if state.darkLance.isSome || state.shortsword.isSome || state.aquaAnimation > 0 || state.bowCooldown > 0 ||
      state.combat.swing > 1 || !aim.valid state.world then state
  else
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then state
    else
      let facing := if aim.x > bodyCenterX state.player then 1 else -1
      let boosted := state.equipment.legs == some .crystalAssassinPants
      let lance := { DarkLance.launch state.player aim facing boosted with slot := some slot }
      { state with darkLance := some lance, underworldSwing := true, combat := { state.combat with facing } }

private def GameState.startMagicMissile (state : GameState) (aim : WorldAim) : GameState :=
  if state.bowCooldown > 0 || state.aquaAnimation > 0 || state.shortsword.isSome ||
      state.combat.swing > 1 || !aim.valid state.world ||
      state.magicMissiles.any (·.missile.guided) then state
  else
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then state
    else
      let facing := if aim.x < bodyCenterX state.player then -1 else 1
      let missile := { MagicMissile.launch state.player aim facing with slot := some slot }
      let cost := state.equipment.manaCost magicMissileManaCost
      let delay := if state.mana < cost then magicMissileSlowUseTime else magicMissileUseTime
      { state with
        magicMissiles := state.magicMissiles.push { missile, aim }
        mana := state.mana - cost
        manaRegenDelay := aquaManaDelay
        bowCooldown := delay
        magicMissileSound := true
        combat := { state.combat with facing } }

private def GameState.startUnderworldMagic (state : GameState) (item : Item) (aim : WorldAim) : GameState := Id.run do
  if state.bowCooldown > 0 || state.aquaAnimation > 0 || state.shortsword.isSome ||
      state.combat.swing > 1 || !aim.valid state.world then return state
  let flower := item == .flowerOfFire
  let scythe := item == .demonScythe
  if item == .flamelash && state.flamelashes.any (·.projectile.missile.guided) then return state
  if !scythe && !Flamelash.canCast state.world state.player state.water state.magma then
    return { state with status := "The spell cannot be cast underwater." }
  let slot := firstFreeProjectileSlot state.projectileSlots
  if slot ≥ maxProjectileSlots then return state
  let facing := if aim.x < bodyCenterX state.player then -1 else 1
  let cost := state.equipment.manaCost
    (if flower then flowerFireManaCost else if scythe then demonScytheManaCost else flamelashManaCost)
  let delay := if flower then (if state.mana < cost then flowerFireSlowUseTime else flowerFireUseTime)
    else if scythe then (if state.mana < cost then demonScytheSlowUseTime else demonScytheUseTime)
    else if state.mana < cost then flamelashSlowUseTime else flamelashUseTime
  let cast := { state with
    mana := state.mana - cost, manaRegenDelay := aquaManaDelay, bowCooldown := delay
    underworldMagicCast := !scythe, underworldNpcSound := state.underworldNpcSound || scythe
    combat := { state.combat with facing } }
  if scythe then
    let shot := DemonSickle.cast state.player aim facing
    return { cast with demonSickles := cast.demonSickles.push { shot with slot := some slot } }
  else if flower then
    let shot := FlowerFireball.launch state.player aim facing
    return { cast with flowerFireballs := cast.flowerFireballs.push { shot with slot := some slot } }
  else
    let projectile := Flamelash.launch state.player aim facing
    let projectile := { projectile with missile := { projectile.missile with slot := some slot } }
    return { cast with flamelashes := cast.flamelashes.push { projectile, aim } }

private def GameState.startValor (state : GameState) (aim : WorldAim) : GameState :=
  if state.valor.isSome || state.bowCooldown > 0 || state.aquaAnimation > 0 || state.shortsword.isSome ||
      state.combat.swing > 1 || !aim.valid state.world then state
  else
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then state
    else
      let facing := if aim.x > bodyCenterX state.player then 1 else -1
      let yoyo := { Yoyo.launch state.player aim facing with slot := some slot }
      { state with valor := some { yoyo, aim }, valorCast := true, combat := { state.combat with facing } }

private def GameState.advanceAquaUse (state : GameState) (aim : Option WorldAim)
    (canShoot : Bool) : GameState :=
  match state.aquaBurst with
  | none => state
  | some oldAim =>
    if !state.selectedStack.any (·.item == .aquaScepter) then
      { state with aquaBurst := none, aquaAnimation := 0, aquaShotDelay := 0 }
    else
      let remaining := state.aquaAnimation - 1
      if remaining == 0 then
        { state with
          aquaBurst := none, aquaAnimation := 0, aquaShotDelay := 0, manaRegenDelay := aquaManaDelay }
      else
        let target := (aim.filter (WorldAim.valid state.world)).getD oldAim
        let delay := state.aquaShotDelay - 1
        let next := { state with
          aquaBurst := some target, aquaAnimation := remaining, aquaShotDelay := delay
          manaRegenDelay := aquaManaDelay }
        if delay == 0 && canShoot && state.aquaStreams.size < maxAquaStreams then
          next.launchAquaStream target (if remaining > 7 then aquaMaxShotDelay else 8)
        else next

private def GameState.throwBone (state : GameState) (target : TilePos) : GameState := Id.run do
  if state.bowCooldown > 0 || state.projectiles.size ≥ maxProjectiles ||
      !state.world.contains target || target == state.player.centerTile then return state
  let x := bodyCenterX state.player
  let y := bodyCenterY state.player
  let targetX := target.x * tileUnits + tileUnits / 2
  let targetY := target.y * tileUnits + tileUnits / 2
  let some shot := aimShot .thrownBone x y targetX targetY | return state
  let shot := { shot with slot := some (firstFreeProjectileSlot state.projectileSlots) }
  return { state.consumeSelected with
    projectiles := state.projectiles.push shot
    bowCooldown := 12
    combat := { state.combat with facing := if targetX < x then -1 else 1 } }

private def GameState.arrowTargets (state : GameState) : Array (NpcTarget × Player) := Id.run do
  let mut targets := #[]
  if let some crawler := state.combat.crawler then targets := targets.push (.crawler, crawler.body)
  if let .fighting warden := state.warden then targets := targets.push (.warden, warden.body)
  if let some creature := state.surface.creature then targets := targets.push (.surface creature.kind, creature.body)
  if let some creature := state.surface.flying then targets := targets.push (.surface creature.kind, creature.body)
  if let some crawler := state.quarry.creature then targets := targets.push (.quarry, crawler.body)
  if let some sentry := state.sentry.creature then targets := targets.push (.sentry, sentry.body)
  if let .fighting herald := state.storm then targets := targets.push (.herald, herald.body)
  if let .fighting sentinel := state.ruin then targets := targets.push (.sentinel, sentinel.body)
  if let .fighting colossus := state.cavern then targets := targets.push (.colossus, colossus.body)
  if let .active wave := state.raid then
    for foe in wave.foes do targets := targets.push (.raider wave.number foe.kind, foe.body)
  for index in [:state.ecology.sites.size] do
    if let some (.alive resident) := state.ecology.sites[index]? then
      targets := targets.push (.cave index, resident.body)
  return targets

def dungeonGuardianBox (motion : EyeMotion) : HitBox :=
  let pixel := tileUnits / 16
  let left := (motion.x - skeletronHeadHalfWidth).tdiv pixel * pixel
  let top := (motion.y - skeletronHeadHalfHeight).tdiv pixel * pixel
  ⟨left, top, left + 2 * skeletronHeadHalfWidth, top + 2 * skeletronHeadHalfHeight⟩

private def GameState.projectileTargets (state : GameState) : Array (NpcTarget × HitBox) := Id.run do
  let mut targets := state.arrowTargets.map (fun (kind, body) => (kind, HitBox.ofPlayer body))
  if let some eye := state.eye then
    if eye.phase != .retreat then
      targets := targets.push (.eye, eye.hitBox)
      for servant in eye.servants do targets := targets.push (.eyeServant servant.id, servant.hitBox)
  if let some eater := state.eater then
    if !eater.retreating then
      targets := targets ++ eater.targets.map (fun (id, box) => (.eaterSegment id, box))
  if let some brain := state.brain then
    if brain.phase == .exposed then targets := targets.push (.brainCore, brain.hitBox)
    if brain.phase == .shielded then
      targets := targets ++ brain.creepers.map (fun creeper => (.brainCreeper creeper.id, creeper.hitBox))
  targets := targets ++ state.evilEnemies.map (fun flyer => (.evilFlyer flyer.id, flyer.hitBox))
  if let some boss := state.skeletron then
    if boss.phase != .retreat then
      targets := targets ++ boss.targets.map (fun (part, box) => (.skeletron part, box))
  if let some guardian := state.dungeonGuardian then
    targets := targets.push (.dungeonGuardian, dungeonGuardianBox guardian)
  targets := targets ++ state.dungeonSlimes.map (fun slime => (.dungeonSlime slime.id, slime.hitBox))
  targets := targets ++ state.darkCasters.map (fun caster => (.darkCaster caster.id, caster.hitBox))
  targets := targets ++ state.waterSpheres.map (fun sphere => (.waterSphere sphere.id, sphere.hitBox))
  targets := targets ++ state.angryBones.map (fun bones => (.angryBones bones.id, bones.hitBox))
  targets := targets ++ state.cursedSkulls.map (fun skull => (.cursedSkull skull.id, skull.hitBox))
  let underworld : Array (Nat × (NpcTarget × HitBox)) :=
    (state.wallOfFlesh.map (fun wall => wall.targets.map (fun (part, box) => (0, .wall part, box)))).getD #[] ++
    state.wallHungry.map (fun hungry => (hungry.id, .wallHungry hungry.id, hungry.hitBox)) ++
    (state.wallLeeches.flatMap fun leech =>
      leech.targets.map (fun (part, box) => (leech.id, .wallLeech leech.id part, box))) ++
    state.lavaSlimes.map (fun slime => (slime.id, .lavaSlime slime.id, slime.hitBox)) ++
    state.underworldFlyers.map (fun flyer => (flyer.id, .underworldFlyer flyer.id, flyer.hitBox)) ++
    state.fireImps.map (fun imp => (imp.id, .fireImp imp.id, imp.hitBox)) ++
    state.burningSpheres.map (fun sphere => (sphere.id, .burningSphere sphere.id, sphere.hitBox)) ++
    (state.boneSerpent.map (fun serpent =>
      serpent.targets.map (fun (part, box) => (serpent.id, .boneSerpent serpent.id part, box)))).getD #[] ++
    state.underworldCritters.map (fun critter => (critter.id, .underworldCritter critter.id, critter.hitBox))
  targets := targets ++ (underworld.toList.mergeSort (fun a b => a.1 ≤ b.1)).toArray.map Prod.snd
  targets := targets ++ state.wraiths.map (fun wraith => (.wraith wraith.id, wraith.hitBox))
  targets := targets ++ state.hardmodeCavern.map (fun enemy => (.hardmodeCavern enemy.id, enemy.hitBox))
  for twin in state.retinazer.toArray ++ state.spazmatism.toArray do
    targets := targets.push (.twin twin.kind, twin.hitBox)
  if let some boss := state.destroyer then
    targets := targets ++ boss.segments.zipIdx.map (fun (segment, part) => (.destroyerSegment part, segment.hitBox))
  targets := targets ++ state.destroyerProbes.map (fun probe => (.destroyerProbe probe.id, probe.hitBox))
  if let some head := state.skeletronPrime then targets := targets.push (.primeHead, head.hitBox)
  targets := targets ++ state.primeArms.map (fun arm => (.primeArm arm.kind, arm.hitBox))
  if let some head := state.plantera then targets := targets.push (.planteraHead, head.hitBox)
  targets := targets ++ state.planteraTentacles.map (fun part => (.planteraTentacle part.id, part.hitBox))
  targets := targets ++ state.planteraSpores.map (fun spore => (.planteraSpore spore.id, spore.hitBox))
  targets := targets ++ state.templeEnemies.map (fun enemy => (.templeEnemy enemy.id, enemy.hitBox))
  if let some body := state.golem then
    if body.initialized && !body.invulnerable then targets := targets.push (.golemBody, body.hitBox)
    targets := targets ++ state.golemFists.map (fun fist =>
      (if fist.kind == .left then .golemLeftFist else .golemRightFist, fist.hitBox))
    if let some head := state.golemHead then
      if !head.detached then targets := targets.push (.golemHead, head.hitBox)
  if let some queen := state.queenSlime then
    if queen.timeLeft > 10 then targets := targets.push (.queenSlime, queen.hitBox)
  return targets ++ state.queenMinions.map (fun slime => (.queenMinion slime.id, slime.hitBox))

private def GameState.daggerTargets (state : GameState) : Array (NpcTarget × HitBox) :=
  state.projectileTargets.filter fun (target, _) => match target with
    | .underworldCritter _ | .waterSphere _ | .burningSphere _ => false
    | .warden => match state.warden with
      | .fighting boss => boss.phase == .recover
      | _ => false
    | _ => true

def GameState.minionTargetBox (state : GameState) : Option HitBox := do
  let target ← state.minionTarget
  return (← state.daggerTargets.find? (·.1 == target)).2

private def GameState.markMinionTarget (state : GameState) (aim : WorldAim) : GameState := Id.run do
  let mut closest : Option (NpcTarget × Int) := none
  for (target, box) in state.daggerTargets do
    let dx := aim.x - max box.left (min box.right aim.x)
    let dy := aim.y - max box.top (min box.bottom aim.y)
    let distance := dx * dx + dy * dy
    if closest.all (fun (_, best) => distance < best) then
      closest := some (target, distance)
  let target := closest.map Prod.fst
  return { state with minionTarget := if target == state.minionTarget then none else target }

private def GameState.summonDagger (state : GameState) (aim : WorldAim) : GameState :=
  if !aim.valid state.world || state.bowCooldown > 0 || state.combat.swing > 1 ||
      state.aquaAnimation > 0 || state.shortsword.isSome then state
  else
    let state := { state with dagger := none }
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then state
    else { state with
      dagger := some { blade := EnchantedDagger.spawn aim.x aim.y, slot }
      bowCooldown := bladeStaffUseTicks + bladeStaffReuseDelay }

def GameState.validDagger (state : GameState) : Bool :=
  if state.dagger.isNone && state.minionTarget.isNone then true else
    let targets := state.daggerTargets.map Prod.fst
    state.minionTarget.all (targets.contains ·) && state.dagger.all fun dagger =>
      state.combat.health > 0 && state.respawnTicks == 0 && dagger.blade.valid state.world &&
      dagger.slot < maxProjectileSlots && dagger.cooldowns.size ≤ maxNpcImmunity &&
      (dagger.cooldowns.toList.map Prod.fst).eraseDups.length == dagger.cooldowns.size &&
      dagger.cooldowns.all (fun (target, ticks) => targets.contains target && 0 < ticks && ticks ≤ bladeStaffLocalImmunity) &&
      (match dagger.phase with
      | .idle | .chase _ => true
      | .recoil elapsed => elapsed < bladeStaffRecovery)

def GameState.validQueenSlime (state : GameState) : Bool :=
  let ids := state.queenMinions.map (·.id)
  state.nextQueenMinionId > 0 && ids.toList.eraseDups.length == ids.size &&
    ids.all (· < state.nextQueenMinionId) && state.queenSlime.all (·.valid state.world) &&
    state.queenMinions.size ≤ maxQueenMinions && state.queenMinions.all (·.valid state.world) &&
    state.queenShots.size ≤ maxQueenShots && state.queenShots.all (·.valid state.world) &&
    ((!state.legacyEarly && state.hardmode) ||
      (state.queenSlime.isNone && ids.isEmpty && state.queenShots.isEmpty && !state.queenSlimeDefeated))

def GameState.validTwins (state : GameState) : Bool :=
  (!state.legacyEarly || (!state.twinsActive && !state.twinsDefeated && state.twinShots.isEmpty)) &&
    state.retinazer.all (fun twin => twin.kind == .retinazer && twin.valid state.world) &&
    state.spazmatism.all (fun twin => twin.kind == .spazmatism && twin.valid state.world) &&
    state.twinShots.size ≤ maxTwinShots && state.twinShots.all (·.valid state.world) &&
    (!state.twinsPending || (state.hardmode && state.altarsSmashed > 0 && isNight state.surface.clock false))

def GameState.validDestroyer (state : GameState) : Bool :=
  let ids := state.destroyerProbes.map (·.id)
  (!state.legacyEarly || (state.destroyer.isNone && ids.isEmpty && !state.destroyerDefeated)) &&
    state.destroyer.all (·.valid state.world) &&
    state.nextDestroyerProbeId > 0 && ids.size ≤ destroyerBodyCount &&
    ids.toList.eraseDups.length == ids.size && ids.all (· < state.nextDestroyerProbeId) &&
    state.destroyerProbes.all (·.valid state.world) &&
    (!state.destroyerPending || (!state.twinsPending && state.hardmode && state.altarsSmashed > 0 &&
      isNight state.surface.clock false))

def GameState.validSkeletronPrime (state : GameState) : Bool :=
  let arms := state.primeArms.map (·.kind)
  (!state.legacyEarly || (state.skeletronPrime.isNone && state.primeShots.isEmpty && !state.primeDefeated)) &&
    state.skeletronPrime.all (·.valid state.world) &&
    (arms.isEmpty || state.skeletronPrime.any (·.initialized)) &&
    arms.size ≤ 4 && arms.toList.eraseDups.length == arms.size &&
    state.primeArms.all (·.valid state.world) &&
    state.primeShots.size ≤ maxProjectileSlots && state.primeShots.all (·.valid state.world) &&
    (!state.primePending || (!state.twinsPending && !state.destroyerPending && state.hardmode &&
      state.altarsSmashed > 0 && isNight state.surface.clock false))

def GameState.canGrowPlanteraBulbs (state : GameState) : Bool :=
  !state.legacyEarly && state.hardmode && state.twinsDefeated && state.destroyerDefeated && state.primeDefeated

def GameState.validPlantera (state : GameState) : Bool :=
  let parts := state.planteraTentacles.map (·.id) ++ state.planteraSpores.map (·.id)
  (!state.planteraUnlocked || state.canGrowPlanteraBulbs) &&
    (state.planteraBulbs.isEmpty || state.planteraUnlocked) &&
    state.planteraBulbs.toList.eraseDups.length == state.planteraBulbs.size &&
    state.planteraBulbs.all (planteraBulbValid state.world) &&
    (state.world.tiles.data.filter (· == Tile.planteraBulb.code)).size == state.planteraBulbs.size * 4 &&
    (state.plantera.isNone && state.planteraSpores.isEmpty && state.planteraShots.isEmpty &&
      !state.planteraDefeated || state.planteraUnlocked) &&
    state.plantera.all (·.valid state.world) &&
    (state.planteraHooks.isEmpty || state.plantera.any (fun head => head.stage ≥ 1)) &&
    (state.planteraTentacles.isEmpty || state.plantera.any (fun head => head.stage == 2)) &&
    state.planteraHooks.size ≤ 3 && state.planteraTentacles.size ≤ 8 && state.planteraSpores.size ≤ 200 &&
    state.planteraHooks.all (·.valid state.world) && state.planteraTentacles.all (·.valid state.world) &&
    state.planteraSpores.all (·.valid state.world) &&
    state.nextPlanteraPartId > 0 && parts.toList.eraseDups.length == parts.size &&
    parts.all (· < state.nextPlanteraPartId) &&
    state.planteraShots.size ≤ maxProjectileSlots && state.planteraShots.all (·.valid state.world) &&
    state.poisonedTicks ≤ 900 && state.poisonProgress < 30 &&
    (state.poisonedTicks > 0 || state.poisonProgress == 0)

def GameState.validUnderworldEnemies (state : GameState) : Bool :=
  let regular := state.lavaSlimes.size + state.underworldFlyers.size + state.fireImps.size
  let ids := state.lavaSlimes.map (·.id) ++ state.underworldFlyers.map (·.id) ++
    state.fireImps.map (·.id) ++ state.burningSpheres.map (·.id) ++ state.boneSerpent.toArray.map (·.id) ++
    state.underworldCritters.map (·.id)
  regular ≤ maxUnderworldNpcs && state.fireImps.size ≤ maxFireImps && state.burningSpheres.size ≤ maxBurningSpheres &&
    state.underworldCritters.size ≤ maxUnderworldCritters &&
    (ids.isEmpty || state.world.height == underworldHeight) && state.nextUnderworldNpcId > 0 &&
    ids.toList.eraseDups.length == ids.size && ids.all (fun id => 0 < id && id < state.nextUnderworldNpcId) &&
    state.lavaSlimes.all (·.valid state.world) && state.underworldFlyers.all (·.valid state.world) &&
    state.fireImps.all (·.valid state.world) && state.burningSpheres.all (·.valid state.world) &&
    state.boneSerpent.all (·.valid state.world) &&
    state.underworldCritters.all (·.valid state.world) &&
    state.demonSickles.all (fun shot => shot.valid state.world && shot.slot.isSome &&
      (!shot.hostile || state.world.height == underworldHeight))

def GameState.validWallEncounter (state : GameState) : Bool :=
  let minionIds := state.wallHungry.map (·.id) ++ state.wallLeeches.map (·.id)
  let otherIds := state.lavaSlimes.map (·.id) ++ state.underworldFlyers.map (·.id) ++
    state.fireImps.map (·.id) ++ state.burningSpheres.map (·.id) ++ state.boneSerpent.toArray.map (·.id) ++
    state.underworldCritters.map (·.id)
  let requiresUnderworld := state.wallOfFlesh.isSome || !minionIds.isEmpty || !state.wallLasers.isEmpty || state.hardmode
  (!requiresUnderworld || state.world.height == underworldHeight) &&
    (!state.horrified || (state.wallOfFlesh.isSome && state.combat.health > 0)) &&
    (!state.tongued || (state.horrified && state.ride.isNone && state.grapple == .idle)) &&
    (!state.guideRespawnPending || state.townGuide.isNone) &&
    (!state.wallHungry.any (·.attached) || state.wallOfFlesh.isSome) &&
    state.wallOfFlesh.all (·.valid state.world) && state.wallHungry.size ≤ maxWallHungry &&
    state.wallLeeches.size ≤ maxWallLeeches && state.wallLasers.size ≤ maxProjectileSlots &&
    state.voodooDolls.size ≤ maxDroppedVoodooDolls &&
    minionIds.toList.eraseDups.length == minionIds.size &&
    minionIds.all (fun id => id < state.nextUnderworldNpcId && !otherIds.contains id) &&
    state.wallHungry.all (·.valid state.world) && state.wallLeeches.all (·.valid state.world) &&
    state.wallLasers.all (fun laser => laser.valid state.world && laser.slot.isSome) &&
    state.voodooDolls.all (·.valid state.world)

def GameState.validHardmode (state : GameState) : Bool :=
  let sites := eyeAltars state.seed ++ evilAltars state.seed
  let enoughAbsent := state.altarsSmashed == 0 ||
    state.altarsSmashed ≤ (sites.filter fun anchor =>
      (altarCells anchor).all (fun cell => !(state.world.tile cell).isEvilAltar)).size
  let ids := state.wraiths.map (·.id)
  enoughAbsent && state.hardmodeOreMask < 2 ^ min 3 state.altarsSmashed &&
    state.nextWraithId > 0 && state.wraiths.size ≤ maxWraiths && state.slowTicks ≤ maxSlowTicks &&
    (state.slowTicks == 0 || (state.combat.health > 0 && state.equipment.accessory != some .fastClock)) &&
    (state.hardmode || (state.altarsSmashed == 0 && state.slowTicks == 0 && ids.isEmpty && state.nextWraithId == 1)) &&
    ids.toList.eraseDups.length == ids.size &&
    state.wraiths.all (fun wraith => wraith.id < state.nextWraithId && wraith.valid state.world)

def GameState.validNpcImmunity (state : GameState) : Bool :=
  let targets := state.projectileTargets.map Prod.fst
  let immunity := state.npcImmunity
  immunity.size ≤ maxNpcImmunity && (immunity.toList.map Prod.fst).eraseDups.length == immunity.size &&
    immunity.all (fun (target, ticks) => 0 < ticks && ticks ≤ npcHitCooldown && targets.contains target)

private def GameState.validFlail (state : GameState) (active : Option BlueMoonAttack) : Bool :=
  active.all fun attack =>
    let targets := state.projectileTargets.map Prod.fst
    state.respawnTicks == 0 && state.combat.health > 0 && attack.flail.valid state.world &&
      attack.aim.valid state.world && attack.flail.slot.any (· < maxProjectileSlots) &&
      attack.cooldowns.size ≤ maxNpcImmunity &&
      (attack.cooldowns.toList.map Prod.fst).eraseDups.length == attack.cooldowns.size &&
      attack.cooldowns.all (fun (target, ticks) => 0 < ticks && ticks ≤ blueMoonHitCooldown && targets.contains target)

def GameState.validBlueMoon (state : GameState) : Bool := state.validFlail state.blueMoon
def GameState.validSunfury (state : GameState) : Bool := state.validFlail state.sunfury

def GameState.validDarkLance (state : GameState) : Bool :=
  state.darkLance.all fun lance => state.respawnTicks == 0 && state.combat.health > 0 &&
    lance.valid && lance.age > 0 && lance.slot.any (· < maxProjectileSlots)

def GameState.validNpcBurns (state : GameState) : Bool :=
  let targets := state.projectileTargets.map Prod.fst
  state.npcBurns.size ≤ maxNpcImmunity &&
    (state.npcBurns.toList.map (·.target)).eraseDups.length == state.npcBurns.size &&
    state.npcBurns.all (fun burn => targets.contains burn.target &&
      (burn.onFire > 0 || burn.shadowflame > 0 || burn.hellfire > 0) && burn.onFire ≤ 479 &&
      burn.shadowflame ≤ 299 && burn.hellfire ≤ 360 && burn.progress < 300)

def GameState.validMagicMissiles (state : GameState) : Bool :=
  state.magicMissiles.size ≤ maxProjectileSlots &&
    (state.magicMissiles.filter (·.missile.guided)).size ≤ 1 &&
    state.magicMissiles.all (fun attack =>
      attack.missile.valid state.world && attack.aim.valid state.world &&
      attack.missile.slot.any (· < maxProjectileSlots) &&
      (!attack.missile.guided || attack.target.isNone))

def GameState.validUnderworldProjectiles (state : GameState) : Bool :=
  let live := state.projectileTargets.map Prod.fst
  state.flowerFireballs.all (fun shot => shot.valid state.world && shot.slot.isSome) &&
    state.hellwingShots.all (fun shot => shot.valid state.world && shot.slot.isSome) &&
    (state.flamelashes.filter (·.projectile.missile.guided)).size ≤ 1 &&
    state.flamelashes.all (fun attack =>
      attack.projectile.valid state.world && attack.projectile.missile.slot.isSome && attack.aim.valid state.world &&
      (!attack.projectile.missile.guided || attack.target.isNone) && attack.target.all live.contains &&
      attack.cooldowns.size ≤ maxNpcImmunity &&
      (attack.cooldowns.toList.map Prod.fst).eraseDups.length == attack.cooldowns.size &&
      attack.cooldowns.all (fun (target, ticks) => 0 < ticks && ticks ≤ flamelashHitCooldown && live.contains target))

def GameState.validValor (state : GameState) : Bool :=
  state.valor.all fun attack => state.respawnTicks == 0 && state.combat.health > 0 &&
    attack.yoyo.age > 0 && attack.yoyo.valid state.world && attack.aim.valid state.world &&
    attack.yoyo.slot.any (· < maxProjectileSlots)

private def GameState.pruneNpcImmunity (state : GameState) : GameState :=
  if state.npcImmunity.isEmpty && state.npcBurns.isEmpty &&
      state.blueMoon.all (·.cooldowns.isEmpty) && state.sunfury.all (·.cooldowns.isEmpty) &&
      state.flamelashes.all (fun attack => attack.cooldowns.isEmpty && attack.target.isNone) then state
  else
    let targets := state.projectileTargets.map Prod.fst
    let live := fun (entry : NpcTarget × Nat) => targets.contains entry.1
    { state with
      npcImmunity := state.npcImmunity.filter live
      npcBurns := state.npcBurns.filter (fun burn => targets.contains burn.target)
      blueMoon := state.blueMoon.map (fun attack => { attack with cooldowns := attack.cooldowns.filter live })
      sunfury := state.sunfury.map (fun attack => { attack with cooldowns := attack.cooldowns.filter live })
      flamelashes := state.flamelashes.map (fun attack =>
        { attack with target := attack.target.filter targets.contains, cooldowns := attack.cooldowns.filter live }) }

private def GameState.tickNpcImmunity (state : GameState) : GameState :=
  let immunity := state.npcImmunity.filterMap fun (target, ticks) =>
    if ticks ≤ 1 then none else some (target, ticks - 1)
  { state with npcImmunity := immunity }

private def GameState.npcImmune (state : GameState) (target : NpcTarget) : Bool :=
  state.npcImmunity.any (fun entry => entry.1 == target)

private def GameState.grantNpcImmunity (state : GameState) (target : NpcTarget) (ticks := npcHitCooldown) : GameState :=
  let immunity := state.npcImmunity.filter (fun entry => entry.1 != target)
  { state with npcImmunity := immunity.push (target, ticks) }

private def NpcTarget.ignoresMeleeTerrain : NpcTarget → Bool
  | .eye | .eyeServant _ | .eaterSegment _ | .brainCore | .brainCreeper _ | .skeletron _
  | .dungeonGuardian | .waterSphere _ | .cursedSkull _ | .burningSphere _ | .boneSerpent _ _
  | .wall _ | .wallHungry _ | .wallLeech _ _ | .wraith _ | .twin _ | .queenSlime
  | .destroyerSegment _ | .destroyerProbe _ | .primeHead | .primeArm _
  | .planteraHead | .planteraTentacle _ | .planteraSpore _ => true
  | _ => false

private def GameState.reachesNpcBox (state : GameState) (target : NpcTarget) (box : HitBox) : Bool :=
  let player := state.player
  let x := max box.left (min box.right (bodyCenterX player))
  let y := max box.top (min box.bottom (bodyCenterY player))
  let contact := { player with x := x - playerWidth / 2, y := y - playerHeight / 2 }
  swordReaches state.world player contact state.combat.facing true target.ignoresMeleeTerrain

private def GameState.recallHome (state : GameState) : GameState := Id.run do
  if state.horrified then
    return { state with combat := state.combat.afterDefeat, status := "You tried to escape the Wall of Flesh." }
  if state.ride.isSome then
    return { state with status := "Dismount the minecart before recalling." }
  let some anchor := state.homeAnchor
    | return { state with status := "Place a Home Beacon and press Q to bind it first." }
  let some player := state.homeLanding state.world anchor
    | return { state with status := "Clear and dry the beacon's landing space. The charm is kept." }
  if state.player.grounded && state.player.centerTile == anchor then
    return { state with status := "You are already at your Home Beacon." }
  if state.arrowTargets.any (fun target => bodiesOverlap target.2 player) then
    return { state with status := "An enemy is on your Home Beacon. The charm is kept." }
  return { state.consumeSelected with
    player
    grapple := .idle, wingedSlime := none
    combat := { state.combat with invulnerable := max 2 state.combat.invulnerable, swing := 0 }
    status := "Returned to your Home Beacon." }

private def GameState.drinkRecallPotion (state : GameState) : GameState := Id.run do
  if state.horrified then
    return { state.consumeSelected with combat := state.combat.afterDefeat, status := "You tried to escape the Wall of Flesh." }
  let player := state.bedArrival.getD (respawn state.world state.player state.combat.crawler)
  return { state.consumeSelected with
    player, ride := none, grapple := .idle, wingedSlime := none,
    fishing := if state.fishing.slot.isSome then state.fishing else .idle
    status := if state.bedSpawnPoint.isSome then "Returned to your bed." else "Returned to the world spawn point." }

private def GameState.dropGel (state : GameState) (position : TilePos) : GameState :=
  let count := 1 + (hash (state.seed, position, state.surface.clock)).toNat % 2
  if let some index := state.gelDrops.findIdx? (·.position == position) then
    let old := state.gelDrops[index]!
    { state with gelDrops := state.gelDrops.set! index { old with count := min 9999 (old.count + count) } }
  else
    let retained := if state.gelDrops.size < maxGelDrops then state.gelDrops else state.gelDrops.extract 1
    { state with gelDrops := retained.push ⟨position, count⟩ }

private def GameState.collectGel (state : GameState) : GameState := Id.run do
  if state.gelDrops.isEmpty then return state
  let mut inventory := state.inventory
  let mut remaining := #[]
  for drop in state.gelDrops do
    if state.tileDistance drop.position ≤ 2 && !(state.world.tile drop.position).solid then
      if let some (added, _) := Inventory.addItems inventory .caveGel drop.count then
        inventory := added
        continue
    remaining := remaining.push drop
  return { state with
    inventory
    gelDrops := remaining
    status := if remaining.size < state.gelDrops.size then "Collected Gel. Craft torches with Wood."
      else state.status }

/-- The native global rules use the player's biome and the NPC's death depth
independently. The mechanical drops stop at the first successful roll. -/
private def GameState.hardmodeLoot (state : GameState) (deathRow : Int) (value : Nat)
    (roll : Nat → Nat) (canDropSouls := true) : Array Stack := Id.run do
  if !state.hardmode || value == 0 then return #[]
  let mut items : Array Stack := #[]
  if canDropSouls && deathRow > state.world.height / 2 && (roll 0 % 5 == 0 || roll 1 % 5 == 0) then
    let (hallow, corrupt, crimson) := state.world.hardmodeZones state.player.centerTile
    if hallow && roll 0 % 5 == 0 then items := items.push ⟨.soulOfLight, 1⟩
    if (corrupt || crimson) && roll 1 % 5 == 0 then items := items.push ⟨.soulOfNight, 1⟩
  let mechanical :=
    if !state.destroyerDefeated && roll 2 % 2500 == 0 then some Item.mechanicalWorm
    else if !state.twinsDefeated && roll 3 % 2500 == 0 then some Item.mechanicalEye
    else if !state.primeDefeated && roll 4 % 2500 == 0 then some Item.mechanicalSkull
    else none
  return items ++ (mechanical.map fun item => ⟨item, 1⟩).toArray

private def wallPixel (units : Int) : Float32 :=
  (Int32.ofInt units).toFloat32 / 16

private def GameState.summonWall (state : GameState) (x y : Int) : GameState :=
  if state.wallOfFlesh.isSome || state.world.height != underworldHeight || y / tileUnits < underworldStart then state
  else { state with
    wallOfFlesh := some (spawnWallAt state.world state.water state.magma state.player (wallPixel x) (wallPixel y))
    status := "Wall of Flesh has awoken!" }

private def GameState.sacrificeGuide (state : GameState) (doll : DroppedVoodooDoll) : GameState :=
  if state.townGuide.isNone then state
  else
    let merchantSlain := doll.count > 1 && state.merchant.isSome
    let sacrificed := { state with
      townGuide := none, openGuide := false, guideRespawnPending := true
      merchant := if merchantSlain then none else state.merchant
      merchantDoor := if merchantSlain then none else state.merchantDoor
      openShop := !merchantSlain && state.openShop
      status := "The Guide was slain." }
    sacrificed.summonWall doll.x doll.y

private def GameState.throwVoodooDoll (state : GameState) : GameState := Id.run do
  let some stack := state.selectedStack | return state
  if stack.item != .guideVoodooDoll then return state
  let doll := DroppedVoodooDoll.launch state.player state.combat.facing stack.count
  let dolls := if state.voodooDolls.size ≥ maxDroppedVoodooDolls then state.voodooDolls.extract 1 else state.voodooDolls
  return { state with
    inventory := if state.cursorStack.isSome then state.inventory else state.inventory.set! state.selected none
    cursorStack := none, voodooDolls := dolls.push doll }

private def GameState.advanceVoodooDolls (initial : GameState) : GameState := Id.run do
  let mut state := { initial with voodooDolls := #[] }
  for doll in initial.voodooDolls do
    let moved := doll.advance state.world state.water
    if doll.touchesLava state.world state.magma then
      state := state.sacrificeGuide doll
    else if moved.touchesLava state.world state.magma then
      state := state.sacrificeGuide moved
    else if moved.valid state.world then
      let inRange := state.combat.health > 0 && moved.pickupDelay == 0 &&
        moved.hitBox.intersects (HitBox.ofPlayer state.player)
      if let some (inventory, _) := if inRange then Inventory.addItems state.inventory .guideVoodooDoll moved.count else none then
        state := { state with inventory }
      else state := { state with voodooDolls := state.voodooDolls.push moved }
  return state

private def GameState.defuseBomb (state : GameState) (position : TilePos) : Option GameState := do
  if !state.canReach position then failure
  let index ← state.bombs.findIdx? (·.position == position)
  let bomb ← state.bombs[index]?
  let item := if bomb.dynamite then Item.dynamite else if bomb.sticky then .stickyBomb else .bomb
  let next := { state with bombs := state.bombs.eraseIdxIfInBounds index }
  return { next.dropLoot position 0 #[⟨item, 1⟩] with status := s!"{item.name} defused." }

private def Tile.isEvilRelic : Tile → Bool
  | .shadowOrb | .crimsonHeart => true
  | _ => false

private def evilObjectAt (world : World) (anchors : Array TilePos) (position : TilePos) (tile : Tile) :
    Option TilePos :=
  anchors.find? fun anchor =>
    let cells := evilRelicCells anchor
    cells.contains position && cells.all (fun cell => world.tile cell == tile)

private def removeEvilObject (world : World) (anchor : TilePos) : World :=
  (evilRelicCells anchor).foldl (fun world cell => world.setTile cell .air) world

private def GameState.awakenEvilBoss (state : GameState) (crimson : Bool) : GameState :=
  if state.eater.isSome || state.brain.isSome || state.combat.health == 0 then state
  else if crimson then
    { state with brain := some (spawnBrain state.world state.player), status := "Brain of Cthulhu has awoken!" }
  else
    { state with eater := some (spawnEater state.world state.player), status := "Eater of Worlds has awoken!" }

private def GameState.summonEvilItem (state : GameState) (item : Item) : GameState :=
  if state.eater.isSome || state.brain.isSome then
    { state with status := "An evil boss is already active." }
  else if !state.canSummonEvil item then
    { state with status := if item == .bloodySpine then "Use Bloody Spine in the Crimson."
      else "Use Worm Food in the Corruption." }
  else
    let summoned := state.awakenEvilBoss (item == .bloodySpine)
    summoned.consumeSelected

private def GameState.breakEvilRelic (state : GameState) (position : TilePos) : GameState := Id.run do
  let tile := evilRelicTile state.seed
  let some anchor := evilObjectAt state.world (evilRelics state.seed) position tile | return state
  let crimson := tile == .crimsonHeart
  let count := state.evilRelicsBroken + 1
  let roll := (hash (state.seed, anchor, count)).toNat % 5
  let rewards := if crimson then
      #[Item.undertaker, .crimsonHeartPet, .panicNecklace, .crimsonRod, .rottedFork]
    else #[Item.musket, .shadowOrbPet, .vilethorn, .ballOHurt, .bandOfStarpower]
  let items := if count == 1 || roll == 0 then #[⟨rewards[0]!, 1⟩, ⟨.musketBall, 100⟩]
    else #[⟨rewards[roll]!, 1⟩]
  let cleared := { state with world := removeEvilObject state.world anchor, evilRelicsBroken := count }
  let status := match count % 3 with
    | 1 => "A horrible chill goes down your spine..."
    | 2 => "Screams echo around you..."
    | _ => if crimson then "The third Crimson Heart shattered." else "The third Shadow Orb shattered."
  let rewarded := { cleared.dropLoot (below anchor) 0 items with status }
  return if count % 3 == 0 then rewarded.awakenEvilBoss crimson else rewarded

private def Tile.cuttableEvilPlant : Tile → Option Item
  | .vileMushroom => some .vileMushroom
  | .viciousMushroom => some .viciousMushroom
  | _ => none

private def GameState.cutEvilPlant (state : GameState) (position : TilePos) : GameState := Id.run do
  let some item := (state.world.tile position).cuttableEvilPlant | return state
  let cut := { state with world := state.world.setTile position .air }
  return cut.dropLoot position 0 #[⟨item, 1⟩]

private def GameState.breakPot (state : GameState) (position : TilePos) : GameState := Id.run do
  let some anchor := evilObjectAt state.world (evilPots state.seed) position .clayPot | return state
  let roll := (hash (state.seed, anchor, (18 : Nat))).toNat
  let variation := roll / 7
  let outcome := if evilPotHasBombs state.seed anchor then 4 else roll % 7
  let items : Array Stack := match outcome with
    | 0 => if state.combat.health < state.combat.healthLimit then #[⟨.heart, 1 + variation % 2⟩] else #[]
    | 1 => #[⟨.torch, 2 + variation % 4⟩]
    | 2 => #[⟨.arrow, 10 + variation % 11⟩]
    | 3 => #[⟨.lesserHealingPotion, 1⟩]
    | 4 => #[⟨.bomb, 2 + variation % 3⟩]
    | _ => #[]
  let coins := if items.isEmpty then 80 + variation % 281 else 0
  let cleared := { state with world := removeEvilObject state.world anchor, status := "Pot shattered." }
  return cleared.dropLoot (below anchor) coins items

private def GameState.cutObjectsAlong (state : GameState) (shot : Projectile) : GameState := Id.run do
  let firstX := min shot.x (shot.x + shot.vx) / tileUnits
  let lastX := max shot.x (shot.x + shot.vx) / tileUnits
  let firstY := min shot.y (shot.y + shot.vy) / tileUnits
  let lastY := max shot.y (shot.y + shot.vy) / tileUnits
  let mut next := state
  for column in [:(lastX - firstX + 1).toNat] do
    for row in [:(lastY - firstY + 1).toNat] do
      let position : TilePos := ⟨firstX + column, firstY + row⟩
      let tile := next.world.tile position
      if tile != .clayPot && tile.cuttableEvilPlant.isNone then continue
      let left := position.x * tileUnits
      let top := position.y * tileUnits
      let box : HitBox := ⟨left, top, left + tileUnits, top + tileUnits⟩
      let (_, cut) := advanceProjectileBoxes next.world shot #[box]
      if cut.isSome then
        next := if tile == .clayPot then next.breakPot position else next.cutEvilPlant position
  return next

private def GameState.dropNightLoot (state : GameState) (enemy : NightCreature) : GameState := Id.run do
  let position := enemy.body.centerTile
  let roll := fun (salt : Nat) => (hash (state.seed, position, state.surface.clock, salt)).toNat
  let mut items : Array Stack := #[]
  if enemy.kind == .zombie then
    if roll 0 % 50 == 0 then items := items.push ⟨.shackle, 1⟩
    if roll 1 % 250 == 0 then items := items.push ⟨.zombieArm, 1⟩
  else
    if roll 2 % 3 == 0 then items := items.push ⟨.lens, 1⟩
    if roll 3 % 100 == 0 then items := items.push ⟨.blackLens, 1⟩
  return state.dropLoot position enemy.kind.rewardCoins
    (items ++ state.hardmodeLoot position.y enemy.kind.rewardCoins (fun salt => roll (650 + salt)))

private def GameState.rewardSurfaceDefeat (state : GameState) (enemy : NightCreature) : GameState :=
  if enemy.kind == .zombie || enemy.kind == .demonEye then state.dropNightLoot enemy
  else
    let position := enemy.body.centerTile
    let roll (salt : Nat) := (hash (state.seed, position, state.surface.clock, 650 + salt)).toNat
    let awarded := (state.creditCoins enemy.kind.rewardCoins).dropLoot position 0
      (state.hardmodeLoot position.y enemy.kind.rewardCoins roll (canDropSouls := !enemy.kind.isDaySlime))
    if enemy.kind.isDaySlime then awarded.dropGel enemy.body.centerTile else awarded

private def Item.floatsAsLoot (item : Item) : Bool :=
  item == .soulOfLight || item == .soulOfNight || item == .soulOfSight || item == .soulOfMight ||
    item == .soulOfFright

private def GameState.moveNightLoot (state : GameState) : Array NightDrop := Id.run do
  if state.surface.clock % 5 != 0 then return state.nightDrops
  let mut moved : Array NightDrop := #[]
  let mut splits := 0
  for drop in state.nightDrops do
    let beneath := below drop.position
    let tile := state.world.tile beneath
    if tile.solid || tile == .platform || state.nightDrops.any (·.position == beneath) ||
        moved.any (·.position == beneath) then
      moved := moved.push drop
    else
      let souls := drop.items.filter (·.item.floatsAsLoot)
      let grounded := drop.items.filter (!·.item.floatsAsLoot)
      let hasGrounded := drop.coins > 0 || !grounded.isEmpty
      if !souls.isEmpty && hasGrounded && state.nightDrops.size + splits ≥ maxNightDrops then
        moved := moved.push drop
      else
        if !souls.isEmpty then moved := moved.push ⟨drop.position, 0, souls⟩
        if hasGrounded then moved := moved.push ⟨beneath, drop.coins, grounded⟩
        if !souls.isEmpty && hasGrounded then splits := splits + 1
  return moved

private def GameState.collectNightLoot (state : GameState) : GameState := Id.run do
  if state.nightDrops.isEmpty then return state
  let mut next := state
  let mut remaining : Array NightDrop := #[]
  for drop in state.moveNightLoot do
    let position := drop.position
    let doll : DroppedVoodooDoll := ⟨position.x * tileUnits + 16, position.y * tileUnits - 80, 0, 0, 1, 0⟩
    let burnsDoll := doll.touchesLava state.world state.magma
    let mut items := drop.items
    if burnsDoll then
      if let some stack := items.find? (·.item == .guideVoodooDoll) then
        next := next.sacrificeGuide { doll with count := stack.count }
        items := items.filter (·.item != .guideVoodooDoll)
    if drop.coins == 0 && items.isEmpty then continue
    let drop := { drop with items }
    let center := state.player.centerTile
    let inRange := if state.equipment.accessory == some .treasureMagnet then
        (center.x - position.x).natAbs ≤ 12 && (center.y - position.y).natAbs ≤ 12
      else state.tileDistance position ≤ 2
    if !inRange || (state.world.tile position).solid then
      remaining := remaining.push { drop with position }
      continue
    let mut uncollected : Array Stack := #[]
    for stack in drop.items do
      if stack.item == .heart then
        let missing := next.combat.healthLimit - next.combat.health
        let used := min stack.count ((missing + 19) / 20)
        let health := min next.combat.healthLimit (next.combat.health + 20 * used)
        next := { next with combat := { next.combat with health } }
        if used < stack.count then uncollected := uncollected.push { stack with count := stack.count - used }
      else if let some (inventory, _) := Inventory.addItems next.inventory stack.item stack.count then
        next := { next with inventory, status := s!"Collected {stack.item.name}." }
      else uncollected := uncollected.push stack
    let mut coins := drop.coins
    if let some inventory := Inventory.creditCoins next.inventory coins then
      next := { next with inventory }
      coins := 0
    if coins > 0 || !uncollected.isEmpty then remaining := remaining.push ⟨position, coins, uncollected⟩
  return { next with nightDrops := remaining }

private def enemyLootPosition (state : GameState) (motion : EyeMotion) : TilePos := Id.run do
  let x := max 0 (min ((state.world.width : Int) - 1) (motion.x / tileUnits))
  let y := max 0 (min ((state.world.height : Int) - 1) (motion.y / tileUnits))
  let y := if y > 0 && state.world.tile ⟨x, y⟩ == .platform &&
      !(state.world.tile ⟨x, y - 1⟩).solid then y - 1 else y
  for distance in [:8] do
    for dx in [:(2 * distance + 1)] do
      let column := x + (dx : Int) - distance
      let dy : Int := distance - (column - x).natAbs
      for row in [y + dy, y - dy] do
        let position : TilePos := ⟨column, row⟩
        if state.world.contains position && !(state.world.tile position).solid then return position
  return state.player.centerTile

private def GameState.defeatEye (state : GameState) (eye : EyeOfCthulhu) : GameState := Id.run do
  let position := enemyLootPosition state eye.motion
  let roll := fun (salt : Nat) => (hash (state.seed, position, state.surface.clock, salt)).toNat
  let crimson := altarTile state.seed == .crimsonAltar
  let ore := if crimson then Item.crimtaneOre else .demoniteOre
  let seeds := if crimson then Item.crimsonSeeds else .corruptSeeds
  let mut items : Array Stack := #[⟨ore, 30 + roll 4 % 61⟩, ⟨seeds, 1 + roll 5 % 3⟩,
    ⟨.unholyArrow, 20 + roll 6 % 31⟩] ++ bossRecoveryDrops .lesserHealingPotion (roll 7) (roll 11)
  if roll 8 % 40 == 0 then items := items.push ⟨.binoculars, 1⟩
  if roll 9 % 7 == 0 then items := items.push ⟨.eyeMask, 1⟩
  if roll 10 % 10 == 0 then items := items.push ⟨.eyeTrophy, 1⟩
  let awarded := state.dropLoot position 30000 items
  return { awarded with eye := none, eyeDefeated := true, status := "Eye of Cthulhu defeated!" }

private def GameState.hurtEye (state : GameState) (damage : Nat) (periodic := false) : GameState :=
  match state.eye with
  | none => state
  | some eye =>
    let wounded := eye.takeHit (if periodic then damage + eye.defense / 2 else damage)
    if wounded.health == 0 then state.defeatEye wounded
    else
      let status := if eye.phase != wounded.phase && wounded.phase == .transformSpinUp then
          "The Eye of Cthulhu is transforming."
        else state.status
      { state with eye := some wounded, status }

private def GameState.hurtEyeServant (state : GameState) (id damage : Nat) : GameState := Id.run do
  let some eye := state.eye | return state
  let some index := eye.servants.findIdx? (·.id == id) | return state
  let some servant := eye.servants[index]? | return state
  let wounded := eye.damageServant index damage
  let next := { state with eye := some wounded }
  if wounded.servants.size == eye.servants.size || state.combat.health == state.combat.healthLimit then return next
  let position := enemyLootPosition state servant.motion
  let roll := (hash (state.seed, position, state.surface.clock, (12 : Nat))).toNat
  return if roll % 12 == 0 then next.dropLoot position 0 #[⟨.heart, 1⟩] else next

private def GameState.finishEvilBoss (state : GameState) (motion : EyeMotion) (crimson : Bool) : GameState := Id.run do
  let position := enemyLootPosition state motion
  let roll (salt : Nat) := (hash (state.seed, position, state.surface.clock, salt)).toNat
  let ore := if crimson then Item.crimtaneOre else .demoniteOre
  let quantity := if crimson then 40 + roll 20 % 51 else 20 + roll 20 % 41
  let mut items : Array Stack := #[⟨ore, quantity⟩] ++
    bossRecoveryDrops .lesserHealingPotion (roll 21) (roll 22)
  if roll 23 % 7 == 0 then items := items.push ⟨if crimson then .brainMask else .eaterMask, 1⟩
  if roll 24 % 10 == 0 then items := items.push ⟨if crimson then .brainTrophy else .eaterTrophy, 1⟩
  if roll 25 % 20 == 0 then items := items.push ⟨if crimson then .boneRattle else .eatersBone, 1⟩
  let name := if crimson then "Brain of Cthulhu" else "Eater of Worlds"
  let rewarded := state.dropLoot position (if crimson then 50000 else 2000) items
  return { rewarded with eater := none, brain := none, evilBossDefeated := true, status := s!"{name} defeated!" }

private def GameState.dropEaterSegment (state : GameState) (segment : WormSegment) : GameState := Id.run do
  let position := enemyLootPosition state segment.motion
  let roll (salt : Nat) := (hash (state.seed, position, state.surface.clock, segment.id, salt)).toNat
  let mut items : Array Stack := #[]
  if roll 1 % 2 == 0 then items := items.push ⟨.shadowScale, 1 + roll 2 % 2⟩
  if roll 3 % 2 == 0 then items := items.push ⟨.demoniteOre, 2 + roll 4 % 4⟩
  return state.dropLoot position 0 items

private def GameState.hurtEater (state : GameState) (id damage : Nat) (periodic := false) : GameState := Id.run do
  let some boss := state.eater | return state
  let some segment := boss.segment? id | return state
  let wounded := boss.damageSegment id damage periodic
  let mut next := { state with eater := some wounded }
  if (wounded.segment? id).isSome then return next
  for chain in boss.chains do
    for piece in chain do
      if (wounded.segment? piece.id).isNone then next := next.dropEaterSegment piece
  if wounded.chains.isEmpty then return next.finishEvilBoss segment.motion false
  if boss.chains.size < wounded.chains.size then
    return { next with status := "The Eater of Worlds split into smaller worms." }
  return next

private def GameState.hurtBrain (state : GameState) (damage : Nat) (periodic := false) : GameState := Id.run do
  let some brain := state.brain | return state
  let wounded := if periodic then { brain with health := brain.health - damage } else brain.takeHit state.player damage
  return if wounded.health == 0 then state.finishEvilBoss brain.motion true else { state with brain := some wounded }

private def GameState.hurtBrainCreeper (state : GameState) (id damage : Nat) (periodic := false) : GameState := Id.run do
  let some brain := state.brain | return state
  let some creeper := brain.creepers.find? (·.id == id) | return state
  let wounded := if periodic && creeper.health > damage then
      { brain with creepers := brain.creepers.map (fun entry =>
          if entry.id == id then { entry with health := entry.health - damage } else entry) }
    else brain.damageCreeper id (if periodic then damage + 5 else damage)
  let next := { state with brain := some wounded }
  if wounded.creepers.size == brain.creepers.size then return next
  let position := enemyLootPosition state creeper.motion
  let roll (salt : Nat) := (hash (state.seed, position, state.surface.clock, id, salt)).toNat
  let mut items : Array Stack := #[]
  if roll 1 % 3 != 0 then items := items.push ⟨.tissueSample, 2 + roll 2 % 4⟩
  if roll 3 % 3 != 0 then items := items.push ⟨.crimtaneOre, 5 + roll 4 % 8⟩
  let next := next.dropLoot position 0 items
  return if wounded.phase == .exposed then { next with status := "The Brain of Cthulhu is exposed!" } else next

private def GameState.hurtEvilFlyer (state : GameState) (id damage : Nat) (periodic := false) : GameState := Id.run do
  let some index := state.evilEnemies.findIdx? (·.id == id) | return state
  let some flyer := state.evilEnemies[index]? | return state
  let health := flyer.health - (if periodic then damage else damageAfterDefense damage 4)
  if health > 0 then
    let hurt := if periodic then { flyer with health } else { flyer with health, ticks := 0, diving := false }
    return { state with evilEnemies := state.evilEnemies.set! index hurt }
  let position := enemyLootPosition state flyer.motion
  let roll := (hash (state.seed, position, state.surface.clock, id)).toNat
  let item := if flyer.kind == .eaterOfSouls then Item.rottenChunk else .vertebra
  let items := if roll % 3 == 0 then #[⟨item, 1⟩] else #[]
  let global := state.hardmodeLoot ((flyer.motion.y - 10 * 16) / tileUnits) 90
    fun salt => (hash (state.seed, state.surface.clock, id, 650 + salt)).toNat
  let next := { state with evilEnemies := state.evilEnemies.eraseIdxIfInBounds index }
  return { next.dropLoot position 90 (items ++ global) with status := s!"{flyer.kind.name} defeated." }

private def GameState.hurtSkeletron (state : GameState) (part : SkeletronPart) (damage : Nat) (periodic := false) :
    GameState := Id.run do
  let some boss := state.skeletron | return state
  if boss.phase == .retreat then return state
  let injured := boss.takeHit part (if periodic then damage + boss.defense part / 2 else damage)
  if injured.health > 0 then return { state with skeletron := some injured }
  let position := enemyLootPosition state boss.motion
  let roll := fun (salt : Nat) => (hash (state.seed, position, state.surface.clock, salt)).toNat
  let mut drops := bossRecoveryDrops .healingPotion (roll 130) (roll 136)
  if roll 131 % 7 == 0 then drops := drops.push ⟨.skeletronMask, 1⟩
  else if roll 132 % 7 == 0 then drops := drops.push ⟨.skeletronHand, 1⟩
  else if roll 133 % 7 == 0 then drops := drops.push ⟨.bookOfSkulls, 1⟩
  if roll 134 % 10 == 0 then drops := drops.push ⟨.skeletronTrophy, 1⟩
  if roll 135 % 7 == 0 then drops := drops.push ⟨.chippysCouch, 1⟩
  let released := state.dropLoot position 50000 drops
  return { released with
    skeletron := none, skeletronDefeated := true, oldManWaiting := false, openOldMan := false
    dungeonGuardian := none, dungeonGuardianHealth := 0
    status := "Skeletron has been defeated!" }

private def GameState.hurtDungeonGuardian (state : GameState) (damage : Nat) : GameState :=
  match state.dungeonGuardian with
  | none => state
  | some guardian =>
    let health := state.dungeonGuardianHealth - damageAfterDefense damage 4999
    if health > 0 then { state with dungeonGuardianHealth := health }
    else
      let released := state.dropLoot (enemyLootPosition state guardian) 0 #[⟨.boneKey, 1⟩]
      { released with
        dungeonGuardian := none, dungeonGuardianHealth := 0
        status := "The Dungeon Guardian dropped a Bone Key." }

private def GameState.hurtDungeonSlime (state : GameState) (id damage : Nat) (periodic := false) : GameState := Id.run do
  let some index := state.dungeonSlimes.findIdx? (·.id == id) | return state
  let some slime := state.dungeonSlimes[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage ((dungeonSlimeDefense + 1) / 2)
  let health := slime.health - dealt
  if health > 0 then
    return { state with dungeonSlimes := state.dungeonSlimes.set! index { slime with health } }
  let tile := slime.tile
  let position := enemyLootPosition state ⟨tile.x * tileUnits, tile.y * tileUnits, 0, 0⟩
  let coins := dungeonSlimeCoins fun salt => (hash (state.seed, position, state.surface.clock, id, 174 + salt)).toNat
  let global := state.hardmodeLoot tile.y coins
    fun salt => (hash (state.seed, position, state.surface.clock, id, 650 + salt)).toNat
  let next := { state with dungeonSlimes := state.dungeonSlimes.eraseIdxIfInBounds index }
  return { next.dropLoot position coins (#[⟨.goldenKey, 1⟩] ++ global) with
    status := "Dungeon Slime dropped a Golden Key." }

/-- The common Classic pre-Hardmode Dungeon item chain. The unimplemented
Bone Wand and Tally Counter still consume their original successful rolls. -/
private def dungeonEnemyItem (roll : Nat → Nat) : Option Stack :=
  if roll 0 % 250 == 0 || roll 1 % 100 == 0 then none
  else if roll 2 % 65 == 0 then some ⟨.goldenKey, 1⟩
  else some ⟨.bone, 1 + roll 3 % 3⟩

private def GameState.hurtDarkCaster (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic := false) : GameState := Id.run do
  let some index := state.darkCasters.findIdx? (·.id == id) | return state
  let some caster := state.darkCasters[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage ((darkCasterDefense + 1) / 2)
  let health := caster.health - dealt
  if health > 0 then
    let interrupted := { caster with health, clock := if periodic then caster.clock else 400 }
    let hurt := (knockback.map fun (side, force) => interrupted.knockback dealt side force).getD interrupted
    return { state with darkCasters := state.darkCasters.set! index hurt, status := "You hit the Dark Caster." }
  let position := enemyLootPosition state ⟨(caster.centerX * 16).toInt32.toInt,
    (caster.centerY * 16).toInt32.toInt, 0, 0⟩
  let roll := fun (salt : Nat) => (hash (state.seed, position, state.surface.clock, id, salt)).toNat
  let coins := dungeonEnemyCoins 140 (fun i => roll (1810 + i))
  let loot := dungeonEnemyItem (fun i => roll (1828 + i))
  let next := { state with darkCasters := state.darkCasters.eraseIdxIfInBounds index }
  let global := state.hardmodeLoot (caster.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  return { next.dropLoot position coins (loot.toArray ++ global) with
    status := (loot.map fun stack => s!"Dark Caster dropped {stack.item.name}.").getD "Dark Caster defeated." }

private def GameState.hurtAngryBones (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic := false) : GameState := Id.run do
  let some index := state.angryBones.findIdx? (·.id == id) | return state
  let some bones := state.angryBones[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage ((bones.kind.defense + 1) / 2)
  let health := bones.health - dealt
  if health > 0 then
    let hurt := { bones with health, justHit := bones.justHit || !periodic }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with angryBones := state.angryBones.set! index hurt, status := "You hit Angry Bones." }
  let position := enemyLootPosition state ⟨(bones.centerX * 16).toInt32.toInt,
    (bones.centerY * 16).toInt32.toInt, 0, 0⟩
  let roll := fun (salt : Nat) => (hash (state.seed, position, state.surface.clock, id, salt)).toNat
  let coins := dungeonEnemyCoins (Int32.ofNat bones.kind.coins).toFloat32 (fun i => roll (1930 + i))
  let loot := dungeonEnemyItem (fun i => roll (1948 + i))
  let next := { state with angryBones := state.angryBones.eraseIdxIfInBounds index }
  let global := state.hardmodeLoot (bones.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  return { next.dropLoot position coins (loot.toArray ++ global) with
    status := (loot.map fun stack => s!"Angry Bones dropped {stack.item.name}.").getD "Angry Bones defeated." }

private def GameState.hurtCursedSkull (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic := false) : GameState := Id.run do
  let some index := state.cursedSkulls.findIdx? (·.id == id) | return state
  let some skull := state.cursedSkulls[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage ((cursedSkullDefense + 1) / 2)
  let health := skull.health - dealt
  if health > 0 then
    let hurt := { skull with health }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with cursedSkulls := state.cursedSkulls.set! index hurt, status := "You hit the Cursed Skull." }
  -- Loot can remain inside the wall where the skull was struck.
  let position : TilePos := ⟨skull.centerX.toInt32.toInt / 16, skull.centerY.toInt32.toInt / 16⟩
  let position := if state.world.contains position then position else state.player.centerTile
  let roll := fun (salt : Nat) => (hash (state.seed, position, state.surface.clock, id, salt)).toNat
  let coins := dungeonEnemyCoins (Int32.ofNat cursedSkullCoins).toFloat32 (fun i => roll (2200 + i))
  let loot := (dungeonEnemyItem (fun i => roll (2218 + i))).toArray
  let loot := if roll 2222 % 100 == 0 then loot.push ⟨.nazar, 1⟩ else loot
  let next := { state with cursedSkulls := state.cursedSkulls.eraseIdxIfInBounds index }
  let global := state.hardmodeLoot (skull.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  return { next.dropLoot position coins (loot ++ global) with status := "Cursed Skull defeated." }

private def UnderworldFlyerKind.name : UnderworldFlyerKind → String
  | .hellbat => "Hellbat"
  | .demon => "Demon"
  | .voodooDemon => "Voodoo Demon"

private def GameState.underworldLootPosition (state : GameState) (box : HitBox) : TilePos :=
  let position : TilePos := ⟨(box.left + box.right) / (2 * tileUnits), (box.top + box.bottom) / (2 * tileUnits)⟩
  if state.world.contains position then position else state.player.centerTile

private def GameState.underworldLootRoll (state : GameState) (id draw : Nat) : Nat :=
  (hash (state.seed, state.surface.clock, id, draw, (59 : Nat))).toNat

private def GameState.cascadeLoot (state : GameState) (position : TilePos) (roll : Nat) : Array Stack :=
  if !state.hardmode && state.skeletronDefeated && position.y > underworldStart - 4 && roll % 400 == 0 then
    #[⟨.cascade, 1⟩]
  else #[]

private def GameState.hurtLavaSlime (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.lavaSlimes.findIdx? (·.id == id) | return state
  let some slime := state.lavaSlimes[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage ((lavaSlimeDefense + 1) / 2)
  let health := slime.health - dealt
  if health > 0 then
    let hurt := { slime with health }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with lavaSlimes := state.lavaSlimes.set! index hurt, status := "You hit the Lava Slime." }
  let position := state.underworldLootPosition slime.hitBox
  let coins := dungeonEnemyCoins lavaSlimeCoins (state.underworldLootRoll id)
  let next := { state with lavaSlimes := state.lavaSlimes.eraseIdxIfInBounds index }
  let roll := state.underworldLootRoll id
  let loot := state.cascadeLoot position (roll 16) ++
    state.hardmodeLoot (slime.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  return { next.dropLoot position coins loot with status := "Lava Slime defeated." }

private def GameState.hurtUnderworldFlyer (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.underworldFlyers.findIdx? (·.id == id) | return state
  let some flyer := state.underworldFlyers[index]? | return state
  let kind := flyer.kind
  let dealt := if periodic then damage else damageAfterDefense damage ((kind.defense + 1) / 2)
  let health := flyer.health - dealt
  if health > 0 then
    let hurt := { flyer with health }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with underworldFlyers := state.underworldFlyers.set! index hurt, status := s!"You hit the {kind.name}." }
  let roll := state.underworldLootRoll id
  let coins := dungeonEnemyCoins kind.coins roll
  let loot : Array Stack := match kind with
    | .hellbat =>
      (if roll 13 % 150 == 0 then #[⟨.magmaStone, 1⟩] else #[]) ++
      (if roll 14 % 300 == 0 then #[⟨.batBat, 1⟩] else #[])
    | .demon | .voodooDemon =>
      (if kind == .voodooDemon then #[⟨.guideVoodooDoll, 1⟩] else #[]) ++
      (if roll 15 % 35 == 0 then #[⟨.demonScythe, 1⟩] else #[])
  let position := state.underworldLootPosition flyer.hitBox
  let next := { state with underworldFlyers := state.underworldFlyers.eraseIdxIfInBounds index }
  let global := state.hardmodeLoot (flyer.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  return { next.dropLoot position coins (loot ++ state.cascadeLoot position (roll 16) ++ global) with
    status := s!"{kind.name} defeated." }

private def GameState.hurtFireImp (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.fireImps.findIdx? (·.id == id) | return state
  let some imp := state.fireImps[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage ((fireImpDefense + 1) / 2)
  let health := imp.health - dealt
  if health > 0 then
    let hurt := { imp with health, clock := if periodic then imp.clock else 400 }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with fireImps := state.fireImps.set! index hurt, status := "You hit the Fire Imp." }
  let roll := state.underworldLootRoll id
  let coins := dungeonEnemyCoins (Int32.ofNat fireImpCoins).toFloat32 roll
  let loot : Array Stack :=
    (if roll 13 % 20 == 0 then #[⟨.obsidianRose, 1⟩] else #[]) ++
    (if roll 14 % 250 == 0 then #[⟨.plumbersHat, 1⟩] else #[])
  let position := state.underworldLootPosition imp.hitBox
  let next := { state with fireImps := state.fireImps.eraseIdxIfInBounds index }
  let global := state.hardmodeLoot (imp.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  return { next.dropLoot position coins (loot ++ state.cascadeLoot position (roll 16) ++ global) with
    status := "Fire Imp defeated." }

private def GameState.hurtBoneSerpent (state : GameState) (id part damage : Nat) (periodic : Bool) :
    GameState := Id.run do
  let some serpent := state.boneSerpent | return state
  if serpent.id != id then return state
  let some segment := serpent.segments[part]? | return state
  let hurt := serpent.damageSegment part damage periodic
  if hurt.health > 0 then
    return { state with boneSerpent := some hurt, status := "You hit the Bone Serpent." }
  let position := state.underworldLootPosition segment.hitBox
  let roll := state.underworldLootRoll id
  let coins := dungeonEnemyCoins boneSerpentCoins roll
  let loot := state.cascadeLoot position (roll 16) ++
    state.hardmodeLoot (segment.hitBox.top / tileUnits) coins (fun i => roll (650 + i))
  let next := { state with boneSerpent := none }
  return { next.dropLoot position coins loot with status := "Bone Serpent defeated." }

private def GameState.finishWall (state : GameState) (wall : WallOfFlesh) : GameState := Id.run do
  let center : TilePos := ⟨(wall.centerX / 16).toInt32.toInt, (wall.centerY / 16).toInt32.toInt⟩
  let radius := wallOfFleshSize / 32 + 1
  let brick := if altarTile state.seed == .crimsonAltar then Tile.crimtaneBrick else .demoniteBrick
  let mut world := state.world
  let mut water := state.water.levels
  let mut magma := state.magma.levels
  let mut honey := state.honey.levels
  for ix in [:2 * radius + 1] do
    for iy in [:2 * radius + 1] do
      let position : TilePos := ⟨center.x + ix - radius, center.y + iy - radius⟩
      if !world.contains position then continue
      if (ix == 0 || ix == 2 * radius || iy == 0 || iy == 2 * radius) && world.tile position == .air &&
          !state.player.overlapsTile position (if state.wingedSlime.isSome then wingedSlimeHeightBoost else 0) then
        world := world.setTile position brick
      let index := position.y.toNat * world.width + position.x.toNat
      water := water.set! index 0
      magma := magma.set! index 0
      if index < honey.size then honey := honey.set! index 0
  let roll := fun (salt : Nat) => (hash (state.seed, center, state.surface.clock, salt)).toNat
  let weapons := #[Item.breakerBlade, .clockworkAssaultRifle, .laserRifle, .firecracker]
  let emblems := #[Item.warriorEmblem, .rangerEmblem, .sorcererEmblem, .summonerEmblem]
  let mut loot : Array Stack := #[⟨.pwnhammer, 1⟩, ⟨weapons[roll 0 % 4]!, 1⟩, ⟨emblems[roll 1 % 4]!, 1⟩]
  if roll 2 % 7 == 0 then loot := loot.push ⟨.wallOfFleshMask, 1⟩
  if roll 3 % 10 == 0 then loot := loot.push ⟨.wallOfFleshTrophy, 1⟩
  loot := loot ++ bossRecoveryDrops .healingPotion (roll 4) (roll 5)
  unless state.hardmode do
    world := convertHardmodeTerrain world state.seed (hardmodeConversionEntropy state.seed state.surface.clock)
  let updated := { state with
    world, water := { levels := water }, magma := { levels := magma }, honey := { levels := honey }
    wallOfFlesh := none, wallHungry := state.wallHungry.filter (!·.attached), hardmode := true, wallDefeatedNow := true
    horrified := false, tongued := false
    status := if state.hardmode then "Wall of Flesh has been defeated!" else "The ancient spirits of light and dark have been released!" }
  -- The compact world stores each loot pile in one tile. Find a free tile
  -- inside the brick frame if the mouth's center overlapped solid terrain.
  let position := ((List.range radius).findSome? fun distance =>
    [⟨center.x, center.y - distance⟩, ⟨center.x, center.y + distance⟩].find? fun pos =>
      world.contains pos && !(world.tile pos).solid).getD center
  return updated.dropLoot position 80000 loot

private def GameState.hurtWall (state : GameState) (part : WallPart) (damage : Nat) (periodic : Bool) : GameState :=
  match state.wallOfFlesh with
  | none => state
  | some wall =>
    let damaged := wall.damagePart part damage periodic
    if damaged.health == 0 then state.finishWall damaged
    else { state with wallOfFlesh := some damaged }

private def GameState.hurtWallHungry (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.wallHungry.findIdx? (·.id == id) | return state
  let hungry := state.wallHungry[index]!
  let dealt := if periodic then damage else damageAfterDefense damage (hungry.defense state.wallOfFlesh / 2)
  if dealt == 0 then return state
  if hungry.health > dealt then
    let hungry := { hungry with
      health := hungry.health - dealt
      hitPause := if hungry.attached && !periodic then 10 else hungry.hitPause }
    let hungry := match knockback with
      | some (side, force) =>
        let force := force * (if hungry.attached then 1.1 else 0.8)
        { hungry with vx := (Int32.ofInt side).toFloat32 * force, vy := -force * 0.5 }
      | none => hungry
    return { state with wallHungry := state.wallHungry.set! index hungry }
  let next := { state with wallHungry := state.wallHungry.eraseIdxIfInBounds index }
  if !hungry.attached then return next
  let detached := hungry.detach state.nextUnderworldNpcId
  let next := { next with wallHungry := next.wallHungry.push detached, nextUnderworldNpcId := detached.id + 1 }
  return if state.world.contains hungry.position then next.dropLoot hungry.position 0 #[⟨.heart, 1⟩] else next

private def GameState.hurtWallLeech (state : GameState) (id part damage : Nat) (periodic : Bool) : GameState := Id.run do
  let some index := state.wallLeeches.findIdx? (·.id == id) | return state
  let leech := state.wallLeeches[index]!
  let some segment := leech.segments[part]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage (leech.defense part / 2)
  if leech.health > dealt then
    return { state with wallLeeches := state.wallLeeches.set! index { leech with health := leech.health - dealt } }
  let next := { state with wallLeeches := state.wallLeeches.eraseIdxIfInBounds index }
  let position : TilePos := ⟨((segment.x + 7) / 16).toInt32.toInt, ((segment.y + 7) / 16).toInt32.toInt⟩
  return if state.world.contains position then next.dropLoot position 0 #[⟨.heart, 1⟩] else next

private def GameState.hurtWraith (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.wraiths.findIdx? (·.id == id) | return state
  let wraith := state.wraiths[index]!
  let dealt := if periodic then damage else damageAfterDefense damage (wraithDefense / 2)
  if dealt == 0 then return state
  if wraith.health > dealt then
    let (side, force) := knockback.getD (state.combat.facing, 0)
    let wraith := if periodic then wraith else wraith.knockback dealt side force
    return { state with wraiths := state.wraiths.set! index { wraith with health := wraith.health - dealt } }
  let next := { state with wraiths := state.wraiths.eraseIdxIfInBounds index }
  let position : TilePos := ⟨(wraith.centerX / 16).toInt32.toInt, (wraith.centerY / 16).toInt32.toInt⟩
  let position := if state.world.contains position then position else state.player.centerTile
  let roll := fun (draw : Nat) => (hash (state.seed, state.surface.clock, id, draw, (82 : Nat))).toNat
  let items : Array Stack := if roll 0 % 100 == 0 then #[⟨.fastClock, 1⟩] else #[]
  let items := items ++ state.hardmodeLoot (wraith.hitBox.top / tileUnits) wraithCoins (fun i => roll (650 + i))
  return { next.dropLoot position wraithCoins items with status := "Wraith defeated." }

private def GameState.hurtHardmodeCavern (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.hardmodeCavern.findIdx? (·.id == id) | return state
  let some enemy := state.hardmodeCavern[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage (enemy.kind.defense / 2)
  let health := enemy.health - dealt
  if health > 0 then
    let hurt := { enemy with health, justHit := enemy.justHit || !periodic }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with hardmodeCavern := state.hardmodeCavern.set! index hurt }
  let position := state.underworldLootPosition enemy.hitBox
  let roll (i : Nat) := (hash (state.seed, state.surface.clock, id, i, (510 : Nat))).toNat
  let chance (i odds : Nat) (item : Item) : Array Stack :=
    if roll i % odds == 0 then #[⟨item, 1⟩] else #[]
  let localLoot := match enemy.kind with
    | .illuminantBat => chance 20 150 .applePie ++ chance 21 300 .batBat ++ chance 22 200 .crystallize
    | .illuminantSlime => #[⟨.caveGel, 1 + roll 20 % 2⟩] ++ chance 21 150 .applePie ++ chance 22 200 .crystallize
    | .clinger => #[⟨.cursedFlame, 2 + roll 20 % 4⟩] ++
        chance 21 200 .meatGrinder ++ chance 22 200 .eaterOfLife ++ chance 23 1500 .monsterMeat
    | .floatyGross => chance 20 3 .vertebra ++ chance 21 100 .vitamins ++
        chance 22 200 .meatGrinder ++ chance 23 200 .eaterOfLife ++ chance 24 1500 .monsterMeat
  let global := state.hardmodeLoot (enemy.hitBox.top / tileUnits) enemy.kind.coins (fun i => roll (650 + i))
  let coins := dungeonEnemyCoins (Int32.ofNat enemy.kind.coins).toFloat32 roll
  let next := { state with hardmodeCavern := state.hardmodeCavern.eraseIdxIfInBounds index }
  return { next.dropLoot position coins (localLoot ++ global) with status := s!"{enemy.kind.name} defeated." }

private def GameState.setTwin (state : GameState) (kind : TwinKind) (twin : Option Twin) : GameState :=
  match kind with
  | .retinazer => { state with retinazer := twin }
  | .spazmatism => { state with spazmatism := twin }

/-- The Classic branches of RegisterBoss_QueenSlime plus boss recovery drops. -/
def queenSlimeRewards (roll : Nat → Nat) : Array Stack :=
  let armor := #[Item.crystalAssassinHood, .crystalAssassinShirt, .crystalAssassinPants]
  let chance (index odds : Nat) (item : Item) : Array Stack :=
    if roll index % odds == 0 then #[⟨item, 1⟩] else #[]
  #[⟨.sparkleSlimeBalloon, 25 + roll 0 % 51⟩] ++ chance 1 7 .queenSlimeMask ++
    #[⟨armor[roll 2 % 3]!, 1⟩] ++ chance 3 4 .bladeStaff ++ chance 4 4 .gelatinousPillion ++
    chance 5 3 .hookOfDissonance ++ chance 6 10 .queenSlimeTrophy ++
    bossRecoveryDrops .greaterHealingPotion (roll 7) (roll 8)

private def GameState.hurtQueenSlime (state : GameState) (damage : Nat) (periodic : Bool) : GameState := Id.run do
  let some queen := state.queenSlime | return state
  let dealt := if periodic then damage else damageAfterDefense damage (queenSlimeDefense / 2)
  if queen.health > dealt then
    return { state with queenSlime := some { queen with health := queen.health - dealt } }
  let position := state.underworldLootPosition queen.hitBox
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, (657 : Nat), draw)).toNat
  let coins := dungeonEnemyCoins 60000 (fun draw => roll (9 + draw))
  return { state.dropLoot position coins (queenSlimeRewards roll) with
    queenSlime := none, queenSlimeDefeated := true, status := "Queen Slime has been defeated!" }

private def GameState.hurtQueenMinion (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.queenMinions.findIdx? (·.id == id) | return state
  let some minion := state.queenMinions[index]? | return state
  let dealt := if periodic then damage else damageAfterDefense damage (minion.kind.defense / 2)
  if minion.health ≤ dealt then
    return { state with queenMinions := state.queenMinions.eraseIdxIfInBounds index }
  let resistance := if minion.kind == .heavenly then 0.8 else 1
  let (vx, vy) := (knockback.map fun (direction, force) =>
    npcKnockbackVelocity minion.vx minion.vy force resistance 0.75 dealt minion.kind.health direction false
    ).getD (minion.vx, minion.vy)
  return { state with queenMinions := state.queenMinions.set! index { minion with health := minion.health - dealt, vx, vy } }

private def GameState.hurtTwin (state : GameState) (kind : TwinKind) (damage : Nat)
    (periodic : Bool) : GameState := Id.run do
  let actor := if kind == .retinazer then state.retinazer else state.spazmatism
  let some twin := actor | return state
  let dealt := if periodic then damage else damageAfterDefense damage (twin.defense / 2)
  if twin.health > dealt then
    return state.setTwin kind (some { twin with health := twin.health - dealt })
  let motion : EyeMotion := ⟨(twin.centerX * 16).toInt32.toInt, (twin.centerY * 16).toInt32.toInt, 0, 0⟩
  let position := enemyLootPosition state motion
  let salt : Nat := if kind == .retinazer then 125 else 126
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, salt, draw)).toNat
  let trophy := if kind == .retinazer then Item.retinazerTrophy else .spazmatismTrophy
  let trophies : Array Stack := if roll 0 % 10 == 0 then #[⟨trophy, 1⟩] else #[]
  let state := { state.setTwin kind none with twinSlain := true }
  if state.twinsActive then
    return { state.dropLoot position 0 trophies with status := s!"{kind.name} was destroyed." }
  let mask : Array Stack := if roll 1 % 7 == 0 then #[⟨.twinMask, 1⟩] else #[]
  let items := trophies ++ mask ++ #[⟨.hallowedBar, 15 + roll 2 % 16⟩, ⟨.soulOfSight, 25 + roll 3 % 16⟩] ++
    bossRecoveryDrops .greaterHealingPotion (roll 4) (roll 5)
  let coins := dungeonEnemyCoins 120000 (fun draw => roll (6 + draw))
  return { state.dropLoot position coins items with
    twinsDefeated := true, status := "The Twins have been defeated!" }

/-- Native Classic rewards use the closest active part to place the boss loot. -/
def destroyerRewards (roll : Nat → Nat) : Array Stack :=
  let mask : Array Stack := if roll 0 % 7 == 0 then #[⟨.destroyerMask, 1⟩] else #[]
  let trophy : Array Stack := if roll 1 % 10 == 0 then #[⟨.destroyerTrophy, 1⟩] else #[]
  #[⟨.hallowedBar, 15 + roll 2 % 16⟩, ⟨.soulOfMight, 25 + roll 3 % 16⟩] ++
    mask ++ trophy ++ bossRecoveryDrops .greaterHealingPotion (roll 4) (roll 5)

private def GameState.hurtDestroyer (state : GameState) (part damage : Nat) (periodic : Bool) :
    GameState := Id.run do
  let some boss := state.destroyer | return state
  let some segment := boss.segments[part]? | return state
  if periodic || damage == 0 then return state
  let dealt := damageAfterDefense damage ((destroyerDefense part + 1) / 2)
  let state := { state with destroyerHitSound := true }
  if dealt ≥ boss.health then
    let px := (Int32.ofInt (bodyCenterX state.player)).toFloat32 / 16
    let py := (Int32.ofInt (bodyCenterY state.player)).toFloat32 / 16
    let distance (body : DestroyerSegment) :=
      let dx := body.centerX - px
      let dy := body.centerY - py
      dx * dx + dy * dy
    let nearest := boss.segments.foldl (fun best body => if distance body < distance best then body else best) segment
    let position := state.underworldLootPosition nearest.hitBox
    let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, (134 : Nat), draw)).toNat
    let coins := dungeonEnemyCoins 120000 (fun draw => roll (6 + draw))
    return { state.dropLoot position coins (destroyerRewards roll) with
      destroyer := none, destroyerDefeated := true, destroyerKilled := true
      status := "The Destroyer has been defeated!" }
  let roll := (hash (state.seed, state.surface.clock, boss.health, part, (139 : Nat))).toNat
  let boss := { boss with health := boss.health - dealt }
  if destroyerBody part && !segment.released && roll % 25 == 0 &&
      state.destroyerProbes.size < destroyerBodyCount then
    let probe := DestroyerProbe.fromSegment state.nextDestroyerProbeId segment
    return { state with
      destroyer := some { boss with segments := boss.segments.set! part { segment with released := true } }
      destroyerProbes := state.destroyerProbes.push probe
      nextDestroyerProbeId := probe.id + 1 }
  return { state with destroyer := some boss }

private def GameState.hurtDestroyerProbe (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.destroyerProbes.findIdx? (·.id == id) | return state
  let some probe := state.destroyerProbes[index]? | return state
  if periodic || damage == 0 then return state
  let dealt := damageAfterDefense damage 10
  let state := { state with destroyerHitSound := true }
  if dealt ≥ probe.health then
    let state := { state with
      destroyerProbes := state.destroyerProbes.eraseIdxIfInBounds index
      probeKilled := true }
    let roll := (hash (state.seed, state.surface.clock, id, (139 : Nat))).toNat
    return if roll % 2 == 0 then state.dropLoot (state.underworldLootPosition probe.hitBox) 0 #[⟨.heart, 1⟩]
      else state
  let (vx, vy) := (knockback.map fun (side, force) =>
    npcKnockbackVelocity probe.vx probe.vy force 0.8 0.75 dealt probeHealth side false).getD (probe.vx, probe.vy)
  let probe := { probe with vx, vy, health := probe.health - dealt, justHit := true }
  return { state with destroyerProbes := state.destroyerProbes.set! index probe }

def skeletronPrimeRewards (roll : Nat → Nat) : Array Stack :=
  let mask : Array Stack := if roll 0 % 7 == 0 then #[⟨.skeletronPrimeMask, 1⟩] else #[]
  let trophy : Array Stack := if roll 1 % 10 == 0 then #[⟨.skeletronPrimeTrophy, 1⟩] else #[]
  #[⟨.hallowedBar, 15 + roll 2 % 16⟩, ⟨.soulOfFright, 25 + roll 3 % 16⟩] ++
    mask ++ trophy ++ bossRecoveryDrops .greaterHealingPotion (roll 4) (roll 5)

private def GameState.hurtPrimeHead (state : GameState) (damage : Nat) (periodic : Bool) :
    GameState := Id.run do
  let some head := state.skeletronPrime | return state
  if damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage ((head.defense + 1) / 2)
  let state := { state with primeHitSound := state.primeHitSound || !periodic }
  if dealt < head.health then
    return { state with skeletronPrime := some { head with health := head.health - dealt } }
  let position := state.underworldLootPosition head.hitBox
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, (127 : Nat), draw)).toNat
  let coins := dungeonEnemyCoins 120000 (fun draw => roll (6 + draw))
  return { state.dropLoot position coins (skeletronPrimeRewards roll) with
    skeletronPrime := none
    primeArms := #[]
    primeDefeated := true
    primeDestroyedPart := true
    status := "Skeletron Prime has been defeated!" }

private def GameState.hurtPrimeArm (state : GameState) (kind : PrimeArmKind) (damage : Nat)
    (periodic : Bool) : GameState := Id.run do
  let some index := state.primeArms.findIdx? (·.kind == kind) | return state
  let some arm := state.primeArms[index]? | return state
  if damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage ((kind.defense + 1) / 2)
  let state := { state with primeHitSound := state.primeHitSound || !periodic }
  if dealt < arm.health then
    return { state with primeArms := state.primeArms.set! index { arm with health := arm.health - dealt } }
  return { state with primeArms := state.primeArms.eraseIdxIfInBounds index, primeDestroyedPart := true }

def planteraRewards (firstKill : Bool) (roll : Nat → Nat) : Array Stack :=
  let weapons : Array Item := #[.grenadeLauncher, .venusMagnum, .nettleBurst, .leafBlower,
    .flowerPow, .waspGun, .seedler, .vulgarDisplayOfFlower]
  let weapon := if firstKill then Item.grenadeLauncher else weapons[roll 5 % weapons.size]!
  let rockets : Array Stack := if weapon == .grenadeLauncher then #[⟨.rocketI, 50 + roll 6 % 101⟩] else #[]
  let optional (item : Item) (chance draw : Nat) : Array Stack :=
    if roll draw % chance == 0 then #[⟨item, 1⟩] else #[]
  #[⟨.templeKey, 1⟩, ⟨weapon, 1⟩] ++ rockets ++
    optional .planteraMask 7 0 ++ optional .seedling 20 1 ++ optional .theAxe 50 2 ++
    optional .pygmyStaff 4 3 ++ optional .thornHook 10 4 ++ optional .planteraTrophy 10 7 ++
    bossRecoveryDrops .greaterHealingPotion (roll 8) (roll 9)

private def GameState.hurtPlantera (state : GameState) (damage : Nat) (periodic : Bool) : GameState := Id.run do
  let some head := state.plantera | return state
  if damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage ((head.defense + 1) / 2)
  let state := { state with planteraHitSound := state.planteraHitSound || !periodic }
  if dealt < head.health then return { state with plantera := some { head with health := head.health - dealt } }
  let position := state.underworldLootPosition head.hitBox
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, (262 : Nat), draw)).toNat
  let coins := dungeonEnemyCoins 150000 (fun draw => roll (10 + draw))
  return { state.dropLoot position coins (planteraRewards (!state.planteraDefeated) roll) with
    plantera := none
    planteraHooks := #[]
    planteraTentacles := #[]
    planteraDefeated := true
    planteraKilledSound := true
    status := "Plantera has been defeated!" }

private def GameState.hurtPlanteraTentacle (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.planteraTentacles.findIdx? (·.id == id) | return state
  let some part := state.planteraTentacles[index]? | return state
  if damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage 10
  let state := { state with planteraHitSound := state.planteraHitSound || !periodic }
  if dealt ≥ part.health then
    return { state with planteraTentacles := state.planteraTentacles.eraseIdxIfInBounds index, planteraKilledSound := true }
  let (vx, vy) := (knockback.map fun (direction, force) =>
    CasterAI.knockback part.vx part.vy part.health dealt direction force 1).getD (part.vx, part.vy)
  return { state with
    planteraTentacles := state.planteraTentacles.set! index { part with health := part.health - dealt, vx, vy } }

def golemRewards (roll : Nat → Nat) : Array Stack :=
  let weapons : Array Item := #[.stynger, .possessedHatchet, .sunStone, .eyeOfGolem,
    .heatRay, .staffOfEarth, .golemFist]
  let weapon := weapons[roll 3 % weapons.size]!
  let bolts : Array Stack := if weapon == .stynger then #[⟨.styngerBolt, 60 + roll 4 % 121⟩] else #[]
  let optional (item : Item) (chance draw : Nat) : Array Stack :=
    if roll draw % chance == 0 then #[⟨item, 1⟩] else #[]
  optional .golemMask 7 0 ++ optional .picksaw 4 1 ++ optional .mobiusStrip 6 2 ++
    #[⟨weapon, 1⟩] ++ bolts ++ #[⟨.beetleHusk, 4 + roll 5 % 5⟩] ++ optional .golemTrophy 10 6 ++
    bossRecoveryDrops .greaterHealingPotion (roll 7) (roll 8)

private def GameState.hurtGolem (state : GameState) (damage : Nat) (periodic : Bool) : GameState := Id.run do
  let some body := state.golem | return state
  if !body.initialized || body.invulnerable || damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage (golemBodyDefense / 2)
  if dealt < body.health then return { state with golem := some { body with health := body.health - dealt } }
  let position := state.underworldLootPosition body.hitBox
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, position, (245 : Nat), draw)).toNat
  let coins := dungeonEnemyCoins 150000 (fun draw => roll (9 + draw))
  return { state.dropLoot position coins (golemRewards roll) with
    golem := none, golemHead := none, golemFists := #[], golemDefeated := true
    status := "Golem has been defeated!" }

private def GameState.hurtGolemHead (state : GameState) (damage : Nat) (periodic : Bool) : GameState :=
  match state.golemHead with
  | some head =>
    if head.detached || damage == 0 then state else
    let dealt := if periodic then damage else damageAfterDefense damage (head.defense / 2)
    if dealt ≥ head.health then { state with golemHead := some head.detach }
    else { state with golemHead := some { head with health := head.health - dealt } }
  | none => state

private def GameState.hurtGolemFist (state : GameState) (kind : GolemFistKind)
    (damage : Nat) (periodic : Bool) : GameState := Id.run do
  let some index := state.golemFists.findIdx? (·.kind == kind) | return state
  let some fist := state.golemFists[index]? | return state
  if damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage (kind.defense / 2)
  let golemFists := if dealt ≥ fist.health then state.golemFists.eraseIdxIfInBounds index
    else state.golemFists.set! index { fist with health := fist.health - dealt }
  return { state with golemFists }

private def GameState.hurtTempleEnemy (state : GameState) (id damage : Nat)
    (knockback : Option (Int × Float32)) (periodic : Bool) : GameState := Id.run do
  let some index := state.templeEnemies.findIdx? (·.id == id) | return state
  let some enemy := state.templeEnemies[index]? | return state
  if damage == 0 then return state
  let dealt := if periodic then damage else damageAfterDefense damage (enemy.kind.defense / 2)
  if dealt < enemy.health then
    let hurt := { enemy with health := enemy.health - dealt, justHit := enemy.justHit || !periodic }
    let hurt := (knockback.map fun (side, force) => hurt.knockback dealt side force).getD hurt
    return { state with
      templeEnemies := state.templeEnemies.set! index hurt
      templeEnemySound := if periodic then state.templeEnemySound else some (enemy.kind, false) }
  let position := state.underworldLootPosition enemy.hitBox
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, id, enemy.kind.npcId, draw)).toNat
  let coins := dungeonEnemyCoins (Int32.ofNat enemy.kind.baseCoins).toFloat32 roll
  let loot := templeEnemyDrops (fun i => roll (20 + i)) ++
    state.hardmodeLoot (enemy.hitBox.top / tileUnits) enemy.kind.baseCoins (fun i => roll (650 + i))
  let next := { state with templeEnemies := state.templeEnemies.eraseIdxIfInBounds index }
  return { next.dropLoot position coins loot with
    templeEnemySound := some (enemy.kind, true), status := s!"{enemy.kind.name} defeated." }

private def GameState.damageTarget (state : GameState) (target : NpcTarget) (damage : Nat)
    (knockback : Option (Int × Float32) := none) (periodic := false) :
    GameState := Id.run do
  match target with
  | .eye => return state.hurtEye damage periodic
  | .eyeServant id => return state.hurtEyeServant id damage
  | .eaterSegment id => return state.hurtEater id damage periodic
  | .brainCore => return state.hurtBrain damage periodic
  | .brainCreeper id => return state.hurtBrainCreeper id damage periodic
  | .evilFlyer id => return state.hurtEvilFlyer id damage periodic
  | .skeletron part => return state.hurtSkeletron part damage periodic
  | .dungeonGuardian => return state.hurtDungeonGuardian damage
  | .dungeonSlime id => return state.hurtDungeonSlime id damage periodic
  | .darkCaster id => return state.hurtDarkCaster id damage knockback periodic
  | .waterSphere id =>
    return { state with waterSpheres := state.waterSpheres.filter (·.id != id), status := "Water Sphere shattered." }
  | .angryBones id => return state.hurtAngryBones id damage knockback periodic
  | .cursedSkull id => return state.hurtCursedSkull id damage knockback periodic
  | .lavaSlime id => return state.hurtLavaSlime id damage knockback periodic
  | .underworldFlyer id => return state.hurtUnderworldFlyer id damage knockback periodic
  | .fireImp id => return state.hurtFireImp id damage knockback periodic
  | .boneSerpent id part => return state.hurtBoneSerpent id part damage periodic
  | .wall part => return state.hurtWall part damage periodic
  | .wallHungry id => return state.hurtWallHungry id damage knockback periodic
  | .wallLeech id part => return state.hurtWallLeech id part damage periodic
  | .wraith id => return state.hurtWraith id damage knockback periodic
  | .hardmodeCavern id => return state.hurtHardmodeCavern id damage knockback periodic
  | .twin kind => return state.hurtTwin kind damage periodic
  | .destroyerSegment part => return state.hurtDestroyer part damage periodic
  | .destroyerProbe id => return state.hurtDestroyerProbe id damage knockback periodic
  | .primeHead => return state.hurtPrimeHead damage periodic
  | .primeArm kind => return state.hurtPrimeArm kind damage periodic
  | .planteraHead => return state.hurtPlantera damage periodic
  | .golemBody => return state.hurtGolem damage periodic
  | .golemHead => return state.hurtGolemHead damage periodic
  | .golemLeftFist => return state.hurtGolemFist .left damage periodic
  | .golemRightFist => return state.hurtGolemFist .right damage periodic
  | .templeEnemy id => return state.hurtTempleEnemy id damage knockback periodic
  | .planteraTentacle id => return state.hurtPlanteraTentacle id damage knockback periodic
  | .planteraSpore id =>
    return { state with
      planteraSpores := state.planteraSpores.filter (·.id != id)
      planteraKilledSound := true }
  | .queenSlime => return state.hurtQueenSlime damage periodic
  | .queenMinion id => return state.hurtQueenMinion id damage knockback periodic
  | .underworldCritter id =>
    let critters := state.underworldCritters.filterMap fun critter =>
      if critter.id != id then some critter else
      if damage ≥ critter.health then none else some { critter with health := critter.health - damage }
    return { state with underworldCritters := critters }
  | .burningSphere id =>
    return { state with burningSpheres := state.burningSpheres.filter (·.id != id), status := "Burning Sphere destroyed." }
  | .crawler =>
    let some enemy := state.combat.crawler | return state
    if enemy.health ≤ damage then
      return { state.creditCoins 2 with
        combat := { state.combat with crawler := none, cleared := true }
        status := "Cave crawler defeated!" }
    return { state with
      combat := { state.combat with crawler := some { enemy with health := enemy.health - damage } }
      status := "You hit the cave crawler." }
  | .warden =>
    let .fighting boss := state.warden | return state
    if boss.phase != .recover then return { state with status := "The Frost Warden's armor blocked the arrow." }
    if boss.health ≤ damage then
      return { state with warden := .reward boss.body.centerTile, status := wardenMessage .defeated }
    let boss := { boss with health := boss.health - damage }
    return { state with
      warden := .fighting { boss with ticks := min boss.ticks boss.phaseDuration }
      status := wardenMessage .hit }
  | .surface kind =>
    let flying := kind == .demonEye
    let some enemy := (if flying then state.surface.flying else state.surface.creature) | return state
    if enemy.kind != kind then return state
    let damage := if periodic then damage else enemy.kind.damageTaken damage
    let creature := if enemy.health ≤ damage then none else some { enemy with health := enemy.health - damage }
    let next := if creature.isNone then state.rewardSurfaceDefeat enemy else state
    let surface := if flying then { state.surface with flying := creature }
      else { state.surface with creature }
    return { next with
      surface
      status := surfaceMessage (if creature.isNone then .defeated enemy.kind else .hit enemy.kind) }
  | .quarry =>
    let some enemy := state.quarry.creature | return state
    let creature := if enemy.health ≤ damage then none else some { enemy with health := enemy.health - damage }
    return { state.creditCoins (if creature.isNone then 4 else 0) with
      quarry := { creature, cleared := creature.isNone }
      status := if creature.isNone then quarryMessage .defeated else quarryMessage .hit }
  | .sentry =>
    let some enemy := state.sentry.creature | return state
    let creature := if enemy.health ≤ damage then none else some { enemy with health := enemy.health - damage }
    return { state.creditCoins (if creature.isNone then 4 else 0) with
      sentry := { creature, cleared := creature.isNone }
      status := if creature.isNone then "Sentry defeated." else "You hit the sentry." }
  | .herald =>
    let (storm, event) := damageStorm state.storm damage
    return { state with
      storm
      status := (event.map stormMessage).getD "The Storm Herald deflected the arrow." }
  | .sentinel =>
    let (ruin, event) := damageRuin state.ruin damage
    return { state with
      ruin
      status := (event.map ruinMessage).getD "The Ruin Sentinel's shell deflected the arrow." }
  | .colossus =>
    let (cavern, event) := damageCavern state.cavern damage
    return { state with
      cavern
      status := (event.map cavernMessage).getD "The Colossus deflected the shot. Aim when its shell opens." }
  | .raider number kind =>
    let .active wave := state.raid | return state
    if wave.number != number then return state
    let some index := wave.foes.findIdx? (·.kind == kind) | return state
    let (raid, event) := damageRaider state.raid index damage
    return { state with raid, status := (event.map raidMessage).getD state.status }
  | .cave index =>
    let (ecology, event) := damageCaveResident state.ecology index damage
    return { state with ecology, status := (event.map caveMessage).getD state.status }

private def GameState.npcDefense (state : GameState) : NpcTarget → Nat
  | .eye => (state.eye.map (·.defense)).getD 0
  | .eaterSegment id => (state.eater.map (·.segmentDefense id)).getD 0
  | .brainCore => 14
  | .brainCreeper _ => 10
  | .evilFlyer _ => 8
  | .surface kind => kind.defense
  | .skeletron part => (state.skeletron.map (·.defense part)).getD 0
  | .dungeonGuardian => 9999
  | .dungeonSlime _ => dungeonSlimeDefense
  | .darkCaster _ => darkCasterDefense
  | .angryBones id =>
    (state.angryBones.find? (·.id == id) |>.map (·.kind.defense)).getD 0
  | .cursedSkull _ => cursedSkullDefense
  | .lavaSlime _ => lavaSlimeDefense
  | .underworldFlyer id =>
    (state.underworldFlyers.find? (·.id == id) |>.map (·.kind.defense)).getD 0
  | .fireImp _ => fireImpDefense
  | .boneSerpent _ part =>
    ((state.boneSerpent.bind (·.partAt part)).map (·.defense)).getD 0
  | .wall part => part.defense
  | .wallHungry id =>
    (state.wallHungry.find? (·.id == id) |>.map (·.defense state.wallOfFlesh)).getD 0
  | .wallLeech id part =>
    (state.wallLeeches.find? (·.id == id) |>.map (·.defense part)).getD 0
  | .wraith _ => wraithDefense
  | .hardmodeCavern id =>
    (state.hardmodeCavern.find? (·.id == id) |>.map (·.kind.defense)).getD 0
  | .twin kind =>
    ((if kind == .retinazer then state.retinazer else state.spazmatism).map (·.defense)).getD 0
  | .destroyerSegment part => destroyerDefense part
  | .destroyerProbe _ => 20
  | .primeHead => (state.skeletronPrime.map (·.defense)).getD 0
  | .primeArm kind => kind.defense
  | .planteraHead => (state.plantera.map (·.defense)).getD 0
  | .planteraTentacle _ => 20
  | .golemBody => golemBodyDefense
  | .golemHead => (state.golemHead.map (·.defense)).getD 0
  | .golemLeftFist => GolemFistKind.left.defense
  | .golemRightFist => GolemFistKind.right.defense
  | .templeEnemy id => (state.templeEnemies.find? (·.id == id) |>.map (·.kind.defense)).getD 0
  | .queenSlime => queenSlimeDefense
  | .queenMinion id =>
    (state.queenMinions.find? (·.id == id) |>.map (·.kind.defense)).getD 0
  | _ => 0

private def GameState.npcDefenseReduction (state : GameState) (target : NpcTarget) : Nat :=
  let defense := state.npcDefense target
  match target with
  | .dungeonSlime _ | .darkCaster _ | .angryBones _ | .cursedSkull _
  | .lavaSlime _ | .underworldFlyer _ | .fireImp _ | .destroyerSegment _ => (defense + 1) / 2
  | .primeHead | .primeArm _ => (defense + 1) / 2
  | _ => defense / 2

private def GameState.armorCritical (state : GameState) (target : NpcTarget) (damage chance salt : Nat) : Nat :=
  if chance == 0 || damage == 0 then damage
  else
    let roll := (hash (state.seed, state.surface.clock, salt, reprStr target, (4982 : Nat))).toNat % 100
    if roll < chance then
      let reduction := state.npcDefenseReduction target
      2 * damageAfterDefense damage reduction + reduction
    else damage

private def GameState.projectileDamage (state : GameState) (slot : Option Nat)
    (target : NpcTarget) (damage : Nat) : Nat :=
  match slot.bind (fun slot => state.weaponArmorStats.find? (·.1 == slot)) with
  | none => damage
  | some (slot, stats) =>
    state.armorCritical target (stats.scaleDamage damage) stats.criticalChance slot

private def NpcTarget.acceptsBurn (target : NpcTarget) : Bool :=
  match target with
  | .dungeonGuardian | .waterSphere _ | .burningSphere _ | .wraith _ | .destroyerSegment _ | .destroyerProbe _ => false
  | _ => true

private def NpcTarget.resistsFire : NpcTarget → Bool
  | .lavaSlime _ | .underworldFlyer _ | .fireImp _ | .boneSerpent _ _ | .underworldCritter _ => true
  | _ => false

private def GameState.addNpcBurn (state : GameState) (target : NpcTarget)
    (onFire shadowflame : Nat) (hellfire := 0) : GameState :=
  let onFire := if target.resistsFire then 0 else onFire
  let hellfire := if target.resistsFire then 0 else hellfire
  if !target.acceptsBurn || (onFire == 0 && shadowflame == 0 && hellfire == 0) then state
  else if state.npcBurns.any (·.target == target) then
    let burns := state.npcBurns.map fun burn =>
      if burn.target == target then
        { burn with
          onFire := max burn.onFire onFire, shadowflame := max burn.shadowflame shadowflame
          hellfire := max burn.hellfire hellfire }
      else burn
    { state with npcBurns := burns }
  else if state.npcBurns.size < maxNpcImmunity then
    { state with npcBurns := state.npcBurns.push { target, onFire, shadowflame, hellfire } }
  else state

private def GameState.applyMagmaStone (state : GameState) (target : NpcTarget) (projectile : Bool) : GameState :=
  if state.equipment.accessory != some .magmaStone then state
  else
    let roll := (hash (state.seed, state.surface.clock, reprStr target, (1322 : Nat))).toNat
    let duration := if projectile then
        if roll % 7 == 0 then 360 else if roll % 7 ≤ 2 then 120 else 60
      else if roll % 8 < 2 then 360 else if roll % 8 < 5 then 240 else 120
    state.addNpcBurn target 0 0 duration

private def GameState.tickNpcBurns (initial : GameState) : GameState := Id.run do
  if initial.npcBurns.isEmpty then return initial
  let mut state := initial
  let mut burns := #[]
  for burn in initial.npcBurns do
    let some (_, box) := state.projectileTargets.find? (fun entry => entry.1 == burn.target) | continue
    let wet := state.water.submerged state.world ((box.left + box.right) / 2) (box.bottom - 1)
    let onFire := if wet then 0 else burn.onFire
    let shadowflame := burn.shadowflame
    let hellfire := if wet then 0 else burn.hellfire
    let packet := if shadowflame > 0 || hellfire > 0 then 5 else 1
    let threshold := 60 * packet
    let progress := burn.progress + (if onFire > 0 then 4 else 0) +
      (if shadowflame > 0 then 15 else 0) + (if hellfire > 0 then 15 else 0)
    let damage := progress / threshold * packet
    if damage > 0 && (onFire > 0 || shadowflame > 0 || hellfire > 0) then
      state := state.damageTarget burn.target damage (periodic := true)
    if onFire > 1 || shadowflame > 1 || hellfire > 1 then
      burns := burns.push ⟨burn.target, onFire - 1, shadowflame - 1, progress % threshold, hellfire - 1⟩
  return { state with npcBurns := burns }

private def Tile.blastDrop : Tile → Option Item
  | .dirt | .corruptGrass | .crimsonGrass | .hallowedGrass => some .dirt
  | .stone => some .stone
  | .ash => some .ashBlock
  | .obsidianBrick => some .obsidianBrick
  | .demoniteBrick => some .demoniteBrick
  | .crimtaneBrick => some .crimtaneBrick
  | .ebonstone => some .ebonstone
  | .crimstone => some .crimstone
  | .timber | .treeTrunk => some .wood
  | .ice => some .ice
  | .torch => some .torch
  | .platform => some .platform
  | .copper => some .copper
  | .tinOre => some .tinOre
  | .iron => some .iron
  | .leadOre => some .leadOre
  | .silver => some .silver
  | .tungstenOre => some .tungstenOre
  | .goldOre => some .goldOre
  | .platinumOre => some .platinumOre
  | .demoniteOre => some .demoniteOre
  | .crimtaneOre => some .crimtaneOre
  | .crystal => some .crystal
  | .cobweb => some .cobweb
  | .sand => some .sand
  | .mud | .jungleGrass => some .mudBlock
  | .honeyBlock => some .honeyBlock
  | .crispyHoneyBlock => some .crispyHoneyBlock
  | .glassBlock => some .glass
  | .pearlstone => some .pearlstone
  | .pearlsand => some .pearlsand
  | .ebonsand => some .ebonsand
  | .crimsand => some .crimsand
  | .purpleIce => some .purpleIce
  | .pinkIce => some .pinkIce
  | .redIce => some .redIce
  | .crystalShard => some .crystalShard
  | .gelatinCrystal => some .gelatinCrystal
  | .placedBottle => some .bottle
  | .lihzahrdPressurePlate => some .lihzahrdPressurePlate
  | .lihzahrdDartTrap => some .superDartTrap
  | .lihzahrdFlameTrap => some .flameTrap
  | .lihzahrdSpikyBallTrap => some .spikyBallTrap
  | .lihzahrdSpearTrap => some .spearTrap
  | .woodenSpikes => some .woodenSpike
  | _ => none

private def GameState.blastCell (state : GameState) (position : TilePos) : GameState := Id.run do
  let tile := state.world.tile position
  if TempleDoor.protects state.world position then return state
  if let some (hinge, mode) := TempleDoor.atCell state.world position then
    if mode == .locked then return state
    let cleared := { state with
      world := TempleDoor.remove state.world hinge mode
      autoTempleDoor := state.autoTempleDoor.filter (· != hinge) }
    return cleared.dropLoot hinge 0 #[⟨.lihzahrdDoor, 1⟩]
  if tile == .planteraBulb then return state.breakPlanteraBulb position
  if state.hardmodeCavern.any (·.anchor == some position) then return state
  if tile.isEvilRelic then return state.breakEvilRelic position
  if tile == .clayPot then return state.breakPot position
  if tile.cuttableEvilPlant.isSome then return state.cutEvilPlant position
  if !state.golemDefeated && (TempleWiring.trapFor tile).isSome then return state
  let some item := tile.blastDrop | return state
  let top := state.world.tile (above position)
  if top.isEvilAltar || top.isTempleAltar || top.isFramedChest ||
      top == .homeBeacon || top == .doorClosed || top == .doorOpen ||
      state.furniture.any (fun object => object.supports.contains position) then return state
  if let some base := doorBase state.world (below position) then
    if base.y == position.y + 3 && (doorCells state.world base).size == 3 then return state
  let world := state.world.setTile position .air
  if !state.keepsCarts world then return state
  let next := ({ state with world } : GameState).dropLoot position 0 #[⟨item, 1⟩]
  let upper := above position
  if top == .placedBottle then
    return ({ next with world := world.setTile upper .air } : GameState).dropLoot upper 0 #[⟨.bottle, 1⟩]
  return (next.cutEvilPlant upper).updateTempleDoorSupports position |>.dropUnsupportedPressurePlate position

private def GameState.explodeBomb (state : GameState) (bomb : ActiveBomb) : GameState := Id.run do
  let mut next := { state with
    bombFlash := some (bomb.position, 12)
    status := if bomb.dynamite then "Dynamite exploded." else "Bomb exploded." }
  if next.combat.health > 0 && next.combat.invulnerable == 0 && bomb.hitsBox (HitBox.ofPlayer next.player) then
    let damage := damageAfterDefense bomb.damage ((next.defense + 1) / 2)
    next := { next with combat := next.combat.hurt damage, status := "You were caught in the blast." }
  for (target, box) in next.projectileTargets do
    if bomb.hitsBox box && !next.npcImmune target then
      let side := if bomb.x * 2 < box.left + box.right then 1 else -1
      next := (next.damageTarget target bomb.damage (some (side, if bomb.dynamite then 10 else 8))).grantNpcImmunity target
  let blast := bomb.blastTiles state.world
  let removeWalls := blast.any (fun cell => !hasWoodWall state.world state.walls cell)
  for position in blast do
    let tile := next.world.tile position
    let justPlacedLootBrick := state.wallOfFlesh.isSome && next.wallOfFlesh.isNone &&
      (tile == .demoniteBrick || tile == .crimtaneBrick) && tile != state.world.tile position
    if !justPlacedLootBrick then next := next.blastCell position
    if removeWalls && hasWoodWall next.world next.walls position then
      let index := position.y.toNat * next.world.width + position.x.toNat
      next := ({ next with walls := next.walls.set! index 0 } : GameState).dropLoot position 0 #[⟨.woodWall, 1⟩]
  for position in blast do
    for neighbor in neighbors position do
      if next.world.tile neighbor == .torch && !hasTorchSupport next.world neighbor then
        next := ({ next with world := next.world.setTile neighbor .air } : GameState).dropLoot neighbor 0 #[⟨.torch, 1⟩]
      if (next.world.tile neighbor).isHallowCrystal && !next.world.crystalShardSupport neighbor then
        next := next.blastCell neighbor
  return next

private def GameState.tickBombFlash (state : GameState) : GameState :=
  let flash := state.bombFlash.bind fun (position, ticks) =>
    if ticks > 1 then some (position, ticks - 1) else none
  { state with bombFlash := flash }

private def GameState.advanceOneBomb (state : GameState) (bomb : ActiveBomb) : GameState :=
  let moving := advanceBomb state.world bomb
  if moving.fuse == 0 then state.explodeBomb moving
  else { state with bombs := state.bombs.push moving }

private def StormEncounter.active : StormEncounter → Bool
  | .fighting _ => true
  | _ => false

private def RaidEncounter.activeNow : RaidEncounter → Bool
  | .active _ => true
  | _ => false

private def RuinEncounter.active : RuinEncounter → Bool
  | .fighting _ => true
  | _ => false

private def GameState.takeProjectileHit (state : GameState) (kind : ShotKind) : GameState :=
  let damage := damageAfterDefense kind.damage state.defense * healthPerHeart
  if state.combat.health == 0 || state.combat.invulnerable > 0 then state
  else if state.combat.health > damage then
    let combat := state.combat.hurt damage
    let status := if kind == .cavernBeam then "A beam hit you. Jump the low beam and land for the high one."
      else if kind == .ruinWave then "A shockwave hit you. Jump just before it reaches you."
      else "A projectile hit you. Use a wall as cover."
    { state with combat, status }
  else
    { state with combat := state.combat.afterDefeat, status := "You were slain by a projectile." }

private def GameState.retainsProjectile (state : GameState) (shot : Projectile) : Bool :=
  (shot.kind != .herald || state.storm.active) &&
    (shot.kind != .raidBolt || state.raid.activeNow) &&
    (shot.kind != .ruinWave || state.ruin.active) &&
    (shot.kind != .cavernBeam || state.cavern.active)

private def GameState.advancePiercingArrow (initial : GameState) (shot : Projectile) : GameState := Id.run do
  let some moved := advanceUnholyArrow initial.world shot | return initial
  let box := moved.unholyHitBox
  let mut state := initial
  let mut current := moved
  for (target, targetBox) in initial.projectileTargets do
    if !box.intersects targetBox || state.npcImmune target then continue
    let side := if shot.vx < 0 then -1 else 1
    state := state.damageTarget target (state.projectileDamage current.slot target current.damage) (some (side, 2))
    if current.hitsLeft == 1 then return state
    state := state.grantNpcImmunity target
    current := { current with hitsLeft := current.hitsLeft - 1 }
  if current.life == 0 then return state
  state := state.cutObjectsAlong shot
  return { state with projectiles := state.projectiles.push current }

private def GameState.advanceRangedProjectile (initial : GameState) (shot : Projectile) : GameState := Id.run do
  if !initial.retainsProjectile shot then return initial
  let mut state := initial
  if shot.kind.hostile then
    let (moved, hit) := advanceProjectile state.world shot #[state.player]
    if hit.isSome then state := state.takeProjectileHit shot.kind
    return { state with projectiles := state.projectiles ++ moved.toArray }
  if shot.kind == .unholyArrow then return state.advancePiercingArrow shot
  let targets := state.projectileTargets
  if shot.kind == .hellfireArrow then
    let age := shot.kind.lifetime - shot.life + 1
    let vy := if age < 20 then shot.vy else if shot.vy > 256 then shot.vy else min 256 (shot.vy + 2)
    let x := shot.x + shot.vx
    let y := shot.y + vy
    let body : HitBox := ⟨x - 80, y - 80, x + 80, y + 80⟩
    let blocked := squareProjectileBlocked state.world shot.x shot.y x y 80
    let direction := if shot.vx < 0 then -1 else 1
    let mut direct := false
    if !blocked then
      for (target, box) in targets do
        if !state.npcImmune target && body.intersects box then
          direct := true
          state := (state.damageTarget target (state.projectileDamage shot.slot target shot.damage) (some (direction, 8))).grantNpcImmunity target
    if blocked || direct || shot.life ≤ 1 then
      let (x, y) := if blocked then (shot.x, shot.y) else (x, y)
      let blast : HitBox := ⟨x - 512, y - 512, x + 512, y + 512⟩
      for (target, box) in state.projectileTargets do
        if !state.npcImmune target && blast.intersects box then
          state := (state.damageTarget target (state.projectileDamage shot.slot target shot.damage) (some (direction, 8))).grantNpcImmunity target
      return { state with underworldMagicImpact := true }
    return { state with projectiles := state.projectiles.push { shot with x, y, vy, life := shot.life - 1 } }
  let (moved, hit) := advanceProjectileBoxes state.world shot (targets.map Prod.snd)
  if let some (target, _) := hit.bind (fun index => targets[index]?) then
    let side := if shot.vx < 0 then -1 else 1
    let force : Float32 := if shot.kind == .thrownBone then 2.3 else if shot.kind.isBullet then 5 else 2
    state := state.damageTarget target (state.projectileDamage shot.slot target shot.damage) (some (side, force))
  let some moved := moved | return state
  state := state.cutObjectsAlong shot
  return { state with projectiles := state.projectiles.push moved }

private def GameState.advanceAquaStream (initial : GameState) (stream : AquaStream) : GameState := Id.run do
  let mut state := initial
  let mut active := stream
  -- Projectile.Update snapshots extraUpdates before the subupdate loop.
  for _ in [:if stream.steam then 1 else 3] do
    let some flight := active.advance state.world state.magma | return state
    let mut hitsLeft := flight.hitsLeft
    for (target, box) in state.projectileTargets do
      if !flight.hitBox.intersects box || state.npcImmune target then continue
      let side := if 2 * bodyCenterX state.player < box.left + box.right then 1 else -1
      state := state.damageTarget target (state.projectileDamage flight.slot target aquaDamage) (some (side, 7))
      if flight.steam || hitsLeft > 1 then state := state.grantNpcImmunity target
      if !flight.steam then hitsLeft := hitsLeft - 1
      if hitsLeft == 0 then return state
    if flight.age == aquaLifetime then return state
    active := { flight with hitsLeft }
  return { state with aquaStreams := state.aquaStreams.push active }

private inductive ActiveProjectile where
  | ranged (shot : Projectile) | aqua (stream : AquaStream) | bomb (bomb : ActiveBomb)
  | shortsword (thrust : ShortswordThrust)
  | flail (attack : BlueMoonAttack) (kind : FlailKind)
  | darkLance (lance : DarkLance)
  | magicMissile (attack : MagicMissileAttack)
  | flowerFire (shot : FlowerFireball)
  | hellwing (shot : HellwingShot)
  | demonSickle (shot : DemonSickle)
  | wallLaser (shot : WallLaser)
  | cavernFlame (shot : CavernFlame)
  | twinShot (shot : TwinShot)
  | primeShot (shot : PrimeShot)
  | planteraShot (shot : PlanteraShot)
  | golemShot (shot : GolemShot)
  | templeTrapShot (shot : TempleTraps.Shot)
  | queenShot (shot : QueenShot)
  | flamelash (attack : FlamelashAttack)
  | valor (attack : ValorAttack)
  | dagger (summon : DaggerSummon)

private def GameState.cutProjectileTiles (initial : GameState) (cells : Array TilePos) : GameState := Id.run do
  let mut state := initial
  for position in cells do
    state := match state.world.tile position with
      | .clayPot => state.breakPot position
      | .cobweb => ({ state with world := state.world.setTile position .air } : GameState).dropLoot
          position 0 #[⟨.cobweb, 1⟩]
      | .vileMushroom | .viciousMushroom => state.cutEvilPlant position
      | _ => state
  return state

private def GameState.advanceFlowerFire (initial : GameState) (shot : FlowerFireball) : GameState := Id.run do
  let (moved, bounced) := shot.advance initial.world initial.water initial.magma
  let state := { initial with underworldMagicImpact := initial.underworldMagicImpact || bounced }
  let some moved := moved | return state
  let box := moved.hitBox
  let state := state.cutProjectileTiles (projectileBoxCells box)
  if let some (target, _) := state.projectileTargets.find? (fun entry => box.intersects entry.2) then
    let direction := if moved.vx < 0 then -1 else 1
    let roll := (hash (state.seed, state.surface.clock, shot.slot.getD 0, (15 : Nat))).toNat
    let hit := state.damageTarget target (state.projectileDamage moved.slot target flowerFireDamage) (some (direction, 5.5))
    return if roll % 2 == 0 then hit.addNpcBurn target 300 0 else hit
  if moved.life == 0 then return state
  return { state with flowerFireballs := state.flowerFireballs.push moved }

private def GameState.advanceHellwing (initial : GameState) (shot : HellwingShot) : GameState := Id.run do
  let some moved := shot.advance initial.world initial.water initial.magma | return initial
  let box := moved.hitBox
  let mut state := initial.cutProjectileTiles (projectileBoxCells box)
  let mut flight := moved
  for (target, body) in state.projectileTargets do
    if state.npcImmune target || !box.intersects body then continue
    let direction := if moved.vx < 0 then -1 else 1
    state := state.damageTarget target (state.projectileDamage flight.slot target flight.damage) (some (direction, flight.knockback))
    if flight.unholy then
      flight := { flight with hits := flight.hits + 1 }
      if flight.hits == 5 then return state
    state := state.grantNpcImmunity target
  if flight.life == 0 then return state
  return { state with hellwingShots := state.hellwingShots.push flight }

private def GameState.advanceDemonSickle (initial : GameState) (shot : DemonSickle) : GameState := Id.run do
  let some (moved, blocked) := shot.advance initial.world initial.water initial.magma | return initial
  let box := moved.hitBox
  if shot.hostile then
    let state := if initial.combat.health > 0 && initial.combat.invulnerable == 0 &&
        box.intersects (HitBox.ofNpcTarget initial.player) then
      let roll := (hash (initial.seed, initial.surface.clock, shot.slot.getD 0, (44 : Nat))).toNat
      let (damage, immunity) := classicNpcContactDamage demonSickleContactDamage roll initial.defense
      let combat := { initial.combat.hurt damage with invulnerable := immunity }
      let player := if initial.equipment.accessory == some .cobaltShield || initial.ride.isSome then initial.player
        else moveBodyDelta initial.world initial.player (if moved.vx < 0 then -npcContactKnockbackX else npcContactKnockbackX) (-56)
      { initial with
        combat, player
        darknessTicks := if combat.health > 0 && roll / 3 % 3 == 0 then maxDarknessTicks else initial.darknessTicks
        status := if combat.health > 0 then "A Demon Sickle hit you." else "You were slain by a Demon Sickle." }
    else initial
    if blocked || moved.life == 0 then return state
    return { state with demonSickles := state.demonSickles.push moved }
  let mut state := initial.cutProjectileTiles (projectileBoxCells box)
  let mut active := moved
  for (target, body) in state.projectileTargets do
    if state.npcImmune target || !box.intersects body then continue
    let direction := if moved.vx < 0 then -1 else 1
    state := state.damageTarget target (state.projectileDamage moved.slot target demonScytheDamage) (some (direction, 5))
    active := { active with hits := active.hits + 1 }
    if active.hits == 5 then return state
    state := state.grantNpcImmunity target
  if blocked || active.life == 0 then return state
  return { state with demonSickles := state.demonSickles.push active }

private def GameState.advanceHandgunBullet (initial : GameState) (shot : Projectile) : GameState := Id.run do
  let mut state := initial
  let mut active := shot
  for _ in [:2] do
    let wet := state.water.submerged state.world active.x active.y || state.magma.submerged state.world active.x active.y
    let some moved := active.advanceHandgun state.world wet | return state
    let box := moved.handgunHitBox
    let left := box.left / tileUnits
    let top := box.top / tileUnits
    let mut cells := #[]
    for x in [:(box.right / tileUnits - left + 1).toNat] do
      for y in [:(box.bottom / tileUnits - top + 1).toNat] do
        cells := cells.push ⟨left + x, top + y⟩
    state := state.cutProjectileTiles cells
    if let some (target, _) := state.projectileTargets.find? (fun entry => box.intersects entry.2) then
      let direction := if moved.vx < 0 then -1 else 1
      return state.damageTarget target (state.projectileDamage moved.slot target moved.damage) (some (direction, 5))
    if moved.life == 0 then return state
    active := moved
  return { state with projectiles := state.projectiles.push active }

private def GameState.advanceShortsword (initial : GameState) (thrust : ShortswordThrust) : GameState := Id.run do
  if initial.combat.health == 0 then return initial
  let mut state := initial
  let mut active := thrust
  for _ in [:shortswordSubupdates] do
    let some moved := active.advance | return state
    state := state.cutProjectileTiles (moved.cutTiles state.player)
    for (target, box) in state.projectileTargets do
      if !state.npcImmune target &&
          moved.reaches state.world state.player state.combat.facing box target.ignoresMeleeTerrain then
        state := (state.damageTarget target (state.projectileDamage moved.slot target moved.damage) (some (moved.spriteDirection, 4)))
          |>.grantNpcImmunity target |>.applyMagmaStone target true
    active := moved
  return { state with shortsword := some active }

private def GameState.advanceFlail (initial : GameState) (attack : BlueMoonAttack) (kind : FlailKind)
    (held paired : Bool) : GameState := Id.run do
  if initial.combat.health == 0 then return initial
  let selected := if kind == .sunfury then Item.sunfury else Item.blueMoon
  let mirrored := paired && !initial.selectedStack.any (·.item == selected)
  let some (flail, facing) := attack.flail.advance initial.world initial.player attack.aim initial.combat.facing held
      (water := initial.water) (magma := initial.magma) (kind := kind) (mirrored := mirrored)
      (meleeSpeed := initial.equipment.meleeSpeed)
    | return initial
  let spun := attack.flail.phase == .spinning
  let released := spun && flail.phase != .spinning
  let mut cooldowns := if released then #[] else attack.cooldowns.filterMap fun (target, ticks) =>
    if ticks ≤ 1 then none else some (target, ticks - 1)
  let mut state := { initial with combat := { initial.combat with facing } }
  if flail.canDamage then
    state := state.cutProjectileTiles flail.cutTiles
    let targets := state.projectileTargets
    for index in [:targets.size] do
      let some (target, box) := targets[index]? | continue
      if state.npcImmune target || cooldowns.any (fun entry => entry.1 == target) ||
          !flail.intersects state.player box then continue
      if spun && !target.ignoresMeleeTerrain &&
          !meleeOwnerCanHit state.world state.player facing box (flail.x, flail.y) then continue
      let side := if bodyCenterX state.player * 2 < box.left + box.right then 1 else -1
      state := (state.damageTarget target (state.projectileDamage flail.slot target (flail.damage kind paired)) (some (side, flail.knockback kind)))
        |>.applyMagmaStone target true
      let roll := (hash (state.seed, state.surface.clock, flail.slot.getD 0, index, (35 : Nat))).toNat
      if kind == .sunfury && roll % 2 == 0 then state := state.addNpcBurn target 300 0
      cooldowns := cooldowns.push (target, flail.localCooldown)
  let updated := some { attack with flail, cooldowns }
  return if kind == .sunfury then { state with sunfury := updated } else { state with blueMoon := updated }

private def GameState.advanceDarkLance (initial : GameState) (lance : DarkLance) : GameState := Id.run do
  let some active := lance.advance (ownerAlive := initial.combat.health > 0) | return initial
  let mut state := initial.cutProjectileTiles (active.cutTiles initial.player)
  let targets := state.projectileTargets
  for index in [:targets.size] do
    let some (target, box) := targets[index]? | continue
    if state.npcImmune target || !active.canHit state.world state.player box
        (ignoreTerrain := target.ignoresMeleeTerrain) then continue
    let roll := (hash (state.seed, state.surface.clock, active.slot.getD 0, index, (46 : Nat))).toNat
    state := ((state.damageTarget target (state.projectileDamage active.slot target darkLanceDamage) (some (active.hitDirection, 5))).grantNpcImmunity target)
      |>.addNpcBurn target 0 (240 + roll % 60) |>.applyMagmaStone target true
  return { state with darkLance := some active, combat := { state.combat with facing := active.facing } }

def magicMissileFlashTime : Nat := 16

private def GameState.hitWithMagicMissile (state : GameState) (target : NpcTarget) (direction : Int)
    (slot : Option Nat) : GameState :=
  (state.damageTarget target (state.projectileDamage slot target magicMissileDamage) (some (direction, 7.5))).grantNpcImmunity target magicMissileHitCooldown

private def GameState.explodeMagicMissile (initial : GameState) (missile : MagicMissile)
    (direction : Int) : GameState := Id.run do
  let cut := initial.cutProjectileTiles (missile.cutTiles (blast := true))
  let mut state := { cut with magicMissileFlashes := cut.magicMissileFlashes.push (missile, magicMissileFlashTime) }
  for (target, box) in initial.projectileTargets do
    if !state.npcImmune target && missile.blastBox.intersects box then
      state := state.hitWithMagicMissile target direction missile.slot
  return state

private def GameState.advanceMagicMissile (state : GameState) (attack : MagicMissileAttack) (held : Bool) : GameState :=
  let held := held && state.combat.health > 0 && state.selectedStack.any (·.item == .magicMissile)
  let targets := state.projectileTargets
  -- Water Spheres have one life and fail CanBeChasedBy, but remain hittable.
  let chaseable := targets.filter fun (target, _) => match target with
    | .waterSphere _ | .burningSphere _ => false
    | _ => true
  let flight := attack.missile.advance state.world state.player attack.aim held chaseable attack.target
  let missile := flight.missile
  let state := { state with magicMissileSound := state.magicMissileSound || flight.loopSound }
  if flight.deactivated then state
  else if flight.hitTerrain then state.explodeMagicMissile missile flight.hitDirection
  else
    let state := state.cutProjectileTiles missile.cutTiles
    if let some (target, _) := targets.find? (fun entry => missile.hitBox.intersects entry.2) then
      -- The single-penetration contact ignores the old shared timer. Its new
      -- timer is already present when the infinite-penetration blast runs.
      (state.hitWithMagicMissile target flight.hitDirection missile.slot).explodeMagicMissile missile flight.hitDirection
    else if missile.life == 0 then state.explodeMagicMissile missile flight.hitDirection
    else { state with magicMissiles := state.magicMissiles.push { attack with missile, target := flight.target } }

private def GameState.flamelashStrike (state : GameState) (target : NpcTarget) (direction : Int)
    (slot draw : Nat) : GameState :=
  let roll := (hash (state.seed, state.surface.clock, slot, draw, (34 : Nat))).toNat
  let duration := flamelashOnFireDuration roll (roll / 2)
  let hit := state.damageTarget target (state.projectileDamage (some slot) target flamelashDamage) (some (direction, flamelashKnockback))
  if duration > 0 then hit.addNpcBurn target duration 0 else hit

private def FlamelashAttack.isImmune (attack : FlamelashAttack) (target : NpcTarget) : Bool :=
  attack.cooldowns.any (fun entry => entry.1 == target)

private def GameState.explodeFlamelash (initial : GameState) (attack : FlamelashAttack)
    (direction : Int) : GameState := Id.run do
  let projectile := attack.projectile
  let mut state := initial.cutProjectileTiles (projectile.cutTiles (blast := true))
  state := { state with
    underworldMagicImpact := true
    flamelashFlashes := state.flamelashFlashes.push (projectile, magicMissileFlashTime) }
  let targets := state.projectileTargets
  for index in [:targets.size] do
    let some (target, box) := targets[index]? | continue
    if !state.npcImmune target && !attack.isImmune target && projectile.blastBox.intersects box then
      state := state.flamelashStrike target direction (projectile.missile.slot.getD 0) (index + maxNpcImmunity)
  return state

private def GameState.advanceFlamelash (initial : GameState) (attack : FlamelashAttack)
    (held : Bool) : GameState := Id.run do
  let held := held && initial.combat.health > 0 && initial.selectedStack.any (·.item == .flamelash)
  let chaseable := initial.projectileTargets.filter fun (target, _) =>
    match target with
    | .waterSphere _ | .burningSphere _ => false
    | _ => true
  let flight := attack.projectile.advance initial.world initial.player attack.aim held chaseable
    attack.target initial.water initial.magma
  if flight.deactivated then return initial
  let cooldowns := attack.cooldowns.filterMap fun (target, ticks) =>
    if ticks > 1 then some (target, ticks - 1) else none
  let mut attack := { attack with projectile := flight.projectile, target := flight.target, cooldowns }
  if flight.hitTerrain || flight.extinguished then
    return initial.explodeFlamelash attack flight.hitDirection
  let mut state := initial.cutProjectileTiles attack.projectile.cutTiles
  let targets := state.projectileTargets
  for index in [:targets.size] do
    let some (target, box) := targets[index]? | continue
    if state.npcImmune target || attack.isImmune target || !attack.projectile.hitBox.intersects box then continue
    state := state.flamelashStrike target flight.hitDirection (attack.projectile.missile.slot.getD 0) index
    let clearTarget := attack.projectile.clearsTargetOnHit
    attack := { attack with
      projectile := attack.projectile.registerHit
      target := if clearTarget then none else attack.target
      cooldowns := attack.cooldowns.push (target, flamelashHitCooldown) }
    if attack.projectile.hitsLeft == 0 then return state.explodeFlamelash attack flight.hitDirection
  if attack.projectile.missile.life == 0 then return state.explodeFlamelash attack flight.hitDirection
  return { state with flamelashes := state.flamelashes.push attack }

private def GameState.advanceValor (initial : GameState) (attack : ValorAttack) (held : Bool) : GameState := Id.run do
  if initial.combat.health == 0 then return initial
  let held := held && initial.selectedStack.any (·.item == .valor)
  let factor := 4 / (1 + 3 / initial.equipment.meleeSpeed)
  let stats := { valorYoyoStats with
    maxRange := valorYoyoStats.maxRange * factor, topSpeed := valorYoyoStats.topSpeed * factor }
  let some (moved, facing) := attack.yoyo.advance initial.world initial.player attack.aim initial.combat.facing held
      (water := initial.water) (magma := initial.magma) (stats := stats) | return initial
  let mut state := { initial with combat := { initial.combat with facing } }
  state := state.cutProjectileTiles moved.cutTiles
  let mut yoyo := moved
  for (target, box) in state.projectileTargets do
    if state.npcImmune target || !moved.hitBox.intersects box then continue
    let (recoiled, damage, direction) := yoyo.strike state.world state.player box
    state := (state.damageTarget target (state.projectileDamage yoyo.slot target damage) (some (direction, valorYoyoKnockback)))
      |>.grantNpcImmunity target |>.applyMagmaStone target true
    yoyo := recoiled
  return { state with valor := some { attack with yoyo } }

private def GameState.advanceWallLaser (initial : GameState) (laser : WallLaser) : GameState := Id.run do
  let mut state := initial
  let mut active := laser
  for update in [:wallLaserUpdates] do
    let some (moved, blocked) := active.advanceSubstep state.world state.water state.magma | return state
    if blocked then return state
    active := moved
    if state.combat.health > 0 && state.combat.invulnerable == 0 &&
        moved.hitBox.intersects (HitBox.ofNpcTarget state.player) then
      let roll := (hash (state.seed, state.surface.clock, laser.slot.getD 0, update, (83 : Nat))).toNat
      let (damage, immunity) := classicNpcContactDamage (moved.damage / 2) roll state.defense (afterVariance := 2)
      let combat := { state.combat.hurt damage with invulnerable := immunity }
      let player := if state.tongued || state.equipment.accessory == some .cobaltShield || state.ride.isSome then state.player
        else
          let push := if bodyCenterX state.player < (moved.x * 16).toInt32.toInt then -npcContactKnockbackX else npcContactKnockbackX
          moveBodyDelta state.world state.player push (-56)
      state := { state with combat, player, status := "A Wall of Flesh laser hit you." }
    if active.life == 0 then return state
  return { state with wallLasers := state.wallLasers.push active }

private def GameState.advanceCavernFlame (state : GameState) (shot : CavernFlame) : GameState := Id.run do
  let some moved := shot.advance state.world state.water state.magma | return state
  let state := if moved.life > 0 then { state with cavernFlames := state.cavernFlames.push moved } else state
  if state.combat.health == 0 || state.combat.invulnerable > 0 ||
      !moved.hitBox.intersects (HitBox.ofNpcTarget state.player) then
    return state
  let roll (i : Nat) := (hash (state.seed, state.surface.clock, shot.slot.getD 0, i, (96 : Nat))).toNat
  let (damage, immunity) := classicNpcContactDamage cavernFlameDamage (roll 0) state.defense (afterVariance := 2)
  let combat := { state.combat.hurt damage with invulnerable := immunity }
  let player := if state.tongued || state.equipment.accessory == some .cobaltShield || state.ride.isSome then state.player
    else
      let push := if bodyCenterX state.player < (moved.x * 16).toInt32.toInt then -npcContactKnockbackX
        else npcContactKnockbackX
      moveBodyDelta state.world state.player push (-56)
  return { state with
    combat, player
    cursedInfernoTicks := max state.cursedInfernoTicks (CavernFlame.cursedInfernoTicks fun i => roll (i + 1))
    status := "A Clinger's Cursed Flame hit you." }

private def GameState.advanceTwinShot (initial : GameState) (shot : TwinShot) : GameState := Id.run do
  let mut state := initial
  let mut active := shot
  for update in [:shot.kind.updates] do
    let some moved := active.advanceSubstep state.world state.water state.magma | return state
    active := moved
    if state.combat.health > 0 && state.combat.invulnerable == 0 &&
        moved.hitBox.intersects (HitBox.ofNpcTarget state.player) then
      let roll (draw : Nat) :=
        (hash (state.seed, state.surface.clock, shot.slot.getD 0, shot.kind.sprite, update, draw)).toNat
      let (damage, immunity) := classicNpcContactDamage shot.damage (roll 0) state.defense (afterVariance := 2)
      let combat := { state.combat.hurt damage with invulnerable := immunity }
      let player := if state.tongued || state.equipment.accessory == some .cobaltShield || state.ride.isSome then state.player
        else
          let push := if bodyCenterX state.player < (moved.x * 16).toInt32.toInt then -npcContactKnockbackX
            else npcContactKnockbackX
          moveBodyDelta state.world state.player push (-56)
      let curse := if shot.kind.isLaser then 0 else CavernFlame.cursedInfernoTicks fun draw => roll (draw + 1)
      state := { state with
        combat, player, cursedInfernoTicks := max state.cursedInfernoTicks curse
        status := match shot.kind with
        | .destroyerLaser => "The Destroyer's Death Laser hit you."
        | .probeLaser => "A Probe's Pink Laser hit you."
        | .eyeLaser | .deathLaser => "Retinazer's laser hit you."
        | _ => "Spazmatism's flames hit you." }
    if active.life == 0 then return state
  return { state with twinShots := state.twinShots.push active }

private def GameState.advancePrimeShot (initial : GameState) (shot : PrimeShot) : GameState := Id.run do
  let mut state := initial
  let mut active := shot
  for update in [:shot.kind.updates] do
    let result := active.advanceSubstep state.world state.water state.magma state.honey
    state := { state with
      primeBombLit := state.primeBombLit || (shot.kind == .bomb && result.launchSound)
      primeBombExploded := state.primeBombExploded || result.detonated
      primeLaserFired := state.primeLaserFired || (shot.kind == .laser && result.launchSound)
      primeLaserImpact := state.primeLaserImpact || result.impactSound }
    if state.combat.health > 0 && state.combat.invulnerable == 0 &&
        result.hit.any (·.intersects (HitBox.ofNpcTarget state.player)) then
      let roll := (hash (state.seed, state.surface.clock, shot.slot.getD 0, shot.kind.sprite, update)).toNat
      let (damage, immunity) := classicNpcContactDamage result.damage roll state.defense (afterVariance := 2)
      let combat := { state.combat.hurt damage with invulnerable := immunity }
      let player := if state.tongued || state.equipment.accessory == some .cobaltShield || state.ride.isSome then
          state.player
        else
          let fromX := ((result.next.getD active).centerX * 16).toInt32.toInt
          let push := if bodyCenterX state.player < fromX then -npcContactKnockbackX else npcContactKnockbackX
          moveBodyDelta state.world state.player push (-56)
      state := { state with
        combat, player
        status := if result.detonated then "A Prime Cannon bomb exploded near you." else "Prime Laser hit you." }
    let some moved := result.next | return state
    active := moved
  return { state with primeShots := state.primeShots.push active }

private def GameState.advancePlanteraShot (state : GameState) (shot : PlanteraShot) : GameState := Id.run do
  let result := shot.advanceSubstep state.world state.water state.magma state.honey
  let state := { state with
    planteraShots := state.planteraShots ++ result.next.toArray
    planteraShotSound := state.planteraShotSound || result.launchSound
    planteraShotImpact := state.planteraShotImpact || result.impactSound }
  if state.combat.health == 0 || state.combat.invulnerable > 0 ||
      !result.hit.any (·.intersects (HitBox.ofNpcTarget state.player)) then return state
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, shot.slot.getD 0, shot.kind.sprite, draw)).toNat
  let (damage, immunity) := classicNpcContactDamage result.damage (roll 0) state.defense (afterVariance := 2)
  let combat := { state.combat.hurt damage with invulnerable := immunity }
  let player := if state.equipment.accessory == some .cobaltShield || state.ride.isSome || state.tongued then
      state.player
    else
      let source := ((result.next.getD shot).centerX * 16).toInt32.toInt
      let push := if bodyCenterX state.player < source then -npcContactKnockbackX else npcContactKnockbackX
      moveBodyDelta state.world state.player push (-56)
  let poisoned := shot.kind.poisonTicks (fun draw => roll (draw + 1))
  return { state with
    combat, player
    poisonedTicks := max state.poisonedTicks poisoned
    status := if poisoned > 0 then "Plantera's seed poisoned you." else "Plantera's projectile hit you." }

private def GameState.hurtByTempleShot (state : GameState) (x : Float32)
    (base roll : Nat) (message : String) : GameState :=
  let (damage, immunity) := classicNpcContactDamage base roll state.defense (afterVariance := 2)
  let combat := { state.combat.hurt damage with invulnerable := immunity }
  let player := if state.equipment.accessory == some .cobaltShield || state.ride.isSome || state.tongued then
      state.player
    else
      let source := (x * 16).toInt32.toInt
      let push := if bodyCenterX state.player < source then -npcContactKnockbackX else npcContactKnockbackX
      moveBodyDelta state.world state.player push (-56)
  { state with combat, player, status := message }

private def GameState.advanceGolemShot (initial : GameState) (shot : GolemShot) : GameState := Id.run do
  let mut state := initial
  let mut active := shot
  for update in [:shot.kind.updates] do
    let result := active.advanceSubstep state.world state.water state.magma state.honey
    state := { state with
      golemFireballSound := state.golemFireballSound || result.sound == some 20
      golemBeamSound := state.golemBeamSound || result.sound == some 33
      golemImpactSound := state.golemImpactSound || result.impact }
    if state.combat.health > 0 && state.combat.invulnerable == 0 &&
        result.hit.any (·.intersects (HitBox.ofNpcTarget state.player)) then
      let roll (draw : Nat) := (hash (state.seed, state.surface.clock, shot.slot.getD 0, update, draw)).toNat
      let struck := state.hurtByTempleShot (result.shot.getD active).centerX active.damage (roll 0)
        (if shot.kind == .fireball then "Golem's fireball hit you." else "Golem's Eye Beam hit you.")
      let fire := if struck.obsidianSkinTicks > 0 then 0 else shot.kind.burnTicks (fun draw => roll (draw + 1))
      state := { struck with combat := { struck.combat with onFire := max struck.combat.onFire fire } }
    let some moved := result.shot | return state
    active := moved
  return { state with golemShots := state.golemShots.push active }

private def GameState.advanceTempleTrapShot (initial : GameState)
    (shot : TempleTraps.Shot) : GameState × Option TempleTraps.Shot := Id.run do
  let terrain := TempleTraps.Terrain.ofWorld initial.world
  let mut state := initial
  let mut active := shot
  let mut child := none
  for update in [:shot.kind.updates] do
    let result := active.step terrain
    child := child.orElse (fun _ => result.child)
    state := { state with
      templeDartSound := state.templeDartSound || result.sound == some 17
      templeFlameSound := state.templeFlameSound || result.sound == some 34 }
    if let some box := result.hit then
      for (target, targetBox) in state.projectileTargets do
        if !box.intersects targetBox || state.npcImmune target then continue
        state := (state.damageTarget target active.damage).grantNpcImmunity target
      if state.combat.health > 0 && state.combat.invulnerable == 0 &&
          box.intersects (HitBox.ofNpcTarget state.player) then
        let roll := (hash (state.seed, state.surface.clock, shot.slot.getD 0, update, (137 : Nat))).toNat
        state := state.hurtByTempleShot (result.next.getD active).center.x active.damage roll
          (match shot.kind with
            | .dart => "A Super Dart Trap hit you."
            | .flame => "A Flame Trap hit you."
            | .spear => "A Spear Trap hit you."
            | .spikyBall => "A Spiky Ball Trap hit you."
            | .flameEmitter => "A Flame Trap fired.")
        if let some (buff, duration) := shot.kind.status then
          state := if buff == 20 then { state with poisonedTicks := max state.poisonedTicks duration }
            else { state with hellfireTicks := max state.hellfireTicks duration }
    let some moved := result.next | return (state, child)
    active := moved
  return ({ state with templeTrapShots := state.templeTrapShots.push active }, child)

private def GameState.advanceQueenShot (state : GameState) (shot : QueenShot) : GameState := Id.run do
  let some moved := shot.advance state.world state.water state.magma state.honey | return state
  let hit := state.combat.health > 0 && state.combat.invulnerable == 0 &&
    moved.hitBox.intersects (HitBox.ofNpcTarget state.player)
  let moved := if hit && shot.kind != .stomp then { moved with bounced := moved.bounced + 1 } else moved
  let state := if moved.age < shot.kind.lifetime && moved.bounced ≤ shot.kind.bounces then
      { state with queenShots := state.queenShots.push moved }
    else state
  if !hit then return state
  let roll := (hash (state.seed, state.surface.clock, shot.slot.getD 0, shot.kind.sprite)).toNat
  let (damage, immunity) := classicNpcContactDamage shot.kind.damage roll state.defense (afterVariance := 2)
  let combat := { state.combat.hurt damage with invulnerable := immunity }
  let player := if state.tongued || state.equipment.accessory == some .cobaltShield || state.ride.isSome then state.player
    else
      let push := if bodyCenterX state.player < (moved.x * 16).toInt32.toInt then -npcContactKnockbackX
        else npcContactKnockbackX
      moveBodyDelta state.world state.player push (-56)
  return { state with
    combat, player
    status := if shot.kind == .stomp then "Queen Slime's smash hit you." else "A slime projectile hit you." }

private def HitBox.center (box : HitBox) : Int × Int :=
  ((box.left + box.right) / 2, (box.top + box.bottom) / 2)

private def distanceSquared (ax ay bx targetY : Int) : Int :=
  let dx := ax - bx
  let dy := ay - targetY
  dx * dx + dy * dy

private def GameState.findDaggerTarget (state : GameState) (blade : EnchantedDagger)
    (targets : Array (NpcTarget × HitBox)) : Option NpcTarget := Id.run do
  let (x, y) := blade.pose.position
  let boost := if state.wingedSlime.isSome then wingedSlimeHeightBoost else 0
  let ownerX := bodyCenterX state.player
  let ownerY := bodyCenterY state.player - boost / 2
  let mut closest := none
  let mut best := bladeStaffAcquireRange * bladeStaffAcquireRange
  for (target, box) in targets do
    let preferred := state.minionTarget == some target
    let boss := match target with
      | .eye | .eaterSegment _ | .brainCore | .skeletron .head | .wall _ | .twin _ | .queenSlime
      | .destroyerSegment _ | .primeHead | .planteraHead | .golemBody | .golemHead => true
      | _ => false
    if !preferred && !boss && !(box.intersects ⟨ownerX - 960 * 16, ownerY - 600 * 16,
        ownerX + 960 * 16, ownerY + 600 * 16⟩) then continue
    let (tx, ty) := box.center
    let ownDistance := distanceSquared x y tx ty
    let playerDistance := distanceSquared ownerX ownerY tx ty
    let limit := if preferred then bladeStaffAcquireRange * bladeStaffAcquireRange else best
    let ownSight := ownDistance ≤ limit && terrainLineClear state.world x y tx ty
    let ownerSight := playerDistance ≤ limit && ownDistance ≤ 1000 * 16 * (1000 * 16) &&
      terrainLineClear state.world ownerX ownerY tx ty
    if !ownSight && !ownerSight then continue
    if preferred then return some target
    closest := some target
    best := if ownSight && ownerSight then min ownDistance playerDistance
      else if ownSight then ownDistance else playerDistance
  return closest

private def GameState.advanceDagger (state : GameState) (summon : DaggerSummon) : GameState := Id.run do
  if state.combat.health == 0 then return state
  let targets : Array (NpcTarget × HitBox) := state.daggerTargets
  let ownerX := bodyCenterX state.player
  let boost := if state.wingedSlime.isSome then wingedSlimeHeightBoost else 0
  let ownerY := bodyCenterY state.player - boost / 2
  let (blade, phase) := match summon.phase with
    | .idle =>
      let next := summon.blade.follow ownerX (state.player.y - boost)
      let target := state.findDaggerTarget summon.blade targets
      (next, (target.map DaggerPhase.chase).getD .idle)
    | .chase target =>
      match targets.find? (fun (entry : NpcTarget × HitBox) => entry.1 == target) with
      | none => (summon.blade.coast, .recoil 0)
      | some (_, box) =>
        let (x, y) := box.center
        if distanceSquared ownerX ownerY x y ≥ bladeStaffReturnRange * bladeStaffReturnRange then
          (summon.blade.coast, .idle)
        else (summon.blade.chase x y, .chase target)
    | .recoil elapsed =>
      let roll := hash (state.seed, state.surface.clock, summon.slot, (864 : Nat))
      let angle := (Int32.ofNat (roll.toNat % 16777216)).toFloat32 * (6.2831854820251465 / 16777216)
      (summon.blade.recoil (if elapsed == 0 then some angle else none),
        if elapsed + 1 == bladeStaffRecovery then .idle else .recoil (elapsed + 1))
  if !blade.valid state.world then return state
  let mut phase := phase
  let mut cooldowns := summon.cooldowns.filterMap fun (target, ticks) =>
    if ticks > 1 && targets.any (·.1 == target) then some (target, ticks - 1) else none
  let mut state := state
  for (target, box) in targets do
    if !blade.hitBox.intersects box || cooldowns.any (·.1 == target) then continue
    let roll := (hash (state.seed, state.surface.clock, summon.slot, reprStr target)).toNat
    let dealt := bladeStaffHitDamage state.equipment.summonDamageBonus (state.npcDefense target) roll
    -- Pass the damage through the target's normal hit path, including AI reactions.
    state := state.damageTarget target (dealt + state.npcDefenseReduction target)
    state := { state with npcImmunity := state.npcImmunity.filter (·.1 != target) }
    cooldowns := cooldowns.push (target, bladeStaffLocalImmunity)
    if let .chase _ := phase then phase := .recoil 0
  return { state with dagger := some { summon with blade, phase, cooldowns } }

private def GameState.pruneDaggerTargets (state : GameState) : GameState :=
  if state.minionTarget.isNone && state.dagger.all (·.cooldowns.isEmpty) then state
  else
    let targets := state.daggerTargets.map Prod.fst
    { state with
      dagger := state.dagger.map fun dagger => { dagger with cooldowns := dagger.cooldowns.filter (targets.contains ·.1) }
      minionTarget := state.minionTarget.filter (targets.contains ·) }

private def GameState.advanceProjectiles (initial : GameState) (held := false) : GameState := Id.run do
  let initial := initial.assignProjectileSlots
  let order : Array (Nat × ActiveProjectile) :=
    initial.projectiles.map (fun shot => (shot.slot.getD maxProjectileSlots, .ranged shot)) ++
    initial.aquaStreams.map (fun stream => (stream.slot.getD maxProjectileSlots, .aqua stream)) ++
    initial.bombs.map (fun bomb => (bomb.slot.getD maxProjectileSlots, .bomb bomb)) ++
    (initial.shortsword.map fun thrust => (thrust.slot.getD maxProjectileSlots, .shortsword thrust)).toArray ++
    (initial.blueMoon.map fun attack => (attack.flail.slot.getD maxProjectileSlots, .flail attack .blueMoon)).toArray ++
    (initial.sunfury.map fun attack => (attack.flail.slot.getD maxProjectileSlots, .flail attack .sunfury)).toArray ++
    (initial.darkLance.map fun lance => (lance.slot.getD maxProjectileSlots, .darkLance lance)).toArray ++
    initial.magicMissiles.map (fun attack => (attack.missile.slot.getD maxProjectileSlots, .magicMissile attack)) ++
    initial.flowerFireballs.map (fun shot => (shot.slot.getD maxProjectileSlots, .flowerFire shot)) ++
    initial.hellwingShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .hellwing shot)) ++
    initial.demonSickles.map (fun shot => (shot.slot.getD maxProjectileSlots, .demonSickle shot)) ++
    initial.wallLasers.map (fun shot => (shot.slot.getD maxProjectileSlots, .wallLaser shot)) ++
    initial.cavernFlames.map (fun shot => (shot.slot.getD maxProjectileSlots, .cavernFlame shot)) ++
    initial.twinShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .twinShot shot)) ++
    initial.primeShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .primeShot shot)) ++
    initial.planteraShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .planteraShot shot)) ++
    initial.golemShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .golemShot shot)) ++
    initial.templeTrapShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .templeTrapShot shot)) ++
    initial.queenShots.map (fun shot => (shot.slot.getD maxProjectileSlots, .queenShot shot)) ++
    initial.flamelashes.map (fun attack => (attack.projectile.missile.slot.getD maxProjectileSlots, .flamelash attack)) ++
    (initial.valor.map fun attack => (attack.yoyo.slot.getD maxProjectileSlots, .valor attack)).toArray ++
    (initial.dagger.map fun summon => (summon.slot, .dagger summon)).toArray
  let mut state := { initial.tickBombFlash with
    projectiles := #[], aquaStreams := #[], bombs := #[], shortsword := none, blueMoon := none, magicMissiles := #[]
    valor := none, sunfury := none, darkLance := none
    flowerFireballs := #[], hellwingShots := #[], flamelashes := #[], demonSickles := #[], wallLasers := #[]
    cavernFlames := #[], twinShots := #[], primeShots := #[], planteraShots := #[], queenShots := #[], dagger := none
    golemShots := #[], templeTrapShots := #[]
    flamelashFlashes := initial.flamelashFlashes.filterMap fun (projectile, ticks) =>
      if ticks > 1 then some (projectile, ticks - 1) else none
    magicMissileFlashes := initial.magicMissileFlashes.filterMap fun (missile, ticks) =>
      if ticks > 1 then some (missile, ticks - 1) else none }
  let mut order := order.qsort (fun a b => a.1 < b.1)
  let mut index := 0
  while index < order.size do
    let some (slot, entry) := order[index]? | break
    index := index + 1
    if let .templeTrapShot shot := entry then
      let (after, child) := state.advanceTempleTrapShot shot
      state := after
      if let some child := child then
        let remaining := order.extract index
        let used := (state.projectileSlots.push slot) ++ remaining.map Prod.fst
        let childSlot := firstFreeProjectileSlot used
        if childSlot < maxProjectileSlots then
          let child := { child with slot := some childSlot }
          if childSlot < slot then
            state := { state with templeTrapShots := state.templeTrapShots.push child }
          else
            order := order.extract 0 index ++
              (remaining.push (childSlot, .templeTrapShot child)).qsort (fun a b => a.1 < b.1)
      continue
    state := match entry with
      | .ranged shot => if shot.kind == .handgunBullet then state.advanceHandgunBullet shot
        else state.advanceRangedProjectile shot
      | .aqua stream => state.advanceAquaStream stream
      | .bomb bomb => state.advanceOneBomb bomb
      | .shortsword thrust => state.advanceShortsword thrust
      | .flail attack kind => state.advanceFlail attack kind held (initial.blueMoon.isSome && initial.sunfury.isSome)
      | .darkLance lance => state.advanceDarkLance lance
      | .magicMissile attack => state.advanceMagicMissile attack held
      | .flowerFire shot => state.advanceFlowerFire shot
      | .hellwing shot => state.advanceHellwing shot
      | .demonSickle shot => state.advanceDemonSickle shot
      | .wallLaser shot => state.advanceWallLaser shot
      | .cavernFlame shot => state.advanceCavernFlame shot
      | .twinShot shot => state.advanceTwinShot shot
      | .primeShot shot => state.advancePrimeShot shot
      | .planteraShot shot => state.advancePlanteraShot shot
      | .golemShot shot => state.advanceGolemShot shot
      | .templeTrapShot _ => state
      | .queenShot shot => state.advanceQueenShot shot
      | .flamelash attack => state.advanceFlamelash attack held
      | .valor attack => state.advanceValor attack held
      | .dagger summon => state.advanceDagger summon
  return ({ state with projectiles := state.projectiles.filter state.retainsProjectile } : GameState).pruneWeaponArmor

private def GameState.updateBreath (state : GameState) : GameState × Bool :=
  let x := state.player.x + playerWidth / 2
  let y := state.player.y + playerHeight / 6
  let submerged := state.water.submerged state.world x y || state.honey.submerged state.world x y
  let breath := if submerged then
      if state.gills > 0 then state.breath else state.breath - 1
    else min maxBreath (state.breath + 8)
  if state.combat.health == 0 || !submerged || state.gills > 0 || breath > 0 ||
      state.combat.invulnerable > 0 then
    ({ state with breath }, false)
  else if state.combat.health > healthPerHeart then
    let combat := state.combat.hurt healthPerHeart
    ({ state with breath, combat, status := "Out of air. Hold Space to swim up." }, false)
  else
    ({ state with breath, combat := state.combat.afterDefeat, status := "You drowned." }, true)

private def GameState.updateMagmaDamage (state : GameState) : GameState × Bool :=
  if state.combat.health == 0 || state.obsidianSkinTicks > 0 || state.combat.lavaCooldown > 0 ||
      !Magma.touches state.magma state.world state.player then (state, false)
  else
    let rose := state.equipment.accessory == some .obsidianRose
    let baseDamage := if rose then 35 else Magma.contactDamage
    let damage := max 1 (baseDamage - (state.defense + 1) / 2)
    if state.combat.health > damage then
      let combat := { state.combat with
        health := state.combat.health - damage
        regenTime := 0
        lavaCooldown := if damage == 1 then 20 else lavaHurtCooldown
        onFire := max state.combat.onFire (if rose then lavaBurnTicks / 2 else lavaBurnTicks) }
      ({ state with combat, status := "On Fire! Water puts out the flames." }, false)
    else
      ({ state with combat := state.combat.afterDefeat, status := "You melted in lava." }, true)

private def GameState.touchesHellstone (state : GameState) : Bool := Id.run do
  if state.equipment.accessory == some .obsidianSkull || state.obsidianSkinTicks > 0 ||
      state.world.height != underworldHeight then
    return false
  let box := HitBox.ofPlayer state.player
  let tolerance := tileUnits / 32
  let left := (box.left - 1) / tileUnits
  let top := (box.top - tolerance - 1) / tileUnits
  let right := box.right / tileUnits
  let bottom := (box.bottom + tolerance) / tileUnits
  for y in [:(bottom - top + 1).toNat] do
    for x in [:(right - left + 1).toNat] do
      let position : TilePos := ⟨left + x, top + y⟩
      if state.world.tile position == .hellstone then return true
  return false

private def GameState.updateBurning (state : GameState) : GameState :=
  let duration := if state.touchesHellstone then 20 else state.burningTicks - 1
  if state.combat.health == 0 || state.burningTicks == 0 then
    { state with burningTicks := if state.combat.health > 0 then duration else 0, hotTileTicks := 0 }
  else
    let ticks := state.hotTileTicks + 1
    let damage := if ticks == 10 then 5 else 0
    let combat := { state.combat with
      health := state.combat.health - damage, regenTime := 0, regenProgress := 0 }
    if combat.health == 0 then
      { state with
        combat := combat.afterDefeat, burningTicks := 0, hotTileTicks := 0
        status := "You burned to death." }
    else
      { state with
        combat, burningTicks := duration, hotTileTicks := if duration == 0 then 0 else ticks % 10 }

private def evaporateUnderworldWater (world : World) (water : Water) : Water := Id.run do
  let mut levels := water.levels
  for index in [world.width * (underworldStart + 1):levels.size] do
    let level := levels[index]!.toNat
    if level > 0 then levels := levels.set! index (level - 2).toUInt8
  return { levels }

private def GameState.advanceLiquids (state : GameState) : GameState × Bool := Id.run do
  let clock := state.surface.clock
  if clock % 4 != 0 then return (state, false)
  let hasUnderworld := state.world.height == underworldHeight
  if !hasUnderworld && state.world.tile magmaMarker != .magmaMarker && state.honey.levels.isEmpty then
    return ({ state with water := state.water.advance state.world (clock % 8 == 0) }, false)
  let water := if hasUnderworld then evaporateUnderworldWater state.world state.water else state.water
  let water := water.advance state.world (clock % 8 == 0) (Water.liquidBarrier state.magma state.honey)
  let magma := if clock % 12 == 0 then
      state.magma.advance state.world (clock % 24 == 0) (Water.liquidBarrier water state.honey)
    else state.magma
  let honey := if clock % 16 == 0 && !state.honey.levels.isEmpty then
      state.honey.advance state.world (clock % 32 == 0) (Water.liquidBarrier water magma)
    else state.honey
  let terrain := Magma.burnPlatforms state.world state.magma magma
  let mut terrain := terrain
  let mut furniture := #[]
  for object in state.furniture do
    let wooden := object.kind == .bed || object.kind == .sawmill || object.kind == .loom
    let burns := wooden && object.cells.any (fun cell =>
      state.magma.level terrain cell > 0 || magma.level terrain cell > 0)
    if !object.intact terrain || burns then
      terrain := object.cells.foldl (fun terrain cell => terrain.setTile cell .air) terrain
    else
      furniture := furniture.push object
  if terrain.tiles != state.world.tiles then
    for index in [:terrain.tiles.size] do
      if terrain.tiles[index]! != Tile.placedBottle.code then continue
      let cell : TilePos := ⟨index % terrain.width, index / terrain.width⟩
      if !(terrain.tile (below cell)).bottleSupport then terrain := terrain.setTile cell .air
  let canSolidify := fun terrain position =>
    !state.occupied position &&
      !state.projectiles.any (fun shot => position == ⟨shot.x / tileUnits, shot.y / tileUnits⟩) &&
      state.keepsCarts terrain
  let (mixed, water, magma, honey) := Magma.coolHoney terrain water magma honey canSolidify
  let (world, water, magma, formed) := Magma.cool mixed water magma canSolidify
  let status := if formed.any (fun position => state.tileDistance position ≤ 12) then
      if state.legacyEarly then "Water cooled the lava into Obsidian. Mine it with a crystal pick."
      else "Water cooled the lava into Obsidian. Use a Gold or Platinum Pickaxe."
    else state.status
  let honey := if honey.levels != state.honey.levels && !honey.levels.data.any (· != 0) then default else honey
  let updated := { state with world, water, magma, honey, furniture, status }
  let changed := world.tiles != state.world.tiles
  return (if changed then updated.refreshHome.refreshMerchant else updated, changed)

private def GameState.collectDrop (state : GameState) (position : TilePos) (item : Item)
    (message : String) : GameState × Bool := Id.run do
  let center := state.player.centerTile
  if (center.x - position.x).natAbs + (center.y - position.y).natAbs > 1 then return (state, false)
  let some (inventory, index) := Inventory.addOne state.inventory item
    | return ({ state with status := s!"Make room in your inventory to collect {item.name}." }, false)
  return (state.equipCrafted inventory index item message, true)

private def GameState.collectCaveLoot (state : GameState) (before : CaveEcology) : GameState := Id.run do
  let mut next := state
  for index in [:before.sites.size] do
    let some (.loot position) := before.sites[index]? | continue
    if next.tileDistance position > 1 then continue
    let some habitat := before.habitats[index]? | continue
    let item := habitat.kind.dropItem
    let some (inventory, _) := Inventory.addOne next.inventory item
      | return { next with status := s!"Make room to pick up {item.name}." }
    next := { next with
      inventory
      ecology := { next.ecology with sites := next.ecology.sites.set! index (.waiting caveRespawnTicks) }
      status := s!"Collected {item.name}." }
  return next

private def GameState.restoreHealth (state : GameState) (amount : Nat) (message : String) : GameState :=
  if state.combat.health == state.combat.healthLimit then
    { state with status := "Your health is already full." }
  else
    { state.consumeSelected with
      combat := { state.combat with health := min state.combat.healthLimit (state.combat.health + amount) }
      status := message }

private def GameState.drinkHealingPotion (state : GameState) (item : Item) : GameState :=
  if state.potionSickness > 0 then { state with status := "Potion Sickness prevents healing potions." }
  else if state.combat.health == state.combat.healthLimit then { state with status := "Your health is full." }
  else
    let amount := if item == .greaterHealingPotion then 150 else if item == .restorationPotion then 90
      else if item == .honeyfin then 120 else if item == .healingPotion then 100 else 50
    let healed := state.restoreHealth amount s!"{item.name} restored up to {amount} HP."
    { healed with potionSickness := if item == .restorationPotion then 2700 else potionSicknessTicks }

private def GameState.handleOldMan (state : GameState) (input : TickInput) : GameState := Id.run do
  let some position := state.oldManPosition | return { state with openOldMan := false }
  if input.closeOldMan || !state.canReach position then return { state with openOldMan := false }
  if !input.curseOldMan then return state
  if !isNight state.surface.clock false then return { state with status := "Return at night to lift the curse." }
  if state.skeletron.isSome then return { state with status := "Skeletron is already here." }
  return { state with
    skeletron := some (spawnSkeletron position (some state.player))
    openOldMan := false, oldManWaiting := false
    status := "Skeletron has awoken!" }

private def GameState.summonEye (state : GameState) : GameState :=
  if state.eye.isSome then { state with status := "The Eye of Cthulhu is already present." }
  else if !state.canSummonEye then { state with status := "Use the Suspicious Looking Eye at night." }
  else
    { state.consumeSelected with
      eye := some (spawnEye state.world state.player)
      status := "Eye of Cthulhu has awoken!" }

private def GameState.summonQueenSlime (state : GameState) : GameState :=
  if state.queenSlime.isSome then { state with status := "Queen Slime is already present." }
  else if state.bowCooldown > 0 then state
  else if !state.hardmode || !(state.world.hardmodeZones state.player.centerTile).1 then
    { state with status := "Use the Gelatin Crystal in the Hallow." }
  else
    let roll := (hash (state.seed, state.surface.clock, state.player.centerTile, (657 : Nat))).toNat
    { state.consumeSelected with
      queenSlime := some (QueenSlime.spawn state.world state.water state.magma state.player roll)
      bowCooldown := 45
      status := "Queen Slime has awoken!" }

private def GameState.spawnTwins (state : GameState) : GameState :=
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, state.player.centerTile, (544 : Nat), draw)).toNat
  let (retinazer, spazmatism) := Twin.spawnPair state.world state.player roll
  { state with
    retinazer := some retinazer, spazmatism := some spazmatism, twinsRoared := true
    status := "The Twins have awoken!" }

private def GameState.summonTwins (state : GameState) : GameState :=
  if state.twinsActive then { state with status := "The Twins are already present." }
  else if !state.canSummonTwins then { state with status := "Use the Mechanical Eye at night." }
  else state.consumeSelected.spawnTwins

private def GameState.spawnDestroyer (state : GameState) : GameState :=
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, state.player.centerTile, (556 : Nat), draw)).toNat
  { state with
    destroyer := some (Destroyer.spawn state.world state.player roll)
    destroyerRoared := true
    status := "The Destroyer has awoken!" }

private def GameState.summonDestroyer (state : GameState) : GameState :=
  if state.destroyer.isSome then { state with status := "The Destroyer is already present." }
  else if state.legacyEarly || !isNight state.surface.clock false then
    { state with status := "Use the Mechanical Worm at night." }
  else if state.bowCooldown > 0 then state
  else { state.consumeSelected.spawnDestroyer with bowCooldown := destroyerUseTime }

private def GameState.spawnSkeletronPrime (state : GameState) : GameState :=
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, state.player.centerTile, (557 : Nat), draw)).toNat
  { state with
    skeletronPrime := some (PrimeHead.spawn state.world state.player roll)
    primeRoared := true
    status := "Skeletron Prime has awoken!" }

private def GameState.summonSkeletronPrime (state : GameState) : GameState :=
  if state.skeletronPrime.isSome then { state with status := "Skeletron Prime is already present." }
  else if state.legacyEarly || !isNight state.surface.clock false then
    { state with status := "Use the Mechanical Skull at night." }
  else if state.bowCooldown > 0 then state
  else { state.consumeSelected.spawnSkeletronPrime with bowCooldown := 45 }

private def GameState.spawnWraith (state : GameState) (roll : Nat) (nearAltar : Bool) : GameState := Id.run do
  if state.wraiths.size ≥ maxWraiths || state.combat.health == 0 then return state
  let center := state.player.centerTile
  -- SpawnOnPlayer searches offscreen positions. Scale its search window to
  -- this game's 32-pixel tiles and keep the player's own tiles outside it.
  let offsets := if nearAltar then 11 else 24
  for attempt in [:80] do
    let sample := (hash (state.seed, roll, attempt)).toNat
    let side : Int := if sample % 2 == 0 then -1 else 1
    let x := center.x + side * (offsets + sample / 2 % 18 : Nat)
    let y := center.y + ((sample / 36 % 25 : Nat) : Int) - 12
    if x < 3 || x ≥ (state.world.width : Int) - 3 || y < 5 then continue
    let wraith := Wraith.atFloor state.nextWraithId x (y + 2)
    if !wraith.valid state.world || state.wraiths.any (fun other => other.hitBox.intersects wraith.hitBox) then continue
    let start := ⟨x, y⟩
    if (state.world.tile start).solid || state.water.level state.world start > 0 ||
        state.magma.level state.world start > 0 then continue
    return { state with wraiths := state.wraiths.push wraith, nextWraithId := wraith.id + 1 }
  return state

private def GameState.hammerAltar (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canReach position then return { state with status := "Move closer to the altar." }
  let power := (state.selectedStack.map (·.item.hammerPower)).getD 0
  if !state.hardmode || power < 80 then
    if state.combat.invulnerable > 0 then return state
    return { state with
      combat := state.combat.hurt (max 1 (state.combat.health / 2))
      status := "The altar resisted the hammer and hurt you." }
  if state.toolCooldown > 0 then return state
  let altar := altarTile state.seed
  let sites := eyeAltars state.seed ++ evilAltars state.seed
  let some anchor := sites.find? (fun anchor =>
    (altarCells anchor).contains position && (altarCells anchor).all (fun cell => state.world.tile cell == altar))
    | return state
  let world := (altarCells anchor).foldl (fun world cell => world.setTile cell .air) state.world
  let (world, mask, ore) := smashHardmodeAltar world state.seed state.surface.clock state.altarsSmashed state.hardmodeOreMask
  let mut smashed := { state with
    world, altarsSmashed := state.altarsSmashed + 1, hardmodeOreMask := mask, toolCooldown := 14
    status := s!"Your world has been blessed with {hardmodeOreBlessing ore}!" }
  let roll := (hash (state.seed, state.surface.clock, state.altarsSmashed, (82 : Nat))).toNat
  for index in [:1 + roll % 2] do smashed := smashed.spawnWraith (roll + index) true
  return smashed

private def GameState.useLifeCrystal (state : GameState) : GameState :=
  if state.combat.lifeCrystals ≥ maxLifeCrystals then
    { state with status := "Maximum health reached. The Life Crystal is kept." }
  else
    let combat := { state.combat with
      lifeCrystals := state.combat.lifeCrystals + 1
      health := state.combat.health + healthPerHeart }
    { state.consumeSelected with
      combat
      status := s!"Permanent heart gained. Maximum health is now {combat.healthLimit}." }

private def GameState.eatCaveEel (state : GameState) : GameState :=
  if state.breath == maxBreath then
    { state with status := "Your breath is already full." }
  else
    { state.consumeSelected with
      breath := min maxBreath (state.breath + eelBreathTicks)
      status := "The cave eel restored breath." }

private def GameState.fishingBaitSlot (state : GameState) (wanted : Option Item := none) : Option Nat := Id.run do
  for (first, last) in [(Inventory.ammoStart, Inventory.trashSlot), (0, Inventory.mainSlotCount)] do
    for index in [first:last] do
      if (state.inventory[index]?.join).any (fun stack =>
          stack.item.isFishingBait && wanted.all (· == stack.item)) then return some index
  return none

private def GameState.castOrReelLegacy (state : GameState) (target : TilePos) : GameState := Id.run do
  if state.grapple != .idle then
    return { state with status := "Release the grappling hook before fishing." }
  let some bait := state.fishingBaitSlot
    | return { state with fishing := .idle, status := "No bait. Press F2 to make some from wood and dirt." }
  match state.fishing with
  | .idle =>
    let some spot := legacyFishingSpot state.world state.water state.player target
      | return { state with status := "Cast into open water within reach." }
    return { state with fishing := .waiting spot fishingWaitTicks, status := "Line cast. Wait for a bite." }
  | .waiting .. => return { state with fishing := .idle, status := "Line reeled in early. Bait kept." }
  | .biting spot _ =>
    if legacyFishingSpot state.world state.water state.player spot != some spot then
      return { state with fishing := .idle, status := "Fishing spot lost. Bait kept." }
    let fish := match fishingWater state.seed spot with
      | .surface => Item.pondPerch
      | .underground => Item.caveEel
    let spent := Inventory.removeFromSlot state.inventory bait 1
    let some (inventory, _) := Inventory.addOne spent fish
      | return { state with status := "Make room for the catch. Bait kept." }
    return { state with inventory, fishing := .idle, status := s!"Caught a {fish.name}." }
  | .soaking .. | .hooked .. | .bobber .. => return { state with fishing := .idle }

private def GameState.currentFishingPower (state : GameState) (baitItem : Option Item) : Nat :=
  let polePower := (state.selectedStack.map (·.item.fishingPolePower)).getD 0
  let gear := match state.equipment.accessory with
    | some .anglerEarring | some .anglerBobber => 10
    | _ => 0
  let bonus := gear + if state.fishingPotion > 0 then 15 else 0
  fishingPower state.surface.clock (baitItem.map (·.fishingBaitPower) |>.getD 0) state.weather (polePower + bonus)

private def GameState.finishFishing (state : GameState) (reward : Option Item) : GameState :=
  let reeled := { state with fishing := .idle }
  match reward with
  | none => reeled
  | some fish =>
    let bait := (state.fishingBaitSlot.bind fun slot => state.inventory[slot]?.join).map (·.item)
    let power := state.currentFishingPower bait
    let count := fishingCatchQuantity fish power fun draw =>
      (hash (state.seed, state.weather.day, state.surface.clock, fish.code, (1169 : Nat), draw)).toNat
    let caught := match Inventory.addItems reeled.inventory fish count with
      | some (inventory, _) => { reeled with inventory }
      | none => reeled.dropLoot reeled.player.centerTile 0 #[⟨fish, count⟩]
    { caught with status := if count == 1 then s!"Caught {fish.name}." else s!"Caught {fish.name} ({count})." }

private def GameState.pullCatch (state : GameState) (motion : FishingBobber) (fish baitItem : Item) :
    GameState := Id.run do
  let motion := motion.pull
  let reeled := { state with fishing := .bobber motion (.returning none), status := "Reeling in." }
  let some slot := state.fishingBaitSlot (some baitItem)
    | return { reeled with status := "The bait used for this catch is missing." }
  let spot := motion.tile
  let sample := (hash (state.seed, state.surface.clock, spot, fish.code, (2846 : Nat))).toNat % 16777216
  let baitFactor : Float32 := if state.equipment.accessory == some .tackleBox then 2 else 1
  let consume := if baitItem == .goldWorm then sample % 20 == 0
    else fish != .seaweed && fish != .tinCan && fish != .oldShoe && fish != .jojaCola &&
      ((Int32.ofNat sample).toFloat32 / 16777216) * (baitFactor + (Int32.ofNat baitItem.fishingBaitPower).toFloat32 / 6) < 1
  let inventory := if consume then Inventory.removeFromSlot reeled.inventory slot 1 else reeled.inventory
  if state.equipment.accessory != some .highTestFishingLine &&
      (hash (state.seed, state.surface.clock, spot, fish.code, (2845 : Nat))).toNat % 7 == 0 then
    return { reeled with inventory, status := "The fishing line snapped." }
  return { reeled with inventory, fishing := .bobber motion (.returning (some fish)) }

private def GameState.castOrReel (state : GameState) (target : TilePos) (aim : WorldAim) : GameState := Id.run do
  if state.legacyEarly then return state.castOrReelLegacy target
  if state.bowCooldown > 0 || state.ride.isSome then return state
  let state := { state with bowCooldown := 8 }
  match state.fishing with
  | .idle =>
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots || !aim.valid state.world then return state
    let facing := if aim.x > bodyCenterX state.player then 1 else -1
    let speed := (state.selectedStack.map (·.item.fishingCastSpeed)).getD 9
    let motion := FishingBobber.launch state.world state.water state.magma state.player aim facing slot speed state.honey
    return { state with
      fishing := .bobber motion (.drifting 0)
      combat := { state.combat with facing }
      status := "Line cast." }
  | .bobber motion (.drifting _) =>
    return { state with fishing := .bobber motion.pull (.returning none), status := "Reeling in." }
  | .bobber motion (.hooked _ fish baitItem) => return state.pullCatch motion fish baitItem
  | .bobber _ (.returning _) => return state
  | .hooked spot _ fish baitItem =>
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then return state
    let fill := max (state.water.level state.world spot) (state.magma.level state.world spot)
    return state.pullCatch (FishingBobber.atSurface spot fill slot) fish baitItem
  | .waiting .. | .biting .. => return state.castOrReelLegacy target
  | .soaking spot _ =>
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then return state
    let fill := max (state.water.level state.world spot) (state.magma.level state.world spot)
    let motion := (FishingBobber.atSurface spot fill slot).pull
    return { state with fishing := .bobber motion (.returning none), status := "Reeling in." }

private def GameState.fishingBiomes (state : GameState) (surface : TilePos) (surfaceRow : Nat) :
    FishingBiomes := Id.run do
  let center := state.player.centerTile
  let (hallow, corruption, crimson) := state.world.hardmodeZones center
  let mut snow := 0
  let mut jungle := 0
  let mut sand := 0
  let mut dungeon := 0
  let left := max 0 (center.x - 32)
  let right := min (state.world.width : Int) (center.x + 33)
  let top := max 0 (center.y - 22)
  let bottom := min (state.world.height : Int) (center.y + 23)
  for col in [left.toNat:right.toNat] do
    for row in [top.toNat:bottom.toNat] do
      let tile := state.world.tile ⟨col, row⟩
      if tile == .ice || tile == .purpleIce || tile == .pinkIce || tile == .redIce then snow := snow + 1
      if tile == .jungleGrass then jungle := jungle + 1
      if tile == .sand || tile == .ebonsand || tile == .pearlsand || tile == .crimsand then sand := sand + 1
      if tile.isDungeonBrick then dungeon := dungeon + 1
  let unsafeWall := dungeonUnsafeWall state.seed center state.lowerDungeonInstalled
    state.blueMoonDungeonInstalled state.magicMissileDungeonInstalled state.deepDungeonInstalled
  return {
    dungeon := state.skeletronDefeated && state.dungeonInstalled && dungeon ≥ 250 &&
      unsafeWall && !hasWoodWall state.world state.walls center
    beach := center.y ≤ (surfaceRow : Int) + 12 && sand ≥ 210 &&
      (center.x < state.world.width / 24 || center.x ≥ state.world.width - state.world.width / 24)
    ocean := surface.x < state.world.width * 380 / 4200 ||
      surface.x > state.world.width - state.world.width * 380 / 4200
    corruption, crimson, hallow
    -- Scale native thresholds to the compact 65 by 45 scan.
    jungle := jungle ≥ 20
    snow := snow ≥ 210
    desert := sand ≥ 210
    sky := 2 * surface.y < (surfaceRow : Int)
  }

private def GameState.checkFishingCatch (state : GameState) (surface : TilePos)
    (baitItem : Item) (power : Nat) (roll : Nat → Nat) (bobberY : Float32) : Option (Item × Nat) :=
  let pond := fishingPond state.world state.water state.magma state.honey surface
  let tiles := pond.effectiveTiles
  let surfaceRow := groundHeight state.seed 19
  let underground := (surfaceRow : Int) ≤ surface.y
  -- Project the compact world into Small-world depth before applying the altitude formula.
  let scale := 1200 / state.world.height.toFloat
  let needed := fishingWaterNeeded 4200 (surfaceRow.toFloat * scale) (bobberY * scale.toFloat32)
  let lavaGear := baitItem.isLavaBait || state.equipment.accessory == some .lavaproofFishingHook ||
    state.selectedStack.any (·.item == .hotlineFishingHook)
  let quest := state.angler.quest
  let questAvailable := state.angler.rescued && state.angler.position.isSome && !state.angler.turnedIn &&
    !(state.inventory.extract 0 Inventory.mainSlotCount).any (·.any (·.item == quest.fish))
  let biomes := if pond.lava || pond.honey || tiles < 75 then {}
    else state.fishingBiomes surface surfaceRow
  let layer := if 2 * surface.y < (surfaceRow : Int) then 0
    else if !underground || biomes.beach || (biomes.jungle && surface.y ≤ (surfaceRow : Int) + 3) then 1
    else if surface.y < (state.world.height / 2 : Nat) then 2
    else if surface.y < (underworldStart : Nat) then 3 else 4
  let fish := if questAvailable then some quest.fish else none
  fishingCatch pond.lava lavaGear underground tiles power (fun draw => roll (draw + 4)) needed
    state.hardmode biomes (state.cratePotion > 0) fish layer (nativeOceanPondTiles state.world.width tiles) pond.honey

private def GameState.tickClassicFishing (state : GameState) : GameState := Id.run do
  let (target, clock, hooked) ← match state.fishing with
    | .soaking pos clock => pure (pos, clock, none)
    | .hooked pos remaining catchItem baitItem => pure (pos, remaining, some (catchItem, baitItem))
    | _ => return state
  let some (surface, fill) := fishingLiquidAt state.world state.water state.magma state.player target state.honey
    | return { state with fishing := .idle, status := "The bobber is out of the liquid or beyond the line's reach." }
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, surface, clock, (61 : Nat), draw)).toNat
  if let some (caught, baitItem) := hooked then
    let remaining := clock - (1 + roll 0 % 4)
    return { state with
      fishing := if remaining == 0 then .soaking surface 0 else .hooked surface remaining caught baitItem
      status := if remaining == 0 then "The fish got away. The line is still cast." else state.status }
  let baitItem := (state.fishingBaitSlot.bind fun slot => state.inventory[slot]?.join).map (·.item)
  let power := state.currentFishingPower baitItem
  let clock := clock + fishingWaitGain power roll
  if clock < fishingCheckClock then return { state with fishing := .soaking surface clock }
  let waiting := { state with fishing := .soaking surface 0 }
  let some baitItem := baitItem | return waiting
  if !fishingCanBite state.world state.water state.magma state.player surface fill state.honey then return waiting
  let motion := FishingBobber.atSurface surface fill 0
  let some (caught, remaining) := state.checkFishingCatch surface baitItem power roll motion.y
    | return waiting
  return { waiting with fishing := .hooked surface remaining caught baitItem, status := "Bite! Left-click to reel in." }

private def GameState.tickBobber (state : GameState) (motion : FishingBobber) (phase : BobberPhase) :
    GameState := Id.run do
  if let .returning reward := phase then
    if motion.distanceTo state.player > 3000 || motion.touchingPlayer state.player then
      let state := if state.status == "Reeling in." then { state with status := "Line reeled in." } else state
      return state.finishFishing reward
    let moved := motion.reel state.world state.water state.magma state.player state.honey
    if !moved.inWorld state.world then return state.finishFishing reward
    return { state with fishing := .bobber moved phase }
  let tile := motion.tile
  let next := motion.drift state.world state.water state.magma state.player state.honey
  let (next, phase, status) ← if motion.distanceTo state.player > 900 then
      pure (next, .returning none, "Reeling in.")
    else match phase with
    | .hooked clock fish baitItem =>
      let roll (draw : Nat) := (hash (state.seed, state.surface.clock, tile, clock, (61 : Nat), draw)).toNat
      let remaining := clock - (1 + roll 0 % 4)
      let motion := next.dip (roll 14) (roll 15)
      pure (motion, if remaining == 0 then .drifting 0 else .hooked remaining fish baitItem,
        if remaining == 0 then "The fish got away. The line is still cast." else state.status)
    | .drifting clock =>
      if !next.still then pure (next, phase, state.status)
      else do
        let baitItem := (state.fishingBaitSlot.bind fun slot => state.inventory[slot]?.join).map (·.item)
        let power := state.currentFishingPower baitItem
        let roll (draw : Nat) := (hash (state.seed, state.surface.clock, tile, clock, (61 : Nat), draw)).toNat
        let clock := clock + fishingWaitGain power roll
        if clock < fishingCheckClock then pure (next, .drifting clock, state.status)
        else
          let result := if fishingCanBiteAt state.world state.water state.magma state.player motion.centerY state.honey then
            baitItem.bind fun item => (state.checkFishingCatch tile item power roll motion.y).map (·, item)
            else none
          match result with
          | some ((fish, remaining), item) => pure (next, .hooked remaining fish item, "Bite! Left-click to reel in.")
          | none => pure (next, .drifting 0, state.status)
    | .returning _ => pure (next, phase, state.status)
  let moved := next.move state.world state.water state.magma state.honey
  if !moved.inWorld state.world then return { state with fishing := .idle }
  return { state with fishing := .bobber moved phase, status }

private def GameState.tickFishing (state : GameState) : GameState :=
  if state.fishing == .idle then state
  else if !state.selectedStack.any (·.item.isFishingPole) || state.cursedTicks > 0 then
    let reward := match state.fishing with | .bobber _ (.returning reward) => reward | _ => none
    ({ state with status := "Line reeled in." } : GameState).finishFishing reward
  else if let .bobber motion phase := state.fishing then
    state.tickBobber motion phase
  else if (match state.fishing with | .soaking .. | .hooked .. => true | _ => false) then
    state.tickClassicFishing
  else if state.fishingBaitSlot.isNone then
    { state with fishing := .idle, status := "Line reeled in. Bait kept." }
  else
    let fishing := advanceLegacyFishing state.world state.water state.player state.fishing
    let status := match state.fishing, fishing with
      | .waiting .., .biting .. => "Bite! Left-click to reel in."
      | .biting _ 1, .idle => "The fish got away. Bait kept."
      | _, .idle => "Fishing spot lost. Bait kept."
      | _, _ => state.status
    { state with fishing, status }

private def GameState.changeEquipment (state : GameState) (input : TickInput) : GameState :=
  if let some slot := input.gearClick then
    let (cursorStack, equipment) := clickGear state.cursorStack state.equipment slot
    { state with cursorStack, equipment }
  else
    let change := if let some index := input.equipGear then
        some (equipFromSlot state.inventory state.equipment index)
      else input.unequipGear.map (unequipGear state.inventory state.equipment)
    match change with
    | some (.ok (inventory, equipment)) =>
      { state with inventory, equipment, status := "Equipment updated." }
    | some (.error reason) => { state with status := reason }
    | none => state

private def GameState.swingToolAtSphere (state : GameState) (position : TilePos) (aim : WorldAim) :
    Option GameState := do
  let stack ← state.selectedStack
  let tool := stack.item
  if state.toolCooldown > 0 || !(tool.pickPower > 0 || tool.axePower > 0 || tool.hammerPower > 0) ||
      !state.canReachWithTool position tool then failure
  let sphere ← state.burningSpheres.find? fun sphere =>
    let box := sphere.hitBox
    box.left ≤ aim.x && aim.x < box.right && box.top ≤ aim.y && aim.y < box.bottom
  let next := state.damageTarget (.burningSphere sphere.id) 1
  return { next with toolCooldown := tool.toolSpeed }

private def UnderworldCritterKind.catchItem : UnderworldCritterKind → Item
  | .hellButterfly => .hellButterfly
  | .lavafly => .lavafly
  | .magmaSnail => .magmaSnail

private def Item.critterKind : Item → Option UnderworldCritterKind
  | .hellButterfly => some .hellButterfly
  | .lavafly => some .lavafly
  | .magmaSnail => some .magmaSnail
  | _ => none

private def GameState.releaseLavaCritter (state : GameState) (kind : UnderworldCritterKind) : GameState :=
  if state.world.height != underworldHeight || state.underworldCritters.size ≥ maxUnderworldCritters then state
  else
    let player := state.player
    let column := player.centerTile.x + state.combat.facing
    let floor := (player.y + playerHeight) / tileUnits
    let critter := UnderworldCritter.atFloor state.nextUnderworldNpcId kind column floor
    { state.consumeSelected with
      nextUnderworldNpcId := critter.id + 1
      underworldCritters := state.underworldCritters.push critter
      status := s!"Released the {kind.name}." }

/-- The normal-gravity item hitbox follows the three original swing phases.
Dimensions come from the source item frames. -/
private def GameState.netHitBox (state : GameState) (item : Item) : HitBox :=
  let (w, h) : Int × Int := match item with
    | .lavaproofBugNet => (48, 50)
    | .goldenBugNet => (44, 48)
    | _ => (36, 40)
  let remaining := state.netSwingTicks * 1000
  let duration := item.netUseTime
  let late := remaining < duration * 333
  let early := remaining ≥ duration * 666
  let facing := state.combat.facing
  let handX := bodyCenterX state.player / 16 +
    facing * (w / 2 - (if early && w ≥ 48 then 18 else 14)) * (if early then -1 else 1)
  let handY := state.player.y / 16 + if early then 10 else if late then 24 else 8
  let left := handX - (if facing < 0 then w else 0) -
    (if early && facing > 0 then w * 12 / 10 else if late && facing < 0 then w * 4 / 10 else 0)
  let top := handY - h + (if early then -(h * 4 / 10) else if late then h / 2 else 0)
  let width := if early then 2 * w else if late then w * 14 / 10 else w
  let height := if early then h * 14 / 10 else if late then h * 11 / 10 else h
  ⟨left * 16, top * 16, (left + width) * 16, (top + height) * 16⟩

private def GameState.advanceNetSwing (initial : GameState) : GameState := Id.run do
  let some selected := initial.selectedStack | return { initial with netSwingTicks := 0 }
  if selected.item.netUseTime == 0 || initial.combat.health == 0 then
    return { initial with netSwingTicks := 0 }
  if initial.netSwingTicks == 0 then return initial
  let hitBox := initial.netHitBox selected.item
  let mut state := initial
  let mut critters := #[]
  for critter in initial.underworldCritters do
    if !hitBox.intersects critter.hitBox then
      critters := critters.push critter
      continue
    if selected.item == .bugNet then
      if state.combat.invulnerable == 0 then
        let hurt := state.combat.hurt 1
        state := { state with
          combat := { hurt with
            invulnerable := 20
            onFire := if state.obsidianSkinTicks > 0 then hurt.onFire else max 300 hurt.onFire }
          status := "The Bug Net cannot catch lava critters." }
      critters := critters.push critter
    else
      let item := critter.kind.catchItem
      if let some (inventory, _) := Inventory.addOne state.inventory item then
        state := { state with inventory, status := s!"Caught {item.name}." }
      else
        state := state.dropLoot state.player.centerTile 0 #[⟨item, 1⟩]
  return { state with underworldCritters := critters, netSwingTicks := initial.netSwingTicks - 1 }

private def GameState.useHoneyTool (state : GameState) (item : Item) (target : TilePos) : GameState := Id.run do
  if state.toolCooldown > 0 || !state.world.contains target || state.tileDistance target > actionReach + 2 ||
      (state.world.tile target).solid then return state
  if item == .bottomlessHoneyBucket then
    if state.water.level state.world target > 0 || state.magma.level state.world target > 0 ||
        state.honey.level state.world target == Water.capacity then return state
    return { state with
      honey := state.honey.setLevel state.world target Water.capacity
      toolCooldown := 12
      status := "Poured honey." }
  let fill := state.honey.level state.world target
  if fill == 0 then return state
  let mut honey := state.honey.setLevel state.world target 0
  let mut room := Water.capacity - fill
  for dx in [-1, 0, 1] do
    for dy in [-1, 0, 1] do
      if room == 0 then break
      let pos : TilePos := ⟨target.x + dx, target.y + dy⟩
      let available := honey.level state.world pos
      let removed := min room available
      if removed == 0 then continue
      honey := honey.setLevel state.world pos (available - removed)
      room := room - removed
  if !honey.levels.data.any (· != 0) then honey := default
  return { state with honey, toolCooldown := 12, status := "Absorbed honey." }

private def GameState.applyActions (state : GameState) (input : TickInput) (aquaWasActive : Bool) :
    GameState × Option TilePos × Nat × Bool := Id.run do
  let input := if state.blueMoon.isSome || state.sunfury.isSome || state.darkLance.isSome || state.valor.isSome then
      { input with mine := none, place := none, selectSlot := none, swapSlot := none, inventoryClick := none }
    else input
  let mut next := state
  if let some index := input.selectSlot then
    if index < Inventory.hotbarCount && index < next.inventory.size then
      next := { next with selected := index }
  if let some index := input.swapSlot then
    next := next.swapBackpackSlot index
  next := next.handleInventoryClick input.inventoryClick
  if input.cancelCratePotion then next := { next with cratePotion := 0 }
  if let some mark := input.minionAim.filter (WorldAim.valid next.world) then
    if next.selectedStack.any (·.item == .bladeStaff) then next := next.markMinionTarget mark
  if input.dropVoodooDoll then next := next.throwVoodooDoll
  if let some aim := input.quickGrapple.filter (WorldAim.valid next.world) then
    if let some item := next.quickHook then
      next := next.startGrapple item aim
  let damage := next.selectedStack.map (fun stack =>
    if next.legacyEarly then stack.item.legacySwordDamage else stack.item.swordDamage) |>.getD 0
  let damage := if damage > 0 && next.weakTicks > 0 then max 1 ((949 * damage + 500) / 1000) else damage
  let mut attackAt := none
  let mut horn := false
  let useAt := input.mine.orElse fun _ =>
    if input.useHeld && next.selectedStack.any (fun stack => stack.item == .hellwingBow || stack.item == .bladeStaff ||
        stack.item.netUseTime > 0 ||
        stack.item == .bottomlessHoneyBucket || stack.item == .honeyAbsorbantSponge) &&
        state.blueMoon.isNone && state.sunfury.isNone && state.darkLance.isNone && state.valor.isNone then
      (input.aim.filter (WorldAim.valid next.world)).map (fun aim => ⟨aim.x / tileUnits, aim.y / tileUnits⟩)
    else none
  if let some position := useAt then
    if next.selectedStack.any (·.item == .watchhorn) then
      horn := true
    else if next.selectedStack.any (·.item == .gelatinousPillion) then
      next := next.mountWingedSlime
    else if next.selectedStack.any (·.item == .bladeStaff) then
      next := next.summonDagger (input.aim.getD (WorldAim.atTile position))
    else if next.selectedStack.any (·.item == .draught) then
      next := next.restoreHealth (2 * healthPerHeart) "The healing draught restored health."
    else if let some potion := next.selectedStack.filter
        (fun stack => stack.item == .lesserHealingPotion || stack.item == .healingPotion ||
          stack.item == .restorationPotion || stack.item == .greaterHealingPotion || stack.item == .honeyfin) then
      next := next.drinkHealingPotion potion.item
    else if let some potion := next.selectedStack.filter
        (fun stack => stack.item == .lesserManaPotion || stack.item == .manaPotion) then
      if next.mana < baseMana then
        let mana := if potion.item == .lesserManaPotion then 50 else 100
        next := { next.consumeSelected with mana := min baseMana (next.mana + mana) }
    else if let some tool := next.selectedStack.filter (fun stack =>
        stack.item == .bottomlessHoneyBucket || stack.item == .honeyAbsorbantSponge) then
      next := next.useHoneyTool tool.item position
    else if next.selectedStack.any (·.item == .cratePotion) then
      next := { next.consumeSelected with cratePotion := cratePotionTicks, status := "Crate Potion active." }
    else if next.selectedStack.any (·.item == .fishingPotion) then
      next := { next.consumeSelected with fishingPotion := fishingPotionTicks, status := "Fishing Potion active." }
    else if next.selectedStack.any (·.item == .sonarPotion) then
      next := { next.consumeSelected with sonarPotion := sonarPotionTicks, status := "Sonar Potion active." }
    else if next.selectedStack.any (·.item == .obsidianSkinPotion) then
      next := { next.consumeSelected with
        obsidianSkinTicks := maxObsidianSkinTicks
        combat := { next.combat with onFire := 0 }
        burningTicks := 0
        status := "Obsidian Skin protects you from lava and hot blocks." }
    else if next.selectedStack.any (·.item == .gelatinCrystal) then
      next := next.summonQueenSlime
    else if next.selectedStack.any (·.item == .suspiciousLookingEye) then
      next := next.summonEye
    else if next.selectedStack.any (·.item == .mechanicalEye) then
      next := next.summonTwins
    else if next.selectedStack.any (·.item == .mechanicalWorm) then
      next := next.summonDestroyer
    else if next.selectedStack.any (·.item == .mechanicalSkull) then
      next := next.summonSkeletronPrime
    else if let some stack := next.selectedStack.filter (fun stack => stack.item == .wormFood || stack.item == .bloodySpine) then
      next := next.summonEvilItem stack.item
    else if next.selectedStack.any (·.item == .highlandBerry) then
      next := next.restoreHealth highlandBerryHealing "The Highland Berry restored health."
    else if next.selectedStack.any (·.item == .lifeCrystal) then
      next := next.useLifeCrystal
    else if next.selectedStack.any (·.item == .pondPerch) then
      next := next.restoreHealth healthPerHeart "The pond perch restored health."
    else if next.selectedStack.any (·.item == .caveEel) then
      next := next.eatCaveEel
    else if next.selectedStack.any (·.item == .gillsElixir) then
      next := { next.consumeSelected with
        gills := gillsTicks
        status := "Gills Elixir active. Your breath holds underwater." }
    else if next.selectedStack.any (·.item == .barkskinTonic) then
      next := { next.consumeSelected with
        barkskin := barkskinTicks
        status := "Barkskin Tonic active. Defense increased by one." }
    else if next.selectedStack.any (·.item.isFishingPole) then
      let aim := (input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position)
      next := next.castOrReel position aim
    else if let some selected := next.selectedStack.filter (fun stack => stack.item.netUseTime > 0) then
      if next.netSwingTicks == 0 && next.bowCooldown == 0 && next.combat.swing ≤ 1 && next.shortsword.isNone then
        let aim := (input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position)
        let facing := if aim.x < bodyCenterX next.player then -1 else 1
        next := { next with netSwingTicks := selected.item.netUseTime, combat := { next.combat with facing } }
    else if let some kind := next.selectedStack.bind (·.item.critterKind) then
      next := next.releaseLavaCritter kind
    else if let some stack := next.selectedStack.filter (fun stack => stack.item.gearSlot == some .hook) then
      let aim := (input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position)
      next := next.startGrapple stack.item aim
    else if next.selectedStack.any (·.item == .recallCharm) then
      next := next.recallHome
    else if next.selectedStack.any (·.item == .recallPotion) then
      next := next.drinkRecallPotion
    else if let some stack := next.selectedStack.filter (·.item.isBomb) then
      next := next.useBomb position stack.item
    else if next.selectedStack.any (·.item == .bone) then
      next := next.throwBone position
    else if next.selectedStack.any (·.item == .aquaScepter) then
      if !aquaWasActive then next := next.startAquaUse (input.aim.getD (WorldAim.atTile position))
    else if next.selectedStack.any (fun stack => stack.item == .blueMoon || stack.item == .sunfury) then
      next := next.startBlueMoon ((input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position))
    else if next.selectedStack.any (·.item == .darkLance) then
      next := next.startDarkLance ((input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position))
    else if next.selectedStack.any (·.item == .magicMissile) then
      next := next.startMagicMissile ((input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position))
    else if let some stack := next.selectedStack.filter (fun stack =>
      stack.item == .flowerOfFire || stack.item == .flamelash || stack.item == .demonScythe) then
      next := next.startUnderworldMagic stack.item
        ((input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position))
    else if next.selectedStack.any (·.item == .valor) then
      next := next.startValor ((input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position))
    else if next.selectedStack.any (·.item == .hellwingBow) then
      next := next.fireHellwing ((input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile position))
    else if let some stack := next.selectedStack.filter (fun stack =>
        stack.item.gunShot.isSome || stack.item.bowShot.isSome) then
      let kind := (stack.item.gunShot.orElse fun _ => stack.item.bowShot).getD .arrow
      let shot := if !next.legacyEarly && kind == .arrow then .woodenArrow else kind
      next := next.fireRanged stack.item shot position input.aim
    else if next.selectedStack.any (·.item == .copperShortsword) then
      attackAt := some position
    else if let some hit := next.swingToolAtSphere position (input.aim.getD (WorldAim.atTile position)) then
      next := hit
    else if next.world.tile position == .clayPot && next.canReach position &&
        next.selectedStack.any (fun stack =>
          stack.item.pickPower > 0 || stack.item.axePower > 0 || stack.item.hammerPower > 0 || damage > 0) then
      next := next.breakPot position
    else if (next.world.tile position).cuttableEvilPlant.isSome && next.canReach position &&
        next.selectedStack.any (fun stack =>
          stack.item.pickPower > 0 || stack.item.axePower > 0 || stack.item.hammerPower > 0 || damage > 0) then
      next := next.cutEvilPlant position
    else if next.selectedStack.any (fun stack => stack.item.hammerPower > 0) &&
        ((next.world.tile position).isEvilAltar || (next.world.tile position).isEvilRelic ||
          hasWoodWall next.world next.walls position) then
      let tile := next.world.tile position
      next := if tile.isEvilAltar then next.hammerAltar position
        else if tile.isEvilRelic then
          if next.canReach position && terrainLineClear next.world
              (bodyCenterX next.player) (bodyCenterY next.player)
              (position.x * tileUnits + tileUnits / 2) (position.y * tileUnits + tileUnits / 2) then
            next.breakEvilRelic position
          else { next with status := "Reach the relic before hammering it." }
        else next.hammerWall position
    else if next.world.tile position == .cobweb && damage > 0 then
      next := next.mineTile position
    else if damage > 0 then
      attackAt := some position
    else
      next := next.mineTile position
      if (next.world.tile position).evilMushroom != some (next.world.tile (above position)) then
        next := next.cutEvilPlant (above position)
  if let some position := input.place then
    next := next.placeTile position
  if let some recipe := input.craftClassic then
    next := next.craftClassic recipe input.craftToCursor
  if input.craft then
    next := if next.legacyEarly then next.craftPick else next.craftClassic (.pick .copper)
  if input.craftTorches then
    next := if next.legacyEarly then next.craftEquipment .torch 3 [(.wood, 1)]
      "Made 3 torches. Right-click beside a block to place one." else next.craftClassic .torches
  if input.craftSword && next.legacyEarly then
    next := next.craftEquipment .stoneSword 1 [(.stone, 2), (.wood, 1)]
      "Stone sword equipped. Left-click toward a crawler to swing."
  if input.craftCrystalPick then
    next := next.craftEquipment .crystalPick 1 [(.crystal, crystalPickCost)]
      "Crystal pick equipped. Mine iron in the frost cavern."
  if input.craftIronSword then
    next := if next.legacyEarly then next.craftEquipment .ironSword 1 [(.iron, ironSwordCost)]
      "Iron sword equipped. Explore the ice hall to the east." else next.craftClassic (.sword .iron)
  if input.craftWorkbench then
    next := if next.legacyEarly then next.craftEquipment .workbench 1 [(.wood, workbenchCost)]
      "Workbench crafted. Right-click to place it." false else next.craftClassic .workbench
  if input.craftChest then
    next := if next.legacyEarly then next.craftEquipment .chest 1 [(.wood, chestCost)]
      "Chest crafted. Place it, then point at it and press Q." else next.craftClassic .chest
  if input.craftDoor then
    next := if next.legacyEarly then next.craftEquipment .door 1 [(.wood, doorCost)]
      "Door crafted. Place it in a two-tile doorway." else next.craftClassic .door
  if input.craftSilverSword then
    next := if next.legacyEarly then next.craftEquipment .silverSword 1 [(.silver, silverSwordCost)]
      "Silver sword equipped. Four damage per strike." else next.craftClassic (.sword .silver)
  if input.craftBow then
    next := if next.legacyEarly then next.craftEquipment .bow 1 [(.wood, bowCost)]
      "Wooden bow equipped. Craft arrows with I, then aim and click." else next.craftClassic .woodBow
  if input.craftArrows then
    let selected := next.selected
    next := if next.legacyEarly then next.craftEquipment .arrow arrowBatch [(.wood, 1), (.stone, 1)]
      s!"Crafted {arrowBatch} arrows." else next.craftClassic .arrows
    next := { next with selected }
  let ores := earlyOres next.seed
  let armorOre : Item := if !next.legacyEarly && ores.silver == .tungstenOre then .tungstenOre else .silver
  if input.craftMail then
    next := next.craftEquipment .geodeMail 1 [(.geode, geodeMailGeodes), (armorOre, geodeMailSilver)]
      "Geode Mail crafted. Open the backpack and right-click it to equip."
  if input.craftPlatforms then
    next := if next.legacyEarly then next.craftEquipment .platform platformBatch [(.wood, platformWood)]
      "Platforms crafted. Jump up through them or hold Down to drop." false else next.craftClassic .platforms
  if input.craftBeacon then
    next := next.craftEquipment .homeBeacon 1 [(.wood, beaconWood), (.stone, beaconStone)]
      "Home Beacon crafted. Right-click to place it above a solid floor."
  if input.craftRecall then
    let recallOre : Item := if !next.legacyEarly && ores.copper == .tinOre then .tinOre else .copper
    next := next.craftEquipment .recallCharm 1 [(recallOre, recallCopper), (.crystal, recallCrystal)]
      "Recall Charm crafted. Left-click to return to your bound beacon."
  if input.craftGelTorches then
    next := if next.legacyEarly then next.craftEquipment .torch gelTorchBatch [(.caveGel, 1), (.wood, 1)]
      s!"Made {gelTorchBatch} torches with Cave Gel." else next.craftClassic .torches
  if input.craftHunterBow then
    next := next.craftEquipment .hunterBow 1 [(.frostShell, 1), (.quarryScale, 1), (.wood, hunterBowWood)]
      "Hunter Bow equipped. Regular arrows deal three damage."
  if input.craftFishingRod then
    next := if next.legacyEarly then next.craftEquipment .fishingRod 1 [(.wood, rodWood), (.stone, rodStone)]
      "Fishing Rod equipped. Left-click reachable water to cast." else next.craftClassic .woodFishingPole
  if input.craftBait && next.legacyEarly then
    let selected := next.selected
    next := next.craftEquipment .bait baitBatch [(.wood, 1), (.dirt, 1)]
      s!"Made {baitBatch} bait."
      false
    next := { next with selected }
  if input.craftGills then
    next := next.craftEquipment .gillsElixir 1 [(.sunleaf, 1), (.caveEel, 1)]
      "Gills Elixir made. Select and left-click to drink."
  if input.craftBarkskin then
    next := next.craftEquipment .barkskinTonic 1 [(.sunleaf, 1), (.pondPerch, 1)]
      "Barkskin Tonic made. Select and left-click to drink."
  let hardwareOre : Item := if !next.legacyEarly && ores.iron == .leadOre then .leadOre else .iron
  if input.craftRails then
    next := next.craftEquipment .rail railBatch [(hardwareOre, railIronCost), (.wood, railWoodCost)]
      s!"Made {railBatch} rails. Right-click an open tile to lay track."
  if input.craftMinecart then
    next := next.craftEquipment .minecart 1 [(hardwareOre, minecartIronCost), (.wood, minecartWoodCost)]
      "Minecart made. Right-click a rail to deploy it."
  if input.craftGrapplingHook then
    next := next.craftEquipment .grapplingHook 1 [(hardwareOre, grapplingHookIronCost), (.wood, grapplingHookWoodCost)]
      "Grappling Hook made. Select it and left-click to fire."
  if input.craftHighlandCrossbow then
    next := next.craftEquipment .highlandCrossbow 1
      [(.deepsteel, highlandCrossbowOreCost), (.wood, highlandCrossbowWoodCost)]
      "Highland Crossbow made. Its bolts fly straight and need time to reload."
  if next.legacyEarly && (next.homeDoor.isSome || next.merchantDoor.isSome) &&
      (input.mine.isSome || input.place.isSome) && next.world.tiles != state.world.tiles then
    next := next.refreshHome.refreshMerchant
  if let some position := input.interact then
    next := (next.defuseBomb position).getD (next.interactAt position)
  if let some position := input.claimHome then
    let assigned := next.claimHouse position
    next := if assigned.legacyEarly && assigned.homeDoor != next.homeDoor then assigned.refreshMerchant else assigned
  if let some position := input.claimMerchant then
    next := next.claimHouse position true
  return (next, attackAt, damage, horn)

private def GameState.updateExploration (next before : GameState) (input : TickInput)
    (ambientEdit : Bool) : GameState :=
  let edited := next.world.tiles != before.world.tiles
  let wallEdit := next.walls != before.walls
  let next := if edited || wallEdit then next.refreshBed else next
  let next := if !next.legacyEarly && (edited || wallEdit) then
      next.refreshClassicHousing (input.place.orElse (fun _ => input.mine))
    else next
  let next := if !next.angler.rescued || !(edited || wallEdit) then next else
    let available (home : ClassicHome) :=
      next.guide != some home.stand && next.merchant != some home.stand
    let existing := next.angler.home.bind fun pos =>
      ((inspectClassicHome next.world next.walls pos).toOption).filter available
    let candidate := (input.place.orElse (fun _ => input.mine)).bind fun pos =>
      ((inspectClassicHome next.world next.walls pos).toOption).filter available
    let home := (existing.orElse fun _ => candidate).map (·.stand)
    { next with angler := { next.angler with home } }
  let next := if edited && !next.tileHits.isEmpty then
      { next with tileHits := next.tileHits.filter (fun hit => next.world.tile hit.position == hit.tile) }
    else next
  let center := next.player.centerTile
  let mayEdit := input.mine.isSome || input.place.isSome || input.interact.isSome
  if center != before.player.centerTile || ambientEdit || (mayEdit && edited) then
    { next with chart := next.chart.reveal next.world center }
  else next

private def GameState.walkOrSwim (state : GameState) (input : TickInput) (horizontal : Int)
    (modal jump : Bool) (knockback : Option Int) : Player :=
  if let some direction := knockback then
    moveBodyDelta state.world state.player (npcContactKnockbackX * direction) (-56)
      (!modal && input.dropThrough)
  else
    let inWater := state.water.submerged state.world
      (state.player.x + playerWidth / 2) (state.player.y + playerHeight * 3 / 4)
    let inHoney := state.honey.submerged state.world
      (state.player.x + playerWidth / 2) (state.player.y + playerHeight * 3 / 4)
    if inWater || inHoney then
      moveSwimmingPlayer state.world state.player horizontal (!modal && (input.swim || jump)) inHoney
    else
      let speed := if state.burningTicks > 0 || state.slowTicks > 0 then state.equipment.speed / 2 else state.equipment.speed
      let speed := if state.weakTicks > 0 then speed * 9 / 10 else speed
      moveEquippedPlayer state.world state.player horizontal (!modal && jump) speed
        (!modal && input.dropThrough)

private def GameState.moveAssassin (state : GameState) (input : TickInput) (horizontal : Int)
    (modal : Bool) (knockback : Option Int) : GameState := Id.run do
  let walk := state.walkOrSwim input horizontal modal input.jump knockback
  if !state.equipment.crystalAssassinSet then
    return { state with player := walk, assassinDash := {} }
  let dash := state.assassinDash
  let tap := if modal then 0 else max (-1) (min 1 input.dashTap)
  let expiredTap := if dash.tap < 0 then dash.tap + 1 else max 0 (dash.tap - 1)
  let cooldown := dash.cooldown - 1
  if knockback.isSome || dash.cooldown > 0 then
    return { state with player := walk, assassinDash := { cooldown } }
  let launch := dash.velocity == 0 && tap * expiredTap > 0
  let speed := if launch then
      let front := if tap < 0 then state.player.x - 32 else state.player.x + playerWidth + 32
      let blocked := (state.world.tile ⟨front / tileUnits, (state.player.y + 32) / tileUnits⟩).solid ||
        (state.world.tile ⟨front / tileUnits, (bodyCenterY state.player) / tileUnits⟩).solid
      let speed := if blocked then assassinDashSpeed / 2 else assassinDashSpeed
      if tap < 0 then -speed else speed
    else if dash.velocity.abs > 12 then dash.velocity * 0.992
    else dash.velocity * 0.96
  let dx := (speed * 16).toInt32.toInt
  if dx.natAbs ≤ state.equipment.speed.toNat then
    let ended := dash.velocity != 0
    let tap := if ended then 0 else if tap != 0 then tap * 15 else expiredTap
    return { state with
      player := walk
      assassinDash := { tap, cooldown := if ended then assassinDashCooldown else 0 } }
  let player := moveDashingPlayer state.world state.player dx (!modal && input.jump)
    (!modal && input.dropThrough)
  let assassinDash : AssassinDash := if player.vx == 0 then { cooldown := assassinDashCooldown }
    else { velocity := speed }
  return { state with
    player, assassinDash
    combat := { state.combat with facing := if dx < 0 then -1 else 1 } }

private def GameState.move (state : GameState) (input : TickInput) (horizontal : Int)
    (modal : Bool) (knockback : Option Int) : GameState :=
  match state.ride with
  | some ride =>
    if !modal && (input.dismountCart || input.quickMount) then
      let body := cartBody ride.parkingTile
      { state.parkCart ride with
        player := moveBodyDelta state.world { body with y := body.y + tileUnits } 0 0
        status := "Minecart parked." }
    else
      let ride := advanceRail state.world state.carts ride horizontal
      { state with player := ride.body, ride := some ride }
  | none =>
    if let some mount := state.wingedSlime then
      let (player, mount) := advanceWingedSlime state.world state.water state.honey state.magma state.player mount
        horizontal (!modal && (input.swim || input.jump)) (!modal && input.dropThrough) knockback
      { state with player, wingedSlime := some mount }
    else if state.grapple == .idle then
      state.moveAssassin input horizontal modal knockback
    else if !modal && input.jump then
      { state with
        player := state.walkOrSwim input horizontal modal false knockback
        grapple := .idle
        status := "Hook released." }
    else
      let player := if state.grapple.attachment.isSome then state.player
        else state.walkOrSwim input horizontal modal false knockback
      let (player, grapple, event) := advanceGrapple state.world player state.grapple (!modal && input.reelHook)
      { state with player, grapple, status := (event.map grappleMessage).getD state.status }

private def GameState.wallReleasePosition (state : GameState) : Player := Id.run do
  if !state.player.collides state.world then return state.player
  for range in [1:13] do
    for (dx, dy) in [(0, -(range : Int)), (0, range), (-(range : Int), 0), (range, 0)] do
      let player := { state.player with
        x := state.player.x + dx * tileUnits, y := state.player.y + dy * tileUnits
        vx := 0, vy := 0, grounded := false }
      if state.world.contains player.centerTile && !player.collides state.world then return player
  return respawn state.world state.player state.combat.crawler

private def GameState.wallPlayerStatus (state : GameState) : GameState := Id.run do
  let some wall := state.wallOfFlesh | return { state with horrified := false, tongued := false }
  if state.combat.health == 0 then return { state with horrified := false, tongued := false }
  let player := state.player
  let x := wallPixel player.x
  let y := wallPixel player.y
  let dx := wall.centerX - wallPixel (bodyCenterX player)
  let dy := wall.centerY - wallPixel (bodyCenterY player)
  let grossLine := wall.x + (if wall.direction > 0 then -56 else 40)
  let contact := state.horrified && x + wallPixel playerWidth > grossLine && x < grossLine + 140 &&
    state.combat.invulnerable ≤ 1
  let combat := if !contact then state.combat else
    let roll := (hash (state.seed, state.surface.clock, player.x, player.y, (113 : Nat))).toNat
    let (damage, immunity) := classicNpcContactDamage 50 roll state.defense
    { state.combat.hurt damage with invulnerable := immunity + 1 }
  if combat.health == 0 then
    return { state with horrified := false, tongued := false, combat, status := "Wall of Flesh hit you." }
  let state := { state with combat }
  let horrified := state.horrified || (y > (Int32.ofNat (underworldStart - 45)).toFloat32 * 16 &&
    (x - grossLine).abs < 1920)
  let behind := if wall.direction < 0 then dx < -40 else dx > 40
  let tongued := horrified && (state.tongued || y < (Int32.ofNat underworldStart).toFloat32 * 16 || behind)
  if !tongued then return { state with horrified, tongued }
  let bound := (Int32.ofNat (state.world.width - 38)).toFloat32 * 16
  if dx * dx + dy * dy > 9000000 || wall.x < 608 || wall.x > bound then
    return { state with
      horrified := false, tongued := false, combat := state.combat.afterDefeat
      status := "You were slain by the Wall of Flesh." }
  return { state with horrified, tongued }

private def GameState.moveTongued (state : GameState) : GameState := Id.run do
  let some wall := state.wallOfFlesh | return state
  let dx := wall.centerX + 200 * (Int32.ofInt wall.direction).toFloat32 - wallPixel (bodyCenterX state.player)
  let dy := wall.centerY - wallPixel (bodyCenterY state.player)
  let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  let scale := if distance > 11 then 11 / distance else 1
  let x := state.player.x + (dx * scale * 16).toInt32.toInt
  let y := state.player.y + (dy * scale * 16).toInt32.toInt
  let player : Player := ⟨max 0 (min (state.world.width * 256 - playerWidth) x),
    max 0 (min (state.world.height * 256 - playerHeight) y), 0, 0, false⟩
  let state := if let some ride := state.ride then state.parkCart ride else state
  let moved := { state with
    player, grapple := .idle, wingedSlime := none
    fishing := if state.fishing.slot.isSome then state.fishing else .idle }
  if distance > 11 then return moved
  return { moved with tongued := false, player := moved.wallReleasePosition }

private def GameState.beginDefeat (state : GameState) : GameState :=
  let state := state.dismountWingedSlime
  let state := match state.ride with
    | some ride => state.parkCart ride
    | none => state
  let state := match state.fishing with
    | .bobber _ (.returning (some fish)) => { state.finishFishing (some fish) with status := state.status }
    | _ => state
  { state with
    combat := state.combat.afterDefeat
    respawnTicks := classicRespawnTicks
    assassinDash := {}
    wingedSlime := none
    dagger := none
    minionTarget := none
    player := state.wallReleasePosition
    horrified := false
    tongued := false
    openChest := none
    openShop := false
    openGuide := false
    openAngler := false
    openOldMan := false
    dungeonGuardian := none
    dungeonGuardianHealth := 0
    projectiles := #[]
    aquaStreams := #[]
    shortsword := none
    blueMoon := none
    sunfury := none
    darkLance := none
    hotTileTicks := 0
    burningTicks := 0
    poisonedTicks := 0
    poisonProgress := 0
    hellfireTicks := 0
    hellfireProgress := 0
    bleedingTicks := 0
    spikeCooldown := 0
    darknessTicks := 0
    obsidianSkinTicks := 0
    netSwingTicks := 0
    valor := none
    aquaBurst := none
    aquaAnimation := 0
    aquaShotDelay := 0
    manaRegenDelay := 0
    manaRegenProgress := 0
    bowCooldown := 0
    toolCooldown := 0
    tileHits := #[]
    potionSickness := 0
    honeyBuff := 0
    cursedTicks := 0
    slowTicks := 0
    cursedInfernoTicks := 0
    cursedInfernoProgress := 0
    weakTicks := 0
    eye := state.eye.bind fun eye =>
      if eye.phase == .retreat then some eye else advanceEye state.world state.player eye true
    eater := state.eater.bind (fun boss => advanceEater state.world state.water state.player boss true)
    brain := state.brain.bind (fun boss => advanceBrain state.world state.player boss true)
    evilEnemies := #[]
    fishing := .idle
    grapple := .idle
    gills := 0
    barkskin := 0
    cratePotion := 0
    fishingPotion := 0
    sonarPotion := 0
    warden := match state.warden with | .fighting _ => .ready | other => other
    storm := if state.storm.active then .ready else state.storm
    ruin := if state.ruin.active then .ready else state.ruin
    cavern := if state.cavern.active then .ready else state.cavern
    raid := if state.raid.activeNow then .ready else state.raid }

private def guideHomeBody (world : World) (position : TilePos) : Option Player :=
  let floor := position.y + 1
  let body : Player := ⟨position.x * tileUnits + (tileUnits - playerWidth) / 2,
    floor * tileUnits - playerHeight, 0, 0, true⟩
  if body.collides world then none else some body

private def guideOffscreen (player : Player) (position : TilePos) : Bool :=
  let center := player.centerTile
  (center.x - position.x).natAbs > 24 || (center.y - position.y).natAbs > 16

private def GameState.guideProtectsTown (state : GameState) : Bool :=
  state.townGuide.any fun npc => !guideOffscreen state.player npc.body.centerTile

private def GameState.walkGuide (state : GameState) (guide : GuideNPC) (night : Bool) : GameState × GuideNPC := Id.run do
  let destination := if night then state.guide else none
  if let some home := destination then
    if guideOffscreen state.player home && guideOffscreen state.player guide.body.centerTile then
      if let some body := guideHomeBody state.world home then
        return (state, { guide with body, doorToClose := none })
  let body := guide.body
  let pos := body.centerTile
  let center := (state.guide.map (·.x)).getD 21
  let direction := if let some home := destination then
      if pos.x < home.x then 1 else if pos.x > home.x then -1 else 0
    else if pos.x ≥ center + 6 then -1 else if pos.x ≤ center - 6 then 1
    else if body.vx == 0 && guide.doorToClose.isNone then -guide.direction else guide.direction
  let talking := state.openGuide && state.canReach pos
  let direction := if talking then 0 else direction
  let edge := if direction < 0 then (body.x - 1) / tileUnits else (body.x + playerWidth) / tileUnits
  let ahead := doorBase state.world ⟨edge, pos.y⟩
  let mut next := state
  let mut doorToClose := guide.doorToClose.filter (fun door => state.world.tile door == .doorOpen)
  if direction != 0 && destination.isSome && doorToClose.isNone then
    if let some door := ahead then
      if state.world.tile door == .doorClosed then
        let opened := state.useDoor door
        if opened.world.tile door == .doorOpen then
          next := { opened with status := state.status }
          doorToClose := some door
  let walked := moveBody next.world body (12 * direction) false
  let moved := if direction != 0 && body.grounded && walked.x == body.x && ahead.isNone then
      moveBody next.world body (12 * direction) true else walked
  let movedGuide := { guide with body := moved, direction := if direction == 0 then guide.direction else direction }
  if let some door := doorToClose then
    let crossed := if movedGuide.direction > 0 then moved.x ≥ (door.x + 1) * tileUnits
      else moved.x + playerWidth ≤ door.x * tileUnits
    if crossed then
      let closed := ({ next with townGuide := some movedGuide } : GameState).useDoor door
      if closed.world.tile door == .doorClosed then
        next := { closed with status := next.status }
        doorToClose := none
  return (next, { movedGuide with doorToClose })

private def GameState.advanceGuide (state : GameState) (mayShoot := true) : GameState := Id.run do
  if state.legacyEarly then return state
  let night := isNight state.surface.clock false
  let some guide := state.townGuide
    | if !night && state.surface.clock > 0 && state.surface.clock % 7200 == 0 then
        if let some body := state.guide.bind (guideHomeBody state.world) then
          return { state with townGuide := some { body }, guideRespawnPending := false, status := "The Guide arrived at his house." }
      return state
  let (next, guide) := state.walkGuide guide night
  let enemies := [next.surface.creature, next.surface.flying].filterMap id
  let touching := enemies.find? (fun enemy => bodiesOverlap enemy.body guide.body)
  let inLava := Magma.touches next.magma next.world guide.body
  let raw := if inLava then 50
    else touching.map (·.kind.contactDamage) |>.getD 0
  let guide := { guide with attackCooldown := guide.attackCooldown - 1, hurtCooldown := guide.hurtCooldown - 1 }
  let damage := if guide.hurtCooldown == 0 then damageAfterDefense raw 15 else 0
  if guide.health ≤ damage then
    let slain := { next with
      townGuide := none, openGuide := false, guideRespawnPending := true
      status := "The Guide was slain." }
    return if inLava then slain.summonWall (bodyCenterX guide.body) (bodyCenterY guide.body) else slain
  let guide := if damage > 0 then { guide with health := guide.health - damage, hurtCooldown := guideHurtCooldown }
    else guide
  let talking := next.openGuide && next.canReach guide.body.centerTile
  let updated := { next with townGuide := some guide, openGuide := talking }
  if !mayShoot || talking || guide.attackCooldown > 0 || next.projectiles.size ≥ maxProjectiles then return updated
  let x := bodyCenterX guide.body
  let y := bodyCenterY guide.body
  let threat := enemies.find? fun enemy =>
    (enemy.kind == .zombie || enemy.kind == .demonEye ||
      (enemy.kind.isDaySlime && enemy.health < enemy.kind.maxHealth)) &&
    (bodyCenterX enemy.body - x).natAbs + (bodyCenterY enemy.body - y).natAbs ≤ 10 * tileUnits.toNat &&
    terrainLineClear updated.world x y (bodyCenterX enemy.body) (bodyCenterY enemy.body)
  let some enemy := threat | return updated
  let some arrow := aimShot .guideArrow x y (bodyCenterX enemy.body) (bodyCenterY enemy.body) | return updated
  return { updated with
    projectiles := updated.projectiles.push arrow
    townGuide := some { guide with attackCooldown := guideAttackCooldown } }

private def GameState.strikeEye (state : GameState) (damage : Nat) : GameState × Bool := Id.run do
  let some eye := state.eye | return (state, false)
  if eye.phase == .retreat then return (state, false)
  let reaches := fun target box => !state.npcImmune target && state.reachesNpcBox target box
  let chance := state.equipment.weaponArmorStats.criticalChance
  if let some servant := eye.servants.find? (fun servant => reaches (.eyeServant servant.id) servant.hitBox) then
    let target := NpcTarget.eyeServant servant.id
    let damaged := state.hurtEyeServant servant.id (state.armorCritical target damage chance maxProjectileSlots)
    return (damaged.applyMagmaStone target false, true)
  return if reaches .eye eye.hitBox then
      ((state.hurtEye (state.armorCritical .eye damage chance maxProjectileSlots)).applyMagmaStone .eye false, true)
    else (state, false)

private def GameState.strikeEvil (state : GameState) (damage : Nat) : GameState × Bool := Id.run do
  if state.eater.isNone && state.brain.isNone && state.evilEnemies.isEmpty &&
      state.skeletron.isNone && state.dungeonGuardian.isNone && state.dungeonSlimes.isEmpty &&
      state.darkCasters.isEmpty && state.waterSpheres.isEmpty && state.angryBones.isEmpty &&
      state.cursedSkulls.isEmpty && state.lavaSlimes.isEmpty && state.underworldFlyers.isEmpty &&
      state.fireImps.isEmpty && state.burningSpheres.isEmpty && state.boneSerpent.isNone &&
      state.underworldCritters.isEmpty && state.wallOfFlesh.isNone &&
      state.wallHungry.isEmpty && state.wallLeeches.isEmpty && state.wraiths.isEmpty &&
      state.hardmodeCavern.isEmpty && !state.twinsActive && state.destroyer.isNone && state.destroyerProbes.isEmpty &&
      state.skeletronPrime.isNone && state.plantera.isNone && state.planteraSpores.isEmpty &&
      state.golem.isNone && state.templeEnemies.isEmpty && state.queenSlime.isNone &&
      state.queenMinions.isEmpty then return (state, false)
  let mut next := state
  let mut landed := false
  let chance := state.equipment.weaponArmorStats.criticalChance
  let force : Float32 := if state.selectedStack.any (·.item == .batBat) then 5.5 else 5
  for (target, box) in state.projectileTargets do
    match target with
    | .eaterSegment _ | .brainCore | .brainCreeper _ | .evilFlyer _
    | .skeletron _ | .dungeonGuardian | .dungeonSlime _ | .darkCaster _ | .waterSphere _ | .angryBones _
    | .cursedSkull _ | .lavaSlime _ | .underworldFlyer _ | .fireImp _ | .burningSphere _ | .boneSerpent _ _
    | .wall _ | .wallHungry _ | .wallLeech _ _ | .wraith _ | .hardmodeCavern _ | .twin _
    | .destroyerSegment _ | .destroyerProbe _ | .queenSlime | .queenMinion _ | .primeHead | .primeArm _
    | .planteraHead | .planteraTentacle _ | .planteraSpore _
    | .golemBody | .golemHead | .golemLeftFist | .golemRightFist | .templeEnemy _ =>
      if !next.npcImmune target && state.reachesNpcBox target box then
        let impact := state.armorCritical target damage chance maxProjectileSlots
        next := (next.damageTarget target impact (some (state.combat.facing, force))).applyMagmaStone target false
        landed := true
    | .underworldCritter _ =>
      if !next.npcImmune target && state.reachesNpcBox target box then
        next := next.damageTarget target damage
    | _ => pure ()
  return (next, landed)

private def GameState.advanceEyeEncounter (state : GameState) : GameState := Id.run do
  let some eye := state.eye | return state
  let retreat := !isNight state.surface.clock false || state.combat.health == 0
  let some moved := advanceEye state.world state.player eye retreat
    | return { state with eye := none, status := "The Eye of Cthulhu withdrew." }
  let next := { state with eye := some moved }
  if moved.phase == .retreat then
    return if eye.phase == .retreat then next else { next with status := "The Eye of Cthulhu is retreating." }
  if state.combat.invulnerable > 0 then return next
  let contact := moved.contact state.player
  if contact == 0 then return next
  let damage := damageAfterDefense contact ((state.defense + 1) / 2)
  let combat := state.combat.hurt damage
  let status := if combat.health == 0 then "You were slain by the Eye of Cthulhu."
    else "The Eye of Cthulhu hit you."
  return { next with combat, status }

private def GameState.advanceEvilEncounters (state : GameState) : GameState := Id.run do
  if state.legacyEarly then return state
  let alive := state.combat.health > 0
  let position := state.player.centerTile
  let biome := state.inInstalledEvil
  let nearby := position.y ≤ 54 && inEvilBiome state.seed { position with y := min 48 position.y }
  let retreat := !alive || !(biome || nearby)
  let eater := state.eater.bind (fun boss => advanceEater state.world state.water state.player boss retreat)
  let brain := state.brain.bind (fun boss => advanceBrain state.world state.player boss retreat)
  let mut enemies := if alive then state.evilEnemies.filterMap (advanceEvilFlyer state.world state.player) else #[]
  let interval := if eater.isSome || brain.isSome then 720 else 300
  if alive && biome && enemies.size < maxEvilFlyers && state.surface.clock % interval == 0 then
    for id in [:maxEvilFlyers] do
      if enemies.any (·.id == id) then continue
      let kind := if altarTile state.seed == .crimsonAltar then EvilFlyerKind.crimera else .eaterOfSouls
      if let some flyer := spawnEvilFlyer state.world state.seed state.player id state.surface.clock kind then
        enemies := enemies.push flyer
      break
  let status := if state.eater.isSome && eater.isNone then "The Eater of Worlds withdrew."
    else if state.brain.isSome && brain.isNone then "The Brain of Cthulhu withdrew."
    else state.status
  let next := { state with eater, brain, evilEnemies := enemies, status }
  if !alive || state.combat.invulnerable > 0 then return next
  let wormContact := (eater.map (·.contact state.player)).getD 0
  let brainContact := (brain.map (·.contact state.player)).getD 0
  let flyer := enemies.find? (fun enemy => enemy.hitBox.overlapsPlayer state.player)
  let contact := max (max wormContact brainContact) (if flyer.isSome then 22 else 0)
  if contact == 0 then return next
  let damage := damageAfterDefense contact ((state.defense + 1) / 2)
  let name := if brainContact == contact then "Brain of Cthulhu"
    else if wormContact == contact then "Eater of Worlds"
    else (flyer.map (·.kind.name)).getD "evil flyer"
  return { next with combat := state.combat.hurt damage, status := s!"{name} hit you." }

private inductive ContactDebuff where
  | none | curse | fire | slow | weak
  deriving BEq

private def GameState.dungeonContact (state : GameState) : Option (GameState × Option Int) := do
  -- General immunity decreases before Player.Update_NPCCollision.
  if state.combat.health == 0 || state.combat.invulnerable > 1 then failure
  let hit : Option (Nat × Int × String × ContactDebuff) :=
    (state.skeletron.bind (·.contact state.player)).map fun (base, direction) =>
      (base, direction, "Skeletron hit you.", .none)
  let hit := hit.orElse fun _ => do
    let guardian ← state.dungeonGuardian
    if !(dungeonGuardianBox guardian).intersects (HitBox.ofNpcTarget state.player) then failure
    let direction := if guardian.x < bodyCenterX state.player then 1 else -1
    return (9999, direction, "You were slain by a Dungeon Guardian.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.dungeonSlimes.findSome? (·.contact state.player)
    return (dungeonSlimeDamage, direction, "Dungeon Slime hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.darkCasters.findSome? (·.contact state.player)
    return (darkCasterDamage, direction, "Dark Caster hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.waterSpheres.findSome? (·.contact state.player)
    return (waterSphereDamage, direction, "Water Sphere hit you.", .none)
  let hit := hit.orElse fun _ => do
    let (base, direction) ← state.angryBones.findSome? (fun bones =>
      (bones.contact state.player).map fun direction => (bones.kind.damage, direction))
    return (base, direction, "Angry Bones hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.cursedSkulls.findSome? (·.contact state.player)
    return (cursedSkullDamage, direction, "Cursed Skull hit you.", .curse)
  let hit := hit.orElse fun _ => do
    let direction ← state.lavaSlimes.findSome? (·.contact state.player)
    return (lavaSlimeDamage, direction, "Lava Slime hit you.", .none)
  let hit := hit.orElse fun _ => do
    let (kind, direction) ← state.underworldFlyers.findSome? (fun flyer =>
      (flyer.contact state.player).map (flyer.kind, ·))
    return (kind.damage, direction, s!"{kind.name} hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.fireImps.findSome? (·.contact state.player)
    return (fireImpDamage, direction, "Fire Imp hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.burningSpheres.findSome? (·.contact state.player)
    return (burningSphereDamage, direction, "Burning Sphere hit you.", .fire)
  let hit := hit.orElse fun _ => do
    let serpent ← state.boneSerpent
    let (damage, direction) ← serpent.contact state.player
    return (damage, direction, "Bone Serpent hit you.", .none)
  let hit := hit.orElse fun _ => do
    let wall ← state.wallOfFlesh
    let direction ← wall.contact state.player
    return (WallPart.damage .mouth, direction, "Wall of Flesh hit you.", .none)
  let hit := hit.orElse fun _ => do
    let box := HitBox.ofNpcTarget state.player
    let hungry ← state.wallHungry.find? (fun hungry => hungry.hitBox.intersects box)
    let direction := if hungry.hitBox.left < bodyCenterX state.player then 1 else -1
    return (hungry.damage state.wallOfFlesh, direction, "The Hungry hit you.", .none)
  let hit := hit.orElse fun _ => do
    let player := HitBox.ofNpcTarget state.player
    state.wallLeeches.findSome? fun leech => do
      let (part, box) ← leech.targets.find? (fun (_, box) => box.intersects player)
      let direction := if box.left < bodyCenterX state.player then 1 else -1
      return (leech.damage part, direction, "A Leech hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.wraiths.findSome? (·.contact state.player)
    return (wraithDamage, direction, "Wraith hit you.", .slow)
  let hit := hit.orElse fun _ => do
    let (kind, direction) ← state.hardmodeCavern.findSome? (fun enemy =>
      (enemy.contact state.player).map (enemy.kind, ·))
    return (kind.damage, direction, s!"{kind.name} hit you.", if kind == .floatyGross then .weak else .none)
  let hit := hit.orElse fun _ => do
    let (twin, direction) ← (state.retinazer.toArray ++ state.spazmatism.toArray).findSome? fun twin =>
      (twin.contact state.player).map (twin, ·)
    return (twin.contactDamage, direction, s!"{twin.kind.name} hit you.", .none)
  let side (box : HitBox) := if bodyCenterX state.player < (box.left + box.right) / 2 then -1 else 1
  let hit := hit.orElse fun _ => do
    let boss ← state.destroyer
    let (segment, part) ← boss.segments.zipIdx.find? (fun (segment, _) =>
      segment.hitBox.intersects (HitBox.ofNpcTarget state.player))
    return (destroyerContactDamage part, side segment.hitBox, "The Destroyer hit you.", .none)
  let hit := hit.orElse fun _ => do
    let probe ← state.destroyerProbes.find? (fun probe => probe.hitBox.intersects (HitBox.ofNpcTarget state.player))
    return (50, side probe.hitBox, "A Probe hit you.", .none)
  let hit := hit.orElse fun _ => do
    let head ← state.skeletronPrime
    let direction ← head.contact state.player
    return (head.contactDamage, direction, "Skeletron Prime hit you.", .none)
  let hit := hit.orElse fun _ => do
    let (kind, direction) ← state.primeArms.findSome? fun arm =>
      (arm.contact state.player).map (arm.kind, ·)
    return (kind.damage, direction, "A Skeletron Prime arm hit you.", .none)
  let hit := hit.orElse fun _ => do
    let head ← state.plantera
    let direction ← head.contact state.player
    return (head.contactDamage, direction, "Plantera hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.planteraHooks.findSome? (·.contact state.player)
    return (60, direction, "A Plantera hook hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.planteraTentacles.findSome? (·.contact state.player)
    return (60, direction, "A Plantera tentacle hit you.", .none)
  let hit := hit.orElse fun _ => do
    let direction ← state.planteraSpores.findSome? (·.contact state.player)
    return (70, direction, "A Plantera spore hit you.", .none)
  let hit := hit.orElse fun _ => do
    let body ← state.golem
    let direction ← body.contact state.player
    return (golemBodyDamage, direction, "Golem hit you.", .none)
  let hit := hit.orElse fun _ => do
    let (kind, direction) ← state.golemFists.findSome? fun fist =>
      (fist.contact state.player).map (fist.kind, ·)
    return (kind.damage, direction, "Golem's fist hit you.", .none)
  let hit := hit.orElse fun _ => do
    let head ← state.golemHead
    let direction ← head.contact state.player
    return (head.damage, direction, "Golem's head hit you.", .none)
  let hit := hit.orElse fun _ => do
    let (kind, direction) ← state.templeEnemies.findSome? (fun enemy =>
      (enemy.contact state.player).map (enemy.kind, ·))
    return (kind.damage, direction, s!"A {kind.name} hit you.", .none)
  let hit := hit.orElse fun _ => do
    let queen ← state.queenSlime.filter (·.timeLeft > 10)
    let direction ← queen.contact state.player
    return (60, direction, "Queen Slime hit you.", .none)
  let hit := hit.orElse fun _ => do
    let (kind, direction) ← state.queenMinions.findSome? fun slime =>
      (slime.contact state.player).map (slime.kind, ·)
    return (kind.damage, direction, "A Queen Slime minion hit you.", .none)
  let (base, direction, status, debuff) ← hit
  let roll := (hash (state.seed, state.surface.clock, state.player.x, state.player.y)).toNat
  let (damage, immunity) := classicNpcContactDamage base roll state.defense
  let sphereFire := debuff == .fire && state.obsidianSkinTicks == 0 &&
    (hash (state.seed, state.surface.clock, state.player.x, state.player.y, (25 : Nat))).toNat % 3 == 0
  let combat := { state.combat.hurt damage with
    invulnerable := immunity
    onFire := if sphereFire then max state.combat.onFire burningSphereFireTicks else state.combat.onFire }
  let curseRoll := (hash (state.seed, state.surface.clock, state.player.x, state.player.y, (23 : Nat))).toNat
  let cursedTicks := if debuff == .curse && combat.health > 0 &&
      state.equipment.accessory != some .nazar && curseRoll % 3 == 0 then maxCursedTicks
    else state.cursedTicks
  let knockback := if state.equipment.accessory == some .cobaltShield || state.ride.isSome then none
    else some direction
  let slowRoll := (hash (state.seed, state.surface.clock, state.player.x, state.player.y, (32 : Nat))).toNat
  let slowTicks := if debuff == .slow && combat.health > 0 &&
      state.equipment.accessory != some .fastClock && slowRoll % 8 == 0 then maxSlowTicks else state.slowTicks
  let weakRoll := (hash (state.seed, state.surface.clock, state.player.x, state.player.y, (33 : Nat))).toNat
  let weakTicks := if debuff == .weak && combat.health > 0 &&
      state.equipment.accessory != some .vitamins && weakRoll % 25 == 0 then maxWeakTicks else state.weakTicks
  return ({ state with combat, status, cursedTicks, slowTicks, weakTicks }, knockback)

/-- The normal Dungeon fighter fallback in `Spawner.SpawnAnNPC`. The three
armored-looking variants have priority over the size variants of type 31. -/
def selectAngryBonesKind (variantRoll bigRoll shortRoll : Nat) : AngryBonesKind :=
  match variantRoll % 5 with
  | 0 => .big
  | 1 => .bigMuscle
  | 2 => .bigHelmet
  | _ => if bigRoll % 4 == 0 then .bigBoned
    else if shortRoll % 5 == 0 then .short
    else .regular

private def GameState.dungeonEnemyFits (state : GameState) (box : HitBox) (cx cy : Float32) : Bool :=
  let x := box.left / tileUnits
  let y := box.top / tileUnits
  let columns := ((box.right - 1) / tileUnits - x + 1).toNat
  let rows := ((box.bottom - 1) / tileUnits - y + 1).toNat
  !(List.range columns).any (fun dx => (List.range rows).any fun dy =>
      (state.world.tile ⟨x + dx, y + dy⟩).solid) &&
    !state.water.submerged state.world (cx * 16).toInt32.toInt (cy * 16).toInt32.toInt

private def GameState.spawnAngryBones (state : GameState) (kind : AngryBonesKind)
    (unsafeWall : TilePos → Bool) (roll : Nat) : Option AngryBones :=
  findDungeonSpawnFloor state.world state.seed state.player.centerTile unsafeWall roll
    (lowerDungeon := state.lowerDungeonInstalled) (blueMoonDungeon := state.blueMoonDungeonInstalled)
    (magicMissileDungeon := state.magicMissileDungeonInstalled) (deepDungeon := state.deepDungeonInstalled) fun column floor => do
    let bones := AngryBones.atFloor state.nextAngryBonesId kind column floor
    if !state.dungeonEnemyFits bones.hitBox bones.centerX bones.centerY then failure
    return bones

private def GameState.spawnCursedSkull (state : GameState) (unsafeWall : TilePos → Bool)
    (roll : Nat) : Option CursedSkull :=
  findDungeonSpawnFloor state.world state.seed state.player.centerTile unsafeWall roll
    (lowerDungeon := state.lowerDungeonInstalled) (blueMoonDungeon := state.blueMoonDungeonInstalled)
    (magicMissileDungeon := state.magicMissileDungeonInstalled) (deepDungeon := state.deepDungeonInstalled) fun column floor => do
    let skull := CursedSkull.atFloor state.nextCursedSkullId column floor
    if !state.dungeonEnemyFits skull.hitBox skull.centerX skull.centerY then failure
    return skull

private def GameState.advanceDungeonEnemies (state : GameState) (daylight : Bool) : GameState := Id.run do
  let dead := state.combat.health == 0
  let surface := (dungeonLayout state.seed).surface
  let slimes := state.dungeonSlimes.filterMap fun slime =>
    slime.advance state.world state.water state.player dead daylight surface
  let skulls := state.cursedSkulls.filterMap (·.advance state.player dead)
  let clock := state.surface.clock
  let roll := fun (salt : Nat) => (hash (state.seed, clock, salt)).toNat
  let lowerDungeon := state.lowerDungeonInstalled
  let blueMoonDungeon := state.blueMoonDungeonInstalled
  let magicMissileDungeon := state.magicMissileDungeonInstalled
  let deepDungeon := state.deepDungeonInstalled
  let unsafeWall := fun p => dungeonUnsafeWall state.seed p lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon &&
    !hasWoodWall state.world state.walls p
  let mut casters := #[]
  let mut spheres := state.waterSpheres.filterMap (·.advance state.player)
  let mut sphereId := state.nextWaterSphereId
  for caster in state.darkCasters do
    let (active, shot) := caster.advance state.world state.water state.magma state.player
      dead state.skeletron.isSome surface unsafeWall (fun draw => roll (2000 + 200 * caster.id + draw))
    if let some active := active then casters := casters.push active
    if let some fired := shot then
      if spheres.size < maxWaterSpheres then
        let sphere := WaterSphere.fromCaster sphereId fired state.player
        if let some active := sphere.advance state.player then
          spheres := spheres.push active
          sphereId := sphereId + 1
  let mut world := state.world
  let mut skeletons := #[]
  for bones in state.angryBones do
    let (active, changed) := bones.advance world state.water state.player dead surface (daylight := daylight)
    world := changed
    if let some active := active then skeletons := skeletons.push active
  let next := { state with
    world, dungeonSlimes := slimes, darkCasters := casters
    waterSpheres := spheres, nextWaterSphereId := sphereId, angryBones := skeletons
    cursedSkulls := skulls }
  let slots := 8 * (slimes.size + casters.size) + cursedSkullSlotsQuarters * skulls.size +
    skeletons.foldl (fun total bones => total + 4 * bones.kind.npcSlots) 0
  let cap := if daylight then 36 else 40
  if dead || !state.skeletronDefeated || slots ≥ cap then return next
  if roll 171 % dungeonSpawnRate daylight slots != 0 then return next
  let position := state.player.centerTile
  if !unsafeWall position ||
      !inDungeon state.seed world position lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon then
    return next
  if roll 172 % 35 == 0 then
    for id in [:maxDungeonSlimes] do
      if slimes.any (·.id == id) then continue
      let some slime := spawnDungeonSlime world state.water state.seed state.player id (roll 173)
          lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon
        | return next
      let some active := slime.advance world state.water state.player false daylight surface
        | return next
      return { next with dungeonSlimes := (slimes.push active).qsort (fun a b => a.id < b.id) }
    return next
  -- The regular Dungeon wall's Cursed Skull roll precedes Dark Caster.
  if roll 177 % 7 == 0 then
    if skulls.size ≥ maxCursedSkulls then return next
    let some skull := next.spawnCursedSkull unsafeWall (roll 173) | return next
    let some active := skull.advance state.player false | return next
    return { next with cursedSkulls := skulls.push active, nextCursedSkullId := skull.id + 1 }
  if roll 176 % 7 == 0 then
    if casters.size ≥ maxDarkCasters then return next
    let id := state.nextDarkCasterId
    let some caster := spawnDarkCaster world state.water state.seed state.player unsafeWall id (roll 173)
        lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon
      | return next
    let (active, _) := caster.advance world state.water state.magma state.player
      false false surface unsafeWall roll
    if let some active := active then
      return { next with darkCasters := casters.push active, nextDarkCasterId := id + 1 }
    return next
  if skeletons.size < maxAngryBones then
    let kind := selectAngryBonesKind (roll 178) (roll 179) (roll 180)
    if let some bones := next.spawnAngryBones kind unsafeWall (roll 173) then
      let (active, changed) := bones.advance world state.water state.player false surface (daylight := daylight)
      if let some active := active then
        return { next with world := changed, angryBones := skeletons.push active, nextAngryBonesId := bones.id + 1 }
  return next

private def GameState.advanceDungeon (state : GameState) (daylight : Bool) : GameState := Id.run do
  if !state.dungeonInstalled then return state
  let state := state.advanceDungeonEnemies daylight
  if state.skeletronDefeated then return state
  let alive := state.combat.health > 0
  let oldManWaiting := state.oldManWaiting || state.surface.clock == oldManReturnTick
  let skeletron := state.skeletron.bind (fun boss =>
    advanceSkeletron state.player boss daylight !alive)
  let position := state.player.centerTile
  let mut dungeonGuardian := none
  let mut dungeonGuardianHealth := 0
  let mut status := if state.skeletron.isSome && skeletron.isNone then "Skeletron withdrew."
    else state.status
  if alive then
    if let some motion := state.dungeonGuardian then
      let dx := bodyCenterX state.player - motion.x
      let dy := bodyCenterY state.player - motion.y
      if dx.natAbs ≤ 125 * tileUnits.toNat && dy.natAbs ≤ 125 * tileUnits.toNat then
        let (vx, vy) := (aimVelocity dungeonGuardianSpeed dx dy).getD (0, 0)
        let moved : EyeMotion := { x := motion.x + vx, y := motion.y + vy, vx, vy }
        if moved.valid state.world dungeonGuardianSpeed then
          dungeonGuardian := some moved
          dungeonGuardianHealth := state.dungeonGuardianHealth
    else if !hasWoodWall state.world state.walls position &&
        inDungeon state.seed state.world position state.lowerDungeonInstalled state.blueMoonDungeonInstalled
          state.magicMissileDungeonInstalled state.deepDungeonInstalled then
      let x := bodyCenterX state.player + 18 * tileUnits
      let y := bodyCenterY state.player + 8 * tileUnits
      dungeonGuardian := some { x, y, vx := 0, vy := 0 }
      dungeonGuardianHealth := dungeonGuardianMaxHealth
      status := "A Dungeon Guardian is approaching!"
  return { state with
    oldManWaiting, skeletron, dungeonGuardian, dungeonGuardianHealth, status }

/-- Normal Underworld population bands from Spawner.GetSpawnRate.
NPC slot counts are measured in quarters. -/
def underworldSpawnRate (slots : Nat) : Nat :=
  if slots < 8 then 360 else if slots < 16 then 420 else if slots < 24 then 480
  else if slots < 32 then 540 else 600

private inductive UnderworldSpawn where
  | slime | flyer (kind : UnderworldFlyerKind) | imp | serpent | critters

private def selectUnderworldSpawn (serpentActive : Bool) (roll : Nat → Nat) : UnderworldSpawn :=
  if roll 0 % 8 == 0 then .critters
  else if roll 1 % 40 == 0 && !serpentActive then .serpent
  else if roll 2 % 14 == 0 then .imp
  else if roll 3 % 7 == 0 then .flyer (if roll 4 % 10 == 0 then .voodooDemon else .demon)
  else if roll 5 % 3 == 0 then .slime
  else .flyer .hellbat

private def GameState.findUnderworldFloor (state : GameState) (roll : Nat → Nat)
    (candidate : Int → Int → Option α) : Option α := Id.run do
  let center := state.player.centerTile
  let world := state.world
  for attempt in [:50] do
    let column := center.x + (roll (2 * attempt) % 75 : Nat) - 37
    let top := max (underworldStart + 1 : Int) (center.y + (roll (2 * attempt + 1) % 43 : Nat) - 21)
    if column < 2 || column ≥ (world.width : Int) - 2 || (world.tile ⟨column, top⟩).solid then continue
    for y in [top.toNat + 1:world.height - 1] do
      let floor : Int := y
      if !(world.tile ⟨column, floor⟩).solid then continue
      if floor > underworldStart + 10 &&
          ((column - center.x).natAbs ≥ 26 || (floor - center.y).natAbs ≥ 18) &&
          state.magma.level world ⟨column, floor - 1⟩ == 0 &&
          state.water.level world ⟨column, floor - 1⟩ == 0 &&
          !hasWoodWall world state.walls ⟨column, floor - 1⟩ then
        if let some spawned := candidate column floor then return some spawned
      break
  return none

private def GameState.spawnLavaCritters (state : GameState) (column floor : Int)
    (roll : Nat → Nat) : GameState := Id.run do
  let daylight := !isNight state.surface.clock state.legacyEarly
  let mut critters := state.underworldCritters
  let mut nextId := state.nextUnderworldNpcId
  let mut candidates : Array (UnderworldCritterKind × Int × Int) := #[]
  if roll 0 % 3 == 0 then candidates := #[(.magmaSnail, column, floor)]
  else if daylight then candidates := #[(.hellButterfly, column, floor)]
  else
    -- NPC.setFireFlyChance sets the multiple-spawn denominator for the night.
    let nightRoll (draw : Nat) := (hash (state.seed, (654 : Nat), draw)).toNat
    let multiple := if nightRoll 0 % 9 == 0 then 3 + nightRoll 1 % 5
      else if nightRoll 2 % 3 == 0 then 999999 else 6 + nightRoll 3 % 24
    let offsets : Array (Int × Int) := #[(-1, 0), (1, 0), (0, -1), (0, 1)]
    for i in [:offsets.size] do
      if roll (i + 1) % multiple == 0 then
        let (dx, dy) := offsets[i]!
        candidates := candidates.push (.lavafly, column + dx, floor + dy)
    candidates := candidates.push (.lavafly, column, floor)
  for (kind, x, y) in candidates do
    if critters.size ≥ maxUnderworldCritters then break
    critters := critters.push (UnderworldCritter.atFloor nextId kind x y)
    nextId := nextId + 1
  return { state with underworldCritters := critters, nextUnderworldNpcId := nextId }

private def GameState.spawnUnderworld (state : GameState) (kind : UnderworldSpawn)
    (roll : Nat → Nat) : GameState :=
  let id := state.nextUnderworldNpcId
  match kind with
  | .slime =>
    match state.findUnderworldFloor roll (fun x y =>
      let slime := LavaSlime.atFloor id x y
      if slime.clear state.world then some slime else none) with
    | none => state
    | some slime => { state with lavaSlimes := state.lavaSlimes.push slime, nextUnderworldNpcId := id + 1 }
  | .flyer kind =>
    match state.findUnderworldFloor roll (fun x y =>
      let flyer := UnderworldFlyer.atFloor id kind x y
      if flyer.clear state.world then some flyer else none) with
    | none => state
    | some flyer =>
      { state with underworldFlyers := state.underworldFlyers.push flyer, nextUnderworldNpcId := id + 1 }
  | .imp =>
    match state.findUnderworldFloor roll (FireImp.spawnAt state.world state.water id) with
    | none => state
    | some imp => { state with fireImps := state.fireImps.push imp, nextUnderworldNpcId := id + 1 }
  | .serpent =>
    match state.findUnderworldFloor roll (fun x y => some (BoneSerpent.atFloor id x y (roll 101))) with
    | none => state
    | some serpent => { state with boneSerpent := some serpent, nextUnderworldNpcId := id + 1 }
  | .critters =>
    match state.findUnderworldFloor roll (fun x y => some (x, y)) with
    | none => state
    | some (x, y) => state.spawnLavaCritters x y (fun draw => roll (101 + draw))

private def GameState.demonVolley (state : GameState) (flyer : UnderworldFlyer)
    (roll : Nat → Nat) (clearSight : Bool) : UnderworldFlyer × Option DemonSickle :=
  if flyer.kind == .hellbat then (flyer, none) else
  let clock := min 16777216 (flyer.attackClock + 1)
  let dx := (Int32.ofInt (bodyCenterX state.player)).toFloat32 / 16 - flyer.centerX
  let dy := (Int32.ofInt (bodyCenterY state.player)).toFloat32 / 16 - flyer.centerY
  if clock == 20 || clock == 40 || clock == 60 || clock == 80 then
    let dx := dx + (Int32.ofInt ((roll 0 % 201 : Nat) - (100 : Int))).toFloat32
    let dy := dy + (Int32.ofInt ((roll 1 % 201 : Nat) - (100 : Int))).toFloat32
    ({ flyer with attackClock := clock }, if clearSight then
      some (DemonSickle.fire flyer.centerX flyer.centerY dx dy flyer.direction true) else none)
  else
    let restart := dx.abs < 910 && dy.abs < 550 && clock ≥ 300 + roll 2 % 300
    ({ flyer with attackClock := if restart then 0 else clock }, none)

private def GameState.advanceUnderworld (initial : GameState) : GameState := Id.run do
  if initial.world.height != underworldHeight then return initial
  let player := initial.player
  let dead := initial.combat.health == 0
  let roll (id draw : Nat) := (hash (initial.seed, initial.surface.clock, id, draw, (60 : Nat))).toNat
  let surface := groundHeight initial.seed 19
  let daylight := !isNight initial.surface.clock initial.legacyEarly
  let lavaSlimes := initial.lavaSlimes.filterMap fun slime =>
    slime.advance initial.world initial.water initial.magma player dead (daylight := daylight) (surface := surface)
  let mut state := { initial with
    lavaSlimes, underworldFlyers := #[], fireImps := #[]
    boneSerpent := initial.boneSerpent.bind (·.advance initial.world initial.water initial.magma player dead)
    burningSpheres := initial.burningSpheres.filterMap (·.advance player) }
  for flyer in initial.underworldFlyers do
    let sight := flyer.canSee state.world player
    let steered := flyer.advanceAI player dead sight (underworldTargetWet state.world state.water state.magma player)
    let (steered, shot) := state.demonVolley steered (roll flyer.id) sight
    if let some active := steered.advanceMotion state.world state.water state.magma player then
      state := { state with underworldFlyers := state.underworldFlyers.push active }
    if let some shot := shot then
      let slot := firstFreeProjectileSlot state.projectileSlots
      if slot < maxProjectileSlots then
        state := { state with
          demonSickles := state.demonSickles.push { shot with slot := some slot }, underworldNpcSound := true }
  for imp in initial.fireImps do
    let (active, shot) := imp.advance state.world state.water state.magma player dead surface
      (roll imp.id)
    if let some active := active then state := { state with fireImps := state.fireImps.push active }
    if imp.destination.isSome then state := { state with underworldNpcSound := true }
    if let some castingImp := shot then
      if state.burningSpheres.size < maxBurningSpheres then
        let sphere := BurningSphere.fromImp state.nextUnderworldNpcId castingImp player
        if let some sphere := sphere.advance player then
          state := { state with
            burningSpheres := state.burningSpheres.push sphere
            nextUnderworldNpcId := state.nextUnderworldNpcId + 1
            underworldNpcSound := true }
  let threats := if initial.underworldCritters.any (·.kind == .hellButterfly) then
      (state.projectileTargets.filter (fun (target, _) =>
        match target with | .underworldCritter _ => false | _ => true)).map Prod.snd
    else #[]
  let critters := initial.underworldCritters.filterMap fun critter =>
    critter.advance state.world state.water state.magma player dead (roll critter.id) (threats := threats)
  state := { state with underworldCritters := critters }
  if dead || player.centerTile.y ≤ underworldStart then return state
  let hostileSlots := 4 * state.lavaSlimes.size + 12 * state.fireImps.size +
    state.underworldFlyers.foldl (fun total flyer => total + flyer.kind.slotQuarters) 0 +
    (if state.boneSerpent.isSome then boneSerpentSlotQuarters else 0)
  let critterSlots := critters.foldl (fun total critter =>
    total + (match critter.kind with | .hellButterfly => 5 | .lavafly => 4 | .magmaSnail => 10)) 0
  let slots := hostileSlots + critterSlots / 5
  if slots ≥ 40 || roll 0 0 % underworldSpawnRate slots != 0 then return state
  let kind := selectUnderworldSpawn state.boneSerpent.isSome (fun draw => roll 0 (1 + draw))
  return state.spawnUnderworld kind (fun draw => roll 0 (10 + draw))

private def GameState.advanceWall (initial : GameState) : GameState := Id.run do
  if initial.wallOfFlesh.isNone && initial.wallHungry.isEmpty && initial.wallLeeches.isEmpty then return initial
  let dead := initial.combat.health == 0
  let roll := fun (draw : Nat) => (hash (initial.seed, initial.surface.clock, draw, (113 : Nat))).toNat
  let advanced := initial.wallOfFlesh.bind (fun wall =>
    wall.advance initial.world initial.water initial.magma initial.player dead roll)
  let wallOfFlesh := advanced.map Prod.fst
  let event := (advanced.map Prod.snd).getD {}
  let mut state := { initial with wallOfFlesh, wallRoared := event.roared }
  if let some (x, y) := event.hungrySpawn then
    for index in [:11] do
      if state.wallHungry.size == maxWallHungry then break
      let id := state.nextUnderworldNpcId
      state := { state with nextUnderworldNpcId := id + 1, wallHungry := state.wallHungry.push (WallHungry.at id index x y) }
  if let some (x, y, vx) := event.leechSpawn then
    if state.wallLeeches.size < maxWallLeeches then
      let id := state.nextUnderworldNpcId
      let leech := WallLeech.at id x y vx (roll id)
      state := { state with nextUnderworldNpcId := id + 1, wallLeeches := state.wallLeeches.push leech }
  let hungry := state.wallHungry.filterMap (·.advance state.world wallOfFlesh state.player dead)
  let leeches := state.wallLeeches.filterMap (·.advance state.world state.water state.magma wallOfFlesh state.player dead)
  state := { state with wallHungry := hungry, wallLeeches := leeches }
  for laser in event.lasers do
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot < maxProjectileSlots then
      state := { state with wallLasers := state.wallLasers.push { laser with slot := some slot } }
  if wallOfFlesh.isNone then
    if initial.horrified && !dead && initial.wallOfFlesh.isSome then
      return { state with
        horrified := false, tongued := false, combat := state.combat.afterDefeat
        status := "You were slain by the Wall of Flesh." }
    return { state with horrified := false, tongued := false }
  return state

private def GameState.advanceWraiths (state : GameState) : GameState :=
  if !state.hardmode then state
  else
    let dead := state.combat.health == 0
    let wraiths := state.wraiths.filterMap (·.advance state.world state.water state.magma state.player dead)
    let next := { state with wraiths }
    let surface := (groundHeight state.seed 19 : Int)
    let roll := (hash (state.seed, state.surface.clock, (8294 : Nat))).toNat
    if !dead && isNight state.surface.clock false && state.player.centerTile.y ≤ surface &&
        wraiths.size < 3 && roll % 1800 == 0 then next.spawnWraith roll false
    else next

private def GameState.findCavernEnemy (state : GameState) (hallow : Bool) (roll : Nat → Nat) :
    Option CavernEnemy := Id.run do
  let center := state.player.centerTile
  let world := state.world
  let rock := world.height / 2
  let id := state.nextHardmodeCavernId
  for attempt in [:50] do
    let column := center.x + (roll (2 * attempt) % 75 : Nat) - 37
    let top := max (rock : Int) (center.y + (roll (2 * attempt + 1) % 43 : Nat) - 21)
    if column < 2 || column ≥ (world.width : Int) - 2 || (world.tile ⟨column, top⟩).solid then continue
    for row in [top.toNat + 1:min underworldStart (world.height - 2)] do
      let floor : Int := row
      let tile := world.tile ⟨column, floor⟩
      if !tile.solid then continue
      let air : TilePos := ⟨column, floor - 1⟩
      if ((column - center.x).natAbs ≥ 26 || (floor - center.y).natAbs ≥ 18) &&
          state.water.level world air == 0 && state.magma.level world air == 0 &&
          !hasWoodWall world state.walls air then
        let biome := tile.hardmodeBiome
        let choice := roll (102 + attempt)
        let kind : Option CavernEnemyKind :=
          if biome == some .crimson then
            if choice % 5 == 0 then some .floatyGross else none
          else if biome == some .corruption then
            if choice % 3 == 0 then some .clinger else none
          else if hallow then
            some (if choice % 2 == 0 then .illuminantSlime else .illuminantBat)
          else none
        if let some kind := kind then
          let enemy := CavernEnemy.atFloor id kind column floor
          if enemy.valid world && (kind == .clinger || enemy.clear world) &&
              !state.hardmodeCavern.any (·.hitBox.intersects enemy.hitBox) then return some enemy
      break
  return none

private def GameState.advanceHardmodeCavern (initial : GameState) : GameState := Id.run do
  if !initial.hardmode then return initial
  let dead := initial.combat.health == 0
  let surface := groundHeight initial.seed 19
  let roll (id draw : Nat) := (hash (initial.seed, initial.surface.clock, id, draw, (512 : Nat))).toNat
  let mut state := { initial with hardmodeCavern := #[] }
  for enemy in initial.hardmodeCavern do
    let (active, shot) := enemy.advance state.world state.water state.magma state.player dead surface (roll enemy.id)
    if let some enemy := active then state := { state with hardmodeCavern := state.hardmodeCavern.push enemy }
    if let some shot := shot then
      let slot := firstFreeProjectileSlot state.projectileSlots
      if slot < maxProjectileSlots && state.cavernFlames.size < maxCavernFlames then
        state := { state with cavernFlames := state.cavernFlames.push { shot with slot := some slot } }
  let center := state.player.centerTile
  if dead || center.y.toNat < state.world.height / 2 || center.y.toNat ≥ underworldStart ||
      state.hardmodeCavern.size ≥ 8 then
    return state
  -- Desktop's 600-tick base, 0.9 Hardmode, 0.4 Cavern, and 0.65 infected biome rate.
  let population := if state.hardmodeCavern.size < 2 then 7 else if state.hardmodeCavern.size < 4 then 9 else 10
  let sample := roll 0 100
  let infectedRoll := sample % (140 * population / 10) == 0
  let ordinaryRoll := sample % (216 * population / 10) == 0
  if !infectedRoll && !ordinaryRoll then return state
  let (hallow, corrupt, crimson) := state.world.hardmodeZones center
  if !(if hallow || corrupt || crimson then infectedRoll else ordinaryRoll) then return state
  let some enemy := state.findCavernEnemy hallow (roll 0) | return state
  return { state with hardmodeCavern := state.hardmodeCavern.push enemy, nextHardmodeCavernId := enemy.id + 1 }

private def GameState.tickCursedInferno (state : GameState) : GameState :=
  if state.cursedInfernoTicks == 0 || state.combat.health == 0 then state
  else
    let ticks := state.cursedInfernoTicks - 1
    let progress := state.cursedInfernoProgress + 1
    let combat := { state.combat with
      health := state.combat.health - progress / 5, regenTime := 0, regenProgress := 0 }
    { state with
      combat, cursedInfernoTicks := ticks, cursedInfernoProgress := if ticks > 0 then progress % 5 else 0
      status := if combat.health == 0 then "You burned to death from Cursed Inferno." else state.status }

private def GameState.tickPoison (state : GameState) : GameState :=
  if state.poisonedTicks == 0 || state.combat.health == 0 then state
  else
    let poisonedTicks := state.poisonedTicks - 1
    let progress := state.poisonProgress + 1
    let combat := { state.combat with
      health := state.combat.health - progress / 30, regenTime := 0, regenProgress := 0 }
    { state with
      combat, poisonedTicks
      poisonProgress := if poisonedTicks == 0 then 0 else progress % 30
      status := if combat.health == 0 then "You died from poison." else state.status }

private def GameState.advancePlanteraBulbs (initial : GameState) : GameState := Id.run do
  if !initial.canGrowPlanteraBulbs then return initial
  let surface := groundHeight initial.seed 19
  let roll (draw : Nat) := (hash (initial.seed, initial.surface.clock, (238 : Nat), draw)).toNat
  if !initial.planteraUnlocked then
    let unlocked := { initial with planteraUnlocked := true, status := "The jungle grows restless..." }
    let dry (position : TilePos) :=
      initial.water.level initial.world position == 0 && initial.magma.level initial.world position == 0 &&
        initial.honey.level initial.world position == 0
    if let some (world, bulb) := firstPlanteraBulb initial.world surface roll dry then
      return { unlocked with world, planteraBulbs := #[bulb] }
    return unlocked
  let mut state := initial
  for bulb in initial.planteraBulbs do
    let cells := planteraBulbCells bulb
    if !planteraBulbValid state.world bulb || cells.any (fun cell => state.magma.level state.world cell > 0) then
      state := state.breakPlanteraBulb bulb
  -- WorldGen.UpdateWorld_Inner uses this density for underground tile updates.
  let updates := (state.world.width * state.world.height * 15 + 999999) / 1000000
  let growthRoll (draw : Nat) :=
    (hash (state.seed, state.weather.day, state.surface.clock, (238 : Nat), draw)).toNat
  for draw in [:updates] do
    if let some (world, bulb) := growPlanteraBulb state.world state.water state.magma state.honey
        surface state.planteraBulbs (fun offset => growthRoll (4 * draw + offset)) then
      state := { state with world, planteraBulbs := state.planteraBulbs.push bulb }
  return state

private def GameState.hasBossEncounter (state : GameState) : Bool :=
  state.eye.isSome || state.eater.isSome || state.brain.isSome || state.skeletron.isSome ||
    state.wallOfFlesh.isSome || state.twinsActive || state.destroyer.isSome ||
    state.skeletronPrime.isSome || state.plantera.isSome || state.queenSlime.isSome

private def GameState.scheduleMechanicalBosses (state : GameState) : GameState :=
  if !isNight state.surface.clock state.legacyEarly then
    { state with twinsPending := false, destroyerPending := false, primePending := false }
  else if state.surface.clock == classicDayTicks && state.hardmode && state.altarsSmashed > 0 then
    let dusk := { state with mechanicalNights := state.mechanicalNights + 1 }
    let roll (draw : Nat) := (hash (state.seed, dusk.mechanicalNights, (97 : Nat), draw)).toNat
    if state.hasBossEncounter || roll 0 % 10 != 0 then dusk
    else
      let candidates := #[0, 1, 2].filter fun option =>
        (option != 0 || !state.destroyerDefeated) && (option != 1 || !state.twinsDefeated) &&
          (option != 2 || !state.primeDefeated)
      match candidates[roll 1 % candidates.size]? with
      | some 0 => { dusk with destroyerPending := true, status := "You feel vibrations from deep below..." }
      | some 1 => { dusk with twinsPending := true, status := "This is going to be a terrible night..." }
      | some 2 => { dusk with primePending := true, status := "The air is getting colder around you..." }
      | _ => dusk
  else if (state.twinsPending || state.destroyerPending || state.primePending) &&
      state.surface.clock > classicDayTicks + 4860 then
    let ready := { state with twinsPending := false, destroyerPending := false, primePending := false }
    if ready.combat.health > 0 && !ready.hasBossEncounter &&
        ready.player.y / tileUnits < (groundHeight ready.seed 19 : Int) then
      if state.destroyerPending then ready.spawnDestroyer
      else if state.primePending then ready.spawnSkeletronPrime
      else ready.spawnTwins
    else ready
  else state

private def GameState.launchMechanicalShot (state : GameState) (attack : TwinAttack) : GameState := Id.run do
  if state.twinShots.size ≥ maxTwinShots then return state
  let slot := firstFreeProjectileSlot state.projectileSlots
  if slot ≥ maxProjectileSlots then return state
  let shot := TwinShot.fire attack slot
  if !shot.valid state.world then return state
  return { state with
    twinShots := state.twinShots.push shot
    twinsShotSound := state.twinsShotSound || (attack.kind.isLaser && attack.kind != .probeLaser)
    probeShotSound := state.probeShotSound || attack.kind == .probeLaser }

private def GameState.advanceDestroyer (initial : GameState) : GameState := Id.run do
  let dead := initial.combat.health == 0
  let daylight := !isNight initial.surface.clock initial.legacyEarly
  let mut state := initial
  if let some boss := state.destroyer then
    let roll (part draw : Nat) := (hash (state.seed, state.surface.clock, part, (134 : Nat), draw)).toNat
    let surface := groundHeight state.seed 19
    match boss.advance state.world state.water state.magma state.honey state.player surface (daylight || dead) roll with
    | some (moved, shots) =>
      state := { state with destroyer := some moved, destroyerBurrowed := moved.burrowSoundDelay > 0 &&
        boss.burrowSoundDelay ≤ 1 }
      for attack in shots do state := state.launchMechanicalShot attack
    | none => state := { state with destroyer := none, status := "The Destroyer withdrew." }
  let mut probes := #[]
  for probe in state.destroyerProbes do
    let (moved, shot) := probe.advance state.world state.player state.destroyer.isSome dead
    probes := probes ++ moved.toArray
    if let some attack := shot then state := state.launchMechanicalShot attack
  return { state with destroyerProbes := probes }

private def GameState.launchPrimeShot (state : GameState) (attack : PrimeAttack) : GameState := Id.run do
  let slot := firstFreeProjectileSlot state.projectileSlots
  if slot ≥ maxProjectileSlots then return state
  let shot := PrimeShot.fire attack slot
  if !shot.valid state.world then return state
  return { state with primeShots := state.primeShots.push shot }

private def GameState.advanceSkeletronPrime (initial : GameState) : GameState := Id.run do
  let some head := initial.skeletronPrime | return initial
  let dead := initial.combat.health == 0
  let daylight := !isNight initial.surface.clock initial.legacyEarly
  let some moved := head.advance initial.world initial.player daylight dead
    | return { initial with skeletronPrime := none, primeArms := #[], status := "Skeletron Prime withdrew." }
  let arms := if !head.initialized && moved.initialized then
      PrimeArmKind.all.map (fun kind => PrimeArm.atHead kind head)
    else initial.primeArms
  let enraged := moved.phase == PrimePhase.guardian && head.phase != PrimePhase.guardian
  let roar := (moved.phase == PrimePhase.spin && moved.timer == 2) || enraged
  let mut state := { initial with
    skeletronPrime := some moved
    primeRoared := initial.primeRoared || roar
    status := if enraged then "Skeletron Prime is enraged." else initial.status }
  let mut nextArms := #[]
  for arm in arms do
    let roll (draw : Nat) := (hash (state.seed, state.surface.clock, arm.kind.id, draw)).toNat
    let (arm, shot) := arm.advance moved state.player dead roll
    if arm.valid state.world then nextArms := nextArms.push arm
    if let some attack := shot then state := state.launchPrimeShot attack
  return { state with primeArms := nextArms }

private def GameState.advancePlantera (initial : GameState) : GameState := Id.run do
  let mut state : GameState := { initial with
    planteraSpores := initial.planteraSpores.filterMap (fun spore => spore.advance initial.world initial.player) }
  let some head := state.plantera | return state
  let surface := groundHeight state.seed 19
  let center := state.player.centerTile
  let jungle := (surface : Int) ≤ center.y && center.y < (underworldStart : Int) &&
    (state.fishingBiomes center surface).jungle
  let dead := state.combat.health == 0
  let origin : TilePos :=
    ⟨max 1 (min (state.world.width - 1 : Nat) ((head.x.toInt32.toInt + 43) / 16)),
      max 1 (min (state.world.height - 1 : Nat) ((head.y.toInt32.toInt + 43) / 16))⟩
  let visible := head.needsLineOfSight jungle dead &&
    canHitTiles state.world origin center
  let roll (actor draw : Nat) := (hash (state.seed, state.surface.clock, (262 : Nat), actor, draw)).toNat
  let anchors := state.planteraHooks.map fun hook => (hook.centerX, hook.centerY)
  let some frame := head.advance state.world state.player anchors jungle dead visible (roll 0)
    | return { state with plantera := none, planteraHooks := #[], planteraTentacles := #[], status := "Plantera withdrew." }
  let moved := frame.head
  let mut hooks : Array PlanteraHook :=
    if frame.spawnHooks then Array.replicate 3 (PlanteraHook.atHead head.centerX head.centerY)
    else state.planteraHooks
  for index in [:hooks.size] do
    let count := (hooks.zipIdx.filter (fun (hook, i) => i != index && hook.moving)).size
    let hook := hooks[index]!.advance state.world (hasWoodWall state.world state.walls)
      state.player moved.x moved.y moved.health count moved.enraged dead (roll (index + 1))
    hooks := hooks.set! index hook
  let tentacles : Array PlanteraTentacle := if frame.spawnTentacles then
      (Array.range 8).map (fun index => PlanteraTentacle.atHead (state.nextPlanteraPartId + index) head.centerX head.centerY)
    else state.planteraTentacles
  state := { state with
    plantera := some moved
    planteraRoared := frame.spawnTentacles || state.planteraRoared
    planteraHooks := hooks.filter (·.valid state.world)
    nextPlanteraPartId := state.nextPlanteraPartId + if frame.spawnTentacles then 8 else 0
    planteraTentacles := tentacles.filterMap (fun part =>
      part.advance state.world state.player moved.centerX moved.centerY moved.health (roll (part.id + 4))) }
  if let some launch := frame.spore then
    let spore := PlanteraSpore.atHead state.nextPlanteraPartId launch.x launch.y launch.vx launch.vy
    if state.planteraSpores.size < 200 && spore.valid state.world then
      state := { state with
        planteraSpores := state.planteraSpores.push spore
        nextPlanteraPartId := state.nextPlanteraPartId + 1 }
  if let some attack := frame.projectile then
    let slot := firstFreeProjectileSlot state.projectileSlots
    let shot := PlanteraShot.fire attack slot
    if slot < maxProjectileSlots && shot.valid state.world then
      state := { state with planteraShots := state.planteraShots.push shot }
  return state

private def GameState.advanceGolem (initial : GameState) : GameState := Id.run do
  let some previous := initial.golem | return initial
  let world := initial.world
  let player := initial.player
  let center := player.centerTile
  let surface := groundHeight initial.seed 19
  let temple := TempleLayout.hasNaturalWall world initial.walls center
  let jungle := temple || (initial.fishingBiomes center surface).jungle
  let left := initial.golemFists.any (·.kind == .left)
  let right := initial.golemFists.any (·.kind == .right)
  let attached := initial.golemHead.any (!·.detached)
  let some frame := previous.advance world player attached left right jungle (center.y < surface)
      (initial.combat.health == 0) (previous.canSee world player) surface.toFloat
    | return { initial with golem := none, golemHead := none, golemFists := #[], status := "Golem withdrew." }
  let body := frame.body
  let mut state := { initial with golem := some body, golemLanded := frame.landed }
  if frame.spawnParts then
    let (_, _, (x, y)) := previous.partSpawnBottoms
    state := { state with
      golemHead := some (GolemHead.atBottom x y)
      golemFists := #[GolemFist.spawn .left previous.centerX previous.centerY,
        GolemFist.spawn .right previous.centerX previous.centerY] }
  let mut fists := #[]
  for fist in state.golemFists do
    let (next, sound) := fist.advance world body.centerX body.centerY body.vx body.vy
      (body.health < golemBodyHealth) player body.enraged
    fists := fists.push next
    state := { state with golemPunchSound := state.golemPunchSound || sound }
  state := { state with golemFists := fists }
  if let some head := state.golemHead then
    let roll (draw : Nat) := (hash (state.seed, state.surface.clock, head.sprite, state.weather.day, draw)).toNat
    let sample (draw : Nat) :=
      (Int32.ofNat (roll draw % 16777216)).toFloat32 / 16777216
    let result := head.advance world player body.centerX body.centerY body.health body.enraged roll sample
    state := { state with golemHead := some result.head }
    for attack in result.attacks do
      let slot := firstFreeProjectileSlot state.projectileSlots
      if slot ≥ maxProjectileSlots then break
      let shot := GolemShot.fire attack slot
      if shot.valid world then state := { state with golemShots := state.golemShots.push shot }
  return state

private def GameState.advanceTempleEnemies (initial : GameState) : GameState := Id.run do
  let dead := initial.combat.health == 0
  let surface := groundHeight initial.seed 19
  let daylight := !isNight initial.surface.clock initial.legacyEarly
  let player := initial.player
  let center := player.centerTile
  let mut state := { initial with templeEnemies := #[] }
  for enemy in initial.templeEnemies do
    let (active, world) := enemy.advance state.world state.water state.magma player dead daylight surface
    state := { state with world, templeEnemies := state.templeEnemies ++ active.toArray }
  if dead || !TempleLayout.hasNaturalWall state.world state.walls center then return state
  let population := state.templeEnemies.foldl (fun slots enemy => slots + enemy.kind.halfSlots) 0
  let cavern := center.y ≥ (state.world.height / 2 : Nat)
  let deep := 2 * center.y > (surface + state.world.height / 2 : Nat)
  let sample (draw : Nat) := (hash (state.seed, state.surface.clock, draw, (198 : Nat))).toNat
  let (regularRate, regularCap) := templeSpawnRate state.hardmode cavern deep false population
  let (jungleRate, jungleCap) := templeSpawnRate state.hardmode cavern deep true population
  let regular := population < regularCap && sample 0 % regularRate == 0
  let inJungle := population < jungleCap && sample 0 % jungleRate == 0
  if !regular && !inJungle then return state
  if !(if (state.fishingBiomes center surface).jungle then inJungle else regular) then return state
  let some enemy := findTempleSpawn state.world state.water state.magma state.honey player
      state.templeEnemies state.nextTempleEnemyId (fun i => sample (i + 1)) | return state
  return { state with templeEnemies := state.templeEnemies.push enemy, nextTempleEnemyId := enemy.id + 1 }

private def GameState.tickTempleCooldowns (state : GameState) : GameState :=
  { state with
    templeCooldowns := state.templeCooldowns.filterMap fun (p, ticks) =>
      if ticks > 1 then some (p, ticks - 1) else none
    spikeCooldown := state.spikeCooldown - 1 }

private def GameState.checkWoodenSpikes (state : GameState) : GameState := Id.run do
  if state.combat.health == 0 || state.spikeCooldown > 0 then return state
  let body := HitBox.ofPlayer state.player
  for x in [((body.left - 1) / tileUnits).toNat:((body.right + 1) / tileUnits).toNat + 1] do
    for y in [((body.top - 8) / tileUnits).toNat:((body.bottom + 8) / tileUnits).toNat + 1] do
      if state.world.tile ⟨x, y⟩ != .woodenSpikes then continue
      let roll (draw : Nat) := (hash (state.seed, state.surface.clock, x, y, draw)).toNat
      let (damage, immunity) := classicNpcContactDamage 80 (roll 0) state.defense
      let combat := { state.combat.hurt damage with invulnerable := state.combat.invulnerable }
      return { state with
        combat, spikeCooldown := immunity
        bleedingTicks := max state.bleedingTicks (600 + roll 1 % 600)
        status := "Wooden Spikes cut you." }
  return state

private def GameState.tripTemplePlate (state : GameState) (previous : Player) : GameState := Id.run do
  if state.combat.health == 0 || state.templeWires.size == 0 || previous == state.player then return state
  let px (value : Int) := OriginalCollision.single value / 16
  let boost := if state.wingedSlime.isSome then wingedSlimeHeightBoost else 0
  let old : OriginalCollision.Vector := ⟨px previous.x, px (previous.y - boost)⟩
  let current : OriginalCollision.Vector := ⟨px state.player.x, px (state.player.y - boost)⟩
  let box := HitBox.ofPlayer state.player
  let mut plate := none
  for x in [(box.left / tileUnits).toNat:(box.right / tileUnits).toNat + 1] do
    for y in [(box.top / tileUnits).toNat:(box.bottom / tileUnits).toNat + 1] do
      let p : TilePos := ⟨x, y⟩
      if state.world.tile p == .lihzahrdPressurePlate &&
          TempleTraps.plateTriggered p old current (playerWidth.toNat / 16) ((playerHeight + boost).toNat / 16) then
        plate := some p
        break
    if plate.isSome then break
  let some triggered := plate | return state
  let mut next := state
  for (source, trap) in TempleWiring.trip state.world state.templeWires triggered do
    if next.templeCooldowns.any (·.1 == source) then continue
    next := { next with templeCooldowns := next.templeCooldowns.push (source, trap.kind.cooldown) }
    let slot := firstFreeProjectileSlot next.projectileSlots
    if slot ≥ maxProjectileSlots then continue
    let rng := UnifiedRandom.fromEntropy (hash (state.seed, state.surface.clock, source, state.weather.day))
    let shot := TempleTraps.launch trap.kind source trap.frameX
      (fun lo hi => (rng.next lo hi).1) (fun lo hi => (rng.nextInt.2.next lo hi).1)
    next := { next with templeTrapShots := next.templeTrapShots.push { shot with slot := some slot } }
  return next

private def GameState.tickTempleDebuffs (state : GameState) (wet : Bool) : GameState :=
  let duration := if wet then 0 else state.hellfireTicks
  let progress := if duration == 0 then 0 else state.hellfireProgress + 1
  let health := state.combat.health - progress / 15
  { state with
    combat := { state.combat with health }
    hellfireTicks := duration - 1
    hellfireProgress := if duration ≤ 1 then 0 else progress % 15
    bleedingTicks := state.bleedingTicks - 1 }

private def GameState.advanceTwins (initial : GameState) : GameState := Id.run do
  let initial := initial.scheduleMechanicalBosses
  let mut state := initial
  let daylight := !isNight state.surface.clock state.legacyEarly
  let dead := state.combat.health == 0
  for kind in [TwinKind.retinazer, .spazmatism] do
    let (actor, partner) := if kind == .retinazer then (state.retinazer, state.spazmatism)
      else (state.spazmatism, state.retinazer)
    let some twin := actor | continue
    let npcType := twin.kind.sprite
    let roll (draw : Nat) := (hash (state.seed, state.surface.clock, npcType, draw)).toNat
    let (moved, shots) := twin.advance state.world state.player daylight dead partner.isSome roll
      (partner.map (·.timeLeft) |>.getD 0)
    state := state.setTwin kind moved
    if let some moved := moved then
      let roar := (moved.secondForm && twin.transforming) ||
        (kind == .spazmatism && twin.powered && twin.ai1 == 1 && moved.ai1 == 2)
      let flameSound := kind == .spazmatism && twin.localAI2 == 22 && moved.localAI2 == 0
      state := { state with
        twinsRoared := state.twinsRoared || roar
        twinsFlameSound := state.twinsFlameSound || flameSound }
    for shot in shots do state := state.launchMechanicalShot shot
  if initial.twinsActive && !state.twinsActive then
    return { state with status := "The Twins withdrew." }
  return state

private def GameState.launchQueenShots (initial : GameState) (shots : Array QueenShot) : GameState := Id.run do
  let mut state := initial
  for shot in shots do
    if state.queenShots.size ≥ maxQueenShots then break
    let slot := firstFreeProjectileSlot state.projectileSlots
    if slot ≥ maxProjectileSlots then break
    let launched := { shot with slot := some slot }
    if launched.valid state.world then state := { state with queenShots := state.queenShots.push launched }
  return state

private def GameState.advanceQueenSlime (initial : GameState) : GameState := Id.run do
  let mut state := initial
  let dead := state.combat.health == 0
  let mut minions := #[]
  for minion in state.queenMinions do
    let (moved, shot) := minion.advance state.world state.water state.magma state.player dead
    minions := minions ++ moved.toArray
    state := state.launchQueenShots shot.toArray
  state := { state with queenMinions := minions }
  let some queen := state.queenSlime | return state
  let roll (draw : Nat) := (hash (state.seed, state.surface.clock, (657 : Nat), draw)).toNat
  let (moved, shots) := queen.advance state.world state.water state.magma state.player dead roll
  let some moved := moved | return { state with queenSlime := none, status := "Queen Slime withdrew." }
  let (queen, spawned) := moved.spawnMinions state.nextQueenMinionId (fun draw => roll (draw + 5))
  let available := maxQueenMinions - minions.size
  return { state.launchQueenShots shots with
    queenSlime := some queen
    queenMinions := minions ++ spawned.extract 0 available
    nextQueenMinionId := state.nextQueenMinionId + min available spawned.size }

private def GameState.advanceWeather (state : GameState) (clock : Nat) (player : Player) : Weather :=
  if state.legacyEarly then state.weather
  else
    let surfaceRow : Int := groundHeight state.seed player.centerTile.x.toNat
    let surfaceVisible := player.centerTile.y ≤ surfaceRow + 24
    let roll (draw : Nat) := (hash (state.seed, state.weather.day, clock, (3964 : Nat), draw)).toNat
    state.weather.advance (clock == 0) (state.combat.healthLimit ≥ 120) surfaceVisible roll

private def GameState.advanceDead (state : GameState) : GameState :=
  let (_, _, surface, _) :=
    advanceSurface state.world state.seed state.player state.combat state.surface none state.defense
      (!state.legacyEarly) none state.guideProtectsTown
  let merchantStock := if surface.clock == 0 then initialMerchantStockFor state.legacyEarly else state.merchantStock
  let weather := state.advanceWeather surface.clock state.player
  let next := ({ state with surface, weather, merchantStock } : GameState).advanceAngler (surface.clock == 0)
  let (flowing, cooled) :=
    next.tickTempleCooldowns.tickNpcBurns.tickNpcImmunity.advanceProjectiles.advanceLiquids
  let (world, crops) := advanceGarden flowing.world flowing.water flowing.crops (!isNight surface.clock state.legacyEarly)
  let (spread, spreadGrass) := ({ flowing with world } : GameState).advanceEvilGrass
  let encounters := ({ flowing with world := spread, crops, respawnTicks := state.respawnTicks - 1 } : GameState).advanceGuide false
    |>.advanceEyeEncounter
    |>.advanceEvilEncounters
  let dungeon := (encounters.advanceDungeon (!isNight state.surface.clock false)).advanceUnderworld.advanceWall.advanceWraiths
    |>.advanceHardmodeCavern.advanceTwins.advanceDestroyer.advanceSkeletronPrime.advancePlantera.advanceGolem.advanceTempleEnemies
    |>.advancePlanteraBulbs.advanceQueenSlime.advanceVoodooDolls
  let dungeonEdit := (!encounters.angryBones.isEmpty || !encounters.templeEnemies.isEmpty) &&
    dungeon.world.tiles != encounters.world.tiles
  let missileEdit := (!state.magicMissiles.isEmpty || !state.flamelashes.isEmpty || !state.flowerFireballs.isEmpty ||
      !state.hellwingShots.isEmpty || state.demonSickles.any (!·.hostile) ||
      state.projectiles.any (·.kind == .handgunBullet)) &&
    flowing.world.tiles != state.world.tiles
  let wallEdit := state.wallOfFlesh.isSome && dungeon.wallOfFlesh.isNone && flowing.world.tiles != state.world.tiles
  let next := dungeon.pruneNpcImmunity.updateExploration state {}
    (dungeonEdit || missileEdit || wallEdit || cooled || spreadGrass || crops.size < flowing.crops.size ||
      flowing.bombFlash.any (·.2 == 12))
  if next.respawnTicks > 0 then next
  else
    let bed := next.bedArrival
    let player := bed.getD (respawn next.world next.player next.combat.crawler)
    ({ next with
      player
      surface := { next.surface with
        creature := next.surface.creature.filter (·.staysNear next.seed player)
        flying := next.surface.flying.filter (·.staysNear next.seed player) }
      combat := next.combat.afterRespawn
      mana := baseMana
      breath := maxBreath
      chart := next.chart.reveal next.world player.centerTile
      status := if bed.isSome then "Respawned at your bed." else "Respawned at the world spawn point." } : GameState).pruneNpcImmunity

private def GameState.stompWingedSlime (state : GameState) : GameState := Id.run do
  let some mount := state.wingedSlime | return state
  if mount.vy ≤ 0 || mount.wetSlime > 0 then return state
  let pixel := tileUnits / 16
  let x := state.player.x.tdiv pixel * pixel
  let feet := (state.player.y - wingedSlimeHeightBoost).tdiv pixel * pixel +
    playerHeight + wingedSlimeHeightBoost
  let stomp : HitBox := ⟨x - 12 * pixel, feet - 7 * pixel,
    x + playerWidth / pixel * pixel + 12 * pixel, feet + 7 * pixel⟩
  let sourceX := bodyCenterX state.player
  let sourceY := bodyCenterY state.player - wingedSlimeHeightBoost / 2
  for (target, box) in state.projectileTargets do
    if state.npcImmune target || !stomp.intersects box then continue
    if let .underworldCritter _ := target then continue
    let targetX := (box.left + box.right) / 2
    let targetY := (box.top + box.bottom) / 2
    if !target.ignoresMeleeTerrain &&
        (terrainHit state.world sourceX sourceY (targetX - sourceX) (targetY - sourceY)).isSome then
      continue
    let side := if mount.vx < 0 then -1 else if mount.vx > 0 then 1 else state.combat.facing
    let damage := 40 * (100 + state.equipment.summonDamageBonus) / 100
    let state := (state.damageTarget target damage (some (side, 5))).grantNpcImmunity target (npcHitCooldown + 1)
    let (mount, immune) := mount.stomp
    return { state with
      wingedSlime := some mount
      player := { state.player with vy := -160, grounded := false }
      combat := { state.combat with invulnerable := if immune then max 7 state.combat.invulnerable
        else state.combat.invulnerable } }
  return state

def step (initial : GameState) (input : TickInput) : GameState := Id.run do
  let state : GameState := { initial.assignProjectileSlots.wallPlayerStatus with
    magicMissileSound := false, valorCast := false, handgunFired := false, underworldSwing := false
    underworldMagicCast := false, underworldMagicImpact := false, hellwingFired := false, underworldNpcSound := false
    wallRoared := false, wallDefeatedNow := false
    twinsRoared := false, twinSlain := false, twinsShotSound := false, twinsFlameSound := false
    destroyerRoared := false, destroyerBurrowed := false, destroyerKilled := false
    destroyerHitSound := false, probeShotSound := false, probeKilled := false
    primeRoared := false, primeHitSound := false, primeDestroyedPart := false
    primeBombLit := false, primeBombExploded := false, primeLaserFired := false, primeLaserImpact := false
    planteraRoared := false, planteraHitSound := false, planteraKilledSound := false
    planteraShotSound := false, planteraShotImpact := false
    golemLanded := false, golemPunchSound := false, golemFireballSound := false
    golemBeamSound := false, golemImpactSound := false, templeDartSound := false, templeFlameSound := false
    templeEnemySound := none }
  if state.respawnTicks > 0 then return state.advanceDead
  -- Player.UpdateBuffs sets noItems before contact, even on the last buff tick.
  let itemsBlocked := state.cursedTicks > 0 || state.tongued
  let stationary := (state.player.vx == 0 && state.player.vy == 0) || state.grapple.attachment.isSome
  let (mana, manaRegenDelay, manaRegenProgress) :=
    regenerateMana state.mana state.manaRegenDelay state.manaRegenProgress stationary
  let modal := state.openChest.isSome || state.openShop || state.openGuide || state.openOldMan || state.openAngler
  let state := if state.tongued || input.cancelMount then state.dismountWingedSlime
    else if !modal && input.quickMount && state.wingedSlime.isSome then state.dismountWingedSlime
    else if !modal && input.quickMount && !itemsBlocked && state.hasPillion then state.mountWingedSlime
    else state
  let state := if input.cancelDagger then { state with dagger := none } else state
  let ticking := { state with
    cursedTicks := state.cursedTicks - 1, darknessTicks := state.darknessTicks - 1, slowTicks := state.slowTicks - 1
    weakTicks := state.weakTicks - 1
    obsidianSkinTicks := state.obsidianSkinTicks - 1
    mana, manaRegenDelay, manaRegenProgress }
  -- The player checks contact before moving and before this frame's NPC AI.
  let ticking := ticking.tickTempleCooldowns.checkWoodenSpikes
  let ticking := if ticking.combat.health > 0 then ticking.stompWingedSlime else ticking
  let contact := ticking.dungeonContact
  let (contacted, knockback) := contact.getD (ticking, none)
  let alive := contacted.combat.health > 0
  let input : TickInput := if alive then input else {}
  let input := if itemsBlocked then
      { input with
        mine := none, place := none, useHeld := false, dropVoodooDoll := false
        quickGrapple := none, minionAim := none }
    else input
  let equipped := if modal then contacted else contacted.changeEquipment input
  let equipped := if equipped.equipment.accessory == some .nazar then { equipped with cursedTicks := 0 }
    else if equipped.equipment.accessory == some .fastClock then { equipped with slowTicks := 0 }
    else if equipped.equipment.accessory == some .vitamins then { equipped with weakTicks := 0 }
    else equipped
  let horizontal := if modal then 0 else input.horizontal
  let equipped := if alive && !modal && !state.tongued && equipped.ride.isNone then
    equipped.updateTempleDoors horizontal else equipped
  let moving := if !alive then equipped
    else if state.tongued then equipped.moveTongued
    else equipped.move input horizontal modal knockback
  let moved := { moving.tripTemplePlate state.player with
    bowCooldown := state.bowCooldown - 1
    toolCooldown := state.toolCooldown - 1
    potionSickness := state.potionSickness - 1
    gills := state.gills - 1
    cratePotion := state.cratePotion - 1
    fishingPotion := state.fishingPotion - 1
    sonarPotion := state.sonarPotion - 1
    barkskin := state.barkskin - 1 }
  let aquaWasActive := moved.aquaAnimation > 0
  let aim := input.aim.orElse (fun _ => input.mine.map WorldAim.atTile)
  let moved := (moved.advanceAquaUse aim (!modal && alive && !itemsBlocked)).captureWeaponArmor moved
  let blueMoon := if itemsBlocked || input.cancelFlail then none else
    moved.blueMoon.map fun attack => { attack with aim := (aim.filter (WorldAim.valid moved.world)).getD attack.aim }
  let sunfury := if itemsBlocked || input.cancelFlail then none else
    moved.sunfury.map fun attack => { attack with aim := (aim.filter (WorldAim.valid moved.world)).getD attack.aim }
  let magicMissiles := if let some aim := aim.filter (WorldAim.valid moved.world) then
      moved.magicMissiles.map (fun attack => if attack.missile.guided then { attack with aim } else attack)
    else moved.magicMissiles
  let flamelashes := if let some aim := aim.filter (WorldAim.valid moved.world) then
      moved.flamelashes.map (fun attack => if attack.projectile.missile.guided then { attack with aim } else attack)
    else moved.flamelashes
  let valor := moved.valor.map fun attack => { attack with aim := (aim.filter (WorldAim.valid moved.world)).getD attack.aim }
  let moved := ({ moved with blueMoon, sunfury, magicMissiles, flamelashes, valor } : GameState).pruneWeaponArmor
  let (next, attackAt, damage, horn) := match state.openChest with
    | some position => (moved.handleChest position input, none, 0, false)
    | none => if state.openShop then (moved.handleShop input, none, 0, false)
      else if state.openGuide then ({ moved with openGuide := !input.closeGuide }, none, 0, false)
      else if state.openAngler then (moved.handleAngler input, none, 0, false)
      else if state.openOldMan then (moved.handleOldMan input, none, 0, false)
      else moved.applyActions input aquaWasActive
  let next := next.captureWeaponArmor moved
  let weapon := next.selectedStack.map (·.item)
  let shortsword := weapon == some .copperShortsword
  let stats := next.equipment.weaponArmorStats
  let meleeDamage := stats.scaleDamage damage
  let impact := if shortsword then damage
    else next.armorCritical .crawler meleeDamage stats.criticalChance maxProjectileSlots
  let delay := if next.legacyEarly then swingCooldown else (weapon.map Item.swordUseTime).getD swingCooldown
  let delay := next.equipment.meleeTicks delay
  let delay := if next.weakTicks > 0 then (1000 * delay + 948) / 949 else delay
  let (player, combat, event) :=
    advanceCombat next.world next.player next.combat horizontal attackAt impact delay shortsword contact.isSome
      (!shortsword && !next.npcImmune .crawler)
  let status := event.map (fun value => combatMessage value next.status) |>.getD next.status
  -- The shared combat step consumes the swing and sets its facing before the Warden checks reach.
  let strike := if event == some .swing && !shortsword then some impact else none
  let (next, combat) := if shortsword && next.combat.swing ≤ 1 && next.shortsword.isNone then
      match attackAt with
      | none => (next, combat)
      | some tile =>
        let aim := (input.aim.filter (WorldAim.valid next.world)).getD (WorldAim.atTile tile)
        let facing := if aim.x ≤ bodyCenterX player then -1 else 1
        let thrust := ShortswordThrust.launch player aim.x aim.y facing damage
        let thrust := { thrust with slot := some (firstFreeProjectileSlot next.projectileSlots) }
        (({ next with shortsword := some thrust } : GameState).captureWeaponArmor next, { combat with facing })
    else (next, combat)
  let (player, combat, warden, wardenEvent) := advanceWarden next.world player combat next.warden
    (strike.filter fun _ => !next.npcImmune .warden)
  let status := wardenEvent.map wardenMessage |>.getD status
  let surfaceStrike := if wardenEvent == some .hit || wardenEvent == some .defeated then none else strike
  let (player, combat, surface, surfaceEvent) :=
    advanceSurface next.world next.seed player combat next.surface (surfaceStrike.map (fun _ => meleeDamage)) next.defense
      (!next.legacyEarly) none next.guideProtectsTown (fun kind => !next.npcImmune (.surface kind))
      (fun kind damage => next.armorCritical (.surface kind) damage stats.criticalChance maxProjectileSlots)
  let phaseStatus := if !next.legacyEarly && surface.clock == 0 then "Dawn breaks across the surface."
    else if !next.legacyEarly && surface.clock == classicDayTicks then "Night has fallen."
    else status
  let status := surfaceEvent.map surfaceMessage |>.getD phaseStatus
  let quarryStrike := match surfaceEvent with
    | some (.hit _) | some (.defeated _) => none
    | _ => surfaceStrike
  let (player, combat, quarry, quarryEvent) :=
    advanceQuarry next.world player combat next.quarry (quarryStrike.filter fun _ => !next.npcImmune .quarry)
  let status := quarryEvent.map quarryMessage |>.getD status
  let previousSurface := next.surface
  let weather := next.advanceWeather surface.clock player
  let next := ({ next with surface, weather } : GameState).advanceAngler (surface.clock == 0)
  let next := match surfaceEvent with
    | some (.defeated kind) =>
      let defeated := if kind == SurfaceKind.demonEye then previousSurface.flying else previousSurface.creature
      (defeated.map next.rewardSurfaceDefeat).getD next
    | _ => next
  let earnedCoins :=
    (if event == some .defeatedCrawler then 2 else 0) +
    (if quarryEvent == some .defeated then 4 else 0)
  let merchantStock := if surface.clock == 0 then initialMerchantStockFor next.legacyEarly else next.merchantStock
  let mut next := { next.creditCoins earnedCoins with player, combat, warden, surface, quarry, merchantStock, status }
  let strike := if quarryEvent == some .hit || quarryEvent == some .defeated then none else quarryStrike
  let mut sentryHit := false
  if let some damage := strike then
    if !next.npcImmune .sentry &&
        next.sentry.creature.any (fun enemy => swordReaches next.world player enemy.body combat.facing true) then
      next := next.damageTarget .sentry damage
      sentryHit := true
  let (sentry, sentryShot) := advanceSentry next.world player next.sentry
  let summon := !modal && input.interact.any (fun pos => next.canReach pos &&
    next.world.tile pos == .stormAltar)
  let (storm, stormShots, stormEvent) :=
    advanceStorm next.world player combat next.storm summon
      (strike.filter fun _ => !sentryHit && !next.npcImmune .herald)
  let status := (stormEvent.map stormMessage).getD next.status
  let raidStrike := if sentryHit || stormEvent == some .hit || stormEvent == some .defeated then none else strike
  let raidWave := match next.raid with | .active wave => wave.number | _ => 0
  let (player, combat, raid, raidShots, raidEvent) :=
    advanceRaid next.world next.seed player combat next.raid horn next.defense raidStrike
      (fun kind => !next.npcImmune (.raider raidWave kind))
  let status := (raidEvent.map raidMessage).getD status
  let caveStrike := if raidEvent == some .hit || raidEvent == some .defeated || raidEvent == some .nextWave then
      none else raidStrike
  let (player, combat, ecology, caveEvent) :=
    advanceCaveEcology next.world player combat next.ecology next.defense caveStrike
      (fun index => !next.npcImmune (.cave index))
  let status := (caveEvent.map caveMessage).getD status
  let ruinStrike := if caveEvent == some .hit || caveEvent == some .defeated then none else caveStrike
  let summonRuin := !modal && input.interact.any (fun pos => next.canReach pos &&
    next.world.tile pos == .ruinAltar)
  let (ruin, ruinShots, ruinEvent) := advanceRuin next.world player combat next.ruin summonRuin
    (ruinStrike.filter fun _ => !next.npcImmune .sentinel)
  let status := (ruinEvent.map ruinMessage).getD status
  let cavernStrike := if ruinEvent == some .hit || ruinEvent == some .defeated then none else ruinStrike
  let summonCavern := !modal && input.interact.any (fun pos => next.canReach pos &&
    next.world.tile pos == .cavernAltar)
  let (cavern, cavernShots, cavernEvent) :=
    advanceCavern next.world player combat next.cavern summonCavern
      (cavernStrike.filter fun _ => !next.npcImmune .colossus)
  let status := (cavernEvent.map cavernMessage).getD status
  let shots := sentryShot.toArray ++ stormShots ++ raidShots ++ ruinShots ++ cavernShots
  let projectiles := next.projectiles ++ shots.extract 0 (maxProjectiles - next.projectiles.size)
  next := { next with sentry, storm, player, combat, raid, ruin, cavern, ecology, projectiles, status }
  let mut meleeHit := event == some .hitCrawler || event == some .defeatedCrawler ||
    wardenEvent == some .hit || wardenEvent == some .defeated || quarryEvent == some .hit || quarryEvent == some .defeated ||
    stormEvent == some .hit || stormEvent == some .defeated ||
    raidEvent == some .hit || raidEvent == some .defeated || raidEvent == some .nextWave ||
    caveEvent == some .hit || caveEvent == some .defeated || ruinEvent == some .hit || ruinEvent == some .defeated ||
    cavernEvent == some .hit || cavernEvent == some .defeated ||
    sentryHit || (match surfaceEvent with | some (.hit _) | some (.defeated _) => true | _ => false)
  if next.combat.health > 0 && event == some .swing && !shortsword then
    let (eye, eyeHit) := next.strikeEye meleeDamage
    let (struck, evilHit) := eye.strikeEvil meleeDamage
    next := struck
    meleeHit := meleeHit || eyeHit || evilHit
  if weapon == some .batBat && meleeHit && next.combat.health > 0 then
    next := { next with combat := { next.combat with health := min next.combat.healthLimit (next.combat.health + 1) } }
  let encounters := next.advanceEyeEncounter.advanceEvilEncounters
  next := (encounters.advanceDungeon (!isNight state.surface.clock false)).advanceUnderworld.advanceWall.advanceWraiths
    |>.advanceHardmodeCavern.advanceTwins.advanceDestroyer.advanceSkeletronPrime.advancePlantera.advanceGolem
    |>.advanceTempleEnemies.advanceQueenSlime
  if !modal && !itemsBlocked then next := next.advanceNetSwing
  else next := { next with netSwingTicks := 0 }
  let dungeonEdit := (!encounters.angryBones.isEmpty || !encounters.templeEnemies.isEmpty) &&
    next.world.tiles != encounters.world.tiles
  let stepped := (next.advanceGuide (next.combat.health > 0)).tickNpcBurns.tickNpcImmunity.pruneNpcImmunity
    |>.advanceProjectiles (input.useHeld && !modal && !itemsBlocked)
  let missileEdit := (!next.magicMissiles.isEmpty || !next.flamelashes.isEmpty || !next.flowerFireballs.isEmpty ||
      !next.hellwingShots.isEmpty || next.demonSickles.any (!·.hostile) || next.valor.isSome ||
      next.projectiles.any (·.kind == .handgunBullet)) &&
    stepped.world.tiles != next.world.tiles
  -- The persisted clock also preserves the cadence and parity of the water steps.
  let (flowing, cooled) := stepped.advanceLiquids
  let flowing := flowing.advanceVoodooDolls
  let player := flowing.player
  let honeyWet := flowing.honey.wetsPlayer flowing.world player
  let isWet := (Magma.touches flowing.water flowing.world player || honeyWet) &&
    !Magma.touches flowing.magma flowing.world player
  let honeyBuff := if flowing.combat.health == 0 then 0
    else if honeyWet then honeyBuffDuration else flowing.honeyBuff - 1
  let resting := player.vx == 0 || flowing.grapple.attachment.isSome
  let stopsRegen := flowing.poisonedTicks > 0 || flowing.bleedingTicks > 0 || (!isWet && flowing.hellfireTicks > 0)
  let combat := if stopsRegen && flowing.combat.onFire == 0 then
      { flowing.combat with regenTime := 0, regenProgress := 0 }
    else flowing.combat.tickLife resting isWet (honeyBuff > 0)
  let status := if combat.health == 0 && flowing.combat.health > 0 then "You burned to death."
    else flowing.status
  let (warmed, _) := ({ flowing with combat, honeyBuff, status } : GameState)
    |>.tickTempleDebuffs isWet |>.tickCursedInferno |>.tickPoison |>.updateMagmaDamage
  let (breathing, _) := warmed.updateBurning.updateBreath
  let (world, crops) := advanceGarden breathing.world breathing.water breathing.crops
    (!isNight surface.clock state.legacyEarly)
  let (spread, spreadGrass) := ({ breathing with world } : GameState).advanceEvilGrass
  let wallEdit := state.wallOfFlesh.isSome && flowing.wallOfFlesh.isNone && flowing.world.tiles != state.world.tiles
  let altarEdit := state.altarsSmashed != breathing.altarsSmashed
  next := ({ breathing with world := spread, crops } : GameState).advancePlanteraBulbs
  let ambientEdit := dungeonEdit || missileEdit || wallEdit || altarEdit || cooled || spreadGrass || crops.size < breathing.crops.size ||
    flowing.bombFlash.any (·.2 == 12) || next.planteraBulbs != state.planteraBulbs
  if next.shadowKeyRecovery then
    if let some (inventory, _) := Inventory.addOne next.inventory .shadowKey then
      next := { next with inventory, shadowKeyRecovery := false, status := "Recovered your Shadow Key." }
  if next.combat.health == 0 then
    return next.beginDefeat.pruneWeaponArmor.pruneNpcImmunity.updateExploration state input ambientEdit
  if next.openChest.any (fun position => !next.canReachChest position) then
    next := { next with openChest := none }
  if next.openShop && !(next.merchant.any next.canReach) then
    next := { next with openShop := false }
  if let .reward position := state.warden then
    let (collected, taken) := next.collectDrop position .frostbrand "Frostbrand equipped. Three damage per strike."
    next := if taken then { collected.creditCoins 8 with warden := .cleared } else collected
  if let .reward position := state.storm then
    let (collected, taken) := next.collectDrop position .stormBow "Storm Bow equipped. Four damage per arrow."
    next := if taken then { collected.creditCoins 12 with storm := .cleared } else collected
  if let .reward position := state.raid then
    let (collected, taken) := next.collectDrop position .raidCrest "Raid Crest collected. Equip it from the backpack."
    next := if taken then { collected.creditCoins 20 with raid := .cleared } else collected
  if let .reward position := state.ruin then
    let (collected, taken) := next.collectDrop position .ruinHammer "Ruin Hammer equipped. Five damage per strike."
    next := if taken then { collected.creditCoins 16 with ruin := .cleared } else collected
  if let .reward position := state.cavern then
    let (collected, taken) := next.collectDrop position .deepstoneAegis
      "Deepstone Aegis collected. Equip it from the backpack for two defense."
    next := if taken then { collected.creditCoins 24 with cavern := .cleared } else collected
  next := next.collectCaveLoot state.ecology
  if next.grapple != .idle && next.world.tiles != state.world.tiles &&
      !next.grapple.valid next.world next.player then
    next := { next with grapple := .idle, status := grappleMessage .detached }
  next := next.collectGel.collectNightLoot.tickFishing.pruneNpcImmunity.pruneDaggerTargets
  if next.ride.isSome || next.grapple != .idle || next.tongued || next.wingedSlime.isSome ||
      !next.equipment.crystalAssassinSet then
    next := { next with assassinDash := {} }
  if let some mount := next.wingedSlime then
    if next.player.collides next.world wingedSlimeHeightBoost then
      next := next.dismountWingedSlime
    else if next.player.vx != moved.player.vx || next.player.vy != moved.player.vy then
      let motion := WingedSlime.start next.player
      next := { next with wingedSlime := some { mount with
        vx := motion.vx, vy := motion.vy, remainderX := 0, remainderY := 0 } }
  if state.tongued && !next.tongued then next := { next with player := next.wallReleasePosition }
  return next.updateExploration state input ambientEdit

end Lean4Terria
