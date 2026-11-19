import Lean4Terria.Inventory

namespace Lean4Terria

structure ShopOffer where
  item : Item
  quantity : Nat
  price : Nat
  limit : Nat
  deriving BEq, Repr

def shopOffers : Array ShopOffer := #[
  ⟨.arrow, 12, 6, 6⟩,
  ⟨.torch, 3, 4, 4⟩,
  ⟨.draught, 1, 10, 3⟩,
  ⟨.amberTreads, 1, 18, 1⟩,
  ⟨.watchhorn, 1, 8, 1⟩,
  ⟨.sunleafSeed, 1, 3, 2⟩
]

def initialMerchantStock : Array Nat := shopOffers.map (·.limit)

def shopOffersFor (legacyEarly : Bool) : Array ShopOffer :=
  if legacyEarly then shopOffers
  else (shopOffers.set! 2 ⟨.lesserHealingPotion, 1, 300, 3⟩).push ⟨.bugNet, 1, 2500, 1⟩

def initialMerchantStockFor (legacyEarly : Bool) : Array Nat :=
  (shopOffersFor legacyEarly).map (·.limit)

def sellPrice : Item → Nat
  | .stone | .wood => 1
  | .copper | .tinOre => 2
  | .silver | .tungstenOre | .obsidian => 3
  | .amber => 4
  | .geode => 6
  | .lesserHealingPotion => 60
  | .demoniteOre => 1000
  | .crimtaneOre => 1300
  | .musket | .undertaker | .shadowOrbPet | .crimsonHeartPet | .vilethorn | .ballOHurt
  | .bandOfStarpower | .panicNecklace | .crimsonRod | .rottedFork => 15000
  | .musketBall => 1
  | .bomb => 60
  | .stickyBomb => 100
  | .dynamite => 400
  | .bone => 10
  | .nazar => 20000
  | .crystalAssassinHood | .crystalAssassinShirt | .crystalAssassinPants => 20000
  | .gelatinousPillion => 50000
  | .bladeStaff => 10000
  | .summonerEmblem => 20000
  | .aquaScepter | .blueMoon | .magicMissile | .valor | .handgun => 17500
  | .shadowKey => 17500
  | .sunfury | .darkLance | .flowerOfFire | .flamelash | .hellwingBow => 25000
  | .hellstone => 250
  | .shadowChest => 1000
  | .woodenCrate | .pearlwoodCrate => 1000
  | .ironCrate | .mythrilCrate => 5000
  | .goldenCrate | .titaniumCrate => 20000
  | .corruptCrate | .crimsonCrate | .dungeonCrate | .skyCrate | .hallowedCrate
  | .jungleCrate | .defiledCrate | .hematicCrate | .stockadeCrate | .azureCrate
  | .divineCrate | .brambleCrate | .frozenCrate | .borealCrate | .oasisCrate
  | .mirageCrate | .obsidianCrate | .hellstoneCrate | .oceanCrate | .seasideCrate => 10000
  | .obsidianSkull => 5400
  | .obsidianRose | .magmaStone => 20000
  | .demonScythe => 15000
  | .guideVoodooDoll => 200
  | .plumbersHat => 2000
  | .batBat => 2500
  | .bugNet => 500
  | .bass => 500
  | .armoredCavefish => 1500
  | .specularFish => 750
  | .goldenCarp => 100000
  | .flarefinKoi => 5000
  | .obsidifish => 1500
  | .canOfWorms | .worm => 500
  | .enchantedNightcrawler | .goldWorm => 100000
  | .lavaproofBugNet => 50000
  | .hellforge => 600
  | .hellstoneBar => 4000
  | .treasureMagnet => 30000
  | .hellButterfly | .lavafly | .magmaSnail => 2000
  | _ => 0

/-- Buy one batch, updating the inventory and stock together. -/
def buyStock (inventory : Inventory) (stock : Array Nat) (offer : Nat) (legacyEarly := true) :
    Except String (Inventory × Array Nat) := do
  let offers := shopOffersFor legacyEarly
  let some entry := offers[offer]? | throw "That offer is unavailable."
  let some remaining := stock[offer]? | throw "Merchant stock is unavailable."
  if stock.size != offers.size ||
      (stock.zip offers).any (fun (amount, offer) => amount > offer.limit) then
    throw "Merchant stock is unavailable."
  if remaining == 0 then throw "That offer is sold out for today."
  let coins := Inventory.coinBalance inventory
  if coins < entry.price then throw "You do not have enough coins."
  let some inventory := Inventory.setCoinBalance inventory (coins - entry.price)
    | throw "Make room in your inventory for the change."
  let some (inventory, _) := Inventory.addItems inventory entry.item entry.quantity
    | throw "Make room in your inventory before buying."
  return (inventory, if entry.item == .bugNet then stock else stock.set! offer (remaining - 1))

/-- Sell all units in a single eligible inventory slot. -/
def sellStack (inventory : Inventory) (slot : Nat) : Except String (Inventory × Nat) := do
  if slot == Inventory.trashSlot then throw "Take the item out of the trash first."
  let some stack := inventory[slot]?.join | throw "Choose a stack to sell."
  if stack.count == 0 then throw "Choose a stack to sell."
  let price := sellPrice stack.item
  if price == 0 then throw "The merchant does not buy that item."
  let proceeds := stack.count * price
  let some inventory := Inventory.creditCoins (inventory.set! slot none) proceeds
    | throw "Make room in your inventory for the coins."
  return (inventory, proceeds)

end Lean4Terria
