import Lean4Terria.ClassicCrafting

namespace Lean4Terria

/-- Ordinary-world Shadow Chest items after the main weapon. Draw indices
refer to independent calls to Next in WorldGen.AddBuriedChest. -/
def shadowChestExtras (ore : EarlyOreSet) (roll : Nat → Nat) : Array Stack := Id.run do
  let mut loot : Array Stack := #[]
  if roll 0 % 5 == 0 then loot := loot.push ⟨.treasureMagnet, 1⟩
  if roll 1 % 10 == 0 then loot := loot.push ⟨.demonicHellcart, 1⟩
  if roll 2 % 10 == 0 then loot := loot.push ⟨.ornateShadowKey, 1⟩
  if roll 3 % 10 == 0 then loot := loot.push ⟨.sliceOfHellCake, 1⟩
  if roll 4 % 3 == 0 then loot := loot.push ⟨.dynamite, 1⟩
  if roll 5 % 2 == 0 then
    let gold := if ore.gold == .platinumOre then Item.platinumBar else .goldBar
    loot := loot.push ⟨if roll 6 % 2 == 0 then .meteoriteBar else gold, 15 + roll 7 % 15⟩
  if roll 8 % 2 == 0 then
    let bullet := if ore.silver == .tungstenOre then Item.tungstenBullet else .silverBullet
    loot := loot.push ⟨if roll 9 % 2 == 0 then .hellfireArrow else bullet, 50 + roll 10 % 25⟩
  if roll 11 % 2 == 0 then loot := loot.push ⟨.restorationPotion, 15 + roll 12 % 6⟩
  if roll 13 % 4 > 0 then
    let potions : Array Item := #[.spelunkerPotion, .featherfallPotion, .manaRegenerationPotion,
      .obsidianSkinPotion, .magicPowerPotion, .invisibilityPotion, .hunterPotion, .heartreachPotion]
    loot := loot.push ⟨potions[roll 14 % 8]!, 1 + roll 15 % 2⟩
  if roll 16 % 3 > 0 then
    let potions : Array Item := #[.gravitationPotion, .thornsPotion, .waterWalkingPotion,
      .obsidianSkinPotion, .battlePotion, .teleportationPotion, .infernoPotion, .lifeforcePotion]
    loot := loot.push ⟨potions[roll 17 % 8]!, 1 + roll 18 % 2⟩
  if roll 19 % 3 == 0 then
    loot := loot.push ⟨if roll 21 % 2 == 0 then .recallPotion else .potionOfReturn, 1 + roll 20 % 2⟩
  if roll 22 % 2 == 0 then
    loot := loot.push ⟨if roll 23 % 2 == 0 then .torch else .glowstick, 15 + roll 24 % 15⟩
  if roll 25 % 2 == 0 then loot := loot.push ⟨.goldCoin, 2 + roll 26 % 3⟩
  if roll 27 % 12 == 0 then
    let voices : Array Item := #[.froggyNeckband, .goatsTuft, .oldCompanionLocket, .catChime, .dogCollar,
      .turkeyWattleNecklace, .meanGoblinSpikes, .crowsBeak, .balloonyBeads, .grimOldBarb,
      .vampirePendant, .cowBell, .chickenCharm, .fairyChoker]
    loot := loot.push ⟨voices[roll 28 % 14]!, 1⟩
  return loot

end Lean4Terria
