import Lean4Terria.Inventory

namespace Lean4Terria

inductive EarlyMetal where
  | copper | tin | iron | lead | silver | tungsten | gold | platinum
  deriving BEq, Repr, Inhabited

namespace EarlyMetal

def all : Array EarlyMetal := #[.copper, .tin, .iron, .lead, .silver, .tungsten, .gold, .platinum]

def ore : EarlyMetal → Item
  | .copper => .copper
  | .tin => .tinOre
  | .iron => .iron
  | .lead => .leadOre
  | .silver => .silver
  | .tungsten => .tungstenOre
  | .gold => .goldOre
  | .platinum => .platinumOre

def bar : EarlyMetal → Item
  | .copper => .copperBar
  | .tin => .tinBar
  | .iron => .ironBar
  | .lead => .leadBar
  | .silver => .silverBar
  | .tungsten => .tungstenBar
  | .gold => .goldBar
  | .platinum => .platinumBar

def pick : EarlyMetal → Item
  | .copper => .copperPick
  | .tin => .tinPick
  | .iron => .ironPick
  | .lead => .leadPick
  | .silver => .silverPick
  | .tungsten => .tungstenPick
  | .gold => .goldPick
  | .platinum => .platinumPick

def axe : EarlyMetal → Item
  | .copper => .copperAxe
  | .tin => .tinAxe
  | .iron => .ironAxe
  | .lead => .leadAxe
  | .silver => .silverAxe
  | .tungsten => .tungstenAxe
  | .gold => .goldAxe
  | .platinum => .platinumAxe

def sword : EarlyMetal → Item
  | .copper => .copperSword
  | .tin => .tinSword
  | .iron => .ironSword
  | .lead => .leadSword
  | .silver => .silverSword
  | .tungsten => .tungstenSword
  | .gold => .goldSword
  | .platinum => .platinumSword

def bow : EarlyMetal → Item
  | .copper => .copperBow
  | .tin => .tinBow
  | .iron => .ironBow
  | .lead => .leadBow
  | .silver => .silverBow
  | .tungsten => .tungstenBow
  | .gold => .goldBow
  | .platinum => .platinumBow

def oreCost : EarlyMetal → Nat
  | .copper | .tin | .iron | .lead => 3
  | _ => 4

def axeBarCost : EarlyMetal → Nat
  | .copper | .tin => 6
  | _ => 8

def pickBarCost : EarlyMetal → Nat
  | .copper | .tin => 8
  | _ => 10

def pickWoodCost : EarlyMetal → Nat
  | .iron | .lead => 3
  | _ => 4

end EarlyMetal

inductive HardmodeMetal where
  | cobalt | palladium | mythril | orichalcum | adamantite | titanium
  deriving BEq, Repr, Inhabited

namespace HardmodeMetal

def all : Array HardmodeMetal := #[.cobalt, .palladium, .mythril, .orichalcum, .adamantite, .titanium]

def ore : HardmodeMetal → Item
  | .cobalt => .cobaltOre
  | .palladium => .palladiumOre
  | .mythril => .mythrilOre
  | .orichalcum => .orichalcumOre
  | .adamantite => .adamantiteOre
  | .titanium => .titaniumOre

def bar : HardmodeMetal → Item
  | .cobalt => .cobaltBar
  | .palladium => .palladiumBar
  | .mythril => .mythrilBar
  | .orichalcum => .orichalcumBar
  | .adamantite => .adamantiteBar
  | .titanium => .titaniumBar

def pick : HardmodeMetal → Item
  | .cobalt => .cobaltPickaxe
  | .palladium => .palladiumPickaxe
  | .mythril => .mythrilPickaxe
  | .orichalcum => .orichalcumPickaxe
  | .adamantite => .adamantitePickaxe
  | .titanium => .titaniumPickaxe

def drill : HardmodeMetal → Item
  | .cobalt => .cobaltDrill
  | .palladium => .palladiumDrill
  | .mythril => .mythrilDrill
  | .orichalcum => .orichalcumDrill
  | .adamantite => .adamantiteDrill
  | .titanium => .titaniumDrill

def oreCost : HardmodeMetal → Nat
  | .cobalt | .palladium => 3
  | _ => 4

def pickCost : HardmodeMetal → Nat
  | .cobalt | .mythril => 15
  | .palladium | .orichalcum | .adamantite => 18
  | .titanium => 20

end HardmodeMetal

inductive ClassicRecipe where
  | workbench | torches | platforms | woodWall | woodHammer | woodSword | woodBow | arrows | door | chest
  | woodChair | woodTable | woodFishingPole
  | furnace | smelt (metal : EarlyMetal) | ironAnvil | leadAnvil | chain
  | pick (metal : EarlyMetal) | axe (metal : EarlyMetal) | sword (metal : EarlyMetal) | copperShortsword
  | sawmill | loom | silk | bed
  | suspiciousLookingEye | demoniteBar | crimtaneBar | metalBow (metal : EarlyMetal) | demonBow | tendonBow | stickyBomb
  | glass | bottle | vilePowder | viciousPowder | wormFood | bloodySpine | nightmarePickaxe | deathbringerPickaxe | obsidianSkull
  | hellstoneBar | lavaproofBugNet | moltenPickaxe
  | hardmodeBar (metal : HardmodeMetal) | hardmodePick (metal : HardmodeMetal) | hardmodeDrill (metal : HardmodeMetal)
  | mythrilAnvil | orichalcumAnvil | adamantiteForge | titaniumForge
  | mechanicalEye | mechanicalWorm | mechanicalWormCrimson | mechanicalSkull
  deriving BEq, Repr, Inhabited

namespace ClassicRecipe

def ironBar : ClassicRecipe := .smelt .iron

def all : Array ClassicRecipe :=
  #[.workbench, .torches, .platforms, .woodWall, .woodHammer, .woodSword, .woodBow, .arrows,
    .door, .chest, .woodChair, .woodTable, .furnace, .ironAnvil, .leadAnvil] ++
  EarlyMetal.all.map .smelt ++ #[.chain, .copperShortsword] ++
  EarlyMetal.all.map .pick ++ EarlyMetal.all.map .axe ++ EarlyMetal.all.map .sword ++
  EarlyMetal.all.map .metalBow ++
  #[.sawmill, .loom, .silk, .bed, .suspiciousLookingEye, .demoniteBar, .crimtaneBar, .demonBow, .tendonBow, .stickyBomb,
    .glass, .bottle, .vilePowder, .viciousPowder, .wormFood, .bloodySpine, .nightmarePickaxe, .deathbringerPickaxe,
    .obsidianSkull, .hellstoneBar, .lavaproofBugNet, .moltenPickaxe] ++
  HardmodeMetal.all.map .hardmodeBar ++ HardmodeMetal.all.map .hardmodePick ++
  HardmodeMetal.all.map .hardmodeDrill ++ #[.mythrilAnvil, .orichalcumAnvil, .adamantiteForge, .titaniumForge,
    .mechanicalEye, .mechanicalWorm, .mechanicalWormCrimson, .mechanicalSkull, .woodFishingPole]

def item : ClassicRecipe → Item
  | .workbench => .workbench
  | .torches => .torch
  | .platforms => .platform
  | .woodWall => .woodWall
  | .woodHammer => .woodHammer
  | .woodSword => .woodSword
  | .woodBow => .bow
  | .arrows => .arrow
  | .door => .door
  | .chest => .chest
  | .woodChair => .woodChair
  | .woodTable => .woodTable
  | .woodFishingPole => .fishingRod
  | .furnace => .furnace
  | .smelt metal => metal.bar
  | .ironAnvil => .ironAnvil
  | .leadAnvil => .leadAnvil
  | .chain => .chain
  | .pick metal => metal.pick
  | .axe metal => metal.axe
  | .sword metal => metal.sword
  | .copperShortsword => .copperShortsword
  | .sawmill => .sawmill
  | .loom => .loom
  | .silk => .silk
  | .bed => .bed
  | .suspiciousLookingEye => .suspiciousLookingEye
  | .demoniteBar => .demoniteBar
  | .crimtaneBar => .crimtaneBar
  | .metalBow metal => metal.bow
  | .demonBow => .demonBow
  | .tendonBow => .tendonBow
  | .stickyBomb => .stickyBomb
  | .glass => .glass
  | .bottle => .bottle
  | .vilePowder => .vilePowder
  | .viciousPowder => .viciousPowder
  | .wormFood => .wormFood
  | .bloodySpine => .bloodySpine
  | .nightmarePickaxe => .nightmarePickaxe
  | .deathbringerPickaxe => .deathbringerPickaxe
  | .obsidianSkull => .obsidianSkull
  | .hellstoneBar => .hellstoneBar
  | .lavaproofBugNet => .lavaproofBugNet
  | .moltenPickaxe => .moltenPickaxe
  | .hardmodeBar metal => metal.bar
  | .hardmodePick metal => metal.pick
  | .hardmodeDrill metal => metal.drill
  | .mythrilAnvil => .mythrilAnvil
  | .orichalcumAnvil => .orichalcumAnvil
  | .adamantiteForge => .adamantiteForge
  | .titaniumForge => .titaniumForge
  | .mechanicalEye => .mechanicalEye
  | .mechanicalWorm | .mechanicalWormCrimson => .mechanicalWorm
  | .mechanicalSkull => .mechanicalSkull

def quantity : ClassicRecipe → Nat
  | .torches => 3
  | .platforms | .bottle => 2
  | .vilePowder | .viciousPowder => 5
  | .woodWall => 4
  | .arrows => 25
  | .chain => 10
  | _ => 1

def acceptsLeadBar : ClassicRecipe → Bool
  | .chain | .chest | .sawmill | .mechanicalEye | .mechanicalWorm | .mechanicalWormCrimson | .mechanicalSkull => true
  | _ => false

def ingredients : ClassicRecipe → List (Item × Nat)
  | .workbench => [(.wood, 10)]
  | .torches => [(.caveGel, 1), (.wood, 1)]
  | .platforms | .woodWall => [(.wood, 1)]
  | .woodHammer => [(.wood, 8)]
  | .woodSword => [(.wood, 7)]
  | .woodBow => [(.wood, 10)]
  | .arrows => [(.wood, 1), (.stone, 1)]
  | .door => [(.wood, 6)]
  | .chest => [(.wood, 8), (.ironBar, 2)]
  | .woodChair => [(.wood, 4)]
  | .woodTable => [(.wood, 8)]
  | .woodFishingPole => [(.wood, 8)]
  | .furnace => [(.stone, 20), (.wood, 4), (.torch, 3)]
  | .smelt metal => [(metal.ore, metal.oreCost)]
  | .ironAnvil => [(.ironBar, 5)]
  | .leadAnvil => [(.leadBar, 5)]
  | .chain => [(.ironBar, 1)]
  | .pick metal => [(metal.bar, metal.pickBarCost), (.wood, metal.pickWoodCost)]
  | .axe metal => [(metal.bar, metal.axeBarCost), (.wood, 3)]
  | .sword metal => [(metal.bar, metal.axeBarCost)]
  | .copperShortsword => [(.copperBar, 5)]
  | .sawmill => [(.wood, 10), (.ironBar, 2), (.chain, 1)]
  | .loom => [(.wood, 12)]
  | .silk => [(.cobweb, 7)]
  | .bed => [(.wood, 15), (.silk, 5)]
  | .suspiciousLookingEye => [(.lens, 6)]
  | .demoniteBar => [(.demoniteOre, 3)]
  | .crimtaneBar => [(.crimtaneOre, 3)]
  | .metalBow metal => [(metal.bar, 7)]
  | .demonBow => [(.demoniteBar, 8)]
  | .tendonBow => [(.crimtaneBar, 8)]
  | .stickyBomb => [(.bomb, 1), (.caveGel, 1)]
  | .glass => [(.sand, 2)]
  | .bottle => [(.glass, 1)]
  | .vilePowder => [(.vileMushroom, 1)]
  | .viciousPowder => [(.viciousMushroom, 1)]
  | .wormFood => [(.vilePowder, 30), (.rottenChunk, 15)]
  | .bloodySpine => [(.viciousPowder, 30), (.vertebra, 15)]
  | .nightmarePickaxe => [(.demoniteBar, 12), (.shadowScale, 6)]
  | .deathbringerPickaxe => [(.crimtaneBar, 12), (.tissueSample, 6)]
  | .obsidianSkull => [(.obsidian, 20)]
  | .hellstoneBar => [(.hellstone, 3), (.obsidian, 1)]
  | .lavaproofBugNet => [(.hellstoneBar, 15), (.bugNet, 1)]
  | .moltenPickaxe => [(.hellstoneBar, 20)]
  | .hardmodeBar metal => [(metal.ore, metal.oreCost)]
  | .hardmodePick metal | .hardmodeDrill metal => [(metal.bar, metal.pickCost)]
  | .mythrilAnvil => [(.mythrilBar, 10)]
  | .orichalcumAnvil => [(.orichalcumBar, 12)]
  | .adamantiteForge => [(.adamantiteOre, 30), (.hellforge, 1)]
  | .titaniumForge => [(.titaniumOre, 30), (.hellforge, 1)]
  | .mechanicalEye => [(.lens, 3), (.ironBar, 5), (.soulOfLight, 6)]
  | .mechanicalWorm => [(.rottenChunk, 6), (.ironBar, 5), (.soulOfNight, 6)]
  | .mechanicalWormCrimson => [(.vertebra, 6), (.ironBar, 5), (.soulOfNight, 6)]
  | .mechanicalSkull => [(.bone, 30), (.ironBar, 5), (.soulOfLight, 3), (.soulOfNight, 3)]

def station : ClassicRecipe → Option Tile
  | .workbench | .torches | .platforms | .stickyBomb => none
  | .smelt _ | .demoniteBar | .crimtaneBar | .glass | .bottle | .obsidianSkull => some .furnace
  | .hellstoneBar | .lavaproofBugNet => some .hellforge
  | .chain | .pick _ | .axe _ | .sword _ | .copperShortsword | .metalBow _ | .demonBow | .tendonBow => some .ironAnvil
  | .nightmarePickaxe | .deathbringerPickaxe | .moltenPickaxe | .mythrilAnvil | .orichalcumAnvil => some .ironAnvil
  | .hardmodePick .cobalt | .hardmodePick .palladium
  | .hardmodeDrill .cobalt | .hardmodeDrill .palladium => some .ironAnvil
  | .hardmodePick _ | .hardmodeDrill _ | .adamantiteForge | .titaniumForge => some .mythrilAnvil
  | .mechanicalEye | .mechanicalWorm | .mechanicalWormCrimson | .mechanicalSkull => some .mythrilAnvil
  | .hardmodeBar .adamantite | .hardmodeBar .titanium => some .adamantiteForge
  | .hardmodeBar _ => some .furnace
  | .suspiciousLookingEye | .wormFood | .bloodySpine => some .demonAltar
  | .vilePowder | .viciousPowder => some .placedBottle
  | .loom | .bed => some .sawmill
  | .silk => some .loom
  | _ => some .workbench

def stationName (recipe : ClassicRecipe) : String :=
  match recipe.station with
  | none => "By Hand"
  | some .furnace => "Furnace"
  | some .hellforge => "Hellforge"
  | some .adamantiteForge => "Adamantite or Titanium Forge"
  | some .mythrilAnvil => "Mythril or Orichalcum Anvil"
  | some .ironAnvil => "Iron or Lead Anvil"
  | some .demonAltar => "Demon or Crimson Altar"
  | some .sawmill => "Sawmill"
  | some .loom => "Loom"
  | some .placedBottle => "Placed Bottle"
  | _ => "Work Bench"

end ClassicRecipe

end Lean4Terria
