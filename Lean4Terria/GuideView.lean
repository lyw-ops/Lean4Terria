import Lean4Terria.View

namespace Lean4Terria.GuideView

open Desktop

private def background : UInt32 := 0x101822ff
private def surface : UInt32 := 0x1b2936ff
private def line : UInt32 := 0x394956ff
private def paper : UInt32 := 0xf6f0dcff
private def muted : UInt32 := 0xaebdc5ff
private def gold : UInt32 := 0xf6bd76ff
private def mint : UInt32 := 0x89e2d0ff

private def tabs : Array String := #["Route", "Recipes", "Controls"]

def pageCount : Nat := tabs.size

private def tabX (page : Nat) : Nat := 48 + 162 * page

def pageAt (mouseX mouseY : Nat) : Option Nat :=
  if 104 ≤ mouseY && mouseY < 144 then
    (List.range pageCount).find? fun page => tabX page ≤ mouseX && mouseX < tabX page + 152
  else
    none

private inductive Location where
  | missing | carried | stored | equipped
  deriving BEq

private def itemLocation (state : GameState) (item : Item) : Location :=
  if state.equipment.armor == some item || state.equipment.accessory == some item then .equipped
  else if Inventory.count state.inventory item > 0 then .carried
  else if state.chests.any (fun chest => Inventory.count chest.slots item > 0) then .stored
  else .missing

private def owns (state : GameState) (item : Item) : Bool := itemLocation state item != .missing

private def Location.label (location : Location) (missing : String := "") : String :=
  match location with
  | .missing => missing
  | .carried => "CARRIED"
  | .stored => "IN CHEST"
  | .equipped => "EQUIPPED"

private def firstOwned (state : GameState) (items : List Item) : Location :=
  ((items.map (itemLocation state)).find? (· != .missing)).getD .missing

private def clearedStatus (state : GameState) (reward : Item) : String :=
  match itemLocation state reward with
  | .stored => "CLEARED / STORED"
  | .equipped => "CLEARED / WORN"
  | _ => "CLEARED"

private def benchStep (state : GameState) (key action : String) : String :=
  if state.nearWorkbench then s!"Press {key} at this workbench to {action}."
  else s!"Find a workbench, then press {key} to {action}."

private def miningStep (state : GameState) : String :=
  if !state.legacyEarly then (View.starterObjective state).2
  else if itemLocation state .copperPick == .stored && !owns state .crystalPick then
    "Retrieve your copper pick from its chest to mine crystal."
  else if !owns state .copperPick && !owns state .crystalPick then
    if Inventory.count state.inventory .copper ≥ copperPickCost then
      benchStep state "E" "craft a copper pick"
    else
      s!"Mine {copperPickCost} copper ore beneath the starting area."
  else if itemLocation state .crystalPick == .stored then
    "Retrieve your crystal pick from its chest to mine iron and silver."
  else if !owns state .crystalPick then
    if Inventory.count state.inventory .crystal ≥ crystalPickCost then
      benchStep state "T" "craft a crystal pick"
    else
      s!"Follow the cave east and mine {crystalPickCost} crystal with the copper pick."
  else if !owns state .ironSword && !owns state .frostbrand && !owns state .silverSword then
    if Inventory.count state.inventory .iron ≥ ironSwordCost then
      benchStep state "G" "forge an iron sword"
    else
      s!"Mine {ironSwordCost} iron ore in Frost Cavern for an iron sword."
  else
    "Follow Frost Cavern east into Warden Hall. Jump over charges and strike during recovery."

private def afterWarden (state : GameState) : String :=
  let bows : List Item := [.stormBow, .hunterBow, .bow]
  let carryingBow := bows.any (fun item => itemLocation state item == .carried)
  if !state.legacyEarly && !state.quarry.cleared then
    "Break the bottom two ice tiles east of Warden Hall. Swim through the Flooded Quarry."
  else if state.legacyEarly && !state.quarry.cleared && !owns state .silverSword then
    if Inventory.count state.inventory .silver ≥ silverSwordCost then
      benchStep state "Y" "forge a silver sword"
    else if itemLocation state .crystalPick == .stored then
      "Retrieve your crystal pick from its chest to break ice and mine silver."
    else if !owns state .crystalPick then
      miningStep state
    else
      "Break the bottom two ice tiles in Warden Hall's east wall. Find silver in the quarry."
  else if !bows.any (owns state) then
    let wood := Inventory.count state.inventory .wood
    if Inventory.count state.inventory .frostShell > 0 && Inventory.count state.inventory .quarryScale > 0 &&
        wood ≥ hunterBowWood then
      benchStep state "O" "make a Hunter Bow"
    else if !state.legacyEarly then
      "Use Esc > Crafting at a Work Bench to make a Wooden Bow for the Storm Vault."
    else if wood ≥ bowCost then
      benchStep state "U" "make a wooden bow"
    else
      s!"Gather {bowCost} wood for a bow, then head east to the Storm Vault."
  else if !carryingBow then
    let stored := (bows.find? (owns state)).getD .bow
    s!"Retrieve your {stored.name} from its chest before entering the Storm Vault."
  else if Inventory.count state.inventory .arrow == 0 then
    if itemLocation state .arrow == .stored then "Retrieve arrows from a chest before entering the Storm Vault."
    else if !state.legacyEarly then "Use Esc > Crafting at a Work Bench to make Wooden Arrows."
    else benchStep state "I" "make arrows"
  else if !state.sentry.cleared then
    "Head east past the quarry. Use stone cover against the Sentry and fire from clear ground."
  else
    "Point at the Storm Altar in the eastern vault and press Q to summon the Storm Herald."

private def afterStorm (state : GameState) : String :=
  if itemLocation state .watchhorn == .stored then
    "Retrieve the Watchhorn from its chest and take it to the Amber Bluffs."
  else if owns state .watchhorn then
    if canStartRaid state.world state.seed state.player then
      "Select the Watchhorn and left-click to call the Amber Raid. Prepare for two waves."
    else
      "Take the Watchhorn to open sky on the Amber Bluffs, west of the quarry's surface exit."
  else if state.merchant.isNone then
    "Build a furnished, torchlit room and point at its door with J to invite a Merchant."
  else
    match ((shopOffersFor state.legacyEarly).zip state.merchantStock).find? (fun (offer, _) => offer.item == .watchhorn) with
    | some (_, 0) => "The Watchhorn is sold out. The Merchant restocks at dawn."
    | some (offer, _) =>
      if state.coins ≥ offer.price then
        s!"Point at the Merchant and press Q to buy the Watchhorn for {offer.price} coins."
      else
        s!"Sell spare ore to the Merchant to raise {offer.price} coins for a Watchhorn."
    | none => "Talk to the Merchant with Q to look for a Watchhorn."

private def deepDungeonStep (state : GameState) (layout : DungeonLayout) : Option String :=
  if !state.deepDungeonInstalled then none
  else if state.world.tile layout.valorChest == .dungeonGoldChest &&
      Inventory.count state.inventory .goldenKey > 0 then
    some "Climb the east steps beyond the Magic Missile vault and use a Golden Key on the first Gold Chest."
  else if (state.chestAt layout.valorChest).any (fun chest =>
      state.world.tile layout.valorChest == .dungeonOpenChest && Inventory.count chest.slots .valor > 0) then
    some "Take Valor from the Gold Chest and put it on your hotbar."
  else if (state.chestAt layout.deepKeyChest).any (fun chest => Inventory.count chest.slots .goldenKey > 0) then
    some "The wooden chest in the eastern gallery opens freely and contains a Golden Key."
  else if state.world.tile layout.valorChest == .dungeonGoldChest then
    some "A Dungeon Slime can drop a Golden Key for the Valor chest."
  else if state.world.tile layout.handgunChest == .dungeonGoldChest then
    if Inventory.count state.inventory .goldenKey > 0 then some "Use a Golden Key on the far eastern Gold Chest."
    else some "A Dungeon Slime can drop a Golden Key for the locked eastern chest."
  else if (state.chestAt layout.handgunChest).any (fun chest => Inventory.count chest.slots .handgun > 0) then
    some "Take the Handgun. Equip it and load Musket Balls from your inventory."
  else none

private def magicMissileDungeonStep (state : GameState) (layout : DungeonLayout) : Option String :=
  if !state.magicMissileDungeonInstalled then none
  else if state.world.tile layout.magicMissileChest == .dungeonGoldChest then
    if Inventory.count state.inventory .goldenKey > 0 then
      some "Head west past Blue Moon's chest, climb the platform shaft, then follow the upper vault east."
    else some "Find a Golden Key for the vault above the western platform shaft."
  else if (state.chestAt layout.magicMissileChest).any (fun chest => Inventory.count chest.slots .magicMissile > 0) then
    some "Take the Magic Missile from the Gold Chest and put it on your hotbar."
  else deepDungeonStep state layout

private def blueMoonDungeonStep (state : GameState) (layout : DungeonLayout) : Option String := do
  if !state.blueMoonDungeonInstalled then none
  else if state.world.tile layout.blueMoonChest == .dungeonGoldChest then
    if Inventory.count state.inventory .goldenKey > 0 then
      some "Follow the steps west of the lower hall to the next locked Gold Chest."
    else some "Find another Golden Key for the chest west of the lower hall."
  else if (state.chestAt layout.blueMoonChest).any (fun chest => Inventory.count chest.slots .blueMoon > 0) then
    some "Take Blue Moon from the Gold Chest and put it on your hotbar."
  else magicMissileDungeonStep state layout

private def lowerDungeonStep (state : GameState) (layout : DungeonLayout) : Option String := do
  if !state.lowerDungeonInstalled then none
  else if state.world.tile layout.lowerGoldChest == .dungeonGoldChest then
    if Inventory.count state.inventory .goldenKey > 0 then
      some "Drop down the eastern shaft, then follow the lower hall west to the locked Gold Chest."
    else some "Defeat a Dungeon Slime for a Golden Key to the lower Gold Chest."
  else if (state.chestAt layout.lowerGoldChest).any (fun chest => Inventory.count chest.slots .aquaScepter > 0) then
    some "Take the Aqua Scepter from the lower Gold Chest and put it on your hotbar."
  else blueMoonDungeonStep state layout

private def dungeonStep (state : GameState) : Option String := do
  if !state.dungeonInstalled then none else
    let layout := dungeonLayout state.seed
    let player := state.player.centerTile
    if state.dungeonGuardian.isSome then
      some "The Dungeon Guardian is pursuing you. Defeat Skeletron to enter safely."
    else if !state.skeletronDefeated &&
        (layout.oldMan.x - player.x).natAbs ≤ 14 && (layout.oldMan.y - player.y).natAbs ≤ 6 then
      some (if isNight state.surface.clock state.legacyEarly then
        "Aim at the Old Man, press Q, and choose Curse to summon Skeletron."
      else "Speak to the Old Man at the Dungeon entrance. Return after dark to summon Skeletron.")
    else if state.skeletronDefeated && state.deepDungeonInstalled &&
        dungeonUnsafeWall state.seed player false false false true &&
        !dungeonUnsafeWall state.seed player state.lowerDungeonInstalled state.blueMoonDungeonInstalled
          state.magicMissileDungeonInstalled then
      deepDungeonStep state layout
    else if state.skeletronDefeated && state.blueMoonDungeonInstalled && player.x < layout.left &&
        dungeonUnsafeWall state.seed player state.lowerDungeonInstalled true state.magicMissileDungeonInstalled then
      blueMoonDungeonStep state layout
    else if state.skeletronDefeated && (layout.left : Int) ≤ player.x && player.x ≤ layout.right then
      if state.equipment.accessory == some .cobaltShield then lowerDungeonStep state layout
      else if Inventory.count state.inventory .cobaltShield > 0 ||
          state.cursorStack.any (·.item == .cobaltShield) then
        some "Equip the Cobalt Shield in your inventory's accessory slot."
      else if state.world.tile layout.goldChest == .dungeonOpenChest then
        if (state.chestAt layout.goldChest).any (fun chest => Inventory.count chest.slots .cobaltShield > 0) then
          some "Take the Cobalt Shield from the Gold Chest and equip it from your inventory."
        else lowerDungeonStep state layout
      else if Inventory.count state.inventory .goldenKey > 0 then
        some "Descend the Dungeon shaft and follow the lower hall west to the locked Gold Chest."
      else some "Look for a Golden Key in the entrance chest or defeat a Dungeon Slime."
    else none

private def nextStep (state : GameState) : String :=
  if let some hint := View.evilBossInstruction state then hint
  else if let some boss := state.eye then View.eyeInstruction boss
  else if let some boss := state.skeletron then View.skeletronInstruction boss
  else if let some hint := dungeonStep state then hint else
  match state.cavern, state.raid, state.ruin, state.storm, state.warden with
  | .reward _, _, _, _, _ => "Collect the Deepstone Aegis, then right-click it in your backpack to equip it."
  | .fighting boss, _, _, _, _ =>
    if boss.phase == .recover then "The Colossus's shell is open. Attack while its core is exposed."
    else "Jump the first beam. Land and stay grounded for the second beam, then attack."
  | _, .reward _, _, _, _ => "Walk over the dropped Raid Crest. Open your backpack and right-click it to equip."
  | _, _, .reward _, _, _ => "Walk over the dropped Ruin Hammer. Select it to deal five damage per swing."
  | _, _, _, .reward _, _ => "Walk over the dropped Storm Bow to collect it. The bow uses regular arrows."
  | _, _, _, _, .reward _ => "Walk over the dropped Frostbrand to collect it, then explore east of Warden Hall."
  | _, .active wave, _, _, _ =>
    if wave.countdown > 0 then "Prepare stone cover before the next raid wave arrives."
    else "Stay in the Amber Bluffs and use walls to block slinger shots."
  | _, _, .fighting boss, _, _ =>
    match boss.phase with
    | .windup => "Watch the floor marker. When the hammer falls, jump over the ground waves."
    | .slam => "Jump over the ground waves. Retreat east if you need to reset the fight."
    | .recover => "The Sentinel's core is exposed. Hit it with a sword or arrows."
  | _, _, _, .fighting _, _ => "Keep cover between you and the Storm Herald during volleys. Fire during recovery."
  | _, _, _, _, .fighting _ => "Watch the Warden's windup, jump over its charge and strike during recovery."
  | cavern, raid, ruin, storm, warden =>
    let hasMine := state.world.tile (deepsteelMarker state.seed) == .mineMarker
    let atMine := hasMine && (inDeepsteelMine state.seed state.player.centerTile ||
      state.canReach (deepsteelMouth state.seed))
    let hasCavern := state.world.tile cavernAltar == .cavernAltar
    let atCavern := hasCavern && inHighlandCavern state.seed state.player.centerTile
    let hasGallery := state.world.tile magmaMarker == .magmaMarker
    let atGallery := hasGallery && inMagmaGallery state.player.centerTile
    let nearGallery := hasGallery && magmaEntranceCaps.any state.canReach
    if nearGallery && magmaEntranceCaps.any (fun cap => (state.world.tile cap).solid) then
      "Open the two stone caps in Deepstone Cavern's west wall to reach the Magma Gallery."
    else if atGallery then
      if state.world.tile magmaWaterGate == .stone then
        "Mine the blue-marked gate to release water. Lava can destroy wood platforms."
      else if state.legacyEarly then "Water cools lava into obsidian. Mine it with a crystal pick."
      else "Water cools lava into obsidian. Bring a Gold or Platinum Pickaxe."
    else if atCavern && cavern == .ready then
      if inCavernArena state.player.centerTile then
        "Point at the Deepstone Altar and press Q. Jump the low beam, then land for the high beam."
      else "Hold Down to descend through the platforms. Turn west for the Deepstone Altar."
    else if cavern == .cleared && itemLocation state .deepstoneAegis == .carried then
      "Open your backpack and right-click the Deepstone Aegis to equip it."
    else if cavern == .cleared && itemLocation state .deepstoneAegis == .stored then
      "Retrieve your Deepstone Aegis from its chest, then equip it from the backpack."
    else if atCavern && hasGallery then
      "Follow the Deepstone Cavern west to the capped entrance to the Magma Gallery."
    else if atMine then
      if Inventory.count state.inventory .deepsteel ≥ highlandCrossbowOreCost then
        benchStep state "F11" "forge a Highland Crossbow"
      else if hasCavern && cavern == .ready && inDeepsteelRoom state.seed state.player.centerTile then
        "At the mine's east end, open the two stone caps. Descend to the Deepstone Cavern."
      else if inDeepsteelRoom state.seed state.player.centerTile then
        if state.legacyEarly then "Keep clear of the Oreback. Mine the Deepsteel veins with a crystal pick."
        else "Keep clear of the Oreback. Deepsteel requires at least 45% pickaxe power."
      else "Open the stone caps by the torch and follow the steps into the Deepsteel Mine."
    else if inHighlands state.player.centerTile then
      if isNight state.surface.clock state.legacyEarly then
        "Bramble Crawlers hunt after dusk. Close the camp door or draw a sword."
      else if hasMine && !owns state .highlandCrossbow then
        if state.legacyEarly then "Look east for the torch at the Deepsteel Mine. Bring a crystal pick and a weapon."
        else "Look east for the torch at the Deepsteel Mine. Bring a 45% pickaxe and a weapon."
      else s!"Harvest orange berries with a pick. Select one and left-click to heal up to {highlandBerryHealing} HP."
    else if let some hint := View.evilObjective state then hint
    else if warden == .ready then miningStep state
    else if storm == .ready then afterWarden state
    else if ruin == .ready && state.world.tile ruinAltar == .ruinAltar then
      if inRuinArena state.player.centerTile then "Aim at the ruin altar and press Q to summon the Sentinel."
      else "Take the vault's east exit, descend the platforms, then turn left toward the ruin altar."
    else if raid == .ready then afterStorm state
    else
      match itemLocation state .raidCrest with
      | .stored => "Your Raid Crest is in a chest. Retrieve and equip it to gain speed and defense."
      | .carried => "Open your backpack and right-click the Raid Crest to gain speed and defense."
      | _ =>
        if !owns state .geodeMail then "Mine geodes in the grotto and forge Geode Mail to add defense."
        else "Cross the shaft at the Storm Vault's east end and climb the steps to Cedar Highlands."

private def wardenStatus (state : GameState) : String :=
  match state.warden with
  | .ready => ""
  | .fighting _ => "FIGHTING"
  | .reward _ => "CLAIM DROP"
  | .cleared => clearedStatus state .frostbrand

private def stormStatus (state : GameState) : String :=
  match state.storm with
  | .ready => ""
  | .fighting _ => "FIGHTING"
  | .reward _ => "CLAIM DROP"
  | .cleared => clearedStatus state .stormBow

private def ruinStatus (state : GameState) : String :=
  match state.ruin with
  | .ready => ""
  | .fighting _ => "FIGHTING"
  | .reward _ => "CLAIM DROP"
  | .cleared => clearedStatus state .ruinHammer

private def raidStatus (state : GameState) : String :=
  match state.raid with
  | .ready => (itemLocation state .watchhorn).label
  | .active wave => s!"WAVE {wave.number}/{raidWaveCount}"
  | .reward _ => "CLAIM DROP"
  | .cleared => clearedStatus state .raidCrest

private def routeRow (x y : Int) (w : Nat) (title status first second : String) : IO Unit := do
  rect x y w 1 line
  text title x (y + 6) 17 paper
  text status (x + w - 135) (y + 9) 12 mint
  text first x (y + 23) 15 muted
  text second x (y + 38) 15 muted

private def homeRoute (state : GameState) : String :=
  match state.homeAnchor with
  | none =>
    match itemLocation state .homeBeacon with
    | .carried => "Place the Home Beacon on dry ground and bind it with Q."
    | .stored => "Retrieve your Home Beacon from its chest, then place and bind it."
    | _ => "Home Beacon (K): place on dry ground and bind with Q."
  | some _ =>
    match itemLocation state .recallCharm with
    | .carried => "Home bound. Select a Recall Charm and left-click to return."
    | .stored => "Home bound. Retrieve your Recall Charm from its chest."
    | _ => "Home bound. Craft a Recall Charm (Z) at a workbench."

private def drawRoute (state : GameState) : IO Unit := do
  rect 48 166 1024 71 surface
  rect 48 166 3 71 mint
  text "NEXT STEP" 65 179 13 mint
  text (nextStep state) 65 202 19 paper
  text "CAVERNS & BOSSES" 48 253 14 gold
  text "HOUSING & SURVIVAL" 624 253 14 gold
  if state.legacyEarly then
    routeRow 48 276 548 "Copper Pick" (itemLocation state .copperPick).label
      s!"Mine {copperPickCost} copper ore below spawn and craft a pick with E."
      "Make a stone sword (R) for the Cave Crawler."
    routeRow 48 330 548 "Frost Cavern" (firstOwned state [.crystalPick, .ironSword]).label
      "Mine crystal with the copper pick, then make a crystal pick (T)."
      "Mine iron for a sword (G) or a grappling hook (F10)."
  else
    routeRow 48 276 548 "First tools and light" ""
      "Hold left-click with the Copper Axe on a tree. Craft a Work Bench."
      "Slimes drop Gel. Combine it with Wood to make torches."
    routeRow 48 330 548 "Furnace and anvil" (firstOwned state [.ironPick, .leadPick]).label
      "Mine stone for a Furnace. Smelt Iron or Lead Ore in it."
      "Craft an anvil at the Work Bench, then forge tools beside the anvil."
  if state.legacyEarly then
    routeRow 48 384 548 "Frost Warden" (wardenStatus state)
      "Walk into Warden Hall. Jump over charges and strike in recovery."
      "Retreat west to reset. Walk over Frostbrand to collect it."
    routeRow 48 438 548 "Flooded Quarry" (if state.quarry.cleared then "CLEARED" else "")
      "Break the bottom two ice tiles. Hold Space to swim up."
      "Mine silver with a crystal pick and forge a silver sword (Y)."
  else
    let eyeStatus := if state.eye.isSome then "FIGHTING" else if state.eyeDefeated then "DEFEATED" else ""
    routeRow 48 384 548 "Eye of Cthulhu" eyeStatus
      "Hunt Demon Eyes for 6 Lens. Look for an altar in the caves."
      "Craft a Suspicious Looking Eye. Select it and left-click at night."
    if state.dungeonInstalled then
      let status := if state.skeletronDefeated then "OPEN" else if state.skeletron.isSome then "FIGHTING" else ""
      routeRow 48 438 548 "Dungeon & Skeletron" status
        "Find the brick terrace east of spawn. Q on the Old Man after dark."
        "Defeat Skeletron. Bring a Golden Key to the lower Gold Chest."
    else
      routeRow 48 438 548 "Frost Warden & Flooded Quarry" (wardenStatus state)
        "Jump the Warden's charges and strike during recovery."
        "Break the east wall's lower ice and hold Space to swim up."
  routeRow 48 492 548 "Storm Herald" (stormStatus state)
    (if state.legacyEarly then "Craft a bow and arrows (U, I). Use cover against the Sentry."
      else "Craft a bow and arrows at a Work Bench. Use cover against the Sentry.")
    "At the altar press Q. Fire during recovery and collect the Storm Bow."
  if state.world.tile ruinAltar == .ruinAltar then
    routeRow 48 546 548 "Ruin Sentinel" (ruinStatus state)
      "Descend the vault's east shaft. Turn left and press Q at the altar."
      "Jump the waves, strike in recovery, then collect the Ruin Hammer."
  else
    routeRow 48 546 548 "Eastern Passage" "" "This world's original eastern passage was preserved." ""
  let housed := if state.guide.isSome then "HOUSED" else ""
  if state.legacyEarly then
    routeRow 624 276 448 "House the Guide" housed
      "Seal a room with a two-tile door, torch and chest or bench."
      "Leave 9 open tiles (3+ wide and high). Aim at the door, H."
  else
    routeRow 624 276 448 "House the Guide" housed
      "Build a walled room with an entrance, bench, chair and light."
      "Aim inside and press H to check it. Ask the Guide with Q."
  routeRow 624 330 448 "Open a Shop" (if state.merchant.isSome then "HOUSED" else "")
    "Press J at a separate room's door to house the Merchant."
    "Point at the Merchant and press Q to buy and sell."
  let evil := (evilAltars state.seed)[0]?.any (fun altar => state.world.tile altar == altarTile state.seed)
  if evil then
    let crimson := evilRelicTile state.seed == .crimsonHeart
    let relic := if crimson then "Crimson Heart" else "Shadow Orb"
    let gun := if crimson then "Undertaker" else "Musket"
    let boss := if crimson then "the Brain" else "the Eater"
    let summon := if crimson then "Bloody Spine" else "Worm Food"
    let status := if state.evilBossDefeated then "BOSS DEFEATED"
      else if state.evilRelicsBroken == 0 then "" else s!"BROKEN  {state.evilRelicsBroken}/3"
    routeRow 624 384 448 (if crimson then "The Crimson" else "The Corruption") status
      (if state.evilRelicsBroken < 3 then s!"Break three {relic}s to summon {boss}."
        else if state.legacyEarly then "All three relics are broken."
        else s!"Craft {summon} at an altar, then use it here.")
      (if state.evilRelicsBroken == 0 then s!"The first drops a {gun} and 100 Musket Balls."
        else if state.evilRelicsBroken < 3 then "Use a Wooden Hammer or bombs."
        else if crimson then "Defeat Creepers first, then attack the exposed heart."
        else "Destroy each chain. Keep clear of their heads.")
  else
    routeRow 624 384 448 "Amber Bluffs & Geode Grotto" ""
      "Climb out of the quarry and head west for surface amber."
      (if state.legacyEarly then "Dig east of silver and down. Use a crystal pick on geodes."
        else "Dig east of silver and down. Geodes need a 45% pickaxe.")
  routeRow 624 438 448 "Gear" (firstOwned state [.deepstoneAegis, .geodeMail, .raidCrest, .amberTreads]).label
    "Forge Geode Mail (N) and buy Amber Treads for speed."
    "Open your backpack and right-click a piece to equip it."
  routeRow 624 492 448 "Amber Raid" (raidStatus state)
    "Buy a Watchhorn and use it outdoors in the Amber Bluffs."
    s!"Fight {raidWaveCount} waves, then walk over the Raid Crest to collect it."
  let healthCap := maxHealth + maxLifeCrystals * healthPerHeart
  routeRow 624 546 448 "Life Crystals" s!"HP CAP {state.combat.healthLimit}/{healthCap}"
    "Mine rose crystals in caves and supply pockets with any pick."
    s!"Select one and left-click to gain {healthPerHeart} maximum HP."
  text (if state.world.tile magmaMarker == .magmaMarker then
      "Reach Highlands by the vault's east stairs or surface. Magma Gallery branches west from Deepstone Cavern."
    else if state.world.tile cavernAltar == .cavernAltar then
      "Highlands via the east vault stairs or surface. The Deepsteel Mine's east shaft leads to the Deepstone Altar."
    else if state.world.tile (deepsteelMarker state.seed) == .mineMarker then
      "Reach Cedar Highlands via the vault's east stairs or surface. Look east for the torch over the Deepsteel Mine."
    else "Reach Cedar Highlands via the vault's east staircase or by crossing the far eastern surface.")
    48 606 14 mint
  text "Look below torch-marked ledges in the early cave, Frost Cavern and Flooded Quarry. Q claims nearby supplies."
    48 622 14 gold
  text (if evil then
      "Pots can drop bombs. Left-click to throw, then retreat. Aim at a reachable lit bomb and press Q to defuse."
    else if state.legacyEarly then
      "Defeat the cave creature and walk over its drop. Gel makes torches (X), shell and scale make a Hunter Bow (O)."
    else "Defeat cave creatures and walk over their drops. Shell and scale make a Hunter Bow (O).")
    48 638 14 muted
  rect 48 655 1024 1 line
  text (homeRoute state) 48 663 14 mint
  text s!"Coins  {state.coins}       Defense  {state.defense}       Move speed  {state.equipment.speed}"
    624 663 14 muted

private structure Recipe where
  key : String
  product : String
  cost : String
  anywhere : Bool := false
  deriving Inhabited

private def recipes : Array Recipe := #[
  ⟨"B", "Workbench", s!"{workbenchCost} wood", true⟩,
  ⟨"P", s!"{platformBatch} wood platforms", s!"{platformWood} wood", true⟩,
  ⟨"F2", s!"{baitBatch} bait", "1 wood + 1 dirt", true⟩,
  ⟨"E", "Copper pick", s!"{copperPickCost} copper ore", false⟩,
  ⟨"F", "3 torches", "1 wood", false⟩,
  ⟨"X", s!"{gelTorchBatch} torches", "1 cave gel + 1 wood", false⟩,
  ⟨"R", "Stone sword", "2 stone + 1 wood", false⟩,
  ⟨"T", "Crystal pick", s!"{crystalPickCost} crystal", false⟩,
  ⟨"G", "Iron sword", s!"{ironSwordCost} iron ore", false⟩,
  ⟨"Y", "Silver sword", s!"{silverSwordCost} silver ore", false⟩,
  ⟨"U", "Wooden bow", s!"{bowCost} wood", false⟩,
  ⟨"O", "Hunter Bow", s!"1 frost shell + 1 quarry scale + {hunterBowWood} wood", false⟩,
  ⟨"I", s!"{arrowBatch} arrows", "1 wood + 1 stone", false⟩,
  ⟨"W", "Fishing Rod", s!"{rodWood} wood + {rodStone} stone", false⟩,
  ⟨"C", "Chest", s!"{chestCost} wood", false⟩,
  ⟨"V", "Door", s!"{doorCost} wood", false⟩,
  ⟨"N", "Geode Mail", s!"{geodeMailGeodes} geodes + {geodeMailSilver} silver ore", false⟩,
  ⟨"K", "Home Beacon", s!"{beaconWood} wood + {beaconStone} stone", false⟩,
  ⟨"Z", "Recall Charm", s!"{recallCopper} copper ore + {recallCrystal} crystal", false⟩,
  ⟨"F3", "Gills Elixir", "1 cave eel + 1 Sunleaf", false⟩,
  ⟨"F4", "Barkskin Tonic", "1 pond perch + 1 Sunleaf", false⟩,
  ⟨"F6", s!"{railBatch} rails", s!"{railIronCost} iron ore + {railWoodCost} wood", false⟩,
  ⟨"F7", "Minecart", s!"{minecartIronCost} iron ore + {minecartWoodCost} wood", false⟩,
  ⟨"F10", "Grappling Hook", s!"{grapplingHookIronCost} iron ore + {grapplingHookWoodCost} wood", false⟩,
  ⟨"F11", "Highland Crossbow", s!"{highlandCrossbowOreCost} deepsteel ore + {highlandCrossbowWoodCost} wood", false⟩
]

private def drawLegacyRecipes (state : GameState) : IO Unit := do
  rect 48 166 1024 49 surface
  text (if state.nearWorkbench then "WORKBENCH IN REACH" else "WORKBENCH OUT OF REACH")
    65 183 15 (if state.nearWorkbench then mint else gold)
  text "Place a crafted workbench with right-click." 728 183 15 muted
  for i in [:recipes.size] do
    let recipe := recipes[i]!
    let rows := (recipes.size + 2) / 3
    let x : Int := 48 + 348 * (i / rows : Nat)
    let y : Int := 229 + 46 * (i % rows : Nat)
    rect x y 328 43 surface
    rect (x + 12) (y + 4) 32 21 line
    let keyX := if recipe.key.length == 1 then 22 else if recipe.key.length == 2 then 17 else 14
    text recipe.key (x + keyX) (y + 6) 16 mint
    text recipe.product (x + 49) (y + 6) 17 paper
    text recipe.cost (x + 12) (y + 27) 14 muted
    text (if recipe.anywhere then "ANYWHERE" else "WORKBENCH") (x + 237) (y + 8) 12 gold
  text s!"Select a potion and left-click. Gills preserves breath for {gillsTicks / 60}s. Barkskin gives +1 defense for {barkskinTicks / 60}s."
    48 650 14 muted
  text "Esc > Crafting lists furniture, materials and their required stations." 48 668 11 mint

private def drawStarterRecipes (state : GameState) : IO Unit := do
  let lead := (earlyOres state.seed).iron == .leadOre
  let metal := if lead then EarlyMetal.lead else EarlyMetal.iron
  let examples : Array ClassicRecipe := #[.workbench, .torches, .platforms, .furnace,
    .smelt metal, if lead then .leadAnvil else .ironAnvil, .pick metal, .axe metal, .sword metal,
    .woodSword, .woodBow, .arrows]
  rect 48 166 1024 56 surface
  text "ESC > CRAFTING" 65 179 17 mint
  text "Use the arrows to browse recipes. Click a highlighted recipe to craft it." 65 200 15 paper
  text "FIRST RECIPES" 48 240 14 gold
  for index in [:examples.size] do
    let recipe := examples[index]!
    let x := 48 + 348 * (index % 3)
    let y := 265 + 50 * (index / 3)
    let label := if recipe.quantity > 1 then s!"{recipe.item.name} x{recipe.quantity}" else recipe.item.name
    let cost := String.intercalate " + " (recipe.ingredients.map fun (item, quantity) => s!"{quantity} {item.name}")
    rect x y 328 44 surface
    text label (x + 12) (y + 7) 16 paper
    text recipe.stationName (x + 213) (y + 9) 12 gold
    text cost (x + 12) (y + 27) 13 muted
  rect 48 477 1024 1 line
  text "EYE OF CTHULHU" 48 489 14 gold
  text "Craft a Suspicious Looking Eye at an evil altar with 6 Lens." 48 511 15 paper
  text "Use it at night. Smelt its ore for a Demon Bow or Tendon Bow." 48 531 15 muted
  text "BUILD A BEDROOM" 48 558 14 gold
  text "Make a Sawmill at the Work Bench, then build a Loom." 48 579 15 paper
  text "The Loom turns Cobweb into Silk for a Bed." 48 599 15 muted
  if (evilAltars state.seed)[0]?.any (fun altar => state.world.tile altar == altarTile state.seed) then
    let crimson := evilRelicTile state.seed == .crimsonHeart
    let fungus := if crimson then Item.viciousMushroom else .vileMushroom
    let powder := if crimson then Item.viciousPowder else .vilePowder
    let summon := if crimson then Item.bloodySpine else .wormFood
    let pick := if crimson then Item.deathbringerPickaxe else .nightmarePickaxe
    let cost (item : Item) := (ClassicRecipe.all.find? (·.item == item)).map fun recipe =>
      String.intercalate " + " (recipe.ingredients.map fun (material, amount) => s!"{amount} {material.name}")
    text (if crimson then "BRAIN OF CTHULHU" else "EATER OF WORLDS") 589 489 14 gold
    text "Place a Bottle on a bench, table or platform." 589 511 15 paper
    text s!"1 {fungus.name} makes 5 {powder.name} there." 589 531 15 muted
    text "SUMMON AT AN ALTAR  /  FORGE AT AN ANVIL" 589 558 14 gold
    if let some ingredients := cost summon then text s!"{summon.name}: {ingredients}" 589 579 15 paper
    if let some ingredients := cost pick then
      text s!"{pick.name}: {ingredients}" 589 599 15 muted
  text "N Geode Mail   O Hunter Bow   W Wood Fishing Pole   F6 Rails   F7 Minecart   F10 Grappling Hook"
    48 634 14 muted
  text "F3 Gills Elixir   F4 Barkskin Tonic   T Crystal Pick   F11 Crossbow" 48 648 12 muted
  text "Place a station with right-click, then stand nearby while you craft." 48 662 14 mint

private def controlRow (x y : Int) (title keys first second : String) : IO Unit := do
  rect x y 500 1 line
  text title x (y + 6) 16 paper
  text keys (x + 189) (y + 7) 14 gold
  text first x (y + 23) 14 muted
  text second x (y + 37) 14 muted

private def drawControls (legacyEarly : Bool) : IO Unit := do
  text "EXPLORING" 48 171 14 mint
  text "BUILDING & TRADING" 572 171 14 mint
  controlRow 48 197 "Move & swim" "A / D  or  Left / Right     Space"
    "Move with A/D or arrows. Press Space to jump."
    "Hold Space to swim up when submerged."
  controlRow 48 248 "Grappling Hook" "F10     Left click     Up     Space"
    s!"Click toward a solid surface to cast or retarget ({hookMaxReach / tileUnits} tiles)."
    "Hold Up to reel in. Space releases. Change tools while hanging."
  controlRow 48 299 "Wood platforms" (if legacyEarly then "P     Right click     Down" else "Esc     Right click     Down")
    (if legacyEarly then "Craft with P and place with right-click. Hold Down to drop."
      else "Craft in Esc > Crafting. Right-click to place. Hold Down to drop.")
    "Water and light pass through. Lava can destroy wood platforms."
  controlRow 48 350 "Rail building" "F6     F7     Right click"
    "Place tracks in empty tiles. They can float and survive lava."
    "Rails join over one-tile steps. Pick the empty cart, then the rail."
  controlRow 48 401 "Use an item" "Left click     Right click"
    "Hold left-click with a pickaxe to mine or an axe to chop."
    "Click to attack or use an item. Right-click to place blocks."
  controlRow 48 452 "Fishing" (if legacyEarly then "W     F2     Left click" else "W     Left click")
    (if legacyEarly then "Craft a rod (W) and bait (F2). Cast into open water."
      else "Craft a Wood Fishing Pole from eight Wood at a Work Bench.")
    (if legacyEarly then "Left-click when the bobber bites to reel in."
      else "Put bait in your inventory. Cast, then click when the bobber dips.")
  controlRow 48 503 "Inventory" "1-0     Esc"
    "Select with 1-0, scroll, or click a closed hotbar slot."
    "In the pack, left click moves stacks. Right click takes one."
  controlRow 48 554 "Equipment" "Esc     Right click"
    "Right-click armor or an accessory in the open backpack to equip."
    "Click an equipment slot to take the item off."
  controlRow 48 605 "World map" "M     Tab"
    "Hold left click or use arrows to pan. Scroll to zoom."
    "Tab cycles the corner map, overlay and hidden modes."
  controlRow 572 197 "Interact" "Q"
    (if legacyEarly then "Aim at a door, chest, supply cache, resident or altar."
      else "Aim at the Guide or Old Man and press Q to talk.")
    "Q opens doors and chests. Right-click also opens Dungeon chests."
  controlRow 572 248 "Ride minecart" "Q     A/D     F8"
    "Aim at an empty cart and press Q. Hold a direction to drive."
    "Press F8 to dismount. You can board the same cart again."
  controlRow 572 299 "Return home" "Right click     Q     Left click"
    "Place a Home Beacon on dry, solid ground. Aim at it and press Q."
    "Select a Recall Charm and left-click to return. Uses one charm."
  controlRow 572 350 "Assign rooms" "H     J"
    (if legacyEarly then "Aim at a room's door and press H to house the Guide."
      else "Aim inside a room and press H to check it or house the Guide.")
    "Use J at a separate room's door to house the Merchant."
  controlRow 572 401 "Use fish" "Select fish     Left click"
    s!"Pond perch heals {healthPerHeart} HP. Catch it at the west surface pond."
    s!"Cave eel from the quarry restores up to {eelBreathTicks / 60} seconds of breath."
  controlRow 572 452 "Plant & harvest" "Right click     Left click"
    "Find wild Sunleaf or buy seeds. Plant above dry, sunlit dirt."
    "Pick to harvest. Night, shade and water pause growth."
  controlRow 572 503 "Store & trade" "Shift / Ctrl + click"
    "Shift-click a stack to transfer it between you and a chest."
    "Ctrl-click to sell, or click a shop offer to buy."
  controlRow 572 554 "Save & load" "S / F5     L / F9"
    "Save with S or F5. Reload with L or F9."
    "Open inventory and choose Save & Exit to return to worlds."
  controlRow 572 605 "Bed spawn" "Esc     Right click     Q"
    "Craft a bed in Esc > Crafting. Place it in an enclosed, walled room."
    "Aim at the foot and press Q to set or clear your spawn."
  text "F1 opens the guide. It pauses the world while you read." 48 664 14 muted
  text "Highland Crossbow (F11) uses one arrow per straight shot." 572 664 14 muted

def draw (state : GameState) (page mouseX mouseY : Nat) : IO Unit := do
  let page := if page < pageCount then page else 0
  rect 0 0 View.width View.height background
  rect 24 22 (View.width - 48) (View.height - 44) 0x15212cff
  rect 24 22 (View.width - 48) 3 mint
  text "FIELD GUIDE" 48 44 33 paper
  text "Left / Right  Switch tab" 737 59 15 muted
  text "F1 / Esc  Close" 950 59 15 mint
  for index in [:pageCount] do
    let x := tabX index
    let active := index == page
    let hover := pageAt mouseX mouseY == some index
    rect x 104 152 40 (if active then mint else if hover then line else surface)
    text tabs[index]! (x + 16) 115 19 (if active then background else paper)
  rect 48 151 1024 1 line
  match page with
  | 1 => if state.legacyEarly then drawLegacyRecipes state else drawStarterRecipes state
  | 2 => drawControls state.legacyEarly
  | _ => drawRoute state

end Lean4Terria.GuideView
