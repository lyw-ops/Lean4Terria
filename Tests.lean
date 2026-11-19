import Lean4Terria
import Lean4Terria.Lighting
import Lean4Terria.WorldSlots

open Lean4Terria

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def sameSnapshot (left right : GameState) : Bool :=
  left.seed == right.seed && left.world == right.world && left.player == right.player &&
    left.inventory == right.inventory && left.cursorStack == right.cursorStack && left.selected == right.selected && left.combat == right.combat &&
    left.legacyEarly == right.legacyEarly && left.toolCooldown == right.toolCooldown &&
    left.tileHits == right.tileHits && left.gelDrops == right.gelDrops &&
    left.nightDrops == right.nightDrops && left.townGuide == right.townGuide &&
    left.eye == right.eye && left.eyeDefeated == right.eyeDefeated && left.potionSickness == right.potionSickness &&
    left.evilRelicsBroken == right.evilRelicsBroken && left.bombs == right.bombs &&
    left.eater == right.eater && left.brain == right.brain &&
    left.evilBossDefeated == right.evilBossDefeated && left.evilEnemies == right.evilEnemies &&
    left.dungeonInstalled == right.dungeonInstalled && left.lowerDungeonInstalled == right.lowerDungeonInstalled &&
    left.blueMoonDungeonInstalled == right.blueMoonDungeonInstalled && left.blueMoon == right.blueMoon &&
    left.magicMissileDungeonInstalled == right.magicMissileDungeonInstalled && left.magicMissiles == right.magicMissiles &&
    left.deepDungeonInstalled == right.deepDungeonInstalled && left.valor == right.valor &&
    left.sunfury == right.sunfury && left.darkLance == right.darkLance &&
    left.npcBurns == right.npcBurns && left.burningTicks == right.burningTicks &&
    left.hotTileTicks == right.hotTileTicks && left.shadowKeyRecovery == right.shadowKeyRecovery &&
    left.flowerFireballs == right.flowerFireballs && left.hellwingShots == right.hellwingShots &&
    left.flamelashes == right.flamelashes &&
    left.lavaSlimes == right.lavaSlimes && left.underworldFlyers == right.underworldFlyers &&
    left.fireImps == right.fireImps && left.burningSpheres == right.burningSpheres &&
    left.demonSickles == right.demonSickles && left.nextUnderworldNpcId == right.nextUnderworldNpcId &&
    left.boneSerpent == right.boneSerpent && left.underworldCritters == right.underworldCritters &&
    left.netSwingTicks == right.netSwingTicks && left.obsidianSkinTicks == right.obsidianSkinTicks &&
    left.darknessTicks == right.darknessTicks &&
    left.skeletron == right.skeletron &&
    left.skeletronDefeated == right.skeletronDefeated && left.oldManWaiting == right.oldManWaiting &&
    left.dungeonGuardian == right.dungeonGuardian && left.dungeonGuardianHealth == right.dungeonGuardianHealth &&
    left.dungeonSlimes == right.dungeonSlimes && left.darkCasters == right.darkCasters &&
    left.waterSpheres == right.waterSpheres && left.nextDarkCasterId == right.nextDarkCasterId &&
    left.nextWaterSphereId == right.nextWaterSphereId && left.angryBones == right.angryBones &&
    left.nextAngryBonesId == right.nextAngryBonesId &&
    left.cursedSkulls == right.cursedSkulls && left.nextCursedSkullId == right.nextCursedSkullId &&
    left.cursedTicks == right.cursedTicks && left.mana == right.mana &&
    left.manaRegenDelay == right.manaRegenDelay && left.manaRegenProgress == right.manaRegenProgress &&
    left.aquaStreams == right.aquaStreams && left.aquaBurst == right.aquaBurst &&
    left.aquaAnimation == right.aquaAnimation && left.aquaShotDelay == right.aquaShotDelay &&
    left.shortsword == right.shortsword &&
    left.walls == right.walls && left.furniture == right.furniture &&
    left.bedSpawnPoint == right.bedSpawnPoint && left.respawnTicks == right.respawnTicks &&
    left.warden == right.warden && left.surface == right.surface && left.weather == right.weather &&
    left.crateLifeCrystals == right.crateLifeCrystals && left.cratePotion == right.cratePotion &&
    left.angler == right.angler && left.fishingPotion == right.fishingPotion &&
    left.sonarPotion == right.sonarPotion &&
    left.chests == right.chests &&
    left.homeDoor == right.homeDoor && left.homeAnchor == right.homeAnchor &&
    left.guide == right.guide && left.water == right.water && left.queenSlime == right.queenSlime && left.queenMinions == right.queenMinions &&
    left.queenShots == right.queenShots && left.queenSlimeDefeated == right.queenSlimeDefeated &&
    left.nextQueenMinionId == right.nextQueenMinionId && left.magma == right.magma && left.honey == right.honey && left.honeyBuff == right.honeyBuff &&
    left.breath == right.breath && left.quarry == right.quarry && left.sentry == right.sentry &&
    left.storm == right.storm && left.projectiles == right.projectiles &&
    left.npcImmunity == right.npcImmunity &&
    left.bowCooldown == right.bowCooldown && left.chart == right.chart && left.coins == right.coins &&
    left.merchantStock == right.merchantStock && left.merchantDoor == right.merchantDoor &&
    left.merchant == right.merchant && left.equipment == right.equipment && left.raid == right.raid &&
    left.ecology == right.ecology && left.fishing == right.fishing &&
    left.crops == right.crops && left.gills == right.gills && left.barkskin == right.barkskin &&
    left.ruin == right.ruin && left.cavern == right.cavern && left.carts == right.carts && left.ride == right.ride &&
    left.grapple == right.grapple && left.wingedSlime == right.wingedSlime && left.assassinDash == right.assassinDash &&
    left.weaponArmorStats == right.weaponArmorStats && left.dagger == right.dagger &&
    left.minionTarget == right.minionTarget &&
    left.destroyer == right.destroyer && left.destroyerDefeated == right.destroyerDefeated &&
    left.destroyerPending == right.destroyerPending && left.destroyerProbes == right.destroyerProbes &&
    left.nextDestroyerProbeId == right.nextDestroyerProbeId &&
    left.skeletronPrime == right.skeletronPrime && left.primeArms == right.primeArms &&
    left.primeShots == right.primeShots && left.primePending == right.primePending &&
    left.primeDefeated == right.primeDefeated &&
    left.plantera == right.plantera && left.planteraHooks == right.planteraHooks &&
    left.planteraTentacles == right.planteraTentacles && left.planteraSpores == right.planteraSpores &&
    left.planteraShots == right.planteraShots && left.nextPlanteraPartId == right.nextPlanteraPartId &&
    left.planteraBulbs == right.planteraBulbs && left.planteraUnlocked == right.planteraUnlocked &&
    left.planteraDefeated == right.planteraDefeated &&
    left.poisonedTicks == right.poisonedTicks && left.poisonProgress == right.poisonProgress &&
    left.golem == right.golem && left.golemHead == right.golemHead && left.golemFists == right.golemFists &&
    left.golemShots == right.golemShots && left.golemDefeated == right.golemDefeated &&
    left.templeWires == right.templeWires && left.templeCooldowns == right.templeCooldowns &&
    left.templeTrapShots == right.templeTrapShots && left.hellfireTicks == right.hellfireTicks &&
    left.hellfireProgress == right.hellfireProgress && left.bleedingTicks == right.bleedingTicks &&
    left.spikeCooldown == right.spikeCooldown && left.templeEnemies == right.templeEnemies &&
    left.nextTempleEnemyId == right.nextTempleEnemyId

private def historicalTerrain (state : GameState) : World :=
  let baseline := generatePreClassicTerrain state.seed
  (classicResources state.seed).foldl (fun world (position, resource) =>
    if world.tile position == resource then world.setTile position (baseline.tile position)
    else world) state.world

private def sameHistoricalSnapshot (left right : GameState) : Bool :=
  sameSnapshot { left with world := historicalTerrain left } { right with world := historicalTerrain right }

private def fullBackpack : Inventory :=
  Array.replicate Inventory.mainSlotCount (some ⟨.stone, Inventory.maxStack⟩) ++
    Array.replicate 4 none ++ Array.replicate 4 (some ⟨.musketBall, 9999⟩) ++ #[none]

private def walk (state : GameState) (ticks : Nat) (direction : Int) (jump := false) : IO GameState := do
  let mut state := state
  for index in [:ticks] do
    state := step state { horizontal := direction, jump := jump && index == 0 }
    check (!state.player.collides state.world) s!"Player overlapped a tile at movement tick {index}."
  return state

private def waitForRespawn (state : GameState) : IO GameState := do
  check (state.respawnTicks == classicRespawnTicks && state.combat.health == 0)
    "Defeat did not start the Classic respawn countdown."
  let next ← walk state state.respawnTicks 0
  check (next.respawnTicks == 0 && next.combat.health == max maxHealth (state.combat.healthLimit / 2) &&
    next.combat.invulnerable == spawnInvulnerability)
    "The respawn timer or restored life did not follow the Classic rules."
  return next

private def slotFor (state : GameState) (item : Item) : IO Nat := do
  let some slot := state.inventory.findIdx? (fun entry => entry.any (·.item == item))
    | throw (IO.userError s!"Missing {item.name} in inventory.")
  return slot

private def equip (state : GameState) (item : Item) : IO GameState := do
  let slot ← slotFor state item
  if slot < Inventory.hotbarCount then
    return step state { selectSlot := some slot }
  return step state { selectSlot := some (Inventory.hotbarCount - 1), swapSlot := some slot }

private def walkToColumn (state : GameState) (column : Int) (limit : Nat) : IO GameState := do
  let start := state.player.centerTile
  let mut state := state
  for index in [:limit] do
    let current := state.player.centerTile.x
    if current == column && state.player.grounded then return state
    let direction := if current < column then 1 else if current > column then -1 else 0
    let jump := direction != 0 && index > 0 && state.player.grounded && state.player.vx == 0
    state := step state { horizontal := direction, jump }
    check (!state.player.collides state.world) s!"Player overlapped terrain on the way to column {column}."
  throw (IO.userError s!"Could not walk from {repr start} to column {column} in {limit} ticks. Stopped at {repr state.player.centerTile}. {state.status}")

private def playRoute (initial : GameState) (prepareCombat := false) : IO GameState := do
  let earlyCraft := step initial { craft := true }
  check (earlyCraft.inventory == initial.inventory) "Crafting without ore changed the inventory."
  let farMine := step initial { mine := some ⟨35, 42⟩ }
  check (farMine.world == initial.world) "Out-of-range mining changed the world."
  let approach ← walk initial 126 1
  check (approach.player.centerTile == ⟨34, 41⟩) "Could not reach the copper terrace."

  let stone := step approach { mine := some ⟨33, 42⟩ }
  let blocked := step stone { selectSlot := some 1, place := some ⟨34, 41⟩ }
  check (blocked.world == stone.world && blocked.inventory == stone.inventory)
    "Placement inside the player changed game state."
  let placed := step blocked { place := some ⟨33, 39⟩ }
  check (placed.world.tile ⟨33, 39⟩ == .stone && Inventory.count placed.inventory .stone == 0)
    "Placement did not consume exactly one stone block."

  let fullInventory := (fullBackpack).set!
    0 (some ⟨.basicPick, 1⟩)
  let full := { approach with inventory := fullInventory }
  let fullMine := step full { mine := some ⟨35, 42⟩ }
  check (fullMine.world == full.world && fullMine.inventory == full.inventory)
    "A full inventory caused a mined tile to be lost."

  let mut mined := approach
  if prepareCombat then
    for position in [⟨33, 43⟩, ⟨34, 43⟩] do
      mined := step mined { mine := some position }
    check (Inventory.count mined.inventory .stone == 2) "Could not gather stone for the sword."
  for column in [35:38] do
    mined := step mined { mine := some ⟨column, 42⟩ }
    check (mined.world.tile ⟨column, 42⟩ == .air) "Copper tile was not mined."
  let repeated := step mined { mine := some ⟨35, 42⟩ }
  check (Inventory.count mined.inventory .copper == 3 && repeated.inventory == mined.inventory)
    "Copper was lost or duplicated during mining."

  let retreat ← walk mined 27 (-1)
  let climbed ← walk retreat 42 (-1) true
  let bench ← walk climbed 120 (-1)
  let mut crafted := step bench { craft := true }
  check (crafted.selectedStack.any (·.item == .copperPick) &&
    Inventory.count crafted.inventory .copper == 0) "Copper pick crafting failed."

  if prepareCombat then
    crafted := step crafted { craftTorches := true, craftSword := true }
    check (Inventory.count crafted.inventory .torch == 3 &&
      Inventory.count crafted.inventory .stoneSword == 1 &&
      Inventory.count crafted.inventory .wood == startingWood - 2 &&
      Inventory.count crafted.inventory .stone == 0) "Equipment recipes did not apply their costs."
    let pickSlot ← slotFor crafted .copperPick
    crafted := step crafted { selectSlot := some pickSlot }
  let deep ← walk crafted 420 1
  let wrongTool := step deep { selectSlot := some 0, mine := some ⟨64, 49⟩ }
  check (wrongTool.world.tile ⟨64, 49⟩ == .crystal) "The basic pick mined crystal."
  let complete := step deep { mine := some ⟨64, 49⟩ }
  check (Inventory.count complete.inventory .crystal == 1 && complete.world.tile ⟨64, 49⟩ == .air)
    "The copper pick did not collect crystal."
  return complete

private def lightingChecks : IO Unit := do
  let positions : List TilePos := [⟨4, 49⟩, ⟨5, 49⟩, ⟨7, 49⟩,
    ⟨4, 50⟩, ⟨5, 50⟩, ⟨6, 50⟩, ⟨7, 50⟩]
  let dark : World := ⟨12, 60, ByteArray.mk (Array.replicate (12 * 60) Tile.bedrock.code)⟩
  let tunnel := positions.foldl (fun world position => world.setTile position .air) dark
  let tunnel := tunnel.setTile ⟨3, 49⟩ .torch
  let light := computeLighting tunnel defaultSeed ⟨4, 49⟩ 5 2 ⟨-1, -1⟩ false
  check (light.level ⟨4, 49⟩ == 14 && light.level ⟨6, 49⟩ == 12 && light.level ⟨7, 49⟩ == 9)
    "An offscreen torch failed to light a wall or spread around the corner."
  let closed := tunnel.setTile ⟨6, 50⟩ .bedrock
  let blocked := computeLighting closed defaultSeed ⟨4, 49⟩ 5 2 ⟨-1, -1⟩ false
  check (blocked.level ⟨7, 49⟩ == 0) "Torchlight passed through solid rock."

private def playCombatRoute (initial : GameState) : IO GameState := do
  let mut state ← playRoute initial true
  let unsupported := step state { selectSlot := some (← slotFor state .torch), place := some ⟨61, 47⟩ }
  check (unsupported.world == state.world && unsupported.inventory == state.inventory)
    "An unsupported torch was placed."
  state := step unsupported { place := some ⟨61, 49⟩ }
  check (state.world.tile ⟨61, 49⟩ == .torch && !(state.world.tile ⟨61, 49⟩).solid &&
    Inventory.count state.inventory .torch == 2) "Placed torch did not consume one item or was solid."
  let light := computeLighting state.world state.seed ⟨60, 47⟩ 10 4 state.player.centerTile false
  check (light.level ⟨61, 49⟩ == 15 && light.level ⟨62, 49⟩ == 14) "Placed torch did not light the cave."
  let copperPick ← slotFor state .copperPick
  let recovered := step state { selectSlot := some copperPick, mine := some ⟨61, 50⟩ }
  check (recovered.world.tile ⟨61, 49⟩ == .air && Inventory.count recovered.inventory .torch == 3)
    "Removing a torch's last support did not return it to the inventory."
  let tightInventory := (fullBackpack)
    |>.set! 0 (some ⟨.basicPick, 1⟩) |>.set! (Inventory.mainSlotCount - 1) none
  let tight := { state with inventory := tightInventory }
  let blocked := step tight { selectSlot := some 0, mine := some ⟨61, 50⟩ }
  check (blocked.world == tight.world && blocked.inventory == tight.inventory)
    "Removing a torch support with insufficient storage was not atomic."
  for row in [47:49] do
    state := step state { selectSlot := some copperPick, mine := some ⟨64, row⟩ }
  check (state.world.tile ⟨64, 48⟩ == .air) "Could not open the passage beyond the crystal."
  state ← walk state 28 1
  let sword ← slotFor state .stoneSword
  for _ in [:240] do
    if let some crawler := state.combat.crawler then
      let difference := crawler.body.x - state.player.x
      let direction := if difference.natAbs > 2 * tileUnits.toNat then
        (if difference < 0 then -1 else 1) else 0
      state := step state {
        horizontal := direction
        selectSlot := some sword
        mine := some crawler.body.centerTile }
      check (!state.player.collides state.world) "Player overlapped terrain in combat."
    else
      break
  check (state.combat.cleared && state.combat.crawler.isNone && state.player.centerTile.x ≥ 66)
    s!"Could not defeat the crawler with the crafted stone sword at {repr state.player.centerTile}: {repr state.combat}"
  return state

private def respawnCheck : IO Unit := do
  let initial := newLegacyGame defaultSeed
  let some crawler := initial.combat.crawler | throw (IO.userError "Crawler did not spawn.")
  let exposed := { initial with player := crawler.body, combat := { initial.combat with health := 1 } }
  let next ← waitForRespawn (step exposed {})
  check (next.player.centerTile == initial.player.centerTile && next.combat.health == maxHealth &&
    next.inventory == exposed.inventory && next.world == exposed.world)
    "Lethal contact did not safely respawn the player with their world and gear."

private def frostRoute (state : GameState) : IO GameState := do
  let bench ← walkToColumn state 93 260
  check bench.nearWorkbench "The frost cavern workbench is out of reach."
  let upgraded := step bench { craftCrystalPick := true }
  check (Inventory.count upgraded.inventory .crystal == 0 &&
    upgraded.selectedStack.any (·.item == .crystalPick)) "Crystal pick did not consume three crystals and equip."
  let duplicate := step upgraded { craftCrystalPick := true }
  check (duplicate.inventory == upgraded.inventory) "Crafting allowed a duplicate crystal pick."
  let ice : TilePos := ⟨92, 56⟩
  check (bench.world.tile ice == .ice) "The test cavern floor does not contain ice."
  let dug := step duplicate { mine := some ice }
  let iceSlot ← slotFor dug .ice
  let rebuilt := step dug { selectSlot := some iceSlot, place := some ice }
  check (dug.world.tile ice == .air && Inventory.count dug.inventory .ice == 1 &&
    rebuilt.world.tile ice == .ice && Inventory.count rebuilt.inventory .ice == 0)
    "Mining and placing ice did not transfer exactly one block."
  let iron ← walkToColumn duplicate 96 60
  let copperPick ← slotFor iron .copperPick
  let wrongTool := step iron { selectSlot := some copperPick, mine := some ⟨95, 58⟩ }
  check (wrongTool.world == iron.world && wrongTool.inventory == iron.inventory)
    "Copper pick mined iron ore."
  let crystalPick ← slotFor iron .crystalPick
  let mut mined := iron
  for column in [95:99] do
    mined := step mined { selectSlot := some crystalPick, mine := some ⟨column, 58⟩ }
    check (mined.world.tile ⟨column, 58⟩ == .air) s!"Could not mine iron at column {column}."
  check (Inventory.count mined.inventory .iron == ironSwordCost) "Iron vein did not yield four ore."
  -- Jump at each obstacle, including the intact step at the edge of the mined vein.
  let mut returned := mined
  for index in [:100] do
    if returned.nearWorkbench then break
    let jump := index > 0 && returned.player.grounded && returned.player.vx == 0
    returned := step returned { horizontal := -1, jump }
    check (!returned.player.collides returned.world) "Player overlapped terrain while leaving the vein."
  let sword := step returned { craftIronSword := true }
  check (Inventory.count sword.inventory .iron == 0 && sword.selectedStack.any (·.item == .ironSword))
    s!"Could not return to the workbench and craft an iron sword from {repr returned.player.centerTile}."
  return sword

private def backpackChecks : IO GameState := do
  let initial := newLegacyGame defaultSeed
  let filled := (Array.replicate Inventory.hotbarCount (some (Stack.mk .stone Inventory.maxStack)))
    ++ Array.replicate (Inventory.slotCount - Inventory.hotbarCount) none
  let inventory := filled.set! 0 (some ⟨.basicPick, 1⟩)
    |>.set! Inventory.hotbarCount (some ⟨.crystal, crystalPickCost⟩)
  let initial := { initial with inventory }
  let crafted := step initial { craftCrystalPick := true }
  check (crafted.selected == 0 && crafted.inventory[Inventory.hotbarCount]? == some (some ⟨.crystalPick, 1⟩))
    "Crafting with a full hotbar did not store the new tool in the backpack."
  let invalid := step crafted { selectSlot := some Inventory.hotbarCount, swapSlot := some Inventory.slotCount }
  check (invalid.selected == 0 && invalid.inventory == crafted.inventory) "Invalid slot input changed equipment."
  let swapped := step invalid { swapSlot := some Inventory.hotbarCount }
  check (swapped.selectedStack.any (·.item == .crystalPick) &&
    swapped.inventory[Inventory.hotbarCount]? == some (some ⟨.basicPick, 1⟩))
    "A backpack swap lost either item."
  let overflow := step swapped { mine := some ⟨19, 40⟩ }
  check (overflow.world.tile ⟨19, 40⟩ == .air &&
    overflow.inventory[Inventory.hotbarCount + 1]? == some (some ⟨.dirt, 1⟩))
    "Mining with a full hotbar failed to collect the drop in the backpack."
  let fullInventory := (fullBackpack)
    |>.set! 0 (some ⟨.crystal, crystalPickCost + 1⟩)
  let full := { initial with inventory := fullInventory }
  let failed := step full { craftCrystalPick := true }
  check (failed.inventory == full.inventory) "A full inventory consumed crystals during failed crafting."
  return overflow

private def ironSwordCheck : IO Unit := do
  let state := newLegacyGame defaultSeed
  let some enemy := state.combat.crawler | throw (IO.userError "Crawler did not spawn.")
  let player := { enemy.body with x := enemy.body.x - 2 * tileUnits }
  let state := { state with player, inventory := state.inventory.set! 0 (some ⟨.ironSword, 1⟩) }
  let strike := step state { mine := some enemy.body.centerTile }
  check (strike.combat.crawler.any (·.health == 1) && strike.combat.swing == swingCooldown)
    "An iron sword strike did not remove two crawler health."
  let early := step strike { mine := some enemy.body.centerTile }
  check (early.combat.crawler.any (·.health == 1)) "Iron sword bypassed the swing cooldown."

private def saveRoundTrip (state : GameState) : IO Unit := IO.FS.withTempDir fun directory => do
  let path := directory / "world.json"
  match ← Save.write path state with
  | .ok _ => pure ()
  | .error reason => throw (IO.userError reason)
  match ← Save.read path with
  | .ok loaded => check (sameSnapshot loaded state) "Saved state did not round trip."
  | .error reason => throw (IO.userError reason)
  IO.FS.writeFile path "{malformed"
  match ← Save.read path with
  | .ok _ => throw (IO.userError "Malformed save was accepted.")
  | .error _ => check ((← IO.FS.readFile path) == "{malformed") "Bad save file was modified."

private def legacyLiquid (liquid : Water) : Lean.Json := Id.run do
  let mut entries := #[]
  for index in [:liquid.levels.size] do
    let eighths := (liquid.levels[index]!.toNat * 8 + 127) / Water.capacity
    if eighths > 0 then entries := entries.push (Lean.toJson #[index, eighths])
  return .arr entries

private def classicInventoryChecks : IO Unit := do
  let initial := newGame defaultSeed
  let some copper := Inventory.creditCoins initial.inventory 99
    | throw (IO.userError "Could not store copper.")
  let some coins := Inventory.creditCoins copper 1
    | throw (IO.userError "Could not make change.")
  check ((coins[Inventory.coinStart]?.join) == some ⟨.silverCoin, 1⟩ && Inventory.coinBalance coins == 100)
    "Copper coins did not convert to silver in the dedicated slots."
  let inventory := coins.set! 9 (some ⟨.goldBow, 1⟩)
    |>.set! Inventory.hotbarCount (some ⟨.arrow, 5⟩)
    |>.set! Inventory.ammoStart (some ⟨.musketBall, 10⟩)
    |>.set! (Inventory.ammoStart + 1) (some ⟨.unholyArrow, 3⟩)
    |>.set! (Inventory.ammoStart + 2) (some ⟨.arrow, 3⟩)
  let state := { initial with inventory }
  let split := step state { inventoryClick := some (Inventory.hotbarCount, true) }
  check (split.cursorStack == some ⟨.arrow, 1⟩ && Inventory.count split.inventory .arrow == 7)
    "Right-click did not take one arrow into the cursor."
  let rejected := step split { inventoryClick := some (Inventory.coinStart, false) }
  check (rejected.cursorStack == split.cursorStack && rejected.inventory == split.inventory)
    "The coin column accepted ammunition."
  let merged := step rejected { inventoryClick := some (Inventory.ammoStart + 2, false) }
  check (merged.cursorStack.isNone && merged.inventory[Inventory.ammoStart + 2]?.join == some ⟨.arrow, 4⟩)
    "The ammo column did not merge the cursor item."
  let target : TilePos := ⟨merged.player.centerTile.x + 4, merged.player.centerTile.y - 2⟩
  let fired := step merged { selectSlot := some 9, mine := some target }
  check (fired.selected == 9 && fired.inventory[Inventory.ammoStart + 1]?.join == some ⟨.unholyArrow, 2⟩ &&
    fired.inventory[Inventory.hotbarCount]?.join == some ⟨.arrow, 4⟩ &&
    fired.inventory[Inventory.ammoStart]?.join == some ⟨.musketBall, 10⟩)
    "A shot from hotbar slot 10 did not spend the first compatible dedicated ammo."
  let held := step fired { inventoryClick := some (Inventory.coinStart, false) }
  check (held.cursorStack == some ⟨.silverCoin, 1⟩ && held.coins == 0)
    "Lifting coins left spendable currency behind in the inventory."
  saveRoundTrip held
  let trashed := step held { inventoryClick := some (Inventory.trashSlot, false) }
  check (trashed.cursorStack.isNone && trashed.coins == 0 &&
    trashed.inventory[Inventory.trashSlot]?.join == some ⟨.silverCoin, 1⟩)
    "Putting coins in the trash counted them toward purchases."
  saveRoundTrip trashed
  let packed := { initial with inventory := fullBackpack.set! 0 (some ⟨.wood, 11⟩) }
  check (packed.canCraftClassic .workbench true && !packed.canCraftClassic .workbench)
    "Crafting into the cursor incorrectly depended on a free inventory slot."
  let bench := step packed { craftClassic := some .workbench, craftToCursor := true }
  check (bench.cursorStack == some ⟨.workbench, 1⟩ && Inventory.count bench.inventory .wood == 1 &&
    Inventory.count bench.inventory .workbench == 0) "Crafting did not put the output only on the cursor."

private def legacyJson (state : GameState) (version slotCount : Nat) : IO Lean.Json := do
  let json ← (Lean.Json.parse (Save.encode state)).mapError IO.userError |> IO.ofExcept
  let slots ← ((json.getObjVal? "inventory") >>= Lean.Json.getArr?).mapError IO.userError |> IO.ofExcept
  let ecology ← ((json.getObjVal? "ecology") >>= Lean.Json.getArr?).mapError IO.userError |> IO.ofExcept
  let combat ← (json.getObjVal? "combat").mapError IO.userError |> IO.ofExcept
  let chests ← ((json.getObjVal? "chests") >>= Lean.Json.getArr?).mapError IO.userError |> IO.ofExcept
  let chests ← chests.mapM fun chest => do
    let slots ← ((chest.getObjVal? "slots") >>= Lean.Json.getArr?).mapError IO.userError |> IO.ofExcept
    return chest.setObjVal! "slots" (.arr (slots.extract 0 16))
  let stockCount := if version < 9 then 3 else if version < 14 then 5
    else if version < 49 then shopOffers.size else state.merchantStock.size
  let habitatCount := if version < 20 then 3 else ecology.size
  let oldSlots := slots.extract 0 8 ++ slots.extract Inventory.hotbarCount (Inventory.hotbarCount + 16)
  let json := json.setObjVal! "version" (Lean.toJson version)
    |>.setObjVal! "inventory" (.arr (oldSlots.extract 0 slotCount))
    |>.setObjVal! "chests" (.arr chests)
    |>.setObjVal! "merchantStock" (Lean.toJson (state.merchantStock.extract 0 stockCount))
    |>.setObjVal! "ecology" (.arr (ecology.extract 0 habitatCount))
    |>.setObjVal! "combat" (combat.setObjVal! "health" (Lean.toJson (state.combat.health / healthPerHeart)))
  return if version < 23 then json.setObjVal! "water" (legacyLiquid state.water)
      |>.setObjVal! "magma" (legacyLiquid state.magma)
    else json

private def legacyMigrationChecks : IO Unit := do
  let seed := UInt64.ofNat (UInt64.size - 1)
  let initial := newLegacyGame seed
  let legacy := generateLegacyTerrain seed
  let edited : TilePos := ⟨95, 58⟩
  let world := legacy.setTile edited .dirt
  let state := { initial with world, selected := 5 }
  let v2 ← legacyJson state 2 8
  let migrated ← (Save.decode v2.compress).mapError IO.userError |> IO.ofExcept
  let original := generatePrePocketsTerrain seed
  let expectedWorld := (supplyPockets seed).foldl (fun world pocket =>
    if pocket.footprint.contains edited then
      pocket.edits.foldl (fun world (position, _) => world.setTile position (original.tile position)) world
    else world) initial.world
  let expected := { state with world := expectedWorld.setTile edited .dirt }
  check (sameHistoricalSnapshot migrated expected) "Version 2 migration changed legacy edits or failed to add the cavern."
  let tiles := ByteArray.mk (world.tiles.data.map fun code =>
    if code == Tile.timber.code then Tile.air.code else code)
  let v1 ← legacyJson { state with world := { world with tiles } } 1 6
  let migrated ← (Save.decode v1.compress).mapError IO.userError |> IO.ofExcept
  check (migrated.seed == seed && migrated.player == state.player && migrated.selected == 5 &&
    migrated.inventory == state.inventory && migrated.combat == initialCombat legacy &&
    migrated.world.tile ⟨14, 39⟩ == .air && migrated.world.tile edited == .dirt &&
    migrated.world.tile ⟨96, 58⟩ == .iron)
    "Version 1 migration failed to preserve equipment and terrain edits while adding the cavern."
  let badSelected := v2.setObjVal! "selected" (Lean.toJson Inventory.hotbarCount)
  check ((Save.decode badSelected.compress).isOk == false) "A legacy save selected a backpack slot."
  let newSelected := (← (Lean.Json.parse (Save.encode initial)).mapError IO.userError |> IO.ofExcept)
    |>.setObjVal! "selected" (Lean.toJson Inventory.hotbarCount)
  check ((Save.decode newSelected.compress).isOk == false) "A current save selected a backpack slot."

  let builtWall : TilePos := ⟨150, 64⟩
  let editedV3 := (generateV3Terrain seed).setTile builtWall .dirt
  let v3 ← legacyJson { initial with world := editedV3 } 3 24
  let migrated ← (Save.decode v3.compress).mapError IO.userError |> IO.ofExcept
  check (sameHistoricalSnapshot migrated { initial with world := initial.world.setTile builtWall .dirt } &&
    migrated.world.tile ⟨156, 65⟩ == .air && migrated.warden == .ready)
    "Version 3 migration failed to introduce the ice hall while preserving a player's wall."

  let initial := newLegacyGame defaultSeed
  let torch : TilePos := ⟨76, 47⟩
  let world := (generateLegacyTerrain defaultSeed).setTile torch .torch
  let bodyAt (col floor : Int) : Player :=
    { x := col * tileUnits + (tileUnits - playerWidth) / 2
      y := floor * tileUnits - playerHeight
      vx := 0, vy := 0, grounded := true }
  let player := bodyAt 81 44
  check (player.collides initial.world && !player.collides world) "Legacy body fixture no longer tests a collision."
  let v2 ← legacyJson { initial with world, player } 2 8
  let loaded ← (Save.decode v2.compress).mapError IO.userError |> IO.ofExcept
  check (loaded.player == player && !loaded.player.collides loaded.world &&
    loaded.world.tile torch == .torch && (loaded.world.tile ⟨77, 47⟩).solid)
    "Cavern migration trapped a legacy player or detached a placed torch."
  let player := bodyAt 85 45
  let v2 ← legacyJson { initial with world, player } 2 8
  let loaded ← (Save.decode v2.compress).mapError IO.userError |> IO.ofExcept
  check (initial.world.tile ⟨85, 45⟩ == .air && loaded.player == player &&
    (loaded.world.tile ⟨85, 45⟩).solid) "Cavern migration removed the player's floor."

private def waitForWardenPhase (state : GameState) (phase : WardenPhase) : IO GameState := do
  let mut state := state
  for _ in [:600] do
    if let .fighting boss := state.warden then
      if boss.phase == phase then return state
    state := step state {}
  throw (IO.userError s!"The Frost Warden did not reach {repr phase}.")

private def wardenRetryChecks (gate : GameState) : IO GameState := do
  let entered ← walkToColumn gate 148 120
  let .fighting _ := entered.warden | throw (IO.userError "The Frost Warden did not awaken on entry.")
  let retreat ← walkToColumn entered 137 160
  check (retreat.warden == .ready && retreat.inventory == gate.inventory)
    "Retreat did not reset the Warden while preserving gear."
  let telegraph ← waitForWardenPhase entered .windup
  saveRoundTrip telegraph
  let json ← (Lean.Json.parse (Save.encode telegraph)).mapError IO.userError |> IO.ofExcept
  let warden ← (json.getObjVal? "warden").mapError IO.userError |> IO.ofExcept
  let invalid := json.setObjVal! "warden" (warden.setObjVal! "ticks" (Lean.toJson (1000 : Nat)))
  check (!((Save.decode invalid.compress).isOk)) "A Warden save accepted an impossible windup timer."
  let mut state := telegraph
  let mut wasHurt := false
  for _ in [:2400] do
    if state.warden == .ready then break
    let next := step state {}
    if next.combat.health < state.combat.health then wasHurt := true
    state := next
    check (!state.player.collides state.world) "Warden combat or respawn pushed the player into a wall."
  let respawned ← waitForRespawn state
  check (wasHurt && respawned.warden == .ready && respawned.player.centerTile.x < 32 &&
    respawned.inventory == gate.inventory && respawned.world == gate.world)
    "Repeated charges did not lead to a world respawn with the player's gear."
  return retreat

private def fightWarden (initial : GameState) : IO GameState := do
  let mut state := initial
  let mut sawSecondCharge := false
  let mut jumpedOverCharge := false
  for _ in [:2400] do
    let .fighting boss := state.warden | break
    sawSecondCharge := sawSecondCharge || (boss.enraged && boss.phase == .charge)
    let distance := boss.body.x - state.player.x
    let jump := boss.phase == .charge && state.player.grounded &&
      boss.facing * distance < 0 && distance.natAbs ≤ 3 * tileUnits.toNat
    let direction := if boss.phase == .recover && distance.natAbs > tileUnits.toNat then
        (if distance > 0 then 1 else -1) else 0
    let strike := boss.phase == .recover && state.combat.swing ≤ 1 &&
      distance.natAbs ≤ (tileUnits * 5 / 2).toNat &&
      (state.player.y - boss.body.y).natAbs ≤ playerHeight.toNat
    let target := if strike then some boss.body.centerTile else none
    let next := step state { horizontal := direction, jump := jump, mine := target }
    if boss.phase == .charge && !next.player.grounded && state.combat.health == next.combat.health then
      if let .fighting moved := next.warden then
        if (moved.body.x - next.player.x) * distance < 0 then jumpedOverCharge := true
    check (!next.player.collides next.world) "A fight tick put the player in solid terrain."
    state := next
  check sawSecondCharge "The replay did not face a second-phase charge."
  check jumpedOverCharge "The replay never jumped over a charge without taking damage."
  let .reward _ := state.warden
    | throw (IO.userError s!"The iron sword replay did not defeat the Warden: {repr state.warden}.")
  return state

private def wardenRewardChecks (won : GameState) : IO GameState := do
  let .reward position := won.warden | throw (IO.userError "The Warden left no blade to collect.")
  check (Inventory.count won.inventory .frostbrand == 0) "The Warden reward was granted before pickup."
  saveRoundTrip won
  let loot ← walkToColumn won position.x 120
  let collected := step loot {}
  check (collected.warden == .cleared && Inventory.count collected.inventory .frostbrand == 1)
    "Walking to the Warden's drop did not collect Frostbrand."
  let next ← walk collected 30 0
  check (next.warden == .cleared && next.inventory == collected.inventory)
    "The defeated Warden or its reward returned."
  -- The drop stays in the world when every slot is occupied and can be collected after freeing one.
  let inventory := (fullBackpack)
    |>.set! 0 (some ⟨.stone, 1⟩)
  let full := { won with selected := 0, inventory }
  let near ← walkToColumn full position.x 120
  let blocked := step near {}
  check (blocked.warden == .reward position && blocked.inventory == full.inventory)
    "A full inventory caused the Warden's drop to disappear."
  let buildingSpot : TilePos := ⟨position.x + 2, position.y - 1⟩
  let freed := step blocked { place := some buildingSpot }
  check (freed.world.tile buildingSpot == .stone && freed.warden == .cleared &&
    Inventory.count freed.inventory .frostbrand == 1)
    "Freeing a slot did not allow the remaining Warden drop to be collected."
  return ← equip collected .frostbrand

private def buildWith (state : GameState) (item : Item) (position : TilePos) (tile : Tile) :
    IO GameState := do
  let equipped ← equip state item
  let built := step equipped { place := some position }
  check (built.world.tile position == tile)
    s!"Could not place {item.name} at {repr position} from {repr built.player.centerTile}. {built.status}"
  return built

private def buildWallTile (state : GameState) (position : TilePos) : IO GameState :=
  if Inventory.count state.inventory .stone > 0 then buildWith state .stone position .stone
  else buildWith state .dirt position .dirt

private def mineLifeCrystalAt (state : GameState) (position : TilePos) : IO GameState := do
  let pick ← equip state .basicPick
  check (pick.canReach position && pick.chart.tile? pick.world position == some .lifeCrystal)
    s!"The Life Crystal at {repr position} is out of sight or reach from {repr pick.player.centerTile}."
  let mined := step pick { mine := some position }
  check (mined.world.tile position == .air && mined.chart.tile? mined.world position == some .air &&
    Inventory.count mined.inventory .lifeCrystal == Inventory.count pick.inventory .lifeCrystal + 1)
    s!"Mining the Life Crystal at {repr position} did not update the world, chart and inventory. {mined.status}"
  return mined

private def useMinedHeart (state : GameState) : IO GameState := do
  let equipped ← equip state .lifeCrystal
  let next := step equipped { mine := some equipped.player.centerTile }
  check (next.combat.healthLimit == equipped.combat.healthLimit + healthPerHeart &&
    next.combat.health == equipped.combat.health + healthPerHeart &&
    Inventory.count next.inventory .lifeCrystal + 1 == Inventory.count equipped.inventory .lifeCrystal)
    "A Life Crystal did not add exactly one current and permanent heart."
  return next

private def minePocketHeart (state : GameState) (index : Nat) : IO GameState := do
  let pocket := (supplyPockets state.seed)[index]!
  let heart := (lifeCrystalSites state.seed)[index]!.position
  let direction : Int := if pocket.cache.x < pocket.mouth.x then -1 else 1
  let lip := pocket.mouth.x - direction
  let second := { pocket.mouth with x := pocket.mouth.x + direction }
  let atLip ← walkToColumn state lip 1650
  let pick ← equip atLip .basicPick
  let openMouth := step (step pick { mine := some pocket.mouth }) { mine := some second }
  let lower := { second with y := second.y + 1 }
  let descent := if openMouth.world.tile lower == .stone then step openMouth { mine := some lower } else openMouth
  let mut below ← equip descent .ironSword
  for tick in [:330] do
    let attack := match below.ecology.sites[index]? with
      | some (.alive resident) =>
        let facing : Int := if resident.body.x < below.player.x then -1 else 1
        if swordReaches below.world below.player resident.body facing true then some resident.body.centerTile else none
      | _ => none
    if attack.isNone && below.canReach heart && below.chart.tile? below.world heart == some .lifeCrystal then break
    let horizontal := if attack.isSome || below.player.centerTile.x == heart.x then 0 else direction
    let next := step below { horizontal, mine := attack, jump := tick > 0 && horizontal != 0 &&
      below.player.grounded && below.player.vx == 0 }
    check ((next.player.x - below.player.x).natAbs ≤ (15 * tileUnits).toNat)
      s!"Defeated while approaching the Life Crystal in pocket {index + 1} at {repr below.player.centerTile}."
    below := next
  let crystal ← mineLifeCrystalAt below heart
  let climbed ← walkToColumn crystal lip 490
  let capped ← buildWith climbed .stone pocket.mouth .stone
  buildWith capped .stone second .stone

private def buildHome (hero : GameState) : IO GameState := do
  let pick ← equip hero .basicPick
  let mut state ← walkToColumn pick 15 2300
  for column in [14, 12, 10] do
    state ← walkToColumn state (column + 1) 120
    let floor := groundHeight state.seed column.toNat
    for row in [floor - 4:floor] do
      let position : TilePos := ⟨column, row⟩
      if state.world.tile position == .timber then
        state := step state { mine := some position }
        check (state.world.tile position == .air) s!"Could not gather wood at {repr position}."
  let miner ← walkToColumn state 27 250
  check (!miner.nearWorkbench) "Workbench recipe test must start away from a workbench."
  state := miner
  for column in [25:30] do
    for row in [41:43] do
      state := step state { mine := some ⟨column, row⟩ }
  for column in [26:29] do
    state := step state { mine := some ⟨column, 43⟩ }
  check (Inventory.count state.inventory .stone + Inventory.count state.inventory .dirt ≥ 13)
    "Could not gather the walls and roof from the cave entrance."
  state := step state { craftWorkbench := true }
  check (Inventory.count state.inventory .workbench == 1) "Could not craft a portable workbench."

  state ← walkToColumn state 22 90
  for row in [36:40] do
    state ← buildWallTile state ⟨24, row⟩
  for column in [18:25] do
    state ← walkToColumn state (max 19 (min 23 column)) 90
    state ← buildWallTile state ⟨column, 35⟩
  state ← walkToColumn state 20 90
  for row in [36:38] do
    state ← buildWallTile state ⟨18, row⟩

  state ← buildWith state .workbench ⟨22, 39⟩ .workbench
  check state.nearWorkbench "Placed workbench did not enable recipes."
  state := step state { craftDoor := true, craftChest := true, craftTorches := true }
  state ← buildWith state .chest ⟨23, 39⟩ .chest
  let door : TilePos := ⟨18, 39⟩
  state ← buildWith state .door door .doorClosed
  check (state.world.tile ⟨18, 38⟩ == .doorClosedTop) "Placed door is missing the top half."
  let dark := step state { claimHome := some door }
  check (dark.guide.isNone && dark.homeDoor.isNone) "The Guide accepted a dark room."
  state ← buildWith dark .torch ⟨19, 37⟩ .torch
  let claimed := step state { claimHome := some door }
  check (claimed.homeDoor == some door && claimed.guide.isSome) s!"The Guide would not move in. {claimed.status}"
  let opened := step claimed { interact := some ⟨18, 38⟩ }
  check (opened.world.tile door == .doorOpen && opened.guide == claimed.guide)
    "Opening the door invalidated the assigned room."
  let threshold ← walkToColumn opened door.x 60
  let blocked := step threshold { interact := some door }
  check (blocked.world.tile door == .doorOpen) "The door closed on the player."
  let clear ← walkToColumn blocked 20 60
  let closed := step clear { interact := some door }
  check (closed.world.tile door == .doorClosed) "An unobstructed door could not close."

  let equipped ← equip closed .basicPick
  let roof : TilePos := ⟨20, 35⟩
  let roofTile := equipped.world.tile roof
  let leak := step equipped { mine := some roof }
  check (leak.world.tile roof == .air && leak.guide.isNone && leak.homeDoor == some door)
    "Opening a roof gap did not make the Guide leave."
  let material := if roofTile == .stone then Item.stone else Item.dirt
  let repaired ← buildWith leak material roof roofTile
  check (repaired.guide.isSome) "The Guide did not return after a roof repair."
  return repaired

private def storageAndNight (home : GameState) : IO GameState := IO.FS.withTempDir fun directory => do
  let position : TilePos := ⟨23, 39⟩
  let slot ← slotFor home .frostbrand
  let opened := step home { interact := some position }
  check (opened.openChest == some position) "Could not open the placed chest."
  let packed := (Array.replicate chestSlots (some ⟨Item.stone, Inventory.maxStack⟩))
    |>.set! 0 (some ⟨.stone, Inventory.maxStack - 1⟩)
  let almostFull := { opened with
    inventory := opened.inventory.set! 0 (some ⟨.stone, 2⟩)
    chests := #[⟨position, packed⟩] }
  let rejected := step almostFull { chestDeposit := some 0 }
  check (rejected.inventory == almostFull.inventory && rejected.chests == almostFull.chests)
    "A stack that did not fit was partially transferred."
  let stored := step opened { chestDeposit := some slot }
  let chestHasBlade (state : GameState) := (state.chestAt position).any
    (fun chest => Inventory.count chest.slots .frostbrand == 1)
  check (chestHasBlade stored && Inventory.count stored.inventory .frostbrand == 0)
    "The chest did not take the Warden reward."
  let close := step stored { closeChest := true }
  let equipped ← equip close .basicPick
  let blocked := step equipped { mine := some position }
  check (blocked.world.tile position == .chest && chestHasBlade blocked) "Mining lost a filled chest."
  let path := directory / "home.json"
  IO.ofExcept <| (← Save.write path blocked).mapError IO.userError
  let loaded ← IO.ofExcept <| (← Save.read path).mapError IO.userError
  check (sameSnapshot loaded blocked && loaded.openChest.isNone) "The saved home did not round trip."
  let opened := step loaded { interact := some position }
  let retrieved := step opened { chestWithdraw := some 0 }
  check (Inventory.count retrieved.inventory .frostbrand == 1 && !chestHasBlade retrieved)
    "The Frostbrand was lost or duplicated during save, load, and retrieval."
  let closed := step retrieved { closeChest := true }
  let untilNight := (cycleTicks + dayTicks - closed.surface.clock) % cycleTicks
  let night ← walk closed (untilNight + 650) 0
  check (isNight night.surface.clock && night.surface.creature.isSome &&
    night.combat.health ≥ closed.combat.health && night.guide.isSome)
    s!"The occupied closed home did not protect the player on the first night. {repr night.surface}"
  saveRoundTrip night
  let json ← IO.ofExcept <| (Lean.Json.parse (Save.encode night)).mapError IO.userError
  let surface ← IO.ofExcept <| (json.getObjVal? "surface").mapError IO.userError
  let creature ← IO.ofExcept <| (surface.getObjVal? "creature").mapError IO.userError
  let body ← IO.ofExcept <| (creature.getObjVal? "body").mapError IO.userError
  let fast := body.setObjVal! "vx" (Lean.toJson (nightCreatureMaxSpeed + 1))
  let invalid := json.setObjVal! "surface" (surface.setObjVal! "creature" (creature.setObjVal! "body" fast))
  check (!(Save.decode invalid.compress).isOk) "A night slime save accepted an impossible speed."
  let exposed := step night { interact := some position }
  let creature : NightCreature := { body := exposed.player, health := 1, hopIn := 0 }
  let lethal := { exposed with
    combat := { exposed.combat with health := 1, invulnerable := 0 }
    surface := { exposed.surface with creature := some creature } }
  let recovered ← waitForRespawn (step lethal {})
  check (exposed.openChest.isSome && recovered.openChest.isNone &&
    recovered.combat.health == maxHealth && recovered.chests == exposed.chests)
    "Respawning with a chest open did not close it safely."
  let dawn ← walk night (cycleTicks - night.surface.clock) 0
  check (dawn.surface.clock == 0 && dawn.surface.creature.isNone)
    "Dawn did not reset the clock and clear the night creature."
  return dawn

private def waterVolume (state : GameState) : Nat :=
  state.water.levels.data.foldl (fun total level => total + level.toNat) 0

private def magmaVolume (state : GameState) : Nat :=
  state.magma.levels.data.foldl (fun total level => total + level.toNat) 0

private def waterIn (state : GameState) (left right top bottom : Nat) : Nat := Id.run do
  let mut amount := 0
  for y in [top:bottom] do
    for x in [left:right] do
      amount := amount + state.water.level state.world ⟨x, y⟩
  return amount

private def swimEastTo (initial : GameState) (column : Int) (limit : Nat) : IO GameState := do
  let mut state := initial
  for index in [:limit] do
    if state.player.centerTile.x == column then return state
    let jump := index > 0 && state.player.grounded && state.player.vx == 0
    state := step state { horizontal := 1, jump := jump, swim := true }
    check (!state.player.collides state.world && state.combat.health > 0)
      "The swimming route overlapped a tile or exhausted player health."
  throw (IO.userError s!"Could not swim east to {column} from {repr state.player.centerTile}.")

private def quarryEntrance (hero : GameState) : IO GameState := do
  let pick ← equip hero .crystalPick
  let wall ← walkToColumn pick 163 1600
  let opened := step (step wall { mine := some ⟨164, 64⟩ }) { mine := some ⟨164, 65⟩ }
  check (opened.world.tile ⟨164, 64⟩ == .air && opened.world.tile ⟨164, 65⟩ == .air)
    "Could not open the quarry behind the Warden hall."
  return ← walkToColumn opened 214 550

private def dryQuarryRoute (ledge : GameState) : IO GameState := do
  let crossed ← walk ledge 38 1 true
  check (crossed.player.centerTile.x ≥ 218 && crossed.player.centerTile.y ≤ 65)
    "Could not jump across the reservoir entrance on the dry route."
  let oldEdge ← walkToColumn crossed 256 390
  check (oldEdge.player.centerTile.y == 65) "The dry route did not pass through the old world's edge."
  let secondLedge ← walkToColumn oldEdge 271 160
  let crossed ← walk secondLedge 37 1 true
  let bench ← walkToColumn crossed 281 110
  check bench.nearWorkbench "The dry route did not reach the new quarry workbench."
  return bench

private def quarryWaterRoute (ledge : GameState) : IO GameState := do
  let shaft ← walkToColumn ledge 216 110
  let submerged ← swimEastTo shaft 231 370
  let amount := waterVolume submerged
  check (waterIn submerged 220 235 74 76 == 30 * Water.capacity && waterIn submerged 236 258 70 78 == 0)
    "The sealed reservoir lost water before the sluice was dug."
  let lowered ← walk submerged 30 0
  check (lowered.breath < maxBreath) "Submerging the player did not reduce air."
  let mut drowning := lowered
  for _ in [:maxBreath + 5 * hurtCooldown] do
    if drowning.respawnTicks > 0 then break
    drowning := step drowning {}
  let drowned ← waitForRespawn drowning
  check (drowned.player.centerTile.x < 24 && drowned.combat.health == maxHealth &&
    drowned.inventory == lowered.inventory && drowned.breath == maxBreath)
    "Drowning did not return the player to the surface with their gear."
  let surfaced := step lowered { swim := true }
  check (surfaced.player.y < lowered.player.y) "Holding swim did not start an upward stroke."
  let playerAtSluice ← swimEastTo surfaced 233 90
  let released := step playerAtSluice { mine := some quarrySluice }
  check (released.world.tile quarrySluice == .air) "The quarry sluice could not be mined."
  let flowing ← walk released 60 0
  check (waterIn flowing 236 258 70 78 > 0 && waterVolume flowing == amount)
    "Water did not flow downstream conservatively after opening the sluice."
  saveRoundTrip flowing
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode flowing)).mapError IO.userError
  let continued ← walk flowing 24 0
  let resumed ← walk loaded 24 0
  check (sameSnapshot continued resumed && flowing.water != resumed.water)
    "Reloading a flowing sluice changed or stopped the simulation."

  let mut ready ← equip flowing .stone
  let mut builtDam := none
  for _ in [:80] do
    if ready.water.level ready.world quarrySluice > 0 then
      let placed := step ready { place := some quarrySluice }
      if placed.world.tile quarrySluice == .stone then
        builtDam := some placed
        break
      ready := placed
    else ready := step ready {}
  let some dam := builtDam | throw (IO.userError "A wet sluice could not be closed by building a dam.")
  check (waterVolume dam == amount && dam.water.level dam.world quarrySluice == 0)
    "Dam placement destroyed water or left water inside a solid tile."
  let damSettled ← walk dam 40 0
  check (waterIn damSettled 220 235 74 76 == waterIn dam 220 235 74 76)
    "Water crossed the rebuilt dam."

  let pick ← equip damSettled .crystalPick
  let openLow := step pick { mine := some quarrySluice }
  let openBoth := step openLow { mine := some ⟨quarrySluice.x, quarrySluice.y - 1⟩ }
  let oldEdge ← swimEastTo openBoth 256 410
  check (oldEdge.player.centerTile.y ≥ 70 && waterVolume oldEdge == amount)
    "The low water route did not pass through the old world's edge."
  let exit ← swimEastTo oldEdge 275 420
  let bench ← walkToColumn exit 281 140
  check (bench.nearWorkbench && bench.combat.health ≥ ledge.combat.health)
    "The water route could not reach the workbench safely."
  return bench

private def quarrySwordRoute (bench : GameState) : IO GameState := do
  let vein ← walkToColumn bench 286 100
  let first : TilePos := ⟨285, 66⟩
  let wrong ← equip vein .copperPick
  check ((step wrong { mine := some first }).world.tile first == .silver)
    "Copper pick mined the new silver ore."
  let mut mined ← equip vein .crystalPick
  for x in [285:289] do
    mined := step mined { mine := some ⟨x, 66⟩ }
  check (Inventory.count mined.inventory .silver == silverSwordCost)
    "The reachable silver vein did not yield the recipe's four ore."
  let workbench ← walkToColumn mined 281 120
  let forged := step workbench { craftSilverSword := true }
  check (Inventory.count forged.inventory .silver == 0 &&
    Inventory.count forged.inventory .silverSword == 1)
    "Could not forge a silver sword at the quarry workbench."
  let sword ← equip forged .silverSword
  let mut state ← walkToColumn sword 294 390
  for _ in [:220] do
    let some enemy := state.quarry.creature | break
    let dx := enemy.body.x - state.player.x
    let direction := if dx.natAbs > tileUnits.toNat then (if dx < 0 then -1 else 1) else 0
    state := step state { horizontal := direction, mine := some enemy.body.centerTile }
  check (state.quarry.cleared && state.quarry.creature.isNone &&
    state.selectedStack.any (fun stack => stack.item.legacySwordDamage == 4))
    "The forged silver sword could not defeat the quarry crawler."
  return state

private def rangedKit (home : GameState) (batches := 4) : IO GameState := do
  let westDoor : TilePos := ⟨18, 39⟩
  let opened := step home { interact := some westDoor }
  let pick ← equip opened .basicPick
  let mut state := pick
  for (stand, columns) in ([(9, [8, 9]), (7, [6, 7]), (11, [11, 13])] : List (Int × List Nat)) do
    state ← walkToColumn state stand 250
    for column in columns do
      let floor := groundHeight state.seed column
      for row in [floor - 4:floor] do
        let position : TilePos := ⟨column, row⟩
        if state.world.tile position == .timber then
          state := step state { mine := some position }
  let workbench ← walkToColumn state 17 260
  let bow := step workbench { craftBow := true, craftDoor := true }
  check (Inventory.count bow.inventory .bow == 1 && Inventory.count bow.inventory .door == 1)
    s!"Could not craft a wooden bow and a second door. {bow.status}"
  let inside ← walkToColumn bow 21 70
  let pick ← equip inside .basicPick
  let eastDoor : TilePos := ⟨24, 38⟩
  let opening := step (step pick { mine := some ⟨24, 37⟩ }) { mine := some eastDoor }
  let door ← buildWith opening .door eastDoor .doorClosed
  let sameRoom := step door { claimMerchant := some eastDoor }
  check (sameRoom.merchantDoor.isNone && sameRoom.guide.isSome)
    "A second door allowed two residents to claim the same room."
  let opened := step door { interact := some eastDoor }
  let outside ← walkToColumn opened 26 100
  let closed := step outside { interact := some eastDoor }
  check (closed.guide.isSome && closed.world.tile eastDoor == .doorClosed)
    "Adding an eastern exit did not preserve the occupied house."
  let pick ← equip closed .basicPick
  state ← walkToColumn pick 68 470
  for column in [65:72] do
    let position : TilePos := ⟨column, 53⟩
    if state.world.tile position == .stone then
      state := step state { mine := some position }
  state ← walkToColumn state 93 280
  for _ in [:batches] do
    state := step state { craftArrows := true }
  check (Inventory.count state.inventory .arrow == batches * arrowBatch)
    s!"Could not craft {batches} batches of arrows. {state.status} Wood: {Inventory.count state.inventory .wood}. Stone: {Inventory.count state.inventory .stone}."
  return state

private def sentryCover (state : GameState) : IO GameState := do
  let approach ← walkToColumn state 316 260
  let mut waiting := approach
  for _ in [:250] do
    if waiting.sentry.creature.any (fun sentry => 12 ≤ sentry.aim && sentry.aim ≤ sentryWindupTicks) then
      break
    waiting := step waiting {}
  check (waiting.sentry.creature.any (fun sentry => 12 ≤ sentry.aim && sentry.aim ≤ sentryWindupTicks))
    "The outpost sentry did not wind up a shot."
  let cover ← buildWallTile waiting ⟨317, 61⟩
  let cover ← buildWallTile cover ⟨317, 60⟩
  let mut held := cover
  let mut sawShot := false
  for _ in [:70] do
    held := step held {}
    sawShot := sawShot || held.projectiles.any (fun shot => shot.kind == .sentry)
  check (sawShot && held.combat.health ≥ cover.combat.health)
    "The newly built wall did not stop an actual sentry shot."
  let pick ← equip held .basicPick
  let lane := step (step pick { mine := some ⟨317, 61⟩ }) { mine := some ⟨317, 60⟩ }
  let bow ← equip lane .bow
  let target : TilePos := ⟨319, 61⟩
  let ammo := Inventory.count bow.inventory .arrow
  let fired := step bow { mine := some target }
  check (Inventory.count fired.inventory .arrow == ammo - 1 &&
    fired.projectiles.any (fun shot => shot.kind == .arrow)) "A shot did not spend exactly one arrow."
  let tooSoon := step fired { mine := some target }
  check (Inventory.count tooSoon.inventory .arrow == ammo - 1) "An early click spent an extra arrow."
  let mut state := tooSoon
  for _ in [:110] do
    if state.sentry.cleared then break
    state := step state { mine := some target }
  check state.sentry.cleared s!"The wooden bow did not defeat the sentry. {repr state.sentry}"
  return state

private def summonStorm (state : GameState) : IO GameState := do
  let altar ← walkToColumn state stormAltar.x 310
  check (altar.storm == .ready) "The second Boss began without using its altar."
  let summoned := step altar { interact := some stormAltar }
  check (match summoned.storm with | .fighting _ => true | _ => false)
    s!"Could not summon the Storm Herald. {summoned.status}"
  return summoned

private def migratedAltarCheck : IO Unit := do
  let initial := newLegacyGame defaultSeed
  let world := generateV6Terrain defaultSeed
  let player : Player := {
    x := stormAltar.x * tileUnits + (tileUnits - playerWidth) / 2
    y := stormAltar.y * tileUnits
    vx := 0, vy := 0, grounded := false }
  check (!player.collides world && player.overlapsTile stormAltar)
    "The legacy altar fixture no longer occupies the proposed altar site."
  let legacy ← legacyJson { initial with world, player, water := Water.initialLegacy world } 6 24
  let loaded ← IO.ofExcept <| (Save.decode legacy.compress).mapError IO.userError
  check (loaded.player == player && !loaded.player.collides loaded.world)
    "The migrated altar displaced or blocked a legacy player."
  let row := stormArenaFloor - 1
  let backup := (List.range (stormArenaEnd - stormArenaStart)).findSome? fun (offset : Nat) =>
    let position : TilePos := ⟨(stormArenaStart + offset : Nat), row⟩
    if loaded.world.tile position == .stormAltar then some position else none
  let some altar := backup | throw (IO.userError "The blocked altar had no replacement.")
  let some player := findStandingNear loaded.world (altar.x + 1) stormArenaFloor 0 0 (fun _ => true)
    | throw (IO.userError "The relocated altar was not reachable.")
  let summoned := step { loaded with player } { interact := some altar }
  check (match summoned.storm with | .fighting _ => true | _ => false)
    "The replacement altar did not summon the Storm Herald."

private def stormWin (initial : GameState) : IO GameState := do
  let mut state := initial
  let mut sawSecondPhase := false
  for _ in [:720] do
    let .fighting boss := state.storm | break
    sawSecondPhase := sawSecondPhase || (boss.enraged && boss.phase == .barrage &&
      state.projectiles.any (fun shot => shot.kind == .herald))
    let ready := boss.phase == .recover && boss.ticks ≥ 16 && state.bowCooldown ≤ 1
    let target : TilePos := ⟨boss.body.centerTile.x, boss.body.centerTile.y - 1⟩
    state := step state { mine := if ready then some target else none }
  check sawSecondPhase "The Storm Herald never entered its second phase."
  let .reward _ := state.storm
    | throw (IO.userError s!"The wooden bow did not defeat the Storm Herald. {repr state.storm}")
  return state

private def stormRoute (quarryHero : GameState) : IO GameState := do
  let sentry ← sentryCover quarryHero
  let summoned ← summonStorm sentry
  saveRoundTrip summoned
  let retreat ← walkToColumn summoned 326 125
  check (retreat.storm == .ready && !retreat.projectiles.any (fun shot => shot.kind == .herald))
    "Leaving the hall did not reset the encounter and clear its shots."
  let exposed ← walkToColumn summoned 339 95
  let mut barrage := exposed
  for _ in [:150] do
    if barrage.projectiles.any (fun shot => shot.kind == .herald) then break
    barrage := step barrage {}
  check (barrage.projectiles.any (fun shot => shot.kind == .herald)) "The Herald did not fire a volley."
  saveRoundTrip barrage
  let mut downed := barrage
  for _ in [:1500] do
    if downed.storm == .ready then break
    downed := step downed {}
  let respawned ← waitForRespawn downed
  check (respawned.storm == .ready && respawned.player.centerTile.x < 32 &&
    respawned.inventory == exposed.inventory)
    "Standing in the volley did not lead to a world respawn with the same gear."

  let retry ← summonStorm retreat
  let position ← walkToColumn retry 339 95
  let mut waiting := position
  for _ in [:180] do
    if let .fighting boss := waiting.storm then
      if boss.phase == .recover && boss.ticks ≥ 16 then break
    waiting := step waiting {}
  let fired := step waiting { mine := some ⟨352, 60⟩ }
  check (fired.projectiles.any (fun shot => shot.kind == .arrow)) "No player arrow was in flight to save."
  saveRoundTrip fired
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode fired)).mapError IO.userError
  let won ← stormWin fired
  check (sameSnapshot won (← stormWin loaded)) "Restoring an arrow in flight changed the Boss replay."
  saveRoundTrip won
  let .reward drop := won.storm | throw (IO.userError "The Herald left no bow.")
  let approached ← walkToColumn won drop.x 170
  let collected := step approached {}
  check (collected.storm == .cleared && Inventory.count collected.inventory .stormBow == 1 &&
    !collected.projectiles.any (fun shot => shot.kind == .herald)) "The Storm Bow was not collected exactly once."
  let stormBow ← equip collected .stormBow
  let fired := step stormBow { mine := some ⟨drop.x - 5, drop.y - 1⟩ }
  check (fired.projectiles.any (fun shot => shot.kind == .stormArrow && shot.kind.damage == 4))
    "The Storm Bow did not fire its upgraded arrow."
  return fired

private def walkToLeftLip (initial : GameState) (column : Nat) (limit : Nat) : IO GameState := do
  let mut state := initial
  for index in [:limit] do
    if state.player.x ≤ (column : Int) * tileUnits + 5 && state.player.grounded then return state
    let jump := index > 0 && state.player.grounded && state.player.vx == 0
    state := step state { horizontal := -1, jump := jump }
    check (!state.player.collides state.world) "The westbound ledge route crossed solid terrain."
  throw (IO.userError s!"Could not reach the ledge at {column} from {repr state.player.centerTile}.")

private def quarrySurface (quarry : GameState) : IO GameState := do
  let gap ← walkToLeftLip quarry 274 220
  let crossed ← walk gap 39 (-1) true
  check (crossed.player.centerTile.x ≤ 271 && crossed.player.centerTile.y ≤ 65)
    "Could not jump back to the upper quarry path."
  let ramp ← walkToLeftLip crossed 249 255
  let jumped ← walk ramp 39 (-1) true
  let surface ← walkToColumn jumped 202 650
  check (surface.player.centerTile.y < 50) "The quarry staircase did not lead back to the surface."
  return surface

private def biomeRoute (quarryHero : GameState) (forgeMail := false) : IO GameState := do
  let deepOre : TilePos := ⟨330, 86⟩
  check ((quarryHero.chart.tile? quarryHero.world deepOre).isNone)
    "The chart exposed the unopened geode grotto."
  let pick ← equip quarryHero .basicPick
  let mut state ← walkToColumn pick 288 115
  for column in [289:291] do
    for row in [66:68] do
      let position : TilePos := ⟨column, row⟩
      state := step state { mine := some position }
      check (state.world.tile position == .air) "Could not dig through the old quarry floor."
  let mut shaft := state
  let shaftCenter := 290 * tileUnits + (tileUnits - playerWidth) / 2
  for _ in [:35] do
    if shaft.player.x ≥ shaftCenter then break
    shaft := step shaft { horizontal := 1 }
  let lowered ← walk shaft 40 0
  check (lowered.player.centerTile.y == 69)
    s!"The player did not descend into the grotto from {repr lowered.player.centerTile}."
  let geodes ← walkToColumn lowered 330 650
  check (inGeodeGrotto geodes.player.centerTile &&
    geodes.chart.tile? geodes.world deepOre == some .geode)
    "The chart did not record the geodes visible from the grotto."
  let blocked := step geodes { mine := some deepOre }
  check (blocked.world.tile deepOre == .geode) "A basic pick mined a geode."
  state ← equip blocked .crystalPick
  for column in [329:333] do
    state := step state { mine := some ⟨column, 86⟩ }
  check (Inventory.count state.inventory .geode == 4 &&
    state.chart.tile? state.world deepOre == some .air) "Four mined geodes did not update the chart."
  if forgeMail then
    state ← mineLifeCrystalAt state (lifeCrystalSites state.seed)[4]!.position
  let returned ← walkToColumn state 288 780
  check (returned.player.centerTile.y ≤ 66)
    s!"The grotto could not be exited with its resources. {repr returned.player.centerTile}"
  let returned ← if forgeMail then do
      let bench ← walkToColumn returned 281 105
      let forged := step bench { craftMail := true }
      check (Inventory.count forged.inventory .geode == 4 - geodeMailGeodes &&
        Inventory.count forged.inventory .silver == 0) "The mail recipe did not spend the mined materials."
      let slot ← slotFor forged .geodeMail
      let armored := step forged { equipGear := some slot }
      check (armored.equipment.armor == some .geodeMail && armored.equipment.defense == 1)
        "The forged mail did not enter the armor slot."
      pure armored
    else pure returned
  let surface ← quarrySurface returned
  let pick ← equip surface .basicPick
  state ← walkToColumn pick 185 280
  let amber : TilePos := ⟨185, amberBluffSurface state.seed 185⟩
  check (inAmberBluffs state.player.centerTile && state.chart.tile? state.world amber == some .amber)
    "The surface chart did not record the amber bluff."
  for column in [183:187] do
    state := step state { mine := some ⟨column, amberBluffSurface state.seed column⟩ }
  let geodeCount := if forgeMail then 4 - geodeMailGeodes else 4
  check (Inventory.count state.inventory .amber == 4 && Inventory.count state.inventory .geode == geodeCount)
    "The two explored routes did not deliver their resources."
  let returned ← walkToColumn state 202 230
  check (returned.chart.tile? returned.world deepOre == some .air)
    "Leaving the grotto lost its exploration record."
  return returned

private def merchantHome (initial : GameState) : IO GameState := do
  let firstDoor : TilePos := ⟨18, 39⟩
  let refused := step initial { claimMerchant := some firstDoor }
  check refused.merchant.isNone "The Guide's room was assigned to the Merchant."
  let opened := step initial { interact := some firstDoor }
  let pick ← equip opened .basicPick
  let mut state := pick
  for (stand, columns) in ([(9, [8, 9]), (7, [6, 7]), (13, [11, 13])] : List (Int × List Nat)) do
    state ← walkToColumn state stand 260
    for column in columns do
      let floor := groundHeight state.seed column
      for row in [floor - 4:floor] do
        let pos : TilePos := ⟨column, row⟩
        if state.world.tile pos == .timber then state := step state { mine := some pos }
  for column in [11:16] do
    for row in [41:43] do
      state := step state { mine := some ⟨column, row⟩ }
  for column in [12:15] do
    state := step state { mine := some ⟨column, 43⟩ }
  state := step state { craftDoor := true }
  let pick ← equip state .basicPick
  state := step pick { mine := some ⟨16, 39⟩ }
  check (Inventory.count state.inventory .workbench == 1)
    "Could not move the original workbench into the second room."
  state ← walkToColumn state 12 70
  for row in [36:40] do
    state ← buildWallTile state ⟨10, row⟩
  for column in [10:17] do
    state ← walkToColumn state (max 11 (min 15 column)) 75
    state ← buildWallTile state ⟨column, 35⟩
  state ← walkToColumn state 14 70
  for row in [36:38] do
    state ← buildWallTile state ⟨16, row⟩
  state ← buildWith state .workbench ⟨12, 39⟩ .workbench
  state ← buildWith state .torch ⟨11, 37⟩ .torch
  let door : TilePos := ⟨16, 39⟩
  state ← buildWith state .door door .doorClosed
  let claimed := step state { claimMerchant := some door }
  check (claimed.guide.isSome && claimed.merchant.isSome && claimed.merchantDoor == some door)
    s!"The second room did not attract the Merchant. {claimed.status}"
  return claimed

private def tradeRoute (village : GameState) : IO GameState := do
  let some merchant := village.merchant | throw (IO.userError "The village has no merchant.")
  let opened := step village { interact := some merchant }
  check opened.openShop "Could not open the merchant's shop."
  let wood ← slotFor opened .wood
  let woodCount := Inventory.count opened.inventory .wood
  let sold := step opened { sellSlot := some wood }
  check (Inventory.count sold.inventory .wood == 0 && sold.coins == opened.coins + woodCount)
    "Selling a harvested stack did not credit exactly its price."
  let seed := step sold { buyOffer := some 5 }
  check (Inventory.count seed.inventory .sunleafSeed == Inventory.count sold.inventory .sunleafSeed + 1 &&
    seed.coins + 3 == sold.coins && seed.merchantStock[5]? == some 1)
    "The sixth shop offer did not sell one Sunleaf seed for three coins."
  let twice := step seed { buyOffer := some 5 }
  let exhausted := step twice { buyOffer := some 5 }
  check (twice.merchantStock[5]? == some 0 && exhausted.inventory == twice.inventory &&
    exhausted.coins == twice.coins) "Sold-out seeds changed the player's inventory or coins."
  let endOfDay := { exhausted with surface := { exhausted.surface with clock := cycleTicks - 1 } }
  let restocked := step endOfDay {}
  check (restocked.merchantStock[5]? == some 2) "The seed offer did not restock at dawn."
  let bought := step sold { buyOffer := some 0 }
  check (Inventory.count bought.inventory .arrow == Inventory.count sold.inventory .arrow + 12 &&
    bought.coins == sold.coins - 6 && bought.merchantStock[0]? == some 5)
    "Buying arrows did not update money, stock and goods together."
  let noCoins := (Inventory.setCoinBalance bought.inventory 0).getD bought.inventory
  let noMoney := step { bought with inventory := noCoins } { buyOffer := some 1 }
  check (noMoney.coins == 0 && noMoney.inventory == noCoins && noMoney.merchantStock == bought.merchantStock)
    "An unaffordable purchase changed the inventory or stock."
  let potion := step bought { buyOffer := some 2 }
  check (Inventory.count potion.inventory .draught == 1 && potion.coins == bought.coins - 10)
    "The merchant did not sell a healing draught."
  saveRoundTrip potion
  let restored ← IO.ofExcept <| (Save.decode (Save.encode potion)).mapError IO.userError
  check (!restored.openShop && restored.merchant == potion.merchant &&
    restored.chart == potion.chart && restored.merchantStock == potion.merchantStock)
    "Reloading did not restore the village and its daily stock."
  let archive ← IO.ofExcept <| (Lean.Json.parse (Save.encode potion)).mapError IO.userError
  let emptyChart := archive.setObjVal! "chart" (.arr #[])
  let recovered ← IO.ofExcept <| (Save.decode emptyChart.compress).mapError IO.userError
  check ((recovered.chart.tile? recovered.world recovered.player.centerTile).isSome &&
    (recovered.chart.tile? recovered.world ⟨330, 86⟩).isNone)
    "An empty chart did not recover the current view while keeping distant areas hidden."
  let equipped ← equip restored .draught
  let injured := { equipped with combat := { equipped.combat with health := 2 * healthPerHeart } }
  let healed := step injured { mine := some injured.player.centerTile }
  check (healed.combat.health == 4 * healthPerHeart && Inventory.count healed.inventory .draught == 0)
    "Using the purchased draught did not restore two health and consume one dose."
  let dawn ← walk healed (cycleTicks - healed.surface.clock) 0
  check (dawn.merchantStock == initialMerchantStock && dawn.coins == healed.coins &&
    dawn.inventory == healed.inventory) "Daily restock changed the player's money or goods."
  return dawn

private def openSettlementExit (village : GameState) : IO GameState := do
  let crafted := step village { craftDoor := true }
  let opened := step crafted { interact := some ⟨16, 39⟩ }
  let inside ← walkToColumn opened 21 125
  let pick ← equip inside .basicPick
  let door : TilePos := ⟨24, 38⟩
  let opening := step (step pick { mine := some ⟨24, 37⟩ }) { mine := some door }
  let built ← buildWith opening .door door .doorClosed
  let opened := step built { interact := some door }
  return ← walkToColumn opened 26 100

private def prepareRaidGear (village : GameState) : IO GameState := do
  let armed := step village { craftBow := true }
  check (Inventory.count armed.inventory .bow == 1)
    s!"Could not prepare a bow for the eastern outpost. {armed.status}"
  let outside ← openSettlementExit armed
  let frost ← minePocketHeart outside 1
  let prepared ← useMinedHeart (← useMinedHeart frost)
  let quarry ← quarryEntrance prepared
  let explored ← minePocketHeart quarry 2
  let ledge ← walkToColumn explored 214 350
  let bench ← dryQuarryRoute ledge
  let pick ← equip bench .basicPick
  let minedStone := step pick { mine := some ⟨283, 66⟩ }
  let supplied := step minedStone { craftArrows := true }
  check (Inventory.count supplied.inventory .arrow == arrowBatch)
    s!"Could not prepare arrows for the eastern outpost. {supplied.status}"
  let armed ← quarrySwordRoute supplied
  let vein ← walkToColumn armed 300 130
  let pick ← equip vein .crystalPick
  let mined := step (step pick { mine := some ⟨301, 66⟩ }) { mine := some ⟨302, 66⟩ }
  check (Inventory.count mined.inventory .silver == geodeMailSilver)
    "Could not mine the silver needed for Geode Mail."
  let explorer ← biomeRoute mined true
  let chestSite ← walkToColumn explorer 21 2400
  let chest : TilePos := ⟨23, 39⟩
  let mut packed := step chestSite { interact := some chest }
  check (packed.openChest == some chest) "Could not open the home chest after the cave expedition."
  for material in [Item.caveGel, .frostShell, .quarryScale] do
    if let some slot := packed.inventory.findIdx? (·.any (·.item == material)) then
      packed := step packed { chestDeposit := some slot }
      check (Inventory.count packed.inventory material == 0) s!"Could not store the expedition's {material.name}."
  let atHome ← walkToColumn (step packed { closeChest := true }) 14 240
  let some merchant := atHome.merchant | throw (IO.userError "The village lost its merchant.")
  let opened := step atHome { interact := some merchant }
  check opened.openShop "Could not return to the merchant with the mined amber."
  let amber ← slotFor opened .amber
  let sold := step opened { sellSlot := some amber }
  check (sold.coins == opened.coins + 16 && Inventory.count sold.inventory .amber == 0)
    "The harvested amber did not fund the next equipment."
  let geode ← slotFor sold .geode
  let sold := step sold { sellSlot := some geode }
  let sold := if let some wood := sold.inventory.findIdx? (·.any (·.item == .wood)) then
      step sold { sellSlot := some wood }
    else sold
  let gear := step (step sold { buyOffer := some 3 }) { buyOffer := some 4 }
  check (Inventory.count gear.inventory .amberTreads == 1 && Inventory.count gear.inventory .watchhorn == 1 &&
    gear.coins == sold.coins - 26 && gear.merchantStock[3]? == some 0 && gear.merchantStock[4]? == some 0)
    s!"Could not buy the treads and Watchhorn. {gear.status}"
  let supplied := step gear { buyOffer := some 2 }
  check (Inventory.count supplied.inventory .draught == 1) "The raid supplies did not include a healing draught."
  let closed := step supplied { closeShop := true }
  let slot ← slotFor closed .amberTreads
  let faster := step closed { equipGear := some slot, horizontal := 1 }
  let ordinary := step closed { horizontal := 1 }
  check (faster.equipment.accessory == some .amberTreads && faster.player.vx == maxEquippedSpeed &&
    faster.player.x - ordinary.player.x == maxEquippedSpeed - maxHorizontalSpeed)
    "The treads did not change actual horizontal movement."
  saveRoundTrip faster
  return faster

private def beginAmberRaid (state : GameState) : IO GameState := do
  let horn ← equip state .watchhorn
  let started := step horn { mine := some horn.player.centerTile }
  let .active wave := started.raid
    | throw (IO.userError s!"The Watchhorn did not call a raid. {started.status}")
  check (wave.number == 1 && wave.foes.size == 2 && Inventory.count started.inventory .watchhorn == 1)
    "The Watchhorn did not start its first wave without being consumed."
  return started

private def headToward (state : GameState) (goal : Int) : TickInput :=
  let x := state.player.centerTile.x
  let direction := if x < goal then 1 else if x > goal then -1 else 0
  { horizontal := direction, jump := direction != 0 && state.player.vx == 0 && state.player.grounded }

private def winAmberRaid (initial : GameState) : IO GameState := do
  let mut state ← equip initial .silverSword
  let mut sawBrute := false
  for tick in [:1800] do
    let .active wave := state.raid | break
    sawBrute := sawBrute || (wave.number == 2 && wave.foes.any (·.kind == .brute))
    if state.combat.health ≤ 2 * healthPerHeart && Inventory.count state.inventory .draught > 0 then
      let draught ← equip state .draught
      let healed := step draught { mine := some draught.player.centerTile }
      check (healed.combat.health > state.combat.health) "Could not drink the raid supplies under cover."
      state ← equip healed .silverSword
      continue
    let mut input : TickInput := {}
    if wave.countdown > 0 then
      input := headToward state 185
    else
      let target := wave.foes.find? fun foe =>
        let facing : Int := if foe.body.x < state.player.x then -1 else 1
        swordReaches state.world state.player foe.body facing true
      if let some foe := target then
        input := { mine := some foe.body.centerTile }
      else if wave.foes.all (·.kind == .slinger) then
        if let some foe := wave.foes[0]? then
          input := headToward state foe.body.centerTile.x
    let next := step state input
    if next.raid == .ready then
      throw (IO.userError s!"Raid replay reset on tick {tick} at {repr state.player.centerTile}, HP {state.combat.health}, clock {state.surface.clock}. {repr wave} {next.status}")
    state := next
    check (!state.player.collides state.world) "The raid combat replay crossed terrain."
  let .reward _ := state.raid
    | throw (IO.userError s!"The raiders were not defeated. {repr state.raid} {state.status}")
  check sawBrute "The second wave's brute did not participate."
  return state

private def raidRoute (gear : GameState) : IO GameState := do
  let horn ← equip gear .watchhorn
  let indoors := step horn { mine := some horn.player.centerTile }
  check (indoors.raid == .ready && indoors.inventory == horn.inventory) "The Watchhorn started a raid indoors."
  let door : TilePos := ⟨16, 39⟩
  let sheltered := if gear.world.tile door == .doorOpen then step gear { interact := some door } else gear
  let dawn ← walk sheltered ((cycleTicks - sheltered.surface.clock) % cycleTicks) 0
  let departure := step dawn { interact := some door }
  let bench ← dryQuarryRoute (← quarryEntrance departure)
  let surface ← quarrySurface bench
  let bluff ← walkToColumn surface 182 260
  let mut prepared := bluff
  for column in [183:187] do
    let position : TilePos := ⟨column, amberBluffSurface prepared.seed column⟩
    if prepared.world.tile position == .air then
      prepared ← walkToColumn prepared ((column : Int) - 1) 70
      prepared ← buildWallTile prepared position
  let field ← walkToColumn prepared 185 80
  let cover ← buildWallTile field ⟨189, 39⟩
  let cover ← buildWallTile cover ⟨189, 38⟩
  let ready ← if cover.combat.health < 3 * healthPerHeart then do
      let draught ← equip cover .draught
      pure (step draught { mine := some draught.player.centerTile })
    else pure cover
  check (ready.combat.health ≥ 3 * healthPerHeart && canStartRaid ready.world ready.seed ready.player)
    "The prepared surface position was not viable for the raid."
  let started ← beginAmberRaid ready
  let west ← walkToColumn started 163 230
  check (west.raid == .ready && !west.projectiles.any (fun shot => shot.kind == .raidBolt))
    "Retreating from the bluffs did not reset the raid or clear its bolts."
  let mut volley := started
  for _ in [:420] do
    if volley.projectiles.any (fun shot => shot.kind == .raidBolt) then break
    volley := step volley {}
  check (volley.projectiles.any (fun shot => shot.kind == .raidBolt)) "The first slinger did not fire."
  saveRoundTrip volley
  let mut waiting := started
  let mut firstHit := none
  for _ in [:420] do
    let next := step waiting {}
    if waiting.combat.health > next.combat.health then
      firstHit := some waiting
      break
    waiting := next
  let some before := firstHit | throw (IO.userError "The waiting player was never struck by a raider.")
  let armored := step before {}
  let unarmored := step { before with equipment := { before.equipment with armor := none } } {}
  check (before.combat.health - armored.combat.health == healthPerHeart &&
    before.combat.health - unarmored.combat.health == 2 * healthPerHeart)
    "Geode Mail did not reduce the same raid hit from two damage to one."
  let mut defeated := armored
  for _ in [:1400] do
    if defeated.raid == .ready then break
    defeated := step defeated {}
  let respawned ← waitForRespawn defeated
  check (respawned.raid == .ready && respawned.player.centerTile.x < 32 &&
    respawned.inventory == started.inventory && respawned.equipment == started.equipment)
    s!"Raid defeat did not respawn at the world spawn with gear. {repr respawned.player.centerTile} {respawned.status}"

  let field ← walkToColumn west 185 260
  let restarted ← beginAmberRaid field
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode restarted)).mapError IO.userError
  let won ← winAmberRaid restarted
  check (sameSnapshot won (← winAmberRaid loaded)) "Reading a raid save changed its battle replay."
  saveRoundTrip won
  let .reward drop := won.raid | throw (IO.userError "The Raid Crest was not dropped.")
  let loot ← walkToColumn won drop.x 180
  let collected := step loot {}
  check (collected.raid == .cleared && Inventory.count collected.inventory .raidCrest == 1 &&
    collected.coins == won.coins + 20 && Inventory.count collected.inventory .watchhorn == 1)
    "The raid reward was missing, duplicated, or consumed the Watchhorn."
  let slot ← slotFor collected .raidCrest
  let equipped := step collected { equipGear := some slot }
  check (equipped.equipment.accessory == some .raidCrest && equipped.equipment.defense == 2 &&
    equipped.equipment.speed == maxEquippedSpeed && Inventory.count equipped.inventory .amberTreads == 1)
    "Equipping the Raid Crest did not upgrade defense and retain the replaced treads."
  return equipped

private def gatherHomeMaterials (cave : GameState) : IO GameState := do
  let pick ← equip cave .copperPick
  let vein ← walkToColumn pick 71 150
  let mut mined := vein
  for column in [72:74] do
    for row in [52:55] do
      mined := step mined { mine := some ⟨column, row⟩ }
  check (Inventory.count mined.inventory .copper == 2 * recallCopper &&
    Inventory.count mined.inventory .crystal ≥ 2 * recallCrystal &&
    Inventory.count mined.inventory .stone ≥ beaconStone + 1)
    s!"Could not mine supplies for two return charms and a beacon. {mined.status}"
  let grove ← walkToColumn mined 15 1100
  let mut stocked := grove
  for position in [⟨14, 39⟩, ⟨14, 38⟩, ⟨12, 39⟩] do
    stocked := step stocked { mine := some position }
    check (stocked.world.tile position == .air) s!"Could not harvest timber at {repr position}."
  check (Inventory.count stocked.inventory .wood ≥ platformWood + beaconWood)
    "The grove did not yield enough wood for the beacon and platforms."
  return stocked

private def buildPlatforms (supplies : GameState) : IO GameState := do
  let site ← walkToColumn supplies 23 110
  check (!site.nearWorkbench) "The platform crafting check needs an open site away from a bench."
  let crafted := step site { craftPlatforms := true }
  check (Inventory.count crafted.inventory .platform == platformBatch &&
    Inventory.count site.inventory .wood - Inventory.count crafted.inventory .wood == platformWood)
    "Handcrafting did not yield four platforms for one wood."
  let lower : TilePos := ⟨23, 38⟩
  let upper : TilePos := ⟨23, 36⟩
  let built ← buildWith (← buildWith crafted .platform lower .platform) .platform upper .platform
  check (Inventory.count built.inventory .platform == platformBatch - 2)
    "Placing two platforms did not use two pieces."
  let lowerLanding ← walk built 38 0 true
  check (lowerLanding.player.grounded && lowerLanding.player.y + playerHeight == lower.y * tileUnits)
    "The first jump did not pass through and land on the lower platform."
  let upperLanding ← walk lowerLanding 38 0 true
  check (upperLanding.player.grounded && upperLanding.player.y + playerHeight == upper.y * tileUnits)
    "The second jump did not land on the upper platform."
  saveRoundTrip upperLanding
  let descended ← walk (step upperLanding { dropThrough := true }) 25 0
  check (descended.player.grounded && descended.player.y + playerHeight == lower.y * tileUnits)
    "One drop command did not land on the platform below."
  let pick ← equip descended .basicPick
  let removed := step pick { mine := some lower }
  check (removed.world.tile lower == .air && Inventory.count removed.inventory .platform == platformBatch - 1)
    "Mining the support did not recover one platform."
  let ground ← walk removed 28 0
  check (ground.player.grounded && ground.player.y == built.player.y)
    "Removing a platform did not drop the player to the solid floor."
  return ground

private def homeRoute (cave : GameState) : IO GameState := do
  let supplies ← gatherHomeMaterials cave
  let built ← buildPlatforms supplies
  let bench ← walkToColumn built 18 100
  let beacon := step bench { craftBeacon := true }
  let charms := step (step beacon { craftRecall := true }) { craftRecall := true }
  check (Inventory.count charms.inventory .homeBeacon == 1 && Inventory.count charms.inventory .recallCharm == 2 &&
    Inventory.count charms.inventory .copper == 0 &&
    Inventory.count bench.inventory .stone - Inventory.count charms.inventory .stone == beaconStone &&
    Inventory.count bench.inventory .wood - Inventory.count charms.inventory .wood == beaconWood &&
    Inventory.count bench.inventory .crystal - Inventory.count charms.inventory .crystal == 2 * recallCrystal)
    "Beacon or charm crafting did not consume exactly the mined ingredients."
  let withoutHome := step charms { mine := some charms.player.centerTile }
  check (withoutHome.inventory == charms.inventory && withoutHome.homeAnchor.isNone)
    "Using a charm without a bound beacon consumed an item."
  let marker : TilePos := ⟨20, 39⟩
  let beacon ← equip withoutHome .homeBeacon
  let floating := step beacon { place := some ⟨20, 37⟩ }
  check (floating.inventory == beacon.inventory && floating.world == beacon.world)
    "A beacon without a solid floor was placed."
  let placed ← buildWith floating .homeBeacon marker .homeBeacon
  check placed.homeAnchor.isNone "Placing a beacon unexpectedly changed the binding."
  let bound := step placed { interact := some marker }
  check (bound.homeAnchor == some marker) "Q did not bind the newly placed beacon."
  let distant ← walkToColumn bound 70 740
  let selected ← equip distant .recallCharm
  saveRoundTrip selected
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode selected)).mapError IO.userError
  let action : TickInput := { mine := some selected.player.centerTile }
  let returned := step selected action
  let some arrival := homeArrival bound.world bound.water marker
    | throw (IO.userError "The bound beacon has no valid landing.")
  check (sameSnapshot returned (step loaded action) && returned.player == arrival &&
    returned.homeAnchor == some marker && Inventory.count returned.inventory .recallCharm == 1)
    "The return did not consume one charm and land at the same beacon after reload."
  let alreadyHome := step returned { mine := some marker }
  check (alreadyHome.inventory == returned.inventory) "Recalling while at home spent a charm."
  let clear ← walkToColumn alreadyHome 23 90
  let blocked ← buildWith clear .stone ⟨20, 38⟩ .stone
  let charm ← equip blocked .recallCharm
  let refused := step charm { mine := some charm.player.centerTile }
  check (refused.homeAnchor == some marker && refused.inventory == charm.inventory && refused.player == charm.player)
    "A blocked beacon consumed the charm, moved the player, or lost its binding."
  saveRoundTrip refused
  let pick ← equip refused .basicPick
  let removed := step pick { mine := some marker }
  check (removed.homeAnchor.isNone && removed.world.tile marker == .air &&
    Inventory.count removed.inventory .homeBeacon == 1) "Mining the beacon did not clear the binding and recover it."
  let charm ← equip removed .recallCharm
  let refused := step charm { mine := some charm.player.centerTile }
  check (refused.inventory == charm.inventory && refused.player == charm.player)
    "Using a charm after dismantling the beacon spent it or moved the player."
  let rebuilt : TilePos := ⟨22, 39⟩
  let replaced ← buildWith refused .homeBeacon rebuilt .homeBeacon
  let bound := step replaced { interact := some rebuilt }
  let charm ← equip bound .recallCharm
  let returned := step charm { mine := some charm.player.centerTile }
  check (returned.player.centerTile == rebuilt && returned.homeAnchor == some rebuilt &&
    Inventory.count returned.inventory .recallCharm == 0)
    "A replacement beacon did not support a later recall."
  return returned

private def worldSlotChecks : IO Unit := IO.FS.withTempDir fun directory => do
  let empty ← WorldSlots.scan directory
  check (empty.isEmpty && WorldSlots.nextSlot empty == 0)
    "A new directory already listed worlds or failed to offer its first save path."
  let fresh := newLegacyGame defaultSeed
  let oldWorld := generatePrePocketsTerrain defaultSeed
  let oldChart := (Chart.empty oldWorld).reveal oldWorld fresh.player.centerTile
  let legacy ← legacyJson { fresh with world := oldWorld, chart := oldChart, water := Water.initialLegacy oldWorld }
    8 24
  let legacy := legacy.setObjVal! "merchantStock" (Lean.toJson #[6, 4, 3])
  let legacyPath := directory / "world.json"
  IO.FS.writeFile legacyPath legacy.compress
  let first ← IO.ofExcept <| (← WorldSlots.load directory 0).mapError IO.userError
  check (sameHistoricalSnapshot fresh first && (← IO.FS.readFile legacyPath) == legacy.compress)
    "Opening the first world did not preserve a v8 file and its contents."
  let catalog ← WorldSlots.scan directory
  let secondSeed := defaultSeed + 1
  check (WorldSlots.nextSlot catalog == 1) "The chooser tried to overwrite a legacy save."
  let second ← IO.ofExcept <| (← WorldSlots.create directory 1 secondSeed "Birch Glade").mapError IO.userError
  check (second.seed != first.seed && second.world != first.world) "Two world slots started with the same terrain."
  let secondPath := WorldSlots.path directory 1
  let moved ← walk second 12 1
  let _ ← IO.ofExcept <| (← Save.write secondPath moved).mapError IO.userError
  let restored ← IO.ofExcept <| (← WorldSlots.load directory 1).mapError IO.userError
  check (sameSnapshot moved restored && (← IO.FS.readFile legacyPath) == legacy.compress)
    "Saving another slot altered the first or failed to preserve the second."
  let corruptPath := WorldSlots.path directory 2
  IO.FS.writeFile corruptPath "incomplete save"
  let catalog ← WorldSlots.scan directory
  check (catalog.size == 3 && catalog.any (fun world => world.slot == 0 && world.seed == some first.seed) &&
    catalog.any (fun world => world.slot == 1 && world.name == "Birch Glade" && world.seed == some second.seed) &&
    catalog.any (fun world => world.slot == 2 && world.seed.isNone) && WorldSlots.nextSlot catalog == 3 &&
    (← WorldSlots.create directory 2 123).isOk == false &&
    (← WorldSlots.create directory 1 456).isOk == false &&
    (← IO.FS.readFile corruptPath) == "incomplete save")
    "A damaged or occupied slot was not protected."
  let _ ← IO.ofExcept <| (← WorldSlots.rename directory 1 "Silver Meadow").mapError IO.userError
  let _ ← IO.ofExcept <| (← WorldSlots.toggleFavorite directory 1).mapError IO.userError
  let entries ← WorldSlots.scan directory
  check (entries[0]?.any (fun world =>
    world.slot == 1 && world.name == "Silver Meadow" && world.favorite && !world.created.isEmpty))
    "Rename or favorite failed to persist or move the world to the top of the list."
  check (!(← WorldSlots.remove directory 1).isOk) "The chooser allowed a favorite world to be removed."
  let updated ← IO.ofExcept <| (← WorldSlots.load directory 1).mapError IO.userError
  check (sameSnapshot moved updated && updated.worldCreated == second.worldCreated &&
    updated.worldName == "Silver Meadow" && updated.worldFavorite)
    "Editing menu metadata changed gameplay or lost the creation date."
  let _ ← IO.ofExcept <| (← WorldSlots.toggleFavorite directory 1).mapError IO.userError
  let _ ← IO.ofExcept <| (← WorldSlots.remove directory 1).mapError IO.userError
  let archives ← (directory / "Deleted Worlds").readDir
  check (archives.size == 1 && !(← secondPath.pathExists) &&
    WorldSlots.nextSlot (← WorldSlots.scan directory) == 1)
    "Removing a world did not free its slot or leave a recovery copy."
  let some archive := archives[0]? | throw (IO.userError "The recovery directory is missing.")
  let archived ← IO.ofExcept <| (← Save.read (archive.path / "world-slot-2.json")).mapError IO.userError
  check (sameSnapshot moved archived && archived.worldName == "Silver Meadow" && !archived.worldFavorite)
    "The deleted world's recovery copy lost data."

private def mapCameraChecks : IO Unit := do
  let world := (newGame defaultSeed).world
  let camera := MapCamera.atTile ⟨30, 45⟩
  let dragged := camera.pan world (-20) 12
  check (dragged.x == 25.5 && dragged.y == 48.5) "Map drag did not move in screen pixels."
  let zoomed := camera.zoom world 2 1 16 (-8)
  check (zoomed.scale == 8 && zoomed.x + 16 / zoomed.scale == camera.x + 16 / camera.scale &&
    zoomed.y - 8 / zoomed.scale == camera.y - 8 / camera.scale)
    "Map zoom moved the tile under the cursor."
  let edge := camera.pan world (-50000) 50000
  check (edge.x == 0.5 && edge.y == world.height.toFloat - 0.5)
    "Map panning escaped the world bounds."
  check ((camera.zoom world 0.01 1 0 0).scale == 1 &&
    (camera.zoom world 100 1 0 0).scale == 31.18) "Map zoom escaped its bounds."

private def pocketMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let pockets := supplyPockets defaultSeed
  let some changed := pockets[0]? | throw (IO.userError "The starter seed has no supply pocket.")
  let baseline := generatePrePocketsTerrain defaultSeed
  let world := baseline.setTile changed.mouth .platform
  let chart := (Chart.empty world).reveal world fresh.player.centerTile
  let archive ← legacyJson { fresh with world, chart, water := Water.initialLegacy world } 10 24
  let restored ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  let historical := historicalTerrain restored
  check (changed.footprint.all (fun position => historical.tile position == world.tile position))
    "Migrating a pocket overlapped a previously built platform."
  check (pockets.extract 1 pockets.size |>.all (fun pocket => restored.world.tile pocket.cache == .supplyCache))
    "A local edit prevented the other unopened branches from appearing."
  check (restored.chart == (Chart.empty fresh.world).reveal fresh.world fresh.player.centerTile)
    "Migration revealed a pocket on the player's map or shifted the old observations."
  saveRoundTrip restored
  let falseCache : TilePos := ⟨20, 36⟩
  let invalid := { fresh with world := fresh.world.setTile falseCache .supplyCache }
  check (!(Save.decode (Save.encode invalid)).isOk) "A cache was accepted outside the generated pockets."
  let chartIndex (position : TilePos) := position.y.toNat * fresh.world.width + position.x.toNat
  let marked := { fresh.chart with cells := fresh.chart.cells.set! (chartIndex falseCache) (Tile.supplyCache.code + 1) }
  check (!(Save.decode (Save.encode { fresh with chart := marked })).isOk)
    "An impossible cache location was accepted in the map."
  let chart := { fresh.chart with cells := fresh.chart.cells.set! (chartIndex changed.cache) (Tile.openedCache.code + 1) }
  saveRoundTrip { fresh with chart, world := fresh.world.setTile changed.cache .air }

private def checkSupplyReward (state : GameState) (position : TilePos) (rewards : List (Item × Nat)) :
    IO GameState := do
  check (state.canOpenSupplyCache position && state.chart.tile? state.world position == some .supplyCache)
    s!"Could not get next to the discovered cache at {repr position} from {repr state.player.centerTile}."
  let nearlyFull : Inventory := (fullBackpack)
    |>.set! (Inventory.mainSlotCount - 1) none
  let input : TickInput := { interact := some position }
  let refused := step { state with inventory := nearlyFull } input
  check (refused.inventory == nearlyFull && refused.world.tile position == .supplyCache)
    "A cache with room for only its first stack was partially opened."
  let attempted := step state { mine := some position }
  check (attempted.inventory == state.inventory && attempted.world.tile position == .supplyCache)
    "Mining a closed cache changed the loot or destroyed it."
  let opened := step state input
  check (opened.world.tile position == .openedCache && opened.chart.tile? opened.world position == some .openedCache)
    "Opening the cache did not update both the world and the map."
  for (item, amount) in rewards do
    check (Inventory.count opened.inventory item == Inventory.count state.inventory item + amount)
      s!"The cache did not award exactly {amount} of {item.name}."
  saveRoundTrip opened
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode opened)).mapError IO.userError
  let repeated := step loaded input
  check (repeated.inventory == loaded.inventory && repeated.world.tile position == .openedCache)
    "Reading the save allowed the same cache to award its contents again."
  let removed := step repeated { mine := some position }
  check (removed.inventory == repeated.inventory && removed.world.tile position == .air &&
    removed.chart.tile? removed.world position == some .air) "Removing a spent cache awarded loot or left a marker."
  return removed

private def fightResident (initial : GameState) (index : Nat) : IO GameState := do
  let mut state := initial
  let mut retried := false
  for _ in [:600] do
    let some (.alive resident) := state.ecology.sites[index]? | return state
    let target := resident.body.centerTile
    let facing := if resident.body.x < state.player.x then -1 else 1
    let ranged := state.selectedStack.any (fun stack => stack.item.bowShot.isSome)
    let aim := if target == state.player.centerTile then { target with x := target.x + facing } else target
    let canFire := ranged && (resident.body.x - state.player.x).natAbs ≤ (8 * tileUnits).toNat &&
      (resident.body.y - state.player.y).natAbs ≤ tileUnits.toNat
    let input := if canFire then
        { mine := some aim : TickInput }
      else if !ranged && swordReaches state.world state.player resident.body facing true then
        { mine := some target : TickInput }
      else if state.player.centerTile.x == target.x then { jump := state.player.grounded }
      else headToward state target.x
    let next := step state input
    if next.respawnTicks > 0 then
      let next ← waitForRespawn next
      check (!retried && next.combat.health == maxHealth && next.ecology.sites[index]?.any (fun site =>
        match site with | .alive _ => true | _ => false))
        "A cave defeat did not leave a resident for a retry."
      retried := true
      let pocket := (supplyPockets state.seed)[index]!
      let direction : Int := if pocket.cache.x < pocket.mouth.x then -1 else 1
      let door : TilePos := ⟨24, 38⟩
      let departure ← if next.world.tile door == .doorClosed then do
          let doorway ← walkToColumn next 21 120
          pure (step doorway { interact := some door })
        else pure next
      let lip ← walkToColumn departure (pocket.mouth.x - direction) 1850
      state := step lip { horizontal := direction }
    else state := next
  throw (IO.userError s!"Resident fight stalled in room {index + 1} at {repr state.player.centerTile}: {repr state.ecology.sites[index]?}, HP {state.combat.health}.")

private def exploreSupplyPocket (state : GameState) (index : Nat) (weapon : Item)
    (rewards : List (Item × Nat)) (keepRewards := false) : IO GameState := do
  let some pocket := (supplyPockets state.seed)[index]?
    | throw (IO.userError s!"Seed {state.seed} is missing pocket {index + 1}.")
  let direction : Int := if pocket.cache.x < pocket.mouth.x then -1 else 1
  let lip := pocket.mouth.x - direction
  let pick ← equip state .basicPick
  let atLip ← walkToColumn pick lip 1550
  let unseen := step atLip { interact := some pocket.cache }
  check (unseen.inventory == atLip.inventory && unseen.chart.tile? unseen.world pocket.cache == none)
    "The unopened pocket granted or revealed its reward through rock."
  let second : TilePos := { pocket.mouth with x := pocket.mouth.x + direction }
  let capped := step (step atLip { mine := some pocket.mouth }) { mine := some second }
  check (capped.world.tile pocket.mouth == .air && capped.world.tile second == .air)
    "The main-road entrance caps could not be mined with a basic pick."
  let lower : TilePos := { second with y := second.y + 1 }
  let cut := if capped.world.tile lower == .stone then step capped { mine := some lower } else capped
  let mut entry ← equip cut weapon
  for tick in [:420] do
    if let some (.alive _) := entry.ecology.sites[index]? then break
    let input := headToward entry pocket.cache.x
    entry := step entry { input with jump := tick > 0 && input.jump }
  let some (.alive resident) := entry.ecology.sites[index]?
    | throw (IO.userError s!"No resident spawned below {repr pocket.mouth} from {repr entry.player.centerTile}.")
  check (entry.ecology.habitats[index]?.any (·.kind == resident.kind)) "A cave spawned the wrong resident."
  saveRoundTrip entry
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode entry)).mapError IO.userError
  let defeated ← fightResident entry index
  check (sameSnapshot defeated (← fightResident loaded index)) "Reloading changed the resident fight replay."
  let some (.loot position) := defeated.ecology.sites[index]?
    | throw (IO.userError "Defeating the resident did not leave a drop.")
  saveRoundTrip defeated
  let blocked : Inventory := fullBackpack
  let full ← walkToColumn { defeated with inventory := blocked } position.x 160
  let refused := step full {}
  check (refused.inventory == blocked && refused.ecology.sites[index]? == some (.loot position))
    "A full inventory lost the resident's pending drop."
  let pending ← IO.ofExcept <| (Save.decode (Save.encode defeated)).mapError IO.userError
  let approached ← walkToColumn pending position.x 160
  let picked := step approached {}
  let item := resident.kind.dropItem
  check (Inventory.count picked.inventory item == Inventory.count state.inventory item + 1)
    "Walking over the resident's saved drop did not grant exactly one material."
  let some (.waiting cooldown) := picked.ecology.sites[index]?
    | throw (IO.userError "A collected resident drop did not start the respawn delay.")
  check (cooldown > 0 && cooldown ≤ caveRespawnTicks) "Resident respawn started outside its valid bounds."
  let reached ← walkToColumn picked pocket.cache.x 420
  let pick ← equip reached .basicPick
  let supplied ← checkSupplyReward pick pocket.cache rewards
  let climbed ← walkToColumn (if keepRewards then supplied else pick) lip 490
  check (climbed.player.centerTile == atLip.player.centerTile)
    s!"Could not return to the main route at {repr atLip.player.centerTile} from {repr climbed.player.centerTile}."
  return climbed

private def craftHunterKit (ledge : GameState) : IO Unit := do
  let hunted ← exploreSupplyPocket ledge 2 .bow [(.draught, 2), (.recallCharm, 1)]
  let crossed ← walk hunted 30 1 true
  let entrance ← walkToColumn crossed 214 380
  let bench ← dryQuarryRoute entrance
  let pick ← equip bench .basicPick
  let stocked := step (step pick { mine := some ⟨280, 62⟩ }) { mine := some ⟨281, 62⟩ }
  let armed := step stocked { craftHunterBow := true }
  check (Inventory.count armed.inventory .hunterBow == 1 &&
    Inventory.count armed.inventory .frostShell == 0 && Inventory.count armed.inventory .quarryScale == 0 &&
    Inventory.count stocked.inventory .wood == Inventory.count armed.inventory .wood + hunterBowWood)
    s!"Could not craft the Hunter Bow with natural drops. Wood {Inventory.count stocked.inventory .wood}, shell {Inventory.count stocked.inventory .frostShell}, scale {Inventory.count stocked.inventory .quarryScale}. {armed.status}"
  let bow ← equip armed .hunterBow
  let target := { bow.player.centerTile with x := bow.player.centerTile.x + 4 }
  let fired := step bow { mine := some target }
  check (Inventory.count fired.inventory .arrow + 1 == Inventory.count bow.inventory .arrow &&
    fired.projectiles.any (fun shot => shot.kind == .hunterArrow && shot.kind.damage == 3))
    "The Hunter Bow did not spend a standard arrow and fire its own three-damage projectile."
  saveRoundTrip fired

private def classicFishingChecks : IO Unit := do
  let world : World := { width := 24, height := 80, tiles := ⟨Array.replicate (24 * 80) Tile.air.code⟩ }
  let pool (depth : Nat) : Water := Id.run do
    let mut levels := (Water.empty world).levels
    for row in [1:depth + 1] do
      levels := levels.set! (row * world.width + 2) 255
    for row in [2:35] do
      levels := levels.set! (row * world.width + 3) 127
    return ⟨levels⟩
  let bobber : TilePos := ⟨2, 1⟩
  let water := pool 75
  let empty := Water.empty world
  check (fishingPondTiles world (pool 74) bobber == 74 && fishingPondTiles world water bobber == 75)
    "Fishing counted water outside the width of the pond's surface."
  let surfaceY := 2 * tileUnits - 15 * 16
  let level : Player := ⟨2 * tileUnits, surfaceY - playerHeight / 2, 0, 0, false⟩
  let lower := { level with y := level.y + 1 }
  let dry := { lower with x := 6 * tileUnits }
  check (fishingCanBite world water empty level bobber 255 &&
    !fishingCanBite world water empty lower bobber 255 &&
    fishingCanBite world water empty dry bobber 255)
    "The bobber height rule changed at the wet/dry boundary."
  let fishRoll (draw : Nat) := if draw == 11 then 99 else 1
  check ((fishingCatch false false false 74 30 fishRoll).isNone &&
    (fishingCatch false false false 75 30 fishRoll).any (fun (fish, _) => fish == .bass) &&
    (fishingCatch true false false 300 30 (fun _ => 0)).isNone)
    "The water and lava requirements no longer match Classic fishing."

  let state := newGame 42
  let state := { state with selected := 3, inventory := state.inventory.set! 3 (some ⟨.fishingRod, 1⟩) }
  let target : TilePos := ⟨state.player.centerTile.x + 8, state.player.centerTile.y - 8⟩
  let fired := step state { mine := some target, aim := some (WorldAim.atTile target) }
  let .bobber launched (.drifting 0) := fired.fishing
    | throw (IO.userError "A dry cast without bait did not create a bobber.")
  check (!launched.wet && launched.vx > 0 && launched.vy < 0 && fired.inventory == state.inventory)
    "The Wood Fishing Pole did not launch toward the air above the player."
  let loaded ← IO.ofExcept ((Save.decode (Save.encode fired)).mapError IO.userError)
  let air := (List.range 8).foldl (fun s _ => step s {}) fired
  let replayed := (List.range 8).foldl (fun s _ => step s {}) loaded
  check (air.fishing == replayed.fishing && air.fishing != fired.fishing) "The saved bobber did not resume its flight."
  let reeled := step air { mine := some target }
  let .bobber returnMotion (.returning none) := reeled.fishing
    | throw (IO.userError "The second click did not start reeling.")
  check (returnMotion.reelTicks == 1) "The bobber did not begin to return on the click."
  saveRoundTrip reeled
  let landed := (List.range 100).foldl (fun s _ => step s {}) reeled
  check (landed.fishing == .idle && landed.inventory == state.inventory)
    "The empty bobber did not return without changing inventory."

private def classicWeatherChecks : IO Unit := do
  let clear : Weather := {}
  let cloudy := { clear with cloudOpacity := 1 }
  let rainy := { cloudy with rainTicks := 30000, rainStrength := 20 }
  check (fishingPower 0 25 clear == 42 && fishingPower 5400 25 clear == 33 &&
    fishingPower 16200 25 clear == 33 && fishingPower 16201 25 clear == 26 &&
    fishingPower 37800 25 clear == 33 && fishingPower 48600 25 clear == 33 &&
    fishingPower 48601 25 clear == 42 && fishingPower 60480 25 clear == 33 &&
    fishingPower 60481 25 clear == 26 && fishingPower 79920 25 clear == 33)
    "Classic fishing time thresholds changed."
  check (fishingPower 0 25 cloudy == 47 && fishingPower 0 25 rainy == 56 &&
    fishingPower 0 25 { clear with day := 4 } == 35 && fishingPower 0 0 rainy == 0)
    "Rain, overcast, moon or missing-bait Fishing Power differs."
  check (fishingWaterNeeded 4200 350 (60 * 16) == 75 &&
    fishingWaterNeeded 4200 350 (100 * 16) == 154 &&
    fishingWaterNeeded 4200 350 (120 * 16) == 257 &&
    fishingWaterNeeded 4200 350 (130 * 16) == 300 &&
    fishingPondPower 30 75 300 == 7 && fishingPondPower 30 100 300 == 10)
    "Classic pond-size or altitude penalty differs."
  let dry := { clear with cloudActivity := 12000 }
  let started := dry.advance false true true (fun index => if index == 24 then 0 else 1)
  check (started.rainTicks == 28801 && started.rainStrength == 41 && started.overcast)
    "Rain did not use the sampled native duration or cloud-dependent intensity."
  let ending := ({ rainy with day := 7, rainTicks := 1, cloudActivity := -100 } : Weather).advance
    true false true (fun _ => 1)
  check (!ending.raining && ending.rainStrength == 0 && ending.overcast && ending.cloudOpacity < 1 &&
    ending.moonPhase == 0) "Rain did not end independently of the fading clouds or the moon did not wrap."
  let state := newGame 42
  let state := { state with
    surface := { state.surface with clock := classicCycleTicks - 1 }
    weather := { rainy with day := 3, cloudActivity := 8000 } }
  let dawn := step state {}
  check (dawn.surface.clock == 0 && dawn.weather.moonPhase == 4 &&
    dawn.weather.rainTicks == rainy.rainTicks - 1)
    "World dawn did not advance the moon and ongoing rain."
  saveRoundTrip dawn

private def fishingCrateChecks : IO Unit := do
  let woodenRoll (draw : Nat) := if draw == 11 then 9 else 1
  let ironRoll (draw : Nat) := if draw == 3 || draw == 11 then 0 else 1
  let goldenRoll (draw : Nat) := if draw == 5 || draw == 11 then 0 else 1
  let catchAt (hardmode : Bool) (roll : Nat → Nat) :=
    (fishingCatch false false false 300 30 roll (hardmode := hardmode)).map (·.1)
  check (catchAt false woodenRoll == some .woodenCrate && catchAt false ironRoll == some .ironCrate &&
    catchAt false goldenRoll == some .goldenCrate && catchAt true woodenRoll == some .pearlwoodCrate &&
    catchAt true ironRoll == some .mythrilCrate && catchAt true goldenRoll == some .titaniumCrate)
    "Standard crate rarity or Hardmode substitution changed."
  let start := newGame 42
  let index := Inventory.hotbarCount
  let state := { start with inventory := start.inventory.set! index (some ⟨.goldenCrate, 2⟩) }
  let opened := step state { inventoryClick := some (index, true) }
  check (Inventory.count opened.inventory .goldenCrate == 1 && opened.status == "Opened Golden Crate." &&
    opened.inventory != Inventory.removeFromSlot state.inventory index 1)
    "Right-click did not consume exactly one crate and grant its loot."
  saveRoundTrip opened
  let full := (Array.replicate Inventory.slotCount (none : Option Stack)).mapIdx fun i slot =>
    if i < Inventory.mainSlotCount then some ⟨Item.wood, 99⟩ else slot
  let full := { state with inventory := full.set! index (some ⟨.goldenCrate, 2⟩) }
  let opened := step full { inventoryClick := some (index, true) }
  check (Inventory.count opened.inventory .goldenCrate == 1 &&
    (opened.coins > 0 || !opened.nightDrops.isEmpty || Inventory.count opened.inventory .masterBait > 0))
    "Opening a crate with a full pack lost its loot."
  saveRoundTrip opened
  let center := start.player.centerTile
  let some crate := (#[-3, 3, -2, 2] : Array Int).findSome? fun dx =>
      (#[-1, 0, 1, 2] : Array Int).findSome? fun dy =>
        let object : Furniture := ⟨.woodenCrate, ⟨center.x + dx, center.y + dy⟩, false⟩
        if object.canPlace start.world && start.canReach object.position then some object else none
    | throw (IO.userError "No placeable crate position near spawn.")
  let ready := { start with inventory := start.inventory.set! 3 (some ⟨.woodenCrate, 1⟩), selected := 3 }
  let placed := step ready { place := some crate.position }
  check (placed.furniture.contains crate && crate.intact placed.world &&
    Inventory.count placed.inventory .woodenCrate == 0) "Placing a crate did not create a 2 by 2 object."
  saveRoundTrip placed
  let recovered := step placed { mine := some crate.position, selectSlot := some 1 }
  check (!recovered.furniture.contains crate && crate.cells.all (fun p => recovered.world.tile p == .air) &&
    Inventory.count recovered.inventory .woodenCrate == 1) "Mining a crate did not recover the unopened item."
  saveRoundTrip recovered
  let rareRoll (draw : Nat) := if draw == 4 || draw == 5 || draw == 11 then 0 else 1
  let mixed : FishingBiomes := { dungeon := true, beach := true, jungle := true, sky := true }
  check ((fishingCatch false false false 300 30 rareRoll (biomes := mixed)).map (·.1) == some .dungeonCrate &&
    (fishingCatch false false false 300 30 rareRoll (hardmode := true)
      (biomes := { mixed with dungeon := false })).map (·.1) == some .seasideCrate &&
    ({ corruption := true, crimson := true, snow := true } : FishingBiomes).crate true 1 == some .hematicCrate)
    "Biome crates did not take priority over golden crates or resolve hybrid biomes."
  let lavaRoll (draw : Nat) := if draw == 11 then 24 else if draw == 12 then 6 else 1
  check ((fishingCatch true true true 300 30 lavaRoll (hardmode := true) (cratePotion := true)).map (·.1) ==
    some .hellstoneCrate && (fishingCatch true true true 300 30 lavaRoll).isNone)
    "The Crate buff or the separate lava crate roll changed."
  let potion := { start with inventory := start.inventory.set! 3 (some ⟨.cratePotion, 1⟩), selected := 3 }
  let buff := step potion { mine := some start.player.centerTile }
  check (buff.cratePotion == cratePotionTicks && Inventory.count buff.inventory .cratePotion == 0 &&
    (step buff { cancelCratePotion := true }).cratePotion == 0) "Crate Potion did not activate or cancel."
  saveRoundTrip buff
  let placed := step { ready with inventory := ready.inventory.set! 3 (some ⟨.obsidianCrate, 1⟩) }
    { place := some crate.position }
  check (placed.furniture.contains { crate with kind := .biomeCrate .obsidianCrate })
    "Placed biome crate lost its item type."
  saveRoundTrip placed
  let box := { start with inventory := start.inventory.set! index (some ⟨.obsidianLockBox, 2⟩) }
  let locked := step box { inventoryClick := some (index, true) }
  let key := { box with inventory := box.inventory.set! (index + 1) (some ⟨.shadowKey, 1⟩) }
  let unlocked := step key { inventoryClick := some (index, true) }
  check (Inventory.count locked.inventory .obsidianLockBox == 2 &&
    Inventory.count unlocked.inventory .obsidianLockBox == 1 && Inventory.count unlocked.inventory .shadowKey == 1 &&
    unlocked.status == "Opened Obsidian Lock Box.") "The Obsidian Lock Box key requirement changed."
  saveRoundTrip unlocked

private def anglerChecks : IO Unit := do
  let roll (draw : Nat) := if draw == 11 then 99 else if draw == 3 then 0 else 1
  let catchQuest (layer : Nat) :=
    (fishingCatch false false false 300 30 roll (questFish := some .dirtfish) (layer := layer)).map (·.1)
  check (catchQuest 1 == some .dirtfish && catchQuest 2 == some .dirtfish &&
    catchQuest 3 != some .dirtfish &&
    (fishingCatch false false false 300 30 (fun draw => if draw == 11 then 0 else roll draw)
      (questFish := some .dirtfish)).map (·.1) == some .ironCrate)
    "Quest fish changed depth or crate priority."
  let start := newGame 42
  let shore : Nat := start.world.width - 19
  let atTile (position : TilePos) : Player :=
    ⟨position.x * tileUnits + 16, (position.y + 1) * tileUnits - playerHeight, 0, 0, true⟩
  let moveTo (state : GameState) (position : TilePos) : GameState :=
    let player := atTile position
    { state with player, chart := state.chart.reveal state.world player.centerTile }
  let mut sleeping := moveTo start ⟨shore, (groundHeight start.seed shore : Int) - 1⟩
  for _ in [:600] do
    if sleeping.angler.position.isSome then break
    sleeping := step sleeping {}
  let some position := sleeping.angler.position | throw (IO.userError "No Sleeping Angler appeared at the coast.")
  check (!sleeping.angler.rescued && sleeping.world.tile ⟨position.x, position.y + 1⟩ == .sand)
    "Sleeping Angler did not spawn on sand."
  let awake := step (moveTo sleeping position) { interact := some position }
  check (awake.openAngler && awake.angler.rescued && !awake.showAnglerQuest)
    "Interacting with the Sleeping Angler did not open the greeting."
  let ready := { awake with
    angler := { awake.angler with questIndex := 5, completed := 4 }
    weather := { awake.weather with day := 4 } }
  let asked := step ready { askAnglerQuest := true }
  check (asked.showAnglerQuest && !asked.angler.turnedIn) "Asking without a fish did not reveal the quest."
  let stocked := { asked with inventory := asked.inventory.set! 3 (some ⟨.dirtfish, 2⟩) }
  let paid := step stocked { askAnglerQuest := true }
  check (paid.angler.completed == 5 && paid.angler.turnedIn &&
    Inventory.count paid.inventory .dirtfish == 1 && Inventory.count paid.inventory .fuzzyCarrot == 1)
    "Turn-in did not consume one fish and award the fifth-quest reward."
  let repeated := step paid { askAnglerQuest := true }
  check (repeated.angler == paid.angler && repeated.inventory == paid.inventory)
    "The same daily quest paid out twice."
  saveRoundTrip paid
  let dawn := step { paid with surface := { paid.surface with clock := classicCycleTicks - 1 } } {}
  check (dawn.weather.day == 5 && dawn.angler.completed == 5 && !dawn.angler.turnedIn)
    "Dawn did not reopen daily Angler quests."
  saveRoundTrip dawn

private def honeyFishingChecks : IO Unit := do
  let world : World := ⟨12, 18, ⟨Array.replicate (12 * 18) Tile.air.code⟩⟩
  let surface : TilePos := ⟨3, 5⟩
  let mut pool := Water.empty world
  for row in [5:10] do
    for col in [1:11] do
      if row == 9 && col == 10 then continue
      pool := pool.setLevel world ⟨col, row⟩ 255
  let pond := fishingPond world default default pool surface
  check (pond.honey && !pond.lava && pond.tiles == 49 && pond.effectiveTiles == 73)
    "Honey fishing crossed the 50-tile native cutoff early."
  let water := (Water.empty world).setLevel world ⟨10, 9⟩ 255
  let pond := fishingPond world water default pool surface
  check (pond.honey && pond.tiles == 50 && pond.effectiveTiles == 75)
    "A connected mixed-liquid column was excluded from the native pond count."
  let catchAt (tiles quality : Nat) :=
    fishingCatch false false false tiles 60 (fun i => if i == 4 then quality else 0)
      (questFish := some .bumblebeeTuna) (honey := true)
  check ((catchAt 74 0).isNone && (catchAt 75 0).map Prod.fst == some .honeyfin)
    "Honeyfin was not selected before the crate and quest branches."
  let outcomes := (List.range 30).map fun seed =>
    honeyFishingCatch false true (some .bumblebeeTuna) (UnifiedRandom.seed (seed : Nat))
  check (outcomes.contains (some .honeyfin) && outcomes.contains (some .bumblebeeTuna) &&
    (honeyFishingCatch false false (some .bumblebeeTuna) (UnifiedRandom.seed 9)).isNone)
    "The uncommon honey catch or quest gate changed."
  let honey := (Water.empty world).setLevel world surface 255
  let water := (Water.empty world).setLevel world ⟨2, 5⟩ 24
  let (cooled, water, _, honey) := Magma.coolHoney world water default honey (fun _ _ => true)
  check (cooled.tile surface == .honeyBlock && water.level world ⟨2, 5⟩ == 0 &&
    honey.level world surface == 0) "Water did not harden honey at the contact cell."
  let honey := (Water.empty world).setLevel world surface 255
  let lava := (Water.empty world).setLevel world ⟨3, 6⟩ 255
  let (cooled, _, lava, honey) := Magma.coolHoney world default lava honey (fun _ _ => true)
  check (cooled.tile ⟨3, 6⟩ == .crispyHoneyBlock && lava.level world ⟨3, 6⟩ == 0 &&
    honey.level world surface == 0) "Honey above lava did not harden the lower cell."
  let state := newGame 42
  let some index := state.honey.levels.data.findIdx? (· != 0)
    | throw (IO.userError "New Classic world has no honey chamber.")
  let x := index % state.world.width
  let y := index / state.world.width
  let position : TilePos := ⟨x, y⟩
  check ((fishingPond state.world state.water state.magma state.honey position).effectiveTiles ≥ 75)
    "The generated honey chamber is too small for fishing."
  let player : Player := ⟨(x : Int) * tileUnits + (tileUnits - playerWidth) / 2,
    ((y : Int) + 2) * tileUnits - playerHeight, 0, 0, false⟩
  let state := { state with
    player, chart := state.chart.reveal state.world player.centerTile
    inventory := state.inventory.set! 0 (some ⟨.honeyfin, 2⟩)
    crateLifeCrystals := 5
    combat := { state.combat with health := 22, lifeCrystals := 5 } }
  let healed := step state { mine := some position }
  check (healed.combat.health == 142 && healed.potionSickness == potionSicknessTicks &&
    Inventory.count healed.inventory .honeyfin == 1 && healed.honeyBuff == honeyBuffDuration)
    "Honeyfin or the immersion buff did not apply through normal item use."
  saveRoundTrip healed
  let sponge := { healed with inventory := healed.inventory.set! 0 (some ⟨.honeyAbsorbantSponge, 1⟩) }
  let absorbed := step sponge { mine := some position }
  check (absorbed.status == "Absorbed honey." && Inventory.count absorbed.inventory .honeyAbsorbantSponge == 1)
    "The Honey Absorbant Sponge was consumed or did not remove honey."
  let poured := step { absorbed with
      toolCooldown := 0
      inventory := absorbed.inventory.set! 0 (some ⟨.bottomlessHoneyBucket, 1⟩) }
    { mine := some position }
  check (poured.status == "Poured honey." && Inventory.count poured.inventory .bottomlessHoneyBucket == 1)
    "The Bottomless Honey Bucket was consumed or did not refill the tile."
  saveRoundTrip poured

private def removeTempleForOldFixture (state : GameState) : GameState :=
  let baseline := (generateWorld state.seed).1
  let layout := TempleLayout.sites.find? fun layout =>
    TempleDoor.atCell state.world layout.door == some (layout.door, .locked)
  match layout with
  | none => state
  | some layout =>
    let arena := (Array.range (state.world.width * state.world.height)).filterMap fun (i : Nat) =>
      let p : TilePos := ⟨i % state.world.width, i / state.world.width⟩
      if TempleLayout.hasNaturalWall state.world state.walls p || state.world.tile p == .lihzahrdBrick then
        some p else none
    let (world, walls) := (layout.footprint ++ arena).foldl (fun (world, walls) p =>
      (world.setTile p (baseline.tile p), walls.set! (p.y.toNat * world.width + p.x.toNat) 0))
      (state.world, state.walls)
    { state with
      world, walls, templeWires := .empty
      chests := state.chests.filter (fun chest => !layout.chestAnchors.contains chest.position) }

private def destroyerChecks : IO Unit := do
  let (dx, dy) := destroyerFallVelocity 0 0 100 16
  check (dx == 0.11 && dy == 0.15) "Destroyer free-fall did not match Classic's first update."
  let (dx, dy) := destroyerBurrowVelocity 1 1 80 60
  check (dx == 1.25 && dy == 1.25) "Destroyer lost the two-axis burrow acceleration."
  let head : DestroyerSegment := { x := 120, y := 50, alpha := 120 }
  let tail : DestroyerSegment := { x := 20, y := 50 }
  let follow := tail.follow head
  check (follow.x == 65 && follow.y == 50 && follow.alpha == 213 &&
    destroyerDefense 0 == 0 && destroyerDefense 1 == 30 && destroyerDefense 81 == 35)
    "Segment spacing, fade or defense changed."
  let fresh := newGame 42
  let ready := { fresh with inventory := fresh.inventory.set! 0 (some ⟨.mechanicalWorm, 2⟩) }
  let use := { mine := some ready.player.centerTile : TickInput }
  let denied := step ready use
  check (denied.destroyer.isNone && Inventory.count denied.inventory .mechanicalWorm == 2)
    "Mechanical Worm was consumed in daylight."
  let start := { ready with surface := { ready.surface with clock := classicDayTicks + 60 } }
  let summoned := step start use
  let some boss := summoned.destroyer | throw (IO.userError "Mechanical Worm did not summon the Destroyer.")
  check (boss.health == 80000 && boss.segments.size == 82 && summoned.bowCooldown > 0 &&
    Inventory.count summoned.inventory .mechanicalWorm == 1)
    "Manual summon did not create the full chain or consume exactly one worm."
  check (Inventory.count (step summoned use).inventory .mechanicalWorm == 1)
    "An active Destroyer consumed a second worm."
  let part : DestroyerSegment := { x := 360, y := 580, laserClock := 1397, released := true }
  let roll (draw : Nat) := if draw == 0 then 3 else if draw == 1 then 0 else 20
  let (body, laser) := part.advanceLaser fresh.player (fun _ => true) roll
  let (blocked, noLaser) := part.advanceLaser fresh.player (fun _ => false) roll
  check (body.laserClock == 0 && blocked.laserClock == 0 && noLaser.isNone &&
    laser.any (fun shot => shot.kind == .destroyerLaser && shot.damage == 22 &&
      shot.kind.lifetime == 300 && shot.kind.updates == 3))
    "The body clock or the Death Laser's native parameters changed."
  let probe : DestroyerProbe := {
    id := 1
    x := (Int32.ofInt (bodyCenterX fresh.player)).toFloat32 / 16 - 15
    y := (Int32.ofInt (bodyCenterY fresh.player)).toFloat32 / 16 - 115
    laserClock := 119 }
  let (probeAfter, pink) := probe.advance fresh.world fresh.player true false
  check (probeAfter.any (·.laserClock == 0) && pink.any (fun shot => shot.kind == .probeLaser &&
    shot.damage == 25 && shot.kind.lifetime == 600))
    "A Probe did not fire its Pink Laser at frame 120."
  let (interrupted, noPink) := { probe with justHit := true }.advance fresh.world fresh.player true false
  check (interrupted.any (·.laserClock == 0) && noPink.isNone)
    "Hitting a Probe did not reset its shot timer."
  let active := { summoned with
    destroyerProbes := #[probe], nextDestroyerProbeId := 2
    twinShots := (laser.toArray ++ pink.toArray).mapIdx (fun i attack => TwinShot.fire attack i)
    npcImmunity := #[(.destroyerProbe 1, 10), (.destroyerSegment 4, 10)] }
  saveRoundTrip active
  let decoded ← IO.ofExcept <| (Save.decode (Save.encode active)).mapError IO.userError
  check (sameSnapshot (step active {}) (step decoded {})) "Reload changed the next Destroyer tick."
  let old ← IO.ofExcept <| (Lean.Json.parse (Save.encode (removeTempleForOldFixture fresh))).mapError IO.userError
  let migrated ← IO.ofExcept <| (Save.decode ((old.setObjVal! "version" (Lean.toJson (67 : Nat))).compress)).mapError IO.userError
  check (sameSnapshot fresh migrated) "Migrating v67 changed a world without a Destroyer."
  let p := fresh.player
  let chestX := bodyCenterX p + 8 * tileUnits
  let chestY := bodyCenterY p - 2 * tileUnits
  let spot := Destroyer.atCenter ((Int32.ofInt chestX).toFloat32 / 16) ((Int32.ofInt chestY).toFloat32 / 16)
  let arrow : Projectile := {
    x := chestX - 226, y := chestY, vx := 226, vy := 0, life := ShotKind.unholyArrow.lifetime
    kind := .unholyArrow, hitsLeft := 5, slot := some 0 }
  let state := { start with destroyer := some spot, projectiles := #[arrow] }
  let struck := step state {}
  check (struck.destroyer.any (fun boss => boss.health < destroyerHealth && boss.segments.size == 82))
    "A piercing shot did not reduce the shared health across the full Destroyer."
  let ended := step { state with
    destroyer := some { spot with health := 1 }
    destroyerProbes := #[probe]
    nextDestroyerProbeId := 2 } {}
  check (ended.destroyer.isNone && ended.destroyerDefeated && ended.destroyerProbes.any (·.id == 1) &&
    ended.nightDrops.any (fun drop => drop.items.any (·.item == .soulOfMight)))
    "A lethal hit failed to award Soul of Might, set progression, or leave the surviving Probe."
  saveRoundTrip ended
  let collected := step { ended with equipment := { ended.equipment with accessory := some .treasureMagnet } } {}
  check (Inventory.count collected.inventory .soulOfMight ≥ 25)
    "The Destroyer's Soul of Might could not be picked up."
  saveRoundTrip collected
  let withdrawn := step { state with
    destroyer := some (Destroyer.atCenter 450 1800)
    surface := { start.surface with clock := 0 } } {}
  check (withdrawn.destroyer.isNone && !withdrawn.destroyerDefeated && withdrawn.nightDrops.isEmpty)
    "A dawn withdrawal granted victory or loot."
  let minimum := destroyerRewards (fun _ => 0)
  let maximum := destroyerRewards (fun draw => if draw == 2 || draw == 3 then 15 else 1)
  check (minimum[0]? == some ⟨.hallowedBar, 15⟩ && minimum[1]? == some ⟨.soulOfMight, 25⟩ &&
    maximum[0]? == some ⟨.hallowedBar, 30⟩ && maximum[1]? == some ⟨.soulOfMight, 40⟩ &&
    minimum.any (·.item == .destroyerTrophy) && !maximum.any (·.item == .destroyerTrophy))
    "The Classic Destroyer loot table changed."

private def skeletronPrimeChecks : IO Unit := do
  let fresh := newGame 42
  let p := fresh.player
  let center := p.centerTile
  let station := step { fresh with inventory := fresh.inventory.set! 0 (some ⟨.mythrilAnvil, 1⟩) }
    { place := some ⟨center.x - 2, center.y⟩ }
  let resources := fresh.inventory
    |>.set! 10 (some ⟨.bone, 30⟩)
    |>.set! 11 (some ⟨.leadBar, 5⟩)
    |>.set! 12 (some ⟨.soulOfLight, 3⟩)
    |>.set! 13 (some ⟨.soulOfNight, 3⟩)
  let built := { station with inventory := resources }
  let crafted := step built { craftClassic := some .mechanicalSkull, craftToCursor := true }
  check (crafted.cursorStack == some ⟨.mechanicalSkull, 1⟩ &&
    !(crafted.inventory.extract 10 14).any Option.isSome)
    "Mechanical Skull crafting did not consume the Classic recipe materials."
  let ready := { fresh with inventory := fresh.inventory.set! 0 (some ⟨.mechanicalSkull, 2⟩) }
  let use := { mine := some p.centerTile : TickInput }
  let denied := step ready use
  check (denied.skeletronPrime.isNone && Inventory.count denied.inventory .mechanicalSkull == 2)
    "Mechanical Skull consumed in daylight."
  let night := { ready with surface := { ready.surface with clock := classicDayTicks + 100 } }
  let active := step night use
  check (active.skeletronPrime.any (fun head => head.health == 28000 && head.initialized) &&
    active.primeArms.map (·.health) == #[7000, 9000, 9000, 6000] &&
    Inventory.count active.inventory .mechanicalSkull == 1)
    "Skeletron Prime did not create its four independently damageable arms."
  check (Inventory.count (step active use).inventory .mechanicalSkull == 1)
    "An existing Skeletron Prime consumed a second skull."

  let x := bodyCenterX p + 8 * tileUnits
  let y := bodyCenterY p - 2 * tileUnits
  let pixel (value : Int) := (Int32.ofInt value).toFloat32 / 16
  let spot := PrimeHead.atCenter (pixel x) (pixel y)
  let head := { spot with initialized := true }
  let spin := { head with timer := 599 }.advance fresh.world p false false
  check (spin.any (fun next => next.phase == PrimePhase.spin && next.contactDamage == 47 && next.defense == 24))
    "Skeletron Prime's hover-to-spin transition frame changed."
  let spinning := spin.bind (fun next => next.advance fresh.world p false false)
  check (spinning.any (fun next => next.timer == 1 && next.contactDamage == 94 && next.defense == 48))
    "Skeletron Prime did not double its damage and defense while spinning."
  let dawn := head.advance fresh.world p true false
  let nextNight := dawn.bind (fun next => next.advance fresh.world p false false)
  check (nextNight.any (fun next => next.phase == PrimePhase.guardian &&
    next.contactDamage == 9999 && next.defense == 9999))
    "Skeletron Prime did not retain its lethal dawn state."

  let saw := { PrimeArm.atHead .saw head with y := pixel y - 26 }
  let far := { head with x := head.x + 80 }
  let arrow : Projectile := {
    x := x - 226, y, vx := 226, vy := 0, life := ShotKind.unholyArrow.lifetime
    kind := .unholyArrow, hitsLeft := 5, slot := some 0 }
  let struck := step { night with
    skeletronPrime := some far
    primeArms := #[{ saw with health := 1 }]
    projectiles := #[arrow] } {}
  check (struck.skeletronPrime.any (·.health == 28000) && struck.primeArms.isEmpty &&
    !struck.primeDefeated && struck.nightDrops.isEmpty)
    "Destroying one arm affected the head's health or mechanical progression."
  let defeated := step { night with
    skeletronPrime := some { head with health := 1 }
    primeArms := #[PrimeArm.atHead .cannon far]
    projectiles := #[arrow] } {}
  check (defeated.skeletronPrime.isNone && defeated.primeArms.isEmpty && defeated.primeDefeated)
    "A lethal hit to the head did not complete the boss encounter."
  let collected := step { defeated with equipment := { defeated.equipment with accessory := some .treasureMagnet } } {}
  check (Inventory.count collected.inventory .soulOfFright ≥ 25 &&
    Inventory.count collected.inventory .hallowedBar ≥ 15)
    "Skeletron Prime's Classic victory loot was missing."
  saveRoundTrip { collected with inventory := collected.inventory.set! 14 (some ⟨.skeletronPrimeTrophy, 1⟩) }

  let bomb := PrimeShot.fire ⟨.bomb, pixel (bodyCenterX p), pixel (bodyCenterY p), 0, 0, 0⟩ 0
  let blasted := step { night with primeShots := #[bomb] } {}
  check (blasted.combat.health < night.combat.health && blasted.world == night.world && blasted.primeBombExploded)
    "The 128-pixel cannon blast failed to damage the player or edited the terrain."
  let laser := PrimeShot.fire ⟨.laser, pixel x, pixel y - 80, 0, 8, 25⟩ 1
  let inFlight := { active with
    primeShots := #[{ bomb with vy := 1 }, laser]
    npcImmunity := #[(.primeHead, 5), (.primeArm .saw, 5)] }
  saveRoundTrip inFlight
  let decoded ← IO.ofExcept ((Save.decode (Save.encode inFlight)).mapError IO.userError)
  check (sameSnapshot (step inFlight {}) (step decoded {})) "v69 changed Prime's next frame after reload."
  let current ← IO.ofExcept ((Lean.Json.parse (Save.encode (removeTempleForOldFixture fresh))).mapError IO.userError)
  let legacy := (current.setObjVal! "version" (Lean.toJson (68 : Nat))).compress
  let migrated ← IO.ofExcept ((Save.decode legacy).mapError IO.userError)
  check (sameSnapshot fresh migrated) "v68 migration changed the world without Skeletron Prime."

  let count := ((List.range 512).find? (fun n =>
    (hash (fresh.seed, n + 1, (97 : Nat), (0 : Nat))).toNat % 10 == 0)).getD 0
  let altar := (eyeAltars fresh.seed ++ evilAltars fresh.seed)[0]!
  let world := (altarCells altar).foldl (fun world site => world.setTile site .air) fresh.world
  let afterAltar := { fresh with
    world
    hardmode := true
    altarsSmashed := 1
    twinsDefeated := true
    destroyerDefeated := true
    mechanicalNights := count
    surface := { fresh.surface with clock := classicDayTicks - 1 } }
  let warning := step afterAltar {}
  check (warning.primePending && warning.skeletronPrime.isNone &&
    warning.status == "The air is getting colder around you...")
    "The last undefeated mechanical boss did not warn at dusk."
  saveRoundTrip warning
  let appears := step { warning with surface := { warning.surface with clock := classicDayTicks + 4860 } } {}
  check (appears.skeletronPrime.any (·.health == 28000) && !appears.primePending &&
    !(step { afterAltar with primeDefeated := true } {}).primePending)
    "Skeletron Prime did not appear after the warning or reappeared after being defeated."

private def planteraChecks : IO Unit := do
  let fresh := newGame 42
  let earlier := { fresh with hardmode := true, twinsDefeated := true, destroyerDefeated := true }
  let locked := step earlier {}
  check (locked.planteraBulbs.isEmpty && !locked.planteraUnlocked)
    "Plantera's Bulb appeared with an undefeated mechanical boss."
  let won := step { earlier with primeDefeated := true } {}
  check (won.planteraUnlocked && won.planteraBulbs.size == 1 &&
    won.status == "The jungle grows restless..." && won.validPlantera)
    "Victory over all three mechanical bosses did not generate the first bulb."
  let bulb := won.planteraBulbs[0]!
  saveRoundTrip won
  let player := { won.player with
    x := (bulb.x - 2) * tileUnits
    y := (bulb.y + 2) * tileUnits - playerHeight
    vx := 0, vy := 0, grounded := true }
  let ready := { won with player, selected := 1 }
  check (!player.collides ready.world) "The Plantera test player is blocked by the chamber."
  let active := step ready { mine := some bulb }
  check (active.plantera.any (fun head => head.health == planteraHeadHealth && head.stage == 1) &&
    active.planteraHooks.size == 3 && active.planteraBulbs.isEmpty &&
    (planteraBulbCells bulb).all (fun p => active.world.tile p == .air))
    "Mining a bulb did not remove all four tiles and summon Plantera with three hooks."
  let surface := groundHeight active.seed 19
  let sample (draw : Nat) :=
    if draw == 0 then (bulb.x - 9).toNat else if draw == 1 then (bulb.y + 2 - (surface : Int)).toNat else 0
  let regrowth := growPlanteraBulb active.world active.water active.magma active.honey surface
  check ((regrowth #[] sample).any (·.2 == bulb) && (regrowth #[bulb] sample).isNone)
    "Natural bulb regrowth lost its Jungle surface or spacing rule."
  saveRoundTrip active
  let second := step { active with plantera := active.plantera.map (fun head => { head with health := 15000 }) } {}
  check (second.plantera.any (fun head => head.secondForm && head.stage == 2) &&
    second.planteraTentacles.size == 8)
    "Plantera's half-life transition did not produce eight Classic tentacles."
  let pixel (n : Int) : Float32 := (Int32.ofInt n).toFloat32 / 16
  let x := bodyCenterX player + 3 * tileUnits
  let y := bodyCenterY player - 3 * tileUnits
  let thorn := PlanteraShot.fire ⟨.thornBall, pixel x, pixel y, 2, 0, 31⟩ 0
  let tentacle := second.planteraTentacles[0]!
  let saved := { second with
    npcImmunity := #[(.planteraHead, 5), (.planteraTentacle tentacle.id, 4)]
    planteraShots := #[thorn]
    poisonedTicks := 170
    poisonProgress := 13 }
  saveRoundTrip saved
  let decoded ← IO.ofExcept ((Save.decode (Save.encode saved)).mapError IO.userError)
  check (sameSnapshot (step decoded {}) (step saved {}))
    "Plantera's v70 combat save changed the next frame."
  let poisoned := step {ready with poisonedTicks := 2, poisonProgress := 29} {}
  check (poisoned.combat.health + 1 == ready.combat.health && poisoned.poisonedTicks == 1)
    "Poisoned did not drain the player's life."
  let head := { PlanteraHead.atCenter (pixel x) (pixel y) with health := 1, stage := 2 }
  let arrow : Projectile := {
    x := x - 226, y, vx := 226, vy := 0, life := ShotKind.unholyArrow.lifetime
    kind := .unholyArrow, hitsLeft := 5, slot := some 0 }
  let dying := { active with
    player
    plantera := some head
    planteraHooks := #[]
    projectiles := #[arrow]
    equipment := { active.equipment with accessory := some .treasureMagnet } }
  let defeated := step dying {}
  check (defeated.plantera.isNone && defeated.planteraDefeated &&
    Inventory.count defeated.inventory .templeKey == 1 &&
    Inventory.count defeated.inventory .grenadeLauncher == 1 &&
    Inventory.count defeated.inventory .rocketI ≥ 50)
    "The first Plantera kill did not grant its key, launcher and rockets."
  saveRoundTrip defeated
  let replayDrops := (Array.range 8).filterMap fun weapon =>
    (planteraRewards false (fun draw => if draw == 5 then weapon else 1))[1]?.map (·.item)
  check (replayDrops.toList.eraseDups.length == 8 && replayDrops[0]? == some .grenadeLauncher &&
    replayDrops[7]? == some .vulgarDisplayOfFlower)
    "Plantera's repeat-kill weapon pool changed."

private def templeChecks : IO Unit := do
  let fresh := newGame 42
  let some layout := TempleLayout.sites.find? (fun layout =>
      TempleDoor.atCell fresh.world layout.door == some (layout.door, .locked))
    | throw (IO.userError "The Classic world has no Jungle Temple.")
  let hinge := layout.door
  let click := { hinge with y := hinge.y + 1 }
  let outside : Player := ⟨(hinge.x + 2) * tileUnits, (hinge.y + 3) * tileUnits - playerHeight, 0, 0, true⟩
  let initial := { fresh with player := outside, combat := { fresh.combat with facing := -1 } }
  let tap (state : GameState) (p : TilePos) := step state { interact := some p }
  let locked := tap initial click
  check (TempleDoor.atCell locked.world click == some (hinge, .locked) &&
    Inventory.count locked.inventory .templeKey == 0) "The Lihzahrd Door accepted a player without a key."
  let keyed := { locked with inventory := locked.inventory.set! 8 (some ⟨.templeKey, 2⟩) }
  let unlocked := tap keyed click
  let lockedFrame := TempleDoor.frame locked.world click
  check (TempleDoor.atCell unlocked.world click == some (hinge, .closed) &&
    Inventory.count unlocked.inventory .templeKey == 1 &&
    TempleDoor.frame unlocked.world click == lockedFrame.map (fun (id, x, y) => (id, x, y + 54)))
    "First right-click must consume one key, preserve the variant and leave the door closed."
  saveRoundTrip unlocked
  let openState := tap unlocked click
  let leaf : TilePos := ⟨click.x - 1, click.y⟩
  check (TempleDoor.atCell openState.world leaf == some (hinge, .openLeft) &&
    TempleDoor.frame openState.world leaf == some (11, 36, 666))
    "Open Lihzahrd Door lost its native two-column facing."
  let onHinge := { outside with x := hinge.x * tileUnits + 16 }
  check (TempleDoor.atCell (tap { openState with player := onHinge } leaf).world leaf == some (hinge, .openLeft))
    "The Lihzahrd Door closed on the player."
  let closed := tap openState leaf
  check (TempleDoor.atCell closed.world click == some (hinge, .closed) &&
    closed.world.tile leaf == .air && Inventory.count closed.inventory .templeKey == 1)
    "Closing an open leaf did not restore its unsealed hinge."
  let damaged := step { closed with selected := 1 } { mine := some click }
  check (TempleDoor.atCell damaged.world click == some (hinge, .closed) &&
    damaged.tileHits.any (fun hit => hit.position == click && hit.damage == 35))
    "The first Copper Pick strike should only crack an unlocked door."
  saveRoundTrip damaged
  let walked ← walk unlocked 56 (-1)
  check (walked.player.centerTile.x < hinge.x && walked.autoTempleDoor.isNone &&
    TempleDoor.atCell walked.world click == some (hinge, .closed))
    "An unlocked door did not automatically open and close while walking through."
  let some anchor := layout.chestAnchors[0]? | throw (IO.userError "Missing Lihzahrd Chest site.")
  let nearChest : Player :=
    ⟨(anchor.x + 2) * tileUnits, (anchor.y + 2) * tileUnits - playerHeight, 0, 0, true⟩
  let opened := tap { walked with player := nearChest } ⟨anchor.x + 1, anchor.y + 1⟩
  check (opened.openChest == some anchor && (opened.chestAt anchor).any (fun chest =>
    chest.slots[0]?.join == some ⟨.lihzahrdPowerCell, 1⟩ &&
      chest.slots.any (· == some ⟨.lihzahrdFurnace, 1⟩)))
    "Using a Lihzahrd Chest segment did not show its guaranteed loot."
  let looted := step opened { chestClick := some (0, false) }
  check (looted.cursorStack == some ⟨.lihzahrdPowerCell, 1⟩ &&
    (looted.chestAt anchor).any (fun chest => chest.slots[0]?.join.isNone))
    "The normal chest interface did not transfer the temple's Power Cell."
  saveRoundTrip looted

private def golemChecks : IO Unit := do
  let fresh := newGame 42
  let some layout := TempleLayout.sites.find? (fun layout =>
      TempleDoor.atCell fresh.world layout.door == some (layout.door, .locked))
    | throw (IO.userError "Missing temple for the Golem encounter.")
  let plate := layout.worldCell 11 5
  let trap := layout.worldCell 35 5
  let playerAt (x floor : Int) : Player :=
    ⟨x * tileUnits + 38, floor * tileUnits - playerHeight, 0, 0, true⟩
  let atPlate := playerAt (plate.x + 2) (plate.y + 1)
  let wired := { fresh with player := atPlate, chart := fresh.chart.reveal fresh.world atPlate.centerTile }
  let mut triggered := wired
  for _ in [:80] do
    if !triggered.templeCooldowns.isEmpty then break
    triggered := step triggered { horizontal := -1 }
  check (triggered.templeCooldowns.any (·.1 == trap) &&
    triggered.templeTrapShots.any (·.kind == .dart))
    "Walking onto the Lihzahrd Pressure Plate did not fire the wired Super Dart Trap."
  saveRoundTrip triggered
  let struck ← walk triggered 45 0
  check (struck.poisonedTicks > 800 && struck.combat.health < fresh.combat.health)
    "A Super Dart did not damage and poison the player in the temple corridor."
  let again := { triggered with player := wired.player }
  let cooling ← walk again 36 (-1)
  check (cooling.templeCooldowns.any (fun (p, ticks) => p == trap &&
      ticks + 36 == ((triggered.templeCooldowns.find? (·.1 == trap)).map (·.2)).getD 0))
    "Re-entering the same pressure plate restarted an active trap cooldown."

  let some altar := (Array.range fresh.world.tiles.size).findSome? (fun (i : Nat) =>
      let p : TilePos := ⟨i % fresh.world.width, i / fresh.world.width⟩
      if fresh.world.tile p == .lihzahrdAltarUpperMiddle then some p else none)
    | throw (IO.userError "Missing framed altar in the Golem chamber.")
  let player := playerAt (altar.x + 3) (altar.y + 2)
  let ready := { fresh with
    player, hardmode := true, twinsDefeated := true
    destroyerDefeated := true, primeDefeated := true, planteraUnlocked := true
    chart := fresh.chart.reveal fresh.world player.centerTile
    inventory := fresh.inventory.set! 8 (some ⟨.lihzahrdPowerCell, 2⟩) }
  let use : TickInput := { interact := some altar }
  let locked := step ready use
  check (locked.golem.isNone && Inventory.count locked.inventory .lihzahrdPowerCell == 2)
    "The altar used a cell before defeating Plantera."
  let cleared := { ready with planteraDefeated := true }
  let noCell := step { cleared with inventory := cleared.inventory.set! 8 none } use
  check noCell.golem.isNone "The altar summoned Golem without a Power Cell."
  let active := step cleared use
  check (active.golem.any (fun body => body.health == 15000 && body.initialized && body.invulnerable) &&
    active.golemHead.any (fun head => head.health == 25000 && !head.detached) &&
    active.golemFists.map (·.health) == #[10000, 10000] &&
    Inventory.count active.inventory .lihzahrdPowerCell == 1 &&
    Inventory.count (step active use).inventory .lihzahrdPowerCell == 1)
    "The altar did not summon the Classic parts for exactly one Power Cell."
  saveRoundTrip active
  let arrowAt (x y : Float32) : Projectile := {
    x := x.toInt32.toInt * 16 - 226, y := y.toInt32.toInt * 16, vx := 226, vy := 0
    life := ShotKind.unholyArrow.lifetime, kind := .unholyArrow, hitsLeft := 5, slot := some 0 }
  let head := active.golemHead.getD default
  let detached := step { active with
    golemHead := some { head with health := 1 }
    projectiles := #[arrowAt head.centerX head.centerY] } {}
  let second := step detached {}
  check (second.golemHead.any (·.detached) && second.golem.any (!·.invulnerable))
    "Defeating the head did not detach it and make Golem's body vulnerable."
  let body := second.golem.getD default
  let fireball := GolemShot.fire ⟨.fireball, body.centerX, body.centerY, 3, -2, 20⟩ 1
  let dart := TempleTraps.launch .dart trap 18 (fun lo _ => lo) (fun lo _ => lo)
  let saved := { second with
    golemShots := #[fireball]
    templeTrapShots := #[{ dart with slot := some 2 }], hellfireTicks := 45, hellfireProgress := 14 }
  saveRoundTrip saved
  let decoded ← IO.ofExcept ((Save.decode (Save.encode saved)).mapError IO.userError)
  check (sameSnapshot (step decoded {}) (step saved {}))
    "Restoring the detached head, projectiles and Hellfire changed the next frame."
  let finish := { second with
    golem := some { body with health := 1 }, npcImmunity := #[]
    projectiles := #[arrowAt body.centerX (body.centerY + 30)]
    equipment := { second.equipment with accessory := some .treasureMagnet } }
  let defeated := step finish {}
  let husks := Inventory.count defeated.inventory .beetleHusk + defeated.nightDrops.foldl
    (fun total drop => total + ((drop.items.find? (·.item == .beetleHusk)).map (·.count)).getD 0) 0
  check (defeated.golemDefeated && defeated.golem.isNone && defeated.golemFists.isEmpty &&
    defeated.golemHead.isNone && 4 ≤ husks && husks ≤ 8)
    "Ending the second phase did not grant the Classic Beetle Husk reward."
  saveRoundTrip defeated

private def templeEnemyChecks : IO Unit := do
  let fresh := newGame 42
  let player : Player := ⟨397 * tileUnits + 38, 94 * tileUnits - playerHeight, 0, 0, true⟩
  let ready := { fresh with player, chart := fresh.chart.reveal fresh.world player.centerTile }
  let spawnAt (clock : Nat) := step { ready with surface := { ready.surface with clock } } {}
  let standing := spawnAt 55805
  let flying := spawnAt 55980
  check (!standing.hardmode && !standing.planteraDefeated && standing.templeEnemies.size == 1 &&
    standing.templeEnemies[0]?.any (fun e => e.kind == .lihzahrd && e.health == 400) &&
    flying.templeEnemies.size == 1 && flying.templeEnemies[0]?.any (fun e => e.kind == .flyingSnake && e.health == 260))
    "The natural temple walls did not spawn both Classic enemies before Hardmode."
  let outside := step { fresh with surface := { fresh.surface with clock := 55805 } } {}
  check outside.templeEnemies.isEmpty "A Lihzahrd spawned outside the unsafe temple wall biome."
  saveRoundTrip flying
  let loaded ← IO.ofExcept ((Save.decode (Save.encode flying)).mapError IO.userError)
  check (sameSnapshot (step flying {}) (step loaded {})) "Reload changed the Flying Snake's next tick."

  let arrowFor (enemy : TempleEnemy) : Projectile := {
    x := enemy.centerX.toInt32.toInt * 16 - 226, y := enemy.centerY.toInt32.toInt * 16
    vx := 226, vy := 0, life := ShotKind.unholyArrow.lifetime, kind := .unholyArrow
    hitsLeft := 5, slot := some 0 }
  let lizard := { TempleEnemy.atFloor 1 .lihzahrd ⟨399, 94⟩ with health := 221 }
  let armed := { ready with
    templeEnemies := #[lizard], nextTempleEnemyId := 2
    projectiles := #[arrowFor lizard] }
  let wounded := step armed {}
  check (wounded.templeEnemies[0]?.any (fun e => e.kind == .lihzahrd && e.health ≤ 220))
    "A projectile did not bring the Lihzahrd to its transform threshold."
  let crawling := step wounded {}
  check (crawling.templeEnemies[0]?.any (fun e =>
    e.id == 1 && e.kind == .crawlingLihzahrd && e.kind.defense == 30 &&
    wounded.templeEnemies[0]?.any (·.health == e.health)))
    "The hurt Lihzahrd did not crawl with its identity, health and defense preserved."
  let runner : TempleEnemy := { TempleEnemy.atFloor 1 .crawlingLihzahrd ⟨399, 94⟩ with vx := 4, vy := -1 }
  let struck := runner.knockback 20 1 5
  check (struck.justHit && struck.vx == runner.vx && struck.vy == runner.vy)
    "The crawling Lihzahrd lost its native knockback immunity."
  saveRoundTrip crawling

  let snake := { TempleEnemy.atFloor 1 .flyingSnake ⟨399, 94⟩ with health := 1 }
  let defeated := step { armed with templeEnemies := #[snake], projectiles := #[arrowFor snake] } {}
  check (defeated.templeEnemies.isEmpty && defeated.templeEnemySound == some (.flyingSnake, true) &&
    (defeated.coins > ready.coins || defeated.nightDrops.any (·.coins > 0)))
    "Killing a Flying Snake did not remove it and drop coins through ordinary combat."
  let loot := templeEnemyDrops (fun i => if i == 3 then 1 else 0)
  check (loot == #[⟨.lizardEgg, 1⟩, ⟨.lihzahrdPowerCell, 1⟩, ⟨.solarTabletFragment, 2⟩] &&
    (templeEnemyDrops (fun _ => 1)).isEmpty) "The native temple enemy drop table changed."
  saveRoundTrip { defeated with inventory := defeated.inventory.set! 12 (some ⟨.lizardEgg, 1⟩) }

private def queenSlimeChecks : IO Unit := do
  let world : World := ⟨100, 70, ⟨(Array.range (100 * 70)).map fun i =>
    if i / 100 ≥ 48 then Tile.stone.code else Tile.air.code⟩⟩
  let player : Player := ⟨45 * tileUnits, 48 * tileUnits - playerHeight, 0, 0, true⟩
  let queen : QueenSlime := { x := 480, y := 668, timer := 60 }
  let next (queen : QueenSlime) (sample : Nat) :=
    queen.advance world default default player false (fun _ => sample)
  check (((next queen 0).1.map (·.phase)) == some .jumps &&
    ((next queen 1).1.map (·.phase)) == some .smash &&
    ((next queen 2).1.map (·.phase)) == some .gel)
    "Queen Slime's three grounded attacks changed."
  let (_, bodyGel) := next { queen with phase := .gel, timer := 9, sequence := 1 } 0
  let (_, wingedGel) := next { queen with
    phase := .gel, timer := 9, sequence := 1, health := queenSlimeHealth / 2 } 0
  check (bodyGel.size == 6 && wingedGel.size == 10 && wingedGel[0]!.vx == 9 && wingedGel[0]!.vy == 0)
    "The grounded and winged Regal Gel volleys changed."
  let (_, slam) := next { queen with phase := .smash, sequence := 1, timer := 55 } 0
  check (slam.size == 1 && slam[0]!.kind == .stomp &&
    ({ slam[0]! with age := 9 } : QueenShot).diameter == 480)
    "The Queenly Smash no longer grows to 30 tiles."
  let (small, _) := next { queen with phase := .shrink, timer := 29, destination := some ⟨40, 48⟩ } 0
  check (small.any (fun boss => boss.scale == 0.75 && boss.bottom == queen.bottom))
    "Teleport scaling shifted Queen Slime away from her ground contact."
  let (_, atCutoff) := ({ queen with health := 17640 } : QueenSlime).spawnMinions 1 (fun _ => 1)
  let (_, belowCutoff) := ({ queen with health := 17639 } : QueenSlime).spawnMinions 1 (fun _ => 1)
  let (_, atFlightCutoff) := ({ queen with
      health := 8630, minionHealthMark := 8900 } : QueenSlime).spawnMinions 1 (fun _ => 1)
  let (_, belowFlightCutoff) := ({ queen with
      health := 8629, minionHealthMark := 8900 } : QueenSlime).spawnMinions 1 (fun _ => 1)
  check (atCutoff.isEmpty && atFlightCutoff.isEmpty && belowCutoff.size == 2 && belowFlightCutoff.size == 2)
    "Queen Slime's strict minion health thresholds changed."
  let items := queenSlimeRewards (fun _ => 0)
  check (items.size == 9 && items[0]? == some ⟨.sparkleSlimeBalloon, 25⟩ &&
    items.any (·.item == .crystalAssassinHood) && items.any (·.item == .queenSlimeTrophy))
    "The Queen Slime drop pool or guaranteed armor piece changed."
  let before := newGame 42
  let before := { before with inventory := before.inventory.set! 0 (some ⟨.gelatinCrystal, 2⟩) }
  let denied := step before { mine := some before.player.centerTile }
  check (denied.queenSlime.isNone && Inventory.count denied.inventory .gelatinCrystal == 2)
    "A Gelatin Crystal was consumed outside Hardmode Hallow."
  let world := convertHardmodeTerrain before.world before.seed (hardmodeConversionEntropy before.seed 0)
  let some player := (List.range' 7 498).findSome? fun (col : Nat) =>
      let row := groundHeight before.seed col
      let body : Player := ⟨col * tileUnits + 16, row * tileUnits - playerHeight, 0, 0, true⟩
      if body.collides world || !(world.hardmodeZones body.centerTile).1 then none else some body
    | throw (IO.userError "No surface Hallow spawn point.")
  let ready := { before with
    world, player, hardmode := true
    chart := before.chart.reveal world player.centerTile }
  let summoned := step ready { mine := some player.centerTile }
  let some boss := summoned.queenSlime | throw (IO.userError "Gelatin Crystal did not summon Queen Slime.")
  check (boss.health == queenSlimeHealth && Inventory.count summoned.inventory .gelatinCrystal == 1)
    "The summon did not consume exactly one crystal."
  let duplicate := step summoned { mine := some player.centerTile }
  check (duplicate.queenSlime.isSome && Inventory.count duplicate.inventory .gelatinCrystal == 1)
    "An active Queen Slime consumed a second crystal."
  let active := { duplicate with
    queenSlime := some { boss with phase := .gel, timer := 8, sequence := 1, health := 8888, minionHealthMark := 8888 }
    queenMinions := belowCutoff
    nextQueenMinionId := 3
    queenShots := bodyGel.mapIdx (fun slot shot => { shot with slot := some slot }) }
  saveRoundTrip active
  let restored ← IO.ofExcept <| (Save.decode (Save.encode active)).mapError IO.userError
  check (sameSnapshot (step active {}) (step restored {}))
    "Reload changed the next Queen Slime tick."

private def dissonanceHookChecks : IO Unit := do
  let floor : Int := 31
  let room : World := ⟨70, 40, ⟨(Array.range (70 * 40)).map fun i =>
    if i / 70 ≥ floor.toNat then Tile.stone.code else Tile.air.code⟩⟩
  let player : Player := ⟨5 * tileUnits, floor * tileUnits - playerHeight, 0, 0, true⟩
  let tile : TilePos := ⟨29, 30⟩
  let room := room.setTile tile .stone
  let aimX := tile.x * tileUnits + tileUnits / 2
  let aimY := bodyCenterY player
  let some launched := fireDissonanceHook room player aimX aimY none
    | throw (IO.userError "The Dissonance hook did not launch.")
  let (standing, first, _) := advanceGrapple room player (.flying launched) false
  check (standing == player && (match first with
    | .flying hook => hook.x - launched.x == tileUnits && hook.life + 4 == launched.life
    | _ => false)) "The hook did not take four four-pixel updates in one frame."
  let attach (world : World) := Id.run do
    let mut status := (player, GrappleState.flying launched, (none : Option GrappleEvent))
    for _ in [:35] do
      status := advanceGrapple world status.1 status.2.1 false
      if status.2.2.isSome then break
    return status
  let (teleported, latch, event) := attach room
  check (event == some .teleported && latch.attachment.any (·.tile == tile) &&
    teleported.x == tile.x * tileUnits - playerWidth && latch.valid room teleported)
    "An open corridor did not teleport the player to the wall and clear momentum."
  let lowRoof := room.setTile ⟨14, 29⟩ .stone
  let (clipped, stopped, event) := attach lowRoof
  check (event == some .teleported && stopped.attachment.any (·.tile == tile) &&
    clipped.x == 14 * tileUnits - playerWidth && stopped.valid lowRoof clipped)
    "The hook should fit through a one-tile opening while the player stops at its edge."
  let empty := room.setTile tile .air
  let mut out := GrappleState.flying launched
  let mut turned := false
  for _ in [:60] do
    let (_, grapple, _) := advanceGrapple empty player out false
    if let .flying flight := grapple then
      if !turned && flight.returning then
        check (flight.x - bodyCenterX player > dissonanceReach - tileUnits)
          "The reel-back started before the 500-pixel reach."
      turned := turned || flight.returning
    out := grapple
    if out == .idle then break
  check (turned && out == .idle) "A hook that missed should retract and disappear."
  let some attached := latch.attachment | throw (IO.userError "Missing hook attachment.")
  let (_, expired, event) := advanceGrapple room teleported (.latched { attached with life := 4 }) false
  check (expired == .idle && event == some .detached) "An expired attachment stayed active."

  let fresh := newGame 42
  let fresh := { fresh with inventory := fresh.inventory.set! 12 (some ⟨.hookOfDissonance, 1⟩) }
  let equipped := step fresh { equipGear := some 12 }
  check (equipped.equipment.hook == some .hookOfDissonance &&
    Inventory.count equipped.inventory .hookOfDissonance == 0) "The hook did not move into its equipment slot."
  let aim := WorldAim.atTile ⟨equipped.player.centerTile.x, equipped.player.centerTile.y + 3⟩
  let fired := step equipped { quickGrapple := some aim }
  check (match fired.grapple with | .flying hook => hook.kind == .dissonance | _ => false)
    "Quick grapple did not use the equipped hook with a different item selected."
  saveRoundTrip fired
  let landed ← walk fired 5 0
  check (landed.grapple.attachment.any (·.kind == .dissonance)) "The equipped hook did not attach to the floor."
  saveRoundTrip landed
  let nextAim := WorldAim.atTile ⟨landed.player.centerTile.x + 12, landed.player.centerTile.y - 3⟩
  let retarget := step landed { quickGrapple := some nextAim }
  check ((match retarget.grapple with | .flying hook => hook.held.isSome | _ => false) &&
    retarget.player == landed.player) "Retargeting released the first attachment before contact."
  saveRoundTrip retarget
  check ((step retarget { jump := true }).grapple == .idle) "Space did not release both hook heads."

private def bladeStaffChecks : IO Unit := do
  check (bladeStaffHitDamage 0 0 15 == 6 && bladeStaffHitDamage 0 24 15 == 6 &&
    bladeStaffHitDamage 0 25 15 == 5 && bladeStaffHitDamage 0 28 15 == 4 &&
    bladeStaffHitDamage 0 9999 15 == 1 && bladeStaffHitDamage 30 24 15 == 7)
    "Blade Staff damage diverged from the native armor penetration and truncation order."
  let spawn := EnchantedDagger.spawn (16 * 350) (16 * 200)
  let following := spawn.follow (16 * 10) (16 * 230)
  check (following.vx == -20.5 && following.vy == 0) "The dagger did not scale its return speed."
  let lunging := spawn.chase (16 * 320) (16 * 200)
  check (lunging.vx == -16 && lunging.pose.x == 334)
    "The dagger did not use a 16-pixel homing strike."
  let recoiling := lunging.recoil (some 0)
  check ((recoiling.vx + 11.04).abs < 0.00001 && recoiling.pose.x < lunging.pose.x &&
    recoiling.trail[0]? == some lunging.pose) "The dagger recoil did not add and damp its edge velocity."
  let base := newGame 42
  let mut world := base.world
  for x in [7:60] do
    for y in [20:44] do
      world := world.setTile ⟨x, y⟩ (if y == 43 then .stone else .air)
  let player : Player := ⟨15 * tileUnits + 38, 43 * tileUnits - playerHeight, 0, 0, true⟩
  let initial := { base with
    world, player, inventory := base.inventory.set! 0 (some ⟨.bladeStaff, 1⟩)
    mana := 0, manaRegenDelay := aquaManaDelay
    chart := base.chart.reveal world player.centerTile }
  let aim := WorldAim.atTile ⟨24, 33⟩
  let cast := step initial { mine := some ⟨24, 33⟩, aim := some aim }
  check (cast.mana == 0 && cast.bowCooldown == 38 && cast.dagger.any (fun dagger =>
    dagger.slot == 0 && dagger.blade.trail[0]? == some (EnchantedDagger.spawn aim.x aim.y).pose))
    "The staff did not summon at the cursor without spending mana."
  let blocked := step cast { mine := some ⟨26, 35⟩ }
  check (blocked.bowCooldown == 37 && blocked.dagger.any (·.blade.trail.size == 2))
    "A second cast bypassed the use animation and reuse delay."
  let mut ready := blocked
  for _ in [:36] do ready := step ready {}
  let recast := step ready { useHeld := true, aim := some (WorldAim.atTile ⟨26, 35⟩) }
  check (recast.dagger.any (fun dagger => dagger.slot == 0 && dagger.blade.trail.size == 1) &&
    recast.bowCooldown == 38) "Holding use did not replace the dagger at the native base slot limit."
  let zombie : NightCreature := {
    body := { player with x := 26 * tileUnits },
    health := SurfaceKind.zombie.maxHealth, hopIn := 20, kind := .zombie }
  let eye : NightCreature := {
    body := { player with x := 35 * tileUnits, y := 36 * tileUnits, grounded := false },
    health := SurfaceKind.demonEye.maxHealth, hopIn := 0, kind := .demonEye }
  let encounter := { recast with
    surface := { clock := classicDayTicks + 130, creature := some zombie, flying := some eye }
    dagger := recast.dagger.map fun dagger => { dagger with
      blade := EnchantedDagger.spawn (bodyCenterX player) (player.y - 30 * 16) } }
  let automatic := step encounter {}
  check (automatic.dagger.any (·.phase == .chase (.surface .zombie)))
    "The dagger did not acquire the nearest visible foe."
  let mark : WorldAim := ⟨bodyCenterX eye.body, bodyCenterY eye.body⟩
  let preferred := step encounter { minionAim := some mark }
  check (preferred.minionTarget == some (.surface .demonEye) &&
    preferred.dagger.any (·.phase == .chase (.surface .demonEye)))
    "Right-click priority did not override automatic acquisition."
  check ((step preferred { minionAim := some mark }).minionTarget.isNone)
    "Right-clicking the marked enemy again did not clear it."
  let striking := { encounter with
    npcImmunity := #[(.surface .zombie, npcHitCooldown)]
    dagger := some {
      slot := 0, phase := .chase (.surface .zombie),
      blade := EnchantedDagger.spawn (bodyCenterX zombie.body) (bodyCenterY zombie.body) } }
  let hit := step striking {}
  check (hit.dagger.any (fun dagger => dagger.phase == .recoil 0 &&
      dagger.cooldowns == #[(.surface .zombie, bladeStaffLocalImmunity)]) &&
    !hit.npcImmunity.any (·.1 == .surface .zombie) &&
    hit.surface.creature.any (fun enemy => enemy.health + 5 ≤ zombie.health && zombie.health ≤ enemy.health + 7))
    "A dagger strike did not use and refresh its own immunity, and clear the shared immunity."
  let next := step hit {}
  check (next.dagger.any (·.cooldowns == #[(.surface .zombie, bladeStaffLocalImmunity - 1)]) &&
    next.surface.creature.map (·.health) == hit.surface.creature.map (·.health))
    "The local timer did not prevent a second immediate hit."
  saveRoundTrip hit
  let restored ← IO.ofExcept <| (Save.decode (Save.encode hit)).mapError IO.userError
  check (sameSnapshot (step restored {}) next) "Reload changed the dagger's next recoil frame."
  check ((step restored { cancelDagger := true }).dagger.isNone) "Canceling the buff kept the dagger."
  let emblem := { initial with inventory := initial.inventory.set! 12 (some ⟨.summonerEmblem, 1⟩) }
  let equipped := step emblem { equipGear := some 12 }
  check (equipped.equipment.accessory == some .summonerEmblem &&
    equipped.equipment.summonDamageBonus == 15 && equipped.equipment.damageBonus == 0)
    "The Summoner Emblem did not grant its class-specific damage bonus."

private def wingedSlimeChecks : IO Unit := do
  let floor : Int := 120
  let room : World := ⟨40, 122, ⟨(Array.range (40 * 122)).map fun i =>
    if i / 40 ≥ floor.toNat then Tile.stone.code else Tile.air.code⟩⟩
  let player : Player := ⟨10 * tileUnits + 38, floor * tileUnits - playerHeight, 0, 0, true⟩
  let empty := Water.empty room
  let run := fun player mount horizontal jump down =>
    advanceWingedSlime room empty empty empty player mount horizontal jump down none
  let (launched, rider) := run player {} 0 true false
  check (rider.flight == 80 && rider.jump == 10 && rider.vy == -6.85 &&
    launched.y == player.y - 219) "Mounted ground jump did not run both vertical passes."
  let mut flight := (launched, rider)
  for _ in [:120] do
    if flight.2.flight == 0 then break
    flight := run flight.1 flight.2 0 true false
  check (flight.2.flight == 0 && flight.2.vy == -5.4375 &&
    player.y - flight.1.y > 50 * tileUnits) "Mounted flight did not exhaust the desktop mount's fuel."
  let falling : Player := { player with y := player.y - 24 * tileUnits, grounded := false, vy := 160 }
  let fall := WingedSlime.start falling
  let (fast, _) := run falling fall 0 false false
  let (glide, gliding) := run falling fall 0 true false
  let (lower, hovering) := run falling { fall with flight := 2, vy := 0.4 } 0 true true
  check (fast.y - falling.y == 320 && glide.y - falling.y == 106 &&
    gliding.vy > 3.3 && gliding.vy < 3.4 && lower.y == falling.y &&
    hovering.vy == 0.00001 && hovering.flight == 1)
    "Falling, gliding or holding Down while flying diverged."
  let fastAboveFloor := { falling with y := player.y - 250 }
  let (landed, landMount) := run fastAboveFloor fall 0 false false
  check (landed.y == player.y && landed.grounded && landMount.vy == 0)
    "The second vertical pass skipped the floor."
  let (again, recharged) := run landed landMount 0 true false
  check (again.y < landed.y && recharged.flight == 80) "Held jump did not take off again on landing."
  let liquidCell : TilePos := ⟨10, 100⟩
  let liquid := Magma.fillFromEighths empty room #[(liquidCell, 8)]
  let wetBody : Player := { player with y := 100 * tileUnits - playerHeight + 80, grounded := false }
  for (water, honey, magma) in [(liquid, empty, empty), (empty, liquid, empty), (empty, empty, liquid)] do
    let (rising, buoyant) := advanceWingedSlime room water honey magma wetBody {} 0 false false none
    check (rising.y < wetBody.y && buoyant.wetSlime == 30) "A mounted slime did not rise in liquid."

  let base := newGame 42
  let mut world := base.world
  for x in [7:45] do
    for y in [23:43] do
      world := world.setTile ⟨x, y⟩ (if y == 42 then .stone else .air)
  let player : Player := ⟨15 * tileUnits + 38, 42 * tileUnits - playerHeight, 0, 0, true⟩
  let initial := { base with
    world, player
    inventory := base.inventory.set! 12 (some ⟨.gelatinousPillion, 1⟩)
    chart := base.chart.reveal world player.centerTile }
  let quick := step initial { quickMount := true }
  check (quick.wingedSlime.isSome && Inventory.count quick.inventory .gelatinousPillion == 1)
    "Quick mount did not use the backpack saddle without consuming it."
  let dismissed := step quick { cancelMount := true }
  check dismissed.wingedSlime.isNone "Canceling the mount buff left the rider mounted."
  let equipped := step dismissed { equipGear := some 12 }
  check (equipped.equipment.mount == some .gelatinousPillion && Inventory.count equipped.inventory .gelatinousPillion == 0)
    "The saddle did not go in the mount slot."
  let low := { equipped with world := world.setTile ⟨15, 39⟩ .stone }
  check ((step low { quickMount := true }).wingedSlime.isNone)
    "The player mounted in a space only the unmounted body fits."
  let hotbar := { initial with inventory := initial.inventory.set! 0 (some ⟨.gelatinousPillion, 1⟩) }
  check ((step hotbar { mine := some player.centerTile }).wingedSlime.isSome)
    "Using the saddle from the hotbar did not summon the mount."
  let airborne := step equipped { quickMount := true, swim := true, horizontal := 1 }
  saveRoundTrip airborne
  let restored ← IO.ofExcept <| (Save.decode (Save.encode airborne)).mapError IO.userError
  check (sameSnapshot (step airborne { swim := true }) (step restored { swim := true }))
    "Reload changed the next mounted flight frame."
  check ((step airborne { quickMount := true }).wingedSlime.isNone)
    "The quick-mount key did not dismount the mounted player."
  let enemy : NightCreature := { body := player, health := 14, hopIn := 20, kind := .greenSlime }
  let dive : Player := { player with y := player.y - playerHeight - 10, vy := 48, grounded := false }
  let stomping := { quick with
    player := dive, wingedSlime := some (WingedSlime.start dive)
    surface := { quick.surface with creature := some enemy } }
  let bounce := step stomping {}
  check (bounce.wingedSlime.any (·.stompCount == 1) && bounce.player.vy < -150 &&
    bounce.combat.health == quick.combat.health && !bounce.surface.creature.any (·.kind == .greenSlime))
    "The mounted player failed to stomp and bounce off a green slime."

private def crystalAssassinChecks : IO Unit := do
  let base := newGame 42
  let mut world := base.world
  for x in [7:57] do
    for y in [26:44] do
      world := world.setTile ⟨x, y⟩ (if y == 43 then .stone else .air)
  let player : Player := ⟨15 * tileUnits + 36, 43 * tileUnits - playerHeight, 0, 0, true⟩
  let inventory := base.inventory.set! 10 (some ⟨.crystalAssassinHood, 1⟩)
  let inventory := inventory.set! 11 (some ⟨.crystalAssassinShirt, 1⟩)
  let inventory := inventory.set! 12 (some ⟨.crystalAssassinPants, 1⟩)
  let initial := { base with world, player, inventory, chart := base.chart.reveal world player.centerTile }
  let hood := step initial { equipGear := some 10 }
  check (hood.defense == 12 && hood.equipment.manaCost 14 == 12 && hood.equipment.criticalBonus == 5)
    "The hood did not grant the native defense, critical chance, and mana cost."
  let shirt := step hood { equipGear := some 11 }
  check (shirt.defense == 26 && shirt.equipment.damageBonus == 5 && shirt.equipment.ammoSavings == 10 &&
    !shirt.equipment.crystalAssassinSet) "The shirt should grant its bonuses before completing the set."
  let incomplete := step (step shirt { dashTap := 1 }) { dashTap := 1 }
  check (incomplete.assassinDash == {} && incomplete.player.x == shirt.player.x)
    "A partial set could dash."
  let dressed := step shirt { equipGear := some 12 }
  check (dressed.defense == 36 && dressed.equipment.crystalAssassinSet &&
    dressed.equipment.speed == 36 && dressed.equipment.meleeTicks 22 == 20 &&
    dressed.equipment.damageBonus == 15 && dressed.equipment.criticalBonus == 15)
    "The set's armor, movement, damage and critical bonuses changed."
  let ready := step dressed { dashTap := 1 }
  check (ready.assassinDash.tap == 15 && ready.assassinDash.velocity == 0)
    "One press started a dash."
  let dashing := step (step ready {}) { horizontal := 1, dashTap := 1 }
  check (dashing.assassinDash.velocity == assassinDashSpeed &&
    dashing.player.x - ready.player.x == 270) "The second press should start a 16.9 pixel dash."
  saveRoundTrip dashing
  let restored ← IO.ofExcept <| (Save.decode (Save.encode dashing)).mapError IO.userError
  check (sameSnapshot (step dashing {}) (step restored {})) "Reload changed the next dash frame."
  let obstacle : TilePos := ⟨19, 42⟩
  let wall := { dashing with world := dashing.world.setTile obstacle .stone }
  let blocked ← walk wall 8 0
  check (!blocked.player.collides blocked.world && blocked.player.x < obstacle.x * tileUnits &&
    blocked.assassinDash.cooldown > 0) "The dash crossed a solid tile or skipped cooldown."
  let canceled := step dashing { unequipGear := some .legs }
  check (canceled.assassinDash == {} && !canceled.equipment.crystalAssassinSet)
    "Removing the pants left an active dash."
  let waited ← walk ready 15 0
  check ((step waited { dashTap := 1 }).assassinDash.velocity == 0)
    "A second press outside the 15-frame window launched a dash."

  let magic := { dressed with inventory := dressed.inventory.set! 0 (some ⟨.magicMissile, 1⟩) }
  let cast := step magic { mine := some ⟨29, 40⟩, useHeld := true }
  check (cast.mana == baseMana - 12 && cast.magicMissiles.size == 1 &&
    cast.weaponArmorStats == #[(0, { damageBonus := 15, criticalChance := 19 })])
    "Casting with the set did not pay the discounted mana or snapshot the offensive stats."
  let unequipped := step cast { unequipGear := some .armor, useHeld := true }
  check (unequipped.equipment.damageBonus == 0 && unequipped.weaponArmorStats == cast.weaponArmorStats)
    "Changing armor altered the stats of an already launched projectile."
  saveRoundTrip unequipped
  let bow := { dressed with inventory := dressed.inventory.set! 0 (some ⟨.goldBow, 1⟩) }
  let bow := { bow with inventory := bow.inventory.set! Inventory.ammoStart (some ⟨.arrow, 30⟩) }
  let mut conserved := 0
  for clock in [:72] do
    let shooter := { bow with surface := { bow.surface with clock := clock + 500 } }
    let shot := step shooter { mine := some ⟨29, 40⟩ }
    check (!shot.projectiles.isEmpty) "The test bow failed to fire."
    if Inventory.count shot.inventory .arrow == 30 then conserved := conserved + 1
  check (0 < conserved && conserved < 72) "The shirt did not conserve some of the fired ammunition."

private def waterCatchChecks : IO Unit := do
  let rng := UnifiedRandom.seed 9
  let waterCatch (rarity : FishingRarity) (biomes : FishingBiomes) (layer tiles : Nat) :=
    waterFishingCatch rarity biomes false false layer tiles none false rng
  check (waterCatch {} { ocean := true } 1 1000 == some .bass &&
    waterCatch {} { ocean := true } 1 1001 == some .trout &&
    waterCatch {} { ocean := true } 2 1001 == some .bass &&
    waterCatch { common := true } {} 1 1001 == some .salmon)
    "Ocean catches or the large freshwater Salmon threshold changed."
  check (waterCatch { uncommon := true } { jungle := true } 2 300 == some .variegatedLardfish &&
    waterCatch { uncommon := true } { jungle := true } 1 300 == some .doubleCod &&
    waterCatch { rare := true } { corruption := true } 2 300 == some .purpleClubberfish &&
    waterCatch { common := true } { crimson := true } 1 300 == some .crimsonTigerfish)
    "Water catches no longer respect biome and depth."
  check (fishingCatchQuantity .bombFish 50 (fun _ => 0) == 2 &&
    fishingCatchQuantity .bombFish 50 (fun draw => if draw == 0 then 7 else 0) == 9 &&
    fishingCatchQuantity .frostDaggerfish 100 (fun _ => 0) == 20 &&
    fishingCatchQuantity .bass 100 (fun _ => 0) == 1)
    "Fish stack sizes no longer match the reel-in formula."
  let state := newGame 42
  let oceanX := state.world.width - 10
  let some waterY := (List.range state.world.height).find? (fun (y : Nat) =>
      state.water.level state.world ⟨oceanX, y⟩ > 0)
    | throw (IO.userError "The east coast has no ocean surface.")
  let pond := fishingPondTiles state.world state.water ⟨oceanX, waterY⟩
  check (pond ≥ 75 && nativeOceanPondTiles state.world.width pond > 1000 &&
    state.world.tile (deepsteelMine state.seed).marker == .mineMarker)
    "The generated coast cannot satisfy the Ocean catch threshold."
  let bait := state.inventory.set! Inventory.ammoStart (some ⟨.masterBait, 20⟩)
  let inventory := bait.set! 3 (some ⟨.fishingRod, 1⟩)
  let motion := FishingBobber.atSurface ⟨oceanX, waterY⟩ 255 0
  let hooked := { state with inventory, selected := 3, fishing := .bobber motion (.hooked 120 .reaverShark .masterBait) }
  saveRoundTrip hooked
  let floor := { state with nightDrops := #[⟨state.player.centerTile, 0, #[⟨.frostDaggerfish, 32⟩]⟩] }
  saveRoundTrip floor
  check (Item.reaverShark.pickPower == 59 && Item.sawtoothShark.axePower == 70 &&
    Item.rockfish.hammerPower == 70) "Fishing tools lost their native tool power."

private def prepareFishingGear (home : GameState) : IO GameState := do
  let pick ← equip home .basicPick
  let supplied := if Inventory.count pick.inventory .dirt > 0 then pick
    else step pick { mine := some ⟨17, 40⟩ }
  let caveBench ← rangedKit supplied 0
  let pick ← equip caveBench .basicPick
  let stocked := step (step pick { mine := some ⟨92, 52⟩ }) { mine := some ⟨93, 52⟩ }
  let rod := step stocked { craftFishingRod := true }
  check (Inventory.count rod.inventory .fishingRod == 1 &&
    Inventory.count rod.inventory .wood + rodWood == Inventory.count stocked.inventory .wood &&
    Inventory.count rod.inventory .stone + rodStone == Inventory.count stocked.inventory .stone)
    s!"Natural materials did not craft the rod. {rod.status}"
  let gear := step rod { craftBait := true }
  check (Inventory.count gear.inventory .bait == baitBatch &&
    Inventory.count gear.inventory .wood + 1 == Inventory.count rod.inventory .wood &&
    Inventory.count gear.inventory .dirt + 1 == Inventory.count rod.inventory .dirt)
    s!"Natural wood and dirt did not craft the bait. {gear.status}"
  return gear

private def castFrom (bank : GameState) (target : TilePos) : IO GameState := do
  let rod ← equip bank .fishingRod
  let cast := step rod { mine := some target }
  let .waiting position ticks := cast.fishing
    | throw (IO.userError s!"Could not cast from {repr rod.player.centerTile} into {repr target}. {cast.status}")
  check (ticks > 0 && ticks < fishingWaitTicks &&
    fishingSpot cast.world cast.water cast.player position == some position && cast.inventory == rod.inventory)
    "Casting spent bait or did not store the actual water surface."
  return cast

private def waitForBite (cast : GameState) : IO GameState := do
  let .waiting position ticks := cast.fishing | throw (IO.userError "Missing cast to advance.")
  let last ← walk cast (ticks - 1) 0
  check (last.fishing == .waiting position 1) "The wait timer changed before the bite."
  let biting := step last {}
  check (biting.fishing == .biting position fishingBiteTicks) "The bite did not start on time."
  return biting

private def landFish (cast : GameState) (fish : Item) : IO GameState := do
  saveRoundTrip cast
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode cast)).mapError IO.userError
  let biting ← waitForBite cast
  check (sameSnapshot biting (← waitForBite loaded)) "Reading a live cast changed its next bite."
  saveRoundTrip biting
  let before := Inventory.count biting.inventory .bait
  let action : TickInput := { mine := some biting.player.centerTile }
  let landed := step biting action
  check (landed.fishing == .idle && Inventory.count landed.inventory .bait + 1 == before &&
    Inventory.count landed.inventory fish == Inventory.count biting.inventory fish + 1)
    s!"The catch did not land a {fish.name} for exactly one bait. {landed.status}"
  return landed

private def fishWesternPond (gear : GameState) : IO GameState := do
  let east ← walkToColumn gear 26 950
  let door : TilePos := ⟨24, 38⟩
  let opened := if east.world.tile door == .doorClosed then step east { interact := some door } else east
  let mut state ← equip opened .basicPick
  for tree in [12, 10, 8, 6] do
    state ← walkToColumn state (tree + 1 : Nat) 180
    let floor := groundHeight state.seed tree
    for row in [floor - 4:floor] do
      let timber : TilePos := ⟨tree, row⟩
      if state.world.tile timber == .timber then state := step state { mine := some timber }
  let bank ← walkToColumn state 6 110
  let top := surfacePondTop bank.seed
  let target : TilePos := ⟨4, top + 1⟩
  let cast ← castFrom bank target
  let surface : TilePos := ⟨4, top⟩
  check (match cast.fishing with | .waiting position _ => position == surface | _ => false)
    "Clicking deeper water did not cast to the surface."
  let reeled := step cast { mine := some cast.player.centerTile }
  check (reeled.fishing == .idle && reeled.inventory == cast.inventory) "Reeling early spent bait."
  let jumping := step cast { jump := true }
  check (jumping.fishing == .idle && jumping.inventory == cast.inventory) "Jumping with a cast spent bait."
  let biting ← waitForBite cast
  let missed ← walk biting fishingBiteTicks 0
  check (missed.fishing == .idle && missed.inventory == cast.inventory) "A missed bite spent bait."
  let full : Inventory := (fullBackpack)
    |>.set! biting.selected (some ⟨.fishingRod, 1⟩)
    |>.set! (Inventory.mainSlotCount - 1) (some ⟨.bait, 2⟩)
  let blocked := step { biting with inventory := full } { mine := some biting.player.centerTile }
  check (blocked.inventory == full && (match blocked.fishing with | .biting .. => true | _ => false))
    "A full inventory spent bait or discarded a still-live bite."
  let oneBait := full.set! (Inventory.mainSlotCount - 1) (some ⟨.bait, 1⟩)
  let swapped := step { biting with inventory := oneBait } { mine := some biting.player.centerTile }
  check (swapped.fishing == .idle && Inventory.count swapped.inventory .bait == 0 &&
    Inventory.count swapped.inventory .pondPerch == 1) "The last bait did not free its slot for the catch."
  let perch ← landFish cast .pondPerch
  let equipped ← equip perch .pondPerch
  let injured := { equipped with combat := { equipped.combat with health := maxHealth - 2 * healthPerHeart } }
  let fed := step injured { mine := some injured.player.centerTile }
  check (fed.combat.health == maxHealth - healthPerHeart && Inventory.count fed.inventory .pondPerch == 0)
    "Eating a caught perch did not restore one health."
  let pick ← equip fed .basicPick
  let deeper := step pick { mine := some ⟨5, top + 3⟩ }
  check (deeper.world.tile ⟨5, top + 3⟩ == .air && waterVolume deeper == waterVolume pick)
    "Digging a deeper pool changed the amount of water."
  let stone ← equip (← walk deeper 80 0) .stone
  let reshaped := step stone { place := some surface }
  check (reshaped.world.tile surface == .stone && waterVolume reshaped == waterVolume stone)
    "Building in the pond did not displace its water."
  let rod ← equip reshaped .fishingRod
  let denied := step rod { mine := some surface }
  check (denied.fishing == .idle && denied.inventory == rod.inventory) "A cast entered the new solid block."
  let settled ← walk denied 100 0
  let jump ← walk settled 16 (-1) true
  let bank ← walkToColumn jump surface.x 100
  let bank ← walk bank 3 (-1)
  check (bank.player.y + playerHeight == surface.y * tileUnits)
    "The new stone did not provide a foothold above the reshaped pond."
  let targets := (List.range 4).flatMap fun offset =>
    ([2, 3, 5] : List Int).map fun column => (⟨column, Int.ofNat (top + offset)⟩ : TilePos)
  let some other := targets.findSome? (fishingSpot bank.world bank.water bank.player)
    | throw (IO.userError s!"No reachable surface from {repr bank.player.centerTile}.")
  let caught ← landFish (← castFrom bank other) .pondPerch
  saveRoundTrip caught
  let jump ← walk caught 16 1 true
  return ← walkToColumn jump 6 120

private def fishQuarry (ledge : GameState) : IO GameState := do
  let shaft ← walkToColumn ledge 216 140
  let bank ← walkToColumn shaft 219 180
  let cast ← castFrom bank ⟨222, 74⟩
  let eel ← landFish cast .caveEel
  let deep ← swimEastTo eel 223 100
  let submerged ← walk deep 270 0
  check (submerged.breath < maxBreath - 100 && submerged.combat.health ≥ eel.combat.health)
    "Could not spend breath safely in the actual quarry reservoir."
  let selected ← equip submerged .caveEel
  let fed := step selected { mine := some selected.player.centerTile }
  check (fed.breath == min maxBreath (selected.breath + eelBreathTicks) - 1 &&
    Inventory.count fed.inventory .caveEel == 0) "Eating the cave eel did not restore breath underwater."
  saveRoundTrip fed
  return eel

private def cultivateSunleaf (caught : GameState) : IO GameState := do
  let pick ← equip caught .basicPick
  let some wild := (wildSunleaf pick.seed).find? fun position =>
      pick.canReach position && pick.world.tile position == .sunleaf
    | throw (IO.userError "No wild Sunleaf was reachable from the western bank.")
  let harvested := step pick { mine := some wild }
  check (Inventory.count harvested.inventory .sunleafSeed == 2 && Inventory.count harvested.inventory .sunleaf == 1)
    "The wild Sunleaf did not drop a leaf and two seeds."
  let some plot := ([8, 9, 10, 11, 12] : List Nat).findSome? fun (x : Nat) =>
      let position : TilePos := ⟨x, groundHeight harvested.seed x - 1⟩
      if harvested.canReach position && canPlantSunleaf harvested.world harvested.water position then
        some position else none
    | throw (IO.userError "The grove did not offer an open dirt plot after harvesting the timber.")
  let seed ← equip harvested .sunleafSeed
  let roof : TilePos := { plot with y := plot.y - 1 }
  let denied := step seed { place := some roof }
  check (denied.inventory == seed.inventory && denied.crops.isEmpty) "Planting above air consumed a seed."
  let planted := step seed { place := some plot }
  check (planted.world.tile plot == .sunleafSprout && planted.crops.size == 1 &&
    Inventory.count planted.inventory .sunleafSeed == 1) "Planting did not add one timed sprout."
  let pick ← equip planted .basicPick
  let refused := step pick { mine := some { plot with y := plot.y + 1 } }
  check (refused.world == pick.world && refused.inventory == pick.inventory) "Mining soil detached a sprout."
  let early := step pick { mine := some plot }
  check (early.world.tile plot == .air && early.crops.isEmpty &&
    Inventory.count early.inventory .sunleafSeed == 2 && Inventory.count early.inventory .sunleaf == 1)
    "Uprooting an unripe plant did not return just its seed."
  let shaded ← buildWith pick .stone roof .stone
  let underRoof ← walk shaded 20 0
  check (underRoof.crops == shaded.crops) "Sunleaf grew under a solid roof."
  let pick ← equip underRoof .basicPick
  let clear := step pick { mine := some roof }
  let night := step { clear with surface := { clear.surface with clock := dayTicks } } {}
  check (night.crops == clear.crops) "Sunleaf grew at night."
  let cell := plot.y.toNat * clear.world.width + plot.x.toNat
  let wet : Water := { levels := clear.water.levels.set! cell 1 }
  check ((advanceGarden clear.world wet clear.crops true).2 == clear.crops) "A wet seedling grew."
  saveRoundTrip clear
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode clear)).mapError IO.userError
  let age := (loaded.crops[0]?.map (·.ticks)).getD cropGrowthTicks
  let ready ← walk loaded (cropGrowthTicks - age) 0
  check (ready.world.tile plot == .sunleaf && ready.crops.isEmpty &&
    ready.chart.tile? ready.world plot == some .sunleaf) "The sprout did not mature on schedule after reloading."
  let inventory : Inventory := (fullBackpack)
    |>.set! ready.selected (some ⟨.basicPick, 1⟩)
    |>.set! (Inventory.mainSlotCount - 1) none
  let full := step { ready with inventory } { mine := some plot }
  check (full.world.tile plot == .sunleaf && full.inventory == inventory)
    "A harvest that could only fit its seeds partially succeeded."
  let reap := step ready { mine := some plot }
  check (reap.world.tile plot == .air && Inventory.count reap.inventory .sunleaf == 2 &&
    Inventory.count reap.inventory .sunleafSeed == 3) "The grown plant did not yield a leaf and two seeds."
  return reap

private def brewBarkskin (harvest : GameState) : IO GameState := do
  let bench ← walkToColumn harvest 15 180
  let brew := step bench { craftBarkskin := true }
  check (Inventory.count brew.inventory .barkskinTonic == 1 &&
    Inventory.count brew.inventory .pondPerch == 0 && Inventory.count brew.inventory .sunleaf == 1)
    s!"Could not brew Barkskin from a caught perch and harvested Sunleaf. {brew.status}"
  let selected ← equip brew .barkskinTonic
  let shielded := step selected { mine := some selected.player.centerTile }
  check (shielded.barkskin == barkskinTicks && shielded.defense == selected.defense + 1 &&
    Inventory.count shielded.inventory .barkskinTonic == 0) "Barkskin did not consume the tonic and raise defense."
  saveRoundTrip shielded
  let some habitat := shielded.ecology.habitats[2]? | throw (IO.userError "Missing quarry habitat.")
  let some body := findStandingNear shielded.world habitat.cache.x (habitat.cache.y + 1) 3 3
      (habitat.contains ·.centerTile)
    | throw (IO.userError "No standing position for the quarry damage check.")
  let resident : CaveResident := { body, kind := .skitter, health := CaveMobKind.skitter.maxHealth, hopIn := 0 }
  let attack := { shielded with
    player := body
    combat := { shielded.combat with health := maxHealth, invulnerable := 0 }
    ecology := { shielded.ecology with sites := shielded.ecology.sites.set! 2 (.alive resident) } }
  let bare := step { attack with barkskin := 0 } {}
  let guarded := step attack {}
  check (bare.combat.health + healthPerHeart == guarded.combat.health && guarded.combat.health < maxHealth)
    "Barkskin did not reduce the actual quarry resident's contact damage."
  let last ← walk shielded (barkskinTicks - 1) 0
  let expired := step last {}
  check (last.barkskin == 1 && expired.barkskin == 0 && expired.defense == selected.defense)
    "Barkskin did not expire at its timer boundary."
  return brew

private def brewGills (caught : GameState) : IO Unit := do
  let site : TilePos := ⟨218, 73⟩
  let bank ← if caught.player.overlapsTile site then walk caught 3 1 else pure caught
  let bench ← buildWith bank .workbench site .workbench
  let brew := step bench { craftGills := true }
  check (Inventory.count brew.inventory .gillsElixir == 1 &&
    Inventory.count brew.inventory .caveEel == 0 && Inventory.count brew.inventory .sunleaf == 0)
    s!"Could not brew Gills from a caught eel and harvested Sunleaf. {brew.status}"
  let selected ← equip brew .gillsElixir
  let dosed := step selected { mine := some selected.player.centerTile }
  check (dosed.gills == gillsTicks && Inventory.count dosed.inventory .gillsElixir == 0)
    "The Gills Elixir did not start its full timer."
  let deep ← swimEastTo dosed 223 100
  let submerged ← walk deep 48 0
  check (submerged.water.submerged submerged.world (bodyCenterX submerged.player)
      (submerged.player.y + playerHeight / 6) && submerged.breath == maxBreath && submerged.gills > 0)
    "The Gills Elixir did not hold breath while genuinely submerged."
  saveRoundTrip submerged
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode submerged)).mapError IO.userError
  let last ← walk loaded (loaded.gills - 1) 0
  let expired := step last {}
  check (last.gills == 1 && last.breath == maxBreath && expired.gills == 0 && expired.breath == maxBreath - 1)
    "The reloaded Gills Elixir did not resume breath loss at expiry."

private def ruinLanding (initial : GameState) : IO GameState := do
  let shaft ← walkToColumn initial ruinShaftColumn 550
  check (shaft.player.centerTile.y ≤ ruinEntrance.y + 2) "The east passage missed the top of the ruin shaft."
  let mut state := shaft
  for _ in [:150] do
    if state.player.grounded && state.player.centerTile.y == (ruinShaftFloor : Int) - 1 then
      return state
    state := step state { dropThrough := true }
    check (!state.player.collides state.world) "Dropping through the ruin shaft crossed terrain."
  throw (IO.userError s!"The ruin shaft did not reach its landing: {repr state.player.centerTile}.")

private def ruinMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let baseline := generatePreRuinsTerrain defaultSeed
  let historical := { fresh with
    world := baseline
    water := Water.initial baseline defaultSeed
    chart := (Chart.empty baseline).reveal baseline fresh.player.centerTile }
  let archive ← legacyJson historical 14 24
  let installed ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot installed fresh) "An untouched v14 save did not receive the ruins."
  let edited := baseline.setTile ruinEntrance .platform
  let archive ← legacyJson { historical with world := edited } 14 24
  let preserved ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  let expected := (originalLifeCrystalSites defaultSeed).foldl (fun world site =>
    if ruinFootprint.contains site.position then world else world.setTile site.position .lifeCrystal) edited
  let extended := (deepsteelMine defaultSeed).edits.foldl (fun world (position, tile) =>
    world.setTile position tile) (extendHighlands defaultSeed expected)
  let expanded := (highlandCavern defaultSeed).edits.foldl
    (fun world (position, tile) => world.setTile position tile) extended
  let expanded := (magmaGallery defaultSeed).edits.foldl
    (fun world (position, tile) => world.setTile position tile) expanded
  check (sameHistoricalSnapshot preserved { fresh with world := expanded })
    "Retrofitting the ruins overwrote old construction or exposed a partial route."
  saveRoundTrip preserved

private def lifeCrystalMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let baseline := generatePreLifeCrystalTerrain defaultSeed
  let sites := originalLifeCrystalSites defaultSeed
  let first := sites[0]!.position
  let edited := baseline.setTile first .platform
  let chart := (Chart.empty edited).reveal edited fresh.player.centerTile
  let archive ← legacyJson { fresh with world := edited, chart, water := Water.initial edited defaultSeed }
    17 24
  let migrated ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  let expected := fresh.world.setTile first .platform
  check (sameHistoricalSnapshot migrated { fresh with world := expected } && migrated.combat.healthLimit == maxHealth)
    "Migrating one protected crystal site prevented other sites or changed prior exploration."
  saveRoundTrip migrated

private def deepsteelMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let mine := deepsteelMine defaultSeed
  let baseline := generatePreDeepsteelTerrain defaultSeed
  let historical := { fresh with world := baseline, water := Water.initial baseline defaultSeed }
  let archive ← legacyJson historical 19 24
  let installed ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot installed fresh && installed.world.tile mine.marker == .mineMarker)
    "An untouched v2.0 archive did not receive the Deepsteel Mine."
  let edited := baseline.setTile mine.caps[0]! .platform
  let archive ← legacyJson { historical with world := edited } 19 24
  let migrated ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot migrated { historical with world := edited, magma := Water.empty edited } &&
    migrated.ecology.sites[3]? == some (.waiting 0))
    "The mine migration overwrote a player platform or created an encounter in a skipped site."
  saveRoundTrip migrated

private def cavernMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let baseline := generatePreCavernTerrain defaultSeed
  let historical := { fresh with world := baseline, water := Water.initial baseline defaultSeed }
  let archive ← legacyJson historical 20 24
  let installed ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot installed fresh && installed.world.tile cavernAltar == .cavernAltar)
    "An untouched v2.1 archive did not receive the Deepstone Cavern."
  let entrance := (highlandCavern defaultSeed).caps[0]!
  let edited := baseline.setTile entrance .platform
  let archive ← legacyJson { historical with world := edited } 20 24
  let migrated ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot migrated { historical with world := edited, magma := Water.empty edited } &&
    migrated.world.tile cavernAltar != .cavernAltar)
    "The cavern migration changed the protected entrance or installed a partial site."
  saveRoundTrip migrated

private def magmaMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let baseline := generatePreMagmaTerrain defaultSeed
  let historical := { fresh with world := baseline, water := Water.initial baseline defaultSeed }
  let archive ← legacyJson historical 21 24
  let installed ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot installed fresh && installed.world.tile magmaMarker == .magmaMarker)
    "A pristine v2.2 archive did not receive the gallery and both liquids."
  let cap := magmaEntranceCaps[0]!
  let edited := baseline.setTile cap .platform
  let archive ← legacyJson { historical with world := edited } 21 24
  let preserved ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (sameHistoricalSnapshot preserved { historical with world := edited, magma := Water.empty edited })
    "Installing the gallery partially flooded a protected v2.2 site."
  saveRoundTrip preserved

private def highlandsMigrationChecks : IO Unit := do
  let fresh := newLegacyGame defaultSeed
  let edit : TilePos := ⟨382, 21⟩
  let seam : TilePos := ⟨383, 20⟩
  let old := (generatePreHighlandsTerrain defaultSeed).setTile edit .stone
  let chart := ((Chart.empty old).reveal old fresh.player.centerTile).reveal old ⟨382, 20⟩
  let water := Water.initial old defaultSeed
  let archive ← legacyJson { fresh with world := old, chart, water } 18 24
  let migrated ← IO.ofExcept <| (Save.decode archive.compress).mapError IO.userError
  check (migrated.world.width == 512 && migrated.world.height == old.height &&
    migrated.world.tile edit == .stone && migrated.world.tile seam == .air &&
    migrated.chart.tile? migrated.world seam == some .bedrock &&
    migrated.chart.tile? migrated.world ⟨384, 20⟩ == none && migrated.player == fresh.player)
    "Expanding the old east edge changed a build or revealed the new region."
  for row in [:old.height] do
    for col in [:old.width] do
      let position : TilePos := ⟨col, row⟩
      check (migrated.chart.tile? migrated.world position == chart.tile? old position &&
        migrated.water.level migrated.world position == water.level old position)
        s!"The width change shifted an old map or water cell at {repr position}."
  let reservoir := (magmaGallery defaultSeed).water.foldl
    (fun amount cell => amount + Water.fromEighths cell.2) 0
  check (waterVolume migrated == waterVolume { fresh with world := old, water } + reservoir)
    "Expanding the old world changed water outside the new reservoir."
  saveRoundTrip migrated

private def summonSentinel (initial : GameState) : IO GameState := do
  let altar ← walkToColumn initial (ruinAltar.x - 2) 190
  let summoned := step altar { interact := some ruinAltar }
  check (match summoned.ruin with | .fighting boss => boss.phase == .windup | _ => false)
    s!"Could not summon the Ruin Sentinel at {repr altar.player.centerTile}. {summoned.status}"
  return summoned

private def dodgeFirstRuinWave (initial : GameState) : IO GameState := do
  let mut state := initial
  for _ in [:160] do
    let .fighting boss := state.ruin | break
    if boss.phase == .slam then
      let jumped := step state { jump := true }
      check (!jumped.player.grounded && jumped.combat.health == state.combat.health &&
        jumped.projectiles.any (fun shot => shot.kind == .ruinWave && shot.vx < 0) &&
        jumped.projectiles.any (fun shot => shot.kind == .ruinWave && shot.vx > 0))
        "A jump on the impact tick did not clear both ground waves."
      return jumped
    state := step state {}
  throw (IO.userError "The Sentinel never telegraphed its first slam.")

private def winRuinDuel (initial : GameState) : IO GameState := do
  let mut state ← equip initial .silverSword
  for tick in [:1300] do
    let .fighting boss := state.ruin | break
    let move := headToward state (boss.body.centerTile.x + 1)
    let incoming := state.projectiles.any fun shot =>
      let dx := bodyCenterX state.player - shot.x
      shot.kind == .ruinWave && dx * shot.vx ≥ 0 && dx.natAbs ≤ 3 * shot.vx.natAbs
    let impact := boss.phase == .slam && boss.ticks == boss.phaseDuration &&
      (bodyCenterX state.player - boss.targetX).natAbs ≤ 3 * ShotKind.ruinWave.speed.toNat
    let canStrike := boss.phase == .recover && state.combat.swing ≤ 1 &&
      swordReaches state.world state.player boss.body (if boss.body.x < state.player.x then -1 else 1) true
    let input := { move with
      jump := state.player.grounded && (move.jump || incoming || impact)
      mine := if canStrike then some boss.body.centerTile else none }
    let next := step state input
    check (next.ruin != .ready && !next.player.collides next.world)
      s!"Ruin duel reset at tick {tick}, HP {state.combat.health}, position {repr state.player.centerTile}. {next.status}"
    state := next
  let .reward _ := state.ruin | throw (IO.userError s!"The silver sword did not defeat the Sentinel. {repr state.ruin}")
  return state

private def ruinReturn (initial : GameState) : IO GameState := do
  let mut state ← walkToColumn initial ruinShaftColumn 480
  for _ in [:620] do
    if state.player.grounded && state.player.centerTile.y == ruinEntrance.y + 2 then
      let exit ← walk state 28 (-1) true
      return ← walkToColumn exit (stormArenaEnd - 3 : Nat) 220
    state := step state { jump := state.player.grounded }
    check (!state.player.collides state.world) "Climbing the ruin shaft crossed terrain."
  throw (IO.userError s!"Could not climb back to the Storm Vault from {repr state.player.centerTile}.")

private def useExploredLifeCrystals (state : GameState) : IO GameState := do
  let sites := originalLifeCrystalSites state.seed
  check (Inventory.count state.inventory .lifeCrystal + state.combat.lifeCrystals == sites.size &&
    sites.all (fun site => state.world.tile site.position == .air))
    "The same world did not provide all six natural crystals before the Sentinel fight."
  let mut upgraded := state
  for _ in [:sites.size - state.combat.lifeCrystals] do
    upgraded ← useMinedHeart upgraded
  check (upgraded.combat.healthLimit == 220 && Inventory.count upgraded.inventory .lifeCrystal == 0)
    "The six original crystals did not all apply to the Classic life cap."
  saveRoundTrip upgraded
  let some (inventory, _) := Inventory.addOne upgraded.inventory .draught
    | throw (IO.userError "No room to verify healing above the initial life.")
  let wounded ← equip { upgraded with inventory, combat := { upgraded.combat with health := 213 } } .draught
  let healed := step wounded { mine := some wounded.player.centerTile }
  check (healed.combat.health == healed.combat.healthLimit && healed.combat.health == 220)
    "A healing draught did not clamp at the upgraded health limit."
  return upgraded

private def ruinRoute (initial : GameState) : IO GameState := do
  let distant := step initial { interact := some ruinAltar }
  check (distant.ruin == .ready) "The ruins could be summoned from the Storm Vault."
  let landing ← ruinLanding distant
  let passage ← walkToColumn landing ruinCheckpointColumn 150
  let crystal ← mineLifeCrystalAt passage (lifeCrystalSites passage.seed)[5]!.position
  let prepared ← useExploredLifeCrystals crystal
  let started ← summonSentinel prepared
  let inFlight ← dodgeFirstRuinWave started
  saveRoundTrip inFlight
  let escaped ← walkToColumn inFlight ruinCheckpointColumn 185
  check (escaped.ruin == .ready && !escaped.projectiles.any (·.kind == .ruinWave))
    "Leaving the arena did not reset the Sentinel and clear its waves."
  let exposed ← summonSentinel escaped
  let mut fallen := exposed
  for _ in [:700] do
    if fallen.ruin == .ready then break
    fallen := step fallen {}
  let respawned ← waitForRespawn fallen
  check (respawned.ruin == .ready && respawned.combat.lifeCrystals == 6 &&
    respawned.player.centerTile.x < 32 && respawned.inventory == exposed.inventory &&
    !respawned.projectiles.any (·.kind == .ruinWave)) "Ruin shockwaves did not cause a world respawn."
  let retry ← summonSentinel escaped
  let inFlight ← dodgeFirstRuinWave retry
  let restored ← IO.ofExcept <| (Save.decode (Save.encode inFlight)).mapError IO.userError
  let won ← winRuinDuel inFlight
  check (sameSnapshot won (← winRuinDuel restored)) "Loading during the shockwave changed the Sentinel replay."
  saveRoundTrip won
  let .reward drop := won.ruin | throw (IO.userError "The Sentinel left no hammer.")
  let fullInventory := won.inventory.map fun slot => some (slot.getD ⟨.stone, Inventory.maxStack⟩)
  let full ← walkToColumn { won with inventory := fullInventory } drop.x 100
  check (full.ruin == .reward drop && full.coins == won.coins && full.inventory == fullInventory)
    "Collecting the Sentinel reward with a full inventory partially succeeded."
  let reached ← walkToColumn won drop.x 100
  let claimed := step reached {}
  let altar ← walkToColumn claimed ruinAltar.x 180
  let revisited := step altar { interact := some ruinAltar }
  check (claimed.ruin == .cleared && revisited.ruin == .cleared &&
    Inventory.count revisited.inventory .ruinHammer == 1 && revisited.coins == won.coins + 16 &&
    Item.ruinHammer.swordDamage == 5)
    s!"The Sentinel reward was missing, duplicated or replayable. {claimed.status}"
  let returned ← ruinReturn revisited
  check (inStormArena returned.player.centerTile) "Leaving the ruins did not reconnect to the Storm Vault."
  return returned

private def traversalKit (frost : GameState) : IO GameState := do
  let stocked ← exploreSupplyPocket frost 0 .ironSword [(.torch, 6), (.wood, 4)] true
  let patched ← minePocketHeart stocked 0
  let terrace := (lifeCrystalSites frost.seed)[3]!.position
  let there ← walkToColumn patched terrace.x 360
  let crystal ← mineLifeCrystalAt there terrace
  let seam ← walkToColumn crystal 114 1700
  let mut mined ← equip seam .crystalPick
  for position in [⟨115, 62⟩, ⟨115, 63⟩, ⟨116, 62⟩, ⟨116, 63⟩, ⟨117, 62⟩, ⟨117, 63⟩,
      ⟨113, 63⟩, ⟨112, 61⟩] do
    mined := step mined { mine := some position }
    check (mined.world.tile position == .air) s!"Could not mine traversal supplies at {repr position}. {mined.status}"
  check (Inventory.count mined.inventory .iron == railIronCost + minecartIronCost + grapplingHookIronCost &&
    Inventory.count mined.inventory .ice == 2) "Iron ore and grading blocks were not collected."
  let away := step mined { craftRails := true, craftMinecart := true, craftGrapplingHook := true }
  check (away.inventory == mined.inventory) "Traversal equipment could be crafted away from the workbench."
  let bench ← walkToColumn mined 93 350
  let tracks := step bench { craftRails := true }
  let cart := step tracks { craftMinecart := true }
  check (Inventory.count cart.inventory .rail == railBatch && Inventory.count cart.inventory .minecart == 1 &&
    Inventory.count cart.inventory .iron == grapplingHookIronCost &&
    Inventory.count cart.inventory .wood + railWoodCost + minecartWoodCost == Inventory.count bench.inventory .wood)
    s!"Could not craft the track and minecart from natural materials at {repr bench.player.centerTile}. {cart.status}"
  let hook := step cart { craftGrapplingHook := true }
  check (Inventory.count hook.inventory .grapplingHook == 1 && Inventory.count hook.inventory .iron == 0 &&
    Inventory.count hook.inventory .wood + grapplingHookWoodCost == Inventory.count cart.inventory .wood)
    s!"The grappling hook recipe did not consume the mined iron and wood. {hook.status}"
  return hook

private def stoppedCart (state : GameState) (position : TilePos) : Bool :=
  state.ride.any fun ride => ride.rail == position && ride.next.isNone && ride.speed == 0

private def railRoute (frost : GameState) : IO GameState := do
  let kit ← traversalKit frost
  let high : TilePos := ⟨102, 57⟩
  let gap : TilePos := ⟨109, 59⟩
  let low : TilePos := ⟨112, 59⟩
  let start ← walkToColumn kit high.x 230
  let selected ← equip start .rail
  let floating := step selected { place := some ⟨103, 54⟩ }
  check (floating.world.tile ⟨103, 54⟩ == .rail &&
    Inventory.count floating.inventory .rail + 1 == Inventory.count selected.inventory .rail)
    "An empty air cell could not accept a rail."
  let mut line := floating
  for column in [102:107] do
    line ← walkToColumn line column 130
    line ← buildWith line .rail ⟨column, 57⟩ .rail
  line ← buildWith line .ice ⟨107, 59⟩ .ice
  line ← buildWith line .rail ⟨107, 58⟩ .rail
  for column in [108:113] do
    line ← walkToColumn line column 130
    if column != gap.x then line ← buildWith line .rail ⟨column, 59⟩ .rail
  let station ← walkToColumn line high.x 240
  let cart ← equip station .minecart
  let rejected := step cart { place := some ⟨high.x, high.y - 1⟩ }
  check (rejected.inventory == cart.inventory && rejected.carts.isEmpty) "Deploying away from rail consumed the cart."
  let parked := step cart { place := some high }
  check (parked.carts == #[high] && Inventory.count parked.inventory .minecart == 0)
    s!"Could not deploy a cart on the west station. {parked.status}"
  let boarded := step parked { interact := some ⟨high.x, high.y - 1⟩ }
  check (stoppedCart boarded high && boarded.carts.isEmpty) s!"Could not board the parked cart. {boarded.status}"
  let hook ← equip boarded .grapplingHook
  let refused := step hook { mine := some ⟨high.x, high.y - 7⟩ }
  check (stoppedCart refused high && refused.grapple == .idle && refused.inventory == hook.inventory)
    "Casting a hook from a minecart changed the ride or deployed a rope."
  let mut moving := refused
  for _ in [:60] do
    if moving.ride.any (fun ride => ride.next.isSome && ride.speed > maxHorizontalSpeed) then break
    moving := step moving { horizontal := 1 }
  check (moving.ride.any (fun ride => ride.next.isSome && ride.speed > maxHorizontalSpeed))
    "The cart failed to accelerate above walking speed on intact rails."
  saveRoundTrip moving
  let resumed ← IO.ofExcept <| (Save.decode (Save.encode moving)).mapError IO.userError
  let halted ← walk moving 120 1
  check (stoppedCart halted ⟨108, 59⟩ && sameSnapshot halted (← walk resumed 120 1))
    "A moving save changed the downhill replay, or the cart crossed the rail gap."
  let pick ← equip halted .crystalPick
  let support := step pick { mine := some ⟨108, 60⟩ }
  check (support.world.tile ⟨108, 60⟩ == .air && stoppedCart support ⟨108, 59⟩)
    "Removing terrain beneath the rail invalidated the occupied track."
  saveRoundTrip support
  let repaired ← buildWith support .rail gap .rail
  let wall : TilePos := ⟨110, 58⟩
  let blocked ← buildWith repaired .ice wall .ice
  let reached ← walk blocked 120 1
  check (stoppedCart reached gap) "The cart crossed a wall in the rider's path."
  let pick ← equip reached .crystalPick
  let opened := step pick { mine := some wall }
  let far ← walk opened 120 1
  check (stoppedCart far low && far.world.tile wall == .air) "The repaired line could not reach the far station."
  let back ← walk far 120 (-1)
  check (stoppedCart back high) "The cart could not climb the same slope on the return trip."
  let parked := step back { dismountCart := true }
  check (parked.ride.isNone && parked.carts == #[high] && parked.player.grounded)
    "Dismount did not leave a recoverable cart at the station."
  let pick ← equip parked .crystalPick
  let recovered := step pick { mine := some high }
  check (recovered.carts.isEmpty && recovered.world.tile high == .rail &&
    Inventory.count recovered.inventory .minecart == 1) "Recovering the cart removed the rail or lost the item."
  return ← equip parked .ironSword

private def waitForHook (initial : GameState) : IO GameState := do
  let mut state := initial
  for _ in [:hookFlightTicks + 1] do
    match state.grapple with
    | .latched _ => return state
    | .flying _ => state := step state {}
    | .idle => break
  throw (IO.userError s!"The grappling hook did not attach from {repr state.player.centerTile}. {state.status}")

private def reelFor (initial : GameState) (ticks : Nat) : IO GameState := do
  let mut state := initial
  for _ in [:ticks] do
    state := step state { reelHook := true }
    check (!state.player.collides state.world && (match state.grapple with | .latched _ => true | _ => false))
      s!"Reeling lost the anchor or crossed terrain at {repr state.player.centerTile}. {state.status}"
  return state

private def grappleShaftRoute (initial : GameState) : IO GameState := do
  let landing ← ruinLanding initial
  let stopper : TilePos := ⟨ruinShaftColumn, (ruinShaftFloor - 5 : Nat)⟩
  let roof : TilePos := ⟨ruinShaftColumn, 55⟩
  let blocked ← buildWith landing .ice stopper .ice
  let hook ← equip blocked .grapplingHook
  let flying := step hook { mine := some roof }
  check (match flying.grapple with | .flying _ => true | _ => false) "The hook did not launch into the shaft."
  saveRoundTrip flying
  let restoredFlight ← IO.ofExcept <| (Save.decode (Save.encode flying)).mapError IO.userError
  let attached ← waitForHook flying
  check (sameSnapshot attached (← waitForHook restoredFlight)) "A flying save changed the hook's first contact."
  let .latched anchor := attached.grapple | throw (IO.userError "The shaft obstacle caught no hook.")
  check (anchor.tile == stopper && (step attached {}).player == attached.player)
    "The hook passed through the obstruction or would not hold position."
  let onRope ← reelFor attached 4
  saveRoundTrip onRope
  let restored ← IO.ofExcept <| (Save.decode (Save.encode onRope)).mapError IO.userError
  let near ← reelFor onRope 17
  check (sameSnapshot near (← reelFor restored 17) && near.player.y + tileUnits < attached.player.y)
    "Loading a latched hook changed the pull, or the hook did not raise the player."
  let pick ← equip near .crystalPick
  let cut := step pick { mine := some stopper }
  check (cut.grapple == .idle && cut.world.tile stopper == .air && (step cut {}).player.y > cut.player.y)
    "Mining an attached block did not release the rope and resume falling."

  let mut state ← equip landing .grapplingHook
  for target in ([⟨383, 85⟩, ⟨374, 77⟩, ⟨383, 68⟩, ⟨383, 59⟩, ⟨377, 55⟩] : List TilePos) do
    check (bodyCenterY state.player - (target.y * tileUnits + tileUnits / 2) ≤ 21 * tileUnits / 2)
      s!"The shaft aim at {repr target} from {repr state.player.centerTile} was above the playable desktop viewport."
    let fired := step state { mine := some target }
    let latched ← try waitForHook fired catch error =>
      throw (IO.userError s!"Shaft cast at {repr target} from {repr state.player.centerTile}: {error}")
    let lifted ← reelFor latched 68
    check (lifted.player.y + 3 * tileUnits ≤ state.player.y)
      s!"The shaft cast toward {repr target} gained too little height from {repr state.player.centerTile} to {repr lifted.player.centerTile}."
    state := lifted
  check (state.player.centerTile.y < ruinEntrance.y && state.player.y + 30 * tileUnits < landing.player.y)
    "Chained grapples did not reach the upper passage of the natural ruin shaft."
  let released := step state { jump := true, horizontal := -1 }
  check (released.grapple == .idle) "Space did not release the hook."
  let returned ← walkToColumn released (stormArenaEnd - 3 : Nat) 330
  check (inStormArena returned.player.centerTile && Inventory.count returned.inventory .grapplingHook == 1)
    "The player did not leave the shaft with the grappling hook."
  return returned

private def waitForSurfaceKind (initial : GameState) (kind : SurfaceKind) : IO GameState := do
  let mut state := initial
  for _ in [:610] do
    if state.surface.creature.any (·.kind == kind) then return state
    state := step state {}
  throw (IO.userError s!"No {kind.name} appeared near {repr state.player.centerTile}.")

private def harvestCedar (initial : GameState) (index := 1) (logs := 6) : IO GameState := do
  let cedar := (highlandCedarColumns initial.seed)[index]!
  let floor := groundHeight initial.seed cedar
  let crown := highlandCedarCrownFloor initial.seed cedar
  let stump : TilePos := ⟨cedar, floor - 1⟩
  let nearby ← walkToColumn initial (cedar - 1 : Nat) 550
  let pick ← equip nearby .basicPick
  let mut state := step pick { mine := some stump }
  check (state.world.tile stump == .air) s!"The cedar stump did not yield wood. {state.status}"
  for column in [cedar + 4 - logs:cedar + 4] do
    let log : TilePos := ⟨column, crown - 6⟩
    state ← walkToColumn state column 100
    if !state.canReach log then
      state := step state { jump := true }
      for _ in [:15] do
        if state.canReach log then break
        state := step state {}
    check (state.world.tile log == .timber && state.canReach log)
      s!"The cedar log at {repr log} was unreachable from {repr state.player.centerTile}."
    state := step state { mine := some log }
    check (state.world.tile log == .air) "A cedar log was not harvested."
  check (Inventory.count state.inventory .wood == Inventory.count initial.inventory .wood + logs + 1)
    "The cedar did not supply the expected wood."
  return state

private def buildHighlandCamp (initial : GameState) : IO (GameState × TilePos) := do
  let floor := groundHeight initial.seed 417
  let mut state ← equip initial .basicPick
  for column in [424:429] do
    state ← walkToColumn state column 550
    for row in [floor + 1:floor + 4] do
      let position : TilePos := ⟨column, row⟩
      check (state.world.tile position == .dirt) "The camp material was missing from the glade."
      state := step state { mine := some position }
      check (state.world.tile position == .air) "A camp block could not be mined."
  state ← walkToColumn state 415 170
  for row in [floor - 4:floor] do
    state ← buildWallTile state ⟨414, row⟩
  state ← walkToColumn state 419 125
  for row in [floor - 4:floor - 2] do
    state ← buildWith state .dirt ⟨420, row⟩ .dirt
  for column in [414:421] do
    state ← walkToColumn state (max 415 (min 419 column) : Nat) 120
    state ← buildWith state .dirt ⟨column, floor - 5⟩ .dirt
  state ← walkToColumn state 416 120
  state ← buildWith (step state { craftWorkbench := true }) .workbench ⟨418, floor - 1⟩ .workbench
  let door : TilePos := ⟨420, floor - 1⟩
  state ← buildWith (step state { craftDoor := true }) .door door .doorClosed
  state ← buildWith state .torch ⟨415, floor - 3⟩ .torch
  check ((inspectHome state.world door).isOk && state.guide == initial.guide && state.merchant == initial.merchant)
    "The Highland camp was not enclosed or interfered with the old village."
  return (state, door)

private def highlandExpedition (initial : GameState) : IO GameState := do
  let dawn ← walk initial ((cycleTicks - initial.surface.clock) % cycleTicks) 0
  let passage ← walkToColumn dawn 384 350
  check (inHighlands passage.player.centerTile && passage.player.centerTile.y ≥ 60)
    "The old Storm Vault did not connect to the Highlands staircase."
  let emerged ← walkToColumn passage 410 950
  check (emerged.world.clearSkyAbove emerged.player.centerTile &&
    emerged.player.centerTile.y == (groundHeight emerged.seed 410 : Int) - 1)
    "The Highland staircase did not lead to the surface."
  let west ← walkToColumn emerged 382 950
  check (!inHighlands west.player.centerTile) "The Highlands could not be left on foot."
  let field ← walkToColumn west 410 950
  let hare ← waitForSurfaceKind field .cedarHare
  check (!isNight hare.surface.clock && hare.combat.health ≥ initial.combat.health)
    "Highland daylight fauna did not appear safely."
  saveRoundTrip hare

  let wood ← harvestCedar hare
  let (built, door) ← buildHighlandCamp wood
  let openDoor : TickInput := { interact := some door }
  let some berry := (highlandBerryPositions built.seed).find? (fun position => 434 ≤ position.x && position.x ≤ 438)
    | throw (IO.userError "The camp's Highland Berry patch did not generate.")
  let patch ← walkToColumn (step built openDoor) berry.x 360
  let pick ← equip patch .basicPick
  let gathered := step pick { mine := some berry }
  check (gathered.world.tile berry == .air && gathered.chart.tile? gathered.world berry == some .air &&
    Inventory.count gathered.inventory .highlandBerry == 1) "Could not forage the Highlands berry patch."
  let returned ← walkToColumn gathered 416 360
  let camp := step returned openDoor
  check (camp.world.tile door == .doorClosed) "Returning to camp did not close the door."

  let dusk ← walk camp ((cycleTicks + dayTicks - camp.surface.clock) % cycleTicks) 0
  let crawler ← waitForSurfaceKind dusk .brambleCrawler
  let sheltered ← walk crawler 185 0
  check (isNight sheltered.surface.clock && sheltered.combat.health ≥ camp.combat.health &&
    sheltered.surface.creature.any (·.kind == .brambleCrawler))
    "The camp did not shelter its occupant from a Highland predator."
  saveRoundTrip sheltered
  let restored ← IO.ofExcept <| (Save.decode (Save.encode sheltered)).mapError IO.userError
  let opened := step sheltered openDoor
  check (sameSnapshot opened (step restored openDoor) && opened.world.tile door == .doorOpen)
    "Reloading a living Highland predator changed the door encounter."
  let mut hurt := opened
  for _ in [:260] do
    if hurt.combat.health < opened.combat.health then break
    hurt := step hurt {}
  check (hurt.combat.health + SurfaceKind.brambleCrawler.contactDamage * healthPerHeart == opened.combat.health)
    "Opening the camp did not expose the player to the Bramble Crawler."
  let berry ← equip hurt .highlandBerry
  let healed := step berry { mine := some berry.player.centerTile }
  check (healed.combat.health == opened.combat.health && Inventory.count healed.inventory .highlandBerry == 0)
    "Foraged berries did not heal the actual Highland encounter damage."
  let hammer ← equip healed .ruinHammer
  let some enemy := hammer.surface.creature | throw (IO.userError "The Bramble Crawler disappeared before combat.")
  let won := step hammer { mine := some enemy.body.centerTile }
  check (won.surface.creature.isNone && won.coins == hammer.coins + SurfaceKind.brambleCrawler.rewardCoins)
    "Defeating the Highland predator did not resolve the encounter and reward."
  let closed := step won openDoor
  check (closed.world.tile door == .doorClosed && (inspectHome closed.world door).isOk)
    "Closing the Highland camp after the encounter failed."
  return closed

private def stockDeepsteelCamp (initial : GameState) : IO GameState := do
  let floor := groundHeight initial.seed 417
  let door : TilePos := ⟨420, floor - 1⟩
  let dawn ← walk initial ((cycleTicks - initial.surface.clock) % cycleTicks) 0
  let outside := step dawn { interact := some door }
  check (outside.world.tile door == .doorOpen) "The expedition could not open the Highland camp."
  let supplied ← harvestCedar outside 2 7
  let inside ← walkToColumn supplied 416 560
  let chest : TilePos := ⟨415, floor - 1⟩
  let built ← buildWith (step inside { craftChest := true }) .chest chest .chest
  let mut state := step built { interact := some chest }
  check (state.openChest == some chest) "The expedition chest did not open."
  for item in [Item.stoneSword, .copperPick, .ironSword, .watchhorn] do
    let slot ← slotFor state item
    state := step state { chestDeposit := some slot }
    check (Inventory.count state.inventory item == 0 &&
      state.chests.any (fun chest => Inventory.count chest.slots item == 1))
      s!"The camp chest did not store {item.name}."
  return step state { closeChest := true }

private def openDeepsteelTrail (state : GameState) : IO (GameState × TilePos) := do
  let mine := deepsteelMine state.seed
  let first := mine.caps[0]!
  let lip ← walkToColumn state (first.x - 1) 600
  let mut pick ← equip lip .basicPick
  for cap in mine.caps do
    pick := step pick { mine := some cap }
    check (pick.world.tile cap == .air) s!"Could not open the marked mine cap at {repr cap}. {pick.status}"
  let deck : TilePos := ⟨first.x, first.y + 1⟩
  let built ← buildWith (step pick { craftPlatforms := true }) .platform deck .platform
  return (built, deck)

private def descendDeepsteelDeck (initial : GameState) (deck : TilePos) : IO GameState := do
  let edge ← walkToColumn initial deck.x 180
  let onDeck ← walk edge 4 1
  check (onDeck.player.grounded && onDeck.player.y + playerHeight == deck.y * tileUnits)
    s!"The built platform did not support the mine entrance at {repr deck}."
  let mut dropped := onDeck
  for _ in [:38] do
    dropped := step dropped { dropThrough := true }
  check (dropped.player.y > onDeck.player.y && !dropped.player.collides dropped.world)
    "Down did not pass through the player-built mine platform."
  return dropped

private def mineDeepsteel (initial : GameState) (deck : TilePos) : IO GameState := do
  let mine := deepsteelMine initial.seed
  let dropped ← descendDeepsteelDeck initial deck
  let ledge ← walkToColumn dropped ((mine.roomStart : Int) - 5) 650
  check (ledge.ecology.sites[3]? == some (.waiting 0)) "The entrance vein is already guarded."
  let ore := mine.ore[0]!
  let basic ← equip ledge .basicPick
  let refused := step basic { mine := some ore }
  check (basic.canReach ore && refused.world.tile ore == .deepsteel && refused.inventory == basic.inventory)
    "A basic pick mined Deepsteel ore or the tool test was out of reach."
  let mut state ← equip refused .crystalPick
  for index in [:highlandCrossbowOreCost] do
    let position := mine.ore[index]!
    state := step state { mine := some position }
    check (state.world.tile position == .air && state.chart.tile? state.world position == some .air)
      s!"Could not mine the exposed ore at {repr position} from {repr state.player.centerTile}. {state.status}"
  check (Inventory.count state.inventory .deepsteel == highlandCrossbowOreCost) "The vein did not yield the recipe's ore."
  return state

private def crossbowOreback (initial : GameState) : IO GameState := do
  let mine := deepsteelMine initial.seed
  let entered ← walkToColumn initial (mine.roomStart + 1 : Nat) 1150
  let some (CaveSite.alive resident) := entered.ecology.sites[3]? | throw (IO.userError "The mine guard did not emerge.")
  check (resident.kind == .oreback && resident.health == CaveMobKind.oreback.maxHealth)
    "The new habitat spawned the wrong resident."
  let wall : TilePos := ⟨entered.player.centerTile.x + 2, entered.player.centerTile.y⟩
  let covered ← buildWith entered .dirt wall .dirt
  let blocked ← equip covered .highlandCrossbow
  let stopped ← walk (step blocked { mine := some resident.body.centerTile }) 12 0
  check (!stopped.projectiles.any (·.kind == .highlandBolt) &&
    (match stopped.ecology.sites[3]? with
      | some (CaveSite.alive foe) => foe.health == resident.health
      | _ => false))
    "The Crossbow shot passed through a player-built wall."

  let crossbow ← equip entered .highlandCrossbow
  let aim := resident.body.centerTile
  let fired := step crossbow { mine := some aim }
  let some shot := fired.projectiles.find? (·.kind == .highlandBolt)
    | throw (IO.userError "The Crossbow did not launch a bolt.")
  check (fired.bowCooldown == highlandCrossbowCooldown &&
    Inventory.count fired.inventory .arrow + 1 == Inventory.count crossbow.inventory .arrow)
    "The Crossbow did not consume one arrow and begin its full reload."
  let reloading := step fired { mine := some aim }
  check (Inventory.count reloading.inventory .arrow == Inventory.count fired.inventory .arrow &&
    reloading.projectiles.any (fun bolt => bolt.kind == .highlandBolt && bolt.vy == shot.vy))
    "The bolt curved or the Crossbow fired again during reload."
  saveRoundTrip reloading
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode reloading)).mapError IO.userError
  let hit ← walk reloading 19 0
  check (sameSnapshot hit (← walk loaded 19 0)) "Reopening a flying bolt changed the encounter replay."
  let some (CaveSite.alive injured) := hit.ecology.sites[3]? | throw (IO.userError "The first Crossbow hit did not leave a wounded guard.")
  check (injured.health + ShotKind.highlandBolt.damage == resident.health)
    "The straight bolt did not inflict the Crossbow's six damage."
  let ready ← walk hit hit.bowCooldown 0
  let some (CaveSite.alive wounded) := ready.ecology.sites[3]? | throw (IO.userError "The wounded guard disappeared before reloading.")
  let won ← walk (step ready { mine := some wounded.body.centerTile }) 22 0
  let some (CaveSite.loot drop) := won.ecology.sites[3]? | throw (IO.userError "A second Crossbow hit did not defeat Oreback.")
  saveRoundTrip won
  let approached ← walkToColumn won drop.x 190
  let collected := step approached {}
  check (Inventory.count collected.inventory .deepsteel == 1 &&
    collected.ecology.sites[3]?.any (fun site => match site with
      | CaveSite.waiting ticks => ticks > 0
      | _ => false))
    "Defeating Oreback did not award reusable ore and start its respawn delay."
  return collected

private def deepsteelExpedition (initial : GameState) : IO GameState := do
  let supplied ← stockDeepsteelCamp initial
  let (trail, deck) ← openDeepsteelTrail supplied
  let mined ← mineDeepsteel trail deck
  let refused := step mined { craftHighlandCrossbow := true }
  check (refused.inventory == mined.inventory) "The Crossbow could be forged away from a workbench."
  let mine := deepsteelMine initial.seed
  let west ← walkToColumn mined (mine.caps[0]!.x - 1) 850
  check (west.world.clearSkyAbove west.player.centerTile) "The mine staircase did not return to the surface."
  let bench ← walkToColumn west 416 560
  let arrows := step bench { craftArrows := true }
  let forged := step arrows { craftHighlandCrossbow := true }
  check (bench.nearWorkbench && Inventory.count arrows.inventory .arrow == Inventory.count bench.inventory .arrow + arrowBatch &&
    Inventory.count forged.inventory .highlandCrossbow == 1 && Inventory.count forged.inventory .deepsteel == 0 &&
    Inventory.count forged.inventory .wood + highlandCrossbowWoodCost == Inventory.count arrows.inventory .wood)
    s!"The locally supplied camp could not forge the Crossbow. {forged.status}"
  let onTrail ← walkToColumn forged (deck.x - 1) 600
  let dropped ← descendDeepsteelDeck onTrail deck
  let won ← crossbowOreback dropped
  let west ← walkToColumn won (deck.x - 1) 1150
  let returned ← walkToColumn west 416 560
  let door : TilePos := ⟨420, groundHeight initial.seed 417 - 1⟩
  let camp := step returned { interact := some door }
  check (camp.world.tile door == .doorClosed && camp.world.tile deck == .platform && (inspectHome camp.world door).isOk)
    "The built mine trail and old camp did not survive the second journey."
  return camp

private def descendCavern (camp : GameState) : IO GameState := do
  let door : TilePos := ⟨420, groundHeight camp.seed 417 - 1⟩
  let first := (deepsteelMine camp.seed).caps[0]!
  let deck : TilePos := ⟨first.x, first.y + 1⟩
  let trail ← walkToColumn (step camp { interact := some door }) (deck.x - 1) 600
  let dropped ← descendDeepsteelDeck trail deck
  let caps := (highlandCavern camp.seed).caps
  let lip ← walkToColumn dropped (caps[0]!.x - 1) 1150
  let mut opened ← equip lip .crystalPick
  for cap in caps do
    opened := step opened { mine := some cap }
    check (opened.world.tile cap == .air) s!"Could not excavate the cavern cap {repr cap}. {opened.status}"
  let mut state ← walkToColumn opened cavernShaftColumn 170
  for _ in [:180] do
    if state.player.grounded && state.player.centerTile.y == (cavernArenaFloor : Int) - 1 then
      return state
    state := step state { dropThrough := true }
    check (!state.player.collides state.world) "Dropping through the cavern platforms crossed terrain."
  throw (IO.userError s!"The cavern shaft did not reach the landing: {repr state.player.centerTile}.")

private def approachCavern (state : GameState) : IO GameState := do
  let altar ← walkToColumn state (cavernAltar.x - 2) 300
  check (altar.world.tile cavernAltar == .cavernAltar && altar.canReach cavernAltar &&
    inCavernArena altar.player.centerTile) s!"Could not reach the Deepstone Altar from {repr state.player.centerTile}."
  return altar

private def summonCavern (altar : GameState) : IO GameState := do
  let state := step altar { interact := some cavernAltar }
  check (match state.cavern with
    | .fighting boss => boss.health == cavernMaxHealth && boss.phase == .windup
    | _ => false) s!"Could not summon the Colossus from {repr altar.player.centerTile}. {state.status}"
  return state

private def lowCavernBeamNear (state : GameState) : Bool :=
  let x := bodyCenterX state.player
  state.player.grounded && state.projectiles.any fun shot =>
    shot.kind == .cavernBeam && shot.y == cavernLowBeamY &&
    shot.vx * (x - shot.x) > 0 && (x - shot.x).natAbs ≤ (3 * tileUnits).toNat

private def waitForCavernBeam (initial : GameState) : IO GameState := do
  let mut state := initial
  for _ in [:180] do
    if state.projectiles.any (·.kind == .cavernBeam) then return state
    state := step state {}
  throw (IO.userError "The Colossus never released its first beam.")

private def dodgeCavernCycle (initial : GameState) : IO GameState := do
  let mut state := initial
  let mut jumped := false
  let mut sawHigh := false
  for _ in [:190] do
    let .fighting boss := state.cavern | throw (IO.userError "The Colossus left during its sweep.")
    if boss.phase == .recover then
      check (jumped && sawHigh && state.player.grounded && state.combat.health == initial.combat.health)
        "Jumping the low beam and landing for the high beam did not avoid both attacks."
      return state
    let jump := lowCavernBeamNear state
    jumped := jumped || jump
    sawHigh := sawHigh || state.projectiles.any (fun shot =>
      shot.kind == .cavernBeam && shot.y == cavernHighBeamY)
    state := step state { jump }
  throw (IO.userError "The Colossus did not expose its core after the two beams.")

private def loseCavernFight (initial : GameState) : IO Unit := do
  let mut state := initial
  for _ in [:1250] do
    state := step state {}
    if state.cavern == .ready then
      let respawned ← waitForRespawn state
      check (respawned.player.centerTile.x < 32 &&
        respawned.inventory == initial.inventory && respawned.projectiles.isEmpty)
        s!"Beam defeat did not return to the world spawn. {respawned.status}"
      return
  throw (IO.userError "Standing in the low beam never caused a defeat.")

private def winCavernFight (initial : GameState) : IO GameState := do
  let oldBow ← equip initial .stormBow
  let .fighting exposed := oldBow.cavern | throw (IO.userError "The Colossus is missing.")
  check (exposed.phase == .recover) "The old bow was readied outside the recovery window."
  let target := exposed.body.centerTile
  let oldHit ← walk (step oldBow { mine := some ⟨target.x, target.y - 1⟩ }) 21 0
  let .fighting injured := oldHit.cavern | throw (IO.userError "The old bow unexpectedly ended the fight.")
  check (injured.health + ShotKind.stormArrow.damage == cavernMaxHealth)
    "The Storm Bow could not damage the exposed Colossus."
  let mut state ← equip oldHit .highlandCrossbow
  for _ in [:760] do
    let .fighting boss := state.cavern | break
    let target := if boss.phase == .recover && boss.ticks > 17 && state.bowCooldown ≤ 1 then
        some boss.body.centerTile else none
    state := step state { mine := target, jump := lowCavernBeamNear state }
  check ((match state.cavern with | .reward _ => true | _ => false) &&
    Inventory.count state.inventory .arrow + 5 == Inventory.count initial.inventory .arrow &&
    !state.projectiles.any (·.kind == .cavernBeam))
    s!"The old bow and four Crossbow bolts did not win the fight. {repr state.cavern} {state.status}"
  return state

private def returnFromCavern (state : GameState) : IO GameState := do
  let mut state ← walkToColumn state cavernShaftColumn 470
  let floor := deepsteelFloor state.seed
  for _ in [:820] do
    if state.player.y + playerHeight ≤ (floor : Int) * tileUnits - 18 then
      let west ← walkToColumn state ((highlandCavern state.seed).caps[0]!.x - 2) 125
      check (west.player.centerTile.y < (floor : Int)) "The final jump did not reach the mine floor."
      let deck := (deepsteelMine state.seed).caps[0]!
      let daylight ← walkToColumn west (deck.x - 1) 1150
      let camp ← walkToColumn daylight 416 600
      let door : TilePos := ⟨420, groundHeight state.seed 417 - 1⟩
      let camp := step camp { interact := some door }
      check (camp.world.tile door == .doorClosed && (inspectHome camp.world door).isOk &&
        camp.equipment.armor == some .deepstoneAegis && camp.cavern == .cleared)
        "The equipped reward or camp was lost on the return route."
      return camp
    state := step state { jump := state.player.grounded }
  throw (IO.userError s!"Could not climb back to the mine: {repr state.player.centerTile}.")

private def cavernExpedition (camp : GameState) : IO GameState := do
  check ((step camp { interact := some cavernAltar }).cavern == .ready)
    "The cavern altar responded from the camp."
  let altar ← approachCavern (← descendCavern camp)
  let bow ← equip altar .highlandCrossbow
  let started ← summonCavern bow
  let .fighting shielded := started.cavern | throw (IO.userError "The Colossus is missing.")
  let blocked ← walk (step started { mine := some shielded.body.centerTile }) 21 0
  check (blocked.status.startsWith "The Colossus deflected" &&
    (match blocked.cavern with | .fighting boss => boss.health == cavernMaxHealth | _ => false))
    "A Crossbow bolt penetrated the closed shell."
  let beam ← waitForCavernBeam started
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode beam)).mapError IO.userError
  check (sameSnapshot beam loaded) "The active low beam and phase did not round trip."
  let exposed ← dodgeCavernCycle beam
  check (sameSnapshot exposed (← dodgeCavernCycle loaded)) "Reloading an active beam changed the dodge."
  let retreated ← walkToColumn loaded cavernCheckpointColumn 165
  check (retreated.cavern == .ready && !retreated.projectiles.any (·.kind == .cavernBeam))
    "Retreating to the ledge did not reset the Colossus and clear the beams."
  let retry ← summonCavern (← approachCavern retreated)
  loseCavernFight retry
  let ready ← summonCavern (← approachCavern retreated)
  let won ← winCavernFight (← dodgeCavernCycle (← waitForCavernBeam ready))
  let some drop := (match won.cavern with | .reward position => some position | _ => none)
    | throw (IO.userError "There was no Aegis drop.")
  saveRoundTrip won
  let collected ← walkToColumn won drop.x 250
  check (collected.cavern == .cleared && collected.coins == won.coins + 24 &&
    Inventory.count collected.inventory .deepstoneAegis == 1)
    "The Colossus did not award one Aegis and 24 coins."
  let gearSlot ← slotFor collected .deepstoneAegis
  let equipped := step collected { equipGear := some gearSlot }
  check (equipped.equipment.armor == some .deepstoneAegis &&
    equipped.defense == collected.defense + 2 && Inventory.count equipped.inventory .deepstoneAegis == 0)
    "The Aegis did not occupy the armor slot and add two defense."
  saveRoundTrip equipped
  let cleared ← approachCavern equipped
  let repeated := step cleared { interact := some cavernAltar }
  check (repeated.cavern == .cleared && repeated.coins == collected.coins)
    "The completed altar granted another encounter or reward."
  return repeated

private def approachMagmaGallery (cavern : GameState) : IO GameState := do
  let entrance ← walkToColumn cavern (magmaEntranceCaps[0]!.x + 2) 600
  let mut pick ← equip entrance .crystalPick
  for cap in magmaEntranceCaps do
    pick := step pick { mine := some cap }
    check (pick.world.tile cap == .air) s!"Could not open the magma passage at {repr cap}. {pick.status}"
  let shelf ← walkToColumn pick magmaLedgeColumn 360
  check (inMagmaGallery shelf.player.centerTile && shelf.canReach magmaWaterGate &&
    shelf.player.y + playerHeight == (magmaLedgeFloor : Int) * tileUnits &&
    shelf.world.tile magmaWaterGate == .stone && !Magma.touches shelf.magma shelf.world shelf.player)
    s!"The lava pool was unreachable from Deepstone. {repr shelf.player.centerTile}"
  return shelf

private def magmaContactCheck (shelf : GameState) : IO Unit := do
  let deck ← walkToColumn shelf 420 160
  check (deck.player.y + playerHeight == (magmaLedgeFloor : Int) * tileUnits)
    "The platform did not hold the player clear of the magma."
  let target : TilePos := ⟨420, 110⟩
  let building ← equip deck .stone
  let blocked := step building { place := some target }
  check (building.magma.level building.world target > 0 && blocked.world == building.world &&
    blocked.inventory == building.inventory && blocked.magma == building.magma)
    "A solid block displaced lava or was consumed by a rejected placement."
  let oldBridge := deck.world.setTile target .platform |>.setTile ⟨420, 109⟩ .rail
  let heated ← walk { deck with world := oldBridge } 4 0
  check (heated.world.tile target == .air && heated.world.tile ⟨420, 109⟩ == .rail &&
    canParkCart heated.world ⟨420, 109⟩ && heated.magma.level heated.world target > 0)
    "Lava did not burn the platform while leaving the track usable."
  let damage := max 1 (Magma.contactDamage - (deck.defense + 1) / 2)
  let mut state := deck
  let mut touched := false
  for _ in [:640] do
    let next := step state { dropThrough := true }
    if !touched && next.combat.onFire == lavaBurnTicks then
      check (next.combat.health + damage == state.combat.health &&
        Magma.touches next.magma next.world next.player && next.combat.lavaCooldown == lavaHurtCooldown)
        "The magma burn ignored depth, armor, or hurt cooldown."
      touched := true
    if next.respawnTicks > 0 then
      let respawned ← waitForRespawn next
      check (touched && respawned.player == respawn respawned.world state.player respawned.combat.crawler &&
        respawned.inventory == deck.inventory && respawned.magma == deck.magma)
        "Lava defeat did not preserve the gear and liquid while returning the player to spawn."
      return
    state := next
  throw (IO.userError s!"Dropping through the deck did not produce a burn and respawn. {state.status}")

private def releaseGalleryWater (shelf : GameState) : IO GameState := do
  let mut state := step shelf { mine := some magmaWaterGate }
  check (state.world.tile magmaWaterGate == .air)
    s!"The crystal pick could not open the sluice. {state.status}"
  let fall : TilePos := ⟨magmaWaterGate.x, magmaWaterGate.y + 1⟩
  for _ in [:36] do
    if state.water.level state.world fall > 0 then
      check (magmaVolume state == magmaVolume shelf && waterVolume state == waterVolume shelf)
        "Opening the sluice lost liquid before contact."
      return state
    state := step state {}
  throw (IO.userError "Water never passed through the open sluice.")

private def lavaMergeChecks : IO Unit := do
  let world : World := { width := 5, height := 5, tiles := ByteArray.mk (Array.replicate 25 Tile.air.code) }
  let center : TilePos := ⟨2, 2⟩
  let below : TilePos := ⟨2, 3⟩
  let above : TilePos := ⟨2, 1⟩
  let left : TilePos := ⟨1, 2⟩
  let right : TilePos := ⟨3, 2⟩
  let fill (cells : List (TilePos × Nat)) : Water :=
    cells.foldl (fun liquid (position, amount) =>
      { levels := liquid.levels.set! (position.y.toNat * world.width + position.x.toNat) amount.toUInt8 })
      (Water.empty world)
  let merge (water magma : List (TilePos × Nat)) :=
    Magma.cool world (fill water) (fill magma) (fun _ _ => true)
  let (terrain, water, lava, blocks) := merge [(above, 12), (left, 12), (below, 90)] [(center, 240)]
  check (terrain.tile center == .obsidian && blocks == #[center] &&
    water.level terrain below == 90 && water.level terrain above == 0 && water.level terrain left == 0 &&
    lava.level terrain center == 0) "Water at 24/255 above/beside lava merged at the wrong position or amount."
  let (_, water, lava, blocks) := merge [(right, 23), (below, 90)] [(center, 240)]
  check (blocks.isEmpty && water.level world right == 0 && water.level world below == 90 &&
    lava.level world center == 240) "A thin lateral film failed to evaporate before the below-water check."
  let (terrain, water, lava, blocks) := merge [(below, 1)] [(center, 24)]
  check (blocks == #[below] && terrain.tile below == .obsidian &&
    water.level terrain below == 0 && lava.level terrain center == 0)
    "Lava at 24/255 above water did not form obsidian in the lower cell."
  let (_, water, lava, blocks) := merge [(below, 90)] [(center, 23)]
  check (blocks.isEmpty && water.level world below == 90 && lava.level world center == 0)
    "A sub-threshold drop of lava above water did not evaporate by itself."
  let deck := world.setTile center .platform
  check ((Magma.burnPlatforms deck (fill [(center, 1)]) (fill [(below, 1)])).tile center == .air)
    "Lava left a wooden platform intact after flowing down through its cell."

private def magmaExpedition (cavern : GameState) : IO GameState := do
  let shelf ← approachMagmaGallery cavern
  magmaContactCheck shelf
  let flowing ← releaseGalleryWater shelf
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode flowing)).mapError IO.userError
  check (sameSnapshot flowing loaded) "The open sluice and in-progress water flow did not round trip."
  let cooled ← walk flowing 72 0
  check (sameSnapshot cooled (← walk loaded 72 0)) "The resumed fluids reacted differently."
  check (magmaVolume cooled < magmaVolume flowing && waterVolume cooled < waterVolume flowing)
    "Cooling did not consume both liquids."
  let some (block, _) := (magmaGallery cavern.seed).magma.find? (fun (position, _) =>
      cooled.world.tile position == .obsidian && cooled.canReach position)
    | throw (IO.userError "The new Obsidian did not form within reach of the ledge.")
  check (cooled.chart.tile? cooled.world block == some .obsidian &&
    cooled.water.level cooled.world block == 0 && cooled.magma.level cooled.world block == 0)
    "Solidification did not update the chart and clear the block's liquid."
  let basic ← equip cooled .basicPick
  let refused := step basic { mine := some block }
  check (refused.world.tile block == .obsidian && refused.inventory == basic.inventory)
    "A basic pick mined Obsidian."
  let pick ← equip refused .crystalPick
  let mined := step pick { mine := some block }
  check (mined.world.tile block == .air && Inventory.count mined.inventory .obsidian == 1)
    "The crystal pick did not yield one block of Obsidian."
  let material ← equip mined .obsidian
  let sealed := step material { place := some magmaWaterGate }
  check (sealed.world.tile magmaWaterGate == .obsidian &&
    Inventory.count sealed.inventory .obsidian == 0)
    s!"The Obsidian could not rebuild the sluice. {sealed.status}"
  saveRoundTrip sealed
  let exited ← walkToColumn sealed (magmaEntranceCaps[0]!.x + 2) 500
  check (inCavernArena exited.player.centerTile && exited.cavern == .cleared)
    "The return from the gallery did not reach the previous arena."
  return exited

private def classicEyeChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let some altar := (eyeAltars fresh.seed)[0]? | throw (IO.userError "No early natural Demon Altar.")
  check ((altarCells altar).all (fun pos => fresh.world.tile pos == .demonAltar) &&
    (altarCells altar).all (fun pos => (generatePreEyeTerrain fresh.seed).tile pos == .air))
    "The altar did not have a complete natural footprint."
  let some (lenses, _) := Inventory.addItems fresh.inventory .lens 12
    | throw (IO.userError "Could not prepare lenses.")
  let stocked := { fresh with inventory := lenses }
  let recipe : ClassicRecipe := .suspiciousLookingEye
  let refused := step stocked { craftClassic := some recipe }
  check (refused.inventory == stocked.inventory) "Crafting outside an altar consumed lenses."
  let some player := findStandingNear stocked.world (altar.x + 1) (altar.y + 1) 0 0 (fun _ => true)
    | throw (IO.userError "The natural altar cannot be approached.")
  let atAltar := { stocked with player, chart := stocked.chart.reveal stocked.world player.centerTile }
  let crafted := step (step atAltar { craftClassic := some recipe }) { craftClassic := some recipe }
  check (Inventory.count crafted.inventory .lens == 0 &&
    Inventory.count crafted.inventory .suspiciousLookingEye == 2) "Each summoning item did not consume six lenses."
  let selected ← equip crafted .suspiciousLookingEye
  let sunlit := step selected { mine := some selected.player.centerTile }
  check (sunlit.eye.isNone && sunlit.inventory == selected.inventory) "Daylight did not preserve the summon item."
  let evening := { sunlit with surface := { clock := classicDayTicks + 5000 } }
  let summon := step evening { mine := some evening.player.centerTile }
  let some eye := summon.eye | throw (IO.userError "The night summon did not spawn an Eye.")
  check (eye.health == 2800 && eye.phase == .firstHover && Inventory.count summon.inventory .suspiciousLookingEye == 1)
    "The summon did not create a full-health Classic Eye and consume one item."
  let repeated := step summon { mine := some summon.player.centerTile }
  check (repeated.inventory == summon.inventory && repeated.eye.isSome) "A second manual summon was consumed."
  saveRoundTrip repeated
  let dawn := step { repeated with surface := { clock := classicCycleTicks - 1 } } {}
  check (dawn.eye.any (·.phase == .retreat) && !dawn.eyeDefeated) "The boss did not retreat at dawn."
  let gone ← walk dawn 10 0
  check (gone.eye.isNone && gone.nightDrops.isEmpty) "A retreat paid loot or failed to despawn."

  let mut inventory := fresh.inventory
  for (item, quantity) in [(Item.goldBow, 1), (.arrow, 3), (.lesserHealingPotion, 2)] do
    let some (updated, _) := Inventory.addItems inventory item quantity
      | throw (IO.userError "Cannot prepare the bow fixture.")
    inventory := updated
  let above : EyeMotion := ⟨bodyCenterX fresh.player + 6 * tileUnits, bodyCenterY fresh.player - 8 * tileUnits, 0, 0⟩
  let target : TilePos := ⟨above.x / tileUnits, above.y / tileUnits⟩
  let archer : GameState := { fresh with
    inventory
    townGuide := none
    surface := { clock := classicDayTicks + 1830 }
    eye := some { eye with motion := above, health := 1406, ticks := 0, servantTicks := 0 } }
  let prepared ← equip archer .goldBow
  let shot := step prepared { mine := some target }
  check (shot.bowCooldown == 26 && shot.projectiles.any (fun projectile => projectile.damage == 16))
    "Gold Bow and wooden arrow damage were not combined."
  let mut transformed := shot
  for _ in [:30] do
    if transformed.eye.any (·.phase == .transformSpinUp) then break
    transformed := step transformed {}
  check (transformed.eye.any (fun boss => boss.phase == .transformSpinUp && boss.health == 1396))
    "A real arrow hit did not cross the 1400-HP phase threshold."
  saveRoundTrip transformed
  let lastHit : GameState := { prepared with
    world := prepared.world.setTile target .platform
    eye := some { eye with motion := above, health := 16, phase := .secondHover, ticks := 0, servantTicks := 0 } }
  let mut winner := step lastHit { mine := some target }
  for _ in [:30] do
    if winner.eyeDefeated then break
    winner := step winner {}
  check (winner.eye.isNone && winner.eyeDefeated && winner.nightDrops.any (fun drop =>
    drop.position == { target with y := target.y - 1 } && drop.coins == 30000 &&
    drop.items.any (fun item => item.item == .demoniteOre && 30 ≤ item.count && item.count ≤ 90) &&
    drop.items.any (fun item => item.item == .lesserHealingPotion && 5 ≤ item.count && item.count ≤ 15)))
    "The finishing arrow did not leave the Classic boss loot above the platform."
  saveRoundTrip winner
  let hurt := { winner with combat := { winner.combat with health := 20 } }
  let potions ← equip hurt .lesserHealingPotion
  let healed := step potions { mine := some potions.player.centerTile }
  let denied := step healed { mine := some healed.player.centerTile }
  check (healed.combat.health == 70 && healed.potionSickness == 3600 &&
    Inventory.count denied.inventory .lesserHealingPotion == 1 && denied.combat.health == 70)
    "Lesser Healing Potion did not heal 50 HP and prevent immediate reuse."
  saveRoundTrip denied

private def evilBiomeChecks : IO Unit := do
  for seed in ([42, 0] : List UInt64) do
    let fresh := newGame seed
    let crimson := altarTile seed == .crimsonAltar
    let rock := if crimson then Tile.crimstone else .ebonstone
    let gun := if crimson then Item.undertaker else .musket
    let layout := evilLayout seed
    check (layout.relics.size == 3 && layout.relics.all (fun anchor =>
      (evilRelicCells anchor).all (fun cell => fresh.world.tile cell == evilRelicTile seed)))
      "A natural evil relic was missing part of its footprint."
    let anchor := layout.relics[0]!
    let explosive : ActiveBomb := {
      x := anchor.x * tileUnits + tileUnits / 2
      y := anchor.y * tileUnits + tileUnits / 2
      vx := 0, vy := 0, fuse := 1, sticky := false }
    let ready := { fresh with bombs := #[explosive] }
    let resumed ← IO.ofExcept ((Save.decode (Save.encode ready)).mapError IO.userError)
    let shattered := step ready {}
    check (sameSnapshot shattered (step resumed {})) "A saved bomb changed its explosion."
    check (shattered.evilRelicsBroken == 1 && shattered.bombs.isEmpty &&
      (evilRelicCells anchor).all (fun cell => shattered.world.tile cell == .air) &&
      (explosive.blastTiles fresh.world).any (fun cell =>
        fresh.world.tile cell == rock && shattered.world.tile cell == .air))
      "The blast did not remove a whole relic and its stone seal."
    let some reward := shattered.nightDrops.find? (fun drop => drop.items.any (·.item == gun))
      | throw (IO.userError "The first relic did not drop its firearm.")
    check (reward.items.contains ⟨.musketBall, 100⟩) "The first relic dropped the wrong ammunition count."
    saveRoundTrip shattered

    let inventory := fresh.inventory.set! 3 (some ⟨gun, 1⟩) |>.set! 4 (some ⟨.musketBall, 100⟩)
    let equipped := { fresh with inventory, selected := 3 }
    let center := equipped.player.centerTile
    let shot := step equipped { mine := some ⟨center.x + 4, center.y - 8⟩ }
    check (Inventory.count shot.inventory .musketBall == 99 && shot.bowCooldown == gun.gunUseTime &&
      shot.projectiles.any (fun bullet => bullet.kind.isBullet && bullet.damage == gun.rangedDamage + 7))
      "The firearm did not consume its own ammunition or apply Classic damage and use time."
    saveRoundTrip shot

  let fresh := newGame defaultSeed
  let inventory := fresh.inventory.set! 3 (some ⟨.bomb, 1⟩) |>.set! 4 (some ⟨.caveGel, 1⟩)
  let made := step { fresh with inventory } { craftClassic := some .stickyBomb }
  let selected ← equip made .stickyBomb
  let center := selected.player.centerTile
  let lit := step selected { mine := some ⟨center.x, center.y + 1⟩ }
  let some bomb := lit.bombs[0]? | throw (IO.userError "A crafted Sticky Bomb could not be thrown.")
  check (bomb.sticky && bomb.fuse == bombFuseTicks - 1 && Inventory.count lit.inventory .stickyBomb == 0)
    "The bomb was not consumed when its fuse started."
  saveRoundTrip lit
  let defused := step lit { interact := some bomb.position }
  check (defused.bombs.isEmpty && Inventory.count defused.inventory .stickyBomb == 1)
    "A defused bomb was not recovered exactly once."

set_option maxHeartbeats 400000 in
private def evilBossChecks : IO Unit := do
  for seed in ([42, 0] : List UInt64) do
    let fresh := newGame seed
    let crimson := altarTile seed == .crimsonAltar
    let layout := evilLayout seed
    let hub := layout.entrance.x + (if crimson then 20 else 9)
    let some player := findStandingNear fresh.world hub layout.floor 0 0 (fun _ => true)
      | throw (IO.userError "The generated evil hub has no test position.")
    let spawn := { fresh with player, chart := fresh.chart.reveal fresh.world player.centerTile }
    let mut current := spawn
    for anchor in layout.relics do
      let bomb : ActiveBomb := {
        x := anchor.x * tileUnits + tileUnits / 2
        y := anchor.y * tileUnits + tileUnits / 2
        vx := 0, vy := 0, fuse := 1, sticky := false }
      let detonated := step { current with bombs := #[bomb], player := fresh.player } {}
      current := { detonated with player }
      if current.evilRelicsBroken < 3 then
        check (current.eater.isNone && current.brain.isNone) "An evil boss appeared before the third relic."
    check (current.evilRelicsBroken == 3 &&
      (if crimson then current.brain.any (fun boss => boss.totalHealth == brainTotalMaxHealth && boss.phase == .shielded)
        else current.eater.any (fun boss => boss.health == eaterMaxHealth)))
      "The third relic failed to summon the full Classic boss."
    saveRoundTrip current
    let resumed ← IO.ofExcept ((Save.decode (Save.encode current)).mapError IO.userError)
    check (sameSnapshot (step resumed {}) (step current {})) "An active evil fight diverged after reload."

    let summon := if crimson then Item.bloodySpine else .wormFood
    let inventory := fresh.inventory.set! 3 (some ⟨summon, 1⟩)
    let distant := step { fresh with inventory, selected := 3 } { mine := some fresh.player.centerTile }
    check (Inventory.count distant.inventory summon == 1 && distant.eater.isNone && distant.brain.isNone)
      "Using the boss summon outside the biome consumed it."
    let used := step { spawn with inventory, selected := 3 } { mine := some player.centerTile }
    check (Inventory.count used.inventory summon == 0 && (if crimson then used.brain.isSome else used.eater.isSome))
      "The evil boss summon did not work by day in its own biome."
    if let some brain := used.brain then
      check ((brain.takeHit used.player 400).health == brainMaxHealth) "The shielded brain took damage."
      let exposed := brain.creepers.foldl (fun boss creeper => boss.damageCreeper creeper.id 105) brain
      check (exposed.phase == .exposed && exposed.creepers.isEmpty &&
        (exposed.takeHit used.player 17).health == brainMaxHealth - 10)
        "The brain's vulnerability did not follow the last Creeper."
    if let some eater := used.eater then
      let split := eater.damageSegment 33 152
      check (split.chains.size == 2 && split.segmentKind 32 == some .tail && split.segmentKind 34 == some .head)
        "A destroyed worm segment failed to split the chain."

    let recipe := if crimson then ClassicRecipe.deathbringerPickaxe else .nightmarePickaxe
    let (bar, material) := if crimson then (Item.crimtaneBar, Item.tissueSample) else (.demoniteBar, .shadowScale)
    let stock := fresh.inventory.set! 3 (some ⟨bar, 12⟩) |>.set! 4 (some ⟨material, 6⟩)
    let center := fresh.player.centerTile
    let rock : TilePos := ⟨center.x + 2, center.y⟩
    let world := fresh.world.setTile ⟨center.x - 2, center.y⟩ .ironAnvil
      |>.setTile rock (if crimson then .crimstone else .ebonstone)
    let bench := { fresh with inventory := stock, world, selected := 1 }
    let refused := step bench { mine := some rock }
    check (refused.world.tile rock == bench.world.tile rock) "A copper pick damaged the evil stone seal."
    let forged := step bench { craftClassic := some recipe }
    let mut equipped ← equip forged recipe.item
    for _ in [:40] do
      if equipped.world.tile rock == .air then break
      equipped := step equipped { mine := some rock }
    check (equipped.world.tile rock == .air && Inventory.count equipped.inventory material == 0)
      "The new pickaxe did not mine its stone or spend the boss material."

private def skeletronHeadChecks : IO Unit := do
  let initial := spawnSkeletron ⟨22, 40⟩
  let headFromBits := fun (bits : Array UInt32) (direction : Int) =>
    ({ x := Float32.ofBits bits[0]!, y := Float32.ofBits bits[1]!,
       vx := Float32.ofBits bits[2]!, vy := Float32.ofBits bits[3]!,
       rotation := Float32.ofBits bits[4]!, direction } : SkeletronHead)
  let frameWords := fun (boss : Skeletron) =>
    let head := boss.head
    let phase := match boss.phase with
      | .hover => 0 | .spin => 1 | .enraged => 2 | .retreat => 3
    #[head.x.toBits, head.y.toBits, head.vx.toBits, head.vy.toBits, head.rotation.toBits,
      UInt32.ofNat (head.direction + 1).toNat, phase, UInt32.ofNat boss.ticks,
      UInt32.ofNat boss.headDefense]
  let checkFrame := fun (label : String) (bits : Array UInt32) phase ticks direction player expected =>
    let boss := { initial with head := headFromBits bits direction, phase, ticks }
    let actual := frameWords (boss.advanceHead player)
    check (actual == expected) s!"Skeletron head disagreed with Terraria 1.4.5.8 for {label}: {actual}."
  -- Reference values come from the desktop 1.4.5.8 Classic AI and NPC update IL.
  -- Both simulations receive the same player center and hover target height.
  checkFrame "hover exact target" #[0x42968000, 0x437a0000, 0xc0000000, 0x3f800000, 0x3f900000] .hover 1 (-1) ⟨1754, 8000, 0, 0, false⟩
    #[0x42928000, 0x437b0000, 0xc0000000, 0x3f800000, 0xbe088889, 0, 0, 2, 10]
  checkFrame "hover horizontal snap" #[0x42960000, 0x437a0000, 0xbb83126f, 0x3a83126f, 0x3f900000] .hover 1 (-1) ⟨1750, 8000, 0, 0, false⟩
    #[0x42960000, 0x437a0042, 0x00000000, 0x3a83126f, 0xb98bcf65, 0, 0, 2, 10]
  checkFrame "hover threshold rounds down" #[0x42960000, 0x437a0000, 0x3ba3d70a, 0xba83126f, 0x3f900000] .hover 1 (-1) ⟨1750, 8000, 0, 0, false⟩
    #[0x42960000, 0x4379ffbe, 0x00000000, 0xba83126f, 0x39aec33e, 0, 0, 2, 10]
  checkFrame "hover caps backwards momentum" #[0x42960000, 0x43820000, 0x42c80000, 0x41000000, 0x3f900000] .hover 1 (-1) ⟨1670, 8000, 0, 0, false⟩
    #[0x42a60000, 0x43830000, 0x41000000, 0x40000000, 0x40d55555, 0, 0, 2, 10]
  checkFrame "hover leaves forward momentum" #[0x42960000, 0x43820000, 0xc2c80000, 0xc1000000, 0x3f900000] .hover 1 (-1) ⟨1670, 8000, 0, 0, false⟩
    #[0xc1c86668, 0x437bfae1, 0xc2c8199a, 0xc10051ec, 0xc0d55555, 0, 0, 2, 10]
  checkFrame "hover to spin facing right" #[0x42940000, 0x437a0000, 0xc0000000, 0x3f800000, 0x3f900000] .hover 799 (-1) ⟨1750, 8000, 0, 0, false⟩
    #[0x42902e14, 0x437b0000, 0xbff47ae2, 0x3f800000, 0xbe088889, 2, 1, 0, 10]
  checkFrame "hover to spin facing left" #[0x42968000, 0x437a0000, 0xc0000000, 0x3f800000, 0x3f900000] .hover 799 (1) ⟨1754, 8000, 0, 0, false⟩
    #[0x42928000, 0x437b0000, 0xc0000000, 0x3f800000, 0xbe088889, 0, 1, 0, 10]
  checkFrame "spin to hover" #[0x43965000, 0x43c81000, 0x3dcccccd, 0x3e4ccccd, 0x3f900000] .spin 399 (-1) ⟨4689, 9620, 0, 0, false⟩
    #[0x43961fa3, 0x43c8c9cf, 0xbec17300, 0x3fb9cf2d, 0x3f533333, 0, 0, 0, 0]
  checkFrame "spin exact zero distance" #[0x42960000, 0x437a0000, 0x40800000, 0x40a00000, 0x3f900000] .spin 3 (-1) ⟨1750, 4640, 0, 0, false⟩
    #[0x42960000, 0x437a0000, 0x00000000, 0x00000000, 0x3f533333, 0, 1, 4, 0]
  checkFrame "spin nearly vertical" #[0x42960000, 0x437a0000, 0x40800000, 0x40a00000, 0x3f900000] .spin 2 (-1) ⟨1755, 9280, 0, 0, false⟩
    #[0x42960000, 0x437b8000, 0x00000000, 0x3fbffff9, 0x3f533333, 0, 1, 3, 0]
  checkFrame "enrage" #[0x43965000, 0x43c81000, 0x41000000, 0x40c00000, 0x3f900000] .enraged 0 (-1) ⟨6481, 9620, 0, 0, false⟩
    #[0x4397e856, 0x43cbbb10, 0x404c2af2, 0x40eac3fe, 0x3f533333, 0, 2, 0, 9999]
  checkFrame "retreat retains rotation" #[0x42968000, 0x437a0000, 0x3b83126f, 0xc0000000, 0x3f900000] .retreat 0 (-1) ⟨3970, 8000, 0, 0, false⟩
    #[0x42968000, 0x437831ec, 0x00000000, 0xbfe70a3d, 0x3f900000, 0, 3, 0, 10]
  let start := headFromBits #[0x43965000, 0x43c81000, 0x3dcccccd, 0x3e4ccccd, 0x3f900000] (-1)
  let mut boss := { initial with head := start }
  let mut trace : UInt64 := 14695981039346656037
  for i in [:2400] do
    let player : Player := ⟨4480 + (i * 73) % 3520, 9440 + (i * 41) % 900, 0, 0, false⟩
    boss := boss.advanceHead player
    for word in frameWords boss do
      trace := (trace ^^^ word.toUInt64) * 1099511628211
  check (trace == 0x2df943bff9d29bfe) s!"Skeletron head diverged during the two-cycle reference trace: {trace}."

private def skeletronHandChecks : IO Unit := do
  let head : SkeletronHead := ⟨500, 450, 0, 0, 0, -1⟩
  let initial := { spawnSkeletron ⟨30, 50⟩ with head }
  let phases : Array SkeletronHandPhase := #[.hover, .lift, .slam, .reset, .windup, .swipe]
  let frameWords := fun (hand : SkeletronHand) =>
    #[hand.x.toBits, hand.y.toBits, hand.vx.toBits, hand.vy.toBits, hand.rotation.toBits,
      UInt32.ofNat ((phases.findIdx? (· == hand.phase)).getD phases.size), UInt32.ofNat hand.ticks]
  let compare := fun (label : String) (bits : Array UInt32) (phase : SkeletronHandPhase)
      (ticks : Nat) (parent : SkeletronPhase) (side : Int) (target : SkeletronHandTarget) expected =>
    let read := fun i => Float32.ofBits bits[i]!
    let hand : SkeletronHand := ⟨read 0, read 1, read 2, read 3, read 4, 600, phase, ticks⟩
    let actual := frameWords (hand.advance { initial with phase := parent } target side)
    check (actual == expected) s!"Skeletron hand disagreed with Terraria 1.4.5.8 for {label}: {actual}."
  -- Cases from the desktop Classic style-12 and NPC.UpdateNPC instruction paths.
  compare "track hover on tick 299" #[0x44110000, 0x442f0000, 0xbfa66666, 0x3f4ccccd, 0x3f900000] .hover 299
    .hover 1 ⟨420, 820, 430, 841⟩ #[0x4410b49c, 0x442f2e98, 0xbf96c8b3, 0x3f3a5e35, 0x3f9ea214, 1, 0]
  compare "track retreat on tick 299" #[0x44110000, 0x442f0000, 0xbfa66666, 0x3f4ccccd, 0x3f900000] .hover 299
    .retreat 1 ⟨420, 820, 430, 841⟩ #[0x4410b687, 0x442f2cac, 0xbf92f1a9, 0x3f32b021, 0x3f9ea214, 0, 299]
  compare "reset hover on tick 299" #[0x44110000, 0x442f0000, 0xbfa66666, 0x3f4ccccd, 0x3f900000] .reset 299
    .hover 1 ⟨420, 820, 430, 841⟩ #[0x4410b49c, 0x442f2e98, 0xbf96c8b3, 0x3f3a5e35, 0x3f9ea214, 4, 0]
  compare "track hover equal and tiny snap" #[0x439d0000, 0x442a0000, 0x3b83126f, 0x3a83126f, 0x3f900000] .hover 298
    .hover (-1) ⟨420, 820, 430, 841⟩ #[0x439d0000, 0x442a0010, 0x00000000, 0x3a83126f, 0xba50c000, 0, 299]
  compare "lift strict y minus200 equality" #[0x43d20000, 0x437a0000, 0x3b83126f, 0xc1000000, 0x3f900000] .lift 0
    .hover (-1) ⟨420, 820, 430, 841⟩ #[0x43d20000, 0x43720000, 0x00000000, 0xc1000000, 0x40596ed2, 1, 0]
  compare "lift launch at same center" #[0x43d20000, 0x4379f000, 0x40000000, 0xc1000000, 0x3f900000] .lift 0
    .hover (-1) ⟨436, 254.9375, 446, 275.9375⟩ #[0x43d20000, 0x4379f000, 0x00000000, 0x00000000, 0x40596e32, 2, 0]
  compare "slam continues at distance exact2000" #[0x43d20000, 0x443b8000, 0x3f800000, 0x00000000, 0x3f900000] .slam 0
    .hover (-1) ⟨2446, 776, 2456, 797⟩ #[0x43d28000, 0x443b8000, 0x3f800000, 0x00000000, 0x3f900000, 2, 0]
  compare "slam ends past distance2000" #[0x43d20000, 0x443b8000, 0x3f800000, 0x00000000, 0x3f900000] .slam 0
    .hover (-1) ⟨2446.0625, 776, 2456.0625, 797⟩ #[0x43d28000, 0x443b8000, 0x3f800000, 0x00000000, 0x3f900000, 3, 0]
  compare "windup equality left" #[0x41600000, 0x44200000, 0xbb83126f, 0x3a83126f, 0x3f900000] .windup 0
    .hover (-1) ⟨420, 820, 430, 841⟩ #[0x415e5604, 0x44200010, 0xbdd4fdf4, 0x3a79096c, 0x3fceedd4, 4, 0]
  compare "windup crosses right" #[0x447d8400, 0x44200000, 0x40800000, 0x3f666666, 0x3f900000] .windup 0
    .hover 1 ⟨420, 820, 430, 841⟩ #[0x44796e2e, 0x44212c00, 0xc182ba4f, 0x40960006, 0x4095476e, 5, 0]
  compare "swipe equal x keeps charging" #[0x43d20000, 0x442f0000, 0x3f800000, 0x40000000, 0x3f900000] .swipe 0
    .hover (-1) ⟨436, 800, 446, 821⟩ #[0x43d28000, 0x442f8000, 0x3f800000, 0x40000000, 0x3f900000, 5, 0]
  compare "swipe passed x ends" #[0x43d20000, 0x442f0000, 0x3f800000, 0x40000000, 0x3f900000] .swipe 0
    .hover (-1) ⟨435.9375, 800, 445.9375, 821⟩ #[0x43d28000, 0x442f8000, 0x3f800000, 0x40000000, 0x3f900000, 0, 0]
  let spawned := ({ head with x := 500.625, y := 450.9375 } : SkeletronHead).spawnHand 150
  check (spawned.x == 514 && spawned.y == 449 && spawned.ticks == 150)
    "Skeletron hand spawn did not truncate the head center X and top Y before assigning the bottom."
  for moving in [false, true] do
    let mut right := head.spawnHand 0
    let mut left := head.spawnHand 150
    let mut trace : UInt64 := 14695981039346656037
    for tick in [1:2501] do
      let (parent, target) := if moving then
        let offset := fun (period : Nat) (divisor : Float32) =>
          ((Int32.ofNat (tick % period)) - Int32.ofNat (period / 2)).toFloat32 / divisor
        let phase : SkeletronPhase := if tick ≤ 750 then .hover
          else if tick ≤ 940 then .spin else if tick ≤ 1020 then .enraged
          else if tick ≤ 1070 then .retreat else .hover
        let head := { head with x := 500 + offset 160 8, y := 450 + offset 96 8 }
        let x := 420 + offset 220 4
        let y := 820 + offset 128 8
        ({ initial with head, phase }, (⟨x, y, x + 10, y + 21⟩ : SkeletronHandTarget))
      else (initial, ⟨420, 820, 430, 841⟩)
      right := right.advance parent target 1
      left := left.advance parent target (-1)
      for word in frameWords right ++ frameWords left do
        trace := (trace ^^^ word.toUInt64) * 1099511628211
    let expected : UInt64 := if moving then 0x340fa0ad478e1cb3 else 0x04e2d8fcb25a2318
    check (trace == expected)
      s!"Skeletron hands diverged during the Classic 2,500-frame reference trace (moving={moving}): {trace}."

private def skeletronLifetimeChecks : IO Unit := do
  let spawn := spawnSkeletron ⟨30, 40⟩
  let boss := { spawn with head := { spawn.head with x := 500, y := 450 }, ticks := 221 }
  let atPixels := fun (x y : Int) => (⟨16 * x, 16 * y, 0, 0, false⟩ : Player)
  let nearby := atPixels 500 450
  let far := atPixels 2501 450
  let trace := fun (start : Skeletron) (player : Player) (dead : Bool) => Id.run do
    let mut live := some start
    let mut firstUnrefreshed := 0
    for tick in [1:200] do
      live := live.bind (fun current => advanceSkeletron player current false dead)
      if firstUnrefreshed == 0 && live.all (·.timeLeft != 749) then firstUnrefreshed := tick
      if live.isNone then return (firstUnrefreshed, tick)
    return (firstUnrefreshed, 0)
  -- The original style-11 and CheckActive instructions with this game's player rectangle.
  check (trace boss nearby true == (116, 165) && trace boss far false == (1, 50) &&
    trace { boss with timeLeft := 5 } far false == (1, 5))
    "Skeletron despawned before its original inactivity countdown expired."
  let elapsed := advanceSkeletron (atPixels 2500 450) { boss with timeLeft := 10 } false false
  let abandoned := elapsed.bind (fun current => advanceSkeletron (atPixels 2500 450) current false true)
  check (elapsed.any (fun current => current.phase == .hover && current.timeLeft == 9) &&
    abandoned.any (fun current => current.phase == .retreat && current.ticks == 222 && current.timeLeft == 8))
    "Switching to withdrawal did not carry the existing lifetime and AI counter."
  let edge := atPixels 1580 450
  let toward := { boss with head := { boss.head with vx := 10 } }
  check ((advanceSkeletron edge boss false true).any (·.timeLeft == 49) &&
    (advanceSkeletron edge toward false true).any (·.timeLeft == 749))
    "CheckActive did not use strict edges and the post-move head rectangle."
  let enraged := advanceSkeletron nearby boss true false
  let withdrawing := enraged.bind (fun current => advanceSkeletron nearby current false true)
  check (withdrawing.any (fun current => current.phase == .retreat && current.ticks == 221 &&
    current.headDamage == 9999 && current.headDefense == 10 && current.timeLeft == 749))
    "A withdrawing head lost its original damage or failed to refresh near a dead player."

private def dungeonContactChecks : IO Unit := do
  let handDamage := #[17,17,17,18,18,18,18,18,19,19,19,19,19,20,20,20,20,20,21,21,21,21,21,22,22,22,22,22,23,23,23]
  let headDamage := #[27,28,28,28,28,29,29,29,30,30,30,31,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,36,36,37]
  for roll in [:31] do
    check (classicNpcContactDamage 20 roll 0 == (handDamage[roll]!, 40) &&
      classicNpcContactDamage 32 roll 0 == (headDamage[roll]!, 40) &&
      classicNpcContactDamage 9999 roll 3 == (8497 + 100 * roll, 40))
      "Dungeon contact damage diverged from the original Classic damage roll."
  check (classicNpcContactDamage 2 15 2 == (1, 20) &&
    classicNpcContactDamage 2 15 1 == (1, 40) &&
    classicNpcContactDamage 50 0 0 == (42, 40) && classicNpcContactDamage 50 2 0 == (44, 40))
    "Dungeon contact damage lost half-point immunity or round-to-even behavior."
  let fresh := newGame defaultSeed
  let player := { fresh.player with y := fresh.player.y - 4 * tileUnits, grounded := false }
  let x := (Int32.ofInt (player.x.tdiv 16)).toFloat32
  let y := (Int32.ofInt (player.y.tdiv 16)).toFloat32
  let spawn := spawnSkeletron player.centerTile
  let head := { spawn.head with x := x - 45, y := y - 40 }
  let hand := head.spawnHand 0
  let right := { hand with x := x - 35, y := y - 12 }
  let left := { hand with x := x - 10, y := y - 12 }
  let boss := { spawn with head, leftHand := some left, rightHand := some right }
  let bothHands := { boss with head := { head with y := y - 220 } }
  check (boss.contact player == some (32, 1) && bothHands.contact player == some (20, 1) &&
    ({ bothHands with rightHand := none } : Skeletron).contact player == some (20, -1))
    "Skeletron contact did not use head, right hand, left hand slot order."
  let contact := { fresh with
    player
    skeletron := some boss
    surface := { fresh.surface with clock := classicDayTicks + 900 }
    chart := fresh.chart.reveal fresh.world player.centerTile }
  let pushed := step contact {}
  check (pushed.combat.health < contact.combat.health && pushed.combat.invulnerable == 40 &&
    pushed.player.x == player.x + 72 && pushed.player.y == player.y - 56)
    "Skeletron did not knock back the player on the contact frame."
  saveRoundTrip pushed
  let shielded := step { contact with equipment := { contact.equipment with accessory := some .cobaltShield } } {}
  check (shielded.combat.health < contact.combat.health && shielded.combat.invulnerable == 40 &&
    shielded.player.x == player.x && shielded.player.y > player.y)
    "Cobalt Shield did not prevent the original contact knockback."
  let edgeHead := { head with x := x - 80, y := y - 40 }
  let spinning := { boss with
    head := edgeHead
    phase := .spin
    ticks := 1
    headDefense := 0
    leftHand := none
    rightHand := none }
  let touching := step { contact with skeletron := some spinning } {}
  let hit := step touching {}
  check (touching.combat.health == contact.combat.health && hit.combat.health < touching.combat.health)
    "Skeletron contact used its next AI position or counted a tangent as an overlap."

private def dungeonSlimeChecks : IO Unit := do
  let floorY := 200
  let empty : World := ⟨128, 202, ByteArray.mk (Array.replicate (128 * 202) Tile.air.code)⟩
  let world := (List.range empty.width).foldl (fun world x => world.setTile ⟨x, floorY⟩ .stone) empty
  let water := Water.empty world
  -- The projected target has the same integer center X as the source fixture.
  let player : Player := ⟨1005 * 16, 3150 * 16, 0, 0, false⟩
  let mut slime : DungeonSlime := { id := 0, x := 777.5, y := 3170 }
  let mut trace : UInt64 := 14695981039346656037
  let mut hops : Array Nat := #[]
  for tick in [1:361] do
    let some next := slime.advance world water player false true 125
      | throw (IO.userError "The reference Dungeon Slime despawned.")
    if slime.vy == 0 && next.vy < 0 then hops := hops.push tick
    slime := next
    let words := #[slime.x.toBits, slime.y.toBits, slime.vx.toBits, slime.vy.toBits, slime.lastJumpX.toBits,
      UInt32.ofInt (slime.jumpClock + 2120), UInt32.ofNat slime.retargetClock,
      UInt32.ofInt (slime.direction + 1), if slime.collideY then 1 else 0]
    for word in words do trace := (trace ^^^ word.toUInt64) * 1099511628211
  check (trace == 0xb1bfd68f3b15b527 && hops == #[20, 83, 146, 239, 302])
    s!"The flat 1.4.5.8 Dungeon Slime trace diverged: hash {trace}, jumps {hops}."
  let wet := { slime with vx := 0, vy := 3, wet := true, collideY := false }
  let (wading, _) := wet.advanceAI player false true (fun _ => false)
  check (wading.vy.toBits == 0x400ccccc && wading.lastJumpX == wet.x && wading.jumpClock == wet.jumpClock)
    "A submerged Dungeon Slime did not damp its downward velocity or hold its jump clock."
  let bonuses : Array Nat := #[95, 0, 5, 0, 10, 0, 15, 0, 20, 0, 25, 0, 50]
  check (dungeonSlimeCoins (fun i => if i == 0 then 0 else 1) == 120 &&
    dungeonSlimeCoins (fun i => bonuses[i]!) == 1891)
    "The original unbuffed Dungeon Slime coin endpoints changed."

private def darkCasterChecks : IO Unit := do
  let empty : World := ⟨96, 100, ByteArray.mk (Array.replicate (96 * 100) Tile.air.code)⟩
  let world := (List.range empty.width).foldl (fun w x => w.setTile ⟨x, 82⟩ .blueDungeonBrick) empty
  let magma := Water.empty world
  let player : Player := ⟨40 * tileUnits, 82 * tileUnits - playerHeight, 0, 0, true⟩
  let spawn := DarkCaster.atFloor 1 62 82
  -- Fixed draws select column 50, start y=80, and first find ground at y=82.
  let rolls := fun i : Nat => if i % 2 == 0 then 30 else 20
  let ai := fun (caster : DarkCaster) => caster.advanceAI world magma player false false (fun _ => true) rolls
  check ((ai spawn).1.clock == 501) "A new Dark Caster skipped the vanilla initial pause."
  let (queued, early) := ai { spawn with clock := 649 }
  check (queued.x == spawn.x && queued.clock == 1 && queued.casting == 19 &&
    queued.destination == some ⟨50, 82⟩ && early.isNone)
    "Dark Caster did not queue a legal teleport one tick before moving."
  let (teleported, shot) := ai queued
  check (teleported.x == 799 && teleported.y == 1272 && teleported.vx == 0 && teleported.vy == 0 &&
    teleported.destination.isNone && shot.isNone) "Teleport destination or velocity reset differs."
  let obstructed := world.setTile ⟨49, 78⟩ .stone
  check ((spawn.findTeleport obstructed magma player false false (fun _ => true) rolls).isNone &&
    (spawn.findTeleport world magma { player with vx := 160 } false false (fun _ => true) rolls).isNone &&
    (spawn.findTeleport world magma player false false (fun _ => false) rolls).isNone &&
    (spawn.findTeleport world magma player false true (fun _ => false) rolls) == some ⟨50, 82⟩)
    "Caster teleport ignored clearance, predicted movement, or Dungeon walls."
  let mut caster := { spawn with clock := 1 }
  let mut spells := #[]
  for _ in [:320] do
    let (next, fire) := ai caster
    caster := next
    if fire.isSome then spells := spells.push next.clock
  check (spells == #[104, 204, 304]) s!"Dark Caster volley ticks changed: {spells}."
  let (_, armed) := ai { spawn with clock := 400, casting := 26 }
  check (armed.isSome && (ai { spawn with clock := 400 }).2.isNone)
    "A hit must cancel future windups without cancelling the armed shot."
  let target : Player := ⟨(997 * 16) - playerWidth / 2, (1260 * 16) - playerHeight / 2, 0, 0, false⟩
  let sphere := WaterSphere.fromCaster 1 spawn target
  check (sphere.x == 992 && sphere.y == 1248 && sphere.vx.toBits == 0xc0666667 &&
    sphere.vy.toBits == 0x4099999a)
    "Unexpected Water Sphere launch origin or velocity."
  let target : Player := ⟨(1000 * 16) - playerWidth / 2, (1266 * 16) - playerHeight / 2, 0, 0, false⟩
  let sphere := WaterSphere.fromCaster 1 spawn target
  check (sphere.x == 992 && sphere.y == 1248 && sphere.vx == 0 && sphere.vy == 6 &&
    (sphere.advance target).any (fun next => next.y == 1254 && next.timeLeft == 749))
    "Water Sphere's first move or near-player inactivity refresh differs."
  let distant := { target with x := target.x + 2000 * 16 }
  check ((sphere.advance distant).any (·.timeLeft == 99) &&
    ({ sphere with timeLeft := 1 }.advance distant).isNone)
    "An offscreen Water Sphere failed to age after EncourageDespawn."
  let fresh := newGame defaultSeed
  let dungeon := dungeonLayout fresh.seed
  let savedCaster := { DarkCaster.atFloor 1 dungeon.goldChest.x 88 with
    clock := 1, casting := 19
    destination := some ⟨dungeon.goldChest.x + 7, 88⟩ }
  let sphere := WaterSphere.fromCaster 1 savedCaster fresh.player
  let live : GameState := { fresh with
    skeletronDefeated := true, oldManWaiting := false
    darkCasters := #[savedCaster], waterSpheres := #[{ sphere with timeLeft := 749 }]
    nextDarkCasterId := 2, nextWaterSphereId := 2 }
  saveRoundTrip live
  let bone := aimShot .thrownBone (50 * tileUnits) (79 * tileUnits) (51 * tileUnits) (79 * tileUnits)
  let mut airborne := bone
  for tick in [1:21] do
    let some shot := airborne | throw (IO.userError "The thrown Bone fell out of the fixture.")
    airborne := (advanceProjectileBoxes world shot #[]).1
    let (vx, vy) := if tick < 20 then (128, 0) else (124, 6)
    check (airborne.any (fun next => next.vx == vx && next.vy == vy && next.valid world))
      "Thrown Bone diverged from its straight launch or the first gravity tick."

private def angryBonesChecks : IO Unit := do
  let kinds : Array AngryBonesKind := #[.regular, .short, .bigBoned, .big, .bigMuscle, .bigHelmet]
  let stats := kinds.map fun kind => (kind.health, kind.damage, kind.defense, kind.coins)
  check (stats == #[(80,26,8,130), (72,23,7,117), (101,32,9,149),
    (70,34,6,130), (70,28,12,130), (120,24,14,130)])
    "An Angry Bones variant differs from the Classic SetDefaults values."
  let mut frequencies := Array.replicate kinds.size 0
  for first in [:5] do
    for big in [:4] do
      for short in [:5] do
        let selected := selectAngryBonesKind first big short
        let some index := kinds.findIdx? (· == selected)
          | throw (IO.userError "Unknown Dungeon fighter variant.")
        frequencies := frequencies.modify index (· + 1)
  check (frequencies == #[24, 6, 10, 20, 20, 20] &&
    selectAngryBonesKind 0 0 0 == .big && selectAngryBonesKind 4 0 0 == .bigBoned)
    "The ordinary Dungeon spawn's ordered Angry Bones draws changed."
  -- Ground, ledge and door vectors obtained from the installed style-3 AI.
  let empty : World := ⟨128, 202, ByteArray.mk (Array.replicate (128 * 202) Tile.air.code)⟩
  let ground := (List.range empty.width).foldl (fun world x => world.setTile ⟨x, 200⟩ .stone) empty
  let actor : AngryBones := {
    id := 1, kind := .regular, x := 797, y := 3160
    vx := 1, oldX := 796, spriteDirection := 1 }
  let target : Player := ⟨900 * 16, 3178 * 16, 0, 0, true⟩
  let close := (actor.advanceAI ground target false false 125).1
  let edge := (actor.advanceAI ground { target with x := target.x + 16 } false false 125).1
  check (close.vx.toBits == 0x4008f5c3 && close.vy == -4 && edge.vy == 0)
    "Angry Bones must lunge just inside, but not at, its 100-pixel boundary."
  let distant := { target with x := 1000 * 16 }
  let ledge := ground.setTile ⟨51, 199⟩ .stone
  let stepped := (actor.advanceAI ledge distant false false 125).1
  let stacked := [2, 3, 4].map fun height =>
    let tiles := (List.range height).foldl (fun world offset => world.setTile ⟨51, 199 - (offset : Int)⟩ .stone) ground
    (actor.advanceAI tiles distant false false 125).1.vy
  check (stepped.y == 3144 && stepped.vx.toBits == 0x3f88f5c3 && stacked == [-6, -7, -8])
    "Angry Bones did not step up or select the matching jump for the obstacle height."
  let doorway := ground.setTile ⟨51, 196⟩ .stone |>.setTile ⟨51, 197⟩ .doorClosedTop
    |>.setTile ⟨51, 198⟩ .doorClosedMiddle |>.setTile ⟨51, 199⟩ .doorClosed
  let (waiting, closed) := { actor with ai2 := 58 }.advanceAI doorway distant false false 125
  let (recoil, opened) := { actor with ai2 := 59 }.advanceAI doorway distant false false 125
  check (waiting.ai2 == 59 && closed == doorway && recoil.vx == -0.5 && recoil.ai2 == 0 &&
    opened.tile ⟨51, 199⟩ == .doorOpen && opened.tile ⟨51, 197⟩ == .doorOpenTop)
    "Angry Bones opened the door at the wrong tick or lost its recoil."
  let fresh := newGame defaultSeed
  let room := (dungeonLayout fresh.seed).goldChest
  let actors := kinds.mapIdx fun i kind => AngryBones.atFloor (i + 3) kind (room.x + i) 88
  saveRoundTrip { fresh with angryBones := actors, nextAngryBonesId := 20 }

private def cursedSkullChecks : IO Unit := do
  -- The digests were computed by executing the installed 1.4.5.8 AI and NPC update branches.
  let player : Player := ⟨14643, 50846, 0, 0, true⟩
  for (dead, expected) in ([(false, 0x9238af7f7f3df77d), (true, 0x8a9df75f0ce87358)] : List (Bool × UInt64)) do
    let mut skull : CursedSkull := { id := 1, x := 800, y := 3160 }
    let mut trace : UInt64 := 14695981039346656037
    for _ in [:1350] do
      let some moved := skull.advance player dead | throw (IO.userError "Reference Cursed Skull despawned.")
      skull := moved
      let words := #[skull.x.toBits, skull.y.toBits, skull.vx.toBits, skull.vy.toBits,
        skull.orbit.toBits, skull.rotation.toBits, UInt32.ofNat skull.clock, UInt32.ofNat skull.timeLeft,
        UInt32.ofNat skull.frame, UInt32.ofNat skull.frameClock, UInt32.ofInt skull.direction,
        UInt32.ofInt skull.spriteDirection, 1]
      for word in words do trace := (trace ^^^ word.toUInt64) * 1099511628211
    check (trace == expected) s!"Cursed Skull differed from the Classic trace (dead={dead}): {trace}."
  let fresh := newGame defaultSeed
  let center := fresh.player.centerTile
  let skull : CursedSkull := {
    id := 1
    x := (Int32.ofInt (bodyCenterX fresh.player)).toFloat32 / 16 - 13
    y := (Int32.ofInt (bodyCenterY fresh.player)).toFloat32 / 16 - 14 }
  let inventory := fresh.inventory.set! 0 (some ⟨.goldBow, 1⟩)
    |>.set! 3 (some ⟨.arrow, 3⟩) |>.set! 4 (some ⟨.nazar, 1⟩)
  let clock := ((List.range 90).map (· + fresh.surface.clock)).find? (fun clock =>
    (hash (fresh.seed, clock, fresh.player.x, fresh.player.y, (23 : Nat))).toNat % 3 == 0)
  let some clock := clock | throw (IO.userError "No controlled Classic curse roll.")
  let armed : GameState := { fresh with
    inventory, cursedSkulls := #[skull], nextCursedSkullId := 2
    surface := { fresh.surface with clock } }
  let aim := some { center with x := center.x + 4 }
  let contact := step armed { mine := aim }
  check (contact.cursedTicks == 240 && contact.combat.health < fresh.combat.health &&
    Inventory.count contact.inventory .arrow == 2)
    "Skull contact must inflict the curse after this frame's item lock was decided."
  saveRoundTrip contact
  let lastTick := { contact with cursedTicks := 1, cursedSkulls := #[], bowCooldown := 0 }
  let locked := step lastTick { mine := aim, horizontal := 1 }
  check (locked.cursedTicks == 0 && locked.player.x > lastTick.player.x &&
    Inventory.count locked.inventory .arrow == 2)
    "The last tick of Cursed must block the bow while still permitting movement."
  let expired := step locked { mine := aim }
  check (Inventory.count expired.inventory .arrow == 1) "Item use did not return after Cursed expired."
  let equipped := step contact { equipGear := some 4 }
  check (equipped.equipment.accessory == some .nazar && equipped.cursedTicks == 0)
    "Equipping Nazar did not purge Cursed."
  let immune := step { armed with equipment := equipped.equipment } {}
  check (immune.cursedTicks == 0 && immune.combat.health < fresh.combat.health)
    "Nazar should block the debuff while retaining contact damage."
  let fatal := step { armed with combat := { armed.combat with health := 1 }, cursedTicks := 4 } {}
  check (fatal.respawnTicks > 0 && fatal.cursedTicks == 0) "Cursed survived the player's death."
  let obstacle : TilePos := { center with x := center.x + 1 }
  let hidden : CursedSkull := { skull with x := skull.x + 24 }
  let melee : GameState := { armed with
    world := armed.world.setTile obstacle .stone
    inventory := inventory.set! 0 (some ⟨.copperSword, 1⟩)
    cursedSkulls := #[hidden] }
  let cut := step melee { mine := some obstacle }
  check (cut.cursedSkulls[0]?.any (·.health < cursedSkullHealth))
    "A sword did not hit the no-tile-collision skull through the obstacle."

private def dungeonChecks : IO Unit := do
  for potion in [Item.lesserHealingPotion, .healingPotion] do
    for originalPotionRoll in [5:16] do
      for originalHeartRoll in [:5] do
        let drops := bossRecoveryDrops potion (originalPotionRoll - 5) originalHeartRoll
        check (drops == #[⟨potion, originalPotionRoll⟩, ⟨.heart, 5 + originalHeartRoll⟩])
          "Boss recovery drops diverged from the original potion and heart draws."
  let fresh := newGame defaultSeed
  let layout := dungeonLayout fresh.seed
  let some surface := findStandingNear fresh.world layout.oldMan.x layout.surface 1 1 (fun _ => true)
    | throw (IO.userError "The Old Man's terrace has no standing space.")
  let some vault := findStandingNear fresh.world (layout.goldChest.x + 2) (layout.goldChest.y + 2) 2 2
      (fun player => inDungeon fresh.seed fresh.world player.centerTile)
    | throw (IO.userError "The lower Dungeon has no standing space.")
  let relocate := fun (state : GameState) (player : Player) =>
    { state with player, chart := state.chart.reveal state.world player.centerTile }
  let day := step (relocate fresh surface) { interact := some layout.oldMan }
  let refused := step day { curseOldMan := true }
  check (day.openOldMan && refused.openOldMan && refused.skeletron.isNone)
    "The Old Man summoned Skeletron during daytime."
  let night := { day with surface := { day.surface with clock := classicDayTicks + 900 } }
  let summoned := step night { curseOldMan := true }
  let some boss := summoned.skeletron | throw (IO.userError "The night curse did not summon Skeletron.")
  check (boss.health == skeletronMaxHealth && boss.targets.size == 3 && !summoned.openOldMan &&
    !summoned.oldManWaiting) "The night curse failed to initialize the head and hands."
  saveRoundTrip summoned
  let daybreak := step { summoned with surface := { summoned.surface with clock := classicCycleTicks - 1 } } {}
  let dawn := step daybreak {}
  check (daybreak.skeletron.any (·.phase == .hover) &&
    dawn.skeletron.any (fun current => current.phase == .enraged && current.headDamage == 9999))
    "Skeletron did not enrage on the first NPC update after the clock reached dawn."
  saveRoundTrip dawn
  let underground := relocate fresh vault
  let guarded := step underground { interact := some layout.goldChest }
  check (guarded.dungeonGuardian.isSome && guarded.dungeonGuardianHealth == dungeonGuardianMaxHealth &&
    guarded.openChest.isNone && guarded.world.tile layout.goldChest == .dungeonGoldChest)
    "Premature entry failed to spawn a guardian or incorrectly opened the chest."
  saveRoundTrip guarded
  let mut hunted := guarded
  for _ in [:90] do
    if hunted.respawnTicks > 0 then break
    hunted := step hunted {}
  check (hunted.combat.health == 0 && hunted.respawnTicks > 0 && hunted.dungeonGuardian.isNone)
    "A Dungeon Guardian did not kill the player on contact."
  let nearDawn := { hunted with
    oldManWaiting := false
    surface := { hunted.surface with clock := oldManReturnTick - 1 } }
  let returned := step nearDawn {}
  check (returned.oldManPosition == some layout.oldMan) "The Old Man missed his 6:30 AM return."
  let bomb : ActiveBomb := { x := boss.motion.x, y := boss.motion.y, vx := 0, vy := 0, fuse := 1, sticky := false }
  let beaten := step { summoned with skeletron := some { boss with health := 1 }, bombs := #[bomb] } {}
  let reward := Inventory.count beaten.inventory .healingPotion +
    (beaten.nightDrops.foldl (fun count drop =>
      count + drop.items.foldl (fun n stack => if stack.item == .healingPotion then n + stack.count else n) 0) 0)
  let hearts := beaten.nightDrops.foldl (fun count drop =>
    count + drop.items.foldl (fun n stack => if stack.item == .heart then n + stack.count else n) 0) 0
  check (beaten.skeletronDefeated && beaten.skeletron.isNone && beaten.oldManPosition.isNone &&
    5 ≤ reward && reward ≤ 15 && 5 ≤ hearts && hearts ≤ 9)
    "Defeating the head did not release the Old Man and drop potions and hearts."
  saveRoundTrip beaten
  let some alcove := findStandingNear beaten.world layout.keyChest.x layout.surface 1 1 (fun _ => true)
    | throw (IO.userError "The shallow key chest is unreachable.")
  let woodChest := step { relocate beaten alcove with combat := initialCombat beaten.world, respawnTicks := 0 }
    { interact := some layout.keyChest }
  let key := step woodChest { chestWithdraw := some 0 }
  check (Inventory.count key.inventory .goldenKey == 1) "The shallow chest did not contain a Golden Key."
  let opened := step { relocate key vault with openChest := none } { interact := some layout.goldChest }
  check (opened.openChest == some layout.goldChest && opened.dungeonGuardian.isNone &&
    Inventory.count opened.inventory .goldenKey == 0 &&
    (dungeonChestCells layout.goldChest).all (fun cell => opened.world.tile cell == .dungeonOpenChest))
    "Defeating Skeletron and using the Golden Key did not unlock the full chest."
  let shield := step opened { chestWithdraw := some 0 }
  let some shieldSlot := shield.inventory.findIdx? (·.any (·.item == .cobaltShield))
    | throw (IO.userError "The first locked chest did not contain a Cobalt Shield.")
  let equipped := step { shield with openChest := none } { equipGear := some shieldSlot }
  check (equipped.equipment.accessory == some .cobaltShield && equipped.defense == shield.defense + 1)
    "The Cobalt Shield did not equip with one defense."
  saveRoundTrip equipped

private def lowerDungeonChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let layout := dungeonLayout fresh.seed
  let anchor := layout.lowerGoldChest
  check (fresh.lowerDungeonInstalled && anchor.y ≥ (layout.surface : Int) + 50 &&
    dungeonChestAnchor fresh.seed ⟨anchor.x + 1, anchor.y + 1⟩ true == some anchor &&
    dungeonChestAnchor fresh.seed anchor == none)
    "The lower Dungeon gold chest is outside its generated depth or footprint."
  let some player := findStandingNear fresh.world (anchor.x + 2) dungeonLowerFloor 1 0
      (fun player => inDungeon fresh.seed fresh.world player.centerTile true)
    | throw (IO.userError "No walkable floor by the new locked chest.")
  let nearby : GameState := { fresh with
    player, chart := fresh.chart.reveal fresh.world player.centerTile
    skeletronDefeated := true, oldManWaiting := false }
  let blocked := step nearby { interact := some anchor }
  check (blocked.openChest.isNone && blocked.world.tile anchor == .dungeonGoldChest)
    "The new locked chest opened without a key."
  let some (inventory, _) := Inventory.addItems nearby.inventory .goldenKey 2
    | throw (IO.userError "Could not add the lock test keys.")
  let unlocked := step { nearby with inventory } { interact := some ⟨anchor.x + 1, anchor.y + 1⟩ }
  check (unlocked.openChest == some anchor && Inventory.count unlocked.inventory .goldenKey == 1 &&
    unlocked.world.tile layout.goldChest == .dungeonGoldChest &&
    (dungeonChestCells anchor).all (fun cell => unlocked.world.tile cell == .dungeonOpenChest))
    "Opening the new chest did not spend exactly one key or changed the old chest."
  let looted := step unlocked { chestWithdraw := some 0 }
  let closed := step looted { closeChest := true }
  let reopened := step closed { interact := some anchor }
  check (Inventory.count looted.inventory .aquaScepter == 1 && reopened.openChest == some anchor &&
    Inventory.count reopened.inventory .goldenKey == 1)
    "The Aqua Scepter or re-opening an unlocked chest changed the key count."
  let ready := step reopened { closeChest := true }
  let some slot := ready.inventory.findIdx? (·.any (·.item == .aquaScepter))
    | throw (IO.userError "The new chest yielded no usable scepter.")
  let aim : TilePos := ⟨player.centerTile.x + 8, player.centerTile.y - 1⟩
  let cast := step { ready with manaRegenProgress := 0 } { selectSlot := some slot, mine := some aim }
  check (cast.mana == 13 && cast.aquaAnimation == 15 && cast.aquaShotDelay == 8 &&
    cast.aquaStreams.any (·.age == 3))
    "Aqua Scepter's first shot, initial mana cost, or extra updates differ."
  let mut charging := cast
  for _ in [:7] do charging := step charging {}
  check (!charging.aquaStreams.any (·.age == 3) && charging.aquaShotDelay == 1)
    "The second stream fired before eight ticks."
  let second := step charging {}
  check (second.mana == 13 && second.aquaStreams.any (·.age == 3) && second.aquaShotDelay == 8)
    "Releasing the button cancelled the second stream or paid mana twice."
  saveRoundTrip second
  let slow := step { ready with mana := 6, manaRegenProgress := 0 }
    { selectSlot := some slot, mine := some aim }
  check (slow.mana == 0 && slow.aquaAnimation == 39 && slow.aquaShotDelay == 20 &&
    slow.aquaStreams.any (·.age == 3))
    "A low-mana cast did not use the original 2.5× timing."
  check (regenerateMana 13 4 0 true == (13, 2, 2) &&
    regenerateMana 13 1 115 true == (14, 0, 6))
    "Mana regeneration lost its delay, truncation, or fractional progress."
  let empty : World := ⟨128, 128, ByteArray.mk (Array.replicate (128 * 128) Tile.air.code)⟩
  let mut stream : AquaStream := { x := 500, y := 500, vx := 12.5, vy := 0 }
  for age in [1:101] do
    let some moved := stream.advance empty | throw (IO.userError "Aqua Stream expired early.")
    stream := moved
    if age == 4 then check (stream.y == 500 && stream.vy == 0) "The first four stream updates must be straight."
    if age == 5 then check (stream.vy.toBits == 0x3e19999a) "The water arc gravity changed."
  check (stream.age == 100 && (stream.advance empty).isNone) "Aqua Stream did not expire on update 101."
  let magma := Magma.fillFromEighths (Water.empty empty) empty #[(⟨31, 31⟩, 1)]
  let approach : AquaStream := { x := 500, y := 495, vx := 0, vy := 3, age := 4, lavaSeed := 12345 }
  let some entered := approach.advance empty magma
    | throw (IO.userError "An Aqua Stream expired on entering a lava tile.")
  check (entered.tile == ⟨31, 31⟩ && entered.lavaBounceAge.isNone)
    "Aqua Stream checked lava after movement instead of before it."
  let some bounced := entered.advance empty magma
    | throw (IO.userError "A descending Aqua Stream did not bounce in lava.")
  check (bounced.lavaBounceAge == some 6 && !bounced.steam && bounced.vx.toBits == 0x3f9da920 &&
    bounced.vy.toBits == 0xc09d9306 && bounced.hitsLeft == entered.hitsLeft)
    "The original seeded rebound or gravity on the conversion update changed."
  let some steam := bounced.advance empty magma
    | throw (IO.userError "Aqua Stream expired when the steam branch started.")
  check (steam.steam && steam.vy.toBits == 0xc09b2ca0 && steam.scale.toBits == bounced.scale.toBits)
    "Steam did not keep its scale and switch gravity on the following subupdate."
  let rising := { entered with vy := -1 }
  check ((rising.advance empty magma).any (·.lavaBounceAge.isNone)) "An ascending Aqua Stream bounced."
  let late := { entered with age := 99, lavaSeed := 0, hitsLeft := 1 }
  let some lateBounce := late.advance empty magma
    | throw (IO.userError "Aqua Stream lost the last possible lava bounce.")
  let some lateSteam := lateBounce.advance empty magma
    | throw (IO.userError "The last lava bounce did not extend the stream's life.")
  check (lateSteam.steam && lateSteam.age == 101 && lateSteam.lavaBounceAge == some 100 &&
    lateSteam.scale.toBits == 0x35340000 && lateSteam.vy.toBits == 0xc0719999 &&
    ({ late with age := 100 }.advance empty magma).isNone)
    "The shrink boundary or seed-zero rebound changed."
  let pool := Magma.fillFromEighths fresh.magma fresh.world #[(⟨68, 112⟩, 8)]
  let probe := { approach with x := 1096, y := 1788, vy := 2 }
  let converted := step { fresh with magma := pool, aquaStreams := #[probe] } {}
  check (converted.aquaStreams.any fun shot => shot.age == 7 && shot.lavaBounceAge == some 7 && !shot.steam)
    "The probe did not convert on its last scheduled subupdate."
  saveRoundTrip converted
  let scheduled := step converted {}
  let slowed := step scheduled {}
  check (scheduled.aquaStreams.any (fun shot => shot.age == 10 && shot.steam) &&
    slowed.aquaStreams.any (fun shot => shot.age == 11 && shot.steam))
    "Changing extra updates did not preserve the already scheduled subupdates."
  saveRoundTrip slowed
  let offsetAim : WorldAim := { WorldAim.atTile aim with y := aim.y * tileUnits + 28 }
  let precise := step { ready with manaRegenProgress := 0 }
    { selectSlot := some slot, mine := some aim, aim := some offsetAim }
  check (precise.aquaBurst == some offsetAim && precise.aquaStreams != cast.aquaStreams)
    "An exact mouse position within a tile collapsed to the tile-center aim."
  saveRoundTrip precise

private def npcImmunityChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let some player := findStandingNear fresh.world 67 dungeonLowerFloor 0 0 (fun _ => true)
    | throw (IO.userError "No walkable floor for the projectile immunity fixture.")
  let foe := AngryBones.atFloor 1 .bigHelmet 68 dungeonLowerFloor
  let state : GameState := { fresh with
    player, chart := fresh.chart.reveal fresh.world player.centerTile
    skeletronDefeated := true, oldManWaiting := false
    combat := { fresh.combat with invulnerable := 40 }
    angryBones := #[foe], nextAngryBonesId := 2 }
  let health (s : GameState) := (s.angryBones.find? (·.id == 1)).map (·.health)
  let target := NpcTarget.angryBones 1
  let aqua : AquaStream := { x := foe.centerX - 12.5, y := foe.centerY, vx := 12.5, vy := 0, slot := some 0 }
  let x := (foe.centerX * 16).toInt32.toInt
  let y := (foe.centerY * 16).toInt32.toInt
  let arrow : Projectile := {
    x := x - 226, y, vx := 226, vy := 0, life := ShotKind.unholyArrow.lifetime
    kind := .unholyArrow, hitsLeft := 5, slot := some 1 }
  let waterFirst := step { state with aquaStreams := #[aqua], projectiles := #[arrow] } {}
  check (health waterFirst == some 100 && waterFirst.npcImmunity == #[(target, 10)] &&
    waterFirst.projectiles.any (·.hitsLeft == 5))
    "A later piercing arrow consumed a hit or damaged an enemy already struck by water."
  let arrowFirst := step { state with
    aquaStreams := #[{ aqua with slot := some 1 }], projectiles := #[{ arrow with slot := some 0 }] } {}
  check (health arrowFirst == some 111 && arrowFirst.npcImmunity == #[(target, 10)] &&
    arrowFirst.aquaStreams.any (·.hitsLeft == 5))
    "Projectile family order overrode the lower occupied slot."
  saveRoundTrip waterFirst
  let overlapping := step { state with
    angryBones := #[foe, { foe with id := 2 }], nextAngryBonesId := 3, projectiles := #[arrow] } {}
  check (overlapping.angryBones.map (·.health) == #[111, 112] &&
    overlapping.npcImmunity == #[(target, 10), (.angryBones 2, 10)] &&
    overlapping.projectiles.any (·.hitsLeft == 3))
    "One piercing arrow did not attenuate damage across two overlapping enemies in the same update."
  let skipped := step { state with aquaStreams := #[{ aqua with hitsLeft := 1 }], npcImmunity := #[(target, 2)] } {}
  check (health skipped == some 120 && skipped.npcImmunity == #[(target, 1)] &&
    skipped.aquaStreams.any (·.hitsLeft == 1))
    "The last hit bypassed existing immunity or reduced its timer in each subupdate."
  let finalHit := step { state with aquaStreams := #[{ aqua with hitsLeft := 1 }], npcImmunity := #[(target, 1)] } {}
  check (health finalHit == some 100 && finalHit.aquaStreams.isEmpty && finalHit.npcImmunity.isEmpty)
    "The last hit failed to become eligible after the NPC update or gave fresh immunity."
  let bullet : Projectile := { arrow with x := x - 416, vx := 416, kind := .musketBullet, hitsLeft := 1 }
  let ordinary := step { state with projectiles := #[bullet], npcImmunity := #[(target, 10)] } {}
  check (health ordinary == some 89 && ordinary.npcImmunity == #[(target, 9)] && ordinary.projectiles.isEmpty)
    "A single-hit bullet consumed or was blocked by existing immunity."
  let aim : TickInput := { mine := some ⟨68, foe.centerY.toInt32.toInt / 16⟩ }
  let sword := { state with inventory := state.inventory.set! 0 (some ⟨.woodSword, 1⟩) }
  let blocked := step { sword with npcImmunity := #[(target, 1)] } aim
  let swung := step sword aim
  let thrust := step { state with npcImmunity := #[(target, 1)] } aim
  check (health blocked == some 120 && blocked.npcImmunity.isEmpty &&
    (health swung).any (· < 120) && swung.npcImmunity.isEmpty)
    "A direct swing ignored the last immunity frame or wrote shared projectile immunity."
  check ((health thrust).any (· < 120) && thrust.npcImmunity == #[(target, 10)])
    "The Copper Shortsword did not resolve after the NPC's cooldown tick."

private def shortswordChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let some player := findStandingNear fresh.world 67 dungeonLowerFloor 0 0 (fun _ => true)
    | throw (IO.userError "No walkable floor for the Copper Shortsword fixture.")
  let foe := AngryBones.atFloor 1 .bigHelmet 70 dungeonLowerFloor
  let initial : GameState := { fresh with
    player, chart := fresh.chart.reveal fresh.world player.centerTile
    skeletronDefeated := true, oldManWaiting := false
    combat := { fresh.combat with invulnerable := 40 }
    angryBones := #[foe], nextAngryBonesId := 2 }
  let health (s : GameState) := (s.angryBones.find? (·.id == 1)).map (·.health)
  let aim : WorldAim := ⟨bodyCenterX player + 4 * tileUnits, bodyCenterY player⟩
  let input : TickInput := { mine := some ⟨71, 115⟩, aim := some aim }
  let launched := step initial input
  let some thrust := launched.shortsword | throw (IO.userError "The Copper Shortsword did not launch.")
  check (thrust.age == 2 && thrust.slot == some 0 && thrust.vx.toBits == 0x40066666 && thrust.vy == 0 &&
    health launched == some 120 && launched.combat.swing == 13) "The first shortsword frame changed."
  saveRoundTrip launched
  let moved := step launched { horizontal := 1, aim := some { aim with x := aim.x - 8 * tileUnits } }
  check (moved.player.x > launched.player.x && moved.shortsword == some { thrust with age := 4 })
    "Walking or moving the cursor replaced the active shortsword direction or slot."
  let mut attack := launched
  for frame in [2:8] do
    attack := step attack {}
    check (attack.shortsword == some { thrust with age := frame * 2 } &&
      health attack == some (if frame < 6 then 120 else 119))
      s!"Copper Shortsword reach or immunity changed on frame {frame}."
  check ((step attack {}).shortsword.isNone) "The shortsword survived its terminal subupdate."
  let vertical : WorldAim := ⟨bodyCenterX player, aim.y - 2 * tileUnits⟩
  let lifted := step initial { input with aim := some vertical }
  check (lifted.combat.facing == -1 && lifted.shortsword.any (fun blade =>
    blade.vx == 0 && blade.vy.toBits == 0xc0066666 && blade.spriteDirection == 1))
    "A vertical stab used the tile center or confused owner facing with projectile direction."
  let cobweb : TilePos := ⟨68, player.centerTile.y⟩
  let cutting := { initial with world := initial.world.setTile cobweb .cobweb, angryBones := #[] }
  let first := step cutting input
  check (first.world.tile cobweb == .cobweb) "The cursor shortcut cut a tile before the sword reached it."
  let cut ← walk first 7 0
  check (cut.world.tile cobweb == .air) "An extended shortsword did not cut the cobweb."

private def blueMoonChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let layout := dungeonLayout fresh.seed
  let chest := layout.blueMoonChest
  check (fresh.blueMoonDungeonInstalled && chest.y ≥ (layout.surface : Int) + 50 &&
    dungeonChestAnchor fresh.seed chest true true == some chest && dungeonChestAnchor fresh.seed chest true == none)
    "The Blue Moon chest is outside the new Dungeon branch or generated depth."
  let some player := findStandingNear fresh.world (chest.x + 3) dungeonBlueMoonFloor 1 0 (fun _ => true)
    | throw (IO.userError "No walkable floor at the Blue Moon chest.")
  let state : GameState := { fresh with
    player, chart := fresh.chart.reveal fresh.world player.centerTile
    skeletronDefeated := true, oldManWaiting := false
    combat := { fresh.combat with invulnerable := 60 } }
  let cell : TilePos := ⟨chest.x + 1, chest.y + 1⟩
  let locked := step state { interact := some cell }
  check (locked.openChest.isNone && locked.world.tile chest == .dungeonGoldChest)
    "Blue Moon's chest opened without a Golden Key."
  let some (inventory, _) := Inventory.addItems state.inventory .goldenKey 1
    | throw (IO.userError "Could not stage the dungeon key.")
  let unlocked := step { state with inventory } { interact := some cell }
  let looted := step unlocked { chestWithdraw := some 0 }
  let ready := step looted { closeChest := true }
  check (Inventory.count ready.inventory .goldenKey == 0 && Inventory.count ready.inventory .blueMoon == 1 &&
    ready.world.tile layout.lowerGoldChest == .dungeonGoldChest &&
    (dungeonChestCells chest).all (fun pos => ready.world.tile pos == .dungeonOpenChest))
    "Unlocking the Blue Moon chest affected the Aqua Scepter chest or the item transfer."
  let slot ← slotFor ready .blueMoon
  let foe := AngryBones.atFloor 1 .bigHelmet (ready.player.centerTile.x + 2) dungeonBlueMoonFloor
  let aim : WorldAim := ⟨bodyCenterX ready.player + 6 * tileUnits, bodyCenterY ready.player⟩
  let click : TickInput := {
    selectSlot := some slot
    mine := some ⟨aim.x / tileUnits, aim.y / tileUnits⟩
    aim := some aim
    useHeld := true }
  let mut attack := step { ready with angryBones := #[foe], nextAngryBonesId := 2 } click
  for _ in [:11] do attack := step attack { useHeld := true }
  check (attack.blueMoon.any (fun active => active.flail.phase == .spinning && active.flail.spinTicks == 12 &&
    !active.flail.canDamage && active.cooldowns.isEmpty) && attack.angryBones[0]?.any (·.health == 120))
    "The spinning flail could damage before its native startup elapsed."
  attack := step attack { useHeld := true }
  check (attack.blueMoon.any (fun active => active.flail.canDamage &&
    active.cooldowns.contains (.angryBones 1, 15)) && attack.angryBones[0]?.any (·.health == 95) &&
    attack.npcImmunity.isEmpty) "The active spin did not set its own 15-frame target cooldown."
  saveRoundTrip attack
  let thrown := step attack {}
  check (thrown.blueMoon.any (fun active => active.flail.phase == .outbound && active.flail.damage == 54 &&
    active.cooldowns.all (fun entry => entry.2 ≤ 10))) "Release did not throw the flail and clear spin immunity."
  let dropped := step thrown { useHeld := true }
  check (dropped.blueMoon.any (fun active => active.flail.phase == .dropped && active.flail.knockback == 3))
    "Pressing again did not drop the flail."
  let returning := step dropped {}
  check (returning.blueMoon.any (fun active => active.flail.phase == .forcedReturn))
    "Releasing a dropped flail did not start forced return."
  let returned ← walk returning 36 0
  check returned.blueMoon.isNone "The flail did not retract or release its projectile slot."

private def magicMissileChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let layout := dungeonLayout fresh.seed
  let chest := layout.magicMissileChest
  check (fresh.magicMissileDungeonInstalled &&
    dungeonChestAnchor fresh.seed chest true true true == some chest &&
    dungeonChestAnchor fresh.seed chest true true == none) "The fifth Dungeon chest was outside its own wing."
  let some player := findStandingNear fresh.world (chest.x + 3) dungeonMagicMissileFloor 1 0 (fun _ => true)
    | throw (IO.userError "No walkable floor at the Magic Missile chest.")
  let standing : GameState := { fresh with
    player, chart := fresh.chart.reveal fresh.world player.centerTile
    skeletronDefeated := true, oldManWaiting := false
    combat := { fresh.combat with invulnerable := 60 } }
  let chestCell : TilePos := ⟨chest.x + 1, chest.y + 1⟩
  check ((step standing { interact := some chestCell }).openChest.isNone)
    "The Magic Missile chest accepted an inventory without a Golden Key."
  let some (inventory, _) := Inventory.addItems standing.inventory .goldenKey 1
    | throw (IO.userError "Could not stage the dungeon key.")
  let opened := step { standing with inventory } { interact := some chestCell }
  let looted := step opened { chestWithdraw := some 0 }
  let ready := step looted { closeChest := true }
  check (Inventory.count ready.inventory .goldenKey == 0 && Inventory.count ready.inventory .magicMissile == 1 &&
    ready.world.tile layout.blueMoonChest == .dungeonGoldChest)
    "The Magic Missile chest changed the previous chest or consumed the wrong key count."
  let slot ← slotFor ready .magicMissile
  let aim : WorldAim := ⟨bodyCenterX player - 8 * tileUnits, bodyCenterY player⟩
  let cast : TickInput := {
    selectSlot := some slot
    mine := some ⟨aim.x / tileUnits, aim.y / tileUnits⟩
    aim := some aim
    useHeld := true }
  let launched := step ready cast
  check (launched.mana == 6 && launched.bowCooldown == 22 && launched.magicMissileSound &&
    launched.magicMissiles[0]?.any (fun attack => attack.missile.guided && attack.missile.vx < 0))
    "The launch did not channel at native mana, timing, and aim."
  let revised : WorldAim := ⟨bodyCenterX player - 6 * tileUnits, bodyCenterY player - 4 * tileUnits⟩
  let steered := step launched { useHeld := true, aim := some revised }
  check (steered.mana == 6 && steered.magicMissiles[0]?.any (fun attack =>
    attack.missile.guided && attack.aim == revised && attack.missile.vy < 0 && attack.missile.trail.size == 2))
    "Holding the cast neither followed the revised aim nor retained its trail and mana."
  saveRoundTrip steered
  let released := step steered {}
  check (released.magicMissiles[0]?.any (fun attack => !attack.missile.guided))
    "Releasing the missile did not switch it to free flight."
  saveRoundTrip released
  let slow := step { ready with mana := 0, manaRegenProgress := 0 } cast
  check (slow.mana == 0 && slow.bowCooldown == 55 && slow.magicMissiles.size == 1)
    "ManaV2 did not produce the slower cast at zero mana."
  saveRoundTrip slow
  let direct := AngryBones.atFloor 1 .bigHelmet 25 dungeonMagicMissileFloor
  let nearby := AngryBones.atFloor 2 .bigHelmet 28 dungeonMagicMissileFloor
  let cobweb : TilePos := ⟨28, dungeonMagicMissileFloor - 1⟩
  let missile : MagicMissile := {
    x := direct.centerX, y := direct.centerY, vx := 0, vy := 0, life := 60, guided := false, slot := some 0 }
  let impact := step { ready with
    world := ready.world.setTile cobweb .cobweb
    angryBones := #[direct, nearby], nextAngryBonesId := 3
    magicMissiles := #[{ missile, aim, target := some (.angryBones 1) }]
    npcImmunity := #[(.angryBones 1, 10)] } {}
  check (impact.magicMissiles.isEmpty && impact.magicMissileFlashes.size == 1 &&
    impact.angryBones.all (·.health == 92) &&
    impact.npcImmunity == #[(.angryBones 1, 8), (.angryBones 2, 8)] &&
    impact.world.tile cobweb == .air && impact.world == ready.world)
    "The blast failed to cut a cobweb, shared immunity was wrong, or solid terrain changed."
  saveRoundTrip impact

private def deepDungeonChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let layout := dungeonLayout fresh.seed
  check (fresh.deepDungeonInstalled &&
    fresh.world.tile layout.valorChest == .dungeonGoldChest &&
    fresh.world.tile layout.deepKeyChest == .dungeonKeyChest &&
    fresh.world.tile layout.handgunChest == .dungeonGoldChest &&
    dungeonChestAnchor fresh.seed layout.handgunChest true true true == none)
    "The last three Dungeon chests did not have the right anchors and lock styles."
  let atChest (state : GameState) (anchor : TilePos) (side : Int) : IO GameState := do
    let some player := findStandingNear state.world (anchor.x + side) dungeonDeepFloor 1 0 (fun _ => true)
      | throw (IO.userError "No walkable floor at the eastern Dungeon chest.")
    return { state with player, chart := state.chart.reveal state.world player.centerTile }
  let corner (anchor : TilePos) : TilePos := ⟨anchor.x + 1, anchor.y + 1⟩
  let state : GameState := { fresh with
    skeletronDefeated := true, oldManWaiting := false
    combat := { fresh.combat with invulnerable := 70 } }
  let standing ← atChest state layout.valorChest 3
  check ((step standing { interact := some (corner layout.valorChest) }).openChest.isNone)
    "The Valor chest accepted an inventory without a Golden Key."
  let some (inventory, _) := Inventory.addItems standing.inventory .goldenKey 1
    | throw (IO.userError "Could not stage the dungeon key.")
  let unlocked := step { standing with inventory } { interact := some (corner layout.valorChest) }
  let equipped := step (step unlocked { chestWithdraw := some 0 }) { closeChest := true }
  let slot ← slotFor equipped .valor
  let aim : WorldAim := ⟨bodyCenterX equipped.player + 8 * tileUnits, bodyCenterY equipped.player - 2 * tileUnits⟩
  let cast : TickInput := {
    selectSlot := some slot
    mine := some ⟨aim.x / tileUnits, aim.y / tileUnits⟩
    aim := some aim
    useHeld := true }
  let launched := step equipped cast
  check (launched.valorCast && launched.valor.any (fun attack =>
    !attack.yoyo.returning && attack.yoyo.vx > 0 && attack.yoyo.age == 1) &&
    Inventory.count launched.inventory .goldenKey == 0) "Valor did not cast or the first chest charged the wrong key count."
  let mut deployed := launched
  for _ in [:6] do deployed := step deployed { useHeld := true }
  saveRoundTrip deployed
  let returned := step deployed {}
  check (returned.valor.any (·.yoyo.returning)) "Releasing the mouse did not send out-of-reach Valor back."
  saveRoundTrip returned
  let caught ← walk returned 40 0
  check caught.valor.isNone "Valor did not finish returning to the player."
  let some attack := deployed.valor | throw (IO.userError "No deployed Valor for the time-limit test.")
  let elapsed := step { deployed with valor := some { attack with yoyo := { attack.yoyo with age := 660 } } }
    { useHeld := true }
  check (elapsed.valor.any (·.yoyo.returning)) "Holding past 660 ticks did not start the timed return."
  let withValor ← atChest caught layout.deepKeyChest 3
  let free := step withValor { interact := some (corner layout.deepKeyChest) }
  check (free.openChest == some layout.deepKeyChest && Inventory.count free.inventory .goldenKey == 0)
    "The wooden chest did not open freely."
  let withKey := step (step free { chestWithdraw := some 0 }) { closeChest := true }
  let atGun ← atChest withKey layout.handgunChest (-2)
  let handgun := step atGun { interact := some (corner layout.handgunChest) }
  let armed := step (step handgun { chestWithdraw := some 0 }) { closeChest := true }
  let gunSlot ← slotFor armed .handgun
  check (Inventory.count armed.inventory .goldenKey == 0 && armed.world.tile layout.deepKeyChest == .dungeonKeyChest)
    "The wooden key did not unlock only the Handgun chest."
  let aim : WorldAim := ⟨bodyCenterX armed.player - 8 * tileUnits, bodyCenterY armed.player⟩
  let fire : TickInput := {
    selectSlot := some gunSlot
    mine := some ⟨aim.x / tileUnits, aim.y / tileUnits⟩
    aim := some aim }
  let dry := step armed fire
  check (!dry.handgunFired && dry.bowCooldown == 0 && dry.status == "No Musket Balls.")
    "The Handgun fired without ammunition."
  let some (inventory, _) := Inventory.addItems armed.inventory .musketBall 3
    | throw (IO.userError "Could not stage Handgun rounds.")
  let fired := step { armed with inventory } fire
  check (fired.handgunFired && fired.bowCooldown == 15 && Inventory.count fired.inventory .musketBall == 2 &&
    fired.projectiles.any (fun shot => shot.kind == .handgunBullet && shot.damage == 33 && shot.life == 598 &&
      shot.vx == -224 && shot.vy == 0 && shot.x == bodyCenterX armed.player - 448))
    "The Handgun did not spend one round and make two 14-pixel subupdates."
  saveRoundTrip fired
  let blocked := step fired fire
  check (!blocked.handgunFired && Inventory.count blocked.inventory .musketBall == 2)
    "A repeated click fired before the Handgun cooldown elapsed."

private def underworldChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let dungeon := dungeonLayout fresh.seed
  let layout := underworldLayout fresh.seed
  check (fresh.world.height == underworldHeight && (fresh.chestAt dungeon.goldChest).any
    (fun chest => chest.slots[1]? == some (some ⟨.shadowKey, 1⟩)))
    "The first locked Dungeon chest did not contain the Underworld key."
  let mut state := fresh
  for anchor in layout.shadowChests do
    let some player := findStandingNear state.world (anchor.x + 2) (anchor.y + 2) 2 0 (fun _ => true)
      | throw (IO.userError "No floor next to the Shadow Chest.")
    let standing := { state with player, chart := state.chart.reveal state.world player.centerTile }
    let corner : TilePos := ⟨anchor.x + 1, anchor.y + 1⟩
    let click : TickInput := { interact := some corner }
    check ((step standing click).openChest.isNone) "The Shadow Chest opened without its key."
    let unlocked := step { standing with cursorStack := some ⟨.shadowKey, 1⟩ } click
    check (unlocked.openChest == some anchor && unlocked.cursorStack == some ⟨.shadowKey, 1⟩ &&
      (dungeonChestCells anchor).all (fun cell => unlocked.world.tile cell == .shadowOpenChest))
      "A Shadow Chest consumed the key or opened only part of its frame."
    state := step (step { unlocked with cursorStack := none } { chestWithdraw := some 0 }) { closeChest := true }
  let weapons : Array Item := #[.darkLance, .sunfury, .flowerOfFire, .flamelash, .hellwingBow]
  check (layout.shadowChests.size == weapons.size && weapons.all (fun item => Inventory.count state.inventory item == 1))
    "The Shadow Chest cycle repeated or lost one of its five primary items."
  saveRoundTrip state
  let cast (state : GameState) (slot : Nat) : TickInput :=
    let aim : WorldAim := ⟨bodyCenterX state.player + 3 * tileUnits, bodyCenterY state.player⟩
    { selectSlot := some slot, aim := some aim
      mine := some ⟨aim.x / tileUnits, aim.y / tileUnits⟩, useHeld := true }
  let lanceSlot ← slotFor state .darkLance
  let lance := step state (cast state lanceSlot)
  check (lance.underworldSwing && lance.darkLance.any (·.age == 1)) "Dark Lance did not begin its thrust."
  saveRoundTrip lance
  let sunfurySlot ← slotFor state .sunfury
  let some (inventory, _) := Inventory.addOne state.inventory .blueMoon
    | throw (IO.userError "Could not equip the second flail.")
  let paired := step { state with inventory } (cast state sunfurySlot)
  check (paired.underworldSwing && paired.blueMoon.any (·.flail.slot == some 0) &&
    paired.sunfury.any (·.flail.slot == some 1))
    "The companion Blue Moon did not take the first projectile slot."
  saveRoundTrip paired

  let flowerSlot ← slotFor state .flowerOfFire
  let flower := step state (cast state flowerSlot)
  check (flower.underworldMagicCast && flower.mana == state.mana - 9 && flower.bowCooldown == 16 &&
    flower.flowerFireballs.any (fun ball => ball.life == flowerFireLife - 1 && ball.bounces == 0 && ball.vx == 7.5))
    "The Flower of Fire spent the wrong mana or missed its original launch speed."
  saveRoundTrip flower
  let flameSlot ← slotFor state .flamelash
  let guided := step state (cast state flameSlot)
  check (guided.underworldMagicCast && guided.mana == 0 && guided.bowCooldown == 75 &&
    guided.flamelashes.any (fun attack => attack.projectile.missile.guided && attack.projectile.hitsLeft == 2))
    "Flamelash did not enter its held state or apply the low-mana delay."
  saveRoundTrip guided
  let released := step guided {}
  check (released.flamelashes.any (fun attack => !attack.projectile.missile.guided))
    "Flamelash kept following the cursor after release."
  saveRoundTrip released

  let bowSlot ← slotFor state .hellwingBow
  let aim := (cast state bowSlot).aim
  let ammo := state.inventory.set! Inventory.ammoStart (some ⟨.arrow, 3⟩)
  let fired := step { state with inventory := ammo } (cast state bowSlot)
  check (fired.hellwingFired && fired.bowCooldown == 13 &&
    Inventory.count fired.inventory .arrow == 2 && fired.hellwingShots.any (fun shot =>
      !shot.unholy && shot.damage == 27 && shot.life == hellwingShotLife - 1 && shot.hits == 0))
    "Hellwing Bow failed to convert exactly one wooden arrow into a bat."
  saveRoundTrip fired
  let mut autofire := fired
  for _ in [:13] do autofire := step autofire { aim, useHeld := true }
  check (autofire.hellwingFired && Inventory.count autofire.inventory .arrow == 1)
    "Holding Hellwing Bow did not fire again on tick 13."
  let ammo := state.inventory.set! Inventory.ammoStart (some ⟨.unholyArrow, 2⟩)
  let normal := step { state with inventory := ammo } (cast state bowSlot)
  check (normal.hellwingFired && Inventory.count normal.inventory .unholyArrow == 1 &&
    normal.hellwingShots.any (fun shot => shot.unholy && shot.damage == 34))
    "Hellwing Bow converted an Unholy Arrow instead of retaining the ammunition type."
  saveRoundTrip normal

  let arena : World := {
    width := 32, height := 20
    tiles := ByteArray.mk (Array.replicate (32 * 20) Tile.air.code) }
  let dry := Water.empty arena
  let wall := arena.setTile ⟨12, 6⟩ .stone
  let fireball : FlowerFireball := { x := 180, y := 104, vx := 7.5, vy := 0 }
  let (bounced, impact) := fireball.advance wall dry dry
  check (impact && bounced.any (fun ball => ball.bounces == 1 && ball.vx == -7.5 && ball.x == 172.5))
    "A fireball striking a wall did not use the reflected velocity in its collision tick."
  check (({ fireball with bounces := 5 } : FlowerFireball).advance wall dry dry == (none, true))
    "A fireball survived its sixth terrain contact."
  let platform := arena.setTile ⟨11, 8⟩ .platform
  let falling : FlowerFireball := { x := 184, y := 113.5, vx := 0, vy := 7.5 }
  let (bounced, _) := falling.advance platform dry dry
  check (bounced.any (fun ball => ball.bounces == 1 && ball.vy == -6))
    "A fireball did not rebound off the platform using the clipped vertical displacement."
  let unholy : HellwingShot := {
    x := 160, y := 100, vx := 9.4, vy := 0, goalVx := 9.4, goalVy := 0
    life := hellwingShotLife - 4, unholy := true }
  let beforeFall := unholy.advance arena dry dry
  let falling := beforeFall.bind (·.advance arena dry dry)
  check (beforeFall.any (·.vy == 0) && falling.any (·.vy == 0.1))
    "A Hellwing-fired Unholy Arrow did not seed its gravity timer with the wider launch scatter."

  let upper : TilePos := ⟨49, underworldStart⟩
  let lower : TilePos := ⟨52, underworldStart + 1⟩
  let enclose (world : World) (cell : TilePos) : World :=
    world.setTile cell .air |>.setTile ⟨cell.x, cell.y + 1⟩ .ash
      |>.setTile ⟨cell.x - 1, cell.y⟩ .ash |>.setTile ⟨cell.x + 1, cell.y⟩ .ash
  let world := enclose (enclose fresh.world upper) lower
  let levels := fresh.water.levels.set! (upper.y.toNat * world.width + upper.x.toNat) 120
    |>.set! (lower.y.toNat * world.width + lower.x.toNat) 120
  let clock := fresh.surface.clock / 4 * 4 + 3
  let watered := step { fresh with world, water := { levels }, surface := { fresh.surface with clock } } {}
  check (watered.water.level world upper == 120 && watered.water.level world lower == 118)
    "Water evaporated on the boundary or failed to evaporate below it."
  check (#[0, 7, 8, 15, 16, 23, 24, 31, 32, 39].map underworldSpawnRate ==
      #[360, 360, 420, 420, 480, 480, 540, 540, 600, 600])
    "Underworld population bands use the wrong NPC slot thresholds."

  let some (inventory, _) := Inventory.addOne fresh.inventory .demonScythe
    | throw (IO.userError "Could not equip the Demon Scythe.")
  let scytheSlot ← slotFor { fresh with inventory } .demonScythe
  let aim : WorldAim := ⟨bodyCenterX fresh.player + 8 * tileUnits, bodyCenterY fresh.player - 4 * tileUnits⟩
  let scythe := step { fresh with inventory, mana := 20 }
    { selectSlot := some scytheSlot, aim := some aim, mine := some ⟨aim.x / tileUnits, aim.y / tileUnits⟩ }
  check (scythe.underworldNpcSound && scythe.mana == 6 && scythe.bowCooldown == 20 &&
    scythe.demonSickles.any (fun shot =>
      let speedSquared := shot.vx * shot.vx + shot.vy * shot.vy
      !shot.hostile && (speedSquared - 0.04).abs < 0.000001 && shot.life == demonSickleFriendlyLife - 1))
    "Demon Scythe spent the wrong mana or changed its slow launch."
  saveRoundTrip scythe
  let shot := DemonSickle.fire 160 100 1 0 1 true
  let (at29, _) := (({ shot with life := demonSickleHostileLife - 28 }).advance arena dry dry).getD (shot, true)
  let (at30, _) := (at29.advance arena dry dry).getD (shot, true)
  let (at100, _) := (({ shot with life := demonSickleHostileLife - 99 }).advance arena dry dry).getD (shot, true)
  check (at29.vx == 0.2 && at30.vx == (0.2 : Float32) * 1.06 && at100.vx == 0.2)
    "The Demon Sickle accelerated outside ages 30 through 99."

  let ores : EarlyOreSet := { earlyOres fresh.seed with silver := .tungstenOre, gold := .platinumOre }
  let drops := shadowChestExtras ores (fun i =>
    match i with
    | 6 | 9 | 13 | 14 | 16 | 17 | 21 | 23 => 1
    | 7 => 14
    | 10 => 24
    | 12 => 5
    | 24 => 14
    | 26 => 2
    | 28 => 13
    | _ => 0)
  let expected : Array Stack := #[⟨.treasureMagnet, 1⟩, ⟨.demonicHellcart, 1⟩, ⟨.ornateShadowKey, 1⟩,
    ⟨.sliceOfHellCake, 1⟩, ⟨.dynamite, 1⟩, ⟨.platinumBar, 29⟩, ⟨.tungstenBullet, 74⟩,
    ⟨.restorationPotion, 20⟩, ⟨.featherfallPotion, 1⟩, ⟨.thornsPotion, 1⟩, ⟨.potionOfReturn, 1⟩,
    ⟨.glowstick, 29⟩, ⟨.goldCoin, 4⟩, ⟨.fairyChoker, 1⟩]
  check (drops == expected) "Shadow Chest extras missed the original order, draw thresholds or ore tiers."
  check (fresh.furniture.any (fun forge => forge.kind == .hellforge && forge.intact fresh.world))
    "The generated Ruined Houses have no complete Hellforge."
  let some forge := fresh.furniture.find? (·.kind == .hellforge)
    | throw (IO.userError "No forge for the Lavaproof Bug Net.")
  let some player := findStandingNear fresh.world forge.position.x (forge.position.y + 1) 1 0 (fun _ => true)
    | throw (IO.userError "Could not approach a Hellforge.")
  let some inventory := (Inventory.addItems fresh.inventory .hellstone 45).bind (fun (inventory, _) =>
      (Inventory.addItems inventory .obsidian 15).map Prod.fst)
    | throw (IO.userError "Could not stage Hellstone and Obsidian.")
  let some inventory := (Inventory.creditCoins inventory 5000)
    | throw (IO.userError "Could not stage merchant payment.")
  let (inventory, stock) ← IO.ofExcept ((buyStock inventory fresh.merchantStock shopOffers.size false).mapError IO.userError)
  let (inventory, _) ← IO.ofExcept ((buyStock inventory stock shopOffers.size false).mapError IO.userError)
  let mut working : GameState := { fresh with
    player, inventory, chart := fresh.chart.reveal fresh.world player.centerTile }
  for _ in [:15] do working := step working { craftClassic := some .hellstoneBar }
  working := step working { craftClassic := some .lavaproofBugNet }
  check (Inventory.count working.inventory .hellstoneBar == 0 &&
    Inventory.count working.inventory .lavaproofBugNet == 1 && Inventory.count working.inventory .bugNet == 1)
    "Hellforge smelting, the unlimited merchant offer, or Lavaproof Bug Net crafting failed."
  let netSlot ← slotFor working .lavaproofBugNet
  let plainSlot ← slotFor working .bugNet
  let critter := UnderworldCritter.atFloor 1 .magmaSnail (player.centerTile.x + 1) (forge.position.y + 1)
  let active : GameState := { working with underworldCritters := #[critter], nextUnderworldNpcId := 2 }
  let mut wrongNet := step active (cast active plainSlot)
  let mut caught := step active (cast active netSlot)
  for _ in [:25] do
    wrongNet := step wrongNet {}
    caught := step caught {}
  check (wrongNet.underworldCritters.any (·.id == 1) && wrongNet.combat.onFire > 0)
    "A basic Bug Net caught the Magma Snail or omitted the original burn."
  check (caught.underworldCritters.all (·.id != 1) && Inventory.count caught.inventory .magmaSnail == 1)
    "The crafted Lavaproof Bug Net failed to capture a Magma Snail."
  saveRoundTrip caught

private def classicTownChecks : IO Unit := do
  let fresh := newGame defaultSeed
  let some guide := fresh.townGuide | throw (IO.userError "A new world has no Guide.")
  check (fresh.surface.clock == classicStartClock && fresh.guide.isNone &&
    !isNight (classicDayTicks - 1) false && isNight classicDayTicks false &&
    !isNight classicCycleTicks false) "Classic dawn, dusk or the 8:15 start were wrong."
  let talking := step fresh { interact := some guide.body.centerTile }
  let closed := step talking { closeGuide := true }
  check (talking.openGuide && !closed.openGuide && closed.surface.clock == classicStartClock + 2)
    "Talking to the Guide froze the world or the dialogue could not close."

  let mut world := fresh.world
  let mut walls := fresh.walls
  for x in [14:26] do
    world := world.setTile ⟨x, 35⟩ .timber |>.setTile ⟨x, 40⟩ .timber
  for y in [36:40] do
    world := world.setTile ⟨14, y⟩ .timber |>.setTile ⟨25, y⟩ .air
    for x in [15:25] do
      world := world.setTile ⟨x, y⟩ .air
      walls := walls.set! (y * world.width + x) 1
  world := world.setTile ⟨25, 36⟩ .timber |>.setTile ⟨16, 39⟩ .workbench |>.setTile ⟨15, 37⟩ .torch
  let some player := findStandingNear world 21 40 0 0 (fun _ => true)
    | throw (IO.userError "No floor in the housing fixture.")
  let some (material, _) := Inventory.addItems fresh.inventory .wood 10
    | throw (IO.userError "Cannot add housing materials.")
  let mut state : GameState := { fresh with
    world, walls, player, inventory := material
    chart := fresh.chart.reveal world player.centerTile }
  state := step state { craftClassic := some .door }
  state ← buildWith state .door ⟨25, 39⟩ .doorClosed
  let base : TilePos := ⟨25, 39⟩
  let middle : TilePos := ⟨25, 38⟩
  check (doorCells state.world middle == #[base, middle, ⟨25, 37⟩] && state.guide.isNone &&
    !(inspectClassicHome state.world state.walls base).isOk)
    "A three-high door was incomplete or a room without comfort was accepted."
  state := step state { craftClassic := some .woodChair }
  state ← buildWith state .woodChair ⟨19, 39⟩ .woodChair
  check (state.homeDoor == some base && state.guide.isSome && state.townGuide.isSome &&
    Inventory.count state.inventory .wood == 0) "The Guide did not automatically claim a furnished room."
  let opened := step state { interact := some middle }
  check (opened.world.tile middle == .doorOpenMiddle && opened.guide == state.guide)
    "Opening a door by its middle invalidated housing or left a blocking tile."
  state := step opened { interact := some middle }
  saveRoundTrip state
  let pick ← equip state .copperPick
  let roof := step pick { mine := some ⟨25, 36⟩ }
  check (roof.world.tile ⟨25, 36⟩ == .timber && roof.tileHits.isEmpty)
    "Mining under a new door's overhead support created an unsupported door."
  let unseated := step roof { mine := some ⟨19, 39⟩ }
  check (unseated.guide.isNone && unseated.townGuide.isSome && unseated.world.tile ⟨19, 39⟩ == .air)
    "Removing the chair did not evict the Guide while keeping the NPC alive."

  let mut inventory := fresh.inventory
  for (item, quantity) in [(Item.bow, 1), (.arrow, 2)] do
    let some (updated, _) := Inventory.addItems inventory item quantity
      | throw (IO.userError "Cannot prepare bow combat.")
    inventory := updated
  let eyeBody : Player := ⟨25 * tileUnits, 37 * tileUnits, 0, 0, false⟩
  let eye : NightCreature := ⟨eyeBody, 8, 0, .demonEye⟩
  let archer : GameState := { fresh with
    inventory
    townGuide := none
    surface := { clock := classicDayTicks + 1811, flying := some eye } }
  let bow ← equip archer .bow
  let shot := step bow { mine := some eyeBody.centerTile }
  check (shot.bowCooldown == woodenBowCooldown && shot.projectiles.any (·.kind == .woodenArrow))
    "The Classic Wooden Bow did not use the normal arrow damage and cooldown."
  let mut defeated := shot
  for _ in [:30] do
    if !defeated.nightDrops.isEmpty then break
    defeated := step defeated {}
  check (defeated.nightDrops.any (·.coins == 75) && defeated.coins == 0 && defeated.surface.flying.isNone)
    "A defeated Demon Eye did not leave its coins for the player to pick up."
  saveRoundTrip defeated

private def classicEarlyChecks : IO Unit := do
  let fresh := newGame defaultSeed
  check (!fresh.legacyEarly && fresh.inventory.extract 0 3 == #[some ⟨.copperShortsword, 1⟩,
    some ⟨.copperPick, 1⟩, some ⟨.copperAxe, 1⟩] && (fresh.inventory.extract 3).all Option.isNone &&
    fresh.world.tile ⟨16, 39⟩ == .air) "A new world did not start with just the Classic copper tools."
  let some body := findStandingNear fresh.world 22 40 0 0 (fun _ => true)
    | throw (IO.userError "No ground for the Forest Slime fixture.")
  let slime : NightCreature := { body := { body with vx := 1 }, health := 14, hopIn := 0, kind := .greenSlime }
  let passive := step { fresh with surface := { clock := 400, creature := some slime } } {}
  let provoked := step { fresh with surface := { clock := 400, creature := some { slime with health := 9 } } } {}
  check (passive.surface.creature.any (fun next => next.body.x > body.x) &&
    provoked.surface.creature.any (fun next => next.body.x < body.x))
    "The Green Slime did not remain passive until damaged in a daytime Forest."
  let treeX : Nat := 14
  let tree : TilePos := ⟨treeX, groundHeight fresh.seed treeX - 1⟩
  let nearTree ← walkToColumn fresh 16 90
  let pick ← equip nearTree .copperPick
  let refused := step pick { mine := some tree }
  check (refused.world.tile tree == .treeTrunk && refused.tileHits.isEmpty)
    "The Copper Pickaxe damaged a natural tree."
  let axe ← equip refused .copperAxe
  let mut chopped := axe
  for _ in [:252] do chopped := step chopped { mine := some tree }
  check (chopped.world.tile tree == .treeTrunk && (chopped.tileHits[0]?.map (·.damage)) == some 96)
    "A 35% Copper Axe did not deal eight damage per hit across the first twelve swings."
  chopped := step chopped { mine := some tree }
  let trunk := (starterTreePositions fresh.seed).filter (·.x == tree.x)
  check (trunk.all (fun position => chopped.world.tile position == .air) &&
    Inventory.count chopped.inventory .wood ≥ trunk.size && Inventory.count chopped.inventory .wood ≤ 2 * trunk.size)
    "Chopping a tree at its base did not harvest the whole trunk."
  let bench := step chopped { craftClassic := some .workbench }
  check (Inventory.count bench.inventory .wood + 10 == Inventory.count chopped.inventory .wood &&
    Inventory.count bench.inventory .workbench == 1) "A Work Bench did not cost ten wood by hand."

  let iron : TilePos := ⟨21, 38⟩
  let obsidian : TilePos := ⟨22, 38⟩
  let terrain := fresh.world.setTile iron .iron |>.setTile obsidian .obsidian
  let pick ← equip { fresh with world := terrain, chart := fresh.chart.reveal terrain fresh.player.centerTile } .copperPick
  let hit := step pick { mine := some iron }
  check (hit.world.tile iron == .iron && hit.toolCooldown == 15 && hit.tileHits == #[⟨iron, .iron, 35, 59⟩])
    "Copper Pickaxe did not apply 35 damage and a 15-tick cooldown to iron ore."
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode hit)).mapError IO.userError
  check (sameSnapshot hit loaded) "Opening a save mid-mining changed the cracks or cooldown."
  let mut second := loaded
  for _ in [:14] do second := step second { mine := some iron }
  check (second.toolCooldown == 1 && second.tileHits == hit.tileHits) "A held pick attacked before its cooldown."
  second := step second { mine := some iron }
  check (second.tileHits == #[⟨iron, .iron, 70, 59⟩]) "The second hit did not preserve the first hit's damage."
  let mut mined := second
  for _ in [:15] do mined := step mined { mine := some iron }
  check (mined.world.tile iron == .air && mined.tileHits.isEmpty && Inventory.count mined.inventory .iron == 1)
    "Three hits with the starting Copper Pickaxe did not mine iron ore."
  let denied := step mined { mine := some obsidian }
  check (denied.world.tile obsidian == .obsidian && denied.tileHits.isEmpty)
    "Obsidian admitted the starting pickaxe."
  let some (gold, _) := Inventory.addOne mined.inventory .goldPick
    | throw (IO.userError "Could not prepare the Gold Pickaxe.")
  let mut gold ← equip { mined with inventory := gold } .goldPick
  for _ in [:32] do gold := step gold { mine := some obsidian }
  check (gold.world.tile obsidian == .air && Inventory.count gold.inventory .obsidian == 1)
    "A 55% Gold Pickaxe could not break Obsidian in two hits."

  let site : TilePos := ⟨17, 39⟩
  let terrain := fresh.world.setTile site .workbench
  let mut stocked := fresh.inventory
  for (item, quantity) in [(Item.wood, 20), (.caveGel, 1), (.leadOre, 48), (.ironBar, 1), (.stone, 20)] do
    let some (filled, _) := Inventory.addItems stocked item quantity
      | throw (IO.userError s!"Could not prepare {item.name}.")
    stocked := filled
  let supplied := { fresh with inventory := stocked, world := terrain }
  let lit := step supplied { craftClassic := some .torches }
  check (Inventory.count lit.inventory .torch == 3 && Inventory.count lit.inventory .caveGel == 0)
    "The hand torch recipe did not consume Gel."
  let smelt : ClassicRecipe := .smelt .lead
  let noFurnace := step lit { craftClassic := some smelt }
  check (noFurnace.inventory == lit.inventory) "Lead could be smelted without a furnace."
  let mut smith ← buildWith (step lit { craftClassic := some .furnace }) .furnace ⟨21, 39⟩ .furnace
  for _ in [:16] do smith := step smith { craftClassic := some smelt }
  check (Inventory.count smith.inventory .leadBar == 16 && Inventory.count smith.inventory .leadOre == 0)
    "The Furnace did not convert three Lead Ore into each Lead Bar."
  smith := step smith { craftClassic := some .leadAnvil }
  smith ← buildWith smith .leadAnvil ⟨24, 39⟩ .leadAnvil
  smith ← walkToColumn smith 21 80
  smith := step smith { craftClassic := some (.pick .lead) }
  check (Inventory.count smith.inventory .leadPick == 1 && Inventory.count smith.inventory .leadBar == 1 &&
    Item.leadPick.pickPower == 43) "The Lead Anvil did not craft the 43% pick for ten bars."
  let carpenter := step smith { craftClassic := some .chest }
  check (Inventory.count carpenter.inventory .chest == 1 && Inventory.count carpenter.inventory .leadBar == 0 &&
    Inventory.count carpenter.inventory .ironBar == 0) "A chest did not accept mixed Iron and Lead bars."
  saveRoundTrip carpenter

private def classicLifeChecks : IO Unit := do
  let fresh := newGame defaultSeed
  check (fresh.combat.health == 100 && fresh.combat.healthLimit == 100)
    "Classic characters did not start with 100 life."
  let sites := (lifeCrystalSites fresh.seed).extract 0 16
  check (sites.size == 16 && sites.all (fun site => fresh.world.tile site.position == .lifeCrystal))
    "The world cannot supply the 15 upgrades and a spare crystal."
  let world := sites.foldl (fun terrain site => terrain.setTile site.position .air) fresh.world
  let some (inventory, slot) := Inventory.addItems fresh.inventory .lifeCrystal sites.size
    | throw (IO.userError "Could not prepare the harvested crystals.")
  let mut state := { fresh with world, inventory, selected := slot }
  for used in [:maxLifeCrystals] do
    state := step state { mine := some state.player.centerTile }
    check (state.combat.lifeCrystals == used + 1 && state.combat.health == 100 + 20 * (used + 1) &&
      state.combat.healthLimit == state.combat.health) "A Life Crystal did not add 20 current and maximum HP."
  let refused := step state { mine := some state.player.centerTile }
  check (refused.inventory == state.inventory && Inventory.count refused.inventory .lifeCrystal == 1 &&
    refused.combat.healthLimit == 400 &&
    refused.combat.afterDefeat.afterRespawn.health == 200)
    "The 400-HP cap consumed a spare crystal or respawn ignored half of the cap."
  saveRoundTrip refused

  let healthy := fresh.combat
  let scorched := { healthy with health := 100, onFire := lavaBurnTicks }
  let expired := (List.range lavaBurnTicks).foldl (fun combat _ => combat.tickLife true false) scorched
  check (expired.health == 72 && expired.onFire == 0 && expired.regenTime == 0)
    "Seven seconds of On Fire! did not consume 28 HP."
  let rinsed := scorched.tickLife true true
  check (rinsed.onFire == 0 && rinsed.health == scorched.health && rinsed.fireProgress == 0)
    "Wet contact did not clear On Fire!."
  let hurt := { healthy with health := 80 }
  let resting := (List.range 719).foldl (fun combat _ => combat.tickLife true false) hurt
  check (resting.health == 81) "The Classic recovery ramp did not restore one HP after the initial wait."

private def classicBedChecks : IO Unit := do
  let initial := newLegacyGame defaultSeed
  let mut world := initial.world
  for column in [100:144] do
    world := world.setTile ⟨column, 32⟩ .stone
  for column in [122:134] do
    world := world.setTile ⟨column, 24⟩ .stone
  for row in [25:32] do
    world := world.setTile ⟨122, row⟩ .stone |>.setTile ⟨133, row⟩ .stone
  world := world.setTile ⟨122, 30⟩ .doorOpenTop |>.setTile ⟨122, 31⟩ .doorOpen
  world := world.setTile ⟨103, 31⟩ .workbench
  let some player := findStandingNear world 104 32 0 0 (fun _ => true)
    | throw (IO.userError "The crafting fixture has no floor.")
  let mut inventory := initial.inventory
  for (item, amount) in [(Item.stone, 20), (.wood, 50), (.torch, 3), (.iron, 24), (.cobweb, 35)] do
    let some (filled, _) := Inventory.addItems inventory item amount
      | throw (IO.userError s!"Could not prepare {item.name}.")
    inventory := filled
  let mut state : GameState :=
    { initial with world, player, inventory, chart := initial.chart.reveal world player.centerTile }
  let refused := step state { craftClassic := some .ironBar }
  check (refused.inventory == state.inventory) "Iron was smelted away from the furnace."
  state := step state { craftClassic := some .furnace }
  state ← buildWith state .furnace ⟨105, 31⟩ .furnace
  for _ in [:8] do state := step state { craftClassic := some .ironBar }
  state := step state { craftClassic := some .ironAnvil }
  state ← buildWith state .ironAnvil ⟨109, 31⟩ .ironAnvil
  state := step state { craftClassic := some .chain }
  state := step state { craftClassic := some .sawmill }
  state := step state { craftClassic := some .woodWall }
  state := step state { craftClassic := some .woodHammer }
  check (Inventory.count state.inventory .chain == 9 && Inventory.count state.inventory .iron == 0 &&
    Inventory.count state.inventory .ironBar == 0 && Inventory.count state.inventory .woodWall == 4)
    "The Classic iron or wall recipes consumed the wrong amounts."
  state ← walkToColumn state 109 125
  state ← buildWith state .sawmill ⟨112, 31⟩ .sawmill
  state := step state { craftClassic := some .loom }
  state ← walkToColumn state 113 125
  state ← buildWith state .loom ⟨116, 31⟩ .loom
  for _ in [:5] do state := step state { craftClassic := some .silk }
  state := step state { craftClassic := some .bed }
  check (Inventory.count state.inventory .bed == 1 && Inventory.count state.inventory .silk == 0 &&
    Inventory.count state.inventory .cobweb == 0)
    "The loom and sawmill did not turn 35 cobwebs into the silk needed for a bed."
  state ← walkToColumn state 125 220
  state ← buildWith state .bed ⟨125, 31⟩ .bed
  let some bed := state.furnitureAt ⟨125, 31⟩ | throw (IO.userError "The placed bed is missing.")
  let some foot := bed.cells.find? bed.isBedFoot | throw (IO.userError "The bed has no foot.")
  let openRoom := step state { interact := some foot }
  check (openRoom.bedSpawnPoint.isNone) "An unbacked room accepted a bed spawn."
  let mut walls := state.walls
  for row in [25:32] do
    for column in [123:133] do
      if row != 31 || column != 124 then
        walls := walls.set! (row * state.world.width + column) 1
  let wall ← equip { openRoom with walls } .woodWall
  let backed := step wall { place := some ⟨124, 31⟩ }
  check (hasWoodWall backed.world backed.walls ⟨124, 31⟩ &&
    Inventory.count backed.inventory .woodWall == 3) "Placing a background wall did not consume a wall."
  let hammer ← equip backed .woodHammer
  let recovered := step hammer { mine := some ⟨124, 31⟩ }
  check (!hasWoodWall recovered.world recovered.walls ⟨124, 31⟩ &&
    Inventory.count recovered.inventory .woodWall == 4) "The hammer did not recover the background wall."
  let bound := step backed { interact := some foot }
  check (bound.bedSpawnPoint == some bed.position &&
    (step bound { interact := some foot }).bedSpawnPoint.isNone) "Bed foot did not set and clear the spawn."
  saveRoundTrip bound
  let pick ← equip bound .basicPick
  let unsealed := step pick { mine := some ⟨122, 31⟩ }
  check (unsealed.bedSpawnPoint.isNone && unsealed.world.tile ⟨122, 31⟩ == .air)
    "Removing the doorway did not invalidate the bed's spawn."

  let some player := findStandingNear bound.world 140 32 0 0 (fun _ => true)
    | throw (IO.userError "The lava fixture has no floor.")
  let lava : TilePos := ⟨140, 31⟩
  let magma : Water := { levels := bound.magma.levels.set! (31 * bound.world.width + 140) 255 }
  let hazard : GameState := { bound with player, magma, chart := bound.chart.reveal bound.world player.centerTile }
  let ignited := step hazard {}
  check (Magma.touches ignited.magma ignited.world ignited.player &&
    ignited.magma.level ignited.world lava > 0 && ignited.combat.health == 20 &&
    ignited.combat.onFire == 420 && ignited.combat.lavaCooldown == 40)
    "First lava contact did not inflict 80 HP and apply the independent Classic cooldown and fire."
  saveRoundTrip ignited
  let cell := 31 * ignited.world.width + 140
  let quenched := step { ignited with
    magma := { levels := ignited.magma.levels.set! cell 0 }
    water := { levels := ignited.water.levels.set! cell 255 } } {}
  check (quenched.combat.onFire == 0 && quenched.combat.health == ignited.combat.health)
    "Contact with water did not extinguish On Fire! through the game step."
  let mut dead := ignited
  for _ in [:60] do
    if dead.respawnTicks > 0 then break
    dead := step dead {}
  check (dead.combat.health == 0 && dead.respawnTicks == 600) "Sustained contact did not start a death countdown."
  let countdown ← walk dead 239 0
  let loaded ← IO.ofExcept <| (Save.decode (Save.encode countdown)).mapError IO.userError
  check (sameSnapshot countdown loaded && loaded.combat.health == 0 && loaded.respawnTicks == 361)
    "Saving while dead changed the Classic countdown."
  let waiting ← walk loaded 360 0
  check (waiting.respawnTicks == 1 && waiting.combat.health == 0) "Respawn occurred before ten seconds."
  let respawned := step waiting {}
  check ((bedSpawn respawned.world respawned.walls bed) == some respawned.player &&
    respawned.combat.health == 100 && respawned.inventory == bound.inventory)
    "The delayed respawn did not use the bed spawn with the original gear."
  saveRoundTrip respawned

def main (args : List String) : IO Unit := do
  let (initial, output) ← match args with
    | [] => pure (newLegacyGame defaultSeed, none)
    | ["--playthrough", source, destination] => do
      let saved ← IO.ofExcept <| (← Save.read (System.FilePath.mk source)).mapError IO.userError
      check (sameHistoricalSnapshot (newLegacyGame defaultSeed) saved) "Playthrough requires a freshly created seed-42 save."
      let destination := System.FilePath.mk destination
      check (!(← destination.pathExists)) "The playthrough output file already exists."
      pure (saved, some destination)
    | _ => throw (IO.userError "Usage: lean4terria_test [--playthrough FRESH_SAVE OUTPUT_SAVE]")
  check ((newLegacyGame defaultSeed).world != (newLegacyGame 12345).world) "Seed did not change terrain."
  classicInventoryChecks
  classicFishingChecks
  classicWeatherChecks
  fishingCrateChecks
  anglerChecks
  honeyFishingChecks
  waterCatchChecks
  queenSlimeChecks
  destroyerChecks
  skeletronPrimeChecks
  planteraChecks
  templeChecks
  golemChecks
  templeEnemyChecks
  bladeStaffChecks
  dissonanceHookChecks
  crystalAssassinChecks
  wingedSlimeChecks
  skeletronHeadChecks
  skeletronHandChecks
  skeletronLifetimeChecks
  dungeonContactChecks
  dungeonSlimeChecks
  darkCasterChecks
  angryBonesChecks
  cursedSkullChecks
  dungeonChecks
  lowerDungeonChecks
  npcImmunityChecks
  shortswordChecks
  blueMoonChecks
  magicMissileChecks
  deepDungeonChecks
  underworldChecks
  let complete ← playRoute (newLegacyGame defaultSeed)
  let replay ← playRoute (newLegacyGame defaultSeed)
  check (sameSnapshot complete replay) "The same seed and inputs diverged."
  let _ ← playRoute (newLegacyGame 12345)
  saveRoundTrip complete
  lightingChecks
  respawnCheck
  let cleared ← playCombatRoute initial
  saveRoundTrip cleared
  saveRoundTrip (← homeRoute cleared)
  let frost ← frostRoute cleared
  check (sameSnapshot frost (← frostRoute cleared)) "The frost cavern replay diverged."
  saveRoundTrip frost
  let frostRail ← railRoute frost
  saveRoundTrip frostRail
  saveRoundTrip (← backpackChecks)
  ironSwordCheck
  legacyMigrationChecks
  worldSlotChecks
  mapCameraChecks
  pocketMigrationChecks
  ruinMigrationChecks
  lifeCrystalMigrationChecks
  highlandsMigrationChecks
  deepsteelMigrationChecks
  cavernMigrationChecks
  magmaMigrationChecks
  lavaMergeChecks
  classicEyeChecks
  evilBiomeChecks
  evilBossChecks
  classicTownChecks
  classicEarlyChecks
  classicLifeChecks
  classicBedChecks
  let earlyRewards := [(.torch, 6), (.wood, 4)]
  let first := (supplyPockets defaultSeed)[0]!
  let other := (supplyPockets 12345)[0]!
  let relativeTiles (pocket : SupplyPocket) := pocket.edits.map fun (position, tile) =>
    (position.x - pocket.mouth.x, position.y - pocket.mouth.y, tile)
  check (first.mouth != other.mouth && relativeTiles first != relativeTiles other)
    "Different seeds did not change both the entrance and the room geometry."
  let early ← exploreSupplyPocket cleared 0 .stoneSword earlyRewards
  let retreat ← walk early 30 (-1) true
  let bench ← walkToColumn retreat 19 410
  let torches := step bench { craftGelTorches := true }
  check (Inventory.count torches.inventory .caveGel == 0 &&
    Inventory.count torches.inventory .torch == Inventory.count bench.inventory .torch + gelTorchBatch &&
    Inventory.count torches.inventory .wood + 1 == Inventory.count bench.inventory .wood)
    "The Cave Gel recipe did not turn a natural drop and wood into six torches."
  let otherCleared ← playCombatRoute (newLegacyGame 12345)
  saveRoundTrip (← exploreSupplyPocket otherCleared 0 .stoneSword earlyRewards)
  let gate ← walkToColumn frostRail (wardenFightStart - 1 : Nat) 700
  check (gate.warden == .ready) "The Warden awakened outside the hall."
  saveRoundTrip gate
  let checkpoint ← wardenRetryChecks gate
  let entered ← walkToColumn checkpoint 148 140
  let telegraph ← waitForWardenPhase entered .windup
  let restored ← (Save.decode (Save.encode telegraph)).mapError IO.userError |> IO.ofExcept
  let won ← fightWarden telegraph
  check (sameSnapshot won (← fightWarden restored)) "Reloading during the Warden fight changed its replay."
  let hero ← wardenRewardChecks won
  saveRoundTrip hero
  let home ← storageAndNight (← buildHome hero)
  saveRoundTrip home
  let roofGap := home.world.setTile ⟨20, 35⟩ .platform
  check (!(inspectHome roofGap ⟨18, 39⟩).isOk) "A porous platform sealed an open roof."
  let fishingGear ← prepareFishingGear home
  let perch ← fishWesternPond fishingGear
  let harvest ← cultivateSunleaf perch
  let barkskin ← brewBarkskin harvest
  let pick ← equip barkskin .basicPick
  let portable := step pick { mine := some ⟨16, 39⟩ }
  check (portable.world.tile ⟨16, 39⟩ == .air && Inventory.count portable.inventory .workbench == 1)
    "Could not bring a workbench to the quarry bank."
  let eel ← fishQuarry (← quarryEntrance portable)
  brewGills eel
  let huntingGear ← rangedKit home 2
  let frostHunt ← exploreSupplyPocket huntingGear 1 .bow [(.arrow, arrowBatch), (.draught, 1)]
  craftHunterKit (← quarryEntrance frostHunt)
  let ledge ← quarryEntrance (← rangedKit home)
  let _ ← dryQuarryRoute ledge
  let quarryHero ← quarrySwordRoute (← quarryWaterRoute ledge)
  saveRoundTrip (← stormRoute quarryHero)
  migratedAltarCheck
  saveRoundTrip (← biomeRoute quarryHero)
  let village ← merchantHome home
  saveRoundTrip (← tradeRoute village)
  let raider ← raidRoute (← prepareRaidGear village)
  let ordinary := step (step raider { unequipGear := some .accessory }) { unequipGear := some .armor }
  let descent ← walkToColumn ordinary 249 700
  let bench ← walkToColumn descent 281 420
  check bench.nearWorkbench s!"The eastbound surface exit missed the quarry bench at {repr bench.player.centerTile}."
  let shaft ← walkToColumn bench 288 140
  let crossed ← walk shaft 37 1 true
  check (crossed.player.centerTile.x ≥ 291 && crossed.player.centerTile.y ≤ 66)
    s!"Could not jump east across the grotto shaft from {repr shaft.player.centerTile} to {repr crossed.player.centerTile}."
  let pick ← equip crossed .basicPick
  let covered := step (step pick { mine := some ⟨294, 66⟩ }) { mine := some ⟨295, 66⟩ }
  check (Inventory.count covered.inventory .stone ≥ 2) "Could not mine stone for the sentry cover."
  let mineCamp ← deepsteelExpedition (← highlandExpedition (← grappleShaftRoute (← ruinRoute (← stormRoute covered))))
  let completed ← returnFromCavern (← magmaExpedition (← cavernExpedition mineCamp))
  check (completed.warden == .cleared && completed.storm == .cleared &&
    completed.ruin == .cleared && completed.raid == .cleared && completed.cavern == .cleared &&
    completed.guide.isSome && completed.merchant.isSome)
    "A single world did not retain the village and all five victory rewards."
  saveRoundTrip completed
  if let some output := output then
    let _ ← IO.ofExcept <| (← Save.write output completed).mapError IO.userError
    IO.println s!"Completed playthrough saved to {output}."
  IO.println "Passed gallery migration, magma damage and recovery, open-sluice replay, Obsidian cooling and construction, and the original campaign."
