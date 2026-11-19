import Lean4Terria.Save
import Lean4Terria.View
import Lean4Terria.WorldSlots
import Lean4Terria.TitleView
import Lean4Terria.GuideView
import Lean4Terria.OriginalDesktop

namespace Lean4Terria

open Desktop

private def tickSeconds : Float := 1.0 / 60.0

private def defaultWorldDirectory : IO System.FilePath := do
  let some home ← IO.getEnv "HOME" | return "."
  return System.FilePath.mk home / "Library" / "Application Support" / "Lean4Terria"

private def defaultSavePath (seed : Option UInt64) : IO System.FilePath := do
  let name := seed.map (fun value => s!"world-{value}.json") |>.getD "world.json"
  let some home ← IO.getEnv "HOME" | return System.FilePath.mk s!"lean4terria-{name}"
  return System.FilePath.mk home / "Library" / "Application Support" / "Lean4Terria" / name

private structure Options where
  seed : Option UInt64 := none
  savePath : Option System.FilePath := none
  worldDirectory : Option System.FilePath := none
  smoke : Bool := false

private def parseSeed (value : String) : Option UInt64 := do
  let seed ← value.toNat?
  if seed ≥ UInt64.size then none else some seed.toUInt64

private def parseOptions : List String → Options → Except String Options
  | [], options =>
    if options.worldDirectory.isSome && (options.seed.isSome || options.savePath.isSome || options.smoke) then
      .error "--world-dir is for the world chooser. Use --seed or --save to open a world directly."
    else .ok options
  | "--seed" :: value :: rest, options => do
    let some seed := parseSeed value | throw "--seed requires an unsigned 64-bit integer."
    parseOptions rest { options with seed := some seed }
  | "--save" :: path :: rest, options =>
    parseOptions rest { options with savePath := some (System.FilePath.mk path) }
  | "--world-dir" :: path :: rest, options =>
    parseOptions rest { options with worldDirectory := some (System.FilePath.mk path) }
  | "--smoke" :: rest, options => parseOptions rest { options with smoke := true }
  | arg :: _, _ => throw s!"Unknown or incomplete option: {arg}"

private def pressed (keys : UInt64) (index : Nat) : Bool :=
  keys &&& ((1 : UInt64) <<< index.toUInt64) != 0

private def selectedSlot (keys : UInt64) : Option Nat := Id.run do
  for index in [:8] do
    if pressed keys (8 + index) then return some index
  if pressed keys 58 then return some 8
  if pressed keys 59 then return some 9
  return none

private def updateInput (state : GameState) (pending : TickInput) (keys : UInt64) (camera : View.Camera)
    (mouseX mouseY : Nat) (backpackOpen mapOpen classicCrafting : Bool) (craftingPage : Nat)
    (directionTap : Int) : TickInput :=
  let hotbarSlot := View.hotbarSlotAt mouseX mouseY
  let playerSlot := hotbarSlot.orElse fun _ => View.backpackSlotAt mouseX mouseY
  let left := pressed keys 3
  let right := pressed keys 4
  let click (slot : Option Nat) := if left || right then slot.map (·, right) else none
  if mapOpen || pressed keys 30 then { cancelFlail := true }
  else if state.openShop then
    let offer := View.shopOfferAt mouseX mouseY
    let quickSell := left && pressed keys 62
    { closeShop := pending.closeShop || pressed keys 18 || pressed keys 21
      sellSlot := (if quickSell then playerSlot else none).orElse fun _ => pending.sellSlot
      sellCursor := pending.sellCursor || (left && offer.isSome && state.cursorStack.isSome)
      buyOffer := (if (left || right) && state.cursorStack.isNone then offer else none).orElse
        fun _ => pending.buyOffer
      inventoryClick := (if quickSell then none else click playerSlot).orElse fun _ => pending.inventoryClick }
  else if state.openChest.isSome then
    let chestSlot := View.chestSlotAt mouseX mouseY
    let quickMove := left && pressed keys 63
    { closeChest := pending.closeChest || pressed keys 18 || pressed keys 21
      chestDeposit := (if quickMove then playerSlot else none).orElse fun _ => pending.chestDeposit
      chestWithdraw := (if quickMove then chestSlot else none).orElse fun _ => pending.chestWithdraw
      inventoryClick := (if quickMove then none else click playerSlot).orElse fun _ => pending.inventoryClick
      chestClick := (if quickMove then none else click chestSlot).orElse fun _ => pending.chestClick }
  else
    let toggledBackpack := pressed keys 18
    let worldEnabled := !toggledBackpack
    let clicked := left && !toggledBackpack
    let cursed := state.cursedTicks > 0
    let burning := state.burningTicks > 0
    let darkness := state.darknessTicks > 0
    let overBuff := (cursed && View.cursedBuffAt backpackOpen mouseX mouseY) ||
      (burning && View.burningBuffAt backpackOpen cursed mouseX mouseY) ||
      (darkness && View.darknessBuffAt backpackOpen cursed burning mouseX mouseY) ||
      (state.obsidianSkinTicks > 0 && View.obsidianSkinBuffAt backpackOpen cursed burning darkness mouseX mouseY) ||
      (state.horrified && View.horrifiedBuffAt backpackOpen cursed burning darkness (state.obsidianSkinTicks > 0) mouseX mouseY) ||
      (state.tongued && View.tonguedBuffAt backpackOpen cursed burning darkness (state.obsidianSkinTicks > 0) state.horrified mouseX mouseY) ||
      (state.slowTicks > 0 && View.slowBuffAt backpackOpen cursed burning darkness (state.obsidianSkinTicks > 0)
        state.horrified state.tongued mouseX mouseY) ||
      View.cavernBuffAt state backpackOpen mouseX mouseY || View.poisonBuffAt state backpackOpen mouseX mouseY ||
      View.templeBuffAt state backpackOpen mouseX mouseY ||
      View.crateBuffAt state backpackOpen mouseX mouseY ||
      View.mountBuffAt state backpackOpen mouseX mouseY ||
      View.daggerBuffAt state backpackOpen mouseX mouseY
    let overUi := overBuff || (backpackOpen && View.inventoryUiAt mouseX mouseY classicCrafting)
    let target := if overUi then none else camera.target mouseX mouseY
    let horizontal := if worldEnabled then
        (if pressed keys 1 then 1 else 0) - (if pressed keys 0 then 1 else 0)
      else 0
    let clickedHotbar := if clicked && !backpackOpen then hotbarSlot else none
    let wheelSlot :=
      if backpackOpen then none
      else if pressed keys 60 then some ((state.selected + Inventory.hotbarCount - 1) % Inventory.hotbarCount)
      else if pressed keys 61 then some ((state.selected + 1) % Inventory.hotbarCount)
      else none
    let selection := clickedHotbar.orElse fun _ =>
      (selectedSlot keys).orElse fun _ => wheelSlot.orElse fun _ => pending.selectSlot
    let active := state.cursorStack.orElse fun _ => state.inventory[selection.getD state.selected]?.join
    let places := !state.legacyEarly && active.any (·.item.canPlaceInWorld)
    let interaction := if !overBuff && (pressed keys 21 || right) then
        View.interactionTarget state camera mouseX mouseY else none
    let rightInteract := right && interaction.any (fun position =>
      let tile := state.world.tile position
      state.oldManPosition == some position || state.canTalkToAngler position || tile.isFramedChest ||
        (TempleDoor.kind tile).isSome || tile.isTempleAltar)
    let openHeldCrate := right && !rightInteract && !overBuff && !backpackOpen && worldEnabled &&
      state.cursorStack.isNone && active.any (·.item.isCrateContainer)
    let marking := right && !rightInteract && worldEnabled && !overUi && hotbarSlot.isNone &&
      active.any (·.item == .bladeStaff) && mouseX < View.width && mouseY < View.height
    let cursorAim : WorldAim := ⟨(camera.x + mouseX) * tileUnits / View.tilePixels,
      (camera.y + mouseY) * tileUnits / View.tilePixels⟩
    let interact := if pressed keys 21 || rightInteract then interaction else none
    let gearItem := if backpackOpen && !toggledBackpack && right && state.cursorStack.isNone then
        playerSlot.filter fun index => index < Inventory.mainSlotCount &&
          (state.inventory[index]?.join).any (fun stack => stack.item.gearSlot.isSome)
      else none
    let gearSlot := if clicked && backpackOpen then View.gearSlotAt mouseX mouseY else none
    let inventoryClick := if openHeldCrate && !rightInteract then some (selection.getD state.selected, true)
      else if backpackOpen && !toggledBackpack && gearItem.isNone then click playerSlot else none
    let recipe := if clicked && backpackOpen && classicCrafting then
        (View.classicRecipeAt mouseX mouseY craftingPage).filter
          (fun recipe => state.canCraftClassic recipe (!pressed keys 63))
      else none
    let quickGrapple := if !worldEnabled then none
      else if pressed keys 5 && hotbarSlot.isNone && !overUi &&
          mouseX < View.width && mouseY < View.height && state.quickHook.isSome then
        some ⟨(camera.x + mouseX) * tileUnits / View.tilePixels,
          (camera.y + mouseY) * tileUnits / View.tilePixels⟩
      else pending.quickGrapple
    let mountKey := state.wingedSlime.isSome || state.ride.isSome || state.hasPillion
    { horizontal
      dashTap := if !worldEnabled then 0 else if directionTap == 0 then pending.dashTap else directionTap
      jump := worldEnabled && (pending.jump || pressed keys 2)
      swim := worldEnabled && pressed keys 26
      dropThrough := worldEnabled && pressed keys 44
      mine := if worldEnabled then (if clicked && !places then target else none).orElse fun _ => pending.mine else none
      aim := if worldEnabled && hotbarSlot.isNone && !overUi &&
          (target.isSome || ((state.blueMoon.isSome || state.sunfury.isSome || state.valor.isSome ||
            state.magicMissiles.any (·.missile.guided) || state.flamelashes.any (·.projectile.missile.guided)) &&
            mouseX < View.width && mouseY < View.height)) then
        some ⟨(camera.x + mouseX) * tileUnits / View.tilePixels,
          (camera.y + mouseY) * tileUnits / View.tilePixels⟩
        else none
      useHeld := worldEnabled && pressed keys 57
      place := if worldEnabled then
          (if (clicked && places) || (right && !rightInteract && !marking && !backpackOpen && !openHeldCrate) then target else none).orElse
            fun _ => pending.place
        else none
      craft := pending.craft || (pressed keys 5 && quickGrapple.isNone)
      craftTorches := pending.craftTorches || pressed keys 16
      craftSword := pending.craftSword || (pressed keys 17 && !mountKey)
      craftCrystalPick := pending.craftCrystalPick || (pressed keys 19 && !active.any (·.item == .guideVoodooDoll))
      dropVoodooDoll := pending.dropVoodooDoll || (pressed keys 19 && active.any (·.item == .guideVoodooDoll)) ||
        (right && worldEnabled && target.isSome && !rightInteract && state.cursorStack.any (·.item == .guideVoodooDoll))
      craftIronSword := pending.craftIronSword || pressed keys 20
      craftSilverSword := pending.craftSilverSword || pressed keys 27
      craftBow := pending.craftBow || pressed keys 28
      craftArrows := pending.craftArrows || pressed keys 29
      craftMail := pending.craftMail || pressed keys 32
      interact := if worldEnabled && !backpackOpen then interact.orElse (fun _ => pending.interact) else none
      claimHome := if worldEnabled && !backpackOpen then (if pressed keys 22 then target else none).orElse fun _ => pending.claimHome else none
      claimMerchant := if worldEnabled && !backpackOpen then (if pressed keys 31 then target else none).orElse fun _ => pending.claimMerchant else none
      craftWorkbench := pending.craftWorkbench || pressed keys 23
      craftChest := pending.craftChest || pressed keys 24
      craftDoor := pending.craftDoor || pressed keys 25
      craftPlatforms := pending.craftPlatforms || pressed keys 41
      craftBeacon := pending.craftBeacon || pressed keys 42
      craftRecall := pending.craftRecall || pressed keys 43
      craftGelTorches := pending.craftGelTorches || pressed keys 45
      craftHunterBow := pending.craftHunterBow || pressed keys 46
      craftFishingRod := pending.craftFishingRod || pressed keys 47
      craftBait := pending.craftBait || pressed keys 48
      craftGills := pending.craftGills || pressed keys 49
      craftBarkskin := pending.craftBarkskin || pressed keys 50
      dismountCart := worldEnabled && (pending.dismountCart || pressed keys 51)
      quickMount := worldEnabled && (pending.quickMount || (pressed keys 17 && mountKey))
      craftRails := pending.craftRails || pressed keys 52
      craftMinecart := pending.craftMinecart || pressed keys 53
      craftGrapplingHook := pending.craftGrapplingHook || pressed keys 54
      reelHook := worldEnabled && pressed keys 55
      quickGrapple
      craftHighlandCrossbow := pending.craftHighlandCrossbow || pressed keys 56
      craftClassic := recipe.orElse fun _ => pending.craftClassic
      craftToCursor := if recipe.isSome then !pressed keys 63 else pending.craftToCursor
      equipGear := gearItem.orElse fun _ => pending.equipGear
      gearClick := gearSlot.orElse fun _ => pending.gearClick
      selectSlot := selection
      inventoryClick := inventoryClick.orElse fun _ => pending.inventoryClick
      cancelCratePotion := pending.cancelCratePotion || (right && View.crateBuffAt state backpackOpen mouseX mouseY)
      cancelMount := pending.cancelMount || (right && View.mountBuffAt state backpackOpen mouseX mouseY)
      cancelDagger := pending.cancelDagger || (right && View.daggerBuffAt state backpackOpen mouseX mouseY)
      minionAim := if marking then some cursorAim else pending.minionAim }

private def heldToolTarget (state : GameState) (pending : TickInput) (mouseX mouseY : Nat) : Option TilePos := do
  let index := pending.selectSlot.getD state.selected
  let stack ← state.cursorStack.orElse fun _ => state.inventory[index]?.join
  if stack.item.pickPower > 0 || stack.item.axePower > 0 || stack.item == .pwnhammer ||
      stack.item == .rockfish || stack.item == .aquaScepter then
    (View.cameraFor state).target mouseX mouseY
  else none

private def playActionSound (before after : GameState) (input : TickInput) : IO Unit := do
  if input.inventoryClick.any (fun (_, right) => right) && after.status.startsWith "Opened " &&
      after.status.endsWith " Crate." && before.inventory != after.inventory then sound 39
  let beforeWall := hasWoodWall before.world before.walls
  let afterWall := hasWoodWall after.world after.walls
  if input.mine.any (fun pos => (before.world.tile pos != .air && after.world.tile pos == .air) ||
      (beforeWall pos && !afterWall pos)) then
    sound 0
  if input.place.any (fun pos => (before.world.tile pos == .air && after.world.tile pos != .air) ||
      (!beforeWall pos && afterWall pos)) then
    sound 1
  if input.place.isSome && after.carts.size > before.carts.size then sound 1
  if input.mine.isSome && after.carts.size < before.carts.size && before.ride.isNone then sound 0
  if input.interact.isSome && before.ride.isNone && after.ride.isSome then sound 1
  if (input.dismountCart || input.quickMount) && before.ride.isSome && after.ride.isNone then sound 1
  if before.wingedSlime.isNone && after.wingedSlime.isSome then sound 40
  if after.bowCooldown == bladeStaffUseTicks + bladeStaffReuseDelay &&
      after.dagger.isSome && after.selectedStack.any (·.item == .bladeStaff) then sound 41
  if before.dagger.any (·.phase == .recoil 0) && after.dagger.any (·.phase == .recoil 1) then
    let draw := (hash (after.seed, after.surface.clock, (864 : Nat))).toNat % 3
    sound (42 + draw.toUInt32)
  match before.grapple, after.grapple with
  | .latched _, .latched _ => pure ()
  | _, .latched anchor => sound (if anchor.kind == .dissonance then 27 else 15)
  | .flying previous, .flying hook =>
    if hook.life > previous.life || hook.kind != previous.kind then
      sound (if hook.kind == .dissonance then 23 else 14)
  | _, .flying hook => sound (if hook.kind == .dissonance then 23 else 14)
  | _, .idle => pure ()
  if input.interact.any (fun pos => before.world.tile pos == .supplyCache && after.world.tile pos == .openedCache) then
    sound 7
  else if input.interact.any (fun pos => (TempleDoor.kind (before.world.tile pos)).isNone &&
      (before.world.tile pos != after.world.tile pos || before.openChest != after.openChest ||
        before.openGuide != after.openGuide || before.openAngler != after.openAngler)) then
    sound 1
  if after.angler.completed > before.angler.completed then sound 7
  if (input.chestDeposit.isSome || input.chestWithdraw.isSome) && before.inventory != after.inventory then
    sound 1
  if (input.buyOffer.isSome || input.sellSlot.isSome) && before.coins != after.coins then sound 7
  if before.equipment.armor != after.equipment.armor || before.equipment.accessory != after.equipment.accessory then
    sound 1
  if input.mine.isSome && Inventory.count after.inventory .draught < Inventory.count before.inventory .draught then
    sound 7
  if input.mine.isSome &&
      Inventory.count after.inventory .highlandBerry < Inventory.count before.inventory .highlandBerry then
    sound 7
  if after.combat.lifeCrystals > before.combat.lifeCrystals then sound 7
  if after.potionSickness > before.potionSickness &&
      Inventory.count after.inventory .honeyfin < Inventory.count before.inventory .honeyfin then sound 7
  if after.toolCooldown == 12 && before.honey != after.honey &&
      (after.status == "Poured honey." || after.status == "Absorbed honey.") then sound 37
  if input.interact.isSome && after.homeAnchor.isSome && before.homeAnchor != after.homeAnchor then
    sound 10
  if input.interact.isSome && before.bedSpawnPoint != after.bedSpawnPoint then
    sound (if after.bedSpawnPoint.isSome then 10 else 1)
  if before.status != after.status then
    if after.status == "Temple Key used. Lihzahrd Door unlocked." then sound 52
    if after.status == "Lihzahrd Door opened." then sound 53
    if after.status == "Lihzahrd Door closed." then sound 54
  if input.mine.isSome && Inventory.count after.inventory .recallCharm < Inventory.count before.inventory .recallCharm then
    sound 11
  if input.mine.isSome &&
      (Inventory.count after.inventory .gillsElixir < Inventory.count before.inventory .gillsElixir ||
       Inventory.count after.inventory .barkskinTonic < Inventory.count before.inventory .barkskinTonic) then
    sound 13
  match before.fishing, after.fishing with
  | .idle, .bobber motion _ =>
    sound 23
    if motion.wet then sound 37
  | .bobber old oldPhase, .bobber motion phase =>
    if input.mine.isSome && after.bowCooldown == 8 &&
        (match oldPhase, phase with | .returning _, _ => false | _, .returning _ => true | _, _ => false) then
      sound 23
    if !old.wet && motion.wet then sound 37
    if after.status == "The fishing line snapped." && before.status != after.status then sound 38
  | .bobber _ (.returning (some fish)), .idle
  | .bobber _ (.hooked _ fish _), .idle
  | .hooked _ _ fish _, .idle =>
    if Inventory.count before.inventory fish < Inventory.count after.inventory fish ||
        after.nightDrops.any (fun drop => drop.items.any (·.item == fish)) then sound 39
  | .biting .., .idle =>
    if Inventory.count before.inventory .pondPerch < Inventory.count after.inventory .pondPerch ||
        Inventory.count before.inventory .caveEel < Inventory.count after.inventory .caveEel then sound 12
  | _, _ => pure ()
  if (input.craft && Inventory.count before.inventory .copperPick < Inventory.count after.inventory .copperPick) ||
      (input.craftTorches && Inventory.count before.inventory .torch < Inventory.count after.inventory .torch) ||
      (input.craftSword && Inventory.count before.inventory .stoneSword < Inventory.count after.inventory .stoneSword) ||
      (input.craftCrystalPick && Inventory.count before.inventory .crystalPick < Inventory.count after.inventory .crystalPick) ||
      (input.craftIronSword && Inventory.count before.inventory .ironSword < Inventory.count after.inventory .ironSword) ||
      (input.craftSilverSword && Inventory.count before.inventory .silverSword < Inventory.count after.inventory .silverSword) ||
      (input.craftBow && Inventory.count before.inventory .bow < Inventory.count after.inventory .bow) ||
      (input.craftArrows && Inventory.count before.inventory .arrow < Inventory.count after.inventory .arrow) ||
      (input.craftMail && Inventory.count before.inventory .geodeMail < Inventory.count after.inventory .geodeMail) ||
      (input.craftWorkbench && Inventory.count before.inventory .workbench < Inventory.count after.inventory .workbench) ||
      (input.craftChest && Inventory.count before.inventory .chest < Inventory.count after.inventory .chest) ||
      (input.craftDoor && Inventory.count before.inventory .door < Inventory.count after.inventory .door) ||
      (input.craftPlatforms && Inventory.count before.inventory .platform < Inventory.count after.inventory .platform) ||
      (input.craftBeacon && Inventory.count before.inventory .homeBeacon < Inventory.count after.inventory .homeBeacon) ||
      (input.craftRecall && Inventory.count before.inventory .recallCharm < Inventory.count after.inventory .recallCharm) ||
      (input.craftGelTorches && Inventory.count before.inventory .torch < Inventory.count after.inventory .torch) ||
      (input.craftHunterBow && Inventory.count before.inventory .hunterBow < Inventory.count after.inventory .hunterBow) ||
      (input.craftFishingRod && Inventory.count before.inventory .fishingRod < Inventory.count after.inventory .fishingRod) ||
      (input.craftBait && Inventory.count before.inventory .bait < Inventory.count after.inventory .bait) ||
      (input.craftGills && Inventory.count before.inventory .gillsElixir < Inventory.count after.inventory .gillsElixir) ||
      (input.craftBarkskin && Inventory.count before.inventory .barkskinTonic < Inventory.count after.inventory .barkskinTonic) ||
      (input.craftRails && Inventory.count before.inventory .rail < Inventory.count after.inventory .rail) ||
      (input.craftMinecart && Inventory.count before.inventory .minecart < Inventory.count after.inventory .minecart) ||
      (input.craftGrapplingHook &&
        Inventory.count before.inventory .grapplingHook < Inventory.count after.inventory .grapplingHook) ||
      (input.craftHighlandCrossbow &&
        Inventory.count before.inventory .highlandCrossbow < Inventory.count after.inventory .highlandCrossbow) ||
      input.craftClassic.any (fun recipe =>
        Inventory.count before.inventory recipe.item < Inventory.count after.inventory recipe.item) then
    sound 2
  if (before.ecology.sites.zip after.ecology.sites).any (fun
      | (.loot _, .waiting _) => true
      | _ => false) then
    sound 7
  if (after.combat.swing == swingCooldown && before.combat.swing != swingCooldown) ||
      (before.shortsword.isNone && after.shortsword.isSome) then sound 3
  if before.respawnTicks == 0 && (after.respawnTicks > 0 ||
      after.combat.lavaCooldown > before.combat.lavaCooldown ||
      (after.combat.invulnerable == hurtCooldown && before.combat.invulnerable != hurtCooldown)) then
    sound 4
  if input.mine.isSome && after.bowCooldown > before.bowCooldown && !after.handgunFired &&
      !after.hellwingFired && !after.underworldMagicCast &&
      !after.selectedStack.any (fun stack => stack.item == .magicMissile || stack.item == .demonScythe) then
    sound (if before.selectedStack.any (·.item == .highlandCrossbow) then 16 else 8)
  if after.magicMissileSound then sound 21
  if after.underworldMagicImpact || after.magicMissileFlashes.any (·.2 == magicMissileFlashTime) then sound 22
  if after.valorCast || after.underworldSwing then sound 23
  if after.handgunFired then sound 24
  if after.underworldMagicCast then sound 25
  if after.hellwingFired then sound 26
  if after.underworldNpcSound then sound 27
  if after.wallRoared || (before.wallOfFlesh.isNone && after.wallOfFlesh.isSome) then sound 28
  if let some beforeWall := before.wallOfFlesh then
    if after.wallOfFlesh.any (fun wall => wall.health < beforeWall.health) then sound 29
  if after.wallDefeatedNow then sound 30
  if after.twinsRoared || (before.queenSlime.isNone && after.queenSlime.isSome) then sound 28
  if after.twinsShotSound then sound 33
  if after.destroyerRoared then sound 28
  if after.destroyerBurrowed then sound 45
  if after.destroyerHitSound then sound 46
  if after.destroyerKilled || after.probeKilled then sound 47
  if after.probeShotSound then sound 48
  if after.primeRoared then sound 28
  if after.primeHitSound then sound 46
  if after.primeDestroyedPart then sound 47
  if after.primeLaserFired then sound 33
  if after.primeBombLit || after.primeLaserImpact then sound 49
  if after.primeBombExploded then sound 50
  if after.planteraRoared then sound 28
  if after.planteraHitSound then sound 35
  if after.planteraKilledSound then sound 51
  if after.planteraShotSound then sound 38
  if after.planteraShotImpact then sound ((42 + after.surface.clock % 3).toUInt32)
  if before.golem.isNone && after.golem.isSome then sound 28
  if after.golemLanded || after.golemPunchSound then sound 50
  if after.golemFireballSound then sound 25
  if after.golemBeamSound then sound 33
  if after.golemImpactSound then sound 49
  if after.templeDartSound then sound 38
  if after.templeFlameSound then sound 34
  if let some (kind, killed) := after.templeEnemySound then
    sound ((if kind == .flyingSnake then 57 else 55) + if killed then 1 else 0)
  if (before.golem.any fun body => after.golem.any (·.health < body.health)) ||
      (before.golemHead.any fun head => after.golemHead.any (fun current =>
        current.health < head.health || (current.detached && !head.detached))) ||
      before.golemFists.any (fun old => after.golemFists.any (fun fist =>
        fist.kind == old.kind && fist.health < old.health)) then sound 46
  if (before.golem.isSome && after.golem.isNone && after.status == "Golem has been defeated!") ||
      (after.golem.isSome && after.golemFists.size < before.golemFists.size) then sound 47
  if after.twinsFlameSound then sound 34
  if (before.retinazer.any fun twin => after.retinazer.any (·.health < twin.health)) ||
      (before.spazmatism.any fun twin => after.spazmatism.any (·.health < twin.health)) then sound 35
  if after.twinSlain then sound 36
  if before.wraiths.any (fun old => after.wraiths.any (fun w => w.id == old.id && w.health < old.health)) then sound 31
  if before.wraiths.any (fun old => !after.wraiths.any (·.id == old.id)) && after.status == "Wraith defeated." then sound 32
  if after.netSwingTicks > before.netSwingTicks then sound 3
  match before.eye, after.eye with
  | none, some _ => sound 17
  | some previous, some boss =>
    if boss.phase.transforming && !previous.phase.transforming then sound 18
    else if boss.phase.charging && !previous.phase.charging then sound 20
  | some previous, none =>
    if previous.phase != .retreat && after.eyeDefeated && after.respawnTicks == 0 then sound 19
  | none, none => pure ()
  if before.skeletron.isNone && after.skeletron.isSome then sound 17
  if after.skeletron.any (fun boss => boss.phase == .spin && boss.ticks == 2) &&
      !before.skeletron.any (fun boss => boss.phase == .spin && boss.ticks == 2) then sound 20
  if !before.skeletronDefeated && after.skeletronDefeated then sound 19
  match before.warden, after.warden with
  | .fighting _, .cleared | .reward _, .cleared => sound 7
  | .fighting _, .reward _ => sound 6
  | .fighting previous, .fighting boss =>
    if previous.phase != .windup && boss.phase == .windup then sound 5
  | _, _ => pure ()
  match before.storm, after.storm with
  | .fighting _, .cleared | .reward _, .cleared => sound 7
  | .fighting _, .reward _ => sound 6
  | .ready, .fighting _ => sound 5
  | .fighting previous, .fighting boss =>
    if previous.phase != .aim && boss.phase == .aim then sound 5
    else if previous.phase != .barrage && boss.phase == .barrage then sound 9
  | _, _ => pure ()
  match before.ruin, after.ruin with
  | .ready, .fighting _ => sound 5
  | .fighting _, .reward _ => sound 6
  | .reward _, .cleared => sound 7
  | .fighting previous, .fighting boss =>
    if previous.phase != .windup && boss.phase == .windup then sound 5
    else if previous.phase == .slam && previous.ticks == previous.phaseDuration &&
        after.projectiles.any (fun shot => shot.kind == .ruinWave && shot.life == ShotKind.ruinWave.lifetime - 1) then
      sound 9
  | _, _ => pure ()
  match before.cavern, after.cavern with
  | .ready, .fighting _ => sound 5
  | .fighting _, .reward _ => sound 6
  | .reward _, .cleared => sound 7
  | .fighting previous, .fighting boss =>
    if previous.phase != .windup && boss.phase == .windup then sound 5
    else if let some beam := after.projectiles.find? (fun shot =>
        shot.kind == .cavernBeam && shot.life == ShotKind.cavernBeam.lifetime - 1) then
      sound (if beam.y == cavernLowBeamY then 9 else 8)
  | _, _ => pure ()
  match before.raid, after.raid with
  | .ready, .active _ => sound 5
  | .active previous, .active wave =>
    if previous.number != wave.number || (previous.countdown > 0 && wave.countdown == 0) then sound 5
  | .active _, .reward _ => sound 6
  | .active _, .cleared | .reward _, .cleared => sound 7
  | _, _ => pure ()

private def loadWorld (path : System.FilePath) (fallback : GameState) : IO GameState := do
  match ← Save.read path with
  | .ok saved => return saved
  | .error reason =>
    IO.eprintln reason
    return { fallback with status := "Load failed. See terminal for details." }

private def timingSummary (timings : Array Nat) : String :=
  let ordered := timings.qsort (· < ·)
  let median := ordered[ordered.size / 2]!
  let p95 := ordered[(ordered.size * 95 + 99) / 100 - 1]!
  s!"median {median / 1000} us, p95 {p95 / 1000} us"

private def readTitleCharacters (digitsOnly : Bool) : IO String := do
  let accepts (c : Char) : Bool := if digitsOnly then c.isDigit
    else c.toNat ≥ 32 && c.toNat != 127
  let mut result := ""
  repeat
    let code ← readCharacter
    if code == 0 then break
    let char := Char.ofNat code.toNat
    if accepts char then result := result.push char
  return result

private def randomWorldName : IO String := do
  let seed ← WorldSlots.randomSeed
  let first := #["Copper", "Hidden", "Twilight", "Dewy", "Emerald", "Silver", "Whispering", "Golden"]
  let second := #["Forest", "Meadow", "Hollow", "Glade", "River", "Vale", "Grove", "Bluffs"]
  return first[seed.toNat % first.size]! ++ " " ++ second[seed.toNat / first.size % second.size]!

private def runTitle (directory : System.FilePath) (previous : Option Nat) :
    IO (Option (GameState × Nat)) := do
  let mut message := ""
  let mut catalog ← try WorldSlots.scan directory catch error =>
    IO.eprintln error
    pure #[]
  let mut selected := previous.getD (catalog[0]?.map (·.slot) |>.getD 0)
  let mut scroll := TitleView.selectedScroll catalog.size (catalog.findIdx? (·.slot == selected) |>.getD 0) 0
  let mut scrollDrag := false
  let mut page : TitleView.Page := .worlds
  let mut name := ""
  let mut seed := ""
  let mut editValue := ""
  let mut selectAll := false
  let mut lastClick : Option (Nat × Nat) := none
  let mut frame := 0
  while !(← shouldClose) do
    -- The world may have returned before presenting its last input frame.
    let keys ← if frame == 0 then pure 0 else input
    frame := frame + 1
    let mouseCol := (← mouseX).toNat
    let mouseRow := (← mouseY).toNat
    let cursor := TitleView.actionAt page catalog scroll mouseCol mouseRow
    let mut action := if pressed keys 3 then cursor else none
    if pressed keys 33 then action := some .back
    if pressed keys 34 then
      action := match page with
        | .main => some .singlePlayer
        | .worlds => if catalog.isEmpty then some .newWorld else some (.play selected)
        | .create => some .create
        | .edit _ => some .submit
        | .delete slot => some (.confirmDelete slot)
    if page == .worlds then
      if pressed keys 60 then scroll := scroll - 45
      if pressed keys 61 then scroll := min (TitleView.maxScroll catalog.size) (scroll + 45)
      if !pressed keys 57 then scrollDrag := false
      if action == some .scroll then scrollDrag := true
      if scrollDrag then scroll := TitleView.scrollAt catalog.size mouseRow
      if !catalog.isEmpty && (pressed keys 35 || pressed keys 36) then
        let index := catalog.findIdx? (·.slot == selected) |>.getD 0
        let index := if pressed keys 35 then index - 1 else min (catalog.size - 1) (index + 1)
        selected := catalog[index]!.slot
        scroll := TitleView.selectedScroll catalog.size index scroll
    if let .edit field := page then
      let pasted ← if pressed keys 62 && pressed keys 25 then readClipboard else pure ""
      if pressed keys 62 && pressed keys 0 then selectAll := true
      let digitsOnly := field == .seed
      let typed ← readTitleCharacters digitsOnly
      let pasted := String.ofList (pasted.toList.filter fun c =>
        if digitsOnly then c.isDigit else c.toNat ≥ 32 && c.toNat != 127)
      if pressed keys 37 || !typed.isEmpty || !pasted.isEmpty then
        let value := if selectAll then "" else if pressed keys 37 then
          (editValue.take (editValue.length - 1)).toString else editValue
        editValue := ((value ++ typed ++ pasted).take (if digitsOnly then 20 else 40)).toString
        selectAll := false
    if let some (.row slot) := action then
      selected := slot
      let now ← IO.monoMsNow
      if lastClick.any (fun (previous, atTime) => previous == slot && now - atTime ≤ 450) then
        action := some (.play slot)
        lastClick := none
      else lastClick := some (slot, now)
    else if action.isSome then
      lastClick := none
    if let some command := action then
      if command != .scroll then message := ""
      let mut reload := false
      match command with
      | .exit => return none
      | .back =>
        match page with
        | .main => return none
        | .worlds => page := .main
        | .edit .name | .edit .seed => page := .create
        | _ => page := .worlds
      | .singlePlayer => page := .worlds
      | .newWorld =>
        name ← randomWorldName
        seed := ""
        page := .create
        scrollDrag := false
      | .create =>
        let value ← if seed.isEmpty then some <$> WorldSlots.randomSeed else pure (parseSeed seed)
        if let some value := value then
          let slot := WorldSlots.nextSlot catalog
          match ← WorldSlots.create directory slot value name with
          | .ok _ =>
            selected := slot
            page := .worlds
            reload := true
          | .error reason =>
            message := reason
            reload := true
        else message := "Enter a number from 0 to 18446744073709551615."
      | .edit field =>
        editValue := match field with
          | .name => name
          | .seed => seed
          | .rename slot => (catalog.find? (·.slot == slot)).map (·.name) |>.getD ""
        selectAll := true
        page := .edit field
      | .submit =>
        if let .edit field := page then
          let value := editValue.trimAscii.toString
          match field with
          | .seed =>
            if value.isEmpty || (parseSeed value).isSome then
              seed := value
              page := .create
            else message := "Enter a number from 0 to 18446744073709551615."
          | .name =>
            if value.isEmpty then message := "Enter a world name."
            else
              name := value
              page := .create
          | .rename slot =>
            match ← WorldSlots.rename directory slot value with
            | .ok _ =>
              page := .worlds
              reload := true
            | .error reason => message := reason
      | .randomName => name ← randomWorldName
      | .randomSeed => seed := toString (← WorldSlots.randomSeed)
      | .play slot =>
        match ← WorldSlots.load directory slot with
        | .ok state => return some (state, slot)
        | .error reason =>
          IO.eprintln reason
          message := "Could not load this world."
          reload := true
      | .favorite slot =>
        selected := slot
        match ← WorldSlots.toggleFavorite directory slot with
        | .ok _ => reload := true
        | .error reason => message := reason
      | .copySeed slot =>
        if let some value := (catalog.find? (·.slot == slot)).bind (·.seed) then
          setClipboard (toString value)
          message := "Seed copied."
      | .delete slot =>
        if (catalog.find? (·.slot == slot)).any (·.favorite) then
          message := "Unfavorite this world before deleting it."
        else page := .delete slot
      | .confirmDelete slot =>
        match ← WorldSlots.remove directory slot with
        | .ok _ =>
          page := .worlds
          reload := true
        | .error reason => message := reason
      | .row _ | .cloud | .scroll => pure ()
      if reload then
        catalog ← WorldSlots.scan directory
        if !(catalog.any (·.slot == selected)) then selected := catalog[0]?.map (·.slot) |>.getD 0
        let index := catalog.findIdx? (·.slot == selected) |>.getD 0
        scroll := TitleView.selectedScroll catalog.size index (min scroll (TitleView.maxScroll catalog.size))
    beginDrawing View.backgroundColor
    TitleView.draw page catalog selected scroll name seed editValue message mouseCol mouseRow frame
    endDrawing
  return none

/-- Play one world. Return true when it was saved and the world chooser should reopen. -/
private def runDesktop (initial : GameState) (savePath : System.FilePath)
    (managed smoke smokeFromSave : Bool) : IO Bool := do
  let mut state := initial
  let mut pending : TickInput := {}
  let mut remaining := 0.0
  let mut frameTimings : Array Nat := #[]
  let mut drawTimings : Array Nat := #[]
  let mut stepTimings : Array Nat := #[]
  let mut wetFrames := 0
  let mut peakWetCells := 0
  let mut backpackOpen := false
  let mut classicCrafting := !initial.legacyEarly
  let mut craftingPage := 0
  let mut miningHeld := false
  let mut mapOpen := false
  let mut mapCamera : MapCamera := .atTile initial.player.centerTile
  let mut mapDrag : Option (Nat × Nat) := none
  let mut mapClick : Option (Nat × Nat × Nat) := none
  let mut mapPings : Array (TilePos × Nat) := #[]
  let mut mapClickBlocked := false
  let mut minimap : View.MinimapStyle := .corner
  let mut minimapScale := View.minimapDefaultScale
  let mut minimapAlpha : UInt32 := 255
  let mut overlayScale : Float := 2.5
  let mut overlayAlpha : UInt32 := 89
  let mut guidePage : Option Nat := none
  let mut guideDialogue : View.GuideDialogue := {}
  let mut hookTeleport : Option (Player × Player × Nat) := none
  let mut firstFrame := true
  while !(← shouldClose) && (!smoke || frameTimings.size < 120) do
    let mouseCol := (← mouseX).toNat
    let mouseRow := (← mouseY).toNat
    -- A click that opened this world belongs to the chooser.
    let keys ← if firstFrame then pure 0 else input
    firstFrame := false
    if !mapPings.isEmpty then
      let now ← IO.monoMsNow
      mapPings := mapPings.filter (fun (_, expires) => now < expires)
    if !pressed keys 57 then
      miningHeld := false
      mapClickBlocked := false
    if !smoke then
      let minimapAction :=
        if state.openChest.isNone && !state.openShop && state.respawnTicks == 0 &&
            !mapClickBlocked && pressed keys 3 then
          (if backpackOpen then View.minimapControlAt mouseCol mouseRow else none).orElse fun _ =>
            if minimap == .corner then View.minimapAt backpackOpen mouseCol mouseRow else none
        else none
      if mapOpen then
        pending := { cancelFlail := true }
        let close := pressed keys 3 && View.closeMapAt mouseCol mouseRow
        if close || pressed keys 30 || pressed keys 33 then
          mapOpen := false
          mapDrag := none
          mapClick := none
          mapClickBlocked := pressed keys 57
        else
          let controls ← mapInput
          let dt := min 0.05 (← frameTime)
          let axis (positive negative : Nat) : Float :=
            (if pressed controls positive then 1 else 0) - (if pressed controls negative then 1 else 0)
          if pressed keys 3 then
            let now ← IO.monoMsNow
            if mapClick.any (fun (x, y, previous) => now - previous ≤ 400 &&
                ((x : Int) - mouseCol).natAbs ≤ 6 && ((y : Int) - mouseRow).natAbs ≤ 6) then
              let pos := mapCamera.tileAt ((mouseCol : Int) - View.width / 2) ((mouseRow : Int) - View.height / 2)
              if state.world.contains pos then
                mapPings := mapPings.push (pos, now + 15000)
              mapClick := none
            else mapClick := some (mouseCol, mouseRow, now)
          if pressed keys 57 then
            if let some (prevX, prevY) := mapDrag then
              let dx := (prevX : Int) - mouseCol
              let dy := (prevY : Int) - mouseRow
              if dx.natAbs > 2 || dy.natAbs > 2 then mapClick := none
              mapCamera := mapCamera.pan state.world dx.toFloat dy.toFloat
            mapDrag := some (mouseCol, mouseRow)
          else
            mapDrag := none
          if axis 1 0 != 0 || axis 3 2 != 0 then
            mapCamera := mapCamera.pan state.world (axis 1 0 * 480 * dt) (axis 3 2 * 480 * dt)
          let wheel : Float := if pressed keys 60 then 1.15 else if pressed keys 61 then 1 / 1.15 else 1
          if wheel != 1 then
            mapCamera := mapCamera.zoom state.world wheel (View.mapMinScale state.world)
              ((mouseCol : Int) - View.width / 2).toFloat ((mouseRow : Int) - View.height / 2).toFloat
          let zoom := axis 4 5
          if zoom != 0 then
            mapCamera := mapCamera.zoom state.world (Float.exp (zoom * dt * 1.8))
              (View.mapMinScale state.world) 0 0
      else if let some page := guidePage then
        if pressed keys 33 || pressed keys 38 then
          guidePage := none
        else
          let clicked := if pressed keys 3 then GuideView.pageAt mouseCol mouseRow else none
          let next := if pressed keys 39 then (page + GuideView.pageCount - 1) % GuideView.pageCount
            else if pressed keys 40 then (page + 1) % GuideView.pageCount else page
          guidePage := some (clicked.getD next)
        pending := {}
      else if state.openGuide || state.openOldMan || state.openAngler then
        let clicked := pressed keys 3
        let close := pressed keys 33 || pressed keys 21 || pressed keys 18 ||
          (clicked && (if state.openOldMan || state.openAngler then View.oldManCloseAt mouseCol mouseRow
            else View.guideCloseAt mouseCol mouseRow))
        if state.openAngler then
          pending :=
            { closeAngler := pending.closeAngler || close
              askAnglerQuest := pending.askAnglerQuest || (clicked && !close && View.anglerQuestAt mouseCol mouseRow) }
        else if state.openOldMan then
          pending :=
            { closeOldMan := pending.closeOldMan || close
              curseOldMan := pending.curseOldMan || (clicked && !close && View.oldManCurseAt mouseCol mouseRow) }
        else
          pending := { closeGuide := pending.closeGuide || close }
        if state.openGuide && clicked && !close then
          guideDialogue := View.guideDialogueClick state guideDialogue mouseCol mouseRow
        if pressed keys 6 then
          match ← Save.write savePath state with
          | .ok _ => state := { state with status := "World saved." }
          | .error reason =>
            IO.eprintln reason
            state := { state with status := "Save failed. See terminal for details." }
        if pressed keys 7 then
          state ← loadWorld savePath { state with openGuide := false, openOldMan := false, openAngler := false }
          pending := {}
      else if pressed keys 38 && state.respawnTicks == 0 then
        guidePage := some 0
        pending := {}
        remaining := 0
      else if state.respawnTicks == 0 && (pressed keys 30 || minimapAction == some .full) then
        mapOpen := true
        mapCamera := .atTile state.player.centerTile mapCamera.scale
        mapDrag := none
        backpackOpen := false
        state := { state with openChest := none, openShop := false }
        pending := { cancelFlail := true }
        miningHeld := false
      else if let some action := minimapAction then
        match action with
        | .style value => minimap := value
        | .zoomIn => minimapScale := min 3 (minimapScale * 1.025)
        | .zoomOut => minimapScale := max 0.2 (minimapScale * 0.975)
        | .reset | .full => minimapScale := View.minimapDefaultScale
        mapClickBlocked := true
        pending := {}
      else if pressed keys 3 && (backpackOpen || state.openChest.isSome || state.openShop) &&
          View.saveQuitAt mouseCol mouseRow then
        pending := {}
        match ← Save.write savePath state with
        | .ok _ =>
          wallMusic false
          twinsMusic false
          destroyerMusic false
          primeMusic false
          planteraMusic false
          golemMusic false
          templeMusic false
          return managed
        | .error reason =>
          IO.eprintln reason
          state := { state with status := "Save failed. Still in this world." }
      else if pressed keys 33 then
        pending := {}
        if state.openChest.isSome || state.openShop then
          backpackOpen := false
          state := { state with openChest := none, openShop := false }
        else if state.respawnTicks == 0 then
          backpackOpen := !backpackOpen
          if backpackOpen && state.canCraftClassic .suspiciousLookingEye then
            classicCrafting := true
            craftingPage := View.altarCraftingPage
      else
        if pressed keys 18 then minimap := minimap.next
        if minimap != .hidden then
          let controls ← mapInput
          let dt := min 0.05 (← frameTime)
          let zoom : Float := (if pressed controls 4 then 1 else 0) - (if pressed controls 5 then 1 else 0)
          let factor := Float.exp (zoom * dt * 1.5)
          let opacityStep := (dt * 85).ceil.toUInt64.toUInt32
          let opacity (alpha : UInt32) :=
            if pressed controls 6 then min 255 (alpha + opacityStep)
            else if pressed controls 7 then max 3 (alpha - min alpha opacityStep)
            else alpha
          if minimap == .corner then
            minimapScale := max 0.2 (min 3 (minimapScale * factor))
            minimapAlpha := opacity minimapAlpha
          else
            overlayScale := max (View.width.toFloat / state.world.width.toFloat)
              (min 24 (overlayScale * factor))
            overlayAlpha := opacity overlayAlpha
        if backpackOpen && pressed keys 3 && state.openChest.isNone && !state.openShop then
          if let some crafting := View.classicTabAt mouseCol mouseRow then classicCrafting := crafting
          else if classicCrafting && !pressed keys 18 then
            if let some page := View.classicPageAt craftingPage mouseCol mouseRow then craftingPage := page
        if pressed keys 6 then
          match ← Save.write savePath state with
          | .ok _ => state := { state with status := "World saved." }
          | .error reason =>
            IO.eprintln reason
            state := { state with status := "Save failed. See terminal for details." }
        if pressed keys 7 then
          state ← loadWorld savePath { state with openChest := none, openShop := false }
          pending := {}
          backpackOpen := false
          craftingPage := 0
        else if state.respawnTicks == 0 then
          let tap ← if state.equipment.crystalAssassinSet then dashTap else pure 0
          let direction := if tap == 2 then -1 else (tap.toNat : Int)
          let heldClickMask := ((1 : UInt64) <<< 3) ||| ((1 : UInt64) <<< 57)
          let gameKeys := if mapClickBlocked then keys &&& ~~~heldClickMask else keys
          pending := updateInput state pending gameKeys (View.cameraFor state) mouseCol mouseRow
            backpackOpen mapOpen classicCrafting craftingPage direction
        else pending := {}
      let cursed := state.cursedTicks > 0
      let burning := state.burningTicks > 0
      let darkness := state.darknessTicks > 0
      if guidePage.isSome || mapOpen || state.openChest.isSome || state.openShop || state.openGuide || state.openAngler ||
          state.openOldMan ||
          (cursed && View.cursedBuffAt backpackOpen mouseCol mouseRow) ||
          (burning && View.burningBuffAt backpackOpen cursed mouseCol mouseRow) ||
          (darkness && View.darknessBuffAt backpackOpen cursed burning mouseCol mouseRow) ||
          (state.obsidianSkinTicks > 0 && View.obsidianSkinBuffAt backpackOpen cursed burning darkness mouseCol mouseRow) ||
          (state.horrified && View.horrifiedBuffAt backpackOpen cursed burning darkness (state.obsidianSkinTicks > 0) mouseCol mouseRow) ||
          (state.tongued && View.tonguedBuffAt backpackOpen cursed burning darkness (state.obsidianSkinTicks > 0) state.horrified mouseCol mouseRow) ||
          (state.slowTicks > 0 && View.slowBuffAt backpackOpen cursed burning darkness (state.obsidianSkinTicks > 0)
            state.horrified state.tongued mouseCol mouseRow) ||
          View.cavernBuffAt state backpackOpen mouseCol mouseRow ||
          View.poisonBuffAt state backpackOpen mouseCol mouseRow ||
          View.templeBuffAt state backpackOpen mouseCol mouseRow ||
          View.crateBuffAt state backpackOpen mouseCol mouseRow ||
          View.mountBuffAt state backpackOpen mouseCol mouseRow ||
          View.daggerBuffAt state backpackOpen mouseCol mouseRow ||
          (backpackOpen && View.inventoryUiAt mouseCol mouseRow classicCrafting) ||
          state.respawnTicks > 0 || pressed keys 7 || pressed keys 18 || pressed keys 33 || pressed keys 38 then
        miningHeld := false
      else if pressed keys 3 then
        miningHeld := pressed keys 57 && pending.mine.isSome
    else
      pending := { horizontal := if !smokeFromSave && frameTimings.size < 40 then 1 else 0 }
    if guidePage.isNone then
      remaining := remaining + min 0.25 (← frameTime)
      while remaining ≥ tickSeconds do
        let stepStarted ← if smoke then IO.monoNanosNow else pure 0
        let tickInput := if miningHeld && pending.mine.isNone then
            { pending with mine := heldToolTarget state pending mouseCol mouseRow }
          else pending
        let next := step state tickInput
        hookTeleport := match state.grapple, next.grapple with
          | .flying flight, .latched anchor =>
            if anchor.kind == .dissonance && !flight.returning then some (state.player, next.player, 0)
            else none
          | _, _ => hookTeleport.bind fun (start, finish, age) =>
            if age < 21 then some (start, finish, age + 1) else none
        if smoke then stepTimings := stepTimings.push ((← IO.monoNanosNow) - stepStarted)
        playActionSound state next tickInput
        if next.respawnTicks > 0 then
          backpackOpen := false
          mapOpen := false
        if !state.openGuide && next.openGuide then guideDialogue := {}
        state := next
        let interactionOpen := state.openChest.isSome || state.openShop || state.openGuide || state.openOldMan || state.openAngler
        if interactionOpen || state.respawnTicks > 0 then miningHeld := false
        pending :=
          { horizontal := if interactionOpen then 0 else pending.horizontal
            aim := if interactionOpen then none else pending.aim
            useHeld := !interactionOpen && pending.useHeld
            cancelFlail := pending.cancelFlail
            swim := !interactionOpen && pending.swim
            dropThrough := !interactionOpen && pending.dropThrough
            reelHook := !interactionOpen && pending.reelHook }
        remaining := remaining - tickSeconds
    let started ← if smoke then IO.monoNanosNow else pure 0
    beginDrawing View.backgroundColor
    let wetCells ← if let some page := guidePage then
        GuideView.draw state page mouseCol mouseRow
        pure 0
      else if mapOpen then
        View.drawMap state mapCamera mouseCol mouseRow (mapPings.map (·.1))
        pure 0
      else do
        let wet ← View.draw state (View.cameraFor state) mouseCol mouseRow backpackOpen managed
          classicCrafting craftingPage hookTeleport
        if state.openChest.isNone && !state.openShop &&
            !state.openGuide && !state.openOldMan && !state.openAngler && state.respawnTicks == 0 then
          let (scale, alpha) := if minimap == .overlay then (overlayScale, overlayAlpha)
            else (minimapScale, minimapAlpha)
          View.drawMinimap state minimap scale alpha backpackOpen mouseCol mouseRow (mapPings.map (·.1))
        if state.openGuide then View.drawGuideDialogue state guideDialogue mouseCol mouseRow
        if state.openOldMan then View.drawOldManDialogue state mouseCol mouseRow
        if state.openAngler then View.drawAnglerDialogue state mouseCol mouseRow
        pure wet
    if smoke then drawTimings := drawTimings.push ((← IO.monoNanosNow) - started)
    endDrawing
    wallMusic (state.wallOfFlesh.isSome && !state.twinsActive && state.destroyer.isNone &&
      state.skeletronPrime.isNone && state.plantera.isNone)
    twinsMusic (state.twinsActive && state.destroyer.isNone && state.plantera.isNone)
    destroyerMusic (state.destroyer.isSome && state.plantera.isNone)
    primeMusic (state.skeletronPrime.isSome && !state.twinsActive && state.destroyer.isNone && state.plantera.isNone)
    planteraMusic state.plantera.isSome
    golemMusic (state.golem.isSome && state.plantera.isNone && state.skeletronPrime.isNone &&
      state.destroyer.isNone && !state.twinsActive && state.wallOfFlesh.isNone)
    templeMusic (state.golem.isNone && state.plantera.isNone && state.skeletronPrime.isNone &&
      state.destroyer.isNone && !state.twinsActive && state.wallOfFlesh.isNone &&
      TempleLayout.hasNaturalWall state.world state.walls state.player.centerTile)
    if smoke then
      frameTimings := frameTimings.push ((← IO.monoNanosNow) - started)
      if wetCells > 0 then wetFrames := wetFrames + 1
      peakWetCells := max peakWetCells wetCells
  wallMusic false
  twinsMusic false
  destroyerMusic false
  primeMusic false
  planteraMusic false
  golemMusic false
  templeMusic false
  if managed then
    match ← Save.write savePath state with
    | .ok _ => pure ()
    | .error reason => throw (IO.userError reason)
  if smoke then
    if frameTimings.size != 120 || stepTimings.isEmpty || (!smokeFromSave && state.player.x <= initial.player.x) then
      throw (IO.userError "Desktop smoke test did not complete its frame and simulation sequence.")
    IO.println s!"Desktop smoke test passed ({frameTimings.size} frames, {stepTimings.size} simulation steps)."
    IO.println s!"Water visible in {wetFrames}/{frameTimings.size} frames, peak {peakWetCells} wet tiles on screen."
    IO.println s!"Simulation step: {timingSummary stepTimings}."
    IO.println s!"Draw submission: {timingSummary drawTimings}."
    IO.println s!"Draw + present + frame pacing: {timingSummary frameTimings}."
  return false

private def runWorlds (directory : System.FilePath) : IO Unit := do
  let mut previous := none
  repeat
    let some (state, slot) ← runTitle directory previous | break
    unless ← runDesktop state (WorldSlots.path directory slot) true false false do break
    previous := some slot

def run (args : List String) : IO UInt32 := do
  if let "--original-world" :: previewArgs := args then
    return ← OriginalDesktop.runArgs previewArgs
  if args == ["--help"] then
    IO.println s!"Lean4Terria [--world-dir DIRECTORY]\nLean4Terria [--seed UINT64] [--save FILE]\n{OriginalDesktop.usage}\nLean4Terria --smoke [--save FILE]"
    return 0
  match parseOptions args {} with
  | .error message =>
    IO.eprintln message
    return 2
  | .ok options =>
    if !options.smoke && options.seed.isNone && options.savePath.isNone then
      let directory ← options.worldDirectory.map pure |>.getD defaultWorldDirectory
      openWindow View.width.toUInt32 View.height.toUInt32 "Lean4Terria"
      try runWorlds directory finally closeWindow
      return 0
    let savePath ← options.savePath.map pure |>.getD (defaultSavePath options.seed)
    let smokeFromSave := options.smoke && options.savePath.isSome
    let initial ← if smokeFromSave then
        match ← Save.read savePath with
        | .ok saved => pure saved
        | .error reason => throw (IO.userError reason)
      else
        let fresh := newGame (options.seed.getD defaultSeed)
        if !options.smoke && (← savePath.pathExists) then loadWorld savePath fresh else pure fresh
    openWindow View.width.toUInt32 View.height.toUInt32 "Lean4Terria"
    try
      if options.smoke then
        IO.println "Terraria textures and Mouse_Text font: local XNB assets."
      let _ ← runDesktop initial savePath false options.smoke smokeFromSave
      pure ()
    finally closeWindow
    return 0

end Lean4Terria

def main (args : List String) : IO UInt32 := Lean4Terria.run args
