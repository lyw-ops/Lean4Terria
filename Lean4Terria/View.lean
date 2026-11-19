import Lean4Terria.Desktop
import Lean4Terria.Game
import Lean4Terria.Lighting

namespace Lean4Terria.View

open Lean4Terria.Desktop

def width : Nat := 1120
def height : Nat := 704
def tilePixels : Nat := 24

private def panelY : Nat := height - 117
private def hudTop : Int := (panelY : Int) - 18
private def hotbarLabelY : Nat := 3
private def slotSize : Nat := 44
private def slotLeft : Nat := 20
private def hotbarY : Nat := 20
private def inventoryStep (index : Nat) : Nat := 238 * index / 5
private def inventoryX (col : Nat) : Nat := slotLeft + inventoryStep col
private def inventoryY (row : Nat) : Nat := hotbarY + inventoryStep row
private def coinX : Nat := 497
private def ammoX : Nat := 534
private def sideSlotSize : Nat := 31
private def sideSlotY (row : Nat) : Nat := 105 + 168 * row / 5
private def trashX : Nat := 448
private def trashY : Nat := 258
private def chestLeft : Nat := 20
private def chestY : Nat := 323
private def chestSlotsY : Nat := chestY + 45
private def chestColumns : Nat := 10
private def shopLeft : Nat := 584
private def shopOfferStride : Nat := 39
private def shopOfferHeight : Nat := 35
private def shopPanelHeight : Nat := 50 + (shopOffersFor false).size * shopOfferStride
private def shopPanelY : Nat := 105
private def shopOfferY : Nat := shopPanelY + 48
private def gearX : Nat := 1055
private def gearY : Nat := 122
private def armorY : Nat := gearY + 52
private def legsY : Nat := gearY + 104
private def accessoryY : Nat := gearY + 156
private def hookY : Nat := gearY + 208
private def mountY : Nat := hookY + 52
private def craftLeft : Nat := 20
private def craftY : Nat := 322
private def recipeWidth : Nat := 249
private def recipeStride : Nat := 260
private def classicToggleWidth : Nat := 99
private def classicRowY : Nat := craftY + 32
private def classicRowHeight : Nat := 25
private def classicRowStride : Nat := 27
private def classicRows : Nat := 5
private def classicPageSize : Nat := 2 * classicRows
private def classicPrevX : Nat := craftLeft + recipeStride
private def classicNextX : Nat := classicPrevX + recipeWidth - 36
private def classicPagerWidth : Nat := 36
private def saveQuitX : Nat := 928
private def saveQuitY : Nat := 643
private def saveQuitW : Nat := 171
private def saveQuitH : Nat := 39

def classicPageCount : Nat := max 1 ((ClassicRecipe.all.size + classicPageSize - 1) / classicPageSize)

def altarCraftingPage : Nat :=
  (ClassicRecipe.all.findIdx? (· == .suspiciousLookingEye)).getD 0 / classicPageSize

private def containsRect (x y left top w h : Nat) : Bool :=
  left ≤ x && x < left + w && top ≤ y && y < top + h

private def cursedBuffOrigin (inventoryOpen : Bool) : Nat × Nat :=
  if inventoryOpen then (width - 84, hookY + 91) else (32, 76)

def cursedBuffAt (inventoryOpen : Bool) (x y : Nat) : Bool :=
  let (left, top) := cursedBuffOrigin inventoryOpen
  containsRect x y left top 32 32

private def burningBuffOrigin (inventoryOpen cursed : Bool) : Nat × Nat :=
  let (x, y) := cursedBuffOrigin inventoryOpen
  (x + if cursed then 38 else 0, y)

def burningBuffAt (inventoryOpen cursed : Bool) (x y : Nat) : Bool :=
  let (left, top) := burningBuffOrigin inventoryOpen cursed
  containsRect x y left top 32 32

private def darknessBuffOrigin (inventoryOpen cursed burning : Bool) : Nat × Nat :=
  let (x, y) := burningBuffOrigin inventoryOpen cursed
  if inventoryOpen && cursed && burning then (x - 76, y) else (x + if burning then 38 else 0, y)

def darknessBuffAt (inventoryOpen cursed burning : Bool) (x y : Nat) : Bool :=
  let (left, top) := darknessBuffOrigin inventoryOpen cursed burning
  containsRect x y left top 32 32

private def obsidianSkinBuffOrigin (inventoryOpen cursed burning darkness : Bool) : Nat × Nat :=
  let (x, y) := cursedBuffOrigin inventoryOpen
  let index := cursed.toNat + burning.toNat + darkness.toNat
  if inventoryOpen then (width - 84 - 38 * (index / 2) + 38 * (index % 2), y + 44)
  else (x + 38 * index, y)

def obsidianSkinBuffAt (inventoryOpen cursed burning darkness : Bool) (x y : Nat) : Bool :=
  let (left, top) := obsidianSkinBuffOrigin inventoryOpen cursed burning darkness
  containsRect x y left top 32 32

private def wallBuffOrigin (inventoryOpen : Bool) (index : Nat) : Nat × Nat :=
  let (x, y) := cursedBuffOrigin inventoryOpen
  if inventoryOpen then (x + 38 * (index % 2), y + 88 + 44 * (index / 2))
  else (x + 38 * index, y)

def horrifiedBuffAt (inventoryOpen cursed burning darkness skin : Bool) (x y : Nat) : Bool :=
  let index := if inventoryOpen then 0 else cursed.toNat + burning.toNat + darkness.toNat + skin.toNat
  let (left, top) := wallBuffOrigin inventoryOpen index
  containsRect x y left top 32 32

def tonguedBuffAt (inventoryOpen cursed burning darkness skin horrified : Bool) (x y : Nat) : Bool :=
  let index := (if inventoryOpen then 0 else cursed.toNat + burning.toNat + darkness.toNat + skin.toNat) +
    horrified.toNat
  let (left, top) := wallBuffOrigin inventoryOpen index
  containsRect x y left top 32 32

private def slowBuffOrigin (inventoryOpen cursed burning darkness skin horrified tongued : Bool) : Nat × Nat :=
  let index := (if inventoryOpen then 0 else cursed.toNat + burning.toNat + darkness.toNat + skin.toNat) +
    horrified.toNat + tongued.toNat
  wallBuffOrigin inventoryOpen index

def slowBuffAt (inventoryOpen cursed burning darkness skin horrified tongued : Bool) (x y : Nat) : Bool :=
  let (left, top) := slowBuffOrigin inventoryOpen cursed burning darkness skin horrified tongued
  containsRect x y left top 32 32

private def cavernBuffOrigin (state : GameState) (inventoryOpen inferno : Bool) : Nat × Nat :=
  let index := (if inventoryOpen then 0 else
    (state.cursedTicks != 0).toNat + (state.burningTicks != 0).toNat +
      (state.darknessTicks != 0).toNat + (state.obsidianSkinTicks != 0).toNat) +
    state.horrified.toNat + state.tongued.toNat + (state.slowTicks != 0).toNat +
    (inferno && state.weakTicks > 0).toNat
  wallBuffOrigin inventoryOpen index

def cavernBuffAt (state : GameState) (inventoryOpen : Bool) (x y : Nat) : Bool :=
  let (weakX, weakY) := cavernBuffOrigin state inventoryOpen false
  let (infernoX, infernoY) := cavernBuffOrigin state inventoryOpen true
  (state.weakTicks > 0 && containsRect x y weakX weakY 32 32) ||
    (state.cursedInfernoTicks > 0 && containsRect x y infernoX infernoY 32 32)

private def crateBuffOrigin (state : GameState) (inventoryOpen : Bool) : Nat × Nat :=
  if inventoryOpen then (width - 200, hookY + 50)
  else
    let (x, y) := cavernBuffOrigin state false true
    (x + 38 * (state.cursedInfernoTicks != 0).toNat, y)

def crateBuffAt (state : GameState) (inventoryOpen : Bool) (x y : Nat) : Bool :=
  let (left, top) := crateBuffOrigin state inventoryOpen
  state.cratePotion > 0 && containsRect x y left top 32 32

private def mountBuffOrigin (state : GameState) (inventoryOpen : Bool) : Nat × Nat :=
  let (x, y) := crateBuffOrigin state inventoryOpen
  if inventoryOpen then (x, y + 44) else (x + (if state.cratePotion > 0 then 38 else 0), y)

def mountBuffAt (state : GameState) (inventoryOpen : Bool) (x y : Nat) : Bool :=
  let (left, top) := mountBuffOrigin state inventoryOpen
  state.wingedSlime.isSome && containsRect x y left top 32 32

private def daggerBuffOrigin (state : GameState) (inventoryOpen : Bool) : Nat × Nat :=
  let (x, y) := mountBuffOrigin state inventoryOpen
  if inventoryOpen then (x, y + 44) else (x + 38 * state.wingedSlime.isSome.toNat, y)

def daggerBuffAt (state : GameState) (inventoryOpen : Bool) (x y : Nat) : Bool :=
  let (left, top) := daggerBuffOrigin state inventoryOpen
  state.dagger.isSome && containsRect x y left top 32 32

private def poisonBuffOrigin (state : GameState) (inventoryOpen : Bool) : Nat × Nat :=
  let (x, y) := daggerBuffOrigin state inventoryOpen
  if inventoryOpen then (x - 44, y) else (x + 38 * state.dagger.isSome.toNat, y)

def poisonBuffAt (state : GameState) (inventoryOpen : Bool) (x y : Nat) : Bool :=
  let (left, top) := poisonBuffOrigin state inventoryOpen
  state.poisonedTicks > 0 && containsRect x y left top 32 32

private def templeBuffs (state : GameState) : Array (Nat × String × String) :=
  (#[(state.hellfireTicks, "Buff_323", "Hellfire  /  Losing life"),
    (state.bleedingTicks, "Buff_30", "Bleeding  /  Cannot regenerate life")]).filter (·.1 > 0)

private def templeBuffOrigin (state : GameState) (inventoryOpen : Bool) (index : Nat) : Nat × Nat :=
  let (x, y) := poisonBuffOrigin state inventoryOpen
  let spacing := if inventoryOpen then 44 else 38
  let distance := spacing * (index + (decide (state.poisonedTicks > 0)).toNat)
  (if inventoryOpen then x - distance else x + distance, y)

def templeBuffAt (state : GameState) (inventoryOpen : Bool) (x y : Nat) : Bool :=
  (Array.range (templeBuffs state).size).any fun index =>
    let (left, top) := templeBuffOrigin state inventoryOpen index
    containsRect x y left top 32 32

private def cellAt (position origin count size : Nat) (step := inventoryStep) : Option Nat := Id.run do
  for index in [:count] do
    let start := origin + step index
    if start ≤ position && position < start + size then return some index
  return none

def hotbarSlotAt (x y : Nat) : Option Nat :=
  if hotbarY ≤ y && y < hotbarY + slotSize then
    cellAt x slotLeft Inventory.hotbarCount slotSize
  else none

private def inventoryRows : Nat := Inventory.mainSlotCount / Inventory.hotbarCount

def backpackSlotAt (x y : Nat) : Option Nat := do
  if containsRect x y trashX trashY slotSize slotSize then return Inventory.trashSlot
  if coinX ≤ x && x < coinX + sideSlotSize then
    return Inventory.coinStart + (← cellAt y (sideSlotY 0) (Inventory.ammoStart - Inventory.coinStart)
      sideSlotSize (fun row => sideSlotY row - sideSlotY 0))
  if ammoX ≤ x && x < ammoX + sideSlotSize then
    return Inventory.ammoStart + (← cellAt y (sideSlotY 0) (Inventory.trashSlot - Inventory.ammoStart)
      sideSlotSize (fun row => sideSlotY row - sideSlotY 0))
  let row ← cellAt y hotbarY inventoryRows slotSize
  if row == 0 then none else
    let col ← cellAt x slotLeft Inventory.hotbarCount slotSize
    some (row * Inventory.hotbarCount + col)

def chestSlotAt (x y : Nat) : Option Nat := do
  let row ← cellAt y chestSlotsY ((chestSlots + chestColumns - 1) / chestColumns) slotSize
  let col ← cellAt x chestLeft chestColumns slotSize
  let slot := row * chestColumns + col
  if slot < chestSlots then some slot else none

def shopOfferAt (x y : Nat) : Option Nat :=
  if x < shopLeft || x ≥ 1100 || y < shopOfferY then none
  else
    let offset := y - shopOfferY
    let index := offset / shopOfferStride
    if index < (shopOffersFor false).size && offset % shopOfferStride < shopOfferHeight then some index else none

def gearSlotAt (x y : Nat) : Option GearSlot :=
  if containsRect x y gearX gearY slotSize slotSize then some .head
  else if containsRect x y gearX armorY slotSize slotSize then some .armor
  else if containsRect x y gearX legsY slotSize slotSize then some .legs
  else if containsRect x y gearX accessoryY slotSize slotSize then some .accessory
  else if containsRect x y gearX hookY slotSize slotSize then some .hook
  else if containsRect x y gearX mountY slotSize slotSize then some .mount
  else none

/-- Toggle the recipe list below the backpack. -/
def classicTabAt (x y : Nat) : Option Bool :=
  if containsRect x y craftLeft craftY classicToggleWidth 24 then some true
  else if containsRect x y (craftLeft + classicToggleWidth + 5) craftY 25 24 then some false
  else none

def saveQuitAt (x y : Nat) : Bool := containsRect x y saveQuitX saveQuitY saveQuitW saveQuitH

/-- World clicks may pass through the rest of the screen while an item is held. -/
def inventoryUiAt (x y : Nat) (crafting := true) : Bool :=
  let gridBottom := inventoryY (inventoryRows - 1) + slotSize
  let craftHeight := if crafting then classicRowY + classicRows * classicRowStride - craftY + 16 else 24
  containsRect x y slotLeft 0 (ammoX + sideSlotSize - slotLeft) gridBottom ||
    containsRect x y trashX trashY slotSize slotSize ||
    containsRect x y (gearX - 120) (gearY - 25) (120 + slotSize) (mountY - gearY + slotSize + 25) ||
    containsRect x y craftLeft craftY (recipeStride + recipeWidth) craftHeight ||
    saveQuitAt x y

/-- The arrows in the crafting tab change page without selecting a recipe. -/
def classicPageAt (page x y : Nat) : Option Nat :=
  if y < craftY || y ≥ craftY + 24 then none
  else if page > 0 && classicPrevX ≤ x && x < classicPrevX + classicPagerWidth then some (page - 1)
  else if page + 1 < classicPageCount && classicNextX ≤ x && x < classicNextX + classicPagerWidth then
    some (page + 1)
  else none

def classicRecipeAt (x y : Nat) (page := 0) : Option ClassicRecipe := do
  if x < craftLeft || y < classicRowY then none
  else
    let col := (x - craftLeft) / recipeStride
    let row := (y - classicRowY) / classicRowStride
    if col < 2 && row < classicRows && (x - craftLeft) % recipeStride < recipeWidth &&
        (y - classicRowY) % classicRowStride < classicRowHeight then
      ClassicRecipe.all[page * classicPageSize + row * 2 + col]?
    else none

structure Camera where
  x : Int
  y : Int

def cameraFor (state : GameState) : Camera :=
  let x := (state.player.x + playerWidth / 2) * tilePixels / tileUnits
  let y := (state.player.y + playerHeight / 2) * tilePixels / tileUnits
  let lift : Int := if state.eye.isSome || state.skeletron.isSome || state.skeletronPrime.isSome then 132 else 0
  { x := max 0 (min (x - width / 2) (state.world.width * tilePixels - (width : Int)))
    y := max 0 (min (y - height / 2 - lift) (state.world.height * tilePixels - (height : Int))) }

def Camera.target (camera : Camera) (x y : Nat) (blocked := false) (compactHud := true) : Option TilePos :=
  if !blocked && x < width && 96 ≤ y && y < (if compactHud then height else height - 126) then
    some ⟨(camera.x + x) / tilePixels, (camera.y + y) / tilePixels⟩
  else
    none

private def npcOrigin (position : TilePos) (camera : Camera) : Int × Int :=
  (position.x * tilePixels + 3 - camera.x,
    (position.y + 1) * tilePixels - playerHeight * tilePixels / tileUnits - camera.y)

private def bodyOrigin (body : Player) (camera : Camera) : Int × Int :=
  (body.x * tilePixels / tileUnits - camera.x, body.y * tilePixels / tileUnits - camera.y)

private def overNpc (x y : Int) (mouseX mouseY : Nat) : Bool :=
  x ≤ (mouseX : Int) && (mouseX : Int) < x + 18 &&
    y ≤ (mouseY : Int) && (mouseY : Int) < y + 33

def interactionTarget (state : GameState) (camera : Camera) (mouseX mouseY : Nat) : Option TilePos := do
  let target ← camera.target mouseX mouseY
  let tile := state.world.tile target
  if (state.cartIndex target).isSome || tile == .supplyCache || tile == .bed || tile.isFramedChest ||
      (TempleDoor.kind tile).isSome || tile.isTempleAltar ||
      state.bombs.any (·.position == target) then return target
  if let some oldMan := state.oldManPosition then
    let (x, y) := npcOrigin oldMan camera
    if x - 7 ≤ (mouseX : Int) && (mouseX : Int) < x + 25 &&
        y - 10 ≤ (mouseY : Int) && (mouseY : Int) < y + 36 then return oldMan
  if let some guide := state.townGuide then
    let (x, y) := bodyOrigin guide.body camera
    if overNpc x y mouseX mouseY then return guide.body.centerTile
  if let some angler := state.angler.position then
    let (x, y) := npcOrigin angler camera
    if x - 13 ≤ (mouseX : Int) && (mouseX : Int) < x + 32 &&
        y - 10 ≤ (mouseY : Int) && (mouseY : Int) < y + 34 then return angler
  let guideMarker := if state.legacyEarly then state.guide else none
  for npc in [state.merchant, guideMarker].filterMap id do
    let (x, y) := npcOrigin npc camera
    if overNpc x y mouseX mouseY then return npc
  return target

def backgroundColor : UInt32 := 0x111925ff
private def muted : UInt32 := 0xa2b1baff
private def paper : UInt32 := 0xf6f0dcff
private def gold : UInt32 := 0xf6bd76ff
private def mint : UInt32 := 0x89e2d0ff
private def amberColor : UInt32 := 0xe0a157ff
private def geodeColor : UInt32 := 0xb28cdfff
private def beaconColor : UInt32 := 0x71e3ffff
private def supplyColor : UInt32 := 0xf6e18cff
private def gillsColor : UInt32 := 0x94e4f5ff
private def barkskinColor : UInt32 := 0xf4c183ff
private def ruinColor : UInt32 := 0xf0bf76ff
private def ruinRune : UInt32 := 0x93ddcaff
private def railColor : UInt32 := 0xd0d4ccff
private def cartColor : UInt32 := 0xffb16fff
private def hookColor : UInt32 := 0xffd193ff
private def lifeColor : UInt32 := 0xff9eacff
private def shardColor : UInt32 := 0xe0a1f2ff
private def dungeonColor : Tile → UInt32
  | .greenDungeonBrick => 0x577c6aff
  | .pinkDungeonBrick => 0x95677fff
  | _ => 0x63789bff

private def dungeonWallAsset : Tile → String
  | .greenDungeonBrick => "Wall_8"
  | .pinkDungeonBrick => "Wall_9"
  | _ => "Wall_7"
private def highlandColor : UInt32 := 0xa4d09aff
private def berryColor : UInt32 := 0xffb873ff
private def deepsteelColor : UInt32 := 0xb9eeedff
private def cavernColor : UInt32 := 0xc8b4f4ff
private def cavernLowColor : UInt32 := 0xffb481ff
private def cavernHighColor : UInt32 := 0xe5bcfcff
private def cavernCoreColor : UInt32 := 0x92efd1ff
private def magmaColor : UInt32 := 0xffaa71ff
private def obsidianColor : UInt32 := 0xbba4d4ff
private def shadowChestColor : UInt32 := 0xc597e5ff
private def fireColor : UInt32 := 0xffa16fff
private def bedroomColor : UInt32 := 0xacc5f1ff
private def eyeIrisColor : UInt32 := 0x84c7eeff
private def eyeMawColor : UInt32 := 0xffa693ff
private def demoniteColor : UInt32 := 0xbda3eaff
private def crimtaneColor : UInt32 := 0xe68a78ff

private def drawTorch (x y : Int) : IO Unit := do
  discard <| originalSprite "Tiles_4" (x - 10) y 20 24 0 0 20 20 (fit := true)

private def drawWoodWall (x y : Int) (col row : Nat) : IO Unit := do
  discard <| originalSprite "Wall_4" x y 24 24 ((col + row) % 3 * 36 + 8) 44 16 16

private def drawCobweb (x y : Int) : IO Unit := do
  discard <| originalSprite "Tiles_51" x y 24 24 18 18 16 16

private def drawFurniture (furniture : Furniture) (camera : Camera) (spawnBound : Bool) : IO Unit := do
  let x := furniture.position.x * tilePixels - camera.x
  let bottom := (furniture.position.y + 1) * tilePixels - camera.y
  let w := furniture.kind.width * tilePixels
  let h := furniture.kind.height * tilePixels
  let y := bottom - h
  if x + w < 0 || x ≥ width || bottom < 0 || y ≥ height then return
  let atlas := match furniture.kind with
    | .bed => some ("Tiles_79", 0)
    | .furnace => some ("Tiles_17", 0)
    | .ironAnvil => some ("Tiles_16", 0)
    | .leadAnvil => some ("Tiles_16", 36)
    | .sawmill => some ("Tiles_106", 0)
    | .loom => some ("Tiles_86", 0)
    | .woodChair => some ("Tiles_15", 0)
    | .woodTable => some ("Tiles_14", 0)
    | .hellforge => some ("Tiles_77", 0)
    | .adamantiteForge => some ("Tiles_133", 0)
    | .titaniumForge => some ("Tiles_133", 54)
    | .mythrilAnvil => some ("Tiles_134", 0)
    | .orichalcumAnvil => some ("Tiles_134", 36)
    | .woodenCrate => some ("Tiles_376", 0)
    | .ironCrate => some ("Tiles_376", 36)
    | .goldenCrate => some ("Tiles_376", 72)
    | .pearlwoodCrate => some ("Tiles_376", 324)
    | .mythrilCrate => some ("Tiles_376", 360)
    | .titaniumCrate => some ("Tiles_376", 396)
    | .biomeCrate item =>
      match item with
      | .corruptCrate => some ("Tiles_376", 108)
      | .crimsonCrate => some ("Tiles_376", 144)
      | .dungeonCrate => some ("Tiles_376", 180)
      | .skyCrate => some ("Tiles_376", 216)
      | .hallowedCrate => some ("Tiles_376", 252)
      | .jungleCrate => some ("Tiles_376", 288)
      | .defiledCrate => some ("Tiles_376", 432)
      | .hematicCrate => some ("Tiles_376", 468)
      | .stockadeCrate => some ("Tiles_376", 504)
      | .azureCrate => some ("Tiles_376", 540)
      | .divineCrate => some ("Tiles_376", 576)
      | .brambleCrate => some ("Tiles_376", 612)
      | .frozenCrate => some ("Tiles_376", 648)
      | .borealCrate => some ("Tiles_376", 684)
      | .oasisCrate => some ("Tiles_376", 720)
      | .mirageCrate => some ("Tiles_376", 756)
      | .obsidianCrate => some ("Tiles_376", 792)
      | .hellstoneCrate => some ("Tiles_376", 828)
      | .oceanCrate => some ("Tiles_376", 864)
      | .seasideCrate => some ("Tiles_376", 900)
      | _ => none
  if let some (asset, styleX) := atlas then
    for row in [:furniture.kind.height] do
      for col in [:furniture.kind.width] do
        let sourceCol := if furniture.flipped then furniture.kind.width - 1 - col else col
        discard <| originalSprite asset (x + col * tilePixels) (y + row * tilePixels)
          tilePixels tilePixels (styleX + sourceCol * 18) (row * 18) 16 16
          (mirror := furniture.flipped)
  if spawnBound then
    discard <| originalSprite "UI/SpawnBed" (x + 15) (y - 17) 18 18 (fit := true)

private def drawDoor (x y : Int) (openDoor lower : Bool) (middle := false) : IO Unit := do
  let frame := if lower then 2 else if middle then 1 else 0
  if openDoor then
    discard <| originalSprite "Tiles_11" x y 48 24 0 (18 * frame) 34 16
  else
    discard <| originalSprite "Tiles_10" x y 24 24 0 (18 * frame) 16 16

private def drawPlatform (x y : Int) : IO Unit := do
  discard <| originalSprite "Tiles_19" x y 24 24 18 0 16 16

private def drawMinecart (x axleY : Int) (_occupied : Bool) (wheelFrame := false) : IO Unit := do
  discard <| originalSprite "Mount_Minecart" (x - 25) (axleY - 34) 50 34
    0 (if wheelFrame then 34 else 0) 50 34

private def drawCartBody (camera : Camera) (body : Player) (occupied : Bool) (wheelFrame := false) : IO Unit := do
  let x := (body.x + playerWidth / 2) * tilePixels / tileUnits - camera.x
  let y := (body.y + playerHeight) * tilePixels / tileUnits - camera.y
  if -12 ≤ x && x < (width : Int) + 12 && -4 ≤ y && y < (height : Int) + 15 then
    drawMinecart x (y - 5) occupied wheelFrame

private def drawHomeBeacon (x y : Int) (_bound : Bool) : IO Unit := do
  discard <| originalSprite "Item_4876" x y 24 24 (fit := true)

private def drawSupplyCache (x y : Int) (opened : Bool) : IO Unit := do
  discard <| originalSprite "Tiles_21" x y 24 24 0 (if opened then 76 else 0) 34 34 (fit := true)

private def drawRuinAltar (x y : Int) : IO Unit := do
  discard <| originalSprite "Item_544" x y 24 24 (fit := true)

private def drawCavernAltar (x y : Int) (_ready : Bool) : IO Unit := do
  discard <| originalSprite "Item_4988" x y 24 24 (fit := true)

private def cropTicksAt (state : GameState) (position : TilePos) : Nat :=
  ((state.crops.find? (·.position == position)).map (·.ticks)).getD 0

private def drawSunleaf (x y : Int) (ripe : Bool) (ticks : Nat) : IO Unit := do
  let frame := if ripe then 36 else if ticks > 0 then 18 else 0
  discard <| originalSprite "Tiles_82" x y 24 24 0 frame 16 16

private def drawLifeCrystalTile (x y : Int) (_clock : Nat) : IO Unit := do
  discard <| originalSprite "Item_29" x y 24 24 (fit := true)

private def drawEvilAltar (x bottom : Int) (crimson : Bool) : IO Unit := do
  let first := if crimson then 54 else 0
  for row in [:2] do
    for col in [:3] do
      discard <| originalSprite "Tiles_26" (x + col * tilePixels) (bottom - 48 + row * tilePixels)
        tilePixels tilePixels (first + col * 18) (row * 18) 16 16

private def altarAnchorAt (world : World) (position : TilePos) (tile : Tile) : TilePos :=
  let row := if world.tile { position with y := position.y + 1 } == tile then position.y + 1 else position.y
  let col := if world.tile ⟨position.x - 1, row⟩ != tile then position.x
    else if world.tile ⟨position.x - 2, row⟩ == tile then position.x - 2 else position.x - 1
  ⟨col, row⟩

private def orbColor : UInt32 := 0xc69af3ff
private def crimsonHeartColor : UInt32 := 0xf78886ff
private def wormColor : UInt32 := 0xb88fd5ff
private def brainColor : UInt32 := 0xf78983ff

private def evilRegion (state : GameState) : Option Bool :=
  if (evilAltars state.seed)[0]?.any (fun altar => state.world.tile altar == altarTile state.seed) then
    some (evilRelicTile state.seed == .crimsonHeart)
  else none

private def relicColor (crimson : Bool) : UInt32 :=
  if crimson then crimsonHeartColor else orbColor

private def nearGunDrop (state : GameState) : Bool :=
  state.nightDrops.any fun drop => state.canReach drop.position &&
    drop.items.any (fun stack => stack.item.gunShot.isSome)

private def nearEvilBossDrop (state : GameState) : Bool :=
  state.nightDrops.any fun drop => state.canReach drop.position &&
    drop.items.any (fun stack => stack.item == .shadowScale || stack.item == .tissueSample)

def evilObjective (state : GameState) : Option String := do
  let crimson ← evilRegion state
  let player := state.player.centerTile
  if !inEvilBiome state.seed player then none
  else if nearEvilBossDrop state then
    some (if crimson then "Collect the Tissue Samples and Crimtane Ore."
      else "Collect the Shadow Scales and Demonite Ore.")
  else if nearGunDrop state then
    some (if crimson then "Walk over the Undertaker and Musket Balls to collect them."
      else "Walk over the Musket and Musket Balls to collect them.")
  else if player.y < (evilSurfaceHeight state.seed player.x.toNat : Int) + 1 then
    some (if crimson then "Look for the sloping cave mouth to enter the Crimson."
      else "Follow a chasm down to the Corruption tunnels.")
  else if state.evilRelicsBroken == 0 then
    some (if crimson then "Use a Wooden Hammer or a bomb to break a Crimson Heart."
      else "Use a Wooden Hammer or a bomb to break a Shadow Orb.")
  else if state.evilRelicsBroken ≥ (evilRelics state.seed).size then
    some (if state.legacyEarly then "All three relics are broken."
      else if crimson then "Craft Bloody Spine at an altar to summon the Brain here."
      else "Craft Worm Food at an altar to summon the Eater here.")
  else if state.evilRelicsBroken % 3 == 2 then
    some (if crimson then "The next Crimson Heart summons the Brain of Cthulhu."
      else "The next Shadow Orb summons the Eater of Worlds.")
  else
    some (if crimson then "Break three Crimson Hearts to summon the Brain of Cthulhu."
      else "Break three Shadow Orbs to summon the Eater of Worlds.")

private def drawEvilRelic (x y : Int) (crimson : Bool) (clock : Nat) : IO Unit := do
  let first := if crimson then 36 else 0
  let frame := (clock / 9 % 4) * 36
  for row in [:2] do
    for col in [:2] do
      discard <| originalSprite "Tiles_31" (x + col * tilePixels) (y + row * tilePixels)
        tilePixels tilePixels (first + col * 18) (frame + row * 18) 16 16

private def drawClayPot (x y : Int) : IO Unit := do
  for row in [:2] do
    for col in [:2] do
      discard <| originalSprite "Tiles_28" (x + col * tilePixels) (y + row * tilePixels)
        tilePixels tilePixels (col * 18) (row * 18) 16 16

private def drawEvilMushroom (x y : Int) (crimson : Bool) : IO Unit := do
  discard <| originalSprite (if crimson then "Item_2887" else "Item_60") x (y + 4) 24 20 (fit := true)

private def drawBottle (x y : Int) : IO Unit := do
  discard <| originalSprite "Item_31" x y 24 24 (fit := true)

private def drawHighlandBerry (x y : Int) : IO Unit := do
  discard <| originalSprite "Item_4282" x y 24 24 (fit := true)

private def metalOreColor : Tile → UInt32
  | .tinOre => 0xb1b597ff
  | .leadOre => 0x9397bdff
  | .tungstenOre => 0x82c5a4ff
  | .goldOre => 0xf5c757ff
  | .platinumOre => 0xc7b7edff
  | _ => 0xd6e3eaff

private def hardmodeOreStyle : Tile → Option (String × UInt32 × String)
  | .cobaltOre => some ("COBALT ORE", 0x53a4dfff, "Tiles_107")
  | .palladiumOre => some ("PALLADIUM ORE", 0xeb985cff, "Tiles_221")
  | .mythrilOre => some ("MYTHRIL ORE", 0x6db89eff, "Tiles_108")
  | .orichalcumOre => some ("ORICHALCUM ORE", 0xd97cc4ff, "Tiles_222")
  | .adamantiteOre => some ("ADAMANTITE ORE", 0xe36574ff, "Tiles_111")
  | .titaniumOre => some ("TITANIUM ORE", 0xaca8cfff, "Tiles_223")
  | _ => none

private def originalTile : Tile → Option String
  | .dirt => some "Tiles_0"
  | .stone | .bedrock => some "Tiles_1"
  | .crystal => some "Tiles_68"
  | .amber => some "Tiles_566"
  | .geode => some "Tiles_67"
  | .deepsteel => some "Tiles_107"
  | .ruinBrick => some "Tiles_38"
  | .mineMarker => some "Tiles_55"
  | .timber => some "Tiles_30"
  | .ice => some "Tiles_161"
  | .copper => some "Tiles_7"
  | .iron => some "Tiles_6"
  | .silver => some "Tiles_9"
  | .goldOre => some "Tiles_8"
  | .tinOre => some "Tiles_166"
  | .leadOre => some "Tiles_167"
  | .tungstenOre => some "Tiles_168"
  | .platinumOre => some "Tiles_169"
  | .demoniteOre => some "Tiles_22"
  | .crimtaneOre => some "Tiles_204"
  | .obsidian => some "Tiles_56"
  | .honeyBlock => some "Tiles_229"
  | .crispyHoneyBlock => some "Tiles_230"
  | .ash => some "Tiles_57"
  | .hellstone => some "Tiles_58"
  | .obsidianBrick => some "Tiles_75"
  | .lihzahrdBrick => some "Tiles_226"
  | .woodenSpikes => some "Tiles_232"
  | .demoniteBrick => some "Tiles_140"
  | .crimtaneBrick => some "Tiles_347"
  | .hallowedGrass => some "Tiles_109"
  | .pearlstone => some "Tiles_117"
  | .pearlsand => some "Tiles_116"
  | .ebonsand => some "Tiles_112"
  | .crimsand => some "Tiles_234"
  | .purpleIce => some "Tiles_163"
  | .pinkIce => some "Tiles_164"
  | .redIce => some "Tiles_200"
  | .corruptGrass => some "Tiles_23"
  | .crimsonGrass => some "Tiles_199"
  | .ebonstone => some "Tiles_25"
  | .crimstone => some "Tiles_203"
  | .sand => some "Tiles_53"
  | .mud => some "Tiles_59"
  | .jungleGrass => some "Tiles_60"
  | .glassBlock => some "Tiles_54"
  | .blueDungeonBrick => some "Tiles_41"
  | .greenDungeonBrick => some "Tiles_43"
  | .pinkDungeonBrick => some "Tiles_44"
  | tile => (hardmodeOreStyle tile).map (·.2.2)

private def drawOriginalBlock (world : World) (position : TilePos) (tile : Tile)
    (x y : Int) (variation : Nat) (surface : Bool) : IO Bool := do
  if tile.isDoor then
    drawDoor x y (tile == .doorOpen || tile == .doorOpenTop || tile == .doorOpenMiddle) (tile == .doorClosed || tile == .doorOpen)
      (tile == .doorClosedMiddle || tile == .doorOpenMiddle)
    return true
  if tile == .workbench || tile == .chest then
    return ← originalSprite (if tile == .workbench then "Item_36" else "Item_48") x y 24 24 (fit := true)
  let asset := if tile == .dirt && surface then some "Tiles_2" else originalTile tile
  let some name := asset | return false
  let exposedAbove := !(world.tile { position with y := position.y - 1 }).solid
  let exposedBelow := !(world.tile { position with y := position.y + 1 }).solid
  let exposedLeft := !(world.tile { position with x := position.x - 1 }).solid
  let exposedRight := !(world.tile { position with x := position.x + 1 }).solid
  let frameX := if exposedLeft then 0 else if exposedRight then 72 else (1 + variation % 3) * 18
  let frameY := if exposedAbove then 0 else if exposedBelow then 36 else 18
  originalSprite name x y 24 24 frameX frameY 16 16

private def drawCaveWall (asset : String) (x y : Int) (col row : Nat) : IO Unit :=
  discard <| originalSprite asset x y 24 24 ((col + row) % 3 * 36 + 8) 44 16 16

private def mixSky (dark bright : UInt32) (level : Nat) : UInt32 :=
  let channel (shift : UInt32) :=
    let low := ((dark >>> shift) &&& 255).toNat
    let high := ((bright >>> shift) &&& 255).toNat
    ((low * (15 - level) + high * (level - 4)) / 11).toUInt32 <<< shift
  channel 24 ||| channel 16 ||| channel 8 ||| 255

private def drawTreeTrunk (x y : Int) (variation light : Nat) (root : Bool) : IO Unit := do
  let amount := (75 + 12 * light).toUInt32
  let tint := (amount <<< 24) ||| (amount <<< 16) ||| (amount <<< 8) ||| 255
  discard <| originalSprite "Tiles_5" x y 24 24 0 (if root then 132 else 22 * (variation % 3))
      20 20 (color := tint)

private def drawTreeCrown (x y : Int) (variation light : Nat) : IO Unit := do
  let amount := (75 + 12 * light).toUInt32
  let tint := (amount <<< 24) ||| (amount <<< 16) ||| (amount <<< 8) ||| 255
  discard <| originalSprite "Tree_Tops_0" (x - 33) (y - 60) 90 90 (variation % 3 * 82) 0 82 82
      (color := tint)

private def drawHighlandRidges (camera : Camera) (light : Nat) : IO Unit := do
  let tint := mixSky 0x4f5671ff 0xffffffff light
  let start := -((camera.x / 4) % 1024)
  for col in [:3] do
    discard <| originalSprite "Background_7" (start + col * 1024) (740 - camera.y) 1024 435 (color := tint)

private def drawCloud (x y : Int) (light : Nat) : IO Unit := do
  let opacity := ((light - 3) * 11 + 27).toUInt32
  discard <| originalSprite "Cloud_1" (x - 10) (y - 12) 92 36
      (color := 0xffffff00 ||| min 220 (opacity + 35))

private def drawEvilRidges (camera : Camera) (light : Nat) (crimson : Bool) : IO Unit := do
  let tint := mixSky 0x584456ff 0xffffffff light
  let asset := if crimson then "Background_93" else "Background_43"
  let start := -((camera.x / 4) % 1024)
  for col in [:3] do
    discard <| originalSprite asset (start + col * 1024) (735 - camera.y) 1024 530 (color := tint)

private def hardmodeBiome (state : GameState) : Option HardmodeBiome := Id.run do
  let player := state.player.centerTile
  if !state.hardmode || player.y ≥ underworldStart then return none
  -- Native scene metrics require 125 Hallow or 300 evil tiles. This smaller
  -- world samples the 47 × 29 tile neighborhood used by the desktop view.
  let mut hallow := 0
  let mut corruption := 0
  let mut crimson := 0
  for row in [:29] do
    for col in [:47] do
      match state.world.tile ⟨player.x + col - 23, player.y + row - 14⟩ with
      | .hallowedGrass | .pearlstone | .pearlsand | .pinkIce => hallow := hallow + 1
      | .corruptGrass | .ebonstone | .ebonsand | .purpleIce => corruption := corruption + 1
      | .crimsonGrass | .crimstone | .crimsand | .redIce => crimson := crimson + 1
      | _ => pure ()
  if hallow ≥ 125 + corruption + crimson then return some .hallow
  if corruption ≥ 300 + hallow && corruption ≥ crimson then return some .corruption
  if crimson ≥ 300 + hallow then return some .crimson
  return none

private def drawHallowRidges (camera : Camera) (surface : Nat) (light : Nat) : IO Unit := do
  let tint := mixSky 0x555779ff 0xffffffff light
  -- Local Terraria style 1 uses three layers (102–104) and native
  -- horizontal parallax factors .40/.43/.49.
  let layers : List (String × Nat × Nat × Nat × Nat) := [
      ("Background_102", 720, 384, 40, 123),
      ("Background_103", 820, 437, 43, 58),
      ("Background_104", 960, 512, 49, 0)]
  for (asset, span, imageHeight, drift, lift) in layers do
    let floor : Int := (surface * tilePixels : Nat) - camera.y - lift
    let y := floor - (imageHeight * 5 / 8 : Nat)
    let x : Int := -((camera.x * drift / 100) % span)
    for column in [:2 + width / span] do
      let _ ← originalSprite asset (x + column * span) y span imageHeight (color := tint)

private def drawSky (camera : Camera) (clock light : Nat) (legacyEarly : Bool) (weather : Weather)
    (evil : Option Bool) (hallowSurface : Option Nat) : IO Unit := do
  let (top, bottom) := if hallowSurface.isSome then
      (mixSky 0x171d41ff 0x6087bdff light, mixSky 0x414572ff 0xe3bdd9ff light)
    else match evil with
    | some true => (mixSky 0x23172dff 0x785470ff light, mixSky 0x694054ff 0xd39787ff light)
    | some false => (mixSky 0x161631ff 0x575779ff light, mixSky 0x514262ff 0xad8ab5ff light)
    | none => (mixSky 0x0b183aff 0x477d91ff light, mixSky 0x31436aff 0xc7c5a0ff light)
  discard <| originalSprite "Background_0" 0 0 width height
    (color := mixSky top bottom 10)
  if light < 11 then
    let shine := 0xc7e7ff00 ||| ((11 - light) * 25).toUInt32
    for index in [:24] do
      let x := (43 + (index : Int) * 167 - camera.x / 5) % 1190
      let y := 670 + (index * 41 % 214) - camera.y
      discard <| originalSprite "Star_0" x y 7 7 (color := shine)
  let night := isNight clock legacyEarly
  let cycle := phaseCycleTicks legacyEarly
  let day := phaseDayTicks legacyEarly
  let elapsed := clock % cycle - if night then day else 0
  let duration := if night then cycle - day else day
  let arc := 180 * 4 * elapsed * (duration - elapsed) / (duration * duration)
  let orbX := if legacyEarly then (865 - camera.x / 6) % 1260
    else 72 + (970 * elapsed / duration : Nat)
  let orbY := if legacyEarly then 762 - camera.y else 854 - camera.y - arc
  if night then
    let phase := if legacyEarly then 3 else weather.moonPhase
    discard <| originalSprite "Moon_0" orbX orbY 30 30 0 (50 * phase) 50 50
  else
    discard <| originalSprite "Sun" (orbX - 7) (orbY - 7) 42 42
  for index in [:8] do
    let x := ((170 + (index : Int) * 203 - camera.x / 4) % 1450) - 100
    let y := 728 + (index % 3) * 26 - camera.y
    drawCloud x y light
  let overcast := max weather.cloudOpacity (weather.rainStrength.toFloat.toFloat32 / 200)
  if overcast > 0 then
    let opacity := (overcast * 210).toUInt32
    rect 0 0 width height (0x2a354700 ||| (opacity / 3))
    let tint := (mixSky 0x52607aff 0xbbcad8ff light &&& 0xffffff00) ||| opacity
    let layers : List (String × Nat × Nat × Int × Nat) :=
      [("Background_112", 2048, 434, 9, 480), ("Background_113", 1024, 550, 15, 560)]
    for (asset, span, tall, parallax, altitude) in layers do
      let x := -((camera.x * parallax / 100 + (clock / 90 : Nat)) % span)
      let y : Int := (altitude : Nat) - camera.y
      for column in [:2 + width / span] do
        let _ ← originalSprite asset (x + column * span) y span tall (color := tint)
  if let some crimson := evil then drawEvilRidges camera light crimson
  if let some surface := hallowSurface then drawHallowRidges camera surface light
  else drawHighlandRidges camera light

private def drawRain (state : GameState) (camera : Camera) : IO Unit := do
  if !state.weather.raining then return
  let weather := state.weather
  let clock := state.surface.clock
  let tint := (mixSky 0x677a93ff 0xb9d2e6ff (daylight clock false) &&& 0xffffff00) ||| 162
  for index in [:20 + weather.rainStrength * 2] do
    let drift := clock * (8 + index % 5)
    let x := (index * 193 + index * index * 23 + drift / 12) % width
    let y := (index * 71 + index * index * 13 + drift) % (height + 40)
    let worldX := (camera.x + x) / tilePixels
    let worldY := (camera.y + y) / tilePixels
    let tile : TilePos := ⟨worldX, worldY⟩
    if y < height && state.world.contains tile && !(state.world.tile tile).solid &&
        state.world.clearSkyAbove tile then
      discard <| originalSprite "Rain" x y 2 32 (index % 3 * 4) 0 2 40 (color := tint) (rotation := -5)
private def drawUnderworldBackdrop (camera : Camera) (clock : Nat) : IO Unit := do
  let seam : Int := (underworldStart : Int) * tilePixels - camera.y
  let top := max 0 seam
  if top ≥ height then return
  rect 0 top width (height - top.toNat) 0x351722ff
  let horizon : Int := (underworldStart + 26 : Nat) * tilePixels - camera.y
  -- Terraria's background set 0 draws its far gradient first, then its
  -- four cave silhouettes from layer 3 through 0.
  let _ ← originalSprite "Backgrounds/Underworld 4" 0 top width (height - top.toNat) 0 280 48 860
  let layers : List (Nat × Nat × Nat) := [(3, 630, 372), (2, 630, 358), (1, 950, 540), (0, 858, 715)]
  for (layer, sourceHeight, floorPixel) in layers do
    let destinationWidth : Nat := 1024 * 39 / 20
    let depth := 3 + 2 * layer
    let y := horizon - (floorPixel * 39 / 20 : Nat) - (3 - layer : Nat) * 18 - seam / depth
    let crop := min sourceHeight (((top - y).toNat * 20 + 38) / 39)
    let drawY := y + (crop * 39 / 20 : Nat)
    let drawHeight := (sourceHeight - crop) * 39 / 20
    if drawHeight == 0 || drawY ≥ height then continue
    let frame := if layer == 1 then clock * 8 / 60 % 4 else 0
    let sourceX := frame / 2 * 1024
    let sourceY := frame % 2 * 950 + crop
    let firstX : Int := -((camera.x / depth) % destinationWidth)
    for column in [:2 + width / destinationWidth] do
      let _ ← originalSprite s!"Backgrounds/Underworld {layer}" (firstX + column * destinationWidth)
        drawY destinationWidth drawHeight sourceX sourceY 1024 (sourceHeight - crop)

private def drawEvilBackdrop (x y : Int) (col row : Nat) (crimson : Bool) : IO Unit := do
  drawCaveWall (if crimson then "Wall_83" else "Wall_3") x y col row

private def inFrostCavern (col : Nat) : Bool :=
  frostCavernStart ≤ col && col ≤ frostCavernEnd

private def inWardenHall (col row : Nat) : Bool :=
  wardenArenaStart ≤ col && col < wardenArenaEnd && 55 ≤ row && row < wardenArenaFloor

private def inQuarry (col : Nat) : Bool := quarryStart ≤ col && col ≤ quarryEnd

private def drawFrostBackdrop (x y : Int) (variation : Nat) : IO Unit := do
  drawCaveWall "Wall_71" x y variation 0

private def drawWardenBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_7" x y col row

private def drawQuarryBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_1" x y col row

private def drawStormBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_40" x y col row

private def drawRuinBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_7" x y col row

private def inRuinDescent (position : TilePos) : Bool :=
  ruinEntrance.x - 2 ≤ position.x && position.x ≤ ruinShaftColumn + 3 &&
    ruinEntrance.y ≤ position.y && position.y < ruinShaftFloor

private def drawRuinEntrance (state : GameState) (camera : Camera) : IO Unit := do
  let x := ruinAltar.x * tilePixels - camera.x
  let y := ruinAltar.y * tilePixels - camera.y
  if state.canReach ruinAltar then
    discard <| originalSprite "Item_544" x (y - 22) 20 20 (fit := true)

private def drawAmberStone (x y : Int) (col row : Nat) (surface : Bool) : IO Unit := do
  let asset := if surface then "Tiles_2" else "Tiles_1"
  discard <| originalSprite asset x y 24 24 (18 * (1 + (col + row) % 3)) 18 16 16

private def drawGeodeBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_1" x y col row

private def drawHighlandSoil (x y : Int) (tile : Tile) (surface : Bool) (variation : Nat) : IO Unit := do
  let asset := if tile == .stone then "Tiles_1" else if surface then "Tiles_2" else "Tiles_0"
  discard <| originalSprite asset x y 24 24 (18 * (1 + variation % 3)) 18 16 16

private def drawMineBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_1" x y col row

private def drawMineMarker (x y : Int) : IO Unit := do
  discard <| originalSprite "Item_171" x y 24 24 (fit := true)

private def drawCavernBackdrop (x y : Int) (col row : Nat) (_ledge : Bool) : IO Unit := do
  drawCaveWall "Wall_7" x y col row

private def drawMagmaBackdrop (x y : Int) (col row : Nat) : IO Unit := do
  drawCaveWall "Wall_14" x y col row

private def drawMagmaMarker (x y : Int) : IO Unit := do
  discard <| originalSprite "Item_4263" x y 24 24 (fit := true)

private def drawMagmaGate (x y : Int) : IO Unit := do
  discard <| originalSprite "Tiles_10" x y 24 24 0 18 16 16

private def drawMineEntrance (camera : Camera) (position : TilePos) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  discard <| originalSprite "Item_171" x y 24 24 (fit := true)

private def drawCavernEntrance (camera : Camera) (position : TilePos) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  discard <| originalSprite "Item_4988" x y 24 24 (fit := true)

private def drawMagmaEntrance (camera : Camera) (position : TilePos) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  discard <| originalSprite "Item_4263" x y 24 24 (fit := true)

private def drawHighlandSign (camera : Camera) (position : TilePos) (_heightOffset : Int) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  discard <| originalSprite "Item_171" x y 24 24 (fit := true)

private def liquidDepth (amount : Nat) : Nat :=
  (tilePixels * amount + Water.capacity - 1) / Water.capacity

private def drawWater (x y : Int) (amount light : Nat) (surface : Bool) : IO Unit := do
  let depth := liquidDepth amount
  -- The first four rows of the atlas draw the surface highlight.
  let sourceY := if surface then 0 else 4
  discard <| originalSprite "Liquid_0" x (y + tilePixels - depth) tilePixels depth
    0 sourceY 16 (16 - sourceY) (color := if light ≥ 7 then 0xffffffb9 else 0x7a90c2ae)

private def drawMagma (x y : Int) (amount _animation : Nat) (surface : Bool) : IO Unit := do
  let depth := liquidDepth amount
  discard <| originalSprite "Liquid_1" x (y + tilePixels - depth) tilePixels depth
    (color := if surface then 0xffffffff else 0xe6bda9ff)

private def drawHoney (x y : Int) (amount light : Nat) (surface : Bool) : IO Unit := do
  let depth := liquidDepth amount
  let sourceY := if surface then 0 else 4
  let channel := min 255 (95 + 11 * light).toUInt32
  let tint := (channel <<< 24) ||| (channel <<< 16) ||| (channel <<< 8) ||| 255
  discard <| originalSprite "Liquid_11" x (y + tilePixels - depth) tilePixels depth
    0 sourceY 16 (16 - sourceY) (color := tint)

private def shadeCaveTile (x y : Int) (level : Nat) (deep frozen : Bool) : IO Unit := do
  if level < Lighting.maxLevel then
    let opacity := min 166 ((Lighting.maxLevel - level) * 12)
    rect x y tilePixels tilePixels (0x07122400 ||| opacity.toUInt32)
  if deep && level > 7 then
    let tint := min 39 ((level - 7) * 6)
    let color : UInt32 := if frozen then 0x9ec8ff00 else 0xffa65500
    rect x y tilePixels tilePixels (color ||| tint.toUInt32)

private def drawWorkbenchHint (x y : Int) (nearby : Bool) : IO Unit := do
  rect (x - 38) (y - 24) 101 18 (if nearby then 0x412f25f5 else 0x212733eb)
  text "WORKBENCH" (x - 31) (y - 21) 12 gold

private def drawDungeonWall (brick : Tile) (x y : Int) (col row : Nat) : IO Unit := do
  -- Take the tile-sized center of a 32-pixel dungeon wall frame.
  let sourceX := 8 + 36 * ((col + row * 2) % 3)
  discard <| originalSprite (dungeonWallAsset brick) x y 24 24 sourceX 44 16 16

private def drawObsidianWall (x y : Int) (col row : Nat) : IO Unit := do
  let sourceX := 8 + 36 * ((col + row * 2) % 3)
  discard <| originalSprite "Wall_14" x y 24 24 sourceX 44 16 16

private def drawFramedChestTile (tile : Tile) (x y : Int) (position anchor : TilePos) (opened : Bool) : IO Unit := do
  let dx := (position.x - anchor.x).toNat
  let dy := (position.y - anchor.y).toNat
  let first := match tile with
    | .dungeonKeyChest => 0
    | .dungeonGoldChest => 72
    | .shadowOpenChest => 108
    | .shadowChest => 144
    | .lihzahrdChest => 576
    | _ => 36
  let frame := if opened then 76 else 0
  discard <| originalSprite "Tiles_21" x y 24 24 (first + 18 * dx) (frame + 18 * dy) 16 16

private def drawRailTrack (state : GameState) (position : TilePos) (camera : Camera) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  let traversed := state.ride.bind fun ride => ride.next.bind fun next =>
    if ride.rail == position then some next
    else if next == position then some ride.rail
    else none
  let grade (direction : Int) :=
    let neighbor := (traversed.filter (·.x == position.x + direction)).orElse fun _ =>
      railNeighbor state.world position direction
    (neighbor.map (·.y - position.y)).getD 0
  let left := grade (-1)
  let right := grade 1
  let frame := if left < 0 || right > 0 then 7 else if right < 0 || left > 0 then 6 else 1
  discard <| originalSprite "Tiles_314" x y 24 24 (frame % 8 * 18) (frame / 8 * 18) 16 16

private def npcPixelTile (x y : Float32) : TilePos :=
  ⟨(x / 16).floor.toInt32.toInt, (y / 16).floor.toInt32.toInt⟩

private def waterSphereCenter (sphere : WaterSphere) : Float32 × Float32 :=
  (sphere.x + (Int32.ofInt waterSphereWidth).toFloat32 / 2,
    sphere.y + (Int32.ofInt waterSphereHeight).toFloat32 / 2)

private def drawBoneSerpent (serpent : BoneSerpent) (camera : Camera) (light : LightField) : IO Unit := do
  for index in [:serpent.segments.size] do
    let some part := serpent.partAt index | continue
    let segment := serpent.segments[index]!
    let x := (segment.centerX * 1.5).toInt32.toInt - camera.x
    -- Terraria adds 26 px to the normal bottom-anchored draw pivot.
    let y := ((segment.y + 7) * 1.5).toInt32.toInt - camera.y
    if x + 75 < 0 || x ≥ width + 75 || y + 75 < 0 || y ≥ height + 75 then continue
    let shade := (75 + 12 * light.level (npcPixelTile segment.centerX segment.centerY)).toUInt32
    let tint := (shade <<< 24) ||| (shade <<< 16) ||| (shade <<< 8) ||| 255
    let degrees := segment.rotation.toFloat * 57.29577951308232
    discard <| originalSprite s!"NPC_{part.sprite}" (x - 31) (y - 67) 63 135 0 0 42 90
        (color := tint) (rotation := degrees)
private def npcScreenPixel (pixel : Float32) (cameraOffset : Int) : Int :=
  (pixel * 1.5).toInt32.toInt - cameraOffset

private def drawDagger (blade : EnchantedDagger) (camera : Camera) : IO Unit := do
  let drawPose (pose : DaggerPose) (glowFrame : Nat) (color : UInt32) := do
    let x := npcScreenPixel pose.x camera.x
    let y := npcScreenPixel pose.y camera.y
    discard <| originalSprite "Projectile_864" (x - 13) (y - 24) 27 48 0 glowFrame 18 32
      (color := color) (rotation := pose.rotation.toFloat * 57.29577951308232)
  for i in [:blade.trail.size] do
    let index := blade.trail.size - i - 1
    let pose := blade.trail[index]!
    if pose.x != blade.pose.x || pose.y != blade.pose.y then
      let alpha := 132 * (bladeStaffTrailLength - index) / bladeStaffTrailLength
      drawPose pose 32 (0xffffff00 ||| alpha.toUInt32)
  drawPose blade.pose 0 0xffffffff

private def drawMinionTarget (state : GameState) (camera : Camera) : IO Unit := do
  let some box := state.minionTargetBox | return
  let x := (box.left + box.right) * tilePixels / (2 * tileUnits) - camera.x
  let y := (box.top + box.bottom) * tilePixels / (2 * tileUnits) - camera.y
  if -12 ≤ x && x < width + 12 && -12 ≤ y && y < height + 12 then
    discard <| originalSprite "Extra_199" (x - 12) (y - 12) 24 24
      0 (18 * (state.surface.clock / 6 % 4)) 16 16 (color := 0xffffffb2)

private def npcLightTint (light : LightField) (x y : Float32) : UInt32 :=
  let shade := (75 + 12 * light.level (npcPixelTile x y)).toUInt32
  (shade <<< 24) ||| (shade <<< 16) ||| (shade <<< 8) ||| 255

private def drawCavernCrystal (world : World) (position : TilePos) (tile : Tile) (x y : Int)
    (clock : Nat) : IO Unit := do
  let row := (world.crystalShardFrame position).getD 0
  let gelatin := tile == .gelatinCrystal
  let variation := position.x.toNat * 31 + position.y.toNat * 17
  let column := if gelatin then 18 + clock / 8 % 6 else variation % 18
  let sx := 18 * column
  let sy := 18 * row
  let x := x + if row == 2 then 3 else if row == 3 then -3 else 0
  let y := y + if row == 0 then 3 else if row == 1 then -3 else 0
  if gelatin then
    let pulse := (clock / 8 + variation) % 6
    let tint : UInt32 := if pulse < 2 then 0xd09af547 else if pulse < 4 then 0x9baef747 else 0xf4b5de47
    for (dx, dy) in ([(3, 0), (0, 3), (-3, 0), (0, -3)] : List (Int × Int)) do
      let _ ← originalSprite "Tiles_129" (x + dx) (y + dy) 24 24 sx (sy + 72) 16 16
        (color := tint) (additive := true)
  discard <| originalSprite "Tiles_129" x y 24 24 sx sy 16 16
private def drawWallTether (camera : Camera) (light : LightField) (fromX fromY toX toY : Float32)
    (scale : Float32) (hungry : Bool) (mirror := false) : IO Unit := do
  let dx := toX - fromX
  let dy := toY - fromY
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if length < 40 * scale then return
  let degrees := (Float.atan2 dy.toFloat dx.toFloat - 1.57) * 57.29577951308232
  let link := 28 * scale
  let count := ((length - 40 * scale) / link).toUInt32.toNat + 1
  for index in [:count] do
    let offset := (index + 1).toFloat.toFloat32 * link
    let worldX := fromX + dx * offset / length
    let worldY := fromY + dy * offset / length
    let x := npcScreenPixel worldX camera.x
    let y := npcScreenPixel worldY camera.y
    if x + 32 < 0 || x ≥ width + 32 || y + 32 < 0 || y ≥ height + 32 then continue
    let wide := (24 * scale + 0.5).toUInt32.toNat
    let tall := (42 * scale + 0.5).toUInt32.toNat
    let flip := hungry && ((index % 2 == 0) == mirror)
    discard <| originalSprite "Chain12" (x - (wide / 2 : Nat)) (y - (tall / 2 : Nat))
        wide tall 0 0 16 28 (color := npcLightTint light worldX worldY)
        (mirror := flip) (rotation := degrees)
-- DrawWOFBody uses 16px bands of a three-frame, 190 × 140px repeating strip.
private def drawWallBody (wall : WallOfFlesh) (camera : Camera) (light : LightField)
    (clock : Nat) : IO Unit := do
  if wall.top < 0 then return
  let nativeX := wall.x - if wall.direction > 0 then 80 else 0
  let x := (nativeX * 1.5).toInt32.toInt - camera.x
  if x + 285 < 0 || x ≥ width then return
  let start := wall.top + 140 * max 0 ((camera.y * 2 / 3 - wall.top) / 140)
  let mut row := start
  while row * 3 / 2 < camera.y + height do
    for band in [:9] do
      let sourceY := 16 * band
      let sampleY := row + sourceY
      let y := sampleY * 3 / 2 - camera.y
      if y + 24 ≥ 0 && y < height then
        let column := (nativeX + 95).toInt32.toInt / 16
        let shade := (75 + 12 * light.level ⟨column, sampleY / 16⟩).toUInt32
        let tint := (shade <<< 24) ||| (shade <<< 16) ||| (shade <<< 8) ||| 255
        let sourceHeight := min 16 (140 - sourceY)
        discard <| originalSprite "WallOfFlesh" x y 285 (sourceHeight * 3 / 2)
            0 (140 * (clock / 6 % 3) + sourceY) 190 sourceHeight
            (color := tint) (mirror := wall.direction > 0)
    row := row + 140

private def drawWallLeech (leech : WallLeech) (camera : Camera) (light : LightField) : IO Unit := do
  for index in [:leech.segments.size] do
    let segment := leech.segments[index]!
    let centerX := segment.x + 7
    let centerY := segment.y + 7
    let x := (centerX * 1.5).toInt32.toInt - camera.x
    let y := ((segment.y + 3) * 1.5).toInt32.toInt - camera.y
    if x + 23 < 0 || x ≥ width + 23 || y + 23 < 0 || y ≥ height + 23 then continue
    let shade := (75 + 12 * light.level (npcPixelTile centerX centerY)).toUInt32
    let tint := (shade <<< 24) ||| (shade <<< 16) ||| (shade <<< 8) ||| 255
    let sprite : Nat := if index == 0 then 117 else if index + 1 == leech.segments.size then 119 else 118
    discard <| originalSprite s!"NPC_{sprite}" (x - 13) (y - 22) 27 45 0 0 18 30
        (color := tint) (rotation := segment.rotation.toFloat * 57.29577951308232)
private def drawWorld (state : GameState) (camera : Camera) : IO (Nat × LightField) := do
  let dungeon := if state.dungeonInstalled then some (dungeonLayout state.seed) else none
  let outdoorLevel := daylight state.surface.clock state.legacyEarly
  let hasRuins := state.world.tile ruinAltar == .ruinAltar
  let hasMine := state.world.tile (deepsteelMarker state.seed) == .mineMarker
  let hasCavern := state.world.tile cavernAltar == .cavernAltar
  let hasGallery := state.world.tile magmaMarker == .magmaMarker
  let mineMouth := deepsteelMouth state.seed
  let player := state.player.centerTile
  let evil := evilRegion state
  let biome := hardmodeBiome state
  let evilHere := match biome with
    | some .corruption => some false
    | some .crimson => some true
    | some .hallow => none
    | none => evil.filter (fun _ => inEvilBiome state.seed player)
  let surface := if evil.isSome then evilSurfaceHeight state.seed player.x.toNat else groundHeight state.seed player.x.toNat
  let hallowSurface := if biome == some .hallow && player.y ≤ (surface : Int) + 5 then some surface else none
  drawSky camera state.surface.clock outdoorLevel state.legacyEarly state.weather evilHere hallowSurface
  if state.world.height == underworldHeight then
    drawUnderworldBackdrop camera state.surface.clock
  let firstX := (camera.x / tilePixels).toNat
  let firstY := (camera.y / tilePixels).toNat
  let lastX := min state.world.width ((camera.x + width + tilePixels - 1) / tilePixels).toNat
  let lastY := min state.world.height ((camera.y + height + tilePixels - 1) / tilePixels).toNat
  let cedars := if lastX > highlandStart then
      (highlandCedarColumns state.seed).map (fun column => (column, highlandCedarCrownFloor state.seed column))
    else #[]
  let legacyCedars := cedars.filter fun (column, floor) =>
    state.world.tile ⟨column, groundHeight state.seed column - 1⟩ == .timber &&
    state.world.tile ⟨column, floor - 6⟩ == .timber
  let holdingTorch := state.selectedStack.any (·.item == .torch)
  let magmaAt := state.magma.level state.world
  let sphereTiles := state.waterSpheres.foldl (fun (tiles : Std.HashSet TilePos) sphere =>
    let (x, y) := waterSphereCenter sphere
    tiles.insert (npcPixelTile x y)) {}
  let dustTiles := state.aquaStreams.foldl (fun tiles stream =>
    if stream.age > 4 then tiles.insert (npcPixelTile stream.x stream.y) else tiles) sphereTiles
  let dustTiles := (state.blueMoon.map fun attack => dustTiles.insert attack.flail.tile).getD dustTiles
  let dustTiles := (state.sunfury.map fun attack => dustTiles.insert attack.flail.tile).getD dustTiles
  let missileTiles := state.magicMissiles.foldl (fun (tiles : Std.HashSet TilePos) attack =>
    tiles.insert attack.missile.tile) {}
  let fireTiles := state.flowerFireballs.foldl (fun (tiles : Std.HashSet TilePos) shot =>
    tiles.insert (npcPixelTile shot.x shot.y)) {}
  let fireTiles := state.flamelashes.foldl (fun tiles attack => tiles.insert attack.projectile.tile) fireTiles
  let fireTiles := state.burningSpheres.foldl (fun tiles sphere =>
    tiles.insert (npcPixelTile sphere.centerX sphere.centerY)) fireTiles
  let fireTiles := state.lavaSlimes.foldl (fun tiles slime =>
    tiles.insert (npcPixelTile slime.centerX slime.centerY)) fireTiles
  let critterTiles := state.underworldCritters.foldl (fun (tiles : Std.HashSet TilePos) critter =>
    if critter.kind == .lavafly && critter.local2 == 0 then tiles
    else tiles.insert (npcPixelTile critter.centerX critter.centerY)) {}
  let illuminantTiles := state.hardmodeCavern.foldl (fun (tiles : Std.HashSet TilePos) enemy =>
    if enemy.kind == .illuminantBat || enemy.kind == .illuminantSlime then
      tiles.insert (npcPixelTile enemy.centerX enemy.centerY)
    else tiles) {}
  let cavernFlameTiles := state.cavernFlames.foldl (fun (tiles : Std.HashSet TilePos) flame =>
    tiles.insert (npcPixelTile flame.x flame.y)) {}
  let sceneEmission (pos : TilePos) : Nat :=
    let amount := magmaAt pos
    if amount > 0 then 9 + (4 * amount + Water.capacity - 1) / Water.capacity
    else if fireTiles.contains pos then 11
    else if missileTiles.contains pos then 10
    else if cavernFlameTiles.contains pos then 11
    else if state.world.tile pos == .gelatinCrystal then 10
    else if illuminantTiles.contains pos || state.world.tile pos == .crystalShard then 7
    else if dustTiles.contains pos || critterTiles.contains pos then 7 else 0
  let light := computeLighting state.world state.seed ⟨firstX, firstY⟩ (lastX - firstX)
    (lastY - firstY) state.player.centerTile holdingTorch outdoorLevel sceneEmission
  -- The original client lowers global brightness from 1.2 to 1 with Darkness.
  let light := if state.darknessTicks == 0 then light else
    { light with levels := ByteArray.mk (light.levels.data.map fun value => (value.toNat * 5 / 6).toUInt8) }
  let behindUnderworld := state.boneSerpent.isSome || state.wallOfFlesh.isSome || !state.wallLeeches.isEmpty
  if behindUnderworld then
    for col in [firstX:lastX] do
      for row in [max firstY underworldStart:lastY] do
        let position : TilePos := ⟨col, row⟩
        if !(state.world.tile position).solid && underworldUnsafeWall state.seed position (world := some state.world) then
          drawObsidianWall ((col : Int) * tilePixels - camera.x) ((row : Int) * tilePixels - camera.y) col row
  if let some wall := state.wallOfFlesh then
    if state.tongued && state.respawnTicks == 0 then
      let x := (Int32.ofInt (bodyCenterX state.player)).toFloat32 / 16
      let y := (Int32.ofInt (bodyCenterY state.player)).toFloat32 / 16
      drawWallTether camera light x y wall.centerX wall.centerY 1 false
    for hungry in state.wallHungry do
      if hungry.attached then
        let (_, anchorY) := wall.attachment hungry.fraction
        drawWallTether camera light (hungry.x + 15) (hungry.y + 15) wall.centerX anchorY 1 true
          (state.surface.clock % 15 > 7)
    drawWallBody wall camera light state.surface.clock
  if let some serpent := state.boneSerpent then drawBoneSerpent serpent camera light
  for leech in state.wallLeeches do drawWallLeech leech camera light
  let mut workbench := none
  let mut rails := #[]
  let mut altars : Array TilePos := #[]
  let mut treeCrowns : Array (Int × Int × Nat) := #[]
  let mut hasEvilObjects := false
  let mut wetCells := 0
  for col in [firstX:lastX] do
    let originalSurface := if dungeon.isSome && 77 ≤ col && col ≤ 102 then
        dungeonSurfaceHeight state.seed col
      else if evil.isSome && inEvilBiome state.seed ⟨col, evilSurfaceHeight state.seed col⟩ then
        evilSurfaceHeight state.seed col
      else if inAmberBluffs ⟨col, 35⟩ then amberBluffSurface state.seed col
      else groundHeight state.seed col
    let frozen := inFrostCavern col
    let highland := inHighlands ⟨col, 0⟩
    let cedar := legacyCedars.find? (·.1 == col)
    let canopy := legacyCedars.filter (fun (center, _) => max col center - min col center ≤ 3)
    let crownBottom := (cedar.map (·.2)).getD originalSurface - 6
    for row in [firstY:lastY] do
      let position : TilePos := ⟨col, row⟩
      let tile := state.world.tile position
      let underground := row ≥ originalSurface
      let underworld := row ≥ underworldStart
      let x := (col : Int) * tilePixels - camera.x
      let y := (row : Int) * tilePixels - camera.y
      let variation := (col * 23 + row * 13) % 16
      let hall := inWardenHall col row
      let quarry := underground && inQuarry col
      let storm := inStormArena position
      let ruin := hasRuins && (inRuinArena position || inRuinDescent position)
      let amber := inAmberBluffs position
      let geode := inGeodeGrotto position
      let mine := hasMine && mineMouth.x ≤ position.x && inDeepsteelMine state.seed position
      let cavern := hasCavern && inHighlandCavern state.seed position
      let gallery := hasGallery && inMagmaGallery position
      let cavernFloor := hasCavern && row == cavernArenaFloor && tile.solid &&
        inCavernArena { position with y := position.y - 1 }
      let lightLevel := light.level position
      let templeWall := !tile.solid && TempleLayout.hasNaturalWall state.world state.walls position
      let obsidianWall := underworld && !tile.solid && underworldUnsafeWall state.seed position (world := some state.world)
      let dungeonWall := !tile.solid && dungeon.isSome && dungeonUnsafeWall state.seed position
        state.lowerDungeonInstalled state.blueMoonDungeonInstalled state.magicMissileDungeonInstalled
        state.deepDungeonInstalled
      -- The raised entrance has background buttresses and a clear opening
      -- across the terrace. These walls do not count as unsafe Dungeon walls.
      let dungeonEntranceWall := !tile.solid && row < originalSurface && dungeon.any (fun layout =>
        layout.surface - 5 ≤ row &&
        (col == 81 || col == 95 ||
          ((col == 82 || col == 94) && row + 4 ≤ layout.surface) ||
          ((col == 83 || col == 93) && row + 5 ≤ layout.surface)))
      if templeWall then drawCaveWall "Wall_87" x y col row
      else if obsidianWall then
        if !behindUnderworld then drawObsidianWall x y col row
      else if underworld && !tile.solid then pure ()
      else if dungeonWall || dungeonEntranceWall then
        drawDungeonWall ((dungeon.map (·.brick)).getD .blueDungeonBrick) x y col row
      else if underground && !tile.solid && biome.isNone && evil.isSome && inEvilBiome state.seed position then
        drawEvilBackdrop x y col row (evil.getD false)
      else if hall && !tile.solid then
        drawWardenBackdrop x y col row
      else if ruin && !tile.solid then
        drawRuinBackdrop x y col row
      else if storm && !tile.solid then
        drawStormBackdrop x y col row
      else if gallery && !tile.solid then
        drawMagmaBackdrop x y col row
      else if (cavern && !tile.solid) || cavernFloor then
        drawCavernBackdrop x y col row cavernFloor
      else if mine && !tile.solid then
        drawMineBackdrop x y col row
      else if underground && !tile.solid && biome == some .hallow then
        discard <| originalSprite s!"Background_{197 + state.seed.toNat % 3}"
          x y tilePixels tilePixels (16 * (col % 10)) (16 * (row % 6)) 16 16
      else if underground && !tile.solid && (biome == some .corruption || biome == some .crimson) then
        drawEvilBackdrop x y col row (biome == some .crimson)
      else if amber && (tile == .stone || tile == .dirt) then
        drawAmberStone x y col row (row == originalSurface)
      else if highland && row < originalSurface + 8 && (tile == .stone || tile == .dirt) then
        drawHighlandSoil x y tile (row == originalSurface) variation
      else if cedar.isSome && row + 1 == originalSurface && tile == .timber then
        drawTreeTrunk x y variation outdoorLevel true
      else if tile == .timber && canopy.any (fun (center, floor) => inCedarCrown col row center floor) then
        if cedar.isSome && row == crownBottom then
          treeCrowns := treeCrowns.push (x, y, variation)
      else if cedar.isSome && crownBottom < row && row + 1 < originalSurface && tile == .air then
        drawTreeTrunk x y variation outdoorLevel false
      else if geode && !tile.solid then
        drawGeodeBackdrop x y col row
      else if quarry && !tile.solid then
        drawQuarryBackdrop x y col row
      else if underground && frozen && !tile.solid then
        drawFrostBackdrop x y variation
      else if tile.solid || tile == .workbench || tile == .chest then
        discard <| drawOriginalBlock state.world position tile x y variation (row == originalSurface)
      else if underground then
        drawCaveWall "Wall_1" x y col row
      let woodWall := !tile.solid && hasWoodWall state.world state.walls position
      if woodWall then drawWoodWall x y col row
      if position == quarrySluice && tile == .stone then
        discard <| originalSprite "Tiles_10" x y 24 24 0 18 16 16
      if tile.isDungeonChest && state.dungeonInstalled then
        if let some anchor := dungeonChestAnchor state.seed position state.lowerDungeonInstalled
            state.blueMoonDungeonInstalled state.magicMissileDungeonInstalled state.deepDungeonInstalled then
          drawFramedChestTile tile x y position anchor (state.openChest == some anchor)
      if tile.isShadowChest then
        if let some anchor := state.shadowChestAtCell position then
          drawFramedChestTile tile x y position anchor (state.openChest == some anchor)
      if tile == .lihzahrdChest then
        if let some anchor := state.templeChestAtCell position then
          drawFramedChestTile tile x y position anchor (state.openChest == some anchor)
      if (TempleDoor.kind tile).isSome then
        if let some (asset, sx, sy) := TempleDoor.frame state.world position then
          discard <| originalSprite s!"Tiles_{asset}" x y 24 24 sx sy 16 16
      if tile.isTempleAltar then
        if let some anchor := TempleLayout.altarAnchor state.world position then
          discard <| originalSprite "Tiles_237" x y 24 24
            (18 * (position.x - anchor.x).toNat) (18 * (position.y - anchor.y).toNat) 16 16
      if tile == .lihzahrdPressurePlate then
        discard <| originalSprite "Tiles_135" x y 24 24 0 108 16 16
      if let some trap := TempleWiring.trapFor tile then
        discard <| originalSprite "Tiles_137" x y 24 24 trap.frameX.toNat trap.kind.frameY.toNat 16 16
      if (underground && !underworld) || tile.solid || dungeonWall || obsidianWall ||
          (underworld && (tile.isShadowChest || woodWall)) then
        shadeCaveTile x y lightLevel (row > originalSurface + 2) (!underworld && (frozen || hall || quarry || geode))
      if tile == .treeTrunk then
        let below := state.world.tile ⟨col, row + 1⟩
        let top := state.world.tile ⟨col, row - 1⟩ != .treeTrunk
        drawTreeTrunk x y variation outdoorLevel below.solid
        if top then treeCrowns := treeCrowns.push (x, y, variation)
      if tile == .platform then
        drawPlatform x y
      if tile == .homeBeacon then
        drawHomeBeacon x y (state.homeAnchor == some position)
      if tile == .supplyCache || tile == .openedCache then
        drawSupplyCache x y (tile == .openedCache)
      if tile == .sunleaf then drawSunleaf x y true 0
      if tile == .sunleafSprout then drawSunleaf x y false (cropTicksAt state position)
      if tile == .lifeCrystal then drawLifeCrystalTile x y state.surface.clock
      if tile == .crystalShard || tile == .gelatinCrystal then
        drawCavernCrystal state.world position tile x y state.surface.clock
      if tile == .highlandBerry then drawHighlandBerry x y
      if tile == .vileMushroom || tile == .viciousMushroom then drawEvilMushroom x y (tile == .viciousMushroom)
      if tile == .placedBottle then drawBottle x y
      if tile == .ruinAltar then drawRuinAltar x y
      if tile == .mineMarker then drawMineMarker x y
      if tile == .cavernAltar then drawCavernAltar x y (state.cavern == .ready)
      if tile == .magmaMarker then drawMagmaMarker x y
      if tile == .cobweb then drawCobweb x y
      if hasGallery && position == magmaWaterGate && tile == .stone then drawMagmaGate x y
      if tile == .doorOpen || tile == .doorOpenTop || tile == .doorOpenMiddle then
        drawDoor x y true (tile == .doorOpen) (tile == .doorOpenMiddle)
      if tile == .torch then
        drawTorch (x + 12) (y + 2)
      if !tile.solid then
        let above := { position with y := position.y - 1 }
        let amount := state.water.level state.world position
        if amount > 0 then
          wetCells := wetCells + 1
          let surface := amount < Water.capacity || state.water.level state.world above == 0
          drawWater x y amount lightLevel surface
        let honey := state.honey.level state.world position
        if honey > 0 then
          wetCells := wetCells + 1
          let surface := honey < Water.capacity || state.honey.level state.world above == 0
          drawHoney x y honey lightLevel surface
        let magma := magmaAt position
        if magma > 0 then
          let surface := magma < Water.capacity || magmaAt above == 0
          drawMagma x y magma (state.surface.clock / 6 + variation * 3) surface
      if tile == .workbench then workbench := some (x, y)
      if tile == .rail then rails := rails.push position
      if tile == .demonAltar || tile == .crimsonAltar then
        let anchor := altarAnchorAt state.world position tile
        if !altars.contains anchor then altars := altars.push anchor
      if tile == .shadowOrb || tile == .crimsonHeart || tile == .clayPot then hasEvilObjects := true
  for (x, y, variation) in treeCrowns do drawTreeCrown x y variation outdoorLevel
  if hasEvilObjects then
    let layout := evilLayout state.seed
    let drawObject (anchor : TilePos) (tile : Tile) : IO Unit := do
      let x := anchor.x * tilePixels - camera.x
      let y := anchor.y * tilePixels - camera.y
      if x + 48 ≥ 0 && x < width && y + 48 ≥ 0 && y < height &&
          (evilRelicCells anchor).all (fun pos => state.world.tile pos == tile) then
        if tile == .clayPot then drawClayPot x y
        else drawEvilRelic x y (tile == .crimsonHeart) state.surface.clock
    for anchor in layout.relics do drawObject anchor (evilRelicTile state.seed)
    for anchor in layout.pots do drawObject anchor .clayPot
  for position in rails do drawRailTrack state position camera
  for anchor in altars do
    let tile := state.world.tile anchor
    if tile == .demonAltar || tile == .crimsonAltar then
      let x := anchor.x * tilePixels - camera.x
      let bottom := (anchor.y + 1) * tilePixels - camera.y
      if x + 72 ≥ 0 && x < width && bottom ≥ 0 && bottom - 48 < height then
        drawEvilAltar x bottom (tile == .crimsonAltar)
  for furniture in state.furniture do
    drawFurniture furniture camera (state.bedSpawnPoint == some furniture.position)
  for bulb in state.planteraBulbs do
    let x := bulb.x * tilePixels - camera.x
    let y := bulb.y * tilePixels - camera.y
    if x + 48 < 0 || x ≥ width || y + 48 < 0 || y ≥ height then continue
    let frame := state.surface.clock / 21 % 4
    for row in [:2] do
      for col in [:2] do
        discard <| originalSprite "Tiles_238" (x + 24 * col) (y + 24 * row) 24 24
          (18 * col) (36 * frame + 18 * row) 16 16
  if state.eye.isNone && state.eater.isNone && state.brain.isNone && state.skeletron.isNone &&
      state.wallOfFlesh.isNone then
    if let some (x, y) := workbench then drawWorkbenchHint x y state.nearWorkbench
  if hasRuins then drawRuinEntrance state camera
  let seam := highlandStart - 1
  let surfaceGate : TilePos := ⟨seam, groundHeight state.seed seam - 1⟩
  if state.canReach surfaceGate && !((state.world.tile surfaceGate).solid) then
    drawHighlandSign camera surfaceGate 26
  if state.canReach highlandVaultPassage && !((state.world.tile highlandVaultPassage).solid) then
    drawHighlandSign camera highlandVaultPassage 100
  if hasMine && state.canReach mineMouth then drawMineEntrance camera mineMouth
  let descent := cavernEntrance state.seed
  if hasCavern && state.canReach descent then drawCavernEntrance camera descent
  if hasGallery then
    if let some cap := magmaEntranceCaps[0]? then
      if state.canReach cap then drawMagmaEntrance camera cap
  return (wetCells, light)

private def originalItem (item : Item) : Option String :=
  if let some id := item.queenSlimeTextureId then some s!"Item_{id}"
  else if let some id := item.skeletronPrimeTextureId then some s!"Item_{id}"
  else if let some id := item.planteraTextureId then some s!"Item_{id}"
  else if let some id := item.templeTextureId then some s!"Item_{id}"
  else if let some id := item.waterCatchTextureId then some s!"Item_{id}"
  else if let some id := item.anglerTextureId then some s!"Item_{id}"
  else if let some id := item.biomeFishingTextureId then some s!"Item_{id}"
  else if let some id := item.underworldTextureId then some s!"Item_{id}"
  else if let some id := item.wallTextureId then some s!"Item_{id}"
  else if let some id := item.hardmodeTextureId then some s!"Item_{id}"
  else match item with
  | .copperPick => some "Item_3509"
  | .dirt => some "Item_2"
  | .stone => some "Item_3"
  | .copper => some "Item_12"
  | .wood => some "Item_9"
  | .torch => some "Item_8"
  | .ice => some "Item_664"
  | .iron => some "Item_11"
  | .ironSword => some "Item_4"
  | .frostbrand => some "Item_676"
  | .workbench => some "Item_36"
  | .chest => some "Item_48"
  | .door => some "Item_25"
  | .silver => some "Item_14"
  | .silverSword => some "Item_3514"
  | .bow => some "Item_39"
  | .arrow => some "Item_40"
  | .amber => some "Item_999"
  | .geode => some "Item_4400"
  | .platform => some "Item_94"
  | .caveGel => some "Item_23"
  | .fishingRod => some "Item_2289"
  | .bait => some "Item_2674"
  | .rail => some "Item_2340"
  | .minecart => some "Item_2343"
  | .grapplingHook => some "Item_84"
  | .lifeCrystal => some "Item_29"
  | .obsidian => some "Item_173"
  | .honeyBlock => some "Item_1125"
  | .crispyHoneyBlock => some "Item_1127"
  | .ironBar => some "Item_22"
  | .chain => some "Item_85"
  | .cobweb => some "Item_150"
  | .silk => some "Item_225"
  | .bed => some "Item_224"
  | .furnace => some "Item_33"
  | .ironAnvil => some "Item_35"
  | .sawmill => some "Item_363"
  | .loom => some "Item_332"
  | .woodWall => some "Item_93"
  | .woodHammer => some "Item_196"
  | .copperShortsword => some "Item_3507"
  | .copperAxe => some "Item_3506"
  | .woodSword => some "Item_24"
  | .tinOre => some "Item_699"
  | .leadOre => some "Item_700"
  | .tungstenOre => some "Item_701"
  | .goldOre => some "Item_13"
  | .platinumOre => some "Item_702"
  | .copperBar => some "Item_20"
  | .tinBar => some "Item_703"
  | .leadBar => some "Item_704"
  | .silverBar => some "Item_21"
  | .tungstenBar => some "Item_705"
  | .goldBar => some "Item_19"
  | .platinumBar => some "Item_706"
  | .leadAnvil => some "Item_716"
  | .tinPick => some "Item_3503"
  | .ironPick => some "Item_1"
  | .leadPick => some "Item_3497"
  | .silverPick => some "Item_3515"
  | .tungstenPick => some "Item_3491"
  | .goldPick => some "Item_3521"
  | .platinumPick => some "Item_3485"
  | .copperSword => some "Item_3508"
  | .tinSword => some "Item_3502"
  | .leadSword => some "Item_3496"
  | .tungstenSword => some "Item_3490"
  | .goldSword => some "Item_3520"
  | .platinumSword => some "Item_3484"
  | .tinAxe => some "Item_3500"
  | .ironAxe => some "Item_10"
  | .leadAxe => some "Item_3494"
  | .silverAxe => some "Item_3512"
  | .tungstenAxe => some "Item_3488"
  | .goldAxe => some "Item_3518"
  | .platinumAxe => some "Item_3482"
  | .woodChair => some "Item_34"
  | .woodTable => some "Item_32"
  | .lens => some "Item_38"
  | .blackLens => some "Item_236"
  | .shackle => some "Item_216"
  | .zombieArm => some "Item_1304"
  | .suspiciousLookingEye => some "Item_43"
  | .demoniteOre => some "Item_56"
  | .crimtaneOre => some "Item_880"
  | .corruptSeeds => some "Item_59"
  | .crimsonSeeds => some "Item_2171"
  | .lesserHealingPotion => some "Item_28"
  | .binoculars => some "Item_1299"
  | .eyeMask => some "Item_2112"
  | .eyeTrophy => some "Item_1360"
  | .unholyArrow => some "Item_47"
  | .demoniteBar => some "Item_57"
  | .crimtaneBar => some "Item_1257"
  | .copperBow => some "Item_3504"
  | .tinBow => some "Item_3498"
  | .ironBow => some "Item_99"
  | .leadBow => some "Item_3492"
  | .silverBow => some "Item_3510"
  | .tungstenBow => some "Item_3486"
  | .goldBow => some "Item_3516"
  | .platinumBow => some "Item_3480"
  | .demonBow => some "Item_44"
  | .tendonBow => some "Item_796"
  | .heart => some "Item_58"
  | .ebonstone => some "Item_61"
  | .crimstone => some "Item_836"
  | .musket => some "Item_96"
  | .undertaker => some "Item_800"
  | .handgun => some "Item_164"
  | .musketBall => some "Item_97"
  | .bomb => some "Item_166"
  | .stickyBomb => some "Item_235"
  | .shadowOrbPet => some "Item_115"
  | .crimsonHeartPet => some "Item_3062"
  | .vilethorn => some "Item_64"
  | .ballOHurt => some "Item_162"
  | .bandOfStarpower => some "Item_111"
  | .panicNecklace => some "Item_1290"
  | .crimsonRod => some "Item_1256"
  | .rottedFork => some "Item_802"
  | .rottenChunk => some "Item_68"
  | .vertebra => some "Item_1330"
  | .vileMushroom => some "Item_60"
  | .viciousMushroom => some "Item_2887"
  | .vilePowder => some "Item_67"
  | .viciousPowder => some "Item_2886"
  | .wormFood => some "Item_70"
  | .bloodySpine => some "Item_1331"
  | .shadowScale => some "Item_86"
  | .tissueSample => some "Item_1329"
  | .nightmarePickaxe => some "Item_103"
  | .deathbringerPickaxe => some "Item_798"
  | .eaterMask => some "Item_2111"
  | .eaterTrophy => some "Item_1361"
  | .brainMask => some "Item_2104"
  | .brainTrophy => some "Item_1362"
  | .eatersBone => some "Item_994"
  | .boneRattle => some "Item_3060"
  | .sand => some "Item_169"
  | .glass => some "Item_170"
  | .bottle => some "Item_31"
  | .blueDungeonBrick => some "Item_134"
  | .greenDungeonBrick => some "Item_137"
  | .pinkDungeonBrick => some "Item_139"
  | .goldenKey => some "Item_327"
  | .cobaltShield => some "Item_156"
  | .healingPotion => some "Item_188"
  | .skeletronMask => some "Item_1281"
  | .skeletronTrophy => some "Item_1363"
  | .skeletronHand => some "Item_1273"
  | .bookOfSkulls => some "Item_1313"
  | .chippysCouch => some "Item_4993"
  | .boneKey => some "Item_1169"
  | .bone => some "Item_154"
  | .nazar => some "Item_891"
  | .aquaScepter => some "Item_157"
  | .blueMoon => some "Item_163"
  | .magicMissile => some "Item_113"
  | .valor => some "Item_3317"
  | .shadowKey => some "Item_329"
  | .shadowChest => some "Item_328"
  | .ashBlock => some "Item_172"
  | .obsidianBrick => some "Item_192"
  | .hellstone => some "Item_174"
  | .obsidianSkull => some "Item_193"
  | .sunfury => some "Item_220"
  | .darkLance => some "Item_274"
  | .flowerOfFire => some "Item_112"
  | .flamelash => some "Item_218"
  | .hellwingBow => some "Item_3019"
  | .obsidianRose => some "Item_1323"
  | .magmaStone => some "Item_1322"
  | .demonScythe => some "Item_272"
  | .guideVoodooDoll => some "Item_267"
  | .plumbersHat => some "Item_244"
  | .batBat => some "Item_5097"
  | .copperCoin => some "Item_71"
  | .silverCoin => some "Item_72"
  | .goldCoin => some "Item_73"
  | .platinumCoin => some "Item_74"
  | .basicPick => some "Item_3509"
  | .crystalPick => some "Item_777"
  | .crystal => some "Item_502"
  | .stoneSword => some "Item_724"
  | .stormBow => some "Item_725"
  | .draught => some "Item_28"
  | .geodeMail => some "Item_82"
  | .amberTreads => some "Item_54"
  | .watchhorn => some "Item_361"
  | .raidCrest => some "Item_3090"
  | .homeBeacon => some "Item_4876"
  | .recallCharm => some "Item_50"
  | .frostShell => some "Item_857"
  | .quarryScale => some "Item_86"
  | .hunterBow => some "Item_99"
  | .pondPerch => some "Item_2290"
  | .caveEel => some "Item_2309"
  | .sunleafSeed => some "Item_307"
  | .sunleaf => some "Item_313"
  | .gillsElixir => some "Item_291"
  | .barkskinTonic => some "Item_292"
  | .ruinHammer => some "Item_196"
  | .highlandBerry => some "Item_4282"
  | .deepsteel => some "Item_1104"
  | .highlandCrossbow => some "Item_1319"
  | .deepstoneAegis => some "Item_156"
  | _ => none

private def toolMetal : Item → UInt32
  | .copperShortsword | .copperPick | .copperAxe | .copperSword | .copperBar | .copperBow => 0xf3a168ff
  | .tinPick | .tinSword | .tinAxe | .tinBar | .tinBow => metalOreColor .tinOre
  | .ironPick | .ironSword | .ironAxe | .ironBar | .ironBow => 0xc3dbffff
  | .leadPick | .leadSword | .leadAxe | .leadBar | .leadBow => metalOreColor .leadOre
  | .silverPick | .silverSword | .silverAxe | .silverBar | .silverBow => 0xecf8ffff
  | .tungstenPick | .tungstenSword | .tungstenAxe | .tungstenBar | .tungstenBow => metalOreColor .tungstenOre
  | .goldPick | .goldSword | .goldAxe | .goldBar | .goldBow => metalOreColor .goldOre
  | .platinumPick | .platinumSword | .platinumAxe | .platinumBar | .platinumBow => metalOreColor .platinumOre
  | .demoniteBar | .demonBow => demoniteColor
  | .crimtaneBar | .tendonBow => crimtaneColor
  | .woodSword => 0xd4a372ff
  | .zombieArm => 0xa2b58dff
  | .crystalPick => mint
  | .nightmarePickaxe => 0xc39bcfff
  | .deathbringerPickaxe => 0xf09887ff
  | .frostbrand => 0x99fff0ff
  | _ => 0xd7e3ebff

private def drawHarvestTool (item : Item) (x y : Int) (left := false) : IO Unit := do
  if let some asset := originalItem item then
    discard <| originalSprite asset x y 24 24 (fit := true) (mirror := left)

private def isBow (item : Item) : Bool := item.bowShot.isSome

private def isGun (item : Item) : Bool := item.gunShot.isSome
private def isRanged (item : Item) : Bool := isBow item || isGun item

private def drawGun (item : Item) (x y : Int) (left := false) : IO Unit := do
  if let some asset := originalItem item then
    discard <| originalSprite asset x y 25 24 (fit := true) (mirror := left)

private def drawBombSprite (x y : Int) (sticky : Bool) (spark : UInt32) : IO Unit := do
  let asset := if sticky then "Item_235" else "Item_166"
  discard <| originalSprite asset x y 24 24 (fit := true) (color := spark)

private def ammoForWeapon (state : GameState) (weapon : Item) : Option Item := do
  let kind ← weapon.gunShot.orElse fun _ => weapon.bowShot
  let kind := if weapon == .bow && !state.legacyEarly then .woodenArrow else kind
  let index ← state.ammoSlotFor kind
  (state.inventory[index]?.join).map (·.item)

private def visibleAmmo (state : GameState) : Option Item :=
  state.selectedStack.bind (fun stack => ammoForWeapon state stack.item)

private def drawHighlandCrossbow (x y : Int) (left := false) : IO Unit := do
  discard <| originalSprite "Item_1319" x y 29 24 (fit := true) (mirror := left)

private def drawBow (x y : Int) (item : Item) : IO Unit := do
  if let some asset := originalItem item then
    discard <| originalSprite asset x y 24 30 (fit := true)

private def drawRuinHammer (x y : Int) (left := false) : IO Unit := do
  discard <| originalSprite "Item_196" x y 24 24 (fit := true) (mirror := left)

private def drawGrapplingHook (x y : Int) : IO Unit := do
  discard <| originalSprite "Item_84" x y 24 24 (fit := true)

private def fishingTarget : FishingState → Option TilePos
  | .idle => none
  | .waiting target _ | .biting target _ | .soaking target _ | .hooked target .. => some target
  | .bobber motion _ => some motion.tile

private def drawFishingLinePart (x y endX endY : Int) : IO Unit := do
  let dx := (endX - x).toFloat
  let dy := (endY - y).toFloat
  let length := (dx * dx + dy * dy).sqrt.toUInt64.toNat
  if length > 0 then
    discard <| originalSprite "FishingLine" ((x + endX) / 2 - 7) ((y + endY) / 2 - length / 2)
      14 length (color := 0xc8c8c864) (rotation := Float.atan2 dy dx * 57.29577951308232 - 90)

private def drawHook (state : GameState) (camera : Camera) (endX endY : Int) (kind : HookKind) : IO Unit := do
  let x := bodyCenterX state.player * tilePixels / tileUnits - camera.x
  let y := bodyCenterY state.player * tilePixels / tileUnits - camera.y
  let endX := endX * tilePixels / tileUnits - camera.x
  let endY := endY * tilePixels / tileUnits - camera.y
  let dx := endX - x
  let dy := endY - y
  let length := (dx.toFloat * dx.toFloat + dy.toFloat * dy.toFloat).sqrt.toUInt64.toNat
  let heading := Float.atan2 dy.toFloat dx.toFloat * 57.29577951308232
  if kind == .dissonance then
    let rotation := heading + 90
    if length > 12 then
      for i in [1:length / 12] do
        discard <| originalSprite "Extra_208" (endX - dx * (12 * i : Nat) / length - 3)
          (endY - dy * (12 * i : Nat) / length - 6) 6 12 (rotation := rotation)
    discard <| originalSprite "Projectile_935" (endX - 10) (endY - 24) 21 48 (rotation := rotation)
  else
    let rotation := heading - 90
    let links := max 1 (length / 10)
    for i in [:links] do
      discard <| originalSprite "Chain" (x + dx * i / links - 6) (y + dy * i / links - 6) 12 12
        (rotation := rotation)
    discard <| originalSprite "Projectile_13" (endX - 9) (endY - 9) 18 18 (rotation := rotation)

private def drawGrapple (state : GameState) (camera : Camera) : IO Unit := do
  if let some anchor := state.grapple.attachment then
    drawHook state camera anchor.x anchor.y anchor.kind
  if let .flying hook := state.grapple then
    drawHook state camera hook.x hook.y hook.kind

private def drawDissonanceDust (camera : Camera) (body : Player) (age : Nat) : IO Unit := do
  let x := bodyCenterX body * tilePixels / tileUnits - camera.x
  let y := bodyCenterY body * tilePixels / tileUnits - camera.y
  for i in [:60] do
    let seed := (hash (body.x, body.y, i)).toNat
    let dx : Int := (seed % 62 : Nat) - 31
    let dy : Int := (seed / 62 % 85 : Nat) - 42
    let drift := 18 + age
    let size := 3 + seed / 5270 % 8
    let alpha := (22 - age) * 7
    let tint : UInt32 := if seed % 3 == 0 then 0x8175ff00
      else if seed % 3 == 1 then 0xb87cff00 else 0xec8cf900
    discard <| originalSprite "Dust" (x + dx * drift / 18) (y + dy * drift / 18) size size
      670 (60 + seed % 3 * 10) 8 8 (color := tint ||| alpha.toUInt32)

private def drawOriginalBobber (camera : Camera) (rodX rodY : Int) (left : Bool)
    (pole : Item) (motion : FishingBobber) : IO Unit := do
  let worldX := fun screen : Int => (Int32.ofInt (screen + camera.x)).toFloat32 / 1.5
  let worldY := fun screen : Int => (Int32.ofInt (screen + camera.y)).toFloat32 / 1.5
  let mut x := worldX (rodX + if left then 5 else 31)
  let mut y := worldY (rodY + 8)
  for _ in [:256] do
    let dx := motion.centerX - x
    let dy := motion.y + 1.4 - y
    let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
    if length < 0.5 then break
    let speed := min 16 (motion.vx.abs + motion.vy.abs)
    let amount := 0.3 * (1 - speed / 16) * min 1 (length / 80) *
      (1 - motion.reelTicks.toFloat.toFloat32 / 100)
    let slack := if dy > 0 then amount else
      let adjusted := amount * (min 1 (motion.vx.abs / 3) - 0.5)
      if adjusted > 0 then adjusted * 2 else adjusted
    let last := length ≤ 16
    let stepX := if last then dx else dx * (1 - slack) * (12 / length)
    let stepY := if last then dy else dy * (1 + slack) * (12 / length)
    let h := max 1 ((stepX * stepX + stepY * stepY).toFloat.sqrt * 1.5 + 0.5).toUInt64.toNat
    let angle := Float.atan2 stepY.toFloat stepX.toFloat * 57.29577951308232 - 90
    let centerX := npcScreenPixel (x + stepX / 2) camera.x
    let centerY := npcScreenPixel (y + stepY / 2) camera.y
    discard <| originalSprite "FishingLine" (centerX - 9) (centerY - (h / 2 : Nat)) 18 h
        (color := 0xc8c8c864) (rotation := angle)
    if last then break
    x := x + stepX
    y := y + stepY
  let bobberX := npcScreenPixel motion.centerX camera.x
  let bobberY := npcScreenPixel motion.centerY camera.y
  let rotation := motion.rotation.toFloat * 57.29577951308232
  discard <| originalSprite pole.fishingBobberTexture (bobberX - 11) (bobberY - 11) 22 22 (rotation := rotation)
private def drawFishingLine (state : GameState) (camera : Camera) (rodX rodY : Int)
    (left : Bool) (target : TilePos) : IO Unit := do
  let biting := match state.fishing with | .biting .. | .hooked .. => true | _ => false
  let amount := max (state.water.level state.world target)
    (max (state.magma.level state.world target) (state.honey.level state.world target))
  let waterTop := target.y * tilePixels - camera.y + (tilePixels - tilePixels * amount / Water.capacity : Nat)
  let x := target.x * tilePixels + tilePixels / 2 - camera.x
  let bob := if biting then state.surface.clock / 4 % 3 else state.surface.clock / 12 % 2
  let y := waterTop + bob
  let tipX := rodX + if left then 5 else 18
  let middleX := (tipX + x) / 2
  let middleY := (rodY + y) / 2 + if biting then 0 else 6
  drawFishingLinePart tipX rodY middleX middleY
  drawFishingLinePart middleX middleY x y
  discard <| originalSprite "Projectile_360" (x - 11) (y - 9) 22 19 (fit := true)
  if biting && state.legacyEarly then
    let spray := state.surface.clock / 4 % 4
    discard <| originalSprite "Dust" (x - 11 - spray) (y - 4) 9 9 330 0 8 8
    discard <| originalSprite "Dust" (x + 7 + spray) (y - 4) 9 9 330 10 8 8
    rect (x - 21) (y - 27) 42 15 0x15212ced
    text "BITE!" (x - 17) (y - 25) 12 gold

private def drawSwing (x y facing : Int) (age : Nat) (item : Item) : IO Unit := do
  let handX := if facing < 0 then x - 17 else x + 8
  let angle := (age.toFloat - 4) * (if facing < 0 then -17 else 17)
  if let some asset := originalItem item then
    discard <| originalSprite asset handX (y - 9) 36 43 (fit := true) (mirror := facing < 0) (rotation := angle)

private def headUnderwater (state : GameState) : Bool :=
  let x := state.player.x + playerWidth / 2
  let y := state.player.y + playerHeight / 6
  state.water.submerged state.world x y || state.honey.submerged state.world x y

private def drawOriginalPlayer (x y : Int) (player : Player) (clock : Nat) (left hurt : Bool)
    (equipment : Equipment) (boots : UInt32) (mounted := false) : IO Unit := do
  let atX := x - 12
  let atY := y - 12
  let skin := if hurt then 0xffa690ff else 0xf7c392ff
  let cloth := if equipment.armor.isSome then 0xa993d2ff else 0x62b9a9ff
  let hood := equipment.head == some .crystalAssassinHood
  let shirt := equipment.armor == some .crystalAssassinShirt
  let pants := equipment.legs == some .crystalAssassinPants
  let walking := if mounted || !player.grounded then 5 else if player.vx != 0 then 6 + clock / 4 % 14 else 0
  let drawLayer (asset : String) (sx sy : Nat) (color : UInt32 := 0xffffffff) := do
    discard <| originalSprite asset atX atY 40 47 sx sy 40 56 (color := color) (mirror := left)
  drawLayer "Player_0_0" 0 0 skin
  drawLayer "Player_0_1" 0 0 0xfff8e4ff
  drawLayer "Player_0_2" 0 0 0x5d5e75ff
  if !hood then drawLayer "Player_Hair_1" 0 0 0xd59450ff
  if shirt then
    let sheet := "Armor/Armor_230"
    let arm := if walking < 6 then 2 else
      (#[3, 4, 4, 4, 4, 3, 3, 3, 5, 6, 6, 5, 3, 3] : Array Nat)[walking - 6]!
    let row := if walking == 0 then 0 else 56
    drawLayer sheet (arm * 40) (row + 112)
    drawLayer sheet 40 56
    drawLayer sheet (if walking == 5 then 40 else 0) 0
    drawLayer sheet (arm * 40) row
    drawLayer sheet 0 56
  else
    drawLayer "Player_0_3" 0 0 skin
    drawLayer "Player_0_6" 0 0 cloth
  if pants then drawLayer "Armor_Legs_213" 0 (walking * 56)
  else
    drawLayer "Player_0_11" 0 (walking * 56) 0x886859ff
    drawLayer "Player_0_12" 0 (walking * 56) boots
  if hood then drawLayer "Armor_Head_261" 0 0

private def drawPlayer (state : GameState) (camera : Camera) (facesLeft : Bool) : IO Unit := do
  let player := state.player
  let x := player.x * tilePixels / tileUnits - camera.x
  let y := player.y * tilePixels / tileUnits - camera.y
  let cast := fishingTarget state.fishing
  let facesLeft := match state.fishing with
    | .bobber .. => facesLeft
    | _ => (cast.map (fun target => decide (target.x < player.centerTile.x))).getD facesLeft
  let y ← if let some mount := state.wingedSlime then do
    let bob : Int := if mount.frame == 1 || mount.frame == 4 || mount.frame == 5 then 3 else 0
    let left := x + playerWidth * tilePixels / tileUnits / 2 - 52 + if facesLeft then -1 else 1
    let top := (player.y + playerHeight) * tilePixels / tileUnits - camera.y - 105 + bob
    let wings := top + 12 - bob
    discard <| originalSprite "Extra_207" left wings 105 105 0 (72 * mount.wingFrame) 70 70 (mirror := facesLeft)
    discard <| originalSprite "Extra_204" left top 105 105 0 (72 * mount.frame) 70 70
      (mirror := facesLeft) (queenSlimeMount := true)
    discard <| originalSprite "Extra_205" left top 105 105 0 (72 * mount.frame) 70 70 (mirror := facesLeft)
    discard <| originalSprite "Extra_206" left wings 105 105 0 (72 * mount.wingFrame) 70 70 (mirror := facesLeft)
    pure (y - 30 + bob)
  else pure y
  let hurt := state.combat.invulnerable > 0 && state.combat.invulnerable / 4 % 2 == 1
  let boots : UInt32 := match state.equipment.accessory with
    | some .amberTreads => 0xd49255ff
    | some .raidCrest => 0xc47271ff
    | _ => 0x352b2cff
  drawOriginalPlayer x y player state.surface.clock facesLeft hurt state.equipment boots state.wingedSlime.isSome
  if state.assassinDash.velocity != 0 then
    let direction : Int := if state.assassinDash.velocity < 0 then -1 else 1
    for i in [:3] do
      discard <| originalSprite "Dust" (x - direction * (9 + 10 * i : Nat)) (y + 20 + i % 2 * 5)
        9 9 ((68 + i) * 10) ((state.surface.clock + i) % 3 * 10) 8 8 (color := 0xffffffb8)
  if state.combat.onFire > 0 || state.burningTicks > 0 then
    let frame := (state.surface.clock / 4) % 3
    let emberY := y + 24 - frame * 3
    for (dx, dy) in [(-4, 0), (10, 8), (12, -9)] do
      discard <| originalSprite "Dust" (x + dx) (emberY + dy) 12 12 60 (frame * 10) 8 8
  let held := state.selectedStack.map (·.item)
  let useTime := state.equipment.meleeTicks
    (if state.legacyEarly then swingCooldown else (held.map Item.swordUseTime).getD swingCooldown)
  let age := useTime - state.combat.swing
  if state.netSwingTicks > 0 then
    if let some item := held.filter (fun item => item.netUseTime > 0) then
      let (w, h) : Nat × Nat := match item with
        | .lavaproofBugNet => (48, 50)
        | .goldenBugNet => (44, 48)
        | _ => (36, 40)
      let duration := item.netUseTime
      let phase := ((duration - state.netSwingTicks).toFloat / duration.toFloat)
      let left := state.combat.facing < 0
      let angle := -115 + 155 * phase
      let degrees := if left then -angle else angle
      let heading := degrees / 57.29577951308232
      let reach := 19 * heading.cos * (if left then -1 else 1)
      let lift := 19 * heading.sin * (if left then -1 else 1)
      let netX := x + 8 + reach.toInt64.toInt - (w / 2 : Nat)
      let netY := y + 14 + lift.toInt64.toInt - (h / 2 : Nat)
      if let some sprite := originalItem item then
        let _ ← originalSprite sprite netX netY w h (mirror := left) (rotation := degrees)
  else if state.shortsword.isSome || held == some .copperShortsword ||
      state.darkLance.isSome || state.sunfury.isSome then pure ()
  else if state.combat.swing > 0 && age < 9 then
    if held == some .batBat then
      let left : Bool := state.combat.facing < 0
      let angle := (age.toFloat - 4) * (if left then -15 else 15)
      discard <| originalSprite "Item_5097" (if left then x - 32 else x + 1) (y - 17) 47 47
          (mirror := left) (rotation := angle)
    else if held == some .ruinHammer then
      let left : Bool := state.combat.facing < 0
      drawRuinHammer (x + if left then -16 else 11) (y + if age < 3 then 1 else 11) left
    else
      drawSwing x y state.combat.facing age (held.getD .woodSword)
  else if state.selectedStack.any (·.item == .torch) then
    drawTorch (if facesLeft then x - 4 else x + 21) (y + 8)
  else if let some stack := state.selectedStack then
    if !state.legacyEarly && (stack.item.pickPower > 0 || stack.item.axePower > 0) then
      drawHarvestTool stack.item (if facesLeft then x - 14 else x + 8) (y + 11) facesLeft
    else if stack.item == .pwnhammer then
      let hammerX := if facesLeft then x - 24 else x + 7
      let angle := if state.toolCooldown == 0 then 0 else
        ((14 - min 14 state.toolCooldown).toFloat - 5) * (if facesLeft then -8 else 8)
      discard <| originalSprite "Item_367" hammerX (y - 2) 35 39
          (fit := true) (mirror := facesLeft) (rotation := angle)
    else if stack.item == .highlandCrossbow then
      drawHighlandCrossbow (if facesLeft then x - 17 else x + 10) (y + 9) facesLeft
    else if isGun stack.item then
      drawGun stack.item (if facesLeft then x - 19 else x + 11) (y + 10) facesLeft
    else if stack.item == .aquaScepter then
      let staffX := if facesLeft then x - 32 else x + 10
      discard <| originalSprite "Item_157" staffX (y + 12) 40 15 (mirror := facesLeft)
    else if (stack.item == .flowerOfFire || stack.item == .demonScythe) && state.bowCooldown > 0 then
      let itemX := if facesLeft then x - 23 else x + 10
      let flower := stack.item == .flowerOfFire
      discard <| originalSprite (if flower then "Item_112" else "Item_272") itemX (y + 6) 29 (if flower then 27 else 31)
          (fit := true) (mirror := facesLeft)
    else if (stack.item == .magicMissile &&
        (state.bowCooldown > 0 || state.magicMissiles.any (·.missile.guided))) ||
        (stack.item == .flamelash &&
        (state.bowCooldown > 0 || state.flamelashes.any (·.projectile.missile.guided))) then
      let staffX := if facesLeft then x - 23 else x + 10
      let flame := stack.item == .flamelash
      discard <| originalSprite (if flame then "Item_218" else "Item_113") staffX (y - 7) 32 32 (mirror := facesLeft)
    else if stack.item == .dynamite then
      let itemX := if facesLeft then x - 13 else x + 10
      discard <| originalSprite "Item_167" itemX (y + 9) 23 25 (fit := true) (mirror := facesLeft)
    else if stack.item.isBomb then
      drawBombSprite (if facesLeft then x - 13 else x + 10) (y + 11) (stack.item == .stickyBomb) gold
    else if stack.item == .hellwingBow then
      let bowX := if facesLeft then x - 13 else x + 14
      discard <| originalSprite "Item_3019" bowX (y + 1) 16 41 (mirror := facesLeft)
    else if isBow stack.item then drawBow (if facesLeft then x - 16 else x + 12) (y + 10) stack.item
    else if stack.item == .grapplingHook && state.grapple == .idle then
      drawGrapplingHook (if facesLeft then x - 16 else x + 10) (y + 9)
    else if stack.item.isFishingPole then
      let rodX := if facesLeft then x - 31 else x + 12
      let rodY := y - 10
      if let .bobber motion _ := state.fishing then
        drawOriginalBobber camera rodX rodY facesLeft stack.item motion
      else if let some target := cast then
        drawFishingLine state camera rodX (y + 1) facesLeft target
      discard <| originalSprite ((originalItem stack.item).getD "Item_2289") rodX rodY 36 36
          (mirror := facesLeft)
  if headUnderwater state then
    let lift := state.surface.clock / 5 % 10
    let bubbleX := x + if facesLeft then -6 else 19
    let bubbleY := y + 5 - lift
    discard <| originalSprite "Bubble" bubbleX bubbleY 6 6 (fit := true)

private def drawCrawler (crawler : Crawler) (camera : Camera) : IO Unit := do
  let x := crawler.body.x * tilePixels / tileUnits - camera.x
  let y := crawler.body.y * tilePixels / tileUnits - camera.y
  discard <| originalSprite "NPC_1" x y 26 22 0 0 32 26

private def drawCedarHare (x y : Int) (left _grounded : Bool) : IO Unit := do

  discard <| originalSprite "NPC_46" (x - 7) (y + 5) 34 30 0 0 48 40 (mirror := !left)

private def drawBrambleCrawler (x y : Int) (creature : NightCreature) (_clock : Nat) : IO Unit := do

  discard <| originalSprite "NPC_164" (x - 6) (y - 2) 42 24 0 0 70 30 (mirror := creature.body.vx > 0)

private def drawSurfaceHealth (creature : NightCreature) (x y : Int) (color : UInt32) : IO Unit := do
  rect (x - 2) y 21 4 0x262e3eff
  rect (x - 1) (y + 1) (19 * creature.health / creature.kind.maxHealth) 2 color

private def eyeScreen (motion : EyeMotion) (camera : Camera) : Int × Int :=
  (motion.x * tilePixels / tileUnits - camera.x, motion.y * tilePixels / tileUnits - camera.y)

private def spriteAlpha (color : UInt32) (opacity : Nat) : UInt32 :=
  (color &&& 0xffffff00) ||| (min 255 opacity).toUInt32

private def spriteHeading (dx dy : Int) : Float :=
  Float.atan2 dy.toFloat dx.toFloat * 57.29577951308232

private def drawWallFace (camera : Camera) (light : LightField) (centerX y rotation : Float32)
    (frame : Nat) (mouth mirror : Bool) : IO Unit := do
  let frameWidth : Nat := if mouth then 142 else 130
  let w := (frameWidth * 9 + 2) / 5
  let x := npcScreenPixel centerX camera.x
  let screenY := npcScreenPixel (y + 71) camera.y
  if x + 150 < 0 || x ≥ width + 150 || screenY + 150 < 0 || screenY ≥ height + 150 then return
  discard <| originalSprite (if mouth then "NPC_113" else "NPC_114") (x - (w / 2 : Nat))
      (screenY - 95) w 191 0 (106 * frame) frameWidth 106
      (color := npcLightTint light centerX (y + 60)) (mirror := mirror)
      (rotation := rotation.toFloat * 57.29577951308232)
private def drawWallOfFlesh (wall : WallOfFlesh) (camera : Camera) (light : LightField) : IO Unit := do
  drawWallFace camera light wall.centerX wall.y wall.rotation wall.frame true (wall.direction > 0)
  for eye in wall.eyes do
    drawWallFace camera light eye.centerX eye.y eye.rotation eye.frame false (wall.direction > 0)

private def drawWallHungry (hungry : WallHungry) (camera : Camera) (light : LightField)
    (player : Player) (clock : Nat) : IO Unit := do
  let centerX := hungry.x + 15
  let centerY := hungry.y + if hungry.attached then 15 else 16
  let frameWidth : Nat := if hungry.attached then 44 else 110
  let frameHeight : Nat := if hungry.attached then 38 else 50
  let cycle := clock % 15
  let frame := if !hungry.attached then clock / 5 % 6 else
    if cycle < 3 then 0 else if cycle < 6 || cycle ≥ 12 then 1 else 2
  let x := npcScreenPixel centerX camera.x
  let y := npcScreenPixel (hungry.y + if hungry.attached then 15 else 11) camera.y
  if x + 100 < 0 || x ≥ width + 100 || y + 100 < 0 || y ≥ height + 100 then return
  let playerX := (Int32.ofInt (bodyCenterX player)).toFloat32 / 16
  let mirror := if hungry.attached then playerX > centerX else hungry.vx > 0
  let angle := if hungry.attached || hungry.vx == 0 then hungry.rotation.toFloat else
    Float.atan2 hungry.vy.toFloat hungry.vx.toFloat + if hungry.vx < 0 then 3.14 else 0
  let asset := if hungry.attached then "NPC_115" else "NPC_116"
  discard <| originalSprite asset (x - (frameWidth * 3 / 4 : Nat)) (y - (frameHeight * 3 / 4 : Nat))
      (frameWidth * 3 / 2) (frameHeight * 3 / 2) 0 (frameHeight * frame) frameWidth frameHeight
      (color := npcLightTint light centerX centerY) (mirror := mirror)
      (rotation := angle * 57.29577951308232)
private def drawWallLaser (shot : WallLaser) (camera : Camera) : IO Unit := do
  -- Native type 83 starts at alpha 255, subtracts 15 each subupdate, and
  -- stays invisible while alpha is at least 200.
  let fade := min 255 (15 * (wallLaserLife - shot.life))
  if fade ≤ 55 then return
  let x := npcScreenPixel shot.x camera.x
  let y := npcScreenPixel shot.y camera.y
  if x + 80 < 0 || x ≥ width + 80 || y + 80 < 0 || y ≥ height + 80 then return
  let angle := Float.atan2 shot.vy.toFloat shot.vx.toFloat
  discard <| originalSprite "Projectile_83" (x - 2) (y - 76) 5 153 0 0 2 60
      (color := spriteAlpha 0xffffffff fade) (additive := true)
      (rotation := (angle + 1.57) * 57.29577951308232)
private def drawShortsword (state : GameState) (thrust : ShortswordThrust) (camera : Camera)
    (light : LightField) : IO Unit := do
  let (centerX, centerY) := thrust.center state.player
  let x := npcScreenPixel centerX camera.x
  let y := npcScreenPixel centerY camera.y
  let direction := thrust.spriteDirection
  let left := direction < 0
  let heading := thrust.rotation.toFloat - 1.5707963267948966
  let bladeAngle := thrust.rotation.toFloat - 0.7853981633974483 * direction.toFloat
  let opacity := (thrust.opacity * 255 + 0.5).toUInt32.toNat
  let playerX := state.player.x * tilePixels / tileUnits - camera.x
  let playerY := state.player.y * tilePixels / tileUnits - camera.y
  let tile := state.player.centerTile
  let tint := npcLightTint light (Int32.ofInt (tile.x * 16 + 8)).toFloat32
    (Int32.ofInt (tile.y * 16 + 8)).toFloat32
  discard <| originalSprite "Projectile_938" (x - 24) (y - 24) 48 48 0 0 32 32
      (color := spriteAlpha tint opacity) (mirror := left)
      (rotation := bladeAngle * 57.29577951308232)
  -- Fit the original composite arm frames to the existing scaled avatar.
  let period := Item.swordUseTime .copperShortsword
  let progress := (period - state.combat.swing % period) * 100
  let frame := if progress ≤ 33 * period then 2 else if progress ≤ 66 * period then 1 else 0
  let armAngle := heading - 1.5707963267948966
  let armDirection : Int := if state.combat.facing < 0 then -1 else 1
  let offset := 5 * armDirection.toFloat
  let armX := playerX + 8 - 4 * armDirection + (armAngle.cos * offset).toInt64.toInt
  let armY := playerY + 13 + (armAngle.sin * offset).toInt64.toInt
  let hurt := state.combat.invulnerable > 0 && state.combat.invulnerable / 4 % 2 == 1
  let skin : UInt32 := if hurt then 0xffa690ff else 0xf7c392ff
  let cloth : UInt32 := if state.equipment.armor.isSome then 0xa993d2ff else 0x62b9a9ff
  let drawArm (asset : String) (color : UInt32) := do
    discard <| originalSprite asset (armX - 20) (armY - 24) 40 47 280 (56 * frame) 40 56
      (color := color) (mirror := armDirection < 0) (rotation := armAngle * 57.29577951308232)
  if state.equipment.armor == some .crystalAssassinShirt then
    drawArm "Armor/Armor_230" 0xffffffff
  else
    drawArm "Player_0_7" skin
    drawArm "Player_0_8" cloth

private def drawDungeonSlime (slime : DungeonSlime) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel (slime.x - 5.5) camera.x
  let y := npcScreenPixel (slime.y - 8.5) camera.y
  if x + 83 < 0 || width ≤ x || y + 64 < 0 || height ≤ y then return
  let keyX := npcScreenPixel (slime.x + 15 - 0.6 * slime.vx) camera.x
  let keyY := npcScreenPixel (slime.y + 3 - 0.3 * slime.vy) camera.y
  let rotation := (0.09 * slime.vx).toFloat * 57.29577951308232
  discard <| originalSprite "Item_327" keyX keyY 21 36 (rotation := rotation)
  discard <| originalSprite "NPC_71" x y 83 64 0 (34 * slime.frame) 44 34 (color := 0xffffffc3)
private def drawDarkCaster (caster : DarkCaster) (camera : Camera) (light : LightField) : IO Unit := do
  let centerX := caster.centerX
  let bottom := caster.y + (Int32.ofInt darkCasterHeight).toFloat32
  -- The original 40×56 frame ends four pixels below the collider's feet.
  let x := npcScreenPixel (centerX - 20) camera.x
  let y := npcScreenPixel (bottom - 52) camera.y
  if x + 60 < 0 || width ≤ x || y + 84 < 0 || height ≤ y then return
  let frame := if caster.vy != 0 then 2 else if caster.casting > 0 then 1 else 0
  let left := caster.spriteDirection < 0
  let tint := npcLightTint light centerX caster.centerY
  discard <| originalSprite "NPC_32" x y 60 84 0 (56 * frame) 40 56 (color := tint)
      (mirror := !left)
private def drawFireImp (imp : FireImp) (camera : Camera) (light : LightField) : IO Unit := do
  let x := npcScreenPixel (imp.centerX - 27) camera.x
  let y := npcScreenPixel (imp.y - 12) camera.y
  if x + 81 < 0 || width ≤ x || y + 84 < 0 || height ≤ y then return
  let mirror := imp.spriteDirection == 1
  let tint := npcLightTint light imp.centerX imp.centerY
  let frameY := imp.frame * 56
  discard <| originalSprite "NPC_24" x y 81 84 0 frameY 54 56 (color := tint) (mirror := mirror)
  let _ ← originalSprite "Glow_360" x y 81 84 0 frameY 54 56 (mirror := mirror)

private def drawLavaSlime (slime : LavaSlime) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel (slime.centerX - 17.6) camera.x
  let y := npcScreenPixel (slime.y + 19 + 4 - 28.6) camera.y
  if x + 53 < 0 || width ≤ x || y + 43 < 0 || height ≤ y then return
  discard <| originalSprite "NPC_59" x y 53 43 0 (26 * slime.frame) 32 26
      (color := 0xffffffc8) (additive := true)
private def drawUnderworldFlyer (flyer : UnderworldFlyer) (camera : Camera) (light : LightField) : IO Unit := do
  let (frameWidth, frameHeight) : Nat × Nat := match flyer.kind with
    | .hellbat => (44, 32)
    | .demon => (86, 62)
    | .voodooDemon => (86, 88)
  let scale := flyer.kind.scale
  let w := (frameWidth.toFloat.toFloat32 * scale * 1.5 + 0.5).toUInt32.toNat
  let h := (frameHeight.toFloat.toFloat32 * scale * 1.5 + 0.5).toUInt32.toNat
  let x := npcScreenPixel (flyer.centerX - frameWidth.toFloat.toFloat32 * scale / 2) camera.x
  let bottom := flyer.y + flyer.kind.height.toFloat.toFloat32
  let y := npcScreenPixel (bottom + 4 - frameHeight.toFloat.toFloat32 * scale) camera.y
  if x + w < 0 || width ≤ x || y + h < 0 || height ≤ y then return
  let bat := flyer.kind == .hellbat
  let tint := if bat then 0xffffffc8 else npcLightTint light flyer.centerX flyer.centerY
  let sprite := s!"NPC_{flyer.kind.sprite}"
  discard <| originalSprite sprite x y w h 0 (frameHeight * flyer.frame) frameWidth frameHeight
      (color := tint) (mirror := flyer.spriteDirection == 1) (additive := bat)
      (rotation := flyer.rotation.toFloat * 57.29577951308232)
private def drawTempleEnemy (enemy : TempleEnemy) (camera : Camera) (light : LightField) : IO Unit := do
  let (frameWidth, frameHeight) : Nat × Nat := match enemy.kind with
    | .lihzahrd => (40, 56)
    | .crawlingLihzahrd => (66, 50)
    | .flyingSnake => (64, 62)
  let w := frameWidth * 3 / 2
  let h := frameHeight * 3 / 2
  let x := npcScreenPixel (enemy.centerX - frameWidth.toFloat.toFloat32 / 2) camera.x
  let bottom := enemy.y + enemy.kind.height.toFloat.toFloat32
  let y := npcScreenPixel (bottom + 4 - frameHeight.toFloat.toFloat32 + enemy.gfxOffsetY) camera.y
  if x + w < 0 || width ≤ x || y + h < 0 || height ≤ y then return
  discard <| originalSprite s!"NPC_{enemy.kind.npcId}" x y w h 0 (frameHeight * enemy.frame) frameWidth frameHeight
    (color := npcLightTint light enemy.centerX enemy.centerY) (mirror := enemy.spriteDirection == 1)
    (rotation := enemy.rotation.toFloat * 57.29577951308232)

private def drawUnderworldCritter (critter : UnderworldCritter) (camera : Camera) (light : LightField) : IO Unit := do
  let (frameWidth, frameHeight, glow) : Nat × Nat × Option String := match critter.kind with
    | .hellButterfly => (24, 24, some "Glow_288")
    | .lavafly => (14, 16, some "Glow_290")
    | .magmaSnail => (42, 24, none)
  let scale := critter.scale * 1.5
  let w := (frameWidth.toFloat.toFloat32 * scale + 0.5).toUInt32.toNat
  let h := (frameHeight.toFloat.toFloat32 * scale + 0.5).toUInt32.toNat
  let snail := critter.kind == .magmaSnail
  let wall := snail && critter.ai2 == 0 && (critter.rotation == 1.57 || critter.rotation == 4.71)
  let ceiling := snail && critter.ai2 == 0 && (critter.rotation == 3.14 || critter.rotation == (-3.14))
  let pivotX := critter.centerX - if wall then (Int32.ofInt critter.direction).toFloat32 else 0
  let x := npcScreenPixel pivotX camera.x - (w / 2 : Nat)
  let bottom := critter.y + critter.kind.height.toFloat.toFloat32
  let y := npcScreenPixel (bottom + if ceiling then 6 else 4) camera.y - h
  if x + w < 0 || width ≤ x || y + h < 0 || height ≤ y then return
  let mirror := critter.spriteDirection == 1
  let degrees := critter.rotation.toFloat * 57.29577951308232
  let sourceY := critter.frame * frameHeight
  let tint := if snail then 0xffa532ff else npcLightTint light critter.centerX critter.centerY
  discard <| originalSprite s!"NPC_{critter.kind.sprite}" x y w h 0 sourceY frameWidth frameHeight
      (color := tint) (mirror := mirror) (rotation := degrees)
  if let some asset := glow then
    let _ ← originalSprite asset x y w h 0 sourceY frameWidth frameHeight
      (mirror := mirror) (rotation := degrees) (additive := true)
    pure ()

private def drawAngryBones (bones : AngryBones) (camera : Camera) (light : LightField) : IO Unit := do
  let scale := bones.kind.scale
  let bottom := bones.y + (Int32.ofInt bones.kind.height).toFloat32
  let x := npcScreenPixel (bones.centerX - 20 * scale) camera.x
  let y := npcScreenPixel (bottom + 4 - 56 * scale) camera.y
  let spriteWidth := (60 * scale + 0.5).toUInt32.toNat
  let spriteHeight := (84 * scale + 0.5).toUInt32.toNat
  if x + spriteWidth < 0 || width ≤ x || y + spriteHeight < 0 || height ≤ y then return
  let sprite := bones.kind.sprite
  let mirror := bones.spriteDirection == 1
  let tint := npcLightTint light bones.centerX bones.centerY
  discard <| originalSprite s!"NPC_{sprite}" x y spriteWidth spriteHeight 0 (56 * bones.frame) 40 56
      (color := tint) (mirror := mirror)
private def drawWraith (wraith : Wraith) (camera : Camera) (light : LightField) : IO Unit := do
  let x := npcScreenPixel wraith.centerX camera.x
  let y := npcScreenPixel (wraith.y + 23) camera.y
  if x + 40 < 0 || x ≥ width + 40 || y + 45 < 0 || y ≥ height + 45 then return
  let tint := spriteAlpha (npcLightTint light wraith.centerX wraith.centerY) 155
  discard <| originalSprite "NPC_82" (x - 19) (y - 37) 39 75 0 (50 * wraith.frame) 26 50
      (color := tint) (mirror := wraith.spriteDirection == 1)
      (rotation := wraith.rotation.toFloat * 57.29577951308232)
private def drawClingerTether (enemy : CavernEnemy) (anchor : TilePos) (world : World)
    (camera : Camera) (light : LightField) : IO Unit := do
  let dx := (Int32.ofInt (16 * anchor.x + 8)).toFloat32 - enemy.centerX
  let dy := (Int32.ofInt (16 * anchor.y + 8)).toFloat32 - enemy.centerY
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if length ≤ 12 then return
  let degrees := (Float.atan2 dy.toFloat dx.toFloat - 1.57) * 57.29577951308232
  for index in [:((length - 12) / 15).ceil.toUInt32.toNat] do
    let distance := (15 * (index + 1)).toFloat.toFloat32
    let wx := enemy.centerX + dx * distance / length
    let wy := enemy.centerY + dy * distance / length
    -- The original draws Clinger behind tiles. Suppress links whose centers are buried.
    if (world.tile (npcPixelTile wx wy)).solid then continue
    let x := npcScreenPixel wx camera.x
    let y := npcScreenPixel wy camera.y
    if x + 27 < 0 || width + 27 ≤ x || y + 27 < 0 || height + 27 ≤ y then continue
    let asset := if index % 2 == 0 then "Chain11" else "Chain10"
    let remaining := length - 15 * index.toFloat.toFloat32
    let sourceHeight := if remaining < 21 then min 24 (remaining.toUInt32.toNat - 12) else 24
    discard <| originalSprite asset (x - 19) (y - 15) 38 (sourceHeight * 9 / 8)
        0 0 34 sourceHeight (color := npcLightTint light wx wy) (rotation := degrees)
private def drawCavernEnemy (enemy : CavernEnemy) (world : World) (camera : Camera) (light : LightField) : IO Unit := do
  if let some anchor := enemy.anchor then drawClingerTether enemy anchor world camera light
  let (frameWidth, frameHeight, drawLift, alpha) : Nat × Nat × Nat × Nat := match enemy.kind with
    | .illuminantBat => (44, 32, 0, 255)
    | .illuminantSlime => (32, 26, 0, 155)
    | .clinger => (62, 42, 0, 255)
    | .floatyGross => (34, 88, 24, 155)
  let pivotX := enemy.centerX
  let bottom := enemy.y + enemy.kind.height.toFloat.toFloat32
  let scale := enemy.kind.scale
  let w := (frameWidth.toFloat.toFloat32 * scale * 1.5 + 0.5).toUInt32.toNat
  let h := (frameHeight.toFloat.toFloat32 * scale * 1.5 + 0.5).toUInt32.toNat
  let pivotY := bottom + 4 + drawLift.toFloat.toFloat32 - frameHeight.toFloat.toFloat32 * scale / 2
  let x := npcScreenPixel pivotX camera.x - (w / 2 : Nat)
  let y := npcScreenPixel pivotY camera.y - (h / 2 : Nat)
  if x + w < 0 || width ≤ x || y + h < 0 || height ≤ y then return
  if enemy.kind == .clinger && (world.tile (npcPixelTile enemy.centerX enemy.centerY)).solid then return
  let mirror := enemy.spriteDirection == 1
  let rotation := enemy.rotation.toFloat * 57.29577951308232
  let asset := s!"NPC_{enemy.kind.sprite}"
  let frameY := frameHeight * enemy.frame
  let illuminant := enemy.kind == .illuminantBat || enemy.kind == .illuminantSlime
  if illuminant && (enemy.vx != 0 || enemy.vy != 0) then
    -- DrawNPCDirect uses nine oldPos samples. The compact save retains current velocity.
    for index in [:9] do
      let framesBack := 9 - index
      let delta := framesBack.toFloat.toFloat32
      let tx := npcScreenPixel (pivotX - enemy.vx * delta) camera.x - (w / 2 : Nat)
      let ty := npcScreenPixel (pivotY - enemy.vy * delta) camera.y - (h / 2 : Nat)
      let tint := spriteAlpha 0x966496ff ((10 - framesBack) * 50 / 15)
      let _ ← originalSprite asset tx ty w h 0 frameY frameWidth frameHeight
        (color := tint) (mirror := mirror) (rotation := rotation) (additive := true)
  let tint := spriteAlpha (npcLightTint light enemy.centerX enemy.centerY) alpha
  discard <| originalSprite asset x y w h 0 frameY frameWidth frameHeight
      (color := tint) (mirror := mirror) (rotation := rotation)
private def drawCursedSkull (skull : CursedSkull) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel (skull.centerX - 14) camera.x
  let y := npcScreenPixel (skull.centerY - 12) camera.y
  if x + 52 < 0 || width + 10 ≤ x || y + 55 < 0 || height + 10 ≤ y then return
  let mirror := skull.spriteDirection == 1
  let rotation := skull.rotation.toFloat * 57.29577951308232
  -- The original style 10 skull is full-bright, even as it crosses a wall.
  discard <| originalSprite "NPC_34" x y 42 45 0 (30 * skull.frame) 28 30
      (mirror := mirror) (rotation := rotation)
private def drawAquaDust (x y : Int) (size phase alpha : Nat) (steam := false)
    (tint : UInt32 := 0xfafafaff) : IO Unit := do
  let px := x - (size / 2 : Nat)
  let py := y - (size / 2 : Nat)
  let frameX := if steam then 310 else 720
  let frameY := (if steam then 0 else 30) + phase % 3 * 10
  discard <| originalSprite "Dust" px py size size frameX frameY 8 8
      (color := spriteAlpha tint alpha)
private def drawWaterSphere (sphere : WaterSphere) (camera : Camera) (clock : Nat) : IO Unit := do
  let (centerX, centerY) := waterSphereCenter sphere
  let x := npcScreenPixel centerX camera.x
  let y := npcScreenPixel centerY camera.y
  if x + 96 < 0 || width + 96 ≤ x || y + 96 < 0 || height + 96 ≤ y then return
  -- NPC_33 is transparent in the original. Its visible trail is Dust type 172.
  for index in [:7] do
    let age := 6 - index
    let phase := clock * 7 + sphere.id * 19 + age * 23
    let lag := (Int32.ofInt age).toFloat32 * 0.7
    let driftX := (Int32.ofInt (Int.ofNat (phase * 29 % 13) - 6)).toFloat32 / 2
    let driftY := (Int32.ofInt (Int.ofNat (phase * 31 % 17) - 8)).toFloat32 / 2
    let size := 15 - age
    let px := npcScreenPixel (centerX - sphere.vx * lag + driftX) camera.x
    let py := npcScreenPixel (centerY - sphere.vy * lag + driftY) camera.y
    drawAquaDust px py size phase (150 - 12 * age)

private def drawAquaSteam (stream : AquaStream) (camera : Camera) (light : LightField)
    (bounceAge : Nat) : IO Unit := do
  let elapsed := stream.age - bounceAge
  let samples := min elapsed (min 4 (stream.age - 4))
  let origin (lag : Float32) : Float32 × Float32 :=
    (stream.x - stream.vx * lag, stream.y - stream.vy * lag + 0.075 * lag * (lag - 1) / 2)
  -- Use the original smoke and water atlas frames with a short reconstructed trail.
  for index in [:samples] do
    let older := samples - 1 - index
    let phase := (stream.age - older) * 19 + stream.vx.toBits.toNat
    let lag := (Int32.ofNat (older + 1)).toFloat32
    let (x, y) := origin lag
    let tint := npcLightTint light x y
    if phase % 2 == 0 then
      let drift := (Int32.ofInt (Int.ofNat (phase % 9) - 4)).toFloat32 / 2
      drawAquaDust (npcScreenPixel (x + drift) camera.x) (npcScreenPixel (y - lag / 2) camera.y)
        (10 - older) phase (155 - 18 * older) (steam := true) (tint := tint)
    if phase / 2 % 2 == 0 then
      for drop in [:3] do
        let (px, py) := origin (lag + (Int32.ofNat drop).toFloat32 / 3)
        drawAquaDust (npcScreenPixel px camera.x) (npcScreenPixel py camera.y)
          (15 - older) (phase + drop) (150 - 24 * older)
      if phase / 4 % 8 == 0 then
        drawAquaDust (npcScreenPixel (x + 2) camera.x) (npcScreenPixel (y + 3) camera.y)
          9 phase (150 - 24 * older)
  if elapsed < 7 then
    let lag := (Int32.ofNat (elapsed + 1)).toFloat32
    let (x, y) := origin lag
    let tint := npcLightTint light x y
    for puff in [:4] do
      let side := (Int32.ofInt (Int.ofNat puff * 2 - 3)).toFloat32
      drawAquaDust (npcScreenPixel (x + side * (2 + lag / 4)) camera.x)
        (npcScreenPixel (y - lag * 1.4 + side * side / 2) camera.y)
        (18 - elapsed / 2) (bounceAge + puff) (155 - 16 * elapsed) (steam := true) (tint := tint)

private def drawAquaStream (stream : AquaStream) (camera : Camera) (light : LightField) : IO Unit := do
  let x := npcScreenPixel stream.x camera.x
  let y := npcScreenPixel stream.y camera.y
  if x + 128 < 0 || width + 128 ≤ x || y + 128 < 0 || height + 128 ≤ y then return
  if let some bounceAge := stream.lavaBounceAge then
    drawAquaSteam stream camera light bounceAge
    if bounceAge < stream.age then return
  -- Projectile_22 has no visible sprite. The original trail starts at subupdate five.
  let samples := min 9 (3 * (stream.age - 4))
  for index in [:samples] do
    let sample := samples - 1 - index
    let phase := stream.age * 19 + sample * 31 + stream.x.toInt32.toInt.natAbs
    let lag := (Int32.ofInt (sample + 1)).toFloat32 / 3
    let driftX := (Int32.ofInt (Int.ofNat (phase * 17 % 9) - 4)).toFloat32 / 2
    let driftY := (Int32.ofInt (Int.ofNat (phase * 23 % 13) - 6)).toFloat32 / 2
    let px := npcScreenPixel (stream.x - stream.vx * lag + driftX) camera.x
    let py := npcScreenPixel (stream.y - stream.vy * lag + driftY) camera.y
    drawAquaDust px py (15 - sample / 2) phase (150 - 9 * sample)

private def drawValor (player : Player) (yoyo : Yoyo) (camera : Camera) (light : LightField) : IO Unit := do
  let ownerX := (bodyCenterX player).toFloat / 16
  let ownerY := (bodyCenterY player).toFloat / 16
  let headX := yoyo.x.toFloat
  let headY := yoyo.y.toFloat
  let dx := headX - ownerX
  let dy := headY - ownerY
  let distance := (dx * dx + dy * dy).sqrt
  if distance > 0 then
    let mut x := ownerX - 1.2 * dx / distance
    let mut y := ownerY - 1.2 * dy / distance
    let mut dx := headX - x
    let mut dy := headY - y
    let speed := min 16 (yoyo.vx.abs + yoyo.vy.abs).toFloat
    for index in [:48] do
      let distance := (dx * dx + dy * dy).sqrt
      if distance ≤ 8 then break
      let final := distance < 20
      let sourceLength := if final then (distance - 8).toUInt64.toNat else 12
      if index > 0 then
        x := x + dx * 12 / distance
        y := y + dy * 12 / distance
      dx := headX - x
      dy := headY - 6.8 - y
      if distance > 12 then
        let factor := min 1 (distance / 80)
        let sag := 0.15 * (1 - speed / 16) * factor * factor
        let sag := if dy > 0 then sag else sag * (min 1 (yoyo.vx.abs.toFloat / 3) - 0.5)
        let sag := if dy ≤ 0 && sag > 0 then sag * 2 else sag
        dx := dx * (1 - sag)
        dy := dy * (1 + sag)
      let heading := Float.atan2 dy dx
      let half := sourceLength.toFloat / 2
      let px := npcScreenPixel (x + heading.cos * half).toFloat32 camera.x
      let py := npcScreenPixel (y + 6 + heading.sin * half).toFloat32 camera.y
      let tint := spriteAlpha (npcLightTint light x.toFloat32 y.toFloat32) 64
      discard <| originalSprite "FishingLine" (px - 9) (py - (sourceLength * 3 / 4 : Nat))
          18 (sourceLength * 3 / 2) 0 0 12 sourceLength (color := tint)
          (rotation := (heading - 1.5700000524520874) * 57.29577951308232)
      if final then break
  let x := npcScreenPixel yoyo.x camera.x
  let y := npcScreenPixel yoyo.y camera.y
  let tint := npcLightTint light yoyo.x yoyo.y
  discard <| originalSprite "Projectile_564" (x - 13) (y - 13) 26 26 0 0 16 16
      (color := tint) (rotation := yoyo.rotation.toFloat * 57.29577951308232)
private def missileTrailSamples (missile : MagicMissile) (camera : Camera) : FloatArray :=
  missile.trail.foldl (fun (vertices : FloatArray) point =>
    vertices.push (point.x.toFloat * 1.5 - camera.x.toFloat)
      |>.push (point.y.toFloat * 1.5 - camera.y.toFloat) |>.push point.rotation.toFloat)
    (FloatArray.emptyWithCapacity (3 * missile.trail.size))

private def drawMagicMissileTrail (missile : MagicMissile) (camera : Camera) (clock : Nat)
    (opacity : Float) : IO Unit := do
  let clockPhase := (clock % 90).toFloat / 90
  let samples := missileTrailSamples missile camera
  discard <| Desktop.magicMissileTrail samples clockPhase opacity
private def drawMagicMissile (missile : MagicMissile) (camera : Camera) (clock : Nat) : IO Unit := do
  drawMagicMissileTrail missile camera clock 1
  let x := npcScreenPixel missile.x camera.x
  let y := npcScreenPixel missile.y camera.y
  if x + 90 < 0 || width + 90 ≤ x || y + 90 < 0 || height + 90 ≤ y then return
  let speed := (missile.vx * missile.vx + missile.vy * missile.vy).toFloat.sqrt
  let stretch := min 1 (speed / 8)
  let width := 81 * 0.6 * (1 + 0.5 * stretch)
  let height := 81 * 0.6 * (1 - 0.2 * stretch)
  let angle := missile.rotation.toFloat * stretch
  let degrees := angle * 57.29577951308232
  let pulse := 1 + 0.4 * (6.283185307179586 * (clock % 60).toFloat / 60).cos
  let glowWidth := (width * pulse).toUInt64.toNat
  let glowHeight := (height * pulse).toUInt64.toNat
  for ray in [:4] do
    let heading := angle + ray.toFloat * 1.5707963267948966
    let dx := (heading.cos * width * pulse / 24).toInt64.toInt
    let dy := (heading.sin * width * pulse / 24).toInt64.toInt
    let _ ← originalSprite "Projectile_16" (x + dx - (glowWidth / 2 : Nat)) (y + dy - (glowHeight / 2 : Nat))
      glowWidth glowHeight 0 0 54 54 (color := 0xffffff45) (rotation := degrees) (additive := true)
  let width := width.toUInt64.toNat
  let height := height.toUInt64.toNat
  discard <| originalSprite "Projectile_16" (x - (width / 2 : Nat)) (y - (height / 2 : Nat)) width height
      0 0 54 54 (rotation := degrees) (additive := true)
private def drawMagicMissileImpact (missile : MagicMissile) (camera : Camera) (clock ticks : Nat) : IO Unit := do
  drawMagicMissileTrail missile camera clock (ticks.toFloat / (2 * magicMissileFlashTime).toFloat)
  let age := magicMissileFlashTime - ticks
  for particle in [:18] do
    let angle := particle.toFloat * 2.399963229728653
    let distance := (age + 3).toFloat * (1 + (particle % 5).toFloat / 3)
    let x := npcScreenPixel (missile.x + (angle.cos * distance).toFloat32) camera.x
    let y := npcScreenPixel (missile.y + (angle.sin * distance).toFloat32) camera.y
    let size := 8 + (particle % 3) - age / 4
    let tint := spriteAlpha (if particle % 3 == 0 then 0xa58fffff else 0x8fdcffff) (ticks * 12)
    discard <| originalSprite "Dust" (x - (size / 2 : Nat)) (y - (size / 2 : Nat)) size size
        150 ((particle % 3) * 10) 8 8 (color := tint) (additive := true)
private def drawWeaponDust (kind size phase alpha : Nat) (x y : Int) : IO Unit := do
  let left := x - (size / 2 : Nat)
  let top := y - (size / 2 : Nat)
  discard <| originalSprite "Dust" left top size size (kind % 100 * 10) (kind / 100 * 30 + phase % 3 * 10)
      8 8 (color := spriteAlpha 0xffffffff alpha)
private def drawCavernFlame (flame : CavernFlame) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel flame.x camera.x
  let y := npcScreenPixel flame.y camera.y
  if x + 75 < 0 || width + 75 ≤ x || y + 75 < 0 || height + 75 ≤ y then return
  for index in [:5] do
    let age := (index + 1).toFloat.toFloat32
    let jitter := ((flame.clock + index * 3) % 5 : Int) - 2
    let px := npcScreenPixel (flame.x - flame.vx * age) camera.x + jitter
    let py := npcScreenPixel (flame.y - flame.vy * age) camera.y - jitter
    drawWeaponDust 75 (12 - index) (flame.clock + index) (170 - 27 * index) px py
  let direction : Float := if flame.vx < 0 then -1 else 1
  let degrees := 0.3 * direction * flame.clock.toFloat * 57.29577951308232
  discard <| originalSprite "Projectile_96" (x - 15) (y - 15) 31 31 0 0 16 16
      (color := 0xffffff9b) (rotation := degrees)
private def drawTwinShot (shot : TwinShot) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.x camera.x
  let y := npcScreenPixel shot.y camera.y
  if x + 110 < 0 || width + 110 ≤ x || y + 110 < 0 || height + 110 ≤ y then return
  let asset := s!"Projectile_{shot.kind.sprite}"
  if shot.kind.isLaser then
    let fade := min 255 (15 * shot.age)
    if fade ≤ 55 then return
    let angle := (Float.atan2 shot.vy.toFloat shot.vx.toFloat + 1.57) * 57.29577951308232
    let scale := match shot.kind with
      | .probeLaser => 76
      | .destroyerLaser => 90
      | .eyeLaser => 153
      | _ => 162
    discard <| originalSprite asset (x - 2) (y - (scale / 2 : Nat)) 5 scale
        (color := spriteAlpha 0xffffffff fade) (rotation := angle) (additive := true)
  else if shot.kind == .cursedFlame then
    drawCavernFlame ⟨shot.x, shot.y, shot.vx, shot.vy, shot.life, shot.age, shot.slot⟩ camera
  else
    for i in [:6] do
      let lag := ((i * 2 + 1 : Nat).toFloat).toFloat32
      let px := npcScreenPixel (shot.x - shot.vx * lag) camera.x
      let py := npcScreenPixel (shot.y - shot.vy * lag) camera.y
      drawWeaponDust 75 (13 - i) (shot.age + i) (220 - 30 * i) px py

private def drawBurningSphere (sphere : BurningSphere) (camera : Camera) (clock : Nat) : IO Unit := do
  let x := npcScreenPixel sphere.centerX camera.x
  let y := npcScreenPixel sphere.centerY camera.y
  if x + 62 < 0 || width + 62 ≤ x || y + 62 < 0 || height + 62 ≤ y then return
  for sample in [:4] do
    let phase := clock * 7 + sphere.id * 19 + sample * 29
    let lag := (sample + 1).toFloat.toFloat32 * 0.8
    let rise := (Int32.ofInt (Int.ofNat (phase % 7) - 3)).toFloat32
    let dustX := npcScreenPixel (sphere.centerX - sphere.vx * lag) camera.x
    let dustY := npcScreenPixel (sphere.centerY - sphere.vy * lag + rise) camera.y
    drawWeaponDust 6 (22 - sample * 3) phase (215 - sample * 42) dustX dustY
  discard <| originalSprite "NPC_25" (x - 12) (y - 12) 24 24 0 0 16 16
      (color := 0xffffffc8) (additive := true)
private def drawDemonSickle (shot : DemonSickle) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.x camera.x
  let y := npcScreenPixel shot.y camera.y
  if x + 70 < 0 || width + 70 ≤ x || y + 70 < 0 || height + 70 ≤ y then return
  let sprite := if shot.hostile then "Projectile_44" else "Projectile_45"
  discard <| originalSprite sprite (x - 32) (y - 32) 65 65 0 0 48 48
      (rotation := shot.rotation.toFloat * 57.29577951308232)
private def drawFlowerFireball (shot : FlowerFireball) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.x camera.x
  let y := npcScreenPixel shot.y camera.y
  let age := flowerFireLife - shot.life
  for sample in [:4] do
    let lag := (sample + 1).toFloat.toFloat32 / 2
    let rise := sample.toFloat.toFloat32 - ((age + sample) % 3).toFloat.toFloat32
    let px := npcScreenPixel (shot.x - shot.vx * lag) camera.x
    let py := npcScreenPixel (shot.y - shot.vy * lag + rise) camera.y
    drawWeaponDust 6 (15 - 2 * sample) (age + sample) (220 - 42 * sample) px py
  discard <| originalSprite "Projectile_15" (x - 12) (y - 12) 24 24
private def drawHellwingShot (shot : HellwingShot) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.x camera.x
  let y := npcScreenPixel shot.y camera.y
  let degrees := Float.atan2 shot.vy.toFloat shot.vx.toFloat * 57.29577951308232
  if shot.unholy then
    discard <| originalSprite "Projectile_4" (x - 10) (y - 24) 21 48 0 0 14 32 (rotation := degrees + 90)
  else
    let left := shot.vx < 0
    let heading := if left then Float.atan2 (-shot.vy).toFloat (-shot.vx).toFloat * 57.29577951308232 else degrees
    let phase := hellwingShotLife - shot.life
    let dustX := npcScreenPixel (shot.x - shot.vx) camera.x
    let dustY := npcScreenPixel (shot.y - shot.vy) camera.y
    drawWeaponDust 6 10 phase 155 dustX dustY
    discard <| originalSprite "Projectile_485" (x - 18) (y - 22) 36 45 0 (shot.frame * 30) 24 30
        (mirror := left) (rotation := heading)
private def drawFlamelashTrail (projectile : Flamelash) (camera : Camera) (clock : Nat)
    (opacity : Float) : IO Unit := do
  let missile := projectile.missile
  let phase := (clock % 90).toFloat / 90
  let empowerment := min 1 (projectile.empoweredTicks.toFloat / 6)
  discard <| Desktop.flamelashTrail (missileTrailSamples missile camera) phase opacity empowerment
private def drawFlamelash (projectile : Flamelash) (camera : Camera) (clock : Nat) : IO Unit := do
  drawFlamelashTrail projectile camera clock 1
  if projectile.empoweredTicks == 0 then return
  let missile := projectile.missile
  let x := npcScreenPixel (missile.x - missile.vx / 2) camera.x
  let y := npcScreenPixel (missile.y - missile.vy / 2) camera.y
  if x + 120 < 0 || width + 120 ≤ x || y + 120 < 0 || height + 120 ≤ y then return
  let speed := (missile.vx * missile.vx + missile.vy * missile.vy).toFloat.sqrt
  let stretch := min 1 (speed / 8)
  let angle := (missile.rotation.toFloat - 1.5707963267948966) * stretch
  let degrees := angle * 57.29577951308232
  let empowerment := min 1 (projectile.empoweredTicks.toFloat / 6)
  let w := (72 * empowerment).toUInt64.toNat
  let h := (96 * empowerment).toUInt64.toNat
  let frameY := projectile.frame * 64
  for ray in [:4] do
    let phase := angle + ray.toFloat * 1.5707963267948966
    let dx := (phase.cos * 6 * empowerment).toInt64.toInt
    let dy := (phase.sin * 6 * empowerment).toInt64.toInt
    let _ ← originalSprite "Projectile_34" (x + dx - (w / 2 : Nat)) (y + dy - (h / 2 : Nat)) w h
      0 frameY 48 64 (color := 0xffffff60) (rotation := degrees) (additive := true)

private def drawFlamelashImpact (projectile : Flamelash) (camera : Camera) (clock ticks : Nat) : IO Unit := do
  drawFlamelashTrail projectile camera clock (ticks.toFloat / (2 * magicMissileFlashTime).toFloat)
  let age := magicMissileFlashTime - ticks
  let missile := projectile.missile
  for particle in [:24] do
    let angle := particle.toFloat * 2.399963229728653
    let distance := (age + 4).toFloat * (1 + (particle % 5).toFloat / 2)
    let x := npcScreenPixel (missile.x + (angle.cos * distance).toFloat32) camera.x
    let y := npcScreenPixel (missile.y + (angle.sin * distance).toFloat32) camera.y
    drawWeaponDust 6 (13 + particle % 5 - age / 3) (age + particle) (min 255 (ticks * 17)) x y

private def drawFlail (state : GameState) (kind : FlailKind) (flail : Flail)
    (camera : Camera) (light : LightField) : IO Unit := do
  let sunfury := kind == .sunfury
  let chain := if sunfury then "Chain6" else "Chain3"
  let sprite := if sunfury then "Projectile_35" else "Projectile_26"
  let handX := (Int32.ofInt (bodyCenterX state.player)).toFloat32 / 16 +
    (Int32.ofInt state.combat.facing).toFloat32 * 5
  let handY := (Int32.ofInt (bodyCenterY state.player)).toFloat32 / 16 + 1
  let dx := handX - flail.x
  let dy := handY - flail.y
  let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if distance > 0 then
    let unitX := dx / distance
    let unitY := dy / distance
    let rotation := (Float.atan2 dy.toFloat dx.toFloat + 1.5707963267948966) * 57.29577951308232
    let count := min 76 (((max 0 (distance - 4) + 6) / 12).ceil.toUInt32.toNat)
    for index in [:count] do
      let advance := (Int32.ofNat index).toFloat32 * 12
      let x := flail.x + advance * unitX
      let y := flail.y + advance * unitY
      let px := npcScreenPixel x camera.x
      let py := npcScreenPixel y camera.y
      let tint := npcLightTint light x y
      discard <| originalSprite chain (px - 7) (py - 9) 15 18 0 0 10 12 (color := tint) (rotation := rotation)
  let x := npcScreenPixel flail.x camera.x
  let y := npcScreenPixel flail.y camera.y
  let phase := state.surface.clock * 23
  for lag in [:3] do
    let drift := (Int32.ofNat (lag + 1)).toFloat32 / 3
    let px := npcScreenPixel (flail.x - flail.vx * drift) camera.x
    let py := npcScreenPixel (flail.y - flail.vy * drift) camera.y
    if sunfury then drawWeaponDust 6 (17 - 2 * lag) (phase + 7 * lag) (200 - 28 * lag) px py
    else drawAquaDust px py (17 - 2 * lag) (phase + 7 * lag) (155 - 25 * lag)
  discard <| originalSprite sprite (x - 23) (y - 23) 46 46 0 0 38 38
      (color := npcLightTint light flail.x flail.y) (mirror := flail.spriteDirection < 0)
      (rotation := flail.rotation.toFloat * 57.29577951308232)
private def drawDarkLanceGlow (lance : DarkLance) (player : Player) (camera : Camera)
    (angle : Float) : IO Unit := do
  let some box := lance.extensionHitBox player | return
  let progress := min 1 ((lance.age - 1).toFloat / (darkLanceUseTime.toFloat * 2 / 3))
  let fade := min 1 (progress / 0.3) * min 1 ((1 - progress) / 0.7)
  let fade := 1 - (1 - fade) * (1 - fade)
  let ratio := ((box.right - box.left).tdiv 16).toFloat / 22
  let size := 72 * 1.65 * ratio * ratio
  let (centerX, centerY) := lance.center player
  let tipX := (box.left.tdiv 16 + (box.right - box.left).tdiv 32).toFloat
  let tipY := (box.top.tdiv 16 + (box.bottom - box.top).tdiv 32).toFloat
  let ownerX := (bodyCenterX player).toFloat / 16
  let ownerY := (bodyCenterY player).toFloat / 16
  let glow (x y wide tall opacity : Float) : IO Unit := do
    let w := wide.toUInt64.toNat
    let h := tall.toUInt64.toNat
    if w == 0 || h == 0 then return
    let _ ← originalSprite "Extra_98" (npcScreenPixel x.toFloat32 camera.x - (w / 2 : Nat))
      (npcScreenPixel y.toFloat32 camera.y - (h / 2 : Nat)) w h 0 0 72 72
      (color := spriteAlpha 0xb450ffff opacity.toUInt64.toNat)
      (rotation := angle * 57.29577951308232) (additive := true)
  glow ((tipX + centerX.toFloat) / 2) ((tipY + centerY.toFloat) / 2) (size * fade) size (185 * fade)
  glow centerX.toFloat centerY.toFloat (size * fade) (size * 1.5) (185 * fade)
  for step in [:7] do
    let sample := 0.4 + step.toFloat / 10
    let x := ownerX + (tipX - ownerX) * (sample + 0.2)
    let y := ownerY + (tipY - ownerY) * (sample + 0.2) + 2
    glow x y (size * fade * fade) (size * 2 * fade) (139 * fade * sample)

private def drawDarkLance (state : GameState) (lance : DarkLance) (camera : Camera) (light : LightField) : IO Unit := do
  let (x, y) := lance.center state.player
  let mirror := lance.facing > 0
  let heading := Float.atan2 lance.vy.toFloat lance.vx.toFloat
  let angle := heading + 2.355 - if mirror then 1.5707963705062866 else 0
  drawDarkLanceGlow lance state.player camera (angle - 0.7853981633974483 * lance.spriteDirection.toFloat)
  -- The original SpriteBatch anchors the spear's corner at the projectile.
  let pivotX : Float := if mirror then -55 else 55
  let dx := pivotX * angle.cos - 55 * angle.sin
  let dy := pivotX * angle.sin + 55 * angle.cos
  let px := npcScreenPixel (x + dx.toFloat32) camera.x
  let py := npcScreenPixel (y + dy.toFloat32) camera.y
  let ownerX := (Int32.ofInt (bodyCenterX state.player)).toFloat32 / 16
  let ownerY := (Int32.ofInt (bodyCenterY state.player)).toFloat32 / 16
  let tint := npcLightTint light ownerX ownerY
  discard <| originalSprite "Projectile_46" (px - 82) (py - 82) 165 165 0 0 100 100
      (color := tint) (mirror := mirror) (rotation := angle * 57.29577951308232)
  let clock := state.surface.clock
  for sample in [:2] do
    let phase := clock * 29 + sample * 37
    let driftX := (Int32.ofInt (Int.ofNat (phase % 13) - 6)).toFloat32
    let driftY := (Int32.ofInt (Int.ofNat (phase / 5 % 13) - 6)).toFloat32
    let lag := (Int32.ofNat sample).toFloat32 * 2
    let sx := npcScreenPixel (x + driftX - lag * lance.vx) camera.x
    let sy := npcScreenPixel (y + driftY - lag * lance.vy) camera.y
    drawWeaponDust 27 (16 - 2 * sample) phase (140 - 32 * sample) sx sy

private def drawSkeletronBone (baseX baseY dx dy reach : Float32) (camera : Camera) :
    IO (Float32 × Float32) := do
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if length == 0 then return (0, 0)
  let scale := reach / length
  let offsetX := dx * scale
  let offsetY := dy * scale
  let x := baseX + offsetX
  let y := baseY + offsetY
  let rotation := ((Float.atan2 dy.toFloat dx.toFloat).toFloat32 - 1.57).toFloat * 57.29577951308232
  discard <| originalSprite "Arm_Bone" (npcScreenPixel (x - 13) camera.x)
      (npcScreenPixel (y - 49) camera.y) 39 147 (rotation := rotation)
  return (offsetX, offsetY)

private def drawSkeletronHand (hand : SkeletronHand) (head : SkeletronHead) (side : Int)
    (camera : Camera) : IO Unit := do
  let offset := (Int32.ofInt side).toFloat32
  let headX := head.x + 40
  let headY := head.y + 51
  let wristX := hand.centerX + 5 * offset
  let wristY := hand.y + 20
  let (dx, dy) ← drawSkeletronBone wristX wristY (headX - wristX + 200 * offset)
    (headY - wristY + 130) 92 camera
  let jointX := wristX + dx + dx / 2
  let jointY := wristY + dy + dy / 2
  let _ ← drawSkeletronBone jointX jointY (headX - jointX + 50 * offset) (headY - jointY + 80) 60 camera
  let rotation := hand.rotation.toFloat * 57.29577951308232
  discard <| originalSprite "NPC_36" (npcScreenPixel (hand.centerX - 29) camera.x)
      (npcScreenPixel (hand.centerY - 48) camera.y) 87 117 0 0 58 78
      (mirror := side > 0) (rotation := rotation)
private def drawSkeletron (boss : Skeletron) (camera : Camera) : IO Unit := do
  let head := boss.head
  if let some hand := boss.leftHand then drawSkeletronHand hand head (-1) camera
  if let some hand := boss.rightHand then drawSkeletronHand hand head 1 camera
  let rotation := head.rotation.toFloat * 57.29577951308232
  discard <| originalSprite "NPC_35" (npcScreenPixel (head.x + 40 - 57) camera.x)
      (npcScreenPixel (head.y + 51 - 77) camera.y) 171 198 0 0 114 132
      (rotation := rotation)
private def drawDungeonGuardian (motion : EyeMotion) (camera : Camera) (clock : Nat) : IO Unit := do
  let (x, y) := eyeScreen motion camera
  discard <| originalSprite "NPC_68" (x - 60) (y - 76) 120 153
      (rotation := (clock * 16 % 360).toFloat)
private def drawWorm (boss : EaterOfWorlds) (state : GameState) (camera : Camera) : IO Unit := do
  for chain in boss.chains do
    for reverseIndex in [:chain.size] do
      let i := chain.size - 1 - reverseIndex
      let segment := chain[i]!
      let (x, y) := eyeScreen segment.motion camera
      if x + 72 < 0 || width ≤ x - 72 || y + 72 < 0 || height ≤ y - 72 then continue
      let head := i == 0
      let tail := !head && i + 1 == chain.size
      let previous := if head then chain[1]? else chain[i - 1]?
      let adjacent := previous.map (fun prior => eyeScreen prior.motion camera)
      if !head && adjacent == some (x, y) then continue
      let (tx, ty) := adjacent.getD (x - 1, y)
      let mx := segment.motion.vx
      let my := segment.motion.vy
      let (dx, dy) := if head then
          if mx != 0 || my != 0 then (mx, my) else (x - tx, y - ty)
        else (tx - x, ty - y)
      let dx := if dx == 0 && dy == 0 then 1 else dx
      let pos : TilePos := ⟨segment.motion.x / tileUnits, segment.motion.y / tileUnits⟩
      let buried := (state.world.tile pos).solid
      let asset := if head then "NPC_13" else if tail then "NPC_15" else "NPC_14"
      let tint : UInt32 := if buried then 0xb9a1c0b8 else 0xffffffff
      discard <| originalSprite asset (x - 34) (y - 67) 69 135 0 0 46 90 (color := tint)
          (rotation := spriteHeading dx dy + 90)

private def drawDestroyer (boss : Destroyer) (world : World) (camera : Camera)
    (light : LightField) : IO Unit := do
  for offset in [:boss.segments.size] do
    let index := boss.segments.size - offset - 1
    let part := boss.segments[index]!
    let x := npcScreenPixel part.centerX camera.x
    let y := npcScreenPixel part.centerY camera.y
    if part.alpha == 255 || x + 100 < 0 || width + 100 ≤ x || y + 100 < 0 || height + 100 ≤ y then continue
    if (world.tile (npcPixelTile part.centerX part.centerY)).solid then continue
    let npc := destroyerSprite index
    let frame := if part.released then 104 else 0
    let frameHeight := if npc == 135 then 104 else 102
    let opacity := 255 - part.alpha
    let tint := spriteAlpha (npcLightTint light part.centerX part.centerY) opacity
    let rotation := part.rotation.toFloat * 57.29577951308232
    let left := x - 47
    let top := y - 72
    discard <| originalSprite s!"NPC_{npc}" left top 94 191 0 frame 50 frameHeight
      (color := tint) (rotation := rotation)
    discard <| originalSprite s!"Dest{npc - 133}" left top 94 191 0 frame 50 frameHeight
      (color := spriteAlpha 0xffffffff opacity) (rotation := rotation) (additive := true)

private def drawDestroyerProbe (probe : DestroyerProbe) (camera : Camera) (light : LightField) : IO Unit := do
  let cx := probe.x + 15
  let cy := probe.y + 15
  let x := npcScreenPixel cx camera.x
  let y := npcScreenPixel cy camera.y
  if x + 24 < 0 || width + 24 ≤ x || y + 24 < 0 || height + 24 ≤ y then return
  let rotation := probe.rotation.toFloat * 57.29577951308232
  discard <| originalSprite "NPC_139" (x - 24) (y - 24) 48 48
    (color := npcLightTint light cx cy) (rotation := rotation) (mirror := probe.facing > 0)
  discard <| originalSprite "Probe" (x - 24) (y - 24) 48 48
    (rotation := rotation) (mirror := probe.facing > 0) (additive := true)

private def drawPrimeArm (arm : PrimeArm) (head : PrimeHead) (camera : Camera) (light : LightField) : IO Unit := do
  let side := (Int32.ofInt arm.kind.side).toFloat32
  let mut armX := arm.centerX - 5 * side
  let mut armY := arm.y + 20
  for index in [:2] do
    let dx := head.centerX - armX - (if index == 0 then 200 else 50) * side
    let dy := head.centerY - armY + if index == 0 then 130 else 80
    let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
    if length == 0 then continue
    let scale := (if index == 0 then 92 else 60) / length
    armX := armX + dx * scale
    armY := armY + dy * scale
    let x := npcScreenPixel armX camera.x
    let y := npcScreenPixel armY camera.y
    let rotation := (Float.atan2 dy.toFloat dx.toFloat - 1.57) * 57.29577951308232
    discard <| originalSprite "Arm_Bone_2" (x - 19) (y - 73) 39 147
      (color := npcLightTint light armX armY) (rotation := rotation)
    if index == 0 then
      armX := armX + dx * scale / 2
      armY := armY + dy * scale / 2
  let (sourceWidth, sourceHeight) : Nat × Nat := match arm.kind with
    | .cannon => (38, 76)
    | .saw => (38, 78)
    | .vice => (40, 70)
    | .laser => (40, 62)
  let w := sourceWidth * 3 / 2
  let h := sourceHeight * 3 / 2
  let x := npcScreenPixel arm.centerX camera.x - w / 2
  let y := npcScreenPixel (arm.y + 56 - (Int32.ofNat sourceHeight).toFloat32) camera.y
  let rotation := arm.rotation.toFloat * 57.29577951308232
  discard <| originalSprite s!"NPC_{arm.kind.id}" x y w h 0 (arm.frame * sourceHeight) sourceWidth sourceHeight
    (color := npcLightTint light arm.centerX arm.centerY) (rotation := rotation) (mirror := arm.facing > 0)
  if arm.kind == .laser then
    discard <| originalSprite "Bone_Laser" x y w h
      (color := 0xffffffc8) (rotation := rotation) (mirror := arm.facing > 0) (additive := true)

private def drawSkeletronPrime (head : PrimeHead) (arms : Array PrimeArm) (camera : Camera)
    (light : LightField) : IO Unit := do
  for arm in arms do drawPrimeArm arm head camera light
  let x := npcScreenPixel head.centerX camera.x - 105
  let y := npcScreenPixel (head.y - 50) camera.y
  let rotation := head.rotation.toFloat * 57.29577951308232
  let frame := head.frame * 156
  discard <| originalSprite "NPC_127" x y 210 234 0 frame 140 156
    (color := npcLightTint light head.centerX head.centerY) (rotation := rotation)
  discard <| originalSprite "Bone_Eyes" x y 210 234 0 frame 140 156
    (color := 0xffffffc8) (rotation := rotation) (additive := true)

private def drawPrimeShot (shot : PrimeShot) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.centerX camera.x
  let y := npcScreenPixel shot.centerY camera.y
  if x + 83 < 0 || width + 83 ≤ x || y + 83 < 0 || height + 83 ≤ y then return
  let rotation := shot.rotation.toFloat * 57.29577951308232
  if shot.kind == .bomb then
    discard <| originalSprite "Projectile_102" (x - 16) (y - 18) 33 36 0 (shot.frame * 24) 22 24
      (rotation := rotation)
  else if shot.alpha < 200 then
    discard <| originalSprite "Projectile_100" (x - 2) (y - 81) 5 162
      (color := spriteAlpha 0xffffffff (255 - shot.alpha)) (rotation := rotation) (additive := true)

private def drawPlanteraChain (asset : String) (sourceWidth : Nat) (x y : Float32) (head : PlanteraHead)
    (camera : Camera) (light : LightField) : IO Unit := do
  let deltaX := head.centerX - x
  let deltaY := head.centerY - y
  let length := (deltaX * deltaX + deltaY * deltaY).toFloat.sqrt.toFloat32
  if length < 17 || length > 2000 then return
  let screenWidth := sourceWidth * 3 / 2
  let rotation := (Float.atan2 deltaY.toFloat deltaX.toFloat - 1.57) * 57.29577951308232
  let mut traveled : Nat := 0
  for _ in [:length.toInt32.toInt.toNat / 16 + 1] do
    let remaining := length.toInt32.toInt.toNat - traveled
    let piece := min 16 (remaining - 16)
    if piece == 0 then break
    traveled := traveled + piece
    let part := (Int32.ofNat traveled).toFloat32 / length
    let px := x + deltaX * part
    let py := y + deltaY * part
    let left := npcScreenPixel px camera.x - screenWidth / 2
    let top := npcScreenPixel py camera.y - 12
    discard <| originalSprite asset left top screenWidth (piece * 3 / 2) 0 0 sourceWidth piece
      (rotation := rotation) (color := npcLightTint light px py)

private def drawPlantera (head : PlanteraHead) (hooks : Array PlanteraHook) (tentacles : Array PlanteraTentacle)
    (camera : Camera) (light : LightField) : IO Unit := do
  for hook in hooks do
    drawPlanteraChain "Chain26" 24 hook.centerX hook.centerY head camera light
    let x := npcScreenPixel hook.centerX camera.x - 42
    let y := npcScreenPixel (hook.y - 6) camera.y
    discard <| originalSprite "NPC_263" x y 84 75 0 (hook.frame * 50) 56 50
      (color := npcLightTint light hook.centerX hook.centerY)
      (rotation := hook.rotation.toFloat * 57.29577951308232)
  for part in tentacles do
    drawPlanteraChain "Chain27" 16 part.centerX part.centerY head camera light
    let x := npcScreenPixel part.centerX camera.x - 22
    let y := npcScreenPixel (part.y - 13) camera.y
    discard <| originalSprite "NPC_264" x y 44 62 0 (part.frame * 41) 29 41
      (color := npcLightTint light part.centerX part.centerY)
      (rotation := part.rotation.toFloat * 57.29577951308232) (mirror := part.facing > 0)
  let x := npcScreenPixel head.centerX camera.x - 87
  let y := npcScreenPixel (head.y - 38) camera.y
  discard <| originalSprite "NPC_262" x y 174 231 0 (head.frame * 154) 116 154
    (color := npcLightTint light head.centerX head.centerY)
    (rotation := head.rotation.toFloat * 57.29577951308232)

private def drawPlanteraSpore (spore : PlanteraSpore) (camera : Camera) (light : LightField) : IO Unit := do
  let x := npcScreenPixel spore.centerX camera.x - 13
  let y := npcScreenPixel (spore.y - 4) camera.y
  discard <| originalSprite "NPC_265" x y 27 39
    (color := npcLightTint light spore.centerX spore.centerY)
    (rotation := spore.rotation.toFloat * 57.29577951308232)

private def drawPlanteraShot (shot : PlanteraShot) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.centerX camera.x
  let y := npcScreenPixel shot.centerY camera.y
  if x + 30 < 0 || x ≥ width + 30 || y + 30 < 0 || y ≥ height + 30 then return
  let rotation := shot.rotation.toFloat * 57.29577951308232
  let color := spriteAlpha 0xffffffff (255 - shot.alpha)
  if shot.kind == .thornBall then
    discard <| originalSprite "Projectile_277" (x - 28) (y - 28) 57 57 (color := color) (rotation := rotation)
  else
    discard <| originalSprite s!"Projectile_{shot.kind.sprite}" (x - 10) (y - 18) 21 36 0 (shot.frame * 24) 14 24
      (color := color) (rotation := rotation)

private def drawGolem (body : GolemBody) (head : Option GolemHead) (fists : Array GolemFist)
    (camera : Camera) (light : LightField) : IO Unit := do
  for fist in fists do
    for (x, y, rotation) in fist.chainLinks body.centerX body.centerY do
      discard <| originalSprite "Chain21" (npcScreenPixel x camera.x - 12)
        (npcScreenPixel y camera.y - 12) 24 24
        (color := spriteAlpha (npcLightTint light x y) (255 - fist.alpha))
        (rotation := rotation.toFloat * 57.29577951308232)
  let x := npcScreenPixel (body.centerX - 93) camera.x
  let y := npcScreenPixel (body.bottomY - 170) camera.y
  let color := spriteAlpha (npcLightTint light body.centerX body.centerY) (255 - body.alpha)
  discard <| originalSprite "NPC_245" x y 279 258 0 body.frameY 186 172 (color := color)
  if body.alpha == 0 then
    discard <| originalSprite "GolemLights3" x y 279 258 0 body.frameY 186 172 (additive := true)
  for fist in fists do
    let left := npcScreenPixel (fist.centerX - 25) camera.x
    let top := npcScreenPixel (fist.centerY - 21) camera.y
    let color := spriteAlpha (npcLightTint light fist.centerX fist.centerY) (255 - fist.alpha)
    let rotation := fist.rotation.toFloat * 57.29577951308232
    let asset := s!"NPC_{fist.kind.id}"
    discard <| originalSprite asset left top 75 60 (color := color) (rotation := rotation)
    if fist.phase == 1 then
      let pulse (offset : Nat) :=
        if fist.timer < offset || fist.timer ≥ offset + 15 then 0
        else 180 - (fist.timer - offset) * 12
      for offset in #[0, 5, 10] do
        let intensity := pulse offset
        if intensity > 0 then
          discard <| originalSprite asset left top 75 60
            (color := spriteAlpha 0xffa050ff intensity) (rotation := rotation) (additive := true)
  if let some head := head then
    let stride := head.frameHeight
    let left := npcScreenPixel (head.centerX - 56) camera.x
    let top := npcScreenPixel (head.y + 74 - (Int32.ofNat stride).toFloat32) camera.y
    let frame := head.frame * stride
    let color := spriteAlpha (npcLightTint light head.centerX head.centerY) (255 - head.alpha)
    let turn := head.rotation.toFloat * 57.29577951308232
    discard <| originalSprite s!"NPC_{head.sprite}" left top 168 (stride * 3 / 2) 0 frame 112 stride
      (color := color) (rotation := turn)
    if head.alpha < 255 then
      let glowX := if head.detached || head.frame < 2 then head.centerX - 20
        else if head.look > 0 then head.centerX + 26 else head.centerX - 38
      let glowY := head.centerY - if head.detached then 47 else 27
      let tint := spriteAlpha 0xffffffff (255 - head.alpha)
      if head.detached || head.frame < 2 then
        discard <| originalSprite "GolemLights1" (npcScreenPixel glowX camera.x)
          (npcScreenPixel glowY camera.y) 57 15 0 (if head.detached then head.frame * 10 else 0) 38 10
          (color := tint) (additive := true)
      else
        discard <| originalSprite "GolemLights2" (npcScreenPixel glowX camera.x)
          (npcScreenPixel glowY camera.y) 18 21 0 (if head.look > 0 then 0 else 28) 12 14
          (color := tint) (additive := true)

private def drawGolemShot (shot : GolemShot) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.centerX camera.x
  let y := npcScreenPixel shot.centerY camera.y
  if x + 50 < 0 || x ≥ width + 50 || y + 50 < 0 || y ≥ height + 50 then return
  let turn := shot.rotation.toFloat * 57.29577951308232
  let color := spriteAlpha 0xffffffff (255 - shot.alpha)
  if shot.kind == .fireball then
    discard <| originalSprite "Projectile_258" (x - 12) (y - 12) 24 24 (color := color) (rotation := turn)
  else
    discard <| originalSprite "Projectile_259" (x - 7) (y - 40) 13 79 (color := color) (rotation := turn)

private def drawTempleTrapShot (shot : TempleTraps.Shot) (camera : Camera) : IO Unit := do
  if shot.kind == .flameEmitter then return
  let x := npcScreenPixel shot.center.x camera.x
  let y := npcScreenPixel shot.center.y camera.y
  if x + 32 < 0 || x ≥ width + 32 || y + 32 < 0 || y ≥ height + 32 then return
  if shot.kind == .flame then
    for i in [:4] do
      let offset := (Int32.ofNat i).toFloat32
      let px := npcScreenPixel (shot.center.x - offset * shot.velocity.x) camera.x - 6
      let py := npcScreenPixel (shot.center.y - offset * shot.velocity.y) camera.y - 6
      discard <| originalSprite "Dust" px py 12 12 60 (10 * ((shot.age + i) % 3)) 8 8
        (color := spriteAlpha 0xffbd7aff (220 - 40 * i))
  else
    let (w, h) := match shot.kind with
      | .dart => (15, 30)
      | .spikyBall => (30, 30)
      | .spear => (22, 54)
      | _ => (12, 12)
    discard <| originalSprite s!"Projectile_{shot.kind.sprite}" (x - w / 2) (y - h / 2) w h
      (rotation := shot.rotation.toFloat * 57.29577951308232)

private def drawBrain (boss : BrainOfCthulhu) (camera : Camera) (clock : Nat) : IO Unit := do
  let (x, y) := eyeScreen boss.motion camera
  let drawCreeper (creeper : BrainCreeper) : IO Unit := do
    let (cx, cy) := eyeScreen creeper.motion camera
    if cx + 32 ≥ 0 && cx - 32 < width && cy + 22 ≥ 0 && cy - 22 < height then
      discard <| originalSprite "NPC_267" (cx - 24) (cy - 24) 48 48
          (rotation := spriteHeading creeper.motion.vx creeper.motion.vy)
  for creeper in boss.creepers do
    if creeper.motion.y < boss.motion.y then drawCreeper creeper
  let opacity := boss.visibleAlpha
  if opacity > 0 && x + 150 ≥ 0 && x - 150 < width && y + 140 ≥ 0 && y - 140 < height then
    let exposed := boss.phase == .exposed
    let frame := (if exposed then 4 else 0) + clock / 6 % 4
    discard <| originalSprite "NPC_266" (x - 150) (y - 110) 300 273 0 (frame * 182) 200 182
        (color := spriteAlpha 0xffffffff opacity)
  for creeper in boss.creepers do
    if boss.motion.y ≤ creeper.motion.y then drawCreeper creeper

private def drawEvilEnemy (enemy : EvilFlyer) (camera : Camera) (clock : Nat) : IO Unit := do
  let (cx, cy) := eyeScreen enemy.motion camera
  if cx + 24 < 0 || cx - 24 ≥ width || cy + 30 < 0 || cy - 30 ≥ height then return
  let crimson := enemy.kind == .crimera
  let asset := if crimson then "NPC_173" else "NPC_6"
  discard <| originalSprite asset (cx - 26) (cy - 25) 52 50 0 (clock / 5 % 2 * 80) 42 80
      (rotation := spriteHeading enemy.motion.vx enemy.motion.vy - 90)
  if enemy.health < enemy.kind.maxHealth then
    rect (cx - 11) (cy + 17) 22 3 0x30263aff
    rect (cx - 10) (cy + 18) (20 * enemy.health / enemy.kind.maxHealth) 1 (relicColor crimson)

private def drawEyeServant (servant : EyeServant) (camera : Camera) : IO Unit := do
  let (x, y) := eyeScreen servant.motion camera
  if x + 22 < 0 || x - 22 ≥ width || y + 18 < 0 || y - 18 ≥ height then return
  discard <| originalSprite "NPC_5" (x - 15) (y - 19) 30 45 0 0 20 32
      (rotation := spriteHeading servant.motion.vx servant.motion.vy + 90)

private def drawQueenSlime (queen : QueenSlime) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel queen.centerX camera.x
  let y := npcScreenPixel queen.bottom camera.y + 3
  let rotation := queen.rotation.toFloat * 57.29577951308232
  let width := (270 * queen.scale).toUInt32.toNat
  let height := (177 * queen.scale).toUInt32.toNat
  if queen.flying then
    let centerY := npcScreenPixel queen.centerY camera.y
    let wingY := (queen.wingClock / 6) * 290
    let tilt := (max (-6) (min 6 queen.vy)).toFloat * 5.729577951308232
    discard <| originalSprite "Extra_185" (x - 207) (centerY - 174) 211 348
      0 wingY 176 290 (rotation := rotation + tilt)
    discard <| originalSprite "Extra_185" (x - 2) (centerY - 174) 211 348
      0 wingY 176 290 (mirror := true) (rotation := rotation - tilt)
  let frame : Nat := if queen.phase == .smash then
      if queen.sequence == 1 then 10 else 8
    else if queen.phase == .gel then if queen.sequence == 1 then 6 else 4
    else if queen.flying then 12 + queen.wingClock / 6
    else if queen.vy > 0 then 2 else if queen.vy < 0 then 1 else 0
  discard <| originalSprite "NPC_657" (x - (width / 2 : Nat)) (y - height) width height
    0 (frame * 122 + 2) 180 118 (rotation := rotation) (queenSlime := true)
  let crownY := y - (if frame == 8 || frame ≥ 11 then height * 2 / 3 else height * 4 / 5)
  let crownWidth := (51 * queen.scale).toUInt32.toNat
  let crownHeight := (66 * queen.scale).toUInt32.toNat
  discard <| originalSprite "Extra_186" (x - (crownWidth / 2 : Nat)) (crownY - crownHeight) crownWidth crownHeight
    (rotation := rotation)

private def drawQueenMinion (slime : QueenMinion) (camera : Camera) : IO Unit := do
  let flying := slime.kind == .heavenly
  let w := if flying then 90 else 48
  let h := if flying then 42 else 32
  let x := npcScreenPixel slime.centerX camera.x
  let y := npcScreenPixel slime.centerY camera.y
  let frame := slime.frameClock / 8 % (if flying then 4 else 2)
  discard <| originalSprite s!"NPC_{slime.kind.sprite}" (x - (w * 3 / 4 : Nat))
    (y - (h * 3 / 4 : Nat)) (w * 3 / 2) (h * 3 / 2) 0 (h * frame) w h
    (mirror := slime.direction > 0)

private def drawQueenShot (shot : QueenShot) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel shot.x camera.x
  let y := npcScreenPixel shot.y camera.y
  if shot.kind == .stomp then
    for i in [:25] do
      let angle := i.toFloat * 0.25132741228718347
      let radius := (shot.diameter.toFloat / 2) * 1.5
      let dx := (angle.cos * radius).toInt32.toInt
      let dy := (angle.sin * radius / 3).toInt32.toInt
      discard <| originalSprite "Dust" (x + dx - 5) (y + dy - 5) 10 10 40 (shot.age % 3 * 10) 8 8
        (color := 0xe8b6ffcf)
  else if shot.kind == .spike then
    let angle := Float.atan2 (shot.vy.toFloat) (shot.vx.toFloat) * 57.29577951308232 + 90
    discard <| originalSprite "Projectile_920" (x - 7) (y - 12) 15 24
      0 ((shot.age / 4 % 6) * 11) 10 10 (rotation := angle)
  else
    discard <| originalSprite (if shot.kind == .regal then "Projectile_926" else "Projectile_921")
      (x - 11) (y - 11) 22 22 (fit := true)

private def drawTwin (twin : Twin) (camera : Camera) : IO Unit := do
  let x := npcScreenPixel twin.centerX camera.x
  let y := npcScreenPixel (twin.centerY - 15) camera.y
  if x + 190 < 0 || width + 190 ≤ x || y + 190 < 0 || height + 190 ≤ y then return
  let rotation := twin.rotation.toFloat * 57.29577951308232
  let sourceY := twin.frame * 200
  let asset := s!"NPC_{twin.kind.sprite}"
  discard <| originalSprite asset (x - 82) (y - 150) 165 300 0 sourceY 110 198 (rotation := rotation)
  if twin.kind == .retinazer && twin.frame ≥ 3 then
    let _ ← originalSprite "Eye_Laser" (x - 82) (y - 150) 165 300 0 sourceY 110 198
      (rotation := rotation) (additive := true)

private def drawEyeBoss (boss : EyeOfCthulhu) (state : GameState) (camera : Camera) : IO Unit := do
  for servant in boss.servants do drawEyeServant servant camera
  let (x, y) := eyeScreen boss.motion camera
  if x + 142 < 0 || x - 142 ≥ width || y + 122 < 0 || y - 122 ≥ height then return
  let jaws := boss.phase.secondForm || boss.phase == .transformSpinDown ||
    (boss.phase == .retreat && boss.health ≤ eyeMaxHealth / 2)
  let dx := bodyCenterX state.player - boss.motion.x
  let dy := bodyCenterY state.player - boss.motion.y
  let frame := (if jaws then 3 else 0) + state.surface.clock / 6 % 3
  let rotation := if boss.phase.transforming then (boss.ticks * 22).toFloat else spriteHeading dx dy - 90
  discard <| originalSprite "NPC_4" (x - 83) (y - 100) 165 249 0 (frame * 166) 110 166 (rotation := rotation)

private def drawSurfaceCreature (creature : NightCreature) (camera : Camera) (clock : Nat) : IO Unit := do
  let (x, y) := bodyOrigin creature.body camera
  if x + 30 < 0 || x - 7 ≥ width || y + 33 < 0 || y ≥ height then return
  match creature.kind with
  | .cedarHare => drawCedarHare x y (creature.body.vx < 0) creature.body.grounded
  | .brambleCrawler => drawBrambleCrawler x y creature clock
  | .zombie =>
    discard <| originalSprite "NPC_3" (x - 7) (y - 9) 32 42 0 (clock / 7 % 3 * 48) 38 48
      (mirror := creature.body.vx > 0)
    drawSurfaceHealth creature x (y - 7) 0xb6d398ff
  | .demonEye =>
    discard <| originalSprite "NPC_2" (x - 9) (y + 5) 36 24 0 (clock / 7 % 2 * 24) 38 24
      (mirror := creature.body.vx > 0)
    drawSurfaceHealth creature x (y + 1) 0xe7909eff
  | _ =>
    let tint : UInt32 := if creature.kind == .greenSlime then 0xa5ffa8ff
      else if creature.kind == .slime then 0xd3a9eeff else 0xffffffff
    discard <| originalSprite "NPC_1" (x - 5) (y + 13) 27 21 0 (clock / 6 % 2 * 26) 32 26 (color := tint)
    drawSurfaceHealth creature x (y + 7) tint

private def drawQuarryCrawler (creature : QuarryCrawler) (camera : Camera) : IO Unit := do
  let x := creature.body.x * tilePixels / tileUnits - camera.x
  let y := creature.body.y * tilePixels / tileUnits - camera.y
  discard <| originalSprite "NPC_31" x y 32 46 0 0 40 56

private def residentColor : CaveMobKind → UInt32
  | .slime => 0xa2e9a4ff
  | .beetle => 0xb5def9ff
  | .skitter => 0xf3ba86ff
  | .oreback => deepsteelColor

private def drawCaveResident (mob : CaveResident) (camera : Camera) : IO Unit := do
  let (x, y) := bodyOrigin mob.body camera
  if x + 32 < 0 || x ≥ width || y + 35 < 0 || y ≥ height then return
  match mob.kind with
  | .slime => discard <| originalSprite "NPC_1" x y 29 23 0 0 32 26
  | .beetle => discard <| originalSprite "NPC_218" x y 31 23 0 0 34 22
  | .skitter => discard <| originalSprite "NPC_164" x y 42 24 0 0 70 30
  | .oreback => discard <| originalSprite "NPC_110" x y 39 41 0 0 60 60

private def drawSentry (sentry : Sentry) (_state : GameState) (camera : Camera) : IO Unit := do
  let x := sentry.body.x * tilePixels / tileUnits - camera.x
  let y := sentry.body.y * tilePixels / tileUnits - camera.y
  discard <| originalSprite "NPC_110" x y 46 50 0 0 60 60

private def drawRaider (foe : Raider) (_state : GameState) (camera : Camera) : IO Unit := do
  let x := foe.body.x * tilePixels / tileUnits - camera.x
  let y := foe.body.y * tilePixels / tileUnits - camera.y
  discard <| originalSprite "NPC_120" x y 32 45 0 0 38 48

private def drawProjectile (shot : Projectile) (camera : Camera) : IO Unit := do
  let x := shot.x * tilePixels / tileUnits - camera.x
  let y := shot.y * tilePixels / tileUnits - camera.y
  if x + 25 < 0 || x - 25 ≥ width || y + 25 < 0 || y - 25 ≥ height then return
  let degrees := Float.atan2 shot.vy.toFloat shot.vx.toFloat * 57.29577951308232 + 90
  let asset := match shot.kind with
    | .hellfireArrow => "Projectile_41"
    | .unholyArrow => "Projectile_4"
    | .thrownBone => "Projectile_21"
    | .musketBullet | .undertakerBullet | .handgunBullet => "Projectile_14"
    | .cavernBeam | .raidBolt => "Projectile_83"
    | .sentry | .herald => "Projectile_15"
    | .ruinWave => "Projectile_85"
    | _ => "Projectile_1"
  let spin := (shot.kind.lifetime - shot.life).toFloat * (if shot.vx < 0 then -13.751 else 13.751)
  discard <| originalSprite asset (x - 10) (y - 18) 20 36 (fit := true)
    (rotation := if shot.kind == .thrownBone then spin else degrees)

private def drawActiveBomb (bomb : ActiveBomb) (camera : Camera) : IO Unit := do
  let x := bomb.x * tilePixels / tileUnits - camera.x
  let y := bomb.y * tilePixels / tileUnits - camera.y
  if x + 20 < 0 || width ≤ x - 20 || y + 30 < 0 || height ≤ y - 30 then return
  let warning := bomb.fuse ≤ 60
  let spark := if warning && bomb.fuse / 4 % 2 == 0 then 0xff6659ff else 0xffd581ff
  if bomb.dynamite then
    discard <| originalSprite "Projectile_29" (x - 12) (y - 15) 24 29 (fit := true)
    drawWeaponDust 6 8 bomb.fuse 220 (x - 2) (y - 19)
  else drawBombSprite (x - 12) (y - 14) bomb.sticky spark
  rect (x - 10) (y - 22) 20 3 0x302637ff
  rect (x - 10) (y - 22) (max 1 (20 * bomb.fuse / bomb.fuseLimit)) 3 spark

private def drawBombFlash (position : TilePos) (ticks : Nat) (camera : Camera) : IO Unit := do
  let x := position.x * tilePixels + 12 - camera.x
  let y := position.y * tilePixels + 12 - camera.y
  let spread := (13 - ticks) * 4
  for (dx, dy) in [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)] do
    drawWeaponDust 6 (7 + ticks / 2) ticks 185 (x + dx * spread) (y + dy * spread)

private def drawGuideSprite (x y : Int) (direction : Int) (walking : Bool) : IO Unit :=
  discard <| originalSprite "NPC_22" (x - 7) (y - 9) 33 46
    0 (if walking then 56 * 6 else 0) 40 56 (mirror := direction > 0)

private def drawGuide (x y : Int) (direction : Int) (walking nearby : Bool)
    (health := guideMaxHealth) (firing := false) : IO Unit := do
  if x + 19 ≥ 0 && x < width && y + 33 ≥ 0 && y < height then
    discard <| originalSprite "NPC_22" (x - 7) (y - 10) 32 46 0
        (if firing then 56 * 21 else if walking then 56 * 6 else 0) 40 56
        (mirror := direction > 0)
    if firing then drawBow (x + if direction < 0 then -16 else 12) (y + 10) .bow
    rect (x - 25) (y - 22) 68 17 0x192b2df2
    text (if nearby then "Q  GUIDE" else "GUIDE") (x - 18) (y - 19) 12 mint
    if health < guideMaxHealth then
      rect (x - 18) (y - 8) 54 2 0x42505dff
      rect (x - 18) (y - 8) (54 * health / guideMaxHealth) 2 mint

private def drawOldMan (state : GameState) (position : TilePos) (camera : Camera) : IO Unit := do
  let (x, y) := npcOrigin position camera
  if x + 25 < 0 || x - 7 ≥ width || y + 36 < 0 || y - 10 ≥ height then return
  let facesRight := state.player.centerTile.x > position.x
  discard <| originalSprite "NPC_37" (x - 7) (y - 10) 32 46 0 0 40 56 (mirror := facesRight)
  if !state.openOldMan && state.canReach position then
    let _ ← originalSprite "Chat" (x - 5) (y - 27) 21 17
    text "Q" (x + 17) (y - 25) 13 paper

private def drawMerchant (position : TilePos) (camera : Camera) (nearby : Bool) : IO Unit := do
  let (x, y) := npcOrigin position camera
  if x + 28 ≥ 0 && x < width && y + 39 ≥ 0 && y < height then
    discard <| originalSprite "NPC_17" (x - 7) (y - 10) 32 46 0 0 40 56
    if nearby then text "Q  MERCHANT" (x - 29) (y - 19) 12 gold

private def drawWarden (boss : Warden) (camera : Camera) : IO Unit := do
  let x := (boss.body.x + playerWidth / 2) * tilePixels / tileUnits - camera.x
  let y := boss.body.y * tilePixels / tileUnits - camera.y
  discard <| originalSprite "NPC_110" (x - 48) (y - 60) 96 105 0 0 60 60
    (color := if boss.phase == .windup then 0xafdcffff else 0xffffffff)

private def drawStormHerald (boss : StormHerald) (_state : GameState) (camera : Camera) : IO Unit := do
  let x := (boss.body.x + playerWidth / 2) * tilePixels / tileUnits - camera.x
  let y := boss.body.y * tilePixels / tileUnits - camera.y
  discard <| originalSprite "NPC_48" (x - 44) (y - 35) 88 78 0 0 100 86
    (color := if boss.phase == .aim then 0xffd99eff else 0xffffffff)

private def drawRuinImpact (boss : RuinSentinel) (camera : Camera) : IO Unit := do
  let x := boss.targetX * tilePixels / tileUnits - camera.x
  let floorY := (ruinArenaFloor : Int) * tilePixels - camera.y
  if boss.phase == .windup then
    let edge := if boss.targetLocked then paper else ruinColor
    rect (x - 14) (floorY - 25) 2 22 edge
    rect (x + 12) (floorY - 25) 2 22 edge
    rect (x - 14) (floorY - 4) 28 3 edge
    for offset in [:5] do
      rect (x - 5 + offset) (floorY - 36 + offset) 2 2 edge
      rect (x + 4 - offset) (floorY - 36 + offset) 2 2 edge
    if boss.targetLocked then
      for col in [ruinArenaStart:ruinArenaEnd + 1] do
        rect ((col : Int) * tilePixels - camera.x + 4) (floorY - 2) 14 1 ruinColor
  else if boss.phase == .slam then
    let elapsed := boss.phaseDuration - boss.ticks
    let spread := min 32 (12 + 2 * elapsed)
    for offset in [:9] do
      let dx := (spread : Int) * ((offset : Int) - 4) / 4
      let y := floorY - 16 + (offset % 4) * 3
      drawWeaponDust 6 (11 - offset / 2) (elapsed + offset) 210 (x + dx) y

private def drawRuinSentinel (boss : RuinSentinel) (camera : Camera) : IO Unit := do
  drawRuinImpact boss camera
  let x := bodyCenterX boss.body * tilePixels / tileUnits - camera.x
  let feet := (boss.body.y + playerHeight) * tilePixels / tileUnits - camera.y
  let y := feet - 38
  discard <| originalSprite "NPC_216" (x - 39) (y - 48) 78 90 0 0 60 60

private def cavernHighStage (boss : DeepstoneColossus) : Bool :=
  boss.phase == .beam && boss.phaseDuration - boss.ticks ≥ cavernHighBeamDelay * 2 / 3

private def drawCavernTell (boss : DeepstoneColossus) (world : World) (camera : Camera) : IO Unit := do
  let high := cavernHighStage boss
  let warning := boss.phase == .windup || (high && boss.phaseDuration - boss.ticks ≤ cavernHighBeamDelay)
  if !warning then return
  let beamY := if high then cavernHighBeamY else cavernLowBeamY
  let color := if high then cavernHighColor else cavernLowColor
  let origin := bodyCenterX boss.body
  if (world.tile ⟨origin / tileUnits, beamY / tileUnits⟩).solid then return
  let edge (col : Nat) : Int :=
    let endX := (col : Int) * tileUnits
    let stop := (terrainHit world origin beamY (endX - origin) 0).map (·.x)
    (stop.getD endX) * tilePixels / tileUnits - camera.x
  let left := edge cavernArenaStart
  let right := edge (cavernArenaEnd + 1)
  let y := beamY * tilePixels / tileUnits - camera.y
  let start := max cavernArenaStart (camera.x / tilePixels).toNat
  let stop := min (cavernArenaEnd + 1) ((camera.x + width + tilePixels - 1) / tilePixels).toNat
  let flash := boss.ticks % 12 < 6
  for col in [start:stop] do
    let x := (col : Int) * tilePixels - camera.x
    let segmentLeft := max (x + 2) left
    let segmentRight := min (x + 13) right
    if segmentLeft < segmentRight then
      rect segmentLeft y (segmentRight - segmentLeft).toNat (if flash then 2 else 1) color
    if col % 3 == 0 && left ≤ x + 7 && x + 17 ≤ right then
      -- Up chevrons mark the jump. Down chevrons mark the beam to stay beneath.
      for offset in [:4] do
        let dy : Int := if high then (offset : Int) - 11 else 5 - (offset : Int)
        rect (x + 7 + offset) (y + dy) 2 2 color
        rect (x + 15 - offset) (y + dy) 2 2 color

private def drawDeepstoneColossus (boss : DeepstoneColossus) (camera : Camera) : IO Unit := do
  let x := bodyCenterX boss.body * tilePixels / tileUnits - camera.x
  let floorY := (boss.body.y + playerHeight) * tilePixels / tileUnits - camera.y
  let y := floorY - 40
  discard <| originalSprite "NPC_494" (x - 51) (y - 28) 102 60 0 0 62 32

private def outline (x y : Int) (size : Nat) (color : UInt32) : IO Unit := do
  rect x y size 2 color
  rect x (y + size - 2) size 2 color
  rect x y 2 size color
  rect (x + size - 2) y 2 size color

private def drawCrossbowAim (state : GameState) (camera : Camera) (target : TilePos) : IO Unit := do
  let x := target.x * tilePixels + 12 - camera.x
  let y := target.y * tilePixels + 12 - camera.y
  let loaded := Inventory.count state.inventory .arrow > 0
  let color := if !loaded then muted else if state.bowCooldown > 0 then gold else deepsteelColor
  rect (x - 8) (y - 6) 2 12 color
  rect (x + 7) (y - 6) 2 12 color
  rect (x - 5) y 3 2 color
  rect (x + 3) y 3 2 color
  if !loaded || state.bowCooldown > 0 || target == state.player.centerTile then return
  let startX := bodyCenterX state.player
  let startY := bodyCenterY state.player
  let dx := target.x * tileUnits + tileUnits / 2 - startX
  let dy := target.y * tileUnits + tileUnits / 2 - startY
  let some (vx, vy) := aimVelocity ShotKind.highlandBolt.speed dx dy | return
  let hit := terrainHit state.world startX startY (vx * 4) (vy * 4)
  let spanX := (hit.map (·.x - startX)).getD (vx * 4)
  let spanY := (hit.map (·.y - startY)).getD (vy * 4)
  let steps := max spanX.natAbs spanY.natAbs * tilePixels / tileUnits.toNat / 8
  for index in [2:steps] do
    let x := (startX + spanX * index / steps) * tilePixels / tileUnits - camera.x
    let y := (startY + spanY * index / steps) * tilePixels / tileUnits - camera.y
    rect x y 2 2 0xb9eeed86

private def drawCursor (state : GameState) (camera : Camera) (position : TilePos) : IO Unit := do
  if !state.world.contains position then return
  let cart := (state.cartIndex position).bind (fun index => state.carts[index]?)
  let spot := if state.legacyEarly && cart.isNone && state.selectedStack.any (·.item == .fishingRod) &&
      state.fishing == .idle then
      legacyFishingSpot state.world state.water state.player position
    else none
  let position := spot.getD position
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  let nearby := state.canReach position
  let tile := state.world.tile position
  let shadowChest := if nearby && tile.isShadowChest then state.shadowChestAtCell position else none
  let harvesting := state.selectedStack.any (fun stack =>
    (stack.item.pickPower > 0 || stack.item.axePower > 0) && state.canReachWithTool position stack.item)
  let casting := spot.isSome
  let hook := state.selectedStack.any (fun stack => stack.item.gearSlot == some .hook)
  let crossbow := state.selectedStack.any (·.item == .highlandCrossbow)
  let crystal := nearby && tile == .lifeCrystal
  let cavernCrystal := nearby && (tile == .crystalShard || tile == .gelatinCrystal)
  let berry := nearby && tile == .highlandBerry
  let hasPick := state.selectedStack.any (·.item.pickPower > 0)
  let hasAxe := state.selectedStack.any (·.item.axePower > 0)
  let steel := nearby && tile == .deepsteel
  let hardmodeOre := hardmodeOreStyle tile
  let altar := nearby && (tile == .demonAltar || tile == .crimsonAltar)
  let bomb := nearby && state.bombs.any (·.position == position)
  let relic := if nearby && (tile == .shadowOrb || tile == .crimsonHeart) then
      (evilRelics state.seed).find? fun anchor => (evilRelicCells anchor).contains position
    else none
  let pot := nearby && tile == .clayPot
  let hammer := state.selectedStack.any (·.item == .woodHammer)
  let strongPick := state.selectedStack.any fun stack =>
    if state.legacyEarly then stack.item.legacyPickRank ≥ 3
    else stack.item.pickPower ≥ tile.minimumPickPower
  let mouth := deepsteelMouth state.seed
  let nextCap : TilePos := ⟨mouth.x + 1, groundHeight state.seed (mouth.x + 1).toNat⟩
  let mineCap := nearby && tile == .stone && (position == mouth || position == nextCap) &&
    state.world.tile (deepsteelMarker state.seed) == .mineMarker
  let descent := cavernEntrance state.seed
  let cavernCap := nearby && tile == .stone && position.y == descent.y &&
    (position.x == descent.x || position.x == descent.x + 1) &&
    state.world.tile cavernAltar == .cavernAltar
  let summonColossus := nearby && tile == .cavernAltar && state.cavern == .ready
  let gallery := state.world.tile magmaMarker == .magmaMarker
  let magmaCap := nearby && gallery && tile == .stone && magmaEntranceCaps.contains position
  let waterGate := nearby && gallery && tile == .stone && position == magmaWaterGate
  let obsidian := nearby && tile == .obsidian
  let bed := if tile == .bed then state.furnitureAt position else none
  let bedFoot := nearby && bed.any (·.isBedFoot position)
  let woodWall := nearby && hasWoodWall state.world state.walls position &&
    state.selectedStack.any (·.item == .woodHammer)
  let planting := nearby && state.selectedStack.any (·.item == .sunleafSeed) &&
    canPlantSunleaf state.world state.water position
  let spreading := nearby && tile == .dirt && !(state.world.tile { position with y := position.y - 1 }).solid &&
    state.selectedStack.any (fun stack => stack.item == .corruptSeeds || stack.item == .crimsonSeeds)
  let boarding := nearby && cart.any state.canBoardCart
  let layRail := nearby && tile == .air && state.selectedStack.any (·.item == .rail)
  let setCart := nearby && cart.isNone && tile == .rail && canParkCart state.world position &&
    state.selectedStack.any (·.item == .minecart)
  if bomb then
    outline x y 24 gold
  else if let some anchor := relic then
    outline (anchor.x * tilePixels - camera.x) (anchor.y * tilePixels - camera.y) 48
      (relicColor (tile == .crimsonHeart))
  else if let some anchor := shadowChest then
    outline (anchor.x * tilePixels - camera.x) (anchor.y * tilePixels - camera.y) 48 shadowChestColor
  else if cart.isSome || layRail || setCart then
    outline x y 24 (if boarding || layRail || setCart then cartColor else muted)
  else if casting then
    outline x y 24 0x83d8e4ff
  else if planting then
    outline x y 24 0xb8db78ff
  else if spreading then
    outline x y 24 (if state.selectedStack.any (·.item == .corruptSeeds) then demoniteColor else crimtaneColor)
  else if altar then
    outline x y 24 (if tile == .demonAltar then demoniteColor else crimtaneColor)
  else if tile == .treeTrunk && harvesting && hasAxe then
    outline x y 24 gold
  else if crystal && hasPick then
    outline x y 24 lifeColor
  else if cavernCrystal && hasPick then
    outline x y 24 shardColor
  else if berry && hasPick then
    outline x y 24 berryColor
  else if (steel && strongPick) || (mineCap && hasPick) then
    outline x y 24 deepsteelColor
  else if summonColossus || (cavernCap && hasPick) then
    outline x y 24 cavernColor
  else if waterGate || magmaCap || obsidian then
    let color := if waterGate then gillsColor else if magmaCap then magmaColor else obsidianColor
    outline x y 24 color
  else if bedFoot || woodWall then
    outline x y 24 bedroomColor
  else if hook then
    let color := if state.ride.isSome then muted else hookColor
    rect (x + 11) (y + 5) 2 4 color
    rect (x + 11) (y + 15) 2 4 color
    rect (x + 5) (y + 11) 4 2 color
    rect (x + 15) (y + 11) 4 2 color
    rect (x + 11) (y + 11) 2 2 color
  else if crossbow then drawCrossbowAim state camera position
  else if state.selectedStack.any (isRanged ∘ Stack.item) then
    let color := if (visibleAmmo state).isNone then muted
      else if state.bowCooldown > 0 then gold else mint
    rect (x + 11) (y + 3) 2 6 color
    rect (x + 11) (y + 15) 2 6 color
    rect (x + 3) (y + 11) 6 2 color
    rect (x + 15) (y + 11) 6 2 color
  else
    outline x y 24 (if nearby || harvesting then mint else 0xd6a2a2b0)
  if nearby || harvesting || casting || cart.isSome then
    let label : String := if bomb then "Q  DEFUSE BOMB"
      else if relic.isSome then
        let name := if tile == .crimsonHeart then "CRIMSON HEART" else "SHADOW ORB"
        if !hammer then s!"{name}  /  HAMMER OR BOMB"
        else if terrainLineClear state.world (bodyCenterX state.player) (bodyCenterY state.player)
            (position.x * tileUnits + tileUnits / 2) (position.y * tileUnits + tileUnits / 2) then
          s!"LMB  BREAK {name}"
        else s!"{name}  /  CLEAR A PATH"
      else if cart.isSome then
        if state.ride.isSome then "F8  DISMOUNT FIRST"
        else if boarding then "Q  BOARD MINECART"
        else "STAND BESIDE MINECART TO BOARD"
      else if layRail then "RMB  LAY RAIL"
      else if setCart then "RMB  SET MINECART"
      else if casting then
        "LMB  CAST LINE"
      else if planting then
        if !isNight state.surface.clock state.legacyEarly then "RMB  PLANT SUNLEAF" else "RMB  PLANT  /  GROWS BY DAY"
      else if spreading then "RMB  PLANT IN EXPOSED DIRT"
      else if mineCap then if hasPick then "LMB  OPEN MINE CAP" else "SELECT A PICK TO OPEN CAP"
      else if cavernCap then if hasPick then "LMB  OPEN CAVERN CAP" else "SELECT A PICK TO OPEN CAP"
      else if magmaCap then if hasPick then "LMB  OPEN GALLERY CAP" else "SELECT A PICK TO OPEN CAP"
      else if waterGate then if hasPick then "LMB  RELEASE WATER" else "SELECT A PICK TO RELEASE WATER"
      else if woodWall then "LMB  REMOVE WOOD WALL"
      else if position == quarrySluice && tile == .stone then "LMB  MINE SLUICE" else match tile with
      | .chest => "Q  OPEN CHEST"
      | .dungeonKeyChest => "RMB  OPEN CHEST"
      | .dungeonGoldChest =>
        if Inventory.count state.inventory .goldenKey > 0 then "RMB  USE GOLDEN KEY"
        else "LOCKED GOLD CHEST  /  GOLDEN KEY REQUIRED"
      | .dungeonOpenChest => "RMB  OPEN GOLD CHEST"
      | .shadowChest =>
        if Inventory.count state.inventory .shadowKey > 0 || state.cursorStack.any (·.item == .shadowKey) then
          "RMB  UNLOCK SHADOW CHEST"
        else "SHADOW CHEST  /  SHADOW KEY REQUIRED"
      | .shadowOpenChest => "RMB  OPEN SHADOW CHEST"
      | .blueDungeonBrick | .greenDungeonBrick | .pinkDungeonBrick =>
        if strongPick then "DUNGEON BRICK" else s!"DUNGEON BRICK  /  {tile.minimumPickPower}% PICKAXE"
      | .supplyCache =>
        if state.canOpenSupplyCache position then "Q  CLAIM SUPPLIES" else "MOVE CLOSER TO CLAIM SUPPLIES"
      | .openedCache => "EMPTY CACHE  /  PICK TO REMOVE"
      | .homeBeacon => if state.homeAnchor == some position then "HOME BEACON  /  BOUND" else "Q  BIND HOME BEACON"
      | .platform => "DOWN  DROP THROUGH"
      | .rail => "RAIL  /  PICK TO REMOVE"
      | .treeTrunk => if hasAxe then "HOLD LMB  CHOP TREE" else "SELECT AN AXE TO CHOP"
      | .workbench => "WORK BENCH  /  ESC / CRAFTING"
      | .demonAltar | .crimsonAltar =>
        let name := if tile == .demonAltar then "DEMON ALTAR" else "CRIMSON ALTAR"
        if state.hardmode && state.selectedStack.any (·.item == .pwnhammer) then s!"{name}  /  LMB SMASH"
        else s!"{name}  /  ESC / CRAFTING"
      | .sunleaf => "PICK  HARVEST SUNLEAF"
      | .lifeCrystal => if hasPick then "LMB  MINE LIFE CRYSTAL" else "SELECT A PICK FOR LIFE CRYSTAL"
      | .crystalShard => if hasPick then "LMB  MINE CRYSTAL SHARD" else "CRYSTAL SHARD"
      | .gelatinCrystal => if hasPick then "LMB  MINE GELATIN CRYSTAL" else "GELATIN CRYSTAL"
      | .highlandBerry => if hasPick then "LMB  PICK HIGHLAND BERRIES" else "SELECT A PICK FOR BERRIES"
      | .deepsteel => if strongPick then "LMB  MINE DEEPSTEEL"
          else if state.legacyEarly then "DEEPSTEEL NEEDS CRYSTAL PICK"
          else s!"NEEDS {tile.minimumPickPower}% PICKAXE POWER"
      | .mineMarker => "DEEPSTEEL MINE"
      | .magmaMarker => "MAGMA GALLERY"
      | .bed =>
        if bedFoot then
          if bed.any (fun entry => state.bedSpawnPoint == some entry.position) then "Q  CLEAR BED SPAWN"
          else "Q  SET SPAWN AT FOOT"
        else "BED HEAD  /  AIM AT FOOT TO SET SPAWN"
      | .furnace => "FURNACE  /  ESC / CRAFTING"
      | .hellforge => "HELLFORGE  /  ESC / CRAFTING"
      | .mythrilAnvil => "MYTHRIL ANVIL  /  ESC / CRAFTING"
      | .orichalcumAnvil => "ORICHALCUM ANVIL  /  ESC / CRAFTING"
      | .adamantiteForge => "ADAMANTITE FORGE  /  ESC / CRAFTING"
      | .titaniumForge => "TITANIUM FORGE  /  ESC / CRAFTING"
      | .ironAnvil => "IRON ANVIL  /  ESC / CRAFTING"
      | .leadAnvil => "LEAD ANVIL  /  ESC / CRAFTING"
      | .sawmill => "SAWMILL  /  ESC / CRAFTING"
      | .loom => "LOOM  /  ESC / CRAFTING"
      | .woodChair => "WOODEN CHAIR  /  ROOM SEATING"
      | .woodTable => "WOODEN TABLE  /  ROOM SURFACE"
      | .obsidian => if strongPick then "LMB  MINE OBSIDIAN"
          else if state.legacyEarly then "OBSIDIAN NEEDS CRYSTAL PICK"
          else s!"NEEDS {tile.minimumPickPower}% PICKAXE POWER"
      | .hellstone =>
        if (state.legacyEarly && hasPick) || strongPick then "HELLSTONE  /  RELEASES LAVA"
        else s!"HELLSTONE  /  {tile.minimumPickPower}% PICKAXE"
      | .ash => "ASH BLOCK"
      | .obsidianBrick => "OBSIDIAN BRICK"
      | .demoniteBrick => "DEMONITE BRICK"
      | .crimtaneBrick => "CRIMTANE BRICK"
      | .copper => "COPPER ORE"
      | .tinOre => "TIN ORE"
      | .iron => "IRON ORE"
      | .leadOre => "LEAD ORE"
      | .silver => "SILVER ORE"
      | .tungstenOre => "TUNGSTEN ORE"
      | .goldOre => "GOLD ORE"
      | .platinumOre => "PLATINUM ORE"
      | .demoniteOre => "DEMONITE ORE"
      | .crimtaneOre => "CRIMTANE ORE"
      | .cobaltOre | .palladiumOre | .mythrilOre | .orichalcumOre | .adamantiteOre | .titaniumOre =>
        let name := (hardmodeOre.map (·.1)).getD "ORE"
        if strongPick then name else s!"{name}  /  {tile.minimumPickPower}% PICKAXE"
      | .hallowedGrass => "HALLOWED GRASS"
      | .pearlstone =>
        if strongPick then "PEARLSTONE BLOCK" else s!"PEARLSTONE BLOCK  /  {tile.minimumPickPower}% PICKAXE"
      | .pearlsand => "PEARLSAND BLOCK"
      | .ebonsand => "EBONSAND BLOCK"
      | .crimsand => "CRIMSAND BLOCK"
      | .pinkIce => "PINK ICE BLOCK"
      | .purpleIce => "PURPLE ICE BLOCK"
      | .redIce => "RED ICE BLOCK"
      | .ebonstone | .crimstone =>
        let name := if tile == .ebonstone then "EBONSTONE" else "CRIMSTONE"
        if strongPick then name else s!"{name}  /  65% PICKAXE OR BOMB"
      | .clayPot =>
        if hasPick || hasAxe || hammer || state.selectedStack.any (·.item.swordDamage > 0) then
          "POT  /  LMB BREAK" else "POT"
      | .vileMushroom | .viciousMushroom =>
        if tile == .vileMushroom then "VILE MUSHROOM" else "VICIOUS MUSHROOM"
      | .placedBottle => "PLACED BOTTLE  /  ESC / CRAFTING"
      | .sand => "SAND BLOCK"
      | .glassBlock => "GLASS"
      | .sunleafSprout =>
        let progress := min 99 (100 * cropTicksAt state position / cropGrowthTicks)
        let growing := !isNight state.surface.clock state.legacyEarly && sunleafCanGrow state.world state.water position
        s!"SUNLEAF  {progress}%  /  {if growing then "GROWING" else "PAUSED"}"
      | .stormAltar => if state.storm == .ready then "Q  SUMMON HERALD" else ""
      | .ruinAltar => if state.ruin == .ready then "Q  SUMMON SENTINEL" else ""
      | .cavernAltar => if summonColossus then "Q  SUMMON COLOSSUS" else ""
      | .doorClosed | .doorClosedTop | .doorClosedMiddle =>
        if state.legacyEarly then "Q OPEN   H GUIDE   J MERCHANT" else "Q OPEN   H HOUSING   J MERCHANT"
      | .doorOpen | .doorOpenTop | .doorOpenMiddle =>
        if state.legacyEarly then "Q CLOSE   H GUIDE   J MERCHANT" else "Q CLOSE   H HOUSING   J MERCHANT"
      | _ => ""
    if !label.isEmpty then
      let labelWidth := 13 + label.length * 8
      let left := max 4 (min (x - 5) (width - labelWidth - 4 : Nat))
      let labelY := y - if altar || relic.isSome || pot || planting || tile == .sunleaf ||
        tile == .sunleafSprout then 44 else 19
      rect left labelY labelWidth 17 0x141f2aed
      let color := if crystal then lifeColor else if cavernCrystal then shardColor else if berry then berryColor
        else if cavernCap || summonColossus then cavernColor
        else if waterGate then gillsColor
        else if magmaCap || tile == .magmaMarker then magmaColor
        else if obsidian then obsidianColor
        else if tile == .hellstone then magmaColor
        else if shadowChest.isSome then shadowChestColor
        else if woodWall || bed.isSome then bedroomColor
        else if altar then if tile == .demonAltar then demoniteColor else crimtaneColor
        else if relic.isSome then relicColor (tile == .crimsonHeart)
        else if steel || mineCap || tile == .mineMarker then deepsteelColor
        else (hardmodeOre.map (·.2.1)).getD gold
      text label (left + 6) (labelY + 3) 12 color

private def isSoul (item : Item) : Bool :=
  item == .soulOfLight || item == .soulOfNight || item == .soulOfSight || item == .soulOfMight ||
    item == .soulOfFright

private def drawSoul (item : Item) (x y : Int) (frame : Nat) : IO Unit := do
  let light := item == .soulOfLight
  let sight := item == .soulOfSight
  let asset := if light then "Item_520" else if sight then "Item_549"
    else if item == .soulOfMight then "Item_548" else if item == .soulOfFright then "Item_547" else "Item_521"
  discard <| originalSprite asset x y 24 24 0 (28 * (frame % 4)) 22 28 (fit := true)
private def drawItem (item : Item) (x y : Int) : IO Unit := do
  if isSoul item then drawSoul item x y 0
  else if let some asset := originalItem item then
    discard <| originalSprite asset x y 24 24 (fit := true)

private def drawReward (position : TilePos) (item : Item) (camera : Camera) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  discard <| originalSprite "Star_0" (x + 14) (y - 1) 13 13 (color := mint)
  drawItem item x y
  let label := match item with
    | .stormBow => "STORM BOW"
    | .raidCrest => "RAID CREST"
    | .ruinHammer => "RUIN HAMMER"
    | .deepstoneAegis => "DEEPSTONE AEGIS"
    | _ => "FROSTBRAND"
  let aegis := item == .deepstoneAegis
  let left := x - if aegis then 49 else 37
  rect left (y - 27) (if aegis then 122 else 98) 16 0x17233bed
  text label (left + 9) (y - 25) 12 (if aegis then cavernCoreColor else mint)

private def drawGelDrop (drop : GelDrop) (camera : Camera) : IO Unit := do
  let x := drop.position.x * tilePixels - camera.x
  let y := drop.position.y * tilePixels - camera.y
  if x + 30 < 0 || x ≥ width || y + 25 < 0 || y ≥ height then return
  drawItem .caveGel x (y + 3)
  discard <| originalSprite "Star_0" (x + 15) (y + 5) 9 9 (color := 0xc9edb1ff)

private def drawNightDrop (drop : NightDrop) (camera : Camera) (clock : Nat) : IO Unit := do
  let x := drop.position.x * tilePixels - camera.x
  let y := drop.position.y * tilePixels - camera.y
  if x + 36 < 0 || x ≥ width || y + 25 < 0 || y ≥ height then return
  if let some stack := drop.items.find? (isGun ∘ Stack.item) then
    drawItem stack.item (x - 8) (y + 4)
    if drop.items.any (·.item == .musketBall) then drawItem .musketBall (x + 12) (y + 4)
    discard <| originalSprite "Star_0" (x + 10) (y + 1) 8 8 (color := gold)
    return
  let items := if drop.items.any (isSoul ∘ Stack.item) then
      (drop.items.filter (isSoul ∘ Stack.item)) ++ (drop.items.filter fun stack => !isSoul stack.item)
    else drop.items
  for index in [:min 2 items.size] do
    let some stack := items[index]? | break
    if isSoul stack.item then
      drawSoul stack.item (x + index * 9) (y - 5 - clock / 12 % 4) (clock / 6)
    else
      drawItem stack.item (x + index * 9) (y + 3)
  if drop.coins > 0 then
    drawItem (if drop.coins ≥ 10000 then .goldCoin else if drop.coins ≥ 100 then .silverCoin else .copperCoin)
      (x + 3) (y + 6)
  discard <| originalSprite "Star_0" (x + 16) (y + 5) 8 8 (color := gold)

private def drawToolCracks (hit : TileHit) (camera : Camera) : IO Unit := do
  let x := hit.position.x * tilePixels - camera.x
  let y := hit.position.y * tilePixels - camera.y
  if x + tilePixels < 0 || x ≥ width || y + tilePixels < 0 || y ≥ height then return
  let frame := min 3 (hit.damage / 25)
  discard <| originalSprite "TileCracks" x y tilePixels tilePixels (frame * 18) 18 16 16

private def drawCaveLoot (position : TilePos) (kind : CaveMobKind) (camera : Camera)
    (nearby : Bool) : IO Unit := do
  let x := position.x * tilePixels - camera.x
  let y := position.y * tilePixels - camera.y
  if x + 26 ≥ 0 && x - 2 < width && y + 26 ≥ 0 && y - 18 < height then
    let item := kind.dropItem
    let color := residentColor kind
    drawItem item x y
    if nearby then
      let labelWidth := item.name.length * 7 + 12
      let left := max 4 (min (x - 4) (width - labelWidth - 4 : Nat))
      rect left (y - 17) labelWidth 15 0x141f2aed
      text item.name (left + 6) (y - 15) 12 color

private def drawHeart (x y : Int) (hp : Nat) (_color : UInt32) : IO Unit := do
  discard <| originalSprite "Heart" x (y - 1) 14 14 (color := 0x504e6bff)
  if hp > 0 then
    let _ ← originalSprite "Heart" x (y - 1) (max 1 (14 * hp / healthPerHeart)) 14
      0 0 (max 1 (22 * hp / healthPerHeart)) 22

private def drawHealth (state : GameState) : IO Unit := do
  text s!"Life: {state.combat.health}/{state.combat.healthLimit}" 910 23 15 paper
  let color : UInt32 := if state.combat.invulnerable > 0 then 0xffb48fff else 0xe88681ff
  for index in [:state.combat.healthLimit / healthPerHeart] do
    let x := 910 + (index % 10) * 18
    let y := 38 + (index / 10) * 14
    let hp := min healthPerHeart (state.combat.health - index * healthPerHeart)
    drawHeart x y hp color

private def manaStarX : Nat := width - 19

private def drawMana (mana : Nat) : IO Unit := do
  text "Mana" (width - 49) 6 15 paper
  let amount := min mana baseMana
  let scale := 75 + 25 * amount / baseMana
  let w := (22 * scale + 50) / 100
  let h := (24 * scale + 50) / 100
  let brightness := 30 + 225 * amount / baseMana
  let x := manaStarX - w / 2
  let y := 54 - h
  discard <| originalSprite "Mana" x y w h (color := spriteAlpha 0xffffffff brightness)
private def drawBreath (breath : Nat) : IO Unit := do
  let low := breath ≤ maxBreath / 4
  let color : UInt32 := if low then gold else 0x8ad8ecff
  text (if low then "AIR LOW" else "AIR") 910 66 13 color
  for index in [:10] do
    let alpha : UInt32 := if (index + 1) * maxBreath ≤ 10 * breath then 0xffffffff else 0xffffff35
    discard <| originalSprite "Bubble" (975 + index * 12) 66 11 11 (color := alpha)

private def drawBuffs (state : GameState) : IO Unit := do
  if state.potionSickness > 0 then
    rect 728 85 154 11 0x503541ff
    text s!"POTION SICKNESS {(state.potionSickness + 59) / 60}s" 738 85 10 lifeColor
  if state.gills > 0 then
    rect 893 85 53 11 0x244655ff
    text s!"GILLS {(state.gills + 59) / 60}s" 896 85 10 gillsColor
  if state.barkskin > 0 then
    rect 948 85 75 11 0x4a3933ff
    text s!"BARKSKIN {(state.barkskin + 59) / 60}s" 951 85 10 barkskinColor
  if state.combat.onFire > 0 then
    rect 1025 85 75 11 0x583334ff
    text s!"ON FIRE! {(state.combat.onFire + 59) / 60}s" 1028 85 10 fireColor

private inductive SlotSkin where
  | inventory | hotbarOpen | hotbarClosed | chest | coin | ammo | trash | armor | accessory
  deriving BEq, Inhabited

private def SlotSkin.asset (skin : SlotSkin) (selected : Bool) : String :=
  match skin with
  | .inventory => "Inventory_Back"
  | .hotbarOpen => "Inventory_Back9"
  | .hotbarClosed => if selected then "Inventory_Back20" else "Inventory_Back"
  | .chest => "Inventory_Back5"
  | .coin | .ammo => "Inventory_Back"
  | .trash => "Inventory_Back7"
  | .armor => "Inventory_Back13"
  | .accessory => "Inventory_Back13"

private def drawSlot (stack : Option Stack) (x y : Int) (key : Option Nat)
    (selected hovered : Bool) (selling := false) (skin := SlotSkin.inventory)
    (size := slotSize) (quantity : Option Nat := none) : IO Unit := do
  let gear := skin == .armor || skin == .accessory
  -- Equipment uses the original first loadout's green tint on a white slot sprite.
  let tint : UInt32 := if gear then
      if hovered then 0x326a40ff else 0x2b5b37dc
    else if selected || hovered then 0xffffffff else 0xffffffd4
  discard <| originalSprite (skin.asset selected) x y size size (color := tint)
  if skin == .trash && stack.isNone then
    discard <| originalSprite "Trash" (x + 9) (y + 9) 26 26 (color := 0xffffffc0)
  if gear && stack.isNone then
    let (sourceX, sourceY) : Nat × Nat := if skin == .armor then (0, 68) else (68, 102)
    let _ ← originalSprite "Extra_54" (x + 8) (y + 8) 28 28 sourceX sourceY 32 32
      (color := 0xffffff59)
  if let some key := key then
    text (toString (key % 10)) (x + 4) (y + 2) 11 (if selected then gold else paper)
  if let some stack := stack then
    let inset := (size - 24) / 2
    drawItem stack.item (x + inset) (y + inset)
    let count := quantity.getD stack.count
    if count > 1 || quantity.isSome then
      let digits := toString count
      let textSize := if size < slotSize then 9 else 11
      let ink := if count == 0 then lifeColor else paper
      text digits (x + (size - 3 - digits.length * 6 : Nat)) (y + (size - 11 : Nat)) textSize ink
    if selling then
      let price := stack.count * sellPrice stack.item
      text (if price == 0 then "X" else toString price) (x + 4) (y + 2) 9
        (if price == 0 then muted else gold)

private def stackLabel (stack : Option Stack) (legacyEarly := false) : String :=
  match stack with
  | some stack =>
    let melee := if legacyEarly then stack.item.legacySwordDamage else stack.item.swordDamage
    let damage := (stack.item.bowShot.map ShotKind.damage).getD melee
    if stack.item == .bone then s!"Bone  /  LMB throw  /  {ShotKind.thrownBone.damage} ranged damage"
    else if stack.item == .aquaScepter then "Aqua Scepter  /  27 magic damage  /  Uses 7 mana"
    else if stack.item == .blueMoon then "Blue Moon  /  54 melee damage  /  Hold to spin"
    else if stack.item == .magicMissile then "Magic Missile  /  35 magic damage  /  Uses 14 mana"
    else if stack.item == .valor then "Valor  /  28 melee damage  /  Hold to guide"
    else if stack.item == .sunfury then "Sunfury  /  64 melee damage  /  Hold to spin"
    else if stack.item == .darkLance then "Dark Lance  /  34 melee damage  /  Inflicts Shadowflame"
    else if stack.item == .flowerOfFire then s!"Flower of Fire  /  {flowerFireDamage} magic damage  /  {flowerFireManaCost} mana"
    else if stack.item == .flamelash then s!"Flamelash  /  {flamelashDamage} magic damage  /  {flamelashManaCost} mana"
    else if stack.item == .hellwingBow then s!"Hellwing Bow  /  {stack.item.rangedDamage} ranged damage  /  Wooden Arrows become bats"
    else if stack.item == .demonScythe then s!"Demon Scythe  /  {demonScytheDamage} magic damage  /  {demonScytheManaCost} mana"
    else if stack.item == .guideVoodooDoll then "Guide Voodoo Doll  /  T Drop"
    else if stack.item == .obsidianRose then "Obsidian Rose  /  Reduces lava damage"
    else if stack.item == .magmaStone then "Magma Stone  /  Melee attacks inflict Hellfire"
    else if stack.item == .batBat then s!"Bat Bat  /  {melee} melee damage  /  Heals on hit"
    else if stack.item == .treasureMagnet then "Treasure Magnet  /  Increases item pickup range"
    else if stack.item == .gelatinousPillion then "Gelatinous Pillion  /  Summons a rideable Winged Slime"
    else if stack.item == .bladeStaff then "Blade Staff  /  6 summon damage  /  25 armor penetration"
    else if stack.item == .summonerEmblem then "Summoner Emblem  /  15% increased summon damage"
    else if stack.item == .dynamite then "Dynamite  /  Throws a large timed explosive"
    else if stack.item == .bugNet then "Bug Net  /  Swing near critters"
    else if stack.item == .lavaproofBugNet || stack.item == .goldenBugNet then
      s!"{stack.item.name}  /  Catches lava critters"
    else if stack.item.fishingBaitPower > 0 then
      s!"{stack.item.name}  /  {stack.item.fishingBaitPower}% bait power"
    else if stack.item.rangedDamage > 0 && (!legacyEarly || stack.item != .bow) then
      s!"{stack.item.name}  /  {stack.item.rangedDamage} ranged damage"
    else if damage > 0 then s!"{stack.item.name}  /  {damage} damage"
    else if stack.item == .suspiciousLookingEye then "Suspicious Looking Eye  /  LMB at night"
    else if stack.item == .wormFood then "Worm Food  /  LMB in Corruption"
    else if stack.item == .bloodySpine then "Bloody Spine  /  LMB in Crimson"
    else if stack.item == .bottle then "Bottle  /  RMB on a bench, table or platform"
    else if stack.item == .lesserHealingPotion then "Lesser Healing Potion  /  LMB heal 50 HP"
    else if stack.item == .healingPotion then "Healing Potion  /  LMB heal 100 HP"
    else if stack.item == .goldenKey then "Golden Key  /  Unlocks a Gold Chest"
    else if stack.item == .shadowKey then "Shadow Key  /  Reusable key for Shadow Chests"
    else if stack.item == .obsidianSkull then "Obsidian Skull  /  Protects against hot blocks"
    else if stack.item == .nazar then "Nazar  /  Grants immunity to Cursed"
    else if stack.item == .unholyArrow then "Unholy Arrow  /  Piercing ammo"
    else if stack.item == .musketBall then "Musket Ball  /  Gun ammo"
    else if stack.item.isBomb then s!"{stack.item.name}  /  LMB throw"
    else if stack.item == .corruptSeeds || stack.item == .crimsonSeeds then s!"{stack.item.name}  /  RMB exposed Dirt"
    else if !legacyEarly && stack.item.pickPower > 0 then s!"{stack.item.name}  /  {stack.item.pickPower}% pickaxe power"
    else if !legacyEarly && stack.item.axePower > 0 then s!"{stack.item.name}  /  {stack.item.axePower}% axe power"
    else if stack.item == .draught then "Healing draught  /  LMB drink"
    else if stack.item == .watchhorn then "Watchhorn  /  LMB blow"
    else if stack.item == .platform then "Wood platform  /  Hold DOWN to drop through"
    else if stack.item == .homeBeacon then "Home Beacon  /  Place with RMB, bind with Q"
    else if stack.item == .recallCharm then "Recall Charm  /  LMB to return home"
    else if stack.item == .pondPerch then s!"{stack.item.name}  /  LMB heal {healthPerHeart} HP"
    else if stack.item == .caveEel then s!"{stack.item.name}  /  LMB restore breath"
    else if stack.item == .sunleafSeed then s!"{stack.item.name}  /  RMB plant"
    else if stack.item == .gillsElixir then s!"{stack.item.name}  /  LMB breathe for {gillsTicks / 60}s"
    else if stack.item == .barkskinTonic then s!"{stack.item.name}  /  LMB +1 defense for {barkskinTicks / 60}s"
    else if stack.item == .woodWall then "Wood Wall  /  RMB place background"
    else if stack.item == .woodHammer then "Wooden Hammer  /  LMB on walls or relics"
    else if stack.item == .pwnhammer then "Pwnhammer  /  LMB to smash Demon or Crimson Altars in Hardmode"
    else if stack.item == .bed then "Bed  /  RMB place, Q at foot sets spawn"
    else if stack.item == .hookOfDissonance then "Hook of Dissonance  /  E to grapple and teleport"
    else if stack.item == .crystalAssassinHood then "Crystal Assassin Hood  /  12 defense, +5% crit, -10% mana cost"
    else if stack.item == .crystalAssassinShirt then "Crystal Assassin Shirt  /  14 defense, +5% damage, 10% ammo savings"
    else if stack.item == .crystalAssassinPants then "Crystal Assassin Pants  /  10 defense, +20% movement, +10% melee speed"
    else if stack.item == .deepstoneAegis then
      s!"{stack.item.name}  /  +{({ armor := some .deepstoneAegis } : Equipment).defense} defense"
    else if stack.item.gearSlot == some .armor then s!"{stack.item.name}  /  Armor"
    else if stack.item.gearSlot == some .accessory then s!"{stack.item.name}  /  Accessory"
    else stack.item.name
  | none => "Empty slot"

private def drawBackpack (state : GameState) (hovered : Option Nat) : IO Unit := do
  let selling := state.openShop
  for index in [Inventory.hotbarCount:Inventory.mainSlotCount] do
    let x := inventoryX (index % Inventory.hotbarCount)
    let y := inventoryY (index / Inventory.hotbarCount)
    drawSlot state.inventory[index]?.join x y none false (hovered == some index) selling
  text "Coins" coinX 87 12 paper
  text "Ammo" ammoX 87 12 paper
  for row in [:Inventory.ammoStart - Inventory.coinStart] do
    let index := Inventory.coinStart + row
    drawSlot state.inventory[index]?.join coinX (sideSlotY row) none false (hovered == some index)
      (skin := .coin) (size := sideSlotSize)
  for row in [:Inventory.trashSlot - Inventory.ammoStart] do
    let index := Inventory.ammoStart + row
    drawSlot state.inventory[index]?.join ammoX (sideSlotY row) none false (hovered == some index)
      (skin := .ammo) (size := sideSlotSize)
  drawSlot state.inventory[Inventory.trashSlot]?.join trashX trashY none false
    (hovered == some Inventory.trashSlot) (skin := .trash)

private def drawChest (world : World) (chest : Chest) (hovered : Option Nat) : IO Unit := do
  let tile := world.tile chest.position
  let title := if tile.isShadowChest then "Shadow Chest"
    else if tile == .dungeonOpenChest then "Gold Chest"
    else if tile == .lihzahrdChest then "Lihzahrd Chest" else "Chest"
  text title chestLeft (chestY + 9) 17 paper
  text "Left click moves a stack. Shift-click transfers it." chestLeft (chestY + 27) 13 muted
  for index in [:chestSlots] do
    let x := chestLeft + inventoryStep (index % chestColumns)
    let y := chestSlotsY + inventoryStep (index / chestColumns)
    drawSlot chest.slots[index]?.join x y none false (hovered == some index) (skin := .chest)

private def drawSaveQuit (mouseX mouseY : Nat) : IO Unit := do
  let hovered := saveQuitAt mouseX mouseY
  let color := if hovered then gold else paper
  rect saveQuitX saveQuitY saveQuitW saveQuitH (if hovered then 0x293242ed else 0x182435d6)
  text "Save & Exit" (saveQuitX + 30) (saveQuitY + 8) 20 color

private def drawInventoryHover (label : String) (mouseX mouseY : Nat) : IO Unit := do
  if label.isEmpty then return
  let extent := min 520 (max 42 (label.length * 8 + 16))
  let x := min (mouseX + 17) (width - extent - 6)
  let y := min (mouseY + 18) (height - 26)
  rect x y extent 23 0x111925ed
  text label (x + 6) (y + 3) 14 paper

private def drawCursedBuff (ticks : Nat) (inventoryOpen : Bool) (mouseX mouseY : Nat)
    (showTooltip : Bool) : IO Unit := do
  if ticks == 0 then return
  let (x, y) := cursedBuffOrigin inventoryOpen
  let hovered := cursedBuffAt inventoryOpen mouseX mouseY
  discard <| originalSprite "Buff_23" x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
  if ticks > 2 then
    let time := s!"{ticks / 60} s"
    text time (x + 1) (y + 33) 11 0x0c1427ee
    text time x (y + 32) 11 paper
  if hovered && showTooltip then drawInventoryHover "Cursed  /  Cannot use any items" mouseX mouseY

private def drawBurningBuff (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  if state.burningTicks == 0 then return
  let cursed := state.cursedTicks > 0
  let (x, y) := burningBuffOrigin inventoryOpen cursed
  let hovered := burningBuffAt inventoryOpen cursed mouseX mouseY
  discard <| originalSprite "Buff_67" x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
  if hovered && state.cursorStack.isNone then
    drawInventoryHover "Burning  /  Hot blocks slow movement" mouseX mouseY

private def drawDarknessBuff (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  if state.darknessTicks == 0 then return
  let cursed := state.cursedTicks > 0
  let burning := state.burningTicks > 0
  let (x, y) := darknessBuffOrigin inventoryOpen cursed burning
  let hovered := darknessBuffAt inventoryOpen cursed burning mouseX mouseY
  discard <| originalSprite "Buff_22" x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
  let duration := s!"{(state.darknessTicks + 59) / 60} s"
  text duration (x + 1) (y + 33) 11 0x0c1427ee
  text duration x (y + 32) 11 paper
  if hovered && state.cursorStack.isNone then
    drawInventoryHover "Darkness  /  Decreased light vision" mouseX mouseY

private def drawObsidianSkinBuff (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  if state.obsidianSkinTicks == 0 then return
  let cursed := state.cursedTicks > 0
  let burning := state.burningTicks > 0
  let darkness := state.darknessTicks > 0
  let (x, y) := obsidianSkinBuffOrigin inventoryOpen cursed burning darkness
  let hovered := obsidianSkinBuffAt inventoryOpen cursed burning darkness mouseX mouseY
  discard <| originalSprite "Buff_1" x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
  let duration := (state.obsidianSkinTicks + 59) / 60
  let caption := if duration ≥ 60 then s!"{duration / 60} m" else s!"{duration} s"
  text caption (x + 1) (y + 33) 11 0x0c1427ee
  text caption x (y + 32) 11 paper
  if hovered && state.cursorStack.isNone then
    drawInventoryHover "Obsidian Skin  /  Immune to lava and hot blocks" mouseX mouseY

private def drawWallBuffs (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  let index := if inventoryOpen then 0 else
    (state.cursedTicks != 0).toNat + (state.burningTicks != 0).toNat +
    (state.darknessTicks != 0).toNat + (state.obsidianSkinTicks != 0).toNat
  let drawBuff (index : Nat) (asset title : String) (_color : UInt32) : IO Unit := do
    let (x, y) := wallBuffOrigin inventoryOpen index
    let hovered := containsRect mouseX mouseY x y 32 32
    discard <| originalSprite asset x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
    if hovered && state.cursorStack.isNone then drawInventoryHover title mouseX mouseY
  if state.horrified then
    drawBuff index "Buff_37" "Horrified  /  You cannot escape the Wall of Flesh" 0xd3796aff
  if state.tongued then
    drawBuff (index + state.horrified.toNat) "Buff_38" "The Tongue  /  The Wall of Flesh is pulling you in" 0xbc546aff

private def drawSlowBuff (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  if state.slowTicks == 0 then return
  let (x, y) := slowBuffOrigin inventoryOpen (state.cursedTicks != 0) (state.burningTicks != 0)
    (state.darknessTicks != 0) (state.obsidianSkinTicks != 0) state.horrified state.tongued
  let hovered := containsRect mouseX mouseY x y 32 32
  discard <| originalSprite "Buff_32" x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
  let duration := s!"{(state.slowTicks + 59) / 60} s"
  text duration (x + 1) (y + 33) 11 0x0c1427ee
  text duration x (y + 32) 11 paper
  if hovered && state.cursorStack.isNone then
    drawInventoryHover "Slow  /  Movement speed reduced" mouseX mouseY

private def drawCavernBuffs (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  let drawBuff (ticks : Nat) (inferno : Bool) (asset title : String) (_color : UInt32) : IO Unit := do
    if ticks == 0 then return
    let (x, y) := cavernBuffOrigin state inventoryOpen inferno
    let hovered := containsRect mouseX mouseY x y 32 32
    discard <| originalSprite asset x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
    let seconds := (ticks + 59) / 60
    let caption := if seconds ≥ 60 then s!"{seconds / 60} m" else s!"{seconds} s"
    text caption (x + 1) (y + 33) 11 0x0c1427ee
    text caption x (y + 32) 11 paper
    if hovered && state.cursorStack.isNone then drawInventoryHover title mouseX (max mouseY (y + 29))
  drawBuff state.weakTicks false "Buff_33" "Weak  /  Reduced defense, movement and melee ability" 0xadc5edff
  drawBuff state.cursedInfernoTicks true "Buff_39" "Cursed Inferno  /  Losing life" 0x9fdf4bff

private def drawPoisonBuff (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  if state.poisonedTicks == 0 then return
  let (x, y) := poisonBuffOrigin state inventoryOpen
  let hovered := poisonBuffAt state inventoryOpen mouseX mouseY
  discard <| originalSprite "Buff_20" x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
  text s!"{(state.poisonedTicks + 59) / 60} s" x (y + 32) 11 paper
  if hovered && state.cursorStack.isNone then drawInventoryHover "Poisoned  /  Losing life" mouseX mouseY

private def drawTempleBuffs (state : GameState) (inventoryOpen : Bool) (mouseX mouseY : Nat) : IO Unit := do
  for ((ticks, asset, label), index) in (templeBuffs state).zipIdx do
    let (x, y) := templeBuffOrigin state inventoryOpen index
    let hovered := containsRect mouseX mouseY x y 32 32
    discard <| originalSprite asset x y 32 32 (color := if hovered then 0xffffffff else 0xffffffd8)
    text s!"{(ticks + 59) / 60} s" x (y + 32) 11 paper
    if hovered && state.cursorStack.isNone then drawInventoryHover label mouseX mouseY

private def drawHeldStack (stack : Stack) (mouseX mouseY : Nat) : IO Unit := do
  let x := min (mouseX + 7) (width - 29)
  let y := min (mouseY + 7) (height - 29)
  drawItem stack.item x y
  if stack.count > 1 then
    text (toString stack.count) (x + 12) (y + 17) 11 paper

private def drawShop (state : GameState) (hovered : Option Nat) : IO Unit := do
  rect 575 shopPanelY 533 shopPanelHeight 0x111925fa
  rect 575 shopPanelY 533 2 gold
  text "MERCHANT" (shopLeft + 9) (shopPanelY + 12) 17 paper
  text s!"COINS  {state.coins}" 937 (shopPanelY + 14) 14 gold
  text "Click an offer to buy or sell the stack you hold." (shopLeft + 9) (shopPanelY + 33) 13 muted
  let offers := shopOffersFor state.legacyEarly
  for index in [:offers.size] do
    let some offer := offers[index]? | continue
    let remaining := state.merchantStock[index]?.getD 0
    let unlimited := offer.item == .bugNet
    let y := shopOfferY + index * shopOfferStride
    let available := (remaining > 0 || unlimited) && state.coins ≥ offer.price
    let background : UInt32 := if hovered == some index then 0x3f3a48ff else 0x273141ff
    rect shopLeft y (1100 - shopLeft) shopOfferHeight background
    drawItem offer.item (shopLeft + 8) (y + 5)
    text s!"{offer.quantity}x  {offer.item.name}" (shopLeft + 44) (y + 10) 15
      (if remaining == 0 && !unlimited then muted else paper)
    if offer.item == .amberTreads then text "Accessory" 802 (y + 11) 12 muted
    if offer.item == .watchhorn then text "Raid item" 802 (y + 11) 12 muted
    text s!"{offer.price} {if state.legacyEarly then "coins" else "copper"}" 896 (y + 11) 13
      (if available then gold else muted)
    text (if unlimited then "Unlimited" else if remaining == 0 then "SOLD OUT" else s!"{remaining} left")
      1003 (y + 11) 13 muted

private def drawBackpackTabs (crafting : Bool) (mouseX mouseY : Nat) : IO Unit := do
  let hovered := classicTabAt mouseX mouseY
  let icon := if hovered == some true then "UI/Craft_Toggle_1" else "UI/Craft_Toggle_0"
  let _ ← originalSprite icon craftLeft (craftY - 1) 23 25
  text "Crafting" (craftLeft + 29) (craftY + 2) 16 (if hovered == some true then gold else paper)
  if crafting then
    let x := craftLeft + classicToggleWidth + 5
    rect x craftY 25 24 (if hovered == some false then 0x3b4c5bff else 0x232e3ce0)
    text "-" (x + 9) (craftY + 3) 16 paper

private def drawEquipment (state : GameState) (hovered : Option GearSlot) : IO Unit := do
  let equipment := state.equipment
  text s!"Defense: {state.defense}" (gearX - 82) (gearY - 22) 13 paper
  for (label, item, slot, y, skin) in [
      ("Head", equipment.head, GearSlot.head, gearY, SlotSkin.armor),
      ("Body", equipment.armor, GearSlot.armor, armorY, SlotSkin.armor),
      ("Legs", equipment.legs, GearSlot.legs, legsY, SlotSkin.armor),
      ("Accessory", equipment.accessory, GearSlot.accessory, accessoryY, SlotSkin.accessory),
      ("Hook", equipment.hook, GearSlot.hook, hookY, SlotSkin.accessory),
      ("Mount", equipment.mount, GearSlot.mount, mountY, SlotSkin.accessory)] do
    text label (gearX - 73) (y + 13) 13 paper
    drawSlot (item.map fun item => ⟨item, 1⟩) gearX y none false (hovered == some slot) (skin := skin)
  if equipment.crystalAssassinSet then
    text "Set bonus / Double-tap a direction to dash" (gearX - 246) (gearY - 39) 13 paper

private def isMechanicalSummonRecipe : ClassicRecipe → Bool
  | .mechanicalEye | .mechanicalWorm | .mechanicalWormCrimson | .mechanicalSkull => true
  | _ => false

private def recipeCost (recipe : ClassicRecipe) (compact := false) : String :=
  String.intercalate " + " (recipe.ingredients.map fun (item, quantity) =>
    let name := if item == .ironBar && recipe.acceptsLeadBar then
        if compact then "Iron/Lead" else "Iron/Lead Bar"
      else if compact && item == .soulOfLight then "Light"
      else if compact && item == .soulOfNight then "Night"
      else item.name
    s!"{quantity} {name}")

private def drawClassicCard (recipe : ClassicRecipe) (x y : Nat) (ready hovered : Bool)
    (preview := false) : IO Unit := do
  let active := ready && hovered
  let label := if recipe.quantity > 1 then s!"{recipe.item.name} x{recipe.quantity}" else recipe.item.name
  let compact := isMechanicalSummonRecipe recipe
  rect x y recipeWidth classicRowHeight (if active then 0x3b4c5bff else if ready then 0x2e404dff else 0x232e3cff)
  rect x y 2 classicRowHeight (if ready then mint else 0x415061ff)
  drawItem recipe.item (x + 5) (y + 1)
  text label (x + 34) (y + 2) (if label.length > 18 then 11 else 12)
    (if ready || preview then paper else muted)
  let station := match recipe.station with
    | some .demonAltar => "Either Altar"
    | some .ironAnvil => "Iron/Lead Anvil"
    | some .mythrilAnvil => "HM Anvil"
    | some .adamantiteForge => "HM Forge"
    | _ => recipe.stationName
  text station (x + 165) (y + 3) 10 (if ready then mint else muted)
  text (recipeCost recipe compact) (x + 34) (y + 14) (if compact then 10 else 11) muted

private def drawClassicCrafting (state : GameState) (page mouseX mouseY : Nat) : IO Unit := do
  let hovered := classicRecipeAt mouseX mouseY page
  let paging := classicPageAt page mouseX mouseY
  for (label, x, enabled) in [("<", classicPrevX, decide (page > 0)),
      (">", classicNextX, decide (page + 1 < classicPageCount))] do
    let active := enabled && paging.isSome && x ≤ mouseX && mouseX < x + classicPagerWidth
    rect x craftY classicPagerWidth 23 (if active then 0x3b4c5bff else 0x232e3cff)
    text label (x + 14) (craftY + 4) 15 (if enabled then mint else 0x667482ff)
  text s!"PAGE {page + 1} / {classicPageCount}" (classicPrevX + 81) (craftY + 6) 13 paper
  for index in [:classicPageSize] do
    let some recipe := ClassicRecipe.all[page * classicPageSize + index]? | break
    let x := craftLeft + index % 2 * recipeStride
    let y := classicRowY + index / 2 * classicRowStride
    drawClassicCard recipe x y (state.canCraftClassic recipe true || state.canCraftClassic recipe)
      (hovered == some recipe)
  text "Click to craft to cursor. Shift-click crafts to inventory." (craftLeft + 8)
    (classicRowY + classicRows * classicRowStride - 2) 11 muted
  if let some recipe := hovered then
    if isMechanicalSummonRecipe recipe then
      let y := classicRowY + classicRows * classicRowStride + 13
      rect craftLeft y (recipeStride + recipeWidth) 23 0x111925ed
      text (recipeCost recipe) (craftLeft + 8) (y + 5) 12 paper

private def wardenHint (boss : Warden) (armed : Bool) : String :=
  match boss.phase with
  | .approach =>
    if armed then "Keep moving. Watch for arrows before the Warden charges."
    else "Select a sword from the hotbar. Retreat west to prepare."
  | .windup =>
    let direction := if boss.facing < 0 then "left" else "right"
    s!"Charge aimed {direction}. Jump over the Warden as it lunges."
  | .charge => "Charging! Jump over the Warden."
  | .recover =>
    if armed then "Staggered! Move close and left-click toward the Warden."
    else "Staggered! Equip a sword to land a hit."

private def drawBossHealth (health maximum : Nat) (color : UInt32) : IO Unit := do
  rect 445 (hudTop + 34) 172 13 0x323b54ff
  rect 446 (hudTop + 35) (170 * health / maximum) 11 color
  rect 530 (hudTop + 34) 1 13 backgroundColor
  text s!"{health}/{maximum}" 629 (hudTop + 34) 14 paper

private def drawBossTimer (ticks duration : Nat) (color : UInt32) : IO Unit := do
  rect 39 (hudTop + 78) 653 2 0x323b54ff
  rect 39 (hudTop + 78) (653 * ticks / duration) 2 color

private def drawCompactBossBar (name : String) (icon : Item) (health maximum : Nat)
    (_color : UInt32) (headAsset := "") (top : Int := 531) : IO Unit := do
  let asset := "UI/UI_BossBar"
  let drawIcon (x y : Int) : IO Unit := do
    if !headAsset.isEmpty then
      if ← originalSprite headAsset x y 24 24 (fit := true) then return
    drawItem icon x y
  discard <| originalSprite asset 330 top 460 52 0 0 516 58
  let _ ← originalSprite asset 358 (top + 20) 407 20 32 196 456 22
  if health > 0 then
    let _ ← originalSprite asset 358 (top + 20) (max 1 (407 * health / maximum)) 20 32 80 2 22
  drawIcon 331 (top + 16)
  text name 366 (top + 1) 13 paper
  text s!"{health}/{maximum}" 696 (top + 1) 13 paper

def skeletronInstruction (boss : Skeletron) : String :=
  match boss.phase with
  | .hover =>
    if boss.leftHand.isNone && boss.rightHand.isNone then "Both hands are down. Aim for Skeletron's head."
    else "Dodge the hands as they rise or draw back. Aim at a hand or the skull."
  | .spin => "Keep moving away from the spinning skull. It has no defense during the spin."
  | .enraged => "Dawn has enraged Skeletron. Escape its skull."
  | .retreat => "Skeletron is leaving. Speak to the Old Man again after dark."

private def drawSkeletronHud (boss : Skeletron) : IO Unit := do
  let phase := match boss.phase with
    | .hover => if boss.leftHand.isNone && boss.rightHand.isNone then "  /  HANDS DOWN" else ""
    | .spin => "  /  SPINNING"
    | .enraged => "  /  ENRAGED"
    | .retreat => "  /  RETREATING"
  drawCompactBossBar ("SKELETRON" ++ phase) .skeletronMask boss.health skeletronMaxHealth
    (if boss.phase == .enraged then lifeColor else gold) (headAsset := "NPC_Head_Boss_19") (top := 632)

private def drawWallOfFleshHud (boss : WallOfFlesh) : IO Unit :=
  drawCompactBossBar "Wall of Flesh" .wallOfFleshMask boss.health wallOfFleshMaxHealth
    0xd15067ff (headAsset := "NPC_Head_Boss_22") (top := 632)

private def drawMechanicalBossBar (health maximum : Nat) (head : String) : IO Unit := do
  let asset := "UI/UI_BossBar"
  let left : Int := 332
  let top : Int := 643
  discard <| originalSprite asset (left - 32) (top - 24) 516 58 0 174 516 58 (color := 0xffffff33)
  let fill := 456 * health / maximum
  let fill := fill - fill % 2
  if fill ≥ 2 then
    let _ ← originalSprite asset left top fill 22 32 140 2 22
    let _ ← originalSprite asset (left + (fill : Int) - 2) top 2 22 32 82 2 22
  let _ ← originalSprite asset (left - 32) (top - 24) 516 58 0 0 516 58
  let _ ← originalSprite head (left - 30) (top - 4) 30 28
  let label := s!"{health}/{maximum}"
  let x : Int := 560 - (label.length * 7 / 2 : Nat)
  for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] do
    text label (x + dx) (top + 4 + dy) 14 0x000000ff
  text label x (top + 4) 14 0xffffffff

private def drawTwinsHud (state : GameState) : IO Unit := do
  let some boss := state.retinazer.or state.spazmatism | return
  let actors := state.retinazer.toArray ++ state.spazmatism.toArray
  let health := actors.foldl (fun total twin => total + twin.health) 0
  let maximum := actors.foldl (fun total twin => total + twin.kind.health) 0
  let icon : Nat := if boss.kind == .retinazer then (if boss.secondForm then 16 else 15)
    else (if boss.secondForm then 21 else 20)
  drawMechanicalBossBar health maximum s!"NPC_Head_Boss_{icon}"

def eyeInstruction (boss : EyeOfCthulhu) : String :=
  match boss.phase with
  | .firstHover =>
    if boss.ticks + 48 ≥ boss.phase.duration then "The Eye is preparing to dive. Move across its path."
    else if !boss.servants.isEmpty then "Fire upward or clear the Servants that approach you."
    else "Fire upward while the Eye hovers. Watch for the dive."
  | .firstCharge | .secondCharge =>
    if boss.ticks ≤ eyeChargeCruiseTicks then "Charging! Move across its path and keep clear of contact."
    else "It is slowing down. Attack before the next charge."
  | .transformSpinUp | .transformSpinDown => "The Eye is changing form. Keep your distance."
  | .secondHover => "Its jaws are exposed. Attack while it slows between charges."
  | .retreat => "The Eye is retreating. Prepare to summon it another night."

private def drawEyeHud (boss : EyeOfCthulhu) (overlayOpen : Bool) : IO Unit := do
  let color := if boss.phase.secondForm || boss.phase.transforming then eyeMawColor else eyeIrisColor
  let phase := if boss.phase.transforming then "TRANSFORMING"
    else if boss.phase == .retreat then "RETREATING"
    else if boss.phase.secondForm then "FORM II" else "FORM I"
  if overlayOpen then
    text "EYE OF CTHULHU" 39 (hudTop + 28) 23 paper
    text phase 285 (hudTop + 35) 13 color
    drawBossHealth boss.health eyeMaxHealth color
  else
    text phase 39 (hudTop + 30) 20 color
    drawCompactBossBar "EYE OF CTHULHU" .suspiciousLookingEye boss.health eyeMaxHealth color
  text (eyeInstruction boss) 39 (hudTop + 58) 17 paper
  if boss.phase.transforming || boss.phase.charging || boss.ticks + 48 ≥ boss.phase.duration then
    drawBossTimer (min boss.ticks boss.phase.duration) boss.phase.duration color

private def wormInstruction (boss : EaterOfWorlds) : String :=
  if boss.retreating then "The Eater is retreating. Stay in the Corruption to fight."
  else if boss.chains.size > 1 then "The worm has split. Destroy every chain and dodge its heads."
  else "Attack the segments as they pass. Keep clear of the head."

private def brainInstruction (boss : BrainOfCthulhu) : String :=
  match boss.phase with
  | .shielded => "Destroy the Creepers to expose the Brain."
  | .exposed => "Attack the exposed heart. Watch where the Brain reappears."
  | .retreat => "The Brain is retreating. Summon it again in the Crimson."

def evilBossInstruction (state : GameState) : Option String :=
  (state.eater.map wormInstruction).orElse fun _ => state.brain.map brainInstruction

private def drawEvilBossHud (name phase hint : String) (icon : Item) (health maximum : Nat)
    (color : UInt32) (overlayOpen : Bool) : IO Unit := do
  if overlayOpen then
    text name 39 (hudTop + 28) 21 paper
    text phase 282 (hudTop + 34) 13 color
    drawBossHealth health maximum color
  else
    text phase 39 (hudTop + 30) 19 color
    drawCompactBossBar name icon health maximum color
  text hint 39 (hudTop + 58) 17 paper

private def drawWormHud (boss : EaterOfWorlds) (overlayOpen : Bool) : IO Unit := do
  let phase := if boss.retreating then "RETREATING"
    else if boss.chains.size == 1 then "ONE CHAIN" else s!"SPLIT  /  {boss.chains.size} CHAINS"
  drawEvilBossHud "EATER OF WORLDS" phase (wormInstruction boss) .eaterMask
    boss.health eaterMaxHealth wormColor overlayOpen

private def drawBrainHud (boss : BrainOfCthulhu) (overlayOpen : Bool) : IO Unit := do
  let phase := match boss.phase with
    | .shielded => s!"SHIELDED  /  {boss.creepers.size} CREEPERS"
    | .exposed => "HEART EXPOSED"
    | .retreat => "RETREATING"
  drawEvilBossHud "BRAIN OF CTHULHU" phase (brainInstruction boss) .brainMask
    boss.totalHealth brainTotalMaxHealth brainColor overlayOpen

private def drawWardenHud (boss : Warden) (armed : Bool) : IO Unit := do
  let phaseColor := if boss.enraged then gold else mint
  text "FROST WARDEN" 39 (hudTop + 28) 23 paper
  text (if boss.enraged then "PHASE 2" else "PHASE 1") 266 (hudTop + 35) 13 phaseColor
  drawBossHealth boss.health wardenMaxHealth phaseColor
  text (wardenHint boss armed) 39 (hudTop + 58) 17 (if boss.phase == .approach then muted else paper)
  if boss.phase == .windup || boss.phase == .recover then
    drawBossTimer boss.ticks boss.phaseDuration phaseColor

private def drawStormHud (boss : StormHerald) : IO Unit := do
  let color : UInt32 := if boss.enraged then 0xffb477ff else 0xddb7f5ff
  text "STORM HERALD" 39 (hudTop + 28) 23 paper
  text (if boss.enraged then "PHASE 2" else "PHASE 1") 266 (hudTop + 35) 13 color
  drawBossHealth boss.health stormMaxHealth color
  let hint := match boss.phase with
    | .aim => s!"Aiming {if boss.facing < 0 then "left" else "right"}. Get behind cover before the volley."
    | .barrage => "Barrage! Shelter behind a wall or cross to its back."
    | .recover => "Exposed! Fire from a clear line or get close and swing."
  text hint 39 (hudTop + 58) 17 paper
  drawBossTimer boss.ticks boss.phaseDuration color

private def drawRuinHud (boss : RuinSentinel) (armed : Bool) : IO Unit := do
  let (phase, hint, color) : String × String × UInt32 := match boss.phase with
    | .windup =>
      if boss.targetLocked then ("TARGET LOCKED", "Jump over the shockwaves when the hammer lands.", ruinColor)
      else ("TRACKING", "The floor marker follows you. Get ready to jump.", ruinColor)
    | .slam => ("SHOCKWAVES", "Jump over the waves. The Sentinel opens after the slam.", paper)
    | .recover =>
      ("EXPOSED", if armed then "Strike up close or shoot while its core is exposed."
        else "Select a sword or bow and attack while its core is exposed.", ruinRune)
  text "RUIN SENTINEL" 39 (hudTop + 28) 23 paper
  text phase 267 (hudTop + 35) 13 color
  drawBossHealth boss.health ruinMaxHealth color
  text hint 39 (hudTop + 58) 17 paper
  drawBossTimer boss.ticks boss.phaseDuration color

private def drawCavernHud (boss : DeepstoneColossus) (armed : Bool) : IO Unit := do
  let high := cavernHighStage boss
  let (phase, hint, color) : String × String × UInt32 := match boss.phase with
    | .windup =>
      ("1  LOW BEAM", "Jump just before the low beam reaches you. Land for the high beam.", cavernLowColor)
    | .beam =>
      if high then ("2  HIGH BEAM", "Stay grounded. The high beam passes overhead.", cavernHighColor)
      else ("1  LOW BEAM", "Jump over the low beam, then land and stay grounded.", cavernLowColor)
    | .recover =>
      ("SHELL OPEN", if armed then "Stay grounded and attack the exposed core."
        else "Select a sword or ranged weapon to hit the exposed core.", cavernCoreColor)
  text "DEEPSTONE COLOSSUS" 39 (hudTop + 30) 21 paper
  text phase 326 (hudTop + 35) 13 color
  drawBossHealth boss.health cavernMaxHealth color
  text hint 39 (hudTop + 58) 17 paper
  drawBossTimer boss.ticks boss.phaseDuration color

private def drawRaidHud (wave : RaidWave) : IO Unit := do
  text "AMBER RAID" 39 (hudTop + 28) 23 paper
  text s!"WAVE {wave.number}/{raidWaveCount}" 251 (hudTop + 35) 15 amberColor
  if wave.countdown > 0 then
    text "INCOMING" 529 (hudTop + 34) 14 gold
    let seconds := (wave.countdown + 59) / 60
    text s!"The next wave arrives in {seconds}s. Prepare cover and draw your weapon." 39 (hudTop + 58) 17 paper
    drawBossTimer wave.countdown raidCountdownTicks amberColor
  else
    text s!"RAIDERS LEFT  {wave.foes.size}" 493 (hudTop + 34) 14 gold
    text "Stay in the bluffs. Use walls to block slinger bolts." 39 (hudTop + 58) 17 paper

/-- Shared first steps for new worlds, also shown in the Field Guide. -/
def starterObjective (state : GameState) : String × String :=
  let count := Inventory.count state.inventory
  let lead := (earlyOres state.seed).iron == .leadOre
  let ore := if lead then EarlyMetal.lead else EarlyMetal.iron
  let anvil : Item := if lead then .leadAnvil else .ironAnvil
  let hasAnvil := state.furniture.any (fun entry => entry.kind == .ironAnvil || entry.kind == .leadAnvil)
  let hasFurnace := state.furniture.any (·.kind == .furnace)
  let wood := count .wood
  let upgraded := state.inventory.any (fun stack => stack.any (·.item.pickPower ≥ 40))
  if !state.eyeDefeated && count .suspiciousLookingEye > 0 then
    ("EYE OF CTHULHU", if isNight state.surface.clock state.legacyEarly then
      "Ready a bow. Select the Suspicious Looking Eye and left-click to summon."
    else "The Suspicious Looking Eye calls a boss after dusk. Prepare a clear arena.")
  else if !state.eyeDefeated && count .lens ≥ 6 then
    ("ALTAR RECIPE", "Take 6 Lens to a Demon or Crimson Altar and open Esc > Crafting.")
  else if !state.eyeDefeated && upgraded then
    ("FIRST BOSS", "Hunt Demon Eyes for Lens after dusk. Look for an altar in the eastern caves.")
  else if upgraded || (state.player.centerTile.x ≥ frostCavernStart && state.player.centerTile.y ≥ 54) then
    ("WARDEN AHEAD", "Bring a forged weapon and follow Frost Cavern east into Warden Hall.")
  else if hasAnvil then
    ("FORGE TOOLS", "Open Esc > Crafting near your anvil to forge a pickaxe and sword.")
  else if count .ironAnvil > 0 || count .leadAnvil > 0 then
    ("PLACE ANVIL", "Select the anvil and right-click on clear ground to place it.")
  else if hasFurnace then
    ("FORGE ANVIL", if count ore.bar ≥ 5 then
      s!"Craft a {anvil.name} at a Work Bench, then place it on clear ground."
    else if count ore.ore ≥ ore.oreCost then
      s!"Open Esc > Crafting beside a Furnace. Smelt {ore.ore.name} into bars."
    else s!"Mine {ore.ore.name} along the eastern cave floor for bars and an anvil.")
  else if count .furnace > 0 then
    ("PLACE FURNACE", "Select the Furnace and right-click on clear ground to place it.")
  else if count .workbench > 0 then
    ("PLACE BENCH", "Select the Work Bench and right-click on clear ground to place it.")
  else if wood == 0 then
    ("GATHER WOOD", "Select the axe and hold left-click on the base of a tree.")
  else if !state.nearWorkbench && wood ≥ 10 && state.player.centerTile.x < 29 then
    ("WORK BENCH", "Open Esc > Crafting to make a Work Bench from wood.")
  else if count .torch < 3 && count .caveGel > 0 then
    ("CRAFT TORCHES", "Open Esc > Crafting. Combine Gel and Wood to make torches by hand.")
  else if count .torch < 3 && state.player.centerTile.x < 30 then
    ("GATHER GEL", if isNight state.surface.clock state.legacyEarly then
      "Look for green or blue slimes on the forest surface after dawn."
    else "Look for slimes above ground. Defeat one and collect its Gel.")
  else if count .stone < 20 then
    ("BUILD FURNACE", "Hold left-click with the pickaxe to mine stone for a Furnace.")
  else
    ("BUILD FURNACE", "Open Esc > Crafting near a Work Bench. A Furnace takes stone, wood and torches.")

inductive GuideTopic where
  | help | crafting | housing
  deriving BEq, Inhabited

structure GuideDialogue where
  topic : GuideTopic := .help
  material : Option Item := none
  page : Nat := 0

private def guideTopics : Array (GuideTopic × String) :=
  #[(.help, "Help"), (.crafting, "Crafting"), (.housing, "Housing")]

private def guideButtonY : Nat := 164
private def guideButtonHeight : Nat := 31
private def guideButtonWidth : Nat := 102
private def guideButtonX (index : Nat) : Nat := 646 + index * 110

private def guideButtonAt (x y index : Nat) : Bool :=
  guideButtonY ≤ y && y < guideButtonY + guideButtonHeight &&
    guideButtonX index ≤ x && x < guideButtonX index + guideButtonWidth

def guideCloseAt (x y : Nat) : Bool := guideButtonAt x y guideTopics.size

def oldManCurseAt (x y : Nat) : Bool := containsRect x y 34 258 110 32

def oldManCloseAt (x y : Nat) : Bool := containsRect x y 153 258 110 32

def anglerQuestAt (x y : Nat) : Bool := containsRect x y 34 258 110 32

def drawAnglerDialogue (state : GameState) (mouseX mouseY : Nat) : IO Unit := do
  let quest := state.angler.quest
  discard <| originalSprite "Chat_Back" 20 146 548 153
  text "Angler" 35 158 21 gold
  if state.angler.turnedIn then
    let message := if state.status.startsWith "Quest complete." then state.status
      else "Come back at dawn for another quest."
    text message 35 203 17 paper
  else if state.showAnglerQuest then
    drawItem quest.fish 35 198
    text quest.fish.name 68 192 18 paper
    text quest.location 68 217 15 mint
  else
    text "Want to catch a fish for me?" 35 203 17 paper
  for (label, x, hover) in [
      ("Quest", 34, anglerQuestAt mouseX mouseY),
      ("Close", 153, oldManCloseAt mouseX mouseY)] do
    if hover then rect x 258 110 32 0x7171bb91
    text label (x + 16) 262 19 (if hover then gold else paper)

def drawOldManDialogue (state : GameState) (mouseX mouseY : Nat) : IO Unit := do
  let night := isNight state.surface.clock state.legacyEarly
  discard <| originalSprite "Chat_Back" 20 146 500 153
  text "Old Man" 35 158 21 gold
  if night then
    text "If you wish to enter, you must free me of my curse." 35 197 17 paper
  else
    text "Come back at night if you wish to enter." 35 197 17 paper
  for (label, x, visible, hover) in [
      ("Curse", 34, night, oldManCurseAt mouseX mouseY),
      ("Close", 153, true, oldManCloseAt mouseX mouseY)] do
    if visible then
      if hover then rect x 258 110 32 0x7171bb91
      text label (x + 16) 262 19 (if hover then gold else paper)

private def guideSlotY : Nat := 279
private def guidePageY : Nat := 421
private def guideRecipeLeft : Nat := 584
private def guidePrevX : Nat := guideRecipeLeft + recipeStride
private def guideNextX : Nat := guidePrevX + recipeWidth - classicPagerWidth

private def guideMaterialSlotAt (x y : Nat) : Option Nat := do
  let row ← cellAt y guideSlotY inventoryRows slotSize
  let col ← cellAt x slotLeft Inventory.hotbarCount slotSize
  return row * Inventory.hotbarCount + col

private def guideRecipes (material : Item) : Array ClassicRecipe :=
  ClassicRecipe.all.filter fun recipe => recipe.ingredients.any fun (item, _) =>
    item == material || (material == .leadBar && item == .ironBar && recipe.acceptsLeadBar)

private def guideRecipePages (recipeCount : Nat) : Nat :=
  max 1 ((recipeCount + classicPageSize - 1) / classicPageSize)

def guideDialogueClick (state : GameState) (dialogue : GuideDialogue) (x y : Nat) : GuideDialogue := Id.run do
  for i in [:guideTopics.size] do
    if guideButtonAt x y i then return { dialogue with topic := guideTopics[i]!.1 }
  if dialogue.topic != .crafting then return dialogue
  if let some index := guideMaterialSlotAt x y then
    if let some stack := state.inventory[index]?.join then
      return { dialogue with material := some stack.item, page := 0 }
  if guidePageY ≤ y && y < guidePageY + 25 then
    let pages := guideRecipePages ((dialogue.material.map fun material => (guideRecipes material).size).getD 0)
    if guidePrevX ≤ x && x < guidePrevX + classicPagerWidth && dialogue.page > 0 then
      return { dialogue with page := dialogue.page - 1 }
    if guideNextX ≤ x && x < guideNextX + classicPagerWidth && dialogue.page + 1 < pages then
      return { dialogue with page := dialogue.page + 1 }
  return dialogue

private def drawGuideMaterials (state : GameState) (dialogue : GuideDialogue) (mouseX mouseY : Nat) : IO Unit := do
  let recipes := (dialogue.material.map guideRecipes).getD #[]
  let pages := guideRecipePages recipes.size
  let hovered := guideMaterialSlotAt mouseX mouseY
  text "Pick a material from your items. I'll show you what you can make with it." 33 225 18 paper
  text "YOUR ITEMS" slotLeft 255 14 mint
  if let some index := hovered then
    if let some stack := state.inventory[index]?.join then
      text stack.item.name 160 255 14 paper
  for index in [:Inventory.mainSlotCount] do
    let col := index % Inventory.hotbarCount
    let row := index / Inventory.hotbarCount
    let stack := state.inventory[index]?.join
    let number := if row == 0 then some (col + 1) else none
    drawSlot stack (inventoryX col) (guideSlotY + inventoryStep row) number
      (dialogue.material.isSome && stack.map (·.item) == dialogue.material) (hovered == some index)
  let heading := if let some material := dialogue.material then material.name ++ " recipes" else "Recipes"
  text heading guideRecipeLeft 252 17 gold
  if recipes.isEmpty then
    text (if dialogue.material.isSome then "No listed recipes use this item." else "Choose an item to see matching recipes.")
      guideRecipeLeft (guideSlotY + 14) 15 muted
  for index in [:classicPageSize] do
    let some recipe := recipes[dialogue.page * classicPageSize + index]? | break
    let x := guideRecipeLeft + index % 2 * recipeStride
    let y := guideSlotY + index / 2 * classicRowStride
    drawClassicCard recipe x y false false true
  if pages > 1 then
    for (label, x, enabled) in [("<", guidePrevX, decide (dialogue.page > 0)),
        (">", guideNextX, decide (dialogue.page + 1 < pages))] do
      let active := enabled && x ≤ mouseX && mouseX < x + classicPagerWidth &&
        guidePageY ≤ mouseY && mouseY < guidePageY + 25
      rect x guidePageY classicPagerWidth 25 (if active then 0x3b4c5bff else 0x232e3cff)
      text label (x + 14) (guidePageY + 5) 15 (if enabled then mint else 0x667482ff)
    text s!"PAGE {dialogue.page + 1} / {pages}" (guidePrevX + 81) (guidePageY + 7) 13 paper
  text "To make an item, open Esc > Crafting near the station on its recipe." slotLeft 521 14 muted

private def drawGuideAdvice (title detail : String) (x y : Nat) : IO Unit := do
  rect x y 504 56 0x223141ff
  text title (x + 12) (y + 9) 15 gold
  text detail (x + 12) (y + 32) 13 paper

def drawGuideDialogue (state : GameState) (dialogue : GuideDialogue) (mouseX mouseY : Nat) : IO Unit := do
  rect 0 96 width 490 0x11192777
  discard <| originalSprite "Chat_Back" 20 146 1080 409
  rect 33 158 42 43 0x293c45ff
  drawGuideSprite 45 163 1 false
  text "GUIDE" 86 167 25 paper
  for i in [:guideTopics.size + 1] do
    let entry := guideTopics[i]?
    let label := (entry.map (·.2)).getD "Close"
    let selected := entry.any (·.1 == dialogue.topic)
    let hovering := guideButtonAt mouseX mouseY i
    let x := guideButtonX i
    rect x guideButtonY guideButtonWidth guideButtonHeight
      (if selected then mint else if hovering then 0x415565ff else 0x293848ff)
    text label (x + 10) (guideButtonY + 8) 15 (if selected then backgroundColor else paper)
  rect 33 209 1054 1 0x435763ff
  match dialogue.topic with
  | .help =>
    text "I can help you get started." 33 225 20 mint
    text (starterObjective state).2 33 256 17 paper
    drawGuideAdvice "GATHERING" "Use an axe on trees and a pickaxe on stone or ore." 33 309
    drawGuideAdvice "CRAFTING" "Choose Crafting above to look up recipes by material." 565 309
    drawGuideAdvice "AFTER DARK" "Close your door before zombies and Demon Eyes arrive." 33 377
    drawGuideAdvice "SHELTER" "Choose Housing above to see what a room needs." 565 377
    text "Q or Esc closes the conversation." 33 521 14 muted
  | .crafting => drawGuideMaterials state dialogue mouseX mouseY
  | .housing =>
    text "A sheltered room gives someone a place to live." 33 225 20 mint
    text (if state.guide.isSome then "The Guide has a home." else "The Guide will move into the first suitable vacant room.")
      33 256 17 paper
    drawGuideAdvice "FRAME AND WALLS" "Enclose at least 60 tiles, including the frame. Add background walls." 33 309
    drawGuideAdvice "ENTRANCE" "Use a platform entrance or a three-tile door between floor and roof." 565 309
    drawGuideAdvice "FURNISHINGS" "Add a work bench or table, a chair or bed, and a light." 33 377
    drawGuideAdvice "FLOOR" "Leave a clear place to stand on a solid floor tile." 565 377
    text "Aim inside a room and press H to check it or assign it to the Guide." 33 521 14 muted

private def drawHud (state : GameState) (backpackOpen chestOpen _managed : Bool)
    (hotbarHover : Option Nat) : IO Unit := do
  let inventoryOpen := backpackOpen || chestOpen || state.openShop
  let armed := state.selectedStack.any (fun stack => stack.item.swordDamage > 0)
  let ranged := state.selectedStack.any (isRanged ∘ Stack.item)
  let battle := state.eater.isSome || state.brain.isSome || state.eye.isSome ||
    (match state.cavern, state.raid, state.ruin, state.storm, state.warden with
      | .fighting _, _, _, _, _ | _, .active _, _, _, _ | _, _, .fighting _, _, _
      | _, _, _, .fighting _, _ | _, _, _, _, .fighting _ => true
      | _, _, _, _, _ => false)
  if state.wallOfFlesh.isNone && state.skeletron.isNone && !state.twinsActive && battle then
    rect 20 (hudTop + 18) 697 68 0x111925ec
  if let some body := state.golem then
    let head := (state.golemHead.filter (!·.detached)).map (·.health)
    drawMechanicalBossBar (body.health + head.getD 0) (golemBodyHealth + 25000) "NPC_Head_Boss_5"
  else if let some head := state.plantera then
    let icon := if head.secondForm then 12 else 11
    drawMechanicalBossBar head.health planteraHeadHealth s!"NPC_Head_Boss_{icon}"
  else match state.skeletronPrime, state.destroyer, state.twinsActive, state.wallOfFlesh, state.skeletron, state.eater, state.brain with
  | some head, _, _, _, _, _, _ => drawMechanicalBossBar head.health primeHeadHealth "NPC_Head_Boss_18"
  | none, some boss, _, _, _, _, _ => drawMechanicalBossBar boss.health destroyerHealth "NPC_Head_Boss_25"
  | none, none, true, _, _, _, _ => drawTwinsHud state
  | none, none, false, some boss, _, _, _ => drawWallOfFleshHud boss
  | none, none, false, none, some boss, _, _ => drawSkeletronHud boss
  | none, none, false, none, none, some boss, _ => drawWormHud boss false
  | none, none, false, none, none, none, some boss => drawBrainHud boss false
  | none, none, false, none, none, none, none =>
    if let some boss := state.queenSlime then
      drawCompactBossBar "Queen Slime" .gelatinCrystal boss.health queenSlimeHealth 0xe396f0ff
        (headAsset := "NPC_Head_Boss_38") (top := 632)
    else
      match state.eye, state.cavern, state.raid, state.ruin, state.storm, state.warden with
      | some boss, _, _, _, _, _ => drawEyeHud boss false
      | none, .fighting boss, _, _, _, _ => drawCavernHud boss (armed || ranged)
      | none, _, .active wave, _, _, _ => drawRaidHud wave
      | none, _, _, .fighting boss, _, _ => drawRuinHud boss (armed || ranged)
      | none, _, _, _, .fighting boss, _ => drawStormHud boss
      | none, _, _, _, _, .fighting boss => drawWardenHud boss armed
      | none, _, _, _, _, _ => pure ()
  drawHealth state
  drawMana state.mana
  if state.respawnTicks > 0 then
    text "RESPAWNING" 910 66 15 lifeColor
  else if state.breath < maxBreath || headUnderwater state then
    drawBreath state.breath
  else if Magma.touches state.magma state.world state.player then
    text "LAVA CONTACT" 910 66 15 magmaColor
  else if state.bombs.any (fun bomb => bomb.fuse ≤ 60 && state.canReach ⟨bomb.x / tileUnits, bomb.y / tileUnits⟩) then
    text "LIT BOMB NEARBY" 910 66 15 gold
  drawBuffs state
  let title := if inventoryOpen then "Inventory"
    else (state.selectedStack.map (·.item.name)).getD ""
  text title (slotLeft + 1) (hotbarLabelY + 1) 14 0x0c1427ee
  text title slotLeft hotbarLabelY 14 paper
  for index in [:Inventory.hotbarCount] do
    let stack := state.inventory[index]?.join
    let quantity := if inventoryOpen then none else
      stack.filter (isRanged ∘ Stack.item) |>.map fun weapon =>
        ((ammoForWeapon state weapon.item).map (Inventory.count state.inventory)).getD 0
    drawSlot stack (inventoryX index) hotbarY (some (index + 1)) (index == state.selected)
      (hotbarHover == some index) (skin := if inventoryOpen then .hotbarOpen else .hotbarClosed)
      (quantity := quantity)
  let status := state.status
  if !status.isEmpty && status != "World loaded." && state.skeletron.isNone && state.wallOfFlesh.isNone &&
      !status.startsWith "Select the Copper Axe" && !status.startsWith "Make torches at the workbench" then
    text status 21 (height - 33) 14 0x0c1427ee
    text status 20 (height - 34) 14 paper

private def drawRespawn (ticks : Nat) : IO Unit := do
  rect 397 264 326 98 0x111925f4
  rect 397 264 326 2 lifeColor
  text "YOU DIED" 482 284 29 lifeColor
  text s!"Respawn in {(ticks + 59) / 60}s" 484 328 17 paper

/-- Draw the scene in an open frame and return the number of visible wet cells. -/
def draw (state : GameState) (camera : Camera) (mouseX mouseY : Nat) (backpackOpen : Bool)
    (managed := false) (classicCrafting := false) (craftingPage := 0)
    (hookTeleport : Option (Player × Player × Nat) := none) : IO Nat := do
  let chest := state.openChest.bind state.chestAt
  let chestOpen := chest.isSome
  let inventoryOpen := backpackOpen || chestOpen || state.openShop
  let (wetCells, light) ← drawWorld state camera
  for hit in state.tileHits do drawToolCracks hit camera
  if let .fighting boss := state.cavern then drawCavernTell boss state.world camera
  if let some crawler := state.combat.crawler then drawCrawler crawler camera
  if let some creature := state.surface.creature then drawSurfaceCreature creature camera state.surface.clock
  if let some eye := state.surface.flying then drawSurfaceCreature eye camera state.surface.clock
  if let some boss := state.eye then drawEyeBoss boss state camera
  for twin in state.retinazer.toArray ++ state.spazmatism.toArray do drawTwin twin camera
  if let some queen := state.queenSlime then drawQueenSlime queen camera
  for slime in state.queenMinions do drawQueenMinion slime camera
  for shot in state.queenShots do drawQueenShot shot camera
  if let some boss := state.destroyer then drawDestroyer boss state.world camera light
  for probe in state.destroyerProbes do drawDestroyerProbe probe camera light
  if let some head := state.skeletronPrime then drawSkeletronPrime head state.primeArms camera light
  if let some head := state.plantera then drawPlantera head state.planteraHooks state.planteraTentacles camera light
  if let some body := state.golem then drawGolem body state.golemHead state.golemFists camera light
  for spore in state.planteraSpores do drawPlanteraSpore spore camera light
  if let some boss := state.eater then drawWorm boss state camera
  if let some boss := state.brain then drawBrain boss camera state.surface.clock
  if let some boss := state.skeletron then drawSkeletron boss camera
  if let some wall := state.wallOfFlesh then drawWallOfFlesh wall camera light
  if let some guardian := state.dungeonGuardian then drawDungeonGuardian guardian camera state.surface.clock
  for slime in state.dungeonSlimes do drawDungeonSlime slime camera
  for caster in state.darkCasters do drawDarkCaster caster camera light
  for slime in state.lavaSlimes do drawLavaSlime slime camera
  for imp in state.fireImps do drawFireImp imp camera light
  for flyer in state.underworldFlyers do drawUnderworldFlyer flyer camera light
  for enemy in state.templeEnemies do drawTempleEnemy enemy camera light
  for critter in state.underworldCritters do drawUnderworldCritter critter camera light
  for hungry in state.wallHungry do drawWallHungry hungry camera light state.player state.surface.clock
  for bones in state.angryBones do drawAngryBones bones camera light
  for skull in state.cursedSkulls do drawCursedSkull skull camera
  for wraith in state.wraiths do drawWraith wraith camera light
  for enemy in state.hardmodeCavern do drawCavernEnemy enemy state.world camera light
  if let some oldMan := state.oldManPosition then drawOldMan state oldMan camera
  for enemy in state.evilEnemies do drawEvilEnemy enemy camera state.surface.clock
  for drop in state.gelDrops do drawGelDrop drop camera
  for drop in state.nightDrops do drawNightDrop drop camera state.surface.clock
  for doll in state.voodooDolls do
    let x := doll.x * tilePixels / tileUnits - camera.x
    let y := doll.y * tilePixels / tileUnits - camera.y
    if x + 21 ≥ 0 && x < width && y + 39 ≥ 0 && y < height then
      discard <| originalSprite "Item_267" x y 21 39 (fit := true)
  if let some creature := state.quarry.creature then drawQuarryCrawler creature camera
  for index in [:state.ecology.sites.size] do
    match state.ecology.sites[index]? with
    | some (.alive mob) => drawCaveResident mob camera
    | some (.loot position) =>
      if let some habitat := state.ecology.habitats[index]? then
        drawCaveLoot position habitat.kind camera (state.canReach position)
    | _ => pure ()
  if let some sentry := state.sentry.creature then drawSentry sentry state camera
  if let some guide := state.townGuide then
    let (x, y) := bodyOrigin guide.body camera
    let walking := guide.body.vx != 0 && state.surface.clock / 8 % 2 == 0
    drawGuide x y guide.direction walking (state.canTalkToGuide guide.body.centerTile)
      guide.health (guide.attackCooldown + 9 > guideAttackCooldown)
  else if state.legacyEarly then
    if let some guide := state.guide then
      let (x, y) := npcOrigin guide camera
      drawGuide x y 1 false (state.canReach guide)
  if let some merchant := state.merchant then drawMerchant merchant camera (state.canReach merchant)
  if let some position := state.angler.position then
    let (x, y) := npcOrigin position camera
    if x + 48 ≥ 0 && x < width && y + 48 ≥ 0 && y < height then
      let awake := state.angler.rescued
      let sprite := if awake then "NPC_369" else "NPC_376"
      let spriteY := if awake then y else y + 11
      let spriteX := if awake then x - 1 else x - 4
      discard <| originalSprite sprite spriteX spriteY (if awake then 30 else 36) (if awake then 32 else 22)
          0 (if awake then 12 else 6) (if awake then 40 else 44) (if awake then 42 else 28)
      if state.canTalkToAngler position then
        text (if awake then "Angler" else "Sleeping Angler") (x - 27) (y - 29) 13 paper
  match state.warden with
  | .fighting boss => drawWarden boss camera
  | .reward position => drawReward position .frostbrand camera
  | _ => pure ()
  match state.storm with
  | .fighting boss => drawStormHerald boss state camera
  | .reward position => drawReward position .stormBow camera
  | _ => pure ()
  match state.ruin with
  | .fighting boss => drawRuinSentinel boss camera
  | .reward position => drawReward position .ruinHammer camera
  | _ => pure ()
  match state.cavern with
  | .fighting boss => drawDeepstoneColossus boss camera
  | .reward position => drawReward position .deepstoneAegis camera
  | _ => pure ()
  match state.raid with
  | .active wave =>
    for foe in wave.foes do drawRaider foe state camera
  | .reward position => drawReward position .raidCrest camera
  | _ => pure ()
  for rail in state.carts do drawCartBody camera (cartBody rail) false
  drawGrapple state camera
  let travel := (state.ride.map (·.speed)).getD 0
  let facesLeft := if travel == 0 then state.combat.facing < 0 else travel < 0
  if state.respawnTicks == 0 then
    drawPlayer state camera facesLeft
  if let some dagger := state.dagger then drawDagger dagger.blade camera
  drawMinionTarget state camera
    if let some thrust := state.shortsword then drawShortsword state thrust camera light
  if let some (start, finish, age) := hookTeleport then
    drawDissonanceDust camera start age
    drawDissonanceDust camera finish age
  if let some ride := state.ride then
    drawCartBody camera ride.body true (ride.speed != 0 && state.surface.clock / 3 % 2 == 0)
  for shot in state.projectiles do drawProjectile shot camera
  for sphere in state.waterSpheres do drawWaterSphere sphere camera state.surface.clock
  for sphere in state.burningSpheres do drawBurningSphere sphere camera state.surface.clock
  for stream in state.aquaStreams do drawAquaStream stream camera light
  if let some attack := state.blueMoon then drawFlail state .blueMoon attack.flail camera light
  if let some attack := state.sunfury then drawFlail state .sunfury attack.flail camera light
  if let some lance := state.darkLance then drawDarkLance state lance camera light
  for attack in state.magicMissiles do drawMagicMissile attack.missile camera state.surface.clock
  for (missile, ticks) in state.magicMissileFlashes do
    drawMagicMissileImpact missile camera state.surface.clock ticks
  for shot in state.flowerFireballs do drawFlowerFireball shot camera
  for shot in state.hellwingShots do drawHellwingShot shot camera
  for shot in state.demonSickles do drawDemonSickle shot camera
  for shot in state.wallLasers do drawWallLaser shot camera
  for shot in state.cavernFlames do drawCavernFlame shot camera
  for shot in state.twinShots do drawTwinShot shot camera
  for shot in state.primeShots do drawPrimeShot shot camera
  for shot in state.planteraShots do drawPlanteraShot shot camera
  for shot in state.golemShots do drawGolemShot shot camera
  for shot in state.templeTrapShots do drawTempleTrapShot shot camera
  for attack in state.flamelashes do drawFlamelash attack.projectile camera state.surface.clock
  for (projectile, ticks) in state.flamelashFlashes do
    drawFlamelashImpact projectile camera state.surface.clock ticks
  if let some attack := state.valor then drawValor state.player attack.yoyo camera light
  for bomb in state.bombs do drawActiveBomb bomb camera
  if let some (position, ticks) := state.bombFlash then drawBombFlash position ticks camera
  drawRain state camera
  if state.respawnTicks > 0 then drawRespawn state.respawnTicks
  else if !state.openGuide && !state.openOldMan then
    let cursed := state.cursedTicks > 0
    let burning := state.burningTicks > 0
    let darkness := state.darknessTicks > 0
    let skin := state.obsidianSkinTicks > 0
    let buffHover := (cursed && cursedBuffAt inventoryOpen mouseX mouseY) ||
      (burning && burningBuffAt inventoryOpen cursed mouseX mouseY) ||
      (darkness && darknessBuffAt inventoryOpen cursed burning mouseX mouseY) ||
      (skin && obsidianSkinBuffAt inventoryOpen cursed burning darkness mouseX mouseY) ||
      (state.horrified && horrifiedBuffAt inventoryOpen cursed burning darkness skin mouseX mouseY) ||
      (state.tongued && tonguedBuffAt inventoryOpen cursed burning darkness skin state.horrified mouseX mouseY) ||
      (state.slowTicks > 0 && slowBuffAt inventoryOpen cursed burning darkness skin state.horrified state.tongued mouseX mouseY) ||
      cavernBuffAt state inventoryOpen mouseX mouseY || templeBuffAt state inventoryOpen mouseX mouseY
    let blocked := chestOpen || state.openShop || buffHover || (backpackOpen && inventoryUiAt mouseX mouseY classicCrafting)
    if let some position := camera.target mouseX mouseY blocked then drawCursor state camera position
  let hotbarHover := hotbarSlotAt mouseX mouseY
  let backpackHover := backpackSlotAt mouseX mouseY
  let chestHover := chestSlotAt mouseX mouseY
  let gearHover := gearSlotAt mouseX mouseY
  drawHud state backpackOpen chestOpen managed hotbarHover
  if inventoryOpen then
    drawBackpack state backpackHover
    drawSaveQuit mouseX mouseY
  if let some chest := chest then drawChest state.world chest chestHover
  if state.openShop then drawShop state (shopOfferAt mouseX mouseY)
  if backpackOpen && !chestOpen && !state.openShop then
    drawBackpackTabs classicCrafting mouseX mouseY
    if classicCrafting then drawClassicCrafting state craftingPage mouseX mouseY
    drawEquipment state gearHover
  drawCursedBuff state.cursedTicks inventoryOpen mouseX mouseY state.cursorStack.isNone
  drawPoisonBuff state inventoryOpen mouseX mouseY
  drawTempleBuffs state inventoryOpen mouseX mouseY
  drawBurningBuff state inventoryOpen mouseX mouseY
  drawDarknessBuff state inventoryOpen mouseX mouseY
  drawObsidianSkinBuff state inventoryOpen mouseX mouseY
  drawWallBuffs state inventoryOpen mouseX mouseY
  drawSlowBuff state inventoryOpen mouseX mouseY
  drawCavernBuffs state inventoryOpen mouseX mouseY
  if state.cratePotion > 0 then
    let (x, y) := crateBuffOrigin state inventoryOpen
    discard <| originalSprite "Buff_123" x y 32 32
    let seconds := (state.cratePotion + 59) / 60
    text (if seconds > 60 then s!"{(seconds + 59) / 60} m" else s!"{seconds} s") x (y + 32) 11 paper
    if state.cursorStack.isNone && crateBuffAt state inventoryOpen mouseX mouseY then
      drawInventoryHover "Crate / Greater chance of fishing up a crate" mouseX mouseY
  if state.wingedSlime.isSome then
    let (x, y) := mountBuffOrigin state inventoryOpen
    discard <| originalSprite "Buff_318" x y 32 32
    if state.cursorStack.isNone && mountBuffAt state inventoryOpen mouseX mouseY then
      drawInventoryHover "Winged Slime Mount / BOING FLAP BOING!" mouseX mouseY
  if state.dagger.isSome then
    let (x, y) := daggerBuffOrigin state inventoryOpen
    discard <| originalSprite "Buff_271" x y 32 32
    if state.cursorStack.isNone && daggerBuffAt state inventoryOpen mouseX mouseY then
      drawInventoryHover "Enchanted Daggers / Death by a thousand cuts" mouseX mouseY
  for (ticks, asset, x) in [(state.fishingPotion, "Buff_121", 610), (state.sonarPotion, "Buff_122", 648),
      (state.honeyBuff, "Buff_48", 686)] do
    if ticks > 0 then
      discard <| originalSprite asset x 76 32 32
      let seconds := (ticks + 59) / 60
      text (if seconds > 60 then s!"{(seconds + 59) / 60} m" else s!"{seconds} s") x 108 11 paper
  if state.sonarPotion > 0 then
    if let .bobber motion (.hooked _ fish _) := state.fishing then
      let x := npcScreenPixel motion.centerX camera.x
      let y := npcScreenPixel motion.centerY camera.y
      if 0 ≤ x && x < width && 45 ≤ y && y < height then
        text fish.name (x.toNat - fish.name.length * 4) (y.toNat - 28) 16 mint
  if let some stack := state.cursorStack then
    drawHeldStack stack mouseX mouseY
  else if containsRect mouseX mouseY (manaStarX - 11) 30 22 24 then
    drawInventoryHover s!"Mana: {state.mana}/{baseMana}" mouseX mouseY
  else if inventoryOpen then
    if let some slot := hotbarHover.orElse (fun _ => backpackHover) then
      let stack := state.inventory[slot]?.join
      let label := match stack with
        | some entry =>
          if state.openShop then
            let price := entry.count * sellPrice entry.item
            if price > 0 then s!"{entry.item.name} / {price} copper"
            else s!"{entry.item.name} / Cannot sell"
          else if entry.item == .goldenLockBox then "Golden Lock Box / Requires a Golden Key"
          else if entry.item == .obsidianLockBox then "Obsidian Lock Box / Requires a Shadow Key"
          else if entry.item.isFishingCrate then stackLabel stack state.legacyEarly ++ " / Right click to open"
          else stackLabel stack state.legacyEarly
        | none =>
          if slot == Inventory.trashSlot then "Trash"
          else if Inventory.ammoStart ≤ slot then "Ammo"
          else if Inventory.coinStart ≤ slot then "Coins"
          else ""
      drawInventoryHover label mouseX mouseY
    else if let some stack := chest.bind (fun store => chestHover.bind fun slot => store.slots[slot]?.join) then
      drawInventoryHover (stackLabel (some stack) state.legacyEarly) mouseX mouseY
    else if backpackOpen && !chestOpen && !state.openShop then
      if let some slot := gearHover then
        let item := state.equipment.get slot
        if let some equipped := item then
          let label := stackLabel (some ⟨equipped, 1⟩) state.legacyEarly
          drawInventoryHover (label ++ " / Click to unequip") mouseX mouseY
  return wetCells

private def chartUnknown : UInt32 := 0x000000ff
private def chartBench : UInt32 := 0xf1b079ff
private def chartAltar : UInt32 := 0xe9b0e9ff
private def chartEmptyCache : UInt32 := 0xa3a9b9ff

private def chartColor (position : TilePos) : Tile → UInt32
  | .air =>
    if position.y ≥ underworldStart then 0x5c2f40ff
    else if inAmberBluffs position then 0x695244ff
    else if inGeodeGrotto position then 0x55466fff
    else if inHighlands position then 0x415c51ff
    else if position.y < 47 then 0x547cabff else if position.y < 83 then 0x382819ff else 0x191519ff
  | .stone | .dirt =>
    if inAmberBluffs position then 0xa57953ff
    else if inGeodeGrotto position then 0x80689bff
    else if inHighlands position then 0x71846bff
    else 0x526170ff
  | .bedrock => 0x373c52ff
  | .copper => 0xc08163ff
  | .tinOre => metalOreColor .tinOre
  | .leadOre => metalOreColor .leadOre
  | .tungstenOre => metalOreColor .tungstenOre
  | .goldOre => metalOreColor .goldOre
  | .platinumOre => metalOreColor .platinumOre
  | .crystal => 0x88dccfff
  | .lifeCrystal => lifeColor
  | .crystalShard | .gelatinCrystal => shardColor
  | .highlandBerry => berryColor
  | .iron => 0x8fabc0ff
  | .silver => 0xd4e7ecff
  | .deepsteel => deepsteelColor
  | .obsidian => obsidianColor
  | .ash => 0x85717cff
  | .obsidianBrick => 0x685373ff
  | .demoniteBrick => 0x73548dff
  | .crimtaneBrick => 0x985568ff
  | .hallowedGrass => 0x73bfc9ff
  | .pearlstone => 0xb4a4caff
  | .pearlsand => 0xd3c1d8ff
  | .ebonsand => 0x887298ff
  | .crimsand => 0xb67064ff
  | .pinkIce => 0xd185b9ff
  | .purpleIce => 0x966bc5ff
  | .redIce => 0xc05b69ff
  | .cobaltOre => 0x53a4dfff
  | .palladiumOre => 0xeb985cff
  | .mythrilOre => 0x6db89eff
  | .orichalcumOre => 0xd97cc4ff
  | .adamantiteOre => 0xe36574ff
  | .titaniumOre => 0xaca8cfff
  | .hellstone => magmaColor
  | .shadowChest => shadowChestColor
  | .shadowOpenChest => chartEmptyCache
  | .demoniteOre | .corruptGrass => demoniteColor
  | .crimtaneOre | .crimsonGrass => crimtaneColor
  | .ebonstone => 0x735783ff
  | .crimstone => 0x8f454fff
  | .blueDungeonBrick => dungeonColor .blueDungeonBrick
  | .greenDungeonBrick => dungeonColor .greenDungeonBrick
  | .pinkDungeonBrick => dungeonColor .pinkDungeonBrick
  | .dungeonKeyChest => 0xad8156ff
  | .dungeonGoldChest => gold
  | .dungeonOpenChest => chartEmptyCache
  | .shadowOrb => orbColor
  | .crimsonHeart => crimsonHeartColor
  | .clayPot => 0xac755eff
  | .vileMushroom => orbColor
  | .viciousMushroom => crimsonHeartColor
  | .sand => 0xbba279ff
  | .mud => 0x62473aff
  | .jungleGrass => 0x538b36ff
  | .planteraBulb => 0xde66b9ff
  | .lihzahrdBrick => 0x8d3800ff
  | .lihzahrdChest => 0xae815cff
  | .lihzahrdAltar | .lihzahrdAltarUpperLeft | .lihzahrdAltarUpperMiddle | .lihzahrdAltarUpperRight
  | .lihzahrdAltarLowerLeft | .lihzahrdAltarLowerMiddle | .lihzahrdAltarLowerRight => 0xfff133ff
  | .lihzahrdDartTrap | .lihzahrdFlameTrap | .lihzahrdSpikyBallTrap | .lihzahrdSpearTrap => 0x8d3800ff
  | .lihzahrdPressurePlate => 0x716258ff
  | .woodenSpikes => 0x805d3aff
  | .lihzahrdDoorLocked0 | .lihzahrdDoorLocked1 | .lihzahrdDoorLocked2
  | .lihzahrdDoorClosed0 | .lihzahrdDoorClosed1 | .lihzahrdDoorClosed2
  | .lihzahrdDoorRight | .lihzahrdDoorRightLeaf | .lihzahrdDoorLeft | .lihzahrdDoorLeftLeaf => 0x77694fff
  | .honeyBlock => 0xca872aff
  | .crispyHoneyBlock => 0x9b5b34ff
  | .glassBlock => 0x87b2b8ff
  | .placedBottle => chartBench
  | .bed => bedroomColor
  | .furnace | .hellforge | .adamantiteForge | .titaniumForge |
    .ironAnvil | .leadAnvil | .mythrilAnvil | .orichalcumAnvil | .sawmill | .loom => chartBench
  | .cobweb => 0x65738aff
  | .amber => amberColor
  | .geode => geodeColor
  | .ice => 0x5480a5ff
  | .workbench => chartBench
  | .stormAltar => chartAltar
  | .demonAltar => demoniteColor
  | .crimsonAltar => crimtaneColor
  | .ruinAltar => ruinRune
  | .mineMarker => deepsteelColor
  | .cavernAltar => cavernColor
  | .magmaMarker => magmaColor
  | .ruinBrick => 0x896c54ff
  | .timber | .chest | .fishingCrate | .woodChair | .woodTable | .doorClosed | .doorClosedTop | .doorClosedMiddle |
    .doorOpen | .doorOpenTop | .doorOpenMiddle => 0x9d745aff
  | .treeTrunk => 0x7a9666ff
  | .platform => 0xcc9961ff
  | .rail => railColor
  | .homeBeacon => 0xa5a9b1ff
  | .supplyCache => supplyColor
  | .openedCache => chartEmptyCache
  | .sunleaf => 0x9fbd68ff
  | .sunleafSprout => 0x78a363ff
  | .torch => 0xc19a59ff

private def mapTooltip (label : String) (mouseX mouseY : Nat) : IO Unit := do
  let extent ← textWidth label 17
  let x := min (mouseX + 16) (width - extent - 3)
  let y := min (mouseY + 16) (height - 24)
  text label (x + 1) (y + 1) 17 chartUnknown
  text label x y 17 0xffffffff

/-- Use the pixel bounds of the original 32×30 close-map button. -/
def closeMapAt (mouseX mouseY : Nat) : Bool :=
  containsRect mouseX mouseY 10 (height - 40) 32 30

def mapMinScale (world : World) : Float :=
  width.toFloat / world.width.toFloat * 0.599

inductive MinimapStyle where
  | corner | overlay | hidden
  deriving BEq

def MinimapStyle.next : MinimapStyle → MinimapStyle
  | .corner => .overlay
  | .overlay => .hidden
  | .hidden => .corner

private def minimapWidth : Nat := 240
private def minimapX (backpackOpen : Bool) : Nat :=
  if backpackOpen then 800 else width - 20 - minimapWidth
private def minimapY : Nat := 113
def minimapDefaultScale : Float := 1.05

inductive MinimapAction where
  | full | zoomIn | zoomOut | reset | style (value : MinimapStyle)
  deriving BEq

private def minimapControls : List (Nat × MinimapAction) :=
  [(2, .style .corner), (1, .style .overlay), (0, .style .hidden), (3, .full)]

private def minimapButtons : List (String × Nat × MinimapAction) :=
  [("MinimapButton_Reset", 142, .reset), ("MinimapButton_ZoomOut", 168, .zoomOut),
   ("MinimapButton_ZoomIn", 194, .zoomIn)]

def minimapControlAt (mouseX mouseY : Nat) : Option MinimapAction :=
  minimapControls.zipIdx.findSome? fun ((_, action), index) =>
    if containsRect mouseX mouseY (960 + index * 34) 73 32 30 then some action else none

def minimapAt (backpackOpen : Bool) (mouseX mouseY : Nat) : Option MinimapAction :=
  let left := minimapX backpackOpen
  (minimapButtons.findSome? fun (_, dx, action) =>
    if containsRect mouseX mouseY (left + dx) (minimapY + 225) 18 18 then some action else none).orElse fun _ =>
      if containsRect mouseX mouseY left minimapY minimapWidth minimapWidth then some .full else none

private def mapPosition (camera : MapCamera) (midX midY : Int) (x y : Float) : Int × Int :=
  (midX + ((x - camera.x) * camera.scale).floor.toInt64.toInt,
   midY + ((y - camera.y) * camera.scale).floor.toInt64.toInt)

private def shadeMap (color : UInt32) (alpha : UInt32) : UInt32 :=
  (color &&& 0xffffff00) ||| alpha

private def mappedColor (state : GameState) (position : TilePos) : UInt32 :=
  match state.chart.tile? state.world position with
  | none => chartUnknown
  | some .air =>
    if state.magma.level state.world position > 0 then 0xfd2003ff
    else if state.honey.level state.world position > 0 then 0xfeb619ff
    else if state.water.level state.world position > 0 then 0x095defff
    else chartColor position .air
  | some tile => chartColor position tile

private def drawMapTiles (state : GameState) (camera : MapCamera)
    (midX midY : Int) (halfWidth halfHeight : Nat) (fog : Bool) (alpha : UInt32) : IO Unit := do
  let world := state.world
  let firstCol := (camera.x - halfWidth.toFloat / camera.scale).floor.toInt64.toInt.toNat
  let firstRow := (camera.y - halfHeight.toFloat / camera.scale).floor.toInt64.toInt.toNat
  let endCol := min world.width ((camera.x + halfWidth.toFloat / camera.scale).ceil.toInt64.toInt.toNat + 1)
  let endRow := min world.height ((camera.y + halfHeight.toFloat / camera.scale).ceil.toInt64.toInt.toNat + 1)
  for row in [firstRow:endRow] do
    let (_, top) := mapPosition camera midX midY 0 row.toFloat
    let (_, bottom) := mapPosition camera midX midY 0 (row + 1).toFloat
    if top == bottom then continue
    let mut start := firstCol
    let mut color := mappedColor state ⟨firstCol, row⟩
    for col in [firstCol + 1:endCol + 1] do
      let next := if col == endCol then chartUnknown else mappedColor state ⟨col, row⟩
      if col == endCol || next != color then
        if fog || color != chartUnknown then
          let (left, _) := mapPosition camera midX midY start.toFloat 0
          let (right, _) := mapPosition camera midX midY col.toFloat 0
          if right > left then
            rect left top (right - left).toNat (bottom - top).toNat (shadeMap color alpha)
        start := col
        color := next

private def drawMapHead (state : GameState) (x y : Int) : IO Unit := do
  let left := state.combat.facing < 0
  let layer (asset : String) (color : UInt32) :=
    discard <| originalSprite asset (x - 12) (y - 12) 24 24 8 3 24 24 (mirror := left) (color := color)
  layer "Player_0_0" 0xf7c392ff
  layer "Player_0_1" 0xfff8e4ff
  layer "Player_0_2" 0x5d5e75ff
  if state.equipment.head == some .crystalAssassinHood then
    layer "Armor_Head_261" 0xffffffff
  else layer "Player_Hair_1" 0xd59450ff

private def drawMapIcons (state : GameState) (camera : MapCamera) (midX midY : Int)
    (mouseX mouseY : Nat) (showHover : Bool) (pings : Array TilePos) : IO (Option String) := do
  let pixel (pos : TilePos) := mapPosition camera midX midY (pos.x.toFloat + 0.5) (pos.y.toFloat + 0.5)
  let hovered (x y : Int) := showHover &&
    ((mouseX : Int) - x).natAbs ≤ 11 && ((mouseY : Int) - y).natAbs ≤ 11
  let mut hover := none
  for (position, asset, label) in [
      (state.bedSpawnPoint, "UI/SpawnBed", "Spawn Point"),
      (state.homeAnchor, "Item_4876", "Home Beacon"),
      (state.townGuide.map (·.body.centerTile), "NPC_Head_1", "Guide"),
      (state.merchantDoor, "NPC_Head_2", "Merchant")] do
    if let some pos := position then
      if (state.chart.tile? state.world pos).isSome then
        let (x, y) := pixel pos
        discard <| originalSprite asset (x - 9) (y - 9) 18 18 (fit := true)
        if hovered x y then hover := some label
  if let some head := state.golemHead then
    let (x, y) := mapPosition camera midX midY (head.centerX.toFloat / 16) (head.centerY.toFloat / 16)
    discard <| originalSprite "NPC_Head_Boss_5" (x - 11) (y - 11) 22 22 (fit := true)
    if hovered x y then hover := some "Golem"
  for pos in pings do
    let (x, y) := pixel pos
    discard <| originalSprite "UI/MapPing" (x - 9) (y - 9) 18 18
      0 (18 * (state.surface.clock / 6 % 5)) 18 18
  if state.respawnTicks == 0 then
    let x := (bodyCenterX state.player).toFloat / tileUnits.toFloat
    let y := (bodyCenterY state.player).toFloat / tileUnits.toFloat
    let (sx, sy) := mapPosition camera midX midY x y
    drawMapHead state sx sy
    if hovered sx sy then hover := some "Player"
  return hover

private def mapTileLabel : Tile → String
  | .planteraBulb => "Plantera's Bulb"
  | .lihzahrdBrick => "Lihzahrd Brick"
  | .lihzahrdChest => "Lihzahrd Chest"
  | .lihzahrdAltar | .lihzahrdAltarUpperLeft | .lihzahrdAltarUpperMiddle | .lihzahrdAltarUpperRight
  | .lihzahrdAltarLowerLeft | .lihzahrdAltarLowerMiddle | .lihzahrdAltarLowerRight => "Lihzahrd Altar"
  | .lihzahrdPressurePlate => "Lihzahrd Pressure Plate"
  | .lihzahrdDartTrap => "Super Dart Trap"
  | .lihzahrdFlameTrap => "Flame Trap"
  | .lihzahrdSpikyBallTrap => "Spiky Ball Trap"
  | .lihzahrdSpearTrap => "Spear Trap"
  | .woodenSpikes => "Wooden Spike"
  | .lihzahrdDoorLocked0 | .lihzahrdDoorLocked1 | .lihzahrdDoorLocked2
  | .lihzahrdDoorClosed0 | .lihzahrdDoorClosed1 | .lihzahrdDoorClosed2
  | .lihzahrdDoorRight | .lihzahrdDoorRightLeaf | .lihzahrdDoorLeft | .lihzahrdDoorLeftLeaf => "Lihzahrd Door"
  | .chest | .supplyCache | .openedCache => "Chest"
  | .dungeonKeyChest | .dungeonGoldChest | .dungeonOpenChest => "Gold Chest"
  | .shadowChest | .shadowOpenChest => "Shadow Chest"
  | .demonAltar => "Demon Altar"
  | .crimsonAltar => "Crimson Altar"
  | .shadowOrb => "Shadow Orb"
  | .crimsonHeart => "Crimson Heart"
  | .lifeCrystal => "Life Crystal"
  | .gelatinCrystal => "Gelatin Crystal"
  | .homeBeacon => "Home Beacon"
  | .workbench => "Work Bench"
  | .bed => "Bed"
  | .mineMarker => "Deepsteel Mine"
  | .ruinAltar => "Ruin Altar"
  | .cavernAltar => "Deepstone Altar"
  | .magmaMarker => "Magma Gallery"
  | _ => ""

private def mapBackground (state : GameState) : String × UInt32 := Id.run do
  let pos := state.player.centerTile
  if pos.y ≥ underworldStart then return ("MapBG3", 0xffffffff)
  if inDungeon state.seed state.world pos state.lowerDungeonInstalled state.blueMoonDungeonInstalled
      state.magicMissileDungeonInstalled state.deepDungeonInstalled then return ("MapBG5", 0xffffffff)
  let underground := (groundHeight state.seed pos.x.toNat : Int) + 7 < pos.y
  let surfaceTint := if isNight state.surface.clock state.legacyEarly then 0x333853ff else 0x93a7e0ff
  let tint := if underground then 0xffffffff else surfaceTint
  let biome := (hardmodeBiome state).orElse fun _ =>
    if !inEvilBiome state.seed pos then none
    else some (if altarTile state.seed == .crimsonAltar then .crimson else .corruption)
  let asset := match biome with
    | some .corruption => if underground then "MapBG23" else "MapBG6"
    | some .crimson => if underground then "MapBG24" else "MapBG7"
    | some .hallow => if underground then "MapBG22" else "MapBG8"
    | none => if pos.y > 83 then "MapBG32" else if underground then "MapBG2" else "MapBG1"
  return (asset, tint)

def drawMap (state : GameState) (camera : MapCamera) (mouseX mouseY : Nat) (pings : Array TilePos) : IO Unit := do
  let midX := width / 2
  let midY := height / 2
  let (asset, tint) := mapBackground state
  discard <| originalSprite asset 0 0 width height (color := tint)
  drawMapTiles state camera midX midY midX midY true 255
  let hover ← drawMapIcons state camera midX midY mouseX mouseY true pings
  let close := closeMapAt mouseX mouseY
  discard <| originalSprite (if close then "Map_4" else "Map_0") 10 (height - 40) 32 30
    (color := if close then 0xffffffff else 0xffffff82)
  let tile := camera.tileAt ((mouseX : Int) - midX) ((mouseY : Int) - midY)
  let label := if close then "Close Map" else hover.getD
    ((state.chart.tile? state.world tile).map mapTileLabel |>.getD "")
  if !label.isEmpty then mapTooltip label mouseX mouseY

def drawMinimap (state : GameState) (style : MinimapStyle) (scale : Float) (alpha : UInt32) (backpackOpen : Bool)
    (mouseX mouseY : Nat) (pings : Array TilePos) : IO Unit := do
  let camera := MapCamera.atTile state.player.centerTile scale
  if style == .overlay && !backpackOpen then
    drawMapTiles state camera (width / 2) (height / 2) (width / 2) (height / 2) false alpha
    discard <| drawMapIcons state camera (width / 2) (height / 2) mouseX mouseY false pings
  if style == .corner then
    let left := minimapX backpackOpen
    let midX := left + minimapWidth / 2
    let midY := minimapY + minimapWidth / 2
    rect left minimapY minimapWidth minimapWidth (shadeMap chartUnknown alpha)
    withClip left minimapY minimapWidth minimapWidth do
      drawMapTiles state camera midX midY (minimapWidth / 2) (minimapWidth / 2) false alpha
      discard <| drawMapIcons state camera midX midY mouseX mouseY false pings
    let assets := "UI/Minimap/Default/"
    discard <| originalSprite (assets ++ "MinimapFrame") (left - 8) (minimapY - 15) 256 264
    for (asset, dx, action) in minimapButtons do
      let tint := if minimapAt backpackOpen mouseX mouseY == some action then 0xffffffff else 0xc3c3c3ff
      discard <| originalSprite (assets ++ asset) (left + dx) (minimapY + 225) 18 18 (color := tint)
  if backpackOpen then
    for ((icon, action), index) in minimapControls.zipIdx do
      let hover := minimapControlAt mouseX mouseY == some action
      let tint := if hover || action == .style style then 0xffffffff else 0xffffffad
      discard <| originalSprite s!"Map_{icon + if hover then 4 else 0}" (960 + index * 34) 73 32 30 (color := tint)

end Lean4Terria.View
