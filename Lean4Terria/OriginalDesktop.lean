import Lean4Terria.Desktop
import Lean4Terria.OriginalCrafting
import Lean4Terria.OriginalCursorItemIcon
import Lean4Terria.OriginalMouseCursor
import Lean4Terria.OriginalBasicDustDraw
import Lean4Terria.OriginalDesktopDialogue
import Lean4Terria.OriginalDesktopGuideGrid
import Lean4Terria.OriginalDesktopLiquid
import Lean4Terria.OriginalDesktopRename
import Lean4Terria.OriginalDesktopSign
import Lean4Terria.OriginalDesktopWaterfall
import Lean4Terria.OriginalForestGuideCrafting
import Lean4Terria.OriginalForestGuideCraftGridCatalog
import Lean4Terria.OriginalForestNightSky
import Lean4Terria.OriginalForestSky
import Lean4Terria.OriginalForestSkyLighting
import Lean4Terria.OriginalFraming
import Lean4Terria.OriginalInventoryItemDrawing
import Lean4Terria.OriginalPlayerFile
import Lean4Terria.OriginalPlayerBuffBar
import Lean4Terria.OriginalResidentText
import Lean4Terria.OriginalSession
-- BEGIN B-ZONE playerdraw-import (S05)
import Lean4Terria.OriginalPlayerDrawRuntime
-- END B-ZONE playerdraw-import
import Lean4Terria.OriginalSkeletronPresentation
import Lean4Terria.OriginalWorldExport
import Lean.Data.Json
import Std.Time.Zoned

namespace Lean4Terria.OriginalDesktop

open Desktop
open OriginalFraming (FramePatch terrainFrame)
open OriginalItems (copperAxe copperPickaxe)
open OriginalMining (Cell stableTileForItem)
open OriginalPlayerFile (Appearance)
open OriginalRecipeCatalog (catalog)
open OriginalWorld (Archive Chest Terrain)

@[extern "terria_original_input"] private opaque originalInput : IO UInt64
@[extern "terria_original_cursor_layer_handled"] private opaque cursorLayerHandled : IO Unit
@[extern "terria_original_pointer_position"] private opaque pointerPosition : IO UInt64
@[extern "terria_original_mana_refill_sound"] private opaque playManaRefillSound : IO Unit
@[extern "terria_original_audio_supported"] private opaque audioSupported : IO Bool
@[extern "terria_original_tile_hit_sound"] private opaque playTileHitSound
    (typeId style variation : UInt32) (volume pan pitch : Float32) : IO Unit
/-- Bit `i` is set while the number key of hotbar slot `i` is down (`Triggers.Current.HotbarN`). -/
@[extern "terria_hotbar_held"] private opaque hotbarHeld : IO UInt32
@[extern "terria_dungeon_music"] private opaque dungeonMusic
  (dungeonPlaying : Bool) (dungeonGain : Float32) (skeletronPlaying : Bool) (skeletronGain : Float32) : IO Unit
@[extern "terria_bucket_splash_ready"] private opaque bucketSplashReady : IO Bool
@[extern "terria_bucket_splash"] private opaque playBucketSplash (pitch volume pan : Float) : IO Unit
@[extern "terria_death_text"] private opaque deathText (label : @& String) (center y : UInt32)
    (scale : Float) (color : UInt32) : IO Unit
@[extern "terria_original_posed_sprite"] private opaque posedSprite (asset : @& String)
    (sourceY width height color : UInt32) (x y pivotX pivotY scale rotation : Float) (mirror : Bool) : IO Unit
@[extern "terria_original_posed_sprite_region"] private opaque posedSpriteRegion (asset : @& String)
    (sourceX sourceY width height color : UInt32)
    (x y pivotX pivotY scale rotation : Float) (mirror : Bool) : IO Unit
-- BEGIN B-ZONE playerdraw-texture-size (S05)
/-- `(width, height)` of a texture as the native asset loader actually loaded it (`width | height << 32`,
0 when it did not load). -/
@[extern "terria_original_loaded_texture_size"] private opaque loadedTextureSize (asset : @& String) : IO UInt64
-- END B-ZONE playerdraw-texture-size
@[extern "terria_original_death_templates"] private opaque originalDeathTemplates : IO String
@[extern "terria_original_local_death_date"] private opaque localDeathDate : IO String
@[extern "terria_original_assembly_path"] private opaque originalAssemblyPath : IO String

private def parseDeathTemplates (rows : String) : Except String (Array String) := do
  let mut templates := #[]
  for row in rows.splitOn "\n" do
    let row := row.trimAscii.toString
    if row.isEmpty then continue
    let row := if row.endsWith "," then (row.dropEnd 1).toString else row
    let entry ← (← Lean.Json.parse ("{" ++ row ++ "}")).getObj?
    let [(_, value)] := entry.toList | throw "Invalid original death-text row"
    templates := templates.push (← value.getStr?)
  return templates

private def screenWidth : Nat := 1120
private def screenHeight : Nat := 704
private def tickSeconds : Float := 1 / 60
private def hotbarSize : Nat := 10
private def slotSize : Nat := 52
private def slotStride : Nat := 56
private def inventoryInset : Nat := 20
private def craftTop : Nat := inventoryInset + 5 * slotStride + 32
private def craftSlots : Nat := (screenHeight - craftTop) / slotStride
private def guideSlotLeft : Nat := 73
private def guideSlotTop : Nat := 331 + (screenHeight - 600) / 2
private def guideSlotSize : Nat := 39
private def chestLeft : Nat := 73
private def chestRows : Nat := 4
private def chestSlotSize : Nat := 39
private def chestStep (index : Nat) : Nat := index * 56 * 755 / 1000
private def sideSlotSize : Nat := 39
private def sideSlotStep : Nat := 43
private def coinLeft : Nat := inventoryInset + hotbarSize * slotStride + 7
private def coinTop : Nat := inventoryInset + 35
private def nearbyToggleLeft : Nat := coinLeft
private def nearbyToggleTop : Nat := craftTop - 29
private def nearbyToggleSize : Nat := 26
private def inventorySortLeft : Nat := coinLeft + sideSlotStep
private def inventorySortTop : Nat := inventoryInset + 4 * slotStride
private def inventorySortSize : Nat := 30
private def storageActions : Array OriginalStorage.Action :=
  #[.lootAll, .depositAll, .quickStack, .restock, .sortItems]
private def storageActionLeft : Nat := 506
private def storageActionWidth : Nat := 106
private def storageActionHeight : Nat := 26
private def rightMouseHeld : UInt64 := 56
private def pickupSound : UInt32 := 39
private def unlockSound : UInt32 := 52
private def chestOpenSound : UInt32 := 59
private def chestCloseSound : UInt32 := 60
private def menuTickSound : UInt32 := 61
private def playerHitSound : UInt32 := 63
private def playerDeathSound : UInt32 := 64
private def chatOpenSound : UInt32 := 65
private def angryBonesHitSound : UInt32 := 66
private def angryBonesKilledSound : UInt32 := 67
private def casterMagicSound : UInt32 := 27
private def waterSphereBreakSound : UInt32 := 22
private def waterSphereHitSound : UInt32 := 68
private def waterSphereKilledSound : UInt32 := 69
private def dungeonSlimeHitSound : UInt32 := 35
private def dungeonSlimeKilledSound : UInt32 := 51
private def cursedSkullHitSound : UInt32 := 66
private def cursedSkullKilledSound : UInt32 := 67
private def mechanicHitSound : UInt32 := 35
private def mechanicKilledSound : UInt32 := 51
private def skeletronRoarSound : UInt32 := 28

private structure Camera where
  x : Int
  y : Int

private def cameraFor (state : OriginalSession.State) : Camera :=
  let body := state.player.body
  let header := state.terrain.header
  { x := min (header.width * 16 - screenWidth : Nat) <|
      max 0 (body.position.x.toInt32.toInt + (body.width / 2 : Nat) - (screenWidth / 2 : Nat))
    y := min (header.height * 16 - screenHeight : Nat) <|
      max 0 (body.position.y.toInt32.toInt + (body.height / 2 : Nat) - (screenHeight / 2 : Nat)) }

private def Camera.tile (camera : Camera) (x y : Nat) : Int × Int :=
  ((x : Int) * 16 - camera.x, (y : Int) * 16 - camera.y)

private def Camera.viewport (camera : Camera) : OriginalDesktopLiquid.Viewport :=
  ⟨camera.x, camera.y, screenWidth, screenHeight⟩

private def Camera.bounds (camera : Camera) : OriginalStarterMelee.Rect :=
  ⟨camera.x, camera.y, screenWidth, screenHeight⟩

private def Camera.skyView (camera : Camera) : OriginalForestSky.Camera :=
  ⟨⟨OriginalCollision.single camera.x, OriginalCollision.single camera.y⟩,
    ⟨screenWidth.toFloat32, screenHeight.toFloat32⟩⟩

private def Camera.target (camera : Camera) (x y : Nat) : Option Cell :=
  if x < screenWidth && y < screenHeight then
    some ⟨(camera.x + (x : Int)).toNat / 16, (camera.y + (y : Int)).toNat / 16⟩
  else none

-- BEGIN B-ZONE lighting-draw-light (S01)
/-- `Lighting.GetColor` of one draw, read from the session's lighting runtime: every cell of the
engine's `_activeProcessedArea` through `OriginalLightingRuntime.packedColor` (`0xRRGGBBAA`, the
renderer's tint order) and `OriginalLightingRuntime.packedValue` (FNA `Color.PackedValue`, as
`Main.DrawRain` and the water dust take it), once per draw after the draw's `LightTiles` calls (the
active map only changes there, at phase 3 Present). Outside the area `GetColor` is the black of an
empty cell; `outsideColor`/`outsideValue` are read through the same calls at a cell outside it. -/
private structure DrawLight where
  area : OriginalLightMap.Rectangle
  colors : Array UInt32
  values : Array UInt32
  outsideColor : UInt32
  outsideValue : UInt32

/-- Read the active map. A `Fault` is fatal (natively an exception in `Lighting.GetColor`): it is
thrown, never substituted. -/
private def DrawLight.read (rt : OriginalLightingRuntime.Runtime) : IO DrawLight := do
  let get (what : String) (x y : Int32) (r : Except OriginalLightingEngine.Fault UInt32) : IO UInt32 :=
    match r with
    | .ok v => pure v
    | .error f => throw (IO.userError s!"Lighting.GetColor({x}, {y}) [{what}]: {reprStr f}")
  let area := rt.lighting.engine.activeProcessedArea
  -- `Rectangle.Contains` is half-open: the cell at `X + Width` is outside the area.
  let ox := area.x + area.width
  let outsideColor ← get "packedColor" ox area.y (OriginalLightingRuntime.packedColor rt ox area.y)
  let outsideValue ← get "packedValue" ox area.y (OriginalLightingRuntime.packedValue rt ox area.y)
  let w := area.width.toInt.toNat
  let h := area.height.toInt.toNat
  let mut colors := Array.mkEmpty (w * h)
  let mut values := Array.mkEmpty (w * h)
  for j in [:h] do
    for i in [:w] do
      let x := area.x + Int32.ofNat i
      let y := area.y + Int32.ofNat j
      colors := colors.push (← get "packedColor" x y (OriginalLightingRuntime.packedColor rt x y))
      values := values.push (← get "packedValue" x y (OriginalLightingRuntime.packedValue rt x y))
  return { area, colors, values, outsideColor, outsideValue }

private def DrawLight.index? (l : DrawLight) (x y : Int) : Option Nat :=
  let dx := x - l.area.x.toInt
  let dy := y - l.area.y.toInt
  if dx < 0 || dy < 0 || dx ≥ l.area.width.toInt || dy ≥ l.area.height.toInt then none
  else some (dy.toNat * l.area.width.toInt.toNat + dx.toNat)

/-- `Lighting.GetColor(x, y)` packed `0xRRGGBBAA` (`OriginalLightingRuntime.packedColor`). -/
private def DrawLight.color (l : DrawLight) (x y : Int) : UInt32 :=
  match l.index? x y with
  | some k => l.colors[k]!
  | none => l.outsideColor

/-- `Lighting.GetColor(x, y)` as FNA `Color.PackedValue` (`OriginalLightingRuntime.packedValue`). -/
private def DrawLight.value (l : DrawLight) (x y : Int) : UInt32 :=
  match l.index? x y with
  | some k => l.values[k]!
  | none => l.outsideValue
-- END B-ZONE lighting-draw-light

private structure Scene where
  state : OriginalSession.State
  -- BEGIN B-ZONE lighting-scene (S01)
  /-- `Lighting.GetColor` of the latest draw (`DrawLight.read` after its `LightTiles` calls). -/
  light : DrawLight
  -- END B-ZONE lighting-scene
  /-- UI clock/cache: neither belongs to the world or player save. -/
  cursorClock : OriginalCursorItemIcon.Clock := {}
  cursorIconCache : OriginalCursorItemIcon.Cache := {}
  mouseColor : OriginalForestChatLayout.Color := OriginalMouseCursor.defaultMouseColor
  mouseBorderColor : OriginalForestChatLayout.Color := OriginalMouseCursor.defaultBorderColor
  cursorColor : OriginalForestChatLayout.Color := OriginalMouseCursor.initialCursorColor
  /-- Set by the preceding late NPC mouse-over, consumed by the next Player.Update. -/
  currentNPCShowingChatBubble : Int := -1
  liquidRenderer : OriginalDesktopLiquid.Renderer := {}
  waterfallRenderer : OriginalDesktopWaterfall.Renderer := {}
  sky : OriginalForestSky.Sky := {}
  skyAtmosphere : Float32 := 1
  -- BEGIN B-ZONE playerdraw-scene (S05)
  /-- Why the latest draw of the living player fell back to the simplified route (`none`: the armor
  layers of `OriginalPlayerDrawRuntime` drew it). Logged when it changes. -/
  playerDrawBoundary : Option String := none
  -- END B-ZONE playerdraw-scene
  manaDraws : Array (Nat × Array OriginalForestChatLayout.Sprite) := #[]
  spikeBallDraws : Array OriginalForestChatLayout.Sprite := #[]
  /-- The sprites `Main.DrawRain` emitted this draw (screen positions, packed R | G<<8 | B<<16 | A<<24). -/
  rainDraws : Array OriginalRainAtmosphere.Sprite := #[]
  waterScene : OriginalSceneWaterStyle.State := OriginalSceneWaterStyle.State.teleported
  appearance : Appearance := default
  dirtyFrames : Std.HashSet (Nat × Nat) := {}
  /-- `Player.changeItem`: the hotbar slot a draw's click requested for the next update. -/
  changeItem : Option Nat := none
  /-- `Main.BlackFadeCameraTeleport` ran in an update since the last draw. -/
  blackFade : Bool := false
  inventoryOpen : Bool := false
  rename : OriginalStorageRename.State := {}
  renamePressed : Option OriginalStorageRename.Keyboard.Part := none
  renameHovered : Option OriginalStorageRename.Keyboard.Part := none
  openSign : Option Cell := none
  signEdit : Option OriginalDesktopSign.Edit := none
  talk : OriginalForestGuideDialogue.State := {}
  talkPortraitFrame : Nat := 0
  talkLeftRelease : Bool := true
  guideCraft : Option OriginalForestGuideCrafting.State := none
  guideGrid : Option OriginalForestGuideCraftGridLayout.State := none
  guideGridOutput : Option OriginalForestGuideCraftGridLayout.Output := none
  guideGridRequested : Bool := false
  guideToggleHovered : Bool := false
  bossNotice : Option (String × Nat) := none
  bossBarSlot : Option Nat := none
  dungeonMusicMix : OriginalSkeletronPresentation.MusicMix := {}
  nearbyCrafting : Bool := true
  chestRow : Nat := 0
  chestLids : Array (Nat × Nat) := #[]
  splitClock : OriginalStorage.SplitClock := {}
  recipeFocus : Option Nat := none
  recipePosition : Nat := 0
  recipeScroll : Nat := 0
  activeCraft : Option Nat := none
  focusedOnMouseDown : Bool := false
  respawnJumpHeld : Bool := false
  tick : Nat := 0
  inventoryGlow : Array (Nat × Float32 × Nat) := #[]
  storageGlow : Array (Nat × Float32 × Nat) := #[]
  buffBar : OriginalPlayerBuffBar.State := {}
  buffOutput : Option OriginalPlayerBuffBar.Output := none
  buffRightRelease : Bool := true
  itemCoins : OriginalInventoryItemDrawing.CoinSlots := {}
  slotSprites : Std.HashMap (Nat × Nat) (Array OriginalForestChatLayout.Sprite) := {}

private def pressed (keys index : UInt64) : Bool :=
  (keys &&& (1 <<< index)) != 0

/-- `Player.selectedItem`: the effective slot, 58 while a cursor item overrides the hotbar. -/
private def Scene.selected (scene : Scene) : Nat := scene.state.selection.selected

private def Scene.guideGridVisible (scene : Scene) : Bool :=
  scene.guideCraft.isSome && (scene.guideGridRequested || scene.guideGrid.any (·.visible))

private def Scene.guideSearchActive (scene : Scene) : Bool :=
  scene.guideGridVisible && scene.guideGrid.any (·.writing)

private def guideGridToggleAt (x y : Nat) : Bool :=
  x > 27 && x < 57 && y > 270 && y < 300

private def Scene.guideGridUnderMouse (scene : Scene) (x y : Nat) : Bool :=
  scene.guideCraft.isSome && (guideGridToggleAt x y ||
    (scene.guideGridVisible && scene.guideGrid.any (fun grid => grid.contains .state x y)))

private def playDialogueSound (effects : OriginalForestGuideDialogue.Effects) : IO Unit := do
  match effects.sound with
  | some 11 => sound chestCloseSound
  | some 12 => sound menuTickSound
  | some 24 => sound chatOpenSound
  | _ => pure ()

private def markFrameNeighbors (dirty : Std.HashSet (Nat × Nat)) (x y : Nat) :
    Std.HashSet (Nat × Nat) := Id.run do
  let mut dirty := dirty
  for nx in [x - 1:x + 2] do
    for ny in [y - 1:y + 2] do
      dirty := dirty.insert (nx, ny)
  return dirty

private def Scene.updateTerrain (scene : Scene) (state : OriginalSession.State) : Scene :=
  let dirtyFrames := state.foregroundEdits.foldl
    (fun dirty (x, y) => markFrameNeighbors dirty x y) scene.dirtyFrames
  { scene with state, dirtyFrames }

private def Scene.currentChest (scene : Scene) : Option Chest :=
  scene.state.openChest.bind fun index => scene.state.chests[index]?

private def chestRenameTile (terrain : Terrain) (chest : Chest) : Option OriginalStorageRename.ContainerTile := do
  let tile ← terrain.tile? chest.x.toNat chest.y.toNat
  let id ← tile.id
  let (frameX, _) ← tile.frame
  OriginalStorageRename.ContainerTile.ofTile id.toNat frameX.toNat

private def Scene.renameIndex (scene : Scene) : Int :=
  (scene.state.openChest.map Int.ofNat).getD (-1)

private def Scene.renameName (scene : Scene) : String :=
  (scene.currentChest.map (·.name)).getD ""

private def Scene.storageSize (scene : Scene) : Nat :=
  match scene.state.openStorage with
  | some (.bank furniture) => (scene.state.banks.items furniture.kind).size
  | _ => (scene.currentChest.map (·.items.size)).getD 0

private def inside (x y left top width height : Nat) : Bool :=
  x ≥ left && x < left + width && y ≥ top && y < top + height

private def nearbyToggleAt (x y : Nat) : Bool :=
  inside x y nearbyToggleLeft nearbyToggleTop nearbyToggleSize nearbyToggleSize

private def Scene.inventorySortAt (scene : Scene) (x y : Nat) : Bool :=
  scene.inventoryOpen && scene.state.openStorage.isNone &&
    inside x y inventorySortLeft inventorySortTop (inventorySortSize + 1) (inventorySortSize + 1)

private def storageActionAt (x y : Nat) : Option OriginalStorage.Action :=
  if inside x y storageActionLeft craftTop storageActionWidth
      (storageActions.size * storageActionHeight) then storageActions[(y - craftTop) / storageActionHeight]?
  else none

private def Scene.renameButtonAt (scene : Scene) (x y : Nat) : Bool :=
  scene.state.openChest.isSome &&
    inside x y storageActionLeft (craftTop + storageActions.size * storageActionHeight)
      storageActionWidth storageActionHeight

private def Scene.renameCancelAt (scene : Scene) (x y : Nat) : Bool :=
  scene.rename.editChest && scene.state.openChest.isSome &&
    inside x y storageActionLeft (craftTop + (storageActions.size + 1) * storageActionHeight)
      storageActionWidth storageActionHeight

private def Scene.storageUnderMouse (scene : Scene) (x y : Nat) : Bool :=
  scene.state.openStorage.isSome && (inside x y chestLeft (craftTop - 22)
    (chestStep hotbarSize) (chestStep chestRows + 22) || (storageActionAt x y).isSome ||
    scene.renameButtonAt x y || scene.renameCancelAt x y)

private def Scene.storageSlotAt (scene : Scene) (x y : Nat) : Option Nat := do
  let column ← (List.range hotbarSize).find? fun column =>
    x ≥ chestLeft + chestStep column && x < chestLeft + chestStep column + chestSlotSize
  let row ← (List.range chestRows).find? fun row =>
    y ≥ craftTop + chestStep row && y < craftTop + chestStep row + chestSlotSize
  let index := (scene.chestRow + row) * hotbarSize + column
  guard (index < scene.storageSize)
  return index

private def sideSlotAt (x y : Nat) : Option Nat := do
  guard (inside x y coinLeft coinTop (2 * sideSlotStep) (4 * sideSlotStep))
  let dx := x - coinLeft
  let dy := y - coinTop
  guard (dx % sideSlotStep < sideSlotSize && dy % sideSlotStep < sideSlotSize)
  return 4 * (dx / sideSlotStep) + dy / sideSlotStep

private def craftingListAt (x y : Nat) : Bool :=
  inside x y inventoryInset craftTop slotStride (craftSlots * slotStride)

private def Scene.craftDetailsLeft (scene : Scene) : Nat :=
  if scene.state.openStorage.isSome then storageActionLeft + storageActionWidth + 12
  else if scene.guideCraft.isSome then guideSlotLeft + 3 * slotStride
  else inventoryInset + slotStride

private def Scene.inventoryUnderMouse (scene : Scene) (x y : Nat) : Bool :=
  let rows := if scene.inventoryOpen then 5 else 1
  (x ≥ inventoryInset && x < inventoryInset + hotbarSize * slotStride &&
    y ≥ inventoryInset && y < inventoryInset + rows * slotStride) ||
  (scene.inventoryOpen && (scene.storageUnderMouse x y || nearbyToggleAt x y || scene.inventorySortAt x y ||
    inside x y coinLeft coinTop (2 * sideSlotStep) (4 * sideSlotStep) ||
    scene.guideGridUnderMouse x y ||
    (!scene.guideGridVisible && (craftingListAt x y ||
      (scene.guideCraft.isSome && inside x y guideSlotLeft guideSlotTop guideSlotSize guideSlotSize))) ||
    (!scene.guideGridVisible && scene.recipeFocus.isSome && inside x y scene.craftDetailsLeft craftTop
      (screenWidth - scene.craftDetailsLeft) (screenHeight - craftTop))))

private def Scene.focusRecipe (scene : Scene) (recipes : Array Nat) (position : Nat) : Scene :=
  let position := min position (recipes.size - 1)
  let scroll := min scene.recipeScroll (recipes.size - craftSlots)
  let scroll := min position (max scroll (position + 1 - craftSlots))
  { scene with recipeFocus := recipes[position]?, recipePosition := position, recipeScroll := scroll }

private def Scene.refocusRecipe (scene : Scene) (recipes : Array Nat) : Scene :=
  let position := scene.recipeFocus.bind fun index => recipes.findIdx? (· == index)
  scene.focusRecipe recipes (position.getD scene.recipePosition)

private def Scene.withInventoryResult (scene : Scene) (result : OriginalCrafting.Result) : Scene :=
  { scene with state := { scene.state with inventory := result.inventory, mouseItem := result.mouseItem } }

private def Scene.recipes (scene : Scene) : Array Nat :=
  match scene.guideCraft.bind (·.guideItem) with
  | some material => catalog.guideRecipes material.item.toNat
  | none => scene.state.availableRecipes scene.nearbyCrafting

private def guideMaterialTable : Array Bool := OriginalForestGuideCrafting.materialTable catalog

private def guideMaterial (item : Nat) : Bool := guideMaterialTable[item]?.getD false

private def Scene.withGuideCraft (scene : Scene) (craft : OriginalForestGuideCrafting.State) : Scene :=
  { scene with
    guideCraft := some craft
    splitClock := { scene.splitClock with cooldown := craft.stackSplit }
    state := { scene.state.setCarriedItems craft.inventory with mouseItem := craft.mouseItem } }

private def Scene.guideCraftWithItems (scene : Scene) (craft : OriginalForestGuideCrafting.State) :
    OriginalForestGuideCrafting.State :=
  { craft with
    inventory := scene.state.carriedItems, mouseItem := scene.state.mouseItem
    stackSplit := scene.splitClock.cooldown, stackDelay := scene.splitClock.delay, stackExtra := scene.splitClock.extra }

private def Scene.controlGuideSlot (scene : Scene) (keys : UInt64) (x y : Nat) : IO Scene := do
  let some craft := scene.guideCraft | return scene
  let craft := scene.guideCraftWithItems craft
  if !inside x y guideSlotLeft guideSlotTop guideSlotSize guideSlotSize then
    return { scene with guideCraft := some craft }
  let (craft, effects) := OriginalForestGuideCrafting.guideSlot guideMaterial
    { left := pressed keys 3, right := pressed keys rightMouseHeld, rightRelease := pressed keys 4, shift := pressed keys 63 }
    (scene.guideCraftWithItems craft)
  for effect in effects.sounds do
    sound (if effect == 7 then pickupSound else menuTickSound)
  return scene.withGuideCraft craft

private def Scene.controlCrafting (scene : Scene) (keys : UInt64) (x y : Nat) : Scene := Id.run do
  let down := pressed keys 57 || pressed keys rightMouseHeld
  let clicked := pressed keys 3 || pressed keys 4
  let wheelUp := pressed keys 60
  let wheelDown := pressed keys 61
  if !craftingListAt x y || !(down || wheelUp || wheelDown) then return scene
  let recipes := scene.recipes
  let scene := scene.refocusRecipe recipes
  if wheelUp || wheelDown then
    let next := if wheelUp then scene.recipePosition - 1 else scene.recipePosition + 1
    return scene.focusRecipe recipes next
  if x - inventoryInset ≥ slotSize || (y - craftTop) % slotStride ≥ slotSize then return scene
  let row := scene.recipeScroll + (y - craftTop) / slotStride
  let some index := recipes[row]? | return scene
  if scene.recipeFocus != some index then
    return if clicked then
      { scene.focusRecipe recipes row with
        focusedOnMouseDown := true
        splitClock := { scene.splitClock with cooldown := 15 } }
    else scene
  if (scene.guideCraft.bind (·.guideItem)).isSome || scene.focusedOnMouseDown || scene.splitClock.cooldown > 1 ||
      scene.activeCraft.any (· != index) ||
      scene.state.itemUse.itemAnimation != 0 then return scene
  let batch := (scene.splitClock.extra + 1) * (if pressed keys 63 then 10 else 1)
  let some state := scene.state.craftRecipe scene.nearbyCrafting index batch | return scene
  return { scene with state, splitClock := scene.splitClock.refresh, activeCraft := some index }

private def Scene.clickStorageSlot (scene : Scene) (slot : Nat) (shift : Bool) : Scene := Id.run do
  if shift then
    if let some state := scene.state.shiftStorage slot false then return { scene with state }
  let some slots := scene.state.openStorageItems | return scene
  let keepFavorites := match scene.state.openStorage with
    | some (.bank furniture) => furniture.kind.keepsFavorites
    | _ => false
  let result := OriginalCrafting.clickSlot slots scene.state.mouseItem slot keepFavorites
  let state := scene.state.replaceOpenStorage result.inventory
  return { scene with state := { state with mouseItem := result.mouseItem } }

private def toggleBagIn (scene : Scene) (slot : Option OriginalItems.Stack) : Option OriginalItems.Stack := do
  guard (scene.state.itemUse.itemAnimation == 0)
  let stack ← slot
  guard (!scene.state.mouseItem.any (·.canStack stack))
  OriginalBanks.toggleBag stack

private def Scene.rightClickStorage (scene : Scene) (slot : Nat) (pressedNow : Bool) : Scene := Id.run do
  if scene.state.itemUse.itemAnimation > 0 then return scene
  let some slots := scene.state.openStorageItems | return scene
  let item := slots[slot]?.join
  if let some stack := toggleBagIn scene item then
    if !pressedNow then return scene
    return { scene with state := scene.state.replaceOpenStorage (slots.set! slot (some stack)) }
  let some (splitClock, item, mouseItem) := scene.splitClock.take item scene.state.mouseItem | return scene
  let state := scene.state.replaceOpenStorage (slots.set! slot item)
  return { scene with splitClock, state := { state with mouseItem } }

private def updateSortGlow (old : Array (Nat × Float32 × Nat))
    (new : Array (Nat × Float32)) (tick : Nat) : Array (Nat × Float32 × Nat) :=
  old.filter (fun (slot, _, expires) => expires > tick && !new.any (·.1 == slot)) ++
    new.map (fun (slot, hue) => (slot, hue, tick + 301))

private def Scene.controlInventory (scene : Scene) (keys : UInt64) (x y : Nat) : Scene := Id.run do
  let inventoryOpen := if pressed keys 33 then !scene.inventoryOpen else scene.inventoryOpen
  let state := if inventoryOpen then scene.state else { scene.state with openStorage := none }
  -- ItemSlot.LeftClick returns while an item is in use (IL_003b..004b, after OverrideLeftClick);
  -- ItemSlot.RightClick returns while itemAnimation > 0 (IL_001d..0026).
  let busyLeft := state.itemUse.itemAnimation != 0 || state.itemUse.itemTime != 0
  let busyRight := state.itemUse.itemAnimation > 0
  let scene := { scene with
    state
    inventoryOpen
    storageGlow := if inventoryOpen then scene.storageGlow else #[] }
  if inventoryOpen then
    if scene.inventorySortAt x y then
      if !pressed keys 3 then return scene
      let (state, glow) := state.sortCarried
      return { scene with state, inventoryGlow := updateSortGlow scene.inventoryGlow glow scene.tick }
    if nearbyToggleAt x y && pressed keys 3 then
      return { scene with nearbyCrafting := !scene.nearbyCrafting }
    if scene.storageUnderMouse x y then
      if let some action := storageActionAt x y then
        if !pressed keys 3 then return scene
        let (state, glow) := scene.state.storageActionWithGlow action
        return { scene with state, storageGlow := updateSortGlow scene.storageGlow glow scene.tick }
      let maxRow := (scene.storageSize + hotbarSize - 1) / hotbarSize - chestRows
      let row := if pressed keys 60 then scene.chestRow - 1
        else if pressed keys 61 then min maxRow (scene.chestRow + 1) else scene.chestRow
      let scene := { scene with chestRow := row }
      if let some slot := scene.storageSlotAt x y then
        if pressed keys 3 && (pressed keys 63 || !busyLeft) then
          return scene.clickStorageSlot slot (pressed keys 63)
        if pressed keys rightMouseHeld && !busyRight then return scene.rightClickStorage slot (pressed keys 4)
      return scene
    if pressed keys 3 then
      if let some slot := sideSlotAt x y then
        if pressed keys 63 then
          if let some state := scene.state.shiftStorage (scene.state.inventory.size + slot) true then
            return { scene with state }
        if busyLeft then return scene
        let cursor := scene.state.mouseItem
        let isCoin (stack : OriginalItems.Stack) := stack.item ≥ 71 && stack.item ≤ 74
        if cursor.all (fun stack => if slot < 4 then isCoin stack else OriginalItemStats.fitsAmmoSlot stack.item) then
          let result := OriginalCrafting.clickSlot scene.state.coinAndAmmo cursor slot
          return { scene with state := { scene.state with
            coinAndAmmo := result.inventory, mouseItem := result.mouseItem } }
        return scene
    if (pressed keys 3 || pressed keys rightMouseHeld) && x ≥ inventoryInset && y ≥ inventoryInset then
      let col := (x - inventoryInset) / slotStride
      let row := (y - inventoryInset) / slotStride
      if col < hotbarSize && row < 5 && (x - inventoryInset) % slotStride < slotSize &&
          (y - inventoryInset) % slotStride < slotSize then
        let slot := row * hotbarSize + col
        if pressed keys 3 then
          if let some craft := scene.guideCraft then
            if pressed keys 63 then
              let (craft, _) := OriginalForestGuideCrafting.inventorySlot guideMaterial slot
                { left := true, shift := true } (scene.guideCraftWithItems craft)
              return scene.withGuideCraft craft
          if pressed keys 63 then
            if let some state := scene.state.shiftStorage slot true then return { scene with state }
          if busyLeft then return scene
          let result := OriginalCrafting.clickSlot scene.state.inventory scene.state.mouseItem slot
          return scene.withInventoryResult result
        if busyRight then return scene
        if let some stack := toggleBagIn scene scene.state.inventory[slot]?.join then
          if !pressed keys 4 then return scene
          let inventory := scene.state.inventory.set! slot (some stack)
          return { scene with state := { scene.state with inventory } }
        let item := scene.state.inventory[slot]?.join
        let some (splitClock, item, mouseItem) := scene.splitClock.take item scene.state.mouseItem | return scene
        return { scene with splitClock, state := { scene.state with
          inventory := scene.state.inventory.set! slot item, mouseItem } }
    return scene
  return { scene with
    activeCraft := none
    focusedOnMouseDown := false }

/-- Main.GUIHotbarDrawInner IL_01f5..02e3, inventory closed: while the left button is down over
a hotbar slot and `Main.blockMouse` is clear, every draw writes the slot to `Player.changeItem`.
There is no release, selection or animation test; the next update's Select consumes it. -/
private def Scene.hotbarClick (scene : Scene) (keys : UInt64) (x y : Nat) : Scene :=
  if !scene.inventoryOpen && pressed keys 57 && !scene.state.blockMouse &&
      y ≥ inventoryInset && y < inventoryInset + slotSize && x ≥ inventoryInset &&
      x < inventoryInset + hotbarSize * slotStride && (x - inventoryInset) % slotStride < slotSize then
    { scene with changeItem := some ((x - inventoryInset) / slotStride) }
  else scene

/-- The `SelectedItemState.Select` calls of one local `Player.Update`, in their IL order:
`changeItem` (IL_1308), the held number key with the highest slot (IL_1335..1535), then a nonzero
hotbar scroll from the remembered hotbar slot, which only exists with the inventory closed
(HandleHotbarControls IL_0108..0123). -/
private def Scene.selectEvents (scene : Scene) (heldKeys : UInt32) (wheel : Int) : Array Nat := Id.run do
  let mut events : Array Nat := #[]
  let mut hotbar := scene.state.selection.hotbar
  if let some slot := scene.changeItem then
    events := events.push slot
    hotbar := slot
  let key := (List.range hotbarSize).foldl (init := none) fun found index =>
    if heldKeys &&& ((1 : UInt32) <<< index.toUInt32) != 0 then some index else found
  if let some slot := key then
    events := events.push slot
    hotbar := slot
  if wheel != 0 && !scene.inventoryOpen then
    let offset := ((hotbar : Int) + wheel) % hotbarSize
    events := events.push (if offset < 0 then offset + hotbarSize else offset).toNat
  return events

private def Scene.interactStorage (scene : Scene) (keys : UInt64) (x y : Nat) : Scene × Bool := Id.run do
  if !pressed keys 4 || scene.inventoryUnderMouse x y || scene.state.mouseItem.isSome then return (scene, false)
  let some tile := (cameraFor scene.state).target x y | return (scene, false)
  match scene.state.interactStorage tile with
  | some (state, .toggled) =>
    return ({ scene with state, inventoryOpen := true, chestRow := 0, storageGlow := #[],
                         openSign := none, signEdit := none }, false)
  | some (state, .unlocked) => return (scene.updateTerrain state, true)
  | none => return (scene, false)

private def Scene.signUnderMouse (scene : Scene) (x y : Nat) : Option Cell := do
  guard (!scene.state.health.dead && !scene.inventoryUnderMouse x y)
  let tile ← (cameraFor scene.state).target x y
  guard (OriginalMining.inToolRange scene.state.player.body tile 5 3 0)
  scene.state.signOrigin tile

private def Scene.signInReach (scene : Scene) (origin : Cell) : Bool :=
  !scene.state.health.dead && scene.state.signOrigin origin == some origin &&
    (scene.state.signAt origin).isSome &&
    [origin, ⟨origin.x + 1, origin.y⟩, ⟨origin.x, origin.y + 1⟩, ⟨origin.x + 1, origin.y + 1⟩].any
      (fun cell => OriginalMining.inToolRange scene.state.player.body cell 5 3 0)

private def Scene.signPanel (scene : Scene) : IO (Option OriginalDesktopSign.Panel) := do
  let some sign := scene.openSign.bind scene.state.signAt | return none
  let text := (scene.signEdit.map (fun edit => OriginalStorageRename.ofUtf16 edit.text)).getD sign.text
  return some (← OriginalDesktopSign.panel screenWidth text scene.signEdit.isSome)

private def Scene.closeSign (scene : Scene) : IO Scene := do
  sound chestCloseSound
  return { scene with openSign := none, signEdit := none }

private def Scene.controlSign (scene : Scene) (panel : Option OriginalDesktopSign.Panel)
    (keys : UInt64) (x y : Nat) : IO (Option Scene) := do
  if let some origin := scene.openSign then
    if pressed keys 33 then
      return some (← if scene.signEdit.isSome then pure { scene with signEdit := none } else scene.closeSign)
    if pressed keys 3 && panel.any (fun view => view.contains x y) then
      match panel.bind (fun view => view.buttonAt x y) with
      | some .close => return some (← scene.closeSign)
      | some .edit =>
        sound menuTickSound
        if let some edit := scene.signEdit then
          let state := scene.state.writeSign origin (OriginalStorageRename.ofUtf16 edit.text)
          return some { scene with state, signEdit := none }
        discard OriginalDesktopRename.readCharacters
        let signEdit := scene.state.signAt origin |>.map (OriginalDesktopSign.Edit.start ∘ OriginalWorld.Sign.text)
        return some { scene with signEdit }
      | none => return some scene
  if !pressed keys 4 || panel.any (fun view => view.contains x y) then return none
  let some origin := scene.signUnderMouse x y | return none
  if scene.openSign == some origin then return some (← scene.closeSign)
  let state := { scene.state with openStorage := none }
  let state := if (state.signAt origin).isSome then state else state.writeSign origin ""
  if (state.signAt origin).isNone then return some scene
  sound chestOpenSound
  return some { scene with state, openSign := some origin, inventoryOpen := false, signEdit := none }

private def Scene.applyRename (scene : Scene)
    (result : OriginalStorageRename.State × Array OriginalStorageRename.Effect) : IO Scene := do
  let (rename, effects) := result
  let mut scene := { scene with rename }
  for effect in effects do
    match effect with
    | .sound 10 => sound chestOpenSound
    | .sound 11 => sound chestCloseSound
    | .sound _ => sound menuTickSound
    | .setName index name => scene := { scene with state := scene.state.renameOpenChest index name }
    | .clipboard text => Desktop.setClipboard text
    | .closeKeyboard =>
      scene := { scene with
        inventoryOpen := !scene.state.health.dead
        renamePressed := none
        renameHovered := none }
  return scene

private def Scene.openRenameAt (scene : Scene) (keys : UInt64) (x y : Nat) : IO Scene := do
  if !scene.inventoryOpen || !pressed keys 3 || !scene.renameButtonAt x y then return scene
  let some chest := scene.currentChest | return scene
  let some tile := chestRenameTile scene.state.terrain chest | return scene
  discard OriginalDesktopRename.readCharacters
  scene.applyRename (OriginalStorageRename.renameButton scene.rename scene.renameIndex chest.name tile)

private def renamePoint (x y : Nat) : Float32 × Float32 :=
  (x.toUInt64.toFloat32, y.toUInt64.toFloat32)

private def Scene.controlRenamePointer (scene : Scene) (keys : UInt64) (x y : Nat) : IO (Scene × Bool) := do
  let some keyboard := scene.rename.keyboard | return (scene, false)
  let pointer := renamePoint x y
  let panel ← OriginalDesktopRename.panel screenWidth screenHeight keyboard pointer
  if panel.hovered.isSome && panel.hovered != scene.renameHovered then sound menuTickSound
  let scene := { scene with renameHovered := panel.hovered }
  if pressed keys 3 && panel.hovered.isNone then
    if scene.renameButtonAt x y then
      return (← scene.applyRename (OriginalStorageRename.submit scene.rename scene.renameIndex scene.renameName), false)
    if scene.renameCancelAt x y then
      return (← scene.applyRename (OriginalStorageRename.cancelButton scene.rename), false)
  if pressed keys 3 then return ({ scene with renamePressed := panel.hovered }, false)
  if !pressed keys 57 then
    if let some part := scene.renamePressed then
      let scene := { scene with renamePressed := none }
      if panel.hovered == some part then
        let result := OriginalStorageRename.Keyboard.click scene.rename keyboard.keyState part
          scene.renameIndex scene.renameName
        return (← scene.applyRename result, false)
      return (scene, false)
  if pressed keys 4 && !panel.blocksWorld pointer then
    let (changed, unlocked) := scene.interactStorage keys x y
    if unlocked || changed.state.openStorage != scene.state.openStorage then
      return (← changed.applyRename (OriginalStorageRename.interactWithContainer scene.rename), unlocked)
  return (scene, false)

private def Scene.advanceRename (scene : Scene) (enter : Bool) (focused : Bool) : IO Scene := do
  if scene.rename.keyboard.isNone then return scene
  let rename := OriginalStorageRename.afterPlayerUpdate scene.rename scene.renameIndex
  let frame ← if focused then OriginalDesktopRename.readFrame rename.input enter else pure {}
  scene.applyRename (OriginalStorageRename.drawFrame rename frame scene.renameIndex scene.renameName)

private abbrev ResidentText := String → Option String

private structure ResidentResources where
  fonts : OriginalDesktopDialogue.Renderer
  dictionary : OriginalResidentText.Dict
  guideGrid : Option OriginalForestGuideCraftGridCatalog.Built := none
  buffs : OriginalPlayerBuffBar.Catalog
  inventorySizes : Std.HashMap String (Nat × Nat) := {}

private def ResidentResources.load (loaded : Option OriginalDesktopDialogue.Renderer := none) : IO ResidentResources := do
  let fonts ← loaded.map pure |>.getD OriginalDesktopDialogue.Renderer.load
  let path ← originalAssemblyPath
  let bytes ← IO.FS.readBinFile ⟨path⟩
  let dictionary ← IO.ofExcept ((OriginalResidentText.load bytes).mapError fun e => IO.userError (reprStr e))
  let buffs := OriginalPlayerBuffBar.priorityBuffs fun key => (dictionary.raw? key).getD key
  for buff in buffs do
    let size ← OriginalDesktopGuideGrid.textureSize s!"Images/Buff_{buff.id}"
    unless size == (buff.width, buff.height) do
      throw (IO.userError s!"Unexpected original Buff_{buff.id} texture dimensions {size}.")
  return { fonts, dictionary, buffs := { mouseText := fonts.mouseText, itemStack := fonts.itemStack, buffs } }

/-- The native Buff interface runs once per draw, after Player.Update. Cancellation
writes back the slot arrays once, retaining the flags already set by that update. -/
private def Scene.controlBuffBar (scene : Scene) (resources : ResidentResources)
    (keys : UInt64) (x y : Nat) (focused : Bool) : IO Scene := do
  let health := scene.state.health
  let right := pressed keys rightMouseHeld
  let output ← IO.ofExcept ((OriginalPlayerBuffBar.frame resources.buffs scene.buffBar {
    types := health.buffType, times := health.buffTime
    manaHeat := scene.state.manaHeat.heat
    mouseX := x, mouseY := y, inventory := scene.inventoryOpen
    screenWidth, screenHeight, right, rightRelease := scene.buffRightRelease
    ignoreMouse := !focused, honeyWet := scene.state.playerLiquid.honeyWet
    holdingItem := scene.state.mouseItem.isSome }).mapError IO.userError)
  for event in output.events do
    if event == .menuTick then sound menuTickSound
  return { scene with
    buffBar := output.state, buffOutput := some output, buffRightRelease := !right
    state := { scene.state with health := { health with buffType := output.types, buffTime := output.times } } }

private def Scene.prepareInventoryIcons (scene : Scene) (resources : ResidentResources)
    (mouseX mouseY : Nat) : IO (Scene × ResidentResources) := do
  let some built := resources.guideGrid | return (scene, resources)
  let item := fun (s : Option OriginalItems.Stack) => s.map fun s =>
    ({ type := s.item.toNat, stack := s.count } : OriginalInventoryItemDrawing.SlotItem)
  let player : OriginalInventoryItemDrawing.PlayerView := {
    inventory := scene.state.carriedItems.map item, selectedItem := scene.selected }
  let mut requests : Array ((Nat × Nat) × OriginalInventoryItemDrawing.SlotDraw × Option OriginalItems.Stack) := #[]
  let count := if scene.inventoryOpen then scene.state.inventory.size else hotbarSize
  for index in [:count] do
    let stack := scene.state.inventory[index]?.join
    let scale := slotSize.toFloat32 / 52
    let d : OriginalInventoryItemDrawing.SlotDraw := {
      context := if scene.inventoryOpen then 0 else 13, slot := index
      x := (inventoryInset + index % hotbarSize * slotStride).toFloat32
      y := (inventoryInset + index / hotbarSize * slotStride).toFloat32
      scale, item := item stack, player
      light := if scene.inventoryOpen then OriginalForestGuideCraftGridLayout.transparent
        else OriginalInventoryItemDrawing.hotbarLight scale }
    requests := requests.push ((0, index), d, stack)
  if scene.inventoryOpen then
    for index in [:scene.state.coinAndAmmo.size] do
      let stack := scene.state.coinAndAmmo[index]?.join
      requests := requests.push ((1, index), {
        context := if index < 4 then 1 else 2, slot := 50 + index
        x := (coinLeft + index / 4 * sideSlotStep).toFloat32
        y := (coinTop + index % 4 * sideSlotStep).toFloat32
        scale := sideSlotSize.toFloat32 / 52, item := item stack, player }, stack)
    if let some slots := scene.state.openStorageItems then
      let offset := scene.chestRow * hotbarSize
      for index in [:min (chestRows * hotbarSize) (slots.size - offset)] do
        let stack := slots[offset + index]?.join
        requests := requests.push ((2, index), {
          context := if scene.state.openChest.isSome then 3 else 4, slot := offset + index
          x := (chestLeft + chestStep (index % hotbarSize)).toFloat32
          y := (craftTop + chestStep (index / hotbarSize)).toFloat32
          scale := chestSlotSize.toFloat32 / 52, item := item stack, player }, stack)
  if let some stack := scene.state.mouseItem then
    requests := requests.push ((3, 0), {
      context := 21, slot := 0, x := mouseX.toFloat32, y := mouseY.toFloat32
      scale := 1, item := item (some stack), player }, some stack)
  let mut sizes ← OriginalInventoryItemDrawing.addSizes resources.inventorySizes
    OriginalDesktopGuideGrid.textureSize OriginalInventoryItemDrawing.inventoryTextures
  for (_, d, _) in requests do
    if let some item := d.item then
      let names ← IO.ofExcept ((OriginalInventoryItemDrawing.itemTextures built.defs item.type).mapError IO.userError)
      sizes ← OriginalInventoryItemDrawing.addSizes sizes OriginalDesktopGuideGrid.textureSize names
  let catalog := OriginalInventoryItemDrawing.withSizes built sizes
  let mut coins := scene.itemCoins.updateAnims
  let mut outputs := {}
  for (key, d, stack) in requests do
    let visual := fun type => do
      let v ← OriginalInventoryItemDrawing.visualFor built sizes type
      let color := stack.bind (·.color)
      pure (match color with
        | none => v
        | some c => { v with color := ⟨((c >>> 24) &&& 255).toNat, ((c >>> 16) &&& 255).toNat,
            ((c >>> 8) &&& 255).toNat, (c &&& 255).toNat⟩ })
    let (sprites, next) ← IO.ofExcept ((OriginalInventoryItemDrawing.slotItemSprites catalog built.defs visual coins d).mapError IO.userError)
    coins := next
    outputs := outputs.insert key sprites
  return ({ scene with itemCoins := coins, slotSprites := outputs }, { resources with inventorySizes := sizes })

private def Scene.controlGuideGrid (scene : Scene) (resources : ResidentResources)
    (keys : UInt64) (x y : Nat) : IO Scene := do
  let some built := resources.guideGrid | return scene
  let catalog := built.catalog
  let some crafting := scene.guideCraft | return scene
  let hoverToggle := guideGridToggleAt x y
  let openingFromToggle := !scene.guideGridVisible
  if hoverToggle && !scene.guideToggleHovered then sound menuTickSound
  let mut scene := { scene with guideToggleHovered := hoverToggle }
  if !scene.guideGridVisible then
    scene := { scene with guideGridOutput := none }
    if !(hoverToggle && pressed keys 3) then return scene
    sound chestOpenSound
    sound menuTickSound
    scene := { scene with guideGridRequested := true }
  let crafting := OriginalForestGuideCrafting.updateRecipeList OriginalRecipeCatalog.catalog
    (fun _ => scene.state.availableRecipes scene.nearbyCrafting) (scene.guideCraftWithItems crafting)
  let available := crafting.availableTable.extract 0 crafting.numAvailable
  let input : OriginalForestGuideCraftGridLayout.Input := {
    screenWidth, screenHeight, mouseX := x, mouseY := y
    mouseLeft := pressed keys 57, mouseLeftRelease := pressed keys 3 && !openingFromToggle
    available, guideObject := crafting.guideToken }
  let grid ← if scene.guideGridRequested then
      IO.ofExcept ((OriginalForestGuideCraftGridLayout.State.openWindow catalog input
        crafting.focusRecipe (available[crafting.focusRecipe.toNat]?.getD 0) scene.guideGrid).mapError IO.userError)
    else match scene.guideGrid with
      | some grid => pure grid
      | none => throw (IO.userError "The Guide crafting grid has no open state.")
  let input ← OriginalDesktopGuideGrid.readInput input grid.keyboard (pressed keys 34)
  let frame ← IO.ofExcept ((OriginalDesktopGuideGrid.advance catalog grid crafting input
    { left := pressed keys 3, right := pressed keys rightMouseHeld, rightRelease := pressed keys 4, shift := pressed keys 63 }
    guideMaterial (clockAdvanced := true)).mapError IO.userError)
  for effect in frame.effects.sounds do
    sound (if effect == 7 then pickupSound else menuTickSound)
  if frame.output.recipeClicked || frame.output.gridClosed then sound menuTickSound
  if let some copied := frame.output.copied then Desktop.setClipboard copied
  return { scene.withGuideCraft frame.crafting with
    guideGrid := some frame.grid, guideGridOutput := some frame.output, guideGridRequested := false
    recipeFocus := frame.grid.selected, recipePosition := frame.grid.focusRecipe.toNat }

private def Scene.textWorld (scene : Scene) (playerName : String) : OriginalResidentText.World :=
  let header := scene.state.terrain.header
  {
    playerName, worldName := header.name, dayTime := scene.state.dayTime
    bloodMoon := header.bloodMoon, eclipse := header.eclipse, hardMode := header.hardMode
    raining := header.raining, crimson := header.crimson, downedGolemBoss := header.downedGolem
    npcs := scene.state.combat.residents.filterMap fun n =>
      if !n.active then none
      else some (n.kind.typeId.toNat, if !n.name.isEmpty then n.name else n.kind.typeName)
    homeless := some ((OriginalDesktopDialogue.Talk.resident scene.state scene.talk).any (·.homeless))
    zoneGraveyard := some (((scene.state.forest.map (·.playerFlags)).getD []).lookup "ZoneGraveyard" |>.getD false)
    deathsPVE := some scene.state.health.deathsPVE }

private def Scene.localize (scene : Scene) (resources : ResidentResources)
    (playerName : String) : ResidentText :=
  let world := scene.textWorld playerName
  let env := OriginalResidentText.env resources.dictionary world
  fun key => if key.startsWith "GuideHelpText." then
      (resources.dictionary.textIfConditionsMet env key).toOption.join
    else (resources.dictionary.text env key).toOption

private def Scene.guideCatalogEnv (scene : Scene) (built : OriginalForestGuideCraftGridCatalog.Built) :
    OriginalForestGuideCraftGridCatalog.Env :=
  let header := scene.state.terrain.header
  let selected := ((scene.state.equipped scene.selected).map (·.item.toNat)).getD 0
  { selectedItemType := selected
    selectedItemCrit := (built.defs.items[selected]?.map (·.crit)).getD 0
    hasCoinGun := scene.state.carriedItems.any (fun item => item.any (·.item == 905))
    crimson := header.crimson, downedPlantBoss := header.downedPlantBoss
    tenthAnniversaryWorld := header.tenthAnniversary, getGoodWorld := header.forTheWorthy
    mechdusa := header.zenithWorld, expertMode := header.gameMode == 1 || header.gameMode == 2 }

private structure ResidentPanel where
  buttons : Array OriginalForestChatLayout.Button
  layout : OriginalForestChatLayout.Output

private def Scene.residentPanel (scene : Scene) (renderer : OriginalDesktopDialogue.Renderer)
    (localize : ResidentText) (keys : UInt64) (x y : Nat) : Option ResidentPanel := do
  let resident := OriginalDesktopDialogue.Talk.resident scene.state scene.talk
  let bound := scene.state.combat.actors[scene.talk.talkNPC.toNat]?.bind OriginalCombat.Actor.boundMechanic?
  guard (scene.talk.talkNPC ≥ 0 && (resident.isSome || bound.any (·.body.active)))
  let text ← match scene.talk.text with
    | .none => none
    | .key "TownNPCMood_Guide.NoHome" => do
      let first ← localize "TownNPCMood_Guide.NoHome"
      let last ← localize "HousingText.HousingRequirements"
      some (first ++ "\n\n" ++ last)
    | line => localize line.toKey
  let portrait := resident.map fun resident => match resident.kind with
    | .guide => OriginalForestChatLayout.guidePortrait
      (if resident.name.isEmpty then (localize "NPCName.Guide").getD "Guide" else resident.name)
      scene.talkPortraitFrame
    | .oldMan => OriginalForestChatLayout.oldManPortrait scene.talkPortraitFrame
    | .mechanic => OriginalForestChatLayout.mechanicPortrait resident.name scene.talkPortraitFrame
  let buttons := match resident.map (·.kind) with
    | some .guide => OriginalForestChatLayout.guideButtons (resident.any (·.homeless)) false
    | some .oldMan => OriginalForestChatLayout.oldManButtons (OriginalDesktopDialogue.Talk.time scene.state).isDay
    | _ => #[⟨.close, OriginalForestChatLayout.closeLabel, false⟩]
  let layout ← (renderer.layout {
    screenWidth, screenHeight, mouseX := x, mouseY := y, text, buttons, portrait
    mouseLeft := pressed keys 57, mouseLeftRelease := scene.talkLeftRelease || !pressed keys 57
    time := ((scene.tick % 216000).toFloat / 60).toFloat32 }).toOption
  return { buttons, layout }

private def Scene.finishGuideCraft (scene : Scene) : Scene :=
  let state := (scene.guideCraft.bind (·.guideItem)).map scene.state.returnInterfaceItem |>.getD scene.state
  { scene with
    guideCraft := none, state, guideGridRequested := false, guideGridOutput := none
    guideGrid := scene.guideGrid.map fun grid => grid.close
      { screenWidth, screenHeight, mouseX := 0, mouseY := 0 } }

private def Scene.closeTalk (scene : Scene) : IO Scene := do
  let (talk, effects) := OriginalForestGuideDialogue.closeButton scene.talk
  playDialogueSound effects
  return { scene.finishGuideCraft with talk }

private def Scene.checkTalkRange (scene : Scene) : IO Scene := do
  if scene.talk.talkNPC < 0 then return scene
  let (talk, effects) := OriginalDesktopDialogue.Talk.closeIfOutOfReach scene.state scene.talk
  playDialogueSound effects
  let scene := if talk.talkNPC < 0 then scene.finishGuideCraft else scene
  return { scene with talk }

private def Scene.clickTalkButton (scene : Scene) (localize : ResidentText)
    (button : OriginalForestChatLayout.Interaction) : IO Scene := do
  match button with
  | .close => scene.closeTalk
  | .help =>
    let .ok (talk, effects) := OriginalDesktopDialogue.Talk.help scene.state (fun key => (localize key).isSome) scene.talk | return scene
    playDialogueSound effects
    return { scene with talk }
  | .housing =>
    sound menuTickSound
    return { scene with talk := { scene.talk with text := .key "TownNPCMood_Guide.NoHome", cornerItem := -1 } }
  | .crafting =>
    let (crafting, _) := OriginalForestGuideCrafting.craftingButton .grid { dialogue := scene.talk }
    sound menuTickSound
    return { scene with
      inventoryOpen := true, guideCraft := some crafting, talk := crafting.dialogue
      guideGridRequested := true
      recipeFocus := none, recipePosition := 0, recipeScroll := 0 }
  | .curse =>
    let (table, cursor) := scene.state.outerRandom.state
    let (combat, talk, announce) := scene.state.combat.curse (OriginalDesktopDialogue.Talk.time scene.state)
      scene.talk ⟨table, cursor⟩
    let notice := if announce then do
        let name ← localize "NPCName.SkeletronHead"
        let raw ← localize "Announcement.HasAwoken"
        let message ← (OriginalResidentText.appendFormat "Announcement.HasAwoken" raw #[name]).toOption
        some (message, scene.tick + 360)
      else scene.bossNotice
    return { scene with talk, state := { scene.state with combat }, bossNotice := notice }
  | _ => return scene

private def Scene.advanceTalk (scene : Scene) (panel : Option ResidentPanel) (localize : ResidentText)
    (keys : UInt64) (x y : Nat) (mouseInterface tileInteraction : Bool) : IO Scene := do
  let scene := { scene with
    talkLeftRelease := (panel.map (fun p : ResidentPanel => p.layout.mouseLeftRelease)).getD (!pressed keys 57)
    talkPortraitFrame := (panel.bind (·.layout.frameCounter)).getD scene.talkPortraitFrame }
  let clicked := panel.bind fun p => p.layout.clicked.bind (fun index => p.buttons[index]?)
  let scene ← if let some button := clicked then scene.clickTalkButton localize button.kind else pure scene
  let right := pressed keys rightMouseHeld
  let talk := scene.talk
  if scene.state.health.dead || mouseInterface || x ≥ screenWidth || y ≥ screenHeight ||
      panel.any (·.layout.mouseInterface) then
    return { scene with talk }
  let camera := cameraFor scene.state
  let input : OriginalForestGuideDialogue.Input := {
    mouse := ⟨camera.x + x, camera.y + y, 1, 1⟩, mouseRight := right }
  let result := OriginalDesktopDialogue.Talk.hover scene.state input (OriginalDesktopDialogue.Talk.player scene.state false tileInteraction) talk
  let state := if let some noThrow := result.effects.noThrow then
      { scene.state with throwNoThrow := noThrow.toNat }
    else scene.state
  let scene := { scene with currentNPCShowingChatBubble :=
    (result.effects.hovering.map (fun n => (n : Int))).getD scene.currentNPCShowingChatBubble }
  if !result.effects.closeWindows then return { scene with talk := result.state, state }
  if (localize result.state.text.toKey).isNone then return { scene with talk, state }
  playDialogueSound result.effects
  let scene := scene.finishGuideCraft
  let outerRandom : UnifiedRandom := ⟨result.rng.table, result.rng.inext⟩
  let state := scene.state.withOuterRandom outerRandom
  return { scene with
    state := { state with openStorage := none, throwNoThrow := 0 }
    talk := result.state, talkPortraitFrame := 0, inventoryOpen := false, openSign := none, signEdit := none }

private def skyMotion (state : OriginalSession.State) : OriginalForestSky.Motion :=
  let header := state.terrain.header
  let wind := (state.forest.bind (·.weather)).map (·.dry.windCurrent)
  { worldSurface := header.surface, visualWind := wind.getD header.windSpeedTarget,
    dayTime := (state.forest.map (·.clock.dayTime)).getD header.dayTime,
    raining := header.raining, eclipse := header.eclipse }

private def Scene.step (scene : Scene) (keys : UInt64) (target : Option Cell)
    (splashReady : Bool) (mouse : Float32 × Float32) (throwControls : OriginalItemThrow.Controls)
    (windGuidHash : Option Int32) (client : OriginalSession.ClientInput) : Scene := Id.run do
  let movement := {
    left := pressed keys 0
    right := pressed keys 1
    jump := pressed keys 26
    down := pressed keys 44
  }
  -- SkyManager.Update precedes the ambience server and the player update.
  let motion := skyMotion scene.state
  let sky := scene.sky.update { motion }
  let camera := (cameraFor scene.state).skyView
  let viewport : OriginalForestGuideArrow.Viewport := ⟨camera.unscaledPosition, screenWidth, screenHeight⟩
  let state := scene.state.stepClient movement client target throwControls splashReady
    (some mouse) windGuidHash (some viewport) scene.talk.talkNPC
  let sky := Id.run do
    let some forest := state.forest | return sky
    if motion.raining || forest.weatherBoundary.isSome then return sky
    let some request := forest.weather.bind (·.skySpawn) | return sky
    let body := state.player.body
    let center := body.position + ⟨(body.width / 2).toFloat32, (body.height / 2).toFloat32⟩
    let view : OriginalForestSky.SpawnView :=
      { playerCenter := center, camera := (cameraFor state).skyView, worldSurface := motion.worldSurface,
        visualWind := (skyMotion state).visualWind }
    return (sky.spawn request view).getD sky
  let existingLids := scene.chestLids.filter fun (index, _) =>
    state.chests[index]?.any (!·.isVacant)
  let chestLids := OriginalChests.updateLids state.openChest existingLids
  let storageGlow := if state.openStorage == scene.state.openStorage then scene.storageGlow else #[]
  let inventoryOpen := if state.health.dead then false
    else scene.inventoryOpen || (scene.state.health.dead && state.mouseItem.isSome)
  let bounds := (cameraFor state).bounds
  let playerCenter := (OriginalCombat.owner state.player.body).center
  let bossBarSlot := OriginalSkeletronPresentation.track scene.bossBarSlot state.combat.actors bounds playerCenter
  let zoneDungeon := state.forest.any fun forest => (forest.playerFlags.lookup "ZoneDungeon").getD false
  let header := state.terrain.header
  let musicCue := OriginalSkeletronPresentation.musicCue state.combat.actors bounds zoneDungeon
    header.width header.height header.surface playerCenter.y
  return { scene.updateTerrain state with
    tick := scene.tick + 1
    -- The update consumed `changeItem` unless the dead player never reached Select.
    changeItem := if scene.state.health.dead then scene.changeItem else none
    blackFade := scene.blackFade || state.blackFade
    chestLids
    sky
    storageGlow
    bossBarSlot
    dungeonMusicMix := scene.dungeonMusicMix.step musicCue
    inventoryOpen }

private def variant (x y : Nat) : Nat := (x * 17 + y * 31) % 3

private def foregroundFrame (scene : Scene) (patch : Option FramePatch)
    (x y : Nat) (tile : OriginalWorld.Tile) : Nat × Nat :=
  let preserved := tile.frame.map fun (sx, sy) => (sx.toNat, sy.toNat)
  let frameNumber := scene.state.tileFrameNumbers.get? (x, y)
  let captured := if scene.dirtyFrames.contains (x, y) then none
    else patch.bind fun frames => frames.foreground x y tile
  ((preserved.or captured).orElse fun _ => terrainFrame scene.state.terrain x y tile frameNumber)
    |>.getD (18 + 18 * frameNumber.getD (variant x y), 18)

private def skyFrame (scene : Scene) (camera : Camera) : OriginalForestNightSky.Frame :=
  let header := scene.state.terrain.header
  let previewColor : OriginalForestSky.Rgba := ⟨0x79, 0xac, 0xde, 0xff⟩
  if header.raining || header.eclipse || header.bloodMoon then
    ⟨⟨previewColor, previewColor, 1⟩, none⟩
  else
    let clock := scene.state.forest.map (·.clock)
    OriginalForestNightSky.frame {
      time := (clock.map (·.time)).getD header.time
      moonPhase := (clock.map (·.moonPhase)).getD header.moonPhase
      worldWidth := header.width
      worldSurface := header.surface
      screenY := OriginalCollision.single camera.y
      screenHeight, screenWidth, dayTime := scene.state.dayTime, moonType := header.moonType
      previousAtmosphere := scene.skyAtmosphere }

private def skyTint (c : OriginalForestSky.Rgba) : UInt32 :=
  (c.r.toUInt32 <<< 24) ||| (c.g.toUInt32 <<< 16) ||| (c.b.toUInt32 <<< 8) ||| c.a.toUInt32

private def drawSky (sky : OriginalForestSky.Sky) (camera : Camera)
    (lighting : OriginalForestSkyLighting.Lighting) (minDepth maxDepth : Float32) :
    IO Unit := do
  let (_, sprites) := sky.draw {
    camera := camera.skyView, color := lighting.color, atmosphere := lighting.atmosphere, minDepth, maxDepth }
  if sprites.isEmpty then return
  premultipliedAlpha true
  try
    for sprite in sprites do
      let source := sprite.source
      let asset := (sprite.atlas.path.drop "Images/".length).toString
      posedSpriteRegion asset source.x.toUInt32 source.y.toUInt32 source.width.toUInt32 source.height.toUInt32
        (skyTint sprite.color) sprite.position.x.toFloat sprite.position.y.toFloat sprite.origin.x.toFloat
        sprite.origin.y.toFloat sprite.scale.toFloat sprite.rotation.toFloat sprite.flipX
  finally premultipliedAlpha false

private def drawBackdrop (camera : Camera) (sky : OriginalForestSky.Sky)
    (frame : OriginalForestNightSky.Frame) (dayTime : Bool) : IO Unit := do
  let lighting := frame.lighting
  let skyColor := if dayTime then 0x79acdeff else skyTint lighting.color
  discard <| originalSprite "Background_0" 0 0 screenWidth screenHeight (color := skyColor)
  if dayTime then
    discard <| originalSprite "Sun" 141 62 46 46
  else if let some moon := frame.moon then
    let asset := (moon.texturePath.drop "Images/".length).toString
    let source := moon.source
    premultipliedAlpha true
    try
      posedSpriteRegion asset source.x.toUInt32 source.y.toUInt32 source.width.toUInt32 source.height.toUInt32
        (skyTint moon.tint) moon.position.x.toFloat moon.position.y.toFloat moon.origin.x.toFloat
        moon.origin.y.toFloat moon.scale.toFloat moon.rotation.toFloat false
    finally premultipliedAlpha false
  -- The surface mountain pass places the ambient depth slices on either side of its texture.
  let mountainDepth : Float32 := 1 / 0.15
  drawSky sky camera lighting mountainDepth (1 / 0.0)
  let start := -((camera.x / 8) % 1024)
  let mountainColor := if dayTime then 0xb9d4efff else skyTint lighting.beforeAtmosphere
  for column in [:3] do
    discard <| originalSprite "Background_7" (start + (column : Int) * 1024) 145 1024 435
      (color := mountainColor)
  drawSky sky camera lighting 5 mountainDepth
  drawSky sky camera lighting 0 5

private def drawWalls (scene : Scene) (camera : Camera)
    (left right top bottom : Nat) : IO Unit := do
  let terrain := scene.state.terrain
  for x in [left:right] do
    for y in [top:bottom] do
      let tile := (terrain.tile? x y).getD default
      if tile.wall == 0 || tile.invisibleWall then continue
      let (dx, dy) := camera.tile x y
      let number := ((scene.state.wallFrames.get? (x, y)).map (·.number)).getD
        (OriginalWallWork.fallbackWallNumber terrain.header.id x y)
      let frame := OriginalWallWork.wallSpriteFrame terrain x y tile.wall number
      let color := if tile.fullbrightWall then 0xffffffff else scene.light.color x y
      discard <| originalSprite s!"Wall_{tile.wall}" (dx - 8) (dy - 8) 32 32
        frame.frameX frame.frameY 32 32 (color := color)

private def drawSlope (asset : String) (dx dy : Int) (sx sy : Nat) (shape : UInt8) (color : UInt32) :
    IO Unit := do
  for column in [:8] do
    let px := column * 2
    let rise := if shape == 1 || shape == 4 then px else 14 - px
    let (skip, h) := if shape ≤ 2 then (rise, 16 - rise) else (0, rise + 2)
    discard <| originalSprite asset (dx + px) (dy + skip) 2 h (sx + px) (sy + skip) 2 h (color := color)

private def drawTree (asset : String) (dx dy : Int) (sx sy : Nat) (color : UInt32) : IO Unit := do
  if sy ≥ 198 && sx ≥ 22 then
    let variation := (sy - 198) / 22 % 3
    if sx == 22 then
      discard <| originalSprite "Tree_Tops_0" (dx - 32) (dy - 66) 80 80
        (variation * 82) 0 80 80 (color := color)
    else
      let sourceX := if sx == 44 then 0 else 42
      let atX := if sx == 44 then dx - 24 else dx
      discard <| originalSprite "Tree_Branches_0" atX (dy - 20) 40 40
        sourceX (variation * 42) 40 40 (color := color)
  else
    discard <| originalSprite asset (dx - 2) dy 20 20 sx sy 20 20 (color := color)

private def Scene.chestLidOffset (scene : Scene) (x y sx sy : Nat) : Nat := Id.run do
  let left := x - if sx % 36 == 0 then 0 else 1
  let top := y - if sy % 36 == 0 then 0 else 1
  for (index, ticks) in scene.chestLids do
    if scene.state.chests[index]?.any (fun chest => chest.x == Int.ofNat left && chest.y == Int.ofNat top) then
      return OriginalChests.lidOffset ticks
  return 0

private def drawTileInSolidLayer (id : UInt16) : Bool :=
  [11, 470, 475, 78, 579].contains id || (OriginalTileRules.collision id).solid

private def drawTiles (scene : Scene) (patch : Option FramePatch) (camera : Camera) (solidLayer : Bool)
    (left right top bottom : Nat) : IO Unit := do
  let terrain := scene.state.terrain
  for x in [left:right] do
    for y in [top:bottom] do
      let tile := (terrain.tile? x y).getD default
      let (dx, dy) := camera.tile x y
      if let some id := tile.id then
        if !tile.invisibleBlock && drawTileInSolidLayer id == solidLayer then
          let (sx, sy) := foregroundFrame scene patch x y tile
          let source := s!"Tiles_{id}"
          let color := if tile.actuated then 0xbcbcbcff else if tile.fullbrightBlock then 0xffffffff
            else scene.light.color x y
          if OriginalPlatformFraming.isPlatform id.toNat then
            let inset : Nat := if tile.shape == .halfBlock then 8 else 0
            if let some foot := OriginalPlatformFraming.stairFoot terrain x y then
              discard <| originalSprite source dx (dy + 16) 16 16 foot sy 16 16 (color := color)
            discard <| originalSprite source dx (dy + inset) 16 (16 - inset) sx sy 16 (16 - inset) (color := color)
          else if id == 5 || id == 616 then
            drawTree source dx dy sx sy color
          else if id == 21 || id == 467 then
            let h := if sy == 0 then 16 else 18
            let offset := scene.chestLidOffset x y sx sy
            discard <| originalSprite source dx dy 16 h sx (18 * (sy / 18) + offset) 16 h (color := color)
          else if id == 88 then
            -- TileDrawing.GetTileDrawData wraps the saved horizontal frame after 37 dresser styles.
            let row := sx / 1998
            discard <| originalSprite source dx dy 16 16 (sx % 1998) (sy + row * 36) 16 16
              (color := color)
          else
            match tile.shape with
            | .slope shape => drawSlope source dx dy sx sy shape color
            | .halfBlock =>
              discard <| originalSprite source dx (dy + 8) 16 8 sx sy 16 8 (color := color)
            | .full =>
              let h : Nat := if id == 3 || id == 52 || id == 73 || id == 74 then 20 else 16
              discard <| originalSprite source dx (dy + 16 - (h : Int)) 16 h sx sy 16 h (color := color)

/-- The dead player: head, body and legs at the offsets and rotations `Player.UpdateDead` gives them
(`OriginalPlayerDeath.CorpseParts`), each turned about its own centre — `headVect` (20, 22.4),
`bodyVect` (20, 28), `legVect` (20, 42) of the 40×56 frame (PlayerDrawSet.BoringSetup IL_5b44..5be8) —
and every colour multiplied by `(255 − immuneAlpha) / 255` (`Player.GetImmuneAlphaPure`). The layers
are the same simplified ones the living sprite uses; this is not checked against a native draw. -/
private def drawCorpse (scene : Scene) (camera : Camera) (parts : OriginalPlayerDeath.CorpseParts) : IO Unit := do
  let player := scene.state.player
  let body := player.body
  let x := (body.position.x.toInt32.toInt - camera.x - 10).toFloat
  let y := ((body.position.y + body.gfxOffY).toInt32.toInt - camera.y - 14).toFloat
  let left := player.direction < 0
  let fade : Float := (255 - parts.immuneAlpha).toNat.toFloat / 255
  let shade (rgba : UInt32) : UInt32 :=
    let channel (shift : UInt32) := (((rgba >>> shift) &&& 255).toNat.toFloat * fade).toUInt32 <<< shift
    channel 24 ||| channel 16 ||| channel 8 ||| channel 0
  let look := scene.appearance
  let draw (asset : String) (color : UInt32) (offset : OriginalCollision.Vector) (pivotX pivotY : Float)
      (rotation : Float32) :=
    posedSprite asset 0 40 56 (shade color) (x + pivotX + offset.x.toFloat) (y + pivotY + offset.y.toFloat)
      pivotX pivotY 1 rotation.toFloat left
  premultipliedAlpha true
  try
    for (index, color) in [(11, look.pantsColor), (12, look.shoeColor)] do
      draw s!"Player_{look.skinVariant}_{index}" color parts.legPosition 20 42 parts.legRotation
    for (index, color) in [(3, look.skinColor), (4, look.underShirtColor), (6, look.shirtColor)] do
      draw s!"Player_{look.skinVariant}_{index}" color parts.bodyPosition 20 28 parts.bodyRotation
    for (index, color) in [(0, look.skinColor), (1, 0xfff8e4ff), (2, look.eyeColor)] do
      draw s!"Player_{look.skinVariant}_{index}" color parts.headPosition 20 22.4 parts.headRotation
    draw s!"Player_Hair_{look.hair.toNat + 1}" look.hairColor parts.headPosition 20 22.4 parts.headRotation
  finally premultipliedAlpha false

-- BEGIN B-ZONE playerdraw-armor (S05)
/-- The loaded asset name of a layer texture (`TextureAssets`: `Images/…` without the prefix). -/
private def armorAsset : OriginalPlayerArmorDraw.Tex → String
  | .armorHead id => s!"Armor_Head_{id}"
  | .armorBody id => s!"Armor_Body_{id}"
  | .femaleBody id => s!"Female_Body_{id}"
  | .armorBodyComposite id => s!"Armor/Armor_{id}"
  | .armorLeg id => s!"Armor_Legs_{id}"
  | .players skin part => s!"Player_{skin}_{part}"
  | .playerHair id => s!"Player_Hair_{id + 1}"
  | .playerHairAlt id => s!"Player_HairAlt_{id + 1}"
  | .glowMask id => s!"Glow_{id}"
  | .extra id => s!"Extra_{id}"

/-- Buffs whose `UpdateBuffs` sets a player flag that `BoringSetup` IL_2a2c‥4907 turns into a visual effect
(honey … witheredWeapon: poisoned, venom, ichor, on fire 1–3, frostburn 1–2, burned, shadowflame, slimed,
lovestruck, stinky, withered armor/weapon, wet, sparkle slime). Declared mapping, not read from the CIL. -/
private def visualEffectBuffs : Array Nat :=
  #[20, 70, 69, 24, 39, 323, 44, 324, 67, 153, 197, 119, 120, 195, 196, 48, 103, 320]

/-- `LegacyPlayerRenderer.DrawPlayerFull` IL_0876..08d5 for the ordinary unmounted living player: `DrawPlayer`
at `position + netOffset + (0, gfxOffY)` with shadow 0, through `OriginalPlayerDrawRuntime.drawArmor` (leggings,
torso, head layers in `DrawDataCache` order, lit by the session's lighting runtime). Returns `some why` (and draws
nothing) when the leaf reports `OutOfScope` or a layer cannot be rendered faithfully here; the caller then uses the
simplified route and records the boundary. Pose: `bodyFrame` row 0 and `legFrame` row `walking` of the window's
existing frame animation (the rest of `PlayerFrame` is not in the leaf). Declared: `eyeFrameToShow` 0 (no blink),
`immuneAlpha` 0 (the immune blink is not tracked; reported as a boundary while it would blink), no hair/skin dye. -/
private def drawArmorLayers (scene : Scene) (camera : Camera) (walking : Nat) : IO (Option String) := do
  let state := scene.state
  let body := state.player.body
  let health := state.health
  let live (id : Nat) := (health.buffType.zip health.buffTime).any fun (b, t) => b == id && t > 0
  let equipment := state.equipment
  -- inputs this window does not know: refuse instead of drawing them wrongly
  if (equipment.dye.extract 0 3).any (·.item != 0) then return some "armor dye (shader) equipped"
  if ((List.range 20).filter (fun i => (3 ≤ i && i ≤ 9) || 13 ≤ i)).any (fun i => (equipment.armor[i]?.map (·.item)).getD 0 != 0) then
    return some "accessory equipped (its draw slots are not mapped)"
  if live 156 then return some "stoned (DrawPlayerStoned)"
  if live 353 then return some "shimmering (shimmerTransparency)"
  if live 10 then return none  -- Invisibility: DrawPlayerFull IL_08b2 skips DrawPlayer
  let look := scene.appearance
  let color (rgba : UInt32) : OriginalPlayerArmorDraw.Color :=
    ⟨(rgba >>> 24).toUInt8, (rgba >>> 16).toUInt8, (rgba >>> 8).toUInt8, rgba.toUInt8⟩
  let held := ((state.equipped scene.selected).map (·.item.toNat)).getD 0
  let player : OriginalPlayerArmorDraw.Player := {
    position := ⟨body.position.x, body.position.y⟩, width := Int32.ofNat body.width, height := Int32.ofNat body.height
    direction := if state.player.direction < 0 then -1 else 1, gravDir := 1
    skinVariant := Int32.ofNat look.skinVariant.toNat, hair := Int32.ofNat look.hair.toNat
    bodyFrame := ⟨0, 0, 40, 56⟩, legFrame := ⟨0, Int32.ofNat (walking * 56), 40, 56⟩
    hairColor := color look.hairColor, skinColor := color look.skinColor, eyeColor := color look.eyeColor
    shirtColor := color look.shirtColor, underShirtColor := color look.underShirtColor
    pantsColor := color look.pantsColor, shoeColor := color look.shoeColor
    heldItemType := Int32.ofNat held
    effectFlags := visualEffectBuffs.toList.map fun id => (s!"buff {id}", live id) }
  -- `TextureAssets.Players[skin, 15]` as actually loaded
  let eyelidName := s!"Player_{look.skinVariant}_15"
  let eyelidSize ← loadedTextureSize eyelidName
  let eyelid : Int32 → Option (Int32 × Int32) := fun skin =>
    if skin == player.skinVariant && eyelidSize != 0 then
      some (Int32.ofNat (eyelidSize &&& 0xffffffff).toNat, Int32.ofNat (eyelidSize >>> 32).toNat)
    else none
  let context := OriginalPlayerDrawRuntime.context state.lighting
    ⟨OriginalCollision.single camera.x, OriginalCollision.single camera.y⟩ eyelid
  let drawPosition : OriginalPlayerArmorDraw.Vector2 := ⟨body.position.x, body.position.y + body.gfxOffY⟩
  let result ← match OriginalPlayerDrawRuntime.drawArmor context equipment player drawPosition 0 with
    | .ok r => pure r
    | .error why => return some s!"OutOfScope {reprStr why}"
  let layers := result.leggings ++ result.torso ++ result.head
  -- validate every layer before drawing any of them
  let mut plan : Array (String × OriginalPlayerArmorDraw.Rect × OriginalPlayerArmorDraw.DrawData) := #[]
  for d in layers do
    let some src := d.sourceRect | return some s!"layer {reprStr d.texture} without a source rectangle"
    if src.x < 0 || src.y < 0 || src.w ≤ 0 || src.h ≤ 0 then return some s!"layer {reprStr d.texture}: source {reprStr src}"
    if d.effect &&& 2 != 0 then return some "vertical flip (gravity)"
    let hairLayer := match d.texture with | .playerHair _ | .playerHairAlt _ => true | _ => false
    -- 1000/1001: the hair shader packing of an undyed player (PackShader(0 or 1, HairShader))
    if d.shader != 0 && !(hairLayer && (d.shader == 1000 || d.shader == 1001)) then
      return some s!"layer {reprStr d.texture}: shader {d.shader}"
    let asset := armorAsset d.texture
    let size ← loadedTextureSize asset
    if size == 0 then return some s!"texture {asset} not loaded"
    let (tw, th) := ((size &&& 0xffffffff).toNat, (size >>> 32).toNat)
    if src.x.toInt.toNat + src.w.toInt.toNat > tw || src.y.toInt.toNat + src.h.toInt.toNat > th then
      return some s!"layer {asset}: source {reprStr src} outside {tw}×{th}"
    plan := plan.push (asset, src, d)
  premultipliedAlpha true
  try
    for (asset, src, d) in plan do
      let c := d.color
      let rgba := (c.r.toUInt32 <<< 24) ||| (c.g.toUInt32 <<< 16) ||| (c.b.toUInt32 <<< 8) ||| c.a.toUInt32
      posedSpriteRegion asset src.x.toInt.toNat.toUInt32 src.y.toInt.toNat.toUInt32
        src.w.toInt.toNat.toUInt32 src.h.toInt.toNat.toUInt32 rgba
        d.position.x.toFloat d.position.y.toFloat d.origin.x.toFloat d.origin.y.toFloat
        d.scale.toFloat d.rotation.toFloat (d.effect &&& 1 != 0)
  finally premultipliedAlpha false
  -- drawn; the immune blink is the one declared input that is visibly off
  if health.immune && !health.immuneNoBlink then return some "drawn with immuneAlpha 0 (immune blink not tracked)"
  return none
-- END B-ZONE playerdraw-armor

private def drawPlayer (scene : Scene) (camera : Camera) : IO (Option String) := do
  if scene.state.health.dead then
    if let some parts := scene.state.corpseParts then drawCorpse scene camera parts
    return none
  let player := scene.state.player
  let body := player.body
  let x := body.position.x.toInt32.toInt - camera.x - 10
  let y := (body.position.y + body.gfxOffY).toInt32.toInt - camera.y - 14
  let left := player.direction < 0
  let walking := if body.velocity.y != 0 then 5
    else if body.velocity.x != 0 then 6 + scene.tick / 4 % 14 else 0
  let layer (asset : String) (sy : Nat) (color : UInt32) := do
    discard <| originalSprite asset x y 40 56 0 sy 40 56 (color := color) (mirror := left)
  let look := scene.appearance
  let part (index sy : Nat) (color : UInt32) :=
    layer s!"Player_{look.skinVariant}_{index}" sy color
  -- BEGIN B-ZONE playerdraw-route (S05): the armor layers, else the simplified route with the boundary
  let boundary ← drawArmorLayers scene camera walking
  let drawnByLayers := boundary.isNone || (boundary.any (·.startsWith "drawn with"))
  if !drawnByLayers then
    part 0 0 look.skinColor
    part 1 0 0xfff8e4ff
    part 2 0 look.eyeColor
    layer s!"Player_Hair_{look.hair.toNat + 1}" 0 look.hairColor
    part 3 0 look.skinColor
    part 4 0 look.underShirtColor
    part 6 0 look.shirtColor
    part 11 (walking * 56) look.pantsColor
    part 12 (walking * 56) look.shoeColor
  -- END B-ZONE playerdraw-route
  let animation := scene.state.itemUse.itemAnimation
  let item := (scene.state.equipped scene.selected).map (·.item)
  if animation > 0 then
    if scene.state.combat.weapon == some 24 then
      let use := scene.state.combat.use
      let sx := use.itemLocation.x.toInt32.toInt - camera.x
      let sy := (use.itemLocation.y + body.gfxOffY).toInt32.toInt - camera.y
      posedSprite "Item_24" 0 32 32 0xffffffff sx.toFloat sy.toFloat (if left then 32 else 0) 32
        1 use.itemRotation.toFloat left
      return boundary
    if let some id := item then
      if id == copperPickaxe || id == copperAxe || (OriginalWallWork.Tables.hammerStats id).isSome then
        let degreesPerTick := if id == copperAxe then 6.0 else 7.8
        let degreesPerTick := if (OriginalWallWork.Tables.hammerStats id).isSome then
          170.0 / (max 1 scene.state.itemUse.itemAnimationMax).toFloat else degreesPerTick
        let angle := 100.0 - animation.toFloat * degreesPerTick
        discard <| originalSprite s!"Item_{id}" (if left then x - 8 else x + 22) (y + 12) 23 23
          (fit := true) (mirror := left) (rotation := if left then -angle else angle)
      else if id == 36 || id == 94 || (stableTileForItem id).isSome ||
          (OriginalChestMining.placeFor id).isSome || OriginalLiquidBuckets.isBucket id ||
          (OriginalWallWork.Tables.wallItem id).isSome then
        discard <| originalSprite s!"Item_{id}" (if left then x - 2 else x + 24) (y + 26) 14 14
            (fit := true) (mirror := left)
  return boundary

private def drawSlimes (scene : Scene) (camera : Camera) : IO Unit := do
  for slime in scene.state.combat.slimes do
    if !slime.active then continue
    let x := slime.position.x.toFloat + (slime.width / 2).toFloat - camera.x.toFloat
    let y := slime.position.y.toFloat + slime.height.toFloat + slime.gfxOffY.toFloat - camera.y.toFloat
    if x < -32 || x ≥ screenWidth.toFloat + 32 || y < 0 || y ≥ screenHeight.toFloat + 26 then continue
    let light := scene.light.color (slime.position.x / 16).toUInt32.toNat
      (slime.position.y / 16).toUInt32.toNat
    let base := (light &&& 0xffffff00) ||| (255 - slime.alpha).toUInt32
    let colored := slime.color.rgba
    let sy := slime.frameY.toNat.toUInt32
    posedSprite "NPC_1" sy 32 26 base x y 16 26 slime.scale.toFloat slime.rotation.toFloat
      (slime.spriteDirection == 1)
    posedSprite "NPC_1" sy 32 26 colored x y 16 26 slime.scale.toFloat slime.rotation.toFloat
      (slime.spriteDirection == 1)

private def drawGhosts (scene : Scene) (camera : Camera) : IO Unit := do
  for ghost in scene.state.combat.ghosts do
    if !ghost.active then continue
    let x := ghost.position.x.toFloat + (ghost.width / 2).toFloat - camera.x.toFloat
    let y := ghost.position.y.toFloat + ghost.height.toFloat + ghost.gfxOffY.toFloat - camera.y.toFloat
    if x < -56 || x ≥ screenWidth.toFloat + 56 || y < -56 || y ≥ screenHeight.toFloat + 56 then continue
    let light := scene.light.color (ghost.position.x / 16).toUInt32.toNat
      (ghost.position.y / 16).toUInt32.toNat
    let tint := (light &&& 0xffffff00) ||| (255 - ghost.alpha).toUInt32
    let (_, sy, width, height) := ghost.textureFrame
    posedSprite "NPC_316" sy.toUInt32 width.toUInt32 height.toUInt32 tint x y
      (width / 2).toFloat height.toFloat ghost.scale.toFloat ghost.rotation.toFloat (ghost.spriteDirection == 1)

private def drawResidents (scene : Scene) (camera : Camera) : IO Unit := do
  for actor in scene.state.combat.actors do
    let some bound := actor.boundMechanic? | continue
    let n := bound.body
    if !n.active then continue
    let center := n.center
    let x := center.x.toFloat - camera.x.toFloat
    let y := n.position.y.toFloat + n.height.toFloat + n.gfxOffY.toFloat - camera.y.toFloat
    if x < -42 || x ≥ screenWidth.toFloat + 42 || y < -34 || y ≥ screenHeight.toFloat + 34 then continue
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    posedSprite "NPC_123" 0 42 34 tint x y 21 34 n.scale.toFloat 0 (n.spriteDirection == 1)
  for resident in scene.state.combat.residents do
    if !resident.active then continue
    let center := resident.center
    let x := center.x.toFloat - camera.x.toFloat
    let y := resident.position.y.toFloat + resident.height.toFloat + resident.gfxOffY.toFloat - camera.y.toFloat
    if x < -56 || x ≥ screenWidth.toFloat + 56 || y < -56 || y ≥ screenHeight.toFloat + 56 then continue
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    let (_, sy, width, height) := resident.textureFrame
    posedSprite resident.kind.textureName sy.toUInt32 width.toUInt32 height.toUInt32 tint x y
      (width / 2).toFloat height.toFloat 1 0 (resident.spriteDirection == 1)

private def drawAngryBones (scene : Scene) (camera : Camera) : IO Unit := do
  for actor in scene.state.combat.actors do
    let some enemy := actor.angryBones? | continue
    let n := enemy.body
    if !n.active then continue
    let center := n.center
    let x := center.x.toFloat - camera.x.toFloat
    let y := n.position.y.toFloat + n.height.toFloat + n.gfxOffY.toFloat - camera.y.toFloat
    if x < -56 || x ≥ screenWidth.toFloat + 56 || y < -56 || y ≥ screenHeight.toFloat + 56 then continue
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    posedSprite s!"NPC_{enemy.npcType}" (n.frameY * 56).toNat.toUInt32 40 56 tint x y
      20 56 n.scale.toFloat n.rotation.toFloat (n.spriteDirection == 1)

private def drawDarkCasters (scene : Scene) (camera : Camera) : IO Unit := do
  for actor in scene.state.combat.actors do
    let some enemy := actor.darkCaster? | continue
    let n := enemy.body
    -- Water spheres have a fully transparent NPC sprite. The trail is drawn by DrawDust.
    if !n.active || enemy.kind == .waterSphere then continue
    let center := n.center
    let sprite := OriginalDarkCaster.npcSprite enemy camera.skyView.unscaledPosition 40 56
      (scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat)
    let x := sprite.position.x.toFloat
    let y := sprite.position.y.toFloat
    if x < -40 || x ≥ screenWidth.toFloat + 40 || y < -56 || y ≥ screenHeight.toFloat + 56 then continue
    posedSprite "NPC_32" sprite.source.y.toNat.toUInt32 40 56 sprite.rgba x y
      sprite.origin.x.toFloat sprite.origin.y.toFloat sprite.scale.toFloat sprite.rotation.toFloat sprite.flip

private def drawDungeonSlimes (scene : Scene) (camera : Camera) : IO Unit := do
  for actor in scene.state.combat.actors do
    let some slime := actor.dungeonSlime? | continue
    if !slime.active then continue
    let center := slime.center
    let x := center.x.toFloat - camera.x.toFloat
    let y := center.y.toFloat - camera.y.toFloat
    if x < -44 || x ≥ screenWidth.toFloat + 44 || y < -68 || y ≥ screenHeight.toFloat + 68 then continue
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    let channel (shift : UInt32) := ((tint >>> shift) &&& 255).toNat
    let light : OriginalGreenSlime.Color := ⟨channel 24, channel 16, channel 8, channel 0⟩
    let key := OriginalDungeonSlime.keySprite slime camera.skyView.unscaledPosition
    posedSprite "Item_327" 0 14 24 tint key.position.x.toFloat key.position.y.toFloat
      key.origin.x.toFloat key.origin.y.toFloat key.scale.toFloat key.rotation.toFloat false
    let body := OriginalDungeonSlime.bodySprite slime camera.skyView.unscaledPosition light
    let (_, sy, width, height) := body.source
    posedSprite "NPC_71" sy.toNat.toUInt32 width.toNat.toUInt32 height.toNat.toUInt32 body.color.rgba
      body.position.x.toFloat body.position.y.toFloat body.origin.x.toFloat body.origin.y.toFloat
      body.scale.toFloat body.rotation.toFloat body.flip

private def drawCursedSkulls (scene : Scene) (camera : Camera) : IO Unit := do
  for actor in scene.state.combat.actors do
    let some enemy := actor.cursedSkull? | continue
    if !enemy.body.active then continue
    -- The production update retains frame.Y in pixels (0, 30, 60).
    -- Style 10 uses white even in unlit tiles.
    let sprite := OriginalCursedSkull.npcSprite enemy camera.skyView.unscaledPosition
    let x := sprite.position.x.toFloat
    let y := sprite.position.y.toFloat
    if x < -28 || x ≥ screenWidth.toFloat + 28 || y < -30 || y ≥ screenHeight.toFloat + 30 then continue
    posedSprite "NPC_34" sprite.source.y.toNat.toUInt32 28 30 sprite.rgba x y
      sprite.origin.x.toFloat sprite.origin.y.toFloat sprite.scale.toFloat sprite.rotation.toFloat sprite.flip

private def drawSkeletronBone (camera : Camera) (wrist direction : OriginalCollision.Vector)
    (reach : Float32) (color : UInt32) : IO OriginalCollision.Vector := do
  let length := (direction.x * direction.x + direction.y * direction.y).toFloat.sqrt.toFloat32
  if length == 0 then return ⟨0, 0⟩
  let offset := ⟨direction.x * (reach / length), direction.y * (reach / length)⟩
  let p := wrist + offset
  let rotation := (Float.atan2 direction.y.toFloat direction.x.toFloat).toFloat32 - 1.57
  posedSprite "Arm_Bone" 0 26 98 color (p.x.toFloat - camera.x.toFloat) (p.y.toFloat - camera.y.toFloat)
    13 49 1 rotation.toFloat false
  return offset

private def drawSkeletron (scene : Scene) (camera : Camera) : IO Unit := do
  for actor in scene.state.combat.actors do
    let some part := actor.skeletron? | continue
    if !part.active then continue
    let center := OriginalSkeletron.Part.center part
    let x := center.x.toFloat - camera.x.toFloat
    let y := center.y.toFloat - camera.y.toFloat
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    if part.typeId == 35 then
      posedSprite "NPC_35" 0 114 132 tint x y 57 77 1 part.rotation.toFloat false
    else if part.typeId == OriginalSkeletron.guardianType then
      posedSprite "NPC_68" 0 80 102 tint x y 40 51 1 part.rotation.toFloat false
    else
      let head := scene.state.combat.actors[part.ai1.toUInt32.toNat]?.bind OriginalCombat.Actor.skeletron?
      if let some head := head.filter (·.active) then
        let side := -part.ai0
        let headCenter := OriginalSkeletron.Part.center head
        let wrist : OriginalCollision.Vector := ⟨center.x + 5 * side, part.position.y + 20⟩
        let offset ← drawSkeletronBone camera wrist ⟨headCenter.x - wrist.x + 200 * side, headCenter.y - wrist.y + 130⟩ 92 tint
        let joint := wrist + offset + ⟨offset.x / 2, offset.y / 2⟩
        discard <| drawSkeletronBone camera joint ⟨headCenter.x - joint.x + 50 * side, headCenter.y - joint.y + 80⟩ 60 tint
      posedSprite "NPC_36" 0 58 78 tint x y 29 48 1 part.rotation.toFloat (part.spriteDirection > 0)

private def drawResidentEmotes (scene : Scene) (camera : Camera) : IO Unit := do
  for resident in scene.state.combat.residents do
    let some emote := resident.emote | continue
    if !resident.active then continue
    let offset := OriginalCollision.single (-resident.direction * resident.width) * 0.75
    let x := (resident.center.x + offset - OriginalCollision.single camera.x).toFloat.floor
    let y := (resident.position.y + 2 - OriginalCollision.single camera.y).toFloat.floor
    let age := emote.duration - emote.timeLeft
    let closed := age < 6 || emote.timeLeft < 6
    let mirror := resident.direction != -1
    posedSpriteRegion "Extra_48" (if closed then 0 else 34) 0 34 28 0xffffffff x y 17 28 1 0 mirror
    if !closed then
      let frame := (2 * emote.id) % 8 + (age / 8) % 2
      let row := 1 + emote.id / 4
      let fixedDirection := mirror && (emote.id == 87 || emote.id == 89)
      posedSpriteRegion "Extra_48" (34 * frame).toUInt32 (28 * row).toUInt32 34 28 0xffffffff
        (if fixedDirection then x + 4 else x) y 17 28 1 0 (mirror && !fixedDirection)

private def drawBunnies (scene : Scene) (camera : Camera) : IO Unit := do
  for bunny in scene.state.combat.bunnies do
    if !bunny.active then continue
    let center := bunny.center
    let x := center.x.toFloat - camera.x.toFloat
    let y := bunny.position.y.toFloat + bunny.height.toFloat + bunny.gfxOffY.toFloat - camera.y.toFloat
    if x < -48 || x ≥ screenWidth.toFloat + 48 || y < -40 || y ≥ screenHeight.toFloat + 40 then continue
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    posedSprite "NPC_46" bunny.frameY.toNat.toUInt32 48 40 tint x y 24 40 1 0 (bunny.spriteDirection == 1)

private def drawBirds (scene : Scene) (camera : Camera) : IO Unit := do
  for bird in scene.state.combat.birds do
    if !bird.active then continue
    let cx := bird.position.x.toFloat + (bird.width / 2).toFloat
    let cy := bird.position.y.toFloat + (bird.height / 2).toFloat
    let x := cx - camera.x.toFloat
    let y := cy + (bird.height / 2).toFloat + bird.gfxOffY.toFloat - camera.y.toFloat
    if x < -30 || x ≥ screenWidth.toFloat + 30 || y < -24 || y ≥ screenHeight.toFloat + 24 then continue
    let tint := scene.light.color (cx / 16).toUInt64.toNat (cy / 16).toUInt64.toNat
    let (_, sy, width, height) := bird.textureFrame
    posedSprite bird.kind.textureName sy.toUInt32 width.toUInt32 height.toUInt32 tint x y
      (width / 2).toFloat height.toFloat 1 bird.rotation.toFloat (bird.spriteDirection == 1)

private def drawSnails (scene : Scene) (camera : Camera) : IO Unit := do
  for snail in scene.state.combat.snails do
    if !snail.active then continue
    let center := snail.center
    let x := center.x - OriginalCollision.single camera.x
    let y := snail.position.y + OriginalCollision.single snail.height - OriginalCollision.single camera.y
      + ((-120 : Float32) * snail.scale / 6 + 4 + 10 * snail.scale + snail.gfxOffY)
    if x < -30 || x > screenWidth.toFloat32 + 30 || y < -20 || y > screenHeight.toFloat32 + 20 then continue
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    posedSprite "NPC_359" snail.frameY.toNat.toUInt32 30 20 tint x.toFloat y.toFloat
      15 10 snail.scale.toFloat snail.rotation.toFloat (snail.spriteDirection == 1)

private def drawStab (scene : Scene) (camera : Camera) : IO Unit := do
  for slot in scene.state.combat.projectileList do
    let .stab stab := slot | continue
    if !stab.active then continue
    let center := stab.center
    let x := center.x.toFloat - camera.x.toFloat
    let y := center.y.toFloat - camera.y.toFloat
    let alpha := (255 - stab.alpha).toNat.toUInt32
    let rotation := stab.rotation - 0.7853981852531433 * Float32.ofInt stab.spriteDirection
    posedSprite "Projectile_938" 0 32 32 (0xffffff00 ||| alpha) x y 16 16 1 rotation.toFloat
      (stab.spriteDirection == -1)

private def drawGuideArrows (scene : Scene) (camera : Camera) : IO Unit := do
  for slot in scene.state.combat.projectileList do
    -- BEGIN B-ZONE projectiles-draw (S03): the player's Wooden Arrows are the same type-1 projectile
    -- (`DrawProjDirect` geometry of `Projectile_1`) as the Guide's
    let arrow : OriginalForestGuideArrow.Arrow ← match slot with
      | .guideArrow arrow => pure arrow
      | .woodenArrow a =>
        pure { position := a.position, velocity := a.velocity, owner := a.owner, damage := a.damage
               knockback := a.knockback, rotation := a.rotation, active := a.active }
      | _ => continue
    -- END B-ZONE projectiles-draw
    let some draw := OriginalForestGuideArrow.draw arrow camera.skyView.unscaledPosition | continue
    let x := draw.position.x.toFloat
    let y := draw.position.y.toFloat
    if x < -32 || x > screenWidth.toFloat + 32 || y < -32 || y > screenHeight.toFloat + 32 then continue
    let tint := scene.light.color
      ((arrow.position.x + 5) / 16).toUInt32.toNat ((arrow.position.y + 5) / 16).toUInt32.toNat
    posedSpriteRegion "Projectile_1" draw.source.x.toNat.toUInt32 draw.source.y.toNat.toUInt32
      draw.source.w.toNat.toUInt32 draw.source.h.toNat.toUInt32 tint x y draw.origin.x.toFloat draw.origin.y.toFloat
      draw.scale.x.toFloat draw.rotation.toFloat draw.flipX

private def Scene.prepareManaDust (scene : Scene) (camera : Camera) : Scene := Id.run do
  let view : OriginalForestGuideArrow.Viewport := ⟨camera.skyView.unscaledPosition,screenWidth,screenHeight⟩
  let mut manaDust := #[]
  let mut manaDraws := #[]
  for p in scene.state.combat.manaDust do
    if p.slot ≥ scene.state.combat.dustDrawBudget then
      manaDust := manaDust.push p
      continue
    let drawn := p.draw view
    if !drawn.sprites.isEmpty then manaDraws := manaDraws.push (p.slot,drawn.sprites)
    if drawn.particle.active then
      manaDust := manaDust.push drawn.particle
  let mut manaRefillDust := #[]
  for p in scene.state.combat.manaRefillDust do
    if p.dust.slot ≥ scene.state.combat.dustDrawBudget then
      manaRefillDust := manaRefillDust.push p
      continue
    let position := p.dust.position
    let light := scene.light.color
      ((position.x.toFloat + 4).toUInt64.toNat / 16) ((position.y.toFloat + 4).toUInt64.toNat / 16)
    let drawn := p.draw view light
    if !drawn.sprites.isEmpty then manaDraws := manaDraws.push (p.dust.slot,drawn.sprites)
    if drawn.particle.active then
      manaRefillDust := manaRefillDust.push drawn.particle
  return { scene with manaDraws, state := { scene.state with combat := { scene.state.combat with manaDust, manaRefillDust } } }

private def Scene.prepareSpikeBalls (scene : Scene) (camera : Camera) : Scene := Id.run do
  let mut actors := scene.state.combat.actors
  let mut sprites := #[]
  let lighting := fun (x y : Int) =>
    let tint := scene.light.color x y
    (⟨(tint >>> 24).toNat,((tint >>> 16) &&& 255).toNat,
      ((tint >>> 8) &&& 255).toNat,(tint &&& 255).toNat⟩ : OriginalGreenSlime.Color)
  let screen := camera.skyView.unscaledPosition
  let visible : OriginalStarterMelee.Rect :=
    ⟨screen.x.toInt32.toInt - 800, screen.y.toInt32.toInt - 800,
      (screenWidth : Int) + 1600, (screenHeight : Int) + 1600⟩
  for offset in [:actors.size] do
    let index := actors.size - 1 - offset
    let some enemy := actors[index]!.dungeonSpikeBall? | continue
    if !enemy.body.active then continue
    if !visible.intersects enemy.toCombatNpc.rect then continue
    let drawn := OriginalDungeonSpikeBall.draw enemy screen lighting
    actors := actors.set! index (.dungeonSpikeBall drawn.enemy)
    sprites := sprites ++ drawn.sprites
  return { scene with
    spikeBallDraws := sprites
    state := { scene.state with combat := { scene.state.combat with actors } } }

private def drawGuideDust (scene : Scene) (camera : Camera) : IO Scene := do
  -- Draw is a mutation phase: skipped/repeated draws must observe the same
  -- survivor semantics as Main.DrawDust, independently of world updates.
  let scene := scene.prepareManaDust camera
  let view : OriginalForestGuideArrow.Viewport :=
    ⟨camera.skyView.unscaledPosition,screenWidth,screenHeight⟩
  let mut guideDust := #[]
  -- The water dust cohort (rain splashes): Main.DrawDust's per-slot body (camera cull, lit sprite and
  -- overlay, black-light removal). Its sprites go before the other cohorts' (draw order only).
  let mut water := scene.state.combat.waterDust
  let waterContext : OriginalWaterDust.DrawContext := {
    screenPosition := camera.skyView.unscaledPosition, screenWidth := screenWidth, screenHeight := screenHeight }
  -- `Lighting.GetColor` as FNA `Color.PackedValue` (`OriginalLightingRuntime.packedValue`)
  let packedLight := fun (x y : Int) => scene.light.value x y
  for i in [:water.size] do
    let some o := water[i]? | continue
    if !o.dust.active || o.slot ≥ scene.state.combat.dustDrawBudget then continue
    match OriginalWaterDust.drawOne waterContext packedLight o.dust with
    | .ok (dust, sprites) =>
      water := water.set! i { o with dust }
      OriginalDesktopDialogue.drawSprites sprites
    | .error why => throw (IO.userError s!"Main.DrawDust (water): {why}")
  let slots := (scene.state.combat.guideDust.mapIdx fun i p => (p.slot,false,i)) ++
    (scene.manaDraws.mapIdx fun i p => (p.1,true,i))
  for (_,mana,index) in slots.qsort (fun a b => a.1 < b.1) do
    if mana then
      OriginalDesktopDialogue.drawSprites scene.manaDraws[index]!.2
      continue
    let some dust := scene.state.combat.guideDust[index]? | continue
    if dust.slot ≥ scene.state.combat.dustDrawBudget then
      guideDust := guideDust.push dust
      continue
    if OriginalForestGuideArrow.supportsBasicGround dust.request.typeId ||
        OriginalSnowIceDust.supports dust.request.typeId ||
        OriginalBloodDust.supports dust.request.typeId then
      let render := if OriginalSnowIceDust.supports dust.request.typeId then OriginalSnowIceDust.drawParticles
        else if OriginalForestGuideArrow.supportsBasicGround dust.request.typeId then OriginalBasicDustDraw.drawParticles
        else OriginalBloodDust.drawParticles
      let drawn := render #[dust] scene.state.combat.dustDrawBudget view fun x y =>
        -- `Lighting.GetColor` of this draw (`OriginalLightingRuntime.packedColor`)
        scene.light.color x y
      OriginalDesktopDialogue.drawSprites drawn.sprites
      guideDust := guideDust ++ drawn.particles
      continue
    guideDust := guideDust.push dust
    let x := dust.position.x.toFloat - camera.x.toFloat
    let y := dust.position.y.toFloat - camera.y.toFloat
    if x < -8 || x > screenWidth.toFloat + 8 || y < -8 || y > screenHeight.toFloat + 8 then continue
    let light := scene.light.color ((dust.position.x.toFloat + 4).toUInt64.toNat / 16)
      ((dust.position.y.toFloat + 4).toUInt64.toNat / 16)
    let typeId := dust.request.typeId
    -- Main.DrawDust lights types 6, 15 and 59..64 with Color.White before Dust.GetAlpha. The
    -- GetAlpha colours are premultiplied, as the original's AlphaBlend sprite batch expects.
    let lit := if typeId == 6 || typeId == 15 || (typeId ≥ 59 && typeId ≤ 64) then 0xffffffff else light
    let (base, overlay, premultiplied) := if typeId == 172 || typeId == 6 then
        (OriginalDarkCaster.dustAlpha dust lit, 0, true)
      else if OriginalHeldItemLight.handlesDust typeId then
        (OriginalHeldItemLight.dustAlpha dust lit, 0, true)
      else
        let (base, overlay) := dust.drawColors light
        (base, overlay, false)
    let draw := fun color => posedSpriteRegion "Dust" dust.frame.x.toNat.toUInt32 dust.frame.y.toNat.toUInt32
      8 8 color x y 4 4 dust.scale.toFloat dust.rotation.toFloat false
    if premultiplied then
      premultipliedAlpha true
      try draw base finally premultipliedAlpha false
    else
      draw base
      if overlay != 0 then draw overlay
  let combat := { scene.state.combat with guideDust, waterDust := water.filter (·.dust.active) }
  return { scene with state := { scene.state with combat } }

/-- `Main.DrawRain`'s sprites: `Images/Rain` crop (type * 4, 0, 2, 40), origin zero. -/
private def drawRainSprites (scene : Scene) : IO Unit := do
  for sprite in scene.rainDraws do
    let c := sprite.color
    let rgba := ((c &&& 255) <<< 24) ||| (((c >>> 8) &&& 255) <<< 16) ||| (((c >>> 16) &&& 255) <<< 8) ||| (c >>> 24)
    posedSpriteRegion "Rain" sprite.frameX.toNat.toUInt32 sprite.frameY.toNat.toUInt32 sprite.width.toNat.toUInt32
      sprite.height.toNat.toUInt32 rgba sprite.position.x.toFloat sprite.position.y.toFloat
      sprite.origin.x.toFloat sprite.origin.y.toFloat sprite.scale.toFloat sprite.rotation.toFloat false

private def drawGravestones (scene : Scene) (camera : Camera) : IO Unit := do
  for slot in scene.state.combat.projectileList do
    let .gravestone grave := slot | continue
    if grave.timeLeft == 0 then continue
    let x := grave.position.x.toFloat + 12 - camera.x.toFloat
    let y := grave.position.y.toFloat + 12 - camera.y.toFloat
    if x < -32 || x ≥ screenWidth.toFloat + 32 || y < -32 || y ≥ screenHeight.toFloat + 32 then continue
    let (width, height) : UInt32 × UInt32 := match grave.projectileType with
      | 43 | 528 => (26, 30)
      | 531 => (28, 32)
      | _ => (32, 32)
    let tint := scene.light.color (grave.position.x / 16).toUInt32.toNat
      (grave.position.y / 16).toUInt32.toNat
    posedSprite s!"Projectile_{grave.projectileType}" 0 width height tint x y
      (width.toNat.toFloat / 2) (height.toNat.toFloat / 2) 1 grave.rotation.toFloat false

private def vitalIcon (amount : Float32) (heart := false) : Nat × UInt32 :=
  let amount := min 1 (max 0 amount)
  let shade := (30 + 225 * amount).toUInt32
  let alpha := if heart then (shade.toNat.toFloat * 0.9).toUInt64.toUInt32 else shade
  let color := (shade <<< 24) ||| (shade <<< 16) ||| (shade <<< 8) ||| alpha
  ((22 * (0.75 + amount / 4)).toUInt32.toNat, color)

private def drawVitals (scene : Scene) (camera : Camera) : IO Unit := do
  let health := scene.state.health
  let goldHearts := (health.statLifeMax - 400).toNat / 5
  let perHeart : Float32 := if goldHearts > 0 then Float32.ofInt health.statLifeMax / 20 else 20
  let hearts := (Float32.ofInt health.statLifeMax2 / perHeart).toUInt32.toNat
  let left := (screenWidth : Int) - 300
  let label := s!"Life: {max 0 health.statLife}/{health.statLifeMax2}"
  let labelWidth ← textWidth label 16
  text label (left + (min 10 hearts * 13 : Nat) - (labelWidth / 2 : Nat)) 6 16 0xffffffff
  premultipliedAlpha true
  try
    for i in [:hearts] do
      let (size, color) := vitalIcon ((Float32.ofInt health.statLife - i.toFloat32 * perHeart) / perHeart) true
      let shift := (22 - size : Nat) / 2
      discard <| originalSprite (if i < goldHearts then "Heart2" else "Heart")
        (left + (26 * (i % 10) + shift : Nat)) (32 + (26 * (i / 10) + shift : Nat)) size size (color := color)
    if !health.dead && health.breath < health.breathMax then
      let body := scene.state.player.body
      let x := body.position.x.toInt32.toInt + (body.width / 2 : Nat) - camera.x - 125
      let y := (body.position.y + body.gfxOffY).toInt32.toInt - camera.y + 32 +
        (if scene.inventoryOpen then (body.height : Int) - 20 else -100)
      for i in [:health.breathMax.toNat / 20] do
        let (size, color) := vitalIcon (Float32.ofInt (health.breath - (20 * i : Nat)) / 20)
        discard <| originalSprite "Bubble" (x + (26 * (i % 10) : Nat))
          (y + (26 * (i / 10) + (22 - size) / 2 : Nat)) size size (color := color)
  finally premultipliedAlpha false

private def drawDeath (health : OriginalPlayerLiquidHealth.Health) : IO Unit := do
  if !health.dead then return
  let fade := min 255 (health.deadTime.toNat * 2)
  let red := (fade * 9 / 10).toUInt32
  let gray := (fade / 2).toUInt32
  let alpha := (fade * 4 / 10).toUInt32
  let color := (red <<< 24) ||| (gray <<< 16) ||| (gray <<< 8) ||| alpha
  let center (label : String) (y : Nat) (scale : Float) (tint : UInt32 := color) :=
    deathText label (screenWidth / 2).toUInt32 y.toUInt32 scale tint
  let y := screenHeight / 2 - 60
  center "You were slain..." y 1
  let seconds := 1 + health.respawnTimer.toNat / 60
  if health.lostCoins > 0 then
    let coins := (#[74, 73, 72, 71] : Array Nat).filterMap fun id =>
      let n := (health.lostCoins / 100 ^ (id - 71)) % 100
      if n == 0 then none else some s!"{n} {#["copper", "silver", "gold", "platinum"][id - 71]!}"
    center s!"Dropped {String.intercalate " " coins.toList}" (y + 50) 0.7
  let skip := health.deadTime ≥ OriginalPlayerLiquidHealth.respawnSkipDelay
  let offset := if health.lostCoins > 0 then 24 else 0
  if skip then
    let amount := min 1 ((health.deadTime - OriginalPlayerLiquidHealth.respawnSkipDelay + 1).toNat.toFloat32 / 60)
    center "Press Space to respawn" (y + 60 + offset) 0.7 (OriginalLiquidRender.scaleColor color amount)
  if health.respawnTimer > 1 then
    center s!"Respawning in {seconds}" (y + (if skip then 104 else 70) + offset) 0.7

private def drawItemSprite (stack : OriginalItems.Stack) (x y : Int) (size : Nat) : IO Unit := do
  let asset := s!"Item_{stack.item}"
  if stack.item != 23 then
    discard <| originalSprite asset x y size size (fit := true)
    return
  -- Gel draws an alpha-175 base followed by the item's color in the original batch.
  premultipliedAlpha true
  try
    discard <| originalSprite asset x y size size (fit := true) (color := 0x50505050)
    discard <| originalSprite asset x y size size (fit := true) (color := stack.color.getD 0x0050ff64)
  finally premultipliedAlpha false

private def drawStack (stack : OriginalItems.Stack) (x y size : Nat) : IO Unit := do
  drawItemSprite stack (x + 8 : Nat) (y + 8 : Nat) (size - 16)
  if stack.count > 1 then
    let label := toString stack.count
    let width ← textWidth label 13
    let left := x + size - width - 4
    text label (left + 1 : Nat) (y + size - 14 : Nat) 13 0x000000ff
    text label left (y + size - 15 : Nat) 13 0xffffffff

private def hueChannel (hue : Float) : UInt32 :=
  let h := if hue < 0 then hue + 1 else if hue > 1 then hue - 1 else hue
  let rgb := if 6 * h < 1 then 6 * h else if 2 * h < 1 then 1
    else if 3 * h < 2 then 6 * (2 / 3 - h) else 0
  let value := rgb * 255
  let lower := value.floor.toUInt32
  let fraction := value - value.floor
  if fraction > 0.5 || (fraction == 0.5 && lower % 2 == 1) then lower + 1 else lower

private def slotGlowColor (glows : Array (Nat × Float32 × Nat))
    (index tick : Nat) (slot : Option OriginalItems.Stack) (base : UInt32) : Option UInt32 := do
  guard (slot.any (fun stack => !stack.favorite && stack.count > 0))
  let (_, hue, expires) ← glows.find? (·.1 == index)
  guard (expires > tick)
  let timer : Float32 := (min (expires - tick) 300).toFloat32 / 300
  let amount := timer * timer / 2
  let blend (shift channel : UInt32) : UInt32 :=
    let background := ((base >>> shift) &&& 255).toNat.toFloat32
    (background + (channel.toNat.toFloat32 - background) * amount).toUInt32
  let h := hue.toFloat
  return (blend 24 (hueChannel (h + 1 / 3)) <<< 24) |||
    (blend 16 (hueChannel h) <<< 16) ||| (blend 8 (hueChannel (h - 1 / 3)) <<< 8) ||| 255

private def drawInventory (scene : Scene) (mouseX mouseY : Nat) : IO Unit := do
  let inventory : OriginalItems.Inventory := scene.state.inventory
  let count := if scene.inventoryOpen then inventory.size else hotbarSize
  for index in [:count] do
    let x := inventoryInset + index % hotbarSize * slotStride
    let y := inventoryInset + index / hotbarSize * slotStride
    let selected := index == scene.selected
    let slot := (inventory[index]?.join).filter (·.count > 0)
    let glow := slotGlowColor scene.inventoryGlow index scene.tick slot 0x3f4197ff
    let skin := if glow.isSome then "Inventory_Back13"
      else if selected then "Inventory_Back20"
      else if slot.any (·.favorite) then "Inventory_Back10"
      else if scene.inventoryOpen && index < hotbarSize then "Inventory_Back9"
      else "Inventory_Back"
    discard <| originalSprite skin x y slotSize slotSize (color := glow.getD 0xffffffff)
    if let some sprites := scene.slotSprites[(0, index)]? then
      OriginalDesktopDialogue.drawSprites sprites
    else
      if index < hotbarSize then
        text (toString ((index + 1) % 10)) (x + 4 : Nat) (y + 2 : Nat) 11 0xffffffe0
      if let some stack := slot then drawStack stack x y slotSize
  if !scene.inventoryOpen then return
  text "Coins" coinLeft (coinTop - 22 : Nat) 14 0xffffffff
  text "Ammo" (coinLeft + sideSlotStep : Nat) (coinTop - 22 : Nat) 14 0xffffffff
  for index in [:scene.state.coinAndAmmo.size] do
    let x := coinLeft + index / 4 * sideSlotStep
    let y := coinTop + index % 4 * sideSlotStep
    let slot := scene.state.coinAndAmmo[index]?.join
    let glow := slotGlowColor scene.inventoryGlow (inventory.size + index) scene.tick slot 0x3f4197ff
    let skin := if glow.isSome then "Inventory_Back13"
      else if index < 4 then "Inventory_Back3" else "Inventory_Back4"
    discard <| originalSprite skin x y sideSlotSize sideSlotSize (color := glow.getD 0xffffffff)
    if let some sprites := scene.slotSprites[(1, index)]? then
      OriginalDesktopDialogue.drawSprites sprites
    else if let some stack := slot then drawStack stack x y sideSlotSize
  if scene.state.openStorage.isNone then
    let asset := if scene.inventorySortAt mouseX mouseY then "UI/Sort_1" else "UI/Sort_0"
    discard <| originalSprite asset inventorySortLeft inventorySortTop inventorySortSize inventorySortSize

private def drawStorage (scene : Scene) (mouseX mouseY : Nat) : IO Unit := do
  let some storage := scene.state.openStorage | return
  let offset := scene.chestRow * hotbarSize
  let limit := chestRows * hotbarSize
  let (name, slots, skin, showFavorites, base) ← match storage with
    | .chest index =>
      let some chest := scene.state.chests[index]? | return ()
      let name := OriginalStorageRename.title scene.rename index chest.name (chestRenameTile scene.state.terrain chest)
      pure ((if name.isEmpty && !scene.rename.editChest then "Chest" else name),
        (chest.items.extract offset (offset + limit)).map (·.bind OriginalChests.stack?),
        "Inventory_Back5", false, 0x683434ff)
    | .bank furniture =>
      let kind := furniture.kind
      pure (kind.name, (scene.state.banks.items kind).extract offset (offset + limit),
        "Inventory_Back2", kind.keepsFavorites, 0x823e66ff)
  text name chestLeft (craftTop - 21 : Nat) 17 0xffffffff
  for (action, index) in storageActions.zipIdx do
    let hovering := storageActionAt mouseX mouseY == some action
    text action.label storageActionLeft (craftTop + storageActionHeight * index + 3)
      (if hovering then 19 else 17) (if hovering then 0xffe880ff else 0xffffffff)
  if scene.state.openChest.isSome then
    let name := if scene.rename.editChest then OriginalStorageRename.Data.saveLabel
      else OriginalStorageRename.Data.renameLabel
    let hovered := scene.renameButtonAt mouseX mouseY
    text name storageActionLeft (craftTop + storageActionHeight * storageActions.size + 3)
      (if hovered then 19 else 17) (if hovered then 0xffe880ff else 0xffffffff)
    if scene.rename.editChest then
      let hovered := scene.renameCancelAt mouseX mouseY
      text OriginalStorageRename.Data.cancelLabel storageActionLeft
        (craftTop + storageActionHeight * (storageActions.size + 1) + 3)
        (if hovered then 19 else 17) (if hovered then 0xffe880ff else 0xffffffff)
  for index in [:slots.size] do
    let x := chestLeft + chestStep (index % hotbarSize)
    let y := craftTop + chestStep (index / hotbarSize)
    let slot := slots[index]?.join
    let glow := slotGlowColor scene.storageGlow (offset + index) scene.tick slot base
    let asset := if glow.isSome then "Inventory_Back13"
      else if showFavorites && slot.any (·.favorite) then "Inventory_Back19" else skin
    discard <| originalSprite asset x y chestSlotSize chestSlotSize (color := glow.getD 0xffffffff)
    if let some sprites := scene.slotSprites[(2, index)]? then
      OriginalDesktopDialogue.drawSprites sprites
    else if let some stack := slot then drawStack stack x y chestSlotSize

private def drawCrafting (scene : Scene) (recipes : Array Nat)
    (mouseX mouseY : Nat) : IO Unit := do
  if !scene.inventoryOpen || scene.guideGridVisible || scene.guideGridOutput.isSome then return
  let hovering := nearbyToggleAt mouseX mouseY
  let texture : Nat := (if scene.nearbyCrafting then 0 else 2) + if hovering then 1 else 0
  discard <| originalSprite s!"UI/ChestCraft_{texture}" nearbyToggleLeft nearbyToggleTop nearbyToggleSize nearbyToggleSize
  if hovering then
    let state := if scene.nearbyCrafting then "On" else "Off"
    text s!"Craft from nearby chests: {state}" (nearbyToggleLeft + 34 : Nat) (nearbyToggleTop - 12 : Nat) 15 0xffffffff
  if recipes.isEmpty then return
  if scene.state.openStorage.isNone then text "Crafting" inventoryInset (craftTop - 23 : Nat) 19 0xffffffff
  for row in [:craftSlots] do
    let some index := recipes[scene.recipeScroll + row]? | break
    let some recipe := catalog.recipes[index]? | continue
    let y := craftTop + slotStride * row
    let selected := scene.recipeFocus == some index
    discard <| originalSprite (if selected then "Inventory_Back20" else "Inventory_Back")
      inventoryInset y slotSize slotSize
    drawStack recipe.output inventoryInset y slotSize
    if selected then
      let left := scene.craftDetailsLeft
      let columns := max 1 ((screenWidth - left) / slotStride)
      let rows := (recipe.ingredients.size + columns - 1) / columns
      let top := min y (screenHeight - rows * slotStride - 26)
      text (OriginalRecipeCatalog.itemName recipe.create) left top 17 0xffffffff
      for i in [:recipe.ingredients.size] do
        let ingredient := recipe.ingredients[i]!
        let x := left + (i % columns) * slotStride
        let y := top + 24 + (i / columns) * slotStride
        discard <| originalSprite "Inventory_Back" x y slotSize slotSize
        drawStack (.ofItem ingredient.item.toUInt16 ingredient.stack) x y slotSize
        if inside mouseX mouseY x y slotSize slotSize then
          let entry := (catalog.lookups[index]?.getD #[])[i]?
          let name := if let some e := entry then
              if catalog.isGroupId e.id then OriginalRecipeCatalog.groupName (e.id - catalog.fakeItemIdOffset)
              else OriginalRecipeCatalog.itemName ingredient.item
            else OriginalRecipeCatalog.itemName ingredient.item
          let width ← textWidth name 15
          let tx := min (mouseX + 14) (screenWidth - width - 12)
          let ty := min (mouseY + 14) (screenHeight - 25)
          rect tx ty (width + 10) 22 0x141827eb
          text name (tx + 5 : Nat) (ty + 2 : Nat) 15 0xffffffff

private def drawGuideMaterial (scene : Scene) (localize : ResidentText) : IO Unit := do
  if scene.guideGridVisible || scene.guideGridOutput.isSome then return
  let some craft := scene.guideCraft | return
  discard <| originalSprite "Inventory_Back" guideSlotLeft guideSlotTop guideSlotSize guideSlotSize
  if let some material := craft.guideItem then
    drawStack material guideSlotLeft guideSlotTop guideSlotSize
  else if let some hint := localize "LegacyInterface.24" then
    text hint (guideSlotLeft + 50 : Nat) (guideSlotTop + 12 : Nat) 18 0xffffffff

private def drawBossBar (scene : Scene) (font : Option OriginalForestChatLayout.Font) : IO Unit := do
  let some slot := scene.bossBarSlot | return
  let some part := scene.state.combat.actors[slot]?.bind OriginalCombat.Actor.skeletron? | return
  let sprites := OriginalSkeletronPresentation.meter screenWidth screenHeight part.life part.lifeMax
  let labels := font.elim #[] fun font =>
    (OriginalSkeletronPresentation.meterText font screenWidth screenHeight part.life part.lifeMax).toOption.getD #[]
  OriginalDesktopDialogue.drawSprites (sprites ++ labels)

private def drawOldShakingChests (scene : Scene) (camera : Camera) : IO Unit := do
  let mut sprites := #[]
  for actor in scene.state.combat.actors do
    let some enemy := actor.oldShakingChest? | continue
    if !enemy.body.active then continue
    let center := enemy.toCombatNpc.center
    let tint := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    let light : OriginalGreenSlime.Color :=
      ⟨(tint >>> 24).toNat,((tint >>> 16) &&& 255).toNat,((tint >>> 8) &&& 255).toNat,(tint &&& 255).toNat⟩
    let pose := OriginalOldSlime.npcSprite enemy camera.skyView.unscaledPosition light
    let (sx,sy,w,h) := pose.source
    let color : OriginalForestChatLayout.Color := ⟨pose.color.r,pose.color.g,pose.color.b,pose.color.a⟩
    sprites := sprites.push (OriginalForestChatLayout.draw "Images/NPC_685" 32 28
      pose.position.x pose.position.y (some ⟨sx,sy,w,h⟩) color pose.rotation pose.origin.x pose.origin.y
      pose.scale.x pose.scale.y (if pose.flip then 1 else 0))
  OriginalDesktopDialogue.drawSprites sprites

/-- Zombies and Demon Eyes: `OriginalNightEnemies.npcSprite`, lit at the NPC centre. -/
private def drawNightEnemies (scene : Scene) (camera : Camera) : IO Unit := do
  let view : OriginalCollision.Vector := ⟨OriginalCollision.single camera.x, OriginalCollision.single camera.y⟩
  let mut sprites := #[]
  for actor in scene.state.combat.actors do
    let some enemy := actor.nightEnemy? | continue
    if !enemy.body.active then continue
    let center := enemy.toCombatNpc.center
    let rgba := scene.light.color (center.x / 16).toUInt32.toNat (center.y / 16).toUInt32.toNat
    let light : OriginalForestChatLayout.Color :=
      ⟨(rgba >>> 24).toNat,((rgba >>> 16) &&& 255).toNat,((rgba >>> 8) &&& 255).toNat,(rgba &&& 255).toNat⟩
    sprites := sprites.push (← IO.ofExcept (OriginalNightEnemies.npcSprite enemy view light))
  OriginalDesktopDialogue.drawSprites sprites

private def drawGores (scene : Scene) (camera : Camera) : IO Unit := do
  let view : OriginalCollision.Vector := ⟨OriginalCollision.single camera.x, OriginalCollision.single camera.y⟩
  for p in scene.state.combat.gorePool.particles do
    if !p.active then continue
    let some (w,h) := OriginalCombat.goreTextureSize p.typeId | continue
    let x := ((p.position.x.toFloat + w.toFloat * 0.5) / 16).toUInt64.toNat
    let y := ((p.position.y.toFloat + h.toFloat * 0.5) / 16).toUInt64.toNat
    let rgba := scene.light.color x y
    let light : OriginalForestChatLayout.Color :=
      ⟨(rgba >>> 24).toNat,((rgba >>> 16) &&& 255).toNat,((rgba >>> 8) &&& 255).toNat,(rgba &&& 255).toNat⟩
    let sprite ← IO.ofExcept (OriginalCombat.goreSprite p view light)
    if let some sprite := sprite then OriginalDesktopDialogue.drawSprites #[sprite]

private def draw (scene : Scene) (patch : Option FramePatch) (camera : Camera)
    (exportStatus : Option String) (mouseX mouseY : Nat) (recipes : Array Nat)
    (waterfalls : OriginalWaterfallDraw.Output) (time : Float)
    (sky : OriginalForestNightSky.Frame) (residentPanel : Option ResidentPanel)
    (localize : ResidentText) (itemStackFont : Option OriginalForestChatLayout.Font)
    (guideCatalog : Option OriginalForestGuideCraftGridLayout.Catalog)
    (rawMouse : Int × Int) : IO Scene := do
  beginDrawing (if scene.state.dayTime then 0x77a9d8ff else skyTint sky.lighting.color)
  drawBackdrop camera scene.sky sky scene.state.dayTime
  scene.liquidRenderer.composite camera.viewport false
  let left := camera.x.toNat / 16 - 2
  let right := (camera.x.toNat + screenWidth) / 16 + 3
  let top := camera.y.toNat / 16 - 2
  let bottom := (camera.y.toNat + screenHeight) / 16 + 3
  drawWalls scene camera left right top bottom
  drawTiles scene patch camera false left right top bottom
  OriginalDesktopWaterfall.draw camera.viewport waterfalls time
  scene.liquidRenderer.drawBelowSolids camera.viewport scene.state.terrain
    (fun x y => scene.light.color x y)
  drawTiles scene patch camera true left right top bottom
  -- BEGIN B-ZONE actions-hit-cracks (S02): the tile entries of the shared `Player.hitTile`
  for hit in (OriginalSession.hitCacheOf scene.state.hits).entries do
    let (x, y) := camera.tile hit.cell.x hit.cell.y
    if x ≥ -16 && x < screenWidth && y ≥ -16 && y < screenHeight && hit.damage > 0 then
      discard <| originalSprite "TileCracks" x y 16 16 (min 3 (hit.damage / 25) * 18) 18 16 16
  for hit in scene.state.swapHits.entries do
    let (x, y) := camera.tile hit.cell.x hit.cell.y
    if x ≥ -16 && x < screenWidth && y ≥ -16 && y < screenHeight && hit.damage > 0 then
      discard <| originalSprite "TileCracks" x y 16 16 (min 3 (hit.damage / 25) * 18) 18 16 16
  -- END B-ZONE actions-hit-cracks
  for item in scene.state.worldItems do
    let some drop := item | continue
    let x := drop.position.x.toInt32.toInt - camera.x
    let y := drop.position.y.toInt32.toInt - camera.y
    if x ≥ -16 && x < screenWidth && y ≥ -16 && y < screenHeight then
      drawItemSprite drop.stack x y 16
  drawSlimes scene camera
  drawGhosts scene camera
  drawResidents scene camera
  drawAngryBones scene camera
  drawDarkCasters scene camera
  drawDungeonSlimes scene camera
  drawCursedSkulls scene camera
  drawOldShakingChests scene camera
  drawNightEnemies scene camera
  OriginalDesktopDialogue.drawSprites scene.spikeBallDraws
  drawSkeletron scene camera
  drawBunnies scene camera
  drawBirds scene camera
  drawSnails scene camera
  -- BEGIN B-ZONE playerdraw-call (S05)
  let playerBoundary ← drawPlayer scene camera
  if playerBoundary != scene.playerDrawBoundary then
    if let some why := playerBoundary then IO.eprintln s!"player draw boundary: {why}"
  let scene := { scene with playerDrawBoundary := playerBoundary }
  -- END B-ZONE playerdraw-call
  drawStab scene camera
  drawGuideArrows scene camera
  drawGravestones scene camera
  scene.liquidRenderer.composite camera.viewport true
  drawGores scene camera
  let scene ← drawGuideDust scene camera
  drawRainSprites scene
  drawResidentEmotes scene camera
  if let some fonts := scene.state.combat.textFonts then
    let sprites ← IO.ofExcept ((OriginalCombatText.draw scene.state.combat.texts fonts {
      camera := ⟨OriginalCollision.single camera.x,OriginalCollision.single camera.y⟩
      screenHeight, gravity := scene.state.combat.textContext.gravity }).mapError IO.userError)
    OriginalDesktopDialogue.drawSprites sprites
  -- The world emote UI layer consumes the previous bubble before the later
  -- mouse-over layer can select the next one. MouseOversClear does not clear it.
  let scene := { scene with currentNPCShowingChatBubble := -1 }
  drawVitals scene camera
  if let some output := scene.buffOutput then
    OriginalDesktopDialogue.drawSprites (output.sprites.extract 0 output.tooltipStart)
  drawInventory scene mouseX mouseY
  drawStorage scene mouseX mouseY
  drawCrafting scene recipes mouseX mouseY
  drawGuideMaterial scene localize
  if scene.inventoryOpen && scene.guideCraft.isSome then
    if let some output := scene.guideGridOutput then
      OriginalDesktopGuideGrid.draw output
    else if let some catalog := guideCatalog then
      let sprites ← IO.ofExcept ((OriginalForestGuideCraftGridLayout.gridToggleSprites catalog
        { screenWidth, screenHeight, mouseX, mouseY } false).mapError IO.userError)
      OriginalDesktopDialogue.drawSprites sprites
  if let some panel := residentPanel then
    OriginalDesktopDialogue.draw panel.layout 1
  else if let some panel ← scene.signPanel then
    panel.draw mouseX mouseY scene.tick
  else if let some origin := scene.signUnderMouse mouseX mouseY then
    let (x, y) := camera.tile origin.x origin.y
    let body := scene.state.player.body
    let mirror := OriginalCollision.single (origin.x * 16 + 16 : Nat) >
      body.position.x + OriginalCollision.single body.width
    let x := x + 16 + if mirror then -40 else 8
    discard <| originalSprite "Chat2" x (y - 22) 32 28 (mirror := mirror)
  drawDeath scene.state.health
  drawBossBar scene itemStackFont
  if let some output := scene.buffOutput then
    OriginalDesktopDialogue.drawSprites (output.sprites.extract output.tooltipStart output.sprites.size)
  if scene.inventorySortAt mouseX mouseY then
    text "Sort Inventory" (mouseX + 14 : Nat) (mouseY + 14 : Nat) 15 0xffffffff
  if let some status := exportStatus then
    let width ← textWidth status 15
    rect 16 (screenHeight - 37 : Nat) (width + 20) 27 0x141827db
    text status 26 (screenHeight - 31 : Nat) 15 0xffffffff
  if let some (notice, untilTick) := scene.bossNotice then
    if scene.tick < untilTick then
      text notice 18 (screenHeight - 68 : Nat) 18 0xaf4bffff
  if let some keyboard := scene.rename.keyboard then
    let panel ← OriginalDesktopRename.panel screenWidth screenHeight keyboard (renamePoint mouseX mouseY)
    panel.draw keyboard.drawnFrames
  -- Main's cursor UI layer precedes Mouse Item and Interact Item Icon. Do not redraw
  -- the pointer from EndDrawing after these later layers.
  cursorLayerHandled
  OriginalDesktopDialogue.drawSprites (OriginalMouseCursor.draw {} {
    mouseX := rawMouse.1, mouseY := rawMouse.2
    cursorScale := scene.cursorClock.scale, cursorColor := scene.cursorColor
    borderColor := scene.mouseBorderColor })
  if let some item := scene.state.mouseItem.filter (·.count > 0) then
    if let some sprites := scene.slotSprites[(3, 0)]? then
      OriginalDesktopDialogue.drawSprites sprites
    else drawStack item mouseX mouseY 48
  let mut scene := scene
  let mouseInterface := scene.rename.keyboard.isSome || scene.signEdit.isSome ||
      scene.inventoryUnderMouse mouseX mouseY || residentPanel.any (·.layout.mouseInterface) ||
      scene.buffOutput.any (·.mouseInterface) ||
      (← scene.signPanel).any (fun panel => panel.contains mouseX mouseY)
  -- MouseOver precedes Interact Item Icon. This preview does not open chat or
  -- consume RNG, but its persistent tooltip mutation must survive zero updates.
  let pointer := OriginalDesktopDialogue.Talk.player scene.state mouseInterface false
  let hover := OriginalDesktopDialogue.Talk.hover scene.state
      { mouse := ⟨camera.x + rawMouse.1, camera.y + rawMouse.2, 1, 1⟩, mouseRight := false }
      pointer scene.talk
  if hover.effects.clearCursorItemIcon then
    let icon := scene.state.candleInteraction
    scene := { scene with state := { scene.state with candleInteraction :=
      { icon with cursorItemIconEnabled := false } } }
  if let some catalog := guideCatalog then
    let state := scene.state
    let icon := state.candleInteraction
    let selected := state.inventorySlot scene.selected
    let output ← IO.ofExcept ((OriginalCursorItemIcon.draw catalog scene.cursorIconCache {
      mouseX := rawMouse.1, mouseY := rawMouse.2, cursorScale := scene.cursorClock.scale
      iconEnabled := icon.cursorItemIconEnabled, iconID := icon.cursorItemIconID
      iconText := icon.cursorItemIconText, iconPush := icon.cursorItemIconPush
      hoveringOverNPC := hover.effects.hovering.isSome, mouseInterface
      mouseDisplayItemIsAir := state.mouseItem.all (fun s => s.item == 0 || s.count == 0)
      -- The earlier MouseText layer clears raw mouseItem.type for stack<=0.
      -- Pass the selected item's type separately for the ID0 fallback.
      mouseItemType := (state.mouseItem.filter (·.count > 0) |>.map (fun s => (s.item.toNat : Int))).getD 0
      selectedItemType := (selected.map (fun s => (s.item.toNat : Int))).getD 0
    }).mapError IO.userError)
    OriginalDesktopDialogue.drawSprites output.sprites
    scene := { scene with cursorIconCache := output.cache }
  endDrawing
  return scene

private def saveCopy (path : System.FilePath) (write : IO Unit) : IO String := do
  let name := path.fileName.getD path.toString
  try
    write
    return s!"Saved {name}"
  catch error =>
    IO.eprintln s!"Could not save {path}. {error}"
    return match error with
      | .alreadyExists .. => s!"{name} already exists"
      | _ => s!"Could not save {name}"

private def gainedStack (before after : OriginalItems.Inventory) : Bool :=
  (before.zip after).any fun (old, current) =>
    (current.map (·.count)).getD 0 > (old.map (·.count)).getD 0

/-- An independent signed 32-bit seed for the native GUID-hash stream. -/
private def windHash : IO Int32 := do
  let bytes ← IO.getRandomBytes 4
  let mut bits : UInt32 := 0
  for i in [:4] do
    bits := bits ||| (bytes[i]!.toUInt32 <<< (8 * i).toUInt32)
  return Int32.ofUInt32 bits

private def runLoop (initial : OriginalSession.State) (patch : Option FramePatch)
    (exportWorld : Option (Archive × System.FilePath))
    (player : Option OriginalPlayerFile.Archive) (exportPlayer : Option System.FilePath)
    (residentResources : Option ResidentResources) (achievementsPath : Option System.FilePath := none) :
    IO Unit := do
  let dirtyFrames := (patch.map (·.changedCells initial.terrain)).getD #[] |>.foldl
    (fun dirty (x, y) => markFrameNeighbors dirty x y) {}
  let mut scene : Scene := {
    state := initial
    light := ← DrawLight.read initial.lighting
    dirtyFrames
    appearance := (player.map (·.appearance)).getD default
  }
  scene := scene.updateTerrain initial
  let skyblockLowTiles := OriginalDesktopLiquid.skyblockLowTilesAtLoad initial.terrain
  -- Item.GetDrawHitbox of the held light items: the size of the whole item texture.
  let mut heldItemFrames : Std.HashMap Nat OriginalHeldItemLight.ItemFrame := {}
  for item in [:6200] do
    match OriginalHeldItemLight.coverage item with
    | .untouched => pure ()
    | _ =>
      try
        let (width, height) ← OriginalDesktopGuideGrid.textureSize s!"Images/Item_{item}"
        heldItemFrames := heldItemFrames.insert item ⟨width, height⟩
      catch _ => pure ()
  let entryCamera := cameraFor initial
  scene := { scene with state := scene.state.enterClient (some {
    position := (OriginalCollision.single entryCamera.x, OriginalCollision.single entryCamera.y)
    size := (screenWidth.toFloat32, screenHeight.toFloat32), skyblockLowTiles }) }
  let mut residentResources := residentResources
  if let some resources := residentResources then
    let textEnv := OriginalResidentText.env resources.dictionary (scene.textWorld
      (player.map OriginalPlayerFile.Archive.name |>.getD "Player"))
    let mut built ← OriginalForestGuideCraftGridCatalog.load OriginalRecipeCatalog.catalog
      resources.dictionary textEnv resources.fonts.mouseText resources.fonts.itemStack OriginalDesktopGuideGrid.textureSize
    -- The closed Guide grid uses the two classic-view toggle frames.
    for name in #["Images/UI/Craft_Toggle_2", "Images/UI/Craft_Toggle_3"] do
      let size ← OriginalDesktopGuideGrid.textureSize name
      built := { built with
        textures := built.textures.insert name size
        catalog := { built.catalog with textures := built.catalog.textures.insert name size } }
    built ← IO.ofExcept ((built.withEnv resources.dictionary textEnv (scene.guideCatalogEnv built)).mapError IO.userError)
    residentResources := some { resources with guideGrid := some built }
  let mut accumulator := 0.0
  -- Player.Spawn enters the world after that tick's DoUpdate: a world draw comes first.
  let mut drawn := false
  -- FNA Game's fixed-step TargetElapsedTime is 166667 TimeSpan ticks.
  -- This global time keeps advancing during the focus return; world time does not.
  let mut gameTimeTicks : Nat := 0
  let mut sortHovered := false
  let mut pendingCursorThrow := false
  let playerName := (player.map OriginalPlayerFile.Archive.name).getD "Player"
  let mut exportStatus :=
    match exportWorld.isSome, exportPlayer.isSome with
    | true, true => some "F5  Save World and Character Copies"
    | true, false => some "F5  Save World Copy"
    | false, true => some "F5  Save Character Copy"
    | false, false => none
  -- Worn items, set bonuses and effects this build does not act on are named, never dropped silently.
  let equipmentBoundaries := scene.state.equipmentBoundaries ++ OriginalSession.luckBoundaries
  for boundary in equipmentBoundaries do
    IO.eprintln s!"Equipment not applied: {boundary}"
  if !equipmentBoundaries.isEmpty then
    let note := s!"{equipmentBoundaries.size} equipment effects not applied (see the log)"
    exportStatus := some ((exportStatus.map (· ++ "  ·  ")).getD "" ++ note)
  repeat
    if ← shouldClose then break
    let keys ← originalInput
    let heldHotbarKeys ← hotbarHeld
    let pointer ← pointerPosition
    let rawMouseX := (Int32.ofInt (Int.ofNat (pointer >>> 32).toNat)).toInt
    let rawMouseY := (Int32.ofInt (Int.ofNat (pointer &&& 0xffffffff).toNat)).toInt
    let mouseX := rawMouseX.toNat
    let mouseY := rawMouseY.toNat
    let oldStorage := scene.state.openStorage
    let oldInventory := scene.state.carriedItems
    let oldCursor := scene.state.mouseItem
    let oldSign := scene.openSign
    let nearbyCrafting := scene.nearbyCrafting
    let wasRenaming := scene.rename.keyboard.isSome
    let wasEditingSign := scene.signEdit.isSome
    let wasSearching := scene.guideSearchActive
    let drawSeconds ← frameTime
    let focused ← OriginalDesktopLiquid.windowFocused
    let mouseHeld := pressed keys rightMouseHeld || pressed keys 57
    scene := { scene with splitClock := scene.splitClock.beginDraw }
    if !mouseHeld then scene := { scene with activeCraft := none, focusedOnMouseDown := false }
    let signPanel ← scene.signPanel
    let pointerInSign := signPanel.any (fun panel => panel.contains mouseX mouseY)
    let pointerInTalk := residentResources.any fun resources =>
      (scene.residentPanel resources.fonts (scene.localize resources playerName) keys mouseX mouseY).any
        (·.layout.mouseInterface)
    let pointerInBuff := scene.buffOutput.any (·.mouseInterface)
    let (interacted, unlocked) ← if scene.state.health.dead then pure (scene, false)
      else if wasRenaming then scene.controlRenamePointer keys mouseX mouseY
      else if pressed keys 33 && !wasSearching && scene.talk.talkNPC ≥ 0 then
        pure (← ({ scene with inventoryOpen := false } : Scene).closeTalk, false)
      else if pointerInTalk then pure (scene, false)
      else do
        match ← scene.controlSign signPanel keys mouseX mouseY with
        | some next => pure (next, false)
        | none =>
          if wasEditingSign || pointerInSign then pure (scene, false)
          else
            let inventoryKeys := if wasSearching then keys &&& (0xf300000000000018 : UInt64) else keys
            let (controlled, unlocked) := (scene.controlInventory inventoryKeys mouseX mouseY).interactStorage inventoryKeys mouseX mouseY
            pure (← controlled.openRenameAt keys mouseX mouseY, unlocked)
    scene := interacted
    if scene.guideCraft.isSome && (!scene.inventoryOpen || scene.state.openStorage.isSome) then
      scene := scene.finishGuideCraft
    let renameActive := wasRenaming || scene.rename.keyboard.isSome
    let editingSign := wasEditingSign || scene.signEdit.isSome
    let playKeys := if renameActive || editingSign || wasSearching then 0 else keys
    pendingCursorThrow := !renameActive && !editingSign && !wasSearching && !pointerInSign && !pointerInTalk && !pointerInBuff && (pendingCursorThrow ||
      (pressed keys 4 && scene.inventoryOpen && scene.state.mouseItem.isSome &&
        !scene.inventoryUnderMouse mouseX mouseY))
    let inventorySortHovered := scene.inventorySortAt mouseX mouseY
    let hovered := inventorySortHovered ||
      (scene.state.openStorage.isSome && storageActionAt mouseX mouseY == some .sortItems)
    if !renameActive && ((hovered && !sortHovered) || (inventorySortHovered && pressed keys 3)) then sound menuTickSound
    sortHovered := hovered
    if unlocked then sound unlockSound
    if pressed playKeys 3 && storageActionAt mouseX mouseY == some .restock &&
        oldStorage.isSome && gainedStack oldInventory scene.state.carriedItems then sound pickupSound
    if pressed playKeys rightMouseHeld && !pressed playKeys 3 &&
        (scene.state.mouseItem.map (·.count)).getD 0 > (oldCursor.map (·.count)).getD 0 then
      sound menuTickSound
    accumulator := if drawn then min 0.1 (accumulator + drawSeconds) else 0
    -- PlayerInput.ScrollWheelDelta is the wheel movement since the previous update.
    let mut wheel : Int := if pressed playKeys 60 then -1 else if pressed playKeys 61 then 1 else 0
    if accumulator ≥ tickSeconds && !scene.state.health.dead then
      let date ← localDeathDate
      let state := { scene.state with contactDeathText := { scene.state.contactDeathText with date } }
      scene := { scene with state }
    while accumulator ≥ tickSeconds do
      gameTimeTicks := gameTimeTicks + 166667
      residentResources := residentResources.map fun resources =>
        { resources with guideGrid := resources.guideGrid.map fun built =>
          built.withIcons (·.updateTime gameTimeTicks) }
      accumulator := accumulator - tickSeconds
      -- FocusHelper.UpdateFocus returns from Main.DoUpdate before animations
      -- and the world update in a single-player session.
      if !focused then continue
      let cursorClock := scene.cursorClock.update
      scene := { scene with
        cursorClock
        cursorColor := OriginalMouseCursor.colorFromClock cursorClock scene.mouseColor }
      let camera := cameraFor scene.state
      let mouseInterface := renameActive || editingSign || pointerInSign || pointerInTalk || pointerInBuff ||
        scene.inventoryUnderMouse mouseX mouseY
      let target := if mouseInterface then none else camera.target mouseX mouseY
      let mouse := (OriginalCollision.single (camera.x + rawMouseX), OriginalCollision.single (camera.y + rawMouseY))
      -- Number keys reach Select unless a text field owns the keyboard (IL_1317..132b).
      let selects := scene.selectEvents
        (if renameActive || editingSign || wasSearching then 0 else heldHotbarKeys) wheel
      wheel := 0
      -- The item this update may start using: the current slot, a pending change or the cursor.
      let candidates := #[scene.selected, 58] ++ scene.state.selection.buffered.toArray ++ selects
      let bucketHeld := candidates.any fun slot =>
        (scene.state.inventorySlot slot).any (OriginalLiquidBuckets.isBucket ·.item)
      let splashReady ← if bucketHeld && pressed playKeys 57 && target.isSome then bucketSplashReady else pure false
      let actionKeys := playKeys
      let audioReady ← audioSupported
      let clockNow ← Std.Time.PlainDateTime.now
      let client : OriginalSession.ClientInput := {
        selects, mouseLeft := pressed playKeys 57, mouseInterface
        mouseRight := pressed playKeys rightMouseHeld
        talkNPC := scene.talk.talkNPC, npcChatRelease := scene.talk.npcChatRelease
        currentNPCShowingChatBubble := scene.currentNPCShowingChatBubble
        tileHitContext := some {
          audioSupported := audioReady
          cameraCenter := ⟨OriginalCollision.single camera.x + screenWidth.toFloat32 * 0.5,
            OriginalCollision.single camera.y + screenHeight.toFloat32 * 0.5⟩ }
        combatTextFonts := residentResources.map (·.fonts.combatText)
        -- `DateTime.Now` as .NET ticks of local time, as the loader seeds from it
        clockTicks := some (clockNow.toWallTime.toNanoseconds.val / 100 + 621355968000000000)
        heldItemFrame := fun item => heldItemFrames.getD item ⟨0, 0⟩
        camera := some {
          position := (OriginalCollision.single camera.x, OriginalCollision.single camera.y)
          size := (screenWidth.toFloat32, screenHeight.toFloat32), skyblockLowTiles }
        -- BEGIN B-ZONE actions-desktop-keys (S02)
        -- QuickHeal / QuickMana (default keys H / B; the bridge reports their presses as bits 22 / 23).
        -- The resolved potion sounds (`quickSounds`, Item2/Item3) are submitted after the update.
        controlQuickHeal := pressed playKeys 22, controlQuickMana := pressed playKeys 23
        -- END B-ZONE actions-desktop-keys
        }
      let windGuidHash ← if scene.state.forest.any (·.needsWindHash) then
          some <$> windHash else pure none
      scene := scene.step actionKeys target splashReady mouse {
        controlThrow := pressed playKeys 21
        playerInventory := scene.inventoryOpen
        mouseRight := pressed playKeys rightMouseHeld || pendingCursorThrow
        mouseRightRelease := pendingCursorThrow
        mouseInterface } windGuidHash client
      if scene.state.selectionMenuTick then sound menuTickSound
      if scene.state.manaRefillSound then playManaRefillSound
      -- BEGIN B-ZONE actions-quick-sounds (S02): QuickHeal / QuickMana use sounds (Item_2 / Item_3)
      for played in scene.state.quickSounds do
        playTileHitSound played.request.typeId.toUInt32 played.request.style.toUInt32
          played.variation.toUInt32 played.volume played.pan played.pitch
      -- END B-ZONE actions-quick-sounds
      for played in scene.state.pickupSounds ++ (scene.state.tileHitEffects.map (·.resolvedSounds)).getD #[] do
        playTileHitSound played.request.typeId.toUInt32 played.request.style.toUInt32
          played.variation.toUInt32 played.volume played.pan played.pitch
      scene ← scene.checkTalkRange
      if scene.state.combat.oldManRetired || scene.state.combat.skeletronRoared then sound skeletronRoarSound
      -- The fixed hit / killed sounds of Angry Bones, Dungeon Slime, Cursed Skull and the Water
      -- Sphere are gone: every strike by the player now carries its resolved HitSound / DeathSound.
      for effect in scene.state.combat.mechanicSounds do
        sound (match effect with | .hit => mechanicHitSound | .killed => mechanicKilledSound)
      -- HitSound / DeathSound of the player's own strikes, resolved at each strike.
      for played in scene.state.combat.strikeSounds do
        playTileHitSound played.typeId.toUInt32 played.style.toUInt32 played.variation.toUInt32 played.volume played.pan played.pitch
      -- NPCHit1 / NPCDeath1 of a friendly NPC struck in the NPC phase, already resolved there.
      for played in scene.state.combat.npcSounds do
        playTileHitSound played.typeId.toUInt32 played.style.toUInt32 played.variation.toUInt32 played.volume played.pan played.pitch
      for effect in scene.state.combat.doorSounds do
        if effect.typeId == 0 then sound 42
        else if effect.typeId == 8 then sound 52
      if !scene.state.combat.darkCasterSounds.isEmpty then sound casterMagicSound
      -- The Water Sphere's own break sound is not a strike sound and stays.
      if scene.state.combat.waterSphereHit && scene.state.combat.waterSphereKilled then
        sound waterSphereBreakSound
      pendingCursorThrow := false
      scene := { scene with
        liquidRenderer := scene.liquidRenderer.update focused
        waterfallRenderer := scene.waterfallRenderer.update }
      -- Each accepted Hurt already resolved its PlayHurtSound: asset, variation, final volume,
      -- pan and pitch, and whether the previous instance of that slot is stopped. Nothing is
      -- rolled or scaled again here. KillMe's sound follows the hurt sound of a lethal hit.
      for hurt in scene.state.hurts do
        if let some played := hurt.played then
          playTileHitSound played.sound.request.typeId.toUInt32 played.sound.request.style.toUInt32
            played.sound.variation.toUInt32 played.sound.volume played.sound.pan played.sound.pitch
      -- KillMe: PlayDeathSound resolved at its place in the update (type 5, positioned), then the
      -- chat message's MenuTick. Nothing is rolled or scaled again here.
      if let some played := scene.state.deathSound then
        playTileHitSound played.request.typeId.toUInt32 played.request.style.toUInt32
          played.variation.toUInt32 played.volume played.pan played.pitch
      if scene.state.deathMessageTick then sound menuTickSound
      -- an achievement completed this update: Main.NewText's MenuTick, the profile file kept current
      if !scene.state.achievementsCompleted.isEmpty then
        sound menuTickSound
        if let some path := achievementsPath then
          if let some done := scene.state.achievements then
            IO.FS.writeFile path (String.intercalate "\n" done.toList ++ "\n")
      -- The projectile phase's resolved sounds (a gravestone's placement Dig).
      for played in scene.state.projectileSounds do
        playTileHitSound played.request.typeId.toUInt32 played.request.style.toUInt32
          played.variation.toUInt32 played.volume played.pan played.pitch
      if let some splash := scene.state.bucketSplash then
        if splash.pickupBefore then sound pickupSound
        if let some pitch := splash.pitchDraw then
          let camera := cameraFor scene.state
          let dx := (splash.x - camera.x - (screenWidth / 2 : Nat)).toFloat
          let dy := (splash.y - camera.y - (screenHeight / 2 : Nat)).toFloat
          let volume := max 0 (1 - (dx * dx + dy * dy).sqrt / 2500)
          let pan := max (-1) (min 1 (dx / 960))
          playBucketSplash (pitch.toFloat * 0.01) volume pan
        if splash.pickupAfter then sound pickupSound
      residentResources := residentResources.map fun resources =>
        { resources with guideGrid := resources.guideGrid.map fun built =>
          built.tick.withIcons (·.playerTick) }
    -- DoDraw advances essScale once, before DrawInterface reads item icons.
    residentResources := residentResources.map fun resources =>
      { resources with guideGrid := resources.guideGrid.map (·.drawTick (!focused)) }
    let jumpHeld := pressed playKeys 26
    let health := scene.state.health
    if health.dead && health.deadTime ≥ OriginalPlayerLiquidHealth.respawnSkipDelay &&
        jumpHeld && !scene.respawnJumpHeld then
      scene := { scene with state := { scene.state with health := { health with respawnTimer := 1 } } }
    scene := { scene with respawnJumpHeld := jumpHeld }
    scene ← scene.advanceRename (pressed keys 34) focused
    if let some origin := scene.openSign then
      if !scene.signInReach origin then
        scene ← scene.closeSign
      else if focused then
        if let some edit := scene.signEdit then
          let signEdit ← edit.advance (pressed keys 34)
          scene := { scene with signEdit }
    if !renameActive && scene.inventoryOpen && !scene.state.health.dead then
      if let some resources := residentResources then
        let mut resources := resources
        if let some built := resources.guideGrid then
          let env := scene.guideCatalogEnv built
          if reprStr env != reprStr built.env then
            let textEnv := OriginalResidentText.env resources.dictionary (scene.textWorld playerName)
            let built ← IO.ofExcept ((built.withEnv resources.dictionary textEnv env).mapError IO.userError)
            resources := { resources with guideGrid := some built }
            residentResources := some resources
        scene ← scene.controlGuideGrid resources keys mouseX mouseY
      if !scene.guideGridVisible && scene.guideGridOutput.isNone then
        scene ← scene.controlGuideSlot keys mouseX mouseY
        if !pointerInTalk then scene := scene.controlCrafting keys mouseX mouseY
    if oldStorage != scene.state.openStorage then
      sound (if scene.state.openStorage.isNone then chestCloseSound
        else if oldStorage.isNone then chestOpenSound else menuTickSound)
    if nearbyCrafting != scene.nearbyCrafting then sound menuTickSound
    let inventory := scene.state.inventory
    if let some resources := residentResources then
      scene ← scene.controlBuffBar resources keys mouseX mouseY focused
    if pressed keys 6 then
      let mut results := []
      if let some (archive, path) := exportWorld then
        let signs := if scene.state.signsChanged then some scene.state.signs else none
        results := [← saveCopy path (archive.exportCopy scene.state.terrain path (some scene.state.chests) signs
          scene.state.saveProgress)]
      if let (some character, some path) := (player, exportPlayer) then
        results := results ++ [← saveCopy path
          (character.exportCopy inventory path scene.state.mouseItem scene.state.coinAndAmmo scene.state.banks
            (some scene.state.health) (equipment := scene.state.equipment) (voice := scene.state.voice))]
      if !results.isEmpty then
        exportStatus := some (String.intercalate "  ·  " results)
    let recipes := if scene.inventoryOpen then scene.recipes else #[]
    if scene.inventoryOpen then scene := scene.refocusRecipe recipes
    if let some resources := residentResources then
      let (next, resources) ← scene.prepareInventoryIcons resources mouseX mouseY
      scene := next
      residentResources := some resources
    scene := { scene with splitClock := scene.splitClock.endDraw mouseHeld }
    let camera := cameraFor scene.state
    let terrain := scene.state.terrain
    -- GUIHotbarDrawInner leaves `changeItem` for the next update.
    if !renameActive && !editingSign then scene := scene.hotbarClick keys mouseX mouseY
    -- BlackFadeCameraTeleport: instant background transition for the next ten draws.
    let waterScene := if scene.blackFade then
        OriginalSceneWaterStyle.State.teleported scene.waterScene.bg.bgStyle
      else scene.waterScene
    let forceFindWaterfalls := waterScene.bg.instantBGTransitionCounter == 10
    -- Main.DoDraw's Lighting.LightTiles calls advance the engine; only its state 1 rescans the
    -- shared scene metrics that DrawBG, the water style and the next UpdateBiomes read.
    let sceneCamera : OriginalSession.SceneCamera := {
      position := (OriginalCollision.single camera.x, OriginalCollision.single camera.y)
      size := (screenWidth.toFloat32, screenHeight.toFloat32), skyblockLowTiles }
    -- BEGIN B-ZONE lighting-draw-globals (S01)
    -- `SetBackColor`'s ColorOfTheSkies of this draw (read before the lighting: `skyFrame` reads only the
    -- clock, the header and the previous draw's atmosphere, none of which the draw's earlier slices change).
    -- BOUNDARY: while raining / eclipse / blood moon `skyFrame` is the preview colour, not the original's.
    let sky := skyFrame scene camera
    let skyColor := sky.lighting.beforeAtmosphere
    let lightingDraw : OriginalSession.LightingDraw := {
      -- DoDraw IL_0735 `ApplyColorOfTheSkiesToTiles` (SetBackColor, before UpdateAtmosphereTransparency)
      tileColor := OriginalLightingRuntime.tileColorOfSkies skyColor.r.toUInt8 skyColor.g.toUInt8 skyColor.b.toUInt8
      -- Main.mouseTextColor / DiscoR,G,B as the update clocks of the waterfall renderer advance them
      mouseTextColor := scene.waterfallRenderer.mouseTextColor.toNat.toUInt8
      discoR := Int32.ofInt scene.waterfallRenderer.disco[0]!
      discoG := Int32.ofInt scene.waterfallRenderer.disco[1]!
      discoB := Int32.ofInt scene.waterfallRenderer.disco[2]!
      -- DoUpdate IL_0066..0087 of the latest update (the same ticks the item icons take)
      globalTimeWrappedHourly := OriginalItemIcon.globalTimeOfTicks gameTimeTicks
      -- Main.liquidAlpha as the previous draw's fade (`fadeLiquidAlpha` in prepareTargets) left it
      liquidAlpha := scene.liquidRenderer.alpha
      -- Main.waterStyle of the previous draw: this draw's water style is computed after its lighting
      waterStyle := Int32.ofNat scene.waterScene.waterStyle
      -- the renderer's framing of tiles whose frames the world file does not store
      frameOf := fun x y tile => some (let (fx, fy) := foregroundFrame scene patch x y tile; ((fx : Int), (fy : Int))) }
    scene := { scene with state := scene.state.drawLighting sceneCamera lightingDraw, blackFade := false }
    if let some fault := scene.state.lightingFault then
      throw (IO.userError s!"lighting stopped the session: {fault}")
    scene := { scene with light := ← DrawLight.read scene.state.lighting }
    -- END B-ZONE lighting-draw-globals
    drawn := true
    let waterScene := OriginalDesktopLiquid.sharedSceneWater waterScene camera.viewport terrain
      scene.state.sceneMetrics skyblockLowTiles
    let time := (scene.tick % 216000).toFloat
    let drawLight := scene.light
    let light : OriginalDesktopLiquid.Light := fun x y => drawLight.color x y
    let liquidRenderer ← scene.liquidRenderer.prepareTargets camera.viewport terrain light waterScene.waterStyle time
    let (waterfallRenderer, waterfalls) := scene.waterfallRenderer.prepare camera.viewport terrain
      scene.state.liquids light liquidRenderer.alpha forceFindWaterfalls focused
    scene := { scene with liquidRenderer, waterScene, waterfallRenderer }
    -- DoDraw's weather slices: cloudAlpha (IL_0516), Main.atmo (IL_074d), Main.DrawRain (IL_13ff) with this
    -- draw's water style and the waterfall ambient sources Main.Ambience reads on the next update
    let ambient : Option OriginalForest.AmbientSources := waterfalls.ambient.map fun a =>
      { waterfallX := a.waterfallX, waterfallY := a.waterfallY, waterfallStrength := a.waterfallStrength
        lavafallX := a.lavafallX, lavafallY := a.lavafallY, lavafallStrength := a.lavafallStrength }
    -- `Lighting.GetColor` as FNA `Color.PackedValue` (`OriginalLightingRuntime.packedValue`)
    let packed := fun (x y : Int) => drawLight.value x y
    let weatherState := ((scene.state.noteDrawnScene waterScene.waterStyle ambient).drawCloudAlpha).drawAtmosphere sceneCamera
    let (weatherState, rainDraws) := weatherState.drawRain sceneCamera packed focused
    scene := { scene with state := weatherState, rainDraws }
    let localize := (residentResources.map fun r => scene.localize r playerName).getD (fun _ => none)
    let residentPanel := residentResources.bind fun resources =>
      scene.residentPanel resources.fonts localize keys mouseX mouseY
    let mix := scene.dungeonMusicMix
    dungeonMusic (mix.dungeon.fade > 0) mix.dungeon.gain
      (mix.skeletron.fade > 0) mix.skeletron.gain
    scene := scene.prepareSpikeBalls camera
    scene ← draw scene patch camera exportStatus mouseX mouseY recipes waterfalls time sky residentPanel localize
      (residentResources.map (·.fonts.itemStack))
      (residentResources.bind fun r => r.guideGrid.map (·.catalog))
      (rawMouseX, rawMouseY)
    let combat := scene.state.combat
    scene := { scene with state := { scene.state with combat := { combat with
      goreLoadedTypes := combat.gorePool.loadedAfterDraw combat.goreLoadedTypes } } }
    if residentResources.isSome then
      let tileInteraction := scene.state.candleInteraction.tileInteractionHappened ||
        unlocked || oldStorage != scene.state.openStorage || oldSign != scene.openSign
      scene ← scene.advanceTalk residentPanel localize keys mouseX mouseY
        (renameActive || editingSign || pointerInSign || (scene.buffOutput.any (·.mouseInterface)) || scene.inventoryUnderMouse mouseX mouseY) tileInteraction
    -- InterfaceLogic4 is unconditional and runs after Hover, even over empty/UI
    -- regions and on draws without a Player update.
    scene := { scene with talk := { scene.talk with npcChatRelease := !pressed keys rightMouseHeld } }
    scene := { scene with skyAtmosphere := sky.lighting.atmosphere }

private structure Options where
  frames : Option System.FilePath := none
  exportWorld : Option System.FilePath := none
  player : Option System.FilePath := none
  exportPlayer : Option System.FilePath := none
  /-- This application's own profile achievements (one name per line), read and kept up to date. Without it
  the profile is a fresh install's for this run. The original's profile is never read. -/
  achievements : Option System.FilePath := none
  greenSlime : Option (Nat × Nat) := none
  /-- Controlled encounter coordinates and initial life; ordinary spawns use SetDefaults. -/
  dungeonSlime : Option (Nat × Nat × Nat) := none
  /-- Controlled AI20 encounter using the ordinary NPC70 spawn/update/draw path. -/
  spikeBall : Option (Nat × Nat) := none
  /-- Controlled bound resident plus overlapping hostile NPC, for effects QA. -/
  mechanicEncounter : Option (Nat × Nat × Nat × Nat) := none

private def run (world : System.FilePath) (options : Options) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile world
  let archive ← IO.ofExcept ((OriginalWorld.decodeArchive bytes).mapError IO.userError)
  let chests ← IO.ofExcept (archive.readChests.mapError IO.userError)
  let signs ← IO.ofExcept (archive.readSigns.mapError IO.userError)
  let terrain := archive.terrain
  let player : Option OriginalPlayerFile.Archive ← options.player.mapM OriginalPlayerFile.read
  let localTime ← Std.Time.PlainDateTime.now
  let ticks := localTime.toWallTime.toNanoseconds.val / 100 + 621355968000000000
  let loaderRandom := UnifiedRandom.seed ticks
  -- BEGIN B-ZONE lighting-seeds-desktop (S01)
  -- `new LightingEngine()`: three fresh Guid hashes (scanner, active map, working map) like the wind
  -- hash, and the worker threads' `Main.rand` seeded from the millisecond clock (`Environment.TickCount`).
  let lightingSeeds : OriginalSession.LightingSeeds := {
    scannerHash := ← windHash, activeHash := ← windHash, workingHash := ← windHash
    scanRandomSeed := (Int32.ofNat ((← IO.monoMsNow) % 4294967296)).toInt }
  -- END B-ZONE lighting-seeds-desktop
  let state ← if options.greenSlime.isSome then
      IO.ofExcept ((OriginalSession.startWithLiquids terrain loaderRandom lightingSeeds).mapError IO.userError)
    else do
      let guidHash ← windHash
      IO.ofExcept ((OriginalSession.startArchive archive loaderRandom lightingSeeds (some guidHash)).mapError IO.userError)
  let now ← OriginalPlayerFile.savedTimeNow
  let patch : Option FramePatch ← options.frames.mapM fun path => FramePatch.read path terrain (allowTerrainEdits := true)
  let initial : OriginalSession.State := { state with
    health := (player.map (·.enterWorld now)).getD state.health
    inventory := (player.map (·.inventory)).getD state.inventory
    coinAndAmmo := (player.map (·.coinAndAmmo)).getD state.coinAndAmmo
    banks := (player.map (·.banks)).getD state.banks
    unlockedBiomeTorches := player.any (·.unlockedBiomeTorches)
    usedGalaxyPearl := player.any (·.usedGalaxyPearl)
    ateArtisanBread := player.any (·.ateArtisanBread)
    wallFrames := (patch.map (·.wallFrames state.terrain)).getD {}
    mouseItem := player.bind (·.mouseItem), chests, signs }
  -- Profile achievements (progression events complete them once, with a chat line and MenuTick).
  let achievements ← match options.achievements with
    | some path =>
      if ← path.pathExists then
        pure (((← IO.FS.readFile path).splitOn "\n").filter (!·.isEmpty) |>.toArray)
      else pure #[]
    | none => pure #[]
  let initial := { initial with achievements := some achievements }
  -- The character's equipment and voice live in the session from here on and go back out on export.
  let initial := match player with
    | some character => initial.withCharacter character.equipment character.voice
    | none => initial
  -- `SoundStyle._random` is a client static created by `new UnifiedRandom()`, seeded with
  -- `Environment.TickCount` (milliseconds since start-up, Int32). The default hurt voice (3)
  -- draws its pitch variance from it. The seed is this process's own monotonic milliseconds.
  let soundTicks ← IO.monoMsNow
  let initial := { initial with hurtSound := { initial.hurtSound with
    pitchRandom := some (UnifiedRandom.seed (Int32.ofNat soundTicks).toInt) } }
  let initial ← match options.greenSlime with
    | none => pure initial
    | some (x, y) =>
      if x == 0 || y == 0 || x + 1 ≥ terrain.header.width || y + 1 ≥ terrain.header.height then
        throw (IO.userError "Green Slime spawn must be inside the world.")
      pure (initial.addGreenSlime (16 * x + 8) (16 * y))
  let initial ← match options.dungeonSlime with
    | none => pure initial
    | some (x, y, life) =>
      if x == 0 || y == 0 || x + 1 ≥ terrain.header.width || y + 1 ≥ terrain.header.height then
        throw (IO.userError "Dungeon Slime spawn must be inside the world.")
      -- Like --green-slime, this is a controlled desktop encounter. Keep the
      -- loaded banner counters, but isolate this actor from natural spawns.
      let slime := { OriginalDungeonSlime.spawn (16 * x + 8) (16 * y) with life := life }
      pure { initial with forest := none, combat := { initial.combat with actors := #[.dungeonSlime slime] } }
  let initial ← match options.spikeBall with
    | none => pure initial
    | some (x,y) =>
      if x == 0 || y == 0 || x + 1 ≥ terrain.header.width || y + 1 ≥ terrain.header.height then
        throw (IO.userError "Spike Ball spawn must be inside the world.")
      pure { initial with forest := none, combat := { initial.combat with
        actors := #[.dungeonSpikeBall (OriginalDungeonSpikeBall.spawn (16*x+8) (16*y))] } }
  let initial ← match options.mechanicEncounter with
    | none => pure initial
    | some (x,y,life,delay) =>
      if x == 0 || y == 0 || x + 1 ≥ terrain.header.width || y + 1 ≥ terrain.header.height then
        throw (IO.userError "Mechanic encounter must be inside the world.")
      let world : OriginalGreenSlime.World := {
        grid := OriginalCollision.Grid.ofTerrain terrain OriginalTileRules.collision
        dayTime := initial.dayTime, worldSurface := terrain.header.surface, rockLayer := terrain.header.rockLayer }
      let bound := OriginalDungeonMechanic.spawn world (16*x+8) (16*y)
      -- Declared QA inputs: an optional delay before the contact,
      -- and keep the viewer in place while observing the resident's effects.
      let bound := { bound with body := { bound.body with
        life := life, immune := bound.body.immune.set! 255 delay } }
      let some bones := OriginalAngryBones.spawn 31 (16*x+14) (16*y)
        | throw (IO.userError "Invalid controlled Angry Bones type.")
      pure { initial with
        forest := none
        health := { initial.health with immune := true,immuneTime := 1000000 }
        combat := { initial.combat with actors := #[.boundMechanic bound,.angryBones bones] } }
  let exportWorld := options.exportWorld.map fun path => (archive, path)
  let title := s!"Lean4Terria — {terrain.header.name}" ++
    ((player.map fun character => s!" — {character.name}").getD "")
  openWindow screenWidth.toUInt32 screenHeight.toUInt32 title
  try
    let templates := (parseDeathTemplates (← originalDeathTemplates)).toOption.getD #[]
    let interfaceFonts ← OriginalDesktopDialogue.Renderer.load
    let residentResources ← try
        some <$> ResidentResources.load (some interfaceFonts)
      catch e =>
        IO.eprintln s!"Resident dialogue unavailable. {e}"
        pure none
    let initial : OriginalSession.State := { initial with
      contactDeathDust := 100
      combat := {initial.combat with textFonts := some interfaceFonts.combatText}
      contactDeathText := {
        playerName := (player.map OriginalPlayerFile.Archive.name).getD "Player", templates
        -- The numbered and single texts of the other death reasons, from the original's localization.
        named := (residentResources.map fun resources =>
          let numbered := #[("Fell_", 9), ("Drowned_", 7), ("Lava_", 5), ("Suffocated_", 2), ("Burned_", 4)].flatMap
            fun (stem, count) => (Array.range count).map fun i => s!"DeathText.{stem}{i + 1}"
          (numbered ++ #["DeathText.Default", "DeathText.Slain", "DeathText.Stabbed", "DeathText.Poisoned"]).filterMap
            fun key => (resources.dictionary.raw? key).map fun text => (key, text)).getD #[] } }
    let names := residentResources.map (fun resources =>
      resources.dictionary.order.filterMap fun key =>
        if key.startsWith "MechanicNames." then resources.dictionary.raw? key else none) |>.getD #[]
    let initial := { initial with combat := { initial.combat with mechanicNames := names } }
    runLoop initial patch exportWorld player options.exportPlayer residentResources options.achievements
  finally closeWindow
  return 0

def usage : String :=
  "Lean4Terria --original-world FILE [--frames JSON] [--green-slime X,Y | --dungeon-slime X,Y[,LIFE] | --spike-ball X,Y | --mechanic-encounter X,Y[,LIFE[,DELAY]]] [--export-world FILE] [--player FILE.plr [--export-player FILE.plr]]"

private def parseOptions : List String → Options → Except String Options
  | [], options =>
    if options.exportPlayer.isSome && options.player.isNone then
      .error "--export-player requires --player."
    else if (#[options.greenSlime.isSome,options.dungeonSlime.isSome,options.spikeBall.isSome,options.mechanicEncounter.isSome].countP id) > 1 then
      .error "Use only one controlled encounter."
    else .ok options
  | "--mechanic-encounter" :: value :: rest, options => do
    if options.mechanicEncounter.isSome then throw "Repeated option --mechanic-encounter."
    let (rawX,rawY,rawLife,rawDelay) ← match value.splitOn "," with
      | [x,y] => pure (x,y,"250","300")
      | [x,y,life] => pure (x,y,life,"300")
      | [x,y,life,delay] => pure (x,y,life,delay)
      | _ => throw "Expected --mechanic-encounter X,Y[,LIFE[,DELAY]] in tile coordinates."
    let some x := rawX.toNat? | throw "Invalid X coordinate for --mechanic-encounter."
    let some y := rawY.toNat? | throw "Invalid Y coordinate for --mechanic-encounter."
    let some life := rawLife.toNat? | throw "Invalid life for --mechanic-encounter."
    let some delay := rawDelay.toNat? | throw "Invalid delay for --mechanic-encounter."
    if life == 0 || life > 250 then throw "Mechanic life must be between 1 and 250."
    parseOptions rest { options with mechanicEncounter := some (x,y,life,delay) }
  | "--spike-ball" :: value :: rest, options => do
    if options.spikeBall.isSome then throw "Repeated option --spike-ball."
    let [rawX,rawY] := value.splitOn "," | throw "Expected --spike-ball X,Y in tile coordinates."
    let some x := rawX.toNat? | throw "Invalid X coordinate for --spike-ball."
    let some y := rawY.toNat? | throw "Invalid Y coordinate for --spike-ball."
    parseOptions rest { options with spikeBall := some (x,y) }
  | "--dungeon-slime" :: value :: rest, options => do
    if options.dungeonSlime.isSome then throw "Repeated option --dungeon-slime."
    let (rawX, rawY, rawLife) ← match value.splitOn "," with
      | [x, y] => pure (x, y, "150")
      | [x, y, life] => pure (x, y, life)
      | _ => throw "Expected --dungeon-slime X,Y[,LIFE] in tile coordinates."
    let some x := rawX.toNat? | throw "Invalid X coordinate for --dungeon-slime."
    let some y := rawY.toNat? | throw "Invalid Y coordinate for --dungeon-slime."
    let some life := rawLife.toNat? | throw "Invalid life for --dungeon-slime."
    if life == 0 || life > 150 then throw "Dungeon Slime life must be between 1 and 150."
    parseOptions rest { options with dungeonSlime := some (x, y, life) }
  | "--green-slime" :: value :: rest, options => do
    if options.greenSlime.isSome then throw "Repeated option --green-slime."
    let [rawX, rawY] := value.splitOn "," | throw "Expected --green-slime X,Y in tile coordinates."
    let some x := rawX.toNat? | throw "Invalid X coordinate for --green-slime."
    let some y := rawY.toNat? | throw "Invalid Y coordinate for --green-slime."
    parseOptions rest { options with greenSlime := some (x, y) }
  | flag :: value :: rest, options => do
    if value.isEmpty || value.startsWith "--" then throw s!"Missing file after {flag}."
    let path := some (System.FilePath.mk value)
    let (previous, next) : Option System.FilePath × Options ← match flag with
      | "--frames" => pure (options.frames, { options with frames := path })
      | "--export-world" => pure (options.exportWorld, { options with exportWorld := path })
      | "--player" => pure (options.player, { options with player := path })
      | "--export-player" => pure (options.exportPlayer, { options with exportPlayer := path })
      | "--achievements" => pure (options.achievements, { options with achievements := path })
      | _ => throw s!"Unknown option {flag}."
    if previous.isSome then throw s!"Repeated option {flag}."
    parseOptions rest next
  | [flag], _ => .error s!"Missing file after {flag}."

def runArgs (args : List String) : IO UInt32 := do
  if let world :: rest := args then
    if !world.isEmpty && !world.startsWith "--" then
      match parseOptions rest {} with
      | .ok options => return ← run (System.FilePath.mk world) options
      | .error message => IO.eprintln message
  IO.eprintln usage
  return 2

end Lean4Terria.OriginalDesktop
