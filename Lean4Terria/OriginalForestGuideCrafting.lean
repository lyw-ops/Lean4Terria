import Lean4Terria.OriginalRecipeCatalog
import Lean4Terria.OriginalCrafting
import Lean4Terria.OriginalStorage
import Lean4Terria.OriginalForestGuideDialogue

/-!
# Guide "Crafting" (Terraria 1.4.5.8 desktop, single player): input, menu switching and the material slot

A hand transcription of the widget-free state behind the Guide's Crafting button, checked against the vanilla
assembly run in a private host (`.lake/original-forest-guide-crafting/1.4.5.8`).

## Path

* **Button.** `NPCChatPanel.DrawButtons` → `NPCInteraction.Interact` of `GuideReverseCrafting` (shown for talk NPC type
  22; its label is `Lang.inter[25]`, "Crafting") → `Main.NPCChatText_GuideReverseCrafting`: open the inventory, clear
  the chat text, sound 12, `InGuideCraftMenu = true`; with `Player.Settings.CraftingGridControl == 0` (the default, and
  the shipped `config.json` value `"CraftingControlMode": 0`) `NewCraftingUI.Open(quiet: true, filter: null)`; then
  `TryChangePipsPage(0)`.
* **Material slot.** `ItemSlot.Handle(ref Main.guideItem, 7, true)` (from `NewCraftingUI.DrawChildren` in the grid
  window, from `Main.DrawInventory` in the Classic list): `OverrideHover` (Shift → `TransferToBackpack` when
  `Player.ItemSpace` has room), `LeftClick` (context 7 accepts only `Item.material` or an empty cursor), `RightClick`
  (one item onto the cursor, stack-split cooldown). Inventory slots (context 0) with Shift swap a whole material stack
  with `Main.guideItem` (`TransferToChest`, cursor override 9).
* **Recipes shown.** `Recipe.UpdateRecipeList` (every drawn frame): with a guide item, `CollectGuideRecipes` (every
  recipe that has an ingredient entry matching it, stations and owned items ignored), then `TryRefocusingRecipe` on
  the recipe that was focused.
* **Selection and wheel.** Classic list: `Main.HoverOverCraftingItemButton(i)` (a click on another recipe focuses it;
  a click on the focused one would craft, which never happens while a guide item is set) and
  `Main.DoScrollingInInventory` → `CraftingUI.ScrollCraftingList` (`focusRecipe += ScrollWheelDelta / 120`, clamped).
  Grid window: `NewCraftingUI.HandleCraftSlot` / `UpdateContents` (selection by recipe index); the wheel scrolls the
  grid view and leaves `focusRecipe` alone.
* **Leaving.** The inventory key while talking (`Player.ToggleInv`: chat closed, inventory closed) → the draw's
  `DrawInterface_26_InterfaceLogic3` clears `InGuideCraftMenu` → the next `Player.Update`'s `dropItemCheck` returns the
  material (`GetOrDropItem`: `GetItem`, the rest spawned as an inventory-overflow world item). Walking out of reach
  (`Player.Update` closes the talk, see `OriginalForestGuideDialogue.updateClose`) → `Main.DrawInventory`'s guide block
  (`talkNPC == -1`, a chest, a shop or the reforge menu) clears it and calls `dropItemCheck` at once.

## What the host could and could not run

The dedicated-server host cannot construct `NewCraftingUI` (its `UIPanel`s ask `Main.Assets` for textures), and cannot
draw. Everything at the `Main` / `Recipe` / `ItemSlot` / `Player` level ran for real in the Classic setting
(`CraftingGridControl = 1`), which is the same code for both settings except where `NewCraftingUI.Visible` is read.
`Grid` (the grid window's list and selection), `gridOpen` / `gridClose`, the grid branches of `toggleInventory` and
`scroll`, and `drawInventoryGuideExit`'s flag write are IL transcriptions only.

Effects that belong to widgets (sounds, hover text, `craftingHide`, gamepad points, chat-panel layout) are returned in
`Effects`; nothing here draws.
-/

namespace Lean4Terria.OriginalForestGuideCrafting

open OriginalItems (Stack Inventory)
abbrev Catalog := OriginalRecipeCatalog.Catalog
abbrev Dialogue := OriginalForestGuideDialogue.State

/-! ## `Item.material` -/

/-- Item types whose `SetDefaults` writes `material = false` after copying `ItemID.Sets.IsAMaterial`, among those a recipe
uses: the four coins (`SetDefaults1` IL_1985 / IL_1a31 / IL_1add / IL_1b84) and the three mechanical boss summons 544,
556, 557 (IL_c174 / IL_c54d / IL_c59f). The two writes in `SetDefaults5` (IL_2a68, IL_2cf4) hit types no recipe uses. -/
def materialOverridesFalse : Array Nat := #[71, 72, 73, 74, 544, 556, 557]

/-- `ItemID.Sets.IsAMaterial` as `Recipe.UpdateWhichItemsAreMaterials` fills it: every plain ingredient entry, and every
item of a recipe group some recipe accepts; then the `SetDefaults` overrides. Indexed by item type. -/
def materialTable (c : Catalog) (itemCount : Nat := 6196) : Array Bool := Id.run do
  let mut t := Array.replicate itemCount false
  for entries in c.lookups do
    for e in entries do
      if e.id == 0 then break
      if c.isGroupId e.id then
        if let some g := c.group? (e.id - c.fakeItemIdOffset) then
          for i in g.items do t := t.set! i true
      else t := t.set! e.id true
  for i in materialOverridesFalse do t := t.set! i false
  return t

/-! ## State -/

/-- `Player.Settings.CraftingGridControl`. -/
inductive GridMode where
  /-- 0, `CraftingGridMode` default: the `NewCraftingUI` grid window. -/
  | grid
  /-- 1: the Classic recipe list of `Main.DrawInventory`. -/
  | classic
  deriving BEq, Repr, Inhabited

/-- One `NewCraftingUI.RecipeEntry`. -/
structure GridEntry where
  /-- `Main.recipe` index. -/
  index : Nat
  /-- Position in `Main.availableRecipe` this frame, -1 when not available (`Available` is `availableIndex >= 0`). -/
  availableIndex : Int := -1
  /-- Position among the filtered entries, -1 when filtered out. -/
  gridIndex : Int := -1
  deriving BEq, Repr, Inhabited

/-- The grid window's own recipe state (`NewCraftingUI._recipes`, `_recipeListLookup`, `_selectedRecipeIndex`,
`_resetForGuideItem`). IL transcription only. -/
structure Grid where
  visible : Bool := false
  entries : Array GridEntry := #[]
  /-- `_selectedRecipeIndex` (a recipe index). -/
  selected : Option Nat := none
  /-- `_resetForGuideItem`: the guide `Item` object the list was built for (`guideToken`), `none` for null. -/
  resetFor : Option Nat := none
  deriving BEq, Repr, Inhabited

structure State where
  /-- `Main.InGuideCraftMenu`. -/
  inGuideCraftMenu : Bool := false
  /-- `Main.playerInventory`. -/
  playerInventory : Bool := false
  /-- `Main.InReforgeMenu`. -/
  inReforgeMenu : Bool := false
  /-- `Main.guideItem` (`none` for air). -/
  guideItem : Option Stack := none
  /-- Identity of the `Main.guideItem` object: bumped whenever the field is given another `Item` (a swap, `GetItem`'s
  result, `new Item()`); an in-place stack change keeps it. Only the grid window reads it. -/
  guideToken : Nat := 0
  /-- `Main.mouseItem`. -/
  mouseItem : Option Stack := none
  /-- `player.inventory[0..57]`. -/
  inventory : Inventory := Array.replicate 58 none
  /-- `Main.focusRecipe`. -/
  focusRecipe : Int := 0
  /-- `Main.availableRecipe` (length `Recipe.maxRecipes` = 3700; zero past `numAvailableRecipes`). -/
  availableTable : Array Nat := Array.replicate 3700 0
  /-- `Main.numAvailableRecipes`. -/
  numAvailable : Nat := 0
  /-- `Main.stackSplit`. -/
  stackSplit : Nat := 0
  /-- `Main.stackDelay` (7 unless the hold-to-repeat accelerated it). -/
  stackDelay : Nat := 7
  /-- `Main.superFastStack`: extra pieces taken by an accelerated right click. -/
  stackExtra : Nat := 0
  /-- `Main._preventCraftingBecauseClickWasUsedToChangeFocusedRecipe`. -/
  preventCraft : Bool := false
  /-- `Main.PipsCurrentPage`, `Main.PipsUseGrid`. -/
  pipsCurrentPage : Nat := 0
  pipsUseGrid : Bool := false
  /-- `player.chest`. -/
  chest : Int := -1
  /-- `Main.npcShop != 0`. -/
  npcShop : Bool := false
  /-- talk NPC, chat text, corner item (`OriginalForestGuideDialogue`). -/
  dialogue : Dialogue := {}
  grid : Grid := {}
  deriving Repr, Inhabited

/-- The recipes shown, `Main.availableRecipe[0 .. numAvailableRecipes)`. -/
def State.available (st : State) : Array Nat := st.availableTable.extract 0 st.numAvailable

/-- Widget and other-system work a step asks for. -/
structure Effects where
  /-- `SoundEngine.PlaySound(type)`. -/
  sounds : Array Nat := #[]
  /-- `player.mouseInterface = true`. -/
  mouseInterface : Bool := false
  /-- An item `GetOrDropItem` could not put back: spawn it with `OriginalItemThrow.newItem` as an inventory-overflow
  item at the player's center. -/
  overflow : Option Stack := none
  /-- The click would craft (`CraftingRequests.CraftItem`), which belongs to `OriginalCrafting`. -/
  craft : Option Nat := none
  deriving Repr, Inhabited

def Effects.sound (e : Effects) (n : Nat) : Effects := { e with sounds := e.sounds.push n }

/-! ## Opening -/

/-- `NewCraftingUI.ResetRecipes`. -/
def Grid.reset (g : Grid) (st : State) : Grid :=
  { g with resetFor := if st.inGuideCraftMenu then some st.guideToken else none, entries := #[] }

/-- `NewCraftingUI.Open(quiet, null)` (grid setting): IL only. -/
def gridOpen (quiet : Bool) (st : State) : State × Effects := Id.run do
  if st.grid.visible then return (st, {})
  let mut st := st
  let mut e : Effects := {}
  -- CloseAll when the inventory is open for a chest or not for the guide (not the case after the Crafting button)
  st := { st with playerInventory := true, pipsCurrentPage := 0, preventCraft := true }
  st := { st with grid := { st.grid.reset st with visible := true } }
  if !quiet then e := e.sound 10
  return (st, e)

/-- `NewCraftingUI.Close(quiet, keepInventory)`: IL only. -/
def gridClose (quiet keepInventory : Bool) (st : State) : State × Effects :=
  if !st.grid.visible then (st, {}) else
  let st := { st with grid := { st.grid with visible := false },
                      playerInventory := if keepInventory then st.playerInventory else false }
  (st, if quiet then {} else ({} : Effects).sound 11)

/-- `Main.TryChangePipsPage(page)`. -/
def tryChangePipsPage (page : Nat) (st : State) : State :=
  if st.pipsCurrentPage == page then st else { st with pipsCurrentPage := page, pipsUseGrid := false }

/-- The Crafting button: `GuideReverseCrafting.Interact` → `Main.NPCChatText_GuideReverseCrafting`. -/
def craftingButton (mode : GridMode) (st : State) : State × Effects := Id.run do
  let mut st := { st with playerInventory := true, dialogue := { st.dialogue with text := .none } }
  let mut e : Effects := ({} : Effects).sound 12
  st := { st with inGuideCraftMenu := true }
  if mode == .grid then
    let (st', e') := gridOpen true st
    st := st'
    e := { e with sounds := e.sounds ++ e'.sounds }
  return (tryChangePipsPage 0 st, e)

/-! ## The material slot and the inventory slots -/

/-- The mouse buttons and modifiers one `ItemSlot.Handle` call sees (`Main.mouseLeft`, `mouseLeftRelease`, …;
Shift is `ItemSlot.ShiftInUse`). Ctrl (quick trash / sell), the favorite key and gamepad input are not transcribed. -/
structure Mouse where
  left : Bool := false
  leftRelease : Bool := true
  right : Bool := false
  rightRelease : Bool := true
  shift : Bool := false
  deriving Repr, Inhabited

/-- `ItemSlot.retainFavoriteOnMoveAt[context]` (true for contexts 0, 1, 2, 5, 32). -/
def retainFavoriteOnMoveAt (context : Nat) : Bool := [0, 1, 2, 5, 32].contains context

/-- `Player.ItemSpace(item).CanTakeItemToPersonalInventory` (no void vault). -/
def itemSpace (inv : Inventory) (s : Stack) : Bool := Id.run do
  let limit := if s.isCoin then 54 else 50
  let accepts (slot : Option Stack) : Bool := match slot with
    | none => true
    | some x => x.acceptsPickup s
  for i in [0:limit] do
    if accepts (inv[i]?.getD none) then return true
  if OriginalItemStats.isAmmoOnPickup s.item then
    for i in [54:58] do
      let slot := inv[i]?.getD none
      if (OriginalItemStats.autoFillAmmoSlot s.item || slot.isSome) && accepts slot then return true
  for i in [54:58] do
    if let some x := inv[i]?.getD none then
      if x.count < OriginalItemStats.maxStack x.item && x.canStack s then return true
  return false

/-- `ItemSlot.Handle(ref Main.guideItem, 7, true)` while the mouse is over the slot. `material` is `Item.material` by
type (`materialTable`). -/
def guideSlot (material : Nat → Bool) (m : Mouse) (st : State) : State × Effects := Id.run do
  let mut st := st
  let mut e : Effects := {}
  -- OverrideHover → GetAlternateClickAction(context 7): Shift, an unfavorited item, room in the backpack
  let transferToBackpack := m.shift && (st.guideItem.any fun g => !g.favorite && itemSpace st.inventory g)
  -- LeftClick
  if m.leftRelease && m.left then
    if transferToBackpack then
      -- OverrideLeftClick, cursor override 7: guideItem = GetItem(guideItem, QuickTransferFromSlot)
      if let some g := st.guideItem then
        let (inv, rest) := OriginalItems.store st.inventory g
        st := { st with inventory := inv, guideItem := rest, guideToken := st.guideToken + 1 }
      return (st, e.sound 12)
    -- PickItemMovementAction(context 7): a material or an empty cursor
    let ok := match st.mouseItem with
      | none => true
      | some c => material c.item.toNat
    if ok then
      let r := OriginalCrafting.clickSlot #[st.guideItem] st.mouseItem 0 (retainFavoriteOnMoveAt 7)
      let g := r.inventory[0]!
      let swapped := g != st.guideItem
      let cursorChanged := r.mouseItem != st.mouseItem
      st := { st with guideItem := g, mouseItem := r.mouseItem,
                      guideToken := st.guideToken + 1 }
      if swapped || cursorChanged then e := e.sound 7
  -- RightClick (context 7): one item onto the cursor while the split cooldown allows
  if m.right && st.stackSplit ≤ 1 && st.guideItem.isSome then
    let clock : OriginalStorage.SplitClock := {
      cooldown := st.stackSplit, delay := st.stackDelay, extra := st.stackExtra }
    if let some (clock, slot, cursor) := clock.take st.guideItem st.mouseItem then
      st := { st with guideItem := slot, mouseItem := cursor,
                      guideToken := if slot.isNone then st.guideToken + 1 else st.guideToken,
                      stackSplit := clock.cooldown }
      e := e.sound 12
  return (st, e)

/-- `ItemSlot.Handle(player.inventory, 0, k, true)` while the guide menu is open, for the left button. Shift on an
unfavorited material swaps the whole stack with `Main.guideItem` (`TransferToChest`, cursor override 9, sound 7);
otherwise the ordinary left click (`OriginalCrafting.clickSlot`). The inventory's right click is not part of this
module. -/
def inventorySlot (material : Nat → Bool) (k : Nat) (m : Mouse) (st : State) : State × Effects := Id.run do
  let item := st.inventory[k]?.getD none
  let swapWithGuide := st.inGuideCraftMenu && m.shift && !st.inReforgeMenu &&
    (item.any fun x => !x.favorite && material x.item.toNat)
  if !(m.leftRelease && m.left) then return (st, {})
  if swapWithGuide then
    let st := { st with inventory := st.inventory.set! k st.guideItem, guideItem := item,
                        guideToken := st.guideToken + 1 }
    return (st, ({} : Effects).sound 7)
  let r := OriginalCrafting.clickSlot st.inventory st.mouseItem k (retainFavoriteOnMoveAt 0)
  let e : Effects := if r.inventory != st.inventory || r.mouseItem != st.mouseItem then ({} : Effects).sound 7 else {}
  return ({ st with inventory := r.inventory, mouseItem := r.mouseItem }, e)

/-! ## The recipe list -/

/-- `Recipe.TryRefocusingRecipe(recipe)`. -/
def tryRefocusingRecipe (recipe : Nat) (st : State) : State := Id.run do
  let mut focus := st.focusRecipe
  for i in [0:st.numAvailable] do
    if st.availableTable[i]! == recipe then
      focus := i
      break
  if focus ≥ st.numAvailable then focus := (st.numAvailable : Int) - 1
  if focus < 0 then focus := 0
  return { st with focusRecipe := focus }

/-- `Recipe.UpdateRecipeList`. With a guide item the list is `CollectGuideRecipes`; without one it is the ordinary
available list, which `OriginalRecipeCatalog.Catalog.available` computes (`withoutGuide`). -/
def updateRecipeList (c : Catalog) (withoutGuide : Unit → Array Nat) (st : State) : State :=
  let prev := if st.focusRecipe ≥ 0 then st.availableTable.getD st.focusRecipe.toNat 0 else 0
  let list := match st.guideItem with
    | some g => c.guideRecipes g.item.toNat
    | none => withoutGuide ()
  let table := Id.run do
    let mut t := Array.replicate st.availableTable.size 0
    for h : i in [0:list.size] do t := t.set! i list[i]
    return t
  tryRefocusingRecipe prev { st with availableTable := table, numAvailable := list.size }

/-- `Main.HoverOverCraftingItemButton(i)` (Classic list), focus part. A click on another recipe focuses it; a click on
the focused one crafts only without a guide item (reported in `craft`, done by `OriginalCrafting`). -/
def hoverRecipeButton (i : Nat) (m : Mouse) (st : State) : State × Effects := Id.run do
  let mut st := st
  let mut e : Effects := { mouseInterface := true }
  let pressed := (m.leftRelease && m.left) || (m.rightRelease && m.right)
  let held := m.left || m.right
  if st.focusRecipe == (i : Int) && st.guideItem.isNone then
    if held && st.stackSplit ≤ 1 && !st.preventCraft then
      e := { e with craft := st.availableTable[i]? }
  else if pressed then
    st := { st with focusRecipe := i, stackSplit := 15, preventCraft := true }
    e := e.sound 12
  if !held then st := { st with preventCraft := false }
  return (st, e)

/-- `Main.DoScrollingInInventory` for the crafting list: `delta` is `PlayerInput.ScrollWheelDelta`. The grid window
(visible) and `PipsUseGrid` keep `focusRecipe`; so does a chest list under the mouse (`overChestList`). -/
def scroll (delta : Int) (overChestList : Bool := false) (st : State) : State := Id.run do
  let d := delta.tdiv 120
  if st.pipsUseGrid || st.grid.visible || (st.chest != -1 && overChestList) then return st
  -- InPipCrafting: PipsCurrentPage == 0, or the guide menu
  if !(st.pipsCurrentPage == 0 || st.inGuideCraftMenu) then return st
  let mut f := st.focusRecipe + d
  if f > (st.numAvailable : Int) - 1 then f := (st.numAvailable : Int) - 1
  if f < 0 then f := 0
  return { st with focusRecipe := f }

/-! ## The grid window's list (IL only) -/

/-- `NewCraftingUI.UpdateContents` after `UpdateRecipeList` (no search text and no station filter: `fits` true). -/
def gridUpdateContents (fits : Nat → Bool := fun _ => true) (st : State) : State := Id.run do
  let mut g := st.grid
  if st.inGuideCraftMenu && g.resetFor != some st.guideToken then g := g.reset st
  g := { g with entries := g.entries.map fun x => { x with availableIndex := -1 } }
  let num := if st.inGuideCraftMenu && st.guideItem.isNone then 0 else st.numAvailable
  let mut filtered : Nat := (g.entries.filter (·.gridIndex ≥ 0)).size
  for i in [0:num] do
    let r := st.availableTable[i]!
    match g.entries.findIdx? (·.index == r) with
    | some j => g := { g with entries := g.entries.modify j fun x => { x with availableIndex := i } }
    | none =>
      let gi : Int := if fits r then filtered else -1
      if fits r then filtered := filtered + 1
      g := { g with entries := g.entries.push { index := r, availableIndex := i, gridIndex := gi } }
  let mut st := { st with grid := g }
  match g.selected.bind fun s => g.entries.find? (·.index == s) with
  | none => st := { st with grid := { g with selected := none } }
  | some x => if x.availableIndex ≥ 0 then st := { st with focusRecipe := x.availableIndex }
  return st

/-- `NewCraftingUI.HandleCraftSlot(entry)` for the mouse: a press on an entry that is not the selected one selects it
(and focuses it when available); a press on the selected, available entry goes to `hoverRecipeButton focusRecipe`. -/
def gridHandleCraftSlot (recipe : Nat) (m : Mouse) (st : State) : State × Effects := Id.run do
  let some x := st.grid.entries.find? (·.index == recipe) | return (st, {})
  let notSelected := st.grid.selected != some recipe
  if x.availableIndex < 0 || notSelected then
    if notSelected && ((m.left && m.leftRelease) || (m.right && m.rightRelease)) then
      let mut st := { st with grid := { st.grid with selected := some recipe }, stackSplit := 15, preventCraft := true }
      if x.availableIndex ≥ 0 then st := { st with focusRecipe := x.availableIndex }
      return (st, ({} : Effects).sound 12)
    return (st, {})
  hoverRecipeButton st.focusRecipe.toNat m st

/-! ## Leaving -/

/-- `Player.ToggleInv` (the inventory key, single player, no map / building mode / options / fancy UI / capture). -/
def toggleInventory (st : State) : State × Effects := Id.run do
  if st.dialogue.talkNPC ≥ 0 then
    let d := { st.dialogue with talkNPC := -1, cornerItem := 0, text := .none }
    return ({ st with dialogue := d, playerInventory := false }, ({} : Effects).sound 11)
  if st.grid.visible then return gridClose false false st
  if !st.playerInventory then return ({ st with playerInventory := true }, ({} : Effects).sound 10)
  return ({ st with playerInventory := false }, ({} : Effects).sound 11)

/-- `Main.DrawInterface_26_InterfaceLogic3` (`dead` closes the inventory first). -/
def interfaceLogic3 (dead : Bool := false) (st : State) : State :=
  let st := if dead then { st with playerInventory := false } else st
  if st.playerInventory then st else
  let st := { st with chest := -1, inGuideCraftMenu := false, inReforgeMenu := false }
  (gridClose true false st).1

/-- The guide part of `Player.dropItemCheck`: with the menu closed, `GetOrDropItem(guideItem, ReturnItemFromSlot)`,
then `guideItem = new Item()`. The cursor item and the throw key are `OriginalItemThrow.dropItemCheck`'s part, which
runs after this with the guide slot empty. -/
def returnGuideItem (st : State) : State × Effects :=
  if st.inGuideCraftMenu then (st, {}) else
  match st.guideItem with
  | none => (st, {})
  | some g =>
    let (inv, rest) := OriginalItems.store st.inventory g
    ({ st with inventory := inv, guideItem := none, guideToken := st.guideToken + 1 }, { overflow := rest })

/-- `Main.DrawInventory`'s guide block (IL_1d7a … IL_1dd5, drawn while the inventory is open and the reforge menu is
not): the menu ends when a chest or a shop is open or the talk is over, then `NewCraftingUI.Close(true, true)` and
`player.dropItemCheck()` right away. -/
def drawInventoryGuideExit (st : State) : State × Effects :=
  if !(st.playerInventory && st.inGuideCraftMenu && !st.inReforgeMenu) then (st, {}) else
  if !(st.chest != -1 || st.npcShop || st.dialogue.talkNPC == -1) then (st, {}) else
  let st := (gridClose true true { st with inGuideCraftMenu := false }).1
  returnGuideItem st

/-- One `Player.Update` as far as this menu goes: the inventory key (`ToggleInv`, when pressed after a release), the
out-of-range talk close (`OriginalForestGuideDialogue.updateClose`, sound 11), and `dropItemCheck`'s guide part. -/
def update (inventoryKey : Bool) (closeTalk : Dialogue → Dialogue × OriginalForestGuideDialogue.Effects)
    (st : State) : State × Effects := Id.run do
  let mut st := st
  let mut e : Effects := {}
  if inventoryKey then
    let (st', e') := toggleInventory st
    st := st'
    e := { e with sounds := e.sounds ++ e'.sounds }
  let (d, de) := closeTalk st.dialogue
  st := { st with dialogue := d }
  if let some snd := de.sound then e := e.sound snd
  let (st', e') := returnGuideItem st
  return (st', { e with overflow := e'.overflow })

/-- The per-drawn-frame `if (Main.stackSplit > 0) Main.stackSplit--` of `Main.DoDraw`. -/
def drawTick (st : State) : State := { st with stackSplit := st.stackSplit - 1 }

end Lean4Terria.OriginalForestGuideCrafting
