import Std.Data.HashMap
import Lean4Terria.OriginalForestChatLayout
import Lean4Terria.OriginalStorageRename
import Lean4Terria.OriginalItemIcon

/-!
# The Guide's crafting grid (Terraria 1.4.5.8 desktop, `Player.Settings.CraftingGridControl = 0`): layout, draws, input

The default crafting window the Guide's Crafting button opens (`Main.NPCChatText_GuideReverseCrafting` →
`NewCraftingUI.Open`), as one frame of the client runs it: `NewCraftingUI.UpdateUI` (the update pass:
`UserInterface.Update` — hover, the left button, the wheel — then `UIElement.Update` of the tree) and
`NewCraftingUI.DrawUI` (`Draw` → `UpdateCraftAreaSize`, `UpdateText`, `UpdateContents`, the element tree, then
`NewCraftingUI.DrawChildren`'s guide slot, texts, selected recipe and grid toggle). Pure: the caller passes the
frame's input and the previous `State`, and gets every `SpriteBatch.Draw` in order (UI space, as FNA's `PushSprite`
stores it) with the state and outputs the frame leaves. Checked draw for draw against the vanilla assembly run in a
private host (`.lake/original-forest-guide-grid/1.4.5.8`).

IL read (desktop `Terraria.exe`): `NewCraftingUI` (`.ctor`, `BuildInfinitesMenuContents`, `Open`, `OnActivate`,
`Draw`, `UpdateCraftAreaSize`, `UpdateText`, `UpdateContents`, `ResetRecipes`, `FitsFilter`, `DrawChildren`,
`DrawRecipeSlot`, `HandleCraftSlot`, `get_SelectedEntry`), `NewCraftingUI.ItemGrid.DrawSlot`,
`UIDynamicItemCollection<T>` (`DrawSelf`, `GetGridParameters`, `GetItemSlotHitbox`, `UpdateSize`, `Recalculate`,
`Update`), `UIElement` (`.ctor`, `Recalculate`, `GetDimensionsBasedOnParentDimensions`, `RecalculateChildren`, `Draw`,
`DrawChildren`, `GetElementAt`, `ContainsPoint`, `GetClippingRectangle`, `MouseOver` / `MouseOut` / `LeftClick` /
`ScrollWheel` bubbling), `UserInterface` (`Update`, `HandleClick`, `Draw`, `SetState`, `ResetState`, `GetDimensions`),
`UIList` (+ `UIInnerList`), `UIScrollbar`, `UIPanel`, `UIText`, `UIWrappedSearchBar` / `UISearchBar` / `UITextBox`,
`UIImageButton`, `UIImageFramed`, `UIHorizontalSeparator`, `UICreativeItemsInfiniteFilteringOptions` and the
`ItemFilters.*.GetImage` frames, `ItemSlot.Draw` (contexts 7, 41, 42, 43), `ItemSlot.DrawItemIcon`,
`DrawItem_GetColorAndScale`, `GetItemLight`, `Item.GetAlpha` / `GetColor`, `CraftingUI.DrawGridToggle`,
`Utils.DrawSelectedCraftingBarIndicator`, `Utils.DrawPanel`, FNA `SpriteBatch.Draw` overloads; for the keyboard and
mouse interactions: `UISearchBar` (`SetContents`, `TrimDisplayIfOverElementDimensions`, `ToggleTakingText`, `Update`,
`DrawSelf`, `OnDeactivate`, `HasContents`), `UIWrappedSearchBar` (`Click_SearchArea`, `searchCancelButton_OnClick`,
`On{Start,End}TakingInput`, `UpdateSearchContents`, `Update`), `UITextBox` (`SetText`, `DrawSelf`'s ticker),
`Main.GetInputText` / `clrInput`, `Utils.TrimUserString` / `TrimLastCharacter`, `EntryFilterer.SetSearchFilter` /
`FitsFilter`, `ItemFilters.BySearch`, `UIScrollbar` (`LeftMouseDown`, `LeftMouseUp`, `DrawSelf`'s drag),
`Main.HoverOverCraftingItemButton`, `NewCraftingUI` (`OnSearchContentsChanged`, `ToggleInInventory`, `Close`,
`OnDeactivate`), `UserInterface.SetState` / `ResetState`.

## What is an input

* the frame: backbuffer size, `Main.UIScale` (as the setter leaves it), the UI-space mouse, `Main.mouseLeft` /
  `mouseLeftRelease`, `PlayerInput.ScrollWheelDeltaForUI`, the guide item and the identity of the `Main.guideItem`
  object, `Main.availableRecipe[0 .. numAvailableRecipes)` after `Recipe.UpdateRecipeList` (what the existing
  crafting logic computes), the frame's text events (`Main.keyInt` / `keyString`: character code and text) and the
  held keys (`Keyboard.GetState()`; Back = 8);
* the catalogue: per recipe its create item and stack, materials, `Main.GetRecipeRequirementsText(recipe, false)`,
  `AvailableFilters[k].FitsFilter(createItem)` for the 11 tabs and the create item's tooltip lines (what
  `ItemFilters.BySearch` searches); per item its texture size, `Item.color`, `alpha`,
  name, `DrawAnimationVertical` state; the texture sizes of the UI art; the language strings (`LegacyInterface.25`,
  `.24`, `.21`, `UI.PlayerNameSlot`); the `Mouse_Text` / `Item_Stack` fonts (the desktop's runtime XNB fonts).

## What is an output

Every `SpriteBatch.Draw` in order (`Output.sprites`, the `OriginalForestChatLayout.Sprite` record the desktop draw
layer already takes), the scissor rectangle of the grid's draws and the sprite range inside it, the click areas of the
frame (`cells`, `areas`), `mouseInterface`, the hovered item, whether the guide slot's `ItemSlot.Handle` ran (the
existing crafting logic performs the swap), whether a recipe was clicked (`recipeClicked`, `preventCrafting`,
`craftingHide`) and whether the grid toggle closed the window (`gridClosed`); and the next `State` (selection,
`focusRecipe`, scroll and scrollbar dragging, filter tab, search box and its text, recipe entries, hover and button
state, open / closed). Keep one `State` for the session: vanilla creates the window once and reuses it on every open
(`State.openWindow … (prev := some st)`).

## Boundaries (`Except String`, never a guess)

Items whose icon is not the generic path (`Item.GetAlpha` special types and boss bags, `GetItemLight` specials,
glow masks, item 5644's frame override, an animation other than `DrawAnimationVertical`), the left button on the
selected recipe with the guide slot empty (it crafts), IME composition, text
outside the BMP, a non-ASCII search (and tooltip lines with U+0131 / U+017F), the search box's right-click and virtual
keyboard, the grid toggle's right-click, gamepad input, a `RecipeFilter` (station filter: not used by the
Guide), Journey mode (the creative toggle shifts the grid toggle), and an item texture that is not loaded (the
vanilla grid stops drawing at the first unloaded texture and loads it; here every texture counts as loaded).
-/

namespace Lean4Terria.OriginalForestGuideCraftGridLayout

open OriginalForestChatLayout (Sprite Color Font Rect white black multiplyRGBA)

/-! ## Float32 and colours as the IL computes them -/

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def truncI (f : Float32) : Int := f.toInt32.toInt
private def natF (n : Nat) : Float32 := single n

/-- `MathHelper.Clamp(value, min, max)` (FNA: `value > max ? max : value`, then `< min ? min`). -/
private def clampF (v lo hi : Float32) : Float32 :=
  let v := if v > hi then hi else v
  if v < lo then lo else v

/-- `Color * float` (FNA): `(int)(channel * scale)` per channel, clamped. -/
def scaleColor (c : Color) (s : Float32) : Color :=
  Color.mk' (truncI (natF c.r * s)) (truncI (natF c.g * s)) (truncI (natF c.b * s)) (truncI (natF c.a * s))

/-- A colour from its packed value (`R | G << 8 | B << 16 | A << 24`). -/
def Color.ofPacked (v : UInt32) : Color :=
  ⟨(v &&& 0xFF).toNat, ((v >>> 8) &&& 0xFF).toNat, ((v >>> 16) &&& 0xFF).toNat, ((v >>> 24) &&& 0xFF).toNat⟩

def transparent : Color := ⟨0, 0, 0, 0⟩

/-! ### .NET string helpers the search box uses -/

/-- `Char.IsWhiteSpace` (U+0009 … U+000D, the space separators, U+0085, U+00A0, U+2028 / U+2029) -/
def isWhiteSpace (c : Char) : Bool :=
  let n := c.toNat
  (9 ≤ n && n ≤ 13) || n == 32 || n == 0x85 || n == 0xA0 || n == 0x1680 || (0x2000 ≤ n && n ≤ 0x200A) ||
    n == 0x2028 || n == 0x2029 || n == 0x202F || n == 0x205F || n == 0x3000

/-- `String.IsNullOrWhiteSpace` -/
def isNullOrWhiteSpace : Option String → Bool
  | none => true
  | some s => s.all isWhiteSpace

/-- `line.IndexOf(search, StringComparison.OrdinalIgnoreCase) != -1` for an ASCII `search`: both upper-cased char by
char. The only non-ASCII characters whose invariant upper case is ASCII (U+0131 dotless i, U+017F long s) are a
boundary. -/
def containsOrdinalIgnoreCase (line search : String) : Except String Bool := do
  if line.any (fun c => c.toNat == 0x131 || c.toNat == 0x17F) then
    throw "a tooltip line with U+0131 / U+017F (OrdinalIgnoreCase folds it to ASCII) is not transcribed"
  let up (c : Char) : Char := if 97 ≤ c.toNat && c.toNat ≤ 122 then Char.ofNat (c.toNat - 32) else c
  let l := line.toList.map up
  let k := search.toList.map up
  return (List.range (l.length + 1)).any fun i => (l.drop i).take k.length == k

/-- `(int)Math.Round((double)x)` (half to even), for `x ≥ 0` -/
def roundHalfEven (x : Float) : Int :=
  let f := x.floor
  let i : Int := f.toUInt64.toNat
  let d := x - f
  if d < 0.5 then i else if d > 0.5 then i + 1 else if i % 2 == 0 then i else i + 1

/-! ### Theme colours

`Utils.ShiftBlueToCyanTheme` (`ShiftHue(c, -0.04, 0.04, 0.2)` through `Main.rgbToHsl` / `hslToRgb`) is only ever
applied to constant colours here; these are its results for those inputs, as vanilla computes them (checked by the
replay on every frame), not a transcription of the HSL code. -/

/-- the main panel: `ShiftBlueToCyanTheme(new Color(63, 82, 151) * 0.7f) * 0.8f` -/
def panelBackground : Color := Color.ofPacked 0x8e6c401a
/-- `ShiftBlueToCyanTheme(Color.Black) * 0.8f` -/
def panelBorder : Color := Color.ofPacked 0xcc070609
/-- the search box: `ShiftBlueToCyanTheme(new Color(35, 40, 83))` (background and border) -/
def searchPanelColor : Color := Color.ofPacked 0xff6f381b
/-- `ShiftBlueToCyanTheme(new Color(89, 116, 213, 255) * 0.9f)` -/
def separatorColor : Color := Color.ofPacked 0xe5da8b49
/-- `ItemSlot.Draw` contexts 41 / 42 / 43: `ShiftBlueToCyanTheme(new Color(16, 36, 56, 180) * 0.9f)` -/
def craftSlotColor : Color := Color.ofPacked 0xa24a3a0a
/-- `NewCraftingUI.DisabledSlotColor` -/
def disabledSlotColor : Color := ⟨160, 160, 160, 255⟩
/-- `Main.OurFavoriteColor` -/
def ourFavoriteColor : Color := ⟨255, 231, 69, 255⟩
/-- `Main.inventoryBack` while the grid draws (the value `ItemSlot.Draw` restores to: `(220, 220, 220, 220)`). -/
def inventoryBackDefault : Color := ⟨220, 220, 220, 220⟩

/-! ## FNA `SpriteBatch.Draw` overloads -/

/-- `Draw(texture, Vector2 position, Color)` -/
def drawAt (tex : String) (tw th : Nat) (x y : Float32) (c : Color) : Sprite :=
  { tex, sourceX := 0, sourceY := 0, sourceW := 1, sourceH := 1, destX := x, destY := y, destW := natF tw,
    destH := natF th, originX := 0, originY := 0, rotSin := 0, rotCos := 1, depth := 0, color := c.packed, effects := 0 }

/-- `Draw(texture, Vector2 position, Rectangle? source, Color)` -/
def drawAtSrc (tex : String) (tw th : Nat) (x y : Float32) (src : Rect) (c : Color) : Sprite :=
  { tex, sourceX := single src.x / natF tw, sourceY := single src.y / natF th, sourceW := single src.w / natF tw,
    sourceH := single src.h / natF th, destX := x, destY := y, destW := single src.w, destH := single src.h,
    originX := 0, originY := 0, rotSin := 0, rotCos := 1, depth := 0, color := c.packed, effects := 0 }

/-- `Draw(texture, Rectangle destination, Color)` -/
def drawRect (tex : String) (d : Rect) (c : Color) : Sprite :=
  { tex, sourceX := 0, sourceY := 0, sourceW := 1, sourceH := 1, destX := single d.x, destY := single d.y,
    destW := single d.w, destH := single d.h, originX := 0, originY := 0, rotSin := 0, rotCos := 1, depth := 0,
    color := c.packed, effects := 0 }

/-- `Draw(texture, Rectangle destination, Rectangle? source, Color)` -/
def drawRectSrc (tex : String) (tw th : Nat) (d src : Rect) (c : Color) : Sprite :=
  { tex, sourceX := single src.x / natF tw, sourceY := single src.y / natF th, sourceW := single src.w / natF tw,
    sourceH := single src.h / natF th, destX := single d.x, destY := single d.y, destW := single d.w,
    destH := single d.h, originX := 0, originY := 0, rotSin := 0, rotCos := 1, depth := 0, color := c.packed,
    effects := 0 }

/-- `Draw(texture, position, source, color, rotation 0, origin, float scale, effects 0, depth 0)` (FNA's scaled
overload, the same arithmetic as `OriginalForestChatLayout.draw`). -/
def drawScaled (tex : String) (tw th : Nat) (x y : Float32) (src : Option Rect) (c : Color) (ox oy sx sy : Float32) :
    Sprite :=
  OriginalForestChatLayout.draw tex tw th x y src c 0 ox oy sx sy

/-! ## Inputs -/

/-- What `ItemSlot.DrawItemIcon` needs of an item type. -/
structure ItemVisual where
  type : Nat
  /-- `TextureAssets.Item[type]` size (`Images/Item_<type>`) -/
  texW : Nat
  texH : Nat
  /-- `Item.color` -/
  color : Color := transparent
  /-- `Item.alpha` -/
  alpha : Nat := 0
  /-- `Item.Name` (the guide line "Showing recipes that use …") -/
  name : String := ""
  /-- `Main.itemAnimations[type]` when it is a `DrawAnimationVertical`: `(FrameCount, Frame, PingPong)` -/
  animation : Option (Nat × Nat × Bool) := none
  /-- Why the generic icon path does not apply (`Item.GetAlpha` special type or boss bag, `GetItemLight` special,
  `glowMask`, another kind of `Main.itemAnimations`): drawing it is a boundary. -/
  special : Option String := none
  /-- The asset `TextureAssets.Item[type]` holds: `AssetInitializer.LoadTextures` gives an item with
  `ItemID.Sets.TextureCopyLoad[type] ≠ −1` the texture of that item (e.g. 3665 → `Images/Item_48`); empty means
  `Images/Item_<type>`. `texW` / `texH` are this asset's size. -/
  texture : String := ""
  /-- `TextureAssets.GlowMask[item.glowMask]`: asset and size (`none` for glow mask −1) -/
  glow : Option (String × Nat × Nat) := none
  /-- `ItemID.Sets.ItemIconPulse` (`GetItemLight`: colour and icon scale follow `Main.essScale`) -/
  iconPulse : Bool := false
  /-- `ItemID.Sets.BossBag` (`Item.GetAlpha`) -/
  bossBag : Bool := false
  /-- `ItemID.Sets.TrapSigned` / `DrawUnsafeIndicator`: the wire / unsafe overlays of `DrawItemIcon` -/
  trapSigned : Bool := false
  unsafeIndicator : Bool := false
  /-- `Item.shoot` (the phase-saber glow colour) -/
  shoot : Int := 0
  /-- `Main.itemAnimations[type]` is a `DrawAnimationScryingOrb` (`animation` holds its frame count) -/
  scryingOrb : Bool := false
  deriving Repr, Inhabited

/-- `Item.GetAlpha` special types (`IL_002e` … `IL_0782` dispatch of 1.4.5.8), as ranges. -/
def getAlphaSpecialTypes : List (Nat × Nat) :=
  [(51, 51), (58, 58), (75, 75), (119, 122), (184, 184), (198, 203), (217, 220), (501, 502), (520, 522), (547, 549),
   (575, 575), (671, 671), (757, 757), (787, 787), (1260, 1260), (1306, 1306), (1326, 1326), (1332, 1332),
   (1446, 1446), (1506, 1508), (1543, 1545), (1572, 1572), (1734, 1735), (1826, 1826), (1867, 1868), (2763, 2765),
   (2782, 2786), (3065, 3065), (3191, 3191), (3453, 3459), (3522, 3522), (3580, 3580), (3764, 3769), (3822, 3822),
   (3858, 3858), (4143, 4143), (4258, 4259), (4354, 4354), (4377, 4378), (4389, 4389), (4956, 4956), (4988, 4988),
   (5043, 5043), (5127, 5128), (5335, 5335), (5535, 5536), (5669, 5671), (6155, 6155), (6173, 6173)]

/-- `ItemSlot.GetItemLight` special types (Disco colour, 5128). -/
def itemLightSpecialTypes : List Nat := [662, 663, 5444, 5450, 5643, 5128]

/-- `ItemSlot.DrawItemIcon` overrides the animation frame of type 5644 (outside context 31). -/
def iconFrameSpecialTypes : List Nat := [5644]

/-- One recipe as the grid shows it. -/
structure Recipe where
  /-- `Main.recipe` index -/
  index : Nat
  create : Nat
  createStack : Nat
  /-- `requiredItem[0 ..]` up to the first air: `(type, stack)` -/
  materials : Array (Nat × Nat)
  /-- `Main.GetRecipeRequirementsText(recipe, false)` -/
  requirements : String
  /-- `_filterer.AvailableFilters[k].FitsFilter(createItem)`, k = 0 … 10 (tab order) -/
  fits : Array Bool
  /-- the create item's tooltip lines `ItemFilters.BySearch` searches (`Main.MouseText_DrawItemTooltip_GetLinesInfo`) -/
  searchLines : Array String := #[]
  deriving Repr, Inhabited

/-- Everything the frame reads that does not change while the window is open. -/
structure Catalog where
  recipes : Std.HashMap Nat Recipe
  items : Std.HashMap Nat ItemVisual
  /-- texture sizes of the UI art (`Images/...`), from the XNB headers -/
  textures : Std.HashMap String (Nat × Nat)
  mouseText : Font
  itemStack : Font
  /-- `Lang.inter[25]` (the title), `Lang.inter[24]` (empty guide slot), `Lang.inter[21]` ("Showing recipes that
  use"), `Language.GetText("UI.PlayerNameSlot")` (the search box's empty text) -/
  title : String
  guideEmpty : String
  guideShowing : String
  searchHint : String
  /-- the vanilla globals special item icons read (Disco, gFade, essScale, miscCounter, …; `OriginalItemIcon`) -/
  icons : OriginalItemIcon.IconRuntime := {}

def Catalog.tex (c : Catalog) (name : String) : Except String (Nat × Nat) :=
  match c.textures[name]? with
  | some s => .ok s
  | none => .error s!"texture size of {name} not supplied"

def Catalog.item (c : Catalog) (t : Nat) : Except String ItemVisual :=
  match c.items[t]? with
  | some v => .ok v
  | none => .error s!"item {t} not in the catalogue"

def Catalog.recipe (c : Catalog) (i : Nat) : Except String Recipe :=
  match c.recipes[i]? with
  | some r => .ok r
  | none => .error s!"recipe {i} not in the catalogue"

/-- One frame's input. -/
structure Input where
  /-- `PlayerInput.OriginalScreenSize` (the backbuffer) -/
  screenWidth : Int
  screenHeight : Int
  /-- `Main.UIScale` as its setter leaves it (`_uiScaleUsed`) -/
  uiScale : Float32 := 1
  /-- `Main.mouseX` / `mouseY` after `PlayerInput.SetZoom_UI` (UI space) -/
  mouseX : Int
  mouseY : Int
  mouseLeft : Bool := false
  mouseLeftRelease : Bool := true
  mouseRight : Bool := false
  mouseRightRelease : Bool := true
  /-- `PlayerInput.ScrollWheelDeltaForUI` -/
  wheel : Int := 0
  /-- `GameTime.ElapsedGameTime.TotalMilliseconds` (the client's fixed step: 166667 ticks) -/
  elapsedMs : Float := 16.6667
  /-- `Main.guideItem`: `(type, stack)`, `none` for air -/
  guide : Option (Nat × Nat) := none
  /-- `Main.guideItem` after the guide slot's `ItemSlot.Handle` of this frame (the existing crafting logic swaps it
  with the mouse item between the slot's draw and the guide line's): the line shows this one -/
  guideAfter : Option (Nat × Nat) := none
  /-- the identity of the `Main.guideItem` object (`NewCraftingUI._resetForGuideItem` compares references) -/
  guideObject : Int := 0
  /-- `Main.availableRecipe[0 .. numAvailableRecipes)` after `Recipe.UpdateRecipeList` -/
  available : Array Nat := #[]
  /-- `Main.mouseItem` is not air -/
  holdingItem : Bool := false
  /-- this frame's text events (`Main.keyInt` / `keyString`, `keyCount` of them): `(code, text)`; 13 Enter, 27 Escape -/
  typed : Array (Nat × String) := #[]
  /-- the keys held (`Keyboard.GetState()`, `Microsoft.Xna.Framework.Input.Keys` values); 8 = Back -/
  held : Array Nat := #[]
  /-- The desktop text listener's UTF-16 queue and the clipboard on a paste request. -/
  textQueued : Option (Array UInt16) := none
  clipboard : Array UInt16 := #[]
  deriving Repr, Inhabited

/-! ## The element tree -/

inductive El where
  | state | root | panel | title | search | searchButton | searchPanel | searchBar | textBox | cancel
  | grid | separator | list | innerList | items | scrollbar | filters
  | tab (i : Nat)
  | icon (i : Nat)
  deriving BEq, Hashable, Repr, Inhabited

def filterCount : Nat := 11

def El.children : El → List El
  | .state => [.root]
  | .root => [.panel, .filters]
  | .panel => [.title, .search, .grid]
  | .search => [.searchButton, .searchPanel]
  | .searchPanel => [.searchBar, .cancel]
  | .searchBar => [.textBox]
  | .grid => [.separator, .list, .scrollbar]
  | .list => [.innerList]
  | .innerList => [.items]
  | .filters => (List.range filterCount).map El.tab
  | .tab i => [.icon i]
  | _ => []

def El.parent : El → Option El
  | .state => none
  | .root => some .state
  | .panel | .filters => some .root
  | .title | .search | .grid => some .panel
  | .searchButton | .searchPanel => some .search
  | .searchBar | .cancel => some .searchPanel
  | .textBox => some .searchBar
  | .separator | .list | .scrollbar => some .grid
  | .innerList => some .list
  | .items => some .innerList
  | .tab _ => some .filters
  | .icon i => some (.tab i)

/-- `UIElement.IgnoresMouseInteraction` -/
def El.ignoresMouse : El → Bool
  | .searchBar | .icon _ => true
  | _ => false

/-- `StyleDimension`: `Pixels + Precent * parent`. -/
structure Dim where
  px : Float32 := 0
  pc : Float32 := 0
  deriving Repr, Inhabited

def Dim.get (d : Dim) (parent : Float32) : Float32 := d.px + d.pc * parent

def fill : Dim := ⟨0, 1⟩

structure Style where
  left : Dim := {}
  top : Dim := {}
  width : Dim := {}
  height : Dim := {}
  minW : Dim := {}
  maxW : Dim := fill
  minH : Dim := {}
  maxH : Dim := fill
  hAlign : Float32 := 0
  vAlign : Float32 := 0
  padL : Float32 := 0
  padR : Float32 := 0
  padT : Float32 := 0
  padB : Float32 := 0
  deriving Repr, Inhabited

/-- `CalculatedStyle` -/
structure Calc where
  x : Float32 := 0
  y : Float32 := 0
  w : Float32 := 0
  h : Float32 := 0
  deriving Repr, Inhabited, BEq

/-- `CalculatedStyle.ToRectangle()` -/
def Calc.rect (c : Calc) : Rect := ⟨truncI c.x, truncI c.y, truncI c.w, truncI c.h⟩

structure Dims where
  /-- `_dimensions` (margins are 0 everywhere here, so outer = dimensions) -/
  dims : Calc := {}
  inner : Calc := {}
  deriving Repr, Inhabited

/-- `UIElement.GetDimensionsBasedOnParentDimensions` + the padding step of `Recalculate`. -/
def layout (s : Style) (p : Calc) : Dims :=
  let x := s.left.get p.w + p.x
  let y := s.top.get p.h + p.y
  let w := clampF (s.width.get p.w) (s.minW.get p.w) (s.maxW.get p.w)
  let h := clampF (s.height.get p.h) (s.minH.get p.h) (s.maxH.get p.h)
  let x := x + (p.w * s.hAlign - w * s.hAlign)
  let y := y + (p.h * s.vAlign - h * s.vAlign)
  let d : Calc := ⟨x, y, w, h⟩
  { dims := d, inner := ⟨x + s.padL, y + s.padT, w - (s.padL + s.padR), h - (s.padT + s.padB)⟩ }

/-! ## State -/

/-- One `NewCraftingUI.RecipeEntry`. -/
structure Entry where
  /-- the `RecipeEntry` object (the item grid keeps drawing entries `ResetRecipes` dropped) -/
  uid : Nat := 0
  index : Nat
  availableIndex : Int := -1
  gridIndex : Nat := 0
  deriving Repr, Inhabited, BEq

/-- `UserInterface.InputPointerCache` (left button). -/
structure Pointer where
  wasDown : Bool := false
  lastDown : Option El := none
  lastClicked : Option El := none
  lastTimeDown : Float := 0
  deriving Repr, Inhabited

structure State where
  dims : Std.HashMap El Dims := {}
  /-- `_gridContainer.Height.Pixels` (0 until the first `UpdateCraftAreaSize`) -/
  gridHeightPx : Float32 := 0
  /-- `_text` (`UIText`) contents and its layout size -/
  titleText : String := ""
  titleW : Float32 := 0
  titleH : Float32 := 0
  /-- `_searchBar.Width.Pixels` -/
  searchWidthPx : Float32 := 0
  /-- the search box `UITextBox` MinWidth / MinHeight (`UITextPanel.SetText` of the empty text) -/
  textBoxMinW : Float32 := 0
  textBoxMinH : Float32 := 0
  /-- the item grid: `MinHeight.Pixels`, `_itemsPerLine`, `_contents` (entry `uid`s) -/
  itemsMinH : Float32 := 0
  perLine : Nat := 0
  contents : Array Nat := #[]
  /-- entries `ResetRecipes` dropped (still referenced by the item grid until its next `SetContentsToShow`) -/
  detached : Std.HashMap Nat Entry := {}
  nextUid : Nat := 1
  /-- the window is open (`NewCraftingUI.Visible`) -/
  visible : Bool := true
  /-- the search box: `UISearchBar.IsWritingText`, `actualContents` (`none` = null), the text shown (`_text.Text`:
  the hint when empty), `UITextBox._frameCount` (the input ticker) -/
  writing : Bool := false
  actualContents : Option String := none
  shownText : String := ""
  /-- `_text.TextColor`: White for typed text, Gray for the hint -/
  shownWhite : Bool := false
  /-- the text last passed to `OnContentsChanged` (`UIWrappedSearchBar._searchString`, `SetSearchFilter`'s argument):
  a search filter applies when it is not null / white space -/
  searchString : Option String := none
  tickerFrame : Nat := 0
  /-- `Main.inputText` has Back down; `Main.backSpaceRate`, `backSpaceCount` -/
  keyboard : OriginalStorageRename.TextInput := {}
  inputBack : Bool := false
  backSpaceRate : Float32 := 0
  backSpaceCount : Int := 0
  /-- `UIScrollbar._isDragging`, `_dragYOffset` -/
  dragging : Bool := false
  dragYOffset : Float32 := 0
  innerListTop : Float32 := 0
  innerListHeight : Float32 := 0
  /-- `UIScrollbar` -/
  viewPosition : Float32 := 0
  viewSize : Float32 := 1
  maxViewSize : Float32 := 20
  hoveringHandle : Bool := false
  /-- `UserInterface` -/
  lastHover : Option El := none
  left : Pointer := {}
  clickDisabledMs : Float := 0
  totalMs : Float := 0
  /-- `_filterer.ActiveFilters` (the tab index) -/
  activeFilter : Option Nat := none
  /-- `_recipes` (insertion order), `_filteredRecipes` (positions in `recipes`) -/
  recipes : Array Entry := #[]
  filtered : Array Nat := #[]
  resetForGuide : Option Int := none
  selected : Option Nat := none
  /-- `Main.focusRecipe` -/
  focusRecipe : Int := 0
  deriving Inhabited

/-- What a frame reports besides its draws. -/
structure Output where
  copied : Option String := none
  sprites : Array Sprite := #[]
  /-- `player.mouseInterface` set by the window -/
  mouseInterface : Bool := false
  /-- the item `ItemSlot.MouseHover` made `Main.HoverItem` (`none`: not touched) -/
  hoverItem : Option Nat := none
  /-- the guide slot's `ItemSlot.Handle(ref Main.guideItem, 7, true)` ran (hovered: the crafting logic swaps) -/
  guideSlotHandle : Bool := false
  /-- `Main.stackSplit = 15` and `_preventCraftingBecauseClickWasUsedToChangeFocusedRecipe` (a recipe was clicked: a
  new one selected, or the selected one clicked again — `HoverOverCraftingItemButton` with an item in the guide slot,
  where nothing crafts) -/
  recipeClicked : Bool := false
  /-- `UIList.GetClippingRectangle` (backbuffer pixels): the scissor of the grid's draws -/
  clip : Rect := ⟨0, 0, 0, 0⟩
  /-- sprite index range drawn inside that scissor -/
  clipRange : Nat × Nat := (0, 0)
  /-- the click areas of this frame (UI space): the drawn grid cells `(recipe index, hitbox)` (the hover test also
  requires the item grid itself to be hovered, i.e. the point inside the list), and the named areas: `guide`, `create`,
  `material i`, `tab i`, `search`, `cancel`, `toggle` (the toggle's test is the open square 29 x 29 around it),
  `scrollbar`, `window` -/
  cells : Array (Nat × Rect) := #[]
  areas : Array (String × Rect) := #[]
  /-- the grid toggle was clicked: `NewCraftingUI.ToggleInInventory` closed the window (`PipsUseGrid = false`, the
  classic list shows; `Main.mouseLeftRelease = false`) -/
  gridClosed : Bool := false
  /-- what the frame wrote to `Main._preventCraftingBecauseClickWasUsedToChangeFocusedRecipe` (`true` with every
  `recipeClicked`; `false` from `HoverOverCraftingItemButton` while no button is down), `none`: not written -/
  preventCrafting : Option Bool := none
  /-- `Main.HoverOverCraftingItemButton` ran (the selected, available recipe hovered): `Main.craftingHide = true` -/
  craftingHide : Bool := false
  deriving Inhabited

/-! ## Styles -/

def textBoxPad : Float32 := 12

def State.style (st : State) (cat : Catalog) (e : El) : Except String Style := do
  match e with
  | .state => return { left := ⟨20, 0⟩, top := ⟨312, 0⟩, width := ⟨490, 0⟩, height := ⟨-350, 1⟩ }
  | .root => return { width := fill, height := fill }
  | .panel => return { width := fill, height := ⟨-38, 1⟩, top := ⟨38, 0⟩, padL := 12, padR := 8, padT := 12, padB := 12 }
  | .title =>
    return { left := ⟨-1, 0⟩, top := ⟨-2, 0⟩, minW := ⟨st.titleW, 0⟩, minH := ⟨st.titleH, 0⟩ }
  | .search => return { height := ⟨24, 0⟩, width := ⟨st.searchWidthPx, 1⟩, top := ⟨-4, 0⟩, hAlign := 1 }
  | .searchButton =>
    let (w, h) ← cat.tex "Images/UI/Bestiary/Button_Search_2"
    return { width := ⟨natF w, 0⟩, height := ⟨natF h, 0⟩, vAlign := 0.5 }
  | .searchPanel =>
    let (w, _) ← cat.tex "Images/UI/Bestiary/Button_Search_2"
    return { width := ⟨-(natF w) - 3, 1⟩, height := fill, vAlign := 0.5, hAlign := 1 }
  | .searchBar => return { width := fill, height := fill, vAlign := 0.5 }
  | .textBox =>
    return { width := fill, height := fill, vAlign := 0.5, minW := ⟨st.textBoxMinW, 0⟩, minH := ⟨st.textBoxMinH, 0⟩,
             padL := textBoxPad, padR := textBoxPad, padT := textBoxPad, padB := textBoxPad }
  | .cancel =>
    let (w, h) ← cat.tex "Images/UI/SearchCancel"
    return { width := ⟨natF w, 0⟩, height := ⟨natF h, 0⟩, hAlign := 1, vAlign := 0.5, left := ⟨-2, 0⟩ }
  | .grid => return { width := fill, height := ⟨st.gridHeightPx, 1⟩, vAlign := 1 }
  | .separator =>
    let (_, h) ← cat.tex "Images/UI/CharCreation/Separator1"
    return { width := ⟨-8, 1⟩, height := ⟨natF h, 0⟩, hAlign := 0.5 }
  | .list => return { width := ⟨-20, 1⟩, height := ⟨-7, 1⟩, vAlign := 1 }
  | .innerList => return { width := fill, height := fill, top := ⟨st.innerListTop, 0⟩ }
  | .items => return { width := fill, hAlign := 0.5, minH := ⟨st.itemsMinH, 0⟩ }
  | .scrollbar => return { width := ⟨20, 0⟩, maxW := ⟨20, 0⟩, height := ⟨-19, 1⟩, top := ⟨-4, 0⟩, vAlign := 1, hAlign := 1, padT := 5, padB := 5 }
  | .filters => return { height := ⟨40, 0⟩, width := ⟨natF (40 * filterCount), 0⟩, top := ⟨4, 0⟩, hAlign := 0.5 }
  | .tab i => return { left := ⟨natF (40 * i), 0⟩, width := ⟨40, 0⟩, height := ⟨46, 0⟩ }
  | .icon _ => return { left := ⟨6, 0⟩, width := ⟨28, 0⟩, height := ⟨30, 0⟩, vAlign := 0.5 }

/-- `UserInterface.GetDimensions()`: `(0, 0, OriginalScreenSize / UIScale)`. -/
def screenCalc (inp : Input) : Calc := ⟨0, 0, single inp.screenWidth / inp.uiScale, single inp.screenHeight / inp.uiScale⟩

def floatMax : Float32 := Float32.ofBits 0x7F7FFFFF

/-- `UIDynamicItemCollection.UpdateSize`: `_itemsPerLine = width / 44`, `MinHeight = 44 * ceil(count / perLine)`. -/
def State.updateSize (st : State) : State :=
  let w := truncI (st.dims.getD .items {}).dims.w
  let per := (w / 44).toNat
  let rows : Nat := if per == 0 then 0 else
    (Float.ceil (natF st.contents.size / natF per).toFloat).toUInt64.toNat
  { st with perLine := per, itemsMinH := natF (44 * rows) }

/-- `UIScrollbar.SetView(viewSize, maxViewSize)` -/
def State.setView (st : State) (viewSize maxViewSize : Float32) : State :=
  let viewSize := clampF viewSize 0 maxViewSize
  { st with viewPosition := clampF st.viewPosition 0 (maxViewSize - viewSize), viewSize, maxViewSize }

/-- `UIElement.Recalculate` of `e` and its subtree (the item grid's `UpdateSize` after its own recalculation;
`UIList.Recalculate`'s inner height and `UpdateScrollbar`). -/
partial def State.recalc (st : State) (cat : Catalog) (inp : Input) (e : El) : Except String State := do
  let parentInner : Calc := match e.parent with
    | none => screenCalc inp
    | some p => (st.dims.getD p {}).inner
  -- `Recalculate`: a child of a UIList sees an unbounded parent height
  let parentInner := if e.parent == some .list then { parentInner with h := floatMax } else parentInner
  let d := layout (← st.style cat e) parentInner
  let mut st := { st with dims := st.dims.insert e d }
  for c in e.children do st ← st.recalc cat inp c
  if e == .items then st := st.updateSize
  if e == .list then
    let ih := (st.dims.getD .items {}).dims.h
    st := { st with innerListHeight := ih }
    st := st.setView (st.dims.getD .list {}).inner.h ih
  return st

/-! ## Hit testing and hover (`UIElement.GetElementAt`, `ContainsPoint`, `MouseOver` / `MouseOut` bubbling) -/

/-- `ContainsPoint`: the `_dimensions` rectangle (`UIInnerList` contains every point). -/
def State.contains (st : State) (e : El) (mx my : Int) : Bool :=
  if e == .innerList then true else (st.dims.getD e {}).dims.rect.contains mx my

/-- `UIElement.GetElementAt` (no element here passes the mouse through). -/
partial def State.elementAt (st : State) (e : El) (mx my : Int) : Option El := Id.run do
  let mut found : Option El := none
  for c in e.children.reverse do
    if !c.ignoresMouse && st.contains c mx my then
      found := some c
      break
  match found with
  | some c => st.elementAt c mx my
  | none => if e.ignoresMouse then none else if st.contains e mx my then some e else none

partial def El.chain (e : El) : List El := e :: (match e.parent with | some p => p.chain | none => [])

/-- `IsMouseHovering`: `MouseOut` of the last hovered element and `MouseOver` of the new one both bubble to the root,
so after an update exactly the new element and its ancestors are hovering. -/
def State.hovering (st : State) (e : El) : Bool :=
  match st.lastHover with
  | some h => h.chain.contains e
  | none => false

/-! ## The recipe lists (`ResetRecipes`, `UpdateContents`) -/

/-- `NewCraftingUI.ResetRecipes`: both lists emptied (the item grid keeps showing its entry objects until its next
`SetContentsToShow`). -/
def State.resetRecipes (st : State) (inp : Input) : State :=
  let kept := st.recipes.foldl (fun m e => if st.contents.contains e.uid then m.insert e.uid e else m) st.detached
  { st with resetForGuide := some inp.guideObject, recipes := #[], filtered := #[], detached := kept }

/-- The `RecipeEntry` object an item-grid slot holds. -/
def State.entry (st : State) (uid : Nat) : Option Entry :=
  match st.recipes.find? (·.uid == uid) with
  | some e => some e
  | none => st.detached[uid]?

/-- `NewCraftingUI.FitsFilter` → `EntryFilterer.FitsFilter`: the search (`ItemFilters.BySearch` when the search text is
not null / white space: one of the create item's tooltip lines contains it, `OrdinalIgnoreCase`), then the active tab
(no `RecipeFilter`, no always-active filter). -/
def State.fitsFilter (st : State) (cat : Catalog) (index : Nat) : Except String Bool := do
  let r ← cat.recipe index
  if !isNullOrWhiteSpace st.searchString then
    let k := st.searchString.getD ""
    if k.any (fun c : Char => c.toNat ≥ 128) then throw "a non-ASCII search text (OrdinalIgnoreCase beyond ASCII) is not transcribed"
    let mut hit := false
    for l in r.searchLines do
      if ← containsOrdinalIgnoreCase l k then
        hit := true
        break
    if !hit then return false
  match st.activeFilter with
  | none => return true
  | some k => return (r.fits[k]?).getD false

/-- `NewCraftingUI.UpdateContents` (after the caller's `Recipe.UpdateRecipeList`). -/
def State.updateContents (st : State) (cat : Catalog) (inp : Input) : Except String State := do
  let mut st := st
  if st.resetForGuide != some inp.guideObject then st := st.resetRecipes inp
  st := { st with recipes := st.recipes.map ({ · with availableIndex := -1 }) }
  let wasEmpty := st.filtered.isEmpty
  -- NewCraftingUI.GetNumRecipes suppresses the ordinary available-recipe list
  -- while the Guide's material slot is empty.
  let count := if inp.guide.isNone then 0 else inp.available.size
  for i in [0:count] do
    let idx := inp.available[i]!
    match st.recipes.findIdx? (·.index == idx) with
    | some p => st := { st with recipes := st.recipes.modify p ({ · with availableIndex := i }) }
    | none =>
      let mut e : Entry := { uid := st.nextUid, index := idx, availableIndex := i }
      st := { st with nextUid := st.nextUid + 1 }
      if ← st.fitsFilter cat idx then
        e := { e with gridIndex := st.filtered.size }
        st := { st with filtered := st.filtered.push st.recipes.size }
      st := { st with recipes := st.recipes.push e }
  match st.selected with
  | none => pure ()
  | some sel =>
    match st.recipes.find? (·.index == sel) with
    | none => st := { st with selected := none }
    | some e => if e.availableIndex >= 0 then st := { st with focusRecipe := e.availableIndex }
  if st.contents.size != st.filtered.size || (wasEmpty && st.filtered.size > 0) then
    let rs := st.recipes
    st := { st with contents := st.filtered.map (fun p => rs[p]!.uid), detached := {} }
    st := st.updateSize
  return st

/-! ## The search box (`UIWrappedSearchBar` → `UISearchBar` → `UITextBox`) -/

/-- `UITextBox.SetText(text)`: `Utils.TrimUserString(text, 50)`, then `UITextPanel.SetText`: `MinWidth =
MeasureString(text).X * 0.8 + PaddingLeft + PaddingRight` (the text box is recalculated only by its owners). -/
def State.setText (st : State) (cat : Catalog) (text : String) (white : Bool) : Except String State := do
  if text.any (fun c : Char => c.toNat ≥ 0x10000) then throw "a search text outside the BMP (UTF-16 surrogate pairs) is not transcribed"
  let text := if text.length > 50 then String.ofList (text.toList.take 50) else text
  let (w, _) ← cat.mouseText.measure text
  return { st with shownText := text, shownWhite := white, textBoxMinW := w * 0.8 + textBoxPad + textBoxPad }

/-- `UISearchBar.SetContents(text, forced)`: the shown text (the hint in Gray when null / empty, else the text in
White, `actualContents` the text as `UITextBox` kept it), `TrimDisplayIfOverElementDimensions(0)` (while the text
box's `X + MinWidth` reaches past the search bar's right edge: the last character dropped, `RecalculateChildren`),
then `OnContentsChanged(text)` → `UIWrappedSearchBar.UpdateSearchContents` → `NewCraftingUI.OnSearchContentsChanged`:
`_filterer.SetSearchFilter(text)` (the untrimmed text), `ResetRecipes`. -/
def State.setContents (st : State) (cat : Catalog) (inp : Input) (text : Option String) (forced : Bool) :
    Except String State := do
  if st.actualContents == text && !forced then return st
  let mut st := { st with actualContents := text }
  let t := text.getD ""
  if t.isEmpty then
    st ← st.setText cat cat.searchHint false
  else
    st ← st.setText cat t true
    st := { st with actualContents := some st.shownText }
  let bar := (st.dims.getD .searchBar {}).dims
  if !(bar.w == 0 && bar.h == 0) then
    let right := truncI bar.x + truncI bar.w
    while truncI (st.dims.getD .textBox {}).dims.x + truncI st.textBoxMinW > right && st.shownText.length > 0 do
      st ← st.setText cat (String.ofList st.shownText.toList.dropLast) st.shownWhite
      st ← st.recalc cat inp .textBox
      st := { st with actualContents := some st.shownText }
  return { st with searchString := text }.resetRecipes inp

/-- The shared transcription of `Main.GetInputText` supplies the same key-history,
clipboard, chat-tag Backspace and repeat rules used by the original rename editor. -/
def State.getInputText (st : State) (typed : Array (Nat × String)) (held : Array Nat) (old : Option String)
    (queued : Option (Array UInt16) := none) (clipboard : Array UInt16 := #[]) :
    Except String (State × String × Bool × Bool × Option String) := do
  let keys : OriginalStorageRename.Keys := {
    control := held.contains 162 || held.contains 163, alt := held.contains 164 || held.contains 165
    shift := held.contains 160 || held.contains 161, z := held.contains 90, x := held.contains 88
    c := held.contains 67, v := held.contains 86, insert := held.contains 45, delete := held.contains 46
    escape := held.contains 27, back := held.contains 8 }
  let queued := queued.getD (typed.foldl (fun queue (code, text) =>
    if code == 13 || code == 27 then queue.push code.toUInt16
    else if code ≥ 32 && code != 127 then queue ++ OriginalStorageRename.utf16 text
    else queue) #[])
  let (keyboard, result) := OriginalStorageRename.getInputText st.keyboard
    (OriginalStorageRename.utf16 (old.getD "")) { keys, queued, clipboard }
  return ({ st with
    keyboard, inputBack := keyboard.last.back, backSpaceRate := keyboard.backSpaceRate
    backSpaceCount := keyboard.backSpaceCount }, OriginalStorageRename.ofUtf16 result.text,
    result.enter, result.escape, result.copied.map OriginalStorageRename.ofUtf16)

/-- `UISearchBar.ToggleTakingText`: `IsWritingText` and the text box's `ShowInputTicker` flip, `Main.clrInput` (the
caller drops the frame's text events), the wrapped bar's start / end handlers recolour the search panel's border
(`Main.OurFavoriteColor` while writing). -/
def State.toggleTakingText (st : State) : State := { st with writing := !st.writing }

/-- `UIScrollbar.GetHandleRectangle` -/
def State.handleRect (st : State) : Rect :=
  let inner := (st.dims.getD .scrollbar {}).inner
  let (vs, mv) := if st.maxViewSize == 0 && st.viewSize == 0 then ((1 : Float32), (1 : Float32)) else (st.viewSize, st.maxViewSize)
  ⟨truncI inner.x, truncI (inner.y + inner.h * (st.viewPosition / mv)) - 3, 20, truncI (inner.h * (vs / mv)) + 7⟩

/-! ## The update pass (`NewCraftingUI.UpdateUI` → `UserInterface.Update`) -/

/-- What the click on an element does (`LeftClick` bubbling to the handlers of this tree); also whether the search
box toggled (`Main.clrInput`). -/
def State.leftClick (st : State) (cat : Catalog) (inp : Input) (e : El) : Except String (State × Bool) := do
  let mut st := st
  let mut toggled := false
  for x in e.chain do
    match x with
    | .tab i =>
      -- singleFilterButtonClick: a tab not active clears the others, then ToggleFilter; OnClickingOption → ResetRecipes
      let active := if st.activeFilter == some i then none else some i
      return ({ st with activeFilter := active }.resetRecipes inp, toggled)
    | .cancel =>
      -- searchCancelButton_OnClick: HasContents → SetContents(null, forced)
      if !isNullOrWhiteSpace st.actualContents then st ← st.setContents cat inp none true
    | .searchButton | .searchPanel =>
      -- Click_SearchArea (on the search button and the search panel), not for the panel's own child (cancel)
      if e.parent != some .searchPanel then
        st := st.toggleTakingText
        toggled := true
    | _ => pure ()
  return (st, toggled)

/-- `UserInterface.Update` (hover, the left button's down / click / up, the wheel), then the tree's `Update`
(`UIWrappedSearchBar.Update`: while writing, the left button down outside the search button and panel ends it).
Returns the state, whether the item grid is hovered (its `Update` sets `mouseInterface`), whether the search box
toggled (`Main.clrInput`: the frame's text events are dropped). -/
def State.update (st : State) (cat : Catalog) (inp : Input) : Except String (State × Bool × Bool) := do
  let mut st := st
  let mut cleared := false
  let hovered := st.elementAt .state inp.mouseX inp.mouseY
  st := { st with clickDisabledMs := max 0 (st.clickDisabledMs - inp.elapsedMs), totalMs := st.totalMs + inp.elapsedMs }
  let disabled := st.clickDisabledMs > 0
  if hovered != st.lastHover then st := { st with lastHover := hovered }
  if !disabled then
    let down := inp.mouseLeft
    let p := st.left
    if down && !p.wasDown && hovered.isSome then
      let t := hovered.get!
      let mut p := { p with lastDown := some t }
      if p.lastClicked == some t && st.totalMs - p.lastTimeDown < 500 then p := { p with lastClicked := none }
      st := { st with left := { p with lastTimeDown := st.totalMs } }
      -- LeftMouseDown → UIScrollbar.LeftMouseDown (the scrollbar itself the target): on the handle it starts
      -- dragging, elsewhere the view jumps to centre the handle on the mouse
      if t == .scrollbar then
        let h := st.handleRect
        if h.contains inp.mouseX inp.mouseY then
          st := { st with dragging := true, dragYOffset := single inp.mouseY - single h.y }
        else
          let inner := (st.dims.getD .scrollbar {}).inner
          let v := single inp.mouseY - inner.y - single (h.h / 2)
          st := { st with viewPosition := clampF (v / inner.h * st.maxViewSize) 0 (st.maxViewSize - st.viewSize) }
    else if !down && p.wasDown && p.lastDown.isSome then
      let ld := p.lastDown.get!
      if st.contains ld inp.mouseX inp.mouseY then
        let (st', t) ← st.leftClick cat inp ld
        st := { st' with left := { st'.left with lastClicked := some ld } }
        cleared := cleared || t
      -- LeftMouseUp → UIScrollbar.LeftMouseUp: dragging ends
      if ld == .scrollbar then st := { st with dragging := false }
      st := { st with left := { st.left with lastDown := none } }
    st := { st with left := { st.left with wasDown := down } }
  if inp.wheel != 0 then
    if let some h := hovered then
      if h.chain.contains .list then
        st := { st with viewPosition := clampF (st.viewPosition - single inp.wheel) 0 (st.maxViewSize - st.viewSize) }
  -- UIWrappedSearchBar.Update: a press anywhere but the search button / panel while writing ends the writing
  if st.writing && inp.mouseLeft && !(st.hovering .searchButton || st.hovering .searchPanel) then
    st := st.toggleTakingText
    cleared := true
  -- UIElement.Update: the item grid sets mouseInterface while hovered
  return (st, st.hovering .items, cleared)

/-! ## Drawing helpers -/

/-- `UIPanel.DrawPanel` (corner 12, bar 4) on `_dimensions`. -/
def drawPanel (tex : String) (tw th : Nat) (d : Calc) (c : Color) : Array Sprite :=
  let cs : Int := 12
  let bs : Int := 4
  let x1 := truncI d.x
  let y1 := truncI d.y
  let x2 := x1 + truncI d.w - cs
  let y2 := y1 + truncI d.h - cs
  let w := x2 - x1 - cs
  let h := y2 - y1 - cs
  let r (a b c' d' : Int) : Rect := ⟨a, b, c', d'⟩
  #[drawRectSrc tex tw th (r x1 y1 cs cs) (r 0 0 cs cs) c,
    drawRectSrc tex tw th (r x2 y1 cs cs) (r (cs + bs) 0 cs cs) c,
    drawRectSrc tex tw th (r x1 y2 cs cs) (r 0 (cs + bs) cs cs) c,
    drawRectSrc tex tw th (r x2 y2 cs cs) (r (cs + bs) (cs + bs) cs cs) c,
    drawRectSrc tex tw th (r (x1 + cs) y1 w cs) (r cs 0 bs cs) c,
    drawRectSrc tex tw th (r (x1 + cs) y2 w cs) (r cs (cs + bs) bs cs) c,
    drawRectSrc tex tw th (r x1 (y1 + cs) cs h) (r 0 cs cs bs) c,
    drawRectSrc tex tw th (r x2 (y1 + cs) cs h) (r (cs + bs) cs cs bs) c,
    drawRectSrc tex tw th (r (x1 + cs) (y1 + cs) w h) (r cs cs bs bs) c]

/-- `UIPanel.DrawSelf`: background, then border. -/
def panelSprites (cat : Catalog) (d : Calc) (bg border : Color) : Except String (Array Sprite) := do
  let (bw, bh) ← cat.tex "Images/UI/PanelBackground"
  let (rw, rh) ← cat.tex "Images/UI/PanelBorder"
  return drawPanel "Images/UI/PanelBackground" bw bh d bg ++ drawPanel "Images/UI/PanelBorder" rw rh d border

/-- `ChatManager.DrawColorCodedStringWithShadow(…, text, pos, color, 0, origin, scale, -1, spread)` (plain text). -/
def shadowText (f : Font) (text : String) (x y : Float32) (c : Color) (spread : Float32) (s : Float32 := 1)
    (ox oy : Float32 := 0) : Except String (Array Sprite) :=
  OriginalForestChatLayout.drawWithShadow f text x y c ox oy s s spread

/-- `Item.GetAlpha(color)`, generic branch: `(int)(c * (255 - alpha) / 255f)`, `A = clamp(A - alpha)`. -/
def getAlpha (it : ItemVisual) (c : Color) : Color :=
  let f : Float32 := single (255 - it.alpha : Int) / 255
  Color.mk' (truncI (natF c.r * f)) (truncI (natF c.g * f)) (truncI (natF c.b * f)) ((c.a : Int) - it.alpha)

/-- `Item.GetColor(color)`: `item.color - (255 - c)` per channel, clamped. -/
def getColor (it : ItemVisual) (c : Color) : Color :=
  Color.mk' ((it.color.r : Int) - (255 - c.r)) ((it.color.g : Int) - (255 - c.g)) ((it.color.b : Int) - (255 - c.b))
    ((it.color.a : Int) - (255 - c.a))

/-- Whether the generic icon path applies to an item type. -/
def ItemVisual.check (it : ItemVisual) : Except String Unit := do
  if let some why := it.special then throw s!"item {it.type}: {why}"
  if getAlphaSpecialTypes.any (fun (a, b) => a ≤ it.type && it.type ≤ b) then throw s!"item {it.type}: Item.GetAlpha special type"
  if itemLightSpecialTypes.contains it.type then throw s!"item {it.type}: ItemSlot.GetItemLight special type"
  if iconFrameSpecialTypes.contains it.type then throw s!"item {it.type}: DrawItemIcon frame override"

/-- `Utils.Frame(texture, 1, frames, 0, frame, 0, -2)`: one frame of a vertical strip, two pixels shorter. -/
private def stripFrame (w h frames frame : Nat) : Rect :=
  ⟨0, frame * (h / frames), w, ((h / frames : Nat) : Int) - 2⟩

/-- `ItemSlot.DrawItemIcon(item, context, sb, center, scale, sizeLimit 32, environmentColor, 1f, false)`:
the frame (`Main.itemAnimations`, 5644's override), `DrawItem_GetColorAndScale` (`GetItemLight`, the 32 px fit),
the icon (`Item.GetAlpha`), the `Item.color` overlay (`Item.GetColor`), the glow mask, the `TrapSigned` wire and the
`DrawUnsafeIndicator` mark, in vanilla order. `cat.icons` holds the globals the special types read. Also returns
`DrawItemIcon`'s return value, the icon's final scale (`ItemSlot.Draw` sizes its cooldown overlay with it). -/
def itemIconScaled (cat : Catalog) (it : ItemVisual) (ctx : Nat) (cx cy : Float32) (scale : Float32) (c : Color) :
    Except String (Array Sprite × Float32) := do
  if let some why := it.special then throw s!"item {it.type}: {why}"
  let rt := cat.icons
  let tex := if it.texture.isEmpty then s!"Images/Item_{it.type}" else it.texture
  -- 5644 asks its animation for frame 0 unless the slot is context 31 or the player spectates
  let frameOverride := it.type == 5644 && ctx != 31 && !rt.anyoneToSpectate
  let frame : Rect := match it.animation with
    | none => ⟨0, 0, it.texW, it.texH⟩
    | some (n, f, pp) =>
      let f := if frameOverride then 0 else if pp && f ≥ n then n * 2 - 2 - f else f
      stripFrame it.texW it.texH n f
  if it.scryingOrb && !frameOverride then
    throw s!"item {it.type}: DrawAnimationScryingOrb frame outside the override (its state follows Main.rand)"
  -- DrawItem_GetColorAndScale: GetItemLight, then fit into 32 px (width test first when width > height)
  let (light, lightScale) := OriginalItemIcon.itemLight rt it.type it.iconPulse c
  let fit : Float32 :=
    if single frame.w > 32 || single frame.h > 32 then
      if frame.w > frame.h then 32 / single frame.w else 32 / single frame.h
    else 1
  let s := scale * fit * lightScale
  let ox := single frame.w / 2
  let oy := single frame.h / 2
  let alphaColor := OriginalItemIcon.getAlpha rt it.type it.alpha it.bossBag light
  let mut out := #[drawScaled tex it.texW it.texH cx cy (some frame) (scaleColor (multiplyRGBA alphaColor white) 1) ox oy s s]
  if it.color != transparent then
    let overlay := scaleColor (multiplyRGBA (OriginalItemIcon.getColor it.color c) white) 1
    out := out.push (drawScaled tex it.texW it.texH cx cy (some frame) overlay ox oy s s)
  -- the glow mask (not for 3779, 46, 5462), drawn over the icon's frame unless the type picks its own
  if let some (gtex, gw, gh) := it.glow then
    if !([3779, 46, 5462].contains it.type) then
      let mut gframe := frame
      let mut gcolor := white
      match it.type with
      | 3858 =>
        gcolor := scaleColor ⟨255, 255, 255, 63⟩ 0.75
        gframe := stripFrame gw gh 3 ((rt.miscCounter.tmod 15).tdiv 5).toNat
      | 5146 => gcolor := Color.mk' rt.discoR rt.discoG rt.discoB 255
      | 5669 =>
        let t := OriginalItemIcon.wrappedLerp 0.5 1 (single (rt.miscCounter.tmod 100) / 100)
        gcolor := OriginalItemIcon.withAlpha (Color.lerp gcolor ⟨180, 85, 30, 255⟩ t) it.alpha
      | 5670 | 5671 => gcolor := OriginalItemIcon.phaseColor rt it.shoot false
      | 6148 | 6149 | 6150 | 6151 | 6174 => gcolor := OriginalItemIcon.trustyPalGlow rt
      | _ => pure ()
      let gox := single gframe.w / 2
      let goy := single gframe.h / 2
      if it.type == 5670 || it.type == 5671 then
        out := out.push (drawScaled gtex gw gh cx cy (some gframe) (scaleColor gcolor 1) gox goy s s)
        gcolor := OriginalItemIcon.phaseColor rt it.shoot true
      out := out.push (drawScaled gtex gw gh cx cy (some gframe) (scaleColor gcolor 1) gox goy s s)
  -- ItemID.Sets.TrapSigned: Wires (4, 58, 8 x 8) at the top-right corner of the icon
  if it.trapSigned then
    let (ww, wh) ← cat.tex "Images/Wires"
    let dx := single frame.w * 0.45 * 1 * s * 1
    let dy := single frame.h * 0.45 * -1 * s * 1
    out := out.push (drawScaled "Images/Wires" ww wh (cx + dx) (cy + dy) (some ⟨4, 58, 8, 8⟩) (scaleColor c 1) 4 4 s s)
  -- ItemID.Sets.DrawUnsafeIndicator: Extra_258 at the top-right corner, 4 px further in
  if it.unsafeIndicator then
    let (ew, eh) ← cat.tex "Images/Extra_258"
    let dx := (single frame.w * 0.45 + -4) * 1 * s * 1
    let dy := (single frame.h * 0.45 + -4) * -1 * s * 1
    out := out.push (drawScaled "Images/Extra_258" ew eh (cx + dx) (cy + dy) (some ⟨0, 0, ew, eh⟩) (scaleColor c 1)
      (natF ew / 2) (natF eh / 2) s s)
  return (out, s)

/-- `ItemSlot.DrawItemIcon`'s draws (`itemIconScaled` without the returned scale). -/
def itemIcon (cat : Catalog) (it : ItemVisual) (ctx : Nat) (cx cy : Float32) (scale : Float32) (c : Color) :
    Except String (Array Sprite) :=
  (·.1) <$> itemIconScaled cat it ctx cx cy scale c

/-- `ItemSlot.Draw(sb, ref item, context, position, slotColor)` for contexts 7 (guide), 41 (grid), 42 (create),
43 (material), plain items (not favourite, not new, no mouse item over the slot). -/
def itemSlot (cat : Catalog) (ctx : Nat) (item : Option (Nat × Nat)) (x y : Float32) (slotColor : Color)
    (scale : Float32) (inventoryBack : Color) (gridSelected : Bool) : Except String (Array Sprite) := do
  let (tex, col) := if ctx == 7 then ("Images/Inventory_Back4", inventoryBack) else ("Images/Inventory_Back18", craftSlotColor)
  let col := if ctx == 41 || ctx == 42 || ctx == 43 then multiplyRGBA col slotColor else col
  let col := scaleColor col 1
  let (tw, th) ← cat.tex tex
  let mut out := #[drawScaled tex tw th x y none col 0 0 scale scale]
  if ctx == 41 && gridSelected then
    let (sw, sh) ← cat.tex "Images/Inventory_Back24"
    out := out.push (drawScaled "Images/Inventory_Back24" sw sh x y none inventoryBack 0 0 scale scale)
  let cx := x + natF tw / 2 * scale
  let cy := y + natF th / 2 * scale
  match item with
  | none =>
    if ctx == 7 then
      -- the empty guide slot's hint icon: TextureAssets.Extra[54] frame 18 of 3 x 7, shrunk by 2
      let (ew, eh) ← cat.tex "Images/Extra_54"
      let fw := ew / 3
      let fh := eh / 7
      let fr : Rect := ⟨(18 % 3) * fw, (18 / 3) * fh, fw - 2, fh - 2⟩
      out := out.push (drawScaled "Images/Extra_54" ew eh cx cy (some fr) (scaleColor white (0.35 * 1))
        (single fr.w / 2) (single fr.h / 2) scale scale)
  | some (t, n) =>
    let it ← cat.item t
    out := out ++ (← itemIcon cat it ctx cx cy scale slotColor)
    if n > 1 then
      let ls := single cat.itemStack.lineSpacing
      let (offX, offY, s) := if ctx == 43 then ((10 : Float32) - 5, single (26 + cat.itemStack.lineSpacing) + 7, scale * 1.2)
        else (10, single (26 + cat.itemStack.lineSpacing), scale)
      out := out ++ (← shadowText cat.itemStack (toString n) (x + offX * scale) (y + offY * scale) (scaleColor slotColor 1) s s 0 ls)
  return out

/-! ## The frame -/

/-- `NewCraftingUI.Open` → `UserInterface.SetState`: `ResetState` (clicks disabled for 200 ms, the pointer re-read,
nothing hovered), `OnActivate` (`_selectedRecipeIndex` = `availableRecipe[focusRecipe]`, `ResetRecipes`, the search
box's `SetContents("")`, no filter tab), then `Recalculate`. `prev`: the state the window was closed with — the
kept `_instance` the grid toggle reopens (`ToggleInInventory`), which keeps its scroll position, search box, item
grid and layout. Vanilla creates the window once (`NewCraftingUI._instance`) and every later `Open` — from the
Guide's Crafting button again, or the toggle — reuses it, so pass the state of the previous open (after `close`);
`none` only for the first open of the session. -/
def State.openWindow (cat : Catalog) (inp : Input) (focusRecipe : Int := 0) (availableAtFocus : Nat := 0)
    (prev : Option State := none) : Except String State := do
  let base : State := match prev with
    | some p => p
    | none => { titleH := 16, textBoxMinH := 16 * 0.8 + textBoxPad + textBoxPad }
  let mut st : State := { base with
    visible := true
    clickDisabledMs := max base.clickDisabledMs 200
    left := { wasDown := inp.mouseLeft }
    lastHover := none
    focusRecipe := focusRecipe
    selected := some availableAtFocus
    activeFilter := none }
  st := st.resetRecipes inp
  st ← st.setContents cat inp (some "") false
  st.recalc cat inp .state

/-- `NewCraftingUI.Close` (the grid toggle's `ToggleInInventory`) → `UserInterface.SetState(null)`: `Deactivate`
(`NewCraftingUI.OnDeactivate`: no selection; `UISearchBar.OnDeactivate`: the writing ends), `ResetState`. -/
def State.close (st : State) (inp : Input) : State :=
  { st with visible := false, selected := none, writing := false, lastHover := none,
            left := { wasDown := inp.mouseLeft }, clickDisabledMs := max st.clickDisabledMs 200 }

/-- `Main.HoverOverCraftingItemButton(Main.focusRecipe)` (`HandleCraftSlot` on the selected, available recipe):
`mouseInterface`, `craftingHide`, the hovered item `recipe[availableRecipe[focusRecipe]].createItem`; with an item
in the guide slot a click only refocuses (`stackSplit = 15`, the craft suppressed); with the guide slot empty a
press would craft (`CraftingRequests.CraftItem`: a boundary). No button down: the suppression flag clears. `guide`:
`Main.guideItem` at that point of the draw. -/
def hoverOverCraftingItemButton (cat : Catalog) (st : State) (inp : Input) (guide : Option (Nat × Nat))
    (out : Output) : Except String Output := do
  let idx ← match (if st.focusRecipe < 0 then none else inp.available[st.focusRecipe.toNat]?) with
    | some i => pure i
    | none => throw s!"Main.availableRecipe[{st.focusRecipe}] not supplied"
  let r ← cat.recipe idx
  let mut out := { out with mouseInterface := true, craftingHide := true, hoverItem := some r.create }
  let clicked := (inp.mouseLeft && inp.mouseLeftRelease) || (inp.mouseRight && inp.mouseRightRelease)
  let held := inp.mouseLeft || inp.mouseRight
  if guide.isNone then
    if held then
      throw "the left button on the selected recipe with the guide slot empty (crafting: CraftingRequests.CraftItem) is not transcribed"
  else if clicked then
    out := { out with recipeClicked := true, preventCrafting := some true }
  if !held then out := { out with preventCrafting := some false }
  return out

/-- Grid parameters (`UIDynamicItemCollection.GetGridParameters`): start x / y, first and end index. -/
def State.gridParams (st : State) : Int × Int × Nat × Nat :=
  let r := (st.dims.getD .items {}).dims.rect
  let cull := (st.dims.getD .list {}).dims.rect
  let per : Int := st.perLine
  let centerX := r.x + r.w / 2
  let sx := centerX - truncI (single (44 * per) * 0.5)
  let rows := (min cull.y r.y - cull.y).tdiv 44
  let sy := r.y + (-rows) * 44
  let start := (-rows) * per
  let visible := (Float.ceil (single cull.h / 44).toFloat).toUInt64.toNat * per.toNat
  let stop := min st.contents.size (visible + start.toNat + per.toNat)
  (sx, sy, start.toNat, stop)

/-- `Collision.CheckAABBvAABBCollision` -/
def aabb (a b : Calc) : Bool := a.x < b.x + b.w && a.y < b.y + b.h && b.x < a.x + a.w && b.y < a.y + a.h

/-- `CraftingUI.DrawGridToggle` in the Guide's crafting page, including its
selected-page indicator. The closed window uses frames 2/3, the open one 4/5. -/
def gridToggleSprites (cat : Catalog) (inp : Input) (visible : Bool) : Except String (Array Sprite) := do
  let gx : Int := 42
  let gy : Int := 285
  let hover := inp.mouseX > gx - 15 && inp.mouseX < gx + 15 && inp.mouseY > gy - 15 && inp.mouseY < gy + 15
  let _ ← cat.tex "Images/Black_Tile"
  let mut sp := #[]
  let mut i : Float32 := 16
  while i > 0 do
    let f := 1 - i / 16
    sp := sp.push (drawRect "Images/Black_Tile" ⟨gx - 16, gy + 16 + truncI i * -1, 32, 2⟩ (scaleColor ourFavoriteColor (f * 0.6)))
    i := i - 1
  sp := sp.push (drawRect "Images/Black_Tile" ⟨gx - 16, gy + 16, 32, 4⟩ ourFavoriteColor)
  let toggleTex := s!"Images/UI/Craft_Toggle_{(if visible then 4 else 2) + (if hover then 1 else 0)}"
  let (gw, gh) ← cat.tex toggleTex
  return sp.push (drawScaled toggleTex gw gh (single gx) (single gy) none white (natF gw / 2) (natF gh / 2) 1 1)

/-- One frame: the update pass, then the draw pass. `inventoryScale` / `inventoryBack`: `Main.inventoryScale`
(0.85, what `DrawInventory` leaves) and `Main.inventoryBack` when `DrawUI` runs. -/
def frame (cat : Catalog) (st : State) (inp : Input) (inventoryScale : Float32 := 0.85)
    (inventoryBack : Color := inventoryBackDefault) : Except String (State × Output) := do
  -- NewCraftingUI.UpdateUI / DrawUI do nothing while the window is closed
  if !st.visible then return (st, {})
  let (st, itemsHover, cleared) ← st.update cat inp
  -- Main.clrInput (a toggle in the update pass) dropped the frame's text events
  let typed := if cleared then #[] else inp.typed
  let mut st := st
  let mut out : Output := { mouseInterface := itemsHover }
  -- Draw: the window hovered → mouseInterface
  if st.hovering .state then out := { out with mouseInterface := true }
  -- UpdateCraftAreaSize (InGuideCraftMenu: 130)
  if st.gridHeightPx != -130 then
    st := { st with gridHeightPx := -130 }
    st ← st.recalc cat inp .grid
  -- UpdateText
  if cat.title != st.titleText then
    let (tw, _) ← cat.mouseText.measure cat.title
    st := { st with titleText := cat.title, titleW := tw, titleH := 16 }
    st ← st.recalc cat inp .title
    st := { st with searchWidthPx := -((st.dims.getD .title {}).dims.w) - 10 }
    st ← st.recalc cat inp .search
  st ← st.updateContents cat inp
  let mut sp : Array Sprite := #[]
  let d (e : El) := st.dims.getD e {}
  -- the main panel
  sp := sp ++ (← panelSprites cat (d .panel).dims panelBackground panelBorder)
  -- the title (UIText.DrawSelf)
  let ti := (d .title).inner
  sp := sp ++ (← shadowText cat.mouseText cat.title (ti.x + (ti.w - st.titleW) * 0.5) (ti.y - 2 * 1 + (ti.h - 16) * 0) white 1.5)
  -- the search box
  let (bw, bh) ← cat.tex "Images/UI/Bestiary/Button_Search_2"
  let sb := (d .searchButton).dims
  sp := sp.push (drawAt "Images/UI/Bestiary/Button_Search_2" bw bh sb.x sb.y white)
  if st.hovering .searchButton then
    let (ow, oh) ← cat.tex "Images/UI/Bestiary/Button_Search_Border"
    sp := sp.push (drawAt "Images/UI/Bestiary/Button_Search_Border" ow oh sb.x sb.y white)
  sp := sp ++ (← panelSprites cat (d .searchPanel).dims searchPanelColor (if st.writing then ourFavoriteColor else searchPanelColor))
  -- UISearchBar.DrawSelf: while taking text, Main.GetInputText; Enter or Escape ends the writing; SetContents(text)
  -- (a changed text resets the recipe lists here, mid-draw: the item grid below still shows its old entries)
  if st.writing then
    let (st', text, enter, escape, copied) ← st.getInputText typed inp.held st.actualContents
      (if cleared then some #[] else inp.textQueued) inp.clipboard
    out := { out with copied }
    st := st'
    if enter || escape then st := st.toggleTakingText
    st ← st.setContents cat inp (some text) false
  -- UITextBox.DrawSelf: the transparent panel, the text (Utils.DrawBorderString: shadow spread 1.5, scale 0.8; the
  -- hint in Color.Gray), the input ticker (`|` after the text, 21 of every 40 frames while writing)
  let tbd := st.dims.getD .textBox {}
  sp := sp ++ (← panelSprites cat tbd.dims transparent transparent)
  let tb := tbd.inner
  let textColor : Color := if st.shownWhite then white else ⟨128, 128, 128, 255⟩
  sp := sp ++ (← shadowText cat.mouseText st.shownText tb.x (tb.y - 2 * 0.8) textColor 1.5 0.8)
  if st.writing then
    let fc := (st.tickerFrame + 1) % 40
    st := { st with tickerFrame := fc }
    if fc ≤ 20 then
      let (tw, _) ← cat.mouseText.measure st.shownText
      let x := tb.x + tw * 0.8
      let x := x + (6 - 4 * 0.8)
      sp := sp ++ (← shadowText cat.mouseText "|" x (tb.y - 2 * 0.8) textColor 1.5 0.8)
  let (cw, ch) ← cat.tex "Images/UI/SearchCancel"
  let cb := (d .cancel).dims
  sp := sp.push (drawAt "Images/UI/SearchCancel" cw ch cb.x cb.y (scaleColor white (if st.hovering .cancel then 1 else 0.4)))
  -- the separator (Utils.DrawPanel, edge 0)
  let (pw, ph) ← cat.tex "Images/UI/CharCreation/Separator1"
  let se := (d .separator).dims
  let px := Float32.floor se.x
  let py := Float32.floor se.y
  sp := sp.push (drawAtSrc "Images/UI/CharCreation/Separator1" pw ph px py ⟨0, 0, 0, ph⟩ separatorColor)
  sp := sp.push (drawScaled "Images/UI/CharCreation/Separator1" pw ph px py (some ⟨0, 0, pw, ph⟩) separatorColor 0 0
    (se.w / natF pw) 1)
  sp := sp.push (drawAtSrc "Images/UI/CharCreation/Separator1" pw ph (px + se.w) py ⟨pw, 0, 0, ph⟩ separatorColor)
  -- the list: UIList.DrawSelf (inner list Top = -view, Recalculate), then the scissored children
  st := { st with innerListTop := -st.viewPosition }
  st ← st.recalc cat inp .list
  let clipStart := sp.size
  let mut hoveredEntry : Option Entry := none
  let dl := st.dims.getD .list {}
  let di := st.dims.getD .items {}
  -- Main.inventoryScale: the grid's DrawSelf (when the inner list draws it) sets 0.8461538 and leaves it
  let mut curScale := inventoryScale
  if aabb dl.dims di.dims then
    let gridScale : Float32 := 0.8461538553237915
    curScale := gridScale
    let (sx, sy, a, b) := st.gridParams
    for i in [a:b] do
      let e ← match st.entry st.contents[i]! with
        | some e => pure e
        | none => throw s!"item grid entry {st.contents[i]!} lost"
      let k : Int := i - a
      let per : Int := st.perLine
      let hx := sx + (k % per) * 44
      let hy := sy + (k / per) * 44
      let hov := st.hovering .items && (Rect.mk hx hy 44 44).contains inp.mouseX inp.mouseY
      out := { out with cells := out.cells.push (e.index, ⟨hx, hy, 44, 44⟩) }
      let selectedHere := st.selected == some e.index
      let r ← cat.recipe e.index
      let avail := e.availableIndex >= 0
      if hov then
        hoveredEntry := some e
        -- HandleCraftSlot(entry, 41)
        let flag := st.selected != some e.index
        if !avail || flag then
          if flag && ((inp.mouseLeft && inp.mouseLeftRelease) || (inp.mouseRight && inp.mouseRightRelease)) then
            st := { st with selected := some e.index }
            if avail then st := { st with focusRecipe := e.availableIndex }
            out := { out with recipeClicked := true, preventCrafting := some true }
          out := { out with hoverItem := some r.create }
        else
          out ← hoverOverCraftingItemButton cat st inp inp.guide out
      -- ItemSlot's mouse-item highlight/dim helpers have no branch for contexts
      -- 7, 41, 42 or 43: they return false / dim = 1 even with a held item.
      sp := sp ++ (← itemSlot cat 41 (some (r.create, r.createStack)) (single hx) (single hy)
        (if avail then white else disabledSlotColor) gridScale inventoryBack selectedHere)
  let clipEnd := sp.size
  -- the scrollbar (AutoHide: drawn only when it can scroll)
  if st.maxViewSize != st.viewSize then
    let sd := (d .scrollbar)
    let inner := sd.inner
    -- UIScrollbar.DrawSelf while dragging: the view follows the mouse (after the list drew: one frame behind)
    if st.dragging then
      let v := single inp.mouseY - inner.y - st.dragYOffset
      st := { st with viewPosition := clampF (v / inner.h * st.maxViewSize) 0 (st.maxViewSize - st.viewSize) }
    let handle := st.handleRect
    let hoverHandle := handle.contains inp.mouseX inp.mouseY
    st := { st with hoveringHandle := hoverHandle }
    let bar (tex : String) (r : Rect) (c : Color) : Except String (Array Sprite) := do
      let (tw, th) ← cat.tex tex
      return #[drawRectSrc tex tw th ⟨r.x, r.y - 6, r.w, 6⟩ ⟨0, 0, tw, 6⟩ c,
               drawRectSrc tex tw th ⟨r.x, r.y, r.w, r.h⟩ ⟨0, 6, tw, 4⟩ c,
               drawRectSrc tex tw th ⟨r.x, r.y + r.h, r.w, 6⟩ ⟨0, th - 6, tw, 6⟩ c]
    sp := sp ++ (← bar "Images/UI/Scrollbar2" sd.dims.rect white)
    sp := sp ++ (← bar "Images/UI/ScrollbarInner" handle (scaleColor white (if st.dragging || hoverHandle then 1 else 0.85)))
  -- the filter tabs
  let (fw, fh) ← cat.tex "Images/UI/Creative/Infinite_Tabs_B_2"
  let (iw, ih) ← cat.tex "Images/UI/Creative/Infinite_Icons"
  let iconFrames : Array Nat := #[0, 2, 8, 4, 7, 1, 9, 3, 6, 10, 5]
  for i in [0:filterCount] do
    let active := st.activeFilter == some i
    let fx : Nat := if active then 1 else 0
    let fy : Nat := (if active then 2 else 0) + (if st.hovering (.tab i) then 1 else 0)
    let tdim := (d (.tab i)).dims
    sp := sp.push (drawAtSrc "Images/UI/Creative/Infinite_Tabs_B_2" fw fh tdim.x tdim.y
      ⟨fx * (fw / 2), fy * (fh / 4), fw / 2 - 2, fh / 4 - 2⟩ white)
    let idim := (d (.icon i)).dims
    sp := sp.push (drawAtSrc "Images/UI/Creative/Infinite_Icons" iw ih idim.x idim.y
      ⟨iconFrames[i]! * (iw / 11), 0, iw / 11 - 2, ih⟩ (if active then white else scaleColor white 0.5))
  -- NewCraftingUI.DrawChildren: the guide slot, its line, the shown recipe, the grid toggle
  let base := (d .state).inner.rect
  let ox := single base.x + 24
  let oy := single base.y + 73
  let guideScale := curScale * 1
  let guideBack := scaleColor white 0.7490196228027344
  sp := sp ++ (← itemSlot cat 7 inp.guide ox (oy + 58) white guideScale guideBack false)
  let (bkw, bkh) ← cat.tex "Images/Inventory_Back"
  let guideHit : Rect := ⟨truncI ox, truncI (oy + 58), truncI (natF bkw * 1), truncI (natF bkh * 1)⟩
  out := { out with areas := out.areas.push ("guide", guideHit) }
  if guideHit.contains inp.mouseX inp.mouseY then
    -- ItemSlot.Handle(ref Main.guideItem, 7, true): after the swap, MouseHover shows the slot's item
    out := { out with guideSlotHandle := true, mouseInterface := true }
    if let some (t, _) := inp.guideAfter then out := { out with hoverItem := some t }
  let guideLine ← match inp.guideAfter with
    | none => pure cat.guideEmpty
    | some (t, _) => do pure (cat.guideShowing ++ " " ++ (← cat.item t).name)
  sp := sp ++ (← shadowText cat.mouseText guideLine (ox + 52) (oy + 73) white 1)
  let shown := match hoveredEntry with
    | some e => some e
    | none => match st.selected with
      | some sel => st.recipes.find? (·.index == sel)
      | none => none
  if let some e := shown then
    let r ← cat.recipe e.index
    sp := sp ++ (← shadowText cat.mouseText r.requirements (ox + 52) (oy + 36) white 1)
    let avail := e.availableIndex >= 0
    let cs := curScale * 1
    sp := sp ++ (← itemSlot cat 42 (some (r.create, r.createStack)) ox oy (if avail then white else disabledSlotColor) cs guideBack false)
    let createHit : Rect := ⟨truncI ox, truncI oy, truncI (natF bkw * 1), truncI (natF bkh * 1)⟩
    out := { out with areas := out.areas.push ("create", createHit) }
    if createHit.contains inp.mouseX inp.mouseY then
      -- HandleCraftSlot(entry, 42)
      let flag := st.selected != some e.index
      if (!avail || flag) && flag && ((inp.mouseLeft && inp.mouseLeftRelease) || (inp.mouseRight && inp.mouseRightRelease)) then
        st := { st with selected := some e.index }
        if avail then st := { st with focusRecipe := e.availableIndex }
        out := { out with recipeClicked := true, preventCrafting := some true }
      if !avail || flag then out := { out with hoverItem := some r.create }
      else out ← hoverOverCraftingItemButton cat st inp inp.guideAfter out
    let (kw, kh) ← cat.tex "Images/UI/Craft"
    sp := sp.push (drawScaled "Images/UI/Craft" kw kh (ox + 47) (oy + 13) none white 0 0 0.8 0.8)
    let n := r.materials.size
    let s : Float32 := min (11 / natF n) 1
    for i in [0:n] do
      let (t, k) := r.materials[i]!
      let mx := ox + 72 + natF (i * 34) * s
      let my := oy + 22 + (-16) * s
      let ms := curScale * (0.7 * s)
      sp := sp ++ (← itemSlot cat 43 (some (t, k)) mx my white ms guideBack false)
      let hit : Rect := ⟨truncI mx, truncI my, truncI (natF bkw * (0.7 * s)), truncI (natF bkh * (0.7 * s))⟩
      out := { out with areas := out.areas.push (s!"material {i}", hit) }
      if hit.contains inp.mouseX inp.mouseY then out := { out with hoverItem := some t }
  -- CraftingUI.DrawGridToggle(sb, 42, 285, 20030)
  let gx : Int := 42
  let gy : Int := 285
  let toggleHover := inp.mouseX > gx - 15 && inp.mouseX < gx + 15 && inp.mouseY > gy - 15 && inp.mouseY < gy + 15
  sp := sp ++ (← gridToggleSprites cat inp true)
  if toggleHover then
    out := { out with mouseInterface := true }
    -- a click: Main.PipsUseGrid = false, NewCraftingUI.ToggleInInventory → Close, Main.mouseLeftRelease = false
    if inp.mouseLeft && inp.mouseLeftRelease then
      st := st.close inp
      out := { out with gridClosed := true }
  -- the scissor of the grid's draws: UIList.GetClippingRectangle (UI scale matrix, the UI screen, the full scissor)
  let li := dl.inner
  let u := inp.uiScale
  let v0x := li.x * u
  let v0y := li.y * u
  let v1x := (li.x + li.w) * u
  let v1y := (li.y + li.h) * u
  let clampI (v lo hi : Int) : Int := if v < lo then lo else if v > hi then hi else v
  let sw := truncI (single (truncI (single inp.screenWidth / u)) * u)
  let sh := truncI (single (truncI (single inp.screenHeight / u)) * u)
  let rx := clampI (truncI v0x) 0 sw
  let ry := clampI (truncI v0y) 0 sh
  let rw := clampI (truncI (v1x - v0x)) 0 (sw - rx)
  let rh := clampI (truncI (v1y - v0y)) 0 (sh - ry)
  let (bw', bh') := (inp.screenWidth, inp.screenHeight)
  let left := clampI rx 0 bw'
  let top := clampI ry 0 bh'
  let right := clampI (rx + rw) 0 bw'
  let bottom := clampI (ry + rh) 0 bh'
  let named : List (String × El) :=
    (List.range filterCount).map (fun i => (s!"tab {i}", El.tab i)) ++
    [("search", .searchPanel), ("searchButton", .searchButton), ("cancel", .cancel), ("scrollbar", .scrollbar), ("window", .state)]
  out := { out with areas := out.areas ++ (named.map fun (ne : String × El) => (ne.1, (st.dims.getD ne.2 {}).dims.rect)).toArray
                    |>.push ("toggle", ⟨gx - 14, gy - 14, 29, 29⟩) }
  out := { out with sprites := sp, clip := ⟨left, top, right - left, bottom - top⟩, clipRange := (clipStart, clipEnd) }
  return (st, out)

end Lean4Terria.OriginalForestGuideCraftGridLayout
