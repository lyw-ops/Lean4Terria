import Lean4Terria.OriginalStorageRename

/-!
# The rename keyboard panel (`UIVirtualKeyboard`, context 2), as pure layout

Positions are in UI units: the screen is `OriginalScreenSize / UIScale` and
everything is drawn through `Main.UIScaleMatrix`. The mouse, in the same units,
is `Main.MouseScreen`.

The panel has two layers. Without gamepad hints (`SettingsForUI.ShowGamepadHints`
false, the mouse-and-keyboard desktop) `outerLayer2` shows only the text box,
the label and two large text buttons, Save and Cancel; the text box and label
sit 50 lower. With gamepad hints `outerLayer1` shows the 10 × 5 key grid and two
small Save / Cancel panels.

Layout follows `UIElement.Recalculate` exactly, in `Float32`. The only font
input is `DynamicSpriteFont.MeasureString(text).X` of `FontAssets.DeathText`
(`Fonts/Death_Text.xnb`), which the caller supplies; it sets the minimum width
of text panels (only the text box can outgrow its 516 units) and where text is
drawn inside them.

Original assets: `Images/UI/VK_Shift.xnb` (26 × 26), `Images/UI/VK_Backspace.xnb`
(54 × 26), `Images/UI/PanelBackground.xnb` and `PanelBorder.xnb` (28 × 28,
12-unit corners) for every panel, `Fonts/Death_Text.xnb` for all text, and the
sounds `Sounds/Menu_Tick.xnb` (12), `Menu_Open.xnb` (10), `Menu_Close.xnb` (11).
-/

namespace Lean4Terria.OriginalStorageRename.Keyboard

/-- `StyleDimension`: pixels plus a fraction of the parent's size. -/
structure Dim where
  pixels : Float32 := 0
  percent : Float32 := 0
  deriving Repr, Inhabited

def Dim.value (d : Dim) (size : Float32) : Float32 := d.pixels + d.percent * size

/-- `CalculatedStyle`. -/
structure Box where
  x : Float32
  y : Float32
  width : Float32
  height : Float32
  deriving Repr, Inhabited, BEq

def Box.center (b : Box) : Float32 × Float32 := (b.x + b.width * 0.5, b.y + b.height * 0.5)

/-- `CalculatedStyle.ToRectangle` and `Rectangle.Contains(Point)` with `(int)` truncation. -/
def Box.contains (b : Box) (point : Float32 × Float32) : Bool :=
  let px := point.1.toInt32.toInt
  let py := point.2.toInt32.toInt
  let x := b.x.toInt32.toInt
  let y := b.y.toInt32.toInt
  x ≤ px && px < x + b.width.toInt32.toInt && y ≤ py && py < y + b.height.toInt32.toInt

structure Style where
  left : Dim := {}
  top : Dim := {}
  width : Dim := {}
  height : Dim := {}
  minWidth : Dim := {}
  minHeight : Dim := {}
  maxWidth : Dim := ⟨0, 1⟩
  maxHeight : Dim := ⟨0, 1⟩
  hAlign : Float32 := 0
  vAlign : Float32 := 0
  paddingLeft : Float32 := 0
  paddingRight : Float32 := 0
  paddingTop : Float32 := 0
  paddingBottom : Float32 := 0
  deriving Repr, Inhabited

/-- `MathHelper.Clamp`: the maximum first, then the minimum. -/
private def clamp (v lo hi : Float32) : Float32 :=
  let v := if v > hi then hi else v
  if v < lo then lo else v

/-- `UIElement.GetDimensionsBasedOnParentDimensions` plus the padding step of
`Recalculate` (no element here has margins). Returns `(dimensions, inner)`. -/
def place (parent : Box) (s : Style) : Box × Box :=
  let x := s.left.value parent.width + parent.x
  let y := s.top.value parent.height + parent.y
  let w := clamp (s.width.value parent.width) (s.minWidth.value parent.width) (s.maxWidth.value parent.width)
  let h := clamp (s.height.value parent.height) (s.minHeight.value parent.height) (s.maxHeight.value parent.height)
  let x := x + (parent.width * s.hAlign - w * s.hAlign)
  let y := y + (parent.height * s.vAlign - h * s.vAlign)
  let dims : Box := ⟨x, y, w, h⟩
  (dims, ⟨x + s.paddingLeft, y + s.paddingTop, w - (s.paddingLeft + s.paddingRight),
    h - (s.paddingTop + s.paddingBottom)⟩)

/-! ## The element tree -/

/-- Every element of the panel. `key i` is `_keyList[i]` for the letter grid,
`i = row * 10 + column`. -/
inductive Part where
  | panel | textBox | label | gamepadLayer | desktopLayer | keyPanel
  | key (index : Nat)
  | shift | shiftIcon | symbols | space | backspace | backspaceIcon
  | save | cancel
  | saveText | cancelText
  deriving BEq, Repr, Inhabited

/-- The three on-screen key rows `SetKeyState` writes (`_keyState` 0, 2, 1). -/
def keyRows (keyState : Nat) : String :=
  match keyState with
  | 1 => "1234567890!@#$%^&*()-_+=/\\{}[]<>;:\"`|~£¥"
  | 2 => "1234567890QWERTYUIOPASDFGHJKL'ZXCVBNM,.?"
  | _ => "1234567890qwertyuiopasdfghjkl'zxcvbnm,.?"

def keyLabel (keyState index : Nat) : String :=
  ((keyRows keyState).toList[index]?.map toString).getD ""

/-- What the panel shows this frame. -/
structure View where
  /-- `PlayerInput.OriginalScreenSize`. -/
  screenWidth : Float32
  screenHeight : Float32
  /-- `Main.UIScale`. -/
  uiScale : Float32 := 1
  /-- `SettingsForUI.ShowGamepadHints`: the key grid instead of the desktop buttons. -/
  gamepadHints : Bool := false
  /-- The first drawn frame keeps the panel parked 9999 below (`OffsetDown`). -/
  parked : Bool := false
  /-- The text box text the last `Recalculate` saw. -/
  text : String := ""
  /-- `_keyState`. -/
  keyState : Nat := 0
  /-- The mouse is over the desktop Save / Cancel text (their scale grows to 0.85). -/
  saveHovered : Bool := false
  cancelHovered : Bool := false

/-- A laid-out element, its parent and its style. -/
structure Placed where
  part : Part
  parent : Option Nat
  dims : Box
  inner : Box
  style : Style
  deriving Inhabited

private def px (v : Float32) : Dim := ⟨v, 0⟩
private def pct (v : Float32) : Dim := ⟨0, v⟩

/-- `UITextPanel.SetText`: the text is `MeasureString(text).X * scale` by
`(large ? 32 : 16) * scale`, and padding is added for the minimum size. -/
private def textMin (s : Style) (measure : String → Float32) (text : String) (scale : Float32) : Style :=
  { s with
    minWidth := px (measure text * scale + s.paddingLeft + s.paddingRight)
    minHeight := px (32 * scale + s.paddingTop + s.paddingBottom) }

/-- `CreateKeyboardButton(text, x, y, width, style)` before `StyleKey`. -/
private def keyStyle (x y w : Nat) : Style :=
  { width := px (48 * w.toFloat.toFloat32 + 4 * (w - 1).toFloat.toFloat32)
    height := px 37
    left := ⟨52 * x.toFloat.toFloat32 - 516 * 0.5, 0.5⟩
    top := px (41 * y.toFloat.toFloat32)
    paddingLeft := 12, paddingRight := 12, paddingTop := 12, paddingBottom := 12 }

/-- The text box's style for a text (`UITextBox`, scale 0.78, large). `hAlign`
turns 1 when it would be wider than the screen. -/
def textBoxStyle (view : View) (measure : String → Float32) (hAlign : Float32) : Style :=
  let style : Style :=
    { hAlign, width := px 516, height := px 37
      top := px ((if view.gamepadHints then 0 else 50) + (-50 + 270 - 10))
      paddingLeft := 12, paddingRight := 12, paddingTop := 12, paddingBottom := 12 }
  textMin style measure view.text 0.78

/-- The element list in `Elements` order (children follow their parent's
append order), with styles as the constructor and `DrawSelf` leave them. -/
def styles (view : View) (measure : String → Float32) (textBoxHAlign : Float32) : Array (Part × Option Nat × Style) := Id.run do
  let mut out : Array (Part × Option Nat × Style) := #[]
  let top : Float32 := if view.parked then 9999 else 0
  out := out.push (.panel, none, { width := pct 1, height := pct 1, top := px top })
  out := out.push (.textBox, some 0, textBoxStyle view measure textBoxHAlign)
  let label : Style :=
    { hAlign := 0.5, width := px 516, height := px 37
      top := px ((if view.gamepadHints then 0 else 50) + (-50 - 37 - 4 + 270)) }
  out := out.push (.label, some 0, textMin label measure Data.enterNewName 0.75)
  let layer : Style := { width := px (516 + 8 + 16), top := px 270, height := px 266, hAlign := 0.5 }
  if view.gamepadHints then
    out := out.push (.gamepadLayer, some 0, layer)
    let layerIndex := out.size - 1
    let keyPanel : Style :=
      { width := pct 1, height := px 225
        paddingLeft := 12, paddingRight := 12, paddingTop := 12, paddingBottom := 12 }
    out := out.push (.keyPanel, some layerIndex, keyPanel)
    let panelIndex := out.size - 1
    for x in [:10] do
      for y in [:4] do
        let s := { keyStyle x y 1 with paddingLeft := 0, paddingRight := 0 }
        out := out.push (.key (y * 10 + x), some panelIndex, textMin s measure (keyLabel view.keyState (y * 10 + x)) 0.4)
    let shift := { keyStyle 0 4 1 with paddingLeft := 0, paddingRight := 0, paddingTop := 0, paddingBottom := 0 }
    out := out.push (.shift, some panelIndex, textMin shift measure "" 0.4)
    out := out.push (.shiftIcon, some (out.size - 1), { width := px 26, height := px 26, hAlign := 0.5, vAlign := 0.5 })
    let symbols := { keyStyle 1 4 1 with paddingLeft := 0, paddingRight := 0 }
    out := out.push (.symbols, some panelIndex, textMin symbols measure "@%" 0.4)
    let space := { keyStyle 2 4 6 with paddingLeft := 0, paddingRight := 0 }
    out := out.push (.space, some panelIndex, textMin space measure "Space" 0.4)
    let back := { keyStyle 8 4 2 with paddingLeft := 0, paddingRight := 0, paddingTop := 0, paddingBottom := 0 }
    out := out.push (.backspace, some panelIndex, textMin back measure "" 0.4)
    out := out.push (.backspaceIcon, some (out.size - 1), { width := px 54, height := px 26, hAlign := 0.5, vAlign := 0.5 })
    let save : Style := { height := px 37, width := pct 0.4, hAlign := 1, vAlign := 1, paddingTop := 12, paddingBottom := 12 }
    out := out.push (.save, some layerIndex, textMin save measure Data.uiSave 0.4)
    let cancelStyle : Style :=
      { height := px 37, width := pct 0.4, vAlign := 1
        paddingLeft := 12, paddingRight := 12, paddingTop := 12, paddingBottom := 12 }
    out := out.push (.cancel, some layerIndex, textMin cancelStyle measure Data.uiCancel 0.4)
  else
    out := out.push (.desktopLayer, some 0, layer)
    let layerIndex := out.size - 1
    let scale (hovered : Bool) : Float32 := if hovered then 0.85 else 0.72
    let saveText : Style :=
      { height := px 60, width := pct 0.4, hAlign := 0.5, top := px 50, paddingTop := 12, paddingBottom := 12 }
    out := out.push (.saveText, some layerIndex, textMin saveText measure Data.uiSave (scale view.saveHovered))
    let cancelText : Style :=
      { height := px 60, width := pct 0.4, hAlign := 0.5, top := px 114
        paddingLeft := 12, paddingRight := 12, paddingTop := 12, paddingBottom := 12 }
    out := out.push (.cancelText, some layerIndex, textMin cancelText measure Data.uiCancel (scale view.cancelHovered))
  return out

/-- The whole panel for one drawn frame. The screen is
`(0, 0, OriginalScreenSize / UIScale)`; the text box is right-aligned when it
is wider than the screen (`DrawSelf`). -/
def layout (view : View) (measure : String → Float32) : Array Placed := Id.run do
  let screen : Box := ⟨0, 0, view.screenWidth / view.uiScale, view.screenHeight / view.uiScale⟩
  let run (hAlign : Float32) : Array Placed := Id.run do
    let mut placed : Array Placed := #[]
    for (part, parent, style) in styles view measure hAlign do
      let parentBox := match parent with
        | some i => placed[i]!.inner
        | none => screen
      let (dims, inner) := place parentBox style
      placed := placed.push { part, parent, dims, inner, style }
    return placed
  let first := run 0.5
  let tb := first[1]!
  if tb.dims.width > first[0]!.inner.width then run 1 else first

/-- `UIElement.GetElementAt`: the last child containing the point, recursively. -/
def elementAt (placed : Array Placed) (point : Float32 × Float32) : Option Nat := Id.run do
  if placed.isEmpty || !placed[0]!.dims.contains point then return none
  let mut current := 0
  let mut descending := true
  while descending do
    let children := (List.range placed.size).filter (fun i => placed[i]!.parent == some current)
    match children.reverse.find? (fun i => placed[i]!.dims.contains point) with
    | some child => current := child
    | none => descending := false
  return some current

/-- Clicks bubble from the element under the mouse to the first ancestor that
handles them (icons belong to their buttons; panels and text do nothing). -/
def clickTarget (placed : Array Placed) (index : Nat) : Option Part := Id.run do
  let mut i := index
  for _ in [:placed.size] do
    match placed[i]!.part with
    | .key _ | .shift | .symbols | .space | .backspace | .save | .cancel | .saveText | .cancelText =>
      return some placed[i]!.part
    | _ => match placed[i]!.parent with
      | some p => i := p
      | none => return none
  return none

/-- Apply a left click that was pressed and released on `part`. -/
def click (state : State) (keyState : Nat) (part : Part) (playerChest : Int) (currentName : String) :
    State × Array Effect :=
  match part with
  | .key i => pressKey state (keyLabel keyState i)
  | .shift => toggleShift state
  | .symbols => toggleSymbols state
  | .space => pressSpace state
  | .backspace => pressBackspace state
  | .save | .saveText => clickSave state playerChest currentName
  | .cancel | .cancelText => clickCancel state
  | _ => (state, #[])

/-- `UITextPanel.TextDrawPosition` for scale `s` (large text): centred
horizontally, raised by `10 · s · s`. -/
def textPosition (inner : Box) (textWidth scale : Float32) : Float32 × Float32 :=
  (inner.x + (inner.width - textWidth) * 0.5, inner.y - 10 * scale * scale)

/-- The text box caret `|` (`UITextBox.DrawSelf`), shown on 21 of every 40
drawn frames: after the text, `6 − 8 · 0.78` to the right, `2 · 0.78` lower. -/
def caretPosition (inner : Box) (textWidth caretOffsetWidth : Float32) : Float32 × Float32 :=
  let (x, y) := textPosition inner textWidth 0.78
  (x + caretOffsetWidth * 0.78 + (6 - 8 * 0.78), y + 2 * 0.78)

def caretVisible (frameCount : Nat) : Bool := frameCount % 40 ≤ 20

/-! ## Gamepad -/

/-- `SetupGamepadPoints`: link point 3000 is Cancel, 3001 Save, `3002 + i` the
centre of `_keyList[i]` (rows 0–4, columns 0–9; a wide key repeats). Each
entry is `(id, part, up, down, left, right)`; −1 means none. -/
def linkPoints : Array (Nat × Part × Int × Int × Int × Int) := Id.run do
  let owner (i : Nat) : Part :=
    if i < 40 then .key i
    else match i % 10 with
      | 0 => .shift
      | 1 => .symbols
      | 8 | 9 => .backspace
      | _ => .space
  let mut out := #[(3000, Part.cancel, (3002 + 40 : Int), (-1 : Int), (-1 : Int), (3001 : Int)),
    (3001, Part.save, 3002 + 49, -1, 3000, -1)]
  for row in [:5] do
    for col in [:10] do
      let i := row * 10 + col
      let me := owner i
      let leftSame := ((List.range col).reverse.find? (fun j => owner (row * 10 + j) != me))
      let left : Int := match leftSame with
        | some j => 3002 + row * 10 + j
        | none => 3002 + row * 10 + 9
      let rightNext := ((List.range 10).filter (· > col)).find? (fun j => owner (row * 10 + j) != me)
      let right : Int := match rightNext with
        | some j => 3002 + row * 10 + j
        | none => 3002 + row * 10
      let up : Int := if row == 0 then -1 else 3002 + i - 10
      let down : Int := if row != 4 then 3002 + i + 10 else if col < 5 then 3000 else 3001
      out := out.push (3002 + i, me, up, down, left, right)
  return out

/-- Gamepad UI triggers while the keyboard is open (`UILinksInitializer`,
default profile preset 1 "Redigit's Pick", `InputMode.XBoxGamepadUI`). -/
inductive GamepadAction where
  /-- `MouseLeft` (A): click the element at the current link point. -/
  | press
  /-- `MouseRight` (LB): `UIVirtualKeyboard.BackSpace`, hint `Lang.menu[236]` "Backspace". -/
  | backspace
  /-- `SmartCursor` (RB): `Write(" ")`, hint `Lang.menu[238]` "Space". -/
  | space
  /-- `HotbarMinus` (LT): `CycleSymbols`, hint `Lang.menu[235]` "abc / ABC / !@#". -/
  | cycleSymbols
  /-- `HotbarPlus` (RT): `Submit` when `CanSubmit`, hint `Lang.menu[237]` "Submit". -/
  | submit
  /-- `Inventory` (B or Y): `FancyExit` with `BackButtonCommand` 6, `UIVirtualKeyboard.Cancel`. -/
  | back
  deriving BEq, Repr

/-- `CycleSymbols`: lower → shift → symbols → lower (states 0 → 2 → 1 → 0). No sound. -/
def cycleSymbols (state : State) : State :=
  match state.keyboard with
  | none => state
  | some kb => { state with keyboard := some { kb with keyState := match kb.keyState with
      | 0 => 2 | 2 => 1 | _ => 0 } }

def gamepad (state : State) (action : GamepadAction) (playerChest : Int) (currentName : String) :
    State × Array Effect :=
  match action with
  | .press => (state, #[])
  | .backspace => pressBackspace state
  | .space => pressSpace state
  | .cycleSymbols => (cycleSymbols state, #[])
  | .submit => clickSave state playerChest currentName
  | .back => clickCancel state

/-! ## Colours (XNA `Color`, `× f` scales every channel) -/

/-- Panel backgrounds: keys and buttons (63, 82, 151) × 0.7; hovered key
(73, 94, 171) × 0.85; hovered Save/Cancel panel (73, 94, 171); active Shift or
@% (93, 114, 191); the key area (23, 33, 69) × 0.7. Borders: keys
(89, 116, 213) × 0.7, hovered `Main.OurFavoriteColor` × 0.85, hovered
Save/Cancel `Colors.FancyUIFatButtonMouseOver`, otherwise black. Desktop
Save/Cancel text is silver at 0.72 and white at 0.85 while hovered. -/
def keyBackground : Nat × Nat × Nat × Float32 := (63, 82, 151, 0.7)

end Lean4Terria.OriginalStorageRename.Keyboard
