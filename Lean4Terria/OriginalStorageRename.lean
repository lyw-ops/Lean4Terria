import Lean4Terria.OriginalStorageRename.Data

/-!
# Renaming world chests and dressers (Terraria 1.4.5.8 desktop)

Chest button 5 (`ChestUI.RenameChest`) opens `UIVirtualKeyboard` in context 2
(`IngameFancyUI.OpenVirtualKeyboard 2`, max length 20). While it is open,
`UIVirtualKeyboard.DrawSelf` reads the keyboard once per drawn frame through
`Main.GetInputText`, trims to 20 UTF-16 code units with `Utils.TrimUserString`
and copies the text to `Main.npcChatText`. Save stores `npcChatText` in
`Chest.name` (`ChestUI.RenameChestSubmit`); typing the default title clears the
name. Personal storage (player.chest < −1) has no Rename button.

The text is kept as UTF-16 code units, like the original string. Emoji and
other characters outside the BMP arrive from the key listener as two queued
units; the 20-unit limit never keeps a trailing high surrogate.

This module is pure: callers pass the frame's keyboard state and receive the
new state plus effects (sounds, chest name writes, clipboard writes, keyboard
closing).
-/

namespace Lean4Terria.OriginalStorageRename

/-! ## UTF-16 text -/

abbrev Utf16 := Array UInt16

def utf16 (text : String) : Utf16 := Id.run do
  let mut out := #[]
  for c in text.toList do
    let v := c.toNat
    if v < 0x10000 then out := out.push v.toUInt16
    else
      let w := v - 0x10000
      out := (out.push (0xD800 + w / 0x400).toUInt16).push (0xDC00 + w % 0x400).toUInt16
  return out

private def isHigh (u : UInt16) : Bool := 0xD800 ≤ u && u < 0xDC00
private def isLow (u : UInt16) : Bool := 0xDC00 ≤ u && u < 0xE000

/-- Decode UTF-16. An unpaired surrogate becomes U+FFFD. The original keeps
it in memory, but `WorldFile.SaveChests` then throws `EncoderFallbackException`
and the world cannot be saved, so a Lean name never holds one. -/
def ofUtf16 (units : Utf16) : String := Id.run do
  let mut out := ""
  let mut i := 0
  while i < units.size do
    let u := units[i]!
    if isHigh u && i + 1 < units.size && isLow units[i + 1]! then
      let v := 0x10000 + (u.toNat - 0xD800) * 0x400 + (units[i + 1]!.toNat - 0xDC00)
      out := out.push (Char.ofNat v)
      i := i + 2
    else
      out := out.push (if isHigh u || isLow u then '�' else Char.ofNat u.toNat)
      i := i + 1
  return out

/-- `Utils.TrimUserString`: cut to `limit` units, one fewer when that would end
on a high surrogate. -/
def trimUserString (text : Utf16) (limit : Nat) : Utf16 :=
  if text.size ≤ limit then text
  else
    let limit := if limit > 0 && isHigh text[limit - 1]! then limit - 1 else limit
    text.extract 0 limit

/-- `Utils.TrimLastCharacter`, defined for non-empty text. -/
def trimLastCharacter (text : Utf16) : Utf16 := trimUserString text (text.size - 1)

/-- The chest keyboard's `UITextBox._maxLength`. -/
def maxLength : Nat := 20

/-! ## Default titles -/

/-- The container a world chest record points at, by its top-left tile. -/
inductive ContainerTile where
  /-- Tile 21 with the top-left `frameX`. -/
  | chest (frameX : Nat)
  /-- Tile 467. -/
  | chest2 (frameX : Nat)
  /-- Tile 88. -/
  | dresser (frameX : Nat)
  deriving BEq, Repr

/-- `Main.defaultChestName` as `OpenVirtualKeyboard` and `ChestUI.DrawName`
compute it: `Lang.chestType[frameX / 36]`, item 3988 for tile 467 style 4,
`Lang.chestType2[frameX / 36]`, or `Lang.dresserType[frameX / 54]`. -/
def defaultName : ContainerTile → String
  | .chest frameX => Data.chestType[frameX / 36]?.getD ""
  | .chest2 frameX => if frameX / 36 == 4 then Data.deadMansChest else Data.chestType2[frameX / 36]?.getD ""
  | .dresser frameX => Data.dresserType[frameX / 54]?.getD ""

def ContainerTile.ofTile (typeId frameX : Nat) : Option ContainerTile :=
  match typeId with
  | 21 => some (.chest frameX)
  | 467 => some (.chest2 frameX)
  | 88 => some (.dresser frameX)
  | _ => none

/-! ## Chat tags (Backspace deletes a whole item or glyph tag) -/

private def asciiLetter (u : UInt16) : Bool := (65 ≤ u && u ≤ 90) || (97 ≤ u && u ≤ 122)
private def lower (u : UInt16) : UInt16 := if 65 ≤ u && u ≤ 90 then u + 32 else u

/-- One match of `ChatManager.Regexes.Format`,
`(?<!\\)\[(?<tag>[a-zA-Z]{1,10})(\/(?<options>[^:]+))?:(?<text>.+?)(?<!\\)\]`
(Singleline), starting at `start`. Returns the tag, text and end index. -/
private def matchTagAt (s : Utf16) (start : Nat) : Option (Utf16 × Utf16 × Nat) := do
  guard (s[start]? == some 91)
  guard (start == 0 || s[start - 1]! != 92)
  let mut i := start + 1
  while i < s.size && asciiLetter s[i]! && i - start - 1 < 10 do i := i + 1
  let tagLength := i - start - 1
  guard (1 ≤ tagLength)
  let tag := s.extract (start + 1) i
  if s[i]? == some 47 then
    let optStart := i + 1
    let mut j := optStart
    while j < s.size && s[j]! != 58 do j := j + 1
    guard (j > optStart)
    i := j
  guard (s[i]? == some 58)
  let textStart := i + 1
  let mut k := textStart + 1
  let mut found := none
  while k < s.size && found.isNone do
    if s[k]! == 93 && s[k - 1]! != 92 then found := some k
    k := k + 1
  let close ← found
  return (tag, s.extract textStart close, close + 1)

/-- `Regex.Matches` over the whole text, left to right, without overlap. -/
private def tagMatches (s : Utf16) : Array (Nat × Utf16 × Utf16 × Nat) := Id.run do
  let mut out := #[]
  let mut i := 0
  while i < s.size do
    match matchTagAt s i with
    | some (tag, text, stop) =>
      out := out.push (i, tag, text, stop)
      i := stop
    | none => i := i + 1
  return out

/-- `String.Replace("\\]", "]")` on the tag text. -/
private def unescapeBracket (s : Utf16) : Utf16 := Id.run do
  let mut out := #[]
  let mut i := 0
  while i < s.size do
    if s[i]! == 92 && s[i + 1]? == some 93 then
      out := out.push 93
      i := i + 2
    else
      out := out.push s[i]!
      i := i + 1
  return out

/-- `Int32.TryParse(string)` with `NumberStyles.Integer`: white space 9–13 or
32 around, one optional sign, ASCII digits, trailing NULs allowed. -/
def tryParseInt32 (s : Utf16) : Option Int := Id.run do
  let white (u : UInt16) := u == 32 || (9 ≤ u && u ≤ 13)
  let mut i := 0
  while i < s.size && white s[i]! do i := i + 1
  let mut negative := false
  if i < s.size && (s[i]! == 43 || s[i]! == 45) then
    negative := s[i]! == 45
    i := i + 1
  let digitsStart := i
  let mut value : Nat := 0
  while i < s.size && 48 ≤ s[i]! && s[i]! ≤ 57 do
    value := value * 10 + (s[i]! - 48).toNat
    i := i + 1
  if i == digitsStart then return none
  while i < s.size && white s[i]! do i := i + 1
  while i < s.size && s[i]! == 0 do i := i + 1
  if i != s.size then return none
  let v : Int := if negative then -(value : Int) else value
  if v < -2147483648 || v > 2147483647 then return none
  return some v

/-- Whether `new Item().SetDefaults(n)` ends with a type above 0. -/
def itemTagResolves (n : Int) : Bool :=
  (-48 ≤ n && n ≤ -1) ||
    (1 ≤ n && n < Data.itemCount && !Data.itemsWithoutType.contains n.toNat)

/-- `TextSnippet.DeleteWhole` of a tag snippet: `ItemSnippet` for a resolving
`[i:…]` / `[item:…]`, `GlyphSnippet` for `[g:…]`, `[glyph:…]`, `[gx:…]`,
`[gp:…]`, `[gn:…]` below 26. Tags are matched case-insensitively. -/
private def deletesWhole (tag text : Utf16) : Bool :=
  let tag := tag.map lower
  let isTag (name : String) := tag == utf16 name
  let value := tryParseInt32 (unescapeBracket text)
  if isTag "i" || isTag "item" then value.any itemTagResolves
  else if isTag "g" || isTag "glyph" || isTag "gx" || isTag "gp" || isTag "gn" then value.any (· < 26)
  else false

/-- `ChatManager.ParseMessage` seen from the end: the snippet count and the
last snippet's `TextOriginal`. Plain runs between tags are snippets; a tag
with a registered handler (c, color, i, item, n, name, a, achievement, g,
glyph, gx, gp, gn) keeps the whole match as `TextOriginal`, an unknown tag
becomes a plain snippet of its text. -/
def parseSummary (text : Utf16) : Nat × Utf16 := Id.run do
  let s := text.filter (· != 13)
  let known := ["c", "color", "i", "item", "n", "name", "a", "achievement", "g", "glyph", "gx", "gp", "gn"]
  let mut count := 0
  let mut last : Utf16 := #[]
  let mut pos := 0
  for (start, tag, body, stop) in tagMatches s do
    if start > pos then
      count := count + 1
      last := s.extract pos start
    count := count + 1
    last := if known.any (fun k => tag.map lower == utf16 k) then s.extract start stop else unescapeBracket body
    pos := stop
  if s.size > pos then
    count := count + 1
    last := s.extract pos s.size
  return (count, last)

/-- What one desktop Backspace removes: the last chat snippet when it is a
whole-delete tag ending the text (after `ParseMessage` drops `\r`), otherwise
one character, keeping surrogate pairs together. -/
def backspace (text : Utf16) : Utf16 :=
  let parsed := text.filter (· != 13)
  let last := (tagMatches parsed).back?
  match last with
  | some (start, tag, body, stop) =>
    if stop == parsed.size && deletesWhole tag body then text.extract 0 (text.size - (stop - start))
    else trimLastCharacter text
  | none => trimLastCharacter text

/-! ## Main.GetInputText -/

/-- The keys `GetInputText` looks at in a `KeyboardState`. Control, Alt and
Shift mean either side (Keys 160–165); the Command key is not Control. -/
structure Keys where
  control : Bool := false
  alt : Bool := false
  shift : Bool := false
  z : Bool := false
  x : Bool := false
  c : Bool := false
  v : Bool := false
  insert : Bool := false
  delete : Bool := false
  escape : Bool := false
  back : Bool := false
  deriving BEq, Repr, Inhabited

/-- Statics `GetInputText` carries between calls: `Main.inputText` and
`oldInputText`, `backSpaceRate`, `backSpaceCount`, `imeCompositionActive`. -/
structure TextInput where
  last : Keys := {}
  beforeLast : Keys := {}
  backSpaceRate : Float32 := 0
  backSpaceCount : Int := 0
  imeCompositionActive : Bool := false
  deriving Inhabited

/-- One frame of desktop input. `queued` holds `Main.keyInt` for the
characters the IME key listener queued since the last call, one UTF-16 unit
each, at most 100 per frame. `keys` is `Keyboard.GetState()` now,
`clipboard` the platform clipboard's single-line `Value`, `imeComposing`
whether the IME composition string is non-empty after this frame. -/
structure Frame where
  keys : Keys := {}
  queued : Array UInt16 := #[]
  clipboard : Utf16 := #[]
  imeComposing : Bool := false
  windows : Bool := false
  deriving Inhabited

structure InputResult where
  text : Utf16
  enter : Bool := false
  escape : Bool := false
  /-- What the frame wrote to the clipboard (copy or cut). -/
  copied : Option Utf16 := none

/-- `Math.Round(double)`: halves go to the even neighbour. -/
private def roundHalfEven (x : Float) : Int :=
  let f := x.floor
  let d := x - f
  let n : Int := if f < 0 then -((-f).toUInt64.toNat : Int) else (f.toUInt64.toNat : Int)
  if d < 0.5 then n else if d > 0.5 then n + 1 else if n % 2 == 0 then n else n + 1

/-- `Main.GetInputText(text, allowMultiLine := false)` on a focused desktop window. -/
def getInputText (state : TextInput) (text : Utf16) (frame : Frame) : TextInput × InputResult := Id.run do
  let newly (pick : Keys → Bool) := pick state.last && !pick state.beforeLast
  let mut s := text
  let mut add : Utf16 := #[]
  let mut enter := false
  let mut escape := false
  let mut copied : Option Utf16 := none
  if state.last.control && !state.last.alt then
    if newly (·.z) then s := #[]
    else if newly (·.x) then
      copied := some text
      s := #[]
    else if newly (·.c) || newly (·.insert) then copied := some text
    else if newly (·.v) then add := add ++ frame.clipboard
  else
    if state.last.shift then
      if newly (·.delete) then
        copied := some text
        s := #[]
      if newly (·.insert) then add := add ++ frame.clipboard
    if !frame.windows && newly (·.escape) then escape := true
    for code in frame.queued.extract 0 100 do
      if code == 13 then enter := true
      else if code == 27 then escape := true
      else if code ≥ 32 && code != 127 then add := add.push code
  s := s ++ add
  let previous := state.last
  let now := frame.keys
  let mut rate := state.backSpaceRate
  let mut count := state.backSpaceCount
  let mut repeatNow := false
  if now.back && previous.back && !state.imeCompositionActive then
    rate := rate - 0.05
    if rate < 0 then rate := 0
    if count ≤ 0 then
      count := roundHalfEven rate.toFloat
      repeatNow := true
    count := count - 1
  else
    rate := 7
    count := 15
  let pressed := now.back && !previous.back && !state.imeCompositionActive
  if (pressed || repeatNow) && s.size > 0 then s := backspace s
  let next : TextInput := {
    last := now, beforeLast := previous, backSpaceRate := rate, backSpaceCount := count,
    imeCompositionActive := frame.imeComposing }
  return (next, { text := s, enter, escape, copied })

/-! ## Rename state -/

/-- The open `UIVirtualKeyboard` (context 2). -/
structure Keyboard where
  /-- `UITextBox.Text`, at most 20 units. -/
  text : Utf16
  /-- `_keyState`: 0 lowercase, 1 symbols, 2 shift. The on-screen keys start
  shifted when the text is empty. -/
  keyState : Nat := 0
  /-- The constructor parks the panel at `OffsetDown` 9999. The first
  `DrawSelf` keeps it there and hides the text box; it slides into place
  on the second drawn frame. -/
  drawnFrames : Nat := 0
  deriving BEq, Repr, Inhabited

/-- `Main.editChest`, `Main.npcChatText`, `Main.defaultChestName`,
`Main.blockKey`, the keyboard in `Main.InGameUI` and the text input statics. -/
structure State where
  editChest : Bool := false
  npcChatText : Utf16 := #[]
  defaultChestName : String := ""
  blockKey : String := ""
  keyboard : Option Keyboard := none
  input : TextInput := {}
  deriving Inhabited

/-- `SoundEngine.PlaySound(id)` legacy ids: 10 `MenuOpen`, 11 `MenuClose`,
12 `MenuTick` (`Sounds/Menu_Open`, `Menu_Close`, `Menu_Tick`). -/
inductive Effect where
  | sound (id : Nat)
  /-- `Main.chest[index].name := name`. -/
  | setName (chest : Nat) (name : String)
  | clipboard (text : String)
  /-- `IngameFancyUI.Close`: the keyboard leaves and the inventory is shown again. -/
  | closeKeyboard
  deriving BEq, Repr

/-- `ChestUI.RenameChestSubmit(player)`. -/
def submit (state : State) (playerChest : Int) (currentName : String) : State × Array Effect := Id.run do
  let state := { state with editChest := false }
  let fx := #[Effect.sound 12]
  if playerChest < 0 then return (state, fx)
  let text := if state.npcChatText == utf16 state.defaultChestName then #[] else state.npcChatText
  let state := { state with npcChatText := text }
  if utf16 currentName != text then
    return (state, fx.push (.setName playerChest.toNat (ofUtf16 text)))
  return (state, fx)

/-- `ChestUI.RenameChestCancel()`. -/
def cancel (state : State) : State × Array Effect :=
  ({ state with editChest := false, npcChatText := #[], blockKey := "Escape" }, #[.sound 12])

private def close (state : State) (quiet : Bool) : State × Array Effect :=
  ({ state with keyboard := none }, (if quiet then #[] else #[Effect.sound 11]).push .closeKeyboard)

/-- `IngameFancyUI.OpenVirtualKeyboard(2)` for the open chest: the old name,
or the default title when it is empty, becomes `npcChatText`; the keyboard
starts with it trimmed to 20 units. -/
def openKeyboard (state : State) (currentName : String) (tile : ContainerTile) : State × Array Effect :=
  let title := defaultName tile
  let text := if currentName.isEmpty then utf16 title else utf16 currentName
  let boxText := trimUserString text maxLength
  ({ state with
      npcChatText := text, defaultChestName := title, editChest := true
      keyboard := some { text := boxText, keyState := if boxText.isEmpty then 2 else 0 } },
    #[.sound 12])

/-- Chest button 5, `ChestUI.RenameChest`: open the keyboard, or save while
already editing. Hidden for personal storage. -/
def renameButton (state : State) (playerChest : Int) (currentName : String) (tile : ContainerTile) :
    State × Array Effect :=
  if playerChest < -1 then (state, #[])
  else if state.editChest then submit state playerChest currentName
  else openKeyboard state currentName tile

/-- Chest button 6, `ChestUI.RenameChestCancel`, shown only while editing. -/
def cancelButton (state : State) : State × Array Effect :=
  if state.editChest then cancel state else (state, #[])

/-- The keyboard's Save button (`_submitButton2` on desktop, `_submitButton`
with gamepad hints, or the gamepad Submit). Chest text is always valid. The
submit event receives the trimmed text, but the chest handler reads
`npcChatText`, so spaces are kept. -/
def clickSave (state : State) (playerChest : Int) (currentName : String) : State × Array Effect :=
  if state.keyboard.isNone then (state, #[]) else
  let (state, fx) := submit state playerChest currentName
  let (state, fx2) := close state true
  (state, #[Effect.sound 10] ++ fx ++ fx2)

/-- The keyboard's Cancel button. -/
def clickCancel (state : State) : State × Array Effect :=
  if state.keyboard.isNone then (state, #[]) else
  let (state, fx) := cancel state
  let (state, fx2) := close state true
  (state, #[Effect.sound 11] ++ fx ++ fx2)

private def editText (state : State) (f : Utf16 → Utf16) : State :=
  match state.keyboard with
  | none => state
  | some kb =>
    let text := trimUserString (f kb.text) maxLength
    let keyState := if kb.text.isEmpty && !text.isEmpty && kb.keyState == 2 then 0 else kb.keyState
    { state with keyboard := some { kb with text, keyState } }

/-- An on-screen key (`TypeText` / `Write`, gamepad hints only): insert at the
end, where the text box cursor sits after every draw, then trim. The first
character typed turns Shift off. -/
def pressKey (state : State) (key : String) : State × Array Effect :=
  if state.keyboard.isNone then (state, #[]) else (editText state (· ++ utf16 key), #[.sound 12])

/-- `UIVirtualKeyboard.set_Text` followed by the next draw's
`CopyTextToChest`: replace the whole text (trimmed to 20 units). -/
def replaceText (state : State) (text : String) : State :=
  let state := editText state (fun _ => utf16 text)
  match state.keyboard with
  | some kb => { state with npcChatText := kb.text }
  | none => state

/-- The on-screen space bar (`PressSpace`). -/
def pressSpace (state : State) : State × Array Effect := pressKey state " "

/-- The on-screen backspace (`UITextBox.Backspace`): one character, no tag rule. -/
def pressBackspace (state : State) : State × Array Effect :=
  match state.keyboard with
  | none => (state, #[])
  | some kb =>
    ({ state with keyboard := some { kb with text := if kb.text.isEmpty then kb.text else trimLastCharacter kb.text } },
      #[.sound 12])

/-- Shift and @% (`<.ctor>b__49_2` / `b__49_5`, `CycleSymbols` on gamepad). -/
def toggleShift (state : State) : State × Array Effect :=
  match state.keyboard with
  | none => (state, #[])
  | some kb => ({ state with keyboard := some { kb with keyState := if kb.keyState == 2 then 0 else 2 } }, #[.sound 12])

def toggleSymbols (state : State) : State × Array Effect :=
  match state.keyboard with
  | none => (state, #[])
  | some kb => ({ state with keyboard := some { kb with keyState := if kb.keyState == 1 then 0 else 1 } }, #[.sound 12])

/-- One drawn frame of the keyboard, `UIVirtualKeyboard.DrawSelf`.
`playerChest` is the local player's open container at draw time;
`currentName` that chest's name. -/
def drawFrame (state : State) (frame : Frame) (playerChest : Int) (currentName : String) :
    State × Array Effect := Id.run do
  let some kb := state.keyboard | return (state, #[])
  let kb := { kb with drawnFrames := kb.drawnFrames + 1 }
  let state := { state with keyboard := some kb }
  if !state.editChest then return close state false
  let (input, r) := getInputText state.input kb.text frame
  let mut state := { state with input }
  let mut fx : Array Effect := match r.copied with
    | some text => #[.clipboard (ofUtf16 text)]
    | none => #[]
  if r.enter then
    let (s, e) := submit state playerChest currentName
    let (s, e2) := close s false
    return (s, fx ++ e ++ e2)
  if playerChest < 0 then
    let (s, e) := cancel state
    state := s
    fx := fx ++ e
  else if r.escape then
    let (s, e) := cancel state
    let (s, e2) := close s true
    return (s, fx.push (.sound 11) ++ e ++ e2)
  let text := if r.text != kb.text then trimUserString r.text maxLength else kb.text
  state := { state with keyboard := some { kb with text }, npcChatText := text }
  return (state, fx)

/-- `Player.Update` after `HandleBeingInChestRange`: a closed chest ends the
edit; the keyboard closes on the next drawn frame. -/
def afterPlayerUpdate (state : State) (playerChest : Int) : State :=
  if state.editChest && playerChest == -1 then { state with editChest := false } else state

/-- `Player.TileInteractionsUse` on a chest or dresser (the same one or
another): chat text cleared, an edit in progress ends with a tick. -/
def interactWithContainer (state : State) : State × Array Effect :=
  let state := { state with npcChatText := #[] }
  if state.editChest then ({ state with editChest := false }, #[.sound 12]) else (state, #[])

/-- `IngameOptions.Open`. -/
def openOptions (state : State) : State :=
  { state with editChest := false, npcChatText := #[] }

/-- The title `ChestUI.DrawName` shows: the edited text while renaming, the
chest name or its default title, or a personal storage name. -/
def title (state : State) (playerChest : Int) (currentName : String) (tile : Option ContainerTile) : String :=
  if state.editChest then ofUtf16 state.npcChatText
  else if playerChest > -1 then
    if !currentName.isEmpty then currentName else (tile.map defaultName).getD ""
  else match playerChest with
    | -2 => Data.piggyBank
    | -3 => Data.safe
    | -4 => Data.defendersForge
    | -5 => Data.voidVault
    | _ => ""

end Lean4Terria.OriginalStorageRename
