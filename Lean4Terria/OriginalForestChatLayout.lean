import Std.Data.HashMap

/-!
# NPC chat panel layout (Terraria 1.4.5.8 desktop): the Guide and the Old Man

A hand transcription of what the live chat panel draws and which button the mouse hovers or clicks, sprite for sprite:
`Main.GUIChatDrawInner` → `NPCChatPanel.Draw` (`PrepareText` → `TextDisplayCache.PrepareCache` → `Utils.WordwrapString`
→ ReLogic `DynamicSpriteFont.CreateWrappedText` / `WrappedTextBuilder`; `PrepareInteractions`; the two `Chat_Back`
slices; `DrawText` → `ChatManager.DrawColorCodedStringWithShadow` → `DynamicSpriteFont.InternalDrawFast`;
`Main.DrawNPCPortrait`; `Main.DrawNPCChatBottomRightItem`; `DrawButtons` → `ChatManager.GetStringSize`,
`DrawColorCodedStringShadow`, `DrawColorCodedString`, `Utils.DrawNotificationIcon`), down to the parameters FNA's
`SpriteBatch.Draw` overloads hand to `PushSprite` (`Sprite`). Checked against the vanilla assemblies run headless in a
private host (`.lake/original-forest-chat-layout/1.4.5.8`).

## Coordinates and UI scale

The panel is drawn in the "Vanilla: NPC / Sign Dialog" interface layer, which uses UI scale: before it,
`PlayerInput.SetZoom_UI` sets `Main.screenWidth / screenHeight / mouseX / mouseY` to the backbuffer values times
`1f / Main.UIScale` (truncated), and the `SpriteBatch` is begun with `Main.UIScaleMatrix` = scale(UIScale). Every
`Sprite` here is in those UI units; on screen a point is multiplied by `UIScale`. `Main.UIScale`'s setter clamps the
wanted value (`config.json` "UIScale", 1 by default) to `UIScaleMax = max(1, min(width / 800, height / 600))`
(`uiScaleUsed`).

## Fonts

`FontAssets.MouseText` (`Fonts/Mouse_Text`, line spacing 29) for the text and the buttons, `FontAssets.ItemStack`
(`Fonts/Item_Stack`, line spacing 24) for the name under the portrait; both ReLogic `DynamicSpriteFont`s with character
spacing 0 and default character `*`. The desktop supplies the glyph metrics read from the installed XNB fonts.
A character absent from the original font uses its default glyph.

## Not transcribed

Rich-text tags (`[c/…]`, `[i:…]`: `ChatManager.MayNeedParsing`; a text with `[` or `\r` is refused), a corner item
(`npcChatCornerItem > 0`), coins on a button (`TryAddCoins`), gamepad input, `DialoguePortraitPreference` 1 and 2
(the live NPC portraits, drawn with `SpriteBatch.End/Begin` and a scissor) and 3 (no portrait: pass `none`), Chinese
line breaking, the sign editor, and the effects of the clicked interaction (the caller runs `Interact` for `Output.clicked`; only its `DoNPCPortraitHop` is applied here).
-/

namespace Lean4Terria.OriginalForestChatLayout

/-! ## Float32 as the IL computes it -/

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
/-- `conv.i4` -/
private def truncI (f : Float32) : Int := f.toInt32.toInt
/-- `Math.Max(float, float)` -/
private def fmax (a b : Float32) : Float32 := if a > b then a else if a.isNaN then a else b
private def fmin (a b : Float32) : Float32 := if a < b then a else if a.isNaN then a else b

/-! ## Colours and sprites -/

/-- `Microsoft.Xna.Framework.Color` -/
structure Color where
  r : Nat
  g : Nat
  b : Nat
  a : Nat
  deriving BEq, Repr, Inhabited

/-- `new Color(int, int, int, int)`: each channel clamped to 0 … 255. -/
def Color.mk' (r g b a : Int) : Color :=
  let c (x : Int) : Nat := (max 0 (min 255 x)).toNat
  ⟨c r, c g, c b, c a⟩

/-- `Color.PackedValue` (R | G << 8 | B << 16 | A << 24). -/
def Color.packed (c : Color) : UInt32 :=
  c.r.toUInt32 ||| (c.g.toUInt32 <<< 8) ||| (c.b.toUInt32 <<< 16) ||| (c.a.toUInt32 <<< 24)

def white : Color := ⟨255, 255, 255, 255⟩
def black : Color := ⟨0, 0, 0, 255⟩
def brown : Color := ⟨165, 42, 42, 255⟩

/-- `Utils.MultiplyRGBA`: `(byte)((float)(c1 * c2) / 255f)` per channel. -/
def multiplyRGBA (x y : Color) : Color :=
  let m (p q : Nat) : Int := truncI (single (p * q : Nat) / 255)
  Color.mk' (m x.r y.r) (m x.g y.g) (m x.b y.b) (m x.a y.a)

/-- `Color.Lerp` (FNA): amount clamped, `(int)MathHelper.Lerp((float)c1, (float)c2, amount)` per channel. -/
def Color.lerp (x y : Color) (t : Float32) : Color :=
  let t := if t < 0 then 0 else if t > 1 then 1 else t
  let l (p q : Nat) : Int := truncI (single p + (single q - single p) * t)
  Color.mk' (l x.r y.r) (l x.g y.g) (l x.b y.b) (l x.a y.a)

/-- One `SpriteBatch.Draw`, as FNA's `PushSprite` stores it (`SpriteInfo`): the texture (its asset path, or
`font.page<N>` for a glyph page), the source rectangle divided by the texture size, the destination position and size,
the origin divided by the source size (in texels), sin / cos of the rotation, depth, colour and the effects bits. -/
structure Sprite where
  tex : String
  sourceX : Float32
  sourceY : Float32
  sourceW : Float32
  sourceH : Float32
  destX : Float32
  destY : Float32
  destW : Float32
  destH : Float32
  originX : Float32
  originY : Float32
  rotSin : Float32
  rotCos : Float32
  depth : Float32
  color : UInt32
  effects : Nat
  deriving Repr, Inhabited

/-- `Rectangle` -/
structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving BEq, Repr, Inhabited

/-- `Rectangle.Contains(Point)` -/
def Rect.contains (r : Rect) (px py : Int) : Bool := r.x ≤ px && px < r.x + r.w && r.y ≤ py && py < r.y + r.h

private def machineEpsilonFloat : Float32 := Float32.ofBits 0x00000001 -- MathHelper.MachineEpsilonFloat (1.401298E-45)

/-- `SpriteBatch.Draw(texture, position, sourceRectangle, color, rotation, origin, Vector2 scale, effects, depth)`
(also the float-scale overload with `scale = (s, s)`). `src = none` is the whole texture. -/
def draw (tex : String) (texW texH : Nat) (px py : Float32) (src : Option Rect) (color : Color) (rotation : Float32)
    (ox oy : Float32) (sx sy : Float32) (effects : Nat := 0) (depth : Float32 := 0) : Sprite :=
  let tw := single texW
  let th := single texH
  let norm (n : Int) : Float32 := single n.sign * fmax (single n.natAbs) machineEpsilonFloat
  let (srcX, srcY, srcW, srcH, dw, dh) := match src with
    | some r => (single r.x / tw, single r.y / th, norm r.w / tw, norm r.h / th, sx * single r.w, sy * single r.h)
    | none => (0, 0, 1, 1, sx * tw, sy * th)
  { tex, sourceX := srcX, sourceY := srcY, sourceW := srcW, sourceH := srcH, destX := px, destY := py, destW := dw,
    destH := dh, originX := ox / srcW / tw, originY := oy / srcH / th,
    rotSin := (Float.sin rotation.toFloat).toFloat32, rotCos := (Float.cos rotation.toFloat).toFloat32,
    depth, color := color.packed, effects := effects % 4 }

/-! ## Fonts (ReLogic `DynamicSpriteFont`) -/

/-- `SpriteCharacterData`: the glyph rectangle on its page, the padding (cropping) and the kerning (left, width,
right). -/
structure Glyph where
  page : Nat
  glyph : Rect
  padding : Rect
  kx : Float32
  ky : Float32
  kz : Float32
  deriving Repr, Inhabited

structure Font where
  name : String
  spacing : Float32
  lineSpacing : Int
  default : Char
  pages : Array (Nat × Nat)
  /-- the code points of the whole font -/
  ranges : Array (Nat × Nat)
  glyphs : Std.HashMap Nat Glyph

private def nat (s : String) : Nat := s.toNat?.getD 0
private def int (s : String) : Int := s.toInt?.getD 0

def Font.parse (name text : String) : Font := Id.run do
  let mut f : Font := { name, spacing := 0, lineSpacing := 0, default := '*', pages := #[], ranges := #[], glyphs := {} }
  for line in text.splitOn "\n" do
    match line.splitOn " " with
    | "F" :: [sp, ls, d] => f := { f with spacing := Float32.ofBits (nat sp).toUInt32, lineSpacing := int ls, default := Char.ofNat (nat d) }
    | "P" :: ps => f := { f with pages := (ps.map fun p => match p.splitOn "," with
        | [w, h] => (nat w, nat h)
        | _ => (0, 0)).toArray }
    | "R" :: rs => f := { f with ranges := (rs.map fun r => match r.splitOn "-" with
        | [a, b] => (nat a, nat b)
        | _ => (0, 0)).toArray }
    | "G" :: [c, pg, gx, gy, gw, gh, px, py, pw, ph, kx, ky, kz] =>
      let g : Glyph := { page := nat pg, glyph := ⟨int gx, int gy, int gw, int gh⟩, padding := ⟨int px, int py, int pw, int ph⟩,
                         kx := Float32.ofBits (nat kx).toUInt32, ky := Float32.ofBits (nat ky).toUInt32,
                         kz := Float32.ofBits (nat kz).toUInt32 }
      f := { f with glyphs := f.glyphs.insert (nat c) g }
    | _ => pure ()
  return f

/-- `DynamicSpriteFont.GetCharacterData`: the font's glyph, or the default character's for a character the font lacks.
A font fixture may record a character's availability without its glyph metrics. -/
def Font.data (f : Font) (c : Char) : Except String Glyph := do
  if let some g := f.glyphs[c.toNat]? then return g
  if f.ranges.any (fun (a, b) => a ≤ c.toNat && c.toNat ≤ b) then
    throw s!"{f.name}: glyph U+{c.toNat} has no metrics"
  match f.glyphs[f.default.toNat]? with
  | some g => return g
  | none => throw s!"{f.name}: default glyph missing"

/-- `DynamicSpriteFont.MeasureString` -/
def Font.measure (f : Font) (text : String) : Except String (Float32 × Float32) := do
  if text.isEmpty then return (0, 0)
  let mut x : Float32 := 0
  let mut y : Float32 := single f.lineSpacing
  let mut widest : Float32 := 0
  let mut lines : Nat := 0
  let mut right : Float32 := 0
  let mut first := true
  for c in text.toList do
    if c == '\n' then
      widest := fmax (x + fmax right 0) widest
      right := 0
      x := 0
      y := single f.lineSpacing
      first := true
      lines := lines + 1
    else if c == '\r' then pure ()
    else
      let g ← f.data c
      let kx := if first then fmax g.kx 0 else g.kx
      if !first then x := x + (f.spacing + right)
      x := x + (kx + g.ky)
      right := g.kz
      y := fmax y (single g.padding.h)
      first := false
  x := x + fmax right 0
  y := y + single (lines * f.lineSpacing : Int)
  return (fmax x widest, y)

/-- `GlyphMetrics.KernedWidth` / `KernedWidthOnNewLine` -/
private def kerned (g : Glyph) : Float32 := g.kx + g.ky + g.kz
private def kernedOnNewLine (g : Glyph) : Float32 := fmax 0 g.kx + g.ky + g.kz

/-- `StringReaderWrapExtension.GetModeForCharacter` (0 space, 1 newline, 2 other, 3 ignored) -/
private def wrapMode (c : Char) : Nat :=
  if c.toNat < 32 && c != '\n' then 3 else if c == '\n' then 1
  else if c == ' ' || (8201 ≤ c.toNat && c.toNat ≤ 8203) then 0 else 2

/-- `StringReaderWrapExtension.CanBreakBetween` for a culture other than Chinese: after a full-width terminator. -/
private def canBreakBetween (prev _cur : Char) : Bool := "！，。、：？".toList.contains prev

/-- `ReadUntilBreakable`: the next chunk of one mode (ignored characters skipped). -/
private def readUntilBreakable (cs : List Char) : String × List Char := Id.run do
  let mut sb : String := ""
  let mut rest := cs
  let mut cur : Char := cs.headD ' '
  let mut mode : Nat := 3
  while true do
    match rest with
    | [] => break
    | c :: tl =>
      if c.toNat == 0 then break
      if wrapMode c == 3 then
        rest := tl
        continue
      let prev := cur
      cur := c
      let prevMode := mode
      mode := wrapMode c
      if !sb.isEmpty && prevMode != mode then break
      if sb.isEmpty then
        sb := sb.push c
        rest := tl
        continue
      if canBreakBetween prev c then break
      sb := sb.push c
      rest := tl
  return (sb, rest)

private structure Builder where
  completed : String := ""
  working : String := ""
  width : Float32 := 0
  firstLine : Bool := true

private def Builder.commit (b : Builder) : Builder :=
  let completed := if b.firstLine then b.completed else b.completed.push '\n'
  { completed := completed ++ b.working, working := "", width := 0, firstLine := false }

private def Builder.finishPartial (b : Builder) : Builder := if b.width > 0 then b.commit else b

/-- `WrappedTextBuilder.AppendWithHardBreaks` (a chunk wider than a line). -/
private def Builder.hardBreaks (f : Font) (maxWidth : Float32) (b : Builder) (s : String) : Except String Builder := do
  let cs := s.toList.toArray
  let mut b := b
  let mut i : Int := 0
  let mut fuel := 4 * cs.size + 16
  while i < cs.size && fuel > 0 do
    fuel := fuel - 1
    let c := cs[i.toNat]!
    let g ← f.data c
    let w := if b.working.isEmpty then kernedOnNewLine g else b.width + f.spacing + kerned g
    if !(w > maxWidth) then
      b := { b with working := b.working.push c, width := w }
    else if b.working.length > 1 then
      let dash ← f.data '-'
      b := { b with width := b.width + (f.spacing + kerned dash) }
      while b.working.length > 1 && b.width > maxWidth do
        let last ← f.data (b.working.back)
        b := { b with width := b.width - (f.spacing + kerned last), working := String.ofList b.working.toList.dropLast }
        i := i - 1
      b := { b with working := b.working.push '-' }
      b := b.finishPartial
      i := i - 1
    else
      b := b.finishPartial
      b := { b with working := b.working.push c, width := kernedOnNewLine g }
    i := i + 1
  return b

/-- `WrappedTextBuilder.Append(NonBreakingText)` -/
private def Builder.append (f : Font) (maxWidth : Float32) (b : Builder) (s : String) : Except String Builder := do
  -- NonBreakingText: Width, WidthOnNewLine, IsWhitespace
  let mut width : Float32 := 0
  let mut adjust : Float32 := 0
  let mut white := true
  let mut i := 0
  for c in s.toList do
    let g ← f.data c
    if i == 0 then adjust := kernedOnNewLine g - kerned g else width := width + f.spacing
    width := width + kerned g
    if c != ' ' then white := false
    i := i + 1
  let onNewLine := width + adjust
  let w := if b.width == 0 then onNewLine else b.width + f.spacing + width
  if !(w > maxWidth) then return { b with working := b.working ++ s, width := w }
  let b := b.finishPartial
  if white then return b
  if !(onNewLine > maxWidth) then return { b with working := b.working ++ s, width := onNewLine }
  b.hardBreaks f maxWidth s

/-- `DynamicSpriteFont.CreateWrappedText(text, maxWidth, culture)` (not Chinese). -/
def Font.wrap (f : Font) (text : String) (maxWidth : Float32) : Except String String := do
  if !(maxWidth > 0) then return text
  let mut b : Builder := {}
  let mut rest := text.toList
  let mut fuel := text.length + 1
  while fuel > 0 do
    fuel := fuel - 1
    match rest with
    | [] => break
    | c :: tl =>
      if c.toNat == 0 then break
      if c == '\n' then
        rest := tl
        b := b.commit
      else
        let (chunk, r) := readUntilBreakable rest
        rest := r
        b ← b.append f maxWidth chunk
        if chunk.isEmpty then fuel := 0
  return (b.commit).completed

/-- `Utils.WordwrapString(text, font, maxWidth, maxLines, out lineAmount)`: the wrapped lines, at most `maxLines`. -/
def wordwrap (f : Font) (text : String) (maxWidth : Nat) (maxLines : Nat) : Except String (Array String) := do
  let w ← f.wrap text (single maxWidth)
  return ((w.splitOn "\n").take maxLines).toArray

/-- `DynamicSpriteFont.InternalDrawFast` (rotation 0, no effects): one sprite per glyph. -/
def Font.drawString (f : Font) (text : String) (px py : Float32) (color : Color) (ox oy : Float32) (sx sy : Float32)
    (depth : Float32 := 0) : Except String (Array Sprite) := do
  let mut x := px - ox * sx
  let mut y := py - oy * sy
  let lineStart := x
  let mut first := true
  let mut out : Array Sprite := #[]
  for c in text.toList do
    if c == '\n' then
      x := lineStart
      y := y + single f.lineSpacing * sy
      first := true
    else if c == '\r' then pure ()
    else
      let g ← f.data c
      let kx := if first then fmax g.kx 0 else g.kx
      if !first then x := x + f.spacing * sx
      x := x + kx * sx
      let dx := x + single g.padding.x * sx
      let dy := y + single g.padding.y * sy
      let (tw, th) := f.pages[g.page]!
      out := out.push (draw s!"{f.name}.page{g.page}" tw th dx dy (some g.glyph) color 0 0 0 sx sy 0 depth)
      x := x + (g.ky + g.kz) * sx
      first := false
  return out

/-- `ChatManager.ShadowDirections`: −UnitX, UnitX, −UnitY, UnitY. -/
private def shadowDirections : List (Float32 × Float32) := [(-1, -0), (1, 0), (-0, -1), (0, 1)]

/-- `ChatManager.DrawColorCodedStringWithShadow` for a text without tags, `maxWidth < 0`: the four shadows in
`MultiplyRGBA(color, Black)` at `spread` px, then the text. -/
def drawWithShadow (f : Font) (text : String) (px py : Float32) (color : Color) (ox oy sx sy : Float32)
    (spread : Float32 := 2) : Except String (Array Sprite) := do
  if text.toList.any (fun c => c == '[' || c == '\r') then throw "rich-text tags are not transcribed"
  let shadow := multiplyRGBA color black
  let mut out := #[]
  for (dx, dy) in shadowDirections do
    out := out ++ (← f.drawString text (px + dx * spread) (py + dy * spread) shadow ox oy sx sy)
  return out ++ (← f.drawString text px py color ox oy sx sy)

/-- `ChatManager.DrawColorCodedString(…, string text, …, maxWidth < 0, ignoreColors)`: lines, `:`-segments (an `sss…`
segment is a colour code, the colons are dropped) and words, one `DrawString` per word with the origin applied to each;
a space's width between words. -/
def drawCodedString (f : Font) (text : String) (px py : Float32) (color : Color) (ox oy sx sy : Float32)
    (ignoreColors : Bool) : Except String (Array Sprite) := do
  let space := (← f.measure " ").1
  let mut x := px
  let mut y := py
  let mut col := color
  let mut lineHeight : Float32 := 0
  let mut out := #[]
  for line in text.splitOn "\n" do
    for seg in line.splitOn ":" do
      if seg.startsWith "sss" then
        if !ignoreColors then
          if seg.startsWith "sss1" then col := ⟨255, 0, 0, 255⟩
          else if seg.startsWith "sss2" then col := ⟨0, 0, 255, 255⟩
          else if seg.startsWith "sssr" then col := white
        continue
      let mut j := 0
      for word in seg.splitOn " " do
        if j > 0 then x := x + space * sx * 1
        if lineHeight < 1 then lineHeight := 1
        out := out ++ (← f.drawString word x y col ox oy (sx * 1) (sy * 1))
        x := x + (← f.measure word).1 * sx * 1
        j := j + 1
    x := px
    y := y + single f.lineSpacing * lineHeight * sy
    lineHeight := 0
  return out

/-! ## The panel -/

/-- The `NPCInteraction`s the Guide's and the Old Man's panels can show. -/
inductive Interaction where
  /-- `CloseChat`: `Main.CloseNPCChatOrSign()` (see `OriginalForestGuideDialogue.closeButton`) -/
  | close
  /-- `GuideTip`: sound 12, `Main.HelpText()`, `DoNPCPortraitHop()` (see `OriginalForestGuideDialogue.helpButton`) -/
  | help
  /-- `GuideReverseCrafting`: `Main.NPCChatText_GuideReverseCrafting()` (see `OriginalForestGuideCrafting.craftingButton`) -/
  | crafting
  /-- `OldManCurse`: `NPC.SpawnSkeletron` -/
  | curse
  /-- `RequestHome`: `npcChatCornerItem = -1`, sound 12, `DoNPCPortraitHop()`, the housing text -/
  | housing
  /-- `ReportHappiness`: sound 12, the happiness report text, `DoNPCPortraitHop()` -/
  | happiness
  deriving BEq, Repr, Inhabited

/-- `Interact()` ends in `Main.DoNPCPortraitHop()` (`npcChatPortraitFrameCounter = 0`). -/
def Interaction.hopsPortrait : Interaction → Bool
  | .help | .housing | .happiness => true
  | _ => false

/-- One chat-panel button (`NPCInteraction`): what it does, its `GetText()` and `ShowExclamation`. -/
structure Button where
  kind : Interaction
  label : String
  exclamation : Bool := false
  deriving Repr, Inhabited

/-- en-US labels: `Lang.inter[52]`, `[51]`, `[25]`, `[50]`, `UI.NPCHousing`, `UI.NPCCheckHappiness`. -/
def closeLabel : String := "Close"
def helpLabel : String := "Help"
def craftingLabel : String := "Crafting"
def curseLabel : String := "Curse"
def housingLabel : String := "Housing"
def happinessLabel : String := " Happiness"

/-- `NPCChatPanel.PrepareInteractions` for the Guide (22): `CloseChat`, `GuideTip`, `GuideReverseCrafting`, then
`ReportHappiness` (when the housing button is not shown and `player.currentShoppingSettings.HappinessReport` is set)
and `RequestHome` (`NPC.CanShowHomelessText`: homeless), in `NPCInteractions.All` order. -/
def guideButtons (homeless : Bool) (happinessReport : Bool := true) : Array Button :=
  #[⟨.close, closeLabel, false⟩, ⟨.help, helpLabel, false⟩, ⟨.crafting, craftingLabel, false⟩] ++
    (if !homeless && happinessReport then #[⟨.happiness, happinessLabel, false⟩] else #[]) ++
    (if homeless then #[⟨.housing, housingLabel, true⟩] else #[])

/-- The Old Man (37): `CloseChat`, and `OldManCurse` while `!Main.IsItDay()` (`dayTime`, never in a remix world);
`CanShowHomelessText` excludes type 37. -/
def oldManButtons (isDay : Bool) : Array Button :=
  #[⟨.close, closeLabel, false⟩] ++ (if isDay then #[] else #[⟨.curse, curseLabel, false⟩])

/-- The portrait (`DialoguePortraitPreference` 0: `BasicNPCPortrait`'s image). -/
structure Portrait where
  /-- the image's asset path and size, and the frame (`Utils.Frame`: the whole image for the Guide and the Old Man) -/
  texture : String
  texW : Nat
  texH : Nat
  frame : Rect
  /-- `NPC.GivenOrTypeName` -/
  name : String
  /-- `Main.npcChatPortraitFrameCounter` before the draw (it counts up by one per draw; `DoNPCPortraitHop` resets it) -/
  frameCounter : Nat
  deriving Repr, Inhabited

def chatBackTexture : String := "Images/Chat_Back"
def portraitWindowTexture : String := "Images/TownNPCs/Portraits/Portrait_Window"
def quickIconTexture : String := "Images/UI/UI_quickicon1"

/-- The Guide's and the Old Man's portraits (`NPCID.Sets.NPCPortraits`: the plain entry; the `_shimmer` image is
chosen for a shimmered NPC). -/
def guidePortrait (name : String) (counter : Nat) (shimmered : Bool := false) : Portrait :=
  { texture := if shimmered then "Images/TownNPCs/Portraits/Portrait_Guide_shimmer" else "Images/TownNPCs/Portraits/Portrait_Guide",
    texW := 200, texH := 200, frame := ⟨0, 0, 200, 200⟩, name, frameCounter := counter }
def oldManPortrait (counter : Nat) (shimmered : Bool := false) : Portrait :=
  { texture := if shimmered then "Images/TownNPCs/Portraits/Portrait_OldMan_shimmer" else "Images/TownNPCs/Portraits/Portrait_OldMan",
    texW := 200, texH := 200, frame := ⟨0, 0, 200, 200⟩, name := "Old Man", frameCounter := counter }

def mechanicPortrait (name : String) (counter : Nat) : Portrait :=
  { texture := "Images/TownNPCs/Portraits/Portrait_Mechanic", texW := 200, texH := 200,
    frame := ⟨0, 0, 200, 200⟩, name, frameCounter := counter }

/-- What one draw of the panel reads. Screen values are the backbuffer's (before `SetZoom_UI`). -/
structure Input where
  screenWidth : Int
  screenHeight : Int
  /-- `Main._uiScaleUsed` (see `uiScaleUsed`) -/
  uiScale : Float32 := 1
  mouseX : Int
  mouseY : Int
  mouseLeft : Bool := false
  mouseLeftRelease : Bool := true
  /-- `Main.mouseTextColor` -/
  mouseTextColor : Nat := 255
  /-- `Main.GlobalTimeWrappedHourly` (the "!" icon pulse) -/
  time : Float32 := 0
  /-- `Main.npcChatText` -/
  text : String
  buttons : Array Button
  portrait : Option Portrait
  /-- `PlayerInput.IgnoreMouseInterface` -/
  ignoreMouseInterface : Bool := false
  deriving Repr, Inhabited

/-- What the draw produced and changed. -/
structure Output where
  /-- every `SpriteBatch.Draw`, in order, in UI units -/
  sprites : Array Sprite
  /-- the UI-space screen and mouse (`SetZoom_UI`) -/
  uiWidth : Int
  uiHeight : Int
  uiMouseX : Int
  uiMouseY : Int
  panel : Rect
  /-- `TextDisplayCache.TextLines` / `AmountOfLines` -/
  lines : Array String
  /-- `_neededInteractionLines` -/
  buttonLines : Nat
  /-- each button's box (`DrawButtons`' rectangle) -/
  boxes : Array Rect
  /-- the button under the mouse (`_lastHovered` afterwards), and the one a fresh left press clicked -/
  hovered : Option Nat
  clicked : Option Nat
  /-- `player.mouseInterface` set (the mouse over the panel or a button) -/
  mouseInterface : Bool
  /-- `Main.mouseLeftRelease` afterwards (a click consumes it) -/
  mouseLeftRelease : Bool
  /-- `Main.npcChatPortraitFrameCounter` afterwards (one more than before, or 0 when the clicked interaction hops the
  portrait) -/
  frameCounter : Option Nat
  deriving Repr, Inhabited

/-- `Main.UIScale`'s setter: the wanted scale clamped to `UIScaleMax = max(1, min(w / 800f, h / 600f))`. -/
def uiScaleUsed (wanted : Float32) (width height : Int) : Float32 :=
  let mx := fmax 1 (fmin (single width / 800) (single height / 600))
  if wanted > mx then mx else wanted

/-- `PlayerInput.SetZoom_Scaled(1f / UIScale)` for one coordinate. -/
def uiCoordinate (uiScale : Float32) (v : Int) : Int := truncI (single v * (1 / uiScale))

/-- `Utils.BounceEaseOut(t, 4, 2.0)` (`Utils.EaseOutBounce`) -/
private def easeOutBounce (t : Float) : Float :=
  1 - Float.pow (1 - t) 2 * Float.abs (Float.sin (t * (4 * 3.141592653589793)))

/-- `Main.DrawNPCPortrait` with `DialoguePortraitPreference` 0. -/
def drawPortrait (itemStack : Font) (p : Portrait) (chatBack : Color) (topLeftX topLeftY : Float32) : Except String (Array Sprite) := do
  let mut x := topLeftX + single (-56 - 6)
  let mut y := topLeftY + single (56 + 6)
  if p.frameCounter / 120 == 0 then
    let t := (min 80 (p.frameCounter % 120)).toFloat / 80
    let e := (easeOutBounce t).toFloat32
    y := y + single (-56) * 0.25 * (1 - e)
  let bg := draw portraitWindowTexture 112 112 x y (some ⟨0, 0, 112, 112⟩) chatBack 0 56 56 1 1
  let img := draw p.texture p.texW p.texH x y (some p.frame) white 0 (single p.frame.w / 2) (single p.frame.h / 2) 1 1
  let (nw, nh) ← itemStack.measure p.name
  let name ← drawWithShadow itemStack p.name x (y + single (56 + 1)) white (nw / 2) (nh / 2) 1 1
  return #[bg, img] ++ name

/-- C# `float % float` for a non-negative dividend (exact: `a − b·⌊a / b⌋` in double for these magnitudes). -/
private def fmod (a b : Float32) : Float32 :=
  (a.toFloat - b.toFloat * (a.toFloat / b.toFloat).floor).toFloat32

/-- `Utils.DrawNotificationIcon(spriteBatch, position, rotationMultiplier 0, worldSpace false, scale 1)` -/
def drawNotificationIcon (time : Float32) (px py : Float32) : Sprite :=
  let twoPi : Float32 := 6.2831855
  let pulse := (Float.sin (twoPi * (fmod time 1 / 1)).toFloat).toFloat32 * 0.5 + 0.5
  let rot := (Float.sin (fmod time 2 / 2 * twoPi).toFloat).toFloat32 * twoPi * 0.035 * 0
  let half : Color := { white with a := 255 / 2 }
  draw quickIconTexture 6 16 px py none (white.lerp half pulse) rot (single (6 / 2)) (single (16 - 4)) 1 1

/-- `NPCChatPanel.Draw` (and `DrawText`, `DrawButtons`) for one frame. -/
def drawPanel (mouseText itemStack : Font) (input : Input) : Except String Output := do
  let uiW := uiCoordinate input.uiScale input.screenWidth
  let uiH := uiCoordinate input.uiScale input.screenHeight
  let mx := uiCoordinate input.uiScale input.mouseX
  let my := uiCoordinate input.uiScale input.mouseY
  let lines ← wordwrap mouseText input.text 460 10
  let need := (Float.ceil (single input.buttons.size / 4).toFloat).toUInt64.toNat
  let chatBack : Color := ⟨200, 200, 200, 200⟩
  let v : Int := (input.mouseTextColor * 2 + 255 : Nat) / 3
  let textColor := Color.mk' v v v v
  let height : Int := 30 + 30 * lines.size + (30 * need + max 0 (2 * ((need : Int) - 1)))
  let panel : Rect := ⟨uiW / 2 - 250, 100, 500, height⟩
  let tlx := single panel.x
  let tly := single panel.y
  let mut out : Array Sprite := #[]
  -- Chat_Back: the top part down to 30 px above the bottom, then its own bottom 30 px
  out := out.push (draw chatBackTexture 500 500 tlx tly (some ⟨0, 0, 500, height - 30⟩) chatBack 0 0 0 1 1)
  out := out.push (draw chatBackTexture 500 500 tlx (single (panel.y + height - 30)) (some ⟨0, 500 - 30, 500, 30⟩) chatBack 0 0 0 1 1)
  -- DrawText
  for h : i in [0:lines.size] do
    out := out ++ (← drawWithShadow mouseText lines[i] (tlx + 20) (tly + 20 + single (i * 30 : Nat)) textColor 0 0 1 1)
  -- DrawNPCPortrait
  if let some p := input.portrait then out := out ++ (← drawPortrait itemStack p chatBack tlx tly)
  let mut mouseInterface := !input.ignoreMouseInterface && panel.contains mx my
  -- DrawButtons
  let startX := tlx + 30
  let startY := single (panel.y + height) + single (-22 * (need : Int) + max 0 (2 * ((need : Int) - 1)) - 4)
  let mut box : Rect := ⟨truncI startX, truncI startY, 100, 22⟩
  let mut boxes : Array Rect := #[]
  let mut hovered : Option Nat := none
  let mut clicked : Option Nat := none
  let mut release := input.mouseLeftRelease
  let base : Float32 := 0.9
  let mtc := input.mouseTextColor
  let buttonColor := Color.mk' mtc ((mtc.toFloat / 1.1).floor.toUInt64.toNat) (mtc / 2) mtc   -- (int)((double)mtc / 1.1)
  for h : k in [0:input.buttons.size] do
    let b := input.buttons[k]
    if k % 4 == 0 then box := { box with x := truncI startX, y := (k / 4 : Nat) * 22 + truncI startY }
    let (w0, h0) ← mouseText.measure b.label
    let mut sizeX := w0 * base
    let mut sizeY := h0 * base
    let mut mul : Float32 := 1
    if sizeX > 260 then mul := mul * (260 / sizeX)
    box := { box with w := truncI (sizeX * mul) }
    let over := box.contains mx my
    let scale : Float32 := if over then 1.2 else base
    let oy := sizeY * 0.5
    let shadow := if over then brown else black
    let mut px := single box.x
    let py := single (box.y + box.h / 2)
    if over then
      px := px - single (truncI ((1.2 - base) * single box.w * 0.5))
      let f := 1.2 / base
      sizeX := sizeX * f
      sizeY := sizeY * f
      hovered := some k
    for (dx, dy) in shadowDirections do
      out := out ++ (← drawCodedString mouseText b.label (px + dx * 2) (py + dy * 2) shadow 0 oy (scale * mul) (scale * mul) true)
    out := out ++ (← drawCodedString mouseText b.label px py buttonColor 0 oy (scale * mul) (scale * mul) false)
    boxes := boxes.push box
    box := { box with x := box.x + (box.w + 30) }
    if b.exclamation then out := out.push (drawNotificationIcon input.time (px + sizeX * mul + 8) py)
    if !input.ignoreMouseInterface && over then
      mouseInterface := true
      hovered := some k
      if input.mouseLeft && release then
        release := false
        clicked := some k
  let hop := match clicked with
    | some k => (input.buttons[k]?.map (·.kind.hopsPortrait)).getD false
    | none => false
  let counter := input.portrait.map fun p => if hop = true then 0 else p.frameCounter + 1
  return { sprites := out, uiWidth := uiW, uiHeight := uiH, uiMouseX := mx, uiMouseY := my, panel, lines,
           buttonLines := need, boxes, hovered, clicked, mouseInterface, mouseLeftRelease := release,
           frameCounter := counter }

end Lean4Terria.OriginalForestChatLayout
