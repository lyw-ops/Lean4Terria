import Lean4Terria.Desktop
import Lean4Terria.OriginalStorageRename.Keyboard

namespace Lean4Terria.OriginalDesktopRename

open OriginalStorageRename
open OriginalStorageRename.Keyboard (Box Part Placed View)

@[extern "terria_rename_keys"] private opaque inputKeys : IO UInt32
@[extern "terria_rename_text_width"] opaque fontWidth (label : @& String) : IO Float
@[extern "terria_rename_text"] private opaque drawText (label : @& String)
    (x y scale : Float) (color : UInt32) : IO Unit

def readCharacters : IO Utf16 := do
  let mut queued := #[]
  repeat
    let code ← Desktop.readCharacter
    if code == 0 then break
    queued := queued ++ utf16 (String.singleton (Char.ofNat code.toNat))
  return queued

def readFrame (input : TextInput) (enter : Bool) (allowMultiline : Bool := false) : IO Frame := do
  let bits ← inputKeys
  let down (n : UInt32) := bits &&& (1 <<< n) != 0
  let keys : Keys := {
    control := down 0, alt := down 1, shift := down 2, z := down 3, x := down 4, c := down 5, v := down 6
    insert := down 7, delete := down 8, escape := down 9, back := down 10 }
  let queued ← readCharacters
  let queued := if enter then queued.push 13 else queued
  let pasting := if input.last.control && !input.last.alt then input.last.v && !input.beforeLast.v &&
      !(input.last.z && !input.beforeLast.z) && !(input.last.x && !input.beforeLast.x) &&
      !(input.last.c && !input.beforeLast.c) && !(input.last.insert && !input.beforeLast.insert)
    else input.last.shift && input.last.insert && !input.beforeLast.insert
  let clipboard ← if pasting then do
      let content ← Desktop.readClipboard
      pure (utf16 (String.ofList (content.toList.filter fun c =>
        (c.toNat ≥ 32 && c.toNat != 127) || (allowMultiline && c == '\n'))))
    else pure #[]
  return { keys, queued, clipboard }

structure Panel where
  parts : Array Placed
  text : String
  textWidth : Float32
  labelWidth : Float32
  saveWidth : Float32
  cancelWidth : Float32
  hovered : Option Part

def panel (width height : Nat) (keyboard : Keyboard) (pointer : Float32 × Float32) : IO Panel := do
  let text := ofUtf16 keyboard.text
  let measure (s : String) := (·.toFloat32) <$> fontWidth s
  let textWidth ← measure text
  let labelWidth ← measure Data.enterNewName
  let saveWidth ← measure Data.uiSave
  let cancelWidth ← measure Data.uiCancel
  let measure := fun s =>
    if s == text then textWidth
    else if s == Data.enterNewName then labelWidth
    else if s == Data.uiSave then saveWidth
    else if s == Data.uiCancel then cancelWidth
    else 0
  let view : View := {
    screenWidth := width.toUInt64.toFloat32, screenHeight := height.toUInt64.toFloat32
    parked := keyboard.drawnFrames ≤ 1, text }
  let parts := Keyboard.layout view measure
  let hovered := (Keyboard.elementAt parts pointer).bind (Keyboard.clickTarget parts)
  let parts := Keyboard.layout { view with
    saveHovered := hovered == some .saveText, cancelHovered := hovered == some .cancelText } measure
  return { parts, text, textWidth, labelWidth, saveWidth, cancelWidth, hovered }

def Panel.blocksWorld (panel : Panel) (pointer : Float32 × Float32) : Bool :=
  panel.parts.any fun element =>
    element.dims.contains pointer && (element.part == .textBox || element.part == .label ||
      element.part == .saveText || element.part == .cancelText)

private def drawAt (value : String) (point : Float32 × Float32) (scale : Float32) (color : UInt32) : IO Unit :=
  drawText value point.1.toFloat point.2.toFloat scale.toFloat color

def Panel.draw (panel : Panel) (frame : Nat) : IO Unit := do
  for part in panel.parts do
    match part.part with
    | .textBox =>
      drawAt panel.text (Keyboard.textPosition part.inner (panel.textWidth * 0.78) 0.78) 0.78 0xffffffff
      if Keyboard.caretVisible frame then
        let position := Keyboard.caretPosition part.inner (panel.textWidth * 0.78) panel.textWidth
        drawAt "|" position 0.78 0xffffffff
    | .label =>
      let x := part.inner.x + (part.inner.width - panel.labelWidth * 0.75) * 0.5
      drawAt Data.enterNewName (x, part.inner.y - 7.5) 0.75 0xffffffff
    | .saveText | .cancelText =>
      let save := part.part == .saveText
      let hovered := panel.hovered == some part.part
      let scale : Float32 := if hovered then 0.85 else 0.72
      let width := if save then panel.saveWidth else panel.cancelWidth
      let label := if save then Data.uiSave else Data.uiCancel
      drawAt label (Keyboard.textPosition part.inner (width * scale) scale) scale
        (if hovered then 0xffffffff else 0xc0c0c0ff)
    | _ => pure ()

end Lean4Terria.OriginalDesktopRename
