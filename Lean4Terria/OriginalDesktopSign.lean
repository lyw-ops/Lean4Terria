import Lean4Terria.OriginalDesktopRename

namespace Lean4Terria.OriginalDesktopSign

open Desktop OriginalStorageRename

@[extern "terria_original_sign_text"] private opaque signText (label : @& String)
    (x y scale : Float) (color border : UInt32) : IO Unit

structure Edit where
  input : TextInput := {}
  text : Utf16

def Edit.start (value : String) : Edit := { text := utf16 value }

def Edit.advance (edit : Edit) (enter : Bool) : IO (Option Edit) := do
  let frame ← OriginalDesktopRename.readFrame edit.input enter (allowMultiline := true)
  let (input, result) := getInputText edit.input edit.text frame
  if let some copied := result.copied then Desktop.setClipboard (ofUtf16 copied)
  if result.escape then return none
  let text := if result.enter then result.text.push 10 else result.text
  return some { input, text := trimUserString text 1200 }

inductive Button where
  | close | edit
  deriving BEq

structure Panel where
  left : Nat
  lines : Array String
  closeWidth : Nat
  editWidth : Nat
  editing : Bool

private def top : Nat := 100

def Panel.height (panel : Panel) : Nat := 60 + 30 * panel.lines.size

private def Panel.buttonY (panel : Panel) : Nat := top + panel.height - 26

def Panel.contains (panel : Panel) (x y : Nat) : Bool :=
  panel.left ≤ x && x < panel.left + 500 && top ≤ y && y < top + panel.height

def Panel.buttonAt (panel : Panel) (x y : Nat) : Option Button :=
  if y < panel.buttonY || panel.buttonY + 22 ≤ y then none
  else if panel.left + 30 ≤ x && x < panel.left + 30 + panel.closeWidth then some .close
  else if panel.left + 136 ≤ x && x < panel.left + 136 + panel.editWidth then some .edit
  else none

private def wrap (value : String) : IO (Array String) := do
  let mut lines := #[]
  for paragraph in value.splitOn "\n" do
    let mut line := ""
    for word in paragraph.splitOn " " do
      if lines.size == 10 then return lines
      let next := if line.isEmpty then word else line ++ " " ++ word
      if (← textWidth next 16) ≤ 460 then
        line := next
      else
        if !line.isEmpty then lines := lines.push line
        line := ""
        for ch in word.toList do
          if lines.size == 10 then return lines
          let next := line.push ch
          if (← textWidth next 16) > 460 then
            lines := lines.push line
            line := String.singleton ch
          else line := next
    if lines.size == 10 then return lines
    lines := lines.push line
  return lines

/-- `NPCChatPanel.Draw` anchors the 500-pixel chat background at y = 100.
`TextDisplayCache` wraps to 460 pixels and at most ten rows. -/
def panel (screenWidth : Nat) (value : String) (editing : Bool) : IO Panel := do
  let lines ← wrap value
  let closeWidth := (← textWidth "Close" 16) * 9 / 10
  let editWidth := (← textWidth (if editing then "Save" else "Edit") 16) * 9 / 10
  return { left := screenWidth / 2 - 250, lines, closeWidth, editWidth, editing }

def Panel.draw (panel : Panel) (mouseX mouseY tick : Nat) : IO Unit := do
  let bodyHeight := panel.height - 30
  discard <| originalSprite "Chat_Back" panel.left top 500 bodyHeight 0 0 500 bodyHeight (color := 0xc8c8c8c8)
  discard <| originalSprite "Chat_Back" panel.left (top + bodyHeight : Nat) 500 30 0 470 500 30
    (color := 0xc8c8c8c8)
  for (line, index) in panel.lines.zipIdx do
    let y := top + 20 + 30 * index
    signText line (panel.left + 20).toFloat y.toFloat 1 0xffffffff 0x000000ff
    if panel.editing && index + 1 == panel.lines.size && tick % 40 ≤ 20 then
      let width ← textWidth line 16
      signText "|" (panel.left + 20 + width).toFloat y.toFloat 1 0xffffffff 0x000000ff
  let hovered := panel.buttonAt mouseX mouseY
  for (button, label, x, width) in #[
      (.close, "Close", panel.left + 30, panel.closeWidth),
      (.edit, if panel.editing then "Save" else "Edit", panel.left + 136, panel.editWidth)] do
    let active := hovered == some button
    let x := if active then x.toFloat - width.toFloat * 0.15 else x.toFloat
    let y := panel.buttonY.toFloat + (if active then 0 else 2)
    signText label x y (if active then 1.2 else 0.9) 0xffe77fff
      (if active then 0xa52a2aff else 0x000000ff)

end Lean4Terria.OriginalDesktopSign
