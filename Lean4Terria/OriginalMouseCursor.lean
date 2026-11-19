import Lean4Terria.OriginalCursorItemIcon

/-! Ordinary keyboard/mouse pointer of desktop Terraria1.4.5.8:
DrawThickCursor(false), then DrawCursor(returnedOffset, false).
No smart/gamepad/rainbow/override cursor or over-cursor particles. -/
namespace Lean4Terria.OriginalMouseCursor

open OriginalForestChatLayout (Color Sprite white)
open OriginalCursorItemIcon (Clock)

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32

/-- Main's static constructor defaults. LoadSettings only changes configured
channels; an absent MouseColor/MouseBorderColor dictionary preserves these. -/
def defaultMouseColor : Color := ⟨255, 50, 95, 255⟩
def defaultBorderColor : Color := ⟨64, 64, 64, 64⟩
def initialCursorColor : Color := white

/-- The color portion of Main.CursorColor, using the already-updated shared
Clock. Call only when that UI clock updates: a draw alone does not recompute
the cached cursorColor after a mouseColor configuration change. Mouse alpha
does not participate; native alpha uses255*(cursorAlpha*.3f+.7f). -/
def colorFromClock (clock : Clock) (mouseColor : Color) : Color :=
  let channel (n : Nat) : Int := (single n * clock.alpha).toInt32.toInt
  Color.mk' (channel mouseColor.r) (channel mouseColor.g) (channel mouseColor.b)
    (255 * (clock.alpha * 0.3 + 0.7)).toInt32.toInt

structure Textures where
  cursorWidth : Nat := 14
  cursorHeight : Nat := 14
  borderWidth : Nat := 18
  borderHeight : Nat := 18
  deriving Repr

structure Input where
  mouseX : Int := 0
  mouseY : Int := 0
  cursorScale : Float32 := 0
  cursorColor : Color := initialCursorColor
  borderColor : Color := defaultBorderColor
  gameMenu : Bool := false
  alreadyGrabbingSunOrMoon : Bool := false
  deriving Repr

structure ThickOutput where
  offset : Float32 × Float32 := (0, 0)
  sprites : Array Sprite := #[]
  deriving Repr

/-- Main.ThickMouse compares the entire packed Color with Transparent,
not only its alpha. A nonzero RGB with alpha0 still enables the four draws. -/
def thickMouse (color : Color) : Bool := color.packed != 0

private def hidden (input : Input) : Bool :=
  input.gameMenu && input.alreadyGrabbingSunOrMoon

/-- Full ordinary mouse branch of DrawThickCursor(false), including its
four-sprite order and the offset subsequently passed to DrawCursor. -/
def drawThick (textures : Textures) (input : Input) : ThickOutput := Id.run do
  if !thickMouse input.borderColor || hidden input then return {}
  let mut sprites := #[]
  let scale := input.cursorScale * 1.1
  for (dx, dy) in (#[(0, 1), (1, 0), (0, -1), (-1, 0)] : Array (Int × Int)) do
    let px := single input.mouseX + (single dx + 2)
    let py := single input.mouseY + (single dy + 2)
    sprites := sprites.push (OriginalForestChatLayout.draw "Images/UI/Cursor_11"
      textures.borderWidth textures.borderHeight px py none input.borderColor 0 2 2 scale scale)
  return { offset := (2, 2), sprites }

private def shadowColor (color : Color) : Color :=
  Color.mk' (single color.r * 0.2).toInt32.toInt (single color.g * 0.2).toInt32.toInt
    (single color.b * 0.2).toInt32.toInt (single color.a * 0.5).toInt32.toInt

/-- Full ordinary mouse sprite branch of DrawCursor(offset,false). Native
dead/UI/camera flags clear smart state but do not suppress these two draws. -/
def drawCursor (textures : Textures) (input : Input) (offset : Float32 × Float32) : Array Sprite :=
  if hidden input then #[] else Id.run do
    let px := single input.mouseX + offset.1
    let py := single input.mouseY + offset.2
    let scale := input.cursorScale * 1.1
    let shadow := OriginalForestChatLayout.draw "Images/UI/Cursor_0"
      textures.cursorWidth textures.cursorHeight (px + 1) (py + 1) none
      (shadowColor input.cursorColor) 0 0 0 scale scale
    let foreground := OriginalForestChatLayout.draw "Images/UI/Cursor_0"
      textures.cursorWidth textures.cursorHeight px py none input.cursorColor 0 0 0
      input.cursorScale input.cursorScale
    return #[shadow, foreground]

/-- Ordinary Vanilla: Cursor layer sprites, before Mouse Item / NPC Head and
Interact Item Icon. Keep their FNA order; do not draw a second native pointer. -/
def draw (textures : Textures) (input : Input) : Array Sprite :=
  let thick := drawThick textures input
  thick.sprites ++ drawCursor textures input thick.offset

end Lean4Terria.OriginalMouseCursor
