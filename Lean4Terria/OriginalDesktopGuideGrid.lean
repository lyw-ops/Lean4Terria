import Lean4Terria.OriginalDesktopDialogue
import Lean4Terria.OriginalDesktopRename
import Lean4Terria.OriginalForestGuideCraftGridLayout
import Lean4Terria.OriginalForestGuideCrafting

namespace Lean4Terria.OriginalDesktopGuideGrid

open OriginalForestGuideCraftGridLayout renaming State → GridState, Catalog → GridCatalog, Input → GridInput, Output → GridOutput
open OriginalForestGuideCrafting renaming State → CraftState, Effects → CraftEffects, Mouse → CraftMouse

@[extern "terria_original_texture_size"] private opaque textureDimensions
  (asset : @& String) : IO UInt64

@[extern "terria_guide_grid_wheel"] private opaque wheelDelta : IO UInt32
/-- Reuse the editor's actual character queue, held keys and paste request. -/
def readInput (input : GridInput) (keyboard : OriginalStorageRename.TextInput) (enter : Bool) : IO GridInput := do
  let wheel := Int32.ofUInt32 (← wheelDelta) |>.toInt
  let frame ← OriginalDesktopRename.readFrame keyboard enter
  let keys := frame.keys
  let mut held := #[]
  for (down, key) in #[(keys.back, 8), (keys.control, 162), (keys.alt, 164), (keys.shift, 160),
      (keys.z, 90), (keys.x, 88), (keys.c, 67), (keys.v, 86), (keys.insert, 45), (keys.delete, 46), (keys.escape, 27)] do
    if down then held := held.push key
  return { input with wheel, held, textQueued := some frame.queued, clipboard := frame.clipboard }

def textureSize (asset : String) : IO (Nat × Nat) := do
  let dimensions ← textureDimensions asset
  let width := (dimensions &&& 0xffffffff).toNat
  let height := (dimensions >>> 32).toNat
  if width == 0 || height == 0 then
    throw (IO.userError s!"Could not read the installed Terraria texture {asset}.")
  return (width, height)

private def item (stack : Option OriginalItems.Stack) : Option (Nat × Nat) :=
  stack.map fun stack => (stack.item.toNat, stack.count)

structure Frame where
  grid : GridState
  crafting : CraftState
  output : GridOutput
  effects : CraftEffects

/-- The layout draws the guide slot before ItemSlot.Handle, then uses its
updated item for the guide line. Re-evaluate the pure layout after an exchange
so its item input has both sides of that draw-time interaction. -/
def advance (catalog : GridCatalog) (grid : GridState) (crafting : CraftState)
    (input : GridInput) (mouse : CraftMouse) (material : Nat → Bool)
    (clockAdvanced : Bool := false) : Except String Frame := do
  let crafting := if clockAdvanced then crafting else OriginalForestGuideCrafting.drawTick crafting
  -- Recipe.UpdateRecipeList may refocus Main.focusRecipe before the window draws.
  let grid := { grid with focusRecipe := crafting.focusRecipe }
  let input := { input with
    guide := item crafting.guideItem
    guideAfter := item crafting.guideItem
    guideObject := crafting.guideToken
    mouseRight := mouse.right, mouseRightRelease := mouse.rightRelease
    holdingItem := crafting.mouseItem.isSome }
  let (next, output) ← OriginalForestGuideCraftGridLayout.frame catalog grid input
  let (crafting, effects) := if output.guideSlotHandle then
      OriginalForestGuideCrafting.guideSlot material mouse crafting
    else (crafting, {})
  let (next, output) ← if item crafting.guideItem != input.guideAfter then
      OriginalForestGuideCraftGridLayout.frame catalog grid { input with guideAfter := item crafting.guideItem }
    else pure (next, output)
  let crafting := { crafting with
    focusRecipe := next.focusRecipe
    preventCraft := output.preventCrafting.getD crafting.preventCraft
    stackSplit := if output.recipeClicked then 15 else crafting.stackSplit
    grid := { crafting.grid with visible := next.visible, selected := next.selected }
    pipsUseGrid := if output.gridClosed then false else crafting.pipsUseGrid }
  return { grid := next, crafting, output, effects }

def draw (output : GridOutput) (scale : Float32 := 1) : IO Unit := do
  let (first, last) := output.clipRange
  OriginalDesktopDialogue.drawSprites (output.sprites.extract 0 first) scale
  let clip := output.clip
  Desktop.beginClip clip.x.toNat.toUInt32 clip.y.toNat.toUInt32 clip.w.toNat.toUInt32 clip.h.toNat.toUInt32
  try
    OriginalDesktopDialogue.drawSprites (output.sprites.extract first last) scale
  finally
    Desktop.endClip
  OriginalDesktopDialogue.drawSprites (output.sprites.extract last output.sprites.size) scale

end Lean4Terria.OriginalDesktopGuideGrid
