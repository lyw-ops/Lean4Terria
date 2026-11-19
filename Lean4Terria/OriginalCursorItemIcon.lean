import Lean4Terria.OriginalForestGuideCraftGridLayout

/-! Keyboard/mouse candle and ordinary workbench icons, desktop Terraria1.4.5.8.
The UI layer is Main.DrawInterface_40_InteractItemIcon, after the cursor layer.
This wraps the existing item-icon renderer; it does not duplicate SpriteBatch
geometry or treat the interaction icon as an inventory slot. -/
namespace Lean4Terria.OriginalCursorItemIcon

open OriginalForestChatLayout (Sprite Color white)
open OriginalForestGuideCraftGridLayout (Catalog)

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32

/-- Main's initial static values, before CursorColor has run. Keep this clock
outside world/player saves. CursorColor advances on its update call, not draw. -/
structure Clock where
  alpha : Float32 := 0
  direction : Int := 1
  scale : Float32 := 0
  deriving Repr

def Clock.update (clock : Clock) : Clock := Id.run do
  let mut alpha := clock.alpha + single clock.direction * 0.015
  let mut direction := clock.direction
  if alpha ≥ 1 then
    alpha := 1
    direction := -1
  -- The native comparison widens the Single alpha to Double, and uses0.6d.
  if alpha.toFloat ≤ 0.6 then
    alpha := 0.6
    direction := 1
  return { alpha, direction, scale := (alpha * 0.3 + 0.7) + 0.1 }

structure Cache where
  time : Int := 0
  selectedItemID : Int := 0
  screenPosition : Float32 × Float32 := (0, 0)
  deriving Repr

/-- Main.ItemIconCacheUpdate, called only by the original owner/mining/etc.
call sites that request it. Candle hover itself does not call this function. -/
def Cache.remember (_cache : Cache) (mouseX mouseY selectedItemID : Int) : Cache :=
  { time := 10, selectedItemID, screenPosition := (single mouseX, single mouseY) }

/-- ItemIconCacheVerification runs inside the draw method, after its two
entry guards. Stationary at the remembered point does not consume time.
Moving away consumes one per draw; farther than4px or changing item clears it.
The remembered point stays unchanged. -/
def Cache.verify (cache : Cache) (mouseX mouseY selectedItemID : Int) : Cache := Id.run do
  if cache.time ≤ 0 then return cache
  let dx := cache.screenPosition.1 - single mouseX
  let dy := cache.screenPosition.2 - single mouseY
  let distance := ((dx * dx + dy * dy).toFloat.sqrt).toFloat32
  let mut time := if distance != 0 then cache.time - 1 else cache.time
  if distance > 4 then time := 0
  if cache.selectedItemID != selectedItemID then time := 0
  return { cache with time }

structure Input where
  mouseX : Int := 0
  mouseY : Int := 0
  cursorScale : Float32 := 1
  iconEnabled : Bool := false
  iconID : Int := 0
  iconPush : Int := 0
  iconText : String := ""
  /-- The checked base icons ignore this flag (only the ammo branch uses it). -/
  iconReversed : Bool := false
  hoveringOverNPC : Bool := false
  mouseInterface : Bool := false
  /-- Main.MouseDisplayItem.IsAir, which also accounts for FakeCursorItem. -/
  mouseDisplayItemIsAir : Bool := true
  /-- Main.mouseItem.type, deliberately distinct from MouseDisplayItem.IsAir. -/
  mouseItemType : Int := 0
  selectedItemType : Int := 0
  selectedItemAlpha : Nat := 0
  deriving Repr

structure Output where
  cache : Cache
  sprites : Array Sprite := #[]
  deriving Repr

private def isCheckedIcon (id : Int) : Bool := id == 148 || id == 3117 || id == 5322 || id == 36

/-- The checked candle/workbench part of DrawInterface_40_InteractItemIcon. The caller
passes its actual, current catalog. These assets are12×20/16×20/12×20 and32×18,
have no animation or special icon overlay, and need no32px fitting: the
existing renderer's32 limit and native9999 limit therefore both leave fit1.
Unsupported item/text branches are explicit boundaries instead of approximate
slot-sized icons. The input player's iconID is not changed by this method. -/
def draw (catalog : Catalog) (cache : Cache) (input : Input) : Except String Output := do
  if input.hoveringOverNPC || input.mouseInterface then return { cache }
  let type := if input.iconID != 0 then input.iconID else input.selectedItemType
  let requested := input.iconEnabled && type != 0 && input.mouseDisplayItemIsAir
  let cache := cache.verify input.mouseX input.mouseY input.selectedItemType
  let cached := cache.time > 0 && input.mouseItemType < 1
  if !requested && !cached then return { cache }
  if !input.iconText.isEmpty then throw "interaction icon MouseText is not covered"
  if type ≤ 0 then return { cache }
  if !isCheckedIcon type then throw s!"interaction icon item {type} is outside the checked candle/workbench path"
  let some visual := catalog.items[type.toNat]? | throw s!"missing interaction icon item {type}"
  if visual.texW > 32 || visual.texH > 32 || visual.animation.isSome then
    throw s!"interaction icon {type} no longer has the checked single-frame size"
  if type == 36 && (visual.texW != 32 || visual.texH != 18) then
    throw "ordinary workbench icon no longer has its checked 32x18 texture"
  -- The cached branch temporarily swaps the player's iconID for the cached
  -- selectedID, but the earlier local item type remains unchanged.
  let effectiveIconID := if requested then input.iconID else cache.selectedItemID
  let color := if effectiveIconID != 0 then white else
    OriginalItemIcon.getAlpha catalog.icons input.selectedItemType.toNat input.selectedItemAlpha false white
  let (color, _) := OriginalItemIcon.itemLight catalog.icons type.toNat false color
  let offset := 10 + input.iconPush
  let x := single (input.mouseX + offset) + single visual.texW * input.cursorScale * 0.5
  let y := single (input.mouseY + offset) + single visual.texH * input.cursorScale * 0.5
  let (sprites, _) ← OriginalForestGuideCraftGridLayout.itemIconScaled catalog visual 21 x y input.cursorScale color
  return { cache, sprites }

end Lean4Terria.OriginalCursorItemIcon
