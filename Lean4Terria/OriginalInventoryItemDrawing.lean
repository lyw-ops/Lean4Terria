import Lean4Terria.OriginalForestGuideCraftGridLayout
import Lean4Terria.OriginalForestGuideCraftGridCatalog

/-!
# What an inventory slot draws for its item (Terraria 1.4.5.8 desktop `ItemSlot.Draw`, item part)

The part of `ItemSlot.Draw(spriteBatch, inv, context, slot, position, lightColor)` after the slot background, for the
contexts the inventory screen and the hotbar use: 0 inventory, 1 coin slots, 2 ammo slots, 3 / 4 chest / bank, 13 the
closed-inventory hotbar, 21 the cursor item (`Main.DrawInterface_38_MouseCarriedObject`: the ref-`Item` overload at
the mouse with `inventoryScale = cursorScale`). Pure: the caller passes the slot's position and scale, the light colour,
the item and what the counts read, and gets every sprite in vanilla order (UI space) plus the coin animation state.
The slot background (`Inventory_Back*`, chosen by context / favourite / selection) is the caller's, as before.

Per item (IL `ItemSlot.Draw` IL_0d1f … IL_1cd4):
* the icon: coins (`Item.IsACoin`, or a coin fading out) through `CoinSlot.DrawItemCoin` (`Images/Coin_<type − 71>`, one
  of 8 frames, the `CoinEntry` jump / spin / fade animation); everything else through `ItemSlot.DrawItemIcon`
  (`OriginalForestGuideCraftGridLayout.itemIconScaled`: one animation frame, `GetAlpha` / `GetItemLight` / glow
  masks / overlays — not the whole texture strip);
* context 13: the ammo / bait / tile-wand / DD2 medal / wire / paint count at (8, 30) · scale, 0.8 × scale;
* the stack number in `Item_Stack` at (10, 26 + lineSpacing) · scale (coin text bounce: `stackTextScale`, fade);
* context 13: the hotbar number at (8, 4) · scale; the cooldown mark (`Images/CoolDown`) of every potion and every
  mana-restoring item, faded by `potionDelay / potionDelayTime` and `Remap(manaPotionDelay, 300, 0, 1, 0)`;
* context 0, slots 0 … 9: the hotbar number at (6, 4) · scale (the selected slot white, alpha 200, 2 px higher), drawn
  also for an empty slot.

The light colour: `lightColor` when not transparent, else White (`default(Color)` → White); the closed hotbar passes
`hotbarLight hotbarScale[i]`. `GetDimSlotForMouseItem` leaves these contexts at 1.

Boundaries (`Except String`): the cursor item's crafting glow (`CraftingEffects.GetGlow > 0`, a shader), the overlays of
5324 / 5329 / 5330 / 6168 / 6169 / 6193 / 6194, loadout-sharing contexts, the 5644 frame while spectating
(`itemIconScaled`), `ItemSlot._nextTickDrawAvailable` (delayed redraw after a transfer).

Checked against vanilla `ItemSlot.Draw` run in the private host
(`.lake/original-forest-guide-grid/1.4.5.8/catalog-production/inventory`).
-/

namespace Lean4Terria.OriginalInventoryItemDrawing

open OriginalForestGuideCraftGridLayout (Catalog ItemVisual drawScaled shadowText scaleColor transparent)
open OriginalForestChatLayout (Sprite Color white multiplyRGBA)
open OriginalForestGuideCraftGridCatalog (Defs ItemDef)

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def natF (n : Nat) : Float32 := single n
private def truncI (f : Float32) : Int := f.toInt32.toInt

/-! ## Static data (vanilla runtime, `inventory/probe/inv_capture.py data`) -/

/-- `Item.potion` after `SetDefaults`. -/
def potionTypes : List Nat := [5, 28, 126, 188, 226, 227, 499, 1134, 1912, 2314, 3001, 3544, 5496]
/-- `Item.DD2Summon` after `SetDefaults`. -/
def dd2SummonTypes : List Nat := [3818, 3819, 3820, 3824, 3825, 3826, 3829, 3830, 3831, 3832, 3833, 3834]
/-- `ItemID.Sets.CommonCoin`. -/
def commonCoins : List Nat := [71, 72, 73, 74]
/-- `Item.GetFlexibleTileWand() != null`, with `ShowsHoverAmmoIcon`. -/
def flexibleWands : List (Nat × Bool) :=
  [(149, false), (177, false), (178, false), (179, false), (180, false), (181, false), (182, false), (275, false),
   (580, false), (999, false), (1417, false), (1813, false), (1869, false), (2625, false), (2626, false), (3749, false),
   (4071, false), (4072, false), (4073, false), (4420, false), (4460, false), (4609, false), (5324, true), (5329, true),
   (5330, true), (5464, true), (5472, false), (5481, true), (6147, false)]

private def f32s (bits : List Nat) : Array Float32 := (bits.map fun b => Float32.ofBits b.toUInt32).toArray
/-- `CoinSlot.FadeAnimKeys` (21) -/
def fadeAnimKeys : Array Float32 := f32s [1050253722, 1053609165, 1056964608, 1058642330, 1060320051, 1061997773,
  1063675494, 1065353216, 1065353216, 1065353216, 1065353216, 1065353216, 1065353216, 1065353216, 1065353216,
  1065353216, 1065353216, 1065353216, 1065353216, 1065353216, 1065353216]
/-- `CoinSlot.TextAnimKeys` (17) -/
def textAnimKeys : Array Float32 := f32s [1065353216, 1065442974, 1065681211, 1066016755, 1066401792, 1066786829,
  1067122373, 1067360610, 1067450368, 1067360610, 1067122373, 1066786829, 1066401792, 1066016755, 1065681211,
  1065442974, 1065353216]
/-- `CoinSlot.JumpAnimKeys` (25) -/
def jumpAnimKeys : Array Float32 := f32s [0, 1047735797, 1054752700, 1058535962, 1060656770, 1062285838, 1063487757,
  1064327289, 1064869193, 1065177894, 1065318487, 1065353216, 1065353216, 1065353216, 1065318487, 1065177894,
  1064869193, 1064327289, 1063487757, 1062285838, 1060656770, 1058535962, 1054752700, 1047735797, 0]
/-- `CoinSlot..cctor` constants -/
def jumpApex : Nat := 12
def jumpTriggerStart : Nat := 9
def jumpTriggerEnd : Nat := 13
def itemSlotCoinJumpScale : Float32 := 10
def jumpAnimHoldTime : Nat := 12
def spinAnimStart : Nat := 9
def spinAnimEnd : Nat := 13

/-! ## The coin animation (`CoinSlot`, `CoinEntry`) -/

/-- One `CoinEntry`. -/
structure CoinEntry where
  type : Nat := 0
  stack : Nat := 0
  drawActive : Nat := 0
  fadeItemType : Nat := 0
  textAnimFrame : Nat := 0
  jumpAnimFrame : Nat := 0
  jumpAnimHold : Nat := 0
  spinAnimFrame : Nat := 0
  deriving Repr, Inhabited, BEq

/-- `CoinDrawState`, what one `ItemSlot.Draw` reads. -/
structure CoinDrawState where
  fadeItem : Nat := 0
  fadeScale : Float32 := 1
  coinAnimFrame : Nat := 0
  coinYOffset : Float32 := 0
  stackTextScale : Float32 := 1
  stackTextDrawFadeOverload : Float32 := -1
  deriving Repr, Inhabited

/-- `CoinSlot.InventoryEntries` (59) and `ChestEntries` (200). -/
structure CoinSlots where
  inventory : Array CoinEntry := Array.replicate 59 {}
  chest : Array CoinEntry := Array.replicate 200 {}
  deriving Repr, Inhabited

/-- `CoinEntry.UpdateAnim` -/
def CoinEntry.updateAnim (e : CoinEntry) : CoinEntry := Id.run do
  let mut e := e
  if e.drawActive > 0 then e := { e with drawActive := e.drawActive - 1 }
  if e.fadeItemType > 0 && e.jumpAnimFrame == 0 && e.textAnimFrame == 0 then e := { e with fadeItemType := 0 }
  if e.textAnimFrame > 0 then e := { e with textAnimFrame := e.textAnimFrame - 1 }
  if e.jumpAnimHold > 0 then e := { e with jumpAnimHold := e.jumpAnimHold - 1 }
  if e.jumpAnimFrame > 0 then
    if e.jumpAnimHold > 0 then
      if e.jumpAnimFrame != jumpApex then
        -- below the apex the frame mirrors to the far side, then steps down
        let f := if e.jumpAnimFrame < jumpApex then jumpApex + jumpApex - e.jumpAnimFrame else e.jumpAnimFrame
        e := { e with jumpAnimFrame := f - 1 }
    else e := { e with jumpAnimFrame := e.jumpAnimFrame - 1 }
  if spinAnimStart ≤ e.jumpAnimFrame && e.jumpAnimFrame ≤ spinAnimEnd then
    return { e with spinAnimFrame := (e.spinAnimFrame + 1) % 14 }
  if e.spinAnimFrame != 0 then e := { e with spinAnimFrame := (e.spinAnimFrame + 1) % 14 }
  return e

/-- `CoinSlot.UpdateSlotAnims` (once per `Main.DrawInterface`; the savings entries are not part of this module). -/
def CoinSlots.updateAnims (c : CoinSlots) : CoinSlots :=
  { chest := c.chest.map (·.updateAnim), inventory := c.inventory.map (·.updateAnim) }

/-- `CoinEntry.UpdateState(type, stack, jumpScale, ref state)` -/
def CoinEntry.updateState (e : CoinEntry) (type stack : Nat) (jumpScale : Float32) : CoinEntry × CoinDrawState :=
  Id.run do
    let mut e := e
    if !(e.type == type && e.drawActive != 0) then
      let mut restart := true
      if type != 0 && e.fadeItemType == type && e.drawActive != 0 then restart := false
      e := { e with fadeItemType := if type == 0 && e.drawActive != 0 && commonCoins.contains e.type then e.type else 0 }
      if e.fadeItemType != 0 then restart := false
      e := { e with type }
      if e.drawActive == 0 then e := { e with stack }
      if restart then e := { e with textAnimFrame := 0, jumpAnimFrame := 0, jumpAnimHold := 0, spinAnimFrame := 0 }
    e := { e with drawActive := 2 }
    if commonCoins.contains e.type || e.type == 3817 || e.fadeItemType != 0 then
      if e.stack != stack then
        e := { e with stack }
        if e.textAnimFrame == 0 then e := { e with textAnimFrame := textAnimKeys.size - 1 }
      if jumpTriggerStart ≤ e.textAnimFrame && e.textAnimFrame ≤ jumpTriggerEnd then
        e := { e with jumpAnimHold := jumpAnimHoldTime }
        if e.jumpAnimFrame == 0 then e := { e with jumpAnimFrame := jumpAnimKeys.size - 1 }
    let mut st : CoinDrawState := {
      stackTextScale := textAnimKeys[e.textAnimFrame]!, coinYOffset := jumpAnimKeys[e.jumpAnimFrame]! * jumpScale,
      coinAnimFrame := e.spinAnimFrame / 2, fadeItem := e.fadeItemType, fadeScale := 1 }
    let fading := !(e.textAnimFrame > 0 || e.jumpAnimFrame ≥ jumpApex || e.jumpAnimFrame ≥ fadeAnimKeys.size)
    if e.fadeItemType != 0 then
      let o := if fading then fadeAnimKeys[e.jumpAnimFrame]! else 1
      return (e, { st with stackTextDrawFadeOverload := o, fadeScale := o })
    if e.stack == 1 && (e.textAnimFrame > 0 || e.jumpAnimFrame != 0) then
      st := { st with stackTextDrawFadeOverload := if fading then fadeAnimKeys[e.jumpAnimFrame]! else 1 }
    else st := { st with stackTextDrawFadeOverload := -1 }
    return (e, st)

/-- `CoinSlot.UpdateDrawState(slot, context, item, ref state)` (called by every `ItemSlot.Draw`). -/
def CoinSlots.drawState (c : CoinSlots) (slot context : Nat) (type stack : Nat) : CoinSlots × CoinDrawState :=
  if context ≤ 2 then
    let (e, st) := (c.inventory[slot]?.getD {}).updateState type stack itemSlotCoinJumpScale
    ({ c with inventory := c.inventory.set! slot e }, st)
  else if context == 3 || context == 4 then
    let (e, st) := (c.chest[slot]?.getD {}).updateState type stack itemSlotCoinJumpScale
    ({ c with chest := c.chest.set! slot e }, st)
  else (c, {})

/-! ## Inputs -/

/-- The item in the slot. -/
structure SlotItem where
  type : Nat
  stack : Nat
  deriving Repr, Inhabited, BEq

/-- What the hotbar's counts and cooldown marks read of the local player. -/
structure PlayerView where
  /-- `player.inventory[0 … 57]` (type, stack); the counts scan these -/
  inventory : Array (Option SlotItem) := #[]
  /-- `Player.selectedItem` -/
  selectedItem : Nat := 0
  potionDelay : Int := 0
  potionDelayTime : Int := 0
  manaPotionDelay : Int := 0
  /-- `Player.FindPaintOrCoating()?.stack` (paint tools 1071, 1072, 1543, 1544) -/
  paintStack : Option Nat := none
  /-- `Player.TryGetFlexibleWandAvailableUsageCount(item, out n)`: `some n` when it returns true -/
  flexibleWandCount : Option Nat := none
  deriving Repr, Inhabited

/-- One `ItemSlot.Draw` call. -/
structure SlotDraw where
  /-- `ItemSlot.Context`: 0, 1, 2, 3, 4, 13 or 21 -/
  context : Nat
  slot : Nat
  /-- the `position` argument: the slot's top-left corner, UI space -/
  x : Float32
  y : Float32
  /-- `Main.inventoryScale` at the call (the hotbar: `hotbarScale[i]`; the cursor item: `Main.cursorScale`) -/
  scale : Float32
  /-- the `lightColor` argument; transparent (`default(Color)`) draws White -/
  light : Color := transparent
  item : Option SlotItem
  player : PlayerView := {}
  /-- the cursor item's `CraftingEffects.GetGlow(item)` (context 21) -/
  mouseGlow : Float32 := 0
  deriving Repr, Inhabited

/-- `Main.GUIHotbarDrawInner` IL_01b5 … IL_01f0: the light colour of hotbar slot `i`,
`Color(255, 255, 255, (int)(75 + 150 · hotbarScale[i]))`. -/
def hotbarLight (hotbarScale : Float32) : Color := Color.mk' 255 255 255 (truncI (75 + 150 * hotbarScale))

/-! ## Drawing -/

/-- `CoinSlot.DrawItemCoin(sb, center, type, frame, scale, sizeLimit 32, color, colorMultiplier)` → final scale. -/
def drawItemCoin (cat : Catalog) (cx cy : Float32) (type frame : Nat) (scale : Float32) (c : Color) (mult : Float32) :
    Except String (Array Sprite × Float32) := do
  let tex := s!"Images/Coin_{type - 71}"
  let (tw, th) ← cat.tex tex
  let fh := th / 8
  let fr : OriginalForestChatLayout.Rect := ⟨0, frame * fh, tw, fh⟩
  let fit : Float32 :=
    if natF tw > 32 || natF fh > 32 then (if tw > fh then 32 / natF tw else 32 / natF fh) else 1
  let s := scale * fit
  -- ContentSamples.ItemsByType[type].GetAlpha(color): coins take the generic path (alpha 0)
  let a := OriginalItemIcon.getAlpha cat.icons type 0 false c
  let col := scaleColor (multiplyRGBA a white) mult
  return (#[drawScaled tex tw th cx cy (some fr) col (natF tw / 2) (natF fh / 2) s s], s)

/-- `ItemSlot.Draw`'s hotbar count (context 13, IL_144c … IL_162a): `none` for -1 (no count drawn). -/
def hotbarCount (defs : Defs) (item : ItemDef) (p : PlayerView) : Except String (Option Nat) := do
  let slots := (p.inventory.extract 0 58).filterMap id
  let sumWhere (f : SlotItem → Except String Bool) : Except String Nat :=
    slots.foldlM (fun n s => do return if ← f s then n + s.stack else n) 0
  let defOf (t : Nat) : Except String ItemDef := defs.item t
  let mut count : Int := -1
  if dd2SummonTypes.contains item.type then
    let n ← sumWhere fun s => pure (s.type == 3822)
    count := if (-1 : Int) + n ≥ 0 then n else -1
  if item.useAmmo > 0 then count ← sumWhere fun s => do return (← defOf s.type).ammo == item.useAmmo
  if item.fishingPole > 0 then count ← sumWhere fun s => do return (← defOf s.type).bait > 0
  if item.tileWand > 0 then count ← sumWhere fun s => pure ((s.type : Int) == item.tileWand)
  if let some n := p.flexibleWandCount then
    if flexibleWands.any (·.1 == item.type) then count := n
  else if flexibleWands.contains (item.type, true) then count := 0
  if [1071, 1072, 1543, 1544].contains item.type then
    if let some n := p.paintStack then count := n
  if [509, 851, 850, 3612, 3625, 3611].contains item.type then count ← sumWhere fun s => pure (s.type == 530)
  return if count < 0 then none else some count.toNat

/-- `Utils.Remap(x, from 300, to 0, 1, 0, clamped)` of the mana-potion delay. -/
private def manaCooldownFade (delay : Int) : Float32 :=
  let x := single delay
  let t := if x > 300 then 0 else if x < 0 then 1 else (x - 300) / (0 - 300)
  1 + (0 - 1) * t

/-- The draws of `ItemSlot.Draw` after the slot background, and the coin animation state the call leaves.
`visual t`: the item's `ItemVisual` (`OriginalForestGuideCraftGridCatalog.visualOf`). -/
def slotItemSprites (cat : Catalog) (defs : Defs) (visual : Nat → Except String ItemVisual) (coins : CoinSlots)
    (d : SlotDraw) : Except String (Array Sprite × CoinSlots) := do
  if ![0, 1, 2, 3, 4, 13, 21].contains d.context then throw s!"ItemSlot.Draw context {d.context} is not transcribed here"
  let scale := d.scale
  let light := if d.light != transparent then d.light else white
  let (bw, bh) ← cat.tex "Images/Inventory_Back"
  let (type, stack) := match d.item with | some i => (i.type, i.stack) | none => (0, 0)
  let (coins, st) := coins.drawState (if d.context == 21 then 0 else d.slot) d.context type stack
  let mut out : Array Sprite := #[]
  let cx := d.x + natF bw * scale / 2
  let cy := d.y + natF bh * scale / 2
  if (type > 0 && stack > 0) || st.fadeItem > 0 then
    let mut finalScale : Float32 := 0
    if (71 ≤ type && type ≤ 74) || st.fadeItem > 0 then
      let t := if st.fadeItem > 0 then st.fadeItem else type
      let (sp, s) ← drawItemCoin cat cx (cy - st.coinYOffset * scale) t st.coinAnimFrame scale light (1 * st.fadeScale)
      out := out ++ sp; finalScale := s
    else
      let it ← visual type
      let y := if type == 3817 then cy - st.coinYOffset * scale else cy
      let (sp, s) ← OriginalForestGuideCraftGridLayout.itemIconScaled cat it d.context cx y scale light
      out := out ++ sp; finalScale := s
    if d.context == 21 && d.mouseGlow > 0 then
      throw "the cursor item's crafting glow (CraftingEffects.GetGlow > 0: the MouseItem shader pass) is not transcribed"
    if [5324, 5329, 5330, 6168, 6169, 6193, 6194].contains type then
      throw s!"item {type}: its ItemSlot.Draw overlay (Extra_257 / Extra_308) is not transcribed"
    let font := cat.itemStack
    let ls := single font.lineSpacing
    let mut count : Option Nat := none
    if d.context == 13 then count ← hotbarCount defs (← defs.item type) d.player
    if let some n := count then
      out := out ++ (← shadowText font (toString n) (d.x + 8 * scale) (d.y + 30 * scale) light scale (scale * 0.8))
    if d.context != 37 && (stack > 1 || st.stackTextDrawFadeOverload > 0) && count.isNone then
      let ts := scale * st.stackTextScale
      let fade := if st.stackTextDrawFadeOverload > 0 then st.stackTextDrawFadeOverload else 1
      out := out ++ (← shadowText font (toString stack) (d.x + 10 * scale) (d.y + (single 26 + ls) * scale)
        (scaleColor light fade) ts ts 0 ls)
    if d.context == 13 then
      let label := if d.slot + 1 == 10 then "0" else toString (d.slot + 1)
      out := out ++ (← shadowText font label (d.x + 8 * scale) (d.y + 4 * scale) light scale scale)
      let (cdw, cdh) ← cat.tex "Images/CoolDown"
      let it ← visual type
      let cdx := cx - natF cdw * scale / 2
      let cdy := cy - natF cdh * scale / 2
      let alphaOf := OriginalItemIcon.getAlpha cat.icons it.type it.alpha it.bossBag light
      -- drawn whenever the item is a potion / restores mana, also at delay 0 (a transparent sprite; 0 / 0 → 0)
      if potionTypes.contains type then
        let f := single d.player.potionDelay / single d.player.potionDelayTime
        out := out.push (drawScaled "Images/CoolDown" cdw cdh cdx cdy none (scaleColor alphaOf f) 0 0 finalScale finalScale)
      if (← defs.item type).healMana > 0 then
        out := out.push (drawScaled "Images/CoolDown" cdw cdh cdx cdy none
          (scaleColor alphaOf (manaCooldownFade d.player.manaPotionDelay)) 0 0 finalScale finalScale)
  if d.context == 0 && d.slot < 10 then
    let label := if d.slot + 1 == 10 then "0" else toString (d.slot + 1)
    let selected := d.player.selectedItem == d.slot
    let col : Color := if selected then { white with a := 200 } else OriginalForestGuideCraftGridLayout.inventoryBackDefault
    let dy : Int := if selected then -2 else 0
    out := out ++ (← shadowText cat.itemStack label (d.x + 6 * scale) (d.y + single (4 + dy) * scale) col scale scale)
  return (out, coins)

/-! ## Visuals for any item (the catalogue only sizes recipe items) -/

/-- The art `slotItemSprites` reads besides item textures. -/
def inventoryTextures : Array String :=
  #["Images/Inventory_Back", "Images/CoolDown", "Images/Coin_0", "Images/Coin_1", "Images/Coin_2", "Images/Coin_3"]

/-- The assets drawing item `t` needs: `TextureAssets.Item[t]` (`TextureCopyLoad` resolved) and its glow mask. -/
def itemTextures (defs : Defs) (t : Nat) : Except String (Array String) := do
  let it ← defs.item t
  let tex ← defs.itemTexture it.type
  return if it.glowMask != -1 then #[tex, OriginalForestGuideCraftGridCatalog.glowTexture it.glowMask] else #[tex]

/-- Read the sizes of `names` not yet known through the caller's XNB header reader
(`OriginalDesktopGuideGrid.textureSize`). -/
def addSizes (sizes : Std.HashMap String (Nat × Nat)) (textureSize : String → IO (Nat × Nat)) (names : Array String) :
    IO (Std.HashMap String (Nat × Nat)) :=
  names.foldlM (fun m n => do if m.contains n then pure m else return m.insert n (← textureSize n)) sizes

/-- The catalogue with extra texture sizes (inventory art, item textures beyond the recipe items). -/
def withSizes (b : OriginalForestGuideCraftGridCatalog.Built) (sizes : Std.HashMap String (Nat × Nat)) : Catalog :=
  { b.catalog with textures := sizes.fold (fun m k v => m.insert k v) b.catalog.textures }

/-- `ItemVisual` of any item type from the catalogue's metadata, the sizes and the current animation clock
(`Built.clock`, advanced by `Built.tick`), so animated items show their current frame. -/
def visualFor (b : OriginalForestGuideCraftGridCatalog.Built) (sizes : Std.HashMap String (Nat × Nat)) (t : Nat) :
    Except String ItemVisual := do
  let textures := sizes.fold (fun m k v => m.insert k v) b.textures
  let d ← b.defs.item t
  let texture ← b.defs.itemTexture d.type
  let tex ← match textures[texture]? with | some x => pure x | none => throw s!"texture size of {texture} not supplied"
  let clock := b.clock.foldl (fun m (t, a) => m.insert t a) {}
  OriginalForestGuideCraftGridCatalog.visualOf b.defs d "" texture tex textures clock

end Lean4Terria.OriginalInventoryItemDrawing
