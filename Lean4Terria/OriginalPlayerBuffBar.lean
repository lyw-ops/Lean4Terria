import Lean4Terria.OriginalForestChatLayout

/-! Terraria 1.4.5.8 desktop Classic Buff icons. UI state is separate from Buff timers.
The caller supplies installed XNB font metrics, textures and localized strings. -/
namespace Lean4Terria.OriginalPlayerBuffBar
open OriginalForestChatLayout (Font Sprite Color Rect draw black multiplyRGBA)
private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32

structure Buff where
  id : Nat
  name : String
  description : String
  width : Nat := 32
  height : Nat := 32
  debuff : Bool := false
  noTime : Bool := false
  deriving Repr

/-- Checked language keys for the current Health states and environmental candles. Mana Surge's
description prefix is completed from the caller's current manaHeat at draw time. -/
def priorityBuffs (lookup : String → String) : Array Buff :=
  #[{ id := 2, name := lookup "BuffName.Regeneration", description := lookup "BuffDescription.Regeneration" },
    { id := 23, name := lookup "BuffName.Cursed", description := lookup "BuffDescription.Cursed", debuff := true },
    { id := 24, name := lookup "BuffName.OnFire", description := lookup "BuffDescription.OnFire", debuff := true },
    { id := 48, name := lookup "BuffName.Honey", description := lookup "BuffDescription.Honey" },
    { id := 86, name := lookup "BuffName.WaterCandle", description := lookup "BuffDescription.WaterCandle", debuff := true },
    { id := 157, name := lookup "BuffName.PeaceCandle", description := lookup "BuffDescription.PeaceCandle", debuff := true },
    { id := 350, name := lookup "BuffName.ShadowCandle", description := lookup "BuffDescription.ShadowCandle", debuff := true },
    { id := 396, name := lookup "BuffName.ManaHeat", description := lookup "BuffDescription.ManaHeat" }]

structure Catalog where
  mouseText : Font
  itemStack : Font
  buffs : Array Buff

structure State where
  /-- DrawBuffIcon draws with the old alpha before it updates this array. -/
  alpha : Array Float32 := Array.replicate 44 0.4
  deriving Repr

structure Input where
  types : Array Nat
  times : Array Int
  mouseX : Int := 0
  mouseY : Int := 0
  inventory : Bool := false
  equipPage : Nat := 0
  screenWidth : Int := 1120
  screenHeight : Int := 704
  mapStyle : Nat := 0
  /-- Main.mH, the equipment area's current minimap offset in UI coordinates. -/
  mapOffset : Int := 0
  right : Bool := false
  rightRelease : Bool := false
  ignoreMouse : Bool := false
  honeyWet : Bool := false
  /-- MouseTextInner uses MouseBorderColor != Transparent. -/
  thickMouse : Bool := true
  holdingItem : Bool := false
  mouseTextColor : Nat := 255
  /-- Player.manaHeat after that update's ResetEffects/UpdateBuffs. This is an
  effect multiplier, not a value inferred from the remaining icon timer. -/
  manaHeat : Float32 := 1
  /-- Resource layer guards; inventories are guarded by their enclosing interface. -/
  options : Bool := false
  fancyUI : Bool := false
  capture : Bool := false
  deriving Repr

structure Text where
  font : String
  text : String
  x : Float32
  y : Float32
  color : Color
  scale : Float32 := 1
  shadow : Bool := false
  deriving Repr, Inhabited

inductive Event where
  | removeBuff (slot : Nat)
  | menuTick
  deriving BEq, Repr

structure Tooltip where
  slot : Nat
  buff : Nat
  name : String
  description : String
  noOverride : Bool := false
  deriving BEq, Repr

structure Output where
  state : State
  sprites : Array Sprite := #[]
  /-- Draw this prefix at the Buff layer, and the remaining glyphs at the tooltip layer. -/
  tooltipStart : Nat := 0
  texts : Array Text := #[]
  events : Array Event := #[]
  tooltip : Option Tooltip := none
  mouseInterface : Bool := false
  /-- Local draw-time result, including immediate cancellation and native compaction. -/
  types : Array Nat
  times : Array Int
  deriving Repr

private def findBuff (catalog : Catalog) (id : Nat) : Except String Buff :=
  match catalog.buffs.find? (·.id == id) with
  | some buff => .ok buff
  | none => .error s!"Buff {id} has no checked icon/tooltip metadata"

/-- Main.GetBuffTooltip IL_00b6: Float32 multiplication precedes Double.Ceiling.
The localized ManaHeat description is a prefix, including its trailing space. -/
def description (buff : Buff) (manaHeat : Float32) : String :=
  if buff.id == 396 then
    let percent := (manaHeat * 100).toFloat.ceil.toInt64.toInt - 100
    s!"{buff.description}{percent}%"
  else buff.description

/-- Lang.LocalizedDuration(span, abbreviated=true, showAllAvailableUnits=false), en-US. -/
def duration (ticks : Int) : String :=
  let seconds := ticks / 60
  if seconds ≥ 86400 then s!"{seconds / 86400} d"
  else if seconds ≥ 3600 then s!"{seconds / 3600} h"
  else if seconds ≥ 60 then s!"{seconds / 60} m"
  else s!"{seconds} s"

/-- DelBuff clears its requested slot and compacts only the first maxBuffs-1 slots. -/
private def remove (types : Array Nat) (times : Array Int) (slot : Nat) : Array Nat × Array Int := Id.run do
  let mut types := types.set! slot 0
  let mut times := times.set! slot 0
  let mut free := 0
  for index in [:43] do
    if types[index]! != 0 && times[index]! != 0 then
      if free < index then
        types := (types.set! free types[index]!).set! index 0
        times := (times.set! free times[index]!).set! index 0
      free := free + 1
  return (types,times)

private def addText (catalog : Catalog) (output : Output) (text : Text) : Except String Output := do
  if text.shadow && text.text.toList.any (fun c => c == '[' || c == '\r') then
    throw "Rich-text buff names need their own checked layout"
  let font := if text.font == catalog.itemStack.name then catalog.itemStack else catalog.mouseText
  let sprites ← if text.shadow then do
      let mut sprites := #[]
      let shadow := multiplyRGBA text.color black
      for (dx,dy) in [((-2 : Float32),0),(2,0),(0,-2),(0,2)] do
        sprites := sprites ++ (← font.drawString text.text (text.x+dx) (text.y+dy) shadow 0 0 1 1)
      pure (sprites ++ (← font.drawString text.text text.x text.y text.color 0 0 1 1))
    else font.drawString text.text text.x text.y text.color 0 0 text.scale text.scale
  return { output with sprites := output.sprites ++ sprites, texts := output.texts.push text }

private def tooltipText (catalog : Catalog) (input : Input) (output : Output) (tip : Tooltip) : Except String Output := do
  let (nameW,nameH) ← catalog.mouseText.measure tip.name
  let (descW,descH) ← catalog.mouseText.measure tip.description
  let mut x := input.mouseX + 14 + (if input.thickMouse then 6 else 0) + (if input.holdingItem then 34 else 0)
  let mut y := input.mouseY + 14 + (if input.thickMouse then 6 else 0)
  -- MouseText_DrawBuffTooltip first keeps the combined name and description on screen.
  let width := max nameW descW
  let height := nameH + descH
  if single x + width + 24 > single input.screenWidth then x := (single input.screenWidth-width-24).toInt32.toInt
  if single y + height + 4 > single input.screenHeight then y := (single input.screenHeight-height-4).toInt32.toInt
  let mut output := output
  let foreground : Color := Color.mk' input.mouseTextColor input.mouseTextColor input.mouseTextColor input.mouseTextColor
  for (dx,dy) in [((-2 : Int),0),(2,0),(0,-2),(0,2),(0,0)] do
    output ← addText catalog output {
      font := catalog.mouseText.name, text := tip.description
      x := single (x+dx), y := single (y+nameH.toInt32.toInt+dy)
      color := if dx == 0 && dy == 0 then foreground else black }
  -- MouseTextInner then clamps the name with its separate 40-pixel margin.
  if single x + nameW + 40 > single input.screenWidth then x := (single input.screenWidth-nameW-40).toInt32.toInt
  if single y + nameH + 40 > single input.screenHeight then y := (single input.screenHeight-nameH-40).toInt32.toInt
  addText catalog output { font := catalog.mouseText.name, text := tip.name, x := single x, y := single y, color := foreground, shadow := true }

/-- One draw pass. No timer is decremented here. Cancellation happens locally in slot order. -/
def frame (catalog : Catalog) (state : State) (input : Input) : Except String Output := do
  unless input.types.size == 44 && input.times.size == 44 && state.alpha.size == 44 do
    throw "Buff bar requires the 44 native slots and alpha entries"
  let mut output : Output := { state, types := input.types, times := input.times }
  if input.inventory && input.equipPage != 2 then return output
  if !input.inventory && (input.options || input.fancyUI || input.capture) then return output
  let mut ordinal := 0
  let mut hovered : Option Nat := none
  let rows := 3 + (if input.screenHeight > 630 + (if input.mapStyle == 1 then 260 else 0) then 1 else 0) +
    (if input.screenHeight > 680 + (if input.mapStyle == 1 then 260 else 0) then 1 else 0) +
    (if input.screenHeight > 730 + (if input.mapStyle == 1 then 260 else 0) then 1 else 0)
  for slot in [:44] do
    let id := output.types[slot]!
    if id == 0 then
      if !input.inventory then output := { output with state.alpha := output.state.alpha.set! slot 0.4 }
      continue
    let buff ← findBuff catalog id
    let x := if input.inventory then input.screenWidth - 84 - (ordinal / rows : Nat) * 46
      else 32 + (ordinal % 11 : Nat) * 38
    let y := if input.inventory then 421 + input.mapOffset + (ordinal % rows : Nat) * 46
      else 76 + (ordinal / 11 : Nat) * 50
    let alpha := output.state.alpha[slot]!
    let c := (alpha * 255).toInt32.toInt
    let color := Color.mk' c c c c
    let icon := draw s!"Images/Buff_{id}" buff.width buff.height (single x) (single y)
      (some ⟨0,0,buff.width,buff.height⟩) color 0 0 0 1 1
    output := { output with sprites := output.sprites.push icon }
    if !buff.noTime && !(id == 48 && input.honeyWet) && output.times[slot]! > 2 then
      output ← addText catalog output { font := catalog.itemStack.name, text := duration output.times[slot]!, x := single x, y := single (y+buff.height), color, scale := 0.8 }
    let over := input.mouseX > x && input.mouseX < x+buff.width && input.mouseY > y && input.mouseY < y+buff.height && !input.ignoreMouse
    let alpha := if over then alpha + 0.1 else alpha - 0.05
    output := { output with state.alpha := output.state.alpha.set! slot (max 0.4 (min 1 alpha)) }
    if over then
      hovered := some slot
      output := { output with mouseInterface := true }
      if input.right && input.rightRelease && !buff.debuff && id != 60 && id != 151 then
        let (types,times) := remove output.types output.times slot
        output := { output with types, times, events := output.events ++ #[.menuTick,.removeBuff slot] }
    if input.inventory then
      output := { output with state.alpha := output.state.alpha.set! slot (max 0.65 output.state.alpha[slot]!) }
    ordinal := ordinal + 1
  output := { output with tooltipStart := output.sprites.size }
  if let some slot := hovered then
    let id := output.types[slot]!
    if id > 0 then
      let buff ← findBuff catalog id
      let tip : Tooltip := {
        slot, buff := id, name := buff.name
        description := description buff input.manaHeat
        noOverride := !input.inventory }
      output := { output with tooltip := some tip }
      output ← tooltipText catalog input output tip
  return output

end Lean4Terria.OriginalPlayerBuffBar
