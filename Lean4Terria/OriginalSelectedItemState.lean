/-! Terraria 1.4.5.8 Player.SelectedItemState. Native evidence and caller notes:
`.lake/original-selected-item-state/REPORT.md`. The effective selection is
independent of the desired hotbar slot and may be overridden by mouse slot58. -/
namespace Lean4Terria.OriginalSelectedItemState

structure State where
  selected : Nat := 0
  hotbar : Nat := 0
  buffered : Option Nat := none
  overridden : Option Nat := none
  deriving BEq, Repr, Inhabited

def State.hasBuffered (state : State) : Bool := state.buffered.isSome
def State.hasOverride (state : State) : Bool := state.overridden.isSome

/-- The original's LastNonOverridenSelection getter, including -1 during an
active override without a buffered change. -/
def State.lastNonOverridden (state : State) : Int :=
  match state.buffered with
  | some slot => slot
  | none => if state.hasOverride then -1 else state.selected

structure Context where
  itemAnimation : Int := 0
  itemTime : Int := 0
  itemTimeMax : Int := 0
  reuseDelay : Int := 0
  channel : Bool := false
  pendingItemReuse : Bool := false
  controlUseItem : Bool := false
  releaseUseItem : Bool := true
  blockMouse : Bool := false
  lastItemUseAttemptSuccess : Bool := false
  afkCounter : Int := 0
  afkCounterForKiting : Int := 0
  deriving BEq, Repr, Inhabited

def Context.usingOrReusingItem (context : Context) : Bool :=
  context.itemAnimation > 0 || context.reuseDelay > 0 || context.channel || context.pendingItemReuse

def Context.canChange (context : Context) : Bool :=
  !context.usingOrReusingItem && context.itemTime == 0

/-- The ordinary MouseLeft branch in TriggersSet.CopyInto IL_01c9..01ee.
The caller starts controlUseItem=false; other input modes remain its responsibility.
blockMouse is sticky until physical MouseLeft is released, including over a GUI. -/
def mouseControls (blockMouse rawMouseLeft mouseInterface : Bool) : Bool × Bool :=
  if !rawMouseLeft then (false,false)
  else (blockMouse,!blockMouse && !mouseInterface)

structure Input where
  context : Context
  localPlayer : Bool := true
  /-- IsAir of inventory0..58. Slot58 is the cursor inventory mirror. -/
  inventoryAir : Array Bool
  /-- Main.mouseItem.IsAir, tested before SmartSelect / auto-kite selection. -/
  mouseAir : Bool := true
  /-- The result of SmartSelectLookup or auto-kiting when mouseAir=true.
  The caller must explicitly reject those modes unless it supplies their result. -/
  fallbackOverride : Option Nat := none
  deriving Repr

structure Output where
  state : State
  context : Context
  /-- Original Select calls PlaySound(12). Avoid duplicating an existing UI sound. -/
  menuTick : Bool := false
  /-- Original OnSelectionChanged was reached by Update. -/
  changed : Bool := false
  deriving BEq, Repr

/-- SelectedItemState.Select. The hotbar field changes immediately; the local
effective selected field changes later in Update. Remote selection is immediate. -/
def select (state : State) (input : Input) (slot : Nat) : Except String Output := do
  unless input.inventoryAir.size == 59 && slot < 59 do
    throw "SelectedItemState requires inventory0..58 and a valid selection"
  let state := if slot < 10 then {state with hotbar := slot} else state
  if !input.localPlayer then return {state := {state with selected := slot},context := input.context}
  if slot ≥ 10 && input.inventoryAir[slot]! then return {state,context := input.context}
  let (state,sound) := if state.selected != slot then
      ({state with buffered := some slot},state.overridden != some slot && state.buffered != some slot)
    else ({state with buffered := none,overridden := none},false)
  return {
    state,menuTick := sound
    context := {input.context with afkCounter := 0,afkCounterForKiting := 0}}

private def overrideSelection (state : State) (slot : Nat) : State :=
  if slot == state.selected then state else
    let previous := state.overridden.getD state.selected
    {state with selected := slot,overridden := if slot == previous then none else some previous}

/-- SelectedItemState.Update, called at Player.Update IL_1678. Call after hotbar
Select requests, before ResetEffects/normal Buff updates. This does not evaluate
the unrelated SmartSelect / auto-kite algorithms; their result is explicit input. -/
def update (state : State) (input : Input) : Except String Output := do
  unless input.inventoryAir.size == 59 && state.selected < 59 && state.hotbar < 10 &&
      input.fallbackOverride.all (· < 59) do
    throw "SelectedItemState requires valid native slots"
  if !input.localPlayer || !input.context.canChange then return {state,context := input.context}
  let old := state.selected
  let hadBuffered := state.hasBuffered
  let selected := state.buffered.getD (state.overridden.getD state.selected)
  unless selected < 59 do throw "SelectedItemState contains an invalid buffered/override slot"
  let selected := if selected ≥ 10 && input.inventoryAir[selected]! then state.hotbar else selected
  let state := {state with selected,buffered := none,overridden := none}
  let state := match (if input.mouseAir then input.fallbackOverride else some 58) with
    | some slot => overrideSelection state slot
    | none => state
  if old == state.selected then return {state,context := input.context}
  let mut context := {input.context with itemTime := 0,itemTimeMax := 0,lastItemUseAttemptSuccess := true}
  if context.controlUseItem then
    if hadBuffered && !state.hasOverride then context := {context with releaseUseItem := true}
    else if !context.releaseUseItem then context := {context with blockMouse := true,controlUseItem := false}
  return {state,context,changed := true}

/-- Convenience wrapper when a caller has one optional Select request before
Update in this frame. The two public operations are also available separately. -/
def frame (state : State) (input : Input) (requested : Option Nat := none) : Except String Output := do
  let before ← if let some slot := requested then select state input slot
    else pure {state,context := input.context}
  let after ← update before.state {input with context := before.context}
  return {after with menuTick := before.menuTick}

end Lean4Terria.OriginalSelectedItemState
