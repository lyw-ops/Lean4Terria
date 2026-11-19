import Lean4Terria.OriginalPlayerCollision

/-! Local candle interactions and ordinary workbench hover, Terraria 1.4.5.8.
Run `look` at Player.Update's LookForTileInteractions phase, after GrabItems
and before ItemCheck. Run `endUpdate` at UpdateReleaseUseTile, after ItemCheck.
The caller owns CopyInto/dropItemCheck/ResetEffects and supplies their results.
Native evidence and boundaries: `.lake/original-candle-interaction/REPORT.md`
and `.lake/original-workbench-hover/REPORT.md`. -/
namespace Lean4Terria.OriginalCandleInteraction

open OriginalPlayerCollision (Body)

structure Point where
  x : Int
  y : Int
  deriving BEq, Repr, Inhabited

structure Tile where
  active : Bool := false
  type : Nat := 0
  frameX : Int := 0
  deriving BEq, Repr, Inhabited

def itemForTile : Nat → Option Int
  | 49 => some 148
  | 372 => some 3117
  | 646 => some 5322
  | _ => none

/-- Hover is wider than the candle-use whitelist. The checked ordinary workbench
style0 cells have frameX0/18; other workbench styles/frames are not covered. -/
def hoverItemForTile (tile : Tile) : Option Int :=
  if tile.type == 18 && (tile.frameX == 0 || tile.frameX == 18) then some 36
  else itemForTile tile.type

structure State where
  releaseUseTile : Bool := false
  tileInteractAttempted : Bool := false
  tileInteractionHappened : Bool := false
  noThrow : Int := 0
  cursorItemIconEnabled : Bool := false
  cursorItemIconID : Int := 0
  cursorItemIconText : String := ""
  cursorItemIconPush : Int := 0
  quickGrappleCooldown : Int := 0
  lockTileInteractionsTimer : Int := 0
  deriving BEq, Repr, Inhabited

structure Input where
  body : Body
  tileTarget : Point
  tileAt : Point → Option Tile
  controlUseTile : Bool := false
  localPlayer : Bool := true
  netMode : Int := 0
  mapFullscreen : Bool := false
  bestiaryOpen : Bool := false
  paused : Bool := false
  usingGamepad : Bool := false
  hoveringOverNPC : Bool := false
  smartInteractShowingGenuine : Bool := false
  smartInteractNPC : Int := -1
  smartInteractProj : Int := -1
  smartInteractTile : Point := ⟨0, 0⟩
  currentNPCShowingChatBubble : Int := -1
  talkNPC : Int := -1
  npcChatRelease : Bool := true
  hasInteractableObjectThatIsNotATile : Bool := false
  wiresUIOpen : Bool := false
  ownedProjectile651 : Int := 0
  tileRangeX : Int := 5
  tileRangeY : Int := 3

structure Edit where
  point : Point
  before : Tile
  after : Tile
  deriving BEq, Repr, Inhabited

structure Output where
  state : State
  edit : Option Edit := none
  deriving BEq, Repr, Inhabited

/-- Player.Update IL_0267..0295, before the dead guard. Only the local player
updates this cooldown; a fullscreen map renews3 instead of decrementing it. -/
def beginUpdate (state : State) (localPlayer : Bool := true) (mapFullscreen : Bool := false) : State :=
  if !localPlayer then state else
  { state with quickGrappleCooldown := if mapFullscreen then 3 else
      if state.quickGrappleCooldown > 0 then state.quickGrappleCooldown - 1 else state.quickGrappleCooldown }

/-- Player fields cleared by Main.MouseOversClear (normal DoUpdate IL_05ab,
before UpdateUIStates), rather than by a generic draw-entry reset.
NoThrow, release and interaction flags are deliberately preserved. -/
def clearMouseOver (state : State) : State :=
  { state with cursorItemIconEnabled := false, cursorItemIconID := 0
               cursorItemIconText := "", cursorItemIconPush := 0 }

/-- TileReachCheckSettings.Simple (multiplier1, limit20, TB0).
Range is based on the full player box; upper boundaries use Ceiling after
the Float32 position+size operation. Both edges are inclusive. -/
def inReach (body : Body) (point : Point) (rangeX : Int := 5) (rangeY : Int := 3) : Bool :=
  let rx := min rangeX 20
  let ry := min rangeY 20
  let left := (body.position.x / 16).toInt32.toInt - rx
  let top := (body.position.y / 16).toInt32.toInt - ry
  let right := ((body.position.x + Float32.ofNat body.width) / 16).toFloat.ceil.toInt64.toInt - 1 + rx
  let bottom := ((body.position.y + Float32.ofNat body.height) / 16).toFloat.ceil.toInt64.toInt - 1 + ry
  left ≤ point.x && point.x ≤ right && top ≤ point.y && point.y ≤ bottom

/-- TileInteractionsMouseOver's candles and ordinary workbench. Smart selection may
use another tile, but hover only applies at the actual tileTarget. -/
def hover (state : State) (tile : Tile) (atActualTarget : Bool) : State :=
  if !tile.active || !atActualTarget then state else
  match hoverItemForTile tile with
  | none => state
  | some item => { state with noThrow := 2, cursorItemIconEnabled := true, cursorItemIconID := item }

/-- Native signed-short addition, not snapping arbitrary frames to0/18. -/
def toggleFrame (frameX : Int) : Int :=
  (Int16.ofInt (frameX + if frameX > 0 then -18 else 18)).toInt

/-- LookForTileInteractions → active TileInteractionsCheck → hover, then use.
The candle leaf requests SendTileSquare, sets interactionHappened and the
grapple cooldown3. It has no sound request and draws no random number.
Other tile types and non-tile interactions remain the shared caller's work. -/
def look (state : State) (input : Input) : Output := Id.run do
  if input.netMode == 2 || !input.localPlayer || input.mapFullscreen || input.bestiaryOpen then
    return { state }
  let point := if input.smartInteractShowingGenuine && input.smartInteractNPC == -1 && input.smartInteractProj == -1
    then input.smartInteractTile else input.tileTarget
  let attempted := input.controlUseTile && !input.paused && !(input.usingGamepad && input.hoveringOverNPC)
  let mut state := { state with
    tileInteractionHappened := if state.releaseUseTile then false else state.tileInteractionHappened
    tileInteractAttempted := attempted }
  if attempted && state.releaseUseTile then
    if (input.currentNPCShowingChatBubble > -1 && (input.talkNPC == -1 || !input.npcChatRelease)) ||
        input.hasInteractableObjectThatIsNotATile then
      state := { state with releaseUseTile := false }
  if !inReach input.body point input.tileRangeX input.tileRangeY then return { state }
  let some tile := input.tileAt point | return { state }
  if !tile.active then return { state }
  state := hover state tile (point == input.tileTarget)
  if point == input.tileTarget && input.usingGamepad && state.cursorItemIconText.isEmpty then
    state := { state with cursorItemIconEnabled := false, cursorItemIconID := 0 }
  if input.wiresUIOpen || input.ownedProjectile651 > 0 || !state.tileInteractAttempted || !state.releaseUseTile then
    return { state }
  if (itemForTile tile.type).isNone then return { state }
  let after := { tile with frameX := toggleFrame tile.frameX }
  return {
    state := { state with tileInteractionHappened := true, quickGrappleCooldown := 3 }
    edit := some { point, before := tile, after } }

/-- UpdateReleaseUseTile runs near the end of Player.Update, after ItemCheck.
Mouse-interface prevents rearming. A positive lock timer preserves an already
false release flag; the timer is decremented only after computing the flag. -/
def endUpdate (state : State) (mouseInterface : Bool) : State :=
  let released := !state.tileInteractAttempted &&
    !(state.lockTileInteractionsTimer > 0 && !state.releaseUseTile) && !mouseInterface
  { state with
    releaseUseTile := released
    lockTileInteractionsTimer := if state.lockTileInteractionsTimer > 0 then state.lockTileInteractionsTimer - 1
      else state.lockTileInteractionsTimer }

end Lean4Terria.OriginalCandleInteraction
