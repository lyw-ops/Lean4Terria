import Lean4Terria.OriginalCandlePlacement

/-! Hovering and right-clicking a placed candle in desktop 1.4.5.8: Water Candle tile 49, Peace
Candle tile 372, Shadow Candle tile 646.

Hand translation of what one local `Player.Update` does for these tiles in
`Player.LookForTileInteractions` (called at `Update` IL_6b34, after `GrabItems` and before the
NPC collision, the movement and `ItemCheck`) → `TileInteractionsCheck` →
`TileInteractionsMouseOver` (IL_121f..12af) and `TileInteractionsUse` (IL_0c49..0ce3), together
with `Player.UpdateReleaseUseTile` (`Update` IL_8ebe, after `ItemCheck`).

The toggle only rewrites `frameX` and calls `NetMessage.SendTileSquare`, which returns at once in
single player. It plays no sound, frames no tile, hits no wire and draws no random number. Nothing
in this path reads the item animation, the inventory state or `Main.mouseRightRelease`.

Outside this module: smart interact (`Main.SmartInteractShowingGenuine`), a gamepad, the wire
menu (`WiresUI.Open`), a Grand Design kite (`ownedProjectileCounts[651]`), `gamePaused`, the full
screen map, `_lockTileInteractionsTimer`, minecart tracks near the cursor
(`TryInteractingWithMinecartTrackInNearbyArea`) and every other interactable tile. -/

namespace Lean4Terria.OriginalCandleToggle

open OriginalCandlePlacement (dropForTile)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Terrain)

/-- Player fields the tile interaction keeps from one update to the next. -/
structure State where
  /-- `Player.releaseUseTile` as `UpdateReleaseUseTile` left it. A new `Player` starts with false. -/
  releaseUseTile : Bool := false
  /-- `Player.tileInteractionHappened`. -/
  tileInteractionHappened : Bool := false
  /-- `Player.cursorItemIconEnabled` with `cursorItemIconID`, as a hovered candle wrote them in the
  latest update. `Main.MouseOversClear` (`DoUpdate` IL_05ab) clears both before every update. -/
  cursorItemIcon : Option UInt16 := none
  deriving BEq, Repr, Inhabited

structure Input where
  /-- `Player.tileTargetX/Y` (`Update` IL_1c1b..1cd1): the cell under the cursor, also while the
  cursor is over an interface. -/
  target : Option Cell
  /-- `Player.controlUseTile`: the right button, already cleared for a crowd-controlled player
  (`Update` IL_17a1). -/
  controlUseTile : Bool
  /-- `Player.mouseInterface`, as the preceding draw left it. -/
  mouseInterface : Bool := false
  /-- `LookForTileInteractions` IL_00b3..00ee: an NPC shows its chat bubble
  (`currentNPCShowingChatBubble > -1`) while `talkNPC == -1 || !Main.npcChatRelease`, or
  `Main.HasInteractableObjectThatIsNotATile`. The press then belongs to that object. -/
  otherInteraction : Bool := false

structure Result where
  /-- With the candle's `frameX` rewritten when `toggled` is set. -/
  terrain : Terrain
  state : State
  /-- `Player.noThrow = 2`, written whenever a candle is hovered. -/
  noThrow : Option Nat := none
  /-- The candle `TileInteractionsUse` toggled in this update. -/
  toggled : Option Cell := none

/-- `IsInTileInteractionRange(x, y, TileReachCheckSettings.Simple, 0)` with `Player.tileRangeX/Y`
5 and 3 (`LookForTileInteractions` IL_00f3..0101). -/
def inRange (body : Body) (cell : Cell) : Bool := inToolRange body cell 5 3 0

/-- `Player.cursorItemIconID` of `TileInteractionsMouseOver` IL_1230..12aa: 49 → 148, 372 → 3117,
646 → 5322, the same items the tiles drop. -/
def iconForTile (id : UInt16) : Option UInt16 := dropForTile id

/-- The hovered candle and its icon. `TileInteractionsCheck` (IL_001f..0030) requires `active()`,
which an actuated tile keeps. -/
def hovered (terrain : Terrain) (body : Body) (target : Option Cell) : Option (Cell × UInt16) := do
  let cell ← target
  guard (inRange body cell)
  let id ← (← terrain.tile? cell.x cell.y).id
  return (cell, ← iconForTile id)

/-- `TileInteractionsUse` IL_0ca6..0ccd: `frameX += frameX > 0 ? -18 : 18`, stored as `short`. -/
def toggledFrameX (frameX : Int) : Int :=
  (Int16.ofInt (if frameX > 0 then frameX - 18 else frameX + 18)).toInt

/-- `LookForTileInteractions` and `UpdateReleaseUseTile` of one update of a living local player. -/
def update (terrain : Terrain) (body : Body) (state : State) (input : Input) : Result := Id.run do
  -- IL_008d..009e.
  let happened := state.tileInteractionHappened && !state.releaseUseTile
  let attempted := input.controlUseTile
  -- IL_00a3..00ee.
  let release := state.releaseUseTile && !(attempted && input.otherInteraction)
  -- UpdateReleaseUseTile IL_0000..0029; nothing between the two calls writes its inputs.
  let releaseAfter := !attempted && !input.mouseInterface
  let idle : State := { releaseUseTile := releaseAfter, tileInteractionHappened := happened }
  let some (cell, icon) := hovered terrain body input.target | return { terrain, state := idle }
  -- TileInteractionsMouseOver: noThrow, cursorItemIconEnabled, cursorItemIconID.
  let over : State := { idle with cursorItemIcon := some icon }
  -- TileInteractionsUse IL_0018..0025 and IL_0204.
  if !(attempted && release) then return { terrain, state := over, noThrow := some 2 }
  let some tile := terrain.tile? cell.x cell.y | return { terrain, state := over, noThrow := some 2 }
  let (frameX, frameY) := tile.frame.getD (0, 0)
  let lit := { tile with frame := some (toggledFrameX frameX, frameY) }
  -- IL_0cdb and IL_2911..2916.
  return { terrain := (terrain.replaceTile cell.x cell.y lit).getD terrain
           state := { over with tileInteractionHappened := true }
           noThrow := some 2, toggled := some cell }

end Lean4Terria.OriginalCandleToggle
