import Lean4Terria.OriginalPlacement

/-! Candle placement of desktop 1.4.5.8: Water Candle 148 → tile 49, Peace Candle 3117 → tile 372,
Shadow Candle 5322 → tile 646.

Hand translation of the path one `Player.ItemCheck` takes with one of these items selected:
`ItemCheck_AutoReuseLogic`, the start gate (IL_0451..0488) with `ItemCheck_StartActualUse` →
`ApplyItemAnimation`, the animation/time decrement (IL_071b..07e0), `ItemCheck_OwnerOnlyCode` →
`PlaceThing` → `PlaceThing_Tiles` → `PlaceThing_Tiles_TryPlacing` → `TileObject.CanPlace` →
`PlaceThing_Tiles_PlaceIt` → `TileObject.Place` + `WorldGen.SquareTileFrame`, and the consumable
branch at the end of `ItemCheck` (IL_188a..198e).

All three items share one set of defaults (read back from the runtime): useStyle 1,
useAnimation 15, useTime 10, autoReuse, consumable, placeStyle 0, tileBoost 0. All three tiles use
`TileObjectData.StyleOnTable1x1`: 1×1, origin (0, 0), bottom anchor `AnchorType.Table` on the one
cell below, no alternates, no random style, water placement allowed, lava placement not allowed.

The function draws no random number: none of the methods above reaches `Main.rand` for these tiles
(no `RandomStyleRange`, no flexible wand). `SoundEngine.PlaySound(0, x·16, y·16, 1)` is requested on
every placement; what the client's sound player does with it is outside this module. -/

namespace Lean4Terria.OriginalCandlePlacement

open OriginalCollision (Grid)
open OriginalItems (Stack)
open OriginalItemUse (Timing)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalTileRules (collision isCuttable isTable breakableInObjectPlacement)
open OriginalWorld (Terrain Tile TileShape)

/-- `Item.createTile` of the candle items. -/
def tileForItem : UInt16 → Option UInt16
  | 148 => some 49
  | 3117 => some 372
  | 5322 => some 646
  | _ => none

def isCandleItem (item : UInt16) : Bool := (tileForItem item).isSome

def isCandleTile (id : UInt16) : Bool := id == 49 || id == 372 || id == 646

/-- `Item.useAnimation` and `Item.useTime` of the three items, with `Player.tileSpeed = 1`. -/
def useAnimation : Nat := 15
def useTime : Nat := 10

/-- `Player.tileRangeX`, `Player.tileRangeY` (`DefaultTileRangeX/Y`) and `tileBoost + blockRange`
as `PlaceThing_Tiles` passes them to `IsInTileInteractionRange(…, TileReachCheckSettings.Simple, …)`. -/
def inReach (body : Body) (cell : Cell) : Bool := inToolRange body cell 5 3 0

/-- Inputs at `PlaceThing` / `PlaceThing_Tiles`, after `ItemCheck` has turned an
empty, finished selected item to air (IL_0783..07a3). `target` is the raw tile
target, even when UI input filtering prevents an actual use. The ordinary
unbuffed mouse path has tileBoost/blockRange zero, as in `advance` below. -/
structure CursorIconInput where
  body : Body
  item : UInt16
  target : Option Cell
  /-- The actual `ItemCheck_OwnerOnlyCode` call was reached. The caller owns
  the local-player/CCed/JustDroppedAnItem/IsAllowedToHoldItems/mount gates. -/
  ownerCodeReached : Bool := true
  noBuilding : Bool := false
  /-- The actual `SmartCursorHelper.TileTargetDesired()` result. -/
  tileTargetDesired : Bool := true

/-- Whether this candle's placement caller writes `cursorItemIconEnabled = true`
(`PlaceThing_Tiles` IL_004f). This is a write request: `false` preserves existing
UI/hover state, and `true` only changes Enabled. ID, Text, Push and Reversed are
not written. It precedes all placement-success, occupied-cell, liquid, item-time,
animation and use-button checks, so none of those are gates here. -/
def enableCursorIcon (input : CursorIconInput) : Bool :=
  input.ownerCodeReached && !input.noBuilding && isCandleItem input.item &&
    input.target.any (inReach input.body) && input.tileTargetDesired

/-- `TileID.Sets.BreakableWhenPlacing`. -/
private def breakableWhenPlacing (id : UInt16) : Bool :=
  (#[324, 186, 187, 185, 165, 530, 233, 227, 485, 81, 624] : Array UInt16).contains id

/-- `Player.PlaceThing_IsReplaceableBlock`: the only active tiles `PlaceThing_Tiles` lets a candle
target (IL_00f3). Note the cut exclusions differ from `TileObject.CanPlace` (711 here, 654 there). -/
def replaceableTarget (id : UInt16) : Bool :=
  (isCuttable id && id != 484 && id != 711) || (373 ≤ id && id ≤ 375) || id == 461 || id == 709 ||
    breakableWhenPlacing id

/-- `TileObjectData.LiquidPlace(Tile)` for the candle data: `WaterPlacement = Allowed` covers water,
honey and shimmer, `LavaPlacement = NotAllowed` rejects any lava amount above zero. -/
def liquidAllows (tile : Tile) : Bool :=
  !tile.liquid.any fun liquid => liquid.amount > 0 && liquid.kind == .lava

/-- The object cell of `TileObject.CanPlace` (IL_0565..064a): an active tile blocks unless it is
cut on placement, and the liquid rule must hold. Walls are not read (`AnchorWall` is false). -/
def cellAccepts (tile : Tile) : Bool :=
  tile.id.all breakableInObjectPlacement && liquidAllows tile

/-- `WorldGen.PlatformProperTopFrame(frameX)`. -/
def platformProperTopFrame (frameX : Int) : Bool :=
  let frame := frameX.tdiv 18
  (0 ≤ frame && frame ≤ 7) || (12 ≤ frame && frame ≤ 16) || frame == 25 || frame == 26

/-- The bottom anchor of `TileObject.CanPlace` for `AnchorType.Table` (IL_06c5..07da): the cell
below must be `nactive` and one of
* a platform (`TileID.Sets.Platforms`) that is not a half brick and shows a flat top frame;
* any other `tileSolid && tileSolidTop` tile;
* a `Main.tileTable` tile with `blockType() == 0`. -/
def tableAnchor (below : Tile) : Bool := Id.run do
  let some id := below.id | return false
  if below.actuated then return false
  let rules := collision id
  if rules.platform then
    return below.shape != .halfBlock && platformProperTopFrame (below.frame.getD (0, 0)).1
  if rules.solid && rules.solidTop then return true
  return isTable id && below.shape == .full

/-- `TileObject.CanPlace(x, y, candle, 0, direction, out data, onlyCheck: false)`. The direction is
not read: the data has `Direction = None` and no alternates. -/
def canPlace (grid : Grid) (cell : Cell) : Bool :=
  5 ≤ cell.x && cell.x + 1 + 5 ≤ grid.width && 5 ≤ cell.y && cell.y + 1 + 5 ≤ grid.height &&
    cellAccepts (grid.tileAt cell.x cell.y) && tableAnchor (grid.tileAt cell.x (cell.y + 1))

private def blockType (tile : Tile) : Nat :=
  match tile.shape with
  | .full => 0
  | .halfBlock => 1
  | .slope id => id.toNat + 1

/-- `WorldGen.CheckOnTable1x1(x, y, type)` as `TileFrame` runs it for tiles 49, 372 and 646: `true`
when the candle stays, `false` when the original calls `KillTile(x, y)`. It runs on the candle itself
right after placement (`SquareTileFrame`) and whenever a neighbour is framed, so it is also the rule
for a candle whose support is removed or reshaped. -/
def staysOnSupport (grid : Grid) (cell : Cell) : Bool := Id.run do
  let below := grid.tileAt cell.x (cell.y + 1)
  let platformAt (x : Nat) : Bool :=
    let tile := grid.tileAt x (cell.y + 1)
    tile.id.any (fun id => (collision id).platform) && blockType tile == 0
  let topSlope := below.shape == .slope 1 || below.shape == .slope 2
  if topSlope then
    if !below.id.any (fun id => (collision id).platform) then return false
    -- A sloped platform keeps the candle only next to a flat platform on its low side.
    if blockType below == 3 && platformAt (cell.x - 1) then return true
    if blockType below == 2 && platformAt (cell.x + 1) then return true
    return false
  if below.shape == .halfBlock then return false
  -- WorldGen.AnchorValid(tile, AnchorType.Table) || (tile.active() && Main.tileTable[tile.type])
  let some id := below.id | return false
  let rules := collision id
  let anchorValid :=
    !below.actuated &&
      (if rules.platform then platformProperTopFrame (below.frame.getD (0, 0)).1
       else (rules.solid && rules.solidTop) || isTable id)
  return anchorValid || isTable id

/-- `SoundEngine.PlaySound(0, tileTargetX * 16, tileTargetY * 16, 1, 1f, 0f)` of
`PlaceThing_Tiles_PlaceIt` (IL_0052..006e). -/
structure Sound where
  type : Nat := 0
  x : Int
  y : Int
  style : Int := 1
  deriving BEq, Repr

structure Result where
  terrain : Terrain
  timing : Timing
  stack : Option Stack
  /-- The cell that received the candle in this update. -/
  placed : Option Cell := none
  /-- `Main.blockMouse` is only written by the gamepad branch of `PlaceThing_Tiles_PlaceIt`
  (IL_01b0..01e6); the consumable branch of `ItemCheck` no longer writes it. Always `false` here. -/
  blockMouse : Bool := false
  sound : Option Sound := none
  /-- A branch the original takes that this module does not reproduce. The other fields then hold
  the state before that branch. -/
  boundary : Option String := none

private def edit (terrain : Terrain) (cell : Cell) (f : Tile → Tile) : Terrain := Id.run do
  let some tile := terrain.tile? cell.x cell.y | return terrain
  return (terrain.replaceTile cell.x cell.y (f tile)).getD terrain

/-- One `ItemCheck` with a candle stack selected, for a dry-handed, unbuffed, mouse-and-keyboard
single player (`tileSpeed = 1`, `blockRange = 0`, `noBuilding = false`, no smart cursor).

`held` is `Player.controlUseItem` as `ItemCheck` sees it; `target` is `Player.tileTargetX/Y`.
`stack.count = 0` is the emptied stack that `ItemCheck` keeps until the animation ends. -/
def advance (terrain : Terrain) (body : Body) (timing : Timing) (stack : Stack) (held : Bool)
    (target : Option Cell) : Result := Id.run do
  -- AutoReuseLogic + start gate + ApplyItemAnimation, then both decrements.
  let timing := timing.advance held useAnimation (stack.count != 0)
  -- IL_0783..07a3: the emptied stack turns to air once the animation is over.
  let remaining := if stack.count == 0 && timing.itemAnimation == 0 then none else some stack
  let idle : Result := { terrain, timing, stack := remaining }
  let some tileId := tileForItem stack.item | return idle
  let some cell := target | return idle
  -- PlaceThing_Tiles IL_001c..003e: reach first.
  if !inReach body cell then return idle
  let grid := Grid.ofTerrain terrain collision
  let tile := grid.tileAt cell.x cell.y
  -- IL_00d6..013b: an occupied target must be a replaceable block; an empty one must not hold lava.
  match tile.id with
  | some id => if !replaceableTarget id then return idle
  | none => if !liquidAllows tile then return idle
  -- IL_013d..0154.
  if !held || stack.count == 0 || timing.itemAnimation == 0 || timing.itemTime != 0 then return idle
  if !canPlace grid cell then return idle
  -- TileObject.Place IL_0155..019e first kills a cut tile in the cell (drops, dust, random draws).
  if tile.id.isSome then
    return { idle with boundary := some "candle placement over a tile that TileObject.Place cuts first" }
  -- TileObject.Place IL_0243..0269: active, frameX, frameY, type. Style 0 has frame (0, 0).
  let placed := edit terrain cell fun tile =>
    { OriginalMining.clearGround tile with id := some tileId, frame := some (0, 0) }
  -- SquareTileFrame(x, y): the candle's own CheckOnTable1x1, then the neighbours.
  if !staysOnSupport (Grid.ofTerrain placed collision) cell then
    return { idle with boundary := some "placed candle removed at once by WorldGen.CheckOnTable1x1" }
  let placed := OriginalPlatformFraming.squarePlatformFrame placed cell.x cell.y
  -- ApplyItemTime(item, tileSpeed), then the consumable branch: the last candle ends its item time
  -- together with the animation.
  let stack := { stack with count := stack.count - 1 }
  let timing := timing.setTime useTime
  let timing := if stack.count == 0 then { timing with itemTime := timing.itemAnimation } else timing
  return { terrain := placed, timing, stack := some stack, placed := some cell
           sound := some { x := (cell.x : Int) * 16, y := (cell.y : Int) * 16 } }

/-- `WorldGen.KillTile_GetItemDrops` for the three tiles: the item a broken candle drops. -/
def dropForTile : UInt16 → Option UInt16
  | 49 => some 148
  | 372 => some 3117
  | 646 => some 5322
  | _ => none

end Lean4Terria.OriginalCandlePlacement
