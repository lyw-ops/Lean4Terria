import Lean4Terria.OriginalBanks
import Lean4Terria.OriginalPlatformFraming

namespace Lean4Terria.OriginalPlacement

open OriginalCollision (Grid)
open OriginalItems (Stack)
open OriginalItemUse (Timing)
open OriginalMining (Cell HitCache inToolRange)
open OriginalPlayerCollision (Body)
open OriginalTileRules (collision isCuttable isTreeTrunk supportsBlockPlacement)
open OriginalWorld (Terrain Tile TileShape)

private def isShortGrass (id : UInt16) : Bool :=
  id == 3 || id == 73

private def hasSupport (grid : Grid) (cell : Cell) : Bool :=
  (grid.tileAt cell.x cell.y).wall != 0 ||
    [grid.tileAt (cell.x - 1) cell.y, grid.tileAt (cell.x + 1) cell.y,
      grid.tileAt cell.x (cell.y - 1), grid.tileAt cell.x (cell.y + 1)].any
        fun tile => tile.wall != 0 || tile.id.any supportsBlockPlacement

private def overlapsBody (body : Body) (cell : Cell) : Bool :=
  let playerX := body.position.x.toInt32.toInt
  let playerY := body.position.y.toInt32.toInt
  let x : Int := (cell.x : Int) * 16
  let y : Int := (cell.y : Int) * 16
  x < playerX + body.width && x + 16 > playerX &&
    y < playerY + body.height && y + 16 > playerY

private def edit (terrain : Terrain) (cell : Cell) (f : Tile → Tile) : Terrain := Id.run do
  let some tile := terrain.tile? cell.x cell.y | return terrain
  return (terrain.replaceTile cell.x cell.y (f tile)).getD terrain

private def flatten (terrain : Terrain) (cell : Cell) (below : Bool) : Terrain :=
  edit terrain cell fun tile =>
    let mustFlatten := match tile.shape with
      | .halfBlock => below
      | .slope slope => if below then slope == 1 || slope == 2 else slope == 3 || slope == 4
      | .full => false
    if mustFlatten && tile.id.any (fun id => !(collision id).platform) then
      { tile with shape := .full }
    else tile

private def solidFull (grid : Grid) (x y : Nat) : Bool :=
  let tile := grid.tileAt x y
  !tile.actuated && tile.shape == .full && tile.id.any fun id =>
    let rules := grid.rules id
    rules.solid && !rules.solidTop

private def workbenchSupport (grid : Grid) (cell : Cell) : Bool :=
  let tile := grid.tileAt cell.x (cell.y + 1)
  !tile.actuated && tile.shape == .full && tile.id.any fun id =>
    let rules := grid.rules id
    rules.solid || rules.solidTop

private def cutRockPile (terrain : Terrain) (cell : Cell) : Terrain := Id.run do
  let tile := (terrain.tile? cell.x cell.y).getD default
  if tile.id != some 187 then return terrain
  let some (fx, fy) := tile.frame | return terrain
  let left := cell.x - (fx / 18 % 3).toNat
  let top := cell.y - (fy / 18 % 2).toNat
  let styleX := fx - (fx % 54)
  let styleY := fy - (fy % 36)
  let mut terrain := terrain
  for dx in [:3] do
    for dy in [:2] do
      terrain := edit terrain ⟨left + dx, top + dy⟩ fun part =>
        if part.id == some 187 &&
            part.frame == some (styleX + (dx : Int) * 18, styleY + (dy : Int) * 18)
        then OriginalMining.clearGround part else part
  return terrain

private def removeBuriedGrass (terrain : Terrain) (cell : Cell) : Terrain := Id.run do
  let mut terrain := terrain
  for x in [cell.x - 1:cell.x + 2] do
    for y in [cell.y - 1:cell.y + 2] do
      let tile := (terrain.tile? x y).getD default
      let some id := tile.id | continue
      if !(#[2, 23, 60, 70, 109, 199, 477, 492, 633] : Array UInt16).contains id then continue
      let grid := Grid.ofTerrain terrain collision
      let covered := [x - 1, x, x + 1].all fun nx =>
        [y - 1, y, y + 1].all fun ny => solidFull grid nx ny
      if covered then
        terrain := edit terrain ⟨x, y⟩ fun tile => { tile with id := OriginalMining.exposedGround id }
  return terrain

structure Result where
  terrain : Terrain
  timing : Timing
  stack : Option Stack
  swapHits : HitCache
  drop : Option UInt16 := none

private def Result.consumeBlock (result : Result) (stack : Stack) (time : Nat) : Result :=
  let stack := { stack with count := stack.count - 1 }
  let timing := result.timing.setTime time
  let timing := if stack.count == 0 then { timing with itemTime := timing.itemAnimation } else timing
  { result with timing, stack := some stack }

private def canSwapSupport (grid : Grid) (cell : Cell) : Bool :=
  let tile := grid.tileAt cell.x cell.y
  let upper := grid.tileAt cell.x (cell.y - 1)
  if tile.wall == 350 then false
  else match upper.id with
    | none => true
    | some id =>
      let (fx, fy) := upper.frame.getD (-1, -1)
      if id == 323 then fx != 66 && fx != 220
      else if isTreeTrunk id then
        (fx == 66 && 0 ≤ fy && fy ≤ 44) || (fx == 88 && 66 ≤ fy && fy ≤ 110) || fy ≥ 198
      else id != 80 && id != 488

private def swapBlock (result : Result) (grid : Grid) (cell : Cell) (stack : Stack)
    (oldId newId : UInt16) : Result := Id.run do
  if oldId == newId || oldId == 18 || oldId == 19 || oldId == 58 || oldId == 53 ||
      !canSwapSupport grid cell then return result
  let sourceId := (OriginalMining.exposedGround oldId).getD oldId
  let some drop := OriginalMining.itemForGround sourceId | return result
  if drop == stack.item then return result
  let old := grid.tileAt cell.x cell.y
  let damage := OriginalMining.pickDamage grid { surface := result.terrain.header.surface } cell old 35
  if damage == 0 then return result
  let swapHits := result.swapHits.record cell damage
  let timing := { result.timing with itemAnimation := 15, itemAnimationMax := 15 }
  if swapHits.damage cell < 100 then return { result with swapHits, timing := timing.setTime 15 }
  let terrain := edit result.terrain ⟨cell.x, cell.y - 1⟩ fun tile =>
    if tile.id.any isShortGrass then OriginalMining.clearGround tile else tile
  let terrain := edit terrain cell fun tile =>
    { tile with id := some newId, frame := none, tilePaint := 0
                invisibleBlock := false, fullbrightBlock := false }
  return ({ result with terrain, timing, swapHits := (swapHits.forget cell).prune grid, drop := some drop
           } : Result).consumeBlock stack 25

/-- The wooden work bench has its origin in the left of its two cells. Both
anchors must be valid before placing either half of the object. -/
private def placeWorkbench (result : Result) (grid : Grid) (cell : Cell) (stack : Stack) :
    Result := Id.run do
  let right : Cell := ⟨cell.x + 1, cell.y⟩
  let accepts (target : Cell) :=
    let tile := grid.tileAt target.x target.y
    tile.id.all (fun id => isCuttable id || id == 187) && tile.liquid.all (·.kind == .water) &&
      workbenchSupport grid target
  if !(accepts cell && accepts right) then return result
  let putHalf (terrain : Terrain) (target : Cell) (x : Int) :=
    edit terrain target fun tile => {
      OriginalMining.clearGround tile with id := some 18, frame := some (x, 0)
    }
  let terrain := cutRockPile (cutRockPile result.terrain cell) right
  let terrain := putHalf terrain cell 0
  let terrain := putHalf terrain right 18
  return ({ result with terrain } : Result).consumeBlock stack 10

private def bankAnchor (kind : OriginalBanks.Kind) (tile : Tile) : Bool := Id.run do
  let some id := tile.id | return false
  if tile.actuated then return false
  let table := kind != .defendersForge
  let solid := kind != .piggyBank
  let rules := collision id
  if rules.platform then
    let frame := ((tile.frame.getD (0, 0)).1).tdiv 18
    return tile.shape != .halfBlock &&
      ((0 ≤ frame && frame ≤ 7) || (12 ≤ frame && frame ≤ 16) || frame == 25 || frame == 26)
  if rules.solid && rules.solidTop then return true
  if solid && rules.solid then
    if tile.shape == .full && !OriginalTileRules.preventsSolidAnchor id then return true
    if tile.shape == .slope 3 || tile.shape == .slope 4 then return true
  return table && tile.shape == .full && OriginalTileRules.isTable id

private def placeBank (result : Result) (grid : Grid) (target : Cell) (stack : Stack)
    (kind : OriginalBanks.Kind) : Result := Id.run do
  let (width, height, originX) := kind.footprint
  if target.x < originX || target.y + 1 < height then return result
  let left := target.x - originX
  let top := target.y + 1 - height
  if left + width > grid.width || target.y + 1 ≥ grid.height then return result
  for x in [left:left + width] do
    if !bankAnchor kind (grid.tileAt x (target.y + 1)) then return result
    for y in [top:top + height] do
      let tile := grid.tileAt x y
      if tile.liquid.any (·.kind == .lava) then return result
      if tile.id.any (fun id => !OriginalTileRules.breakableInObjectPlacement id) then
        return result
  let mut terrain := result.terrain
  for dx in [:width] do
    for dy in [:height] do
      let cell : Cell := ⟨left + dx, top + dy⟩
      terrain := cutRockPile terrain cell
      let frame := some (18 * Int.ofNat dx, 18 * Int.ofNat dy)
      terrain := edit terrain cell fun tile => {
        OriginalMining.clearGround tile with id := some kind.tileId, frame }
  return ({ result with terrain } : Result).consumeBlock stack 10

private inductive PlatformNeighbor where
  | air
  | platform
  | solid

private def platformNeighbor (tile : Tile) : PlatformNeighbor :=
  if tile.actuated then .air
  else if tile.id == some 19 then .platform
  else if tile.id.any (fun id => (collision id).solid && !(collision id).solidTop) then .solid
  else .air

/-- Flat wooden platform atlas row. The lower diagonals are used only by
the sloped platform paths in vanilla's framing. -/
def flatPlatformFrame (left right : Tile) : Int :=
  match platformNeighbor left, platformNeighbor right with
  | .platform, .platform => 0
  | .platform, .air => 18
  | .air, .platform => 36
  | .solid, .platform => 54
  | .platform, .solid => 72
  | .solid, .air => 108
  | .air, .solid => 126
  | _, _ => 90

private def platformHasSupport (grid : Grid) (cell : Cell) : Bool :=
  (grid.tileAt cell.x cell.y).wall != 0 ||
    [cell.x - 1, cell.x, cell.x + 1].any fun x =>
      [cell.y - 1, cell.y, cell.y + 1].any fun y =>
        (x != cell.x || y != cell.y) && (grid.tileAt x y).id.isSome

private def placePlatform (result : Result) (grid : Grid) (cell : Cell) (stack : Stack) :
    Result := Id.run do
  let tile := grid.tileAt cell.x cell.y
  if !tile.id.all isCuttable || !platformHasSupport grid cell then return result
  let terrain := edit result.terrain cell fun tile => {
    OriginalMining.clearGround tile with id := some 19, frame := some (90, 0)
  }
  let terrain := OriginalPlatformFraming.squarePlatformFrame terrain cell.x cell.y
  return ({ result with terrain } : Result).consumeBlock stack 10

/-- Update the platforms whose connections can change when a tile is removed. -/
def reframeAfterRemoval (terrain : Terrain) (cell : Cell) : Terrain :=
  OriginalPlatformFraming.squarePlatformFrame terrain cell.x cell.y

/-- Place a stable terrain block using a dry, unbuffed single-player
character. Ordinary short grass can be cut before the body-overlap test. -/
def advance (terrain : Terrain) (body : Body) (timing : Timing) (stack : Stack)
    (tileId : UInt16) (held : Bool) (target : Option Cell)
    (hasCopperPick := false) (swapHits : HitCache := {}) : Result := Id.run do
  let timing := timing.advance held 15 (stack.count != 0)
  let remaining := if stack.count == 0 && timing.itemAnimation == 0 then none else some stack
  let idle : Result := { terrain, timing, stack := remaining, swapHits }
  let some cell := target | return idle
  if !held || stack.count == 0 || timing.itemAnimation == 0 || timing.itemTime != 0 then return idle
  let grid := Grid.ofTerrain terrain collision
  if cell.x == 0 || cell.y == 0 || cell.x + 1 ≥ grid.width || cell.y + 1 ≥ grid.height ||
      !inToolRange body cell 5 3 0 then return idle
  if tileId == 18 then return placeWorkbench idle grid cell stack
  if tileId == 19 then return placePlatform idle grid cell stack
  if let some kind := OriginalBanks.kindForTile tileId then return placeBank idle grid cell stack kind
  let tile := grid.tileAt cell.x cell.y
  if let some id := tile.id then
    if !isShortGrass id then return if hasCopperPick then swapBlock idle grid cell stack id tileId
      else idle
  if !hasSupport grid cell then return idle
  let terrain := if tile.id.isSome then edit terrain cell OriginalMining.clearGround else terrain
  if overlapsBody body cell then return { idle with terrain }
  let terrain := edit terrain cell fun tile =>
    { tile with id := some tileId, frame := none, shape := .full }
  let terrain := flatten terrain ⟨cell.x, cell.y + 1⟩ true
  let terrain := flatten terrain ⟨cell.x, cell.y - 1⟩ false
  let terrain := OriginalPlatformFraming.squarePlatformFrame (removeBuriedGrass terrain cell) cell.x cell.y
  return ({ idle with terrain } : Result).consumeBlock stack 10

end Lean4Terria.OriginalPlacement
