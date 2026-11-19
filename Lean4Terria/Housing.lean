import Lean4Terria.Bedroom
import Std.Data.HashSet.Basic

namespace Lean4Terria

private def minOpenTiles : Nat := 9
private def maxRoomTiles : Nat := 120

private inductive RoomError where
  | blocked | unsealed | small | dark | unfurnished | noStandingTile
  deriving BEq

private def RoomError.message : RoomError → String
  | .blocked => "Leave space beside the door."
  | .unsealed => s!"Seal gaps in the walls, roof and floor. Room limit: {maxRoomTiles} tiles."
  | .small => s!"Make the room at least 3 tiles wide and high, with {minOpenTiles} open tiles."
  | .dark => "Place a torch inside the room."
  | .unfurnished => "Place a workbench or chest inside the room."
  | .noStandingTile => "Leave two clear tiles above a solid floor for the guide."

private def RoomError.enclosed : RoomError → Bool
  | .blocked | .unsealed => false
  | _ => true

private def Tile.isFurniture : Tile → Bool
  | .workbench | .chest => true
  | _ => false

private def Tile.isOpenSpace : Tile → Bool
  | .air | .torch | .platform | .homeBeacon | .supplyCache | .openedCache
  | .sunleaf | .sunleafSprout | .rail | .lifeCrystal | .highlandBerry | .mineMarker => true
  | _ => false

/-- A complete door is a room boundary in either orientation, and furniture occupies room space. -/
private def isRoomCell (world : World) (pos : TilePos) (tile : Tile) : Bool :=
  tile.isOpenSpace || tile.isFurniture ||
    (!tile.solid && tile.isDoor && (doorBase world pos).isNone)

private def roomCells (world : World) (start : TilePos) : Except RoomError (Array TilePos) := do
  let mut seen : Std.HashSet TilePos := ({} : Std.HashSet TilePos).insert start
  let mut cells := #[start]
  let mut cursor := 0
  while cursor < cells.size do
    let pos := cells[cursor]!
    cursor := cursor + 1
    let neighbors := [⟨pos.x - 1, pos.y⟩, ⟨pos.x + 1, pos.y⟩,
      ⟨pos.x, pos.y - 1⟩, ⟨pos.x, pos.y + 1⟩]
    for neighbor in neighbors do
      let some tile := world.tile? neighbor | throw .unsealed
      if isRoomCell world neighbor tile && !seen.contains neighbor then
        if cells.size == maxRoomTiles then throw .unsealed
        seen := seen.insert neighbor
        cells := cells.push neighbor
  return cells

private def guideCanStand (world : World) (center : TilePos) : Bool :=
  let floor := world.tile { center with y := center.y + 1 }
  (world.tile center).isOpenSpace &&
    (world.tile { center with y := center.y - 1 }).isOpenSpace &&
    floor.solid && !floor.isFurniture && !floor.isDoor

private def inspectSide (world : World) (door : TilePos) (direction : Int) :
    Except RoomError (TilePos × Array TilePos) := do
  let bottom : TilePos := ⟨door.x + direction, door.y⟩
  let top := { bottom with y := bottom.y - 1 }
  let some start := [bottom, top].find? (fun pos => isRoomCell world pos (world.tile pos))
    | throw .blocked
  let cells ← roomCells world start
  let mut openTiles := 0
  let mut minX := start.x
  let mut maxX := start.x
  let mut minY := start.y
  let mut maxY := start.y
  let mut lit := false
  let mut furnished := false
  for pos in cells do
    let tile := world.tile pos
    if tile.isOpenSpace then openTiles := openTiles + 1
    minX := min minX pos.x
    maxX := max maxX pos.x
    minY := min minY pos.y
    maxY := max maxY pos.y
    lit := lit || tile == .torch
    furnished := furnished || tile.isFurniture
  if openTiles < minOpenTiles || maxX - minX < 2 || maxY - minY < 2 then throw .small
  if !lit then throw .dark
  if !furnished then throw .unfurnished
  let standing := cells.find? (fun pos => (pos.x - door.x).natAbs ≥ 2 && guideCanStand world pos)
  let some position := standing.orElse (fun _ => cells.find? (guideCanStand world))
    | throw .noStandingTile
  return (position, cells)

private def inspectRoom (world : World) (door : TilePos) : Except String (TilePos × Array TilePos) := do
  let some base := doorBase world door | throw "Choose a complete two-tile door."
  match inspectSide world base (-1) with
  | .ok position => return position
  | .error left =>
    match inspectSide world base 1 with
    | .ok position => return position
    | .error right =>
      let best := if left.enclosed then left else if right.enclosed then right
        else if left == .unsealed then left else right
      throw best.message

/-- Find a stable center tile for the guide inside a valid room beside the given door. -/
def inspectHome (world : World) (door : TilePos) : Except String TilePos :=
  (inspectRoom world door).map Prod.fst

/-- Doors on opposite sides of the same room still identify one residence. -/
def sharesHome (world : World) (first second : TilePos) : Bool :=
  match inspectRoom world first, inspectRoom world second with
  | .ok (_, cells), .ok (position, _) => cells.contains position
  | _, _ => false

def inspectSeparateHome (world : World) (door : TilePos) (other : Option TilePos) :
    Except String TilePos := do
  let position ← inspectHome world door
  if other.any (sharesHome world door) then
    throw "A resident already lives in this room. Choose a separate room."
  return position

/-- Preserve a registered room's natural walls and floor when extending an older world. -/
def homeFootprint (world : World) (door : TilePos) : Std.HashSet TilePos := Id.run do
  let .ok (_, cells) := inspectRoom world door | return {}
  let mut result : Std.HashSet TilePos := {}
  for cell in cells do
    for position in [cell, ⟨cell.x - 1, cell.y⟩, ⟨cell.x + 1, cell.y⟩,
        ⟨cell.x, cell.y - 1⟩, ⟨cell.x, cell.y + 1⟩] do
      result := result.insert position
  return result

structure ClassicHome where
  entrance : TilePos
  stand : TilePos
  deriving BEq, Repr

def isClassicEntrance (world : World) (position : TilePos) : Bool :=
  world.tile position == .platform || doorBase world position == some position

private def classicHomeFloor (world : World) (position : TilePos) : Bool :=
  let tile := world.tile position
  (tile.solid || tile == .platform) && !tile.isDoor && !tile.isFurniture

private def homeScore (world : World) (floor : TilePos) : Option Int := Id.run do
  let nearColumns : List Int := [-1, 0, 1]
  if !nearColumns.all (fun dx => classicHomeFloor world ⟨floor.x + dx, floor.y⟩) then return none
  if nearColumns.any (fun dx => ([-3, -2, -1] : List Int).any fun dy =>
      (world.tile ⟨floor.x + dx, floor.y + dy⟩).solid) then return none
  let mut score : Int := 0
  let mut chests : Int := 0
  let mut above : Int := 0
  for dx in ([-2, -1, 0, 1, 2] : List Int) do
    for dy in ([-4, -3, -2, -1] : List Int) do
      let tile := world.tile ⟨floor.x + dx, floor.y + dy⟩
      if dx == 0 then
        if tile != .air then above := above + 1
      else
        if tile == .chest then chests := chests + 1
        score := score + (if tile.isDoor then -20 else if tile.solid then -5 else 5)
  if score ≤ 0 then return none
  return some (max 1 (score - 30 * chests - 15 * above))

private def earlierTile (left right : TilePos) : Bool :=
  left.x < right.x || (left.x == right.x && left.y < right.y)

private def inspectClassicInterior (world : World) (walls : ByteArray) (start : TilePos)
    (preferred : Option TilePos) : Except String ClassicHome := do
  let cells ← scanBackedRoom world walls start
  if !cells.any (fun pos => world.tile pos == .torch) then throw "The room is missing a light source."
  if !cells.any (fun pos => world.tile pos == .workbench || world.tile pos == .woodTable) then
    throw "The room is missing a table or work bench."
  if !cells.any (fun pos => world.tile pos == .woodChair || world.tile pos == .bed) then
    throw "The room is missing a chair or bed."
  let mut entrance := none
  let mut best : Option (TilePos × Int) := none
  for pos in cells do
    if isClassicEntrance world pos then
      if !(entrance.any fun current => earlierTile current pos) then entrance := some pos
    let stand := { pos with y := pos.y - 1 }
    if roomBoundary world stand || !cells.contains stand then continue
    if let some score := homeScore world pos then
      if !(best.any fun (current, previous) => previous > score || (previous == score && earlierTile current stand)) then
        best := some (stand, score)
  let some selectedEntrance := (preferred.filter (cells.contains ·)).orElse (fun _ => entrance)
    | throw "The room is missing an entrance. Use a door or platform."
  let some (stand, _) := best | throw "The room needs a clear place for a resident to stand."
  return { entrance := selectedEntrance, stand }

/-- Query an interior tile or either side of a door or platform. -/
def inspectClassicHome (world : World) (walls : ByteArray) (target : TilePos) :
    Except String ClassicHome := do
  let anchor := (doorBase world target).getD target
  let entrance := if isClassicEntrance world anchor then some anchor else none
  if !roomBoundary world target then return ← inspectClassicInterior world walls target entrance
  let nearby := [⟨target.x - 1, target.y⟩, ⟨target.x + 1, target.y⟩,
    ⟨target.x, target.y - 1⟩, ⟨target.x, target.y + 1⟩]
  let mut reason := "Point inside an enclosed room."
  for start in nearby do
    if roomBoundary world start then continue
    match inspectClassicInterior world walls start entrance with
    | .ok home => return home
    | .error problem =>
      if hasWoodWall world walls start || reason == "Point inside an enclosed room." then reason := problem
  throw reason

end Lean4Terria
