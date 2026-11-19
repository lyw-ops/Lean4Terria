import Lean4Terria.Water

namespace Lean4Terria

def planteraBulbCells (anchor : TilePos) : Array TilePos :=
  #[anchor, ⟨anchor.x + 1, anchor.y⟩, ⟨anchor.x, anchor.y + 1⟩, ⟨anchor.x + 1, anchor.y + 1⟩]

def planteraBulbSupported (world : World) (anchor : TilePos) : Bool :=
  world.tile ⟨anchor.x, anchor.y + 2⟩ == .jungleGrass &&
    world.tile ⟨anchor.x + 1, anchor.y + 2⟩ == .jungleGrass

def planteraBulbValid (world : World) (anchor : TilePos) : Bool :=
  planteraBulbSupported world anchor && (planteraBulbCells anchor).all fun position =>
    world.contains position && world.tile position == .planteraBulb

private def plantBulb (world : World) (anchor : TilePos) : Option World := do
  if anchor.x < 4 || anchor.x + 6 > world.width || anchor.y < 4 || anchor.y + 6 > world.height then failure
  if !planteraBulbSupported world anchor || !(planteraBulbCells anchor).all (fun p => world.tile p == .air) then
    failure
  return (planteraBulbCells anchor).foldl (fun terrain p => terrain.setTile p .planteraBulb) world

private def forceBulb (world : World) (right bottom : Int) (dry : TilePos → Bool) : Option World := do
  let cells := (List.range 4).flatMap fun (dx : Nat) => (List.range 4).map fun (dy : Nat) =>
    ((⟨right + dx - 2, bottom + dy - 2⟩ : TilePos), dx == 0 || dx == 3 || dy == 0 || dy == 3)
  if !cells.all (fun (p, edge) =>
      world.contains p && (!edge || dry p) && [Tile.mud, .jungleGrass, .air, .stone, .dirt].contains (world.tile p)) then
    failure
  let score := cells.foldl (fun (total : Int) (p, _) =>
    let tile := world.tile p
    if tile == .mud || tile == .jungleGrass then total + 1
    else if tile == .air then total else total - 1) 0
  if score < 12 then failure
  let cleared := cells.foldl (fun terrain (p, edge) =>
    terrain.setTile p (if edge then .jungleGrass else .air)) world
  plantBulb cleared ⟨right - 1, bottom - 1⟩

/-- The first bulb uses the native upward search and tries the adjoining columns.
The compact jungle occupies columns 428–463. The original last 499 attempts
may carve a grass border in a mostly-mud patch. -/
def firstPlanteraBulb (world : World) (surface : Nat) (roll : Nat → Nat)
    (dry : TilePos → Bool := fun _ => true) : Option (World × TilePos) := Id.run do
  let left := 428
  let right := min world.width 464
  let stop := min (world.height - 5) underworldStart
  if left ≥ right || surface + 2 ≥ stop then return none
  for draw in [:2500] do
    let column := left + roll (2 * draw) % (right - left)
    let start := surface + roll (2 * draw + 1) % (stop - surface)
    let force := draw > 2000
    for offset in [1:min 500 (start - surface + 1)] do
      let row := start - offset
      let tile := world.tile ⟨column, row⟩
      if tile == .jungleGrass || (force && tile == .mud) then
        for delta in [0, -1, 1] do
          let x : Int := (column : Int) + delta
          let anchor : TilePos := ⟨x - 1, (row : Int) - 2⟩
          let planted := if force then forceBulb world x (row - 1 : Int) dry else plantBulb world anchor
          if let some terrain := planted then return some (terrain, anchor)
        break
  return none

/-- Native underground jungle growth samples a grass cell, then independently
passes the plant (1/25) and bulb (1/60) rolls. Existing bulbs exclude other
placements within the sampled 300-by-300 square. -/
def growPlanteraBulb (world : World) (water magma honey : Water) (surface : Nat)
    (bulbs : Array TilePos) (roll : Nat → Nat) : Option (World × TilePos) := do
  if roll 2 % 25 != 0 || roll 3 % 60 != 0 then failure
  let depth := min underworldStart world.height - surface
  if depth == 0 || world.width ≤ 20 then failure
  let x : Int := 10 + (roll 0 % (world.width - 20) : Nat)
  let y : Int := surface + roll 1 % depth
  let anchor : TilePos := ⟨x - 1, y - 2⟩
  if (water.level world ⟨x, y - 1⟩ + magma.level world ⟨x, y - 1⟩ + honey.level world ⟨x, y - 1⟩) > 0 then
    failure
  if bulbs.any (fun bulb => (bulb.x + 1 - x).natAbs < 150 && (bulb.y + 2 - y).natAbs < 150) then failure
  return (← plantBulb world anchor, anchor)

/-- Compact underground jungle terrain. The surrounding mud and grass provide
the original substrate and the chamber leaves two tiles of clearance. -/
def installUndergroundJungle (world : World) (seed : UInt64) (canEdit : TilePos → Bool := fun _ => true) : World := Id.run do
  if world.width != worldWidth || world.height != underworldHeight then return world
  for left in [443, 446, 449] do
    let top := (List.range' left 15).foldl (fun height x => max height (groundHeight seed x)) 0 + 22
    let cells := (List.range 15).flatMap fun (dx : Nat) => (List.range 13).map fun (dy : Nat) =>
      ((⟨left + dx, top + dy⟩ : TilePos), dx, dy)
    if !cells.all (fun (position, _, _) => canEdit position &&
        [Tile.stone, .dirt, .mud].contains (world.tile position)) then continue
    return cells.foldl (fun terrain (p, x, y) =>
      let tile := if x == 0 || x == 14 || y == 0 || y == 12 then Tile.mud
        else if x == 1 || x == 13 || y == 1 || y == 11 then .jungleGrass else .air
      terrain.setTile p tile) world
  return world

end Lean4Terria
