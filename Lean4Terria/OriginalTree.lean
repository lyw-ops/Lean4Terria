import Lean4Terria.OriginalItems

namespace Lean4Terria.OriginalTree

open OriginalCollision (Grid)
open OriginalItems (WorldItems spawnStack)
open OriginalItemUse (Timing)
open OriginalMining (Cell HitCache clearGround inToolRange)
open OriginalPlayerCollision (Body)
open OriginalTileRules (collision isTreeTrunk)
open OriginalWorld (Terrain Tile)

private def treeTile : UInt16 := 5
private def wood : UInt16 := 9
private def acorn : UInt16 := 27
private def copperAxePower : Nat := 7
private def choppingDamage : Nat := ((copperAxePower.toUInt64.toFloat32 * 1.2).toUInt64).toNat

private def tileAt (terrain : Terrain) (cell : Cell) : Tile :=
  (terrain.tile? cell.x cell.y).getD default

private def offset (cell : Cell) (dx dy : Int) : Cell :=
  ⟨((cell.x : Int) + dx).toNat, ((cell.y : Int) + dy).toNat⟩

private def neighbors (cell : Cell) : List Cell :=
  [-1, 0, 1].flatMap fun dx => [-1, 0, 1].map (offset cell dx)

private def isTree (tile : Tile) : Bool :=
  tile.id == some treeTile

private def leftBranch (x y : Int) : Bool :=
  (x == 66 && 0 ≤ y && y ≤ 44) || (x == 44 && 198 ≤ y)

private def rightBranch (x y : Int) : Bool :=
  (x == 88 && 66 ≤ y && y ≤ 110) || (x == 66 && 198 ≤ y)

private def supportTile (tile : Tile) : Bool :=
  tile.id.any fun id =>
    (#[2, 23, 60, 70, 109, 147, 199, 477, 492, 661, 662] : Array UInt16).contains id

private def treeGround (terrain : Terrain) (cell : Cell) (x y : Int) : Option UInt16 := Id.run do
  let root := if leftBranch x y || (x == 44 && 132 ≤ y && y ≤ 176) then offset cell 1 0
    else if rightBranch x y || (x == 22 && 132 ≤ y && y ≤ 176) then offset cell (-1) 0
    else cell
  for y in [root.y:terrain.header.height] do
    if let some id := (tileAt terrain ⟨root.x, y⟩).id then
      if !isTreeTrunk id then return some id
  return none

private inductive Check where
  | keep
  | cut
  | reframe (x y : Int)

/-- Local support and frame rules for tile 5. Frame coordinates distinguish
branches and side roots from wood that stands on the tile below. -/
private def check (terrain : Terrain) (cell : Cell) (x y : Int) : Check := Id.run do
  let left := isTree (tileAt terrain (offset cell (-1) 0))
  let right := isTree (tileAt terrain (offset cell 1 0))
  let above := isTree (tileAt terrain (offset cell 0 (-1)))
  let below := tileAt terrain (offset cell 0 1)
  let onGrass := supportTile below
  let onTrunk := isTree below
  let rootRow := 132 ≤ y && y ≤ 176
  let variant := y % 66
  if (x == 0 || x == 22 || x == 44) && y ≤ 130 && !onGrass && !onTrunk then
    return .cut
  if (x == 22 || x == 44) && rootRow then
    return if onGrass && (if x == 22 then left else right) then .keep else .cut
  let fork := (x == 88 && 0 ≤ y && y ≤ 44) || (x == 66 && 66 ≤ y && y ≤ 130) ||
    (x == 110 && 66 ≤ y && y ≤ 110) || (x == 132 && 0 ≤ y && y ≤ 176)
  let (x, y) := if fork then
    if left && right then (110, 66 + variant)
    else if left then (88, variant)
    else if right then (66, 66 + variant)
    else (0, variant)
    else (x, y)
  let centralRoot := (x == 0 || x == 66 || x == 88) && 132 ≤ y && y ≤ 176
  if centralRoot && !onGrass then return .cut
  let (x, y) := if centralRoot then
    if left && right then (88, 132 + variant)
    else if left then (66, 132 + variant)
    else if right then (0, 132 + variant)
    else (0, variant)
    else (x, y)
  if leftBranch x y then return if right then .keep else .cut
  if rightBranch x y then return if left then .keep else .cut
  if below.id.isNone then return .cut
  if above || y ≥ 198 then return .reframe x y
  if !left && !right then return .reframe 110 variant
  let crown := if onTrunk then 132 else 154
  let row := if left && right then 132 else if right then 66 else 0
  return .reframe crown (row + variant)

private def canCut (terrain : Terrain) (cell : Cell) (tile : Tile) : Bool :=
  let above := (tileAt terrain (offset cell 0 (-1))).id
  tile.wall != 350 && !above.any (fun id =>
    (#[21, 26, 72, 77, 88, 467, 488] : Array UInt16).contains id)

private inductive Update where
  | cut (cell : Cell)
  | frame (cell : Cell)

structure Felled where
  terrain : Terrain
  removed : Array Cell := #[]
  items : WorldItems
  random : UnifiedRandom

/-- The forest variant of KillTile_GetTreeDrops. WorldGen.genRand and
Main.rand refer to the same stream here. The bonus uses the internal axe
stat, one fifth of the value shown in the inventory tooltip. -/
private def addDrops (result : Felled) (cell : Cell) (x y : Int) (hasAcorns : Bool) : Felled :=
  let (seed, random) := if x ≥ 22 && y ≥ 198 then result.random.next 0 2 else (1, result.random)
  let (roll, random) := random.next 0 35
  let (bonus, random) := if roll ≤ copperAxePower then (0, random) else random.next 0 3
  let grid := some (Grid.ofTerrain result.terrain collision)
  let (items, random) :=
    spawnStack result.items cell (.ofItem wood (if bonus == 0 then 2 else 1)) random grid
  let (items, random) := if seed == 0 && hasAcorns then
      spawnStack items cell (.ofItem acorn) random grid
    else (items, random)
  { result with items, random }

/-- Killing a tile frames its adjacent tiles in column order. A support
failure immediately breaks that tile and frames its own neighbors before
the caller continues, so the order of drops matches the felling order. -/
def fell (terrain : Terrain) (target : Cell) (items : WorldItems) (random : UnifiedRandom) :
    Felled := Id.run do
  let mut result : Felled := { terrain, items, random }
  let mut pending : List Update := [.cut target]
  while let update :: rest := pending do
    pending := rest
    let cell := match update with
      | .cut cell | .frame cell => cell
    let tile := tileAt result.terrain cell
    if !isTree tile then continue
    let some (x, y) := tile.frame | continue
    let action := match update with
      | .cut _ => Check.cut
      | .frame _ => check result.terrain cell x y
    match action with
    | .keep => pure ()
    | .cut =>
      if !canCut result.terrain cell tile then continue
      let ground := treeGround result.terrain cell x y
      result := addDrops result cell x y (ground == some 2 || ground == some 477)
      result := { result with
        terrain := (result.terrain.replaceTile cell.x cell.y (clearGround tile)).getD result.terrain
        removed := result.removed.push cell }
      pending := (neighbors cell).map Update.frame ++ pending
    | .reframe sx sy =>
      if sx == x && sy == y then continue
      let tile := { tile with frame := some (sx, sy) }
      result := { result with
        terrain := (result.terrain.replaceTile cell.x cell.y tile).getD result.terrain }
      if sx != x && sy != y then
        pending := [offset cell (-1) 0, offset cell 1 0, offset cell 0 (-1),
          offset cell 0 1].map Update.frame ++ pending
  return result

structure Strike where
  timing : Timing
  hits : HitCache
  felled : Felled

/-- Copper Axe item use on ordinary forest trees. This runs after movement
and receives the random stream at the start of the item-use effect. -/
def advance (terrain : Terrain) (body : Body) (timing : Timing) (hits : HitCache)
    (items : WorldItems) (random : UnifiedRandom) (held : Bool) (target : Option Cell) : Strike := Id.run do
  let timing := timing.advance held 30
  let unchanged : Felled := { terrain, items, random }
  let idle : Strike := { timing, hits, felled := unchanged }
  let some cell := target | return idle
  if !(held && timing.itemAnimation > 0 && timing.itemTime == 0 && inToolRange body cell 5 3 (-1)) then
    return idle
  let tile := tileAt terrain cell
  let some (x, y) := tile.frame | return idle
  if !isTree tile || !(treeGround terrain cell x y).any (fun id => id == 2 || id == 477) then
    return idle
  let timing := timing.setTime 21
  if !canCut terrain cell tile then
    return { idle with timing, hits := hits.record cell 0 }
  let (_, random) := random.nextInt
  let hits := hits.record cell choppingDamage
  if hits.damage cell < 100 then
    let grid := Grid.ofTerrain terrain collision
    return { timing, hits := hits.prune grid, felled := { unchanged with random } }
  let felled := fell terrain cell items random
  let grid := Grid.ofTerrain felled.terrain collision
  return { timing, hits := ((hits.forget cell).prune grid).prune grid, felled }

end Lean4Terria.OriginalTree
