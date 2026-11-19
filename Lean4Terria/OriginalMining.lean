import Lean4Terria.OriginalItemUse
import Lean4Terria.OriginalPlayerCollision
import Lean4Terria.OriginalTileRules

namespace Lean4Terria.OriginalMining

open OriginalCollision (Grid single tileIndex)
open OriginalItemUse (Timing)
open OriginalPlayerCollision (Body)
open OriginalWorld (Terrain Tile)

structure Cell where
  x : Nat
  y : Nat
  deriving BEq, Repr

structure WorldRules where
  surface : Float
  deriving Repr

-- The true entries from Main.Initialize in the 1.4.5.8 runtime.
private def noFail : Array UInt16 :=
  #[3, 4, 24, 32, 50, 51, 52, 61, 62, 69, 73, 74, 81, 82, 83, 84, 110, 113, 115, 129, 162, 165,
    184, 185, 186, 187, 192, 201, 205, 227, 233, 254, 324, 330, 331, 332, 333, 352, 373, 374, 375,
    382, 384, 461, 481, 482, 483, 484, 485, 518, 519, 528, 529, 530, 549, 624, 636, 637, 638, 647,
    648, 649, 650, 651, 652, 654, 655, 656, 666, 693, 694, 697, 700, 701, 705, 706, 709]

private def dungeon : Array UInt16 := #[41, 43, 44, 677, 678, 679]
private def clouds : Array UInt16 := #[189, 196, 460, 717, 718, 719]
private def moss : Array UInt16 := #[179, 180, 181, 182, 183, 381, 534, 536, 539, 625, 627]
private def mossBrick : Array UInt16 := #[512, 513, 514, 515, 516, 517, 535, 537, 540, 626, 628]
private def preventsRemoval : Array UInt16 :=
  #[5, 26, 72, 323, 470, 475, 488, 583, 584, 585, 586, 587, 588, 589, 596, 616, 634]

private def minimumPower (grid : Grid) (rules : WorldRules) (cell : Cell) (tile : Tile)
    (id : UInt16) : Nat :=
  let underground := cell.y.toFloat > rules.surface
  let x := cell.x.toFloat
  let width := grid.width.toFloat
  match id with
  | 25 | 58 | 117 | 203 => 65
  | 37 => 50
  | 22 | 204 => if underground then 55 else 0
  | 56 => 55
  | 77 => if cell.y ≥ grid.height - 200 then 65 else 0
  | 107 | 221 => 100
  | 108 | 222 => 110
  | 111 | 223 => 150
  | 211 => 200
  | 226 | 237 => 210
  | 137 =>
    let style := (tile.frame.map (·.2)).getD (-1) |>.tdiv 18
    if 1 ≤ style && style ≤ 4 then 210 else 0
  | _ =>
    if dungeon.contains id && underground && (x < width * 0.35 || x > width * 0.65) then 100
    else 0

/-- Range of a tool using the regular rectangular reach rule. Each side starts
from the outermost tile covered by the player's current collision box. -/
def inToolRange (body : Body) (cell : Cell) (rangeX rangeY tileBoost : Int) : Bool :=
  let position := body.position
  let rx := min rangeX 20 + tileBoost
  let ry := min rangeY 20 + tileBoost
  let right := ((position.x + single body.width) / 16).toFloat.ceil.toInt64.toInt - 1
  let bottom := ((position.y + single body.height) / 16).toFloat.ceil.toInt64.toInt - 1
  (cell.x : Int) ≥ tileIndex position.x - rx && (cell.x : Int) ≤ right + rx &&
    (cell.y : Int) ≥ tileIndex position.y - ry && (cell.y : Int) ≤ bottom + ry

/-- The damage calculation for ordinary worlds. This does not run the
support check, update a hit cache, or target multi-tile furniture. -/
def pickDamage (grid : Grid) (rules : WorldRules) (cell : Cell) (tile : Tile) (power : Nat) : Nat :=
  let id := tile.id.getD 0
  let strength :=
    if dungeon.contains id then power / 2
    else if clouds.contains id then 2 * power
    else match id with
      | 25 | 58 | 107 | 117 | 203 | 221 => power / 2
      | 48 | 232 => 2 * power
      | 108 | 222 => power / 3
      | 111 | 223 | 226 => power / 4
      | 211 => power / 5
      | _ => power
  let damage := if power < minimumPower grid rules cell tile id then 0
    else strength + (if noFail.contains id then 100 else 0)
  if id == 165 || id == 199 || OriginalTileRules.isRope id then 100
  else if (#[0, 40, 53, 57, 59, 123, 147, 224, 397] : Array UInt16).contains id then damage + power
  else if id == 404 then damage + 5
  else damage

def clearGround (tile : Tile) : Tile :=
  { tile with id := none, frame := none, shape := .full, tilePaint := 0, actuated := false
              invisibleBlock := false, fullbrightBlock := false }

def exposedGround (id : UInt16) : Option UInt16 :=
  match id with
  | 2 | 23 | 109 | 199 | 477 | 492 => some 0
  | 60 | 70 | 661 | 662 => some 59
  | 633 => some 57
  | _ => if moss.contains id then some 1 else if mossBrick.contains id then some 38 else none

private def groundBlocks : Array (UInt16 × UInt16) :=
  #[(0, 2), (1, 3), (6, 11), (7, 12), (8, 13), (9, 14), (18, 36), (19, 94), (22, 56), (25, 61), (30, 9),
    (37, 116), (38, 129), (53, 169), (56, 173), (57, 172), (58, 174), (59, 176),
    (147, 593), (161, 664), (166, 699), (167, 700), (168, 701), (169, 702), (204, 880)]

def itemForGround (id : UInt16) : Option UInt16 :=
  (groundBlocks.find? (·.1 == id)).map (·.2)

/-- Stable ground blocks available through mining. Falling sand is handled
by the projectile path in the original game. -/
def stableTileForItem (item : UInt16) : Option UInt16 := do
  let (tile, _) ← groundBlocks.find? (·.2 == item)
  guard (tile != 18 && tile != 19 && tile != 53)
  return tile

private def permittedGroundHit (grid : Grid) (cell : Cell) (tile : Tile)
    (id : UInt16) : Bool := Id.run do
  if tile.wall == 350 then return false
  if cell.y == 0 then return true
  let above := grid.tileAt cell.x (cell.y - 1)
  let some top := above.id | return true
  if top == id then return true
  let (frameX, frameY) := above.frame.getD (-1, -1)
  if OriginalTileRules.isTreeTrunk top then
    return frameY ≥ 198 || (frameX == 66 && 0 ≤ frameY && frameY ≤ 44) ||
      (frameX == 88 && 66 ≤ frameY && frameY ≤ 110)
  if top == 323 then return frameX != 66 && frameX != 220
  if top == 80 then
    let column := frameX.tdiv 18
    return column != 0 && column != 1 && column != 4 && column != 5
  return !(preventsRemoval.contains top || (#[21, 77, 88, 467] : Array UInt16).contains top)

structure Hit where
  cell : Cell
  damage : Nat
  timeToLive : Nat
  deriving BEq, Repr

/-- Tile hit records, in most-recent-first order. -/
structure HitCache where
  entries : Array Hit := #[]
  deriving BEq, Repr

def HitCache.damage (cache : HitCache) (cell : Cell) : Nat :=
  ((cache.entries.find? (·.cell == cell)).map (·.damage)).getD 0

def HitCache.prune (cache : HitCache) (grid : Grid) : HitCache :=
  { entries := cache.entries.filterMap fun hit => do
      guard (hit.timeToLive > 1 && (grid.tileAt hit.cell.x hit.cell.y).id.isSome)
      let timeToLive := hit.timeToLive - 1
      let decay := if timeToLive < 12 then 10 else if timeToLive < 24 then 7
        else if timeToLive < 36 then 5 else if timeToLive < 48 then 2 else 0
      guard (decay ≤ hit.damage)
      return { hit with timeToLive, damage := hit.damage - decay } }

def HitCache.forget (cache : HitCache) (cell : Cell) : HitCache :=
  { entries := cache.entries.filter (·.cell != cell) }

def HitCache.record (cache : HitCache) (cell : Cell) (damage : Nat) : HitCache :=
  let previous := cache.entries.find? (·.cell == cell)
  if damage == 0 && previous.isNone then cache
  else
    let combined := damage + (previous.map (·.damage)).getD 0
    let hit : Hit := ⟨cell, combined, 60⟩
    let entries := cache.entries.filter (·.cell != cell)
    { entries := (#[hit] ++ entries).extract 0 500 }

structure GroundHit where
  cache : HitCache
  changed : Option Tile := none
  drop : Option UInt16 := none
  deriving BEq, Repr

/-- Apply a pickaxe hit to an ordinary ground block. World framing and
effects of nearby objects are separate from this single-cell update. -/
def hitGround (grid : Grid) (rules : WorldRules) (cache : HitCache) (cell : Cell)
    (power : Nat) : GroundHit := Id.run do
  if power == 0 then return { cache }
  let tile := grid.tileAt cell.x cell.y
  let some id := tile.id | return { cache }
  if id == 18 && !(tile.frame == some (0, 0) || tile.frame == some (18, 0)) then return { cache }
  if id == 19 && !(tile.frame.any fun (_, y) => y == 0) then return { cache }
  let exposes := exposedGround id
  let item := itemForGround id
  if exposes.isNone && item.isNone then return { cache }
  if !permittedGroundHit grid cell tile id then return { cache := cache.record cell 0 }
  let raw := pickDamage grid rules cell tile power
  let damage := if exposes.isSome && cache.damage cell + raw ≥ 100 then 0 else raw
  let cache := cache.record cell damage
  if cache.damage cell ≥ 100 then
    let remaining := cache.forget cell
    let removed := clearGround tile
    let removed :=
      if id == 58 && cell.y > grid.height - 200 then
        { removed with liquid := some ⟨.lava, 128⟩ }
      else removed
    return { cache := (remaining.prune grid).prune grid, changed := some removed, drop := item }
  let changed := exposes.map fun base => { tile with id := some base }
  return { cache := if damage == 0 then cache else cache.prune grid, changed }

def GroundHit.apply (hit : GroundHit) (terrain : Terrain) (cell : Cell) : Terrain :=
  match hit.changed with
  | none => terrain
  | some tile => (terrain.replaceTile cell.x cell.y tile).getD terrain

structure CopperPick extends Timing where
  hits : HitCache := {}
  deriving BEq, Repr

/-- The ordinary unmodified Copper Pickaxe's item phase. Raw input polling,
melee damage to enemies, and the world update run outside this phase. -/
def CopperPick.advance (pick : CopperPick) (body : Body) (grid : Grid) (rules : WorldRules)
    (held : Bool) (target : Option Cell) : CopperPick × GroundHit := Id.run do
  let timing := pick.toTiming.advance held 23
  let state := { pick with toTiming := timing }
  let idle : GroundHit := { cache := pick.hits }
  let some cell := target | return (state, idle)
  if !(held && timing.itemAnimation > 0 && timing.itemTime == 0 && inToolRange body cell 5 3 (-1)) ||
      (grid.tileAt cell.x cell.y).id.isNone then
    return (state, idle)
  let hit := hitGround grid rules pick.hits cell 35
  return ({ state with toTiming := timing.setTime 15, hits := hit.cache }, hit)

end Lean4Terria.OriginalMining
