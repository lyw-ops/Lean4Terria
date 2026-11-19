import Lean4Terria.OriginalItems

namespace Lean4Terria.OriginalChests

open OriginalCollision (Vector single tileIndex)
open OriginalItems (Stack)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Chest ChestItem Terrain)

def stack? (item : ChestItem) : Option Stack := do
  guard (item.typeId > 0 && item.typeId ≤ 65535 && item.count > 0)
  return { item := item.typeId.toNat.toUInt16, count := item.count, prefixId := item.prefixId
           favorite := item.favorite, color := item.color }

def ofStack (stack : Stack) : ChestItem :=
  { typeId := stack.item.toNat, count := stack.count, prefixId := stack.prefixId
    favorite := stack.favorite, color := stack.color }

private structure Lock where
  key : UInt16
  frameShift : Int := 36
  requiresPlantera : Bool := false

/-- Chest.IsLocked and TileInteractionsUse read the top-left tile's frame. -/
private def lockFor (tile : OriginalWorld.Tile) : Option Lock := do
  let x := (tile.frame.getD (0, 0)).1
  match tile.id with
  | some 21 =>
    if 144 ≤ x && x ≤ 178 then return { key := 329 }
    if 828 ≤ x && x ≤ 1006 then
      let key := (1533 + (x.tdiv 36 - 23).toNat).toUInt16
      return { key, frameShift := 180, requiresPlantera := true }
    if (72 ≤ x && x ≤ 106) || (1296 ≤ x && x ≤ 1330) ||
        (1368 ≤ x && x ≤ 1402) || (1440 ≤ x && x ≤ 1474) then
      return { key := 327 }
    none
  | some 467 =>
    guard (x.tdiv 36 == 13)
    return { key := 4714, requiresPlantera := true }
  | _ => none

def isLocked (terrain : Terrain) (chest : Chest) : Bool :=
  let tile := terrain.tile? chest.x.toNat chest.y.toNat
  tile.all (fun tile => (lockFor tile).isSome)

def unlockKey (terrain : Terrain) (chest : Chest) : Option UInt16 := do
  let tile ← terrain.tile? chest.x.toNat chest.y.toNat
  let lock ← lockFor tile
  guard (!lock.requiresPlantera || terrain.header.downedPlantBoss)
  return lock.key

/-- Chest.Unlock updates the four frames in place. The storage row and its
contents stay at the same coordinates. -/
def unlock (terrain : Terrain) (chest : Chest) : Option Terrain := do
  let tile ← terrain.tile? chest.x.toNat chest.y.toNat
  let lock ← lockFor tile
  guard (!lock.requiresPlantera || terrain.header.downedPlantBoss)
  let mut updated := terrain
  for x in [chest.x.toNat:chest.x.toNat + 2] do
    for y in [chest.y.toNat:chest.y.toNat + 2] do
      let part ← terrain.tile? x y
      if part.id == tile.id then
        let (frameX, frameY) ← part.frame
        updated ← updated.replaceTile x y { part with frame := some (frameX - lock.frameShift, frameY) }
  return updated

/-- NearbyChests measures from the player's top-left position to the center
of a 2-by-2 chest, also using that point for dressers. -/
def inCraftingRange (position : Vector) (chest : Chest) : Bool :=
  let x := single (chest.x * 16 + 16) - position.x
  let y := single (chest.y * 16 + 16) - position.y
  (x * x + y * y).sqrt ≤ 600

/-- Recipe.CollectItemsFromChests puts the open chest first in single-player.
It scans the other eligible world chests in their saved order. -/
def craftingSources (terrain : Terrain) (position : Vector) (chests : Array Chest)
    (opened : Option Nat) (nearbyEnabled : Bool) : Array Nat := Id.run do
  let mut sources := opened.toArray
  if !nearbyEnabled then return sources
  for index in [:chests.size] do
    let some chest := chests[index]? | continue
    if !chest.isVacant && opened != some index && inCraftingRange position chest && !isLocked terrain chest then
      sources := sources.push index
  return sources

/-- Basic chests open from either row. A dresser exposes its storage when a
top-row tile is activated. Its bottom row belongs to the clothing UI. -/
def atTarget (terrain : Terrain) (chests : Array Chest) (body : Body) (cell : Cell)
    (includeLocked : Bool := false) : Option Nat := do
  guard (inToolRange body cell 5 3 0)
  let tile ← terrain.tile? cell.x cell.y
  let typeId ← tile.id
  let (frameX, frameY) ← tile.frame
  guard (typeId == 21 || typeId == 467 || (typeId == 88 && frameY.tmod 36 == 0))
  let width : Int := if typeId == 88 then 3 else 2
  let x := (cell.x : Int) - frameX.tdiv 18 % width
  let y := (cell.y : Int) - (if typeId == 88 then frameY.tmod 36 else frameY).tdiv 18
  let index ← chests.findIdx? fun chest => !chest.isVacant && chest.x == x && chest.y == y
  let chest ← chests[index]?
  guard (includeLocked || !isLocked terrain chest)
  return index

/-- A stationary open container remains reachable if the nearest tile of its
multitile hitbox is in Simple reach. The native rectangle is inset by 1 px. -/
def inOpenRange (terrain : Terrain) (body : Body) (chest : Chest) : Bool := Id.run do
  if chest.isVacant then return false
  let some tile := terrain.tile? chest.x.toNat chest.y.toNat | return false
  let width : Int := match tile.id with
    | some 21 | some 467 => 32
    | some 88 => 48
    | _ => 0
  if width == 0 then return false
  let x := chest.x * 16
  let y := chest.y * 16
  let center := body.position + ⟨single body.width / 2, single body.height / 2⟩
  let nearestX := max (single (x + 1)) (min center.x (single (x + width - 1)))
  let nearestY := max (single (y + 1)) (min center.y (single (y + 31)))
  return inToolRange body ⟨(tileIndex nearestX).toNat, (tileIndex nearestY).toNat⟩ 5 3 0

/-- The ordinary chest lid spends nine ticks in the intermediate frame on
its way to the final frame. Store counters only for chests that are moving or open. -/
def updateLids (opened : Option Nat) (counters : Array (Nat × Nat)) : Array (Nat × Nat) := Id.run do
  let mut next := #[]
  let mut found := false
  for (index, counter) in counters do
    if opened == some index then
      next := next.push (index, min 10 (counter + 1))
      found := true
    else if counter > 1 then next := next.push (index, counter - 1)
  if !found then
    if let some index := opened then next := next.push (index, 1)
  return next

def lidOffset (counter : Nat) : Nat :=
  if counter == 0 then 0 else if counter ≥ 10 then 76 else 38

end Lean4Terria.OriginalChests
