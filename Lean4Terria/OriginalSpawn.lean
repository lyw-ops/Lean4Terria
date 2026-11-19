import Lean4Terria.OriginalCollision

namespace Lean4Terria.OriginalSpawn

open OriginalCollision (Grid Vector single)
open OriginalWorld (Tile)

structure Spawn where
  tileX : Int
  tileY : Int
  requiresClearance : Bool
  deriving BEq, Repr

def Spawn.position (spawn : Spawn) (width height : Nat) : Vector :=
  ⟨single (spawn.tileX * 16 + 8 - (width / 2 : Nat)), single (spawn.tileY * 16 - (height : Int))⟩

private def inWorld (grid : Grid) (x y : Int) : Bool :=
  x ≥ 5 && y ≥ 5 && x < (grid.width : Int) - 5 && y < (grid.height : Int) - 5

private def tileAt (grid : Grid) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else grid.tileAt x.toNat y.toNat

private def validArea (grid : Grid) (canHurt : UInt16 → Bool) (x y : Int) : Bool := Id.run do
  for dx in [-1, 0, 1] do
    for dy in [-3, -2, -1] do
      let tx := x + dx
      let ty := y + dy
      if !inWorld grid tx ty then continue
      if dy == -1 && (tileAt grid tx y).id.any (fun id => id == 135 || canHurt id) then
        return false
      let tile := tileAt grid tx ty
      if tile.liquid.any (fun liquid => liquid.amount > 0) then return false
      if !tile.actuated && tile.id.any (fun id =>
          let rules := grid.rules id
          id == 135 || (rules.solid && !rules.solidTop)) then
        return false
  return true

private def validFloor (grid : Grid) (x y : Int) : Bool :=
  let tile := tileAt grid x y
  !tile.actuated && tile.id.any fun id =>
    let rules := grid.rules id
    id != 135 && id != 379 && ((rules.solid && !rules.solidTop) || rules.platform || id == 380)

private def descend (grid : Grid) (canHurt : UInt16 → Bool) (x y : Int)
    (allowNarrow : Bool) : Option (Int × Int) := Id.run do
  for dy in [:50] do
    let floorY := y + (dy : Int)
    for dx in ([-1, 0, 1] : List Int) do
      let floorX := x + dx
      let floor := tileAt grid floorX floorY
      if inWorld grid floorX floorY && floor.id == some 135 && !floor.actuated then return none
    let usable := fun tx => inWorld grid tx floorY &&
      validFloor grid tx floorY && validArea grid canHurt tx floorY
    let left := usable (x - 1)
    let center := usable x
    let right := usable (x + 1)
    if center && (left || right || allowNarrow) then return some (x, floorY)
    if !center then
      if (left || allowNarrow) && validFloor grid (x - 2) floorY && validArea grid canHurt (x - 1) floorY then
        return some (x - 1, floorY)
      if (right || allowNarrow) && validFloor grid (x + 2) floorY && validArea grid canHurt (x + 1) floorY then
        return some (x + 1, floorY)
  return none

/-- Resolve the regular world spawn from its saved tile coordinates. The
result includes the original clearance condition so the caller can apply
the tile and liquid update when the surrounding area is blocked. -/
def worldSpawn (grid : Grid) (canHurt : UInt16 → Bool) (x y : Int) : Spawn := Id.run do
  let original : Spawn := { tileX := x, tileY := y, requiresClearance := false }
  let findFloor := fun tx ty => ((descend grid canHurt tx ty false).or (descend grid canHurt tx ty true)).getD (tx, ty)
  if validArea grid canHurt x y then
    let (tileX, tileY) := findFloor x y
    return { original with tileX, tileY }
  for sign in ([-1, 1] : List Int) do
    for dy in [:30] do
      let thisY := y + sign * (dy : Int)
      for thisX in [x, x - 1, x + 1] do
        if validArea grid canHurt thisX thisY then
          let (tileX, tileY) := findFloor thisX thisY
          return { tileX, tileY, requiresClearance := false }
  return { original with requiresClearance := true }

end Lean4Terria.OriginalSpawn
