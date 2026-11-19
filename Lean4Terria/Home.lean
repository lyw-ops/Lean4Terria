import Lean4Terria.Water

namespace Lean4Terria

/-- Find the standing position directly on a beacon. Both cells occupied by
the player must be dry and clear, and the supporting tile must be in the world. -/
def homeArrival (world : World) (water : Water) (anchor : TilePos) : Option Player := do
  let head := { anchor with y := anchor.y - 1 }
  let floor := { anchor with y := anchor.y + 1 }
  if world.tile? anchor != some .homeBeacon || !world.contains head ||
      !(world.tile? floor).any Tile.solid then none else
    let body : Player :=
      { x := anchor.x * tileUnits + (tileUnits - playerWidth) / 2
        y := floor.y * tileUnits - playerHeight
        vx := 0, vy := 0, grounded := true }
    if body.collides world || water.level world head > 0 || water.level world anchor > 0 then none
    else some body

end Lean4Terria
