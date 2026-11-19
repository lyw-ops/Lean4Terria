import Lean4Terria.Water

namespace Lean4Terria

structure Crop where
  position : TilePos
  ticks : Nat
  deriving BEq, Repr, Inhabited

def cropGrowthTicks : Nat := 360

private def hasSoil (world : World) (position : TilePos) : Bool :=
  world.contains position && world.tile { position with y := position.y + 1 } == .dirt

/-- Check the environment independently of the plant's current stage and time of day. -/
def sunleafCanGrow (world : World) (water : Water) (position : TilePos) : Bool :=
  hasSoil world position && water.level world position == 0 && world.clearSkyAbove position

def canPlantSunleaf (world : World) (water : Water) (position : TilePos) : Bool :=
  world.tile position == .air && sunleafCanGrow world water position

/-- Advance elapsed growth for supported sprouts. Darkness, shade and water
pause growth, while mature or removed plants no longer need a timer. -/
def advanceGarden (world : World) (water : Water) (crops : Array Crop) (daylight : Bool) :
    World × Array Crop := Id.run do
  let mut world := world
  let mut active := #[]
  for crop in crops do
    if world.tile crop.position != .sunleafSprout then continue
    if !hasSoil world crop.position then
      world := world.setTile crop.position .air
      continue
    if !daylight || !sunleafCanGrow world water crop.position then
      active := active.push crop
    else if crop.ticks + 1 ≥ cropGrowthTicks then
      world := world.setTile crop.position .sunleaf
    else
      active := active.push { crop with ticks := crop.ticks + 1 }
  return (world, active)

end Lean4Terria
