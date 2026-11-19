import Lean4Terria.JungleTemple
import Lean4Terria.TempleTraps
import Std.Data.HashSet

namespace Lean4Terria.TempleWiring

structure Trap where
  kind : TempleTraps.Kind
  frameX : Int
  deriving BEq, Repr

def trapFor : Tile → Option Trap
  | .lihzahrdDartTrap => some ⟨.dart, 18⟩
  | .lihzahrdFlameTrap => some ⟨.flame, 18⟩
  | .lihzahrdSpikyBallTrap => some ⟨.spikyBall, 0⟩
  | .lihzahrdSpearTrap => some ⟨.spear, 0⟩
  | _ => none

private def wireIndex (world : World) (p : TilePos) : Nat :=
  p.y.toNat * world.width + p.x.toNat

def maskAt (world : World) (wires : ByteArray) (p : TilePos) : UInt8 :=
  if world.contains p then wires[wireIndex world p]?.getD 0 else 0

private def applyWire (world : World) (wires : ByteArray) (p : TilePos) (mask : UInt8) : ByteArray :=
  wires.set! (wireIndex world p) (maskAt world wires p ||| mask)

private def wirePath (a b : TilePos) : Array TilePos := Id.run do
  let mut path := #[a]
  let mut p := a
  while p.y != b.y do
    p := { p with y := p.y + Int.sign (b.y - p.y) }
    path := path.push p
  while p.x != b.x do
    p := { p with x := p.x + Int.sign (b.x - p.x) }
    path := path.push p
  return path

/-- Compact placement of the four original, player-triggered temple trap types.
The wiring layer retains every connected cell independently of foreground tiles. -/
def install (layout : TempleLayout) (world : World) (canEdit : TilePos → Bool) : World × ByteArray := Id.run do
  let mut world := world
  let mut wires := ByteArray.empty
  let pairs : Array (TilePos × TilePos × Tile) := #[
    (layout.worldCell 11 5, layout.worldCell 35 5, .lihzahrdDartTrap),
    (layout.worldCell 28 5, layout.worldCell 26 1, .lihzahrdSpikyBallTrap),
    (layout.worldCell 18 11, layout.worldCell 18 7, .lihzahrdSpearTrap),
    (layout.worldCell 32 11, layout.worldCell 35 11, .lihzahrdFlameTrap)]
  for (plate, source, tile) in pairs do
    let path := wirePath plate source
    let floor := world.tile { plate with y := plate.y + 1 }
    if world.tile plate != .air || world.tile source != .lihzahrdBrick || !floor.solid ||
        !canEdit plate || !canEdit source || !path.all world.contains then continue
    let mask := (1 : UInt8) <<< (plate.x.toNat % 4).toUInt8
    world := world.setTile plate .lihzahrdPressurePlate |>.setTile source tile
    if wires.size == 0 then wires := ByteArray.mk (Array.replicate (world.width * world.height) 0)
    wires := path.foldl (fun current p => applyWire world current p mask) wires
  let spike := layout.worldCell 24 17
  if world.tile spike == .air && canEdit spike then world := world.setTile spike .woodenSpikes
  return (world, wires)

private def adjacent (p : TilePos) : Array TilePos :=
  #[⟨p.x, p.y + 1⟩, ⟨p.x, p.y - 1⟩, ⟨p.x + 1, p.y⟩, ⟨p.x - 1, p.y⟩]

/-- Traverse each wire color separately. Passing through a cell of another
color does not turn its connected devices on. -/
def trip (world : World) (wires : ByteArray) (plate : TilePos) : Array (TilePos × Trap) := Id.run do
  let mut targets := #[]
  for color in (#[(1 : UInt8), 2, 4, 8]) do
    if maskAt world wires plate &&& color == 0 then continue
    let mut queue := #[plate]
    let mut visited : Std.HashSet TilePos := ({} : Std.HashSet TilePos).insert plate
    let mut cursor := 0
    while cursor < queue.size do
      let cell := queue[cursor]!
      cursor := cursor + 1
      if let some trap := trapFor (world.tile cell) then targets := targets.push (cell, trap)
      for p in adjacent cell do
        if !visited.contains p && maskAt world wires p &&& color != 0 then
          visited := visited.insert p
          queue := queue.push p
  return targets

end Lean4Terria.TempleWiring
