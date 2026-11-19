import Lean4Terria.Physics

namespace Lean4Terria

def railMaxSpeed : Int := 84

structure RailRide where
  rail : TilePos
  next : Option TilePos := none
  progress : Nat := 0
  speed : Int := 0
  deriving BEq, Repr, Inhabited

/-- Seat a rider with their feet at the top of the rail tile. The cart's speed
is kept separately from the player's walking and falling velocity. -/
def cartBody (position : TilePos) : Player :=
  { x := position.x * tileUnits + (tileUnits - playerWidth) / 2
    y := position.y * tileUnits - playerHeight
    vx := 0, vy := 0, grounded := false }

def canParkCart (world : World) (position : TilePos) : Bool :=
  world.tile position == .rail && !(cartBody position).collides world

private def seatAlong (origin destination : TilePos) (progress : Nat) : Player :=
  let body := cartBody origin
  { body with
    x := body.x + (destination.x - origin.x) * progress
    y := body.y + (destination.y - origin.y) * progress }

def RailRide.body (ride : RailRide) : Player :=
  match ride.next with
  | none => cartBody ride.rail
  | some target => seatAlong ride.rail target ride.progress

def RailRide.parkingTile (ride : RailRide) : TilePos :=
  if 2 * ride.progress ≤ tileUnits.toNat then ride.rail else ride.next.getD ride.rail

private def nextGridCrossing (coordinate direction : Int) : Nat :=
  if direction < 0 then (coordinate % tileUnits + 1).toNat
  else (tileUnits - coordinate % tileUnits).toNat

/-- The overlap only changes at a body edge's tile crossing. Sample those
four crossings as well as the endpoints to cover every integer position. -/
private def sweptSeats (origin destination : TilePos) : Array Player :=
  let body := cartBody origin
  let dx := destination.x - origin.x
  let dy := destination.y - origin.y
  let points := #[0, tileUnits.toNat, nextGridCrossing body.x dx,
    nextGridCrossing (body.x + playerWidth - 1) dx]
  let points := if dy == 0 then points else points ++
    #[nextGridCrossing body.y dy, nextGridCrossing (body.y + playerHeight - 1) dy]
  points.map (seatAlong origin destination)

private def clearSegment (world : World) (origin destination : TilePos) : Bool :=
  (destination.x - origin.x).natAbs == 1 && (destination.y - origin.y).natAbs ≤ 1 &&
    canParkCart world origin && canParkCart world destination &&
    (sweptSeats origin destination).all (fun seat => !seat.collides world)

private def slopeEligible (world : World) (origin destination : TilePos) : Bool :=
  clearSegment world origin destination &&
    !clearSegment world origin { destination with y := origin.y } &&
    !clearSegment world destination { origin with y := destination.y }

/-- Pair each pair of columns with the same order from both sides: level tracks
first, then eastbound rises, then eastbound descents using any unmatched ends. -/
def railNeighbor (world : World) (position : TilePos) (direction : Int) : Option TilePos := Id.run do
  if direction == 0 || !canParkCart world position then return none
  let dx : Int := if direction < 0 then -1 else 1
  let flat := { position with x := position.x + dx }
  if clearSegment world position flat then return some flat
  let rising := { flat with y := flat.y - dx }
  if slopeEligible world position rising then return some rising
  let falling := { flat with y := flat.y + dx }
  if slopeEligible world position falling &&
      !slopeEligible world falling ⟨position.x, falling.y + dx⟩ then
    return some falling
  return none

-- Inclusive integer interval where one axis of the moving and parked seats overlaps.
private def overlapInterval (start direction parked size : Int) : Int × Int :=
  if direction == 0 then
    if (parked - start).natAbs < size.toNat then (0, tileUnits) else (1, 0)
  else
    let distance := if direction > 0 then parked - start else start - parked
    (distance - size + 1, distance + size - 1)

private def seatCrossesCart (origin destination parked : TilePos) : Bool :=
  let moving := cartBody origin
  let fixed := cartBody parked
  let (left, right) := overlapInterval moving.x (destination.x - origin.x) fixed.x playerWidth
  let (top, bottom) := overlapInterval moving.y (destination.y - origin.y) fixed.y playerHeight
  max 0 (max left top) ≤ min tileUnits (min right bottom)

private def clearOfParked (parked : Array TilePos) (origin destination : TilePos) : Bool :=
  !parked.any (seatCrossesCart origin destination)

def RailRide.valid (world : World) (parked : Array TilePos) (ride : RailRide) : Bool :=
  ride.speed.natAbs ≤ railMaxSpeed.toNat &&
    match ride.next with
    | none => ride.progress == 0 && canParkCart world ride.rail &&
        clearOfParked parked ride.rail ride.rail
    | some target =>
      -- A new branch can change future routing without severing a segment in use.
      0 < ride.progress && ride.progress < tileUnits.toNat && clearSegment world ride.rail target &&
        clearOfParked parked ride.rail target

private def updatedSpeed (speed horizontal : Int) : Int :=
  let direction := max (-1) (min horizontal 1)
  let sign : Int := if speed < 0 then -1 else 1
  if direction == 0 then sign * (speed.natAbs - 3 : Nat)
  else if direction * speed < 0 then sign * (speed.natAbs - 13 : Nat)
  else max (-railMaxSpeed) (min railMaxSpeed (speed + direction * 7))

private def depart (world : World) (parked : Array TilePos) (position : TilePos)
    (speed : Int) (distance : Nat) : RailRide := Id.run do
  let stopped : RailRide := { rail := position }
  let some target := railNeighbor world position speed | return stopped
  if !clearOfParked parked position target then return stopped
  if distance == 0 then return { stopped with speed }
  return { rail := position, next := some target, progress := distance, speed }

private def stopInvalid (world : World) (parked : Array TilePos) (ride : RailRide) : RailRide :=
  let endpoints := ride.parkingTile :: ride.rail :: ride.next.toList
  let position := endpoints.find? (fun pos => canParkCart world pos && clearOfParked parked pos pos)
  { rail := position.getD ride.rail }

/-- Accelerate, coast, or brake, carrying any leftover distance across a joint.
Reversing within a segment changes direction without changing the current seat. -/
def advanceRail (world : World) (parked : Array TilePos) (ride : RailRide) (horizontal : Int) : RailRide :=
  if !ride.valid world parked then stopInvalid world parked ride else
    let speed := updatedSpeed ride.speed horizontal
    if speed == 0 then { ride with speed := 0 } else
      match ride.next with
      | none => depart world parked ride.rail speed speed.natAbs
      | some target =>
        let progress : Int := (ride.progress : Int) + speed * (target.x - ride.rail.x)
        if progress ≤ 0 then depart world parked ride.rail speed progress.natAbs
        else if tileUnits ≤ progress then depart world parked target speed (progress - tileUnits).toNat
        else { ride with progress := progress.toNat, speed }

end Lean4Terria
