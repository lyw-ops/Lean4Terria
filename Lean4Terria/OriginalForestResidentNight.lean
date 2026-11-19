import Lean4Terria.OriginalForestGuide.Resident
import Lean4Terria.UnifiedRandom

/-!
Home and shelter branches of Terraria 1.4.5.8's style 7 town AI for the
Guide and Old Man. The caller keeps its threat scan, ordinary activity
handlers, shared AI tail and physics. Sitting is kept alongside Resident
because activity value 5 is outside its ordinary movement modes.
-/
namespace Lean4Terria.OriginalForestResidentNight

open OriginalForestGuide (Resident)
open OriginalCollision (Grid Vector single tileIndex)
open OriginalWorld (Tile)

abbrev Cell := Int × Int

structure ViewPlayer where
  position : Vector
  width : Nat := 20
  height : Nat := 42
  active : Bool := true

structure Context where
  grid : Grid
  dungeon : Cell
  dayTime : Bool
  players : Array ViewPlayer := #[]
  /-- Each other seated resident's tile containing Bottom - (0, 2). -/
  occupiedSeats : Array Cell := #[]
  screenWidth : Nat := 1920
  screenHeight : Nat := 1200
  safeX : Nat := 62
  safeY : Nat := 39

structure State where
  resident : Resident
  sitting : Bool := false
  deriving Repr

def State.aiValue (state : State) : Float32 :=
  if state.sitting then 5 else state.resident.activity.aiValue

structure Roll where
  upper : Int
  value : Int
  deriving BEq, Repr

structure Progress where
  state : State
  random : UnifiedRandom
  rolls : Array Roll := #[]
  /-- Coordinates given to the native forced-seat helper, as floor cells. -/
  seatAttempts : Array Cell := #[]

private def Progress.next (progress : Progress) (upper : Int) : Int × Progress :=
  let (value, random) := progress.random.next 0 upper
  (value, { progress with random, rolls := progress.rolls.push ⟨upper, value⟩ })

private def intPixel (n : Float32) : Int := n.toInt32.toInt

private def feet (n : Resident) : Float32 := n.position.y + single n.height

private def nudgeTile (n : Resident) (yOffset : Float32) : Cell :=
  (tileIndex n.center.x, tileIndex (feet n + yOffset))

private def withinGrid (grid : Grid) (x y : Int) : Bool :=
  0 ≤ x && x < (grid.width : Int) && 0 ≤ y && y < (grid.height : Int)

private def tile (grid : Grid) (x y : Int) : Tile :=
  if withinGrid grid x y then grid.tileAt x.toNat y.toNat else default

private def surface (grid : Grid) (tile : Tile) : Bool :=
  !tile.actuated && tile.id.any (fun id =>
    let rules := grid.rules id
    (rules.solid && !rules.solidTop) || rules.platform)

private def findFloor (grid : Grid) (home : Cell) : Cell := Id.run do
  if home.1 == -1 || home.2 == -1 then return home
  let mut y := home.2
  let limit := (grid.height : Int) - 20
  while y < limit && !surface grid (tile grid home.1 y) do
    y := y + 1
  return (home.1, y)

private def chair (tile : Tile) : Bool :=
  tile.id.any (fun id => id == 15 || id == 497)

private def frame (tile : Tile) : Int × Int := tile.frame.getD (0, 0)

private def nearby (a b : Cell) (distance : Nat) : Bool :=
  (a.1 - b.1).natAbs ≤ distance && (a.2 - b.2).natAbs ≤ distance

/-- AI_007_FindGoodRestingSpot runs before binding the Old Man's home to
the dungeon. At night the nearest chair is chosen in native scan order. -/
def restingSpot (context : Context) (state : State) : Cell := Id.run do
  let home := findFloor context.grid state.resident.homeTile
  if home.1 == -1 || home.2 == -1 || context.dayTime || state.sitting then return home
  let mut winner : Option (Cell × Nat × Bool) := none
  for column in [:15] do
    let x := home.1 - 7 + column
    for offset in #[2, 0, -2, -4, -6] do
      let y := home.2 + offset
      let cell := tile context.grid x y
      let upperTile := (frame cell).2 % 40 == 0
      if !chair cell || (offset == 2 && upperTile) then continue
      let distance := (x - home.1).natAbs + offset.natAbs
      if winner.any (fun (_, best, _) => best ≤ distance) then continue
      winner := some ((x, y), distance, upperTile)
  let some ((x, y), _, upperTile) := winner | return home
  let floor := (x, y + (if upperTile then 2 else 1))
  if context.occupiedSeats.contains floor then return home
  return floor

private def atRest (context : Context) (state : State) (current rest : Cell) : Bool :=
  if !context.dayTime && state.sitting then nearby current rest 7 else current == rest

private def playerSees (context : Context) (center : Vector) (player : ViewPlayer) : Bool :=
  let x := intPixel (center.x - single (context.screenWidth / 2 : Nat) - single context.safeX)
  let y := intPixel (center.y - single (context.screenHeight / 2 : Nat) - single context.safeY)
  let px := intPixel player.position.x
  let py := intPixel player.position.y
  player.active && px < x + context.screenWidth + 2 * context.safeX &&
    x < px + player.width && py < y + context.screenHeight + 2 * context.safeY && y < py + player.height

private def forceSitting (context : Context) (progress : Progress) (floor : Cell) : Progress :=
  let progress := { progress with seatAttempts := progress.seatAttempts.push floor }
  let state := progress.state
  if !withinGrid context.grid floor.1 floor.2 || state.sitting then progress else
  let n := state.resident
  let cell := tile context.grid floor.1 (floor.2 - 1)
  let (frameX, frameY) := frame cell
  if !chair cell || (cell.id == some 15 && 1080 ≤ frameY && frameY ≤ 1098) ||
      context.occupiedSeats.contains (nudgeTile n (-2)) then progress else
  let (roll, progress) := progress.next 10800
  let direction := if frameX == 0 then -1 else 1
  let position : Vector :=
    ⟨single (floor.1 * 16 + 8 + 2 * direction - n.width / 2), single (floor.2 * 16 - n.height)⟩
  let resident := { n with
    position, direction, velocity := ⟨0, 0⟩, timer := single (900 + roll), turnTimer := 0 }
  { progress with state := { resident, sitting := true } }

private def clearForGuide (grid : Grid) (x y : Int) : Bool := Id.run do
  if x - 1 < 0 || x + 1 ≥ grid.width || y - 3 < 0 || y - 1 ≥ (grid.height : Int) - 40 then
    return false
  for dx in #[-1, 0, 1] do
    for dy in #[-3, -2, -1] do
      let cell := tile grid (x + dx) (y + dy)
      if !cell.actuated && cell.id.any (fun id => (grid.rules id).solid && !(grid.rules id).solidTop) then
        return false
  return true

structure Location where
  /-- Source cell computed before any same-tick teleport. -/
  current : Cell
  rest : Cell
  deriving BEq, Repr

structure Prepared where
  location : Location
  progress : Progress
  teleport : Option Cell := none
  /-- All three destination columns failed. The world housing search owns the next step. -/
  needsHousingSearch : Bool := false

/-- Resolve the floor and any offscreen relocation before dispatching the
regular AI leaf. The Old Man can relocate in daylight from a Dungeon brick. -/
def prepare (context : Context) (state : State) (random : UnifiedRandom) : Prepared := Id.run do
  let n := state.resident
  let n := if n.homeTile == (-1, -1) && n.velocity.y == 0 then
      { n with homeTile := nudgeTile n 4 } else n
  let state := { state with resident := n }
  let current := nudgeTile n 1
  let rest := restingSpot context state
  let n := { n with direction := if n.direction == 0 then 1 else n.direction, directionY := -1 }
  let n := if n.kind == .oldMan then { n with homeTile := context.dungeon, homeless := false } else n
  let state := { state with resident := n }
  let progress : Progress := { state, random }
  let result : Prepared := { location := ⟨current, rest⟩, progress }
  let dungeonBrick := (tile context.grid current.1 current.2).id.any (fun id => id == 41 || id == 43 || id == 44)
  if n.homeless || (context.dayTime && !(n.kind == .oldMan && dungeonBrick)) ||
      atRest context state current rest then return result
  let target : Vector := ⟨single (rest.1 * 16 + 8), single (rest.2 * 16 + 8)⟩
  if context.players.any (fun p => playerSees context n.center p || playerSees context target p) then
    return result
  let shifted := #[0, -1, 1].find? (fun dx => n.kind == .oldMan || clearForGuide context.grid (rest.1 + dx) rest.2)
  let some offset := shifted | return { result with
    teleport := some rest, needsHousingSearch := true
    progress := { progress with state := { state with resident := { n with homeless := true } } } }
  let position : Vector := ⟨single ((rest.1 + offset) * 16 + 8 - n.width / 2),
    single (rest.2 * 16 - n.height) - 0.1⟩
  let progress := { progress with state := { state with
    resident := { n with position, velocity := ⟨0, 0⟩ } } }
  return { result with teleport := some rest, progress := forceSitting context progress rest }

private def idleTurn (context : Context) (location : Location) (progress : Progress) : Progress :=
  let state := progress.state
  if !atRest context state location.current location.rest then progress else
  let (roll, progress) := progress.next 80
  let n := state.resident
  if roll != 0 || n.turnTimer != 0 then progress else
  { progress with state := { state with resident := { n with direction := -n.direction, turnTimer := 200 } } }

private def nightIdle (context : Context) (location : Location) (progress : Progress) : Progress :=
  let n := progress.state.resident
  let n := { n with turnTimer := if n.turnTimer > 0 then n.turnTimer - 1 else n.turnTimer }
  let progress := { progress with state := { progress.state with resident := n } }
  if location.current == location.rest then
    let vx := n.velocity.x
    let approach := if vx > 0.1 then vx - 0.1 else if vx < -0.1 then vx + 0.1 else 0
    let progress := { progress with state := { progress.state with resident := { n with velocity.x := approach } } }
    let progress := if vx > 0.1 || vx < -0.1 then progress else forceSitting context progress location.rest
    idleTurn context location progress
  else
    let (roll, progress) := progress.next 200
    let direction := if location.current.1 > location.rest.1 then -1 else 1
    { progress with state := { progress.state with resident := { n with
      activity := .walking, direction, timer := single (200 + roll), talkTarget := 0, turnTimer := 0 } } }

private def sitting (context : Context) (progress : Progress) : Progress :=
  let n := progress.state.resident
  let anchor := nudgeTile n (-2)
  let timer := if chair (tile context.grid anchor.1 anchor.2) then n.timer - 1 else 0
  let n := { n with timer, velocity.x := n.velocity.x * 0.8 }
  if timer > 0 then { progress with state := { resident := n, sitting := true } }
  else
    let (wait, progress) := progress.next 60
    let (turn, progress) := progress.next 60
    { progress with state := { resident := { n with
      activity := .idle, timer := single (60 + wait), talkTarget := 0, turnTimer := single (30 + turn) } } }

/-- Override the activity dispatch after the shared pre-activity checks.
None requests the existing ordinary handler. Every result still proceeds
through the shared post-activity checks before gravity and movement. -/
def activity (context : Context) (location : Location) (hasConversation : Bool)
    (progress : Progress) : Option Progress :=
  let n := progress.state.resident
  if progress.state.sitting then some (sitting context progress)
  else if context.dayTime then none
  else if n.activity == .idle && !hasConversation then some (nightIdle context location progress)
  else if n.activity == .walking && atRest context progress.state location.current location.rest then
    let (roll, progress) := progress.next 200
    some { progress with state := { progress.state with resident := { n with
      activity := .idle, timer := single (200 + roll), turnTimer := 60 } } }
  else none

def State.seatAnchor (state : State) : Option Cell :=
  if state.sitting then some (nudgeTile state.resident (-2)) else none

/-- The dry seated sprite pose after movement, before ordinary walking frames. -/
def State.sittingFrame (state : State) : Option Resident := do
  guard state.sitting
  let n := state.resident
  return { n with frameRow := 18, frameCounter := 0, spriteDirection := n.direction }

end Lean4Terria.OriginalForestResidentNight
