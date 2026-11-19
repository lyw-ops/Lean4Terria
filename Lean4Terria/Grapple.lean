import Lean4Terria.Ranged

namespace Lean4Terria

def hookSpeed : Int := 224
def hookFlightTicks : Nat := 16
def hookMaxReach : Int := 14 * tileUnits
def hookReelSpeed : Int := 72

inductive HookKind where
  | basic | dissonance
  deriving BEq, Repr, Inhabited

def dissonanceReach : Int := 500 * (tileUnits / 16)
def dissonanceLifetime : Nat := 36000
private def dissonanceUpdates : Nat := 4
private def dissonanceSpeed : Int := 4 * (tileUnits / 16)

structure HookAnchor where
  tile : TilePos
  x : Int
  y : Int
  kind : HookKind := .basic
  life : Nat := 0
  deriving BEq, Repr, Inhabited

structure HookFlight where
  x : Int
  y : Int
  vx : Int
  vy : Int
  life : Nat
  kind : HookKind := .basic
  returning : Bool := false
  held : Option HookAnchor := none
  deriving BEq, Repr, Inhabited

inductive GrappleState where
  | idle
  | flying (hook : HookFlight)
  | latched (anchor : HookAnchor)
  deriving BEq, Repr, Inhabited

inductive GrappleEvent where
  | latched | teleported | missed | detached
  deriving BEq, Repr

def GrappleState.attachment : GrappleState → Option HookAnchor
  | .flying flight => flight.held
  | .latched anchor => some anchor
  | .idle => none

private def pointTile (x y : Int) : TilePos := ⟨x / tileUnits, y / tileUnits⟩

private def squareLength (dx dy : Int) : Int := dx * dx + dy * dy

private def withinHookReach (player : Player) (x y : Int) : Bool :=
  squareLength (x - bodyCenterX player) (y - bodyCenterY player) ≤ hookMaxReach * hookMaxReach

private def openPoint (world : World) (x y : Int) : Bool :=
  !(world.tile (pointTile x y)).solid

private def dissonanceGrips (world : World) (position : TilePos) : Bool :=
  let tile := world.tile position
  world.contains position && (tile.solid || tile == .platform || tile == .rail)

private def HookFlight.valid (flight : HookFlight) (world : World) (player : Player) : Bool :=
  let speedSquared := squareLength flight.vx flight.vy
  let age : Int := hookFlightTicks - flight.life
  let start := pointTile (flight.x - flight.vx * age) (flight.y - flight.vy * age)
  flight.life > 0 && flight.life ≤ hookFlightTicks &&
    (hookSpeed - 2) * (hookSpeed - 2) ≤ speedSquared && speedSquared ≤ hookSpeed * hookSpeed &&
    world.contains start && openPoint world flight.x flight.y &&
    withinHookReach player flight.x flight.y &&
    terrainLineClear world (bodyCenterX player) (bodyCenterY player) flight.x flight.y

private def HookAnchor.exposed (anchor : HookAnchor) (world : World) : Bool :=
  let left := anchor.tile.x * tileUnits
  let top := anchor.tile.y * tileUnits
  let right := left + tileUnits
  let bottom := top + tileUnits
  let clear (dx dy : Int) := !(world.tile ⟨anchor.tile.x + dx, anchor.tile.y + dy⟩).solid
  world.contains anchor.tile && (world.tile anchor.tile).solid &&
    ((top ≤ anchor.y && anchor.y ≤ bottom &&
      ((anchor.x == left && clear (-1) 0) || (anchor.x == right && clear 1 0))) ||
    (left ≤ anchor.x && anchor.x ≤ right &&
      ((anchor.y == top && clear 0 (-1)) || (anchor.y == bottom && clear 0 1))))

/-- Valid active states have a collision-free rope from the player's center.
A latch may touch a solid corner at the rope's endpoint. -/
private def HookAnchor.valid (anchor : HookAnchor) (world : World) (player : Player) : Bool :=
  player.vx == 0 && player.vy == 0 && !player.grounded && !player.collides world &&
    if anchor.kind == .dissonance then
      anchor.life > 0 && anchor.life ≤ dissonanceLifetime && dissonanceGrips world anchor.tile &&
      anchor.x == anchor.tile.x * tileUnits + tileUnits / 2 &&
      anchor.y == anchor.tile.y * tileUnits + tileUnits / 2 &&
      squareLength (anchor.x - bodyCenterX player) (anchor.y - bodyCenterY player) ≤
        (dissonanceReach + tileUnits * 2) ^ 2
    else
      anchor.life == 0 && anchor.exposed world && withinHookReach player anchor.x anchor.y &&
      terrainLineClear world (bodyCenterX player) (bodyCenterY player)
        anchor.x anchor.y (allowEndpoint := true)

def GrappleState.valid (world : World) (player : Player) : GrappleState → Bool
  | .idle => true
  | .flying flight =>
    if flight.kind == .dissonance then
      let speed := squareLength flight.vx flight.vy
      speed ≥ (dissonanceSpeed - 2) ^ 2 && speed ≤ dissonanceSpeed ^ 2 &&
        flight.life > 0 && flight.life ≤ dissonanceLifetime &&
        world.contains (pointTile flight.x flight.y) &&
        flight.held.all (fun anchor => anchor.valid world player)
    else !flight.returning && flight.held.isNone && flight.valid world player
  | .latched anchor => anchor.valid world player

def fireHook (world : World) (player : Player) (target : TilePos) : Option HookFlight := do
  let x := bodyCenterX player
  let y := bodyCenterY player
  if !world.contains target || !openPoint world x y then none else
    let (vx, vy) ← aimVelocity hookSpeed (target.x * tileUnits + tileUnits / 2 - x)
      (target.y * tileUnits + tileUnits / 2 - y)
    let flight := { x, y, vx, vy, life := hookFlightTicks }
    if flight.valid world player then some flight else none

private def hold (player : Player) : Player :=
  { player with vx := 0, vy := 0, grounded := false }

def fireDissonanceHook (world : World) (player : Player) (x y : Int)
    (held : Option HookAnchor) : Option HookFlight := do
  let startX := bodyCenterX player
  let startY := bodyCenterY player
  let (vx, vy) ← aimVelocity dissonanceSpeed (x - startX) (y - startY)
  let flight : HookFlight := {
    x := startX, y := startY, vx, vy, life := dissonanceLifetime, kind := .dissonance, held }
  if world.contains (pointTile x y) && (GrappleState.flying flight).valid world player then
    some flight
  else none

private def anchorAt (world : World) (player : Player) (hit : TerrainHit) : Option HookAnchor := Id.run do
  let anchor : HookAnchor := { tile := hit.tile, x := hit.x, y := hit.y }
  if (GrappleState.latched anchor).valid world player then return some anchor
  if !world.contains hit.tile || hit.x % tileUnits != 0 || hit.y % tileUnits != 0 then
    return none
  -- A corner may also belong to a neighboring solid with an exposed face.
  let corner := pointTile hit.x hit.y
  for tile in #[corner, ⟨corner.x - 1, corner.y⟩, ⟨corner.x, corner.y - 1⟩,
      ⟨corner.x - 1, corner.y - 1⟩] do
    let anchor := { anchor with tile }
    if (GrappleState.latched anchor).valid world player then return some anchor
  return none

private def reelToward (world : World) (player : Player) (anchor : HookAnchor) : Player :=
  let dx := anchor.x - bodyCenterX player
  let dy := anchor.y - bodyCenterY player
  let (dx, dy) :=
    if squareLength dx dy ≤ hookReelSpeed * hookReelSpeed then (dx, dy)
    else (aimVelocity hookReelSpeed dx dy).getD (0, 0)
  hold (moveBodyDelta world player dx dy)

private def holdToward (world : World) (player : Player) (anchor : HookAnchor) (speed : Int)
    (limitToAnchor := false) : Player :=
  let dx := anchor.x - bodyCenterX player
  let dy := anchor.y - bodyCenterY player
  let (dx, dy) := if limitToAnchor && squareLength dx dy ≤ speed ^ 2 then (dx, dy)
    else (aimVelocity speed dx dy).getD (0, 0)
  hold (moveBodyDelta world player dx dy (dropThrough := true))

/-- DoQueenSlimeHookTeleport advances the full player body toward the
anchor in 150 collision-tested steps of 12 pixels. -/
private def dissonanceTeleport (world : World) (player : Player) (anchor : HookAnchor) : Player := Id.run do
  let mut position := hold player
  for _ in [:150] do
    let next := holdToward world position anchor (12 * (tileUnits / 16))
    if next.x == position.x && next.y == position.y then break
    position := next
  return position

private def ageDissonanceLatch (world : World) (player : Player) (anchor : HookAnchor) :
    Player × Option HookAnchor :=
  let next := { anchor with life := anchor.life - dissonanceUpdates }
  if !next.valid world player then (player, none)
  else (holdToward world player next (11 * (tileUnits / 16)) (limitToAnchor := true), some next)

/-- AI_007 checks the tiles overlapped by the central ten pixels of the hook. -/
def HookFlight.dissonanceContact (flight : HookFlight) (world : World) : Option TilePos := Id.run do
  let radius := 5 * (tileUnits / 16)
  let first := pointTile (flight.x - radius) (flight.y - radius)
  let last := pointTile (flight.x + radius - 1) (flight.y + radius - 1)
  for dx in [:(last.x - first.x + 1).toNat] do
    for dy in [:(last.y - first.y + 1).toNat] do
      let tile : TilePos := ⟨first.x + dx, first.y + dy⟩
      let heldNearby := flight.held.any fun anchor =>
        tile == anchor.tile ||
          ((world.tile anchor.tile == .platform || world.tile anchor.tile == .rail) &&
            (world.tile tile == .platform || world.tile tile == .rail) &&
            (tile.x - anchor.tile.x).natAbs ≤ 2 && (tile.y - anchor.tile.y).natAbs ≤ 2)
      if !heldNearby && dissonanceGrips world tile then return some tile
  return none

private def advanceDissonance (world : World) (player : Player) (initial : HookFlight) :
    Player × GrappleState × Option GrappleEvent := Id.run do
  let (player, held) : Player × Option HookAnchor := match initial.held with
    | some anchor =>
      if anchor.kind == .dissonance then ageDissonanceLatch world player anchor
      else (player, (some anchor).filter (fun anchor => anchor.valid world player))
    | none => (player, none)
  let missed := (player, (held.map GrappleState.latched).getD .idle, some GrappleEvent.missed)
  let mut flight := { initial with held }
  for _ in [:dissonanceUpdates] do
    if flight.life ≤ 1 then return missed
    flight := { flight with life := flight.life - 1 }
    let dx := bodyCenterX player - flight.x
    let dy := bodyCenterY player - flight.y
    if flight.returning then
      if squareLength dx dy < (24 * (tileUnits / 16)) ^ 2 then return missed
      let (vx, vy) := (aimVelocity (11 * (tileUnits / 16)) dx dy).getD (0, 0)
      flight := { flight with x := flight.x + vx, y := flight.y + vy }
    else if let some tile := flight.dissonanceContact world then
      let anchor : HookAnchor := {
        tile, x := tile.x * tileUnits + tileUnits / 2, y := tile.y * tileUnits + tileUnits / 2
        kind := .dissonance, life := flight.life }
      return (dissonanceTeleport world player anchor, .latched anchor, some .teleported)
    else if squareLength dx dy > dissonanceReach ^ 2 ||
        !world.contains (pointTile (flight.x + flight.vx) (flight.y + flight.vy)) then
      flight := { flight with returning := true }
    else
      flight := { flight with x := flight.x + flight.vx, y := flight.y + flight.vy }
  return (player, .flying flight, none)

def advanceGrapple (world : World) (player : Player) (state : GrappleState) (reel : Bool) :
    Player × GrappleState × Option GrappleEvent :=
  match state with
  | .idle => (player, state, none)
  | .flying flight =>
    if flight.kind == .dissonance then advanceDissonance world player flight
    else advanceBasicHook world player flight
  | .latched anchor =>
    if anchor.kind == .dissonance then
      let (moved, attached) := ageDissonanceLatch world player anchor
      (moved, (attached.map GrappleState.latched).getD .idle,
        if attached.isSome then none else some .detached)
    else if !state.valid world player then (player, .idle, some .detached)
    else if !reel then (player, state, none)
    else
      let moved := reelToward world player anchor
      if state.valid world moved then (moved, state, none) else (player, .idle, some .detached)
where
  advanceBasicHook (world : World) (player : Player) (flight : HookFlight) :
      Player × GrappleState × Option GrappleEvent := Id.run do
    let missed := (player, GrappleState.idle, some GrappleEvent.missed)
    if !flight.valid world player then return missed
    if let some hit := terrainHit world flight.x flight.y flight.vx flight.vy then
      let held := hold player
      if let some anchor := anchorAt world held hit then return (held, .latched anchor, some .latched)
      return missed
    if flight.life ≤ 1 then return missed
    let next := GrappleState.flying { flight with
      x := flight.x + flight.vx, y := flight.y + flight.vy, life := flight.life - 1 }
    return if next.valid world player then (player, next, none) else missed

end Lean4Terria
