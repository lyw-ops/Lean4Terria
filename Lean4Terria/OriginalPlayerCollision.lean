import Lean4Terria.OriginalCollision

namespace Lean4Terria.OriginalPlayerCollision

open OriginalCollision (Grid Vector Result single tileIndex slopeId)
open OriginalWorld (Tile)

structure Flags where
  up : Bool := false
  down : Bool := false
  stair : Bool := false
  stairFall : Bool := false
  sloping : Bool := false
  deriving BEq, Repr

structure Body where
  position : Vector
  velocity : Vector
  width : Nat := 20
  height : Nat := 42
  gravity : Float32 := 0.4
  gravityDirection : Float32 := 1
  mountType : Option Nat := none
  mountCart : Bool := false
  carpetFrame : Int := -1
  onTrack : Bool := false
  hasGrapple : Bool := false
  controlUp : Bool := false
  controlDown : Bool := false
  sloping : Bool := false
  stairFall : Bool := false
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 1
  jump : Int := 0
  fallStart : Int := 0
  waterWalk : Bool := false
  waterWalk2 : Bool := false
  collision : Flags := {}
  deriving BEq, Repr

/-- Player-specific collision uses the shortened minecart hitbox and enables
the slope side-face rule when both dimensions exceed one tile. -/
def Body.isConsideredStandingStill (body : Body) : Bool :=
  body.velocity.x.abs.toFloat < 0.05 && body.velocity.y.abs.toFloat < 0.05

def Body.tileCollision (body : Body) (grid : Grid) (position velocity : Vector)
    (fallThrough ignorePlatforms : Bool) : Result :=
  let height := if body.onTrack then body.height - 10 else body.height
  OriginalCollision.tileCollision grid position velocity body.width height
    { fallThrough, fall2 := ignorePlatforms, gravityDirection := body.gravityDirection.toInt32.toInt
      hoik := body.width > 16 && height > 16 }

def Body.slopeDownMovement (body : Body) (grid : Grid) : Body :=
  if body.mountType == some 48 then { body with sloping := false }
  else
    let velocity := OriginalCollision.walkDownSlope grid body.position body.velocity body.width body.height
      (body.gravity * body.gravityDirection)
    { body with velocity, sloping := velocity.y != body.velocity.y }

/-- Ignoring platforms, down control, a grapple, or inverted
gravity starts a stair fall. A large stair adjustment also offsets the sprite. -/
def Body.slopingCollision (body : Body) (grid : Grid) (fallThrough ignorePlatforms : Bool) : Body × (Bool × Bool) :=
  let stairFall := body.stairFall || ignorePlatforms || body.controlDown || body.hasGrapple || body.gravityDirection == -1
  let contact := OriginalCollision.slopeCollision grid body.position body.velocity body.width body.height stairFall
  let dy := contact.position.y - body.position.y
  let bigStep := contact.stair && dy.abs > 8 + body.velocity.x.abs
  let velocity := if body.gravityDirection == -1 && contact.velocity.y == 0.0101 then
    { contact.velocity with y := 0 } else contact.velocity
  ({ body with
     position := contact.position
     velocity
     stairFall := contact.stairFall || (fallThrough && stairFall)
     gfxOffY := if bigStep then body.gfxOffY - dy else body.gfxOffY
     stepSpeed := if bigStep then 4 else body.stepSpeed
     collision := {
       up := contact.hitAbove
       down := contact.hitBelow
       stair := contact.stair
       stairFall := contact.stairFall } },
   (contact.hitAbove, contact.hitBelow))

private def tileAt (grid : Grid) (x y : Int) : Tile := grid.tileAt x.toNat y.toNat

private def inWorld (grid : Grid) (x y : Int) (margin : Int := 0) : Bool :=
  x ≥ margin && y ≥ margin && x < (grid.width : Int) - margin && y < (grid.height : Int) - margin

private def active (tile : Tile) : Bool := tile.id.isSome && !tile.actuated

private def rulesFor (grid : Grid) (tile : Tile) : OriginalCollision.TileRules :=
  grid.rules (tile.id.getD 0)

private def openSpace (grid : Grid) (tile : Tile) : Bool :=
  let rules := rulesFor grid tile
  !active tile || !rules.solid || rules.solidTop

private def frameY (tile : Tile) : Int := (tile.frame.map (·.2)).getD (-1)

/-- Step into a drop of more than seven and less than seventeen pixels while
retaining the visual height offset used by the character renderer. -/
def Body.stepDown (body : Body) (grid : Grid) : Body := Id.run do
  let width := single body.width
  let height := single body.height
  let bottom := body.position.y + height
  let nextX := body.position.x + body.velocity.x
  let row := tileIndex (((bottom / 16).floor * 16 - height) + height + 4)
  let firstX := tileIndex nextX
  let lastX := tileIndex (nextX + width)
  let heightTiles := (body.height + 15) / 16
  let mut floor := single ((row + heightTiles) * 16)
  let worldBottomMargin := single grid.height - 42
  for dx in [:(lastX - firstX + 1).toNat] do
    let x := firstX + dx
    for dy in [:2] do
      let y := row + dy
      if !inWorld grid x y 1 then continue
      let tile := tileAt grid x y
      let left := x * 16
      let top := y * 16
      if body.waterWalk || body.waterWalk2 then
        let fill := (tile.liquid.map (·.amount)).getD 0
        let above := tileAt grid x (y - 1)
        if fill > 0 && !(above.liquid.any (·.amount != 0)) then
          let px := body.position.x.toInt32.toInt
          let py := body.position.y.toInt32.toInt
          let surface := top + 16 - (fill.toNat / 32 * 2 + 2)
          if px + body.width > left && px < left + 16 && py + body.height > top - 17 && py < top - 1 then
            floor := min floor (single surface)
      let rules := rulesFor grid tile
      if single y < worldBottomMargin && !(active tile && (rules.solid || rules.solidTop)) then continue
      if body.position.x + width ≥ single left && body.position.x ≤ single (left + 16) &&
          bottom ≥ single (top - 17) && body.position.y ≤ single (top - 1) then
        let tileTop := if tile.shape == .halfBlock then top + 8 else top
        floor := min floor (single tileTop)
  let drop := floor - bottom
  if 7 < drop && drop < 17 then
    return { body with
      position.y := floor - height
      gfxOffY := body.gfxOffY + (bottom - floor)
      stepSpeed := if drop > 9 then 2.5 else 1.5 }
  return body

/-- Step over a short ledge, checking the leading column and the rear corner
of the hitbox for clearance in the current gravity direction. -/
def Body.stepUp (body : Body) (grid : Grid) : Body := Id.run do
  let dir : Int := if body.velocity.x < 0 then -1 else if body.velocity.x > 0 then 1 else 0
  let grav := body.gravityDirection.toInt32.toInt
  let normal := grav == 1
  let width := single body.width
  let height := single body.height
  let nextX := body.position.x + body.velocity.x
  let halfWidth := body.width / 2
  let centerX := body.position.x + single halfWidth
  let x := tileIndex (nextX + single halfWidth + single (((halfWidth : Int) + 1) * dir))
  let bottom := body.position.y + height
  let y := if normal then tileIndex (bottom - 1)
    else ((body.position.y.toFloat + 0.1) / 16).toInt32.toInt
  let heightTiles := (body.height + 15) / 16
  if !inWorld grid x y 1 || y ≥ (grid.height : Int) - 40 then return body
  for offset in [1:heightTiles + 2] do
    if !inWorld grid x (y - (offset : Int) * grav) then return body
  let backY := y - (heightTiles : Int) * grav
  if !inWorld grid (x - dir) backY || !openSpace grid (tileAt grid (x - dir) backY) then return body
  for offset in [2:heightTiles + 1] do
    if !openSpace grid (tileAt grid x (y - (offset : Int) * grav)) then return body
  let ahead := tileAt grid x y
  let above := tileAt grid x (y - grav)
  let aheadRules := rulesFor grid ahead
  let aboveRules := rulesFor grid above
  let topmost := tileAt grid x (y - ((heightTiles : Int) + 1) * grav)
  let aboveSlope := slopeId above.shape
  let aheadSlope := slopeId ahead.shape
  let left := single (x * 16)
  let tileTop := single (y * 16)
  if !(left < nextX + width && left + 16 > nextX) then return body
  if normal then
    let clearDirect := openSpace grid above || (aboveSlope == 1 && centerX > left) ||
      (aboveSlope == 2 && centerX < left + 16) || (above.shape == .halfBlock && openSpace grid topmost)
    if !clearDirect then return body
    let topSlope := 0 < aheadSlope && aheadSlope ≤ 2
    let slopeAllowsStep := !topSlope || ((aheadSlope == 1 && centerX < left ||
      aheadSlope == 2 && centerX > left + 16) && bottom > tileTop)
    let support := (!aboveRules.solid || !active above) && body.controlUp &&
      ((aheadRules.solidTop && frameY ahead == 0) || aheadRules.platform || ahead.id == some 380)
    let canStep := (active ahead && slopeAllowsStep && ((aheadRules.solid && !aheadRules.solidTop) || support)) ||
      (active above && above.shape == .halfBlock)
    if !canStep || (aheadRules.solidTop && aboveRules.solidTop) then return body
    let surface := if above.shape == .halfBlock then tileTop - 8
      else if ahead.shape == .halfBlock then tileTop + 8 else tileTop
    let rise := bottom - surface
    if surface < bottom && rise.toFloat ≤ 16.1 then
      return { body with
        position.y := surface - height
        gfxOffY := body.gfxOffY + rise
        stepSpeed := if rise < 9 then 1 else 2 }
  else
    let clearDirect := openSpace grid above || aboveSlope != 0 ||
      (above.shape == .halfBlock && openSpace grid topmost)
    let canStep := (active ahead && ((aheadRules.solid && !aheadRules.solidTop) ||
      (body.controlUp && aheadRules.solidTop && frameY ahead == 0 && (!aboveRules.solid || !active above)))) ||
      (active above && above.shape == .halfBlock)
    if !clearDirect || !canStep || aheadSlope > 2 || aboveRules.platform then return body
    let surface := tileTop + 16
    let rise := surface - body.position.y
    if rise > 0 && rise.toFloat ≤ 16.1 then
      return { body with
        position.y := surface
        velocity.y := 0
        gfxOffY := body.gfxOffY - rise
        stepSpeed := if rise < 9 then 1 else 2 }
  return body

/-- Effects of the touched tiles applied between substeps. The collision
solver supplies the character state and whether the impact can trigger a bounce. -/
structure ContactEffects where
  onTouchingTiles : Body → Bool → Body := fun body _ => body
  onConveyor : Body → Body := id

private def magnitude (vector : Vector) : Float32 :=
  (vector.x * vector.x + vector.y * vector.y).toFloat.sqrt.toFloat32

private def collideAndMove (body : Body) (grid : Grid) (fallThrough ignorePlatforms : Bool)
    (previousStepVelocity : Option Vector := none) : Body :=
  let hit := body.tileCollision grid body.position body.velocity fallThrough ignorePlatforms
  let velocity := if body.waterWalk || body.waterWalk2 then
    OriginalCollision.waterCollision grid body.position hit.displacement body.width body.height fallThrough body.waterWalk
    else hit.displacement
  let fallStart := if (body.waterWalk || body.waterWalk2) &&
    velocity != previousStepVelocity.getD hit.displacement then tileIndex body.position.y else body.fallStart
  { body with
    position := body.position + velocity
    velocity
    fallStart
    jump := if hit.hitAbove && body.gravityDirection == 1 then 0 else body.jump
    collision := { body.collision with up := hit.hitAbove, down := hit.hitBelow } }

/-- Advance a dry player with its supplied velocity. Fast movement uses
substeps, running the ledge and slope corrections before accumulating the
resulting velocity, in the same order as `Player.DryCollision`. -/
def Body.dryCollision (initial : Body) (grid : Grid) (fallThrough ignorePlatforms : Bool)
    (effects : ContactEffects := {}) : Body := Id.run do
  let limit := min (min 16 (single initial.width - 0.5)) (single initial.height - 0.5)
  let speed := magnitude initial.velocity
  if !(speed > limit && limit > 0) then
    return collideAndMove initial grid fallThrough ignorePlatforms
  let full := initial.tileCollision grid initial.position initial.velocity fallThrough ignorePlatforms
  let invSpeed := 1 / speed
  let unit : Vector := ⟨initial.velocity.x * invSpeed,
    if full.displacement.y == 0 then 0 else initial.velocity.y * invSpeed⟩
  let horse := initial.mountType.any fun mount => mount == 7 || mount == 8 || mount == 12 || mount == 44 || mount == 49
  let mut body := { initial with collision := { initial.collision with up := full.hitAbove, down := full.hitBelow } }
  let mut remaining := speed
  let mut total : Vector := ⟨0, 0⟩
  while remaining > 0 do
    let length := min remaining limit
    remaining := remaining - length
    let step : Vector := ⟨unit.x * length, unit.y * length⟩
    body := ({ body with velocity := step }).slopeDownMovement grid
    let beforeStep := body.velocity
    if body.velocity.y == body.gravity &&
        (body.mountType.isNone || (!body.mountCart && body.mountType != some 48 && !horse)) then
      body := body.stepDown grid
    let canStepUp := if body.gravityDirection == -1 then
      (body.carpetFrame != -1 || body.velocity.y ≤ body.gravity) && !body.controlUp
      else horse || ((body.carpetFrame != -1 || body.velocity.y ≥ body.gravity) &&
        !body.controlDown && !body.mountCart)
    if canStepUp then body := body.stepUp grid
    body := collideAndMove body grid fallThrough ignorePlatforms (some beforeStep)
    body := effects.onTouchingTiles body (body.velocity.y > body.gravity || body.velocity.y < -body.gravity)
    body := (body.slopingCollision grid fallThrough ignorePlatforms).1
    body := effects.onConveyor body
    total := total + body.velocity
  return { body with velocity := total }

end Lean4Terria.OriginalPlayerCollision
