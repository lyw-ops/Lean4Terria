import Lean4Terria.OriginalWorld

namespace Lean4Terria.OriginalCollision

open OriginalWorld (Terrain Tile TileShape)

structure Vector where
  x : Float32
  y : Float32
  deriving BEq, Repr

instance : Add Vector where
  add a b := ⟨a.x + b.x, a.y + b.y⟩

structure TileRules where
  solid : Bool := false
  solidTop : Bool := false
  platform : Bool := false
  closedDoor : Bool := false
  deriving Inhabited

/-- Use the effective tile rules for the current simulation step. The original
game temporarily changes the solidity of some tiles during character movement. -/
structure Grid where
  width : Nat
  height : Nat
  tileAt : Nat → Nat → Tile
  rules : UInt16 → TileRules

def Grid.ofTerrain (terrain : Terrain) (rules : UInt16 → TileRules) : Grid :=
  ⟨terrain.header.width, terrain.header.height, fun x y => (terrain.tile? x y).getD default, rules⟩

structure Options where
  fallThrough : Bool := false
  fall2 : Bool := false
  gravityDirection : Int := 1
  ignoreDoors : Bool := false
  ignoreAetheriumPlatforms : Bool := false
  hoik : Bool := false

structure Result where
  displacement : Vector
  hitAbove : Bool := false
  hitBelow : Bool := false
  deriving BEq, Repr

def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32

def tileIndex (pixel : Float32) : Int := (pixel / 16).toInt32.toInt

private def clampIndex (upper : Nat) (index : Int) : Nat := min upper index.toNat

def slopeId : TileShape → UInt8
  | .slope id => id
  | _ => 0

private def scanBounds (grid : Grid) (pos : Vector) (right bottom : Float32) : Nat × Nat × Nat × Nat :=
  (clampIndex (grid.width - 1) (tileIndex pos.x - 1),
   clampIndex (grid.width - 1) (tileIndex right + 2),
   clampIndex (grid.height - 40) (tileIndex pos.y - 1),
   clampIndex (grid.height - 40) (tileIndex bottom + 2))

private def collidableTile (grid : Grid) (x y : Nat) (ignoreDoors ignoreAetheriumPlatforms : Bool) :
    Option (Tile × TileRules) := do
  let tile := grid.tileAt x y
  let id ← tile.id
  guard (!tile.actuated)
  let rules := grid.rules id
  let frameY := (tile.frame.map (·.2)).getD (-1)
  guard (rules.solid || (rules.solidTop && frameY == 0))
  guard (!(ignoreDoors && rules.closedDoor))
  guard (!(ignoreAetheriumPlatforms && id == 19 && frameY / 18 == 50))
  return (tile, rules)

private def skipsSlopeFace (slope : UInt8) (pos velocity : Vector) (right bottom left top height : Float32) : Bool :=
  match slope with
  | 1 => bottom - velocity.x.abs ≤ top + height && pos.x ≥ left
  | 2 => bottom - velocity.x.abs ≤ top + height && right ≤ left + 16
  | 3 => pos.y + velocity.x.abs ≥ top && pos.x ≥ left
  | 4 => pos.y + velocity.x.abs ≥ top && right ≤ left + 16
  | _ => false

/-- Terraria 1.4.5.8 `Collision.TileCollision`, in pixels. The scan order and
the horizontal and vertical contact coordinates determine corner resolution. -/
def tileCollision (grid : Grid) (pos velocity : Vector) (width height : Nat)
    (options : Options := {}) : Result := Id.run do
  let width := single width
  let height := single height
  let endPos := pos + velocity
  let right := pos.x + width
  let bottom := pos.y + height
  let (xStart, xEnd, yStart, yEnd) := scanBounds grid pos right bottom
  let mut result : Result := { displacement := velocity }
  let mut blockingSideX : Int := -1
  let mut blockingSideY : Int := -1
  let mut blockingVerticalX : Int := -1
  let mut blockingVerticalY : Int := -1
  let mut floorLimit := single ((yEnd + 3) * 16 : Nat)
  for x in [xStart:xEnd] do
    for y in [yStart:yEnd] do
      let some (tile, rules) := collidableTile grid x y options.ignoreDoors options.ignoreAetheriumPlatforms
        | continue
      let left := single (x * 16 : Nat)
      let tileTop := single (y * 16 : Nat)
      let half := tile.shape == .halfBlock
      let top := if half then tileTop + 8 else tileTop
      let tileHeight : Float32 := if half then 8 else 16
      if !(endPos.x + width > left && endPos.x < left + 16 &&
          endPos.y + height > top && endPos.y < top + tileHeight) then continue
      let slope := slopeId tile.shape
      if skipsSlopeFace slope pos velocity right bottom left top tileHeight then continue
      if bottom ≤ top then
        result := { result with hitBelow := true }
        if rules.solidTop && options.fallThrough && (velocity.y ≤ 1 || options.fall2) then continue
        if top ≥ floorLimit then continue
        blockingVerticalX := x
        blockingVerticalY := if half then (y : Int) + 1 else y
        if blockingVerticalX == blockingSideX || (0 < slope && slope ≤ 2) then continue
        let bias : Float32 := if options.gravityDirection == -1 then -0.01 else 0
        result := { result with displacement.y := top - bottom + bias }
        floorLimit := top
      else if right ≤ left && !rules.solidTop then
        if options.hoik && x > 0 then
          let neighbor := slopeId (grid.tileAt (x - 1) y).shape
          if neighbor == 2 || neighbor == 4 then continue
        blockingSideX := x
        blockingSideY := y
        if blockingSideY != blockingVerticalY then
          result := { result with displacement.x := left - right }
        if blockingVerticalX == blockingSideX then
          result := { result with displacement.y := velocity.y }
      else if pos.x ≥ left + 16 && !rules.solidTop then
        if options.hoik then
          let neighbor := slopeId (grid.tileAt (x + 1) y).shape
          if neighbor == 1 || neighbor == 3 then continue
        blockingSideX := x
        blockingSideY := y
        if blockingSideY != blockingVerticalY then
          result := { result with displacement.x := left + 16 - pos.x }
        if blockingVerticalX == blockingSideX then
          result := { result with displacement.y := velocity.y }
      else if pos.y ≥ top + tileHeight && !rules.solidTop then
        blockingVerticalX := x
        blockingVerticalY := y
        let bias : Float32 := if options.gravityDirection == 1 then 0.01 else 0
        result := { result with hitAbove := true, displacement.y := top + tileHeight - pos.y + bias }
        if blockingVerticalY == blockingSideY then
          result := { result with displacement.x := velocity.x }
  return result

/-- Walking on liquid uses the surface of a cell with no liquid immediately above it. -/
def waterCollision (grid : Grid) (pos velocity : Vector) (width height : Nat)
    (fallThrough lavaWalk : Bool) : Vector := Id.run do
  if fallThrough then return velocity
  let widthPx := single width
  let heightPx := single height
  let bottom := pos.y + heightPx
  let moved := pos + velocity
  let (xStart, xEnd, yStart, yEnd) := scanBounds grid pos (pos.x + widthPx) bottom
  let mut resolved := velocity
  for x in [xStart:xEnd] do
    for y in [max 1 yStart:yEnd] do
      let some liquid := (grid.tileAt x y).liquid | continue
      if liquid.amount == 0 || (!lavaWalk && liquid.kind == .lava) then continue
      if (grid.tileAt x (y - 1)).liquid.any (·.amount != 0) then continue
      let depth := liquid.amount.toNat / 32 * 2 + 2
      let top := single (y * 16 + 16 - depth : Nat)
      let left := single (x * 16 : Nat)
      if moved.x + widthPx > left && moved.x < left + 16 && moved.y + heightPx > top &&
          moved.y < top + single depth && bottom ≤ top then
        resolved := { resolved with y := top - bottom }
  return resolved

structure SlopeResult where
  position : Vector
  velocity : Vector
  hitAbove : Bool := false
  hitBelow : Bool := false
  stair : Bool := false
  stairFall : Bool := false
  deriving BEq, Repr

/-- Moves the body onto a diagonal face, then checks whether a neighbouring
block obstructs that correction. Width, height, and vectors are in pixels. -/
def slopeCollision (grid : Grid) (pos velocity : Vector) (width height : Nat)
    (fall ignoreAetheriumPlatforms : Bool := false) : SlopeResult := Id.run do
  let widthPx := single width
  let heightPx := single height
  let right := pos.x + widthPx
  let bottom := pos.y + heightPx
  let (xStart, xEnd, yStart, yEnd) := scanBounds grid pos right bottom
  let mut result : SlopeResult := { position := pos, velocity }
  let mut lowestCeiling := pos.y
  let mut highestFloor := pos.y
  let mut slopeContacts : UInt8 := 0
  for x in [xStart:xEnd] do
    for y in [yStart:yEnd] do
      let some (tile, rules) := collidableTile grid x y false ignoreAetheriumPlatforms | continue
      let left := single (x * 16 : Nat)
      let top := single (y * 16 : Nat)
      let collisionTop := if tile.shape == .halfBlock then top + 8 else top
      if !(right > left && pos.x < left + 16 && bottom > collisionTop && pos.y < top + 16) then
        continue
      let slope := slopeId tile.shape
      if rules.platform then
        if velocity.y < 0 || bottom < top || bottom - (1 + velocity.x.abs) > top + 16 then continue
        let movingDownStair := (slope == 1 && velocity.x ≥ 0) || (slope == 2 && velocity.x ≤ 0)
        if movingDownStair && bottom / 16 - 1 == single y then continue
      let fallThrough := fall && rules.platform
      let across := if slope == 1 || slope == 3 then pos.x - left else left + 16 - right
      if slope == 3 || slope == 4 then
        if across ≥ 0 then
          if pos.y > top + 16 - across then continue
          let y' := pos.y + (top + 16 - pos.y - across)
          if y' ≤ lowestCeiling then continue
          result := { result with position.y := y' }
          lowestCeiling := y'
          if result.velocity.y < 0.0101 then result := { result with velocity.y := 0.0101 }
          slopeContacts := slopeContacts ||| (1 <<< slope)
        else if pos.y > top && result.position.y < top + 16 then
          result := { result with position.y := top + 16 }
          if result.velocity.y < 0.0101 then result := { result with velocity.y := 0.0101 }
      else if slope == 1 || slope == 2 then
        if across ≥ 0 then
          if bottom < top + across then continue
          let y' := pos.y + (top - bottom + across)
          if y' ≥ highestFloor then continue
          if fallThrough then
            result := { result with stairFall := true }
            continue
          result := { result with position.y := y', stair := rules.platform }
          highestFloor := y'
          if result.velocity.y > 0 then result := { result with velocity.y := 0 }
          slopeContacts := slopeContacts ||| (1 <<< slope)
        else
          if rules.platform && bottom - 4 - velocity.x.abs > top then
            if fallThrough then result := { result with stairFall := true }
            continue
          let y' := top - heightPx
          if result.position.y ≤ y' then continue
          if fallThrough then
            result := { result with stairFall := true }
            continue
          result := { result with position.y := y', stair := rules.platform }
          if result.velocity.y > 0 then result := { result with velocity.y := 0 }
  let delta : Vector := ⟨result.position.x - pos.x, result.position.y - pos.y⟩
  let contact := tileCollision grid pos delta width height { hoik := true }
  let adjustedY := contact.displacement.y
  result := { result with hitAbove := contact.hitAbove, hitBelow := contact.hitBelow }
  if adjustedY > delta.y then
    let difference := delta.y - adjustedY
    let mut x := result.position.x
    if slopeContacts &&& 2 != 0 then x := pos.x - difference
    if slopeContacts &&& 4 != 0 then x := pos.x + difference
    result := { result with position := ⟨x, pos.y + adjustedY⟩, velocity := ⟨0, 0⟩, hitAbove := false }
  else if adjustedY < delta.y then
    let difference := adjustedY - delta.y
    let mut x := result.position.x
    if slopeContacts &&& 8 != 0 then x := pos.x - difference
    if slopeContacts &&& 16 != 0 then x := pos.x + difference
    result := { result with position := ⟨x, pos.y + adjustedY⟩, velocity := ⟨0, 0⟩ }
  return result

/-- Keep a body moving down a ramp when its vertical velocity equals gravity. -/
def walkDownSlope (grid : Grid) (pos velocity : Vector) (width height : Nat)
    (gravity : Float32 := 0) : Vector := Id.run do
  if velocity.y != gravity then return velocity
  let right := pos.x + single width
  let bottom := pos.y + single height
  let x0 := clampIndex (grid.width - 1) (tileIndex pos.x)
  let x1 := clampIndex (grid.width - 1) (tileIndex right)
  let y0 := clampIndex (grid.height - 42) (tileIndex (bottom + 4))
  let bodyLeft := pos.x.toInt32.toInt
  let bodyTop := pos.y.toInt32.toInt
  let bodyRight := bodyLeft + width
  let bodyBottom := bodyTop + height
  let direction : UInt8 := if velocity.x < 0 then 2 else 1
  let mut bestTop := (y0 + 3 : Int) * 16
  let mut best : Option (Nat × Nat × UInt8) := none
  for x in [x0:x1 + 1] do
    for y in [y0:y0 + 2] do
      let tile := grid.tileAt x y
      let some id := tile.id | continue
      let rules := grid.rules id
      if tile.actuated || !(rules.solid || rules.solidTop) then continue
      let left := (x : Int) * 16
      let top := (y : Int) * 16
      if !(bodyRight > left && bodyLeft < left + 16 && bodyBottom > top - 17 && bodyTop < top - 1) then
        continue
      let top := if tile.shape == .halfBlock then top + 8 else top
      let slope := slopeId tile.shape
      if top > bestTop then continue
      if top == bestTop && (slope == 0 || (best.any (fun (_, _, oldSlope) => oldSlope != 0) &&
          slope != direction)) then continue
      best := some (x, y, slope)
      bestTop := top
  let some (x, y, slope) := best | return velocity
  let left := single (x * 16 : Nat)
  let top := single (y * 16 : Nat)
  let across := if slope == 1 then pos.x - left else left + 16 - right
  if ((slope == 1 && velocity.x > 0) || (slope == 2 && velocity.x < 0)) && bottom ≥ top + across then
    return { velocity with y := velocity.y + velocity.x.abs }
  return velocity

end Lean4Terria.OriginalCollision
