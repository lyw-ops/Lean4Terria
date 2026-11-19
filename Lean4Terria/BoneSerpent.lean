import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

def boneSerpentWidth : Nat := 22
def boneSerpentHeight : Nat := 22
def boneSerpentHealth : Nat := 300
def boneSerpentCoins : Float32 := 1200
def boneSerpentSlotQuarters : Nat := 24

inductive BoneSerpentPart where
  | head | body | tail
  deriving BEq, Repr, Inhabited

namespace BoneSerpentPart

def sprite : BoneSerpentPart → Nat
  | .head => 39
  | .body => 40
  | .tail => 41

def damage : BoneSerpentPart → Nat
  | .head => 36
  | .body => 20
  | .tail => 16

def defense : BoneSerpentPart → Nat
  | .head => 12
  | .body | .tail => 18

end BoneSerpentPart

/-- NPC positions are their top-left corner in original 16-pixel tile space. -/
structure BoneSerpentSegment where
  x : Float32
  y : Float32
  rotation : Float32 := 0
  deriving Repr, Inhabited

instance : BEq BoneSerpentSegment where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.rotation.toBits == b.rotation.toBits

/-- One linked chain with shared head life. Only the head exists between
NewNPC and the first AI update. Each linked NPC's array index stays fixed. -/
structure BoneSerpent where
  id : Nat
  segments : Array BoneSerpentSegment
  bodyCount : Nat
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := boneSerpentHealth
  timeLeft : Nat := 937
  direction : Int := 0
  directionY : Int := 1
  buried : Bool := false
  soundDelay : Nat := 0
  deriving Repr, Inhabited

instance : BEq BoneSerpent where
  beq a b := a.id == b.id && a.segments == b.segments && a.bodyCount == b.bodyCount &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.health == b.health &&
    a.timeLeft == b.timeLeft && a.direction == b.direction && a.directionY == b.directionY &&
    a.buried == b.buried && a.soundDelay == b.soundDelay

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def trunc (value : Float32) : Int := value.toInt32.toInt
private def pixels (value : Int) : Float32 := single value / 16

def BoneSerpentSegment.centerX (segment : BoneSerpentSegment) : Float32 := segment.x + 11
def BoneSerpentSegment.centerY (segment : BoneSerpentSegment) : Float32 := segment.y + 11

def BoneSerpentSegment.hitBox (segment : BoneSerpentSegment) : HitBox :=
  let left := trunc segment.x * 16
  let top := trunc segment.y * 16
  ⟨left, top, left + 352, top + 352⟩

def BoneSerpent.partAt (serpent : BoneSerpent) (index : Nat) : Option BoneSerpentPart :=
  if index ≥ serpent.segments.size then none
  else if index == 0 then some .head
  else if index + 1 == serpent.segments.size then some .tail
  else some .body

def BoneSerpent.targets (serpent : BoneSerpent) : Array (Nat × HitBox) :=
  serpent.segments.mapIdx fun index segment => (index, segment.hitBox)

def BoneSerpent.contact (serpent : BoneSerpent) (player : Player) : Option (Nat × Int) := Id.run do
  let playerBox := HitBox.ofNpcTarget player
  for index in [:serpent.segments.size] do
    let segment := serpent.segments[index]!
    if segment.hitBox.intersects playerBox then
      if let some part := serpent.partAt index then
        return some (part.damage, if segment.centerX < pixels (bodyCenterX player) then 1 else -1)
  return none

def BoneSerpent.damageSegment (serpent : BoneSerpent) (index raw : Nat) (ignoreDefense := false) : BoneSerpent :=
  if raw == 0 || serpent.health == 0 then serpent else
    match serpent.partAt index with
    | none => serpent
    | some part =>
      let amount := if ignoreDefense then raw else max 1 (raw - part.defense / 2)
      { serpent with health := serpent.health - amount }

def BoneSerpent.valid (serpent : BoneSerpent) (world : World) : Bool :=
  let validPosition (v : Float32) (extent : Nat) :=
    v.isFinite && -4096 ≤ v && v < single ((extent : Int) * 16) + 4096
  let validVelocity (v : Float32) := v.isFinite && v.abs ≤ 16
  serpent.id > 0 && serpent.health > 0 && serpent.health ≤ boneSerpentHealth &&
    14 ≤ serpent.bodyCount && serpent.bodyCount ≤ 22 &&
    (serpent.segments.size == 1 || serpent.segments.size == serpent.bodyCount + 2) &&
    validVelocity serpent.vx && validVelocity serpent.vy && serpent.direction.natAbs ≤ 1 &&
    (serpent.directionY == -1 || serpent.directionY == 1) &&
    serpent.timeLeft > 0 && serpent.timeLeft ≤ 937 && serpent.soundDelay ≤ 20 &&
    serpent.segments.all (fun segment =>
      validPosition segment.x world.width && validPosition segment.y world.height &&
      segment.rotation.isFinite && -1.572 ≤ segment.rotation && segment.rotation ≤ 4.713)

def BoneSerpent.atFloor (id : Nat) (column floor : Int) (bodyRoll : Nat) : BoneSerpent :=
  let head : BoneSerpentSegment := { x := single (column * 16 + 8 - 11), y := single (floor * 16 - 22) }
  { id, segments := #[head], bodyCount := 14 + bodyRoll % 9 }

/-- Style 6 steers as underground if its current rectangle overlaps a solid
or top-solid tile, or a cell with more than 64 liquid units. -/
def BoneSerpentSegment.inTerrain (segment : BoneSerpentSegment) (world : World) (water magma : Water) (size : Float32 := 22)
    (honey : Water := ⟨ByteArray.empty⟩) : Bool := Id.run do
  let left := max 0 (trunc (segment.x / 16) - 1)
  let top := max 0 (trunc (segment.y / 16) - 1)
  let right := min (world.width : Int) (trunc ((segment.x + size) / 16) + 2)
  let bottom := min (world.height : Int) (trunc ((segment.y + size) / 16) + 2)
  for col in [:(right - left).toNat] do
    for row in [:(bottom - top).toNat] do
      let pos : TilePos := ⟨left + col, top + row⟩
      let x := single (pos.x * 16)
      let y := single (pos.y * 16)
      if segment.x + size ≤ x || segment.x ≥ x + 16 ||
          segment.y + size ≤ y || segment.y ≥ y + 16 then continue
      let tile := world.tile pos
      if tile.solid || tile == .platform || water.level world pos > 64 || magma.level world pos > 64 ||
          honey.level world pos > 64 then return true
  return false

private def tooFarToFall (segment : BoneSerpentSegment) (player : Player) : Bool :=
  let x := player.x.tdiv 16
  let y := player.y.tdiv 16
  let area : HitBox := ⟨(x - 1000) * 16, (y - 1000) * 16, (x + 1000) * 16, (y + 1000) * 16⟩
  !segment.hitBox.intersects area

private def targetTilePixel (value : Float32) : Float32 := single (trunc (value / 16) * 16)
private def distance (dx dy : Float32) : Float32 := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
private def turn (vy vx offset : Float32) : Float32 := (Float.atan2 vy.toFloat vx.toFloat).toFloat32 + offset

private def speed : Float32 := 9
private def acceleration : Float32 := 0.1

private def adjust (value target amount : Float32) : Float32 :=
  if value < target then value + amount else if value > target then value - amount else value

private def sameSign (a b : Float32) : Bool := (a < 0 && b < 0) || (a > 0 && b > 0)
private def pushAlong (value amount : Float32) : Float32 :=
  if value > 0 then value + amount else value - amount

def wormFall (speed acceleration vx vy dx : Float32) : Float32 × Float32 :=
  let vy := min speed (vy + if vy < 0 then 0.08 else 0.11)
  let vx := if (vx.abs + vy.abs).toFloat < speed.toFloat * 0.4 then
      if vx < 0 then vx - acceleration * 1.1 else vx + acceleration * 1.1
    else if vy == speed then adjust vx dx acceleration
    else if vy > 4 then
      if vx < 0 then vx + acceleration * 0.9 else vx - acceleration * 0.9
    else vx
  (vx, vy)

def wormSteer (speed acceleration vx vy dx dy wantedX wantedY : Float32) : Float32 × Float32 :=
  if sameSign vx wantedX || sameSign vy wantedY then
    let vx := adjust vx wantedX acceleration
    let vy := adjust vy wantedY acceleration
    let threshold := speed * 0.2
    let vy := if wantedY.abs < threshold && sameSign vx (-wantedX) then
        pushAlong vy (acceleration * 2) else vy
    let vx := if wantedX.abs < threshold && sameSign vy (-wantedY) then
        pushAlong vx (acceleration * 2) else vx
    (vx, vy)
  else if dx.abs > dy.abs then
    let vx := adjust vx wantedX (acceleration * 1.1)
    (vx, if vx.abs + vy.abs < speed * 0.5 then pushAlong vy acceleration else vy)
  else
    let vy := adjust vy wantedY (acceleration * 1.1)
    (if vx.abs + vy.abs < speed * 0.5 then pushAlong vx acceleration else vx, vy)

/-- Native retreat uses the midpoint between rock layer and world bottom.
The compact world places its rock layer at half of its height. -/
def boneSerpentRetreatDepth (world : World) : Float32 := single (world.height * 12)

/-- The type-39 branches of NPC.AI_006_Worms before generic position integration. -/
def BoneSerpent.advanceHeadAI (serpent : BoneSerpent) (world : World) (player : Player)
    (dead inTerrain : Bool) : Option BoneSerpent := do
  let head ← serpent.segments[0]?
  let buried := inTerrain || tooFarToFall head player
  if dead && buried && head.y > boneSerpentRetreatDepth world then failure
  let dx := targetTilePixel (pixels (bodyCenterX player)) - targetTilePixel head.centerX
  let dy := targetTilePixel (pixels (bodyCenterY player)) - targetTilePixel head.centerY
  let distance := distance dx dy
  let delay := serpent.soundDelay - 1
  let soundDelay := if buried && delay == 0 then (trunc (max 10 (min 20 (distance / 40)))).toNat else delay
  let (vx, vy) := if buried then
      let scale := speed / distance
      let wantedX := if dead then 0 else dx * scale
      let wantedY := if dead then speed else dy * scale
      wormSteer speed acceleration serpent.vx serpent.vy dx dy wantedX wantedY
    else wormFall speed acceleration serpent.vx serpent.vy dx
  let retarget := !dead && (serpent.segments.size == 1 || !buried)
  let tx := single (player.x.tdiv 16 + playerWidth / 32)
  let ty := single (player.y.tdiv 16 + playerHeight / 32)
  let direction := if !retarget then serpent.direction else if tx < head.centerX then -1 else 1
  let directionY := if !retarget then serpent.directionY else if ty < head.centerY then -1 else 1
  let timeLeft := if dead then min 300 serpent.timeLeft else serpent.timeLeft
  let head := { head with rotation := turn vy vx 1.5707963705062866 }
  return { serpent with
    vx, vy, buried, soundDelay, direction, directionY, timeLeft
    segments := serpent.segments.set! 0 head }

/-- Style 6 uses the previous NPC's already integrated center and applies
(delta × ((length - width) / length)) directly to the follower position. -/
def BoneSerpentSegment.follow (segment previous : BoneSerpentSegment) (width : Float32 := 22) : BoneSerpentSegment :=
  let dx := previous.centerX - segment.centerX
  let dy := previous.centerY - segment.centerY
  let length := distance dx dy
  let rotation := turn dy dx 1.5700000524520874
  if length == 0 then { segment with rotation } else
    let scale := (length - width) / length
    { x := segment.x + dx * scale, y := segment.y + dy * scale, rotation }

private def BoneSerpent.lifetime (serpent : BoneSerpent) (head : BoneSerpentSegment) (player : Player) : Nat :=
  let left := (head.centerX.toFloat - 982).toInt32.toInt
  let top := (head.centerY.toFloat - 622).toInt32.toInt
  let view : HitBox := ⟨left * 16, top * 16, (left + 1964) * 16, (top + 1244) * 16⟩
  -- CheckActive lists type 39 as active while there is an active player.
  -- A player inside its view rectangle still resets the normal timer.
  (if view.intersects (HitBox.ofNpcTarget player) then 750 else serpent.timeLeft) - 1

def BoneSerpent.advance (serpent : BoneSerpent) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) : Option BoneSerpent := do
  if serpent.health == 0 then failure
  let originalHead ← serpent.segments[0]?
  if originalHead.y > single (world.height * 16 + 22) then failure
  let inTerrain := originalHead.inTerrain world water magma
  let moved ← serpent.advanceHeadAI world player dead inTerrain
  let steeredHead ← moved.segments[0]?
  let vx := if -0.005 < moved.vx.toFloat && moved.vx.toFloat < 0.005 then 0 else moved.vx
  let head : BoneSerpentSegment :=
    { steeredHead with x := originalHead.x + vx, y := originalHead.y + moved.vy }
  let timeLeft := moved.lifetime head player
  if timeLeft == 0 then failure
  let followerSpawn : BoneSerpentSegment := {
    x := single (trunc originalHead.centerX - 11)
    y := single (trunc (originalHead.y + 22) - 22) }
  let segments := Id.run do
    let mut next := #[head]
    for index in [1:moved.bodyCount + 2] do
      let segment := moved.segments[index]?.getD followerSpawn
      next := next.push (segment.follow next[index - 1]!)
    return next
  return { moved with segments, vx, timeLeft }

end Lean4Terria
