import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

def lavaSlimeWidth : Nat := 26
def lavaSlimeHeight : Nat := 19
def lavaSlimeScale : Float32 := 1.1
def lavaSlimeHealth : Nat := 50
def lavaSlimeDefense : Nat := 10
def lavaSlimeDamage : Nat := 15
def lavaSlimeCoins : Float32 := 120
def lavaSlimeSlotQuarters : Nat := 4
def lavaSlimeFrameCount : Nat := 2

inductive UnderworldFlyerKind where
  | hellbat | demon | voodooDemon
  deriving BEq, Repr, Inhabited

namespace UnderworldFlyerKind

def sprite : UnderworldFlyerKind → Nat
  | .hellbat => 60
  | .demon => 62
  | .voodooDemon => 66

def scale (kind : UnderworldFlyerKind) : Float32 := if kind == .hellbat then 1.1 else 1
def width (kind : UnderworldFlyerKind) : Nat := if kind == .hellbat then 24 else 28
def height (kind : UnderworldFlyerKind) : Nat := if kind == .hellbat then 19 else 48
def health : UnderworldFlyerKind → Nat
  | .hellbat => 46
  | .demon => 120
  | .voodooDemon => 140
def defense (_ : UnderworldFlyerKind) : Nat := 8
def damage (kind : UnderworldFlyerKind) : Nat := if kind == .hellbat then 35 else 32
def coins : UnderworldFlyerKind → Float32
  | .hellbat => 120
  | .demon => 300
  | .voodooDemon => 1000
def slotQuarters (kind : UnderworldFlyerKind) : Nat := if kind == .hellbat then 2 else 8
def frameCount (_ : UnderworldFlyerKind) : Nat := 5

end UnderworldFlyerKind

/-- Native NPC positions and velocities in pixels. Float32 fields retain their
bit patterns through persistence, including the previous collision velocity. -/
structure LavaSlime where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  lastJumpX : Float32 := 0
  health : Nat := lavaSlimeHealth
  jumpClock : Int := 0
  retargetClock : Nat := 0
  direction : Int := 0
  directionY : Int := 1
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  frameClock : Nat := 0
  frame : Nat := 0
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq LavaSlime where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.lastJumpX.toBits == b.lastJumpX.toBits &&
    a.health == b.health && a.jumpClock == b.jumpClock && a.retargetClock == b.retargetClock &&
    a.direction == b.direction && a.directionY == b.directionY && a.collideX == b.collideX &&
    a.collideY == b.collideY && a.wet == b.wet && a.frameClock == b.frameClock &&
    a.frame == b.frame && a.timeLeft == b.timeLeft

structure UnderworldFlyer where
  id : Nat
  kind : UnderworldFlyerKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  oldVx : Float32 := 0
  oldVy : Float32 := 0
  rotation : Float32 := 0
  health : Nat
  direction : Int := 0
  directionY : Int := 1
  spriteDirection : Int := -1
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  searchClock : Nat := 0
  orbitClock : Int := 0
  attackClock : Nat := 0
  frameClock : Nat := 0
  frame : Nat := 0
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq UnderworldFlyer where
  beq a b := a.id == b.id && a.kind == b.kind &&
    a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.oldVx.toBits == b.oldVx.toBits && a.oldVy.toBits == b.oldVy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.health == b.health &&
    a.direction == b.direction && a.directionY == b.directionY && a.spriteDirection == b.spriteDirection &&
    a.collideX == b.collideX && a.collideY == b.collideY && a.wet == b.wet &&
    a.searchClock == b.searchClock && a.orbitClock == b.orbitClock && a.attackClock == b.attackClock &&
    a.frameClock == b.frameClock && a.frame == b.frame && a.timeLeft == b.timeLeft

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def edge (tile : Int) : Float32 := single (tile * 16)
private def trunc (value : Float32) : Int := value.toInt32.toInt
private def floorTile (value : Float32) : Int := trunc (value / 16).floor
private def lastTile (value : Float32) : Int := trunc (value / 16).ceil - 1

private def npcBox (x y : Float32) (width height : Nat) : HitBox :=
  let left := trunc x * 16
  let top := trunc y * 16
  ⟨left, top, left + (width : Int) * 16, top + (height : Int) * 16⟩

def LavaSlime.centerX (slime : LavaSlime) : Float32 := slime.x + 13
def LavaSlime.centerY (slime : LavaSlime) : Float32 := slime.y + 9.5
def LavaSlime.hitBox (slime : LavaSlime) : HitBox := npcBox slime.x slime.y lavaSlimeWidth lavaSlimeHeight
def LavaSlime.contact (slime : LavaSlime) (player : Player) : Option Int :=
  if slime.hitBox.intersects (HitBox.ofNpcTarget player) then
    some (if slime.centerX < single (bodyCenterX player) / 16 then 1 else -1)
  else none

def UnderworldFlyer.centerX (flyer : UnderworldFlyer) : Float32 :=
  flyer.x + single flyer.kind.width * 0.5
def UnderworldFlyer.centerY (flyer : UnderworldFlyer) : Float32 :=
  flyer.y + single flyer.kind.height * 0.5
def UnderworldFlyer.hitBox (flyer : UnderworldFlyer) : HitBox := npcBox flyer.x flyer.y flyer.kind.width flyer.kind.height
def UnderworldFlyer.contact (flyer : UnderworldFlyer) (player : Player) : Option Int :=
  if flyer.hitBox.intersects (HitBox.ofNpcTarget player) then
    some (if flyer.centerX < single (bodyCenterX player) / 16 then 1 else -1)
  else none

private def validPosition (value : Float32) (tiles : Nat) : Bool :=
  value.isFinite && -4096 ≤ value && value < edge tiles + 4096
private def validVelocity (value : Float32) : Bool := value.isFinite && value.abs ≤ 32
private def validDirection (value : Int) : Bool := value == -1 || value == 1
private def validLifetime (value : Nat) : Bool := value > 0 && value ≤ 937

def LavaSlime.valid (slime : LavaSlime) (world : World) : Bool :=
  slime.id > 0 && slime.health > 0 && slime.health ≤ lavaSlimeHealth &&
    validPosition slime.x world.width && validPosition slime.y world.height &&
    validPosition slime.lastJumpX world.width && validVelocity slime.vx && validVelocity slime.vy &&
    -2120 ≤ slime.jumpClock && slime.jumpClock ≤ 0 && slime.retargetClock ≤ 200 &&
    (slime.direction == 0 || validDirection slime.direction) && validDirection slime.directionY &&
    slime.frame < lavaSlimeFrameCount && slime.frameClock < 8 && validLifetime slime.timeLeft

def UnderworldFlyer.valid (flyer : UnderworldFlyer) (world : World) : Bool :=
  let bat := flyer.kind == .hellbat
  flyer.id > 0 && flyer.health > 0 && flyer.health ≤ flyer.kind.health &&
    validPosition flyer.x world.width && validPosition flyer.y world.height &&
    validVelocity flyer.vx && validVelocity flyer.vy &&
    validVelocity flyer.oldVx && validVelocity flyer.oldVy &&
    flyer.rotation.isFinite && flyer.rotation.abs ≤ 3.2 &&
    (flyer.direction == 0 || validDirection flyer.direction) && validDirection flyer.directionY &&
    (flyer.spriteDirection == 0 || validDirection flyer.spriteDirection) &&
    flyer.searchClock ≤ 1000 && -300 ≤ flyer.orbitClock && flyer.orbitClock ≤ 300 &&
    flyer.attackClock ≤ (if bat then 0 else 16777216) &&
    flyer.frame < (if bat then 4 else 5) && flyer.frameClock < (if bat then 6 else 25) &&
    validLifetime flyer.timeLeft

def LavaSlime.atFloor (id : Nat) (column floor : Int) : LavaSlime :=
  { id, x := edge column + 8 - 13, y := edge floor - 19 }

def UnderworldFlyer.atFloor (id : Nat) (kind : UnderworldFlyerKind) (column floor : Int) : UnderworldFlyer :=
  { id, kind, x := edge column + 8 - single (kind.width / 2)
    y := edge floor - single kind.height, health := kind.health }

def npcSolidOverlap (world : World) (x y : Float32) (width height : Nat) : Bool := Id.run do
  let col := floorTile x
  let row := floorTile y
  for dy in [:(lastTile (y + single height) - row + 1).toNat] do
    for dx in [:(lastTile (x + single width) - col + 1).toNat] do
      if (world.tile ⟨col + dx, row + dy⟩).solid then return true
  return false

def LavaSlime.clear (slime : LavaSlime) (world : World) : Bool :=
  !npcSolidOverlap world slime.x slime.y lavaSlimeWidth lavaSlimeHeight

def UnderworldFlyer.clear (flyer : UnderworldFlyer) (world : World) : Bool :=
  !npcSolidOverlap world flyer.x flyer.y flyer.kind.width flyer.kind.height

private def targetDirections (x y : Float32) (width height : Nat) (player : Player) : Int × Int :=
  let targetX := single (player.x.tdiv 16 + playerWidth / 32)
  let targetY := single (player.y.tdiv 16 + playerHeight / 32)
  (if targetX < x + single (width / 2) then -1 else 1,
   if targetY < y + single (height / 2) then -1 else 1)

def npcActiveTime (player : Player) (x y : Float32) (width height timeLeft : Nat) : Option Nat := do
  let target := HitBox.ofNpcTarget player
  let x := x + single (width / 2)
  let y := y + single (height / 2)
  let area := fun (left top halfWidth halfHeight : Int) =>
    (⟨left * 16, top * 16, (left + 2 * halfWidth) * 16, (top + 2 * halfHeight) * 16⟩ : HitBox)
  if !(area (trunc (x - 4032)) (trunc (y - 2520)) 4032 2520).intersects target then failure
  let left := (x.toFloat - 960 - width.toFloat).toInt32.toInt
  let top := (y.toFloat - 600 - height.toFloat).toInt32.toInt
  let view := area left top (960 + width) (600 + height)
  let remaining := (if view.intersects target then 750 else timeLeft) - 1
  if remaining == 0 then failure
  return remaining

/-- The two NPC.StrikeNPC_Inner paths share a compressed impulse and apply
resistance a second time on hits of at most one tenth of maximum health. -/
def npcKnockbackVelocity (vx vy force resistance lift : Float32)
    (damage health : Nat) (direction : Int) (crit : Bool) : Float32 × Float32 :=
  if force ≤ 0 then (vx, vy) else
  let reduce := fun (value cap coefficient : Float32) =>
    if value > cap then cap + (value - cap) * coefficient else value
  let impulse := reduce (reduce (reduce (reduce (force * resistance) 8 0.9) 10 0.8) 12 0.7) 14 0.6
  let impulse := min impulse 16
  let impulse := if crit then impulse * 1.4 else impulse
  let upward := -impulse * lift
  let side := single direction
  if damage * 10 ≤ health then
    (impulse * side * resistance, upward * resistance)
  else
    let horizontal := if direction != 0 && side * vx < impulse then
        let vx := if side * vx < 0 then vx + side * impulse else vx
        side * min impulse (side * (vx + side * impulse))
      else vx
    (horizontal, if vy > upward then max upward (vy + upward) else vy)

def LavaSlime.knockback (slime : LavaSlime) (damage : Nat) (direction : Int)
    (force : Float32) (crit := false) : LavaSlime :=
  let (vx, vy) := npcKnockbackVelocity slime.vx slime.vy force 1 0.75 damage lavaSlimeHealth direction crit
  { slime with vx, vy }

def UnderworldFlyer.knockback (flyer : UnderworldFlyer) (damage : Nat) (direction : Int)
    (force : Float32) (crit := false) : UnderworldFlyer :=
  let (vx, vy) := npcKnockbackVelocity flyer.vx flyer.vy force 0.8 0.5 damage flyer.kind.health direction crit
  { flyer with vx, vy }

/-- Vanilla WetCollision tests the center rectangle (at most ten pixels wide)
against each occupied liquid surface. Water and lava give these actors the
same displacement multiplier, and both types of Underworld actor ignore lava damage. -/
private def immersed (world : World) (water magma : Water) (x y : Float32) (width height : Nat) : Bool := Id.run do
  let probeWidth := min width 10
  let probeHeight := height / 2
  let left := x + single (width / 2) - single (probeWidth / 2)
  let top := y + single (height / 2) - single (probeHeight / 2)
  let right := left + single probeWidth
  let bottom := top + single probeHeight
  let col := floorTile left
  let row := floorTile top
  for dx in [:(lastTile right - col + 1).toNat] do
    for dy in [:(lastTile bottom - row + 1).toNat] do
      let pos : TilePos := ⟨col + dx, row + dy⟩
      let level := max (water.level world pos) (magma.level world pos)
      if level == 0 then continue
      let gap := single ((256 : Int) - level) / 16
      let surface := edge pos.y + gap
      if top < surface + single (16 - trunc gap) && bottom > surface then return true
  return false

def underworldTargetWet (world : World) (water magma : Water) (player : Player) : Bool :=
  immersed world water magma (single player.x / 16) (single player.y / 16)
    (playerWidth / 16).toNat (playerHeight / 16).toNat

structure NpcStep where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  oldVx : Float32
  oldVy : Float32
  collideX : Bool
  collideY : Bool
  wet : Bool

/-- The full-tile branch of TileCollision. Terraria scans columns first and
resolves the floor, wall and ceiling cases in that order for each block.
NPCs of style 14 set both platform fall-through flags. -/
def moveNpc (world : World) (water magma : Water) (x y vx vy : Float32)
    (width height : Nat) (wasWet platforms : Bool) : NpcStep := Id.run do
  let wet := immersed world water magma x y width height
  let vx := if wasWet && !wet then vx * 0.5 else vx
  let w := single width
  let h := single height
  let goalX := x + vx
  let goalY := y + vy
  let minCol := (trunc x).tdiv 16 - 1
  let maxCol := (trunc (x + w)).tdiv 16 + 2
  let minRow := (trunc y).tdiv 16 - 1
  let maxRow := (trunc (y + h)).tdiv 16 + 2
  let mut dx := vx
  let mut dy := vy
  let mut wall : TilePos := ⟨-1, -1⟩
  let mut surface : TilePos := ⟨-1, -1⟩
  let mut closestFloor := edge (maxRow + 3)
  let mut ceiling := false
  for col in [:(maxCol - minCol).toNat] do
    for row in [:(maxRow - minRow).toNat] do
      let pos : TilePos := ⟨minCol + col, minRow + row⟩
      let tile := world.tile pos
      let solid := tile.solid
      if !solid && !(platforms && tile == .platform) then continue
      let tileX := edge pos.x
      let tileY := edge pos.y
      if goalX + w ≤ tileX || goalX ≥ tileX + 16 || goalY + h ≤ tileY || goalY ≥ tileY + 16 then
        continue
      if y + h ≤ tileY then
        if tileY < closestFloor then
          surface := pos
          if surface.x != wall.x then
            dy := tileY - (y + h)
            closestFloor := tileY
      else if solid && (x + w ≤ tileX || x ≥ tileX + 16) then
        wall := pos
        if wall.y != surface.y then
          dx := if x + w ≤ tileX then tileX - (x + w) else tileX + 16 - x
        if wall.x == surface.x then dy := vy
      else if solid && y ≥ tileY + 16 then
        surface := pos
        dy := tileY + 16 - y + 0.01
        ceiling := true
        if surface.y == wall.y then dx := vx
  if ceiling then dy := 0.01
  let hitX := dx != vx
  let hitY := dy != vy
  let movedX := if wet && !hitX then dx * 0.5 else dx
  let movedY := if wet && !hitY then dy * 0.5 else dy
  return ⟨x + movedX, y + movedY, dx, dy, vx, vy, hitX, hitY, wet⟩

private def LavaSlime.face (slime : LavaSlime) (player : Player) (dead : Bool) : LavaSlime :=
  if dead then slime else
    let (direction, directionY) := targetDirections slime.x slime.y lavaSlimeWidth lavaSlimeHeight player
    { slime with direction, directionY }

/-- Type 59's style-1 AI through the end of the movement branch. The
Boolean is the native crouch animation flag. -/
def LavaSlime.advanceAI (slime : LavaSlime) (player : Player) (dead hostile : Bool)
    (embedded : LavaSlime → Bool) : LavaSlime × Bool := Id.run do
  let mut next := if slime.retargetClock > 1 then { slime with retargetClock := slime.retargetClock - 1 }
    else slime
  if next.wet then
    let vy : Float32 := if next.collideY then -2 else next.vy
    if vy < 0 && next.lastJumpX == next.x then
      next := { next with direction := -next.direction, retargetClock := 200 }
    let lastJumpX := if vy > 0 then next.x else next.lastJumpX
    let buoyant := if vy > 2 then vy * 0.9 else if next.directionY < 0 then vy - 0.8 else vy
    next := { next with vy := max (-10) (buoyant - 0.5), lastJumpX }
    if next.retargetClock == 1 && hostile then next := next.face player dead
  if next.retargetClock == 0 then
    next := { (next.face player dead) with retargetClock := 1, jumpClock := -100 }
  if next.vy == 0 then
    if next.collideY && embedded next then
      next := { next with x := next.x - (next.vx + single next.direction) }
    if next.lastJumpX == next.x then
      next := { next with direction := -next.direction, retargetClock := 200 }
    let slowed := next.vx * 0.8
    let vx := if -0.1 < slowed.toFloat && slowed.toFloat < 0.1 then 0 else slowed
    let jumpClock := next.jumpClock + if hostile then 4 else 3
    next := { next with vx, jumpClock, lastJumpX := 0 }
    let jump := if jumpClock ≥ 0 then 1 else if -1000 ≤ jumpClock && jumpClock ≤ -500 then 2
      else if -2000 ≤ jumpClock && jumpClock ≤ -1500 then 3 else 0
    if jump == 0 then return (next, jumpClock ≥ -30)
    if next.retargetClock == 1 && hostile then next := next.face player dead
    let side := single next.direction
    if jump == 3 then
      return ({ next with
        vx := next.vx + 3 * side + 0.5 * side, vy := -10
        jumpClock := -200, lastJumpX := next.x }, false)
    return ({ next with
      vx := next.vx + 2 * side + 2 * side, vy := -6
      jumpClock := if jump == 1 then -1120 else -2120 }, false)
  let side := single next.direction
  if next.direction != 0 && side * next.vx < 3 then
    if next.collideX && next.vx.abs == 0.2 then next := { next with x := next.x - 1.4 * side }
    if next.collideY && embedded next then next := { next with x := next.x - (next.vx + side) }
    let vx := if side.toFloat * next.vx.toFloat > -0.01 then next.vx + 0.2 * side else next.vx * 0.93
    next := { next with vx }
  return (next, false)

private def snapVelocity (vx : Float32) : Float32 :=
  if -0.005 < vx.toFloat && vx.toFloat < 0.005 then 0 else vx

def LavaSlime.advance (slime : LavaSlime) (world : World) (water magma : Water) (player : Player)
    (dead : Bool) (daylight := false) (surface := 40) : Option LavaSlime := do
  if slime.health == 0 then failure
  let hostile := !daylight || slime.health < lavaSlimeHealth || slime.y > edge surface
  let (next, crouching) := slime.advanceAI player dead hostile (fun actor => !actor.clear world)
  let vx := snapVelocity next.vx
  let widthScale := single world.width / 4200
  let widthRatio := widthScale * widthScale
  let depth := ((slime.y / 16 - (60 + 10 * widthRatio)).toFloat / (surface.toFloat / 6)).toFloat32
  let gravity := if slime.wet then 0.2 else 0.3 * max 0.25 (min 1 depth)
  let vy := min (if slime.wet then 7 else 10) (next.vy + gravity)
  let moved := moveNpc world water magma next.x next.y vx vy lavaSlimeWidth lavaSlimeHeight slime.wet true
  let frameClock := slime.frameClock + if crouching then 3 else if moved.vx != 0 || moved.vy != 0 then 2 else 1
  let timeLeft ← npcActiveTime player moved.x moved.y lavaSlimeWidth lavaSlimeHeight slime.timeLeft
  return { next with
    x := moved.x, y := moved.y, vx := moved.vx, vy := moved.vy, wet := moved.wet
    collideX := moved.collideX, collideY := moved.collideY, timeLeft
    frame := if frameClock ≥ 8 then 1 - slime.frame else slime.frame
    frameClock := if frameClock ≥ 8 then 0 else frameClock }

/-- The full-tile branch of Collision.CanHit. The native walk advances along
the greater remaining axis and checks for paired blocks on either side. -/
def npcCanSee (world : World) (npcX npcY : Float32) (width height : Nat) (player : Player) : Bool := Id.run do
  let col := fun x => max 1 (min ((world.width : Int) - 2) (x.tdiv 16))
  let row := fun y => max 1 (min ((world.height : Int) - 2) (y.tdiv 16))
  let mut x := col (trunc npcX + width / 2)
  let mut y := row (trunc npcY + height / 2)
  let endX := col (player.x.tdiv 16 + playerWidth / 32)
  let endY := row (player.y.tdiv 16 + playerHeight / 32)
  for _ in [:((endX - x).natAbs + (endY - y).natAbs)] do
    if (endX - x).natAbs > (endY - y).natAbs then
      x := x + if x < endX then 1 else -1
      if (world.tile ⟨x, y - 1⟩).solid && (world.tile ⟨x, y + 1⟩).solid then return false
    else
      y := y + if y < endY then 1 else -1
      if (world.tile ⟨x - 1, y⟩).solid && (world.tile ⟨x + 1, y⟩).solid then return false
    if (world.tile ⟨x, y⟩).solid then return false
  return true

def UnderworldFlyer.canSee (flyer : UnderworldFlyer) (world : World) (player : Player) : Bool :=
  npcCanSee world flyer.x flyer.y flyer.kind.width flyer.kind.height player

private def steerAxis (v : Float32) (direction : Int) (cap acceleration reverseAssist brake : Float32) : Float32 :=
  let side := single direction
  if direction == 0 || side * v ≥ cap then v else
    let accelerated := v + side * acceleration
    let adjusted := if side * accelerated < -cap then accelerated + side * reverseAssist
      else if side * accelerated < 0 then accelerated - side * brake else accelerated
    if side * adjusted > cap then side * cap else adjusted

/-- Shared portion of style 14, before the separate Demon attack and NPC
movement. A lost line of sight starts a slow alternating search after 200
ticks. Native targeting still runs while the flyer is wet. -/
def UnderworldFlyer.advanceAI (flyer : UnderworldFlyer) (player : Player)
    (dead lineOfSight : Bool) (targetWet := false) (illuminant := false) : UnderworldFlyer := Id.run do
  let width := if illuminant then 26 else flyer.kind.width
  let height := if illuminant then 20 else flyer.kind.height
  let face := fun (actor : UnderworldFlyer) =>
    if dead then actor else
      let (direction, directionY) := targetDirections actor.x actor.y width height player
      { actor with direction, directionY }
  let mut next := flyer
  if next.collideX then
    let rebound := next.oldVx * -0.5
    let vx := if next.direction == -1 && 0 < rebound && rebound < 2 then 2
      else if next.direction == 1 && -2 < rebound && rebound < 0 then -2 else rebound
    next := { next with vx }
  if next.collideY then
    let rebound := next.oldVy * -0.5
    let vy := if 0 < rebound && rebound < 1 then 1
      else if -1 < rebound && rebound < 0 then -1 else rebound
    next := { next with vy }
  next := face next
  next := { next with
    vx := steerAxis next.vx next.direction 4 0.1 0.1 0.05
    vy := steerAxis next.vy next.directionY 1.5 0.04 0.05 0.03 }
  if next.wet then
    let vy := max (-4) ((if next.vy > 0 then next.vy * 0.95 else next.vy) - 0.5)
    next := face { next with vy }
  let hellbat := next.kind == .hellbat && !illuminant
  next := { next with
    vx := steerAxis next.vx next.direction 4 0.1 (if hellbat then 0.07 else 0.1) (if hellbat then 0.03 else 0.05)
    vy := steerAxis next.vy next.directionY 1.5 0.04 (if hellbat then 0.03 else 0.05) (if hellbat then 0.02 else 0.03)
    searchClock := next.searchClock + 1 }
  if next.searchClock ≤ 200 then return next
  let searchClock := if (!targetWet && lineOfSight) || next.searchClock > 1000 then 0 else next.searchClock
  let orbit := next.orbitClock + 1
  let bat := next.kind == .hellbat || illuminant
  let yCap : Float32 := if bat then 1.5 else 1.25
  let yStep : Float32 := if bat then 0.1 else 0.07
  let xCap : Float32 := if bat then 4 else 3
  let xStep : Float32 := if bat then 0.2 else 0.12
  let vy := if orbit > 0 then
      if next.vy < yCap then next.vy + yStep else next.vy
    else if next.vy > -yCap then next.vy - yStep else next.vy
  let vx := if orbit < -150 || orbit > 150 then
      if next.vx < xCap then next.vx + xStep else next.vx
    else if next.vx > -xCap then next.vx - xStep else next.vx
  return { next with searchClock, orbitClock := if orbit > 300 then -300 else orbit, vx, vy }

def UnderworldFlyer.advanceMotion (flyer : UnderworldFlyer) (world : World) (water magma : Water)
    (player : Player) : Option UnderworldFlyer := do
  if flyer.health == 0 then failure
  let kind := flyer.kind
  let moved := moveNpc world water magma flyer.x flyer.y (snapVelocity flyer.vx) flyer.vy
    kind.width kind.height flyer.wet false
  let timeLeft ← npcActiveTime player moved.x moved.y kind.width kind.height flyer.timeLeft
  let bat := kind == .hellbat
  let frameClock := (flyer.frameClock + 1) % (if bat then 6 else 25)
  let frame := if !bat then frameClock / 5 else
    if frameClock == 0 then (flyer.frame + 1) % 4 else flyer.frame
  let spriteDirection := if !bat then flyer.direction else
    if moved.vx < 0 then -1 else if moved.vx > 0 then 1 else flyer.spriteDirection
  return { flyer with
    x := moved.x, y := moved.y, vx := moved.vx, vy := moved.vy
    oldVx := moved.oldVx, oldVy := moved.oldVy, collideX := moved.collideX, collideY := moved.collideY
    wet := moved.wet, rotation := moved.vx * 0.1, spriteDirection, frameClock, frame, timeLeft }

def UnderworldFlyer.advance (flyer : UnderworldFlyer) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) : Option UnderworldFlyer :=
  let moved := flyer.advanceAI player dead (flyer.canSee world player) (underworldTargetWet world water magma player)
  moved.advanceMotion world water magma player

end Lean4Terria
