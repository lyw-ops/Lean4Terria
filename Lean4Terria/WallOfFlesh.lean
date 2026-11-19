import Lean4Terria.UnderworldShots

namespace Lean4Terria

def wallOfFleshMaxHealth : Nat := 8000
def wallOfFleshSize : Nat := 120
def wallOfFleshScale : Float32 := 1.2
def wallOfFleshCoins : Float32 := 80000
def wallLaserLife : Nat := 600
def wallLaserUpdates : Nat := 3

inductive WallPart where
  | mouth | upperEye | lowerEye
  deriving BEq, Repr, Inhabited

def WallPart.defense (part : WallPart) : Nat := if part == .mouth then 12 else 0
def WallPart.damage (_ : WallPart) : Nat := 50

/-- NPC positions use the original 16-pixel tile scale and mark the top-left
corner. The mouth owns the shared life. Eye clocks are independent. -/
structure WallEye where
  x : Float32
  y : Float32
  vy : Float32 := 0
  rotation : Float32 := 0
  upper : Bool
  laserClock : Nat := 0
  volley : Nat := 0
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq WallEye where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits && a.upper == b.upper &&
    a.laserClock == b.laserClock && a.volley == b.volley && a.frame == b.frame && a.frameClock == b.frameClock

structure WallOfFlesh where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  direction : Int := 0
  health : Nat := wallOfFleshMaxHealth
  top : Int := -1
  bottom : Int := -1
  eyes : Array WallEye := #[]
  leechClock : Nat := 0
  leeches : Nat := 0
  roarClock : Float32 := 0
  deadFade : Float32 := 0
  frame : Nat := 0
  frameClock : Int := 0
  deriving Repr, Inhabited

instance : BEq WallOfFlesh where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.direction == b.direction && a.health == b.health && a.top == b.top && a.bottom == b.bottom &&
    a.eyes == b.eyes && a.leechClock == b.leechClock && a.leeches == b.leeches &&
    a.roarClock.toBits == b.roarClock.toBits && a.deadFade.toBits == b.deadFade.toBits &&
    a.frame == b.frame && a.frameClock == b.frameClock

/-- Hostile Eye Laser, projectile 83. Position is its center, and its time
left counts individual projectile updates. -/
structure WallLaser where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  life : Nat := wallLaserLife
  penetration : Nat := 3
  slot : Option Nat := none
  deriving Repr, Inhabited

instance : BEq WallLaser where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.damage == b.damage && a.life == b.life && a.penetration == b.penetration && a.slot == b.slot

structure WallStepEvents where
  lasers : Array WallLaser := #[]
  spawnHungry : Bool := false
  spawnLeech : Bool := false
  roared : Bool := false
  hungrySpawn : Option (Float32 × Float32) := none
  leechSpawn : Option (Float32 × Float32 × Float32) := none
  deriving Repr, Inhabited

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def trunc (n : Float32) : Int := n.toInt32.toInt
private def pixels (units : Int) : Float32 := single units / 16
private def tile (n : Float32) : Int := trunc (n / 16)

def WallOfFlesh.centerX (wall : WallOfFlesh) : Float32 := wall.x + 60
def WallOfFlesh.centerY (wall : WallOfFlesh) : Float32 := wall.y + 60
def WallEye.centerX (eye : WallEye) : Float32 := eye.x + 60
def WallEye.centerY (eye : WallEye) : Float32 := eye.y + 60
def WallEye.part (eye : WallEye) : WallPart := if eye.upper then .upperEye else .lowerEye

private def box (x y : Float32) (size : Int) : HitBox :=
  let left := trunc x * 16
  let top := trunc y * 16
  ⟨left, top, left + size * 16, top + size * 16⟩

def WallOfFlesh.hitBox (wall : WallOfFlesh) : HitBox := box wall.x wall.y wallOfFleshSize
def WallEye.hitBox (eye : WallEye) : HitBox := box eye.x eye.y wallOfFleshSize
def WallLaser.hitBox (laser : WallLaser) : HitBox := box (laser.x - 2) (laser.y - 2) 4

def WallOfFlesh.targets (wall : WallOfFlesh) : Array (WallPart × HitBox) :=
  #[(.mouth, wall.hitBox)] ++ wall.eyes.map (fun eye => (eye.part, eye.hitBox))

def WallOfFlesh.contact (wall : WallOfFlesh) (player : Player) : Option Int :=
  let playerBox := HitBox.ofNpcTarget player
  if wall.targets.any (fun (_, body) => body.intersects playerBox) then
    some (if wall.centerX < pixels (bodyCenterX player) then 1 else -1)
  else none

def WallOfFlesh.damagePart (wall : WallOfFlesh) (part : WallPart) (damage : Nat)
    (ignoreDefense := false) : WallOfFlesh :=
  if damage == 0 || (part != .mouth && !wall.eyes.any (·.part == part)) then wall
  else
    let dealt := if ignoreDefense then damage else max 1 (damage - part.defense / 2)
    { wall with health := wall.health - dealt }

/-- Native AI uses the whole wall width from the mouth, and a fractional
vertical anchor for each Hungry. Fractions near the ends can be outside [0,1]. -/
def WallOfFlesh.attachment (wall : WallOfFlesh) (fraction : Float32) : Float32 × Float32 :=
  (wall.centerX, single wall.top + single (wall.bottom - wall.top) * fraction)

private def validPosition (value : Float32) (extent : Nat) (margin : Float32) : Bool :=
  value.isFinite && -margin ≤ value && value < single ((extent : Int) * 16) + margin

private def validAngle (angle : Float32) : Bool := angle.isFinite && -0.002 ≤ angle && angle ≤ 6.282

def WallEye.valid (eye : WallEye) (world : World) : Bool :=
  validPosition eye.x world.width 120 && validPosition eye.y world.height 120 &&
    (eye.vy == -1 || eye.vy == 0 || eye.vy == 1) && validAngle eye.rotation &&
    eye.laserClock ≤ 16777216 && eye.volley ≤ 10 && eye.frame < 2 && eye.frameClock < 12

def WallOfFlesh.valid (wall : WallOfFlesh) (world : World) : Bool :=
  validPosition wall.x world.width 120 && validPosition wall.y world.height 120 &&
    wall.vx.isFinite && wall.vx.abs ≤ 3.26 && wall.vy == 0 && validAngle wall.rotation &&
    wall.direction.natAbs ≤ 1 && 0 < wall.health && wall.health ≤ wallOfFleshMaxHealth &&
    wall.leechClock ≤ 2700 && wall.leeches ≤ 4 &&
    wall.roarClock.isFinite && -199 ≤ wall.roarClock && wall.roarClock ≤ 1598 &&
    wall.deadFade.isFinite && 0 ≤ wall.deadFade && wall.deadFade < 1 &&
    wall.frame < 2 && -60 ≤ wall.frameClock && wall.frameClock < 12 &&
    (if wall.eyes.isEmpty then wall.top == -1 && wall.bottom == -1 && wall.vx == 0
      else wall.eyes.size == 2 && wall.eyes[0]!.upper && !wall.eyes[1]!.upper &&
        0 ≤ wall.top && wall.top + 160 ≤ wall.bottom &&
        wall.bottom ≤ (world.height : Int) * 16 &&
        wall.eyes.all (fun eye => eye.valid world && eye.x.toBits == wall.x.toBits))

def WallLaser.valid (laser : WallLaser) (world : World) : Bool :=
  validPosition laser.x world.width 16 && validPosition laser.y world.height 16 &&
    laser.vx.isFinite && laser.vy.isFinite && laser.vx.abs ≤ 13.001 && laser.vy.abs ≤ 13.001 &&
    (laser.damage == 22 || laser.damage == 24 || laser.damage == 26 || laser.damage == 30) &&
    0 < laser.life && laser.life ≤ wallLaserLife && 0 < laser.penetration && laser.penetration ≤ 3 &&
    laser.slot.all (· < maxProjectileSlots)

/-- Classic mouth speed adds each crossed health threshold in Single order. -/
def wallOfFleshSpeed (health : Nat) : Float32 :=
  let speed : Float32 := 1.5
  let speed := if health < 6000 then speed + 0.25 else speed
  let speed := if health < 4000 then speed + 0.4 else speed
  let speed := if health < 2000 then speed + 0.5 else speed
  if health < 800 then speed + 0.6 else speed

private def clampRow (world : World) (row : Int) : Int :=
  max (underworldStart + 10 : Nat) (min ((world.height : Int) - 2) row)

private def liquid (world : World) (water magma : Water) (position : TilePos) : Nat :=
  max (water.level world position) (magma.level world position)

private def WallLaser.immersed (laser : WallLaser) (world : World) (water magma : Water) : Bool := Id.run do
  -- WetCollision uses a centered 4×2 probe for a 4×4 laser.
  let col := (laser.x - 2).floor.toInt32.toInt / 16
  let row := (laser.y - 1).floor.toInt32.toInt / 16
  for dx in [:2] do
    for dy in [:2] do
      let position : TilePos := ⟨col + dx, row + dy⟩
      let amount := liquid world water magma position
      if amount == 0 then continue
      let left := single (position.x * 16)
      let gap := single ((256 : Int) - amount) / 16
      let surface := single (position.y * 16) + gap
      if laser.x + 2 > left && laser.x - 2 < left + 16 &&
          laser.y + 1 > surface && laser.y - 1 < surface + single (16 - trunc gap) then
        return true
  return false

/-- NPC.SpawnWOF walks away from nearby players until their top-left x is
at least 1200 px away or the native 20-tile edge margin is crossed.
Search the selected column for a dry pocket above then below the summon row. -/
def spawnWallAt (world : World) (water magma : Water) (player : Player)
    (x y : Float32) : WallOfFlesh := Id.run do
  let step : Int := if x / 16 ≤ single (world.width / 2 : Nat) then -16 else 16
  let playerX := pixels player.x
  let mut spawnX := trunc x
  for _ in [:world.width + 1] do
    if playerX ≤ single (spawnX - 1200) || playerX ≥ single (spawnX + 1200) then break
    spawnX := spawnX + step
    let col := spawnX.tdiv 16
    if col < 20 || col > (world.width : Int) - 20 then break
  let col := spawnX.tdiv 16
  let mut row := tile y
  let inRange := fun (p : TilePos) =>
    2 ≤ p.x && p.x < (world.width : Int) - 2 && 2 ≤ p.y && p.y < (world.height : Int) - 2
  if !inRange ⟨col, row⟩ || (world.tile ⟨col,row⟩).solid || liquid world water magma ⟨col,row⟩ ≥ 100 then
    for offset in [:world.height] do
      let above : TilePos := ⟨col, row - offset⟩
      if inRange above && !(world.tile above).solid && liquid world water magma above < 100 then
        row := above.y
        break
      let below : TilePos := ⟨col, row + offset⟩
      if inRange below && !(world.tile below).solid && liquid world water magma below < 100 then
        row := below.y
        break
  return { x := single (spawnX - 60), y := single (clampRow world row * 16 - 120) }

def spawnWallOfFlesh (world : World) (water magma : Water) (player : Player) (summon : TilePos) : WallOfFlesh :=
  spawnWallAt world water magma player (single (summon.x * 16 + 8)) (single (summon.y * 16 + 8))

private def drawBounds (wall : WallOfFlesh) (world : World) (water magma : Water) : Int × Int := Id.run do
  let left := tile wall.x
  let right := tile (wall.x + 120)
  let center := tile wall.centerY
  let firstRow : Int := underworldStart + 10
  let lastRow : Int := (world.height : Int) - 10
  let density := fun y => Id.run do
    let mut count := 0
    for offset in [:(right - left + 1).toNat] do
      let pos : TilePos := ⟨left + offset, y⟩
      if 2 ≤ pos.x && pos.x < (world.width : Int) - 2 &&
          2 ≤ y && y < (world.height : Int) - 2 &&
          ((world.tile pos).solid || liquid world water magma pos > 0) then
        count := count + 1
    return count
  let mut floor := center + 7
  let mut occupied := 0
  for _ in [:world.height] do
    if occupied ≥ 15 || floor ≤ (underworldStart : Int) then break
    floor := floor + 1
    if floor > lastRow then
      floor := lastRow
      break
    if floor ≥ firstRow then occupied := occupied + density floor
  let mut ceiling := center - 7
  occupied := 0
  for _ in [:world.height] do
    if occupied ≥ 15 || ceiling ≥ lastRow then break
    ceiling := ceiling - 1
    if ceiling ≤ 10 then
      ceiling := 10
      break
    if ceiling > firstRow + 70 then continue
    if ceiling < firstRow then
      ceiling := firstRow
      break
    occupied := occupied + density ceiling
  let smooth := fun previous target =>
    if previous == -1 then target else if previous < target then previous + 1
    else if previous > target then previous - 1 else previous
  let limit := (firstRow + 70) * 16
  let smoothTop := max (firstRow * 16) (min limit (smooth wall.top ((ceiling - 4) * 16)))
  let smoothBottom := max (firstRow * 16) (min limit (smooth wall.bottom ((floor + 4) * 16)))
  let top := if smoothTop > smoothBottom - 160 then smoothBottom - 160 else smoothTop
  let bottom := if smoothBottom < top + 160 then top + 160 else smoothBottom
  return (top, bottom)

private def lineOfSight (eye : WallEye) (world : World) (player : Player) : Bool := Id.run do
  let col := fun x => max 1 (min ((world.width : Int) - 2) (x.tdiv 16))
  let row := fun y => max 1 (min ((world.height : Int) - 2) (y.tdiv 16))
  let mut x := col (trunc eye.x + 60)
  let mut y := row (trunc eye.y + 60)
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

private def lookAt (x y : Float32) (direction : Int) (player : Player) : Float32 × Bool :=
  let dx := pixels player.x + single (playerWidth / 32) - x
  let dy := pixels player.y + single (playerHeight / 32) - y
  if (if direction > 0 then dx ≤ 0 else dx ≥ 0) then (0, false)
  else
    let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
    let dx := dx * length
    let dy := dy * length
    let angle := if direction > 0 then Float.atan2 (-dy).toFloat (-dx).toFloat
      else Float.atan2 dy.toFloat dx.toFloat
    (angle.toFloat32 + 3.140000104904175, true)

def wallLaserStats (health : Nat) : Float32 × Nat :=
  if health < 800 then (13, 30)
  else if health < 2000 then (11, 26)
  else if health < 4000 then (10, 24)
  else (9, 22)

def WallEye.fire (eye : WallEye) (health : Nat) (player : Player) : Option WallLaser := do
  let (speed, damage) := wallLaserStats health
  let dx := pixels player.x + single (playerWidth / 16) * 0.5 - eye.centerX
  let dy := pixels player.y + single (playerHeight / 16) * 0.5 - eye.centerY
  let length := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  if length == 0 then failure
  let factor := speed / length
  let vx := dx * factor
  let vy := dy * factor
  return { x := eye.centerX + vx, y := eye.centerY + vy, vx, vy, damage }

private def wallRate (health : Nat) : Nat :=
  1 + (if health < 6000 then 1 else 0) + (if health < 4000 then 1 else 0) +
    (if health < 2000 then 1 else 0) + (if health < 800 then 2 else 0)

private def wallVolleySize (health : Nat) : Nat :=
  4 + (if health < 6000 then 1 else 0) + (if health < 4000 then 1 else 0) +
    (if health < 2000 then 2 else 0) + (if health < 800 then 3 else 0)

/-- The vanilla eye increments its counter even when its line of sight is
blocked. At the firing threshold, an eye looking behind the mouth consumes
a volley slot but emits no projectile. -/
def WallEye.advanceAI (eye : WallEye) (wall : WallOfFlesh) (player : Player)
    (canHit : WallEye → Bool) : WallEye × Option WallLaser :=
  let middle := single ((wall.top + wall.bottom).tdiv 2)
  let edge := if eye.upper then wall.top else wall.bottom
  let desiredY := (middle + single edge) / 2 - 60
  let vy : Float32 := if eye.y > desiredY + 1 then -1 else if eye.y < desiredY - 1 then 1 else 0
  let y := if vy == 0 then desiredY else eye.y
  let eye := { eye with x := wall.x, y, vy }
  let (rotation, forward) := lookAt eye.centerX eye.centerY wall.direction player
  let clock := min 16777216 (eye.laserClock + wallRate wall.health)
  let (laserClock, volley, laser) :=
    if eye.volley == 0 then
      if clock > 600 then (0, 1, none) else (clock, 0, none)
    else if clock > 45 && canHit eye then
      let count := eye.volley + 1
      (0, if count ≥ wallVolleySize wall.health then 0 else count,
        if forward then eye.fire wall.health player else none)
    else (clock, eye.volley, none)
  let frameClock := (eye.frameClock + 1) % 12
  let frame := if frameClock == 0 then 1 - eye.frame else eye.frame
  ({ eye with y := eye.y + eye.vy, rotation, laserClock, volley, frameClock, frame }, laser)

private def advanceLeech (wall : WallOfFlesh) : Nat × Nat × Bool :=
  let clock := wall.leechClock + 1
  let (clock, leeches) := if wall.leeches > 0 then (clock, wall.leeches) else
    let clock := clock + (if wall.health < 4000 then 1 else 0) + (if wall.health < 1600 then 1 else 0)
    (clock, if clock > 2700 then 1 else 0)
  if leeches == 0 || clock ≤ 60 then (clock, leeches, false)
  else
    let count := leeches + 1
    (0, if count > (if wall.health < 2400 then 4 else 3) then 0 else count, true)

private def spawnEye (wall : WallOfFlesh) (upper : Bool) : WallEye :=
  let boundary := if upper then wall.top else wall.bottom
  let spawnY := trunc ((wall.centerY + single boundary) / 2)
  { x := single (trunc wall.x - 60), y := single (spawnY - 120), upper }

/-- One complete mouth update followed by the two eye NPC updates.
Roll indices model the native Next(1000) trigger and Next(200) reset. -/
def WallOfFlesh.advance (wall : WallOfFlesh) (world : World) (water magma : Water) (player : Player)
    (dead : Bool) (roll : Nat → Nat := fun _ => 0) : Option (WallOfFlesh × WallStepEvents) := do
  if wall.health == 0 || wall.x < 160 || wall.x > single (((world.width : Int) - 10) * 16) then failure
  let deadFade := if dead then wall.deadFade + 0.0055555556900799274
    else max 0 (wall.deadFade - 0.03333333507180214)
  if deadFade ≥ 1 then failure
  let (leechClock, leeches, spawnLeech) := advanceLeech wall
  let leechSpawn := if spawnLeech then
      some (single (trunc wall.centerX), single (trunc (wall.centerY + 20)), single wall.direction * 8)
    else none
  let roarClock := wall.roarClock + 1
  let roared := roarClock ≥ single (600 + roll 0 % 1000 : Nat)
  let roarClock := if roared then -(single (roll 1 % 200 : Nat)) else roarClock
  let (top, bottom) := drawBounds wall world water magma
  let y := single ((top + bottom).tdiv 2 - 60)
  let playerX := single (player.x.tdiv 16 + playerWidth / 32)
  let direction : Int :=
    if wall.vx < 0 then -1
    else if wall.vx > 0 then 1
    else if dead then
      if wall.centerX < pixels player.x + single (playerWidth / 16) * 0.5 then 1 else -1
    else if playerX < wall.centerX then -1 else 1
  let vx := single direction * wallOfFleshSpeed wall.health
  let (rotation, _) := lookAt wall.centerX (y + 60) direction player
  let frameClock := if leeches == 0 then wall.frameClock + 1 else -60
  let frame := if leeches != 0 then 0 else if frameClock ≥ 12 then 1 - wall.frame else wall.frame
  let frameClock := if frameClock ≥ 12 then 0 else frameClock
  let prepared : WallOfFlesh := { wall with
    y, vx, vy := 0, rotation, direction, top, bottom, leechClock, leeches,
    roarClock, deadFade, frame, frameClock }
  let spawnHungry := prepared.eyes.isEmpty
  let hungrySpawn := if spawnHungry then
      some (single (trunc prepared.x), single (trunc ((prepared.centerY + single bottom) / 2)))
    else none
  let eyes := if spawnHungry then #[spawnEye prepared true, spawnEye prepared false] else prepared.eyes
  let moved := { prepared with x := prepared.x + vx }
  let (eyes, lasers) := Id.run do
    let mut eyesNext := #[]
    let mut lasers := #[]
    for eye in eyes do
      let (next, shot) := eye.advanceAI moved player (fun actor => lineOfSight actor world player)
      eyesNext := eyesNext.push next
      if let some laser := shot then lasers := lasers.push laser
    return (eyesNext, lasers)
  return ({ moved with eyes }, { lasers, spawnHungry, spawnLeech, roared, hungrySpawn, leechSpawn })

/-- One of three native projectile updates per world tick. Preserve the
contact box of the collision step for callers which resolve hits per update. -/
def WallLaser.advanceSubstep (laser : WallLaser) (world : World) (water magma : Water) :
    Option (WallLaser × Bool) := do
  if laser.life == 0 || laser.penetration == 0 then failure
  let (dx, dy) := clipShotMotion world laser.x laser.y laser.vx laser.vy 2 false
  let blocked := dx != laser.vx || dy != laser.vy
  let wet := laser.immersed world water magma
  let x := laser.x + (if wet && dx == laser.vx then dx * 0.5 else dx)
  let y := laser.y + (if wet && dy == laser.vy then dy * 0.5 else dy)
  if !validPosition x world.width 16 || !validPosition y world.height 16 then failure
  return ({ laser with x, y, life := laser.life - 1 }, blocked)

def WallLaser.advance (laser : WallLaser) (world : World) (water magma : Water) :
    Option (WallLaser × Bool) := do
  let mut laser := laser
  for _ in [:wallLaserUpdates] do
    let (next, blocked) ← laser.advanceSubstep world water magma
    if blocked || next.life == 0 then return (next, blocked)
    laser := next
  return (laser, false)

end Lean4Terria
