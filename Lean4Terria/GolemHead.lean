import Lean4Terria.Magma
import Lean4Terria.Shortsword
import Lean4Terria.TempleTraps

namespace Lean4Terria

open OriginalCollision (Vector)

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (units : Int) : Float32 := single units / 16
private def length (vector : Vector) : Float32 :=
  (vector.x * vector.x + vector.y * vector.y).toFloat.sqrt.toFloat32
private def scaled (vector : Vector) (speed : Float32) : Vector :=
  let ratio := speed / length vector
  ⟨vector.x * ratio, vector.y * ratio⟩

inductive GolemShotKind where
  | fireball | eyeBeam
  deriving BEq, Repr, Inhabited

def GolemShotKind.sprite : GolemShotKind → Nat
  | .fireball => 258
  | .eyeBeam => 259

def GolemShotKind.size : GolemShotKind → Nat
  | .fireball => 16
  | .eyeBeam => 8

def GolemShotKind.updates : GolemShotKind → Nat
  | .fireball => 1
  | .eyeBeam => 2

def GolemShotKind.burnTicks (kind : GolemShotKind) (roll : Nat → Nat) : Nat :=
  if kind == .fireball && roll 0 % 2 == 0 then 60 * (5 + roll 1 % 3) else 0

/-- Projectile constructor position is its center in native world pixels. -/
structure GolemAttack where
  kind : GolemShotKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  deriving BEq, Repr, Inhabited

structure GolemHead where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  fireClock : Float32 := 0
  laserClock : Float32 := 0
  health : Nat := 25000
  alpha : Nat := 255
  frame : Nat := 0
  frameCounter : Nat := 0
  detached : Bool := false
  fast : Bool := false
  fireMouth : Bool := false
  noTileCollide : Bool := false
  look : Int := 0
  deriving BEq, Repr, Inhabited

def GolemHead.atBottom (x y : Float32) : GolemHead :=
  { x := x - 35, y := y - 70 }

def GolemHead.centerX (head : GolemHead) : Float32 := head.x + 35
def GolemHead.centerY (head : GolemHead) : Float32 := head.y + 35

def GolemHead.detach (head : GolemHead) : GolemHead :=
  { atBottom (single head.centerX.toInt32.toInt) (single (head.y.toInt32.toInt + 70)) with
    detached := true, health := 16000, alpha := 0 }

def GolemHead.sprite (head : GolemHead) : Nat := if head.detached then 249 else 246
def GolemHead.frameHeight (head : GolemHead) : Nat := if head.detached then 138 else 118
def GolemHead.damage (head : GolemHead) : Nat := if head.detached then 80 else 64
def GolemHead.defense (head : GolemHead) : Nat := if head.detached then 32 else 20

def GolemHead.hitBox (head : GolemHead) : HitBox :=
  let x := head.x.toInt32.toInt * 16
  let y := head.y.toInt32.toInt * 16
  ⟨x, y, x + 1120, y + 1120⟩

def GolemHead.contact (head : GolemHead) (player : Player) : Option Int :=
  if head.hitBox.intersects (HitBox.ofPlayer player) then
    some (if head.centerX < pixels (bodyCenterX player) then 1 else -1)
  else none

def GolemHead.valid (head : GolemHead) (world : World) : Bool :=
  head.x.isFinite && head.y.isFinite && head.vx.isFinite && head.vy.isFinite &&
    head.rotation.isFinite && head.fireClock.isFinite && head.laserClock.isFinite &&
    -512 ≤ head.x && head.x ≤ single (world.width * 16 : Nat) + 512 &&
    -512 ≤ head.y && head.y ≤ single (world.height * 16 : Nat) + 512 &&
    head.vx.abs ≤ 101 && head.vy.abs ≤ 101 && head.alpha ≤ 255 &&
    0 ≤ head.fireClock && head.fireClock < 300 && 0 ≤ head.laserClock && head.laserClock ≤ 4900 &&
    -1 ≤ head.look && head.look ≤ 1 &&
    (if head.detached then head.health == 16000 && head.alpha == 0 && head.frame < 2 && head.frameCounter < 16
      else 0 < head.health && head.health ≤ 25000 && head.frame < 6 && head.frameCounter == 0)

structure GolemHeadStep where
  head : GolemHead
  attacks : Array GolemAttack := #[]
  deriving Repr

private def fireball (center target : Vector) (damage : Nat) : GolemAttack :=
  let velocity := scaled ⟨target.x - center.x, target.y - center.y⟩ 8
  ⟨.fireball, center.x, center.y, velocity.x, velocity.y, damage⟩

private def eyeBeam (eye target : Vector) (speed : Float32) (damage : Nat) : GolemAttack :=
  let velocity := scaled ⟨target.x - eye.x, target.y - eye.y⟩ speed
  ⟨.eyeBeam, eye.x + velocity.x * 3, eye.y + velocity.y * 3, velocity.x, velocity.y, damage⟩

private def headFrame (head : GolemHead) : GolemHead :=
  let mouth := if head.fireMouth then 1 else 0
  if head.detached then { head with frame := mouth, frameCounter := (head.frameCounter + 1) % 16 }
  else
    let side := if !head.fast || head.look == 0 then 0 else if head.look < 0 then 4 else 2
    { head with frame := side + mouth }

private def attachedHead (head : GolemHead) (target body : Vector) (balance : Float32)
    (canHit : Bool) (roll : Nat → Nat) : GolemHeadStep := Id.run do
  let center : Vector := ⟨head.centerX, head.centerY⟩
  let pull : Vector := ⟨body.x - center.x - 3, body.y - center.y - 57⟩
  let close := length pull < 100
  let velocity := if close then pull else scaled pull 100
  let mut next := { head with
    vx := velocity.x, vy := velocity.y, noTileCollide := true
    rotation := if close then 0 else velocity.x * 0.1
    alpha := head.alpha - 10, fireClock := if head.alpha > 0 then 30 else head.fireClock }
  let mut attacks := #[]
  let look := if target.x < center.x - 70 then -1 else if target.x > center.x + 70 then 1 else 0
  let cadence := (balance + 3) / 4
  let fireClock :=
    if head.fast then
      next.fireClock + cadence + (if head.health < 10000 then cadence else 0) +
        (if head.health < 5000 then cadence else 0)
    else
      let clock := next.fireClock + 1
      clock + (if clock < 20 || clock > 280 then 2 * (balance - 1) / 3 else (balance - 1) / 2)
  let artClock := if head.fast then fireClock else next.fireClock + 1
  let openMouth := artClock < 20 || artClock > 280
  next := { next with
    fireMouth := openMouth
    fireClock := if fireClock ≥ 300 then 0 else fireClock
    look := if head.fast then look else head.look }
  if fireClock ≥ 300 then
    let mouth : Vector := ⟨center.x + (if head.fast then single (look * 40) else 0), center.y + 10⟩
    attacks := attacks.push (fireball mouth target (if head.fast then 24 else 18))
  if head.fast then
    let clock := next.laserClock + cadence + (if head.health < 8333 then cadence else 0) +
      (if head.health < 6250 then cadence else 0) + (if head.health < 5000 then cadence else 0) +
      (if canHit then 0 else 4)
    if clock > single (60 + roll 0 % 600 : Nat) then
      next := { next with laserClock := 0 }
      let eye (x : Float32) := eyeBeam ⟨center.x + x, center.y - 22⟩ target (if look == 0 then 11 else 12) 28
      if look == 0 then attacks := (attacks.push (eye (-18))).push (eye 18)
      else attacks := attacks.push (eye (single (look * 30)))
    else next := { next with laserClock := clock }
  return ⟨{ next with fast := head.health < 12500 }, attacks⟩

private def flightAxis (velocity target : Float32) : Float32 :=
  if velocity < target then
    let shifted := velocity + 0.05
    if shifted < 0 && target > 0 then shifted + 0.05 else shifted
  else if velocity > target then
    let shifted := velocity - 0.05
    if shifted > 0 && target < 0 then shifted - 0.05 else shifted
  else velocity

private def detachedHead (head : GolemHead) (target targetVelocity : Vector) (bodyHealth : Nat)
    (balance : Float32) (canHit canBodyHit solid : Bool) (roll : Nat → Nat)
    (sample : Nat → Float32) : GolemHeadStep := Id.run do
  let center : Vector := ⟨head.centerX, head.centerY⟩
  let velocity := scaled ⟨target.x - center.x, target.y - center.y - 300⟩ 7
  let cadence := (balance + 4) / 5
  let clock := head.fireClock + cadence + (if bodyHealth < 12000 then cadence else 0) +
    (if bodyHealth < 9000 then cadence else 0) + (if bodyHealth < 3000 then cadence else 0) +
    (if bodyHealth < 1500 then cadence else 0)
  let shoot := canHit && clock ≥ 300
  let laserSteps := [12000, 10000, 7500, 5000, 3750, 3000, 2500]
  let laserClock := laserSteps.foldl (fun accumulated threshold =>
    accumulated + if bodyHealth < threshold then balance else 0) (head.laserClock + balance)
  let laserClock := laserClock + (if canBodyHit then 0 else balance * 10)
  let shootLaser := laserClock > single (100 + roll 0 % 4800 : Nat)
  let mut attacks := #[]
  if shoot then attacks := attacks.push (fireball ⟨center.x, center.y - 10⟩ target 20)
  if shootLaser then
    let increase := ([7500, 6000, 4500, 3000, 1500] : List Nat).countP (bodyHealth < ·)
    let speed := (11 + single increase * 0.25) * (if canBodyHit then 1 else 2.5)
    let damage := if canBodyHit then 24 + increase else (24 + increase) * 3 / 2
    for side in [:2] do
      let eye : Vector := ⟨center.x + (if side == 0 then -14 else 14), center.y - 50⟩
      let predicted : Vector := if canBodyHit then target else
        ⟨target.x + targetVelocity.x * sample (2 * side) * 50,
          target.y + targetVelocity.y * sample (2 * side + 1) * 50⟩
      attacks := attacks.push (eyeBeam eye predicted speed damage)
  return {
    head := { head with
      vx := flightAxis head.vx velocity.x, vy := flightAxis head.vy velocity.y
      noTileCollide := !canHit || (head.noTileCollide && !solid)
      fireClock := if !canHit then 20 else if shoot then 0 else clock
      laserClock := if shootLaser then 0 else laserClock
      fireMouth := clock < 20 || clock > 280 }
    attacks }

/-- The NPC AI before UpdateNPC applies clipping, movement and animation. -/
def GolemHead.advanceAI (head : GolemHead) (target targetVelocity body : Vector) (bodyHealth : Nat)
    (enraged canHit canBodyHit solid : Bool) (roll : Nat → Nat) (sample : Nat → Float32) : GolemHeadStep :=
  let balance : Float32 := if enraged then 2 else 1
  if head.detached then detachedHead head target targetVelocity bodyHealth balance canHit canBodyHit solid roll sample
  else attachedHead head target body balance canHit roll

private def terrainGrid (world : World) : OriginalCollision.Grid :=
  { width := world.width, height := world.height
    tileAt := fun x y =>
      let tile := world.tile ⟨x, y⟩
      if tile == .platform then { id := some 19, frame := some (0, 0) }
      else if tile.solid then { id := some 1 }
      else {}
    rules := fun id =>
      if id == 19 then { solid := true, solidTop := true, platform := true }
      else { solid := id == 1 } }

private def sightTile (world : World) (point : Vector) : TilePos :=
  ⟨max 1 (min (world.width - 1 : Nat) (OriginalCollision.tileIndex point.x)),
    max 1 (min (world.height - 40 : Nat) (OriginalCollision.tileIndex point.y))⟩

private def GolemHead.inSolidTiles (head : GolemHead) (world : World) : Bool := Id.run do
  let left := OriginalCollision.tileIndex head.x
  let right := OriginalCollision.tileIndex (head.x + 70)
  let top := OriginalCollision.tileIndex head.y
  let bottom := OriginalCollision.tileIndex (head.y + 70)
  if left < 0 || world.width ≤ right || top < 0 || world.height - 40 ≤ bottom then return true
  for x in [left.toNat:right.toNat + 1] do
    for y in [top.toNat:bottom.toNat + 1] do
      if (world.tile ⟨x, y⟩).solid then return true
  return false

/-- Apply the Classic dry NPC movement and frame selection to an AI result. -/
def GolemHead.move (head : GolemHead) (grid : OriginalCollision.Grid) : GolemHead :=
  let vx := if head.vx.abs.toFloat < 0.005 then 0 else head.vx
  let velocity : Vector := ⟨vx, head.vy⟩
  let clipped := if head.noTileCollide then velocity else
    let collision := OriginalCollision.tileCollision grid ⟨head.x, head.y⟩ velocity 70 70 {}
    let value := collision.displacement
    ⟨value.x, if collision.hitAbove then 0.01 else value.y⟩
  headFrame { head with x := head.x + clipped.x, y := head.y + clipped.y, vx := clipped.x, vy := clipped.y }

def GolemHead.advance (head : GolemHead) (world : World) (player : Player)
    (bodyX bodyY : Float32) (bodyHealth : Nat) (enraged : Bool) (roll : Nat → Nat)
    (sample : Nat → Float32) : GolemHeadStep :=
  let playerCenter : Vector := ⟨pixels (bodyCenterX player), pixels (bodyCenterY player)⟩
  let bodyCenter : Vector := ⟨bodyX, bodyY⟩
  let grid := terrainGrid world
  let target := sightTile world playerCenter
  let visible := canHitTiles world (sightTile world ⟨head.centerX, head.centerY⟩) target
  let bodyVisible := !head.detached || canHitTiles world (sightTile world bodyCenter) target
  let touching := head.detached && head.noTileCollide && head.inSolidTiles world
  let step := head.advanceAI playerCenter ⟨pixels player.vx, pixels player.vy⟩ bodyCenter bodyHealth
    enraged visible bodyVisible touching roll sample
  { step with head := step.head.move grid }

/-- Position and speed use native pixels. Lifetime counts subupdates. -/
structure GolemShot where
  kind : GolemShotKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  rotation : Float32 := 0
  damage : Nat
  life : Nat := 300
  age : Nat := 0
  alpha : Nat := 0
  direction : Int := 1
  bounces : Nat := 0
  honeyWet : Bool := false
  slot : Option Nat := none
  deriving BEq, Repr, Inhabited

private def GolemShotKind.halfSize (kind : GolemShotKind) : Float32 := single (kind.size / 2 : Nat)
def GolemShot.centerX (shot : GolemShot) : Float32 := shot.x + shot.kind.halfSize
def GolemShot.centerY (shot : GolemShot) : Float32 := shot.y + shot.kind.halfSize
def GolemShot.playerDamage (shot : GolemShot) : Nat := shot.damage * 2

private def beamLaunchVelocity (vx vy : Float32) : Vector := Id.run do
  let mut velocity : Vector := ⟨vx, vy⟩
  -- Vanilla aiStyle 1 caps each axis multiplicatively after position is set.
  for _ in [:100] do
    if velocity.x < 16 && velocity.x > -16 && velocity.y < 16 && velocity.y ≥ -16 then break
    velocity := ⟨velocity.x * 0.97, velocity.y * 0.97⟩
  return velocity

def GolemShot.fire (attack : GolemAttack) (slot : Nat) : GolemShot :=
  let velocity := if attack.kind == .eyeBeam then beamLaunchVelocity attack.vx attack.vy else ⟨attack.vx, attack.vy⟩
  { kind := attack.kind, x := attack.x - attack.kind.halfSize, y := attack.y - attack.kind.halfSize
    vx := velocity.x, vy := velocity.y, damage := attack.damage, slot := some slot
    alpha := if attack.kind == .fireball then 100 else 0 }

private def GolemShot.inBounds (shot : GolemShot) (world : World) : Bool :=
  0 < shot.x && 0 < shot.y && shot.x + single shot.kind.size < single (world.width * 16 : Nat) &&
    shot.y + single shot.kind.size < single (world.height * 16 : Nat)

def GolemShot.valid (shot : GolemShot) (world : World) : Bool :=
  shot.x.isFinite && shot.y.isFinite && shot.vx.isFinite && shot.vy.isFinite && shot.rotation.isFinite &&
    (if shot.age == 0 then shot.inBounds world
      else -32 ≤ shot.x && shot.x ≤ single (world.width * 16 : Nat) + 32 &&
        -32 ≤ shot.y && shot.y ≤ single (world.height * 16 : Nat) + 32) &&
    shot.vx.abs ≤ 32 && shot.vy.abs ≤ 32 && shot.life > 0 && shot.age + shot.life == 300 &&
    shot.bounces < 5 && shot.alpha ≤ 100 && (shot.direction == -1 || shot.direction == 1) &&
    shot.slot.any (· < maxProjectileSlots) &&
    (if shot.kind == .fireball then [18, 20, 24].contains shot.damage else 24 ≤ shot.damage && shot.damage ≤ 43)

def GolemShot.hitBox (shot : GolemShot) : HitBox :=
  let x := shot.x.toInt32.toInt * 16
  let y := shot.y.toInt32.toInt * 16
  let size := (shot.kind.size : Int) * 16
  ⟨x, y, x + size, y + size⟩

structure GolemShotStep where
  shot : Option GolemShot := none
  hit : Option HitBox := none
  sound : Option Nat := none
  impact : Bool := false
  deriving Repr

private def GolemShot.wetContact (shot : GolemShot) (world : World) (water magma honey : Water) :
    Option Bool := Id.run do
  let x := shot.centerX - 5
  let y := shot.centerY - 4
  let firstX := OriginalCollision.tileIndex x
  let firstY := OriginalCollision.tileIndex y - 1
  for dx in [:(OriginalCollision.tileIndex (x + 10) - firstX + 1).toNat] do
    for dy in [:(OriginalCollision.tileIndex (y + 8) - firstY + 1).toNat] do
      let p : TilePos := ⟨firstX + dx, firstY + dy⟩
      let honeyDepth := honey.level world p
      let depth := max honeyDepth (max (water.level world p) (magma.level world p))
      if depth == 0 then continue
      let left := single (p.x * 16)
      let offset := single (256 - depth : Nat) / 16
      let surface := single (p.y * 16) + offset
      let height := 16 - single offset.toInt32.toInt
      if x + 10 > left && x < left + 16 && y + 8 > surface && y < surface + height then
        return some (honeyDepth > 0)
  return none

/-- Stepping uses the pre-bounce clipped displacement while wet, as
in Terraria's HandleMovement. The fifth impact kills a fireball. -/
def GolemShot.advanceSubstep (shot : GolemShot) (world : World) (water magma honey : Water) :
    GolemShotStep := Id.run do
  if shot.life == 0 || !shot.inBounds world then return {}
  let fire := shot.kind == .fireball
  let sound := if shot.age == 0 then some (if fire then 20 else 33) else none
  let velocity : Vector := ⟨shot.vx, if fire then min shot.vy 16 else shot.vy⟩
  let origin : Vector := ⟨shot.x, shot.y⟩
  let wet := if fire then shot.wetContact world water magma honey else none
  let honeyWet := wet.any (fun contact => contact || shot.honeyWet)
  let clipped := if !fire then velocity else
    let grid := terrainGrid world
    if wet.isSome then
      (OriginalCollision.tileCollision grid origin velocity 16 16 { fallThrough := true, fall2 := true }).displacement
    else (TempleTraps.Terrain.ofGrid grid).clip origin velocity 16 16
  let blockedX := clipped.x != velocity.x
  let blockedY := clipped.y != velocity.y
  let impact := blockedX || blockedY
  let bounces := shot.bounces + if impact then 1 else 0
  if bounces ≥ 5 then return { sound, impact }
  let vx := if blockedX then 0 - velocity.x else velocity.x
  let vy := if blockedY then 0 - velocity.y else velocity.y
  let scale : Float32 := if honeyWet then 0.25 else 0.5
  let dx := if wet.isNone then vx else if blockedX then clipped.x else clipped.x * scale
  let dy := if wet.isNone then vy else if blockedY then clipped.y else clipped.y * scale
  let rotation := if fire then shot.rotation + 0.3 * single shot.direction
    else (Float.atan2 shot.vy.toFloat shot.vx.toFloat).toFloat32 + 1.57
  let moved := { shot with
    x := shot.x + dx, y := shot.y + dy, vx, vy, rotation, bounces, honeyWet
    alpha := if fire then shot.alpha else shot.alpha - 10
    age := shot.age + 1, life := shot.life - 1, direction := if vx < 0 then -1 else 1 }
  return { shot := if moved.life > 0 then some moved else none, hit := some moved.hitBox, sound, impact }

end Lean4Terria
