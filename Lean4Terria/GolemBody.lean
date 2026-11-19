import Lean4Terria.DarkCaster
import Lean4Terria.OriginalCollision
import Lean4Terria.Shortsword

namespace Lean4Terria

open OriginalCollision (Vector single)

def golemBodyWidth : Nat := 140
def golemBodyHeight : Nat := 140
def golemBodyHealth : Nat := 15000
def golemBodyDamage : Nat := 72
def golemBodyDefense : Nat := 26
def golemBodyFrameStride : Nat := 174

/-- NPC 245 uses top-left coordinates in native pixels. The signed jump clock
counts down the windup after it has accumulated 300 rest units. -/
structure GolemBody where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := golemBodyHealth
  alpha : Nat := 255
  jumpState : Bool := false
  jumpClock : Int := 0
  initialized : Bool := false
  noTileCollide : Bool := false
  invulnerable : Bool := false
  enraged : Bool := false
  direction : Int := 0
  frameY : Nat := 0
  timeLeft : Nat := 750
  deriving BEq, Repr, Inhabited

structure GolemBodyStep where
  body : GolemBody
  spawnParts : Bool := false
  landed : Bool := false
  deriving Repr, Inhabited

structure GolemBodyTarget where
  x : Float32
  y : Float32
  width : Nat
  height : Nat
  deriving Repr, Inhabited

def GolemBodyTarget.ofPlayer (player : Player) : GolemBodyTarget :=
  ⟨single player.x / 16, single player.y / 16, playerWidth.toNat / 16, playerHeight.toNat / 16⟩

private def GolemBodyTarget.centerX (target : GolemBodyTarget) : Float32 :=
  target.x + single target.width / 2

private def GolemBodyTarget.centerY (target : GolemBodyTarget) : Float32 :=
  target.y + single target.height / 2

def GolemBody.centerX (body : GolemBody) : Float32 := body.x + 70
def GolemBody.centerY (body : GolemBody) : Float32 := body.y + 70
def GolemBody.bottomY (body : GolemBody) : Float32 := body.y + 140

def GolemBody.atBottom (x y : Float32) : GolemBody :=
  { x := x - 70, y := y - 140 }

def GolemBody.hitBox (body : GolemBody) : HitBox :=
  CasterAI.npcBox body.x body.y golemBodyWidth golemBodyHeight

def GolemBody.contact (body : GolemBody) (player : Player) : Option Int :=
  CasterAI.contact body.hitBox body.centerX player

/-- The part spawn coordinates are NPC bottom centers, in the native spawn
order. Update them with their own AI after the body moves that frame. -/
def GolemBody.partSpawnBottoms (body : GolemBody) : (Float32 × Float32) ×
    (Float32 × Float32) × (Float32 × Float32) :=
  let x := single body.centerX.toInt32.toInt
  let y := single body.centerY.toInt32.toInt
  ((x - 84, y - 9), (x + 78, y - 9), (x - 3, y - 57))

def GolemBody.valid (body : GolemBody) (world : World) : Bool :=
  body.x.isFinite && body.y.isFinite && body.vx.isFinite && body.vy.isFinite &&
    -32768 ≤ body.x && body.x ≤ single (world.width * 16 + 32768 : Nat) &&
    -32768 ≤ body.y && body.y ≤ single (world.height * 16 + 32768 : Nat) &&
    body.vx.abs ≤ 32 && body.vy.abs ≤ 32 && 0 < body.health && body.health ≤ golemBodyHealth &&
    body.alpha ≤ 255 && -20 ≤ body.jumpClock && body.jumpClock < 300 &&
    [-1, 0, 1].contains body.direction && 0 < body.timeLeft && body.timeLeft ≤ 750 &&
    [0, 1, 174, 348, 522, 696].contains body.frameY

private def GolemBody.solidTiles (body : GolemBody) (world : World) : Bool := Id.run do
  let x0 := OriginalCollision.tileIndex body.x
  let x1 := OriginalCollision.tileIndex (body.x + 140)
  let y0 := OriginalCollision.tileIndex body.y
  let y1 := OriginalCollision.tileIndex body.bottomY
  if x0 < 0 || x1 ≥ world.width || y0 < 0 || y1 ≥ world.height - 40 then return true
  for x in [x0.toNat:x1.toNat + 1] do
    for y in [y0.toNat:y1.toNat + 1] do
      let tile := world.tile ⟨x, y⟩
      if tile.solid && tile != .platform then return true
  return false

def GolemBody.canSee (body : GolemBody) (world : World) (player : Player) : Bool :=
  let target := GolemBodyTarget.ofPlayer player
  let tile (x y : Float32) : TilePos :=
    ⟨max 1 (min (world.width - 1 : Nat) (OriginalCollision.tileIndex x)),
      max 1 (min (world.height - 40 : Nat) (OriginalCollision.tileIndex y))⟩
  canHitTiles world (tile body.centerX body.centerY) (tile target.centerX target.centerY)

private def GolemBody.targetDirection (body : GolemBody) (target : GolemBodyTarget) (dead : Bool) : Int :=
  if dead then body.direction
  else if single (target.x.toInt32.toInt + (target.width / 2 : Nat)) < body.centerX then -1
  else 1

private def GolemBody.rest (body : GolemBody) (target : GolemBodyTarget) (dead : Bool)
    (hasHead hasLeftFist hasRightFist : Bool) (balance : Nat) : GolemBody :=
  if body.vy != 0 then body else
  let clock := 1 + if body.jumpClock ≤ 0 then 0 else
    (if hasHead then 0 else 2) + (if hasLeftFist then 0 else 2) +
    (if hasRightFist then 0 else 2) + (if body.health < golemBodyHealth then 1 else 0) +
    (if body.health < golemBodyHealth / 2 then 4 else 0) +
    (if body.health < golemBodyHealth / 3 then 8 else 0)
  let step := if body.jumpClock > 0 then clock * balance else clock
  let body := { body with vx := body.vx * 0.8, jumpClock := body.jumpClock + step }
  if body.jumpClock ≥ 300 then { body with jumpClock := -20 }
  else if body.jumpClock == -1 then
    let direction := body.targetDirection target dead
    let vy : Float32 := if body.health == golemBodyHealth then -12.1
      else max (-19.1) (-12.1 * (single balance + 9) / 10)
    { body with
      vx := 4 * single direction, vy, direction, noTileCollide := true
      jumpState := true, jumpClock := 0 }
  else body

private def GolemBody.steerJump (body : GolemBody) (target : GolemBodyTarget) (dead : Bool)
    (balance : Nat) : GolemBody :=
  let direction := body.targetDirection target dead
  let speedScale := (single balance + 1) / 2
  if body.x < target.x && body.x + 140 > target.x + single target.width then
    let vy := if body.bottomY < target.y then body.vy + (0.2 * (single balance + 1) / 2) else body.vy
    { body with vx := body.vx * 0.9, vy, direction }
  else
    let speed : Float32 := 3 + (if body.health < golemBodyHealth then 1 else 0) +
      (if body.health < golemBodyHealth / 2 then 1 else 0) +
      (if body.health < golemBodyHealth / 4 then 1 else 0)
    let cap := speed * speedScale
    { body with vx := max (-cap) (min cap (body.vx + 0.2 * single direction)), direction }

private def GolemBody.frame (body : GolemBody) : Nat :=
  if body.jumpState then 1
  else if body.jumpClock < -16 then 0
  else if body.jumpClock < -12 then golemBodyFrameStride
  else if body.jumpClock < -8 then 2 * golemBodyFrameStride
  else if body.jumpClock < -4 then 3 * golemBodyFrameStride
  else if body.jumpClock < 0 then 4 * golemBodyFrameStride
  else 0

private def compactCollisionGrid (world : World) : OriginalCollision.Grid :=
  { width := world.width
    height := world.height
    tileAt := fun x y =>
      let tile := world.tile ⟨x, y⟩
      if tile == .platform then { id := some 19, frame := some (0, 0) }
      else if tile.solid then { id := some 1 }
      else {}
    rules := fun id =>
      if id == 19 then { solid := true, solidTop := true, platform := true }
      else { solid := id == 1 } }

private def GolemBody.dryGravity (body : GolemBody) (worldWidth : Nat) (worldSurface : Float) : Float32 :=
  let width := single worldWidth / 4200
  let altitude := (body.y / 16 - (60 + 10 * (width * width))).toFloat
  let factor := (altitude / (worldSurface / 6)).toFloat32
  0.3 * max 0.25 (min 1 factor)

private def GolemBody.moveDry (body : GolemBody) (world : World) (target : GolemBodyTarget)
    (gravity : Float32) : GolemBody :=
  let loaded := world.contains ⟨OriginalCollision.tileIndex body.centerX, OriginalCollision.tileIndex body.centerY⟩
  let vx := if (!body.noTileCollide && !loaded) || (body.vx.toFloat < 0.005 && body.vx.toFloat > -0.005)
    then 0 else body.vx
  let vy := if !body.noTileCollide && !loaded then 0 else min 10 (body.vy + gravity)
  let position : Vector := ⟨body.x, body.y⟩
  let velocity : Vector := ⟨vx, vy⟩
  if body.noTileCollide then { body with x := body.x + vx, y := body.y + vy, vx, vy }
  else
    let grid := compactCollisionGrid world
    let fall := target.y > body.bottomY
    let velocity := OriginalCollision.walkDownSlope grid position velocity golemBodyWidth golemBodyHeight gravity
    let result := OriginalCollision.tileCollision grid position velocity golemBodyWidth golemBodyHeight
      { fallThrough := fall, fall2 := fall }
    let movement := if result.hitAbove then { result.displacement with y := 0.01 } else result.displacement
    let slope := OriginalCollision.slopeCollision grid (position + movement) movement golemBodyWidth golemBodyHeight fall
    { body with x := slope.position.x, y := slope.position.y, vx := slope.velocity.x, vy := slope.velocity.y }

private def GolemBody.checkActive (body : GolemBody) (target : GolemBodyTarget) : Nat :=
  let left := (body.centerX.toFloat - 1100).toInt32.toInt * 16
  let top := (body.centerY.toFloat - 740).toInt32.toInt * 16
  let nearby : HitBox := ⟨left, top, left + 35200, top + 23680⟩
  (if nearby.intersects (CasterAI.npcBox target.x target.y target.width target.height) then 750 else body.timeLeft) - 1

/-- Classic single-player NPC 245, including the dry NPC physics and frame
selection. The target flags are sampled before parts and movement update. -/
def GolemBody.advanceTarget (body : GolemBody) (world : World) (target : GolemBodyTarget)
    (hasHead hasLeftFist hasRightFist templeOrJungle aboveSurface dead visible : Bool)
    (worldSurface : Float := 50) : Option GolemBodyStep := do
  guard (body.health > 0)
  let gravity := body.dryGravity world.width worldSurface
  let spawnParts := !body.initialized
  let hasHead := hasHead || spawnParts
  let enraged := !templeOrJungle || aboveSurface
  let balance : Nat := if enraged then 2 else 1
  let body := { body with
    initialized := true, invulnerable := hasHead, enraged
    alpha := body.alpha - 10
    jumpClock := if body.alpha > 0 then 0 else body.jumpClock
    noTileCollide := dead || (body.noTileCollide &&
      !(body.vy > 0 && body.bottomY > target.y) && !(visible && !body.solidTiles world)) }
  let landed := body.jumpState && body.vy == 0
  let body := if !body.jumpState then
      body.rest target dead hasHead (hasLeftFist || spawnParts) (hasRightFist || spawnParts) balance
    else if landed then { body with jumpState := false }
    else body.steerJump target dead balance
  guard ((body.centerX - target.centerX).abs + (body.centerY - target.centerY).abs ≤ 3000)
  let body := { body with direction := body.targetDirection target dead }
  let body := body.moveDry world target gravity
  let timeLeft := body.checkActive target
  guard (timeLeft > 0)
  return {
    body := { body with frameY := body.frame, timeLeft }
    spawnParts, landed }

def GolemBody.advance (body : GolemBody) (world : World) (player : Player)
    (hasHead hasLeftFist hasRightFist templeOrJungle aboveSurface dead visible : Bool)
    (worldSurface : Float := 50) : Option GolemBodyStep :=
  body.advanceTarget world (.ofPlayer player) hasHead hasLeftFist hasRightFist
    templeOrJungle aboveSurface dead visible worldSurface

end Lean4Terria
