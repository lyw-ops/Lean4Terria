import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

def maxDarkCasters : Nat := 5
def maxWaterSpheres : Nat := 200
def darkCasterWidth : Nat := 18
def darkCasterHeight : Nat := 40
def darkCasterHealth : Nat := 50
def darkCasterDefense : Nat := 2
def darkCasterDamage : Nat := 20
def waterSphereWidth : Nat := 16
def waterSphereHeight : Nat := 16
def waterSphereDamage : Nat := 20

structure DarkCaster where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := darkCasterHealth
  timeLeft : Nat := 937
  clock : Nat := 0
  casting : Nat := 0
  destination : Option TilePos := none
  direction : Int := 1
  spriteDirection : Int := -1
  deriving Repr, Inhabited

instance : BEq DarkCaster where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.health == b.health &&
    a.timeLeft == b.timeLeft && a.clock == b.clock && a.casting == b.casting &&
    a.destination == b.destination && a.direction == b.direction && a.spriteDirection == b.spriteDirection

structure WaterSphere where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq WaterSphere where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.timeLeft == b.timeLeft

private def f32 (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def px (units : Int) : Float32 := f32 units / 16
private def tileEdge (n : Int) : Float32 := f32 (n * 16)
private def trunc (x : Float32) : Int := x.toInt32.toInt
private def tileFloor (x : Float32) : Int := trunc ((x / 16).floor)
private def tileLast (x : Float32) : Int := trunc ((x / 16).ceil) - 1
private def playerCenter (player : Player) : Float32 × Float32 :=
  (px (bodyCenterX player), px (bodyCenterY player))

def DarkCaster.centerX (caster : DarkCaster) : Float32 := caster.x + 9
def DarkCaster.centerY (caster : DarkCaster) : Float32 := caster.y + 20

def CasterAI.npcBox (x y : Float32) (width height : Nat) : HitBox :=
  let left := trunc x * 16
  let top := trunc y * 16
  ⟨left, top, left + 16 * width, top + 16 * height⟩

def DarkCaster.hitBox (caster : DarkCaster) : HitBox :=
  CasterAI.npcBox caster.x caster.y darkCasterWidth darkCasterHeight

def WaterSphere.hitBox (sphere : WaterSphere) : HitBox :=
  CasterAI.npcBox sphere.x sphere.y waterSphereWidth waterSphereHeight

def CasterAI.contact (box : HitBox) (x : Float32) (player : Player) : Option Int :=
  let target := HitBox.ofNpcTarget player
  if box.intersects target then
    some (if x < px player.x + f32 (playerWidth / 32) then 1 else -1)
  else none

def DarkCaster.contact (caster : DarkCaster) (player : Player) : Option Int :=
  CasterAI.contact caster.hitBox caster.centerX player

def WaterSphere.contact (sphere : WaterSphere) (player : Player) : Option Int :=
  CasterAI.contact sphere.hitBox (sphere.x + 8) player

def CasterAI.validMotion (world : World) (x y vx vy : Float32) : Bool :=
  x.isFinite && y.isFinite && vx.isFinite && vy.isFinite &&
    -4096 ≤ x && x < tileEdge world.width + 4096 &&
    -4096 ≤ y && y < tileEdge world.height + 4096 && vx.abs ≤ 16 && vy.abs ≤ 16

def DarkCaster.valid (caster : DarkCaster) (world : World) : Bool :=
  caster.id > 0 && CasterAI.validMotion world caster.x caster.y caster.vx caster.vy &&
    caster.health > 0 && caster.health ≤ darkCasterHealth && caster.clock < 650 && caster.casting ≤ 29 &&
    (caster.direction == -1 || caster.direction == 1) &&
    (caster.spriteDirection == -1 || caster.spriteDirection == 1) &&
    caster.destination.all (fun p => p.x > 0 && p.y > 0 && world.contains p) &&
    caster.timeLeft > 0 && caster.timeLeft ≤ 937

def WaterSphere.valid (sphere : WaterSphere) (world : World) : Bool :=
  sphere.id > 0 && CasterAI.validMotion world sphere.x sphere.y sphere.vx sphere.vy &&
    sphere.vx.abs ≤ 6.000001 && sphere.vy.abs ≤ 6.000001 &&
    sphere.timeLeft > 0 && sphere.timeLeft ≤ 749

def CasterAI.checkActive (x y : Float32) (width height : Nat) (player : Player) (timeLeft : Nat) : Option Nat := do
  let target := HitBox.ofNpcTarget player
  let area := fun (w h : Int) =>
    let left := trunc (x - f32 w) * 16
    let top := trunc (y - f32 h) * 16
    (⟨left, top, left + 32 * w, top + 32 * h⟩ : HitBox)
  if !(area 4032 2520).intersects target then failure
  let remaining := (if (area (960 + width) (600 + height)).intersects target then 750 else timeLeft) - 1
  if remaining == 0 then failure
  return remaining

def CasterAI.sphereVelocity (x y speed : Float32) (player : Player) : Float32 × Float32 :=
  let (tx, ty) := playerCenter player
  let dx := tx - (x + 8)
  let dy := ty - (y + 8)
  let distance := ((dx * dx + dy * dy).toFloat.sqrt).toFloat32
  let scale := speed / (if distance == 0 then 1 else distance)
  (dx * scale, dy * scale)

/-- Type 33 targets once, from the integer spawn point passed to NewNPC.
Classic speed is six pixels per tick, independent of the caster's momentum. -/
def WaterSphere.fromCaster (id : Nat) (caster : DarkCaster) (player : Player) : WaterSphere :=
  let x := f32 (trunc caster.x + 1)
  let y := f32 (trunc caster.y - 24)
  let (vx, vy) := CasterAI.sphereVelocity x y 6 player
  { id, x, y, vx, vy }

def CasterAI.moveSphere (x y vx vy : Float32) (timeLeft : Nat) (player : Player) :
    Option (Float32 × Float32 × Float32 × Nat) := do
  let vx := if -0.005 < vx && vx < 0.005 then 0 else vx
  let x := x + vx
  let y := y + vy
  -- EncourageDespawn runs before CheckActive, which can refresh a nearby sphere.
  let timeLeft ← CasterAI.checkActive (x + 8) (y + 8) waterSphereWidth waterSphereHeight player (min 100 timeLeft)
  return (x, y, vx, timeLeft)

def WaterSphere.advance (sphere : WaterSphere) (player : Player) : Option WaterSphere := do
  let (x, y, vx, timeLeft) ← CasterAI.moveSphere sphere.x sphere.y sphere.vx sphere.vy sphere.timeLeft player
  return { sphere with x, y, vx, timeLeft }

def DarkCaster.atFloor (id : Nat) (column floor : Int) : DarkCaster :=
  { id, x := tileEdge column - 1, y := tileEdge floor - 40 }

/-- Classic `StrikeNPC_Inner` velocity response for a non-critical hit. -/
def CasterAI.knockback (vx vy : Float32) (health damage : Nat) (direction : Int)
    (force resistance : Float32) : Float32 × Float32 := Id.run do
  if force ≤ 0 then return (vx, vy)
  let mut k := force * resistance
  for (threshold, factor) in ([(8, 0.9), (10, 0.8), (12, 0.7), (14, 0.6)] : List (Float32 × Float32)) do
    if k > threshold then k := threshold + (k - threshold) * factor
  k := min 16 k
  let side := f32 direction
  if damage * 10 ≤ health then
    return (k * side * 0.6, -0.75 * k * 0.6)
  let along := vx * side
  let vx := if along ≥ k then vx else
    min k (along + k + (if along < 0 then k else 0)) * side
  let up := -0.75 * k
  let vy := if vy > up then max up (vy + up) else vy
  return (vx, vy)

def DarkCaster.knockback (caster : DarkCaster) (damage : Nat) (direction : Int)
    (force : Float32) : DarkCaster :=
  let (vx, vy) := CasterAI.knockback caster.vx caster.vy darkCasterHealth damage direction force 0.6
  { caster with vx, vy }

def CasterAI.spaceAbove (world : World) (column floor : Int) (height : Nat) : Bool :=
  (List.range 3).all fun dx => (List.range height).all fun dy =>
    let p : TilePos := ⟨column - 1 + dx, floor - height + dy⟩
    (world.tile? p).any fun tile => !tile.solid

/-- Caster teleport candidates are drawn in a 41×41 square around the target.
Scan downward and exclude the player's current and predicted positions. -/
def CasterAI.findTeleport (centerX centerY : Float32) (world : World) (player : Player)
    (dead : Bool) (landingAllowed : TilePos → Bool) (roll : Nat → Nat) : Option TilePos := Id.run do
  let (tx, ty) := playerCenter player
  let targetX := (trunc tx).tdiv 16
  let targetY := (trunc ty).tdiv 16
  let casterX := (trunc centerX).tdiv 16
  let casterY := (trunc centerY).tdiv 16
  if 16 * ((casterX - targetX).natAbs + (casterY - targetY).natAbs) > 2000 then return none
  let body := HitBox.ofNpcTarget player
  let dx := trunc (px player.vx * 20) * 16
  let dy := trunc (px player.vy * 20) * 16
  let future : HitBox := ⟨body.left + min dx 0, body.top + min dy 0,
    body.right + max dx 0, body.bottom + max dy 0⟩
  for attempt in [:100] do
    let x := targetX - 20 + (roll (2 * attempt) % 41 : Nat)
    let start := targetY - 20 + (roll (2 * attempt + 1) % 41 : Nat)
    for offset in [:(targetY + 20 - start).toNat] do
      let y := start + offset
      let pos : TilePos := ⟨x, y⟩
      if (x - casterX).natAbs ≤ 1 && (y - casterY).natAbs ≤ 1 then continue
      if !(world.tile? pos).any Tile.solid then continue
      if !landingAllowed ⟨x, y - 1⟩ then continue
      if !CasterAI.spaceAbove world x y 4 then continue
      let reserved : HitBox := ⟨(x - 5) * 256, (y - 5) * 256, (x + 6) * 256, (y + 6) * 256⟩
      if !dead && reserved.intersects future then continue
      return some pos
  return none

def DarkCaster.findTeleport (caster : DarkCaster) (world : World) (magma : Water) (player : Player)
    (dead skeletron : Bool) (unsafeWall : TilePos → Bool) (roll : Nat → Nat) : Option TilePos :=
  CasterAI.findTeleport caster.centerX caster.centerY world player dead
    (fun above => if unsafeWall above then magma.level world above == 0 else skeletron) roll

/-- Type 32's style 8 update, before gravity, collision, and inactivity.
Return the caster position at the instant of firing. -/
def DarkCaster.advanceAI (caster : DarkCaster) (world : World) (magma : Water) (player : Player)
    (dead skeletron : Bool) (unsafeWall : TilePos → Bool) (roll : Nat → Nat) :
    DarkCaster × Option DarkCaster := Id.run do
  let (tx, ty) := playerCenter player
  let targetX := f32 (player.x.tdiv 16 + playerWidth / 32)
  let direction := if dead then caster.direction else if targetX < caster.centerX then -1 else 1
  let vx := caster.vx * 0.93
  let vx := if -0.1 < vx && vx < 0.1 then 0 else vx
  let mut next := { caster with direction, vx }
  if let some pos := caster.destination then
    next := { next with x := tileEdge pos.x - 1, y := tileEdge pos.y - 40, vx := 0, vy := 0, destination := none }
  let clock := (if next.clock == 0 then 500 else next.clock) + 1
  let dx := trunc next.centerX - trunc tx
  let dy := trunc next.centerY - trunc ty
  let inRange := -910 ≤ dx && dx < 910 && -550 ≤ dy && dy < 550
  let casting := if inRange && (clock == 100 || clock == 200 || clock == 300) then 30 else next.casting
  next := { next with clock, casting }
  if next.clock ≥ 650 then
    next := { next with clock := 1 }
    if let some p := next.findTeleport world magma player dead skeletron unsafeWall roll then
      next := { next with destination := some p, casting := 20 }
  next := { next with casting := next.casting - 1 }
  return (next, if next.casting == 25 then some next else none)

/-- Shared style-8 movement for 18×40 casters. -/
def CasterAI.move (x y vx vy : Float32) (world : World) (wet : Bool) (surface : Nat) :
    Float32 × Float32 × Float32 × Float32 := Id.run do
  let altitude := (y / 16 - 70) / max 1 (f32 surface / 6)
  let gravity := if wet then 0.2 else 0.3 * max 0.25 (min 1 altitude)
  let vy := min (if wet then 7 else 10) (vy + gravity)
  let mut dx := vx
  if vx != 0 then
    let right := vx > 0
    let col := if right then tileLast (x + 18 + vx) else tileFloor (x + vx)
    let row := tileFloor y
    if (List.range (tileLast (y + 40) - row + 1).toNat).any
        (fun n => (world.tile ⟨col, row + n⟩).solid) then
      dx := if right then max 0 (tileEdge col - 18 - x) else min 0 (tileEdge (col + 1) - x)
  let x := x + if wet && vx == dx then dx * 0.5 else dx
  let mut dy := vy
  if vy != 0 then
    let falling := vy > 0
    let row := if falling then tileLast (y + 40 + vy) else tileFloor (y + vy)
    let col := tileFloor x
    let canLand := falling && y + 40 ≤ tileEdge row
    if (List.range (tileLast (x + 18) - col + 1).toNat).any (fun n =>
        let tile := world.tile ⟨col + n, row⟩
        tile.solid || (canLand && tile == .platform)) then
      dy := if falling then max 0 (tileEdge row - 40 - y) else 0.01
  return (x, y + (if wet && vy == dy then dy * 0.5 else dy), dx, dy)

def DarkCaster.advance (caster : DarkCaster) (world : World) (water magma : Water) (player : Player)
    (dead skeletron : Bool) (surface : Nat) (unsafeWall : TilePos → Bool) (roll : Nat → Nat) :
    Option DarkCaster × Option DarkCaster :=
  let (next, fired) := caster.advanceAI world magma player dead skeletron unsafeWall roll
  let wet := water.submerged world (trunc (next.centerX * 16)) (trunc (next.centerY * 16))
  let (x, y, vx, vy) := CasterAI.move next.x next.y next.vx next.vy world wet surface
  let moved : DarkCaster := { next with x, y, vx, vy }
  let framed := if moved.vy == 0 then { moved with spriteDirection := moved.direction } else moved
  let active := (CasterAI.checkActive framed.centerX framed.centerY darkCasterWidth darkCasterHeight player framed.timeLeft).map
    fun timeLeft => { framed with timeLeft }
  (active, fired)

def spawnDarkCaster (world : World) (water : Water) (seed : UInt64) (player : Player)
    (unsafeWall : TilePos → Bool) (id roll : Nat) (lowerDungeon := false) (blueMoonDungeon := false)
    (magicMissileDungeon := false) (deepDungeon := false) : Option DarkCaster :=
  findDungeonSpawnFloor world seed player.centerTile unsafeWall roll (lowerDungeon := lowerDungeon)
    (blueMoonDungeon := blueMoonDungeon) (magicMissileDungeon := magicMissileDungeon)
    (deepDungeon := deepDungeon) fun column floor => do
    if !CasterAI.spaceAbove world column floor 3 then failure
    let caster := DarkCaster.atFloor id column floor
    if water.submerged world (trunc (caster.centerX * 16)) (trunc (caster.centerY * 16)) then failure
    return caster

end Lean4Terria
