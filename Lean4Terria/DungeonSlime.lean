import Lean4Terria.Ranged
import Lean4Terria.Water

namespace Lean4Terria

def maxDungeonSlimes : Nat := 5
def dungeonSlimeHealth : Nat := 150
def dungeonSlimeDefense : Nat := 7
def dungeonSlimeDamage : Nat := 30

/-- Regular pre-Hardmode Dungeon spawn rate at the current map depth.
Count slots in quarters so a Cursed Skull contributes exactly three. -/
def dungeonSpawnRate (daylight : Bool) (slotQuarters : Nat) : Nat :=
  let rate : Float32 := if daylight then 180 else 108
  let cap : Nat := if daylight then 36 else 40
  let multiplier : Float32 := if 5 * slotQuarters < cap then 0.6 else if 5 * slotQuarters < 2 * cap then 0.7
    else if 5 * slotQuarters < 3 * cap then 0.8 else if 5 * slotQuarters < 4 * cap then 0.9 else 1
  (rate * multiplier).toUInt32.toNat

/-- `NPC.NPCLoot_DropMoney` for unmodified Classic enemies.
The caller supplies distinct draws for the base variation and each bonus. -/
def dungeonEnemyCoins (base : Float32) (roll : Nat → Nat) : Nat := Id.run do
  let varied := fun (amount : Float32) (percent : Int) =>
    amount * (1 + (Int32.ofInt percent).toFloat32 * 0.01)
  let mut coins := varied base ((roll 0 % 96 : Int) - 20)
  let bonuses : Array (Nat × Nat × Nat) := #[(2, 5, 6), (4, 10, 11), (8, 15, 16),
    (16, 20, 21), (32, 25, 26), (64, 50, 51)]
  for index in [:bonuses.size] do
    let (chance, low, width) := bonuses[index]!
    if roll (2 * index + 1) % chance == 0 then
      coins := varied coins (low + roll (2 * index + 2) % width)
  return coins.toUInt32.toNat

def dungeonSlimeCoins (roll : Nat → Nat) : Nat := dungeonEnemyCoins 150 roll

/-- The original type 71 hitbox is 36×24 before its 1.25 scale is applied. -/
structure DungeonSlime where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  lastJumpX : Float32 := 0
  health : Nat := dungeonSlimeHealth
  jumpClock : Int := 0
  retargetClock : Nat := 0
  direction : Int := 1
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  frameClock : Nat := 0
  frame : Nat := 0
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq DungeonSlime where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.lastJumpX.toBits == b.lastJumpX.toBits &&
    a.health == b.health && a.jumpClock == b.jumpClock && a.retargetClock == b.retargetClock &&
    a.direction == b.direction && a.collideX == b.collideX && a.collideY == b.collideY && a.wet == b.wet &&
    a.frameClock == b.frameClock && a.frame == b.frame && a.timeLeft == b.timeLeft

private def pixels (units : Int) : Float32 := (Int32.ofInt units).toFloat32 / 16
private def subpixels (pixels : Float32) : Int := (pixels * 16).toInt32.toInt

def DungeonSlime.centerX (slime : DungeonSlime) : Float32 := slime.x + 22.5
def DungeonSlime.centerY (slime : DungeonSlime) : Float32 := slime.y + 15

def DungeonSlime.hitBox (slime : DungeonSlime) : HitBox :=
  let left := slime.x.toInt32.toInt * 16
  let top := slime.y.toInt32.toInt * 16
  ⟨left, top, left + 720, top + 480⟩

def DungeonSlime.tile (slime : DungeonSlime) : TilePos :=
  ⟨subpixels slime.centerX / tileUnits, subpixels slime.centerY / tileUnits⟩

def DungeonSlime.contact (slime : DungeonSlime) (player : Player) : Option Int :=
  if slime.hitBox.intersects (HitBox.ofNpcTarget player) then
    some (if slime.centerX < pixels (bodyCenterX player) then 1 else -1)
  else none

def DungeonSlime.valid (slime : DungeonSlime) (world : World) : Bool :=
  let margin : Int := 128 * tileUnits
  let x := subpixels slime.x
  let y := subpixels slime.y
  slime.id < maxDungeonSlimes && slime.health > 0 && slime.health ≤ dungeonSlimeHealth &&
    slime.x.isFinite && slime.y.isFinite && slime.vx.isFinite && slime.vy.isFinite && slime.lastJumpX.isFinite &&
    (-margin ≤ x) && x < world.width * tileUnits + margin &&
    (-margin ≤ y) && y < world.height * tileUnits + margin &&
    slime.vx.abs ≤ 16 && slime.vy.abs ≤ 16 && slime.lastJumpX.abs ≤ (pixels (world.width * tileUnits + margin)) &&
    -2120 ≤ slime.jumpClock && slime.jumpClock ≤ 0 && slime.retargetClock ≤ 200 &&
    (slime.direction == -1 || slime.direction == 1) && slime.frame < 2 && slime.frameClock < 8 &&
    slime.timeLeft > 0 && slime.timeLeft ≤ 937

private def floorTile (v : Float32) : Int := (v / 16).floor.toInt32.toInt
private def lastTile (v : Float32) : Int := (v / 16).ceil.toInt32.toInt - 1
private def edge (tile : Int) : Float32 := (Int32.ofInt (tile * 16)).toFloat32

private def DungeonSlime.solidOverlap (slime : DungeonSlime) (world : World) : Bool := Id.run do
  let col := floorTile slime.x
  let row := floorTile slime.y
  for dy in [:(lastTile (slime.y + 30) - row + 1).toNat] do
    for dx in [:(lastTile (slime.x + 45) - col + 1).toNat] do
      if (world.tile ⟨col + dx, row + dy⟩).solid then return true
  return false

private def DungeonSlime.face (slime : DungeonSlime) (player : Player) (dead : Bool) : DungeonSlime :=
  if dead then slime else
    let targetX := (Int32.ofInt (player.x.tdiv 16 + playerWidth / 32)).toFloat32
    { slime with direction := if targetX < slime.x + 22 then -1 else 1 }

/-- Classic style-1 AI for a regular Dungeon Slime, before gravity and collision.
The Boolean result is the crouch animation flag from `aiAction`. -/
def DungeonSlime.advanceAI (slime : DungeonSlime) (player : Player) (dead hostile : Bool)
    (embedded : DungeonSlime → Bool) :
    DungeonSlime × Bool := Id.run do
  let mut next := if slime.retargetClock > 1 then { slime with retargetClock := slime.retargetClock - 1 }
    else slime
  if next.wet then
    let vy : Float32 := if next.collideY then -2 else next.vy
    if vy < 0 && next.lastJumpX == next.x then
      next := { next with direction := -next.direction, retargetClock := 200 }
    let lastJumpX := if vy > 0 then next.x else next.lastJumpX
    let vy := max (-4) ((if vy > 2 then vy * 0.9 else vy) - 0.5)
    next := { next with vy, lastJumpX }
    if next.retargetClock == 1 && hostile then next := next.face player dead
  if next.retargetClock == 0 then
    next := { (next.face player dead) with retargetClock := 1, jumpClock := -100 }
  if next.vy == 0 then
    if next.collideY && embedded next then
      next := { next with x := next.x - (next.vx + (Int32.ofInt next.direction).toFloat32) }
    if next.lastJumpX == next.x then
      next := { next with direction := -next.direction, retargetClock := 200 }
    let slowed := next.vx * 0.8
    let vx := if -0.1 < slowed.toFloat && slowed.toFloat < 0.1 then 0 else slowed
    let jumpClock := next.jumpClock + if hostile then 5 else 4
    next := { next with vx, jumpClock, lastJumpX := 0 }
    let jump := if jumpClock ≥ 0 then 1 else if -1000 ≤ jumpClock && jumpClock ≤ -500 then 2
      else if -2000 ≤ jumpClock && jumpClock ≤ -1500 then 3 else 0
    if jump == 0 then return (next, jumpClock ≥ -30)
    if next.retargetClock == 1 && hostile then next := next.face player dead
    let side := (Int32.ofInt next.direction).toFloat32
    if jump == 3 then
      return ({ next with vx := next.vx + 3 * side, vy := -8, jumpClock := -200, lastJumpX := next.x }, false)
    return ({ next with vx := next.vx + 2 * side, vy := -6, jumpClock := if jump == 1 then -1120 else -2120 }, false)
  let side := (Int32.ofInt next.direction).toFloat32
  if side * next.vx < 3 then
    if next.collideX && next.vx.abs == 0.2 then next := { next with x := next.x - 1.4 * side }
    if next.collideY && embedded next then next := { next with x := next.x - (next.vx + side) }
    let vx := if side.toFloat * next.vx.toFloat > -0.01 then next.vx + 0.2 * side else next.vx * 0.93
    next := { next with vx }
  return (next, false)

private def DungeonSlime.move (slime : DungeonSlime) (world : World) (vx vy : Float32)
    (wet : Bool) : DungeonSlime := Id.run do
  let y0 := floorTile slime.y
  let y1 := lastTile (slime.y + 30)
  let mut dx := vx
  if vx != 0 then
    let right := vx > 0
    let col := if right then lastTile (slime.x + 45 + vx) else floorTile (slime.x + vx)
    if (List.range (y1 - y0 + 1).toNat).any (fun offset => (world.tile ⟨col, y0 + offset⟩).solid) then
      let stop := if right then edge col - 45 else edge (col + 1)
      dx := if right then max 0 (stop - slime.x) else min 0 (stop - slime.x)
  let horizontal := if wet && dx == vx then dx * 0.5 else dx
  let x := slime.x + horizontal
  let x0 := floorTile x
  let x1 := lastTile (x + 45)
  let mut dy := vy
  if vy != 0 then
    let falling := vy > 0
    let row := if falling then lastTile (slime.y + 30 + vy) else floorTile (slime.y + vy)
    let platform := falling && slime.y + 30 ≤ edge row
    if (List.range (x1 - x0 + 1).toNat).any (fun offset =>
        let tile := world.tile ⟨x0 + offset, row⟩
        tile.solid || (platform && tile == .platform)) then
      let stop := if falling then edge row - 30 else edge (row + 1)
      dy := if falling then max 0 (stop - slime.y) else 0.01
  let vertical := if wet && dy == vy then dy * 0.5 else dy
  return { slime with x, y := slime.y + vertical, vx := dx, vy := dy, collideX := dx != vx, collideY := dy != vy, wet }

private def DungeonSlime.checkActive (slime : DungeonSlime) (player : Player) : Option DungeonSlime := do
  let playerBox := HitBox.ofNpcTarget player
  -- CheckActive uses integer half-width for its center, unlike Entity.Center.
  let area := fun (halfWidth halfHeight : Int) =>
    let left := ((slime.x + 22).toFloat - halfWidth.toFloat).toInt32.toInt * 16
    let top := ((slime.y + 15).toFloat - halfHeight.toFloat).toInt32.toInt * 16
    (⟨left, top, left + 32 * halfWidth, top + 32 * halfHeight⟩ : HitBox)
  if !(area 4032 2520).intersects playerBox then failure
  let timeLeft := (if (area 1005 630).intersects playerBox then 750 else slime.timeLeft) - 1
  if timeLeft == 0 then failure
  return { slime with timeLeft }

def DungeonSlime.advance (slime : DungeonSlime) (world : World) (water : Water) (player : Player)
    (dead daylight : Bool) (surface : Nat) : Option DungeonSlime := do
  let hostile := !daylight || slime.health < dungeonSlimeHealth || slime.y > edge surface
  let (next, crouching) := slime.advanceAI player dead hostile (·.solidOverlap world)
  let vx := if -0.005 < next.vx.toFloat && next.vx.toFloat < 0.005 then 0 else next.vx
  let vy := min (if slime.wet then 7 else 10) (next.vy + if slime.wet then 0.2 else 0.3)
  let wet := water.submerged world (subpixels next.centerX) (subpixels next.centerY)
  let moved := next.move world vx vy wet
  let frameClock := moved.frameClock + if crouching then 3 else if moved.vx != 0 || moved.vy != 0 then 2 else 1
  let framed := if frameClock ≥ 8 then { moved with frameClock := 0, frame := 1 - moved.frame }
    else { moved with frameClock }
  framed.checkActive player

def DungeonSlime.atFloor (id : Nat) (column floor : Int) : DungeonSlime :=
  { id, x := edge column + 8 - 22.5, y := edge floor - 30 }

/-- Grounded candidates in the compact Dungeon, outside the player's immediate area. -/
def spawnDungeonSlime (world : World) (water : Water) (seed : UInt64) (player : Player) (id roll : Nat)
    (lowerDungeon := false) (blueMoonDungeon := false) (magicMissileDungeon := false) (deepDungeon := false) :
    Option DungeonSlime :=
  findDungeonSpawnFloor world seed player.centerTile
    (fun p => dungeonUnsafeWall seed p lowerDungeon blueMoonDungeon magicMissileDungeon deepDungeon)
    roll (lowerDungeon := lowerDungeon) (blueMoonDungeon := blueMoonDungeon)
    (magicMissileDungeon := magicMissileDungeon) (deepDungeon := deepDungeon) fun column floor => do
    let slime := DungeonSlime.atFloor id column floor
    if slime.solidOverlap world || water.submerged world (subpixels slime.centerX) (subpixels slime.centerY) then
      failure
    return slime

end Lean4Terria
