import Lean4Terria.DarkCaster
import Lean4Terria.JungleTemple
import Lean4Terria.UnderworldCreatures

namespace Lean4Terria

inductive TempleEnemyKind where
  | lihzahrd | crawlingLihzahrd | flyingSnake
  deriving BEq, Repr, Inhabited

def TempleEnemyKind.npcId : TempleEnemyKind → Nat
  | .lihzahrd => 198
  | .crawlingLihzahrd => 199
  | .flyingSnake => 226

def TempleEnemyKind.ofNpcId? : Nat → Option TempleEnemyKind
  | 198 => some .lihzahrd
  | 199 => some .crawlingLihzahrd
  | 226 => some .flyingSnake
  | _ => none

def TempleEnemyKind.name : TempleEnemyKind → String
  | .flyingSnake => "Flying Snake"
  | _ => "Lihzahrd"

def TempleEnemyKind.width : TempleEnemyKind → Nat
  | .lihzahrd => 18
  | .crawlingLihzahrd => 42
  | .flyingSnake => 34

def TempleEnemyKind.height : TempleEnemyKind → Nat
  | .lihzahrd => 40
  | .crawlingLihzahrd => 30
  | .flyingSnake => 50

def TempleEnemyKind.maxHealth (kind : TempleEnemyKind) : Nat :=
  if kind == .flyingSnake then 260 else 400

def TempleEnemyKind.defense : TempleEnemyKind → Nat
  | .lihzahrd => 20
  | .crawlingLihzahrd => 30
  | .flyingSnake => 28

def TempleEnemyKind.damage : TempleEnemyKind → Nat
  | .lihzahrd => 38
  | .crawlingLihzahrd => 60
  | .flyingSnake => 85

def TempleEnemyKind.baseCoins (kind : TempleEnemyKind) : Nat :=
  if kind == .flyingSnake then 400 else 650

def TempleEnemyKind.halfSlots (kind : TempleEnemyKind) : Nat := if kind == .flyingSnake then 1 else 2

def TempleEnemyKind.frameCount : TempleEnemyKind → Nat
  | .lihzahrd => 16
  | .crawlingLihzahrd => 8
  | .flyingSnake => 4

/-- Motion is in native pixels. The same four AI slots are used by either
the fighter or bat AI, exactly as in the original NPC record. -/
structure TempleEnemy where
  id : Nat
  kind : TempleEnemyKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  previousX : Float32 := 0
  previousVx : Float32 := 0
  previousVy : Float32 := 0
  health : Nat := kind.maxHealth
  direction : Int := 1
  directionY : Int := -1
  spriteDirection : Int := -1
  collideX : Bool := false
  collideY : Bool := false
  ai0 : Nat := 0
  ai1 : Nat := 0
  ai2 : Int := 0
  ai3 : Nat := 0
  frame : Nat := 0
  frameClock : Float := 0
  rotation : Float32 := 0
  timeLeft : Nat := 750
  wet : Bool := false
  gfxOffsetY : Float32 := 0
  stepSpeed : Float32 := 0
  justHit : Bool := false
  deriving BEq, Repr, Inhabited

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def tile (n : Float32) : Int := (n / 16).toInt32.toInt
private def floorY (enemy : TempleEnemy) : Float32 := enemy.y + single enemy.kind.height

def TempleEnemy.centerX (enemy : TempleEnemy) : Float32 := enemy.x + single (enemy.kind.width / 2)
def TempleEnemy.centerY (enemy : TempleEnemy) : Float32 := enemy.y + single (enemy.kind.height / 2)

def TempleEnemy.atFloor (id : Nat) (kind : TempleEnemyKind) (p : TilePos) : TempleEnemy :=
  { id, kind, x := single (p.x * 16 + 8 - (kind.width / 2 : Nat)), y := single (p.y * 16 - kind.height) }

def TempleEnemy.hitBox (enemy : TempleEnemy) : HitBox :=
  CasterAI.npcBox enemy.x enemy.y enemy.kind.width enemy.kind.height

def TempleEnemy.contact (enemy : TempleEnemy) (player : Player) : Option Int :=
  CasterAI.contact enemy.hitBox enemy.centerX player

def TempleEnemy.valid (enemy : TempleEnemy) (world : World) : Bool :=
  enemy.id > 0 && 0 < enemy.health && enemy.health ≤ enemy.kind.maxHealth &&
    CasterAI.validMotion world enemy.x enemy.y enemy.vx enemy.vy &&
    enemy.previousX.isFinite && enemy.previousVx.isFinite && enemy.previousVy.isFinite &&
    enemy.previousVx.abs ≤ 32 && enemy.previousVy.abs ≤ 32 &&
    [-1, 1].contains enemy.direction && [-1, 1].contains enemy.directionY &&
    [-1, 1].contains enemy.spriteDirection && 0 < enemy.timeLeft && enemy.timeLeft ≤ 750 &&
    enemy.frame < enemy.kind.frameCount && enemy.frameClock.isFinite &&
    0 ≤ enemy.frameClock && enemy.frameClock ≤ 12 && enemy.rotation.isFinite &&
    enemy.gfxOffsetY.isFinite && enemy.gfxOffsetY.abs ≤ 33 && [0, 1, 2].contains enemy.stepSpeed &&
    (if enemy.kind == .flyingSnake then enemy.ai0 == 0 && enemy.ai1 ≤ 1000 &&
        -300 ≤ enemy.ai2 && enemy.ai2 ≤ 300 && enemy.ai3 == 0
      else enemy.ai0 ≤ 1 && enemy.ai1 ≤ 10 && 0 ≤ enemy.ai2 && enemy.ai2 < 60 && enemy.ai3 ≤ 600)

def TempleEnemy.knockback (enemy : TempleEnemy) (damage : Nat) (direction : Int)
    (force : Float32) : TempleEnemy :=
  let enemy := { enemy with justHit := true }
  let (resist, lift) : Float32 × Float32 := match enemy.kind with
    | .lihzahrd => (0.4, 0.75)
    | .crawlingLihzahrd => (0, 0.75)
    | .flyingSnake => (0.65, 0.5)
  if resist == 0 then enemy else
    let (vx, vy) := npcKnockbackVelocity enemy.vx enemy.vy force resist lift damage enemy.kind.maxHealth direction false
    { enemy with vx, vy }

private def TempleEnemy.face (enemy : TempleEnemy) (player : Player) (dead : Bool) : TempleEnemy :=
  if dead then enemy else
    let px := single (player.x.tdiv 16 + playerWidth / 32)
    let py := single (player.y.tdiv 16 + playerHeight / 32)
    let above := if enemy.kind == .flyingSnake then py < enemy.centerY else
      single (player.y.tdiv 16 + playerHeight / 16) ≤ floorY enemy
    { enemy with direction := if px < enemy.centerX then -1 else 1, directionY := if above then -1 else 1 }

private def TempleEnemy.transform (enemy : TempleEnemy) (player : Player) (dead : Bool) : TempleEnemy :=
  if enemy.kind == .lihzahrd && enemy.health ≤ 220 then
    ({ enemy with
      kind := .crawlingLihzahrd, y := enemy.y + 10, ai0 := 0, ai1 := 0, ai2 := 0, ai3 := 0
      frame := 0, frameClock := 0, rotation := 0, timeLeft := 750 } : TempleEnemy).face player dead
  else enemy

private def batSteer (v : Float32) (direction : Int) (cap accel reverseAssist brake : Float32) : Float32 :=
  let sign := single direction
  if sign * v ≥ cap then v else
    let accelerated := v + sign * accel
    let steered := if sign * accelerated < -cap then accelerated + sign * reverseAssist
      else if sign * accelerated < 0 then accelerated - sign * brake else accelerated
    if sign * steered > cap then sign * cap else steered

private def TempleEnemy.advanceSnake (enemy : TempleEnemy) (world : World) (player : Player)
    (dead targetWet : Bool) : TempleEnemy := Id.run do
  let x := enemy.previousVx * -0.5
  let y := enemy.previousVy * -0.5
  let vx := if !enemy.collideX then enemy.vx else
    if enemy.direction == -1 && 0 < x && x < 2 then 2
    else if enemy.direction == 1 && -2 < x && x < 0 then -2 else x
  let vy := if !enemy.collideY then enemy.vy else
    if 0 < y && y < 1 then 1 else if -1 < y && y < 0 then -1 else y
  let visible := npcCanSee world enemy.x enemy.y enemy.kind.width enemy.kind.height player
  let mut next := { enemy with vx, vy }.face player dead
  if !visible then
    next := { next with direction := if vx < 0 then -1 else 1, directionY := if vy < 0 then -1 else 1 }
  next := { next with
    vx := batSteer vx next.direction 4 0.2 0.1 0.05
    vy := batSteer vy next.directionY 2.5 0.1 0.05 0.03
    ai1 := next.ai1 + 1 }
  if next.ai1 ≤ 200 then return next
  let orbit := next.ai2 + 1
  let vy := if orbit > 0 then
      if next.vy < 1.5 then next.vy + 0.1 else next.vy
    else if next.vy > -1.5 then next.vy - 0.1 else next.vy
  let vx := if orbit < -150 || orbit > 150 then
      if next.vx < 4 then next.vx + 0.2 else next.vx
    else if next.vx > -4 then next.vx - 0.2 else next.vx
  return { next with
    vx, vy, ai1 := if (!targetWet && visible) || next.ai1 > 1000 then 0 else next.ai1
    ai2 := if orbit > 300 then -300 else orbit }

private def TempleEnemy.chase (enemy : TempleEnemy) (player : Player) (dead daylight : Bool)
    (surface : Nat) : TempleEnemy := Id.run do
  let opposing := enemy.vy == 0 && enemy.vx * single enemy.direction < 0
  let stuck := if enemy.x == enemy.previousX || enemy.ai3 ≥ 60 || opposing then enemy.ai3 + 1
    else if enemy.vx.abs.toFloat > 0.9 then enemy.ai3 - 1 else enemy.ai3
  let ai3 := if stuck > 600 || enemy.justHit || enemy.hitBox.intersects (HitBox.ofNpcTarget player)
    then 0 else stuck
  let directionY := if single (player.y + playerHeight) / 16 == floorY enemy then -1 else enemy.directionY
  let mut next := { enemy with ai3, directionY }
  if next.ai3 < 60 then
    next := next.face player dead
    if next.directionY > 0 && single (bodyCenterY player) / 16 ≤ floorY next then
      next := { next with directionY := -1 }
  else
    if daylight && next.y / 16 < single surface then next := { next with timeLeft := min 10 next.timeLeft }
    if next.vx != 0 then next := { next with ai0 := 0 }
    else if next.vy == 0 then
      next := if next.ai0 + 1 ≥ 2 then
          { next with ai0 := 0, direction := -next.direction, spriteDirection := -next.direction }
        else { next with ai0 := next.ai0 + 1 }
  let crawling := next.kind == .crawlingLihzahrd
  let speed : Float32 := if crawling then 4 else 1.5
  let amount : Float32 := if crawling then 0.1 else 0.07
  let side := single next.direction
  if next.vx.abs > speed then
    return if next.vy == 0 then { next with vx := next.vx * 0.8 } else next
  let vx := if crawling && next.vy == 0 && side * next.vx < 0 then next.vx * 0.8 else next.vx
  return { next with vx := max (-speed) (min speed (vx + side * amount)) }

private def solid (world : World) (x y : Int) : Bool := (world.tile ⟨x, y⟩).solid

private def supported (world : World) (x y : Int) : Bool :=
  solid world x y || world.tile ⟨x, y⟩ == .platform

private def TempleEnemy.stepUp (enemy : TempleEnemy) (world : World) : TempleEnemy :=
  let width := single enemy.kind.width
  let x := enemy.x + enemy.vx
  let side : Int := if enemy.vx < 0 then -1 else if enemy.vx > 0 then 1 else 0
  let half : Int := enemy.kind.width / 2
  let col := tile (x + single (half + (half + 1) * side))
  let row := tile (floorY enemy - 1)
  let top := single (row * 16)
  let height := floorY enemy - top
  if enemy.vy < 0 || !world.contains ⟨col, row⟩ || top ≥ floorY enemy || height > 16.1 ||
      single (col * 16) ≥ x + width || single ((col + 1) * 16) ≤ x || !solid world col row ||
      solid world col (row - 1) || solid world col (row - 2) || solid world col (row - 3) ||
      supported world (col - side) (row - 3) then enemy
  else
    { enemy with
      y := top - single enemy.kind.height, gfxOffsetY := enemy.gfxOffsetY + height
      stepSpeed := if height < 9 then 1 else 2 }

private def TempleEnemy.advanceFighter (enemy : TempleEnemy) (world : World) (player : Player)
    (dead daylight : Bool) (surface : Nat) : TempleEnemy × World := Id.run do
  let wasStationary := enemy.vx == 0 && !enemy.justHit
  let next := (enemy.transform player dead).chase player dead daylight surface
  let left := tile (next.x + 8)
  let right := tile (next.x + single next.kind.width - 8)
  let foot := tile (floorY next + 7)
  let roof := tile (next.y - 9)
  let columns := List.range' left.toNat (right - left + 1).toNat
  let onFloor := next.vy == 0 && columns.any (fun x => supported world x foot) &&
    !columns.any (fun x => solid world x roof)
  let mut next := next.stepUp world
  if !onFloor then return (next, world)
  let crawling := next.kind == .crawlingLihzahrd
  let probe : Int := if crawling then (next.kind.width / 2 : Nat) + 16 else 15
  let col := tile (next.centerX + single (probe * next.direction))
  let row := tile (floorY next - 15)
  let door : TilePos := ⟨col, row - 1⟩
  if !crawling then
    if let some (hinge, mode) := TempleDoor.atCell world door then
      if mode == .locked || mode == .closed then
        if next.ai2 + 1 < 60 then return ({ next with ai2 := next.ai2 + 1, ai3 := 0 }, world)
        let taps := min 10 (next.ai1 + 5)
        next := { next with ai1 := taps, ai2 := 0, ai3 := 0, vx := -0.5 * single next.direction }
        if taps < 10 then return (next, world)
        let opened := TempleDoor.openIn world hinge next.direction (· == .air)
        return (if opened.isSome then next else { next with ai3 := 60 }, opened.getD world)
  if next.vx * single next.spriteDirection ≤ 0 then return (next, world)
  if next.kind.height ≥ 32 && solid world col (row - 2) then
    next := { next with vy := if solid world col (row - 3) then -8 else -7 }
  else if solid world col (row - 1) then next := { next with vy := -6 }
  else if floorY next - single (row * 16) > 20 && solid world col row then
    next := { next with vy := -5 }
  else if next.directionY < 0 && !supported world col (row + 1) &&
      !supported world (col + next.direction) (row + 1) then
    next := { next with vy := -8, vx := next.vx * 1.5 }
  else if !crawling then next := { next with ai1 := 0, ai2 := 0 }
  if next.vy == 0 && wasStationary && next.ai3 == 1 then next := { next with vy := -5 }
  return (next, world)

private def TempleEnemy.animate (enemy : TempleEnemy) : TempleEnemy :=
  match enemy.kind with
  | .flyingSnake =>
    let clock := enemy.frameClock + 1
    let spriteDirection := if enemy.vx < 0 then -1 else if enemy.vx > 0 then 1 else enemy.spriteDirection
    { enemy with
      spriteDirection, rotation := enemy.vx * 0.1
      frameClock := if clock ≥ 6 then 0 else clock
      frame := if clock ≥ 6 then (enemy.frame + 1) % 4 else enemy.frame }
  | .crawlingLihzahrd =>
    let next := if enemy.vy == 0 then { enemy with spriteDirection := enemy.direction } else enemy
    if next.frame < 3 then
      let clock := next.frameClock + 1
      if clock > 6 then { next with frameClock := 0, frame := next.frame + 1 } else { next with frameClock := clock }
    else if next.vy == 0 then
      let clock := next.frameClock + next.vx.abs.toFloat
      if clock > 12 then { next with frameClock := 0, frame := if next.frame == 7 then 3 else next.frame + 1 }
      else { next with frameClock := clock }
    else next
  | .lihzahrd =>
    if enemy.vy != 0 then { enemy with frame := 1, frameClock := 0 }
    else
      let next := { enemy with spriteDirection := enemy.direction }
      if next.vx == (0 : Float32) then { next with frame := 0, frameClock := 0 }
      else
        let clock : Float := next.frameClock + (2 * next.vx.abs).toFloat + 1
        let frame := max 2 next.frame
        if clock > 6 then { next with frame := if frame == 15 then 2 else frame + 1, frameClock := 0 }
        else { next with frame, frameClock := clock }

def TempleEnemy.advance (initial : TempleEnemy) (world : World) (water magma : Water)
    (player : Player) (dead daylight : Bool) (surface : Nat) : Option TempleEnemy × World := Id.run do
  let snake := initial.kind == .flyingSnake
  let fallSpeed : Float32 := if initial.wet then 7 else 10
  let size := single world.width / 4200
  let altitude := ((initial.y / 16 - (60 + 10 * (size * size))).toFloat / (surface.toFloat / 6)).toFloat32
  let gravity : Float32 := if initial.wet then 0.2 else 0.3 * max 0.25 (min 1 altitude)
  let step := (1 + initial.vx.abs / 3) * initial.stepSpeed
  let gfx := if initial.gfxOffsetY > 0 then max 0 (initial.gfxOffsetY - step)
    else min 0 (initial.gfxOffsetY + step)
  let enemy := { initial with gfxOffsetY := max (-16) (min 16 gfx) }
  let (enemy, world) := if snake then
      (enemy.advanceSnake world player dead (underworldTargetWet world water magma player), world)
    else enemy.advanceFighter world player dead daylight surface
  let vx := if -0.005 < enemy.vx.toFloat && enemy.vx.toFloat < 0.005 then 0 else enemy.vx
  let vy := if snake then enemy.vy else min fallSpeed (enemy.vy + gravity)
  let moved := moveNpc world water magma enemy.x enemy.y vx vy enemy.kind.width enemy.kind.height
    enemy.wet (!snake && enemy.directionY != 1)
  let active := npcActiveTime player moved.x moved.y enemy.kind.width enemy.kind.height enemy.timeLeft
  return (active.map (fun timeLeft => ({ enemy with
    x := moved.x, y := moved.y, vx := moved.vx, vy := moved.vy, previousX := enemy.x,
    previousVx := moved.oldVx, previousVy := moved.oldVy, collideX := moved.collideX, collideY := moved.collideY,
    timeLeft, wet := moved.wet, justHit := false } : TempleEnemy).animate), world)

/-- Ordinary non-event underground spawning, with the Desktop temple and
population multipliers. One spawn slot is represented by two half slots. -/
def templeSpawnRate (hardmode cavern deep jungle : Bool) (halfSlots : Nat) : Nat × Nat :=
  let base : Nat := if hardmode then 540 else 600
  let maximum : Nat := if hardmode then 6 else 5
  let rate := (base.toFloat * (if cavern then 0.4 else if hardmode then 0.45 else 0.5 : Float)).toUInt64.toNat
  let maximum := ((single maximum) * (if cavern then 1.9 else if hardmode then 1.8 else 1.7)).toUInt32.toNat
  let rate := if jungle then (rate.toFloat * 0.4).toUInt64.toNat else rate
  let maximum := if jungle then ((single maximum) * 1.5).toUInt32.toNat else maximum
  let rate := ((single rate) * 0.8).toUInt32.toNat
  let maximum := ((single maximum) * 1.2).toUInt32.toNat
  let population : Float32 := if halfSlots * 5 < 2 * maximum then 0.6
    else if halfSlots * 5 < 4 * maximum then 0.7
    else if halfSlots * 5 < 6 * maximum then 0.8
    else if halfSlots * 5 < 8 * maximum then 0.9 else 1
  let rate := ((single rate) * population).toUInt32.toNat
  let deeper : Float32 := if !deep then 1 else if halfSlots * 5 < 2 * maximum then 0.7
    else if halfSlots * 5 < 4 * maximum then 0.9 else 1
  (max 60 (((single rate) * deeper).toUInt32.toNat), min 15 maximum * 2)

def templeSpawnKind (sample : Nat) : TempleEnemyKind := if sample % 3 == 0 then .flyingSnake else .lihzahrd

/-- The compact map's search window uses its own viewport dimensions. A
partitioned room can supply a spawn point behind a wall within that window. -/
def findTempleSpawn (world : World) (water magma honey : Water) (player : Player)
    (active : Array TempleEnemy) (id : Nat) (roll : Nat → Nat) : Option TempleEnemy := Id.run do
  let center := player.centerTile
  for attempt in [:50] do
    let x := center.x + (roll (3 * attempt) % 65 : Nat) - 32
    let top := center.y + (roll (3 * attempt + 1) % 41 : Nat) - 20
    if !world.contains ⟨x, top⟩ || (world.tile ⟨x, top⟩).solid then continue
    for row in [top.toNat + 1:min underworldStart (center.y + 21).toNat] do
      let floor : TilePos := ⟨x, row⟩
      let ground := world.tile floor
      if !ground.solid then continue
      if ground == .lihzahrdBrick || ground == .woodenSpikes then
        let kind := templeSpawnKind (roll (3 * attempt + 2))
        let enemy := TempleEnemy.atFloor id kind floor
        let dy := (row - center.y).natAbs
        let dx := (x - center.x).natAbs
        let air : TilePos := ⟨x, row - 1⟩
        if (dx ≥ 24 || dy ≥ 16 || ((dx ≥ 6 || dy ≥ 5) &&
            !npcCanSee world enemy.x enemy.y kind.width kind.height player)) &&
            water.level world air == 0 && magma.level world air == 0 && honey.level world air == 0 &&
            !npcSolidOverlap world enemy.x enemy.y kind.width kind.height &&
            !active.any (·.hitBox.intersects enemy.hitBox) then return some enemy
      break
  return none

def templeEnemyDrops (roll : Nat → Nat) : Array Stack :=
  let egg : Array Stack := if roll 0 % 1000 == 0 then #[⟨.lizardEgg, 1⟩] else #[]
  let cell : Array Stack := if roll 1 % 50 == 0 then #[⟨.lihzahrdPowerCell, 1⟩] else #[]
  let fragments : Array Stack := if roll 2 % 7 == 0 then #[⟨.solarTabletFragment, 1 + roll 3 % 2⟩] else #[]
  egg ++ cell ++ fragments

end Lean4Terria
