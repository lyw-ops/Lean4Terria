import Lean4Terria.BoneSerpent
import Lean4Terria.TwinShots

namespace Lean4Terria

def destroyerBodyCount : Nat := 80
def destroyerSegmentCount : Nat := destroyerBodyCount + 2
def destroyerHealth : Nat := 80000
def destroyerSize : Nat := 47
def destroyerSpacing : Float32 := 55
def destroyerUseTime : Nat := 45
def probeHealth : Nat := 200

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def pixels (value : Int) : Float32 := single value / 16
private def roundTile (value : Float32) (size : Int) : Float32 :=
  single ((value / single size).toInt32.toInt * size)

private def magnitude (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32

private def unitVelocity (dx dy speed : Float32) : Float32 × Float32 :=
  let length := magnitude dx dy
  if length == 0 then (0, 0)
  else
    let scale := speed / length
    (dx * scale, dy * scale)

private def rotationFor (dx dy : Float32) : Float32 :=
  (Float.atan2 dy.toFloat dx.toFloat).toFloat32 + 1.5700000524520874

def destroyerSprite (part : Nat) : Nat :=
  if part == 0 then 134 else if part == destroyerSegmentCount - 1 then 136 else 135

def destroyerBody (part : Nat) : Bool := 0 < part && part < destroyerSegmentCount - 1

def destroyerDefense (part : Nat) : Nat :=
  if part == 0 then 0 else if destroyerBody part then 30 else 35

def destroyerContactDamage (part : Nat) : Nat :=
  if part == 0 then 70 else if destroyerBody part then 55 else 40

structure DestroyerSegment where
  x : Float32
  y : Float32
  rotation : Float32 := 0
  alpha : Nat := 255
  laserClock : Nat := 0
  released : Bool := false
  deriving Repr, Inhabited

instance : BEq DestroyerSegment where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.alpha == b.alpha &&
    a.laserClock == b.laserClock && a.released == b.released

structure Destroyer where
  segments : Array DestroyerSegment
  health : Nat := destroyerHealth
  vx : Float32 := 0
  vy : Float32 := 0
  burrowSoundDelay : Nat := 0
  deriving Repr, Inhabited

instance : BEq Destroyer where
  beq a b := a.segments == b.segments && a.health == b.health &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.burrowSoundDelay == b.burrowSoundDelay

def DestroyerSegment.centerX (part : DestroyerSegment) : Float32 := part.x + 23.5
def DestroyerSegment.centerY (part : DestroyerSegment) : Float32 := part.y + 23.5

def DestroyerSegment.hitBox (part : DestroyerSegment) : HitBox :=
  CasterAI.npcBox part.x part.y destroyerSize destroyerSize

private def validCoordinate (value : Float32) (tiles : Nat) : Bool :=
  value.isFinite && -1200 ≤ value && value ≤ single (tiles * 16 + 1200 : Nat)

def Destroyer.valid (boss : Destroyer) (world : World) : Bool :=
  0 < boss.health && boss.health ≤ destroyerHealth && boss.segments.size == destroyerSegmentCount &&
    boss.vx.isFinite && boss.vy.isFinite && boss.vx.abs ≤ 40 && boss.vy.abs ≤ 40 &&
    boss.burrowSoundDelay ≤ 20 &&
    (boss.segments.zipIdx.all fun (part, index) =>
      validCoordinate part.x world.width && validCoordinate part.y world.height &&
        part.rotation.isFinite && part.rotation.abs ≤ 5 && part.alpha ≤ 255 &&
        (if destroyerBody index then part.laserClock < 26000 else part.laserClock == 0 && !part.released))

def Destroyer.atCenter (x y : Float32) : Destroyer :=
  let part : DestroyerSegment := { x := x - 23.5, y := y - 23.5 }
  { segments := Array.replicate destroyerSegmentCount part }

/-- Spawn from air outside the player's safe rectangle and use the first
solid floor below it. The search rectangle is scaled to the compact world. -/
def Destroyer.spawn (world : World) (player : Player) (roll : Nat → Nat) : Destroyer := Id.run do
  let center := player.centerTile
  let left := max 4 (center.x - 50)
  let right := min ((world.width : Int) - 5) (center.x + 50)
  let top := max 9 (center.y - 31)
  let bottom := min ((world.height : Int) - 3) (center.y + 31)
  for attempt in [:128] do
    let x := left + roll (2 * attempt) % ((right - left).toNat + 1)
    let y := top + roll (2 * attempt + 1) % ((bottom - top).toNat + 1)
    if (x - center.x).natAbs ≤ 24 && (y - center.y).natAbs ≤ 15 then continue
    if (world.tile ⟨x, y⟩).solid then continue
    for floor in [y.toNat:world.height - 1] do
      if (world.tile ⟨x, floor⟩).solid then
        if (x - center.x).natAbs > 24 || ((floor : Int) - center.y).natAbs > 15 then
          return .atCenter (single (x * 16 + 8)) (single (floor * 16 : Nat) - 23.5)
        break
  let x := if center.x < world.width / 2 then center.x + 28 else center.x - 28
  return .atCenter (single (x * 16 + 8)) (single (min ((world.height : Int) - 3) (center.y + 20) * 16))

private def adjust (value wanted step : Float32) : Float32 :=
  if value < wanted then value + step else if value > wanted then value - step else value

private def sameDirection (a b : Float32) : Bool :=
  (a > 0 && b > 0) || (a < 0 && b < 0)

/-- Style 37 first makes a separate stronger adjustment when both axes
already match the destination signs, then runs the normal worm steering. -/
def destroyerBurrowVelocity (vx vy dx dy : Float32) : Float32 × Float32 :=
  let (wantedX, wantedY) := unitVelocity dx dy 16
  let (vx, vy) := if sameDirection vx wantedX && sameDirection vy wantedY then
      (adjust vx wantedX 0.15, adjust vy wantedY 0.15)
    else (vx, vy)
  wormSteer 16 0.1 vx vy dx dy wantedX wantedY

def destroyerFallVelocity (vx vy dx speed : Float32) : Float32 × Float32 :=
  let vy := min speed (vy + 0.15)
  let vx := if vx.abs.toFloat + vy.abs.toFloat < speed.toFloat * 0.4 then
    if vx < 0 then vx - 0.11 else vx + 0.11
  else if vy == speed then
    if vx < dx then vx + 0.1 else if vx > dx then vx - 0.1 else vx
  else if vy > 4 then
    if vx < 0 then vx + 0.09 else vx - 0.09
  else vx
  (vx, vy)

def DestroyerSegment.follow (part previous : DestroyerSegment) : DestroyerSegment :=
  let dx := previous.centerX - part.centerX
  let dy := previous.centerY - part.centerY
  let length := magnitude dx dy
  let step := if length == 0 then 0 else (length - destroyerSpacing) / length
  { part with x := part.x + dx * step, y := part.y + dy * step, rotation := rotationFor dx dy,
              alpha := part.alpha - (if previous.alpha < 128 then 42 else 0) }

private def clearToPlayer (world : World) (x y : Float32) (player : Player) : Bool :=
  let x := (x * 16).toInt32.toInt
  let y := (y * 16).toInt32.toInt
  (terrainHit world x y (bodyCenterX player - x) (bodyCenterY player - y)).isNone

/-- The Classic style-37 firing clock, with independently injected draws
for each segment. A failed line-of-sight check still resets the clock. -/
def DestroyerSegment.advanceLaser (part : DestroyerSegment) (player : Player) (visible : Unit → Bool)
    (roll : Nat → Nat) : DestroyerSegment × Option TwinAttack :=
  let clock := part.laserClock + roll 0 % 4
  if clock < 1400 + roll 1 % 24600 then ({ part with laserClock := clock }, none)
  else
    let noise (draw : Nat) := single ((roll draw % 41 : Nat) - (20 : Int))
    let (vx, vy) := unitVelocity (pixels (bodyCenterX player) - part.centerX + noise 2)
      (pixels (bodyCenterY player) - part.centerY + noise 3) 8
    let vx := vx + noise 4 * 0.05
    let vy := vy + noise 5 * 0.05
    let attack : TwinAttack := ⟨.destroyerLaser, part.centerX + vx * 5, part.centerY + vy * 5, vx, vy, 22⟩
    ({ part with laserClock := 0 }, if visible () then some attack else none)

def Destroyer.advance (boss : Destroyer) (world : World) (water magma honey : Water)
    (player : Player) (surface : Nat) (withdrawing : Bool) (roll : Nat → Nat → Nat) :
    Option (Destroyer × Array TwinAttack) := do
  let head ← boss.segments[0]?
  let belowSurface := head.y > single (surface * 16 : Nat)
  if withdrawing && boss.segments.any (fun part => part.y > single (world.height / 2 * 16 : Nat)) then failure
  let x := roundTile (pixels (bodyCenterX player)) 16
  let y := roundTile (pixels (bodyCenterY player)) 16
  let dx := x - roundTile head.centerX 16
  let dy := y - roundTile head.centerY 16
  let hit := head.hitBox
  let px := player.x.tdiv 16 * 16
  let py := player.y.tdiv 16 * 16
  let far := head.y > pixels player.y &&
    !hit.intersects ⟨px - 16000, py - 16000, px + 16000, py + 16000⟩
  let body : BoneSerpentSegment := { x := head.x, y := head.y }
  let buried := !withdrawing && (far || body.inTerrain world water magma 47 honey)
  let delay := boss.burrowSoundDelay - 1
  let burrowSoundDelay := if buried && delay == 0 then
      (max 10 (min 20 (magnitude dx dy / 40))).toInt32.toInt.toNat else delay
  let (vx, vy) := if buried then destroyerBurrowVelocity boss.vx boss.vy dx dy
    else destroyerFallVelocity boss.vx (boss.vy + if withdrawing then (if belowSurface then 2 else 1) else 0)
      dx (if withdrawing && belowSurface then 32 else 16)
  let vx := if vx.abs < 0.005 then 0 else vx
  let vy := if vy.abs < 0.005 then 0 else vy
  let moved := { head with
    x := head.x + vx
    y := head.y + vy
    alpha := head.alpha - 42
    rotation := rotationFor vx vy }
  if !validCoordinate moved.x world.width || !validCoordinate moved.y world.height then failure
  let (segments, shots) := Id.run do
    let mut segments := #[moved]
    let mut shots := #[]
    for index in [1:boss.segments.size] do
      let part := boss.segments[index]!
      let visible : Unit → Bool := fun _ => clearToPlayer world part.centerX part.centerY player
      let (part, shot) := if destroyerBody index then
        part.advanceLaser player visible (roll index) else (part, none)
      shots := shots ++ shot.toArray
      segments := segments.push (part.follow segments[index - 1]!)
    return (segments, shots)
  return ({ boss with vx, vy, segments, burrowSoundDelay }, shots)

structure DestroyerProbe where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  facing : Int := 1
  health : Nat := probeHealth
  laserClock : Nat := 0
  timeLeft : Nat := 750
  justHit : Bool := false
  deriving Repr, Inhabited

instance : BEq DestroyerProbe where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.facing == b.facing && a.health == b.health && a.laserClock == b.laserClock &&
    a.timeLeft == b.timeLeft && a.justHit == b.justHit

def DestroyerProbe.fromSegment (id : Nat) (part : DestroyerSegment) : DestroyerProbe :=
  { id, x := single (part.centerX.toInt32.toInt - 15), y := single (part.centerY.toInt32.toInt - 30) }

def DestroyerProbe.hitBox (probe : DestroyerProbe) : HitBox := CasterAI.npcBox probe.x probe.y 30 30

def DestroyerProbe.valid (probe : DestroyerProbe) (world : World) : Bool :=
  0 < probe.id && validCoordinate probe.x world.width && validCoordinate probe.y world.height &&
    probe.vx.isFinite && probe.vy.isFinite && probe.vx.abs ≤ 16 && probe.vy.abs ≤ 16 &&
    probe.rotation.isFinite && probe.rotation.abs ≤ 5 && (probe.facing == 1 || probe.facing == -1) &&
    0 < probe.health && probe.health ≤ probeHealth &&
    probe.laserClock < 120 && 0 < probe.timeLeft && probe.timeLeft ≤ 750

def DestroyerProbe.advance (probe : DestroyerProbe) (world : World) (player : Player) (bossPresent dead : Bool) :
    Option DestroyerProbe × Option TwinAttack := Id.run do
  let cx := roundTile (probe.x + 15) 8
  let cy := roundTile (probe.y + 15) 8
  let dx := roundTile (pixels (bodyCenterX player)) 8 - cx
  let dy := roundTile (pixels (bodyCenterY player)) 8 - cy
  let (wantedX, wantedY) := if dx == 0 && dy == 0 then (probe.vx, probe.vy) else unitVelocity dx dy 6
  let vx := adjust probe.vx wantedX 0.05
  let vy := adjust probe.vy wantedY 0.05
  let facing := if wantedX < 0 then -1 else if wantedX > 0 then 1 else probe.facing
  let rotation := if wantedX == 0 then probe.rotation else
    (Float.atan2 wantedY.toFloat wantedX.toFloat).toFloat32 + (if facing < 0 then 3.14 else 0)
  let clock := if probe.justHit then 0 else probe.laserClock + 1
  let reachX := (probe.x + 15).toInt32.toInt - (pixels (bodyCenterX player)).toInt32.toInt
  let reachY := (probe.y + 15).toInt32.toInt - (pixels (bodyCenterY player)).toInt32.toInt
  let shot : Option TwinAttack := if clock ≥ 120 && !dead && -910 ≤ reachX && reachX < 910 &&
      -550 ≤ reachY && reachY < 550 &&
      clearToPlayer world (probe.x + 15) (probe.y + 15) player then
      some ⟨.probeLaser, cx, cy, wantedX, wantedY, 25⟩ else none
  let nearby := (probe.x + 15 - pixels (bodyCenterX player)).abs < 960 &&
    (probe.y + 15 - pixels (bodyCenterY player)).abs < 600
  let timeLeft := if bossPresent || (!dead && nearby) then 750 else probe.timeLeft - 1
  let moved := { probe with
    x := probe.x + vx
    y := probe.y + vy
    vx, vy, rotation, facing, timeLeft
    justHit := false
    laserClock := clock % 120 }
  return (if moved.valid world then some moved else none, shot)

end Lean4Terria
