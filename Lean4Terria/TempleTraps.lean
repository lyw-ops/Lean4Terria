import Lean4Terria.OriginalCollision
import Lean4Terria.Ranged

namespace Lean4Terria.TempleTraps

open OriginalCollision (Vector single)

/-- The style-6 pressure plate (tile 135, frame Y 108) responds to players.
Positions are the old and current top-left corners in native pixels. -/
def plateTriggered (plate : TilePos) (oldPosition current : Vector)
    (width height : Nat) (entityType : Int := 5) : Bool :=
  let left := single (plate.x * 16)
  let top := single (plate.y * 16 + 12)
  let overlaps (pos : Vector) (depth : Float) :=
    pos.x + single width > left && pos.x < left + 16 &&
    pos.y + single height > top && pos.y.toFloat < top.toFloat + depth
  entityType == 5 && overlaps current 4.01 && !overlaps oldPosition 16.01

inductive Kind where
  | dart | flame | spikyBall | spear
  deriving BEq, Repr, Inhabited

def Kind.ofFrameY : Int → Option Kind
  | 18 => some .dart
  | 36 => some .flame
  | 54 => some .spikyBall
  | 72 => some .spear
  | _ => none

def Kind.frameY : Kind → Int
  | .dart => 18
  | .flame => 36
  | .spikyBall => 54
  | .spear => 72

def Kind.cooldown : Kind → Nat
  | .dart | .flame => 200
  | .spikyBall => 300
  | .spear => 90

inductive ShotKind where
  | dart | flameEmitter | spikyBall | spear | flame
  deriving BEq, Repr, Inhabited

def ShotKind.sprite : ShotKind → Nat
  | .dart => 184
  | .spikyBall => 185
  | .spear => 186
  | .flameEmitter => 187
  | .flame => 188

def ShotKind.size : ShotKind → Nat × Nat
  | .dart => (10, 10)
  | .spikyBall => (14, 14)
  | .spear => (10, 14)
  | .flameEmitter | .flame => (6, 6)

def ShotKind.lifetime : ShotKind → Nat
  | .dart | .flame => 3600
  | .spikyBall => 900
  | .spear => 300
  | .flameEmitter => 60

def ShotKind.updates (kind : ShotKind) : Nat :=
  if kind == .flame then 3 else 1

def ShotKind.status : ShotKind → Option (Nat × Nat)
  | .dart => some (20, 900)
  | .flame => some (323, 1200)
  | _ => none

/-- Native projectile position and velocity in pixels. Age and remaining
life count individual updates, including the extra updates of flames. -/
structure Shot where
  kind : ShotKind
  position : Vector
  velocity : Vector
  damage : Nat
  life : Nat
  age : Nat := 0
  rotation : Float32 := 0
  direction : Int := 1
  anchor : Option Vector := none
  retracting : Bool := false
  slot : Option Nat := none
  deriving BEq, Repr

private def Shot.atCenter (kind : ShotKind) (center velocity : Vector) (damage : Nat) : Shot :=
  let (width, height) := kind.size
  { kind
    position := ⟨center.x - single (width / 2 : Nat), center.y - single (height / 2 : Nat)⟩
    velocity, damage, life := kind.lifetime }

def Shot.center (shot : Shot) : Vector :=
  let (width, height) := shot.kind.size
  shot.position + ⟨single (width / 2 : Nat), single (height / 2 : Nat)⟩

/-- Hostile hits in Classic use twice the damage roll. The variance and
defense are applied by the calling combat system. -/
def Shot.playerDamage (shot : Shot) : Nat := 2 * shot.damage

def Shot.hitBox (shot : Shot) : HitBox :=
  let (width, height) := shot.kind.size
  let margin := if shot.kind == .flame then (20 : Int) else 0
  let x := shot.position.x.toInt32.toInt
  let y := shot.position.y.toInt32.toInt
  ⟨(x - margin) * 16, (y - margin) * 16,
   (x + width + margin) * 16, (y + height + margin) * 16⟩

private def dartDirection (frameX : Int) : Int × Int :=
  (if frameX == 0 then -1 else if frameX == 18 then 1 else 0,
   if frameX < 36 then 0 else if frameX < 72 then -1 else 1)

private def spearDirection (frameX : Int) : Int × Int :=
  match frameX / 18 with
  | 0 | 1 => (0, 1)
  | 2 => (0, -1)
  | 3 => (-1, 0)
  | 4 => (1, 0)
  | _ => (0, 0)

private def spikyVelocity (direction : Int) (offset : Int) : Float32 :=
  single (4 * direction) + single offset * 0.05

/-- Fire one modern-frame tile 137 when `Wiring.CheckMech` permits it.
Each supplied roll is already in its requested half-open range. X and Y
take separate calls to the native random stream. -/
def launch (kind : Kind) (trap : TilePos) (frameX : Int) (rollX rollY : Int → Int → Int) : Shot :=
  let useDart := kind == .dart || kind == .flame
  let (dx, dy) := if useDart then dartDirection frameX else spearDirection frameX
  let reach := if useDart then 10 else if kind == .spikyBall then 14 else 18
  let center : Vector := ⟨single (trap.x * 16 + 8 + reach * dx), single (trap.y * 16 + 8 + reach * dy)⟩
  match kind with
  | .dart => Shot.atCenter .dart center ⟨single dx * 12, single dy * 12⟩ 40
  | .flame => Shot.atCenter .flameEmitter center ⟨single dx * 5, single dy * 5⟩ 40
  | .spear => Shot.atCenter .spear center ⟨single dx * 8, single dy * 8⟩ 60
  | .spikyBall =>
    let roll (direction : Int) (sample : Int → Int → Int) :=
      sample (if direction == 1 then 0 else -20) (if direction == -1 then 1 else 21)
    Shot.atCenter .spikyBall center
      ⟨spikyVelocity dx (roll dx rollX), spikyVelocity dy (roll dy rollY)⟩ 40

/-- The game's movement and overlap routines receive native pixel sizes.
The motion callback returns the velocity clipped by world collision. -/
structure Terrain where
  width : Nat
  height : Nat
  clip : Vector → Vector → Nat → Nat → Vector := fun _ velocity _ _ => velocity
  solid : Vector → Nat → Nat → Bool := fun _ _ _ => false

structure ShotStep where
  next : Option Shot := none
  child : Option Shot := none
  hit : Option HitBox := none
  sound : Option Nat := none
  deriving Repr

private def rotationOf (velocity : Vector) : Float32 :=
  (Float.atan2 velocity.y.toFloat velocity.x.toFloat).toFloat32

private def speed (velocity : Vector) : Float32 :=
  (velocity.x * velocity.x + velocity.y * velocity.y).toFloat.sqrt.toFloat32

private def clippedMotion (grid : OriginalCollision.Grid) (origin velocity : Vector)
    (width height : Nat) : Vector := Id.run do
  let options : OriginalCollision.Options := { fallThrough := true, fall2 := true }
  let mut distance := speed velocity
  let maxStep := single (min 16 (max 3 (min width height)) : Nat)
  let displacement := (OriginalCollision.tileCollision grid origin velocity width height options).displacement
  if distance ≤ maxStep then return displacement
  let ratio := 1 / distance
  let direction : Vector := ⟨velocity.x * ratio, if displacement.y == 0 then 0 else velocity.y * ratio⟩
  let mut position := origin
  let mut total : Vector := ⟨0, 0⟩
  for _ in [:300] do
    if distance ≤ 0 then break
    let part := min distance maxStep
    let delta : Vector := ⟨direction.x * part, direction.y * part⟩
    let clipped :=
      (OriginalCollision.tileCollision grid position delta width height { options with hoik := true }).displacement
    position := position + clipped
    total := total + clipped
    distance := distance - part
  let align (actual requested : Float32) :=
    if (actual - requested).abs < 0.0001 then requested else actual
  return ⟨align total.x velocity.x, align total.y velocity.y⟩

private def solidOverlap (grid : OriginalCollision.Grid) (position : Vector) (width height : Nat) : Bool := Id.run do
  let tileIndex := OriginalCollision.tileIndex
  let right := position.x + single width
  let bottom := position.y + single height
  let minX := (tileIndex position.x - 1).toNat
  let maxX := min (grid.width - 1) (tileIndex right + 2).toNat
  let minY := (tileIndex position.y - 1).toNat
  let maxY := min (grid.height - 40) (tileIndex bottom + 2).toNat
  for x in [minX:maxX] do
    for y in [minY:maxY] do
      let tile := grid.tileAt x y
      let some id := tile.id | continue
      let rules := grid.rules id
      if tile.actuated || !rules.solid || rules.solidTop then continue
      let left := single (x * 16 : Nat)
      let top := single ((y * 16 : Nat) + if tile.shape == .halfBlock then 8 else 0)
      let lower := single ((y * 16 : Nat) + 16)
      if right > left && position.x < left + 16 && bottom > top && position.y < lower then return true
  return false

/-- Dry projectile collisions with the full and half blocks used by
the compact simulation. -/
def Terrain.ofGrid (grid : OriginalCollision.Grid) : Terrain :=
  { width := grid.width, height := grid.height
    clip := clippedMotion grid, solid := solidOverlap grid }

def Terrain.ofWorld (world : World) : Terrain :=
  Terrain.ofGrid
    { width := world.width, height := world.height
      tileAt := fun x y =>
        let tile := world.tile ⟨x, y⟩
        if tile == .platform then { id := some 19, frame := some (0, 0) }
        else if tile.solid then { id := some 1 }
        else {}
      rules := fun id =>
        if id == 19 then { solid := true, solidTop := true, platform := true }
        else { solid := id == 1 } }

private def Shot.inBounds (shot : Shot) (terrain : Terrain) : Bool :=
  let (width, height) := shot.kind.size
  0 < shot.position.x && 0 < shot.position.y &&
    shot.position.x + single width < single (terrain.width * 16 : Nat) &&
    shot.position.y + single height < single (terrain.height * 16 : Nat)

private def Shot.finish (shot : Shot) (canHit : Bool := true)
    (child : Option Shot := none) (sound : Option Nat := none) : ShotStep :=
  let next := { shot with life := shot.life - 1, age := shot.age + 1 }
  { next := if next.life == 0 then none else some next
    child, hit := if canHit then some next.hitBox else none, sound }

private def Shot.spearStep (shot : Shot) (terrain : Terrain) : ShotStep :=
  let anchor := shot.anchor.getD
    ⟨shot.center.x - shot.velocity.x * 1.5, shot.center.y - shot.velocity.y * 1.5⟩
  let difference : Vector := ⟨shot.center.x - anchor.x, shot.center.y - anchor.y⟩
  let solid := terrain.solid shot.position 10 14
  let distance := speed difference
  if shot.retracting && (solid || distance < speed shot.velocity) then {}
  else
    let retracting := shot.retracting || solid || distance > 300
    let velocity : Vector :=
      if retracting && !shot.retracting then ⟨shot.velocity.x * (-1), shot.velocity.y * (-1)⟩ else shot.velocity
    let rotation := rotationOf difference - 1.5707963705062866
    { shot with
      anchor := some anchor, rotation, retracting
      direction := if velocity.x < 0 then -1 else 1
      position := shot.position + velocity, velocity }.finish

private def Shot.flameEmitterStep (shot : Shot) : ShotStep :=
  let shot := { shot with direction := if shot.velocity.x < 0 then -1 else 1 }
  if (shot.age + 1) % 6 == 0 then
    shot.finish false (some (Shot.atCenter .flame shot.position shot.velocity shot.damage)) (some 34)
  else shot.finish false

private def Shot.motionStep (shot : Shot) (terrain : Terrain) : ShotStep :=
  let (width, height) := shot.kind.size
  let spiky := shot.kind == .spikyBall
  let flame := shot.kind == .flame
  let gravity := spiky && shot.age ≥ 5
  let vx := if gravity && shot.velocity.y == 0 && shot.velocity.x != 0 then
    let slowed := shot.velocity.x * 0.97
    if -0.01 < slowed.toFloat && slowed.toFloat < 0.01 then 0 else slowed
    else shot.velocity.x
  let vy := if gravity then min 16 (shot.velocity.y + 0.2) else shot.velocity.y
  let velocity : Vector := ⟨vx, vy⟩
  let rotation := if spiky then shot.rotation + vx * 0.1
    else if flame then shot.rotation + single shot.direction * 0.3 else rotationOf velocity + 1.57
  let clipped := terrain.clip shot.position velocity width height
  let velocity : Vector :=
    if spiky then
      ⟨if clipped.x != vx then vx * (-0.9) else clipped.x,
       if clipped.y != vy && vy > 1 then vy * (-0.9) else clipped.y⟩
    else clipped
  let sound := if !spiky && !flame && shot.age == 0 then some 17 else none
  if !spiky && (clipped.x != vx || clipped.y != vy) then { sound }
  else
    { shot with
      position := shot.position + velocity, velocity, rotation
      life := if flame then min 60 shot.life else shot.life
      direction := if velocity.x < 0 then -1 else 1 }.finish (!flame || shot.age + 1 ≥ 5) (sound := sound)

/-- Advance one dry projectile update. Use `ShotKind.updates` times per
game tick for already active shot slots. Newly spawned flame slots follow
the game's normal slot iteration order. -/
def Shot.step (shot : Shot) (terrain : Terrain) : ShotStep :=
  if shot.life == 0 || !shot.inBounds terrain then {}
  else match shot.kind with
    | .spear => shot.spearStep terrain
    | .flameEmitter => shot.flameEmitterStep
    | .dart | .spikyBall | .flame => shot.motionStep terrain

end Lean4Terria.TempleTraps
