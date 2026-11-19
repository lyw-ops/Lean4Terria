import Lean4Terria.Magma
import Lean4Terria.OriginalCollision
import Lean4Terria.Ranged

namespace Lean4Terria

inductive PlanteraShotKind where
  | seed | poisonSeed | thornBall
  deriving BEq, Repr, Inhabited

def PlanteraShotKind.sprite : PlanteraShotKind → Nat
  | .seed => 275
  | .poisonSeed => 276
  | .thornBall => 277

def PlanteraShotKind.size : PlanteraShotKind → Nat
  | .seed | .poisonSeed => 14
  | .thornBall => 38

def PlanteraShotKind.updates (_ : PlanteraShotKind) : Nat := 1

def PlanteraShotKind.lifetime : PlanteraShotKind → Nat
  | .seed | .poisonSeed => 300
  | .thornBall => 3600

def PlanteraShotKind.baseDamage : PlanteraShotKind → Nat
  | .seed => 22
  | .poisonSeed => 27
  | .thornBall => 31

/-- Classic `Projectile.StatusPlayer`, sampled after a successful hit. -/
def PlanteraShotKind.poisonTicks (kind : PlanteraShotKind) (roll : Nat → Nat) : Nat :=
  if kind == .poisonSeed && roll 0 % 2 == 0 then 180 + roll 1 % 240 else 0

private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32

private def PlanteraShotKind.halfSize (kind : PlanteraShotKind) : Float32 :=
  single (kind.size / 2 : Nat)

/-- Position is the native projectile constructor's center in pixels. -/
structure PlanteraAttack where
  kind : PlanteraShotKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  deriving Repr, Inhabited

/-- Native top-left position in pixels, with age and life in updates. -/
structure PlanteraShot where
  kind : PlanteraShotKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  damage : Nat
  life : Nat
  rotation : Float32 := 0
  age : Nat := 0
  slot : Option Nat := none
  honeyWet : Bool := false
  deriving Repr

instance : BEq PlanteraShot where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.damage == b.damage && a.life == b.life && a.age == b.age && a.slot == b.slot && a.honeyWet == b.honeyWet

def PlanteraShot.centerX (shot : PlanteraShot) : Float32 := shot.x + shot.kind.halfSize

def PlanteraShot.centerY (shot : PlanteraShot) : Float32 := shot.y + shot.kind.halfSize

def PlanteraShot.frame (shot : PlanteraShot) : Nat :=
  if shot.kind == .thornBall then 0 else shot.age / 2 % 2

def PlanteraShot.alpha (shot : PlanteraShot) : Nat := 255 - 30 * shot.age

def PlanteraShot.fire (attack : PlanteraAttack) (slot : Nat) : PlanteraShot :=
  { kind := attack.kind, x := attack.x - attack.kind.halfSize, y := attack.y - attack.kind.halfSize
    vx := attack.vx, vy := attack.vy, damage := attack.damage, life := attack.kind.lifetime, slot := some slot }

private def PlanteraShot.inWorld (shot : PlanteraShot) (world : World) : Bool :=
  shot.x.isFinite && shot.y.isFinite && 0 < shot.x && 0 < shot.y &&
    shot.x + single shot.kind.size < single (world.width * 16 : Nat) &&
    shot.y + single shot.kind.size < single (world.height * 16 : Nat)

private def PlanteraShot.withinUpdateBounds (shot : PlanteraShot) (world : World) : Bool :=
  if shot.age == 0 then shot.inWorld world
  else
    let extent := single shot.kind.size
    let verticalMargin : Float32 := if shot.kind == .thornBall then 768 else 16
    shot.x.isFinite && shot.y.isFinite &&
      -16 ≤ shot.x && shot.x + extent ≤ single (world.width * 16 : Nat) + 16 &&
      -verticalMargin ≤ shot.y && shot.y + extent ≤ single (world.height * 16 : Nat) + verticalMargin

def PlanteraShot.valid (shot : PlanteraShot) (world : World) : Bool :=
  shot.withinUpdateBounds world && shot.vx.isFinite && shot.vy.isFinite && shot.rotation.isFinite &&
    shot.vx.abs ≤ 16 && shot.vy.abs ≤ (if shot.kind == .thornBall then 768 else 16) &&
    shot.life > 0 && shot.age + shot.life == shot.kind.lifetime &&
    shot.slot.any (· < maxProjectileSlots) &&
    (shot.damage == shot.kind.baseDamage || shot.damage == 2 * shot.kind.baseDamage)

def PlanteraShot.hitBox (shot : PlanteraShot) : HitBox :=
  let left := shot.x.toInt32.toInt * 16
  let top := shot.y.toInt32.toInt * 16
  let extent := (shot.kind.size : Int) * 16
  ⟨left, top, left + extent, top + extent⟩

structure PlanteraShotStep where
  next : Option PlanteraShot := none
  hit : Option HitBox := none
  damage : Nat := 0
  launchSound : Bool := false
  impactSound : Bool := false
  deriving Repr

private def PlanteraShot.wetContact (shot : PlanteraShot) (world : World) (water magma honey : Water) :
    Option Bool := Id.run do
  let height := shot.kind.size / 2
  let left := shot.centerX - 5
  let top := shot.centerY - single (height / 2 : Nat)
  let right := left + 10
  let bottom := top + single height
  let firstX := OriginalCollision.tileIndex left
  let firstY := OriginalCollision.tileIndex top - 1
  for column in [:(OriginalCollision.tileIndex right - firstX + 1).toNat] do
    for row in [:(OriginalCollision.tileIndex bottom - firstY + 1).toNat] do
      let pos : TilePos := ⟨firstX + column, firstY + row⟩
      let honeyLevel := honey.level world pos
      let level := max honeyLevel (max (water.level world pos) (magma.level world pos))
      if level == 0 then continue
      let tx := single (pos.x * 16)
      let offset := single (256 - level : Nat) / 16
      let surface := single (pos.y * 16) + offset
      let depth := 16 - single offset.toInt32.toInt
      if right > tx && left < tx + 16 && bottom > surface && top < surface + depth then
        return some (honeyLevel > 0)
  return none

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

private def motionLength (velocity : OriginalCollision.Vector) : Float32 :=
  (velocity.x * velocity.x + velocity.y * velocity.y).toFloat.sqrt.toFloat32

/-- Dry projectiles step at most one collider width or 16 pixels at a time.
Water and honey take the native single tile collision path. -/
private def clippedMotion (grid : OriginalCollision.Grid) (origin velocity : OriginalCollision.Vector)
    (size : Nat) (wet : Bool) : OriginalCollision.Vector := Id.run do
  let options : OriginalCollision.Options := { fallThrough := true, fall2 := true }
  let maxStep := single (min 16 size : Nat)
  let distance := motionLength velocity
  let clipped := (OriginalCollision.tileCollision grid origin velocity size size options).displacement
  if wet || distance ≤ maxStep then return clipped
  let ratio := 1 / distance
  let direction : OriginalCollision.Vector :=
    ⟨velocity.x * ratio, if clipped.y == 0 then 0 else velocity.y * ratio⟩
  let mut start := origin
  let mut remaining := distance
  let mut total : OriginalCollision.Vector := ⟨0, 0⟩
  for _ in [:300] do
    if remaining ≤ 0 then break
    let step := min maxStep remaining
    let delta : OriginalCollision.Vector := ⟨direction.x * step, direction.y * step⟩
    let part := (OriginalCollision.tileCollision grid start delta size size { options with hoik := true }).displacement
    start := start + part
    remaining := remaining - step
    total := total + part
  let align (actual requested : Float32) :=
    if (actual - requested).abs < 0.0001 then requested else actual
  return ⟨align total.x velocity.x, align total.y velocity.y⟩

/-- One Classic projectile update. The thorn ball's terrain collider is
smaller than its damage rectangle. A shot can damage on its last update. -/
def PlanteraShot.advanceSubstep (shot : PlanteraShot) (world : World) (water magma honey : Water) :
    PlanteraShotStep := Id.run do
  if shot.life == 0 || !shot.inWorld world then return {}
  let thorn := shot.kind == .thornBall
  let launchSound := !thorn && shot.age == 0
  let age := shot.age + 1
  let slowing := thorn && age > 15 && shot.vy == 0 && shot.vx != 0
  let vx := if slowing then shot.vx * 0.97 else shot.vx
  if slowing && -0.01 < vx.toFloat && vx.toFloat < 0.01 then return {}
  let vy := if thorn then shot.vy + (if age > 15 then 0.2 else 0)
    else shot.vy + (if age ≥ 35 then 0.025 else 0)
  let rotation := if thorn then shot.rotation + vx * 0.05
    else (Float.atan2 vy.toFloat vx.toFloat).toFloat32 + 1.57
  let vy := if thorn then vy else min 16 vy
  let wetContact := shot.wetContact world water magma honey
  let honeyWet := wetContact.any (fun inHoney => inHoney || shot.honeyWet)
  let scale : Float32 := if honeyWet then 0.25 else if wetContact.isSome then 0.5 else 1
  let offset := if thorn then 6 else 0
  let size := if thorn then 26 else 14
  let collision := clippedMotion (terrainGrid world) ⟨shot.x + offset, shot.y + offset⟩
    ⟨vx, vy⟩ size wetContact.isSome
  let blockedX := collision.x != vx
  let blockedY := collision.y != vy
  if !thorn && (blockedX || blockedY) then return { launchSound, impactSound := true }
  let nextVx := if blockedX then vx * (-0.9) else collision.x
  let nextVy := if blockedY && vy > 3 then vy * (-0.9) else collision.y
  let dx := if scale == 1 then nextVx else if blockedX then collision.x else collision.x * scale
  let dy := if scale == 1 then nextVy else if blockedY then collision.y else collision.y * scale
  let moved := { shot with
    x := shot.x + dx, y := shot.y + dy, vx := nextVx, vy := nextVy, rotation
    age, life := shot.life - 1, honeyWet }
  let next := if moved.life > 0 then some moved else none
  return {
    next, hit := some moved.hitBox, damage := shot.damage, launchSound
    impactSound := !thorn && moved.life == 0 }

end Lean4Terria
