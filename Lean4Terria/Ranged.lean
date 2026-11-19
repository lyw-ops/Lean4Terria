import Lean4Terria.Combat

namespace Lean4Terria

def maxProjectileSlots : Nat := 1000

structure WorldAim where
  x : Int
  y : Int
  deriving BEq, Repr

def WorldAim.atTile (tile : TilePos) : WorldAim :=
  ⟨tile.x * tileUnits + tileUnits / 2, tile.y * tileUnits + tileUnits / 2⟩

def WorldAim.valid (world : World) (aim : WorldAim) : Bool :=
  world.contains ⟨aim.x / tileUnits, aim.y / tileUnits⟩

inductive ShotKind where
  | arrow | stormArrow | sentry | herald | raidBolt | hunterArrow | ruinWave | highlandBolt | cavernBeam | guideArrow
  | woodenArrow | unholyArrow | musketBullet | undertakerBullet | thrownBone | handgunBullet | hellfireArrow
  deriving BEq, Repr

def ShotKind.hostile : ShotKind → Bool
  | .sentry | .herald | .raidBolt | .ruinWave | .cavernBeam => true
  | .arrow | .stormArrow | .hunterArrow | .highlandBolt | .guideArrow | .woodenArrow | .unholyArrow
  | .musketBullet | .undertakerBullet | .thrownBone | .handgunBullet | .hellfireArrow => false

def ShotKind.damage : ShotKind → Nat
  | .arrow | .raidBolt => 2
  | .stormArrow => 4
  | .hunterArrow | .ruinWave | .cavernBeam => 3
  | .sentry | .herald => 1
  | .highlandBolt => 6
  | .guideArrow => 8
  | .woodenArrow => 9
  | .unholyArrow => 16
  | .hellfireArrow => 17
  | .musketBullet => 38
  | .undertakerBullet => 26
  | .handgunBullet => 33
  | .thrownBone => 20

def ShotKind.speed : ShotKind → Int
  | .arrow | .guideArrow | .woodenArrow => 220
  | .unholyArrow => 226
  | .hellfireArrow => 324
  | .stormArrow => 300
  | .hunterArrow => 260
  | .sentry => 190
  | .herald => 235
  | .raidBolt => 176
  | .ruinWave => 224
  | .highlandBolt => 360
  | .cavernBeam => 256
  | .musketBullet => 416
  | .undertakerBullet => 320
  | .handgunBullet => 224
  | .thrownBone => 128

def ShotKind.lifetime : ShotKind → Nat
  | .arrow | .guideArrow | .woodenArrow => 96
  | .unholyArrow => 96
  | .hunterArrow => 100
  | .stormArrow | .herald => 108
  | .sentry | .raidBolt => 88
  | .ruinWave => 90
  | .highlandBolt => 76
  | .musketBullet | .undertakerBullet => 90
  | .handgunBullet => 600
  | .hellfireArrow => 3600
  | .cavernBeam => 90
  | .thrownBone => 3600

def ShotKind.isBullet : ShotKind → Bool
  | .musketBullet | .undertakerBullet | .handgunBullet => true
  | _ => false

def ShotKind.gravity (kind : ShotKind) : Int :=
  if kind.hostile || kind == .highlandBolt || kind.isBullet || kind == .thrownBone || kind == .hellfireArrow then 0
  else 2

def ShotKind.hitLimit (kind : ShotKind) : Nat := if kind == .unholyArrow then 5 else 1

def ShotKind.maxDamageBonus (kind : ShotKind) : Nat :=
  if kind == .hellfireArrow then 18
  else if kind == .woodenArrow || kind == .unholyArrow then 15
  else if kind.isBullet then 2 else 0

structure HitBox where
  left : Int
  top : Int
  right : Int
  bottom : Int
  deriving BEq, Repr, Inhabited

def HitBox.ofPlayer (player : Player) : HitBox :=
  ⟨player.x, player.y, player.x + playerWidth, player.y + playerHeight⟩

def HitBox.intersects (a b : HitBox) : Bool :=
  a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom

/-- NPC contact and inactivity checks use integer pixel rectangles. -/
def HitBox.ofNpcTarget (player : Player) : HitBox :=
  let pixel := tileUnits / 16
  let left := player.x.tdiv pixel * pixel
  let top := player.y.tdiv pixel * pixel
  ⟨left, top, left + playerWidth / pixel * pixel, top + playerHeight / pixel * pixel⟩

def HitBox.overlapsPlayer (box : HitBox) (player : Player) : Bool :=
  box.intersects (HitBox.ofPlayer player)

structure Projectile where
  x : Int
  y : Int
  vx : Int
  vy : Int
  life : Nat
  kind : ShotKind
  damageBonus : Nat := 0
  hitsLeft : Nat := 1
  slot : Option Nat := none
  deriving BEq, Repr

def Projectile.damage (shot : Projectile) : Nat :=
  (List.range (shot.kind.hitLimit - shot.hitsLeft)).foldl (fun damage _ => damage * 95 / 100)
    (shot.kind.damage + shot.damageBonus)

def bodyCenterX (body : Player) : Int := body.x + playerWidth / 2
def bodyCenterY (body : Player) : Int := body.y + playerHeight / 2

/-- Normalize an aim vector with fixed-point integer arithmetic. -/
def aimVelocity (speed dx dy : Int) : Option (Int × Int) :=
  if (dx == 0 && dy == 0) || speed ≤ 0 then none else
    let precision : Nat := 1024
    let square := (dx.natAbs ^ 2 + dy.natAbs ^ 2) * precision ^ 2
    let root := Nat.sqrt square
    let magnitude := if root * root == square then root else root + 1
    let component (delta : Int) : Int :=
      let units : Int := delta.natAbs * speed.toNat * precision / magnitude
      if delta < 0 then -units else units
    some (component dx, component dy)

def aimShot (kind : ShotKind) (x y targetX targetY : Int) : Option Projectile := do
  let (vx, vy) ← aimVelocity kind.speed (targetX - x) (targetY - y)
  return { x, y, vx, vy, life := kind.lifetime, kind, hitsLeft := kind.hitLimit }

def Projectile.valid (shot : Projectile) (world : World) : Bool :=
  let speed := shot.kind.speed
  let age : Int := shot.kind.lifetime - shot.life
  let initialVy := shot.vy - shot.kind.gravity * age
  let square := shot.vx * shot.vx + initialVy * initialVy
  let validVelocity := if shot.kind == .hellfireArrow then
      shot.vx.natAbs ≤ 340 && -340 ≤ shot.vy && shot.vy ≤ 256
    else if shot.kind == .thrownBone && age ≥ 20 then
      shot.vx.natAbs ≤ 124 && -128 ≤ shot.vy && shot.vy ≤ 512
    else (speed - 2) * (speed - 2) ≤ square && square ≤ speed * speed
  shot.life > 0 && shot.life ≤ shot.kind.lifetime &&
    shot.hitsLeft > 0 && shot.hitsLeft ≤ shot.kind.hitLimit && shot.damageBonus ≤ shot.kind.maxDamageBonus &&
    validVelocity &&
    ((shot.kind != .ruinWave && shot.kind != .cavernBeam) ||
      (shot.vy == 0 && shot.vx.natAbs == speed.toNat)) &&
    !(world.tile ⟨shot.x / tileUnits, shot.y / tileUnits⟩).solid

private structure HitTime where
  numerator : Int
  denominator : Int

private def HitTime.le (a b : HitTime) : Bool :=
  a.numerator * b.denominator ≤ b.numerator * a.denominator

private structure Segment where
  x : Int
  y : Int
  dx : Int
  dy : Int

private structure TerrainContact where
  tile : TilePos
  time : HitTime

/-- Intersect the segment's time interval with the interval for one box axis. -/
private def clipAxis (point delta lower upper : Int) (enter exit : HitTime) :
    Option (HitTime × HitTime) :=
  if delta == 0 then
    if lower ≤ point && point ≤ upper then some (enter, exit) else none
  else
    let speed : Int := delta.natAbs
    let near : HitTime := ⟨if delta > 0 then lower - point else point - upper, speed⟩
    let far : HitTime := ⟨if delta > 0 then upper - point else point - lower, speed⟩
    let enter := if enter.le near then near else enter
    let exit := if far.le exit then far else exit
    if enter.le exit then some (enter, exit) else none

private def Segment.boxHit (path : Segment) (left top right bottom : Int) : Option HitTime := do
  let (enter, exit) ← clipAxis path.x path.dx left right ⟨0, 1⟩ ⟨1, 1⟩
  let (enter, _) ← clipAxis path.y path.dy top bottom enter exit
  return enter

/-- Sweep only the grid cells in the segment's bounding box. The one-cell rim
acts as bedrock and catches shots leaving the finite world. -/
private def Segment.solidHit (path : Segment) (world : World) : Option TerrainContact := Id.run do
  let endX := path.x + path.dx
  let endY := path.y + path.dy
  let left := max (-1) ((min path.x endX - 1) / tileUnits)
  let top := max (-1) ((min path.y endY - 1) / tileUnits)
  let right := min (world.width : Int) (max path.x endX / tileUnits)
  let bottom := min (world.height : Int) (max path.y endY / tileUnits)
  let mut earliest : Option TerrainContact := none
  for row in [:(bottom - top + 1).toNat] do
    let y := top + row
    for col in [:(right - left + 1).toNat] do
      let x := left + col
      if (world.tile ⟨x, y⟩).solid then
        if let some time := path.boxHit (x * tileUnits) (y * tileUnits)
            ((x + 1) * tileUnits) ((y + 1) * tileUnits) then
          if !(earliest.any (fun hit => hit.time.le time)) then
            earliest := some { tile := ⟨x, y⟩, time }
  return earliest

structure TerrainHit where
  tile : TilePos
  x : Int
  y : Int
  deriving BEq, Repr

/-- First terrain contact on a displacement. The struck edge stays exact;
the other coordinate is rounded down to an integer subpixel on that edge. -/
def terrainHit (world : World) (x y dx dy : Int) : Option TerrainHit := do
  let hit ← (⟨x, y, dx, dy⟩ : Segment).solidHit world
  let point (start delta : Int) := start + delta * hit.time.numerator / hit.time.denominator
  return { tile := hit.tile, x := point x dx, y := point y dy }

/-- The segment may end exactly on terrain when attaching a rope, but no
solid can intersect any earlier part of the line. -/
def terrainLineClear (world : World) (x y endX endY : Int) (allowEndpoint := false) : Bool :=
  match (⟨x, y, endX - x, endY - y⟩ : Segment).solidHit world with
  | none => true
  | some hit => allowEndpoint && hit.time.numerator == hit.time.denominator

/-- Sweep a square projectile against full tiles. Contact on an edge without
overlap is allowed, and one-way platforms do not stop the projectile. -/
def squareProjectileBlocked (world : World) (x y endX endY radius : Int) : Bool := Id.run do
  let path : Segment := ⟨x, y, endX - x, endY - y⟩
  let inset := max 0 (radius - 1)
  let left := (min x endX - inset) / tileUnits
  let top := (min y endY - inset) / tileUnits
  let right := (max x endX + inset) / tileUnits
  let bottom := (max y endY + inset) / tileUnits
  for row in [:(bottom - top + 1).toNat] do
    for column in [:(right - left + 1).toNat] do
      let tx := left + column
      let ty := top + row
      if (world.tile ⟨tx, ty⟩).solid &&
          (path.boxHit (tx * tileUnits - inset) (ty * tileUnits - inset)
            ((tx + 1) * tileUnits + inset) ((ty + 1) * tileUnits + inset)).isSome then
        return true
  return false

def Projectile.unholyHitBox (shot : Projectile) : HitBox :=
  let left := (shot.x - 80).tdiv 16 * 16
  let top := (shot.y - 80).tdiv 16 * 16
  ⟨left, top, left + 160, top + 160⟩

def Projectile.handgunHitBox (shot : Projectile) : HitBox :=
  let left := (shot.x - 32).tdiv 16 * 16
  let top := (shot.y - 32).tdiv 16 * 16
  ⟨left, top, left + 64, top + 64⟩

/-- Musket Ball, type 14, performs two updates per tick. Each moves its
four-pixel square before checking NPC contact and spending one update of life. -/
def Projectile.advanceHandgun (shot : Projectile) (world : World) (wet : Bool) : Option Projectile := do
  if shot.life == 0 then failure
  let fullX := shot.x + shot.vx
  let fullY := shot.y + shot.vy
  if squareProjectileBlocked world shot.x shot.y fullX fullY 32 then failure
  let x := if wet then shot.x + shot.vx.tdiv 2 else fullX
  let y := if wet then shot.y + shot.vy.tdiv 2 else fullY
  return { shot with x, y, life := shot.life - 1 }

/-- Move the ten-pixel-wide arrow before scanning the targets it overlaps. -/
def advanceUnholyArrow (world : World) (shot : Projectile) : Option Projectile := do
  if shot.life == 0 then failure
  let x := shot.x + shot.vx
  let y := shot.y + shot.vy
  if squareProjectileBlocked world shot.x shot.y x y 80 then failure
  return { shot with x, y, vy := shot.vy + ShotKind.unholyArrow.gravity, life := shot.life - 1 }

/-- Resolve the earliest solid tile or target on the whole tick's path. A solid
wins a tie, and equal target times keep the first array index. -/
def advanceProjectileBoxes (world : World) (shot : Projectile) (targets : Array HitBox) :
    Option Projectile × Option Nat := Id.run do
  if shot.life == 0 || (world.tile ⟨shot.x / tileUnits, shot.y / tileUnits⟩).solid then
    return (none, none)
  -- Style 2 starts damping and falling on update 20, before movement.
  let age := shot.kind.lifetime - shot.life + 1
  let boneFall := shot.kind == .thrownBone && age ≥ 20
  let vx := if boneFall then (shot.vx * 97).tdiv 100 else shot.vx
  -- 0.4 pixels is 6.4 subpixels. Carry the fractional gravity by age.
  let fallTicks : Int := age - 19
  let boneGravity := (32 * fallTicks) / 5 - (32 * (fallTicks - 1)) / 5
  let vy := if boneFall then min 512 (shot.vy + boneGravity) else shot.vy
  let radius := if shot.kind == .thrownBone then 19 * 8 else 0
  let path : Segment := ⟨shot.x, shot.y, vx, vy⟩
  let wall := path.solidHit world
  let mut hit : Option (HitTime × Nat) := none
  for index in [:targets.size] do
    let target := targets[index]!
    if let some time := path.boxHit (target.left - radius) (target.top - radius)
        (target.right + radius) (target.bottom + radius) then
      if !(wall.any (fun terrain => terrain.time.le time)) && !(hit.any (fun previous => previous.1.le time)) then
        hit := some (time, index)
  let index := hit.map Prod.snd
  let hitsLeft := shot.hitsLeft - if index.isSome then 1 else 0
  if hitsLeft == 0 || wall.isSome || shot.life == 1 then return (none, index)
  return (some { shot with
    x := shot.x + vx, y := shot.y + vy, vx
    vy := vy + shot.kind.gravity, life := shot.life - 1, hitsLeft }, index)

def advanceProjectile (world : World) (shot : Projectile) (targets : Array Player) :
    Option Projectile × Option Nat :=
  advanceProjectileBoxes world shot (targets.map HitBox.ofPlayer)

def sentryMaxHealth : Nat := 6
def sentryWindupTicks : Nat := 36
def sentryAimLimit : Nat := 110

structure Sentry where
  body : Player
  health : Nat
  aim : Nat
  deriving BEq, Repr

structure SentryState where
  creature : Option Sentry := none
  cleared : Bool := false
  deriving BEq, Repr, Inhabited

def inSentryRange (position : TilePos) : Bool :=
  (position.x - sentrySpawnColumn).natAbs ≤ 3 &&
    (position.y + 1 - sentryFloor).natAbs ≤ 4

def initialSentry (world : World) : SentryState :=
  let creature := (findStandingNear world sentrySpawnColumn sentryFloor 3 4
    (inSentryRange ·.centerTile)).map fun body => { body, health := sentryMaxHealth, aim := 0 }
  { creature }

/-- Begin aiming only with a clear lane. The charged shot uses the player's
current position, so a wall placed during the windup can still intercept it. -/
def advanceSentry (world : World) (player : Player) (state : SentryState) :
    SentryState × Option Projectile := Id.run do
  let some sentry := state.creature | return (state, none)
  if state.cleared then return (state, none)
  let x := bodyCenterX sentry.body
  let y := bodyCenterY sentry.body
  let targetX := bodyCenterX player
  let targetY := bodyCenterY player
  let dx := targetX - x
  let dy := targetY - y
  let inRange := (dx != 0 || dy != 0) &&
    dx.natAbs ≤ 18 * tileUnits.toNat && dy.natAbs ≤ 8 * tileUnits.toNat
  let (aim, shot) :=
    if sentry.aim > sentryWindupTicks then
      (if sentry.aim == sentryWindupTicks + 1 then 0 else sentry.aim - 1, none)
    else if !inRange then (0, none)
    else if sentry.aim == 0 then
      let sight : Segment := ⟨x, y, dx, dy⟩
      (if (sight.solidHit world).isNone then sentryWindupTicks else 0, none)
    else if sentry.aim == 1 then
      (sentryAimLimit, aimShot .sentry x y targetX targetY)
    else (sentry.aim - 1, none)
  return ({ state with creature := some { sentry with aim } }, shot)

end Lean4Terria
