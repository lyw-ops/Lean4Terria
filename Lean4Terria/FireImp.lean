import Lean4Terria.DarkCaster

namespace Lean4Terria

def maxFireImps : Nat := 4
def maxBurningSpheres : Nat := 200
def fireImpWidth : Nat := 18
def fireImpHeight : Nat := 40
def fireImpHealth : Nat := 70
def fireImpDefense : Nat := 16
def fireImpDamage : Nat := 30
def fireImpCoins : Nat := 350
def burningSphereWidth : Nat := 16
def burningSphereHeight : Nat := 16
def burningSphereDamage : Nat := 30
def burningSphereFireTicks : Nat := 420

structure FireImp where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := fireImpHealth
  timeLeft : Nat := 937
  clock : Nat := 0
  casting : Nat := 0
  destination : Option TilePos := none
  direction : Int := 1
  spriteDirection : Int := -1
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq FireImp where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.health == b.health &&
    a.timeLeft == b.timeLeft && a.clock == b.clock && a.casting == b.casting &&
    a.destination == b.destination && a.direction == b.direction && a.spriteDirection == b.spriteDirection &&
    a.frame == b.frame && a.frameClock == b.frameClock

structure BurningSphere where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq BurningSphere where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.timeLeft == b.timeLeft

private def f32 (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def trunc (x : Float32) : Int := x.toInt32.toInt
private def pixel (units : Int) : Float32 := f32 units / 16

def FireImp.centerX (imp : FireImp) : Float32 := imp.x + 9
def FireImp.centerY (imp : FireImp) : Float32 := imp.y + 20
def BurningSphere.centerX (sphere : BurningSphere) : Float32 := sphere.x + 8
def BurningSphere.centerY (sphere : BurningSphere) : Float32 := sphere.y + 8

def FireImp.hitBox (imp : FireImp) : HitBox :=
  CasterAI.npcBox imp.x imp.y fireImpWidth fireImpHeight

def BurningSphere.hitBox (sphere : BurningSphere) : HitBox :=
  CasterAI.npcBox sphere.x sphere.y burningSphereWidth burningSphereHeight

def FireImp.contact (imp : FireImp) (player : Player) : Option Int :=
  CasterAI.contact imp.hitBox imp.centerX player

def BurningSphere.contact (sphere : BurningSphere) (player : Player) : Option Int :=
  CasterAI.contact sphere.hitBox sphere.centerX player

def FireImp.valid (imp : FireImp) (world : World) : Bool :=
  imp.id > 0 && CasterAI.validMotion world imp.x imp.y imp.vx imp.vy &&
    imp.health > 0 && imp.health ≤ fireImpHealth && imp.clock < 650 && imp.casting ≤ 29 &&
    imp.timeLeft > 0 && imp.timeLeft ≤ 937 && imp.frame ≤ 9 && imp.frameClock ≤ 5 &&
    (imp.direction == -1 || imp.direction == 1) && (imp.spriteDirection == -1 || imp.spriteDirection == 1) &&
    imp.destination.all (fun p => p.x > 0 && p.y > 0 && world.contains p)

def BurningSphere.valid (sphere : BurningSphere) (world : World) : Bool :=
  sphere.id > 0 && CasterAI.validMotion world sphere.x sphere.y sphere.vx sphere.vy &&
    sphere.vx.abs ≤ 5.000001 && sphere.vy.abs ≤ 5.000001 && sphere.timeLeft > 0 && sphere.timeLeft ≤ 749

def FireImp.atFloor (id : Nat) (column floor : Int) : FireImp :=
  { id, x := f32 (16 * column - 1), y := f32 (16 * floor - fireImpHeight) }

def FireImp.spawnAt (world : World) (water : Water) (id : Nat) (column floor : Int) : Option FireImp := do
  if !CasterAI.spaceAbove world column floor 3 then failure
  let imp := FireImp.atFloor id column floor
  if water.submerged world (trunc (imp.centerX * 16)) (trunc (imp.centerY * 16)) then failure
  return imp

def FireImp.knockback (imp : FireImp) (damage : Nat) (direction : Int) (force : Float32) : FireImp :=
  let (vx, vy) := CasterAI.knockback imp.vx imp.vy fireImpHealth damage direction force 0.5
  { imp with vx, vy }

/-- NPC 25 is born with target 255. Its first style-9 update aims at the
player at speed 5; it keeps that velocity thereafter. -/
def BurningSphere.fromImp (id : Nat) (imp : FireImp) (player : Player) : BurningSphere :=
  let x := f32 (trunc imp.x + 1 + 8 * imp.direction)
  let y := f32 (trunc imp.y + 4)
  let (vx, vy) := CasterAI.sphereVelocity x y 5 player
  { id, x, y, vx, vy }

def BurningSphere.advance (sphere : BurningSphere) (player : Player) : Option BurningSphere := do
  let (x, y, vx, timeLeft) ← CasterAI.moveSphere sphere.x sphere.y sphere.vx sphere.vy sphere.timeLeft player
  return { sphere with x, y, vx, timeLeft }

def FireImp.findTeleport (imp : FireImp) (world : World) (magma : Water) (player : Player)
    (dead : Bool) (roll : Nat → Nat) : Option TilePos :=
  CasterAI.findTeleport imp.centerX imp.centerY world player dead
    (fun above => magma.level world above == 0) roll

/-- NPC 24 uses the common style-8 clock and teleport search. The cast
uses a 20-update windup, and a successful teleport skips that windup. -/
def FireImp.advanceAI (imp : FireImp) (world : World) (magma : Water) (player : Player)
    (dead : Bool) (roll : Nat → Nat) : FireImp × Option FireImp := Id.run do
  let targetX := f32 (player.x.tdiv 16 + playerWidth / 32)
  let direction := if dead then imp.direction else if targetX < imp.centerX then -1 else 1
  let vx := imp.vx * 0.93
  let vx := if -0.1 < vx && vx < 0.1 then 0 else vx
  let mut next := { imp with direction, vx }
  if let some pos := next.destination then
    let floor := FireImp.atFloor next.id pos.x pos.y
    next := { next with x := floor.x, y := floor.y, vx := 0, vy := 0, destination := none }
  let clock := (if next.clock == 0 then 500 else next.clock) + 1
  let dx := trunc next.centerX - trunc (pixel (bodyCenterX player))
  let dy := trunc next.centerY - trunc (pixel (bodyCenterY player))
  let inRange := -910 ≤ dx && dx < 910 && -550 ≤ dy && dy < 550
  let casting := if inRange && (clock == 100 || clock == 200 || clock == 300) then 30 else next.casting
  next := { next with clock, casting }
  if next.clock ≥ 650 then
    next := { next with clock := 1 }
    if let some destination := next.findTeleport world magma player dead roll then
      next := { next with destination := some destination, casting := 5 }
  next := { next with casting := next.casting - 1 }
  return (next, if next.casting == 10 then some next else none)

private def FireImp.advanceFrame (imp : FireImp) : FireImp :=
  let spriteDirection := if imp.vy == 0 then imp.direction else imp.spriteDirection
  let casting := imp.casting > 0
  let startCast := casting && (imp.frame < 4 || imp.frame > 9)
  let frame := if startCast then 4 else imp.frame
  let frameClock := if startCast then 1 else imp.frameClock + 1
  let (frame, frameClock) := if frameClock > 5 then (frame + 1, 0) else (frame, frameClock)
  let frame := if casting then min 9 frame else if frame > 3 then 0 else frame
  { imp with frame, frameClock, spriteDirection }

def FireImp.advance (imp : FireImp) (world : World) (water magma : Water) (player : Player)
    (dead : Bool) (surface : Nat) (roll : Nat → Nat) : Option FireImp × Option FireImp :=
  let (next, fired) := imp.advanceAI world magma player dead roll
  let cx := trunc (next.centerX * 16)
  let cy := trunc (next.centerY * 16)
  let wet := water.submerged world cx cy || magma.submerged world cx cy
  let (x, y, vx, vy) := CasterAI.move next.x next.y next.vx next.vy world wet surface
  let framed := ({ next with x, y, vx, vy } : FireImp).advanceFrame
  let active := CasterAI.checkActive framed.centerX framed.centerY fireImpWidth fireImpHeight player framed.timeLeft
  (active.map (fun timeLeft => { framed with timeLeft }), fired)

end Lean4Terria
