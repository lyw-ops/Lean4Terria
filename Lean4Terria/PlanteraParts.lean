import Lean4Terria.DarkCaster

namespace Lean4Terria

def planteraHookDamage : Nat := 60
def planteraTentacleHealth : Nat := 1000
def planteraTentacleDamage : Nat := 60
def planteraTentacleDefense : Nat := 20
def planteraSporeDamage : Nat := 70

/-- NPC 263 stores its current tile attachment in ai[0, 1] and the
retargeting delay in localAI[0]. The position is the native top-left. -/
structure PlanteraHook where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  anchor : Option TilePos := none
  timer : Nat := 0
  frame : Nat := 0
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq PlanteraHook where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.anchor == b.anchor && a.timer == b.timer && a.frame == b.frame && a.frameClock == b.frameClock

/-- Classic NPC 264 attaches to Plantera's center. Its random angle is stored
as the two integral native AI offsets, before normalizing to the desired radius. -/
structure PlanteraTentacle where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  health : Nat := planteraTentacleHealth
  timer : Nat := 0
  offsetX : Int := 0
  offsetY : Int := 0
  facing : Int := -1
  frame : Nat := 0
  frameClock : Nat := 0
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq PlanteraTentacle where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.health == b.health && a.timer == b.timer && a.offsetX == b.offsetX && a.offsetY == b.offsetY &&
    a.facing == b.facing && a.frame == b.frame && a.frameClock == b.frameClock && a.timeLeft == b.timeLeft

/-- NPC 265 is a one-life contact enemy with its own activity countdown. -/
structure PlanteraSpore where
  id : Nat
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  timeLeft : Nat := 937
  deriving Repr, Inhabited

instance : BEq PlanteraSpore where
  beq a b := a.id == b.id && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.timeLeft == b.timeLeft

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def trunc (n : Float32) : Int := n.toInt32.toInt
private def pixel (n : Int) : Float32 := single n / 16
private def length (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32
private def angle (x y : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32

private def deadZone (v : Float32) : Float32 :=
  if -0.005 < v.toFloat && v.toFloat < 0.005 then 0 else v

def PlanteraHook.atHead (centerX centerY : Float32) : PlanteraHook :=
  { x := single (trunc centerX - 20), y := single (trunc centerY - 40) }

def PlanteraTentacle.atHead (id : Nat) (centerX centerY : Float32) : PlanteraTentacle :=
  { id, x := single (trunc centerX - 12), y := single (trunc centerY - 24) }

def PlanteraSpore.atHead (id : Nat) (centerX centerY vx vy : Float32) : PlanteraSpore :=
  { id, x := single (trunc centerX - 9), y := single (trunc centerY - 18), vx, vy }

def PlanteraHook.centerX (hook : PlanteraHook) : Float32 := hook.x + 20
def PlanteraHook.centerY (hook : PlanteraHook) : Float32 := hook.y + 20
def PlanteraTentacle.centerX (tentacle : PlanteraTentacle) : Float32 := tentacle.x + 12
def PlanteraTentacle.centerY (tentacle : PlanteraTentacle) : Float32 := tentacle.y + 12
def PlanteraSpore.centerX (spore : PlanteraSpore) : Float32 := spore.x + 9
def PlanteraSpore.centerY (spore : PlanteraSpore) : Float32 := spore.y + 9

def PlanteraHook.hitBox (hook : PlanteraHook) : HitBox := CasterAI.npcBox hook.x hook.y 40 40
def PlanteraTentacle.hitBox (tentacle : PlanteraTentacle) : HitBox := CasterAI.npcBox tentacle.x tentacle.y 24 24
def PlanteraSpore.hitBox (spore : PlanteraSpore) : HitBox := CasterAI.npcBox spore.x spore.y 18 18

def PlanteraHook.contact (hook : PlanteraHook) (player : Player) : Option Int :=
  CasterAI.contact hook.hitBox hook.centerX player

def PlanteraTentacle.contact (tentacle : PlanteraTentacle) (player : Player) : Option Int :=
  CasterAI.contact tentacle.hitBox tentacle.centerX player

def PlanteraSpore.contact (spore : PlanteraSpore) (player : Player) : Option Int :=
  CasterAI.contact spore.hitBox spore.centerX player

def PlanteraHook.moving (hook : PlanteraHook) : Bool := hook.vx != 0 || hook.vy != 0

def PlanteraHook.valid (hook : PlanteraHook) (world : World) : Bool :=
  CasterAI.validMotion world hook.x hook.y 0 0 &&
    hook.vx.isFinite && hook.vy.isFinite && hook.vx.abs < 52 && hook.vy.abs < 52 &&
    hook.rotation.isFinite && hook.timer < 600 && hook.frame ≤ 1 && hook.frameClock ≤ 4 &&
    hook.anchor.all world.contains

def PlanteraTentacle.valid (tentacle : PlanteraTentacle) (world : World) : Bool :=
  tentacle.id > 0 && CasterAI.validMotion world tentacle.x tentacle.y tentacle.vx tentacle.vy &&
    tentacle.rotation.isFinite && 0 < tentacle.health && tentacle.health ≤ planteraTentacleHealth &&
    tentacle.timer < 480 && tentacle.offsetX.natAbs ≤ 100 && tentacle.offsetY.natAbs ≤ 100 &&
    (tentacle.facing == -1 || tentacle.facing == 1) &&
    tentacle.frame < 4 && tentacle.frameClock < 6 && 0 < tentacle.timeLeft && tentacle.timeLeft ≤ 937

def PlanteraSpore.valid (spore : PlanteraSpore) (world : World) : Bool :=
  spore.id > 0 && CasterAI.validMotion world spore.x spore.y spore.vx spore.vy &&
    spore.rotation.isFinite && 0 < spore.timeLeft && spore.timeLeft ≤ 937

private def playerCenter (player : Player) : Float32 × Float32 :=
  (pixel player.x + single (playerWidth / 16) * 0.5, pixel player.y + single (playerHeight / 16) * 0.5)

private def tileAtPixel (x y : Float32) : TilePos :=
  ⟨trunc (x / 16), trunc (y / 16)⟩

private def PlanteraHook.findAnchor (hook : PlanteraHook) (world : World) (hasWall : TilePos → Bool)
    (player : Player) (headX headY : Float32) (belowHalf dead : Bool) (roll : Nat → Nat) :
    Option TilePos := Id.run do
  let (px, py) := playerCenter player
  let playerTile := tileAtPixel px py
  let target := if dead then
      ⟨(trunc headX).tdiv 16, (trunc (headY + 400)).tdiv 16⟩
    else if !hook.anchor.any (·.x != 0) then
      ⟨trunc ((px + (headX + 43)) / 32), trunc ((py + (headY + 43)) / 32)⟩
    else playerTile
  -- The first draw selected localAI[0]. Each attempt then draws two
  -- coordinates and, below half life, one chance to use the player's wall.
  let mut draw := 1
  for attempt in [1:1002] do
    let reach := 20 + trunc (100 * (single attempt / 1000))
    let span := (2 * reach + 1).toNat
    let x := target.x + Int.ofNat (roll draw % span) - reach
    let y := target.y + Int.ofNat (roll (draw + 1) % span) - reach
    draw := draw + 2
    let mut tile : TilePos := ⟨x, y⟩
    if belowHalf then
      if roll draw % 6 == 0 && hasWall playerTile then tile := playerTile
      draw := draw + 1
    if world.contains tile &&
        ((world.tile tile).solid || (hasWall tile && (attempt > 500 || belowHalf))) then
      return some tile
  return hook.anchor

private def PlanteraHook.animate (hook : PlanteraHook) : PlanteraHook :=
  let moved := hook.moving
  if (moved && hook.frame == 0) || (!moved && hook.frame > 0) then
    if hook.frameClock == 4 then { hook with frame := if moved then 1 else 0, frameClock := 0 }
    else { hook with frameClock := hook.frameClock + 1 }
  else hook

/-- Classic style 52. The moving peer count covers the other two hooks with
nonzero velocity at this point in the update order. The head position is top-left. -/
def PlanteraHook.advance (hook : PlanteraHook) (world : World) (hasWall : TilePos → Bool)
    (player : Player) (headX headY : Float32) (headHealth movingPeers : Nat)
    (enraged dead : Bool) (roll : Nat → Nat) : PlanteraHook :=
  let belowHalf := headHealth < 15000
  let belowQuarter := headHealth < 7500
  let enraged := enraged || dead
  let spent := 1 + (if belowHalf then 2 else 0) + (if belowQuarter then 2 else 0) +
    (if enraged then 10 else 0)
  let timer := if hook.anchor.any (fun tile => tile.x != 0 && tile.y != 0) then hook.timer - spent else 0
  let aimed := if timer != 0 then { hook with timer }
    else if !dead && hook.anchor.any (·.x != 0) && movingPeers > 0 then
      { hook with timer := 60 + roll (movingPeers - 1) % 240 }
    else
      { hook with timer := 300 + roll 0 % 300
                  anchor := hook.findAnchor world hasWall player headX headY belowHalf dead roll }
  let moving := match aimed.anchor.filter (fun tile => tile.x > 0 && tile.y > 0) with
    | none => aimed
    | some anchor =>
      let speed : Float32 := (if belowQuarter then 10 else if belowHalf then 8 else 6) *
        (if enraged then 2 else 1) * (if dead then 2 else 1)
      let dx := single anchor.x * 16 - 8 - aimed.centerX
      let dy := single anchor.y * 16 - 8 - aimed.centerY
      let distance := length dx dy
      let amount := if distance < 12 + speed then 1 else speed / distance
      { aimed with
        vx := dx * amount
        vy := dy * amount
        rotation := angle ((headX + 43) - aimed.centerX) ((headY + 43) - aimed.centerY) - 1.57 }
  let vx := deadZone moving.vx
  { moving with vx, x := moving.x + vx, y := moving.y + moving.vy }.animate

private def tentacleSteer (position velocity center offset : Float32) : Float32 :=
  let target := center + offset
  if position < target then
    let next := velocity + 0.2
    if next < 0 && offset > 0 then next * 0.9 else next
  else if position > target then
    let next := velocity - 0.2
    if next > 0 && offset < 0 then next * 0.9 else next
  else velocity

/-- Classic style 53. Handles native inactivity and advances the four-frame
strip after the no-tile-collision movement. -/
def PlanteraTentacle.advance (tentacle : PlanteraTentacle) (world : World) (player : Player)
    (headCenterX headCenterY : Float32) (headHealth : Nat) (roll : Nat → Nat) :
    Option PlanteraTentacle := do
  let tentacle := if tentacle.timer > 1 then { tentacle with timer := tentacle.timer - 1 }
    else { tentacle with timer := 120 + roll 0 % 360
                         offsetX := Int.ofNat (roll 1 % 201) - 100
                         offsetY := Int.ofNat (roll 2 % 201) - 100 }
  let rawX := headCenterX + single tentacle.offsetX - headCenterX
  let rawY := headCenterY + single tentacle.offsetY - headCenterY
  let radius : Float32 := 200 + (if headHealth < 7500 then 100 else 0) +
    (if headHealth < 3000 then 100 else 0)
  let distance := length rawX rawY
  let aimed := if distance == 0 then tentacle
    else
      let scale := radius / distance
      let dx := rawX * scale
      let dy := rawY * scale
      let facing := if dx > 0 then 1 else if dx < 0 then -1 else tentacle.facing
      let rotation := if dx > 0 then angle dx dy
        else if dx < 0 then angle dx dy + 3.14 else tentacle.rotation
      { tentacle with
        vx := tentacleSteer tentacle.x tentacle.vx headCenterX dx
        vy := tentacleSteer tentacle.y tentacle.vy headCenterY dy
        facing, rotation }
  let vx := deadZone (max (-8) (min 8 aimed.vx))
  let vy := max (-8) (min 8 aimed.vy)
  let moved := { aimed with vx, vy, x := aimed.x + vx, y := aimed.y + vy }
  let animated := if moved.frameClock == 5 then
      { moved with frame := (moved.frame + 1) % 4, frameClock := 0 }
    else { moved with frameClock := moved.frameClock + 1 }
  let timeLeft ← CasterAI.checkActive animated.centerX animated.centerY 24 24 player animated.timeLeft
  let next := { animated with timeLeft }
  if next.valid world then some next else none

/-- Classic style 50, specifically the tile-passing Plantera spore. Native
EncourageDespawn(5) still permits CheckActive to refresh a nearby spore. -/
def PlanteraSpore.advance (spore : PlanteraSpore) (world : World) (player : Player) :
    Option PlanteraSpore := do
  let tx := pixel player.x
  let ty := pixel player.y
  let vy := spore.vy + 0.02
  let vy := min 1 (if vy < 0 && ty > spore.y + 100 then vy * 0.95 else vy)
  let vx := if spore.x + 18 < tx then (if spore.vx < 0 then spore.vx * 0.98 else spore.vx) + 0.1
    else if spore.x > tx + single (playerWidth / 16) then
      (if spore.vx > 0 then spore.vx * 0.98 else spore.vx) - 0.1
    else spore.vx
  let vx := if vx > 5 || vx < -5 then vx * 0.97 else vx
  let rotation := vx * 0.2
  let vx := deadZone vx
  let moved := { spore with vx, vy, rotation, x := spore.x + vx, y := spore.y + vy }
  let timeLeft ← CasterAI.checkActive moved.centerX moved.centerY 18 18 player (min 5 moved.timeLeft)
  let next := { moved with timeLeft }
  if next.valid world then some next else none

end Lean4Terria
