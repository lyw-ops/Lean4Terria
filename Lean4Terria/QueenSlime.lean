import Lean4Terria.UnderworldCreatures
import Lean4Terria.UnderworldShots
import Lean4Terria.DarkCaster

namespace Lean4Terria

def queenSlimeHealth : Nat := 18000
def queenSlimeDefense : Nat := 26
def maxQueenMinions : Nat := 64
def maxQueenShots : Nat := 128

inductive QueenPhase where
  | rest | grow | shrink | jumps | smash | gel
  deriving BEq, Repr, Inhabited

/-- Float32 position and velocity in native pixels, in AI style 121. -/
structure QueenSlime where
  x : Float32 := 0
  y : Float32 := 0
  vx : Float32 := 0
  vy : Float32 := 0
  health : Nat := queenSlimeHealth
  minionHealthMark : Nat := queenSlimeHealth
  phase : QueenPhase := .rest
  timer : Int := -100
  sequence : Nat := 0
  blocked : Nat := 0
  scale : Float32 := 1
  rotation : Float32 := 0
  destination : Option TilePos := none
  timeLeft : Nat := 750
  direction : Int := 1
  wingClock : Nat := 0
  wet : Bool := false
  deriving Repr, Inhabited

instance : BEq QueenSlime where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits && a.vx.toBits == b.vx.toBits &&
    a.vy.toBits == b.vy.toBits && a.health == b.health && a.minionHealthMark == b.minionHealthMark &&
    a.phase == b.phase && a.timer == b.timer && a.sequence == b.sequence && a.blocked == b.blocked &&
    a.scale.toBits == b.scale.toBits && a.rotation.toBits == b.rotation.toBits &&
    a.destination == b.destination && a.timeLeft == b.timeLeft && a.direction == b.direction &&
    a.wingClock == b.wingClock && a.wet == b.wet

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (n : Int) : Float32 := single n / 16
private def distance (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32

private def sized (x y speed : Float32) : Float32 × Float32 :=
  let d := distance x y
  if d == 0 then (0, 0) else (x * (speed / d), y * (speed / d))

def QueenSlime.flying (queen : QueenSlime) : Bool := queen.health ≤ queenSlimeHealth / 2
def QueenSlime.width (queen : QueenSlime) : Nat := (114 * queen.scale).toUInt32.toNat
def QueenSlime.height (queen : QueenSlime) : Nat := (100 * queen.scale).toUInt32.toNat
def QueenSlime.centerX (queen : QueenSlime) : Float32 := queen.x + single (queen.width / 2)
def QueenSlime.bottom (queen : QueenSlime) : Float32 := queen.y + single queen.height
def QueenSlime.centerY (queen : QueenSlime) : Float32 := queen.y + single (queen.height / 2)
def QueenSlime.hitBox (queen : QueenSlime) : HitBox := CasterAI.npcBox queen.x queen.y queen.width queen.height
def QueenSlime.contact (queen : QueenSlime) (player : Player) : Option Int :=
  CasterAI.contact queen.hitBox queen.centerX player

def QueenSlime.valid (queen : QueenSlime) (world : World) : Bool :=
  CasterAI.validMotion world queen.x queen.y 0 0 && queen.vx.isFinite && queen.vy.isFinite &&
    queen.vx.abs ≤ 32 && queen.vy.abs ≤ 32 && queen.scale ≥ 0.5 && queen.scale ≤ 1 &&
    queen.rotation.isFinite && queen.rotation.abs ≤ 0.5 &&
    queen.health > 0 && queen.health ≤ queen.minionHealthMark && queen.minionHealthMark ≤ queenSlimeHealth &&
    0 < queen.timeLeft && queen.timeLeft ≤ 750 && queen.direction.natAbs == 1 &&
    queen.wingClock < 24 && queen.blocked ≤ 720 && queen.destination.all world.contains &&
    match queen.phase with
    | .rest => -100 ≤ queen.timer && queen.timer ≤ 120 && queen.sequence == 0
    | .grow => 0 ≤ queen.timer && queen.timer < 30 && queen.sequence == 0
    | .shrink => 0 ≤ queen.timer && queen.timer < 60 && queen.sequence == 0 && queen.destination.isSome
    | .jumps => -60 ≤ queen.timer && queen.timer ≤ 0 && queen.sequence ≤ 3
    | .smash => 0 ≤ queen.timer && queen.timer ≤ 130 && queen.sequence ≤ 1
    | .gel => 0 ≤ queen.timer && queen.timer < 50 && queen.sequence ≤ 1

private def QueenSlime.atFloor (queen : QueenSlime) (pos : TilePos) : QueenSlime :=
  { queen with
    x := single (pos.x * 16 + 8) - single (queen.width / 2)
    y := single (pos.y * 16) - single queen.height }

/-- Teleport resizing preserves the bottom center. -/
private def QueenSlime.resize (queen : QueenSlime) (scale : Float32) : QueenSlime :=
  let resized := { queen with scale }
  { resized with x := queen.centerX - single (resized.width / 2), y := queen.bottom - single resized.height }

private def QueenSlime.changePhase (queen : QueenSlime) (phase : QueenPhase) : QueenSlime :=
  { queen with phase, timer := 0, sequence := 0, destination := none }

private def floorCandidates (world : World) (center : TilePos) (radius clearance : Int)
    (water magma : Water) : Array TilePos := Id.run do
  let mut candidates := #[]
  for col in [(max 4 (center.x - radius)).toNat:(min ((world.width : Int) - 4) (center.x + radius + 1)).toNat] do
    if ((col : Int) - center.x).natAbs < clearance.toNat then continue
    for row in [(max 7 (center.y - radius)).toNat:(min ((world.height : Int) - 2) (center.y + radius + 1)).toNat] do
      let floor : TilePos := ⟨col, row⟩
      let pos := { floor with y := floor.y - 1 }
      if (world.tile floor).solid && !npcSolidOverlap world (single (floor.x * 16 - 49))
          (single (floor.y * 16 - 100)) 114 100 && water.level world pos == 0 && magma.level world pos == 0 then
        candidates := candidates.push floor
  return candidates

/-- SpawnOnPlayer distances are scaled for the compact world, with clearance
for the original collision rectangle. -/
def QueenSlime.spawn (world : World) (water magma : Water) (player : Player) (sample : Nat) : QueenSlime :=
  let center := player.centerTile
  let candidates := floorCandidates world center 26 9 water magma
  let x := min ((world.width : Int) - 5) (max 4 (center.x + if sample % 2 == 0 then 9 else -9))
  let fallback : TilePos := ⟨x, max 7 (center.y - 9)⟩
  (default : QueenSlime).atFloor (candidates[sample % candidates.size]?.getD fallback)

private def teleportCandidates (world : World) (magma : Water) (player : Player)
    (radius gap : Int) : Array TilePos := Id.run do
  let center := player.centerTile
  let mut candidates := #[]
  for x in [(max 1 (center.x - radius)).toNat:(min (world.width : Int) (center.x + radius + 1)).toNat] do
    for y in [(max 4 (center.y - radius)).toNat:(min (world.height : Int) (center.y + radius + 1)).toNat] do
      if max ((x : Int) - center.x).natAbs ((y : Int) - center.y).natAbs < gap.toNat then continue
      let floor : TilePos := ⟨x, y⟩
      let tile := world.tile floor
      if (tile.solid || tile == .platform) && !(world.tile ⟨x, y - 1⟩).solid &&
          magma.level world floor == 0 &&
          terrainLineClear world ((x : Int) * tileUnits + tileUnits / 2) (y * tileUnits - 50 * 16)
            (bodyCenterX player) (bodyCenterY player) then
        candidates := candidates.push floor
  return candidates

inductive QueenShotKind where
  | spike | bouncy | regal | stomp
  deriving BEq, Repr, Inhabited

def QueenShotKind.sprite : QueenShotKind → Nat
  | .spike => 920
  | .bouncy => 921
  | .regal => 926
  | .stomp => 922

def QueenShotKind.damage : QueenShotKind → Nat
  | .spike | .bouncy => 15
  | .regal => 30
  | .stomp => 40

private def QueenShotKind.halfSize : QueenShotKind → Nat
  | .spike | .bouncy => 3
  | .regal => 6
  | .stomp => 15

def QueenShotKind.bounces : QueenShotKind → Nat
  | .spike | .stomp => 0
  | .bouncy => 2
  | .regal => 1

def QueenShotKind.lifetime : QueenShotKind → Nat
  | .stomp => 9
  | _ => 3600

structure QueenShot where
  kind : QueenShotKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  age : Nat := 0
  bounced : Nat := 0
  slot : Option Nat := none
  deriving Repr, Inhabited

instance : BEq QueenShot where
  beq a b := a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.age == b.age &&
    a.bounced == b.bounced && a.slot == b.slot

def QueenShot.diameter (shot : QueenShot) : Nat :=
  if shot.kind == .stomp && shot.age > 0 then (16 * (5 + 25 * (single shot.age / 9))).toUInt32.toNat
  else 2 * shot.kind.halfSize

def QueenShot.hitBox (shot : QueenShot) : HitBox :=
  let half := single (shot.diameter / 2)
  CasterAI.npcBox (shot.x - half) (shot.y - half) shot.diameter shot.diameter

private def QueenShot.inWorld (shot : QueenShot) (world : World) : Bool :=
  -16 ≤ shot.x && -16 ≤ shot.y && shot.x < single (16 * world.width + 16 : Nat) &&
    shot.y < single (16 * world.height + 16 : Nat)

def QueenShot.valid (shot : QueenShot) (world : World) : Bool :=
  shot.inWorld world && shot.vx.isFinite && shot.vy.isFinite &&
    shot.vx.abs ≤ 32 && shot.vy.abs ≤ 32 && shot.age < shot.kind.lifetime &&
    shot.bounced ≤ shot.kind.bounces && shot.slot.any (· < maxProjectileSlots)

def QueenShot.advance (shot : QueenShot) (world : World) (water magma honey : Water) : Option QueenShot := do
  if shot.age ≥ shot.kind.lifetime then failure
  let age := shot.age + 1
  if shot.kind == .stomp then return { shot with age }
  let half := single shot.kind.halfSize
  let vy := if age ≥ 5 then min 16 (shot.vy + 0.15) else shot.vy
  let (dx, dy) := clipShotMotion world shot.x shot.y shot.vx vy half true
  let hitX := dx != shot.vx
  let hitY := dy != vy
  let bounced := shot.bounced + if hitX || hitY then 1 else 0
  if bounced > shot.kind.bounces then failure
  let inLiquid (liquid : Water) := shotLiquidContact world liquid shot.x shot.y half (half / 2)
  let drag : Float32 := if inLiquid honey then 0.25 else if inLiquid water || inLiquid magma then 0.5 else 1
  let next := { shot with
    x := shot.x + dx * drag, y := shot.y + dy * drag
    vx := if hitX then -shot.vx else shot.vx
    vy := if hitY then -vy else vy
    age, bounced }
  if next.inWorld world then some next else none

private def slowX (vx : Float32) : Float32 :=
  let speed := vx * 0.8
  if speed.abs.toFloat < 0.1 then 0 else speed

private def steerAxis (current goal step : Float32) : Float32 :=
  if current < goal then
    let next := current + step
    if next < 0 && goal > 0 then next + step else next
  else if current > goal then
    let next := current - step
    if next > 0 && goal < 0 then next - step else next
  else current

private def clearLine (world : World) (x y targetX targetY : Float32) : Bool :=
  terrainLineClear world (x * 16).toInt32.toInt (y * 16).toInt32.toInt
    (targetX * 16).toInt32.toInt (targetY * 16).toInt32.toInt

/-- The distance before the 40-pixel horizontal deadzone determines speed
and acceleration in AI_121_QueenSlime_FlyMovement. -/
private def QueenSlime.steerFlight (queen : QueenSlime) (world : World) (player : Player)
    (retreat : Bool) : QueenSlime :=
  let px := pixels (bodyCenterX player)
  let py := pixels (bodyCenterY player)
  let clear := clearLine world queen.centerX queen.centerY px py
  let targetY := if clear then py - 250 else
    let ceiling := (List.range 16).find? fun row =>
      let cell : TilePos := ⟨player.centerTile.x, ((py - single (row * 16 : Nat)) / 16).toInt32.toInt⟩
      world.contains cell && (world.tile cell).solid
    (ceiling.map fun row => py - single (row * 16 : Nat) + 16).getD py
  let dx := if retreat then 500 * single queen.direction else px - queen.centerX
  let dy := if retreat then -250 else targetY - queen.centerY
  let scale := distance dx dy
  let dx := if dx.abs < 40 then queen.vx else dx
  let step : Float32 :=
    if scale > 100 && ((queen.vx < -10 && dx > 0) || (queen.vx > 10 && dx < 0)) then 0.17 else 0.085
  let (vx, vy) := if scale < 40 then (queen.vx, queen.vy) else sized dx dy (if scale < 80 then 12 * 0.65 else 12)
  let vx := steerAxis queen.vx vx step
  { queen with vx, vy := steerAxis queen.vy vy step, rotation := max (-0.5) (min 0.5 (vx * 0.1)) }

private def QueenSlime.gelSpread (queen : QueenSlime) : Array QueenShot :=
  (Array.range (if queen.flying then 10 else 6)).map fun (index : Nat) =>
    let angle := (-single index * 6.2831855 / 10).toFloat
    { kind := .regal, x := queen.centerX, y := queen.centerY
      vx := (9 * angle.cos).toFloat32, vy := (9 * angle.sin).toFloat32 }

/-- Ground jumps, teleport, smash, and gel from NPC.AI_121_QueenSlime.
The terrain step follows AI, as in NPC.UpdateNPC. -/
def QueenSlime.advance (initial : QueenSlime) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) (roll : Nat → Nat) : Option QueenSlime × Array QueenShot := Id.run do
  let px := pixels (bodyCenterX player)
  let py := pixels (bodyCenterY player)
  let dx := px - initial.centerX
  let retreat := dead || dx.abs > 8000 || initial.timeLeft ≤ 10
  let onScreen := dx.abs < 960 && (py - initial.centerY).abs < 600
  let lifetime := if retreat then if onScreen then 10 else min initial.timeLeft 10 - 1 else 750
  if lifetime == 0 then return (none, #[])
  let direction := if dx < 0 then -1 else 1
  let mut queen := { initial with timeLeft := lifetime, direction := if retreat then -direction else direction }
  let flying := queen.flying
  let mut shots : Array QueenShot := #[]
  let mut free := false
  let mut gravity := true
  if flying then queen := { queen with wingClock := (queen.wingClock + 1) % 24 }
  if retreat then
    let phase := if flying then QueenPhase.rest else .jumps
    if queen.phase != phase then queen := queen.changePhase phase
  else if !flying && queen.phase == .rest && queen.vy == 0 && queen.blocked ≥ 600 then
    let candidates := if queen.blocked < 720 && distance dx (py - queen.centerY) ≤ 2000 then
        let wide := teleportCandidates world magma player 10 7
        if wide.isEmpty then teleportCandidates world magma player 6 2 else wide
      else #[]
    let destination := candidates[roll 0 % candidates.size]?.getD
      ⟨player.centerTile.x, (player.y + playerHeight) / tileUnits⟩
    queen := { queen.changePhase .shrink with
      destination := some destination, blocked := if candidates.isEmpty then 0 else queen.blocked }
  let blocked := !flying && (!clearLine world queen.centerX queen.centerY px py ||
    (queen.y - pixels (player.y + playerHeight)).abs > 320)
  queen := { queen with blocked := if blocked then min 720 (queen.blocked + 3) else queen.blocked - 2 }
  match queen.phase with
  | .rest =>
    if queen.scale != 1 then queen := queen.resize 1
    if flying then
      queen := queen.steerFlight world player retreat
      free := true
      gravity := false
    else if queen.vy == 0 then queen := { queen with vx := slowX queen.vx, rotation := 0 }
    if !retreat && (flying || queen.vy == 0) then
      queen := { queen with timer := queen.timer + 1 }
      if queen.timer > (if flying then 120 else 60) then
        if flying then
          let canSmash := pixels (player.y + playerHeight) ≥ queen.bottom && (px - queen.centerX).abs ≤ 250
          if roll 1 % 2 == 0 && canSmash then queen := { queen.changePhase .smash with sequence := 1 }
          else queen := queen.changePhase .gel
        else queen := queen.changePhase (#[QueenPhase.jumps, .smash, .gel][roll 1 % 3]!)
  | .shrink =>
    let timer := queen.timer + 1
    if timer ≥ 60 then
      if let some target := queen.destination then queen := queen.atFloor target
      queen := (queen.changePhase .grow).resize 0.5
    else queen := ({ queen with timer, rotation := 0 }).resize (1 - single timer / 120)
  | .grow =>
    let timer := queen.timer + 1
    if timer ≥ 30 then queen := (queen.changePhase .rest).resize 1
    else queen := ({ queen with timer, rotation := 0 }).resize (0.5 + single timer / 60)
  | .jumps =>
    if queen.vy == 0 then
      let timer := queen.timer + 4 + (if queen.health * 100 < queenSlimeHealth * 66 then 4 else 0) +
        (if queen.health * 100 < queenSlimeHealth * 33 then 4 else 0)
      queen := { queen with timer, vx := slowX queen.vx, rotation := 0 }
      if timer ≥ 0 then
        let (vx, vy) : Float32 × Float32 :=
          if queen.sequence == 3 then (3.5, -13) else if queen.sequence == 2 then (4.5, -6) else (4, -8)
        queen := { queen with vx := queen.vx + vx * single queen.direction, vy }
        if queen.sequence == 3 then
          queen := if retreat then { queen with timer := -60, sequence := 0 } else queen.changePhase .rest
        else queen := { queen with timer := -40, sequence := queen.sequence + 1 }
    else
      let cap : Float32 := if queen.sequence == 3 then 7 else 3
      let signed := queen.vx * single queen.direction
      if signed < cap then
        let vx := if signed < 0.1 then queen.vx + 0.2 * single queen.direction else queen.vx * 0.93
        queen := { queen with vx }
  | .smash =>
    queen := { queen with rotation := queen.rotation * 0.9 }
    gravity := false
    if queen.sequence == 0 then
      free := true
      let timer := queen.timer + 1
      queen := { queen with timer }
      if timer ≥ 30 then
        let (vx, vy) : Float32 × Float32 :=
          if timer ≥ 60 then (queen.vx, -3)
          else if queen.vy == 0 then sized dx (py - 384 - queen.centerY) 20 else (queen.vx, queen.vy)
        queen := { queen with
          vx, vy := vy * 0.95
          timer := if timer ≥ 60 then 0 else timer
          sequence := if timer ≥ 60 then 1 else 0 }
    else if queen.vy == 0 then
      shots := #[{ kind := .stomp, x := queen.centerX, y := queen.bottom }]
      queen := queen.changePhase .rest
    else
      let timer := queen.timer + 1
      let delay := if flying then 10 else 30
      if flying && timer > delay + 120 then queen := { queen.changePhase .rest with vy := queen.vy * 0.8 }
      else
        queen := { queen with
          timer, vx := queen.vx * 0.8
          vy := if timer < delay then queen.vy * 0.8 else min 14 (queen.vy + 1) }
  | .gel =>
    free := true
    gravity := false
    queen := { queen with rotation := queen.rotation * 0.9 }
    let timer := queen.timer + 1
    if queen.sequence == 0 then
      if timer ≥ 50 then queen := { queen with timer := 0, sequence := 1 }
      else queen := { queen with timer }
      if flying then queen := queen.steerFlight world player false
    else if timer ≥ 10 then
      shots := queen.gelSpread
      queen := queen.changePhase .rest
    else queen := { queen with timer }
  if free then
    queen := { queen with x := queen.x + queen.vx, y := queen.y + queen.vy }
  else
    let vy := if gravity then min 10 (queen.vy + (if queen.wet then 0.2 else 0.3 : Float32)) else queen.vy
    let moved := moveNpc world water magma queen.x queen.y queen.vx vy queen.width queen.height queen.wet true
    queen := { queen with x := moved.x, y := moved.y, vx := moved.vx, vy := moved.vy, wet := moved.wet }
  return (some queen, shots)

inductive QueenMinionKind where
  | crystal | bouncy | heavenly
  deriving BEq, Repr, Inhabited

def QueenMinionKind.sprite : QueenMinionKind → Nat
  | .crystal => 658
  | .bouncy => 659
  | .heavenly => 660

def QueenMinionKind.width (kind : QueenMinionKind) : Nat := if kind == .heavenly then 40 else 24
def QueenMinionKind.height (kind : QueenMinionKind) : Nat := if kind == .heavenly then 30 else 18
def QueenMinionKind.health (kind : QueenMinionKind) : Nat := if kind == .heavenly then 120 else 150
def QueenMinionKind.damage (kind : QueenMinionKind) : Nat := if kind == .heavenly then 50 else 40
def QueenMinionKind.defense (kind : QueenMinionKind) : Nat := if kind == .heavenly then 30 else 35

structure QueenMinion where
  id : Nat
  kind : QueenMinionKind
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  health : Nat
  timer : Int
  jumps : Nat := 0
  charge : Nat := 0
  timeLeft : Nat := 750
  direction : Int := 1
  wet : Bool := false
  frameClock : Nat := 0
  deriving Repr, Inhabited

instance : BEq QueenMinion where
  beq a b := a.id == b.id && a.kind == b.kind && a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits && a.health == b.health &&
    a.timer == b.timer && a.jumps == b.jumps && a.charge == b.charge &&
    a.timeLeft == b.timeLeft && a.direction == b.direction && a.wet == b.wet && a.frameClock == b.frameClock

def QueenMinion.centerX (minion : QueenMinion) : Float32 := minion.x + single (minion.kind.width / 2)
def QueenMinion.centerY (minion : QueenMinion) : Float32 := minion.y + single (minion.kind.height / 2)
def QueenMinion.hitBox (minion : QueenMinion) : HitBox :=
  CasterAI.npcBox minion.x minion.y minion.kind.width minion.kind.height

def QueenMinion.contact (minion : QueenMinion) (player : Player) : Option Int :=
  CasterAI.contact minion.hitBox minion.centerX player

def QueenMinion.valid (minion : QueenMinion) (world : World) : Bool :=
  CasterAI.validMotion world minion.x minion.y 0 0 && minion.vx.isFinite && minion.vy.isFinite &&
    minion.vx.abs ≤ 32 && minion.vy.abs ≤ 32 && minion.id > 0 && minion.health > 0 &&
    minion.health ≤ minion.kind.health && minion.timeLeft > 0 && minion.timeLeft ≤ 750 &&
    -1500 ≤ minion.timer && minion.timer ≤ 0 && minion.jumps < 3 && minion.charge < 50 &&
    minion.direction.natAbs == 1 && minion.frameClock < 32

/-- The life mark uses strict 2% and 1.5% cutoffs. Crossing half health
resets the attack and life mark before checking for another spawn. -/
def QueenSlime.spawnMinions (queen : QueenSlime) (firstId : Nat) (roll : Nat → Nat) :
    QueenSlime × Array QueenMinion :=
  if queen.health < queenSlimeHealth / 2 && queen.minionHealthMark ≥ queenSlimeHealth / 2 then
    ({ queen.changePhase .rest with minionHealthMark := queen.health }, #[])
  else if queen.health + (if queen.flying then 270 else 360) ≥ queen.minionHealthMark then (queen, #[])
  else
    let minions := (Array.range (1 + roll 0 % 2)).map fun i =>
      let sample (n : Nat) := roll (1 + i * 6 + n)
      let kind := #[QueenMinionKind.crystal, .bouncy, .heavenly][sample 2 % 3]!
      let x := (queen.x + single (sample 0 % (queen.width - 32))).toInt32.toInt
      let y := (queen.y + single (sample 1 % (queen.height - 32))).toInt32.toInt
      { id := firstId + i, kind, x := single (x - (kind.width / 2 : Nat)), y := single (y - kind.height)
        vx := single ((sample 3 % 31 : Nat) - (15 : Int)) * 0.1
        vy := single ((sample 4 % 31 : Nat) - (30 : Int)) * 0.1
        health := kind.health, timer := -500 * (sample 5 % 3 : Nat) }
    ({ queen with minionHealthMark := queen.health }, minions)

def QueenMinion.advance (initial : QueenMinion) (world : World) (water magma : Water)
    (player : Player) (dead : Bool) : Option QueenMinion × Option QueenShot := Id.run do
  let kind := initial.kind
  let dx := pixels (bodyCenterX player) - initial.centerX
  let dy := pixels (bodyCenterY player) - initial.centerY
  let direction := if dx < 0 then -1 else 1
  let mut minion := { initial with direction := direction, frameClock := (initial.frameClock + 1) % 32 }
  let fly := kind == .heavenly
  if fly then
    let ax : Float32 := 0.1 * single direction
    let ay : Float32 := if dy < 0 then -0.04 else 0.04
    minion := { minion with vx := max (-4) (min 4 (minion.vx + ax)), vy := max (-1.5) (min 1.5 (minion.vy + ay)) }
  else if minion.vy == (0 : Float32) then
    let timer : Int := minion.timer + 4
    minion := { minion with timer, vx := slowX minion.vx }
    if timer ≥ 0 then
      let high := minion.jumps == 2
      let jump : Float32 := if high then -8 else -6
      minion := { minion with
        timer := if high then -200 else -120
        jumps := (minion.jumps + 1) % 3
        vx := minion.vx + single direction * (if high then 3 else 2), vy := jump }
  else
    let step := single direction * 0.05
    if minion.vx.abs < (3 : Float32) then minion := { minion with vx := minion.vx + step }
  let mut shot := none
  let inRange := dx.abs < 500 && dy.abs < 550 &&
    clearLine world minion.centerX minion.centerY (minion.centerX + dx) (minion.centerY + dy)
  if !fly && !dead && inRange then
    let delay := if kind == .crystal then 50 else 40
    let charge := minion.charge + 1
    if charge ≥ delay then
      let (vx, vy) := sized dx (dy - 64) (if kind == .crystal then 6 else 9)
      shot := some {
        kind := if kind == .crystal then .spike else .bouncy
        x := minion.centerX, y := minion.centerY, vx, vy }
      minion := { minion with charge := 0 }
    else minion := { minion with charge }
  let vy := if fly then minion.vy else min 10 (minion.vy + (if minion.wet then 0.2 else 0.3 : Float32))
  let moved := moveNpc world water magma minion.x minion.y minion.vx vy kind.width kind.height minion.wet (!fly)
  let vx := if fly && moved.collideX then -minion.vx * 0.5 else moved.vx
  let vy := if fly && moved.collideY then -vy * 0.5 else moved.vy
  let next := { minion with x := moved.x, y := moved.y, vx, vy, wet := moved.wet }
  let active := npcActiveTime player next.x next.y kind.width kind.height next.timeLeft
  return (active.map fun timeLeft => { next with timeLeft }, shot)

end Lean4Terria
