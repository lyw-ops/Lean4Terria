import Lean4Terria.OriginalPlayerCollision
import Lean4Terria.UnifiedRandom

/-!
# Terraria 1.4.5.8 Classic tombstone Ghost spawn

`CheckSign` has already made the one-in-eight roll and chosen a player.
This module begins at `NPC.SpawnOnPlayer(player, 316, 0, 0, 0, 1)`.
It models ordinary worlds, the vanilla 1920×1200 spawn viewport, and the
in-world RNG stream used by mining. The ghost's AI updates run separately.
-/

namespace Lean4Terria.OriginalGhostSpawn

open OriginalCollision (Grid Vector single)
open OriginalPlayerCollision (Body)

structure Player where
  slot : Nat
  body : Body
  active : Bool := true
  scope : Bool := false
  heldItem : UInt16 := 0

structure SlotInfo where
  active : Bool := false
  protection : Nat := 0
  replaceable : Bool := false
  deriving Inhabited

structure World where
  grid : Grid
  houseWall : UInt16 → Bool
  /-- Active players checked by `CheckNotSpawningOnScreen`, including the target. -/
  players : Array Player
  npcSlot : Nat → SlotInfo := fun _ => {}
  maxNPCs : Nat := 200

private structure Rect where
  left : Int
  top : Int
  right : Int
  bottom : Int

private def Rect.contains (r : Rect) (x y : Int) : Bool :=
  r.left ≤ x && x < r.right && r.top ≤ y && y < r.bottom

private def centered (x y rx ry : Int) : Rect :=
  ⟨x - rx, y - ry, x + rx, y + ry⟩

private def extraView (player : Player) : Int × Int :=
  let scale : Float :=
    if player.heldItem == 1254 && player.scope then 1.25
    else if player.heldItem == 1254 || player.heldItem == 1299 then 1.5
    else if player.scope then 2
    else 0
  if scale == 0 then (0, 0)
  else ((60 / scale).toInt64.toInt, (37.5 / scale).toInt64.toInt)

private def solid (grid : Grid) (x y : Int) : Bool :=
  let tile := grid.tileAt x.toNat y.toNat
  !tile.actuated && tile.id.any (fun id => (grid.rules id).solid)

private def hasTileSpawnSpace (grid : Grid) (x y : Int) : Bool := Id.run do
  -- The bottom-right corner is exclusive for the loop and must still be in the world.
  if x < 1 || x + 1 ≥ grid.width || y < 3 || y ≥ grid.height then return false
  for tx in [(x-1).toNat:(x+1).toNat] do
    for ty in [(y-3).toNat:y.toNat] do
      if solid grid tx ty then return false
      if (grid.tileAt tx ty).liquid.any (fun liquid => liquid.kind == .lava && liquid.amount > 0) then
        return false
  return true

private def notOnScreen (players : Array Player) (safeX safeY x y : Int) : Bool :=
  players.all fun player =>
    if !player.active then true
    else
      let body := player.body
      let cx := body.position.x + single body.width / 2
      let cy := body.position.y + single body.height / 2
      let left := (cx - 960 - single safeX).toInt32.toInt
      let top := (cy - 600 - single safeY).toInt32.toInt
      let px := 16 * x
      let py := 16 * y
      px ≥ left + 1920 + 2 * safeX || px + 16 ≤ left ||
        py ≥ top + 1200 + 2 * safeY || py + 16 ≤ top

private def availableSlot (world : World) : Option Nat := Id.run do
  for index in [1:world.maxNPCs] do
    let slot := world.npcSlot index
    if !slot.active && slot.protection == 0 then return some index
  for index in [1:world.maxNPCs] do
    if (world.npcSlot index).replaceable then return some index
  return none

structure Ghost where
  slot : Nat
  target : Nat
  position : Vector
  npcType : UInt16 := 316
  active : Bool := true
  velocity : Vector := ⟨0, 0⟩
  width : Nat := 24
  height : Nat := 44
  life : Nat := 50
  lifeMax : Nat := 50
  damage : Nat := 15
  defense : Nat := 4
  knockbackResist : Float32 := 0.5
  timeLeft : Nat := 18740
  slotProtection : Nat := 2
  immune : Nat := 0
  justHit : Bool := false
  aiStyle : Nat := 22
  ai : Array Float32 := #[0, 0, 0, 1]
  localAI : Array Float32 := #[0, 0, 0, 0]
  direction : Int := 0
  directionY : Int := 1
  spriteDirection : Int := -1
  frameIndex : Nat := 0
  frameCounter : Nat := 0
  rotation : Float32 := 0
  gfxOffY : Float32 := 0
  scale : Float32 := 1
  alpha : Nat := 100
  noGravity : Bool := true
  noTileCollide : Bool := true
  wet : Bool := false
  deriving BEq, Repr

inductive Action where
  | draw (low high result : Int)
  | space (x y : Int)
  | screen (x y : Int)
  | spawn (bottomX bottomY : Int) (npcType : UInt16) (target : Nat)
  deriving BEq, Repr

structure Result where
  ghost : Option Ghost
  random : UnifiedRandom
  actions : Array Action

/-- Once `CheckSign` has requested a Ghost, search using the same player
stream and return the initial NPC and its native slot. The caller can use
`ghost.isSome` to advance its active Ghost count, and replaces `ghost.slot`
with the returned state. The next stage may immediately update this NPC. -/
def spawnOnPlayer (world : World) (player : Player) (random : UnifiedRandom) : Result := Id.run do
  let grid := world.grid
  let x := player.body.position.x.toInt32.toInt / 16
  let y := player.body.position.y.toInt32.toInt / 16
  let (extraX, extraY) := extraView player
  let safeX := 62 + extraX
  let safeY := 39 + extraY
  let safe := centered x y safeX safeY
  let area := centered x y (84 + extraX) (52 + extraY)
  let area : Rect := ⟨max 0 (min area.left grid.width), max 0 (min area.top grid.height),
    max 0 (min area.right grid.width), max 0 (min area.bottom grid.height)⟩
  let mut actions := #[]
  let mut random := random
  let mut candidate : Option (Int × Int) := none
  for round in [:200] do
    let lastRound := round == 199
    for _ in [:30] do
      let (x, afterX) := random.next area.left area.right
      let (initialY, afterY) := afterX.next area.top area.bottom
      random := afterY
      actions := actions.push (.draw area.left area.right x) |>.push (.draw area.top area.bottom initialY)
      if solid grid x initialY then continue
      if !lastRound && world.houseWall (grid.tileAt x.toNat initialY.toNat).wall then continue
      if lastRound || !safe.contains x initialY then
        for y in [initialY.toNat:area.bottom.toNat] do
          if solid grid x y then
            candidate := some (x, y)
            break
      -- A screen rejection leaves the previous candidate set for the next round.
      if let some (sx, sy) := candidate then
        if !lastRound then
          actions := actions.push (.space sx sy)
          if !hasTileSpawnSpace grid sx sy then
            candidate := none
            continue
        break
    if let some (sx, sy) := candidate then
      if !lastRound then
        actions := actions.push (.screen sx sy)
        if !notOnScreen world.players safeX safeY sx sy then continue
      let bottomX := sx * 16 + 8
      let bottomY := sy * 16
      actions := actions.push (.spawn bottomX bottomY 316 player.slot)
      let ghost := (availableSlot world).map fun slot => ({
        slot, target := player.slot, position := ⟨single (bottomX - 12), single (bottomY - 44)⟩ } : Ghost)
      return { ghost, random, actions }
  return { ghost := none, random, actions }

/-- The full source frame in the original `Images/NPC_316.xnb` (40×224). -/
def Ghost.textureFrame (ghost : Ghost) : Nat × Nat × Nat × Nat :=
  (0, 56 * ghost.frameIndex, 40, 56)

/-- The type-316 branch of `NPC.FindFrame`, after motion has set its velocity. -/
def Ghost.advanceVisual (ghost : Ghost) : Ghost :=
  let facing := if ghost.velocity.x > 0 then 1
    else if ghost.velocity.x < 0 then -1 else ghost.spriteDirection
  let ticks := ghost.frameCounter + 1
  { ghost with
    spriteDirection := facing
    rotation := ghost.velocity.x * 0.1
    frameCounter := if ticks ≥ 6 then 0 else ticks
    frameIndex := if ticks ≥ 6 then (ghost.frameIndex + 1) % 4 else ghost.frameIndex }

private def accelerate (velocity : Float32) (direction : Int)
    (limit step far opposed : Float32) : Float32 :=
  let sign := single direction
  let speed := velocity * sign
  if speed ≥ limit then velocity else
  let speed := speed + step
  let speed := if speed < -limit then speed + far
    else if speed < 0 then speed - opposed else speed
  min limit speed * sign

private def Ghost.fly (grid : Grid) (body : Body) (ghost : Ghost) (fading : Bool) : Ghost := Id.run do
  let ghostX := ghost.position.x + single ghost.width / 2
  let bottom := ghost.position.y + single ghost.height
  let ahead := (ghostX / 16).toInt32.toInt + 2 * ghost.direction
  let firstY := (bottom / 16).toInt32.toInt
  let mut down := true
  if bottom > body.position.y && ahead ≥ 0 && ahead < grid.width then
    for row in [firstY.toNat:min grid.height (firstY + 3).toNat] do
      if solid grid ahead row || (grid.tileAt ahead.toNat row).liquid.isSome then
        down := false
        break
  let vy := if down then
      if fading then min 6 (ghost.velocity.y + 0.1 - 0.05)
      else min 3 (ghost.velocity.y + 0.1)
    else if ghost.directionY < 0 && ghost.velocity.y > 0 then ghost.velocity.y - 0.1
    else ghost.velocity.y
  let vx := if ghost.direction == 0 then ghost.velocity.x
    else accelerate ghost.velocity.x ghost.direction 2 0.1 0.1 0.05
  let vy := if ghost.directionY == 0 then max (-4) vy
    else accelerate (max (-4) vy) ghost.directionY 1.5 0.04 0.05 0.03
  let velocity : Vector := ⟨vx, vy⟩
  return ({ ghost with velocity, position := ghost.position + velocity } : Ghost).advanceVisual

def Ghost.targetOutOfRange (ghost : Ghost) (body : Body) : Bool :=
  let dx := (ghost.position.x + single ghost.width / 2) - (body.position.x + single body.width / 2)
  let dy := (ghost.position.y + single ghost.height / 2) - (body.position.y + single body.height / 2)
  (dx * dx + dy * dy).sqrt > 3000

/-- Tombstone ghosts fade when their target dies or leaves their pursuit range.
This is the dry `ai[3] = 1` branch created by `SpawnOnPlayer`. -/
def Ghost.advanceFade (grid : Grid) (body : Body) (ghost : Ghost) (zeroVelocityRoll : Int) : Ghost :=
  let opacity := 1 - single ghost.alpha / 255 - 0.025
  let alpha := (min (255 : Float32) ((1 - opacity) * 255)).toUInt32.toNat
  let vx := if ghost.velocity.x != 0 then ghost.velocity.x else single zeroVelocityRoll * 1.5
  let ghost := { ghost with
    alpha, active := alpha < 255, localAI := ghost.localAI.set! 3 1
    velocity := ⟨vx, ghost.velocity.y⟩
    ai := if ghost.justHit then ghost.ai.set! 2 0 else ghost.ai }
  ghost.fly grid body true

/-- Normal dry pursuit and drawing update for an active type-316 Ghost with a
living target within 3000 pixels. Generic NPC lifetime, combat and the
fading/despawn branch are handled by the caller. -/
def Ghost.advancePursuit (grid : Grid) (body : Body) (ghost : Ghost)
    (justHit : Bool := false) : Ghost := Id.run do
  if !ghost.active then return ghost
  let ghostX := ghost.position.x + single ghost.width / 2
  let playerX := body.position.x + single body.width / 2
  let direction := if ghostX < playerX then 1 else -1
  let timer : Float32 := if justHit then 0 else ghost.ai[2]!
  let mut ax := ghost.ai[0]!
  let mut ay := ghost.ai[1]!
  let mut timer := timer
  let mut vx := ghost.velocity.x
  let mut direction := direction
  let mut directionY := ghost.directionY
  if timer ≥ 0 then
    let nearX := ax - 16 < ghost.position.x && ghost.position.x < ax + 16
    let opposed := (vx < 0 && ghost.direction > 0) || (vx > 0 && ghost.direction < 0)
    let nearY := ay - 40 < ghost.position.y && ghost.position.y < ay + 40
    if (nearX || opposed) && nearY then
      timer := timer + 1
      if timer ≥ 60 then
        timer := -200
        vx := -vx
    else
      ax := ghost.position.x
      ay := ghost.position.y
      timer := 0
    let ghostY := ghost.position.y + single ghost.height / 2
    let playerY := body.position.y + single body.height / 2
    directionY := if ghostY < playerY then 1 else -1
  else
    timer := timer + 1
    direction := -direction
  let ai := ((ghost.ai.set! 0 ax).set! 1 ay).set! 2 timer
  let ghost := { ghost with direction, directionY, velocity := ⟨vx, ghost.velocity.y⟩, ai }
  return ghost.fly grid body false

end Lean4Terria.OriginalGhostSpawn
