import Lean4Terria.OriginalForestCritter

/-!
# Forest snail (Terraria 1.4.5.8, Classic): `type = 359`, `aiStyle = 67`

A hand transcription of what `NPC.UpdateNPC(i)` does for one natural Snail in a single-player world
(`Main.netMode = 0`), checked frame by frame against vanilla on the snail of the continuous seed-42 Guide run
(born in frame 3832 into slot 11, bottom (32552, 5648), frames 3828 … 3980) and on two arena samples
(`.lake/original-forest-snail/1.4.5.8`).

IL read: `NPC.AI` (the `aiStyle == 67` block, `IL_32fde` … `IL_3398c`), `NPC.UpdateNPC`,
`UpdateNPC_UpdateGravity`, `TargetClosest` (`TryTrackingTarget`, `SetTargetTrackingValues`),
`SubAI_HandleTemporaryCatchableNPCPlayerInvulnerability`, `GetHurtByOtherNPCs` / `GetMeleeCollisionData`,
`UpdateCollision` with `Collision_WaterCollision` (the style-67 skip), `Collision_DecideFallThroughPlatforms`,
`ApplyTileCollision`, `Collision_MoveWhileDry` and `Collision_MoveSnailOnSlopes`, the snail case of `FindFrame`
and `CheckActive`. `TileCollision`, `SlopeCollision` and `WalkDownSlope` are the shared `OriginalCollision`
port; the stream, world, player and other-slot inputs are `OriginalForestCritter`'s.

## What the snail does

`ai[3]` is its scale: rolled once as `Next(80, 111) * 0.01` on its first update, then every update sets
`scale = ai[3]` and resizes the 12 × 12 hitbox to `(int)(12 * scale)`, keeping the bottom and moving `x` by
`width / 2 - new - 2`. It crawls at 0.3 px per frame along floors, walls and ceilings with `noGravity`:
`(direction, directionY)` is the crawl direction, `ai[1]` says whether it follows a horizontal (0) or vertical
(1) surface, `ai[0] = 2` marks "touching". `rotation` eases by 0.1 per frame toward 0 / 1.57 / 3.14 / 4.71 for
floor / left wall / ceiling / right wall. While `ai[2] == 0` it rolls `Next(7200)` every update (1 draw per
frame); a 0, or 6 updates in a row without touching anything, or a slope that moves it in
`Collision_MoveSnailOnSlopes`, sets `ai[2] = 2`: gravity on, spinning, until it has touched the floor twice.

## Stage contract

As `OriginalForestCritter.update`: one `NPC.UpdateNPC(i)` inside the NPC stage, on the shared `"UpdateNPCs"`
stream as the lower slots left it; the returned stream goes to the next slot. Tiles are read by coordinate.

## Not transcribed (the step reports `unsupported` instead of guessing)

Shimmer under the snail (`GetShimmered`), the lit snails 360 / 655 and every other type, water / lava / honey
(a snail never gets `wet`: style 67 skips `WetCollision`, but lava next to it stops the step), buffs,
`life < lifeMax`, hostile contact (`BeHurtByOtherNPC`) and the enlarged melee boxes of NPC types ≥ 400,
pressure plates and mines, players talking, `releaseOwner`, statues, multiplayer smoothing, the bees flags.
-/

namespace Lean4Terria.OriginalForestSnail

open OriginalCollision (Grid Vector single tileIndex slopeId)
open OriginalWorld (Tile TileShape LiquidKind)
open OriginalForestCritter (Rng World Player Other Rect)

/-! ## Inputs and state -/

/-- `FindFrame`'s frame height: 1 on the dedicated server, `NPC_359.xnb` height / `npcFrameCount[359]`
= 120 / 6 = 20 on the desktop client. -/
def frameHeight (dedicatedServer : Bool) : Int := if dedicatedServer then 1 else 120 / 6

/-- The snail's fields that `UpdateNPC` reads or writes on the transcribed paths; defaults are
`NPC.NewNPC(…, 359)`. -/
structure Snail where
  whoAmI : Nat
  type : Nat := 359
  aiStyle : Nat := 67
  active : Bool := true
  position : Vector
  velocity : Vector := ⟨0, 0⟩
  oldPosition : Vector := ⟨0, 0⟩
  oldVelocity : Vector := ⟨0, 0⟩
  width : Int := 12
  height : Int := 12
  direction : Int := 0
  directionY : Int := 1
  oldDirection : Int := 0
  oldDirectionY : Int := 0
  spriteDirection : Int := -1
  target : Int := 255
  oldTarget : Int := 255
  targetRect : Int × Int × Int × Int := (0, 0, 0, 0)
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  ai2 : Float32 := 0
  ai3 : Float32 := 0
  localAI0 : Float32 := 0
  localAI1 : Float32 := 0
  localAI2 : Float32 := 0
  localAI3 : Float32 := 0
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  wetCount : Nat := 0
  lavaWet : Bool := false
  honeyWet : Bool := false
  shimmerWet : Bool := false
  life : Int := 5
  lifeMax : Int := 5
  damage : Int := 0
  friendly : Bool := true
  dontTakeDamage : Bool := false
  catchableNPCTempImmunityCounter : Int := 90
  netUpdate : Bool := true
  justHit : Bool := false
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  stairFall : Bool := false
  soundDelay : Int := 0
  immune : Array Int := Array.replicate 256 0
  frameY : Int := 0
  frameCounter : Float := 0
  timeLeft : Int := 0
  npcSlots : Float32 := 0.5
  noGravity : Bool := true
  noTileCollide : Bool := false
  rotation : Float32 := 0
  scale : Float32 := 1
  knockBackResist : Float32 := 1
  catchItem : Int := 2006
  releaseOwner : Int := 255
  spawnedFromStatue : Bool := false
  extraValue : Int := 0
  despawnEncouraged : Bool := false
  hasBuffs : Bool := false
  deriving Repr

instance : Inhabited Snail := ⟨{ whoAmI := 0, position := ⟨0, 0⟩ }⟩

def Snail.center (n : Snail) : Vector :=
  ⟨n.position.x + single (n.width.tdiv 2), n.position.y + single (n.height.tdiv 2)⟩

/-- `NPC.NewNPC(source, x, y, 359, 0, ai0, ai1, ai2, ai3, target)` into slot `whoAmI`: `Bottom = (x, y)`,
`timeLeft = (int)(activeTime * 1.25)`; no draws. -/
def spawn (world : World) (whoAmI : Nat) (x y : Int) (ai0 ai1 ai2 ai3 : Float32 := 0) (target : Int := 255) : Snail :=
  let base : Snail := { whoAmI, position := ⟨0, 0⟩ }
  { base with
    position := ⟨single (x - base.width.tdiv 2), single (y - base.height)⟩
    timeLeft := (Float.ofInt world.activeTime * 1.25).toInt64.toInt
    ai0, ai1, ai2, ai3, target }

/-- Everything a frame returns besides the snail. -/
structure Step where
  snail : Snail
  rng : Rng
  gravity : Option Float32
  nearby : Array (Nat × Float32) := #[]
  noSpawnCycle : Bool := false
  unsupported : Option String := none
  deriving Repr

/-! ## Tile helpers -/

private def trunc (f : Float32) : Int := f.toInt32.toInt

private def tileAt (g : Grid) (x y : Int) : Tile :=
  if x < 0 || y < 0 || x ≥ g.width || y ≥ g.height then default else g.tileAt x.toNat y.toNat

private def ttype (t : Tile) : UInt16 := t.id.getD 0
private def isActive (t : Tile) : Bool := t.id.isSome
private def slope (t : Tile) : UInt8 := slopeId t.shape
private def liquidAmount (t : Tile) : Nat := (t.liquid.map (·.amount.toNat)).getD 0
private def liquidIs (t : Tile) (k : LiquidKind) : Bool := (t.liquid.map (·.kind == k)).getD false

/-- `Tile.topSlope()` / `bottomSlope()` (inactive tiles keep slope 0 here). -/
private def topSlope (t : Tile) : Bool := slope t == 1 || slope t == 2
private def bottomSlope (t : Tile) : Bool := slope t == 3 || slope t == 4

private def anyTile (g : Grid) (x0 x1 y0 y1 : Int) (p : Tile → Bool) : Bool := Id.run do
  let mut x := x0
  while x ≤ x1 do
    let mut y := y0
    while y ≤ y1 do
      if p (tileAt g x y) then return true
      y := y + 1
    x := x + 1
  return false

/-! ## Pieces of `UpdateNPC` -/

/-- `UpdateNPC_UpdateGravity` for a dry snail: `(NPC.gravity, maxFallSpeed)`. -/
def gravityFor (world : World) (n : Snail) : Float32 × Float32 :=
  let width := single world.grid.width / 4200
  let width := width * width
  let depth := n.position.y / 16 - (60 + 10 * width)
  let factor := (depth.toFloat / (world.worldSurface / 6.0)).toFloat32
  let factor := if factor.toFloat < 0.25 then 0.25 else factor
  let factor := if factor > 1 then 1 else factor
  (0.3 * factor, 10)

/-- `NPC.TargetClosest(true)` with `TryTrackingTarget` / `SetTargetTrackingValues` (`noAggro` is the player's
`npcTypeNoAggro[359]`). -/
def targetClosest (players : Array Player) (n : Snail) : Except String Snail := do
  let mut n := n
  let mut found := false
  let mut best : Float32 := 0
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.dead || p.ghost || p.index ≥ 255 then continue
    let c := p.center
    let own := n.center
    let distance := (c.x - own.x).abs + (c.y - own.y).abs
    let mut score := distance - single p.aggro
    if p.noAggro && n.direction != 0 then score := score + 1000
    if !found || score < best then
      found := true
      best := score
      n := { n with target := p.index }
      if p.tankPet ≥ 0 then throw "TryTrackingTarget: player tank pet"
  if n.target < 0 || n.target ≥ 255 then n := { n with target := 0 }
  let some p := players.find? (fun p => (p.index : Int) == n.target)
    | throw "TargetClosest: target player not described"
  let rect := (trunc p.position.x, trunc p.position.y, (p.width : Int), (p.height : Int))
  n := { n with targetRect := rect }
  let mut face := true
  if p.dead then face := false
  if p.noAggro && n.direction != 0 then face := false
  if face then
    let oldValid := 0 ≤ n.oldTarget && n.oldTarget ≤ 254
    let sneaking := p.itemAnimation == 0 && p.aggro < 0
    if !(oldValid && sneaking) then
      let (rx, ry, rw, rh) := rect
      n := { n with direction := if single (rx + rw.tdiv 2) < n.position.x + single (n.width.tdiv 2) then -1 else 1 }
      n := { n with directionY := if single (ry + rh.tdiv 2) < n.position.y + single (n.height.tdiv 2) then -1 else 1 }
  if n.direction != n.oldDirection && !n.collideX && !n.collideY then n := { n with netUpdate := true }
  return n

/-- The rotation the crawl surface asks for (`IL_3370c` … `IL_33817`): `(rotation, spriteDirection)`, or none. -/
private def surfaceRotation (n : Snail) : Option (Float32 × Int) :=
  if n.directionY < 0 then
    if n.direction < 0 then
      if n.collideX then some (1.57, -1) else if n.collideY then some (3.14, 1) else none
    else
      if n.collideY then some (3.14, -1) else if n.collideX then some (4.71, 1) else none
  else
    if n.direction < 0 then
      if n.collideY then some (0, -1) else if n.collideX then some (1.57, 1) else none
    else
      if n.collideX then some (4.71, -1) else if n.collideY then some (0, 1) else none

/-- `NPC.AI` for `aiStyle == 67`, `type == 359`. -/
def ai067 (world : World) (players : Array Player) (rng : Rng) (n : Snail) : Except String (Snail × Rng) := do
  let g := world.grid
  let mut n := n
  let mut rng := rng
  -- shimmer under the centre (netMode != 1)
  let clampF (v hi : Float32) : Float32 := if v < 0 then 0 else if v > hi then hi else v
  let cx := trunc (clampF (single (trunc (n.center.x / 16))) (single g.width))
  let cy := trunc (clampF (single (trunc (n.center.y / 16))) (single g.height))
  let under := tileAt g cx cy
  if liquidIs under .shimmer && liquidAmount under > 30 then throw "AI_067: shimmer (GetShimmered)"
  -- type 359: the scale from ai[3] and the resized hitbox
  if n.ai3 != 0 then
    n := { n with scale := n.ai3 }
    let w := trunc (12 * n.scale)
    let h := trunc (12 * n.scale)
    if w != n.width then
      n := { n with position.x := n.position.x + single (n.width.tdiv 2) - single w - 2, width := w }
    if h != n.height then
      n := { n with position.y := n.position.y + single n.height - single h, height := h }
  if n.ai3 == 0 then
    let (r, rng') := rng.nextRange 80 111
    rng := rng'
    n := { n with ai3 := single r * 0.01, netUpdate := true }
  let speed : Float32 := 0.3
  if n.ai0 == 0 then
    n ← targetClosest players n
    n := { n with directionY := 1, ai0 := 1 }
    if n.direction > 0 then n := { n with spriteDirection := 1 }
  let mut leftWall := false
  if n.ai2 == 0 then
    let (r, rng') := rng.next 7200
    rng := rng'
    if r == 0 then n := { n with ai2 := 2, netUpdate := true }
  if !n.collideX && !n.collideY then
    n := { n with localAI3 := n.localAI3 + 1 }
    if n.localAI3 > 5 then n := { n with ai2 := 2, netUpdate := true }
  else
    n := { n with localAI3 := 0 }
  if n.ai2 > 0 then
    -- falling: gravity on, spinning while faster than the crawl speed, until it has touched the floor twice
    n := { n with ai1 := 0, ai0 := 1, directionY := 1 }
    if n.velocity.y > speed then n := { n with rotation := n.rotation + single n.direction * 0.1 }
    else n := { n with rotation := 0 }
    n := { n with spriteDirection := n.direction, velocity.x := speed * single n.direction, noGravity := false }
    let behind := (trunc (n.center.x + single (n.width.tdiv 2 * -n.direction))).tdiv 16
    let below := (trunc (n.position.y + single n.height + 8)).tdiv 16
    if !topSlope (tileAt g behind below) && n.collideY then n := { n with ai2 := n.ai2 - 1 }
    let foot := (trunc (n.position.y + single n.height - 4)).tdiv 16
    let ahead := (trunc (n.center.x + single (n.width.tdiv 2 * n.direction))).tdiv 16
    if bottomSlope (tileAt g ahead foot) then n := { n with direction := n.direction * -1 }
    if n.collideX && n.velocity.y == 0 then
      leftWall := true
      n := { n with ai2 := 0, directionY := -1, ai1 := 1 }
    if n.velocity.y == 0 then
      if n.localAI1 == n.position.x then
        n := { n with localAI2 := n.localAI2 + 1 }
        if n.localAI2 > 10 then
          n := { n with direction := 1 }
          n := { n with velocity.x := single n.direction * speed, localAI2 := 0 }
      else
        n := { n with localAI2 := 0, localAI1 := n.position.x }
  if n.ai2 != 0 then return (n, rng)
  -- crawling
  n := { n with noGravity := true }
  if n.ai1 == 0 then
    if n.collideY then n := { n with ai0 := 2 }
    if !n.collideY && n.ai0 == 2 then n := { n with direction := -n.direction, ai1 := 1, ai0 := 1 }
    if n.collideX then n := { n with directionY := -n.directionY, ai1 := 1 }
  else
    if n.collideX then n := { n with ai0 := 2 }
    if !n.collideX && n.ai0 == 2 then n := { n with directionY := -n.directionY, ai1 := 0, ai0 := 1 }
    if n.collideY then n := { n with direction := -n.direction, ai1 := 0 }
  if !leftWall then
    let before := n.rotation
    let target ← match surfaceRotation n with
      | some (r, s) => do n := { n with spriteDirection := s }; pure r
      | none => pure before
    n := { n with rotation := before }
    if n.rotation.toFloat > 6.28 then n := { n with rotation := n.rotation - 6.28 }
    if n.rotation < 0 then n := { n with rotation := n.rotation + 6.28 }
    let gap := (n.rotation - target).abs
    let step : Float32 := 0.1
    if n.rotation > target then
      if gap.toFloat > 3.14 then n := { n with rotation := n.rotation + step }
      else
        n := { n with rotation := n.rotation - step }
        if n.rotation < target then n := { n with rotation := target }
    if n.rotation < target then
      if gap.toFloat > 3.14 then n := { n with rotation := n.rotation - step }
      else
        n := { n with rotation := n.rotation + step }
        if n.rotation > target then n := { n with rotation := target }
  n := { n with velocity := ⟨speed * single n.direction, speed * single n.directionY⟩ }
  return (n, rng)

/-- `NPC.UpdateCollision` for `aiStyle == 67`: slopes walked down, no wet test (style 67 clears `wetCount`
and skips `WetCollision`), tile collision, `Collision_MoveWhileDry`, then `Collision_MoveSnailOnSlopes` and
return. -/
def updateCollision (world : World) (gravity : Float32) (n : Snail) : Except String Snail := do
  let g := world.grid
  let mut n := n
  n := { n with velocity := OriginalCollision.walkDownSlope g n.position n.velocity n.width.toNat n.height.toNat gravity }
  -- Collision_LavaCollision: only an empty answer is transcribed (the box is a superset of its scan)
  let x0 := trunc (n.position.x / 16) - 1
  let x1 := trunc ((n.position.x + single n.width) / 16) + 2
  let y0 := trunc (n.position.y / 16) - 1
  let y1 := trunc ((n.position.y + single n.height) / 16) + 2
  if anyTile g x0 x1 y0 y1 (fun t => liquidAmount t > 0 && liquidIs t .lava) then
    throw "Collision_LavaCollision: lava next to the snail"
  if n.wet then throw "a wet snail"
  n := { n with wetCount := 0, lavaWet := false, honeyWet := false, shimmerWet := false }
  -- Collision_DecideFallThroughPlatforms is false for the snail
  n := { n with oldVelocity := n.velocity, collideX := false, collideY := false }
  let hit := OriginalCollision.tileCollision g n.position n.velocity n.width.toNat n.height.toNat
    { fallThrough := false, fall2 := false, hoik := true }
  n := { n with velocity := hit.displacement }
  -- Collision_MoveWhileDry
  if hit.hitAbove then n := { n with velocity.y := 0.01 }
  if n.oldVelocity.x != n.velocity.x then n := { n with collideX := true }
  if n.oldVelocity.y != n.velocity.y then n := { n with collideY := true }
  n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  -- Collision_MoveSnailOnSlopes
  let s := OriginalCollision.slopeCollision g n.position n.velocity n.width.toNat n.height.toNat
  if n.position.x != s.position.x || n.position.y != s.position.y then
    if n.ai2 == 0 && n.velocity.y > 0 then
      let flip := (n.direction < 0 && n.rotation == 1.57 && n.spriteDirection == 1) ||
        (n.direction > 0 && n.rotation == 4.71 && n.spriteDirection == -1)
      if flip then n := { n with direction := n.direction * -n.direction }
    n := { n with ai2 := 2, directionY := 1, rotation := 0 }
  n := { n with position := s.position, velocity := s.velocity }
  return n

/-- The snail case of `NPC.FindFrame`. -/
def findFrame (frameHeight : Int) (n : Snail) : Snail := Id.run do
  let mut n := n
  if n.velocity.y > 1 then
    n := { n with frameCounter := n.frameCounter + 1 }
    if n.frameCounter < 6 then n := { n with frameY := frameHeight * 4 }
    else
      n := { n with frameY := frameHeight * 5 }
      if !(n.frameCounter < 11) then n := { n with frameCounter := 0 }
  else if n.velocity.x != 0 || n.velocity.y != 0 then
    n := { n with frameCounter := n.frameCounter + 1 }
    let fc := n.frameCounter
    if fc < 10 then n := { n with frameY := 0 }
    else if fc < 20 then n := { n with frameY := frameHeight }
    else if fc < 30 then n := { n with frameY := frameHeight * 2 }
    else if fc < 40 then n := { n with frameY := frameHeight * 3 }
    else if fc < 50 then n := { n with frameY := frameHeight * 2 }
    else
      n := { n with frameY := frameHeight }
      if !(fc < 59) then n := { n with frameCounter := 0 }
  return n

/-- `NPC.CheckActive` (as for the bunny; `npcSlots = 0.5`). -/
def checkActive (world : World) (players : Array Player) (n : Snail) :
    Except String (Snail × Array (Nat × Float32) × Bool) := do
  let mut n := n
  let cx := n.position.x + single (n.width.tdiv 2)
  let cy := n.position.y + single (n.height.tdiv 2)
  let range : Rect := ⟨trunc (cx - single world.activeRangeX), trunc (cy - single world.activeRangeY),
    world.activeRangeX * 2, world.activeRangeY * 2⟩
  let screen : Rect := ⟨(cx.toFloat - Float.ofInt world.sWidth * 0.5 - Float.ofInt n.width).toInt64.toInt,
    (cy.toFloat - Float.ofInt world.sHeight * 0.5 - Float.ofInt n.height).toInt64.toInt,
    world.sWidth + n.width * 2, world.sHeight + n.height * 2⟩
  let mut keep := false
  let mut nearby := #[]
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.index ≥ 255 then continue
    let box : Rect := ⟨trunc p.position.x, trunc p.position.y, p.width, p.height⟩
    if range.intersects box then
      keep := true
      if n.releaseOwner == 255 && n.lifeMax > 0 then
        if world.slimeRain then throw "CheckActive: slime rain"
        nearby := nearby.push (p.index, n.npcSlots)
    if screen.intersects box then
      n := { n with timeLeft := world.activeTime, despawnEncouraged := false }
  n := { n with timeLeft := n.timeLeft - 1 }
  if n.timeLeft ≤ 0 then keep := false
  if !keep then
    if n.extraValue > 0 then throw "CheckActive: extraValue"
    n := { n with active := false }
  return (n, nearby, !keep)

/-- One `NPC.UpdateNPC(i)` for the snail, failing on paths this port does not transcribe. -/
def updateCore (world : World) (dedicatedServer : Bool) (players : Array Player) (others : Array Other)
    (rng : Rng) (n : Snail) : Except String Step := do
  if !n.active then return { snail := n, rng, gravity := none }
  if n.type != 359 then throw s!"type {n.type}"
  if n.hasBuffs then throw "buffs"
  if n.releaseOwner != 255 || n.spawnedFromStatue then throw "released or statue snail"
  if n.wet then throw "a wet snail"
  let (gravity, maxFall) := gravityFor world n
  let mut n := n
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  if n.life ≤ 0 then throw "life ≤ 0"
  n := { n with oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY }
  let ease := 1 + n.velocity.x.abs / 3
  if n.gfxOffY > 0 then
    n := { n with gfxOffY := n.gfxOffY - ease * n.stepSpeed }
    if n.gfxOffY < 0 then n := { n with gfxOffY := 0 }
  else if n.gfxOffY < 0 then
    n := { n with gfxOffY := n.gfxOffY + ease * n.stepSpeed }
    if n.gfxOffY > 0 then n := { n with gfxOffY := 0 }
  if n.gfxOffY > 16 then n := { n with gfxOffY := 16 }
  if n.gfxOffY < -16 then n := { n with gfxOffY := -16 }
  let (n', rng) ← ai067 world players rng n
  n := n'
  -- SubAI_HandleTemporaryCatchableNPCPlayerInvulnerability
  let wasFriendly := n.friendly
  if n.catchableNPCTempImmunityCounter > 0 then
    n := { n with catchableNPCTempImmunityCounter := n.catchableNPCTempImmunityCounter - 1, friendly := true }
  else
    n := { n with friendly := false }
  if n.friendly != wasFriendly then n := { n with netUpdate := true }
  if n.extraValue > 0 then throw "extraValue"
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  if !n.noGravity then
    n := { n with velocity.y := n.velocity.y + gravity }
    if n.velocity.y > maxFall then n := { n with velocity.y := maxFall }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  -- friendly, or NPCID.Sets.TakesDamageFromHostilesWithoutBeingFriendly[359]
  if n.life < n.lifeMax then throw "CheckLifeRegen: life < lifeMax"
  if n.immune[255]! == 0 && !n.dontTakeDamage then
    let own : Rect := ⟨trunc n.position.x, trunc n.position.y, n.width, n.height⟩
    for o in others do
      if o.index == n.whoAmI || o.friendly || o.damage ≤ 0 || world.critterThatCanTurnOnPlayers o.type then continue
      if o.type ≥ 400 then throw s!"GetMeleeCollisionData: hostile type {o.type}"
      let box : Rect := ⟨trunc o.position.x, trunc o.position.y, o.width, o.height⟩
      if own.intersects box then throw "GetHurtByOtherNPCs: hostile contact"
  if world.beesActive then throw "npcsFoundForCheckActive[210/211]"
  n ← updateCollision world gravity n
  -- Collision.SwitchTiles (lifeMax > 1): only an empty answer is transcribed
  let sx := trunc (n.position.x / 16)
  let sy := trunc (n.position.y / 16)
  if anyTile world.grid (sx - 1) (trunc ((n.position.x + single n.width) / 16) + 2) (sy - 1)
      (trunc ((n.position.y + single n.height) / 16) + 2)
      (fun t => isActive t && (ttype t == 135 || ttype t == 210 || ttype t == 442 || ttype t == 443)) then
    throw "SwitchTiles: a pressure plate or mine next to the snail"
  n := findFrame (frameHeight dedicatedServer) n
  let (n', nearby, despawned) ← checkActive world players n
  n := { n' with netUpdate := false, justHit := false }
  return { snail := n, rng, gravity := some gravity, nearby, noSpawnCycle := despawned }

/-- One `NPC.UpdateNPC(i)` for the snail. On a path this port does not transcribe, `unsupported` names it and
the snail and stream are returned unchanged. -/
def update (world : World) (dedicatedServer : Bool) (players : Array Player) (others : Array Other)
    (rng : Rng) (n : Snail) : Step :=
  match updateCore world dedicatedServer players others rng n with
  | .ok step => step
  | .error why => { snail := n, rng, gravity := none, unsupported := some why }

end Lean4Terria.OriginalForestSnail
