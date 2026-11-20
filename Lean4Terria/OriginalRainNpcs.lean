import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalNightEnemies

/-!
# Rain NPCs (Terraria desktop 1.4.5.8 Classic)

Rain Zombie (type 223, net IDs 223 / −54 small / −55 big), Flying Fish (224) and Umbrella Slime (225):
birth (`NPC.NewNPC` → `SetDefaults` / `SetDefaultsFromNetId`), one `NPC.UpdateNPC(i)` (AI, gravity, tile
collision, `FindFrame`, `CheckActive`) in dry terrain, and their `HitEffect` / drop-rule data.

The natural selection of these NPCs belongs to the released spawn modules (`OriginalSurfaceNightSpawn`
IL_b37a for 223/−54/−55 at night, `OriginalForestSpawn` IL_a88e..a921 for 224/225 by day); this module
takes over at their `NewNPC`.

Translation notes (from the shipped IL, `.lake/original-rain-npcs/il`):
* `AI_003_Fighters` for type 223 reaches exactly the calls and data-dependent branches of type 3 (the
  released `OriginalNightEnemies` zombie); the only type-223 difference is the speed base 1.05 (IL_7b42).
  The zombie helpers below are that released translation, re-stated here because they are private there.
* aiStyle 44 (`NPC.AI` IL_25aaa..IL_26867) for type 224 draws no random numbers.
* `AI_001_Slimes` for type 225 reaches a subset of the Green Slime path: no body item, no colour.
* `FindFrame`: 223 is the zombie branch (48-pixel rows); 224 IL_10c8d (4 rows, 6 ticks, rotation 0.2·vy);
  225 the slime branch (2 rows).
-/
namespace Lean4Terria.OriginalRainNpcs

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Rng Slime)
/-- (`OriginalNightEnemies` brings a root `World` / `Player` into scope; these are the Green Slime ones.) -/
abbrev World := OriginalGreenSlime.World
abbrev Player := OriginalGreenSlime.Player

inductive Kind where
  | rainZombie | flyingFish | umbrellaSlime
  deriving BEq, Repr

structure Npc where
  kind : Kind
  netId : Int
  body : Slime
  noGravity : Bool := false
  deriving Repr

/-! ## Birth -/

private structure Base where
  kind : Kind
  npcType : Int
  width : Nat
  height : Nat
  life : Int
  damage : Int
  defense : Int
  knockBackResist : Float32
  value : Float32
  frameWidth : Nat
  frameHeight : Int

private def base? : Int → Option Base
  | 223 => some ⟨.rainZombie, 223, 18, 40, 50, 16, 8, 0.45, 70, 34, 48⟩
  | 224 => some ⟨.flyingFish, 224, 32, 18, 20, 9, 4, 0.8, 300, 40, 32⟩
  | 225 => some ⟨.umbrellaSlime, 225, 38, 26, 35, 10, 5, 0.75, 200, 48, 60⟩
  | _ => none

/-- `SetDefaultsFromNetId` IL_1e44 / IL_1ee1: −54 and −55 are type 223 at scale 0.9 / 1.1. -/
private def netScale? : Int → Option (Int × Float32)
  | -54 => some (223, 0.9)
  | -55 => some (223, 1.1)
  | 223 => some (223, 1)
  | 224 => some (224, 1)
  | 225 => some (225, 1)
  | _ => none

/-- `NPC.NewNPC(source, x, y, netId)` for a dry spot: `SetDefaults`, `Bottom = (x, y)`,
`timeLeft = (int)(activeTime × 1.25)` (937), `WetCollision`. The caller allocates the slot. -/
def birth (grid : Grid) (netId x y : Int) (activeTime : Int := 750) : Option Npc := do
  let (t, scale) ← netScale? netId
  let b ← base? t
  let scaled (v : Nat) : Nat := if scale == 1 then v else (single v * scale).toInt32.toInt.toNat
  let width := scaled b.width
  let height := scaled b.height
  let byScale (v : Int) : Int := if scale == 1 then v else (single v * scale).toInt32.toInt
  let value : Float32 := if scale == 1 then b.value else single (b.value * scale).toInt32.toInt
  let position : Vector := ⟨single x - single width / 2, single (y - height)⟩
  let wet := OriginalPlayerWet.wetCollision grid position width height
  return {
    kind := b.kind, netId
    body := {
      position, width, height, scale, npcSlots := scale, netId
      life := byScale b.life, lifeMax := byScale b.life, damage := byScale b.damage, defDamage := byScale b.damage
      defense := byScale b.defense, defDefense := byScale b.defense
      knockBackResist := if scale == 1 then b.knockBackResist else b.knockBackResist * (2 - scale)
      value, alpha := 0, color := ⟨0, 0, 0, 0⟩, wet := wet.wet
      timeLeft := (Float.ofInt activeTime * 1.25).toInt64.toInt, frameY := 0, frameCounter := 0 } }

/-- Frame rectangle width/height `SetDefaults` leaves (texture width, height / frame count). -/
def frameSize (kind : Kind) : Nat × Int :=
  match kind with
  | .rainZombie => (34, 48)
  | .flyingFish => (40, 32)
  | .umbrellaSlime => (48, 60)

/-! ## Shared tile helpers (as in the released zombie translation) -/

private def tileAt (grid : Grid) (x y : Int) : OriginalWorld.Tile :=
  if x < 0 || y < 0 || x ≥ (grid.width : Int) || y ≥ (grid.height : Int) then {}
  else grid.tileAt x.toNat y.toNat

private def solidTile (grid : Grid) (t : OriginalWorld.Tile) (allowSolidTop : Bool := false) : Bool :=
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    rule.solid && (allowSolidTop || !rule.solidTop))

private def topSlope (t : OriginalWorld.Tile) : Bool :=
  t.shape == .slope 1 || t.shape == .slope 2

private def solidNoPlatforms (grid : Grid) (x y : Int) : Bool :=
  let t := tileAt grid x y
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    (rule.solid || rule.solidTop) && !rule.platform)

private def solidAllowBottomSlope (grid : Grid) (x y : Int) : Bool :=
  let t := tileAt grid x y
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    let frame := ((t.frame.map Prod.fst).getD (-18)).tdiv 18
    let platformTop := rule.platform &&
      ((0 ≤ frame && frame ≤ 7) || (12 ≤ frame && frame ≤ 16) || (25 ≤ frame && frame ≤ 26))
    (rule.solid || rule.solidTop) && t.shape != .halfBlock && (!topSlope t || platformTop))

/-! ## Rain Zombie (AI_003_Fighters, type 223) -/

private def grounded (grid : Grid) (n : Slime) : Bool := Id.run do
  if n.velocity.y != 0 then return false
  let left := tileIndex n.position.x
  let right := tileIndex (n.position.x + single n.width)
  let first := tileIndex (n.position.x + 8)
  let last := tileIndex (n.position.x + single n.width - 8)
  let feet := tileIndex (n.position.y + single n.height + 7)
  let head := tileIndex (n.position.y - 9)
  let mut supported := false
  for x in [first.toNat:last.toNat + 1] do
    if solidNoPlatforms grid x head then return false
    if left ≤ (x : Int) && (x : Int) ≤ right then
      let tile := tileAt grid x feet
      if !tile.actuated && (tile.id.any fun id => (grid.rules id).solid) then supported := true
  return supported

private def trackZombie (players : Array Player) (n : Slime) : Slime :=
  let n := OriginalGreenSlime.targetClosest players n
  let target := OriginalGreenSlime.playerAt players n.target
  if n.directionY > 0 && target.center.y ≤ n.position.y + single n.height then
    { n with directionY := -1 }
  else n

private def zombieStuck (players : Array Player) (n : Slime) : Slime :=
  let vx := n.velocity.x
  let reversed := n.velocity.y == 0 &&
    ((vx > 0 && n.direction < 0) || (vx < 0 && n.direction > 0))
  let counter := if n.position.x == n.oldPosition.x || n.ai3 ≥ 60 || reversed then n.ai3 + 1
    else if vx.abs.toFloat > 0.9 && n.ai3 > 0 then n.ai3 - 1 else n.ai3
  let p := OriginalGreenSlime.playerAt players n.target
  let x := n.position.x.toInt32.toInt
  let y := n.position.y.toInt32.toInt
  let px := p.position.x.toInt32.toInt
  let py := p.position.y.toInt32.toInt
  let overlaps := x < px + p.width && px < x + n.width && y < py + p.height && py < y + n.height
  { n with ai3 := if counter > 600 || n.justHit || overlaps then 0 else counter }

private def seekZombie (world : World) (players : Array Player) (n : Slime) : Slime :=
  let target := OriginalGreenSlime.playerAt players n.target
  let n := if target.position.y + single target.height == n.position.y + single n.height then
    { n with directionY := -1 } else n
  if n.ai3 < 60 && (!world.dayTime || n.position.y.toFloat > world.worldSurface * 16) then trackZombie players n
  else
    let n := if world.dayTime && (n.position.y / 16).toFloat < world.worldSurface then
      { n with timeLeft := min n.timeLeft 10, despawnEncouraged := true } else n
    let n := if n.velocity.x != 0 then { n with ai0 := 0 }
      else if n.velocity.y != 0 then n
      else if n.ai0 + 1 < 2 then { n with ai0 := n.ai0 + 1 }
      else { n with direction := -n.direction, spriteDirection := -n.direction, ai0 := 0 }
    if n.direction == 0 then { n with direction := 1 } else n

/-- IL_7b3c: type 223 runs at base 1.05 (scaled by `1 + (1 − scale)`). -/
def speedBase : Float32 := 1.05

private def zombieAcceleration (n : Slime) : Slime :=
  let cap := speedBase * (1 + (1 - n.scale))
  let vx := n.velocity.x
  if vx.abs > cap then
    if n.velocity.y == 0 then { n with velocity := ⟨vx * 0.8, n.velocity.y * 0.8⟩ } else n
  else if n.direction == 1 && vx < cap then { n with velocity.x := min cap (vx + 0.07) }
  else if n.direction == -1 && vx > -cap then { n with velocity.x := max (-cap) (vx - 0.07) }
  else n

private def zombieStepUp (grid : Grid) (n : Slime) : Slime := Id.run do
  if n.velocity.y < 0 then return n
  let direction : Int := if n.velocity.x < 0 then -1 else if n.velocity.x > 0 then 1 else 0
  let nextX := n.position.x + n.velocity.x
  let half : Int := n.width / 2
  let x := tileIndex (nextX + single half + single ((half + 1) * direction))
  let bottom := n.position.y + single n.height
  let y := tileIndex (bottom - 1)
  if x < 4 || y < 4 || x ≥ (grid.width : Int) - 4 || y ≥ (grid.height : Int) - 4 then return n
  let left := single (x * 16)
  if left ≥ nextX + single n.width || left + 16 ≤ nextX then return n
  let atY := tileAt grid x
  let ahead := atY y
  let above := atY (y - 1)
  let upperHalf := above.id.isSome && !above.actuated && above.shape == .halfBlock
  let ledge := solidTile grid ahead && !topSlope ahead && !topSlope above
  if !ledge && !upperHalf then return n
  if solidTile grid above && (!upperHalf || solidTile grid (atY (y - 4))) then return n
  if solidTile grid (atY (y - 2)) || solidTile grid (atY (y - 3)) ||
      solidTile grid (tileAt grid (x - direction) (y - 3)) true then return n
  let surface : Float32 := single (y * 16) + (if ahead.shape == .halfBlock then 8 else 0) -
    (if above.shape == .halfBlock then 8 else 0)
  let rise := bottom - surface
  if rise ≤ 0 || rise > 16.1 then return n
  return { n with
    position.y := surface - single n.height
    gfxOffY := n.gfxOffY + rise
    stepSpeed := if rise < 9 then 1 else 2 }

private def doorAhead (grid : Grid) (n : Slime) : Bool :=
  let x := tileIndex (n.position.x + single (n.width / 2) + single (15 * n.direction))
  let y := tileIndex (n.position.y + single n.height - 15) - 1
  let ahead := tileAt grid x y
  !ahead.actuated && (ahead.id == some 10 || ahead.id == some 388)

private def zombieJump (grid : Grid) (n : Slime) : Slime := Id.run do
  let x := tileIndex (n.position.x + single (n.width / 2) + single (15 * n.direction))
  let bottom := n.position.y + single n.height
  let y := tileIndex (bottom - 15)
  unless (n.spriteDirection == -1 && n.velocity.x < 0) ||
      (n.spriteDirection == 1 && n.velocity.x > 0) do return n
  if n.height ≥ 32 && solidNoPlatforms grid x (y - 2) then
    return { n with velocity.y := if solidNoPlatforms grid x (y - 3) then -8 else -7, netUpdate := true }
  if solidNoPlatforms grid x (y - 1) then
    return { n with velocity.y := -6, netUpdate := true }
  if bottom - single (y * 16) > 20 && !topSlope (tileAt grid x y) && solidNoPlatforms grid x y then
    return { n with velocity.y := -5, netUpdate := true }
  if n.directionY < 0 && !solidAllowBottomSlope grid x (y + 1) &&
      !solidAllowBottomSlope grid (x + n.direction) (y + 1) then
    return { n with velocity := ⟨n.velocity.x * 1.5, -8⟩, netUpdate := true }
  return { n with ai1 := 0, ai2 := 0 }

private def zombieFrame (n : Slime) : Slime :=
  let vx := n.velocity.x
  let sprite := if n.velocity.y == 0 && (n.direction == 1 || n.direction == -1) then n.direction else n.spriteDirection
  if n.velocity.y != 0 || (n.direction == 1 && vx < 0) || (n.direction == -1 && vx > 0) then
    { n with spriteDirection := sprite, frameY := 96, frameCounter := 0 }
  else if vx == 0 then
    { n with spriteDirection := sprite, frameY := 0, frameCounter := 0 }
  else
    let counter := n.frameCounter + vx.abs.toFloat
    if counter ≥ 32 then { n with spriteDirection := sprite, frameCounter := 0 }
    else
      let row := if counter < 8 then 0 else if counter < 16 then 1 else if counter < 24 then 2 else 1
      { n with spriteDirection := sprite, frameY := row * 48, frameCounter := counter }

/-! ## Flying Fish (aiStyle 44, type 224) -/

/-- `Collision.CanHit(center, 1, 1, playerCenter, 1, 1)`. -/
private def canHitCenters (grid : Grid) (a b : Vector) : Bool :=
  let (x1, y1) := OriginalStarterMelee.LineOfSight.toPoint a
  let (x2, y2) := OriginalStarterMelee.LineOfSight.toPoint b
  OriginalStarterMelee.LineOfSight.canHitPoints grid x1 y1 1 1 x2 y2 1 1

/-- `NPC.AI` IL_25aaa..IL_26867 for type 224. `wetPlayers` holds the player indices whose `wet` is set.
`others` are the positions of the other active Flying Fish, in NPC slot order. -/
def fishAi (grid : Grid) (players : Array Player) (wetPlayers : Array Nat) (others : Array Vector) (n : Slime) : Slime := Id.run do
  let mut n := n
  if n.collideX then
    let d : Int := if n.oldVelocity.x > 0 then -1 else 1
    n := { n with direction := d, velocity.x := single d }
  if n.collideY then
    let d : Int := if n.oldVelocity.y > 0 then -1 else 1
    n := { n with directionY := d, velocity.y := single d }
  let oldTarget := n.target
  let oldDirection := n.direction
  let target := OriginalGreenSlime.playerAt players n.target
  -- IL_25e79..IL_25ed9: `target == 255`, the target's `wet`, its `dead`, or a clear line (`CanHit` true,
  -- `brfalse` to the countdown) all reset the chase timer; only a blocked line lets it run down.
  let reset := n.target == 255 || wetPlayers.contains target.index || target.dead ||
    canHitCenters grid n.center target.center
  if reset then
    n := OriginalGreenSlime.targetClosest players { n with ai0 := 90 }
  else if n.ai0 > 0 then
    n := OriginalGreenSlime.targetClosest players { n with ai0 := n.ai0 - 1 }
  if n.netUpdate && oldTarget == n.target && oldDirection == n.direction then n := { n with netUpdate := false }
  let mut acc : Float32 := 0.05
  let accY : Float32 := 0.01
  let mut maxX : Float32 := 3
  let maxY : Float32 := 1
  let near : Float32 := 30
  let far : Float32 := 100
  let p := OriginalGreenSlime.playerAt players n.target
  let dx := (n.position.x + single ((n.width : Int) / 2) - (p.position.x + single ((p.width : Int) / 2))).abs
  let mut goalY := p.position.y - single ((n.height : Int) / 2)
  -- (the 509/581/587 branches do not apply to 224)
  if !(n.ai0 > 0) then
    maxX := maxX * 0.8
    acc := acc * 0.7
    goalY := n.center.y + single (n.directionY * 1000)
    if n.velocity.x < 0 then n := { n with direction := -1 }
    else if n.velocity.x > 0 || n.direction == 0 then n := { n with direction := 1 }
  if dx > near then
    if n.direction == -1 && n.velocity.x > -maxX then
      n := { n with velocity.x := n.velocity.x - acc }
      if n.velocity.x > maxX then n := { n with velocity.x := n.velocity.x - acc }
      else if n.velocity.x > 0 then n := { n with velocity.x := n.velocity.x - acc / 2 }
      if n.velocity.x < -maxX then n := { n with velocity.x := -maxX }
    else if n.direction == 1 && n.velocity.x < maxX then
      n := { n with velocity.x := n.velocity.x + acc }
      if n.velocity.x < -maxX then n := { n with velocity.x := n.velocity.x + acc }
      else if n.velocity.x < 0 then n := { n with velocity.x := n.velocity.x + acc / 2 }
      if n.velocity.x > maxX then n := { n with velocity.x := maxX }
  if dx > far then goalY := goalY - far / 2
  if n.position.y < goalY then
    n := { n with velocity.y := n.velocity.y + accY }
    if n.velocity.y < 0 then n := { n with velocity.y := n.velocity.y + accY }
  else
    n := { n with velocity.y := n.velocity.y - accY }
    if n.velocity.y > 0 then n := { n with velocity.y := n.velocity.y - accY }
  if n.velocity.y < -maxY then n := { n with velocity.y := -maxY }
  if n.velocity.y > maxY then n := { n with velocity.y := maxY }
  if n.wet then
    if n.velocity.y > 0 then n := { n with velocity.y := n.velocity.y * 0.95 }
    n := { n with velocity.y := n.velocity.y - 0.5 }
    if n.velocity.y < -4 then n := { n with velocity.y := -4 }
  let _ := others
  return n

/-- `FindFrame` IL_10c8d for type 224 (frame height 32, four rows, six ticks). -/
def fishFrame (n : Slime) : Slime := Id.run do
  let mut n := n
  if n.velocity.x > 0 then n := { n with spriteDirection := 1, rotation := n.velocity.y * 0.2 }
  if n.velocity.x < 0 then n := { n with spriteDirection := -1, rotation := -n.velocity.y * 0.2 }
  n := { n with frameCounter := n.frameCounter + 1 }
  if n.frameCounter ≥ 6 then n := { n with frameY := n.frameY + 32, frameCounter := 0 }
  if n.frameY ≥ 32 * 4 then n := { n with frameY := 0 }
  return n

/-! ## Umbrella Slime (AI_001_Slimes, type 225) -/

/-- `AI_001_Slimes` for type 225 in dry terrain: the Green Slime path without the body item and colour. -/
def slimeAi (world : World) (players : Array Player) (n : Slime) : Except String Slime := do
  let grid := world.grid
  let mut n := n
  if n.direction == 0 then n := { n with direction := 1, netUpdate := true }
  let aggressive := !(world.dayTime && n.life == n.lifeMax &&
    !(n.position.y.toFloat > world.worldSurface * 16)) || world.slimeRain
  if n.ai2 > 1 then n := { n with ai2 := n.ai2 - 1 }
  if n.wet then throw "wet Umbrella Slime"
  n := { n with aiAction := 0 }
  if n.ai2 == 0 then
    n := { n with ai0 := -100, ai2 := 1 }
    n := OriginalGreenSlime.targetClosest players n
  if n.velocity.y == 0 then
    if n.collideY && n.oldVelocity.y != 0 && OriginalGreenSlime.solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if n.ai3 == n.position.x then
      n := { n with direction := -n.direction, ai2 := 200 }
    n := { n with ai3 := 0 }
    n := { n with velocity.x := n.velocity.x * 0.8 }
    if n.velocity.x.toFloat > -0.1 && n.velocity.x.toFloat < 0.1 then n := { n with velocity.x := 0 }
    if aggressive then n := { n with ai0 := n.ai0 + 1 }
    n := { n with ai0 := n.ai0 + 1 }
    let threshold : Float32 := -1000
    let mut jump : Nat := 0
    if n.ai0 ≥ 0 then jump := 1
    if n.ai0 ≥ threshold && n.ai0 ≤ threshold * 0.5 then jump := 2
    if n.ai0 ≥ threshold * 2 && n.ai0 ≤ threshold * 1.5 then jump := 3
    if jump > 0 then
      n := { n with netUpdate := true }
      if aggressive && n.ai2 == 1 then n := OriginalGreenSlime.targetClosest players n
      if jump == 3 then
        n := { n with velocity.y := -8, velocity.x := n.velocity.x + single (3 * n.direction), ai0 := -200, ai3 := n.position.x }
      else
        n := { n with velocity.y := -6, velocity.x := n.velocity.x + single (2 * n.direction), ai0 := -120 }
        n := { n with ai0 := if jump == 1 then n.ai0 + threshold else n.ai0 + threshold * 2 }
    else if n.ai0 ≥ -30 then
      n := { n with aiAction := 1 }
  else if n.target < 255 && ((n.direction == 1 && n.velocity.x < 3) || (n.direction == -1 && n.velocity.x > -3)) then
    if n.collideX && n.velocity.x.abs == 0.2 then
      n := { n with position.x := n.position.x - 1.4 * single n.direction }
    if n.collideY && n.oldVelocity.y != 0 && OriginalGreenSlime.solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if (n.direction == -1 && n.velocity.x.toFloat < 0.01) || (n.direction == 1 && n.velocity.x.toFloat > -0.01) then
      n := { n with velocity.x := n.velocity.x + 0.2 * single n.direction }
    else
      n := { n with velocity.x := n.velocity.x * 0.93 }
  return n

/-! ## One `NPC.UpdateNPC(i)` -/

structure Step where
  npc : Npc
  rng : Rng
  /-- `Player.nearbyActiveNPCs` increments from `CheckActive`. -/
  nearby : Array (Nat × Float32) := #[]
  /-- The zombie's `Next(1000) == 0` groan (`SoundEngine.PlaySound(14, …)`); the sound owner plays it. -/
  groan : Bool := false
  deriving Repr

private def prepare (n : Slime) : Slime :=
  let step := (1 + n.velocity.x.abs / 3) * n.stepSpeed
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - step)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + step) else n.gfxOffY
  { n with
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    gfxOffY := max (-16) (min 16 gfx) }

/-- `NPC.UpdateCollision` dry: walk-down-slopes, tile collision (`Collision_DecideFallThroughPlatforms`:
aiStyle 44 always falls through; a fighter only with `directionY == 1`), dry move, slopes/stair-fall. -/
private def move (grid : Grid) (fallThrough : Bool) (gravity : Float32) (n : Slime) : Slime :=
  let velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height gravity
  let hit := OriginalCollision.tileCollision grid n.position velocity n.width n.height
    { fallThrough, fall2 := fallThrough, hoik := true }
  let displacement := if hit.hitAbove then { hit.displacement with y := 0.01 } else hit.displacement
  let position := n.position + displacement
  let slope := OriginalCollision.slopeCollision grid position displacement n.width n.height (fallThrough || n.stairFall)
  let dy := slope.position.y - position.y
  let step := slope.stair && dy.abs > 8
  { n with
    position := slope.position + ⟨0, 0⟩, velocity := slope.velocity
    oldPosition := n.position, oldVelocity := velocity, oldDirection := n.direction
    collideX := velocity.x != displacement.x, collideY := velocity.y != displacement.y
    stairFall := fallThrough || slope.stairFall
    gfxOffY := if step then n.gfxOffY - dy else n.gfxOffY
    stepSpeed := if step then 2 else n.stepSpeed }

/-- One update of an active rain NPC in dry terrain. Carry the returned NPC and random stream
(`UpdateNPCs`) into the next slot. `wetPlayers` / `fishPositions` feed the Flying Fish. -/
def update (world : World) (players : Array Player) (rng : Rng) (npc : Npc)
    (wetPlayers : Array Nat := #[]) (fishPositions : Array Vector := #[]) : Except String Step := do
  if !npc.body.active then return { npc, rng }
  if npc.body.wet then throw "wet rain NPC (water collision not covered)"
  match npc.kind with
  | .umbrellaSlime =>
    let w := { world with frameHeight := 60, frameCount := 2 }
    let (gravity, maxFall) := OriginalGreenSlime.gravityFor w npc.body
    let n := npc.body
    if n.life ≤ 0 then
      return { npc := { npc with body := { n with active := false, netUpdate := false, justHit := false } }, rng }
    let n := prepare n
    let n ← slimeAi w players n
    let n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
    let n := { n with velocity.y := min maxFall (n.velocity.y + gravity) }
    let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then { n with velocity.x := 0 } else n
    let n := OriginalGreenSlime.updateCollision w.grid gravity n
    let n := OriginalGreenSlime.findFrame w n
    let (n, nearby) := OriginalGreenSlime.checkActive w players n
    return { npc := { npc with body := { n with netUpdate := false, justHit := false } }, rng, nearby }
  | .rainZombie =>
    let (gravity, maxFall) := OriginalGreenSlime.gravityFor world npc.body
    let n := npc.body
    if n.life ≤ 0 then
      return { npc := { npc with body := { n with active := false, netUpdate := false, justHit := false } }, rng }
    let n := prepare n
    let n := zombieStuck players n
    let pursue := n.ai3 < 60 && (!world.dayTime || n.position.y.toFloat > world.worldSurface * 16)
    let (roll, rng) := if pursue then rng.next 1000 else (1, rng)
    let n := zombieAcceleration (seekZombie world players n)
    let canJump := grounded world.grid n
    let n := zombieStepUp world.grid n
    if canJump && doorAhead world.grid n then throw "door ahead of the Rain Zombie (door knocking not covered)"
    let n := if canJump then zombieJump world.grid n else { n with ai1 := 0, ai2 := 0 }
    let n := { n with immune := n.immune.map fun value => if value > 0 then value - 1 else value }
    let n := { n with velocity.y := min maxFall (n.velocity.y + gravity) }
    let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then { n with velocity.x := 0 } else n
    let n := move world.grid (n.directionY == 1) gravity n
    let (n, nearby) := OriginalGreenSlime.checkActive world players (zombieFrame n)
    return { npc := { npc with body := { n with netUpdate := false, justHit := false } }, rng, nearby, groan := roll == 0 }
  | .flyingFish =>
    let (gravity, _) := OriginalGreenSlime.gravityFor world npc.body
    let n := npc.body
    if n.life ≤ 0 then
      return { npc := { npc with body := { n with active := false, netUpdate := false, justHit := false } }, rng }
    let n := prepare n
    let n := fishAi world.grid players wetPlayers fishPositions n
    let n := { n with immune := n.immune.map fun value => if value > 0 then value - 1 else value }
    let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then { n with velocity.x := 0 } else n
    let n := move world.grid true gravity n
    let (n, nearby) := OriginalGreenSlime.checkActive world players (fishFrame n)
    return { npc := { npc with body := { n with netUpdate := false, justHit := false }, noGravity := true }, rng, nearby }

/-! ## Strike, `HitEffect`, death and loot -/

/-- The strike view (`OriginalStarterMelee.strikeNpc`); immunity is read from `playerSlot`. -/
def Npc.toCombatNpc (npc : Npc) (playerSlot : Nat := 0) : OriginalStarterMelee.Npc :=
  let n := npc.body
  { active := n.active, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax
    defense := n.defense, knockBackResist := n.knockBackResist, noGravity := npc.noGravity
    justHit := n.justHit, immune := (n.immune[playerSlot]?.getD 0).toNat }

/-- Write a strike back. Activity is kept through `HitEffect`; the death caller clears it after loot. -/
def Npc.applyHit (npc : Npc) (hit : OriginalStarterMelee.Npc) (playerSlot : Nat := 0) : Npc :=
  { npc with body := { npc.body with
      active := hit.active, life := hit.life, velocity := hit.velocity, justHit := hit.justHit
      immune := npc.body.immune.set! playerSlot hit.immune, playerInteraction := true } }

/-- `Gore.NewGore(position, velocity, type, scale)` for the ordinary sticky fragments 309..314: slot first,
then `Next(10, 31)` (up) and `Next(-20, 21)` (side); a fragment outside the world is not kept. -/
def newGore (grid : Grid) (occupied : Array Bool) (typeId : Nat) (position velocity : Vector) (scale : Float32)
    (random : UnifiedRandom) : Array Bool × Option OriginalNightEnemies.Gore × UnifiedRandom := Id.run do
  let slot := (occupied.findIdx? (!·)).getD occupied.size
  if slot ≥ 600 then return (occupied, none, random)
  let (up, random) := random.next 10 31
  let (side, random) := random.next (-20) 21
  let x := position.x.toInt32.toInt / 16
  let y := position.y.toInt32.toInt / 16
  let active := 0 ≤ x && x < (grid.width : Int) && 0 ≤ y && y < (grid.height : Int)
  let particle : OriginalNightEnemies.Gore :=
    { slot, typeId, position, scale, active, velocity := ⟨velocity.x + single side * 0.1, velocity.y - single up * 0.1⟩ }
  let occupied := occupied ++ Array.replicate (slot + 1 - occupied.size) false
  return (occupied.set! slot active, if active then some particle else none, random)

private structure HitStyle where
  dust : Nat
  alpha : Int := 0
  color : UInt32 := 0
  /-- live hits: `i < dealt / lifeMax × factor` dust -/
  factor : Float
  deadCount : Nat
  deadSpeed : Int → Vector
  /-- `(gore type, y offset, scaled by npc.scale)` -/
  gores : Array (Nat × Float32 × Bool)

private def hitStyle : Kind → HitStyle
  -- IL_17e…: the zombie family block (3, 52, 53, …, 186…189, 200, 223, …); IL_18f71 the 223 fragments
  | .rainZombie => { dust := 5, factor := 100, deadCount := 50, deadSpeed := fun d => ⟨2.5 * single d, -2.5⟩
                     gores := #[(309, 0, true), (310, 20, true), (310, 20, true), (311, 34, true), (311, 34, true)] }
  -- IL_13434 (the shared 74/102/224/… block), IL_13b4a the 224 fragments
  | .flyingFish => { dust := 5, factor := 20, deadCount := 10, deadSpeed := fun d => ⟨single (2 * d), -2⟩
                     gores := #[(312, 0, false), (313, 0, false)] }
  -- IL_f99b
  | .umbrellaSlime => { dust := 4, alpha := 100, color := OriginalGreenSlime.Color.rgba ⟨0, 80, 255, 100⟩, factor := 100
                        deadCount := 50, deadSpeed := fun d => ⟨single (2 * d), -2⟩, gores := #[(314, 0, true)] }

/-- `NPC.HitEffect(hitDirection, dealt)` for an active rain NPC: dust through the shared pool, then (dead)
the fragments, all on one stream. -/
def hitEffects (grid : Grid) (pool : OriginalForestGuideArrow.DustPool) (goreOccupied : Array Bool) (npc : Npc)
    (direction : Int) (dealt : Float) (random : UnifiedRandom) : OriginalNightEnemies.HitEffects := Id.run do
  let n := npc.body
  let style := hitStyle npc.kind
  let mut out : OriginalNightEnemies.HitEffects := { pool, goreOccupied, random }
  if !n.active then return out
  let dead := n.life ≤ 0
  let count := if dead then style.deadCount else (dealt / Float.ofInt n.lifeMax * style.factor).ceil.toUInt64.toNat
  let speed : Vector := if dead then style.deadSpeed direction else ⟨single direction, -1⟩
  for _ in [:count] do
    let (pool, particle, random) := out.pool.create {
      position := n.position, width := n.width, height := n.height, typeId := style.dust, speed
      alpha := style.alpha, color := style.color } out.random
    out := { out with pool, random, dust := particle.elim out.dust out.dust.push }
  if !dead then return out
  for (typeId, dy, scaled) in style.gores do
    let (occupied, gore, random) := newGore grid out.goreOccupied typeId (n.position + ⟨0, dy⟩) n.velocity
      (if scaled then n.scale else 1) out.random
    out := { out with random, goreOccupied := occupied, gore := gore.elim out.gore out.gore.push }
  return out

private def push (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16) (count : Nat)
    (loot : OriginalCombat.Loot.Result) : OriginalCombat.Loot.Result :=
  let (prefixId, random) := OriginalNightEnemies.naturalPrefix item loot.random
  let (drop, random) := OriginalItems.rollDropAt position { OriginalItems.Stack.ofItem item count with prefixId } random env.grid
  { drops := loot.drops.push drop, random }

/-- `CommonDrop.TryDroppingItem` at luck 0: `RollLuck(d) < 1`, then `Next(min, max + 1)`. -/
private def common (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16) (denominator lo hi : Int)
    (loot : OriginalCombat.Loot.Result) : Bool × OriginalCombat.Loot.Result :=
  let (roll, random) := loot.random.next 0 denominator
  if roll != 0 then (false, { loot with random }) else
  let (count, random) := random.next lo (hi + 1)
  (true, push env position item count.toNat { loot with random })

/-- `NPC.NPCLoot` in an ordinary pre-Hardmode, non-seasonal, non-Skyblock world at zero luck, for an NPC
not spawned from a statue. Conditional global rules (keys, souls, seasonal, yoyos, mech summons) and the
Skyblock sickle cannot drop and draw nothing. Per-NPC rules (`ItemDropDatabase` IL, order as registered):
* 223 (−54, −55): 216 1/50, 1304 1/250, 5332 1/1500, OneFromOptions 1/20 [1135, 1136], 282 ×1..4;
* 224: 4021 1/50 (NotFromStatue), 4057 1/100, 282 ×1..4;
* 225: 1243 1/45, on a failed roll gel (23) ×2..6 (`DropBasedOnExtraGel`, Classic rule).
Then `NPCLoot_DropMoney` and `NPCLoot_DropHeals`. -/
def dropLoot (env : OriginalCombat.Loot.Context) (npc : Npc) (rng : UnifiedRandom) : Except String OriginalCombat.Loot.Result := do
  if env.luck != 0 then throw "loot at non-zero luck"
  let n := npc.body
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let mut out : OriginalCombat.Loot.Result := { random := rng }
  match npc.kind with
  | .rainZombie =>
    for (item, d) in #[(216, 50), (1304, 250), (5332, 1500)] do
      out := (common env position item d 1 1 out).2
    let (roll, random) := out.random.next 0 20
    out := { out with random }
    if roll == 0 then
      let (k, random) := out.random.next 0 2
      out := push env position (if k == 0 then 1135 else 1136) 1 { out with random }
    out := (common env position 282 1 1 4 out).2
  | .flyingFish =>
    out := (common env position 4021 50 1 1 out).2
    out := (common env position 4057 100 1 1 out).2
    out := (common env position 282 1 1 4 out).2
  | .umbrellaSlime =>
    let (umbrella, next) := common env position 1243 45 1 1 out
    out := next
    if !umbrella then out := (common env position 23 1 2 6 out).2
  out := OriginalCombat.Loot.dropMoneyAt env position n.value out
  return OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) out

/-- Contact with the local player (`Player.Update_NPCCollision` → `Hurt`): ordinary, debuff-free. -/
def contact (npc : Npc) (body : OriginalPlayerCollision.Body) (health : OriginalPlayerLiquidHealth.Health)
    (defense : Int) (pool : OriginalForestGuideArrow.DustPool) (rng : UnifiedRandom) : Option OriginalAngryBones.ContactHit :=
  OriginalAngryBones.contactNpc npc.toCombatNpc npc.body.damage body health defense pool rng

end Lean4Terria.OriginalRainNpcs
