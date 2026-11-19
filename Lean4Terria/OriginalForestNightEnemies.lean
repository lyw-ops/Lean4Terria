import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerLiquidHealth

namespace Lean4Terria.OriginalForestNightEnemies

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Player Rng Slime World)
open OriginalWorld (Tile)

inductive Family where
  | zombie | demonEye
  deriving BEq, Repr

/-- The NPC types with continuous dry Classic samples. A negative net ID keeps its
loaded hitbox and scale while sharing the AI of its underlying type. -/
def family? (npcType : Int) : Option Family :=
  if npcType == 3 || npcType == 132 || npcType == 189 then some .zombie
  else if npcType == 2 || npcType == 194 then some .demonEye
  else none

/-- The common NPC fields use the same representation as the dry Green Slime model.
Spawner defaults must be installed for the actual net ID before the first update. -/
structure Enemy where
  npcType : Int
  netId : Int
  body : Slime
  noGravity : Bool := false
  noTileCollide : Bool := false
  deriving Repr

private structure BirthDefaults where
  npcType : Int
  width : Nat
  height : Nat
  life : Int
  damage : Int
  defense : Int
  knockBackResist : Float32
  value : Float32
  scale : Float32 := 1

/-- The five replayed Classic net IDs, positioned from the pixel anchor
passed to `NPC.NewNPC`. The caller allocates the slot. -/
def spawn (netId x y : Int) : Option Enemy := do
  let d ← (match netId with
    | 3 => some ⟨3, 18, 40, 45, 14, 6, 0.5, 60, 1⟩
    | -28 => some ⟨132, 15, 34, 34, 12, 4, 0.575, 55, 0.85⟩
    | 189 => some ⟨189, 18, 40, 45, 16, 4, 0.55, 70, 1⟩
    | 2 => some ⟨2, 30, 32, 60, 18, 2, 0.8, 75, 1⟩
    | 194 => some ⟨194, 30, 32, 60, 14, 4, 0.8, 75, 1⟩
    | _ => none : Option BirthDefaults)
  return { npcType := d.npcType, netId, body :=
    { position := ⟨single x - single d.width / 2, single (y - d.height)⟩
      width := d.width, height := d.height, scale := d.scale, npcSlots := d.scale
      life := d.life, lifeMax := d.life, damage := d.damage, defDamage := d.damage
      defense := d.defense, defDefense := d.defense, knockBackResist := d.knockBackResist
      value := d.value, alpha := 0, timeLeft := 937, color := ⟨0, 0, 0, 0⟩ } }

inductive Effect where
  | zombieGroan | eyeBlood
  deriving BEq, Repr

structure Step where
  enemy : Enemy
  rng : Rng
  nearby : Array (Nat × Float32) := #[]
  effect : Option Effect := none
  deriving Repr

private def toward (v limit acceleration extra damping : Float32) (direction : Int) : Float32 :=
  if direction == 1 && v < limit then
    let next := v + acceleration
    min limit <| if next < -limit then next + extra else if next < 0 then next - damping else next
  else if direction == -1 && v > -limit then
    let next := v - acceleration
    max (-limit) <| if next > limit then next - extra else if next > 0 then next + damping else next
  else v

/-- Ordinary `AI_002_FloatingEye` during the night. Rebound uses the aim and the
unclipped velocity from the previous update. The target is refreshed afterwards. -/
private def steerEye (players : Array Player) (n : Slime) : Slime := Id.run do
  let mut n := n
  if n.collideX then
    let rebound := n.oldVelocity.x * -0.5
    n := { n with velocity.x :=
      if n.direction == -1 && 0 < rebound && rebound < 2 then 2
      else if n.direction == 1 && -2 < rebound && rebound < 0 then -2 else rebound }
  if n.collideY then
    let rebound := n.oldVelocity.y * -0.5
    n := { n with velocity.y :=
      if 0 < rebound && rebound < 1 then 1
      else if -1 < rebound && rebound < 0 then -1 else rebound }
  n := OriginalGreenSlime.targetClosest players n
  let scaleAdjustment := 1 + (1 - n.scale)
  return { n with velocity :=
    ⟨toward n.velocity.x (4 * scaleAdjustment) 0.1 0.1 0.05 n.direction,
     toward n.velocity.y (1.5 * scaleAdjustment) 0.04 0.05 0.03 n.directionY⟩ }

private def tileAt (grid : Grid) (x y : Int) : Tile :=
  if x < 0 || y < 0 || x ≥ (grid.width : Int) || y ≥ (grid.height : Int) then {}
  else grid.tileAt x.toNat y.toNat

private def solidTile (grid : Grid) (t : Tile) (allowSolidTop : Bool := false) : Bool :=
  !t.actuated && (t.id.any fun id =>
    let rule := grid.rules id
    rule.solid && (allowSolidTop || !rule.solidTop))

private def topSlope (t : Tile) : Bool :=
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

/-- The fighter tests the loaded tiles below its feet even when `collideY` is set.
A real solid tile at head height invalidates the grounded jump branch. -/
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
  { n with ai3 := if counter > 600 || overlaps then 0 else counter }

private def zombieAcceleration (npcType : Int) (n : Slime) : Slime :=
  let base : Float32 := if npcType == 189 then 0.8 else if npcType == 132 then 0.95 else 1
  let cap := base * (1 + (1 - n.scale))
  let vx := n.velocity.x
  if vx.abs > cap then
    if n.velocity.y == 0 then { n with velocity := ⟨vx * 0.8, n.velocity.y * 0.8⟩ } else n
  else if n.direction == 1 && vx < cap then { n with velocity.x := min cap (vx + 0.07) }
  else if n.direction == -1 && vx > -cap then { n with velocity.x := max (-cap) (vx - 0.07) }
  else n

/-- The inline fighter step in `AI_003` checks three rows even for a small
variant. The two leading tiles' upper slopes cannot serve as a ledge. -/
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

private def zombieJump (grid : Grid) (n : Slime) : Except String Slime := do
  let x := tileIndex (n.position.x + single (n.width / 2) + single (15 * n.direction))
  let bottom := n.position.y + single n.height
  let y := tileIndex (bottom - 15)
  let ahead := tileAt grid x (y - 1)
  if !ahead.actuated && (ahead.id.any fun id => (grid.rules id).closedDoor || id == 388) then
    throw "fighter door interaction"
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
  return n

private def prepare (n : Slime) : Slime :=
  let step := (1 + n.velocity.x.abs / 3) * n.stepSpeed
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - step)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + step) else n.gfxOffY
  { n with
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    gfxOffY := max (-16) (min 16 gfx) }

private def move (grid : Grid) (kind : Family) (gravity : Float32) (n : Slime) : Slime :=
  let fallThrough := kind == .demonEye || n.directionY == 1
  let velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height gravity
  let hit := OriginalCollision.tileCollision grid n.position velocity n.width n.height
    { fallThrough, fall2 := fallThrough, hoik := true }
  let displacement := if hit.hitAbove then { hit.displacement with y := 0.01 } else hit.displacement
  let position := n.position + displacement
  let slope := OriginalCollision.slopeCollision grid position displacement n.width n.height
    (fallThrough || n.stairFall)
  let dy := slope.position.y - position.y
  let step := slope.stair && dy.abs > 8
  { n with
    position := slope.position + ⟨0, 0⟩, velocity := slope.velocity
    oldPosition := n.position, oldVelocity := velocity, oldDirection := n.direction
    collideX := velocity.x != displacement.x, collideY := velocity.y != displacement.y
    stairFall := fallThrough || slope.stairFall
    gfxOffY := if step then n.gfxOffY - dy else n.gfxOffY
    stepSpeed := if step then 2 else n.stepSpeed }

private def animation (kind : Family) (n : Slime) : Slime :=
  match kind with
  | .zombie =>
    let vx := n.velocity.x
    let sprite := if n.velocity.y == 0 && (n.direction == 1 || n.direction == -1)
      then n.direction else n.spriteDirection
    if n.velocity.y != 0 || (n.direction == 1 && vx < 0) || (n.direction == -1 && vx > 0) then
      { n with spriteDirection := sprite, frameY := 2, frameCounter := 0 }
    else if vx == 0 then
      { n with spriteDirection := sprite, frameY := 0, frameCounter := 0 }
    else
      let counter := n.frameCounter + vx.abs.toFloat
      if counter ≥ 32 then { n with spriteDirection := sprite, frameCounter := 0 }
      else
        let row := if counter < 8 then 0 else if counter < 16 then 1 else if counter < 24 then 2 else 1
        { n with spriteDirection := sprite, frameY := row, frameCounter := counter }
  | .demonEye =>
    let vx := n.velocity.x
    let vy := n.velocity.y
    let sprite := if vx > 0 then 1 else if vx < 0 then -1 else n.spriteDirection
    let angle := if vx == 0 then n.rotation
      else
        let angle := (Float.atan2 vy.toFloat vx.toFloat).toFloat32
        if vx < 0 then angle + 3.14 else angle
    let counter := n.frameCounter + 1
    if counter ≥ 8 then
      { n with spriteDirection := sprite, rotation := angle, frameY := (n.frameY + 1) % 2, frameCounter := 0 }
    else
      { n with spriteDirection := sprite, rotation := angle, frameCounter := counter }

/-- One dry, ordinary Classic night-time `NPC.UpdateNPC` for a supported type.
The caller owns the shared slot table, player contact, and the surrounding RNG phases.
`emit` runs at the original AI draw site. The default is the headless dust/sound path. -/
def update (world : World) (players : Array Player) (rng : Rng) (enemy : Enemy)
    (emit : Effect → Slime → Rng → Rng := fun _ _ r => r) : Except String Step := do
  if !enemy.body.active then return { enemy, rng }
  let some kind := family? enemy.npcType | throw s!"unsupported forest enemy type {enemy.npcType}"
  if world.dayTime || world.slimeRain then throw "ordinary night required"
  if enemy.noTileCollide || (kind == .zombie && enemy.noGravity) then
    throw "unsupported NPC collision flags"
  if enemy.body.wet then throw "unsupported wet NPC"
  if enemy.body.life ≤ 0 || enemy.body.justHit then throw "unsupported enemy death or knockback"
  if enemy.body.ai1 != 0 || enemy.body.ai2 != 0 then throw "unsupported enemy action in progress"
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world enemy.body
  let n := prepare enemy.body
  let (n, effect, rng) ← match kind with
    | .demonEye =>
      let n := steerEye players n
      let (roll, rng) := rng.next 40
      let effect := if roll == 0 then some .eyeBlood else none
      let rng := if roll == 0 then emit .eyeBlood n rng else rng
      pure (n, effect, rng)
    | .zombie => do
      let n := zombieStuck players n
      if n.ai3 ≥ 60 then throw "fighter stuck recovery"
      let (roll, rng) := rng.next 1000
      let effect := if roll == 0 then some .zombieGroan else none
      let rng := if roll == 0 then emit .zombieGroan n rng else rng
      let n := zombieAcceleration enemy.npcType (trackZombie players n)
      let canJump := grounded world.grid n
      let n := zombieStepUp world.grid n
      let n ← if canJump then zombieJump world.grid n else pure n
      pure (n, effect, rng)
  let noGravity := kind == .demonEye || enemy.noGravity
  let n := { n with immune := n.immune.map fun value => if value > 0 then value - 1 else value }
  let n := if noGravity then n else { n with velocity.y := min maxFall (n.velocity.y + gravity) }
  let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then
      { n with velocity.x := 0 } else n
  if (OriginalPlayerWet.wetCollision world.grid n.position n.width n.height).wet ||
      OriginalPlayerLiquidHealth.lavaCollision world.grid n.position n.width n.height then
    throw "enemy entered liquid"
  let n := move world.grid kind gravity n
  let (n, nearby) := OriginalGreenSlime.checkActive world players (animation kind n)
  let n := { n with netUpdate := false, justHit := false }
  return { enemy := { enemy with body := n, noGravity }, nearby, effect, rng }

end Lean4Terria.OriginalForestNightEnemies
