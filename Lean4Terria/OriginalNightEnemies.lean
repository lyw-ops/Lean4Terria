import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalAngryBones

namespace Lean4Terria.OriginalNightEnemies

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Rng Slime)

open OriginalForestGuideArrow (DustPool DustParticle)

inductive Family where
  | zombie | demonEye
  deriving BEq, Repr

/-- The two base NPC types covered by the ordinary Classic replays. -/
def family? (npcType : Int) : Option Family :=
  if npcType == 3 then some .zombie
  else if npcType == 2 then some .demonEye
  else none

/-- The common NPC fields use the same representation as the dry Green Slime model.
Spawner defaults must be installed for the actual net ID before the first update. -/
structure Enemy where
  npcType : Int
  netId : Int
  body : Slime
  noGravity : Bool := false
  noTileCollide : Bool := false
  lastInteraction : Int := 255
  wetCount : Nat := 0
  lavaWet : Bool := false
  honeyWet : Bool := false
  shimmerWet : Bool := false
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

/-- The two replayed Classic net IDs, positioned from the pixel anchor
passed to `NPC.NewNPC`. The caller allocates the slot. -/
def spawn (grid : Grid) (netId x y : Int) : Option Enemy := do
  let d ← (match netId with
    | 3 => some ⟨3, 18, 40, 45, 14, 6, 0.5, 60, 1⟩
    | 2 => some ⟨2, 30, 32, 60, 18, 2, 0.8, 75, 1⟩
    | _ => none : Option BirthDefaults)
  let position : Vector := ⟨single x - single d.width / 2, single (y - d.height)⟩
  let wet := OriginalPlayerWet.wetCollision grid position d.width d.height
  return {
    npcType := d.npcType, netId, honeyWet := wet.honey, shimmerWet := wet.shimmer
    body :=
    { position := ⟨single x - single d.width / 2, single (y - d.height)⟩
      wet := wet.wet
      width := d.width, height := d.height, scale := d.scale, npcSlots := d.scale
      life := d.life, lifeMax := d.life, damage := d.damage, defDamage := d.damage
      defense := d.defense, defDefense := d.defense, knockBackResist := d.knockBackResist
      value := d.value, alpha := 0, timeLeft := 937, color := ⟨0, 0, 0, 0⟩ } }

inductive Effect where
  | zombieGroan | eyeBlood
  deriving BEq, Repr

structure Sound where
  typeId : Int
  style : Int
  position : Vector
  deriving Repr

structure Step where
  enemy : Enemy
  rng : Rng
  nearby : Array (Nat × Float32) := #[]
  effect : Option Effect := none
  dustPool : DustPool := {}
  dust : Array DustParticle := #[]
  sounds : Array Sound := #[]

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
private def steerEye (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (graveyard : Bool)
    (n : Slime) : Slime := Id.run do
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
  n := if world.dayTime && !graveyard && n.position.y.toFloat ≤ world.worldSurface * 16 then
    { n with
      timeLeft := min n.timeLeft 10
      despawnEncouraged := true
      directionY := -1
      direction := if n.velocity.x > 0 then 1 else -1 }
    else OriginalGreenSlime.targetClosest players n
  let scaleAdjustment := 1 + (1 - n.scale)
  n := { n with velocity :=
    ⟨toward n.velocity.x (4 * scaleAdjustment) 0.1 0.1 0.05 n.direction,
     toward n.velocity.y (1.5 * scaleAdjustment) 0.04 0.05 0.03 n.directionY⟩ }
  if !n.wet then return n
  let vy := if n.velocity.y > 0 then n.velocity.y * 0.95 else n.velocity.y
  return OriginalGreenSlime.targetClosest players { n with velocity.y := max (-4) (vy - 0.5) }

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

private def trackZombie (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
  let n := OriginalGreenSlime.targetClosest players n
  let target := OriginalGreenSlime.playerAt players n.target
  if n.directionY > 0 && target.center.y ≤ n.position.y + single n.height then
    { n with directionY := -1 }
  else n

private def zombieStuck (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
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

private def seekZombie (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (graveyard eclipse : Bool)
    (n : Slime) : Slime :=
  let target := OriginalGreenSlime.playerAt players n.target
  let n := if target.position.y + single target.height == n.position.y + single n.height then
    { n with directionY := -1 } else n
  if n.ai3 < 60 && (!world.dayTime || eclipse || graveyard ||
      n.position.y.toFloat > world.worldSurface * 16) then trackZombie players n
  else
    let n := if world.dayTime && (n.position.y / 16).toFloat < world.worldSurface then
      { n with timeLeft := min n.timeLeft 10, despawnEncouraged := true } else n
    let n := if n.velocity.x != 0 then { n with ai0 := 0 }
      else if n.velocity.y != 0 then n
      else if n.ai0 + 1 < 2 then { n with ai0 := n.ai0 + 1 }
      else { n with direction := -n.direction, spriteDirection := -n.direction, ai0 := 0 }
    if n.direction == 0 then { n with direction := 1 } else n

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

private def doorAhead (grid : Grid) (n : Slime) : Option (Int × Int) :=
  let x := tileIndex (n.position.x + single (n.width / 2) + single (15 * n.direction))
  let y := tileIndex (n.position.y + single n.height - 15) - 1
  let ahead := tileAt grid x y
  if !ahead.actuated && (ahead.id == some 10 || ahead.id == some 388) then some (x,y) else none

/-- Ordinary, non-Blood-Moon type 3 resets ai[1] before each knock. Thus it
reaches five, never ten, and cannot open the door. The hit still calls the
original failed KillTile effects. Graveyard, unbreakable player walls and
special-world overrides need the caller's wider fighter context. -/
private def zombieDoor (grid : Grid) (n : Slime) (x y : Int) (graveyard : Bool) :
    Except String (Slime × Array Sound) := do
  if graveyard then throw "graveyard zombie door permission"
  if (tileAt grid x y).id == some 388 then throw "zombie tall-gate failed-hit effects"
  let n := { n with ai2 := n.ai2 + 1, ai3 := 0 }
  if n.ai2 < 60 then return (n,#[])
  let n := {n with ai1:=5,ai2:=0,velocity.x:=0.5 * single (-n.direction)}
  let sounds := (OriginalDoor.failedHitSounds (tileAt grid x y) x y).map fun s =>
    {typeId:=s.typeId,style:=s.style,position:=⟨single s.x,single s.y⟩ : Sound}
  return (n,sounds)

private def zombieJump (grid : Grid) (n : Slime) : Except String Slime := do
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
  return {n with ai1:=0,ai2:=0}

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
  let physical : Vector := if n.wet then
    ⟨if displacement.x != velocity.x then displacement.x else displacement.x * 0.5,
     if displacement.y != velocity.y then displacement.y else displacement.y * 0.5⟩ else displacement
  let position := n.position + physical
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

/-- Ordinary water transitions in Collision_WaterCollision. Honey/shimmer/lava
are explicit boundaries until their buffs, strike and specialized particles are owned. -/
private def waterContact {σ : Type} (effects : σ) (grid : Grid) (enemy : Enemy) (pool : DustPool) (rng : Rng)
    (playSound : Sound → σ → Rng → σ × Rng) : Except String (Enemy × DustPool × Rng × Array DustParticle × Array Sound × σ) := do
  let n := enemy.body
  let hit := OriginalPlayerWet.wetCollision grid n.position n.width n.height
  if hit.honey || hit.shimmer || enemy.honeyWet || enemy.shimmerWet || enemy.lavaWet ||
      OriginalPlayerLiquidHealth.lavaCollision grid n.position n.width n.height then
    throw "NPC honey/shimmer/lava collision"
  let changed := hit.wet != n.wet
  let splash := changed && enemy.wetCount == 0
  let leaving := n.wet && !hit.wet
  let n := { n with wet := hit.wet, velocity.x := if leaving then n.velocity.x * 0.5 else n.velocity.x }
  let mut pool := pool
  let mut random : UnifiedRandom := ⟨rng.table, rng.inext⟩
  let mut particles := #[]
  if splash then
    for _ in [:30] do
      let request : OriginalForestGuideArrow.DustRequest := {
        position := ⟨n.position.x - 6, n.position.y + single (n.height / 2) - 8⟩
        width := n.width + 12, height := 24, typeId := 33 }
      let (nextPool, particle, nextRandom) := pool.create request random
      pool := nextPool
      random := nextRandom
      if let some p := particle then
        particles := particles.push { p with
          velocity := ⟨(p.velocity.x * 0.5) * 2.5, (p.velocity.y * 0.5 + 1) - 4⟩
          request := { p.request with alpha := 100 }, scale := p.scale * 0.8, noGravity := true }
  let (table, inext) := random.state
  let rng : Rng := { table, inext }
  let sounds : Array Sound := if splash && !enemy.noGravity then
    #[{ typeId := 19, style := 0, position := n.position }] else #[]
  let (effects, rng) := sounds.foldl (fun (state, r) s => playSound s state r) (effects,rng)
  return ({ enemy with body := n, wetCount := if splash then 9 else enemy.wetCount - 1 }, pool, rng, particles, sounds, effects)

private def animation (kind : Family) (n : Slime) : Slime :=
  match kind with
  | .zombie =>
    let vx := n.velocity.x
    let sprite := if n.velocity.y == 0 && (n.direction == 1 || n.direction == -1)
      then n.direction else n.spriteDirection
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
      { n with spriteDirection := sprite, rotation := angle, frameY := (n.frameY + 24) % 48, frameCounter := 0 }
    else
      { n with spriteDirection := sprite, rotation := angle, frameCounter := counter }

/-- A complete NPC update in the declared ordinary Classic environment. Sound
callbacks execute at their original AI site, before the next slot; an identity
callback means the audio device is disabled. Dust allocation carries the shared
pool and the same random stream. Frame Y is stored in pixels. -/
private def updateCore {σ : Type} (effects : σ) (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (rng : Rng) (enemy : Enemy)
    (pool : DustPool) (playSound : Sound → σ → Rng → σ × Rng)
    (graveyard : Bool) (eclipse : Bool) : Except String (Step × σ) := do
  if !enemy.body.active then return ({ enemy, rng, dustPool := pool }, effects)
  let some kind := family? enemy.npcType | throw s!"unsupported night enemy type {enemy.npcType}"
  if enemy.noTileCollide || (kind == .zombie && enemy.noGravity) then
    throw "unsupported NPC collision flags"
  if enemy.body.life ≤ 0 then
    return ({ enemy := { enemy with body := { enemy.body with active := false, netUpdate := false, justHit := false } }, rng, dustPool := pool },effects)
  let (gravity, maxFall) := if enemy.body.wet then (0.2, 7) else OriginalGreenSlime.gravityFor world enemy.body
  let n := prepare enemy.body
  let (n, effect, rng, pool, dust, effects, actionSounds) ← match kind with
    | .demonEye => do
      let n := steerEye world players graveyard n
      let (roll, rng) := rng.next 40
      let effect := if roll == 0 then some .eyeBlood else none
      let stream : UnifiedRandom := ⟨rng.table, rng.inext⟩
      let request : OriginalForestGuideArrow.DustRequest := {
        position := ⟨n.position.x, n.position.y + single n.height * 0.25⟩
        width := n.width, height := (single n.height * 0.5).toInt32.toInt.toNat
        typeId := 5, speed := ⟨n.velocity.x, 2⟩ }
      let (pool, particle, stream) := if roll == 0 then pool.create request stream else (pool, none, stream)
      let dust := (particle.map fun p => { p with velocity := ⟨p.velocity.x * 0.5, p.velocity.y * 0.1⟩ }).toArray
      let (table, inext) := stream.state
      pure (n, effect, { table, inext }, pool, dust, effects, #[])
    | .zombie => do
      let n := zombieStuck players n
      let pursue := n.ai3 < 60 && (!world.dayTime || eclipse || graveyard || n.position.y.toFloat > world.worldSurface * 16)
      let (roll, rng) := if pursue then rng.next 1000 else (1, rng)
      let effect := if roll == 0 then some .zombieGroan else none
      let (effects,rng) := if roll == 0 then playSound ⟨14, 1, n.position⟩ effects rng else (effects,rng)
      let n := zombieAcceleration enemy.npcType (seekZombie world players graveyard eclipse n)
      let canJump := grounded world.grid n
      let n := zombieStepUp world.grid n
      let (n,actionSounds) ← if canJump then
          match doorAhead world.grid n with
          | some (x,y) => zombieDoor world.grid n x y graveyard
          | none => do pure (← zombieJump world.grid n, #[])
        else pure ({n with ai1:=0,ai2:=0},#[])
      let (effects,rng) := actionSounds.foldl (fun (state,r) s => playSound s state r) (effects,rng)
      pure (n, effect, rng, pool, #[], effects, actionSounds)
  let noGravity := kind == .demonEye || enemy.noGravity
  let n := { n with immune := n.immune.map fun value => if value > 0 then value - 1 else value }
  let n := if noGravity then n else { n with velocity.y := min maxFall (n.velocity.y + gravity) }
  let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then { n with velocity.x := 0 } else n
  let velocity := OriginalCollision.walkDownSlope world.grid n.position n.velocity n.width n.height gravity
  let (wetEnemy, pool, rng, splashDust, splashSounds, effects) ← waterContact effects world.grid
    { enemy with body := { n with velocity }, noGravity } pool rng playSound
  let n := move world.grid kind gravity wetEnemy.body
  let (n, nearby) := OriginalGreenSlime.checkActive world players (animation kind n)
  let n := { n with netUpdate := false, justHit := false }
  let sounds := if effect == some .zombieGroan then #[Sound.mk 14 1 (prepare enemy.body).position] else #[]
  return ({
    enemy := { wetEnemy with body := n, noGravity }, nearby, effect, rng, dustPool := pool
    dust := dust ++ splashDust, sounds := sounds ++ actionSounds ++ splashSounds },effects)

/-- Compatibility update for a caller that already owns its sound channel.
The callback runs at each native call site; identity is the audio-off case. -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player)
    (rng : Rng) (enemy : Enemy) (pool : DustPool := {})
    (playSound : Sound → Rng → Rng := fun _ r => r)
    (graveyard : Bool := false) (eclipse : Bool := false) : Except String Step := do
  let (step, _) ← updateCore () world players rng enemy pool
    (fun sound _ random => ((),playSound sound random)) graveyard eclipse
  return step

/-! ## Ordinary Classic combat and death -/

/-- Strike view; immunity is read from the caller's real player slot. -/
def Enemy.toCombatNpc (enemy : Enemy) (playerSlot : Nat := 0) : OriginalStarterMelee.Npc :=
  let n := enemy.body
  { active := n.active, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax
    defense := n.defense, knockBackResist := n.knockBackResist
    noGravity := enemy.noGravity, noTileCollide := enemy.noTileCollide
    justHit := n.justHit, immune := (n.immune[playerSlot]?.getD 0).toNat }

/-- Preserve activity through HitEffect; the death caller clears it after loot.
A weapon's own immunity write is separate from StrikeNPC_Inner. -/
def Enemy.applyHit (enemy : Enemy) (hit : OriginalStarterMelee.Npc)
    (playerSlot : Nat := 0) (credited : Bool := true) : Enemy :=
  { enemy with
    body := { enemy.body with
      active := hit.active, life := hit.life, velocity := hit.velocity, justHit := hit.justHit
      immune := enemy.body.immune.set! playerSlot hit.immune
      playerInteraction := enemy.body.playerInteraction || credited }
    lastInteraction := if credited then playerSlot else enemy.lastInteraction }

def Enemy.contactSource (enemy : Enemy) : Option (Int × String) :=
  if !enemy.body.active || enemy.body.damage ≤ 0 then none
  else some (enemy.body.damage, if enemy.npcType == 3 then "Zombie" else "Demon Eye")

/-- The loaded SetDefaults LegacySoundStyle has one variation, unit volume
and no randomized style/pitch. HitEffect precedes this request. -/
def Enemy.hitSound (enemy : Enemy) : Sound := ⟨3,1,enemy.body.position⟩

/-- checkDead requests this after HitSound and before NPCLoot. -/
def Enemy.deathSound (enemy : Enemy) : Sound :=
  ⟨4,if enemy.npcType == 3 then 2 else 1,enemy.body.position⟩

/-- Native BannerID and BannerToItem. Kill counts and claim state belong to
the shared original BannerSystem caller. -/
def Enemy.bannerInfo (enemy : Enemy) : Int × UInt16 :=
  if enemy.npcType == 3 then (3,1617) else (2,1616)

/-- Both normal first-night enemies use ordinary, debuff-free contact. The
caller owns the shared text pool and player death path; this compatibility
entry assumes a free text slot and a living unequipped player. -/
def contact (enemy : Enemy) (body : OriginalPlayerCollision.Body)
    (health : OriginalPlayerLiquidHealth.Health) (defense : Int) (pool : DustPool)
    (rng : UnifiedRandom) : Option OriginalAngryBones.ContactHit :=
  OriginalAngryBones.contactNpc enemy.toCombatNpc enemy.body.damage body health defense pool rng

/-- Non-craftable first-night loot is absent from the crafting prefix catalog.
The real GetRollablePrefixes/TryGetPrefixStatMultipliersForItem metadata is
recorded with the native loot rules. All Zombie Arm candidates are valid. -/
def naturalPrefix (item : UInt16) (random : UnifiedRandom) : UInt8 × UnifiedRandom := Id.run do
  if item != 216 && item != 1304 then return OriginalRecipeCatalog.rollNaturalPrefix item random
  let (plain, random) := random.next 0 4
  if plain == 0 then return (0,random)
  if item == 216 then
    let (chosen, random) := random.next 62 81
    return (chosen.toNat.toUInt8,random)
  let pool : Array UInt8 := #[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,36,37,38,53,54,55,39,40,56,41,57,42,43,44,45,46,47,48,49,50,51,59,60,61,81]
  let (index, random) := random.next 0 pool.size
  let chosen := pool[index.toNat]!
  if OriginalRecipeCatalog.CraftData.reducedNaturalChance.contains chosen then
    let (keep,random) := random.next 0 3
    return (if keep == 0 then chosen else 0,random)
  return (chosen,random)

private def appendLoot (env : OriginalCombat.Loot.Context) (position : Vector)
    (item : UInt16) (count : Nat) (loot : OriginalCombat.Loot.Result) : OriginalCombat.Loot.Result :=
  let (prefixId, random) := naturalPrefix item loot.random
  let (drop, random) := OriginalItems.rollDropAt position
    { OriginalItems.Stack.ofItem item count with prefixId } random env.grid
  { drops := loot.drops.push drop, random }

private def commonLoot (env : OriginalCombat.Loot.Context) (position : Vector)
    (item : UInt16) (denominator : Int) (loot : OriginalCombat.Loot.Result) : Bool × OriginalCombat.Loot.Result :=
  let (roll, random) := loot.random.next 0 denominator
  if roll != 0 then (false, { loot with random })
  else
    let (_, random) := random.next 1 2
    (true, appendLoot env position item 1 { loot with random })

/-- Full NPCLoot in an ordinary zero-luck, non-seasonal, pre-Hardmode forest.
The two rules in the lens chain are mutually exclusive. The optional Eye
progress condition is the real EyeOfCthulhuDefeatedAndNoAltarsInWorld rule. -/
def dropLoot (env : OriginalCombat.Loot.Context) (enemy : Enemy) (rng : UnifiedRandom)
    (eyeDefeatedNoAltars : Bool := false) : OriginalCombat.Loot.Result := Id.run do
  let n := enemy.body
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let mut out : OriginalCombat.Loot.Result := { random := rng }
  if enemy.npcType == 3 then
    for (item, denominator) in #[(216,50),(1304,250),(5332,1500)] do
      out := (commonLoot env position item denominator out).2
  else
    let (blackLens, next) := commonLoot env position 236 100 out
    out := if blackLens then next else (commonLoot env position 38 3 next).2
    if eyeDefeatedNoAltars then out := (commonLoot env position 43 50 out).2
  out := OriginalCombat.Loot.dropMoneyAt env position n.value out
  return OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) out

/-- Fields written by ordinary NewGore for types 1..5. These are real
particles, with slot ownership; no RNG-only placeholder is used. -/
structure Gore where
  slot : Nat
  typeId : Nat
  position : Vector
  velocity : Vector
  rotation : Float32 := 0
  scale : Float32 := 1
  alpha : Int := 0
  timeLeft : Int := 600
  sticky : Bool := true
  behindTiles : Bool := false
  active : Bool := true
  deriving Repr

def goreTextureSize : Nat → Option (Nat × Nat)
  | 1 => some (18,22)
  | 2 => some (16,22)
  | 3 => some (26,30)
  | 4 => some (22,14)
  | 5 => some (22,20)
  | _ => none

private def goreInWorld (grid : Grid) (position : Vector) : Bool :=
  -- Utils.ToTileCoordinates truncates the pixel first, then arithmetic-shifts
  -- by four. Float division followed by truncation differs at negative edges.
  let x := position.x.toInt32.toInt / 16
  let y := position.y.toInt32.toInt / 16
  0 ≤ x && x < (grid.width : Int) && 0 ≤ y && y < (grid.height : Int)

private def newGoreCore (grid : Grid) (occupied : Array Bool) (typeId : Nat) (position velocity : Vector)
    (scale : Float32) (random : UnifiedRandom) : Array Bool × Option Gore × UnifiedRandom := Id.run do
  let slot := (occupied.findIdx? (!·)).getD occupied.size
  if slot ≥ 600 then return (occupied,none,random)
  let (up,random) := random.next 10 31
  let (side,random) := random.next (-20) 21
  let active := goreInWorld grid position
  let particle : Gore := {
    slot,typeId,position,scale,active
    velocity:=⟨velocity.x + single side*0.1,velocity.y - single up*0.1⟩ }
  let occupied := occupied ++ Array.replicate (slot + 1 - occupied.size) false
  return (occupied.set! slot active,if active then some particle else none,random)

/-- Ordinary NewGore with child safety disabled, allocating before drawing Y/X.
A full pool or a paused/generating/server call consumes no sample. -/
def newGore (grid : Grid) (occupied : Array Bool) (typeId : Nat) (position velocity : Vector)
    (scale : Float32) (random : UnifiedRandom) (blocked : Bool := false) :
    Except String (Array Bool × Option Gore × UnifiedRandom) := do
  if blocked then return (occupied,none,random)
  if (goreTextureSize typeId).isNone then throw "unsupported gore type"
  return newGoreCore grid occupied typeId position velocity scale random

/-- Full ordinary Gore.Update for the five first-night fragments. Loaded
texture state is explicit; otherwise the native 32-pixel collision fallback
applies. No random draw occurs in this branch. -/
def updateGore (grid : Grid) (particle : Gore) (loaded : Bool := true)
    (multiplayerServer : Bool := false) : Except String Gore := do
  if multiplayerServer || !particle.active then return particle
  let some (width,height) := goreTextureSize particle.typeId | throw "unsupported gore update"
  if particle.sticky && !goreInWorld grid particle.position then return {particle with active:=false}
  let mut velocity := particle.velocity
  if particle.sticky then
    let length := (velocity.x*velocity.x+velocity.y*velocity.y).toFloat.sqrt.toFloat32
    if length > 32 then
      let ratio := 32/length
      velocity := ⟨velocity.x*ratio,velocity.y*ratio⟩
  velocity := {velocity with y:=velocity.y+0.2}
  let rotation := particle.rotation+velocity.x*0.05
  let mut alpha := particle.alpha
  let mut timeLeft := particle.timeLeft
  if particle.sticky then
    let size := if loaded then min width height else 32
    let size := ((single ((single size*0.9).toInt32.toInt))*particle.scale).toInt32.toInt.toNat
    velocity := (OriginalCollision.tileCollision grid particle.position velocity size size {hoik:=true}).displacement
    if velocity.y == 0 then
      velocity := {velocity with x:=velocity.x*0.97}
      if velocity.x.toFloat > -0.01 && velocity.x.toFloat < 0.01 then velocity := {velocity with x:=0}
    if timeLeft > 0 then timeLeft := timeLeft-1 else alpha := alpha+1
  else alpha := alpha+2
  return {particle with velocity,position:=particle.position+velocity,rotation,alpha,timeLeft,active:=alpha<255}

def goreAlpha (particle : Gore) (light : OriginalForestChatLayout.Color) : OriginalForestChatLayout.Color :=
  let factor := single (255-particle.alpha)/255
  OriginalForestChatLayout.Color.mk' ((single light.r*factor).toInt32.toInt)
    ((single light.g*factor).toInt32.toInt) ((single light.b*factor).toInt32.toInt)
    ((light.a : Int)-particle.alpha)

/-- Ordinary Main.DrawGore after the caller samples light at the unscaled
texture center. Integer half sizes match the original sprite origin. -/
def goreSprite (particle : Gore) (screen : Vector) (light : OriginalForestChatLayout.Color) :
    Except String (Option OriginalForestChatLayout.Sprite) := do
  if !particle.active then return none
  let some (width,height) := goreTextureSize particle.typeId | throw "unsupported gore draw"
  let x := particle.position.x-screen.x+single (width/2)
  let y := particle.position.y-screen.y+single (height/2)
  return some <| OriginalForestChatLayout.draw s!"Images/Gore_{particle.typeId}" width height x y
    (some ⟨0,0,width,height⟩) (goreAlpha particle light) particle.rotation (single (width/2)) (single (height/2))
    particle.scale particle.scale

structure HitEffects where
  pool : DustPool
  dust : Array DustParticle := #[]
  goreOccupied : Array Bool := Array.replicate 600 false
  gore : Array Gore := #[]
  random : UnifiedRandom

/-- NPC.HitEffect for active ordinary Zombie3/DemonEye2. The same global
stream runs through every allocation, then the Y and X draw of each successful
NewGore. No gore allocation or RNG occurs when paused/generating/full. Child
safety is disabled, as in the recorded desktop configuration. -/
def hitEffects (grid : Grid) (pool : DustPool) (goreOccupied : Array Bool) (enemy : Enemy)
    (direction : Int) (dealt : Float) (random : UnifiedRandom)
    (goreBlocked : Bool := false) : HitEffects := Id.run do
  let n := enemy.body
  let mut out : HitEffects := { pool, goreOccupied, random }
  if !n.active then return out
  let dead := n.life ≤ 0
  let count := if dead then 50 else ((dealt / Float.ofInt n.lifeMax * 100).ceil).toUInt64.toNat
  let speed : Vector := if dead then
    if enemy.npcType == 3 then ⟨2.5 * single direction,-2.5⟩ else ⟨single (2 * direction),-2⟩
    else ⟨single direction,-1⟩
  for _ in [:count] do
    let (pool, particle, random) := out.pool.create {
      position := n.position, width := n.width, height := n.height, typeId := 5, speed } out.random
    out := { out with pool, random, dust := particle.elim out.dust out.dust.push }
  if !dead || goreBlocked then return out
  let fragments : Array (Nat × Vector × Float32) := if enemy.npcType == 3 then
    #[(3,n.position,1),(4,n.position + ⟨0,20⟩,1),(4,n.position + ⟨0,20⟩,1),
      (5,n.position + ⟨0,34⟩,1),(5,n.position + ⟨0,34⟩,1)]
    else #[(1,n.position,n.scale),(2,n.position + ⟨14,0⟩,n.scale)]
  for (typeId,position,scale) in fragments do
    let (occupied,gore,random) := newGoreCore grid out.goreOccupied typeId position n.velocity scale out.random
    out := { out with random,goreOccupied:=occupied,gore:=gore.elim out.gore out.gore.push }
  return out

/-! ## Ordinary NPC draw contract -/

/-- The ordinary GetAlpha branch. Colors use the shared FNA packed convention;
NPC shimmer transparency and enemy status-effect tinting are separate owners. -/
def getAlpha (enemy : Enemy) (light : OriginalForestChatLayout.Color) : OriginalForestChatLayout.Color :=
  let factor := single (255 - enemy.body.alpha) / 255
  OriginalForestChatLayout.Color.mk' ((single light.r * factor).toInt32.toInt)
    ((single light.g * factor).toInt32.toInt) ((single light.b * factor).toInt32.toInt)
    ((light.a : Int) - enemy.body.alpha)

/-- The one ordinary DrawNPCDirect sprite, down to FNA PushSprite fields.
Real XNB dimensions are NPC_3=38x144 /3 frames and NPC_2=38x48 /2 frames.
The caller samples the real light at the NPC center and preserves global order. -/
def npcSprite (enemy : Enemy) (screen : Vector) (light : OriginalForestChatLayout.Color) :
    Except String OriginalForestChatLayout.Sprite := do
  let some _ := family? enemy.npcType | throw "unsupported night-enemy draw type"
  let n := enemy.body
  let frameHeight : Nat := if enemy.npcType == 3 then 48 else 24
  let textureHeight : Nat := if enemy.npcType == 3 then 144 else 48
  if n.color.rgba != 0 then throw "NPC color overlay"
  let x := n.position.x - screen.x + single (n.width / 2)
  let y := n.position.y - screen.y + single n.height - single frameHeight * n.scale +
    (single frameHeight / 2) * n.scale + 4 + n.gfxOffY
  return OriginalForestChatLayout.draw s!"Images/NPC_{enemy.npcType}" 38 textureHeight x y
    (some ⟨0,n.frameY,38,frameHeight⟩) (getAlpha enemy light) n.rotation 19 (single frameHeight / 2)
    n.scale n.scale (if n.spriteDirection == 1 then 1 else 0)

/-! ## Ready-resource LegacySoundPlayer channel -/

structure AudioContext where
  dedicatedServer : Bool := false
  supported : Bool := false
  generating : Bool := false
  netMode : Int := 0
  volume : Float32 := 1
  cameraCenter : Vector := ⟨0,0⟩
  viewWidth : Float32 := 1920
  attenuationDistance : Float32 := 2500

/-- Backend playback states that affect the covered original branches.
The caller sets splashBusy=false when its physical voice has stopped. Zombie
and death voices overlap; NPCHit stops its previous alias. -/
structure AudioState where
  splashBusy : Bool := false
  npcHitUsed : Bool := false
  digUsed : Array Bool := #[false,false,false]
  deriving BEq, Repr

structure PlayedSound where
  request : Sound
  asset : String
  volume : Float32
  pan : Float32
  pitch : Float32
  stopPrevious : Bool := false
  deriving Repr

structure AudioResult where
  state : AudioState
  played : Option PlayedSound := none
  random : Rng

private def resolveAudio (context : AudioContext) (state : AudioState)
    (request : Sound) (random : Rng) : AudioResult := Id.run do
  let silent : AudioResult := {state,random}
  if context.dedicatedServer || !context.supported || context.volume == 0 then return silent
  let x := request.position.x.toInt32.toInt
  let y := request.position.y.toInt32.toInt
  let global := x == -1 || y == -1
  if !global && (context.generating || context.netMode == 2) then return silent
  let dx := single x - context.cameraCenter.x
  let dy := single y - context.cameraCenter.y
  let distance := Float32.sqrt (dx * dx + dy * dy)
  if !global && !(distance < context.attenuationDistance) then return silent
  let pan := if global then 0 else max (-1) (min 1 (dx / (context.viewWidth * 0.5)))
  let volume := min 1 ((if global then 1 else max 0 (min 1 (1 - distance / context.attenuationDistance))) * context.volume)
  if !(volume > 0) then return silent
  if request.typeId == 14 then
    let (variation,random) := random.next 3
    return {state,random,played:=some {request,asset:=s!"Sounds/Zombie_{variation}",volume:=volume*0.4,pan,pitch:=0}}
  if request.typeId == 0 then
    let (variation,random) := random.next 3
    let (pitch,random) := random.nextRange (-10) 11
    let used := state.digUsed[variation]?.getD false
    let state := {state with digUsed:=state.digUsed.set! variation true}
    return {state,random,played:=some {request,asset:=s!"Sounds/Dig_{variation}",volume,pan,pitch:=single pitch*0.01,stopPrevious:=used}}
  if request.typeId == 19 && state.splashBusy then return silent
  let (pitch,random) := random.nextRange (-10) 11
  let state := if request.typeId == 19 then {state with splashBusy:=true}
    else if request.typeId == 3 then {state with npcHitUsed:=true} else state
  let asset := if request.typeId == 19 then "Sounds/Splash_0"
    else if request.typeId == 3 then "Sounds/NPC_Hit_1" else s!"Sounds/NPC_Killed_{request.style}"
  return {
    state,random
    played := some {
      request,asset,volume,pan,pitch:=single pitch*0.01
      stopPrevious:=request.typeId==3 && silent.state.npcHitUsed}}

/-- Real type14/style1, type19/style0, NPCHit1 and NPCDeath1/2 with ready
original assets. Device operations are emitted as resolved requests. -/
def playAudio (context : AudioContext) (state : AudioState) (request : Sound)
    (random : Rng) : Except String AudioResult := do
  unless (request.typeId==0 && request.style==1) || (request.typeId==14 && request.style==1) || (request.typeId==19 && request.style==0) ||
      (request.typeId==3 && request.style==1) || (request.typeId==4 && (request.style==1 || request.style==2)) do
    throw "unsupported night-enemy legacy sound"
  return resolveAudio context state request random

private structure AudioChannel where
  state : AudioState
  played : Array PlayedSound := #[]

/-- Sound and its mutable voice state execute at the actual AI/collision sites.
The same channel is then carried into the next NPC slot. No random samples
are replayed later or injected from the reference trace. -/
def updateWithAudio (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player)
    (random : Rng) (enemy : Enemy) (pool : DustPool) (context : AudioContext) (audio : AudioState)
    (graveyard : Bool := false) (eclipse : Bool := false) :
    Except String (Step × AudioState × Array PlayedSound) := do
  let initial : AudioChannel := {state:=audio}
  let (step,channel) ← updateCore initial world players random enemy pool
    (fun request channel random =>
      let out := resolveAudio context channel.state request random
      ({state:=out.state,played:=out.played.elim channel.played channel.played.push},out.random)) graveyard eclipse
  return (step,channel.state,channel.played)

end Lean4Terria.OriginalNightEnemies
