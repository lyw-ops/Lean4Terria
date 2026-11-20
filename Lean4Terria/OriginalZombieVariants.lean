import Lean4Terria.OriginalNightEnemies
import Lean4Terria.OriginalForestCritter
import Lean4Terria.OriginalZombieVariants.Tables

/-!
# Zombie family variants (desktop Terraria 1.4.5.8 Classic, single player)

The zombie family that shares `aiStyle 3` with the base Zombie (type 3):

* walkers: 3, 186 Pincushion, 187 Slimed, 188 Swamp, 189 Twiggy, and their `SetDefaultsFromNetId` sizes
  (-26 and -27, -30 and -31, -32 and -33, -34 and -35, -36 and -37);
* armed zombies 430–436 (`ArmedZombie…`), which replace the walk acceleration with a lunge.

`NPC.SetDefaults` comes from the native table in `OriginalZombieVariants.Tables`. The per-frame
update ports `NPC.UpdateNPC → AI_003_Fighters → FindFrame` for the family. It starts from the
verified type 3 path of `OriginalNightEnemies`, whose helpers are private there and are therefore
restated here unchanged. The differences come from the original IL:

* the speed cap factor (`AI_003` walk branch): 186 ×1.1, 187 ×0.9, 188 ×1.2, 189 ×0.8, others 1;
* the groan `Main.rand.Next(1000)` (sound 14) is drawn only by 3 and 186–189;
* door knocking (`V_5` in `AI_003`) is refused for 430–436 (type chain at IL_39c0). With it go the
  `ai[1] = ai[2] = 0` resets after a failed jump and when not grounded (IL_dc1d, IL_e4dd). A door
  ahead is then just a wall;
* the armed lunge (IL_75ef): with `ai[2] == 0`, `damage = defDamage`, the ordinary acceleration
  (factor 1), then a grounded night/underground lunge start within 50 px of a living, visible
  target; otherwise `damage = (int)(defDamage * 1.5)`, `ai[3] = 1`, `vx *= 0.9`, `ai[2] += 1`;
* `FindFrame`: the walkers share the type 3 frames (48 px rows); armed zombies use 68 px rows
  (`NPC_430.xnb` 476 px / 7 frames) and the lunge rows 3–6 while `ai[2] > 0`.

190 (Cataract Eye) is in the requested id range but is a Demon Eye variant (`aiStyle 2`). Its
defaults are in the table; its update is not part of this module (`update` refuses it).
-/

namespace Lean4Terria.OriginalZombieVariants

open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Rng Slime)
open OriginalForestGuideArrow (DustPool DustParticle)
open OriginalNightEnemies (Enemy Sound Step Effect)

/-! ## SetDefaults / NewNPC -/

/-- One native `NPC.SetDefaults(netID)` row. -/
structure Defaults where
  netId : Int
  npcType : Int
  width : Nat
  height : Nat
  life : Int
  damage : Int
  defense : Int
  knockBackResist : Float32
  value : Float32
  scale : Float32
  npcSlots : Float32
  aiStyle : Int
  banner : Int
  hitStyle : Int
  deathStyle : Int
  deriving Repr

def defaults? (netId : Int) : Option Defaults :=
  Tables.defaults.findSome? fun (n, t, w, h, life, dmg, def_, kb, v, s, slots, ai, banner, hit, death) =>
    if n == netId then
      some { netId := n, npcType := t, width := w, height := h, life, damage := dmg, defense := def_
             knockBackResist := Float32.ofBits kb, value := Float32.ofBits v, scale := Float32.ofBits s
             npcSlots := Float32.ofBits slots, aiStyle := ai, banner, hitStyle := hit, deathStyle := death }
    else none

/-- The walking family (type 3 rules) and the armed family. -/
inductive Kind where
  | walker | armed
  deriving BEq, Repr

def kind? (npcType : Int) : Option Kind :=
  if npcType == 3 || (186 ≤ npcType && npcType ≤ 189) then some .walker
  else if 430 ≤ npcType && npcType ≤ 436 then some .armed
  else none

/-- `(texture width, frame height)`: `TextureAssets.Npc[type]` width and `Height / Main.npcFrameCount[type]`. -/
def frameSize (npcType : Int) : Nat × Nat :=
  match Tables.textures.find? (·.1 == npcType) with
  | some (_, w, h, count) => (w, h / count)
  | none => (0, 0)

/-- `NPC.NewNPC(source, x, y, netID)` for a family member, in a free slot the caller allocates:
`SetDefaults(netID)`, `Entity.Bottom = (x, y)`: `position = (x - width * 0.5f, y - height)` (a float half: an
odd width such as -27's 19 gives a .5 position), `timeLeft =
(int)(activeTime * 1.25)`, the initial `Collision.WetCollision`. No `Main.rand` draw. -/
def spawn (grid : Grid) (netId x y : Int) : Option Enemy := do
  let d ← defaults? netId
  if d.aiStyle != 3 then none
  let position : Vector := ⟨single x - single d.width / 2, single (y - d.height)⟩
  let wet := OriginalPlayerWet.wetCollision grid position d.width d.height
  return {
    npcType := d.npcType, netId, honeyWet := wet.honey, shimmerWet := wet.shimmer
    body :=
    { position, wet := wet.wet, netId
      width := d.width, height := d.height, scale := d.scale, npcSlots := d.npcSlots
      life := d.life, lifeMax := d.life, damage := d.damage, defDamage := d.damage
      defense := d.defense, defDefense := d.defense, knockBackResist := d.knockBackResist
      value := d.value, alpha := 0, timeLeft := 937, color := ⟨0, 0, 0, 0⟩ } }

/-! ## AI_003 for the family (helpers restated from the verified type 3 port) -/

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

/-- `DespawnEncouragement_AIStyle3_Fighters_NotDiscouraged` for the family in a non-remix world:
eclipse, `!IsItDay()`, below the surface, or a graveyard target (statue spawns are refused by
`update`; no family member is in the special or critter lists). -/
private def notDiscouraged (world : OriginalGreenSlime.World) (graveyard eclipse : Bool) (n : Slime) : Bool :=
  !world.dayTime || eclipse || graveyard || n.position.y.toFloat > world.worldSurface * 16

private def seekZombie (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (graveyard eclipse : Bool)
    (n : Slime) : Slime :=
  let target := OriginalGreenSlime.playerAt players n.target
  let n := if target.position.y + single target.height == n.position.y + single n.height then
    { n with directionY := -1 } else n
  if n.ai3 < 60 && notDiscouraged world graveyard eclipse n then trackZombie players n
  else
    let n := if world.dayTime && (n.position.y / 16).toFloat < world.worldSurface then
      { n with timeLeft := min n.timeLeft 10, despawnEncouraged := true } else n
    let n := if n.velocity.x != 0 then { n with ai0 := 0 }
      else if n.velocity.y != 0 then n
      else if n.ai0 + 1 < 2 then { n with ai0 := n.ai0 + 1 }
      else { n with direction := -n.direction, spriteDirection := -n.direction, ai0 := 0 }
    if n.direction == 0 then { n with direction := 1 } else n

/-- The walk speed factor of the `AI_003` acceleration chain. -/
def speedFactor (npcType : Int) : Float32 :=
  if npcType == 186 then 1.1 else if npcType == 187 then 0.9 else if npcType == 188 then 1.2
  else if npcType == 189 then 0.8 else 1

private def acceleration (factor : Float32) (n : Slime) : Slime :=
  let cap := factor * (1 + (1 - n.scale))
  let vx := n.velocity.x
  if vx < -cap || vx > cap then
    if n.velocity.y == 0 then { n with velocity := ⟨vx * 0.8, n.velocity.y * 0.8⟩ } else n
  else if n.direction == 1 && vx < cap then { n with velocity.x := min cap (vx + 0.07) }
  else if n.direction == -1 && vx > -cap then { n with velocity.x := max (-cap) (vx - 0.07) }
  else n

/-- `Entity.Center`: integer half sizes. -/
private def centerOf (position : Vector) (width height : Nat) : Vector :=
  ⟨position.x + single (width / 2), position.y + single (height / 2)⟩

/-- `Vector2.Length`: the squared length in `float`, `Math.Sqrt` in `double`, back to `float`. -/
private def length (v : Vector) : Float32 :=
  (v.x * v.x + v.y * v.y).toFloat.sqrt.toFloat32

/-- `Collision.CanHit(Vector2, 1, 1, Vector2, 1, 1)`: half of 1 is 0, so the `Point` walk of the
two truncated points (`OriginalForestCritter.canHit`, the 0-size overload). -/
private def canHitPoints (grid : Grid) (a b : Vector) : Bool :=
  OriginalForestCritter.canHit grid a b

/-- The armed lunge (IL_75ef … IL_7d78), in place of the walk acceleration. -/
private def armedLunge (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (n : Slime) : Slime :=
  let day := world.dayTime
  let surface := world.worldSurface * 16
  if n.ai2 == 0 then
    let n := acceleration 1 { n with damage := n.defDamage }
    if n.velocity.y != 0 then n
    else if day && !(n.position.y.toFloat > surface) then n
    else
      let p := OriginalGreenSlime.playerAt players n.target
      if p.dead then n
      else
        let own := centerOf n.position n.width n.height
        let theirs := centerOf p.position p.width p.height
        if !(length ⟨own.x - theirs.x, own.y - theirs.y⟩ < 50) then n
        else if !canHitPoints world.grid own theirs then n
        else { n with velocity.x := n.velocity.x * 0.7, ai2 := 1 }
  else
    let damage := (Float.ofInt n.defDamage * 1.5).toInt64.toInt
    let vx := n.velocity.x * 0.9
    let vx := if vx.abs.toFloat < 0.1 then 0 else vx
    let n := { n with damage, ai3 := 1, velocity.x := vx, ai2 := n.ai2 + 1 }
    let reset := n.ai2 ≥ 20 || n.velocity.y != 0 || (day && n.position.y.toFloat < surface)
    if reset then { n with ai2 := 0 } else n

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

/-- The grounded jump. `knocks` is `V_5`: false for armed zombies, which keep `ai[1]`/`ai[2]`. -/
private def zombieJump (grid : Grid) (knocks : Bool) (n : Slime) : Slime := Id.run do
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
  return if knocks then { n with ai1 := 0, ai2 := 0 } else n

private def prepare (n : Slime) : Slime :=
  let step := (1 + n.velocity.x.abs / 3) * n.stepSpeed
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - step)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + step) else n.gfxOffY
  { n with
    soundDelay := if n.soundDelay > 0 then n.soundDelay - 1 else n.soundDelay
    oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY
    gfxOffY := max (-16) (min 16 gfx) }

private def move (grid : Grid) (gravity : Float32) (n : Slime) : Slime :=
  let fallThrough := n.directionY == 1
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

/-- Ordinary water transitions in `Collision_WaterCollision` (as in the type 3 port). -/
private def waterContact (grid : Grid) (enemy : Enemy) (pool : DustPool) (rng : Rng) :
    Except String (Enemy × DustPool × Rng × Array DustParticle × Array Sound) := do
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
  let sounds : Array Sound := if splash && !enemy.noGravity then
    #[{ typeId := 19, style := 0, position := n.position }] else #[]
  return ({ enemy with body := n, wetCount := if splash then 9 else enemy.wetCount - 1 }, pool, { table, inext }, particles, sounds)

/-- `FindFrame` for the family; `frameY` in pixels (`frameHeight` from the shipped texture). -/
def findFrame (npcType : Int) (n : Slime) : Slime :=
  let h : Int := (frameSize npcType).2
  if kind? npcType == some .armed && n.ai2 > 0 then
    { n with frameY := h * (if n.ai2 < 5 then 3 else if n.ai2 < 10 then 4 else if n.ai2 < 15 then 5 else 6) }
  else
    let vx := n.velocity.x
    let sprite := if n.velocity.y == 0 && (n.direction == 1 || n.direction == -1)
      then n.direction else n.spriteDirection
    if n.velocity.y != 0 || (n.direction == 1 && vx < 0) || (n.direction == -1 && vx > 0) then
      { n with spriteDirection := sprite, frameY := 2 * h, frameCounter := 0 }
    else if vx == 0 then
      { n with spriteDirection := sprite, frameY := 0, frameCounter := 0 }
    else
      let counter := n.frameCounter + vx.abs.toFloat
      if counter ≥ 32 then { n with spriteDirection := sprite, frameCounter := 0 }
      else
        let row := if counter < 8 then 0 else if counter < 16 then 1 else if counter < 24 then 2 else 1
        { n with spriteDirection := sprite, frameY := row * h, frameCounter := counter }

/-- One `NPC.UpdateNPC` for an active family member in the declared ordinary Classic environment
(non-remix, no Blood Moon/Expert/statue/shimmer). Sounds are requests at their AI site; the
caller plays them (the probe's audio is off, so no sound draws `Main.rand`). -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player)
    (rng : Rng) (enemy : Enemy) (pool : DustPool := {})
    (graveyard : Bool := false) (eclipse : Bool := false) : Except String Step := do
  if !enemy.body.active then return { enemy, rng, dustPool := pool }
  let some kind := kind? enemy.npcType | throw s!"unsupported zombie variant type {enemy.npcType}"
  if enemy.noTileCollide || enemy.noGravity then throw "unsupported NPC collision flags"
  if enemy.body.life ≤ 0 then
    return { enemy := { enemy with body := { enemy.body with active := false, netUpdate := false, justHit := false } }, rng, dustPool := pool }
  let (gravity, maxFall) := if enemy.body.wet then (0.2, 7) else OriginalGreenSlime.gravityFor world enemy.body
  let n := prepare enemy.body
  let n := zombieStuck players n
  let groans := kind == .walker && n.ai3 < 60 && notDiscouraged world graveyard eclipse n
  let (roll, rng) := if groans then rng.next 1000 else (1, rng)
  let effect := if roll == 0 then some Effect.zombieGroan else none
  let n := seekZombie world players graveyard eclipse n
  let n := match kind with
    | .walker => acceleration (speedFactor enemy.npcType) n
    | .armed => armedLunge world players n
  let knocks := kind == .walker
  let canJump := grounded world.grid n
  let n := zombieStepUp world.grid n
  let (n, actionSounds) ← if canJump then
      match (if knocks then doorAhead world.grid n else none) with
      | some (x, y) => zombieDoor world.grid n x y graveyard
      | none => pure (zombieJump world.grid knocks n, #[])
    else pure (if knocks then { n with ai1 := 0, ai2 := 0 } else n, #[])
  let n := { n with immune := n.immune.map fun value => if value > 0 then value - 1 else value }
  let n := { n with velocity.y := min maxFall (n.velocity.y + gravity) }
  let n := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then { n with velocity.x := 0 } else n
  let velocity := OriginalCollision.walkDownSlope world.grid n.position n.velocity n.width n.height gravity
  let (wetEnemy, pool, rng, splashDust, splashSounds) ←
    waterContact world.grid { enemy with body := { n with velocity } } pool rng
  let n := move world.grid gravity wetEnemy.body
  let (n, nearby) := OriginalGreenSlime.checkActive world players (findFrame enemy.npcType n)
  let n := { n with netUpdate := false, justHit := false }
  let groan := if effect == some .zombieGroan then #[Sound.mk 14 1 (prepare enemy.body).position] else #[]
  return { enemy := { wetEnemy with body := n }, nearby, effect, rng, dustPool := pool
           dust := splashDust, sounds := groan ++ actionSounds ++ splashSounds }

end Lean4Terria.OriginalZombieVariants
