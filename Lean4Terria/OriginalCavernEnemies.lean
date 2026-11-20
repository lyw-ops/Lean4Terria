import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalForestCritter
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalCombat.Dust

/-!
# Ordinary cavern enemies (Terraria desktop 1.4.5.8)

The most common enemies that `OriginalUndergroundSpawn` births in an ordinary pre-hardmode forest
underground, in the order of the native birth corpus (`.lake/original-cavern-enemies/REPORT.md`).
Transcribed by hand from the current desktop CIL (`.lake/original-cavern-enemies/dump-axty8uro/il`,
desktop sha `d52c7ea7…3e0`). Evidence: `.lake/original-cavern-enemies/`.

## Cave Bat (NPC 49, `aiStyle` 14)

`SetDefaults(49)`, one `NPC.UpdateNPC` (the bat branch of `NPC.AI` IL_9cb0 … IL_bf57, the generic
update with `noGravity`, `UpdateCollision` with platforms fallen through, the bat branch of
`FindFrame`, `CheckActive`), `HitEffect`, and `NPCLoot` (its own rules after the global ones).

The NPC body is the shared `OriginalGreenSlime.Slime` record (position, velocity, `ai`, frame,
immunity…). The random streams are the caller's: `update` takes `Main.rand` as
`OriginalGreenSlime.Rng` (the bat's update draws nothing), the hit and loot entries take it as
`UnifiedRandom`, as the other enemy modules do. The generic `StrikeNPC` damage and knock-back
(`noGravity` uses 0.5) are `OriginalStarterMelee`'s.
-/
namespace Lean4Terria.OriginalCavernEnemies

open OriginalCollision (Grid Vector single)
open OriginalGreenSlime (Rng Slime World Player Step)
open OriginalForestGuideArrow (DustPool DustParticle)

/-! ## Shared pieces -/

/-- A cavern enemy: its body and the `NPC` flags the body record does not carry. -/
structure Enemy where
  npcType : Int
  body : Slime
  noGravity : Bool := false
  noTileCollide : Bool := false
  lastInteraction : Int := 255
  deriving Repr

/-- `Collision.CanHit(pos, w, h, pos2, w2, h2)`: `Utils.ToPoint` of both positions, half sizes added
(integer division), then the `Point` overload's tile walk (`OriginalForestCritter.canHit`, verified
there for that overload). -/
def canHit (grid : Grid) (pos : Vector) (w h : Nat) (pos2 : Vector) (w2 h2 : Nat) : Bool :=
  let pt := fun (v : Vector) (dx dy : Nat) =>
    (⟨single (v.x.toInt32.toInt + (dx / 2 : Nat)), single (v.y.toInt32.toInt + (dy / 2 : Nat))⟩ : Vector)
  OriginalForestCritter.canHit grid (pt pos w h) (pt pos2 w2 h2)

/-- `NPC.UpdateCollision` for a dry NPC whose `Collision_DecideFallThroughPlatforms` is `fall`:
`Collision_WalkDownSlopes`, `Collision_MoveNormal` (`TileCollision(fall, fall, 1, …, hoik)`),
`Collision_MoveWhileDry`, `Collision_MoveSlopesAndStairFall(fall)` (which first sets `stairFall`
when `fall`). -/
def updateCollision (grid : Grid) (gravity : Float32) (fall : Bool) (n : Slime) : Slime := Id.run do
  let mut n := n
  n := { n with velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height gravity }
  n := { n with oldVelocity := n.velocity, collideX := false, collideY := false }
  let hit := OriginalCollision.tileCollision grid n.position n.velocity n.width n.height
    { fallThrough := fall, fall2 := fall, hoik := true }
  n := { n with velocity := hit.displacement }
  if hit.hitAbove then n := { n with velocity.y := 0.01 }
  if n.oldVelocity.x != n.velocity.x then n := { n with collideX := true }
  if n.oldVelocity.y != n.velocity.y then n := { n with collideY := true }
  n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  if fall then n := { n with stairFall := true }
  let slope := OriginalCollision.slopeCollision grid n.position n.velocity n.width n.height n.stairFall
  n := { n with stairFall := slope.stairFall || fall }
  if slope.stair && (slope.position.y - n.position.y).abs > 8 then
    n := { n with gfxOffY := n.gfxOffY - (slope.position.y - n.position.y), stepSpeed := 2 }
  n := { n with position := slope.position + ⟨0, 0⟩, velocity := slope.velocity }
  return n

/-- The generic head of `NPC.UpdateNPC` up to `AI()` for an active NPC with `life > 0`. -/
private def preAI (n : Slime) : Slime := Id.run do
  let mut n := n
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
  return n

/-- `NewGore(position, velocity, type, scale)` (child safety off): the first free slot of 600 (none:
nothing drawn), then `Next(10, 31)` and `Next(-20, 21)` for the velocity; a gore outside the world is
allocated inactive. The texture size matters only to its later update / draw. -/
structure Gore where
  slot : Nat
  typeId : Nat
  position : Vector
  velocity : Vector
  scale : Float32 := 1
  active : Bool := true
  deriving Repr

def newGore (grid : Grid) (occupied : Array Bool) (typeId : Nat) (position velocity : Vector)
    (scale : Float32) (random : UnifiedRandom) : Array Bool × Option Gore × UnifiedRandom := Id.run do
  let slot := (occupied.findIdx? (!·)).getD occupied.size
  if slot ≥ 600 then return (occupied, none, random)
  let (up, random) := random.next 10 31
  let (side, random) := random.next (-20) 21
  let x := position.x.toInt32.toInt / 16
  let y := position.y.toInt32.toInt / 16
  let active := 0 ≤ x && x < (grid.width : Int) && 0 ≤ y && y < (grid.height : Int)
  let g : Gore := { slot, typeId, position, scale, active
                    velocity := ⟨velocity.x + single side * 0.1, velocity.y - single up * 0.1⟩ }
  let occupied := occupied ++ Array.replicate (slot + 1 - occupied.size) false
  return (occupied.set! slot active, if active then some g else none, random)

structure HitEffects where
  pool : DustPool
  dust : Array DustParticle := #[]
  goreOccupied : Array Bool := Array.replicate 600 false
  gore : Array Gore := #[]
  random : UnifiedRandom

/-- `Item.Prefix(-1)` with a declared pool and rejection list (read natively with
`GetRollablePrefixes` / `TryGetPrefixStatMultipliersForItem`): one in four plain, else a pool pick,
the reduced-chance prefixes kept on `Next(3) == 0`, a rejected pick starts over. -/
def naturalPrefix (pool rejected : Array UInt8) (random : UnifiedRandom) : UInt8 × UnifiedRandom := Id.run do
  if pool.isEmpty then return (0, random)
  let mut random := random
  repeat
    let (plain, next) := random.next 0 4
    if plain == 0 then return (0, next)
    let (index, next) := next.next 0 pool.size
    let chosen := pool[index.toNat]!
    random := next
    if OriginalRecipeCatalog.CraftData.reducedNaturalChance.contains chosen then
      let (keep, next) := random.next 0 3
      random := next
      if keep != 0 then return (0, random)
    if !rejected.contains chosen then return (chosen, random)
  return (0, random)

/-- One `CommonDrop(item, 1/denominator, low..high)`: `player.RollLuck(denominator) < 1`, the amount
`Next(low, high + 1)`, then `Item.NewItem` with a natural prefix (an empty pool draws nothing). -/
def commonDropRange (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16) (denominator : Int)
    (low high : Nat) (pool rejected : Array UInt8) (loot : OriginalCombat.Loot.Result) : OriginalCombat.Loot.Result :=
  let (roll, loot) := OriginalCombat.Loot.rollLuck env.luck denominator loot
  if roll ≥ 1 then loot else
  let (amount, random) := loot.random.next low (high + 1)
  let (prefixId, random) := naturalPrefix pool rejected random
  let (drop, random) := OriginalItems.rollDropAt position
    { OriginalItems.Stack.ofItem item amount.toNat with prefixId } random env.grid
  { drops := loot.drops.push drop, random }

/-- One `CommonDrop(item, 1/denominator, 1..1)`. -/
def commonDrop (env : OriginalCombat.Loot.Context) (position : Vector) (item : UInt16) (denominator : Int)
    (pool rejected : Array UInt8) (loot : OriginalCombat.Loot.Result) : OriginalCombat.Loot.Result :=
  commonDropRange env position item denominator 1 1 pool rejected loot

/-- `Dust.NewDust` frames for types ≥ 100: the type steps down by 100 while the frame moves
1000 left and 30 down per step (`NewDust` IL_0315). `DustPool.create` gives `10 · type`. -/
def wrapDustFrame (d : OriginalForestGuideArrow.DustParticle) : OriginalForestGuideArrow.DustParticle :=
  let k : Int := d.request.typeId / 100
  { d with frame := { d.frame with x := d.frame.x - 1000 * k, y := d.frame.y + 30 * k } }

/-! ### `Vector2` arithmetic as FNA does it (`float` throughout) -/

private def vsub (a b : Vector) : Vector := ⟨a.x - b.x, a.y - b.y⟩
private def vscale (a : Vector) (k : Float32) : Vector := ⟨a.x * k, a.y * k⟩
/-- `op_Division(Vector2, float)`: a true division per component. -/
private def vdiv (a : Vector) (k : Float32) : Vector := ⟨a.x / k, a.y / k⟩
/-- `Length()`: `(float)Math.Sqrt(X*X + Y*Y)` with the sum in `float`. -/
private def vlength (a : Vector) : Float32 := (a.x * a.x + a.y * a.y).sqrt
/-- `Normalize()`: `1 / (float)Math.Sqrt(X*X + Y*Y)`, then a multiplication per component. -/
private def vnormalize (a : Vector) : Vector :=
  let inv : Float32 := 1 / (a.x * a.x + a.y * a.y).sqrt
  ⟨a.x * inv, a.y * inv⟩

/-! ## Cave Bat -/

namespace CaveBat

def typeId : Int := 49
/-- `Main.npcFrameCount[49]`; the flight cycle uses the first four. -/
def frameCount : Int := 5
/-- `NPC_49` is 44 × 160: frame height 32 on the desktop client. -/
def desktopFrameHeight : Int := 32
def hitSound : Nat × Nat := (3, 1)
def deathSound : Nat × Nat := (4, 4)
def bannerId : Int := 49

/-- `NPC.NewNPC(…, 49, …)` through `Spawner.SpawnNPC`: `SetDefaults(49)`, `Bottom = (x, y)`,
`timeLeft = (int)(activeTime * 1.25)`. (Native birth record: 22 × 18, life 16, damage 13, defense 2,
knock-back resist 0.8, value 90, npcSlots 0.5, alpha 0, scale 1; `noGravity` is set by the AI.) -/
def spawn (world : World) (x y : Int) (target : Int := 255) : Slime :=
  let w : Nat := 22
  let h : Nat := 18
  { position := ⟨single x - single w / 2, single y - single h⟩, width := w, height := h
    life := 16, lifeMax := 16, damage := 13, defDamage := 13, defense := 2, defDefense := 2
    knockBackResist := Float32.ofBits 1061997773, value := 90, scale := 1, alpha := 0
    npcSlots := 0.5, color := ⟨0, 0, 0, 0⟩, netId := 49, target
    timeLeft := (Float.ofInt world.activeTime * 1.25).toInt64.toInt }

/-- One of the two identical horizontal / vertical steering blocks of the bat AI (IL_a8a1 and
IL_afa7). The `double` comparisons against ±1.5 are exact in `float`. -/
private def steer (n : Slime) : Slime := Id.run do
  let mut vx := n.velocity.x
  let mut vy := n.velocity.y
  if n.direction == -1 && vx > -4 then
    vx := vx - 0.1
    if vx > 4 then vx := vx - 0.1
    else if vx > 0 then vx := vx + 0.05
    if vx < -4 then vx := -4
  else if n.direction == 1 && vx < 4 then
    vx := vx + 0.1
    if vx < -4 then vx := vx + 0.1
    else if vx < 0 then vx := vx - 0.05
    if vx > 4 then vx := 4
  if n.directionY == -1 && vy > -1.5 then
    vy := vy - 0.04
    if vy > 1.5 then vy := vy - 0.05
    else if vy > 0 then vy := vy + 0.03
    if vy < -1.5 then vy := -1.5
  else if n.directionY == 1 && vy < 1.5 then
    vy := vy + 0.04
    if vy < -1.5 then vy := vy + 0.05
    else if vy < 0 then vy := vy - 0.03
    if vy > 1.5 then vy := 1.5
  return { n with velocity := ⟨vx, vy⟩ }

/-- The bat branch of `NPC.AI` (`aiStyle` 14, type 49) for a dry bat: the wall / floor bounce, the
target, the two steering blocks, and the erratic flight after 200 updates without a clear line to
the target. Returns `noGravity = true` with the body. -/
def ai (grid : Grid) (players : Array Player) (targetWet : Bool) (n : Slime) : Slime := Id.run do
  let mut n := n
  -- IL_9cc4: bounce off walls and floors
  if n.collideX then
    let mut vx := n.oldVelocity.x * -0.5
    if n.direction == -1 && vx > 0 && vx < 2 then vx := 2
    if n.direction == 1 && vx < 0 && vx > -2 then vx := -2
    n := { n with velocity.x := vx }
  if n.collideY then
    let mut vy := n.oldVelocity.y * -0.5
    if vy > 0 && vy < 1 then vy := 1
    if vy < 0 && vy > -1 then vy := -1
    n := { n with velocity.y := vy }
  -- IL_9eb1
  n := OriginalGreenSlime.targetClosest players n
  -- IL_a8a1 and (after the dry-only wet test, IL_ac1f) IL_afa7: the same steering twice
  n := steer n
  n := steer n
  -- IL_b4e0: ai[1] counts up; past 200 the bat flies erratically until it can see a dry target
  n := { n with ai1 := n.ai1 + 1 }
  if n.ai1 > 200 then
    let p := OriginalGreenSlime.playerAt players n.target
    if !targetWet && canHit grid n.position n.width n.height p.position p.width p.height then
      n := { n with ai1 := 0 }
    if n.ai1 > 1000 then n := { n with ai1 := 0 }
    n := { n with ai2 := n.ai2 + 1 }
    if n.ai2 > 0 then
      if n.velocity.y < 1.5 then n := { n with velocity.y := n.velocity.y + 0.1 }
    else if n.velocity.y > -1.5 then n := { n with velocity.y := n.velocity.y - 0.1 }
    if n.ai2 < -150 || n.ai2 > 150 then
      if n.velocity.x < 4 then n := { n with velocity.x := n.velocity.x + 0.2 }
    else if n.velocity.x > -4 then n := { n with velocity.x := n.velocity.x - 0.2 }
    if n.ai2 > 300 then n := { n with ai2 := -300 }
  return n

/-- The bat branch of `NPC.FindFrame` (IL_10f54): sprite direction from the velocity, rotation
`0.1 * vx`, a new frame every 6 updates over `npcFrameCount - 1` frames. -/
def findFrame (frameHeight : Int) (n : Slime) : Slime := Id.run do
  let mut n := n
  if n.velocity.x > 0 then n := { n with spriteDirection := 1 }
  if n.velocity.x < 0 then n := { n with spriteDirection := -1 }
  n := { n with rotation := n.velocity.x * 0.1, frameCounter := n.frameCounter + 1 }
  if n.frameCounter ≥ 6 then n := { n with frameY := n.frameY + frameHeight, frameCounter := 0 }
  if n.frameY ≥ frameHeight * (frameCount - 1) then n := { n with frameY := 0 }
  return n

/-- One `NPC.UpdateNPC(i)` for an active, dry Cave Bat (`world.frameHeight` is the desktop 32, or
1 on a dedicated server). `targetWet` is `Main.player[target].wet` after the AI's `TargetClosest`
(only read past 200 updates). Nothing in this update draws from `Main.rand`. -/
def update (world : World) (players : Array Player) (rng : Rng) (n : Slime) (targetWet : Bool := false) : Step := Id.run do
  let (gravity, _) := OriginalGreenSlime.gravityFor world n
  if !n.active then return { slime := n, rng, gravity := none }
  let mut n := n
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  if n.life ≤ 0 then
    return { slime := { n with active := false, netUpdate := false, justHit := false }, rng, gravity := some gravity }
  if n.wet then return { slime := n, rng, gravity := some gravity, unsupported := some "Cave Bat in liquid" }
  n := preAI n
  n := ai world.grid players targetWet n
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  -- noGravity: no gravity step; the small horizontal speed is still cleared
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  n := updateCollision world.grid gravity true n
  n := findFrame world.frameHeight n
  let (checked, nearby) := OriginalGreenSlime.checkActive world players n
  n := { checked with netUpdate := false, justHit := false }
  return { slime := n, rng, gravity := some gravity, nearby }

def toEnemy (n : Slime) : Enemy := { npcType := typeId, body := n, noGravity := true }

/-- `NPC.HitEffect(hitDirection, dmg)` for a bat: alive, `NewDust(position, w, h, 5, dir, -1)` while
`i < dmg / lifeMax * 30` (in `double`); dead, 15 × `NewDust(…, 5, 2 * dir, -2)` and gore 82 at the
position with the bat's velocity. -/
def hitEffect (grid : Grid) (pool : DustPool) (goreOccupied : Array Bool) (n : Slime)
    (direction : Int) (dealt : Float) (random : UnifiedRandom) : HitEffects := Id.run do
  let mut out : HitEffects := { pool, goreOccupied, random }
  if !n.active then return out
  let request := fun (speed : Vector) =>
    ({ position := n.position, width := n.width, height := n.height, typeId := 5, speed } :
      OriginalForestGuideArrow.DustRequest)
  if n.life > 0 then
    let mut i : Nat := 0
    while Float.ofNat i < dealt / Float.ofInt n.lifeMax * 30 do
      let (pool, particle, random) := out.pool.create (request ⟨single direction, -1⟩) out.random
      out := { out with pool, random, dust := particle.elim out.dust out.dust.push }
      i := i + 1
    return out
  for _ in [:15] do
    let (pool, particle, random) := out.pool.create (request ⟨single (2 * direction), -2⟩) out.random
    out := { out with pool, random, dust := particle.elim out.dust out.dust.push }
  let (occupied, gore, random) := newGore grid out.goreOccupied 82 n.position n.velocity 1 out.random
  return { out with goreOccupied := occupied, gore := gore.elim out.gore out.gore.push, random }

/-- `NPCLoot` for a bat in an ordinary pre-hardmode world (not remix, not Don't Starve): the global
rules draw nothing here; then Chain Knife 1325 (1/250), Depth Meter 18 (1/100), item 5097 (1/300),
each `CommonDrop`, then the coins and the heart / star rolls. Prefix pools read natively. -/
def dropLoot (env : OriginalCombat.Loot.Context) (n : Slime) (random : UnifiedRandom) : OriginalCombat.Loot.Result := Id.run do
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let mut out : OriginalCombat.Loot.Result := { random }
  out := commonDrop env position 1325 250 #[36, 37, 38, 53, 54, 55, 39, 40, 56, 41, 57, 59, 60, 61] #[] out
  out := commonDrop env position 18 100 #[62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80] #[] out
  out := commonDrop env position 5097 300
    #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 36, 37, 38, 53, 54, 55, 39, 40, 56, 41, 57,
      42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 59, 60, 61, 81] #[] out
  out := OriginalCombat.Loot.dropMoneyAt env position n.value out
  return OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) out

end CaveBat

/-! ## Granite Elemental (NPC 483, `aiStyle` 91) -/

namespace GraniteElemental

def typeId : Int := 483
/-- `Main.npcFrameCount[483]`. -/
def frameCount : Int := 22
/-- `NPC_483` is 60 × 1320: frame height 60 on the desktop client. -/
def desktopFrameHeight : Int := 60
def hitSound : Nat × Nat := (3, 7)
def deathSound : Nat × Nat := (4, 6)
def bannerId : Int := 483

/-- The elemental's body plus the `NPC` fields its AI writes that `Slime` does not carry. -/
structure Granite where
  body : Slime
  /-- `localAI[0]`: updates since the last free-space path probe in the bounce state. -/
  localAI0 : Float32 := 0
  noGravity : Bool := false
  noTileCollide : Bool := false
  dontTakeDamage : Bool := false
  deriving Repr

/-- `NPC.NewNPC(…, 483, …)` through `Spawner.SpawnNPC`: `SetDefaults(483)` (IL_cb05: 20 × 30, `aiStyle`
91, damage 24, defense 8, life 40, knock-back resist 0.6, value 1000, npcSlots 0.5), `Bottom = (x, y)`,
`timeLeft = (int)(activeTime * 1.25)`. Draws nothing. `noGravity` is first set by the AI. -/
def spawn (world : World) (x y : Int) (target : Int := 255) : Granite :=
  let w : Nat := 20
  let h : Nat := 30
  { body := { position := ⟨single x - single w / 2, single y - single h⟩, width := w, height := h
              life := 40, lifeMax := 40, damage := 24, defDamage := 24, defense := 8, defDefense := 8
              knockBackResist := Float32.ofBits 1058642330, value := 1000, scale := 1, alpha := 0
              npcSlots := 0.5, color := ⟨0, 0, 0, 0⟩, netId := 483, target
              timeLeft := (Float.ofInt world.activeTime * 1.25).toInt64.toInt } }

/-- The chosen detour point: `ai[0] = 3`, `ai[1..2] = point`. -/
private def detour (n : Slime) (point : Vector) : Slime := { n with ai0 := 3, ai1 := point.x, ai2 := point.y }

/-- The `aiStyle` 91 branch of `NPC.AI` (IL_418a6 … IL_4236e). `expert` is `Main.expertMode`; only
there a hit (`justHit`) draws `Main.rand.Next(6)` and may start the shell state `ai[0] = -1`.
States: 0 look for the target (straight line → 1; else a horizontal / vertical detour point → 3;
else a push towards it → 4), 1 chase in sight, 2 phase through tiles (not entered by this AI),
3 fly to the detour point, 4 bounce around until the target is in sight, -1 shell (expert). -/
def ai (grid : Grid) (players : Array Player) (expert : Bool) (rng : Rng) (g : Granite) : Granite × Rng := Id.run do
  let mut g := { g with noGravity := true, noTileCollide := false, dontTakeDamage := false }
  let mut n := g.body
  let mut rng := rng
  -- IL_418c8: an expert hit may curl up
  if n.justHit && expert then
    let (roll, next) := rng.next 6
    rng := next
    if roll == 0 then n := { n with netUpdate := true, ai0 := -1, ai1 := 0 }
  if n.ai0 == -1 then
    -- IL_4191f: shell; falls with gravity, slides to a stop, uncurls after 120 updates
    g := { g with dontTakeDamage := true, noGravity := false }
    n := { n with velocity.x := n.velocity.x * 0.98, ai1 := n.ai1 + 1 }
    if !(n.ai1 < 120) then n := { n with ai0 := 0, ai1 := 0, ai2 := 0, ai3 := 0 }
    return ({ g with body := n }, rng)
  if n.ai0 == 0 then
    -- IL_419b9
    n := OriginalGreenSlime.targetClosest players n
    let p := OriginalGreenSlime.playerAt players n.target
    if canHit grid n.center 1 1 p.center 1 1 then
      return ({ g with body := { n with ai0 := 1 } }, rng)
    let push := vsub p.center n.center
    let push : Vector := ⟨push.x, push.y - single (p.height / 4 : Nat)⟩
    let c := n.center
    let across : Vector := ⟨p.center.x, c.y⟩
    let gap := vsub across c
    if vlength gap > 8 && canHit grid c 1 1 across 1 1 then
      n := detour n across
      let down : Vector := ⟨c.x, p.center.y⟩
      -- the original reuses the horizontal gap's length here (IL_41b1e)
      if vlength gap > 8 && canHit grid c 1 1 down 1 1 && canHit grid down 1 1 p.position 1 1 then
        n := detour n down
    else
      let down : Vector := ⟨c.x, p.center.y⟩
      if vlength (vsub down c) > 8 && canHit grid c 1 1 down 1 1 then n := detour n down
    if n.ai0 == 0 then
      g := { g with localAI0 := 0 }
      n := { n with velocity := n.velocity + vscale (vnormalize push) 0.5, ai0 := 4, ai1 := 0 }
    return ({ g with body := n }, rng)
  if n.ai0 == 1 then
    -- IL_41cce: chase, speed 2 + distance / 200, inertia 50
    let p := OriginalGreenSlime.playerAt players n.target
    let v := vsub p.center n.center
    let speed : Float32 := 2 + vlength v / 200
    let v := vscale (vnormalize v) speed
    n := { n with velocity := vdiv (vscale n.velocity 49 + v) 50 }
    if !canHit grid n.center 1 1 p.center 1 1 then n := { n with ai0 := 0, ai1 := 0 }
    return ({ g with body := n }, rng)
  if n.ai0 == 2 then
    -- IL_41dd2: through tiles, speed 2, inertia 4, back to 0 near the target in free space
    g := { g with noTileCollide := true }
    let p := OriginalGreenSlime.playerAt players n.target
    let v := vsub p.center n.center
    let distance := vlength v
    n := { n with velocity := vdiv (vscale n.velocity 3 + vscale (vnormalize v) 2) 4 }
    if distance < 600 && !OriginalGreenSlime.solidCollision grid n.position n.width n.height then
      n := { n with ai0 := 0 }
    return ({ g with body := n }, rng)
  if n.ai0 == 3 then
    -- IL_41ebd: fly to the detour point, speed 1, inertia 3
    let p := OriginalGreenSlime.playerAt players n.target
    let v := vsub ⟨n.ai1, n.ai2⟩ n.center
    let distance := vlength v
    n := { n with velocity := vdiv (vscale n.velocity 2 + vscale (vnormalize v) 1) 3 }
    if n.collideX || n.collideY then n := { n with ai0 := 4, ai1 := 0 }
    if distance < 1 || distance > 800 || canHit grid n.center 1 1 p.center 1 1 then n := { n with ai0 := 0 }
    return ({ g with body := n }, rng)
  if n.ai0 == 4 then
    -- IL_41ff5: bounce, speed 1.5, inertia 20, give up after 180
    let p := OriginalGreenSlime.playerAt players n.target
    if n.collideX then n := { n with velocity.x := n.velocity.x * -0.8 }
    if n.collideY then n := { n with velocity.y := n.velocity.y * -0.8 }
    if n.velocity.x == 0 && n.velocity.y == 0 then
      let v := vsub p.center n.center
      let v : Vector := ⟨v.x, v.y - single (p.height / 4 : Nat)⟩
      n := { n with velocity := vscale (vnormalize v) 0.1 }
    n := { n with velocity := vdiv (vscale n.velocity 19 + vscale (vnormalize n.velocity) 1.5) 20 }
    n := { n with ai1 := n.ai1 + 1 }
    if n.ai1 > 180 then n := { n with ai0 := 0, ai1 := 0 }
    if canHit grid n.center 1 1 p.center 1 1 then n := { n with ai0 := 0 }
    g := { g with localAI0 := g.localAI0 + 1 }
    if !(g.localAI0 < 5) &&
        !OriginalGreenSlime.solidCollision grid (vsub n.position ⟨10, 10⟩) (n.width + 20) (n.height + 20) then
      g := { g with localAI0 := 0 }
      let c := n.center
      let across : Vector := ⟨p.center.x, c.y⟩
      if canHit grid c 1 1 across 1 1 && canHit grid c 1 1 across 1 1 && canHit grid p.center 1 1 across 1 1 then
        return ({ g with body := detour n across }, rng)
      let down : Vector := ⟨c.x, p.center.y⟩
      if canHit grid c 1 1 down 1 1 && canHit grid p.center 1 1 down 1 1 then
        return ({ g with body := detour n down }, rng)
  return ({ g with body := n }, rng)

/-- The granite branch of `NPC.FindFrame` (IL_11ac4): a frame every 5 updates; frames 0‥11 in flight with
rotation `0.1 vx`, 13‥21 curled up (rotation += `0.2 vx`). The sprite direction is `TargetClosest`'s. -/
def findFrame (frameHeight : Int) (n : Slime) : Slime := Id.run do
  let mut n := { n with frameCounter := n.frameCounter + 1 }
  if n.frameCounter > 4 then n := { n with frameY := n.frameY + frameHeight, frameCounter := 0 }
  if n.ai0 == -1 then
    if n.frameY > frameHeight * 21 then n := { n with frameY := frameHeight * 21 }
    else if n.frameY < frameHeight * 13 then n := { n with frameY := frameHeight * 13 }
    return { n with rotation := n.rotation + n.velocity.x * 0.2 }
  if n.frameY > frameHeight * 11 then n := { n with frameY := 0 }
  return { n with rotation := n.velocity.x * 0.1 }

/-- One `NPC.UpdateNPC(i)` for an active, dry Granite Elemental. `expert` is `Main.expertMode`
(a hit there draws `Next(6)` from `rng`, the caller's `Main.rand`); otherwise nothing is drawn.
The gravity step runs only in the shell state (`noGravity` false); `IsTileLoaded` is the world bounds
for a single-player world. In state 2 the position moves without tile collision. -/
def update (world : World) (players : Array Player) (rng : Rng) (g : Granite) (expert : Bool := false) :
    Granite × Step := Id.run do
  let n := g.body
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world n
  if !n.active then return (g, { slime := n, rng, gravity := none })
  let mut n := n
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  if n.life ≤ 0 then
    let dead := { n with active := false, netUpdate := false, justHit := false }
    return ({ g with body := dead }, { slime := dead, rng, gravity := some gravity })
  if n.wet then
    return ({ g with body := n }, { slime := n, rng, gravity := some gravity, unsupported := some "Granite Elemental in liquid" })
  n := preAI n
  let (g', rng) := ai world.grid players expert rng { g with body := n }
  let mut g := g'
  n := g.body
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  let mut gravity := gravity
  if !g.noGravity then
    if !g.noTileCollide then
      let cx := (n.position.x + single (n.width / 2 : Nat)).toInt32.toInt / 16
      let cy := (n.position.y + single (n.height / 2 : Nat)).toInt32.toInt / 16
      if !(0 ≤ cx && cx < (world.grid.width : Int) && 0 ≤ cy && cy < (world.grid.height : Int)) then
        gravity := 0
        n := { n with velocity := ⟨0, 0⟩ }
    n := { n with velocity.y := n.velocity.y + gravity }
    if n.velocity.y > maxFall then n := { n with velocity.y := maxFall }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  if !g.noTileCollide then
    n := updateCollision world.grid gravity false n
  else
    n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  n := findFrame world.frameHeight n
  let (checked, nearby) := OriginalGreenSlime.checkActive world players n
  n := { checked with netUpdate := false, justHit := false }
  g := { g with body := n }
  return (g, { slime := n, rng, gravity := some gravity, nearby })

/-- `Enemy` view for `OriginalStarterMelee` (`StrikeNPC` damage / knock-back). -/
def toEnemy (g : Granite) : Enemy :=
  { npcType := typeId, body := g.body, noGravity := g.noGravity, noTileCollide := g.noTileCollide }

/-- `NPC.HitEffect(hitDirection, dmg)` for 482 / 483 (IL_d97b): alive, `NewDust(…, 240, dir, -1)` while
`i < dmg / (lifeMax * 50)` (in `double`, so one dust for any positive damage at life 40); dead, 30 ×
`NewDust(…, 240, 2.5 * dir, -2.5)`. No gore for 483. -/
def hitEffect (pool : DustPool) (n : Slime) (direction : Int) (dealt : Float) (random : UnifiedRandom) : HitEffects := Id.run do
  let mut out : HitEffects := { pool, random }
  if !n.active then return out
  let request := fun (speed : Vector) =>
    ({ position := n.position, width := n.width, height := n.height, typeId := 240, speed } :
      OriginalForestGuideArrow.DustRequest)
  let emit := fun (out : HitEffects) (speed : Vector) =>
    let (pool, particle, random) := out.pool.create (request speed) out.random
    { out with pool, random, dust := particle.elim out.dust (out.dust.push ∘ wrapDustFrame) }
  if n.life > 0 then
    let mut i : Nat := 0
    while Float.ofNat i < dealt / Float.ofInt (n.lifeMax * 50) do
      out := emit out ⟨single direction, -1⟩
      i := i + 1
    return out
  for _ in [:30] do
    out := emit out ⟨2.5 * single direction, -2.5⟩
  return out

/-- `NPCLoot` for a Granite Elemental in an ordinary pre-hardmode world: its rules in database order
(read natively with `GetRulesForNPCID(483, false)`): 4036 1/50 unless spawned from a statue
(`ItemDropWithConditionRule` `NotFromStatue`), Granite Block 3086 × 5‥10 (1/1, still one `RollLuck(1)`),
6167 1/80 (prefix pool 62‥80), Night Vision Helmet 3109 1/30, 4400 1/20; then coins (value 1000) and the
heart / star rolls. -/
def dropLoot (env : OriginalCombat.Loot.Context) (n : Slime) (random : UnifiedRandom) (fromStatue : Bool := false) :
    OriginalCombat.Loot.Result := Id.run do
  let position := OriginalCombat.Loot.dropPosition n.position n.width n.height
  let mut out : OriginalCombat.Loot.Result := { random }
  if !fromStatue then out := commonDrop env position 4036 50 #[] #[] out
  out := commonDropRange env position 3086 1 5 10 #[] #[] out
  out := commonDrop env position 6167 80 #[62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80] #[] out
  out := commonDrop env position 3109 30 #[] #[] out
  out := commonDrop env position 4400 20 #[] #[] out
  out := OriginalCombat.Loot.dropMoneyAt env position n.value out
  return OriginalCombat.Loot.dropHealsAt env position (n.lifeMax > 1 && n.damage > 0) out

end GraniteElemental

end Lean4Terria.OriginalCavernEnemies
