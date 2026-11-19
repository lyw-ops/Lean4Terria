import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalTownCombat

/-! Terraria 1.4.5.8 Old Shaking Chest (NPC685, BoundTownSlimeOld).
This is the pre-unlock friendly chest, not Elder Slime679. It has its own
jump multipliers and random turn; FindFrame leaves its sole frame unchanged.
Golden-key unlocking needs the caller's inventory, transform679 and world flag.
-/
namespace Lean4Terria.OriginalOldSlime
open OriginalCollision (Grid Vector single tileIndex)
open OriginalGreenSlime (Rng Slime targetClosest solidCollision)

def typeId : Int := 685
def elderTypeId : Int := 679
def keyItem : UInt16 := 327
def frameCount : Int := 1
def desktopFrameHeight : Int := 28
def textureWidth : Nat := 32

structure Enemy where
  body : Slime
  friendlyRegen : Int := 0
  /-- NPCID.Sets.TrailingMode[685] = 7, TrailCacheLength = 10. Updated after
  collision/FindFrame on every NPC update, including the update that despawns. -/
  oldPos : Array Vector := Array.replicate 10 ⟨0, 0⟩
  oldRot : Array Float32 := Array.replicate 10 0
  deriving Repr

/-- NPC.NewNPC pixel anchor; no name/item roll is made for the bound chest. -/
def spawn (x y : Int) : Enemy :=
  { body := { position := ⟨single x - 15, single (y - 28)⟩
              width := 30, height := 28, life := 250, lifeMax := 250
              damage := 0, defDamage := 0, defense := 10, defDefense := 10
              knockBackResist := 0.3, value := 0, timeLeft := 937, scale := 1, alpha := 0, color := ⟨0, 0, 0, 0⟩ } }

structure Step where
  enemy : Enemy
  rng : Rng
  nearby : Array (Nat × Float32) := #[]
  deriving Repr

/-- Checks performed AFTER Spawner.RollLuck(20), not before it. The caller
must still draw that roll when Skeletron is defeated but any later gate fails. -/
structure SpawnGate where
  waterTile : Bool
  deeperThanRockLayer : Bool
  spawnY : Int
  maxTilesY : Int
  unlockedOldSlime : Bool
  anotherOldChest : Bool
  deriving Repr

def eligibleAfterRoll (gate : SpawnGate) (roll : Int) : Bool :=
  roll == 0 && !gate.waterTile && gate.deeperThanRockLayer &&
  gate.spawnY < gate.maxTilesY - 210 && !gate.unlockedOldSlime && !gate.anotherOldChest

/-- Actor metadata for the ordinary shared collision/target lists. A friendly
zero-damage chest is neither a hostile attacker nor a valid player melee target. -/
def Enemy.toCombatNpc (enemy : Enemy) : OriginalStarterMelee.Npc :=
  let n := enemy.body
  { active := n.active, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax
    defense := n.defense, knockBackResist := n.knockBackResist, friendly := true
    justHit := n.justHit }

def ai (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player) (rng : Rng) (n : Slime) :
    Slime × Rng × Option String := Id.run do
  let grid := world.grid
  let mut n := n
  let mut random := rng
  if n.ai0 == -999 then
    return ({ n with frameY := 0, frameCounter := 0, rotation := 0 }, rng, some "ai[0] = -999 (statue frame)")
  let aggressive := !(world.dayTime && n.life == n.lifeMax &&
    !(n.position.y.toFloat > world.worldSurface * 16)) || world.slimeRain
  if n.ai2 > 1 then n := { n with ai2 := n.ai2 - 1 }
  if n.wet then return (n, rng, some "wet Old Shaking Chest")
  n := { n with aiAction := 0 }
  if n.ai2 == 0 then
    n := { n with ai0 := -100, ai2 := 1 }
    n := targetClosest players n
  if n.velocity.y == 0 then
    if n.collideY && n.oldVelocity.y != 0 && solidCollision grid n.position n.width n.height then
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
      if aggressive && n.ai2 == 1 then n := targetClosest players n
      if jump == 3 then
        n := { n with velocity.y := -8, velocity.x := n.velocity.x + single (3 * n.direction), ai0 := -200,
                      ai3 := n.position.x }
      else
        n := { n with velocity.y := -6, velocity.x := n.velocity.x + single (2 * n.direction), ai0 := -120 }
        n := { n with ai0 := if jump == 1 then n.ai0 + threshold else n.ai0 + threshold * 2 }
      -- AI_001 IL_4edf: this is a shaking chest, with reduced jumps
      -- and a fresh 50% turn after each jump (after assigning vx).
      n := { n with velocity := ⟨n.velocity.x * 0.2, n.velocity.y * 0.5⟩ }
      let (turn, next) := random.next 2
      random := next
      if turn == 0 then n := { n with direction := -n.direction }
    else if n.ai0 ≥ -30 then
      n := { n with aiAction := 1 }
  else if n.target < 255 && ((n.direction == 1 && n.velocity.x < 3) || (n.direction == -1 && n.velocity.x > -3)) then
    if n.collideX && n.velocity.x.abs == 0.2 then
      n := { n with position.x := n.position.x - 1.4 * single n.direction }
    if n.collideY && n.oldVelocity.y != 0 && solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if (n.direction == -1 && n.velocity.x.toFloat < 0.01) || (n.direction == 1 && n.velocity.x.toFloat > -0.01) then
      n := { n with velocity.x := n.velocity.x + 0.2 * single n.direction }
    else
      n := { n with velocity.x := n.velocity.x * 0.93 }
  return (n, random, none)


/-- One dry, pre-unlock NPC.UpdateNPC. Enemy contacts are an explicit boundary
until the chest-specific HitEffect/death path is supplied; it never hurts players. -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player)
    (rng : Rng) (enemy : Enemy) (others : Array OriginalTownCombat.Other := #[]) : Except String Step := do
  if !enemy.body.active then return { enemy, rng }
  if enemy.oldPos.size != 10 || enemy.oldRot.size != 10 then throw "Old Shaking Chest requires ten trail entries"
  if enemy.body.life ≤ 0 then throw "Old Shaking Chest death effects"
  if enemy.body.wet then throw "Old Shaking Chest already in liquid"
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world enemy.body
  let mut n := enemy.body
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  n := { n with oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY }
  let ease := 1 + n.velocity.x.abs / 3
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - ease * n.stepSpeed)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + ease * n.stepSpeed) else n.gfxOffY
  n := { n with gfxOffY := max (-16) (min 16 gfx) }
  let (next, rng, unsupported) := ai world players rng n
  if let some why := unsupported then throw why
  n := next
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t
                velocity.y := min maxFall (n.velocity.y + gravity) }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  let mut regen := enemy.friendlyRegen
  if n.life < n.lifeMax then
    regen := regen + 1
    if regen > 180 then
      regen := 0
      n := { n with life := n.life + 1 }
  if n.immune[255]?.getD 0 == 0 then
    let pixel (f : Float32) : Int := f.toInt32.toInt
    if others.any (fun o => o.active && !o.friendly && o.damage > 0 &&
        o.typeId != 624 && o.typeId != 690 &&
        pixel o.position.x < pixel n.position.x + n.width && pixel n.position.x < pixel o.position.x + o.width &&
        pixel o.position.y < pixel n.position.y + n.height && pixel n.position.y < pixel o.position.y + o.height) then
      throw "Old Shaking Chest was contacted by a hostile NPC"
  if (OriginalPlayerWet.wetCollision world.grid n.position n.width n.height).wet then
    throw "Old Shaking Chest entered liquid"
  n := OriginalGreenSlime.updateCollision world.grid gravity n
  -- FindFrame's type685 switch reaches its default tail. aiStyle1 does not
  -- enter any tail animation; frameY/frameCounter/rotation are left alone.
  let (checked, nearby) := OriginalGreenSlime.checkActive world players n
  let oldPos := #[n.position] ++ enemy.oldPos.extract 0 9
  let oldRot := #[n.rotation] ++ enemy.oldRot.extract 0 9
  return { enemy := { body := { checked with netUpdate := false, justHit := false }, friendlyRegen := regen, oldPos, oldRot }, rng, nearby }

structure NpcSprite where
  source : Int × Int × Int × Int
  position : Vector
  origin : Vector
  color : OriginalGreenSlime.Color
  rotation : Float32
  /-- DrawNPCDirect uses the Vector2 scale overload for NPC685. -/
  scale : Vector
  flip : Bool
  deriving BEq, Repr

private def litColor (n : Slime) (light : OriginalGreenSlime.Color) : OriginalGreenSlime.Color :=
  let factor : Float32 := single (255 - (n.alpha : Int)) / 255
  let channel (c : Nat) : Nat := (single c * factor).toInt32.toInt.toNat
  let a := max 0 (min 255 ((light.a : Int) - n.alpha))
  ⟨channel light.r, channel light.g, channel light.b, a.toNat⟩

/-- Main.DrawNPCDirect type685 branch, IL_f416–f746. Ordinary world draw only:
not a portrait/bestiary dummy, no shimmer transparency or multiplayer netOffset.
The caller supplies actual scene lighting; both source and origin use NPC_685.
The ground snapping is in screen coordinates after subtracting the camera. -/
def npcSprite (enemy : Enemy) (screen : Vector) (light : OriginalGreenSlime.Color) : NpcSprite := Id.run do
  let n := enemy.body
  let mut scale : Vector := ⟨n.scale, n.scale⟩
  let mut rotation := n.rotation
  let old := enemy.oldPos[9]?.getD ⟨0, 0⟩
  if (old.x != 0 || old.y != 0) && n.velocity.y == 0 then
    let delta : Vector := ⟨old.x - n.position.x, old.y - n.position.y⟩
    let length := (delta.x * delta.x + delta.y * delta.y).sqrt
    let tau : Float32 := 6.2831854820251465
    let sinX := ((delta.x / 30 * tau).toFloat.sin).toFloat32
    let sinY := ((delta.y / 30 * tau).toFloat.sin).toFloat32
    let amount := max 0 (min 1 (length / 30))
    scale := ⟨scale.x + sinX * amount * 0.25, scale.y + sinY * amount * 0.25⟩
    let wave := ((max 0 (min 1 (length / 20)) * tau).toFloat.sin).toFloat32
    rotation := rotation + wave * 0.7853981852531433 * 0.1 * single n.spriteDirection
  let origin : Vector := ⟨16, 14⟩
  let mut x := n.position.x - screen.x + single (n.width / 2) - 32 * n.scale / 2 + origin.x * n.scale
  let mut y := n.position.y - 2 - screen.y + single n.height - 28 * n.scale + 4 + origin.y * n.scale + n.gfxOffY
  if n.velocity.y == 0 then
    -- Terraria.Utils.Floor(Vector2) uses conv.i4, i.e. truncation toward zero.
    x := single x.toInt32.toInt
    y := single y.toInt32.toInt
    -- CIL rem uses truncation, including for negative screen coordinates.
    let remainder := x - single (x / 16).toInt32.toInt * 16
    if remainder < 2 then x := x - remainder
    if remainder > 14 then x := x + (16 - remainder)
  return { source := (0, n.frameY, 32, 28), position := ⟨x, y⟩, origin
           color := litColor n light, rotation, scale, flip := n.spriteDirection == 1 }

end Lean4Terria.OriginalOldSlime
