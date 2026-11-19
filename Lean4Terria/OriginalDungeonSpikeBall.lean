import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalForestChatLayout
import Lean4Terria.OriginalAngryBones

/-! Terraria 1.4.5.8 Classic NPC70 (NPCID.SpikeBall), AI style20.
This dungeon hazard is invulnerable, has no gravity and passes through terrain.
The first AI update sets its movement anchor and consumes exactly one Next(15).
Subsequent motion preserves the original Float32 thresholds and branch order.
-/
namespace Lean4Terria.OriginalDungeonSpikeBall
open OriginalCollision (Vector single)
open OriginalGreenSlime (Slime Rng)

def typeId : Int := 70
def aiStyle : Int := 20
def textureWidth : Nat := 34
def desktopFrameHeight : Int := 34
def frameCount : Int := 1

structure Enemy where
  body : Slime
  deriving Repr

/-- Actual NewNPC pixel anchor. SetDefaults scales 34×34 to an integer 51×51. -/
def spawn (x y : Int) : Enemy :=
  { body := { position := ⟨single x - 25.5,single (y - 51)⟩
              width := 51,height := 51,scale := 1.5,npcSlots := 0.3
              life := 100,lifeMax := 100,damage := 32,defDamage := 32,defense := 100,defDefense := 100
              knockBackResist := 0,value := 0,alpha := 0,color := ⟨0,0,0,0⟩,timeLeft := 937 } }

structure Step where
  enemy : Enemy
  rng : Rng
  nearby : Array (Nat × Float32) := #[]
  deriving Repr

def Enemy.toCombatNpc (e : Enemy) : OriginalStarterMelee.Npc :=
  let n := e.body
  { active := n.active,position := n.position,velocity := n.velocity,width := n.width,height := n.height
    life := n.life,lifeMax := n.lifeMax,defense := n.defense,knockBackResist := 0
    friendly := false,dontTakeDamage := true,noGravity := true,noTileCollide := true
    justHit := n.justHit,immune := n.immune[0]?.getD 0 |>.toNat }

def Enemy.contactSource (e : Enemy) : Option (Int × String) :=
  if e.body.active && e.body.damage > 0 then some (e.body.damage,"Spike Ball") else none

/-- Standard NPC contact damage, including full blood-dust allocation and RNG.
NPC70 has no Cursed/status roll. The caller supplies the actual shared dust pool. -/
def contact (enemy : Enemy) (body : OriginalPlayerCollision.Body)
    (health : OriginalPlayerLiquidHealth.Health) (defense : Int)
    (pool : OriginalForestGuideArrow.DustPool) (rng : UnifiedRandom)
    (text : Option OriginalCombatTextChannel.Channel := none)
    (client : OriginalPlayerHurt.Client := {}) : Option OriginalAngryBones.ContactHit :=
  OriginalAngryBones.contactNpc enemy.toCombatNpc enemy.body.damage body health defense pool rng text client

/-- NPC.NearSpikeBall uses the AI anchor rectangle, not the moving hitbox.
The caller's array contains all active AI20 actors, including not-yet-updated
births whose ai1/ai2 are still zero. Rectangle.Intersects has strict edges. -/
def nearSpikeBall (tileX tileY : Int) (enemies : Array Enemy) : Bool :=
  let x := tileX * 16 - 300
  let y := tileY * 16 - 300
  enemies.any fun e =>
    let ax := e.body.ai1.toInt32.toInt
    let ay := e.body.ai2.toInt32.toInt
    e.body.active && x < ax + 20 && ax < x + 600 && y < ay + 20 && ay < y + 600

/-- NPC.AI IL_f00d–f391, ordinary single-player branch. -/
def ai (players : Array OriginalGreenSlime.Player) (rng : Rng) (input : Slime) : Slime × Rng := Id.run do
  let mut n := input
  if n.ai0 == 0 then
    n := OriginalGreenSlime.targetClosest players n
    n := { n with direction := -n.direction,directionY := -n.directionY }
    n := { n with position.y := n.position.y + single (n.height / 2 + 8) }
    n := { n with ai1 := n.position.x + single (n.width / 2),ai2 := n.position.y + single (n.height / 2) }
    if n.direction == 0 then n := { n with direction := 1 }
    if n.directionY == 0 then n := { n with directionY := 1 }
    let (roll,rng) := rng.next 15
    n := { n with ai3 := 1 + single roll * 0.1 }
    n := { n with velocity.y := single (n.directionY * 6) * n.ai3,ai0 := n.ai0 + 1,netUpdate := true }
    return (n,rng)
  let speed : Float32 := 6 * n.ai3
  let acceleration : Float32 := 0.2 * n.ai3
  -- The original converts this quotient to int before comparing with ai0.
  -- It is not always 15 after Float32 rounding.
  let limit := single (speed / acceleration / 2).toInt32.toInt
  if n.ai0 ≥ 1 && n.ai0 < limit then
    n := { n with velocity.y := single n.directionY * speed,ai0 := n.ai0 + 1 }
    return (n,rng)
  if n.ai0 ≥ limit then
    n := { n with velocity.y := 0,directionY := -n.directionY
                  velocity.x := speed * single n.direction,ai0 := -1 }
    return (n,rng)
  if n.directionY > 0 then
    if n.velocity.y ≥ speed then n := { n with directionY := -n.directionY,velocity.y := speed }
  else if n.directionY < 0 then
    if n.velocity.y ≤ -speed then n := { n with directionY := -n.directionY,velocity.y := -speed }
  if n.direction > 0 then
    if n.velocity.x ≥ speed then n := { n with direction := -n.direction,velocity.x := speed }
  else if n.direction < 0 then
    if n.velocity.x ≤ -speed then n := { n with direction := -n.direction,velocity.x := -speed }
  n := { n with velocity := ⟨n.velocity.x + acceleration * single n.direction,
                              n.velocity.y + acceleration * single n.directionY⟩ }
  return (n,rng)

/-- One dry NPC.UpdateNPC. AI20 uses the noTileCollide path: no collision call,
no gravity/max-fall clamp and no oldVelocity update. FindFrame leaves the sole
frame/rotation alone. TrailingMode70=-1, so no cache or localAI update occurs. -/
def update (world : OriginalGreenSlime.World) (players : Array OriginalGreenSlime.Player)
    (rng : Rng) (enemy : Enemy) : Except String Step := do
  if !enemy.body.active then return { enemy,rng }
  if enemy.body.life ≤ 0 then throw "Spike Ball has nonpositive life; forced death is not ordinary gameplay"
  if enemy.body.wet then throw "Spike Ball wet-state update"
  if enemy.body.ai0 != 0 && enemy.body.ai3 ≤ 0 then throw "Spike Ball invalid initialized speed multiplier"
  let mut n := enemy.body
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  n := { n with oldTarget := n.target,oldDirection := n.direction,oldDirectionY := n.directionY }
  let ease := 1 + n.velocity.x.abs / 3
  let gfx := if n.gfxOffY > 0 then max 0 (n.gfxOffY - ease * n.stepSpeed)
    else if n.gfxOffY < 0 then min 0 (n.gfxOffY + ease * n.stepSpeed) else n.gfxOffY
  n := { n with gfxOffY := max (-16) (min 16 gfx) }
  let (next,rng) := ai players rng n
  n := { next with immune := next.immune.map fun t => if t > 0 then t - 1 else t }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  n := { n with oldPosition := n.position,oldDirection := n.direction,position := n.position + n.velocity }
  if (OriginalPlayerWet.wetCollision world.grid n.position n.width n.height).wet then
    throw "Spike Ball entered liquid; water-perishable effects are not ported"
  let (checked,nearby) := OriginalGreenSlime.checkActive world players n
  return { enemy := { body := { checked with netUpdate := false,justHit := false } },rng,nearby }

structure Drawing where
  /-- DrawNPCDirect writes the chain angle into NPC.rotation. -/
  enemy : Enemy
  sprites : Array OriginalForestChatLayout.Sprite
  deriving Repr

private def layoutColor (c : OriginalGreenSlime.Color) : OriginalForestChatLayout.Color := ⟨c.r,c.g,c.b,c.a⟩

private def litColor (n : Slime) (c : OriginalGreenSlime.Color) : OriginalForestChatLayout.Color :=
  let factor : Float32 := single (255 - (n.alpha : Int)) / 255
  let channel (x : Nat) : Nat := (single x * factor).toInt32.toInt.toNat
  ⟨channel c.r,channel c.g,channel c.b,(max 0 (min 255 ((c.a : Int) - n.alpha))).toNat⟩

/-- Ordinary world DrawNPCDirect AI20 chain/base and NPC70 body. The lighting
callback receives world tile coordinates, sampled separately for each segment.
Signed/zero final source heights are retained exactly as FNA stores them.
Drawing is separate from the NPC update and consumes no random numbers. -/
partial def draw (enemy : Enemy) (screen : Vector)
    (lighting : Int → Int → OriginalGreenSlime.Color) : Drawing := Id.run do
  let n := enemy.body
  let mut current : Vector := ⟨n.position.x + single (n.width / 2),n.position.y + single (n.height / 2)⟩
  let mut dx := n.ai1 - current.x
  let mut dy := n.ai2 - current.y
  let angle := (Float.atan2 dy.toFloat dx.toFloat).toFloat32 - 1.5700000524520874
  let mut more := true
  let mut sprites := #[]
  while more do
    let mut sourceHeight : Int := 12
    let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
    if distance < 20 then
      sourceHeight := distance.toInt32.toInt - 20 + 12
      more := false
    let fraction : Float32 := 12 / distance
    dx := dx * fraction
    dy := dy * fraction
    current := ⟨current.x + dx,current.y + dy⟩
    dx := n.ai1 - current.x
    dy := n.ai2 - current.y
    let light := lighting (current.x.toInt32.toInt.tdiv 16) (current.y / 16).toInt32.toInt
    sprites := sprites.push (OriginalForestChatLayout.draw "Images/Chain" 12 12
      (current.x - screen.x) (current.y - screen.y) (some ⟨0,0,12,sourceHeight⟩)
      (layoutColor light) angle 6 6 1 1)
  let light := lighting (n.ai1.toInt32.toInt.tdiv 16) (n.ai2 / 16).toInt32.toInt
  sprites := sprites.push (OriginalForestChatLayout.draw "Images/Spike_Base" 16 16
    (n.ai1 - screen.x) (n.ai2 - screen.y) (some ⟨0,0,16,16⟩)
    (layoutColor light) (angle - 0.75) 8 8 1 1)
  let tx := (n.position.x.toFloat + n.width.toFloat * 0.5).toInt64.toInt.tdiv 16
  let ty := ((n.position.y.toFloat + n.height.toFloat * 0.5) / 16).toInt64.toInt
  let light := lighting tx ty
  let x := n.position.x - screen.x + single (n.width / 2) - 34 * n.scale / 2 + 17 * n.scale
  -- Main.NPCAddHeight(NPC70) returns -4 * scale.
  let y := n.position.y - screen.y + single n.height - 34 * n.scale + 17 * n.scale + 4 + (-4 * n.scale) + n.gfxOffY
  sprites := sprites.push (OriginalForestChatLayout.draw "Images/NPC_70" 34 34 x y (some ⟨0,n.frameY,34,34⟩)
    (litColor n light) angle 17 17 n.scale n.scale (if n.spriteDirection == 1 then 1 else 0))
  return { enemy := { body := { n with rotation := angle } },sprites }

end Lean4Terria.OriginalDungeonSpikeBall
