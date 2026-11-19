import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalForestChatLayout

/-! Terraria 1.4.5.8 UpdateManaRegen's local full-mana effect block.
This module owns the five scale samples and real type-45 pool allocations.
The caller runs numerical regeneration without an additional skip-five pass.
-/
namespace Lean4Terria.OriginalManaRefillEffects

open OriginalCollision (Vector single)
open OriginalForestGuideArrow (DustPool DustParticle Viewport)

structure Input where
  /-- A mana point was actually restored in the current regeneration loop. -/
  restored : Bool
  mana : Int
  maximum : Int
  localPlayer : Bool := true
  manaV2 : Bool := true
  /-- Entry values at UpdateManaRegen, before ItemCheck decrements the action. -/
  itemAnimation : Int := 0
  reuseDelay : Int := 0
  position : Vector
  width : Nat := 20
  height : Nat := 42
  deriving Repr

/-- The particles emitted here always have noLight=true, noGravity=true,
fadeIn=0, no customData and no shader. `dust` retains the shared allocation. -/
structure Particle where
  dust : DustParticle
  active : Bool := true
  deriving Repr

def Particle.allocation (particle : Particle) : DustParticle := particle.dust

structure Emitted where
  random : UnifiedRandom
  pool : DustPool
  particles : Array Particle := #[]
  /-- A SoundEngine.PlaySound(25) request; no audio backend is invoked here. -/
  sound : Option Nat := none

def eligible (input : Input) : Bool :=
  input.localPlayer && input.restored && input.mana >= input.maximum &&
    !(input.manaV2 && (input.itemAnimation > 0 || input.reuseDelay > 0))

/-- UpdateManaRegen IL_040b..04c8. The scale is sampled before every NewDust
attempt, including when the pool is full, disabled or outside the viewport. -/
def emit (input : Input) (pool : DustPool) (random : UnifiedRandom) : Emitted := Id.run do
  if !eligible input then return { random, pool }
  let mut random := random
  let mut pool := pool
  let mut particles := #[]
  for _ in [:5] do
    let (scaleRoll, next) := random.next 20 26
    let request : OriginalForestGuideArrow.DustRequest := {
      position := input.position, width := input.width, height := input.height
      typeId := 45, alpha := 255, scale := single scaleRoll * 0.1 }
    let (nextPool, created, next) := pool.create request next
    random := next
    pool := nextPool
    if let some dust := created then
      let dust := { dust with
        noGravity := true
        velocity := ⟨dust.velocity.x * 0.5, dust.velocity.y * 0.5⟩ }
      particles := particles.push { dust }
  return { random, pool, particles, sound := some 25 }

structure Light where
  x : Int
  y : Int
  r : Float32
  g : Float32
  b : Float32
  deriving Repr

structure Advanced where
  particle : Particle
  light : Option Light := none
  deriving Repr

/-- The actual refill-born type45 path, with noLight/noGravity and fadeIn=0.
The local player's current velocity is added independently of dust velocity;
the multiplier is one half while that player is wet. -/
def Particle.advance (particle : Particle) (playerVelocity : Vector) (wet : Bool)
    (view : Viewport) : Advanced := Id.run do
  if !particle.active then return { particle }
  let dust := particle.dust
  let position := dust.position + dust.velocity
  let velocity : Vector := ⟨dust.velocity.x * 0.94, dust.velocity.y * 0.94⟩
  let scale := dust.scale + 0.002
  let amount := min 1 (scale * 0.1)
  let scale := scale - 0.06
  let scale := if scale < 1 then scale - 0.06 else scale
  let factor : Float32 := if wet then 0.5 else 1
  let position := position + ⟨playerVelocity.x * factor, playerVelocity.y * factor⟩
  let light : Light := {
    x := (position.x / 16).toInt32.toInt, y := (position.y / 16).toInt32.toInt
    r := amount * 0.6, g := amount * 0.2, b := amount }
  let velocity : Vector := ⟨velocity.x * 0.99, velocity.y⟩
  let rotation := dust.rotation + velocity.x * 0.5
  let scale := scale - 0.01
  let velocity : Vector := ⟨velocity.x * 0.92, velocity.y * 0.92⟩
  let scale := scale - 0.04
  let active := dust.scale <= 10 && scale >= 0.1 && position.y <= view.position.y + single view.height
  return { particle := { dust := { dust with position, velocity, rotation, scale }, active }, light := some light }

/-- GetAlpha45 adjusts opacity from (255-alpha)/255 to (opacity+3)/4.
Refill-born particles keep alpha255, so RGB is three quarters of tile light
and the output alpha is zero; rendering requires premultiplied blending. -/
def Particle.getAlpha (particle : Particle) (light : UInt32) : UInt32 :=
  let alpha := particle.dust.request.alpha
  let opacity : Float32 := ((single (255 - alpha) / 255) + 3) / 4
  let channel (shift : UInt32) : UInt32 :=
    ((single ((light >>> shift) &&& 255).toNat * opacity).toInt32.toInt.toNat.toUInt32 &&& 255) <<< shift
  let outAlpha := max 0 (min 255 (((light &&& 255).toNat : Int) - alpha))
  channel 24 ||| channel 16 ||| channel 8 ||| outAlpha.toNat.toUInt32

structure Drawing where
  particle : Particle
  sprites : Array OriginalForestChatLayout.Sprite := #[]
  deriving Repr

/-- Main.DrawDust's ordinary unshaded single-layer type45 path. Visibility
also changes active: callers must save Drawing.particle and release its slot. -/
def Particle.draw (particle : Particle) (view : Viewport) (light : UInt32) : Drawing :=
  if !particle.active then { particle } else
  let dust := particle.dust
  let x := dust.position.x.toInt32.toInt
  let y := dust.position.y.toInt32.toInt
  let left := view.position.x.toInt32.toInt - 1000
  let top := view.position.y.toInt32.toInt - 1050
  if !(x < left + view.width + 2000 && left < x + 4 &&
      y < top + view.height + 2100 && top < y + 4) then
    { particle := { particle with active := false } }
  else
    let color := particle.getAlpha light
    let rgba : OriginalForestChatLayout.Color :=
      ⟨(color >>> 24).toNat, ((color >>> 16) &&& 255).toNat, ((color >>> 8) &&& 255).toNat, (color &&& 255).toNat⟩
    { particle, sprites := #[OriginalForestChatLayout.draw "Images/Dust" 1000 120
        (dust.position.x - view.position.x) (dust.position.y - view.position.y)
        (some ⟨dust.frame.x,dust.frame.y,dust.frame.w,dust.frame.h⟩) rgba dust.rotation 4 4 dust.scale dust.scale] }

end Lean4Terria.OriginalManaRefillEffects
