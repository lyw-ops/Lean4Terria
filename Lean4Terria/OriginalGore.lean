import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalForestChatLayout
import Lean4Terria.UnifiedRandom

/-! The ordinary 151/152/153 Gore path from desktop 1.4.5.8.
Native runtime metadata: SpecialAI=0, IsDrip=false, DisappearSpeed=1,
DisappearSpeedAlpha=1. ChildSafety.Disabled=true is the supported configuration.
The caller owns the Gore random stream, loaded-texture state and lighting. -/
namespace Lean4Terria.OriginalGore

open OriginalCollision (Grid Vector single tileIndex)
open OriginalForestChatLayout (Color Sprite)

def textureSize : Nat → Option (Nat × Nat)
  | 151 => some (22, 24)
  | 152 => some (10, 18)
  | 153 => some (10, 16)
  | _ => none

structure Particle where
  typeId : Nat := 0
  active : Bool := false
  position : Vector := ⟨0, 0⟩
  velocity : Vector := ⟨0, 0⟩
  rotation : Float32 := 0
  scale : Float32 := 1
  alpha : Int := 0
  timeLeft : Int := 600
  sticky : Bool := true
  behindTiles : Bool := false
  light : Float32 := 0
  frameCounter : Nat := 0
  deriving Repr, Inhabited

structure Request where
  typeId : Nat
  position : Vector
  velocity : Vector
  scale : Float32 := 1
  deriving Repr

private def inWorld (grid : Grid) (p : Vector) : Bool :=
  let x := tileIndex p.x
  let y := tileIndex p.y
  0 ≤ x && x < (grid.width : Int) && 0 ≤ y && y < (grid.height : Int)

/-- The reset fields of NewGore, for a velocity whose two native random
adjustments were already applied. No random draw is repeated here. -/
def fromRolled (grid : Grid) (request : Request) : Except String Particle := do
  if (textureSize request.typeId).isNone then throw s!"unsupported Gore {request.typeId}"
  return {
    typeId := request.typeId, position := request.position, velocity := request.velocity
    scale := request.scale, active := inWorld grid request.position }

/-- Gore.Update for these ordinary types. `loaded=false` uses the original
32-pixel collision fallback; DrawGore loads the texture before drawing it. -/
def update (grid : Grid) (p : Particle) (loaded : Bool := true)
    (multiplayerServer : Bool := false) : Except String Particle := do
  if multiplayerServer || !p.active then return p
  let some (width, height) := textureSize p.typeId | throw s!"unsupported Gore {p.typeId}"
  if p.sticky && !inWorld grid p.position then return { p with active := false }
  let mut v := p.velocity
  if p.sticky then
    let length := (v.x * v.x + v.y * v.y).toFloat.sqrt.toFloat32
    if length > 32 then
      let factor := 32 / length
      v := ⟨v.x * factor, v.y * factor⟩
  v := { v with y := v.y + 0.2 }
  let rotation := p.rotation + v.x * 0.05
  let mut alpha := p.alpha
  let mut timeLeft := p.timeLeft
  if p.sticky then
    -- IL_1ec8: Height >= Width keeps Width, otherwise the size becomes Height.
    let baseSize := if loaded then min width height else 32
    let inset := (single baseSize * 0.9).toInt32.toInt
    let size := (single inset * p.scale).toInt32.toInt.toNat
    v := (OriginalCollision.tileCollision grid p.position v size size { hoik := true }).displacement
    if v.y == 0 then
      v := { v with x := v.x * 0.97 }
      if v.x.toFloat > -0.01 && v.x.toFloat < 0.01 then v := { v with x := 0 }
    if timeLeft > 0 then timeLeft := timeLeft - 1
    else alpha := alpha + 1
  else alpha := alpha + 2
  return { p with
    velocity := v, position := p.position + v, rotation, alpha, timeLeft
    active := alpha < 255 }

def getAlpha (p : Particle) (light : Color) : Color :=
  let factor := single (255 - p.alpha) / 255
  Color.mk' ((single light.r * factor).toInt32.toInt)
    ((single light.g * factor).toInt32.toInt) ((single light.b * factor).toInt32.toInt)
    (light.a - p.alpha)

/-- The single-frame DrawGore branch. Lighting is sampled at the unscaled
texture centre by the caller; the sprite centre and origin use integer halves. -/
def sprite (p : Particle) (camera : Vector) (light : Color) : Except String (Option Sprite) := do
  if !p.active then return none
  let some (w, h) := textureSize p.typeId | throw s!"unsupported Gore {p.typeId}"
  let x := p.position.x - camera.x + single (w / 2)
  let y := p.position.y - camera.y + single (h / 2)
  return some (OriginalForestChatLayout.draw s!"Images/Gore_{p.typeId}" w h x y
    (some ⟨0, 0, w, h⟩) (getAlpha p light) p.rotation (single (w / 2)) (single (h / 2)) p.scale p.scale)

structure Pool where
  particles : Array Particle := Array.replicate 600 {}
  deriving Repr

def Pool.freeSlots (pool : Pool) : Nat := pool.particles.countP (!·.active)

/-- The world Gore phase visits all 600 slots, including particles born earlier
in this update. Loaded texture types belong to the client draw state. -/
def Pool.advance (pool : Pool) (grid : Grid) (loadedTypes : Array Nat) : Except String Pool := do
  let particles ← pool.particles.mapM fun p => update grid p (loadedTypes.contains p.typeId)
  return { particles }

def Pool.loadedAfterDraw (pool : Pool) (loaded : Array Nat) : Array Nat :=
  pool.particles.foldl (fun loaded p =>
    if p.active && !loaded.contains p.typeId then loaded.push p.typeId else loaded) loaded

/-- The caller of the rolled request must have reserved this slot before
its two draws. A full pool returns the native sentinel 600 without an edit. -/
def Pool.emitRolled (pool : Pool) (grid : Grid) (request : Request) : Except String (Pool × Nat) := do
  let some slot := pool.particles.findIdx? (!·.active) | return (pool, 600)
  let particle ← fromRolled grid request
  return ({ pool with particles := pool.particles.set! slot particle }, if particle.active then slot else 600)

/-- NewGore for raw velocities: allocate first, then draw Y and X. The live
NPC death path uses this directly; event-only callers may use emitRolled. -/
def Pool.spawn (pool : Pool) (grid : Grid) (request : Request) (rng : UnifiedRandom)
    (blocked : Bool := false) : Except String (Pool × Nat × UnifiedRandom) := do
  if blocked || pool.freeSlots == 0 then return (pool, 600, rng)
  if (textureSize request.typeId).isNone then throw s!"unsupported Gore {request.typeId}"
  let (up, rng) := rng.next 10 31
  let (side, rng) := rng.next (-20) 21
  let velocity : Vector := ⟨request.velocity.x + single side * 0.1, request.velocity.y - single up * 0.1⟩
  let (pool, slot) ← pool.emitRolled grid { request with velocity }
  return (pool, slot, rng)

end Lean4Terria.OriginalGore
