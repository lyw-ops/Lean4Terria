import Lean4Terria.OriginalCollision
import Lean4Terria.UnifiedRandom

namespace Lean4Terria.OriginalForestGuideArrow

open OriginalCollision (Vector single)

private def truncF32 (x : Float32) : Int := x.toInt64.toInt

structure Viewport where
  position : Vector
  width : Nat
  height : Nat

structure DustPool where
  viewport : Option Viewport := none
  /-- Physical allocation slots; the desktop pool has 6000. -/
  capacity : Nat := 6000
  /-- `Main.maxDustToDraw`, independent of physical allocation capacity. -/
  drawBudget : Nat := 6000
  occupied : Array Bool := #[]
  density : Float32 := 0
  enabled : Bool := true

structure DustRequest where
  position : Vector
  width : Nat
  height : Nat
  typeId : Nat
  speed : Vector := ⟨0, 0⟩
  alpha : Int := 0
  color : UInt32 := 0
  scale : Float32 := 1
  deriving Repr

structure DustFrame where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving Repr

structure DustParticle where
  slot : Nat
  request : DustRequest
  position : Vector
  velocity : Vector
  frame : DustFrame
  scale : Float32
  rotation : Float32 := 0
  noGravity : Bool := false
  deriving Repr

def DustPool.occupy (pool : DustPool) (dust : Array DustParticle) : DustPool :=
  let occupied := dust.foldl (init := pool.occupied) fun slots particle =>
    let slots := slots ++ Array.replicate (particle.slot + 1 - slots.size) false
    slots.set! particle.slot true
  { pool with occupied }

/-- The ordinary `UpdateDust` tail, after creation. Existing callers also use
this for types 4, 5 and 26; the basic ground lifecycle audit additionally covers
0, 1, 2, 7, 8, 9, 10 and 11. No RNG is drawn. -/
def DustParticle.advance (dust : DustParticle) (view : Viewport) : Option DustParticle := do
  let position := dust.position + dust.velocity
  let velocity : Vector :=
    ⟨dust.velocity.x * 0.99, dust.velocity.y + if dust.noGravity then 0 else 0.1⟩
  let rotation := dust.rotation + velocity.x * 0.5
  let scale := dust.scale - 0.01
  let velocity := if dust.noGravity then ⟨velocity.x * 0.92, velocity.y * 0.92⟩ else velocity
  let scale := if dust.noGravity then scale - 0.04 else scale
  guard (scale ≥ 0.1 && position.y ≤ view.position.y + single view.height)
  return { dust with position, velocity, rotation, scale }

/-- Terraria draws the lit particle, then overlays the dust's own color if set. -/
def DustParticle.drawColors (dust : DustParticle) (light : UInt32) : UInt32 × UInt32 :=
  let channel := fun (color shift : UInt32) => (color >>> shift) &&& 255
  let opacity := single (255 - dust.request.alpha) / 255
  let clamp := fun (v : Int) => (max 0 (min 255 v)).toNat.toUInt32
  let rgb := fun shift => clamp (((channel light shift).toFloat32 * opacity).toInt64.toInt) <<< shift
  let alpha := clamp ((channel light 0).toNat - dust.request.alpha)
  let base := rgb 24 ||| rgb 16 ||| rgb 8 ||| alpha
  let overlay := fun shift =>
    ((channel dust.request.color shift).toNat + (channel base shift).toNat - 255).toUInt32 <<< shift
  (base, overlay 24 ||| overlay 16 ||| overlay 8 ||| overlay 0)

private def DustPool.visible (pool : DustPool) (position : Vector) : Bool :=
  match pool.viewport with
  | none => true
  | some view =>
    let margin := truncF32 (400 * (1 - pool.density))
    let screen : DustFrame := ⟨truncF32 (view.position.x - single margin),
      truncF32 (view.position.y - single margin), (view.width : Int) + 2 * margin,
      (view.height : Int) + 2 * margin⟩
    let x := truncF32 position.x
    let y := truncF32 position.y
    x < screen.x + screen.w && screen.x < x + 10 &&
      y < screen.y + screen.h && screen.y < y + 10

/-- Ordinary dust creation, including viewport and
dust-pool pressure before drawing positions, velocity, frame and scale. -/
def DustPool.create (pool : DustPool) (request : DustRequest) (random : UnifiedRandom) :
    DustPool × Option DustParticle × UnifiedRandom := Id.run do
  if !pool.enabled || !pool.visible request.position then return (pool, none, random)
  let slot := (pool.occupied.findIdx? (!·)).getD pool.occupied.size
  if slot ≥ pool.capacity then return (pool, none, random)
  let (bound, keepZero) :=
    if slot.toFloat > pool.drawBudget.toFloat * 0.9 then (4, true)
    else if slot.toFloat > pool.drawBudget.toFloat * 0.8 then (3, true)
    else if slot.toFloat > pool.drawBudget.toFloat * 0.7 then (2, false)
    else if slot.toFloat > pool.drawBudget.toFloat * 0.6 then (4, false)
    else if slot.toFloat > pool.drawBudget.toFloat * 0.5 then (5, false)
    else (0, false)
  let mut stream := random
  if bound != 0 then
    let (roll, next) := stream.next 0 bound
    stream := next
    if (roll == 0) != keepZero then return (pool, none, stream)
  let (x, random) := stream.next 0 (max 5 request.width - 4 : Nat)
  let (y, random) := random.next 0 (max 5 request.height - 4 : Nat)
  let (vx, random) := random.next (-20) 21
  let (vy, random) := random.next (-20) 21
  let (row, random) := random.next 0 3
  let (scaleRoll, random) := random.next (-20) 21
  let particle : DustParticle := {
    slot, request
    position := ⟨request.position.x + single x + 4, request.position.y + single y + 4⟩
    velocity := ⟨single vx * 0.1 + request.speed.x, single vy * 0.1 + request.speed.y⟩
    frame := ⟨10 * request.typeId, 10 * row, 8, 8⟩
    scale := (1 + single scaleRoll * 0.01) * request.scale
  }
  let occupied :=
    if slot < pool.occupied.size then pool.occupied.set! slot true else pool.occupied.push true
  let density := if bound == 0 then 0 else pool.density
  return ({ pool with occupied, density }, some particle, random)

/-- Ordinary basic ground dust covered by the desktop lifecycle/DrawDust audit.
Default fadeIn=0, fullBright=false, shader=null and customData=null are assumed. -/
def supportsBasicGround (typeId : Nat) : Bool :=
  #[0,1,2,7,8,9,10,11].contains typeId

/-- Update the eight supported ordinary ground types once and clear their slots
at/above `Main.maxDustToDraw`. Other types pass through unchanged so their
existing specialized update dispatch remains the caller's responsibility. -/
def advanceParticles (particles : Array DustParticle) (drawBudget : Nat) (view : Viewport) : Array DustParticle :=
  particles.filterMap fun particle =>
    if !supportsBasicGround particle.request.typeId then some particle
    else if particle.slot < drawBudget then particle.advance view else none

/-- Rebuild occupancy from ALL live particles in the shared physical pool.
The caller supplies density: this desktop UpdateDust tail resets it to zero;
DrawDust leaves it unchanged. Do not pass only one particle cohort here. -/
def DustPool.withLiveParticles (pool : DustPool) (particles : Array DustParticle)
    (density : Float32) : DustPool :=
  ({pool with occupied := Array.replicate pool.capacity false, density}).occupy particles

end Lean4Terria.OriginalForestGuideArrow
