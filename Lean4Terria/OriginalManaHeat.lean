import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalForestChatLayout

/-! The persistent mana heat fields and the early Player.Update visual phase
from desktop Terraria 1.4.5.8. Heat is read before ResetEffects, so a pickup
does not change that early phase until the next UpdateBuffs has run. -/
namespace Lean4Terria.OriginalManaHeat

open OriginalCollision (Vector single)
open OriginalForestGuideArrow (DustPool DustParticle DustRequest DustFrame)
open OriginalForestChatLayout (Color)

structure State where
  heat : Float32 := 0
  latestBonus : Float32 := 0
  latestDuration : Float32 := 0
  deriving Repr

structure Applied where
  state : State
  /-- Request passed to AddBuff(396, ticks, false), not the resulting timer. -/
  buffTime : Option Int
  deriving Repr

/-- TryApplyManaHeat writes both latest fields even when it does not add a
buff. The caller applies the returned request with normal AddBuff semantics. -/
def State.apply (state : State) (bonus seconds : Float32) : Applied :=
  let duration := seconds * 60
  { state := { state with latestBonus := bonus, latestDuration := duration }
    buffTime := if seconds != 0 && bonus != 0 then some duration.toInt32.toInt else none }

/-- ResetEffects followed by the ordinary 396 UpdateBuffs branch. The timer is
decremented before the fraction is computed; pass its positive entry value.
An expired or absent buff is represented by zero. -/
def State.updateEffects (state : State) (buffTimeBefore : Int) : State :=
  if buffTimeBefore > 0 then
    let duration := if state.latestDuration == 0 then 1 else state.latestDuration
    let remaining := Float32.ofInt (buffTimeBefore - 1)
    { state with heat := (1 : Float32) + state.latestBonus * (remaining / duration)
                 latestDuration := duration }
  else { state with heat := 1 }

/-- The local-player Utils.Remap(heat,1.3,1,1,0,true), retaining its actual
GetLerpValue then MathHelper.Lerp operation order in Single precision. -/
def chance (heat : Float32) : Float32 :=
  let t : Float32 := if heat < 1 then 1 else if heat > 1.3 then 0 else (heat - 1.3) / (1 - 1.3)
  1 + ((0 : Float32) - 1) * t

/-- The early visual phase when NewDust returns its sentinel (game menu,
paused, generating, server, viewport or full pool). CloneDust then returns
that same sentinel. Each successful attempt still draws scale/fade/scale.
This method represents this explicit no-allocation path only. -/
def State.visualsWithoutAllocation (state : State) (rng : UnifiedRandom)
    (localPlayer : Bool := true) : UnifiedRandom := Id.run do
  if state.heat == 1 then return rng
  let threshold : Float32 := if localPlayer then chance state.heat else 0.3
  let mut rng := rng
  for _ in [:2] do
    let (sample, next) := rng.sample
    rng := next
    if sample.toFloat32 * 6 < threshold then
      let (_, next) := rng.next 10 26
      let (_, next) := next.sample
      let (_, next) := next.sample
      rng := next
  return rng

structure Particle where
  slot : Nat
  position : Vector
  velocity : Vector
  frame : DustFrame
  scale : Float32
  fadeIn : Float32
  rotation : Float32 := 0
  alpha : Int := 200
  color : Color := ⟨0,0,255,255⟩
  active : Bool := true
  noGravity : Bool := true
  noLight : Bool := true
  firstFrame : Bool := true
  customPlayer : Bool := true
  noLightEmittance : Bool := false
  deriving Repr

/-- Reserve these slots along with all other active particles in the caller's
shared DustPool. This conversion is for allocation bookkeeping, not UpdateDust. -/
def Particle.allocation (p : Particle) : DustParticle := {
  slot := p.slot,position := p.position,velocity := p.velocity,frame := p.frame,
  scale := p.scale,rotation := p.rotation,noGravity := p.noGravity
  request := {position := p.position,width := 0,height := 0,typeId := 267} }

structure Visuals where
  pool : DustPool
  random : UnifiedRandom
  particles : Array Particle := #[]

/-- The actual NewDustDirect/CloneDust creation path. The caller supplies the
shared occupied slots and viewport; the clone consumes NewDust's own draws,
then copies the original frame/fields over its newly created particle. -/
def State.visuals (state : State) (rng : UnifiedRandom) (pool : DustPool)
    (position : Vector) (width height : Nat) (localPlayer : Bool := true) : Visuals := Id.run do
  let mut result : Visuals := ⟨pool,rng,#[]⟩
  if state.heat == 1 then return result
  let threshold : Float32 := if localPlayer then chance state.heat else 0.3
  for _ in [:2] do
    let (sample,rng) := result.random.sample
    result := {result with random := rng}
    if sample.toFloat32 * 6 ≥ threshold then continue
    let (scaleRoll,rng) := result.random.next 10 26
    let request : DustRequest := {
      position,width,height,typeId := 267,alpha := 255,scale := single scaleRoll * 0.1 }
    let (pool,base,rng) := result.pool.create request rng
    let (fade,rng) := rng.sample
    let (scale,rng) := rng.sample
    result := {result with pool,random := rng}
    let some base := base | continue
    let particle : Particle := {
      slot := base.slot,position := {base.position with y := base.position.y+5}
      velocity := ⟨0,(base.velocity.y*0.5-0.5)-1⟩
      frame := {base.frame with x := base.frame.x-2000,y := base.frame.y+60}
      fadeIn := 0.7+1.3*fade.toFloat32,scale := 0.7+1.3*scale.toFloat32 }
    result := {result with particles := result.particles.push particle}
    let request : DustRequest := {position := particle.position,width := 0,height := 0,typeId := 267}
    let (pool,clone,rng) := result.pool.create request result.random
    result := {result with pool,random := rng}
    if let some clone := clone then
      let child : Particle := { particle with
        slot := clone.slot,scale := particle.scale*0.4
        fadeIn := particle.fadeIn*0.4,color := ⟨255,255,255,255⟩ }
      result := {result with particles := result.particles.push child}
  return result

structure Advanced where
  particle : Particle
  /-- Lighting.AddLight at this particle's unscaled position, before the common
  fade branch. Propagating this light into the scene remains the caller's task. -/
  emittedLight : Vector × (Float32 × Float32 × Float32)
  deriving Repr

/-- The 267 branch plus its common UpdateDust tail. It makes no random draws.
ChildSafety.Disabled=true and the caller's shared density are explicit inputs.
The Player reference stored in customData contributes this frame's displacement. -/
def Particle.advance (p : Particle) (playerDelta : Vector)
    (view : OriginalForestGuideArrow.Viewport) (density : Float32 := 0) : Advanced := Id.run do
  let mut p := p
  if !p.active then return ⟨p,(p.position,(0,0,0))⟩
  let initiallyTooLarge := p.scale > 10
  p := {p with
    position := p.position+p.velocity
    rotation := if p.velocity.x < 0 then p.rotation-1 else p.rotation+1}
  if p.customPlayer then p := {p with position := p.position+playerDelta}
  p := {p with velocity := ⟨p.velocity.x*0.98,p.velocity.y*0.98⟩,scale := p.scale+0.02,noLight := false}
  let intensity := min 1 (p.scale*0.8)
  let light := (p.position,(intensity*(single p.color.r/255),intensity*(single p.color.g/255),intensity*(single p.color.b/255)))
  p := {p with velocity := {p.velocity with x := p.velocity.x*0.99}}
  p := {p with rotation := p.rotation+p.velocity.x*0.5}
  if p.fadeIn > 0 && p.fadeIn < 100 then
    p := {p with scale := p.scale+0.03}
    if p.scale > p.fadeIn then p := {p with fadeIn := 0}
  else p := {p with scale := p.scale-0.01}
  if p.noGravity then
    p := {p with velocity := ⟨p.velocity.x*0.92,p.velocity.y*0.92⟩}
    if p.fadeIn == 0 then p := {p with scale := p.scale-0.04}
  let (shrink,minimum) : Float32 × Float32 :=
    if density.toFloat == 0.5 then (0.001,0.11)
    else if density.toFloat == 0.6 then (0.0025,0.13)
    else if density.toFloat == 0.7 then (0.005,0.16)
    else if density.toFloat == 0.8 then (0.01,0.22)
    else if density.toFloat == 0.9 then (0.02,0.25)
    else (0,0.1)
  p := {p with scale := p.scale-shrink}
  p := {p with active := !initiallyTooLarge && p.scale ≥ minimum && p.position.y ≤ view.position.y+single view.height}
  return ⟨p,light⟩

/-- GetAlpha's explicit 267 branch ignores sampled lighting and returns its
own RGB with alpha zero (additive emission in the ordinary sprite batch). -/
def Particle.getAlpha (p : Particle) : Color := {p.color with a := 0}

structure Drawn where
  particle : Particle
  sprites : Array OriginalForestChatLayout.Sprite := #[]

/-- Ordinary DrawDust, with its 4x4 visibility test and active=false writeback.
The actual source is the 8x8 atlas frame and origin (4,4). There is no added
four-pixel position offset. 267 emits its own color twice when nonzero. -/
def Particle.draw (p : Particle) (view : OriginalForestGuideArrow.Viewport) : Drawn := Id.run do
  if !p.active then return ⟨p,#[]⟩
  let x := p.position.x.toInt32.toInt
  let y := p.position.y.toInt32.toInt
  let left := view.position.x.toInt32.toInt-1000
  let top := view.position.y.toInt32.toInt-1050
  if x+4 ≤ left || x ≥ left+(view.width:Int)+2000 || y+4 ≤ top || y ≥ top+(view.height:Int)+2100 then
    return ⟨{p with active := false},#[]⟩
  let color := p.getAlpha
  let sprite := OriginalForestChatLayout.draw "Images/Dust" 1000 120
    (p.position.x-view.position.x) (p.position.y-view.position.y)
    (some ⟨p.frame.x,p.frame.y,p.frame.w,p.frame.h⟩) color p.rotation 4 4 p.scale p.scale
  -- GetColor(newColor) adds the own channels minus 255 and clamps. The source
  -- particles are blue or white; alpha is zero in both resulting passes.
  let overlay := Color.mk' ((p.color.r:Int)+(color.r:Int)-255)
    ((p.color.g:Int)+(color.g:Int)-255) ((p.color.b:Int)+(color.b:Int)-255)
    ((p.color.a:Int)+(color.a:Int)-255)
  let sprites := if p.color.packed != 0 && overlay.packed != 0 then
      #[sprite,{sprite with color := overlay.packed}]
    else #[sprite]
  return ⟨p,sprites⟩

end Lean4Terria.OriginalManaHeat
