import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalForestChatLayout

/-!
Terraria 1.4.5.8 desktop ordinary ground dust DrawDust path, including active
state writeback. Full original method evidence and exact FNA Sprite records:
`.lake/original-ground-dust-lifecycle/`; formal-module replay and compiled
negative controls: `.lake/original-ground-dust-promotion/`.

Only types 0,1,2,7,8,9,10,11 are handled. Specialized dust types pass through
unchanged without sprites; callers retain their existing dispatch. The basic
types assume default fadeIn=0, fullBright=false, shader=null, customData=null.
-/
namespace Lean4Terria.OriginalBasicDustDraw

open OriginalForestGuideArrow (DustParticle Viewport supportsBasicGround)

structure DrawOutput where
  particles : Array DustParticle
  sprites : Array OriginalForestChatLayout.Sprite

private def truncF32 (x : Float32) : Int := x.toInt64.toInt

private def projectColor (rgba : UInt32) : OriginalForestChatLayout.Color :=
  ⟨(rgba >>> 24).toNat,((rgba >>> 16) &&& 255).toNat,
   ((rgba >>> 8) &&& 255).toNat,(rgba &&& 255).toNat⟩

/-- Draw the supported basic types in ascending physical slot order, returning
both sprites and survivors. Inputs are the DRAW camera, `Main.maxDustToDraw`
and independently computed `Lighting.GetColor(tileX,tileY)` in project
0xRRGGBBAA order. No RNG, motion or animation clock is advanced.

Dust.xnb is 1000x120. In the exact desktop CIL, V_1 is constructed twice before
the loop; the second rectangle overwrites the first, giving margins 1000/1050.
Outside it, the particle is removed without a sprite. Inside it, a black base
sprite is emitted BEFORE removal. Transparent black is not Color.Black.

Persist `particles` before the next allocation/update/repeated draw. When other
cohorts share the physical pool, callers must preserve their interleaved slot
order, for example by invoking this helper per supported particle. Unsupported
types and slots outside the draw budget are returned unchanged, without sprites. -/
def drawParticles (particles : Array DustParticle) (drawBudget : Nat) (view : Viewport)
    (lightAt : Int → Int → UInt32) : DrawOutput := Id.run do
  let screenX := truncF32 view.position.x - 1000
  let screenY := truncF32 view.position.y - 1050
  let screenW := (view.width : Int) + 2000
  let screenH := (view.height : Int) + 2100
  let mut live := #[]
  let mut sprites := #[]
  for particle in particles.qsort (fun p q => p.slot < q.slot) do
    if !supportsBasicGround particle.request.typeId || particle.slot ≥ drawBudget then
      live := live.push particle
    else
      let x := truncF32 particle.position.x
      let y := truncF32 particle.position.y
      if x < screenX + screenW && screenX < x + 4 &&
          y < screenY + screenH && screenY < y + 4 then
        let tx := ((particle.position.x.toFloat + 4).toInt64.toInt).tdiv 16
        let ty := ((particle.position.y.toFloat + 4).toInt64.toInt).tdiv 16
        let (base, overlay) := particle.drawColors (lightAt tx ty)
        let pose := fun rgba => OriginalForestChatLayout.draw "Images/Dust" 1000 120
          (particle.position.x - view.position.x) (particle.position.y - view.position.y)
          (some ⟨particle.frame.x,particle.frame.y,particle.frame.w,particle.frame.h⟩)
          (projectColor rgba) particle.rotation 4 4 particle.scale particle.scale
        sprites := sprites.push (pose base)
        if particle.request.color != 0 && overlay != 0 then sprites := sprites.push (pose overlay)
        if base != 0x000000ff then live := live.push particle
  return ⟨live,sprites⟩

end Lean4Terria.OriginalBasicDustDraw
