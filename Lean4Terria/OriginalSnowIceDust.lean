import Lean4Terria.OriginalBasicDustDraw

/-! Dust 51 (Snow Block) and dust 80 (Ice Block) of desktop 1.4.5.8: `Dust.NewDust`,
`Dust.UpdateDust`, `Dust.GetAlpha` / `GetColor` and `Main.DrawDust`. Neither type has a branch
of its own in `UpdateDust`, `GetAlpha` or `DrawDust`; they emit no light and draw no random
number after birth. This module is the dispatch for exactly these two types, with the same
shapes as `OriginalForestGuideArrow.advanceParticles` and `OriginalBasicDustDraw.drawParticles`,
which leave them untouched. Evidence and limits: `.lake/original-snow-ice-effects/REPORT.md`. -/
namespace Lean4Terria.OriginalSnowIceDust

open OriginalForestGuideArrow (DustPool DustParticle DustRequest Viewport)
open OriginalBasicDustDraw (DrawOutput)

def supports (typeId : Nat) : Bool := typeId == 51 || typeId == 80

/-- `Dust.NewDust` for the two types: the ordinary creation (both sit in the first atlas bank);
dust 80 is then given alpha 50 whatever the request says (IL_0776..0785). -/
def newDust (pool : DustPool) (request : DustRequest) (random : UnifiedRandom) :
    DustPool × Option DustParticle × UnifiedRandom :=
  let (pool, created, random) := pool.create request random
  let created := created.map fun dust =>
    if request.typeId == 80 then { dust with request := { dust.request with alpha := 50 } } else dust
  (pool, created, random)

/-- One `Dust.UpdateDust` for the two types: the ordinary movement, and removal of every slot at
or above `Main.maxDustToDraw`. Other types pass through unchanged. -/
def advanceParticles (particles : Array DustParticle) (drawBudget : Nat) (view : Viewport) : Array DustParticle :=
  particles.filterMap fun particle =>
    if !supports particle.request.typeId then some particle
    else if particle.slot < drawBudget then particle.advance view else none

private def truncF32 (x : Float32) : Int := x.toInt64.toInt

private def projectColor (rgba : UInt32) : OriginalForestChatLayout.Color :=
  ⟨(rgba >>> 24).toNat, ((rgba >>> 16) &&& 255).toNat, ((rgba >>> 8) &&& 255).toNat, (rgba &&& 255).toNat⟩

/-- One `Main.DrawDust` for the two types, in ascending slot order, with the draw camera,
`Main.maxDustToDraw` and `Lighting.GetColor(tileX, tileY)` as 0xRRGGBBAA. A particle outside the
camera rectangle grown by 1000 / 1050 is removed without a sprite; inside it the lit sprite is
drawn, then the own-colour overlay when the particle has a colour, and the particle is removed
when its lit colour is opaque black. Other types and slots at or above the budget are returned
unchanged without sprites. Persist `particles` before the next allocation, update or draw. -/
def drawParticles (particles : Array DustParticle) (drawBudget : Nat) (view : Viewport)
    (lightAt : Int → Int → UInt32) : DrawOutput := Id.run do
  let screenX := truncF32 view.position.x - 1000
  let screenY := truncF32 view.position.y - 1050
  let screenW := (view.width : Int) + 2000
  let screenH := (view.height : Int) + 2100
  let mut live := #[]
  let mut sprites := #[]
  for particle in particles.qsort (fun p q => p.slot < q.slot) do
    if !supports particle.request.typeId || particle.slot ≥ drawBudget then
      live := live.push particle
    else
      let x := truncF32 particle.position.x
      let y := truncF32 particle.position.y
      if x < screenX + screenW && screenX < x + 4 && y < screenY + screenH && screenY < y + 4 then
        let tx := ((particle.position.x.toFloat + 4).toInt64.toInt).tdiv 16
        let ty := ((particle.position.y.toFloat + 4).toInt64.toInt).tdiv 16
        let (base, overlay) := particle.drawColors (lightAt tx ty)
        let pose := fun rgba => OriginalForestChatLayout.draw "Images/Dust" 1000 120
          (particle.position.x - view.position.x) (particle.position.y - view.position.y)
          (some ⟨particle.frame.x, particle.frame.y, particle.frame.w, particle.frame.h⟩)
          (projectColor rgba) particle.rotation 4 4 particle.scale particle.scale
        sprites := sprites.push (pose base)
        if particle.request.color != 0 && overlay != 0 then sprites := sprites.push (pose overlay)
        if base != 0x000000ff then live := live.push particle
  return ⟨live, sprites⟩

end Lean4Terria.OriginalSnowIceDust
