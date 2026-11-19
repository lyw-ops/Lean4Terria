import Lean4Terria.OriginalBasicDustDraw

/-! Desktop 1.4.5.8 ordinary Player.Hurt blood5 and stoned1 dust.
These are existing physical particles, not a second NewDust birth. The ordinary
branch assumes fadeIn0, fullBright=false, shader/customData=null, normal adult
gore settings (ChildSafety.Disabled=true), and the caller's client UpdateDust
gate. Native lifecycle/color/draw evidence: .lake/original-player-hurt-effects/.
-/
namespace Lean4Terria.OriginalBloodDust

open OriginalForestGuideArrow (DustParticle Viewport)

def supports (typeId : Nat) : Bool := typeId == 1 || typeId == 5

/-- Blood5's noGravity branch subtracts .04 BEFORE the common .01 and
noGravity .04 decrements. Preserve those separate Single operations. Normal
blood and stoned dust use the ordinary common tail. No RNG is consumed. -/
def advance (particle : DustParticle) (view : Viewport) : Option DustParticle := do
  guard (particle.scale ≤ 10)
  let particle := if particle.request.typeId == 5 && particle.noGravity then
      {particle with scale := particle.scale - 0.04}
    else particle
  particle.advance view

/-- The supported cohort's contribution to one UpdateDust physical-slot scan.
Other types remain untouched for the caller's mixed-type dispatcher. The
global netMode2 gate belongs to that dispatcher, before any slot is touched. -/
def advanceParticles (particles : Array DustParticle) (drawBudget : Nat) (view : Viewport) :
    Array DustParticle :=
  particles.filterMap fun p =>
    if !supports p.request.typeId then some p
    else if p.slot < drawBudget then advance p view else none

/-- These two IDs take the common GetAlpha/GetColor branches. RGBA is project
0xRRGGBBAA, including clamping rather than wrapping negative alpha channels. -/
def drawColors (particle : DustParticle) (light : UInt32) : UInt32 × UInt32 :=
  particle.drawColors light

/-- Full ordinary Main.DrawDust contribution and active writeback, using the
shared DrawOutput structure. Walk mixed cohorts in physical slot order; pass
each supported particle through here at its actual draw slot. Opaque-black
removal happens after its final sprite. Out-of-budget slots remain unchanged
during draw; UpdateDust owns their removal. No RNG or motion is advanced. -/
def drawParticles (particles : Array DustParticle) (drawBudget : Nat) (view : Viewport)
    (lightAt : Int → Int → UInt32) : OriginalBasicDustDraw.DrawOutput := Id.run do
  let pixel := fun (x : Float32) => x.toInt64.toInt
  let x0 := pixel view.position.x - 1000
  let y0 := pixel view.position.y - 1050
  let w : Int := view.width + 2000
  let h : Int := view.height + 2100
  let mut live := #[]
  let mut sprites := #[]
  for p in particles.qsort (fun p q => p.slot < q.slot) do
    if !supports p.request.typeId || p.slot ≥ drawBudget then
      live := live.push p
    else if pixel p.position.x < x0+w && x0 < pixel p.position.x+4 &&
        pixel p.position.y < y0+h && y0 < pixel p.position.y+4 then
      let tx := (p.position.x.toFloat+4).toInt64.toInt.tdiv 16
      let ty := (p.position.y.toFloat+4).toInt64.toInt.tdiv 16
      let (base,overlay) := drawColors p (lightAt tx ty)
      let pose := fun (rgba : UInt32) =>
        let color : OriginalForestChatLayout.Color :=
          ⟨(rgba >>> 24).toNat,((rgba >>> 16) &&& 255).toNat,
           ((rgba >>> 8) &&& 255).toNat,(rgba &&& 255).toNat⟩
        OriginalForestChatLayout.draw "Images/Dust" 1000 120
          (p.position.x-view.position.x) (p.position.y-view.position.y)
          (some ⟨p.frame.x,p.frame.y,p.frame.w,p.frame.h⟩)
          color p.rotation 4 4 p.scale p.scale
      sprites := sprites.push (pose base)
      if p.request.color != 0 && overlay != 0 then sprites := sprites.push (pose overlay)
      if base != 0x000000ff then live := live.push p
  return ⟨live,sprites⟩

end Lean4Terria.OriginalBloodDust
