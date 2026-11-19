import Lean4Terria.OriginalDesktopLiquid
import Lean4Terria.OriginalLiquid.Frame
import Lean4Terria.OriginalWaterfallDraw

namespace Lean4Terria.OriginalDesktopWaterfall

open OriginalCollision (Grid single)
open OriginalDesktopLiquid (Viewport)
open OriginalLiquidRender (Corners)
open OriginalWaterfallDraw (Camera DrawEnv Manager Output Sprite World)
open OriginalWorld (Terrain)

@[extern "terria_waterfall_quad"] private opaque quad (asset : @& String)
    (sx sy w h : UInt32) (x y : Float) (tl bl br tr : UInt32) (flipX : Bool) : IO Unit

private def swapColorBytes (color : UInt32) : UInt32 :=
  (color <<< 24) ||| ((color <<< 8) &&& (0x00ff0000 : UInt32)) |||
    ((color >>> 8) &&& (0x0000ff00 : UInt32)) ||| (color >>> 24)

/-- Read active terrain the same way as the liquid renderer, retaining the live
liquid type at a dry cell when the simulation still holds its native type bits. -/
def world (terrain : Terrain) (liquids : Option OriginalLiquid.Runtime) : World :=
  let grid := Grid.ofTerrain terrain OriginalTileRules.collision
  let base := World.ofGrid grid
  match liquids with
  | none => base
  | some runtime => { base with cell := fun x y =>
    let cell := base.cell x y
    if cell.liquid != 0 then cell else
      let stale := (runtime.liquid.key? x y).bind runtime.liquid.overlay.get?
      { cell with liquidType := (stale.map (·.kind.toUInt8)).getD cell.liquidType } }

structure Renderer where
  manager : Manager := {}
  mouseTextColor : Int := 0
  mouseTextStep : Int := 1
  disco : Array Int := #[255, 0, 0]
  discoPhase : Nat := 0
  deriving Inhabited

/-- Main's waterfall, cursor-colour and disco clocks advance on game updates. -/
def Renderer.update (renderer : Renderer) : Renderer := Id.run do
  let mouseTextColor := (renderer.mouseTextColor + renderer.mouseTextStep) % 256
  let mouseTextStep := if mouseTextColor ≥ 255 then -1 else
    if mouseTextColor ≤ 190 then 1 else renderer.mouseTextStep
  let mut disco := renderer.disco
  let mut discoPhase := renderer.discoPhase
  let phases : Array (Nat × Int) := #[(1, 7), (0, -7), (2, 7), (1, -7), (0, 7), (2, -7)]
  for ((channel, delta), phase) in phases.zipIdx do
    if phase == discoPhase then
      let value := disco[channel]! + delta
      disco := disco.set! channel (min 255 (max 0 value))
      if value ≥ 255 || value ≤ 0 then discoPhase := (discoPhase + 1) % phases.size
  return { renderer with
    manager := { renderer.manager with frames := renderer.manager.frames.update }
    mouseTextColor, mouseTextStep, disco, discoPhase }

def Renderer.prepare (renderer : Renderer) (view : Viewport) (terrain : Terrain)
    (liquids : Option OriginalLiquid.Runtime) (light : OriginalDesktopLiquid.Light)
    (alpha : Array Float32) (forced focused : Bool) : Renderer × Output :=
  let world := world terrain liquids
  let camera : Camera := ⟨single view.x, single view.y, view.width, view.height⟩
  let manager := OriginalWaterfallDraw.findWaterfalls renderer.manager world camera 1 forced
  let env : DrawEnv := {
    world, camera, gfxQuality := 1, worldSurface := terrain.header.surface
    light := fun x y => swapColorBytes (light x.toNat y.toNat)
    discoR := renderer.disco[0]!, discoG := renderer.disco[1]!, discoB := renderer.disco[2]!
    mouseTextColor := renderer.mouseTextColor, lightsEnabled := focused }
  let (manager, output) := OriginalWaterfallDraw.draw manager env alpha
  ({ renderer with manager }, output)

def spriteColors (time : Float) (sprite : Sprite) : Corners :=
  let around (atTile : Int → Int → UInt32) : Corners :=
    let x := sprite.tileX
    let y := sprite.tileY
    ⟨atTile x y, atTile x (y + 1), atTile (x + 1) (y + 1), atTile (x + 1) y⟩
  match sprite.colors with
  | .uniform packed =>
    let color := swapColorBytes packed
    ⟨color, color, color, color⟩
  | .shimmerBase => around (OriginalLiquidRender.shimmerBaseColor time sprite.opacity)
  | .shimmerSparkle => around (OriginalLiquidRender.shimmerGlitterColor time sprite.opacity true)

private def drawSprite (view : Viewport) (time : Float) (asset : String) (sprite : Sprite) : IO Unit := do
  let x := sprite.x - sprite.originX
  let y := sprite.y - sprite.originY
  if x + single sprite.srcW ≤ 0 || y + single sprite.srcH ≤ 0 ||
      x ≥ single view.width || y ≥ single view.height then return
  let colors := spriteColors time sprite
  quad asset sprite.srcX.toNat.toUInt32 sprite.srcY.toNat.toUInt32
    sprite.srcW.toNat.toUInt32 sprite.srcH.toNat.toUInt32 x.toFloat y.toFloat
    colors.topLeft colors.bottomLeft colors.bottomRight colors.topRight (sprite.effects == 1)

def draw (view : Viewport) (output : Output) (time : Float) : IO Unit := do
  if output.cmds.isEmpty then return
  Desktop.premultipliedAlpha true
  try
    for batch in OriginalWaterfallDraw.flushOrder (OriginalWaterfallDraw.batches output.cmds) do
      let asset := s!"Waterfall_{batch.texture}"
      for sprite in batch.sprites do drawSprite view time asset sprite
  finally Desktop.premultipliedAlpha false

end Lean4Terria.OriginalDesktopWaterfall
