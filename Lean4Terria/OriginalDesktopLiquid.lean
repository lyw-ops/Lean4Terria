import Lean4Terria.Desktop
import Lean4Terria.OriginalLiquidRender
import Lean4Terria.OriginalSceneWaterStyle
import Lean4Terria.OriginalTileRules

namespace Lean4Terria.OriginalDesktopLiquid

open OriginalCollision (Grid single)
open OriginalLiquidRender (Clock Corners PartialLiquid Prepared Sprite)
open OriginalWorld (Terrain Tile)

@[extern "terria_window_focused"] opaque windowFocused : IO Bool
@[extern "terria_begin_liquid_layer"] private opaque beginLayer (foreground : Bool) (width height : UInt32) : IO Unit
@[extern "terria_end_liquid_layer"] private opaque endLayer : IO Unit
@[extern "terria_draw_liquid_layer"] private opaque drawLayer (foreground : Bool) (x y : UInt32) : IO Unit
@[extern "terria_liquid_quad"] private opaque liquidQuad (asset : @& String)
    (sx sy w h : UInt32) (x y : Float) (tl bl br tr : UInt32) : IO Unit

/-- The opaque preview's light field supplies the colors before liquid opacity. -/
abbrev Light := Nat → Nat → UInt32

structure Viewport where
  x : Int
  y : Int
  width : Nat
  height : Nat

private def offscreen : Nat := 200

def Viewport.area (view : Viewport) (terrain : Terrain) (expanded : Bool := true) :
    Int × Int × Nat × Nat :=
  OriginalLiquidRender.screenDrawArea (single view.x) (single view.y) (single view.width) (single view.height)
    offscreen expanded terrain.header.width terrain.header.height

/-- Skyblock counts active tiles inside the forty-tile border on load, but computes its
density using the whole world's dimensions. Count runs directly on the settled terrain. -/
def skyblockLowTilesAtLoad (terrain : Terrain) : Bool := Id.run do
  let header := terrain.header
  if !header.skyblock then return false
  let mut active := 0
  for x in [40:header.width - 40] do
    let mut startY := 0
    for run in terrain.columns[x]! do
      if run.tile.id.isSome then
        active := active + (min run.endY (header.height - 40) - max startY 40)
      startY := run.endY
      if startY ≥ header.height - 40 then break
  return single active / single (header.width * header.height) < 0.1

/-- The draw-side readers of `Main.SceneMetrics`: `DrawBG` and the water style. -/
def sceneWaterFromMetrics (state : OriginalSceneWaterStyle.State) (view : Viewport) (terrain : Terrain)
    (metrics : OriginalSceneWaterStyle.Metrics) (skyblockLowTiles : Bool) :
    OriginalSceneWaterStyle.State :=
  let header := terrain.header
  let world := OriginalSceneWaterStyle.World.ofTerrain terrain header.infectedSeed skyblockLowTiles
  let camera := (single view.x, single view.y)
  let preferred := OriginalSceneWaterStyle.preferredBGStyle metrics camera.1 view.width header.treeX
  let trees := header.treeBackgrounds
  let backgrounds : OriginalSceneWaterStyle.Backgrounds := {
    treeBG1 := trees.getD 0 0, treeBG2 := trees.getD 1 0, treeBG3 := trees.getD 2 0, treeBG4 := trees.getD 3 0 }
  -- RenderToTargets reads the current background before Main.DoDraw advances DrawBG.
  { metrics := some metrics
    bg := OriginalSceneWaterStyle.drawBG backgrounds state.bg preferred
    waterStyle := OriginalSceneWaterStyle.calculateWaterStyle world metrics state.bg.bgStyle
      header.bloodMoon header.dayTime }

/-- A draw that scans its own cache on every call. The desktop client instead leaves the scan
to the Lighting clock and reads the shared cache through `sharedSceneWater`. -/
def updateSceneWater (state : OriginalSceneWaterStyle.State) (view : Viewport) (terrain : Terrain)
    (position : Float32 × Float32) (updateCount : Nat) (skyblockLowTiles : Bool) :
    OriginalSceneWaterStyle.State :=
  let header := terrain.header
  let world := OriginalSceneWaterStyle.World.ofTerrain terrain header.infectedSeed skyblockLowTiles
  let camera := (single view.x, single view.y)
  let input := OriginalSceneWaterStyle.View.ofScreen position camera view.width view.height
  let area := OriginalSceneWaterStyle.visualScanArea input.scaledPosition input.scaledSize
  let metrics := OriginalSceneWaterStyle.scan world state.metrics updateCount input.center (some area)
  sceneWaterFromMetrics state view terrain metrics skyblockLowTiles

/-- `DrawBG` and the water style from the shared `_playerSceneMetrics`, which
`OriginalSession.State.drawLighting` refreshes on one of every four `LightTiles` calls. -/
def sharedSceneWater (state : OriginalSceneWaterStyle.State) (view : Viewport) (terrain : Terrain)
    (shared : Option OriginalSceneWaterStyle.Metrics) (skyblockLowTiles : Bool) :
    OriginalSceneWaterStyle.State :=
  match shared with
  | some metrics => sceneWaterFromMetrics state view terrain metrics skyblockLowTiles
  | none => state

structure Renderer where
  clock : Clock := {}
  prepared : Prepared := {}
  style : Nat := 0
  alpha : Array Float32 := Array.replicate 15 0
  foreground : Option (Int × Int) := none
  background : Option (Int × Int) := none
  renderCount : Nat := 0
  deriving Inhabited

def Renderer.update (renderer : Renderer) (focused : Bool) (wind : Float32 := 0) : Renderer :=
  { renderer with clock := renderer.clock.update wind (paused := !focused) }

private def lightCorners (light : Light) (x y : Int) : Corners :=
  let x := x.toNat
  let y := y.toNat
  ⟨light x y, light x (y + 1), light (x + 1) (y + 1), light (x + 1) y⟩

private def drawSprite (view : Viewport) (margin : Nat) (sprite : Sprite) (color : Corners) : IO Unit :=
  let x := single (sprite.tileX * 16 + margin - view.x) + sprite.offsetX
  let y := single (sprite.tileY * 16 + margin - view.y) + sprite.offsetY
  if x + single sprite.srcW ≤ 0 || y + single sprite.srcH ≤ 0 ||
      x ≥ single (view.width + 2 * margin : Nat) || y ≥ single (view.height + 2 * margin : Nat) then pure ()
  else liquidQuad s!"Misc/water_{sprite.texture}" sprite.srcX.toNat.toUInt32 sprite.srcY.toNat.toUInt32
    sprite.srcW.toNat.toUInt32 sprite.srcH.toNat.toUInt32 x.toFloat y.toFloat
    color.topLeft color.bottomLeft color.bottomRight color.topRight

private def drawPartial (view : Viewport) (margin : Nat) (light : Light)
    (x y : Int) (draw : PartialLiquid) : IO Unit := do
  let colors := draw.colorScales.foldl
    (fun colors scale => colors.map (OriginalLiquidRender.scaleColor · scale)) (lightCorners light x y)
  let asset := if draw.slopeTexture then s!"LiquidSlope_{draw.texture}" else s!"Liquid_{draw.texture}"
  liquidQuad asset draw.srcX.toNat.toUInt32 draw.srcY.toNat.toUInt32 draw.srcW.toNat.toUInt32 draw.srcH.toNat.toUInt32
    (single (draw.posX + margin - view.x)).toFloat (single (draw.posY + margin - view.y)).toFloat
    (if draw.topZeroed then 0 else colors.topLeft) colors.bottomLeft colors.bottomRight
    (if draw.topZeroed then 0 else colors.topRight)

private def drawFloatingTile (view : Viewport) (grid : Grid) (light : Light) (x y : Int) : IO Unit := do
  let some frame := (grid.tileAt x.toNat y.toNat).frame | return
  let tile := grid.tileAt x.toNat y.toNat
  let height := Int.ofNat ((tile.liquid.map (·.amount.toNat)).getD 0 / 16) - 3
  let above := grid.tileAt x.toNat (y - 1).toNat
  let capped := above.id.any fun id =>
    let rules := grid.rules id
    !above.actuated && rules.solid && !rules.solidTop && above.shape == .full
  let height := if capped then min 8 height else height
  discard <| Desktop.originalSprite "Tiles_518" (x * 16 + offscreen - view.x)
    (y * 16 + offscreen - view.y - height) 16 16 frame.1.toNat frame.2.toNat 16 16
    (color := light x.toNat y.toNat)

private def renderLayer (renderer : Renderer) (view : Viewport) (grid : Grid) (surface : Float)
    (light : Light) (foreground : Bool) (time : Float) : IO Unit := do
  beginLayer foreground (view.width + 2 * offscreen).toUInt32 (view.height + 2 * offscreen).toUInt32
  try
    let mut floating := #[]
    for (style, opacity, singlePass) in OriginalLiquidRender.liquidPasses renderer.style renderer.alpha !foreground do
      if !foreground then
        for (x, y, draw) in OriginalLiquidRender.behindTiles grid surface renderer.prepared.area style
            (fun i => renderer.alpha[i]?.getD 0) 1 do
          drawPartial view offscreen light x y draw
      for sprite in OriginalLiquidRender.sprites renderer.prepared renderer.clock style opacity 1 (!foreground) singlePass do
        let color := (lightCorners light sprite.tileX sprite.tileY).map (OriginalLiquidRender.scaleColor · sprite.opacity)
        drawSprite view offscreen sprite color
        if (grid.tileAt sprite.tileX.toNat sprite.tileY.toNat).id == some 518 then
          floating := floating.push (sprite.tileX, sprite.tileY)
      if !singlePass then
        for draw in OriginalLiquidRender.shimmerSprites renderer.prepared renderer.clock time !foreground do
          let sprite := draw.sprite
          drawSprite view offscreen sprite draw.colors
          if !draw.glitter && (grid.tileAt sprite.tileX.toNat sprite.tileY.toNat).id == some 518 then
            floating := floating.push (sprite.tileX, sprite.tileY)
    -- DrawTileInWater is queued on the deferred SpriteBatch and flushed after TileBatch.
    for (x, y) in floating do drawFloatingTile view grid light x y
  finally endLayer

private def missesView (view : Viewport) (position : Option (Int × Int)) : Bool :=
  match position with
  | none => true
  | some (x, y) => (view.x - x).natAbs > offscreen || (view.y - y).natAbs > offscreen

private def Renderer.prepareFront (renderer : Renderer) (view : Viewport) (terrain : Terrain)
    (grid : Grid) (light : Light) (style : Nat) (time : Float) : IO Renderer := do
  let (x, y, w, h) := view.area terrain
  let prepared := OriginalLiquidRender.prepare grid terrain.header.surface x y w h renderer.prepared
  let alpha := OriginalLiquidRender.fadeLiquidAlpha style renderer.alpha
  let renderer := { renderer with prepared, style, alpha, foreground := some (view.x, view.y) }
  renderLayer renderer view grid terrain.header.surface light true time
  return renderer

/-- Use the foreground and background phases of `Main.RenderToTargets` in the
four-step lighting cycle. A large camera move refreshes an uncovered layer early. -/
def Renderer.prepareTargets (renderer : Renderer) (view : Viewport) (terrain : Terrain)
    (light : Light) (style : Nat) (time : Float) : IO Renderer := do
  let grid := Grid.ofTerrain terrain OriginalTileRules.collision
  let mut renderer := renderer
  if renderer.renderCount == 1 || renderer.foreground.isNone then
    renderer ← renderer.prepareFront view terrain grid light style time
  if renderer.renderCount == 2 || missesView view renderer.background then
    renderLayer renderer view grid terrain.header.surface light false time
    renderer := { renderer with background := some (view.x, view.y) }
  if missesView view renderer.foreground then
    renderer ← renderer.prepareFront view terrain grid light style time
  return { renderer with renderCount := (renderer.renderCount + 1) % 4 }

def Renderer.composite (renderer : Renderer) (view : Viewport) (foreground : Bool) : IO Unit := do
  if let some (x, y) := (if foreground then renderer.foreground else renderer.background) then
    drawLayer foreground (UInt32.ofInt (x - offscreen - view.x)) (UInt32.ofInt (y - offscreen - view.y))

/-- Liquids clipped by solid tiles are rendered over the walls and behind the tile artwork. -/
def Renderer.drawBelowSolids (renderer : Renderer) (view : Viewport) (terrain : Terrain)
    (light : Light) : IO Unit := do
  let grid := Grid.ofTerrain terrain OriginalTileRules.collision
  let (left, top, width, height) := view.area terrain false
  Desktop.premultipliedAlpha true
  try
    for row in [:height + 4] do
      for col in [:width + 4] do
        let x := left - 2 + col
        let y := top + row
        for draw in OriginalLiquidRender.behindTile grid terrain.header.surface x y true renderer.style
            (fun i => renderer.alpha[i]?.getD 0) 1 do
          drawPartial view 0 light x y draw
  finally Desktop.premultipliedAlpha false

end Lean4Terria.OriginalDesktopLiquid
