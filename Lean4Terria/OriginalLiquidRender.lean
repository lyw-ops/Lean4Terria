import Lean4Terria.OriginalCollision

/-!
# Liquid rendering data (Terraria 1.4.5.8 desktop `LiquidRenderer`)

Handwritten from `Terraria.GameContent.Liquid.LiquidRenderer` in `TerrariaServer.exe` 1.4.5.8.
The client draws with the same code.

* `prepare` is `InternalPrepareDraw(Rectangle)`. Over the area grown by two tiles on every
  side, it runs nine passes with the same column-major pointer walk and in-place updates:
  1. tile read: level `liquid / 255`, solid (`SolidOrSlopedTile`), half brick under liquid,
     wall, type;
  2. visible level of empty cells: the average of the two vertical or the two horizontal
     neighbours;
  3. waterfall run below each liquid cell: `WATERFALL_LENGTH` and opacity;
  4. the four wall values and the edge flags, then `FrameOffset`;
  5. smoothing of the visible walls;
  6. and 7. corner and waterfall-edge fixes;
  8. `LiquidDrawCache`;
  9. shimmer entries moved to `_drawCacheForShimmer`.
* `Clock.update` is `LiquidRenderer.Update(GameTime)`: wind-driven `_frameState` and the
  waterfall clock.
* `sprites` turns a prepared area into the per-cell draws of `DrawNormalLiquids`: texture
  `Images/Misc/water_N`, source rectangle with the animation row, destination offset and the
  opacity factor before lighting.

The renderer reuses `_cache` and `_drawCache` between frames and grows them only. Fields a pass
does not write keep last frame's values: pass 3 covers only rows `0 .. H-11` of the grown area,
and an invisible draw cell keeps its rectangle. `prepare` takes the previous result to
reproduce this; without one, it starts from zeros as a new `LiquidRenderer` does.

`prepare` and `Clock.update` are checked bit for bit against the running game. The rest is
client-side drawing code that the dedicated-server host cannot execute, so it is transcribed
from the 1.4.5.8 IL only:
* `sprites`: `DrawNormalLiquids` without the lighting;
* `screenDrawArea`, `fadeLiquidAlpha`, `liquidPasses`: the frame sequence of `Main.DrawWaters`
  and `Main.DrawLiquid` (which area is prepared, the water-style fade, the draw passes);
* `behindTile` / `behindTiles`: `TileDrawing.DrawTile_LiquidBehindTile` / `DrawPartialLiquid` and
  the background walk of `DrawLiquidBehindTiles`: the liquid seen behind half bricks, slopes and
  block edges, which `TileDrawing` draws apart from these caches.

Not modelled, because it is visual only or client only:
* lighting (`Lighting.GetCornerColors`) and the vertex colours;
* `WaveFilters` and `_waveMask`;
* the lava bubble dust (`_random`);
* `Main.DrawTileInWater`;
* `Main.CalculateWaterStyle` (biome water style) and `WaterfallManager`, which the caller supplies.
-/

namespace Lean4Terria.OriginalLiquidRender

open OriginalCollision (Grid single)
open OriginalWorld (Tile LiquidKind)

/-- `LiquidRenderer.WATERFALL_LENGTH`, `DEFAULT_OPACITY` (read from the running game). -/
def waterfallLength : Nat → Nat
  | 0 => 10 | 1 => 3 | 2 => 2 | _ => 10
def defaultOpacity : Nat → Float32
  | 0 => 0.6 | 1 => 0.95 | 2 => 0.95 | _ => 0.75

/-- `LiquidRenderer.LiquidCache`. -/
structure Cell where
  liquidLevel : Float32 := 0
  visibleLiquidLevel : Float32 := 0
  opacity : Float32 := 0
  isSolid : Bool := false
  isHalfBrick : Bool := false
  hasLiquid : Bool := false
  hasVisibleLiquid : Bool := false
  hasWall : Bool := false
  hasLeftEdge : Bool := false
  hasRightEdge : Bool := false
  hasTopEdge : Bool := false
  hasBottomEdge : Bool := false
  leftWall : Float32 := 0
  rightWall : Float32 := 0
  topWall : Float32 := 0
  bottomWall : Float32 := 0
  visibleLeftWall : Float32 := 0
  visibleRightWall : Float32 := 0
  visibleTopWall : Float32 := 0
  visibleBottomWall : Float32 := 0
  type : Nat := 0
  visibleType : Nat := 0
  frameX : Int := 0
  frameY : Int := 0
  deriving BEq, Repr, Inhabited

/-- `LiquidRenderer.LiquidDrawCache` (and the shimmer copy, `SpecialLiquidDrawCache`). -/
structure DrawCell where
  isVisible : Bool := false
  opacity : Float32 := 0
  type : Nat := 0
  isSurfaceLiquid : Bool := false
  hasWall : Bool := false
  srcX : Int := 0
  srcY : Int := 0
  srcW : Int := 0
  srcH : Int := 0
  offsetX : Float32 := 0
  offsetY : Float32 := 0
  deriving BEq, Repr, Inhabited

/-- The state `PrepareDraw` leaves for drawing. -/
structure Prepared where
  /-- The requested area in tiles: x, y, width, height (`_drawArea`). -/
  area : Int × Int × Nat × Nat := (0, 0, 0, 0)
  /-- `_cache`: element 0 is never written. Element `1 + c * H + r` is column `c`, row `r` of the
  area grown by 2 (`H = height + 4`). -/
  cache : Array Cell := #[{}]
  /-- `_drawCache`: `width * height` cells, column-major over the requested area. -/
  draw : Array DrawCell := #[{}]
  /-- `_drawCacheForShimmer`: the column and row in the grown area, then the cell. The first
  entry with `isVisible = false` ends the list. -/
  shimmer : Array (Int × Int × DrawCell) := #[(0, 0, {})]
  deriving Inhabited

private def kindIndex : LiquidKind → Nat
  | .water => 0 | .lava => 1 | .honey => 2 | .shimmer => 3

private def tileAt (grid : Grid) (x y : Int) : Tile :=
  if x ≥ 0 && y ≥ 0 && x < grid.width && y < grid.height then grid.tileAt x.toNat y.toNat else {}

/-- `WorldGen.SolidOrSlopedTile(tile, false)`. -/
private def solidOrSloped (grid : Grid) (tile : Tile) : Bool :=
  match tile.id with
  | some id => let r := grid.rules id; r.solid && !r.solidTop && !tile.actuated
  | none => false

/-- `Main.IsLiquidStyleWater`: every style except lava 1, honey 11 and shimmer 14. -/
private def isLiquidStyleWaterStyle (style : Nat) : Bool := style != 1 && style != 11 && style != 14

private def fmax (a b : Float32) : Float32 := if a > b then a else b
private def fmin (a b : Float32) : Float32 := if a < b then a else b

/-- `LiquidRenderer.InternalPrepareDraw`. `worldSurface` is `Main.worldSurface`. -/
def prepare (grid : Grid) (worldSurface : Float) (x y : Int) (w h : Nat)
    (previous : Prepared := {}) : Prepared := Id.run do
  let W := w + 4
  let H := h + 4
  let rx := x - 2
  let ry := y - 2
  let mut c : Array Cell := if previous.cache.size < W * H + 1 then Array.replicate (W * H + 1) {} else previous.cache
  let mut d : Array DrawCell := if previous.draw.size < w * h + 1 then Array.replicate (w * h + 1) {} else previous.draw
  let mut sh := if previous.shimmer.size < w * h + 1 then Array.replicate (w * h + 1) (0, 0, {}) else previous.shimmer
  let idx (col row : Nat) : Nat := 1 + col * H + row
  -- pass 1: read the tiles
  let mut k := 1
  for col in [0:W] do
    for row in [0:H] do
      let tile := tileAt grid (rx + col) (ry + row)
      let amount := (tile.liquid.map (·.amount.toNat)).getD 0
      let kind := (tile.liquid.map (kindIndex ·.kind)).getD 0
      let above := c[k - 1]!
      let platform := match tile.id with | some id => (grid.rules id).platform | none => false
      let half := tile.id.isSome && tile.shape == .halfBlock && above.hasLiquid && !platform
      let mut cell := { c[k]! with
        liquidLevel := (amount.toFloat32) / 255
        isHalfBrick := half
        isSolid := solidOrSloped grid tile
        hasLiquid := amount > 0
        visibleLiquidLevel := 0
        hasWall := tile.wall != 0
        type := kind }
      if cell.isHalfBrick && !cell.hasLiquid then cell := { cell with type := above.type }
      c := c.set! k cell
      k := k + 1
  -- pass 2: visible level of empty cells
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let i := idx col row
      let cell := c[i]!
      let mut v : Float32 := 0
      let mut cell' := cell
      if cell.isHalfBrick && c[i - 1]!.hasLiquid then v := 1
      else if !cell.hasLiquid then
        let up := c[i - 1]!; let dn := c[i + 1]!; let lf := c[i - H]!; let rt := c[i + H]!
        if up.hasLiquid && dn.hasLiquid && up.type == dn.type && !up.isSolid && !dn.isSolid then
          v := up.liquidLevel + dn.liquidLevel
          cell' := { cell' with type := up.type }
        if lf.hasLiquid && rt.hasLiquid && lf.type == rt.type && !lf.isSolid && !rt.isSolid then
          v := fmax v (lf.liquidLevel + rt.liquidLevel)
          cell' := { cell' with type := lf.type }
        v := v * 0.5
      else v := cell.liquidLevel
      c := c.set! i { cell' with visibleLiquidLevel := v, hasVisibleLiquid := v != 0 }
  -- pass 3: opacity and the waterfall run below each liquid cell (rows 0 .. H-11)
  for col in [0:W] do
    for row in [0:H - 10] do
      let i := idx col row
      let mut cell := c[i]!
      if cell.hasVisibleLiquid && (!cell.isSolid || cell.isHalfBrick) then
        cell := { cell with opacity := 1, visibleType := cell.type }
        c := c.set! i cell
        let len := waterfallLength cell.type
        let step : Float32 := 1 / (len + 1).toFloat32
        let mut f : Float32 := 1
        for j in [1:len + 1] do
          f := f - step
          let t := c[i + j]!
          if t.isSolid then break
          c := c.set! (i + j) { t with visibleLiquidLevel := fmax t.visibleLiquidLevel (cell.visibleLiquidLevel * f),
                                       opacity := f, visibleType := cell.type }
      cell := c[i]!
      if cell.isSolid && !cell.isHalfBrick then
        c := c.set! i { cell with visibleLiquidLevel := 1, hasVisibleLiquid := false }
      else
        c := c.set! i { cell with hasVisibleLiquid := cell.visibleLiquidLevel != 0 }
  -- pass 4: walls, edges, frame
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let i := idx col row
      let cell := c[i]!
      if !cell.hasVisibleLiquid then
        c := c.set! i { cell with hasLeftEdge := false, hasTopEdge := false, hasRightEdge := false, hasBottomEdge := false }
        continue
      let up := c[i - 1]!; let dn := c[i + 1]!; let lf := c[i - H]!; let rt := c[i + H]!
      let v := cell.visibleLiquidLevel
      let mut l : Float32 := 0; let mut r : Float32 := 1; let mut t : Float32 := 0; let mut b : Float32 := 1
      if !up.hasVisibleLiquid then t := t + dn.visibleLiquidLevel * (1 - v)
      if !dn.hasVisibleLiquid && !dn.isSolid && !dn.isHalfBrick then b := b - up.visibleLiquidLevel * (1 - v)
      if !lf.hasVisibleLiquid && !lf.isSolid && !lf.isHalfBrick then l := l + rt.visibleLiquidLevel * (1 - v)
      if !rt.hasVisibleLiquid && !rt.isSolid && !rt.isHalfBrick then r := r - lf.visibleLiquidLevel * (1 - v)
      let topE := if up.hasVisibleLiquid || up.isSolid then t != 0 else true
      let botE := if dn.hasVisibleLiquid || dn.isSolid then b != 1 else true
      let leftE := if lf.hasVisibleLiquid || lf.isSolid then l != 0 else true
      let rightE := if rt.hasVisibleLiquid || rt.isSolid then r != 1 else true
      let mut fx : Int := 0; let mut fy : Int := 0
      if !leftE then fx := fx + (if rightE then 32 else 16)
      if leftE && rightE then
        fx := 16; fy := fy + 32
        if topE then fy := 16
      else if !topE then
        fy := fy + (if !leftE && !rightE then 48 else 16)
      if fy == 16 && (leftE != rightE) && (ry + row) % 2 == 0 then fy := fy + 16
      c := c.set! i { cell with leftWall := l, rightWall := r, bottomWall := b, topWall := t,
                                hasTopEdge := topE, hasBottomEdge := botE, hasLeftEdge := leftE, hasRightEdge := rightE,
                                frameX := fx, frameY := fy }
  -- pass 5: smoothed visible walls
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let i := idx col row
      let cell := c[i]!
      if !cell.hasVisibleLiquid then continue
      let up := c[i - 1]!; let dn := c[i + 1]!; let lf := c[i - H]!; let rt := c[i + H]!
      let mut n := { cell with visibleLeftWall := cell.leftWall, visibleRightWall := cell.rightWall,
                               visibleTopWall := cell.topWall, visibleBottomWall := cell.bottomWall }
      if up.hasVisibleLiquid && dn.hasVisibleLiquid then
        if cell.hasLeftEdge then n := { n with visibleLeftWall := (cell.leftWall * 2 + up.leftWall + dn.leftWall) * 0.25 }
        if cell.hasRightEdge then n := { n with visibleRightWall := (cell.rightWall * 2 + up.rightWall + dn.rightWall) * 0.25 }
      if lf.hasVisibleLiquid && rt.hasVisibleLiquid then
        if cell.hasTopEdge then n := { n with visibleTopWall := (cell.topWall * 2 + lf.topWall + rt.topWall) * 0.25 }
        if cell.hasBottomEdge then n := { n with visibleBottomWall := (cell.bottomWall * 2 + lf.bottomWall + rt.bottomWall) * 0.25 }
      c := c.set! i n
  -- pass 6: one-sided surface cells and the cell above a falling column (in place)
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let i := idx col row
      let cell := c[i]!
      if !cell.hasLiquid then continue
      let dn := c[i + 1]!; let lf := c[i - H]!; let rt := c[i + H]!
      if cell.hasTopEdge && !cell.hasBottomEdge && (cell.hasLeftEdge != cell.hasRightEdge) then
        if cell.hasRightEdge then
          c := c.set! i { cell with visibleRightWall := dn.visibleRightWall, visibleTopWall := lf.visibleTopWall }
        else
          c := c.set! i { cell with visibleLeftWall := dn.visibleLeftWall, visibleTopWall := rt.visibleTopWall }
      else if dn.frameX == 16 && dn.frameY == 32 then
        if cell.visibleLeftWall > 0.5 then
          c := c.set! i { cell with visibleLeftWall := 0, frameX := 0, frameY := 0 }
        else if cell.visibleRightWall < 0.5 then
          c := c.set! i { cell with visibleRightWall := 1, frameX := 32, frameY := 0 }
  -- pass 7: inner corners (in place)
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let i := idx col row
      let cell := c[i]!
      if !cell.hasLiquid || cell.hasBottomEdge || cell.hasLeftEdge || cell.hasTopEdge || cell.hasRightEdge then continue
      let up := c[i - 1]!; let lf := c[i - H]!; let rt := c[i + H]!
      let reset (n : Cell) : Cell := { n with visibleLeftWall := 0, visibleTopWall := 0, visibleRightWall := 1, visibleBottomWall := 1 }
      if lf.hasTopEdge && up.hasLeftEdge then
        let fx := max 4 (16 - up.visibleLeftWall * 16).toInt32.toInt - 4
        let fy := 48 + max 4 (16 - lf.visibleTopWall * 16).toInt32.toInt - 4
        c := c.set! i (reset { cell with frameX := fx, frameY := fy })
      else if rt.hasTopEdge && up.hasRightEdge then
        let fx := 32 - min 16 ((up.visibleRightWall * 16).toInt32.toInt - 4)
        let fy := 48 + max 4 (16 - rt.visibleTopWall * 16).toInt32.toInt - 4
        c := c.set! i (reset { cell with frameX := fx, frameY := fy })
  -- pass 8: draw cache
  let mut di := 0
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let cell := c[idx col row]!
      if cell.hasVisibleLiquid then
        let a := fmin 0.75 cell.visibleLeftWall
        let b := fmax 0.25 cell.visibleRightWall
        let t := fmin 0.75 cell.visibleTopWall
        let mut bt := fmax 0.25 cell.visibleBottomWall
        if cell.isHalfBrick && cell.isSolid && bt > 0.5 then bt := 0.5
        let visible := cell.hasWall || !cell.isHalfBrick || !cell.hasLiquid || !(cell.liquidLevel < 1)
        let tileY := ry + row
        d := d.set! di {
          isVisible := visible
          srcX := (16 - b * 16).toInt32.toInt + cell.frameX
          srcY := (16 - bt * 16).toInt32.toInt + cell.frameY
          srcW := (((b - a) * 16).toFloat.ceil).toInt64.toInt
          srcH := (((bt - t) * 16).toFloat.ceil).toInt64.toInt
          isSurfaceLiquid := cell.frameX == 16 && cell.frameY == 0 && tileY.toNat.toFloat > worldSurface - 40 && tileY ≥ 0
          opacity := cell.opacity
          offsetX := ((a * 16).toFloat.floor).toFloat32
          offsetY := ((t * 16).toFloat.floor).toFloat32
          type := cell.visibleType
          hasWall := cell.hasWall }
      else
        d := d.set! di { d[di]! with isVisible := false }
      di := di + 1
  -- pass 9: shimmer cells move to their own list
  let mut si := 0
  di := 0
  for col in [2:W - 2] do
    for row in [2:H - 2] do
      let e := d[di]!
      if e.isVisible && e.type == 3 then
        sh := sh.set! si (col, row, e)
        si := si + 1
        d := d.set! di { e with isVisible := false }
      di := di + 1
  let (sx, sy, se) := sh[si]!
  sh := sh.set! si (sx, sy, { se with isVisible := false })
  return { area := (x, y, w, h), cache := c, draw := d, shimmer := sh }

/-! ## Animation clock (`LiquidRenderer.Update`) -/

structure Clock where
  frameState : Float32 := 0
  waterfallFrameState : Float32 := 0
  animationFrame : Nat := 0
  waterfallAnimationFrame : Nat := 0
  deriving BEq, Repr, Inhabited

/-- `(float)TimeSpan.FromTicks(166667).TotalSeconds`, one 60 Hz update. -/
def secondsPerTick : Float32 := (166667.0 * (1.0 / 10000000.0) : Float).toFloat32

private def mod16 (v : Float32) : Float32 := Id.run do
  let mut x := v
  while x ≥ 16 do x := x - 16
  return x

/-- One `Update(gameTime)`. It does nothing while `FocusHelper.PauseLiquidRenderer` holds, that
is while the window is not focused or the game is paused. -/
def Clock.update (clock : Clock) (windSpeedCurrent : Float32) (elapsed : Float32 := secondsPerTick)
    (paused : Bool := false) : Clock := Id.run do
  if paused then return clock
  let k := windSpeedCurrent * 25
  let k := if k < 0 then k - 6 else k + 6
  let mut fs := clock.frameState + k * elapsed
  let mut wfs := clock.waterfallFrameState + 0.5 * elapsed
  if fs < 0 then fs := fs + 16
  fs := mod16 fs
  wfs := mod16 wfs
  return { frameState := fs, waterfallFrameState := wfs,
           animationFrame := fs.toInt32.toInt.toNat, waterfallAnimationFrame := wfs.toInt32.toInt.toNat }

/-! ## Draw list (`DrawNormalLiquids`) -/

/-- One liquid quad: texture `Images/Misc/water_{texture}`, source rectangle in that texture,
destination `(tileX * 16, tileY * 16) + offset + drawOffset` (`drawOffset` = off-screen margin
minus `Main.screenPosition`), and the opacity factor that multiplies the four corner light
colours (capped at 1). -/
structure Sprite where
  tileX : Int
  tileY : Int
  texture : Nat
  srcX : Int
  srcY : Int
  srcW : Int
  srcH : Int
  offsetX : Float32
  offsetY : Float32
  opacity : Float32
  type : Nat
  deriving BEq, Repr

/-- `DrawNormalLiquids(spriteBatch, drawOffset, waterStyle, globalAlpha, isBackgroundDraw,
drawSinglePassLiquids)` without the lighting. `globalAlpha` is `Main.liquidAlpha[waterStyle]`.
`lavaOpacity` is the local player's `lavaOpacity`. Shimmer cells are not in this list:
`DrawShimmer` draws them. -/
def sprites (p : Prepared) (clock : Clock) (waterStyle : Nat) (globalAlpha lavaOpacity : Float32)
    (isBackgroundDraw : Bool := false) (drawSinglePassLiquids : Bool := false) : Array Sprite := Id.run do
  let (x, y, w, h) := p.area
  let mut out := #[]
  let mut i := 0
  for col in [0:w] do
    for row in [0:h] do
      let e := p.draw[i]!
      i := i + 1
      if !e.isVisible || (drawSinglePassLiquids && e.type != 0) then continue
      let srcY := if e.isSurfaceLiquid then 1280
        else if e.srcX == 16 then e.srcY + clock.waterfallAnimationFrame * 80
        else e.srcY + clock.animationFrame * 80
      let mut opacity := e.opacity * (if isBackgroundDraw then 1 else defaultOpacity e.type)
      let mut texture := e.type
      if e.type == 0 then
        texture := waterStyle
        opacity := opacity * globalAlpha
      else if e.type == 1 then opacity := opacity * lavaOpacity
      else if e.type == 2 then texture := 11
      out := out.push { tileX := x + col, tileY := y + row, texture, srcX := e.srcX, srcY, srcW := e.srcW, srcH := e.srcH,
                        offsetX := e.offsetX, offsetY := e.offsetY, opacity := fmin 1 opacity, type := e.type }
  return out

/-! ## Shimmer (`DrawShimmer`) -/

/-- Packed RGBA in the order used to submit a liquid quad. -/
structure Corners where
  topLeft : UInt32
  bottomLeft : UInt32
  bottomRight : UInt32
  topRight : UInt32
  deriving BEq, Repr, Inhabited

def Corners.map (colors : Corners) (f : UInt32 → UInt32) : Corners :=
  ⟨f colors.topLeft, f colors.bottomLeft, f colors.bottomRight, f colors.topRight⟩

private def rgba (r g b a : UInt32) : UInt32 :=
  (r <<< 24) ||| (g <<< 16) ||| (b <<< 8) ||| a

private def colorByte (value : Float32) : UInt32 := (min 255 (max 0 value)).toUInt32

/-- FNA's `Color * float`, including truncation of alpha and each RGB channel. -/
def scaleColor (color : UInt32) (factor : Float32) : UInt32 :=
  let scale (shift : UInt32) := colorByte (((color >>> shift) &&& 255).toNat.toFloat32 * factor)
  rgba (scale 24) (scale 16) (scale 8) (scale 0)

private def shimmerWave (time : Float) (x y : Float32) : Float32 :=
  ((((x + y / 6) / 10).toFloat - time / 360) * 6.2831854820251465).sin.toFloat32

/-- `SetShimmerVertexColors` replaces lighting with a spatial shimmer wave. -/
def shimmerBaseColor (time : Float) (opacity : Float32) (x y : Int) : UInt32 :=
  let wave := shimmerWave time (single x) (single y)
  let amount := 0.1 + wave * 0.4
  let alpha := colorByte (255 * opacity)
  let channel (low high : Float32) :=
    let value := low + (high - low) * amount
    colorByte ((alpha.toNat.toFloat32 / 255 * value) * 255)
  rgba (channel (165 / 255) (205 / 255)) (channel (130 / 255) (205 / 255))
    (channel (238 / 255) 1) alpha

private def noise (x y : UInt32) : UInt32 :=
  let a : UInt32 := (36469 : UInt32) * (x &&& (65535 : UInt32)) + (x >>> 16)
  let b : UInt32 := (18012 : UInt32) * (y &&& (65535 : UInt32)) + (y >>> 16)
  (a <<< 16) + b

private def hueChannel (hue : Float) : UInt32 :=
  let t := if hue < 0 then hue + 1 else if hue > 1 then hue - 1 else hue
  let v := if 6 * t < 1 then 6 * t else if 2 * t < 1 then 1
    else if 3 * t < 2 then (2 / 3 - t) * 6 else 0
  let scaled := v * 255
  let whole := scaled.toUInt64.toNat
  let fraction := scaled - whole.toFloat
  (if fraction > 0.5 || (fraction == 0.5 && whole % 2 == 1) then whole + 1 else whole).toUInt32

/-- The glitter texture adds light with zero alpha under FNA's premultiplied blend. -/
def shimmerGlitterColor (time : Float) (opacity : Float32) (top : Bool) (x y : Int) : UInt32 :=
  let px := single x
  let py := single y
  let turns := (((px + py / 6).toFloat + time / 30) / 6).toFloat32
  let hue := (turns - single turns.toInt32.toInt).toFloat
  let strength : Float32 := if top then 0.5 else
    let wave := shimmerWave time px py
    let a := min 1 (max 0 ((wave + 0.5) / 1.5)) * 0.35
    let phase := ((noise (UInt32.ofInt x) (UInt32.ofInt y)).toNat.toFloat32 / 10).toFloat + time / 180
    let b := phase.sin.toFloat32
    min 1 (max 0 (a * b / 0.5))
  let channel (h : Float) :=
    let v := (hueChannel h).toNat.toFloat32 / 255
    let t := colorByte (v * strength * 255)
    colorByte (t.toNat.toFloat32 * opacity)
  rgba (channel (hue + 1 / 3)) (channel hue) (channel (hue - 1 / 3)) 0

def shimmerFrame (time : Float) (top : Bool) (x y : Int) : Nat :=
  let px := single x + 0.5
  let py := single y + 0.5
  let frame := ((px + py / 6) / 10).toFloat - time / 360
  let frame := if top then frame else frame + (px + py).toFloat
  (frame.toInt64.toInt % 16).toNat

structure ShimmerSprite where
  sprite : Sprite
  colors : Corners
  glitter : Bool
  deriving BEq, Repr

/-- The base quad and optional sparkle quad of each populated shimmer cache entry. -/
def shimmerSprites (p : Prepared) (clock : Clock) (time : Float)
    (isBackgroundDraw : Bool := false) : Array ShimmerSprite := Id.run do
  let (left, top, _, _) := p.area
  let mut out := #[]
  for (col, row, cell) in p.shimmer do
    if !cell.isVisible then break
    let x := left + col - 2
    let y := top + row - 2
    let opacity := min 1 (cell.opacity * (if isBackgroundDraw then 1 else 0.75))
    let base := shimmerBaseColor time opacity
    let srcY := if cell.isSurfaceLiquid then 1280 else cell.srcY + clock.animationFrame * 80
    let sprite : Sprite := {
      tileX := x, tileY := y, texture := 14, srcX := cell.srcX, srcY, srcW := cell.srcW, srcH := cell.srcH
      offsetX := cell.offsetX, offsetY := cell.offsetY, opacity, type := 3 }
    out := out.push ⟨sprite, ⟨base x y, base x (y + 1), base (x + 1) (y + 1), base (x + 1) y⟩, false⟩
    let top := cell.srcX != 16 || cell.srcY % 80 != 48
    if top || (x + y) % 2 == 0 then
      let sparkle := shimmerGlitterColor time cell.opacity top
      let sprite := { sprite with srcX := cell.srcX + 48, srcY := cell.srcY + shimmerFrame time top x y * 80 }
      out := out.push ⟨sprite,
        ⟨sparkle x y, sparkle x (y + 1), sparkle (x + 1) (y + 1), sparkle (x + 1) y⟩, true⟩
  return out

/-! ## Frame sequence (`Main.DrawWaters`, `Main.DrawLiquid`), IL only -/

/-- `TileDrawing.GetScreenDrawArea(useOffscreen, ...)` as `Main.DrawWaters` hands it to
`PrepareDraw`: `(firstX, firstY, lastX - firstX, lastY - firstY)`. `camera` is
`Main.Camera.ScaledPosition`, `size` is `ScaledSize`; with `useOffscreen` both sides grow by
`Main.offScreenRange` pixels. `DrawWaters` passes `useOffscreen = !Main.drawToScreen` and only
prepares when it is drawing the foreground into the render target (`!drawToScreen`). -/
def screenDrawArea (cameraX cameraY sizeX sizeY : Float32) (offScreenRange : Nat) (useOffscreen : Bool)
    (maxTilesX maxTilesY : Nat) : Int × Int × Nat × Nat :=
  let off : Float32 := if useOffscreen then single offScreenRange else 0
  let firstX := max (((cameraX - off) / 16 - 1).toInt32.toInt) 4
  let lastX := min (((cameraX + sizeX + off) / 16).toInt32.toInt + 2) ((maxTilesX : Int) - 4)
  let firstY := max (((cameraY - off) / 16 - 1).toInt32.toInt) 4
  let lastY := min (((cameraY + sizeY + off) / 16).toInt32.toInt + 5) ((maxTilesY : Int) - 4)
  (firstX, firstY, (lastX - firstX).toNat, (lastY - firstY).toNat)

/-- `Main.CalculateWaterStyle` given the original scene and background inputs. -/
def waterStyle (background : Nat) (cameraTileY : Int) (surface rock : Float)
    (remix glowshroom bloodMoonAtNight : Bool) (fountain : Option Nat := none) : Nat :=
  if let some style := fountain then style
  else if bloodMoonAtNight then 9
  else match background with
    | 1 | 5 => 2
    | 2 => if !remix && cameraTileY.toFloat > surface then 12 else 6
    | 3 => 3
    | 4 => 13
    | 6 | 13 => 4
    | 7 => 5
    | 8 | 14 => 10
    | _ =>
      if remix then
        if cameraTileY.toFloat > rock then 7
        else if cameraTileY.toFloat > surface then 8 else 0
      else if cameraTileY.toFloat > rock + 40 then
        if glowshroom then 7 else 8
      else if cameraTileY.toFloat > surface then 7 else 0

/-- The per-frame fade at the start of a foreground `DrawWaters`, after
`Main.waterStyle = CalculateWaterStyle(false)`: when the current style is a water style, its
`Main.liquidAlpha` rises by 0.2 up to 1 and every other entry falls by 0.2 down to 0. -/
def fadeLiquidAlpha (waterStyle : Nat) (liquidAlpha : Array Float32) : Array Float32 :=
  if !isLiquidStyleWaterStyle waterStyle then liquidAlpha else
  liquidAlpha.mapIdx fun i a => if i == waterStyle then fmin (a + 0.2) 1 else fmax (a - 0.2) 0

/-- The `Main.DrawLiquid(bg, style, alpha, drawSinglePassLiquids)` calls of one `DrawWaters`, in
order: each other visible water style as a water-only pass (`sprites ... drawSinglePassLiquids
:= true`), then the current style. Each call draws `sprites p clock style alpha lavaOpacity bg
single` with `drawOffset = (offScreenRange or 0, same) - screenPosition`; the last call also runs
`DrawShimmer`, and with `bg` every call first runs `TileDrawing.DrawLiquidBehindTiles(style)`. -/
def liquidPasses (waterStyle : Nat) (liquidAlpha : Array Float32) (isBackground : Bool) : Array (Nat × Float32 × Bool) := Id.run do
  let mut out := #[]
  for i in [0:15] do
    let a := liquidAlpha[i]?.getD 0
    if isLiquidStyleWaterStyle i && a > 0 && i != waterStyle then
      out := out.push (i, if isBackground then 1 else a, true)
  let main : Float32 := if out.isEmpty then 1 else liquidAlpha[waterStyle]?.getD 0
  return out.push (waterStyle, main, false)

/-! ## Liquid behind half bricks and slopes (`TileDrawing.DrawTile_LiquidBehindTile`)

Transcribed from the IL only. `TileDrawing` draws through the client's `TileBatch` and
lighting, so the dedicated-server host cannot run it. The tile reads, the neighbour rules and
the rectangles below follow `DrawTile_LiquidBehindTile` and `DrawPartialLiquid` instruction by
instruction. -/

/-- `TileID.Sets.BlocksWaterDrawingBehindSelf` (read from the running game). -/
def blocksWaterDrawingBehindSelf (id : UInt16) : Bool := id == 54 || id == 328 || id == 459 || id == 470 || id == 541

/-- `Main.IsLiquidStyleWater`: every style except lava 1, honey 11 and shimmer 14. -/
def isLiquidStyleWater (style : Nat) : Bool := isLiquidStyleWaterStyle style

/-- One `DrawPartialLiquid` call: texture `Images/Liquid_{texture}`, or `Images/LiquidSlope_{texture}`
when `slopeTexture`; source rectangle; world position in pixels (subtract `screenPosition`, add the
screen offset); and the factors on the four corner light colours. -/
structure PartialLiquid where
  texture : Nat
  slopeTexture : Bool
  srcX : Int
  srcY : Int
  srcW : Int
  srcH : Int
  posX : Int
  posY : Int
  /-- The corner colours are multiplied by each factor in turn: `Color * float` rounds to bytes after every multiply.
  With `topZeroed`, the two top corners are then multiplied by 0. -/
  colorScales : Array Float32
  topZeroed : Bool
  deriving BEq, Repr

private def blockType (tile : Tile) : Nat := match tile.shape with
  | .full => 0 | .halfBlock => 1 | .slope n => n.toNat + 1
private def slopeOf (tile : Tile) : Nat := match tile.shape with
  | .slope n => n.toNat | _ => 0
private def liquidOf (tile : Tile) : Nat × Nat :=
  if tile.id == some 379 then (0, 0)
  else ((tile.liquid.map (·.amount.toNat)).getD 0, (tile.liquid.map (kindIndex ·.kind)).getD 0)

/-- `WorldGen.SolidTile(x, y, false)`. -/
private def solidTile (grid : Grid) (x y : Int) : Bool :=
  let t := tileAt grid x y
  match t.id with
  | some id => let r := grid.rules id; !t.actuated && r.solid && !r.solidTop && t.shape == .full
  | none => false

/-- `TileDrawing.DrawTile_LiquidBehindTile(solidLayerOnly, waterStyleOverride, ...)` for the tile
at `(x, y)`. It returns the `DrawPartialLiquid` calls: at most one water-style blend draw and
the main draw. The caller supplies:
* `waterfall`, the result of `WaterfallManager.CheckForWaterfall(x, y)`;
* `waterStyle`, `Main.waterStyle`;
* `liquidAlpha`, `Main.liquidAlpha`;
* `lavaOpacity`, the local player's.

A non-negative `waterStyleOverride` also sets `Main.waterStyle` in the original. -/
def behindTile (grid : Grid) (worldSurface : Float) (x y : Int) (solidLayerOnly : Bool) (waterStyle : Nat)
    (liquidAlpha : Nat → Float32) (lavaOpacity : Float32) (waterfall : Bool := false)
    (waterStyleOverride : Int := -1) : Array PartialLiquid := Id.run do
  let tile := tileAt grid x y
  let some id := tile.id | return #[]
  let rules := grid.rules id
  if rules.solidTop || !rules.solid then return #[]
  let (rL, rT) := liquidOf (tileAt grid (x + 1) y)
  let (lL, lT) := liquidOf (tileAt grid (x - 1) y)
  let (uL, uT) := liquidOf (tileAt grid x (y - 1))
  let (dL, dT) := liquidOf (tileAt grid x (y + 1))
  if tile.shape == .halfBlock && (lL > 160 || rL > 160) && waterfall then return #[]
  if blocksWaterDrawingBehindSelf id && slopeOf tile == 0 then return #[]
  let selfL := (tile.liquid.map (·.amount.toNat)).getD 0
  let selfT := (tile.liquid.map (kindIndex ·.kind)).getD 0
  let slope := slopeOf tile
  let bt := blockType tile
  if id == 379 && selfL > 0 then return #[]
  let mut maxL := 0
  let mut lf := false; let mut rf := false; let mut tf := false; let mut bf := false; let mut self := false
  let mut tex := 0
  let mut water := false
  let setTex (t : Nat) (tex : Nat) (water : Bool) : Nat × Bool := match t with
    | 0 => (tex, true) | 1 => (1, water) | 2 => (11, water) | 3 => (14, water) | _ => (tex, water)
  if id == 546 && selfL > 0 then
    self := true
    bf := true
    lf := true
    rf := true
    (tex, water) := setTex selfT tex water
    maxL := selfL
  else
    if selfL > 0 && bt != 0 && (bt != 1 || selfL > 160) then
      self := true
      (tex, water) := setTex selfT tex water
      if selfL > maxL then maxL := selfL
    if lL > 0 && slope != 1 && slope != 3 then
      lf := true
      (tex, water) := setTex lT tex water
      if lL > maxL then maxL := lL
    if rL > 0 && slope != 2 && slope != 4 then
      rf := true
      (tex, water) := setTex rT tex water
      if rL > maxL then maxL := rL
    if uL > 0 && slope != 3 && slope != 4 then
      tf := true
      (tex, water) := setTex uT tex water
    if dL > 0 && slope != 1 && slope != 2 then
      if dL > 240 then bf := true
      (tex, water) := setTex dT tex water
  if !tf && !bf && !lf && !rf && !self then return #[]
  let style := if waterStyleOverride != -1 then waterStyleOverride.toNat else waterStyle
  if tex == 0 then tex := style
  let mut px : Int := x * 16
  let mut py : Int := y * 16
  let mut rect : Int × Int × Int × Int := (0, 4, 16, 16)
  if bf && (lf || rf) then
    lf := true
    rf := true
  if tf && (lf || rf) then pure ()
  else if bf && tf then pure ()
  else if tf then
    rect := if tile.shape == .halfBlock || slope != 0 then (0, 4, 16, 12) else (0, 4, 16, 4)
  else if bf && !lf && !rf then
    py := y * 16 + 12; rect := (0, 4, 16, 4)
  else
    let f : Float32 := single (256 - maxL : Nat) / 32
    let edge : Int := if uL > 0 || (bt == 0 && solidTile grid x (y - 1)) then 4 else 0
    let n : Int := f.toInt32.toInt * 2
    py := y * 16 + n
    if slope != 0 then rect := (0, n, 16, 16 - n)
    else if (lf && rf) || tile.shape == .halfBlock then rect := (0, edge, 16, 16 - n)
    else if lf then rect := (0, edge, 4, 16 - n)
    else
      px := x * 16 + 12
      rect := (0, edge, 4, 16 - n)
  let mut alpha : Float32 := 0.5
  if tex == 1 then alpha := lavaOpacity
  else if tex == 11 then alpha := fmax (alpha * 1.7) 1
  if tex != 1 || !(lavaOpacity < 1) then
    if !(y.toNat.toFloat > worldSurface) || alpha > 1 then
      alpha := 1
      if tile.wall == 21 then alpha := 0.9
      else if tile.wall > 0 then alpha := 0.6
  if tile.shape == .halfBlock && uL > 0 && tile.wall > 0 then alpha := 0
  if slope == 4 && lL == 0 && !solidTile grid (x - 1) y then alpha := 0
  if slope == 3 && rL == 0 && !solidTile grid (x + 1) y then alpha := 0
  let topZeroed := tile.shape == .halfBlock && uL > 0 && y.toNat.toFloat > worldSurface
  let partialOf (texture : Nat) (scales : Array Float32) : PartialLiquid :=
    -- DrawPartialLiquid(!solidLayerOnly, tile, ...): a sloped tile behind blocks uses LiquidSlope with X + 18 * (slope - 1)
    let behindBlocks := !solidLayerOnly
    let flat := (behindBlocks && !blocksWaterDrawingBehindSelf id) || slope == 0
    let (rx, ry, rw, rh) := rect
    { texture, slopeTexture := !flat, srcX := if flat then rx else rx + 18 * ((slope : Int) - 1), srcY := ry, srcW := rw, srcH := rh,
      posX := px, posY := py, colorScales := scales, topZeroed }
  let mut out := #[]
  let mut blended := false
  if water then
    for st in [0:15] do
      if isLiquidStyleWater st && liquidAlpha st > 0 && st != tex then
        out := out.push (partialOf st #[alpha])
        blended := true
        break
  -- the second draw reuses the first colours (already scaled by alpha, top corners zeroed) times liquidAlpha[tex]
  out := out.push (partialOf tex (if blended then #[alpha, liquidAlpha tex] else #[alpha, 1]))
  return out

/-- `TileDrawing.DrawLiquidBehindTiles(waterStyleOverride)`, which `Main.DrawLiquid` runs before
each background pass (`isBackground`) with that pass's style. Over `area` from `screenDrawArea`
(`useOffscreen = !drawToScreen`) it walks rows `firstY .. lastY + 3` and, in each row, columns
`firstX - 2 .. lastX + 1`, calling `DrawTile_LiquidBehindTile(false, style, ...)` on every tile.
The result lists `(x, y, draw)` in that order. `waterfall x y` is `WaterfallManager.CheckForWaterfall`. -/
def behindTiles (grid : Grid) (worldSurface : Float) (area : Int × Int × Nat × Nat) (style : Nat)
    (liquidAlpha : Nat → Float32) (lavaOpacity : Float32) (waterfall : Int → Int → Bool := fun _ _ => false) :
    Array (Int × Int × PartialLiquid) := Id.run do
  let (firstX, firstY, w, h) := area
  let lastX := firstX + w
  let lastY := firstY + h
  let mut out := #[]
  for j in [0:(lastY + 4 - firstY).toNat] do
    let y := firstY + j
    for i in [0:(lastX + 2 - (firstX - 2)).toNat] do
      let x := firstX - 2 + i
      for d in behindTile grid worldSurface x y false style liquidAlpha lavaOpacity (waterfall x y) style do
        out := out.push (x, y, d)
  return out

end Lean4Terria.OriginalLiquidRender
