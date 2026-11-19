import Lean4Terria.OriginalCollision

/-!
# Waterfalls (Terraria 1.4.5.8 desktop `WaterfallManager`)

Handwritten from `Terraria.WaterfallManager` in `TerrariaServer.exe` 1.4.5.8 (the client runs the same code).

* `findWaterfalls` is `FindWaterfalls(bool forced)`. It is gated by `findWaterfallCount` (it rescans
  only every 30th call unless forced). It scans the camera rectangle widened by `waterfallDist` and
  produces the source list in x-major, y-inner order:
  * a half brick that spills: liquid > 160 on one side, an open side, and at most 15 liquid (or a
    solid tile) above. The kind comes from the `liquidType` bits of above, right and left, even when
    the amount is zero (lava 1, honey 14, shimmer 25, otherwise 0 = the current water style);
  * the cell under a rain cloud (196 → 11), a snow cloud (460 → 22) and tile 717 (→ 26) when that
    cell is open.
* `checkForWaterfall` is `CheckForWaterfall(x, y)`.
* `Frames.update` is `UpdateFrame()`: the twelve animation counters.
* `draw` is `Draw()`: `stopAtStep := waterfallDist`, then `DrawWaterfall(style, liquidAlpha[i])` for
  every visible water style. `drawWaterfall` walks each source:
  * clouds: a column of rain, snow or lava-rain sprites;
  * water, lava, honey and shimmer: the falling and running path with its turn counter,
    slope, half-brick and corner pieces, the tile-driven styles (rainbow brick, gemsparks), the
    fade over the last ten steps and the cloud-length reset.

  It returns the exact `TileBatch` command stream (`SetLayer` plus sprite draws with texture
  `Images/Waterfall_N`, position, source rectangle, colours, origin and flip), the `AddLight` calls,
  the `TrySparkling` checks and the ambient waterfall and lavafall sound anchors.
* `batches` / `flushOrder` reproduce how `Terraria.Graphics.TileBatch` groups these draws:
  layer stacks, the texture switch to stack + 1, batch reuse by key (including its recent-batch
  cache), pass texture ids, and the `SortKey` order used by `FlushLayered`.

Inputs the caller supplies:
* the water style and `liquidAlpha` (the water-colour module);
* `Lighting.GetColor(x, y)` as a packed colour;
* `Main.DiscoR/G/B`, `Main.mouseTextColor`;
* `ShouldShowInvisibleBlocksAndWalls`.

Not modelled:
* the shimmer vertex colours (`Lighting.GetCornerColors` + `LiquidRenderer.SetShimmerVertexColors*`):
  shimmer sprites carry `Colors.shimmerBase` / `.shimmerSparkle`, and the caller colours them;
* the `Main.rand` draw and the dust of `TrySparkling`, which is reported as a `Sparkle` check only.
-/

namespace Lean4Terria.OriginalWaterfallDraw

open Lean4Terria.OriginalCollision (TileRules)

/-- The tile fields the waterfall code reads. `liquidType` is kept even when `liquid = 0`, as vanilla does. -/
structure Cell where
  active : Bool := false
  type : UInt16 := 0
  halfBrick : Bool := false
  slope : UInt8 := 0
  inActive : Bool := false
  liquid : UInt8 := 0
  /-- 0 water, 1 lava, 2 honey, 3 shimmer. -/
  liquidType : UInt8 := 0
  wall : UInt16 := 0
  invisibleWall : Bool := false
  deriving Inhabited, Repr, BEq

def Cell.ofTile (t : OriginalWorld.Tile) : Cell :=
  let (half, slope) := match t.shape with
    | .full => (false, 0) | .halfBlock => (true, 0) | .slope s => (false, s)
  let (amount, kind) : UInt8 × UInt8 := match t.liquid with
    | none => (0, 0)
    | some l => (l.amount, match l.kind with | .water => 0 | .lava => 1 | .honey => 2 | .shimmer => 3)
  { active := t.id.isSome, type := t.id.getD 0, halfBrick := half, slope := slope, inActive := t.actuated,
    liquid := amount, liquidType := kind, wall := t.wall, invisibleWall := t.invisibleWall }

def Cell.topSlope (c : Cell) : Bool := c.slope == 1 || c.slope == 2
def Cell.bottomSlope (c : Cell) : Bool := c.slope == 3 || c.slope == 4
def Cell.blockType (c : Cell) : Nat := if c.halfBrick then 1 else if c.slope > 0 then c.slope.toNat + 1 else 0
def Cell.nactive (c : Cell) : Bool := c.active && !c.inActive

/-- `TileID.Sets.BlocksWaterDrawingBehindSelf` (read from the running 1.4.5.8 game). -/
def blocksWaterDrawingBehindSelfDefault (t : UInt16) : Bool := t == 54 || t == 328 || t == 459 || t == 470 || t == 541

structure World where
  maxTilesX : Int
  maxTilesY : Int
  /-- `Main.tile[x, y]`; a missing tile reads as air (vanilla creates a `new Tile()`). -/
  cell : Int → Int → Cell
  /-- `Main.tileSolid` / `tileSolidTop` / `TileID.Sets.Platforms`. -/
  rules : UInt16 → TileRules
  blocksWaterDrawingBehindSelf : UInt16 → Bool := blocksWaterDrawingBehindSelfDefault

def World.ofGrid (g : OriginalCollision.Grid) : World :=
  { maxTilesX := g.width, maxTilesY := g.height,
    cell := fun x y => if x < 0 || y < 0 then default else Cell.ofTile (g.tileAt x.toNat y.toNat),
    rules := g.rules }

/-- `WorldGen.SolidTile(Tile)` under the given solidity table. -/
def solidTile (rules : UInt16 → TileRules) (c : Cell) : Bool :=
  let r := rules c.type
  c.nactive && r.solid && !r.solidTop && !c.halfBrick && c.slope == 0

/-- During `DrawWaterfall`, vanilla sets `Main.tileSolid[546] = false`. -/
def drawRules (rules : UInt16 → TileRules) (t : UInt16) : TileRules :=
  if t == 546 then { rules t with solid := false } else rules t

/-! ## Float32 helpers matching the CLR conversions -/

/-- `conv.r4` of an int. -/
def i2f (i : Int) : Float32 := if i ≥ 0 then Float32.ofNat i.toNat else -(Float32.ofNat (-i).toNat)
/-- `conv.i4` of a float (truncation toward zero; the values here are in range). -/
def trunc (f : Float32) : Int :=
  if f < 0 then -((-f).toUInt64.toNat : Int) else (f.toUInt64.toNat : Int)

/-! ## Colours (packed `R | G<<8 | B<<16 | A<<24`, as `Color.PackedValue`) -/

def white : UInt32 := 0xFFFFFFFF
def chan (c : UInt32) (k : Nat) : Nat := ((c >>> (8 * k).toUInt32) &&& 255).toNat
private def clampByte (v : Int) : UInt32 := if v < 0 then 0 else if v > 255 then 255 else v.toNat.toUInt32
/-- `new Color(int r, int g, int b, int a)` (each channel clamped to 0..255). -/
def rgba (r g b a : Int) : UInt32 := clampByte r ||| (clampByte g <<< 8) ||| (clampByte b <<< 16) ||| (clampByte a <<< 24)
/-- `Color * float`. -/
def colorMul (c : UInt32) (s : Float32) : UInt32 :=
  rgba (trunc (i2f (chan c 0) * s)) (trunc (i2f (chan c 1) * s)) (trunc (i2f (chan c 2) * s)) (trunc (i2f (chan c 3) * s))

/-! ## Sources and the animation clock -/

structure Source where
  x : Int
  y : Int
  /-- `WaterfallData.type`: 0 water (current style), 1 lava, 14 honey, 25 shimmer, 11 rain, 22 snow, 26 lava rain. -/
  type : Nat
  deriving Inhabited, Repr, BEq

/-- `UpdateFrame` counters. -/
structure Frames where
  wFallFrCounter : Int := 0
  regularFrame : Int := 0
  wFallFrCounter2 : Int := 0
  slowFrame : Int := 0
  rainFrameCounter : Int := 0
  rainFrameForeground : Int := 0
  rainFrameBackground : Int := 0
  lavaRainFrameCounter : Int := 0
  lavaRainFrameForeground : Int := 0
  lavaRainFrameBackground : Int := 0
  snowFrameCounter : Int := 0
  snowFrameForeground : Int := 0
  deriving Inhabited, Repr, BEq

/-- `WaterfallManager.UpdateFrame()`. -/
def Frames.update (f : Frames) : Frames := Id.run do
  let mut f := f
  f := { f with wFallFrCounter := f.wFallFrCounter + 1 }
  if f.wFallFrCounter > 2 then
    f := { f with wFallFrCounter := 0, regularFrame := f.regularFrame + 1 }
    if f.regularFrame > 15 then f := { f with regularFrame := 0 }
  f := { f with wFallFrCounter2 := f.wFallFrCounter2 + 1 }
  if f.wFallFrCounter2 > 6 then
    f := { f with wFallFrCounter2 := 0, slowFrame := f.slowFrame + 1 }
    if f.slowFrame > 15 then f := { f with slowFrame := 0 }
  f := { f with rainFrameCounter := f.rainFrameCounter + 1 }
  if f.rainFrameCounter > 0 then
    f := { f with rainFrameForeground := f.rainFrameForeground + 1 }
    if f.rainFrameForeground > 7 then f := { f with rainFrameForeground := f.rainFrameForeground - 8 }
    if f.rainFrameCounter > 2 then
      f := { f with rainFrameCounter := 0, rainFrameBackground := f.rainFrameBackground - 1 }
      if f.rainFrameBackground < 0 then f := { f with rainFrameBackground := 7 }
  f := { f with lavaRainFrameCounter := f.lavaRainFrameCounter + 1 }
  if f.lavaRainFrameCounter == 1 || f.lavaRainFrameCounter == 3 then
    f := { f with lavaRainFrameForeground := f.lavaRainFrameForeground + 1 }
    if f.lavaRainFrameForeground > 7 then f := { f with lavaRainFrameForeground := f.lavaRainFrameForeground - 8 }
  else if f.lavaRainFrameCounter > 3 then
    f := { f with lavaRainFrameCounter := 0, lavaRainFrameBackground := f.lavaRainFrameBackground - 1 }
    if f.lavaRainFrameBackground < 0 then f := { f with lavaRainFrameBackground := 7 }
  f := { f with snowFrameCounter := f.snowFrameCounter + 1 }
  if f.snowFrameCounter > 3 then
    f := { f with snowFrameCounter := 0, snowFrameForeground := f.snowFrameForeground + 1 }
    if f.snowFrameForeground > 7 then f := { f with snowFrameForeground := 0 }
  return f

structure Camera where
  /-- `Main.screenPosition`. -/
  screenX : Float32
  screenY : Float32
  /-- `Main.screenWidth` / `Main.screenHeight`. -/
  width : Int
  height : Int

/-- The `WaterfallManager` state. -/
structure Manager where
  /-- `maxWaterfallCount` (`Configuration "WaterfallDrawLimit"`, default 1000). -/
  maxWaterfallCount : Nat := 1000
  qualityMax : Int := 0
  /-- The first `currentMax` entries of `waterfalls`. -/
  sources : Array Source := #[]
  /-- `waterfalls[i].stopAtStep` for the current sources. -/
  stopAtStep : Array Int := #[]
  waterfallDist : Int := 100
  findWaterfallCount : Int := 0
  frames : Frames := {}
  deriving Inhabited

/-- `WaterfallManager.FindWaterfalls(forced)`. -/
def findWaterfalls (m : Manager) (w : World) (cam : Camera) (gfxQuality : Float32) (forced : Bool := false) : Manager := Id.run do
  let count := m.findWaterfallCount + 1
  if count < 30 && !forced then return { m with findWaterfallCount := count }
  let dist := trunc (75 * gfxQuality) + 25
  let qmax := trunc (i2f m.maxWaterfallCount * gfxQuality)
  let solid := solidTile w.rules
  let mut x0 := trunc (cam.screenX / 16 - 1) - dist
  let mut x1 := trunc ((cam.screenX + i2f cam.width) / 16) + 2 + dist
  let mut y0 := trunc (cam.screenY / 16 - 1) - dist
  let mut y1 := trunc ((cam.screenY + i2f cam.height) / 16) + 2 + 20
  if x0 ≤ 0 then x0 := 1
  if x1 ≥ w.maxTilesX then x1 := w.maxTilesX - 1
  if y0 ≤ 0 then y0 := 1
  if y1 ≥ w.maxTilesY then y1 := w.maxTilesY - 1
  let mut out : Array Source := #[]
  for i in [0:(x1 - x0).toNat] do
    let x := x0 + i
    for j in [0:(y1 - y0).toNat] do
      let y := y0 + j
      let t := w.cell x y
      if !t.active then continue
      if t.halfBrick then
        let above := w.cell x (y - 1)
        if above.liquid < 16 || solid above then
          let l := w.cell (x - 1) y
          let r := w.cell (x + 1) y
          let openL := l.liquid == 0 && !solid l && l.slope == 0
          let openR := r.liquid == 0 && !solid r && r.slope == 0
          if (l.liquid > 160 || r.liquid > 160) && (openL || openR) && (out.size : Int) < qmax then
            let has (k : UInt8) := above.liquidType == k || r.liquidType == k || l.liquidType == k
            let ty := if has 1 then 1 else if has 2 then 14 else if has 3 then 25 else 0
            out := out.push { x, y, type := ty }
      for (tile, ty) in [((196 : UInt16), 11), (460, 22), (717, 26)] do
        if t.type == tile then
          let b := w.cell x (y + 1)
          if !solid b && b.liquid == 0 && b.slope == 0 && (out.size : Int) < qmax then
            out := out.push { x, y := y + 1, type := ty }
  return { m with findWaterfallCount := 0, waterfallDist := dist, qualityMax := qmax, sources := out,
                  stopAtStep := (Array.range out.size).map fun i => m.stopAtStep[i]?.getD 0 }

/-- `WaterfallManager.CheckForWaterfall(x, y)`. -/
def checkForWaterfall (m : Manager) (x y : Int) : Bool := m.sources.any fun s => s.x == x && s.y == y

/-! ## Draw output -/

inductive Colors where
  /-- The same colour on all four corners (`VertexColors` from one `Color`). -/
  | uniform (c : UInt32)
  /-- Shimmer: `Lighting.GetCornerColors(x, y)` then `LiquidRenderer.SetShimmerVertexColors(opacity, x, y)`. -/
  | shimmerBase
  /-- Shimmer second pass (`Y + 42`): `SetShimmerVertexColors_Sparkle(opacity, x, y, true)` on the same corners. -/
  | shimmerSparkle
  deriving Inhabited, Repr, BEq

/-- One `TileBatch.Draw(texture, position, source, colors, origin, 1f, effects)`. -/
structure Sprite where
  /-- `Images/Waterfall_N`. -/
  texture : Nat
  x : Float32
  y : Float32
  srcX : Int
  srcY : Int
  srcW : Int
  srcH : Int
  colors : Colors
  originX : Float32 := 0
  originY : Float32 := 0
  /-- `SpriteEffects`: 0 none, 1 flip horizontally. -/
  effects : Nat := 0
  /-- Tile and opacity passed along (shimmer colouring). -/
  tileX : Int := 0
  tileY : Int := 0
  opacity : Float32 := 0
  /-- IL offset of the vanilla draw call in `DrawWaterfall(int, float)` (diagnostics and coverage). -/
  site : Nat := 0
  deriving Inhabited, Repr

inductive Cmd where
  /-- `TileBatch.SetLayer(layer, sublayer)`. -/
  | setLayer (layer : UInt32) (sub : UInt8)
  | draw (s : Sprite)
  deriving Inhabited, Repr

/-- `Lighting.AddLight(x, y, r, g, b)`. -/
structure Light where
  x : Int
  y : Int
  r : Float32
  g : Float32
  b : Float32
  deriving Inhabited, Repr

/-- A `TrySparkling(x, y, direction, color)` call. When a channel of `color` is above 20, vanilla draws
`Main.rand.Next(20000)` and spawns dust 43 if the result is below `max channel / 30`. -/
structure Sparkle where
  x : Int
  y : Int
  direction : Int
  color : UInt32
  deriving Inhabited, Repr

/-- `Main.ambientWaterfall*` / `ambientLavafall*` written at the end of `DrawWaterfall`. -/
structure Ambient where
  waterfallX : Float32 := -1
  waterfallY : Float32 := -1
  waterfallStrength : Float32 := 0
  lavafallX : Float32 := -1
  lavafallY : Float32 := -1
  lavafallStrength : Float32 := 0
  deriving Inhabited, Repr

structure Output where
  cmds : Array Cmd := #[]
  lights : Array Light := #[]
  sparkles : Array Sparkle := #[]
  /-- From the last `DrawWaterfall` call; `none` if `Draw` made no call. -/
  ambient : Option Ambient := none
  deriving Inhabited

structure DrawEnv where
  world : World
  camera : Camera
  gfxQuality : Float32
  /-- `Main.worldSurface`. -/
  worldSurface : Float
  /-- `Lighting.GetColor(x, y)` as a packed colour (alpha 255). -/
  light : Int → Int → UInt32
  /-- `Main.ShouldShowInvisibleBlocksAndWalls()`. -/
  showInvisible : Bool := false
  discoR : Int := 255
  discoG : Int := 0
  discoB : Int := 0
  mouseTextColor : Int := 0
  /-- `Main.drewLava` (never set by `WaterfallManager`; `Draw` clears it). -/
  drewLava : Bool := false
  /-- `Lighting.AddLight` returns early when the game is paused or on a server. -/
  lightsEnabled : Bool := true

/-- `WaterfallManager.AddLight(type, x, y)`. -/
def addLight (env : DrawEnv) (type : Nat) (x y : Int) : Option Light :=
  if !env.lightsEnabled then none else
  let m := i2f (270 - env.mouseTextColor)
  let mk (r g b : Float32) := some { x, y, r, g, b : Light }
  match type with
  | 1 => let r := ((0.55 : Float32) + m / 900) * 0.4; mk r (r * 0.3) (r * 0.1)
  | 2 => mk (i2f env.discoR / 255 * 0.2) (i2f env.discoG / 255 * 0.2) (i2f env.discoB / 255 * 0.2)
  | 15 => mk 0 0 0.2 | 16 => mk 0 0.2 0 | 17 => mk 0 0 0.2 | 18 => mk 0 0.2 0
  | 19 => mk 0.2 0 0 | 20 => mk 0.2 0.2 0.2 | 21 => mk 0.2 0 0
  | 25 => let a := (0.7 : Float32) + m / 900; let b := (0.7 : Float32) + m / 125; mk (a * 0.6) (b * 0.25) (a * 0.9)
  | 26 => let r := ((0.55 : Float32) + m / 900) * 0.6; mk r (r * 0.3) (r * 0.1)
  | _ => none

/-- `WaterfallManager.GetAlpha`. -/
def getAlpha (env : DrawEnv) (alpha : Float32) (maxSteps : Int) (type : Nat) (y : Int) (s : Int) (tile : Cell) : Float32 :=
  let a : Float32 :=
    if type == 1 then 1 else if type == 14 then 0.8 else if type == 25 then 0.75
    else
      let noWall := tile.wall == 0 || (!env.showInvisible && (tile.wall == 318 || tile.invisibleWall))
      if noWall && (Float.ofInt y < env.worldSurface) then alpha else 0.6 * alpha
  if s > maxSteps - 10 then a * (i2f (maxSteps - s) / 10) else a

/-- `WaterfallManager.StylizeColor`. -/
def stylizeColor (env : DrawEnv) (a : Float32) (type : Nat) (color : UInt32) : UInt32 := Id.run do
  let mut r := i2f (chan color 0) * a
  let mut g := i2f (chan color 1) * a
  let mut b := i2f (chan color 2) * a
  let al := i2f (chan color 3) * a
  if type == 1 then
    if r < 190 * a then r := 190 * a
    if g < 190 * a then g := 190 * a
    if b < 190 * a then b := 190 * a
  else if type == 2 then
    r := i2f env.discoR * a; g := i2f env.discoG * a; b := i2f env.discoB * a
  else if 15 ≤ type && type ≤ 21 then
    r := 255 * a; g := 255 * a; b := 255 * a
  return rgba (trunc r) (trunc g) (trunc b) (trunc al)

/-- `WorldGen.InWorld(x, y, 0)`. -/
def inWorld (w : World) (x y : Int) : Bool := x ≥ 0 && x < w.maxTilesX && y ≥ 0 && y < w.maxTilesY

def Layer_Rain : UInt32 := 0
def Layer_Waterfall : UInt32 := 1

private structure St where
  out : Output := {}
  stop : Array Int
  amb : Ambient := {}
  /-- locals 1, 2, 6, 7 (closest distances) -/
  wd : Float32 := 99999
  wdY : Float32 := 99999
  ld : Float32 := 99999
  ldY : Float32 := 99999
  wx : Int := -1
  wy : Int := -1
  lx : Int := -1
  ly : Int := -1

private def St.cmd (s : St) (c : Cmd) : St := { s with out := { s.out with cmds := s.out.cmds.push c } }
private def St.light (s : St) : Option Light → St
  | none => s
  | some l => { s with out := { s.out with lights := s.out.lights.push l } }

/-- `DrawWaterfall(int Style, int x, int y, float opacity, Vector2 position, Rectangle sourceRect, Color color,
SpriteEffects effects)`: one sprite (two for shimmer). -/
private def sprites (env : DrawEnv) (site : Nat) (tex : Nat) (x y : Int) (opacity : Float32) (px py : Int)
    (sx sy sw sh : Int) (color : UInt32) (eff : Nat) : Array Cmd :=
  let fx := i2f px - env.camera.screenX
  let fy := i2f py - env.camera.screenY
  let base : Sprite := { texture := tex, x := fx + 0, y := fy + 0, srcX := sx, srcY := sy, srcW := sw, srcH := sh,
                         colors := .uniform color, effects := eff, tileX := x, tileY := y, opacity, site }
  if tex == 25 then
    #[.draw { base with colors := .shimmerBase }, .draw { base with srcY := sy + 42, colors := .shimmerSparkle }]
  else #[.draw base]

/-- Ambient-sound anchor update of one path step (`IL_093d` lava, `IL_0a39` water). -/
private def ambientStep (env : DrawEnv) (s : St) (lava : Bool) (x y : Int) : St := Id.run do
  let cam := env.camera
  let n := Float32.abs (i2f (x * 16 + 8) - (cam.screenX + i2f (cam.width / 2)))
  let n2 := Float32.abs (i2f (y * 16 + 8) - (cam.screenY + i2f (cam.height / 2)))
  let mut s := s
  let mut add : Float32 := 0
  let mut hasAdd := false
  if n < i2f (cam.width * 2) && n2 < i2f (cam.height * 2) then
    let d := (Float.sqrt (n * n + n2 * n2).toFloat).toFloat32
    let f := 1 - d / (i2f cam.width * 0.75)
    if f > 0 then add := f; hasAdd := true
  if lava then
    if hasAdd then s := { s with amb := { s.amb with lavafallStrength := s.amb.lavafallStrength + add } }
    if n < s.ld then s := { s with ld := n, lx := x * 16 + 8 }
    -- vanilla compares the vertical distance but stores the horizontal one (`IL_09f2`)
    if n2 < s.ldY then s := { s with ldY := n, ly := y * 16 + 8 }
  else
    if hasAdd then s := { s with amb := { s.amb with waterfallStrength := s.amb.waterfallStrength + add } }
    if n < s.wd then s := { s with wd := n, wx := x * 16 + 8 }
    if n2 < s.wdY then s := { s with wdY := n, wy := y * 16 + 8 }
  return s

/-- Rain, snow and lava-rain clouds (types 11, 22, 26), `IL_00ee`–`IL_05a1`. -/
private def drawCloud (env : DrawEnv) (m : Manager) (s : St) (i : Nat) (src : Source) : St := Id.run do
  let w := env.world
  let solid := solidTile (drawRules w.rules)
  let mut s := s
  if env.drewLava then return s
  let type := src.type
  let num := if type == 22 then m.waterfallDist / 2 else m.waterfallDist / 4
  if s.stop[i]! > num then s := { s with stop := s.stop.set! i num }
  if s.stop[i]! == 0 then return s
  let x := src.x
  let mut y := src.y
  let cam := env.camera
  if i2f (y + num) < cam.screenY / 16 then return s
  if i2f x < cam.screenX / 16 - 20 then return s
  if i2f x > (cam.screenX + i2f cam.width) / 16 + 20 then return s
  let fr := m.frames
  let wrap (v : Int) := if v > 7 then v - 8 else v
  let (fg, bg) :=
    if x % 2 == 0 then
      if type == 22 then (wrap (fr.snowFrameForeground + 3), 0)
      else if type == 26 then (wrap (fr.lavaRainFrameForeground + 3), wrap (fr.lavaRainFrameBackground + 2))
      else (wrap (fr.rainFrameForeground + 3), wrap (fr.rainFrameBackground + 2))
    else
      if type == 22 then (fr.snowFrameForeground, 0)
      else if type == 26 then (fr.lavaRainFrameForeground, fr.lavaRainFrameBackground)
      else (fr.rainFrameForeground, fr.rainFrameBackground)
  let mut bgH : Int := 16
  let mut fgH : Int := 16
  let mut px : Float32 := i2f (if y % 2 == 0 then x * 16 + 9 else x * 16 + 8) - cam.screenX
  let mut py : Float32 := i2f (y * 16 + 8) - cam.screenY
  if !inWorld w x (y - 1) then return s
  let above := w.cell x (y - 1)
  if above.active && above.bottomSlope then py := py - 16
  let mut stopped := false
  for k in [0:num.toNat] do
    let k : Int := k
    s := s.cmd (.setLayer Layer_Rain 0)
    let mut c := env.light x y
    let mut a1 : Float32 := 0.6
    let mut a2 : Float32 := 0.3
    if type == 26 then
      c := rgba 255 255 255 127
      s := s.light (addLight env type x y)
      a1 := 0.9; a2 := 0.4
    if k > num - 8 then
      let f := i2f (num - k) / 8
      a1 := a1 * f; a2 := a2 * f
    let fgc := colorMul c a1
    let bgc := colorMul c a2
    let spr (site tex : Nat) (sx sh : Int) (col : UInt32) : Cmd :=
      .draw { texture := tex, x := px, y := py, srcX := sx, srcY := 0, srcW := 16, srcH := sh, colors := .uniform col,
              originX := 8, originY := 8, tileX := x, tileY := y, site }
    if type == 22 then s := s.cmd (spr 0x040d 22 (fg * 18) fgH fgc)
    else if type == 26 then s := (s.cmd (spr 0x0443 27 (bg * 18) bgH bgc)).cmd (spr 0x046e 26 (fg * 18) fgH fgc)
    else s := (s.cmd (spr 0x049b 12 (bg * 18) bgH bgc)).cmd (spr 0x04c6 11 (fg * 18) fgH fgc)
    if stopped then break
    y := y + 1
    if y ≥ w.maxTilesY then break
    let t := w.cell x y
    if solid t then stopped := true
    if t.liquid > 0 then
      let cut : Int := ((trunc (16 * (i2f t.liquid.toNat / 255))).toNat &&& 254 : Nat)
      if cut ≥ 15 then break
      fgH := fgH - cut; bgH := bgH - cut
    if y % 2 == 0 then px := px + 1 else px := px - 1
    py := py + 16
  return { s with stop := s.stop.set! i 0 }

/-- Water, lava, honey and shimmer paths, `IL_05a6`–`IL_1549`. -/
private def drawPath (env : DrawEnv) (m : Manager) (s : St) (i : Nat) (src : Source) (style : Nat) (alphaArg : Float32) :
    St := Id.run do
  let w := env.world
  let dr := drawRules w.rules
  let solid := solidTile dr
  let mut s := s
  let mut type := src.type
  let mut frame : Int := 0
  if type == 1 || type == 14 || type == 25 then
    if env.drewLava || s.stop[i]! == 0 then return s
    frame := 32 * m.frames.slowFrame
  else
    if type == 0 then type := style
    else if type == 2 && env.drewLava then return s
    frame := 32 * m.frames.regularFrame
  let mut x := src.x
  let mut y := src.y
  let mut num12 : Int := 0       -- loc12: vertical offset of the run (0 / 8)
  let mut prevDx : Int := 0      -- loc16
  let mut prevDy : Int := 0      -- loc17
  let mut dir : Int := 0         -- loc18
  let mut prevDir : Int := 0     -- loc19
  let mut prevSlope : Int := 0   -- loc20
  let mut prevType : Nat := 0    -- loc21
  let mut turns : Int := 0       -- loc23
  let mut num : Int := m.waterfallDist   -- loc11
  let mut prevColor : UInt32 := white     -- loc24
  let mut k : Int := 0
  -- the loop runs at most `waterfallDist` steps (the cloud reset can only shorten it to a bounded value)
  for _ in [0:(max num (trunc (40 * (i2f w.maxTilesX / 4200) * env.gfxQuality)) + 2).toNat] do
    if !(k < num && turns < 2) then break
    s := s.light (addLight env type x y)
    let tile := w.cell x y
    let r := dr tile.type
    if tile.nactive && r.solid && !r.solidTop && !r.platform && tile.blockType == 0 then break
    let lt := w.cell (x - 1) y
    let bt := w.cell x (y + 1)
    let rt := w.cell (x + 1) y
    if solid bt && !tile.halfBrick then num12 := 8
    else if prevDy != 0 then num12 := 0
    let mut slopeDir : Int := 0                 -- loc47
    let dirBefore := prevDir                    -- loc48
    let mut dx : Int := 0                       -- loc49
    let mut dy : Int := 0                       -- loc50
    let mut bSlope := false                     -- loc51
    if bt.topSlope && !tile.halfBrick && bt.type != 19 then
      bSlope := true
      if bt.slope == 1 then slopeDir := 1; dx := 1; dir := 1; prevDir := 1
      else slopeDir := -1; dx := -1; dir := -1; prevDir := -1
      dy := 1
    else if !tile.halfBrick && ((!solid bt && !bt.bottomSlope) || !bt.active) then
      turns := 0; dy := 1; dx := 0
    else if (solid lt || lt.topSlope || lt.liquid > 0) && !solid rt && rt.liquid == 0 then
      if dir == -1 then turns := turns + 1
      dx := 1; dy := 0; dir := 1
    else if (solid rt || rt.topSlope || rt.liquid > 0) && !solid lt && lt.liquid == 0 then
      if dir == 1 then turns := turns + 1
      dx := -1; dy := 0; dir := -1
    else if ((!solid rt && !tile.topSlope) || rt.liquid == 0) && !solid lt && !tile.topSlope && lt.liquid == 0 then
      dy := 0; dx := dir
    else
      turns := turns + 1; dy := 0; dx := 0
    if turns ≥ 2 then dir := -dir; dx := -dx
    let mut tt : Int := -1
    if type != 1 && type != 14 && type != 25 then
      if bt.active then tt := bt.type.toNat
      if tile.active then tt := tile.type.toNat
    if tt != -1 then
      if tt == 160 then type := 2
      else if 262 ≤ tt && tt ≤ 268 then type := (15 + tt - 262).toNat
    let lightC := env.light x y
    if k > 50 then
      s := { s with out := { s.out with sparkles := s.out.sparkles.push { x, y, direction := dir, color := lightC } } }
    let alpha := getAlpha env alphaArg num type y k tile
    let mut c := stylizeColor env alpha type lightC
    if type == 1 then s := ambientStep env s true x y
    else if !(type == 14 || type == 25 || type == 11 || type == 12 || type == 22) then s := ambientStep env s false x y
    let liq16 : Int := tile.liquid.toNat / 16
    s := s.cmd (.setLayer Layer_Waterfall 0)
    let dw (site : Nat) (s : St) (tex : Nat) (px py sx sy sw sh : Int) (col : UInt32) (eff : Nat) : St :=
      { s with out := { s.out with cmds := s.out.cmds ++ sprites env site tex x y alpha px py sx sy sw sh col eff } }
    if bSlope && dir != dirBefore then
      if dirBefore == 1 then s := dw 2920 s type (x * 16 - 16) (y * 16 + 16 - 2) frame 24 32 (16 - liq16 - 2) c 1
      else s := dw 2991 s type (x * 16) (y * 16 + 16 - 2) frame 24 32 (16 - liq16 - 2) c 0
    if prevDx == 0 && slopeDir != 0 && prevDy == 1 && dir != prevDir then
      slopeDir := 0; dir := prevDir; c := white
      s := dw 3107 s type (x * 16 - 16) (y * 16 + 16) frame 24 32 (16 - liq16) white 1
    if prevSlope != 0 && dx == 0 && dy == 1 then
      if dir == 1 then
        -- `IL_0c90`: vanilla draws the current type here, only the colour is last step's
        if prevType != type then s := dw 3277 s type (x * 16) (y * 16 + num12 + 8) frame 0 16 (16 - liq16 - 8) prevColor 1
        else s := dw 3348 s type (x * 16) (y * 16 + num12 + 8) frame 0 16 (16 - liq16 - 8) c 1
      else s := dw 3416 s type (x * 16) (y * 16 + num12 + 8) frame 0 16 (16 - liq16 - 8) c 0
    if num12 == 8 && prevDy == 1 && prevSlope == 0 then
      if prevDir == -1 then
        if prevType != type then s := dw 3506 s prevType (x * 16) (y * 16) frame 24 32 8 prevColor 0
        else s := dw 3567 s type (x * 16) (y * 16) frame 24 32 8 c 0
      else
        if prevType != type then s := dw 3634 s prevType (x * 16 - 16) (y * 16) frame 24 32 8 prevColor 1
        else s := dw 3695 s type (x * 16 - 16) (y * 16) frame 24 32 8 c 1
    if slopeDir != 0 && prevDx == 0 then
      if dirBefore == 1 then
        if prevType != type then s := dw 3786 s prevType (x * 16 - 16) (y * 16) frame 24 32 (16 - liq16) prevColor 1
        else s := dw 3854 s type (x * 16 - 16) (y * 16) frame 24 32 (16 - liq16) c 1
      else
        if prevType != type then s := dw 3925 s prevType (x * 16) (y * 16) frame 24 32 (16 - liq16) prevColor 0
        else s := dw 3987 s type (x * 16) (y * 16) frame 24 32 (16 - liq16) c 0
    let submerged := tile.liquid > 0 && !tile.halfBrick
    if dy == 1 && slopeDir == 0 && prevSlope == 0 then
      if dir == -1 then
        if prevDy == 0 then s := dw 4083 s type (x * 16) (y * 16 + num12) frame 0 16 (16 - liq16) c 0
        else if prevType != type then s := dw 4154 s prevType (x * 16) (y * 16) frame 24 32 (16 - liq16) prevColor 0
        else s := dw 4219 s type (x * 16) (y * 16) frame 24 32 (16 - liq16) c 0
      else
        if prevDy == 0 then s := dw 4290 s type (x * 16) (y * 16 + num12) frame 0 16 (16 - liq16) c 1
        else if prevType != type then s := dw 4364 s prevType (x * 16 - 16) (y * 16) frame 24 32 (16 - liq16) prevColor 1
        else s := dw 4432 s type (x * 16 - 16) (y * 16) frame 24 32 (16 - liq16) c 1
    else if dx == 1 then
      if !submerged then
        if slopeDir == 1 then
          for j in [0:8] do
            let j : Int := j
            let a := j * 2
            let b := 14 - j * 2
            let mut cc := a
            num12 := 8
            if prevDx == 0 && j < 2 then cc := 4
            s := dw 4611 s type (x * 16 + a) (y * 16 + num12 + cc) (16 + frame + b) 0 2 (16 - num12) c 1
        else
          let h : Int := if w.blocksWaterDrawingBehindSelf tile.type then 8
            else if w.blocksWaterDrawingBehindSelf (w.cell x (y + 1)).type then 8 else 16
          s := dw 4757 s type (x * 16) (y * 16 + num12) (16 + frame) 0 16 h c 1
    else if dx == -1 then
      if !submerged then
        if slopeDir == -1 then
          for j in [0:8] do
            let j : Int := j
            let a := j * 2
            let b := j * 2
            let mut cc := 14 - j * 2
            num12 := 8
            if prevDx == 0 && j > 5 then cc := 4
            s := dw 4938 s type (x * 16 + a) (y * 16 + num12 + cc) (16 + frame + b) 0 2 (16 - num12) c 1
        else
          let h : Int := if w.blocksWaterDrawingBehindSelf tile.type then 8
            else if w.blocksWaterDrawingBehindSelf (w.cell x (y + 1)).type then 8 else 16
          s := dw 5084 s type (x * 16) (y * 16 + num12) (16 + frame) 0 16 h c 0
    else if dx == 0 && dy == 0 then
      if !submerged then s := dw 5199 s type (x * 16) (y * 16 + num12) (16 + frame) 0 16 16 c 0
      k := 1000
    if submerged then k := 1000
    prevDy := dy; prevDir := dir; prevDx := dx
    x := x + dx; y := y + dy
    prevSlope := slopeDir; prevColor := c
    if prevType != type then prevType := type
    let cloud (t : Cell) := t.active && (t.type == 189 || t.type == 196)
    if cloud lt || cloud rt || cloud bt then
      num := trunc (40 * (i2f w.maxTilesX / 4200) * env.gfxQuality)
    if !inWorld w x y then break
    k := k + 1
  return s

/-- `WaterfallManager.DrawWaterfall(int Style, float Alpha)` on the given `stopAtStep` values. -/
def drawWaterfall (m : Manager) (env : DrawEnv) (stop : Array Int) (style : Nat) (alpha : Float32) (out : Output := {}) :
    Array Int × Output := Id.run do
  let mut s : St := { out, stop }
  for h : i in [0:m.sources.size] do
    let src := m.sources[i]
    if src.type == 11 || src.type == 22 || src.type == 26 then s := drawCloud env m s i src
    else s := drawPath env m s i src style alpha
  let amb : Ambient := { s.amb with waterfallX := i2f s.wx, waterfallY := i2f s.wy, lavafallX := i2f s.lx, lavafallY := i2f s.ly }
  return (s.stop, { s.out with ambient := some amb })

/-- `Main.liquidAlpha` index → waterfall style, in the order of `WaterfallManager.Draw`. -/
def drawStyles : Array (Nat × Nat) :=
  #[(0, 0), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7), (7, 8), (8, 9), (9, 10), (10, 13), (12, 23), (13, 24)]

/-- `WaterfallManager.Draw()`. Returns the manager (with the final `stopAtStep`) and everything drawn. -/
def draw (m : Manager) (env : DrawEnv) (liquidAlpha : Array Float32) : Manager × Output := Id.run do
  let mut stop := m.sources.map fun _ => m.waterfallDist
  let mut out : Output := {}
  let env := { env with drewLava := false }
  for (idx, style) in drawStyles do
    let a := liquidAlpha[idx]?.getD 0
    if a > 0 then
      let (st, o) := drawWaterfall m env stop style a out
      stop := st; out := o
  return ({ m with stopAtStep := stop }, out)

/-! ## `Terraria.Graphics.TileBatch` grouping -/

structure Batch where
  layerStack : UInt32
  /-- Pass texture id (`_textureIdLookup`, in order of first use). -/
  textureId : Nat
  texture : Nat
  sprites : Array Sprite := #[]
  deriving Inhabited

private structure TB where
  batches : Array Batch := #[]
  curTex : Option Nat := none
  curStack : UInt32 := 0
  curIdx : Nat := 0
  next : Option UInt32 := none
  layered : Bool := false
  lookup : Array ((UInt32 × Nat) × Nat) := #[]
  cache : Array (Option Nat × Nat) := Array.replicate 2048 (none, 0)
  texIds : Array Nat := #[]
  unlayered : Array Sprite := #[]

private def cacheIndex (stack : UInt32) : Nat := ((stack >>> 14) ||| (stack &&& 65535)).toNat

private def TB.switch (t : TB) (tex : Nat) (stack : UInt32) : TB := Id.run do
  let oldTex := t.curTex; let oldStack := t.curStack; let oldIdx := t.curIdx
  let mut t := { t with curTex := some tex, curStack := stack }
  let ci := cacheIndex stack
  if ci < 2048 && t.cache[ci]!.1 == some tex then
    t := { t with curIdx := t.cache[ci]!.2 }
  else
    match t.lookup.find? (·.1 == (stack, tex)) with
    | some (_, idx) => t := { t with curIdx := idx }
    | none =>
      let tid := (t.texIds.idxOf? tex).getD t.texIds.size
      let texIds := if tid == t.texIds.size then t.texIds.push tex else t.texIds
      let idx := t.batches.size
      t := { t with texIds, curIdx := idx, batches := t.batches.push { layerStack := stack, textureId := tid, texture := tex },
                    lookup := t.lookup.push ((stack, tex), idx) }
  let oi := cacheIndex oldStack
  if oi < 2048 then t := { t with cache := t.cache.set! oi (oldTex, oldIdx) }
  return t

/-- The batches `TileBatch` builds from a command stream (creation order), and the sprites drawn before the first
`SetLayer` (unlayered mode; empty for waterfalls). -/
def batchesOf (cmds : Array Cmd) : Array Batch × Array Sprite := Id.run do
  let mut t : TB := {}
  for c in cmds do
    match c with
    | .setLayer l sub => t := { t with layered := true, next := some ((l <<< 8) ||| sub.toUInt32) }
    | .draw sp =>
      if !t.layered then t := { t with unlayered := t.unlayered.push sp }; continue
      match t.next with
      | some st => if t.curTex != some sp.texture || st != t.curStack then t := t.switch sp.texture st
      | none => if t.curTex != some sp.texture then t := t.switch sp.texture (t.curStack + 1)
      t := { t with next := none }
      t := { t with batches := t.batches.modify t.curIdx fun b => { b with sprites := b.sprites.push sp } }
  return (t.batches, t.unlayered)

def batches (cmds : Array Cmd) : Array Batch := (batchesOf cmds).1

/-- GPU submission order of `TileBatch.FlushLayered`: batches sorted by `LayerStack << 16 | textureId`. -/
def flushOrder (bs : Array Batch) : Array Batch :=
  bs.qsort fun a b => (a.layerStack.toUInt64 <<< 16 ||| a.textureId.toUInt64) < (b.layerStack.toUInt64 <<< 16 ||| b.textureId.toUInt64)

end Lean4Terria.OriginalWaterfallDraw
