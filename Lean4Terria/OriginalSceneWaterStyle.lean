import Lean4Terria.OriginalWorld
import Lean4Terria.OriginalCandleBuffs

/-!
# Local player water style (Terraria 1.4.5.8)

Handwritten from `TerrariaServer.exe` 1.4.5.8 (the desktop client runs the same code). This is
where `Main.waterStyle`, the style that `OriginalLiquidRender` draws water with, comes from.
Each frame `Main.DoDraw` runs three steps in this order:

1. **Lighting.** `Lighting.LightTiles` reaches `Main.UpdateSceneMetrics`, which calls
   `SceneMetrics.Scan` with the local player's `Center` and the lit area
   (`GetAreaToLight` grown by 28 tiles). Here that step is `scan`:
   * `ScanTiles` counts the tile types in the 169 × 124 zone box around `TileCenter`;
   * `ScanOnScreenTiles` finds the active water fountain;
   * `AggregateTileCounts` sums the biome counts, and holy, evil and blood cancel each other;
   * `CalculateZones` compares the sums with the thresholds.
2. **Background.** `Main.DrawBG` calls `GetPreferredBGStyleForPlayer` and moves `Main.bgStyle`
   towards it through `bgDelay` and `DrawBG_HandleBackgroundTransition`. Here: `preferredBGStyle`
   and `drawBG`.
3. **Water.** `Main.DrawWaters(false)` sets `Main.waterStyle = CalculateWaterStyle(false)`. Here:
   `calculateWaterStyle`.

`frame` chains the three steps. The other definitions are the single vanilla methods.

Checked against the running game: 128 captured scenarios, 1038 frames and 43 596 fields, all
equal (workspace `.lake/original-scene-water-style/1.4.5.8`, `run.sh`). The checked fields are
every value of `Metrics`, `areaToLight`, `preferredBGStyle`, the three `BGState` fields and both
`calculateWaterStyle` results, frame by frame. Transcribed from the IL but not compared: the
adapters `SceneTile.ofTile` and `World.ofTerrain`, and `View.ofScreen` at zoom other than 1.

Constants read from the running game:
* `TileID.Count` = 754;
* `TileID.Sets.isDesertBiomeSand` = {53, 396, 397, 400, 401, 403};
* `SurfaceBackgroundID.Sets.IsForest` = {0, 10, 11, 12};
* `WorldGen.beachDistance` = 380.

Not modelled (the caller must keep these off):
* the main menu `DrawBG` branch (`Main.gameMenu`);
* the separate camera scene metrics used for spectating, drones and golf
  (`_usingSeparateCameraSceneMetrics`);
* the dual dungeons seed zone override (`Main.dualDungeonsSeed`);
* `CaptureManager.IsCapturing`, which skips the scan.
-/

namespace Lean4Terria.OriginalSceneWaterStyle

open OriginalWorld (Tile LiquidKind)

/-! ## Inputs -/

/-- The part of `Main.tile[x, y]` that the scan reads. -/
structure SceneTile where
  active : Bool := false
  type : Nat := 0
  liquid : Nat := 0
  liquidType : Nat := 0
  frameX : Int := 0
  frameY : Int := 0
  deriving BEq, Inhabited, Repr

/-- Adapter from the shared world tile. A tile without a frame reads frame (0, 0), which only
matters for the water fountain (207); that tile always carries a frame. -/
def SceneTile.ofTile (t : Tile) : SceneTile :=
  let (fx, fy) := t.frame.getD (0, 0)
  let (amount, kind) : Nat × Nat := match t.liquid with
    | some l => (l.amount.toNat, match l.kind with
        | .water => 0 | .lava => 1 | .honey => 2 | .shimmer => 3)
    | none => (0, 0)
  { active := t.id.isSome, type := (t.id.getD 0).toNat, liquid := amount, liquidType := kind,
    frameX := fx, frameY := fy }

/-- The world statics the chain reads. `tileAt` is `Main.tile[x, y]`; `none` is a null tile. -/
structure World where
  maxTilesX : Int
  maxTilesY : Int
  worldSurface : Float
  rockLayer : Float
  tileAt : Int → Int → Option SceneTile
  remixWorld : Bool := false
  infectedSeed : Bool := false
  /-- `Skyblock.lowTiles`: the snow and desert thresholds drop to 300. -/
  skyblockLowTiles : Bool := false

/-- The world background choices (`WorldGen.treeBG1` … `mushroomBG`) that
`WorldGen.GetCurrentBackgroundOfStyle` reads. -/
structure Backgrounds where
  treeBG1 : Int := 0
  treeBG2 : Int := 0
  treeBG3 : Int := 0
  treeBG4 : Int := 0
  corruptBG : Int := 0
  crimsonBG : Int := 0
  hallowBG : Int := 0
  desertBG : Int := 0
  jungleBG : Int := 0
  oceanBG : Int := 0
  snowBG : Int := 0
  mushroomBG : Int := 0
  deriving BEq, Inhabited, Repr

/-- `Microsoft.Xna.Framework.Rectangle` as (X, Y, Width, Height). -/
structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving BEq, Inhabited, Repr

def Rect.right (r : Rect) : Int := r.x + r.w
def Rect.bottom (r : Rect) : Int := r.y + r.h
/-- `Rectangle.Inflate(dx, dy)`. -/
def Rect.inflate (r : Rect) (dx dy : Int) : Rect := ⟨r.x - dx, r.y - dy, r.w + 2 * dx, r.h + 2 * dy⟩

/-! ## Constants (`SceneMetrics..cctor`, `WorldGen..cctor`) -/

/-- `ZoneScanSize = (1920 / 16 + 25·2 − 1, 1200 / 16 + 25·2 − 1)`. -/
def zoneScanSize : Int × Int := (1920 / 16 + 25 * 2 - 1, 1200 / 16 + 25 * 2 - 1)
def beachDistance : Int := 380
def corruptionTileThreshold : Int := 300
def crimsonTileThreshold : Int := 300
def hallowTileThreshold : Int := 125
def jungleTileThreshold : Int := 140
def snowTileThreshold (lowTiles : Bool) : Int := if lowTiles then 300 else 1500
def desertTileThreshold (lowTiles : Bool) : Int := if lowTiles then 300 else 1500
def mushroomTileThreshold : Int := 100
/-- `TileID.Count`: the length of `SceneMetrics._tileCounts`. -/
def tileIdCount : Nat := 754
/-- `TileID.Sets.isDesertBiomeSand`. -/
def isDesertBiomeSand (type : Nat) : Bool :=
  type == 53 || type == 396 || type == 397 || type == 400 || type == 401 || type == 403
/-- `SurfaceBackgroundID.Sets.IsForest`. -/
def isForest (style : Int) : Bool := style == 0 || style == 10 || style == 11 || style == 12
/-- `Main.UnderworldLayer`. -/
def underworldLayer (w : World) : Int := w.maxTilesY - 200

/-! ## Geometry helpers -/

/-- `conv.i4` of a float in range: truncation towards zero. -/
private def convI4 (f : Float32) : Int := f.toInt32.toInt
private def convI4d (f : Float) : Int := f.toInt32.toInt

/-- `Utils.Clamp<int>(value, min, max)`. -/
def clampInt (v lo hi : Int) : Int := if v > hi then hi else if v < lo then lo else v

/-- `Utils.ToTileCoordinates(Vector2)`: `((int)X >> 4, (int)Y >> 4)`. -/
def toTileCoordinates (p : Float32 × Float32) : Int × Int :=
  (Int.fdiv (convI4 p.1) 16, Int.fdiv (convI4 p.2) 16)

/-- `Utils.ClampedInWorld(point, 0)`. The original clamps Y against `maxTilesX` too. -/
def clampedInWorld (w : World) (p : Int × Int) : Int × Int :=
  (clampInt p.1 0 (w.maxTilesX - 1), clampInt p.2 0 (w.maxTilesX - 1))

/-- `Utils.CenteredRectangle(Point center, Point size)`. -/
def centeredRectangle (c size : Int × Int) : Rect :=
  ⟨c.1 - size.1.tdiv 2, c.2 - size.2.tdiv 2, size.1, size.2⟩

/-- `WorldUtils.ClampToWorld(rect, 0)`. -/
def clampToWorld (w : World) (r : Rect) : Rect :=
  let l := max 0 (min r.x w.maxTilesX)
  let t := max 0 (min r.y w.maxTilesY)
  let rr := max 0 (min r.right w.maxTilesX)
  let b := max 0 (min r.bottom w.maxTilesY)
  ⟨l, t, rr - l, b - t⟩

/-- `Main.GetAreaToLight()` from `Main.Camera.ScaledPosition` and `ScaledSize` (the unscaled
pair when `Lighting.Mode` is not 3). -/
def areaToLight (pos size : Float32 × Float32) : Rect :=
  let fl (v : Float32) : Int := convI4d v.toFloat.floor
  let x0 := fl (pos.1 / 16) - 1
  let x1 := fl ((pos.1 + size.1) / 16) + 2
  let y0 := fl (pos.2 / 16) - 1
  let y1 := fl ((pos.2 + size.2) / 16) + 2
  ⟨x0, y0, x1 - x0, y1 - y0⟩

/-- `SceneMetricsScanSettings.VisualScanArea` as `Main.UpdateSceneMetrics` builds it. -/
def visualScanArea (pos size : Float32 × Float32) : Rect := (areaToLight pos size).inflate 28 28

/-- `WorldGen.oceanLevel`. -/
def oceanLevel (w : World) : Float := (w.worldSurface + w.rockLayer) / 2 + 40

/-- `WorldGen.oceanDepths(x, y)`. -/
def oceanDepths (w : World) (x y : Int) : Bool :=
  if Float.ofInt y > oceanLevel w then false
  else x < beachDistance || x > w.maxTilesX - beachDistance

/-- The entity center as `Entity.Center` computes it for a player `position`
(`width` 20, `height` 42). -/
def playerCenter (position : Float32 × Float32) (width : Int := 20) (height : Int := 42) :
    Float32 × Float32 :=
  (position.1 + Float32.ofInt (width.tdiv 2), position.2 + Float32.ofInt (height.tdiv 2))

/-! ## `SceneMetrics.Scan` -/

/-- The raw counters of `ScanTiles`. -/
structure Counts where
  tiles : Array Nat := Array.replicate tileIdCount 0
  liquids : Array Nat := Array.replicate 4 0
  oceanSand : Nat := 0
  candles : OriginalCandleBuffs.Counts := {}
  deriving Inhabited

/-- `SceneMetrics.ScanTiles` (the parts that feed counts): the zone box around `tileCenter`,
column-major. An inactive tile counts its liquid; an active tile counts its type, and
desert-biome sand inside `oceanDepths` also counts as ocean sand. -/
def scanTiles (w : World) (tileCenter : Int × Int) : Counts := Id.run do
  let r := clampToWorld w (centeredRectangle tileCenter zoneScanSize)
  let mut c : Counts := {}
  for i in [0:r.w.toNat] do
    let x := r.x + i
    for j in [0:r.h.toNat] do
      let y := r.y + j
      match w.tileAt x y with
      | none => pure ()
      | some t =>
        c := { c with candles := c.candles.addTile t.active t.type t.frameX }
        if !t.active then
          if t.liquid > 0 then c := { c with liquids := c.liquids.modify t.liquidType (· + 1) }
        else
          c := { c with tiles := c.tiles.modify t.type (· + 1) }
          if isDesertBiomeSand t.type && oceanDepths w x y then c := { c with oceanSand := c.oceanSand + 1 }
  return c

/-- The `switch (frameX / 36)` of an active water fountain (tile 207, `frameY >= 72`). -/
def fountainStyle (frameX : Int) : Int :=
  match frameX.tdiv 36 with
  | 0 => 0 | 1 => 12 | 2 => 3 | 3 => 5 | 4 => 2 | 5 => 10 | 6 => 4 | 7 => 9 | 8 => 8 | 9 => 6
  | _ => -1

/-- `SceneMetrics.ScanOnScreenTiles(area)` reduced to `ActiveFountainColor`: column-major, every
active fountain overwrites the previous value, an unknown fountain style writes −1. Starts from
the −1 of `Reset`. -/
def scanFountain (w : World) (area : Rect) : Int := Id.run do
  let r := clampToWorld w area
  let mut color : Int := -1
  for i in [0:r.w.toNat] do
    let x := r.x + i
    for j in [0:r.h.toNat] do
      let y := r.y + j
      match w.tileAt x y with
      | some t => if t.active && t.type == 207 && t.frameY >= 72 then color := fountainStyle t.frameX
      | none => pure ()
  return color

/-- The fields of `Main.SceneMetrics` that the water style chain reads, plus the other height
zones that `CalculateZones` sets beside them. Counts are the values after
`AggregateTileCounts`. -/
structure Metrics where
  lastScanTime : Nat
  center : Float32 × Float32
  tileCenter : Int × Int
  holyTileCount : Int
  evilTileCount : Int
  bloodTileCount : Int
  snowTileCount : Int
  jungleTileCount : Int
  sandTileCount : Int
  oceanSandTileCount : Int
  desertSandTileCount : Int
  mushroomTileCount : Int
  honeyBlockCount : Int
  meteorTileCount : Int
  dungeonTileCount : Int
  shimmerTileCount : Int
  activeFountainColor : Int
  zoneSkyHeight : Bool
  zoneOverworldHeight : Bool
  belowSurface : Bool
  zoneDirtLayerHeight : Bool
  zoneRockLayerHeight : Bool
  zoneUnderworldHeight : Bool
  zoneCorrupt : Bool
  zoneCrimson : Bool
  zoneHallow : Bool
  zoneJungle : Bool
  zoneSnow : Bool
  zoneDesert : Bool
  zoneGlowshroom : Bool
  zoneBeach : Bool
  candleZones : OriginalCandleBuffs.Zones := {}
  /-- `HasGardenGnome`: a tile 567 in the scan (`AggregateTileCounts` IL_0021..0032). -/
  hasGardenGnome : Bool := false
  deriving BEq, Inhabited, Repr

/-- `SceneMetrics.AggregateTileCounts` followed by `CalculateZones` (the zones above). -/
def aggregate (w : World) (scanTime : Nat) (center : Float32 × Float32) (tileCenter : Int × Int)
    (c : Counts) (fountain : Int) : Metrics :=
  let n (i : Nat) : Int := c.tiles[i]?.getD 0
  let sunflower : Int := if w.infectedSeed then -30 else -10
  let holy0 := n 109 + n 492 + n 110 + n 113 + n 117 + n 116 + n 164 + n 403 + n 402
  let snow := n 147 + n 148 + n 161 + n 162 + n 164 + n 163 + n 200
  let jungle := if w.remixWorld then n 60 + n 61 + n 62 + n 74 + n 225
    else n 60 + n 61 + n 62 + n 74 + n 226 + n 225
  let evilBase := n 23 + n 661 + n 24 + n 25 + n 32 + n 112 + n 163 + n 400 + n 398 + n 27 * sunflower
  let evil0 := if w.remixWorld then evilBase + n 474 else evilBase
  let bloodBase := n 199 + n 662 + n 201 + n 203 + n 200 + n 401 + n 399 + n 234 + n 352 + n 27 * sunflower
  let blood0 := if w.remixWorld then bloodBase + n 195 else bloodBase
  let mushroom := n 70 + n 71 + n 72 + n 528
  let sand := n 53 + n 112 + n 116 + n 234 + n 397 + n 398 + n 402 + n 399 + n 396 + n 400 + n 403 + n 401
  -- clamp, cancel against each other, clamp again
  let holy1 := max holy0 0
  let evil1 := max evil0 0
  let blood1 := max blood0 0
  let holy := max (holy1 - evil1 - blood1) 0
  let evil := max (evil1 - holy1) 0
  let blood := max (blood1 - holy1) 0
  let oceanSand : Int := c.oceanSand
  let desertSand := max 0 (sand - oceanSand)
  let y := tileCenter.2
  let yf := Float.ofInt y
  let underworld := y > underworldLayer w
  { lastScanTime := scanTime, center, tileCenter
    holyTileCount := holy, evilTileCount := evil, bloodTileCount := blood, snowTileCount := snow
    jungleTileCount := jungle, sandTileCount := sand, oceanSandTileCount := oceanSand
    desertSandTileCount := desertSand, mushroomTileCount := mushroom, honeyBlockCount := n 229
    hasGardenGnome := n 567 > 0
    meteorTileCount := n 37, dungeonTileCount := n 41 + n 43 + n 44 + n 481 + n 482 + n 483
    shimmerTileCount := (c.liquids[3]?.getD 0 : Nat)
    activeFountainColor := fountain
    -- 0.35 is the float literal widened to double: 0.3499999940395355
    zoneSkyHeight := !(yf > w.worldSurface * 0.3499999940395355)
    zoneOverworldHeight := !(yf > w.worldSurface) && yf > w.worldSurface * 0.3499999940395355
    belowSurface := yf > w.worldSurface
    zoneDirtLayerHeight := !(yf > w.rockLayer) && yf > w.worldSurface
    zoneRockLayerHeight := !underworld && yf > w.rockLayer
    zoneUnderworldHeight := underworld
    zoneCorrupt := evil >= corruptionTileThreshold
    zoneCrimson := blood >= crimsonTileThreshold
    zoneHallow := holy >= hallowTileThreshold
    zoneJungle := jungle >= jungleTileThreshold && !underworld
    zoneSnow := snow >= snowTileThreshold w.skyblockLowTiles
    zoneDesert := desertSand >= desertTileThreshold w.skyblockLowTiles
    zoneGlowshroom := mushroom >= mushroomTileThreshold
    zoneBeach := oceanDepths w tileCenter.1 tileCenter.2
    candleZones := c.candles.zones }

/-- `SceneMetrics.Scan(settings)` with `BiomeScanCenterPositionInWorld = center` and
`VisualScanArea = visualArea`. If the previous scan ran in the same `GameUpdateCount` with the same
center, the original returns at once and the metrics stay as they were. -/
def scan (w : World) (previous : Option Metrics) (gameUpdateCount : Nat) (center : Float32 × Float32)
    (visualArea : Option Rect) : Metrics :=
  match previous with
  | some p =>
    if p.lastScanTime == gameUpdateCount && p.center.1 == center.1 && p.center.2 == center.2 then p
    else fresh
  | none => fresh
where
  fresh :=
    let tc := clampedInWorld w (toTileCoordinates center)
    let counts := scanTiles w tc
    let fountain := match visualArea with | some a => scanFountain w a | none => -1
    aggregate w gameUpdateCount center tc counts fountain

/-! ## Background style (`Main.GetPreferredBGStyleForPlayer`, `Main.DrawBG`) -/

/-- `Main.GetPreferredBGStyleForPlayer()`. `screenPositionX` is `Main.screenPosition.X`, `treeX` is
`Main.treeX` (the first three entries). -/
def preferredBGStyle (m : Metrics) (screenPositionX : Float32) (screenWidth : Int) (treeX : Array Int) : Int :=
  let tileX := convI4 ((screenPositionX + Float32.ofInt (screenWidth.tdiv 2)) / 16)
  if m.zoneBeach then
    if m.zoneHallow then 6
    else if m.zoneCorrupt then (if m.bloodTileCount > m.evilTileCount then 8 else 1)
    else if m.zoneCrimson then 8
    else if m.honeyBlockCount > 400 then 3
    else 4
  else if m.zoneGlowshroom then 9
  else if m.zoneDesert then
    if m.zoneCorrupt && m.evilTileCount > m.bloodTileCount && m.evilTileCount > m.holyTileCount then 5
    else if m.zoneCrimson && m.bloodTileCount > m.holyTileCount then 14
    else if m.zoneHallow then 13
    else 2
  else if m.zoneHallow then 6
  else if m.zoneCorrupt then (if m.bloodTileCount > m.evilTileCount then 8 else 1)
  else if m.zoneCrimson then 8
  else if m.zoneJungle then 3
  else if m.zoneSnow then 7
  else if tileX < treeX.getD 0 0 then 0
  else if tileX < treeX.getD 1 0 then 10
  else if tileX < treeX.getD 2 0 then 11
  else 12

/-- `WorldGen.GetCurrentBackgroundOfStyle(style)`. -/
def Backgrounds.ofStyle (b : Backgrounds) (style : Int) : Int :=
  match style with
  | 0 => b.treeBG1 | 1 => b.corruptBG | 2 => b.desertBG | 3 => b.jungleBG | 4 => b.oceanBG
  | 6 => b.hallowBG | 7 => b.snowBG | 8 => b.crimsonBG | 9 => b.mushroomBG
  | 10 => b.treeBG2 | 11 => b.treeBG3 | 12 => b.treeBG4
  | _ => -1

/-- `Main.bgStyle`, `Main.bgDelay` and `Main.instantBGTransitionCounter`. -/
structure BGState where
  bgStyle : Int := 0
  bgDelay : Int := 0
  instantBGTransitionCounter : Int := 0
  deriving BEq, Inhabited, Repr

/-- `Main.DrawBG_HandleBackgroundTransition(newStyle)` outside the main menu. -/
def handleTransition (b : Backgrounds) (st : BGState) (newStyle : Int) : BGState :=
  let newForest := isForest newStyle
  if isForest st.bgStyle && newForest && b.ofStyle st.bgStyle == b.ofStyle newStyle then
    { st with instantBGTransitionCounter := 1 }
  else
    let limit : Int := if newForest then 60 else 30
    let d := st.bgDelay + 1
    if d > limit then { st with bgDelay := if newForest then 0 else -60, bgStyle := newStyle }
    else { st with bgDelay := d }

/-- The `bgStyle` part of `Main.DrawBG` in a world (`Main.gameMenu` false), given the preferred
style of this frame. -/
def drawBG (b : Backgrounds) (st : BGState) (preferred : Int) : BGState :=
  let st :=
    if st.bgDelay < 0 then { st with bgDelay := st.bgDelay + 1 }
    else if preferred != st.bgStyle then handleTransition b st preferred
    else if st.bgDelay > 0 then { st with bgDelay := st.bgDelay - 1 }
    else st
  if st.instantBGTransitionCounter > 0 then
    { st with instantBGTransitionCounter := st.instantBGTransitionCounter - 1, bgStyle := preferred }
  else st

/-! ## `Main.CalculateWaterStyle` -/

/-- `Main.CalculateWaterStyle(ignoreFountains)`. `Main.DrawWaters` passes `false`. -/
def calculateWaterStyle (w : World) (m : Metrics) (bgStyle : Int) (bloodMoon dayTime : Bool)
    (ignoreFountains : Bool := false) : Nat :=
  if m.activeFountainColor >= 0 && !ignoreFountains then m.activeFountainColor.toNat
  else if bloodMoon && !dayTime then 9
  else match bgStyle with
    | 1 | 5 => 2
    | 2 => if w.remixWorld then 6 else if m.belowSurface then 12 else 6
    | 3 => 3
    | 4 => 13
    | 6 | 13 => 4
    | 7 => 5
    | 8 | 14 => 10
    | _ =>
      let y := Float.ofInt m.tileCenter.2
      if w.remixWorld then
        if y > w.rockLayer then 7 else if m.belowSurface then 8 else 0
      else if y > w.rockLayer + 40 then
        if m.zoneGlowshroom then 7 else 8
      else if m.belowSurface then 7 else 0

/-! ## One frame -/

/-- What the frame reads of the camera and the local player. -/
structure View where
  /-- `Main.LocalPlayer.Center` (see `playerCenter`). -/
  center : Float32 × Float32
  /-- `Main.Camera.ScaledPosition` / `ScaledSize`, for `GetAreaToLight`. -/
  scaledPosition : Float32 × Float32
  scaledSize : Float32 × Float32
  /-- `Main.screenPosition.X` and `Main.screenWidth`, for `GetPreferredBGStyleForPlayer`. -/
  screenPositionX : Float32
  screenWidth : Int
  deriving Inhabited

/-- The state carried from frame to frame. -/
structure State where
  metrics : Option Metrics := none
  bg : BGState := {}
  waterStyle : Nat := 0
  deriving Inhabited

/-! ## Integration helpers -/

/-- World statics and tiles from the shared terrain. `treeX` and the `treeBG*` choices are not in
`OriginalWorld.Header` and must be supplied separately (`Backgrounds`, `frame`'s `treeX`).
`infectedSeed` and `Skyblock.lowTiles` are passed in; the header's `skyblock` flag is not
assumed to equal `lowTiles`. -/
def World.ofTerrain (t : OriginalWorld.Terrain) (infectedSeed skyblockLowTiles : Bool := false) : World :=
  { maxTilesX := t.header.width, maxTilesY := t.header.height, worldSurface := t.header.surface,
    rockLayer := t.header.rockLayer, remixWorld := t.header.remixWorld, infectedSeed, skyblockLowTiles,
    tileAt := fun x y => if x < 0 || y < 0 then none else (t.tile? x.toNat y.toNat).map SceneTile.ofTile }

/-- The view at game zoom 1 (`GameViewMatrix.Translation` = 0), where `Main.Camera.ScaledPosition`
is `Main.screenPosition` and `ScaledSize` is the screen size. `position` is the local player's
`Entity.position`. -/
def View.ofScreen (position screenPosition : Float32 × Float32) (screenWidth screenHeight : Int) : View :=
  { center := playerCenter position, scaledPosition := screenPosition,
    scaledSize := (Float32.ofInt screenWidth, Float32.ofInt screenHeight),
    screenPositionX := screenPosition.1, screenWidth }

/-- The state `Main.BlackFadeCameraTeleport` leaves (spawn, teleport): `bgDelay` 0 and
`instantBGTransitionCounter` 10, so for ten frames `bgStyle` follows the preferred style at once. -/
def State.teleported (bgStyle : Int := 0) : State :=
  { bg := { bgStyle, bgDelay := 0, instantBGTransitionCounter := 10 } }

/-- One `Main.DoDraw`: lighting scan, then `DrawBG`, then the foreground `DrawWaters`. -/
def frame (w : World) (b : Backgrounds) (treeX : Array Int) (st : State) (gameUpdateCount : Nat)
    (v : View) (bloodMoon dayTime : Bool) : State :=
  let m := scan w st.metrics gameUpdateCount v.center (some (visualScanArea v.scaledPosition v.scaledSize))
  let pref := preferredBGStyle m v.screenPositionX v.screenWidth treeX
  let bg := drawBG b st.bg pref
  { metrics := some m, bg, waterStyle := calculateWaterStyle w m bg.bgStyle bloodMoon dayTime }

end Lean4Terria.OriginalSceneWaterStyle
