import Lean4Terria.OriginalLightingEngine
import Lean4Terria.OriginalTileLightSources
import Lean4Terria.OriginalTileLightMask
import Lean4Terria.OriginalWorld
import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalLightingRuntime.Tables

/-!
# Per-frame lighting runtime (desktop 1.4.5.8, new lighting / `LightMode.Color`)

Wiring of the released lighting leaves into one object a session can own and a renderer can read:

* `OriginalLightingEngine` (formal `123e7341…`, Root ACCEPT-WITH-NOTES): `LightingEngine` phases.
* `OriginalTileLightSources` (Root ACCEPT-WITH-NOTES): `TileLightScanner.GetTileLight`.
* `OriginalTileLightMask` (released `862a2a84…`): `TileLightScanner.GetTileMask`.
* `OriginalLightMap` (released `17aeaf5a…`): the maps, blur and per-frame lights (through the engine).

Native call sites transcribed here (IL in `.lake/original-scene-metrics-clock/clock-*/`):

* `Lighting.LightTiles(Rectangle)` (100666294): `Main.render = true`, `UpdateGlobalBrightness()`
  (100666295: `1.2`, or `1` when `Main.player[myPlayer].blind`), `_activeEngine.ProcessArea(area)`.
* `Main.GetAreaToLight()` (100667233) for `Lighting.UsingNewLighting`: the camera's *scaled*
  position/size, `Floor((double)(X / 16f))` etc. with `−1` / `+2` margins.
* `Lighting.GetColor(int, int)` (100666309), outside `Main.gameMenu`:
  `new Color(_activeEngine.GetColor(x, y) * GlobalBrightness)` — FNA `Color(Vector3)` is
  `(byte)MathHelper.Clamp(v * 255f, 0, 255)` per channel, alpha 255.
* `Lighting.AddLight(int, int, float, float, float)` (100666302, in the engine leaf) and the
  `Vector2` overloads 100666298/100666299 (`(int)(X / 16f)`, `(int)(Y / 16f)`).
* `Main.ApplyColorOfTheSkiesToTiles()` (MethodDef 3922): `Main.tileColor` from `ColorOfTheSkies`,
  then `SkyManager.ProcessTileColor` (identity while no custom sky is active; a caller with an
  active custom sky must not use `tileColorOfSkies`).

Every engine callee is backed by real state: the world grid (`OriginalWorld.Terrain`) encoded into
the native `Tile` fields the scanner reads, the native static tables in `Tables` (generated from the
native H lines), and the explicit per-frame `Globals` record, which the owning session fills from
its own producers. Nothing here defaults a provider to zero/false.

Declared policies (not native determinism):

* `Guid.NewGuid().GetHashCode()` seeds (`create`, `rebuild`) are caller-supplied integers.
* `Main.rand` is `[ThreadStatic]`. `GetTileLight` runs on `FastParallel` worker threads, whose
  `Main.rand` is created from `Environment.TickCount` on first use and depends on scheduling.
  This runtime keeps one explicit `scanRandom` stream for those draws (only `ApplySurfaceLight`
  with liquid ≥ 200 and `liquidAlpha[13] > 0` draws). The gameplay `Main.rand` of the main thread
  is never touched by the scan.
* The minimap writer (`WorldMap.UpdateLighting` → `MapHelper.CreateMapTile`) needs
  `GetTileBaseOption`, which is not transcribed yet. Phase 0 therefore returns the exact
  `(x, y, light)` export requests in column order (`Outcome.minimapExports`) for the future map
  owner instead of writing a map; `Main.updateMap` is still set exactly.
-/
namespace Lean4Terria.OriginalLightingRuntime

open Lean4Terria.OriginalLightMap (Vector3 Rectangle FastRandom DecayInputs)
open Lean4Terria.OriginalLightingEngine (Fault MainStatics Callees TileLightScanner)

abbrev Sources := Lean4Terria.OriginalTileLightSources.Tile
abbrev SColor := Lean4Terria.OriginalTileLightSources.Color

/-! ## World tiles as the scanner reads them -/

/-- The native `Terraria.Tile` fields of one `OriginalWorld.Tile`, for the bits the lighting and
mask code reads (`sTileHeader` 0‥4 colour, 5 active, 6 inActive, 10 halfBrick, 12‥14 slope;
`bTileHeader` 0‥4 wall colour, 5‥6 liquid kind; `bTileHeader3` 5 invisibleBlock, 6 invisibleWall).
Wire, actuator, frame-number and fullbright bits are not read by any lighting callee and are
encoded as well where `OriginalWorld.Tile` carries them (wires 7‥9 / `bTileHeader` 7, actuator 11).
`bTileHeader2` (wall frame data) is not carried by `OriginalWorld.Tile` and no lighting callee reads
it; it is 0. An inactive tile has `type` 0 and frames `(-1, -1)` (`WorldFile` load, `KillTile`).
`frame` is the tile's current `(frameX, frameY)`; the caller passes the frame the renderer uses for
tiles whose frames are not stored in the world file. -/
def encodeTile (t : OriginalWorld.Tile) (frame : Option (Int × Int)) : Sources :=
  let active := t.id.isSome
  let slope : UInt16 := match t.shape with
    | .slope s => (s.toUInt16 &&& 7) <<< 12
    | _ => 0
  let half : UInt16 := if t.shape == .halfBlock then 1024 else 0
  let wires := t.wires.toUInt16
  let paint : UInt16 := t.tilePaint.toUInt16 &&& 31
  let bits : UInt16 := (if active then 32 else 0) ||| (if t.actuated then 64 else 0)
    ||| (if t.actuator then 2048 else 0)
  let wireBits : UInt16 := (wires &&& 7) <<< 7
  let s : UInt16 := paint ||| bits ||| wireBits ||| half ||| slope
  let (amount, kind) : UInt8 × UInt8 := match t.liquid with
    | some l => (l.amount, match l.kind with | .water => 0 | .lava => 32 | .honey => 64 | .shimmer => 96)
    | none => (0, 0)
  let yellow : UInt8 := if wires &&& 8 != 0 then 128 else 0
  let b : UInt8 := (t.wallPaint &&& 31) ||| kind ||| yellow
  let b3 : UInt8 := (if t.invisibleBlock then 32 else 0) ||| (if t.invisibleWall then 64 else 0)
  let (fx, fy) := if active then frame.getD (-1, -1) else (-1, -1)
  { type := if active then t.id.getD 0 else 0, wall := t.wall, liquid := amount
    sTileHeader := s, bTileHeader := b, bTileHeader2 := 0, bTileHeader3 := b3
    frameX := Int16.ofInt fx, frameY := Int16.ofInt fy }

/-- The world as `Main.tile`: every in-bounds cell is allocated after a world load (no native
null); an index outside `[0, maxTilesX) × [0, maxTilesY)` is the array's `IndexOutOfRange`. -/
structure World where
  terrain : OriginalWorld.Terrain
  /-- `(frameX, frameY)` of active tiles whose frames the world file does not store (the
  renderer's framing). Stored frames always win. -/
  frameOf : Nat → Nat → OriginalWorld.Tile → Option (Int × Int)

def World.maxTiles (w : World) : Int32 × Int32 :=
  (Int32.ofNat w.terrain.header.width, Int32.ofNat w.terrain.header.height)

def World.tile (w : World) (x y : Int32) : Except Lean4Terria.OriginalTileLightSources.Fault (Option Sources) :=
  let (mx, my) := w.maxTiles
  if x < 0 || y < 0 || x ≥ mx || y ≥ my then .error .indexOutOfRange else
  let xn := x.toNatClampNeg
  let yn := y.toNatClampNeg
  match w.terrain.tile? xn yn with
  | none => .error .indexOutOfRange
  | some t =>
    let frame := t.frame.orElse fun _ => w.frameOf xn yn t
    .ok (some (encodeTile t frame))

/-! ## Per-frame inputs -/

/-- Every `Main` / scene value a `LightTiles` call reads besides the world, as the actual native
values of that draw. Producers (owning session): `tileColor` from `tileColorOfSkies` of the draw's
`ColorOfTheSkies`; `liquidAlpha` from the liquid renderer; clock fields from the world clock;
`decay` from `Main.waterStyle`, the perspective player and `Main.SceneState`. -/
structure Globals where
  /-- `Main.tileColor`. -/
  tileColor : SColor
  mouseTextColor : UInt8
  discoR : Int32
  discoG : Int32
  discoB : Int32
  demonTorch : Float32
  globalTimeWrappedHourly : Float32
  timeForVisualEffects : Float
  dayTime : Bool
  time : Float
  raining : Bool
  sundialCooldown : Int32
  moondialCooldown : Int32
  shimmerDarken : Float32
  /-- `Main.liquidAlpha` (15 entries natively). -/
  liquidAlpha : Array Float32
  /-- `Main.jellyfishCageMode` (`byte[3, 25]`, row-major). -/
  jellyfishCageMode : Array UInt8
  netMode : Int32
  myPlayer : Int32
  /-- `Main.SceneMetrics.EchoMonolith`. -/
  echoMonolith : Bool
  /-- `PerspectivePlayer.CanSeeInvisibleBlocks`; `none` is a null perspective player. -/
  canSeeInvisibleBlocks : Option Bool
  /-- `Main.player[myPlayer].blind` (`UpdateGlobalBrightness`). -/
  localPlayerBlind : Bool
  decay : DecayInputs
  /-- `Main.GameUpdateCount`. -/
  gameUpdateCount : UInt32
  /-- `CaptureManager.Instance.IsCapturing`. -/
  isCapturing : Bool
  /-- `Main.mapEnabled`. -/
  mapEnabled : Bool
  /-- `MapHelper.sceneArea`. -/
  sceneArea : Rectangle

/-- `Main.cageFrames` (constant 25 in 1.4.5.8; native H line `cageFrames=25`). -/
def cageFrames : Int32 := 25

/-- `Main.ApplyColorOfTheSkiesToTiles` with an inactive `SkyManager`:
`c = (R + G + B + 7·c) / 10` per channel (int arithmetic, `conv.u1`), alpha 255. -/
def tileColorOfSkies (r g b : UInt8) : SColor :=
  let sum := r.toNat + g.toNat + b.toNat
  let ch (c : UInt8) : UInt8 := ((sum + c.toNat * 7) / 10).toUInt8
  ⟨ch r, ch g, ch b, 255⟩

def sourcesContext (w : World) (g : Globals) (drawInvisibleWalls : Bool) :
    Lean4Terria.OriginalTileLightSources.Context :=
  let (mx, my) := w.maxTiles
  { tileLighted := Tables.tileLighted, tileNoSunLight := Tables.tileNoSunLight
    wallLight := Tables.wallLight, allowLightInWater := Tables.allowLightInWater
    tileSolid := Tables.tileSolid, tileSolidTop := Tables.tileSolidTop
    mask := { tileBlockLight := Tables.tileBlockLight, crackedBricks := Tables.crackedBricks
              drawInvisibleWalls }
    liquidAlpha := g.liquidAlpha
    jellyfishCageRows := 3, jellyfishCageCols := 25, jellyfishCageMode := g.jellyfishCageMode
    cageFrames, worldSurface := w.terrain.header.surface, maxTilesX := mx, maxTilesY := my
    tileColor := g.tileColor, mouseTextColor := g.mouseTextColor
    discoR := g.discoR, discoG := g.discoG, discoB := g.discoB, demonTorch := g.demonTorch
    globalTimeWrappedHourly := g.globalTimeWrappedHourly
    timeForVisualEffects := g.timeForVisualEffects
    dayTime := g.dayTime, time := g.time, raining := g.raining
    sundialCooldown := g.sundialCooldown, moondialCooldown := g.moondialCooldown
    shimmerDarken := g.shimmerDarken, netMode := g.netMode, myPlayer := g.myPlayer }

/-! ## Camera → area -/

/-- `Main.Camera.ScaledPosition` / `ScaledSize` (with `GameViewMatrix` zoom 1 and translation 0
these are `Main.screenPosition` and `(screenWidth, screenHeight)`). -/
structure Camera where
  position : Float32 × Float32
  size : Float32 × Float32

private def floorI4 (v : Float32) : Except Fault Int32 :=
  let d := v.toFloat.floor
  if d.isNaN || d ≥ 2147483648.0 || d < -2147483648.0 then
    .error (.callee "Main.GetAreaToLight" "conv.i4 of a NaN or out-of-range Math.Floor")
  else .ok (Int32.ofInt d.toInt64.toInt)

/-- `Main.GetAreaToLight()` (new lighting). -/
def areaToLight (c : Camera) : Except Fault Rectangle := do
  let (px, py) := c.position
  let (sx, sy) := c.size
  let left := (← floorI4 (px / 16)) - 1
  let right := (← floorI4 ((px + sx) / 16)) + 2
  let top := (← floorI4 (py / 16)) - 1
  let bottom := (← floorI4 ((py + sy) / 16)) + 2
  return ⟨left, top, right - left, bottom - top⟩

/-! ## The runtime object -/

/-- `Lighting._activeEngine` with the `Main` statics it writes, `Lighting.GlobalBrightness`, and the
declared worker-thread `Main.rand` stream. -/
structure Runtime where
  lighting : OriginalLightingEngine.State
  statics : MainStatics
  globalBrightness : Float32
  scanRandom : UnifiedRandom

/-- Side requests of one `LightTiles` call that belong to other owners. -/
structure Outcome where
  /-- Phase 1 called `Main.UpdateSceneMetrics()`; the session runs it now (phase 1 does nothing
  else, so running it right after the call keeps the native order). -/
  updateSceneMetrics : Bool := false
  /-- Phase 0 `WorldMap.UpdateLighting(x, y, light)` calls, in native column order. -/
  minimapExports : Array (Int32 × Int32 × UInt8) := #[]
  /-- Worker-thread `Main.rand` draws made by `GetTileLight` in this call. -/
  scanRandomDraws : Nat := 0

private structure Ctx where
  scanRandom : UnifiedRandom
  outcome : Outcome

private abbrev M := StateT Ctx (Except Fault)

/-- The worker `Main.rand` with a draw counter (`NextDouble` is `Sample()`). -/
private def mainRandOf (r : UnifiedRandom) : Lean4Terria.OriginalTileLightSources.MainRand (UnifiedRandom × Nat) :=
  { state := some (r, 0), nextDouble := fun (r, n) => let (d, r) := r.sample; (d, (r, n + 1)) }

private def sourcesFault (name : String) (f : Lean4Terria.OriginalTileLightSources.Fault) : Fault :=
  .callee name (reprStr f)

private def callees (w : World) (g : Globals) : Callees M where
  mapEnabled := pure g.mapEnabled
  maxTiles := pure w.maxTiles
  sceneArea := pure g.sceneArea
  mapUpdateLighting x y light := do
    modify fun c => { c with outcome := { c.outcome with
      minimapExports := c.outcome.minimapExports.push (x, y, light) } }
    pure true
  updateSceneMetrics := modify fun c => { c with outcome := { c.outcome with updateSceneMetrics := true } }
  echoMonolith := pure g.echoMonolith
  perspectiveCanSeeInvisibleBlocks := match g.canSeeInvisibleBlocks with
    | some b => pure b
    | none => throw (.callee "Main.ShouldShowInvisibleBlocksAndWalls" "NullReferenceException: PerspectivePlayer")
  tileIsNull x y := match w.tile x y with
    | .ok t => pure t.isNone
    | .error f => throw (sourcesFault "Main.tile" f)
  tileMask x y := do
    -- `GetTileMask` reads the scanner's `_drawInvisibleWalls`, stored by `ExportTo` from
    -- `ShouldShowInvisibleBlocksAndWalls()` (echo first, then the perspective player).
    let draw := g.echoMonolith || g.canSeeInvisibleBlocks.getD false
    let tile ← match w.tile x y with
      | .ok t => pure (t.map (·.toMaskTile))
      | .error f => throw (sourcesFault "Main.tile" f)
    let ctx : OriginalTileLightMask.Context :=
      { tileBlockLight := Tables.tileBlockLight, crackedBricks := Tables.crackedBricks, drawInvisibleWalls := draw }
    match OriginalTileLightMask.getTileMask ctx tile with
    | .ok m => pure m
    | .error f => throw (.callee "TileLightScanner.GetTileMask" (reprStr f))
  tileLight sc x y := do
    let ctx ← get
    let c := sourcesContext w g sc.drawInvisibleWalls
    let cell := Lean4Terria.OriginalTileLightSources.getTileLight c w.tile { random := sc.random }
      (mainRandOf ctx.scanRandom) x y
    let (r, drew) := cell.rand.state.getD (ctx.scanRandom, 0)
    set { ctx with scanRandom := r, outcome := { ctx.outcome with
      scanRandomDraws := ctx.outcome.scanRandomDraws + drew } }
    match cell.result with
    | .ok (v, _) => pure v
    | .error f => throw (sourcesFault "TileLightScanner.GetTileLight" f)
  decayInputs := pure g.decay
  gameUpdateCount := pure g.gameUpdateCount
  isCapturing := pure g.isCapturing

/-- `new LightingEngine()` with the caller's three `Guid` hashes (scanner, active map, working
map), `Main.mapDelay`/`renderCount` as the session holds them, and the worker `Main.rand` seed. -/
def create (scannerHash activeHash workingHash : Int32) (statics : MainStatics)
    (scanRandom : UnifiedRandom) : Runtime :=
  let s : OriginalLightingEngine.State :=
    { engine := OriginalLightMap.Engine.create (FastRandom.ofGuidHash activeHash) (FastRandom.ofGuidHash workingHash)
      scanner := { random := FastRandom.ofGuidHash scannerHash, drawInvisibleWalls := false } }
  { lighting := s, statics, globalBrightness := 1.2, scanRandom }

/-- `Lighting.LightTiles(area)`: `UpdateGlobalBrightness()` then one `ProcessArea`. The caller
sets `Main.render` and runs `Main.UpdateSceneMetrics()` when `updateSceneMetrics` is reported. -/
def lightTiles (rt : Runtime) (w : World) (g : Globals) (area : Rectangle) :
    Except Fault (Runtime × Outcome) := do
  let globalBrightness : Float32 := if g.localPlayerBlind then 1 else 1.2
  let run := OriginalLightingEngine.processArea (callees w g) rt.lighting rt.statics area
  let ((lighting, statics), ctx) ← run.run { scanRandom := rt.scanRandom, outcome := {} }
  return ({ lighting, statics, globalBrightness, scanRandom := ctx.scanRandom }, ctx.outcome)

/-- `Lighting.LightTiles(Main.GetAreaToLight())`. -/
def lightTilesAt (rt : Runtime) (w : World) (g : Globals) (camera : Camera) :
    Except Fault (Runtime × Outcome) := do
  lightTiles rt w g (← areaToLight camera)

/-! ## Readers -/

/-- FNA `Color(Vector3)`: `(byte)MathHelper.Clamp(v * 255f, 0, 255)` per channel, alpha 255. -/
def colorOfVector3 (v : Vector3) : Except Fault SColor := do
  let ch (f : Float32) : Except Fault UInt8 :=
    let c := f * 255
    let c := if c > 255 then 255 else c
    let c := if c < 0 then 0 else c
    if c.isNaN then .error (.callee "Color(Vector3)" "conv.u1 of NaN") else .ok c.toUInt8
  return ⟨← ch v.x, ← ch v.y, ← ch v.z, 255⟩

/-- `Lighting.GetColor(x, y)` in world (not `Main.gameMenu`): the active map scaled by
`GlobalBrightness`. -/
def getColor (rt : Runtime) (x y : Int32) : Except Fault SColor := do
  let v ← OriginalLightingEngine.getColor rt.lighting x y
  colorOfVector3 (OriginalLightMap.Vector3.scale v rt.globalBrightness)

/-- `getColor` as the desktop renderer's packed `0xRRGGBBAA` tint. -/
def packedColor (rt : Runtime) (x y : Int32) : Except Fault UInt32 := do
  let c ← getColor rt x y
  return (c.r.toUInt32 <<< 24) ||| (c.g.toUInt32 <<< 16) ||| (c.b.toUInt32 <<< 8) ||| c.a.toUInt32

/-- `getColor` as FNA `Color.PackedValue` (`A << 24 | B << 16 | G << 8 | R`), the form the rain
(`Main.DrawRain`) and water-dust draws take their light in. -/
def packedValue (rt : Runtime) (x y : Int32) : Except Fault UInt32 := do
  let c ← getColor rt x y
  return (c.a.toUInt32 <<< 24) ||| (c.b.toUInt32 <<< 16) ||| (c.g.toUInt32 <<< 8) ||| c.r.toUInt32

/-! ## Light sources -/

/-- `Lighting.AddLight(int, int, float, float, float)`. -/
def addLight (rt : Runtime) (gamePaused : Bool) (netMode : Int32) (i j : Int32) (r g b : Float32) : Runtime :=
  { rt with lighting := OriginalLightingEngine.lightingAddLight gamePaused netMode rt.lighting i j r g b }

/-- `conv.i4` of `X / 16f` as the `Vector2` overloads compute the tile. -/
def tileOfWorld (v : Float32) : Except Fault Int32 :=
  let t := v / 16
  if t.isNaN || t ≥ 2147483648.0 || t ≤ -2147483904.0 then
    .error (.callee "Lighting.AddLight(Vector2)" "conv.i4 of a NaN or out-of-range coordinate")
  else .ok t.toInt32

/-- `Lighting.AddLight(Vector2 position, float r, float g, float b)` (100666299). -/
def addLightAt (rt : Runtime) (gamePaused : Bool) (netMode : Int32) (position : Float32 × Float32)
    (r g b : Float32) : Except Fault Runtime := do
  return addLight rt gamePaused netMode (← tileOfWorld position.1) (← tileOfWorld position.2) r g b

end Lean4Terria.OriginalLightingRuntime
