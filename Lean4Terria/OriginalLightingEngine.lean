import Lean4Terria.OriginalLightMap

/-!
# `Terraria.Graphics.Light.LightingEngine` frame state machine (desktop 1.4.5.8, `Terraria.exe`
sha256 `d52c7ea7…03e0`)

Hand transcription of the real desktop CIL in `.lake/original-lightmap/dump-e07x7182/il/` and
`.lake/original-lighting-engine/harness/dumpsel-*/il/`, extending the released leaf
`Lean4Terria.OriginalLightMap` (formal sha256 `17aeaf5a…a123a0`). Every map / engine field,
`Float32` / `Int32` operation and the physical column-major arrays come from that module
unchanged (`Engine.beginScan`, `exportCell`, `processBlur`, `present`, `incrementState`, …).

What this module adds, in CIL order:

* `LightingEngine.ProcessArea(Rectangle)` (token 100672166): the `switch` on `_state`, the
  `Main.mapDelay` gate of phase 0 (the **old** value is tested, `> 0` decrements and skips the
  export, `≤ 0` exports), the four `Main.renderCount` stores (3 / 0 / 1 / 2), the
  `TimeLogger.LightingByPass[_state]` access (a `TimeLogData[4]`; any raw state outside `0‥3`
  runs no phase and faults there, before `IncrementState`), then `IncrementState`.
* `ExportToMiniMap` (100672174) and its lambda `<>c__DisplayClass23_0.<ExportToMiniMap>b__0`
  (100681212): `Main.mapEnabled` gate, active-area emptiness test, `(X+28, Y+28, W-56, H-56)`,
  FNA `Rectangle.Intersect` with the world inflated by −40 and `MapHelper.sceneArea`, the
  per-cell `Math.Max(Single)` (Mono corlib, NaN-propagating first argument), `conv.i4`,
  `Math.Min(255, ·)`, `conv.u1`, `WorldMap.UpdateLighting`, `Main.updateMap = area`.
* `ProcessScan` (100672168): the released map half, then `TileLightScanner.Update()`
  (`_random.NextSeed()`, 100672127), `Main.ShouldShowInvisibleBlocksAndWalls()` (100667125),
  `TileLightScanner.ExportTo` (100672125) with its column lambda
  `<>c__DisplayClass2_0.<ExportTo>b__0` (100681202) and `IsTileNullOrTouchingNull`
  (100672126) / `WorldGen.InWorld(int, int, int)` (100667738).
* `ProcessBlur` + `Present` with the native read order of their `Main` inputs.
* `.ctor` / `Rebuild` seed order (scanner, active map, working map), and
  `Lighting.AddLight(int, int, float, float, float)` (100666302).
* `FastParallel.For` (ReLogic 100663446) as used by the two exports: `count = to - from`
  (wrapping), `0` returns, negative throws (`CountdownEvent`), otherwise column ranges on worker
  threads. The per-column bodies write disjoint cells, so the result equals the ascending
  sequential order used here **provided the callees are per-cell** (see `Callees`).

Everything the engine reads from or writes to outside its own object is a typed callee in an
arbitrary monad (`Callees m`), or an explicit `MainStatics` field: nothing defaults to `false`,
zero or "scan succeeded". Natively pending providers (tile light sources, the minimap tile
writer, scene metrics) are supplied by their own modules / Main. `TimeLogger` timing values
(`Stopwatch` ticks) are diagnostics and are not modelled; only the array access that can fault is.
-/
namespace Lean4Terria.OriginalLightingEngine

open Lean4Terria.OriginalLightMap (Vector3 Rectangle FastRandom LightMap LightMaskMode Engine
  DecayInputs PerFrameLight EngineState)

/-! ## Faults -/

/-- Exceptions of the native frame step. `map` wraps the released `OriginalLightMap.Fault`
(including `negativeParallelRange` for `FastParallel.For` with a negative count and the
`runtimeDependentConversion` of a NaN / out-of-range `conv.i4`). -/
inductive Fault where
  | map (fault : OriginalLightMap.Fault)
  /-- `IndexOutOfRangeException` reading `TimeLogger.LightingByPass[_state]` (length 4). -/
  | lightingPassIndex (state : EngineState)
  /-- A `FastParallel.For` whose `to - from` wrapped to a positive count with `from ≥ to`
  (worker ranges would be computed with wrap-around; not reproduced, refused). -/
  | wrappedParallelRange (fromInclusive toExclusive : Int32)
  /-- An exception raised by a callee (provider-defined description). -/
  | callee (name : String) (detail : String)
deriving Repr, Inhabited

@[inline] def liftMap {m : Type → Type} [Monad m] [MonadExceptOf Fault m] {α : Type}
    (r : Except OriginalLightMap.Fault α) : m α :=
  match r with
  | .ok a => pure a
  | .error f => throw (Fault.map f)

/-! ## FNA `Rectangle` helpers used by the exports (FNA.dll `46d201f5…`) -/

namespace Rect
def left (r : Rectangle) : Int32 := r.x
def top (r : Rectangle) : Int32 := r.y
/-- `get_Right`: `X + Width` (wrapping). -/
def right (r : Rectangle) : Int32 := r.x + r.width
/-- `get_Bottom`: `Y + Height` (wrapping). -/
def bottom (r : Rectangle) : Int32 := r.y + r.height
/-- `Intersects(Rectangle)` called on `a` with `value = b`. -/
def intersects (a b : Rectangle) : Bool :=
  left b < right a && left a < right b && top b < bottom a && top a < bottom b
/-- `Rectangle.Intersect(value1, value2)`. -/
def intersect (a b : Rectangle) : Rectangle :=
  if intersects a b then
    let r := min (a.x + a.width) (b.x + b.width)
    let l := max a.x b.x
    let t := max a.y b.y
    let bo := min (a.y + a.height) (b.y + b.height)
    ⟨l, t, r - l, bo - t⟩
  else ⟨0, 0, 0, 0⟩
end Rect

/-! ## `TileLightScanner` state -/

/-- Fields of `Terraria.Graphics.Light.TileLightScanner`. -/
structure TileLightScanner where
  /-- `_random` (seeded by `FastRandom.CreateWithRandomSeed()` in `.ctor`). -/
  random : FastRandom
  /-- `_drawInvisibleWalls` (false after `.ctor`, set by every `ExportTo`). -/
  drawInvisibleWalls : Bool
deriving Inhabited, Repr, BEq

/-- `TileLightScanner.Update()`: `_random.NextSeed()`. -/
def TileLightScanner.update (s : TileLightScanner) : TileLightScanner :=
  { s with random := s.random.nextSeed }

/-! ## Engine object and the `Main` statics it writes -/

/-- The whole native `LightingEngine` object: the released fields plus `_tileScanner`. -/
structure State where
  engine : Engine
  scanner : TileLightScanner
deriving Inhabited

/-- `Main` statics written by `ProcessArea` / `ExportToMiniMap` (owned by Main, passed in and
returned so the caller keeps one copy). -/
structure MainStatics where
  /-- `Main.mapDelay`. -/
  mapDelay : Int32
  /-- `Main.renderCount`. -/
  renderCount : Int32
  /-- `Main.updateMap : Rectangle?`. -/
  updateMap : Option Rectangle
deriving Inhabited, Repr, BEq

/-! ## Callees

Each field is the exact native callee or static read. Requirements on providers:

* `mapUpdateLighting x y light` is `Main.Map.UpdateLighting(x, y, light)` (its `bool` result is
  popped). Natively the calls of different columns run concurrently; a provider must only touch
  map tile `(x, y)` (the real one does: `_tiles[x, y]` and `MapHelper.CreateMapTile(x, y, …)`).
* `tileIsNull x y` is `Main.tile[x, y] == null`; `Tile[,].Get` faults beyond the array bounds.
* `tileMask x y` is `TileLightScanner.GetTileMask(Main.tile[x, y])` (released in
  `OriginalTileLightMask`); `tileLight sc x y` is `GetTileLight(x, y, out color)` run by the
  scanner object `sc` (its `_random` after `Update`, its `_drawInvisibleWalls` after the
  `ExportTo` store). Both are pending in this leaf's closure (other workers' modules).
* `decayInputs` are the reads of `UpdateLightDecay`; `gameUpdateCount` / `isCapturing` are
  `Main.GameUpdateCount` / `CaptureManager.Instance.IsCapturing` (`ApplyPerFrameLights`).
-/
structure Callees (m : Type → Type) where
  /-- `Main.mapEnabled`. -/
  mapEnabled : m Bool
  /-- `(Main.maxTilesX, Main.maxTilesY)`. -/
  maxTiles : m (Int32 × Int32)
  /-- `MapHelper.sceneArea`. -/
  sceneArea : m Rectangle
  /-- `Main.Map.UpdateLighting(x, y, light)`. -/
  mapUpdateLighting : Int32 → Int32 → UInt8 → m Bool
  /-- `Main.UpdateSceneMetrics()`. -/
  updateSceneMetrics : m Unit
  /-- `Main.SceneMetrics.EchoMonolith`. -/
  echoMonolith : m Bool
  /-- `Main.SceneMetrics.PerspectivePlayer.CanSeeInvisibleBlocks` (a null player faults). -/
  perspectiveCanSeeInvisibleBlocks : m Bool
  /-- `Main.tile[x, y] == null`. -/
  tileIsNull : Int32 → Int32 → m Bool
  /-- `GetTileMask(Main.tile[x, y])`. -/
  tileMask : Int32 → Int32 → m LightMaskMode
  /-- `GetTileLight(x, y, out color)` of the given scanner state. -/
  tileLight : TileLightScanner → Int32 → Int32 → m Vector3
  /-- Inputs of `UpdateLightDecay`. -/
  decayInputs : m DecayInputs
  /-- `Main.GameUpdateCount`. -/
  gameUpdateCount : m UInt32
  /-- `CaptureManager.Instance.IsCapturing`. -/
  isCapturing : m Bool

variable {m : Type → Type} [Monad m] [MonadExceptOf Fault m]

/-! ## `FastParallel.For` as the exports use it -/

/-- `FastParallel.For(from, to, body)` in the game's multi-task mode: a zero count returns, a
negative count throws from `new CountdownEvent(n)` before any body runs, otherwise every index of
`[from, to)` runs once (ascending here; natively columns run on several threads). -/
def parallelFor (fromInclusive toExclusive : Int32) (body : Int32 → m Unit) : m Unit := do
  let count := toExclusive - fromInclusive
  if count == 0 then return
  if count < 0 then throw (Fault.map (.negativeParallelRange count))
  if fromInclusive ≥ toExclusive then throw (Fault.wrappedParallelRange fromInclusive toExclusive)
  for k in [0:count.toInt.toNat] do
    body (fromInclusive + Int32.ofNat k)

/-- `for (y = top; y < bottom; y++)` of both lambdas (`blt`, signed; no wrap-around reached
since the loop stops at `bottom`). -/
def forRows (top bottom : Int32) (body : Int32 → m Unit) : m Unit := do
  if top < bottom then
    for k in [0:(bottom.toInt - top.toInt).toNat] do
      body (top + Int32.ofNat k)

/-! ## Phase 0: `ExportToMiniMap` -/

/-- Mono corlib `Math.Max(Single, Single)`: `a > b ? a : (IsNaN(a) ? a : b)`. -/
def mathMaxSingle (a b : Float32) : Float32 :=
  if a > b then a else if a.isNaN then a else b

/-- The byte the lambda passes to `UpdateLighting` for one active-map colour:
`(byte)Math.Min(255, (int)(Max(Max(X, Y), Z) * 255f))`. -/
def miniMapLight (v : Vector3) : Except OriginalLightMap.Fault UInt8 := do
  let l := mathMaxSingle (mathMaxSingle v.x v.y) v.z
  let i ← OriginalLightMap.convI4 (l * 255)
  let i := if (255 : Int32) > i then i else 255
  return i.toUInt32.toUInt8

/-- `ExportToMiniMap()`: reads the active map only; returns the new `Main.updateMap`. -/
def exportToMiniMap (c : Callees m) (e : Engine) (main : MainStatics) : m MainStatics := do
  unless (← c.mapEnabled) do return main
  let a := e.activeProcessedArea
  if a.width ≤ 0 then return main
  if !(a.height > 0) then return main
  let area : Rectangle := ⟨a.x + 28, a.y + 28, a.width - 56, a.height - 56⟩
  let (maxX, maxY) ← c.maxTiles
  let world := (Rectangle.mk 0 0 maxX maxY).inflate (-40) (-40)
  let area := Rect.intersect area world
  let area := Rect.intersect area (← c.sceneArea)
  parallelFor (Rect.left area) (Rect.right area) fun x =>
    forRows (Rect.top area) (Rect.bottom area) fun y => do
      let v ← liftMap (e.activeLightMap.get (x - a.x) (y - a.y))
      let light ← liftMap (miniMapLight v)
      let _ ← c.mapUpdateLighting x y light
      pure ()
  return { main with updateMap := some area }

/-! ## Phase 2: `ProcessScan` -/

/-- `WorldGen.InWorld(x, y, fluff)`. -/
def inWorld (maxTilesX maxTilesY x y fluff : Int32) : Bool :=
  !(x < fluff) && x < maxTilesX - fluff && !(y < fluff) && y < maxTilesY - fluff

/-- `TileLightScanner.IsTileNullOrTouchingNull(x, y)`: not `InWorld(x, y, 1)`, or one of
`(x, y)`, `(x+1, y)`, `(x-1, y)`, `(x, y-1)`, `(x, y+1)` is null (short-circuit in that order). -/
def isTileNullOrTouchingNull (c : Callees m) (x y : Int32) : m Bool := do
  let (maxX, maxY) ← c.maxTiles
  if !inWorld maxX maxY x y 1 then return true
  if ← c.tileIsNull x y then return true
  if ← c.tileIsNull (x + 1) y then return true
  if ← c.tileIsNull (x - 1) y then return true
  if ← c.tileIsNull x (y - 1) then return true
  c.tileIsNull x (y + 1)

/-- `Main.ShouldShowInvisibleBlocksAndWalls()`. -/
def shouldShowInvisibleBlocksAndWalls (c : Callees m) : m Bool := do
  if ← c.echoMonolith then return true
  c.perspectiveCanSeeInvisibleBlocks

/-- `TileLightScanner.ExportTo(area, outputMap, options)` writing the engine's working map
(`outputMap` is `_workingLightMap`, `area` equals `_workingProcessedArea` set just before):
store `_drawInvisibleWalls`, then per column `x ∈ [Left, Right)`, per row `y ∈ [Top, Bottom)`:
null branch `SetMaskAt(None)`, `set_Item(zero)`; otherwise `GetTileMask`, `SetMaskAt`,
`GetTileLight`, `set_Item`. -/
def exportTo (c : Callees m) (s : State) (area : Rectangle) (drawInvisibleWalls : Bool) : m State := do
  let scanner := { s.scanner with drawInvisibleWalls }
  let mut e := s.engine
  -- the column ranges are disjoint; accumulate sequentially
  let count := Rect.right area - Rect.left area
  if count == 0 then return { s with scanner }
  if count < 0 then throw (Fault.map (.negativeParallelRange count))
  if Rect.left area ≥ Rect.right area then
    throw (Fault.wrappedParallelRange (Rect.left area) (Rect.right area))
  for k in [0:count.toInt.toNat] do
    let x := Rect.left area + Int32.ofNat k
    if Rect.top area < Rect.bottom area then
      for j in [0:((Rect.bottom area).toInt - (Rect.top area).toInt).toNat] do
        let y := Rect.top area + Int32.ofNat j
        if ← isTileNullOrTouchingNull c x y then
          e ← liftMap (e.exportNullCell x y)
        else
          let mask ← c.tileMask x y
          let color ← c.tileLight scanner x y
          e ← liftMap (e.exportCell x y mask color)
  return { engine := e, scanner }

/-- `ProcessScan(area)`: `Inflate(28, 28)`, `_workingProcessedArea`, `SetSize`,
`NonVisiblePadding = 18` (released `beginScan`), `_tileScanner.Update()`, the options read
`ShouldShowInvisibleBlocksAndWalls()`, then `ExportTo`. -/
def processScan (c : Callees m) (s : State) (area : Rectangle) : m State := do
  let e := s.engine.beginScan area
  let scanner := s.scanner.update
  let draw ← shouldShowInvisibleBlocksAndWalls c
  exportTo c { engine := e, scanner } e.workingProcessedArea draw

/-! ## Phase 3: `ProcessBlur` + `Present` -/

/-- `ProcessBlur(); Present();` with the native read order (`UpdateLightDecay` inputs, then
`Main.GameUpdateCount`, then `IsCapturing`). -/
def processBlurPresent (c : Callees m) (s : State) : m State := do
  let decay ← c.decayInputs
  let count ← c.gameUpdateCount
  let capturing ← c.isCapturing
  let e ← liftMap (s.engine.processBlur decay count capturing)
  return { s with engine := e.present }

/-! ## `ProcessArea` -/

/-- `LightingEngine.ProcessArea(area)`: one phase per call, selected by the raw `_state`. -/
def processArea (c : Callees m) (s : State) (main : MainStatics) (area : Rectangle) :
    m (State × MainStatics) := do
  let (s, main) ← match s.engine.state.toInt with
    | 0 =>
      if main.mapDelay > 0 then
        pure (s, { main with mapDelay := main.mapDelay - 1, renderCount := 3 })
      else
        let main ← exportToMiniMap c s.engine main
        pure (s, { main with renderCount := 3 })
    | 1 => do
      c.updateSceneMetrics
      pure (s, { main with renderCount := 0 })
    | 2 => do
      let s ← processScan c s area
      pure (s, { main with renderCount := 1 })
    | 3 => do
      let s ← processBlurPresent c s
      pure (s, { main with renderCount := 2 })
    | _ => pure (s, main)
  -- `TimeLogger.LightingByPass[_state].AddTime(start)`: `TimeLogData[4]`
  let st := s.engine.state
  if st < 0 || st ≥ 4 then throw (Fault.lightingPassIndex st)
  return ({ s with engine := s.engine.incrementState }, main)

/-! ## Lifecycle -/

/-- `new LightingEngine()`: lists, `new TileLightScanner()` (first `CreateWithRandomSeed`), active
map, working map. `nextGuidHash` is `Guid.NewGuid().GetHashCode()` (non-deterministic natively;
the caller owns the policy). -/
def create (nextGuidHash : m Int32) : m State := do
  let scannerSeed ← nextGuidHash
  let activeSeed ← nextGuidHash
  let workingSeed ← nextGuidHash
  return { engine := Engine.create (FastRandom.ofGuidHash activeSeed) (FastRandom.ofGuidHash workingSeed),
           scanner := { random := FastRandom.ofGuidHash scannerSeed, drawInvisibleWalls := false } }

/-- `Rebuild()`: areas empty, state 0, new active then working map; scanner and per-frame lists
are kept. -/
def rebuild (nextGuidHash : m Int32) (s : State) : m State := do
  let activeSeed ← nextGuidHash
  let workingSeed ← nextGuidHash
  return { s with engine := s.engine.rebuild (FastRandom.ofGuidHash activeSeed) (FastRandom.ofGuidHash workingSeed) }

/-- `Clear()` (state, areas and scanner kept). -/
def clear (s : State) : State := { s with engine := s.engine.clear }

/-- `AddLight(x, y, color)`. -/
def addLight (s : State) (x y : Int32) (color : Vector3) : State :=
  { s with engine := s.engine.addLight x y color }

/-- `GetColor(x, y)`. -/
def getColor (s : State) (x y : Int32) : Except Fault Vector3 :=
  match s.engine.getColor x y with
  | .ok v => .ok v
  | .error f => .error (.map f)

/-- `Lighting.AddLight(int i, int j, float r, float g, float b)` when this engine is
`Lighting._activeEngine`: dropped while `Main.gamePaused` or on a server (`netMode == 2`). -/
def lightingAddLight (gamePaused : Bool) (netMode : Int32) (s : State) (i j : Int32) (r g b : Float32) : State :=
  if gamePaused then s else if netMode == 2 then s else addLight s i j ⟨r, g, b⟩

end Lean4Terria.OriginalLightingEngine
