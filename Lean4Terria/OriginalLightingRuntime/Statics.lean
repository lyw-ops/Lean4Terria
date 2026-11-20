/-!
# Producers of `Main` statics the light scanner reads (desktop 1.4.5.8)

Transcribed from the CIL of the shipped `Terraria.exe` (`.lake/main-b/lighting/il/scan-*`, FieldScan over a physical
copy):

* `Main.DoUpdate_AnimateTileGlows` (100666683), the `demonTorch` part (IL_0000..004a): `demonTorch += demonTorchDir * 0.01f`;
  `> 1` → `1`, dir `-1`; `< 0` → `0`, dir `1`. `Main..cctor` starts it at `1f` / dir `1`. Called from `Main.DoUpdate`
  IL_0873 on every update. The other glows of that method (`martianLight`, …) are not read by the scanner and are
  not modelled here.
* `Main.DoUpdate` IL_08cc..08f5: `timeForVisualEffects += 1.0`; `≥ 216000` → `0` (double). Every `DoUpdate`, before the
  `gameMenu` branch. Default `0`.
* `SceneState.UpdateShimmer(SceneMetrics)` (100663472) with `SceneState.MoveTowards` (100663476/7): called by
  `SceneState.ApplyVisuals` IL_001f (after `UpdateRGBPeriheralProbe`, `UpdateGraveyard`; `ApplyVisuals` returns first on
  `Main.dedServ`), which `SceneState.Update` calls from `DoUpdateInWorld_Inner` IL_0236; `Update` then clears
  `skipTransitions`. `WorldGen.clearWorld` and `Main..cctor` zero `shimmerAlpha` / `shimmerDarken`.

NOT here: `Main.jellyfishCageMode` is written only inside `Main.AnimateTiles_CritterCages` (100666636; 8493 IL lines,
218 `Main.rand` call sites, run every update from `AnimateTiles`). It is a gameplay-RNG producer that must be owned as
a whole by the world-simulation owner; splitting out the jellyfish block would break the draw order.
-/
namespace Lean4Terria.OriginalLightingRuntime.Statics

/-- `Main.demonTorch`, `Main.demonTorchDir`. -/
structure TileGlows where
  demonTorch : Float32 := 1
  demonTorchDir : Int32 := 1
deriving Repr, BEq, Inhabited

/-- `DoUpdate_AnimateTileGlows`, the `demonTorch` statements. -/
def TileGlows.update (g : TileGlows) : TileGlows := Id.run do
  let mut t := g.demonTorch + Float32.ofInt g.demonTorchDir.toInt * (0.01 : Float32)
  let mut d := g.demonTorchDir
  -- `ble.un.s`: the branch to IL_0033 is taken unless `t > 1` (NaN falls through to the next test too)
  if t > 1 then t := 1; d := -1
  -- `bge.un.s`: skip unless `t < 0`
  if t < 0 then t := 0; d := 1
  return { demonTorch := t, demonTorchDir := d }

/-- The `timeForVisualEffects` statements of `Main.DoUpdate` (IL_08cc..08f5). -/
def stepTimeForVisualEffects (t : Float) : Float :=
  let t := t + 1
  -- `blt.un.s IL_08fa`: keep when `t < 216000` or unordered; reset only on `t ≥ 216000`
  if t ≥ 216000 then 0 else t

/-- `Main.shimmerAlpha`, `Main.shimmerDarken`, `SceneState._shimmerBrightenDelay`, `SceneState.skipTransitions`. -/
structure Shimmer where
  shimmerAlpha : Float32 := 0
  shimmerDarken : Float32 := 0
  shimmerBrightenDelay : Float32 := 0
  skipTransitions : Bool := false
deriving Repr, BEq, Inhabited

/-- Mono corlib `Math.Min(Single, Single)` / `Math.Max(Single, Single)` with NaN propagation of either argument. -/
def mathMin (a b : Float32) : Float32 := if a < b then a else if a.isNaN then a else b
def mathMax (a b : Float32) : Float32 := if a > b then a else if a.isNaN then a else b

/-- `SceneState.MoveTowards(ref value, target, inc, dec)`. -/
def moveTowards (skip : Bool) (value target inc dec : Float32) : Float32 :=
  if skip then target
  else if value < target then mathMin (value + inc) target    -- `bge.un.s` falls through only on `<`
  else if value > target then mathMax (value - dec) target    -- `ble.un.s` falls through only on `>`
  else value

/-- The `SceneMetrics` / perspective-player reads of `UpdateShimmer`. -/
structure ShimmerInputs where
  shimmerMonolithState : Int32
  zoneShimmer : Bool
  shimmerMonolithShader : Bool
  shimmering : Bool
  undergroundForShimmering : Bool
deriving Repr, BEq, Inhabited

/-- `SceneState.UpdateShimmer(metrics)`. -/
def Shimmer.update (s : Shimmer) (i : ShimmerInputs) : Shimmer := Id.run do
  let on := i.shimmerMonolithState == 1 || i.zoneShimmer || i.shimmerMonolithShader ||
    (i.shimmering && i.undergroundForShimmering)
  let on := if i.shimmerMonolithState == 2 then false else on
  let mt := moveTowards s.skipTransitions
  let mut s := s
  if on then
    s := { s with shimmerAlpha := mt s.shimmerAlpha 1 0.025 0.025 }
    if s.shimmerAlpha < 0.5 then return s           -- `blt.un.s IL_00e1` (NaN also returns)
    return { s with shimmerDarken := mt s.shimmerDarken 1 0.025 0.025, shimmerBrightenDelay := 4 }
  s := { s with shimmerDarken := mt s.shimmerDarken 0 0.05 0.05 }
  if s.shimmerDarken == 0 then
    s := { s with shimmerBrightenDelay := mt s.shimmerBrightenDelay 0 1 1 }
  if s.shimmerBrightenDelay == 0 then
    s := { s with shimmerAlpha := mt s.shimmerAlpha 0 0.05 0.05 }
  return s

/-- `WorldGen.clearWorld` (world load): `shimmerAlpha = shimmerDarken = 0`; the delay and `skipTransitions` are
`SceneState` fields it does not touch. -/
def Shimmer.clearWorld (s : Shimmer) : Shimmer := { s with shimmerAlpha := 0, shimmerDarken := 0 }

end Lean4Terria.OriginalLightingRuntime.Statics
