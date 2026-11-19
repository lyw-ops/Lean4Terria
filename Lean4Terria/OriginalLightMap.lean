/-!
# `Terraria.Graphics.Light.LightMap` of desktop Terraria 1.4.5.8 (macOS, `Terraria.exe`
sha256 `d52c7ea7…03e0`)

Hand transcription of the real desktop CIL (dumps and constant bits in
`.lake/original-lightmap/`), in CIL order:

* `Terraria.Utilities.FastRandom` (struct, one `UInt64` seed): the 48-bit LCG
  `NextSeed`, `WithModifier` (both overloads), `NextBits`, `Next(max)` (power-of-two branch
  and rejection loop with `Int32` wrap-around), `Next(min, max)`, `NextFloat`, `NextDouble`.
  `CreateWithRandomSeed` (`Guid.NewGuid().GetHashCode()`) is non-deterministic natively, so
  the constructor takes the resulting seed as an input.
* `LightMap`: every field (`NonVisiblePadding`, `Width`, `Height`, `_colors`, `_mask`, the
  five decay properties, `_random`), `.ctor`, `IndexOf` (column-major `x * Height + y`),
  `get_Item` / `set_Item` / `GetLight` / `GetMask` / `SetMaskAt`, `Clear` (whole physical
  arrays), `SetSize` (grows only, never clears), `Blur` = `BlurPass; BlurPass;
  _random.NextSeed()`, `BlurPass` (the two `FastParallel.For` loops and their lambdas
  `<BlurPass>b__46_0/1`), `BlurLine` (per-channel running maximum, the `0.0185` cut-off
  flags, the mask switch with the per-index water random from `_random.WithModifier(i)`).
* The small `LightingEngine` / `Lighting` pieces that only read or write a `LightMap`:
  `GetColor`, `UpdateLightDecay`, `AddLight`, `ApplyPerFrameLights`, `Present`, `Clear`,
  `Rebuild`, `IncrementState`, the map half of `ProcessScan` (`Inflate(28, 28)`, `SetSize`,
  `NonVisiblePadding = 18`) and the per-cell write order of `TileLightScanner.ExportTo`,
  plus `Lighting.GetColor(int, int)` / `GetColor(int, int, Color)` / `GetColor(Point, Color)`
  conversions to FNA `Color`.

NOT here (pending, see `INTERFACE.md`): `TileLightScanner` (tile/wall/liquid/surface/hell
light sources and masks), `ProcessArea` phases 0/1 (`ExportToMiniMap`,
`UpdateSceneMetrics`), the frame phase that calls `ProcessArea`, legacy/retro/other
`LightMode` engines, `Lighting.AddLight` callers and every renderer.

Floats are `Float32` with IEEE single rounding after every operation (the RyuJIT/.NET 8
behaviour the native comparison ran on; see `REPORT.md` for the only Mono-sensitive
expression, the water factor product).
-/
namespace Lean4Terria.OriginalLightMap

/-! ## FNA value types -/

/-- `Microsoft.Xna.Framework.Vector3`. -/
structure Vector3 where
  x : Float32
  y : Float32
  z : Float32
deriving Inhabited, Repr

namespace Vector3
/-- `Vector3.Zero`. -/
def zero : Vector3 := ⟨0, 0, 0⟩
/-- Exact bit identity of the three channels. -/
def bitEq (a b : Vector3) : Bool :=
  a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits && a.z.toBits == b.z.toBits
/-- `op_Multiply(Vector3, Single)`. -/
def scale (v : Vector3) (s : Float32) : Vector3 := ⟨v.x * s, v.y * s, v.z * s⟩
/-- `op_Multiply(Vector3, Vector3)`. -/
def mul (a b : Vector3) : Vector3 := ⟨a.x * b.x, a.y * b.y, a.z * b.z⟩
/-- `MathHelper.Max(Single, Single)`: `value1 > value2 ? value1 : value2`. -/
def mathMax (a b : Float32) : Float32 := if a > b then a else b
/-- `Vector3.Max(ref a, ref b, out r)`. -/
def max (a b : Vector3) : Vector3 := ⟨mathMax a.x b.x, mathMax a.y b.y, mathMax a.z b.z⟩
end Vector3

/-- `Microsoft.Xna.Framework.Rectangle`. -/
structure Rectangle where
  x : Int32
  y : Int32
  width : Int32
  height : Int32
deriving Inhabited, Repr, BEq

namespace Rectangle
/-- `Rectangle.Empty`. -/
def empty : Rectangle := ⟨0, 0, 0, 0⟩
/-- `Rectangle.Contains(int, int)` / `Contains(Point)`. -/
def contains (r : Rectangle) (px py : Int32) : Bool :=
  r.x ≤ px && px < r.x + r.width && r.y ≤ py && py < r.y + r.height
/-- `Rectangle.Inflate(h, v)`. -/
def inflate (r : Rectangle) (h v : Int32) : Rectangle :=
  ⟨r.x - h, r.y - v, r.width + h * 2, r.height + v * 2⟩
end Rectangle

/-! ## `Terraria.Utilities.FastRandom` -/

/-- `Terraria.Utilities.FastRandom` (`<Seed>k__BackingField : UInt64`). -/
structure FastRandom where
  seed : UInt64
deriving Inhabited, Repr, BEq

namespace FastRandom
def randomMultiplier : UInt64 := 25214903917
def randomAdd : UInt64 := 11
def randomMask : UInt64 := 281474976710655

/-- static `NextSeed(ulong)`. -/
def nextSeedOf (seed : UInt64) : UInt64 := (seed * randomMultiplier + randomAdd) &&& randomMask
/-- `.ctor(int)`: `conv.i8` sign extension. -/
def ofInt32 (seed : Int32) : FastRandom := ⟨seed.toInt64.toUInt64⟩
/-- The seed `CreateWithRandomSeed` builds from `Guid.GetHashCode()` (`conv.i8`). -/
def ofGuidHash (hash : Int32) : FastRandom := ofInt32 hash
/-- `WithModifier(ulong)`. -/
def withModifier (r : FastRandom) (modifier : UInt64) : FastRandom := ⟨nextSeedOf modifier ^^^ r.seed⟩
/-- `WithModifier(int, int)`. -/
def withModifierXY (r : FastRandom) (x y : Int32) : FastRandom :=
  let y64 := y.toInt64.toUInt64
  r.withModifier (x.toInt64.toUInt64 + (2654435769 : UInt64) + (y64 <<< 6) + (y64 >>> 2))
/-- instance `NextSeed()`. -/
def nextSeed (r : FastRandom) : FastRandom := ⟨nextSeedOf r.seed⟩
/-- `NextBits(bits)`: advance, then `(int)(Seed >> ((48 - bits) & 63))`. -/
def nextBits (r : FastRandom) (bits : Int32) : Int32 × FastRandom :=
  let s := nextSeedOf r.seed
  let shift := (((48 : Int32) - bits) &&& 63).toInt64.toUInt64
  ((s >>> shift).toUInt32.toInt32, ⟨s⟩)

/-- Rejection loop of `Next(max)` (`IL_0018‥002d`); unbounded natively, as here. -/
partial def nextRejection (r : FastRandom) (max : Int32) : Int32 × FastRandom :=
  let (bits, r) := r.nextBits 31
  let value := bits % max
  if bits - value + (max - 1) < 0 then nextRejection r max else (value, r)

/-- `Next(max)`. -/
def next (r : FastRandom) (max : Int32) : Int32 × FastRandom :=
  if (max &&& -max) == max then
    let (bits, r) := r.nextBits 31
    (((max.toInt64 * bits.toInt64) >>> 31).toInt32, r)
  else nextRejection r max

/-- `Next(min, max)`. -/
def nextRange (r : FastRandom) (min max : Int32) : Int32 × FastRandom :=
  let (v, r) := r.next (max - min)
  (v + min, r)

/-- `NextFloat()`. -/
def nextFloat (r : FastRandom) : Float32 × FastRandom :=
  let (bits, r) := r.nextBits 24
  (bits.toFloat32 * 5.9604645E-08, r)

/-- `NextDouble()`: the product is a `Single` widened afterwards. -/
def nextDouble (r : FastRandom) : Float × FastRandom :=
  let (bits, r) := r.nextBits 32
  ((bits.toFloat32 * 4.656613E-10).toFloat, r)
end FastRandom

/-! ## `LightMap` -/

/-- `Terraria.Graphics.Light.LightMaskMode` is a `byte` enum. The array is a real byte array,
so every byte value is representable; values `≥ 5` hit the switch default (no decay). -/
abbrev LightMaskMode := UInt8

namespace LightMaskMode
def none : LightMaskMode := 0
def solid : LightMaskMode := 1
def water : LightMaskMode := 2
def honey : LightMaskMode := 3
def crackedBricks : LightMaskMode := 4
end LightMaskMode

/-- Every field of a native `LightMap`. `colors` / `mask` are the physical arrays (their
lengths, not `width * height`, bound indexing; `SetSize` never shrinks them). -/
structure LightMap where
  nonVisiblePadding : Int32
  width : Int32
  height : Int32
  colors : Array Vector3
  mask : Array LightMaskMode
  lightDecayThroughAir : Float32
  lightDecayThroughSolid : Float32
  lightDecayThroughCrackedBrick : Float32
  lightDecayThroughWater : Vector3
  lightDecayThroughHoney : Vector3
  random : FastRandom
deriving Inhabited

/-- Exceptions (or schedule-dependent outcomes) of the native operations. On `indexOutOfRange`
the native object is left partially written; callers get only the fault. -/
inductive Fault where
  /-- `System.IndexOutOfRangeException` on the named array. -/
  | indexOutOfRange (array : String) (index : Int32) (length : Nat)
  /-- `FastParallel.For(0, n)` with `n < 0` in multi-task mode: `CountdownEvent` throws. -/
  | negativeParallelRange (count : Int32)
  /-- Padding outside `[0, Height]` with several columns: `BlurPass` column lines overlap
  and run on different threads, so the native result depends on scheduling. -/
  | scheduleDependent (padding height : Int32)
  /-- A `BlurLine` stride that never reaches its end (native infinite loop). -/
  | nonTerminating
  /-- `conv.i4` / `conv.u1` of NaN or an out-of-range float: runtime-dependent natively. -/
  | runtimeDependentConversion (value : Float32)
deriving Repr, Inhabited

def defaultWidth : Int32 := 203
def defaultHeight : Int32 := 203
/-- `BlurLine`'s cut-off (`IL_0054`, bits `0x3C978D50`). -/
def blurThreshold : Float32 := 0.0185

namespace LightMap

/-- Native default water decay `new Vector3(0.88f, 0.96f, 1.015f) * 0.91f`. -/
def defaultWaterDecay : Vector3 := Vector3.scale ⟨0.88, 0.96, 1.015⟩ 0.91
/-- Native default honey decay `new Vector3(0.75f, 0.7f, 0.6f) * 0.91f`. -/
def defaultHoneyDecay : Vector3 := Vector3.scale ⟨0.75, 0.7, 0.6⟩ 0.91

/-- `.ctor()` with the seed `CreateWithRandomSeed` produced. -/
def create (random : FastRandom) : LightMap :=
  { nonVisiblePadding := 0, width := defaultWidth, height := defaultHeight,
    colors := Array.replicate 41209 Vector3.zero, mask := Array.replicate 41209 0,
    lightDecayThroughAir := 0.91, lightDecayThroughSolid := 0.56,
    lightDecayThroughCrackedBrick := 0.8,
    lightDecayThroughWater := defaultWaterDecay, lightDecayThroughHoney := defaultHoneyDecay,
    random }

/-- `IndexOf(x, y)` (private): `x * Height + y` with `Int32` wrap-around. -/
def indexOf (m : LightMap) (x y : Int32) : Int32 := x * m.height + y

@[inline] private def slot (size : Nat) (i : Int32) : Option Nat :=
  if i < 0 then none else
    let n := i.toInt.toNat
    if n < size then some n else none

/-- `get_Item(x, y)` (also `GetLight(x, y, out)`). -/
def get (m : LightMap) (x y : Int32) : Except Fault Vector3 :=
  let i := m.indexOf x y
  match slot m.colors.size i with
  | some k => .ok m.colors[k]!
  | none => .error (.indexOutOfRange "_colors" i m.colors.size)

/-- `set_Item(x, y, value)`. -/
def set (m : LightMap) (x y : Int32) (value : Vector3) : Except Fault LightMap :=
  let i := m.indexOf x y
  match slot m.colors.size i with
  | some k =>
    -- detach the array first so `set!` updates in place when the map is uniquely owned
    let colors := m.colors
    let m := { m with colors := #[] }
    .ok { m with colors := colors.set! k value }
  | none => .error (.indexOutOfRange "_colors" i m.colors.size)

/-- `GetMask(x, y)`. -/
def getMask (m : LightMap) (x y : Int32) : Except Fault LightMaskMode :=
  let i := m.indexOf x y
  match slot m.mask.size i with
  | some k => .ok m.mask[k]!
  | none => .error (.indexOutOfRange "_mask" i m.mask.size)

/-- `SetMaskAt(x, y, mode)`. -/
def setMaskAt (m : LightMap) (x y : Int32) (mode : LightMaskMode) : Except Fault LightMap :=
  let i := m.indexOf x y
  match slot m.mask.size i with
  | some k =>
    let mask := m.mask
    let m := { m with mask := #[] }
    .ok { m with mask := mask.set! k mode }
  | none => .error (.indexOutOfRange "_mask" i m.mask.size)

/-- `Clear()`: zero every physical colour and mask slot (`_colors.Length` iterations). -/
def clear (m : LightMap) : LightMap :=
  { m with colors := Array.replicate m.colors.size Vector3.zero,
           mask := Array.replicate m.colors.size 0 }

/-- `SetSize(width, height)`: new zeroed arrays only when `width * height` (wrapping) exceeds
`_colors.Length`; otherwise the old contents stay. Then `Width`, `Height`. -/
def setSize (m : LightMap) (width height : Int32) : LightMap :=
  let n := width * height
  let m := if n.toInt > (m.colors.size : Int) then
      { m with colors := Array.replicate n.toInt.toNat Vector3.zero,
               mask := Array.replicate n.toInt.toNat 0 }
    else m
  { m with width, height }

/-! ### `BlurLine` -/

/-- One channel of one `BlurLine` step: `(running, flag, cell) ↦ (running', flag', cell')`. -/
@[inline] def blurChannel (acc : Float32) (flag : Bool) (cell : Float32) : Float32 × Bool × Float32 :=
  if acc < cell then (cell, false, cell)
  else if !flag then
    if acc < blurThreshold then (acc, true, cell) else (acc, flag, acc)
  else (acc, flag, cell)

/-- The decay applied to the running value after a cell (`IL_0147‥0302`). -/
@[inline] def decayStep (m : LightMap) (mode : LightMaskMode) (index : Int32) (acc : Vector3)
    (fx fy fz : Bool) : Vector3 :=
  let apply (fx : Bool) (a k : Float32) : Float32 := if fx then a else a * k
  match mode.toNat with
  | 0 => let k := m.lightDecayThroughAir
         ⟨apply fx acc.x k, apply fy acc.y k, apply fz acc.z k⟩
  | 1 => let k := m.lightDecayThroughSolid
         ⟨apply fx acc.x k, apply fy acc.y k, apply fz acc.z k⟩
  | 2 =>
    let (n, _) := (m.random.withModifier index.toInt64.toUInt64).nextRange 98 100
    let r := n.toFloat32 / 100
    let w := m.lightDecayThroughWater
    ⟨apply fx acc.x (w.x * r), apply fy acc.y (w.y * r), apply fz acc.z (w.z * r)⟩
  | 3 => let w := m.lightDecayThroughHoney
         ⟨apply fx acc.x w.x, apply fy acc.y w.y, apply fz acc.z w.z⟩
  | 4 => let k := m.lightDecayThroughCrackedBrick
         ⟨apply fx acc.x k, apply fy acc.y k, apply fz acc.z k⟩
  | _ => acc

/-- The loop of `BlurLine(start, end, stride)`: `for (i = start; i != end + stride; i += stride)`.
`fuel` bounds the iterations (an in-range progression with `stride ≠ 0` leaves the array
within `colors.size` steps). `m` supplies decay, mask and random only. -/
def blurLoop (m : LightMap) (terminal stride : Int32) :
    Nat → Int32 → Vector3 → Bool → Bool → Bool → Array Vector3 → Except Fault (Array Vector3)
  | 0, _, _, _, _, _, _ => .error .nonTerminating
  | fuel + 1, i, acc, fx, fy, fz, colors =>
    if i == terminal then .ok colors else
    match slot colors.size i with
    | none => .error (.indexOutOfRange "_colors" i colors.size)
    | some k =>
      let cell := colors[k]!
      let (ax, fx, cx) := blurChannel acc.x fx cell.x
      let (ay, fy, cy) := blurChannel acc.y fy cell.y
      let (az, fz, cz) := blurChannel acc.z fz cell.z
      let colors := colors.set! k ⟨cx, cy, cz⟩
      let acc : Vector3 := ⟨ax, ay, az⟩
      if fx && fz && fy then blurLoop m terminal stride fuel (i + stride) acc fx fy fz colors
      else
        match slot m.mask.size i with
        | none => .error (.indexOutOfRange "_mask" i m.mask.size)
        | some j =>
          let acc := decayStep m m.mask[j]! i acc fx fy fz
          blurLoop m terminal stride fuel (i + stride) acc fx fy fz colors

/-- `BlurLine(startIndex, endIndex, stride)` on the colour array `colors`. -/
def blurLine (m : LightMap) (colors : Array Vector3) (start stop stride : Int32) :
    Except Fault (Array Vector3) :=
  blurLoop m (stop + stride) stride (colors.size + 2) start Vector3.zero false false false colors

/-- The column loop of `BlurPass` (`<BlurPass>b__46_0` over `[0, Width)`, ascending):
per column the forward line `(i, 0) → (i, Height - 1 - pad)` then the backward line
`(i, Height - 1) → (i, pad)`. -/
def columnLines (m : LightMap) (colors : Array Vector3) : Except Fault (Array Vector3) := do
  let h := m.height
  let pad := m.nonVisiblePadding
  let mut colors := colors
  for k in [0:m.width.toInt.toNat] do
    let i := Int32.ofNat k
    colors ← blurLine m colors (m.indexOf i 0) (m.indexOf i (h - 1 - pad)) 1
    colors ← blurLine m colors (m.indexOf i (h - 1)) (m.indexOf i pad) (-1)
  return colors

/-- The row loop of `BlurPass` (`<BlurPass>b__46_1` over `[0, Height)`, ascending): per row
the forward line `(0, j) → (Width - 1 - pad, j)` (stride `Height`) then the backward line
`(Width - 1, j) → (pad, j)` (stride `-Height`). -/
def rowLines (m : LightMap) (colors : Array Vector3) : Except Fault (Array Vector3) := do
  let w := m.width
  let h := m.height
  let pad := m.nonVisiblePadding
  let mut colors := colors
  for k in [0:h.toInt.toNat] do
    let j := Int32.ofNat k
    colors ← blurLine m colors (m.indexOf 0 j) (m.indexOf (w - 1 - pad) j) h
    colors ← blurLine m colors (m.indexOf (w - 1) j) (m.indexOf pad j) (-h)
  return colors

/-- `Blur()` with `FastParallel.ForceTasksOnCallingThread = true`: every `For` range is one
task on the calling thread (ascending; a negative range is empty). Exact for every input. -/
def blurSingleTask (m : LightMap) : Except Fault LightMap := do
  let mut colors := m.colors
  let shell := { m with colors := #[] }
  for _ in [0:2] do
    colors ← columnLines shell colors
    colors ← rowLines shell colors
  return { shell with colors, random := shell.random.nextSeed }

/-- `Blur()` as the game runs it (multi-task `FastParallel.For`): `For(0, Width)` columns,
`For(0, Height)` rows, twice, then `_random.NextSeed()`. Faults in native order: a negative
range throws when its `For` starts; padding outside `[0, Height]` with several columns makes
column lines overlap across threads (schedule-dependent, refused). Otherwise equal to
`blurSingleTask` (lines are disjoint, so the schedule cannot matter). -/
def blur (m : LightMap) : Except Fault LightMap := do
  let mut colors := m.colors
  let shell := { m with colors := #[] }
  for _ in [0:2] do
    if shell.width < 0 then throw (.negativeParallelRange shell.width)
    if shell.width > 1 && (shell.nonVisiblePadding < 0 || shell.nonVisiblePadding > shell.height) then
      throw (.scheduleDependent shell.nonVisiblePadding shell.height)
    colors ← columnLines shell colors
    if shell.height < 0 then throw (.negativeParallelRange shell.height)
    colors ← rowLines shell colors
  return { shell with colors, random := shell.random.nextSeed }

end LightMap

/-! ## `LightingEngine` pieces that touch a `LightMap` -/

/-- `LightingEngine.PerFrameLight`. -/
structure PerFrameLight where
  x : Int32
  y : Int32
  color : Vector3
deriving Inhabited, Repr

/-- `LightingEngine/EngineState`: 0 MinimapUpdate, 1 ExportMetrics, 2 Scan, 3 Blur. -/
abbrev EngineState := Int32

/-- Fields of `Terraria.Graphics.Light.LightingEngine` except `_tileScanner` (pending). -/
structure Engine where
  perFrameLightSwapUpdateCount : UInt32
  perFrameLights : Array PerFrameLight
  oldPerFrameLights : Array PerFrameLight
  activeLightMap : LightMap
  activeProcessedArea : Rectangle
  workingLightMap : LightMap
  workingProcessedArea : Rectangle
  state : EngineState
deriving Inhabited

def areaPadding : Int32 := 28
def nonVisiblePadding : Int32 := 18

/-- Native inputs of `UpdateLightDecay`: `Main.waterStyle`, the four flags of
`Main.SceneMetrics.PerspectivePlayer`, and `Main.SceneState.airLightDecay/solidLightDecay`. -/
structure DecayInputs where
  waterStyle : Int32
  nightVision : Bool
  blind : Bool
  blackout : Bool
  headcovered : Bool
  airLightDecay : Float32
  solidLightDecay : Float32
deriving Inhabited, Repr

/-- `UpdateLightDecay` applied to the working map. Water styles 11 and `≥ 14`/negative leave
`LightDecayThroughWater` as it was; `LightDecayThroughCrackedBrick` is never touched. -/
def updateLightDecay (m : LightMap) (i : DecayInputs) : LightMap := Id.run do
  let mut m := { m with lightDecayThroughAir := 0.91, lightDecayThroughSolid := 0.56,
                        lightDecayThroughHoney := Vector3.scale ⟨0.75, 0.7, 0.6⟩ 0.91 }
  let water : Option Vector3 := match i.waterStyle.toInt with
    | 0 | 1 | 7 | 8 => some ⟨0.88, 0.96, 1.015⟩
    | 2 => some ⟨0.94, 0.85, 1.01⟩
    | 3 => some ⟨0.84, 0.95, 1.015⟩
    | 4 => some ⟨0.9, 0.86, 1.01⟩
    | 5 => some ⟨0.84, 0.99, 1.01⟩
    | 6 => some ⟨0.83, 0.93, 0.98⟩
    | 9 => some ⟨1, 0.88, 0.84⟩
    | 10 => some ⟨0.83, 1, 1⟩
    | 12 => some ⟨0.95, 0.98, 0.85⟩
    | 13 => some ⟨0.9, 1, 1.02⟩
    | _ => none
  if let some w := water then m := { m with lightDecayThroughWater := Vector3.scale w 0.91 }
  let both (m : LightMap) (k : Float32) : LightMap :=
    { m with lightDecayThroughAir := m.lightDecayThroughAir * k,
             lightDecayThroughSolid := m.lightDecayThroughSolid * k }
  if i.nightVision then m := both m 1.03
  if i.blind then m := both m 0.95
  if i.blackout then m := both m 0.85
  if i.headcovered then m := both m 0.85
  m := { m with lightDecayThroughAir := m.lightDecayThroughAir * i.airLightDecay }
  m := { m with lightDecayThroughSolid := m.lightDecayThroughSolid * i.solidLightDecay }
  return m

namespace Engine

/-- `.ctor()` given the two `CreateWithRandomSeed` seeds (active, then working). -/
def create (activeSeed workingSeed : FastRandom) : Engine :=
  { perFrameLightSwapUpdateCount := 0, perFrameLights := #[], oldPerFrameLights := #[],
    activeLightMap := LightMap.create activeSeed, activeProcessedArea := Rectangle.empty,
    workingLightMap := LightMap.create workingSeed, workingProcessedArea := Rectangle.empty,
    state := 0 }

/-- `Rebuild()` (lists and swap count are kept). -/
def rebuild (e : Engine) (activeSeed workingSeed : FastRandom) : Engine :=
  { e with activeProcessedArea := Rectangle.empty, workingProcessedArea := Rectangle.empty,
           state := 0, activeLightMap := LightMap.create activeSeed,
           workingLightMap := LightMap.create workingSeed }

/-- `Clear()`. -/
def clear (e : Engine) : Engine :=
  { e with activeLightMap := e.activeLightMap.clear, workingLightMap := e.workingLightMap.clear,
           perFrameLights := #[], oldPerFrameLights := #[], perFrameLightSwapUpdateCount := 0 }

/-- `AddLight(x, y, color)`. -/
def addLight (e : Engine) (x y : Int32) (color : Vector3) : Engine :=
  { e with perFrameLights := e.perFrameLights.push ⟨x, y, color⟩ }

/-- `GetColor(x, y)`: zero outside `_activeProcessedArea`, else the active map cell. -/
def getColor (e : Engine) (x y : Int32) : Except Fault Vector3 :=
  let a := e.activeProcessedArea
  if !a.contains x y then .ok Vector3.zero else e.activeLightMap.get (x - a.x) (y - a.y)

/-- `IncrementState()`. -/
def incrementState (e : Engine) : Engine := { e with state := (e.state + 1) % 4 }

/-- The map half of `ProcessScan(area)`: `area.Inflate(28, 28)`, `_workingProcessedArea`,
`SetSize`, `NonVisiblePadding = 18`. `TileLightScanner.Update` / `ExportTo` must follow
(pending provider; `exportCell` is its per-cell write). -/
def beginScan (e : Engine) (area : Rectangle) : Engine :=
  let area := area.inflate areaPadding areaPadding
  let m := (e.workingLightMap.setSize area.width area.height)
  { e with workingProcessedArea := area, workingLightMap := { m with nonVisiblePadding } }

/-- What `TileLightScanner.ExportTo` writes for world tile `(x, y)` of the scanned area:
`SetMaskAt` then `set_Item`, both at `(x - area.X, y - area.Y)`. -/
def exportCell (e : Engine) (x y : Int32) (mode : LightMaskMode) (color : Vector3) :
    Except Fault Engine := do
  let a := e.workingProcessedArea
  let m := e.workingLightMap
  let e := { e with workingLightMap := default }
  let m ← m.setMaskAt (x - a.x) (y - a.y) mode
  let m ← m.set (x - a.x) (y - a.y) color
  return { e with workingLightMap := m }

/-- `ExportTo`'s branch for a tile that is null, touches a null tile or is not `InWorld(x, y, 1)`:
mask `None`, colour zero. (The lit-tile branch needs the pending tile scanner.) -/
def exportNullCell (e : Engine) (x y : Int32) : Except Fault Engine :=
  e.exportCell x y LightMaskMode.none Vector3.zero

/-- `ApplyPerFrameLights()` with `Main.GameUpdateCount` and `CaptureManager.Instance.IsCapturing`. -/
def applyPerFrameLights (e : Engine) (gameUpdateCount : UInt32) (isCapturing : Bool) :
    Except Fault Engine := do
  let list := if gameUpdateCount == e.perFrameLightSwapUpdateCount then e.oldPerFrameLights
    else e.perFrameLights
  let a := e.workingProcessedArea
  let mut m := e.workingLightMap
  let mut e := { e with workingLightMap := default }
  for light in list do
    if a.contains light.x light.y then
      let current ← m.get (light.x - a.x) (light.y - a.y)
      m ← m.set (light.x - a.x) (light.y - a.y) (Vector3.max current light.color)
  e := { e with workingLightMap := m }
  if !isCapturing then
    if gameUpdateCount != e.perFrameLightSwapUpdateCount then
      e := { e with perFrameLights := e.oldPerFrameLights, oldPerFrameLights := e.perFrameLights }
    e := { e with perFrameLights := #[], perFrameLightSwapUpdateCount := gameUpdateCount }
  return e

/-- `ProcessBlur()`: `UpdateLightDecay`, `ApplyPerFrameLights`, `_workingLightMap.Blur()`. -/
def processBlur (e : Engine) (decay : DecayInputs) (gameUpdateCount : UInt32) (isCapturing : Bool) :
    Except Fault Engine := do
  let m := e.workingLightMap
  let e := { e with workingLightMap := updateLightDecay m decay }
  let e ← e.applyPerFrameLights gameUpdateCount isCapturing
  let m := e.workingLightMap
  let e := { e with workingLightMap := default }
  let m ← m.blur
  return { e with workingLightMap := m }

/-- `Present()`: swap active/working maps and processed areas. -/
def present (e : Engine) : Engine :=
  { e with activeLightMap := e.workingLightMap, workingLightMap := e.activeLightMap,
           activeProcessedArea := e.workingProcessedArea, workingProcessedArea := e.activeProcessedArea }

end Engine

/-! ## `Lighting` colour conversions (new-engine `LightMode`s only) -/

/-- `conv.i4` of a `Single`, defined only for finite values with truncation in range. -/
def convI4 (v : Float32) : Except Fault Int32 :=
  if v.isNaN || v.isInf || v < -2147483648 || v ≥ 2147483648 then .error (.runtimeDependentConversion v)
  else .ok v.toInt32

/-- FNA `Color(Vector3)`: each channel `(byte)Clamp(v * 255, 0, 255)`, alpha 255. Packed
`R | G << 8 | B << 16 | A << 24`. NaN channels are runtime-dependent (`conv.u1`). -/
def colorOfVector (v : Vector3) : Except Fault UInt32 := do
  let channel (c : Float32) : Except Fault UInt32 := do
    let s := c * 255
    let s := if s > 255 then 255 else s
    let s := if s < 0 then 0 else s
    if s.isNaN then throw (.runtimeDependentConversion s)
    return s.toUInt8.toUInt32
  let r ← channel v.x
  let g ← channel v.y
  let b ← channel v.z
  return r ||| (g <<< 8) ||| (b <<< 16) ||| (0xFF000000 : UInt32)

/-- FNA `Color.ToVector3()`. -/
def colorToVector (packed : UInt32) : Vector3 :=
  let c (s : UInt32) : Float32 := ((packed >>> s) &&& 255).toUInt8.toFloat32 / 255
  ⟨c 0, c 8, c 16⟩

/-- `Lighting.GetColor(int x, int y)` given `_activeEngine.GetColor(x, y)`, `Main.gameMenu` and
`Lighting.GlobalBrightness`: channels `(int)(v * (GlobalBrightness * 255))`, capped above at
255 only (negative values keep their sign bits), alpha `0xFF000000`. -/
def lightingGetColor (gameMenu : Bool) (globalBrightness : Float32) (v : Vector3) : Except Fault UInt32 := do
  if gameMenu then return 0xFFFFFFFF
  let k := globalBrightness * 255
  let r ← convI4 (v.x * k)
  let g ← convI4 (v.y * k)
  let b ← convI4 (v.z * k)
  let r := if r > 255 then 255 else r
  let g := if g > 255 then 255 else g
  let b := if b > 255 then 255 else b
  return (r ||| (g <<< 8) ||| (b <<< 16) ||| (-16777216 : Int32)).toUInt32

/-- `Lighting.GetColor(int x, int y, Color oldColor)` / `GetColor(Point, Color)` given the
engine colour: `new Color(engine * oldColor.ToVector3())`; `oldColor` itself in the menu. -/
def lightingGetColorTinted (gameMenu : Bool) (v : Vector3) (oldColor : UInt32) : Except Fault UInt32 :=
  if gameMenu then .ok oldColor else colorOfVector (v.mul (colorToVector oldColor))

/-- `Lighting.GetColor(Point)` given the engine colour: `new Color(engine * GlobalBrightness)`. -/
def lightingGetColorPoint (gameMenu : Bool) (globalBrightness : Float32) (v : Vector3) : Except Fault UInt32 :=
  if gameMenu then .ok 0xFFFFFFFF else colorOfVector (v.scale globalBrightness)

end Lean4Terria.OriginalLightMap
