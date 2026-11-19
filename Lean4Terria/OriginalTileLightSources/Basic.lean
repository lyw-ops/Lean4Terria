import Lean4Terria.OriginalLightMap
import Lean4Terria.OriginalTileLightMask

/-!
# OriginalTileLightSources.Basic — native value types and the scanner's static callees

Hand transcription of the callees `TileLightScanner` uses (desktop 1.4.5.8 `Terraria.exe`
d52c7ea7e4e312d8937fd20d1f2609f0d6bd26daddf5bfab2d106ca9077103e0, FNA.dll 46d201f5…):
`Tile` getters, FNA `Color`/`Vector4`/`MathHelper`, `Main.hslToRgb`/`hue2rgb`,
`WorldGen.paintColor`, `Utils.MultiplyRGBA`, `TorchID.TorchColor` with the 24 providers built by
`TorchID.Initialize`, `PortalHelper.GetPortalColor`, `LiquidRenderer.GetShimmerBaseColor`/
`GetShimmerWave`, `WorldGen.InWorld`/`SolidTile3`.

All `Single` arithmetic is `Float32` (one rounding per IL operation, the shipped Mono float32
path); `Double` arithmetic is `Float`.  Float literals were checked bit-for-bit against every
`ldc.r4`/`ldc.r8` operand of the dumped bodies (`claude/litcheck`).
-/

namespace Lean4Terria.OriginalTileLightSources

open Lean4Terria.OriginalLightMap (Vector3 FastRandom)

/-- A `Single` given by its exact IEEE bits (used by generated tables). -/
@[inline] def f32 (bits : UInt32) : Float32 := Float32.ofBits bits

/-- Native exception classes and runtime-dependent sites reached by these methods. -/
inductive Fault
  /-- `callvirt`/`ldfld` on a null `Tile` (an unallocated `Main.tile[x, y]`). -/
  | nullReference
  /-- `ldelem`/`Tile[,]::Get`/`byte[,]::Get` outside the real array bounds. -/
  | indexOutOfRange
  /-- `rem` by `Main.cageFrames == 0`. -/
  | divideByZero
  /-- `rem` of `Int32.MinValue` by `-1`. -/
  | arithmeticOverflow
  /-- `conv.i4`/`conv.u1` of a NaN or out-of-range float: unspecified by ECMA-335 (JIT dependent). -/
  | runtimeDependentConversion
  /-- `Main.rand == null` at `ApplySurfaceLight` IL_01f5: the game then constructs
  `new UnifiedRandom()` seeded from `Environment.TickCount` — not a function of the inputs. -/
  | mainRandNullTimeSeeded
  deriving Repr, BEq, DecidableEq, Inhabited

abbrev M := Except Fault

/-- `ldelem` on a `bool[]`. -/
def lookupBool (a : Array Bool) (i : Nat) : M Bool :=
  if h : i < a.size then .ok a[i] else .error .indexOutOfRange

/-- `ldelem.r4` on a `float[]`. -/
def lookupF32 (a : Array Float32) (i : Nat) : M Float32 :=
  if h : i < a.size then .ok a[i] else .error .indexOutOfRange

/-! ## `Terraria.Tile` -/

/-- `Terraria.Tile`: exactly the nine instance fields of the native class. -/
structure Tile where
  type : UInt16
  wall : UInt16
  liquid : UInt8
  sTileHeader : UInt16
  bTileHeader : UInt8
  bTileHeader2 : UInt8
  bTileHeader3 : UInt8
  frameX : Int16
  frameY : Int16
  deriving Repr, BEq, Inhabited

namespace Tile
/-- `active()`: `(sTileHeader & 32) == 32`. -/
def active (t : Tile) : Bool := t.sTileHeader &&& 32 == 32
/-- `inActive()`: `(sTileHeader & 64) == 64`. -/
def inActive (t : Tile) : Bool := t.sTileHeader &&& 64 == 64
/-- `nactive()`: `(sTileHeader & 96) == 32`. -/
def nactive (t : Tile) : Bool := t.sTileHeader &&& 96 == 32
/-- `color()`: `(byte)(sTileHeader & 31)`. -/
def color (t : Tile) : UInt8 := (t.sTileHeader &&& 31).toUInt8
/-- `wallColor()`: `(byte)(bTileHeader & 31)`. -/
def wallColor (t : Tile) : UInt8 := t.bTileHeader &&& 31
/-- `lava()`: `(bTileHeader & 96) == 32`. -/
def lava (t : Tile) : Bool := t.bTileHeader &&& 96 == 32
/-- `honey()`: `(bTileHeader & 96) == 64`. -/
def honey (t : Tile) : Bool := t.bTileHeader &&& 96 == 64
/-- `shimmer()`: `(bTileHeader & 96) == 96`. -/
def shimmer (t : Tile) : Bool := t.bTileHeader &&& 96 == 96
/-- `slope()`: `(byte)((sTileHeader & 28672) >> 12)`. -/
def slope (t : Tile) : UInt8 := ((t.sTileHeader &&& 28672) >>> 12).toUInt8
/-- `halfBrick()`: `(sTileHeader & 1024) == 1024`. -/
def halfBrick (t : Tile) : Bool := t.sTileHeader &&& 1024 == 1024
/-- `invisibleBlock()`: `(bTileHeader3 & 32) == 32`. -/
def invisibleBlock (t : Tile) : Bool := t.bTileHeader3 &&& 32 == 32
/-- `invisibleWall()`: `(bTileHeader3 & 64) == 64`. -/
def invisibleWall (t : Tile) : Bool := t.bTileHeader3 &&& 64 == 64
/-- The getter values the released `OriginalTileLightMask` leaf consumes. -/
def toMaskTile (t : Tile) : OriginalTileLightMask.Tile :=
  { active := t.active, typeId := t.type, invisibleBlock := t.invisibleBlock, inActive := t.inActive,
    slope := t.slope, liquid := t.liquid, lava := t.lava, honey := t.honey }
end Tile

/-- `Main.tile[x, y]` (`Tile[,]::Get`): a real tile, a native `null` (`none`), or the array's
`IndexOutOfRangeException`.  The caller must back it with its real grid. -/
abbrev Grid := Int32 → Int32 → M (Option Tile)

/-- Field access on the result of `Main.tile[x, y]`: `null` is a `NullReferenceException`. -/
def Grid.get! (grid : Grid) (x y : Int32) : M Tile := do
  match ← grid x y with
  | some t => pure t
  | none => throw .nullReference

/-! ## Conversions -/

/-- `conv.i4` of a `Single` that the JIT defines only in range. -/
def convI4F32 (v : Float32) : M Int32 :=
  if v.isNaN || v >= 2147483648.0 || v <= -2147483904.0 then throw .runtimeDependentConversion
  else pure v.toInt32

/-- `conv.i4` of a `Double`. -/
def convI4F64 (v : Float) : M Int32 :=
  if v.isNaN || v >= 2147483648.0 || v <= -2147483649.0 then throw .runtimeDependentConversion
  else pure v.toInt32

/-- `conv.u1` of a `Single` (`Utils.MultiplyRGBA`): truncation, defined for `[0, 256)`. -/
def convU1F32 (v : Float32) : M UInt8 :=
  if v.isNaN || v >= 256.0 || v <= -1.0 then throw .runtimeDependentConversion
  else pure v.toUInt8

/-- `conv.u1` of a `Double` (`Main.hslToRgb`). -/
def convU1F64 (v : Float) : M UInt8 :=
  if v.isNaN || v >= 256.0 || v <= -1.0 then throw .runtimeDependentConversion
  else pure v.toUInt8

/-- `Math.Round(double)` (Mono icall, `MidpointRounding.ToEven`). -/
def roundHalfEven (x : Float) : Float :=
  let f := x.floor
  let d := x - f
  if d < 0.5 then f
  else if d > 0.5 then f + 1
  else if (f / 2).floor * 2 == f then f else f + 1

/-- `rem` on two `Single`s with divisor `1` (`fmod`, exact; the result keeps the dividend's sign). -/
def frem1 (x : Float32) : Float32 :=
  let t := if x < 0 then x.ceil else x.floor
  let r := x - t
  if r == 0 then (if x.toBits &&& 0x80000000 != 0 then -0.0 else 0.0) else r

/-! ## FNA `Color`, `Vector4`, `MathHelper` -/

/-- `Microsoft.Xna.Framework.Color` (packed RGBA bytes). -/
structure Color where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8
  deriving Repr, BEq, Inhabited

namespace Color
/-- `Color.White`. -/
def white : Color := ⟨255, 255, 255, 255⟩
/-- `MathHelper.Clamp(int, 0, 255)` then `conv.u1`. -/
def clampByte (v : Int32) : UInt8 := (if v > 255 then 255 else if v < 0 then 0 else v).toNatClampNeg.toUInt8
/-- `.ctor(int r, int g, int b, int alpha)`. -/
def ofInts (r g b a : Int32) : Color := ⟨clampByte r, clampByte g, clampByte b, clampByte a⟩
/-- `ToVector3()`: `new Vector3(R / 255f, G / 255f, B / 255f)`. -/
def toVector3 (c : Color) : Vector3 := ⟨c.r.toFloat32 / 255, c.g.toFloat32 / 255, c.b.toFloat32 / 255⟩
/-- `op_Multiply(Color, float)`: `new Color((int)(R * s), (int)(G * s), (int)(B * s), (int)(A * s))`. -/
def scale (c : Color) (s : Float32) : M Color := do
  let r ← convI4F32 (c.r.toFloat32 * s)
  let g ← convI4F32 (c.g.toFloat32 * s)
  let b ← convI4F32 (c.b.toFloat32 * s)
  let a ← convI4F32 (c.a.toFloat32 * s)
  pure (ofInts r g b a)
end Color

/-- `Microsoft.Xna.Framework.Vector4`. -/
structure Vector4 where
  x : Float32
  y : Float32
  z : Float32
  w : Float32
  deriving Repr, Inhabited

/-- `MathHelper.Clamp(float value, float min, float max)`:
`value = value > max ? max : value; value = value < min ? min : value`. -/
def clampF32 (value min max : Float32) : Float32 :=
  let v := if value > max then max else value
  if v < min then min else v

/-- `MathHelper.Lerp(a, b, t)`: `a + (b - a) * t`. -/
def lerpF32 (a b t : Float32) : Float32 := a + (b - a) * t

namespace Vector4
/-- `Vector4.Lerp(a, b, t)`. -/
def lerp (a b : Vector4) (t : Float32) : Vector4 :=
  ⟨lerpF32 a.x b.x t, lerpF32 a.y b.y t, lerpF32 a.z b.z t, lerpF32 a.w b.w t⟩
/-- `op_Multiply(Vector4, float)` (W first natively; no channel interaction). -/
def scale (v : Vector4) (s : Float32) : Vector4 := ⟨v.x * s, v.y * s, v.z * s, v.w * s⟩
end Vector4

/-! ## `Main.hslToRgb` -/

/-- `Main.hue2rgb(double c, double p, double q)`. -/
def hue2rgb (c p q : Float) : Float :=
  let c := if c < 0 then c + 1 else c
  let c := if c > 1 then c - 1 else c
  if 6 * c < 1 then p + (q - p) * 6 * c
  else if 2 * c < 1 then q
  else if 3 * c < 2 then p + (q - p) * (0.6666666666666666 - c) * 6
  else p

/-- `Main.hslToRgb(float hue, float saturation, float luminosity, byte a = 255)`. -/
def hslToRgb (hue saturation luminosity : Float32) (a : UInt8) : M Color := do
  if saturation == 0 then
    let v ← convU1F64 (roundHalfEven (luminosity.toFloat * 255))
    return ⟨v, v, v, a⟩
  let q : Float := if luminosity.toFloat < 0.5 then luminosity.toFloat * (1 + saturation.toFloat)
    else (luminosity + saturation - luminosity * saturation).toFloat
  let p : Float := 2 * luminosity.toFloat - q
  let h := hue.toFloat
  let r := hue2rgb (h + 0.3333333333333333) p q
  let g := hue2rgb h p q
  let b := hue2rgb (h - 0.3333333333333333) p q
  let r ← convU1F64 (roundHalfEven (r * 255))
  let g ← convU1F64 (roundHalfEven (g * 255))
  let b ← convU1F64 (roundHalfEven (b * 255))
  -- `new Color(int, int, int, int)` clamps each byte value (no change in range).
  return Color.ofInts r.toNat.toInt32 g.toNat.toInt32 b.toNat.toInt32 a.toNat.toInt32

/-! ## `WorldGen.paintColor`, `Utils.MultiplyRGBA` -/

/-- `WorldGen.paintColor(int color)`: `Color.White`, then the native chain of independent
`if (color == k || color == k + 12)` setters (only one can match).  Only `30` changes alpha. -/
def paintColor (color : Int32) : Color :=
  let rgb (r g b : UInt8) : Color := ⟨r, g, b, 255⟩
  if color == 1 || color == 13 then rgb 255 0 0
  else if color == 2 || color == 14 then rgb 255 127 0
  else if color == 3 || color == 15 then rgb 255 255 0
  else if color == 4 || color == 16 then rgb 127 255 0
  else if color == 5 || color == 17 then rgb 0 255 0
  else if color == 6 || color == 18 then rgb 0 255 127
  else if color == 7 || color == 19 then rgb 0 255 255
  else if color == 8 || color == 20 then rgb 0 127 255
  else if color == 9 || color == 21 then rgb 0 0 255
  else if color == 10 || color == 22 then rgb 127 0 255
  else if color == 11 || color == 23 then rgb 255 0 255
  else if color == 12 || color == 24 then rgb 255 0 127
  else if color == 25 then rgb 75 75 75
  else if color == 26 then rgb 255 255 255
  else if color == 27 then rgb 175 175 175
  else if color == 28 then rgb 255 178 125
  else if color == 29 then rgb 25 25 25
  else if color == 30 then ⟨200, 200, 200, 150⟩
  else Color.white

/-- `Utils.MultiplyRGBA(Color, Color)`: `(byte)((float)(a * b) / 255f)` per channel. -/
def multiplyRGBA (c d : Color) : M Color := do
  let ch (u v : UInt8) : M UInt8 := convU1F32 ((u.toNat * v.toNat).toUInt32.toFloat32 / 255)
  let r ← ch c.r d.r
  let g ← ch c.g d.g
  let b ← ch c.b d.b
  let a ← ch c.a d.a
  return Color.ofInts r.toNat.toInt32 g.toNat.toInt32 b.toNat.toInt32 a.toNat.toInt32

/-! ## Dynamic world state the scanner reads -/

/-- `Main.rand` (a `UnifiedRandom`) as a required provider: the current object (`none` = null)
and its real `NextDouble()`.  `Utils.NextFloat(rand)` is `(float)rand.NextDouble()`. -/
structure MainRand (σ : Type) where
  state : Option σ
  nextDouble : σ → Float × σ

/-- Every static/global the five methods read, as the actual native values. Arrays are the real
(non-null) arrays: lookups fault exactly at their native bounds. -/
structure Context where
  /-- `Main.tileLighted`. -/
  tileLighted : Array Bool
  /-- `Main.tileNoSunLight`. -/
  tileNoSunLight : Array Bool
  /-- `Main.wallLight`. -/
  wallLight : Array Bool
  /-- `TileID.Sets.AllowLightInWater`. -/
  allowLightInWater : Array Bool
  /-- `Main.tileSolid`, `Main.tileSolidTop` (`WorldGen.SolidTile3`). -/
  tileSolid : Array Bool
  tileSolidTop : Array Bool
  /-- `Main.tileBlockLight`, `TileID.Sets.CrackedBricks`, scanner `_drawInvisibleWalls`. -/
  mask : OriginalTileLightMask.Context
  /-- `Main.liquidAlpha`. -/
  liquidAlpha : Array Float32
  /-- `Main.jellyfishCageMode` (`byte[,]`, row-major, `rows × cols`). -/
  jellyfishCageRows : Nat
  jellyfishCageCols : Nat
  jellyfishCageMode : Array UInt8
  /-- `Main.cageFrames`. -/
  cageFrames : Int32
  /-- `Main.worldSurface`, `Main.maxTilesX`, `Main.maxTilesY`. -/
  worldSurface : Float
  maxTilesX : Int32
  maxTilesY : Int32
  /-- `Main.tileColor`. -/
  tileColor : Color
  /-- `Main.mouseTextColor`. -/
  mouseTextColor : UInt8
  /-- `Main.DiscoR/G/B`. -/
  discoR : Int32
  discoG : Int32
  discoB : Int32
  /-- `Main.demonTorch`. -/
  demonTorch : Float32
  /-- `Main.GlobalTimeWrappedHourly`. -/
  globalTimeWrappedHourly : Float32
  /-- `Main.timeForVisualEffects`. -/
  timeForVisualEffects : Float
  /-- `Main.dayTime`, `Main.time`, `Main.raining`. -/
  dayTime : Bool
  time : Float
  raining : Bool
  /-- `Main.sundialCooldown`, `Main.moondialCooldown`. -/
  sundialCooldown : Int32
  moondialCooldown : Int32
  /-- `Main.shimmerDarken`. -/
  shimmerDarken : Float32
  /-- `Main.netMode`, `Main.myPlayer` (`PortalHelper.GetPortalColor`). -/
  netMode : Int32
  myPlayer : Int32

/-- `Main.UnderworldLayer`: `Main.maxTilesY - 200`. -/
def Context.underworldLayer (c : Context) : Int32 := c.maxTilesY - 200

/-- `Main.jellyfishCageMode[i, j]` (`byte[,]::Get`, both dimensions bound-checked). -/
def Context.jellyfishCage (c : Context) (i j : Int32) : M UInt8 :=
  if i < 0 || j < 0 || i.toNatClampNeg ≥ c.jellyfishCageRows || j.toNatClampNeg ≥ c.jellyfishCageCols then
    throw .indexOutOfRange
  else lookupU8 (i.toNatClampNeg * c.jellyfishCageCols + j.toNatClampNeg)
where
  lookupU8 (k : Nat) : M UInt8 :=
    if h : k < c.jellyfishCageMode.size then .ok c.jellyfishCageMode[k] else .error .indexOutOfRange

/-- `WorldGen.InWorld(x, y, fluff)`. -/
def Context.inWorld (c : Context) (x y fluff : Int32) : Bool :=
  !(x < fluff || x >= c.maxTilesX - fluff || y < fluff || y >= c.maxTilesY - fluff)

/-- `WorldGen.SolidTile3(int x, int y)`: `InWorld(x, y, 1)`, then `SolidTile3(Main.tile[x, y])`
(null ⇒ false; `nactive() && tileSolid[type] && !tileSolidTop[type]`). -/
def solidTile3 (c : Context) (grid : Grid) (x y : Int32) : M Bool := do
  if !c.inWorld x y 1 then return false
  match ← grid x y with
  | none => return false
  | some t =>
    if !t.nactive then return false
    if !(← lookupBool c.tileSolid t.type.toNat) then return false
    return !(← lookupBool c.tileSolidTop t.type.toNat)

/-! ## `TorchID.TorchColor` -/

/-- The providers `TorchID.Initialize` stores in `TorchID._lights` (24 = `TorchID.Count`). -/
inductive TorchLight
  | constant (r g b : Float32)
  | demon
  | disco
  | shimmer
  deriving Inhabited

/-- `TorchID._lights` exactly as `TorchID.Initialize` (token 100671653) builds it. -/
def torchLights : Array TorchLight := #[
  .constant 1 0.95 0.8,      -- 0  torch
  .constant 0 0.1 1.3,       -- 1  blue
  .constant 1 0.1 0.1,       -- 2  red
  .constant 0 1 0.1,         -- 3  green
  .constant 0.9 0 0.9,       -- 4  purple
  .constant 1.4 1.4 1.4,     -- 5  white
  .constant 0.9 0.9 0,       -- 6  yellow
  .demon,                    -- 7  demon (DemonTorchLight)
  .constant 1 1.6 0.5,       -- 8  cursed
  .constant 0.75 0.85 1.4,   -- 9  ice
  .constant 1 0.5 0,         -- 10 orange
  .constant 1.4 1.4 0.7,     -- 11 ichor
  .constant 0.75 1.3499999 1.5, -- 12 ultrabright
  .constant 0.95 0.75 1.3,   -- 13 bone
  .disco,                    -- 14 rainbow (DiscoTorchLight)
  .constant 1 0 1,           -- 15 pink
  .constant 1.4 0.85 0.55,   -- 16 desert
  .constant 0.25 1.3 0.8,    -- 17 coral
  .constant 0.95 0.4 1.4,    -- 18 corrupt
  .constant 1.4 0.7 0.5,     -- 19 crimson
  .constant 1.25 0.6 1.2,    -- 20 hallowed
  .constant 0.75 1.45 0.9,   -- 21 jungle
  .constant 0.3 0.78 1.2,    -- 22 mushroom
  .shimmer                   -- 23 aether (ShimmerTorchLight)
]

/-- `ITorchLightProvider.GetRGB(out r, out g, out b)` of each provider kind. -/
def TorchLight.getRGB (c : Context) : TorchLight → Float32 × Float32 × Float32
  | .constant r g b => (r, g, b)
  | .demon =>
    -- r = 0.5f * demonTorch + (1f - demonTorch); g = 0.3f; b = demonTorch + 0.5f * (1f - demonTorch)
    (0.5 * c.demonTorch + (1 - c.demonTorch), 0.3, c.demonTorch + 0.5 * (1 - c.demonTorch))
  | .disco => (c.discoR.toFloat32 / 255, c.discoG.toFloat32 / 255, c.discoB.toFloat32 / 255)
  | .shimmer =>
    let r : Float32 := 0.9 + (270 - c.mouseTextColor.toNat.toInt32).toFloat32 / 900
    let g : Float32 := 0.9 + (270 - c.mouseTextColor.toNat.toInt32).toFloat32 / 125
    let r := clampF32 r 0 1
    let g := clampF32 g 0 1
    (r * 0.9, g * 0.55, r * 1.2)

/-- `TorchID.TorchColor(int torchID, out float R, out float G, out float B)`: outside
`[0, _lights.Length)` all three outputs are `0`. -/
def torchColor (c : Context) (torchID : Int32) : Float32 × Float32 × Float32 :=
  if torchID < 0 || torchID.toNatClampNeg ≥ torchLights.size then (0, 0, 0)
  else (torchLights[torchID.toNatClampNeg]!).getRGB c

/-! ## `PortalHelper.GetPortalColor`, `LiquidRenderer.GetShimmerBaseColor` -/

/-- `PortalHelper.GetPortalColor(int colorIndex /* player */, int portal)`; alpha is set to 66. -/
def getPortalColor (c : Context) (player portal : Int32) : M Color := do
  let col ← if c.netMode == 0 then
      if portal == 0 then hslToRgb 0.12 1 0.5 255 else hslToRgb 0.52 1 0.6 255
    else
      let step : Float32 := 0.08
      hslToRgb (frem1 (0.5 + player.toFloat32 * (step * 2) + portal.toFloat32 * step)) 1 0.5 255
  return { col with a := 66 }

/-- `LiquidRenderer.GetShimmerWave(ref float x, ref float y)`:
`(float)Math.Sin(((double)((x + y / 6f) / 10f) - timeForVisualEffects / 360.0) * 6.2831854820251465)`. -/
def getShimmerWave (c : Context) (worldPositionX worldPositionY : Float32) : Float32 :=
  let a : Float32 := (worldPositionX + worldPositionY / 6) / 10
  (Float.sin ((a.toFloat - c.timeForVisualEffects / 360) * 6.2831854820251465)).toFloat32

/-- `LiquidRenderer.GetShimmerBaseColor(float worldPositionX, float worldPositionY)`. -/
def getShimmerBaseColor (c : Context) (worldPositionX worldPositionY : Float32) : Vector4 :=
  let wave := getShimmerWave c worldPositionX worldPositionY
  Vector4.lerp ⟨0.64705884, 0.50980395, 0.93333334, 1⟩ ⟨0.8039216, 0.8039216, 1, 1⟩ (0.1 + wave * 0.4)

/-! ## Arms of the five large frame switches (data in `Arms.lean`) -/

/-- Hand-written arms of the lantern switch (IL_3a36, IL_3a7a). -/
inductive Special
  | lantern3a36
  | lantern3a7a
  deriving Repr, BEq

/-- One switch arm. `const` assigns the listed locals (`none` = not assigned) and optionally sets
the paint flag (`V_3 = true`); the others are the recurring arm shapes of those switches. -/
inductive Arm
  /-- `r/g/b := literal` for each `some`; `flag := true` when the arm sets `V_3`. -/
  | const (r g b : Option Float32) (paint : Bool)
  /-- `if (tile.color() == 0) (r, g, b) := … else V_3 = true`. -/
  | paintedOr (r g b : Float32)
  /-- `r = A*demonTorch + B*(C - demonTorch); g = G; b = D*demonTorch + E*(F - demonTorch)`. -/
  | demonBlend (a b c g d e f : Float32)
  /-- `(r, g, b) = Main.hslToRgb(demonTorch*K + H, S, L, 255).ToVector3() * M`. -/
  | demonHsl (k h s l m : Float32)
  /-- `GetShimmerBaseColor((float)x, (float)y) * M`, X/Y/Z each `MathHelper.Clamp(_, 0, 1)`. -/
  | shimmerClamped (m : Float32)
  /-- `TorchID.TorchColor(id, out r, out g, out b)`. -/
  | torch (id : Int32)
  | special (s : Special)
  deriving Inhabited

end Lean4Terria.OriginalTileLightSources
