import Lean4Terria.OriginalPlayerEquipment.Model

/-!
# OriginalPlayerArmorDraw.Basic — FNA value types, colour helpers and the typed draw command

Hand transcription (desktop 1.4.5.8 `Terraria.exe` d52c7ea7…, FNA.dll 46d201f5…) of the value
operations the player draw setup and layers use: FNA `Vector2`, `Rectangle`, `Color`
(`.ctor(int…)`, `.ctor(Vector3)`, `.ctor(Vector4)`, `Multiply`, `ToVector3/4`), `MathHelper.Clamp/Min`,
`Lighting.GetColorClamped`/`GetColor(int,int)`, `Player.GetImmuneAlpha(Pure)`,
`HairShaderDataSet.GetColor` (undyed branch). `Single` arithmetic is `Float32` (one rounding per IL op).
-/

namespace Lean4Terria.OriginalPlayerArmorDraw

/-- Why a call is outside the transcribed ordinary branch (never a silent default). -/
inductive OutOfScope
  | guard (name : String)
  | runtimeDependentConversion
  | indexOutOfRange (table : String)
  deriving Repr, BEq, Inhabited

abbrev M := Except OutOfScope

def oos (name : String) : M α := throw (.guard name)

/-- FNA `Vector2`. -/
structure Vector2 where
  x : Float32
  y : Float32
  deriving Repr, Inhabited

namespace Vector2
def zero : Vector2 := ⟨0, 0⟩
def add (a b : Vector2) : Vector2 := ⟨a.x + b.x, a.y + b.y⟩
def mul (a b : Vector2) : Vector2 := ⟨a.x * b.x, a.y * b.y⟩
def scale (a : Vector2) (s : Float32) : Vector2 := ⟨a.x * s, a.y * s⟩
def sub (a b : Vector2) : Vector2 := ⟨a.x - b.x, a.y - b.y⟩
/-- `Utils.Floor(Vector2)`: `new Vector2((int)v.X, (int)v.Y)`. -/
def floor (v : Vector2) : M Vector2 := do
  let cv (f : Float32) : M Float32 :=
    if f.isNaN || f >= 2147483648.0 || f <= -2147483904.0 then throw .runtimeDependentConversion
    else pure f.toInt32.toFloat32
  pure ⟨← cv v.x, ← cv v.y⟩
instance : Add Vector2 := ⟨add⟩
end Vector2

/-- FNA `Vector3`. -/
structure Vector3 where
  x : Float32
  y : Float32
  z : Float32
  deriving Repr, Inhabited

/-- FNA `Rectangle`. -/
structure Rect where
  x : Int32
  y : Int32
  w : Int32
  h : Int32
  deriving Repr, BEq, Inhabited

/-- FNA `Color` (packed RGBA bytes). -/
structure Color where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8
  deriving Repr, BEq, Inhabited

/-- `conv.i4` of a `Single` (defined only in range). -/
def convI4 (v : Float32) : M Int32 :=
  if v.isNaN || v >= 2147483648.0 || v <= -2147483904.0 then throw .runtimeDependentConversion
  else pure v.toInt32

/-- `conv.u1` of a `Single` already clamped to `[0, 255]` (NaN is JIT-defined). -/
def convU1 (v : Float32) : M UInt8 :=
  if v.isNaN || v >= 256.0 || v <= -1.0 then throw .runtimeDependentConversion else pure v.toUInt8

/-- `MathHelper.Clamp(float value, min, max)`. -/
def clampF (value min max : Float32) : Float32 :=
  let v := if value > max then max else value
  if v < min then min else v

/-- `MathHelper.Min(a, b)`: `a < b ? a : b`. -/
def minF (a b : Float32) : Float32 := if a < b then a else b

namespace Color
def transparent : Color := ⟨0, 0, 0, 0⟩
def white : Color := ⟨255, 255, 255, 255⟩
def clampByte (v : Int32) : UInt8 := (if v > 255 then 255 else if v < 0 then 0 else v).toNatClampNeg.toUInt8
/-- `.ctor(int r, int g, int b, int a)` (each `MathHelper.Clamp(_, 0, 255)`). -/
def ofInts (r g b a : Int32) : Color := ⟨clampByte r, clampByte g, clampByte b, clampByte a⟩
/-- `.ctor(int r, int g, int b)` (alpha 255). -/
def ofInts3 (r g b : Int32) : Color := ofInts r g b 255
def chan (b : UInt8) : Float32 := b.toFloat32 / 255
/-- `ToVector3()`. -/
def toVector3 (c : Color) : Vector3 := ⟨chan c.r, chan c.g, chan c.b⟩
/-- `.ctor(Vector3)`: `(byte)MathHelper.Clamp(v * 255f, 0, 255)` per channel, alpha 255. -/
def ofVector3 (v : Vector3) : M Color := do
  pure ⟨← convU1 (clampF (v.x * 255) 0 255), ← convU1 (clampF (v.y * 255) 0 255),
        ← convU1 (clampF (v.z * 255) 0 255), 255⟩
/-- `Color.Multiply(Color, float)`: `new Color((int)(R*s), (int)(G*s), (int)(B*s), (int)(A*s))`. -/
def multiply (c : Color) (s : Float32) : M Color := do
  pure (ofInts (← convI4 (c.r.toFloat32 * s)) (← convI4 (c.g.toFloat32 * s))
               (← convI4 (c.b.toFloat32 * s)) (← convI4 (c.a.toFloat32 * s)))
/-- `HairShaderDataSet.GetColor` undyed branch: `new Color(light.ToVector4() * hair.ToVector4())`. -/
def mulVector4 (l h : Color) : M Color := do
  let ch (a b : UInt8) : M UInt8 := convU1 (clampF (chan a * chan b * 255) 0 255)
  pure ⟨← ch l.r h.r, ← ch l.g h.g, ← ch l.b h.b, ← ch l.a h.a⟩
/-- `PackedValue > 0`. -/
def packedPositive (c : Color) : Bool := c.r != 0 || c.g != 0 || c.b != 0 || c.a != 0
end Color

/-- `TextureAssets` reference of a draw command (the texture object identity, not its pixels). -/
inductive Tex
  | armorHead (id : Int32)
  | armorBody (id : Int32)
  | femaleBody (id : Int32)
  | armorBodyComposite (id : Int32)
  | armorLeg (id : Int32)
  | players (skinVar part : Int32)
  | playerHair (id : Int32)
  | playerHairAlt (id : Int32)
  | glowMask (id : Int32)
  | extra (id : Int32)
  deriving Repr, BEq, Inhabited

/-- `Terraria.DataStructures.DrawData` as appended to `PlayerDrawSet.DrawDataCache`. -/
structure DrawData where
  texture : Tex
  position : Vector2
  sourceRect : Option Rect
  color : Color
  rotation : Float32
  origin : Vector2
  scale : Float32
  effect : UInt32
  layerDepth : Float32
  shader : Int32
  deriving Repr, Inhabited

end Lean4Terria.OriginalPlayerArmorDraw
