import Lean4Terria.OriginalForestChatLayout

/-!
# Item icon colours and their clocks (Terraria 1.4.5.8 desktop)

What `ItemSlot.DrawItemIcon` reads besides the item's texture: `Item.GetAlpha` (all special types), `Item.GetColor`,
`ItemSlot.GetItemLight` (Disco items, `ItemID.Sets.ItemIconPulse`, hearts / mana stars), the glow-mask colours
(`Item.GetPhaseColor`, `ItemSlot.GetTrustyPalGlowColor`, 5146's Disco, 5669's copper lerp, 3858's frames) and the
vanilla globals they read with the code that advances them (`Main.DoUpdate_AnimateDiscoRGB`,
`DoUpdate_AnimateVisualPlayerAura`, `Main.DoDraw`'s `essScale`, `Player.UpdateMiscCounter`).

Pure; the drawing itself (`OriginalForestGuideCraftGridLayout.itemIcon`) uses these. Checked against vanilla
`ItemSlot.DrawItemIcon` draws (`.lake/original-forest-guide-grid/1.4.5.8/catalog-production`, `REPORT.md`).
Float32 and integer conversions are those of the IL (`conv.i4` / `conv.u1` truncate).
-/

namespace Lean4Terria.OriginalItemIcon

open OriginalForestChatLayout (Color white)

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def truncI (f : Float32) : Int := f.toInt32.toInt
private def natF (n : Nat) : Float32 := single n

/-- `(byte)value` of an int (`conv.u1`: the low 8 bits). -/
private def u8 (v : Int) : Nat := (v % 256).toNat

/-- `Color * float` (FNA): `(int)(channel * scale)` per channel, clamped. -/
def scale (c : Color) (s : Float32) : Color :=
  Color.mk' (truncI (natF c.r * s)) (truncI (natF c.g * s)) (truncI (natF c.b * s)) (truncI (natF c.a * s))

/-- `new Color(float r, float g, float b)`: `(byte)Clamp(x * 255, 0, 255)`, alpha 255. -/
def ofFloats (r g b : Float32) : Color :=
  let c (x : Float32) : Nat :=
    let v := x * 255
    (truncI (if v > 255 then 255 else if v < 0 then 0 else v)).toNat
  ⟨c r, c g, c b, 255⟩

def withAlpha (c : Color) (a : Nat) : Color := { c with a := a % 256 }

/-! ## The globals -/

/-- Vanilla globals the icons read. Defaults: `Main..cctor` / a fresh `Main` and `Player`. -/
structure IconRuntime where
  /-- `Main.DiscoR` / `DiscoG` / `DiscoB`, `Main.DiscoStyle` (instance field) -/
  discoR : Int := 255
  discoG : Int := 0
  discoB : Int := 0
  discoStyle : Nat := 0
  /-- `Main.gFade` (byte), its accumulator `gFader` and direction `gFadeDir` -/
  gFade : Nat := 0
  gFader : Float32 := 0
  gFadeDir : Nat := 1
  /-- `Main.essScale`, `Main.essDir` -/
  essScale : Float32 := 1
  essDir : Int := -1
  /-- `LocalPlayer.miscCounter` -/
  miscCounter : Int := 0
  /-- `Main.GlobalTimeWrappedHourly` -/
  globalTimeWrappedHourly : Float32 := 0
  /-- `LocalPlayer.AnyoneToSpectate()` -/
  anyoneToSpectate : Bool := false
  deriving Repr, Inhabited, BEq

/-- `Main.DoUpdate_AnimateDiscoRGB` (step 7). -/
def IconRuntime.animateDisco (r : IconRuntime) : IconRuntime := Id.run do
  let k := 7
  let mut r := r
  if r.discoStyle == 0 then
    r := { r with discoG := r.discoG + k }
    if r.discoG ≥ 255 then r := { r with discoG := 255, discoStyle := r.discoStyle + 1 }
  if r.discoStyle == 1 then
    r := { r with discoR := r.discoR - k }
    if r.discoR ≤ 0 then r := { r with discoR := 0, discoStyle := r.discoStyle + 1 }
  if r.discoStyle == 2 then
    r := { r with discoB := r.discoB + k }
    if r.discoB ≥ 255 then r := { r with discoB := 255, discoStyle := r.discoStyle + 1 }
  if r.discoStyle == 3 then
    r := { r with discoG := r.discoG - k }
    if r.discoG ≤ 0 then r := { r with discoG := 0, discoStyle := r.discoStyle + 1 }
  if r.discoStyle == 4 then
    r := { r with discoR := r.discoR + k }
    if r.discoR ≥ 255 then r := { r with discoR := 255, discoStyle := r.discoStyle + 1 }
  if r.discoStyle == 5 then
    r := { r with discoB := r.discoB - k }
    if r.discoB ≤ 0 then r := { r with discoB := 0, discoStyle := 0 }
  return r

/-- `Main.DoUpdate_AnimateVisualPlayerAura`: `gFader ± 0.1`, `gFade = (byte)gFader`, turning above 150 / below 100. -/
def IconRuntime.animateAura (r : IconRuntime) : IconRuntime :=
  if r.gFadeDir == 1 then
    let f := r.gFader + 0.1
    let g := u8 (truncI f)
    { r with gFader := f, gFade := g, gFadeDir := if g > 150 then 0 else 1 }
  else
    let f := r.gFader - 0.1
    let g := u8 (truncI f)
    { r with gFader := f, gFade := g, gFadeDir := if g < 100 then 1 else 0 }

/-- `Main.DoUpdate`'s two animations (IL_0879, IL_087e), in that order. -/
def IconRuntime.updateTick (r : IconRuntime) : IconRuntime := r.animateDisco.animateAura

/-- `Main.DoDraw` IL_139d … IL_13f3: unless `gamePaused`, `essScale += essDir * 0.01`, turning at 1 and 0.7. -/
def IconRuntime.drawTick (r : IconRuntime) (gamePaused : Bool) : IconRuntime := Id.run do
  if gamePaused then return r
  let mut r := { r with essScale := r.essScale + single r.essDir * 0.01 }
  if r.essScale > 1 then r := { r with essDir := -1, essScale := 1 }
  if r.essScale.toFloat < 0.7 then r := { r with essDir := 1, essScale := 0.699999988079071 }
  return r

/-- `Main.DoUpdate` IL_0066 … IL_0087: unless `Main.GlobalTimerPaused` (false in 1.4.5.8: only `Main..cctor` writes
it), `GlobalTimeWrappedHourly = (float)(gameTime.TotalGameTime.TotalSeconds % 3600.0)`. `TotalSeconds` is the game's
Mono `TimeSpan.get_TotalSeconds`: `(double)ticks * 1e-07` (not .NET's `ticks / 1e7`); `%` is the exact `fmod`; the
result is rounded to float (so 35,999,999,999 ticks give 3600.0). -/
def globalTimeOfTicks (totalGameTimeTicks : Nat) : Float32 :=
  let secondsPerTick := Float.ofBits 0x3e7ad7f29abcaf48       -- the double 1e-07 of the IL's ldc.r8
  let x := Float.ofNat totalGameTimeTicks * secondsPerTick
  -- fmod(x, 3600) for x ≥ 0: x − n·3600 with n = ⌊x / 3600⌋ is exact (Sterbenz), corrected if the quotient rounded
  let n := (x / 3600).floor
  let r := x - n * 3600
  let r := if r < 0 then r + 3600 else if r ≥ 3600 then r - 3600 else r
  r.toFloat32

/-- `DoUpdate`'s write of `GlobalTimeWrappedHourly` from the FNA `GameTime.TotalGameTime` ticks of this update. -/
def IconRuntime.updateTime (r : IconRuntime) (totalGameTimeTicks : Nat) : IconRuntime :=
  { r with globalTimeWrappedHourly := globalTimeOfTicks totalGameTimeTicks }

/-- `Player.UpdateMiscCounter`. -/
def IconRuntime.playerTick (r : IconRuntime) : IconRuntime :=
  let m := r.miscCounter + 1
  { r with miscCounter := if m ≥ 300 then 0 else m }

/-! ## `Item.GetAlpha` / `Item.GetColor` -/

/-- `Item.GetAlpha(newColor)` (1.4.5.8: `ItemID.Sets.BossBag`, the type switch IL_0027 … IL_0781, the generic tail). -/
def getAlpha (rt : IconRuntime) (type : Nat) (alpha : Nat) (bossBag : Bool) (c : Color) : Color :=
  let fade (a : Int) : Color := Color.mk' 255 255 255 a
  let rgba (r g b a : Int) : Color := Color.mk' r g b a
  let inRange (lo hi : Nat) := lo ≤ type && type ≤ hi
  let notAlpha : Float32 := (255 - natF alpha) / 255
  -- IL_0625: a base colour raised to the light colour per channel
  let raise (base : Color) : Color :=
    ⟨max base.r c.r, max base.g c.g, max base.b c.b, max base.a c.a⟩
  if bossBag then Color.lerp c white 0.4
  else if type == 1326 || type == 5335 then Color.lerp c white 0.75
  else if type == 5043 || type == 3065 || type == 4956 || type == 5669 then fade (c.a - alpha)
  else if type == 75 || type == 671 || type == 3858 || type == 6155 || type == 6173 ||
      inRange 119 122 || inRange 217 220 then rgba 255 255 255 255
  else if type == 501 then rgba 200 200 200 50
  else if type == 757 || type == 1306 || inRange 3456 3459 then rgba 255 255 255 200
  else if inRange 520 522 || inRange 547 549 || type == 575 || type == 1332 || inRange 3453 3455 || type == 3580 then
    rgba 255 255 255 50
  else if type == 58 || type == 184 || inRange 1734 1735 || inRange 1867 1868 then rgba 200 200 200 200
  else if type == 1572 then rgba 200 200 255 125
  else if type == 787 || type == 1260 then rgba 255 255 255 175
  else if type == 1826 then rgba 255 255 255 200
  else if type == 1508 then rgba 200 200 200 0
  else if type == 502 || type == 4988 then rgba 255 255 255 150
  else if type == 51 then rgba 255 255 255 0
  else if type == 1446 || inRange 1506 1507 || inRange 1543 1545 then rgba c.r c.g c.b rt.gFade
  else if inRange 198 203 || inRange 3764 3769 || inRange 4258 4259 || inRange 5535 5536 || inRange 5670 5671 then white
  else if inRange 2763 2765 || inRange 2782 2786 || type == 3522 then rgba 250 250 250 (255 - alpha)
  else if type == 3191 then rgba 250 250 250 200
  else if type == 3822 then scale (Color.lerp white c 0.5) notAlpha
  else if type == 4143 then scale (Color.lerp white c 0) notAlpha
  else if type == 4377 then raise ⟨50, 255, 50, 200⟩
  else if type == 4378 then raise ⟨50, 200, 255, 255⟩
  else if type == 4389 then raise ⟨255, 50, 125, 200⟩
  else if type == 5127 then raise ⟨150, 50, 250, 200⟩
  else if type == 5128 then raise (Color.mk' rt.discoR rt.discoG rt.discoB 200)
  else if type == 4354 then raise ⟨255, 150, 150, 200⟩
  else
    -- IL_0782: (int)(channel * ((255 - alpha) / 255f)), A = clamp(A - alpha)
    let f : Float32 := single (255 - alpha : Int) / 255
    Color.mk' (truncI (natF c.r * f)) (truncI (natF c.g * f)) (truncI (natF c.b * f)) ((c.a : Int) - alpha)

/-- `Item.GetColor(newColor)`: `item.color − (255 − c)` per channel, clamped. -/
def getColor (itemColor c : Color) : Color :=
  Color.mk' ((itemColor.r : Int) - (255 - c.r)) ((itemColor.g : Int) - (255 - c.g)) ((itemColor.b : Int) - (255 - c.b))
    ((itemColor.a : Int) - (255 - c.a))

/-! ## `ItemSlot.GetItemLight(ref color, ref scale, type, outInTheWorld false, lightScale 1)` -/

/-- The light colour and the icon scale factor. -/
def itemLight (rt : IconRuntime) (type : Nat) (iconPulse : Bool) (c : Color) : Color × Float32 := Id.run do
  let disco : Color := Color.mk' (u8 rt.discoR) (u8 rt.discoG) (u8 rt.discoB) 255
  let pulse (c : Color) (s : Float32) : Color :=
    let ch (x : Nat) : Nat := u8 (truncI (natF x * s))
    ⟨ch c.r, ch c.g, ch c.b, ch c.a⟩
  let mut c := c
  if [662, 663, 5444, 5450, 5643].contains type then c := scale disco 1
  if type == 5128 then return (scale disco 1, 1)
  if iconPulse then return (scale (pulse c rt.essScale) 1, rt.essScale)
  if type == 58 || type == 184 || type == 4143 then
    let s := rt.essScale * 0.25 + 0.75
    return (scale (pulse c s) 1, s)
  return (c, 1)

/-! ## Glow-mask colours -/

/-- `Utils.WrappedLerp` -/
def wrappedLerp (a b t : Float32) : Float32 :=
  let t := t * 2
  let t := if t > 1 then 2 - t else t
  a + (b - a) * t

/-- `Utils.GetLerpValue(from, to, t, clamped true)` -/
def lerpValue (lo hi t : Float32) : Float32 :=
  if lo < hi then (if t < lo then 0 else if t > hi then 1 else (t - lo) / (hi - lo))
  else (if t < hi then 1 else if t > lo then 0 else (t - lo) / (hi - lo))

/-- `Item.GetPhaseColorDirect(shoot)` for the colours `_phaseColors` holds. -/
def phaseColorDirect (shoot : Nat) : Color :=
  let (r, g, b) : Float32 × Float32 × Float32 := match shoot with
    | 1059 | 1065 => (0.1, 0.5, 1.2)
    | 1060 | 1066 => (1, 0.2, 0.1)
    | 1061 | 1067 => (0.1, 1, 0.2)
    | 1062 | 1068 => (0.8, 0.1, 1)
    | 1063 | 1069 => (0.8, 0.9, 1)
    | 1064 | 1070 => (0.8, 0.8, 0)
    | 1071 | 1072 => (0.9, 0.5, 0)
    | 1075 | 1076 => (1.05, 0.1, 0.4)
    | _ => (1, 1, 1)
  ofFloats r g b

/-- `Item._phaseColors` -/
def phaseColors : Array Color := #[1059, 1060, 1061, 1062, 1063, 1064, 1071, 1075, 1059].map phaseColorDirect

/-- `Utils.MultiLerp(percent, Color[])` -/
def multiLerp (percent : Float32) (cs : Array Color) : Color := Id.run do
  let step : Float32 := 1 / (natF cs.size - 1)
  let mut acc := step
  let mut i := 0
  while percent / acc > 1 && i < cs.size - 2 do
    acc := acc + step
    i := i + 1
  return Color.lerp cs[i]! cs[i + 1]! ((percent - step * natF i) / step)

/-- `Item.GetPhaseColor(shoot, forGlowMask)` -/
def phaseColor (rt : IconRuntime) (shoot : Int) (secondPass : Bool) : Color :=
  let s := shoot.toNat
  if 0 ≤ shoot && 1059 ≤ s && s ≤ 1076 then
    let k := s - 1059
    let idx? : Option Nat := if k < 6 then some k else if k < 12 then some (k - 6) else if k < 14 then some 6
      else if k < 16 then none else some 7
    match idx? with
    | some i => phaseColors[i]!
    | none => white
  else if s == 1101 || s == 1102 then
    let c := multiLerp (single rt.miscCounter / 300) phaseColors
    if secondPass then withAlpha c (truncI ((255 : Float32) * 0.5)).toNat else c
  else white

/-- `Main.hue2rgb(t, p, q)` (doubles) -/
def hue2rgb (t p q : Float) : Float :=
  let t := if t < 0 then t + 1 else t
  let t := if t > 1 then t - 1 else t
  if 6 * t < 1 then p + (q - p) * 6 * t
  else if 2 * t < 1 then q
  else if 3 * t < 2 then p + (q - p) * (0.6666666666666666 - t) * 6
  else p

/-- `Math.Round` (half to even) of a non-negative double, then `(byte)`. -/
private def roundByte (x : Float) : Nat :=
  let f := x.floor
  let i : Nat := f.toUInt64.toNat
  let d := x - f
  let r := if d < 0.5 then i else if d > 0.5 then i + 1 else if i % 2 == 0 then i else i + 1
  r % 256

/-- `Main.hslToRgb(hue, saturation, luminosity, alpha)` -/
def hslToRgb (hue sat lum : Float32) (a : Nat) : Color :=
  if sat == 0 then
    let v := roundByte (lum.toFloat * 255)
    ⟨v, v, v, a⟩
  else
    let q : Float := if lum.toFloat < 0.5 then lum.toFloat * (1 + sat.toFloat) else (lum + sat - lum * sat).toFloat
    let p : Float := 2 * lum.toFloat - q
    let h := hue.toFloat
    Color.mk' (roundByte (hue2rgb (h + 0.3333333333333333) p q * 255)) (roundByte (hue2rgb h p q * 255))
      (roundByte (hue2rgb (h - 0.3333333333333333) p q * 255)) a

/-- `x % 1` of a float (`rem`), for `x ≥ 0`. -/
private def frac (x : Float32) : Float32 := x - x.floor

/-- `ItemSlot.GetTrustyPalGlowColor()` -/
def trustyPalGlow (rt : IconRuntime) : Color :=
  let t := wrappedLerp 0 1 (frac (rt.globalTimeWrappedHourly / 3))
  let fadeIn := 0 + (1 - 0) * lerpValue 0.2 1 t
  scale (scale (hslToRgb (frac (0.04 + 0.07 * t)) 1 0.5 127) 0.5) fadeIn

end Lean4Terria.OriginalItemIcon
