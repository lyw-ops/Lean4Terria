import Lean4Terria.OriginalForestSky

/-!
Terraria 1.4.5.8 desktop sky lighting for ordinary dry daytime in the forest.
The default NextHorizonRenderer uses the previous draw's atmosphere for its
sunrise and sunset colors. The background pass receives the current atmosphere.
-/

namespace Lean4Terria.OriginalForestSkyLighting

open OriginalForestSky (Rgba)

structure DayView where
  time : Float
  moonPhase : Nat
  worldWidth : Nat
  worldSurface : Float
  screenY : Float32
  screenHeight : Nat
  previousAtmosphere : Float32

structure Lighting where
  beforeAtmosphere : Rgba
  color : Rgba
  atmosphere : Float32
  deriving BEq, Repr

private def byte (value : Float32) : Nat := value.toInt32.toInt.toNat

private def rgb (r g b : Float32) : Rgba := ⟨byte r, byte g, byte b, 255⟩

private def mapRgb (color : Rgba) (f : Nat → Nat) : Rgba :=
  ⟨f color.r, f color.g, f color.b, color.a⟩

private def clampUnit (value : Float32) : Float32 := max 0 (min 1 value)

private def dayBaseColor (time : Float) : Rgba :=
  if time < 13500 then
    let ramp := (time / 13500).toFloat32
    rgb (ramp * 230 + 25) (ramp * 220 + 35) (ramp * 220 + 35)
  else if time > 45900 then
    let ramp := (1 - (time / 54000 - 0.85) * 6.666666666666667).toFloat32
    rgb (ramp * 200 + 35) (ramp * 85 + 35) (ramp * 135 + 35)
  else if time > 37800 then
    let ramp := (1 - (time / 54000 - 0.7) * 6.666666666666667).toFloat32
    rgb (ramp * 20 + 235) (ramp * 135 + 120) (ramp * 85 + 170)
  else ⟨255, 255, 255, 255⟩

-- The midpoint colors of SunGradients.Sunrises and SunGradients.Sunsets.
private def sunrises : Array Rgba :=
  #[⟨68, 144, 149, 255⟩, ⟨106, 92, 142, 255⟩, ⟨69, 92, 102, 255⟩, ⟨138, 95, 155, 255⟩]

private def sunsets : Array Rgba :=
  #[⟨171, 103, 125, 255⟩, ⟨47, 23, 45, 255⟩, ⟨173, 93, 145, 255⟩, ⟨64, 32, 45, 255⟩]

private def blendHorizon (color swatch : Rgba) (strength : Float32) : Rgba :=
  let strength := clampUnit strength
  let blend (base tint : Nat) :=
    let channel := base.toFloat32
    byte (channel + ((max base tint).toFloat32 - channel) * strength)
  ⟨blend color.r swatch.r, blend color.g swatch.g, blend color.b swatch.b, color.a⟩

private def setBackColor (view : DayView) : Rgba :=
  let minimum := (#[19, 17, 15, 13, 11, 13, 15, 17] : Array Nat)[view.moonPhase]?.getD 15
  let color := mapRgb (dayBaseColor view.time) (max minimum)
  let time := view.time.toFloat32
  if time < 10800 then
    let morning := (view.moonPhase + 7) % sunrises.size
    let rising := clampUnit (time / 2700)
    let falling := 1 - clampUnit ((time - 3600) / 7200)
    blendHorizon color (sunrises[morning]?.getD color) (view.previousAtmosphere * (rising * falling))
  else if time > 43200 then
    let evening := view.moonPhase % sunsets.size
    let rising := clampUnit ((time - 43200) / 7200)
    let falling := 1 - clampUnit ((time - 50400) / 3600)
    blendHorizon color (sunsets[evening]?.getD color) (view.previousAtmosphere * (rising * falling * falling))
  else color

private def currentAtmosphere (view : DayView) : Float32 :=
  let widthScale := view.worldWidth.toFloat32 / 4200
  let widthScaleSquared := widthScale * widthScale
  let cameraCenter := view.screenY + (view.screenHeight / 2).toFloat32
  let height := cameraCenter / 16 - (65 + 10 * widthScaleSquared)
  clampUnit (height.toFloat / (view.worldSurface / 5)).toFloat32

/-- The two native writes to `ColorOfTheSkies`, and `atmo` as read by the
background pass. Use the unscaled camera and retain the result's atmosphere
as `previousAtmosphere` for the next draw. -/
def forestDay (view : DayView) : Lighting :=
  let beforeAtmosphere := setBackColor view
  let atmosphere := currentAtmosphere view
  let color := if atmosphere.toFloat ≤ 0.01 then ⟨0, 0, 0, 255⟩
    else mapRgb beforeAtmosphere (fun channel => byte (channel.toFloat32 * atmosphere))
  { beforeAtmosphere, color, atmosphere }

end Lean4Terria.OriginalForestSkyLighting
