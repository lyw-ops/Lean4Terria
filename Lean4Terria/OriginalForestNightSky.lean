import Lean4Terria.OriginalForestSkyLighting

/-!
Terraria 1.4.5.8 Classic dry sky at night with the default horizon renderer.
The colors correspond to SetBackColor and UpdateAtmosphereTransparencyToSkyColor.
The moon is the ordinary DrawSunAndMoon sprite before the background transform.
-/

namespace Lean4Terria.OriginalForestNightSky

open OriginalCollision (Vector single)
open OriginalForestSky (Rect Rgba)
open OriginalForestSkyLighting (DayView Lighting)

structure View extends DayView where
  dayTime : Bool
  moonType : Nat
  screenWidth : Nat

structure MoonDraw where
  texture : Nat
  source : Rect
  position : Vector
  origin : Vector
  rotation : Float32
  scale : Float32
  tint : Rgba
  normalizedPosition : Vector
  deriving BEq, Repr

def MoonDraw.texturePath (draw : MoonDraw) : String :=
  s!"Images/Moon_{draw.texture}"

structure Frame where
  lighting : Lighting
  moon : Option MoonDraw
  deriving BEq, Repr

private def byte (value : Float32) : Nat := value.toInt32.toInt.toNat

private def nightBaseColor (time : Float) : Rgba :=
  if time < 16200 then
    let ramp := (1 - time / 16200).toFloat32
    let channel := byte (ramp * 30 + 5)
    ⟨channel, channel, channel, 255⟩
  else
    let ramp := ((time / 32400 - 0.5) * 2).toFloat32
    let other := byte (ramp * 30 + 5)
    ⟨byte (ramp * 20 + 5), other, other, 255⟩

/-- NextHorizonRenderer uses zero sunrise and sunset visibility while it is night. -/
def forestNight (view : DayView) : Lighting :=
  let minimum := (#[19, 17, 15, 13, 11, 13, 15, 17] : Array Nat)[view.moonPhase]?.getD 15
  let base := nightBaseColor view.time
  let beforeAtmosphere := ⟨max minimum base.r, max minimum base.g, max minimum base.b, 255⟩
  let widthScale := view.worldWidth.toFloat32 / 4200
  let center := view.screenY + (view.screenHeight / 2).toFloat32
  let altitude := center / 16 - (65 + 10 * (widthScale * widthScale))
  let atmosphere := max 0 (min 1 (altitude.toFloat / (view.worldSurface / 5)).toFloat32)
  let scale (channel : Nat) := byte (channel.toFloat32 * atmosphere)
  let color := if atmosphere.toFloat ≤ 0.01 then ⟨0, 0, 0, 255⟩
    else ⟨scale beforeAtmosphere.r, scale beforeAtmosphere.g, scale beforeAtmosphere.b, 255⟩
  { beforeAtmosphere, color, atmosphere }

/-- SceneArea.bgTopY from the background pass camera. At identity background
zoom this uses the same screen Y as the atmosphere pass. -/
def backdropTop (view : DayView) : Int :=
  (-view.screenY / (view.worldSurface.toFloat32 * 16 - 600) * 200).toInt32.toInt

/-- Plain moon sprite. Its atlas has eight vertically stacked square frames.
The sprite position is its center and can be beyond the viewport edges. -/
def drawMoon (view : View) : Option MoonDraw := do
  if view.dayTime || (view.screenY / 16).toFloat ≥ view.worldSurface + 2 then
    return ← none
  let width := view.screenWidth.toFloat32
  let height := view.screenHeight.toFloat32
  let frameSize : Nat := 50
  let fraction := view.time / 32400
  let span := width + (2 * frameSize).toFloat32
  let x := single ((fraction * span.toFloat).toInt64.toInt - (frameSize : Int))
  let distance := (fraction * 2 - 1).abs
  let arc := distance.pow 2
  let y := single (((backdropTop view.toDayView).toFloat + arc * 250 + 180).toInt64.toInt)
  let half := (frameSize / 2).toFloat32
  return {
    texture := min view.moonType 8
    source := ⟨0, view.moonPhase * frameSize, frameSize, frameSize⟩
    position := ⟨x, y⟩
    origin := ⟨half, half⟩
    rotation := fraction.toFloat32 * 2 - 7.3
    scale := (1.2 - arc * 0.4).toFloat32
    tint := ⟨255, 255, 255, 255⟩
    normalizedPosition := ⟨x / width, y / height⟩
  }

/-- Dry day/night lighting and the ordinary moon at identity background zoom.
Feed lighting.atmosphere into the next drawing frame's previousAtmosphere. -/
def frame (view : View) : Frame :=
  let lighting := if view.dayTime then OriginalForestSkyLighting.forestDay view.toDayView
    else forestNight view.toDayView
  ⟨lighting, drawMoon view⟩

end Lean4Terria.OriginalForestNightSky
