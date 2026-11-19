import Lean4Terria.OriginalCombat
import Lean4Terria.OriginalForestChatLayout

/-! Classic desktop 1.4.5.8 presentation of the ordinary Skeletron encounter. -/
namespace Lean4Terria.OriginalSkeletronPresentation

open OriginalCollision (Vector single)
open OriginalCombat (Actor)
open OriginalForestChatLayout (Font Sprite black draw drawWithShadow white)
open OriginalForestOldManCurse (Part headType handType)
open OriginalStarterMelee (Rect truncF32)

private def activeHead (actor : Actor) : Option Part := do
  let part ← actor.skeletron?
  guard (part.active && part.typeId == headType)
  return part

/-- BigProgressBarSystem keeps its target until it fails validation.
Acquisition uses the NPC hitbox inside the screen rectangle inflated by 5000 px. -/
def track (previous : Option Nat) (actors : Array Actor) (screen : Rect) (playerCenter : Vector) :
    Option Nat := Id.run do
  if let some slot := previous then
    return if (actors[slot]?.bind activeHead).isSome then previous else none
  let bounds : Rect := ⟨screen.x - 5000, screen.y - 5000, screen.w + 10000, screen.h + 10000⟩
  let mut closest : Float32 := (1 : Float32) / 0
  let mut result := none
  for slot in [:actors.size] do
    let some part := actors[slot]?.bind activeHead | continue
    let rect : Rect := ⟨truncF32 part.position.x, truncF32 part.position.y, part.width, part.height⟩
    if !bounds.intersects rect then continue
    let center := OriginalSkeletron.Part.center part
    let dx := center.x - playerCenter.x
    let dy := center.y - playerCenter.y
    let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
    if distance < closest then
      closest := distance
      result := some slot
  return result

/-- The audio selector tests a 10000-by-10000 rectangle around the center
of either active Skeletron part. The standard cue in 1.4.5.8 is Music_104. -/
def musicNearby (actors : Array Actor) (screen : Rect) : Bool :=
  actors.any fun actor =>
    (actor.skeletron?).any fun part =>
      let center := OriginalSkeletron.Part.center part
      part.active && (part.typeId == headType || part.typeId == handType) &&
        screen.intersects ⟨truncF32 center.x - 5000, truncF32 center.y - 5000, 10000, 10000⟩

inductive MusicCue where
  | dungeon | skeletron | outside
  deriving BEq, Repr

def musicCue (actors : Array Actor) (screen : Rect) (zoneDungeon : Bool)
    (worldWidth worldHeight : Nat) (worldSurface : Float) (centerY : Float32) : MusicCue :=
  if musicNearby actors screen then .skeletron
  else
    let size := single worldWidth / 4200
    let altitude := ((single (screen.y + screen.h / 2) / 16 - (65 + 10 * (size * size))).toFloat /
      (worldSurface / 5)).toFloat32
    if zoneDungeon && (altitude ≥ 1) && (centerY ≤ single ((worldHeight : Int) - 200) * 16) then
      .dungeon
    else .outside

structure MusicTrack where
  fade : Float32 := 0
  gain : Float32 := 0
  deriving Inhabited

private def MusicTrack.step (track : MusicTrack) (selected canFadeOut : Bool) : MusicTrack :=
  let fade := if selected then min 1 (track.fade + 0.005)
    else if canFadeOut then max 0 (track.fade - 0.005)
    else track.fade
  ⟨fade, if fade > 0 then track.fade else 0⟩

structure MusicMix where
  dungeon : MusicTrack := {}
  skeletron : MusicTrack := {}
  deriving Inhabited

/-- Main samples the incoming cue's fade before updating either track. The
volume sent to the audio backend uses each track's pre-update fade. -/
def MusicMix.step (mix : MusicMix) (cue : MusicCue) : MusicMix :=
  let canFadeOut := match cue with
    | .dungeon => mix.dungeon.fade > 0.25
    | .skeletron => mix.skeletron.fade > 0.25
    -- No other background cue is streamed by this desktop scene yet.
    | .outside => true
  ⟨mix.dungeon.step (cue == .dungeon) canFadeOut, mix.skeletron.step (cue == .skeletron) canFadeOut⟩

private def meterCenter (screenWidth screenHeight : Nat) : Float32 × Float32 :=
  (single screenWidth * 0.5, single screenHeight - 50)

def meter (screenWidth screenHeight : Nat) (life : Int) (lifeMax : Nat) : Array Sprite :=
  let (cx, cy) := meterCenter screenWidth screenHeight
  let x := single (truncF32 (cx - 228))
  let y := single (truncF32 (cy - 11))
  let frameX := x - 32
  let frameY := y - 24
  let filled := truncF32 (456 * (single life / single lifeMax))
  let length := filled - filled.tmod 2
  let bar := "Images/UI/UI_BossBar"
  #[
    draw bar 516 348 frameX frameY (some ⟨0, 174, 516, 58⟩) ⟨51, 51, 51, 51⟩ 0 0 0 1 1,
    draw bar 516 348 x y (some ⟨32, 140, 2, 22⟩) white 0 0 0 (single (length.tdiv 2)) 1,
    draw bar 516 348 (x + single (length - 2)) y (some ⟨32, 82, 2, 22⟩) white 0 0 0 1 1,
    draw bar 516 348 frameX frameY (some ⟨0, 0, 516, 58⟩) white 0 0 0 1 1,
    draw "Images/NPC_Head_Boss_19" 26 30 (frameX + 17) (frameY + 34)
      (some ⟨0, 0, 26, 30⟩) white 0 13 15 1 1
  ]

/-- DrawHealthText uses three separate Item_Stack strings and four two-pixel shadows. -/
def meterText (font : Font) (screenWidth screenHeight : Nat) (life : Int) (lifeMax : Nat) :
    Except String (Array Sprite) := do
  let (cx, cy) := meterCenter screenWidth screenHeight
  let cx := single (truncF32 (cx - 228) + 228)
  let cy := single (truncF32 (cy - 11) + 12)
  let mut sprites := #[]
  for (label, dx, align) in #[("/", (0 : Float32), (0.5 : Float32)),
      (toString life, -5, 1), (toString lifeMax, 5, 0)] do
    let (w, h) ← font.measure label
    sprites := sprites ++ (← drawWithShadow font label (cx + dx) cy white (w * align) (h * 0.5) 1 1)
  return sprites

end Lean4Terria.OriginalSkeletronPresentation
