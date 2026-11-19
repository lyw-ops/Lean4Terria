import Lean4Terria.OriginalTileHitEffects

/-! Global pickup sounds in desktop Terraria1.4.5.8. These helpers resolve the
complete SoundEngine/Legacy type7 and38 calls at x=y=-1, style1. The caller owns
GetItem's call count and timing, and the backend owns the native instance policy.
Evidence and boundaries: `.lake/original-pickup-sound/REPORT.md`. -/
namespace Lean4Terria.OriginalPickupSound

open OriginalTileHitEffects (Context SoundRequest PlayedSound)
open OriginalCollision (single)

private def clampPitch (value : Float32) : Float32 :=
  if value < -1 then -1 else if value > 1 then 1 else value

private def globalVolume (context : Context) : Option Float32 := do
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then none else
  let volume := 1 * context.soundVolume
  let volume := if volume > 1 then 1 else volume
  if volume > 0 then some volume else none

/-- Type7 uses Sounds/Grab and one pitch draw. Its backend stops the previous
Grab instance, creates a fresh one, then applies these fields and plays it.
Global coordinates bypass positioned generation/netMode/distance checks;
menu,pause,focus,ambient volume and tile-effect flags add no gate here. -/
def playGrab (context : Context) (random : UnifiedRandom) :
    Option PlayedSound × UnifiedRandom :=
  match globalVolume context with
  | none => (none,random)
  | some volume =>
    let (pitch,random) := random.next (-10) 11
    let request : SoundRequest := {typeId := 7,x := -1,y := -1}
    (some {
      request,variation := 0,volume := volume * request.volumeScale,pan := 0
      pitch := clampPitch (single pitch * 0.01 + request.pitchOffset)},random)

/-- Type38 selects Sounds/Coin_0..4, then its pitch. Every accepted call creates
a new overlapping instance, including repeated selections of the same asset;
there is no Stop or playing-state gate. Ordinary GetItem uses starGame=false.
The native minigame reduction happens after the positive-volume gate, so even
a reduction that underflows to0 still creates the sound and consumes both draws. -/
def playCoin (context : Context) (random : UnifiedRandom) (starGame : Bool := false) :
    Option PlayedSound × UnifiedRandom :=
  match globalVolume context with
  | none => (none,random)
  | some volume =>
    let volume := if starGame then volume * 0.15 else volume
    let (variation,random) := random.next 0 5
    let (pitch,random) := random.next (-40) 41
    let request : SoundRequest := {typeId := 38,x := -1,y := -1}
    (some {
      request,variation := variation.toNat,volume := volume * request.volumeScale,pan := 0
      pitch := clampPitch (single pitch * 0.002 + request.pitchOffset)},random)

end Lean4Terria.OriginalPickupSound
