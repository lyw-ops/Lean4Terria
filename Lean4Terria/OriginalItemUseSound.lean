import Lean4Terria.OriginalTileHitEffects

/-! The ordinary LegacySoundPlayer type2/style1 swing request. This is the
ItemCheck start sound, before the owner-only mining effects. It selects Item1,
Item18 or Item19, then samples pitch; it is not a skip-two RNG adjustment. -/
namespace Lean4Terria.OriginalItemUseSound

open OriginalCollision (Vector single)
open OriginalTileHitEffects (Context PlayedSound SoundRequest)

private def clamp (lo hi value : Float32) : Float32 :=
  if value < lo then lo else if value > hi then hi else value

/-- LegacySoundPlayer.PlaySound IL04ab..04c6 selects the item voice;
IL0738..074b samples Next(-6,7)*.01. The returned variation0/1/2 names
Item1/Item18/Item19. Requests remain type2/style1, preserving their origin.
Only successful ordinary-world audio with the three loaded assets is covered. -/
def playSwing (x y : Int) (context : Context) (random : UnifiedRandom) :
    Option PlayedSound × UnifiedRandom := Id.run do
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then
    return (none,random)
  let global := x == -1 || y == -1
  if !global && (context.generating || context.netMode == 2) then return (none,random)
  let offset : Vector := ⟨single x - context.cameraCenter.x,single y - context.cameraCenter.y⟩
  let distance := Float32.sqrt (offset.x * offset.x + offset.y * offset.y)
  if !global && !(distance < context.attenuationDistance) then return (none,random)
  let pan := if global then 0 else clamp (-1) 1 (offset.x / (context.maxWorldViewWidth * 0.5))
  let volume := if global then 1 else 1 - distance / context.attenuationDistance
  let volume := min 1 (clamp 0 1 volume * context.soundVolume)
  if !(volume > 0) then return (none,random)
  let (variation,random) := random.next 0 3
  let (pitch,random) := random.next (-6) 7
  let request : SoundRequest := {typeId := 2,x,y,style := 1}
  let played : PlayedSound := {
    request, variation := variation.toNat, volume, pan
    pitch := clamp (-1) 1 (single pitch * 0.01) }
  return (some played,random)

end Lean4Terria.OriginalItemUseSound
