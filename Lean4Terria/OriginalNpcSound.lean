import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalTileHitEffects
import Lean4Terria.OriginalNpcSoundTable

/-! `LegacySoundPlayer.PlaySound` for NPCHit (type 3, IL_07b6..088d) and NPCKilled (type 4,
IL_0892..094d), desktop 1.4.5.8, with ready assets. Both are positioned at the NPC. An audible one
draws its pitch, `Main.rand.Next(-10, 11) * 0.01`, from the stream that is current at the call.
NPCHit stops the previous instance of its style; NPCKilled instances overlap.
Volume: the common positional volume, times 0.5 for hit styles 20..56 and killed styles 23..57,
times 0.6 for killed 61 and 62. Hit style 57 and killed style 10 depend on whether the previous
instance is still playing: they are refused here, not guessed.
Evidence: `.lake/forest-desktop-integration/death-runtime/` (styles 1 and 2 natively). -/
namespace Lean4Terria.OriginalNpcSound

open OriginalCollision (Vector single)
open OriginalStarterMelee (NpcSound)

private def clamp (lo hi value : Float32) : Float32 :=
  if value < lo then lo else if value > hi then hi else value

def play (typeId style : Nat) (position : Vector) (context : OriginalTileHitEffects.Context)
    (random : UnifiedRandom) : Except String (Option NpcSound × UnifiedRandom) := do
  unless typeId == 3 || typeId == 4 do throw s!"not an NPC hit or death sound: type {typeId}"
  if typeId == 3 && style == 57 then throw "NPCHit57 depends on the playing state of its instance"
  if typeId == 4 && style == 10 then throw "NPCDeath10 depends on the playing state of its instance"
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then return (none, random)
  if context.generating || context.netMode == 2 then return (none, random)
  let dx := single position.x.toInt32.toInt - context.cameraCenter.x
  let dy := single position.y.toInt32.toInt - context.cameraCenter.y
  let distance := Float32.sqrt (dx * dx + dy * dy)
  if !(distance < context.attenuationDistance) then return (none, random)
  let pan := clamp (-1) 1 (dx / (context.maxWorldViewWidth * 0.5))
  let volume := min 1 (clamp 0 1 (1 - distance / context.attenuationDistance) * context.soundVolume)
  if !(volume > 0) then return (none, random)
  let scale : Float32 :=
    if typeId == 3 then (if 20 ≤ style && style ≤ 56 then 0.5 else 1)
    else if 23 ≤ style && style ≤ 57 then 0.5 else if style == 61 || style == 62 then 0.6 else 1
  let (pitch, random) := random.next (-10) 11
  return (some {typeId, style, volume := volume * scale, pan, pitch := single pitch * 0.01}, random)

/-- `HitSound` (`death = false`) or `DeathSound` of the NPC with net id `id`: the canonical style
from the generated table, then `play`. `none` in the first place: the id is not in the plain table
(a missing or special sound), which the caller must surface. -/
def strike (death : Bool) (id : Int) (position : Vector) (context : OriginalTileHitEffects.Context)
    (random : UnifiedRandom) : Except String (Option NpcSound × UnifiedRandom) := do
  let some (hit, killed) := OriginalNpcSoundTable.styles id
    | throw s!"NPC {id}: hit or death sound outside the plain legacy styles"
  play (if death then 4 else 3) (if death then killed else hit) position context random

end Lean4Terria.OriginalNpcSound
