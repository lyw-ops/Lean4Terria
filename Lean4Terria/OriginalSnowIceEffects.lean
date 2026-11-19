import Lean4Terria.OriginalTileHitEffects
import Lean4Terria.OriginalSnowIceDust

/-! The sound/dust prefix of desktop 1.4.5.8 `WorldGen.KillTile` for Snow Block 147 and
Ice Block 161, the two ground tiles `OriginalTileHitEffects.emit` reports as unsupported.
Same contract as that module: the caller owns breakability, terrain, framing and drops, and
invokes `emit` at each actual `KillTile` before its drops. Nothing here edits a tile.
Evidence and limits: `.lake/original-snow-ice-effects/REPORT.md`. -/
namespace Lean4Terria.OriginalSnowIceEffects

open OriginalCollision (Vector single)
open OriginalForestGuideArrow (DustPool DustParticle DustRequest)
open OriginalTileHitEffects (Context SoundRequest PlayedSound Result)

def supports (tile : OriginalWorld.Tile) : Bool :=
  tile.id == some 147 || tile.id == some 161

/-- `KillTile_MakeTileDust`: Snow Block → dust 51, Ice Block → dust 80. Paint does not change it. -/
def dustType (tile : OriginalWorld.Tile) : Option Nat :=
  match tile.id with
  | some 147 => some 51
  | some 161 => some 80
  | _ => none

private def clamp (lo hi value : Float32) : Float32 :=
  if value < lo then lo else if value > hi then hi else value

/-- `SoundEngine.PlaySound(2, x, y, style, volumeScale, pitchOffset)` for the item styles
48, 49 and 50 with ready `SoundItem` assets (`LegacySoundPlayer.PlaySound` IL_0000..0185, the
type 2 branch IL_045b..07b1 and the tail IL_251e..2559). These styles have no volume factor and
no keep-playing rule: the previous instance of the style is stopped, a new one gets the common
volume and pan and `Main.rand.Next(-6, 7) * 0.01f` as pitch. An inaudible call draws nothing.
`variation` of the result is the `SoundItem` index, equal to `request.style`. -/
def playItem (request : SoundRequest) (context : Context) (random : UnifiedRandom) :
    Option PlayedSound × UnifiedRandom := Id.run do
  if context.dedicatedServer || !context.audioSupported || context.soundVolume == 0 then
    return (none, random)
  let global := request.x == -1 || request.y == -1
  if !global && (context.generating || context.netMode == 2) then return (none, random)
  let offset : Vector := ⟨single request.x - context.cameraCenter.x,
    single request.y - context.cameraCenter.y⟩
  let distance := Float32.sqrt (offset.x * offset.x + offset.y * offset.y)
  if !global && !(distance < context.attenuationDistance) then return (none, random)
  let pan := if global then 0 else clamp (-1) 1 (offset.x / (context.maxWorldViewWidth * 0.5))
  let volume := if global then 1 else 1 - distance / context.attenuationDistance
  let volume := if volume > 1 then 1 else volume
  if !(volume > 0) then return (none, random)
  let volume := volume * context.soundVolume
  let volume := if volume > 1 then 1 else volume
  if !(volume > 0) then return (none, random)
  let (pitch, random) := random.next (-6) 7
  return (some {
    request, variation := request.style
    volume := volume * request.volumeScale, pan
    pitch := clamp (-1) 1 (single pitch * 0.01 + request.pitchOffset) }, random)

/-- Invoke only after `KillTile`'s bounds, active and breakability guards, with the possibly
forced `fail`. `KillTile_PlaySounds` (IL_0040..00ee) runs unless `effectOnly`, `stopDrops`, a
dedicated server, world generation or the menu: snow first draws `genRand.Next(2)` (`genRand` is
`Main.rand` in this binary) to choose Item48 or Item49, ice always asks for Item50; that draw
happens whether or not the sound is then audible. The 10 (3 on `fail`) dust requests follow,
each through `OriginalSnowIceDust.newDust`. -/
def emit (tile : OriginalWorld.Tile) (x y : Int) (fail : Bool) (context : Context)
    (pool : DustPool) (random : UnifiedRandom) : Result := Id.run do
  if tile.id.isNone then return {pool, random}
  let some kind := dustType tile
    | return {pool, random, unsupported := some "KillTile prefix: not Snow Block 147 or Ice Block 161"}
  let mut result : Result := {pool, random}
  if !context.effectOnly && !context.stopDrops && !context.dedicatedServer &&
      !context.generating && !context.gameMenu then
    let (style, random) :=
      if kind == 51 then
        let (roll, random) := result.random.next 0 2
        (if roll == 0 then 48 else 49, random)
      else (50, result.random)
    let request : SoundRequest := {typeId := 2, x := x*16, y := y*16, style}
    let (sound, random) := playItem request context random
    result := {result with soundRequest := some request, sound, random}
  let enabled := pool.enabled
  let gated := enabled && !context.gameMenu && !context.paused &&
    !context.generating && context.netMode != 2
  result := {result with pool := {result.pool with enabled := gated}}
  for _ in [:if fail then 3 else 10] do
    let request : DustRequest := {
      position := ⟨single (x*16), single (y*16)⟩
      width := 16, height := 16, typeId := kind}
    let (pool, particle, random) := OriginalSnowIceDust.newDust result.pool request result.random
    result := {result with pool, random, particles := result.particles ++ particle.toArray}
  return {result with pool := {result.pool with enabled}}

end Lean4Terria.OriginalSnowIceEffects
