import Lean4Terria.OriginalPlayerHurtText
import Lean4Terria.OriginalPlayerHurtEffects

/-! The rest of an accepted ordinary desktop 1.4.5.8 `Player.Hurt` after its damage text
(IL_076e) and the signed life write (IL_077e), for the callers that own the hit: NPC contact
and lava. Original order, which `finish` keeps:

    text → statLife -= damage (signed) → immunity / cooldown → knockback (caller)
      → PlayHurtSound (IL_0dde) → blink (IL_0de9)
      → statLife > 0 ? blood loop (IL_0dfa..0f9a) : statLife = 0 (IL_0fa3), KillMe (caller)

The sound is the released `OriginalPlayerHurtEffects.play`, the blood birth the existing
`OriginalPlayerHurtText.blood`; neither is repeated here. Retaliation accessories, mounts,
frost / bone armor and the PvP tail are not part of this path: the callers surface what the
player wears through the equipment boundaries.
Evidence: `.lake/forest-desktop-integration/hurt-runtime/`. -/
namespace Lean4Terria.OriginalPlayerHurt

open OriginalCollision (Vector)
open OriginalForestGuideArrow (DustPool DustParticle)

/-- The client's sound side of a Hurt: the player's real voice fields, the sound gates of this
update and the process-wide `LegacySoundPlayer` slots / `SoundStyle._random`. Headless callers
have none, and then no PlayHurtSound call is modelled at all (the legacy contract). -/
structure SoundClient where
  voice : OriginalPlayerHurtEffects.Voice
  context : OriginalTileHitEffects.Context
  state : OriginalPlayerHurtEffects.State := {}

/-- What a client adds to the ordinary hit. -/
structure Client where
  sound : Option SoundClient := none
  /-- `Player.noKnockback` as this update's `UpdateEquips` left it (read at IL_0d7e). -/
  noKnockback : Bool := false

/-- One accepted Hurt, for the session and the window. -/
structure Record where
  damage : Int
  /-- `statLife` from IL_077e to IL_0fa1: what PlayHurtSound and the blink saw. Signed. -/
  lifeAtSound : Int
  /-- A client made the PlayHurtSound call. -/
  soundCalled : Bool := false
  /-- The canonical engine request, also when nothing was heard. -/
  request : Option OriginalTileHitEffects.SoundRequest := none
  /-- Submit only this to the device; its values are final. -/
  played : Option OriginalPlayerHurtEffects.Start := none
  /-- `eyeHelper.BlinkBecausePlayerGotHurt()` ran (IL_0de9). -/
  blinked : Bool := false
  /-- `Dust.NewDust` calls of the blood loop. -/
  bloodCalls : Nat := 0
  /-- IL_0fa1..0fa3 ran: the caller continues with the death owner. -/
  lethal : Bool := false
  /-- `Main.rand` as PlayHurtSound found it and as it left it (equal when nothing was drawn).
  These are the production values of this very call, kept for observation only. -/
  randomBefore : Option UnifiedRandom := none
  randomAfter : Option UnifiedRandom := none
  /-- `SoundStyle._random` before the call and after it (the default voice draws its pitch
  variance from it before every gate; the canonical engine entry already sees the later state). -/
  styleBefore : Option UnifiedRandom := none
  styleAfter : Option UnifiedRandom := none
  /-- The `LegacySoundPlayer` slots after the call: slot key and the instance it now holds. -/
  slotsAfter : Array (String × Nat) := #[]

structure Input where
  position : Vector
  width : Nat
  height : Nat
  /-- The amount the text showed: integral after IL_03e9 / IL_0722. -/
  damage : Int
  /-- `statLife` as IL_077e left it. -/
  rawLife : Int
  lifeMax2 : Int
  hitDirection : Int

structure Output where
  /-- `statLife` where this method stopped: still the signed value at a sound boundary,
  otherwise positive or the 0 of IL_0fa3. -/
  life : Int
  pool : DustPool
  random : UnifiedRandom
  particles : Array DustParticle := #[]
  /-- The sound side with its slots and pitch stream advanced; commit it even when silent. -/
  sound : Option SoundClient
  record : Record
  /-- Set by an unsupported sound or blood variant. Everything before it is kept. -/
  boundary : Option String := none

/-- IL_0ddd..0fa3. Call after the caller has written life, immunity and knockback. -/
def finish (input : Input) (sound : Option SoundClient) (pool : DustPool)
    (random : UnifiedRandom) : Output := Id.run do
  let mut record : Record := {damage:=input.damage,lifeAtSound:=input.rawLife}
  let mut sound := sound
  let mut random := random
  if let some client := sound then
    let played := OriginalPlayerHurtEffects.play {client.voice with position:=input.position}
      client.context client.state random
    record := {record with
      soundCalled:=true,request:=played.request,played:=played.played
      randomBefore:=some random,randomAfter:=some played.random
      styleBefore:=client.state.pitchRandom,styleAfter:=played.state.pitchRandom
      slotsAfter:=played.state.slots}
    sound := some {client with state:=played.state}
    random := played.random
    if played.boundary.isSome then
      return {life:=input.rawLife,pool,random,sound,record,boundary:=played.boundary}
  record := {record with blinked:=true}
  if input.rawLife ≤ 0 then
    return {life:=0,pool,random,sound,record:={record with lethal:=true}}
  let voice := sound.map (·.voice)
  let blood := OriginalPlayerHurtText.blood {
    position:=input.position,width:=input.width,height:=input.height
    damage:=input.damage,lifeAfter:=input.rawLife,lifeMax:=input.lifeMax2
    hitDirection:=input.hitDirection
    stoned:=voice.any (·.stoned)
    unsupported:=if voice.any (·.frostArmor) then some "frost armor blood shader"
      else if voice.any (·.boneArmor) then some "bone armor blood shader" else none } pool random
  return {
    life:=input.rawLife,pool:=blood.pool,random:=blood.random,particles:=blood.particles,sound
    record:={record with bloodCalls:=blood.requests.size},boundary:=blood.boundary}

end Lean4Terria.OriginalPlayerHurt
