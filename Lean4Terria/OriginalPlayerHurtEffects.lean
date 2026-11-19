import Lean4Terria.OriginalTileHitEffects

/-! Ordinary desktop 1.4.5.8 PlayHurtSound -> LegacySoundPlayer. Input voice
fields are real player state: appearance/skin does not determine voice at each
hit. Player's constructor defaults voiceVariant to1. This module does not
allocate damage text or blood a second time. -/
namespace Lean4Terria.OriginalPlayerHurtEffects

open OriginalCollision (Vector single)
open OriginalTileHitEffects (Context SoundRequest PlayedSound)

structure Voice where
  position : Vector
  whoAmI : Int := 0
  myPlayer : Int := 0
  stoned : Bool := false
  voiceOverride : Int := 0
  voiceVariant : Int := 1
  voicePitchOffset : Float32 := 0
  mountActive : Bool := false
  mountType : Int := 0
  wereWolf : Bool := false
  forceWerewolf : Bool := false
  hideWolf : Bool := false
  frostArmor : Bool := false
  boneArmor : Bool := false
  dontStarveWorld : Bool := false
  remixWorld : Bool := false

/-- Slot references represent nonnull original LegacySoundPlayer array entries,
including completed/stopped instances. Type1/20/Dig Stop those entries without
checking playing state. The caller may inject the shared Dig slots from other
owners. Default voice3 overwrites its last reference while older voices overlap.
pitchRandom is SoundStyle._random, separate from Main.rand. Its initial state
must be declared by the owning world; no guessed time-dependent seed is used. -/
structure State where
  slots : Array (String × Nat) := #[]
  nextRef : Nat := 0
  pitchRandom : Option UnifiedRandom := none

structure Start where
  instanceRef : Nat
  stoppedRef : Option Nat
  slotKey : String
  asset : String
  sound : PlayedSound
  deriving Repr

structure Result where
  state : State
  random : UnifiedRandom
  request : Option SoundRequest := none
  played : Option Start := none
  boundary : Option String := none

private def clamp (lo hi value : Float32) : Float32 :=
  if value < lo then lo else if value > hi then hi else value

/-- Utils.Remap(...,-1,1,-.7,.7,true), preserving Single operations. -/
def voicePitch (offset : Float32) : Float32 :=
  let t := clamp 0 1 ((offset - (-1)) / (1 - (-1)))
  (-0.7 : Float32) + (0.7 - (-0.7)) * t

private def requestFor (voice : Voice) : Except String (Option SoundRequest) := do
  let (x,y) := if voice.whoAmI == voice.myPlayer then (-1,-1) else
    (voice.position.x.toInt32.toInt,voice.position.y.toInt32.toInt)
  if voice.stoned then return some {typeId:=0,x,y}
  if voice.voiceOverride == 1 then return some {typeId:=1,x,y}
  if voice.voiceOverride == 2 then return some {typeId:=20,x,y}
  if voice.voiceOverride == 20 then return some {typeId:=42,style:=317,x,y,volumeScale:=0.9}
  if voice.voiceOverride ≥ 3 && voice.voiceOverride ≤ 19 then
    throw "special voiceOverride sound"
  if voice.mountActive && #[52,54,55,56,61].contains voice.mountType then
    throw "special mount hurt sound"
  if (voice.wereWolf || voice.forceWerewolf) && !voice.hideWolf then
    throw "werewolf hurt sound"
  if voice.frostArmor then throw "frost armor hurt sound"
  if voice.boneArmor then throw "bone armor hurt sound"
  if voice.dontStarveWorld && !voice.remixWorld then throw "Don't Starve hurt voice"
  let pitchOffset := voicePitch voice.voicePitchOffset
  if voice.voiceVariant == 1 then return some {typeId:=1,x,y,pitchOffset}
  if voice.voiceVariant == 2 then return some {typeId:=20,x,y,pitchOffset}
  if voice.voiceVariant == 3 then
    return some {typeId:=42,style:=317,x,y,volumeScale:=0.9,pitchOffset}
  return none

private def volumePan (ctx : Context) (request : SoundRequest) : Option (Float32 × Float32) := do
  if ctx.dedicatedServer || !ctx.audioSupported || ctx.soundVolume == 0 then none else
  let global := request.x == -1 || request.y == -1
  if !global && (ctx.generating || ctx.netMode == 2) then none else
  let offset : Vector := ⟨single request.x-ctx.cameraCenter.x,single request.y-ctx.cameraCenter.y⟩
  let distance := (offset.x*offset.x+offset.y*offset.y).sqrt
  if !global && !(distance < ctx.attenuationDistance) then none else
  let pan := if global then 0 else clamp (-1) 1 (offset.x/(ctx.maxWorldViewWidth*0.5))
  let volume := if global then 1 else 1-distance/ctx.attenuationDistance
  let volume := min 1 (min 1 volume * ctx.soundVolume)
  if !(volume > 0) then none else some (volume,pan)

private def State.remember (state : State) (key : String) (id : Nat) : State :=
  let slots := match state.slots.findIdx? (fun p => p.1 == key) with
    | some index => state.slots.set! index (key,id)
    | none => state.slots.push (key,id)
  {state with slots,nextRef:=id+1}

/-- Call at Hurt's PlayHurtSound point, after the actual text/life/knockback
prefix and before its blood NextFloat/NewDust loop. Accepted male/female calls
consume exactly one Main.rand Next(3), with no random pitch. Stoned Dig draws
variation then pitch. Default voice3 consumes one separate SoundStyle pitch
sample BEFORE all backend/volume/distance gates, including audioOff; its
Variations1 style317 consumes no LegacySoundStyle.Random or Main.rand samples.
Played values are final: the backend must not randomize them again. -/
def play (voice : Voice) (ctx : Context) (state : State) (random : UnifiedRandom) : Result := Id.run do
  let selection := requestFor voice
  if let .error why := selection then
    return {state,random,boundary:=some s!"Player.PlayHurtSound: {why}"}
  let some request := selection.toOption.getD none | return {state,random}
  let mut state := state
  let mut request := request
  if request.typeId == 42 then
    let some pitchRandom := state.pitchRandom |
      return {state,random,boundary:=some "Player.PlayHurtSound: SoundStyle._random state required"}
    let (sample,next) := pitchRandom.sample
    let pitch := sample.toFloat32 * 0.2 - 0.2 * 0.5
    state := {state with pitchRandom:=some next}
    request := {request with pitchOffset:=request.pitchOffset+pitch}
  let some (volume,pan) := volumePan ctx request | return {state,random,request:=some request}
  let (variation,random) := if request.typeId == 42 then (0,random) else random.next 0 3
  let (pitch,random) := if request.typeId == 0 then random.next (-10) 11 else (0,random)
  let (key,asset) := if request.typeId == 0 then (s!"dig:{variation}",s!"Sounds/Dig_{variation}")
    else if request.typeId == 1 then (s!"male:{variation}",s!"Sounds/Player_Hit_{variation}")
    else if request.typeId == 20 then (s!"female:{variation}",s!"Sounds/Female_Hit_{variation}")
    else ("trackable:317","Sounds/Custom/player_hit_default")
  let stoppedRef := if request.typeId == 42 then none else (state.slots.find? (fun p => p.1==key)).map (·.2)
  let id := state.nextRef
  let sound : PlayedSound := {
    request,variation:=variation.toNat,volume:=volume*request.volumeScale,pan
    pitch:=clamp (-1) 1 (single pitch*0.01+request.pitchOffset)}
  return {
    state:=state.remember key id,random,request:=some request
    played:=some {instanceRef:=id,stoppedRef,slotKey:=key,asset,sound}}

end Lean4Terria.OriginalPlayerHurtEffects
