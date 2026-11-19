import Lean4Terria.OriginalHeldItemLight

/-! The sound/dust prefix of desktop 1.4.5.8 `WorldGen.KillTile` for basic ground,
Water/Peace/Shadow Candles, Wooden Work Bench style0 and Wooden Platform style0.
The caller owns breakability/terrain/framing/drop order and invokes this at each
actual KillTile, before its drops. This module never edits a tile or spawns an item.
Ground evidence/scope: `.lake/original-basic-ground-effects-audit/IMPLEMENTATION.md`. -/
namespace Lean4Terria.OriginalTileHitEffects

open OriginalCollision (Vector single)
open OriginalForestGuideArrow (DustPool DustParticle DustRequest)

structure Context where
  dedicatedServer : Bool := false
  gameMenu : Bool := false
  paused : Bool := false
  generating : Bool := false
  stopDrops : Bool := false
  effectOnly : Bool := false
  audioSupported : Bool := false
  netMode : Int := 0
  soundVolume : Float32 := 1
  /-- Main.Camera.Center, from unscaled position/size. -/
  cameraCenter : Vector := ⟨0,0⟩
  maxWorldViewWidth : Float32 := 1920
  attenuationDistance : Float32 := 2500

structure SoundRequest where
  typeId : Nat := 0
  x : Int
  y : Int
  style : Nat := 1
  volumeScale : Float32 := 1
  pitchOffset : Float32 := 0
  deriving Repr

/-- A resolved Dig0 or Tink21 instance. The engine stops/replaces the previous instance of
this variation. No random sample is needed to submit these resolved values. -/
structure PlayedSound where
  request : SoundRequest
  variation : Nat
  volume : Float32
  pan : Float32
  pitch : Float32
  deriving Repr

private def clamp (lo hi value : Float32) : Float32 :=
  if value < lo then lo else if value > hi then hi else value

private def playMaterialSound (tink : Bool) (request : SoundRequest) (context : Context)
    (random : UnifiedRandom) :
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
  let volume := clamp 0 1 volume
  let volume := min 1 (volume * context.soundVolume)
  if !(volume > 0) then return (none, random)
  let (variation, random) := random.next 0 3
  let (pitch, random) := if tink then (0,random) else random.next (-10) 11
  return (some {
    request,variation := variation.toNat
    volume := volume * request.volumeScale,pan,
    pitch := clamp (-1) 1 (single pitch * 0.01 + request.pitchOffset) }, random)

/-- The actual SoundEngine→LegacySoundPlayer type0 branch, with ready SoundDig
assets. Missing assets/backend exceptions are outside the original successful
client branch. Inaudible calls do not select a variation or pitch. -/
def playDig (request : SoundRequest) (context : Context) (random : UnifiedRandom) :
    Option PlayedSound × UnifiedRandom := playMaterialSound false request context random

/-- Type21 uses the three SoundTink assets and one Next(3). Its fresh instance has
pitch0 before the common pitchOffset tail; there is no random pitch draw. -/
def playTink (request : SoundRequest) (context : Context) (random : UnifiedRandom) :
    Option PlayedSound × UnifiedRandom := playMaterialSound true request context random

structure Result where
  pool : DustPool
  random : UnifiedRandom
  particles : Array DustParticle := #[]
  soundRequest : Option SoundRequest := none
  sound : Option PlayedSound := none
  /-- Callers must reject unsupported variants instead of silently omitting effects. -/
  unsupported : Option String := none

/-- Fixed dust types in the checked ground/wood/candle scope. `none` also covers
supported random-choice surfaces: `emit` selects those inside every dust iteration.
Other furniture/platform styles retain their previous explicit boundaries. -/
def dustType (tile : OriginalWorld.Tile) : Option Nat := do
  let id ← tile.id
  let (fx,fy) := tile.frame.getD (0,0)
  if id == 18 then (if tile.tilePaint == 0 && fy == 0 && (fx == 0 || fx == 18) then some 7 else none)
  else if id == 19 then (if tile.tilePaint == 0 && fy == 0 then some 7 else none)
  else if id == 49 || id == 646 then (if tile.tilePaint == 0 then some 29 else none)
  else if id == 372 then (if tile.tilePaint == 0 then some 242 else none)
  else match id with
    | 0 | 60 => some 0
    | 1 | 38 => some 1
    | 6 => some 8
    | 7 => some 9
    | 8 => some 10
    | 9 => some 11
    | 22 => some 14
    | 30 => some 7
    | 53 => some 32
    | 56 => some 37
    | 57 => some 36
    | 59 => some 38
    | 70 => some 17
    | 166 => some 81
    | 167 => some 82
    | 168 => some 83
    | 169 => some 84
    | 179 | 512 => some 93
    | 180 | 513 => some 94
    | 181 | 514 => some 95
    | 182 | 515 => some 96
    | 183 | 516 => some 97
    | 199 | 662 => some 117
    | 381 | 517 => some 258
    | 534 | 535 => some 299
    | 536 | 537 => some 300
    | 539 | 540 => some 301
    | 625 | 626 => some 305
    | _ => none

private inductive DustChoice where
  | fixed (kind : Nat)
  | choose (bound : Int) (zeroKind other : Nat)

private def dustChoice (tile : OriginalWorld.Tile) : Option DustChoice :=
  match dustType tile with
  | some kind => some (.fixed kind)
  | none => match tile.id with
    | some 2 | some 477 => some (.choose 2 0 2)
    | some 23 | some 661 => some (.choose 2 14 17)
    | some 25 => some (.choose 2 14 1)
    | some 37 => some (.choose 2 6 23)
    | some 58 => some (.choose 2 6 25)
    | some 109 | some 492 => some (.choose 2 0 47)
    | some 204 => some (.choose 2 1 117)
    | some 633 => some (.choose 6 36 237)
    | _ => none

private def DustChoice.sample (choice : DustChoice) (random : UnifiedRandom) : Nat × UnifiedRandom :=
  match choice with
  | .fixed kind => (kind,random)
  | .choose bound zeroKind other =>
    let (roll,random) := random.next 0 bound
    (if roll == 0 then zeroKind else other,random)

private def tinkTile (tile : OriginalWorld.Tile) : Bool :=
  tile.id.any fun id => (#[1,6,7,8,9,22,25,37,38,56,58,166,167,168,169,204] : Array UInt16).contains id

private def dustAmount (tile : OriginalWorld.Tile) (fail : Bool) : Nat :=
  if fail then 3 else if tile.id.any fun id =>
    (#[534,536,539,625] : Array UInt16).contains id then 5 else 10

/-- Invoke only after KillTile's bounds/active/breakability guards, using the
possibly forced `fail` value. The original sound precedes the3/10 dust requests;
`effectOnly` and `stopDrops` suppress sound, not the dust loop. `destroyObject`
is deliberately absent: it does not suppress this prefix on nested cell kills. -/
def emit (tile : OriginalWorld.Tile) (x y : Int) (fail : Bool) (context : Context)
    (pool : DustPool) (random : UnifiedRandom) : Result := Id.run do
  if tile.id.isNone then return {pool,random}
  let some choice := dustChoice tile | return {pool,random,unsupported := some "KillTile prefix: tile/style/paint"}
  let mut result : Result := {pool,random}
  if !context.effectOnly && !context.stopDrops && !context.dedicatedServer &&
      !context.generating && !context.gameMenu then
    let request : SoundRequest := {typeId := if tinkTile tile then 21 else 0,x := x*16,y := y*16}
    let (sound, random) := if request.typeId == 21 then playTink request context result.random
      else playDig request context result.random
    result := {result with soundRequest := some request,sound,random}
  let enabled := pool.enabled
  let gated := enabled && !context.gameMenu && !context.paused &&
    !context.generating && context.netMode != 2
  result := {result with pool := {result.pool with enabled := gated}}
  for _ in [:dustAmount tile fail] do
    -- WorldGen.genRand returns Main.rand in this binary. Selection precedes
    -- NewDust's own gates, including a full pool or paused/menu/server call.
    let (kind, random) := choice.sample result.random
    let request : DustRequest := {
      position := ⟨single (x*16),single (y*16)⟩
      width := 16,height := 16,typeId := kind}
    let (pool, particle, random) := OriginalHeldItemLight.newDust result.pool request random
    result := {result with pool,random,particles := result.particles ++ particle.toArray}
  return {result with pool := {result.pool with enabled}}

end Lean4Terria.OriginalTileHitEffects
