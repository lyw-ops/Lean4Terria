import Lean4Terria.OriginalTileHitEffects
import Lean4Terria.OriginalSnowIceEffects

/-! One item phase's shared tile effects. World.random stays the sole random
stream; this accumulator carries allocation state and ordered output only.
`none` preserves legacy headless callers. -/
namespace Lean4Terria.OriginalTileHitRuntime
open OriginalForestGuideArrow (DustPool DustParticle)

structure Boundary where
  x : Int
  y : Int
  typeId : Option UInt16
  reason : String
  deriving Repr

structure State where
  context : OriginalTileHitEffects.Context
  pool : DustPool
  particles : Array DustParticle := #[]
  soundRequests : Array OriginalTileHitEffects.SoundRequest := #[]
  resolvedSounds : Array OriginalTileHitEffects.PlayedSound := #[]
  boundary : Option Boundary := none

def blocked (state : Option State) : Bool := state.any (·.boundary.isSome)

/-- `effectOnly` belongs to this KillTile call; the base context survives intact
so a later nested candle kill can play its normal sound. -/
def emit (state : Option State) (tile : OriginalWorld.Tile) (x y : Int)
    (fail : Bool) (random : UnifiedRandom) (effectOnly : Bool := false) :
    Option State × UnifiedRandom :=
  match state with
  | none => (none,random)
  | some state =>
    if state.boundary.isSome then (some state,random) else
    let emit := if OriginalSnowIceEffects.supports tile then OriginalSnowIceEffects.emit
      else OriginalTileHitEffects.emit
    let result := emit tile x y fail
      {state.context with effectOnly} state.pool random
    (some { state with
      pool := result.pool,particles := state.particles ++ result.particles
      soundRequests := state.soundRequests ++ result.soundRequest.toArray
      resolvedSounds := state.resolvedSounds ++ result.sound.toArray
      boundary := result.unsupported.map fun reason => {x,y,typeId := tile.id,reason} },result.random)

/-- PoundTile/SlopeTile's explicit SoundEngine.PlaySound after their effectOnly
KillTile. This does not apply KillTile's outer effectOnly/stopDrops/menu gates. -/
def dig (state : Option State) (x y : Int) (random : UnifiedRandom) :
    Option State × UnifiedRandom :=
  match state with
  | none => (none,random)
  | some state =>
    if state.boundary.isSome then (some state,random) else
    let request : OriginalTileHitEffects.SoundRequest := {x := x*16,y := y*16}
    let (played,random) := OriginalTileHitEffects.playDig request state.context random
    (some {state with
      soundRequests := state.soundRequests.push request
      resolvedSounds := state.resolvedSounds ++ played.toArray},random)

end Lean4Terria.OriginalTileHitRuntime
