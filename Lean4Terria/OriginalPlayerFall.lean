import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalWorld

/-! The local player's fall bookkeeping of desktop 1.4.5.8 `Player.Update`, IL_188d..1c06: it runs
near the start of the update (after `dropItemCheck`, before `UpdateImmunity` and `ResetEffects`), so
every field it reads is what the PREVIOUS update left: position, velocity, `jump`, `wet`,
`extraFall`, `noFallDmg`.

    velocity.Y == 0:   distance = (int)(position.Y / 16) − fallStart
                       distance > 0 and a cloud tile under the feet → distance = 0
                       distance > 25 + extraFall, no noFallDmg → immune = false,
                           Hurt(ByOther(0), (distance − limit) * 10, 0, General)    (caller)
                       fallStart = (int)(position.Y / 16)
    jump > 0, wet, or thin air (gravity factor < 0.8):  fallStart = (int)(position.Y / 16)

Not modelled, because the session has none of them: mounts, wings, rockets, slow fall, reversed
gravity, the stoned and tongued states, the down dash, `fallStart2` (written at IL_189f..18b2 and
read only by code the session does not have). Evidence: `.lake/forest-desktop-integration/hurt-runtime/`. -/
namespace Lean4Terria.OriginalPlayerFall

open OriginalCollision (Vector single tileIndex)

/-- `TileID.Sets.Clouds` (static data C682E09F…, six entries). -/
def clouds : Array UInt16 := #[189, 196, 460, 717, 718, 719]

/-- IL_1a0d..1a70: a tile that cancels the fall distance — a cloud, type 666, or the platform
style whose `frameY / 18` is 49. -/
def cancelsFall (tile : OriginalWorld.Tile) : Bool :=
  match tile.id with
  | none => false
  | some id =>
    clouds.contains id || id == 666 ||
      (id == 19 && (tile.frame.map fun (_, frameY) => frameY.tdiv 18 == 49).getD false)

/-- `Update` IL_092a..09e2: the factor the update multiplies gravity by near space. Single
operations as in the IL, the division by the surface in Double. -/
def gravityFactor (positionY : Float32) (maxTilesX : Nat) (worldSurface : Float) (remixWorld : Bool) : Float32 :=
  let scale : Float32 := maxTilesX.toFloat32 / 4200
  let scale := scale * scale
  let height : Float32 := positionY / 16 - (60 + 10 * scale)
  let factor : Float32 := (height.toFloat / (worldSurface / (if remixWorld then 1 else 6))).toFloat32
  let factor := if remixWorld then (if factor.toFloat < 0.1 then 0.1 else factor)
    else if factor.toFloat < 0.25 then 0.25 else factor
  if factor > 1 then 1 else factor

structure Input where
  position : Vector
  velocity : Vector
  width : Nat := 20
  height : Nat := 42
  fallStart : Int
  jump : Int := 0
  wet : Bool := false
  extraFall : Int := 0
  noFallDmg : Bool := false
  gravityFactor : Float32 := 1

structure Output where
  fallStart : Int
  /-- The `Damage` argument of the fall `Hurt`; the caller clears `immune` first (IL_1b2b). -/
  damage : Option Int := none
  /-- Tiles fallen as IL_1911 computed them, after the cloud test. -/
  distance : Int := 0

def step (input : Input) (tileAt : Int → Int → Option OriginalWorld.Tile) : Output := Id.run do
  let row := tileIndex input.position.y
  let mut out : Output := {fallStart := input.fallStart}
  if input.velocity.y == 0 then
    let limit := 25 + input.extraFall
    let mut distance := row - input.fallStart
    if distance > 0 then
      let first := tileIndex input.position.x
      let last := tileIndex (input.position.x + single input.width)
      let below := tileIndex (input.position.y + single input.height + 1)
      let mut x := first
      while x ≤ last do
        if (tileAt x below).any cancelsFall then
          distance := 0
          break
        x := x + 1
    let damage := if distance > limit && !input.noFallDmg then (distance - limit) * 10 else 0
    out := {fallStart := row, damage := if damage > 0 then some damage else none, distance}
  if input.jump > 0 || input.wet || input.gravityFactor.toFloat < 0.8 then
    out := {out with fallStart := row}
  return out

end Lean4Terria.OriginalPlayerFall
