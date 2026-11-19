import Lean4Terria.OriginalPlayerMovement

/-!
# Player in liquid (Terraria 1.4.5.8, Classic, no equipment)

Handwritten from `Player.Update`, `Player.WetCollision` and `Collision.WetCollision` of
`TerrariaServer.exe` 1.4.5.8. It covers ordinary water; lava and honey share the parts listed
below. The order inside `Player.Update` is:

1. `IL_0037`–`IL_0265`: effective stats from last frame's `wet`, `honeyWet` and `shimmerWet`.
   Water gives gravity 0.2, maxFallSpeed 5, jumpHeight 30, jumpSpeed 6.01. Honey gives
   gravity 0.1 and maxFallSpeed 3. Then `maxFallSpeed += 0.01`, and `IL_09e3` multiplies
   gravity by the height scale.
2. Horizontal movement, `JumpMovement`, gravity, `GrabItems`: `OriginalPlayerMovement.beginOnFoot`.
   For a player without Flippers, `JumpMovement` only reads `wet`.
3. `IL_7291`–`IL_80aa`: `Collision.WetCollision` on the player's position, entering and leaving
   (`wet`, `wetCount = 10`, the jump cap `jumpHeight / 5` on leaving), then `wetCount -= 1`.
4. `gfxOffY`, `SlopeDownMovement`, `StepDown`, `StepUp`.
5. The collision dispatch (`IL_8aa9`): a wet player uses `Player.WetCollision(fall, ignore, 0.5)`
   (honey 0.25) instead of `DryCollision`. Then `SlopingCollision` and the head bump run
   as on dry land.

`Player.TryFloatingInFluid` and the entry `velocity.Y /= 2` both require
`ShouldFloatInWater` (`canFloatInWater`, an accessory), so they never run here.

The splash dust and the `SoundEngine.PlaySound(19)` on entering and leaving draw from
`Main.rand`. This module does not model that stream: it changes no player state.

Not modelled: lava damage and `lavaTime`, the honey buff, shimmer (`shimmering`), Flippers,
merman, Trident, mounts, water walking, the expert-mode chill debuff, and breath
(`breath` and the drowning timer).
-/

namespace Lean4Terria.OriginalPlayerWet

open OriginalCollision (Grid Vector single)
open OriginalPlayerCollision (Body ContactEffects)
open OriginalPlayerMovement (State Controls)
open OriginalWorld (Tile LiquidKind)

/-- `Entity.wet`, `lavaWet`, `honeyWet`, `shimmerWet` and `wetCount`. -/
structure Liquid where
  wet : Bool := false
  lavaWet : Bool := false
  honeyWet : Bool := false
  shimmerWet : Bool := false
  wetCount : Nat := 0
  deriving BEq, Repr, Inhabited

structure Player where
  move : State
  liquid : Liquid := {}
  deriving Repr

/-! ## Collision.WetCollision -/

/-- The result of `Collision.WetCollision` and the statics it writes. -/
structure WetHit where
  wet : Bool := false
  honey : Bool := false
  shimmer : Bool := false
  deriving BEq, Repr

private def liquidAmount (tile : Tile) : Nat := (tile.liquid.map (·.amount.toNat)).getD 0

private def liquidKind (tile : Tile) : Option LiquidKind := tile.liquid.map (·.kind)

private def clampInt (v lo hi : Int) : Int := if v < lo then lo else if v > hi then hi else v

/-- `Collision.WetCollision(position, width, height)`. The probe is a box centred on the
entity, at most 10 px wide and `height / 2` tall. A liquid cell counts from its surface
`(256 - liquid) / 32 * 2` px below the tile top. A sloped solid tile counts with the
liquid of the cell above it. -/
def wetCollision (grid : Grid) (position : Vector) (width height : Nat) : WetHit := Id.run do
  let center : Vector := ⟨position.x + single (width / 2 : Nat), position.y + single (height / 2 : Nat)⟩
  let w : Nat := if 10 > width then width else 10
  let h : Nat := if height / 2 > height then height else height / 2
  let box : Vector := ⟨center.x - single (w / 2 : Nat), center.y - single (h / 2 : Nat)⟩
  let x0 := clampInt ((position.x / 16).toInt32.toInt - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (((position.x + single width) / 16).toInt32.toInt + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt ((position.y / 16).toInt32.toInt - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (((position.y + single height) / 16).toInt32.toInt + 2) 0 ((grid.height : Int) - 40)
  let overlaps (left top : Float32) (tall : Int) : Bool :=
    box.x + single w > left && box.x < left + 16 && box.y + single h > top && box.y < top + single tall
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      let amount := liquidAmount tile
      if amount > 0 then
        let left := single (x * 16 : Nat)
        let depth : Float32 := (single (256 - amount : Nat)) / 32
        let top := single (y * 16 : Nat) + depth * 2
        let tall : Int := 16 - (depth * 2).toInt32.toInt
        if overlaps left top tall then
          return { wet := true, honey := liquidKind tile == some .honey, shimmer := liquidKind tile == some .shimmer }
      else
        let sloped := match tile.shape with
          | .slope id => id != 0
          | _ => false
        if tile.id.isSome && sloped && y > 0 then
          let above := grid.tileAt x (y - 1)
          if liquidAmount above > 0 && overlaps (single (x * 16 : Nat)) (single (y * 16 : Nat)) 16 then
            let kind := liquidKind above
            return { wet := true, honey := kind == some .honey, shimmer := kind != some .honey && kind == some .shimmer }
  return {}

/-! ## Per-frame stats -/

/-- `Player.Update` `IL_0037`–`IL_0265` and the gravity scale at `IL_09e3`. It reads the
liquid flags left by the previous frame. -/
def Player.prepare (player : Player) (worldWidth : Nat) (worldSurface : Float)
    (remixWorld : Bool := false) : Player :=
  let dry := player.move.prepareDryTick worldWidth worldSurface remixWorld
  let l := player.liquid
  if !l.wet then { player with move := dry } else
  let scale := OriginalPlayerMovement.gravityScale dry.body.position.y worldWidth worldSurface remixWorld
  let (base, maxFall, jumpHeight, jumpSpeed) : Float32 × Float32 × Int × Float32 :=
    if l.shimmerWet then (0.15, 10, 23, 5.51)
    else if l.honeyWet then (0.1, 3, 15, 5.01) else (0.2, 5, 30, 6.01)
  { player with move := { dry with
      body := { dry.body with gravity := base * scale }
      maxFallSpeed := maxFall + 0.01
      jumpHeight, jumpSpeed } }

/-! ## Entering and leaving (IL_7291–IL_80aa) -/

/-- Apply the liquid probe before the movement collision. Leaving liquid caps
`jump` at one fifth of this frame's effective `jumpHeight`. -/
def Player.contactWith (player : Player) (hit : WetHit) : Player := Id.run do
  let mut l := player.liquid
  let mut move := player.move
  if hit.shimmer then l := { l with shimmerWet := true }
  if hit.honey then l := { l with honeyWet := true }
  if hit.wet then
    if !l.wet then
      if l.wetCount == 0 then l := { l with wetCount := 10 }
      l := { l with wet := true }
  else if l.wet then
    l := { l with wet := false }
    let cap := move.jumpHeight / 5
    if move.body.jump > cap then move := { move with body := { move.body with jump := cap } }
    if l.wetCount == 0 then l := { l with wetCount := 10 }
  if !hit.honey then l := { l with honeyWet := false }
  if !hit.shimmer then l := { l with shimmerWet := false }
  if !l.wet then l := { l with lavaWet := false, honeyWet := false, shimmerWet := false }
  if l.wetCount > 0 then l := { l with wetCount := l.wetCount - 1 }
  return { move, liquid := l }

/-- The liquid probe after gravity and `GrabItems`, before the movement collision. -/
def Player.contact (player : Player) (grid : Grid) : Player :=
  let body := player.move.body
  player.contactWith (wetCollision grid body.position body.width body.height)

/-! ## Movement collision -/

/-- `Player.WetCollision(fallThrough, ignorePlats, mult)`. The velocity comes from
`TileCollision`. The position moves by `mult` times that velocity, except on the axes
the collision changed. -/
def Body.wetCollision (body : Body) (grid : Grid) (fallThrough ignorePlatforms : Bool)
    (mult : Float32) : Body :=
  let before := body.velocity
  let hit := body.tileCollision grid body.position body.velocity fallThrough ignorePlatforms
  let v := hit.displacement
  let scaled : Vector := ⟨v.x * mult, v.y * mult⟩
  let step : Vector := ⟨if v.x != before.x then v.x else scaled.x, if v.y != before.y then v.y else scaled.y⟩
  { body with position := body.position + step, velocity := v,
              collision := { body.collision with up := hit.hitAbove, down := hit.hitBelow } }

/-- `OriginalPlayerMovement.finishOnFoot` with the liquid dispatch of `IL_8aa9`. -/
def Player.finish (player : Player) (grid : Grid) (effects : ContactEffects := {}) : Player :=
  let l := player.liquid
  let move := if l.wet then
      let scale : Float32 := if l.shimmerWet then 0.375 else if l.honeyWet then 0.25 else 0.5
      player.move.finishOnFootWith grid (fun body fallThrough =>
        Body.wetCollision body grid fallThrough false scale) effects
    else player.move.finishOnFoot grid effects
  { player with move }

/-- One on-foot frame with liquid: stats, `beginOnFoot`, the liquid probe, then the
collision. A session that runs `GrabItems` or item use between the halves calls
`prepare`, `move.beginOnFoot`, `contact` and `finish` itself. -/
def Player.step (player : Player) (grid : Grid) (worldWidth : Nat) (worldSurface : Float)
    (input : Controls) : Player :=
  let p := player.prepare worldWidth worldSurface
  let p := { p with move := p.move.beginOnFoot input }
  (p.contact grid).finish grid

end Lean4Terria.OriginalPlayerWet
