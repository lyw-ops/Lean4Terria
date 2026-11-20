import Lean4Terria.OriginalPlayerEquipment
import Lean4Terria.OriginalPlayerMovement
import Lean4Terria.OriginalPlayerFall

/-!
# Movement accessories (Terraria 1.4.5.8 desktop, single player, Classic)

Hand port of the installed 1.4.5.8 CIL (`Terraria.exe`
d52c7ea7e4e312d8937fd20d1f2609f0d6bd26daddf5bfab2d106ca9077103e0) of the movement-accessory slice of the local
player's `Player.Update` (100665742) on dry ground, with its callees `UpdateJumpHeight` 100665673,
`RefreshDoubleJumps` 100665707, `JumpMovement` 100665691, `CancelAllJumpVisualEffects`, `DashMovement` 100665692 with
`DoCommonDashHandle`, `WallslideMovement` 100665696, the `sandStorm` part of `DoubleJumpVisuals`, and the
`ApplyEquipFunctional` 100665594 entries of the movement accessories the released `OriginalPlayerEquipment` does not
translate. Reused, not re-implemented: `OriginalPlayerEquipment.apply` (per-slot effects), `OriginalPlayerMovement`
(`horizontalMovement`, gravity, `finishOnFoot` collision), `OriginalPlayerFall` (fall bookkeeping, gravity factor).

Frame order (Update offsets): fall step 188d (previous state) → stat setup 0031 + gravity factor 092a →
ResetEffects (its `ResizeHitbox`) / UpdateEquips 248f → moveSpeed scaling 3307 → UpdateJumpHeight 335c → double-jump refresh 34d0 →
double-jump run boosts 46af..4847 → HorizontalMovement 4b5e → sandStorm=false 4d81 → JumpMovement 4d89 →
CancelAllJumpVisualEffects 4db8 → DashMovement 4dc0 → WallslideMovement 4dea → DoubleJumpVisuals 4df6 →
gravity 67f8 → release flags 71c6 → collision 8394..8c80.

Scope (everything else is an explicit `OutOfScope`): local player, controls given (the input block IL_0bb0..1882 is the
caller's), gravity direction 1, no mount / wings / rocket boots / carpet / grapple / liquids / ice / sand / sticky
tiles / NPC contact, `Main.windPhysics` off, items in `accessories`. Visual-only effects (dust, gore, sounds,
`runSoundDelay`) are not modelled; they draw `Main.rand` and do not feed back into movement.
-/

namespace Lean4Terria.OriginalAccessoryMovement

open OriginalCollision (Vector Grid single)
open OriginalPlayerCollision (Body)
open OriginalPlayerEquipment (Effects Equipment Slot)

inductive OutOfScope where
  | item (type : Nat)
  | dashType (type : Int)
  | state (what : String)
  deriving Repr, BEq, Inhabited

/-! ## Accessories -/

/-- Movement accessories in scope. The Equipment module translates 53, 54, 857, 987, 158, 159, 953, 975, 2423, 285,
212; the others are `ownEntry`. -/
def accessories : List Nat :=
  [53, 54, 857, 987, 158, 159, 953, 975, 2423, 285, 212, 399, 983, 1163, 1164, 1250, 1251, 1252, 5331, 1724, 1863,
   3201, 3241, 3250, 3252, 1579, 3200, 976, 977, 3097, 984]

/-- Fields the Equipment module's `Effects` does not carry. -/
structure Extra where
  dashType : Int := 0
  hasJumpOption_Fart : Bool := false
  hasJumpOption_Sail : Bool := false
  blackBelt : Bool := false
  deriving Repr, BEq, Inhabited

/-- `ApplyEquipFunctional` entries (movement fields) of the in-scope items the Equipment module leaves out. -/
def ownEntry (item : Nat) (e : Effects) (x : Extra) : Option (Effects × Extra) :=
  let horseshoe (e : Effects) : Effects := { e with jumpBoost := true, noFallDmg := true, hasLuck_LuckyHorseshoe := true }
  match item with
  | 399 => some ({ e with jumpBoost := true, hasJumpOption_Cloud := true }, x)               -- Cloud in a Balloon
  | 983 => some ({ e with hasJumpOption_Sandstorm := true, jumpBoost := true }, x)           -- Sandstorm in a Balloon
  | 1163 => some ({ e with hasJumpOption_Blizzard := true, jumpBoost := true }, x)          -- Blizzard in a Balloon
  | 1164 => some ({ e with hasJumpOption_Cloud := true, hasJumpOption_Sandstorm := true,
                           hasJumpOption_Blizzard := true, jumpBoost := true }, x)          -- Bundle of Balloons
  | 1250 => some ({ horseshoe e with hasJumpOption_Cloud := true }, x)
  | 1251 => some ({ horseshoe e with hasJumpOption_Blizzard := true }, x)
  | 1252 => some ({ horseshoe e with hasJumpOption_Sandstorm := true }, x)
  | 5331 => some ({ horseshoe e with hasJumpOption_Cloud := true, hasJumpOption_Sandstorm := true,
                                     hasJumpOption_Blizzard := true }, x)
  | 1724 => some (e, { x with hasJumpOption_Fart := true })                                 -- Fart in a Jar
  | 1863 => some ({ e with jumpBoost := true }, { x with hasJumpOption_Fart := true })      -- Fart in a Balloon
  | 3201 => some (e, { x with hasJumpOption_Sail := true })                                 -- Tsunami in a Bottle
  | 3241 => some ({ e with jumpBoost := true }, { x with hasJumpOption_Sail := true })      -- Sharkron Balloon
  | 3250 => some (horseshoe e, { x with hasJumpOption_Fart := true })
  | 3252 => some (horseshoe e, { x with hasJumpOption_Sail := true })
  | 1579 | 3200 => some ({ e with accRunSpeed := 6 }, x)                                   -- Flurry, Sailfish Boots
  | 976 => some ({ e with spikedBoots := e.spikedBoots + 2 }, x)                            -- Tiger Climbing Gear
  | 977 => some (e, { x with dashType := 1 })                                               -- Tabi
  | 3097 => some (e, { x with dashType := 2 })                                              -- Shield of Cthulhu
  | 984 => some ({ e with spikedBoots := 2 }, { x with blackBelt := true, dashType := 1 })  -- Master Ninja Gear
  | _ => none

/-- ResetEffects + UpdateEquips for accessories in armor slots 3..7, in slot order (assignments such as Master Ninja
Gear's `spikedBoots = 2` depend on it). Supported items go through the released `OriginalPlayerEquipment.apply` one slot
at a time; `context.accRunSpeed` is `maxRunSpeed` (3) as Update IL_0080 leaves it. -/
def equip (items : Array Nat) (context : OriginalPlayerEquipment.Context := {}) : Except OutOfScope (Effects × Extra) := do
  let empty : Equipment := {}
  let mut e := OriginalPlayerEquipment.reset empty context
  let mut x : Extra := {}
  for k in [:items.size] do
    let item := items[k]!
    if item == 0 then continue
    unless accessories.contains item do throw (.item item)
    if let some (e', x') := ownEntry item e x then
      e := e'; x := x'
    else
      let single : Equipment := { empty with armor := empty.armor.set! (3 + k) (Slot.ofItem item.toUInt16) }
      e := OriginalPlayerEquipment.apply e single context
  return (e, x)

/-! ## Player state -/

structure JumpKinds where
  cloud : Bool := false
  sandstorm : Bool := false
  blizzard : Bool := false
  fart : Bool := false
  sail : Bool := false
  deriving Repr, BEq, Inhabited

def JumpKinds.any (j : JumpKinds) : Bool := j.cloud || j.sandstorm || j.blizzard || j.fart || j.sail

structure Controls where
  left : Bool := false
  right : Bool := false
  jump : Bool := false
  down : Bool := false
  dash : Bool := false
  deriving Repr, BEq, Inhabited

/-- The `Player` fields this slice reads and writes. `jumpHeight` / `jumpSpeed` are the static `Player` fields. -/
structure Player where
  body : Body
  direction : Int := 1
  releaseJump : Bool := false
  releaseLeft : Bool := false
  releaseRight : Bool := false
  releaseDash : Bool := false
  sliding : Bool := false
  slideDir : Int := 0
  autoJump : Bool := false
  sandStorm : Bool := false
  justJumped : Bool := false
  has : JumpKinds := {}
  canJumpAgain : JumpKinds := {}
  performing : JumpKinds := {}
  dash : Int := 0
  dashDelay : Int := 0
  dashTime : Int := 0
  eocDash : Int := 0
  eocHit : Int := -1
  dashType : Int := 0
  spikedBoots : Int := 0
  extraFall : Int := 0
  noFallDmg : Bool := false
  jumpBoost : Bool := false
  jumpSpeedBoost : Float32 := 0
  moveSpeed : Float32 := 1
  runAcceleration : Float32 := 0.08
  maxRunSpeed : Float32 := 3
  accRunSpeed : Float32 := 3
  runSlowdown : Float32 := 0.2
  maxFallSpeed : Float32 := 10
  jumpHeight : Int := 15
  jumpSpeed : Float32 := 5.01
  onWrongGround : Bool := false
  /-- `Player.miscCounter` (UpdateMiscCounter: +1, wraps at 300). -/
  miscCounter : Int := 0
  /-- The damage of the fall `Hurt` the fall step requested this frame (the caller owns `Hurt`). -/
  fallDamage : Option Int := none
  deriving Repr

/-- What the frame needs from the world. -/
structure World where
  grid : Grid
  maxTilesX : Nat
  worldSurface : Float
  /-- `WorldGen.SolidTile(x, y)` / `SolidOrSlopedTile` for the dash and wall-slide probes. -/
  solid : Int → Int → Bool
  tileAt : Int → Int → Option OriginalWorld.Tile
  /-- `Main.leftWorld` / `rightWorld` / `topWorld` (pixels). -/
  leftWorld : Float32 := 0
  rightWorld : Float32
  topWorld : Float32 := 0
  /-- `Player.Settings.DashControl == 0` (double tap allowed). -/
  doubleTapDash : Bool := true

/-! ## Update pieces -/

/-- `UpdateJumpHeight` for an unmounted player without brooch / moon-lord legs / werewolf / stool / sticky / dazed. -/
def updateJumpHeight (p : Player) : Player := Id.run do
  let mut p := p
  if p.jumpBoost then
    p := { p with jumpHeight := max p.jumpHeight 20, jumpSpeed := max p.jumpSpeed (Float32.ofBits 0x40D051EC) }
  return { p with jumpSpeed := p.jumpSpeed + p.jumpSpeedBoost }

/-- `RefreshDoubleJumps`: every jump the player has becomes available again; the others keep their flag (it only
sets, it never clears — clearing is the airborne branch of Update IL_34f8). -/
def refreshDoubleJumps (p : Player) : Player :=
  let c := p.canJumpAgain
  let h := p.has
  { p with canJumpAgain := { cloud := c.cloud || h.cloud, sandstorm := c.sandstorm || h.sandstorm,
                             blizzard := c.blizzard || h.blizzard, fart := c.fart || h.fart, sail := c.sail || h.sail } }

/-- `JumpMovement` without mounts, merman, down dash, flippers, wet slime. -/
def jumpMovement (p : Player) (input : Controls) : Player := Id.run do
  let mut p := p
  if !input.jump then
    return { p with body := { p.body with jump := 0 }, releaseJump := true }
  if p.sliding then p := { p with autoJump := false }
  let vy := p.body.velocity.y
  let gravDir := p.body.gravityDirection
  if p.body.jump > 0 then
    if vy == 0 then p := { p with body := { p.body with jump := 0 } }
    else p := { p with body := { p.body with velocity := { p.body.velocity with y := -p.jumpSpeed * gravDir },
                                             jump := p.body.jump - 1 } }
  else if (p.sliding || vy == 0 || p.canJumpAgain.any) && (p.releaseJump || (p.autoJump && (vy == 0 || p.sliding))) then
    if p.sliding || vy == 0 then p := { p with justJumped := true }
    -- IL_0792: the extra jump this press spends (the cloud jump is the fall-through)
    let c := p.canJumpAgain
    let kind : Nat := if c.sandstorm then 1 else if c.blizzard then 2 else if c.fart then 3 else if c.sail then 4 else 0
    p := { p with canJumpAgain := match kind with
      | 1 => { c with sandstorm := false } | 2 => { c with blizzard := false } | 3 => { c with fart := false }
      | 4 => { c with sail := false } | _ => { c with cloud := false } }
    if vy == 0 || p.sliding || (p.autoJump && p.justJumped) then p := refreshDoubleJumps p
    let lift := -p.jumpSpeed * gravDir
    let height (factor : Float) : Int := (Float.ofInt p.jumpHeight * factor).toInt32.toInt
    if vy == 0 || p.sliding then
      let x := if p.sliding then Float32.ofInt (3 * -p.slideDir) else p.body.velocity.x
      p := { p with body := { p.body with velocity := ⟨x, lift⟩, jump := p.jumpHeight } }
    else
      let (perf, jump) := match kind with
        | 1 => ({ p.performing with sandstorm := true }, p.jumpHeight * 3)
        | 2 => ({ p.performing with blizzard := true }, height 1.5)
        | 4 => ({ p.performing with sail := true }, height 1.25)
        | 3 => ({ p.performing with fart := true }, p.jumpHeight * 2)
        | _ => ({ p.performing with cloud := true }, height 0.75)
      p := { p with performing := perf, body := { p.body with velocity := { p.body.velocity with y := lift }, jump } }
  return { p with releaseJump := false }

/-- `CancelAllJumpVisualEffects(false)`. -/
def cancelJumpVisuals (p : Player) : Player := { p with performing := {} }

/-- `DoCommonDashHandle` → (direction, dashing). -/
def commonDashHandle (w : World) (p : Player) (input : Controls) : Player × Int × Bool := Id.run do
  let mut p := p
  if p.dashTime > 0 then p := { p with dashTime := p.dashTime - 1 }
  if p.dashTime < 0 then p := { p with dashTime := p.dashTime + 1 }
  let mut request : Int := 0
  if input.dash && p.releaseDash then
    let inp : Int := (if input.right then 1 else 0) - (if input.left then 1 else 0)
    request := if inp == -p.direction then inp else p.direction
  let mut dir : Int := 0
  let mut go := false
  if (input.right && p.releaseRight && w.doubleTapDash) || request == 1 then
    if p.dashTime > 0 || request == 1 then
      dir := 1; go := true; p := { p with dashTime := 0 }
    else p := { p with dashTime := 15 }
  else if (input.left && p.releaseLeft && w.doubleTapDash) || request == -1 then
    if p.dashTime < 0 || request == -1 then
      dir := -1; go := true; p := { p with dashTime := 0 }
    else p := { p with dashTime := -15 }
  if go && input.dash then p := { p with releaseDash := false }
  return (p, dir, go)

/-- `Utils.ToTileCoordinates`: `(int)v.X >> 4`, `(int)v.Y >> 4`. -/
def tileOf (v : Float32) : Int := v.toInt32.toInt / 16

/-- `DashMovement` for dash types 0, 1 (Tabi, Master Ninja Gear) and 2 (Shield of Cthulhu) without NPCs. -/
def dashMovement (w : World) (p : Player) (input : Controls) : Except OutOfScope Player := do
  let mut p := p
  unless p.dashType == 0 || p.dashType == 1 || p.dashType == 2 do throw (.dashType p.dashType)
  if p.dashDelay == 0 then p := { p with dash := p.dashType }
  if p.dash == 0 then p := { p with dashTime := 0, dashDelay := 0 }
  -- IL_007a: the shield bounce needs an NPC hit (eocHit ≥ 0)
  if p.dash == 2 && p.eocDash > 0 && p.eocHit >= 0 then throw (.state "shield of Cthulhu bounce")
  let vx := p.body.velocity.x
  let setVx (p : Player) (x : Float32) : Player := { p with body := { p.body with velocity := { p.body.velocity with x } } }
  if p.dashDelay > 0 then
    if p.eocDash > 0 then p := { p with eocDash := p.eocDash - 1 }
    if p.eocDash == 0 then p := { p with eocHit := -1 }
    return { p with dashDelay := p.dashDelay - 1 }
  if p.dashDelay < 0 then
    let s2 := max p.accRunSpeed p.maxRunSpeed
    let (s1, k1, k2, delay) : Float32 × Float32 × Float32 × Int :=
      if p.dash == 2 then (12, Float32.ofBits 0x3F7C28F6, Float32.ofBits 0x3F70A3D7, 30)
      else (12, Float32.ofBits 0x3F7DF3B6, Float32.ofBits 0x3F75C28F, 20)
    if p.dash > 0 then
      if vx > s1 || vx < -s1 then return setVx p (vx * k1)
      if vx > s2 || vx < -s2 then return setVx p (vx * k2)
      p := { p with dashDelay := delay }
      if vx < 0 || (vx == 0 && input.left) then return setVx p (-s2)
      if vx > 0 || (vx == 0 && input.right) then return setVx p s2
    return p
  -- dashDelay == 0: start a dash
  if p.dash == 1 || p.dash == 2 then
    let (p', dir, go) := commonDashHandle w p input
    p := p'
    if go then
      let speed : Float32 := if p.dash == 1 then Float32.ofBits 0x41873333 else Float32.ofBits 0x41680000  -- 16.9f, 14.5f
      p := setVx p (speed * Float32.ofInt dir)
      let b := p.body
      let center : Vector := ⟨b.position.x + single b.width / 2, b.position.y + single b.height / 2⟩
      let dx := Float32.ofInt ((dir * b.width).tdiv 2 + 2)
      let gd := b.gravityDirection
      let a : Vector := ⟨center.x + dx, center.y + (gd * Float32.ofInt (-(b.height : Int)) / 2 + gd * 2)⟩
      let c : Vector := ⟨center.x + dx, center.y + 0⟩
      if w.solid (tileOf a.x) (tileOf a.y) || w.solid (tileOf c.x) (tileOf c.y) then
        p := setVx p (p.body.velocity.x / 2)
      p := { p with dashDelay := -1 }
      if p.dash == 2 then p := { p with eocDash := 15 }
  return p

/-- `WallslideMovement` (no mount, not hovering). -/
def wallslideMovement (w : World) (p : Player) (input : Controls) : Player := Id.run do
  let p := { p with sliding := false }
  if p.slideDir == 0 || p.spikedBoots <= 0 then return p
  unless (input.left && p.slideDir == -1) || (input.right && p.slideDir == 1) do return p
  let b := p.body
  let mut x : Float32 := b.position.x
  if p.slideDir == 1 then x := x + Float32.ofInt b.width
  x := x + Float32.ofInt p.slideDir
  let mut y : Float32 := b.position.y + Float32.ofInt b.height + 1
  if b.gravityDirection < 0 then y := b.position.y - 1
  x := x / 16; y := y / 16
  let wall := w.solid x.toInt32.toInt y.toInt32.toInt && w.solid x.toInt32.toInt (y.toInt32.toInt - 1)
  let vy := b.velocity.y
  let gd := b.gravityDirection
  let fallStart := (b.position.y / 16).toInt32.toInt
  if p.spikedBoots >= 2 then
    unless wall do return p
    unless (vy > 0 && gd == 1) || (vy < b.gravity && gd == -1) do return p
    let g := b.gravity
    let y' := if input.down && gd == 1 then 4 * gd else (-g + Float32.ofBits 0x3727C5AC) * gd
    return { p with sliding := true, body := { b with fallStart, velocity := { b.velocity with y := y' } } }
  unless wall do return p
  unless (vy.toFloat > 0.5 && gd == 1) || (vy.toFloat < -0.5 && gd == -1) do return p
  let y' := if input.down then 4 * gd else Float32.ofBits 0x3F000000 * gd
  return { p with sliding := true, body := { b with fallStart, velocity := { b.velocity with y := y' } } }

/-- The `sandStorm` flag `DoubleJumpVisuals` sets while a sandstorm jump is still rising (IL_016b..02b8). -/
def doubleJumpVisuals (p : Player) : Player :=
  let b := p.body
  let rising := (b.gravityDirection == 1 && b.velocity.y < 0) || (b.gravityDirection == -1 && b.velocity.y > 0)
  if p.performing.sandstorm && p.has.sandstorm && !p.canJumpAgain.sandstorm && rising then { p with sandStorm := true }
  else p

/-- Update IL_81e0..8c80 for a dry, unmounted player: `gfxOffY` easing (IL_81e0..82ba), `SlopeDownMovement`,
`StepDown` / `StepUp`, `DryCollision`, `SlopingCollision`, the `StepConveyorBelt` round trip, `slideDir`, and the
ceiling stop. Same steps as the
released `OriginalPlayerMovement.State.finishOnFootWith` except its ±16 velocity clamp: in the CIL that clamp
(IL_8323..8367) belongs to the wet mount-49 jump branch (IL_82ba), so a 16.9 Tabi dash keeps its speed. -/
def finish (grid : Grid) (body : Body) : Body × Int := Id.run do
  let mut body := body
  let gravityDir := body.gravityDirection
  let x := body.velocity.x
  let step := (1 + x.abs / 3) * body.stepSpeed
  let g := body.gfxOffY
  let g := if g > 0 then (if g - step < 0 then 0 else g - step) else if g < 0 then (if g + step > 0 then 0 else g + step) else g
  let g := if g > 32 then 32 else if g < -32 then -32 else g
  body := { body with gfxOffY := g }
  body := body.slopeDownMovement grid
  if body.velocity.y == body.gravity then body := body.stepDown grid
  let canStepUp := if gravityDir == -1 then body.velocity.y ≤ body.gravity && !body.controlUp
    else body.velocity.y ≥ body.gravity && !body.controlDown
  if canStepUp then body := body.stepUp grid
  let before := body.velocity
  let fallThrough := body.controlDown
  body := body.dryCollision grid fallThrough false {}
  body := (body.slopingCollision grid fallThrough false).1
  -- IL_8c80 Collision.StepConveyorBelt (not shimmering / tongued / locked; no grapple, no pulley): with
  -- |gfxOffY| ≤ 2 it lowers the hitbox top by 5 for its tile scan and restores it — a float round trip
  -- y → (y + 5) − 5, which rounds when y + 5 reaches the next power of two. No conveyor tiles in scope.
  if !(body.gfxOffY.abs > 2) then
    body := { body with position := { body.position with y := (body.position.y + 5) - 5 } }
  let slideDir : Int := if before.x == body.velocity.x then 0
    else if before.x < 0 then -1 else if before.x > 0 then 1 else 0
  if (gravityDir == 1 && body.collision.up) || (gravityDir == -1 && body.collision.down) then
    body := { body with jump := 0, velocity := { body.velocity with y := 0.01 * gravityDir } }
  return (body, slideDir)

/-! ## One frame -/

/-- One local `Player.Update` slice with the given controls and accessories (armor slots 3..7). -/
def frame (w : World) (p : Player) (items : Array Nat) (input : Controls) : Except OutOfScope Player := do
  let mut p := { p with fallDamage := none }
  -- IL_0672 UpdateMiscCounter
  p := { p with miscCounter := if p.miscCounter + 1 >= 300 then 0 else p.miscCounter + 1 }
  -- IL_188d..1c06: fall bookkeeping with the previous update's values
  let gf := OriginalPlayerFall.gravityFactor p.body.position.y w.maxTilesX w.worldSurface false
  let fallInput : OriginalPlayerFall.Input := {
    position := p.body.position, velocity := p.body.velocity, width := p.body.width,
    height := p.body.height, fallStart := p.body.fallStart, jump := p.body.jump, extraFall := p.extraFall,
    noFallDmg := p.noFallDmg, gravityFactor := gf }
  let fall := OriginalPlayerFall.step fallInput w.tileAt
  p := { p with body := { p.body with fallStart := fall.fallStart }, fallDamage := fall.damage }
  -- IL_0031..0260 + IL_092a: stat setup (dry), gravity factor
  p := { p with
    maxFallSpeed := 10 + Float32.ofBits 0x3C23D70A, jumpHeight := 15, jumpSpeed := Float32.ofBits 0x40A051EC,
    maxRunSpeed := 3, runAcceleration := Float32.ofBits 0x3DA3D70A, runSlowdown := Float32.ofBits 0x3E4CCCCD,
    accRunSpeed := 3, onWrongGround := false, justJumped := false,
    body := { p.body with gravity := Float32.ofBits 0x3ECCCCCD * gf } }
  -- Update IL_037e and ResetEffects IL_0e73 → ResizeHitbox (unmounted, no stool): feet kept, 20 × 42 hitbox. The
  -- float round trip y + h − 42 rounds when the feet are in a higher binade than the head (the second call is a no-op).
  let feet := p.body.position.y + Float32.ofNat p.body.height
  p := { p with body := { p.body with position := { p.body.position with y := feet - 42 }, width := 20, height := 42 } }
  -- ResetEffects + UpdateEquips
  let (e, x) ← equip items
  if e.rocketBoots != 0 then throw (.state "rocket boots")
  let has : JumpKinds := {
    cloud := e.hasJumpOption_Cloud, sandstorm := e.hasJumpOption_Sandstorm,
    blizzard := e.hasJumpOption_Blizzard, fart := x.hasJumpOption_Fart, sail := x.hasJumpOption_Sail }
  p := { p with
    accRunSpeed := e.accRunSpeed, moveSpeed := e.moveSpeed, jumpBoost := e.jumpBoost,
    noFallDmg := e.noFallDmg, spikedBoots := e.spikedBoots, extraFall := e.extraFall,
    jumpSpeedBoost := e.jumpSpeedBoost, autoJump := e.autoJump, dashType := x.dashType, has }
  -- IL_3307: moveSpeed scaling (strongestMoveSpeedDebuff = 1)
  p := { p with runAcceleration := p.runAcceleration * p.moveSpeed, maxRunSpeed := p.maxRunSpeed * p.moveSpeed }
  p := updateJumpHeight p
  -- IL_34d0: double jumps
  if p.body.velocity.y == 0 || p.sliding then p := refreshDoubleJumps p
  else
    let c := p.canJumpAgain
    let kept : JumpKinds := {
      cloud := c.cloud && p.has.cloud, sandstorm := c.sandstorm && p.has.sandstorm,
      blizzard := c.blizzard && p.has.blizzard, fart := c.fart && p.has.fart, sail := c.sail && p.has.sail }
    p := { p with canJumpAgain := kept }
  -- IL_46af..4847: run boosts while a double jump plays
  if p.sandStorm then p := { p with runAcceleration := p.runAcceleration * 1.5, maxRunSpeed := p.maxRunSpeed * 2 }
  if p.performing.blizzard && p.has.blizzard then
    p := { p with runAcceleration := p.runAcceleration * 3, maxRunSpeed := p.maxRunSpeed * 1.5 }
  if p.performing.fart && p.has.fart then
    p := { p with runAcceleration := p.runAcceleration * 3, maxRunSpeed := p.maxRunSpeed * 1.75 }
  if p.performing.sail && p.has.sail then
    p := { p with runAcceleration := p.runAcceleration * 1.5, maxRunSpeed := p.maxRunSpeed * 1.25 }
  -- IL_4b5e HorizontalMovement (released): ChangeDir is blocked while sandStorm (IL_0269)
  let st : OriginalPlayerMovement.State := {
    body := p.body, direction := p.direction, releaseJump := p.releaseJump,
    sliding := p.sliding, slideDir := p.slideDir, autoJump := p.autoJump, runAcceleration := p.runAcceleration,
    runSlowdown := p.runSlowdown, maxRunSpeed := p.maxRunSpeed, accRunSpeed := p.accRunSpeed,
    maxFallSpeed := p.maxFallSpeed, jumpHeight := p.jumpHeight, jumpSpeed := p.jumpSpeed, dashDelay := p.dashDelay,
    onWrongGround := p.onWrongGround }
  let st := st.horizontalMovement { left := input.left, right := input.right, jump := input.jump, down := input.down }
    (canTurn := !p.sandStorm)
  p := { p with body := st.body, direction := st.direction, sandStorm := false }
  p := jumpMovement p input
  if p.body.jump == 0 then p := cancelJumpVisuals p
  p ← dashMovement w p input
  p := wallslideMovement w p input
  p := doubleJumpVisuals p
  -- IL_67f8: gravity and the fall-speed clamp (gravDir 1, no slow fall)
  let b := p.body
  let vy := b.velocity.y + b.gravity * b.gravityDirection
  let vy := if vy > p.maxFallSpeed then p.maxFallSpeed else vy
  p := { p with body := { b with velocity := { b.velocity with y := vy }, controlDown := input.down } }
  -- IL_71c6: release flags
  p := { p with releaseRight := !input.right, releaseLeft := !input.left }
  -- IL_81e0..8c80: step offset and collision through the released `Body` operations
  let (body, slideDir) := finish w.grid p.body
  p := { p with body, slideDir }
  -- IL_8e19 BordersMovement (not remix, no forced gravity; the bottom border is far below the scope's ground)
  let border : Float32 := Float32.ofInt 640
  let left := w.leftWorld + border
  if p.body.position.x < left then
    p := { p with body := { p.body with position := { p.body.position with x := left }, velocity := { p.body.velocity with x := 0 } } }
  let right := w.rightWorld - border - Float32.ofInt p.body.width
  if p.body.position.x > right then
    p := { p with body := { p.body with position := { p.body.position with x := right }, velocity := { p.body.velocity with x := 0 } } }
  if p.body.position.y < w.topWorld + border then
    let vy := if p.body.velocity.y < Float32.ofBits 0x3DE147AE then Float32.ofBits 0x3DE147AE else p.body.velocity.y
    p := { p with body := { p.body with position := { p.body.position with y := w.topWorld + border },
                                        velocity := { p.body.velocity with y := vy }, gravityDirection := 1 } }
  -- IL_8e54 PlayerFrame IL_1da9..1ddd: a sandstorm jump spins the player every fourth tick (no item animation;
  -- ChangeDir without a pulley only sets `direction`)
  if p.sandStorm && p.miscCounter % 4 == 0 then p := { p with direction := -p.direction }
  return p

end Lean4Terria.OriginalAccessoryMovement
