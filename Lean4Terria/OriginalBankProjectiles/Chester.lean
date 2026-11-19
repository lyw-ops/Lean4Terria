import Lean4Terria.OriginalPlayerCollision

/-!
# Chester (projectile 960, Eye Bone pet)

Handwritten from vanilla 1.4.5.8. Only the branches that projectile 960 actually
reaches are here: the rest of `Projectile.AI_026` (22 451 IL instructions) is
constant-folded away on `type == 960`, `aiStyle == 26`. That leaves 2 947
instructions.

* **Buff path.** `Player.ResetEffects` clears `petFlagChesterPet`. Then
  `Player.UpdateBuffs` runs buff 331 through `BuffHandle_SpawnPetIfNeededAndSetTime`:
  it sets `buffTime = 18000` and the flag, and spawns a Chester at the player
  centre when none is owned.
* **`AI_026` for 960.** A dead owner clears the flag, and the flag sets
  `timeLeft = 2`. Then:
  * walk intent: the target is the owner centre −70·dir, with a ±10 px dead band;
  * teleport above 2000 px; switch to flying above 500 px or 300 px vertical;
  * fly mode (`ai0 = 1`): no tile collision, return to walking within 200 px;
  * walk mode: acceleration, `Collision.StepUp`, a 5.1 to 11.1 jump against a
    wall, a 6 px speed cap, direction;
  * the Chester animation: walk 5–12, idle 0–3, opening 13→14–18, closing 19,
    airborne 4;
  * gravity 0.4, capped at 10.
* **Movement.**
  * `Projectile.Update`: `gfxOffY` easing, then AI, `oldPosition`, `HandleMovement`,
    `timeLeft`.
  * `Projectile.HandleMovement` (dry, `tileCollide`): `Collision.TileCollision`
    with fall-through, closed doors passable and hoik. `correctSlopeCollision` is
    true for 960, so `Collision.SlopeCollision` runs in `UpdatePosition` after
    `position += velocity`, with `fall = false`.

Reuses `OriginalCollision.tileCollision`, `slopeCollision` and
`OriginalPlayerCollision.Body.stepUp` (`Collision.StepUp` with
`holdsMatching = false`).

Not modelled:
* wet, honey and shimmer movement;
* the sub-stepped `HandleMovement` path, taken only when the speed exceeds
  16 px per frame;
* the petting states (`ai0 = −1/−2`);
* `Main.rand.Next(3)` drawn on tile contact (tile cutting needs damage > 0);
* dust and light.
-/

namespace Lean4Terria.OriginalBankProjectiles.Chester

open OriginalCollision (Vector Grid single tileIndex tileCollision slopeCollision)
open OriginalWorld (Tile TileShape)

def width : Nat := 20
def height : Nat := 20
def projectileId : Nat := 960
def buffId : Nat := 331
def lifetime : Nat := 18000
/-- `Main.projFrames[960]`. -/
def frames : Nat := 20

/-- What `AI_026` and `HandleMovement` read from `Main.player[owner]` this frame. -/
structure Owner where
  position : Vector
  velocity : Vector
  width : Nat := 20
  height : Nat := 42
  direction : Int := 1
  active : Bool := true
  dead : Bool := false
  /-- `petFlagChesterPet` after `ResetEffects` and `UpdateBuffs` this frame. -/
  petFlag : Bool := true
  deriving Repr

structure Pet where
  active : Bool := true
  position : Vector
  velocity : Vector
  oldPosition : Vector := ⟨0, 0⟩
  timeLeft : Nat := lifetime
  /-- 0 walks, 1 flies back. -/
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  localAI0 : Float32 := 0
  localAI1 : Float32 := 0
  frame : Nat := 0
  frameCounter : Nat := 0
  direction : Int := 1
  spriteDirection : Int := 1
  rotation : Float32 := 0
  tileCollide : Bool := true
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 1
  deriving BEq, Repr

/-! ## World queries (`WorldGen` and `Collision`) -/

private def inBounds (grid : Grid) (x y : Int) : Bool :=
  x ≥ 0 && y ≥ 0 && x < grid.width && y < grid.height

/-- `WorldGen.InWorld(x, y, fluff)`. -/
def inWorld (grid : Grid) (x y : Int) (fluff : Int := 0) : Bool :=
  x ≥ fluff && x < (grid.width : Int) - fluff && y ≥ fluff && y < (grid.height : Int) - fluff

private def tileAt (grid : Grid) (x y : Int) : Tile := grid.tileAt x.toNat y.toNat

private def rulesOf (grid : Grid) (tile : Tile) : OriginalCollision.TileRules :=
  match tile.id with
  | some id => grid.rules id
  | none => {}

private def isSlope (tile : Tile) : Bool := match tile.shape with
  | .slope id => id != 0
  | _ => false

private def isTopSlope (tile : Tile) : Bool := tile.shape == .slope 1 || tile.shape == .slope 2

/-- `WorldGen.SolidTile(x, y, false)`. An index outside the map throws inside
the `try`, which yields false. -/
def solidTile (grid : Grid) (x y : Int) : Bool :=
  inBounds grid x y &&
    let tile := tileAt grid x y
    let rules := rulesOf grid tile
    tile.id.isSome && !tile.actuated && rules.solid && !rules.solidTop &&
      tile.shape != .halfBlock && !isSlope tile

/-- `WorldGen.PlatformProperTopFrame` with `PlatformFrameWidth() = 18`. -/
private def platformProperTopFrame (frameX : Int) : Bool :=
  let v := frameX.tdiv 18
  (0 ≤ v && v ≤ 7) || (12 ≤ v && v ≤ 16) || (25 ≤ v && v ≤ 26)

/-- `WorldGen.SolidTileAllowBottomSlope`. Outside the world counts as solid. -/
def solidTileAllowBottomSlope (grid : Grid) (x y : Int) : Bool :=
  if !inWorld grid x y then true else
  let tile := tileAt grid x y
  let rules := rulesOf grid tile
  let frameX := (tile.frame.map (·.1)).getD 0
  tile.id.isSome && (rules.solid || rules.solidTop) &&
    (!isTopSlope tile || (rules.platform && platformProperTopFrame frameX)) &&
    tile.shape != .halfBlock && !tile.actuated

private def clampInt (v lo hi : Int) : Int := if v < lo then lo else if v > hi then hi else v

/-- `Collision.SolidCollision(position, width, height)`. -/
def solidCollision (grid : Grid) (pos : Vector) (w h : Nat) : Bool := Id.run do
  let x0 := clampInt ((pos.x / 16).toInt32.toInt - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (((pos.x + single w) / 16).toInt32.toInt + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt ((pos.y / 16).toInt32.toInt - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (((pos.y + single h) / 16).toInt32.toInt + 2) 0 ((grid.height : Int) - 40)
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      let rules := rulesOf grid tile
      if tile.actuated || tile.id.isNone || !rules.solid || rules.solidTop then continue
      let tx := single (x * 16 : Nat)
      let half := tile.shape == .halfBlock
      let ty := single (y * 16 : Nat) + (if half then 8 else 0)
      let th : Float32 := if half then 8 else 16
      if pos.x + single w > tx && pos.x < tx + 16 && pos.y + single h > ty && pos.y < ty + th then
        return true
  return false

/-! ## AI_026 for type 960 -/

/-- `Entity.Center` uses integer halves. -/
private def centerOf (position : Vector) (w h : Nat) : Vector :=
  ⟨position.x + single (w / 2 : Nat), position.y + single (h / 2 : Nat)⟩

def Owner.center (owner : Owner) : Vector := centerOf owner.position owner.width owner.height

/-- `Utils.SafeNormalize(v, -Vector2.UnitY).ToRotation() + π/2`. -/
private def flyRotation (v : Vector) : Float32 :=
  let n : Vector :=
    if (v.x == 0 && v.y == 0) || v.x.isNaN || v.y.isNaN then ⟨0, -1⟩
    else
      let k := 1 / Float32.sqrt (v.x * v.x + v.y * v.y)
      ⟨v.x * k, v.y * k⟩
  (Float.atan2 n.y.toFloat n.x.toFloat).toFloat32 + 1.5707963705062866

private def sign (v : Float32) : Int := if v > 0 then 1 else if v < 0 then -1 else 0

/-- `tx = (int)(position.X + (float)(width / 2)) / 16`, with the C# truncations. -/
private def centerTileX (p : Pet) : Int := (p.position.x + single (width / 2 : Nat)).toInt32.toInt.tdiv 16

/-- The Chester frame block (IL_b154). `vy` is the vertical speed before gravity. -/
private def animate (p : Pet) (owner : Owner) (notMoved tracked : Bool) : Pet := Id.run do
  let mut p := p
  if p.velocity.y != 0 then
    return { p with localAI0 := 0, localAI1 := 0, frameCounter := 0, frame := 4 }
  if notMoved then
    let cx := (centerOf p.position width height).x
    p := { p with spriteDirection := if owner.center.x < cx then -1 else 1 }
    if 5 ≤ p.frame && p.frame < 12 then
      p := { p with frameCounter := p.frameCounter + 1 }
      if p.frameCounter > 3 then p := { p with frame := p.frame + 1, frameCounter := 0 }
      if p.frame ≥ 12 then p := { p with frame := 0 }
      return p
    if tracked then
      if p.localAI0 == 0 then
        if p.localAI1 == 0 then p := { p with localAI1 := 1, frameCounter := 0 }
        p := { p with frame := 13, frameCounter := p.frameCounter + 1 }
        if p.frameCounter > 6 then p := { p with localAI0 := 1, frame := 14, frameCounter := 0 }
        return p
      p := { p with frameCounter := p.frameCounter + 1 }
      if p.frameCounter > 6 then
        p := { p with frame := if p.frame + 1 > 18 then 14 else p.frame + 1, frameCounter := 0 }
      return p
    p := { p with localAI0 := 0 }
    if p.localAI1 == 1 then p := { p with localAI1 := 0, frameCounter := 0 }
    if 12 ≤ p.frame && p.frame ≤ 19 then
      p := { p with frame := 19, frameCounter := p.frameCounter + 1 }
      if p.frameCounter > 6 then p := { p with frame := 0, frameCounter := 0 }
      return p
    p := { p with frameCounter := p.frameCounter + 1 }
    if p.frameCounter ≥ 24 then p := { p with frameCounter := 0 }
    return { p with frame := p.frameCounter / 6 }
  -- moving
  let speed := Float32.sqrt (p.velocity.x * p.velocity.x + p.velocity.y * p.velocity.y)
  let step := (if 2 > speed then (2 : Float32) else speed).toInt32.toInt.toNat
  p := { p with localAI0 := 0, localAI1 := 0, frameCounter := p.frameCounter + step }
  if p.frameCounter > 6 then p := { p with frame := p.frame + 1, frameCounter := 0 }
  if p.frame < 5 || p.frame > 12 then p := { p with frame := 5 }
  return p

/-- The jump against a blocking wall (IL_a0cc..a2d2). -/
private def jump (grid : Grid) (p : Pet) (left right : Bool) : Pet := Id.run do
  let tx := centerTileX p
  let ty := (p.position.y + single height).toInt32.toInt.tdiv 16
  let foot := tileAt grid tx ty
  let support := solidTileAllowBottomSlope grid tx ty ||
    (inBounds grid tx ty && (foot.shape == .halfBlock || isSlope foot))
  if !support then return p
  let mut x := tx
  let y := (p.position.y + single (height / 2 : Nat)).toInt32.toInt.tdiv 16
  if left then x := x - 1
  if right then x := x + 1
  x := x + p.velocity.x.toInt32.toInt
  let vy : Float32 :=
    if !solidTile grid x (y - 1) && !solidTile grid x (y - 2) then -5.1
    else if !solidTile grid x (y - 2) then -7.1
    else if solidTile grid x (y - 5) then -11.1
    else if solidTile grid x (y - 4) then -10.1
    else -9.1
  return { p with velocity := ⟨p.velocity.x, vy⟩ }

/-- `Projectile.AI_026` as reached by type 960. `tracked` is
`Chester_IsAnyPlayerTrackingThisProjectile()` (its storage is open). -/
def ai (grid : Grid) (owner : Owner) (pet : Pet) (tracked : Bool := false) : Pet := Id.run do
  if !owner.active then return { pet with active := false }
  let mut p := pet
  -- buff path: a dead owner clears petFlagChesterPet; the flag keeps the pet alive
  if !owner.dead && owner.petFlag then p := { p with timeLeft := 2 }
  -- walking intent: aim 70 px behind the owner, ±10 px dead band
  let ownerCenter := owner.center
  let cx := (centerOf p.position width height).x
  let mut dirTo : Int := if ownerCenter.x - cx > 0 then 1 else -1
  if owner.velocity.x != 0 then dirTo := owner.direction
  let target := ownerCenter.x + single (-70 * dirTo)
  let mut left := false
  let mut right := false
  if target < cx - 10 then left := true
  else if target > cx + 10 then right := true
  -- distance rules (IL_494c)
  let center : Vector := ⟨p.position.x + single width * 0.5, p.position.y + single height * 0.5⟩
  let dx := owner.position.x + single (owner.width / 2 : Nat) - center.x
  let dy := owner.position.y + single (owner.height / 2 : Nat) - center.y
  let dist := Float32.sqrt (dx * dx + dy * dy)
  if dist > 2000 then
    p := { p with position := ⟨owner.position.x + single (owner.width / 2 : Nat) - single (width / 2 : Nat),
                               owner.position.y + single (owner.height / 2 : Nat) - single (height / 2 : Nat)⟩ }
  else if dist > 500 || dy.abs > 300 then
    if dy > 0 && p.velocity.y < 0 then p := { p with velocity := ⟨p.velocity.x, 0⟩ }
    if dy < 0 && p.velocity.y > 0 then p := { p with velocity := ⟨p.velocity.x, 0⟩ }
    p := { p with ai0 := 1 }
  if p.ai0 != 0 then
    -- fly back (IL_4fef)
    p := { p with tileCollide := false }
    let c : Vector := ⟨p.position.x + single width * 0.5, p.position.y + single height * 0.5⟩
    let fx := owner.position.x + single (owner.width / 2 : Nat) - c.x
    let fy := owner.position.y + single (owner.height / 2 : Nat) - c.y
    let d := Float32.sqrt (fx * fx + fy * fy)
    let acc : Float32 := 0.4
    let ownerSpeed := owner.velocity.x.abs + owner.velocity.y.abs
    let speed : Float32 := if 12 < ownerSpeed then ownerSpeed else 12
    if d < 200 && owner.velocity.y == 0 &&
        p.position.y + single height ≤ owner.position.y + single owner.height &&
        !solidCollision grid p.position width height then
      p := { p with ai0 := 0 }
      if p.velocity.y < -6 then p := { p with velocity := ⟨p.velocity.x, -6⟩ }
    let (tx, ty) := if d < 60 then (p.velocity.x, p.velocity.y)
      else let k := speed / d; (fx * k, fy * k)
    let mut vx := p.velocity.x
    let mut vy := p.velocity.y
    if vx < tx then
      vx := vx + acc
      if vx < 0 then vx := vx + acc * 1.5
    if vx > tx then
      vx := vx - acc
      if vx > 0 then vx := vx - acc * 1.5
    if vy < ty then
      vy := vy + acc
      if vy < 0 then vy := vy + acc * 1.5
    if vy > ty then
      vy := vy - acc
      if vy > 0 then vy := vy - acc * 1.5
    p := { p with velocity := ⟨vx, vy⟩ }
    if vx.toFloat > 0.5 then p := { p with spriteDirection := -1 }
    else if vx.toFloat < -0.5 then p := { p with spriteDirection := 1 }
    if vx != 0 then p := { p with spriteDirection := sign vx }
    return { p with frame := 4, frameCounter := 0, rotation := flyRotation p.velocity }
  -- walk (IL_8d23)
  if p.ai1 != 0 then
    left := false
    right := false
  p := { p with rotation := 0, tileCollide := true }
  let ownerSpeed := owner.velocity.x.abs + owner.velocity.y.abs
  let (maxSpeed, acc) : Float32 × Float32 := if 6 < ownerSpeed then (ownerSpeed, 0.3) else (6, 0.2)
  let mut vx := p.velocity.x
  if left then
    vx := if vx.toFloat > -3.5 then vx - acc else vx - acc * 0.25
  else if right then
    vx := if vx.toFloat < 3.5 then vx + acc else vx + acc * 0.25
  else
    vx := vx * 0.9
    if !(vx < -acc) && !(vx > acc) then vx := 0
  p := { p with velocity := ⟨vx, p.velocity.y⟩ }
  -- a solid tile ahead (IL_9e27)
  let mut blocked := false
  if left || right then
    let mut x := centerTileX p
    let y := (p.position.y + single (height / 2 : Nat)).toInt32.toInt.tdiv 16
    if left then x := x - 1
    if right then x := x + 1
    x := x + p.velocity.x.toInt32.toInt
    if inWorld grid x y && solidTile grid x y && !(rulesOf grid (tileAt grid x y)).closedDoor then
      blocked := true
  let ownerBelow := owner.position.y + single owner.height - 8 > p.position.y + single height
  -- Collision.StepUp(ref position, ref velocity, 20, 20, ref stepSpeed, ref gfxOffY, 1, false, 0)
  let body : OriginalPlayerCollision.Body :=
    { position := p.position, velocity := p.velocity, width, height,
      gfxOffY := p.gfxOffY, stepSpeed := p.stepSpeed }
  let stepped := body.stepUp grid
  p := { p with position := stepped.position, velocity := stepped.velocity,
                gfxOffY := stepped.gfxOffY, stepSpeed := stepped.stepSpeed }
  if p.velocity.y == 0 then
    let _ := ownerBelow   -- the SolidTile probe under !ownerBelow has no effect
    if blocked then p := jump grid p left right
  -- speed cap and facing
  if p.velocity.x > maxSpeed then p := { p with velocity := ⟨maxSpeed, p.velocity.y⟩ }
  if p.velocity.x < -maxSpeed then p := { p with velocity := ⟨-maxSpeed, p.velocity.y⟩ }
  let vxNow := p.velocity.x
  if vxNow < 0 then p := { p with direction := -1 }
  if vxNow > 0 then p := { p with direction := 1 }
  if vxNow > acc && right then p := { p with direction := 1 }
  if vxNow < -acc && left then p := { p with direction := -1 }
  p := { p with spriteDirection := p.direction }
  let notMoved := p.position.x - p.oldPosition.x == 0
  p := animate p owner notMoved tracked
  -- gravity
  let vy := p.velocity.y + 0.4
  return { p with velocity := ⟨p.velocity.x, if vy > 10 then 10 else vy⟩ }

/-! ## Projectile.Update for type 960 -/

/-- `HandleMovement` (dry) followed by `UpdatePosition`. Projectile 960 has
`correctSlopeCollision = true`. `HandleMovement` therefore runs only
`TileCollision`, and `UpdatePosition` runs `SlopeCollision` on the moved
position afterwards, with `fall = false`. -/
def handleMovement (grid : Grid) (owner : Owner) (p : Pet) : Pet := Id.run do
  let mut p := p
  if p.tileCollide then
    let fallThrough := owner.position.y + single owner.height - 12 > p.position.y + single height
    -- GetCollisionParams: (0.5, 0.5), 20 x 20; the collision box is Center - size * 0.5
    let c := centerOf p.position width height
    let topLeft : Vector := ⟨c.x - single width * 0.5, c.y - single height * 0.5⟩
    let v := (tileCollision grid topLeft p.velocity width height
      { fallThrough, fall2 := fallThrough, ignoreDoors := true, hoik := true }).displacement
    p := { p with velocity := v }
  -- UpdatePosition (dry)
  p := { p with position := p.position + p.velocity }
  if p.tileCollide then
    let size : Vector := ⟨single width / 2, single height / 2⟩
    let off : Vector := ⟨size.x - single width * 0.5, size.y - single height * 0.5⟩
    let moved := p.position + off
    let s := slopeCollision grid moved p.velocity width height (fall := false)
    p := { p with position := ⟨s.position.x - off.x, s.position.y - off.y⟩, velocity := s.velocity }
  return p

/-- One `Projectile.Update` of a Chester. -/
def update (grid : Grid) (owner : Owner) (pet : Pet) (tracked : Bool := false) : Pet := Id.run do
  if !pet.active then return pet
  let mut p := pet
  -- gfxOffY easing before the AI
  let k : Float32 := 1 + p.velocity.x.abs / 3
  if p.gfxOffY > 0 then
    let g := p.gfxOffY - k * p.stepSpeed
    p := { p with gfxOffY := if g < 0 then 0 else g }
  else if p.gfxOffY < 0 then
    let g := p.gfxOffY + k * p.stepSpeed
    p := { p with gfxOffY := if g > 0 then 0 else g }
  p := ai grid owner p tracked
  if !p.active then return p
  p := { p with oldPosition := p.position }
  p := handleMovement grid owner p
  let timeLeft := p.timeLeft - 1
  if timeLeft == 0 then return { p with active := false, timeLeft := 0 }
  return { p with timeLeft }

/-! ## Buff 331 -/

/-- Buff 331 on the owner: `buffTime` (0 when absent). -/
structure Buff where
  time : Nat := 0
  deriving BEq, Repr

/-- `UpdateBuffs` for buff 331: the time is reset to 18000 and the pet flag
raised. `spawn` asks for a Chester at the player centre when none is owned. -/
def Buff.update (buff : Buff) (ownedChesters : Nat) : Buff × Bool × Bool :=
  if buff.time == 0 then (buff, false, false)
  else ({ time := 18000 }, true, ownedChesters == 0)

/-- `Item.buffTime` of the Eye Bone, applied by `AddBuff` when the item is used. -/
def eyeBoneBuffTime : Nat := 3600

/-- A Chester spawned by `NewProjectile` at `origin` (centre), velocity `velocity`. -/
def spawn (origin velocity : Vector) : Pet :=
  { position := ⟨origin.x - single width * 0.5, origin.y - single height * 0.5⟩, velocity }

end Lean4Terria.OriginalBankProjectiles.Chester
