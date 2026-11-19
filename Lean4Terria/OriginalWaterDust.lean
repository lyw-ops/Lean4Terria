import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalRainAtmosphere
import Lean4Terria.OriginalForestChatLayout

/-!
# Water dust (`Dust.dustWater()` types) of desktop Terraria 1.4.5.8

The eleven dust types `Dust.dustWater()` can return — 33, 98‥105, 123 and 288 — from their
birth in `Dust.NewDust`, through the `Rain.Update` splash post-block, `Dust.UpdateDust`,
`Dust.GetAlpha` / `Dust.GetColor` to their `Main.DrawDust` sprites and active write-back.

Read from the original CIL (dumps and reachability in `.lake/original-water-dust/`):

* `NewDust`: menu / paused / world-generation / server gates, the camera rectangle widened by
  `400 * (1 - dCount)`, the first inactive of the 6000 physical slots, the `maxDustToDraw`
  pressure draws, then the ordinary constructor with the atlas wrap (`frame.X -= 1000`,
  `frame.Y += 30` per hundred types) and, for 33 and 98‥105 only, `alpha = 170`,
  `velocity *= 0.5`, `velocity.Y += 1`. 123 and 288 keep the requested alpha and velocity.
  The child-safety branch is unreachable for these types (`SafeDust` is true for all
  eleven; checked natively), so both settings behave identically and draw nothing extra.
* `UpdateDust`: the per-slot common head (budget clear, `dCount += 1`, `scale > 10`), the
  ordinary motion, the water block `IL_444d‥45b5` (`SolidCollision(position, 2, 2)`,
  `WetCollision(position, 4, 4)` with its `Collision.honey/shimmer` side effects, alpha +2,
  `velocity.Y ≤ 4`, the no-gravity spin) and the common tail with the running `dCount`
  thresholds. The method's own tail always rewrites `dCount` from a counter that is never
  incremented (`IL_5bbe`), i.e. to 0 for any non-negative `maxDustToDraw`.
* `GetAlpha`, `GetColor`, `GetVisualScale`, `GetVisualRotation` and `Main.DrawDust` take the
  ordinary path for all eleven types (only fall-through range tests differ from type 51).

The whole physical table is shared with every other dust cohort: the caller keeps ONE
`DustPool` (occupancy of all 6000 slots, `dCount`, `maxDustToDraw`) and ONE sentinel
`Main.dust[6000]`. This module owns only the slots it gave birth to.
-/
namespace Lean4Terria.OriginalWaterDust

open OriginalCollision (Vector Grid)
open OriginalForestGuideArrow (DustPool DustFrame)
open OriginalRainAtmosphere (SplashRequest WetFlags)

private def i32 (v : Int) : Int := (Int32.ofInt v).toInt
private def f32 (v : Int) : Float32 := (Int32.ofInt v).toFloat32
/-- `conv.i4` of a finite in-range `Single` (truncation toward zero). -/
private def convI4 (x : Float32) : Int := x.toInt32.toInt

/-- Physical `Main.dust` slots scanned by `NewDust` / `UpdateDust`. -/
def physicalSlots : Nat := 6000
/-- The index `NewDust` returns when nothing was born: `Main.dust[6000]`, a real object. -/
def sentinelSlot : Nat := 6000

def waterTypes : Array Nat := #[33, 98, 99, 100, 101, 102, 103, 104, 105, 123, 288]
def supports (typeId : Nat) : Bool := waterTypes.contains typeId

/-- `Dust.dustWater()`: `switch (Main.waterStyle - 2)`. -/
def dustWater (waterStyle : Int) : Nat :=
  match waterStyle with
  | 2 => 98 | 3 => 99 | 4 => 100 | 5 => 101 | 6 => 102 | 7 => 103 | 8 => 104 | 9 => 105
  | 10 => 123 | 12 => 288 | _ => 33

/-- Every field of a `Terraria.Dust` object except `dustIndex` (no method here reads it).
The defaults are those of `new Dust()` (checked natively). `color` is the native FNA
`Color.PackedValue` (R | G<<8 | B<<16 | A<<24). Inactive objects keep all their fields. -/
structure Particle where
  active : Bool := false
  typeId : Nat := 0
  position : Vector := ⟨0, 0⟩
  velocity : Vector := ⟨0, 0⟩
  fadeIn : Float32 := 0
  noGravity : Bool := false
  scale : Float32 := 0
  rotation : Float32 := 0
  noLight : Bool := false
  noLightEmittance : Bool := false
  fullBright : Bool := false
  color : UInt32 := 0
  alpha : Int := 0
  frame : DustFrame := ⟨0, 0, 0, 0⟩
  shaderNull : Bool := true
  customDataNull : Bool := true
  firstFrame : Bool := false
  deriving Repr

/-- One physical slot owned by this cohort: its index and the full object state. -/
structure Owned where
  slot : Nat
  dust : Particle
  deriving Repr

/-- `pool`: the ONE shared table view (occupancy of all cohorts, `Dust.dCount` as `density`,
`Main.maxDustToDraw` as `drawBudget`; `viewport` is not used — creation reads the live camera
from `BirthContext`). `water`: records of the slots this cohort was born into, ascending, one
per slot; inactive records keep their last fields until the slot is born into again.
`sentinel`: the global `Main.dust[6000]`, shared with every other `NewDust` caller. -/
structure State where
  pool : DustPool := {}
  water : Array Owned := #[]
  sentinel : Particle := {}
  deriving Inhabited

def State.find? (state : State) (slot : Nat) : Option Particle :=
  (state.water.find? (·.slot == slot)).map (·.dust)

/-- Forget this cohort's (inactive) record of a slot another cohort has been born into.
Optional: inactive records never touch shared state; this only keeps `water` small. -/
def State.release (state : State) (slot : Nat) : State :=
  { state with water := state.water.filter (fun o => o.slot != slot || o.dust.active) }

/-- Read the object `NewDust` returned (an owned slot or the sentinel). -/
def State.dustAt? (state : State) (slot : Nat) : Option Particle :=
  if slot == sentinelSlot then some state.sentinel else state.find? slot

private def State.put (state : State) (slot : Nat) (dust : Particle) : State :=
  if slot == sentinelSlot then { state with sentinel := dust } else
    let rest := state.water.filter (·.slot != slot)
    let water := (rest.push ⟨slot, dust⟩).qsort (fun a b => a.slot < b.slot)
    { state with water }

private def occupiedAt (pool : DustPool) (slot : Nat) : Bool := pool.occupied.getD slot false

private def setOccupied (pool : DustPool) (slot : Nat) (value : Bool) : DustPool :=
  let occupied := pool.occupied ++ Array.replicate (slot + 1 - pool.occupied.size) false
  { pool with occupied := occupied.set! slot value }

private def checkPool (pool : DustPool) : Except String Unit := do
  if pool.capacity != physicalSlots then throw "OriginalWaterDust: the desktop dust table has 6000 physical slots"
  if pool.occupied.size > physicalSlots then throw "OriginalWaterDust: occupancy longer than 6000 physical slots"

/-! ## Birth: `Dust.NewDust` -/

/-- Live statics `NewDust` reads. `Main.rand` is the caller's `UnifiedRandom`. -/
structure BirthContext where
  gameMenu : Bool := false
  gamePaused : Bool := false
  /-- `WorldGen.isGeneratingOrLoadingWorld`. -/
  generatingOrLoading : Bool := false
  netMode : Int := 0
  /-- The creation-time `Main.screenPosition`, `screenWidth`, `screenHeight`. -/
  screenPosition : Vector
  screenWidth : Int
  screenHeight : Int
  /-- `ChildSafety.Disabled`; irrelevant for these types (none is `DangerousDust`). -/
  childSafetyDisabled : Bool := true
  deriving Repr

/-- `NewDust(position, width, height, type, speedX, speedY, alpha, color, scale)`. -/
structure Request where
  position : Vector
  width : Int
  height : Int
  typeId : Nat
  speed : Vector := ⟨0, 0⟩
  alpha : Int := 0
  /-- `Color.PackedValue`. -/
  color : UInt32 := 0
  scale : Float32 := 1
  deriving Repr

def Request.ofSplash (r : SplashRequest) : Request :=
  { position := r.position, width := r.width, height := r.height, typeId := r.typeId,
    speed := r.speed, alpha := r.alpha, color := r.color, scale := r.scale }

inductive Outcome where
  | born
  | menu | paused | generating | server
  /-- The 10×10 request box missed the camera rectangle. -/
  | offscreen
  /-- All 6000 physical slots active; no draw. -/
  | full
  /-- A `maxDustToDraw` pressure draw refused the free slot (one draw consumed). -/
  | pressure
  deriving Repr, BEq, DecidableEq

structure BirthResult where
  state : State
  /-- The actual `NewDust` return value: the new slot, or 6000. -/
  returnedSlot : Nat
  outcome : Outcome
  random : UnifiedRandom

/-- The ordinary constructor `IL_01df‥0398` plus the water overrides `IL_06a0‥0753`. -/
private def construct (r : Request) (random : UnifiedRandom) : Particle × UnifiedRandom := Id.run do
  let w := if r.width < 5 then 5 else r.width
  let h := if r.height < 5 then 5 else r.height
  let (dx, random) := random.next 0 (w - 4)
  let px := r.position.x + f32 dx + 4
  let (dy, random) := random.next 0 (h - 4)
  let py := r.position.y + f32 dy + 4
  let (vx, random) := random.next (-20) 21
  let vx := f32 vx * 0.1 + r.speed.x
  let (vy, random) := random.next (-20) 21
  let vy := f32 vy * 0.1 + r.speed.y
  let (row, random) := random.next 0 3
  -- frame.X = 10 * type; then per whole hundred of the type: X -= 1000, Y += 30.
  let mut fx : Int := i32 (10 * r.typeId)
  let mut fy : Int := i32 (10 * row)
  let mut t : Int := r.typeId
  while t ≥ 100 do
    t := t - 100
    fx := i32 (fx - 1000)
    fy := i32 (fy + 30)
  let (s, random) := random.next (-20) 21
  let scale := (1 + f32 s * 0.01) * r.scale
  let mut dust : Particle := {
    active := true, typeId := r.typeId, position := ⟨px, py⟩, velocity := ⟨vx, vy⟩,
    fadeIn := 0, noGravity := false, color := r.color, alpha := r.alpha,
    frame := ⟨fx, fy, 8, 8⟩, shaderNull := true, customDataNull := true,
    noLightEmittance := false, fullBright := false, rotation := 0, scale,
    noLight := false, firstFrame := true }
  if r.typeId == 33 || (98 ≤ r.typeId && r.typeId ≤ 105) then
    let v : Vector := ⟨dust.velocity.x * 0.5, dust.velocity.y * 0.5⟩
    dust := { dust with alpha := 170, velocity := ⟨v.x, v.y + 1⟩ }
  return (dust, random)

/-- Complete `Dust.NewDust` for a water type: every gate, the pool scan and pressure draws,
the constructor. On every refusal the returned slot is 6000 and NO object changes; only a
pressure refusal consumes one draw. A birth marks the slot in the shared occupancy and may
reset `dCount` (below half of `maxDustToDraw`). The returned slot is the caller's handle;
apply caller post-blocks to `state.dustAt? returnedSlot`, including the sentinel. -/
def birth (state : State) (c : BirthContext) (r : Request) (random : UnifiedRandom) :
    Except String BirthResult := do
  checkPool state.pool
  unless supports r.typeId do throw s!"OriginalWaterDust: type {r.typeId} is not a dustWater() type"
  let refuse (outcome : Outcome) (random : UnifiedRandom) : BirthResult :=
    { state, returnedSlot := sentinelSlot, outcome, random }
  if c.gameMenu then return refuse .menu random
  if c.gamePaused then return refuse .paused random
  if c.generatingOrLoading then return refuse .generating random
  if c.netMode == 2 then return refuse .server random
  let pool := state.pool
  let margin := convI4 (400 * (1 - pool.density))
  let sx := convI4 (c.screenPosition.x - f32 margin)
  let sy := convI4 (c.screenPosition.y - f32 margin)
  let sw := i32 (c.screenWidth + i32 (margin * 2))
  let sh := i32 (c.screenHeight + i32 (margin * 2))
  let px := convI4 r.position.x
  let py := convI4 r.position.y
  unless px < i32 (sx + sw) && sx < i32 (px + 10) && py < i32 (sy + sh) && sy < i32 (py + 10) do
    return refuse .offscreen random
  let some slot := (List.range physicalSlots).find? (fun i => !occupiedAt pool i) |
    return refuse .full random
  let budget := (pool.drawBudget : Int).toFloat
  let index := slot.toFloat
  let mut random := random
  let mut density := pool.density
  -- IL_0107‥01da: (double)slot > (double)maxDustToDraw * k
  let pressure : Option (Nat × (Int → Bool)) :=
    if index > budget * 0.9 then some (4, (· == 0))
    else if index > budget * 0.8 then some (3, (· == 0))
    else if index > budget * 0.7 then some (2, (· != 0))
    else if index > budget * 0.6 then some (4, (· != 0))
    else if index > budget * 0.5 then some (5, (· != 0))
    else none
  match pressure with
  | some (bound, keeps) =>
    let (roll, next) := random.next 0 bound
    random := next
    unless keeps roll do return refuse .pressure random
  | none => density := 0
  let (dust, after) := construct r random
  let pool := { setOccupied pool slot true with density }
  return { state := ({ state with pool }).put slot dust, returnedSlot := slot, outcome := .born, random := after }

/-! ## Rain splash post-block -/

/-- `Rain.Update IL_012a‥01ed` on `Main.dust[returnedSlot]`, which may be the sentinel:
position (-2, +2), alpha 38, `velocity = velocity * .1 + (-rainVelocity) * .025`, then
`velocity.Y -= 2`, scale .6, noGravity. No other field, not `active`, changes. -/
def splashPost (dust : Particle) (rainVelocity : Vector) : Particle :=
  let position : Vector := ⟨dust.position.x - 2, dust.position.y + 2⟩
  let scaled : Vector := ⟨dust.velocity.x * 0.1, dust.velocity.y * 0.1⟩
  let push : Vector := ⟨-rainVelocity.x * 0.025, -rainVelocity.y * 0.025⟩
  let velocity : Vector := ⟨scaled.x + push.x, scaled.y + push.y - 2⟩
  { dust with position, alpha := 38, velocity, scale := 0.6, noGravity := true }

/-- The `Rain.Update` callee and post-block at a `pendingSplash`: exactly ONE `NewDust`
from the given entry generator, then the post-block on the actual returned object, also
when that is the sentinel 6000. Never call it twice for one pending splash. -/
def splash (state : State) (c : BirthContext) (request : SplashRequest) (random : UnifiedRandom) :
    Except String BirthResult := do
  let result ← birth state c (Request.ofSplash request) random
  let some dust := result.state.dustAt? result.returnedSlot |
    throw "OriginalWaterDust: returned slot has no record"
  return { result with state := result.state.put result.returnedSlot (splashPost dust request.rainVelocity) }

/-! ## Collision callees used by the water block -/

private def clampInt (v lo hi : Int) : Int := max lo (min hi v)

private def liquidAmount (tile : OriginalWorld.Tile) : Nat := (tile.liquid.map (·.amount.toNat)).getD 0

/-- `Collision.SolidCollision(Vector2, int, int)`; tiles are never null in a loaded world. -/
def solidCollision (grid : Grid) (pos : Vector) (width height : Int) : Bool := Id.run do
  let x0 := clampInt (convI4 (pos.x / 16) - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (convI4 ((pos.x + f32 width) / 16) + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt (convI4 (pos.y / 16) - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (convI4 ((pos.y + f32 height) / 16) + 2) 0 ((grid.height : Int) - 40)
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      if tile.actuated then continue
      let some id := tile.id | continue
      let rules := grid.rules id
      if !rules.solid || rules.solidTop then continue
      let left := f32 (x * 16)
      let half := tile.shape == .halfBlock
      let top := f32 (y * 16) + (if half then 8 else 0)
      let tall : Int := if half then 8 else 16
      if pos.x + f32 width > left && pos.x < left + 16 &&
          pos.y + f32 height > top && pos.y < top + f32 tall then return true
  return false

private def flagsOf (tile : OriginalWorld.Tile) : WetFlags :=
  let kind := tile.liquid.map (·.kind)
  { honey := kind == some .honey, shimmer := kind == some .shimmer }

/-- `Collision.WetCollision(Vector2, int, int)`. Returns the hit and the new
`Collision.honey/shimmer` statics, which the method first clears on every call. -/
def wetCollision (grid : Grid) (pos : Vector) (width height : Int) : Bool × WetFlags := Id.run do
  let center : Vector := ⟨pos.x + f32 (width.tdiv 2), pos.y + f32 (height.tdiv 2)⟩
  let boxW := if 10 > width then width else 10
  let boxH := if height.tdiv 2 > height then height else height.tdiv 2
  let box : Vector := ⟨center.x - f32 (boxW.tdiv 2), center.y - f32 (boxH.tdiv 2)⟩
  let x0 := clampInt (convI4 (pos.x / 16) - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (convI4 ((pos.x + f32 width) / 16) + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt (convI4 (pos.y / 16) - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (convI4 ((pos.y + f32 height) / 16) + 2) 0 ((grid.height : Int) - 40)
  let overlaps (left top : Float32) (tall : Int) :=
    box.x + f32 boxW > left && box.x < left + 16 && box.y + f32 boxH > top && box.y < top + f32 tall
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      let amount := liquidAmount tile
      if amount > 0 then
        let depth := f32 (256 - amount : Nat) / 32
        let top := f32 (y * 16) + depth * 2
        let tall := 16 - convI4 (depth * 2)
        if overlaps (f32 (x * 16)) top tall then return (true, flagsOf tile)
      else
        let sloped := match tile.shape with | .slope id => id != 0 | _ => false
        if tile.id.isSome && sloped && y > 0 then
          let above := grid.tileAt x (y - 1)
          if liquidAmount above > 0 && overlaps (f32 (x * 16)) (f32 (y * 16)) 16 then
            return (true, flagsOf above)
  return (false, {})

/-! ## Lifecycle: `Dust.UpdateDust` -/

structure UpdateContext where
  netMode : Int := 0
  /-- The update-time `Main.screenPosition` / `Main.screenHeight` (removal below the screen). -/
  screenPosition : Vector
  screenHeight : Int
  deriving Repr

structure UpdateResult where
  state : State
  /-- `Collision.honey/shimmer` after this cohort's last `WetCollision` call. -/
  wetFlags : WetFlags
  /-- Slot of that last call; another cohort's later-slot call would override it. -/
  lastWetSlot : Option Nat := none

/-- The per-particle body for an active slot below `maxDustToDraw`, entered with the running
`dCount` AFTER this slot's `+1`. `IL_0058‥5ba6` restricted to the water types. -/
def advance (grid : Grid) (c : UpdateContext) (dCount : Float32) (d : Particle) :
    Particle × WetFlags := Id.run do
  let mut d := d
  if d.scale > 10 then d := { d with active := false }
  -- The child-safety branch needs DangerousDust(type); false for every water type.
  d := { d with position := ⟨d.position.x + d.velocity.x, d.position.y + d.velocity.y⟩ }
  if !d.noGravity then d := { d with velocity := ⟨d.velocity.x, d.velocity.y + 0.1⟩ }
  -- Water block IL_444d‥45b5.
  if d.velocity.x == 0 then
    if solidCollision grid d.position 2 2 then d := { d with scale := 0 }
    d := { d with rotation := d.rotation + 0.5, scale := d.scale - 0.01 }
  let (wet, flags) := wetCollision grid d.position 4 4
  if wet then d := { d with alpha := i32 (d.alpha + 20), scale := d.scale - 0.1 }
  d := { d with alpha := i32 (d.alpha + 2), scale := d.scale - 0.005 }
  if d.alpha > 255 then d := { d with scale := 0 }
  if d.velocity.y > 4 then d := { d with velocity := ⟨d.velocity.x, 4⟩ }
  if d.noGravity then
    d := { d with rotation := if d.velocity.x < 0 then d.rotation - 0.2 else d.rotation + 0.2 }
    d := { d with scale := d.scale + 0.03 }
    d := { d with velocity := ⟨d.velocity.x * 1.05, d.velocity.y + 0.15⟩ }
  -- Common tail.
  d := { d with velocity := ⟨d.velocity.x * 0.99, d.velocity.y⟩ }
  d := { d with rotation := d.rotation + d.velocity.x * 0.5 }
  if d.fadeIn > 0 && d.fadeIn < 100 then
    d := { d with scale := d.scale + 0.03 }
    if d.scale > d.fadeIn then d := { d with fadeIn := 0 }
  else
    d := { d with scale := d.scale - 0.01 }
  if d.noGravity then
    d := { d with velocity := ⟨d.velocity.x * 0.92, d.velocity.y * 0.92⟩ }
    if d.fadeIn == 0 then d := { d with scale := d.scale - 0.04 }
  if d.position.y > c.screenPosition.y + f32 c.screenHeight then d := { d with active := false }
  let count := dCount.toFloat
  if count == 0.5 then d := { d with scale := d.scale - 0.001 }
  if count == 0.6 then d := { d with scale := d.scale - 0.0025 }
  if count == 0.7 then d := { d with scale := d.scale - 0.005 }
  if count == 0.8 then d := { d with scale := d.scale - 0.01 }
  if count == 0.9 then d := { d with scale := d.scale - 0.02 }
  let mut least : Float32 := 0.1
  if count == 0.5 then least := 0.11
  if count == 0.6 then least := 0.13
  if count == 0.7 then least := 0.16
  if count == 0.8 then least := 0.22
  if count == 0.9 then least := 0.25
  if d.scale < least then d := { d with active := false }
  return (d, flags)

/-- `UpdateDust`'s closing `dCount` write: from a local counter that stays 0 (`IL_5bbe`). -/
def closingDensity (drawBudget : Int) : Float32 :=
  let budget := drawBudget.toFloat
  if 0 > budget * 0.9 then 0.9 else if 0 > budget * 0.8 then 0.8
  else if 0 > budget * 0.7 then 0.7 else if 0 > budget * 0.6 then 0.6
  else if 0 > budget * 0.5 then 0.5 else 0

/-- One complete `Dust.UpdateDust` pass as it affects this cohort. Pass the shared pool as it
was BEFORE any cohort's part of this pass: its occupancy of other cohorts' slots is the
"active when visited" set the running `dCount` counts (cohorts that spawn dust during the
pass must supply that visited set instead). Every slot at/above `maxDustToDraw` is
deactivated without other change. `netMode == 2` returns before touching anything. The
result's pool has this cohort's occupancy written back and `dCount` set to the closing
value. Other cohorts' slots are left for their own updaters. No random draws. -/
def update (state : State) (grid : Grid) (c : UpdateContext) (wetFlags : WetFlags) :
    Except String UpdateResult := do
  checkPool state.pool
  if c.netMode == 2 then return { state, wetFlags }
  let budget := state.pool.drawBudget
  let mut pool := state.pool
  let mut running := pool.density
  let mut water := #[]
  let mut flags := wetFlags
  let mut lastWet := none
  let owned := state.water
  let mut next := 0
  for i in [0:physicalSlots] do
    let mine := if h : next < owned.size then (if owned[next].slot == i then some owned[next] else none) else none
    if mine.isSome then next := next + 1
    match mine with
    | some o =>
      if o.dust.active then
        let mut d := o.dust
        if i ≥ budget then d := { d with active := false }
        else
          running := running + 1
          let (d', f) := advance grid c running d
          d := d'
          flags := f
          lastWet := some i
        water := water.push ⟨i, d⟩
        pool := setOccupied pool i d.active
      else
        -- An inactive record never touches shared state: the slot may now be another
        -- cohort's, which its own updater handles; it still counts if that one is active.
        water := water.push o
        if i < budget && occupiedAt state.pool i then running := running + 1
    | none =>
      if i < budget && occupiedAt state.pool i then running := running + 1
  pool := { pool with density := closingDensity budget }
  return { state := { state with pool, water }, wetFlags := flags, lastWetSlot := lastWet }

/-! ## Colours and drawing -/

private def channel (packed : UInt32) (shift : UInt32) : Int := ((packed >>> shift) &&& 255).toNat
private def clampByte (v : Int) : UInt32 := (max 0 (min 255 v)).toNat.toUInt32
private def pack (r g b a : Int) : UInt32 :=
  clampByte r ||| (clampByte g <<< 8) ||| (clampByte b <<< 16) ||| (clampByte a <<< 24)

/-- `Dust.GetAlpha(newColor)` for these types; packed colours. -/
def getAlpha (d : Particle) (light : UInt32) : UInt32 :=
  if d.fullBright then 0xFFFFFFFF else
    let opacity := f32 (i32 (255 - d.alpha)) / 255
    let scaleChannel := fun shift => convI4 (f32 (channel light shift) * opacity)
    pack (scaleChannel 0) (scaleChannel 8) (scaleChannel 16) (i32 (channel light 24 - d.alpha))

/-- `Dust.GetColor(newColor)`: the own-colour overlay. -/
def getColor (d : Particle) (newColor : UInt32) : UInt32 :=
  let mix := fun shift => channel d.color shift - (255 - channel newColor shift)
  pack (mix 0) (mix 8) (mix 16) (mix 24)

structure DrawContext where
  /-- The draw-time `Main.screenPosition`, `screenWidth`, `screenHeight`. -/
  screenPosition : Vector
  screenWidth : Int
  screenHeight : Int
  deriving Repr

structure DrawResult where
  state : State
  /-- Sprites in emission order, tagged with their slot so mixed cohorts can be merged in
  ascending slot order (a slot's base sprite precedes its overlay). -/
  sprites : Array (Nat × OriginalForestChatLayout.Sprite)

private def spriteColor (packed : UInt32) : OriginalForestChatLayout.Color :=
  ⟨(channel packed 0).toNat, (channel packed 8).toNat, (channel packed 16).toNat, (channel packed 24).toNat⟩

/-- One slot of `Main.DrawDust` for an active particle (`i < maxDustToDraw`). Off the camera
rectangle grown by 1000 / 1050 it is deactivated without a sprite; otherwise the lit sprite,
then the overlay when `color` and its result are non-zero; finally removal when the lit
colour is opaque black. `lightAt x y` is `Lighting.GetColor(x, y)`, packed. -/
def drawOne (c : DrawContext) (lightAt : Int → Int → UInt32) (d : Particle) :
    Except String (Particle × Array OriginalForestChatLayout.Sprite) := do
  let x0 := i32 (convI4 c.screenPosition.x - 1000)
  let y0 := i32 (convI4 c.screenPosition.y - 1050)
  let w := i32 (c.screenWidth + 2000)
  let h := i32 (c.screenHeight + 2100)
  let px := convI4 d.position.x
  let py := convI4 d.position.y
  unless px < i32 (x0 + w) && x0 < i32 (px + 4) && py < i32 (y0 + h) && y0 < i32 (py + 4) do
    return ({ d with active := false }, #[])
  unless d.shaderNull do throw "OriginalWaterDust: ArmorShaderData dust is outside this module"
  let tx := (d.position.x.toFloat + 4).toInt32.toInt.tdiv 16
  let ty := (d.position.y.toFloat + 4).toInt32.toInt.tdiv 16
  let lit := getAlpha d (lightAt tx ty)
  let pose := fun (packed : UInt32) => OriginalForestChatLayout.draw "Images/Dust" 1000 120
    (d.position.x - c.screenPosition.x) (d.position.y - c.screenPosition.y)
    (some ⟨d.frame.x, d.frame.y, d.frame.w, d.frame.h⟩) (spriteColor packed) d.rotation 4 4 d.scale d.scale
  let mut sprites := #[pose lit]
  if d.color != 0 then
    let overlay := getColor d lit
    if overlay != 0 then sprites := sprites.push (pose overlay)
  let d := if lit == 0xFF000000 then { d with active := false } else d
  return (d, sprites)

/-- One complete `Main.DrawDust` pass as it affects this cohort: slots below
`maxDustToDraw` (at most 6000) in ascending order. Slots at/above the budget are untouched
(UpdateDust removes them). Occupancy is written back for deactivated slots; `dCount` is not
touched. No random draws, no clock. -/
def draw (state : State) (c : DrawContext) (lightAt : Int → Int → UInt32) : Except String DrawResult := do
  checkPool state.pool
  if state.pool.drawBudget > physicalSlots then
    throw "OriginalWaterDust: DrawDust would index past the 6001-entry table"
  let mut pool := state.pool
  let mut water := #[]
  let mut sprites := #[]
  for o in state.water do
    if o.slot < state.pool.drawBudget && o.dust.active then
      let (d, out) ← drawOne c lightAt o.dust
      water := water.push ⟨o.slot, d⟩
      sprites := sprites ++ out.map (o.slot, ·)
      pool := setOccupied pool o.slot d.active
    else water := water.push o
  return { state := { state with pool, water }, sprites }

end Lean4Terria.OriginalWaterDust
