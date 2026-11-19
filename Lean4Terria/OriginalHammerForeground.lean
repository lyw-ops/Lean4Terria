import Lean4Terria.OriginalWallWork
import Lean4Terria.OriginalCandlePlacement
import Lean4Terria.OriginalPlatformFraming
import Lean4Terria.OriginalTileHitRuntime
import Lean4Terria.OriginalHammerForeground.Tables

/-!
# Foreground hammering: slopes, half blocks and platforms (Terraria 1.4.5.8 Classic, single player)

Hand translation of the hammer part of `Player.ItemCheck_UseMiningTools` for a hammer held by the local
player, including the hand-off to the background wall that `OriginalWallWork` already covers.

One item phase (the order inside `ItemCheck_UseMiningTools`, after `ItemCheck` has advanced the swing,
so `toolTime` equals the decremented `itemTime`):

1. **Disarm.** While the swing is live (`toolTime = 0`, `itemAnimation > 0`, use held) and the target
   cell is air or not `IsAHammerTarget` (`tileHammer`, or `IsTilePoundable`: solid, 314, 424, 442,
   351, 470, living fire), `poundRelease := false`.
2. **Reach.** Outside `IsTargetTileInItemRange` nothing else happens (in particular `poundRelease`
   is *not* re-armed).
3. **Tool.** On a live swing `ItemCheck_UseMiningTools_ActuallyUseMiningTool` on the target: an active
   tile gets a prepared `HitTile` entry of type 1 (`HitObject`), then
   `ItemCheck_UseMiningTools_TryPoundingTile`:
   * only with `poundRelease`; a tile that is not poundable disarms and leaves the wall path open;
   * otherwise the wall path is closed, `ApplyItemTime` (`useTime`), `AddDamage(100)` (one
     `Main.rand` sample), `ClearMiningCacheAt`, then the shape step and `poundRelease := false`:
     - **platforms**: half → `PoundTile` (flat); flat → slope *a*; slope *a* → slope *b*; any other
       slope → `slope(0)` + `PoundTile` (half). *a, b* = 1, 2, or 2, 1 when a platform is up-right
       (x+1, y−1) or down-left (x−1, y+1), or the right cell is solid and the left one is not;
     - **blocks**: full (and every `tileSolidTop` tile) → `PoundTile` (half); half or sloped →
       `SlopeTile` cycle. *a, b* = 1, 2, or 2, 1 when the right cell is solid (or a slope 1 / 3)
       and the left one is not. Cycle `0 → a → b → a+2 → b+2 → 0`, or when the cell above is solid
       and the one below is not, `0 → a+2 → b+2 → a → b → 0`. A half block counts as 0.
   * `PoundTile` / `SlopeTile` refuse when `CanPoundTile` fails (doors 10, spikes 48 / 232, traps 137,
     planter 380, trapdoors 387 / 388, 476, 484, boulders, a `ForbidsSloping` tile above — chests,
     altars, hellforge, dressers … — or `CanKillTile`). They run `KillTile(fail, effectOnly)` (no
     sample in a dedicated host), `SquareTileFrame(x, y, true)` (the centre re-rolls its frame number
     with one `genRand` = `Main.rand` sample when it is not frame-important), and when the result is a
     full / flat cell every player whose box meets the cell is moved to stand on it
     (`position.Y := y·16 − height`, `gfxOffY += old bottom − y·16`).
4. **Re-arm.** `releaseUseItem` (use key up this frame) sets `poundRelease := true`.
5. **Wall.** If the tool step left the wall path open: `TryFindingWallToHammer` → `TryHittingWall`, which
   skips the target cell when it holds an active tile and `poundRelease` is true (so a tap never hits
   the wall behind a block, a hold hits it from the second swing on) → `PickWall` (`HitObject` type 2,
   `AddDamage(hammer · 1.5)`, `KillWall`), `itemTime := useTime / 2`.

`HitTile` is translated slot for slot (501 entries, `order`, `bufferLocation`), shared by tile (type 1)
and wall (type 2) entries as in vanilla.

Not covered (reported through `Result.unsupported`): `tileHammer` targets (altars, orbs, 695 / 696),
display dolls 470, tracks 314, traps 137, 442; locked doors above / below (damage 0 path); falling
blocks whose re-frame spawns a projectile; the frames of frame-important tiles (platform `frameX`) and
the dust / sound samples of a desktop client.
-/

namespace Lean4Terria.OriginalHammerForeground

open OriginalWallWork (Work Progress)
open OriginalItemUse (Timing)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Terrain Tile TileShape)
open OriginalCollision (single)

/-! ## HitTile -/

/-- `HitTile.HitTileObject` without the cosmetic fields (`crackStyle` comes from a private time-seeded
generator, `animationDirection` from the `Main.rand` sample `AddDamage` draws). -/
structure HitEntry where
  x : Int := 0
  y : Int := 0
  damage : Int := 0
  type : Nat := 0
  timeToLive : Int := 0
  deriving BEq, Repr, Inhabited

/-- `Terraria.HitTile` of one player: 501 slots, the `order` permutation and the buffer slot. -/
structure HitTile where
  data : Array HitEntry := Array.replicate 501 {}
  order : Array Nat := Array.range 501
  bufferLocation : Nat := 0
  deriving BEq, Repr

/-- Frame numbers (`Tile.frameNumber`, the 0–2 sprite variant) of the cells this module re-rolled. -/
abbrev TileFrameNumbers := Std.HashMap (Nat × Nat) Nat

/-- What a frame's tile edit did to the local player. -/
structure Push where
  /-- new `position.Y` -/
  y : Float32
  /-- amount added to `Player.gfxOffY` -/
  gfxOffY : Float32
  deriving BEq, Repr

private structure S where
  w : Work
  effectsState : Option OriginalTileHitRuntime.State := none
  frames : TileFrameNumbers
  hits : HitTile
  poundRelease : Bool
  body : Body
  push : Option Push := none
  shaped : Option Cell := none
  cleared : Option Cell := none
  effects : Nat := 0
  unsupported : Option UInt16 := none
  /-- `SetItemTime` from this phase (`ApplyItemTime` of the pound, or `useTime / 2` after a wall hit) -/
  itemTime : Option Nat := none

private abbrev M := StateM S

private def effectsBlocked : M Bool := return OriginalTileHitRuntime.blocked (← get).effectsState

private def tilePrefix (t : Tile) (x y : Int) (fail effectOnly : Bool) : M Unit := do
  let s ← get
  let (effectsState,random) := OriginalTileHitRuntime.emit s.effectsState t x y fail s.w.random effectOnly
  set {s with effectsState,w := {s.w with random}}

private def digSound (x y : Int) : M Unit := do
  let s ← get
  let (effectsState,random) := OriginalTileHitRuntime.dig s.effectsState x y s.w.random
  set {s with effectsState,w := {s.w with random}}


private def tileAt (terrain : Terrain) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (terrain.tile? x.toNat y.toNat).getD default

private def tile (x y : Int) : M Tile := return tileAt (← get).w.terrain x y

private def setTile (x y : Int) (t : Tile) : M Unit := do
  if x < 0 || y < 0 then return
  modify fun s => match s.w.terrain.replaceTile x.toNat y.toNat t with
    | some terrain => { s with w := { s.w with terrain } }
    | none => s

private def sample : M Unit :=
  modify fun s => let (_, random) := s.w.random.nextInt
    { s with w := { s.w with random, draws := s.w.draws + 1 } }

private def nextBelow3 : M Nat := do
  let s ← get
  let (v, random) := s.w.random.next 0 3
  set { s with w := { s.w with random, draws := s.w.draws + 1 } }
  return v.toNat

private def entry (i : Nat) : M HitEntry := return (← get).hits.data[i]!
private def orderAt (i : Nat) : M Nat := return (← get).hits.order[i]!
private def setEntry (i : Nat) (e : HitEntry) : M Unit :=
  modify fun s => { s with hits := { s.hits with data := s.hits.data.set! i e } }
private def setOrder (i v : Nat) : M Unit :=
  modify fun s => { s with hits := { s.hits with order := s.hits.order.set! i v } }

/-- `HitTile.TryFinding(x, y, type)` (`none` = −1); `HitObject` shares the scan. -/
private def scan (x y : Int) (type : Nat) : M (Option Nat) := do
  for i in [0:501] do
    let slot ← orderAt i
    let e ← entry slot
    if e.type == type then
      if e.x == x && e.y == y then return some slot
    else if i != 0 && e.type == 0 then return none
  return none

/-- `HitTile.HitObject(x, y, type)`: the slot holding the cell, else the buffer slot prepared for it. -/
private def hitObject (x y : Int) (type : Nat) : M Nat := do
  if let some slot ← scan x y type then return slot
  let b := (← get).hits.bufferLocation
  let e ← entry b
  setEntry b { e with x, y, type }
  return b

/-- `HitTile.Clear(slot)`. -/
private def clearSlot (slot : Nat) : M Unit := do
  if slot > 500 then return
  setEntry slot {}
  let mut i := 0
  while i < 500 do
    if (← orderAt i) == slot then break
    i := i + 1
  while i < 500 do
    setOrder i (← orderAt (i + 1))
    i := i + 1
  setOrder 500 slot

/-- `HitTile.SortSlots(slot)`. -/
private def sortSlots (slot : Nat) : M Unit := do
  let s ← get
  if slot == s.hits.bufferLocation then
    let b := s.hits.order[500]!
    modify fun s => { s with hits := { s.hits with bufferLocation := b } }
    if slot != b then setEntry b {}
    let mut i := 500
    while i > 0 do
      setOrder i (← orderAt (i - 1))
      i := i - 1
    setOrder 0 b
  else
    let mut i := 0
    while i ≤ 500 do
      if (← orderAt i) == slot then break
      i := i + 1
    while i > 1 do
      let tmp ← orderAt (i - 1)
      setOrder (i - 1) (← orderAt i)
      setOrder i tmp
      i := i - 1
    setOrder 1 slot

/-- `HitTile.AddDamage(slot, damage, updateAmount: true)`; one `Main.rand` sample when it records. -/
private def addDamage (slot : Nat) (damage : Int) : M Int := do
  if slot > 500 then return 0
  if slot == (← get).hits.bufferLocation && damage == 0 then return 0
  let e ← entry slot
  setEntry slot { e with damage := e.damage + damage, timeToLive := 60 }
  sample
  sortSlots slot
  return (← entry slot).damage

private def pruneSlot (terrain : Terrain) (entry : HitEntry) : HitEntry :=
  if entry.type == 0 then entry else
  if entry.timeToLive ≤ 1 then {} else
  let t := tileAt terrain entry.x entry.y
  let ttl := entry.timeToLive - 1
  let decay : Int := if ttl < 12 then 10 else if ttl < 24 then 7 else if ttl < 36 then 5 else if ttl < 48 then 2 else 0
  let damage := entry.damage - decay
  if damage < 0 || (if entry.type == 1 then t.id.isNone else t.wall == 0) then {}
  else { entry with timeToLive := ttl, damage }

/-- `HitTile.Prune()`: age every slot, then stably move empty slots behind used ones.
Keep the reserved buffer at the front, including when prune clears its pending entry. -/
def HitTile.prune (hits : HitTile) (terrain : Terrain) : HitTile :=
  let data := hits.data.map (pruneSlot terrain)
  let cleared := (hits.data.zip data).any fun (old, new) => old.type != 0 && new.type == 0
  let order := if !cleared then hits.order else
    let rest := hits.order.extract 1 hits.order.size
    let active := rest.filter fun i => data[i]!.type != 0
    let empty := rest.filter fun i => data[i]!.type == 0
    hits.order.extract 0 1 ++ active ++ empty
  { hits with data, order }

def HitTile.walls (hits : HitTile) : OriginalWallWork.WallHits :=
  { entries := hits.order.filterMap fun i => do
      let entry ← hits.data[i]?
      guard (entry.type == 2 && entry.timeToLive > 0)
      return ⟨⟨entry.x.toNat, entry.y.toNat⟩, entry.damage.toNat, entry.timeToLive.toNat⟩ }

private def prune : M Unit := modify fun s => { s with hits := s.hits.prune s.w.terrain }

/-- `Player.ClearMiningCacheAt(x, y, type)`: `TryClearingAndPruning(x, y, 1)` on `hitTile` (the block-swap
cache `hitReplace` holds nothing on these paths and is not modelled). Vanilla passes the constant 1, not
`type`: `PickWall`'s call after a wall breaks clears the tile entry `HitObject` prepared on that cell. -/
private def clearMiningCacheAt (x y : Int) (_type : Nat) : M Unit := do
  modify fun s => { s with cleared := some ⟨x.toNat, y.toNat⟩ }
  if let some slot ← scan x y 1 then
    clearSlot slot
    prune

/-! ## Tile rules -/

private def active (t : Tile) : Bool := t.id.isSome
private def typeOf (t : Tile) : UInt16 := t.id.getD 0
private def isPlatform (t : Tile) : Bool := t.id.any Tables.platform
private def slopeOf (t : Tile) : Nat := match t.shape with | .slope k => k.toNat | _ => 0
private def isHalf (t : Tile) : Bool := t.shape == .halfBlock

/-- `WorldGen.SolidTile(x, y, noDoors: false)`. -/
def solidTile (terrain : Terrain) (x y : Int) : Bool :=
  let t := tileAt terrain x y
  t.id.any fun id => Tables.solid id && !Tables.solidTop id && t.shape == .full && !t.actuated

/-- `Player.IsTilePoundable`. -/
def isTilePoundable (t : Tile) : Bool :=
  match t.id with
  | none => false
  | some id => !Tables.hammerTarget id &&
      (Tables.solid id || id == 314 || id == 424 || id == 442 || id == 351 || id == 470 || Tables.livingFire id)

/-- `Player.IsAHammerTarget`. -/
def isAHammerTarget (t : Tile) : Bool := t.id.any Tables.hammerTarget || isTilePoundable t

/-- `WorldGen.ForbidsSloping` (the tile above refuses a slope below it). -/
def forbidsSloping (id : UInt16) : Bool :=
  [21, 26, 77, 88, 235, 237, 441, 467, 468, 470, 475, 488, 597].contains id.toNat

private def frameX (t : Tile) : Int := (t.frame.map (·.1)).getD 0
private def frameY (t : Tile) : Int := (t.frame.map (·.2)).getD 0

/-- `WorldGen.CanKillTile(x, y)` as far as the pound path reaches it. Teleporters (235) with a tile
above are refused (vanilla asks `CheckTileBreakability_HasReasonToReturnEarly`, not modelled). -/
def canKillTile (terrain : Terrain) (x y : Int) : Bool := Id.run do
  if x < 0 || y < 0 || x ≥ terrain.header.width || y ≥ terrain.header.height then return false
  let t := tileAt terrain x y
  let some id := t.id | return false
  if t.wall == 350 then return false
  if y ≥ 1 then
    let a := tileAt terrain x (y - 1)
    if let some above := a.id then
      if Tables.treeTrunk above && id != above then
        let fx := frameX a
        let fy := frameY a
        unless (fx == 66 && fy ≥ 0 && fy ≤ 44) || (fx == 88 && fy ≥ 66 && fy ≤ 110) || fy ≥ 198 do
          return false
      if [21, 26, 72, 77, 88, 467, 488].contains above.toNat && id != above then return false
      if above == 80 && id != above then
        let col := (frameX a).tdiv 18
        if col == 0 || col == 1 || col == 4 || col == 5 then return false
      if above == 323 && id != above && (frameX a == 66 || frameX a == 220) then return false
  if id == 10 && t.frame.any fun (fx, fy) => fy ≥ 594 && fy ≤ 646 && fx < 54 then return false
  if id == 235 then
    let x0 := x - (frameX t % 54) / 18
    for k in [0:3] do
      if active (tileAt terrain (x0 + k) (y - 1)) then return false
  return true

/-- `WorldGen.CanPoundTile(x, y)`. -/
def canPoundTile (terrain : Terrain) (x y : Int) : Bool := Id.run do
  let t := tileAt terrain x y
  let id := typeOf t
  if [10, 48, 137, 232, 380, 387, 388, 476, 484].contains id.toNat then return false
  if Tables.boulder id then return false
  let above := tileAt terrain x (y - 1)
  if above.id.any forbidsSloping then return false
  return canKillTile terrain x y

/-- `WorldGen.IsLockedDoor(x, y)`. -/
private def lockedDoor (terrain : Terrain) (x y : Int) : Bool :=
  let t := tileAt terrain x y
  t.id == some 10 && t.frame.any fun (fx, fy) => fy ≥ 594 && fy ≤ 646 && fx < 54

/-! ## Framing and the player push -/

mutual

/-- `WorldGen.TileFrame(x, y, resetFrame)` as far as this path needs it: an inactive cell loses its
half brick and slope; with `resetFrame` an active, not frame-important tile reaching
`TileFrameCosmetic` re-rolls `frameNumber` with `genRand.Next(0, 3)`. A candle (49, 372, 646) gets
`CheckOnTable1x1`: when the reshaped platform below no longer holds it, `KillTile` drops the item
(two velocity samples), clears the cell and frames its square. -/
private partial def tileFrame (x y : Int) (resetFrame : Bool) : M Unit := do
  if ← effectsBlocked then return
  let s ← get
  if !(x > 5 && y > 5 && x < s.w.terrain.header.width - 5 && y < s.w.terrain.header.height - 5) then return
  let t ← tile x y
  match t.id with
  | none => if t.shape != .full then setTile x y { t with shape := .full }
  | some id =>
    if OriginalCandlePlacement.isCandleTile id then
      let grid := OriginalCollision.Grid.ofTerrain s.w.terrain OriginalTileRules.collision
      let cell : Cell := ⟨x.toNat, y.toNat⟩
      if OriginalCandlePlacement.staysOnSupport grid cell then return
      tilePrefix t x y false false
      if ← effectsBlocked then return
      let s ← get
      if let some item := OriginalCandlePlacement.dropForTile id then
        let (items, random) := OriginalItems.spawnStack s.w.items cell (.ofItem item) s.w.random (some grid)
        set { s with w := { s.w with items, random, draws := s.w.draws + 2 } }
      setTile x y (OriginalMining.clearGround t)
      squareTileFrame x y
    else if Tables.platform id then
      let terrain := OriginalPlatformFraming.framePlatform s.w.terrain x y
      set { s with w := { s.w with terrain } }
    else if resetFrame && !Tables.frameImportant id && id != 49 && id != 80 then
      let n ← nextBelow3
      modify fun s => { s with frames := s.frames.insert (x.toNat, y.toNat) n }

/-- `WorldGen.SquareTileFrame(x, y, true)`: column x−1, x, x+1, each y−1 … y+1; the centre resets. -/
private partial def squareTileFrame (x y : Int) : M Unit := do
  for dx in [(-1 : Int), 0, 1] do
    for dy in [(-1 : Int), 0, 1] do
      tileFrame (x + dx) (y + dy) (dx == 0 && dy == 0)

end

/-- `Rectangle.Intersects` of the cell with the player's integer box. -/
private def touches (body : Body) (x y : Int) : Bool :=
  let px := body.position.x.toFloat.toInt64.toInt
  let py := body.position.y.toFloat.toInt64.toInt
  x * 16 < px + body.width && px < x * 16 + 16 && y * 16 < py + body.height && py < y * 16 + 16

/-- The loop over `Main.player` at the end of `PoundTile` / `SlopeTile` for the local player. -/
private def pushPlayer (x y : Int) : M Unit := do
  let s ← get
  if !touches s.body x y then return
  let h := s.body.height
  let gfx := (s.body.position.y + single h) - single (y * 16)
  let newY := single (y * 16 - h)
  let gfx := ((s.push.map (·.gfxOffY)).getD 0) + gfx
  set { s with body := { s.body with position := { s.body.position with y := newY } }, push := some ⟨newY, gfx⟩ }

/-- `WorldGen.PoundTile(x, y)`. -/
private def poundTile (x y : Int) : M Bool := do
  if !canPoundTile (← get).w.terrain x y then return false
  let t ← tile x y
  let t := { t with shape := if isHalf t then .full else .halfBlock }
  setTile x y t
  modify fun s => { s with shaped := some ⟨x.toNat, y.toNat⟩ }
  if (← get).effectsState.any (·.context.generating) then return true
  modify fun s => {s with effects := s.effects + 1}
  tilePrefix (← tile x y) x y true true
  if ← effectsBlocked then return true
  digSound x y
  squareTileFrame x y
  if !isHalf (← tile x y) then pushPlayer x y
  return true

/-- `WorldGen.SlopeTile(x, y, slope, noEffects: false)`. -/
private def slopeTile (x y : Int) (slope : Nat) : M Bool := do
  if !canPoundTile (← get).w.terrain x y then return false
  let t ← tile x y
  setTile x y { t with shape := if slope == 0 then .full else .slope slope.toUInt8 }
  modify fun s => { s with shaped := some ⟨x.toNat, y.toNat⟩ }
  if (← get).effectsState.any (·.context.generating) then return true
  modify fun s => {s with effects := s.effects + 1}
  tilePrefix (← tile x y) x y true true
  if ← effectsBlocked then return true
  digSound x y
  squareTileFrame x y
  if slopeOf (← tile x y) == 0 then pushPlayer x y
  return true

/-! ## The pound step -/

/-- The shape step of `TryPoundingTile` after the damage reached 100. -/
private def shapeStep (x y : Int) : M Unit := do
  let terrain := (← get).w.terrain
  let t ← tile x y
  let id := typeOf t
  if id == 470 || id == 314 || id == 137 || id == 442 then
    modify fun s => { s with unsupported := some id }
    return
  if Tables.platform id then
    if isHalf t then
      let _ ← poundTile x y
      return
    let diag := isPlatform (tileAt terrain (x + 1) (y - 1)) || isPlatform (tileAt terrain (x - 1) (y + 1))
    let (a, b) := if diag || (solidTile terrain (x + 1) y && !solidTile terrain (x - 1) y) then (2, 1) else (1, 2)
    let s := slopeOf t
    if s == 0 then let _ ← slopeTile x y a
    else if s == a then let _ ← slopeTile x y b
    else if canPoundTile terrain x y then
      setTile x y { t with shape := .full }
      let _ ← poundTile x y
    return
  if id == 424 then
    let fx := frameX t
    setTile x y { t with frame := some (if fx == 0 then 18 else if fx == 18 then 36 else 0, frameY t) }
    return
  if (isHalf t || slopeOf t != 0) && !Tables.solidTop id then
    let right := tileAt terrain (x + 1) y
    let (a, b) :=
      if (solidTile terrain (x + 1) y || slopeOf right == 1 || slopeOf right == 3) && !solidTile terrain (x - 1) y
      then (2, 1) else (1, 2)
    let down := solidTile terrain x (y - 1) && !solidTile terrain x (y + 1)
    let s := slopeOf t
    let next :=
      if !down then
        if s == 0 then a else if s == a then b else if s == b then a + 2 else if s == a + 2 then b + 2 else 0
      else
        if s == 0 then a + 2 else if s == a + 2 then b + 2 else if s == b + 2 then a else if s == a then b else 0
    let _ ← slopeTile x y next
  else
    let _ ← poundTile x y

/-- `ItemCheck_UseMiningTools_TryPoundingTile`; returns false when it closed the wall path. -/
private def tryPounding (useTime : Nat) (hitSlot : Nat) (x y : Int) : M Bool := do
  if !(← get).poundRelease then return true
  let t ← tile x y
  if !active t || !isTilePoundable t then
    modify fun s => { s with poundRelease := false }
    return true
  modify fun s => { s with itemTime := some useTime }   -- ApplyItemTime, before the damage
  let terrain := (← get).w.terrain
  let damage : Int := if lockedDoor terrain x (y - 1) || lockedDoor terrain x (y + 1) then 0 else 100
  let total ← addDamage hitSlot damage
  if total < 100 then
    modify fun s => { s with effects := s.effects + 1, unsupported := some 10 }
    return false
  clearMiningCacheAt x y 1
  shapeStep x y
  modify fun s => { s with poundRelease := false }
  return false

/-- `ItemCheck_UseMiningTools_TryHittingWall` → `PickWall`; the cell hit, if any. -/
private def tryHittingWall (stored : Nat → Nat → Option Nat) (progress : Progress) (target cell : Cell)
    (power useTime : Nat) : M (Option Cell) := do
  let t ← tile cell.x cell.y
  if t.wall == 0 then return none
  if active t && cell == target && (t.id.any Tables.hammerTarget || (← get).poundRelease) then return none
  if !OriginalWallWork.canSmashWall (← get).w.terrain cell.x cell.y then return none
  let damage : Int := (Float32.ofNat power * 1.5).toFloat.toInt64.toInt
  let slot ← hitObject cell.x cell.y 2
  let total ← addDamage slot damage
  let s ← get
  if total ≥ 100 then
    clearSlot slot
    clearMiningCacheAt cell.x cell.y 2
    set { (← get) with w := OriginalWallWork.killWall (← get).w stored progress cell false }
  else
    set { s with w := OriginalWallWork.killWall s.w stored progress cell true }
  if damage != 0 then prune
  modify fun s => { s with itemTime := some (useTime / 2) }
  return some cell

/-! ## One item phase -/

/-- Player-side input of one frame. -/
structure Input where
  /-- `controlUseItem` (left button held) -/
  held : Bool
  /-- the use key went down this frame (only matters for hammers without `autoReuse`) -/
  pressed : Bool := true
  /-- `Player.tileTargetX/Y` -/
  target : Option Cell
  /-- world pixel of the mouse (`Main.mouseX + screenPosition.X`, …), for the wall search -/
  mouse : Float32 × Float32

structure Result where
  work : Work
  effectsState : Option OriginalTileHitRuntime.State := none
  frames : TileFrameNumbers
  hits : HitTile
  timing : Timing
  poundRelease : Bool
  /-- the local player was moved to stand on the edited cell -/
  push : Option Push := none
  /-- the cell whose shape changed -/
  shaped : Option Cell := none
  /-- The cell passed to `Player.ClearMiningCacheAt` for the shared tool caches. -/
  cleared : Option Cell := none
  /-- the wall cell hit (`PickWall`) -/
  wallHit : Option Cell := none
  /-- `KillTile(fail, effectOnly)` calls (dust and sound on a desktop client) -/
  effects : Nat := 0
  /-- a target this translation does not cover (its type); the phase stopped there -/
  unsupported : Option UInt16 := none

/-- One item phase of a held hammer: `itemCheck` timing, the pound step on the target tile and the
wall step behind it. `stored` gives the wall-frame variant of cells without an entry in
`w.frames` (see `OriginalWallWork`); `body` is the player after this frame's movement. -/
def hammer (w : Work) (stored : Nat → Nat → Option Nat) (frames : TileFrameNumbers) (hits : HitTile)
    (poundRelease : Bool) (body : Body) (timing : Timing) (item : UInt16) (input : Input)
    (progress : Progress := {}) (effectsState : Option OriginalTileHitRuntime.State := none) : Result := Id.run do
  let some tool := OriginalWallWork.Tables.hammerStats item
    | return { work := w, effectsState, frames, hits, timing, poundRelease }
  let timing := timing.advance (input.held && (tool.autoReuse || input.pressed)) tool.useAnimation
  let live := input.held && timing.itemAnimation > 0 && timing.itemTime == 0
  let run : M (Option Cell) := do
    let some target := input.target | return none
    let t ← tile target.x target.y
    if live && (!active t || !isAHammerTarget t) then modify fun s => { s with poundRelease := false }
    if !inToolRange body target 5 3 tool.tileBoost then return none
    let mut wallOpen := false
    if live then
      wallOpen := true
      if active t then
        if t.id.any Tables.hammerTarget then
          modify fun s => { s with unsupported := t.id }
          return none
        let slot ← hitObject target.x target.y 1
        wallOpen ← tryPounding tool.useTime slot target.x target.y
    if !input.held then modify fun s => { s with poundRelease := true }
    if live && wallOpen then
      let cell := OriginalWallWork.findWallToHammer (← get).w.terrain target input.mouse
      return ← tryHittingWall stored progress target cell tool.power tool.useTime
    return none
  let (wallHit, s) := run.run { w, effectsState, frames, hits, poundRelease, body }
  let timing := match s.itemTime with
    | some n => timing.setTime n
    | none => timing
  return { work := s.w, effectsState := s.effectsState, frames := s.frames, hits := s.hits, timing, poundRelease := s.poundRelease, push := s.push,
           shaped := s.shaped, cleared := s.cleared, wallHit, effects := s.effects, unsupported := s.unsupported }

end Lean4Terria.OriginalHammerForeground
