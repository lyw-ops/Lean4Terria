import Lean4Terria.OriginalHammerForeground
import Lean4Terria.OriginalMiningTools.Tables

/-!
# Pickaxe use (desktop Terraria 1.4.5.8 Classic, single player)

Hand translation of the pickaxe branch of

* `Player.ItemCheck_UseMiningTools` (range, cursor icon, `poundRelease` gates; the wall path is a no-op
  for an item with `hammer = 0`),
* `Player.ItemCheck_UseMiningTools_ActuallyUseMiningTool` (first `HitTile.HitObject`, `PickTile`,
  the direct store `itemTime = (int)(useTime * pickSpeed)` — `itemTimeMax` is not written —
  and `TryPoundingTile`, which only clears `poundRelease` for a pickaxe),
* `Player.PickTile`, `Player.PickTile_DetermineDamage`, `Player.GetPickaxeDamage`,
  `Player.DoesPickTargetTransformOnKill`, `Player.ClearMiningCacheAt`,
* `Terraria.HitTile` (`HitObject`, `AddDamage`, `SortSlots`, `Clear`, `TryFinding`, `Prune`),
* and, behind a read-only adapter, `WorldGen.KillTile` / `SquareTileFrame` / `TileFrame` for ordinary
  solid blocks (`plainTarget`). The sound/dust prefix of each KillTile is delegated to the released
  `OriginalTileHitRuntime.emit`.

The caller runs `ItemCheck`'s own swing bookkeeping first (start/renew, the `itemAnimation` and
`itemTime` decrements, `releaseUseItem := !controlUseItem`, `toolTime := itemTime`) and passes the
result in `Swing`. Random draws of this phase, in order: `AddDamage` (one `NextFloat` per recording
call), each KillTile's sound/dust (TileHitRuntime), the `SquareTileFrame` centre re-roll of a still
active block (`genRand.Next(0, 3)`, `genRand == Main.rand`), and the two velocity samples of a drop.

Anything outside the checked scope (tree/altar targets, frame-important or non-solid neighbours,
multiplayer client, dungeon/special tiles, axes and hammers) is reported in `Outcome.unsupported`
before any state changes: the caller must route it elsewhere instead of dropping it.
-/
namespace Lean4Terria.OriginalMiningTools

open OriginalCollision (Vector single Grid)
open OriginalWorld (Terrain Tile TileShape)
open OriginalHammerForeground (HitTile HitEntry)

/-- The `Item` fields this branch reads (values from `Item.SetDefaults`, read natively). -/
structure Pickaxe where
  itemType : Nat
  pick : Nat
  axe : Nat := 0
  hammer : Nat := 0
  useTime : Nat
  useAnimation : Nat
  tileBoost : Int
  deriving BEq, Repr

def copperPickaxe : Pickaxe := {itemType := 3509, pick := 35, useTime := 15, useAnimation := 23, tileBoost := -1}
def ironPickaxe : Pickaxe := {itemType := 1, pick := 40, useTime := 13, useAnimation := 20, tileBoost := 0}
def goldPickaxe : Pickaxe := {itemType := 3521, pick := 55, useTime := 17, useAnimation := 20, tileBoost := 0}
def nightmarePickaxe : Pickaxe := {itemType := 103, pick := 65, useTime := 15, useAnimation := 20, tileBoost := 0}

/-- World statics read on this path. -/
structure Rules where
  maxTilesX : Nat
  maxTilesY : Nat
  worldSurface : Float
  hardMode : Bool := false
  getGoodWorld : Bool := false
  infectedSeed : Bool := false
  notTheBeesWorld : Bool := false
  noTrapsWorld : Bool := false
  tenthAnniversaryWorld : Bool := false
  netMode : Int := 0
  generatingWorld : Bool := false
  /-- `Player.tileRangeX` / `tileRangeY` (statics; 5 and 3 in a normal session). -/
  tileRangeX : Int := 5
  tileRangeY : Int := 3
  gamepadDisableCursorItemIcon : Bool := false

/-- `Main.UnderworldLayer`. -/
def Rules.underworldLayer (rules : Rules) : Int := (rules.maxTilesY : Int) - 200

/-- The player after `ItemCheck`'s swing bookkeeping of this frame. -/
structure Swing where
  position : Vector
  width : Nat := 20
  height : Nat := 42
  itemAnimation : Nat
  toolTime : Nat
  controlUseItem : Bool
  releaseUseItem : Bool
  poundRelease : Bool
  noBuilding : Bool := false
  pickSpeed : Float32 := 1
  /-- `Player.tileTargetX/Y`. -/
  target : Int × Int

structure State where
  terrain : Terrain
  items : OriginalItems.WorldItems
  random : UnifiedRandom
  hits : HitTile := {}
  /-- `Player.hitReplace` (block swap cache): only `ClearMiningCacheAt` touches it here. -/
  hitReplace : HitTile := {}
  /-- `Tile.frameNumber` of cells re-rolled by this path. -/
  frameNumbers : Std.HashMap (Nat × Nat) Nat := {}
  /-- Shared KillTile sound/dust accumulator (`none` for headless callers: no effect draws). -/
  effects : Option OriginalTileHitRuntime.State := none

/-- Ordered requests of one call, in vanilla order. -/
inductive Event where
  | actual (x y : Int)
  | pick (x y : Int) (power : Nat)
  | addDamage (slot : Nat) (damage : Int) (update : Bool) (result : Int)
  | kill (x y : Int) (fail : Bool)
  | sound (request : OriginalTileHitEffects.SoundRequest) (played : Option OriginalTileHitEffects.PlayedSound)
  | dust (particle : OriginalForestGuideArrow.DustParticle)
  | drop (slot : Nat) (item : UInt16)
  | square (x y : Int)
  /-- `TileFrame(x, y, resetFrame: true)` of the `SquareTileFrame` centre, at its entry (also when the
  cell is empty or out of bounds); a re-rolled variant lands in `State.frameNumbers`. -/
  | centreFrame (x y : Int)
  | addWater (x y : Int)

structure Outcome where
  state : State
  /-- The direct `itemTime` store of `ActuallyUseMiningTool` (`itemTimeMax` is untouched). -/
  itemTime : Option Nat := none
  poundRelease : Bool
  cursorItemIconEnabled : Bool := false
  events : Array Event := #[]
  unsupported : Option String := none

/-! ## Tile helpers -/

private def tileAt (terrain : Terrain) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (terrain.tile? x.toNat y.toNat).getD default

private def typeOf (t : Tile) : Nat := (t.id.getD 0).toNat
private def active (t : Tile) : Bool := t.id.isSome

/-- Tiles whose whole KillTile/TileFrame path is covered here: solid, not frame-important,
not falling, not gem-framed, with a checked drop or a fail-only transform. -/
def plainTarget (id : Nat) : Bool := (Tables.dropFor id).isSome || id == 2

/-! The pickaxe's own tile-hit cache (`Terraria.HitTile`), translated slot for slot. -/
namespace Hits

def tryFinding (h : HitTile) (x y : Int) (type : Nat) : Option Nat := Id.run do
  for i in [0:501] do
    let slot := h.order[i]!
    let e := h.data[slot]!
    if e.type == type then
      if e.x == x && e.y == y then return some slot
    else if i != 0 && e.type == 0 then return none
  return none

/-- `HitObject(x, y, type)`: the slot holding the cell, else the buffer `Prepare`d for it
(`Prepare` keeps the buffer's damage and time to live). -/
def hitObject (h : HitTile) (x y : Int) (type : Nat) : HitTile × Nat :=
  match tryFinding h x y type with
  | some slot => (h, slot)
  | none =>
    let b := h.bufferLocation
    let e := h.data[b]!
    ({ h with data := h.data.set! b { e with x, y, type } }, b)

/-- `Clear(slot)`: empty the slot and move it to `order[500]`. -/
def clear (h : HitTile) (slot : Nat) : HitTile := Id.run do
  if slot > 500 then return h
  let mut order := h.order
  let mut i := 0
  while i < 500 && order[i]! != slot do i := i + 1
  while i < 500 do
    order := order.set! i order[i + 1]!
    i := i + 1
  order := order.set! 500 slot
  return { h with data := h.data.set! slot {}, order }

def sortSlots (h : HitTile) (slot : Nat) : HitTile := Id.run do
  let mut h := h
  if slot == h.bufferLocation then
    let b := h.order[500]!
    h := { h with bufferLocation := b }
    if slot != b then h := { h with data := h.data.set! b {} }
    let mut order := h.order
    let mut i := 500
    while i > 0 do
      order := order.set! i order[i - 1]!
      i := i - 1
    return { h with order := order.set! 0 b }
  let mut order := h.order
  let mut i := 0
  while i ≤ 500 && order[i]! != slot do i := i + 1
  while i > 1 do
    let tmp := order[i - 1]!
    order := (order.set! (i - 1) order[i]!).set! i tmp
    i := i - 1
  return { h with order := order.set! 1 slot }

/-- `AddDamage(slot, damage, updateAmount)`. Recording draws one `Main.rand.NextFloat()` for the crack
animation direction. -/
def addDamage (h : HitTile) (slot : Nat) (damage : Int) (update : Bool) (random : UnifiedRandom) :
    HitTile × Int × UnifiedRandom :=
  if slot > 500 then (h, 0, random)
  else if slot == h.bufferLocation && damage == 0 then (h, 0, random)
  else if !update then (h, h.data[slot]!.damage + damage, random)
  else
    let e := h.data[slot]!
    let h := { h with data := h.data.set! slot { e with damage := e.damage + damage, timeToLive := 60 } }
    let (_, random) := random.nextInt
    let h := sortSlots h slot
    (h, h.data[slot]!.damage, random)

def tryClearingAndPruning (h : HitTile) (terrain : Terrain) (x y : Int) (type : Nat) : HitTile :=
  match tryFinding h x y type with
  | none => h
  | some slot => (clear h slot).prune terrain

end Hits

/-! ## Damage -/

/-- `Player.GetPickaxeDamage`; `none` for the frame-dependent targets (128, 269, 334) that move the
hit to another cell. -/
def pickaxeDamage (rules : Rules) (x y : Int) (power : Nat) (tile : Tile) : Option Int := Id.run do
  let t := typeOf tile
  let p : Int := power
  let frameY := (tile.frame.map (·.2)).getD 0
  let mut d : Int := if Tables.noFail t then 100 else 0
  if Tables.dungeon t || t == 58 || t == 25 || t == 117 || t == 203 then d := d + p / 2
  else if t == 85 then d := d + (if rules.getGoodWorld then p / 4 else p)
  else if t == 48 || t == 232 || Tables.clouds t then d := d + p * 2
  else if t == 226 then d := d + p / 4
  else if t == 107 || t == 221 then d := d + p / 2
  else if t == 108 || t == 222 then d := d + p / 3
  else if t == 111 || t == 223 then d := d + p / 4
  else if t == 211 then d := d + p / 5
  else d := d + p
  if t == 211 && p < 200 then d := 0
  let yF := Float.ofInt y
  if !rules.infectedSeed && (t == 25 || t == 203) && p < 65 then d := 0
  else if t == 117 && p < 65 then d := 0
  else if t == 37 && p < 50 then d := 0
  else if (t == 22 || t == 204) && yF > rules.worldSurface && p < 55 then d := 0
  else if t == 56 && p < 55 then d := 0
  else if t == 77 && p < 65 && y ≥ rules.underworldLayer then d := 0
  else if t == 58 && p < 65 then d := 0
  else if (t == 226 || t == 237) && p < 210 then d := 0
  else if t == 137 && p < 210 && !(rules.notTheBeesWorld && rules.noTrapsWorld && !rules.tenthAnniversaryWorld) then
    let style := frameY.tdiv 18
    if 1 ≤ style && style ≤ 4 then d := 0
  else if Tables.dungeon t && p < 100 && yF > rules.worldSurface then
    let xF := Float.ofInt x
    let w := rules.maxTilesX.toFloat
    if xF < w * 0.35 || !(xF ≤ w * 0.65) then d := 0
  else if (t == 107 || t == 221) && p < 100 then d := 0
  else if (t == 108 || t == 222) && p < 110 then d := 0
  else if (t == 111 || t == 223) && p < 150 then d := 0
  if t == 147 || t == 0 || t == 40 || t == 53 || t == 57 || t == 59 || t == 123 || t == 224 || t == 397 then
    d := d + p
  if t == 404 then d := d + 5
  if t == 165 || Tables.rope t || t == 199 then d := 100
  if t == 128 || t == 269 || t == 334 then return none
  return some d

/-- `Player.DoesPickTargetTransformOnKill`'s tile set (grass, jungle grass, moss…: the block turns into its
base instead of breaking). -/
def transformsOnKill (t : Nat) : Bool :=
  t == 2 || t == 477 || t == 492 || t == 23 || t == 60 || t == 70 || t == 109 || t == 199 ||
    Tables.moss t || t == 662 || t == 661 || Tables.mossBrick t || t == 633

/-- `KillTile(fail: true)`'s type change of an exposed block. -/
def failType (t : Nat) : Nat :=
  if t == 2 || t == 23 || t == 109 || t == 199 || t == 477 || t == 492 then 0
  else if t == 633 then 57
  else if t == 60 || t == 661 || t == 662 || t == 70 then 59
  else if Tables.moss t then 1
  else if Tables.mossBrick t then 38
  else t

/-- `Player.IsInTileInteractionRange(x, y, TileReachCheckSettings.Simple, tileBoost)`. -/
def inReach (rules : Rules) (pick : Pickaxe) (s : Swing) (x y : Int) : Bool :=
  let rx := min rules.tileRangeX 20 + pick.tileBoost
  let ry := min rules.tileRangeY 20 + pick.tileBoost
  let lx := (s.position.x / 16).toInt64.toInt - rx
  let hx := ((s.position.x + single s.width) / 16).toFloat.ceil.toInt64.toInt - 1 + rx
  let ly := (s.position.y / 16).toInt64.toInt - ry
  let hy := ((s.position.y + single s.height) / 16).toFloat.ceil.toInt64.toInt - 1 + ry
  lx ≤ x && x ≤ hx && ly ≤ y && y ≤ hy

/-! ## The phase -/

private structure S where
  st : State
  events : Array Event := #[]
  unsupported : Option String := none

private abbrev M := StateM S

private def tile (x y : Int) : M Tile := return tileAt (← get).st.terrain x y
private def setTile (x y : Int) (t : Tile) : M Unit := do
  if x < 0 || y < 0 then return
  modify fun s => match s.st.terrain.replaceTile x.toNat y.toNat t with
    | some terrain => { s with st := { s.st with terrain } }
    | none => s
private def emit (e : Event) : M Unit := modify fun s => { s with events := s.events.push e }
private def fail (reason : String) : M Unit :=
  modify fun s => if s.unsupported.isSome then s else { s with unsupported := some reason }
private def blocked : M Bool := do
  let s ← get
  return s.unsupported.isSome || OriginalTileHitRuntime.blocked s.st.effects

private def inWorld (rules : Rules) (x y : Int) : Bool :=
  x ≥ 0 && y ≥ 0 && x < rules.maxTilesX && y < rules.maxTilesY

/-- `WorldGen.TileFrame(x, y, resetFrame)` for ordinary blocks and empty cells. -/
private def tileFrame (rules : Rules) (x y : Int) (reset : Bool) : M Unit := do
  if ← blocked then return
  if reset then emit (.centreFrame x y)
  if !(x > 5 && y > 5 && x < (rules.maxTilesX : Int) - 5 && y < (rules.maxTilesY : Int) - 5) then return
  let t ← tile x y
  if !active t then
    -- halfBrick(false), ClearBlockPaintAndCoating, slope(0)
    setTile x y { t with shape := .full, tilePaint := 0, invisibleBlock := false, fullbrightBlock := false }
  if (t.liquid.any (·.amount > 0)) && rules.netMode != 1 && !rules.generatingWorld then emit (.addWater x y)
  let some id := t.id | return
  let id := id.toNat
  if !plainTarget id then
    fail s!"TileFrame: neighbour tile {id} outside the ordinary-block scope"; return
  if rules.generatingWorld then return
  -- TileFrameCosmetic: only the centre re-rolls its variant
  if reset && x > 0 && y > 0 && x < (rules.maxTilesX : Int) - 1 && y < (rules.maxTilesY : Int) - 1 then
    let s ← get
    let (n, random) := s.st.random.next 0 3
    set { s with st := { s.st with random, frameNumbers := s.st.frameNumbers.insert (x.toNat, y.toNat) n.toNat } }

/-- `WorldGen.SquareTileFrame(x, y, true)`: columns x−1, x, x+1, each y−1 … y+1; only the centre resets. -/
private def squareTileFrame (rules : Rules) (x y : Int) : M Unit := do
  emit (.square x y)
  for dx in [0:3] do
    for dy in [0:3] do
      let cx := x - 1 + dx
      let cy := y - 1 + dy
      tileFrame rules cx cy (dx == 1 && dy == 1)

/-- `WorldGen.CheckTileBreakability` within the ordinary-block scope: 0 (break), or `none` when a
neighbour needs a check this module does not own. -/
private def breakability (rules : Rules) (x y : Int) : M (Option Nat) := do
  if y < 1 || y > (rules.maxTilesY : Int) - 1 then return some 0
  let below ← tile x (y + 1)
  if below.id == some 10 then return none
  let above ← tile x (y - 1)
  match above.id with
  | none => return some 0
  | some a => return if plainTarget a.toNat then some 0 else none

/-- The KillTile sound/dust prefix through the shared runtime; adds its new requests to the event log. -/
private def effects (x y : Int) (t : Tile) (failHit : Bool) : M Unit := do
  let s ← get
  let before := s.st.effects
  let (after, random) := OriginalTileHitRuntime.emit before t x y failHit s.st.random
  set { s with st := { s.st with effects := after, random } }
  match before, after with
  | some b, some a =>
    if let some boundary := a.boundary then fail s!"KillTile effects: {boundary.reason}"
    -- one KillTile requests at most one sound; it is resolved iff the played list grew
    for req in a.soundRequests.extract b.soundRequests.size a.soundRequests.size do
      emit (.sound req (if a.resolvedSounds.size > b.resolvedSounds.size then a.resolvedSounds.back? else none))
    for p in a.particles.extract b.particles.size a.particles.size do emit (.dust p)
  | _, _ => pure ()

/-- `WorldGen.KillTile(x, y, fail)` for an ordinary block (effectOnly = noItem = false). -/
private def killTile (rules : Rules) (x y : Int) (failHit : Bool) : M Unit := do
  if ← blocked then return
  if !inWorld rules x y then return
  let t ← tile x y
  let some id := t.id | return
  emit (.kill x y failHit)
  let some _ ← breakability rules x y | fail "CheckTileBreakability: neighbour outside scope"; return
  effects x y t failHit
  if ← blocked then return
  if failHit then
    let nt := failType id.toNat
    if nt != id.toNat then setTile x y { t with id := some nt.toUInt16 }
    squareTileFrame rules x y
    return
  -- drops (stopDrops/netMode 1 are outside this single-player path)
  match Tables.dropFor id.toNat with
  | none => fail s!"KillTile_DropItems: no checked drop for tile {id}"; return
  | some item =>
    let s ← get
    let slot := (s.st.items.findIdx? Option.isNone).getD s.st.items.size
    let grid := Grid.ofTerrain s.st.terrain OriginalTileRules.collision
    let (items, random) := OriginalItems.spawnStack s.st.items ⟨x.toNat, y.toNat⟩ (.ofItem item.toUInt16)
      s.st.random (some grid)
    set { s with st := { s.st with items, random } }
    emit (.drop slot item.toUInt16)
  let cleared := OriginalMining.clearGround t
  let cleared := if id == 58 && y > rules.underworldLayer then
      { cleared with liquid := some ⟨.lava, 128⟩ } else cleared
  setTile x y cleared
  modify fun s => { s with st := { s.st with frameNumbers := s.st.frameNumbers.erase (x.toNat, y.toNat) } }
  squareTileFrame rules x y

private def clearMiningCacheAt (x y : Int) : M Unit := modify fun s =>
  { s with st := { s.st with
      hitReplace := Hits.tryClearingAndPruning s.st.hitReplace s.st.terrain x y 1
      hits := Hits.tryClearingAndPruning s.st.hits s.st.terrain x y 1 } }

private def addDamage (slot : Nat) (damage : Int) (update : Bool) : M Int := do
  let s ← get
  let (hits, result, random) := Hits.addDamage s.st.hits slot damage update s.st.random
  set { s with st := { s.st with hits, random } }
  emit (.addDamage slot damage update result)
  return result

private def hitObject (x y : Int) : M Nat := do
  let s ← get
  let (hits, slot) := Hits.hitObject s.st.hits x y 1
  set { s with st := { s.st with hits } }
  return slot

/-- `Player.PickTile(x, y, power, -1)`. -/
private def pickTile (rules : Rules) (x y : Int) (power : Nat) : M Unit := do
  emit (.pick x y power)
  let t ← tile x y
  if typeOf t == 504 then return
  -- PickTile_DetermineDamage
  let slot ← hitObject x y
  let some base := pickaxeDamage rules x y power t | fail "GetPickaxeDamage: frame-relocated target"; return
  let mut damage := base
  if !OriginalHammerForeground.canKillTile (← get).st.terrain x y then damage := 0
  if rules.getGoodWorld then damage := damage * 2
  if (← addDamage slot damage false) ≥ 100 && transformsOnKill (typeOf t) then damage := 0
  if (← addDamage slot damage true) ≥ 100 then
    clearMiningCacheAt x y
    if rules.netMode == 1 then fail "PickTile: multiplayer client path"; return
    killTile rules x y false
  else
    killTile rules x y true
  if damage != 0 then
    modify fun s => { s with st := { s.st with hits := s.st.hits.prune s.st.terrain } }

/-- One frame of `Player.ItemCheck_UseMiningTools` with a pickaxe (`pick > 0`, `axe = hammer = 0`). -/
def useMiningTools (rules : Rules) (pick : Pickaxe) (swing : Swing) (state : State) : Outcome := Id.run do
  if pick.axe > 0 || pick.hammer > 0 then
    return { state, poundRelease := swing.poundRelease, unsupported := some "axe/hammer item: other owners" }
  if pick.pick == 0 then return { state, poundRelease := swing.poundRelease }
  let (tx, ty) := swing.target
  let target := tileAt state.terrain tx ty
  let live := swing.toolTime == 0 && swing.itemAnimation > 0 && swing.controlUseItem
  let inRange := inReach rules pick swing tx ty && !swing.noBuilding
  -- refuse before any change when the actual hit would leave the scope
  if live && inRange then
    if let some id := target.id then
      if !plainTarget id.toNat then
        return { state, poundRelease := swing.poundRelease,
                 unsupported := some s!"pickaxe target tile {id} outside the ordinary-block scope" }
  let mut poundRelease := swing.poundRelease
  if live && (!active target || !OriginalHammerForeground.isAHammerTarget target) then poundRelease := false
  if !inRange then return { state, poundRelease }
  let cursorItemIconEnabled := !rules.gamepadDisableCursorItemIcon
  let mut itemTime : Option Nat := none
  let mut s : S := { st := state }
  if live then
    s := { s with events := s.events.push (.actual tx ty) }
    if active target then
      let (_, s') := (do let _ ← hitObject tx ty; pickTile rules tx ty pick.pick : M Unit).run s
      s := s'
      itemTime := some ((single pick.useTime * swing.pickSpeed).toInt64.toInt.toNat)
      -- TryPoundingTile: hammer = 0 only clears an armed poundRelease
      poundRelease := false
  if swing.releaseUseItem then poundRelease := true
  return { state := s.st, itemTime, poundRelease, cursorItemIconEnabled, events := s.events,
           unsupported := s.unsupported }

end Lean4Terria.OriginalMiningTools
