import Lean4Terria.OriginalBankMining
import Lean4Terria.OriginalChests

/-!
# World chests and dressers: placement, runtime index, pickaxe and support (Terraria 1.4.5.8 Classic)

Hand translation of the single-player vanilla paths for tile 21 (chests), 467 (chests 2) and 88
(dressers):

* **Runtime index.** `Main.chest` is a sparse array of 8000 slots. `WorldFile.LoadChests` fills
  slots 0…n−1 in file order; `Chest.FindEmptyChest(x, y)` returns −1 when a chest already has that
  top-left, otherwise the lowest null slot; `Chest.DestroyChest` refuses a chest that holds an
  item, otherwise nulls the slot and closes it for the local player (`player.chest = -1`);
  `WorldFile.SaveChests` writes the non-null slots in index order and drops records whose four
  tiles are not containers.
  Here the session's `Array Chest` *is* that array: a vacant slot is the tombstone `vacant`
  (`x = y = −1`, no items), so `OpenStorage.chest i` and crafting sources keep their meaning, and
  `OriginalWorld.Archive.encode` already drops tombstones when saving (no container tiles at −1).
* **Placement.** `PlaceThing` → `TileObject.Place` (2 × 2, origin = bottom-left; dresser 3 × 2,
  origin = bottom-middle) → `Chest.AfterPlacement_Hook` → `FindEmptyChest` → `CreateWorldChest`
  (40 empty slots, name ""). Anchors: every cell below must be a solid, unactuated, full or
  bottom-sloped block, or a platform on a proper top frame (no tables); measured in
  `.lake/original-chest-placement/1.4.5.8/native/work/chest_place_on_*.json`.
* **Pickaxe.** `PickTile`: `CanKillTile` gives damage 0 unless `Chest.CanDestroyChest` (no items);
  `HitTileObject.AddDamage` draws one sample per hit with damage or an existing cache entry; at
  100 the cell is killed: `CheckTileBreakability2_ShouldTileSurvive` destroys the record at the
  top-left (closing it), the cell is cleared, then `SquareTileFrame(x, y, true)` reaches the first
  intact cell of the object, whose check removes the rest and drops the item once at the 32 × 32
  rectangle of that triggering cell.
* **Support.** `CanKillTile` refuses any tile whose upper neighbour is 21/88/467 (already in
  `OriginalMining`). `CheckChest` breaks a chest whose cell below is not an active solid tile only
  when the chest is empty; `Check3x2` (dresser) uses `SolidTileAllowBottomSlope`.

Random stream: as in `OriginalBankMining`, only the hit's own samples are drawn here (1 for
`AddDamage`, 2 per drop). Desktop dig sounds and on-screen dust draw more; see that module.
-/

namespace Lean4Terria.OriginalChestMining

open OriginalCollision (Grid Vector single)
open OriginalItems (Stack WorldItems)
open OriginalItemUse (Timing)
open OriginalMining (Cell HitCache WorldRules inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Chest ChestItem Terrain Tile)
open OriginalTileRules (collision)

/-! ## Runtime index (`Main.chest`) -/

def maxChests : Nat := 8000

/-- An empty `Main.chest` slot. -/
def vacant : Chest := { x := -1, y := -1, name := "", items := #[] }

def isVacant (chest : Chest) : Bool := chest.isVacant

/-- `WorldFile.LoadChests`: records take runtime slots 0, 1, … in file order. -/
def ofRecords (records : Array Chest) : Array Chest := records

/-- `WorldFile.SaveChests` order: occupied slots in index order. `Archive.encode` performs the
same filtering itself, so the session array can be passed to it unchanged. -/
def toRecords (chests : Array Chest) : Array Chest := chests.filter (!isVacant ·)

/-- `Chest.FindChest(x, y)`: the slot whose top-left is exactly (x, y). -/
def findChest (chests : Array Chest) (x y : Int) : Option Nat :=
  chests.findIdx? fun chest => !isVacant chest && chest.x == x && chest.y == y

/-- `Chest.FindEmptyChest(x, y)`: `none` (−1) when a chest already has this top-left or all
8000 slots are used, otherwise the lowest vacant slot (slots past the array end are vacant). -/
def firstFree (chests : Array Chest) (x y : Int) : Option Nat :=
  if (findChest chests x y).isSome then none
  else match chests.findIdx? isVacant with
    | some index => some index
    | none => if chests.size < maxChests then some chests.size else none

/-- `Chest.CreateWorldChest(index, x, y)`: 40 empty slots, no name. -/
def create (chests : Array Chest) (index : Nat) (x y : Int) : Array Chest :=
  let chest : Chest := { x, y, name := "", items := Array.replicate 40 none }
  let chests := if index < chests.size then chests else chests ++ Array.replicate (index + 1 - chests.size) vacant
  chests.set! index chest

private def holdsItem (item : Option ChestItem) : Bool :=
  item.any fun item => item.typeId > 0 && item.count > 0

/-- `Chest.CanDestroyChest(x, y)`: true without a record at (x, y), else true iff it holds nothing. -/
def canDestroy (chests : Array Chest) (x y : Int) : Bool :=
  match findChest chests x y with
  | none => true
  | some index => !((chests[index]?.getD vacant).items.any holdsItem)

/-- `Chest.DestroyChest(x, y)`: `none` when the chest holds an item (it survives); otherwise the
updated array and the slot that was freed, if there was a record. The caller closes an open
container on that slot (`player.chest = -1`). -/
def destroy (chests : Array Chest) (x y : Int) : Option (Array Chest × Option Nat) :=
  match findChest chests x y with
  | none => some (chests, none)
  | some index =>
    if (chests[index]?.getD vacant).items.any holdsItem then none
    else some (chests.set! index vacant, some index)

/-! ## Item ↔ style tables (`WorldGen.GetItemDrop_Chests`, `GetItemDrop_Dressers`) -/

/-- Tile 21 by style (`frameX / 36`), decoded from the switch in 1.4.5.8. -/
def chestItems : Array UInt16 :=
  #[48, 306, 306, 328, 328, 343, 348, 625, 626, 627, 680, 681, 831, 838, 914, 952, 1142, 1298, 1528,
    1529, 1530, 1531, 1532, 1528, 1529, 1530, 1531, 1532, 2230, 2249, 2250, 2526, 2544, 2559, 2574,
    2612, 2612, 2613, 2613, 2614, 2614, 2615, 2616, 2617, 2618, 2619, 2620, 2748, 2814, 3180, 3125, 3181]

/-- Tile 467 styles 1…37 (style 0 and anything past the table drop 3884). -/
def chest2Items : Array UInt16 :=
  #[3885, 3939, 3965, 3988, 4153, 4174, 4195, 4216, 4265, 4267, 4574, 4712, 4712, 5156, 5177, 5198,
    5556, 5609, 5697, 5720, 5745, 5763, 5784, 5805, 5826, 5846, 5865, 5886, 5905, 5939, 5962, 5982,
    6005, 6028, 6051, 6074, 6118]

def chestDrop (id : UInt16) (style : Nat) : Option UInt16 :=
  match id with
  | 21 => some (chestItems[style]?.getD 48)
  | 467 => some (if style == 0 then 3884 else chest2Items[style - 1]?.getD 3884)
  | _ => none

/-- `GetItemDrop_Dressers` for styles 0…64, also the default item style order. -/
def dresserItems : Array UInt16 :=
  #[334, 647, 648, 649, 918, 2386, 2387, 2388, 2389, 2390, 2391, 2392, 2393, 2394, 2395, 2396,
    2529, 2545, 2562, 2577, 2637, 2638, 2639, 2640, 2816, 3132, 3134, 3133, 3911, 3912, 3913,
    3914, 3934, 3968, 4148, 4169, 4190, 4211, 4301, 4569, 5151, 5172, 5193, 5551, 5604, 5692,
    5715, 5741, 5766, 5787, 5808, 5829, 5848, 5868, 5888, 5908, 5942, 5965, 5985, 6008, 6031,
    6054, 6077, 6099, 6121]

def dresserDrop (style : Nat) : Option UInt16 := some (dresserItems[style]?.getD 334)

/-- The tile and style an item places, for the containers covered here. A style that several
items share (locked and unlocked forms) maps from the unlocked item only. -/
def placeFor (item : UInt16) : Option (UInt16 × Nat) :=
  if item == 3884 then some (467, 0)
  else if let some style := chestItems.findIdx? (· == item) then some (21, style)
  else if let some style := chest2Items.findIdx? (· == item) then some (467, style + 1)
  else (dresserItems.findIdx? (· == item)).map fun style => (88, style)

def isContainerTile (id : UInt16) : Bool := id == 21 || id == 467 || id == 88

/-- Top-left of the container holding `cell` (2 wide for chests, 3 for dressers). -/
def topLeft (tile : Tile) (cell : Cell) : Option (Int × Int) := do
  let id ← tile.id
  guard (isContainerTile id)
  let (fx, fy) := tile.frame.getD (0, 0)
  let width : Int := if id == 88 then 3 else 2
  let row := (if id == 88 then fy.tmod 36 else fy).tdiv 18
  return ((cell.x : Int) - (fx.tdiv 18).tmod width, (cell.y : Int) - row)

/-! ## World state for one break -/

structure World where
  terrain : Terrain
  items : WorldItems
  random : UnifiedRandom
  chests : Array Chest
  destroying : Bool := false
  effects : Option OriginalTileHitRuntime.State := none
  frameNumbers : Std.HashMap (Nat × Nat) Nat := {}
  /-- Slots freed by `DestroyChest`, in order: close `OpenStorage.chest i` for each. -/
  freed : Array Nat := #[]
  drops : Array (UInt16 × Cell) := #[]
  draws : Nat := 0

private def World.tile (w : World) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (w.terrain.tile? x.toNat y.toNat).getD default

private def World.inWorld (w : World) (x y : Int) : Bool :=
  x ≥ 0 && y ≥ 0 && x < w.terrain.header.width && y < w.terrain.header.height

private def active (t : Tile) : Bool := t.id.isSome
private def frameX (t : Tile) : Int := (t.frame.map (·.1)).getD 0
private def frameY (t : Tile) : Int := (t.frame.map (·.2)).getD 0
private def isType (t : Tile) (id : UInt16) : Bool := t.id == some id

private def World.prefix (w : World) (x y : Int) (fail : Bool) : World :=
  let (effects,random) := OriginalTileHitRuntime.emit w.effects (w.tile x y) x y fail w.random
  {w with effects,random}

private def World.clear (w : World) (x y : Int) : World :=
  if OriginalTileHitRuntime.blocked w.effects || !w.inWorld x y then w else
  match w.terrain.replaceTile x.toNat y.toNat (OriginalMining.clearGround (w.tile x y)) with
  | some terrain => { w with terrain }
  | none => w

private def World.destroyRecord (w : World) (x y : Int) : Option World := do
  let (chests, freed) ← destroy w.chests x y
  return { w with chests, freed := match freed with | some i => w.freed.push i | none => w.freed }

/-- `KillTile(x, y)` on a container cell: `CheckTileBreakability2_ShouldTileSurvive` destroys the
record at the top-left first; a chest that still holds items survives untouched. -/
private def World.killContainerCell (w : World) (x y : Int) : World :=
  let w := w.prefix x y false
  if OriginalTileHitRuntime.blocked w.effects then w else
  let t := w.tile x y
  match topLeft t ⟨x.toNat, y.toNat⟩ with
  | none => w.clear x y
  | some (lx, ly) => match w.destroyRecord lx ly with
    | none => w
    | some w => w.clear x y

private def World.drop (w : World) (item : UInt16) (x y : Int) : World :=
  if OriginalTileHitRuntime.blocked w.effects then w else
  let position : Vector := ⟨single (x * 16 + 8), single (y * 16 + 8)⟩
  let (items, random) := OriginalItems.spawnStackAt w.items position (.ofItem item) w.random
    (some (Grid.ofTerrain w.terrain collision))
  { w with items, random, draws := w.draws + 2, drops := w.drops.push (item, ⟨x.toNat, y.toNat⟩) }

/-- Candles (49, 372, 646) are framed by `OriginalBankMining`. Its other objects stay untouched
here (`destroying`), as before; `CheckOnTable1x1` runs whatever `destroyObject` holds. -/
private def World.candles (w : World) (f : OriginalBankMining.World → OriginalBankMining.World) : World :=
  let b := f {
    terrain := w.terrain, items := w.items, random := w.random, destroying := true
    effects := w.effects,frameNumbers := w.frameNumbers }
  { w with terrain := b.terrain, items := b.items, random := b.random, effects := b.effects
           frameNumbers := b.frameNumbers
           draws := w.draws + b.draws, drops := w.drops ++ b.drops }

/-- `WorldGen.CheckChest` (tiles 21 and 467). -/
def checkChest (w : World) (i j : Int) (id : UInt16) : World := Id.run do
  if w.destroying then return w
  let here := w.tile i j
  let mut column := (frameX here).tdiv 18
  while column > 1 do column := column - 2
  let left := i - column
  let top := j - (frameY here).tdiv 18
  let mut broken := false
  for dx in [0:2] do
    let x := left + dx
    for dy in [0:2] do
      let y := top + dy
      let t := w.tile x y
      let mut parity := (frameX t).tdiv 18
      while parity > 1 do parity := parity - 2
      if !(active t && isType t id && parity == x - left && frameY t == (y - top) * 18) then broken := true
    let below := w.tile x (top + 2)
    if !(active below && below.id.any (fun b => (collision b).solid)) && canDestroy w.chests left top then
      broken := true
  if !broken then return w
  let item := chestDrop id ((frameX here).tdiv 36).toNat
  let mut w := { w with destroying := true }
  for dx in [0:2] do
    for dy in [0:2] do
      let t := w.tile (left + dx) (top + dy)
      if isType t id && active t then
        -- DestroyChest(x, y) for the cell, then KillTile(x, y) (whose survive check repeats it)
        w := (w.destroyRecord (left + dx) (top + dy)).getD w
        w := w.killContainerCell (left + dx) (top + dy)
  if let some item := item then w := w.drop item i j
  return { w with destroying := false }

mutual

/-- `TileFrame` for containers, then `OriginalBankMining`'s objects are left to that module. -/
partial def tileFrame (w : World) (x y : Int) : World :=
  if OriginalTileHitRuntime.blocked w.effects then w else
  if x ≤ 5 || y ≤ 5 || x ≥ w.terrain.header.width - 5 || y ≥ w.terrain.header.height - 5 then w else
  match (w.tile x y).id with
  | some 21 => checkChest w x y 21
  | some 467 => checkChest w x y 467
  | some 88 => checkDresser w x y
  | some id =>
    if OriginalBankMining.shapeOf id == some .onTable then w.candles (OriginalBankMining.tileFrame · x y) else w
  | none => w

/-- `WorldGen.Check3x2` for the dresser (88). -/
partial def checkDresser (w : World) (i j : Int) : World := Id.run do
  if w.destroying then return w
  let here := w.tile i j
  let styleY := (frameY here).tdiv 36
  let top := j - ((frameY here).tmod 36).tdiv 18
  let mut column := (frameX here).tdiv 18
  let mut style := 0
  while column > 2 do
    column := column - 3
    style := style + 1
  let left := i - column
  let mut broken := false
  for dx in [0:3] do
    let x := left + dx
    for dy in [0:2] do
      let y := top + dy
      let t := w.tile x y
      if !(active t && isType t 88 && frameX t == (x - left) * 18 + 54 * style &&
          frameY t == (y - top) * 18 + styleY * 36) then broken := true
    if !OriginalBankMining.solidTileAllowBottomSlope
        { terrain := w.terrain, items := w.items, random := w.random } x (top + 2) then broken := true
  if !broken then return w
  let mut w := { w with destroying := true }
  for dx in [0:3] do
    for dy in [0:2] do
      let t := w.tile (left + dx) (top + dy)
      if isType t 88 && active t then
        w := w.killContainerCell (left + dx) (top + dy)
        -- the cell's own `SquareTileFrame`: a candle on the dresser falls before the dresser drops
        if !active (w.tile (left + dx) (top + dy)) then
          w := w.candles (OriginalBankMining.squareFrame · (left + dx) (top + dy))
  if let some item := dresserDrop style.toNat then w := w.drop item i j
  w := { w with destroying := false }
  for dx in [0:5] do
    for dy in [0:5] do
      w := tileFrame w (left - 1 + dx) (top - 1 + dy)
  return w

end

/-- `SquareTileFrame(x, y, true)` restricted to containers (no reset-frame sample: the centre is
the cell just cleared). -/
def squareFrame (w : World) (x y : Int) : World := Id.run do
  let mut w := w
  for dx in [0:3] do
    for dy in [0:3] do
      w := tileFrame w (x - 1 + dx) (y - 1 + dy)
  return w

/-- `CanKillTile` for a container cell: refused while the container holds an item. -/
def canKill (chests : Array Chest) (tile : Tile) (cell : Cell) : Bool :=
  match topLeft tile cell with
  | some (x, y) => canDestroy chests x y
  | none => true

structure Strike where
  world : World
  hits : HitCache
  removed : Bool := false

/-- One `Player.PickTile` on a container cell. -/
def hit (rules : WorldRules) (w : World) (cache : HitCache) (cell : Cell) (power : Nat) : Strike :=
  let grid := Grid.ofTerrain w.terrain collision
  let tile := grid.tileAt cell.x cell.y
  if !(tile.id.any isContainerTile) then { world := w, hits := cache } else
  let damage := if canKill w.chests tile cell then OriginalMining.pickDamage grid rules cell tile power else 0
  -- AddDamage returns before drawing when the damage is 0 on a fresh cache slot
  if damage == 0 && (cache.entries.find? (·.cell == cell)).isNone then
    { world := w.prefix cell.x cell.y true, hits := cache } else
  let (_, random) := w.random.nextInt
  let w := { w with random, draws := w.draws + 1 }
  let cache := cache.record cell damage
  if cache.damage cell < 100 then { world := w.prefix cell.x cell.y true, hits := if damage == 0 then cache else cache.prune grid }
  else
    let cache := (cache.forget cell).prune grid
    let w := w.killContainerCell cell.x cell.y
    let w := squareFrame w cell.x cell.y
    { world := w, hits := cache.prune (Grid.ofTerrain w.terrain collision), removed := !OriginalTileHitRuntime.blocked w.effects }

/-- Copper Pickaxe item phase with containers routed here and everything else to
`OriginalBankMining.advance` (ground blocks, personal storages, work benches). -/
def advance (pick : OriginalMining.CopperPick) (body : Body) (rules : WorldRules) (w : World)
    (held : Bool) (target : Option Cell) (addDamageDraw : Bool := true) :
    OriginalMining.CopperPick × World := Id.run do
  let routed := target.any fun cell =>
    ((w.terrain.tile? cell.x cell.y).bind (·.id)).any isContainerTile
  if !routed then
    let bw : OriginalBankMining.World := {
      terrain := w.terrain, items := w.items, random := w.random
      effects := w.effects,frameNumbers := w.frameNumbers }
    let (pick, bw) := OriginalBankMining.advance pick body rules bw held target addDamageDraw
    return (pick, { w with terrain := bw.terrain, items := bw.items, random := bw.random, effects := bw.effects,
                           frameNumbers := bw.frameNumbers,
                           draws := w.draws + bw.draws, drops := w.drops ++ bw.drops })
  let timing := pick.toTiming.advance held 23
  let state := { pick with toTiming := timing }
  let some cell := target | return (state, w)
  if !(held && timing.itemAnimation > 0 && timing.itemTime == 0 && inToolRange body cell 5 3 (-1)) then
    return (state, w)
  let strike := hit rules w pick.hits cell 35
  return ({ state with toTiming := timing.setTime 15, hits := strike.hits }, strike.world)

/-! ## Placement -/

/-- The anchor test measured for chests (`chest_place_on_*`): the forge-style solid anchor of
`OriginalPlacement` without tables. -/
def anchors (tile : Tile) : Bool := Id.run do
  let some id := tile.id | return false
  if tile.actuated then return false
  let rules := collision id
  if rules.platform then
    let frame := ((tile.frame.getD (0, 0)).1).tdiv 18
    return tile.shape != .halfBlock &&
      ((0 ≤ frame && frame ≤ 7) || (12 ≤ frame && frame ≤ 16) || frame == 25 || frame == 26)
  if rules.solid && rules.solidTop then return true
  if rules.solid then
    if tile.shape == .full && !OriginalTileRules.preventsSolidAnchor id then return true
    if tile.shape == .slope 3 || tile.shape == .slope 4 then return true
  return false

structure Placed where
  terrain : Terrain
  chests : Array Chest
  timing : Timing
  stack : Option Stack
  /-- The runtime slot the new container took. -/
  index : Option Nat := none

/-- Place a chest or dresser with a dry, unbuffed single-player character (`PlaceThing` for a
container item). The mouse cell is the bottom-left (chest) or bottom-middle (dresser) cell. -/
def place (terrain : Terrain) (chests : Array Chest) (body : Body) (timing : Timing) (stack : Stack)
    (held : Bool) (target : Option Cell) : Placed := Id.run do
  let timing := timing.advance held 15 (stack.count != 0)
  let remaining := if stack.count == 0 && timing.itemAnimation == 0 then none else some stack
  let idle : Placed := { terrain, chests, timing, stack := remaining }
  let some (id, style) := placeFor stack.item | return idle
  let some cell := target | return idle
  if !held || stack.count == 0 || timing.itemAnimation == 0 || timing.itemTime != 0 then return idle
  let grid := Grid.ofTerrain terrain collision
  if cell.x == 0 || cell.y == 0 || cell.x + 1 ≥ grid.width || cell.y + 1 ≥ grid.height ||
      !inToolRange body cell 5 3 0 then return idle
  let (width, originX) : Nat × Nat := if id == 88 then (3, 1) else (2, 0)
  if cell.x < originX || cell.y < 1 then return idle
  let left := cell.x - originX
  let top := cell.y - 1
  for x in [left:left + width] do
    if !anchors (grid.tileAt x (cell.y + 1)) then return idle
    for y in [top:top + 2] do
      let t := grid.tileAt x y
      if t.liquid.any (·.kind == .lava) then return idle
      if t.id.any (fun i => !OriginalTileRules.breakableInObjectPlacement i) then
        return idle
  let some index := firstFree chests left top | return idle
  let mut updated := terrain
  for dx in [:width] do
    for dy in [:2] do
      let frame := some ((style * 18 * width : Nat) + 18 * (dx : Int), 18 * (dy : Int))
      let some t := updated.tile? (left + dx) (top + dy) | return idle
      updated := (updated.replaceTile (left + dx) (top + dy)
        { OriginalMining.clearGround t with id := some id, frame }).getD updated
  let stack := { stack with count := stack.count - 1 }
  let timing := timing.setTime 10
  let timing := if stack.count == 0 then { timing with itemTime := timing.itemAnimation } else timing
  return { terrain := updated, chests := create chests index left top, timing, stack := some stack,
           index := some index }

end Lean4Terria.OriginalChestMining
