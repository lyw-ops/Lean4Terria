import Lean4Terria.OriginalCandlePlacement
import Lean4Terria.OriginalTileHitRuntime
import Lean4Terria.OriginalFraming

/-!
# Pickaxe and support removal of personal storage furniture (Terraria 1.4.5.8 Classic)

Hand translation of the vanilla path that removes a Piggy Bank (tile 29), Safe (97),
Defender's Forge (463) or Void Vault (491), and the Wooden Work Bench (18) a Piggy Bank stands
on:

* `Player.PickTile` (IL_0046 `HitTile.AddDamage(slot, damage, true)`, kill at ≥ 100,
  `ClearMiningCacheAt`, `KillTile(x, y, fail)`, `HitTile.Prune`)
* `HitTileObject.AddDamage` draws one `Main.rand.NextFloat()` per damaging hit (crack animation)
* `WorldGen.KillTile` → `SquareTileFrame(x, y)` → `TileFrame` of the eight neighbours in column order
  (x−1, x, x+1; each from y−1 to y+1) → `TileFrameImportant` → `Check2x1` (18, 29), `Check2x2` (97),
  `Check3x3` (491), `Check3x4` (463)
* each check: frame/type integrity of every cell, the support test of its family, then with
  `destroyObject` set `KillTile` on every cell, one `Item.NewItem` with the 32 × 32 rectangle at the
  cell that triggered the check (`Check2x1` first moves x to its left cell), and a re-frame pass
  (`Check2x1`: `SquareTileFrame` on both cells; the others: `TileFrame` over the object grown by one)

The contents of the four personal storages live in the character (`Player.bank` … `bank4`), so
removing the furniture never touches `OriginalBanks.Stores`.

Random stream: in the native host every frame draws 3 `UpdatePlayers` samples before `ItemCheck`.
This module draws only what the hit itself does: 1 for `AddDamage` and 2 per drop
(`Item.NewItem` velocity). On the desktop client `KillTile_PlaySounds` additionally plays the dig
sound through `LegacySoundPlayer.PlaySound`, which draws `Main.rand.Next(3)` and
`Next(-10, 11)` whenever the sound is audible, and on-screen dust draws too. Both depend on volume
and camera, so they are left to the caller.

Only style 0 of the Work Bench and the single styles of the four storages are mapped to items;
other styles are destroyed without a drop.

The Water, Peace and Shadow Candles (49, 372, 646) are framed here too: `TileFrame` runs
`WorldGen.CheckOnTable1x1` on them, which calls `KillTile(x, y)` when the cell below no longer holds
them. That check does not read `destroyObject`, so a candle on a Work Bench falls inside the
bench's own cell kills, before the bench item drops. A pickaxe breaks a candle in three hits like
the furniture above; its `KillTile` drops the item from the cell's 16 × 16 rectangle.
-/

namespace Lean4Terria.OriginalBankMining

open OriginalCollision (Grid Vector single)
open OriginalItems (Stack WorldItems)
open OriginalMining (Cell HitCache WorldRules)
open OriginalWorld (Terrain Tile TileShape)
open OriginalTileRules (collision isTable)

inductive Shape where
  | twoByOne | twoByTwo | threeByThree | threeByFour | onTable
  deriving BEq, Repr

/-- The `TileFrameImportant` dispatch for the tiles this module owns. -/
def shapeOf : UInt16 → Option Shape
  | 18 | 29 => some .twoByOne
  | 97 => some .twoByTwo
  | 491 => some .threeByThree
  | 463 => some .threeByFour
  | 49 | 372 | 646 => some .onTable
  | _ => none

/-- The item each check drops for the styles covered here. -/
def dropFor (id : UInt16) (style : Int) : Option UInt16 :=
  if OriginalCandlePlacement.isCandleTile id then OriginalCandlePlacement.dropForTile id
  else if id == 18 then (if style == 0 then some 36 else none)
  else if style != 0 then none
  else (OriginalBanks.kindForTile id).map OriginalBanks.Kind.itemId

def owns (id : UInt16) : Bool := (shapeOf id).isSome

/-- One tile-break world: terrain, world items, the random stream and `WorldGen.destroyObject`. -/
structure World where
  terrain : Terrain
  items : WorldItems
  random : UnifiedRandom
  destroying : Bool := false
  effects : Option OriginalTileHitRuntime.State := none
  /-- Runtime variants retained when TileFrameCosmetic resets an ordinary tile. -/
  frameNumbers : Std.HashMap (Nat × Nat) Nat := {}
  /-- Each drop as (item, rectangle origin cell), in spawn order. -/
  drops : Array (UInt16 × Cell) := #[]
  /-- Samples drawn by this module. -/
  draws : Nat := 0

private def World.tile (w : World) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (w.terrain.tile? x.toNat y.toNat).getD default

private def World.inWorld (w : World) (x y : Int) : Bool :=
  x ≥ 0 && y ≥ 0 && x < w.terrain.header.width && y < w.terrain.header.height

private def active (tile : Tile) : Bool := tile.id.isSome
private def halfBrick (tile : Tile) : Bool := tile.shape == .halfBlock
private def topSlope (tile : Tile) : Bool := tile.shape == .slope 1 || tile.shape == .slope 2
private def frameX (tile : Tile) : Int := (tile.frame.map (·.1)).getD 0
private def frameY (tile : Tile) : Int := (tile.frame.map (·.2)).getD 0
private def isType (tile : Tile) (id : UInt16) : Bool := tile.id == some id
private def solid (tile : Tile) : Bool := tile.id.any fun id => (collision id).solid
private def solidTop (tile : Tile) : Bool := tile.id.any fun id => (collision id).solidTop
private def table (tile : Tile) : Bool := tile.id.any isTable

/-- `WorldGen.PlatformProperTopFrame` with `TileObjectData.PlatformFrameWidth() = 18`. -/
private def platformProperTopFrame (fx : Int) : Bool :=
  let v := fx.tdiv 18
  (0 ≤ v && v ≤ 7) || (12 ≤ v && v ≤ 16) || (25 ≤ v && v ≤ 26)

/-- `WorldGen.SolidTileAllowBottomSlope`; outside the world counts as support. -/
def solidTileAllowBottomSlope (w : World) (x y : Int) : Bool :=
  if !w.inWorld x y then true else
  let t := w.tile x y
  active t && (solid t || solidTop t) &&
    (!topSlope t || (t.id.any (fun id => (collision id).platform) && platformProperTopFrame (frameX t))) &&
    !halfBrick t && !t.actuated

private def World.prefix (w : World) (x y : Int) (fail : Bool) : World :=
  let (effects,random) := OriginalTileHitRuntime.emit w.effects (w.tile x y) x y fail w.random
  {w with effects,random}

private def World.clear (w : World) (x y : Int) : World :=
  if OriginalTileHitRuntime.blocked w.effects || !w.inWorld x y then w else
  match w.terrain.replaceTile x.toNat y.toNat (OriginalMining.clearGround (w.tile x y)) with
  | some terrain => { w with terrain }
  | none => w

/-- `Item.NewItem(x·16, y·16, 32, 32, item)`: a 16 × 16 world item centred in the rectangle,
velocity from `Main.rand.Next(-30, 31) · 0.1f` and `Next(-40, -15) · 0.1f`. -/
private def World.drop (w : World) (item : UInt16) (x y : Int) : World :=
  if OriginalTileHitRuntime.blocked w.effects then w else
  let position : Vector := ⟨single (x * 16 + 8), single (y * 16 + 8)⟩
  let (items, random) := OriginalItems.spawnStackAt w.items position (.ofItem item) w.random
    (some (Grid.ofTerrain w.terrain collision))
  { w with items, random, draws := w.draws + 2,
           drops := w.drops.push (item, ⟨x.toNat, y.toNat⟩) }

mutual

/-- `WorldGen.TileFrame` for the tiles owned here; other tiles keep their frames. -/
partial def tileFrame (w : World) (x y : Int) : World :=
  if OriginalTileHitRuntime.blocked w.effects then w else
  -- TileFrame ignores the five outermost rows and columns
  if x ≤ 5 || y ≤ 5 || x ≥ w.terrain.header.width - 5 || y ≥ w.terrain.header.height - 5 then w else
  let t := w.tile x y
  match t.id.bind fun id => (shapeOf id).map (id, ·) with
  | some (id, .twoByOne) => check2x1 w x y id
  | some (id, .twoByTwo) => check2x2 w x y id
  | some (id, .threeByThree) => check3x3 w x y id
  | some (id, .threeByFour) => check3x4 w x y id
  | some (id, .onTable) =>
    if OriginalCandlePlacement.staysOnSupport (Grid.ofTerrain w.terrain collision) ⟨x.toNat, y.toNat⟩ then w
    else killOnTable w x y id
  | none => Id.run do
    if t.id != some 147 && t.id != some 161 then return w
    let key := (x.toNat,y.toNat)
    -- Same-material/air atlas; snow/ice merge-specific frames remain a separate
    -- framing extension. A stored runtime variant takes precedence over an old
    -- sprite coordinate when the neighbours have just changed.
    let number := (w.frameNumbers.get? key).orElse fun _ =>
      (Array.range 3).find? fun n =>
        (OriginalFraming.terrainFrame w.terrain x.toNat y.toNat t (some n)).map
          (fun (fx,fy) => ((fx : Int),(fy : Int))) == t.frame
    let some (fx,fy) := OriginalFraming.terrainFrame w.terrain x.toNat y.toNat t number | return w
    let terrain := (w.terrain.replaceTile x.toNat y.toNat
      {t with frame := some (fx,fy)}).getD w.terrain
    return {w with terrain}

/-- `KillTile(x, y)` of a candle, from `CheckOnTable1x1` or from the third hit of
`Player.PickTile`: `KillTile_DropItems` spawns the item from the cell's 16 × 16 rectangle (two
velocity samples), the cell goes inactive and `SquareTileFrame(x, y)` follows. -/
partial def killOnTable (w : World) (x y : Int) (id : UInt16) : World :=
  let w := w.prefix x y false
  if OriginalTileHitRuntime.blocked w.effects then w else
  let cell : Cell := ⟨x.toNat, y.toNat⟩
  let w := match dropFor id 0 with
    | none => w
    | some item =>
      let (items, random) := OriginalItems.spawnStack w.items cell (.ofItem item) w.random
        (some (Grid.ofTerrain w.terrain collision))
      { w with items, random, draws := w.draws + 2, drops := w.drops.push (item, cell) }
  squareFrame (w.clear x y) x y (resetFrame := true)

/-- `KillTile` on one cell of an object while `destroyObject` is set: `KillTile_DropItems` yields
nothing for these tiles, the cell goes inactive and `SquareTileFrame` runs. The object checks of the
neighbours return at once; `CheckOnTable1x1` does not, so a candle on the cell falls here. -/
partial def killCell (w : World) (x y : Int) (id : UInt16) : World :=
  if isType (w.tile x y) id then
    let w := w.prefix x y false
    squareFrame (w.clear x y) x y (resetFrame := true)
  else w

/-- `WorldGen.SquareTileFrame(x, y, resetFrame)`: column x−1, then x, then x+1, each from y−1 to
y+1. Only the centre gets `resetFrame`; on an active tile that is not frame-important
`TileFrameCosmetic` then re-rolls its variant with one `genRand` (= `Main.rand`) sample. Frame-
important tiles are the ones that carry a stored frame (`Tile.frame`). The variant itself is
left to the terrain framing code. -/
partial def squareFrame (w : World) (x y : Int) (resetFrame : Bool := false) : World := Id.run do
  let mut w := w
  for dx in [0:3] do
    for dy in [0:3] do
      if OriginalTileHitRuntime.blocked w.effects then return w
      let cx := x - 1 + dx
      let cy := y - 1 + dy
      let t := w.tile cx cy
      if resetFrame && dx == 1 && dy == 1 && active t &&
          (t.frame.isNone || t.id == some 147 || t.id == some 161) && cx > 5 && cy > 5 &&
          cx < w.terrain.header.width - 5 && cy < w.terrain.header.height - 5 then
        let (number, random) := w.random.next 0 3
        w := { w with
          random
          draws := w.draws + 1
          frameNumbers := w.frameNumbers.insert (cx.toNat,cy.toNat) number.toNat }
      w := tileFrame w cx cy
  return w

/-- `WorldGen.Check2x1` (Work Bench 18, Piggy Bank 29). -/
partial def check2x1 (w : World) (i j : Int) (id : UInt16) : World := Id.run do
  if w.destroying then return w
  let here := w.tile i j
  let column := (frameX here).tdiv 18
  let style := column / 2
  let x := i - column.tmod 2
  let left := w.tile x j
  let right := w.tile (x + 1) j
  let intact := frameX right == frameX left + 18 && isType left id && isType right id &&
    active left && active right
  let mut broken := !intact
  if id == 29 then
    for c in [x, x + 1] do
      let below := w.tile c (j + 1)
      if !(active below && table below && !halfBrick below && !topSlope below) then broken := true
  else
    if !solidTileAllowBottomSlope w x (j + 1) then broken := true
    if !solidTileAllowBottomSlope w (x + 1) (j + 1) then broken := true
  if !broken then return w
  let mut w := { w with destroying := true }
  w := killCell w x j id
  w := killCell w (x + 1) j id
  if let some item := dropFor id style then w := w.drop item x j
  w := { w with destroying := false }
  w := squareFrame w x j (resetFrame := true)
  return squareFrame w (x + 1) j (resetFrame := true)

/-- `WorldGen.Check2x2`, standing branch (Safe 97). -/
partial def check2x2 (w : World) (i j : Int) (id : UInt16) : World := Id.run do
  if w.destroying then return w
  let here := w.tile i j
  let mut left := -((frameX here).tdiv 18)
  let mut styleX := 0
  if left < -1 then
    left := left + 2
    styleX := 36
  left := left + i
  let mut fy := frameY here
  let mut row := 0
  while fy ≥ 36 do
    fy := fy - 36
    row := row + 1
  let top := j - fy.tdiv 18
  let mut broken := false
  for dx in [0:2] do
    let x := left + dx
    for dy in [0:2] do
      let y := top + dy
      let t := w.tile x y
      if !(active t && isType t id && frameX t == (x - left) * 18 + styleX &&
          frameY t == (y - top) * 18 + row * 36) then
        broken := true
        break
    let below := w.tile x (top + 2)
    if !active below || (!solid below && !table below) then broken := true
    if halfBrick below then broken := true
  if !broken then return w
  let mut w := { w with destroying := true }
  for dx in [0:2] do
    for dy in [0:2] do
      let t := w.tile (left + dx) (top + dy)
      if active t && isType t id then w := killCell w (left + dx) (top + dy) id
  if let some item := dropFor id (styleX / 36 + row * 2) then w := w.drop item i j
  w := { w with destroying := false }
  for dx in [0:4] do
    for dy in [0:4] do
      w := tileFrame w (left - 1 + dx) (top - 1 + dy)
  return w

/-- `WorldGen.Check3x3`, standing branch (Void Vault 491). -/
partial def check3x3 (w : World) (i j : Int) (id : UInt16) : World := Id.run do
  if w.destroying then return w
  let here := w.tile i j
  let mut column := (frameX here).tdiv 18
  let mut style := 0
  while column ≥ 3 do
    column := column - 3
    style := style + 1
  let frameLeft := i - column
  let styleX := 54 * style
  let styleY := (frameY here).tdiv 54
  let rowInStyle := ((frameY here).tmod 54).tdiv 18
  let left := i - column
  let top := j - rowInStyle
  let mut broken := false
  for dx in [0:3] do
    for dy in [0:3] do
      let x := left + dx
      let y := top + dy
      let t := w.tile x y
      if !(active t && isType t id && frameX t == (x - frameLeft) * 18 + styleX &&
          frameY t == (y - top) * 18 + styleY * 54) then
        broken := true
  for dx in [0:3] do
    if !solidTileAllowBottomSlope w (left + dx) (top + 3) then broken := true
  if !broken then return w
  let mut w := { w with destroying := true }
  for dx in [0:3] do
    for dy in [0:3] do
      w := killCell w (left + dx) (top + dy) id
  if let some item := dropFor id (style + styleY) then w := w.drop item i j
  w := { w with destroying := false }
  for dx in [0:5] do
    for dy in [0:5] do
      w := tileFrame w (left - 1 + dx) (top - 1 + dy)
  return w

/-- `WorldGen.Check3x4` (Defender's Forge 463). -/
partial def check3x4 (w : World) (i j : Int) (id : UInt16) : World := Id.run do
  if w.destroying then return w
  let here := w.tile i j
  let mut column := (frameX here).tdiv 18
  let mut row := (frameY here).tdiv 18
  let mut styleX := 0
  let mut styleY := 0
  while column ≥ 3 do
    styleX := styleX + 1
    column := column - 3
  while row ≥ 4 do
    styleY := styleY + 1
    row := row - 4
  let left := i - column
  let top := j - row
  let mut broken := false
  for dx in [0:3] do
    let x := left + dx
    for dy in [0:4] do
      let y := top + dy
      let t := w.tile x y
      if !(active t && isType t id && frameX t == styleX * 54 + (x - left) * 18 &&
          frameY t == styleY * 72 + (y - top) * 18) then
        broken := true
    if !solidTileAllowBottomSlope w x (top + 4) then broken := true
  if !broken then return w
  let mut w := { w with destroying := true }
  for dx in [0:3] do
    for dy in [0:4] do
      w := killCell w (left + dx) (top + dy) id
  if let some item := dropFor id (styleX + styleY) then w := w.drop item i j
  w := { w with destroying := false }
  for dx in [0:5] do
    for dy in [0:5] do
      w := tileFrame w (left - 1 + dx) (top - 1 + dy)
  return w

end

/-- The framing that follows `KillTile` of any cell (a mined ground block under an object, or one
cell of an object): `SquareTileFrame(x, y)` with the object checks above. Call it after the cell
has been cleared and after that cell's own drop was spawned, as `KillTile` does. -/
def afterKill (w : World) (cell : Cell) : World :=
  squareFrame w cell.x cell.y (resetFrame := true)

structure Strike where
  world : World
  hits : HitCache
  /-- Whether the hit removed the target cell. -/
  removed : Bool := false

/-- One damaging pickaxe hit on a cell of an owned object (`Player.PickTile`). `power` is the
pickaxe power (35 for the Copper Pickaxe); the furniture takes `pickDamage`, so three hits.
The damage accumulates per cell in the hit cache, as for any other tile. -/
def hit (rules : WorldRules) (w : World) (cache : HitCache) (cell : Cell) (power : Nat) : Strike :=
  let grid := Grid.ofTerrain w.terrain collision
  let tile := grid.tileAt cell.x cell.y
  match tile.id.filter owns with
  | none => { world := w, hits := cache }
  | some id =>
  let damage := OriginalMining.pickDamage grid rules cell tile power
  if damage == 0 && cache.damage cell == 0 then
    { world := w.prefix cell.x cell.y true, hits := cache } else
  -- HitTileObject.AddDamage: Main.rand.NextFloat() for the crack animation
  let (_, random) := w.random.nextInt
  let w := { w with random, draws := w.draws + 1 }
  let cache := cache.record cell damage
  if cache.damage cell < 100 then
    { world := w.prefix cell.x cell.y true, hits := if damage == 0 then cache else cache.prune grid }
  else
    let cache := (cache.forget cell).prune grid
    let w := if shapeOf id == some .onTable then killOnTable w cell.x cell.y id
      else afterKill ((w.prefix cell.x cell.y false).clear cell.x cell.y) cell
    { world := w, hits := cache.prune (Grid.ofTerrain w.terrain collision), removed := !OriginalTileHitRuntime.blocked w.effects }

/-- Copper Pickaxe item phase that routes owned objects here and everything else to
`OriginalMining.hitGround`. A ground block removed by that path is followed by `afterKill`, so an
object standing on it is re-checked after the block's own drop, in vanilla order. `addDamageDraw`
also draws the `AddDamage` sample for ground hits, which `OriginalMining` does not model. -/
def advance (pick : OriginalMining.CopperPick) (body : OriginalPlayerCollision.Body)
    (rules : WorldRules) (w : World) (held : Bool) (target : Option Cell)
    (addDamageDraw : Bool := true) : OriginalMining.CopperPick × World := Id.run do
  let timing := pick.toTiming.advance held 23
  let state := { pick with toTiming := timing }
  let some cell := target | return (state, w)
  let grid := Grid.ofTerrain w.terrain collision
  let tile := grid.tileAt cell.x cell.y
  if !(held && timing.itemAnimation > 0 && timing.itemTime == 0 &&
      OriginalMining.inToolRange body cell 5 3 (-1)) || tile.id.isNone then
    return (state, w)
  let state := { state with toTiming := timing.setTime 15 }
  if tile.id.any owns then
    let strike := hit rules w pick.hits cell 35
    return ({ state with hits := strike.hits }, strike.world)
  -- PickTile's type504 guard is the only active-target early return before AddDamage.
  if tile.id == some 504 then return (state,w)
  let ground := OriginalMining.hitGround grid rules pick.hits cell 35
  -- PickTile always calls AddDamage; it draws unless the damage is 0 on a fresh cache slot,
  -- which is exactly when hitGround leaves the cache unchanged
  let w := if addDamageDraw && ground.cache != pick.hits then
      let (_, random) := w.random.nextInt
      { w with random, draws := w.draws + 1 }
    else w
  -- The tool live/range/active gate above is the actual PickTile event. Cache
  -- equality only gates AddDamage's sample, never the failed KillTile prefix.
  let fail := ground.changed.all (·.id.isSome)
  let w := w.prefix cell.x cell.y fail
  if OriginalTileHitRuntime.blocked w.effects then return ({state with hits := ground.cache},w)
  let terrain := ground.apply w.terrain cell
  let w := { w with terrain }
  let w := if fail && (tile.id == some 147 || tile.id == some 161) then
      squareFrame w cell.x cell.y (resetFrame := true)
    else w
  match ground.drop with
  | none => return ({ state with hits := ground.cache }, w)
  | some item =>
    let (items, random) := OriginalItems.spawnStack w.items cell (.ofItem item) w.random (some grid)
    let w := { w with items, random, draws := w.draws + 2 }
    return ({ state with hits := ground.cache }, afterKill w cell)

end Lean4Terria.OriginalBankMining
