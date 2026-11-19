import Std.Data.HashMap
import Lean4Terria.OriginalWorld
import Lean4Terria.OriginalTileRules
import Lean4Terria.UnifiedRandom

/-!
# Terraria 1.4.5.8 liquids on the real world map

Hand port of `Liquid.UpdateLiquid`, `Liquid.Update`, `Liquid.AddWater`, `Liquid.DelWater`,
`LiquidBuffer.AddBuffer` / `DelBuffer`, `Liquid.LiquidCheck` (+ `GetLiquidMergeTypes`,
`CreateLiquidMergeTile`, `LiquidOverwriteStrip`), the liquid side of `WorldGen.TileFrame` /
`SquareTileFrame`, and `WorldGen.CheckLilyPad`, from the IL of `TerrariaServer.exe` 1.4.5.8
(listings in `.lake/original-liquid/native/il/`). Branch order, byte arithmetic, queue slot moves and
random draws follow the IL.

Storage: the authoritative map is the run-length `OriginalWorld.Terrain`. A sparse overlay keeps every
cell the liquid code has written (and the per-tile `skipLiquid` / `checkingLiquid` flags, which the
`.wld` does not store). `takeChanges` writes the touched cells back into the runs, so no step walks or
copies the whole 4200 × 1200 map.
-/

namespace Lean4Terria.OriginalLiquid

open OriginalWorld (Terrain TileShape LiquidKind)

def kindCode : LiquidKind → Nat
  | .water => 0 | .lava => 1 | .honey => 2 | .shimmer => 3

def kindOf : Nat → LiquidKind
  | 1 => .lava | 2 => .honey | 3 => .shimmer | _ => .water

/-- One tile as the liquid code sees it. `kind` is the two liquid type bits of `Tile.bTileHeader`;
they survive while `liquid` is 0, exactly as in the game. -/
structure Cell where
  id : Option UInt16 := none
  shape : TileShape := .full
  actuated : Bool := false
  frame : Option (Int × Int) := none
  /-- background wall (read only: liquids never change it; `takeChanges` keeps the map's wall) -/
  wall : UInt16 := 0
  liquid : Nat := 0
  kind : Nat := 0
  skip : Bool := false
  checking : Bool := false
  deriving BEq, Inhabited

namespace Cell
def ofTile (t : OriginalWorld.Tile) : Cell :=
  { id := t.id, shape := t.shape, actuated := t.actuated, frame := t.frame, wall := t.wall,
    liquid := (t.liquid.map (·.amount.toNat)).getD 0, kind := (t.liquid.map (kindCode ·.kind)).getD 0 }

/-- Write the liquid-relevant fields back into a map tile; all other fields (wall, paint, wires)
are kept. A cell without liquid stores no liquid record, as the `.wld` does. -/
def applyTo (c : Cell) (t : OriginalWorld.Tile) : OriginalWorld.Tile :=
  { t with id := c.id, shape := c.shape, actuated := c.actuated, frame := c.frame,
           liquid := if c.liquid > 0 then some ⟨kindOf c.kind, c.liquid.toUInt8⟩ else none }

def active (c : Cell) : Bool := c.id.isSome
def type (c : Cell) : Nat := (c.id.map (·.toNat)).getD 0
def nactive (c : Cell) : Bool := c.active && !c.actuated
def halfBrick (c : Cell) : Bool := c.shape == .halfBlock
def slope (c : Cell) : Nat := match c.shape with | .slope s => s.toNat | _ => 0
def lava (c : Cell) : Bool := c.kind == 1
def honey (c : Cell) : Bool := c.kind == 2
def shimmer (c : Cell) : Bool := c.kind == 3
def water (c : Cell) : Bool := c.kind == 0
def anyKind (c : Cell) (k : Nat) : Bool := c.liquid > 0 && c.kind == k
end Cell

/-- `Main.tileSolid[type]` under `Liquid.tilesIgnoreWater(ignore)`: 546 (grate) and the boulder-like ids
`WorldGen.SetBoulderSolidity` touches are not solid while `ignore` holds. -/
def solid (ignore : Bool) (type : Nat) : Bool :=
  if ignore && [546, 138, 484, 664, 711, 712, 713, 714, 715, 716].contains type then false
  else (OriginalTileRules.collision type.toUInt16).solid

def solidTop (type : Nat) : Bool := (OriginalTileRules.collision type.toUInt16).solidTop

/-- `nactive && tileSolid && !tileSolidTop`, the test the liquid code repeats everywhere. -/
def Cell.blocks (ignore : Bool) (c : Cell) : Bool := c.nactive && solid ignore c.type && !solidTop c.type

/-- `Main.liquid[i]` -/
structure Entry where
  x : Int
  y : Int
  kill : Int := 0
  delay : Int := 0
  deriving BEq, Inhabited, Repr

/-- An item `Item.NewItem` created at tile `(x, y)` during a liquid step. The two velocity draws were
taken from the liquid stream at the vanilla moment; `drawX`, `drawY` are their values
(`velocity = (drawX · 0.1, drawY · 0.1)`, as `OriginalItems.spawnStackAt`). -/
structure PendingDrop where
  x : Int
  y : Int
  item : Nat
  stack : Nat
  drawX : Int
  drawY : Int
  wet : Bool := false
  deriving BEq, Repr

/-- A critter `NPC.NewNPC` created by `KillTile_DropBait` at tile `(x, y)` (spawn point
`(16x + 10, 16y)`), with its two velocity draws: `velocity = (drawX · 0.1 · −direction, drawY · 0.1)`,
`direction` from `NPC.TargetClosest`. -/
structure PendingSpawn where
  x : Int
  y : Int
  npc : Nat
  drawX : Int
  drawY : Int
  deriving BEq, Repr

structure State where
  terrain : Terrain
  overlay : Std.HashMap Nat Cell := {}
  /-- cells written since the last `takeChanges`, with their value before the first write -/
  touched : Std.HashMap Nat Cell := {}
  liquid : Array Entry := #[]
  numLiquid : Nat := 0
  buffer : Array (Int × Int) := #[]
  numBuffer : Nat := 0
  maxLiquid : Int := 25000
  maxLiquidBuffer : Int := 50000
  wetCounter : Int := 0
  cycles : Int := 10
  curMaxLiquid : Int := 0
  quickSettle : Bool := false
  quickFall : Bool := false
  stuck : Bool := false
  stuckCount : Int := 0
  stuckAmount : Int := 0
  panicMode : Bool := false
  panicCounter : Int := 0
  /-- `Main.rand` at the moment (the `UpdateLiquid` stream in play, the loader's stream during load);
  `WorldGen.genRand` returns it. -/
  rng : UnifiedRandom
  draws : Nat := 0
  /-- `Liquid.tilesIgnoreWater` mode -/
  ignore : Bool := false
  /-- `WorldGen.isGeneratingOrLoadingWorld` -/
  loading : Bool := false
  /-- items created by liquid kills since the last `takeDrops` -/
  drops : Array PendingDrop := #[]
  /-- critters liquid kills spawned since the last `takeDrops` -/
  spawns : Array PendingSpawn := #[]
  /-- branches reached that are not ported (reported, not approximated) -/
  unsupported : Array String := #[]

abbrev M := StateM State

namespace State
def width (s : State) : Nat := s.terrain.header.width
def height (s : State) : Nat := s.terrain.header.height
def maxTilesX (s : State) : Int := s.width
def maxTilesY (s : State) : Int := s.height
def underworldLayer (s : State) : Int := s.maxTilesY - 200

def key? (s : State) (x y : Int) : Option Nat :=
  if x < 0 || y < 0 || x ≥ s.width || y ≥ s.height then none
  else some (x.toNat * s.height + y.toNat)

def baseCell (s : State) (k : Nat) : Cell :=
  match s.terrain.tile? (k / s.height) (k % s.height) with
  | some t => Cell.ofTile t
  | none => {}

def cellAt (s : State) (k : Nat) : Cell := s.overlay.getD k (s.baseCell k)

def cell (s : State) (x y : Int) : Cell :=
  match s.key? x y with
  | some k => s.cellAt k
  | none => {}
end State

def getCell (x y : Int) : M Cell := return (← get).cell x y

def setCell (x y : Int) (c : Cell) : M Unit := modify fun s =>
  match s.key? x y with
  | none => s
  | some k =>
    let s := if s.touched.contains k then s else { s with touched := s.touched.insert k (s.cellAt k) }
    { s with overlay := s.overlay.insert k c }

def modCell (x y : Int) (f : Cell → Cell) : M Unit := do setCell x y (f (← getCell x y))

def setLiquid (x y : Int) (v : Nat) : M Unit := modCell x y fun c => { c with liquid := v % 256 }
def setKind (x y : Int) (k : Nat) : M Unit := modCell x y fun c => { c with kind := k }
def setSkip (x y : Int) (b : Bool) : M Unit := do
  let c ← getCell x y
  if c.skip != b then setCell x y { c with skip := b }
def setChecking (x y : Int) (b : Bool) : M Unit := do
  let c ← getCell x y
  if c.checking != b then setCell x y { c with checking := b }

def note (msg : String) : M Unit := modify fun s => { s with unsupported := s.unsupported.push msg }

def draw (bound : Nat) : M Nat := do
  let s ← get
  let (v, rng) := s.rng.next 0 bound
  set { s with rng, draws := s.draws + 1 }
  return v.toNat

def entry (i : Nat) : M Entry := return (← get).liquid[i]!
def modEntry (i : Nat) (f : Entry → Entry) : M Unit :=
  modify fun s => { s with liquid := s.liquid.modify i f }

/-- A cell changed since the previous `takeChanges`: map key, value before, value now. -/
structure Change where
  x : Nat
  y : Nat
  before : Cell
  after : Cell

/-- Write every touched cell back into the run-length map and return those whose value changed.
Overlay entries that the map now represents exactly (no queue flags, no stale liquid type bits)
are dropped, so the overlay only holds live liquid state. Cost: the touched cells only. -/
def takeChanges : M (Array Change) := do
  let s ← get
  let h := s.height
  let mut terrain := s.terrain
  let mut overlay := s.overlay
  let mut out : Array Change := #[]
  for (k, before) in s.touched.toArray do
    let x := k / h
    let y := k % h
    let c := overlay.getD k before
    if c != before then out := out.push { x, y, before, after := c }
    if let some base := terrain.tile? x y then
      let t := c.applyTo base
      if t != base then
        if let some next := terrain.replaceTile x y t then terrain := next
      if Cell.ofTile t == c then overlay := overlay.erase k
  set { s with terrain, overlay, touched := {} }
  return out.qsort fun a b => a.x < b.x || (a.x == b.x && a.y < b.y)

/-- `Math.Round((double)(float)(sum / d))`: for the integer sums `Liquid.Update` builds
(`d ∈ {2,3,4,5,7}`, `|sum| ≤ 1785`) this is exact round-half-to-even. -/
def roundDiv (sum : Int) (d : Nat) : Nat :=
  let q := sum / d
  let r := sum - q * d
  let q := if 2 * r > d then q + 1 else if 2 * r < d then q else if q % 2 == 0 then q else q + 1
  q.toNat

/-- `LiquidBuffer.AddBuffer(x, y)` -/
def addBuffer (x y : Int) : M Unit := do
  let s ← get
  if (s.numBuffer : Int) ≥ s.maxLiquidBuffer - 2 then return
  if (s.cell x y).checking then return
  setChecking x y true
  modify fun s =>
    let buf := if s.numBuffer < s.buffer.size then s.buffer.set! s.numBuffer (x, y) else s.buffer.push (x, y)
    { s with buffer := buf, numBuffer := s.numBuffer + 1 }

/-- `LiquidBuffer.DelBuffer(l)` -/
def delBuffer (l : Nat) : M Unit := modify fun s =>
  let n := s.numBuffer - 1
  { s with numBuffer := n, buffer := s.buffer.set! l s.buffer[n]! }

/-- Tile-death and kill behaviour; `OriginalLiquid.objectHooks` (Objects) fills it from the runtime
tables. The defaults kill nothing and report. -/
structure Hooks where
  /-- `CheckLavaDeath` / `CheckWaterDeath` for the tile; `none` = style rule not ported -/
  liquidKills : Cell → Option Bool := fun _ => some false
  /-- `WorldGen.KillTile(x, y)` for a liquid-killed tile; default reports the branch. -/
  killTile : Int → Int → M Unit := fun x y => do
    let c ← getCell x y
    note s!"KillTile({x},{y}) of type {c.type} (liquid death) not ported"
  /-- `UpdateLiquid` stream draws `PlaceTile` takes for one merge tile; with the `TileFrameCosmetic`
  roll of the following `SquareTileFrame(x, y, true)` that makes the two draws measured on the probe -/
  mergeDraws : Nat := 1
  /-- the rest of `WorldGen.TileFrame` for an active tile after its `AddWater` (`TileFrameImportant`
  checks, `CheckVines`, …); returns whether vanilla goes on to `TileFrameCosmetic`, whose
  `resetFrame` branch rolls `genRand.Next(0, 3)` for the frame number. Default: every tile does. -/
  frameTile : Int → Int → M Bool := fun _ _ => pure true

/-- `Liquid.AddWater(x, y)` -/
def addWater (hk : Hooks) (x y : Int) : M Unit := do
  let s ← get
  if (s.key? x y).isNone then return
  let t := s.cell x y
  if t.checking then return
  if x ≥ s.maxTilesX - 5 || y ≥ s.maxTilesY - 5 then return
  if x < 5 || y < 5 then return
  if t.liquid == 0 then return
  if t.nactive && solid s.ignore t.type && t.type != 546 && !solidTop t.type then return
  if (s.numLiquid : Int) ≥ s.curMaxLiquid - 1 then
    addBuffer x y
    return
  setCell x y { t with checking := true, skip := false }
  modify fun s =>
    let e : Entry := { x, y, kill := 0, delay := 0 }
    let arr := if s.numLiquid < s.liquid.size then s.liquid.set! s.numLiquid e else s.liquid.push e
    { s with liquid := arr, numLiquid := s.numLiquid + 1 }
  if !t.active || s.loading then return
  match hk.liquidKills t with
  | some true => hk.killTile x y
  | some false => pure ()
  | none => note s!"AddWater({x},{y}): liquid death of type {t.type} depends on an unported style rule"

def moveLilyPad (x y ny : Int) : M Unit := do
  let c ← getCell x y
  modCell x ny fun t => { t with id := some 518, shape := .full, frame := c.frame }
  setCell x y { c with id := none, frame := none, shape := .full }

mutual
/-- `WorldGen.TileFrame(i, j)` as far as liquids see it: bounds, `AddWater` for a tile holding
liquid, inactive tiles lose their half brick and slope, and the `TileFrameImportant` →
`CheckLilyPad` dispatch for type 518. Other framing is not modelled. -/
partial def tileFrame (hk : Hooks) (i j : Int) (resetFrame : Bool := false) : M Unit := do
  let s ← get
  if !(i > 5 && j > 5 && i < s.maxTilesX - 5 && j < s.maxTilesY - 5) then return
  let t := s.cell i j
  if !t.active && t.shape != .full then setCell i j { t with shape := .full }
  -- vanilla gates this on `WorldGen.generatingWorld` (false while a saved world loads)
  if t.liquid > 0 then addWater hk i j
  let t ← getCell i j
  if !t.active then return
  if t.type == 518 then
    checkLilyPad hk i j
    return
  let cosmetic ← hk.frameTile i j
  if cosmetic && resetFrame && (← getCell i j).active then
    let _ ← draw 3

/-- `WorldGen.SquareTileFrame(i, j, resetFrame)`: `TileFrame` on the 3 × 3 block, column by column;
only the centre gets `resetFrame`. -/
partial def squareTileFrame (hk : Hooks) (i j : Int) (resetFrame : Bool := false) : M Unit := do
  for dx in [(-1 : Int), 0, 1] do
    for dy in [(-1 : Int), 0, 1] do
      tileFrame hk (i + dx) (j + dy) (resetFrame && dx == 0 && dy == 0)

/-- `WorldGen.KillTile` of a lily pad: the cell is cleared and framed; the item drop takes no draw
from the `UpdateLiquid` stream (probe). -/
partial def killLilyPad (hk : Hooks) (x y : Int) : M Unit := do
  modCell x y fun t => { t with id := none, actuated := false, shape := .full, frame := none }
  squareTileFrame hk x y true

/-- `WorldGen.CheckLilyPad(x, y)` (single player) -/
partial def checkLilyPad (hk : Hooks) (x y : Int) : M Unit := do
  let s ← get
  if (s.cell x y).kind != 0 then
    killLilyPad hk x y
    return
  let mut y2 := y
  while true do
    let g := s.cell x y2
    if g.active && solid s.ignore g.type && !solidTop g.type then break
    if y2 < s.maxTilesY - 50 then y2 := y2 + 1 else break
  let ground := (s.cell x y2).type
  let frame : Int := if ground == 2 || ground == 477 then 0 else if ground == 109 || ground == 116 then 18
    else if ground == 60 then 36 else -1
  if frame < 0 then
    killLilyPad hk x y
    return
  let pad := s.cell x y
  let pad := { pad with frame := pad.frame.map fun (fx, _) => (fx, frame) }
  setCell x y pad
  let up := s.cell x (y - 1)
  if up.liquid > 0 && !up.active then
    moveLilyPad x y (y - 1)
    squareTileFrame hk x (y - 1)
    return
  if pad.liquid != 0 then return
  let below := s.cell x (y + 1)
  if !below.active then
    moveLilyPad x y (y + 1)
    squareTileFrame hk x (y + 1)
  else if !(OriginalTileRules.collision below.type.toUInt16).platform &&
      (!solid s.ignore below.type || solidTop below.type) then
    killLilyPad hk x y
end

/-- `Liquid.GetLiquidMergeTypes`: `(mergeTile, liquidMergeType)` -/
def mergeTypes (t : Nat) (water lava honey shimmer : Bool) : Nat × Nat := Id.run do
  let mut tile := 56
  let mut merge := t
  if t != 0 && water then
    tile := match t with | 1 => 56 | 2 => 229 | 3 => 659 | _ => tile
    merge := 0
  if t != 1 && lava then
    tile := match t with | 0 => 56 | 2 => 230 | 3 => 659 | _ => tile
    merge := 1
  if t != 2 && honey then
    tile := match t with | 0 => 229 | 1 => 230 | 3 => 659 | _ => tile
    merge := 2
  if t != 3 && shimmer then
    tile := match t with | 0 | 1 | 2 => 659 | _ => tile
    merge := 3
  return (tile, merge)

/-- `Liquid.LiquidOverwriteStrip(x, y, type)` -/
def overwriteStrip (x y : Int) (k : Nat) : M Unit := do
  let mut i := x
  while i ≥ 0 && (← getCell i y).liquid > 0 do
    setKind i y k
    i := i - 1
  i := x
  while i < (← get).maxTilesX && (← getCell i y).liquid > 0 do
    setKind i y k
    i := i + 1

/-- `Liquid.CreateLiquidMergeTile`. While loading it only retypes the liquid strip; in play it places
the block (`KillTile` is a no-op on an empty cell; `ReplaceTile` on an occupied one is not ported),
takes the measured draws, and runs `SquareTileFrame(x, y, true)`. -/
def createMergeTile (hk : Hooks) (x y : Int) (tileType : Nat) : M Unit := do
  if (← get).loading then
    let num : Int := if tileType == 229 then 0 else if tileType == 230 || tileType == 56 then 1
      else if tileType == 659 then 3 else -1
    let num := if y ≥ (← get).underworldLayer then 1 else num
    if num ≥ 0 then overwriteStrip x y num.toNat
    return
  let t ← getCell x y
  if t.active then
    note s!"CreateLiquidMergeTile({x},{y}): ReplaceTile on active type {t.type} not ported"
    return
  setCell x y { t with id := some tileType.toUInt16, actuated := false, shape := .full, frame := none }
  for _ in [:hk.mergeDraws] do
    let _ ← draw 3
  squareTileFrame hk x y true

/-- `Liquid.LiquidCheck(x, y, thisLiquidType)` -/
def liquidCheck (hk : Hooks) (x y : Int) (k : Nat) : M Unit := do
  let s ← get
  if !(x ≥ 1 && x < s.maxTilesX - 1 && y ≥ 1 && y < s.maxTilesY - 1) then return
  let c := s.cell x y
  if c.nactive && solid s.ignore c.type && !solidTop c.type && !c.halfBrick && c.slope == 0 then return
  let l := s.cell (x - 1) y
  let r := s.cell (x + 1) y
  let u := s.cell x (y - 1)
  let d := s.cell x (y + 1)
  if (l.liquid > 0 && l.kind != k) || (r.liquid > 0 && r.kind != k) || (u.liquid > 0 && u.kind != k) then
    let water := l.anyKind 0 || r.anyKind 0 || u.anyKind 0
    let lava := l.anyKind 1 || r.anyKind 1 || u.anyKind 1
    let honey := l.anyKind 2 || r.anyKind 2 || u.anyKind 2
    let shimmer := l.anyKind 3 || r.anyKind 3 || u.anyKind 3
    let mut sum := 0
    for (dx, dy) in [((-1 : Int), (0 : Int)), (1, 0), (0, -1)] do
      let n ← getCell (x + dx) (y + dy)
      if n.kind != k then
        sum := sum + n.liquid
        setLiquid (x + dx) (y + dy) 0
    let (tile, merge) := mergeTypes k water lava honey shimmer
    if sum < 24 || merge == k then return
    let c ← getCell x y
    if c.active then
      note s!"LiquidCheck({x},{y}): tileObsidianKill on active type {c.type} not ported"
      return
    setLiquid x y 0
    createMergeTile hk x y tile
    return
  if d.liquid > 0 && d.kind != k then
    if c.active then note s!"LiquidCheck({x},{y}): container test on active type {c.type} not ported"
    if k != 0 && d.active && OriginalTileRules.isCuttable d.type.toUInt16 then
      note s!"LiquidCheck({x},{y}): KillTile of cuttable type {d.type} below not ported"
    if !d.active then
      if c.liquid < 24 then
        setCell x y { c with liquid := 0, kind := 0 }
      else
        let (tile, _) := mergeTypes k d.water d.lava d.honey d.shimmer
        setLiquid x y 0
        setLiquid x (y + 1) 0
        createMergeTile hk x (y + 1) tile
    else
      note s!"LiquidCheck({x},{y}): tileObsidianKill below on active type {d.type} not ported"

def lavaCheck (hk : Hooks) (x y : Int) : M Unit := liquidCheck hk x y 1
def honeyCheck (hk : Hooks) (x y : Int) : M Unit := liquidCheck hk x y 2
def shimmerCheck (hk : Hooks) (x y : Int) : M Unit := liquidCheck hk x y 3

/-- `Liquid.Update()` for `Main.liquid[i]` -/
def update (hk : Hooks) (i : Nat) : M Unit := do
  let e ← entry i
  let x := e.x
  let y := e.y
  let s ← get
  let ig := s.ignore
  let c := s.cell x y
  if c.blocks ig then
    modEntry i fun e => { e with kill := 999 }
    return
  let original := c.liquid
  if y > s.underworldLayer && c.kind == 0 && c.liquid > 0 then
    setLiquid x y (c.liquid - min 2 c.liquid)
  if (← getCell x y).liquid == 0 then
    modEntry i fun e => { e with kill := 999 }
    return
  let neighbours : List (Int × Int) := [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  if (← getCell x y).lava then
    lavaCheck hk x y
    if !(← get).quickFall then
      if (← entry i).delay < 5 then
        modEntry i fun e => { e with delay := e.delay + 1 }
        return
      modEntry i fun e => { e with delay := 0 }
  else
    for (nx, ny) in neighbours do
      if (← getCell nx ny).lava then addWater hk nx ny
    if (← getCell x y).honey then
      honeyCheck hk x y
      if !(← get).quickFall then
        if (← entry i).delay < 10 then
          modEntry i fun e => { e with delay := e.delay + 1 }
          return
        modEntry i fun e => { e with delay := 0 }
    else
      for (nx, ny) in neighbours do
        if (← getCell nx ny).honey then addWater hk nx ny
      if (← getCell x y).shimmer then
        shimmerCheck hk x y
      else
        for (nx, ny) in neighbours do
          if (← getCell nx ny).shimmer then addWater hk nx ny
  -- falling
  let d ← getCell x (y + 1)
  let c ← getCell x y
  if !(d.blocks ig) && (d.liquid == 0 || d.kind == c.kind) && d.liquid < 255 then
    let num := min (255 - d.liquid) c.liquid
    let flag := num == 1 && c.liquid == 255
    if !flag then setLiquid x y (c.liquid - num)
    setLiquid x (y + 1) (d.liquid + num)
    setKind x (y + 1) c.kind
    addWater hk x (y + 1)
    setSkip x (y + 1) true
    setSkip x y true
    if (← get).quickSettle && (← getCell x y).liquid > 250 then
      setLiquid x y 255
    else if !flag then
      addWater hk (x - 1) y
      addWater hk (x + 1) y
  -- spreading
  let c ← getCell x y
  if c.liquid > 0 then
    let kind := c.kind
    let side (dir : Int) : M (Bool × Bool) := do
      let n1 ← getCell (x + dir) y
      if n1.blocks ig then return (false, true)
      if n1.liquid > 0 && n1.kind != kind then return (false, true)
      let n2 ← getCell (x + 2 * dir) y
      if n2.blocks ig then return (true, false)
      if n2.liquid == 0 then return (true, false)
      if n2.kind != kind then return (true, false)
      return (true, true)
    let (fL, fL2) ← side (-1)
    let (fR, fR2) ← side 1
    let num13 : Int := if c.liquid < 3 then -1 else 0
    let (fL2, fR2) := if c.liquid > 250 then (false, false) else (fL2, fR2)
    let third (dir : Int) : M Bool := do
      let n ← getCell (x + 3 * dir) y
      return !(n.blocks ig) && n.liquid != 0 && n.kind == kind
    let liq (dx : Int) : M Int := return ((← getCell (x + dx) y).liquid : Int)
    let pushTo (dx : Int) (num : Nat) (orCentre : Bool) : M Bool := do
      setKind (x + dx) y kind
      let n ← getCell (x + dx) y
      let cc ← getCell x y
      if n.liquid != num || (orCentre && cc.liquid != num) then
        setLiquid (x + dx) y num
        addWater hk (x + dx) y
        return false
      return true
    let recheck (dx : Int) (num : Nat) : M Unit := do
      if (← getCell (x + dx) y).liquid != num || (← getCell x y).liquid != num then addWater hk (x + dx) y
    if fL && fR then
      if fL2 && fR2 then
        let fL3 ← third (-1)
        let fR3 ← third 1
        if fL3 && fR3 then
          let sum := (← liq (-1)) + (← liq 1) + (← liq (-2)) + (← liq 2) + (← liq (-3)) + (← liq 3) + (← liq 0) + num13
          let num := roundDiv sum 7
          let mut same := 0
          for dx in [(-1 : Int), 1, -2, 2, -3, 3] do
            if (← pushTo dx num false) then same := same + 1
          for dx in [(-1 : Int), 1, -2, 2, -3, 3] do
            recheck dx num
          if !(same == 6 && (← getCell x (y - 1)).liquid > 0) then setLiquid x y num
        else
          let sum := (← liq (-1)) + (← liq 1) + (← liq (-2)) + (← liq 2) + (← liq 0) + num13
          let num := roundDiv sum 5
          let mut same := 0
          for dx in [(-1 : Int), 1, -2, 2] do
            if (← pushTo dx num false) then same := same + 1
          for dx in [(-1 : Int), 1, -2, 2] do
            recheck dx num
          if !(same == 4 && (← getCell x (y - 1)).liquid > 0) then setLiquid x y num
      else if fL2 then
        let sum := (← liq (-1)) + (← liq 1) + (← liq (-2)) + (← liq 0) + num13
        let num := roundDiv sum 4
        for dx in [(-1 : Int), 1, -2] do
          let _ ← pushTo dx num true
        setLiquid x y num
      else if fR2 then
        let sum := (← liq (-1)) + (← liq 1) + (← liq 2) + (← liq 0) + num13
        let num := roundDiv sum 4
        for dx in [(-1 : Int), 1, 2] do
          let _ ← pushTo dx num true
        setLiquid x y num
      else
        let sum := (← liq (-1)) + (← liq 1) + (← liq 0) + num13
        let mut num := roundDiv sum 3
        if num == 254 then
          if (← draw 30) == 0 then num := 255
        for dx in [(-1 : Int), 1] do
          let _ ← pushTo dx num false
        setLiquid x y num
    else if fL || fR then
      let dx : Int := if fL then -1 else 1
      let sum := (← liq dx) + (← liq 0) + num13
      let num := roundDiv sum 2
      if (← getCell (x + dx) y).liquid != num then setLiquid (x + dx) y num
      setKind (x + dx) y kind
      if (← getCell x y).liquid != num || (← getCell (x + dx) y).liquid != num then addWater hk (x + dx) y
      setLiquid x y num
  -- kill bookkeeping
  let now := (← getCell x y).liquid
  if now != original then
    if now == 254 && original == 255 then
      if (← get).quickSettle then setLiquid x y 255
      modEntry i fun e => { e with kill := e.kill + 1 }
      return
    addWater hk x (y - 1)
    modEntry i fun e => { e with kill := 0 }
  else
    modEntry i fun e => { e with kill := e.kill + 1 }

def openTo (ig : Bool) (t : Cell) : Bool := !(t.blocks ig)

/-- `DelWater` turns these grass tiles next to lava into dirt (0) or mud (59). -/
def burnTarget (type : Nat) : Option UInt16 :=
  if [2, 23, 109, 199, 477, 492].contains type then some 0
  else if [60, 70, 661, 662].contains type then some 59 else none

/-- `Liquid.DelWater(l)` -/
def delWater (hk : Hooks) (l : Nat) : M Unit := do
  let e ← entry l
  let x := e.x
  let y := e.y
  let ig := (← get).ignore
  let l0 ← getCell (x - 1) y
  let r0 ← getCell (x + 1) y
  let d0 ← getCell x (y + 1)
  let c0 ← getCell x y
  let mut keep := false
  if c0.liquid < 2 then
    setLiquid x y 0
    if l0.liquid < 2 then setLiquid (x - 1) y 0 else addWater hk (x - 1) y
    if r0.liquid < 2 then setLiquid (x + 1) y 0 else addWater hk (x + 1) y
  else if c0.liquid < 20 then
    if (l0.liquid < c0.liquid && openTo ig l0) || (r0.liquid < c0.liquid && openTo ig r0) ||
        (d0.liquid < 255 && openTo ig d0) then
      setLiquid x y 0
  else if d0.liquid < 255 && openTo ig d0 && !(← get).stuck && openTo ig c0 then
    keep := true
  if keep then
    modEntry l fun e => { e with kill := 0 }
    return
  let c ← getCell x y
  if c.liquid < 250 && (← getCell x (y - 1)).liquid > 0 then addWater hk x (y - 1)
  let c ← getCell x y
  if c.liquid == 0 then
    setKind x y 0
  else
    let r ← getCell (x + 1) y
    if r.liquid > 0 && r.liquid < 250 && openTo ig r && c.liquid != r.liquid then addWater hk (x + 1) y
    let l ← getCell (x - 1) y
    let c ← getCell x y
    if l.liquid > 0 && l.liquid < 250 && openTo ig l && c.liquid != l.liquid then addWater hk (x - 1) y
    let c ← getCell x y
    if c.lava then
      lavaCheck hk x y
      for i in [x - 1, x, x + 1] do
        for j in [y - 1, y, y + 1] do
          let t ← getCell i j
          if t.active then
            if let some to := burnTarget t.type then
              setCell i j { t with id := some to }
              squareTileFrame hk i j true
    else if c.honey then honeyCheck hk x y
    else if c.shimmer then shimmerCheck hk x y
  modify fun s => { s with numLiquid := s.numLiquid - 1 }
  let e ← entry l
  setChecking e.x e.y false
  let last ← entry (← get).numLiquid
  modEntry l fun e => { e with x := last.x, y := last.y, kill := last.kill }
  let c ← getCell x y
  if [82, 83, 84].contains c.type then
    note s!"DelWater({x},{y}): CheckAlch on type {c.type} not ported"
  else if c.type == 518 then
    if (← get).quickFall then checkLilyPad hk x y
    else if (← getCell x (y + 1)).liquid < 255 || (← getCell x (y - 1)).liquid > 0 then squareTileFrame hk x y
    else checkLilyPad hk x y

/-- `Liquid.UpdateLiquid()` with `Main.netMode != 2`. -/
def updateLiquid (hk : Hooks) : M Unit := do
  modify fun s => { s with ignore := true }
  if !(← get).loading then
    let s ← get
    if !s.panicMode then
      if (s.numBuffer : Int) * 10 ≥ s.maxLiquidBuffer * 9 then
        modify fun s => { s with panicCounter := s.panicCounter + 1 }
        if (← get).panicCounter > 3600 then note "UpdateLiquid: StartPanic not ported"
      else
        modify fun s => { s with panicCounter := 0 }
    if (← get).panicMode then
      note "UpdateLiquid: panic mode (QuickWater) not ported"
      return
  modify fun s => { s with quickFall := s.quickSettle, wetCounter := s.wetCounter + 1 }
  let s ← get
  let num3 := s.curMaxLiquid / s.cycles
  let num4 := num3 * (s.wetCounter - 1)
  let mut num5 := num3 * s.wetCounter
  if s.wetCounter == s.cycles then num5 := s.numLiquid
  if num5 > s.numLiquid then
    num5 := s.numLiquid
    modify fun s => { s with wetCounter := s.cycles }
  if num4 < num5 then
    for k in [num4.toNat : num5.toNat] do
      if (← get).quickFall then
        modEntry k fun e => { e with delay := 10 }
        update hk k
        let e ← entry k
        setSkip e.x e.y false
      else
        let e ← entry k
        if (← getCell e.x e.y).skip then setSkip e.x e.y false
        else update hk k
  let s ← get
  if s.wetCounter ≥ s.cycles then
    modify fun s => { s with wetCounter := 0 }
    let mut k : Int := (← get).numLiquid - 1
    while k ≥ 0 do
      let e ← entry k.toNat
      if e.kill ≥ 8 then
        if (← getCell e.x e.y).liquid == 254 then setLiquid e.x e.y 255
        delWater hk k.toNat
      k := k - 1
    let s ← get
    let n := min (s.curMaxLiquid - (s.curMaxLiquid - s.numLiquid)) s.numBuffer
    for _ in [:n.toNat] do
      let (bx, by_) := (← get).buffer[0]!
      setChecking bx by_ false
      addWater hk bx by_
      delBuffer 0
    let s ← get
    let nl : Int := s.numLiquid
    if nl > 0 && nl > s.stuckAmount - 50 && nl < s.stuckAmount + 50 then
      modify fun s => { s with stuckCount := s.stuckCount + 1 }
      if (← get).stuckCount ≥ 10000 then
        modify fun s => { s with stuck := true }
        let mut j : Int := (← get).numLiquid - 1
        while j ≥ 0 do
          delWater hk j.toNat
          j := j - 1
        modify fun s => { s with stuck := false, stuckCount := 0 }
    else
      modify fun s => { s with stuckCount := 0, stuckAmount := nl }
  modify fun s => { s with ignore := false }

end Lean4Terria.OriginalLiquid
