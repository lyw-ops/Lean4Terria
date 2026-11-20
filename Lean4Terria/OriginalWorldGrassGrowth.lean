import Lean4Terria.UnifiedRandom

/-!
# Ordinary grass growth (Terraria 1.4.5.8 desktop, single player, netMode 0)

Hand port of the installed CIL of `WorldGen.UpdateWorld_GrassGrowth`, `WorldGen.SpreadGrass` and
`WorldGen.SpreadGrassWalls` for the ordinary surface case. Evidence: `.lake/original-world-grass-growth/`.

Scope:
* `spreadGrass` — dirt → grass with no `Convert` involvement (`Conversion.Grass[dirt]` false, grass not one of
  23 / 661 / 199 / 662 / 109 / 492), the real exposure test (3×3 solidity with the lava exception),
  `CanBeClearedDuringGeneration`, the tree-above hand-off, the `Tile.type` store, `SquareTileFrame(x, y, true)`,
  `UseBlockColors`, and the bounded recursion through `grassSpread`.
* `grassGrowth` — the surface (`underground = false`) branch for a grass block (type 2): the `Next(12)` plant roll
  with its `PlaceTile` request, then the neighbour scan with its double `SpreadGrass` call and the extra frames.
  Underground growth for types that have no branch is a no-op exactly as in the original.
* `spreadGrassWalls` — the whole decision tree up to the `Convert` call, which is a boundary.

Everything the original does not decide itself is an explicit adapter, identical on the native side of the
evidence: `SquareTileFrame` and `PlaceTile` only emit ordered requests (the framing owner applies a frame
request; `TileFrame` itself draws from `Main.rand`, which is `WorldGen.genRand`, so the caller must thread the
stream through the requests in the order returned). Unsupported branches set `error` and make no further change.
-/

namespace Lean4Terria.OriginalWorldGrassGrowth

structure Cell where
  active : Bool := false
  type : Nat := 0
  liquid : Nat := 0
  lava : Bool := false
  paint : Nat := 0
  fullBright : Bool := false
  invisible : Bool := false
  wall : Nat := 0
  deriving BEq, Repr, Inhabited

/-- `TileColorCache` (BlockColorAndCoating / UseBlockColors). -/
structure ColorCache where
  paint : Nat := 0
  fullBright : Bool := false
  invisible : Bool := false
  deriving BEq, Repr, Inhabited

structure World where
  width : Nat
  height : Nat
  /-- column-major: index `x * height + y` -/
  cells : Array Cell
  worldSurface : Float
  growGrassUnderground : Bool := false
  deriving Inhabited

/-- Read-only data from the installed image (`Main.tileSolid`, `TileID.Sets`, `WallID.Sets`, `Conversion`,
`WorldGen.IsTreeType`), plus the world flags this code reads. -/
structure Tables where
  solid : Nat → Bool
  clearable : Nat → Bool
  conversionGrass : Nat → Bool
  isTree : Nat → Bool
  tileCrimson : Nat → Bool
  tileCorruption : Nat → Bool
  tileHallow : Nat → Bool
  wallCrimson : Nat → Bool
  wallCorruption : Nat → Bool
  wallHallow : Nat → Bool
  halloween : Bool := false

inductive Request where
  /-- the `Tile.type` store inside SpreadGrass -/
  | setType (x y : Int) (old new : Nat)
  /-- `SquareTileFrame(x, y, reset)`; `around` is the 3×3 world (dx outer, dy inner) at the moment of the request -/
  | frame (x y : Int) (reset : Bool) (around : Array (Option Cell))
  /-- `PlaceTile(x, y, type, mute, forced, plr, style)`; the adapter places nothing -/
  | placeTile (x y type : Int) (mute forced : Bool) (plr style : Int)
  deriving BEq, Repr

structure State where
  world : World
  rng : UnifiedRandom
  requests : Array Request := #[]
  grassSpread : Nat := 0
  error : Option String := none

instance : Inhabited State := ⟨{ world := default, rng := UnifiedRandom.seed 0 }⟩

def World.inArray (w : World) (x y : Int) : Bool := 0 ≤ x && x < w.width && 0 ≤ y && y < w.height

def World.cell (w : World) (x y : Int) : Cell :=
  if w.inArray x y then w.cells[x.toNat * w.height + y.toNat]! else {}

def World.setCell (w : World) (x y : Int) (c : Cell) : World :=
  if w.inArray x y then { w with cells := w.cells.set! (x.toNat * w.height + y.toNat) c } else w

/-- `WorldGen.InWorld(x, y, fluff)` -/
def World.inWorld (w : World) (x y fluff : Int) : Bool :=
  x ≥ fluff && x < (w.width : Int) - fluff && y ≥ fluff && y < (w.height : Int) - fluff

def World.blockColor (w : World) (x y : Int) : ColorCache :=
  let c := w.cell x y
  { paint := c.paint, fullBright := c.fullBright, invisible := c.invisible }

private def World.around (w : World) (x y : Int) : Array (Option Cell) := Id.run do
  let mut out := #[]
  for dx in [0:3] do
    for dy in [0:3] do
      let cx := x + dx - 1
      let cy := y + dy - 1
      out := out.push (if w.inArray cx cy then some (w.cell cx cy) else none)
  return out

private def State.frame (s : State) (x y : Int) (reset : Bool) : State :=
  { s with requests := s.requests.push (.frame x y reset (s.world.around x y)) }

private def State.fail (s : State) (why : String) : State :=
  { s with error := some (s.error.getD why) }

/-- Grass types whose conversion goes through `Convert`. -/
private def infectious (grass : Nat) : Bool :=
  grass == 23 || grass == 661 || grass == 199 || grass == 662 || grass == 109 || grass == 492

/-- `WorldGen.SpreadGrass(i, j, dirt, grass, repeat, color)`. -/
partial def spreadGrass (t : Tables) (s : State) (i j : Int) (dirt grass : Nat) (repeatSpread : Bool)
    (color : ColorCache) : State := Id.run do
  let w := s.world
  if !w.inWorld i j 10 then return s
  let c := w.cell i j
  if !(c.active && c.type == dirt) then return s
  -- isGeneratingOrLoadingWorld is false while playing
  if !(grass == 199 || grass == 23 || grass == 661 || grass == 662) then
    let proceed := Float.ofInt j < w.worldSurface || dirt != 0 || w.growGrassUnderground
    if !proceed then return s
  -- 3×3 exposure test
  let x0 := max (i - 1) 0
  let x1 := min (i + 2) (w.width : Int)
  let y0 := max (j - 1) 0
  let y1 := min (j + 2) (w.height : Int)
  let mut enclosed := true
  for xi in [x0.toNat:x1.toNat] do
    for yi in [y0.toNat:y1.toNat] do
      let n := w.cell xi yi
      if !(n.active && t.solid n.type) then enclosed := false
      if n.lava && n.liquid > 0 then
        enclosed := true
        break
  if enclosed then return s
  if !t.clearable c.type then return s
  if infectious grass then return s.fail "SpreadGrass: infectious grass needs Convert"
  if dirt > 0 && t.conversionGrass dirt then return s.fail "SpreadGrass: Conversion.Grass[dirt] needs Convert"
  -- TryConvertingOrKillingTreesAboveIfTheyWouldBecomeInvalid: only acts on a tree or palm above; a forest tree (5) on
  -- plain grass stays valid (Conversion.Grass[2] and the tree ground test), checked natively
  let above := w.cell i (j - 1)
  if w.inWorld i j 2 && above.active && (t.isTree above.type || above.type == 323) && !(above.type == 5 && grass == 2) then
    return s.fail "SpreadGrass: tree above needs TryConvertingOrKillingTreesAbove"
  let mut s := s
  s := { s with world := w.setCell i j { c with type := grass },
                requests := s.requests.push (.setType i j c.type grass) }
  s := s.frame i j true
  -- flag2 (infectious grass) is false here, so UseBlockColors always runs
  let cur := s.world.cell i j
  s := { s with world := s.world.setCell i j { cur with paint := color.paint, fullBright := color.fullBright, invisible := color.invisible } }
  -- recursion over the 3×3 (repeat only)
  for xi in [x0.toNat:x1.toNat] do
    for yi in [y0.toNat:y1.toNat] do
      let n := s.world.cell xi yi
      if n.active && n.type == dirt && repeatSpread && s.grassSpread < 1000 then
        s := { s with grassSpread := s.grassSpread + 1 }
        s := spreadGrass t s xi yi dirt grass true {}
        s := { s with grassSpread := s.grassSpread - 1 }
  return s

/-- Adapter for `PlaceTile`: the default places nothing (the evidence stubs the native method the same way). -/
def PlaceAdapter := World → Int → Int → Nat → World

def noPlace : PlaceAdapter := fun w _ _ _ => w

/-- Types the surface block of `UpdateWorld_GrassGrowth` treats in its grass group. -/
private def grassGroup (ty : Nat) : Bool :=
  ty == 2 || ty == 23 || ty == 32 || ty == 109 || ty == 199 || ty == 352 || ty == 477 || ty == 492 || ty == 661 || ty == 662

private def otherGrowthType (ty : Nat) : Bool := ty == 70 || ty == 60 || ty == 226 || ty == 633

/-- `WorldGen.UpdateWorld_GrassGrowth(i, j, minI, maxI, minJ, maxJ, underground)`. -/
def grassGrowth (t : Tables) (place : PlaceAdapter := noPlace) (s : State) (i j minI maxI minJ maxJ : Int)
    (underground : Bool) : State := Id.run do
  let w := s.world
  if !w.inWorld i j 10 then return s
  let ty := (w.cell i j).type
  if underground then
    if ty == 23 || ty == 60 || ty == 70 || ty == 199 || ty == 226 || ty == 633 || ty == 661 || ty == 662 then
      return s.fail "UpdateWorld_GrassGrowth: underground branch for this tile type is outside this leaf"
    return s
  -- surface branch
  if !grassGroup ty then
    if otherGrowthType ty then return s.fail "UpdateWorld_GrassGrowth: surface branch for this tile type is outside this leaf"
    return s
  if ty != 2 then return s.fail "UpdateWorld_GrassGrowth: infection / hallow grass is outside this leaf"
  if t.halloween then return s.fail "UpdateWorld_GrassGrowth: Halloween pumpkins are outside this leaf"
  if !w.inArray i minJ then return s.fail "UpdateWorld_GrassGrowth: tile(i, minJ) is outside the tile array (the original throws)"
  let mut s := s
  -- plant roll on the row `minJ`
  let top := w.cell i minJ
  if !top.active && top.liquid == 0 then
    let (roll, rng) := s.rng.next 0 12
    s := { s with rng }
    if roll == 0 then
      s := { s with requests := s.requests.push (.placeTile i minJ 3 true false (-1) 0), world := place s.world i minJ 3 }
      let placed := s.world.cell i minJ
      if placed.active && placed.type == 3 then
        let src := s.world.cell i j
        s := { s with world := s.world.setCell i minJ { placed with paint := src.paint, fullBright := src.fullBright, invisible := src.invisible } }
  -- neighbour scan: only dirt neighbours are treated (AllowedToSpreadInfections does not apply to type 2)
  let mut x := minI
  while x < maxI do
    let mut y := minJ
    while y < maxJ do
      if s.world.inWorld x y 10 && !(x == i && y == j) then
        let n := s.world.cell x y
        if n.active && n.type == 0 then
          let color := s.world.blockColor i j
          for _ in [0:2] do
            s := spreadGrass t s x y 0 2 false color
            if (s.world.cell x y).type == 2 then s := s.frame x y true
      y := y + 1
    x := x + 1
  return s

/-- `WorldGen.SpreadGrassWalls(range, x, y)`. A `Convert` call is a boundary (`error`). -/
def spreadGrassWalls (t : Tables) (s : State) (range x y : Int) : State := Id.run do
  let w := s.world
  if !w.inWorld x y 10 then return s
  let c := w.cell x y
  let crimson := t.wallCrimson c.wall || (c.active && t.tileCrimson c.type)
  let corruption := t.wallCorruption c.wall || (c.active && t.tileCorruption c.type)
  let hallow := t.wallHallow c.wall || (c.active && t.tileHallow c.type)
  let which : Option (Nat × (Nat → Bool)) :=
    if crimson then some (4, t.tileCrimson)
    else if corruption then some (1, t.tileCorruption)
    else if hallow then some (2, t.tileHallow)
    else none
  let some (_, set) := which | return s
  let mut s := s
  let (dx, rng) := s.rng.next (-2) 3
  let (dy, rng) := rng.next (-2) 3
  s := { s with rng }
  let nx := x + dx
  let ny := y + dy
  let wallOk : Bool :=
    if hallow && !crimson && !corruption then
      let wl := (s.world.cell nx ny).wall
      (s.world.inWorld nx ny 10 && wl == 63) || wl == 65 || wl == 66 || wl == 68
    else
      s.world.inWorld nx ny 10 && 63 ≤ (s.world.cell nx ny).wall && (s.world.cell nx ny).wall ≤ 68
  if !wallOk then return s
  let mut found := false
  let mut xi := x - range
  while xi ≤ x + range do
    let mut yi := y - range
    while yi ≤ y + range do
      let n := s.world.cell xi yi
      if n.active && set n.type then found := true
      yi := yi + 1
    xi := xi + 1
  if found then return s.fail "SpreadGrassWalls: Convert is outside this leaf"
  return s

end Lean4Terria.OriginalWorldGrassGrowth
