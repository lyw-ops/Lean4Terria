import Lean4Terria.OriginalLiquid.Load

/-!
# Liquids during play: the per-frame call order of 1.4.5.8

Each game frame (`Main.DoUpdate`):

* `Main.DoUpdate` rewrites `Liquid.cycles = (int)(17 − 10·gfxQuality)` and
  `Liquid.curMaxLiquid = (int)(maxLiquid·0.25 + maxLiquid·0.75·gfxQuality)`, or `maxLiquid` / 1 while
  `Liquid.quickSettle` holds (`Main.DoUpdate` IL_06c4 … IL_07c6).
* Players update first (mining, placing and buckets reach liquids through `WorldGen.SquareTileFrame`
  → `TileFrame` → `Liquid.AddWater`; no draw from the liquid stream).
* `Main.UpdateWorld_WorldGenAndInvasion` → `WorldGen.UpdateWorld`: `Liquid.skipCount++`; when it
  exceeds 1, `Liquid.UpdateLiquid()` runs inside `Main.SwapRandom("UpdateLiquid")` and the count is
  reset — liquids advance every second frame. The `UpdateLiquid` stream is created on first use with
  the world's numeric seed.

`frame` is that schedule; `killTile`, `placeTile`, `placeLiquid` are the liquid-facing halves of the
player's edits; `takeChanges` (Core) hands the touched cells back to the run-length map.
-/

namespace Lean4Terria.OriginalLiquid

open OriginalWorld (Terrain TileShape LiquidKind)

structure Runtime where
  liquid : State
  /-- `Liquid.skipCount` -/
  skipCount : Nat := 0
  /-- `Main.gfxQuality` as the two statics see it -/
  cycles : Int := 7
  curMaxLiquid : Int := 25000
  hooks : Hooks := {}

/-- Load a decoded world and attach the `UpdateLiquid` stream (`worldSeed` = the world's numeric seed,
`OriginalWorld.numericSeed header.seed`). `loaderRng` stands for the clock-seeded `Main.rand` of
`LoadWorld`; vanilla does not make it reproducible either. -/
def Runtime.load (terrain : Terrain) (worldSeed : Int) (loaderRng : UnifiedRandom)
    (cycles : Int := 7) (curMaxLiquid : Int := 25000) (hooks : Hooks := {}) : Runtime × LoadReport :=
  let (s, report) := loadWorld terrain { cycles, curMaxLiquid, loaderRng } hooks
  ({ liquid := { s with rng := UnifiedRandom.seed worldSeed, draws := 0 }, cycles, curMaxLiquid, hooks }, report)

/-- Run an edit or step on the liquid state. -/
def Runtime.run {α : Type} (r : Runtime) (m : M α) : α × Runtime :=
  let (a, s) := m.run r.liquid
  (a, { r with liquid := s })

/-- One `UpdateLiquid` with the `Main.DoUpdate` statics applied. -/
def Runtime.tick (r : Runtime) : Runtime :=
  let s := r.liquid
  let s := if s.quickSettle then { s with curMaxLiquid := s.maxLiquid, cycles := 1 }
    else { s with cycles := r.cycles, curMaxLiquid := r.curMaxLiquid }
  ({ r with liquid := s }.run (updateLiquid r.hooks)).2

/-- One game frame: `WorldGen.UpdateWorld`'s `skipCount` gate around `UpdateLiquid`. -/
def Runtime.frame (r : Runtime) : Runtime :=
  let r := { r with skipCount := r.skipCount + 1 }
  if r.skipCount > 1 then { r.tick with skipCount := 0 } else r

/-- The liquid half of `WorldGen.KillTile(x, y)` (mining, axe, explosions): the tile is gone and the
3 × 3 block is framed, which queues adjacent liquid. -/
def killTile (r : Runtime) (x y : Int) : Runtime :=
  (r.run (do
    modCell x y fun c => { c with id := none, actuated := false, shape := .full, frame := none }
    squareTileFrame r.hooks x y)).2

/-- The liquid half of `WorldGen.PlaceTile` for a block: frame the 3 × 3 block. -/
def placeTile (r : Runtime) (x y : Int) (type : UInt16) : Runtime :=
  (r.run (do
    modCell x y fun c => { c with id := some type, actuated := false, shape := .full, frame := none }
    squareTileFrame r.hooks x y)).2

/-- `Player.ItemCheck_UseBuckets` placing liquid: set the amount and type, `SquareTileFrame`, then
`Liquid.AddWater` at the target. -/
def placeLiquid (r : Runtime) (x y : Int) (kind : LiquidKind) (amount : Nat := 255) : Runtime :=
  (r.run (do
    modCell x y fun c => { c with liquid := amount, kind := kindCode kind }
    squareTileFrame r.hooks x y
    addWater r.hooks x y)).2

private def Runtime.adoptCells (r : Runtime) (terrain : Terrain) (cells : List (Nat × Nat)) : Runtime := Id.run do
  let mut s := { r.liquid with terrain }
  for (x, y) in cells do
    let k := x * s.height + y
    if let some old := s.overlay.get? k then
      let fresh := s.baseCell k
      let kind := if fresh.liquid == old.liquid then old.kind else fresh.kind
      s := { s with overlay := s.overlay.insert k { fresh with kind, skip := old.skip, checking := old.checking } }
  return { r with liquid := s }

/-- Background-wall edits are already framed by `SquareWallFrame`. Update the map and any
cached cell descriptions without the foreground framing or liquid scheduling of `KillTile`. -/
def Runtime.adoptWallEdits (r : Runtime) (terrain : Terrain) (cells : List (Nat × Nat)) : Runtime :=
  r.adoptCells terrain cells

/-- Hand an edit made elsewhere (mining, placing, explosions writing `Terrain.replaceTile`) to the
liquid state: pending liquid changes are written back first (returned), the edited map is adopted,
each edited cell takes the map's tile while keeping its queue flags, and the `SquareTileFrame` that
`KillTile` / `PlaceTile` end with runs for each edited cell in order. `edit` receives the map with
all liquid changes applied. -/
def Runtime.adoptEdit (r : Runtime) (edit : Terrain → Terrain) (cells : List (Nat × Nat))
    (frameCells : Option (List (Nat × Nat)) := none) :
    Array Change × Runtime := Id.run do
  let (flushed, r) := r.run Lean4Terria.OriginalLiquid.takeChanges
  let r := r.adoptCells (edit r.liquid.terrain) cells
  let r := (r.run (do for (x, y) in frameCells.getD cells do squareTileFrame r.hooks x y)).2
  return (flushed, r)

/-- `adoptEdit` with the edit's own random stream. Vanilla mines and places during the player update,
inside `Main.SwapRandom("UpdatePlayers")`, so anything the edit's framing draws (a vine or plant losing
its support and being killed, a `resetFrame` roll) comes from that stream, not from the liquid one.
Returns the flushed liquid changes, the runtime, and the advanced `playerRng`. -/
def Runtime.adoptEditWithRandom (r : Runtime) (edit : Terrain → Terrain) (cells : List (Nat × Nat))
    (playerRng : UnifiedRandom) (frameCells : Option (List (Nat × Nat)) := none) :
    Array Change × Runtime × UnifiedRandom :=
  let liquidRng := r.liquid.rng
  let draws := r.liquid.draws
  let (flushed, r) := { r with liquid := { r.liquid with rng := playerRng } }.adoptEdit edit cells frameCells
  (flushed, { r with liquid := { r.liquid with rng := liquidRng, draws } }, r.liquid.rng)

/-- Write the cells touched since the last call back into the map; returns the changed cells. -/
def Runtime.takeChanges (r : Runtime) : Array Change × Runtime := r.run Lean4Terria.OriginalLiquid.takeChanges

def Runtime.terrain (r : Runtime) : Terrain := r.liquid.terrain

/-- The items liquid kills created since the last call (velocity draws already taken). -/
def Runtime.takeDrops (r : Runtime) : Array PendingDrop × Runtime :=
  (r.liquid.drops, { r with liquid := { r.liquid with drops := #[] } })

/-- The critters liquid kills spawned since the last call (luck and velocity draws already taken). -/
def Runtime.takeSpawns (r : Runtime) : Array PendingSpawn × Runtime :=
  (r.liquid.spawns, { r with liquid := { r.liquid with spawns := #[] } })

/-- Same as `frame` (one game frame); kept as the name the game entry uses. -/
def Runtime.tickFrame (r : Runtime) : Runtime := r.frame

end Lean4Terria.OriginalLiquid
