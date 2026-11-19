import Lean4Terria.OriginalLiquid.Core

/-!
# Liquids while a saved world loads (`WorldFile.LoadWorld`, 1.4.5.8)

After the tile sections are read, `LoadWorld` runs (IL_02b9 … IL_03d8):

1. `isGeneratingOrLoadingWorld = true`, `GenVars.waterLine = maxTilesY`
2. `Liquid.QuickWater(2, -1, -1)` — `SettleWaterAt` on every liquid cell, rows bottom-up
3. `WorldGen.WaterCheck()` — clears every queue flag and re-queues every cell that can still move
4. `Liquid.quickSettle = true`; `UpdateLiquid()` while `numLiquid > 0`, at most 100000 times
5. `quickSettle = false`, weather and clouds (no liquid effect), `WorldGen.WaterCheck()` again
6. `isGeneratingOrLoadingWorld = false`

The liquid statics are not reset on this path (`Liquid.ReInit` is only reached from world
generation), so `cycles` / `curMaxLiquid` are whatever `Main.DoUpdate` last wrote on the title menu:
`(int)(17 − 10·gfxQuality)` and `(int)(25000·(0.25 + 0.75·gfxQuality))`, i.e. 7 / 25000 at full
quality. The random calls inside (three-cell 254 roundings) use `Main.rand`, which `LoadWorld`
reseeds from `DateTime.Now.Ticks` just before; the caller supplies that stream.
-/

namespace Lean4Terria.OriginalLiquid

open OriginalWorld (Terrain TileShape LiquidKind)

/-- `Liquid.AttemptToMoveLava / Honey / Shimmer(x, y, isKind)` -/
def attemptToMove (hk : Hooks) (x y : Int) (k : Nat) (isKind : Bool) : M Unit := do
  for (dx, dy) in [((-1 : Int), (0 : Int)), (1, 0), (0, -1), (0, 1)] do
    let n ← getCell (x + dx) (y + dy)
    if n.liquid > 0 && (n.kind == k) != isKind then
      if isKind then liquidCheck hk x y k else liquidCheck hk (x + dx) (y + dy) k
      return

/-- `Liquid.SettleWaterAt(originX, originY)`; returns where the liquid ended. -/
def settleWaterAt (hk : Hooks) (ox oy : Int) : M (Int × Int) := do
  modify fun s => { s with ignore := true }
  let t ← getCell ox oy
  if t.liquid == 0 then return (ox, oy)
  if t.active && t.type == 379 then return (ox, oy)
  let maxX := (← get).maxTilesX
  let maxY := (← get).maxTilesY
  let ig := true
  let isLava := t.lava
  let isHoney := t.honey
  let isShimmer := t.shimmer
  let mut amt := t.liquid
  let ltype := t.kind
  setLiquid ox oy 0
  let mut num := ox
  let mut num2 := oy
  let mut flag := true
  while true do
    let mut tile2 ← getCell num (num2 + 1)
    while num2 < maxY - 5 && tile2.liquid == 0 && !(tile2.blocks ig) do
      num2 := num2 + 1
      flag := false
      tile2 ← getCell num (num2 + 1)
    -- `isGeneratingOrLoadingWorld && moved`: below `GenVars.waterLine` water turns to lava; the
    -- loader sets `waterLine = maxTilesY`, so this never fires while loading a saved world.
    let mut dir : Int := -1
    let mut dist : Int := 0
    let mut num13 : Int := -1
    let mut num14 : Int := 0
    let mut f15 := false
    let mut f16 := false
    let mut f17 := false
    while true do
      let px := num + dist * dir
      if (← getCell px num2).liquid == 0 then
        num13 := dir
        num14 := dist
      if dir == -1 && px < 5 then f16 := true
      else if dir == 1 && px > maxX - 5 then f15 := true
      let below ← getCell px (num2 + 1)
      if below.liquid != 0 && below.liquid != 255 && below.kind == ltype then
        let n := min (255 - below.liquid) amt
        setLiquid px (num2 + 1) (below.liquid + n)
        amt := amt - n
        if amt == 0 then break
      let below ← getCell px (num2 + 1)
      if num2 < maxY - 5 && below.liquid == 0 && !(below.blocks ig) then
        f17 := true
        break
      let next ← getCell (num + (dist + 1) * dir) num2
      if (next.liquid != 0 && (!flag || dir != 1)) || next.blocks ig then
        if dir == 1 then f15 := true else f16 := true
      if f16 && f15 then break
      if f15 then
        dir := -1
        dist := dist + 1
      else if f16 then
        if dir == 1 then dist := dist + 1
        dir := 1
      else
        if dir == 1 then dist := dist + 1
        dir := -dir
    num := num + num14 * num13
    if amt != 0 && f17 then
      num2 := num2 + 1
    else break
  setCell num num2 { (← getCell num num2) with liquid := amt, kind := ltype }
  if (← getCell num num2).liquid > 0 then
    attemptToMove hk num num2 1 isLava
    attemptToMove hk num num2 2 isHoney
    attemptToMove hk num num2 3 isShimmer
  modify fun s => { s with ignore := false }
  return (num, num2)

/-- Liquid cells of the map, by row, x ascending (runs of the terrain + overlay). -/
def liquidRows (s : State) : Array (Array Nat) := Id.run do
  let mut rows : Array (Array Nat) := Array.replicate s.height #[]
  for x in [:s.width] do
    let runs := s.terrain.columns[x]!
    let mut start := 0
    for run in runs do
      if run.tile.liquid.isSome then
        for y in [start:run.endY] do
          if !s.overlay.contains (x * s.height + y) then rows := rows.modify y (·.push x)
      start := run.endY
  -- overlay cells (live values) with liquid
  let mut extra : Array (Nat × Nat) := #[]
  for (k, c) in s.overlay.toArray do
    if c.liquid > 0 then extra := extra.push (k / s.height, k % s.height)
  for (x, y) in extra do rows := rows.modify y (·.push x)
  return rows.map (·.qsort (· < ·))

private def insertSorted (a : Array Nat) (v : Nat) : Array Nat := Id.run do
  if a.contains v then return a
  let mut i := a.size
  let mut out := a.push v
  while i > 0 && out[i - 1]! > v do
    out := out.swapIfInBounds (i - 1) i
    i := i - 1
  return out

/-- `Liquid.QuickWater(verbose, -1, -1)`: rows `maxTilesY − 3` down to 3, x from 4 to `maxTilesX − 5`. -/
def quickWater (hk : Hooks) : M Unit := do
  modify fun s => { s with ignore := true }
  let s ← get
  let rows := liquidRows s
  let maxX := s.width - 4
  let mut y : Int := s.maxTilesY - 3
  while y ≥ 3 do
    let mut xs := (rows[y.toNat]!).filter fun x => x ≥ 4 && x < maxX
    let mut i := 0
    while i < xs.size do
      let x := xs[i]!
      if (← getCell x y).liquid != 0 then
        let (nx, ny) ← settleWaterAt hk x y
        if ny == y && nx > x && nx < maxX then xs := insertSorted xs nx.toNat
      i := i + 1
    y := y - 1
  modify fun s => { s with ignore := false }

/-- `WorldGen.WaterCheck()` -/
def waterCheck (hk : Hooks) : M Unit := do
  modify fun s => { s with ignore := true, numLiquid := 0, numBuffer := 0 }
  let s ← get
  let w := s.width
  let h := s.height
  -- every cell in x ∈ [1, maxX − 2], y ∈ [1, maxY − 2] gets `checkingLiquid(false)`; only overlay
  -- cells can hold the flag
  for (k, c) in s.overlay.toArray do
    let x := k / h
    let y := k % h
    if c.checking && x ≥ 1 && x ≤ w - 2 && y ≥ 1 && y ≤ h - 2 then setChecking x y false
  let rows := liquidRows (← get)
  -- visit order: x ascending, y descending
  let mut cols : Array (Array Nat) := Array.replicate w #[]
  for y in [:h] do
    for x in rows[y]! do cols := cols.modify x (·.push y)
  for x in [1:w - 1] do
    let ys := cols[x]!
    let mut i := ys.size
    while i > 0 do
      i := i - 1
      let y := ys[i]!
      if y < 1 || y > h - 2 then continue
      let xi : Int := x
      let yi : Int := y
      let t ← getCell xi yi
      if t.liquid == 0 then continue
      if t.nactive && solid true t.type && !solidTop t.type then
        if t.type != 379 then setLiquid xi yi 0
        continue
      if t.active then
        match hk.liquidKills t with
        | some true => hk.killTile xi yi
        | some false => pure ()
        | none => note s!"WaterCheck({x},{y}): liquid death of type {t.type} depends on an unported style rule"
      let t ← getCell xi yi
      let d ← getCell xi (yi + 1)
      if !(d.blocks true) && d.liquid < 255 then
        if d.liquid > 250 then setLiquid xi (yi + 1) 255 else addWater hk xi yi
      let l ← getCell (xi - 1) yi
      let r ← getCell (xi + 1) yi
      if !(l.blocks true) && l.liquid != t.liquid then addWater hk xi yi
      else if !(r.blocks true) && r.liquid != t.liquid then addWater hk xi yi
      if t.lava then
        let u ← getCell xi (yi - 1)
        let d ← getCell xi (yi + 1)
        if l.liquid > 0 && !l.lava then addWater hk xi yi
        else if r.liquid > 0 && !r.lava then addWater hk xi yi
        else if u.liquid > 0 && !u.lava then addWater hk xi yi
        else if d.liquid > 0 && !d.lava then addWater hk xi yi
  modify fun s => { s with ignore := false }

structure LoadSettings where
  /-- `Liquid.cycles` / `curMaxLiquid` left by the title menu's `Main.DoUpdate` -/
  cycles : Int := 7
  curMaxLiquid : Int := 25000
  /-- `Main.rand` after `LoadWorld` reseeds it (`new UnifiedRandom((int)DateTime.Now.Ticks)`) -/
  loaderRng : UnifiedRandom

structure LoadReport where
  settleCalls : Nat
  loaderDraws : Nat
  liquidCells : Nat
  loaderRandom : UnifiedRandom

/-- The liquid part of `WorldFile.LoadWorld` on a freshly decoded map. Returns the state ready for
play (queue and buffer as the second `WaterCheck` leaves them) with the `UpdateLiquid` stream not
yet attached (see `Frame.startPlay`). -/
def loadWorld (terrain : Terrain) (cfg : LoadSettings) (hk : Hooks := {}) : State × LoadReport := Id.run do
  let s0 : State := { terrain, rng := cfg.loaderRng, cycles := cfg.cycles, curMaxLiquid := cfg.curMaxLiquid,
                      loading := true }
  let run : M Nat := do
    quickWater hk
    waterCheck hk
    modify fun s => { s with quickSettle := true }
    let mut calls := 0
    while (← get).numLiquid > 0 && calls < 100000 do
      calls := calls + 1
      updateLiquid hk
    modify fun s => { s with quickSettle := false }
    waterCheck hk
    modify fun s => { s with loading := false }
    return calls
  let (calls, s) := run.run s0
  let cells := (liquidRows s).foldl (fun n r => n + r.size) 0
  return (s, { settleCalls := calls, loaderDraws := s.draws, liquidCells := cells, loaderRandom := s.rng })

end Lean4Terria.OriginalLiquid
