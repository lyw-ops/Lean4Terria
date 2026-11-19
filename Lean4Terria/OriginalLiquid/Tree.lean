import Lean4Terria.OriginalLiquid.Kill

/-!
# Trees (tile 5) destroyed by liquid

Lava kills tree cells (`TileObjectData.CheckLavaDeath(5)`), and every kill frames the 3 × 3 block, so the
rest of the tree follows through `WorldGen.TileFrame` → `CheckTree`. The support, branch and reframe
rules and the forest drop rolls are the ones `Lean4Terria.OriginalTree` verified against vanilla axe
felling (`check`, `treeGround`, `canCut`, `addDrops` there); they are restated here on the liquid
overlay's cells, because `OriginalTree.fell` works on `Terrain` and `WorldItems` directly.

`WorldGen.KillTile_GetTreeDrops` (IL `integration/probe/il/WorldGen.KillTile_GetTreeDrops.short`):
treetop frames (`frameX ≥ 22 && frameY ≥ 198`) roll `genRand.Next(2)` for an acorn; then
`genRand.Next(35) ≤ axe || Main.rand.Next(3) == 0` adds a second wood, where `axe` is the selected item
of the player `Player.FindClosest` returns for the tree bottom — not necessarily an axe in hand (0 on the
probe, `Kill.Env.heldAxe`). Wood and acorn are then `Item.NewItem`ed (two draws each).
Only forest trees (ground 2 / 477, wood 9, acorns) are ported; other tree kinds are reported.
-/

namespace Lean4Terria.OriginalLiquid.Tree

open Lean4Terria.OriginalLiquid

def isTree (c : Cell) : Bool := c.id == some 5

def leftBranch (x y : Int) : Bool := (x == 66 && 0 ≤ y && y ≤ 44) || (x == 44 && 198 ≤ y)
def rightBranch (x y : Int) : Bool := (x == 88 && 66 ≤ y && y ≤ 110) || (x == 66 && 198 ≤ y)

def supportTile (c : Cell) : Bool :=
  c.id.any fun id => (#[2, 23, 60, 70, 109, 147, 199, 477, 492, 661, 662] : Array UInt16).contains id

def isTrunk (id : UInt16) : Bool :=
  (#[5, 72, 583, 584, 585, 586, 587, 588, 589, 596, 616, 634] : Array UInt16).contains id

/-- the first non-trunk tile under the tree's root column -/
def treeGround (cx cy : Int) (x y : Int) : M (Option UInt16) := do
  let rx := if leftBranch x y || (x == 44 && 132 ≤ y && y ≤ 176) then cx + 1
    else if rightBranch x y || (x == 22 && 132 ≤ y && y ≤ 176) then cx - 1 else cx
  let h := (← get).maxTilesY
  let mut yy := cy
  while yy < h do
    if let some id := (← getCell rx yy).id then
      if !isTrunk id then return some id
    yy := yy + 1
  return none

inductive Check where
  | keep
  | cut
  | reframe (x y : Int)

/-- `WorldGen.CheckTree` as `OriginalTree.check`. -/
def check (cx cy : Int) (x y : Int) : M Check := do
  let left := isTree (← getCell (cx - 1) cy)
  let right := isTree (← getCell (cx + 1) cy)
  let above := isTree (← getCell cx (cy - 1))
  let below ← getCell cx (cy + 1)
  let onGrass := supportTile below
  let onTrunk := isTree below
  let rootRow := 132 ≤ y && y ≤ 176
  let variant := y % 66
  if (x == 0 || x == 22 || x == 44) && y ≤ 130 && !onGrass && !onTrunk then return .cut
  if (x == 22 || x == 44) && rootRow then
    return if onGrass && (if x == 22 then left else right) then .keep else .cut
  let fork := (x == 88 && 0 ≤ y && y ≤ 44) || (x == 66 && 66 ≤ y && y ≤ 130) ||
    (x == 110 && 66 ≤ y && y ≤ 110) || (x == 132 && 0 ≤ y && y ≤ 176)
  let (x, y) := if fork then
    if left && right then (110, 66 + variant)
    else if left then (88, variant)
    else if right then (66, 66 + variant)
    else (0, variant)
    else (x, y)
  let centralRoot := (x == 0 || x == 66 || x == 88) && 132 ≤ y && y ≤ 176
  if centralRoot && !onGrass then return .cut
  let (x, y) := if centralRoot then
    if left && right then (88, 132 + variant)
    else if left then (66, 132 + variant)
    else if right then (0, 132 + variant)
    else (0, variant)
    else (x, y)
  if leftBranch x y then return if right then .keep else .cut
  if rightBranch x y then return if left then .keep else .cut
  if below.id.isNone then return .cut
  if above || y ≥ 198 then return .reframe x y
  if !left && !right then return .reframe 110 variant
  let crown := if onTrunk then 132 else 154
  let row := if left && right then 132 else if right then 66 else 0
  return .reframe crown (row + variant)

/-- `CheckTileBreakability` for trees: not under a chest-like tile, not on wall 350. -/
def canCut (cx cy : Int) (c : Cell) : M Bool := do
  let above := (← getCell cx (cy - 1)).id
  return c.wall != 350 && !above.any (fun id => (#[21, 26, 72, 77, 88, 467, 488] : Array UInt16).contains id)

/-- `KillTile` of one tree cell: dust (no draws for 5), `KillTile_GetTreeDrops`, `NewItem`s, clear, frame. -/
def killTree (hk : Hooks) (env : Kill.Env) (cx cy : Int) : M Unit := do
  let c ← getCell cx cy
  let some (x, y) := c.frame | return
  if !(← canCut cx cy c) then return
  let ground ← treeGround cx cy x y
  let forest := ground == some 2 || ground == some 477
  if !forest then note s!"KillTile({cx},{cy}): tree on ground {ground} (non-forest wood) not ported"
  let seed ← if x ≥ 22 && y ≥ 198 then draw 2 else pure 1
  let roll ← draw 35
  let bonus ← if roll ≤ env.heldAxe then pure true else (do let v ← draw 3; pure (v == 0))
  Kill.newItem cx cy 9 (if bonus then 2 else 1)
  if seed == 0 && forest then Kill.newItem cx cy 27 1
  setCell cx cy { c with id := none, actuated := false, shape := .full, frame := none }
  squareTileFrame hk cx cy true

/-- `TileFrame` → `CheckTree` for a tree cell (the `Hooks.frameTile` dispatch). -/
def frameTree (hk : Hooks) (env : Kill.Env) (cx cy : Int) : M Unit := do
  let c ← getCell cx cy
  if !isTree c then return
  let some (x, y) := c.frame | return
  match ← check cx cy x y with
  | .keep => pure ()
  | .cut => killTree hk env cx cy
  | .reframe sx sy =>
    if sx == x && sy == y then return
    setCell cx cy { c with frame := some (sx, sy) }
    if sx != x && sy != y then
      for (dx, dy) in [((-1 : Int), (0 : Int)), (1, 0), (0, -1), (0, 1)] do
        tileFrame hk (cx + dx) (cy + dy)

end Lean4Terria.OriginalLiquid.Tree
