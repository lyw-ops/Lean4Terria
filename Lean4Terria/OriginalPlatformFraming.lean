import Lean4Terria.OriginalWorld

/-!
# Platform frames (Terraria 1.4.5.8 Classic)

Hand translation of the `TileID.Sets.Platforms` branch of `WorldGen.TileFrameImportant`
(IL_0b62–IL_1130), the platform part of `WorldGen.SquareTileFrame`, and the platform branch of
`TileDrawing.DrawBasicTile`.

**Frame.** Only `frameX` is written; `frameY` is the style row (`style · 18`) and stays. The new
`frameX` is a function of the cell and its eight neighbours (type, half brick, slope, echo coating)
and of `ForbidsSloping` on the cell above:

1. *left* / *right* values: −1 for air; 1 for `Main.tileStone` (gem stones); the platform's own type
   for any platform type; else the neighbour's type. A value whose cell is culled
   (`GetTileMergeCulling`: its echo coating differs from the centre's, unless invisible blocks are
   shown) or that is not `tileSolid` becomes −1. A platform neighbour whose half brick differs from
   the centre's becomes −1; on a half platform every other solid becomes −1. An empty left side takes
   an up-left platform of the same type with slope 1 (an empty right side: up-right with slope 2).
   A left platform with slope 2 drops a right side that is not the type; a right platform with
   slope 1 drops a left side that is not the type.
2. *slope 1*: 468 when the right cell is a flat full platform (any platform type) or the cell above
   forbids sloping; else 360 / 432 when the down-right cell is air or culled or a platform with
   slope 2, 180 / 396 otherwise — the first value when the left cell is an unculled tile or the
   up-left cell is an unculled slope-1 platform.
   *slope 2* mirrors it with 450, 342 / 414, 144 / 378.
3. otherwise (flat or half): both sides the type → 252 / 216 / 234 / 0 by left slope 2 and right
   slope 1; left only → 270 (left slope 2) / 18; right only → 288 (right slope 1) / 36; other solid
   left, type right → 54; type left, other solid right → 72; other solid left, nothing right → 108;
   nothing left, other solid right → 126; else 90.

Neighbours outside the world (vanilla: a null tile) leave the frame unchanged. Air cells count as
type 0, which is what `KillTile` and world loading leave behind.

**Refresh.** `SquareTileFrame(x, y)` frames the 3 × 3 column by column (x−1, x, x+1; each y−1 … y+1).
A platform frame reads its neighbours' type and shape only, never their frames, so the order does
not change platform frames; platforms outside the 3 × 3 keep their frames.

**Drawing.** A sloped platform is *not* drawn through the generic slope slicing
(`HasSlopeFrames` / sliced path): the whole 16 × 16 frame is drawn, and a "stair foot" is added in the
cell below (`stairFoot`). Half platforms skip the half-brick neighbour adjustments of
`DrawBasicTile`.
-/

namespace Lean4Terria.OriginalPlatformFraming

open OriginalWorld (Terrain Tile TileShape)

/-! ## Tables (`native/work/pf_tables.json`, read from the initialised 1.4.5.8 runtime) -/

/-- `TileID.Sets.Platforms` -/
def platformTypes : Array Nat := #[19, 427, 435, 436, 437, 438, 439]
/-- `Main.tileStone` (gem stones) -/
def stoneTypes : Array Nat := #[63, 64, 65, 66, 67, 68, 130, 131, 566]
/-- `TileID.Sets.BlocksStairs` -/
def blocksStairsTypes : Array Nat := #[54, 328, 386, 387, 459, 541]
/-- `TileID.Sets.FrameImportantEchoCulling` (`TileFrame(…, noBreak)` still frames these) -/
def echoCullingTypes : Array Nat := #[19, 380, 427, 435, 436, 437, 438, 439]
/-- `Main.tileSolid` -/
def solidRanges : Array (Nat × Nat) := #[
    (0, 2), (6, 10), (19, 19), (22, 23), (25, 25), (30, 30), (37, 41), (43, 48), (53, 54), (56, 60),
    (63, 68), (70, 70), (75, 76), (107, 109), (111, 112), (116, 123), (127, 127), (130, 130), (137, 138),
    (140, 140), (145, 148), (150, 164), (166, 170), (175, 177), (179, 183), (188, 200), (202, 204),
    (206, 206), (208, 208), (211, 211), (221, 226), (229, 230), (232, 232), (234, 235), (239, 239),
    (248, 253), (255, 268), (272, 274), (284, 284), (311, 313), (315, 315), (321, 322), (325, 329),
    (345, 348), (350, 350), (357, 357), (367, 371), (379, 381), (383, 385), (387, 388), (396, 404),
    (407, 409), (415, 418), (421, 422), (426, 427), (430, 439), (446, 448), (458, 460), (472, 474),
    (476, 479), (481, 484), (492, 492), (495, 496), (498, 498), (500, 503), (507, 508), (512, 517),
    (534, 537), (539, 541), (546, 546), (557, 557), (562, 563), (566, 566), (618, 618), (625, 628),
    (633, 633), (635, 635), (641, 641), (659, 659), (661, 662), (664, 664), (666, 692), (708, 708),
    (711, 719), (722, 722), (726, 732), (734, 750)]

def isPlatform (id : Nat) : Bool := platformTypes.contains id
def isSolid (id : Nat) : Bool := solidRanges.any fun (a, b) => a ≤ id && id ≤ b
/-- `WorldGen.ForbidsSloping(x, y)` reads the type only. -/
def forbidsSloping (id : Nat) : Bool := [21, 26, 77, 88, 235, 237, 441, 467, 468, 470, 475, 488, 597].contains id

/-! ## Cells -/

/-- What the framing reads of one cell: `none` outside the world (vanilla: a null tile). -/
structure Cell where
  active : Bool
  /-- 0 for air (`KillTile` clears the type) -/
  type : Nat
  half : Bool
  slope : Nat
  invisible : Bool
  deriving BEq, Repr

def cellOf (t : Tile) : Cell :=
  { active := t.id.isSome, type := (t.id.map (·.toNat)).getD 0, half := t.shape == .halfBlock,
    slope := match t.shape with | .slope k => k.toNat | _ => 0, invisible := t.invisibleBlock }

def cellAt (terrain : Terrain) (x y : Int) : Option Cell := do
  guard (x ≥ 0 && y ≥ 0)
  return cellOf (← terrain.tile? x.toNat y.toNat)

/-- The eight neighbours, named as in the IL. -/
structure Around where
  up : Cell
  down : Cell
  left : Cell
  right : Cell
  downLeft : Cell
  downRight : Cell
  upLeft : Cell
  upRight : Cell

/-- `WorldGen.GetTileMergeCulling`: a neighbour is culled when its echo coating differs from the
centre's, unless invisible blocks are shown (`Main.ShouldShowInvisibleBlocksAndWalls`). -/
structure Cull where
  left : Bool
  right : Bool
  upLeft : Bool
  upRight : Bool
  downLeft : Bool
  downRight : Bool

def cull (showInvisible : Bool) (c : Cell) (n : Around) : Cull :=
  let d (o : Cell) := !showInvisible && o.invisible != c.invisible
  { left := d n.left, right := d n.right, upLeft := d n.upLeft, upRight := d n.upRight,
    downLeft := d n.downLeft, downRight := d n.downRight }

/-! ## The frame -/

/-- The Sets.Platforms branch of `TileFrameImportant` for an active platform `c` of type `c.type`:
the new `frameX`. -/
def frameX (c : Cell) (n : Around) (showInvisible : Bool := false) : Nat := Id.run do
  let type := c.type
  let k := cull showInvisible c n
  let side (o : Cell) (culled : Bool) : Int :=
    if !o.active then -1
    else
      let v : Int := if stoneTypes.contains o.type then 1 else if isPlatform o.type then type else o.type
      if v ≥ 0 && culled then -1
      else if v ≥ 0 && !isSolid v.toNat then -1
      else v
  let mut right := side n.right k.right
  let mut left := side n.left k.left
  let t : Int := type
  if left == t && n.left.half != c.half then left := -1
  if right == t && n.right.half != c.half then right := -1
  if left != -1 && left != t && c.half then left := -1
  if right != -1 && right != t && c.half then right := -1
  if left == -1 && n.upLeft.active && n.upLeft.type == type && n.upLeft.slope == 1 && !k.upLeft then left := t
  if right == -1 && n.upRight.active && n.upRight.type == type && n.upRight.slope == 2 && !k.upRight then right := t
  if left == t && n.left.slope == 2 && right != t then right := -1
  if right == t && n.right.slope == 1 && left != t then left := -1
  let forbids := forbidsSloping n.up.type
  if c.slope == 1 then
    if isPlatform n.right.type && n.right.slope == 0 && !n.right.half && !k.right then return 468
    if forbids then return 468
    let openBelow := (!n.downRight.active || k.downRight) &&
      (!isPlatform n.downRight.type || n.downRight.slope == 2 || k.downRight)
    let leftHeld := (n.left.active && !k.left) || (isPlatform n.upLeft.type && n.upLeft.slope == 1 && !k.upLeft)
    return if openBelow then (if leftHeld then 360 else 432) else (if leftHeld then 180 else 396)
  if c.slope == 2 then
    if isPlatform n.left.type && n.left.slope == 0 && !n.left.half && !k.left then return 450
    if forbids then return 450
    let openBelow := (!n.downLeft.active || k.downLeft) &&
      (!isPlatform n.downLeft.type || n.downLeft.slope == 1 || k.downLeft)
    let rightHeld := (n.right.active && !k.right) || (isPlatform n.upRight.type && n.upRight.slope == 2 && !k.upRight)
    return if openBelow then (if rightHeld then 342 else 414) else (if rightHeld then 144 else 378)
  if left == t && right == t then
    if n.left.slope == 2 && n.right.slope == 1 then return 252
    if n.left.slope == 2 then return 216
    if n.right.slope == 1 then return 234
    return 0
  if left == t && right == -1 then return if n.left.slope == 2 then 270 else 18
  if left == -1 && right == t then return if n.right.slope == 1 then 288 else 36
  if left != t && right == t then return 54
  if left == t && right != t then return 72
  if left != t && left != -1 && right == -1 then return 108
  if left == -1 && right != t && right != -1 then return 126
  return 90

def around (terrain : Terrain) (x y : Int) : Option Around := do
  return { up := ← cellAt terrain x (y - 1), down := ← cellAt terrain x (y + 1), left := ← cellAt terrain (x - 1) y,
           right := ← cellAt terrain (x + 1) y, downLeft := ← cellAt terrain (x - 1) (y + 1),
           downRight := ← cellAt terrain (x + 1) (y + 1), upLeft := ← cellAt terrain (x - 1) (y - 1),
           upRight := ← cellAt terrain (x + 1) (y - 1) }

/-- The new frame of the platform at (x, y): `(frameX, frameY)`, with `frameY` (the style row) kept.
`none` when the cell is not an active platform or a neighbour lies outside the world. -/
def platformFrame (terrain : Terrain) (x y : Int) (showInvisible : Bool := false) : Option (Nat × Int) := do
  guard (x ≥ 0 && y ≥ 0)
  let tile ← terrain.tile? x.toNat y.toNat
  let c := cellOf tile
  guard (c.active && isPlatform c.type)
  let n ← around terrain x y
  return (frameX c n showInvisible, (tile.frame.map (·.2)).getD 0)

/-- `WorldGen.TileFrame(x, y)` for a platform cell: stores the new frame in `Tile.frame`. -/
def framePlatform (terrain : Terrain) (x y : Int) (showInvisible : Bool := false) : Terrain :=
  match platformFrame terrain x y showInvisible with
  | none => terrain
  | some (fx, fy) =>
    match terrain.tile? x.toNat y.toNat with
    | none => terrain
    | some t => (terrain.replaceTile x.toNat y.toNat { t with frame := some (fx, fy) }).getD terrain

/-- The platform part of `WorldGen.SquareTileFrame(x, y)`: column x−1, x, x+1, each y−1 … y+1. -/
def squarePlatformFrame (terrain : Terrain) (x y : Int) (showInvisible : Bool := false) : Terrain := Id.run do
  let mut terrain := terrain
  for dx in [(-1 : Int), 0, 1] do
    for dy in [(-1 : Int), 0, 1] do
      terrain := framePlatform terrain (x + dx) (y + dy) showInvisible
  return terrain

/-! ## Drawing (`TileDrawing.DrawBasicTile`, platform branch) -/

private def blockType (c : Cell) : Nat := if c.half then 1 else if c.slope > 0 then c.slope + 1 else 0

/-- The "stair foot" a sloped platform at (x, y) adds one cell below it (`Layer_BehindTiles`):
the source rectangle `(srcX, frameY, 16, 16)` drawn at (x, y + 1). Slope 1 looks at the
down-right cell (198, or 324 when it is a flat platform), slope 2 at the down-left cell
(162, or 306). The foot is dropped when that cell is air, invisible (unless shown), not solid,
sloped the other way or a half brick, or when the cell right below is a visible `BlocksStairs` tile
that is full (or slope 4 / 3 respectively). -/
def stairFoot (terrain : Terrain) (x y : Int) (showInvisible : Bool := false) : Option Nat := do
  let tile ← terrain.tile? x.toNat y.toNat
  let c := cellOf tile
  guard (c.active && isPlatform c.type && c.slope > 0)
  guard (c.slope ≤ 2)
  let below ← cellAt terrain x (y + 1)
  let side ← cellAt terrain (if c.slope == 1 then x + 1 else x - 1) (y + 1)
  let (other, footBlock, plain, flat) := if c.slope == 1 then (2, 5, 198, 324) else (1, 4, 162, 306)
  guard (side.active && (showInvisible || !side.invisible) && isSolid side.type && side.slope != other && !side.half)
  let belowBlocks := below.active && (showInvisible || !below.invisible) &&
    (blockType below == 0 || blockType below == footBlock) && blocksStairsTypes.contains below.type
  guard (!belowBlocks)
  return if isPlatform side.type && side.slope == 0 then flat else plain

end Lean4Terria.OriginalPlatformFraming
