import Lean4Terria.OriginalCollision

/-!
Terraria 1.4.5.8 line-of-sight checks used by melee hits, ported from the shipped IL:

* `Collision.CanHit(Point,int,int,Point,int,int)` (MethodDef 209) and the `Vector2`
  overload (MethodDef 208, which converts both positions with `Utils.ToPoint`);
* `Collision.CanHitLine(Vector2,int,int,Vector2,int,int)` (MethodDef 213);
* `Player.CanHit(Entity)` (MethodDef 3179);
* `Projectile.CanHitWithMeleeWeapon(Entity)` (MethodDef 3479).

World conventions (`Main.maxTilesX = grid.width`, `Main.maxTilesY = grid.height`):

* tile indices are clamped to `1 ≤ x ≤ maxTilesX - 1` and `1 ≤ y ≤ maxTilesY - 40`
  (each clamp first raises values `≤ 1` to `1`, then lowers values `≥ max` to the max);
* `Main.tile` is allocated larger than the world (8401 × 2401) but cells outside the
  loaded world are `null`; a `null` cell makes both `CanHit` and `CanHitLine` return
  `false`, and an `IndexOutOfRangeException` is caught by their `catch` handler, which
  also returns `false`. Both cases are modelled by `cell?` returning `none` for any
  index outside `0 ≤ x < width`, `0 ≤ y < height`. Inside the world the model has no
  `null` cells.
* `grid.rules id` must give `Main.tileSolid[id]` / `Main.tileSolidTop[id]` at call time.

Integer arithmetic follows C# `int` (division truncates toward zero); the arguments are
assumed to lie in the `int` range. `float` arithmetic is `Float32`; `conv.i4` truncates
toward zero (`Float32.toInt32`).
-/

namespace Lean4Terria.OriginalStarterMelee.LineOfSight

open OriginalWorld (Tile TileShape)
open OriginalCollision (Grid Vector single slopeId)

/-- The `Entity` fields these checks read. -/
structure Box where
  /-- `Entity.position` (top-left corner, world pixels). -/
  position : Vector
  /-- `Entity.width` in pixels. -/
  width : Int
  /-- `Entity.height` in pixels. -/
  height : Int
  deriving BEq, Repr

/-- `Entity.Center`: `position + ((float)width / 2f, (float)height / 2f)`. -/
def Box.center (b : Box) : Vector :=
  ⟨b.position.x + single b.width / 2, b.position.y + single b.height / 2⟩

/-- `Main.tile[x, y]`, or `none` for a `null` cell / an index the game array would reject. -/
private def cell? (grid : Grid) (x y : Int) : Option Tile :=
  if 0 ≤ x && x < grid.width && 0 ≤ y && y < grid.height then
    some (grid.tileAt x.toNat y.toNat)
  else none

/-- `!inActive() && active() && Main.tileSolid[type] && !Main.tileSolidTop[type]`. -/
private def solidBlock (grid : Grid) (t : Tile) : Bool :=
  match t.id with
  | none => false
  | some id =>
    let r := grid.rules id
    !t.actuated && r.solid && !r.solidTop

/-- `solidBlock` and also `slope() == 0 && !halfBrick()` (the neighbour pair test of `CanHit`). -/
private def fullBlock (grid : Grid) (t : Tile) : Bool :=
  solidBlock grid t && slopeId t.shape == 0 && t.shape != .halfBlock

private def clampX (grid : Grid) (v : Int) : Int :=
  let v := if v ≤ 1 then 1 else v
  if v ≥ grid.width then grid.width - 1 else v

private def clampY (grid : Grid) (v : Int) : Int :=
  let v := if v ≤ 1 then 1 else v
  if v ≥ (grid.height : Int) - 40 then (grid.height : Int) - 40 else v

/-- `Collision.CanHit(Point pos1, int w1, int h1, Point pos2, int w2, int h2)`.
`x1 y1` / `x2 y2` are the two `Point`s (world pixels), `w h` the entity sizes. The tile
walk starts at the tile of `(x1 + w1/2, y1 + h1/2)` and steps one tile at a time along the
dominant axis toward the tile of the second centre; a step is blocked when both tiles
beside it across the other axis are full solid blocks (no slope, no half brick), or when
the reached tile is a solid, non-platform block. -/
def canHitPoints (grid : Grid) (x1 y1 w1 h1 x2 y2 w2 h2 : Int) : Bool := Id.run do
  let mut ax := clampX grid ((x1 + w1.tdiv 2).tdiv 16)
  let mut ay := clampY grid ((y1 + h1.tdiv 2).tdiv 16)
  let bx := clampX grid ((x2 + w2.tdiv 2).tdiv 16)
  let by_ := clampY grid ((y2 + h2.tdiv 2).tdiv 16)
  -- every iteration moves one tile closer (|dx| + |dy| decreases by one)
  for _ in [0:(ax - bx).natAbs + (ay - by_).natAbs + 1] do
    let dx := (ax - bx).natAbs
    let dy := (ay - by_).natAbs
    if ax == bx && ay == by_ then return true
    if dx > dy then
      ax := if ax < bx then ax + 1 else ax - 1
      let some up := cell? grid ax (ay - 1) | return false
      let some down := cell? grid ax (ay + 1) | return false
      if fullBlock grid up && fullBlock grid down then return false
    else
      ay := if ay < by_ then ay + 1 else ay - 1
      let some left := cell? grid (ax - 1) ay | return false
      let some right := cell? grid (ax + 1) ay | return false
      if fullBlock grid left && fullBlock grid right then return false
    let some here := cell? grid ax ay | return false
    if solidBlock grid here then return false
  return true

/-- `Utils.ToPoint`: `new Point((int)v.X, (int)v.Y)`. -/
def toPoint (v : Vector) : Int × Int := (v.x.toInt32.toInt, v.y.toInt32.toInt)

/-- `Collision.CanHit(Vector2 pos1, int w1, int h1, Vector2 pos2, int w2, int h2)`
(MethodDef 208): both positions truncated with `Utils.ToPoint`, then `canHitPoints`.
`a` / `b` supply `position`, `width`, `height` (as `Collision.CanHit(Entity, Entity)` does). -/
def canHit (grid : Grid) (a b : Box) : Bool :=
  let (x1, y1) := toPoint a.position
  let (x2, y2) := toPoint b.position
  canHitPoints grid x1 y1 a.width a.height x2 y2 b.width b.height

/-- `(int)((v + (float)(size / 2)) / 16f)`. -/
private def lineTile (v : Float32) (size : Int) : Int :=
  ((v + single (size.tdiv 2)) / 16).toInt32.toInt

/-- `Collision.CanHitLine(Vector2 pos1, int w1, int h1, Vector2 pos2, int w2, int h2)`.
Walks a stair-step line from the tile of `pos1 + (w1/2, h1/2)` to the tile of
`pos2 + (w2/2, h2/2)`. Every visited tile and, for each step, its two neighbours across
the step (vertical neighbours for horizontal steps, horizontal neighbours for vertical
steps) must not be solid non-platform blocks (`!inActive && active && tileSolid &&
!tileSolidTop`; slopes and half bricks block like full blocks). -/
def canHitLine (grid : Grid) (p1 : Vector) (w1 h1 : Int) (p2 : Vector) (w2 h2 : Int) : Bool := Id.run do
  let mut x := clampX grid (lineTile p1.x w1)
  let mut y := clampY grid (lineTile p1.y h1)
  let x2 := clampX grid (lineTile p2.x w2)
  let y2 := clampY grid (lineTile p2.y h2)
  let dxf : Float32 := single (x - x2).natAbs
  let dyf : Float32 := single (y - y2).natAbs
  if dxf == 0 && dyf == 0 then return true
  let mut stepX : Float32 := 1
  let mut stepY : Float32 := 1
  if dxf == 0 || dyf == 0 then
    if dxf == 0 then stepX := 0
    if dyf == 0 then stepY := 0
  else if dxf > dyf then stepX := dxf / dyf
  else stepY := dyf / dxf
  let mut accX : Float32 := 0
  let mut accY : Float32 := 0
  -- 2: horizontal steps (taken first when the line goes down), 1: vertical steps
  let mut mode : Nat := if y < y2 then 2 else 1
  let mut remX : Int := dxf.toInt32.toInt
  let mut remY : Int := dyf.toInt32.toInt
  let sx : Int := if x2 > x then 1 else if x2 < x then -1 else 0
  let sy : Int := if y2 > y then 1 else if y2 < y then -1 else 0
  let mut arrived := false
  let mut lastStep := false
  -- Every round except at most one moves at least one tile in a fixed direction, and
  -- leaving the world returns `false`, so this bound is never reached.
  for _ in [0:2 * (grid.width + grid.height) + (x - x2).natAbs + (y - y2).natAbs + 8] do
    if mode == 2 then
      accX := accX + stepX
      let n := accX.toInt32.toInt
      accX := accX - single n
      for _ in [0:n.toNat] do
        let some up := cell? grid x (y - 1) | return false
        let some down := cell? grid x (y + 1) | return false
        let some here := cell? grid x y | return false
        if solidBlock grid up || solidBlock grid down || solidBlock grid here then return false
        if remX == 0 && remY == 0 then
          arrived := true
          break
        x := x + sx
        remX := remX - 1
        if remX == 0 && remY == 0 && n == 1 then lastStep := true
      if remY != 0 then mode := 1
    else if mode == 1 then
      accY := accY + stepY
      let n := accY.toInt32.toInt
      accY := accY - single n
      for _ in [0:n.toNat] do
        let some left := cell? grid (x - 1) y | return false
        let some right := cell? grid (x + 1) y | return false
        let some here := cell? grid x y | return false
        if solidBlock grid left || solidBlock grid right || solidBlock grid here then return false
        if remX == 0 && remY == 0 then
          arrived := true
          break
        y := y + sy
        remY := remY - 1
        if remX == 0 && remY == 0 && n == 1 then lastStep := true
      if remX != 0 then mode := 2
    let some here := cell? grid x y | return false
    if solidBlock grid here then return false
    if arrived || lastStep then return true
  return false

/-- `(direction * width / 2, ...)`: the horizontal offset of the player's front edge. -/
private def frontOffset (player : Box) (direction : Int) : Float32 :=
  single ((direction * player.width).tdiv 2)

/-- `Player.CanHit(Entity ent)`: `Collision.CanHit(this, ent)`, or one of three
`Collision.CanHitLine` rays (all with zero sizes) succeeds:
1. from `Center + (direction*width/2, gravDir * (float)(-height) / 3f)` to
   `ent.Center + (0, (float)(-ent.height / 3))`;
2. from the same point to `ent.Center`;
3. from `Center + (direction*width/2, 0)` to `ent.Center + (0, (float)(ent.height / 3))`.
`player`: the player's `position`/`width`/`height`; `direction`: `Entity.direction`
(±1); `gravDir`: `Player.gravDir` (1 or -1); `target`: the entity being hit. -/
def playerCanHit (grid : Grid) (player : Box) (direction : Int) (gravDir : Float32) (target : Box) : Bool :=
  canHit grid player target ||
    let c := player.center
    let tc := target.center
    let front := frontOffset player direction
    let eye : Vector := ⟨c.x + front, c.y + gravDir * single (-player.height) / 3⟩
    canHitLine grid eye 0 0 ⟨tc.x + 0, tc.y + single ((-target.height).tdiv 3)⟩ 0 0 ||
    canHitLine grid eye 0 0 tc 0 0 ||
    canHitLine grid ⟨c.x + front, c.y + 0⟩ 0 0 ⟨tc.x + 0, tc.y + single (target.height.tdiv 3)⟩ 0 0

/-- `Vector2.Distance`: `(float)Math.Sqrt((double)(dx * dx + dy * dy))` with float `dx dy`
(the correctly rounded double square root rounds to the float square root). -/
def distance (a b : Vector) : Float32 :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  (dx * dx + dy * dy).sqrt

/-- `Projectile.CanHitWithMeleeWeapon(Entity ent)`: `false` when
`Distance(projectile.Center, ent.Center) > ownerHitCheckDistance` (a NaN comparison
continues), otherwise exactly `playerCanHit` for the owner (`Main.player[owner]`).
`projectile`: the projectile's `position`/`width`/`height` (only its centre is used);
`ownerHitCheckDistance`: `Projectile.ownerHitCheckDistance`; `owner`, `ownerDirection`,
`ownerGravDir`: the owner's box, `direction` and `gravDir`; `target`: the entity hit. -/
def projectileCanHitWithMeleeWeapon (grid : Grid) (projectile : Box) (ownerHitCheckDistance : Float32)
    (owner : Box) (ownerDirection : Int) (ownerGravDir : Float32) (target : Box) : Bool :=
  if distance projectile.center target.center > ownerHitCheckDistance then false
  else playerCanHit grid owner ownerDirection ownerGravDir target

end Lean4Terria.OriginalStarterMelee.LineOfSight
