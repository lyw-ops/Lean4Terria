import Lean4Terria.OriginalLiquid.Core

/-!
# Large piles (186, 187): `WorldGen.Check3x2` as `TileFrame` reaches it

Lava burning grass under a pile (`Liquid.DelWater` → `SquareTileFrame`) frames the pile, and
`TileFrameImportant` sends 186 / 187 to `WorldGen.Check3x2` (IL `integration/probe/il/WorldGen.Check3x2.short`):

1. the object's six cells must be this type with the expected frames (origin from `frameX / 18 mod 3`,
   `frameY mod 36`);
2. each of the three tiles below must pass `SolidTileAllowBottomSlope` and not be a boulder
   (`InvalidTileForPilesOrSpeleothems`); some styles also need snow / ice, mud, jungle, hell or sand ground;
3. a 187 whose top-left `frameX` is 756–900 (the grassy piles) with none of 2 / 477 / 492 below becomes
   the matching 186: every cell `frameX -= 378`, type 186 — no kill, no draws;
4. if 1. or 2. failed, the object is destroyed (`KillTile` of all six cells with drops): reported, not
   ported.
-/

namespace Lean4Terria.OriginalLiquid.Piles

open Lean4Terria.OriginalLiquid

/-- `WorldGen.SolidTileAllowBottomSlope` (platform top frames are taken as proper) -/
def solidTileAllowBottomSlope (x y : Int) : M Bool := do
  let s ← get
  if !(x ≥ 0 && y ≥ 0 && x < s.maxTilesX && y < s.maxTilesY) then return true
  let t := s.cell x y
  if !t.active then return false
  if !(solid s.ignore t.type || solidTop t.type) then return false
  let topSlope := t.slope == 1 || t.slope == 2
  if topSlope && !(OriginalTileRules.collision t.type.toUInt16).platform then return false
  return !t.halfBrick && !t.actuated

def boulder (type : Nat) : Bool := [138, 484, 664, 711, 712, 713, 714, 715, 716].contains type

/-- ground sets used by the style rules (`TileID.Sets` / `TileID.Sets.Conversion`) -/
def snowOrIce (t : Nat) : Bool := [147, 148, 161, 162, 163, 164, 200, 224].contains t
def mud (t : Nat) : Bool := [59, 60, 70, 226, 661, 662].contains t
def hell (t : Nat) : Bool := [57, 58, 75, 76].contains t
def sandy (t : Nat) : Bool := [53, 112, 116, 234, 396, 397, 398, 399, 400, 401, 402, 403].contains t

def checkPile3x2 (x y : Int) : M Unit := do
  let c ← getCell x y
  let type := c.type
  let some (fx, fy) := c.frame | return
  let col := (fx / 18) % 3
  let style := (fx / 18) / 3
  let x0 := x - col
  let row := (fy % 36) / 18
  let y0 := y - row
  let baseX := style * 54
  let baseY := (fy / 36) * 36
  let mut broken := false
  for cx in [x0, x0 + 1, x0 + 2] do
    for cy in [y0, y0 + 1] do
      let t ← getCell cx cy
      if !(t.active && t.type == type && t.frame == some ((cx - x0) * 18 + baseX, (cy - y0) * 18 + baseY)) then
        broken := true
  let st := style.toNat
  for cx in [x0, x0 + 1, x0 + 2] do
    let g ← getCell cx (y0 + 2)
    if !(← solidTileAllowBottomSlope cx (y0 + 2)) || (g.active && boulder g.type) then broken := true
    else if g.active then
      if type == 186 then
        if 26 ≤ st && st ≤ 31 && !snowOrIce g.type then broken := true
        if 32 ≤ st && st ≤ 34 && !mud g.type then broken := true
      if type == 187 then
        if st ≤ 5 && !mud g.type then broken := true
        if 6 ≤ st && st ≤ 8 && !hell g.type then broken := true
        if 29 ≤ st && st ≤ 34 && !sandy g.type then broken := true
  if type == 187 then
    let top ← getCell x0 y0
    let tfx := (top.frame.getD (0, 0)).1
    if 756 ≤ tfx && tfx ≤ 900 then
      let mut grass := false
      for cx in [x0, x0 + 1, x0 + 2] do
        if [2, 477, 492].contains (← getCell cx (y0 + 2)).type then grass := true
      if !grass then
        for cx in [x0, x0 + 1, x0 + 2] do
          for cy in [y0, y0 + 1] do
            let t ← getCell cx cy
            setCell cx cy { t with id := some 186, frame := t.frame.map fun (a, b) => (a - 378, b) }
  if broken then
    note s!"Check3x2({x0},{y0}) type {type}: object destroyed (KillTile of six cells, drops) not ported"

end Lean4Terria.OriginalLiquid.Piles
