import Lean4Terria.OriginalLiquid.Kill

/-!
# Plants and vines when liquid changes their ground (`PlantCheck`, `CheckVines`)

Lava turning grass into dirt or mud (`Liquid.DelWater`), or killing the tile a vine hangs from, frames the
neighbours; `TileFrame` then checks the plants on top and the vines below.

`WorldGen.PlantCheck` (3, 24, 61, 71, 73, 74, 110, 113, 201, 637, 703): the ground is the tile below when it
is `nactive`, not a half brick and not sloped. `PlantCheck_IsBadTypeMatch` decides whether the plant
still fits; if not, `PlantCheck_TryGetNewType` turns it into the ground's variant (frameX adjusted), or,
with no variant for that ground, the plant is killed (`KillTile`, `Kill.killSingleCell`).

`WorldGen.CheckVines` (vines 52, 62, 115, 205, 382, 528, 636, 638): a vine hangs from the tile above
(`nactive`, not bottom-sloped). The same vine continues; another biome's ground or vine turns it into
that biome's vine (`SquareTileFrame(i, j, true)`); no valid support kills it, and the kill frames the
vine below, so a hanging vine falls cell by cell.
IL: `integration/probe/il/WorldGen.PlantCheck*.short`, `WorldGen.CheckVines.short`.
-/

namespace Lean4Terria.OriginalLiquid.Plants

open Lean4Terria.OriginalLiquid

def plantTypes : List Nat := [3, 24, 61, 71, 73, 74, 110, 113, 201, 637, 703]

def isBadTypeMatch (g t : Int) : Bool :=
  if t == 3 && !([2, 477, 78, 380, 579].contains g) then true
  else if t == 73 && !([2, 477, 78, 380, 579].contains g) then true
  else if t == 24 && !([23, 661].contains g) then true
  else if t == 61 && !([60, 226].contains g) then true
  else if t == 74 && !([60, 226].contains g) then true
  else if t == 71 && g != 70 then true
  else if t == 110 && !([109, 492].contains g) then true
  else if t == 113 && !([109, 492].contains g) then true
  else if t == 201 && !([199, 662].contains g) then true
  else if t == 637 then g != 633
  else false

/-- `PlantCheck_TryGetNewType`: `(type, frameX, mushroom)` -/
def tryGetNewType (g : Int) (type : Nat) (frameX : Int) : Nat × Int × Bool := Id.run do
  let mut t := type
  let mut fx := frameX
  let mut flag := false
  if [3, 61, 110, 24].contains t then flag := fx == 144
  if t == 201 then flag := fx == 270
  if (t == 3 || t == 73) && g != 2 && g != 477 && fx ≥ 162 then fx := 126
  if t == 74 && g != 60 && g != 226 && fx ≥ 162 then fx := 126
  if g == 23 || g == 661 then
    t := 24
    if fx ≥ 162 then fx := 126
    return (t, fx, flag)
  if g == 199 || g == 662 then return (201, fx, flag)
  if g == 2 || g == 477 then return (if t == 113 then 73 else 3, fx, flag)
  if g == 109 || g == 492 then return (if t == 73 then 113 else 110, fx, flag)
  if g == 60 || g == 226 then
    while fx > 126 do fx := fx - 126
    return (61, fx, flag)
  if g == 70 then
    while fx > 72 do fx := fx - 72
    return (71, fx, flag)
  return (t, fx, flag)

def plantCheck (hk : Hooks) (env : Kill.Env) (x y : Int) : M Unit := do
  let c ← getCell x y
  let t := c.type
  let below ← getCell x (y + 1)
  let g : Int := if y + 1 ≥ (← get).maxTilesY then t
    else if below.nactive && !below.halfBrick && below.slope == 0 then below.type else -1
  if t == 703 then
    note s!"PlantCheck({x},{y}): type 703 not ported"
    return
  if !isBadTypeMatch g t then return
  let (fx, fy) := c.frame.getD (0, 0)
  let (nt, nfx, mushroom) := tryGetNewType g t fx
  if nt != t then
    let nfx := if mushroom then (if nt == 201 then 270 else 144) else nfx
    setCell x y { c with id := some nt.toUInt16, frame := some (nfx, fy) }
  else if Kill.singleCell.contains t then Kill.killSingleCell hk env x y
  else note s!"PlantCheck({x},{y}): KillTile of type {t} not ported"

def vineTypes : List Nat := [52, 62, 115, 205, 382, 528, 636, 638]

/-- `WorldGen.CheckVines`; returns whether `TileFrame` continues (to `TileFrameCosmetic`). -/
def checkVines (hk : Hooks) (env : Kill.Env) (x y : Int) : M Bool := do
  let c ← getCell x y
  let type : Int := c.type
  let above ← getCell x (y - 1)
  let num : Int := if above.nactive && !(above.slope == 3 || above.slope == 4) then above.type else -1
  if type == num then return true
  let b62 := num == 60 || num == 226 || num == 62
  let b115 := num == 109 || num == 115 || num == 492
  let b636 := num == 23 || num == 636 || num == 661
  let b205 := num == 199 || num == 205 || num == 662
  let b52 := num == 2 || num == 52 || num == 477
  let b382 := num == 382
  let b528 := num == 70 || num == 528
  let b638 := num == 633 || num == 638
  let mut n6 : Int := 0
  if b638 then n6 := 638
  if b528 then n6 := 528
  if b115 then n6 := 115
  if b62 then n6 := 62
  if b636 then n6 := 636
  if b205 then n6 := 205
  if b52 && type != 382 then n6 := 52
  if b382 then n6 := 382
  if n6 != 0 && n6 != type then
    setCell x y { c with id := some n6.toNat.toUInt16 }
    squareTileFrame hk x y true
    return false
  let mut kill := num == -1
  if (type == 52 || type == 382) && !([2, 477, 192].contains num) then kill := true
  if type == 62 && !([60, 384, 226].contains num) then kill := true
  if type == 115 && !([109, 492].contains num) then kill := true
  if type == 528 && num != 70 then kill := true
  if type == 636 && !([23, 661].contains num) then kill := true
  if type == 205 && !([199, 662].contains num) then kill := true
  if type == 638 && num != 633 then kill := true
  if kill then
    Kill.killSingleCell hk env x y
    return false
  return true

end Lean4Terria.OriginalLiquid.Plants
