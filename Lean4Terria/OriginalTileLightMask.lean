import Init

namespace Lean4Terria.OriginalTileLightMask

structure Tile where
  active : Bool
  typeId : UInt16
  invisibleBlock : Bool
  inActive : Bool
  slope : UInt8
  liquid : UInt8
  lava : Bool
  honey : Bool
  deriving Repr, BEq, DecidableEq

/-- Native static tables are initialized and non-null. Array bounds faults are modeled at actual lookup sites. -/
structure Context where
  tileBlockLight : Array Bool
  crackedBricks : Array Bool
  drawInvisibleWalls : Bool
  deriving Repr

inductive Fault
  | nullReference
  | indexOutOfRange
  deriving Repr, BEq, DecidableEq

private def lookupBool (a : Array Bool) (i : Nat) : Except Fault Bool :=
  if h : i < a.size then
    .ok a[i]
  else
    .error .indexOutOfRange

def lightIsBlocked (ctx : Context) (tile : Option Tile) : Except Fault Bool := do
  let t ← match tile with
    | some t => .ok t
    | none => .error .nullReference
  if !t.active then
    return false
  let blocks ← lookupBool ctx.tileBlockLight t.typeId.toNat
  if !blocks then
    return false
  if t.invisibleBlock then
    return ctx.drawInvisibleWalls
  return true

def getTileMask (ctx : Context) (tile : Option Tile) : Except Fault UInt8 := do
  let blocked ← lightIsBlocked ctx tile
  let t ← match tile with
    | some t => .ok t
    | none => .error .nullReference
  if blocked && t.typeId != 131 && !t.inActive && t.slope == 0 then
    let cracked ← lookupBool ctx.crackedBricks t.typeId.toNat
    if cracked then
      return 4
    return 1
  if t.lava || t.liquid.toNat <= 128 then
    return 0
  if t.honey then
    return 3
  return 2

/-- The native GetMaskMode coordinate/global-array behavior is intentionally not
modeled here.  The native provider obtains one tile from Main.tile and then
calls getTileMask; direct grid-provider tests are separate from this DTO. -/
def getMaskMode
    (ctx : Context)
    (provider : Int32 → Int32 → Except Fault (Option Tile))
    (x y : Int32) : Except Fault UInt8 := do
  let tile ← provider x y
  getTileMask ctx tile

end Lean4Terria.OriginalTileLightMask
