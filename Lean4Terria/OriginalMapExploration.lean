/-!
# Map exploration of desktop Terraria 1.4.5.8 (`Terraria.exe` sha256 `d52c7ea7…03e0`)

Hand transcription of the real desktop CIL (dumps in `.lake/original-map-exploration/harness/
dumpsel-*/il/`) of the map-tile state machine and the tile → map-tile → colour conversion:

* `Terraria.Map.MapTile` (struct `Type : UInt16`, `Light : Byte`, `_extraData : Byte`): `.ctor`,
  `Create` (`_extraData = color | 128`), `WithLight`, `Clear`, `Equals` (light, type, colour —
  the changed / queued bits are ignored), `EqualsWithoutLight`, `get/set_Color` (bits 0‥4),
  `get/set_UpdateQueued` (bit 6), `get/set_IsChanged` (bit 7).
* `Terraria.Map.WorldMap` (`MaxWidth`, `MaxHeight`, `_tiles : MapTile[,]`): `.ctor`, `get_Item`,
  `SetTile`, `Update`, `UpdateLighting`, `IsRevealed`, `QueueUpdate` (both overloads),
  `UpdateType` (both overloads, the queue consumer), `ConsumeUpdate`, `Clear`.
* `Terraria.Map.MapHelper`: `CreateMapTile`, `GetTileType`, `GetWallType`, `GetBackgroundType`
  (sky gradient, remix, snow scan of the map / `sceneSnowiness`, dirt / rock / hell),
  `CalcSkyGradient`, `IsBackground`, `GetMapTileXnaColor`, `MapColor` (paint), plus
  `WorldGen.paintColor`, `WorldSections.TileLoaded` / `SectionLoaded`, `Netplay.GetSection{X,Y}`,
  `Utils.Clamp<float>` (`Single.CompareTo`), `Main.UnderworldLayer`, and the `Tile` bit accessors.

Inputs, never defaulted:

* `Lookups`: the tables `MapHelper.Initialize` builds (`tileLookup`, `wallLookup`, `colorLookup`,
  `snowTypes`, the positions, `wallRangeStart/End`) and `WallID.Count`. `Initialize` is 12 154 IL
  lines of colour constants; its output is data the caller supplies (natively captured in the
  harness) — this module does not invent it.
* `Env`: `Main.worldSurface`, `rockLayer`, `maxTilesY`, `remixWorld`, `WorldGen.generatingWorld`,
  `DebugOptions.ShowUnbreakableWall`, `MapHelper.sceneArea`, `MapHelper.sceneSnowiness`.
* `Sections`: `Main.sectionManager` (width, height, bit 0 of every `BitsByte`).
* `Callees m`: `Main.tile[x, y]` (null = `none`), `WorldGen.IsThisAMushroomTree` (tile 5 only),
  and `MapHelper.GetTileBaseOption` for the 62 tile types its `switch` handles; every other tile
  type takes the `switch` default (`baseOption = 0`, `IL_0ecf`).
* Light bytes are plain `UInt8` arguments (the lighting engine is not a dependency).

Floats: `Float32` with single rounding per operation (shipped Mono, both slices), `Float`
for the `double` comparisons and the sky gradient.
-/
namespace Lean4Terria.OriginalMapExploration

/-! ## Faults -/

inductive Fault where
  /-- `IndexOutOfRangeException` on the named array (`MapTile[,]`, a lookup table, …). -/
  | indexOutOfRange (array : String) (detail : String)
  /-- `conv.u1` of a float outside `(-1, 256)` or NaN: runtime-dependent natively. -/
  | runtimeDependentConversion (value : Float)
  /-- `new MapTile[w, h]` with a negative dimension (`OverflowException`). -/
  | negativeDimension (width height : Int32)
  /-- An exception raised by a callee (provider-defined). -/
  | callee (name : String) (detail : String)
deriving Repr, Inhabited

/-! ## `Terraria.Tile` (the fields the map reads) -/

structure Tile where
  type : UInt16
  wall : UInt16
  liquid : UInt8
  sTileHeader : UInt16
  bTileHeader : UInt8
  bTileHeader3 : UInt8
  frameX : Int16
  frameY : Int16
deriving Inhabited, Repr, BEq

namespace Tile
def active (t : Tile) : Bool := t.sTileHeader &&& 32 == 32
def invisibleBlock (t : Tile) : Bool := t.bTileHeader3 &&& 32 == 32
def fullbrightBlock (t : Tile) : Bool := t.bTileHeader3 &&& 128 == 128
def invisibleWall (t : Tile) : Bool := t.bTileHeader3 &&& 64 == 64
def fullbrightWall (t : Tile) : Bool := t.sTileHeader &&& 32768 == 32768
def color (t : Tile) : UInt8 := (t.sTileHeader &&& 31).toUInt8
def wallColor (t : Tile) : UInt8 := t.bTileHeader &&& 31
def liquidType (t : Tile) : UInt8 := (t.bTileHeader &&& 96) >>> 5
end Tile

/-! ## `Terraria.Map.MapTile` -/

structure MapTile where
  type : UInt16
  light : UInt8
  extraData : UInt8
deriving Inhabited, Repr, BEq

namespace MapTile
def empty : MapTile := ⟨0, 0, 0⟩
def color (t : MapTile) : UInt8 := t.extraData &&& 31
def setColor (t : MapTile) (c : UInt8) : MapTile := { t with extraData := (t.extraData &&& 224) ||| (c &&& 31) }
def updateQueued (t : MapTile) : Bool := t.extraData &&& 64 != 0
def setUpdateQueued (t : MapTile) (b : Bool) : MapTile :=
  { t with extraData := if b then t.extraData ||| 64 else t.extraData &&& 191 }
def isChanged (t : MapTile) : Bool := t.extraData &&& 128 != 0
def setIsChanged (t : MapTile) (b : Bool) : MapTile :=
  { t with extraData := if b then t.extraData ||| 128 else t.extraData &&& 127 }
/-- `MapTile.Create(type, light, color)`: the new tile is marked changed. -/
def create (type : UInt16) (light color : UInt8) : MapTile := ⟨type, light, color ||| 128⟩
/-- `WithLight(light)`: same type and colour bits, marked changed. -/
def withLight (t : MapTile) (light : UInt8) : MapTile := ⟨t.type, light, t.extraData ||| 128⟩
/-- `Equals(MapTile)`: light, type and colour; the queued / changed bits are not compared. -/
def equals (a b : MapTile) : Bool := a.light == b.light && a.type == b.type && a.color == b.color
def equalsWithoutLight (a b : MapTile) : Bool := a.type == b.type && a.color == b.color
end MapTile

/-! ## Inputs -/

/-- `MapHelper` tables produced by `MapHelper.Initialize()` and `WallID.Count`. -/
structure Lookups where
  tileLookup : Array UInt16
  wallLookup : Array UInt16
  /-- `colorLookup` as packed FNA colours (`R | G << 8 | B << 16 | A << 24`). -/
  colorLookup : Array UInt32
  snowTypes : Array UInt16
  liquidPosition : UInt16
  skyPosition : UInt16
  dirtPosition : UInt16
  rockPosition : UInt16
  hellPosition : UInt16
  wallRangeStart : UInt16
  wallRangeEnd : UInt16
  wallCount : UInt16
deriving Inhabited, Repr

structure Rect where
  x : Int32
  y : Int32
  width : Int32
  height : Int32
deriving Inhabited, Repr, BEq

/-- FNA `Rectangle.Contains(int, int)`. -/
def Rect.contains (r : Rect) (px py : Int32) : Bool :=
  r.x ≤ px && px < r.x + r.width && r.y ≤ py && py < r.y + r.height

structure Env where
  worldSurface : Float
  rockLayer : Float
  maxTilesY : Int32
  remixWorld : Bool
  generatingWorld : Bool
  showUnbreakableWall : Bool
  sceneArea : Rect
  sceneSnowiness : Float32
deriving Inhabited, Repr

/-- `Main.sectionManager`: `width`, `height` and bit 0 (`SectionLoaded`) of each `data` entry. -/
structure Sections where
  width : Int32
  height : Int32
  loaded : Array Bool
deriving Inhabited, Repr

/-- Tile types for which `MapHelper.GetTileBaseOption`'s dispatch reaches a case body (parsed
from `IL_0000‥IL_02df`); all other types reach `IL_0ecf: baseOption = 0`. -/
def specialBaseOptionTypes : Array Int32 :=
  #[4, 15, 19, 21, 26, 27, 28, 31, 80, 82, 83, 84, 89, 105, 129, 133, 134, 137, 149, 160, 165, 178,
    184, 185, 186, 187, 227, 240, 242, 419, 420, 423, 428, 440, 441, 453, 457, 461, 467, 468, 493,
    518, 519, 529, 530, 548, 560, 572, 591, 597, 627, 628, 647, 648, 649, 650, 653, 692, 693, 694,
    695, 696]

structure Callees (m : Type → Type) where
  /-- `Main.tile[x, y]` (`none` = null; faults beyond the array). -/
  tileAt : Int32 → Int32 → m (Option Tile)
  /-- `WorldGen.IsThisAMushroomTree(x, y)` (`GetTreeType(x, y) == 3`); only for tile type 5. -/
  isMushroomTree : Int32 → Int32 → m Bool
  /-- `MapHelper.GetTileBaseOption(x, y, type, tile, ref option)` for `specialBaseOptionTypes`. -/
  tileBaseOption : Int32 → Int32 → Int32 → Tile → m Int32

variable {m : Type → Type} [Monad m] [MonadExceptOf Fault m]

@[inline] def lookup {α : Type} [Inhabited α] (name : String) (a : Array α) (i : Int) : m α :=
  if 0 ≤ i && i < a.size then pure a[i.toNat]! else
    throw (Fault.indexOutOfRange name s!"{i} of {a.size}")

/-! ## `Terraria.Map.WorldMap` -/

structure WorldMap where
  maxWidth : Int32
  maxHeight : Int32
  /-- `_tiles[x, y]` at `x * MaxHeight + y` (row-major on the last dimension). -/
  tiles : Array MapTile
deriving Inhabited

namespace WorldMap

/-- `.ctor(maxWidth, maxHeight)`. -/
def create (w h : Int32) : Except Fault WorldMap :=
  if w < 0 || h < 0 then .error (.negativeDimension w h)
  else .ok { maxWidth := w, maxHeight := h, tiles := Array.replicate (w.toInt.toNat * h.toInt.toNat) MapTile.empty }

def index (map : WorldMap) (x y : Int32) : m Nat :=
  if x < 0 || x ≥ map.maxWidth || y < 0 || y ≥ map.maxHeight then
    throw (Fault.indexOutOfRange "MapTile[,]" s!"{x},{y}")
  else pure (x.toInt.toNat * map.maxHeight.toInt.toNat + y.toInt.toNat)

/-- `this[x, y]` (`get_Item`). -/
def get (map : WorldMap) (x y : Int32) : m MapTile := do
  let i ← map.index x y
  pure map.tiles[i]!

/-- `SetTile(x, y, ref tile)` / `_tiles[x, y] = tile`. -/
def set (map : WorldMap) (x y : Int32) (t : MapTile) : m WorldMap := do
  let i ← map.index x y
  let tiles := map.tiles
  let map := { map with tiles := #[] }
  pure { map with tiles := tiles.set! i t }

/-- `IsRevealed(x, y)`: `Light > 0`. -/
def isRevealed (map : WorldMap) (x y : Int32) : m Bool := do
  return (← map.get x y).light > 0

/-- `Clear()`: every tile `Type = Light = _extraData = 0`. -/
def clear (map : WorldMap) : WorldMap :=
  { map with tiles := Array.replicate map.tiles.size MapTile.empty }

end WorldMap

/-! ## `MapHelper` -/

/-- `conv.u1` of a `double` / widened `single`: truncation for values in `(-1, 256)`. -/
def convU1 (v : Float) : m UInt8 :=
  if v.isNaN || v ≤ -1 || v ≥ 256 then throw (Fault.runtimeDependentConversion v)
  else pure (if v < 0 then 0 else v.toUInt8)

/-- `Single.CompareTo` (NaN orders below everything, equal to NaN). -/
def compareSingle (a b : Float32) : Int :=
  if a < b then -1 else if a > b then 1 else if a == b then 0
  else if a.isNaN then (if b.isNaN then 0 else -1) else 1

/-- `Utils.Clamp<float>(value, min, max)`. -/
def clampSingle (v lo hi : Float32) : Float32 :=
  if compareSingle v hi > 0 then hi else if compareSingle v lo < 0 then lo else v

/-- `MapHelper.CalcSkyGradient(start, count, y)`. -/
def calcSkyGradient (env : Env) (start : UInt16) (count y : Int32) : m Int32 := do
  let b ← convU1 (Float.ofInt (count - 1).toInt * (Float.ofInt y.toInt / env.worldSurface))
  return start.toNat.toInt32 + b.toNat.toInt32

/-- `MapHelper.IsBackground(type)`: `type >= skyPosition`. -/
def isBackground (lk : Lookups) (type : Int32) : Bool := !(type < lk.skyPosition.toNat.toInt32)

/-- The `ref` locals `CreateMapTile` passes to `GetTileType` / `GetWallType`. -/
structure Refs where
  color : Int32
  newLight : Int32
  baseType : Int32
  baseOption : Int32
deriving Inhabited, Repr

/-- `MapHelper.GetTileType(x, y, tile, ref color, ref newLight, ref baseType, ref baseOption)`. -/
def getTileType (c : Callees m) (env : Env) (lk : Lookups) (x y : Int32) (tile : Tile) (r : Refs) : m Refs := do
  if env.showUnbreakableWall && tile.wall == 350 then return r
  if !tile.active then return r
  let mut type : Int32 := tile.type.toNat.toInt32
  let mut r := { r with baseType := (← lookup "tileLookup" lk.tileLookup type.toInt).toNat.toInt32 }
  let invisible := tile.invisibleBlock
  if tile.fullbrightBlock && !invisible then r := { r with newLight := 255 }
  if invisible then return { r with baseType := 0 }
  if type == 5 then
    if ← c.isMushroomTree x y then r := { r with baseOption := 1 }
    return { r with color := tile.color.toNat.toInt32 }
  if type ≤ 51 then
    if type == 19 then
      if tile.frameY.toInt32 == 864 then r := { r with baseType := 0 }
    else if type == 51 then
      if (x + y) % 2 == 0 then r := { r with baseType := 0 }
  else
    if type == 184 then
      if tile.frameX.toInt32 / 22 == 10 then
        type := 627
        r := { r with baseType := (← lookup "tileLookup" lk.tileLookup 627).toNat.toInt32 }
    else if type == 697 then
      if (x + y) % 2 == 0 then r := { r with baseType := 0 }
  if r.baseType == 0 then return r
  let opt ← if specialBaseOptionTypes.contains type then c.tileBaseOption x y type tile else pure 0
  r := { r with baseOption := opt }
  if type == 160 then return { r with color := 0 }
  return { r with color := tile.color.toNat.toInt32 }

/-- `MapHelper.GetWallType(x, y, tile, ref color, ref newLight, ref baseType, ref baseOption)`. -/
def getWallType (lk : Lookups) (x : Int32) (tile : Tile) (r : Refs) : m Refs := do
  let invisible := tile.invisibleWall
  let mut r := r
  if tile.wall > 0 && tile.fullbrightWall && !invisible then r := { r with newLight := 255 }
  if tile.liquid > 32 then
    return { r with baseType := lk.liquidPosition.toNat.toInt32 + tile.liquidType.toNat.toInt32 }
  if invisible || !(tile.wall > 0) || !(tile.wall < lk.wallCount) then return r
  let wall : Int32 := tile.wall.toNat.toInt32
  r := { r with baseType := (← lookup "wallLookup" lk.wallLookup wall.toInt).toNat.toInt32,
                color := tile.wallColor.toNat.toInt32 }
  if wall == 21 || (wall ≥ 88 && wall ≤ 93) || wall == 168 || wall == 241 then return { r with color := 0 }
  if wall == 27 then return { r with baseOption := x % 2 }
  return { r with baseOption := 0 }

/-- `MapHelper.GetBackgroundType(x, y, ref newLight)` reading `Main.Map` (= `map`). -/
def getBackgroundType (env : Env) (lk : Lookups) (map : WorldMap) (x y : Int32) (newLight : Int32) :
    m (Int32 × Int32) := do
  if Float.ofInt y.toInt < env.worldSurface then
    if env.remixWorld then return (100, 5)
    return (← calcSkyGradient env lk.skyPosition 256 y, 255)
  if !(y < env.maxTilesY - 200) then return (lk.hellPosition.toNat.toInt32, newLight)
  let mut num : Int32 := 0
  if !env.generatingWorld then
    if !env.sceneArea.contains x y then
      let mut i := x - 36
      while i ≤ x + 30 do
        let mut j := y - 36
        while j ≤ y + 30 do
          let t := (← map.get i j).type
          for s in lk.snowTypes do
            if s == t then
              num := 255
              i := x + 31
              j := y + 31
              break
          j := j + 10
        i := i + 10
    else
      num := (← convU1 (clampSingle env.sceneSnowiness 0 1 * 255).toFloat).toNat.toInt32
  if Float.ofInt y.toInt < env.rockLayer then return (lk.dirtPosition.toNat.toInt32 + num, newLight)
  return (lk.rockPosition.toNat.toInt32 + num, newLight)

/-- `MapHelper.CreateMapTile(i, j, light, baseType)`. -/
def createMapTile (c : Callees m) (env : Env) (lk : Lookups) (map : WorldMap) (x y : Int32)
    (light : UInt8) (baseType : Int32) : m MapTile := do
  let some tile ← c.tileAt x y | return MapTile.empty
  let mut r : Refs := { color := 0, newLight := light.toNat.toInt32, baseType := 0, baseOption := 0 }
  r ← getTileType c env lk x y tile r
  if r.baseType == 0 then r ← getWallType lk x tile r
  if r.baseType == 0 then
    r := { r with color := 0, newLight := light.toNat.toInt32 }
    if baseType != 0 then r := { r with baseType }
    else
      let (bt, nl) ← getBackgroundType env lk map x y r.newLight
      r := { r with baseType := bt, newLight := nl }
  return MapTile.create (r.baseType + r.baseOption).toUInt32.toUInt16 r.newLight.toUInt32.toUInt8 r.color.toUInt32.toUInt8

/-! ## Colours -/

/-- FNA `Color` channels of a packed value. -/
def chR (p : UInt32) : UInt8 := p.toUInt8
def chG (p : UInt32) : UInt8 := (p >>> 8).toUInt8
def chB (p : UInt32) : UInt8 := (p >>> 16).toUInt8
def withR (p : UInt32) (v : UInt8) : UInt32 := (p &&& 0xFFFFFF00) ||| v.toUInt32
def withG (p : UInt32) (v : UInt8) : UInt32 := (p &&& 0xFFFF00FF) ||| (v.toUInt32 <<< (8 : UInt32))
def withB (p : UInt32) (v : UInt8) : UInt32 := (p &&& 0xFF00FFFF) ||| (v.toUInt32 <<< (16 : UInt32))
def withA (p : UInt32) (v : UInt8) : UInt32 := (p &&& 0x00FFFFFF) ||| (v.toUInt32 <<< (24 : UInt32))
def rgba (r g b a : UInt8) : UInt32 := r.toUInt32 ||| (g.toUInt32 <<< (8 : UInt32)) ||| (b.toUInt32 <<< (16 : UInt32)) ||| (a.toUInt32 <<< (24 : UInt32))

/-- `WorldGen.paintColor(color)`: starts at `Color.White`, then each `if` of the CIL in order. -/
def paintColor (c : Int32) : UInt32 := Id.run do
  let mut p := rgba 255 255 255 255
  let set3 (p : UInt32) (r g b : UInt8) : UInt32 := withB (withG (withR p r) g) b
  if c == 1 || c == 13 then p := set3 p 255 0 0
  if c == 2 || c == 14 then p := set3 p 255 127 0
  if c == 3 || c == 15 then p := set3 p 255 255 0
  if c == 4 || c == 16 then p := set3 p 127 255 0
  if c == 5 || c == 17 then p := set3 p 0 255 0
  if c == 6 || c == 18 then p := set3 p 0 255 127
  if c == 7 || c == 19 then p := set3 p 0 255 255
  if c == 8 || c == 20 then p := set3 p 0 127 255
  if c == 9 || c == 21 then p := set3 p 0 0 255
  if c == 10 || c == 22 then p := set3 p 127 0 255
  if c == 11 || c == 23 then p := set3 p 255 0 255
  if c == 12 || c == 24 then p := set3 p 255 0 127
  if c == 25 then p := set3 p 75 75 75
  if c == 26 then p := set3 p 255 255 255
  if c == 27 then p := set3 p 175 175 175
  if c == 28 then p := set3 p 255 178 125
  if c == 29 then p := set3 p 25 25 25
  if c == 30 then p := withA (set3 p 200 200 200) 150
  return p

/-- `(byte)(single)`: `conv.u1` of a `Single`. -/
def u1 (v : Float32) : m UInt8 := convU1 v.toFloat

/-- `MapHelper.MapColor(type, ref oldColor, colorType)`. -/
def mapColor (lk : Lookups) (type : UInt16) (old : UInt32) (colorType : UInt8) : m UInt32 := do
  let paint := paintColor colorType.toNat.toInt32
  let mut r : Float32 := (chR old).toNat.toFloat32 / 255
  let mut g : Float32 := (chG old).toNat.toFloat32 / 255
  let mut b : Float32 := (chB old).toNat.toFloat32 / 255
  if g > r then
    let t := r
    r := g
    g := t
  if b > r then
    let t := r
    r := b
    b := t
  if colorType == 29 then
    let k := b * 0.3
    let c := withR old (← u1 ((chR paint).toNat.toFloat32 * k))
    let c := withG c (← u1 ((chG paint).toNat.toFloat32 * k))
    return withB c (← u1 ((chB paint).toNat.toFloat32 * k))
  if colorType == 30 then
    if !(type < lk.wallRangeStart) && !(type > lk.wallRangeEnd) then
      let c := withR old (← u1 ((255 - (chR old).toNat.toInt32).toInt.toFloat32 * 0.5))
      let c := withG c (← u1 ((255 - (chG c).toNat.toInt32).toInt.toFloat32 * 0.5))
      return withB c (← u1 ((255 - (chB c).toNat.toInt32).toInt.toFloat32 * 0.5))
    let c := withR old (255 - chR old)
    let c := withG c (255 - chG c)
    return withB c (255 - chB c)
  let c := withR old (← u1 ((chR paint).toNat.toFloat32 * r))
  let c := withG c (← u1 ((chG paint).toNat.toFloat32 * r))
  return withB c (← u1 ((chB paint).toNat.toFloat32 * r))

/-- `MapHelper.GetMapTileXnaColor(tile, x, y)` (packed FNA colour; `x`, `y` unused natively). -/
def getMapTileXnaColor (lk : Lookups) (t : MapTile) : m UInt32 := do
  let mut c ← lookup "colorLookup" lk.colorLookup t.type.toNat
  if t.color > 0 then c ← mapColor lk t.type c t.color
  if t.light == 255 then return c
  let l : Float32 := t.light.toNat.toFloat32 / 255
  let c2 := withR c (← u1 ((chR c).toNat.toFloat32 * l))
  let c3 := withG c2 (← u1 ((chG c2).toNat.toFloat32 * l))
  return withB c3 (← u1 ((chB c3).toNat.toFloat32 * l))

/-! ## `WorldMap` updates -/

/-- `WorldSections.TileLoaded(x, y)` = `SectionLoaded(x / 200, y / 150)`. -/
def tileLoaded (s : Sections) (x y : Int32) : m Bool := do
  let sx := x / 200
  let sy := y / 150
  if sx < 0 || !(sx < s.width) then return false
  if sy < 0 || !(sy < s.height) then return false
  lookup "WorldSections.data" s.loaded (sy * s.width + sx).toInt

namespace WorldMap

/-- `Update(x, y, light)`: `_tiles[x, y] = CreateMapTile(x, y, light, 0)` (unconditional). -/
def update (c : Callees m) (env : Env) (lk : Lookups) (map : WorldMap) (x y : Int32) (light : UInt8) : m WorldMap := do
  let t ← createMapTile c env lk map x y light 0
  map.set x y t

/-- `UpdateLighting(x, y, light)`: nothing when both lights are 0; otherwise
`CreateMapTile(x, y, Max(old.Light, light), 0)`, stored only when not `Equals` the old tile. -/
def updateLighting (c : Callees m) (env : Env) (lk : Lookups) (map : WorldMap) (x y : Int32) (light : UInt8) :
    m (Bool × WorldMap) := do
  let old ← map.get x y
  if light == 0 && old.light == 0 then return (false, map)
  let t ← createMapTile c env lk map x y (max old.light light) 0
  if t.equals old then return (false, map)
  return (true, ← map.set x y t)

/-- `QueueUpdate(x, y)`: false for an unrevealed or already queued tile, else marks it queued. -/
def queueUpdate (map : WorldMap) (x y : Int32) : m (Bool × WorldMap) := do
  let t ← map.get x y
  if t.light == 0 || t.updateQueued then return (false, map)
  return (true, ← map.set x y (t.setUpdateQueued true))

/-- `UpdateType(x, y)`: the queue consumer. Clears the queued bit (kept even when it then returns
false), skips unrevealed tiles and unloaded sections, recreates the tile with its own light (a
background type is passed as `baseType`), stores it when not `Equals`. -/
def updateType (c : Callees m) (env : Env) (lk : Lookups) (sec : Sections) (map : WorldMap) (x y : Int32) :
    m (Bool × WorldMap) := do
  let t ← map.get x y
  if !t.updateQueued then return (false, map)
  let t := t.setUpdateQueued false
  let map ← map.set x y t
  if t.light == 0 then return (false, map)
  if !(← tileLoaded sec x y) then return (false, map)
  let bg := isBackground lk t.type.toNat.toInt32
  let n ← createMapTile c env lk map x y t.light (if bg then t.type.toNat.toInt32 else 0)
  if n.equals t then return (false, map)
  return (true, ← map.set x y n)

/-- `ConsumeUpdate(x, y)`: clears the changed bit. -/
def consumeUpdate (map : WorldMap) (x y : Int32) : m WorldMap := do
  let t ← map.get x y
  map.set x y (t.setIsChanged false)

end WorldMap

end Lean4Terria.OriginalMapExploration
