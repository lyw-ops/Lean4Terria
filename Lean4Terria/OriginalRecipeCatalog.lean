import Std.Data.HashMap
import Std.Data.HashSet
import Lean4Terria.OriginalCrafting
import Lean4Terria.OriginalRecipeCatalog.Data

/-!
# The 1.4.5.8 recipe catalog and the available-recipe query (`Recipe.UpdateRecipeList`)

`Data.packed` is the runtime export of all 3,603 recipes of Terraria 1.4.5.8 (`Main.recipe` after
`Recipe.SetupRecipes`), the 34 recipe groups (`RecipeGroup.recipeGroups`), the station inheritance table
(`Recipe.TileCountsAs`) and the water-source tiles (`TileID.Sets.CountsAsWaterForCrafting`); see
`.lake/original-recipe-catalog/1.4.5.8`.

The query is a hand port of `Recipe.UpdateRecipeList` (1.4.5.8 has no `FindRecipes`):

* `CollectItemsToCraftWithFrom`: `_ownedItems` = for each item type, the sum of `stack` over the stacks with
  `stack > 0` in `player.inventory[0..57]` and then every container of `_recipeChests` (the shared
  `OriginalCrafting.Storage` order); then `AddFakeCountsForItemGroups`: each group's fake id
  (`FakeItemIdOffset + id`) gets the sum over its `ValidItems`, even when 0; then
  `CraftingRequests.SubtractPendingRequests` (`pending`, empty in single player).
* Each recipe's `requiredItemQuickLookup` (`CreateRequiredItemQuickLookups`): ingredient `i` keeps its item id
  unless an accepted group contains it, then it is that group's fake id (the last such group).
* `HowManyTimesCanRecipeBeCrafted`: `min (owned id / stack)` over the entries in order, 0 as soon as an id is
  missing or the minimum reaches 0; one stack is counted for every entry and every recipe it matches.
* `PlayerMeetsEnvironmentConditions`: `adjTile[requiredTile]`, `adjWaterSource`, `adjHoney`, `adjLava`,
  `ZoneSnow`, `ZoneGraveyard`, `SpecialSeedFeatures.Mechdusa` (remix and for-the-worthy world),
  `unlockedBiomeTorches`.
* Recipes are scanned in index order (`Main.availableRecipe` ascending), stopping at the first empty one;
  `CraftingUI.RecipeFilterHack` (`filter`) applies only with the crafting grid filter open.
* `Player.AdjTiles` / `SetAdjTile` (`envOfRegion`): every active tile of the reach region sets `adjTile` for
  its type and, recursively, for the types it counts as; water from `CountsAsWaterForCrafting` tiles or a cell
  with more than 200 water; honey and lava from more than 200 of them.
* Guide slot (`CollectGuideRecipes`): every recipe with an entry matching the item, ignoring stations and owned
  items.
-/

namespace Lean4Terria.OriginalRecipeCatalog

structure Ingredient where
  item : Nat
  stack : Nat
  deriving BEq, Repr, Inhabited

/-- The environment flags a recipe asks for (`Recipe.need*`). -/
structure Conditions where
  needWater : Bool := false
  needHoney : Bool := false
  needLava : Bool := false
  needSnowBiome : Bool := false
  needGraveyardBiome : Bool := false
  needMechdusa : Bool := false
  needTorchGodsFavor : Bool := false
  deriving BEq, Repr, Inhabited

structure Recipe where
  index : Nat
  create : Nat
  createStack : Nat
  /-- `requiredItem`, in order (the non-air prefix) -/
  ingredients : Array Ingredient
  /-- `acceptedGroups` (the non-negative prefix) -/
  groups : Array Nat
  /-- `requiredTile`, `none` for −1 -/
  requiredTile : Option Nat
  conditions : Conditions
  /-- decrafting fields, carried for completeness (not used by the query) -/
  alchemy : Bool := false
  notDecraftable : Bool := false
  crimson : Bool := false
  corruption : Bool := false
  customShimmerResults : Array Ingredient := #[]
  deriving BEq, Repr, Inhabited

structure Group where
  id : Nat
  /-- `ValidItems` (`HashSet<int>` enumeration order) -/
  validItems : Array Nat
  /-- `Items` (list order) -/
  items : Array Nat
  placeholderItem : Nat
  decraftItemId : Nat
  deriving BEq, Repr, Inhabited

/-- `Recipe.RequiredItemEntry`: an item id or a group's fake id, and the amount. -/
structure Entry where
  id : Nat
  stack : Nat
  deriving BEq, Repr, Inhabited

structure Catalog where
  fakeItemIdOffset : Nat := 1000000
  maxRecipes : Nat := 0
  maxRequirements : Nat := 0
  recipes : Array Recipe := #[]
  groups : Array Group := #[]
  tileCountsAs : Std.HashMap Nat (Array Nat) := {}
  waterTiles : Array Nat := #[]
  /-- `requiredItemQuickLookup` of every recipe, filled by `parse` -/
  lookups : Array (Array Entry) := #[]
  deriving Inhabited

namespace Catalog

def group? (c : Catalog) (id : Nat) : Option Group := c.groups.find? (·.id == id)

def isGroupId (c : Catalog) (id : Nat) : Bool := id ≥ c.fakeItemIdOffset

/-- `Recipe.CreateRequiredItemQuickLookups` for one recipe -/
def quickLookup (c : Catalog) (r : Recipe) : Array Entry :=
  r.ingredients.map fun ing => Id.run do
    let mut id := ing.item
    for g in r.groups do
      if (c.group? g).any (·.validItems.contains ing.item) then id := c.fakeItemIdOffset + g
    return ⟨id, ing.stack⟩

end Catalog

/-- Parse `Data.packed` (format in `gen_lean_data.py`). -/
def parse (text : String) : Except String Catalog := do
  let mut c : Catalog := {}
  for line in text.splitOn "\n" do
    let words := (line.splitOn " ").filter (· ≠ "")
    let some tag := words.head? | continue
    let nums ← (words.drop 1).toArray.mapM fun w =>
      match w.toInt? with
      | some v => pure v
      | none => throw s!"bad number {w} in {line.take 40}"
    let nat (i : Nat) : Nat := (nums[i]?.getD 0).toNat
    let pairs (start n : Nat) : Array Ingredient := (Array.range n).map fun k => ⟨nat (start + 2 * k), nat (start + 2 * k + 1)⟩
    match tag with
    | "O" => c := { c with fakeItemIdOffset := nat 0, maxRecipes := nat 1, maxRequirements := nat 2 }
    | "R" =>
      let n := nat 6
      let gAt := 7 + 2 * n
      let m := nat gAt
      let kAt := gAt + 1 + m
      let cm := nat 4
      let dm := nat 5
      let bit (mask i : Nat) : Bool := (mask >>> i) % 2 == 1
      c := { c with recipes := c.recipes.push {
        index := nat 0, create := nat 1, createStack := nat 2
        requiredTile := if nums[3]?.getD (-1) < 0 then none else some (nat 3)
        conditions := { needWater := bit cm 0, needHoney := bit cm 1, needLava := bit cm 2, needSnowBiome := bit cm 3,
                        needGraveyardBiome := bit cm 4, needMechdusa := bit cm 5, needTorchGodsFavor := bit cm 6 }
        alchemy := bit dm 0, notDecraftable := bit dm 1, crimson := bit dm 2, corruption := bit dm 3
        ingredients := pairs 7 n
        groups := (Array.range m).map fun k => nat (gAt + 1 + k)
        customShimmerResults := pairs (kAt + 1) (nat kAt) } }
    | "G" =>
      let n := nat 3
      let m := nat (4 + n)
      c := { c with groups := c.groups.push {
        id := nat 0, placeholderItem := nat 1, decraftItemId := nat 2
        validItems := (Array.range n).map fun k => nat (4 + k)
        items := (Array.range m).map fun k => nat (5 + n + k) } }
    | "C" => c := { c with tileCountsAs := c.tileCountsAs.insert (nat 0) ((Array.range (nat 1)).map fun k => nat (2 + k)) }
    | "W" => c := { c with waterTiles := (Array.range (nat 0)).map fun k => nat (1 + k) }
    | _ => throw s!"unknown record {tag}"
  return { c with lookups := c.recipes.map c.quickLookup }

/-- The 1.4.5.8 catalog (parsed once). -/
def catalog : Catalog :=
  match parse Data.packed with
  | .ok c => c
  | .error _ => {}

/-- `Recipe._ownedItems`: item id or group fake id → count. -/
abbrev Owned := Std.HashMap Nat Nat

/-- `Recipe.CollectItems`: add every stack with `stack > 0`. -/
def collect (owned : Owned) (stacks : Array (Nat × Nat)) : Owned :=
  stacks.foldl (fun o (item, stack) => if stack > 0 then o.insert item (o.getD item 0 + stack) else o) owned

private def inventoryStacks (items : OriginalItems.Inventory) : Array (Nat × Nat) :=
  items.filterMap fun slot => slot.map fun s => (s.item.toNat, s.count)

private def chestStacks (chest : OriginalWorld.Chest) : Array (Nat × Nat) :=
  chest.items.filterMap fun slot => slot.map fun s => (s.typeId.toNat, s.count)

namespace Catalog

/-- `AddFakeCountsForItemGroups` -/
def addGroupCounts (c : Catalog) (owned : Owned) : Owned :=
  c.groups.foldl (fun o g =>
    o.insert (c.fakeItemIdOffset + g.id) (g.validItems.foldl (fun n i => n + owned.getD i 0) 0)) owned

/-- `CollectItemsToCraftWithFrom`: the carried slots 0–57 (`OriginalSession.State.carriedItems`), the
containers in `storage.order` (`OriginalSession.State.craftingStorage`), the group counts, then
`SubtractPendingRequests` (`SubtractOwnedItem`: `max 0 (owned − stack)` for present ids). -/
def owned (c : Catalog) (carried : OriginalItems.Inventory) (storage : OriginalCrafting.Storage)
    (pending : Array Entry := #[]) : Owned := Id.run do
  let mut o := collect {} (inventoryStacks (carried.extract 0 58))
  for source in storage.order do
    match source with
    | .chest i => if let some chest := storage.chests[i]? then o := collect o (chestStacks chest)
    | .bank kind => o := collect o (inventoryStacks (storage.banks.items kind))
  o := c.addGroupCounts o
  for e in pending do
    if let some held := o.get? e.id then o := o.insert e.id (held - e.stack)
  return o

end Catalog

/-- `HowManyTimesCanRecipeBeCrafted` -/
def timesCraftable (owned : Owned) (entries : Array Entry) : Nat := Id.run do
  let mut num : Nat := 2147483647
  for e in entries do
    let some held := owned.get? e.id | return 0
    num := min num (held / e.stack)
    if num == 0 then return 0
  return num

/-- What `PlayerMeetsEnvironmentConditions` reads from the player. -/
structure Env where
  adjTile : Std.HashSet Nat := {}
  adjWaterSource : Bool := false
  adjHoney : Bool := false
  adjLava : Bool := false
  alchemyTable : Bool := false
  zoneSnow : Bool := false
  zoneGraveyard : Bool := false
  /-- `SpecialSeedFeatures.Mechdusa` = `Main.remixWorld && Main.getGoodWorld` -/
  mechdusa : Bool := false
  unlockedBiomeTorches : Bool := false
  deriving Inhabited

def meets (env : Env) (r : Recipe) : Bool :=
  r.requiredTile.all env.adjTile.contains &&
  (!r.conditions.needWater || env.adjWaterSource) &&
  (!r.conditions.needHoney || env.adjHoney) &&
  (!r.conditions.needLava || env.adjLava) &&
  (!r.conditions.needSnowBiome || env.zoneSnow) &&
  (!r.conditions.needGraveyardBiome || env.zoneGraveyard) &&
  (!r.conditions.needMechdusa || env.mechdusa) &&
  (!r.conditions.needTorchGodsFavor || env.unlockedBiomeTorches)

/-- One cell of the `Player.AdjTiles` region: the active tile's type (none if inactive), liquid amount and type. -/
structure RegionCell where
  tile : Option Nat
  liquid : Nat := 0
  liquidType : Nat := 0

/-- `TileReachCheckSettings.Simple.GetTileRegion(player, extra)`: the inclusive tile rectangle `Player.AdjTiles`
scans (left, top, right, bottom). `Player.tileRangeX` 5 and `tileRangeY` 3 (multiplier 1, no limit, no override);
`extra` is 4 after an Artisan Loaf (`ateArtisanBread`). Left/top truncate `position / 16` (float32); right/bottom
are `Math.Ceiling((position + size) / 16) − 1` (the float32 quotient widened to double). `WorldUtils.ClampToWorld`
(margin 0) is left to the caller. -/
def reachRegion (x y : Float32) (width height : Nat) (extra : Nat := 0) : Int × Int × Int × Int :=
  let rx : Int := 5 + extra
  let ry : Int := 3 + extra
  let trunc (f : Float32) : Int := (f.toFloat.floor.toUInt64.toNat : Int)
  let ceil (f : Float32) : Int := (f.toFloat.ceil.toUInt64.toNat : Int)
  (trunc (x / 16) - rx, trunc (y / 16) - ry,
   ceil ((x + width.toFloat32) / 16) - 1 + rx, ceil ((y + height.toFloat32) / 16) - 1 + ry)

namespace Catalog

/-- `Player.SetAdjTile` (recursive through `TileCountsAs`; `fuel` bounds the depth). -/
def setAdjTile (c : Catalog) (env : Env) (type : Nat) (fuel : Nat := 16) : Env :=
  match fuel with
  | 0 => env
  | fuel + 1 =>
    let env := { env with adjTile := env.adjTile.insert type,
                          alchemyTable := env.alchemyTable || type == 355 || type == 699 }
    (c.tileCountsAs.getD type #[]).foldl (fun env t => c.setAdjTile env t fuel) env

/-- `Player.AdjTiles` over the cells of the reach region (`TileReachCheckSettings.Simple.GetTileRegion`).
The zone, seed and torch flags are kept from `base`. -/
def envOfRegion (c : Catalog) (cells : Array RegionCell) (base : Env := {}) : Env := Id.run do
  let mut env := { base with adjTile := {}, adjWaterSource := false, adjHoney := false, adjLava := false,
                             alchemyTable := false }
  for cell in cells do
    if let some t := cell.tile then
      env := c.setAdjTile env t
      if c.waterTiles.contains t then env := { env with adjWaterSource := true }
    if cell.liquid > 200 then
      if cell.liquidType == 0 then env := { env with adjWaterSource := true }
      if cell.liquidType == 2 then env := { env with adjHoney := true }
      if cell.liquidType == 1 then env := { env with adjLava := true }
  return env

/-- One map cell as `AdjTiles` sees it: `Tile.active()` (an id, actuated or not), liquid amount and type. -/
def regionCell (tile : Option OriginalWorld.Tile) : RegionCell :=
  match tile with
  | none => { tile := none }
  | some t =>
    let (liquid, liquidType) := match t.liquid with
      | some l => (l.amount.toNat, match l.kind with | .water => 0 | .lava => 1 | .honey => 2 | .shimmer => 3)
      | none => (0, 0)
    { tile := t.id.map (·.toNat), liquid, liquidType }

/-- `Player.AdjTiles` on a map: `reachRegion` of the player's hitbox, clamped to the world, then `envOfRegion`. -/
def envAt (c : Catalog) (terrain : OriginalWorld.Terrain) (x y : Float32) (width height : Nat) (base : Env := {})
    (extra : Nat := 0) : Env := Id.run do
  let (l, t, r, b) := reachRegion x y width height extra
  let w : Int := terrain.header.width
  let h : Int := terrain.header.height
  let mut cells := #[]
  for i in [(max 0 l).toNat : (min (w - 1) r + 1).toNat] do
    for j in [(max 0 t).toNat : (min (h - 1) b + 1).toNat] do
      cells := cells.push (regionCell (terrain.tile? i j))
  return c.envOfRegion cells base

/-- `Recipe.UpdateRecipeList` without a guide item: the indices of the available recipes, ascending. -/
def available (c : Catalog) (env : Env) (owned : Owned) (filter : Option (Recipe → Bool) := none) : Array Nat :=
  Id.run do
    let mut out := #[]
    for h : i in [:c.recipes.size] do
      let r := c.recipes[i]
      if r.create == 0 then break
      if !(filter.all (· r)) then continue
      if meets env r && timesCraftable owned (c.lookups[i]?.getD #[]) > 0 then out := out.push r.index
    return out

/-- `RequiredItemEntry.Matches` -/
def entryMatches (c : Catalog) (e : Entry) (item : Nat) : Bool :=
  if c.isGroupId e.id then (c.group? (e.id - c.fakeItemIdOffset)).any (·.validItems.contains item)
  else e.id == item

/-- `Recipe.CollectGuideRecipes`: recipes with an entry matching the guide item, ascending. -/
def guideRecipes (c : Catalog) (item : Nat) : Array Nat := Id.run do
  let mut out := #[]
  for h : i in [:c.recipes.size] do
    let r := c.recipes[i]
    if r.create == 0 then break
    if (c.lookups[i]?.getD #[]).any (c.entryMatches · item) then out := out.push r.index
  return out

/-- The crafting list as `UpdateRecipeList` builds it: guide recipes when a guide item is set, otherwise the
available recipes for the carried items and the storage sources. -/
def query (c : Catalog) (env : Env) (carried : OriginalItems.Inventory) (storage : OriginalCrafting.Storage)
    (guideItem : Option Nat := none) (pending : Array Entry := #[]) (filter : Option (Recipe → Bool) := none) :
    Array Nat :=
  match guideItem with
  | some item => if item != 0 then c.guideRecipes item else c.available env (c.owned carried storage pending) filter
  | none => c.available env (c.owned carried storage pending) filter

end Catalog

end Lean4Terria.OriginalRecipeCatalog
