import Lean4Terria.OriginalRecipeCatalog
import Lean4Terria.OriginalRecipeCatalog.CraftData

namespace Lean4Terria.OriginalRecipeCatalog

open OriginalCrafting (Result Storage consumeFrom)
open OriginalItems (Inventory Stack)

private def prefixPools : Std.HashMap UInt16 (Array UInt8) :=
  CraftData.pools.foldl (fun table (items, pool) =>
    items.foldl (fun table item => table.insert item pool) table) {}

private def rejectedPrefixes : Std.HashMap UInt16 (Array UInt8) :=
  CraftData.rejected.foldl (fun table (item, prefixes) => table.insert item prefixes) {}

private def labels : Std.HashMap Nat String × Std.HashMap Nat String := Id.run do
  let mut items := {}
  let mut groups := {}
  for line in CraftData.labels.splitOn "\n" do
    let kind :: id :: name := line.splitOn "|" | continue
    let some id := id.toNat? | continue
    let name := String.intercalate "|" name
    if kind == "I" then items := items.insert id name
    else groups := groups.insert id name
  return (items, groups)

def itemName (item : Nat) : String := labels.1.getD item ""
def groupName (group : Nat) : String := labels.2.getD group ""

def Recipe.output (recipe : Recipe) : Stack := .ofItem recipe.create.toUInt16 recipe.createStack

/-- `Item.Prefix(-1)`. The runtime rejects some rolls after rounding their effects
against the item's stats. Such a rejection starts over at the one-in-four roll. -/
def rollNaturalPrefix (item : UInt16) (rng : UnifiedRandom) : UInt8 × UnifiedRandom := Id.run do
  let some pool := prefixPools.get? item | return (0, rng)
  let rejected := rejectedPrefixes.getD item #[]
  let mut rng := rng
  repeat
    let (plain, next) := rng.next 0 4
    if plain == 0 then return (0, next)
    let (index, next) := next.next 0 pool.size
    let chosen := pool[index.toNat]!
    rng := next
    if CraftData.reducedNaturalChance.contains chosen then
      let (keep, next) := rng.next 0 3
      rng := next
      if keep != 0 then return (0, rng)
    if !rejected.contains chosen then return (chosen, rng)
  return (0, rng)

private def Recipe.grant (recipe : Recipe) (rng : UnifiedRandom) : Stack × UnifiedRandom :=
  -- Item.OnCreated gives the Shellphone in its default destination mode.
  let stack := if recipe.create == 5437 then Stack.ofItem 5358 else recipe.output
  if stack.count > 1 then (stack, rng)
  else
    let (prefixId, rng) := rollNaturalPrefix stack.item rng
    ({ stack with prefixId }, rng)

private def discount (entries : Array Entry) (alchemy : Bool) (rng : UnifiedRandom) :
    Array Entry × UnifiedRandom := Id.run do
  if !alchemy then return (entries, rng)
  let mut rng := rng
  let mut discounted := #[]
  for entry in entries do
    let mut cost := entry.stack
    for _ in [:entry.stack] do
      let (roll, next) := rng.next 0 3
      rng := next
      if roll == 0 then cost := cost - 1
    if cost != 0 then discounted := discounted.push { entry with stack := cost }
  return (discounted, rng)

/-- Local desktop `CraftingRequests.CraftItem` and `CraftItem_GrantItem`.
Each iteration checks the full cost, draws alchemy discounts before consuming,
and grants one recipe's output. The batch stops if the cursor fills. -/
def Catalog.craft (catalog : Catalog) (env : Env) (inventory : Inventory) (mouseItem : Option Stack)
    (storage : Storage) (index requested : Nat) (rng : UnifiedRandom) : Result × UnifiedRandom := Id.run do
  let mut result : Result := { inventory, mouseItem, chests := storage.chests, banks := storage.banks }
  let some recipe := catalog.recipes[index]? | return (result, rng)
  let some entries := catalog.lookups[index]? | return (result, rng)
  if !meets env recipe then return (result, rng)
  let output := recipe.output
  let trailing := inventory.extract 58 inventory.size
  let mut carried := inventory.extract 0 58
  let mut storage := storage
  let mut owned := catalog.owned carried storage
  let mut rng := rng
  for _ in [:requested] do
    let held := result.mouseItem.filter (·.count > 0)
    let fits := held.all fun stack => stack.canStack output && !prefixPools.contains stack.item &&
      stack.count + output.count ≤ OriginalItemStats.maxStack stack.item
    if !fits || timesCraftable owned entries == 0 then break
    let (cost, afterDiscount) := discount entries (recipe.alchemy && env.alchemyTable) rng
    let mut consumption := some (carried, storage)
    for entry in cost do
      let some (items, boxes) := consumption | break
      consumption := consumeFrom items boxes (catalog.entryMatches entry) entry.stack
    let some (remaining, boxes) := consumption | break
    carried := remaining
    storage := boxes
    for entry in cost do
      owned := owned.insert entry.id (owned.getD entry.id 0 - entry.stack)
    let (stack, afterGrant) := recipe.grant afterDiscount
    rng := afterGrant
    let stack := { stack with count := stack.count + (held.map (·.count)).getD 0 }
    result := {
      inventory := carried ++ trailing, mouseItem := some stack
      chests := storage.chests, banks := storage.banks, crafted := true }
  return (result, rng)

private def skyblockLowTiles (terrain : OriginalWorld.Terrain) : Bool := Id.run do
  if !terrain.header.skyblock then return false
  let mut count := 0
  for column in terrain.columns do
    let mut start := 0
    for run in column do
      if run.tile.id.isSome then count := count + (run.endY - start)
      start := run.endY
  let size := terrain.header.width * terrain.header.height
  return count.toFloat32 / size.toFloat32 < 0.1

/-- The snow and graveyard fields of `SceneMetrics.ScanTiles` and
`AggregateTileCounts`. Read each terrain run's overlap with the 169×124
scan region rather than looking up every cell separately. -/
def biomeFlagsAt (terrain : OriginalWorld.Terrain) (body : OriginalPlayerCollision.Body) :
    Bool × Bool := Id.run do
  let centerX := OriginalCollision.tileIndex (body.position.x + body.width.toFloat32 / 2)
  let centerY := OriginalCollision.tileIndex (body.position.y + body.height.toFloat32 / 2)
  let x := min (terrain.header.width - 1 : Nat) centerX.toNat
  let y := min (terrain.header.height - 1 : Nat) centerY.toNat
  let top := y - 62
  let bottom := min terrain.header.height (y + 62)
  let mut snow := 0
  let mut grave := 0
  let mut sunflowers := 0
  for column in terrain.columns.extract (x - 84) (x + 85) do
    let mut start := 0
    for run in column do
      if start ≥ bottom then break
      let length := min bottom run.endY - max top start
      if length > 0 then
        match run.tile.id with
        | some 27 => sunflowers := sunflowers + length
        | some 85 => grave := grave + length
        | some 147 | some 148 | some 161 | some 162 | some 163 | some 164 | some 200 =>
          snow := snow + length
        | _ => pure ()
      start := run.endY
  let snowy := snow ≥ 1500 || (snow ≥ 300 && skyblockLowTiles terrain)
  return (snowy, grave - sunflowers / 2 ≥ 28)

end Lean4Terria.OriginalRecipeCatalog
