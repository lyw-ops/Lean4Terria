import Lean4Terria.OriginalBanks

namespace Lean4Terria.OriginalCrafting

open OriginalCollision (tileIndex)
open OriginalBanks (Kind Stores)
open OriginalItems (Inventory Stack)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Chest Terrain)

structure Recipe where
  index : Nat
  output : Stack
  ingredient : Stack
  station : Option UInt16 := none
  deriving BEq, Repr

/-- The first wood and work-bench recipes, in vanilla recipe-table order.
Ingredient IDs here are exact items from recipes without an accepted group. -/
private def recipes : Array Recipe :=
  #[
    ⟨299, .ofItem 94 2, .ofItem 9, none⟩,
    ⟨300, .ofItem 9, .ofItem 94 2, none⟩,
    ⟨939, .ofItem 30 4, .ofItem 2, some 18⟩,
    ⟨973, .ofItem 93 4, .ofItem 9, some 18⟩,
    ⟨974, .ofItem 9, .ofItem 93 4, some 18⟩,
    ⟨1014, .ofItem 36, .ofItem 9 10, none⟩
  ]

private def nearStation (terrain : Terrain) (body : Body) (station : UInt16) : Bool := Id.run do
  let x := tileIndex body.position.x
  let y := tileIndex body.position.y
  for i in [(x - 5).toNat:(x + 8).toNat] do
    for j in [(y - 3).toNat:(y + 7).toNat] do
      if inToolRange body ⟨i, j⟩ 5 3 0 &&
          (terrain.tile? i j).any (fun tile => tile.id == some station) then
        return true
  return false

inductive Source where
  | chest (index : Nat)
  | bank (kind : Kind)
  deriving BEq, Repr

structure Storage where
  chests : Array Chest := #[]
  banks : Stores := {}
  order : Array Source := #[]

private def countInventory (inventory : Inventory) (item : UInt16) : Nat :=
  inventory.foldl (fun count slot =>
    match slot with
    | some stack => if stack.item == item then count + stack.count else count
    | none => count) 0

private def countIngredients (inventory : Inventory) (storage : Storage) (item : UInt16) : Nat :=
  storage.order.foldl (fun count source =>
    match source with
    | .bank kind => count + countInventory (storage.banks.items kind) item
    | .chest index =>
      match storage.chests[index]? with
      | none => count
      | some chest => chest.items.foldl (fun count slot =>
        match slot with
        | some stack => if stack.typeId == item.toNat then count + stack.count else count
        | none => count) count) (countInventory inventory item)

def availableFrom (terrain : Terrain) (body : Body) (inventory : Inventory)
    (storage : Storage) : Array Recipe :=
  recipes.filter fun recipe =>
    recipe.ingredient.count ≤ countIngredients inventory storage recipe.ingredient.item &&
      recipe.station.all (nearStation terrain body)

def available (terrain : Terrain) (body : Body) (inventory : Inventory)
    (chests : Array Chest := #[]) (sources : Array Nat := #[]) : Array Recipe :=
  availableFrom terrain body inventory { chests, order := sources.map .chest }

structure Result where
  inventory : Inventory
  mouseItem : Option Stack
  chests : Array Chest := #[]
  banks : Stores := {}
  crafted : Bool := false

/-- ItemSlot.LeftClick swaps with the cursor before it merges compatible
stacks. A full stack remains separate. World chests clear favorite flags. -/
def clickSlot (inventory : Inventory) (mouseItem : Option Stack) (index : Nat)
    (retainFavorite : Bool := true) : Result := Id.run do
  let some slot := inventory[index]? | return { inventory, mouseItem }
  let clearFavorite (stack : Stack) := { stack with favorite := retainFavorite && stack.favorite }
  let incoming := mouseItem.map clearFavorite
  let outgoing := slot.map clearFavorite
  let swapped : Result := { inventory := inventory.set! index incoming, mouseItem := outgoing }
  let (some stack, some cursor) := (incoming, outgoing) | return swapped
  if !stack.canStack cursor then return swapped
  let favorite := stack.favorite
  let stack := { stack with favorite := cursor.favorite }
  let cursor := { cursor with favorite }
  let limit := OriginalItemStats.maxStack stack.item
  let transfer := if stack.count == limit || cursor.count == limit then 0
    else min cursor.count (limit - stack.count)
  return {
    inventory := inventory.set! index (some { stack with count := stack.count + transfer })
    mouseItem := if transfer == cursor.count then none else some { cursor with count := cursor.count - transfer }
  }

private def consumeInventory (initial : Inventory) (accepts : Nat → Bool) (needed : Nat) :
    Inventory × Nat := Id.run do
  let mut updated := initial
  let mut remaining := needed
  for index in [:initial.size] do
    if remaining == 0 then break
    let some stack := initial[index]?.join | continue
    if !accepts stack.item.toNat then continue
    updated := updated.set! index <| if stack.count ≤ remaining then none
      else some { stack with count := stack.count - remaining }
    remaining := remaining - stack.count
  return (updated, remaining)

/-- Consume one requirement, including recipe groups, in the order used by
`CraftingRequests.Consume`. Return no edits if the source stacks fall short. -/
def consumeFrom (inventory : Inventory) (storage : Storage) (accepts : Nat → Bool) (count : Nat) :
    Option (Inventory × Storage) := Id.run do
  let (edited, needed) := consumeInventory inventory accepts count
  let mut remaining := needed
  let mut chests := storage.chests
  let mut banks := storage.banks
  for source in storage.order do
    if remaining == 0 then break
    match source with
    | .bank kind =>
      let (edited, left) := consumeInventory (banks.items kind) accepts remaining
      if left != remaining then banks := banks.replace kind edited
      remaining := left
    | .chest index =>
      let some chest := chests[index]? | continue
      let mut items := chest.items
      let before := remaining
      for slot in [:items.size] do
        if remaining == 0 then break
        let some stack := items[slot]?.join | continue
        if stack.typeId <= 0 || !accepts stack.typeId.toNat then continue
        items := items.set! slot <| if stack.count ≤ remaining then none
          else some { stack with count := stack.count - remaining }
        remaining := remaining - stack.count
      if remaining != before then chests := chests.set! index { chest with items }
  if remaining > 0 then return none
  return some (edited, { storage with chests, banks })

/-- CraftingRequests consumes the carried inventory before any storage, in
slot order. The desktop mouse path grants the result to the cursor. -/
def craftFrom (inventory : Inventory) (mouseItem : Option Stack) (recipe : Recipe)
    (requested : Nat) (storage : Storage) : Result := Id.run do
  let idle : Result := { inventory, mouseItem, chests := storage.chests, banks := storage.banks }
  if !mouseItem.all (·.canStack recipe.output) then return idle
  let heldCount := (mouseItem.map (·.count)).getD 0
  let space := OriginalItemStats.maxStack recipe.output.item - heldCount
  let byOutput := space / recipe.output.count
  let item := recipe.ingredient.item
  let byIngredients := countIngredients inventory storage item / recipe.ingredient.count
  let count := min requested (min byOutput byIngredients)
  if count == 0 then return idle
  let some (edited, storage) := consumeFrom inventory storage (· == item.toNat)
      (count * recipe.ingredient.count) | return idle
  let mouseItem := some { recipe.output with count := heldCount + count * recipe.output.count }
  return { inventory := edited, mouseItem, chests := storage.chests, banks := storage.banks, crafted := true }

def craft (inventory : Inventory) (mouseItem : Option Stack) (recipe : Recipe)
    (requested : Nat := 1) (chests : Array Chest := #[]) (sources : Array Nat := #[]) : Result :=
  craftFrom inventory mouseItem recipe requested { chests, order := sources.map .chest }

end Lean4Terria.OriginalCrafting
