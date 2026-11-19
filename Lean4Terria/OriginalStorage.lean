import Lean4Terria.OriginalItems
import Lean4Terria.OriginalItemSorting

namespace Lean4Terria.OriginalStorage

open OriginalItems (Inventory Stack)
open OriginalItemStats (maxStack)

inductive Action where
  | lootAll | depositAll | quickStack | restock | sortItems
  deriving BEq

def Action.label : Action → String
  | .lootAll => "Loot All"
  | .depositAll => "Deposit All"
  | .quickStack => "Quick Stack"
  | .restock => "Restock"
  | .sortItems => "Sort Items"

private def coin (item : UInt16) : Bool := 71 ≤ item && item ≤ 74

private def remaining (stack : Stack) (count : Nat) : Option Stack :=
  if count == 0 then none else some { stack with count }

private def stackInto (source : Stack) (destination : Stack) : Nat :=
  min source.count (destination.spaceFor source)

private def moveStack (fromSlots toSlots : Inventory) (fromIndex toIndex : Nat) :
    Inventory × Inventory := Id.run do
  let some source := fromSlots[fromIndex]?.join | return (fromSlots, toSlots)
  let some destination := toSlots[toIndex]? | return (fromSlots, toSlots)
  let some destination := destination
    | return (fromSlots.set! fromIndex none, toSlots.set! toIndex (some source))
  let amount := stackInto source destination
  return (fromSlots.set! fromIndex (remaining source (source.count - amount)),
    toSlots.set! toIndex (some { destination with count := destination.count + amount }))

/-- Deposit and Shift-click fill matching stacks before taking the first
empty container slot. The source slot survives if there is no room. -/
def deposit (inventory container : Inventory) (index : Nat) : Inventory × Inventory := Id.run do
  let some stack := inventory[index]?.join | return (inventory, container)
  if stack.favorite then return (inventory, container)
  let mut inventory := inventory
  let mut container := container
  for slot in [:container.size] do
    if inventory[index]?.join.isNone then break
    if container[slot]?.join.isSome then
      let next := moveStack inventory container index slot
      inventory := next.1
      container := next.2
  let some empty := container.findIdx? Option.isNone | return (inventory, container)
  return moveStack inventory container index empty

private def putCoins (slots : Inventory) (index denomination : Nat) (counts : Array Nat) :
    Inventory × Array Nat := Id.run do
  let item := (71 + denomination).toUInt16
  let count := min counts[denomination]! (maxStack item)
  if count == 0 then return (slots, counts)
  return (slots.set! index (some (.ofItem item count)),
    counts.set! denomination (counts[denomination]! - count))

private def highestCoin (counts : Array Nat) : Option Nat :=
  #[3, 2, 1, 0].find? fun i => counts[i]! > 0

/-- ChestUI.MoveCoins only runs when the destination already holds coins.
Normalize the combined denominations, then use old coin slots before
empty slots. Any remainder returns to the slots vacated in the backpack. -/
private def moveCoins (inventory container : Inventory) : Inventory × Inventory := Id.run do
  if !container.any (fun slot => slot.any (coin ∘ Stack.item)) then return (inventory, container)
  let mut counts := #[0, 0, 0, 0]
  let mut oldCoins := #[]
  let mut sourceCoins := #[]
  let mut container := container
  let mut inventory := inventory
  for index in [:container.size] do
    let some stack := container[index]?.join | continue
    if !coin stack.item then continue
    let kind := stack.item.toNat - 71
    counts := counts.modify kind (· + stack.count)
    oldCoins := oldCoins.push (index, kind)
    container := container.set! index none
  for index in [:inventory.size] do
    let some stack := inventory[index]?.join | continue
    if !coin stack.item || stack.favorite then continue
    counts := counts.modify (stack.item.toNat - 71) (· + stack.count)
    sourceCoins := sourceCoins.push index
    inventory := inventory.set! index none
  for kind in [:3] do
    let count := counts[kind]!
    counts := (counts.set! kind (count % 100)).modify (kind + 1) (· + count / 100)
  for (index, kind) in oldCoins do
    let next := putCoins container index kind counts
    container := next.1
    counts := next.2
  let candidates := (oldCoins.map Prod.fst) ++
    ((Array.range container.size).filter fun index => !oldCoins.any (fun pair => pair.1 == index))
  for index in candidates do
    let some kind := highestCoin counts | break
    if container[index]?.join.isSome then continue
    let next := putCoins container index kind counts
    container := next.1
    counts := next.2
  for index in sourceCoins do
    let some kind := highestCoin counts | break
    let next := putCoins inventory index kind counts
    inventory := next.1
    counts := next.2
  return (inventory, container)

/-- The open container is the outer loop for both passes of QuickStack.
Eligible source items start at slot 10 and match the original types.
It uses pre-existing stacks before making new ones in the original gaps. -/
private def quickStack (inventory container : Inventory) (personal : Bool) :
    Inventory × Inventory := Id.run do
  let types := container.filterMap (fun slot =>
    (slot.filter fun item => !coin item.item).map Stack.item)
  let upper := if personal then 54 else 50
  let candidates := (Array.range (min upper inventory.size)).filter fun index =>
    index ≥ 10 && (inventory[index]?.join).any (fun stack => !stack.favorite && types.contains stack.item)
  let empty := (Array.range container.size).filter (fun i => container[i]?.join.isNone)
  let mut inventory := inventory
  let mut container := container
  for index in [:container.size] do
    if (container[index]?.join).all (fun stack => coin stack.item) then continue
    for source in candidates do
      let next := moveStack inventory container source index
      inventory := next.1
      container := next.2
  for index in empty do
    for source in candidates do
      let next := moveStack inventory container source index
      inventory := next.1
      container := next.2
  return (inventory, container)

def loot (inventory container : Inventory) (index : Nat) : Inventory × Inventory := Id.run do
  let some stack := container[index]?.join | return (inventory, container)
  let (inventory, rest) := OriginalItems.store inventory stack
  return (inventory, container.set! index rest)

structure Looted where
  inventory : Inventory
  container : Inventory
  vault : Inventory

/-- LootAllFromChest permits overflow. The personal-container settings use
only the carried slots, even while the Void Bag is active. -/
def lootAll (initial container vault : Inventory) (vaultEnabled preserveFavorites : Bool) :
    Looted := Id.run do
  let mut inventory := initial
  let mut container := container
  let mut vault := vault
  for index in [:container.size] do
    let some stack := container[index]?.join | continue
    if preserveFavorites && stack.favorite then continue
    let result := OriginalItems.receive inventory vault vaultEnabled stack
    inventory := result.carried
    vault := result.vault
    container := container.set! index result.remainder
  return ⟨inventory, container, vault⟩

/-- ChestUI.Restock visits the original partial slots in descending order.
A qualifying ammo stack can then take an empty slot and join the end of
the partial list for subsequent container slots. -/
def restock (inventory container : Inventory) : Inventory × Inventory := Id.run do
  let occupied (slot : Option Stack) := slot.any (fun stack => stack.item > 0 && stack.count > 0)
  let allows (index : Nat) (stack : Stack) :=
    index < 50 || OriginalItemStats.fitsAmmoSlot stack.item
  let mut types : Array UInt16 := #[]
  let mut partials : Array Nat := #[]
  let mut empty : Array Nat := #[]
  for index in (Array.range (min inventory.size 58)).reverse do
    if index ≥ 50 && index < 54 then continue
    let slot := inventory[index]!
    if slot.any Stack.isCoin then continue
    let some stack := slot.filter (fun stack => stack.item > 0 && stack.count > 0) |
      empty := empty.push index
      continue
    let limit := maxStack stack.item
    if limit ≤ 1 || (stack.favorite && OriginalItemStats.onlyNeedOneInInventory stack.item) then
      continue
    if !types.contains stack.item then types := types.push stack.item
    if stack.count < limit then partials := partials.push index
  let mut inventory := inventory
  let mut container := container
  for index in [:container.size] do
    let some stack := container[index]?.join | continue
    if stack.count == 0 || stack.prefixId != 0 || !types.contains stack.item then continue
    for dest in partials do
      if container[index]?.join.isNone then break
      if !allows dest stack then continue
      let next := moveStack container inventory index dest
      container := next.1
      inventory := next.2
    let some remainder := container[index]?.join | continue
    if !OriginalItemStats.hasAmmoType remainder.item then continue
    let some dest := empty.find? (fun i => !occupied inventory[i]! && allows i remainder) | continue
    inventory := inventory.set! dest (some remainder)
    container := container.set! index none
    partials := partials.push dest
  return (inventory, container)

def apply (action : Action) (inventory container : Inventory) (personal : Bool)
    (preserveFavorites : Bool := false) :
    Inventory × Inventory := Id.run do
  let (inventory, container) := if action == .depositAll || (action == .quickStack && personal) then
      moveCoins inventory container
    else (inventory, container)
  match action with
  | .quickStack => return quickStack inventory container personal
  | .restock => return restock inventory container
  | .sortItems => return (inventory, OriginalItemSorting.sortChest {} container)
  | .lootAll =>
    let result := lootAll inventory container #[] false preserveFavorites
    return (result.inventory, result.container)
  | .depositAll =>
    let mut inventory := inventory
    let mut container := container
    for index in (Array.range (min inventory.size 50)).reverse do
      let next := deposit inventory container index
      inventory := next.1
      container := next.2
    return (inventory, container)

private def splitCount (slot cursor : Option Stack) (count : Nat) :
    Option (Option Stack × Option Stack) := do
  let item ← slot
  let held := cursor.getD { item with count := 0, favorite := item.favorite && item.count == 1 }
  let amount := min count (stackInto item held)
  guard (amount > 0)
  return (remaining item (item.count - amount), some { held with count := held.count + amount })

/-- One right press on an ordinary stack takes one item onto the cursor. -/
def splitOne (slot cursor : Option Stack) : Option (Option Stack × Option Stack) :=
  splitCount slot cursor 1

structure SplitClock where
  cooldown : Nat := 0
  delay : Nat := 7
  draws : Nat := 0
  accelerations : Nat := 0
  extra : Nat := 0
  deriving BEq, Repr

/-- Main.DoDraw updates the acceleration counters before drawing the
inventory. After the delay reaches two, it starts increasing the batch. -/
def SplitClock.beginDraw (clock : SplitClock) : SplitClock :=
  if clock.cooldown == 0 then {}
  else
    let draws := clock.draws + 1
    if draws < 5 * (clock.delay - 1) then { clock with draws }
    else
      let extra := if clock.accelerations < 10 then 0
        else if clock.delay == 2 then clock.extra + 1 else clock.extra
      { cooldown := clock.cooldown
        delay := max 2 (clock.delay - 1)
        draws := 0
        accelerations := clock.accelerations + 1
        extra }

def SplitClock.endDraw (clock : SplitClock) (held : Bool) : SplitClock :=
  { clock with cooldown := if held then clock.cooldown - 1 else 0 }

def SplitClock.refresh (clock : SplitClock) : SplitClock :=
  { clock with cooldown := if clock.cooldown == 0 then 30 else clock.delay }

/-- ItemSlot.RightClick and RefreshStackSplitCooldown for an ordinary slot. -/
def SplitClock.take (clock : SplitClock) (slot cursor : Option Stack) :
    Option (SplitClock × Option Stack × Option Stack) := do
  guard (clock.cooldown ≤ 1)
  let (slot, cursor) ← splitCount slot cursor (clock.extra + 1)
  return (clock.refresh, slot, cursor)

end Lean4Terria.OriginalStorage
