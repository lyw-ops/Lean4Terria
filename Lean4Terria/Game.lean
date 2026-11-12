import Lean4Terria.Physics

namespace Lean4Terria

inductive Item where
  | basicPick | copperPick | dirt | stone | copper | crystal
  deriving BEq, Repr, Inhabited

def Item.name : Item → String
  | .basicPick => "Basic pick"
  | .copperPick => "Copper pick"
  | .dirt => "Dirt"
  | .stone => "Stone"
  | .copper => "Copper ore"
  | .crystal => "Crystal"

def Item.code : Item → UInt8
  | .basicPick => 1
  | .copperPick => 2
  | .dirt => 3
  | .stone => 4
  | .copper => 5
  | .crystal => 6

def Item.ofCode? : UInt8 → Option Item
  | 1 => some .basicPick
  | 2 => some .copperPick
  | 3 => some .dirt
  | 4 => some .stone
  | 5 => some .copper
  | 6 => some .crystal
  | _ => none

structure Stack where
  item : Item
  count : Nat
  deriving BEq, Repr

abbrev Inventory := Array (Option Stack)

namespace Inventory

def slotCount : Nat := 6

def maxStack : Nat := 99

def stackLimit : Item → Nat
  | .basicPick | .copperPick => 1
  | _ => maxStack

def count (inventory : Inventory) (item : Item) : Nat :=
  inventory.foldl (fun total slot =>
    match slot with
    | some stack => if stack.item == item then total + stack.count else total
    | none => total) 0

private def addOne (inventory : Inventory) (item : Item) : Option (Inventory × Nat) := do
  let stackIndex := inventory.findIdx? fun slot =>
    slot.any fun stack => stack.item == item && stack.count < stackLimit item
  let index ← stackIndex.orElse fun _ => inventory.findIdx? Option.isNone
  let quantity := match inventory[index]?.join with
    | some stack => stack.count + 1
    | none => 1
  return (inventory.set! index (some ⟨item, quantity⟩), index)

private def removeFromSlot (inventory : Inventory) (index : Nat) (quantity : Nat) :
    Inventory :=
  match inventory[index]?.join with
  | none => inventory
  | some stack =>
    let remaining := stack.count - quantity
    inventory.set! index (if remaining == 0 then none else some { stack with count := remaining })

/-- Remove up to `quantity` units of `item`, starting at the first slot. -/
private def remove (inventory : Inventory) (item : Item) (quantity : Nat) : Inventory := Id.run do
  let mut result := inventory
  let mut remaining := quantity
  for index in [:inventory.size] do
    if remaining == 0 then break
    if let some stack := result[index]?.join then
      if stack.item == item then
        let used := min remaining stack.count
        result := removeFromSlot result index used
        remaining := remaining - used
  return result

end Inventory

inductive CraftError where
  | alreadyOwned | insufficientCopper | inventoryFull
  deriving BEq, Repr

def copperPickCost : Nat := 3

def tryCraft (inventory : Inventory) : Except CraftError (Inventory × Nat) :=
  if Inventory.count inventory .copperPick > 0 then
    .error .alreadyOwned
  else if Inventory.count inventory .copper < copperPickCost then
    .error .insufficientCopper
  else
    let spent := Inventory.remove inventory .copper copperPickCost
    match Inventory.addOne spent .copperPick with
    | some result => .ok result
    | none => .error .inventoryFull

theorem tryCraft_inventory_unchanged_of_insufficientCopper (inventory : Inventory)
    (h : Inventory.count inventory .copper < copperPickCost) :
    (match tryCraft inventory with
      | .ok (crafted, _) => crafted
      | .error _ => inventory) = inventory := by
  by_cases ownsPick : Inventory.count inventory .copperPick > 0 <;>
    simp [tryCraft, ownsPick, h]

structure GameState where
  seed : UInt64
  world : World
  player : Player
  inventory : Inventory
  selected : Nat
  status : String

structure TickInput where
  horizontal : Int := 0
  jump : Bool := false
  mine : Option TilePos := none
  place : Option TilePos := none
  selectSlot : Option Nat := none
  craft : Bool := false
  deriving Inhabited

def newGame (seed : UInt64) : GameState :=
  let (world, player) := generateWorld seed
  let inventory : Inventory := (Array.replicate Inventory.slotCount none).set! 0
    (some ⟨.basicPick, 1⟩)
  { seed, world, player, inventory, selected := 0
    status := "Explore the cave and collect copper ore." }

private def actionReach : Nat := 5

def GameState.canReach (state : GameState) (position : TilePos) : Bool :=
  let center := state.player.centerTile
  state.world.contains position &&
    (position.x - center.x).natAbs + (position.y - center.y).natAbs <= actionReach

private def GameState.nearWorkbench (state : GameState) : Bool := Id.run do
  let center := state.player.centerTile
  for row in [:2 * actionReach + 1] do
    for column in [:2 * actionReach + 1] do
      let pos : TilePos := ⟨center.x + (column : Int) - (actionReach : Int),
        center.y + (row : Int) - (actionReach : Int)⟩
      if state.world.tile pos == .workbench && state.canReach pos then
        return true
  return false

def GameState.selectedStack (state : GameState) : Option Stack :=
  state.inventory[state.selected]?.join

private def GameState.mineTile (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canReach position then
    return { state with status := "Move closer to mine that tile." }
  let tile := state.world.tile position
  if tile == .air then return state
  let item := match tile with
    | .dirt => some Item.dirt
    | .stone => some Item.stone
    | .copper => some Item.copper
    | .crystal => some Item.crystal
    | _ => none
  let some drop := item
    | return { state with status := "That tile cannot be mined." }
  let some tool := state.selectedStack
    | return { state with status := "Select a pick in your hotbar." }
  if tool.item != .basicPick && tool.item != .copperPick then
    return { state with status := "Select a pick in your hotbar." }
  if tile == .crystal && tool.item != .copperPick then
    return { state with status := "Crystal requires a copper pick." }
  let some (inventory, _) := Inventory.addOne state.inventory drop
    | return { state with status := "Your hotbar is full." }
  return { state with
    world := state.world.setTile position .air
    inventory
    status := if drop == .crystal then "You found crystal! Save or keep exploring."
      else s!"Collected {drop.name}." }

private def GameState.placeTile (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canReach position then
    return { state with status := "Move closer to place that tile." }
  if state.world.tile position != .air || state.player.overlapsTile position then
    return { state with status := "Choose an empty tile clear of your character." }
  let some stack := state.selectedStack
    | return { state with status := "Select dirt or stone in your hotbar." }
  let block := match stack.item with
    | .dirt => some Tile.dirt
    | .stone => some Tile.stone
    | _ => none
  let some tile := block
    | return { state with status := "Select dirt or stone in your hotbar." }
  return { state with
    world := state.world.setTile position tile
    inventory := Inventory.removeFromSlot state.inventory state.selected 1
    status := s!"Placed {stack.item.name}." }

private def GameState.craftPick (state : GameState) : GameState :=
  if !state.nearWorkbench then
    { state with status := "Get closer to the workbench to craft." }
  else
    match tryCraft state.inventory with
    | .ok (inventory, selected) =>
      { state with inventory, selected, status := "Copper pick equipped. Look for crystal below." }
    | .error .alreadyOwned => { state with status := "You already have a copper pick." }
    | .error .insufficientCopper =>
      let available := Inventory.count state.inventory .copper
      { state with status := s!"Copper pick: {available}/{copperPickCost} copper ore." }
    | .error .inventoryFull => { state with status := "Make room for the pick in your hotbar." }

def step (state : GameState) (input : TickInput) : GameState := Id.run do
  let mut next := { state with player := movePlayer state.world state.player input.horizontal input.jump }
  if let some index := input.selectSlot then
    if index < next.inventory.size then
      next := { next with selected := index }
  if let some position := input.mine then
    next := next.mineTile position
  if let some position := input.place then
    next := next.placeTile position
  if input.craft then
    next := next.craftPick
  return next

end Lean4Terria
