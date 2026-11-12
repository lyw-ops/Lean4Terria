import Lean4Terria.Combat

namespace Lean4Terria

inductive Item where
  | basicPick | copperPick | dirt | stone | copper | crystal | wood | torch | stoneSword
  deriving BEq, Repr, Inhabited

def Item.name : Item → String
  | .basicPick => "Basic pick"
  | .copperPick => "Copper pick"
  | .dirt => "Dirt"
  | .stone => "Stone"
  | .copper => "Copper ore"
  | .crystal => "Crystal"
  | .wood => "Wood"
  | .torch => "Torch"
  | .stoneSword => "Stone sword"

def Item.code : Item → UInt8
  | .basicPick => 1
  | .copperPick => 2
  | .dirt => 3
  | .stone => 4
  | .copper => 5
  | .crystal => 6
  | .wood => 7
  | .torch => 8
  | .stoneSword => 9

def Item.ofCode? : UInt8 → Option Item
  | 1 => some .basicPick
  | 2 => some .copperPick
  | 3 => some .dirt
  | 4 => some .stone
  | 5 => some .copper
  | 6 => some .crystal
  | 7 => some .wood
  | 8 => some .torch
  | 9 => some .stoneSword
  | _ => none

structure Stack where
  item : Item
  count : Nat
  deriving BEq, Repr

abbrev Inventory := Array (Option Stack)

namespace Inventory

def slotCount : Nat := 8

def maxStack : Nat := 99

def stackLimit : Item → Nat
  | .basicPick | .copperPick | .stoneSword => 1
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

private def addItems (inventory : Inventory) (item : Item) (quantity : Nat) :
    Option (Inventory × Nat) := do
  let (updated, first) ← if quantity == 0 then none else addOne inventory item
  let mut result := updated
  for _ in [1:quantity] do
    let (updated, _) ← addOne result item
    result := updated
  return (result, first)

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
def startingWood : Nat := 4

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
  combat : CombatState

structure TickInput where
  horizontal : Int := 0
  jump : Bool := false
  mine : Option TilePos := none
  place : Option TilePos := none
  selectSlot : Option Nat := none
  craft : Bool := false
  craftTorches : Bool := false
  craftSword : Bool := false
  deriving Inhabited

def newGame (seed : UInt64) : GameState :=
  let (world, player) := generateWorld seed
  let inventory : Inventory := (Array.replicate Inventory.slotCount none).set! 0
    (some ⟨.basicPick, 1⟩)
  let inventory := inventory.set! (Inventory.slotCount - 1) (some ⟨.wood, startingWood⟩)
  { seed, world, player, inventory, selected := 0, combat := initialCombat world
    status := "Make torches at the workbench before heading underground." }

private def actionReach : Nat := 5

def GameState.canReach (state : GameState) (position : TilePos) : Bool :=
  let center := state.player.centerTile
  state.world.contains position &&
    (position.x - center.x).natAbs + (position.y - center.y).natAbs <= actionReach

def GameState.nearWorkbench (state : GameState) : Bool := Id.run do
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

private def neighbors (position : TilePos) : List TilePos :=
  [⟨position.x - 1, position.y⟩, ⟨position.x + 1, position.y⟩,
    ⟨position.x, position.y - 1⟩, ⟨position.x, position.y + 1⟩]

private def hasTorchSupport (world : World) (position : TilePos) : Bool :=
  (neighbors position).any fun neighbor => world.contains neighbor && (world.tile neighbor).solid

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
    | .timber => some Item.wood
    | .torch => some Item.torch
    | _ => none
  let some drop := item
    | return { state with status := "That tile cannot be mined." }
  let some tool := state.selectedStack
    | return { state with status := "Select a pick in your hotbar." }
  if tool.item != .basicPick && tool.item != .copperPick then
    return { state with status := "Select a pick in your hotbar." }
  if tile == .crystal && tool.item != .copperPick then
    return { state with status := "Crystal requires a copper pick." }
  let some (collected, _) := Inventory.addOne state.inventory drop
    | return { state with status := "Your hotbar is full." }
  let mut world := state.world.setTile position .air
  let mut inventory := collected
  let mut recovered := 0
  for neighbor in neighbors position do
    if world.tile neighbor == .torch && !hasTorchSupport world neighbor then
      let some (updated, _) := Inventory.addOne inventory .torch
        | return { state with status := "Make room to recover the torch first." }
      inventory := updated
      world := world.setTile neighbor .air
      recovered := recovered + 1
  let status := if recovered > 0 then
      let torches := if recovered == 1 then "a torch" else s!"{recovered} torches"
      s!"Collected {drop.name} and recovered {torches}."
    else if drop == .crystal then "You found crystal! Save or keep exploring."
    else s!"Collected {drop.name}."
  return { state with world, inventory, status }

private def GameState.placeTile (state : GameState) (position : TilePos) : GameState := Id.run do
  if !state.canReach position then
    return { state with status := "Move closer to place that tile." }
  if state.world.tile position != .air then
    return { state with status := "Choose an empty tile." }
  let some stack := state.selectedStack
    | return { state with status := "Select a block or torch in your hotbar." }
  let block := match stack.item with
    | .dirt => some Tile.dirt
    | .stone => some Tile.stone
    | .wood => some Tile.timber
    | .torch => some Tile.torch
    | _ => none
  let some tile := block
    | return { state with status := "Select a block or torch in your hotbar." }
  if tile.solid && (state.player.overlapsTile position ||
      state.combat.crawler.any (fun crawler => crawler.body.overlapsTile position)) then
    return { state with status := "Choose a tile clear of characters." }
  if tile == .torch && !hasTorchSupport state.world position then
    return { state with status := "Place the torch next to a solid tile." }
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

private def GameState.craftEquipment (state : GameState) (item : Item) (stoneCost amount : Nat) :
    GameState := Id.run do
  if !state.nearWorkbench then
    return { state with status := "Get closer to the workbench to craft." }
  if item == .stoneSword && Inventory.count state.inventory item > 0 then
    return { state with status := "You already have a stone sword." }
  if Inventory.count state.inventory .wood == 0 || Inventory.count state.inventory .stone < stoneCost then
    let cost := if stoneCost == 0 then "1 wood" else s!"{stoneCost} stone and 1 wood"
    return { state with status := s!"{item.name} needs {cost}." }
  let spent := Inventory.remove (Inventory.remove state.inventory .wood 1) .stone stoneCost
  let some (inventory, selected) := Inventory.addItems spent item amount
    | return { state with status := "Make room in your hotbar before crafting." }
  let status := if item == .torch then "Made 3 torches. Right-click beside a block to place one."
    else "Stone sword equipped. Left-click toward a crawler to swing."
  return { state with inventory, selected, status }

private def combatMessage (event : CombatEvent) (previous : String) : String :=
  match event with
  | .swing => previous
  | .hitCrawler => "You hit the cave crawler."
  | .defeatedCrawler => "Cave crawler defeated!"
  | .hurt => "The cave crawler hit you."
  | .respawned => "You returned to the surface with your items."

def step (state : GameState) (input : TickInput) : GameState := Id.run do
  let mut next := { state with player := movePlayer state.world state.player input.horizontal input.jump }
  if let some index := input.selectSlot then
    if index < next.inventory.size then
      next := { next with selected := index }
  let mut attackAt := none
  if let some position := input.mine then
    if next.selectedStack.any (fun stack => stack.item == .stoneSword) then
      attackAt := some position
    else
      next := next.mineTile position
  if let some position := input.place then
    next := next.placeTile position
  if input.craft then
    next := next.craftPick
  if input.craftTorches then
    next := next.craftEquipment .torch 0 3
  if input.craftSword then
    next := next.craftEquipment .stoneSword 2 1
  let (player, combat, event) := advanceCombat next.world next.player next.combat input.horizontal attackAt
  let status := event.map (fun value => combatMessage value next.status) |>.getD next.status
  return { next with player, combat, status }

end Lean4Terria
