import Lean4Terria.OriginalItemStats
import Lean4Terria.OriginalMining
import Lean4Terria.OriginalPlayerWet
import Lean4Terria.UnifiedRandom

namespace Lean4Terria.OriginalItems

open OriginalCollision (Grid Vector single tileIndex)
open OriginalMining (Cell)
open OriginalPlayerCollision (Body)

structure Stack where
  item : UInt16
  count : Nat := 1
  prefixId : UInt8 := 0
  favorite : Bool := false
  /-- Runtime item color. Terraria restores the item's default color from the saved ID. -/
  color : Option UInt32 := none
  deriving BEq, Repr

def Stack.ofItem (item : UInt16) (count : Nat := 1) : Stack :=
  { item, count }

def Stack.canStack (stack other : Stack) : Bool :=
  stack.item == other.item && stack.prefixId == other.prefixId

def Stack.spaceFor (slot incoming : Stack) : Nat :=
  if slot.canStack incoming then OriginalItemStats.maxStack slot.item - slot.count else 0

def Stack.acceptsPickup (slot incoming : Stack) : Bool :=
  slot.spaceFor incoming > 0 && (!slot.favorite || !OriginalItemStats.onlyNeedOneInInventory slot.item)

abbrev Inventory := Array (Option Stack)

def copperPickaxe : UInt16 := 3509
def copperAxe : UInt16 := 3506

def starterInventory : Inventory :=
  #[some (.ofItem 3507), some (.ofItem copperPickaxe), some (.ofItem copperAxe)] ++
  Array.replicate 47 none

private def grabRange : Int := 42
private def dropSize : Nat := 16
private def worldItemLimit : Nat := 400

private def overflowProtectionAge : UInt16 → Nat
  | 2 | 172 | 593 => 200
  | 3 | 61 => 150
  | 169 | 176 | 664 => 100
  | _ => 0

/-- Pickup cooldowns carried by an individual world item. The owner of a manual
drop and all players in the Guide Voodoo Doll case are the same local player here. -/
structure GrabDelays where
  grabDelay : Nat := 0
  grabDelayAllPlayers : Bool := false
  enemyGrabDelay : Nat := 0
  deriving BEq, Repr, Inhabited

def GrabDelays.blocksLocalGrab (delays : GrabDelays) : Bool := delays.grabDelay > 0

structure Drop where
  stack : Stack
  position : Vector
  velocity : Vector
  beingGrabbed : Bool := false
  age : Nat := 0
  liquid : OriginalPlayerWet.Liquid := {}
  delays : GrabDelays := {}
  deriving BEq, Repr

/-- The item phase ages the drop and its pickup cooldowns after movement. -/
def Drop.advanceTimers (drop : Drop) : Drop :=
  { drop with
    age := min 2147483547 (drop.age + 1)
    delays := { drop.delays with
      grabDelay := drop.delays.grabDelay - 1
      enemyGrabDelay := drop.delays.enemyGrabDelay - 1 } }

abbrev WorldItems := Array (Option Drop)

def spawnWet (grid : Grid) (position : Vector) : Bool :=
  (OriginalPlayerWet.wetCollision grid position dropSize dropSize).wet

def droppedStack (position velocity : Vector) (stack : Stack) (wet : Bool := false) : Drop :=
  {
    stack, position, velocity
    age := overflowProtectionAge stack.item
    liquid := { wet }
  }

def insertDrop (items : WorldItems) (drop : Drop) : WorldItems := Id.run do
  if let some index := items.findIdx? Option.isNone then
    return items.set! index (some drop)
  if items.size < worldItemLimit then return items.push (some drop)
  let mut oldest := 0
  for index in [1:items.size] do
    if ((items[index]?.join).map (·.age)).getD 0 >
        ((items[oldest]?.join).map (·.age)).getD 0 then
      oldest := index
  return items.set! oldest (some drop)

def spawnStackWithVelocity (items : WorldItems) (position velocity : Vector) (stack : Stack)
    (wet : Bool := false) : WorldItems :=
  insertDrop items (droppedStack position velocity stack wet)

def rollDropAt (position : Vector) (stack : Stack)
    (rng : UnifiedRandom) (grid : Option Grid := none) : Drop × UnifiedRandom :=
  let (x, rng) := rng.next (-30) 31
  let (y, rng) := rng.next (-40) (-15)
  (droppedStack position ⟨single x * 0.1, single y * 0.1⟩ stack
    (grid.any fun grid => spawnWet grid position), rng)

def spawnStackAt (items : WorldItems) (position : Vector) (stack : Stack)
    (rng : UnifiedRandom) (grid : Option Grid := none) : WorldItems × UnifiedRandom :=
  let (drop, rng) := rollDropAt position stack rng grid
  (insertDrop items drop, rng)

/-- A tile-break drop starts in a 16 × 16 hitbox centered on the tile. -/
def spawnStack (items : WorldItems) (cell : Cell) (stack : Stack)
    (rng : UnifiedRandom) (grid : Option Grid := none) : WorldItems × UnifiedRandom :=
  spawnStackAt items ⟨single (cell.x * 16 : Nat), single (cell.y * 16 : Nat)⟩ stack rng grid

def spawnBlock (items : WorldItems) (cell : Cell) (item : UInt16)
    (rng : UnifiedRandom) (grid : Option Grid := none) : WorldItems × UnifiedRandom :=
  spawnStack items cell (.ofItem item) rng grid

private def lerp (left right : Vector) (amount : Float32) : Vector :=
  ⟨left.x + (right.x - left.x) * amount, left.y + (right.y - left.y) * amount⟩

def Stack.isCoin (stack : Stack) : Bool := 71 ≤ stack.item && stack.item ≤ 74

def combineNearby (items : WorldItems) (index : Nat) (drop : Drop) :
    WorldItems × Drop := Id.run do
  let limit := OriginalItemStats.maxStack drop.stack.item
  if drop.stack.isCoin || drop.stack.item == 58 || drop.stack.item == 184 ||
      drop.stack.count ≥ limit then return (items, drop)
  let mut items := items
  let mut drop := drop
  for otherIndex in [index + 1:items.size] do
    let some other := items[otherIndex]?.join | continue
    if !drop.stack.canStack other.stack then continue
    let distance := (drop.position.x - other.position.x).abs + (drop.position.y - other.position.y).abs
    if distance > 30 then continue
    let moved := min other.stack.count (limit - drop.stack.count)
    let count := drop.stack.count + moved
    let amount := single moved / single count
    drop := { drop with
      stack.count := count
      position := lerp other.position drop.position amount
      velocity := lerp other.velocity drop.velocity amount }
    items := items.set! otherIndex <| if other.stack.count ≤ moved then none
      else some { other with stack.count := other.stack.count - moved }
  return (items, drop)

def inWorld (grid : Grid) (position : Vector) : Bool :=
  let x := tileIndex position.x
  let y := tileIndex position.y
  x ≥ 20 && y ≥ 20 && x < (grid.width : Int) - 20 && y < (grid.height : Int) - 20

/-- The dry path of WorldItem.UpdateItem for ordinary block and ore drops. -/
def updateDry (grid : Grid) (initial : WorldItems) : WorldItems := Id.run do
  let mut items := initial
  for index in [:items.size] do
    let some drop := items[index]?.join | continue
    if drop.beingGrabbed then
      items := items.set! index (some ({ drop with
        beingGrabbed := false, position := drop.position + drop.velocity } : Drop).advanceTimers)
    else
      let (combined, drop) := combineNearby items index drop
      let x := drop.velocity.x * 0.95
      let x := if x.abs.toFloat < 0.1 then 0 else x
      let velocity : Vector := ⟨x, min 7 (drop.velocity.y + 0.1)⟩
      let contact := OriginalCollision.tileCollision grid drop.position velocity dropSize dropSize
        { ignoreAetheriumPlatforms := true, hoik := true }
      let position := drop.position + contact.displacement
      let slope := OriginalCollision.slopeCollision grid position contact.displacement dropSize dropSize false true
      items := combined.set! index <| if inWorld grid slope.position then
        some ({ drop with position := slope.position, velocity := slope.velocity } : Drop).advanceTimers
      else none
  return items

/-- Player.DoCoins promotes a stack of exactly 100 and first tries to merge
the new coin into another carried slot. At most three promotions are possible. -/
private def promoteCoins (initial : Inventory) (slot : Nat) : Inventory := Id.run do
  let mut inventory := initial
  let mut index := slot
  for _ in [:3] do
    let some slot := inventory[index]?.join | continue
    if slot.count != 100 || slot.item < 71 || slot.item ≥ 74 then break
    let coin := slot.item + 1
    inventory := inventory.set! index (some (.ofItem coin))
    let target := (Array.range (min inventory.size 54)).find? fun other =>
      other != index && inventory[other]!.any
        (fun stack => stack.item == coin && stack.count < OriginalItemStats.maxStack coin)
    let some target := target | break
    inventory := (inventory.set! index none).modify target (·.map fun s => { s with count := s.count + 1 })
    index := target
  return inventory

private def fillOccupied (initial : Inventory) (stack : Stack) (order : Array Nat)
    (protectFavorites : Bool) (coins : Bool) : Inventory × Nat × Nat := Id.run do
  let mut inventory := initial
  let mut count := stack.count
  let mut transfers := 0
  for index in order do
    if count == 0 then break
    let some slot := inventory[index]?.join | continue
    if protectFavorites && !slot.acceptsPickup stack then continue
    let amount := min count (slot.spaceFor stack)
    if amount == 0 then continue
    inventory := inventory.set! index (some { slot with count := slot.count + amount })
    if coins then inventory := promoteCoins inventory index
    count := count - amount
    transfers := transfers + 1
  return (inventory, count, transfers)

structure Stored where
  inventory : Inventory
  remainder : Option Stack
  /-- One global PlaySound per successful destination, including partial fills. -/
  sounds : Array Nat := #[]

/-- Player.GetItem fills ammo first. Coins try their occupied side slots
before scanning the main inventory. After merging, items with a use style
try the hotbar, and other empty-slot searches respect the incoming favorite. -/
def storeWithSounds (initial : Inventory) (stack : Stack) : Stored := Id.run do
  let mut inventory := initial
  let mut count := stack.count
  let mut sounds := #[]
  let sound := if stack.isCoin then 38 else 7
  if count == 0 then return { inventory, remainder := none }
  if OriginalItemStats.fitsAmmoSlot stack.item then
    let ammo := (Array.range (min inventory.size 58)).filter (· ≥ 54)
    let (updated, rest, transfers) := fillOccupied inventory stack ammo false true
    sounds := sounds ++ Array.replicate transfers sound
    inventory := updated
    count := rest
    if count == 0 then return { inventory, remainder := none, sounds }
    if OriginalItemStats.autoFillAmmoSlot stack.item then
      if let some slot := ammo.find? (fun i => inventory[i]!.isNone) then
        return {
          inventory := inventory.set! slot (some { stack with count })
          remainder := none
          sounds := sounds.push sound }
  let main := Array.range (min 50 inventory.size)
  let side := (Array.range (min 54 inventory.size)).filter (· ≥ 50)
  let order := if stack.isCoin then side ++ main else main
  let (updated, remaining, transfers) := fillOccupied inventory { stack with count } order true true
  sounds := sounds ++ Array.replicate transfers sound
  if remaining == 0 then return { inventory := updated, remainder := none, sounds }
  let stack := { stack with count := remaining }
  let hotbar := if !stack.isCoin && OriginalItemStats.hasUseStyle stack.item then
      (updated.extract 0 10).findIdx? Option.isNone
    else none
  let slot := hotbar.orElse fun _ =>
    let order := Array.range (min updated.size (if stack.isCoin then 54 else 50))
    (if stack.favorite then order else order.reverse).find? (fun i => updated[i]!.isNone)
  if let some index := slot then
    return {
      inventory := promoteCoins (updated.set! index (some stack)) index
      remainder := none
      sounds := sounds.push sound }
  return { inventory := updated, remainder := some stack, sounds }

/-- Compatibility projection for callers that own their separate UI effects. -/
def store (initial : Inventory) (stack : Stack) : Inventory × Option Stack :=
  let result := storeWithSounds initial stack
  (result.inventory,result.remainder)

structure Received where
  carried : Inventory
  vault : Inventory
  remainder : Option Stack
  sounds : Array Nat := #[]

/-- Player.GetItem_VoidVault first merges into matching stacks. Its empty
slot path calls DoCoins at the same index in the player's carried slots. -/
def receive (carried vault : Inventory) (vaultEnabled : Bool) (stack : Stack) : Received := Id.run do
  let stored := storeWithSounds carried stack
  let carried := stored.inventory
  let remainder := stored.remainder
  let mut sounds := stored.sounds
  let some stack := remainder | return { carried, vault, remainder := none, sounds }
  if !vaultEnabled || !OriginalItemStats.canGoInVoidVault stack.item then
    return { carried, vault, remainder, sounds }
  let (vault, count, transfers) := fillOccupied vault stack (Array.range vault.size) false false
  sounds := sounds ++ Array.replicate transfers (if stack.isCoin then 38 else 7)
  if count == 0 then return { carried, vault, remainder := none, sounds }
  let some index := vault.findIdx? Option.isNone
    | return { carried, vault, remainder := some { stack with count }, sounds }
  return {
    carried := promoteCoins carried index
    vault := vault.set! index (some { stack with count })
    remainder := none
    sounds := sounds.push (if stack.isCoin then 38 else 7) }

private inductive PickupSpace where
  | none | carried | vault

private def pickupSpace (carried vault : Inventory) (vaultEnabled : Bool)
    (stack : Stack) : PickupSpace := Id.run do
  let accepts (slot : Option Stack) := slot.all (·.acceptsPickup stack)
  if (carried.extract 0 (if stack.isCoin then 54 else 50)).any accepts then return .carried
  let ammo := carried.extract 54 58
  if OriginalItemStats.isAmmoOnPickup stack.item &&
      OriginalItemStats.autoFillAmmoSlot stack.item && ammo.any Option.isNone then return .carried
  if ammo.any (fun slot => slot.any (fun item => item.spaceFor stack > 0)) then return .carried
  if vaultEnabled && OriginalItemStats.canGoInVoidVault stack.item && vault.any accepts then
    return .vault
  return .none

private def inReach (body : Body) (drop : Drop) (range : Int) : Bool :=
  let px := body.position.x.toInt32.toInt
  let py := body.position.y.toInt32.toInt
  let dx := drop.position.x.toInt32.toInt
  let dy := drop.position.y.toInt32.toInt
  dx < px + body.width + range && dx + dropSize > px - range &&
    dy < py + body.height + range && dy + dropSize > py - range

private def pullAxis (velocity upper lower : Float32) (positive : Bool) : Float32 :=
  if positive then
    let speed := if velocity < upper then velocity + 0.45 else velocity
    if speed < 0 then speed + 0.45 * 0.75 else speed
  else
    let speed := if velocity > lower then velocity - 0.45 else velocity
    if speed > 0 then speed - 0.45 * 0.75 else speed

private def pullToVault (body : Body) (drop : Drop) : Vector :=
  let dx := body.position.x + single (body.width / 2 : Nat) - (drop.position.x + 8)
  let dy := body.position.y + single (body.height / 2 : Nat) - (drop.position.y + 8)
  let scale := 12 / (dx * dx + dy * dy).toFloat.sqrt.toFloat32
  ⟨(drop.velocity.x * 4 + dx * scale) / 5, (drop.velocity.y * 4 + dy * scale) / 5⟩

inductive PickupEffect where
  | sound (typeId : Nat)
  | healText
  | manaText
  deriving BEq, Repr

structure Pickup where
  carried : Inventory
  vault : Inventory
  items : WorldItems
  collected : Bool := false
  healed : Nat := 0
  manaStars : Nat := 0
  /-- Actual slot-ordered GetItem/PickupItem effects, before later player phases. -/
  effects : Array PickupEffect := #[]

/-- Full true set read from desktop1.4.5.8 ItemID.Sets (6196 entries). -/
def ignoresEncumberingStone (item : UInt16) : Bool :=
  #[58,184,1734,1735,1867,1868,3453,3454,3455,4143].contains item

/-- Single-player GrabItems for an unbuffed Classic character. The
player phase receives this tick's latched Void Vault setting. -/
def grabInto (body : Body) (selectedEmptyDuringUse vaultEnabled : Bool)
    (carried vault : Inventory) (worldItems : WorldItems)
    (manaNeeded : Nat := 0) (preventAllItemPickups : Bool := false)
    (slotRange : Option (Nat × Nat) := none) : Pickup := Id.run do
  let mut carried := carried
  let mut vault := vault
  let mut items := worldItems
  let mut collected := false
  let mut healed := 0
  let mut manaStars := 0
  let mut effects := #[]
  let (first,last) := slotRange.getD (0,items.size)
  for index in [first:min last items.size] do
    let some drop := items[index]?.join | continue
    if drop.delays.blocksLocalGrab then continue
    if preventAllItemPickups && !ignoresEncumberingStone drop.stack.item then continue
    let heart := drop.stack.item == 58
    let manaStar := drop.stack.item == 184
    if inReach body drop 0 then
      if selectedEmptyDuringUse then continue
      if heart || manaStar then
        effects := (effects.push (.sound 7)).push (if heart then .healText else .manaText)
        if heart then healed := healed + 20 else manaStars := manaStars + 1
        collected := true
        items := items.set! index none
        continue
      let result := receive carried vault vaultEnabled drop.stack
      effects := effects ++ result.sounds.map PickupEffect.sound
      collected := collected || (result.remainder.map (·.count)).getD 0 < drop.stack.count
      carried := result.carried
      vault := result.vault
      items := items.set! index <| result.remainder.map fun stack => { drop with stack }
    else if inReach body drop grabRange then
      if manaStar && manaNeeded ≤ manaStars * 100 then continue
      let space := if heart || manaStar then .carried else pickupSpace carried vault vaultEnabled drop.stack
      let velocity := match space with
        | .none => none
        | .vault => some (pullToVault body drop)
        | .carried =>
          let right := body.position.x.toFloat + body.width.toFloat * 0.5 > drop.position.x.toFloat + 8
          let down := body.position.y.toFloat + body.height.toFloat * 0.5 > drop.position.y.toFloat + 8
          some ⟨pullAxis drop.velocity.x (4 + body.velocity.x) (-4 + body.velocity.x) right,
            pullAxis drop.velocity.y 4 (-4) down⟩
      let some velocity := velocity | continue
      items := items.set! index (some { drop with velocity, beingGrabbed := true })
  return { carried, vault, items, collected, healed, manaStars, effects }

def grab (body : Body) (selectedEmptyDuringUse : Bool) (inventory : Inventory)
    (worldItems : WorldItems) : Inventory × WorldItems :=
  let result := grabInto body selectedEmptyDuringUse false inventory #[] worldItems
  (result.carried, result.items)

end Lean4Terria.OriginalItems
