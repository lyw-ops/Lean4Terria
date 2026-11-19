import Lean4Terria.OriginalItems
import Lean4Terria.OriginalItemThrow.Data

/-!
# Manual item drop (Terraria 1.4.5.8, single-player Classic)

`Player.dropItemCheck` as it runs for the local player, through the normal Throw hotkey (Q) and
the cursor item (right click outside the UI, or closing the inventory while holding an item),
together with the parts of `Item.NewItem` the drop reaches: `PickAnItemSlotToSpawnItemOn`,
`EmergencyStacking.EmergencyStackItemsToMakeSpace`, `Collision.WetCollision` for the spawn box,
the `OverflowProtectionTimeOffset` start age and `WorldItem.ApplySpawnOwnership`.

The interface is pure. `step` is one `Player.Update` worth of drop handling. `dropItemCheck`
itself is gated by the caller-visible `itemAnimation`, `itemTime` and `reuseDelay`, and
`releaseThrow := !controlThrow` follows it every frame. Inventories, cursor stacks and world items
are the shared `OriginalItems` types, and cursor items that go back into the inventory use
`OriginalItems.store` (`GetItem` with `GetItemSettings.ReturnItemFromSlot`).

The drop carries its pickup timers into the item and player phases. Evidence and the replay live
in `.lake/original-item-throw/1.4.5.8`.
-/

namespace Lean4Terria.OriginalItemThrow

open OriginalCollision (Grid Vector single)
open OriginalItems (Stack Inventory Drop WorldItems)
open OriginalPlayerCollision (Body)

/-! ## World item spawn -/

/-- `Main.item` has 400 live slots. Slot 400 is the sentinel that `NewItem` returns when nothing
can be reused. An item placed there is never updated by `UpdateWorld_Items` and never grabbed. -/
def worldItemSlots : Nat := 400

/-- `Item.PickupReplacementTime`: a heart or star older than this may be overwritten. -/
def pickupReplacementTime : Nat := 1200

/-- `WorldItem.DefaultGrabDelay`. -/
def defaultGrabDelay : Nat := 100

/-- `NewItemOwnership` as the local single-player drop uses it. -/
inductive Ownership where
  /-- 1: `GetOrDropItem` overflow. Single player keeps only `enemyGrabDelayTime`. -/
  | inventoryOverflow
  /-- 2: `DropSelectedItem`. The local player may not grab it for `DefaultGrabDelay` frames. -/
  | dropped
  /-- 3: `DropSelectedItem` of a `DropWithGrabDelayForAllPlayers` item (Guide Voodoo Doll). -/
  | droppedForAllPlayers
  deriving BEq, Repr, Inhabited

/-- A manual drop made in player frame `n` has its first timer tick in the same frame's
item phase and becomes eligible for pickup in player frame `n + 100`. -/
def Ownership.delays : Ownership → OriginalItems.GrabDelays
  | .inventoryOverflow => { enemyGrabDelay := defaultGrabDelay }
  | .dropped => { grabDelay := defaultGrabDelay, enemyGrabDelay := defaultGrabDelay }
  | .droppedForAllPlayers => { grabDelay := defaultGrabDelay, grabDelayAllPlayers := true }

/-- One `Item.NewItem` result. `slot = 400` is the sentinel slot (see `worldItemSlots`). `drop` is
not in the returned world then. -/
structure Spawn where
  slot : Nat
  drop : Drop
  ownership : Ownership
  deriving BEq, Repr

def Spawn.delays (spawn : Spawn) : OriginalItems.GrabDelays := spawn.drop.delays

private def entry (world : WorldItems) (index : Nat) : Option Drop := world[index]?.join

private def isAir (world : WorldItems) (index : Nat) : Bool :=
  match entry world index with
  | some drop => drop.stack.item == 0 || drop.stack.count == 0
  | none => true

private def setEntry (world : WorldItems) (index : Nat) (value : Option Drop) : WorldItems :=
  let padded := if world.size ≤ index then world ++ Array.replicate (index + 1 - world.size) none else world
  padded.set! index value

/-- `Entity.Center` for a body: `position + (width / 2, height / 2)` with integer halves. -/
def center (body : Body) : Vector :=
  ⟨body.position.x + single (body.width / 2 : Nat), body.position.y + single (body.height / 2 : Nat)⟩

/-- `(int)` of a float32 the way the IL `conv.i4` truncates (the values here are world pixels). -/
private def toInt (value : Float32) : Int := value.toInt32.toInt

/-! ## EmergencyStacking (when all 400 slots are taken) -/

/-- `EmergencyStacking.PlayerViewRectSize`, `MaxTransferDistance`,
`OnScreenDistancePriorityPenalty`, `ItemsToStackEachTime`. -/
def viewWidth : Int := 2320
def viewHeight : Int := 1600
def maxTransferDistance : Int := 2400
def onScreenPenalty : Int := 3
def transfersPerSpawn : Nat := 20

private structure Stackable where
  index : Nat
  item : UInt16
  age : Nat
  onScreen : Bool

/-- `StackableItem.IsPreferredDestination`: on screen first, then younger, then lower index. -/
private def Stackable.preferredOver (src dst : Stackable) : Bool :=
  if src.onScreen != dst.onScreen then src.onScreen
  else if src.age != dst.age then src.age < dst.age
  else src.index < dst.index

structure Transfer where
  src : Nat
  dst : Nat
  distanceOrder : Int
  preservationOrder : Int
  distance : Int
  deriving BEq, Repr, Inhabited

/-- `Transfer.CompareTo`. -/
private def Transfer.lt (a b : Transfer) : Bool :=
  if a.distanceOrder != b.distanceOrder then a.distanceOrder < b.distanceOrder
  else if a.preservationOrder != b.preservationOrder then a.preservationOrder < b.preservationOrder
  else a.distance < b.distance

/-- `EmergencyStacking.AddToOrderedList`. -/
private def addOrdered (list : Array Transfer) (limit : Nat) (t : Transfer) : Array Transfer :=
  let at_ := (list.findIdx? fun other => t.lt other).getD list.size
  if at_ == limit then list
  else
    let list := if list.size == limit then list.pop else list
    list.insertIdxIfInBounds at_ t

/-- `Utils.CenteredRectangle(player.Center.ToPoint(), PlayerViewRectSize).Contains(item.Center.ToPoint())`. -/
private def onScreen (viewCenters : Array Vector) (drop : Drop) : Bool :=
  let px := toInt (drop.position.x + 8)
  let py := toInt (drop.position.y + 8)
  viewCenters.any fun c =>
    let left := toInt c.x - viewWidth / 2
    let top := toInt c.y - viewHeight / 2
    left ≤ px && px < left + viewWidth && top ≤ py && py < top + viewHeight

/-- `EmergencyStacking.DistanceBetween`: `|(int)dx| + |(int)dy|` of the two positions. -/
private def distanceBetween (a b : Drop) : Int :=
  (toInt (a.position.x - b.position.x)).natAbs + (toInt (a.position.y - b.position.y)).natAbs

/-- `EmergencyStacking.FindBestTransfers` over `MemoStackableItems`. -/
def findTransfers (world : WorldItems) (viewCenters : Array Vector) (shimmering : Nat → Bool := fun _ => false) :
    Array Transfer := Id.run do
  let mut memo : Array (Option Stackable) := #[]
  for index in [0:worldItemSlots] do
    memo := memo.push <| match entry world index with
      | some d =>
        if d.stack.item != 0 && d.stack.count > 0 && d.stack.count < OriginalItemStats.maxStack d.stack.item
            && !shimmering index then
          some { index, item := d.stack.item, age := d.age, onScreen := onScreen viewCenters d }
        else none
      | none => none
  let mut list : Array Transfer := #[]
  for s? in memo do
    let some s := s? | continue
    let some sd := entry world s.index | continue
    let mut best : Option Transfer := none
    for d? in memo do
      let some d := d? | continue
      if d.item != s.item || d.index == s.index || s.preferredOver d then continue
      let some dd := entry world d.index | continue
      if !sd.stack.canStack dd.stack then continue
      let distance := distanceBetween sd dd
      if distance > maxTransferDistance then continue
      let (priority, step) := Data.stackingGroup s.item
      let t : Transfer := {
        src := s.index, dst := d.index, preservationOrder := priority, distance
        distanceOrder := distance / step + (if s.onScreen then onScreenPenalty else 0) }
      if best.all (t.lt ·) then best := some t
    if let some t := best then list := addOrdered list transfersPerSpawn t
  return list

/-- `EmergencyStacking.OnReachingMaxStack`: a full copper, silver or gold stack becomes one coin of
the next kind (`SetDefaults`, so the prefix resets and the stack becomes 1). -/
private def promote (stack : Stack) : Stack :=
  if 71 ≤ stack.item && stack.item ≤ 73 then { item := stack.item + 1, count := 1 } else stack

/-- `ProcessPendingTransfersRef`: each transfer first follows earlier transfers whose source it
targets while its destination has become air (`UpdateDestinationFromPreviousTransfers`). Then
`DoTransfer` moves `min(src.stack, dst.maxStack - dst.stack)` if the two still `CanStack`. In
single player every item is reserved for the local player, so `HasOwnership` always holds. The
result is the lowest source index that became air, or 400. -/
def processTransfers (world : WorldItems) (list : Array Transfer) : WorldItems × Nat := Id.run do
  let mut world := world
  let mut list := list
  let mut slot := worldItemSlots
  for k in [0:list.size] do
    let mut dst := list[k]!.dst
    let mut j : Int := (k : Int) - 1
    while isAir world dst && j ≥ 0 do
      if list[j.toNat]!.src == dst then dst := list[j.toNat]!.dst
      j := j - 1
    list := list.set! k { list[k]! with dst }
    let src := list[k]!.src
    if let (some s, some d) := (entry world src, entry world dst) then
      let n := if s.stack.canStack d.stack then
          min s.stack.count (OriginalItemStats.maxStack d.stack.item - d.stack.count) else 0
      if n != 0 then
        let s' := { s with stack := { s.stack with count := s.stack.count - n } }
        let mut d' := { d with stack := { d.stack with count := d.stack.count + n } }
        if d'.stack.count == OriginalItemStats.maxStack d'.stack.item then d' := { d' with stack := promote d'.stack }
        world := setEntry world src (if s'.stack.count == 0 then none else some s')
        world := setEntry world dst (some d')
    if isAir world src then slot := min slot src
  return (world, slot)

/-- `Item.PickAnItemSlotToSpawnItemOn` for single player (`timeItemSlotCannotBeReusedFor` stays 0).
In order: the first free slot; else the oldest heart or star older than 1200 (only reached when
no slot is free); else emergency stacking (whose transfers stay applied even if they free nothing);
else the oldest item with age above 0 (ties keep the lowest index); else 400. -/
def pickSlot (world : WorldItems) (viewCenters : Array Vector) (shimmering : Nat → Bool := fun _ => false) :
    WorldItems × Nat := Id.run do
  let mut pickup : Option Nat := none
  let mut best := pickupReplacementTime
  for index in [0:worldItemSlots] do
    match entry world index with
    | none => return (world, index)
    | some d =>
      if Data.isAPickup d.stack.item && d.age > best then
        best := d.age
        pickup := some index
  if let some index := pickup then return (world, index)
  let (world, slot) := processTransfers world (findTransfers world viewCenters shimmering)
  if slot < worldItemSlots then return (world, slot)
  let mut oldest := worldItemSlots
  let mut age : Nat := 0
  for index in [0:worldItemSlots] do
    let a : Nat := ((entry world index).map (·.age)).getD 0
    if a > age then
      age := a
      oldest := index
  return (world, oldest)

/-- `Item.NewItem(source, center, type, stack, prefix, ownership, velocity)` for the local player.
A null velocity draws `Main.rand` after the slot is picked:
`X = Next(-30, 31) * 0.1`, and `Y = Next(-40, -15) * 0.1` (`Next(-30, 31)` for `ItemNoGravity`).
A stack of 0 or less spawns nothing and returns 400. The spawned item keeps type, count and prefix,
but not `favorited`. -/
def newItem (world : WorldItems) (grid : Grid) (viewCenters : Array Vector) (at_ : Vector) (stack : Stack)
    (ownership : Ownership) (velocity : Option Vector) (rng : UnifiedRandom)
    (shimmering : Nat → Bool := fun _ => false) : WorldItems × Option Spawn × UnifiedRandom := Id.run do
  if stack.count == 0 || stack.item == 0 then return (world, none, rng)
  let (world, slot) := pickSlot world viewCenters shimmering
  let position : Vector := ⟨at_.x - 8, at_.y - 8⟩
  let (velocity, rng) := match velocity with
    | some v => (v, rng)
    | none =>
      let (x, rng) := rng.next (-30) 31
      let (y, rng) := if Data.noGravity stack.item then rng.next (-30) 31 else rng.next (-40) (-15)
      (⟨single x * 0.1, single y * 0.1⟩, rng)
  let drop : Drop := {
    stack := { stack with favorite := false }, position, velocity
    age := Data.overflowProtectionTimeOffset stack.item
    liquid := { wet := OriginalItems.spawnWet grid position }
    delays := ownership.delays }
  let spawn : Spawn := { slot, drop, ownership }
  let world := if slot < worldItemSlots then setEntry world slot (some drop) else world
  return (world, some spawn, rng)

/-! ## Player.dropItemCheck -/

/-- Inputs `dropItemCheck` reads that are not player-owned state. They are taken as
`Main.DoUpdate` and the player's control phase leave them before `Player.Update` reaches the drop. -/
structure Controls where
  /-- `Player.controlThrow` (Throw / Q triggers this frame). -/
  controlThrow : Bool := false
  /-- `Main.playerInventory`. -/
  playerInventory : Bool := false
  /-- `Main.drawingPlayerChat`. -/
  drawingPlayerChat : Bool := false
  /-- `Main.mouseRight`, `Main.mouseRightRelease`. -/
  mouseRight : Bool := false
  mouseRightRelease : Bool := false
  /-- `Player.mouseInterface`: the cursor is over UI, so right click does not drop. -/
  mouseInterface : Bool := false
  deriving BEq, Repr, Inhabited

/-- Player-side state the drop reads and writes. -/
structure State where
  inventory : Inventory
  /-- `Main.mouseItem`. -/
  mouse : Option Stack := none
  /-- `Player.selectedItem` as the previous frame left it (0-9 hotbar, 10-57 inventory, 58 = cursor). -/
  selected : Nat := 0
  /-- `Player.releaseThrow` from the previous frame (true once Throw has been up for a frame). -/
  releaseThrow : Bool := true
  /-- `Player.noThrow`. -/
  noThrow : Nat := 0
  /-- `Player.inventoryChestStack[selectedItem]` (chest quick-stack lock on the selected slot). -/
  selectedChestStackLocked : Bool := false
  deriving Repr

/-- The world the spawn writes to. -/
structure Env where
  grid : Grid
  body : Body
  /-- `Player.direction` (1 or -1). -/
  direction : Int
  world : WorldItems
  rng : UnifiedRandom
  /-- `WorldItem.shimmerTime != 0` per slot (excluded from emergency stacking). -/
  shimmering : Nat → Bool := fun _ => false

structure Result where
  state : State
  world : WorldItems
  rng : UnifiedRandom
  /-- Items spawned this call, in order: the inventory overflow of a cursor item put back when the
  inventory is closed, then the drop. -/
  spawns : Array Spawn := #[]
  /-- `SetItemAnimation(10)` ran (`itemAnimation = itemAnimationMax = 10`, `JustDroppedAnItem = true`,
  then `DropSelectedItem_InterruptActionsThatUseAnimations`). `ItemCheck` later in the same
  `Player.Update` decrements it, so a snapshot after the update shows 9. -/
  startedAnimation : Bool := false
  /-- The right click was used (`Main.mouseRightRelease = false`, `Player.releaseUseTile = false`). -/
  consumedRightClick : Bool := false
  /-- The cursor stack was swapped into the drop (`delayUseItem = true`, `controlUseItem = false`). -/
  blockedItemUse : Bool := false

/-- `inventory[selectedItem]`. Selection 58 is `inventory[58]`, which `dropItemCheck` has just set to a
clone of `Main.mouseItem`. `SelectedItemState.Update` selects 58 through `PickItemSelectionOverride`
whenever the cursor holds an item and the selection may change (`!UsingOrReusingItem && ItemTimeIsZero`).
That runs after `dropItemCheck`, so it shows up from the next frame on. -/
private def selectedStack (state : State) : Option Stack :=
  let selected := if state.selected == 58 then state.mouse else state.inventory[state.selected]?.join
  selected.filter fun stack => stack.count > 0 && stack.item != 0

private def rightClickDrop (c : Controls) : Bool := c.mouseRight && !c.mouseInterface && c.mouseRightRelease

/-- `Player.DropSelectedItem(slot, ref item)` with the item already chosen (58 = the cursor). -/
private def dropSlot (c : Controls) (env : Env) (s : State) (cursor : Bool) (item : Stack) :
    State × WorldItems × UnifiedRandom × Option Spawn × Bool × Bool := Id.run do
  let mut s := s
  let mut item := item
  if item.favorite then
    -- GetItem(item, ReturnItemFromSlot): a favorited stack goes back into the inventory
    let (inventory, rest) := OriginalItems.store s.inventory item
    s := { s with inventory }
    if cursor then s := { s with mouse := rest }
    match rest with
    | none => return (s, env.world, env.rng, none, false, false)
    | some r => item := r
  -- the right-click swap (loc1 = item; item = mouseItem): for slot 58 this is the same stack
  let swapped := (rightClickDrop c || !c.playerInventory) && s.mouse.isSome
  let ownership := if Data.dropWithGrabDelayForAllPlayers item.item then .droppedForAllPlayers else .dropped
  let velocity : Vector := ⟨single (4 * env.direction) + env.body.velocity.x, -2⟩
  let views := #[center env.body]
  let (world, spawn, rng) :=
    newItem env.world env.grid views (center env.body) item ownership (some velocity) env.rng env.shimmering
  s := if cursor then { s with mouse := none }
    else { s with inventory := s.inventory.set! s.selected none }
  -- the restore branch (item = loc1, no animation) needs a non-empty cursor here, which cannot
  -- happen: a cursor drop has just emptied it, and a slot drop only runs with an empty cursor
  return (s, world, rng, spawn, true, swapped)

/-- `Player.dropItemCheck` (local player, single player, guide and reforge slots empty). -/
def dropItemCheck (c : Controls) (env : Env) (s : State) : Result := Id.run do
  let mut s := { s with noThrow := if c.playerInventory then s.noThrow else 0 }
  if s.noThrow > 0 then s := { s with noThrow := s.noThrow - 1 }
  let mut env := env
  let mut spawns : Array Spawn := #[]
  -- a cursor item does not survive a closed inventory: GetOrDropItem(mouseItem, ReturnItemFromSlot)
  if let some held := s.mouse then
    if !c.playerInventory then
      let (inventory, rest) := OriginalItems.store s.inventory held
      s := { s with inventory, mouse := none }
      if let some r := rest then
        let (world, spawn, rng) := newItem env.world env.grid #[center env.body] (center env.body) r
          .inventoryOverflow none env.rng env.shimmering
        env := { env with world, rng }
        if let some sp := spawn then spawns := spawns.push sp
  let throwKey := c.controlThrow && s.releaseThrow
  let hotbar := throwKey && (selectedStack s).any (!·.favorite) && !c.drawingPlayerChat
  let cursor := (rightClickDrop c || !c.playerInventory) && s.mouse.isSome
  if !(hotbar || cursor) || (s.noThrow > 0 && !throwKey) then
    return { state := s, world := env.world, rng := env.rng, spawns }
  -- Player.DropSelectedItem()
  let target : Option (Bool × Stack) := match s.mouse with
    | some m => some (true, m)
    | none => if s.selectedChestStackLocked then none else (selectedStack s).map (false, ·)
  let mut animation := false
  let mut blocked := false
  if let some (isCursor, item) := target then
    let (s', world, rng, spawn, anim, swapped) := dropSlot c env s isCursor item
    s := s'
    env := { env with world, rng }
    if let some sp := spawn then spawns := spawns.push sp
    animation := anim
    blocked := swapped
  let consumed := c.playerInventory && rightClickDrop c
  return { state := s, world := env.world, rng := env.rng, spawns, startedAnimation := animation,
           consumedRightClick := consumed, blockedItemUse := blocked }

/-- One frame of drop handling inside `Player.Update`. `dropItemCheck` runs only while
`itemAnimation == 0 && itemTime == 0 && reuseDelay == 0` (`canDrop`). After it,
`releaseThrow = !controlThrow` runs every frame. -/
def step (canDrop : Bool) (c : Controls) (env : Env) (s : State) : Result :=
  let r := if canDrop then dropItemCheck c env s else { state := s, world := env.world, rng := env.rng }
  { r with state := { r.state with releaseThrow := !c.controlThrow } }

end Lean4Terria.OriginalItemThrow
