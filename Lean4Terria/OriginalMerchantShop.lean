import Lean4Terria.OriginalMerchantShop.Tables

/-!
# Merchant shop (desktop 1.4.5.8): stage A

* `Chest.SetupShop(1)` (Merchant branch and the common pylon tail),
* `Chest.AddItemToShop` (with an empty sell-back helper),
* `Player.GetItemExpectedPrice` (no special currency), `Item.GetStoreValue`,
* `Utils.CoinsCount`, `Utils.CoinsCombineStacks`.

Not here yet: `BuyItem`/`TryPurchasing`, `SellItem`, `ShopHelper.GetShoppingSettings`/`ProcessMood`.
The price adjustment is therefore an input (`Player.currentShoppingSettings.PriceAdjustment`).
-/

namespace Lean4Terria.OriginalMerchantShop

/-! ## Items -/

/-- The `Terraria.Item` fields the shop code reads or writes. A default value is an empty slot. -/
structure ShopItem where
  type : UInt16 := 0
  stack : Int := 0
  maxStack : Int := 0
  value : Int := 0
  prefixId : UInt8 := 0
  favorited : Bool := false
  buyOnce : Bool := false
  isAShopItem : Bool := false
  shopCustomPrice : Option Int := none
  deriving BEq, Repr, Inhabited

private def parseRow (line : String) : Option (Nat × ShopItem) :=
  match (line.splitOn ",").map String.toInt? with
  | [some id, some type, some value, some maxStack] =>
    some (id.toNat, { type := type.toNat.toUInt16, stack := 1, maxStack, value })
  | _ => none

private def table : Array (Option ShopItem) :=
  (Tables.itemRows.splitOn "\n").toArray.filterMap parseRow |>.foldl
    (init := Array.replicate Tables.itemCount none) fun table (id, item) => table.set! id (some item)

/-- `Item.SetDefaults(id)`; `none` for id 0 and for ids the headless host cannot build. -/
def ShopItem.ofId (id : Nat) : Option ShopItem := (table[id]?).getD none

def ShopItem.isAir (item : ShopItem) : Bool := item.type == 0 || item.stack ≤ 0

/-- `Item.CanStack`. -/
def ShopItem.canStack (a b : ShopItem) : Bool := a.type == b.type && a.prefixId == b.prefixId

/-- `Item.GetStoreValue`. -/
def ShopItem.storeValue (item : ShopItem) : Int := item.shopCustomPrice.getD item.value

/-! ## Item.SetDefaults inputs of SetupShop -/

/-- Static world state `SetupShop` reads. -/
structure World where
  hardMode : Bool := false
  bloodMoon : Bool := false
  dayTime : Bool := true
  notTheBeesWorld : Bool := false
  remixWorld : Bool := false
  tenthAnniversaryWorld : Bool := false
  /-- `Main._shouldUseWindyDayMusic` (`IsItAHappyWindyDay`). -/
  windy : Bool := false
  /-- `BirthdayParty.PartyIsUp`. -/
  party : Bool := false
  downedBoss2 : Bool := false
  downedBoss3 : Bool := false
  worldSurface : Float := 0
  rockLayer : Float := 0
  maxTilesX : Int := 4200
  maxTilesY : Int := 1200

/-- The `Player.Zone*` properties the shop reads. -/
structure Zones where
  corrupt : Bool := false
  crimson : Bool := false
  snow : Bool := false
  desert : Bool := false
  beach : Bool := false
  jungle : Bool := false
  hallow : Bool := false
  glowshroom : Bool := false
  underworldHeight : Bool := false

/-- The local player as `SetupShop` sees it. -/
structure Viewer where
  zones : Zones := {}
  /-- `Entity.position` of the player (`width` 20, `height` 42). -/
  x : Float32 := 0
  y : Float32 := 0
  /-- `inventory[i].type` and `stack` for the player's inventory array. -/
  inventory : Array (UInt16 × Int) := #[]
  /-- `TeleportPylonsSystem.DoesPositionHaveEnoughNPCs(2, …)`. -/
  pylonNpcs : Bool := false

private def widthHalf : Float32 := 10
private def heightHalf : Float32 := 21

/-- `Player.HasItem(int)`: the first 58 inventory slots (found by the replay: the coin/ammo-free main inventory plus hotbar). -/
def Viewer.hasItem (viewer : Viewer) (type : UInt16) : Bool :=
  (viewer.inventory.toList.take 58).any fun (t, stack) => t == type && stack > 0

/-! ## Chest.SetupShop -/

private def shopSlots : Nat := 39

/-- The Merchant branch of `SetupShop` in order, as item ids. -/
private def merchantIds (w : World) (v : Viewer) : Array Nat := Id.run do
  let mut ids : Array Nat := #[88, 87, 35, 1991, 3509, 3506, 8]
  if w.notTheBeesWorld && !w.remixWorld then ids := ids.push 4388
  ids := ids.push 28
  if w.hardMode then ids := ids.push 188
  ids := ids.push 110
  if w.hardMode then ids := ids.push 189
  ids := ids ++ #[40, 42, 965]
  if v.zones.snow then ids := ids.push 967
  if v.zones.jungle || (w.tenthAnniversaryWorld && w.notTheBeesWorld && !w.remixWorld) then ids := ids.push 33
  if w.dayTime && w.windy then ids := ids.push 4074
  if w.bloodMoon then ids := ids.push 279
  if !w.dayTime then ids := ids.push 282
  if w.party then ids := ids.push 5643
  if w.downedBoss3 then ids := ids.push 346
  if w.hardMode then ids := ids.push 488
  -- the first inventory slot (of 58) holding a 930 adds 931 and 1614, once
  if (v.inventory.toList.take 58).any fun (t, _) => t == 930 then ids := ids ++ #[931, 1614]
  ids := ids.push 1786
  if w.hardMode then ids := ids.push 1348
  if w.hardMode then ids := ids.push 3198
  if w.downedBoss2 || w.downedBoss3 || w.hardMode then ids := ids ++ #[4063, 4673]
  if v.hasItem 3107 then ids := ids.push 3108
  return ids

/-- The pylon part of the common tail (IL_44ce ..). -/
private def pylonIds (w : World) (v : Viewer) : Array Nat := Id.run do
  let z := v.zones
  let mut ids : Array Nat := #[]
  -- merchant: type is not 19, 20 or 21
  if !(v.pylonNpcs && !z.corrupt && !z.crimson) then return ids
  let cx : Float32 := v.x + widthHalf
  let cy : Float32 := v.y + heightHalf
  let cy16 : Float32 := cy / 16
  let belowSurface : Bool := (Float32.toInt32 cy16).toInt.toFloat > w.worldSurface
  if !(z.snow || z.desert || z.beach || z.jungle || z.hallow || z.glowshroom) then
    if w.remixWorld then
      if cy16.toFloat > w.rockLayer && cy16 < Float32.ofInt (w.maxTilesY - 350) then ids := ids.push 4876
    else if !belowSurface then ids := ids.push 4876
  if z.snow then ids := ids.push 4920
  if z.desert then ids := ids.push 4919
  if z.underworldHeight then ids := ids.push 5652
  else if w.remixWorld then
    if !(z.snow || z.desert || z.beach || z.jungle || z.hallow) && belowSurface then ids := ids.push 4917
  else
    if !(z.snow || z.desert || z.beach || z.jungle || z.hallow || z.glowshroom) && belowSurface then ids := ids.push 4917
  let mut beach := z.beach && v.y.toFloat < w.worldSurface * 16.0
  if w.remixWorld then
    let px : Float32 := v.x / 16
    let py : Float32 := v.y / 16
    let outerX := px.toFloat < w.maxTilesX.toFloat * 0.43 || px.toFloat > w.maxTilesX.toFloat * 0.57
    beach := beach || (outerX && py.toFloat > w.rockLayer && py < Float32.ofInt (w.maxTilesY - 350))
  if beach then ids := ids.push 4918
  if z.jungle then ids := ids.push 4875
  if z.hallow then ids := ids.push 4916
  if z.glowshroom && !(w.remixWorld && z.underworldHeight) then ids := ids.push 4921
  return ids

/-- `Chest.SetupShop(1)`: the 40 slots of the chest afterwards. -/
def merchantShop (w : World) (v : Viewer) : Array ShopItem := Id.run do
  let mut slots : Array ShopItem := Array.replicate 40 {}
  let mut index := 0
  for id in merchantIds w v do
    if let some item := ShopItem.ofId id then
      slots := slots.set! index item
    index := index + 1
  for id in pylonIds w v do
    if index < shopSlots then
      if let some item := ShopItem.ofId id then
        slots := slots.set! index item
      index := index + 1
  for k in [:index] do
    slots := slots.modify k fun item => { item with isAShopItem := true }
  return slots

/-! ## Chest.AddItemToShop -/

/-- `Chest.AddItemToShop` with an empty `Main.shopSellbackHelper`: merges into buy-once stacks of the
first 39 slots, then takes the first empty slot as a clone with `favorited` cleared and `buyOnce` set. -/
def addItemToShop (slots : Array ShopItem) (item : ShopItem) : Array ShopItem := Id.run do
  let mut slots := slots
  let mut remaining := item.stack
  let mut i := 0
  while i < 39 && remaining > 0 do
    let slot := slots[i]!
    if slot.type != 0 || slot.stack != 0 then
      if slot.buyOnce && slot.canStack item then
        let moved := min remaining (slot.maxStack - slot.stack)
        slots := slots.set! i { slot with stack := slot.stack + moved }
        remaining := remaining - moved
    i := i + 1
  let mut j := 0
  while j < 39 && remaining > 0 do
    if slots[j]!.isAir then
      slots := slots.set! j { item with favorited := false, buyOnce := true, stack := remaining }
      return slots
    j := j + 1
  return slots

/-! ## Prices -/

/-- `Math.Round((double)x)` (half to even). -/
private def roundEven (x : Float) : Float :=
  let lower := x.floor
  let rest := x - lower
  if rest < 0.5 then lower
  else if rest > 0.5 then lower + 1
  else if (lower / 2).floor * 2 == lower then lower else lower + 1

private def toInt32Round (x : Float32) : Int := (roundEven x.toFloat).toInt32.toInt

/-- `Player.GetItemExpectedPrice` for items without a special currency: `(calcForSelling, calcForBuying)`.
`adjustment` is `currentShoppingSettings.PriceAdjustment`. -/
def expectedPrice (item : ShopItem) (discountAvailable : Bool) (adjustment : Float32) : Int × Int :=
  let value := item.storeValue
  let sell := value
  let buy := value
  let buy := if discountAvailable && !item.buyOnce then (Float32.ofInt buy * 0.8).toInt32.toInt else buy
  let sell := if discountAvailable && item.isAShopItem then buy else sell
  let (buy, sell) :=
    if item.buyOnce then (toInt32Round (Float32.ofInt buy / adjustment), toInt32Round (Float32.ofInt sell / adjustment))
    else (toInt32Round (Float32.ofInt buy * adjustment), toInt32Round (Float32.ofInt sell / adjustment))
  let buy := if item.buyOnce then buy / 5 else buy
  let buy := if item.buyOnce && value != 0 && buy < 1 then 1 else buy
  (sell, buy)

/-! ## Coins -/

/-- `Utils.CoinsCount` over item slots, skipping the given indices. -/
def coinsCount (slots : Array ShopItem) (skip : List Nat := []) : Int := Id.run do
  let mut total : Int := 0
  for i in [:slots.size] do
    if !skip.contains i then
      let s := slots[i]!
      total := total + (match s.type with
        | 71 => s.stack
        | 72 => s.stack * 100
        | 73 => s.stack * 10000
        | 74 => s.stack * 1000000
        | _ => 0)
  return total

/-- `Utils.CoinsCombineStacks`: the sum, and whether it reached the 9999999999 cap (then returns the cap). -/
def coinsCombine (counts : Array Int) : Bool × Int := Id.run do
  let mut total : Int := 0
  for c in counts do
    total := total + c
    if total ≥ 9999999999 then return (true, 9999999999)
  return (false, total)


/-! ## Player.BuyItem and Player.TryPurchasing -/

/-- A slot address: (storage index, slot) with storage 0 the inventory and 1..4 the four banks. -/
abbrev Pt := Nat × Nat

/-- `DelegateMethods.CompareYReverse`. -/
private def cmpY (a b : Pt) : Int := if b.2 < a.2 then -1 else if b.2 > a.2 then 1 else 0

private def swapAt (a : Array Pt) (i j : Nat) : Array Pt :=
  let x := a[i]!
  (a.set! i a[j]!).set! j x

private def swapIfGreater (a : Array Pt) (lo i j : Nat) : Array Pt :=
  if i != j && cmpY a[lo + i]! a[lo + j]! > 0 then swapAt a (lo + i) (lo + j) else a

private def insertionSort (a : Array Pt) (lo n : Nat) : Array Pt := Id.run do
  let mut a := a
  for i in [:n - 1] do
    let t := a[lo + i + 1]!
    let mut j : Int := i
    while j ≥ 0 && cmpY t a[lo + j.toNat]! < 0 do
      a := a.set! (lo + j.toNat + 1) a[lo + j.toNat]!
      j := j - 1
    a := a.set! (lo + (j + 1).toNat) t
  return a

private def pickPivotAndPartition (a : Array Pt) (lo size : Nat) : Array Pt × Nat := Id.run do
  let hi := size - 1
  let middle := hi / 2
  let mut a := swapIfGreater a lo 0 middle
  a := swapIfGreater a lo 0 hi
  a := swapIfGreater a lo middle hi
  let pivot := a[lo + middle]!
  a := swapAt a (lo + middle) (lo + hi - 1)
  let mut left := 0
  let mut right := hi - 1
  for _ in [:100000] do
    if left < right then
      left := left + 1
      while cmpY a[lo + left]! pivot < 0 do left := left + 1
      right := right - 1
      while cmpY pivot a[lo + right]! < 0 do right := right - 1
      if left >= right then break
      a := swapAt a (lo + left) (lo + right)
    else break
  if left != hi - 1 then a := swapAt a (lo + left) (lo + hi - 1)
  return (a, left)

/-- `ArraySortHelper<T>.DownHeap` on `[lo, lo + n)` with a 1-based index `i`. -/
private def downHeap (a : Array Pt) (lo i n : Nat) : Array Pt := Id.run do
  let mut a := a
  let mut i := i
  let d := a[lo + i - 1]!
  for _ in [:100000] do
    if i ≤ n / 2 then
      let mut child := 2 * i
      if child < n && cmpY a[lo + child - 1]! a[lo + child]! < 0 then child := child + 1
      if !(cmpY d a[lo + child - 1]! < 0) then break
      a := a.set! (lo + i - 1) a[lo + child - 1]!
      i := child
    else break
  return a.set! (lo + i - 1) d

/-- `ArraySortHelper<T>.HeapSort` on `[lo, lo + n)` (the introsort fallback at the depth limit). -/
private def heapSort (a : Array Pt) (lo n : Nat) : Array Pt := Id.run do
  let mut a := a
  let mut i := n / 2
  while i ≥ 1 do
    a := downHeap a lo i n
    i := i - 1
  i := n
  while i > 1 do
    a := swapAt a lo (lo + i - 1)
    a := downHeap a lo 1 (i - 1)
    i := i - 1
  return a

/-- `IntroSort` of `ArraySortHelper<T>` on `[lo, lo + size)`; the flag records that heap sort was used. -/
private partial def introSort (a : Array Pt) (lo size depth : Nat) : Array Pt × Bool :=
  if size ≤ 1 then (a, false)
  else if size ≤ 16 then
    if size == 2 then (swapIfGreater a lo 0 1, false)
    else if size == 3 then (swapIfGreater (swapIfGreater (swapIfGreater a lo 0 1) lo 0 2) lo 1 2, false)
    else (insertionSort a lo size, false)
  else if depth == 0 then (heapSort a lo size, true)
  else
    let (a, p) := pickPivotAndPartition a lo size
    let (a, h1) := introSort a (lo + p + 1) (size - p - 1) (depth - 1)
    let (a, h2) := introSort a lo p (depth - 1)
    (a, h1 || h2)

/-- `List<Point>.Sort(CompareYReverse)` and whether the depth limit sent a partition to heap sort. -/
def sortYInstrumented (a : Array Pt) : Array Pt × Bool :=
  if a.size ≤ 1 then (a, false) else introSort a 0 a.size (2 * (Nat.log2 a.size + 1))

/-- `List<Point>.Sort(CompareYReverse)`. -/
def sortY (a : Array Pt) : Array Pt := (sortYInstrumented a).1

/-- The player's money containers. `banks` are `bank`, `bank2`, `bank3`, `bank4`. -/
structure Wallet where
  inventory : Array ShopItem
  banks : Array (Array ShopItem)

private def Wallet.lists (w : Wallet) : Array (Array ShopItem) := #[w.inventory] ++ w.banks

private def isCoin (item : ShopItem) : Bool := 71 ≤ item.type.toNat && item.type.toNat ≤ 74

private def slotOf (lists : Array (Array ShopItem)) (p : Pt) : ShopItem := lists[p.1]![p.2]!
private def setSlot (lists : Array (Array ShopItem)) (p : Pt) (item : ShopItem) : Array (Array ShopItem) :=
  lists.set! p.1 (lists[p.1]!.set! p.2 item)

private structure Purchase where
  lists : Array (Array ShopItem)
  coins : Array Pt
  /-- Empty-slot lists: index 0 is the inventory's (`arg3`), 1..4 the banks'. -/
  empties : Array (Array Pt)
  backups : Array (Pt × ShopItem)
  price : Int

private def Purchase.backup (st : Purchase) (p : Pt) : Purchase :=
  let item := slotOf st.lists p
  match st.backups.findIdx? (·.1 == p) with
  | some k => { st with backups := st.backups.set! k (p, item) }
  | none => { st with backups := st.backups.push (p, item) }

/-- An item after `SetDefaults(0)` / `TurnToAir`: `maxStack` is 9999, unlike a fresh `new Item()` (0). -/
def ShopItem.air : ShopItem := { maxStack := 9999 }

private def coinItem (type : Nat) (stack : Int) : ShopItem :=
  { (ShopItem.ofId type).getD {} with stack }

/-- Break one coin of `slot` into 100 coins of `lowerType` placed in the first empty slot of `list`
(`arg3`, or the bank's list when that is not empty). Returns the new coin slot. -/
private def breakCoin (st : Purchase) (p : Pt) (bank : Nat) (lowerType : Nat) : Purchase × Pt := Id.run do
  let listIndex := if bank ≥ 1 && bank ≤ 4 && st.empties[bank]!.size > 0 then bank else 0
  let mut st := st
  let item := slotOf st.lists p
  let remaining := item.stack - 1
  st := { st with lists := setSlot st.lists p { item with stack := remaining } }
  if remaining ≤ 0 then
    st := { st with lists := setSlot st.lists p ShopItem.air, empties := st.empties.modify listIndex (·.push p) }
  let target := st.empties[listIndex]![0]!
  st := st.backup target
  st := { st with lists := setSlot st.lists target (coinItem lowerType 100),
                  empties := st.empties.modify listIndex (·.eraseIdx! 0) }
  return (st, target)

/-- `Player.TryPurchasing`: whether the payment succeeded, and the slots afterwards. A failure restores the
slots recorded in the backup dictionary only, so slots changed without a backup (the coin that is broken
for change) stay changed. -/
private def tryPurchasing (price : Int) (lists : Array (Array ShopItem)) (coins : Array Pt) (empties : Array (Array Pt)) :
    Bool × Array (Array ShopItem) := Id.run do
  let mut st : Purchase := { lists, coins, empties, backups := #[], price }
  for _ in [:100000] do
    if st.price ≤ 0 then return (true, st.lists)
    -- pay with coins of each denomination, largest first
    let mut cv : Int := 1000000
    for d in [:4] do
      if st.price ≥ cv then
        for p in st.coins do
          if (slotOf st.lists p).type.toNat == 74 - d then
            let n := st.price / cv
            st := st.backup p
            let item := slotOf st.lists p
            let before := item.stack
            if n < item.stack then
              st := { st with lists := setSlot st.lists p { item with stack := item.stack - n } }
            else
              st := { st with lists := setSlot st.lists p ShopItem.air, empties := st.empties.modify 0 (·.push p) }
            let now := (slotOf st.lists p).stack
            st := { st with price := st.price - cv * (before - now) }
      cv := cv / 100
    if st.price > 0 then
      if st.empties[0]!.size == 0 then
        -- restore every slot that was touched
        let mut lists := st.lists
        for (p, item) in st.backups do lists := setSlot lists p item
        return (false, lists)
      st := { st with empties := st.empties.set! 0 (sortY st.empties[0]!) }
      let mut found : Option Pt := none
      for bank in [:st.lists.size] do
        if found.isNone then
          let mut cv2 : Int := 10000
          for d2 in [:3] do
            if found.isNone then
              if st.price ≥ cv2 then
                for p in st.coins do
                  if found.isNone && p.1 == bank && (slotOf st.lists p).type.toNat == 74 - d2 && (slotOf st.lists p).stack ≥ 1 then
                    let (st', q) := breakCoin st p bank (73 - d2)
                    st := st'
                    found := some q
              if found.isNone then cv2 := cv2 / 100
          for k in [:2] do
            if found.isNone then
              for p in st.coins do
                if found.isNone && p.1 == bank && (slotOf st.lists p).type.toNat == 73 + k && (slotOf st.lists p).stack ≥ 1 then
                  let (st', q) := breakCoin st p bank (72 + k)
                  st := st'
                  found := some q
          if let some q := found then st := { st with coins := st.coins.push q }
      st := { st with empties := st.empties.map sortY }
  return (false, st.lists)

/-- `Player.BuyItem(price, -1)`: whether the player paid, and the wallet afterwards. -/
def buyItem (w : Wallet) (price : Int) : Bool × Wallet := Id.run do
  let counts := #[coinsCount w.inventory [54, 55, 56, 57, 58]] ++ w.banks.map (coinsCount · [])
  let (_, total) := coinsCombine counts
  if total < price then return (false, w)
  let lists := w.lists
  let mut coins : Array Pt := #[]
  let mut empties : Array (Array Pt) := Array.replicate lists.size #[]
  for i in [:lists.size] do
    let ignore : List Nat := if i == 0 then [58, 57, 56, 55, 54] else []
    let slots := lists[i]!
    for j in [:slots.size] do
      if !ignore.contains j && isCoin slots[j]! then coins := coins.push (i, j)
    for k in [:slots.size] do
      let j := slots.size - 1 - k
      if !ignore.contains j && (slots[j]!.type == 0 || slots[j]!.stack == 0) then
        empties := empties.modify i (·.push (i, j))
  let (ok, lists) := tryPurchasing price lists coins empties
  return (ok, { inventory := lists[0]!, banks := lists.extract 1 lists.size })


/-! ## Player.SellItem -/

/-- `Item.TurnToAir`. -/
private def turnToAir (item : ShopItem) : ShopItem :=
  if item.type != 0 || item.stack != 0 then ShopItem.air else item

/-- `Player.DoCoins(slot)`: a full stack of copper, silver or gold becomes one coin of the next kind and is
merged into another slot of the first 54 that still has room (recursively). -/
private partial def doCoins (inv : Array ShopItem) (i : Nat) : Array ShopItem := Id.run do
  let it := inv[i]!
  if it.stack == 100 && (it.type == 71 || it.type == 72 || it.type == 73) then
    let mut inv := inv.set! i (coinItem (it.type.toNat + 1) 1)
    for j in [:54] do
      let ii := inv[i]!
      let ij := inv[j]!
      if ij.type == ii.type && j != i && ij.stack < ij.maxStack then
        inv := inv.set! j { ij with stack := ij.stack + 1 }
        inv := inv.set! i (turnToAir inv[i]!)
        inv := doCoins inv j
    return inv
  return inv

/-- One `ItemShopSellbackHelper` memo (`ItemMemo`): items the player sold to this shop. -/
structure Memo where
  type : UInt16
  prefixId : UInt8
  stack : Int

/-- `ItemShopSellbackHelper.GetAmount` (items are the same when their type is). -/
def memoAmount (memos : Array Memo) (item : ShopItem) : Int :=
  match memos.find? fun m => m.type == item.type && m.prefixId == item.prefixId with
  | some m => m.stack
  | none => 0

/-- `ItemShopSellbackHelper.Add`. -/
def memoAdd (memos : Array Memo) (item : ShopItem) : Array Memo :=
  match memos.findIdx? fun m => m.type == item.type && m.prefixId == item.prefixId with
  | some k => memos.modify k fun m => { m with stack := m.stack + item.stack }
  | none => memos.push { type := item.type, prefixId := item.prefixId, stack := item.stack }

/-- `Player.SellItem(item, stack)` (`stack = -1` sells the whole stack): whether the sale was paid, and the
inventory afterwards. Money goes into the first 54 slots; a failure restores the first 58 slots. -/
def sellItem (inventory : Array ShopItem) (memos : Array Memo) (item : ShopItem) (stackArg : Int)
    (discountAvailable : Bool) (adjustment : Float32) : Bool × Array ShopItem := Id.run do
  let (sell, buy) := expectedPrice item discountAvailable adjustment
  if sell ≤ 0 then return (false, inventory)
  let stack := if stackArg == -1 then item.stack else stackArg
  let backup := inventory.extract 0 58
  let unit := max 1 (sell / 5)
  let mut total := unit * stack
  let amount := memoAmount memos item
  if amount > 0 then total := total + (buy - unit) * (min amount item.stack)
  let mut inv := inventory
  let mut failed := false
  for (type, value) in [(74, 1000000), (73, 10000), (72, 100), (71, 1)] do
    for _ in [:1000000] do
      if total ≥ value && !failed then
        let mut empty : Option Nat := none
        for k in [:54] do
          let i := 53 - k
          if empty.isNone && (inv[i]!.type == 0 || inv[i]!.stack == 0) then empty := some i
          -- the merge repeats on one slot while it still has room and enough is owed
          for _ in [:100000] do
            if inv[i]!.type.toNat == type && inv[i]!.stack < inv[i]!.maxStack && total ≥ value then
              inv := inv.set! i { inv[i]! with stack := inv[i]!.stack + 1 }
              total := total - value
              inv := doCoins inv i
              if inv[i]!.stack == 0 && empty.isNone then empty := some i
            else break
        if total ≥ value then
          match empty with
          | none => failed := true
          | some e =>
            inv := inv.set! e (coinItem type 1)
            total := total - value
      else break
  if failed then
    for i in [:58] do inv := inv.set! i backup[i]!
    return (false, inv)
  return (true, inv)


/-! ## ShopHelper.GetShoppingSettings (Merchant) -/

private def mf (bits : UInt32) : Float32 := Float32.ofBits bits

/-- An entry of `Main.npc`; index 0 of the array handed to `merchantPriceAdjustment` is the shopkeeper. -/
structure Resident where
  type : Nat
  active : Bool := true
  townNPC : Bool := true
  housingCategory : Nat := 0
  homeless : Bool := false
  homeTileX : Int := 0
  homeTileY : Int := 0
  /-- `Entity.Center`. -/
  cx : Float32 := 0
  cy : Float32 := 0
  loveStruck : Bool := false
  deriving Inhabited

/-- The `Player` zone properties `ProcessMood` reads. -/
structure MoodZones where
  dungeon : Bool := false
  corrupt : Bool := false
  crimson : Bool := false
  glowshroom : Bool := false
  hallow : Bool := false
  jungle : Bool := false
  snow : Bool := false
  beach : Bool := false
  desert : Bool := false

/-- `Vector2.Distance` in single precision. -/
private def distance (ax ay bx by_ : Float32) : Float32 :=
  let dx := ax - bx
  let dy := ay - by_
  (Float.sqrt (dx * dx + dy * dy).toFloat).toFloat32

private def notReallyTown (r : Resident) : Bool := r.type == 37 || r.type == 368 || r.type == 453

/-- `MathHelper.Clamp(x, 0.75, 1.5)` then `Math.Round(x * 100) / 100`. -/
private def limitAndRound (x : Float32) : Float32 :=
  let x := if x > mf 1069547520 then mf 1069547520 else x
  let x := if x < mf 1061158912 then mf 1061158912 else x
  let scaled := (x * mf 1120403456).toFloat
  let lower := scaled.floor
  let rest := scaled - lower
  let rounded := if rest < 0.5 then lower else if rest > 0.5 then lower + 1
    else if (lower / 2).floor * 2 == lower then lower else lower + 1
  rounded.toFloat32 / mf 1120403456

/-- `ShopHelper.GetShoppingSettings(player, npc).PriceAdjustment` for the Merchant (index 0 of `residents`).
`belowSurface` is `Player.ShoppingZone_BelowSurface`. Report text is not modelled. -/
def merchantPriceAdjustment (remixWorld : Bool) (zones : MoodZones) (belowSurface : Bool)
    (residents : Array Resident) : Float32 := Id.run do
  let keeper := residents[0]!
  let mut adj : Float32 := 1
  if keeper.loveStruck then adj := adj * mf 1063675494
  if remixWorld then return adj
  -- RuinMoodIfHomeless, else IsFarFromHome
  let center := (keeper.cx / 16, keeper.cy / 16)
  if keeper.homeless then adj := mf 1148846080
  else if distance (Float32.ofInt keeper.homeTileX) (Float32.ofInt keeper.homeTileY) center.1 center.2 > mf 1123024896 then
    adj := mf 1148846080
  -- IsPlayerInEvilBiomes: Corruption, Crimson, Dungeon
  if adj < mf 1148846080 && (zones.corrupt || zones.crimson || zones.dungeon) then adj := mf 1148846080
  -- GetNearbyResidentNPCs
  let home := if keeper.homeless then center else (Float32.ofInt keeper.homeTileX, Float32.ofInt keeper.homeTileY)
  let mut near : Nat := 0
  let mut far : Nat := 0
  let mut byType : Array Bool := Array.replicate 700 false
  let mut present : Array Nat := #[]
  for k in [1:residents.size] do
    let other := residents[k]!
    if other.active && other.townNPC && !notReallyTown other && !(keeper.housingCategory != other.housingCategory) then
      let spot := if other.homeless then (other.cx / 16, other.cy / 16)
        else (Float32.ofInt other.homeTileX, Float32.ofInt other.homeTileY)
      let d := distance home.1 home.2 spot.1 spot.2
      if d < mf 1103626240 then
        near := near + 1
        present := present.push other.type
      else if d < mf 1123024896 then far := far + 1
  for t in present do byType := byType.set! t true
  -- crowding (the Merchant is not Princess 663)
  if near > 3 then
    for _ in [3:near] do adj := adj * mf 1065772646
  if near ≤ 2 && far < 4 then adj := adj * mf 1064514355
  -- ShopModifiers of the Merchant: Forest (Like), Desert (Dislike); the preferred one the player is in
  let anyBiome := zones.dungeon || zones.corrupt || zones.crimson || zones.glowshroom || zones.hallow || zones.jungle ||
    zones.snow || zones.beach || zones.desert
  let inForest := !anyBiome && !belowSurface
  if zones.desert then adj := adj * mf 1065856532   -- Dislike beats nothing; Forest and Desert are exclusive here
  else if inForest then adj := adj * mf 1064346583
  -- AllPersonalitiesModifier: Princess is nearby
  if byType[663]! then adj := adj * mf 1064346583
  if byType[588]! then adj := adj * mf 1064346583
  if byType[18]! then adj := adj * mf 1064346583
  if byType[441]! then adj := adj * mf 1065856532
  if byType[369]! then adj := adj * mf 1066359849
  return limitAndRound adj

end Lean4Terria.OriginalMerchantShop
