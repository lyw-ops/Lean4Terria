import Lean4Terria.OriginalItems

namespace Lean4Terria.OriginalBanks

open OriginalCollision (single tileIndex)
open OriginalItems (Inventory Stack)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalWorld (Terrain)

inductive Kind where
  | piggyBank | safe | defendersForge | voidVault
  deriving BEq, Repr

def kinds : Array Kind := #[.piggyBank, .safe, .defendersForge, .voidVault]

def Kind.index : Kind → Nat
  | .piggyBank => 0
  | .safe => 1
  | .defendersForge => 2
  | .voidVault => 3

def Kind.tileId : Kind → UInt16
  | .piggyBank => 29
  | .safe => 97
  | .defendersForge => 463
  | .voidVault => 491

def Kind.name : Kind → String
  | .piggyBank => "Piggy Bank"
  | .safe => "Safe"
  | .defendersForge => "Defender's Forge"
  | .voidVault => "Void Vault"

def Kind.keepsFavorites (kind : Kind) : Bool := kind == .voidVault

def slotCount : Nat := 40

structure Stores where
  piggyBank : Inventory := Array.replicate slotCount none
  safe : Inventory := Array.replicate slotCount none
  defendersForge : Inventory := Array.replicate slotCount none
  voidVault : Inventory := Array.replicate slotCount none
  voidVaultInfo : UInt8 := 0
  deriving BEq, Repr, Inhabited

def Stores.items (stores : Stores) : Kind → Inventory
  | .piggyBank => stores.piggyBank
  | .safe => stores.safe
  | .defendersForge => stores.defendersForge
  | .voidVault => stores.voidVault

def Stores.replace (stores : Stores) (kind : Kind) (inventory : Inventory) : Stores :=
  match kind with
  | .piggyBank => { stores with piggyBank := inventory }
  | .safe => { stores with safe := inventory }
  | .defendersForge => { stores with defendersForge := inventory }
  | .voidVault => { stores with voidVault := inventory }

def hasBag (inventory : Inventory) (id : UInt16) : Bool :=
  inventory.any (·.any (fun stack => stack.item == id && stack.count > 0))

def openVoidBag : UInt16 := 4131
def closedVoidBag : UInt16 := 5325

def Stores.voidVaultEnabled (stores : Stores) : Bool :=
  stores.voidVaultInfo &&& 1 != 0

/-- ItemSlot.TryItemSwap applies ChangeItemType to a single Void Bag.
The stack and favorite survive. Neither form accepts an item prefix. -/
def toggleBag (stack : Stack) : Option Stack := do
  guard (stack.count == 1)
  if stack.item == openVoidBag then return { stack with item := closedVoidBag, prefixId := 0 }
  if stack.item == closedVoidBag then return { stack with item := openVoidBag, prefixId := 0 }
  none

/-- Player.Update assigns bit 0 from the open bag in the carried inventory. -/
def Stores.updateVoidVaultInfo (stores : Stores) (inventory : Inventory) : Stores :=
  { stores with voidVaultInfo := if hasBag inventory openVoidBag then stores.voidVaultInfo ||| 1
                                else stores.voidVaultInfo &&& 0xfe }

def kindForTile : UInt16 → Option Kind
  | 29 => some .piggyBank
  | 97 => some .safe
  | 463 => some .defendersForge
  | 491 => some .voidVault
  | _ => none

def Kind.itemId : Kind → UInt16
  | .piggyBank => 87
  | .safe => 346
  | .defendersForge => 3813
  | .voidVault => 4076

def tileForItem (item : UInt16) : Option UInt16 :=
  (kinds.find? fun kind => kind.itemId == item).map Kind.tileId

/-- Object width and height with the native bottom-row placement origin. -/
def Kind.footprint : Kind → Nat × Nat × Nat
  | .piggyBank => (2, 1, 0)
  | .safe => (2, 2, 0)
  | .defendersForge => (3, 4, 1)
  | .voidVault => (3, 3, 1)

/-- GetBanksInRangeOf uses a 79-tile square around the player's center.
Tile duplicates share one inventory. The scan runs by column, then row. -/
def nearby (terrain : Terrain) (body : Body) (inventory : Inventory) : Array Kind := Id.run do
  let center := body.position + ⟨single body.width / 2, single body.height / 2⟩
  let cx := tileIndex center.x
  let cy := tileIndex center.y
  let startY := (cy - 39).toNat
  let stopY := min terrain.header.height (cy + 40).toNat
  let allowVoid := !hasBag inventory closedVoidBag
  let limit := if allowVoid then 4 else 3
  let mut found := #[]
  for x in [(cx - 39).toNat:min terrain.header.width (cx + 40).toNat] do
    for y in [startY:stopY] do
      let kind := (terrain.tile? x y).bind fun tile => tile.id.bind kindForTile
      let some kind := kind | continue
      if (kind != .voidVault || allowVoid) && !found.contains kind then
        found := found.push kind
        if found.size == limit then return found
  return found

structure Placed where
  kind : Kind
  anchor : Cell
  deriving BEq, Repr

/-- Forge and Void Vault use the middle column, third row as chestX/Y.
The two smaller containers use the top-left tile. -/
def atTarget (terrain : Terrain) (body : Body) (cell : Cell) : Option Placed := do
  guard (inToolRange body cell 5 3 0)
  let tile ← terrain.tile? cell.x cell.y
  let kind ← tile.id.bind kindForTile
  let (frameX, frameY) ← tile.frame
  let x := (cell.x : Int) - (frameX.tdiv 18) % 2
  let y := (cell.y : Int) - frameY.tdiv 18
  let anchor := if kind == .defendersForge || kind == .voidVault then
      (x + (if frameX == 36 then -1 else 1), y + 2)
    else (x, y)
  guard (anchor.1 ≥ 0 && anchor.2 ≥ 0)
  return ⟨kind, ⟨anchor.1.toNat, anchor.2.toNat⟩⟩

def inOpenRange (terrain : Terrain) (body : Body) (bank : Placed) : Bool := Id.run do
  let anchor := bank.anchor
  if !(terrain.tile? anchor.x anchor.y).any (fun tile => tile.id == some bank.kind.tileId) then return false
  let (dx, dy, width, height) : Int × Int × Int × Int := match bank.kind with
    | .piggyBank => (0, 0, 32, 16)
    | .safe => (0, 0, 32, 32)
    | .defendersForge | .voidVault => (-16, -32, 48, 64)
  let x := (anchor.x : Int) * 16 + dx
  let y := (anchor.y : Int) * 16 + dy
  let center := body.position + ⟨single body.width / 2, single body.height / 2⟩
  let nearX := max (single (x + 1)) (min center.x (single (x + width - 1)))
  let nearY := max (single (y + 1)) (min center.y (single (y + height - 1)))
  return inToolRange body ⟨(tileIndex nearX).toNat, (tileIndex nearY).toNat⟩ 5 3 0

end Lean4Terria.OriginalBanks
