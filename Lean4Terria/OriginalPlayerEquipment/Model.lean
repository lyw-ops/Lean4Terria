/-! The equipment state of a desktop 1.4.5.8 player as `Player.Serialize` (release 326) stores it:
`armor[20]`, `dye[10]`, `miscEquips[5]`, `miscDyes[5]`, the visibility flags, `extraAccessory`,
`CurrentLoadoutIndex` and the three `EquipmentLoadout`s. Slot meaning in `armor`: 0 head, 1 body,
2 legs, 3–7 accessories, 8 the Demon Heart slot, 9 the Master Mode slot, 10–12 vanity armor,
13–19 vanity accessories; `dye[i]` colours `armor[i]`. `miscEquips`: pet, light pet, minecart,
mount, hook. Evidence: `.lake/original-player-equipment/REPORT.md`. -/
namespace Lean4Terria.OriginalPlayerEquipment

/-- One stored item: `Item.type`, `Item.stack`, `Item.prefix`, `Item.favorited`. The player's own
`armor`, `dye`, `miscEquips` and `miscDyes` records have no stack field; decoding gives them stack 1
(0 for an empty slot). `miscEquips` and `miscDyes` records have no favourite flag either. -/
structure Slot where
  item : UInt16 := 0
  stack : Nat := 0
  prefixId : UInt8 := 0
  favorited : Bool := false
  deriving BEq, Repr, Inhabited

def Slot.isAir (slot : Slot) : Bool := slot.item == 0 || slot.stack == 0

def Slot.ofItem (item : UInt16) (prefixId : UInt8 := 0) (favorited : Bool := false) : Slot :=
  if item == 0 then {} else { item, stack := 1, prefixId, favorited }

/-- `Terraria.EquipmentLoadout`: `Armor[20]`, `Dye[10]`, `Hide[10]`. -/
structure Loadout where
  armor : Array Slot := Array.replicate 20 {}
  dye : Array Slot := Array.replicate 10 {}
  hide : Array Bool := Array.replicate 10 false
  deriving BEq, Repr, Inhabited

structure Equipment where
  armor : Array Slot := Array.replicate 20 {}
  dye : Array Slot := Array.replicate 10 {}
  miscEquips : Array Slot := Array.replicate 5 {}
  miscDyes : Array Slot := Array.replicate 5 {}
  /-- `Player.hideVisibleAccessory[0..9]`. -/
  hideVisibleAccessory : Array Bool := Array.replicate 10 false
  /-- `Player.hideMisc` as its byte. -/
  hideMisc : UInt8 := 0
  /-- `Player.extraAccessory` (Demon Heart used). -/
  extraAccessory : Bool := false
  /-- `Player.CurrentLoadoutIndex`. The active loadout lives in `armor` / `dye` /
  `hideVisibleAccessory`; `loadouts[currentLoadout]` is whatever the file holds for it. -/
  currentLoadout : Nat := 0
  loadouts : Array Loadout := Array.replicate 3 {}
  deriving BEq, Repr, Inhabited

/-- The array sizes `Player.Serialize` writes. -/
def Equipment.wellFormed (equipment : Equipment) : Bool :=
  equipment.armor.size == 20 && equipment.dye.size == 10 && equipment.miscEquips.size == 5 &&
    equipment.miscDyes.size == 5 && equipment.hideVisibleAccessory.size == 10 &&
    equipment.currentLoadout < 3 && equipment.loadouts.size == 3 &&
    equipment.loadouts.all fun loadout =>
      loadout.armor.size == 20 && loadout.dye.size == 10 && loadout.hide.size == 10

end Lean4Terria.OriginalPlayerEquipment
