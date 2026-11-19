import Lean4Terria.Inventory

namespace Lean4Terria

inductive GearSlot where
  | head | armor | legs | accessory | hook | mount
  deriving BEq, Repr

structure Equipment where
  head : Option Item := none
  armor : Option Item := none
  legs : Option Item := none
  accessory : Option Item := none
  hook : Option Item := none
  mount : Option Item := none
  deriving BEq, Repr, Inhabited

def Item.gearSlot : Item → Option GearSlot
  | .crystalAssassinHood => some .head
  | .geodeMail | .deepstoneAegis | .crystalAssassinShirt => some .armor
  | .crystalAssassinPants => some .legs
  | .amberTreads | .raidCrest | .shackle | .cobaltShield | .nazar | .obsidianSkull | .obsidianRose
  | .magmaStone | .treasureMagnet | .fastClock | .trifoldMap | .vitamins | .lavaproofFishingHook => some .accessory
  | .highTestFishingLine | .anglerEarring | .tackleBox | .anglerBobber => some .accessory
  | .summonerEmblem => some .accessory
  | .grapplingHook | .hookOfDissonance => some .hook
  | .gelatinousPillion => some .mount
  | _ => none

def geodeMailGeodes : Nat := 3
def geodeMailSilver : Nat := 2
def maxEquippedSpeed : Int := 42

def Equipment.crystalAssassinSet (equipment : Equipment) : Bool :=
  equipment.head == some .crystalAssassinHood &&
    equipment.armor == some .crystalAssassinShirt && equipment.legs == some .crystalAssassinPants

def Equipment.damageBonus (equipment : Equipment) : Nat :=
  if equipment.crystalAssassinSet then 15
  else if equipment.armor == some .crystalAssassinShirt then 5 else 0

def Equipment.summonDamageBonus (equipment : Equipment) : Nat :=
  equipment.damageBonus + if equipment.accessory == some .summonerEmblem then 15 else 0

def Equipment.criticalBonus (equipment : Equipment) : Nat :=
  if equipment.crystalAssassinSet then 15
  else if equipment.head == some .crystalAssassinHood then 5 else 0

def Equipment.ammoSavings (equipment : Equipment) : Nat :=
  if equipment.armor == some .crystalAssassinShirt then 10 else 0

def Equipment.manaCost (equipment : Equipment) (base : Nat) : Nat :=
  if equipment.head == some .crystalAssassinHood then base * 90 / 100 else base

def Equipment.meleeTicks (equipment : Equipment) (base : Nat) : Nat :=
  if equipment.legs == some .crystalAssassinPants then max 1 (base * 100 / 110) else base

def Equipment.meleeSpeed (equipment : Equipment) : Float32 :=
  if equipment.legs == some .crystalAssassinPants then 1.1 else 1

structure WeaponArmorStats where
  damageBonus : Nat := 0
  criticalChance : Nat := 0
  deriving BEq, Repr, Inhabited

def Equipment.weaponArmorStats (equipment : Equipment) : WeaponArmorStats :=
  let bonus := equipment.criticalBonus
  { damageBonus := equipment.damageBonus, criticalChance := if bonus > 0 then 4 + bonus else 0 }

def WeaponArmorStats.scaleDamage (stats : WeaponArmorStats) (base : Nat) : Nat :=
  base * (100 + stats.damageBonus) / 100

def WeaponArmorStats.valid (stats : WeaponArmorStats) : Bool :=
  (stats.damageBonus == 15 && stats.criticalChance == 19) ||
    ((stats.damageBonus == 0 || stats.damageBonus == 5) &&
      (stats.criticalChance == 0 || stats.criticalChance == 9) && stats != {})

structure AssassinDash where
  tap : Int := 0
  velocity : Float32 := 0
  cooldown : Nat := 0
  deriving Repr, Inhabited

instance : BEq AssassinDash where
  beq a b := a.tap == b.tap && a.velocity.toBits == b.velocity.toBits && a.cooldown == b.cooldown

def assassinDashSpeed : Float32 := 16.9
def assassinDashCooldown : Nat := 20

def AssassinDash.valid (dash : AssassinDash) : Bool :=
  dash.tap.natAbs ≤ 15 && dash.cooldown ≤ assassinDashCooldown && dash.velocity.isFinite &&
    dash.velocity.abs ≤ assassinDashSpeed && (dash.velocity == 0 || (dash.cooldown == 0 && dash.tap == 0))

def Equipment.defense (equipment : Equipment) : Int :=
  let armor := match equipment.armor with
    | some .deepstoneAegis => 2
    | some .geodeMail => 1
    | some .crystalAssassinShirt => 14
    | _ => 0
  let head := if equipment.head == some .crystalAssassinHood then 12 else 0
  let legs := if equipment.legs == some .crystalAssassinPants then 10 else 0
  head + armor + legs +
    (if equipment.accessory == some .raidCrest || equipment.accessory == some .shackle ||
        equipment.accessory == some .cobaltShield || equipment.accessory == some .obsidianSkull then 1 else 0)

def Equipment.speed (equipment : Equipment) : Int :=
  let speed := match equipment.accessory with
    | some .amberTreads | some .raidCrest => maxEquippedSpeed
    | _ => maxHorizontalSpeed
  if equipment.legs == some .crystalAssassinPants then speed * 120 / 100 else speed

def Equipment.get (equipment : Equipment) : GearSlot → Option Item
  | .head => equipment.head
  | .armor => equipment.armor
  | .legs => equipment.legs
  | .accessory => equipment.accessory
  | .hook => equipment.hook
  | .mount => equipment.mount

private def Equipment.set (equipment : Equipment) (slot : GearSlot) (item : Option Item) : Equipment :=
  match slot with
  | .head => { equipment with head := item }
  | .armor => { equipment with armor := item }
  | .legs => { equipment with legs := item }
  | .accessory => { equipment with accessory := item }
  | .hook => { equipment with hook := item }
  | .mount => { equipment with mount := item }

/-- Equip one item and put any previously worn item in the clicked inventory slot. -/
def equipFromSlot (inventory : Inventory) (equipment : Equipment) (index : Nat) :
    Except String (Inventory × Equipment) := do
  let some stack := inventory[index]?.join | throw "Choose a piece of equipment."
  let some slot := stack.item.gearSlot | throw "That item does not go in an equipment slot."
  if stack.count != 1 then throw "Equipment must be a single item."
  let previous : Option Stack := (equipment.get slot).map (fun item => ⟨item, 1⟩)
  return (inventory.set! index previous, equipment.set slot (some stack.item))

def unequipGear (inventory : Inventory) (equipment : Equipment) (slot : GearSlot) :
    Except String (Inventory × Equipment) := do
  let some item := equipment.get slot | throw "There is nothing in that equipment slot."
  let some (inventory, _) := Inventory.addOne inventory item
    | throw "Make room in your inventory before taking off equipment."
  return (inventory, equipment.set slot none)

def clickGear (cursor : Option Stack) (equipment : Equipment) (slot : GearSlot) :
    Option Stack × Equipment :=
  if cursor.any (fun stack => stack.item.gearSlot != some slot || stack.count != 1) then
    (cursor, equipment)
  else
    ((equipment.get slot).map (fun item => ⟨item, 1⟩), equipment.set slot (cursor.map (·.item)))

def damageAfterDefense (raw defense : Nat) : Nat :=
  if raw == 0 then 0 else max 1 (raw - defense)

end Lean4Terria
