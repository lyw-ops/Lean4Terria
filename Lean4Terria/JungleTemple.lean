import Lean4Terria.ClassicCrafting
import Lean4Terria.Hardmode

namespace Lean4Terria

namespace TempleDoor

private def lockedTiles : Array Tile :=
  #[.lihzahrdDoorLocked0, .lihzahrdDoorLocked1, .lihzahrdDoorLocked2]

private def closedTiles : Array Tile :=
  #[.lihzahrdDoorClosed0, .lihzahrdDoorClosed1, .lihzahrdDoorClosed2]

inductive Position where
  | locked | closed | openLeft | openRight
  deriving BEq, Repr

def kind (tile : Tile) : Option Position :=
  if lockedTiles.contains tile then some .locked
  else if closedTiles.contains tile then some .closed
  else match tile with
    | .lihzahrdDoorLeft | .lihzahrdDoorLeftLeaf => some .openLeft
    | .lihzahrdDoorRight | .lihzahrdDoorRightLeaf => some .openRight
    | _ => none

def cells (top : TilePos) : Array TilePos :=
  #[top, { top with y := top.y + 1 }, { top with y := top.y + 2 }]

private def leafColumn (top : TilePos) (mode : Position) : TilePos :=
  { top with x := top.x + if mode == .openLeft then -1 else 1 }

private def leafTile (mode : Position) : Tile :=
  if mode == .openLeft then .lihzahrdDoorLeftLeaf else .lihzahrdDoorRightLeaf

private def hingeTile (mode : Position) : Tile :=
  if mode == .openLeft then .lihzahrdDoorLeft else .lihzahrdDoorRight

private def openMode (mode : Position) : Bool :=
  mode == .openLeft || mode == .openRight

def supports (world : World) (top : TilePos) : Bool :=
  let anchors (tile : Tile) := tile.solid && !tile.isDoor && (kind tile).isNone
  (world.tile? { top with y := top.y - 1 }).any anchors &&
    (world.tile? { top with y := top.y + 3 }).any anchors

private def complete (world : World) (top : TilePos) (mode : Position) : Bool :=
  if openMode mode then
    (cells top).all (fun p => world.tile p == hingeTile mode) &&
      (cells (leafColumn top mode)).all (fun p => world.tile p == leafTile mode)
  else (cells top).all (fun p => kind (world.tile p) == some mode)

/-- Resolve any of the original three closed or six open cells to the top hinge. -/
def atCell (world : World) (position : TilePos) : Option (TilePos × Position) := do
  let tile := world.tile position
  let mode ← kind tile
  let x := if tile == .lihzahrdDoorLeftLeaf then position.x + 1
    else if tile == .lihzahrdDoorRightLeaf then position.x - 1 else position.x
  [0, 1, 2].findSome? fun dy =>
    let top : TilePos := ⟨x, position.y - dy⟩
    if complete world top mode then some (top, mode) else none

def protects (world : World) (position : TilePos) : Bool :=
  [1, -1].any fun offset =>
    let tile := world.tile { position with y := position.y + offset }
    kind tile == some .locked

def place (world : World) (top : TilePos) (locked : Bool) (variant : TilePos → Nat) : World :=
  let variants := if locked then lockedTiles else closedTiles
  (cells top).foldl (fun terrain p => terrain.setTile p variants[variant p % 3]!) world

def unlock (world : World) (top : TilePos) : Option World := do
  if !complete world top .locked || !supports world top then none else
  some ((cells top).foldl (fun terrain p =>
    let frame := (lockedTiles.findIdx? (· == world.tile p)).getD 0
    terrain.setTile p closedTiles[frame]!) world)

def openIn (world : World) (top : TilePos) (direction : Int)
    (canSweep : Tile → Bool) : Option World := do
  let mode := if direction < 0 then Position.openLeft else .openRight
  let leaf := cells (leafColumn top mode)
  if !complete world top .closed ||
      !leaf.all (fun p => world.contains p && canSweep (world.tile p)) then none else
  let opened := (cells top).foldl (fun terrain p => terrain.setTile p (hingeTile mode)) world
  some (leaf.foldl (fun terrain p => terrain.setTile p (leafTile mode)) opened)

def close (world : World) (top : TilePos) (mode : Position) (occupied : TilePos → Bool)
    (variant : TilePos → Nat) : Option World := do
  if !openMode mode || !complete world top mode || (cells top).any occupied then none else
  let cleared := (cells (leafColumn top mode)).foldl (fun terrain p => terrain.setTile p .air) world
  some (place cleared top false variant)

def remove (world : World) (top : TilePos) (mode : Position) : World :=
  let footprint := if openMode mode then cells top ++ cells (leafColumn top mode) else cells top
  footprint.foldl (fun terrain p => terrain.setTile p .air) world

/-- Texture and source rectangle origin, in the original Tiles_10/Tiles_11 atlases. -/
def frame (world : World) (position : TilePos) : Option (Nat × Nat × Nat) := do
  let (top, mode) ← atCell world position
  let row := (position.y - top.y).toNat
  let tile := world.tile position
  let closedFrame (variants : Array Tile) := (variants.findIdx? (· == tile)).getD 0 * 18
  return match mode with
    | .locked => (10, closedFrame lockedTiles, 594 + row * 18)
    | .closed => (10, closedFrame closedTiles, 648 + row * 18)
    | .openLeft => (11, if position.x == top.x then 54 else 36, 648 + row * 18)
    | .openRight => (11, if position.x == top.x then 0 else 18, 648 + row * 18)

end TempleDoor

structure TempleLayout where
  origin : TilePos
  deriving BEq, Repr, Inhabited

namespace TempleLayout

def width : Nat := 39
def height : Nat := 19

def worldCell (layout : TempleLayout) (x y : Nat) : TilePos :=
  ⟨layout.origin.x + (width - 1 - x : Nat), layout.origin.y + y⟩

def door (layout : TempleLayout) : TilePos := layout.worldCell 4 2

def chestAnchors (layout : TempleLayout) : Array TilePos :=
  #[layout.worldCell 16 4, layout.worldCell 22 10, layout.worldCell 32 16]

def altar (layout : TempleLayout) : TilePos := layout.worldCell 26 16

def altarCells (anchor : TilePos) : Array TilePos :=
  (Array.range 6).map fun (i : Nat) => ⟨anchor.x + (i % 3 : Nat), anchor.y + (i / 3 : Nat)⟩

private def framedAltar : Array Tile := #[
  .lihzahrdAltarUpperLeft, .lihzahrdAltarUpperMiddle, .lihzahrdAltarUpperRight,
  .lihzahrdAltarLowerLeft, .lihzahrdAltarLowerMiddle, .lihzahrdAltarLowerRight]

def placeAltar (world : World) (anchor : TilePos) : World :=
  ((altarCells anchor).zip framedAltar).foldl (fun terrain (p, tile) => terrain.setTile p tile) world

def sites : Array TempleLayout :=
  #[⟨⟨410, 75⟩⟩, ⟨⟨407, 76⟩⟩, ⟨⟨404, 74⟩⟩, ⟨⟨401, 73⟩⟩]

def altarAnchor (world : World) (position : TilePos) : Option TilePos :=
  if let some offset := framedAltar.findIdx? (· == world.tile position) then
    let anchor : TilePos := ⟨position.x - (offset % 3 : Nat), position.y - (offset / 3 : Nat)⟩
    if ((altarCells anchor).zip framedAltar).all (fun (p, tile) => world.tile p == tile)
      then some anchor else none
  else if world.tile position != .lihzahrdAltar then none
  else sites.findSome? fun layout =>
    let anchor := layout.altar
    let cells := altarCells anchor
    if cells.contains position && cells.all (fun p => world.tile p == .lihzahrdAltar)
      then some anchor else none

def footprint (layout : TempleLayout) : Array TilePos :=
  (Array.range (width * height)).map fun i => layout.worldCell (i % width) (i / width)

def wallCode : UInt8 := 2

def hasNaturalWall (world : World) (walls : ByteArray) (position : TilePos) : Bool :=
  world.contains position &&
    walls[position.y.toNat * world.width + position.x.toNat]?.getD 0 == wallCode

def findLegacy (world : World) : Option TempleLayout :=
  sites.find? fun layout => (altarCells layout.altar).all (fun p => world.tile p == .lihzahrdAltar)

/-- Golem's native SpawnOnPlayer scans the full square in column order and uses
the last altar center, with the ceiling weighted twice as much as the floor. -/
def golemSpawnBottom (world : World) (center : TilePos) : Option (Int × Int) := Id.run do
  if !world.contains ⟨center.x - 20, center.y - 20⟩ ||
      !world.contains ⟨center.x + 19, center.y + 19⟩ then return none
  let legacyCenters := sites.filterMap fun layout =>
    let anchor := layout.altar
    if world.tile anchor == .lihzahrdAltar && (altarAnchor world anchor).isSome
      then some { anchor with x := anchor.x + 1 } else none
  let mut altar := none
  for x in [(center.x - 20).toNat:(center.x + 20).toNat] do
    for y in [(center.y - 20).toNat:(center.y + 20).toNat] do
      let p : TilePos := ⟨x, y⟩
      if world.tile p == .lihzahrdAltarUpperMiddle || legacyCenters.contains p then altar := some p
  let some selectedAltar := altar | return none
  let findSolid (direction : Int) :=
    ((List.range 50).findSome? fun (distance : Nat) =>
      let row := selectedAltar.y + direction * distance
      if (world.tile? ⟨selectedAltar.x, row⟩).any Tile.solid then some row else none).getD (selectedAltar.y - 15)
  return some (selectedAltar.x * 16 + 8, ((2 * findSolid (-1) + findSolid 1) / 3) * 16)

/-- Extend the final compact corridor into a vaulted altar room. The old
altar site and existing chest footprints stay on the main exploration route. -/
def installGolemArena (layout : TempleLayout) (world : World) (walls : ByteArray)
    (canEdit : TilePos → Bool) : Option (World × ByteArray) := Id.run do
  let left : Int := 384
  let right := min 406 (layout.origin.x + 2)
  let floor := layout.altar.y + 2
  let roof := max 64 (floor - 30)
  let entry := (layout.worldCell 35 17).x
  let oldAltar := altarCells layout.altar
  if !oldAltar.all (fun p => world.tile p == .lihzahrdAltar) then return none
  let permitted (p : TilePos) :=
    let tile := world.tile p
    (tile == .lihzahrdBrick || [Tile.stone, .dirt, .mud, .air].contains tile) && canEdit p
  let positions := (List.range' left.toNat (right - left + 1).toNat).flatMap fun (x : Nat) =>
    (List.range' roof.toNat (floor - roof + 1).toNat).map fun (y : Nat) => (⟨x, y⟩ : TilePos)
  let connector := (List.range' right.toNat (entry - right).toNat).flatMap fun (x : Nat) =>
    (List.range' (floor - 4).toNat 4).map fun (y : Nat) => (⟨x, y⟩ : TilePos)
  if !(positions ++ connector).all permitted then return none
  let mut terrain := oldAltar.foldl (fun current p => current.setTile p .air) world
  let mut background := walls
  for p in positions do
    let shell := p.x == left || p.x == right || p.y == roof || p.y == floor
    terrain := terrain.setTile p (if shell then .lihzahrdBrick else .air)
    if !shell then background := background.set! (p.y.toNat * world.width + p.x.toNat) wallCode
  for p in connector do
    terrain := terrain.setTile p .air
    background := background.set! (p.y.toNat * world.width + p.x.toNat) wallCode
  let anchor : TilePos := ⟨(left + right) / 2 - 1, floor - 2⟩
  return some (placeAltar terrain anchor, background)

private def room (x y : Nat) : Bool :=
  (x < 5 && 2 ≤ y && y ≤ 4) ||
  (5 ≤ x && x ≤ 34 && 2 ≤ y && y ≤ 5) ||
  (31 ≤ x && x ≤ 34 && 6 ≤ y && y ≤ 7) ||
  (9 ≤ x && x ≤ 34 && 8 ≤ y && y ≤ 11) ||
  (8 ≤ x && x ≤ 11 && 10 ≤ y && y ≤ 15 && !(x ≤ 9 && y == 13)) ||
  (8 ≤ x && x ≤ 35 && 14 ≤ y && y ≤ 17)

/-- Build a compact temple in untouched terrain. The entrance, two descents and
lower altar chamber follow the structure of the Desktop temple. -/
def install (world : World) (walls : ByteArray) (seed : UInt64)
    (canEdit : TilePos → Bool) : Option (TempleLayout × World × ByteArray) := do
  if world.height != underworldHeight then none else
  let layout ← sites.find? fun candidate => candidate.footprint.all fun position =>
    [Tile.stone, .dirt, .mud].contains (world.tile position) &&
      walls[position.y.toNat * world.width + position.x.toNat]?.getD 1 == 0 &&
      canEdit position
  let (terrain, background) := (Array.range (width * height)).foldl (fun (terrain, background) i =>
    let x := i % width
    let y := i / width
    let position := layout.worldCell x y
    let terrain := terrain.setTile position (if room x y then .air else .lihzahrdBrick)
    let background := if x == 0 || y == 0 || x + 1 == width || y + 1 == height then background
      else background.set! (position.y.toNat * world.width + position.x.toNat) wallCode
    (terrain, background)) (world, walls)
  let terrain := TempleDoor.place terrain layout.door true (fun p => (hash (seed, p)).toNat)
  let terrain := layout.chestAnchors.foldl (fun current anchor =>
    (dungeonChestCells anchor).foldl (fun current p => current.setTile p .lihzahrdChest) current) terrain
  let terrain := (altarCells layout.altar).foldl (fun current p => current.setTile p .lihzahrdAltar) terrain
  return (layout, terrain, background)

end TempleLayout

/-- Cavern-depth Lihzahrd Chest inventory, in the order of the native RNG calls. -/
def templeChestLoot (ore : EarlyOreSet) (rng : UnifiedRandom) : Array Stack :=
  let next (bound : Nat) : StateM UnifiedRandom Nat :=
    modifyGet fun rng =>
      let (value, rng) := rng.next 0 bound
      (value.toNat, rng)
  let generate : StateM UnifiedRandom (Array Stack) := do
    let mut loot : Array Stack := #[⟨.lihzahrdPowerCell, 1⟩]
    if (← next 5) == 0 then loot := loot.push ⟨.suspiciousLookingEye, 1⟩
    if (← next 3) == 0 then loot := loot.push ⟨.dynamite, 1⟩
    if (← next 4) == 0 then loot := loot.push ⟨.jestersArrow, 25 + (← next 26)⟩
    if (← next 2) == 0 then
      let gold := if ore.gold == .platinumOre then Item.platinumBar else .goldBar
      let silverKind := if ore.silver == .tungstenOre then Item.tungstenBar else .silverBar
      let item := if (← next 2) == 0 then gold else silverKind
      loot := loot.push ⟨item, 3 + (← next 8)⟩
    if (← next 2) == 0 then
      let item := if (← next 2) == 0 then Item.flamingArrow else .throwingKnife
      loot := loot.push ⟨item, 25 + (← next 26)⟩
    if (← next 2) == 0 then loot := loot.push ⟨.healingPotion, 3 + (← next 3)⟩
    if (← next 3) > 0 then
      let potions : Array Item := #[.spelunkerPotion, .featherfallPotion, .nightOwlPotion,
        .waterWalkingPotion, .archeryPotion, .gravitationPotion]
      loot := loot.push ⟨potions[(← next 6)]!, 1 + (← next 2)⟩
    if (← next 3) > 1 then
      let potions : Array Item := #[.thornsPotion, .invisibilityPotion, .hunterPotion,
        .trapsightPotion, .teleportationPotion, .titanPotion]
      loot := loot.push ⟨potions[(← next 6)]!, 1 + (← next 2)⟩
    if (← next 2) == 0 then loot := loot.push ⟨.recallPotion, 2 + (← next 3)⟩
    if (← next 2) == 0 then
      let item := if (← next 2) == 0 then Item.torch else .glowstick
      loot := loot.push ⟨item, 15 + (← next 16)⟩
    if (← next 2) == 0 then loot := loot.push ⟨.goldCoin, 1 + (← next 2)⟩
    loot := loot.push ⟨.lihzahrdFurnace, 1⟩
    if (← next 5) == 0 then loot := loot.push ⟨.solarTablet, 1⟩
    else loot := loot.push ⟨.solarTabletFragment, 3 + (← next 5)⟩
    if (← next 12) == 0 then
      let voices : Array Item := #[.froggyNeckband, .goatsTuft, .oldCompanionLocket, .catChime, .dogCollar,
        .turkeyWattleNecklace, .meanGoblinSpikes, .crowsBeak, .balloonyBeads, .grimOldBarb,
        .vampirePendant, .cowBell, .chickenCharm, .fairyChoker]
      loot := loot.push ⟨voices[(← next 14)]!, 1⟩
    return loot
  generate.run' rng

end Lean4Terria
