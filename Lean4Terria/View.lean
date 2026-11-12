import Lean4Terria.Desktop
import Lean4Terria.Game

namespace Lean4Terria.View

open Lean4Terria.Desktop

def width : Nat := 1120
def height : Nat := 704
def tilePixels : Nat := 24

structure Camera where
  x : Int
  y : Int

def cameraFor (state : GameState) : Camera :=
  let x := (state.player.x + playerWidth / 2) * tilePixels / tileUnits
  let y := (state.player.y + playerHeight / 2) * tilePixels / tileUnits
  { x := max 0 (min (x - width / 2) (state.world.width * tilePixels - (width : Int)))
    y := max 0 (min (y - height / 2) (state.world.height * tilePixels - (height : Int))) }

def Camera.target (camera : Camera) (x y : Nat) : Option TilePos :=
  if x < width && 96 ≤ y && y < height - 126 then
    some ⟨(camera.x + x) / tilePixels, (camera.y + y) / tilePixels⟩
  else
    none

private def ink : UInt32 := 0x111925ff
private def muted : UInt32 := 0xa2b1baff
private def paper : UInt32 := 0xf6f0dcff
private def gold : UInt32 := 0xf6bd76ff
private def mint : UInt32 := 0x89e2d0ff

private def drawBlock (tile : Tile) (x y : Int) (variation : Nat) (surface : Bool) : IO Unit := do
  let (base, shade, fleck) : UInt32 × UInt32 × UInt32 := match tile with
    | .dirt => (0x68463dff, 0x825748ff, 0xb2805bff)
    | .stone => (0x35404cff, 0x44515bff, 0x667080ff)
    | .copper => (0x50434dff, 0x604b50ff, 0xc87d57ff)
    | .crystal => (0x3c405bff, 0x505273ff, 0x82f1d9ff)
    | .workbench => (0x35252cff, 0x98634aff, 0xe9b773ff)
    | .bedrock => (0x262938ff, 0x333747ff, 0x515568ff)
    | .air => (0x202734ff, 0x252d3aff, 0x323a48ff)
  rect x y 24 24 base
  rect (x + 2) (y + 2) 20 3 shade
  rect (x + 1) (y + 22) 22 1 0x171c284f
  rect (x + 3 + variation % 6) (y + 8) 5 2 fleck
  rect (x + 12 + variation % 5) (y + 16) 3 2 fleck
  match tile with
  | .copper =>
    rect (x + 12) (y + 6) 6 8 0xa95d46ff
    rect (x + 13) (y + 7) 4 4 0xf7a86cff
    rect (x + 4) (y + 15) 7 4 0xde8b5eff
  | .crystal =>
    rect (x + 10) (y + 5) 4 14 0x5ad0c9ff
    rect (x + 14) (y + 3) 3 10 mint
    rect (x + 7) (y + 10) 3 7 0xb3fff0ff
  | .workbench =>
    rect (x + 1) (y + 4) 22 5 gold
    rect (x + 2) (y + 9) 20 4 0x9d633eff
    rect (x + 4) (y + 13) 4 10 0x98634aff
    rect (x + 16) (y + 13) 4 10 0x98634aff
  | .dirt =>
    if surface then
      rect x y 24 4 0x78a46aff
      rect (x + 3) (y + 4) 5 2 0x5e8355ff
  | _ => pure ()

private def drawCloud (x y : Int) : IO Unit := do
  rect (x + 8) y 41 5 0xfaf3d7a0
  rect x (y + 5) 68 5 0xfaf3d7a0
  rect (x + 12) (y + 10) 48 4 0xfaf3d76b

private def drawWorld (state : GameState) (camera : Camera) : IO Unit := do
  gradient width.toUInt32 height.toUInt32 0x477d91ff 0xc7c5a0ff
  for index in [:8] do
    let x := ((170 + (index : Int) * 203 - camera.x / 4) % 1450) - 100
    let y := 728 + (index % 3) * 26 - camera.y
    drawCloud x y
  let firstX := (camera.x / tilePixels).toNat
  let firstY := (camera.y / tilePixels).toNat
  let lastX := min state.world.width ((camera.x + width + tilePixels - 1) / tilePixels).toNat
  let lastY := min state.world.height ((camera.y + height + tilePixels - 1) / tilePixels).toNat
  for col in [firstX:lastX] do
    let originalSurface := groundHeight state.seed col
    for row in [firstY:lastY] do
      let tile := state.world.tile ⟨col, row⟩
      if tile != .air || row ≥ originalSurface then
        let x := (col : Int) * tilePixels - camera.x
        let y := (row : Int) * tilePixels - camera.y
        drawBlock tile x y ((col * 23 + row * 13) % 16) (row == originalSurface)
        if tile == .workbench then
          rect (x - 38) (y - 24) 101 18 0x212733eb
          text "WORKBENCH" (x - 31) (y - 21) 12 gold

private def drawPlayer (player : Player) (camera : Camera) (facesLeft : Bool) : IO Unit := do
  let x := player.x * tilePixels / tileUnits - camera.x
  let y := player.y * tilePixels / tileUnits - camera.y
  rect x (y + 6) 17 26 0x172331ff
  rect (x + 1) (y + 3) 15 9 0xd98b42ff
  rect (x + 4) y 9 5 0xf8c372ff
  rect (x + 2) (y + 12) 13 8 0xf1b97dff
  rect (x + (if facesLeft then 3 else 12)) (y + 14) 2 3 0x28303bff
  rect (x + 2) (y + 20) 13 7 0x66b8a7ff
  rect (x + 2) (y + 28) 5 4 0x352b2cff
  rect (x + 10) (y + 28) 5 4 0x352b2cff

private def outline (x y : Int) (size : Nat) (color : UInt32) : IO Unit := do
  rect x y size 2 color
  rect x (y + size - 2) size 2 color
  rect x y 2 size color
  rect (x + size - 2) y 2 size color

private def drawCursor (state : GameState) (camera : Camera) (position : TilePos) : IO Unit :=
  if state.world.contains position then
    outline (position.x * tilePixels - camera.x) (position.y * tilePixels - camera.y) 24
      (if state.canReach position then mint else 0xd6a2a2b0)
  else
    pure ()

private def drawItem (item : Item) (x y : Int) : IO Unit := do
  match item with
  | .basicPick | .copperPick =>
    let head := if item == .copperPick then 0xf3a168ff else 0xb4c4ccff
    for i in [:7] do
      rect (x + 5 + (i : Int) * 2) (y + 20 - (i : Int) * 2) 4 4 0xbb8554ff
    rect (x + 8) (y + 4) 13 4 head
    rect (x + 19) (y + 7) 4 8 head
  | .dirt => drawBlock .dirt x y 3 false
  | .stone => drawBlock .stone x y 4 false
  | .copper => drawBlock .copper x y 6 false
  | .crystal => drawBlock .crystal x y 8 false

private def drawHud (state : GameState) : IO Unit := do
  let copper := Inventory.count state.inventory .copper
  let hasPick := Inventory.count state.inventory .copperPick > 0
  let hasCrystal := Inventory.count state.inventory .crystal > 0
  let objective :=
    if hasCrystal then "Crystal found. Build, explore, or save your world."
    else if hasPick then "Follow the cave deeper and mine a crystal."
    else if copper ≥ copperPickCost then "Return to the workbench and press E."
    else "Head right into the cave and mine three copper ore."
  let progress :=
    if hasCrystal then "CRYSTAL FOUND"
    else if hasPick then "COPPER PICK"
    else s!"COPPER  {copper}/{copperPickCost}"
  rect 20 18 650 67 0x111925ec
  rect 20 18 4 67 mint
  text "LEAN4TERRIA" 39 28 23 paper
  text objective 39 58 17 muted
  rect 893 18 207 67 0x111925ec
  text progress 910 28 18 (if hasPick then mint else gold)
  text s!"WORLD  {state.seed}" 910 55 15 muted

  let panelY : Int := (height : Int) - 117
  rect 20 panelY 535 97 0x111925ed
  rect 568 panelY 532 97 0x111925ed
  text (state.selectedStack.map (fun stack => stack.item.name) |>.getD "Empty slot")
    32 (panelY + 8) 15 muted
  for index in [:Inventory.slotCount] do
    let x := 32 + (index : Int) * 84
    let y := panelY + 28
    let selected := index == state.selected
    rect x y 75 54 (if selected then 0x34434eff else 0x222d3aff)
    rect x y 75 2 (if selected then gold else 0x42505dff)
    text (toString (index + 1)) (x + 6) (y + 7) 14 (if selected then gold else muted)
    if let some stack := state.inventory[index]?.join then
      drawItem stack.item (x + 24) (y + 14)
      text (toString stack.count) (x + 53) (y + 34) 14 paper
  text state.status 584 (panelY + 12) 16 paper
  rect 584 (panelY + 42) 498 1 0x495768ff
  text "A/D Move   SPACE Jump   LMB Mine   RMB Place" 584 (panelY + 51) 15 muted
  text "1-6 Hotbar   E Craft   S Save   L Load" 584 (panelY + 72) 15 muted

def draw (state : GameState) (camera : Camera) (target : Option TilePos) (facesLeft : Bool) : IO Unit := do
  beginDrawing ink
  drawWorld state camera
  drawPlayer state.player camera facesLeft
  if let some position := target then drawCursor state camera position
  drawHud state
  endDrawing

end Lean4Terria.View
