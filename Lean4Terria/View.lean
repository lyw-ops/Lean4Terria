import Lean4Terria.Desktop
import Lean4Terria.Game
import Lean4Terria.Lighting

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

private def drawTorch (x y : Int) : IO Unit := do
  rect (x - 2) (y + 8) 4 13 0x805038ff
  rect (x - 1) (y + 9) 2 11 0xc48a50ff
  rect (x - 6) y 12 12 0xff9a532e
  rect (x - 4) (y + 3) 8 7 0xef8145ff
  rect (x - 2) (y + 1) 5 7 0xffcb6cff
  rect x (y + 4) 2 4 0xfff0abff

private def drawBlock (tile : Tile) (x y : Int) (variation : Nat) (surface : Bool) : IO Unit := do
  let (base, shade, fleck) : UInt32 × UInt32 × UInt32 := match tile with
    | .dirt => (0x68463dff, 0x825748ff, 0xb2805bff)
    | .stone => (0x35404cff, 0x44515bff, 0x667080ff)
    | .copper => (0x50434dff, 0x604b50ff, 0xc87d57ff)
    | .crystal => (0x3c405bff, 0x505273ff, 0x82f1d9ff)
    | .workbench => (0x35252cff, 0x98634aff, 0xe9b773ff)
    | .bedrock => (0x262938ff, 0x333747ff, 0x515568ff)
    | .timber => (0x654132ff, 0x9a6343ff, 0xc18b5bff)
    | .air | .torch => (0x1a2330ff, 0x202a38ff, 0x293548ff)
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
  | .timber =>
    rect (x + 2) (y + 10) 20 1 0x432f29ff
    rect (x + 2) (y + 18) 20 1 0x432f29ff
    rect (x + 8) (y + 11) 1 7 0x432f29ff
  | _ => pure ()

private def drawCloud (x y : Int) : IO Unit := do
  rect (x + 8) y 41 5 0xfaf3d7a0
  rect x (y + 5) 68 5 0xfaf3d7a0
  rect (x + 12) (y + 10) 48 4 0xfaf3d76b

private def shadeCaveTile (x y : Int) (level : Nat) (deep : Bool) : IO Unit := do
  if level < Lighting.maxLevel then
    let opacity := min 166 ((Lighting.maxLevel - level) * 12)
    rect x y tilePixels tilePixels (0x07122400 ||| opacity.toUInt32)
  if deep && level > 7 then
    let warmth := min 39 ((level - 7) * 6)
    rect x y tilePixels tilePixels (0xffa65500 ||| warmth.toUInt32)

private def drawWorkbenchHint (x y : Int) (nearby : Bool) : IO Unit := do
  if nearby then
    rect (x - 26) (y - 79) 252 68 0x111925f5
    rect (x - 26) (y - 79) 3 68 gold
    let recipes := #[("E", "Copper pick", "3 copper"), ("F", "3 torches", "1 wood"),
      ("R", "Stone sword", "2 stone + 1 wood")]
    for index in [:recipes.size] do
      let (key, name, cost) := recipes[index]!
      let row := y - 70 + (index : Int) * 19
      text key (x - 14) row 13 gold
      text name (x + 4) row 13 paper
      text cost (x + 98) row 13 muted
  else
    rect (x - 38) (y - 24) 101 18 0x212733eb
    text "WORKBENCH" (x - 31) (y - 21) 12 gold

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
  let holdingTorch := state.selectedStack.any (·.item == .torch)
  let light := computeLighting state.world state.seed ⟨firstX, firstY⟩ (lastX - firstX)
    (lastY - firstY) state.player.centerTile holdingTorch
  let mut workbench := none
  for col in [firstX:lastX] do
    let originalSurface := groundHeight state.seed col
    for row in [firstY:lastY] do
      let tile := state.world.tile ⟨col, row⟩
      let underground := row ≥ originalSurface
      let x := (col : Int) * tilePixels - camera.x
      let y := (row : Int) * tilePixels - camera.y
      if tile.solid || underground then
        drawBlock tile x y ((col * 23 + row * 13) % 16) (row == originalSurface)
      if underground then
        shadeCaveTile x y (light.level ⟨col, row⟩) (row > originalSurface + 2)
      if tile == .torch then
        rect (x + 8) (y + 21) 8 2 0x684439ff
        drawTorch (x + 12) (y + 2)
      if tile == .workbench then workbench := some (x, y)
  if let some (x, y) := workbench then drawWorkbenchHint x y state.nearWorkbench

private def drawSwing (x y facing : Int) (age : Nat) : IO Unit := do
  let handX := if facing < 0 then x - 1 else x + 16
  let handY := y + 18
  let bladeY : Int := if age < 3 then -3 else if age < 6 then -1 else 1
  for offset in [:8] do
    let dx := (offset : Int) * 3 * facing
    let dy := (offset : Int) * bladeY
    rect (handX + dx) (handY + dy + 3) 7 3 0xe4f2f448
    rect (handX + dx) (handY + dy) 5 4 (if offset < 2 then 0xbb8554ff else 0xd7e3ebff)

private def drawPlayer (state : GameState) (camera : Camera) (facesLeft : Bool) : IO Unit := do
  let player := state.player
  let x := player.x * tilePixels / tileUnits - camera.x
  let y := player.y * tilePixels / tileUnits - camera.y
  let hurt := state.combat.invulnerable > 0 && state.combat.invulnerable / 4 % 2 == 1
  rect x (y + 6) 17 26 0x172331ff
  rect (x + 1) (y + 3) 15 9 (if hurt then 0xf47e73ff else 0xd98b42ff)
  rect (x + 4) y 9 5 (if hurt then 0xffc1a8ff else 0xf8c372ff)
  rect (x + 2) (y + 12) 13 8 0xf1b97dff
  rect (x + (if facesLeft then 3 else 12)) (y + 14) 2 3 0x28303bff
  rect (x + 2) (y + 20) 13 7 (if hurt then 0xef8274ff else 0x66b8a7ff)
  rect (x + 2) (y + 28) 5 4 0x352b2cff
  rect (x + 10) (y + 28) 5 4 0x352b2cff
  let age := swingCooldown - state.combat.swing
  if state.combat.swing > 0 && age < 9 then
    drawSwing x y state.combat.facing age
  else if state.selectedStack.any (·.item == .torch) then
    drawTorch (if facesLeft then x - 4 else x + 21) (y + 8)
    rect (if facesLeft then x - 5 else x + 15) (y + 20) 7 4 0xf1b97dff

private def drawCrawler (crawler : Crawler) (camera : Camera) : IO Unit := do
  let x := crawler.body.x * tilePixels / tileUnits - camera.x
  let y := crawler.body.y * tilePixels / tileUnits - camera.y
  if x + 25 ≥ 0 && x < width && y + 36 ≥ 0 && y < height then
    let left := crawler.body.vx < 0
    let eyeX := if left then x + 3 else x + 12
    rect (x - 3) (y + 26) 23 6 0x181229ff
    rect (x - 1) (y + 13) 19 13 0x3c2954ff
    rect (x + 1) (y + 6) 15 15 0x8b5db4ff
    rect (x + 3) (y + 3) 11 5 0xb483d3ff
    rect (x + 1) (y + 23) 16 4 0x69448eff
    rect (x - 2) (y + 30) 6 3 0x513767ff
    rect (x + 13) (y + 30) 6 3 0x513767ff
    rect eyeX (y + 12) 3 4 0xffdea3ff
    rect (x - 5) (y - 8) 27 5 0x1b1529ff
    for index in [:crawlerMaxHealth] do
      rect (x - 4 + (index : Int) * 8) (y - 7) 7 3
        (if index < crawler.health then 0xda9ddfff else 0x493c53ff)

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
  | .wood => drawBlock .timber x y 5 false
  | .torch => drawTorch (x + 12) (y + 1)
  | .stoneSword =>
    for offset in [:8] do
      rect (x + 5 + (offset : Int) * 2) (y + 20 - (offset : Int) * 2) 4 4
        (if offset < 2 then 0xbb8554ff else 0xd7e3ebff)
    rect (x + 7) (y + 13) 3 8 0x97714eff
    rect (x + 9) (y + 18) 5 3 0x97714eff

private def drawHealth (state : GameState) : IO Unit := do
  rect 893 18 207 67 0x111925ec
  text "HEALTH" 910 31 13 muted
  for index in [:maxHealth] do
    let x := 985 + (index : Int) * 19
    let color : UInt32 :=
      if index ≥ state.combat.health then 0x553b47ff
      else if state.combat.invulnerable > 0 then 0xffb48fff
      else 0xe88681ff
    rect x 31 5 6 color
    rect (x + 7) 31 5 6 color
    rect (x + 1) 35 10 5 color
    rect (x + 3) 40 6 2 color
    rect (x + 5) 42 2 2 color

private def drawHud (state : GameState) : IO Unit := do
  let copper := Inventory.count state.inventory .copper
  let stone := Inventory.count state.inventory .stone
  let hasPick := Inventory.count state.inventory .copperPick > 0
  let hasCrystal := Inventory.count state.inventory .crystal > 0
  let hasSword := Inventory.count state.inventory .stoneSword > 0
  let objective :=
    if state.combat.cleared then "The cave is clear. Build, explore, or save your world."
    else if hasCrystal && hasSword then "Keep right to find the Cave Crawler. Swing with the stone sword."
    else if hasCrystal then "Craft a stone sword at the workbench before going deeper."
    else if hasPick && !hasSword then "Prepare a stone sword, then head deeper to mine crystal."
    else if hasPick then "Mine crystal farther right. Select a torch to light the cave."
    else if copper ≥ copperPickCost && !hasSword && stone < 2 then
      "Gather two stone for a sword before returning to the workbench."
    else if copper ≥ copperPickCost then "Return to the workbench and press E for a copper pick."
    else "Go right and mine three copper ore. Craft torches to light the cave."
  let progress :=
    if state.combat.cleared then "CAVE CLEARED"
    else if hasCrystal then "CRYSTAL FOUND"
    else if hasPick then "COPPER PICK"
    else s!"COPPER  {copper}/{copperPickCost}"
  rect 20 18 697 67 0x111925ec
  rect 20 18 4 67 mint
  text "LEAN4TERRIA" 39 28 23 paper
  text s!"WORLD  {state.seed}" 231 35 13 muted
  text objective 39 58 17 muted
  drawHealth state
  text progress 910 55 15 (if hasPick then mint else gold)

  let panelY : Int := (height : Int) - 117
  rect 20 panelY 547 97 0x111925ed
  rect 580 panelY 520 97 0x111925ed
  text (state.selectedStack.map (fun stack => stack.item.name) |>.getD "Empty slot")
    32 (panelY + 8) 15 muted
  for index in [:Inventory.slotCount] do
    let x := 32 + (index : Int) * 65
    let y := panelY + 28
    let selected := index == state.selected
    rect x y 61 54 (if selected then 0x34434eff else 0x222d3aff)
    rect x y 61 2 (if selected then gold else 0x42505dff)
    text (toString (index + 1)) (x + 6) (y + 7) 14 (if selected then gold else muted)
    if let some stack := state.inventory[index]?.join then
      drawItem stack.item (x + 19) (y + 14)
      text (toString stack.count) (x + 42) (y + 36) 13 paper
  text state.status 596 (panelY + 12) 16 paper
  rect 596 (panelY + 42) 488 1 0x495768ff
  text "A/D Move   SPACE Jump   LMB Mine/Swing   RMB Place" 596 (panelY + 51) 14 muted
  text "1-8 Hotbar   E/F/R Craft   S Save   L Load" 596 (panelY + 72) 14 muted

def draw (state : GameState) (camera : Camera) (target : Option TilePos) (facesLeft : Bool) : IO Unit := do
  beginDrawing ink
  drawWorld state camera
  if let some crawler := state.combat.crawler then drawCrawler crawler camera
  drawPlayer state camera facesLeft
  if let some position := target then drawCursor state camera position
  drawHud state
  endDrawing

end Lean4Terria.View
