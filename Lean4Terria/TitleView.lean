import Lean4Terria.Desktop
import Lean4Terria.WorldSlots

namespace Lean4Terria.TitleView

open Lean4Terria.Desktop

inductive Field where
  | name | seed | rename (slot : Nat)
  deriving BEq, Repr

inductive Page where
  | main | worlds | create | edit (field : Field) | delete (slot : Nat)
  deriving BEq, Repr

inductive Action where
  | back | newWorld | create | submit | singlePlayer | exit
  | scroll
  | row (slot : Nat)
  | play (slot : Nat)
  | favorite (slot : Nat)
  | cloud
  | copySeed (slot : Nat)
  | edit (field : Field)
  | randomName | randomSeed
  | delete (slot : Nat)
  | confirmDelete (slot : Nat)
  deriving BEq, Repr

private def menuWidth : Nat := 1120
private def menuHeight : Nat := 704
private def listLeft : Nat := 235
private def listTop : Nat := 220
private def listWidth : Nat := 650
private def listHeight : Nat := 374
private def rowsLeft : Nat := listLeft + 10
private def rowsTop : Nat := listTop + 9
private def rowWidth : Nat := 604
private def rowHeight : Nat := 96
private def rowStride : Nat := 101
private def rowsHeight : Nat := listHeight - 18
private def scrollX : Nat := listLeft + listWidth - 30
private def footerY : Nat := 612

def maxScroll (count : Nat) : Nat :=
  (count * rowStride - (rowStride - rowHeight)) - rowsHeight

def selectedScroll (count index scroll : Nat) : Nat :=
  let top := index * rowStride
  if top < scroll then top
  else min (maxScroll count) (max scroll (top + rowHeight - rowsHeight))

def scrollAt (count mouseY : Nat) : Nat :=
  let range := maxScroll count
  let thumb := max 24 (rowsHeight * rowsHeight / max rowsHeight (count * rowStride))
  if thumb ≥ rowsHeight then 0
  else min range ((mouseY - (rowsTop + thumb / 2)) * range / (rowsHeight - thumb))

private def inside (x y left top width height : Nat) : Bool :=
  left ≤ x && x < left + width && top ≤ y && y < top + height

private def rowActions (entry : WorldSlots.Entry) : Array (String × Action × String) :=
  #[
    ("UI/ButtonPlay", .play entry.slot, "Play"),
    ((if entry.favorite then "UI/ButtonFavoriteActive" else "UI/ButtonFavoriteInactive"),
      .favorite entry.slot, if entry.favorite then "Unfavorite" else "Favorite"),
    ("UI/ButtonCloudInactive", .cloud, "Steam Cloud unavailable"),
    ("UI/ButtonSeed", .copySeed entry.slot, "Copy Seed"),
    ("UI/ButtonRename", .edit (.rename entry.slot), "Rename")
  ]

def actionAt (page : Page) (catalog : Array WorldSlots.Entry) (scroll mouseX mouseY : Nat) :
    Option Action := Id.run do
  let hit := inside mouseX mouseY
  match page with
  | .main =>
    if hit 410 244 300 45 then return some .singlePlayer
    if hit 410 298 300 45 then return some .exit
  | .worlds =>
    if hit listLeft footerY 315 47 then return some .back
    if hit 570 footerY 315 47 then return some .newWorld
    if hit scrollX rowsTop 23 rowsHeight && maxScroll catalog.size > 0 then return some .scroll
    if hit rowsLeft rowsTop rowWidth rowsHeight then
      let row := (mouseY - rowsTop + scroll) / rowStride
      let localY := (mouseY - rowsTop + scroll) % rowStride
      if let some entry := catalog[row]? then
        if 69 ≤ localY && localY < 93 && entry.seed.isSome then
          for index in [:5] do
            if hit (rowsLeft + 10 + 24 * index) mouseY 22 1 then
              return (rowActions entry)[index]?.map (·.2.1)
          if hit (rowsLeft + rowWidth - 28) mouseY 22 1 then return some (.delete entry.slot)
        if localY < rowHeight then return some (.row entry.slot)
  | .create =>
    if hit 310 512 238 46 then return some .back
    if hit 572 512 238 46 then return some .create
    if hit 318 214 39 36 then return some .randomName
    if hit 318 257 39 36 then return some .randomSeed
    if hit 362 214 348 36 then return some (.edit .name)
    if hit 362 257 348 36 then return some (.edit .seed)
  | .edit _ =>
    if hit 310 427 238 46 then return some .submit
    if hit 572 427 238 46 then return some .back
  | .delete slot =>
    if hit 310 427 238 46 then return some (.confirmDelete slot)
    if hit 572 427 238 46 then return some .back
  return none

private def white : UInt32 := 0xffffffff
private def pale : UInt32 := 0xfff2d6ff
private def muted : UInt32 := 0xb9c7eaff
private def gold : UInt32 := 0xffe177ff
private def blue : UInt32 := 0x495eabff

private def label (value : String) (x y : Int) (size : Nat) (tint := pale) : IO Unit := do
  text value (x + 1) (y + 2) size 0x0d112cff
  text value x y size tint

private def centered (value : String) (x y : Int) (size : Nat) (tint := pale) : IO Unit := do
  let extent ← textWidth value size
  label value (x - extent / 2) y size tint

private def nineSlice (asset : String) (x y : Int) (w h : Nat) (color : UInt32) : IO Unit := do
  let cuts (span : Nat) : Array (Nat × Nat × Nat) := #[(0, 0, 12), (12, 12, span - 24), (16, span - 12, 12)]
  for (sy, dy, dh) in cuts h do
    for (sx, dx, dw) in cuts w do
      discard <| originalSprite asset (x + dx) (y + dy) dw dh sx sy
        (if sx == 12 then 4 else 12) (if sy == 12 then 4 else 12) (color := color)

private def panel (x y : Int) (w h : Nat) (fill : UInt32) (border := (0x070b1bff : UInt32)) : IO Unit := do
  nineSlice "UI/PanelBackground" x y w h fill
  nineSlice "UI/PanelBorder" x y w h border

private def button (caption : String) (x y : Int) (w h : Nat) (hovered : Bool) : IO Unit := do
  panel x y w h (if hovered then blue else 0x3e5389e8) (if hovered then gold else 0x080b1cff)
  centered caption (x + w / 2) (y + 6) 27

private def scenery (frame : Nat) : IO Unit := do
  discard <| originalSprite "Background_0" 0 0 menuWidth menuHeight 0 0 48 700
  discard <| originalSprite "Sun" 825 106 115 115 (fit := true)
  for (asset, x, y, w, h) in
      [("Cloud_0", 6, 195, 175, 71), ("Cloud_1", 831, 278, 166, 55), ("Cloud_1", 235, 127, 111, 37)] do
    discard <| originalSprite asset (x + (frame / 10 % 48 : Nat)) y w h (color := 0xe7edffe8)
  discard <| originalSprite "Background_7" 0 287 menuWidth 477 (color := 0xc8d9eeff)
  discard <| originalSprite "Background_10" 0 404 menuWidth 378 (color := 0xbfd3caff)
  let sway : Float := (Float.sin (frame.toFloat / 70)) * 1.8
  discard <| originalSprite "Logo" 276 16 568 166 (rotation := sway)

private def iconButton (asset : String) (x y : Int) (hovered enabled : Bool) : IO Unit := do
  let tint := if hovered && enabled then white else if enabled then 0xc4c9e4ff else 0x7c839aff
  discard <| originalSprite "UI/ButtonBacking" x y 22 22 (color := tint)
  discard <| originalSprite asset x y 22 22 (color := tint)

private def scrollbar (asset : String) (top height : Nat) : IO Unit := do
  discard <| originalSprite asset scrollX top 20 6 0 0 20 6
  discard <| originalSprite asset scrollX (top + 6) 20 (height - 12) 0 6 20 4
  discard <| originalSprite asset scrollX (top + height - 6) 20 6 0 10 20 6

private def drawRow (entry : WorldSlots.Entry) (top : Int) (active : Bool) (hover : Option Action) : IO Unit := do
  let entryActions := rowActions entry
  let highlighted := active || hover == some (.row entry.slot) || hover == some (.delete entry.slot) ||
    entryActions.any (fun (_, action, _) => action != .cloud && hover == some action)
  panel rowsLeft top rowWidth rowHeight (if highlighted then 0x4c61acff else 0x435997ef)
    (if highlighted then 0x7187e4ff else 0x566babff)
  let x := rowsLeft + 76
  let icon := if entry.hardmode then "UI/IconHallow" else "UI/Icon"
  let icon := icon ++ (if entry.seed.any (fun seed => altarTile seed == .crimsonAltar) then "Crimson" else "Corruption")
  panel (rowsLeft + 10) (top + 6) 61 62 0x293562ff (if entry.favorite then gold else 0x6675b6ff)
  if entry.seed.isSome then
    discard <| originalSprite icon (rowsLeft + 11) (top + 7) 58 58 (fit := true)
  else
    centered "!" (rowsLeft + 39) (top + 13) 26 gold
  if entry.favorite then
    discard <| originalSprite "UI/ButtonFavoriteActive" (rowsLeft + 30) (top - 3) 23 23
  label ((entry.name.take 54).toString) x (top + 4) 18
  rect (x - 2) (top + 27) 522 1 0x6a80c6ff
  let stats := if entry.seed.isSome then
      #[("Classic", 99), ("Small World", 193), (if entry.created.isEmpty then "" else "Created: " ++ entry.created, 222)]
    else #[("Unreadable save", 522)]
  let mut dx := x
  for (value, w) in stats do
    rect dx (top + 34) w 26 0x1f2b58aa
    centered value (dx + w / 2) (top + 36) 16
    dx := dx + w + 4
  if entry.seed.isSome then
    for ((asset, action, _), i) in entryActions.toList.zipIdx do
      iconButton asset (rowsLeft + 10 + i * 24) (top + 69) (hover == some action) (action != .cloud)
    iconButton "UI/ButtonDelete" (rowsLeft + rowWidth - 28) (top + 69) (hover == some (.delete entry.slot))
      (!entry.favorite)

private def drawWorlds (catalog : Array WorldSlots.Entry) (selected scroll : Nat) (hover : Option Action) :
    IO Unit := do
  panel listLeft listTop listWidth listHeight 0x212b4fd4
  withClip rowsLeft rowsTop rowWidth rowsHeight do
    for index in [scroll / rowStride:min catalog.size ((scroll + rowsHeight) / rowStride + 1)] do
      let entry := catalog[index]!
      drawRow entry ((rowsTop : Int) + index * rowStride - scroll) (entry.slot == selected) hover
  if maxScroll catalog.size > 0 then
    let thumb := max 24 (rowsHeight * rowsHeight / max rowsHeight (catalog.size * rowStride))
    let y := rowsTop + scroll * (rowsHeight - thumb) / maxScroll catalog.size
    scrollbar "UI/Scrollbar" rowsTop rowsHeight
    scrollbar "UI/ScrollbarInner" y thumb
  panel 449 180 222 53 blue
  centered "Select World" 560 185 30
  button "Back" listLeft footerY 315 47 (hover == some .back)
  button "New" 570 footerY 315 47 (hover == some .newWorld)

private def drawCreate (name seed : String) (hover : Option Action) : IO Unit := do
  panel 310 205 500 288 0x25315cff
  let fields := #[
    ("UI/WorldCreation/IconRandomName", "Name: " ++ name, Field.name, Action.randomName),
    ("UI/WorldCreation/IconRandomSeed", "Seed: " ++ (if seed.isEmpty then "<Leave Blank For Random>" else seed),
      Field.seed, Action.randomSeed)]
  for ((asset, value, field, random), i) in fields.toList.zipIdx do
    let y := 214 + i * 43
    panel 318 y 39 36 (if hover == some random then blue else 0x343f7cff)
    discard <| originalSprite asset 325 (y + 5) 25 25 (fit := true)
    panel 362 y 348 36 (if hover == some (.edit field) then blue else 0x343f7cff)
    label ((value.take 47).toString) 370 (y + 6) 18
  for asset in ["PreviewSizeSmall", "PreviewDifficultyNormal1", "PreviewEvilRandom", "PreviewBorder"] do
    discard <| originalSprite ("UI/WorldCreation/" ++ asset) 717 215 80 80
  for (i, (asset, value)) in ([
      ("IconSizeSmall", "Small"), ("IconDifficultyNormal", "Classic"), ("IconEvilRandom", "Random Evil")]
      : List (String × String)).zipIdx.map Prod.swap do
    let y := 306 + i * 53
    rect 319 (y - 4) 482 1 0x5466a5ff
    panel 319 y 482 42 0x343f7cff
    discard <| originalSprite ("UI/WorldCreation/" ++ asset) 330 (y + 9) 24 24 (fit := true)
    centered value 560 (y + 7) 22
  button "Back" 310 512 238 46 (hover == some .back)
  button "Create" 572 512 238 46 (hover == some .create)

private def prompt (title value left right : String) (hover : Option Action) (confirm : Action) : IO Unit := do
  panel 310 235 500 174 0x25315cff
  centered title 560 247 24
  panel 324 310 472 65 0x151f45ff blue
  withClip 338 314 444 56 do
    centered value 560 324 24
  button left 310 427 238 46 (hover == some confirm)
  button right 572 427 238 46 (hover == some .back)

private def drawTooltip (value : String) (mouseX mouseY : Nat) : IO Unit := do
  let size ← textWidth value 16
  label value (min (mouseX + 15) (menuWidth - size - 5)) (min (mouseY + 12) (menuHeight - 22)) 16 white

def draw (page : Page) (catalog : Array WorldSlots.Entry) (selected scroll : Nat)
    (name seed inputValue message : String) (mouseX mouseY frame : Nat) : IO Unit := do
  scenery frame
  let hover := actionAt page catalog scroll mouseX mouseY
  match page with
  | .main =>
    centered "Single Player" 560 244 35 (if hover == some .singlePlayer then gold else pale)
    centered "Exit" 560 298 35 (if hover == some .exit then gold else pale)
  | .worlds =>
    drawWorlds catalog selected scroll hover
    if let some action := hover then
      if let some word := catalog.findSome? fun entry =>
          if action == .delete entry.slot then some (if entry.favorite then "Unfavorite first" else "Delete")
          else (rowActions entry).findSome? (fun (_, key, tip) => if key == action then some tip else none) then
        drawTooltip word mouseX mouseY
  | .create => drawCreate name seed hover
  | .edit field =>
    let title := if field == .seed then "Enter World Seed:" else "Enter World Name:"
    prompt title (inputValue ++ "|") "Submit" "Cancel" hover .submit
  | .delete slot =>
    let world := (catalog.find? (·.slot == slot)).map (·.name) |>.getD ""
    prompt "Delete this world?" world "Delete" "Cancel" hover (.confirmDelete slot)
  if !message.isEmpty then
    centered ((message.take 88).toString) 560 671 16 0xffe8a2ff

end Lean4Terria.TitleView
