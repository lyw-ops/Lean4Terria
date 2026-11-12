import Lean4Terria
import Lean4Terria.Lighting

open Lean4Terria

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def sameSnapshot (left right : GameState) : Bool :=
  left.seed == right.seed && left.world == right.world && left.player == right.player &&
    left.inventory == right.inventory && left.selected == right.selected && left.combat == right.combat

private def walk (state : GameState) (ticks : Nat) (direction : Int) (jump := false) : IO GameState := do
  let mut state := state
  for index in [:ticks] do
    state := step state { horizontal := direction, jump := jump && index == 0 }
    check (!state.player.collides state.world) s!"Player overlapped a tile at movement tick {index}."
  return state

private def slotFor (state : GameState) (item : Item) : IO Nat := do
  let some slot := state.inventory.findIdx? (fun entry => entry.any (·.item == item))
    | throw (IO.userError s!"Missing {item.name} in hotbar.")
  return slot

private def playRoute (seed : UInt64) (prepareCombat := false) : IO GameState := do
  let initial := newGame seed
  let earlyCraft := step initial { craft := true }
  check (earlyCraft.inventory == initial.inventory) "Crafting without ore changed the inventory."
  let farMine := step initial { mine := some ⟨35, 42⟩ }
  check (farMine.world == initial.world) "Out-of-range mining changed the world."
  let approach ← walk initial 126 1
  check (approach.player.centerTile == ⟨34, 41⟩) "Could not reach the copper terrace."

  let stone := step approach { mine := some ⟨33, 42⟩ }
  let blocked := step stone { selectSlot := some 1, place := some ⟨34, 41⟩ }
  check (blocked.world == stone.world && blocked.inventory == stone.inventory)
    "Placement inside the player changed game state."
  let placed := step blocked { place := some ⟨33, 39⟩ }
  check (placed.world.tile ⟨33, 39⟩ == .stone && Inventory.count placed.inventory .stone == 0)
    "Placement did not consume exactly one stone block."

  let fullInventory := (Array.replicate Inventory.slotCount (some (Stack.mk .stone Inventory.maxStack))).set!
    0 (some ⟨.basicPick, 1⟩)
  let full := { approach with inventory := fullInventory }
  let fullMine := step full { mine := some ⟨35, 42⟩ }
  check (fullMine.world == full.world && fullMine.inventory == full.inventory)
    "A full inventory caused a mined tile to be lost."

  let mut mined := approach
  if prepareCombat then
    for position in [⟨33, 43⟩, ⟨34, 43⟩] do
      mined := step mined { mine := some position }
    check (Inventory.count mined.inventory .stone == 2) "Could not gather stone for the sword."
  for column in [35:38] do
    mined := step mined { mine := some ⟨column, 42⟩ }
    check (mined.world.tile ⟨column, 42⟩ == .air) "Copper tile was not mined."
  let repeated := step mined { mine := some ⟨35, 42⟩ }
  check (Inventory.count mined.inventory .copper == 3 && repeated.inventory == mined.inventory)
    "Copper was lost or duplicated during mining."

  let retreat ← walk mined 27 (-1)
  let climbed ← walk retreat 42 (-1) true
  let bench ← walk climbed 120 (-1)
  let mut crafted := step bench { craft := true }
  check (crafted.selectedStack.any (·.item == .copperPick) &&
    Inventory.count crafted.inventory .copper == 0) "Copper pick crafting failed."

  if prepareCombat then
    crafted := step crafted { craftTorches := true, craftSword := true }
    check (Inventory.count crafted.inventory .torch == 3 &&
      Inventory.count crafted.inventory .stoneSword == 1 &&
      Inventory.count crafted.inventory .wood == startingWood - 2 &&
      Inventory.count crafted.inventory .stone == 0) "Equipment recipes did not apply their costs."
    let pickSlot ← slotFor crafted .copperPick
    crafted := step crafted { selectSlot := some pickSlot }
  let deep ← walk crafted 420 1
  let wrongTool := step deep { selectSlot := some 0, mine := some ⟨64, 49⟩ }
  check (wrongTool.world.tile ⟨64, 49⟩ == .crystal) "The basic pick mined crystal."
  let complete := step deep { mine := some ⟨64, 49⟩ }
  check (Inventory.count complete.inventory .crystal == 1 && complete.world.tile ⟨64, 49⟩ == .air)
    "The copper pick did not collect crystal."
  return complete

private def lightingChecks : IO Unit := do
  let positions : List TilePos := [⟨4, 49⟩, ⟨5, 49⟩, ⟨7, 49⟩,
    ⟨4, 50⟩, ⟨5, 50⟩, ⟨6, 50⟩, ⟨7, 50⟩]
  let dark : World := ⟨12, 60, ByteArray.mk (Array.replicate (12 * 60) Tile.bedrock.code)⟩
  let tunnel := positions.foldl (fun world position => world.setTile position .air) dark
  let tunnel := tunnel.setTile ⟨3, 49⟩ .torch
  let light := computeLighting tunnel defaultSeed ⟨4, 49⟩ 5 2 ⟨-1, -1⟩ false
  check (light.level ⟨4, 49⟩ == 14 && light.level ⟨6, 49⟩ == 12 && light.level ⟨7, 49⟩ == 9)
    "An offscreen torch failed to light a wall or spread around the corner."
  let closed := tunnel.setTile ⟨6, 50⟩ .bedrock
  let blocked := computeLighting closed defaultSeed ⟨4, 49⟩ 5 2 ⟨-1, -1⟩ false
  check (blocked.level ⟨7, 49⟩ == 0) "Torchlight passed through solid rock."

private def playCombatRoute : IO GameState := do
  let mut state ← playRoute defaultSeed true
  let unsupported := step state { selectSlot := some (← slotFor state .torch), place := some ⟨61, 47⟩ }
  check (unsupported.world == state.world && unsupported.inventory == state.inventory)
    "An unsupported torch was placed."
  state := step unsupported { place := some ⟨61, 49⟩ }
  check (state.world.tile ⟨61, 49⟩ == .torch && !(state.world.tile ⟨61, 49⟩).solid &&
    Inventory.count state.inventory .torch == 2) "Placed torch did not consume one item or was solid."
  let light := computeLighting state.world state.seed ⟨60, 47⟩ 10 4 state.player.centerTile false
  check (light.level ⟨61, 49⟩ == 15 && light.level ⟨62, 49⟩ == 14) "Placed torch did not light the cave."
  let copperPick ← slotFor state .copperPick
  let recovered := step state { selectSlot := some copperPick, mine := some ⟨61, 50⟩ }
  check (recovered.world.tile ⟨61, 49⟩ == .air && Inventory.count recovered.inventory .torch == 3)
    "Removing a torch's last support did not return it to the inventory."
  let tightInventory := (Array.replicate Inventory.slotCount (some (Stack.mk .stone Inventory.maxStack)))
    |>.set! 0 (some ⟨.basicPick, 1⟩) |>.set! (Inventory.slotCount - 1) none
  let tight := { state with inventory := tightInventory }
  let blocked := step tight { selectSlot := some 0, mine := some ⟨61, 50⟩ }
  check (blocked.world == tight.world && blocked.inventory == tight.inventory)
    "Removing a torch support with insufficient storage was not atomic."
  for row in [47:49] do
    state := step state { selectSlot := some copperPick, mine := some ⟨64, row⟩ }
  check (state.world.tile ⟨64, 48⟩ == .air) "Could not open the passage beyond the crystal."
  state ← walk state 28 1
  let sword ← slotFor state .stoneSword
  for _ in [:240] do
    if let some crawler := state.combat.crawler then
      let difference := crawler.body.x - state.player.x
      let direction := if difference.natAbs > 2 * tileUnits.toNat then
        (if difference < 0 then -1 else 1) else 0
      state := step state {
        horizontal := direction
        selectSlot := some sword
        mine := some crawler.body.centerTile }
      check (!state.player.collides state.world) "Player overlapped terrain in combat."
    else
      break
  check (state.combat.cleared && state.combat.crawler.isNone && state.player.centerTile.x ≥ 66)
    s!"Could not defeat the crawler with the crafted stone sword at {repr state.player.centerTile}: {repr state.combat}"
  return state

private def respawnCheck : IO Unit := do
  let initial := newGame defaultSeed
  let some crawler := initial.combat.crawler | throw (IO.userError "Crawler did not spawn.")
  let exposed := { initial with player := crawler.body, combat := { initial.combat with health := 1 } }
  let next := step exposed {}
  check (next.player.centerTile == initial.player.centerTile && next.combat.health == maxHealth &&
    next.inventory == exposed.inventory && next.world == exposed.world)
    "Lethal contact did not safely respawn the player with their world and gear."

private def saveRoundTrip (state : GameState) : IO Unit := IO.FS.withTempDir fun directory => do
  let path := directory / "world.json"
  match ← Save.write path state with
  | .ok _ => pure ()
  | .error reason => throw (IO.userError reason)
  match ← Save.read path with
  | .ok loaded => check (sameSnapshot loaded state) "Saved state did not round trip."
  | .error reason => throw (IO.userError reason)
  IO.FS.writeFile path "{malformed"
  match ← Save.read path with
  | .ok _ => throw (IO.userError "Malformed save was accepted.")
  | .error _ => check ((← IO.FS.readFile path) == "{malformed") "Bad save file was modified."

private def legacyMigrationCheck : IO Unit := do
  let state := newGame (UInt64.ofNat (UInt64.size - 1))
  let tiles := ByteArray.mk (state.world.tiles.data.map fun code =>
    if code == Tile.timber.code then Tile.air.code else code)
  let state := { state with world := { state.world with tiles }, selected := 5 }
  let json ← (Lean.Json.parse (Save.encode state)).mapError IO.userError |> IO.ofExcept
  let slots ← ((json.getObjVal? "inventory") >>= Lean.Json.getArr?).mapError IO.userError |> IO.ofExcept
  let mut fields : List (String × Lean.Json) := [("version", Lean.toJson (1 : Nat)),
    ("inventory", .arr (slots.extract 0 6))]
  for key in ["seed", "width", "height", "tiles", "player", "selected"] do
    let value ← (json.getObjVal? key).mapError IO.userError |> IO.ofExcept
    fields := (key, value) :: fields
  let migrated ← (Save.decode (Lean.Json.mkObj fields).compress).mapError IO.userError |> IO.ofExcept
  check (migrated.seed == state.seed && migrated.world == state.world && migrated.player == state.player &&
    migrated.selected == 5 && migrated.inventory == state.inventory &&
    migrated.combat == initialCombat state.world) "Version 1 save migration changed the old world or equipment."

def main : IO Unit := do
  check ((newGame defaultSeed).world != (newGame 12345).world) "Seed did not change terrain."
  let complete ← playRoute defaultSeed
  let replay ← playRoute defaultSeed
  check (sameSnapshot complete replay) "The same seed and inputs diverged."
  let _ ← playRoute 12345
  saveRoundTrip complete
  lightingChecks
  respawnCheck
  saveRoundTrip (← playCombatRoute)
  legacyMigrationCheck
  IO.println "Passed terrain, crafting, lighting, combat, respawn, replay, and save checks."
