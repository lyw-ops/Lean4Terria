import Lean4Terria

open Lean4Terria

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def sameSnapshot (left right : GameState) : Bool :=
  left.seed == right.seed && left.world == right.world && left.player == right.player &&
    left.inventory == right.inventory && left.selected == right.selected

private def walk (state : GameState) (ticks : Nat) (direction : Int) (jump := false) : IO GameState := do
  let mut state := state
  for index in [:ticks] do
    state := step state { horizontal := direction, jump := jump && index == 0 }
    check (!state.player.collides state.world) s!"Player overlapped a tile at movement tick {index}."
  return state

private def playRoute (seed : UInt64) : IO GameState := do
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
  for column in [35:38] do
    mined := step mined { mine := some ⟨column, 42⟩ }
    check (mined.world.tile ⟨column, 42⟩ == .air) "Copper tile was not mined."
  let repeated := step mined { mine := some ⟨35, 42⟩ }
  check (Inventory.count mined.inventory .copper == 3 && repeated.inventory == mined.inventory)
    "Copper was lost or duplicated during mining."

  let retreat ← walk mined 27 (-1)
  let climbed ← walk retreat 42 (-1) true
  let bench ← walk climbed 120 (-1)
  let crafted := step bench { craft := true }
  check (crafted.selectedStack.any (·.item == .copperPick) &&
    Inventory.count crafted.inventory .copper == 0) "Copper pick crafting failed."

  let deep ← walk crafted 420 1
  let wrongTool := step deep { selectSlot := some 0, mine := some ⟨64, 49⟩ }
  check (wrongTool.world.tile ⟨64, 49⟩ == .crystal) "The basic pick mined crystal."
  let complete := step deep { mine := some ⟨64, 49⟩ }
  check (Inventory.count complete.inventory .crystal == 1 && complete.world.tile ⟨64, 49⟩ == .air)
    "The copper pick did not collect crystal."
  return complete

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

def main : IO Unit := do
  check ((newGame defaultSeed).world != (newGame 12345).world) "Seed did not change terrain."
  let complete ← playRoute defaultSeed
  let replay ← playRoute defaultSeed
  check (sameSnapshot complete replay) "The same seed and inputs diverged."
  let _ ← playRoute 12345
  saveRoundTrip complete
  IO.println "Passed terrain, collision, mining, placement, crafting, crystal, replay, and save checks."
