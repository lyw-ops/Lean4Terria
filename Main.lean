import Lean4Terria.Save
import Lean4Terria.View

namespace Lean4Terria

open Desktop

private def tickSeconds : Float := 1.0 / 60.0

private def defaultSavePath (seed : Option UInt64) : IO System.FilePath := do
  let name := seed.map (fun value => s!"world-{value}.json") |>.getD "world.json"
  let some home ← IO.getEnv "HOME" | return System.FilePath.mk s!"lean4terria-{name}"
  return System.FilePath.mk home / "Library" / "Application Support" / "Lean4Terria" / name

private structure Options where
  seed : Option UInt64 := none
  savePath : Option System.FilePath := none
  smoke : Bool := false

private def parseOptions : List String → Options → Except String Options
  | [], options => .ok options
  | "--seed" :: value :: rest, options => do
    let some seed := value.toNat? | throw "--seed requires an unsigned 64-bit integer."
    if seed ≥ UInt64.size then throw "--seed requires an unsigned 64-bit integer."
    parseOptions rest { options with seed := some seed.toUInt64 }
  | "--save" :: path :: rest, options =>
    parseOptions rest { options with savePath := some (System.FilePath.mk path) }
  | "--smoke" :: rest, options => parseOptions rest { options with smoke := true }
  | arg :: _, _ => throw s!"Unknown or incomplete option: {arg}"

private def pressed (keys : UInt32) (index : Nat) : Bool :=
  keys &&& ((1 : UInt32) <<< index.toUInt32) != 0

private def selectedSlot (keys : UInt32) : Option Nat := Id.run do
  for index in [:Inventory.slotCount] do
    if pressed keys (8 + index) then return some index
  return none

private def updateInput (pending : TickInput) (keys : UInt32) (target : Option TilePos) : TickInput :=
  let horizontal := (if pressed keys 1 then 1 else 0) - (if pressed keys 0 then 1 else 0)
  { horizontal
    jump := pending.jump || pressed keys 2
    mine := (if pressed keys 3 then target else none).orElse fun _ => pending.mine
    place := (if pressed keys 4 then target else none).orElse fun _ => pending.place
    craft := pending.craft || pressed keys 5
    craftTorches := pending.craftTorches || pressed keys 16
    craftSword := pending.craftSword || pressed keys 17
    selectSlot := (selectedSlot keys).orElse fun _ => pending.selectSlot }

private def playActionSound (before after : GameState) (input : TickInput) : IO Unit := do
  if input.mine.any (fun pos => before.world.tile pos != .air && after.world.tile pos == .air) then
    sound 0
  if input.place.any (fun pos => before.world.tile pos == .air && after.world.tile pos != .air) then
    sound 1
  if (input.craft && Inventory.count before.inventory .copperPick < Inventory.count after.inventory .copperPick) ||
      (input.craftTorches && Inventory.count before.inventory .torch < Inventory.count after.inventory .torch) ||
      (input.craftSword && Inventory.count before.inventory .stoneSword < Inventory.count after.inventory .stoneSword) then
    sound 2
  if after.combat.swing == swingCooldown && before.combat.swing != swingCooldown then sound 3
  if after.combat.invulnerable == hurtCooldown && before.combat.invulnerable != hurtCooldown then sound 4

private def loadWorld (path : System.FilePath) (fallback : GameState) : IO GameState := do
  match ← Save.read path with
  | .ok saved => return saved
  | .error reason =>
    IO.eprintln reason
    return { fallback with status := "Load failed. See terminal for details." }

private def runDesktop (initial : GameState) (savePath : System.FilePath) (smoke : Bool) : IO Unit := do
  let mut state := initial
  let mut pending : TickInput := {}
  let mut remaining := 0.0
  let mut timings : Array Nat := #[]
  openWindow View.width.toUInt32 View.height.toUInt32 "Lean4Terria"
  try
    while !(← shouldClose) && (!smoke || timings.size < 120) do
      let mouseCol := (← mouseX).toNat
      let mouseRow := (← mouseY).toNat
      let keys ← input
      if pressed keys 6 && !smoke then
        match ← Save.write savePath state with
        | .ok _ => state := { state with status := "World saved." }
        | .error reason =>
          IO.eprintln reason
          state := { state with status := "Save failed. See terminal for details." }
      if pressed keys 7 && !smoke then
        state ← loadWorld savePath state
        pending := {}
      else
        let target := (View.cameraFor state).target mouseCol mouseRow
        pending := updateInput pending keys target
      if smoke then
        pending := { horizontal := if timings.size < 40 then 1 else 0 }
      remaining := remaining + min 0.25 (← frameTime)
      while remaining ≥ tickSeconds do
        let next := step state pending
        playActionSound state next pending
        state := next
        pending := { horizontal := pending.horizontal }
        remaining := remaining - tickSeconds
      let camera := View.cameraFor state
      let started ← if smoke then IO.monoNanosNow else pure 0
      View.draw state camera (camera.target mouseCol mouseRow) (state.combat.facing < 0)
      if smoke then
        timings := timings.push ((← IO.monoNanosNow) - started)
  finally
    closeWindow
  if smoke then
    if timings.size != 120 || state.player.x <= initial.player.x then
      throw (IO.userError "Desktop smoke test did not complete its movement sequence.")
    let ordered := timings.qsort (· < ·)
    let median := ordered[ordered.size / 2]!
    let p95 := ordered[(ordered.size * 95 + 99) / 100 - 1]!
    IO.println s!"Desktop smoke test passed ({ordered.size} frames, median {median / 1000} us, p95 {p95 / 1000} us draw + present + frame pacing)."

def run (args : List String) : IO UInt32 := do
  if args == ["--help"] then
    IO.println "Lean4Terria [--seed UINT64] [--save FILE]\nLean4Terria --smoke"
    return 0
  match parseOptions args {} with
  | .error message =>
    IO.eprintln message
    return 2
  | .ok options =>
    let savePath ← options.savePath.map pure |>.getD (defaultSavePath options.seed)
    let initial := newGame (options.seed.getD defaultSeed)
    let initial ← if !options.smoke && (← savePath.pathExists) then
        loadWorld savePath initial else pure initial
    runDesktop initial savePath options.smoke
    return 0

end Lean4Terria

def main (args : List String) : IO UInt32 := Lean4Terria.run args
