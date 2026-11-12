import Lean.Data.Json
import Lean4Terria.Game

namespace Lean4Terria.Save

open Lean (Json FromJson toJson fromJson?)

private def version : Nat := 2

private def encodePlayer (player : Player) : Json :=
  Json.mkObj [
    ("x", toJson player.x), ("y", toJson player.y),
    ("vx", toJson player.vx), ("vy", toJson player.vy),
    ("grounded", toJson player.grounded)
  ]

private def encodeSlot : Option Stack → Json
  | none => .null
  | some stack => Json.mkObj [
    ("item", toJson stack.item.code.toNat), ("count", toJson stack.count)
  ]

private def encodeCrawler : Option Crawler → Json
  | none => .null
  | some crawler => Json.mkObj [
    ("body", encodePlayer crawler.body),
    ("health", toJson crawler.health),
    ("hopIn", toJson crawler.hopIn)
  ]

private def encodeCombat (combat : CombatState) : Json :=
  Json.mkObj [
    ("health", toJson combat.health),
    ("invulnerable", toJson combat.invulnerable),
    ("swing", toJson combat.swing),
    ("facing", toJson combat.facing),
    ("crawler", encodeCrawler combat.crawler),
    ("cleared", toJson combat.cleared)
  ]

/-- Serialize the state needed to continue playing. The current status message is transient. -/
def encode (state : GameState) : String :=
  Json.compress <| Json.mkObj [
    ("version", toJson version),
    ("seed", toJson state.seed),
    ("width", toJson state.world.width),
    ("height", toJson state.world.height),
    ("tiles", .arr (state.world.tiles.data.map fun byte => toJson byte.toNat)),
    ("player", encodePlayer state.player),
    ("inventory", .arr (state.inventory.map encodeSlot)),
    ("selected", toJson state.selected),
    ("combat", encodeCombat state.combat)
  ]

private def readField {α : Type} [FromJson α] (json : Json) (key : String) : Except String α :=
  (json.getObjVal? key >>= fromJson?).mapError fun error => s!"Invalid '{key}' field ({error})"

-- Version 1 used codes through 6 for both tiles and items.
private def acceptsCode (legacy : Bool) (code : Nat) : Bool :=
  code < UInt8.size && (!legacy || code ≤ 6)

private def decodeTileBytes (array : Array Json) (expectedSize : Nat) (legacy : Bool) :
    Except String ByteArray := do
  if array.size != expectedSize then
    throw s!"Expected {expectedSize} tiles, found {array.size}."
  let mut tiles := ByteArray.emptyWithCapacity expectedSize
  for entry in array do
    let code ← entry.getNat?
    let byte := UInt8.ofNat code
    if !(acceptsCode legacy code) || (Tile.ofCode? byte).isNone then
      throw s!"Unknown tile code {code} at index {tiles.size}."
    tiles := tiles.push byte
  return tiles

private def decodeSlot (legacy : Bool) : Json → Except String (Option Stack)
  | .null => pure none
  | json => do
    let code : Nat ← readField json "item"
    let some item := if acceptsCode legacy code then Item.ofCode? (UInt8.ofNat code) else none
      | throw s!"Unknown item code {code}."
    let count : Nat ← readField json "count"
    if count == 0 || count > Inventory.stackLimit item then
      throw s!"Invalid stack size {count}."
    return some { item, count }

private def decodePlayer (json : Json) (world : World) : Except String Player := do
  let x : Int ← readField json "x"
  let y : Int ← readField json "y"
  let vx : Int ← readField json "vx"
  let vy : Int ← readField json "vy"
  let grounded : Bool ← readField json "grounded"
  if vx.natAbs > maxHorizontalSpeed.toNat || vy.natAbs > maxVerticalSpeed.toNat then
    throw "Player velocity exceeds the simulation bounds."
  let player : Player := { x, y, vx, vy, grounded }
  if player.collides world then
    throw "Player position overlaps terrain or lies outside the world."
  return player

private def decodeCrawler (world : World) : Json → Except String (Option Crawler)
  | .null => pure none
  | json => do
    let body ← (decodePlayer (← json.getObjVal? "body") world).mapError fun error =>
      s!"Invalid crawler body ({error})"
    let health : Nat ← readField json "health"
    if health == 0 || health > crawlerMaxHealth then
      throw s!"Invalid crawler health {health}."
    let hopIn : Nat ← readField json "hopIn"
    if hopIn > crawlerHopDelay then
      throw s!"Invalid crawler hop timer {hopIn}."
    return some { body, health, hopIn }

private def decodeCombat (json : Json) (world : World) : Except String CombatState := do
  let health : Nat ← readField json "health"
  if health == 0 || health > maxHealth then
    throw s!"Invalid player health {health}."
  let invulnerable : Nat ← readField json "invulnerable"
  let swing : Nat ← readField json "swing"
  if invulnerable > hurtCooldown || swing > swingCooldown then
    throw "Combat cooldown exceeds the simulation bounds."
  let facing : Int ← readField json "facing"
  if facing != -1 && facing != 1 then
    throw s!"Invalid facing direction {facing}."
  let crawler ← decodeCrawler world (← json.getObjVal? "crawler")
  let cleared : Bool ← readField json "cleared"
  if cleared && crawler.isSome then
    throw "A cleared world cannot have an active crawler."
  return { health, invulnerable, swing, facing, crawler, cleared }

/-- Validate a save before exposing it as a playable state. -/
def decode (text : String) : Except String GameState := do
  let json ← Json.parse text
  let saveVersion : Nat ← readField json "version"
  let legacy := saveVersion == 1
  if !legacy && saveVersion != version then
    throw s!"Unsupported save version {saveVersion}."
  let width : Nat ← readField json "width"
  let height : Nat ← readField json "height"
  if width != worldWidth || height != worldHeight then
    throw s!"Unsupported world dimensions {width} × {height}."
  let tileArray ← (← json.getObjVal? "tiles").getArr?
  let tiles ← decodeTileBytes tileArray (width * height) legacy
  let world : World := { width, height, tiles }
  let player ← decodePlayer (← json.getObjVal? "player") world
  let slots ← (← json.getObjVal? "inventory").getArr?
  let expectedSlots := if legacy then 6 else Inventory.slotCount
  if slots.size != expectedSlots then
    throw s!"Expected {expectedSlots} inventory slots, found {slots.size}."
  let inventory ← slots.mapM (decodeSlot legacy)
  let selected : Nat ← readField json "selected"
  if selected ≥ slots.size then
    throw s!"Selected slot {selected} is outside the inventory."
  let seed : UInt64 ← readField json "seed"
  if legacy then
    let inventory := (inventory.push none).push (some ⟨.wood, startingWood⟩)
    let combat := initialCombat world
    return { seed, world, player, inventory, selected, combat, status := "Version 1 world loaded." }
  let combat ← decodeCombat (← json.getObjVal? "combat") world
  return { seed, world, player, inventory, selected, combat, status := "World loaded." }

private def openTemporaryFile (directory : System.FilePath) : IO (IO.FS.Handle × System.FilePath) := do
  let pid ← IO.Process.getPID
  let timestamp ← IO.monoNanosNow
  for attempt in [:16] do
    let path := directory / s!".lean4terria-save.{pid}.{timestamp}.{attempt}.tmp"
    try
      let handle ← IO.FS.Handle.mk path .writeNew
      return (handle, path)
    catch
      | .alreadyExists .. => pure ()
      | error => throw error
  throw (IO.userError "Could not create an unused temporary save file.")

/-- Flush a uniquely named file beside the destination before atomically replacing the save. -/
def write (path : System.FilePath) (state : GameState) : IO (Except String Unit) := do
  try
    let directory := path.parent.getD "."
    IO.FS.createDirAll directory
    let (handle, temporary) ← openTemporaryFile directory
    try
      handle.putStr (encode state)
      handle.flush
      IO.FS.rename temporary path
    catch error =>
      try IO.FS.removeFile temporary catch _ => pure ()
      throw error
    return .ok ()
  catch error =>
    return .error s!"Could not write save ({error})."

/-- Read a save without altering the original file, including when validation fails. -/
def read (path : System.FilePath) : IO (Except String GameState) := do
  try
    return decode (← IO.FS.readFile path)
  catch error =>
    return .error s!"Could not read save ({error})."

end Lean4Terria.Save
