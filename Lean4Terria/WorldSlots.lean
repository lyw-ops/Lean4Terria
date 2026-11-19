import Lean4Terria.Save
import Std.Time

namespace Lean4Terria.WorldSlots

structure Entry where
  slot : Nat
  name : String
  seed : Option UInt64
  created : String := ""
  favorite : Bool := false
  hardmode : Bool := false
  deriving BEq, Repr, Inhabited

def path (directory : System.FilePath) (slot : Nat) : System.FilePath :=
  directory / (if slot == 0 then "world.json" else s!"world-slot-{slot + 1}.json")

private def slotOfName (name : String) : Option Nat := do
  if name == "world.json" then return 0
  if !name.startsWith "world-slot-" || !name.endsWith ".json" then none else
    let number ← ((name.drop 11).dropEnd 5).toString.toNat?
    if number < 2 || name != s!"world-slot-{number}.json" then none else some (number - 1)

private def metadata (file : System.FilePath) : IO (Option IO.FS.Metadata) := do
  try
    return some (← file.symlinkMetadata)
  catch
    | .noFileOrDirectory .. => return none
    | error => throw error

private def isRegular (file : System.FilePath) (info : IO.FS.Metadata) : IO Bool := do
  if info.type == .symlink then return (← file.metadata).type == .file
  return info.type == .file

def load (directory : System.FilePath) (slot : Nat) : IO (Except String GameState) := do
  let file := path directory slot
  try
    let some info ← metadata file | return .error "This world no longer exists."
    if !(← isRegular file info) then return .error "This world is not a regular save file."
    Save.read file
  catch error => return .error s!"Could not open the world ({error})."

private def inspect (directory : System.FilePath) (slot : Nat) : IO Entry := do
  match ← load directory slot with
  | .ok state =>
    return { slot, name := if state.worldName.isEmpty then s!"World {slot + 1}" else state.worldName
             seed := some state.seed, created := state.worldCreated, favorite := state.worldFavorite
             hardmode := state.hardmode }
  | .error _ =>
    return { slot, name := (path directory slot).fileName.getD "Unreadable world", seed := none }

/-- List existing saves by favorite, then by name. Legacy filenames keep their slot. -/
def scan (directory : System.FilePath) : IO (Array Entry) := do
  let files ← try directory.readDir catch
    | .noFileOrDirectory .. => pure #[]
    | error => throw error
  let entries ← (files.filterMap fun file => slotOfName file.fileName).mapM (inspect directory)
  return entries.qsort fun a b =>
    if a.favorite != b.favorite then a.favorite
    else if a.name.toLower == b.name.toLower then a.slot < b.slot
    else a.name.toLower < b.name.toLower

def nextSlot (catalog : Array Entry) : Nat := Id.run do
  for slot in [:catalog.size + 1] do
    if !catalog.any (·.slot == slot) then return slot
  return catalog.size

private def openStagingFile (directory : System.FilePath) : IO (IO.FS.Handle × System.FilePath) := do
  let pid ← IO.Process.getPID
  let stamp ← IO.monoNanosNow
  for attempt in [:16] do
    let temporary := directory / s!".lean4terria-create.{pid}.{stamp}.{attempt}.tmp"
    try
      let handle ← IO.FS.Handle.mk temporary .writeNew
      return (handle, temporary)
    catch
      | .alreadyExists .. => pure ()
      | error => throw error
  throw (IO.userError "Could not create a staging file for the world.")

private def publish (directory destination : System.FilePath) (state : GameState) : IO Unit := do
  let (handle, temporary) ← openStagingFile directory
  try
    handle.putStr (Save.encode state)
    handle.flush
    IO.FS.hardLink temporary destination
  finally
    try IO.FS.removeFile temporary catch _ => pure ()

private def validName (name : String) : Bool :=
  !name.trimAscii.isEmpty && name.length ≤ 40 && name.toList.all (fun c => c.toNat ≥ 32 && c.toNat != 127)

/-- Publish a complete new world without replacing an existing save path. -/
def create (directory : System.FilePath) (slot : Nat) (seed : UInt64)
    (name : String := s!"World {slot + 1}") : IO (Except String GameState) := do
  if !validName name then return .error "Enter a world name of at most 40 characters."
  let destination := path directory slot
  let occupied := "This world slot already contains a save."
  try
    IO.FS.createDirAll directory
    if (← metadata destination).isSome then return .error occupied
    let created ← Std.Time.PlainDate.now
    let state := { newGame seed with
      worldName := name.trimAscii.toString
      worldCreated := created.toLeanDateString }
    publish directory destination state
    return .ok state
  catch
    | .alreadyExists .. => return .error occupied
    | error => return .error s!"Could not create the world ({error})."

def rename (directory : System.FilePath) (slot : Nat) (name : String) : IO (Except String Unit) := do
  if !validName name then return .error "Enter a world name of at most 40 characters."
  match ← load directory slot with
  | .error reason => return .error reason
  | .ok state => Save.write (path directory slot) { state with worldName := name.trimAscii.toString }

def toggleFavorite (directory : System.FilePath) (slot : Nat) : IO (Except String Unit) := do
  match ← load directory slot with
  | .error reason => return .error reason
  | .ok state => Save.write (path directory slot) { state with worldFavorite := !state.worldFavorite }

/-- Remove a world from the selector, keeping its save recoverable in the same directory tree. -/
def remove (directory : System.FilePath) (slot : Nat) : IO (Except String Unit) := do
  let .ok state ← load directory slot | return .error "Could not read this world."
  if state.worldFavorite then return .error "Unfavorite this world before deleting it."
  let source := path directory slot
  try
    let some info ← metadata source | return .error "This world no longer exists."
    if info.type != .file then return .error "Only local save files can be deleted here."
    let pid ← IO.Process.getPID
    let stamp ← IO.monoNanosNow
    let trash := directory / "Deleted Worlds" / s!"{pid}-{stamp}"
    IO.FS.createDirAll trash
    IO.FS.rename source (trash / (source.fileName.getD "world.json"))
    return .ok ()
  catch error => return .error s!"Could not delete the world ({error})."

def randomSeed : IO UInt64 := do
  let bytes ← IO.getRandomBytes 8
  return bytes.data.foldl (fun seed byte => (seed <<< 8) ||| byte.toUInt64) 0

end Lean4Terria.WorldSlots
