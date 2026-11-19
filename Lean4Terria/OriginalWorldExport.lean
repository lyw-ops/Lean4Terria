import Lean4Terria.OriginalSaveCopy
import Lean4Terria.OriginalWorld

namespace Lean4Terria.OriginalWorld

def Archive.exportCopy (archive : Archive) (terrain : Terrain) (destination : System.FilePath)
    (chests : Option (Array Chest) := none) (signs : Option (Array Sign) := none)
    (progress : SaveProgress := {}) : IO Unit := do
  let bytes ← IO.ofExcept ((archive.encode terrain chests signs progress).mapError IO.userError)
  OriginalSaveCopy.write destination bytes

end Lean4Terria.OriginalWorld
