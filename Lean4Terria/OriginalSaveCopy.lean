namespace Lean4Terria.OriginalSaveCopy

private def openStagingFile (directory : System.FilePath) : IO (IO.FS.Handle × System.FilePath) := do
  let pid ← IO.Process.getPID
  let timestamp ← IO.monoNanosNow
  for attempt in [:16] do
    let path := directory / s!".lean4terria.{pid}.{timestamp}.{attempt}.tmp"
    try
      return (← IO.FS.Handle.mk path .writeNew, path)
    catch
      | .alreadyExists .. => pure ()
      | error => throw error
  throw (IO.userError "Could not create a temporary save file.")

/-- Publish a finished copy atomically. Creating the destination link fails
if a file, directory, or symlink already occupies that name. -/
def write (destination : System.FilePath) (bytes : ByteArray) : IO Unit := do
  let directory := destination.parent.getD "."
  IO.FS.createDirAll directory
  let (handle, stagingPath) ← openStagingFile directory
  try
    handle.write bytes
    handle.flush
    IO.FS.hardLink stagingPath destination
  finally
    try IO.FS.removeFile stagingPath catch _ => pure ()

end Lean4Terria.OriginalSaveCopy
