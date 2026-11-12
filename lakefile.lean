import Lake
open Lake DSL System

package Lean4Terria where
  moreLeancArgs := if Platform.isOSX then #["-mmacosx-version-min=15.0"] else #[]

@[default_target]
lean_lib Lean4Terria

lean_exe lean4terria_test where
  root := `Tests

input_file raylibArchive where
  path := ".lake/raylib-6.2_macos/lib/libraylib.a"
  text := false

input_file desktopBridge where
  path := "native/desktop.c"
  text := true

target desktopObject pkg : FilePath := do
  let source ← desktopBridge.fetch
  let includeDir ← getLeanIncludeDir
  let raylibInclude := pkg.dir / ".lake" / "raylib-6.2_macos" / "include"
  buildO (pkg.buildDir / "native" / "desktop.o") source
    #["-I", includeDir.toString, "-I", raylibInclude.toString]
    #["-std=c11", "-O2", "-Wall", "-Wextra", "-mmacosx-version-min=15.0"] "/usr/bin/clang"

lean_exe lean4terria where
  root := `Main
  moreLinkObjs := #[desktopObject, raylibArchive]
  weakLinkArgs := (get_config? leanNativeLib).map (fun directory => #["-L", directory]) |>.getD #[]
  moreLinkArgs := #["-framework", "Cocoa", "-framework", "IOKit", "-framework", "OpenGL",
    "-framework", "CoreVideo", "-framework", "CoreAudio", "-framework", "AudioToolbox",
    "-framework", "CoreFoundation", "-framework", "CoreGraphics", "-framework", "QuartzCore"]
