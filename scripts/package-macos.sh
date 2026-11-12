#!/usr/bin/env bash
# Package an already-built macOS executable. Usage: scripts/package-macos.sh [BINARY]
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "$0")/.." && pwd)
binary=${1:-"$repo_dir/.lake/build/bin/lean4terria"}
version=0.2.0
zip_name="Lean4Terria-v$version-macos-arm64.zip"

die() { printf 'package-macos: %s\n' "$*" >&2; exit 1; }

[[ $# -le 1 ]] || die 'Usage: scripts/package-macos.sh [BINARY]'
[[ $(uname -s) == Darwin && $(uname -m) == arm64 ]] || die 'Apple Silicon macOS is required.'
[[ -x "$binary" && -f "$binary" ]] || die "Build the executable first: $binary"
[[ $(file -b "$binary") == 'Mach-O 64-bit executable arm64'* ]] || die 'Expected an arm64 Mach-O executable.'

build_info=$(otool -l "$binary" | awk '
  $1 == "cmd" { building = ($2 == "LC_BUILD_VERSION") }
  building && $1 == "platform" { platform = $2 }
  building && $1 == "minos" { minimum = $2 }
  END { print platform " " minimum }
')
case "${build_info%% *}" in
  1|MACOS) minimum=${build_info#* } ;;
  *) die 'The executable does not declare macOS as its platform.' ;;
esac
[[ "$minimum" =~ ^[0-9]+(\.[0-9]+){0,2}$ ]] || die 'Could not read the minimum macOS version.'

linked_libraries=$(otool -L "$binary")
external_libraries=$(printf '%s\n' "$linked_libraries" | awk 'NR > 1 && $1 !~ /^\/(usr\/lib|System\/Library)\// { print $1 }')
[[ -z "$external_libraries" ]] || die "Executable depends on non-system libraries: $external_libraries"

# Stage on the local filesystem. iCloud can attach FinderInfo to an app in Documents,
# which invalidates strict signature verification even after a successful signing pass.
stage=$(mktemp -d "${TMPDIR:-/tmp}/lean4terria-package.XXXXXXXX")
pending_zip=''
cleanup() {
  [[ -z "$pending_zip" ]] || rm -f -- "$pending_zip"
  rm -rf -- "$stage"
}
trap cleanup EXIT

app="$stage/Lean4Terria.app"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources"
cp -X "$binary" "$app/Contents/MacOS/lean4terria"
chmod 755 "$app/Contents/MacOS/lean4terria"
cp "$(cd "$repo_dir" && lean --print-prefix)/LICENSE" "$app/Contents/Resources/Lean-LICENSE.txt"
cp "$repo_dir/.lake/raylib-6.2_macos/LICENSE" "$app/Contents/Resources/raylib-LICENSE.txt"

cat > "$app/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key><string>en</string>
    <key>CFBundleDisplayName</key><string>Lean4Terria</string>
    <key>CFBundleExecutable</key><string>lean4terria</string>
    <key>CFBundleIdentifier</key><string>io.github.lyw-ops.Lean4Terria</string>
    <key>CFBundleName</key><string>Lean4Terria</string>
    <key>CFBundlePackageType</key><string>APPL</string>
    <key>CFBundleShortVersionString</key><string>$version</string>
    <key>CFBundleVersion</key><string>$version</string>
    <key>LSApplicationCategoryType</key><string>public.app-category.games</string>
    <key>LSMinimumSystemVersion</key><string>$minimum</string>
    <key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
PLIST
plutil -lint -s "$app/Contents/Info.plist"
codesign --force --sign - --timestamp=none "$app"
codesign --verify --strict --deep "$app"

ditto -c -k --norsrc --noextattr --noqtn --noacl --keepParent "$app" "$stage/$zip_name"
entries=$(zipinfo -1 "$stage/$zip_name")
if printf '%s\n' "$entries" | grep -Eq '(^|/)(\._|__MACOSX/)'; then
  die 'ZIP contains AppleDouble metadata entries.'
fi
ditto -x -k "$stage/$zip_name" "$stage/verify"
codesign --verify --strict --deep "$stage/verify/Lean4Terria.app"

mkdir -p "$repo_dir/dist"
output="$repo_dir/dist/$zip_name"
[[ ! -d "$output" ]] || die "Expected a file path: $output"
pending_zip=$(mktemp "$repo_dir/dist/.$zip_name.XXXXXXXX")
cp "$stage/$zip_name" "$pending_zip"
chmod 644 "$pending_zip"
mv -f -- "$pending_zip" "$output"
pending_zip=''
printf 'Packaged %s (ad-hoc signed, macOS %s or later).\n' "$output" "$minimum"
