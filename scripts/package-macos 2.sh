#!/bin/bash
# Package an already-built Lean4Terria executable as a macOS app bundle and ZIP.
#
# Usage: scripts/package-macos.sh [BINARY]
#
#   BINARY  Apple Silicon (arm64) macOS executable to package.
#           Default: <repo>/.lake/build/bin/lean4terria
#           A relative path is resolved against the current directory.
#
# Outputs (always under the repository root, replaced on every run):
#   dist/Lean4Terria.app
#   dist/Lean4Terria-macos-arm64.zip
#
# The app is ad-hoc signed (codesign --sign -). It is not signed with a
# Developer ID and not notarized.
#
# Environment:
#   LEAN4TERRIA_ALLOW_EXTERNAL_DYLIBS=1
#       Package even if the executable links dynamic libraries outside
#       /usr/lib and /System/Library. Such an app will usually not start on
#       other Macs; use only for local testing.

set -euo pipefail
umask 022

readonly APP_NAME="Lean4Terria"
readonly BUNDLE_ID="io.github.lyw-ops.Lean4Terria"
readonly APP_VERSION="0.1.0"
readonly EXECUTABLE_NAME="lean4terria"
readonly ZIP_NAME="Lean4Terria-macos-arm64.zip"
readonly REQUIRED_ARCH="arm64"

log() { printf 'package-macos: %s\n' "$*"; }
die() { printf 'package-macos: error: %s\n' "$*" >&2; exit 1; }

usage() {
  sed -n '2,21p' "$0" | sed 's/^# \{0,1\}//'
}

case "${1:-}" in
  -h|--help) usage; exit 0 ;;
esac
if [ "$#" -gt 1 ]; then
  usage >&2
  exit 2
fi

[ "$(uname -s)" = "Darwin" ] || die "this script must run on macOS"

# Resolve the binary path relative to the caller before changing directory.
if [ "$#" -eq 1 ]; then
  [ -n "$1" ] || die "binary path is empty"
  case "$1" in
    /*) BINARY="$1" ;;
    *)  BINARY="$PWD/$1" ;;
  esac
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd -P)"
DIST_DIR="$REPO_ROOT/dist"
APP_OUT="$DIST_DIR/$APP_NAME.app"
ZIP_OUT="$DIST_DIR/$ZIP_NAME"
BINARY="${BINARY:-$REPO_ROOT/.lake/build/bin/$EXECUTABLE_NAME}"

# --- Tools -----------------------------------------------------------------

for tool in codesign ditto plutil file xattr; do
  command -v "$tool" >/dev/null 2>&1 || die "required tool not found: $tool"
done
# lipo and otool are Command Line Tools shims; make sure they actually work.
xcode-select -p >/dev/null 2>&1 \
  || die "Xcode Command Line Tools are not installed (run: xcode-select --install)"
for tool in lipo otool; do
  xcrun --find "$tool" >/dev/null 2>&1 || die "required tool not found: $tool"
done

# --- Validate the input executable -------------------------------------------

[ -e "$BINARY" ] || die "executable not found: $BINARY (build it first, or pass a path)"
[ -f "$BINARY" ] || die "not a regular file: $BINARY"
[ -x "$BINARY" ] || die "file is not executable: $BINARY"

file_desc="$(file -b "$BINARY")"
case "$file_desc" in
  "Mach-O 64-bit executable $REQUIRED_ARCH"*) ;;
  *) die "not a thin $REQUIRED_ARCH Mach-O executable: $BINARY ($file_desc)" ;;
esac

archs="$(lipo -archs "$BINARY" 2>/dev/null)" || die "lipo could not read: $BINARY"
[ "$archs" = "$REQUIRED_ARCH" ] \
  || die "expected architecture '$REQUIRED_ARCH' only, found '$archs': $BINARY"

# Platform and minimum macOS version from LC_BUILD_VERSION.
build_info="$(otool -l "$BINARY" | awk '
  $1 == "cmd" { in_bv = ($2 == "LC_BUILD_VERSION") }
  in_bv && $1 == "platform" { platform = $2 }
  in_bv && $1 == "minos" { minos = $2 }
  END { print platform " " minos }
')"
platform="${build_info%% *}"
min_os="${build_info#* }"
case "$platform" in
  1|MACOS) ;;
  "") die "no LC_BUILD_VERSION load command found: $BINARY" ;;
  *) die "executable is not built for macOS (platform $platform): $BINARY" ;;
esac
printf '%s' "$min_os" | grep -Eq '^[0-9]+(\.[0-9]+){0,2}$' \
  || die "could not read minimum macOS version (got '$min_os'): $BINARY"

# Dynamic libraries must come from the OS; nothing else is shipped in the app.
external_libs="$(otool -L "$BINARY" | tail -n +2 | awk '{print $1}' \
  | grep -Ev '^(/usr/lib/|/System/Library/)' || true)"
if [ -n "$external_libs" ]; then
  if [ "${LEAN4TERRIA_ALLOW_EXTERNAL_DYLIBS:-0}" = "1" ]; then
    log "warning: executable links non-system libraries (app may not run on other Macs):" >&2
    printf '%s\n' "$external_libs" | sed 's/^/  /' >&2
  else
    printf 'package-macos: error: executable links libraries that are not part of macOS:\n' >&2
    printf '%s\n' "$external_libs" | sed 's/^/  /' >&2
    die "link them statically, or set LEAN4TERRIA_ALLOW_EXTERNAL_DYLIBS=1 for a local-only build"
  fi
fi

log "input:  $BINARY ($archs, macOS >= $min_os)"

# --- Assemble in a temporary directory -------------------------------------

mkdir -p "$DIST_DIR"
# Stage inside dist/ so the final moves are renames on the same volume.
STAGE="$(mktemp -d "$DIST_DIR/.package-macos.XXXXXX")" \
  || die "could not create staging directory in $DIST_DIR"
cleanup() { rm -rf "$STAGE"; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

APP_STAGE="$STAGE/$APP_NAME.app"
ZIP_STAGE="$STAGE/$ZIP_NAME"

mkdir -p "$APP_STAGE/Contents/MacOS"
cp -X "$BINARY" "$APP_STAGE/Contents/MacOS/$EXECUTABLE_NAME"
chmod 755 "$APP_STAGE/Contents/MacOS/$EXECUTABLE_NAME"

cat > "$APP_STAGE/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>en</string>
	<key>CFBundleDisplayName</key>
	<string>$APP_NAME</string>
	<key>CFBundleExecutable</key>
	<string>$EXECUTABLE_NAME</string>
	<key>CFBundleIdentifier</key>
	<string>$BUNDLE_ID</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundleName</key>
	<string>$APP_NAME</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleShortVersionString</key>
	<string>$APP_VERSION</string>
	<key>CFBundleVersion</key>
	<string>$APP_VERSION</string>
	<key>LSApplicationCategoryType</key>
	<string>public.app-category.games</string>
	<key>LSMinimumSystemVersion</key>
	<string>$min_os</string>
	<key>NSHighResolutionCapable</key>
	<true/>
</dict>
</plist>
PLIST
plutil -lint -s "$APP_STAGE/Contents/Info.plist" || die "generated Info.plist is invalid"

# Extended attributes (quarantine, Finder info) break signing; drop them.
xattr -cr "$APP_STAGE" 2>/dev/null || true

# --- Sign and verify ---------------------------------------------------------

sign_out="$(codesign --force --sign - --timestamp=none "$APP_STAGE" 2>&1)" \
  || { printf '%s\n' "$sign_out" >&2; die "ad-hoc signing failed"; }
codesign --verify --strict --deep "$APP_STAGE" 2>&1 \
  || die "signature verification failed for staged app"

signed_id="$(codesign -dv "$APP_STAGE" 2>&1 | sed -n 's/^Identifier=//p')"
[ "$signed_id" = "$BUNDLE_ID" ] \
  || die "signed identifier is '$signed_id', expected '$BUNDLE_ID'"

# --- ZIP and verify round trip -----------------------------------------------

ditto -c -k --keepParent "$APP_STAGE" "$ZIP_STAGE" || die "ditto could not create ZIP"

mkdir "$STAGE/unzip"
ditto -x -k "$ZIP_STAGE" "$STAGE/unzip" || die "ZIP could not be extracted"
top_level="$(ls -A "$STAGE/unzip")"
[ "$top_level" = "$APP_NAME.app" ] \
  || die "ZIP top level should be only $APP_NAME.app, found: $top_level"
codesign --verify --strict --deep "$STAGE/unzip/$APP_NAME.app" 2>&1 \
  || die "signature verification failed for app extracted from ZIP"
[ -x "$STAGE/unzip/$APP_NAME.app/Contents/MacOS/$EXECUTABLE_NAME" ] \
  || die "extracted app lost its executable permission"

# --- Install into dist/ -------------------------------------------------------

if [ -e "$APP_OUT" ] || [ -L "$APP_OUT" ]; then
  mv "$APP_OUT" "$STAGE/previous.app" || die "could not move aside existing $APP_OUT"
fi
if ! mv "$APP_STAGE" "$APP_OUT"; then
  [ -e "$STAGE/previous.app" ] && mv "$STAGE/previous.app" "$APP_OUT"
  die "could not install $APP_OUT"
fi
mv -f "$ZIP_STAGE" "$ZIP_OUT" || die "could not install $ZIP_OUT"

log "app:    $APP_OUT"
log "zip:    $ZIP_OUT ($(du -h "$ZIP_OUT" | awk '{print $1}'))"
log "signed ad-hoc as $BUNDLE_ID $APP_VERSION (not notarized)"
