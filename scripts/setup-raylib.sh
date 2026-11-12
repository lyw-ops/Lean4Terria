#!/usr/bin/env bash
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "$0")/.." && pwd)
if [[ $(uname -s) != Darwin || $(uname -m) != arm64 ]]; then
  echo 'The desktop build currently targets Apple Silicon macOS.' >&2
  exit 1
fi

version=6.2
directory="raylib-${version}_macos"
archive="$directory.tar.gz"
expected_sha=100437734a44d56ed9d2a1c4b249dbfc6d6cc3477a45e306f9f56fe5f1e2b4ef
destination="$repo_dir/.lake/$directory"
if [[ -f "$destination/include/raylib.h" && -f "$destination/lib/libraylib.a" ]]; then
  exit 0
fi

mkdir -p "$repo_dir/.lake"
staging=$(mktemp -d "$repo_dir/.lake/raylib.XXXXXXXX")
trap 'rm -rf -- "$staging"' EXIT
curl --fail --silent --show-error --location --retry 2 \
  "https://github.com/raysan5/raylib/releases/download/$version/$archive" \
  --output "$staging/$archive"
(
  cd -- "$staging"
  printf '%s  %s\n' "$expected_sha" "$archive" | shasum -a 256 --check --status
  tar -xzf "$archive" "$directory/include" "$directory/lib/libraylib.a" "$directory/LICENSE"
) || {
  echo 'The raylib archive failed verification or extraction.' >&2
  exit 1
}
mv -n -- "$staging/$directory" "$repo_dir/.lake/"
test -f "$destination/include/raylib.h" && test -f "$destination/lib/libraylib.a"
