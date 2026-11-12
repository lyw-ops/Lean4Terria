#!/usr/bin/env bash
set -euo pipefail

cd -- "$(dirname -- "$0")/.."
./scripts/setup-raylib.sh
lean_prefix=$(lean --print-prefix)
LEAN_CC=/usr/bin/clang MACOSX_DEPLOYMENT_TARGET=15.0 \
  lake -R -KleanNativeLib="$lean_prefix/lib" build lean4terria
