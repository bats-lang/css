#!/bin/sh
# The luminance table src/contrast.bats is generated: it must be exactly what
# scripts/gen-contrast.py writes, so no entry can be edited by hand.
# The script writes <its parent>/src/contrast.bats, so it runs from a scratch
# tree and the checkout is never written.
#
# usage: tests/static/contrast-table.sh   (python3 must be on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
mkdir "$TMP/scripts" "$TMP/src"
cp "$ROOT/scripts/gen-contrast.py" "$TMP/scripts/gen-contrast.py"
python3 "$TMP/scripts/gen-contrast.py"
if cmp "$TMP/src/contrast.bats" "$ROOT/src/contrast.bats"; then
  echo "ok   contrast-table: src/contrast.bats is what scripts/gen-contrast.py writes"
else
  echo "FAIL contrast-table: src/contrast.bats differs from what scripts/gen-contrast.py writes"
  diff "$TMP/src/contrast.bats" "$ROOT/src/contrast.bats" | head -10 || true
  exit 1
fi
