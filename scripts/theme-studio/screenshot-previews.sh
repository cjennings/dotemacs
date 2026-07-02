#!/usr/bin/env bash
# Screenshot each theme-studio app's page (face table + live preview) headlessly.
#
#   ./screenshot-previews.sh OUTDIR [APP ...]
#   THEME=dupre.json ./screenshot-previews.sh OUTDIR [APP ...]
#
# With no APP arguments, shoots every app in the studio. Rides the #preview=<app>
# hash handler in app.js (the same hash-URL pattern as the browser gates), so the
# page selects the app itself before Chrome takes the shot. THEME names a theme
# JSON next to this script (default dupre.json; THEME= empty shoots untitled) so
# shots show real theme colors -- fresh headless profiles carry no localStorage.
# Regenerate the page first (make gen) when sources changed; this script shoots
# what is on disk.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT="${1:?usage: screenshot-previews.sh OUTDIR [APP ...]}"
shift || true
mkdir -p "$OUT"

BROWSER=""
for b in google-chrome-stable google-chrome chromium chromium-browser; do
  command -v "$b" >/dev/null 2>&1 && { BROWSER="$b"; break; }
done
[ -n "$BROWSER" ] || { echo "no Chromium-family browser found" >&2; exit 1; }

if [ "$#" -gt 0 ]; then
  APPS="$*"
else
  APPS="$(cd "$HERE" && python3 -c 'import generate; print(" ".join(sorted(generate.APPS)))')"
fi

THEME="${THEME-WIP.json}"
SUFFIX=""
if [ -n "$THEME" ]; then
  [ -f "$HERE/$THEME" ] || { echo "theme not found: $HERE/$THEME" >&2; exit 1; }
  SUFFIX="&theme=$THEME"
fi

for app in $APPS; do
  "$BROWSER" --headless=new --disable-gpu --hide-scrollbars \
    --allow-file-access-from-files \
    --window-size=1500,2000 --virtual-time-budget=6000 \
    --screenshot="$OUT/$app.png" \
    "file://$HERE/theme-studio.html#preview=$app$SUFFIX" >/dev/null 2>&1
  if [ -s "$OUT/$app.png" ]; then
    echo "  shot $app"
  else
    echo "  FAILED $app" >&2
  fi
done