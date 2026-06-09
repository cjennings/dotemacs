#!/usr/bin/env bash
# Test runner for the theme-studio tool. Drives the whole pyramid in one command:
#   - regenerate theme-studio.html from generate.py
#   - Python templating tests (export-strip + placeholder substitution)
#   - Node unit tests for colormath.js (+ inline-integrity)
#   - syntax-check the spliced page <script>
#   - browser hash gates in headless Chrome (#selftest and the metric tests)
#
# Exit status is non-zero if any stage fails. Browser gates need a Chromium-family
# browser; when none is found they are reported as SKIPPED (not passed) so the
# gap is visible rather than silently green.
#
# Usage: scripts/theme-studio/run-tests.sh   (or: make theme-studio-test)
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"

# --no-browser skips the headless-Chrome hash gates for a fast inner loop.
NO_BROWSER=0
[ "${1:-}" = "--no-browser" ] && NO_BROWSER=1

fail=0
pass_msg() { printf '  PASS  %s\n' "$1"; }
fail_msg() { printf '  FAIL  %s\n' "$1"; fail=1; }
skip_msg() { printf '  SKIP  %s\n' "$1"; }

echo "theme-studio tests"

# 1. Regenerate the page (the browser gates and inline-integrity read it).
if python3 generate.py >/dev/null; then pass_msg "generate.py wrote theme-studio.html"
else fail_msg "generate.py failed"; fi

# 2. Python templating tests.
if python3 -m unittest test_generate >/tmp/ts-pytest.log 2>&1; then
  pass_msg "Python templating tests ($(grep -oE 'Ran [0-9]+' /tmp/ts-pytest.log | awk '{print $2}') tests)"
else fail_msg "Python templating tests"; sed 's/^/        /' /tmp/ts-pytest.log; fi

# 3. Node unit tests + inline-integrity. Node 26 broke `--test <dir>`; glob the files.
if node --test ./*.mjs >/tmp/ts-node.log 2>&1; then
  pass_msg "Node unit tests ($(grep -E '^. tests' /tmp/ts-node.log | grep -oE '[0-9]+' | head -1) tests)"
else fail_msg "Node unit tests"; grep -E 'not ok|AssertionError|Error' /tmp/ts-node.log | sed 's/^/        /' | head -20; fi

# 4. Syntax-check the inlined page script.
python3 - <<'PY' && node --check /tmp/ts-script.js >/dev/null 2>&1 && pass_msg "spliced page <script> parses" || fail_msg "spliced page <script> syntax"
import re
h = open('theme-studio.html').read()
open('/tmp/ts-script.js', 'w').write(re.search(r'<script>(.*)</script>', h, re.S).group(1))
PY

# 5. Browser hash gates.
CHROME=""
for c in google-chrome-stable google-chrome chromium chromium-browser; do
  if command -v "$c" >/dev/null 2>&1; then CHROME="$c"; break; fi
done
HASHES="selftest cursortest readouttest deltatest oklchtest planetest locktest sorttest mocktest ramptest"
if [ "$NO_BROWSER" = 1 ]; then
  skip_msg "browser hash gates (--no-browser)"
elif [ -z "$CHROME" ]; then
  for t in $HASHES; do skip_msg "#$t (no Chromium-family browser found)"; done
else
  PROF="$(mktemp -d)"
  trap 'rm -rf "$PROF"' EXIT
  for t in $HASHES; do
    upper="$(echo "$t" | tr '[:lower:]' '[:upper:]')"
    res="$("$CHROME" --headless --no-sandbox --disable-gpu --user-data-dir="$PROF" \
      --virtual-time-budget=8000 --dump-dom "file://$HERE/theme-studio.html#$t" 2>/dev/null \
      | grep -o "${upper}[^<]*" | head -1)"
    case "$res" in
      *PASS*) pass_msg "#$t" ;;
      *FAIL*) fail_msg "#$t -> $res" ;;
      *)      fail_msg "#$t -> no verdict (browser did not run the test)" ;;
    esac
  done
fi

echo
if [ "$fail" -eq 0 ]; then echo "all stages green"; else echo "FAILURES above"; fi
exit "$fail"
