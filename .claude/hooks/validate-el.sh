#!/usr/bin/env bash
# Validate and test .el files after Edit/Write/MultiEdit.
# PostToolUse hook: receives tool-call JSON on stdin.
# Silent on success; on failure, prints emacs output and exits 2
# so Claude sees the error and can correct it.
#
# Phase 1: check-parens + byte-compile
# Phase 2: for modules/*.el, run matching tests/test-<stem>*.el

set -u

# Portable project root: prefer Claude Code's env var, fall back to deriving
# from this script's location ($project/.claude/hooks/validate-el.sh).
PROJECT_ROOT="${CLAUDE_PROJECT_DIR:-$(cd "$(dirname "$0")/../.." && pwd)}"

f="$(jq -r '.tool_input.file_path // .tool_response.filePath // empty')"
[ -z "$f" ] && exit 0
[ "${f##*.}" = "el" ] || exit 0

MAX_AUTO_TEST_FILES=20  # skip if more matches than this (large test suites)

# --- Phase 1: syntax + byte-compile ---
case "$f" in
  */init.el|*/early-init.el)
    # Byte-compile here would load the full package graph. Parens only.
    if ! output="$(emacs --batch --no-site-file --no-site-lisp "$f" \
                     --eval '(check-parens)' 2>&1)"; then
      printf 'PAREN CHECK FAILED: %s\n%s\n' "$f" "$output" >&2
      exit 2
    fi
    ;;
  *.el)
    if ! output="$(emacs --batch --no-site-file --no-site-lisp \
                     -L "$PROJECT_ROOT" \
                     -L "$PROJECT_ROOT/modules" \
                     -L "$PROJECT_ROOT/tests" \
                     "$f" \
                     --eval '(check-parens)' \
                     --eval "(or (byte-compile-file \"$f\") (kill-emacs 1))" 2>&1)"; then
      printf 'VALIDATION FAILED: %s\n%s\n' "$f" "$output" >&2
      exit 2
    fi
    ;;
esac

# --- Phase 2: test runner ---
# Determine which tests (if any) apply to this edit.
tests=()
case "$f" in
  "$PROJECT_ROOT/modules/"*.el)
    stem="$(basename "${f%.el}")"
    mapfile -t tests < <(find "$PROJECT_ROOT/tests" -maxdepth 1 -name "test-${stem}*.el" 2>/dev/null | sort)
    ;;
  "$PROJECT_ROOT/tests/testutil-"*.el)
    stem="$(basename "${f%.el}")"
    stem="${stem#testutil-}"
    mapfile -t tests < <(find "$PROJECT_ROOT/tests" -maxdepth 1 -name "test-${stem}*.el" 2>/dev/null | sort)
    ;;
  "$PROJECT_ROOT/tests/test-"*.el)
    tests=("$f")
    ;;
esac

count="${#tests[@]}"
if [ "$count" -ge 1 ] && [ "$count" -le "$MAX_AUTO_TEST_FILES" ]; then
  load_args=()
  for t in "${tests[@]}"; do load_args+=("-l" "$t"); done
  if ! output="$(emacs --batch --no-site-file --no-site-lisp \
                   -L "$PROJECT_ROOT" \
                   -L "$PROJECT_ROOT/modules" \
                   -L "$PROJECT_ROOT/tests" \
                   -l ert "${load_args[@]}" \
                   --eval "(ert-run-tests-batch-and-exit '(not (tag :slow)))" 2>&1)"; then
    printf 'TESTS FAILED for %s (%d test file(s)):\n%s\n' "$f" "$count" "$output" >&2
    exit 2
  fi
fi

exit 0
