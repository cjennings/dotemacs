#!/usr/bin/env bash
# Validate and test .el files after Edit/Write/MultiEdit.
# PostToolUse hook: receives tool-call JSON on stdin.
#
# On success: exit 0 silent.
# On failure: emit JSON with hookSpecificOutput.additionalContext so Claude
# sees a structured error in its context, THEN exit 2 to block the tool
# pipeline. stderr still echoes the error for terminal visibility.
#
# Phase 1: check-parens + byte-compile
# Phase 2: for non-test .el files, run matching tests/test-<stem>*.el

set -u

# Emit a JSON failure payload and exit 2. Arguments:
#   $1 — short failure type (e.g. "PAREN CHECK FAILED")
#   $2 — file path
#   $3 — emacs output (error body)
fail_json() {
    local ctx
    ctx="$(printf '%s: %s\n\n%s\n\nFix before proceeding.' "$1" "$2" "$3" \
        | jq -Rs .)"
    cat <<EOF
{"hookSpecificOutput": {"hookEventName": "PostToolUse", "additionalContext": $ctx}}
EOF
    printf '%s: %s\n%s\n' "$1" "$2" "$3" >&2
    exit 2
}

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
      fail_json "PAREN CHECK FAILED" "$f" "$output"
    fi
    ;;
  *.el)
    if ! output="$(emacs --batch --no-site-file --no-site-lisp \
                     -L "$PROJECT_ROOT" \
                     -L "$PROJECT_ROOT/modules" \
                     -L "$PROJECT_ROOT/tests" \
                     --eval '(package-initialize)' \
                     "$f" \
                     --eval '(check-parens)' \
                     --eval "(or (byte-compile-file \"$f\") (kill-emacs 1))" 2>&1)"; then
      fail_json "VALIDATION FAILED" "$f" "$output"
    fi
    ;;
esac

# --- Phase 2: test runner ---
# Determine which tests (if any) apply to this edit. Works for projects with
# source at root, in modules/, or elsewhere — stem-based test lookup is the
# common pattern.
tests=()
case "$f" in
  */init.el|*/early-init.el)
    : # Phase 1 handled it; skip test runner
    ;;
  "$PROJECT_ROOT/tests/testutil-"*.el)
    stem="$(basename "${f%.el}")"
    stem="${stem#testutil-}"
    mapfile -t tests < <(find "$PROJECT_ROOT/tests" -maxdepth 1 -name "test-${stem}*.el" 2>/dev/null | sort)
    ;;
  "$PROJECT_ROOT/tests/test-"*.el)
    tests=("$f")
    ;;
  *.el)
    # Any other .el under the project — find matching tests by stem
    stem="$(basename "${f%.el}")"
    mapfile -t tests < <(find "$PROJECT_ROOT/tests" -maxdepth 1 -name "test-${stem}*.el" 2>/dev/null | sort)
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
                   --eval '(package-initialize)' \
                   -l ert "${load_args[@]}" \
                   --eval "(ert-run-tests-batch-and-exit '(not (tag :slow)))" 2>&1)"; then
    fail_json "TESTS FAILED ($count test file(s))" "$f" "$output"
  fi
fi

exit 0
