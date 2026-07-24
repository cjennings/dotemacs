#!/usr/bin/env bats
# Tests for .claude/hooks/validate-el.sh — the auto-test runner.
#
# The runner used to skip entirely above MAX_AUTO_TEST_FILES=20, with no else
# branch: nothing printed, exit 0, indistinguishable from a passing run.  That
# was live for the three largest families here (calendar-sync 63 test files,
# music 45, ai-term 35), so every edit to those ran parens and byte-compile and
# zero tests, silently.
#
# The cap was removed rather than made loud, because its premise did not hold.
# Measured on this machine, running a whole family takes about a second:
# ai-term 208 tests in 1.0s, music 403 in 1.7s, calendar-sync 633 in 0.9s.  It
# was also concealing a real cross-test pollution bug in calendar-sync that
# only appears when that family runs in one process.
#
# These tests pin that no file count is skipped.  Each builds a synthetic
# project in BATS_TEST_TMPDIR and points CLAUDE_PROJECT_DIR at it, so nothing
# runs against the real tree.

setup() {
    HOOK="${BATS_TEST_DIRNAME}/../.claude/hooks/validate-el.sh"
    PROJ="${BATS_TEST_TMPDIR}/proj"
    mkdir -p "$PROJ/modules" "$PROJ/tests"
    export CLAUDE_PROJECT_DIR="$PROJ"
    printf '(provide (quote widget))\n' > "$PROJ/modules/widget.el"
}

# N green test files matching the widget stem.
make_tests() {
    local n="$1" i
    for ((i = 1; i <= n; i++)); do
        printf '(require (quote ert))\n(ert-deftest test-widget-%d () (should t))\n' \
            "$i" > "$PROJ/tests/test-widget-${i}.el"
    done
}

# One failing test file, to prove the run is real rather than merely quiet.
make_failing_test() {
    printf '(require (quote ert))\n(ert-deftest test-widget-bad () (should nil))\n' \
        > "$PROJ/tests/test-widget-bad.el"
}

hook_input() {
    printf '{"tool_input":{"file_path":"%s"}}' "$PROJ/modules/widget.el"
}

run_hook() {
    run bash -c "$(printf '%q' "$HOOK") <<< '$(hook_input)'"
}

# ------------------------------- Normal cases -------------------------------

@test "a small family runs and passes quietly" {
    make_tests 3
    run_hook
    [ "$status" -eq 0 ]
}

@test "a failing test blocks, so a quiet pass means the tests really ran" {
    make_tests 3
    make_failing_test
    run_hook
    [ "$status" -eq 2 ]
    [[ "$output" == *"TESTS FAILED"* ]]
}

# ------------------------------ Boundary cases ------------------------------

@test "at the old cap of 20 files: runs" {
    make_tests 20
    run_hook
    [ "$status" -eq 0 ]
}

@test "past the old cap: still runs, no longer skipped" {
    make_tests 21
    run_hook
    [ "$status" -eq 0 ]
    [[ "${output,,}" != *"skipped"* ]]
}

@test "well past the old cap: a failure in file 63 is still caught" {
    # The regression this guards: at 63 files the runner used to skip, so a red
    # test in a big family reported clean.  calendar-sync is exactly this size.
    make_tests 63
    make_failing_test
    run_hook
    [ "$status" -eq 2 ]
    [[ "$output" == *"TESTS FAILED"* ]]
}

# -------------------------------- Error cases -------------------------------

@test "no matching tests: exits clean without running anything" {
    run_hook
    [ "$status" -eq 0 ]
}
