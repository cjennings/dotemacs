#!/usr/bin/env bats
# Tests for .claude/hooks/validate-el.sh — specifically the auto-test cap.
#
# The hook runs the tests matching an edited file, but only when the match
# count is between 1 and MAX_AUTO_TEST_FILES.  Above the cap the whole block
# was skipped with no else branch: nothing printed, exit 0, indistinguishable
# from a passing run.  That is live for the three largest families here
# (calendar-sync 63 test files, music 45, ai-term 35), so every edit to those
# modules ran parens and byte-compile and zero tests, silently.
#
# The cap itself is fine — running 63 files per keystroke is not wanted.  The
# defect is the silence, so these tests assert the skip announces itself and
# names what to run.
#
# Each test builds a synthetic project in BATS_TEST_TMPDIR and points
# CLAUDE_PROJECT_DIR at it, so nothing runs against the real tree.

setup() {
    HOOK="${BATS_TEST_DIRNAME}/../.claude/hooks/validate-el.sh"
    PROJ="${BATS_TEST_TMPDIR}/proj"
    mkdir -p "$PROJ/modules" "$PROJ/tests"
    export CLAUDE_PROJECT_DIR="$PROJ"
    printf '(provide (quote widget))\n' > "$PROJ/modules/widget.el"
}

# Create N test files matching the widget stem.  Each is trivially green so a
# run below the cap succeeds and the only variable is the count.
make_tests() {
    local n="$1" i
    for ((i = 1; i <= n; i++)); do
        printf '(require (quote ert))\n(ert-deftest test-widget-%d () (should t))\n' \
            "$i" > "$PROJ/tests/test-widget-${i}.el"
    done
}

hook_input() {
    printf '{"tool_input":{"file_path":"%s"}}' "$PROJ/modules/widget.el"
}

# ------------------------------- Normal cases -------------------------------

@test "under the cap: runs the tests and stays quiet on success" {
    make_tests 3
    run bash -c "$(printf '%q' "$HOOK") <<< '$(hook_input)'"
    [ "$status" -eq 0 ]
    [[ "${output,,}" != *"skipped"* ]]
}

# ------------------------------ Boundary cases ------------------------------

@test "exactly at the cap: still runs the tests" {
    make_tests 20
    run bash -c "$(printf '%q' "$HOOK") <<< '$(hook_input)'"
    [ "$status" -eq 0 ]
    [[ "${output,,}" != *"skipped"* ]]
}

# -------------------------------- Error cases -------------------------------

@test "over the cap: says it skipped rather than exiting silently" {
    make_tests 21
    run bash -c "$(printf '%q' "$HOOK") <<< '$(hook_input)'"
    # Must not fail the edit — the cap is deliberate, the silence is not.
    [ "$status" -eq 0 ]
    [[ "${output,,}" == *"skipped"* ]]
}

@test "over the cap: names the count and how to run them" {
    make_tests 21
    run bash -c "$(printf '%q' "$HOOK") <<< '$(hook_input)'"
    [[ "$output" == *"21"* ]]
    [[ "$output" == *"make test-file"* ]]
}
