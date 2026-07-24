#!/usr/bin/env bats
# Tests for githooks/pre-commit — the secret scan and paren check.
#
# The scan reads its input through a pipeline:
#
#   added_lines="$(git diff --cached ... | grep '^+' | grep -v '^+++' || true)"
#
# `grep` exits 1 when it matches nothing, which is the ordinary case, so the
# `|| true` has to stay.  But with no `pipefail` it also swallows a failure of
# `git diff` itself, and an empty `added_lines` makes the scan search nothing,
# find nothing, and report clean.  A gate that passes without looking is the
# failure this file exists to pin: the fail-open test drives a broken `git diff`
# and asserts the hook refuses rather than exiting 0.
#
# Each test builds a throwaway git repo in BATS_TEST_TMPDIR, so nothing touches
# the real repository or its hooks.

setup() {
    HOOK="${BATS_TEST_DIRNAME}/../githooks/pre-commit"
    REPO="${BATS_TEST_TMPDIR}/repo"
    mkdir -p "$REPO"
    cd "$REPO" || return 1
    git init -q .
    git config user.email t@example.com
    git config user.name  Test
    # Split so the fixtures never appear as credential-shaped literals here.
    AWS_TAIL="IOSFODNN7EXAMPLE"
    WORD_TAIL="word"
}

# Put a stub `git` ahead of the real one that fails for the staged-diff call
# and delegates everything else, so only the pipeline under test breaks.
break_staged_diff() {
    mkdir -p "${BATS_TEST_TMPDIR}/bin"
    cat > "${BATS_TEST_TMPDIR}/bin/git" <<'STUB'
#!/usr/bin/env bash
if [ "${1:-}" = "diff" ] && [ "${2:-}" = "--cached" ] && [ "${3:-}" = "-U0" ]; then
  echo "simulated git failure" >&2
  exit 128
fi
exec /usr/bin/git "$@"
STUB
    chmod +x "${BATS_TEST_TMPDIR}/bin/git"
    PATH="${BATS_TEST_TMPDIR}/bin:$PATH"
}

# ------------------------------- Normal cases -------------------------------

@test "secret scan: blocks a staged AWS key" {
    # Assembled at runtime: a literal key-shaped string in this file would trip
    # the very hook under test on every commit that touches it, and this repo
    # mirrors to a public remote.
    printf 'aws = "%s"\n' "AKIA${AWS_TAIL}" > creds.txt
    git add creds.txt
    run "$HOOK"
    [ "$status" -eq 1 ]
    [[ "$output" == *"potential secret"* ]]
}

@test "secret scan: blocks a staged keyword=value password" {
    printf '%s = "%s"\n' "pass${WORD_TAIL}" "correcthorsebatterystaple" > conf.txt
    git add conf.txt
    run "$HOOK"
    [ "$status" -eq 1 ]
    [[ "$output" == *"potential secret"* ]]
}

@test "secret scan: allows an ordinary staged file" {
    printf 'just some prose\n' > notes.txt
    git add notes.txt
    run "$HOOK"
    [ "$status" -eq 0 ]
}

# ------------------------------ Boundary cases ------------------------------

@test "secret scan: allows a commit with nothing staged" {
    run "$HOOK"
    [ "$status" -eq 0 ]
}

@test "paren check: blocks an unbalanced staged .el file" {
    printf '(defun broken ()\n  (message "no close"\n' > bad.el
    git add bad.el
    run "$HOOK"
    [ "$status" -eq 1 ]
    [[ "$output" == *"paren check failed"* ]]
}

@test "paren check: allows a balanced staged .el file" {
    printf '(defun fine ()\n  (message "ok"))\n' > good.el
    git add good.el
    run "$HOOK"
    [ "$status" -eq 0 ]
}

# -------------------------------- Error cases -------------------------------

@test "secret scan: refuses to pass when the staged diff cannot be read" {
    # The scan must not report clean after searching nothing.  Without a
    # pipefail-aware guard the broken diff yields an empty added_lines and the
    # hook exits 0, letting a real secret through unscanned.
    printf 'aws = "%s"\n' "AKIA${AWS_TAIL}" > creds.txt
    git add creds.txt
    break_staged_diff
    run "$HOOK"
    [ "$status" -ne 0 ]
}

@test "paren check: refuses to pass when the staged file list cannot be read" {
    printf '(defun broken ()\n  (message "no close"\n' > bad.el
    git add bad.el
    mkdir -p "${BATS_TEST_TMPDIR}/bin2"
    cat > "${BATS_TEST_TMPDIR}/bin2/git" <<'STUB'
#!/usr/bin/env bash
if [ "${1:-}" = "diff" ] && [ "${2:-}" = "--cached" ] && [ "${3:-}" = "--name-only" ]; then
  echo "simulated git failure" >&2
  exit 128
fi
exec /usr/bin/git "$@"
STUB
    chmod +x "${BATS_TEST_TMPDIR}/bin2/git"
    PATH="${BATS_TEST_TMPDIR}/bin2:$PATH"
    run "$HOOK"
    [ "$status" -ne 0 ]
}
