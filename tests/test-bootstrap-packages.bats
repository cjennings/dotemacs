#!/usr/bin/env bats
# Tests for scripts/bootstrap-packages.sh — the headless package installer.
#
# The elisp tests cover what happens inside one Emacs.  What only a shell test
# can cover is the pass loop: whether a run that reports packages still missing
# gets another pass, whether a run that converges stops early, and whether a
# broken init breaks out instead of burning every pass on the same failure.
#
# Every test drives a fake emacs whose exit statuses are scripted, so no test
# touches the network, the real elpa directory, or a real Emacs.  The script
# honours $EMACS, which is the seam these hang on.

setup() {
	SCRIPT="${BATS_TEST_DIRNAME}/../scripts/bootstrap-packages.sh"
	BIN="${BATS_TEST_TMPDIR}/bin"
	COUNTER="${BATS_TEST_TMPDIR}/attempts"
	mkdir -p "$BIN"
	echo 0 >"$COUNTER"
	export BOOTSTRAP_PASSES=3
	export BOOTSTRAP_TIMEOUT=30
	# Point the script at a scratch config dir rather than the real checkout, so
	# the byte-compiled-modules check reads fixture state instead of whatever
	# this working tree happens to have compiled.
	export BOOTSTRAP_DIR="${BATS_TEST_TMPDIR}/emacsd"
	mkdir -p "$BOOTSTRAP_DIR/modules"
}

# Write a fake emacs that exits with the given statuses in order, repeating the
# last one once the list runs out.  Status 1 also prints the "still missing"
# line the real cj/package-bootstrap-batch prints, so the script's grep is
# exercised rather than assumed.
fake_emacs() {
	{
		echo '#!/usr/bin/env bash'
		echo "n=\$(cat '$COUNTER')"
		echo "n=\$((n + 1))"
		echo "echo \$n >'$COUNTER'"
		echo "statuses=($*)"
		echo 'idx=$((n - 1))'
		echo 'last=$((${#statuses[@]} - 1))'
		echo '[ $idx -gt $last ] && idx=$last'
		echo 'status=${statuses[$idx]}'
		echo '[ "$status" -eq 1 ] && echo "package-bootstrap: 2 missing: foo bar"'
		echo 'exit $status'
	} >"$BIN/emacs"
	chmod +x "$BIN/emacs"
	export EMACS="$BIN/emacs"
}

attempts() { cat "$COUNTER"; }

@test "normal: a clean first pass succeeds and stops there" {
	fake_emacs 0
	run bash "$SCRIPT"
	[ "$status" -eq 0 ]
	[ "$(attempts)" -eq 1 ]
	[[ "$output" == *"every package is installed"* ]]
}

@test "normal: a pass reporting missing packages is retried until it converges" {
	fake_emacs 1 0
	run bash "$SCRIPT"
	[ "$status" -eq 0 ]
	[ "$(attempts)" -eq 2 ]
	[[ "$output" == *"2 missing: foo bar"* ]]
}

@test "error: packages that never install exhaust the passes and fail" {
	fake_emacs 1
	run bash "$SCRIPT"
	[ "$status" -eq 1 ]
	[ "$(attempts)" -eq 3 ]
	[[ "$output" == *"FAILED"* ]]
}

@test "error: exit 1 without a missing-packages line is not blamed on packages" {
	# The fake exits 1 silently, which is any other failure, not a short install.
	{
		echo '#!/usr/bin/env bash'
		echo "n=\$(cat '$COUNTER'); echo \$((n + 1)) >'$COUNTER'"
		echo 'exit 1'
	} >"$BIN/emacs"
	chmod +x "$BIN/emacs"
	export EMACS="$BIN/emacs"
	run bash "$SCRIPT"
	[ "$status" -eq 1 ]
	[ "$(attempts)" -eq 1 ]
	[[ "$output" == *"without reporting missing packages"* ]]
}

@test "error: a broken init breaks out instead of burning every pass" {
	fake_emacs 255
	run bash "$SCRIPT"
	[ "$status" -eq 255 ]
	[ "$(attempts)" -eq 1 ]
	[[ "$output" == *"failed to load init"* ]]
}

@test "boundary: a timed-out pass is reported and still retried" {
	fake_emacs 124 0
	run bash "$SCRIPT"
	[ "$status" -eq 0 ]
	[ "$(attempts)" -eq 2 ]
	[[ "$output" == *"timeout"* ]]
}

@test "boundary: the pass ceiling is honoured" {
	export BOOTSTRAP_PASSES=1
	fake_emacs 1
	run bash "$SCRIPT"
	[ "$status" -eq 1 ]
	[ "$(attempts)" -eq 1 ]
}

@test "error: a byte-compiled tree is refused rather than passed vacuously" {
	touch "$BOOTSTRAP_DIR/modules/foo.elc"
	fake_emacs 0
	run bash "$SCRIPT"
	[ "$status" -eq 2 ]
	[ "$(attempts)" -eq 0 ]
	[[ "$output" == *"REFUSING"* ]]
	[[ "$output" == *"clean-compiled"* ]]
	[[ "$output" != *"every package is installed"* ]]
}

@test "boundary: a zero pass ceiling fails cleanly without a tail error" {
	export BOOTSTRAP_PASSES=0
	fake_emacs 0
	run bash "$SCRIPT"
	[ "$status" -ne 0 ]
	[ "$(attempts)" -eq 0 ]
	[[ "$output" != *"cannot open"* ]]
	[[ "$output" == *"FAILED after 0 pass"* ]]
}

@test "boundary: recovery on the final allowed pass still succeeds" {
	export BOOTSTRAP_PASSES=3
	fake_emacs 1 1 0
	run bash "$SCRIPT"
	[ "$status" -eq 0 ]
	[ "$(attempts)" -eq 3 ]
}
