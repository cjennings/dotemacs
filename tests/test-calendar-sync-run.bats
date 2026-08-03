#!/usr/bin/env bats
# Tests for scripts/calendar-sync-run — the batch syncer behind the timer.
#
# The elisp tests cover the wait loop and the result tally with the state
# predicate stubbed.  What only a shell test can cover is the thing that makes
# the whole script necessary: the sync pipeline is asynchronous end to end
# (curl in one process, the org conversion in a second batch Emacs), and a
# batch Emacs exits as soon as its top-level form returns.  A version that
# launches the fetch and returns would exit zero, write nothing, and look
# exactly like a success.  Every assertion here that checks the output file
# exists is really asserting that the script waited.
#
# Isolation rules, mirroring test-agenda-render-cache.bats:
#
#   EMACS_D points at THIS checkout, so a broken tree cannot pass by running
#   the installed config's elisp.
#
#   CALENDAR_SYNC_CONFIG and CALENDAR_SYNC_STATE point at fixtures, so the run
#   neither reads Craig's real feed URLs nor writes his persisted sync state.
#
#   The feed is a file:// URL served to the script's own curl.  That keeps the
#   test hermetic -- no network, no live calendar -- while still exercising the
#   real fetch path rather than a stub.

setup() {
	SCRIPT="${BATS_TEST_DIRNAME}/../scripts/calendar-sync-run"
	export EMACS_D="${BATS_TEST_DIRNAME}/.."
	export CALENDAR_SYNC_STATE="${BATS_TEST_TMPDIR}/state.el"
	export CALENDAR_SYNC_TIMEOUT=120

	OUT="${BATS_TEST_TMPDIR}/testcal.org"
	ICS="${BATS_TEST_TMPDIR}/feed.ics"
	TODAY="$(date +%Y%m%d)"

	cat > "$ICS" <<-EOF
		BEGIN:VCALENDAR
		VERSION:2.0
		PRODID:-//bats//test//EN
		BEGIN:VEVENT
		UID:bats-fixture-1
		DTSTART:${TODAY}T140000Z
		DTEND:${TODAY}T150000Z
		SUMMARY:Batch Fixture Event
		END:VEVENT
		END:VCALENDAR
	EOF

	write_config "file://${ICS}"
}

# The calendar list is normally private config; the test writes its own so the
# feed URL is a local file and the output lands in the temp dir.
write_config() {
	export CALENDAR_SYNC_CONFIG="${BATS_TEST_TMPDIR}/config.el"
	cat > "$CALENDAR_SYNC_CONFIG" <<-EOF
		(setq calendar-sync-calendars
		      (list (list :name "testcal" :url "$1" :file "${OUT}")))
	EOF
}

@test "the script is executable" {
	[ -x "$SCRIPT" ]
}

@test "waits for the async pipeline and writes the org file" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	# The file existing at all is the assertion: it is written by a grandchild
	# process, so a script that did not wait would have exited before this.
	[ -f "$OUT" ]
	grep -q "Batch Fixture Event" "$OUT"
}

@test "reports the calendar and its status on stdout" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	[[ "$output" == *"testcal: ok"* ]]
}

@test "a failed fetch exits non-zero so systemd records it" {
	write_config "file://${BATS_TEST_TMPDIR}/does-not-exist.ics"
	run "$SCRIPT"
	[ "$status" -ne 0 ]
	[ ! -f "$OUT" ]
}

@test "a failed fetch names the calendar rather than failing silently" {
	write_config "file://${BATS_TEST_TMPDIR}/does-not-exist.ics"
	run "$SCRIPT"
	[[ "$output" == *"testcal"* ]]
	[[ "$output" != *"testcal: ok"* ]]
}

@test "a failed fetch prints why, not just that it failed" {
	# The interactive path logs the reason to *Messages*, which batch Emacs
	# discards at exit.  Without the reason on stdout the journal shows only
	# "error" -- no way to tell a cold gpg-agent from a revoked feed token.
	write_config "file://${BATS_TEST_TMPDIR}/does-not-exist.ics"
	run "$SCRIPT"
	[[ "$output" == *"testcal: error"* ]]
	[[ "$output" == *"Fetch failed"* ]]
}

@test "refuses to run against a checkout with no modules directory" {
	EMACS_D="${BATS_TEST_TMPDIR}/empty" run "$SCRIPT"
	[ "$status" -ne 0 ]
	[[ "$output" == *"no modules directory"* ]]
}

@test "does not write the real session's sync state" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	# The state override is honoured, so a timer run cannot corrupt or race
	# the interactive session's persisted state.
	[ -f "$CALENDAR_SYNC_STATE" ]
}
