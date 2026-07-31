#!/usr/bin/env bats
# Tests for scripts/agenda-render-cache — the batch writer behind the timer.
#
# The elisp tests cover the query and the row shape.  What only a shell test
# can cover is the thing that actually broke: the script runs a batch Emacs
# with -Q, so none of the editor's configuration is loaded, and anything it
# forgets to set up degrades silently rather than erroring.  The first version
# wrote a file that parsed fine and was wrong, because org did not know DOING
# was a keyword and left it glued to the front of every title.
#
# Two isolation rules, both learned the hard way:
#
#   EMACS_D points at THIS checkout, not $HOME/.emacs.d.  Without it the script
#   under test runs the installed config's elisp, so a broken tree passes.
#
#   AGENDA_RENDER_FILES points at a fixture written here.  Asserting over the
#   machine's real agenda makes the result depend on what Craig happens to have
#   scheduled today: the keyword assertion only bites if some entry carries a
#   keyword, and on an empty agenda every all() assertion passes over an empty
#   list.  The fixture makes the failure mode reachable every run.

setup() {
	SCRIPT="${BATS_TEST_DIRNAME}/../scripts/agenda-render-cache"
	export EMACS_D="${BATS_TEST_DIRNAME}/.."
	export XDG_CACHE_HOME="${BATS_TEST_TMPDIR}/cache"
	OUT="${XDG_CACHE_HOME}/settings/agenda.json"

	FIXTURE="${BATS_TEST_TMPDIR}/fixture.org"
	TODAY="$(date +%Y-%m-%d)"
	DOW="$(date +%a)"
	{
		printf '* DOING [#A] Advisor projects\nSCHEDULED: <%s %s 09:00>\n' "$TODAY" "$DOW"
		printf '* TODO Standup\nSCHEDULED: <%s %s 10:00-10:15>\n' "$TODAY" "$DOW"
		printf '* Lunch\n<%s %s 12:00-13:00>\n' "$TODAY" "$DOW"
		printf '* VERIFY [#B] Check the render\nDEADLINE: <%s %s 17:00>\n' "$TODAY" "$DOW"
	} > "$FIXTURE"
	export AGENDA_RENDER_FILES="$FIXTURE"
}

# Every assertion below runs through this, so an empty result can never pass
# vacuously — all() over an empty list is true, which is how a writer that
# produced nothing would look like a writer that produced correct rows.
assert_json() {
	python3 -c "
import json, sys
rows = json.load(open(sys.argv[1]))
assert isinstance(rows, list), 'not a JSON array'
assert len(rows) == 4, 'expected 4 fixture rows, got %d' % len(rows)
$1
" "$OUT"
}

@test "the script is executable" {
	[ -x "$SCRIPT" ]
}

@test "writes a parseable JSON array and creates its directory" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	[ -f "$OUT" ]
	run assert_json "pass"
	[ "$status" -eq 0 ]
}

@test "every row carries the three keys the renderer reads" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	run assert_json "
assert all({'s','e','t'} <= set(r) for r in rows), 'missing s/e/t'
assert all(isinstance(r['s'], int) and isinstance(r['e'], int) for r in rows), 'non-integer instant'
assert all(r['e'] >= r['s'] for r in rows), 'negative duration'
"
	[ "$status" -eq 0 ]
}

@test "instants are milliseconds, not seconds" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	# Seconds since 1970 are 10 digits; milliseconds are 13.
	run assert_json "assert all(len(str(r['s'])) == 13 for r in rows), 'looks like seconds'"
	[ "$status" -eq 0 ]
}

@test "titles do not carry the TODO keyword vocabulary" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	# The regression this file exists for.  The fixture carries DOING, TODO and
	# VERIFY entries plus two priority cookies, so a script that has not set
	# org-todo-keywords fails here every run rather than only on days when
	# Craig happens to have such an entry.
	run assert_json "
KEYWORDS = {'TODO','PROJECT','DOING','WAITING','VERIFY','STALLED',
            'DELEGATED','FAILED','DONE','CANCELLED'}
bad = [r['t'] for r in rows if r['t'].split(' ')[0] in KEYWORDS]
assert not bad, 'keyword left in title: %r' % bad
cookie = [r['t'] for r in rows if r['t'].startswith('[#')]
assert not cookie, 'priority cookie left in title: %r' % cookie
assert sorted(r['t'] for r in rows) == [
    'Advisor projects', 'Check the render', 'Lunch', 'Standup']
"
	[ "$status" -eq 0 ]
}

@test "keyword and completion state survive the batch environment" {
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	run assert_json "
by_title = {r['t']: r for r in rows}
assert by_title['Advisor projects']['keyword'] == 'DOING', by_title['Advisor projects']
assert by_title['Check the render']['keyword'] == 'VERIFY'
assert by_title['Lunch']['keyword'] is None
assert all(r['done'] is False for r in rows)
assert by_title['Check the render']['type'] == 'deadline'
"
	[ "$status" -eq 0 ]
}

@test "replaces an existing file rather than appending" {
	mkdir -p "$(dirname "$OUT")"
	printf 'stale garbage that is not json at all\n' > "$OUT"
	run "$SCRIPT"
	[ "$status" -eq 0 ]
	run assert_json "pass"
	[ "$status" -eq 0 ]
}

@test "fails loudly when the config directory is missing" {
	run env EMACS_D="${BATS_TEST_TMPDIR}/nonexistent" "$SCRIPT"
	[ "$status" -ne 0 ]
	[[ "$output" == *"no modules directory"* ]]
}
