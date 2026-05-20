"""Tests for the Google Calendar API sync helper.

Pure rendering functions only — auth and fetch are thin wrappers
around the Google libraries and get smoke-tested manually after
OAuth setup.

Run: python3 -m unittest tests/test_calendar_sync_api.py
"""

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path

# Add scripts/ to path so we can import the helper as a module.
REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

import calendar_sync_api as api  # noqa: E402

FIXTURES = Path(__file__).parent / "fixtures" / "calendar-sync-api"


def load_fixture(name):
    return json.loads((FIXTURES / name).read_text())


# --- self_response_status ---

class TestSelfResponseStatus(unittest.TestCase):
    """Normal/Boundary/Error coverage for self_response_status."""

    def test_normal_finds_self_declined(self):
        ev = load_fixture("declined-recurring.json")
        self.assertEqual(api.self_response_status(ev), "declined")

    def test_normal_finds_self_accepted(self):
        ev = load_fixture("accepted-with-conference.json")
        self.assertEqual(api.self_response_status(ev), "accepted")

    def test_boundary_no_attendees_returns_none(self):
        ev = load_fixture("all-day-multi.json")
        self.assertIsNone(api.self_response_status(ev))

    def test_boundary_attendees_without_self_flag_returns_none(self):
        ev = {"attendees": [
            {"email": "other@example.com", "responseStatus": "accepted"},
        ]}
        self.assertIsNone(api.self_response_status(ev))

    def test_error_missing_attendees_key(self):
        self.assertIsNone(api.self_response_status({}))


# --- filter_declined ---

class TestFilterDeclined(unittest.TestCase):

    def setUp(self):
        self.events = [
            load_fixture("declined-recurring.json"),
            load_fixture("accepted-with-conference.json"),
            load_fixture("all-day-multi.json"),
        ]

    def test_normal_drops_self_declined(self):
        result = api.filter_declined(self.events, skip_declined=True)
        self.assertEqual(len(result), 2)
        for e in result:
            self.assertNotEqual(api.self_response_status(e), "declined")

    def test_normal_toggle_off_keeps_all(self):
        result = api.filter_declined(self.events, skip_declined=False)
        self.assertEqual(len(result), 3)

    def test_boundary_empty_list(self):
        self.assertEqual(api.filter_declined([], skip_declined=True), [])

    def test_boundary_none_declined(self):
        events = [load_fixture("accepted-with-conference.json")]
        self.assertEqual(len(api.filter_declined(events)), 1)


# --- format_org_timestamp ---

class TestFormatOrgTimestamp(unittest.TestCase):

    def test_normal_timed_same_day(self):
        start = {"dateTime": "2026-05-19T14:45:00-04:00"}
        end   = {"dateTime": "2026-05-19T15:45:00-04:00"}
        # Expected uses the offset's local time directly (no TZ conversion
        # needed when the dateTime already carries its offset).
        self.assertEqual(
            api.format_org_timestamp(start, end),
            "<2026-05-19 Tue 14:45-15:45>",
        )

    def test_normal_all_day_single(self):
        start = {"date": "2026-05-19"}
        end   = {"date": "2026-05-20"}
        self.assertEqual(
            api.format_org_timestamp(start, end),
            "<2026-05-19 Tue>",
        )

    def test_boundary_all_day_multi(self):
        start = {"date": "2026-05-17"}
        end   = {"date": "2026-05-22"}
        # End is exclusive in the Google API; last inclusive day is 5/21.
        self.assertEqual(
            api.format_org_timestamp(start, end),
            "<2026-05-17 Sun>--<2026-05-21 Thu>",
        )

    def test_boundary_timed_no_end(self):
        start = {"dateTime": "2026-05-19T14:45:00-04:00"}
        self.assertEqual(
            api.format_org_timestamp(start, None),
            "<2026-05-19 Tue 14:45>",
        )


# --- HTML / text cleaning ---

class TestStripHtml(unittest.TestCase):

    def test_normal_strips_tags(self):
        self.assertEqual(api.strip_html("Hello <b>world</b>"), "Hello world")

    def test_normal_converts_br_to_newline(self):
        self.assertEqual(api.strip_html("Line 1<br>Line 2"), "Line 1\nLine 2")
        self.assertEqual(api.strip_html("Line 1<br/>Line 2"), "Line 1\nLine 2")
        self.assertEqual(api.strip_html("Line 1<BR />Line 2"), "Line 1\nLine 2")

    def test_normal_decodes_entities(self):
        self.assertEqual(api.strip_html("AT&amp;T &quot;hi&quot;"), 'AT&T "hi"')

    def test_boundary_empty_string(self):
        self.assertEqual(api.strip_html(""), "")

    def test_error_none_input(self):
        self.assertIsNone(api.strip_html(None))


class TestSanitizeHeading(unittest.TestCase):

    def test_normal_collapses_whitespace(self):
        self.assertEqual(api.sanitize_heading("a   b\n\tc"), "a b c")

    def test_normal_neutralizes_leading_stars(self):
        # Leading "* " in heading text would create a nested heading.
        self.assertEqual(api.sanitize_heading("* foo"), "- foo")

    def test_boundary_empty(self):
        self.assertEqual(api.sanitize_heading(""), "")


# --- extract_meeting_url ---

class TestExtractMeetingUrl(unittest.TestCase):

    def test_normal_prefers_conference_data_video(self):
        ev = load_fixture("accepted-with-conference.json")
        self.assertEqual(
            api.extract_meeting_url(ev),
            "https://meet.google.com/abc-defg-hij",
        )

    def test_normal_falls_back_to_hangout_link(self):
        ev = load_fixture("declined-recurring.json")
        self.assertEqual(
            api.extract_meeting_url(ev),
            "https://meet.google.com/suw-ornf-iuv",
        )

    def test_boundary_no_meeting_url(self):
        ev = load_fixture("all-day-multi.json")
        self.assertIsNone(api.extract_meeting_url(ev))


# --- render_event ---

class TestRenderEvent(unittest.TestCase):

    def test_normal_renders_accepted_meeting(self):
        ev = load_fixture("accepted-with-conference.json")
        out = api.render_event(ev)
        # Heading + timestamp + property drawer + description body
        self.assertIn("* Breakout: Machine Speed, Human Will", out)
        self.assertIn("<2026-05-19 Tue 14:45-15:45>", out)
        self.assertIn(":LOCATION: Marriott Water Street: Florida Salon V - VI / Breakout Room 2", out)
        self.assertIn(":ORGANIZER: Eric Bell", out)
        self.assertIn(":STATUS: accepted", out)
        self.assertIn(":URL: https://meet.google.com/abc-defg-hij", out)
        self.assertIn("Bring your laptop", out)
        # HTML <br/> got normalized to newlines, not left as raw tags.
        self.assertNotIn("<br", out)

    def test_normal_renders_all_day_event(self):
        ev = load_fixture("all-day-multi.json")
        out = api.render_event(ev)
        self.assertIn("* SOFWeek — Tampa", out)
        self.assertIn("<2026-05-17 Sun>--<2026-05-21 Thu>", out)
        self.assertIn(":LOCATION: Tampa, FL", out)
        # No :STATUS: when there's no attendee block.
        self.assertNotIn(":STATUS:", out)

    def test_normal_renders_minimal_self_event(self):
        ev = load_fixture("all-day-single.json")
        out = api.render_event(ev)
        self.assertIn("* Home", out)
        self.assertIn("<2026-05-19 Tue>", out)
        # No property drawer at all when none of the optional fields are set.
        self.assertNotIn(":PROPERTIES:", out)

    def test_boundary_missing_summary_falls_back(self):
        ev = {"start": {"date": "2026-05-19"}, "end": {"date": "2026-05-20"}}
        out = api.render_event(ev)
        self.assertIn("* (No Title)", out)

    def test_error_missing_start_returns_none(self):
        ev = {"summary": "Broken"}
        self.assertIsNone(api.render_event(ev))


# --- render_calendar end-to-end with filter ---

class TestRenderCalendar(unittest.TestCase):

    def test_normal_filters_declined_and_renders_rest(self):
        events = [
            load_fixture("declined-recurring.json"),
            load_fixture("accepted-with-conference.json"),
            load_fixture("all-day-multi.json"),
        ]
        events = api.filter_declined(events, skip_declined=True)
        out = api.render_calendar(events)
        # Declined event must not appear.
        self.assertNotIn("STRATFI/FR Standup", out)
        # Accepted ones must appear.
        self.assertIn("Breakout: Machine Speed", out)
        self.assertIn("SOFWeek — Tampa", out)
        # Header is the same shape the Elisp parser produces.
        self.assertTrue(out.startswith("# Calendar Events\n"))


if __name__ == "__main__":
    unittest.main()
