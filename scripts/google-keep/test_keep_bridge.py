#!/usr/bin/env python3
"""Tests for keep-bridge's pure shaping helpers + its failure degradation.

The gkeepapi auth/fetch path is the IO boundary and is exercised live once the
token is configured; here we test the JSON-shaping logic (the round-trip
contract the elisp side reads) with duck-typed stand-ins, plus a subprocess
smoke test that the script degrades with a reason token rather than crashing.

Run: python3 -m unittest test_keep_bridge   (from scripts/google-keep/)
"""

import importlib.util
import os
import subprocess
import sys
import unittest
from datetime import datetime, timezone, timedelta

_HERE = os.path.dirname(os.path.abspath(__file__))
_BRIDGE = os.path.join(_HERE, "keep-bridge.py")

_spec = importlib.util.spec_from_file_location("keep_bridge", _BRIDGE)
assert _spec and _spec.loader
kb = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(kb)


# --- duck-typed stand-ins for a gkeepapi note ---------------------------------

class FakeLabel:
    def __init__(self, name):
        self.name = name


class FakeLabels:
    def __init__(self, names):
        self._labels = [FakeLabel(n) for n in names]

    def all(self):
        return self._labels


class FakeTimestamps:
    def __init__(self, updated):
        self.updated = updated


class FakeColor:
    def __init__(self, value):
        self.value = value


class FakeNote:
    def __init__(self, id="n1", title: object = "T", text: object = "B", labels=(),
                 pinned=False, archived=False, color: object = "WHITE", updated=None):
        self.id = id
        self.title = title
        self.text = text
        self.labels = FakeLabels(labels)
        self.pinned = pinned
        self.archived = archived
        self.color = color
        self.timestamps = FakeTimestamps(updated)


class TestIso8601Utc(unittest.TestCase):
    def test_normal_naive_datetime_treated_as_utc(self):
        self.assertEqual(kb.iso8601_utc(datetime(2026, 6, 25, 4, 12, 0)),
                         "2026-06-25T04:12:00Z")

    def test_normal_aware_datetime_converted_to_utc(self):
        est = timezone(timedelta(hours=-5))
        self.assertEqual(kb.iso8601_utc(datetime(2026, 6, 24, 23, 12, 0, tzinfo=est)),
                         "2026-06-25T04:12:00Z")

    def test_boundary_none_returns_none(self):
        self.assertIsNone(kb.iso8601_utc(None))


class TestColorName(unittest.TestCase):
    def test_normal_enum_with_value(self):
        self.assertEqual(kb.color_name(FakeColor("RED")), "RED")

    def test_normal_plain_string(self):
        self.assertEqual(kb.color_name("WHITE"), "WHITE")

    def test_boundary_name_only_object(self):
        class C:
            name = "BLUE"
        self.assertEqual(kb.color_name(C()), "BLUE")


class TestNoteToDict(unittest.TestCase):
    def test_normal_full_note(self):
        note = FakeNote(id="abc", title="Groceries", text="milk\neggs",
                        labels=("shopping", "home"), pinned=True, archived=False,
                        color=FakeColor("YELLOW"),
                        updated=datetime(2026, 6, 25, 4, 0, 0, tzinfo=timezone.utc))
        self.assertEqual(kb.note_to_dict(note), {
            "id": "abc",
            "title": "Groceries",
            "text": "milk\neggs",
            "labels": ["shopping", "home"],
            "pinned": True,
            "archived": False,
            "color": "YELLOW",
            "updated": "2026-06-25T04:00:00Z",
        })

    def test_boundary_empty_title_and_no_labels(self):
        note = FakeNote(title="", labels=(), color="WHITE",
                        updated=datetime(2026, 1, 1, tzinfo=timezone.utc))
        d = kb.note_to_dict(note)
        self.assertEqual(d["title"], "")
        self.assertEqual(d["labels"], [])

    def test_boundary_none_title_text_coerced_to_empty(self):
        note = FakeNote(title=None, text=None, color="WHITE",
                        updated=datetime(2026, 1, 1, tzinfo=timezone.utc))
        d = kb.note_to_dict(note)
        self.assertEqual(d["title"], "")
        self.assertEqual(d["text"], "")


class TestNotesToJson(unittest.TestCase):
    def test_normal_array_of_notes(self):
        import json
        notes = [FakeNote(id="a", updated=datetime(2026, 1, 1, tzinfo=timezone.utc)),
                 FakeNote(id="b", updated=datetime(2026, 1, 2, tzinfo=timezone.utc))]
        parsed = json.loads(kb.notes_to_json(notes))
        self.assertEqual([n["id"] for n in parsed], ["a", "b"])

    def test_boundary_empty_keep_is_empty_array(self):
        self.assertEqual(kb.notes_to_json([]), "[]")


class TestDegradation(unittest.TestCase):
    def test_error_no_env_exits_nonzero_with_reason_token(self):
        # With no KEEP_EMAIL/KEEP_MASTER_TOKEN the script must exit non-zero
        # with a single reason token, never crash. The exact token depends on
        # whether gkeepapi is installed in this environment.
        env = {k: v for k, v in os.environ.items()
               if k not in ("KEEP_EMAIL", "KEEP_MASTER_TOKEN")}
        proc = subprocess.run([sys.executable, _BRIDGE], env=env,
                              capture_output=True, text=True)
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn(proc.stderr.strip(), ("no-gkeepapi", "no-token"))
        self.assertEqual(proc.stdout, "")


if __name__ == "__main__":
    unittest.main()
