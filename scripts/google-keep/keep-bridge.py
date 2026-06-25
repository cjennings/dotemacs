#!/usr/bin/env python3
"""keep-bridge -- fetch Google Keep notes via gkeepapi and emit JSON.

The one place the unofficial Google Keep API lives, isolated so a break is
contained and the elisp renderer talks only to this script's JSON contract.
See docs/specs/google-keep-emacs-integration-spec.org (Bridge JSON schema).

Reads two environment variables (set by the elisp caller, which pulls the
token from authinfo.gpg via auth-source):

  KEEP_EMAIL          the Google account email
  KEEP_MASTER_TOKEN   the gkeepapi master token

On success: prints a JSON array of note objects on stdout, exits 0. An empty
Keep prints "[]". On failure: exits non-zero with one reason token on stderr,
which the elisp sentinel maps to a display-warning:

  no-gkeepapi   gkeepapi is not importable
  no-token      KEEP_MASTER_TOKEN or KEEP_EMAIL is unset
  auth-failed   gkeepapi rejected the credentials
  network       a network/other error reaching Keep
"""

import json
import os
import sys
from datetime import timezone
from typing import NoReturn


def iso8601_utc(dt):
    """Format DT (a datetime) as ISO8601 UTC with a trailing Z, or None."""
    if dt is None:
        return None
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def color_name(color):
    """Return the Keep color as a plain string from a gkeepapi enum or a string."""
    return getattr(color, "value", None) or getattr(color, "name", None) or str(color)


def note_to_dict(note):
    """Shape one gkeepapi note (or a duck-typed stand-in) into the schema dict."""
    return {
        "id": note.id,
        "title": note.title or "",
        "text": note.text or "",
        "labels": [label.name for label in note.labels.all()],
        "pinned": bool(note.pinned),
        "archived": bool(note.archived),
        "color": color_name(note.color),
        "updated": iso8601_utc(note.timestamps.updated),
    }


def notes_to_json(notes):
    """Serialize an iterable of NOTES to the schema JSON string."""
    return json.dumps([note_to_dict(n) for n in notes], ensure_ascii=False)


def _fail(token) -> NoReturn:
    sys.stderr.write(token + "\n")
    sys.exit(1)


def main():
    try:
        import gkeepapi  # type: ignore[import]  # optional runtime dep
    except ImportError:
        _fail("no-gkeepapi")
    email = os.environ.get("KEEP_EMAIL")
    token = os.environ.get("KEEP_MASTER_TOKEN")
    if not email or not token:
        _fail("no-token")
    keep = gkeepapi.Keep()
    try:
        keep.resume(email, token)
    except Exception as exc:  # gkeepapi raises LoginException on bad credentials
        _fail("auth-failed" if type(exc).__name__ == "LoginException" else "network")
    try:
        keep.sync()
        notes = list(keep.all())
    except Exception:
        _fail("network")
    sys.stdout.write(notes_to_json(notes))


if __name__ == "__main__":
    main()
