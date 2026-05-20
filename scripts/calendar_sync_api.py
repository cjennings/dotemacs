#!/usr/bin/env python3
"""Fetch Google Calendar events via the API and emit org-mode entries.

Usage:
    calendar_sync_api.py --account work --calendar-id primary --output FILE

First-run auth (per account):
    1. Create a Google Cloud OAuth client (Desktop application) and
       download client_secret.json to ~/.config/calendar-sync/.
    2. Run the script. A browser tab opens; grant the readonly
       calendar scope. The refresh token is written to
       ~/.config/calendar-sync/token-<account>.json.
    3. Subsequent runs refresh the access token without a browser.

This script exists because the secret-URL .ics export drops per-
occurrence response statuses for recurring events (Google's OOO
auto-decline writes only on the API side). The API path expands
recurrences server-side via singleEvents=True and returns one event
per occurrence, each carrying its own attendees[].self.responseStatus.
"""

from __future__ import annotations

import argparse
import datetime as dt
import html
import os
import re
import sys
from pathlib import Path

WEEKDAY_ABBREV = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

# --- Attendee status ---------------------------------------------------------


def self_response_status(event):
    """Return the authenticated user's responseStatus for EVENT, or None."""
    for attendee in event.get("attendees", []) or []:
        if attendee.get("self"):
            return attendee.get("responseStatus")
    return None


def filter_declined(events, skip_declined=True):
    """Return EVENTS minus self-declined entries when SKIP_DECLINED is true."""
    if not skip_declined:
        return list(events)
    return [e for e in events if self_response_status(e) != "declined"]


# --- Timestamp formatting ----------------------------------------------------


def _format_date(d):
    return f"{d.isoformat()} {WEEKDAY_ABBREV[d.weekday()]}"


def format_org_timestamp(start, end):
    """Render an org timestamp for a Google API start/end block pair.

    Timed same-day:   <YYYY-MM-DD Day HH:MM-HH:MM>
    Timed open-ended: <YYYY-MM-DD Day HH:MM>
    Timed cross-day:  <YYYY-MM-DD Day HH:MM>--<YYYY-MM-DD Day HH:MM>
    All-day single:   <YYYY-MM-DD Day>
    All-day multi:    <YYYY-MM-DD Day>--<YYYY-MM-DD Day>   (end is inclusive)
    """
    if "dateTime" in start:
        s = dt.datetime.fromisoformat(start["dateTime"])
        if end and "dateTime" in end:
            e = dt.datetime.fromisoformat(end["dateTime"])
            if s.date() == e.date():
                return (f"<{_format_date(s.date())} "
                        f"{s:%H:%M}-{e:%H:%M}>")
            return (f"<{_format_date(s.date())} {s:%H:%M}>--"
                    f"<{_format_date(e.date())} {e:%H:%M}>")
        return f"<{_format_date(s.date())} {s:%H:%M}>"

    # All-day: the Google API uses end.date as an exclusive upper bound.
    start_date = dt.date.fromisoformat(start["date"])
    if end and "date" in end:
        last_inclusive = dt.date.fromisoformat(end["date"]) - dt.timedelta(days=1)
        if start_date == last_inclusive:
            return f"<{_format_date(start_date)}>"
        return f"<{_format_date(start_date)}>--<{_format_date(last_inclusive)}>"
    return f"<{_format_date(start_date)}>"


# --- Text cleaning -----------------------------------------------------------

_HTML_TAG_RE = re.compile(r"<[^>]+>")
_WS_RE = re.compile(r"[ \t\r\n]+")
_LEADING_STARS_RE = re.compile(r"^(\*+) ", flags=re.MULTILINE)


def strip_html(text):
    """Remove HTML tags and decode entities. Returns None for None input."""
    if text is None:
        return None
    text = re.sub(r"<br\s*/?>", "\n", text, flags=re.IGNORECASE)
    text = re.sub(r"</p>", "\n", text, flags=re.IGNORECASE)
    text = _HTML_TAG_RE.sub("", text)
    return html.unescape(text)


def clean_text(text):
    """Strip HTML, collapse 3+ blank lines, trim."""
    if not text:
        return text
    cleaned = strip_html(text) or ""
    cleaned = re.sub(r"\n{3,}", "\n\n", cleaned)
    return cleaned.strip()


def sanitize_body_text(text):
    """Neutralize leading asterisks so external text isn't parsed as Org headings."""
    if not text:
        return text
    return _LEADING_STARS_RE.sub(lambda m: "-" * len(m.group(1)) + " ", text)


def sanitize_property_value(text):
    """Collapse whitespace + newlines to single spaces, trim."""
    if not text:
        return text
    return _WS_RE.sub(" ", text).strip()


def sanitize_heading(text):
    """Mirrors cj/org-sanitize-heading: neutralize stars, flatten whitespace."""
    return sanitize_property_value(sanitize_body_text(text))


# --- Event field extraction --------------------------------------------------


def extract_organizer(event):
    """Return organizer display name or email, or None."""
    org = event.get("organizer") or {}
    return org.get("displayName") or org.get("email")


def extract_meeting_url(event):
    """Prefer conferenceData video entryPoint, fall back to hangoutLink."""
    cd = event.get("conferenceData") or {}
    for ep in cd.get("entryPoints", []) or []:
        if ep.get("entryPointType") == "video":
            uri = ep.get("uri")
            if uri:
                return uri
    return event.get("hangoutLink")


# --- Rendering ---------------------------------------------------------------


def render_event(event):
    """Render one Google Calendar event as an org entry, or None to skip."""
    start = event.get("start")
    if not start:
        return None

    summary = event.get("summary") or "(No Title)"
    heading = sanitize_heading(summary) or "(No Title)"
    timestamp = format_org_timestamp(start, event.get("end"))

    props = []
    location = event.get("location")
    if location:
        props.append(f":LOCATION: {sanitize_property_value(location)}")
    organizer = extract_organizer(event)
    if organizer:
        props.append(f":ORGANIZER: {sanitize_property_value(organizer)}")
    status = self_response_status(event)
    if status:
        props.append(f":STATUS: {sanitize_property_value(status)}")
    url = extract_meeting_url(event)
    if url:
        props.append(f":URL: {sanitize_property_value(url)}")

    parts = [f"* {heading}", timestamp]
    if props:
        parts.append(":PROPERTIES:")
        parts.extend(props)
        parts.append(":END:")

    description = clean_text(event.get("description"))
    if description:
        parts.append(sanitize_body_text(description))

    return "\n".join(parts)


def render_calendar(events, header="# Calendar Events"):
    """Render every event in EVENTS as an org file body."""
    chunks = [header, ""]
    for event in events:
        org = render_event(event)
        if org:
            chunks.append(org)
            chunks.append("")
    return "\n".join(chunks) + "\n"


# --- OAuth + fetch -----------------------------------------------------------


def _config_dir():
    override = os.environ.get("CALENDAR_SYNC_CONFIG_DIR")
    if override:
        return Path(override)
    return Path.home() / ".config" / "calendar-sync"


SCOPES = ["https://www.googleapis.com/auth/calendar.readonly"]


def load_credentials(account):
    """Load OAuth credentials for ACCOUNT; refresh or run flow as needed."""
    try:
        from google.auth.transport.requests import Request
        from google.oauth2.credentials import Credentials
        from google_auth_oauthlib.flow import InstalledAppFlow
    except ImportError as e:
        raise SystemExit(
            f"ERROR: Missing Python dependency ({e.name}).\n"
            "Install with: sudo pacman -S python-google-api-python-client "
            "python-google-auth-oauthlib"
        )

    config_dir = _config_dir()
    config_dir.mkdir(parents=True, exist_ok=True)

    client_secret = config_dir / "client_secret.json"
    token_file = config_dir / f"token-{account}.json"
    creds = None
    if token_file.exists():
        creds = Credentials.from_authorized_user_file(str(token_file), SCOPES)
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            if not client_secret.exists():
                raise SystemExit(
                    f"ERROR: {client_secret} not found.\n"
                    "See docs/calendar-sync-api-setup.org for the OAuth setup."
                )
            flow = InstalledAppFlow.from_client_secrets_file(str(client_secret), SCOPES)
            creds = flow.run_local_server(port=0)
        token_file.write_text(creds.to_json())
        os.chmod(token_file, 0o600)
    return creds


def fetch_events(creds, calendar_id, past_months=3, future_months=12):
    """Fetch all events from CALENDAR_ID over the configured window."""
    try:
        from googleapiclient.discovery import build
    except ImportError as e:
        raise SystemExit(
            f"ERROR: Missing Python dependency ({e.name}).\n"
            "Install with: sudo pacman -S python-google-api-python-client"
        )

    service = build("calendar", "v3", credentials=creds, cache_discovery=False)
    now = dt.datetime.now(dt.timezone.utc)
    time_min = (now - dt.timedelta(days=30 * past_months)).isoformat()
    time_max = (now + dt.timedelta(days=30 * future_months)).isoformat()
    events = []
    page_token = None
    while True:
        resp = service.events().list(
            calendarId=calendar_id,
            timeMin=time_min,
            timeMax=time_max,
            singleEvents=True,
            orderBy="startTime",
            pageToken=page_token,
            maxResults=2500,
        ).execute()
        events.extend(resp.get("items", []))
        page_token = resp.get("nextPageToken")
        if not page_token:
            break
    return events


# --- CLI ---------------------------------------------------------------------


_CLI_DESCRIPTION = (
    "Fetch Google Calendar events via the API and emit org-mode entries."
)


def main(argv=None):
    parser = argparse.ArgumentParser(description=_CLI_DESCRIPTION)
    parser.add_argument("--account", required=True,
                        help="OAuth account nickname (work, personal, ...).")
    parser.add_argument("--calendar-id", required=True,
                        help="Calendar ID (e.g. 'primary' or a long ID).")
    parser.add_argument("--output", required=True, type=Path,
                        help="Path to write the rendered org file.")
    parser.add_argument("--past-months", type=int, default=3)
    parser.add_argument("--future-months", type=int, default=12)
    parser.add_argument("--keep-declined", action="store_true",
                        help="Skip the self-declined filter (default: filter on).")
    args = parser.parse_args(argv)

    creds = load_credentials(args.account)
    events = fetch_events(creds, args.calendar_id,
                          past_months=args.past_months,
                          future_months=args.future_months)
    events = filter_declined(events, skip_declined=not args.keep_declined)

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(render_calendar(events))
    sys.stderr.write(f"calendar-sync-api: wrote {len(events)} events to {args.output}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
