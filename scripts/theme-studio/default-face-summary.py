#!/usr/bin/env python3
"""Print a concise summary of theme-studio's captured Emacs default faces."""

from __future__ import annotations

import json
import pathlib
import sys

from default_faces import DefaultFaces, changed_summary


HERE = pathlib.Path(__file__).resolve().parent


def main() -> None:
    paths = [pathlib.Path(p) for p in sys.argv[1:]]
    if not paths:
        paths = [HERE / "emacs-default-faces.json"]
    summaries = [DefaultFaces.from_path(path).summary() for path in paths]
    if len(summaries) == 1:
        print(json.dumps(summaries[0], indent=2, sort_keys=True))
    elif len(summaries) == 2:
        print(json.dumps(changed_summary(summaries[0], summaries[1]), indent=2, sort_keys=True))
    else:
        raise SystemExit("usage: default-face-summary.py [snapshot.json [snapshot-after.json]]")


if __name__ == "__main__":
    main()
