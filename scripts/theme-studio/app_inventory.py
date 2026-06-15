"""Theme-studio package/app face inventory assembly helpers."""

from __future__ import annotations

import json
import os
from collections.abc import Sequence
from typing import Any


BESPOKE_APPS = {
    "magit",
    "elfeed",
    "org",
    "org-mode",
    "mu4e",
    "ghostel",
    "auto-dim-other-buffers",
    "dashboard",
    "lsp-mode",
    "git-gutter",
    "flycheck",
    "dired",
    "dirvish",
    "calibredb",
    "erc",
    "org-drill",
    "org-noter",
    "signel",
    "pearl",
    "slack",
    "telega",
    "shr",
}


def face_label(face: str, prefix: str) -> str:
    label = face[len(prefix) :] if face.startswith(prefix) else face
    return label.replace("-face", "").replace("-", " ")


def face_rows(names: Sequence[str], prefix: str, seed: dict[str, dict[str, Any]]) -> list[list[Any]]:
    return [[face, face_label(face, prefix), seed.get(face, {})] for face in names]


def add_inventory_apps(apps: dict[str, Any], inventory_path: str) -> dict[str, Any]:
    """Add generic editable apps for installed packages not covered by bespoke previews."""
    if not os.path.exists(inventory_path):
        return apps
    with open(inventory_path) as src:
        inventory = json.load(src)
    for pkg in sorted(inventory):
        if pkg in BESPOKE_APPS or pkg in apps:
            continue
        apps[pkg] = {
            "label": pkg,
            "preview": "generic",
            "faces": [[face, face_label(face, pkg + "-"), {}] for face in inventory[pkg]],
        }
    return apps


def apply_default_face_seeds(apps: dict[str, Any], defaults: Any) -> None:
    if not defaults.available:
        return
    for app in apps.values():
        for row in app["faces"]:
            row[2] = defaults.seed(row[0], False)


def apply_package_overrides(apps: dict[str, Any], packages: dict[str, Any] | None) -> None:
    if not packages:
        return
    for app, package_faces in packages.items():
        if app not in apps:
            continue
        for row in apps[app]["faces"]:
            if row[0] in package_faces:
                row[2] = package_faces[row[0]]
