"""Theme-studio package/app face inventory assembly helpers."""

from __future__ import annotations

import json
import os
from collections.abc import Sequence
from typing import Any

from face_data import BESPOKE_APP_SPECS


# Keys of the bespoke apps (single-sourced in face_data), excluded from the
# generic-inventory path so they aren't also emitted as plain inventory apps.
# "org" is an explicit alias of the "org-mode" bespoke app, so an inventory
# package literally named "org" never gets a duplicate generic entry.
BESPOKE_APPS = {spec[0] for spec in BESPOKE_APP_SPECS} | {"org"}


# Inventory apps (not in BESPOKE_APPS) default to the generic preview. A few have
# a dedicated PACKAGE_PREVIEWS renderer in app.js, keyed by name here.
PREVIEW_KEYS = {
    "markdown-mode": "markdown",
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
            "preview": PREVIEW_KEYS.get(pkg, "generic"),
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
