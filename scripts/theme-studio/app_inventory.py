"""Theme-studio package/app face inventory assembly helpers."""

from __future__ import annotations

import json
import os
from collections.abc import Sequence
from typing import Any

from face_data import BESPOKE_APP_SPECS, PINNED_PACKAGE_FACES


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

# Custom display labels for inventory apps whose package name is an acronym
# worth spelling out (matches the bespoke EAT / LSP / SHR style: full name with
# the acronym in parentheses).
PACKAGE_LABEL_OVERRIDES = {
    "emms": "emacs multimedia system (emms)",
}


def face_label(face: str, prefix: str) -> str:
    label = face[len(prefix) :] if face.startswith(prefix) else face
    return label.replace("-face", "").replace("-", " ")


def face_rows(names: Sequence[str], prefix: str, seed: dict[str, dict[str, Any]]) -> list[list[Any]]:
    return [[face, face_label(face, prefix), seed.get(face, {})] for face in names]


def add_inventory_apps(apps: dict[str, Any], inventory_path: str) -> dict[str, Any]:
    """Add generic editable apps for installed packages not covered by bespoke previews.

    PINNED_PACKAGE_FACES (the ecosystem coverage policy) is the curated record
    of packages retired from this config: a pinned package is always marked
    not loaded, and it survives inventory regeneration -- an uninstall must
    never drop an app from the studio. Its face list stays fresh from the live
    inventory when present (a still-installed dependency can grow faces); the
    pin is the fallback when the inventory no longer carries it.
    """
    inventory: dict[str, Any] = {}
    if os.path.exists(inventory_path):
        with open(inventory_path) as src:
            inventory = json.load(src)
    for pkg in sorted(inventory):
        if pkg in BESPOKE_APPS or pkg in apps:
            continue
        apps[pkg] = {
            "label": PACKAGE_LABEL_OVERRIDES.get(pkg, pkg),
            "preview": PREVIEW_KEYS.get(pkg, "generic"),
            "faces": [[face, face_label(face, pkg + "-"), {}] for face in inventory[pkg]],
        }
    for pkg in sorted(PINNED_PACKAGE_FACES):
        if pkg in BESPOKE_APPS:
            continue
        faces = inventory.get(pkg, PINNED_PACKAGE_FACES[pkg])
        apps[pkg] = {
            "label": PACKAGE_LABEL_OVERRIDES.get(pkg, pkg) + " · not loaded",
            "preview": PREVIEW_KEYS.get(pkg, "generic"),
            "unloaded": True,
            "hover": ("Retired from this config; its faces are pinned so ecosystem "
                      "themes still cover it. The live preview is the only place its "
                      "theming can be seen."),
            "faces": [[face, face_label(face, pkg + "-"), {}] for face in faces],
        }
    return apps


def add_nerd_icons_app(apps: dict[str, Any], inventory_path: str, legend: Any,
                       gallery: Any = None) -> dict[str, Any]:
    """Register nerd-icons as a bespoke legend app from its inventory faces.

    The 34 nerd-icons color faces stay editable rows; LEGEND (the validated rows
    from generate.load_nerd_icons_legend) rides the app so the bespoke previews.js
    renderer can draw each filetype glyph in its mapped face color. GALLERY (the
    full colored catalog grouped by face, from generate.load_nerd_icons_gallery)
    rides alongside when present so the same renderer can draw the gallery section;
    a falsy GALLERY simply omits it (legend-only). A no-op when LEGEND is falsy or
    the inventory lacks nerd-icons -- the caller guards on a valid legend, and
    add_inventory_apps then creates the generic fallback app. Must run before
    add_inventory_apps so the generic path skips nerd-icons.
    """
    if not legend or not os.path.exists(inventory_path):
        return apps
    with open(inventory_path) as src:
        faces = json.load(src).get("nerd-icons")
    if not faces:
        return apps
    app = {
        "label": "nerd-icons",
        "preview": "nerdicons",
        "faces": [[face, face_label(face, "nerd-icons-"), {}] for face in faces],
        "legend": legend,
    }
    if gallery:
        app["gallery"] = gallery
    apps["nerd-icons"] = app
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
