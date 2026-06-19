"""Shared face-spec defaults for theme-studio generation."""

from __future__ import annotations

from typing import Any


# The full per-face attribute model. inherit and height live here (every tier
# can set them now, not just packages). bold/italic/underline/strike stay as the
# legacy booleans for this phase; the weight/slant/underline-object cutover lands
# with the editor widgets that force it. distant-fg, family, overline, inverse,
# and extend are added in their final shape (no legacy form to migrate).
STYLE_DEFAULTS: dict[str, Any] = {
    "fg": None,
    "bg": None,
    "distant-fg": None,
    "family": None,
    "bold": False,
    "italic": False,
    "underline": False,
    "strike": False,
    "overline": None,
    "box": None,
    "inverse": False,
    "extend": False,
    "inherit": None,
    "height": None,
}

# Kept as a distinct name for callers, but inherit/height are no longer
# package-only, so the package defaults are now the same full set.
PACKAGE_DEFAULTS: dict[str, Any] = dict(STYLE_DEFAULTS)


def face_spec(spec: dict[str, Any] | None = None, *, package: bool = False) -> dict[str, Any]:
    out = dict(PACKAGE_DEFAULTS if package else STYLE_DEFAULTS)
    if spec:
        out.update(spec)
    return out


def ui_face_spec(spec: dict[str, Any] | None = None) -> dict[str, Any]:
    return face_spec(spec, package=False)


def package_face_spec(spec: dict[str, Any] | None = None) -> dict[str, Any]:
    return face_spec(spec, package=True)
