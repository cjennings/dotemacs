"""Shared face-spec defaults for theme-studio generation."""

from __future__ import annotations

from typing import Any


STYLE_DEFAULTS: dict[str, Any] = {
    "fg": None,
    "bg": None,
    "bold": False,
    "italic": False,
    "underline": False,
    "strike": False,
}

PACKAGE_DEFAULTS: dict[str, Any] = {
    **STYLE_DEFAULTS,
    "inherit": None,
    "height": 1,
    "box": None,
}


def face_spec(spec: dict[str, Any] | None = None, *, package: bool = False) -> dict[str, Any]:
    out = dict(PACKAGE_DEFAULTS if package else STYLE_DEFAULTS)
    if spec:
        out.update(spec)
    return out


def ui_face_spec(spec: dict[str, Any] | None = None) -> dict[str, Any]:
    return face_spec(spec, package=False)


def package_face_spec(spec: dict[str, Any] | None = None) -> dict[str, Any]:
    return face_spec(spec, package=True)
