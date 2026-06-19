"""Shared face-spec defaults for theme-studio generation."""

from __future__ import annotations

from typing import Any


# The full per-face attribute model, in its final shape. weight and slant replace
# the legacy bold/italic booleans (weight is one of light/normal/medium/semibold/
# bold/heavy; slant is normal/italic/oblique). underline and strike are objects:
# underline is {style: line|wave, color} and strike is {color}; null means unset.
# inherit and height are no longer package-only — every tier can set them.
STYLE_DEFAULTS: dict[str, Any] = {
    "fg": None,
    "bg": None,
    "distant-fg": None,
    "family": None,
    "weight": None,
    "slant": None,
    "underline": None,
    "strike": None,
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


def migrate_legacy(spec: dict[str, Any]) -> dict[str, Any]:
    """Convert a face spec's legacy boolean style fields to the new shape.

    bold -> weight "bold", italic -> slant "italic", underline true ->
    {style: line, color: null}, strike true -> {color: null}. An explicit
    weight/slant already present wins over the legacy flag. Specs already in the
    new shape pass through unchanged, so this is safe to apply to any input. The
    JS side mirrors this in app-core.js migrateLegacyFace; keep them in step.
    """
    out = dict(spec)
    if "bold" in out:
        bold = out.pop("bold")
        if bold and not out.get("weight"):
            out["weight"] = "bold"
    if "italic" in out:
        italic = out.pop("italic")
        if italic and not out.get("slant"):
            out["slant"] = "italic"
    if "underline" in out:
        underline = out["underline"]
        if underline is True:
            out["underline"] = {"style": "line", "color": None}
        elif underline is False:
            out["underline"] = None
    if "strike" in out:
        strike = out["strike"]
        if strike is True:
            out["strike"] = {"color": None}
        elif strike is False:
            out["strike"] = None
    return out


def face_spec(spec: dict[str, Any] | None = None, *, package: bool = False) -> dict[str, Any]:
    out = dict(PACKAGE_DEFAULTS if package else STYLE_DEFAULTS)
    if spec:
        out.update(migrate_legacy(spec))
    return out


def ui_face_spec(spec: dict[str, Any] | None = None) -> dict[str, Any]:
    return face_spec(spec, package=False)


def package_face_spec(spec: dict[str, Any] | None = None) -> dict[str, Any]:
    return face_spec(spec, package=True)
