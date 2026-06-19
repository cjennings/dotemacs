"""Shared face-spec defaults for theme-studio generation."""

from __future__ import annotations

from typing import Any


# The full per-face attribute model, in its final shape, as one spec list that is
# the single source for: STYLE_DEFAULTS (model key -> default), the capture probe
# map in capture-default-faces.py (emacs :attr -> snapshot field), and the
# snapshot extraction in default_faces.seed (snapshot field + kind -> model value).
# Per row:
#   model    : theme-model key
#   default  : value when unset
#   capture  : the Emacs face :attribute keyword the capture probes (None = not
#              captured, e.g. family, which the model carries but the snapshot omits)
#   snapshot : the field name the capture writes (and seed reads)
#   kind     : how seed turns the snapshot field into a model value (None = not seeded)
# weight/slant replaced the legacy bold/italic booleans; underline/strike/overline
# are objects ({style: line|wave, color} / {color}); inherit and height apply to
# every tier. Keep this in step with app-core.js FACE_ATTRS (separate runtime).
FACE_ATTRS: list[dict[str, Any]] = [
    {"model": "fg",         "default": None,  "capture": ":foreground",         "snapshot": "foreground",         "kind": "color"},
    {"model": "bg",         "default": None,  "capture": ":background",         "snapshot": "background",         "kind": "color"},
    {"model": "distant-fg", "default": None,  "capture": ":distant-foreground", "snapshot": "distantForeground", "kind": "color"},
    {"model": "family",     "default": None,  "capture": None,                  "snapshot": None,                "kind": None},
    {"model": "weight",     "default": None,  "capture": ":weight",             "snapshot": "weight",            "kind": "weight-bold"},
    {"model": "slant",      "default": None,  "capture": ":slant",              "snapshot": "slant",             "kind": "slant-italic"},
    {"model": "underline",  "default": None,  "capture": ":underline",          "snapshot": "underline",         "kind": "underline-obj"},
    {"model": "strike",     "default": None,  "capture": ":strike-through",     "snapshot": "strike",            "kind": "color-obj"},
    {"model": "overline",   "default": None,  "capture": ":overline",           "snapshot": "overline",          "kind": "color-obj"},
    {"model": "box",        "default": None,  "capture": ":box",                "snapshot": "box",               "kind": "box"},
    {"model": "inverse",    "default": False, "capture": ":inverse-video",      "snapshot": "inverseVideo",      "kind": "bool"},
    {"model": "extend",     "default": False, "capture": ":extend",             "snapshot": "extend",            "kind": "bool"},
    {"model": "inherit",    "default": None,  "capture": ":inherit",            "snapshot": "inherit",           "kind": "scalar"},
    {"model": "height",     "default": None,  "capture": ":height",             "snapshot": "height",            "kind": "height"},
]

# model key -> default, derived from the spec above (order preserved).
STYLE_DEFAULTS: dict[str, Any] = {a["model"]: a["default"] for a in FACE_ATTRS}

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
