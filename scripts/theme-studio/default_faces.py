"""Helpers for theme-studio's captured Emacs default face snapshot."""

from __future__ import annotations

import json
import pathlib
from typing import Any


class DefaultFaces:
    def __init__(self, data: dict[str, Any] | None):
        self.data = data
        self.color_hex = self._build_color_hex()
        self.color_names = self._build_color_names()

    @classmethod
    def from_path(cls, path: str | pathlib.Path) -> "DefaultFaces":
        path = pathlib.Path(path)
        if not path.exists():
            return cls(None)
        return cls(json.loads(path.read_text()))

    @property
    def available(self) -> bool:
        return bool(self.data)

    def face(self, face: str, effective: bool = True) -> dict[str, Any]:
        if not self.data:
            return {}
        data = self.data.get("faces", {}).get(face, {})
        block = "effectiveGuiLight" if effective else "chosenGuiLight"
        return data.get(block, {}) or {}

    def color(self, face: str, attr: str = "foreground", effective: bool = True) -> Any:
        data = self.face(face, effective)
        return data.get(attr + "Hex") or data.get(attr)

    def seed(self, face: str, effective: bool = False) -> dict[str, Any]:
        data = self.face(face, effective)
        out: dict[str, Any] = {}
        fg = data.get("foregroundHex") or data.get("foreground")
        bg = data.get("backgroundHex") or data.get("background")
        if fg:
            out["fg"] = fg
        if bg:
            out["bg"] = bg
        # Representation-only cutover: the snapshot's bold/italic become the new
        # weight/slant shape, and underline/strike become objects. The same
        # narrowing as before (only "bold"/"italic" survive; richer weights and
        # underline colors wait for the snapshot refresh), so the emitted theme
        # is byte-identical.
        if data.get("weight") == "bold":
            out["weight"] = "bold"
        if data.get("slant") == "italic":
            out["slant"] = "italic"
        if data.get("underline"):
            out["underline"] = {"style": "line", "color": None}
        if data.get("strike"):
            out["strike"] = {"color": None}
        if data.get("inherit"):
            out["inherit"] = data.get("inherit")
        if data.get("height") and data.get("height") != 1:
            out["height"] = data.get("height")
        box = self.box_to_theme(data.get("box"))
        if box:
            out["box"] = box
        return out

    def box_to_theme(self, box: Any) -> dict[str, Any] | None:
        if not box:
            return None
        if isinstance(box, dict):
            return box
        if not isinstance(box, list):
            return None

        vals = {}
        i = 0
        while i + 1 < len(box):
            vals[box[i]] = box[i + 1]
            i += 2

        width = vals.get(":line-width", 1)
        if isinstance(width, list) and width and width[0] == "cons":
            width = width[1]
        if isinstance(width, (int, float)):
            width = abs(int(width)) or 1
        else:
            width = 1

        color = vals.get(":color")
        if color:
            color = self.color_hex.get(str(color).lower().replace(" ", ""), color)

        style = vals.get(":style")
        if style == "released-button":
            return {"style": "released", "width": width, "color": None}
        if style == "pressed-button":
            return {"style": "pressed", "width": width, "color": None}
        return {"style": "line", "width": width, "color": color}

    def label(self, value: str | None, fallback: str) -> str:
        if not value:
            return fallback
        return self.color_names.get(str(value).lower(), fallback)

    def summary(self) -> dict[str, Any]:
        if not self.data:
            return {}
        inventory = self.data.get("package-inventory", {})
        package_faces = sorted({face for faces in inventory.values() for face in faces})
        package_inherits = {
            face: self.seed(face).get("inherit")
            for face in package_faces
            if self.seed(face).get("inherit")
        }
        ui_faces = self.data.get("ui-faces", [])
        return {
            "emacsVersion": self.data.get("meta", {}).get("emacs-version"),
            "default": {
                "foreground": self.color("default", "foreground"),
                "background": self.color("default", "background"),
            },
            "faceCount": len(self.data.get("faces", {})),
            "packageFaceCount": len(package_faces),
            "packageUnresolvedFaceCount": self.data.get("meta", {}).get("package-unresolved-face-count", 0),
            "uiOwnSeeds": {face: self.seed(face) for face in ui_faces if self.seed(face)},
            "packageInherits": package_inherits,
        }

    def _build_color_hex(self) -> dict[str, str]:
        out: dict[str, str] = {}
        if not self.data:
            return out
        for data in self.data.get("faces", {}).values():
            for block in ("chosenGuiLight", "effectiveGuiLight"):
                face_data = data.get(block, {}) or {}
                for attr in ("foreground", "background", "distantForeground"):
                    name = face_data.get(attr)
                    hex_value = face_data.get(attr + "Hex")
                    if name and hex_value:
                        out[str(name).lower().replace(" ", "")] = hex_value
        return out

    def _build_color_names(self) -> dict[str, str]:
        out: dict[str, str] = {}
        if not self.data:
            return out
        for data in self.data.get("faces", {}).values():
            for block in ("chosenGuiLight", "effectiveGuiLight"):
                face_data = data.get(block, {}) or {}
                for attr in ("foreground", "background", "distantForeground"):
                    hex_value = face_data.get(attr + "Hex")
                    name = face_data.get(attr)
                    if hex_value and name and not str(name).startswith("#"):
                        out.setdefault(hex_value.lower(), str(name).lower().replace(" ", "-"))
        return out


def changed_summary(before: dict[str, Any], after: dict[str, Any]) -> dict[str, Any]:
    changed = {}
    for key in sorted(set(before) | set(after)):
        if before.get(key) != after.get(key):
            changed[key] = {"before": before.get(key), "after": after.get(key)}
    return changed
