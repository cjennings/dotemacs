#!/usr/bin/env python3
"""Capture Emacs' default face attributes for theme-studio.

The output is a checked-in snapshot used to seed/review theme-studio defaults.
It runs `emacs -Q --batch`, loads the files that define each `defface`, stores
the raw `face-default-spec`, then selects the normal GUI/light/24-bit branch in
Python. This avoids opening a visible white `emacs -Q` frame while still keeping
the default data reproducible.
"""

from __future__ import annotations

import json
import os
import pathlib
import re
import subprocess
import tempfile

from face_specs import FACE_ATTRS

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[1]
OUT = HERE / "emacs-default-faces.json"
INVENTORY = HERE / "package-inventory.json"
PROGRESS = pathlib.Path("/tmp/theme-studio-default-face-capture-progress.txt")

SYNTAX = {
    "bg": ["default"],
    "p": ["default"],
    "kw": ["font-lock-keyword-face"],
    "bi": ["font-lock-builtin-face"],
    "pp": ["font-lock-preprocessor-face"],
    "fnd": ["font-lock-function-name-face"],
    "fnc": ["font-lock-function-call-face"],
    "dec": [],
    "ty": ["font-lock-type-face"],
    "prop": ["font-lock-property-name-face", "font-lock-property-use-face"],
    "con": ["font-lock-constant-face"],
    "num": ["font-lock-number-face"],
    "str": ["font-lock-string-face"],
    "esc": ["font-lock-escape-face"],
    "re": ["font-lock-regexp-face"],
    "doc": ["font-lock-doc-face"],
    "cm": ["font-lock-comment-face"],
    "cmd": ["font-lock-comment-delimiter-face"],
    "var": ["font-lock-variable-name-face", "font-lock-variable-use-face"],
    "op": ["font-lock-operator-face"],
    "punc": [
        "font-lock-punctuation-face",
        "font-lock-bracket-face",
        "font-lock-delimiter-face",
        "font-lock-misc-punctuation-face",
    ],
}

UI = [
    "cursor",
    "region",
    "hl-line",
    "highlight",
    "mode-line",
    "mode-line-inactive",
    "fringe",
    "line-number",
    "line-number-current-line",
    "minibuffer-prompt",
    "isearch",
    "lazy-highlight",
    "isearch-fail",
    "show-paren-match",
    "show-paren-mismatch",
    "link",
    "error",
    "warning",
    "success",
    "vertical-border",
]

BUILTIN_FEATURES = [
    "font-lock",
    "hl-line",
    "isearch",
    "paren",
    "button",
    "display-line-numbers",
    "shr",
]

# Emacs face :attribute keyword -> snapshot field name, derived from the shared
# face-attribute spec so the capture, the seed extraction, and STYLE_DEFAULTS all
# stay in step. Attributes the snapshot doesn't carry (e.g. family) have no
# capture keyword and are skipped.
ATTRS = {a["capture"]: a["snapshot"] for a in FACE_ATTRS if a["capture"]}


def x11_colors() -> dict[str, str]:
    colors: dict[str, str] = {}
    paths = [pathlib.Path("/usr/share/X11/rgb.txt")]
    paths.extend(pathlib.Path("/usr/share/emacs").glob("*/etc/rgb.txt"))
    path = next((p for p in paths if p.exists()), None)
    if path:
        for line in path.read_text(errors="ignore").splitlines():
            if not line or line.startswith("!"):
                continue
            parts = line.split()
            if len(parts) < 4:
                continue
            try:
                r, g, b = [int(x) for x in parts[:3]]
            except ValueError:
                continue
            name = " ".join(parts[3:]).lower().replace(" ", "")
            colors[name] = f"#{r:02x}{g:02x}{b:02x}"
    return colors


X11_COLORS = x11_colors()


def color_hex(value: object) -> str | None:
    if not isinstance(value, str):
        return None
    if re.fullmatch(r"#[0-9a-fA-F]{6}", value):
        return value.lower()
    key = value.lower().replace(" ", "")
    if key in X11_COLORS:
        return X11_COLORS[key]
    m = re.fullmatch(r"gr[ae]y(\d{1,3})", key)
    if m:
        n = max(0, min(100, int(m.group(1))))
        v = round(255 * n / 100)
        return f"#{v:02x}{v:02x}{v:02x}"
    return None


def plist_to_dict(items: object) -> dict[str, object]:
    if isinstance(items, list) and len(items) == 1 and isinstance(items[0], list):
        items = items[0]
    if not isinstance(items, list):
        return {}
    out: dict[str, object] = {}
    i = 0
    while i + 1 < len(items):
        key = items[i]
        val = items[i + 1]
        if isinstance(key, str) and key in ATTRS:
            out[ATTRS[key]] = normalize_value(val)
        elif key == ":bold" and val in (True, "t"):
            out["weight"] = "bold"
        elif key == ":italic" and val in (True, "t"):
            out["slant"] = "italic"
        i += 2
    return out


def normalize_value(value: object) -> object:
    if isinstance(value, list):
        as_plist = plist_to_dict(value)
        if as_plist:
            return as_plist
        return [normalize_value(v) for v in value]
    return value


def _condition_clauses_pass(clauses: dict) -> bool:
    """Apply the four display-condition rules to a {key: values} mapping.
    Returns False when any present clause excludes the GUI-light target."""
    if "class" in clauses:
        vals = clauses["class"]
        if "color" not in vals and "grayscale" not in vals:
            return False
    if "min-colors" in clauses:
        vals = clauses["min-colors"]
        if vals and isinstance(vals[0], int) and vals[0] > 16777216:
            return False
    if "background" in clauses:
        vals = clauses["background"]
        if vals and "light" not in vals:
            return False
    if "type" in clauses:
        if "tty" in clauses["type"]:
            return False
    return True


def condition_matches(condition: object) -> bool:
    if condition in (True, "t", None):
        return True
    if condition == "default":
        return False
    # Normalize the two display-spec shapes -- a {key: values} dict, or a list of
    # [key, *values] clauses -- to one {key: values} mapping, then run the four
    # rules once (see `_condition_clauses_pass').
    if isinstance(condition, dict):
        clauses = {k: (condition[k] or []) for k in condition}
        return _condition_clauses_pass(clauses)
    if not isinstance(condition, list):
        return False
    clauses = {}
    for clause in condition:
        if isinstance(clause, list) and clause:
            clauses[clause[0]] = clause[1:]
    return _condition_clauses_pass(clauses)


def choose_gui_light(default_spec: object) -> dict[str, object]:
    chosen: dict[str, object] = {}
    if not isinstance(default_spec, list):
        return chosen
    for entry in default_spec:
        if not isinstance(entry, list) or not entry:
            continue
        condition = entry[0]
        attrs = plist_to_dict(entry[1:])
        if condition == "default":
            chosen.update(attrs)
        elif condition_matches(condition):
            chosen.update(attrs)
            break
    for key in ("foreground", "background", "distantForeground"):
        if key in chosen:
            hx = color_hex(chosen[key])
            if hx:
                chosen[key + "Hex"] = hx
    return chosen


def inherit_list(value: object) -> list[str]:
    if value in (None, False):
        return []
    if isinstance(value, str):
        return [value]
    if isinstance(value, list):
        return [v for v in value if isinstance(v, str)]
    return []


def enrich_chosen_defaults(data: dict[str, object]) -> None:
    faces: dict[str, dict[str, object]] = data["faces"]
    for face, info in faces.items():
        info["chosenGuiLight"] = choose_gui_light(info.get("default-spec"))

    fallback = {
        "foreground": "black",
        "foregroundHex": "#000000",
        "background": "white",
        "backgroundHex": "#ffffff",
        "weight": "normal",
        "slant": "normal",
        "underline": None,
        "strike": None,
        "box": None,
        "height": 1,
    }

    def effective(face: str, seen: set[str] | None = None) -> dict[str, object]:
        seen = seen or set()
        if face in seen:
            return dict(fallback)
        seen.add(face)
        info = faces.get(face, {})
        own = dict(info.get("chosenGuiLight") or {})
        result = dict(fallback)
        inherits = inherit_list(own.get("inherit"))
        for parent in inherits:
            result.update({k: v for k, v in effective(parent, seen).items() if v is not None})
        result.update({k: v for k, v in own.items() if v is not None})
        for key in ("foreground", "background", "distantForeground"):
            if key in result:
                hx = color_hex(result[key])
                if hx:
                    result[key + "Hex"] = hx
                else:
                    result.pop(key + "Hex", None)
        if inherits:
            result["selectedInherits"] = inherits
        return result

    for face, info in faces.items():
        info["effectiveGuiLight"] = effective(face)


def package_dirs(pkg: str) -> list[pathlib.Path]:
    roots = [ROOT / "elpa", ROOT / "straight" / "build", ROOT / "site-lisp"]
    out: list[pathlib.Path] = []
    for root in roots:
        if not root.exists():
            continue
        out.extend(
            p
            for p in root.iterdir()
            if p.is_dir() and (p.name == pkg or p.name.startswith(pkg + "-"))
        )
    return out


def defface_files(inventory: dict[str, list[str]]) -> tuple[dict[str, dict[str, str]], dict[str, list[str]], list[str]]:
    found: dict[str, dict[str, str]] = {}
    missing: dict[str, list[str]] = {}
    load_paths: list[str] = []
    for pkg, faces in inventory.items():
        dirs = package_dirs(pkg)
        load_paths.extend(str(d) for d in dirs)
        by_face: dict[str, str] = {}
        pending = set(faces)
        for directory in dirs:
            for path in directory.rglob("*.el"):
                if not pending:
                    break
                try:
                    text = path.read_text(errors="ignore")
                except OSError:
                    continue
                for face in list(pending):
                    pat = r"\(\s*defface\s+" + re.escape(face) + r"\b"
                    if re.search(pat, text):
                        by_face[face] = str(path)
                        pending.remove(face)
            if not pending:
                break
        found[pkg] = by_face
        if pending:
            missing[pkg] = sorted(pending)
    return found, missing, sorted(set(load_paths))


def elisp_quote(value: object) -> str:
    return json.dumps(value)


def main() -> None:
    inventory = json.loads(INVENTORY.read_text())
    face_files, missing, load_paths = defface_files(inventory)
    package_faces = sorted({face for faces in inventory.values() for face in faces})
    package_files = sorted(set(face_files[pkg][face] for pkg in face_files for face in face_files[pkg]))
    all_faces = sorted(set(UI) | {f for faces in SYNTAX.values() for f in faces} | set(package_faces))

    script = f"""
(require 'json)
(require 'cl-lib)
(setq json-object-type 'alist)
(setq json-array-type 'list)
(setq json-key-type 'symbol)
(defconst ts-probe-load-paths (json-read-from-string {elisp_quote(json.dumps(load_paths))}))
(defconst ts-probe-builtin-features (json-read-from-string {elisp_quote(json.dumps(BUILTIN_FEATURES))}))
(defconst ts-probe-package-files (json-read-from-string {elisp_quote(json.dumps(package_files))}))
(defconst ts-probe-syntax-map (json-read-from-string {elisp_quote(json.dumps(SYNTAX))}))
(defconst ts-probe-ui-faces (json-read-from-string {elisp_quote(json.dumps(UI))}))
(defconst ts-probe-package-inventory (json-read-from-string {elisp_quote(json.dumps(inventory))}))
(defconst ts-probe-package-defface-files (json-read-from-string {elisp_quote(json.dumps(face_files))}))
(defconst ts-probe-package-unresolved-faces (json-read-from-string {elisp_quote(json.dumps(missing))}))
(defconst ts-probe-all-faces (json-read-from-string {elisp_quote(json.dumps(all_faces))}))
(dolist (dir ts-probe-load-paths)
  (add-to-list 'load-path dir))
(dolist (feature (mapcar #'intern ts-probe-builtin-features))
  (ignore-errors (require feature)))
(defun ts-probe--eval-deffaces (file)
  "Evaluate only the `defface' forms in FILE.

A defface form is self-contained, so registering a face this way avoids
loading the whole package (and its dependencies / side effects), which in
batch -Q is fragile: a missing dependency or mid-load error would silently
drop every face in the file. Each form is evaluated independently.

Catch any error from `read', not just `end-of-file': a reader-level error
(unrecognized syntax) otherwise propagates out and aborts the whole pass,
dropping every face in every later file. Stop reading this file instead."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form) (eq (car form) 'defface))
                (ignore-errors (eval form t)))))
        (error nil)))))
;; Pass 1: best-effort full load. Registers faces that are defined by a macro
;; or loop rather than a literal defface (e.g. rainbow-delimiters depth faces,
;; markdown header faces), which pass 2 cannot see. Failures are swallowed.
(dolist (file ts-probe-package-files)
  (with-temp-file {elisp_quote(str(PROGRESS))} (insert file))
  (ignore-errors (load file nil t)))
;; Pass 2: evaluate literal defface forms directly. Robustly registers faces
;; whose package failed to fully load in pass 1 (e.g. transient needing
;; cond-let, magit's transient/forge stack) and resets literal faces to their
;; pristine defface default spec. Runs last so the default spec wins over any
;; customization a pass-1 load may have applied.
(dolist (file ts-probe-package-files)
  (ts-probe--eval-deffaces file))
(defun ts-probe--proper-list-p (value)
  (or (null value)
      (and (consp value) (ts-probe--proper-list-p (cdr value)))))
(defun ts-probe--safe (value)
  (cond ((keywordp value) (symbol-name value))
        ((symbolp value) (symbol-name value))
        ((and (consp value) (ts-probe--proper-list-p value))
         (vconcat (mapcar #'ts-probe--safe value)))
        ((consp value)
         (vector "cons" (ts-probe--safe (car value)) (ts-probe--safe (cdr value))))
        ((vectorp value) (mapcar #'ts-probe--safe (append value nil)))
        (t value)))
(defun ts-probe--json-bool (v) (if v t :json-false))
(defun ts-probe--attr (face attr)
  (let ((v (face-attribute face attr nil 'default)))
    (cond ((eq v 'unspecified) nil)
          ((eq v 'unspecified-fg) nil)
          ((eq v 'unspecified-bg) nil)
          (t v))))
(defun ts-probe--face (name)
  (let ((face (intern name)))
    (if (not (facep face))
        `((exists . :json-false))
      `((exists . t)
        (foreground . ,(ts-probe--safe (ts-probe--attr face :foreground)))
        (background . ,(ts-probe--safe (ts-probe--attr face :background)))
        (weight . ,(ts-probe--safe (ts-probe--attr face :weight)))
        (slant . ,(ts-probe--safe (ts-probe--attr face :slant)))
        (underline . ,(ts-probe--safe (ts-probe--attr face :underline)))
        (strike . ,(ts-probe--safe (ts-probe--attr face :strike-through)))
        (overline . ,(ts-probe--safe (ts-probe--attr face :overline)))
        (box . ,(ts-probe--safe (ts-probe--attr face :box)))
        (height . ,(ts-probe--safe (ts-probe--attr face :height)))
        (inherit . ,(ts-probe--safe (ts-probe--attr face :inherit)))
        (default-spec . ,(ts-probe--safe (face-default-spec face)))))))
(let ((json-encoding-pretty-print t))
  (with-temp-file {elisp_quote(str(OUT))}
    (insert
     (json-encode
      `((meta . ((captured-by . "scripts/theme-studio/capture-default-faces.py")
                 (emacs-version . ,emacs-version)
                 (resolution-model . "gui-light-24bit-from-face-default-spec")
                 (window-system . "batch")
                 (display-color-cells . 16777216)
                 (default-foreground . "black")
                 (default-background . "white")
                 (package-face-count . ,{len(package_faces)})
                 (loaded-defface-file-count . ,{len(package_files)})))
        (syntax-map . ,ts-probe-syntax-map)
        (ui-faces . ,ts-probe-ui-faces)
        (package-inventory . ,ts-probe-package-inventory)
        (package-defface-files . ,ts-probe-package-defface-files)
        (package-unresolved-faces . ,ts-probe-package-unresolved-faces)
        (faces . ,(mapcar (lambda (face) (cons (intern face) (ts-probe--face face)))
                           ts-probe-all-faces)))))))
(kill-emacs)
"""
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as f:
        f.write(script)
        probe = f.name
    try:
        subprocess.run(["emacs", "-Q", "--batch", "-l", probe], cwd=ROOT, check=True, timeout=240)
    finally:
        try:
            os.unlink(probe)
        except OSError:
            pass

    data = json.loads(OUT.read_text())
    enrich_chosen_defaults(data)
    data["meta"]["package-unresolved-face-count"] = sum(len(v) for v in missing.values())
    OUT.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")
    print(f"wrote {OUT}")
    print(json.dumps(data["meta"], indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
