# theme-selector

A self-contained tool for designing Emacs color themes by eye. One generated
HTML page drives the whole theme: a palette, the syntax (font-lock /
tree-sitter) layer, the built-in UI faces with a live mock-frame preview, and
package-specific faces (org, magit, elfeed, plus every other installed package).
Reassign colors against the palette, judge legibility with live WCAG-contrast
readouts, then export a `theme.json` that a build step turns into
`themes/<name>-*.el`.

## Run

```bash
python3 generate.py          # writes theme-selector.html beside this script
```

Then open it in Chrome (Firefox had color-rendering flakiness during design):

```bash
WAYLAND_DISPLAY=wayland-1 google-chrome-stable theme-selector.html
```

During color work, disable Hyprland inactive-window dimming so colors read true:

```bash
hyprctl keyword decoration:dim_inactive false
```

## Files

- `generate.py` — emits the HTML+JS, and embeds the package data. Edit here to
  change layout or behavior.
- `samples.py` — the six language code samples and the default syntax
  category→color map (`COLS`). `generate.py` reads the part before the `cols=`
  marker.
- `package-inventory.json` — generated map of every installed package to the
  faces it defines (see Package faces below). A committed data artifact.
- `build-inventory.el` — refreshes `package-inventory.json` from a running
  Emacs.
- `theme-selector.html` — generated output. Regenerate; don't hand-edit.

## What it captures

Three tiers of faces, plus the palette:

- **Palette** — named colors. Add by hex or with the in-page color picker
  (saturation/value square, hue slider, palette reuse chips, live contrast
  readout, and an any / AA+ / AAA legibility mask). Remove, rename, reorder with
  arrows or drag. The colors serving as background and foreground are locked.
- **Syntax** — every font-lock / tree-sitter category (keyword, string,
  function, type, comment, and the rest), each with normal/bold/italic and a
  contrast rating. Click a category to flash its tokens in the code; click a
  token to flash its row.
- **UI faces** — cursor, region, mode-line, fringe, line numbers, isearch, paren
  match, link, error/warning/success, and the rest, foreground and background
  per face, shown in a live mock Emacs buffer.
- **Package faces** — per-package face tables with a live preview (below).

## Package faces

Pick an application from the dropdown to edit its faces. Each row has a
foreground and background dropdown, bold/italic toggles, an `inherit` dropdown
(base faces like `fixed-pitch`/`link` plus the app's own faces), a relative
height stepper, a contrast readout, and a per-face reset. There's a per-app
reset and a text filter for the large sets.

Twenty applications have bespoke previews that exercise nearly all of their
faces: org-mode (a document plus an agenda view), magit (a status buffer plus
blame, reflog, sequence, bisect, and signature rows), elfeed (a search list and
log), ghostel (a mock terminal with the 16 ANSI colors), mu4e (a headers list,
message view, and compose stub), dashboard, lsp-mode (signatures, inlay hints,
symbol highlights, rename), git-gutter, flycheck (a diagnostic line plus an
error-list buffer), dired, dirvish (attribute columns, vc states, media, proc,
narrow), calibredb (a library listing and detail view), erc (an IRC channel),
org-drill (a cloze flashcard), org-noter, signel (a Signal chat), pearl (a
ticket), slack (a channel with mrkdwn, attachments, blocks, and dialogs), and
telega (chat entities, reactions, buttons, and webpage rendering), and shr (the
built-in HTML renderer behind nov, eww, elfeed's article view, and HTML mail, so
theming it themes all of them). Every other installed package is reachable too, with an editable
table and a generic preview (each face name in its own colors), so any package
can be themed. Clicking a face row flashes that face in the preview, and clicking
a preview element flashes its row.

**Inheritance** is modeled, not flattened: a face's effective color is resolved
through its `inherit` chain and shown in the table and preview; setting an
explicit color overrides it. `height` is a float multiplier off the base font
and is read directly off the face (not cascaded through `inherit`, since Emacs
multiplies float heights along a chain). The base monospace family is *not* the
theme's job — it lives in `modules/font-config.el`; the tool owns only relative
size and the `fixed-pitch` inherit relationships.

### Refreshing the package inventory

The reachable packages come from `package-inventory.json`. Regenerate it from a
running Emacs (so it reflects what's actually installed), then rebuild the HTML:

```bash
emacsclient -e '(load "/home/cjennings/.emacs.d/scripts/theme-selector/build-inventory.el")'
python3 generate.py
```

`build-inventory.el` groups each face by the package whose file defines it; it
depends on the target Emacs having those packages loaded. Built-in faces are
skipped (they're covered by the syntax and UI tiers).

## theme.json contract

The export (and what a build step consumes):

```json
{
  "name": "dupre",
  "palette": [["#67809c", "blue"], ["#e8bd30", "gold"]],
  "assignments": {"kw": "#67809c", "str": "#5d9b86", "bg": "#000000", "p": "#ffffff"},
  "bold": ["kw", "fnd"],
  "italic": [],
  "ui": {"region": {"fg": null, "bg": "#264364"}, "cursor": {"fg": null, "bg": "#a9b2bb"}},
  "packages": {
    "org-mode": {
      "org-level-1": {"fg": "#67809c", "bg": null, "bold": true, "italic": false,
                      "underline": false, "strike": false,
                      "inherit": null, "height": 1.3, "source": "default"}
    }
  }
}
```

- `assignments` maps syntax category keys to hexes; `bg` is the `default` face
  background, `p` the foreground.
- `ui` and `packages` faces carry `fg`/`bg` (hex or `null`), `bold`, `italic`,
  `underline`, `strike`, and for package faces `inherit` (a face name or
  `null`), `height` (a float, omitted at 1.0), and `source` (`"default"` seeded,
  `"user"` edited, `"cleared"`). The converter writes `underline` as
  `:underline t` and `strike` as `:strike-through t`.
- The theme name is both the `name` field and the download filename. Import a
  `theme.json` to start from a prior theme; a file with no `packages` key still
  loads.

`export` always downloads a fresh file; `save` (shown once a name is entered)
writes the same file in place via the File System Access API.

## Build step — `build-theme.el`

`build-theme.el` converts a `theme.json` into a single self-contained
`themes/<name>-theme.el` deftheme. JSON in, valid Emacs faces out, across all
four tiers: `default` from `assignments.bg`/`.p`, the syntax categories mapped
to their font-lock / tree-sitter faces (with the `bold`/`italic` sets applied),
the UI faces passed through, and the package faces with `:inherit`/`:height`
and weight/slant written.

```bash
emacs --batch -l scripts/theme-selector/build-theme.el \
  --eval '(build-theme/convert-file "scripts/theme-selector/dupre-revised.json" "themes")'
```

Output is a flat generated deftheme, not the palette/faces/theme trio the
original dupre ships — a `theme.json` carries resolved per-face hex, not dupre's
semantic-mapping layer, so a flat deftheme is the faithful output and never
clobbers the curated dupre files.

One mapping limitation: the `dec` (decorator) syntax key has no independent
Emacs face. Emacs renders decorators with `font-lock-type-face`, which the `ty`
key already owns, so `dec` is omitted from the output and decorators follow the
type color (as they do in stock Emacs). Tests live in
`tests/test-build-theme.el` (Normal / Boundary / Error, plus a WCAG-contrast
assertion on the round-tripped result).
