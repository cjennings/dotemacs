# theme-studio

A self-contained tool for designing Emacs color themes by eye. One generated
HTML page drives the whole theme: a palette, the syntax (font-lock /
tree-sitter) layer, the built-in UI faces with a live mock-frame preview, and
package-specific faces (org, magit, elfeed, plus every other installed package).
Reassign colors against the palette, judge legibility with live WCAG-contrast
readouts, then export a `theme.json` that a build step turns into
`themes/<name>-*.el`.

For the color-assignment philosophy behind the tool — how to group syntax roles,
what to share, where to spend chroma and bold — see
[`theme-coloring-guide.org`](theme-coloring-guide.org).

## Run

```bash
python3 generate.py          # writes theme-studio.html beside this script
```

Then open it in Chrome (Firefox had color-rendering flakiness during design):

```bash
WAYLAND_DISPLAY=wayland-1 google-chrome-stable theme-studio.html
```

During color work, disable Hyprland inactive-window dimming so colors read true:

```bash
hyprctl keyword decoration:dim_inactive false
```

## Tests

```bash
make theme-studio-test       # from the repo root, runs the whole pyramid
scripts/theme-studio/run-tests.sh   # or call the runner directly
```

The runner regenerates the page, runs the Python templating tests
(`test_generate.py`), the Node unit tests for `colormath.js`
(`test-colormath.mjs`, including the inline-integrity check), a syntax check of
the spliced page script, and the browser hash gates in headless Chrome
(`#selftest`, `#cursortest`, `#readouttest`, `#deltatest`, `#oklchtest`,
`#planetest`, `#locktest`, `#sorttest`, `#mocktest`, `#contrasttest`,
`#safetest`, `#healtest`, `#columntest`, `#counttest`, `#baseedittest`,
`#roundtriptest`, `#beveltest`, `#previewlinktest`). It exits non-zero on any failure. The browser gates need a
Chromium-family browser; without one they report SKIPPED rather than passing
silently. The pure color math and the extracted picker logic (`planeCell`,
`paletteWarnings`) live in `colormath.js` so they are unit-tested directly in
Node; palette-column plans and lock-set plans live in `app-core.js` so edge
cases are unit-tested directly. The DOM glue is covered by the browser hash
gates.

## Files

- `generate.py` — assembles the generated page from the source JS/CSS, data, and
  template.
- `theme-studio.template.html` — static page shell with placeholders for the
  inlined CSS/JS/data. Edit here for layout markup.
- `face_data.py` — bespoke package face lists and seed defaults.
- `palette-actions.js` — stateful palette-panel actions and rendering, inlined
  into the generated page.
- `browser-gates.js` — the browser hash-gate test harness, also inlined.
- `app_inventory.py`, `face_specs.py`, `default_faces.py` — generator helpers for
  package inventory, face-spec defaults, and captured Emacs defaults.
- `samples.py` — the language code samples and the default syntax
  category→color map (`COLS`). `generate.py` reads the part before the `cols=`
  marker.
- `package-inventory.json` — generated map of every installed package to the
  faces it defines (see Package faces below). A committed data artifact.
- `build-inventory.el` — refreshes `package-inventory.json` from a running
  Emacs.
- `theme-studio.html` — generated output. Regenerate; don't hand-edit.
  Use `make check-generated` before review if you want to verify the committed
  page matches the generator without leaving the tree dirty.

## What it captures

Three tiers of faces, plus the palette:

- **Palette** — named colors, shown grouped into stable structural columns. Add
  by hex or with the in-page color picker
  (saturation/value square, hue slider, palette reuse chips, live contrast
  readout, and an any / AA+ / AAA legibility mask). Remove and rename per chip;
  the colors serving as background and foreground are locked. `clear palette`
  removes every non-ground color and leaves only the `bg` and `fg` tiles; existing
  face assignments remain on their old hexes and show as "(gone)" until a color
  with the same name is recreated.

  The picker also shows perceptual readouts beside the WCAG ratio: the OKLCH
  coordinates (lightness, chroma, hue°) and the APCA Lc contrast against the
  ground color. APCA Lc is signed — positive means dark text on a light
  background, negative means light text on a dark background — so a light color
  on dupre's dark ground reads as a negative Lc. WCAG stays the rating used in
  the syntax/UI/package tables; APCA and OKLCH are picker-only diagnostics.

  An edit-model toggle switches the picker between HSV and OKLCH, independent of
  the contrast mask. In OKLCH mode the L/C/H dials drive the color and the square
  becomes a Chroma×Lightness plane at the current hue, with the out-of-gamut
  region greyed out; the hue strip selects the hue. Pushing chroma past sRGB
  snaps to the reachable color and shows a clamp note. The palette also warns
  when two colors fall below a perceptual ΔE threshold, hard to tell apart.
- **Syntax** — every font-lock / tree-sitter category (keyword, string,
  function, type, comment, and the rest), each with normal/bold/italic and a
  contrast rating. Click a category to flash its tokens in the code; click a
  token to flash its row. `lock all` flips to `unlock all` when every row in the
  tier is locked; `clear unlocked` leaves locked rows untouched.
- **UI faces** — cursor, region, mode-line, fringe, line numbers, isearch, paren
  match, link, error/warning/success, and the rest, foreground and background
  per face, shown in a live mock Emacs buffer. The same lock-all and clear
  unlocked controls apply to the UI face tier.
- **Package faces** — per-package face tables with a live preview (below).

## Color columns

The palette is displayed as **columns**. The ground column is pinned first: `bg`
at one end, `fg` at the other, with optional `ground+N` span colors between them.
Every other color stays in the column where it was created. Columns are not
derived from hue, chroma, lightness, or the visible color name.

- **Grouping.** Each palette entry carries a stable column id. New colors start
  their own column; generated ramp steps inherit the base color's column id.
  Renaming a color only changes its label, so a renamed tile stays in its original
  column. Older two-field palette entries still load by falling back to the
  generated-name stem (`blue-1`, `blue`, `blue+1` -> `blue`).
  Generic Emacs names like `color-22` stay separate base columns unless they
  already carry an explicit column id. Numbered named colors such as `blue1`,
  `grey80`, `orange3`, and `orchid4` group by their text stem. Imported names
  that begin with `bg` or `fg` are normal colors unless they are exact ground
  endpoints or explicitly use the `ground` column id.
- **Deleting.** Normal columns have a separated header delete control with a
  confirmation prompt. Confirming removes every tile in that column. The ground
  column is pinned and cannot be deleted. Face assignments that used a deleted
  tile stay on that old hex and appear as recoverable "(gone)" values, matching
  individual chip deletion.
- **Tile clicks.** Single-clicking a tile, including its name, selects that
  whole color. Double-clicking the name enters name-edit mode with the cursor at
  the start of the name.
- **The count control** under each non-ground column sets how many steps sit on
  each side of the column's base. Setting N regenerates the column as a symmetric
  base ±N tonal ramp via `ramp()` — lighter and darker steps on the base's hue
  with chroma easing toward the extremes — *replacing* the column's current
  colors. N=0 collapses to the base alone.
- **Editing a base** recolors the whole column: change a base color and the column
  regenerates from it at the same count.
- **References follow.** When a regenerate changes a step's hex, any face assigned
  to that step is re-pointed to the new hex. A step *removed* by lowering the count
  leaves its references showing "(gone)" — visible and recoverable, never a silent
  jump to a different color.
- **Dropdown order.** Color dropdowns show the default entry, then `bg` and `fg`,
  then palette columns from left to right. Within each column's dropdown group,
  colors are ordered lightest to darkest.

The standalone ramp generator is gone; fanning a color into a ramp is now "add the
color, then raise its column's count."

## Background-contrast safety

Keep background tints readable. Works in OKLCH; the pure math is in `app-core.js`
(`fgSetFor`, `floor`, `lMax`), the DOM in `app.js`.

**Worst-case contrast.** A background overlay sits behind many foregrounds at
once, so one fg/bg contrast pair is the wrong number. For the covered overlay
faces — `region`, `hl-line`, `highlight`, `lazy-highlight`, `isearch` — the
contrast cell shows the *worst-case floor*: the lowest contrast over the face's
foreground set (the syntax-token colors plus the default foreground), naming the
*limiting foreground* that sets it. A tint that clears the default text but fails
the darkest token reads FAIL, with that token named. Package faces and the other
UI rows keep their single-pair readout.

The verdict is WCAG: AA (4.5) by default, AAA (7) selectable. APCA Lc stays a
picker-only diagnostic and does not drive PASS/FAIL.

**Safe lightness.** In the OKLCH picker, the "safe for" selector picks one
covered face. The Chroma×Lightness plane then shades the lightness band too light
to keep that face readable over its foreground set, with the L_max ceiling as the
band's lower edge. If even pure black can't satisfy the target (a foreground is
too dark), the whole plane shades; that is a true finding about the foreground,
not a tool bug.

## Package faces

Pick an application from the dropdown to edit its faces. Each row has a
foreground and background dropdown, bold/italic toggles, an `inherit` dropdown
(base faces like `fixed-pitch`/`link` plus the app's own faces), a relative
height stepper, a contrast readout, and a per-face reset. There's a per-app
reset and a text filter for the large sets. Package `lock all` / `unlock all`
applies to the whole currently selected package, not only the rows visible under
the text filter.

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
emacsclient -e '(load "/home/cjennings/.emacs.d/scripts/theme-studio/build-inventory.el")'
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
  "palette": [["#67809c", "blue", "blue"], ["#e8bd30", "gold", "gold"]],
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
- `palette` is a flat list of `[hex, name, columnId]`. `name` is the editable
  display label; `columnId` is the durable grouping key that keeps generated
  colors in their original column even if they are renamed. Older `[hex, name]`
  entries still import and are normalized on export.
- `ui` and `packages` faces carry `fg`/`bg` (hex or `null`), `bold`, `italic`,
  `underline`, `strike`, and for package faces `inherit` (a face name or
  `null`), `height` (a float, omitted at 1.0), and `source` (`"default"` seeded,
  `"user"` edited, `"cleared"`). The converter writes `underline` as
  `:underline t` and `strike` as `:strike-through t`.
- The theme name is both the `name` field and the download filename. Import a
  `theme.json` to start from a prior theme; a file with no `packages` key still
  loads.

`export` downloads the current theme JSON using the theme name as the filename.

## Build step — `build-theme.el`

`build-theme.el` converts a `theme.json` into a single self-contained
`themes/<name>-theme.el` deftheme. JSON in, valid Emacs faces out, across all
four tiers: `default` from `assignments.bg`/`.p`, the syntax categories mapped
to their font-lock / tree-sitter faces (with the `bold`/`italic` sets applied),
the UI faces passed through, and the package faces with `:inherit`/`:height`
and weight/slant written.

```bash
emacs --batch -l scripts/theme-studio/build-theme.el \
  --eval '(build-theme/convert-file "scripts/theme-studio/dupre-revised.json" "themes")'
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
