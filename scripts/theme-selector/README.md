# theme-selector

A self-contained tool for designing Emacs color themes by eye. It renders six
languages of tree-sitter-tokenized code, a category→color assignment table, a
UI-faces table, and an editable palette into one HTML page you drive in the
browser. Reassign colors, toggle weight/slant, edit the palette, then export a
`theme.json` that a build step turns into `themes/<name>-*.el`.

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

- `generate.py` — emits the HTML+JS. Edit here to change layout or behavior.
- `samples.py` — the language code samples and the default category→color map
  (`COLS`). `generate.py` reads the part before the `cols=` marker.
- `theme-selector.html` — generated output. Regenerate; don't hand-edit.

## What it captures

- Background and foreground (the `default` face's `:background` / `:foreground`).
- The syntax layer: every font-lock / tree-sitter category (keyword, string,
  function, type, comment, and the rest), each with normal/bold/italic.
- UI faces: cursor, region, mode-line, fringe, line numbers, isearch, paren
  match, link, error/warning/success, and more — foreground and background per
  face.
- The palette itself: add by hex or swatch, remove, rename, drag to reorder.

## theme.json contract

The export (and what a build step consumes):

```json
{
  "name": "dupre-revision",
  "palette": [["#67809c", "blue"], ["#e8bd30", "gold"]],
  "assignments": {"kw": "#67809c", "str": "#5d9b86", "bg": "#0d0b0a", "p": "#cdced1"},
  "bold": ["kw", "fnd"],
  "italic": [],
  "ui": {"region": {"fg": null, "bg": "#264364"}, "cursor": {"fg": null, "bg": "#a9b2bb"}}
}
```

The theme name is both the `name` field and the download filename
(`<name>.json`, sanitized). Upload a `theme.json` to start from a prior theme.

## Next step (not yet built)

A `theme.json` → `themes/<name>-palette.el` + `-faces.el` + `-theme.el`
converter. That step is the correctness-sensitive part and is the one worth
TDD: JSON in, valid Emacs palette + faces out, every face mapped, WCAG-contrast
asserted on the result.
