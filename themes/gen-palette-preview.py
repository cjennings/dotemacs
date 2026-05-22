#!/usr/bin/env python3
"""Generate the Dupre palette preview HTML from dupre-palette.el.

Reads the (name "#hex") pairs straight from the palette source so the preview
can't drift from the theme. Groups colors into one row per family (by name
prefix, e.g. gray-2/gray/gray+1 share the "gray" row), with fixed-width
swatches so columns line up across rows.

Regenerate the PNG from the HTML afterwards:

  google-chrome-stable --headless=new --screenshot=themes/dupre-palette.png \\
    --window-size=975,1520 --hide-scrollbars --default-background-color=151311FF \\
    "file://$PWD/themes/dupre-color-palette.html"
"""
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
SRC = HERE / "dupre-palette.el"
OUT = HERE / "dupre-color-palette.html"

text = open(SRC).read()
# Only the dupre-palette defconst has (name "#hex") pairs; semantic mappings are
# (name colorname) with no hex, so this regex selects exactly the base palette.
pairs = re.findall(r'\(([a-z0-9]+(?:[+\-]\d+)?(?:-[a-z]+)?)\s+"(#[0-9a-fA-F]{6})"\)', text)

def family(name):
    m = re.match(r'^([a-z]+(?:-[a-z]+)??)(?:[+\-]\d+)?$', name)
    return m.group(1) if m else name

# Group consecutive same-family entries (palette.el already orders them so).
rows, cur_fam, cur = [], None, []
for name, hexv in pairs:
    fam = family(name)
    if fam != cur_fam:
        if cur:
            rows.append((cur_fam, cur))
        cur_fam, cur = fam, []
    cur.append((name, hexv))
if cur:
    rows.append((cur_fam, cur))

def swatch(name, hexv):
    return (f'      <div class="swatch">'
            f'<div class="box" style="background:{hexv}"></div>'
            f'<div class="name">{name}</div>'
            f'<div class="hex">{hexv}</div></div>')

body = []
for fam, items in rows:
    body.append('    <div class="family">')
    body.append(f'      <div class="fam">{fam}</div>')
    body.extend(swatch(n, h) for n, h in items)
    body.append('    </div>')

html = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Dupre Theme Color Palette</title>
  <style>
    body {{ font-family: 'Courier New', monospace; background: #151311; color: #f0fef0;
            padding: 28px; margin: 0; width: max-content; }}
    h1 {{ font-size: 20px; font-weight: bold; margin: 0 0 24px; }}
    .family {{ display: flex; align-items: flex-start; gap: 12px; margin-bottom: 14px; }}
    .fam {{ width: 96px; text-align: right; padding-top: 46px; color: #b4b1a2;
            font-size: 13px; flex-shrink: 0; }}
    .swatch {{ width: 120px; flex-shrink: 0; }}
    .box {{ width: 112px; height: 112px; border: 1px solid #474544; border-radius: 3px; }}
    .name {{ font-weight: bold; color: #f0fef0; font-size: 12px; margin-top: 5px; }}
    .hex {{ color: #969385; font-size: 11px; }}
  </style>
</head>
<body>
  <h1>Dupre Theme Color Palette</h1>
{chr(10).join(body)}
</body>
</html>
"""

open(OUT, "w").write(html)
print(f"wrote {OUT}: {len(pairs)} colors in {len(rows)} family rows")
print("families:", ", ".join(f for f, _ in rows))
