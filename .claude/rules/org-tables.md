# Org Table Standard

Applies to: `**/*.org`

Every org table in project docs follows one shape. Wide tables overflow the
page in exported PDF/docx and run off the edge of the org buffer; this is the
standing fix. Promoted from the work project's local convention 2026-06-11.

## Three requirements

1. **Max width 120 columns — measured at render width.** The whole table
   line, leading/trailing pipes included, is ≤120 characters as the table
   *renders* (exported output, or the org buffer). An org link counts as its
   visible label, not its full `[[target][label]]` source, because export and
   the live buffer show only the label. This is the one place source width
   and render width diverge; **never split a link** to chase a source-width
   number — the render is what overflows the page. Non-link cells have no
   source/render gap.
2. **Multi-line cells.** When a cell's text would push the row past 120, wrap
   it onto continuation rows: repeat the row with the overflow column's text
   continued and the other columns left blank, as many continuation rows as
   the content needs. Never truncate content to hit the width; wrap it.
3. **A rule under the header and under every logical row.** Put a horizontal
   rule (`|---+---|`) after the header and after every data row, closing rule
   included. Each logical row then reads as a bordered block, and the rules
   are what mark where a logical row (with its continuation lines) ends.

Example — the logical row "arch-00" wrapped across two physical rows, rules
between every row:

    | Document | Doc Status | Notes                            |
    |----------+------------+----------------------------------|
    | arch-00  | Current    | Source-of-truth spec; references |
    |          |            | arch-NN as authority             |
    |----------+------------+----------------------------------|
    | arch-01  | Current    | Linear introduction for          |
    |          |            | first-time readers               |
    |----------+------------+----------------------------------|

## How to apply

When authoring or editing any table, produce this shape from the start. When
a table already violates it, reformat in place — preserve every cell's
content and any links verbatim, only change the layout.

Tooling (in every project's `.ai/scripts/` via the template sync):

- `wrap-org-table.el` reflows tables to the standard mechanically:
  `emacs --batch -q -l .ai/scripts/wrap-org-table.el [--width=120] FILE.org`.
  It wraps over-budget cells onto continuation rows, adds the rules, measures
  links at label width, and never splits a token or a link. Re-running on a
  conformant table is a no-op.
- `lint-org.el` flags violating tables as judgment items (checker
  `org-table-standard`) during its sweep — width overruns, missing rules, or
  both — and names the helper in the message.

The helper can't fix a table whose single narrowest-possible columns still
exceed the budget (some token or link label is just too wide). That table
needs restructuring — merge or drop columns, shorten labels — which is a
judgment call: the lint item stays until a human reshapes it.
