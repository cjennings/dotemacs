# Keybinding Display Format

Applies to: `**/*`

How to present a keymap's bindings when the user asks to see them — "show the keybindings", "list the bindings", "what's bound under X", or any request to display a prefix keymap and its structure.

## The Format

A bulleted list grouped by **category**, where each category is one level of the keymap's prefix tree.

- **One header per category.** Format: `<Package> <Category> — <full prefix>:`. The package name is the keymap's owner (e.g. `Pearl`); the category is the human name for that sub-map (`Save`, `Edit`, `Add`, `Delete`); the full prefix is the complete chord that lands on that sub-map.
- **The top level is always the `General` category.** Its prefix is the base prefix itself. General lists the terminal commands bound directly off the base prefix **and** the sub-prefix keys that lead into the other categories — so the reader sees every door off the top level in one place.
- **Each bullet is three fields:** `<full keybinding> — <command> — "<which-key label>"`.
  - *Full keybinding* — the complete chord, base prefix included (`C-; L s s`), not just the leaf key. The reader should be able to type it verbatim.
  - *Command* — the bound command symbol. For a sub-prefix entry in the General category, mark it as a prefix rather than a command (e.g. *(Save prefix)*).
  - *Which-key label* — the short string that shows in the which-key popup, in quotes (`"save ticket"`). For a sub-prefix, use the which-key prefix label (`"+save"`).
- **General comes first**, then one section per sub-category in a sensible order.

## Plain text in chat

Render this in chat as plain text — no Markdown bold and no inline-code spans. Headers, dashes, parentheses, and double-quoted labels carry the structure without them. See the "No Reverse-Video Highlighting in Chat Output" rule in [`interaction.md`](interaction.md): bold and backtick spans invert to reverse video on the user's terminal. The example below is shown the way it should appear in chat.

## Example

For an imaginary command set Pearl on base prefix C-; L:

Pearl General — C-; L:
- C-; L s — (save prefix) — "+save"
- C-; L e — (edit prefix) — "+edit"
- C-; L m — pearl-menu — "menu"

Pearl Save — C-; L s:
- C-; L s s — pearl-save-issue — "save ticket"
- C-; L s a — pearl-save-all — "save all"

Pearl Edit — C-; L e:
- C-; L e p — pearl-set-priority — "priority"
- C-; L e s — pearl-set-state — "state"

## Why

The header carries the full prefix so the category's depth is unambiguous — the reader knows exactly how many keys deep each section sits. Listing the sub-prefixes inside General makes the top level a complete map of where every door leads, rather than scattering that across the sections. The three-field bullet ties the chord a user types to the command it runs and the label they'll actually see in which-key, so the written view matches the on-screen view.
