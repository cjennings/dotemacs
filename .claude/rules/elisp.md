# Elisp / Emacs Rules

Applies to: `**/*.el`

## Style

- 2-space indent, no tabs
- Hyphen-case for identifiers: `cj/do-thing`, not `cj/doThing`
- Naming prefixes:
  - `cj/name` — user-facing functions and commands (bound to keys, called from init)
  - `cj/--name` — private helpers (double-dash signals "internal")
  - `<module>/name` — module-scoped where appropriate (e.g., `calendar-sync/parse-ics`)
- File header: `;;; foo-config.el --- brief description -*- lexical-binding: t -*-`
- `(provide 'foo-config)` at the bottom of every module
- `lexical-binding: t` is mandatory — no file without it

## Function Design

- Keep functions under 15 lines where possible
- One responsibility per function
- Extract helpers instead of nesting deeply — 5+ levels of nesting is a refactor signal
- Prefer named helpers over lambdas for anything nontrivial
- No premature abstraction — three similar lines beats a clever macro

Small functions are the single strongest defense against paren errors. Deeply nested code is where AI and humans both fail.

## Requires and Loading

- Every `(require 'foo)` must correspond to a loadable file on the load-path
- Byte-compile warnings about free variables usually indicate a missing `require` or a typo in a symbol name — read them
- Use `use-package` for external (MELPA/ELPA) packages
- Use plain `(require 'foo-config)` for internal modules
- For optional features, `(when (require 'foo nil t) ...)` degrades gracefully if absent

## Lexical-Binding Traps

- `(boundp 'x)` where `x` is a lexical variable always returns nil. Bind with `defvar` at top level if you need `boundp` to work, or use the value directly.
- `setq` on an undeclared free variable is a warning — use `let` for locals or `defvar` for module-level state
- Closures capture by reference. Avoid capturing mutating loop variables in nested defuns.

## Regex Gotchas

- `\s` is NOT whitespace in Emacs regex. Use `[ \t]` or `\\s-` (syntax class).
- `^` in `string-match` matches after `\n` OR at position 0 — use `(= (match-beginning 0) start)` for positional checks when that matters.
- `replace-regexp-in-string` interprets backslashes in the replacement. Pass `t t` (FIXEDCASE LITERAL) when the replacement contains literal backslashes.

## Keybindings

- `keymap-global-set` for global; `keymap-set KEYMAP ...` for mode-local
- Group module-specific bindings inside the module's file
- Autoload cookies (`;;;###autoload`) don't activate through plain `(require ...)` — use the form directly, not an autoloaded wrapper

## Module Template

```elisp
;;; foo-config.el --- Foo feature configuration -*- lexical-binding: t -*-

;;; Commentary:
;; One-line description.

;;; Code:

;; ... code ...

(provide 'foo-config)
;;; foo-config.el ends here
```

Then `(require 'foo-config)` in `init.el` (or a config aggregator).

## Editing Workflow

- A PostToolUse hook runs `check-parens` and `byte-compile-file` on every `.el` save
- If it blocks, read the error — don't retry blindly
- Prefer Write over repeated Edits for nontrivial new code; incremental edits accumulate subtle paren mismatches
