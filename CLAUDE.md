# Emacs Configuration — CLAUDE.md

## Project

Craig's personal Emacs configuration. Pure Elisp, organized as modules loaded from `init.el`. Single-user, used daily for real work.

**Layout:**
- `init.el`, `early-init.el` — startup
- `modules/*.el` — feature modules (one domain per file, e.g. `browser-config.el`, `calendar-sync.el`)
- `tests/test-*.el` — ERT unit tests (one or many per module)
- `tests/testutil-*.el` — shared test fixtures and mocks
- `assets/` — data files checked into git
- `data/` — runtime state (mostly gitignored)
- `.ai/` — Claude tooling (gitignored); session state and workflows. See `.ai/protocols.org` and `.ai/notes.org` first.
- `docs/` — real project documentation (if/when created); not touched by Claude sync

## Build & Test Commands

```bash
make                                    # Show all targets
make test                               # Run all tests
make test-file FILE=test-foo.el         # One file
make test-name TEST=pattern             # Match test names
make coverage                           # Generate .coverage/simplecov.json
make validate-parens                    # Balanced parens in modules
make validate-modules                   # Load all modules to verify they compile
make compile                            # Byte-compile (writes .elc)
make lint                               # checkdoc + package-lint + elisp-lint
make profile                            # Startup profiling
make clean                              # Remove .elc and test artifacts
```

## Language Rules

See rule files in `.claude/rules/`:
- `elisp.md` — code style and patterns
- `elisp-testing.md` — ERT conventions
- `verification.md` — verify-before-claim-done discipline

## Git Workflow

- Single-user repo, commits go to `main`
- Conventional prefixes: `feat:`, `fix:`, `refactor:`, `test:`, `docs:`, `chore:`
- Commit conventions and attribution rules: see `.claude/rules/commits.md`
- Pre-commit hook scans for secrets and runs `make validate-parens`

## Problem-Solving Approach

Investigate before fixing. When diagnosing a bug:
1. Read the relevant module and trace what actually happens
2. Identify the root cause, not a surface symptom
3. Write a failing test that captures the correct behavior
4. Fix, then re-run tests

This project has a history of finding real bugs (most recent: lexical-binding + `boundp` trap in reconcile-open-repos) only by tracing to root. Don't skip that step.

## Testing Discipline

TDD is the default: write a failing test before any implementation. If you can't write the test, you don't yet understand the change. Details in `.claude/rules/elisp-testing.md`.

## Editing Discipline

A PostToolUse hook runs `check-parens` + `byte-compile-file` on every `.el` file after Edit/Write/MultiEdit. Byte-compile warnings (free variables, wrong argument counts) are signal — read them.

Prefer Write over cumulative Edits for nontrivial new code. Small functions (under 15 lines) are near-impossible to get wrong; deeply nested code is where paren errors hide.

## What Not to Do

- Don't add features beyond what was asked
- Don't refactor surrounding code when fixing a bug
- Don't add comments to code you didn't change
- Don't create abstractions for one-time operations
- Don't commit `.env` files, credentials, or API keys — pre-commit hook catches common patterns but isn't a substitute for care

## Codified Insights

- **Run a full Emacs launch after any `use-package` `:config` block edit.** Two regressions this session (dashboard `void-function nerd-icons-faicon` from a `:defer t` change in `nerd-icons-config.el`; flycheck `Command executable... must be a string` from `(eval ...)` in a `:command` form) both passed unit tests and `make validate-modules` but failed at full Emacs launch. Drop the "non-trivial changes" threshold in `notes.org` to "any `:config` block edit" — `:config` runs at load time and is exactly the place where module-load errors hide. Smoke command: `emacs --batch --eval "(load (expand-file-name \"init.el\" user-emacs-directory) nil t)"`. (`verify` — 2026-05-16)

- **`gptel-model` must be a symbol, not a string.** gptel's modeline-display code calls `symbolp` on `gptel-model` and signals `wrong-type-argument symbolp "..."` otherwise — manifests as Emacs hanging in the AI-Assistant buffer with "Querying ..." → process-sentinel quit → redisplay-error loop. The Anthropic backend tolerated string values (the prior `"claude-opus-4-7"` default looked correct), but the OpenAI backend's render is strict. Always write `'gpt-5.5`, never `"gpt-5.5"`. (`gotcha` — 2026-05-16)

- **`flycheck-define-checker` `:command` executable must be a literal string at macro-expansion.** `flycheck.el:5927` does `(stringp (car command))` and errors otherwise. `(eval FORM)` is legal only in subsequent (argument) positions, not the executable slot. To inject a computed path like `(expand-file-name "scripts/foo" user-emacs-directory)`, wrap the entire `flycheck-define-checker` form in `eval` + backquote so the path splices in as a string literal before the macro inspects it. See `modules/flycheck-config.el` for the working shape. (`gotcha` — 2026-05-16)

- **Emacs 30 batch mode: `provide` does not fire registered `eval-after-load` callbacks.** Only an actual `load` triggers them. Tests that drive lazy-loading via `(provide 'foo)` will see registered callbacks fail to run. Two robust alternatives: (a) `(load <temp-file-that-provides-foo>)`, or (b) assert against `after-load-alist` directly — stronger evidence anyway since it proves the hook is registered for the right feature, not just that the body happens to execute. See `tests/test-ai-config-gptel-magit-lazy-loading.el` for the after-load-alist inspection pattern. (`gotcha` — 2026-05-16)

- **Warn at module load when an external tool path is configured but missing.** Calling `cj/executable-find-or-warn` (from `system-lib.el`) at `:config` time emits a `display-warning` if `prettier` / `pyright` / `pandoc` / etc. isn't on PATH, instead of letting the first format-on-save or LSP-attach fail with a confusing mid-edit error. Pattern in use: `modules/prog-webdev.el` (prettier), `modules/prog-python.el` (pyright). (`pattern` — 2026-05-16)

- **ghostel F-key / prefix bindings need `ghostel-keymap-exceptions` + a rebuild, not just `ghostel-mode-map`.** In semi-char mode ghostel forwards every key not in `ghostel-keymap-exceptions` to the pty, and `ghostel-semi-char-mode-map` (rebuilt from that list, and outranking the major-mode map) wins. So binding F9 / F12 / C-; in `ghostel-mode-map` alone is silently dead inside agent/terminal buffers — the key reaches the shell, not Emacs. Fix: add the key to `ghostel-keymap-exceptions` AND call `ghostel--rebuild-semi-char-keymap` (`add-to-list` updates the list but not the already-built map). `term-config.el` (C-;, F12) and `ai-term.el` (F9 family) do this in their `with-eval-after-load 'ghostel`. This is the opposite of vterm, where binding in `vterm-mode-map` sufficed. (`gotcha` — 2026-06-05)
