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
make test-file FILE=tests/test-foo.el   # One file
make test-name TEST=pattern             # Match test names
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
