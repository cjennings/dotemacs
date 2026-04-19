# Elisp Testing Rules

Applies to: `**/tests/*.el`

## Framework: ERT

Use `ert-deftest` for all tests. One test = one scenario.

## File Layout

- `tests/test-<module>.el` — tests for `modules/<module>.el`
- `tests/test-<module>--<helper>.el` — tests for a specific private helper (matches `<module>--<helper>` function naming)
- `tests/testutil-<module>.el` — fixtures and mocks for one module
- `tests/testutil-general.el`, `testutil-filesystem.el`, `testutil-org.el` — cross-module helpers

Tests must `(require 'module-name)` before the testutil file that stubs its internals, unless documented otherwise. Order matters — a testutil that defines a stub can be shadowed by a later `require` of the real module.

## Test Naming

```elisp
(ert-deftest test-<module>-<function>-<scenario> ()
  "Normal/Boundary/Error: brief description."
  ...)
```

Put the category (Normal, Boundary, Error) in the docstring so the category is grep-able.

## Required Coverage

Every non-trivial function needs at least:
- One **Normal** case (happy path)
- One **Boundary** case (empty, nil, min, max, unicode, long string)
- One **Error** case (invalid input, missing resource, failure mode)

Missing a category is a test gap. If three cases look near-identical, parametrize with a loop or `dolist` rather than copy-pasting.

## TDD Workflow

Write the failing test first. A failing test proves you understand the change. Assume the bug is in production code until the test proves otherwise — never fix the test before proving the test is wrong.

For untested code, write a **characterization test** that captures current behavior before you change anything. It becomes the safety net for the refactor.

## Mocking

Mock at boundaries:
- Shell: `cl-letf` on `shell-command`, `shell-command-to-string`, `call-process`
- File I/O when tests shouldn't touch disk
- Network: URL retrievers, HTTP clients
- Time: `cl-letf` on `current-time`, `format-time-string`

Never mock:
- The code under test
- Core Emacs primitives (buffer ops, string ops, lists)
- Your own domain logic — restructure it to be testable instead

## Idioms

- `cl-letf` for scoped overrides (self-cleaning)
- `with-temp-buffer` for buffer manipulation tests
- `make-temp-file` with `.el` suffix for on-disk fixtures
- Tests must run in any order; no shared mutable state

## Running Tests

```bash
make test                               # All
make test-file FILE=tests/test-foo.el   # One file
make test-name TEST=pattern             # Match by test name pattern
```

A PostToolUse hook runs matching tests automatically after edits to a module, when the match count is small enough to be fast.

## Anti-Patterns

- Hardcoded timestamps — generate relative to `current-time` or mock
- Testing implementation details (private storage structure) instead of behavior
- Mocking the thing you're testing
- Skipping a failing test without an issue to track it
