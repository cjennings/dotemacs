# Elisp Testing Rules

Applies to: `**/tests/*.el`

Implements the core principles from `testing.md`. All rules there apply here —
this file covers Elisp-specific patterns.

## Framework: ERT

Use `ert-deftest` for all tests. One test = one scenario.

## File Layout

- `tests/test-<module>.el` — tests for `<module>.el`
- `tests/test-<module>--<helper>.el` — tests for a specific private helper (matches `<module>--<helper>` function naming)
- `tests/testutil-<module>.el` — fixtures and mocks scoped to one module
- `tests/testutil-*.el` — cross-module helpers (shared fixtures, generic mocks, filesystem helpers); name them for what they help with

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

## Interactive vs Internal — Split for Testability

When a function mixes business logic with user interaction, split it:

- **Internal** (`cj/--foo`) — pure logic. All parameters explicit. No prompts,
  no UI. Deterministic and trivially testable.
- **Interactive wrapper** (`cj/foo`) — thin layer that reads user input and
  delegates to the internal.

```elisp
(defun cj/--move-buffer-and-file (dir &optional ok-if-exists)
  "Move the current buffer's file into DIR. Overwrite if OK-IF-EXISTS."
  ...)

(defun cj/move-buffer-and-file ()
  "Interactive wrapper: prompt for DIR, delegate."
  (interactive)
  (let ((dir (read-directory-name "Move to: ")))
    (cj/--move-buffer-and-file dir)))
```

Test the internal directly with parameter values — no `cl-letf` on
`read-directory-name`, `yes-or-no-p`, etc. The wrapper gets a smoke test or
nothing — Emacs already tests its own prompts. The internal also becomes
reusable by other Elisp code without triggering UI.

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

## Batch-Mode Reproducibility

Tests must pass under `emacs --batch` — the headless, scriptable path that CI and the `make` targets use. `--batch` is the source of truth, not an interactive session.

- Don't depend on interactive-session state: window configuration, frame parameters, `this-command`, minibuffer activity, or anything a running editor accumulates. A test that passes in a live Emacs but fails (or hangs) under `--batch` is broken.
- Don't block on a prompt. `--batch` has no one to answer `y-or-n-p` or `read-string`, so an unmocked prompt either errors or stalls the run. Test the internal directly (see *Interactive vs Internal* above) or `cl-letf` the prompt.
- Keep tests deterministic: no reliance on test execution order, wall-clock time (mock `current-time`), or environment that differs between the developer's machine and CI.

## Isolating Emacs State

A test must not read or mutate the developer's real Emacs config. Bind a throwaway environment so the run is hermetic regardless of who runs it.

- Bind `user-emacs-directory` (and, when relevant, `user-init-file`) to a temp directory so package state, `custom-file` writes, caches, and auto-save files land in the sandbox rather than the developer's `~/.emacs.d`.
- Control `load-path` explicitly. Add only the project's own directories; don't lean on whatever happens to be installed in the developer's session.
- Depend only on the project's declared dependencies. A test that passes because some unrelated package is installed on this machine will fail on a clean checkout or in CI.

```elisp
(ert-deftest test-foo-writes-to-sandbox ()
  "Normal: writes under an isolated user-emacs-directory."
  (let* ((sandbox (make-temp-file "elisp-test-" t))
         (user-emacs-directory (file-name-as-directory sandbox)))
    (unwind-protect
        (progn
          (cj/--foo)
          (should (file-exists-p (expand-file-name "foo.cache" user-emacs-directory))))
      (delete-directory sandbox t))))
```

## Byte-Compile and Native-Comp Warnings

A clean compile is part of green. Byte-compile warnings (free variables, wrong argument counts, unused lexical bindings, obsolete-function calls) flag real defects, so treat them as failures rather than noise.

This can be enforced in the test run by binding `byte-compile-error-on-warn` to `t` and compiling the modules under test, optionally extending to native compilation where `native-comp-async-report-warnings-errors` is available.

Keep the native-comp half conditional. Native compilation exists only on builds with the `native-compile` feature (Emacs 28+ compiled with it); older or non-native builds lack `native-comp-*` variables and `native-compile` entirely. Gate on the feature so the suite still runs everywhere:

```elisp
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  ;; native-comp-specific checks here
  )
```

Make the warnings-as-errors gate opt-in or version-aware rather than absolute — a warning that's clean on the project's pinned Emacs may differ across versions, and a hard failure on every build penalizes contributors on a different Emacs than the maintainer's.

## Anti-Patterns

- Hardcoded timestamps — generate relative to `current-time` or mock
- Testing implementation details (private storage structure) instead of behavior
- Mocking the thing you're testing
- Skipping a failing test without an issue to track it
