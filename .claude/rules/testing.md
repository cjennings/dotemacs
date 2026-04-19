# Testing Standards

Applies to: `**/*`

Core TDD discipline and test quality rules. Language-specific patterns
(frameworks, fixture idioms, mocking tools) live in per-language testing files
under `languages/<lang>/claude/rules/`.

## Test-Driven Development (Default)

TDD is the default workflow for all code, including demos and prototypes. **Write tests first, before any implementation code.** Tests are how you prove you understand the problem — if you can't write a failing test, you don't yet understand what needs to change.

1. **Red**: Write a failing test that defines the desired behavior
2. **Green**: Write the minimal code to make the test pass
3. **Refactor**: Clean up while keeping tests green

Do not skip TDD for demo code. Demos build muscle memory — the habit carries into production.

### Understand Before You Test

Before writing tests, invest time in understanding the code:

1. **Explore the codebase** — Read the module under test, its callers, and its dependencies. Understand the data flow end to end.
2. **Identify the root cause** — If fixing a bug, trace the problem to its origin. Don't test (or fix) surface symptoms when the real issue is deeper in the call chain.
3. **Reason through edge cases** — Consider boundary conditions, error states, concurrent access, and interactions with adjacent modules. Your tests should cover what could actually go wrong, not just the obvious happy path.

### Adding Tests to Existing Untested Code

When working in a codebase without tests:

1. Write a **characterization test** that captures current behavior before making changes
2. Use the characterization test as a safety net while refactoring
3. Then follow normal TDD for the new change

## Test Categories (Required for All Code)

Every unit under test requires coverage across three categories:

### 1. Normal Cases (Happy Path)
- Standard inputs and expected use cases
- Common workflows and default configurations
- Typical data volumes

### 2. Boundary Cases
- Minimum/maximum values (0, 1, -1, MAX_INT)
- Empty vs null vs undefined (language-appropriate)
- Single-element collections
- Unicode and internationalization (emoji, RTL text, combining characters)
- Very long strings, deeply nested structures
- Timezone boundaries (midnight, DST transitions)
- Date edge cases (leap years, month boundaries)

### 3. Error Cases
- Invalid inputs and type mismatches
- Network failures and timeouts
- Missing required parameters
- Permission denied scenarios
- Resource exhaustion
- Malformed data

## Combinatorial Coverage

For functions with 3+ parameters that each take multiple values (feature-flag
combinations, config matrices, permission/role interactions, multi-field
form validation, API parameter spaces), the exhaustive test count explodes
(M^N) while 3-5 ad-hoc cases miss pair interactions. Use **pairwise /
combinatorial testing** — generate a minimal matrix that hits every 2-way
combination of parameter values. Empirically catches 60-90% of combinatorial
bugs with 80-99% fewer tests.

Invoke `/pairwise-tests` on the offending function; continue using `/add-tests`
and the Normal/Boundary/Error discipline for the rest. The two approaches
complement: pairwise covers parameter *interactions*; category discipline
covers each parameter's individual edge space.

Skip pairwise when: the function has 1-2 parameters (just write the cases),
the context requires *provably* exhaustive coverage (regulated systems — document
in an ADR), or the testing target is non-parametric (single happy path,
performance regression, a specific error).

## Test Organization

Typical layout:

```
tests/
  unit/           # One test file per source file
  integration/    # Multi-component workflows
  e2e/            # Full system tests
```

Per-language files may adjust this (e.g. Elisp collates ERT tests into
`tests/test-<module>*.el` without subdirectories).

### Testing Pyramid

Rough proportions for most projects:
- Unit tests: 70-80% (fast, isolated, granular)
- Integration tests: 15-25% (component interactions, real dependencies)
- E2E tests: 5-10% (full system, slowest)

Don't duplicate coverage: if unit tests fully exercise a function's logic,
integration tests should focus on *how* components interact — not repeat the
function's case coverage.

## Integration Tests

Integration tests exercise multiple components together. Two rules:

**The docstring names every component integrated** and marks which are real vs
mocked. Integration failures are harder to pinpoint than unit failures;
enumerating the participants up front tells you where to start looking.

Example:

```
def test_integration_refund_during_sync_updates_ledger_atomically():
    """Refund processed mid-sync updates order and ledger in one transaction.

    Components integrated:
    - OrderService.refund (entry point)
    - PaymentGateway.reverse (MOCKED — returns success)
    - Ledger.credit (real)
    - db.transaction (real)

    Validates:
    - Refund rolls back if ledger write fails
    - Both tables updated or neither
    """
```

**Write an integration test when** multiple components must work together,
state crosses function boundaries, or edge cases combine. **Don't** when
single-function behavior suffices, or when mocking would erase the interaction
you meant to test.

## Naming Convention

- Unit: `test_<module>_<function>_<scenario>_<expected>`
- Integration: `test_integration_<workflow>_<scenario>_<outcome>`

Examples:
- `test_cart_apply_discount_expired_coupon_raises_error`
- `test_integration_order_sync_network_timeout_retries_three_times`

Languages that prefer camelCase, kebab-case, or other conventions keep the
structure but use their idiom. Consistency within a project matters more than
the specific case choice.

## Test Quality

### Independence
- No shared mutable state between tests
- Each test runs successfully in isolation
- Explicit setup and teardown

### Determinism
- Never hardcode dates or times — generate them relative to `now()`
- No reliance on test execution order
- No flaky network calls in unit tests

### Performance
- Unit tests: <100ms each
- Integration tests: <1s each
- E2E tests: <10s each
- Mark slow tests with appropriate decorators/tags

### Mocking Boundaries
Mock external dependencies at the system boundary:
- Network calls (HTTP, gRPC, WebSocket)
- File I/O and cloud storage
- Time and dates
- Third-party service clients

Never mock:
- The code under test
- Internal domain logic
- Framework behavior (ORM queries, middleware, hooks, buffer primitives)

### Signs of Overmocking

Ask yourself:

- Would this test still pass if I replaced the function body with `raise NotImplementedError` (or equivalent)? If yes, the mocks are doing the work — you're testing mocks, not code.
- Is the mock more complex than the function being tested? Smell.
- Am I mocking internal string / parsing / decoding helpers? Those aren't boundaries — they're the work.
- Does the test break when I refactor without changing behavior? Good tests survive refactors; overmocked ones couple to implementation.

When tests demand heavy internal mocking, the fix isn't better mocks — it's
restructuring the code (see *If Tests Are Hard to Write* below).

### Testing Code That Uses Frameworks

When a function mostly delegates to framework or library code, test *your*
integration logic:
- ✓ "I call the library with the right arguments in the right context"
- ✓ "I handle its return value correctly"
- ✗ "The library works in 50 scenarios" — trust it; it has its own tests

For polyglot behavior (e.g., comment handling across C/Java/Go/JS), test 2-3
representative modes thoroughly plus a minimal smoke test in the others.
Exhaustive permutations are diminishing returns.

### Test Real Code, Not Copies

Never inline or copy production code into test files. Always `require`/`import`
the module under test. Copied code passes even when production breaks — the
bug hides behind the duplicate.

Mock dependencies at their boundary; exercise the real function body.

### Error Behavior, Not Error Text

Test that errors occur with the right type; don't assert exact wording:
- ✓ Right exception type (`pytest.raises(ValueError)`, `(should-error ... :type 'user-error)`)
- ✓ Regex on values the message *must* contain (e.g., the offending filename)
- ✗ `assert str(e) == "File 'foo' not found"` — breaks when prose changes even though behavior is unchanged

Production code should emit clear, contextual errors. Tests verify the
behavior (raised, caught, returned nil) and values that must appear — not the
prose.

## If Tests Are Hard to Write, Refactor the Code

If a test needs extensive mocking of internal helpers, elaborate fixture
scaffolding, or mocks that recreate the function's own logic, the production
code needs restructuring — not the test.

Signals:
- Deep nesting (callbacks inside callbacks)
- Long functions doing multiple things ("fetch AND parse AND decode AND save")
- Tests that mock internal string / parsing / I/O helpers
- Tests that break on refactors with no behavior change

Fix: extract focused helpers (one responsibility each), test each in isolation
with real inputs, compose them in a thin outer function. Several small unit
tests plus one composition test beats one monster test behind a wall of mocks.

## Coverage Targets

- Business logic and domain services: **90%+**
- API endpoints and views: **80%+**
- UI components: **70%+**
- Utilities and helpers: **90%+**
- Overall project minimum: **80%+**

New code must not decrease coverage. PRs that lower coverage require justification.

## TDD Discipline

TDD is non-negotiable. These are the rationalizations agents use to skip it — don't fall for them:

| Excuse | Why It's Wrong |
|--------|----------------|
| "This is too simple to need a test" | Simple code breaks too. The test takes 30 seconds. Write it. |
| "I'll add tests after the implementation" | You won't, and even if you do, they'll test what you wrote rather than what was needed. Test-after validates implementation, not behavior. |
| "Let me just get it working first" | That's not TDD. If you can't write a failing test, you don't understand the requirement yet. |
| "This is just a refactor" | Refactors without tests are guesses. Write a characterization test first, then refactor while it stays green. |
| "I'm only changing one line" | One-line changes cause production outages. Write a test that covers the line you're changing. |
| "The existing code has no tests" | Start with a characterization test. Don't make the problem worse. |
| "This is demo/prototype code" | Demos build habits. Untested demo code becomes untested production code. |
| "I need to spike first" | Spikes are fine — then throw away the spike, write the test, and implement properly. |

If you catch yourself thinking any of these, stop and write the test.

## Anti-Patterns (Do Not Do)

- Hardcoded dates or timestamps (they rot)
- Testing implementation details instead of behavior
- Mocking the thing you're testing
- Mocking internal helpers (string ops, parsing, decoding) — those are the work
- Inlining production code into test files — always `require` / `import` the real module
- Asserting exact error-message text instead of type + key values
- Shared mutable state between tests
- Non-deterministic tests (random without seed, network in unit tests)
- Testing framework behavior instead of your code
- Ignoring or skipping failing tests without a tracking issue
