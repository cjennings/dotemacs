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
- Shared mutable state between tests
- Non-deterministic tests (random without seed, network in unit tests)
- Testing framework behavior instead of your code
- Ignoring or skipping failing tests without a tracking issue
