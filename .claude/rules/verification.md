# Verification Before Completion

Applies to: `**/*`

## The Rule

Do not claim work is done without fresh verification evidence. Run the command, read the output, confirm it matches the claim, then — and only then — declare success.

This applies to every completion claim:
- "Tests pass" → Run the test suite. Read the output. Confirm all green.
- "Linter is clean" → Run the linter. Read the output. Confirm no warnings.
- "Build succeeds" → Run the build. Read the output. Confirm no errors.
- "Bug is fixed" → Run the reproduction steps. Confirm the bug is gone.
- "No regressions" → Run the full test suite, not just the tests you added.

## What Fresh Means

- Run the verification command **now**, in the current session
- Do not rely on a previous run from before your changes
- Do not assume your changes didn't break something unrelated
- Do not extrapolate from partial output — read the whole result

## Red Flags

If you find yourself using these words, you haven't verified:

- "should" ("tests should pass")
- "probably" ("this probably works")
- "I believe" ("I believe the build is clean")
- "based on the changes" ("based on the changes, nothing should break")

Replace beliefs with evidence. Run the command.

## Before Committing

Before any commit:
1. Run the test suite — confirm all tests pass
2. Run the linter — confirm no new warnings
3. Run the type checker — confirm no new errors
4. Review the diff — confirm only intended changes are staged

Do not commit based on the assumption that nothing broke. Verify.
