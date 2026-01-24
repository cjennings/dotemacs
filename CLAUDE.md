# Claude Code Instructions

## SESSION CONTEXT - MANDATORY

**After every 3rd response, update `docs/session-context.org`.**

This is non-negotiable. Count your responses: 1, 2, 3 → UPDATE FILE.

The file must contain:
- Current task/goal
- Key decisions made
- Files modified
- Next steps
- Any context needed to resume if session crashes

If the file exists at session start, the previous session was interrupted - read it first.

## Project Documentation

Read these files at session start:
1. `docs/protocols.org` - Behavioral protocols and conventions
2. `docs/NOTES.org` - Project-specific context and history

## Key Rules

- **Commits**: No Claude/Anthropic attribution. Commit as Craig.
- **Files**: Use `.org` format, hyphens in filenames (no spaces)
- **Wrap-up phrases**: "wrap it up", "that's a wrap" → Execute wrap-up workflow
