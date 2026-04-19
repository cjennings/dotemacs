# Commit Rules

Applies to: `**/*`

## Author Identity

All commits are authored as the user (repo owner / maintainer), never as
Claude, Claude Code, Anthropic, or any AI tool. Git uses the configured
`user.name` and `user.email` — do not modify git config to attribute
otherwise.

## No AI Attribution — Anywhere

Absolutely no AI/LLM/Claude/Anthropic attribution in:

- Commit messages (subject or body)
- PR descriptions and titles
- Issue comments and reviews
- Code comments
- Commit trailers
- Release notes, changelogs, and any public-facing artifact

This means:

- **No** `Co-Authored-By: Claude …` (or Claude Code, or any AI) trailers
- **No** "Generated with Claude Code" footers or equivalents
- **No** 🤖 emojis or similar markers implying AI authorship
- **No** references to "Claude", "Anthropic", "LLM", "AI tool" as a credited contributor
- **No** attribution added via template defaults — strip them before committing

If a tool, template, or default config inserts attribution, remove it. If
settings.json needs it, set `attribution.commit: ""` and `attribution.pr: ""`
to suppress the defaults.

## Commit Message Format

Conventional prefixes:

- `feat:` — new feature
- `fix:` — bug fix
- `refactor:` — code restructuring, no behavior change
- `test:` — adding or updating tests
- `docs:` — documentation only
- `chore:` — build, tooling, meta

Subject line ≤72 characters. Body explains the *why* when not obvious.
Skip the body entirely when the subject line is self-explanatory.

## Before Committing

1. Check author identity: `git log -1 --format='%an <%ae>'` — should be the user.
2. Scan the message for AI-attribution language (including emojis and footers).
3. Review the diff — only intended changes staged; no unrelated files.
4. Run tests and linters (see `verification.md`).

## If You Catch Yourself

Typing any of the following — stop, delete, rewrite:

- `Co-Authored-By: Claude`
- `🤖 Generated with …`
- "Created with Claude Code"
- "Assisted by AI"

Rewrite the commit as the user would write it: concise, focused on the
change, no mention of how the change was produced.
