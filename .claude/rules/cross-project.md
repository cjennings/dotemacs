# Cross-Project Boundaries

Applies to: `**/*`

How to handle requests that target files or tasks belonging to a different project's `.ai/` scope than the current session.

## The Rule

When a request points at a file or task living under a *different* project's `.ai/` scope, stop before doing the work. Surface the boundary crossing in one line and ask: "this looks like it belongs to `<other project>`'s session — confirm you want me to do it from here, or switch projects?"

Each project's `.ai/` directory is the scope boundary. It carries that project's `protocols.org`, `session-context.org`, `sessions/`, `notes.org`, `todo.org`, `inbox/`, and its own memory dir under `~/.claude/projects/<encoded-cwd>/memory/`. Crossing the boundary without flagging it pollutes the current session's log with the other project's content, drops memories into the wrong dir, and skips the other project's protocols / CLAUDE.md / startup-extras that would otherwise apply.

## When to Detect

Trigger the check on any of these:

- A skill or tool argument names a file under another known project (e.g. cwd is `~/.emacs.d/` and the path is `~/projects/work/todo.org`).
- A file read or write would cross into another project's `.ai/`.
- A user request names another project by topic ("the work todo", "the deepsat repo", "my emacs config") while we're not in that project.

## How to Apply

State the mismatch and offer the two acceptable answers. Inline numbered options per `interaction.md` — no popup.

Two acceptable outcomes:

1. **"Yes, do it from here"** — proceed. Record the cross-project artifact in a handoff file under the *other* project's `inbox/`, named `YYYY-MM-DD-handoff-from-<this-project>-<topic>.org`, with a top note explaining the crossover. The other project's startup workflow picks it up during inbox processing.

   Prefer the `inbox-send` script (`.ai/scripts/inbox-send.py`, auto-synced into every project) over a manual `Write`/`Edit` for the drop. It handles project discovery, source-project provenance in the filename, slug derivation, and timestamping in one call:

   ```
   inbox-send <target> --text "your message"          # text → dated .org file
   inbox-send <target> --file <path>                  # copy a file into target/inbox/
   inbox-send --list                                  # see available projects
   ```

   Output filenames follow `YYYY-MM-DD-HHMM-from-<this-project>-<slug>.<ext>` automatically, so the target's next session sees the source + timestamp at a glance without you having to construct the name. Fall back to `Write`/`Edit` only when the script isn't available (e.g. a freshly-cloned project before the first startup-rsync).
2. **"Switch projects"** — stop. Let the user reopen Claude in the right cwd.

Don't assume which one was meant. Either guess is wrong half the time and the cost of asking once is one short turn.

## Recovery When It Goes Wrong

If you do the work first and the boundary issue surfaces afterwards:

1. Move the cross-project session-log entries out of the current session's `.ai/session-context.org` into `<other-project>/inbox/YYYY-MM-DD-handoff-from-<this-project>-<topic>.org`. Top of that file: a heads-up explaining the crossover so the other project's next session knows what happened.
2. Replace the moved content in `session-context.org` with a brief stub pointing at the handoff file.
3. Move any project-specific memories you saved into the right project's memory dir, or note them in the handoff file if you can't move them.

## Why

The user sometimes invokes a skill from whatever shell they happen to be in. The request may be accidental (they meant to be in the other project's terminal) or deliberate (knowing cross-project handoff). The model can't tell from the request alone, and assuming wrong both times costs more than asking once.

The per-project scope of `.ai/` is the design — protocols, history, memory, inbox, and todo all coupled to one project. Cross-project work breaks every assumption the next session of each project will make.

## Related

- `subagents.md` — the per-agent context-isolation discipline. Same principle, smaller scope.
- `interaction.md` — inline numbered options for the "from here / switch?" prompt.
- Per-project `.ai/protocols.org` — the project-scoped instructions this rule protects.
