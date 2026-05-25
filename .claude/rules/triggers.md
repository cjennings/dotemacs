# Cross-Project Trigger Phrases

Applies to: `**/*`

Trigger phrases the user can say from any session to invoke a cross-project action. These live in the global rules layer because they cross project boundaries — the user can be sitting in any cwd, including outside a project, and the phrase still means the same thing.

## "Launch project X"

Synonyms: "Launch X", "Open project X", "Switch to project X".

**Action:** run the `ai` script (the Claude Code session launcher, installed at `~/.local/bin/ai`) in single-project mode targeting the named project.

```
ai <project-path>
```

The `ai` script handles tmux session creation, window placement, and the per-project Claude opening line — see `~/code/rulesets/claude-templates/bin/ai` for the canonical source.

**Resolving X.** Match against project basenames discoverable by `ai` — directories under `~/code/`, `~/projects/`, and `~/.emacs.d` that contain `.ai/protocols.org`.

- Exact basename match (case-insensitive) → invoke `ai <path>` directly.
- No match → list all available basenames, ask which to launch.
- Multiple partial matches (X is a substring of two or more candidates) → list the matching basenames, ask which.

Do not guess. The cost of asking once is one short turn; launching the wrong project is a wrong-context Claude session that has to be killed and restarted.

## Why a separate file

Other claude-rules files cover specific concerns: `commits.md` for the publish flow, `subagents.md` for delegation, `testing.md` for test discipline. None is a natural home for "phrases the user says to trigger global actions." The trigger phrases in `protocols.org` (`Let's run the [X] workflow`, `Wrap it up`) are project-scoped — they assume an active `.ai/` session. Cross-project launchers warrant their own file.

## Adding new triggers

Same shape. Each entry: phrase (in quotes), synonyms, the action, ambiguity handling.
