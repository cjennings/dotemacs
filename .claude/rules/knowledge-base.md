# Agent Knowledge Base (org-roam)

Applies to: `**/*`

Craig's org-roam knowledge base is the shared, cross-project store for durable agent knowledge. It lives at `~/org/roam/` — a git repo (origin `git@cjennings.net:roam.git`), auto-synced on Craig's machines by the `roam-sync` systemd timer. Per-project harness memory stays the fast capture layer; durable facts get promoted here.

## Reading (any project)

Before relying on a remembered project fact, a prior decision, or reference material, search the KB first. The interface is plain files — never the org-roam SQLite cache:

```sh
# content/tag search
rg --glob '*.org' --glob '!*sync-conflict*' '<query>' ~/org/roam/
# follow an [[id:UUID]] link to its node
rg --glob '*.org' --glob '!*sync-conflict*' ':ID:[[:space:]]+<uuid>' ~/org/roam/
```

Pull before querying (`git -C ~/org/roam pull --ff-only`); skip silently if offline. If `~/org/roam/` doesn't exist on this machine, proceed without the KB and say so — never fabricate recall.

## Writing (personal projects only)

Classify the project before any write. The source of truth is the work-root denylist below — never inference from remotes, names, or task content:

- **Work** — project root is, or sits under, a denylisted root. No KB write, ever. Record durable facts per that project's own conventions.
- **Personal** — project root sits under `~/code/`, `~/projects/`, or `~/.emacs.d` and is not denylisted. KB writes allowed.
- **Unknown** — anything else. No KB write.

Work-root denylist (confirmed by Craig, 2026-06-10): `~/projects/work`

**Refusal contract** (work and unknown alike): state the classification, name the durable fact in a one-line redacted summary, and say where it was or wasn't written — so Craig can re-route it deliberately instead of losing it silently.

A write is one node per fact, under `agents/`, roam-valid so Craig's org-roam indexes it:

```
~/org/roam/agents/YYYYMMDDHHMMSS-<slug>.org
---
:PROPERTIES:
:ID:       <uuid — generate with uuidgen>
:END:
#+title: <concise title>
#+filetags: :agent:<scope>:

<the fact, with [[id:...]] links to related nodes>
```

Pull before writing, commit and push after (`git -C ~/org/roam add -A && git commit && git push`) — same session discipline as any repo. Never edit Craig's hand-authored nodes; link to them. This write autonomy is scoped to the KB alone — it is not permission to send email, comment on tickets, or post to any public or external channel.

## What goes in, what stays out

**In:** durable facts with cross-project or cross-machine value — decisions and their why, environment and tooling gotchas, reference pointers (URLs, dashboards, key paths), lessons that transfer beyond the project that learned them.

**Out:** anything the repo already records (code structure, git history, CLAUDE.md content), session state, task state (todo.org owns that), high-churn facts that will be stale in a month, secrets and credentials, anything work-confidential.

## Capture, then promote

Harness memory (`~/.claude/projects/<enc>/memory/`) remains the per-project capture layer: fast, automatic, allowed to be at-risk. At wrap-up (or a task audit, or an explicit prompt), promote facts that meet the inclusion bar into the KB as nodes. The wrap-up workflow asks; answer it honestly — promotion discipline is what keeps the capture layer from silting up.

## Inventory

`rg '#\+filetags:.*:agent:' ~/org/roam/` lists everything agents ever wrote; the git log is the per-write audit trail. Craig prunes at will — deletion or revert of an `:agent:` node is never something to argue with.
