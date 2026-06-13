# Todo Entry Format

Applies to: `**/*.org` (org-mode todo and inbox files)

How task entries are structured in org-mode todo files (`todo.org`,
`inbox.org`, any GTD-style org file). Same shape across every project.

## Priority and Tag Scheme Header

Every project's `todo.org` opens with a top-level section named
`[Projectname] Priority Scheme` (e.g. `* Rulesets Priority Scheme`,
`* Work Priority Scheme`), placed above the first `* <Project> Open Work`
section. It declares two things so the rest of the file is legible without
guessing:

1. **Priorities** — what `[#A]` through `[#D]` mean for this project. At
   minimum, what makes something `[#A]` (urgent / blocking) versus `[#D]`
   (someday / watchlist).
2. **Tags** — the tag vocabulary in use. Name each tag and what it marks.
   For code projects the typical set is `:feature:`, `:bug:`, `:test:`,
   `:refactor:`, plus the effort/autonomy tags `:quick:` and `:solo:`. A
   project may add, drop, or rename tags to fit its work — the requirement
   is that the set is declared, not that it matches a fixed list.

The section is mandatory. A `todo.org` without it leaves `[#A]` and the tags
undefined, so task-audit can't enforce a vocabulary, task-review can't grade
against agreed semantics, and process-inbox can't file new tasks correctly
(its Phase B.1 already checks for this scheme). Each project defines the
scheme its own way; the floor is that priorities and tags are both spelled
out under the header.

When a project's `todo.org` lacks the section, add it before filing or
grading further tasks — propose the priority semantics and tag set from the
project's existing usage, and confirm with Craig.

## The Rule

A todo entry has two parts:

1. **Heading** — terse subject naming just the topic. No action verbs, no
   sentence-shape, no dates. Tags belong on the heading line.
2. **Body** (optional) — fuller description: action verbs, context,
   rationale, source/origin, links, deadlines. Used when the topic alone
   isn't enough.

When the topic alone is enough, skip the body entirely.

## Format

    ** TODO [#A] Terse topic phrase :tag1:tag2:
    Optional body — fuller description, action verbs, context, links.

    Multi-paragraph body is fine when context warrants it.

## Examples

Good:

    ** TODO [#A] Blacken + Prettier config from Vrezh
    Ask Jason to implement the formatter config Vrezh sends over.

    ** TODO [#B] TAK-server plugin user scenarios :quick:
    Develop with Eric, send to Nate Soule for review.
    Out of the 2026-05-13 RTX<>DeepSat sync.

Bad (sentence-shaped heading, details crammed in):

    ** TODO [#B] Develop TAK-server plugin user scenarios with Eric, send to Nate :quick:

## Why

The org agenda view shows the heading. A short heading is scannable; a
sentence-shaped one runs off the edge of the agenda buffer, and the
context that mattered ends up in the truncated tail. The body is always
reachable by visiting the entry — push everything beyond the topic there.

## How to apply

When adding a new task:

1. Pick the smallest noun phrase that names the topic.
2. If anything else is worth saying, put it in the body.
3. Tags go on the heading line, not in the body.

When restructuring an existing entry that's already sentence-shaped, split
it: keep the topic as the heading, move the rest to the body.

## Completion — depth-based

How a finished `TODO` / `DOING` task is closed depends on its depth in the outline. The rule is *depth-based*, not keyword-based, because the agenda truncates beyond level-2 and the visibility tradeoff flips at that boundary.

### Top-level tasks (`*` and `**`) — stay task-shaped

A completed `*` section or `**` task keeps its `TODO`/`DOING`-shape so it remains visible in the agenda as a record of what shipped:

1. Change the keyword to `DONE` (or `CANCELLED` if the task was abandoned rather than completed).
2. Add a `CLOSED: [YYYY-MM-DD Day]` line directly under the heading. Use `date "+%Y-%m-%d %a"` to generate.
3. Leave the original heading text, priority cookie, and tags intact.
4. Optionally add a one-line resolution note in the body.

The entry is then a candidate for `--archive-done` in the wrap-up cleanup, which moves level-2 `DONE`/`CANCELLED` subtrees from the project's "Open Work" section into "Resolved."

**Example:**

    ** TODO [#B] Convert <cj structure template to universal yasnippet

becomes

    ** DONE [#B] Convert <cj structure template to universal yasnippet
    CLOSED: [2026-05-15 Fri]

### Sub-tasks (`***` and deeper) — rewrite to a dated event-log entry

A completed sub-task disappears as a task and becomes an in-place event-log entry under its parent. The parent's subtree organically grows into a chronological history of what landed without the agenda being cluttered by a long tail of nested `DONE` lines.

1. Replace the heading with `<same depth> YYYY-MM-DD Day @ HH:MM:SS -ZZZZ <past-tense description>`.
2. Generate the timestamp with `date "+%Y-%m-%d %a @ %H:%M:%S %z"`.
3. Reword the original imperative title into the past-tense action that landed. Trim or restate if the original wording doesn't fit the action.
4. Drop the `TODO`/`DOING` keyword, the priority cookie, and the tags. The body stays as the record of what was done (if useful).

**Example:**

    *** TODO [#B] Wire yasnippet for universal availability :refactor:

becomes

    *** 2026-05-15 Fri @ 12:58:08 -0500 Wired yasnippet for universal availability

### Why depth-based

The agenda view (`org-agenda`) shows entries at the section + top-task level. Letting `**` tasks stay task-shaped preserves their visibility as "things that recently shipped." Letting `***+` sub-tasks flip to dated entries keeps the agenda from being clogged with a long list of completed sub-tasks at every depth — those become history within their parent instead.

`VERIFY` is the documented exception: it follows the dated-rewrite rule at **all** depths (including `**`), because a resolved VERIFY is an answered question rather than a finished task. See the VERIFY section below.

## VERIFY tasks

`VERIFY` is the keyword for "waiting on Craig's input" — open questions,
pending decisions, drafts awaiting approval. The placement and completion
rules below are stricter than normal TODO tasks because VERIFYs are
open-question placeholders, not regular tasks.

### Placement — top-level or first-level child only

A VERIFY task lives at exactly one of two depths:

1. **Top-level** under the relevant section (e.g., `** VERIFY ...` directly
   under `* Work Open Work`).
2. **First-level child** of a parent task (e.g., `*** VERIFY ...` under a
   `** TODO` / `** DOING` parent).

Never deeper. A VERIFY at `****` or below is buried — the agenda view
collapses it under its grandparent and Craig stops seeing it. When you
catch a VERIFY at `****+`, flatten it up to one of the two allowed depths.

The goal is a flat, scannable task list: the parent task is the topic; its
VERIFYs are the open threads under that topic; the dated history sits as
log entries (which *can* be deeper).

### Creating a new VERIFY — sibling of its trigger

When you add a new VERIFY task, place it as a **sibling of the heading
that triggered its creation** — not as a child of that heading.

The trigger is whatever surfaced the open question:

- A cj annotation that asks something or marks a pending decision →
  trigger is the heading the annotation sits under.
- A task body or sub-task that uncovered an unanswered question while you
  were working it → trigger is that task.
- A draft awaiting Craig's sign-off → trigger is the draft's parent task.

Depth-wise, this means:

- Trigger at `**` → new VERIFY at `**` (top-level sibling under the
  section).
- Trigger at `***` → new VERIFY at `***` (first-level-child sibling under
  the same `**` parent).
- Trigger at `****` or deeper → VERIFY can't follow it there (Placement
  rule above). Climb the VERIFY up to `***` and surface the buried
  trigger as a candidate to flatten.
- Trigger at `*` (a top-level section) → VERIFY goes at `**` (top-level
  task under the section).

File placement: insert the new VERIFY *after* the trigger's entire
sub-tree ends — i.e., after any sub-tasks, dated log headers, or draft
sub-headings the trigger already contains. This keeps everything under a
`**` parent flat: sub-tasks, dated logs, and VERIFYs all live as
siblings at `***`, never burrowing deeper.

The sibling rule is the active force that keeps `todo.org` flat. Without
it, VERIFYs accumulate one level deeper than their trigger every time —
turning a clean parent tree into a long pole of nested sub-headings.

### Completion — dated rewrite + content replacement

When a VERIFY resolves, **rewrite the heading and body together** at the
same depth — regardless of whether the VERIFY is at `**` or `***`:

1. **Replace the heading.** Drop the `VERIFY` keyword (and any priority
   cookie / tags) and replace with a timestamp + short description:

       *** 2026-05-15 Fri @ 14:00:00 -0500 <what was answered or done>

   Generate the timestamp with `date "+%Y-%m-%d %a @ %H:%M:%S %z"`.
   Match the original depth (a `**` VERIFY becomes `** YYYY-MM-DD ...`;
   a `***` VERIFY becomes `*** YYYY-MM-DD ...`).

2. **Replace the body.** Drop the original question/instruction prose and
   replace with either:
   - **The information Craig provided** (when the VERIFY was a question
     — paste the answer, a quote, a link, whatever resolved it), or
   - **A description of the action taken** (when the VERIFY was an
     instruction or pending-decision marker — what was done, when, where
     the artifact lives).

The completed VERIFY becomes an in-place event log entry. The original
question is preserved by the dated heading + body shape; anyone scanning
the agenda or `git log` can see what was asked and what landed.

**Note on the top-level case.** Regular `**` DONE tasks stay task-shaped
with a `DONE` keyword + `CLOSED:` line per *Completion — depth-based*
above. VERIFYs at `**` are the exception — they convert to dated log
entries on completion because a resolved VERIFY isn't a "done task," it's
an answered question. The dated-rewrite rule wins for VERIFYs at all
depths.

### Don't leave stale placeholders

A well-named VERIFY heading carries the question on its own. Don't append
`cj: <fill in>` placeholder lines under the heading — Craig adds his own
cj comment (a `#+begin_src cj: ... #+end_src` block) when he's ready to
answer, and that's the trigger to process the response. Empty placeholders
are noise that pollute his `cj:` greps.

### Examples

**Top-level VERIFY, before / after:**

    ** VERIFY What's our position on the BBN NDA reciprocal-term length?

    ** 2026-05-15 Fri @ 14:00:00 -0500 BBN NDA — standard 2-year reciprocal terms confirmed
    Per Nerses 2026-05-15 (Slack DM D0AAAEW3BS4): DeepSat's standard NDA template uses 2-year reciprocal. Sent to BBN legal for sign-off.

**First-level child VERIFY, before / after:**

    ** DOING [#A] Kostya's contract :admin:kostya:
    *** VERIFY Confirm Kostya's basis — part-time hr/week or full-time?

    ** DOING [#A] Kostya's contract :admin:kostya:
    *** 2026-05-15 Fri @ 14:00:00 -0500 Kostya basis — part-time, 20 hr/week
    Nerses confirmed 5/15 13:30 CDT: Kostya runs at 20 hr/week part-time, mirroring Vrezh's structure. Plugged into Exhibit A § 2 of the contract draft.
