# Working-Files Convention

Applies to: `**/*` (every project)

How in-progress task artifacts are organized in the project tree, and how
they migrate to their permanent home when the task ships.

## The Rule

Every in-progress task that produces files (drafts, source documents,
diagrams, scripts, sample data, transcripts, sub-deliverables) gets a
dedicated subdirectory under the project's `working/` directory, named
after the task. All artifacts for that task live in that subdirectory —
the org source, every supporting file, every draft revision, every
generated artifact — until the task is marked done.

When the task is marked done, the files get **renamed individually** and
**moved flat** into the appropriate permanent home (typically `assets/`,
or an area-specific `assets/` like `deepsat/assets/`). The working
subdirectory is then empty and gets deleted.

**Never rename the directory itself as a substitute for filing.** Renaming
the working subdir to its permanent name keeps every file together as a
bundle, but loses the flat-filing property — future grep for an artifact
hits a nested path instead of a single canonical name. Always rename the
files individually with a shared prefix so they sort together but live as
flat siblings in `assets/`.

## Directory Layout

    <project-root>/
      working/
        <task-slug>/
          <artifact-1>
          <artifact-2>
          ...

Examples:

    work/working/tech-deck-vol2/
      tech-deck-vol2.org
      slide-04-platform-at-a-glance.mmd
      slide-04-platform-at-a-glance.png
      vol1-template.pptx

    finances/working/2026-q2-tax-return/
      schedule-c.org
      receipts-import.csv
      depreciation-worksheet.ods

    work/working/SE-297-justin-onboarding/
      justin-tasking-options.org
      rate-research.org
      advisory-agreement-draft.docx

## Task-Slug Naming

Keep the slug short, kebab-case, and recognizable on a 1-second scan.

- Topic-led for narrative tasks: `tech-deck-vol2`, `branching-strategy-spec`, `tampa-trip`.
- Ticket-led for ticket-driven tasks: `SE-297-justin-onboarding`, `DEE-722-vincent-vetting`.
- Date-led for time-boxed work: `2026-q2-tax-return`, `2026-05-sofweek-prep`.

Avoid: trailing dates on topic-led slugs (`tech-deck-vol2-2026-05-18`)
when the task is open-ended; the slug names *the task*, not a snapshot.

## Filing on Completion

When the task is marked done:

1. **Decide each file's permanent home.** Usually one of:
   - `<area>/assets/` (e.g. `deepsat/assets/`) — for area-scoped reference
     material (transcripts, PDFs, diagrams that document the area's state).
   - Project-root `assets/` — for project-scoped material.
   - `<area>/<topic>/` — for material that joins an existing topic
     subdirectory (rare; only when the topic dir already exists).

2. **Rename each file** so it carries the task context and is sortable
   alongside its siblings. Standard form:
   `<YYYY-MM-DD>-<task-slug>-<descriptor>.<ext>`

   Examples:
   - `tech-deck-vol2.org` → `2026-05-18-tech-deck-vol2-source.org`
   - `slide-04-platform-at-a-glance.png` → `2026-05-18-tech-deck-vol2-slide-04-diagram.png`
   - `vol1-template.pptx` → `2026-05-08-tech-deck-vol1-template.pptx`
     (use the date the artifact originated, not the date the task closed,
     when it's clearly meaningful — Vol 1 template predates Vol 2 work)

3. **Move flat into the permanent home.** No nested subdirectory in
   `assets/`. The files sort together by date + task-slug prefix.

4. **Delete the now-empty `working/<task-slug>/` subdirectory.**

5. **Update any inbound links.** Tasks in `todo.org`, references in
   `notes.org`, cross-links from other documents — all need their `file:`
   paths updated to the new flat-filed locations. Use grep to find every
   occurrence before deleting the working dir.

## Why This Shape

- *Discovery during work* — every artifact for the current task is in
  one place. No hunting across `assets/`, `drafts/`, scratch dirs.
- *Discovery after completion* — `assets/` stays a flat searchable
  store. Every artifact is reachable by a single `find assets/ -name '*<slug>*'`.
- *Atomic completion* — the act of filing forces a review of every
  artifact's permanent value. Files that aren't worth keeping get
  deleted in the move; the rename forces a meaningful name.
- *No orphan subdirs in `assets/`* — renaming the directory instead of
  files would bury artifacts inside a nested path that no flat search
  reaches. Three years from now `assets/old-tech-deck-2026/slide.png`
  is harder to find than `assets/2026-05-18-tech-deck-vol2-slide-04-diagram.png`.

## When the Rule Doesn't Apply

- *Single-file scratch work that lives one day* — a `/tmp/foo.txt` for
  a draft is fine. The convention is for tasks producing multiple
  artifacts or artifacts worth keeping.
- *Source-controlled subprojects* — code under `<project>/code/<repo>/`
  follows the subproject's own conventions, not this rule.
- *Inbox content* — `inbox/` and `daily-prep/` follow their own
  conventions (dated filenames, processed and moved on cadence).

## Implementation Note for Claude Sessions

When the user starts a new task that's going to produce file artifacts:

1. Propose the working-dir path before creating any files
   (`working/<task-slug>/`).
2. Create the directory and put the first artifact there.
3. Add or update the inbound link in `todo.org` to point at the new
   path.
4. Note the working-dir in the task's body so future sessions find it.

When the user marks the task done:

1. List every file in the working subdir.
2. Propose renames + permanent homes for each.
3. Move flat after confirmation.
4. Delete the empty working subdir.
5. Update inbound links.

The directory layout is the same shape across every project — see
per-project `CLAUDE.md` or `notes.org` for project-specific
permanent-home conventions (e.g. `deepsat/assets/` vs project-root
`assets/`).
