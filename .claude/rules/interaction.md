# Interaction Style

Applies to: `**/*`

How Claude communicates with the user during a session — choice prompts, status updates, decision points.

## No Popup Menus for Choices

When Claude needs the user to pick between options, **do not** use the AskUserQuestion popup. Present the options inline in chat as a numbered list and ask the user to reply with a number.

**Why:** The popup menu UI sits at the bottom of the chat window and obscures the chat content directly above it — exactly the area the user needs to read to make the choice. Inline numbered options keep the question, the surrounding context, and the proposed text all visible in the same scrollback.

**How to apply:**

For approve / changes / cancel flows (commit-message review, PR-description review, plan approval), draft inline:

```
1. Approve — commit now
2. Request changes — tell me what to adjust
3. Open in editor — emacsclient -n /tmp/...

Pick a number.
```

For pick-one decisions, same shape: numbered list, one-line prompt at the end.

For multi-select decisions, say so explicitly: "Pick any combination — reply with the numbers."

**Render-merge guard.** GFM renderers merge adjacent ordered lists and renumber them — a message with a 1-3 content list followed by a 1-3 options list rendered as options 4-6 on the user's screen, and his pick ("4") didn't exist in the author's numbering (work session, 2026-06-10). Numerals belong to the options list only: any other enumeration in the same message uses dashes or prose, and a short prose lead-in line sits directly above the options so no renderer can merge them. When the user picks a number outside the offered range, suspect the render-merge and ask which item text they meant rather than treating it as an error.

Reserve `AskUserQuestion` only when the user explicitly asks for the popup form ("use the popup for this one") or for genuinely free-form input where numbered options don't fit.

This rule applies to all three approval gates in the `commits.md` publish flow (commit message, PR description, PR review reply): print the draft inline, then offer numbered approve / changes / edit options inline. Do not switch to the popup form for the gate even though the prior protocol referenced it.

### Order options with the recommendation at item 1

When the question has a clear recommended option, put it at item 1. The other paths stay visible so the user can override without having to type a custom answer, but the common case collapses to a single keystroke.

Default to this pattern whenever the user needs to weigh a genuine choice — task-review actions, brainstorm decisions, judgment calls, approve/edit/cancel gates, "what should I do next" prompts. Skip it only when the question is free-form (open prose, names, dates), when the user has already issued a directive and a single confirmation suffices, or when there is no clearly-recommended option (in which case state that plainly: "I don't have a recommendation here — what's your read?").

Include a "Skip" / "Defer" / "Tell me how" as the last option when the user's answer might be "none of these."

The convention reinforces the popup-denial rule above by giving every inline choice list a single canonical shape, and it forces the model to actually pick a recommendation rather than offering a neutrally-ordered enumerate-and-defer list. A neutrally-ordered list is a covert way to push the decision back; recommendation-first puts skin in the game.

**Enforcement:** a global `PreToolUse` hook (matcher `AskUserQuestion`) in `~/.claude/settings.json` hard-denies the popup and returns this rule as the reason — the prose alone proved too easy to forget. Because the deny is unconditional, the "use the popup for this one" exception above can't be honored in-turn; to get the popup, disable the hook via `/hooks` (or edit settings) first.

## No Reverse-Video Highlighting in Chat Output

In conversational output to the user, do not use Markdown bold (`**...**`) or inline-code spans (backtick `` `...` ``). The user's terminal renders both as reverse video, which is hard to read on the display.

**Why:** The styling that looks like emphasis in most renderers comes through as inverted foreground/background here. Plain prose, plain identifiers, and plain key chords read cleanly; the "highlight" actively hurts legibility.

**How to apply:**

- Write command names, file paths, key chords, and code identifiers as plain text — `pearl-save-issue` becomes pearl-save-issue, `C-; L s s` becomes C-; L s s.
- Use structure that doesn't invert colors: headers, numbered lists, dashes, parentheses, and double quotes for labels are all fine.
- Fenced code blocks (triple backtick) are acceptable when the user explicitly wants a block to copy — they don't invert the way inline spans do. Default to plain text otherwise.

This governs **chat output**, not the Markdown source of rule files, specs, or docs the user reads in an editor — those keep normal Markdown formatting. The constraint is the terminal rendering of the live conversation.
