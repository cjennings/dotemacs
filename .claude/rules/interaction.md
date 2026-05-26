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

Reserve `AskUserQuestion` only when the user explicitly asks for the popup form ("use the popup for this one") or for genuinely free-form input where numbered options don't fit.

This rule applies to all three approval gates in the `commits.md` publish flow (commit message, PR description, PR review reply): print the draft inline, then offer numbered approve / changes / edit options inline. Do not switch to the popup form for the gate even though the prior protocol referenced it.

**Enforcement:** a global `PreToolUse` hook (matcher `AskUserQuestion`) in `~/.claude/settings.json` hard-denies the popup and returns this rule as the reason — the prose alone proved too easy to forget. Because the deny is unconditional, the "use the popup for this one" exception above can't be honored in-turn; to get the popup, disable the hook via `/hooks` (or edit settings) first.

## No Reverse-Video Highlighting in Chat Output

In conversational output to the user, do not use Markdown bold (`**...**`) or inline-code spans (backtick `` `...` ``). The user's terminal renders both as reverse video, which is hard to read on the display.

**Why:** The styling that looks like emphasis in most renderers comes through as inverted foreground/background here. Plain prose, plain identifiers, and plain key chords read cleanly; the "highlight" actively hurts legibility.

**How to apply:**

- Write command names, file paths, key chords, and code identifiers as plain text — `pearl-save-issue` becomes pearl-save-issue, `C-; L s s` becomes C-; L s s.
- Use structure that doesn't invert colors: headers, numbered lists, dashes, parentheses, and double quotes for labels are all fine.
- Fenced code blocks (triple backtick) are acceptable when the user explicitly wants a block to copy — they don't invert the way inline spans do. Default to plain text otherwise.

This governs **chat output**, not the Markdown source of rule files, specs, or docs the user reads in an editor — those keep normal Markdown formatting. The constraint is the terminal rendering of the live conversation.
