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
